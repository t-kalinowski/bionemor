#!/usr/bin/env python3

"""Run pinned ESM-2 pooling models and write portable embeddings."""

from __future__ import annotations

import argparse
import hashlib
import importlib.metadata
import importlib.util
import json
import math
import os
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Any


PROTOCOL_VERSION = 1
HELPER_VERSION = "0.1.2"
EXECUTION_SCHEMA_VERSION = 1
DRIVER = "esm2-vllm"
VLLM_VERSION = "0.15.1"
VLLM_REVISION = "1892993bc18e243e2c05841314c5e9c06a80c70d"


def atomic_write_text(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=f".{path.name}-", dir=path.parent
    )
    temporary = Path(temporary_name)
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as stream:
            stream.write(text)
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, path)
    finally:
        temporary.unlink(missing_ok=True)


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def package_version(name: str) -> str | None:
    try:
        return importlib.metadata.version(name)
    except importlib.metadata.PackageNotFoundError:
        return None


def compatible_vllm_version(version: str | None) -> bool:
    if version is None:
        return False
    return version.split("+", maxsplit=1)[0] == VLLM_VERSION or version.startswith(
        f"0.15.2.dev0+g{VLLM_REVISION[:9]}"
    )


def supported_compute_capabilities() -> list[str]:
    return [
        value.strip()
        for value in os.environ.get("BIONEMOR_CUDA_ARCH_LIST", "").split(";")
        if value.strip()
    ]


def gpu_runtime() -> dict[str, Any]:
    import torch

    cuda_available = torch.cuda.is_available()
    gpu_count = torch.cuda.device_count()
    gpus = []
    driver = None
    if cuda_available:
        probe = subprocess.run(
            [
                "nvidia-smi",
                "--query-gpu=driver_version",
                "--format=csv,noheader",
            ],
            check=True,
            capture_output=True,
            text=True,
        )
        drivers = {line.strip() for line in probe.stdout.splitlines() if line.strip()}
        if len(drivers) != 1:
            raise RuntimeError("GPU driver versions are missing or inconsistent")
        driver = drivers.pop()
        for index in range(gpu_count):
            properties = torch.cuda.get_device_properties(index)
            major, minor = torch.cuda.get_device_capability(index)
            gpus.append(
                {
                    "index": index,
                    "name": properties.name,
                    "total_memory_bytes": properties.total_memory,
                    "compute_capability_major": major,
                    "compute_capability_minor": minor,
                }
            )
    return {
        "python": sys.version.split()[0],
        "pytorch": torch.__version__,
        "cuda": torch.version.cuda,
        "cuda_available": cuda_available,
        "gpu_count": gpu_count,
        "driver": driver,
        "gpus": gpus,
        "vllm": package_version("vllm"),
    }


def description() -> dict[str, Any]:
    version = package_version("vllm")
    revision = os.environ.get("BIONEMOR_VLLM_REVISION")
    runtime = gpu_runtime()
    supported = supported_compute_capabilities()
    gpu_supported = runtime["gpu_count"] > 0 and (
        not supported
        or all(
            f"{gpu['compute_capability_major']}.{gpu['compute_capability_minor']}"
            in supported
            for gpu in runtime["gpus"]
        )
    )
    available = (
        importlib.util.find_spec("vllm") is not None
        and compatible_vllm_version(version)
        and (not revision or revision == VLLM_REVISION)
        and gpu_supported
    )
    runtime["supported_compute_capabilities"] = supported
    runtime["vllm_revision"] = revision
    return {
        "protocol_version": PROTOCOL_VERSION,
        "helper_version": HELPER_VERSION,
        "helper_sha256": sha256_file(Path(__file__).resolve()),
        "recipe_version": f"vllm-{VLLM_VERSION}",
        "recipe_revision": os.environ.get("BIONEMOR_RECIPE_REVISION"),
        "driver": DRIVER,
        "execution_schema_version": EXECUTION_SCHEMA_VERSION,
        "semantic_operations": ["embed"],
        "commands": {"embed": available},
        "features": {"pooled_embeddings_jsonl": available},
        "runtime": runtime,
    }


def read_requests(path: Path) -> tuple[list[str], list[str]]:
    ids: list[str] = []
    sequences: list[str] = []
    identifier: str | None = None
    residues: list[str] = []

    def append_record() -> None:
        if identifier is None:
            return
        sequence = "".join(residues)
        if not sequence:
            raise ValueError(f"protein {identifier!r} has an empty sequence")
        ids.append(identifier)
        sequences.append(sequence)

    with path.open(encoding="utf-8") as stream:
        for line_number, line in enumerate(stream, start=1):
            line = line.rstrip("\r\n")
            if not line:
                continue
            if line.startswith(">"):
                append_record()
                identifier = line[1:]
                residues = []
                if not identifier:
                    raise ValueError(f"FASTA header {line_number} has an empty id")
            elif identifier is None:
                raise ValueError("FASTA input must start with a header")
            else:
                residues.append(line)
    append_record()
    if not ids:
        raise ValueError("input must contain at least one protein")
    if len(ids) != len(set(ids)):
        raise ValueError("input ids must be unique")
    return ids, sequences


def embedding_values(output: Any) -> list[float]:
    embedding = output.outputs.embedding
    if hasattr(embedding, "tolist"):
        embedding = embedding.tolist()
    values = [float(value) for value in embedding]
    if not values or any(not math.isfinite(value) for value in values):
        raise RuntimeError("vLLM returned an empty or non-finite embedding")
    return values


def embed(args: argparse.Namespace) -> None:
    from vllm import LLM

    ids, sequences = read_requests(args.input)
    model_args: dict[str, Any] = {
        "model": args.model,
        "runner": "pooling",
        "trust_remote_code": True,
        "dtype": "float32",
        "enforce_eager": True,
        "max_num_batched_tokens": args.max_num_batched_tokens,
        # NVEsm is bidirectional. Its vLLM Transformers fallback does not
        # preserve request boundaries or support causal prefix reuse.
        "max_num_seqs": args.max_num_seqs,
        "enable_prefix_caching": not args.disable_prefix_caching,
        "tensor_parallel_size": args.tensor_parallel_size,
        "disable_log_stats": True,
        "seed": 42,
    }
    if args.revision is not None:
        model_args["revision"] = args.revision
        model_args["tokenizer_revision"] = args.revision
    model = LLM(**model_args)
    outputs = model.embed(sequences, use_tqdm=False)
    if len(outputs) != len(ids):
        raise RuntimeError("vLLM returned the wrong number of embeddings")
    rows = []
    width = None
    for identifier, output in zip(ids, outputs, strict=True):
        values = embedding_values(output)
        if width is None:
            width = len(values)
        elif len(values) != width:
            raise RuntimeError("vLLM returned embeddings with different widths")
        rows.append(json.dumps({"id": identifier, "embedding": values}))
    atomic_write_text(args.output, "\n".join(rows) + "\n")


def parser() -> argparse.ArgumentParser:
    value = argparse.ArgumentParser(description=__doc__)
    subparsers = value.add_subparsers(dest="command", required=True)

    describe = subparsers.add_parser("describe")
    describe.add_argument("--json", action="store_true", required=True)

    embedding = subparsers.add_parser("embed")
    embedding.add_argument("--model", required=True)
    embedding.add_argument("--revision")
    embedding.add_argument("--input", type=Path, required=True)
    embedding.add_argument("--output", type=Path, required=True)
    embedding.add_argument(
        "--max-num-batched-tokens", type=int, required=True
    )
    embedding.add_argument("--max-num-seqs", type=int, choices=[1], required=True)
    embedding.add_argument(
        "--disable-prefix-caching", action="store_true", required=True
    )
    embedding.add_argument("--tensor-parallel-size", type=int, required=True)
    return value


def main() -> None:
    args = parser().parse_args()
    if args.command == "describe":
        print(json.dumps(description(), sort_keys=True))
    else:
        embed(args)


if __name__ == "__main__":
    main()
