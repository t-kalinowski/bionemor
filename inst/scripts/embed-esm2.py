#!/usr/bin/env python3

"""Run pinned ESM-2 models and write portable protein embeddings."""

from __future__ import annotations

import argparse
from collections.abc import Iterable
import gzip
import hashlib
import importlib
import importlib.metadata
import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Any


PROTOCOL_VERSION = 2
HELPER_VERSION = "0.3.0"
EXECUTION_SCHEMA_VERSION = 1
DRIVER = "esm2-transformers"
TRANSFORMERS_VERSION = "5.14.1"


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


def write_gzip(path: Path, chunks: Iterable[bytes]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary: Path | None = None
    try:
        with tempfile.NamedTemporaryFile(
            prefix=f".{path.name}-", dir=path.parent, delete=False
        ) as raw:
            temporary = Path(raw.name)
            with gzip.GzipFile(
                filename="", mode="wb", compresslevel=1, fileobj=raw, mtime=0
            ) as stream:
                for chunk in chunks:
                    stream.write(chunk)
            raw.flush()
            os.fsync(raw.fileno())
        os.replace(temporary, path)
    finally:
        if temporary is not None:
            temporary.unlink(missing_ok=True)


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def md5_file(path: Path) -> str:
    with path.open("rb") as stream:
        return hashlib.file_digest(stream, "md5").hexdigest()


def package_version(name: str) -> str | None:
    try:
        return importlib.metadata.version(name)
    except importlib.metadata.PackageNotFoundError:
        return None


def compatible_transformers_version(version: str | None) -> bool:
    return version == TRANSFORMERS_VERSION


def import_available(module: str) -> bool:
    try:
        importlib.import_module(module)
    except Exception:
        return False
    return True


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
        "transformers": package_version("transformers"),
        "transformer_engine": package_version("transformer-engine"),
    }


def description() -> dict[str, Any]:
    version = package_version("transformers")
    runtime = gpu_runtime()
    imports = {
        "torch": True,
        "transformers": import_available("transformers"),
        "transformer_engine": all(
            import_available(module)
            for module in (
                "transformer_engine.common.recipe",
                "transformer_engine.pytorch",
                "transformer_engine.pytorch.attention.rope",
            )
        ),
    }
    runtime["imports"] = imports
    available = (
        all(imports.values())
        and compatible_transformers_version(version)
        and runtime["cuda_available"]
    )
    return {
        "protocol_version": PROTOCOL_VERSION,
        "helper_version": HELPER_VERSION,
        "helper_sha256": sha256_file(Path(__file__).resolve()),
        "recipe_version": f"transformers-{TRANSFORMERS_VERSION}",
        "recipe_revision": os.environ.get("BIONEMOR_RECIPE_REVISION"),
        "driver": DRIVER,
        "execution_schema_version": EXECUTION_SCHEMA_VERSION,
        "semantic_operations": ["embed"],
        "commands": {"embed": available},
        "features": {"pooled_embeddings_f32_gzip": available},
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


def atomic_write_pooled_embeddings(
    prefix: Path, ids: list[str], rows: list[Any]
) -> None:
    assert rows and len(ids) == len(rows)
    width = rows[0].numel()
    assert all(
        value.ndim == 1
        and value.numel() == width
        and str(value.dtype) == "torch.float32"
        for value in rows
    )
    chunks = (
        value.contiguous().numpy().astype("<f4", copy=False).tobytes(order="C")
        for value in rows
    )
    data_path = Path(f"{prefix}.f32.gz")
    metadata_path = Path(f"{prefix}.json")
    write_gzip(data_path, chunks)
    atomic_write_text(
        metadata_path,
        json.dumps(
            {
                "format": "bionemor-pooled-embeddings",
                "version": 1,
                "shape": [len(rows), width],
                "ids": ids,
                "source_dtype": "float32",
                "storage_dtype": "float32",
                "byte_order": "little",
                "order": "row-major",
                "compression": "gzip",
                "compression_level": 1,
                "uncompressed_bytes": len(rows) * width * 4,
                "data_md5": md5_file(data_path),
            },
            indent=2,
            sort_keys=True,
        )
        + "\n",
    )


def embed(args: argparse.Namespace) -> None:
    import torch
    from transformers import AutoModel, AutoTokenizer

    ids, sequences = read_requests(args.input)
    load_args: dict[str, Any] = {
        "trust_remote_code": True,
    }
    if args.revision is not None:
        load_args["revision"] = args.revision
    torch.manual_seed(42)
    torch.cuda.manual_seed_all(42)
    tokenizer = AutoTokenizer.from_pretrained(args.model, **load_args)
    model = (
        AutoModel.from_pretrained(args.model, **load_args)
        .to("cuda", dtype=torch.float32)
        .eval()
    )
    rows = []
    width = None
    with torch.inference_mode():
        for identifier, sequence in zip(ids, sequences, strict=True):
            inputs = tokenizer(sequence, return_tensors="pt")
            inputs = {name: value.to("cuda") for name, value in inputs.items()}
            hidden = model(**inputs).last_hidden_state[0, -1, :].float()
            norm = torch.linalg.vector_norm(hidden)
            if not bool(torch.isfinite(hidden).all()) or norm.item() <= 1e-9:
                raise RuntimeError("ESM-2 returned an invalid embedding")
            values = (hidden / norm).detach().cpu()
            if width is None:
                width = values.numel()
            elif values.numel() != width:
                raise RuntimeError("ESM-2 returned embeddings with different widths")
            rows.append(values)
    atomic_write_pooled_embeddings(args.output, ids, rows)


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
    return value


def main() -> None:
    args = parser().parse_args()
    if args.command == "describe":
        print(json.dumps(description(), sort_keys=True))
    else:
        embed(args)


if __name__ == "__main__":
    main()
