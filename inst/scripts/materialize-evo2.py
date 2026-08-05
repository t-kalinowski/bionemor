#!/usr/bin/env python3

"""Portable output and checkpoint inspection helper for bionemor."""

from __future__ import annotations

import argparse
import hashlib
import importlib.metadata
import json
import math
import os
import pickletools
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Any

import torch


PROTOCOL_VERSION = 1
HELPER_VERSION = "0.1.0"
EXECUTION_SCHEMA_VERSION = 1
DRIVER = "evo2-megatron"
DNA_COMPLEMENT = str.maketrans(
    "ACGTRYSWKMBDHVNacgtryswkmbdhvn",
    "TGCAYRSWMKVHDBNtgcayrswmkvhdbn",
)


class PortableOutputError(RuntimeError):
    def __init__(self, message: str, exit_status: int) -> None:
        super().__init__(message)
        self.exit_status = exit_status


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


def atomic_write_json(path: Path, value: Any) -> None:
    atomic_write_text(path, json.dumps(value, indent=2, sort_keys=True) + "\n")


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


def import_available(module: str) -> bool:
    try:
        __import__(module)
    except Exception:
        return False
    return True


def recipe_tokenizer_paths() -> dict[str, str]:
    from bionemo.evo2.data.dataset_tokenizer import (
        DEFAULT_HF_TOKENIZER_MODEL_PATH,
        DEFAULT_HF_TOKENIZER_MODEL_PATH_512,
    )

    paths = {
        "nucleotide_fast_tokenizer_256": Path(DEFAULT_HF_TOKENIZER_MODEL_PATH),
        "nucleotide_fast_tokenizer_512": Path(DEFAULT_HF_TOKENIZER_MODEL_PATH_512),
    }
    missing = [str(path) for path in paths.values() if not path.is_dir()]
    if missing:
        raise RuntimeError(f"recipe tokenizer directory is missing: {missing[0]}")
    return {name: str(path.resolve()) for name, path in paths.items()}


def capabilities() -> dict[str, Any]:
    commands = {
        "infer_evo2": shutil.which("infer_evo2") is not None,
        "predict_evo2": shutil.which("predict_evo2") is not None,
        "train_evo2": shutil.which("train_evo2") is not None,
        "preprocess_evo2": shutil.which("preprocess_evo2") is not None,
        "savanna_to_mbridge": shutil.which("evo2_convert_savanna_to_mbridge")
        is not None,
        "nemo2_to_mbridge": shutil.which("evo2_convert_nemo2_to_mbridge") is not None,
        "mbridge_to_vortex": shutil.which("evo2_export_mbridge_to_vortex") is not None,
        "remove_optimizer": shutil.which("evo2_remove_optimizer") is not None,
    }
    cuda_available = torch.cuda.is_available()
    gpu_count = torch.cuda.device_count()
    gpus = []
    driver = None
    if cuda_available:
        driver_probe = subprocess.run(
            [
                "nvidia-smi",
                "--query-gpu=driver_version",
                "--format=csv,noheader",
            ],
            check=True,
            capture_output=True,
            text=True,
        )
        drivers = {
            line.strip() for line in driver_probe.stdout.splitlines() if line.strip()
        }
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
        "protocol_version": PROTOCOL_VERSION,
        "helper_version": HELPER_VERSION,
        "helper_sha256": sha256_file(Path(__file__).resolve()),
        "recipe_version": package_version("bionemo-evo2") or "unknown",
        "recipe_revision": os.environ.get("BIONEMOR_RECIPE_REVISION"),
        "tokenizers": recipe_tokenizer_paths(),
        "commands": commands,
        "features": {
            "generation_jsonl": commands["infer_evo2"],
            "generation_log_probs": commands["infer_evo2"],
            "score_sum": commands["predict_evo2"],
            "score_mean": commands["predict_evo2"],
            "score_per_token": commands["predict_evo2"],
            "embedding_layer": commands["predict_evo2"],
            "lora": commands["train_evo2"],
        },
        "runtime": {
            "python": sys.version.split()[0],
            "pytorch": torch.__version__,
            "cuda": torch.version.cuda,
            "cuda_available": cuda_available,
            "gpu_count": gpu_count,
            "driver": driver,
            "gpus": gpus,
            "transformer_engine": package_version("transformer-engine"),
            "megatron_bridge": package_version("megatron-bridge"),
            "megatron_core": package_version("megatron-core"),
            "imports": {
                "torch": True,
                "bionemo": import_available("bionemo.evo2"),
                "megatron_bridge": import_available("megatron.bridge"),
                "transformer_engine": import_available("transformer_engine"),
            },
        },
    }


def description() -> dict[str, Any]:
    return {
        **capabilities(),
        "driver": DRIVER,
        "execution_schema_version": EXECUTION_SCHEMA_VERSION,
        "semantic_operations": ["generate"],
    }


def checkpoint_iteration(root: Path) -> Path | None:
    latest = root / "latest_checkpointed_iteration.txt"
    if latest.exists():
        value = latest.read_text(encoding="utf-8").strip()
        if not value.isdigit():
            raise RuntimeError(f"invalid checkpoint iteration in {latest}")
        selected = root / f"iter_{int(value):07d}"
        if not selected.is_dir():
            raise RuntimeError(f"checkpoint iteration does not exist: {selected}")
        return selected
    iterations = sorted(
        (
            item
            for item in root.glob("iter_*")
            if item.is_dir() and item.name.removeprefix("iter_").isdigit()
        ),
        key=lambda item: int(item.name.removeprefix("iter_")),
    )
    return iterations[-1] if iterations else None


def resolve_checkpoint(path: Path) -> tuple[Path, Path]:
    path = path.expanduser().resolve(strict=True)
    if not path.is_dir():
        raise RuntimeError(f"MBridge checkpoint must be a directory: {path}")
    if (path / "run_config.yaml").is_file():
        return path, path
    selected = checkpoint_iteration(path)
    if selected is not None and (selected / "run_config.yaml").is_file():
        return path, selected
    if path.name.startswith("iter_") and (path / "run_config.yaml").is_file():
        return path.parent, path
    raise RuntimeError(
        f"checkpoint has no direct run_config.yaml or valid iter_* checkpoint: {path}"
    )


def nested_find(value: Any, names: set[str]) -> Any:
    if isinstance(value, dict):
        for key, item in value.items():
            normalized = str(key).lower().replace("-", "_")
            if normalized in names and item is not None:
                return item
        for item in value.values():
            found = nested_find(item, names)
            if found is not None:
                return found
    elif isinstance(value, list):
        for item in value:
            found = nested_find(item, names)
            if found is not None:
                return found
    return None


def infer_model_size(config: dict[str, Any]) -> str | None:
    explicit = nested_find(config, {"model_size", "model_name"})
    if isinstance(explicit, str):
        return explicit
    model = config.get("model", config)
    provider = nested_find(model, {"_target_", "target"})
    if isinstance(provider, str):
        providers = {
            "Hyena1bModelProvider": "evo2_1b_base",
            "Hyena7bModelProvider": "evo2_7b_base",
            "Hyena7bARCLongContextModelProvider": "evo2_7b",
            "Hyena20bARCModelProvider": "evo2_20b",
            "Hyena40bModelProvider": "evo2_40b_base",
            "Hyena40bARCLongContextModelProvider": "evo2_40b",
        }
        model_size = providers.get(provider.rsplit(".", maxsplit=1)[-1])
        if model_size is not None:
            return model_size
    layers = nested_find(model, {"num_layers"})
    hidden = nested_find(model, {"hidden_size"})
    sequence = nested_find(model, {"seq_length", "sequence_length"})
    if not isinstance(layers, int) or not isinstance(hidden, int):
        return None
    known = {
        (25, 1920): ("evo2_1b_base", "evo2_1b"),
        (32, 4096): ("evo2_7b_base", "evo2_7b"),
        (24, 8192): ("evo2_20b", "evo2_20b"),
        (50, 8192): ("evo2_40b_base", "evo2_40b"),
    }
    choices = known.get((layers, hidden))
    if choices is None:
        return None
    return choices[1] if isinstance(sequence, int) and sequence > 8192 else choices[0]


def checkpoint_transformer_engine(path: Path) -> bool | None:
    string_opcodes = {
        "UNICODE",
        "BINUNICODE",
        "SHORT_BINUNICODE",
        "BINUNICODE8",
    }
    try:
        with (path / ".metadata").open("rb") as stream:
            metadata_strings = [
                argument
                for opcode, argument, _ in pickletools.genops(stream)
                if opcode.name in string_opcodes and isinstance(argument, str)
            ]
    except (OSError, ValueError):
        return None
    te_markers = (
        ".mixer.dense_projection.layer_norm_weight",
        ".self_attention.linear_qkv.layer_norm_weight",
        ".mlp.linear_fc1.layer_norm_weight",
    )
    transformer_engine = any(
        marker in value for value in metadata_strings for marker in te_markers
    )
    non_te = any(
        value.endswith(".pre_mlp_layernorm.weight")
        or value.endswith(".input_layernorm.weight")
        or re.search(r"(?:^|[.])decoder[.]layers[.][0-9]+[.]norm[.]weight$", value)
        for value in metadata_strings
    )
    if transformer_engine and non_te:
        raise RuntimeError(
            "MBridge checkpoint mixes Transformer Engine and non-TE layernorm keys"
        )
    if transformer_engine:
        return True
    if non_te:
        return False
    return None


def checkpoint_inspection(path: Path) -> dict[str, Any]:
    try:
        import yaml
    except ImportError as error:
        raise RuntimeError(
            "PyYAML is required to inspect MBridge checkpoints"
        ) from error

    root, selected = resolve_checkpoint(path)
    metadata = selected / ".metadata"
    shards = sorted(
        shard
        for shard in selected.glob("*.distcp")
        if shard.is_file() and shard.stat().st_size > 0
    )
    if not metadata.is_file():
        raise RuntimeError(
            f"MBridge checkpoint is missing distributed checkpoint metadata: {metadata}"
        )
    if not shards:
        raise RuntimeError(
            f"MBridge checkpoint has no distributed checkpoint weight shard: {selected}"
        )
    transformer_engine = checkpoint_transformer_engine(selected)
    config_path = selected / "run_config.yaml"
    config = yaml.safe_load(config_path.read_text(encoding="utf-8"))
    if not isinstance(config, dict):
        raise RuntimeError(f"run_config.yaml must contain a mapping: {config_path}")
    model_size = infer_model_size(config)
    if not isinstance(model_size, str):
        raise RuntimeError("run_config.yaml does not identify a supported model size")

    peft = config.get("peft")
    explicit_kind = nested_find(config, {"kind", "checkpoint_kind"})
    kind = "lora" if peft not in (None, False, {}) else "dense"
    if explicit_kind in {"dense", "lora"}:
        kind = explicit_kind
    tokenizer = selected / "tokenizer"
    tokenizer_value = (
        str(tokenizer)
        if tokenizer.is_dir()
        else nested_find(
            config,
            {
                "hf_tokenizer_model_path",
                "hf_tokenizer_model_or_path",
                "tokenizer_path",
            },
        )
    )
    precision = nested_find(
        config,
        {"mixed_precision_recipe", "precision_config", "mixed_precision"},
    )
    if isinstance(precision, dict):
        precision = precision.get("_target_") or precision.get("name")
    base_checkpoint = nested_find(
        config,
        {"pretrained_checkpoint", "base_checkpoint", "base_checkpoint_path"},
    )
    provider = nested_find(config.get("model", {}), {"_target_", "target"})
    vortex_style_fp8 = nested_find(
        config.get("model", {}),
        {"vortex_style_fp8"},
    )
    if not isinstance(vortex_style_fp8, bool):
        vortex_style_fp8 = False

    return {
        "path": str(root),
        "resolved_path": str(selected),
        "run_config": str(config_path),
        "model_provider": provider,
        "model_size": model_size,
        "kind": kind,
        "vortex_style_fp8": vortex_style_fp8,
        "transformer_engine": transformer_engine,
        "tokenizer": tokenizer_value,
        "mixed_precision_recipe": precision,
        "base_checkpoint": base_checkpoint,
        "distributed_checkpoint": {
            "metadata": str(metadata),
            "weight_shards": [str(shard) for shard in shards],
        },
    }


def prediction_files(directory: Path) -> list[Path]:
    files = sorted(directory.rglob("predictions__*.pt"))
    if not files:
        raise RuntimeError(f"no prediction rank files in {directory}")
    return files


def load_sequence_map(path: Path) -> list[dict[str, Any]]:
    text = path.read_text(encoding="utf-8").strip()
    if not text:
        raise RuntimeError("sequence map is empty")
    lines = [line.strip() for line in text.splitlines() if line.strip()]
    if text.startswith("["):
        value = json.loads(text)
    elif len(lines) > 1 and all(
        line.startswith("{") and line.endswith("}") for line in lines
    ):
        value = [json.loads(line) for line in lines]
    elif text.startswith("{"):
        value = json.loads(text)
        if isinstance(value, dict) and all(
            isinstance(item, list) for item in value.values()
        ):
            value = [
                {name: column[index] for name, column in value.items()}
                for index in range(len(next(iter(value.values()))))
            ]
        else:
            value = [value]
    else:
        value = [json.loads(line) for line in lines]
    if (
        not isinstance(value, list)
        or not value
        or not all(isinstance(row, dict) for row in value)
    ):
        raise RuntimeError("sequence map must contain one or more JSON records")
    return value


def reverse_complement(sequence: str) -> str:
    complement = sequence.translate(DNA_COMPLEMENT)
    if len(complement) != len(sequence) or any(
        base not in "ACGTRYSWKMBDHVNacgtryswkmbdhvn" for base in sequence
    ):
        raise RuntimeError("reverse-strand sequences must use IUPAC DNA symbols")
    return complement[::-1]


def expected_sequence_rows(
    path: Path, prediction_directory: Path
) -> tuple[dict[int, dict[str, Any]], list[str]]:
    required = {"derived_id", "id", "strand", "sequence", "sequence_length"}
    rows = load_sequence_map(path)
    by_derived_id: dict[str, dict[str, Any]] = {}
    original_order: list[str] = []
    original_records: dict[str, tuple[str, int, set[str]]] = {}
    result: dict[int, dict[str, Any]] = {}
    for row in rows:
        if not required.issubset(row):
            missing = sorted(required - set(row))
            raise RuntimeError(f"sequence map row is missing fields: {missing}")
        derived_id = row["derived_id"]
        original_id = row["id"]
        strand = row["strand"]
        sequence = row["sequence"]
        sequence_length = row["sequence_length"]
        if (
            not isinstance(derived_id, str)
            or not derived_id
            or derived_id in by_derived_id
        ):
            raise RuntimeError("sequence map contains invalid or duplicate derived IDs")
        if not isinstance(original_id, str) or not original_id:
            raise RuntimeError("sequence map contains an invalid original ID")
        if strand not in {"forward", "reverse"}:
            raise RuntimeError(f"sequence map contains an invalid strand: {strand!r}")
        if not isinstance(sequence, str) or not sequence:
            raise RuntimeError(
                f"sequence map contains an invalid sequence for {derived_id}"
            )
        if (
            not isinstance(sequence_length, int)
            or isinstance(sequence_length, bool)
            or sequence_length != len(sequence)
        ):
            raise RuntimeError(
                f"sequence length does not match the supplied sequence for {derived_id}"
            )
        supplied_sequence = (
            sequence if strand == "forward" else reverse_complement(sequence)
        )
        row["_supplied_sequence"] = supplied_sequence
        original = original_records.get(original_id)
        if original is None:
            original_records[original_id] = (
                supplied_sequence,
                sequence_length,
                {strand},
            )
        else:
            original_sequence, original_length, strands = original
            if (
                supplied_sequence != original_sequence
                or sequence_length != original_length
            ):
                raise RuntimeError(
                    f"sequence map has inconsistent supplied sequences for {original_id}"
                )
            if strand in strands:
                raise RuntimeError(
                    f"sequence map repeats strand {strand} for {original_id}"
                )
            strands.add(strand)
        by_derived_id[derived_id] = row
        if original_id not in original_order:
            original_order.append(original_id)

    index_path = prediction_directory / "seq_idx_map.json"
    if not index_path.is_file():
        raise RuntimeError(
            f"prediction sequence index map does not exist: {index_path}"
        )
    index_map = json.loads(index_path.read_text(encoding="utf-8"))
    if not isinstance(index_map, dict):
        raise RuntimeError("prediction sequence index map must be a JSON object")
    if set(index_map) != set(by_derived_id):
        raise RuntimeError(
            "prediction sequence index map IDs do not match the sequence map"
        )
    for derived_id, index in index_map.items():
        if (
            not isinstance(index, int)
            or isinstance(index, bool)
            or index < 0
            or index in result
        ):
            raise RuntimeError(
                "prediction sequence index map contains invalid or duplicate indices"
            )
        result[index] = by_derived_id[derived_id]
    return result, original_order


def indexed_predictions(
    directory: Path,
    expected: dict[int, dict[str, Any]],
    required: set[str],
    optional: set[str],
) -> tuple[
    dict[int, dict[str, torch.Tensor]],
    dict[str, str],
    dict[str, str],
]:
    records: dict[int, dict[str, torch.Tensor]] = {}
    dtypes: dict[str, set[str]] = {key: set() for key in required}
    hashes: dict[str, str] = {}
    for path in prediction_files(directory):
        payload = torch.load(path, map_location="cpu", weights_only=True)
        if not isinstance(payload, dict):
            raise RuntimeError(f"prediction file is not a tensor mapping: {path}")
        keys = set(payload)
        if not required.issubset(keys) or not keys.issubset(required | optional):
            raise RuntimeError(
                f"unexpected prediction tensor schema in {path}: {sorted(keys)}"
            )
        for key in keys:
            tensor = payload[key]
            if not isinstance(tensor, torch.Tensor):
                raise RuntimeError(f"prediction value {key} is not a tensor")
        indices_tensor = payload["seq_idx"]
        if (
            indices_tensor.ndim != 1
            or indices_tensor.dtype == torch.bool
            or indices_tensor.is_floating_point()
            or indices_tensor.is_complex()
        ):
            raise RuntimeError(
                f"seq_idx must be a one-dimensional integer tensor: {path}"
            )
        indices = [int(item) for item in indices_tensor.tolist()]
        for key in required - {"seq_idx"}:
            tensor = payload[key]
            if tensor.ndim == 0 or tensor.shape[0] != len(indices):
                raise RuntimeError(
                    f"prediction tensor {key} has an inconsistent batch shape in {path}"
                )
        for row, index in enumerate(indices):
            if index in records:
                raise RuntimeError(
                    f"prediction output contains duplicate sequence index {index}"
                )
            records[index] = {key: payload[key][row] for key in required - {"seq_idx"}}
        for key in required:
            dtypes[key].add(str(payload[key].dtype))
        hashes[str(path.relative_to(directory))] = sha256_file(path)
    if set(records) != set(expected):
        raise RuntimeError("prediction output indices do not match the sequence map")
    inconsistent = [key for key, values in dtypes.items() if len(values) != 1]
    if inconsistent:
        raise RuntimeError(
            f"prediction tensor dtypes differ across rank files: {inconsistent}"
        )
    return records, {key: values.pop() for key, values in dtypes.items()}, hashes


def ensure_finite(tensor: torch.Tensor, name: str) -> None:
    if tensor.is_floating_point() and not bool(torch.isfinite(tensor).all()):
        raise RuntimeError(f"{name} contains non-finite values")


def boolean_mask(tensor: torch.Tensor, name: str) -> torch.Tensor:
    if tensor.ndim != 1 or tensor.is_complex():
        raise RuntimeError(f"{name} must be a one-dimensional binary tensor")
    ensure_finite(tensor, name)
    if not bool(((tensor == 0) | (tensor == 1)).all()):
        raise RuntimeError(f"{name} must contain only zero and one")
    return tensor.bool()


def write_json_lines(path: Path, rows: list[dict[str, Any]]) -> None:
    atomic_write_text(
        path,
        "".join(json.dumps(row, separators=(",", ":")) + "\n" for row in rows),
    )


def atomic_write_parquet(
    path: Path, rows: list[dict[str, Any]], schema: dict[str, Any]
) -> None:
    try:
        import polars as pl
    except ImportError as error:
        raise RuntimeError("polars is required to write prediction Parquet") from error

    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=f".{path.name}-", suffix=".parquet", dir=path.parent
    )
    os.close(descriptor)
    temporary = Path(temporary_name)
    try:
        frame = pl.DataFrame(rows, schema=schema)
        frame.write_parquet(temporary)
        with temporary.open("rb+") as stream:
            os.fsync(stream.fileno())
        os.replace(temporary, path)
    finally:
        temporary.unlink(missing_ok=True)


def validate_prediction_arguments(args: argparse.Namespace) -> None:
    if args.mode == "score":
        if args.reduction is None:
            raise RuntimeError("--reduction is required for score materialization")
        if args.pool is not None:
            raise RuntimeError("--pool is only valid for pooled embeddings")
    elif args.mode == "embedding-pooled":
        if args.pool is None:
            raise RuntimeError("--pool is required for pooled embeddings")
        if args.reduction is not None:
            raise RuntimeError("--reduction is only valid for score materialization")
    elif args.reduction is not None or args.pool is not None:
        raise RuntimeError(
            "--reduction and --pool are not valid for this materialization mode"
        )


def score_rows(
    args: argparse.Namespace,
    expected: dict[int, dict[str, Any]],
) -> tuple[list[dict[str, Any]], dict[str, str], dict[str, str]]:
    predictions, dtypes, hashes = indexed_predictions(
        args.input_dir,
        expected,
        {"seq_idx", "log_probs_seqs", "loss_mask"},
        {"batch_idx"},
    )
    rows: list[dict[str, Any]] = []
    for index, source in expected.items():
        values = predictions[index]["log_probs_seqs"]
        if values.ndim != 1 or not values.is_floating_point():
            raise RuntimeError(
                f"log_probs_seqs must be a one-dimensional floating tensor for {source['derived_id']}"
            )
        mask = boolean_mask(
            predictions[index]["loss_mask"],
            f"loss_mask for {source['derived_id']}",
        )
        if values.shape != mask.shape:
            raise RuntimeError(
                f"log_probs_seqs and loss_mask shapes differ for {source['derived_id']}"
            )
        ensure_finite(values, f"log_probs_seqs for {source['derived_id']}")
        selected = values[mask]
        tokens_scored = int(selected.numel())
        if args.reduction == "mean" and tokens_scored == 0:
            raise RuntimeError(
                f"cannot compute a mean with no scored tokens for {source['derived_id']}"
            )
        score = selected.sum() if args.reduction == "sum" else selected.mean()
        rows.append(
            {
                "derived_id": source["derived_id"],
                "score": float(score),
                "tokens_scored": tokens_scored,
            }
        )
    return rows, dtypes, hashes


def positional_values(
    values: torch.Tensor,
    mask_tensor: torch.Tensor,
    source: dict[str, Any],
) -> dict[int, float]:
    derived_id = source["derived_id"]
    if values.ndim != 1 or not values.is_floating_point():
        raise RuntimeError(
            f"log_probs_seqs must be a one-dimensional floating tensor for {derived_id}"
        )
    mask = boolean_mask(mask_tensor, f"loss_mask for {derived_id}")
    if values.shape != mask.shape:
        raise RuntimeError(
            f"log_probs_seqs and loss_mask shapes differ for {derived_id}"
        )
    ensure_finite(values, f"log_probs_seqs for {derived_id}")
    selected = values[mask]
    sequence_length = source["sequence_length"]
    active = mask.nonzero(as_tuple=True)[0].tolist()
    if selected.numel() == sequence_length:
        if active != list(range(sequence_length)):
            raise RuntimeError(
                f"loss_mask positions are inconsistent with the supplied sequence for {derived_id}"
            )
        derived_positions = range(1, sequence_length + 1)
    elif selected.numel() == sequence_length - 1:
        if active != list(range(sequence_length - 1)):
            raise RuntimeError(
                f"loss_mask positions are inconsistent with the supplied sequence for {derived_id}"
            )
        derived_positions = range(2, sequence_length + 1)
    else:
        raise RuntimeError(
            f"loss_mask selects an inconsistent number of profile positions for {derived_id}"
        )
    result: dict[int, float] = {}
    for position, value in zip(derived_positions, selected.tolist()):
        supplied_position = (
            position
            if source["strand"] == "forward"
            else sequence_length - position + 1
        )
        result[supplied_position] = float(value)
    return result


def profile_rows(
    args: argparse.Namespace,
    expected: dict[int, dict[str, Any]],
    original_order: list[str],
) -> tuple[list[dict[str, Any]], dict[str, str], dict[str, str]]:
    predictions, dtypes, hashes = indexed_predictions(
        args.input_dir,
        expected,
        {"seq_idx", "log_probs_seqs", "loss_mask"},
        {"batch_idx"},
    )
    grouped: dict[str, dict[str, tuple[dict[str, Any], dict[int, float]]]] = {}
    for index, source in expected.items():
        strands = grouped.setdefault(source["id"], {})
        if source["strand"] in strands:
            raise RuntimeError(
                f"sequence map repeats strand {source['strand']} for {source['id']}"
            )
        strands[source["strand"]] = (
            source,
            positional_values(
                predictions[index]["log_probs_seqs"],
                predictions[index]["loss_mask"],
                source,
            ),
        )

    rows: list[dict[str, Any]] = []
    for original_id in original_order:
        strands = grouped[original_id]
        sources = [entry[0] for entry in strands.values()]
        if len({source["_supplied_sequence"] for source in sources}) != 1:
            raise RuntimeError(
                f"sequence map has inconsistent supplied sequences for {original_id}"
            )
        sequence = sources[0]["_supplied_sequence"]
        if set(strands) == {"forward", "reverse"}:
            forward_positions = set(strands["forward"][1])
            reverse_positions = set(strands["reverse"][1])
            if forward_positions != reverse_positions:
                raise RuntimeError(
                    f"forward and reverse profile masks do not align for {original_id}"
                )
            positions = sorted(forward_positions)
            output_strand = "both"
            values_by_position = {
                position: (
                    strands["forward"][1][position] + strands["reverse"][1][position]
                )
                / 2
                for position in positions
            }
        elif len(strands) == 1:
            output_strand = next(iter(strands))
            values_by_position = strands[output_strand][1]
            positions = sorted(values_by_position)
        else:
            raise RuntimeError(
                f"sequence map has an incomplete strand set for {original_id}"
            )
        for position in positions:
            rows.append(
                {
                    "id": original_id,
                    "position": position,
                    "base": sequence[position - 1],
                    "log_probability": values_by_position[position],
                    "strand": output_strand,
                }
            )
    return rows, dtypes, hashes


def pooled_embedding(selected: torch.Tensor, pool: str) -> torch.Tensor:
    if pool == "mean":
        return selected.mean(dim=0)
    if pool == "max":
        return selected.max(dim=0).values
    if pool == "first":
        return selected[0]
    if pool == "last":
        return selected[-1]
    raise AssertionError(f"unhandled embedding pool: {pool}")


def embedding_rows(
    args: argparse.Namespace,
    expected: dict[int, dict[str, Any]],
    original_order: list[str],
) -> tuple[list[dict[str, Any]], dict[str, str], dict[str, str], int]:
    predictions, dtypes, hashes = indexed_predictions(
        args.input_dir,
        expected,
        {"seq_idx", "hidden_embeddings", "pad_mask", "tokens"},
        {"batch_idx"},
    )
    selected_by_index: dict[int, torch.Tensor] = {}
    for index, source in expected.items():
        embeddings = predictions[index]["hidden_embeddings"]
        pad_mask = predictions[index]["pad_mask"]
        tokens = predictions[index]["tokens"]
        if embeddings.ndim != 2 or not embeddings.is_floating_point():
            raise RuntimeError(
                f"hidden_embeddings must be a two-dimensional floating tensor for {source['derived_id']}"
            )
        mask = boolean_mask(pad_mask, f"pad_mask for {source['derived_id']}")
        if (
            tokens.ndim != 1
            or tokens.dtype == torch.bool
            or tokens.is_floating_point()
            or tokens.is_complex()
        ):
            raise RuntimeError(
                f"tokens must be a one-dimensional integer tensor for {source['derived_id']}"
            )
        if embeddings.shape[0] != mask.shape[0] or tokens.shape != mask.shape:
            raise RuntimeError(
                f"embedding, token, and pad-mask shapes differ for {source['derived_id']}"
            )
        ensure_finite(embeddings, f"hidden_embeddings for {source['derived_id']}")
        selected = embeddings[mask]
        if selected.shape[0] != source["sequence_length"]:
            raise RuntimeError(
                f"pad_mask selects an inconsistent number of embeddings for {source['derived_id']}"
            )
        active = mask.nonzero(as_tuple=True)[0].tolist()
        sequence_length = source["sequence_length"]
        if active not in (
            list(range(sequence_length)),
            list(range(1, sequence_length + 1)),
        ):
            raise RuntimeError(
                f"pad_mask positions are inconsistent with the supplied sequence for {source['derived_id']}"
            )
        selected_by_index[index] = selected

    width = next(iter(selected_by_index.values())).shape[1]
    if any(value.shape[1] != width for value in selected_by_index.values()):
        raise RuntimeError("embedding outputs do not have one common width")

    if args.mode == "embedding-pooled":
        grouped: dict[str, dict[str, torch.Tensor]] = {}
        for index, source in expected.items():
            strands = grouped.setdefault(source["id"], {})
            if source["strand"] in strands:
                raise RuntimeError(
                    f"sequence map repeats strand {source['strand']} for {source['id']}"
                )
            strands[source["strand"]] = pooled_embedding(
                selected_by_index[index], args.pool
            )
        rows: list[dict[str, Any]] = []
        for original_id in original_order:
            strands = grouped[original_id]
            if set(strands) == {"forward", "reverse"}:
                value = (strands["forward"] + strands["reverse"]) / 2
            elif len(strands) == 1:
                value = next(iter(strands.values()))
            else:
                raise RuntimeError(
                    f"sequence map has an incomplete strand set for {original_id}"
                )
            rows.append({"id": original_id, "embedding": value.tolist()})
        return rows, dtypes, hashes, width

    grouped_strands: dict[str, set[str]] = {}
    for source in expected.values():
        grouped_strands.setdefault(source["id"], set()).add(source["strand"])
    bidirectional = [
        original_id
        for original_id, strands in grouped_strands.items()
        if strands == {"forward", "reverse"}
    ]
    if bidirectional:
        raise RuntimeError(
            "unpooled bidirectional embeddings do not have defined coordinate semantics"
        )

    rows = []
    index_order = {
        original_id: offset for offset, original_id in enumerate(original_order)
    }
    for index, source in expected.items():
        values = selected_by_index[index]
        for offset, value in enumerate(values.tolist(), start=1):
            position = (
                offset
                if source["strand"] == "forward"
                else source["sequence_length"] - offset + 1
            )
            rows.append(
                {
                    "id": source["id"],
                    "position": position,
                    "embedding": value,
                    "strand": source["strand"],
                }
            )
    rows.sort(key=lambda row: (index_order[row["id"]], row["position"]))
    return rows, dtypes, hashes, width


def materialize_predictions(args: argparse.Namespace) -> None:
    validate_prediction_arguments(args)
    expected, original_order = expected_sequence_rows(args.sequence_map, args.input_dir)
    if args.mode == "score":
        rows, dtypes, input_hashes = score_rows(args, expected)
        write_json_lines(args.output, rows)
        shape = [len(rows)]
        primary_dtype = dtypes["log_probs_seqs"]
    elif args.mode == "profile":
        rows, dtypes, input_hashes = profile_rows(args, expected, original_order)
        try:
            import polars as pl
        except ImportError as error:
            raise RuntimeError(
                "polars is required to write prediction Parquet"
            ) from error
        atomic_write_parquet(
            args.output,
            rows,
            {
                "id": pl.String,
                "position": pl.Int64,
                "base": pl.String,
                "log_probability": pl.Float64,
                "strand": pl.String,
            },
        )
        shape = [len(rows), 5]
        primary_dtype = dtypes["log_probs_seqs"]
    else:
        rows, dtypes, input_hashes, width = embedding_rows(
            args, expected, original_order
        )
        if args.mode == "embedding-pooled":
            write_json_lines(args.output, rows)
            shape = [len(rows), width]
        else:
            try:
                import polars as pl
            except ImportError as error:
                raise RuntimeError(
                    "polars is required to write prediction Parquet"
                ) from error
            atomic_write_parquet(
                args.output,
                rows,
                {
                    "id": pl.String,
                    "position": pl.Int64,
                    "embedding": pl.List(pl.Float64),
                    "strand": pl.String,
                },
            )
            shape = [len(rows), 4]
        primary_dtype = dtypes["hidden_embeddings"]

    summary_path = args.summary or Path(f"{args.output}.summary.json")
    summary = {
        "rows": len(rows),
        "mode": args.mode,
        "shape": shape,
        "dtype": primary_dtype,
        "prediction_sha256": input_hashes,
        "sequence_index_sha256": sha256_file(args.input_dir / "seq_idx_map.json"),
        "sequence_map_sha256": sha256_file(args.sequence_map),
        "output_sha256": sha256_file(args.output),
    }
    if args.mode == "embedding-unpooled":
        summary["schema"] = {
            "id": "string",
            "position": "int64",
            "embedding": "list<double>",
            "strand": "string",
        }
    atomic_write_json(summary_path, summary)


def generation_log_probabilities(row: dict[str, Any]) -> list[float] | None:
    values = row.get("log_probabilities")
    if values is None:
        logprobs = row.get("logprobs")
        if isinstance(logprobs, dict):
            values = logprobs.get("completion_logprobs")
    if values is None:
        return None
    if not isinstance(values, list):
        raise PortableOutputError("generation log probabilities must be a list", 65)
    if values and all(isinstance(value, dict) for value in values):
        values = [
            value.get("logprob", value.get("log_probability")) for value in values
        ]
    if not all(
        isinstance(value, (int, float))
        and not isinstance(value, bool)
        and math.isfinite(value)
        and value <= 0
        for value in values
    ):
        raise PortableOutputError(
            "generation log probabilities must be finite and non-positive", 66
        )
    return [float(value) for value in values]


def longest_homopolymer(sequence: str) -> int:
    longest = 0
    current = 0
    previous = ""
    for base in sequence:
        current = current + 1 if base == previous else 1
        longest = max(longest, current)
        previous = base
    return longest


def generation_validation_warnings(
    completions: list[str], validation: str
) -> list[list[str]]:
    if validation == "none":
        return [[] for _ in completions]
    duplicate = {
        completion for completion in completions if completions.count(completion) > 1
    }
    result: list[list[str]] = []
    for completion in completions:
        warnings: list[str] = []
        if re.search("[^ACGT]", completion):
            warnings.append("completion contains non-ACGT symbols")
        dna = [base for base in completion if base in "ACGT"]
        if dna:
            gc_fraction = sum(base in "GC" for base in dna) / len(dna)
            if gc_fraction < 0.2 or gc_fraction > 0.8:
                warnings.append("completion has extreme GC fraction")
        if longest_homopolymer(completion) >= 10:
            warnings.append("completion contains a long homopolymer")
        if len(dna) >= 8 and len(set(dna)) <= 2:
            warnings.append("completion has low complexity")
        if completion in duplicate:
            warnings.append("completion is duplicated in this batch")
        result.append(warnings)
    return result


def validate_generation(args: argparse.Namespace) -> None:
    try:
        upstream = load_sequence_map(args.input)
        prompts = load_sequence_map(args.prompts)
    except RuntimeError as error:
        raise PortableOutputError(str(error), 65) from error
    if len(upstream) != len(prompts):
        raise PortableOutputError(
            "generation output has an unexpected number of rows", 65
        )
    prompt_ids = [row.get("id") for row in prompts]
    upstream_ids = [row.get("id") for row in upstream]
    if (
        any(not isinstance(value, str) or not value for value in prompt_ids)
        or len(set(prompt_ids)) != len(prompt_ids)
        or upstream_ids != prompt_ids
    ):
        raise PortableOutputError("generation output IDs do not match the request", 65)
    completions = [row.get("completion") for row in upstream]
    if any(not isinstance(value, str) or not value for value in completions):
        raise PortableOutputError("generation output is missing a completion", 65)
    if args.validate == "strict" and any(
        re.search("[^ACGT]", completion) for completion in completions
    ):
        raise PortableOutputError(
            "strict generation validation rejected non-ACGT output", 67
        )
    warnings = generation_validation_warnings(completions, args.validate)
    rows: list[dict[str, Any]] = []
    for upstream_row, prompt, completion, row_warnings in zip(
        upstream, prompts, completions, warnings, strict=True
    ):
        prompt_value = prompt.get("prompt")
        if not isinstance(prompt_value, str) or not prompt_value:
            raise PortableOutputError(
                "generation prompt must be a non-empty string", 65
            )
        log_probabilities = generation_log_probabilities(upstream_row)
        if args.return_probabilities and log_probabilities is None:
            raise PortableOutputError(
                "generation output is missing requested log probabilities", 65
            )
        usage = upstream_row.get("usage")
        usage = usage if isinstance(usage, dict) else {}
        generated_tokens = upstream_row.get(
            "generated_tokens", usage.get("completion_tokens")
        )
        if generated_tokens is None:
            generated_tokens = (
                len(completion) if log_probabilities is None else len(log_probabilities)
            )
        if (
            not isinstance(generated_tokens, int)
            or isinstance(generated_tokens, bool)
            or generated_tokens < 0
        ):
            raise PortableOutputError(
                "generation output has an invalid generated token count", 65
            )
        finish_reason = upstream_row.get("finish_reason", "unknown")
        if not isinstance(finish_reason, str) or not finish_reason:
            raise PortableOutputError(
                "generation output has an invalid finish reason", 65
            )
        if args.validate != "none" and generated_tokens > args.num_tokens:
            raise PortableOutputError("generated token count exceeds the request", 65)
        if (
            args.validate != "none"
            and finish_reason == "length"
            and generated_tokens != args.num_tokens
        ):
            raise PortableOutputError(
                "length-finished generation has an unexpected token count", 65
            )
        if (
            args.validate != "none"
            and log_probabilities is not None
            and len(log_probabilities) != generated_tokens
        ):
            raise PortableOutputError(
                "generation probability count does not match generated tokens", 65
            )
        prompt_tokens = upstream_row.get("prompt_tokens", usage.get("prompt_tokens"))
        if prompt_tokens is None:
            prompt_tokens = len(prompt_value)
        total_tokens = upstream_row.get("total_tokens", usage.get("total_tokens"))
        if total_tokens is None:
            total_tokens = prompt_tokens + generated_tokens
        dna = [base for base in completion if base in "ACGT"]
        retained_log_probabilities = (
            log_probabilities if args.return_probabilities else None
        )
        rows.append(
            {
                "id": prompt["id"],
                "input_id": prompt.get("input_id", prompt["id"]),
                "sample": int(prompt.get("sample", 1)),
                "prompt": prompt_value,
                "completion": completion,
                "sequence": prompt_value + completion,
                "finish_reason": finish_reason,
                "prompt_tokens": int(prompt_tokens),
                "generated_tokens": generated_tokens,
                "total_tokens": int(total_tokens),
                "log_probabilities": retained_log_probabilities,
                "probabilities": (
                    None
                    if retained_log_probabilities is None
                    else [math.exp(value) for value in retained_log_probabilities]
                ),
                "generated_bases": len(dna),
                "gc_fraction": (
                    sum(base in "GC" for base in dna) / len(dna) if dna else None
                ),
                "ambiguous_fraction": (
                    1 - len(dna) / len(completion) if completion else 0
                ),
                "longest_homopolymer": longest_homopolymer(completion),
                "validation_warnings": row_warnings,
            }
        )
    fasta = "".join(f">{row['id']}\n{row['sequence']}\n" for row in rows)
    write_json_lines(args.output, rows)
    atomic_write_text(args.fasta, fasta)
    atomic_write_json(
        args.validation,
        {
            "validate": args.validate,
            "warnings": {row["id"]: row["validation_warnings"] for row in rows},
        },
    )


def manifest_fragment(path: Path) -> dict[str, Any]:
    path = path.expanduser().resolve(strict=True)
    if path.is_dir():
        raise RuntimeError("manifest fragment path must be a file")
    return {
        "path": str(path),
        "kind": "dense",
        "size": path.stat().st_size,
        "sha256": sha256_file(path),
    }


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(prog="bionemor-evo2-helper")
    subcommands = result.add_subparsers(dest="command", required=True)

    capability = subcommands.add_parser("capabilities")
    capability.add_argument("--json", action="store_true")

    describe = subcommands.add_parser("describe")
    describe.add_argument("--json", action="store_true")

    inspect = subcommands.add_parser("inspect-checkpoint")
    inspect.add_argument("--path", type=Path, required=True)
    inspect.add_argument("--output", type=Path, required=True)

    predictions = subcommands.add_parser("materialize-predictions")
    predictions.add_argument(
        "--mode",
        choices=(
            "score",
            "profile",
            "embedding-pooled",
            "embedding-unpooled",
        ),
        required=True,
    )
    predictions.add_argument("--input", dest="input_dir", type=Path, required=True)
    predictions.add_argument("--sequence-map", type=Path, required=True)
    predictions.add_argument("--output", type=Path, required=True)
    predictions.add_argument("--summary", type=Path)
    predictions.add_argument("--reduction", choices=("mean", "sum"))
    predictions.add_argument("--pool", choices=("mean", "max", "first", "last"))

    generation = subcommands.add_parser("validate-generation")
    generation.add_argument("--input", type=Path, required=True)
    generation.add_argument("--prompts", type=Path, required=True)
    generation.add_argument("--output", type=Path, required=True)
    generation.add_argument("--fasta", type=Path, required=True)
    generation.add_argument("--validation", type=Path, required=True)
    generation.add_argument("--num-tokens", type=int, required=True)
    generation.add_argument(
        "--validate", choices=("basic", "strict", "none"), required=True
    )
    generation.add_argument("--return-probabilities", action="store_true")

    fragment = subcommands.add_parser("write-manifest-fragment")
    fragment.add_argument("--path", type=Path, required=True)
    fragment.add_argument("--output", type=Path, required=True)
    return result


def main() -> None:
    args = parser().parse_args()
    if args.command == "capabilities":
        print(json.dumps(capabilities(), sort_keys=True))
    elif args.command == "describe":
        print(json.dumps(description(), sort_keys=True))
    elif args.command == "inspect-checkpoint":
        atomic_write_json(args.output, checkpoint_inspection(args.path))
    elif args.command == "materialize-predictions":
        materialize_predictions(args)
    elif args.command == "validate-generation":
        validate_generation(args)
    elif args.command == "write-manifest-fragment":
        atomic_write_json(args.output, manifest_fragment(args.path))
    else:
        raise AssertionError(f"unhandled command: {args.command}")


if __name__ == "__main__":
    try:
        main()
    except PortableOutputError as error:
        print(error, file=sys.stderr)
        raise SystemExit(error.exit_status) from None
    except RuntimeError as error:
        print(error, file=sys.stderr)
        raise SystemExit(1) from None
