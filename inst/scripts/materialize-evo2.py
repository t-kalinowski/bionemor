#!/usr/bin/env python3

import json
import sys
from pathlib import Path

import torch


def prediction_tensor(directory: Path) -> Path:
    tensors = list(directory.rglob("*.pt"))
    if len(tensors) != 1:
        raise RuntimeError(
            f"expected one prediction tensor in {directory}, found {len(tensors)}"
        )
    return tensors[0]


def materialize_score(directory: Path, destination: Path) -> None:
    result = torch.load(
        prediction_tensor(directory),
        map_location="cpu",
        weights_only=True,
    )
    payload = {
        "sequence_indices": result["seq_idx"].tolist(),
        "scores": result["log_probs_seqs"].tolist(),
    }
    destination.write_text(json.dumps(payload) + "\n", encoding="utf-8")


def main() -> None:
    if len(sys.argv) != 4 or sys.argv[1] != "score":
        raise SystemExit("usage: materialize-evo2.py score INPUT_DIR OUTPUT_JSON")
    materialize_score(Path(sys.argv[2]), Path(sys.argv[3]))


if __name__ == "__main__":
    main()
