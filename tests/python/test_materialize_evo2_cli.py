from __future__ import annotations

import json
import math
import os
import pickle
import shlex
import subprocess
import sys
import tempfile
import types
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
HELPER = ROOT / "inst" / "scripts" / "materialize-evo2.py"
GENERATION_PROMPT = {
    "id": "prompt-1",
    "input_id": "sequence-1",
    "sample": 1,
    "prompt": "ACGT",
}


def generation_record(
    completion: str,
    finish_reason: str,
    log_probabilities: list[float] | None,
) -> dict[str, object]:
    row: dict[str, object] = {
        "id": "prompt-1",
        "prompt": "ACGT",
        "completion": completion,
        "finish_reason": finish_reason,
        "usage": {
            "prompt_tokens": 4,
            "completion_tokens": len(completion),
            "total_tokens": 4 + len(completion),
        },
    }
    if log_probabilities is not None:
        row["logprobs"] = {"completion_logprobs": log_probabilities}
    return row


class MaliciousMetadataValue:
    def __init__(self, sentinel: Path) -> None:
        self.sentinel = sentinel

    def __reduce__(self) -> tuple[object, tuple[str]]:
        return os.system, (f"touch {shlex.quote(str(self.sentinel))}",)


class CheckpointInspectionTest(unittest.TestCase):
    def test_reports_transformer_engine_key_layout(self) -> None:
        layouts = {
            "te": (
                "decoder.layers.0.mlp.linear_fc1.layer_norm_weight",
                True,
            ),
            "non_te": ("decoder.layers.0.pre_mlp_layernorm.weight", False),
        }
        for name, (key, expected) in layouts.items():
            with self.subTest(name=name), tempfile.TemporaryDirectory() as temporary:
                root = Path(temporary)
                checkpoint = root / "checkpoint"
                checkpoint.mkdir()
                with (checkpoint / ".metadata").open("wb") as stream:
                    pickle.dump(
                        types.SimpleNamespace(state_dict_metadata={key: None}),
                        stream,
                    )
                (checkpoint / "__0_0.distcp").write_bytes(b"weights")
                (checkpoint / "run_config.yaml").write_text(
                    "model_size: evo2_7b\nkind: dense\n",
                    encoding="utf-8",
                )
                output = root / "inspection.json"
                stubs = root / "stubs"
                stubs.mkdir()
                (stubs / "torch.py").write_text("", encoding="utf-8")
                environment = os.environ.copy()
                environment["PYTHONPATH"] = str(stubs)

                subprocess.run(
                    (
                        sys.executable,
                        str(HELPER),
                        "inspect-checkpoint",
                        "--path",
                        str(checkpoint),
                        "--output",
                        str(output),
                    ),
                    check=True,
                    env=environment,
                )

                inspection = json.loads(output.read_text(encoding="utf-8"))
                self.assertIs(inspection["transformer_engine"], expected)

    def test_metadata_inspection_does_not_execute_pickle_payloads(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            checkpoint = root / "checkpoint"
            checkpoint.mkdir()
            sentinel = root / "payload-executed"
            key = "decoder.layers.0.mlp.linear_fc1.layer_norm_weight"
            with (checkpoint / ".metadata").open("wb") as stream:
                pickle.dump(
                    types.SimpleNamespace(
                        state_dict_metadata={
                            key: MaliciousMetadataValue(sentinel),
                        }
                    ),
                    stream,
                )
            (checkpoint / "__0_0.distcp").write_bytes(b"weights")
            (checkpoint / "run_config.yaml").write_text(
                "model_size: evo2_7b\nkind: dense\n",
                encoding="utf-8",
            )
            output = root / "inspection.json"
            stubs = root / "stubs"
            module = stubs / "torch" / "distributed" / "checkpoint"
            module.mkdir(parents=True)
            (stubs / "torch" / "__init__.py").write_text("", encoding="utf-8")
            (stubs / "torch" / "distributed" / "__init__.py").write_text(
                "", encoding="utf-8"
            )
            (module / "__init__.py").write_text(
                "\n".join(
                    (
                        "import pickle",
                        "from pathlib import Path",
                        "",
                        "class FileSystemReader:",
                        "    def __init__(self, path):",
                        "        self.path = Path(path)",
                        "",
                        "    def read_metadata(self):",
                        "        with (self.path / '.metadata').open('rb') as stream:",
                        "            return pickle.load(stream)",
                        "",
                    )
                ),
                encoding="utf-8",
            )
            environment = os.environ.copy()
            environment["PYTHONPATH"] = str(stubs)

            subprocess.run(
                (
                    sys.executable,
                    str(HELPER),
                    "inspect-checkpoint",
                    "--path",
                    str(checkpoint),
                    "--output",
                    str(output),
                ),
                check=True,
                env=environment,
            )

            inspection = json.loads(output.read_text(encoding="utf-8"))
            self.assertIs(inspection["transformer_engine"], True)
            self.assertFalse(sentinel.exists())

    def test_metadata_only_checkpoint_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            checkpoint = root / "checkpoint"
            checkpoint.mkdir()
            (checkpoint / ".metadata").write_bytes(b"metadata")
            (checkpoint / "run_config.yaml").write_text(
                "model_size: evo2_7b\nkind: dense\n",
                encoding="utf-8",
            )
            output = root / "inspection.json"
            stubs = root / "stubs"
            stubs.mkdir()
            (stubs / "torch.py").write_text("", encoding="utf-8")
            environment = os.environ.copy()
            environment["PYTHONPATH"] = str(stubs)

            result = subprocess.run(
                (
                    sys.executable,
                    str(HELPER),
                    "inspect-checkpoint",
                    "--path",
                    str(checkpoint),
                    "--output",
                    str(output),
                ),
                check=False,
                capture_output=True,
                text=True,
                env=environment,
            )

            self.assertNotEqual(result.returncode, 0)
            self.assertIn("distributed checkpoint weight shard", result.stderr)


class GenerationMaterializationTest(unittest.TestCase):
    def run_helper(
        self,
        root: Path,
        prompt: dict[str, object],
        generated: dict[str, object],
    ) -> subprocess.CompletedProcess[str]:
        prompts = root / "prompts.jsonl"
        upstream = root / "upstream.jsonl"
        prompts.write_text(json.dumps(prompt) + "\n", encoding="utf-8")
        upstream.write_text(json.dumps(generated) + "\n", encoding="utf-8")
        stubs = root / "stubs"
        stubs.mkdir()
        (stubs / "torch.py").write_text("", encoding="utf-8")
        environment = os.environ.copy()
        environment["PYTHONPATH"] = str(stubs)
        return subprocess.run(
            (
                sys.executable,
                str(HELPER),
                "validate-generation",
                "--input",
                str(upstream),
                "--prompts",
                str(prompts),
                "--output",
                str(root / "generation.jsonl"),
                "--fasta",
                str(root / "generated.fasta"),
                "--validation",
                str(root / "validation.json"),
                "--num-tokens",
                "4",
                "--validate",
                "basic",
                "--return-probabilities",
            ),
            check=False,
            capture_output=True,
            text=True,
            env=environment,
        )

    def test_writes_complete_portable_generation_outputs(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            generated = generation_record("TGCA", "length", [math.log(0.25)] * 4)

            result = self.run_helper(root, GENERATION_PROMPT, generated)

            self.assertEqual(result.returncode, 0, result.stderr)
            row = json.loads((root / "generation.jsonl").read_text(encoding="utf-8"))
            self.assertEqual(row["sequence"], "ACGTTGCA")
            self.assertEqual(row["probabilities"], [0.25] * 4)
            self.assertEqual(
                (root / "generated.fasta").read_text(encoding="utf-8"),
                ">prompt-1\nACGTTGCA\n",
            )
            validation = json.loads(
                (root / "validation.json").read_text(encoding="utf-8")
            )
            self.assertEqual(
                validation, {"validate": "basic", "warnings": {"prompt-1": []}}
            )

    def test_rejects_positive_log_probabilities_before_writing_outputs(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            generated = generation_record("T", "stop", [0.1])

            result = self.run_helper(root, GENERATION_PROMPT, generated)

            self.assertNotEqual(result.returncode, 0)
            self.assertIn("finite and non-positive", result.stderr)
            self.assertFalse((root / "generation.jsonl").exists())
            self.assertFalse((root / "generated.fasta").exists())
            self.assertFalse((root / "validation.json").exists())

    def test_requires_requested_log_probabilities(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            generated = generation_record("TGCA", "length", None)

            result = self.run_helper(root, GENERATION_PROMPT, generated)

            self.assertEqual(result.returncode, 65)
            self.assertIn("requested log probabilities", result.stderr)
            self.assertFalse((root / "generation.jsonl").exists())
            self.assertFalse((root / "generated.fasta").exists())
            self.assertFalse((root / "validation.json").exists())

    def test_rejects_unpinned_upstream_fields(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            generated = generation_record("TGCA", "length", None)
            generated["log_probabilities"] = [math.log(0.25)] * 4

            result = self.run_helper(root, GENERATION_PROMPT, generated)

            self.assertEqual(result.returncode, 65)
            self.assertIn("fields", result.stderr)


if __name__ == "__main__":
    unittest.main()
