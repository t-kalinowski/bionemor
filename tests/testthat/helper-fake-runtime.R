write_executable <- function(path, lines) {
  writeLines(c("#!/usr/bin/env bash", "set -euo pipefail", lines), path)
  Sys.chmod(path, "0755")
  path
}

fake_slurm_runtime <- function(bin) {
  dir.create(bin, recursive = TRUE)
  write_executable(
    file.path(bin, "sbatch"),
    c(
      "if [[ \"${1:-}\" == \"--parsable\" ]]; then",
      "  printf '123\\n'",
      "else",
      "  printf '123\\n'",
      "fi"
    )
  )
  write_executable(
    file.path(bin, "sacct"),
    "printf '123|%s|%s\\n' \"${BIONEMOR_FAKE_STATE:-COMPLETED}\" \"${BIONEMOR_FAKE_EXIT:-0:0}\""
  )
  write_executable(
    file.path(bin, "scancel"),
    "printf '%s\\n' \"$@\" > \"${BIONEMOR_CANCEL_ARGS:-/dev/null}\""
  )
  invisible(bin)
}

fake_bionemo_runtime <- function(bin) {
  dir.create(bin, recursive = TRUE)
  write_executable(
    file.path(bin, "preprocess_evo2"),
    c(
      "if [[ \"${1:-}\" == \"--help\" ]]; then printf 'preprocess help\\n'; exit 0; fi",
      "printf 'preprocessed\\n'"
    )
  )
  write_executable(
    file.path(bin, "train_evo2"),
    c(
      "if [[ \"${1:-}\" == \"--help\" ]]; then printf 'train help\\n'; exit 0; fi",
      "result=",
      "name=",
      "while [[ $# -gt 0 ]]; do",
      "  case \"$1\" in",
      "    --result-dir) shift; result=\"$1\" ;;",
      "    --experiment-name) shift; name=\"$1\" ;;",
      "  esac",
      "  shift",
      "done",
      "sleep \"${BIONEMOR_TRAIN_SLEEP:-0}\"",
      "checkpoint=\"$result/$name/checkpoints/step-last\"",
      "mkdir -p \"$checkpoint/context\" \"$checkpoint/weights\"",
      "printf 'model\\n' > \"$checkpoint/context/model.yaml\"",
      "printf '{}\\n' > \"$checkpoint/weights/metadata.json\"",
      "printf 'trained\\n'"
    )
  )
  write_executable(
    file.path(bin, "predict_evo2"),
    c(
      "if [[ \"${1:-}\" == \"--help\" ]]; then printf 'predict help\\n'; exit 0; fi",
      "output=",
      "while [[ $# -gt 0 ]]; do",
      "  if [[ \"$1\" == \"--output-dir\" ]]; then",
      "    shift",
      "    output=\"$1\"",
      "  fi",
      "  shift",
      "done",
      "mkdir -p \"$output\"",
      "printf 'tensor\\n' > \"$output/predictions__0_0.pt\"",
      "printf 'predicted\\n'"
    )
  )
  write_executable(
    file.path(bin, "infer_evo2"),
    c(
      "if [[ \"${1:-}\" == \"--help\" ]]; then printf 'infer help\\n'; exit 0; fi",
      "prompt=",
      "output=",
      "while [[ $# -gt 0 ]]; do",
      "  case \"$1\" in",
      "    --prompt) shift; prompt=\"$1\" ;;",
      "    --output-file) shift; output=\"$1\" ;;",
      "  esac",
      "  shift",
      "done",
      "sleep \"${BIONEMOR_INFER_SLEEP:-0}\"",
      "if [[ \"${BIONEMOR_ECHO_NGC:-false}\" == \"true\" ]]; then",
      "  printf 'NGC_API_KEY=%s\\n' \"${NGC_API_KEY-unset}\"",
      "  printf 'NGC_CLI_API_KEY=%s\\n' \"${NGC_CLI_API_KEY-unset}\"",
      "fi",
      "mkdir -p \"$(dirname \"$output\")\"",
      "printf 'generated-%s\\n' \"$prompt\" > \"$output\"",
      "printf 'generated\\n'"
    )
  )
  write_executable(
    file.path(bin, "python"),
    c(
      "if [[ \"${1:-}\" == \"--version\" ]]; then",
      "  printf 'Python 3.12.0\\n'",
      "  exit 0",
      "fi",
      "output=\"${@: -1}\"",
      "printf '{\"sequence_indices\":[0,1],\"scores\":[-1.25,-2.5]}\\n' > \"$output\""
    )
  )
  write_executable(
    file.path(bin, "nvidia-smi"),
    "printf 'Fake GPU, 49152 MiB, 555.1\\n'"
  )
  invisible(bin)
}

make_checkpoint_dir <- function(workspace, name = "checkpoint") {
  path <- file.path(workspace, name)
  dir.create(file.path(path, "context"), recursive = TRUE)
  dir.create(file.path(path, "weights"))
  writeLines("model", file.path(path, "context", "model.yaml"))
  writeLines("{}", file.path(path, "weights", "metadata.json"))
  path
}
