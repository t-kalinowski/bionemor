#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_dir="$(cd -- "$script_dir/../.." && pwd)"
workspace="${BIONEMOR_EVO2_WORKSPACE:-/home/ubuntu/bionemor-workspace}"
run_id="${BIONEMOR_EVO2_RUN_ID:-$(date -u +%Y%m%dT%H%M%SZ)}"
base_image="nvcr.io/nvidia/clara/bionemo-framework:2.6.3"
image="bionemor-bionemo:2.6.3"
run_dir="$workspace/runs/$run_id"
training_container="brev-evo2-finetune-train-$run_id"

if [[ "$workspace" != /* ]]; then
  echo "BIONEMOR_EVO2_WORKSPACE must be an absolute path" >&2
  exit 2
fi
if [[
  ! "$run_id" =~ ^[A-Za-z0-9_.-]+$ ||
  "$run_id" == "." ||
  "$run_id" == ".."
]]; then
  echo "BIONEMOR_EVO2_RUN_ID must be a safe name" >&2
  exit 2
fi
if ! command -v docker >/dev/null 2>&1; then
  echo "docker is not installed" >&2
  exit 1
fi
if ! command -v timeout >/dev/null 2>&1; then
  echo "timeout is not installed" >&2
  exit 1
fi

cleanup_training_container() {
  if docker container inspect "$training_container" >/dev/null 2>&1; then
    docker rm --force "$training_container" >/dev/null
  fi
}
trap cleanup_training_container EXIT INT TERM

mkdir -p "$workspace" "$run_dir/logs"
docker pull "$base_image"
docker run --rm --gpus all "$base_image" nvidia-smi
docker build \
  -f "$repo_dir/inst/docker/bionemo-framework/Dockerfile" \
  -t "$image" \
  "$repo_dir"

run_phase() {
  local phase="$1"
  docker run --rm \
    --gpus all \
    --ipc=host \
    -e BIONEMOR_EVO2_WORKSPACE=/workspace \
    -e BIONEMOR_EVO2_RUN_ID="$run_id" \
    -v "$workspace:/workspace" \
    -w /workspace \
    "$image" \
    Rscript /opt/bionemor/inst/scripts/brev-evo2-finetune.R "$phase"
}

run_phase prepare
run_phase baseline

training_started="$(date +%s)"
set +e
timeout --signal=TERM --kill-after=15s 300s \
  docker run --rm \
    --name "$training_container" \
    --gpus all \
    --ipc=host \
    -e BIONEMOR_EVO2_WORKSPACE=/workspace \
    -e BIONEMOR_EVO2_RUN_ID="$run_id" \
    -v "$workspace:/workspace" \
    -w /workspace \
    "$image" \
    Rscript /opt/bionemor/inst/scripts/brev-evo2-finetune.R fit \
  2>&1 | tee "$run_dir/logs/fit.log"
training_status="${PIPESTATUS[0]}"
set -e
training_seconds="$(("$(date +%s)" - training_started))"
printf '%s\n' "$training_seconds" > "$run_dir/training-seconds.txt"

if [[ "$training_status" -ne 0 ]]; then
  echo "training failed with status $training_status after $training_seconds seconds" >&2
  exit "$training_status"
fi
if [[ "$training_seconds" -gt 300 ]]; then
  echo "training exceeded the 300-second limit" >&2
  exit 1
fi

run_phase verify
printf 'Evo 2 fitting workflow passed. Artifacts: %s\n' "$run_dir"
