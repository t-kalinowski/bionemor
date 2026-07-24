#!/usr/bin/env bash
set -euo pipefail

if ! command -v docker >/dev/null 2>&1; then
  echo "docker is not installed" >&2
  exit 1
fi

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_dir="$(cd -- "$script_dir/../.." && pwd)"
workspace="${BIONEMOR_EVO2_WORKSPACE:-/home/ubuntu/bionemor-workspace}"
base_image="nvcr.io/nvidia/clara/bionemo-framework:2.6.3"
image="bionemor-bionemo:2.6.3"

if [[ "$workspace" != /* ]]; then
  echo "BIONEMOR_EVO2_WORKSPACE must be an absolute path" >&2
  exit 2
fi

mkdir -p "$workspace"
docker pull "$base_image"
docker run --rm --gpus all "$base_image" nvidia-smi
docker build \
  -f "$repo_dir/inst/docker/bionemo-framework/Dockerfile" \
  -t "$image" \
  "$repo_dir"

docker run --rm \
  --gpus all \
  --ipc=host \
  -e BIONEMOR_EVO2_WORKSPACE=/workspace \
  -v "$workspace:/workspace" \
  -w /workspace \
  "$image" \
  Rscript /opt/bionemor/inst/scripts/brev-evo2-framework-smoke.R
