#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "Usage: $0 --run [instance-name]" >&2
}

if [[ "${1:-}" != "--run" ]]; then
  usage
  exit 2
fi
shift
if [[ $# -gt 1 ]]; then
  usage
  exit 2
fi

instance="${1:-bionemor-evo2}"
checkpoint_source="${BIONEMOR_EVO2_CHECKPOINT_SOURCE:-}"
checkpoint="${BIONEMOR_EVO2_CHECKPOINT:-/home/ubuntu/bionemor-workspace/checkpoints/evo2-7b}"
if [[ -z "$checkpoint_source" ]]; then
  echo "BIONEMOR_EVO2_CHECKPOINT_SOURCE is required" >&2
  exit 2
fi
if [[ ! -d "$checkpoint_source" ]]; then
  echo "BIONEMOR_EVO2_CHECKPOINT_SOURCE must be an existing directory" >&2
  exit 2
fi
if [[ ! "$checkpoint" =~ ^/[A-Za-z0-9._/-]+$ ]]; then
  echo "BIONEMOR_EVO2_CHECKPOINT must be a safe absolute path on the instance" >&2
  exit 2
fi
if [[ "$checkpoint" != /home/ubuntu/bionemor-workspace/* ]]; then
  echo "BIONEMOR_EVO2_CHECKPOINT must be inside /home/ubuntu/bionemor-workspace" >&2
  exit 2
fi
checkpoint_source="$(cd -- "$checkpoint_source" && pwd -P)"

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_dir="$(cd -- "$script_dir/../.." && pwd)"
provisioning_attempted=false

stop_instance() {
  if [[ "$provisioning_attempted" == true ]]; then
    brev stop "$instance"
  fi
}
trap stop_instance EXIT

provisioning_attempted=true
"$script_dir/brev-evo2-create.sh" --create "$instance"
brev exec "$instance" "mkdir -p '$checkpoint'"
brev copy "$checkpoint_source/" "$instance:$checkpoint/"
brev exec "$instance" \
  "test -d '$checkpoint' || { echo 'BIONEMOR_EVO2_CHECKPOINT is not available on the instance' >&2; exit 2; }"
brev copy "$repo_dir/" "$instance:/home/ubuntu/bionemor/"
brev exec "$instance" \
  "BIONEMOR_EVO2_CHECKPOINT='$checkpoint' bash /home/ubuntu/bionemor/inst/scripts/brev-evo2-recipes.sh"
