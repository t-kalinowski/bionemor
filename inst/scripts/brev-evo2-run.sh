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
brev copy "$repo_dir/" "$instance:/home/ubuntu/bionemor/"
brev exec "$instance" \
  "bash /home/ubuntu/bionemor/inst/scripts/brev-evo2-framework.sh"
brev exec "$instance" \
  "bash /home/ubuntu/bionemor/inst/scripts/brev-evo2-finetune.sh"
