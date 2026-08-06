#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "Usage: $0 [--create|--dry-run] [instance-name]" >&2
}

create=false
case "${1:-}" in
  --create)
    create=true
    shift
    ;;
  --dry-run)
    shift
    ;;
esac

if [[ $# -gt 1 ]]; then
  usage
  exit 2
fi
if ! command -v brev >/dev/null 2>&1; then
  echo "brev is not installed; see https://docs.nvidia.com/brev/cli/getting-started" >&2
  exit 1
fi

instance_name="${1:-bionemor-evo2}"
instance_type="${BREV_EVO2_INSTANCE_TYPE:-l40s-48gb.1x}"
script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_dir="$(cd -- "$script_dir/../../.." && pwd)"
setup_script="$repo_dir/tools/brev/setup.sh"
if [[ ! "$instance_name" =~ ^[A-Za-z0-9][A-Za-z0-9._-]*$ ]]; then
  echo "instance name must contain only letters, digits, periods, underscores, and hyphens" >&2
  exit 2
fi

args=(
  create
  "$instance_name"
  --mode vm
  --type "$instance_type"
  --timeout 900
  --jupyter=false
)
if [[ "$create" == false ]]; then
  args+=(--dry-run)
fi

brev "${args[@]}"
if [[ "$create" == true ]]; then
  brev exec "$instance_name" "@$setup_script"
fi
