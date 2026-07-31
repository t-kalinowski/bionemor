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
capture_date="${BIONEMOR_EVO2_CAPTURE_DATE:-$(date -u +%Y-%m-%d)}"
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
if [[ ! "$capture_date" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]]; then
  echo "BIONEMOR_EVO2_CAPTURE_DATE must use YYYY-MM-DD" >&2
  exit 2
fi
checkpoint_source="$(cd -- "$checkpoint_source" && pwd -P)"

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_dir="$(cd -- "$script_dir/../.." && pwd)"
package_revision="$(git -C "$repo_dir" rev-parse HEAD)"
if [[ ! "$package_revision" =~ ^[0-9a-f]{40}$ ]]; then
  echo "package source revision must be a full commit SHA" >&2
  exit 2
fi
package_dirty=false
if [[ -n "$(git -C "$repo_dir" status --porcelain)" ]]; then
  package_dirty=true
fi
validation_root="$repo_dir/validation/brev-evo2"
capture_dir="$validation_root/$capture_date"
remote_evidence="/home/ubuntu/bionemor-workspace/validation/brev-evo2/$capture_date"
if [[ -e "$capture_dir" ]]; then
  echo "validation capture already exists: $capture_dir" >&2
  exit 2
fi
provisioning_attempted=false
capture_tmp=""

cleanup() {
  status=$?
  set +e
  if [[ -n "$capture_tmp" && -d "$capture_tmp" ]]; then
    rm -rf "$capture_tmp"
  fi
  if [[ "$provisioning_attempted" == true ]]; then
    if ! brev stop "$instance"; then
      echo "failed to stop Brev instance: $instance" >&2
      if [[ "$status" -eq 0 ]]; then
        status=1
      fi
    fi
  fi
  trap - EXIT
  exit "$status"
}
trap cleanup EXIT

provisioning_attempted=true
"$script_dir/brev-evo2-create.sh" --create "$instance"
brev exec "$instance" "mkdir -p '$checkpoint'"
brev copy "$checkpoint_source/" "$instance:$checkpoint/"
brev exec "$instance" \
  "test -d '$checkpoint' || { echo 'BIONEMOR_EVO2_CHECKPOINT is not available on the instance' >&2; exit 2; }"
brev copy "$repo_dir/" "$instance:/home/ubuntu/bionemor/"
brev exec "$instance" \
  "BIONEMOR_EVO2_CAPTURE_DATE='$capture_date' BIONEMOR_EVO2_CHECKPOINT='$checkpoint' BIONEMOR_EVO2_EVIDENCE='$remote_evidence' BIONEMOR_PACKAGE_DIRTY='$package_dirty' BIONEMOR_PACKAGE_REVISION='$package_revision' bash /home/ubuntu/bionemor/inst/scripts/brev-evo2-recipes.sh --acceptance"

mkdir -p "$validation_root"
capture_tmp="$(mktemp -d "$validation_root/.capture-${capture_date}.XXXXXX")"
brev copy "$instance:$remote_evidence/" "$capture_tmp/"
required_capture_files=(
  README.md
  evidence.json
  outputs/dense.json
  outputs/fitted.json
  manifests/dense-score.json
  manifests/dense-generation.json
  manifests/dense-embedding.json
  manifests/prepare.json
  manifests/fine-tune.json
  manifests/fitted-score.json
  manifests/fitted-generation.json
  lora-inspection.json
)
for relative_path in "${required_capture_files[@]}"; do
  if [[ ! -f "$capture_tmp/$relative_path" ]]; then
    echo "Brev evidence capture is missing $relative_path" >&2
    exit 1
  fi
done
mv "$capture_tmp" "$capture_dir"
capture_tmp=""
echo "Brev acceptance evidence copied to $capture_dir"
