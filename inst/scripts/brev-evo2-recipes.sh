#!/usr/bin/env bash
set -euo pipefail

for command in awk docker git R Rscript; do
  if ! command -v "$command" >/dev/null 2>&1; then
    echo "$command is not installed" >&2
    exit 1
  fi
done

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_dir="$(cd -- "$script_dir/../.." && pwd)"
lock_file="$repo_dir/inst/recipes/evo2.json"
appendage="$repo_dir/inst/docker/evo2-recipes/Dockerfile.append"
helper="$repo_dir/inst/scripts/materialize-evo2.py"
workspace="${BIONEMOR_EVO2_WORKSPACE:-/home/ubuntu/bionemor-workspace}"
checkpoint="${BIONEMOR_EVO2_CHECKPOINT:-}"
source_root="$(mktemp -d)"

cleanup() {
  rm -rf "$source_root"
}
trap cleanup EXIT

if [[ "$workspace" != /* ]]; then
  echo "BIONEMOR_EVO2_WORKSPACE must be an absolute path" >&2
  exit 2
fi
if [[ -z "$checkpoint" ]]; then
  echo "BIONEMOR_EVO2_CHECKPOINT is required" >&2
  exit 2
fi
if [[ "$checkpoint" != /* || ! -d "$checkpoint" ]]; then
  echo "BIONEMOR_EVO2_CHECKPOINT must be an existing absolute directory" >&2
  exit 2
fi

mkdir -p "$workspace"
workspace="$(cd -- "$workspace" && pwd -P)"
checkpoint="$(cd -- "$checkpoint" && pwd -P)"
case "$checkpoint" in
  "$workspace" | "$workspace"/*) ;;
  *)
    echo "BIONEMOR_EVO2_CHECKPOINT must be inside BIONEMOR_EVO2_WORKSPACE" >&2
    exit 2
    ;;
esac

mapfile -t lock < <(
  Rscript - "$lock_file" <<'RSCRIPT'
lock_file <- commandArgs(trailingOnly = TRUE)[[1L]]
lock <- jsonlite::read_json(lock_file, simplifyVector = TRUE)
writeLines(c(
  lock$repository,
  lock$revision,
  lock$recipe_version,
  lock$subdirectory,
  lock$dockerfile_blob,
  lock$base_image,
  lock$base_image_digest,
  lock$uv_image,
  lock$uv_image_digest,
  lock$bridge_protocol
))
RSCRIPT
)
if [[ "${#lock[@]}" -ne 10 ]]; then
  echo "the Evo 2 recipe lock is incomplete" >&2
  exit 1
fi

repository="${lock[0]}"
revision="${lock[1]}"
recipe_version="${lock[2]}"
subdirectory="${lock[3]}"
dockerfile_blob="${lock[4]}"
base_image="${lock[5]}"
base_image_digest="${lock[6]}"
uv_image="${lock[7]}"
uv_image_digest="${lock[8]}"
bridge_protocol="${lock[9]}"
image="${BIONEMOR_EVO2_IMAGE:-bionemor/evo2:${revision:0:12}}"

if [[ ! "$revision" =~ ^[0-9a-f]{40}$ ]]; then
  echo "the recipe lock revision is not a full commit SHA" >&2
  exit 1
fi
if [[ ! "$dockerfile_blob" =~ ^[0-9a-f]{40}$ ]]; then
  echo "the recipe lock Dockerfile blob is invalid" >&2
  exit 1
fi
if [[ ! "$base_image_digest" =~ ^sha256:[0-9a-f]{64}$ ]]; then
  echo "the recipe lock base-image digest is invalid" >&2
  exit 1
fi
if [[ ! "$uv_image_digest" =~ ^sha256:[0-9a-f]{64}$ ]]; then
  echo "the recipe lock uv-image digest is invalid" >&2
  exit 1
fi
base_image_reference="${base_image}@${base_image_digest}"
uv_image_reference="${uv_image}@${uv_image_digest}"

source_dir="$source_root/bionemo-recipes"
mkdir "$source_dir"
git -C "$source_dir" init
git -C "$source_dir" fetch --depth 1 "$repository" "$revision"
git -C "$source_dir" checkout --detach FETCH_HEAD

resolved_revision="$(git -C "$source_dir" rev-parse HEAD)"
if [[ "$resolved_revision" != "$revision" ]]; then
  echo "BioNeMo Recipes checkout does not match the package lock" >&2
  exit 1
fi

official_dockerfile="$source_dir/$subdirectory/Dockerfile"
if [[ ! -f "$official_dockerfile" ]]; then
  echo "the locked BioNeMo Recipes Dockerfile is missing" >&2
  exit 1
fi
resolved_blob="$(git -C "$source_dir" hash-object "$official_dockerfile")"
if [[ "$resolved_blob" != "$dockerfile_blob" ]]; then
  echo "BioNeMo Recipes Dockerfile does not match the package lock" >&2
  exit 1
fi

docker pull "$base_image_reference"
repo_digests="$(
  docker image inspect \
    --format '{{range .RepoDigests}}{{println .}}{{end}}' \
    "$base_image_reference"
)"
digest_matches=false
while IFS= read -r reference; do
  if [[ "${reference##*@}" == "$base_image_digest" ]]; then
    digest_matches=true
  fi
done <<< "$repo_digests"
if [[ "$digest_matches" != true ]]; then
  echo "NGC PyTorch base image does not match the package lock" >&2
  exit 1
fi

context="$source_root/build-context"
mkdir "$context"
cp -a "$source_dir/$subdirectory/." "$context"
mkdir "$context/bionemor-helper"
cp "$helper" "$context/bionemor-helper/materialize-evo2.py"
expected="FROM $base_image"
replacement="FROM $base_image_reference"
from_count="$(
  awk -v expected="$expected" \
    '$0 == expected { count++ } END { print count + 0 }' \
    "$context/Dockerfile"
)"
if [[ "$from_count" -ne 1 ]]; then
  echo "the locked recipe Dockerfile has an unexpected FROM instruction" >&2
  exit 1
fi
awk -v expected="$expected" -v replacement="$replacement" \
  '$0 == expected { print replacement; next } { print }' \
  "$context/Dockerfile" > "$context/Dockerfile.tmp"
mv "$context/Dockerfile.tmp" "$context/Dockerfile"
uv_fallback="#COPY --from=ghcr.io/astral-sh/uv:latest /uv /uvx /bin/"
uv_replacement="COPY --from=$uv_image_reference /uv /uvx /bin/"
uv_count="$(
  awk -v expected="$uv_fallback" \
    '$0 == expected { count++ } END { print count + 0 }' \
    "$context/Dockerfile"
)"
if [[ "$uv_count" -ne 1 ]]; then
  echo "the locked recipe Dockerfile has an unexpected uv fallback" >&2
  exit 1
fi
awk -v expected="$uv_fallback" -v replacement="$uv_replacement" \
  '$0 == expected { print replacement; next } { print }' \
  "$context/Dockerfile" > "$context/Dockerfile.tmp"
mv "$context/Dockerfile.tmp" "$context/Dockerfile"
cat "$appendage" >> "$context/Dockerfile"

helper_revision="$(git hash-object "$helper")"
docker build \
  --file "$context/Dockerfile" \
  --tag "$image" \
  --build-arg "BIONEMOR_RECIPE_REVISION=$revision" \
  --build-arg "BIONEMOR_HELPER_REVISION=$helper_revision" \
  --build-arg "BIONEMOR_BASE_IMAGE=$base_image" \
  --build-arg "BIONEMOR_BASE_IMAGE_DIGEST=$base_image_digest" \
  "$context"

image_id="$(docker image inspect --format '{{.Id}}' "$image")"
if [[ ! "$image_id" =~ ^sha256:[0-9a-f]{64}$ ]]; then
  echo "the derived recipe image has an invalid immutable ID" >&2
  exit 1
fi

verify_image_label() {
  local label="$1"
  local expected="$2"
  local actual
  actual="$(
    docker image inspect \
      --format "{{ index .Config.Labels \"$label\" }}" \
      "$image_id"
  )"
  if [[ "$actual" != "$expected" ]]; then
    echo "image label $label does not match the package lock" >&2
    exit 1
  fi
}

verify_image_label "org.opencontainers.image.source" "$repository"
verify_image_label "org.opencontainers.image.revision" "$revision"
verify_image_label \
  "org.opencontainers.image.version" \
  "evo2-recipe-$recipe_version"
verify_image_label "io.bionemor.helper.revision" "$helper_revision"
verify_image_label "io.bionemor.base.image" "$base_image"
verify_image_label "io.bionemor.base.digest" "$base_image_digest"
verify_image_label "io.bionemor.bridge.protocol" "$bridge_protocol"

docker run \
  --rm \
  --gpus all \
  "$image_id" \
  bionemor-evo2-helper capabilities --json

R CMD INSTALL "$repo_dir"
BIONEMOR_EVO2_CHECKPOINT="$checkpoint" \
  BIONEMOR_EVO2_IMAGE="$image_id" \
  BIONEMOR_EVO2_WORKSPACE="$workspace" \
  Rscript "$repo_dir/inst/scripts/brev-evo2-recipes-smoke.R"
