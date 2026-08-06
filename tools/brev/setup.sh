#!/usr/bin/env bash
set -euo pipefail

if [[ "${EUID:-$(id -u)}" -eq 0 ]]; then
  echo "run this setup as the regular Brev user, not root" >&2
  exit 1
fi
if ! command -v sudo >/dev/null 2>&1; then
  echo "sudo is required to install rig and R" >&2
  exit 1
fi
sudo -n true

package_spec="${BIONEMOR_PACKAGE_SPEC:-t-kalinowski/bionemor}"
if [[ -z "$package_spec" ]]; then
  echo "BIONEMOR_PACKAGE_SPEC must not be empty" >&2
  exit 2
fi
export BIONEMOR_PACKAGE_SPEC="$package_spec"

sudo -n apt-get update
sudo -n env DEBIAN_FRONTEND=noninteractive \
  apt-get install -y ca-certificates curl git tar
sudo -n curl -fsSL \
  https://rig.r-pkg.org/deb/rig.gpg \
  -o /etc/apt/trusted.gpg.d/rig.gpg
printf '%s\n' "deb http://rig.r-pkg.org/deb rig main" |
  sudo -n tee /etc/apt/sources.list.d/rig.list >/dev/null
sudo -n apt-get update
sudo -n env DEBIAN_FRONTEND=noninteractive apt-get install -y r-rig

sudo -n rig add release
sudo -n rig default release
Rscript --vanilla - <<'RSCRIPT'
stopifnot(getRversion() >= "4.2")
pak::pkg_install(Sys.getenv("BIONEMOR_PACKAGE_SPEC"))
RSCRIPT

workspace="$HOME/workspace/bionemor"
mkdir -p "$workspace"
printf 'bionemor is installed; persistent workspace: %s\n' "$workspace"
