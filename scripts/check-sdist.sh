#!/usr/bin/env bash
set -euo pipefail

# Run from the repository root. Build the actual published sources without the
# repository's project configuration or any Nix development-tool overrides.
review_sdist_dir=$(mktemp -d)
trap 'rm -rf "$review_sdist_dir"' EXIT
cabal sdist --output-directory="$review_sdist_dir"
review_archives=("$review_sdist_dir"/*.tar.gz)
if [[ ${#review_archives[@]} -ne 1 || ! -f ${review_archives[0]} ]]; then
  echo "Expected one source distribution" >&2
  exit 1
fi
tar -xzf "${review_archives[0]}" -C "$review_sdist_dir"
review_sources=("$review_sdist_dir"/jev-*/)
cd "${review_sources[0]}"
printf 'packages: .\n' > cabal.project
cabal check
cabal build all --enable-tests
cabal test all
