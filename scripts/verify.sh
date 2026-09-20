#!/usr/bin/env bash
set -euo pipefail

echo "Checking package metadata..."
cabal check

echo "Building project..."
cabal build all

echo "Running tests..."
cabal test all

echo "Checking source distribution..."
./scripts/check-sdist.sh
echo "Verification passed!"
