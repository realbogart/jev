#!/usr/bin/env bash

set -e

echo "Running tests..."
cabal test
echo "Tests completed!"