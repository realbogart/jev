#!/usr/bin/env bash
set -euo pipefail

echo "Formatting Haskell files..."
while IFS= read -r -d '' source; do
  ormolu --ghc-opt=-XGHC2021 --ghc-opt=-XOverloadedRecordDot --mode inplace "$source"
done < <(find lib/ test/ examples/ -name '*.hs' -print0)
echo "Formatting complete!"
