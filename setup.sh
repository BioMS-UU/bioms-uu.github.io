#!/usr/bin/env bash
# Run this ONCE on every machine that works on the book, right after cloning.
# It installs the merge driver that stops docs/ from producing conflicts.
set -euo pipefail
cd "$(dirname "$0")"

git config merge.keeptheirs.name "take the incoming copy of generated files"
git config merge.keeptheirs.driver 'cp -f "%B" "%A"'
git config pull.rebase false

echo "Merge driver installed."

version=$(tr -d '[:space:]' < .quarto-version)
have=$(quarto --version 2>/dev/null || echo "")

if [ -z "$have" ]; then
  echo
  echo "Quarto is not installed. Install it with:"
  echo "  brew install quarto"
  echo "and run this script again."
elif [ "$version" != "$have" ]; then
  echo
  echo "Quarto version: you have $have, this book was last built with $version."
  echo "Minor differences are fine. To update: brew upgrade quarto"
else
  echo "Quarto version: $version ✓"
fi
