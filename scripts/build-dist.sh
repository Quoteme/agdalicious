#!/usr/bin/env bash
# Assemble what the `dist` branch holds: the neovim plugin at the top, its
# tree-sitter queries, and the tree-sitter parser prebuilt for each platform
# (cross-compiled with zig), so that installing that branch with a plugin
# manager needs nothing else.
#
#   scripts/build-dist.sh OUT_DIR
#
# The plugin picks parsers/agda-<os>-<arch>.so for the platform it runs on
# (see nvim/lua/agdalicious/init.lua).
set -euo pipefail

out=$1
root=$(cd "$(dirname "$0")/.." && pwd)
src=$root/tree-sitter-agda/src

rm -rf "$out"
mkdir -p "$out/parsers" "$out/queries/agda"
cp -r "$root/nvim/." "$out/"
cp "$root/tree-sitter-agda/queries/"*.scm "$out/queries/agda/"
cp "$root/README.md" "$out/"

# zig target, and the platform as the plugin names it
while read -r target platform; do
  zig cc -target "$target" -shared -fPIC -O2 -I "$src" \
    "$src/parser.c" "$src/scanner.c" -o "$out/parsers/agda-$platform.so"
done <<'EOF'
x86_64-linux-gnu.2.17 linux-x86_64
aarch64-linux-gnu.2.17 linux-aarch64
x86_64-macos macos-x86_64
aarch64-macos macos-aarch64
x86_64-windows-gnu windows-x86_64
aarch64-windows-gnu windows-aarch64
EOF

# what the Windows builds leave besides the DLLs (debug info, import library)
rm -f "$out/parsers/"*.pdb "$out/parsers/"*.lib
