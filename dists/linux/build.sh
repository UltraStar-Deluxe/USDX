#!/usr/bin/env bash

set -eo pipefail

root=$(pwd)

OUTPUT="$root/output"
export SHELL=/bin/bash
export PREFIX="$root/prefix"
LIBDIR="lib"
export PATH="$PREFIX/bin:$PATH"

./tasks.sh usdx


# strip executable
strip -s "$OUTPUT/ultrastardx"

# set executable rpath
patchelf --set-rpath "\$ORIGIN/$LIBDIR" "$OUTPUT/ultrastardx"

tput setaf 2 && tput bold
echo "==> Scanning and copying libraries..."
tput sgr0
rm -rf "$OUTPUT/$LIBDIR"
mkdir -p "$OUTPUT/$LIBDIR"
LD_LIBRARY_PATH="$PREFIX/lib" "$root/scan_libs.sh" "$OUTPUT/ultrastardx" "$OUTPUT/$LIBDIR" | tee "$OUTPUT/$LIBDIR/libs.txt"
sed -r -i "s/\x1B(\[[0-9;]*[JKmsu]|\(B)//g" "$OUTPUT/$LIBDIR/libs.txt" # remove color codes

mkdir -p "$OUTPUT/data/songs"
cp launch.sh ../../LICENSE ../../game/LICENSE.* "$OUTPUT/"
echo -e "Version: $(cat ../../VERSION)\nBuild date: `date -u +%FT%TZ`" > "$OUTPUT/VERSION"
