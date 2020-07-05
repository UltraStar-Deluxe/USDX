#!/usr/bin/env bash

set -eo pipefail

root=$(pwd)


export SHELL=/bin/bash
ARCH=$(uname -m)
export PREFIX="$root/prefix/$ARCH"
OUTPUT="$root/build/$ARCH/usr/bin"
LIBDIR="../lib"
export PATH="$PREFIX/bin:$PATH"

./tasks.sh usdx

tput setaf 2 && tput bold
echo "==> Scanning and copying libraries..."
tput sgr0
rm -rf "$OUTPUT/$LIBDIR"
mkdir -p "$OUTPUT/$LIBDIR"
LD_LIBRARY_PATH="$PREFIX/lib" "$root/scan_libs.py" "$OUTPUT/ultrastardx" "$OUTPUT/$LIBDIR" --strip --rpath | tee "$OUTPUT/$LIBDIR/libs.txt"
sed -r -i "s/\x1B(\[[0-9;]*[JKmsu]|\(B)//g" "$OUTPUT/$LIBDIR/libs.txt" # remove color codes

# strip executable
strip -s "$OUTPUT/ultrastardx"

# set executable rpath
patchelf --set-rpath "\$ORIGIN/$LIBDIR" "$OUTPUT/ultrastardx"

mkdir -p "$OUTPUT/data/songs"
cp ../../LICENSE ../../game/LICENSE.* "$OUTPUT/"
echo -e "Version: $(cat ../../VERSION)\nBuild date: `date -u +%FT%TZ`" > "$OUTPUT/VERSION"
