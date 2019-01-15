#!/usr/bin/env bash

set -eo pipefail

root=$(pwd)

OUTPUT="$root/output"
export SHELL=/bin/bash
export PREFIX="$root/prefix"
[ "$(uname -m)" == "i686" ] && export PREFIX="${PREFIX}32"
LIBDIR="lib"
[ "$(uname -m)" == "i686" ] && LIBDIR="${LIBDIR}32"
EXE="ultrastardx.x86_64"
[ "$(uname -m)" == "i686" ] && EXE="ultrastardx.x86"
export PATH="$PREFIX/bin:$PATH"

./tasks.sh usdx

mv "$OUTPUT/ultrastardx" "$OUTPUT/$EXE"

# strip executable
strip -s "$OUTPUT/$EXE"

# set executable rpath
patchelf --set-rpath "\$ORIGIN/$LIBDIR" "$OUTPUT/$EXE"

tput setaf 2 && tput bold
echo "==> Scanning and copying libraries..."
tput sgr0
rm -rf "$OUTPUT/$LIBDIR"
mkdir -p "$OUTPUT/$LIBDIR"
LD_LIBRARY_PATH="$PREFIX/lib" "$root/scan_libs.sh" "$OUTPUT/$EXE" "$OUTPUT/$LIBDIR" | tee "$OUTPUT/$LIBDIR/libs.txt"
sed -r -i "s/\x1B(\[[0-9;]*[JKmsu]|\(B)//g" "$OUTPUT/$LIBDIR/libs.txt" # remove color codes

mkdir -p "$OUTPUT/data/songs"
cp launch.sh ../../LICENSE ../../game/LICENSE.* "$OUTPUT/"
echo -e "Version: $(cat ../../VERSION)\nBuild date: `date -u +%FT%TZ`" > "$OUTPUT/VERSION"
