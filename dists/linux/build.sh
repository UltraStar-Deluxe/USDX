#!/usr/bin/env bash

set -eo pipefail

root=$(pwd)

OUTPUT="$root/output"
[ "$(uname -m)" == "i686" ] && OUTPUT="${OUTPUT}32"
export SHELL=/bin/bash
export PREFIX="$root/prefix"
[ "$(uname -m)" == "i686" ] && export PREFIX="${PREFIX}32"
export PATH="$PREFIX/bin:$PATH"
export LD_LIBRARY_PATH="$PREFIX/lib:$LD_LIBRARY_PATH"
PKG_CONFIG_PATH="$PREFIX/lib/pkgconfig:$PKG_CONFIG_PATH"

./tasks.sh usdx

tput setaf 2 && tput bold
echo "==> Scanning and copying libraries..."
tput sgr0
rm -rf "$OUTPUT/lib"
mkdir -p "$OUTPUT/lib"
"$root/scan_libs.sh" "$OUTPUT/ultrastardx" "$OUTPUT/lib" | tee "$OUTPUT/lib/libs.txt"
sed -r -i "s/\x1B(\[[0-9;]*[JKmsu]|\(B)//g" "$OUTPUT/lib/libs.txt" # remove color codes

# strip executable
strip -s "$OUTPUT/ultrastardx"
# strip libs
find "$OUTPUT/lib" -type f -name "*.so*" -exec strip -s {} \;
# remove rpath from libs
find "$OUTPUT/lib" -type f -name "*.so*" -exec chrpath --delete --keepgoing {} \;

mkdir -p "$OUTPUT/data/songs"
cp launch.sh ../../LICENSE ../../game/LICENSE.* "$OUTPUT/"
echo -e "Version: $(cat ../../VERSION)\nBuild date: `date -u +%FT%TZ`" > "$OUTPUT/VERSION"
