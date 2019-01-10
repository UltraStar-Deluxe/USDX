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

./build-deps.sh usdx

mkdir -p "$OUTPUT/lib"

tput setaf 2 && tput bold
echo "==> Scanning and copying libraries..."
tput sgr0

"$root/scan_libs.sh" "$OUTPUT/ultrastardx" "$OUTPUT/lib" | tee "$OUTPUT/lib/libs.txt"
sed -r -i "s/\x1B(\[[0-9;]*[JKmsu]|\(B)//g" "$OUTPUT/lib/libs.txt" # remove color codes

tput setaf 2 && tput bold
echo "==> Stripping executable and libraries..."
tput sgr0

# strip executable
strip -s "$OUTPUT/ultrastardx"
# strip libs
find "$OUTPUT/lib" -type f -name "*.so*" -exec strip -s {} \;
# remove rpath from libs
find "$OUTPUT/lib" -type f -name "*.so*" -exec chrpath --delete --keepgoing {} \;
