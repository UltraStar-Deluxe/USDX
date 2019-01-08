#!/usr/bin/env bash

set -eo pipefail

root=$(pwd)

SRC="$root/deps"
OUTPUT="$root/output"
export SHELL=/bin/bash
export PREFIX="$root/prefix"
export PATH="$PREFIX/bin:$PATH"
export LD_LIBRARY_PATH="$PREFIX/lib:$LD_LIBRARY_PATH"
PKG_CONFIG_PATH="$PREFIX/lib/pkgconfig:$PKG_CONFIG_PATH"

[ "$(uname -m)" == "i686" ] && M32="-m32"
export CPPFLAGS="-I$PREFIX/include $M32"
export CFLAGS="-I$PREFIX/include $M32"
[ -f /.dockerenv ] && export PPC_CONFIG_PATH="$PREFIX/etc"

export CC="gcc"
export CXX="g++"

# export CC="$CC -U_FORTIFY_SOURCE -include $SRC/libcwrap.h"
# export CXX="$CXX -U_FORTIFY_SOURCE -D_GLIBCXX_USE_CXX11_ABI=0 -include $SRC/libcwrap.h"

echo "Building UltraStar Deluxe"
cd "$root/../.."
bash ./autogen.sh
./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CC="$CC" CXX="$CXX" --enable-debug
sleep 1
# -rpath \\\$\$ORIGIN/$1
make LDFLAGS="-O2 --sort-common --as-needed -z relro" datadir="./data" prefix="" bindir="" INSTALL_DATADIR="./data"
rm -rf "$OUTPUT"
sleep 1
make DESTDIR="$OUTPUT/" datadir="/data" prefix="" bindir="" INSTALL_DATADIR="./data" install
make clean

mkdir -p "$OUTPUT/lib"

scan_libs() {
	if [ ! -f "$1" ]; then return; fi
	local libs=$(objdump -x "$1" | awk '$1 == "NEEDED" { print $2 }' | grep -E -v '(libc[^_a-zA-Z0-9])|(libm[^_a-zA-Z0-9])|libpthread|(librt[^_a-zA-Z0-9])|(libdl[^_a-zA-Z0-9])|(libcrypt[^_a-zA-Z0-9])|(libutil[^_a-zA-Z0-9])|(libnsl[^_a-zA-Z0-9])|(libresolv[^_a-zA-Z0-9])|libasound|libglib|libgcc_s|libX11|ld-linux|(libstdc\+\+[^_a-zA-Z0-9])')
	if [ -z "$libs" ]; then return; fi
	local lddoutput=$(ldd "$1")
	#echo $3${1##*/}
	local indent="  $4"
	local IFS=$'\n'
	while read -r file
	do
		if [ -z "$file" ]; then continue; fi
		local filepath=$(echo "$lddoutput" | grep -F "$file" | awk '{print $3}')
		if [ -e "$filepath" ]; then
			echo "$indent$file"
			if [ -e "$filepath" ] && [ ! -e "$2/$file" ]; then
				cp "$filepath" "$2/"
				scan_libs "$filepath" "$2" "" "$indent"
			fi
		fi
		if [ ! -e "$filepath" ]; then
			echo "$filepath not found"
		fi
	done <<< "$libs"

	# handle extras
	local IFS=' '
	while read -r file
	do
		if [ -z "$file" ]; then continue; fi
		local filepath="$PREFIX/lib/$file"
		if [ -e "$filepath" ] && [ ! -e "$2/$file" ]; then
			echo "$indent$file"
			cp "$filepath" "$2/"
			scan_libs "$filepath" "$2" "" "$indent"
		fi
		if [ ! -e "$filepath" ]; then
			echo "$filepath not found"
		fi
	done <<< "$3"
}

echo "Scanning and copying libraries..."
scan_libs "$OUTPUT/ultrastardx" "$OUTPUT/lib" | tee "$OUTPUT/lib/libs.txt"

# strip executable
strip -s "$OUTPUT/ultrastardx"
# strip libs
find "$OUTPUT/lib" -type f -name "*.so*" -exec strip -s {} \;
# remove rpath from libs
find "$OUTPUT/lib" -type f -name "*.so*" -exec chrpath --delete --keepgoing {} \;

