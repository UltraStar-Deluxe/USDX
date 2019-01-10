#!/usr/bin/env bash

set -eo pipefail

root=$(pwd)

SRC="$root/deps"
export SHELL=/bin/bash
export PREFIX="$root/prefix"
[ "$(uname -m)" == "i686" ] && export PREFIX="${PREFIX}32"
export PATH="$PREFIX/bin:$PATH"
export LD_LIBRARY_PATH="$PREFIX/lib:$LD_LIBRARY_PATH"
PKG_CONFIG_PATH="$PREFIX/lib/pkgconfig:$PKG_CONFIG_PATH"
[ -f /.dockerenv ] && export PPC_CONFIG_PATH="$PREFIX/etc"

[ "$(uname -m)" == "i686" ] && M32="-m32"
export LDFLAGS="-L$PREFIX/lib $M32"
export CPPFLAGS="-I$PREFIX/include $M32"
export CFLAGS="-I$PREFIX/include $M32"

export CC="gcc -U_FORTIFY_SOURCE"
export CXX="g++ -U_FORTIFY_SOURCE"

# export CC="$CC -include $SRC/libcwrap.h"
# export CXX="$CXX -D_GLIBCXX_USE_CXX11_ABI=0 -include $SRC/libcwrap.h"

# multicore compilation
makearg="-j$(nproc)"

clean_prefix() {
	tput setaf 2 && tput bold
	echo "==> Cleaning prefix"
	tput sgr0
	rm -rf "$PREFIX"
	mkdir -pv $PREFIX/{etc,bin,include,lib}
}

task_zlib() {
	tput setaf 2 && tput bold
	echo "==> Building zlib"
	tput sgr0
	cd "$SRC/zlib"
	./configure --prefix="$PREFIX"
	make $makearg
	make install
	make distclean
}

task_libpng() {
	tput setaf 2 && tput bold
	echo "==> Building libpng"
	tput sgr0
	cd "$SRC/libpng"
	./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CC="$CC" CXX="$CXX" \
		--disable-static
	make $makearg
	make install
	make distclean
}

task_freetype() {
	tput setaf 2 && tput bold
	echo "==> Building FreeType"
	tput sgr0
	cd "$SRC/freetype"
	./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CC="$CC" CXX="$CXX" \
		--disable-static --with-harfbuzz=no --with-png=yes --with-bzip2=no
	make $makearg
	make install
	make distclean
}

task_sdl2() {
	tput setaf 2 && tput bold
	echo "==> Building SDL2"
	tput sgr0
	cd "$SRC/SDL2"
	bash ./autogen.sh
	mkdir -p build
	cd build
	../configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CC="$CC" CXX="$CXX" \
		--enable-sdl-dlopen \
		--disable-arts --disable-esd --disable-nas \
		--disable-sndio --enable-alsa --enable-pulseaudio-shared \
		--enable-video-wayland --enable-wayland-shared \
		--enable-x11-shared --enable-ibus --enable-fcitx --enable-ime \
		--disable-rpath --disable-input-tslib
	make $makearg
	make install
	make distclean
}

task_sdl2_image() {
	tput setaf 2 && tput bold
	echo "==> Building SDL2_image"
	tput sgr0
	cd "$SRC/SDL2_image"
	bash ./autogen.sh
	./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CC="$CC" CXX="$CXX" \
		--disable-static --disable-jpg-shared --disable-png-shared --disable-webp-shared
	make $makearg
	make install
	make distclean
}

task_sqlite() {
	tput setaf 2 && tput bold
	echo "==> Building SQLite"
	tput sgr0
	cd "$SRC/sqlite"
	./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CC="$CC" CXX="$CXX" \
		--disable-static
	make $makearg
	make install
	make distclean
}

task_portaudio() {
	tput setaf 2 && tput bold
	echo "==> Building PortAudio"
	tput sgr0
	cd "$SRC/portaudio"
	./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CC="$CC" CXX="$CXX" \
		--without-jack --disable-static
	make $makearg
	make install
	make distclean
}

task_portmidi() {
	tput setaf 2 && tput bold
	echo "==> Building PortMidi"
	tput sgr0
	cd "$SRC"
	find portmidi/ portmidi-debian/patches/ -type f | while read a ; do
		tr -d '\r' < "$a" > t
		cat t >$a
	done
	rm t
	cd portmidi/portmidi
	while read a ; do
		patch -l -N -r - -p1 < ../../portmidi-debian/patches/$a || true
	done < ../../portmidi-debian/patches/series
	rm -rf build
	mkdir -p build
	cd build
	cmake \
		-DCMAKE_CACHEFILE_DIR="$(pwd)/out" \
		-DCMAKE_INSTALL_PREFIX="$PREFIX" \
		-DCMAKE_BUILD_TYPE="Release" \
		-DCMAKE_C_FLAGS="$CFLAGS" \
		-DCMAKE_CXX_FLAGS="$CPPFLAGS" \
		-DDCMAKE_SHARED_LINKER_FLAGS="$LDFLAGS" \
		..
	make portmidi-dynamic
	make -C pm_dylib install
	cd ..
	rm -r build
}

task_yasm() {
	tput setaf 2 && tput bold
	echo "==> Building Yasm"
	tput sgr0
	cd "$SRC/yasm"
	./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CC="$CC" CXX="$CXX" \
		--disable-static
	make $makearg
	make install
	make distclean
}

task_ffmpeg() {
	tput setaf 2 && tput bold
	echo "==> Building FFmpeg"
	tput sgr0
	cd "$SRC/ffmpeg"
	# disable vaapi until it can be tested
	./configure --prefix="$PREFIX" \
		--cc="$CC" \
		--cxx="$CXX" \
		--enable-gpl \
		--disable-static \
		--enable-shared \
		--disable-programs \
		--disable-doc \
		--disable-encoders \
		--disable-xlib \
		--disable-libxcb \
		--disable-libxcb-shm \
		--disable-libx264 \
		--disable-libx265 \
		--disable-network \
		--disable-debug \
		--disable-indevs \
		--disable-outdevs \
		--disable-postproc \
		--disable-muxers \
		--disable-bsfs \
		--disable-filters \
		--disable-protocols \
		--disable-lzma \
		--disable-bzlib \
		--disable-vaapi \
		--disable-vdpau
	make $makearg
	make install
	make distclean
}

task_fpc() {
	arch=$(uname -m)
	tput setaf 2 && tput bold
	echo "==> Installing fpc for $arch"
	cd "$SRC/fpc-$arch"
	tput sgr0
	printf "$PREFIX\nn\nn\nn" | ./install.sh
	"$PREFIX/lib/fpc/3.0.4/samplecfg" "$PREFIX/lib/fpc/3.0.4" "$PREFIX/etc"
}

task_usdx() {
	tput setaf 2 && tput bold
	echo "==> Building UltraStar Deluxe"
	tput sgr0
	local OUTPUT="$root/output"
	[ "$(uname -m)" == "i686" ] && OUTPUT="${OUTPUT}32"
	cd "$root/../.."
	bash ./autogen.sh
	./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CC="$CC" CXX="$CXX"
	sleep 1
	make LDFLAGS="-O2 --sort-common --as-needed -z relro" datadir="./data" prefix="" bindir="" INSTALL_DATADIR="./data"
	rm -rf "$OUTPUT"
	make DESTDIR="$OUTPUT/" datadir="/data" prefix="" bindir="" INSTALL_DATADIR="./data" install
	make clean
}

# START OF BUILD PROCESS

if [ "$1" == "all" ]; then
	tput setaf 2 && tput bold
	echo "==> Building all dependencies"
	tput sgr0

	clean_prefix
	echo

	task_zlib
	echo

	task_libpng
	echo
	task_freetype
	echo

	task_sdl2
	echo
	task_sdl2_image
	echo

	task_sqlite
	echo
	task_portaudio
	echo
	task_portmidi
	echo

	task_yasm
	echo
	task_ffmpeg

	if [ -f /.dockerenv ]; then
		echo
		task_fpc
	fi

	touch "$PREFIX/built_libs"

elif [ ! -z "$1" ]; then
	if [ "$(type -t "task_$1")" = "function" ]; then
		"task_$1"
	else
		tput setaf 1 && tput bold
		echo "Task $1 does not exist"
		tput sgr0
	fi
fi
