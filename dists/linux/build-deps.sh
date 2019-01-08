#!/usr/bin/env bash

set -eo pipefail

root=$(pwd)

SRC="$root/deps"
export SHELL=/bin/bash
export PREFIX="$root/prefix"
export PATH="$PREFIX/bin:$PATH"
export LD_LIBRARY_PATH="$PREFIX/lib:$LD_LIBRARY_PATH"
PKG_CONFIG_PATH="$PREFIX/lib/pkgconfig:$PKG_CONFIG_PATH"

[ "$(uname -m)" == "i686" ] && M32="-m32"
export LDFLAGS="-L$PREFIX/lib $M32"
export CPPFLAGS="-I$PREFIX/include $M32"
export CFLAGS="-I$PREFIX/include $M32"

export CC="gcc"
export CXX="g++"

# export CC="$CC -U_FORTIFY_SOURCE -include $SRC/libcwrap.h"
# export CXX="$CXX -U_FORTIFY_SOURCE -D_GLIBCXX_USE_CXX11_ABI=0 -include $SRC/libcwrap.h"

echo "Building dependencies"

# multicore compilation
makearg="-j$(nproc)"

clean_prefix() {
	rm -rf "$PREFIX"
	mkdir -pv $PREFIX/{etc,bin,include,lib}
}

build_zlib() {
	echo "Building zlib"
	cd "$SRC/zlib"
	./configure --prefix="$PREFIX"
	make $makearg
	make install
	make distclean
}

build_libpng() {
	echo "Building libpng"
	cd "$SRC/libpng"
	./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CC="$CC" CXX="$CXX" \
		--disable-static
	make $makearg
	make install
	make distclean
}

build_freetype() {
	echo "Building FreeType"
	cd "$SRC/freetype"
	./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CC="$CC" CXX="$CXX" \
		--disable-static --with-harfbuzz=no --with-png=yes --with-bzip2=no
	make $makearg
	make install
	make distclean
}

build_sdl2() {
	echo "Building SDL2"
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

build_sdl2_image() {
	echo "Building SDL2_image"
	cd "$SRC/SDL2_image"
	bash ./autogen.sh
	./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CC="$CC" CXX="$CXX" \
		--disable-static --disable-jpg-shared --disable-png-shared
	make $makearg
	make install
	make distclean
}

build_sqlite() {
	echo "Building SQLite"
	cd "$SRC/sqlite"
	./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CC="$CC" CXX="$CXX" \
		--disable-static
	make $makearg
	make install
	make distclean
}

build_portaudio() {
	echo "Building PortAudio"
	cd "$SRC/portaudio"
	./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CC="$CC" CXX="$CXX" \
		--without-jack --disable-static
	make $makearg
	make install
	make distclean
}

build_yasm() {
	echo "Building Yasm"
	cd "$SRC/yasm"
	./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CC="$CC" CXX="$CXX" \
		--disable-static
	make $makearg
	make install
	make distclean
}

build_ffmpeg() {
	echo "Building FFmpeg"
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

install_fpc() {
	arch=$(uname -m)
	echo "Installing fpc for $arch"
	cd "$SRC/fpc-$arch"
	printf "$PREFIX\nn\nn\nn" | ./install.sh
	"$PREFIX/lib/fpc/3.0.4/samplecfg" "$PREFIX/lib/fpc/3.0.4" "$PREFIX/etc"
}

# START OF BUILD PROCESS

if [ "$1" == "all" ]; then
	echo "Building all dependencies"
	clean_prefix

	build_zlib

	build_libpng
	build_freetype

	build_sdl2
	build_sdl2_image

	build_sqlite
	build_portaudio

	build_yasm
	build_ffmpeg

	if [ -f /.dockerenv ]; then
		install_fpc
	fi

elif [ ! -z "$1" ]; then
	echo "Building dependencies $1"
	$1
fi

touch "$PREFIX/built_libs"
