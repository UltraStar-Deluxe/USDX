#!/usr/bin/env bash

set -e

root=$(pwd)

SRC="$root/deps"
export SHELL=/bin/bash
export PREFIX="$root/prefix"
export PATH="$PREFIX/bin:$PATH"
export LD_LIBRARY_PATH="$PREFIX/lib:$LD_LIBRARY_PATH"
PKG_CONFIG_PATH="$PREFIX/lib/pkgconfig:$PKG_CONFIG_PATH"

echo "Building dependencies"
mkdir -pv "$PREFIX"

# multicore compilation
makearg="-j$(nproc)"

rm -rf "$PREFIX"

echo "Building libpng"
cd "$SRC/libpng"
./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH"  --disable-static
make $makearg
make install
make distclean

echo "Building FreeType"
cd "$SRC/freetype"
./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" --disable-static --with-harfbuzz=no --with-png=yes --with-bzip2=no
make $makearg
make install
make distclean

echo "Building SDL2"
cd "$SRC/SDL2"
bash ./autogen.sh
mkdir -p build
cd build
../configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" \
	--enable-sdl-dlopen \
	--disable-arts --disable-esd --disable-nas \
	--enable-alsa --enable-pulseaudio-shared \
	--enable-video-wayland --enable-wayland-shared \
	--enable-x11-shared --enable-ibus --enable-fcitx --enable-ime \
	--disable-rpath
make $makearg
make install
make distclean

echo "Building SDL2_mixer"
cd "$SRC/SDL2_mixer"
bash ./autogen.sh
./configure --prefix="$PREFIX" --with-sdl-prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" --disable-static --disable-music-mod --disable-music-midi --disable-smpegtest --disable-music-mp3 --disable-music-ogg --disable-music-flac --disable-music-midi
make $makearg
make install
make distclean

echo "Building SDL2_image"
cd "$SRC/SDL2_image"
bash ./autogen.sh
./configure --prefix="$PREFIX" --with-sdl-prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" --disable-static
make $makearg
make install
make distclean

echo "Building SDL2_ttf"
cd "$SRC/SDL2_ttf"
bash ./autogen.sh
./configure --prefix="$PREFIX" --with-sdl-prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" --disable-static
make $makearg
make install
make distclean

echo "Building SDL2_net"
cd "$SRC/SDL2_net"
bash ./autogen.sh
./configure --prefix="$PREFIX" --with-sdl-prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" --disable-static --disable-gui
make $makearg
make install
make distclean

echo "Building SDL2_gfx"
cd "$SRC/SDL2_gfx"
bash ./autogen.sh
./configure --prefix="$PREFIX" --with-sdl-prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" --disable-static
make $makearg
make install
make distclean

echo "Building SQLite"
cd "$SRC/sqlite"
./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" --disable-static
make $makearg
make install
make distclean

echo "Building PortAudio"
cd "$SRC/portaudio"
./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" --disable-static
make $makearg
make install
make distclean

#	echo "Building PCRE"
#	cd "$SRC/pcre"
#	./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" --disable-static --enable-utf --enable-unicode-properties
#	make $makearg
#	make install
#	make distclean

echo "Building Yasm"
cd "$SRC/yasm"
./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" --disable-static
make $makearg
make install
make distclean


echo "Building FFmpeg"
cd "$SRC/ffmpeg"
./configure --prefix="$PREFIX" \
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
	--disable-bzlib
make $makearg
make install
make distclean

# echo "Building projectM"
# cd "$SRC/projectm"
# mkdir -p build
# cd build
# cmake \
# 	-Wno-dev \
# 	-DINCLUDE-PROJECTM-QT=0 \
# 	-DINCLUDE-PROJECTM-PULSEAUDIO=0 \
# 	-DINCLUDE-PROJECTM-LIBVISUAL=0 \
# 	-DINCLUDE-PROJECTM-JACK=0 \
# 	-DINCLUDE-PROJECTM-TEST=0 \
# 	-DINCLUDE-PROJECTM-XMMS=0 \
# 	-DCMAKE_INSTALL_PREFIX="$PREFIX" \
# 	-DCMAKE_BUILD_TYPE=Release \
# 	..
# make
# make install

touch "$PREFIX/built_libs"
