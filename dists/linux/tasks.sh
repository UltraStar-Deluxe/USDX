#!/usr/bin/env bash

set -eo pipefail

root=$(pwd)

SRC="$root/deps"
export SHELL=/bin/bash
ARCH=$(uname -m)
export PREFIX="$root/prefix/$ARCH"
export PATH="$PREFIX/bin:$PATH"
export LD_LIBRARY_PATH="$PREFIX/lib:$LD_LIBRARY_PATH"
PKG_CONFIG_PATH="$PREFIX/lib/pkgconfig:$PREFIX/share/pkgconfig:$PKG_CONFIG_PATH"
export CMAKE_PREFIX_PATH="$PREFIX:$CMAKE_PREFIX_PATH"
[ -f /.dockerenv ] && export PPC_CONFIG_PATH="$PREFIX/etc"

export LDFLAGS="-Wl,-z,now -Wl,-z,relro -L$PREFIX/lib"
export CPPFLAGS="-I$PREFIX/include"
export CFLAGS="-O2 -fPIE $CPPFLAGS"
[ "$ARCH" == "i686" ] && export CFLAGS+=" -mincoming-stack-boundary=2"
export CXXFLAGS="$CFLAGS"

export CC="gcc"
export CXX="g++"


# multicore compilation
makearg="-j$(nproc)"

clean_prefix() {
	tput setaf 2 && tput bold
	echo "==> Cleaning prefix"
	tput sgr0
	rm -rf "$PREFIX"
	mkdir -pv $PREFIX/{etc,bin,include,lib}
}

task_projectm() {
	tput setaf 2 && tput bold
	echo "==> Building projectM"
	tput sgr0
	cd "$SRC/projectm"
	patch -p1 < $root/../flatpak/patches/projectM-2.2.1.patch
	chmod a+x autogen.sh
	./autogen.sh
	./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CC="$CC" CXX="$CXX" \
		--disable-static
	make $makearg
	make install
	make distclean
}

task_AppImageKit() {
	tput setaf 2 && tput bold
	echo "==> Building AppImageKit"
	tput sgr0
	cd "$SRC/AppImageKit"
	! pkg-config --exists libarchive || export EXTRA_CMAKE_FLAGS="-DUSE_SYSTEM_LIBARCHIVE=ON"
	./build.sh
	unset EXTRA_CMAKE_FLAGS
	cp build/out/appimagetool $PREFIX/bin/
	mkdir $PREFIX/lib/appimagekit
	cp build/out/mksquashfs $PREFIX/lib/appimagekit
	rm -fR build
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

task_wayland() {
	tput setaf 2 && tput bold
	echo "==> Building wayland"
	tput sgr0
	cd "$SRC/wayland"
	./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" --disable-documentation
	make $makearg
	make install
	make distclean
}

task_wayland_protocols() {
	tput setaf 2 && tput bold
	echo "==> Building wayland-protocols"
	tput sgr0
	cd "$SRC/wayland-protocols"
	./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH"
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
	sed -i 's/GBM_BO_USE_CURSOR\>/&_64X64/g' src/video/kmsdrm/SDL_kmsdrmmouse.c
	mkdir -p build
	cd build
	../configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CC="$CC" CXX="$CXX" \
		--enable-sdl-dlopen \
		--disable-arts --disable-esd --disable-nas \
		--disable-sndio --enable-pulseaudio-shared --enable-pulseaudio \
		--enable-jack --enable-jack-shared \
		--enable-video-opengl --disable-video-opengles1 \
		--enable-video-wayland --enable-wayland-shared \
		--enable-video-kmsdrm --enable-kmsdrm-shared \
		--disable-video-vulkan \
		--enable-x11-shared --disable-ibus --disable-fcitx --disable-ime \
		--disable-rpath --disable-input-tslib
	make $makearg
	make install
	make distclean
}

task_libjpeg_turbo() {
	tput setaf 2 && tput bold
	echo "==> Building libjpeg-turbo"
	tput sgr0
	cd "$SRC/libjpeg-turbo"
	rm -rf build
	mkdir -p build
	cd build
	cmake \
		-DCMAKE_CACHEFILE_DIR="$(pwd)/out" \
		-DCMAKE_INSTALL_PREFIX="$PREFIX" \
		-DCMAKE_INSTALL_LIBDIR:PATH="lib" \
		-DCMAKE_BUILD_TYPE="Release" \
		-DCMAKE_C_FLAGS="$CFLAGS" \
		-DCMAKE_SHARED_LINKER_FLAGS="$LDFLAGS" \
		-DWITH_TURBOJPEG=OFF \
		-DWITH_JAVA=OFF \
		-DWITH_ARITH_ENC=OFF \
		-DWITH_MEM_SRCDST=OFF \
		-DENABLE_SHARED=ON \
		-DENABLE_STATIC=OFF \
		..
	make
	make install
	cd ..
	rm -r build
}

task_sdl2_image() {
	tput setaf 2 && tput bold
	echo "==> Building SDL2_image"
	tput sgr0
	cd "$SRC/SDL2_image"
	bash ./autogen.sh
	./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CC="$CC" CXX="$CXX" \
		--disable-static --disable-jpg-shared --disable-png-shared --disable-webp-shared --disable-webp --disable-tif-shared --disable-tif
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
		-DCMAKE_CXX_FLAGS="$CXXFLAGS" \
		-DDCMAKE_SHARED_LINKER_FLAGS="$LDFLAGS" \
		..
	make portmidi-dynamic
	make -C pm_dylib install
	cd ..
	rm -r build
}

task_nasm() {
	tput setaf 2 && tput bold
	echo "==> Building NASM"
	tput sgr0
	cd "$SRC/nasm"
	./configure --prefix="$PREFIX" CC="$CC" CXX="$CXX"
	hide make $makearg
	hide make install
	hide make distclean
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

task_openssl() {
	tput setaf 2 && tput bold
	echo "==> Building OpenSSL"
	tput sgr0
	cd "$SRC/openssl"
	CC="$CC" CXX="$CXX" ./config --prefix="$PREFIX" no-hw threads shared zlib-dynamic
	make $makearg
	make install
	make distclean
}

task_python() {
	tput setaf 2 && tput bold
	echo "==> Building Python"
	tput sgr0
	cd "$SRC/python"
	./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CC="$CC" CXX="$CXX"
	make $makearg
	make install
	make distclean
}

task_ninja() {
	tput setaf 2 && tput bold
	echo "==> Building ninja"
	tput sgr0
	pip3 install ninja
}

task_meson() {
	tput setaf 2 && tput bold
	echo "==> Building meson"
	tput sgr0
	pip3 install meson
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
		--disable-lzo \
		--disable-bzlib \
		--disable-vaapi \
		--disable-vdpau
	make $makearg
	make install
	make distclean
}

# task_fpc() {
# 	arch=$(uname -m)
# 	tput setaf 2 && tput bold
# 	echo "==> Installing fpc for $arch"
# 	cd "$SRC/fpc-$arch"
# 	tput sgr0
# 	printf "$PREFIX\nn\nn\nn" | ./install.sh
# 	"$PREFIX/lib/fpc/3.0.4/samplecfg" "$PREFIX/lib/fpc/3.0.4" "$PREFIX/etc"
# }

task_patchelf() {
	tput setaf 2 && tput bold
	echo "==> Building PatchELF"
	tput sgr0
	cd "$SRC/patchelf"
	./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CC="$CC" CXX="$CXX"
	make $makearg
	make install
	make distclean
}

task_lua() {
	tput setaf 2 && tput bold
	echo "==> Building Lua"
	tput sgr0
	cd "$SRC/lua"
	if fgrep -q 'This is Lua 5.3.5,' README ; then
		sed -i '/^R=/s/4/5/' Makefile
	fi
	eval `make pc | grep ^version=`
	patch -p1 < $root/lua-relocatable.patch
	make INSTALL_TOP="$PREFIX" MYCFLAGS="-DLUA_COMPAT_5_3 -DLUA_COMPAT_5_2 -DLUA_COMPAT_5_1 $CFLAGS -fPIC" LUA_A="liblua.so.$version" AR="\$(CC) -shared -ldl -lm -Wl,-soname,liblua.so.${version%.*} -o" RANLIB=true ALL='$(LUA_A)' linux
	make INSTALL_TOP="$PREFIX" TO_LIB="liblua.so.$version" INSTALL_EXEC=true INSTALL_BIN= install
	ln -s liblua.so.$version "$PREFIX/lib/liblua.so.${version%.*}"
	ln -s liblua.so.$version "$PREFIX/lib/liblua.so"
	mkdir -p $PREFIX/lib/pkgconfig
	make -s INSTALL_TOP="$PREFIX" pc > "$PREFIX/lib/pkgconfig/lua.pc"
	cat >> "$PREFIX/lib/pkgconfig/lua.pc" <<"EOF"
Name: Lua
Description: An Extensible Language
Version: ${version}
Requires:
Libs: -L${libdir} -llua
Cflags: -I${includedir}
EOF
	make clean
}

task_opencv() {
	tput setaf 2 && tput bold
	echo "==> Building OpenCV"
	tput sgr0
	cd "$SRC/opencv"
	rm -rf build
	mkdir -p build
	cd build
	cmake \
		-DCMAKE_CACHEFILE_DIR="$(pwd)/out" \
		-DCMAKE_INSTALL_PREFIX="$PREFIX" \
		-DCMAKE_BUILD_TYPE="Release" \
		-DDCMAKE_SHARED_LINKER_FLAGS="$LDFLAGS" \
		-DOPENCV_GENERATE_PKGCONFIG=ON \
		-DBUILD_TESTS=OFF \
		-DBUILD_opencv_apps=OFF \
		-DBUILD_opencv_calib3d=OFF \
		-DBUILD_opencv_dnn=OFF \
		-DBUILD_opencv_features2d=OFF \
		-DBUILD_opencv_flann=OFF \
		-DBUILD_opencv_gapi=OFF \
		-DBUILD_opencv_highgui=OFF \
		-DBUILD_opencv_java_bindings_generator=OFF \
		-DBUILD_opencv_ml=OFF \
		-DBUILD_opencv_objdetect=OFF \
		-DBUILD_opencv_photo=OFF \
		-DBUILD_opencv_python2=OFF \
		-DBUILD_opencv_python_bindings_generator=OFF \
		-DBUILD_opencv_python_tests=OFF \
		-DBUILD_opencv_stitching=OFF \
		-DBUILD_opencv_ts=OFF \
		-DBUILD_opencv_video=OFF \
		-DCV_TRACE=OFF \
		-DWITH_PROTOBUF=OFF \
		-DWITH_FFMPEG=OFF \
		-DWITH_ADE=OFF \
		-DWITH_QUIRC=OFF \
		-DWITH_GTK=OFF \
		-DWITH_TIFF=OFF \
		-DWITH_WEBP=OFF \
		-DWITH_JASPER=OFF \
		-DWITH_OPENJPEG=OFF \
		-DWITH_PNG=OFF \
		-DWITH_OPENEXR=OFF \
		-DWITH_GDAL=OFF \
		-DWITH_GDCM=OFF \
		-DWITH_IMGCODEC_HDR=OFF \
		-DWITH_IMGCODEC_SUNRASTER=OFF \
		-DWITH_IMGCODEC_PXM=OFF \
		-DWITH_IMGCODEC_PFM=OFF \
		..
	make
	make install
	cd ..
	rm -r build
}

task_usdx() {
	tput setaf 2 && tput bold
	echo "==> Building UltraStar Deluxe"
	tput sgr0
	local OUTPUT="$root/build/$ARCH"
	cd "$root/../.."
	bash ./autogen.sh
	./configure --prefix=/usr PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CMAKE_PREFIX_PATH="$CMAKE_PREFIX_PATH" CC="$CC" CXX="$CXX" --enable-debug --with-opencv-cxx-api --without-portaudio --with-libprojectM --with-local-projectM-presets
	sleep 1
	make LDFLAGS="-O2 --sort-common --as-needed -z relro" INSTALL_DATADIR="../share/ultrastardx"
	rm -rf "$OUTPUT"
	make DESTDIR="$OUTPUT/" install
	rm -r "$OUTPUT/usr/share/ultrastardx/visuals/projectM/presets"
	local projectM_datadir=`PKG_CONFIG_PATH="$PKG_CONFIG_PATH" pkg-config --variable=pkgdatadir libprojectM`
	cp -av "$projectM_datadir/presets" "$OUTPUT/usr/share/ultrastardx/visuals/projectM"
	for i in 32 256 512 ; do
		mkdir -p "$OUTPUT/usr/share/icons/hicolor/${i}x${i}/apps"
		cp "$root/../../icons/ultrastardx-icon_${i}.png" "$OUTPUT/usr/share/icons/hicolor/${i}x${i}/apps/ultrastardx.png"
	done
	ln -s "usr/share/icons/hicolor/256x256/apps/ultrastardx.png" "$OUTPUT/.DirIcon"
	mkdir -p "$OUTPUT/usr/share/icons/hicolor/scalable/apps"
	cp "$root/../../icons/ultrastardx-icon.svg" "$OUTPUT/usr/share/icons/hicolor/scalable/apps/ultrastardx.svg"
	ln -s "usr/share/icons/hicolor/scalable/apps/ultrastardx.svg" "$OUTPUT/ultrastardx.svg"
	mkdir -p "$OUTPUT/usr/share/applications"
	cp "$root/../ultrastardx.desktop" "$OUTPUT/usr/share/applications"
	ln -s "usr/share/applications/ultrastardx.desktop" "$OUTPUT/"
	mkdir -p "$OUTPUT/usr/share/metainfo"
	appdata="$OUTPUT/usr/share/metainfo/ultrastardx.appdata.xml"
	cp "$root/../ultrastardx.appdata.xml" "$appdata"
	version=`git describe --tags --match='v*' || true`
	if [ -n "$version" ] ; then
		version=${version#v}
		case "$version" in
		*-*-g*)	t=development ;;
		*)      t=stable
		esac
		if ! grep -q "version=\"$version\"" "$appdata" ; then
			set -- `git log -n 1 --pretty=format:%ci`
			release="<release version=\"$version\" type=\"$t\" date=\"$1T$2${3:0:3}:${3:3}\" />"
			if grep -q '<releases>' "$appdata" ; then
				sed -i 's%\([[:space:]]*\)<releases>%&\n\1  '"$release%" "$appdata"
			else
				sed -i 's%\([[:space:]]*\)</component>%\1  <releases>'"$release"'</releases>\n&%' "$appdata"
			fi
		fi
	fi
	ln -s usr/bin/ultrastardx "$OUTPUT/AppRun"
	make clean
}

# START OF BUILD PROCESS

if [ "$1" == "all_deps" ]; then
	tput setaf 2 && tput bold
	echo "==> Building all dependencies"
	tput sgr0

	clean_prefix
	echo

	task_AppImageKit
	echo
	task_nasm
	echo
	task_yasm
	echo

	task_libjpeg_turbo
	echo
	task_libpng
	echo

	task_wayland
	echo
	task_wayland_protocols
	echo
	task_sdl2
	echo
	task_sdl2_image
	echo

	task_sqlite
	echo
	task_portmidi
	echo

	task_lua
	echo

	task_openssl
	echo
	task_python
	echo
	task_ninja
	echo
	task_meson
	echo
	task_ffmpeg

	# if [ -f /.dockerenv ]; then
	# 	echo
	# 	task_fpc
	# fi

	echo
	task_patchelf

	echo
	task_opencv
	echo
	task_projectm

	touch "$PREFIX/built_libs"

elif [ ! -z "$1" ]; then
	if [ "$(type -t "task_$1")" = "function" ]; then
		"task_$1"
	else
		tput setaf 1 && tput bold
		echo "==> Error: Task '$1' does not exist"
		tput sgr0
	fi
else
	echo "Usage: ./tasks.sh <task|all_deps>"
fi
