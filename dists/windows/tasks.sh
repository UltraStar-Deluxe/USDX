#!/usr/bin/env bash

set -e

if ! tput setaf 1 || ! tput bold || ! tput sgr0 ; then
	tput() { return 0 ; }
fi >/dev/null 2>/dev/null

root=$(pwd)

SRC="$root/deps"
mkdir -p "$SRC"
export SHELL=/bin/bash
ARCH=$(uname -m)
export PREFIX="$root/prefix/$ARCH"
mkdir -p "$PREFIX"
export PATH="$PREFIX/bin:$PATH"
PKG_CONFIG_PATH="$PREFIX/lib/pkgconfig"

DLL_DIR=$root/../../game
export CC=gcc
export CXX=G++
export CPPFLAGS="-I$PREFIX/include"
export CFLAGS="-O2 -gdwarf-2 -gas-loc-support $CPPFLAGS"
export CXXFLAGS="$CFLAGS"

CMAKE_DEFAULT_OPTIONS=(
"-DCMAKE_BUILD_TYPE=None"
"-DCMAKE_INSTALL_PREFIX=$PREFIX"
"-DCMAKE_PREFIX_PATH=$PREFIX"
"-DCMAKE_C_FLAGS=$CFLAGS"
"-DCMAKE_CXX_FLAGS=$CXXFLAGS"
"-DBUILD_SHARED_LIBS=ON"
"-Wno-author"
)

# multicore compilation
makearg="-j$(nproc)"

declare -a deps

# Dependencies
deps+=('SDL2,https://www.libsdl.org/release/SDL2-2.32.10.tar.gz,5f5993c530f084535c65a6879e9b26ad441169b3e25d789d83287040a9ca5165')
deps+=('SDL2_image,https://www.libsdl.org/projects/SDL_image/release/SDL2_image-2.8.12.tar.gz,393f5efb50536ec13ca4f4affb69cc9966d3c3f969e6c5e701faddf9f9785381')
deps+=('sqlite,https://sqlite.org/2026/sqlite-autoconf-3530100.tar.gz,83e6b2020a034e9a7ad4a72feea59e1ad52f162e09cbd26735a3ffb98359fc4f')
deps+=('freetype,https://download-mirror.savannah.gnu.org/releases/freetype/freetype-2.14.3.tar.xz,36bc4f1cc413335368ee656c42afca65c5a3987e8768cc28cf11ba775e785a5f')
deps+=('dav1d,https://downloads.videolan.org/pub/videolan/dav1d/1.5.4/dav1d-1.5.4.tar.xz,686616b7c69eb88d44459391ab25cac13b6647a3b288835c5784e71c1514a5c5')
deps+=('ffmpeg,https://ffmpeg.org/releases/ffmpeg-9.0.2.tar.xz,8c3850283eb25fa026482078a04051e0be17347b09ef81a0849bec15a96e002e')
deps+=('lua,https://www.lua.org/ftp/lua-5.5.1.tar.gz,1c4b4068d67061f2a2231ad2b5422e77acea1487ea9890f6320af614f4373dce')
deps+=('libjpeg-turbo,https://github.com/libjpeg-turbo/libjpeg-turbo/releases/download/3.2.0/libjpeg-turbo-3.2.0.tar.gz,6f30092cef9fb839779646608f4ee14ae3cbac989c47fa05e841b0841f09878e')
deps+=('libpng,https://download.sourceforge.net/libpng/libpng-1.6.58.tar.xz,28eb403f51f0f7405249132cecfe82ea5c0ef97f1b32c5a65828814ae0d34775')
deps+=('libwebp,https://storage.googleapis.com/downloads.webmproject.org/releases/webp/libwebp-1.6.0.tar.gz,e4ab7009bf0629fd11982d4c2aa83964cf244cffba7347ecd39019a9e38c4564')
deps+=('libtiff,https://download.osgeo.org/libtiff/tiff-4.7.1.tar.xz,b92017489bdc1db3a4c97191aa4b75366673cb746de0dce5d7a749d5954681ba')
deps+=('bass,https://www.un4seen.com/files/bass24.zip,3a03ec9a33d0f4f9d167660da51c8bb1432e8977496995455ab137277d69636e')
PROJECTM_VERSION=4.1.6
deps+=('projectm',https://github.com/projectM-visualizer/projectm/archive/4d2849333b63235a6af4d1f02508a97529d96dc7.tar.gz,218e1e5042ae4caaf7b17497722e9a04b4844444a6e2844f58ddce0c805043ea)
deps+=('projectm-eval,https://github.com/projectM-visualizer/projectm-eval/archive/refs/tags/v1.0.6.tar.gz,d663f7fc2c6962e29d5f3977f22c9a988e01638911e0efabea6b7ed4dcb0a8ce')
deps+=('opencv,https://github.com/opencv/opencv/archive/refs/tags/4.13.0.tar.gz,1d40ca017ea51c533cf9fd5cbde5b5fe7ae248291ddf2af99d4c17cf8e13017d')

# DLL Names
ffmpeg_DLLs="avcodec avformat avutil swresample swscale"
libpng_DLLs=libpng
dav1d_DLLs=libdav1d
libtiff_DLLs=libtiff
libjpeg_turbo_DLLs=libjpeg
libwebp_DLLs="libwebp libsharpyuv"
SDL2_DLLs=SDL2
SDL2_image_DLLs=SDL2_image
lua_DLLs=lua
sqlite_DLLs=libsqlite3-0:sqlite3
freetype_DLLs=libfreetype
opencv_DLLs="libopencv_core libopencv_imgcodecs libopencv_imgproc libopencv_videoio"
projectm_DLLs=libprojectM-4:projectM-4

clean_prefix() {
	tput setaf 2 && tput bold
	echo "==> Cleaning prefix"
	tput sgr0
	rm -rf "$PREFIX"
	mkdir -pv $PREFIX/{etc,bin,include,lib}
}

start_build() {
	[ -d "$SRC/$1" ] || return 1
	du -s "$PREFIX"
	date
	cd "$SRC/$1"
	if [ $# -gt 1 ] ; then
		shift
	fi
	tput setaf 2 && tput bold
	echo "==> Building $*"
	tput sgr0
	hash -r
}

copy_msys2_dlls() {
	local MSYS2_DLLS=
	local DLL_PATH
	mkdir -p $PREFIX/bin/msys2
	shopt -s nullglob
	for i in $1; do
		local DLL_NAME=${i%%:*}
		local DLL_PATH=($PREFIX/bin/$DLL_NAME*.dll)
		if [ ${#DLL_PATH[@]} -eq 0 ]; then
			continue
		fi
		MSYS2_DLLS="$MSYS2_DLLS $(ldd "${DLL_PATH[0]}" | grep $MINGW_PREFIX | sed 's/.* => //' | sed 's/ \(.*\)//' | xargs)"
	done
	shopt -u nullglob

	for i in $MSYS2_DLLS; do
		cp -n $i $PREFIX/bin/msys2
	done
}

task_libpng() {
	start_build libpng || return 0
	cmake -S . -B build \
		"${CMAKE_DEFAULT_OPTIONS[@]}" \
		-DPNG_EXECUTABLES=OFF \
		-DPNG_TOOLS=OFF \
		-DPNG_SHARED=ON \
		-DPNG_STATIC=OFF
	cmake --build build --target install
	rm -rf build
	copy_msys2_dlls "$libpng_DLLs"
}

task_libjpeg_turbo() {
	start_build libjpeg-turbo || return 0
	cmake -S . -B build \
		"${CMAKE_DEFAULT_OPTIONS[@]}" \
		-DCMAKE_CACHEFILE_DIR="$(pwd)/out" \
		-DWITH_TURBOJPEG=OFF \
		-DWITH_ARITH_ENC=OFF \
		-DENABLE_STATIC=OFF \
		-DWITH_TOOLS=OFF \
		-DWITH_TESTS=OFF \
		-DCMAKE_POLICY_VERSION_MINIMUM=3.5
	cmake --build build --target install --parallel
	rm -rf build
	copy_msys2_dlls "$libjpeg_turbo_DLLs"
}

task_libtiff() {
	start_build libtiff || return 0
	cmake -S . -B build \
		"${CMAKE_DEFAULT_OPTIONS[@]}" \
		-Dtiff-tools=OFF \
		-Dtiff-test=OFF \
		-Dtiff-contrib=OFF \
		-Dtiff-tools-unsupported=OFF \
		-Dtiff-docs=OFF \
		-Dlzma=OFF \
		-Dwebp=OFF \
		-Dlibdeflate=OFF \
		-Djbig=OFF \
		-Dlerc=OFF \
		-Dzstd=OFF
	cmake --build build --target install --parallel
	rm -rf build
	copy_msys2_dlls "$libtiff_DLLs"
}

task_libwebp() {
	start_build libwebp || return 0
	cmake -S . -B build \
		"${CMAKE_DEFAULT_OPTIONS[@]}" \
		-DWEBP_BUILD_ANIM_UTILS=OFF \
		-DWEBP_BUILD_CWEBP=OFF \
		-DWEBP_BUILD_DWEBP=OFF \
		-DWEBP_BUILD_GIF2WEBP=OFF \
		-DWEBP_BUILD_IMG2WEBP=OFF \
		-DWEBP_BUILD_VWEBP=OFF \
		-DWEBP_BUILD_WEBPINFO=OFF \
		-DWEBP_BUILD_LIBWEBPMUX=OFF \
		-DWEBP_BUILD_WEBPMUX=OFF \
		-DWEBP_BUILD_EXTRAS=OFF
	cmake --build build --target install --parallel
	rm -rf build
	copy_msys2_dlls "$libwebp_DLLs"
}

task_sdl2() {
	start_build SDL2 || return 0
	cmake -S . -B build \
		"${CMAKE_DEFAULT_OPTIONS[@]}" \
		-DSDL2_DISABLE_SDL2MAIN=ON \
		-DSDL_TEST=OFF
	cmake --build build --target install --parallel
	rm -rf build
	copy_msys2_dlls "$SDL2_DLLs"
}

task_sdl2_image() {
	start_build SDL2_image || return 0
	patch -p1 < $root/sdl2_image-1-no-webp-anim.patch
	cmake -S . -B build \
		"${CMAKE_DEFAULT_OPTIONS[@]}" \
		-DSDL2IMAGE_BACKEND_STB=OFF \
		-DSDL2IMAGE_AVIF_SHARED=OFF \
		-DSDL2IMAGE_JPG_SHARED=OFF \
		-DSDL2IMAGE_PNG_SHARED=OFF \
		-DSDL2IMAGE_TIF_SHARED=OFF \
		-DSDL2IMAGE_WEBP_SHARED=OFF \
		-DSDL2IMAGE_AVIF=OFF \
		-DSDL2IMAGE_SAMPLES=OFF
	cmake --build build --target install --parallel
	rm -rf build
	copy_msys2_dlls "$SDL2_image_DLLs"
}

task_sqlite() {
	start_build sqlite SQLite || return 0
	./configure --prefix="$PREFIX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH" CC="$CC" CXX="$CXX" \
		--disable-static
	make $makearg
	make install
	make distclean
	copy_msys2_dlls "$sqlite_DLLs"
}

task_freetype() {
	start_build freetype || return 0
	cmake -S . -B build \
		"${CMAKE_DEFAULT_OPTIONS[@]}" \
		-DFT_DISABLE_HARFBUZZ=ON \
		-DFT_DISABLE_BROTLI=ON \
		-DFT_DISABLE_PNG=ON \
		-DFT_DISABLE_BZIP2=ON \
		-DFT_DISABLE_ZLIB=ON
	cmake --build build --target install --parallel
	rm -rf build
	copy_msys2_dlls "$freetype_DLLs"
}

task_dav1d() {
	start_build dav1d || return 0
	CC="$CC" CXX="$CXX" meson setup --prefix "$PREFIX" --libdir=lib -Denable_tools=false -Denable_tests=false build
	ninja -C build $makearg
	ninja -C build install
	rm -Rf build
	copy_msys2_dlls "$dav1d_DLLs"
}

task_ffmpeg() {
	start_build ffmpeg FFmpeg || return 0
	./configure \
		--prefix=$PREFIX \
		--disable-static \
		--enable-shared \
		--disable-avfilter \
		--disable-avdevice \
		--x86asmexe=nasm \
		--enable-debug \
		--disable-stripping \
		--disable-pthreads \
		--enable-w32threads \
		--disable-doc \
		--enable-libdav1d \
		--enable-version3 \
		--extra-libs=-mconsole \
		--disable-programs \
		--disable-doc \
		--disable-encoders \
		--disable-iconv \
		--disable-xlib \
		--disable-libxcb \
		--disable-libxcb-shm \
		--disable-libx264 \
		--disable-libx265 \
		--disable-vaapi \
		--disable-network \
		--disable-indevs \
		--disable-outdevs \
		--disable-muxers \
		--disable-bsfs \
		--disable-filters \
		--disable-protocols \
		--disable-lzma \
		--disable-bzlib \
		--extra-ldflags=-fstack-protector
	make $makearg
	make install
	make distclean
	copy_msys2_dlls "$ffmpeg_DLLs"
}

task_lua() {
	start_build lua Lua || return 0
	eval `make pc | grep ^version=`
	IFS=. read -r lua_major lua_minor lua_patch <<< "$version"
	gcc $CFLAGS -DLUA_BUILD_AS_DLL -c src/*.c
	rm lua.o luac.o
	gcc -shared -o $PREFIX/bin/lua$lua_major$lua_minor.dll *.o -Wl,--out-implib,$PREFIX/lib/liblua$lua_major$lua_minor.dll.a
	mkdir -p $PREFIX/lib/pkgconfig
	make -s INSTALL_TOP="$PREFIX" pc > "$PREFIX/lib/pkgconfig/lua$lua_major.$lua_minor.pc"
	cp src/lua.h src/luaconf.h $PREFIX/include
	echo -e "Name: Lua
Description: An Extensible Language
Version: ${version}
Requires:
Libs: -L\${libdir} -llua$lua_major$lua_minor
Cflags: -I\${includedir}
" >> "$PREFIX/lib/pkgconfig/lua$lua_major.$lua_minor.pc"
	rm *.o
	copy_msys2_dlls "$lua_DLLs"
}

task_bass() {
	cp -f $root/deps/bass/x64/bass.dll $PREFIX/bin
}

task_opencv() {
	start_build opencv OpenCV || return 0
	# Note: OpenCV does not support CMake build type None
	cmake -S . -B build \
		"${CMAKE_DEFAULT_OPTIONS[@]}" \
		-DCMAKE_BUILD_TYPE=Release \
		-DWITH_QT=OFF \
		-DWITH_OPENGL=ON \
		-DWITH_FFMPEG=OFF \
		-DWITH_GSTREAMER=OFF \
		-DWITH_GTK=OFF \
		-DWITH_VIDEOINPUT=ON \
		-DWITH_XINE=OFF \
		-DWITH_LAPACK=OFF \
		-DWITH_EIGEN=OFF \
		-DWITH_OPENBLAS=OFF \
		-DWITH_IPP=OFF \
		-DBUILD_opencv_apps=OFF \
		-DBUILD_opencv_calib3d=OFF \
		-DBUILD_opencv_dnn=OFF \
		-DBUILD_DOCS=OFF \
		-DBUILD_EXAMPLES=OFF \
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
		-DBUILD_PACKAGE=OFF \
		-DBUILD_PERF_TESTS=OFF \
		-DBUILD_opencv_stitching=OFF \
		-DBUILD_TESTS=OFF \
		-DBUILD_opencv_ts=OFF \
		-DBUILD_opencv_video=OFF \
		-DBUILD_WITH_DEBUG_INFO=OFF \
		-DBUILD_FAT_JAVA_LIB=OFF \
		-DCV_TRACE=OFF \
		-DBUILD_ZLIB=OFF \
		-DBUILD_TIFF=OFF \
		-DBUILD_JASPER=OFF \
		-DBUILD_JPEG=OFF \
		-DBUILD_WEBP=OFF \
		-DBUILD_PROTOBUF=OFF \
		-DBUILD_PNG=OFF \
		-DBUILD_OPENEXR=OFF \
		-DWITH_JPEG=ON \
		-DWITH_PNG=OFF \
		-DWITH_AVIF=OFF \
		-DWITH_TIFF=OFF \
		-DWITH_WEBP=OFF \
		-DWITH_JASPER=OFF \
		-DWITH_OPENJPEG=OFF \
		-DWITH_OPENEXR=OFF \
		-DWITH_PROTOBUF=OFF \
		-DWITH_ZLIB=OFF \
		-DWITH_ADE=OFF \
		-DWITH_QUIRC=OFF \
		-DWITH_GDAL=OFF \
		-DWITH_GDCM=OFF \
		-DWITH_IMGCODEC_HDR=OFF \
		-DWITH_IMGCODEC_SUNRASTER=OFF \
		-DWITH_IMGCODEC_PXM=OFF \
		-DWITH_IMGCODEC_PFM=OFF \
		-DCMAKE_VERBOSE=ON \
		-DOPENCV_GENERATE_PKGCONFIG=ON \
		-DOPENCV_BIN_INSTALL_PATH=bin \
		-DOPENCV_LIB_INSTALL_PATH=lib \
		-DOPENCV_CONFIG_INSTALL_PATH=lib/cmake/OpenCV
	cmake --build build --target install --parallel
	rm -rf build
	copy_msys2_dlls "$opencv_DLLs"
}

task_projectm() {
	start_build projectm ProjectM || return 0
	cp -r $root/deps/projectm-eval/ $root/deps/projectm/vendor
	cmake -S . -B build \
		"${CMAKE_DEFAULT_OPTIONS[@]}" \
		-DENABLE_PLAYLIST=OFF
	cmake --build build --target install --parallel
	echo -e "prefix=$PREFIX
libdir=\${prefix}/lib
includedir=\${prefix}/include
Name: ProjectM
Description: projectM Music Visualizer
Version: $PROJECTM_VERSION
Libs: -L\${libdir} -lprojectM-4
Cflags: -I\${includedir}
" > $PREFIX/lib/pkgconfig/projectM-4.pc
	rm -rf build
	copy_msys2_dlls "$projectm_DLLs"
}

add_dll() {
	local dll="$1"
	local base=
	shopt -s nullglob
	local DLL_NAME=${1%%:*}
	local DLL_PATH=($PREFIX/bin/$DLL_NAME*.dll)
	shopt -u nullglob
	[ "${#DLL_PATH[@]}" -gt 0 ] || return 0
	dll="$(basename ${DLL_PATH[0]})"
	dll="${dll%.*}"
	base="$(basename ${dll##*:})"
	if [[ "$1" == *":"* ]]; then
		base="${1##*:}"
	else
		base=$dll
	fi
	dll=$PREFIX/bin/$dll.dll
	echo "Installing $base.dll"
	objcopy --only-keep-debug "$dll" "$DLL_DIR/$base.debug"
	(
		cd "$DLL_DIR"
		set -- `md5sum "$base.debug"`
		k=$base-$1
		mv "$base.debug" "$k.debug"
		objcopy -S --add-gnu-debuglink="$k.debug" "$dll" "$base.dll"
		chmod a-x "$base.dll" "$k.debug"
	)
}

task_install() {
	rm -f $DLL_DIR/*.debug $DLL_DIR/*.dll
	for i in $ffmpeg_DLLs $dav1d_DLLs $libjpeg_turbo_DLLs $libpng_DLLs $libtiff_DLLs $libwebp_DLLs $SDL2_DLLs $SDL2_image_DLLs $lua_DLLs $sqlite_DLLs $freetype_DLLs $projectm_DLLs $opencv_DLLs; do
		add_dll "$i"
	done
	for i in $PREFIX/bin/bass.dll $PREFIX/bin/msys2/*.dll; do
		echo "Installing $(basename $i)"
		install $i $DLL_DIR
	done
}

task_dl() {
	for i in "${deps[@]}"; do
		IFS=',' read -a dep <<< "$i"
		name="${dep[0]}"
		extract=yes
		case "$name" in //*)
			name=${name#//}
			extract=no
		esac
		url="${dep[1]}"
		hashA="${dep[2]}"
		filename="${url%/download}"
		bname="$(basename "$filename")"
		case "$filename" in
		*.zip)
			mkdir -p "deps/dl"
			hashB="$(sha256sum "deps/dl/$bname" 2> /dev/null | awk '{print $1}')"
			if [ ! -f "deps/dl/$bname" ] || [ "$hashA" != "$hashB" ]; then
				echo "Downloading $name from $url"
				curl -s -S -L "$url" -o "deps/dl/$bname"
				hashB="$(sha256sum "deps/dl/$bname" 2> /dev/null | awk '{print $1}')"
				if [ "$hashA" != "$hashB" ]; then
					echo "Hashes doesn't match!" "$hashA" "$hashB"
					exit 1
				fi
			fi
			[ $extract = yes ] || continue
			mkdir -p "deps/$name.tmp"
			echo "Extracting $name"
			unzip -q "deps/dl/$bname" -d "deps/$name.tmp"
			# rm "deps/dl/$bname"
			rm -rf "deps/$name"
			mkdir -p "deps/$name"
			mv "deps/$name.tmp/"* "deps/$name"
			rmdir "deps/$name.tmp"
			;;

		*.tar|*.tar.gz|*.tar.xz|*.tar.bz2|*.tgz)
			case "$filename" in
			*xz)	compressor=J ;;
			*bz2)	compressor=j ;;
			*gz)	compressor=z ;;
			*tgz)	compressor=z ;;
			*)	compressor= ;;
			esac
			mkdir -p "deps/dl"
			hashB="$(sha256sum "deps/dl/$bname" 2> /dev/null | awk '{print $1}')"
			if [ ! -f "deps/dl/$bname" ] || [ "$hashA" != "$hashB" ]; then
				echo "Downloading $name from $url"
				curl -s -S -L "$url" -o "deps/dl/$bname"
				hashB="$(sha256sum "deps/dl/$bname" 2> /dev/null | awk '{print $1}')"
				if [ "$hashA" != "$hashB" ]; then
					echo "Hashes doesn't match!" "$hashA" "$hashB"
					exit 1
				fi
			fi
			[ $extract = yes ] || continue
			echo "Extracting $name"
			rm -rf "deps/$name"
			mkdir -p "deps/$name"
			tar -x${compressor}f "deps/dl/$bname" -C "deps/$name" --strip-components=1 --warning=no-unknown-keyword
			;;

		*)
			mkdir -p deps
			hashB="$(sha256sum "deps/$name" 2> /dev/null | awk '{print $1}')"
			if [ ! -f "deps/$name" ] || [ "$hashA" != "$hashB" ]; then
				echo "Downloading $name from $url"
				curl -s -S -L "$url" -o "deps/$name"
				hashB="$(sha256sum "deps/$name" 2> /dev/null | awk '{print $1}')"
				if [ "$hashA" != "$hashB" ]; then
					echo "Hashes doesn't match!" "$hashA" "$hashB"
					exit 1
				fi
			fi
			echo "Extracting $name"
			chmod +x "deps/$name"
			;;
		esac
	done
}
if [ "$1" == "all_deps" ]; then
	tput setaf 2 && tput bold
	echo "==> Building all dependencies"
	tput sgr0

	clean_prefix
	echo

	task_bass
	echo
	task_libjpeg_turbo
	echo
	task_libpng
	echo
	task_libwebp
	echo
	task_libtiff
	echo
	task_sdl2
	echo
	task_sdl2_image
	echo
	task_sqlite
	echo
	task_dav1d
	echo
	task_ffmpeg
	echo
	task_lua
	echo
	task_freetype
	echo
	task_opencv
	echo
	task_projectm
	echo

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