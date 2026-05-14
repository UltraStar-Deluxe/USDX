#!/usr/bin/env bash

set -e

declare -a deps

older() {
	local minver=$1
	shift
	set -- `"$@" | tr '\n' ' ' | sed 's/\([^0-9[:space:]][0-9]*\|[[:space:]]\)*\(\<[0-9\.]\+\).*/\2/;s/\./ /g'` 0 0 0
	local toolver=$(($1 * 1000000 + $2 * 1000 + $3))
	set -- `echo $minver | sed 's/\./ /g'` 0 0 0
	local minver=$(($1 * 1000000 + $2 * 1000 + $3))
	[ $toolver -lt $minver ]
}

needossl=no

if older 3.5.1 cmake --version && older 3.5.1 cmake3 --version ; then
	deps+=('cmake,https://github.com/Kitware/CMake/releases/download/v3.25.2/cmake-3.25.2.tar.gz,2f444bb9951aad09452b692d8b3b9e33309e1641')
	needossl=yes
fi

if older 0.56 meson --version ; then
	deps+=('meson,https://github.com/mesonbuild/meson/releases/download/1.0.1/meson-1.0.1.tar.gz,a2d102eb6a37307c9b67283e9764ed57cf286223')

	if older 3.7 python3 --version ; then
		deps+=('python,https://www.python.org/ftp/python/3.11.2/Python-3.11.2.tar.xz,ae1c199ecb7a969588b15354e19e7b60cb65d1b9')
		needossl=yes
	fi

	if older 1.8.2 ninja --version ; then
		deps+=('ninja,https://github.com/ninja-build/ninja/archive/v1.11.1.tar.gz,938723cdfc7a6f7c8f84c83b9a2cecdf1e5e1ad3')
	fi
fi

if [ $needossl = yes ] ; then
	if older 1.0.2 pkg-config --modversion libssl ; then
		deps+=('openssl,https://www.openssl.org/source/openssl-1.1.1t.tar.gz,a06b067b7e3bd6a2cb52a06f087ff13346ce7360')
	fi
fi

if older 1.8 wayland-scanner --version ; then
	deps+=('wayland,https://gitlab.freedesktop.org/wayland/wayland/-/releases/1.21.91/downloads/wayland-1.21.91.tar.xz,d4f5b5e2453d45902016138dae0605cae096e00b')
fi

deps+=('pulseaudio,https://www.freedesktop.org/software/pulseaudio/releases/pulseaudio-16.1.tar.xz,7bf3845b522a1da263b6b84a0bc5aa761e49bc87')
deps+=('pipewire,https://gitlab.freedesktop.org/pipewire/pipewire/-/archive/0.3.66/pipewire-0.3.66.tar.bz2,20c58dd867c33a7d111df69d7ec8d31c45b92a0b')
deps+=('wayland-protocols,https://gitlab.freedesktop.org/wayland/wayland-protocols/-/releases/1.31/downloads/wayland-protocols-1.31.tar.xz,5a84628630598027fab1708f822fc399d9e70b02')
deps+=('xkbcommon,https://github.com/xkbcommon/libxkbcommon/archive/refs/tags/xkbcommon-1.13.1.tar.gz,f487d1e7f5e362b8971e23e354d9eb4bdc51e453')
deps+=('SDL2,https://www.libsdl.org/release/SDL2-2.32.10.tar.gz,ce98fa93e31836a751feca374ab28a0770b63c16')
deps+=('SDL2_image,https://www.libsdl.org/projects/SDL_image/release/SDL2_image-2.8.12.tar.gz,d7075e3fe8c1780c5d239c69371ab19089209ea0')
deps+=('sqlite,https://sqlite.org/2026/sqlite-autoconf-3530100.tar.gz,f5828a709d18bb4857cec8ddb4298c47a9143892')

if older 2.15 nasm --version ; then
	deps+=('nasm,https://www.nasm.us/pub/nasm/releasebuilds/3.01/nasm-3.01.tar.xz,f0531dfe9728192fe190cc4932df44a26f1bde7c')
fi

deps+=('dav1d,https://downloads.videolan.org/pub/videolan/dav1d/1.5.3/dav1d-1.5.3.tar.xz,a547b3830b3469ef9e6a33a2fa62b714eb1f611c')
deps+=('ffmpeg,https://ffmpeg.org/releases/ffmpeg-8.1.1.tar.xz,0b8318579556aaa736096ceb1e8333d2260b63bf')
deps+=('portmidi,https://sourceforge.net/projects/portmedia/files/portmidi/217/portmidi-src-217.zip,f45bf4e247c0d7617deacd6a65d23d9fddae6117')
deps+=('portmidi-debian,http://http.debian.net/debian/pool/main/p/portmidi/portmidi_217-6.debian.tar.xz,02e4c6dcfbd35a75913de2acd39be8f0cfd0b244')
deps+=('lua,https://www.lua.org/ftp/lua-5.4.7.tar.gz,29b54f97dab8631f52ee21a44871622eaefbe235')
deps+=('libjpeg-turbo,https://download.sourceforge.net/libjpeg-turbo/libjpeg-turbo-2.1.5.1.tar.gz,3ec9f6a19781a583285d93c2c4653f3dbe845fcc')
deps+=('libpng,https://download.sourceforge.net/libpng/libpng-1.6.43.tar.xz,ad9f087b73acf01e2c252920810b005ee69d3e0e')
deps+=('libwebp,https://storage.googleapis.com/downloads.webmproject.org/releases/webp/libwebp-1.5.0.tar.gz,b21aa842136dc59a72a38776a5aa73f4d0b00ac5')
# deps+=('libcwrap.h,https://raw.githubusercontent.com/wheybags/glibc_version_header/master/version_headers/force_link_glibc_2.10.2.h,aff0c46cf3005fe15c49688e74df62a9988855a5')

if ! patchelf 2>&1 | grep -q syntax ; then
	deps+=('patchelf,https://github.com/NixOS/patchelf/releases/download/0.13.1/patchelf-0.13.1.tar.bz2,5d9c1690c0fbe70c312f43d597e04b6c1eeffc60')
fi

deps+=('opencv,https://github.com/opencv/opencv/archive/refs/tags/4.10.0.tar.gz,6bd566dc4297023914137677aa96fdebee89f5bd')
deps+=('projectm,https://github.com/projectM-visualizer/projectm/releases/download/v2.2.1/projectM-2.2.1.tar.gz,bfd0cb09797384a814c3585b7b0369fc1c8b04fe')
# if [ -f /.dockerenv ]; then
# 	deps+=('fpc-x86_64,https://sourceforge.net/projects/freepascal/files/Linux/3.0.4/fpc-3.0.4.x86_64-linux.tar,0720e428eaea423423e1b76a7267d6749c3399f4')
# 	deps+=('fpc-i686,https://sourceforge.net/projects/freepascal/files/Linux/3.0.4/fpc-3.0.4.i386-linux.tar,0a51364bd1a37f1e776df5357ab5bfca8cc7ddeb')
# fi

if ! desktop-file-validate -h >/dev/null ; then
	deps+=('desktop-file-utils,https://www.freedesktop.org/software/desktop-file-utils/releases/desktop-file-utils-0.26.tar.xz,9fd94cb7de302163015fcbc0e157c61323b1205d')
fi

deps+=('//xz,https://download.sourceforge.net/lzmautils/xz-5.2.3.tar.gz,529638eec3597e429cc54c74551ac0a89169e841')

if ! pkg-config --exists libarchive ; then
	deps+=('//libarchive,https://www.libarchive.org/downloads/libarchive-3.3.1.tar.gz,d5616f81804aba92547629c08a3ccff86c2844ae')
fi

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
		hashB="$(sha1sum "deps/dl/$bname" 2> /dev/null | awk '{print $1}')"
		if [ ! -f "deps/dl/$bname" ] || [ "$hashA" != "$hashB" ]; then
			echo "Downloading $name from $url"
			curl -s -S -L "$url" -o "deps/dl/$bname"
			hashB="$(sha1sum "deps/dl/$bname" 2> /dev/null | awk '{print $1}')"
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
		hashB="$(sha1sum "deps/dl/$bname" 2> /dev/null | awk '{print $1}')"
		if [ ! -f "deps/dl/$bname" ] || [ "$hashA" != "$hashB" ]; then
			echo "Downloading $name from $url"
			curl -s -S -L "$url" -o "deps/dl/$bname"
			hashB="$(sha1sum "deps/dl/$bname" 2> /dev/null | awk '{print $1}')"
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
		hashB="$(sha1sum "deps/$name" 2> /dev/null | awk '{print $1}')"
		if [ ! -f "deps/$name" ] || [ "$hashA" != "$hashB" ]; then
			echo "Downloading $name from $url"
			curl -s -S -L "$url" -o "deps/$name"
			hashB="$(sha1sum "deps/$name" 2> /dev/null | awk '{print $1}')"
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

git_source() {
	if ! [ -d deps/$1 ] ; then
		git clone -b $3 --depth 1 --recursive $2 deps/$1
	else
		(
			cd deps/$1
			git checkout -f $3
			git submodule update --init -f --checkout --recursive
			find -name .git | while read a ; do
				export GIT_DIR=$a
				export GIT_WORK_TREE=${a%/.git}
				echo cleaning $GIT_WORK_TREE
				git clean -d -f -x
			done
		)
	fi
}

git_source AppImageKit https://github.com/AppImage/AppImageKit 13
