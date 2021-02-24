#!/usr/bin/env bash

set -e

declare -a deps
deps+=('openssl,https://www.openssl.org/source/openssl-1.1.1j.tar.gz,04c340b086828eecff9df06dceff196790bb9268')
deps+=('cmake,https://github.com/Kitware/CMake/releases/download/v3.19.5/cmake-3.19.5.tar.gz,b8b9b9ccd54ceb35dd0e9a4348696d8a127fcd88')
deps+=('python,https://www.python.org/ftp/python/3.9.2/Python-3.9.2.tar.xz,110ca5bca7989f9558a54ee6762e6774a4b9644a')
deps+=('meson,https://github.com/mesonbuild/meson/releases/download/0.57.1/meson-0.57.1.tar.gz,eb87a41d4265aec1223c446c344455b674d4538c')
deps+=('ninja,https://github.com/ninja-build/ninja/archive/v1.10.2.tar.gz,8d2e8c1c070c27fb9dc46b4a6345bbb1de7ccbaf')
deps+=('wayland,https://wayland.freedesktop.org/releases/wayland-1.19.0.tar.xz,9ae0a89cfe6798250b19c72a987bda734d269060')
deps+=('wayland-protocols,https://wayland.freedesktop.org/releases/wayland-protocols-1.20.tar.xz,e78c739a3a85477ed524b81e8bb75efe7f8bf4df')
deps+=('SDL2,https://www.libsdl.org/release/SDL2-2.0.14.tar.gz,212b17d988c417a1a905ab09c50d1845cc48ddb7')
deps+=('SDL2_image,https://www.libsdl.org/projects/SDL_image/release/SDL2_image-2.0.5.tar.gz,c0aed07994f670a3758f6b8b93d9034a58df5781')
deps+=('sqlite,https://www.sqlite.org/2021/sqlite-autoconf-3340100.tar.gz,c20286e11fe5c2e3712ce74890e1692417de6890')
deps+=('nasm,https://www.nasm.us/pub/nasm/releasebuilds/2.15.05/nasm-2.15.05.tar.xz,d338409a03fc6d1508102881a675a00275fcb879')
deps+=('dav1d,https://downloads.videolan.org/pub/videolan/dav1d/0.8.1/dav1d-0.8.1.tar.xz,508f2314488c6e0f7927a56b2554e760abcd12cd')
deps+=('ffmpeg,https://www.ffmpeg.org/releases/ffmpeg-4.2.4.tar.xz,eca62adfdda5cbb5fc3af9dd236c058c046201a1')
deps+=('portmidi,https://sourceforge.net/projects/portmedia/files/portmidi/217/portmidi-src-217.zip,f45bf4e247c0d7617deacd6a65d23d9fddae6117')
deps+=('portmidi-debian,http://http.debian.net/debian/pool/main/p/portmidi/portmidi_217-6.debian.tar.xz,02e4c6dcfbd35a75913de2acd39be8f0cfd0b244')
deps+=('lua,https://www.lua.org/ftp/lua-5.3.6.tar.gz,f27d20d6c81292149bc4308525a9d6733c224fa5')
deps+=('libjpeg-turbo,https://download.sourceforge.net/libjpeg-turbo/libjpeg-turbo-2.0.6.tar.gz,5406c7676d7df89fb4da791ad5af51202910fb25')
deps+=('libpng,https://download.sourceforge.net/libpng/libpng-1.6.37.tar.xz,3ab93fabbf4c27e1c4724371df408d9a1bd3f656')
# deps+=('libcwrap.h,https://raw.githubusercontent.com/wheybags/glibc_version_header/master/version_headers/force_link_glibc_2.10.2.h,aff0c46cf3005fe15c49688e74df62a9988855a5')
deps+=('patchelf,https://github.com/NixOS/patchelf/releases/download/0.12/patchelf-0.12.tar.bz2,58cf949052cc63cdd52e9ab347dcafc6c6c36f33')
deps+=('opencv,https://github.com/opencv/opencv/archive/4.5.1.tar.gz,3e464886dc9907e879e2fc7097364427e96861f6')
deps+=('projectm,https://github.com/projectM-visualizer/projectm/releases/download/v2.2.1/projectM-2.2.1.tar.gz,bfd0cb09797384a814c3585b7b0369fc1c8b04fe')
# if [ -f /.dockerenv ]; then
# 	deps+=('fpc-x86_64,https://sourceforge.net/projects/freepascal/files/Linux/3.0.4/fpc-3.0.4.x86_64-linux.tar,0720e428eaea423423e1b76a7267d6749c3399f4')
# 	deps+=('fpc-i686,https://sourceforge.net/projects/freepascal/files/Linux/3.0.4/fpc-3.0.4.i386-linux.tar,0a51364bd1a37f1e776df5357ab5bfca8cc7ddeb')
# fi

for i in "${deps[@]}"; do
	IFS=',' read -a dep <<< "$i"
	name="${dep[0]}"
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

git_source AppImageKit https://github.com/AppImage/AppImageKit 12
