#!/usr/bin/env bash

set -e

declare -a deps
deps+=('SDL2,https://www.libsdl.org/release/SDL2-2.0.9.tar.gz,4354c6baad9a48486182656a7506abfb63e9bff5')
deps+=('SDL2_image,https://www.libsdl.org/projects/SDL_image/release/SDL2_image-2.0.4.tar.gz,aed0c6e5feb5ae933410c150d33c319000ea4cfd')
deps+=('sqlite,https://www.sqlite.org/2018/sqlite-autoconf-3260000.tar.gz,9af2df1a6da5db6e2ecf3f463625f16740e036e9')
deps+=('yasm,http://www.tortall.net/projects/yasm/releases/yasm-1.3.0.tar.gz,b7574e9f0826bedef975d64d3825f75fbaeef55e')
deps+=('ffmpeg,https://www.ffmpeg.org/releases/ffmpeg-4.1.tar.xz,40bb9002df044514723e9cca7f0a049eee88fed8')
deps+=('portmidi,https://sourceforge.net/projects/portmedia/files/portmidi/217/portmidi-src-217.zip,f45bf4e247c0d7617deacd6a65d23d9fddae6117')
deps+=('portmidi-debian,http://http.debian.net/debian/pool/main/p/portmidi/portmidi_217-6.debian.tar.xz,02e4c6dcfbd35a75913de2acd39be8f0cfd0b244')
deps+=('lua,https://www.lua.org/ftp/lua-5.3.5.tar.gz,112eb10ff04d1b4c9898e121d6bdf54a81482447')
deps+=('libpng,https://download.sourceforge.net/libpng/libpng-1.6.36.tar.xz,aec9548c8319104226cc4c31d1f5e524f1b55295')
# deps+=('libcwrap.h,https://raw.githubusercontent.com/wheybags/glibc_version_header/master/version_headers/force_link_glibc_2.10.2.h,aff0c46cf3005fe15c49688e74df62a9988855a5')
deps+=('patchelf,https://github.com/NixOS/patchelf/archive/0.9.tar.gz,c068c60a67388fbf9267142516d3a8cd6ffc4397')
deps+=('opencv,https://github.com/opencv/opencv/archive/4.1.1.tar.gz,a7beeaada9b6c45a389b9aee391e82c092537819')
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
		tar -x${compressor}f "deps/dl/$bname" -C "deps/$name" --strip-components=1
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
