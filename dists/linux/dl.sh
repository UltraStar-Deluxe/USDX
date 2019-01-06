#!/usr/bin/env bash

set -e

declare -a deps
deps+=('SDL2,https://www.libsdl.org/release/SDL2-2.0.9.tar.gz')
deps+=('SDL2_image,https://www.libsdl.org/projects/SDL_image/release/SDL2_image-2.0.4.tar.gz')
deps+=('sqlite,https://www.sqlite.org/2018/sqlite-autoconf-3260000.tar.gz')
deps+=('yasm,http://www.tortall.net/projects/yasm/releases/yasm-1.3.0.tar.gz')
deps+=('ffmpeg,https://www.ffmpeg.org/releases/ffmpeg-4.1.tar.xz')
deps+=('portaudio,http://www.portaudio.com/archives/pa_stable_v190600_20161030.tgz')
deps+=('freetype,https://download.savannah.gnu.org/releases/freetype/freetype-2.9.1.tar.gz')
deps+=('libpng,https://download.sourceforge.net/libpng/libpng-1.6.36.tar.xz')
deps+=('portmidi,https://sourceforge.net/projects/portmedia/files/portmidi/217/portmidi-src-217.zip/download')
deps+=('portmidi-debian,http://http.debian.net/debian/pool/main/p/portmidi/portmidi_217-6.debian.tar.xz')
deps+=('zlib,https://zlib.net/zlib-1.2.11.tar.gz')
deps+=('libcwrap.h,https://raw.githubusercontent.com/wheybags/glibc_version_header/master/version_headers/force_link_glibc_2.10.2.h')

rm -rf deps

echo "Downloading dependencies"
for i in "${deps[@]}"; do
	IFS=',' read -a dep <<< "$i"
	name="${dep[0]}"
	url="${dep[1]}"
	echo "Downloading $url"
	filename="${url%/download}"
	case "$filename" in
	*.zip)
		mkdir -p deps
		curl --progress-bar -L "$url" -o deps/$name.zip
		mkdir -p "deps/$name.tmp"
		unzip -q deps/$name.zip -d "deps/$name.tmp"
		rm deps/$name.zip
		mv "deps/$name.tmp/"* "deps/$name"
		rmdir "deps/$name.tmp"
		;;

	*.tar|*.tar.gz|*.tar.xz|*.tar.bz2|*.tgz)
		case "$filename" in
		*xz)	compressor=J ;;
		*bz2)	compressor=j ;;
		*gz)	compressor=z ;;
		*)	compressor= ;;
		esac
		mkdir -p "deps/$name"
		curl --progress-bar -L "$url" | tar -x$compressor -C "deps/$name" --strip-components=1
		;;

	*)
		mkdir -p deps
		curl --progress-bar -L "$url" -o deps/$name
		chmod +x deps/$name
		;;
	esac
done
