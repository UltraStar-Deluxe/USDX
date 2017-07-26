#!/usr/bin/env bash

set -e

declare -a deps
deps+=('SDL2,https://www.libsdl.org/release/SDL2-2.0.5.tar.gz')
deps+=('SDL2_mixer,https://www.libsdl.org/projects/SDL_mixer/release/SDL2_mixer-2.0.1.tar.gz')
deps+=('SDL2_image,https://www.libsdl.org/projects/SDL_image/release/SDL2_image-2.0.1.tar.gz')
deps+=('SDL2_ttf,https://www.libsdl.org/projects/SDL_ttf/release/SDL2_ttf-2.0.14.tar.gz')
deps+=('SDL2_net,https://www.libsdl.org/projects/SDL_net/release/SDL2_net-2.0.1.tar.gz')
deps+=('SDL2_gfx,http://www.ferzkopp.net/Software/SDL2_gfx/SDL2_gfx-1.0.3.tar.gz')
deps+=('sqlite,https://sqlite.org/2017/sqlite-autoconf-3170000.tar.gz')
deps+=('yasm,http://www.tortall.net/projects/yasm/releases/yasm-1.3.0.tar.gz')
deps+=('ffmpeg,https://ffmpeg.org/releases/ffmpeg-3.2.2.tar.gz')
deps+=('portaudio,http://www.portaudio.com/archives/pa_stable_v190600_20161030.tgz')
deps+=('freetype,http://download.savannah.gnu.org/releases/freetype/freetype-2.7.1.tar.gz')
deps+=('libpng,https://sourceforge.net/projects/libpng/files/libpng16/older-releases/1.6.28/libpng-1.6.28.tar.gz/download')

rm -rf deps

echo "Downloading dependencies"
for i in "${deps[@]}"; do
	IFS=',' read -a dep <<< "$i"
	name="${dep[0]}"
	url="${dep[1]}"
	echo "Downloading $url"
	mkdir -p "deps/$name"
	curl --progress-bar -L "$url" | tar -xz -C "deps/$name" --strip-components=1
done
