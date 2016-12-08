#!/bin/sh

DIR=$(dirname $0)

if [ -n "$LAZ_VER" ]; then
    # Lazarus build (with wine)

    $DIR/lazarus/.travis.install.py

elif [ "$TRAVIS_OS_NAME" = "osx" ]; then
    # OSX build

    # already present on travis
    #brew cask install xquartz

    brew install sdl2 sdl2_gfx sdl2_image sdl2_mixer sdl2_net sdl2_ttf \
        fpc portaudio binutils sqlite freetype libpng pcre lua libtiff

    # This is from: https://github.com/Homebrew/homebrew-versions
    brew install ffmpeg28

else
    # Linux build

    sudo apt-get install fpc \
        libsdl2-dev libsdl2-image-dev libsdl2-mixer-dev libsdl2-net-dev \
        libsdl2-ttf-dev libsdl2-gfx-dev \
        libavcodec-dev libavformat-dev libswscale-dev \
        portaudio19-dev libprojectm-dev libopencv-highgui-dev \
        libsqlite3-dev libpcre3-dev liblua5.1-dev libpng-dev \
        ttf-dejavu ttf-freefont

fi
