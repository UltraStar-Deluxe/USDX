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
        fpc portaudio binutils freetype libpng lua libtiff opencv \
        portmidi

    # This is from: https://github.com/Homebrew/homebrew-core
    brew install ffmpeg@4

elif [ "$VARIANT" = appimage ] && [ "$TRAVIS_DIST" = trusty ]; then
    # Linux build

    sudo apt-get install \
        fpc \
        cmake \
        build-essential autoconf automake \
        libtool libasound2-dev libx11-dev libxext-dev \
        libxrandr-dev libxcursor-dev libxi-dev libxinerama-dev libxxf86vm-dev \
        libxss-dev libgl1-mesa-dev libdbus-1-dev libudev-dev \
        libjack-jackd2-dev libpulse-dev \
        libegl1-mesa-dev libgbm-dev libdrm-dev \
        libxkbcommon-dev \
        zlib1g-dev libfreetype6-dev \
        libfuse-dev libcairo2-dev libglib2.0-dev \
        zsync desktop-file-utils libarchive-dev \
        curl realpath

elif [ "$VARIANT" = appimage ] ; then
    # Linux build inside docker container

    sudo apt-get install \
        curl

else
    # Linux build

    # Keep list in sync with README.md.
    # No need for git. The source code has already been downloaded.
    sudo apt-get install \
        automake make gcc fpc libsdl2-image-dev libavformat-dev \
        libswscale-dev libsqlite3-dev libfreetype6-dev portaudio19-dev \
        libportmidi-dev liblua5.3-dev libopencv-videoio-dev \
        g++ libprojectm-dev libopencv-dev

fi
