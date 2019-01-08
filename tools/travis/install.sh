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
        fpc portaudio binutils freetype libpng lua libtiff \
        portmidi

    # This is from: https://github.com/Homebrew/homebrew-core
    brew install ffmpeg@4

else
    # Linux build

    #sudo apt-get install fpc \
    #    libsdl2-dev libsdl2-image-dev libsdl2-mixer-dev libsdl2-net-dev \
    #    libsdl2-ttf-dev libsdl2-gfx-dev \
    #    libavcodec-dev libavformat-dev libswscale-dev \
    #    portaudio19-dev libprojectm-dev libopencv-highgui-dev \
    #    libsqlite3-dev liblua5.1-dev libpng-dev \
    #    ttf-dejavu ttf-freefont
        
    # Extra dependencies for ffmpeg from ppa
    # sudo apt-get install \
    #    libavcodec-ffmpeg-dev libavformat-ffmpeg-dev libswscale-ffmpeg-dev \
    #    libavutil-ffmpeg-dev libswresample-ffmpeg-dev

    sudo apt-get install \
        fpc liblua5.1-dev libopencv-highgui-dev \
        cmake ftgl-dev libglew-dev \
        build-essential autoconf automake \
        libtool libasound2-dev libpulse-dev libaudio-dev libx11-dev libxext-dev \
        libxrandr-dev libxcursor-dev libxi-dev libxinerama-dev libxxf86vm-dev \
        libxss-dev libgl1-mesa-dev libdbus-1-dev libudev-dev libportmidi-dev \
        libgles1-mesa-dev libgles2-mesa-dev libegl1-mesa-dev libibus-1.0-dev \
        fcitx-libs-dev libsamplerate0-dev \
        libwayland-dev libxkbcommon-dev ibus \
        chrpath curl

fi
