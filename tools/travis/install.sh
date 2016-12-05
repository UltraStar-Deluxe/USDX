#!/bin/sh

DIR=$(dirname $0)

if [ -n "$LAZ_VER" ]; then
    # Lazarus build (with wine)

    $DIR/lazarus/.travis.install.py

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
