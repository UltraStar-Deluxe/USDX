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

elif [ "$VARIANT" = flatpak ]; then
    # Linux build

    sudo apt-get install elfutils unzip librsvg2-common
    sudo apt-get install flatpak flatpak-builder
    case "$TRAVIS_CPU_ARCH" in
    amd64)
        if [ "$BUILD_32BIT" = yes ] ; then
            FLATPAK_ARCH=i386
            # 18.08 was the last runtime to officially support x86-32
            sed -i "/runtime-version:/s/:.*/: '18.08'/" $DIR/../../dists/flatpak/eu.usdx.UltraStarDeluxe.yaml
        else
            FLATPAK_ARCH=x86_64
        fi
        ;;
    arm64)
        if [ "$BUILD_32BIT" = yes ] ; then
            FLATPAK_ARCH=arm
            sed -i "/runtime-version:/s/:.*/: '19.08'/" $DIR/../../dists/flatpak/eu.usdx.UltraStarDeluxe.yaml
        else
            FLATPAK_ARCH=aarch64
        fi
        ;;
    *) FLATPAK_ARCH=$TRAVIS_CPU_ARCH
    esac
    case "$TRAVIS_CPU_ARCH" in
    ppc64le)
        FLATPAK_REMOTE=freedesktop-sdk
        FLATPAK_REMOTE_URL=https://releases.freedesktop-sdk.io/freedesktop-sdk.flatpakrepo
        ;;
    *)
        FLATPAK_REMOTE=flathub
        FLATPAK_REMOTE_URL=https://flathub.org/repo/flathub.flatpakrepo
        ;;
    esac
    flatpak remote-add --user --if-not-exists $FLATPAK_REMOTE $FLATPAK_REMOTE_URL
    RUNTIME_VERSION=`sed -n "/runtime-version:/s/.*'\([^']*\)'/\1/p" $DIR/../../dists/flatpak/eu.usdx.UltraStarDeluxe.yaml`
    flatpak install --user --arch=$FLATPAK_ARCH --noninteractive -y $FLATPAK_REMOTE org.freedesktop.Platform//$RUNTIME_VERSION org.freedesktop.Sdk//$RUNTIME_VERSION
    if [ ${RUNTIME_VERSION%%.*} -lt 19 ] ; then
        sed -i "/name: dav1d/,/^-/{/disabled:/s/true/false/}" $DIR/../../dists/flatpak/eu.usdx.UltraStarDeluxe.yaml
    fi

elif [ "$VARIANT" = appimage ]; then
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

fi
