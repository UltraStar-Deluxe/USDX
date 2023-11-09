#!/bin/bash -e

if [ -n "$LAZ_OPT" ]; then
    # Lazarus build (with wine)

    lazbuild $LAZ_OPT ./src/ultrastardx-travis.lpi

elif [ "$TRAVIS_OS_NAME" = "osx" ]; then
    # OSX build

    ./autogen.sh
    ./configure --enable-osx-brew --with-opencv-cxx-api
    make macosx-standalone-app
    make macosx-dmg

    if [ -r "UltraStarDeluxe.dmg" ]; then
        link=$(curl --upload-file 'UltraStarDeluxe.dmg' "https://transfer.sh/UltraStarDeluxe-$(git rev-parse --short HEAD).dmg")
        echo "UltraStarDeluxe.dmg should be available at:"
        echo "    $link"
    fi

elif [ "$VARIANT" = appimage ] ; then
    # Linux build

    git fetch --unshallow --tags
    cd dists/linux

    prepend=""
    if [ "$TRAVIS_CPU_ARCH" != amd64 ] || [ "$BUILD_32BIT" = yes ] ; then
        prepend=./dockerenv.sh
        sed -i '/docker/s/-it\>//' dockerenv.sh

        if [ "$BUILD_32BIT" = yes ] ; then
            prepend="linux32 $prepend"
        fi
    fi

    $prepend make compress
    set --  UltraStarDeluxe-*.AppImage
    filename="$1"
    outfile="UltraStarDeluxe-$(git rev-parse --short HEAD)-${filename#*-}"
    if [ -r "$filename" ]; then
        link="$(curl --upload-file "$filename" "https://transfer.sh/$outfile")"
        echo "$outfile should be available at:"
        echo "    $link"
    fi
else
    # Linux build

    ./autogen.sh
    ./configure --with-opencv-cxx-api --with-libprojectM
    make
    make install DESTDIR=out
fi
