#!/bin/sh -e

if [ -n "$LAZ_OPT" ]; then
    # Lazarus build (with wine)

    lazbuild $LAZ_OPT ./src/ultrastardx-travis.lpi

elif [ "$TRAVIS_OS_NAME" = "osx" ]; then
    # OSX build

    ./autogen.sh
    ./configure --enable-osx-brew
    make macosx-standalone-app
    make macosx-dmg

    if [ -r "UltraStarDeluxe.dmg" ]; then
        link=$(curl --upload-file 'UltraStarDeluxe.dmg' "https://transfer.sh/UltraStarDeluxe-$(git rev-parse --short HEAD).dmg")
        echo "UltraStarDeluxe.dmg should be available at:"
        echo "    $link"
    fi

else
    # Linux build

    # ./autogen.sh
    # ./configure
    # make

    cd dists/linux
    make compress
    filename="UltraStarDeluxe-$(uname -m).tar.xz"
    outfile="UltraStarDeluxe-$(git rev-parse --short HEAD)-$(uname -m).tar.xz"
    if [ -r "$filename" ]; then
        link="$(curl --upload-file "$filename" "https://transfer.sh/$outfile")"
        echo "$outfile should be available at:"
        echo "    $link"
    fi
fi
