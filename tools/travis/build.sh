#!/bin/sh

if [ -n "$LAZ_OPT" ]; then
    # Lazarus build (with wine)

    lazbuild $LAZ_OPT ./src/ultrastardx-travis.lpi

elif [ "$TRAVIS_OS_NAME" = "osx" ]; then
    # OSX build

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

    #./autogen.sh
    #./configure
    #make
    cd dists/linux
    make upload
fi
