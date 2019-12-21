#!/bin/sh -e

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

elif [ "$VARIANT" = flatpak ]; then
    # Linux build

    sed -i 's/^\([[:space:]]*\)-\([[:space:]]*\)\(\<type: dir\>.*\)/&\n\1 \2skip:\n\1 \2- flatpak\n\1 \2- dists\/linux\/prefix/' dists/flatpak/*.yaml
    mkdir flatpak
    cd flatpak
    ln -s ../dists/linux/prefix .flatpak-builder
    rm -Rf .flatpak-builder/build
    flatpak-builder --user --stop-at=ultrastardx build ../dists/flatpak/eu.usdx.UltraStarDeluxe.yaml
    rm -Rf build
    rm .flatpak-builder
    cp -al ../dists/linux/prefix .flatpak-builder
    flatpak-builder --user --repo=repo build ../dists/flatpak/eu.usdx.UltraStarDeluxe.yaml
    flatpak build-bundle repo UltraStarDeluxe.flatpak eu.usdx.UltraStarDeluxe
    filename="UltraStarDeluxe.flatpak"
    outfile="UltraStarDeluxe-$(git rev-parse --short HEAD)-$(uname -m).flatpak"
    if [ -r "$filename" ]; then
        link="$(curl --upload-file "$filename" "https://transfer.sh/$outfile")"
        echo "$outfile should be available at:"
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
