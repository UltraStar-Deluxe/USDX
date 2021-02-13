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

    CACHEDIR=dists/linux/prefix
    DONTCACHE=ultrastardx

    sed -i 's%^\([[:space:]]*\)-\([[:space:]]*\)\(\<type: dir\>.*\)%&\n\1 \2skip:\n\1 \2- flatpak\n\1 \2- '$CACHEDIR'%' dists/flatpak/*.yaml
    mkdir flatpak
    cd flatpak

    case "$TRAVIS_CPU_ARCH" in
    amd64)
        if [ "$BUILD_32BIT" = yes ] ; then
            FLATPAK_ARCH=i386
        else
            FLATPAK_ARCH=x86_64
        fi
        ;;
    arm64)
        if [ "$BUILD_32BIT" = yes ] ; then
            FLATPAK_ARCH=arm
        else
            FLATPAK_ARCH=aarch64
        fi
        ;;
    *) FLATPAK_ARCH=$TRAVIS_CPU_ARCH
    esac

    mkdir -p ../$CACHEDIR/flatpak-builder

    for i in downloads cache build checksums ccache rofiles ; do
        if [ -d ../$CACHEDIR/$i -a ! -e ../$CACHEDIR/flatpak-builder/$i ]; then
            mv ../$CACHEDIR/$i ../$CACHEDIR/flatpak-builder/$i
        elif [ -e ../$CACHEDIR/$i ]; then
            rm -Rf ../$CACHEDIR/$i
        fi
    done

    ln -s ../$CACHEDIR/flatpak-builder .flatpak-builder
    rm -Rf .flatpak-builder/build
    flatpak-builder --arch=$FLATPAK_ARCH --jobs=2 --user --stop-at=$DONTCACHE build ../dists/flatpak/eu.usdx.UltraStarDeluxe.yaml
    rm -Rf build
    rm .flatpak-builder
    cp -al ../$CACHEDIR/flatpak-builder .flatpak-builder
    flatpak-builder --arch=$FLATPAK_ARCH --jobs=2 --user --repo=repo build ../dists/flatpak/eu.usdx.UltraStarDeluxe.yaml
    date +"%c Creating flatpak bundle"
    flatpak build-bundle --arch=$FLATPAK_ARCH repo UltraStarDeluxe.flatpak eu.usdx.UltraStarDeluxe
    filename="UltraStarDeluxe.flatpak"
    outfile="UltraStarDeluxe-$(git rev-parse --short HEAD)-${FLATPAK_ARCH}.flatpak"
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
