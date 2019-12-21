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

    if [ "$TRAVIS_CPU_ARCH" = arm64 ]; then
        FPC_ARCH=aarch64
        FPC_ARCH_EXT=a64
        BOOTSTRAP_VERSION=3.0.4
        # arm64 is not officially supported in Free Pascal 3.0.4,
        # but Bionic backported support for it. We use this opportunity
        # to test building with a bleeding edge Free Pascal.
        NOW=`date +%s`
        FPC_INSTALLER=$CACHEDIR/fpc-installer
        CACHETIME=0
        if [ -e ../$FPC_INSTALLER/time ]; then
            read CACHETIME < ../$FPC_INSTALLER/time
        fi
        # Don't build more than one snapshot per day and check the revision
        # information before discarding an older cached build.
        if [ `expr $NOW - $CACHETIME` -ge 86400 ]; then
            curl -s -S -L ftp://ftp.freepascal.org/pub/fpc/snapshot/trunk/source/fpcbuild-readme | tail -n +2 > revision
        else
            cp ../$FPC_INSTALLER/revision .
        fi
        if cmp revision ../$FPC_INSTALLER/revision ; then
            date +"%c Using Free Pascal snapshot from cache"
        else
            rm -Rf ../$FPC_INSTALLER
            mkdir -p ../$FPC_INSTALLER
            date +"%c Installing Free Pascal compiler for bootstrapping"
            sudo apt-get install fp-compiler-$BOOTSTRAP_VERSION
            FPC=ppc$FPC_ARCH_EXT-$BOOTSTRAP_VERSION
            date +"%c Downloading Free Pascal snapshot"
            curl -s -S -L ftp://ftp.freepascal.org/pub/fpc/snapshot/trunk/source/fpcbuild.zip -o fpcbuild.zip
            date +"%c Building Free Pascal snapshot"
            unzip fpcbuild.zip

            cd fpcbuild
            make distclean CPU_TARGET=$FPC_ARCH OS_TARGET=linux
            VERSION=`grep '^version *=' fpcsrc/Makefile.fpc | sed 's+[^=]*= *\([0-9\.]*\).*+\1+'`
            make zipinstall CPU_TARGET=$FPC_ARCH OS_TARGET=linux OPT="" FPC_VERSION=$VERSION FPC=$FPC
            tar cf ../../$FPC_INSTALLER/binary.$FPC_ARCH-linux.tar *.$FPC_ARCH-linux.tar.gz
            sed -e s+%version%+$VERSION+ -e s+%fullversion%+$VERSION+ install/install.sh > ../../$FPC_INSTALLER/install.sh
            chmod 755 ../../$FPC_INSTALLER/install.sh
            cd ..

            cp revision ../$FPC_INSTALLER
            echo $NOW > ../$FPC_INSTALLER/time
            date +"%c Building remaining packages with flatpak-builder"
        fi
        sed -i 's%.*aarch64 is supported in SVN trunk.*%  - type: dir\n    only-arches:\n    - aarch64\n    path: ../../'$FPC_INSTALLER'\n%' ../dists/flatpak/eu.usdx.UltraStarDeluxe.yaml
        DONTCACHE=freepascal
    fi

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
    flatpak-builder --user --stop-at=$DONTCACHE build ../dists/flatpak/eu.usdx.UltraStarDeluxe.yaml
    rm -Rf build
    rm .flatpak-builder
    cp -al ../$CACHEDIR/flatpak-builder .flatpak-builder
    flatpak-builder --user --repo=repo build ../dists/flatpak/eu.usdx.UltraStarDeluxe.yaml
    date +"%c Creating flatpak bundle"
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
