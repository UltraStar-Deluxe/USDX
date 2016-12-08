#!/bin/sh

if [ -n "$LAZ_OPT" ]; then
    # Lazarus build (with wine)

    lazbuild $LAZ_OPT ./src/ultrastardx-travis.lpi

elif [ "$TRAVIS_OS_NAME" = "osx" ]; then
    # OSX build

    ./configure --enable-osx-brew
    make macosx-standalone-app

else
    # Linux build

    ./autogen.sh
    ./configure
    make

fi
