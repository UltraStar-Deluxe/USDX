#!/bin/sh

if [ -n "$LAZ_OPT" ]; then
    # Lazarus build (with wine)

    lazbuild $LAZ_OPT ./src/ultrastardx-travis.lpi

else
    # Linux build

    ./autogen.sh
    ./configure
    make

fi
