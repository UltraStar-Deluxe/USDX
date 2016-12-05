#!/bin/sh

if [ -n "$LAZ_VER" ]; then
    # Lazarus build (with wine)

    # Start virtual display server
    sh /etc/init.d/xvfb start

else
    # Linux build

    # Adding fpc repository
    sudo add-apt-repository -y ppa:ok2cqr/lazarus
    sudo apt-get update

fi
