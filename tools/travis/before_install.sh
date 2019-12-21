#!/bin/sh

if [ -n "$LAZ_VER" ]; then
    # Lazarus build (with wine)

    # Start virtual display server - this should be done in the .travis.yml services area once the Lazarus submodule supports Ubuntu >= Xenial
    sh /etc/init.d/xvfb start
	echo "Lazarus build (with wine)"

elif [ "$TRAVIS_OS_NAME" = "osx" ]; then
    # OSX build

    brew tap caskroom/cask
    brew tap homebrew/versions
    brew update

elif [ "$VARIANT" = flatpak ]; then

    # flatpak-builder in bionic is too old to understand YAML manifests
    sudo add-apt-repository -y ppa:alexlarsson/flatpak
    sudo apt-get update

else
    # Linux build

    # Adding fpc repository
    sudo add-apt-repository -y ppa:ok2cqr/lazarus

    # ffmpeg (2.4) version for trusty
    # sudo add-apt-repository -y ppa:kirillshkrogalev/ffmpeg-next

    sudo apt-get update

fi
