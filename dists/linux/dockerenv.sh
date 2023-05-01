#!/usr/bin/env bash

set -eo pipefail

SUDO=
if docker -v > /dev/null && ! docker version >/dev/null 2>&1 ; then
	echo Assuming sudo has to be used to be able to connect to Docker daemon
	SUDO=sudo
fi

targetarch="${ARCH-$(uname -m)}"

if [ "$targetarch" == "x86_64" ]; then
	imagename="usdx/buildenv:centos7"
	from="centos:7"
	fpcpackage="https://sourceforge.net/projects/freepascal/files/Linux/3.2.2/fpc-3.2.2.x86_64-linux.tar"
	prefixcmd="linux64"
	epelpkgs="cmake3 ninja-build patchelf"
elif [ "$targetarch" == "i386" ] || [ "$targetarch" == "i686" ]; then
	imagename="usdx/buildenv:centos7-i386"
	from="i386/centos:7"
	fpcpackage="https://sourceforge.net/projects/freepascal/files/Linux/3.2.2/fpc-3.2.2.i386-linux.tar"
	prefixcmd="linux32"
	epelpkgs=""
else
	echo "Unsupported architecture: $targetarch"
	exit 1
fi

replacements="
	s!%%from%%!$from!g;
	s!%%fpcpackage%%!$fpcpackage!g;
	s!%%epelpkgs%%!$epelpkgs!g;
"

sed -r "$replacements" Dockerfile.in | $SUDO docker build --force-rm=true --rm -t "$imagename" -

$SUDO docker run --rm -it \
	-v "$(realpath ../..):/src" \
	-v "/etc/passwd:/etc/passwd:ro" \
	-v "/etc/group:/etc/group:ro" \
	--user $(id -u):$(id -g) \
	-e "TERM=$TERM" \
	-e "PS1=[\u@$(tput setaf 6)$(tput bold)\h:$(uname -m)$(tput sgr0) \W]\$ " \
	-h "usdxbuilder" \
	-w /src/dists/linux \
	"$imagename" \
	"$prefixcmd" "$@"
