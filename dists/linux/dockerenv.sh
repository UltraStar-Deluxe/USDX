#!/usr/bin/env bash

set -eo pipefail

while true ; do
	case "$1" in
	--dockerfile)
		only_dockerfile=1
		shift
		;;
	--use-existing=*)
		use_existing="${1#*=}"
		shift
		;;
	*)
		break
		;;
	esac
done

targetarch="${ARCH-$(uname -m)}"

if [ "$targetarch" == "x86_64" ]; then
	imagename="usdx/buildenv:jessie"
	from="debian/eol:jessie"
	fpcpackage="https://sourceforge.net/projects/freepascal/files/Linux/3.2.2/fpc-3.2.2.x86_64-linux.tar"
	prefixcmd="linux64"
else
	echo "Unsupported architecture: $targetarch"
	exit 1
fi

replacements="
	s!%%from%%!$from!g;
	s!%%fpcpackage%%!$fpcpackage!g;
"

if [ "$only_dockerfile" = 1 ] ; then
	sed -r "$replacements" Dockerfile.in
	exit 0
fi

SUDO=
if docker -v > /dev/null && ! docker version >/dev/null 2>&1 ; then
	echo Assuming sudo has to be used to be able to connect to Docker daemon
	SUDO=sudo
fi

if [ -z "$use_existing" ] ; then
	sed -r "$replacements" Dockerfile.in | $SUDO docker build --force-rm=true --rm -t "$imagename" -
else
	imagename="$use_existing"
fi

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
