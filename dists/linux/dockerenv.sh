#!/usr/bin/env bash

set -eo pipefail

docker build --force-rm=true --rm -t usdx/centos7-ultrastardx .

docker run --rm -it \
	-v "$(realpath ../..):/src" \
	-v "/etc/passwd:/etc/passwd:ro" \
	-v "/etc/group:/etc/group:ro" \
	--user $(id -u):$(id -g) \
	-e "TERM=$TERM" \
	-e "PS1=[\u@$(tput setaf 6 && tput bold)\h$(tput sgr0) \W]\$ " \
	-h usdxbuilder \
	-w /src/dists/linux \
	usdx/centos7-ultrastardx \
	"$@"
