#!/usr/bin/env bash

set -eo pipefail

docker build --force-rm=true -t usdx/centos7-ultrastardx .
docker run --rm -it -v "$(realpath ../..):/src" -w /src/dists/linux --user $(id -u):$(id -g) -e "TERM=$TERM" -h usdxbuilder usdx/centos7-ultrastardx ./dockerinit.sh
