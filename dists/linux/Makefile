
ARCH = $(shell uname -m)
VERSION = $(shell cat ../../VERSION)
REV = $(shell git rev-parse --short HEAD)
SHELL := /bin/bash
EXE = ultrastardx

.PHONY: default build-deps build run compress bundle upload clean distclean

default: build

build-deps: prefix/built_libs
prefix/built_libs:
	@./dl.sh
	@echo
	@bash -eo pipefail -c './tasks.sh all_deps | tee build-deps.log'
	@sed -r -i "s/\x1B(\[[0-9;]*[JKmsu]|\(B)//g" "build-deps.log" # remove color codes
	@echo

build: output/ultrastardx
output/ultrastardx output/VERSION: prefix/built_libs
	@bash -eo pipefail -c './build.sh | tee build.log'
	@sed -r -i "s/\x1B(\[[0-9;]*[JKmsu]|\(B)//g" "build.log" # remove color codes

# run with Docker:
# ./dockerenv.sh make
bundle: clean compress
	@echo Complete!

run: output/ultrastardx
	@output/launch.sh

compress: UltraStarDeluxe.tar.xz
UltraStarDeluxe.tar.xz: output/ultrastardx output/VERSION
	rm -f "./UltraStarDeluxe.tar.xz"
	cd output && XZ_OPT="--threads=0" tar cJf "../UltraStarDeluxe.tar.xz" .

upload: UltraStarDeluxe.tar.xz
	curl --upload-file './UltraStarDeluxe.tar.xz' "https://transfer.sh/UltraStarDeluxe.tar.xz" && echo

clean:
	rm -rf output prefix UltraStarDeluxe*.tar.xz

distclean: clean
	rm -rf deps
