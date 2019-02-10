
ARCH = $(shell uname -m)
VERSION = $(shell cat ../../VERSION)
REV = $(shell git rev-parse --short HEAD)
SHELL := /bin/bash
EXE = ultrastardx

.PHONY: default build-deps build run compress bundle upload clean distclean

default: build

build-deps: prefix/$(ARCH)/built_libs
prefix/$(ARCH)/built_libs:
	@./dl.sh
	@echo
	@bash -eo pipefail -c './tasks.sh all_deps | tee build-deps.log'
	@sed -r -i "s/\x1B(\[[0-9;]*[JKmsu]|\(B)//g" "build-deps.log" # remove color codes
	@echo

build: build/$(ARCH)/ultrastardx
build/$(ARCH)/ultrastardx build/$(ARCH)/VERSION: prefix/$(ARCH)/built_libs
	@bash -eo pipefail -c './build.sh | tee build.log'
	@sed -r -i "s/\x1B(\[[0-9;]*[JKmsu]|\(B)//g" "build.log" # remove color codes

# run with Docker:
# ./dockerenv.sh make bundle # x86_64
# linux32 ./dockerenv.sh make bundle # i386/i686
bundle: clean compress
	@echo Complete!

run: build/$(ARCH)/ultrastardx
	@build/$(ARCH)/launch.sh

compress: UltraStarDeluxe-$(ARCH).tar.xz
UltraStarDeluxe-$(ARCH).tar.xz: build/$(ARCH)/ultrastardx build/$(ARCH)/VERSION
	rm -f "./UltraStarDeluxe-$(ARCH).tar.xz"
	cd "build/$(ARCH)" && XZ_OPT="--threads=0" tar cJf "../../UltraStarDeluxe-$(ARCH).tar.xz" .

upload: UltraStarDeluxe-$(ARCH).tar.xz
	curl --upload-file './UltraStarDeluxe-$(ARCH).tar.xz' "https://transfer.sh/UltraStarDeluxe-$(ARCH).tar.xz" && echo

clean:
	rm -rf build/$(ARCH) prefix/$(ARCH) UltraStarDeluxe-$(ARCH).tar.xz

distclean: clean
	rm -rf deps build prefix UltraStarDeluxe-*.tar.xz
