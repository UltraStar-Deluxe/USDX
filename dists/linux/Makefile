
ARCH = $(shell uname -m)
VERSION = $(shell cat ../../VERSION)
REV = $(shell git rev-parse --short HEAD)
SHELL := /bin/bash

.PHONY: default build-deps build run compress clean distclean upload

default: build

build-deps: prefix/built_libs
prefix/built_libs:
	@./dl.sh
	@bash -eo pipefail -c './build-deps.sh all | tee build-deps.log'

run: output/ultrastardx
	output/launch.sh

build: output/ultrastardx
output/ultrastardx output/VERSION: prefix/built_libs
	@bash -eo pipefail -c './build.sh | tee build.log'
	@mkdir -p output/data/songs
	@cp -v launch.sh output/
	@cp -v ../../LICENSE output/
	@cp -v ../../game/LICENSE.* output/
	echo -e "Version: $(VERSION)\nRevison: $(REV)\nBuild date: `date -u +%FT%TZ`" > output/VERSION

compress: UltraStarDeluxe-$(REV)-$(ARCH).tar.xz
UltraStarDeluxe-$(REV)-$(ARCH).tar.xz: output/ultrastardx output/VERSION
	rm -f "./UltraStarDeluxe-$(REV)-$(ARCH).tar.xz"
	cd output && tar cJf "../UltraStarDeluxe-$(REV)-$(ARCH).tar.xz" .

upload: UltraStarDeluxe-$(REV)-$(ARCH).tar.xz
	curl --upload-file './UltraStarDeluxe-$(REV)-$(ARCH).tar.xz' "https://transfer.sh/UltraStarDeluxe-$(REV)-$(ARCH).tar.xz"

clean:
	rm -rf output UltraStarDeluxe*.tar.xz

distclean: clean
	rm -rf deps prefix
