
ARCH = $(shell uname -m)
VERSION = $(shell cat ../../VERSION)
REV = $(shell git rev-parse --short HEAD)
SHELL := /bin/bash
ifeq ($(ARCH),i686)
	POSTFIX = 32
endif

.PHONY: default build-deps build run compress clean distclean upload

default: build

build-deps: prefix$(POSTFIX)/built_libs
prefix$(POSTFIX)/built_libs:
	@./dl.sh
	@echo
	@bash -eo pipefail -c './tasks.sh all | tee build-deps.log'
	@sed -r -i "s/\x1B(\[[0-9;]*[JKmsu]|\(B)//g" "build-deps.log" # remove color codes
	@echo

run: output/ultrastardx
	output/launch.sh

build: output$(POSTFIX)/ultrastardx
output$(POSTFIX)/ultrastardx output$(POSTFIX)/VERSION: prefix$(POSTFIX)/built_libs
	@bash -eo pipefail -c './build.sh | tee build.log'
	@sed -r -i "s/\x1B(\[[0-9;]*[JKmsu]|\(B)//g" "build.log" # remove color codes

compress: UltraStarDeluxe-$(REV)-$(ARCH).tar.xz
UltraStarDeluxe-$(REV)-$(ARCH).tar.xz: output$(POSTFIX)/ultrastardx output$(POSTFIX)/VERSION
	rm -f "./UltraStarDeluxe-$(REV)-$(ARCH).tar.xz"
	cd output$(POSTFIX) && XZ_OPT="--threads=0" tar cJf "../UltraStarDeluxe-$(REV)-$(ARCH).tar.xz" .

upload: UltraStarDeluxe-$(REV)-$(ARCH).tar.xz
	curl --upload-file './UltraStarDeluxe-$(REV)-$(ARCH).tar.xz' "https://transfer.sh/UltraStarDeluxe-$(REV)-$(ARCH).tar.xz"

clean:
	rm -rf output output32 prefix prefix32 UltraStarDeluxe*.tar.xz

distclean: clean
	rm -rf deps
