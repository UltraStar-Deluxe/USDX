
ARCH = $(shell uname -m)
VERSION = $(shell cat ../../VERSION)
REV = $(shell git rev-parse --short HEAD)
SHELL := /bin/bash
ifeq ($(ARCH),i686)
	POSTFIX = 32
endif

.PHONY: default build-deps build$(POSTFIX) run compress clean distclean upload

default: build$(POSTFIX)

build-deps: prefix$(POSTFIX)/built_libs
prefix/built_libs:
	@./dl.sh
	@bash -eo pipefail -c './build-deps.sh all | tee build-deps.log'

run: output/ultrastardx
	output/launch.sh

build$(POSTFIX): output$(POSTFIX)/ultrastardx
output$(POSTFIX)/ultrastardx output$(POSTFIX)/VERSION: prefix$(POSTFIX)/built_libs
	@bash -eo pipefail -c './build.sh | tee build.log'
	@mkdir -p output$(POSTFIX)/data/songs
	@cp -v launch.sh output$(POSTFIX)/
	@cp -v ../../LICENSE output$(POSTFIX)/
	@cp -v ../../game/LICENSE.* output$(POSTFIX)/
	echo -e "Version: $(VERSION)\nRevison: $(REV)\nBuild date: `date -u +%FT%TZ`" > output$(POSTFIX)/VERSION

compress: UltraStarDeluxe-$(REV)-$(ARCH).tar.xz
UltraStarDeluxe-$(REV)-$(ARCH).tar.xz: output$(POSTFIX)/ultrastardx output$(POSTFIX)/VERSION
	rm -f "./UltraStarDeluxe-$(REV)-$(ARCH).tar.xz"
	cd output$(POSTFIX) && XZ_OPT="--threads=0" tar cJf "../UltraStarDeluxe-$(REV)-$(ARCH).tar.xz" .

upload: UltraStarDeluxe-$(REV)-$(ARCH).tar.xz
	curl --upload-file './UltraStarDeluxe-$(REV)-$(ARCH).tar.xz' "https://transfer.sh/UltraStarDeluxe-$(REV)-$(ARCH).tar.xz"

clean:
	rm -rf output output32 UltraStarDeluxe*.tar.xz

distclean: clean
	rm -rf deps prefix prefix32
