
ARCH = $(shell uname -m)
VERSION = $(shell cat ../../VERSION)
REV = $(shell git rev-parse --short HEAD)
SHELL := /bin/bash
EXE = ultrastardx.x86_64
ifeq ($(ARCH),i686)
	POSTFIX = 32
	EXE = ultrastardx.x86
endif

.PHONY: default build-deps build complete_bundle run compress upload clean distclean

default: build

build-deps: prefix$(POSTFIX)/built_libs
prefix$(POSTFIX)/built_libs:
	@./dl.sh
	@echo
	@bash -eo pipefail -c './tasks.sh all_deps | tee build$(POSTFIX)-deps.log'
	@sed -r -i "s/\x1B(\[[0-9;]*[JKmsu]|\(B)//g" "build$(POSTFIX)-deps.log" # remove color codes
	@echo

build: output/$(EXE)
output/$(EXE) output/VERSION: prefix$(POSTFIX)/built_libs
	@bash -eo pipefail -c './build.sh | tee build$(POSTFIX).log'
	@sed -r -i "s/\x1B(\[[0-9;]*[JKmsu]|\(B)//g" "build$(POSTFIX).log" # remove color codes

# run with Docker:
# ./dockerenv.sh make complete_bundle
complete_bundle:
	@make clean # ensure clean build
	@make build # 64-bit build
	@linux32 make build # 32-bit build
	@make compress # create archive

run: output/$(EXE)
	@output/launch.sh

compress: UltraStarDeluxe.tar.xz
UltraStarDeluxe.tar.xz: output/$(EXE) output/VERSION
	rm -f "./UltraStarDeluxe.tar.xz"
	cd output && XZ_OPT="--threads=0" tar cJf "../UltraStarDeluxe.tar.xz" .

upload: UltraStarDeluxe.tar.xz
	curl --upload-file './UltraStarDeluxe.tar.xz' "https://transfer.sh/UltraStarDeluxe.tar.xz" && echo

clean:
	rm -rf output prefix prefix32 UltraStarDeluxe*.tar.xz

distclean: clean
	rm -rf deps
