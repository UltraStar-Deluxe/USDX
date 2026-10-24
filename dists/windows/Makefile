ARCH = $(shell uname -m)

.PHONY: all install
all: prefix/$(ARCH)/built_libs

prefix/$(ARCH)/built_libs: tasks.sh
	@./tasks.sh dl
	@./tasks.sh all_deps

install: all
	@./tasks.sh install
