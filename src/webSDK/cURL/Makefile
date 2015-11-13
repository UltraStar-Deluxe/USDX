
usage: oschk
	@${ECHO} ""
	@${ECHO} "Usage: make <option>"
	@${ECHO} ""
	@${ECHO} "Available options are:"
	@${ECHO} "  fpc:    ( To build the Free Pascal units. )"
	@${ECHO} "  laz:    ( To build the Lazarus component. )"
	@${ECHO} "  dcc:    ( To build the Borland component. )"
	@${ECHO} "  clean:  ( To remove the binary files created by make. )"
	@${ECHO} "  fpc-debug: ( fpc with debug symbols. )"
	@${ECHO} "  laz-debug: ( laz with debug symbols. )"
	@${ECHO} "  dcc-debug: ( dcc with debug symbols. )"
	@${ECHO} ""

fpc:
	 $(MAKE) -C src  fpc
	 $(MAKE) -C demo fpc

dcc:
	$(MAKE) -C src  dcc
	$(MAKE) -C demo dcc
	$(MAKE) -C gui  dcc

laz:
	$(MAKE) -C src laz
	$(MAKE) -C gui laz

fpc-debug:
	$(MAKE) -C src  fpc-debug
	$(MAKE) -C demo fpc

dcc-debug:
	$(MAKE) -C src  dcc-debug
	$(MAKE) -C demo dcc
	$(MAKE) -C gui  dcc


laz-debug:
	$(MAKE) -C src laz-debug
	$(MAKE) -C gui laz


clean:
	 $(MAKE) -C src clean
	 $(MAKE) -C demo clean
	 $(MAKE) -C gui clean

.PHONY: clean dcc dcc-debug fpc fpc-debug laz laz-debug usage

include ./OSCheck.gmk
