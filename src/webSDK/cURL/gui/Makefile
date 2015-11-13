SRC_DIR=../src

ifneq "$(findstring /, $(PATH))" ""
CONVERT := $(shell \
for A in $$HOME  /usr/local  /opt  /usr ; \
do \
  [ -f $$A/kylix*/bin/convert ] && \
  [ -x $$A/kylix*/bin/convert ] && \
  echo $$A/kylix*/bin/convert | tail -n 1 && break; \
done\
)
else
CONVERT=convert.exe
endif


usage: oschk
	@${ECHO} ""
	@${ECHO} "Usage: make <option>"
	@${ECHO} ""
	@${ECHO} "Available options are:"
	@${ECHO} "  laz:    ( To build the Lazarus Demo. )"
	@${ECHO} "  dcc:    ( To build the Kylix/Delphi Demo. )"
	@${ECHO} "  clean:  ( To remove the binary files created by make. )"
	@${ECHO} ""


dcc: dcc-clean $(SRC_DIR)/bplcurlpkg.so dcc-form
	$(DCC)  -Q -V -H -W -$$D+ -$$Y+ -U$(SRC_DIR) curldemo.dpr



dcc-form: oschk
ifeq "$(CONVERT)" ""
	@${ECHO} ""
	@${ECHO} "Warning: Did not find a form conversion utility"
	@${ECHO} "Warning: I will try it anyway..."
	@${ECHO} ""
	convert -b curlform.frm
else
	$(CONVERT) -b curlform.frm
endif



$(SRC_DIR)/bplcurlpkg.so:
	$(MAKE) -C $(SRC_DIR) dcc



dcc-clean:
	rm -f *.o *.dcu *.~??? *.~desk *.desk  *.conf bordemo *.kof
	rm -f curlform.xfm curlform.dfm


laz: laz-clean $(SRC_DIR)/lib/lazcurl.ppu
	ppc386 -gl -Fu$(SRC_DIR)/lib curldemo.dpr


$(SRC_DIR)/lib/lazcurl.ppu:
	$(MAKE) -C $(SRC_DIR) laz


laz-clean:
	rm -f *.o *.ppu *.bak curldemo
	cp curldemo.prj curldemo.lpi
	cp curlform.frm curlform.lfm

clean: laz-clean dcc-clean
	rm -f curldemo.exe


.PHONY: clean dcc dcc-clean dcc-form dcc-pkg laz laz-clean laz-pkg usage


include ../OSCheck.gmk
