
usage: oschk
	@${ECHO} ""
	@${ECHO} "Usage: make <option>"
	@${ECHO} ""
	@${ECHO} "Available options are:"
	@${ECHO} "  fpc:    ( To build the FreePascal units. )"
	@${ECHO} "  laz:    ( To build the Lazarus component. )"
	@${ECHO} "  dcc:    ( To build the Kylix/Delphi component. )"
	@${ECHO} "  clean:  ( To remove the binary files created by make. )"
	@${ECHO} "  fpc-debug: ( fpc with debug symbols. )"
	@${ECHO} "  laz-debug: ( laz with debug symbols. )"
	@${ECHO} "  dcc-debug: ( dcc with debug symbols. )"
	@${ECHO} ""

UNITS=curl_h.pas  curlobj.pas


fpc: fpc-clean
	ppc386 curl_h.pas
	ppc386 curlobj.pas

dcc: dcc-clean
	$(PKG_RES)
	$(UNIT_RES)
	$(DCC) -Q curl_h.pas
	$(DCC) -Q curlobj.pas
	$(DCC) -Q curlpkg.dpk

laz: laz-clean
	$(MKDIR) -p ./lib
	ppc386 -FE./lib curlpas.pas


windres:
	windres.exe curlobj.rc curlobj.res
	windres.exe curlpkg.rc curlpkg.res
	cp curlobj.res lazcurl.res


brcc32:
	brcc32.exe -focurlobj.res -r curlobj.rc
	brcc32.exe -focurlpkg.res -r curlpkg.rc


fpc-debug: fpc-clean
	ppc386 -vewh -gl -Crtoi curl_h.pas
	ppc386 -vewh -gl -Crtoi curlobj.pas


dcc-debug: dcc-clean
	$(DCC) -Q -V -H -W -$$D+ -$$Y+ curl_h.pas
	$(DCC) -Q -V -H -W -$$D+ -$$Y+ curlobj.pas
	$(DCC) -Q -V -H -W -$$D+ -$$Y+ curlpkg.dpk

laz-debug: laz-clean
	$(MKDIR) -p ./lib
	ppc386 -vewh -gl -Crtoi -FE./lib curlpas.pas


all: fpc dcc laz

all-debug: fpc-debug dcc-debug laz-debug


fpc-clean:
	rm -f *.ppu *.o *.ppw *.ow

dcc-clean: oschk
	rm -f *.dpu bplcurlpkg.so  *.dcu *.bpl *.dcp
	@$(ECHO) -en "package curlpkg;$(NL)\
$(NL){\044R *.res}$(NL)$(NL)requires$(NL) \
rtl;$(NL)$(NL)contains$(NL) \
curlobj in 'curlobj.pas';$(NL)$(NL)end.$(NL)" > curlpkg.dpk

laz-clean: oschk
	rm -rf ./lib curlpas.compiled *.or

tmp-clean:
	rm -f *.a *.~desk *.conf *.desk  *.~?? *.~??? *.bak *.cfg *.dof *.dsk


clean: tmp-clean fpc-clean laz-clean dcc-clean



.PHONY: all all-debug clean dcc dcc-clean dcc-debug fpc fpc-clean fpc-debug \
laz laz-clean laz-debug tmp-clean usage doc

include ../OSCheck.gmk
