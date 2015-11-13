
usage: oschk 
	@${ECHO} ""
	@${ECHO} "Usage: make <option>"
	@${ECHO} ""
	@${ECHO} "Available options are:"
	@${ECHO} "  fpc:    ( To use the FreePascal compiler. )"
	@${ECHO} "  dcc:    ( To use the Kylix/Delphi compiler. )"
	@${ECHO} "  clean:  ( To remove the binary files created by make. )"
	@${ECHO} ""



TARGETS=simple testcb testcook testevnt testinfo testmobj testmthd testmpf1 testmpf2 \
testmult testpart testpost testput testsave testtxcb testver


ifneq "$(findstring /, $(PATH))" ""
DCC=dcc
else
DCC=dcc32.exe
endif

DCC_SMALL = $(DCC) -Q -U../src
DCC_DEBUG = $(DCC_SMALL) -V -H -W -$$D+ -$$Y+


FPC_SMALL = ppc386 -Fu../src
FPC_DEBUG = $(FPC_SMALL) -gl -Crtoi
FPC_LOUD  = $(FPC_DEBUG) -vewh

DCC_COMPILE=$(DCC_DEBUG)
FPC_COMPILE=$(FPC_DEBUG)

fpc: clean
	$(FPC_COMPILE) simple.pas
	$(FPC_COMPILE) testcb.pas
	$(FPC_COMPILE) testcook.pas
	$(FPC_COMPILE) testevnt.pas
	$(FPC_COMPILE) testinfo.pas
	$(FPC_COMPILE) testmobj.pas
	$(FPC_COMPILE) testmthd.pas
	$(FPC_COMPILE) testmpf1.pas
	$(FPC_COMPILE) testmpf2.pas
	$(FPC_COMPILE) testmult.pas
	$(FPC_COMPILE) testpart.pas
	$(FPC_COMPILE) testpost.pas
	$(FPC_COMPILE) testput.pas
	$(FPC_COMPILE) testsave.pas
	$(FPC_COMPILE) testtxcb.pas
	$(FPC_COMPILE) testver.pas
	rm -f *.o


dcc: oschk clean
	$(DCC_COMPILE) simple.pas
	$(DCC_COMPILE) testcb.pas
	$(DCC_COMPILE) testcook.pas
	$(DCC_COMPILE) testevnt.pas
	$(DCC_COMPILE) testinfo.pas
	$(DCC_COMPILE) testmobj.pas
	$(DCC_COMPILE) testmthd.pas
	$(DCC_COMPILE) testmpf1.pas
	$(DCC_COMPILE) testmpf2.pas
	$(DCC_COMPILE) testmult.pas
	$(DCC_COMPILE) testpart.pas
	$(DCC_COMPILE) testpost.pas
	$(DCC_COMPILE) testput.pas
	$(DCC_COMPILE) testsave.pas
	$(DCC_COMPILE) testtxcb.pas
	$(DCC_COMPILE) testver.pas


clean:
	rm -f $(TARGETS)
	rm -f *.exe errors.txt headers.txt *.tmp *.htm *.o *.ow


.PHONY: clean dcc fpc usage 

include ../OSCheck.gmk
