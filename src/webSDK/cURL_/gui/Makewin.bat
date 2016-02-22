@ECHO OFF

IF "%1" == "dcc" GOTO MAKE_DCC
IF "%1" == "clean" GOTO MAKE_CLEAN
ECHO.
ECHO Usage: MAKEWIN (option)
ECHO.
ECHO Available options are:
ECHO   MAKEWIN dcc   ( To build the Delphi GUI demo. )
ECHO   MAKEWIN clean ( To clean up temp files )
ECHO.
GOTO END

:MAKE_DCC
IF EXIST ..\src\curlobj.dcu GOTO MAKE_GUI
CD ..\src
CALL makewin dcc
CD ..\gui
:MAKE_GUI
convert -b curlform.frm
dcc32 -U..\src curldemo.dpr
GOTO END


:MAKE_CLEAN
@ECHO ON
IF EXIST *.o   DEL *.o
IF EXIST *.ppu  DEL *.ppu
IF EXIST *.exe  DEL *.exe
IF EXIST *.bak  DEL *.bak
@ECHO OFF
GOTO END

:END
