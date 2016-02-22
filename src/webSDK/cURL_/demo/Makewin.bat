@ECHO OFF

IF "%1" == "dcc" GOTO MAKE_DCC
IF "%1" == "clean" GOTO MAKE_CLEAN
ECHO.
ECHO Usage: MAKEWIN (option)
ECHO.
ECHO Available options are:
ECHO   MAKEWIN dcc   ( To build the Delphi console programs. )
ECHO   MAKEWIN clean ( To clean up temp files )
ECHO.
GOTO END

:MAKE_DCC
IF NOT EXIST ..\src\curlobj.dcu GOTO MAKE_SRC
IF NOT EXIST ..\src\curl_h.dcu GOTO  MAKE_SRC
:MAKE_DEMO
FOR %%F IN ( *.pas ) DO dcc32 -U..\src %%F
GOTO END


:MAKE_SRC
CD ..\src
CALL makewin dcc
CD ..\demo
GOTO MAKE_DEMO


:MAKE_CLEAN
@ECHO ON
IF EXIST *.o   DEL *.o
IF EXIST *.ppu  DEL *.ppu
IF EXIST *.exe  DEL *.exe
IF EXIST *.htm  DEL *.htm
IF EXIST *.bak  DEL *.bak
@ECHO OFF
GOTO END

:END
