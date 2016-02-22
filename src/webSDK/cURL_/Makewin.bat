@ECHO OFF

IF "%1" == "src" GOTO MAKE_SRC
IF "%1" == "gui" GOTO MAKE_GUI
IF "%1" == "demo" GOTO MAKE_DEMO
IF "%1" == "all" GOTO MAKE_ALL
IF "%1" == "clean" GOTO MAKE_CLEAN
ECHO.
ECHO Usage: MAKEWIN (option)
ECHO.
ECHO Available options are:
ECHO  MAKEWIN  src   ( To make the Delphi bindings & component. )
ECHO  MAKEWIN  gui   ( To build the Delphi GUI demo. )
ECHO  MAKEWIN  demo  ( To build the Delphi console programs. )
ECHO  MAKEWIN  all   ( To build everything. )
ECHO  MAKEWIN  clean ( To clean up temp files )
ECHO.
ECHO  Or choose an option now:
ECHO.  Src, Gui, Demo, All, Clean, None...
ECHO.
CHOICE /c:sgdacn "  "
IF ERRORLEVEL 6 GOTO MAKE_NONE
IF ERRORLEVEL 5 GOTO MAKE_CLEAN
IF ERRORLEVEL 4 GOTO MAKE_ALL
IF ERRORLEVEL 3 GOTO MAKE_DEMO
IF ERRORLEVEL 2 GOTO MAKE_GUI
IF ERRORLEVEL 1 GOTO MAKE_SRC
IF ERRORLEVEL 0 GOTO MAKE_NONE
GOTO END



:MAKE_SRC
CD .\src\
CALL makewin dcc
CD ..
GOTO END


:MAKE_DEMO
CD .\demo\
CALL makewin dcc
CD ..
GOTO END


:MAKE_GUI
CD .\gui\
CALL makewin dcc
CD ..
GOTO END

:MAKE_ALL
CD .\demo\
CALL makewin dcc
CD ..\gui\
CALL makewin dcc
GOTO END


:MAKE_CLEAN
@ECHO ON
CD .\src\
CALL makewin clean
CD ..\demo\
CALL makewin clean
CD ..\gui\
CALL makewin clean
@ECHO OFF
GOTO END

:MAKE_NONE
ECHO Operation cancelled.
GOTO END

:END
ECHO Done.
