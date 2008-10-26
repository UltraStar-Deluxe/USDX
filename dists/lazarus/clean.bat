@ECHO OFF
set OBJ_PATH=%1
mkdir %OBJ_PATH%
del %OBJ_PATH%\*.o
del %OBJ_PATH%\*.ppu
del %OBJ_PATH%\*.a
del %OBJ_PATH%\*.rst
del %OBJ_PATH%\*.compiled
