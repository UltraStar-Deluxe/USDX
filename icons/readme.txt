Resource files (.rc/.res) are needed for MS Windows builds only.

The .res file is the compiled version of .rc and appended to the
executable. It is just used to provide an icon for the executable
that is shown in the explorer.

Delphi does the compilation of the .rc to .res file automatically.
FPC needs windres (delivered with FPC) for ths purpose but windres
must be started separately.

You can manually run the compilation process with either
	rccompile-delphi.bat or
	rccompile-fpc.bat
depending on the available compiler (delphi/windres files are compatible).

If FPC is used, the path to the FPC bin-dir might have to be adjusted,
the default is "PATH=C:\Programme\lazarus\fpc\2.2.0\bin\i386-win32\".

To avoid the need to run the resource-compiler, a pre-compiled .res-file
was added to SVN. Please commit the res-file if the rc-file was changed.