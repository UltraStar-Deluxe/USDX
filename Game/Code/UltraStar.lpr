program UltraStar;

{$DEFINE TRANSLATE}
{$MODE DELPHI}
{$I switches.inc}

uses
  {$ifdef unix}            // http://wiki.lazarus.freepascal.org/Multithreaded_Application_Tutorial
    cthreads,              // THIS MUST be the first used unit !!
  {$endif}
  {$ifdef MSWINDOWS}
  {$ifdef LCL}
  Interfaces,              // Initialize Lazarus LCL (necessary for usage of LCLIntf, etc.)
  {$endif}
  {$endif}

  {$I UltraStar.dpr}

  // ***************************************************************************
  //
  //                         Developers PLEASE NOTE !!!!!!!
  //
  //  As of september 2007, I am working towards porting Ultrastar-DX to run
  //  on Linux.  I will be modifiying the source to make it compile in lazarus
  //  on windows & linux and I will make sure that it compiles in delphi still
  //  To help me in this endevour, please can you make a point of remembering
  //  that linux is CASE SENSATIVE, and file / unit names must be as per
  //  the filename exactly.
  //
  //  EG :  opengl12.pas  must not be OpenGL in the uses cluase.
  //
  //  thanks for your help...
  //
  // ***************************************************************************

  // Interesting stuff... :)
  // http://burningsmell.org/sdl_audioin/

begin
  main();
end.
