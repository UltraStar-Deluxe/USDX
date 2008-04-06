program UltraStar;

{$MODE DELPHI}
{$I switches.inc}

uses
  {$ifdef unix}            // http://wiki.lazarus.freepascal.org/Multithreaded_Application_Tutorial
    cthreads,              // THIS MUST be the first used unit !!
    cwstring,
  {$endif}
  {$ifdef MSWINDOWS}       // do not initialize the widgets in linux -> see below
  {$ifdef LCL}
    Interfaces,            // Initialize Lazarus LCL (necessary for usage of LCLIntf, etc.)
                           // WARNING: in linux (with the gtk2 interface) this will change the locale
                           //   settings like LC_NUMERIC (which cannot be reverted, can it? At least I have not managed to do so.).
                           //   As a result external libs like projectM might not be able to parse
                           //   floating-point values anymore (because ',' might be used as decimal-seperator in atof() or strtod()).
                           //   As a result, projectM does not work anymore and crashes because of access violations.
                           // Not initializing Interfaces crashes USDX when widgets should be shown, as done with
                           // MessageBox(). In addition LCL assigns itself as a Signal-handler for almost every
                           // exception (FPE, SEGV, etc.) and wants to show a message-box with some info on
                           // the exception that occured. It will crash a second time because of the missing
                           // widget-set binding (error in winapi.pp).
                           //
                           // Removing the lazarus widget stuff seems to be the only solution but even if just
                           // the LCLIntf unit is included, lazarus catches signals and tries to show the exception-info.
                           // -> So we have to remove all the lazarus stuff except LResources (hope this doesn't influence the signal-handler too)
                           // We can leave the windows-only LCL-stuff in DirWatch and the Midi-classes.
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
