// Comment by eddie:
// The mac port currently also uses the WIN32 define.
// Once I get the beast compiled, linked and running
// I will change this.
// There are some parts where the WIN32 define could not
// be used. I changed the WIN32 to MSWINDOWS.
// So, for Windows-only code use the MSWINDOWS define.

// compiler/IDE dependent config
{$IFDEF FPC}
  {$DEFINE CONSOLE}
  {$IFDEF DARWIN}
    {$H+} // enables usage of AnsiString as String-type 
    {$R-} // disables range-checks
  {$ENDIF}

  // if -dDEBUG is specified on the command-line, FPC uses some default
  // compiler-flags specified in fpc.cfg -> use -dDEBUG_MODE instead
  {$IFDEF DEBUG_MODE}
    {$DEFINE DEBUG}
  {$ENDIF}

  {$DEFINE DLL_CDECL}
  {$DEFINE HasInline}
{$ELSE}
  {$UNDEF CONSOLE}  // Delphi requires a special app type... no thanks ! :)
  // {$DEFINE CONSOLE} // -- use for development only !
  {$DEFINE Delphi}

  // Delphi version numbers (ignore versions released before Delphi 6 as they miss the $IF directive):
  // Delphi 6 (VER140), Delphi 7 (VER150), Delphi 8 (VER160)
  // Delphi 9/2005 (VER170), Delphi 10/2006 (VER180)

  // the inline-procedure directive was introduced with Delphi 2005
  {$IF not (Defined(VER140) or Defined(VER150) or Defined(VER160))}
    {$DEFINE HasInline}
  {$IFEND}
{$ENDIF}


// platform dependent config
{$IF Defined(MSWINDOWS)}
  // include defines but no constants
  {$I config-win.inc}

  {$IFDEF DEBUG}
    {$IFDEF CONSOLE}
      {$APPTYPE CONSOLE}
    {$ENDIF}
  {$ENDIF}

  {$DEFINE HaveBASS}
  {$UNDEF UseSerialPort}
  {$DEFINE UseMIDIPort}
{$ELSEIF Defined(LINUX)}
  // include defines but no constants
  {$I config-linux.inc}
{$ELSEIF Defined(DARWIN)}
  // include defines but no constants
  {$I config-macosx.inc}

  {$DEFINE HaveBASS}
  {$DEFINE DLL_CDECL}
  {$DEFINE WIN32}
  {$DEFINE UTF8_FILENAMES}
{$IFEND}

// audio config
{$IF Defined(HaveBASS)}
  {$DEFINE UseBASSPlayback}
  {$DEFINE UseBASSInput}
{$ELSEIF Defined(HavePortaudio)}
  //{$DEFINE UsePortaudioPlayback}
  {$DEFINE UseSDLPlayback}
  {$DEFINE UsePortaudioInput}
  {$IFDEF HavePortmixer}
    {$DEFINE UsePortmixer}
  {$ENDIF}
{$IFEND}

// ffmpeg config
{$IFDEF HaveFFMpeg}
  {$DEFINE UseFFMpegDecoder}
  {$DEFINE UseFFMpegVideo}
  {$IFDEF HaveSWScale}
    {$DEFINE UseSWScale}
  {$ENDIF}
{$ENDIF}

// projectM config
{$IF Defined(HaveProjectM)}
  {$DEFINE UseProjectM}
{$IFEND}

// specify some useful defines

{$IF Defined(UseFFMpegVideo) or Defined(UseFFMpegDecoder)}
  {$DEFINE UseFFMpeg}
{$IFEND}

{$IF Defined(UseBASSInput) or Defined(UseBASSPlayback)}
  {$DEFINE UseBASS}
{$IFEND}

{$IF Defined(UsePortaudioInput) or Defined(UsePortaudioPlayback)}
  {$DEFINE UsePortaudio}
{$IFEND}

