// prevent pasdoc from parsing this file
{$IFNDEF PASDOC}

// compiler/IDE dependent config
{$IFDEF FPC}
  {$H+} // use AnsiString instead of ShortString as String-type (default in Delphi)

  // if -dDEBUG is specified on the command-line, FPC uses some default
  // compiler-flags specified in fpc.cfg -> use -dDEBUG_MODE instead
  {$IFDEF DEBUG_MODE}
    {$DEFINE DEBUG}
  {$ENDIF}

  {$DEFINE HasInline}
{$ELSE}
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

  // enable debug-mode. For development only!
  {.$DEFINE DEBUG}
  {$IFDEF DEBUG}
    // windows apps are either GUI- or console-apps. Console-apps will open
    // an additional console-window for output. For development only!
    {$DEFINE CONSOLE}
  {$ENDIF}

  {$DEFINE HaveBASS}
  {$DEFINE UseMIDIPort}
{$ELSEIF Defined(DARWIN)}
  // include defines but no constants
  {$I config-darwin.inc}

  // enable debug-mode. For development only!
  {.$DEFINE DEBUG}
  {$DEFINE CONSOLE}
  {.$DEFINE HaveBASS}
  {$DEFINE UTF8_FILENAMES}
{$ELSEIF Defined(UNIX)}
  // include defines but no constants
  {$IF Defined(FREEBSD)}
    {$I config-freebsd.inc}
  {$ELSEIF Defined(Linux)}
    {$I config-linux.inc}
  {$IFEND}

  // use "configure --enable-debug", "make debug" or
  // the command-line parameter "-debug" instead of defining DEBUG directly
  {.$DEFINE DEBUG}
  // linux apps are always console-apps so leave this defined.
  {$DEFINE CONSOLE}
{$IFEND}

// audio config
{$IF Defined(HaveBASS)}
  {$DEFINE UseBASSPlayback}
  {$DEFINE UseBASSDecoder}
  {$DEFINE UseBASSInput}
{$ELSEIF Defined(HavePortaudio)}
  {$DEFINE UseSDLPlayback}
  {.$DEFINE UsePortaudioPlayback}
  {$DEFINE UsePortaudioInput}
  {$IFDEF HavePortmixer}
    {$DEFINE UsePortmixer}
  {$ENDIF}
{$IFEND}

// ffmpeg config
{$IFDEF HaveFFmpeg}
  {$DEFINE UseFFmpegDecoder}
  {$DEFINE UseFFmpegResample}
  {$DEFINE UseFFmpegVideo}
  {$IFDEF HaveSWScale}
    {$DEFINE UseSWScale}
  {$ENDIF}
{$ENDIF}

{$IFDEF HaveLibsamplerate}
  {$DEFINE UseSRCResample}
{$ENDIF}

// projectM config
{$IF Defined(HaveProjectM)}
  {$DEFINE UseProjectM}
{$IFEND}

// specify some useful defines

{$IF Defined(UseFFmpegVideo) or Defined(UseFFmpegDecoder)}
  {$DEFINE UseFFmpeg}
{$IFEND}

{$IF Defined(UseBASSInput) or Defined(UseBASSPlayback) or Defined(UseBASSDecoder)}
  {$DEFINE UseBASS}
{$IFEND}

{$IF Defined(UsePortaudioInput) or Defined(UsePortaudioPlayback)}
  {$DEFINE UsePortaudio}
{$IFEND}

{$ENDIF PASDOC}