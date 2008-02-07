// Comment by eddie:
// The mac port currently also uses the WIN32 define.
// Once I get the beast compiled, linked and running
// I will change this.
// There are some parts where the WIN32 define could not
// be used. I changed the WIN32 to MSWINDOWS.
// So, for Windows-only code use the MSWINDOWS define.

{$IFDEF FPC}
  {$IFDEF DARWIN}
    {$H+}
    {$R-}
    {$DEFINE WIN32}
    {$DEFINE TRANSLATE}
		{$DEFINE UTF8_FILENAMES}
  {$ELSE}
    {$DEFINE LAZARUS}
    {$IFNDEF LCL}
      // LCL should be defined already
      {$MESSAGE Warn 'LCL undefined, compile with "fpc -dLCL ..."!'}
      {$DEFINE LCL}
    {$ENDIF}
  {$ENDIF}

  {$DEFINE DLL_CDECL}
  {$UNDEF UseSerialPort}
  {$UNDEF UseMIDIPort}
{$ELSE}
  {$DEFINE Delphi}
  {$DEFINE DLL_STDCALL}
  {$UNDEF UseSerialPort}
  {$DEFINE UseMIDIPort}
{$ENDIF}


{$IF Defined(win32)}
  // include defines but no constants
  {$I config-win.inc}

  // audio config
  {$DEFINE WinAudioLib_BASS}
  {$IFDEF WinAudioLib_BASS}
    {$DEFINE UseBASSPlayback}
    {$DEFINE UseBASSInput}
  {$ELSE}
    {$DEFINE UseFFMpegDecoder}
    {$DEFINE HaveAvcodecDecodeAudio2}
    {$DEFINE UsePortaudioPlayback}
    {$DEFINE UsePortaudioInput}
    {$DEFINE UsePortmixer}
  {$ENDIF}
  {$UNDEF WinAudioLib_BASS}

  // video config
  {$IFDEF HaveFFMpeg}
    {$DEFINE UseFFMpegVideo}
    {$IFDEF HaveSWScale}
      {$DEFINE UseSWScale}
    {$ENDIF}
  {$ENDIF}

  // misc defines

  {$IF Defined(HaveProjectM_0_9)}
    {$DEFINE UseProjectM_0_9}
  {$ELSEIF Defined(HaveProjectM_1_0_PLUS)}
    {$DEFINE UseProjectM_1_0_PLUS}
  {$IFEND}

  {$IFDEF DEBUG}
    {$IFNDEF DARWIN}
      {$APPTYPE CONSOLE}
	{$ENDIF}
  {$ENDIF}
{$ELSEIF Defined(Linux)}
  // include defines but no constants
  {$I config-linux.inc}

  // audio config
  {$IFDEF HaveFFMpeg}
    {$DEFINE UseFFMpegDecoder}
    {$IFDEF HavePortaudio}
      {$DEFINE UsePortaudioPlayback}
      {$DEFINE UsePortaudioInput}
    {$ENDIF}
  {$ENDIF}

  // video config
  {$IFDEF HaveFFMpeg}
    {$DEFINE UseFFMpegVideo}
    {$IFDEF HaveSWScale}
      {$DEFINE UseSWScale}
    {$ENDIF}
  {$ENDIF}

  // misc defines
  {$IFDEF HaveProjectM}
    // this causes trouble at the moment
    //{$DEFINE UseProjectM_0_9}
  {$ENDIF}
{$IFEND}

{$IF Defined(UseFFMpegVideo) or Defined(UseFFMpegDecoder)}
  {$DEFINE UseFFMpeg}
{$IFEND}

{$IF Defined(UseBASSInput) or Defined(UseBASSPlayback)}
  {$DEFINE UseBASS}
{$IFEND}

{$IF Defined(UsePortaudioInput) or Defined(UsePortaudioPlayback)}
  {$DEFINE UsePortaudio}
{$IFEND}

{$IF Defined(UseProjectM_0_9) or Defined(UseProjectM_1_0_PLUS)}
  {$DEFINE UseProjectM}
{$IFEND}

