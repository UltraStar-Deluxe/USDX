// Comment by eddie:
// The mac port currently also uses the WIN32 define.
// Once I get the beast compiled, linked and running
// I will change this.
// There are some parts where the WIN32 define could not
// be used. I changed the WIN32 to MSWINDOWS.
// So, for Windows-only code use the MSWINDOWS define.

{$IFDEF FPC}
  {$DEFINE CONSOLE}
  {$IFDEF DARWIN}
    {$H+}
    {$R-}
    {$DEFINE WIN32}
    {$DEFINE TRANSLATE}
		{$DEFINE UTF8_FILENAMES}
  {$ENDIF}

  {$DEFINE DLL_CDECL}
  {$DEFINE HasInline}
  {$UNDEF UseSerialPort}
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

  {$DEFINE DLL_STDCALL}
  {$UNDEF UseSerialPort}
{$ENDIF}


{$IF Defined(win32)}
  // include defines but no constants
	{$IFDEF DARWIN}
    {$I config-macosx.inc}
	{$ELSE}
    {$I config-win.inc}
	{$ENDIF}

  // audio config
  {$DEFINE WinAudioLib_BASS}
  {$IFDEF WinAudioLib_BASS}
    {$DEFINE UseBASSPlayback}
    {$DEFINE UseBASSInput}
  {$ELSE}
    {$DEFINE UseFFMpegDecoder}
    //{$DEFINE UsePortaudioPlayback}
    {$DEFINE UseSDLPlayback}
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
      {$IFDEF CONSOLE}
        {$APPTYPE CONSOLE}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF MSWINDOWS}
    {$DEFINE UseMIDIPort}
  {$ENDIF}
{$ELSEIF Defined(Linux)}
  // include defines but no constants
  {$I config-linux.inc}

  // audio config
  {$IFDEF HaveFFMpeg}
    {$DEFINE UseFFMpegDecoder}
    {$IFDEF HavePortaudio}
      //{$DEFINE UsePortaudioPlayback}
      {$DEFINE UseSDLPlayback}
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

