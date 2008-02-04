{$DEFINE DEBUG} // to-do : Remove b4 release

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


{$IFDEF win32}
  {$DEFINE UseBASSPlayback}
  {$DEFINE UseBASSInput}

  //{$DEFINE UseFFMpegDecoder}
  //{$DEFINE UsePortaudioPlayback}
  //{$DEFINE UsePortaudioInput}
  //{$DEFINE UsePortmixer}
  
  {$DEFINE UseProjectM_0_9}
  //{$DEFINE UseProjectM_1_0}

  {$IFDEF DEBUG}
    {$IFNDEF DARWIN}
      {$APPTYPE CONSOLE}
	{$ENDIF}
  {$ENDIF}
{$ELSE}
  {$DEFINE UseFFMpegAudio}
  {$DEFINE UsePortaudio}
  {$DEFINE UseProjectM_0_9}
{$ENDIF}

{$IF Defined(UseBASSInput) or Defined(UseBASSPlayback)}
  {$DEFINE UseBASS}
{$IFEND}

{$IF Defined(UsePortaudioInput) or Defined(UsePortaudioPlayback)}
  {$DEFINE UsePortaudio}
{$IFEND}

{$IF Defined(UseProjectM_0_9) or Defined(UseProjectM_1_0)}
  {$DEFINE UseProjectM}
{$IFEND}

// I had to add these (jay)... but maybe henny has some way that the config script is meant to do this ?
// Ill need to check with him.
//{$DEFINE FPC_V220}
//{$DEFINE LAZARUS_V0924}
