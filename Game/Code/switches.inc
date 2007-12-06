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
  {$ELSE}
    {$DEFINE LAZARUS}
  {$ENDIF}

//  {$MODE DELPHI}  // JB - This is not allowed by the free pascal compiler for some reason ( At least on linux )

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
  {$DEFINE UseBASSOutput}
  {$DEFINE UseBASSInput}

  //{$DEFINE UsePortaudio}
  //{$DEFINE UsePortmixer}
  
  {$DEFINE UseProjectM}
  
  {$IFDEF DEBUG}
    {$IFNDEF DARWIN}
      {$APPTYPE CONSOLE}
	{$ENDIF}
  {$ENDIF}
{$ELSE}
  //{$DEFINE FFMpegAudio}
//  {$DEFINE UsePortaudio}
  {$DEFINE UseProjectM}
{$ENDIF}

{$IF Defined(UseBASSInput) or Defined(UseBASSOutput)}
  {$DEFINE UseBASS}
{$IFEND}
