{$DEFINE DEBUG} // to-do : Remove b4 release

{$IFDEF FPC}
  {$UNDEF UseSerialPort}
  {$UNDEF UseMIDIPort}
{$ELSE}
  {$UNDEF UseSerialPort}
  {$DEFINE UseMIDIPort}
{$ENDIF}

{$IFDEF win32}
  {$DEFINE UseBASS}
  {$IFDEF DEBUG}
    {$APPTYPE CONSOLE}
  {$ENDIF}
{$ELSE}
  {$UNDEF  UseBASS}
{$ENDIF}

