{$IFDEF FPC}
  {$UNDEF UseSerialPort}
  {$UNDEF UseMIDIPort}
{$ELSE}
  {$DEFINE UseSerialPort}
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