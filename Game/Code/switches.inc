{$IFDEF FPC}
  {$UNDEF UseSerialPort}
  {$UNDEF UseMIDIPort}
{$ELSE}
  {$UNDEF UseSerialPort}
  {$DEFINE UseMIDIPort}
{$ENDIF}  

{$IFDEF win32}
  {$DEFINE UseBASS}
{$ELSE}
  {$UNDEF  UseBASS}
{$ENDIF}
