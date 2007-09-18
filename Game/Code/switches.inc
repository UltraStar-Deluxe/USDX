{$IFDEF FPC}
  {$UNDEF UseSerialPort}
  {$UNDEF UseMIDIPort}
{$ELSE}
  {$DEFINE UseSerialPort}
  {$DEFINE UseMIDIPort}
{$ENDIF}  
