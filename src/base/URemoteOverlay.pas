{* UltraStar Deluxe - Karaoke Game
 *
 * Lightweight global display overlay for USDX Remote room status.
 *}

unit URemoteOverlay;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

procedure DrawRemoteRoomOverlay;

implementation

uses
  dglOpenGL,
  SysUtils,
  TextGL,
  URemoteBridgeIPC;

procedure DrawRemoteRoomOverlay;
var
  Connected: boolean;
  DisplayCode: UTF8String;
  StatusSeq: integer;
  ConnectedPlayers: integer;
  Text: UTF8String;
  TextW: real;
begin
  RemoteBridgeIPC.GetRoomStatus(Connected, DisplayCode, StatusSeq, ConnectedPlayers);
  if (not Connected) then
    Exit;

  if (DisplayCode = '') then
    Text := 'usdx.at connecting'
  else
    Text := 'usdx.at/' + DisplayCode + '  ' + IntToStr(ConnectedPlayers) + ' player';
  if (ConnectedPlayers <> 1) and (DisplayCode <> '') then
    Text := Text + 's';

  SetFontFamily(0);
  SetFontStyle(ftRegular);
  SetFontSize(20);
  SetFontItalic(false);
  SetFontReflection(false, 0);
  glColor4f(1, 1, 1, 1);
  TextW := glTextWidth(Text);
  SetFontPos(800 - TextW - 10, 6);
  glPrint(Text);
  glColor4f(1, 1, 1, 1);
end;

end.
