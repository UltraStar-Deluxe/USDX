{* UltraStar Deluxe - Karaoke Game
 *
 * UltraStar Deluxe is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenLoading.pas $
 * $Id: UScreenLoading.pas 1939 2009-11-09 00:27:55Z s_alexander $
 *}

unit UScreenLoading;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu,
  UMenuStatic,
  SysUtils,
  UThemes,
  dglOpenGL,
  sdl2;

type
  TScreenLoading = class(TMenu)
    private
      DiscoveryBarBaseIndex: integer;
      DiscoveryBarFillIndex: integer;
      LoadingBarBaseIndex: integer;
      LoadingBarFillIndex: integer;
      StatusTextIndex: integer;
      StatusTextBase: UTF8String;
      DiscoveryBarAlpha: real;
      LoadingBarAlpha: real;
      LastRedrawTicks: cardinal;
      function ClampProgress(Current, Total: integer): real;
      function AddProgressOverlay(SourceBarIndex: integer; Alpha: real): integer;
      procedure UpdateBar(BaseBarIndex, FillBarIndex: integer; Progress, BaseAlpha, FillAlpha: real);
      procedure UpdateStatus(const Value: UTF8String);
    public
      Fadeout: boolean;
      TextDescription: integer;

      constructor Create; override;
      procedure OnShow; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure SetDiscoveryProgress(Current, Total, SongsFound: integer);
      procedure SetSongLoadingProgress(Current, Total: integer);
      procedure RefreshProgress(Force: boolean = false);
  end;

implementation

uses
  UGraphic,
  UDisplay,
  UTime;

function TScreenLoading.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
end;

constructor TScreenLoading.Create;
begin
  inherited Create;

  LoadFromTheme(Theme.Loading);

  if Length(Statics) > 0 then
  begin
    DiscoveryBarBaseIndex := 0;
    DiscoveryBarAlpha := Statics[DiscoveryBarBaseIndex].Texture.Alpha;
    DiscoveryBarFillIndex := AddProgressOverlay(DiscoveryBarBaseIndex, DiscoveryBarAlpha);
  end
  else
  begin
    DiscoveryBarBaseIndex := -1;
    DiscoveryBarFillIndex := -1;
    DiscoveryBarAlpha := 1;
  end;

  if Length(Statics) > 1 then
  begin
    LoadingBarBaseIndex := 1;
    LoadingBarAlpha := Statics[LoadingBarBaseIndex].Texture.Alpha;
    LoadingBarFillIndex := AddProgressOverlay(LoadingBarBaseIndex, LoadingBarAlpha);
  end
  else
  begin
    LoadingBarBaseIndex := -1;
    LoadingBarFillIndex := -1;
    LoadingBarAlpha := 1;
  end;

  if Length(Text) > 0 then
  begin
    StatusTextIndex := 0;
    StatusTextBase := Text[0].Text;
  end
  else
  begin
    StatusTextIndex := -1;
    StatusTextBase := '';
  end;

  LastRedrawTicks := 0;
  UpdateBar(DiscoveryBarBaseIndex, DiscoveryBarFillIndex, 0, DiscoveryBarAlpha * 0.5, DiscoveryBarAlpha);
  UpdateBar(LoadingBarBaseIndex, LoadingBarFillIndex, 0, LoadingBarAlpha * 0.5, LoadingBarAlpha);
  UpdateStatus('');
  RefreshProgress(true);
  Fadeout := false;
end;

procedure TScreenLoading.OnShow;
begin
  inherited;
end;

function TScreenLoading.ClampProgress(Current, Total: integer): real;
begin
  if Total <= 0 then
    Result := 0
  else
    Result := Current / Total;

  if Result < 0 then
    Result := 0
  else if Result > 1 then
    Result := 1;
end;

function TScreenLoading.AddProgressOverlay(SourceBarIndex: integer; Alpha: real): integer;
var
  StaticNum: integer;
begin
  if (SourceBarIndex < 0) or (SourceBarIndex >= Length(Statics)) then
    Exit(-1);

  StaticNum := Length(Statics);
  SetLength(Statics, StaticNum + 1);
  Statics[StaticNum] := TStatic.Create(Statics[SourceBarIndex].Texture);
  Statics[StaticNum].Texture.Alpha := Alpha;
  Statics[StaticNum].Texture.ScaleW := 0;
  Statics[StaticNum].Visible := Statics[SourceBarIndex].Visible;
  Statics[StaticNum].Reflection := Statics[SourceBarIndex].Reflection;
  Statics[StaticNum].ReflectionSpacing := Statics[SourceBarIndex].ReflectionSpacing;
  Result := StaticNum;
end;

procedure TScreenLoading.UpdateBar(BaseBarIndex, FillBarIndex: integer; Progress, BaseAlpha, FillAlpha: real);
begin
  if (BaseBarIndex >= 0) and (BaseBarIndex < Length(Statics)) then
  begin
    Statics[BaseBarIndex].Texture.ScaleW := 1;
    Statics[BaseBarIndex].Texture.Alpha := BaseAlpha;
  end;

  if (FillBarIndex >= 0) and (FillBarIndex < Length(Statics)) then
  begin
    Statics[FillBarIndex].Texture.ScaleW := Progress;
    Statics[FillBarIndex].Texture.Alpha := FillAlpha;
  end;
end;

procedure TScreenLoading.UpdateStatus(const Value: UTF8String);
begin
  if (StatusTextIndex >= 0) and (StatusTextIndex < Length(Text)) then
  begin
    if Value = '' then
      Text[StatusTextIndex].Text := StatusTextBase
    else
      Text[StatusTextIndex].Text := StatusTextBase + ' ' + Value;
  end;
end;

procedure TScreenLoading.SetDiscoveryProgress(Current, Total, SongsFound: integer);
begin
  UpdateBar(DiscoveryBarBaseIndex, DiscoveryBarFillIndex, ClampProgress(Current, Total), DiscoveryBarAlpha * 0.5, DiscoveryBarAlpha);
  UpdateStatus(Format('0 / %5d', [SongsFound]));
  RefreshProgress;
end;

procedure TScreenLoading.SetSongLoadingProgress(Current, Total: integer);
begin
  UpdateBar(LoadingBarBaseIndex, LoadingBarFillIndex, ClampProgress(Current, Total), LoadingBarAlpha * 0.5, LoadingBarAlpha);
  if Total > 0 then
    UpdateStatus(Format('%5d / %5d', [Current, Total]))
  else
    UpdateStatus('');
  RefreshProgress;
end;

procedure TScreenLoading.RefreshProgress(Force: boolean);
var
  NowTicks: cardinal;
begin
  if (Display = nil) or (Display.CurrentScreen <> @ScreenLoading) then
    Exit;

  NowTicks := SDL_GetTicks;
  if not Force and (LastRedrawTicks <> 0) and (NowTicks - LastRedrawTicks < 100) then
    Exit;

  Self.Draw;
  Display.Draw;
  SwapBuffers;
  LastRedrawTicks := NowTicks;
end;

end.
