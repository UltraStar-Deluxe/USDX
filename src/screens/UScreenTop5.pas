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
 * $URL$
 * $Id$
 *}

unit UScreenTop5;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  SDL,
  UDisplay,
  UMenu,
  UMusic,
  USongs,
  UThemes;

type
  TScreenTop5 = class(TMenu)
    public
      TextLevel:       integer;
      TextArtistTitle: integer;
      DifficultyShow:  integer;

      StaticNumber:    array[1..5] of integer;
      TextNumber:      array[1..5] of integer;
      TextName:        array[1..5] of integer;
      TextScore:       array[1..5] of integer;
      TextDate:        array[1..5] of integer;

      Fadeout:         boolean;

      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      function ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean; override;
      procedure OnShow; override;
      procedure DrawScores(difficulty: integer);
      function Draw: boolean; override;
  end;

implementation

uses
  UDataBase,
  UGraphic,
  UMain,
  UIni,
  UNote,
  UUnicodeUtils;

function TScreenTop5.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
  if PressedDown then
  begin
    // check normal keys
    case UCS4UpperCase(CharCode) of
      Ord('Q'):
        begin
          Result := false;
          Exit;
        end;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE,
      SDLK_RETURN:
        begin
          if (not Fadeout) then
          begin
            FadeTo(@ScreenSong);
            Fadeout := true;
          end;
        end;
      SDLK_RIGHT:
        begin
          inc(DifficultyShow);
          if (DifficultyShow>2) then
            DifficultyShow:=0;
          DrawScores(DifficultyShow);
        end;
      SDLK_LEFT:
        begin
          dec(DifficultyShow);
          if (DifficultyShow<0) then
            DifficultyShow:=2;
          DrawScores(DifficultyShow);
        end;
      SDLK_UP:
        begin
          inc(DifficultyShow);
          if (DifficultyShow>2) then
            DifficultyShow:=0;
          DrawScores(DifficultyShow);
        end;
      SDLK_DOWN:
        begin
          dec(DifficultyShow);
          if (DifficultyShow<0) then
            DifficultyShow:=2;
          DrawScores(DifficultyShow);
        end;
      SDLK_SYSREQ:
        begin
          Display.SaveScreenShot;
        end;
    end;
  end;
end;

function TScreenTop5.ParseMouse(MouseButton: integer;
                                BtnDown: boolean;
				X, Y: integer): boolean;
begin
  Result := true;
  if (MouseButton = SDL_BUTTON_LEFT) and BtnDown then
    //left-click anywhere sends return
    ParseInput(SDLK_RETURN, 0, true);
end;

constructor TScreenTop5.Create;
var
  I: integer;
begin
  inherited Create;

  LoadFromTheme(Theme.Top5);

  TextLevel       := AddText(Theme.Top5.TextLevel);
  TextArtistTitle := AddText(Theme.Top5.TextArtistTitle);

  for I := 0 to 4 do
  begin
    StaticNumber[I+1] := AddStatic(Theme.Top5.StaticNumber[I]);
    TextNumber[I+1]   := AddText  (Theme.Top5.TextNumber[I]);
    TextName[I+1]     := AddText  (Theme.Top5.TextName[I]);
    TextScore[I+1]    := AddText  (Theme.Top5.TextScore[I]);
    TextDate[I+1]     := AddText  (Theme.Top5.TextDate[I]);
  end;

end;

procedure TScreenTop5.OnShow;
var
  I:    integer;
  PMax: integer;
  sung: boolean; //score added? otherwise in wasn't sung!
begin
  inherited;

  sung := false;
  Fadeout := false;
  DifficultyShow := Ini.Difficulty;

  //ReadScore(CurrentSong);

  PMax := Ini.Players;
  if PMax = 4 then
    PMax := 5;
  for I := 0 to PMax do
  begin
    if (Round(Player[I].ScoreTotalInt) > 0) and (ScreenSing.SungToEnd) then
    begin
      DataBase.AddScore(CurrentSong, Ini.Difficulty, Ini.Name[I], Round(Player[I].ScoreTotalInt));
      sung:=true;
    end;
  end;

  if sung then
    DataBase.WriteScore(CurrentSong);
  DataBase.ReadScore(CurrentSong);

  Text[TextArtistTitle].Text := CurrentSong.Artist + ' - ' + CurrentSong.Title;

  for I := 1 to Length(CurrentSong.Score[Ini.Difficulty]) do
  begin
    Static[StaticNumber[I]].Visible := true;
    Text[TextNumber[I]].Visible := true;
    Text[TextName[I]].Visible := true;
    Text[TextScore[I]].Visible := true;
    Text[TextDate[I]].Visible := true;

    Text[TextName[I]].Text := CurrentSong.Score[Ini.Difficulty, I-1].Name;
    Text[TextScore[I]].Text := IntToStr(CurrentSong.Score[Ini.Difficulty, I-1].Score);
    Text[TextDate[I]].Text := CurrentSong.Score[Ini.Difficulty, I-1].Date;
  end;

  for I := Length(CurrentSong.Score[Ini.Difficulty]) + 1 to 5 do
  begin
    Static[StaticNumber[I]].Visible := false;
    Text[TextNumber[I]].Visible := false;
    Text[TextName[I]].Visible := false;
    Text[TextScore[I]].Visible := false;
    Text[TextDate[I]].Visible := false;
  end;

  Text[TextLevel].Text := IDifficulty[Ini.Difficulty];
end;

procedure TScreenTop5.DrawScores(difficulty: integer);
var
  I:    integer;
begin
  for I := 1 to Length(CurrentSong.Score[difficulty]) do
  begin
    Static[StaticNumber[I]].Visible := true;
    Text[TextNumber[I]].Visible := true;
    Text[TextName[I]].Visible := true;
    Text[TextScore[I]].Visible := true;
    Text[TextDate[I]].Visible := true;

    Text[TextName[I]].Text := CurrentSong.Score[difficulty, I-1].Name;
    Text[TextScore[I]].Text := IntToStr(CurrentSong.Score[difficulty, I-1].Score);
    Text[TextDate[I]].Text := CurrentSong.Score[difficulty, I-1].Date;
  end;

  for I := Length(CurrentSong.Score[difficulty]) + 1 to 5 do
  begin
    Static[StaticNumber[I]].Visible := false;
    Text[TextNumber[I]].Visible := false;
    Text[TextName[I]].Visible := false;
    Text[TextScore[I]].Visible := false;
    Text[TextDate[I]].Visible := false;
  end;

  Text[TextLevel].Text := IDifficulty[difficulty];
end;

function TScreenTop5.Draw: boolean;
//var
{
  Min:     real;
  Max:     real;
  Factor:  real;
  Factor2: real;

  Item:    integer;
  P:       integer;
  C:       integer;
}
begin
  // Singstar - let it be...... with 6 statics
(*
  if PlayersPlay = 6 then
  begin
    for Item := 4 to 6 do
    begin
      if ScreenAct = 1 then P := Item-4;
      if ScreenAct = 2 then P := Item-1;

      FillPlayer(Item, P);
{
      if ScreenAct = 1 then
      begin
        LoadColor(
          Static[StaticBoxLightest[Item]].Texture.ColR,
          Static[StaticBoxLightest[Item]].Texture.ColG,
          Static[StaticBoxLightest[Item]].Texture.ColB,
          'P1Dark');
      end;

      if ScreenAct = 2 then
      begin
        LoadColor(
          Static[StaticBoxLightest[Item]].Texture.ColR,
          Static[StaticBoxLightest[Item]].Texture.ColG,
          Static[StaticBoxLightest[Item]].Texture.ColB,
          'P4Dark');
      end;
}
    end;
  end;
*)

  Result := inherited Draw;
end;

end.
