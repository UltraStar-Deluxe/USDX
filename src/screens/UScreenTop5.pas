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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/screens/UScreenTop5.pas $
 * $Id: UScreenTop5.pas 2548 2010-06-18 11:00:21Z whiteshark0 $
 *}

unit UScreenTop5;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UDisplay,
  UMenu,
  UMusic,
  USongs,
  UThemes,
  sdl2,
  SysUtils;

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

const
  ID='ID_024';   //for help system

implementation

uses
  UDataBase,
  UGraphic,
  UHelp,
  ULog,
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
    case PressedKey of
      SDLK_Q:
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
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
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
  sung: boolean; //score added? otherwise in wasn't sung!
  Report: string;
  Name1, Name2:   UTF8String;
  Score1, Score2: Integer;
  CombinedName:   UTF8String;
  CombinedScore:  Integer;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenTop5');

  sung := false;
  Fadeout := false;
  DifficultyShow := Ini.PlayerLevel[0];

  if ScreenSing.SungToEnd then
  begin
    if not(CurrentSong.isDuet) then
    begin
      for I := 0 to PlayersPlay - 1 do
      begin
        Score1 := Round(Player[I].ScoreTotalInt);
        if Score1 > 0 then
        begin
          DataBase.AddScore(CurrentSong, Ini.PlayerLevel[I], 0, Ini.Name[I], Score1);
          sung := True;
        end;
      end;
    end
    else if (Ini.DuetScores > 0) then
    begin
      I := 0;
      while I < PlayersPlay - 1 do
      begin
        Name1  := Ini.Name[I];
        Name2  := Ini.Name[I+1];
        Score1 := Round(Player[I].ScoreTotalInt);
        Score2 := Round(Player[I+1].ScoreTotalInt);

        if (Ini.DuetScores = 1) or (Ini.DuetScores = 3) then
        begin
          if Score1 > 0 then
          begin
            DataBase.AddScore(CurrentSong, Ini.PlayerLevel[I], 1, Name1, Score1);
            sung := True;
          end;
          if Score2 > 0 then
          begin
            DataBase.AddScore(CurrentSong, Ini.PlayerLevel[I+1], 2, Name2, Score2);
            sung := True;
          end;
        end;

        if ((Ini.DuetScores = 2) or (Ini.DuetScores = 3)) and (Ini.PlayerLevel[I] = Ini.PlayerLevel[I+1]) then
        begin
          CombinedName := Format('%s & %s', [Name1, Name2]);

          CombinedScore := (Score1 + Score2) div 2;
          if CombinedScore > 0 then
          begin
            DataBase.AddScore(CurrentSong, Ini.PlayerLevel[I], 3, CombinedName, CombinedScore);
            sung := True;
          end;
        end;

        Inc(I, 2);
      end;
    end;
  end;  // if SungToEnd

  try
    if sung then
    begin
       DataBase.WriteScore(CurrentSong);
    end;
    DataBase.ReadScore(CurrentSong);
  except
    on E : Exception do
    begin
      Report := 'Writing or reading songscore failed in Top-5-creen. Faulty database file?' + LineEnding +
      'Stacktrace:' + LineEnding;
      if E <> nil then
      begin
	Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
	'Message: ' + E.Message + LineEnding;
      end;
      Report := Report + BackTraceStrFunc(ExceptAddr);
      for I := 0 to ExceptFrameCount - 1 do
      begin
	Report := Report + LineEnding + BackTraceStrFunc(ExceptFrames[I]);
      end;
      Log.LogWarn(Report, 'UScreenTop5.OnShow');
    end;
  end;

  Text[TextArtistTitle].Text := CurrentSong.Artist + ' - ' + CurrentSong.Title;

  for I := 1 to Length(CurrentSong.Score[Ini.PlayerLevel[0]]) do
  begin
    Statics[StaticNumber[I]].Visible := true;
    Text[TextNumber[I]].Visible := true;
    Text[TextName[I]].Visible := true;
    Text[TextScore[I]].Visible := true;
    Text[TextDate[I]].Visible := true;

    Text[TextName[I]].Text := CurrentSong.Score[Ini.PlayerLevel[0], I-1].Name;
    if (CurrentSong.Score[Ini.PlayerLevel[0], I-1].Track = 1) or (CurrentSong.Score[Ini.PlayerLevel[0], I-1].Track = 2) then
      Text[TextName[I]].Text := Text[TextName[I]].Text + ' (P' + IntToStr(CurrentSong.Score[Ini.PlayerLevel[0], I-1].Track) + ')';
    Text[TextScore[I]].Text := IntToStr(CurrentSong.Score[Ini.PlayerLevel[0], I-1].Score);
    Text[TextDate[I]].Text := CurrentSong.Score[Ini.PlayerLevel[0], I-1].Date;
  end;

  If Length(CurrentSong.Score[Ini.PlayerLevel[0]])=0 then
    FadeTo(@ScreenSong); //if there are no scores to show, go to next screen
  for I := Length(CurrentSong.Score[Ini.PlayerLevel[0]]) + 1 to 5 do
  begin
    Statics[StaticNumber[I]].Visible := false;
    Text[TextNumber[I]].Visible := false;
    Text[TextName[I]].Visible := false;
    Text[TextScore[I]].Visible := false;
    Text[TextDate[I]].Visible := false;
  end;

  Text[TextLevel].Text := IDifficultyTranslated[Ini.PlayerLevel[0]];
end;

procedure TScreenTop5.DrawScores(difficulty: integer);
var
  I:    integer;
begin
  for I := 1 to Length(CurrentSong.Score[difficulty]) do
  begin
    Statics[StaticNumber[I]].Visible := true;
    Text[TextNumber[I]].Visible := true;
    Text[TextName[I]].Visible := true;
    Text[TextScore[I]].Visible := true;
    Text[TextDate[I]].Visible := true;

    Text[TextName[I]].Text := CurrentSong.Score[difficulty, I-1].Name;
    if (CurrentSong.Score[difficulty, I-1].Track = 1) or (CurrentSong.Score[difficulty, I-1].Track = 2) then
      Text[TextName[I]].Text := Text[TextName[I]].Text + ' (P' + IntToStr(CurrentSong.Score[difficulty, I-1].Track) + ')';
    Text[TextScore[I]].Text := IntToStr(CurrentSong.Score[difficulty, I-1].Score);
    Text[TextDate[I]].Text := CurrentSong.Score[difficulty, I-1].Date;
  end;

  for I := Length(CurrentSong.Score[difficulty]) + 1 to 5 do
  begin
    Statics[StaticNumber[I]].Visible := false;
    Text[TextNumber[I]].Visible := false;
    Text[TextName[I]].Visible := false;
    Text[TextScore[I]].Visible := false;
    Text[TextDate[I]].Visible := false;
  end;

  Text[TextLevel].Text := IDifficulty[difficulty];
end;

function TScreenTop5.Draw: boolean;
begin
  Result := inherited Draw;
end;

end.
