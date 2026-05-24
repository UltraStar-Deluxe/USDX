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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/screens/UScreenHighScores.pas $
 * $Id: UScreenHighScores.pas 2548 2010-06-18 11:00:21Z whiteshark0 $
 *}

unit UScreenHighScores;

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
  TScreenHighScores = class(TMenu)
    private
      procedure ApplyTextLayout(TextIndex: integer; const ThemeText: TThemeText);
      procedure ApplyStaticLayout(StaticIndex: integer; const ThemeStatic: TThemeStatic);
      procedure ChangeEntryCount(Delta: integer);
      function InterpolateInteger(StartValue, EndValue, EntryIndex, EntryCount: integer): integer;
      function InterpolateStatic(const ThemeStatics: AThemeStatic; EntryIndex, EntryCount: integer): TThemeStatic;
      function InterpolateText(const ThemeTexts: AThemeText; EntryIndex, EntryCount: integer): TThemeText;
      procedure LoadCurrentSongScores;
      procedure SetEntryVisible(EntryIndex: integer; Visible: boolean);
      procedure UpdateEntryLayout;
    public
      TextDifficulty:  integer;
      TextArtistTitle: integer;
      VisibleDifficulty: integer;

      EntryNumberStatic: array of integer;
      EntryNumberText:   array of integer;
      EntryNameText:     array of integer;
      EntryScoreText:    array of integer;
      EntryDateText:     array of integer;

      IsFadingOut:    boolean;

      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      function ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean; override;
      procedure OnShow; override;
      procedure ShowScoresForDifficulty(difficulty: integer);
      function Draw: boolean; override;
  end;

const
  HighScoresHelpID = 'ID_024';

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

procedure TScreenHighScores.ApplyTextLayout(TextIndex: integer; const ThemeText: TThemeText);
begin
  Text[TextIndex].X := ThemeText.X;
  Text[TextIndex].Y := ThemeText.Y;
  Text[TextIndex].W := ThemeText.W;
  Text[TextIndex].H := ThemeText.H;
  Text[TextIndex].Size := ThemeText.Size;
end;

procedure TScreenHighScores.ApplyStaticLayout(StaticIndex: integer; const ThemeStatic: TThemeStatic);
begin
  Statics[StaticIndex].Texture.X := ThemeStatic.X;
  Statics[StaticIndex].Texture.Y := ThemeStatic.Y;
  Statics[StaticIndex].Texture.W := ThemeStatic.W;
  Statics[StaticIndex].Texture.H := ThemeStatic.H;
end;

procedure TScreenHighScores.ChangeEntryCount(Delta: integer);
var
  NewEntryCount: integer;
begin
  NewEntryCount := Ini.HighScoreScreenEntries + Delta;
  if (NewEntryCount < MIN_HIGH_SCORE_SCREEN_ENTRIES) then
    NewEntryCount := MIN_HIGH_SCORE_SCREEN_ENTRIES
  else if (NewEntryCount > MAX_HIGH_SCORE_SCREEN_ENTRIES) then
    NewEntryCount := MAX_HIGH_SCORE_SCREEN_ENTRIES;

  if (NewEntryCount = Ini.HighScoreScreenEntries) then
    Exit;

  Ini.HighScoreScreenEntries := NewEntryCount;
  Ini.SaveHighScoreScreenEntries;

  UpdateEntryLayout;
  LoadCurrentSongScores;
  ShowScoresForDifficulty(VisibleDifficulty);
end;

function TScreenHighScores.InterpolateInteger(StartValue, EndValue, EntryIndex, EntryCount: integer): integer;
begin
  if (EntryCount <= 1) then
    Result := StartValue
  else
    Result := Round(StartValue + ((EndValue - StartValue) * EntryIndex) / (EntryCount - 1));
end;

function TScreenHighScores.InterpolateStatic(const ThemeStatics: AThemeStatic; EntryIndex, EntryCount: integer): TThemeStatic;
begin
  Result := ThemeStatics[0];
  Result.X := InterpolateInteger(ThemeStatics[0].X, ThemeStatics[High(ThemeStatics)].X, EntryIndex, EntryCount);
  Result.Y := InterpolateInteger(ThemeStatics[0].Y, ThemeStatics[High(ThemeStatics)].Y, EntryIndex, EntryCount);
end;

function TScreenHighScores.InterpolateText(const ThemeTexts: AThemeText; EntryIndex, EntryCount: integer): TThemeText;
begin
  Result := ThemeTexts[0];
  Result.X := InterpolateInteger(ThemeTexts[0].X, ThemeTexts[High(ThemeTexts)].X, EntryIndex, EntryCount);
  Result.Y := InterpolateInteger(ThemeTexts[0].Y, ThemeTexts[High(ThemeTexts)].Y, EntryIndex, EntryCount);
end;

procedure TScreenHighScores.LoadCurrentSongScores;
var
  I: integer;
  Report: string;
begin
  try
    DataBase.ReadScore(CurrentSong);
  except
    on E : Exception do
    begin
      Report := 'Reading songscore failed in highscore screen. Faulty database file?' + LineEnding +
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
      Log.LogWarn(Report, 'UScreenHighScores.LoadCurrentSongScores');
    end;
  end;
end;

procedure TScreenHighScores.SetEntryVisible(EntryIndex: integer; Visible: boolean);
begin
  Statics[EntryNumberStatic[EntryIndex]].Visible := Visible;
  Text[EntryNumberText[EntryIndex]].Visible := Visible;
  Text[EntryNameText[EntryIndex]].Visible := Visible;
  Text[EntryScoreText[EntryIndex]].Visible := Visible;
  Text[EntryDateText[EntryIndex]].Visible := Visible;
end;

procedure TScreenHighScores.UpdateEntryLayout;
var
  I: integer;
  LayoutIndex: integer;
  ThemeText: TThemeText;
begin
  for I := 0 to High(EntryNumberStatic) do
  begin
    if (I < Ini.HighScoreScreenEntries) then
      LayoutIndex := I
    else
      LayoutIndex := Ini.HighScoreScreenEntries - 1;

    ApplyStaticLayout(EntryNumberStatic[I],
      InterpolateStatic(Theme.HighScores.StaticNumber, LayoutIndex, Ini.HighScoreScreenEntries));

    ThemeText := InterpolateText(Theme.HighScores.TextNumber, LayoutIndex, Ini.HighScoreScreenEntries);
    ApplyTextLayout(EntryNumberText[I], ThemeText);

    ThemeText := InterpolateText(Theme.HighScores.TextName, LayoutIndex, Ini.HighScoreScreenEntries);
    ApplyTextLayout(EntryNameText[I], ThemeText);

    ThemeText := InterpolateText(Theme.HighScores.TextScore, LayoutIndex, Ini.HighScoreScreenEntries);
    ApplyTextLayout(EntryScoreText[I], ThemeText);

    ThemeText := InterpolateText(Theme.HighScores.TextDate, LayoutIndex, Ini.HighScoreScreenEntries);
    ApplyTextLayout(EntryDateText[I], ThemeText);
  end;
end;

function TScreenHighScores.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
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
          if (not IsFadingOut) then
          begin
            FadeTo(@ScreenSong);
            IsFadingOut := true;
          end;
        end;
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;
      SDLK_RIGHT:
        begin
          inc(VisibleDifficulty);
          if (VisibleDifficulty > 2) then
            VisibleDifficulty := 0;
          ShowScoresForDifficulty(VisibleDifficulty);
        end;
      SDLK_LEFT:
        begin
          dec(VisibleDifficulty);
          if (VisibleDifficulty < 0) then
            VisibleDifficulty := 2;
          ShowScoresForDifficulty(VisibleDifficulty);
        end;
      SDLK_UP:
        begin
          inc(VisibleDifficulty);
          if (VisibleDifficulty > 2) then
            VisibleDifficulty := 0;
          ShowScoresForDifficulty(VisibleDifficulty);
        end;
      SDLK_DOWN:
        begin
          dec(VisibleDifficulty);
          if (VisibleDifficulty < 0) then
            VisibleDifficulty := 2;
          ShowScoresForDifficulty(VisibleDifficulty);
        end;
      SDLK_PLUS,
      SDLK_EQUALS,
      SDLK_KP_PLUS:
        begin
          ChangeEntryCount(1);
        end;
      SDLK_MINUS,
      SDLK_KP_MINUS:
        begin
          ChangeEntryCount(-1);
        end;
    end;
  end;
end;

function TScreenHighScores.ParseMouse(MouseButton: integer;
                                      BtnDown: boolean;
                                      X, Y: integer): boolean;
begin
  Result := true;
  if (MouseButton = SDL_BUTTON_LEFT) and BtnDown then
    //left-click anywhere sends return
    ParseInput(SDLK_RETURN, 0, true);
end;

constructor TScreenHighScores.Create;
var
  I: integer;
  ThemeText: TThemeText;
begin
  inherited Create;
  SetLength(EntryNumberStatic, MAX_HIGH_SCORE_SCREEN_ENTRIES);
  SetLength(EntryNumberText, MAX_HIGH_SCORE_SCREEN_ENTRIES);
  SetLength(EntryNameText, MAX_HIGH_SCORE_SCREEN_ENTRIES);
  SetLength(EntryScoreText, MAX_HIGH_SCORE_SCREEN_ENTRIES);
  SetLength(EntryDateText, MAX_HIGH_SCORE_SCREEN_ENTRIES);

  LoadFromTheme(Theme.HighScores);

  TextDifficulty  := AddText(Theme.HighScores.TextLevel);
  TextArtistTitle := AddText(Theme.HighScores.TextArtistTitle);

  for I := 0 to MAX_HIGH_SCORE_SCREEN_ENTRIES - 1 do
  begin
    EntryNumberStatic[I] := AddStatic(
      InterpolateStatic(Theme.HighScores.StaticNumber, I, MAX_HIGH_SCORE_SCREEN_ENTRIES));

    ThemeText := InterpolateText(Theme.HighScores.TextNumber, I, MAX_HIGH_SCORE_SCREEN_ENTRIES);
    ThemeText.Text := IntToStr(I + 1);
    EntryNumberText[I] := AddText(ThemeText);

    EntryNameText[I] := AddText(
      InterpolateText(Theme.HighScores.TextName, I, MAX_HIGH_SCORE_SCREEN_ENTRIES));
    EntryScoreText[I] := AddText(
      InterpolateText(Theme.HighScores.TextScore, I, MAX_HIGH_SCORE_SCREEN_ENTRIES));
    EntryDateText[I] := AddText(
      InterpolateText(Theme.HighScores.TextDate, I, MAX_HIGH_SCORE_SCREEN_ENTRIES));
  end;

  UpdateEntryLayout;
end;

procedure TScreenHighScores.OnShow;
begin
  inherited;

  if not Help.SetHelpID(HighScoresHelpID) then
    Log.LogWarn('No Entry for Help-ID ' + HighScoresHelpID, 'ScreenHighScores');

  IsFadingOut := false;
  VisibleDifficulty := Player[0].Level;

  UpdateEntryLayout;
  LoadCurrentSongScores;

  Text[TextArtistTitle].Text := CurrentSong.Artist + ' - ' + CurrentSong.Title;

  if Length(CurrentSong.Score[Player[0].Level]) = 0 then
    FadeTo(@ScreenSong); //if there are no scores to show, go to next screen

  ShowScoresForDifficulty(Player[0].Level);
end;

procedure TScreenHighScores.ShowScoresForDifficulty(difficulty: integer);
var
  I:    integer;
begin
  for I := 0 to High(EntryNumberStatic) do
  begin
    if (I < Ini.HighScoreScreenEntries) and (I < Length(CurrentSong.Score[difficulty])) then
    begin
      SetEntryVisible(I, true);
      Text[EntryNameText[I]].Text := CurrentSong.Score[difficulty, I].Name;
      Text[EntryScoreText[I]].Text := IntToStr(CurrentSong.Score[difficulty, I].Score);
      Text[EntryDateText[I]].Text := CurrentSong.Score[difficulty, I].Date;
    end
    else
      SetEntryVisible(I, false);
  end;

  Text[TextDifficulty].Text := IDifficultyTranslated[difficulty];
end;

function TScreenHighScores.Draw: boolean;
begin
  Result := inherited Draw;
end;

end.
