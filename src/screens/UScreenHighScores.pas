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
  Math,
  SysUtils;

type
  TScreenHighScores = class(TMenu)
    private
      procedure AddThemedFrame;
      procedure ApplyEntryStaticLayout(StaticIndex, EntryIndex: integer; const ThemeStatic: TThemeStatic);
      procedure ApplyEntryTextLayout(TextIndex, EntryIndex: integer; const ThemeText: TThemeText);
      procedure ApplyTextLayout(TextIndex: integer; const ThemeText: TThemeText);
      procedure ApplyStaticLayout(StaticIndex: integer; const ThemeStatic: TThemeStatic);
      procedure ChangeEntryCount(Delta: integer);
      function EntryY(EntryIndex: integer): integer;
      function FormatEntryCountText(const TextTemplate: UTF8String): UTF8String;
      procedure LoadCurrentSongScores;
      function ScaledSize(ThemeSize: integer): integer;
      procedure SetEntryVisible(EntryIndex: integer; Visible: boolean);
      procedure UpdateEntryLayout;
      procedure UpdateHeaderText;
    public
      TextTitle:            integer;
      TextWhereAmI:         integer;
      TextArtistTitle:      integer;
      TextDifficultyLabel:  integer;
      TextDifficulty:       integer;
      TextContinue:         integer;
      TextNavigate:         integer;
      VisibleDifficulty: integer;
      EntryCount: integer;
      LayoutEntryCount: integer;
      EntryRowPitch: real;
      EntryTextSize: real;

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

function TrackNumber(TrackMask: integer): integer;
begin
  if (TrackMask <= 0) or ((TrackMask and (TrackMask - 1)) <> 0) then
    Exit(0);

  Result := 1;
  while TrackMask > 1 do
  begin
    TrackMask := TrackMask shr 1;
    Inc(Result);
  end;
end;

procedure TScreenHighScores.AddThemedFrame;
var
  I: integer;
begin
  PrepareButtonCollections(Theme.HighScores.ButtonCollection);
  AddBackground(Theme.HighScores.Background);

  for I := 0 to High(Theme.HighScores.Statics) do
    AddStatic(Theme.HighScores.Statics[I]);
end;

procedure TScreenHighScores.ApplyEntryStaticLayout(StaticIndex, EntryIndex: integer; const ThemeStatic: TThemeStatic);
var
  EntryStatic: TThemeStatic;
  NewSize: integer;
begin
  EntryStatic := ThemeStatic;
  EntryStatic.Y := EntryY(EntryIndex) + Round(
      (ThemeStatic.Y - Theme.HighScores.RowNameText.Y) *
      EntryTextSize / Theme.HighScores.RowNameText.Size);

  NewSize := ScaledSize(ThemeStatic.H);
  if (ThemeStatic.H > 0) and (NewSize < ThemeStatic.H) then
  begin
    EntryStatic.H := NewSize;
    EntryStatic.W := Round(ThemeStatic.W * NewSize / ThemeStatic.H);
    EntryStatic.X := ThemeStatic.X + Round((ThemeStatic.W - EntryStatic.W) / 2);
  end;

  ApplyStaticLayout(StaticIndex, EntryStatic);
end;

procedure TScreenHighScores.ApplyEntryTextLayout(TextIndex, EntryIndex: integer; const ThemeText: TThemeText);
var
  EntryText: TThemeText;
begin
  EntryText := ThemeText;
  EntryText.Y := EntryY(EntryIndex) + Round(
      (ThemeText.Y - Theme.HighScores.RowNameText.Y) *
      EntryTextSize / Theme.HighScores.RowNameText.Size);
  EntryText.Size := ScaledSize(ThemeText.Size);
  ApplyTextLayout(TextIndex, EntryText);
end;

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
  NewEntryCount := EntryCount + Delta;
  if (NewEntryCount < 5) then
    NewEntryCount := 5
  else if (NewEntryCount > Theme.HighScores.List.MaxEntries) then
    NewEntryCount := Theme.HighScores.List.MaxEntries;

  if (NewEntryCount = EntryCount) then
    Exit;

  EntryCount := NewEntryCount;
  Ini.HighScoreScreenEntries := EntryCount;
  Ini.SaveHighScoreScreenEntries;

  UpdateHeaderText;
  ShowScoresForDifficulty(VisibleDifficulty);
end;

function TScreenHighScores.EntryY(EntryIndex: integer): integer;
begin
  Result := Round(Theme.HighScores.RowNameText.Y +
      (EntryIndex + 1) * EntryRowPitch);
end;

function TScreenHighScores.FormatEntryCountText(const TextTemplate: UTF8String): UTF8String;
begin
  try
    Result := Format(TextTemplate, [EntryCount]);
  except
    Result := TextTemplate;
  end;
end;

procedure TScreenHighScores.LoadCurrentSongScores;
var
  I: integer;
  Report: string;
begin
  try
    DataBase.ReadScore(CurrentSong, Theme.HighScores.List.MaxEntries);
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

function TScreenHighScores.ScaledSize(ThemeSize: integer): integer;
begin
  Result := Round(ThemeSize * Min(1.0,
      EntryTextSize / Theme.HighScores.RowNameText.Size));
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
  LayoutRows: integer;
  Density: real;
  FontToPitchRatio: real;
  AvailableHeight: real;
begin
  LayoutRows := LayoutEntryCount + 2;
  Density := EnsureRange((LayoutEntryCount - 5) / 15, 0.0, 1.0);
  Density := Power(Density, 2.0);
  FontToPitchRatio := 0.78 + Density * (1.10 - 0.78);
  AvailableHeight := Max(0,
      Theme.HighScores.List.BottomY - Theme.HighScores.RowNameText.Y);
  EntryTextSize := AvailableHeight /
      ((LayoutRows - 1) / FontToPitchRatio + 0.85);
  EntryTextSize := EnsureRange(EntryTextSize,
      Theme.HighScores.List.MinTextSize,
      Theme.HighScores.RowNameText.Size);
  EntryRowPitch := Max(0,
      (AvailableHeight - EntryTextSize * 0.85) / (LayoutRows - 1));

  for I := 0 to High(EntryNumberStatic) do
  begin
    if (I < LayoutEntryCount) then
      LayoutIndex := I
    else
      LayoutIndex := LayoutEntryCount - 1;

    ApplyEntryStaticLayout(EntryNumberStatic[I], LayoutIndex, Theme.HighScores.RowNumberStatic);

    ApplyEntryTextLayout(EntryNumberText[I], LayoutIndex,
      Theme.HighScores.RowNumberText);
    ApplyEntryTextLayout(EntryNameText[I], LayoutIndex,
      Theme.HighScores.RowNameText);
    ApplyEntryTextLayout(EntryScoreText[I], LayoutIndex,
      Theme.HighScores.RowScoreText);
    ApplyEntryTextLayout(EntryDateText[I], LayoutIndex,
      Theme.HighScores.RowDateText);
  end;
end;

procedure TScreenHighScores.UpdateHeaderText;
begin
  Text[TextTitle].Text := FormatEntryCountText(Theme.HighScores.TextTitle.Text);
  Text[TextWhereAmI].Text := FormatEntryCountText(Theme.HighScores.TextWhereAmI.Text);
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
          ChangeEntryCount(-1);
        end;
      SDLK_DOWN:
        begin
          ChangeEntryCount(1);
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
  EntryCount := EnsureRange(Ini.HighScoreScreenEntries, 5, Theme.HighScores.List.MaxEntries);
  LayoutEntryCount := 5;
  SetLength(EntryNumberStatic, Theme.HighScores.List.MaxEntries);
  SetLength(EntryNumberText, Theme.HighScores.List.MaxEntries);
  SetLength(EntryNameText, Theme.HighScores.List.MaxEntries);
  SetLength(EntryScoreText, Theme.HighScores.List.MaxEntries);
  SetLength(EntryDateText, Theme.HighScores.List.MaxEntries);

  AddThemedFrame;

  TextTitle := AddText(Theme.HighScores.TextTitle);
  TextWhereAmI := AddText(Theme.HighScores.TextWhereAmI);
  TextArtistTitle := AddText(Theme.HighScores.TextArtistTitle);
  TextDifficultyLabel := AddText(Theme.HighScores.TextDifficultyLabel);
  TextDifficulty := AddText(Theme.HighScores.TextDifficulty);
  TextContinue := AddText(Theme.HighScores.TextContinue);
  TextNavigate := AddText(Theme.HighScores.TextNavigate);

  for I := 0 to Theme.HighScores.List.MaxEntries - 1 do
  begin
    EntryNumberStatic[I] := AddStatic(Theme.HighScores.RowNumberStatic);

    ThemeText := Theme.HighScores.RowNumberText;
    ThemeText.Text := IntToStr(I + 1);
    EntryNumberText[I] := AddText(ThemeText);

    EntryNameText[I] := AddText(Theme.HighScores.RowNameText);
    EntryScoreText[I] := AddText(Theme.HighScores.RowScoreText);
    EntryDateText[I] := AddText(Theme.HighScores.RowDateText);
  end;

  UpdateEntryLayout;
  UpdateHeaderText;
end;

procedure TScreenHighScores.OnShow;
begin
  inherited;

  if not Help.SetHelpID(HighScoresHelpID) then
    Log.LogWarn('No Entry for Help-ID ' + HighScoresHelpID, 'ScreenHighScores');

  IsFadingOut := false;
  VisibleDifficulty := Player[0].Level;

  UpdateEntryLayout;
  UpdateHeaderText;
  LoadCurrentSongScores;

  Text[TextArtistTitle].Text := CurrentSong.Artist + ' - ' + CurrentSong.Title;

  if Length(CurrentSong.Score[Player[0].Level]) = 0 then
    FadeTo(@ScreenSong); //if there are no scores to show, go to next screen

  ShowScoresForDifficulty(Player[0].Level);
end;

procedure TScreenHighScores.ShowScoresForDifficulty(difficulty: integer);
var
  I:    integer;
  Track: integer;
begin
  LayoutEntryCount := EnsureRange(Length(CurrentSong.Score[difficulty]), 5, EntryCount);
  UpdateEntryLayout;

  for I := 0 to High(EntryNumberStatic) do
  begin
    if (I < EntryCount) and (I < Length(CurrentSong.Score[difficulty])) then
    begin
      SetEntryVisible(I, true);
      Text[EntryNameText[I]].Text := CurrentSong.Score[difficulty, I].Name;
      Track := TrackNumber(CurrentSong.Score[difficulty, I].TrackMask);
      if CurrentSong.IsDuet and (Track > 0) then
        Text[EntryNameText[I]].Text := Text[EntryNameText[I]].Text +
            ' (P' + IntToStr(Track) + ')';
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
