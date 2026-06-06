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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/screens/UScreenStatMain.pas $
 * $Id: UScreenStatMain.pas 1939 2009-11-09 00:27:55Z s_alexander $
 *}

unit UScreenStatMain;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UDisplay,
  UIni,
  UMenu,
  UMusic,
  UThemes,
  sdl2,
  SysUtils;

type
  TStatMainLibraryOverview = record
    SongCount: cardinal;
    ArtistCount: cardinal;
    SongsWithVideo: cardinal;
  end;

  TStatMainScoreOverview = record
    ScoreCount: cardinal;
    SingerCount: cardinal;
    ScoredSongCount: cardinal;
    AverageScore: integer;
  end;

  TScreenStatMain = class(TMenu)
    private
      procedure AppendOverviewLine(var Overview: UTF8String; const Line: UTF8String);
      procedure AppendOverviewSection(var Overview: UTF8String; const Header: RawByteString);
      procedure AppendOverviewSectionText(var Overview: UTF8String; const Header: UTF8String);
      procedure AppendLibraryOverview(var Overview: UTF8String;
          const LibraryOverview: TStatMainLibraryOverview; SongsWithoutVideo: integer);
      procedure AppendOverallScoreOverview(var Overview: UTF8String;
          const ScoreOverview: TStatMainScoreOverview;
          const LibraryOverview: TStatMainLibraryOverview);
      procedure AppendRecentScoreOverview(var Overview: UTF8String;
          const ScoreOverview: TStatMainScoreOverview;
          const RecentScoreOverview: TStatMainScoreOverview;
          const RecentPeriod: TThemeStatMainRecentPeriod);
      procedure AppendScoreOverviewLines(var Overview: UTF8String;
          const ScoreOverview: TStatMainScoreOverview; const EmptyTextID: RawByteString;
          IncludeAverage: boolean = true);
      procedure AppendFunFacts(var Overview: UTF8String);
      function BoldOverviewText(const Text: UTF8String): UTF8String;
      function FormatAverage(Numerator, Denominator: cardinal): UTF8String;
      function FormatOverviewDate(Date: TDateTime): UTF8String;
      function FormatOverviewText(const TextID: RawByteString; const Args: array of const): UTF8String;
      function FormatPercent(Numerator, Denominator: cardinal): UTF8String;
      function FormatSongFact(const Artist, Title: UTF8String): UTF8String;
      function FormatSignedDelta(Value: integer): UTF8String;
      function LoadLibraryOverview: TStatMainLibraryOverview;
      function LoadScoreOverview(SinceDate: TDateTime = 0): TStatMainScoreOverview;
      function LoadRecentScoreOverview(out RecentPeriod: TThemeStatMainRecentPeriod): TStatMainScoreOverview;
    public
      TextOverview:    integer;
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure SetAnimationProgress(Progress: real); override;

      procedure SetOverview;
  end;

const
  ID='ID_050';   //for help system

implementation

uses
  UDataBase,
  UGraphic,
  UHelp,
  ULanguage,
  ULog,
  USong,
  USongs,
  Classes,
  DateUtils;

function TScreenStatMain.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
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
      SDLK_BACKSPACE :
        begin
          Ini.Save;
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenMain);
        end;
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;
      SDLK_RETURN:
        begin
          //Exit Button Pressed
          if Interaction = 4 then
          begin
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(@ScreenMain);
          end
          else //One of the Stats Buttons Pressed
          begin
            AudioPlayback.PlaySound(SoundLib.Back);
            ScreenStatDetail.Typ := TStatType(Interaction);
            FadeTo(@ScreenStatDetail);
          end;
        end;
      SDLK_LEFT:
      begin
          InteractPrev;
      end;
      SDLK_RIGHT:
      begin
          InteractNext;
      end;
      SDLK_UP:
      begin
          InteractPrev;
      end;
      SDLK_DOWN:
      begin
          InteractNext;
      end;
    end;
  end;
end;

constructor TScreenStatMain.Create;
begin
  inherited Create;

  TextOverview := AddText(Theme.StatMain.TextOverview);

  LoadFromTheme(Theme.StatMain);

  AddButton(Theme.StatMain.ButtonScores);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.StatDetail.Description[0]);

  AddButton(Theme.StatMain.ButtonSingers);
  if (Length(Button[1].Text)=0) then
    AddButtonText(14, 20, Theme.StatDetail.Description[1]);

  AddButton(Theme.StatMain.ButtonSongs);
  if (Length(Button[2].Text)=0) then
    AddButtonText(14, 20, Theme.StatDetail.Description[2]);

  AddButton(Theme.StatMain.ButtonBands);
  if (Length(Button[3].Text)=0) then
    AddButtonText(14, 20, Theme.StatDetail.Description[3]);

  AddButton(Theme.StatMain.ButtonExit);
  if (Length(Button[4].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[OPTIONS_DESC_INDEX_BACK]);

  Interaction := 0;
end;

procedure TScreenStatMain.OnShow;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenStatMain');

  //Set Overview Text:
  SetOverview;
end;

procedure TScreenStatMain.AppendOverviewLine(var Overview: UTF8String; const Line: UTF8String);
begin
  if (Line = '') then
    Exit;

  if (Overview <> '') then
    Overview := Overview + '\n';
  Overview := Overview + Line;
end;

procedure TScreenStatMain.AppendOverviewSection(var Overview: UTF8String; const Header: RawByteString);
begin
  AppendOverviewSectionText(Overview, BoldOverviewText(Language.Translate(Header)));
end;

procedure TScreenStatMain.AppendOverviewSectionText(var Overview: UTF8String; const Header: UTF8String);
begin
  if (Overview <> '') then
    Overview := Overview + '\n\n';
  Overview := Overview + Header;
end;

procedure TScreenStatMain.AppendLibraryOverview(var Overview: UTF8String;
    const LibraryOverview: TStatMainLibraryOverview; SongsWithoutVideo: integer);
var
  Line: UTF8String;
begin
  Line := BoldOverviewText(Language.Translate('STAT_OVERVIEW_LIBRARY')) + ': ' +
      FormatOverviewText('STAT_OVERVIEW_LIBRARY_TOTAL',
      [LibraryOverview.SongCount, LibraryOverview.ArtistCount]);
  if (SongsWithoutVideo > 0) then
    Line := Line + ' - ' + FormatOverviewText('STAT_OVERVIEW_LIBRARY_VIDEO',
        [LibraryOverview.SongsWithVideo, SongsWithoutVideo]);
  AppendOverviewLine(Overview, Line);
end;

procedure TScreenStatMain.AppendOverallScoreOverview(var Overview: UTF8String;
    const ScoreOverview: TStatMainScoreOverview;
    const LibraryOverview: TStatMainLibraryOverview);
var
  Header: UTF8String;
begin
  Header := Language.Translate('STAT_OVERVIEW_SCORES') + ' (' +
      FormatOverviewDate(Database.GetStatReset()) + ')';
  AppendOverviewSectionText(Overview, BoldOverviewText(Header));

  if (ScoreOverview.ScoreCount > 0) then
  begin
    AppendOverviewLine(Overview, FormatOverviewText('STAT_OVERVIEW_SCORE_ACTIVITY',
        [ScoreOverview.ScoreCount, ScoreOverview.SingerCount,
        ScoreOverview.ScoredSongCount]) + ', ' +
        FormatOverviewText('STAT_OVERVIEW_SCORE_DENSITY',
        [FormatAverage(ScoreOverview.ScoreCount, ScoreOverview.SingerCount),
        FormatAverage(ScoreOverview.ScoreCount, ScoreOverview.ScoredSongCount)]));
    AppendOverviewLine(Overview, FormatOverviewText('STAT_OVERVIEW_COVERAGE',
        [FormatPercent(ScoreOverview.ScoredSongCount, LibraryOverview.SongCount)]) +
        ' - ' + FormatOverviewText('STAT_OVERVIEW_AVERAGE_SCORE',
        [ScoreOverview.AverageScore]));
  end
  else
    AppendOverviewLine(Overview, Language.Translate('STAT_OVERVIEW_NO_SCORES'));
end;

procedure TScreenStatMain.AppendRecentScoreOverview(var Overview: UTF8String;
    const ScoreOverview: TStatMainScoreOverview;
    const RecentScoreOverview: TStatMainScoreOverview;
    const RecentPeriod: TThemeStatMainRecentPeriod);
begin
  if (RecentPeriod.TitleTextID = '') then
    Exit;

  AppendOverviewSection(Overview, RecentPeriod.TitleTextID);
  if (RecentScoreOverview.ScoreCount > 0) then
  begin
    AppendOverviewLine(Overview, FormatOverviewText('STAT_OVERVIEW_SCORE_ACTIVITY',
        [RecentScoreOverview.ScoreCount, RecentScoreOverview.SingerCount,
        RecentScoreOverview.ScoredSongCount]) + ' (' +
        FormatOverviewText('STAT_OVERVIEW_RECENT_SHARE',
        [FormatPercent(RecentScoreOverview.ScoreCount, ScoreOverview.ScoreCount)]) + ')');
    AppendOverviewLine(Overview, FormatOverviewText('STAT_OVERVIEW_RECENT_AVERAGE',
        [RecentScoreOverview.AverageScore,
        FormatSignedDelta(RecentScoreOverview.AverageScore - ScoreOverview.AverageScore)]));
  end
  else
    AppendOverviewLine(Overview, Language.Translate('STAT_OVERVIEW_RECENT_EMPTY'));
end;

procedure TScreenStatMain.AppendScoreOverviewLines(var Overview: UTF8String;
    const ScoreOverview: TStatMainScoreOverview; const EmptyTextID: RawByteString;
    IncludeAverage: boolean);
begin
  if (ScoreOverview.ScoreCount > 0) then
  begin
    AppendOverviewLine(Overview, FormatOverviewText('STAT_OVERVIEW_SCORE_ACTIVITY',
        [ScoreOverview.ScoreCount, ScoreOverview.SingerCount,
        ScoreOverview.ScoredSongCount]));
    if IncludeAverage then
      AppendOverviewLine(Overview, FormatOverviewText('STAT_OVERVIEW_AVERAGE_SCORE',
          [ScoreOverview.AverageScore]));
  end
  else
    AppendOverviewLine(Overview, Language.Translate(EmptyTextID));
end;

procedure TScreenStatMain.AppendFunFacts(var Overview: UTF8String);
var
  Fact: TStatFunFact;
  FactIndex: integer;
  FactText: UTF8String;
  FunFacts: TList;
  SongText: UTF8String;
begin
  FunFacts := Database.GetFunFacts(5);
  try
    if (FunFacts = nil) or (FunFacts.Count = 0) then
      Exit;

    AppendOverviewSection(Overview, 'STAT_OVERVIEW_FUN');
    for FactIndex := 0 to FunFacts.Count - 1 do
    begin
      Fact := TStatFunFact(FunFacts[FactIndex]);
      SongText := FormatSongFact(Fact.Artist, Fact.Title);
      FactText := '';

      case Fact.FunFactType of
        sffArtistRegular:
          FactText := FormatOverviewText('STAT_OVERVIEW_FUN_ARTIST_REGULAR',
              [BoldOverviewText(Fact.Artist), BoldOverviewText(Fact.Player),
              BoldOverviewText(IntToStr(Fact.Count))]);
        sffSongRegular:
          FactText := FormatOverviewText('STAT_OVERVIEW_FUN_SONG_REGULAR',
              [BoldOverviewText(SongText), BoldOverviewText(Fact.Player),
              BoldOverviewText(IntToStr(Fact.Count))]);
        sffOnlySinger:
          FactText := FormatOverviewText('STAT_OVERVIEW_FUN_ONLY_SINGER',
              [BoldOverviewText(Fact.Player), BoldOverviewText(SongText),
              BoldOverviewText(IntToStr(Fact.Count))]);
        sffTiedRivals:
          FactText := FormatOverviewText('STAT_OVERVIEW_FUN_TIED_RIVALS',
              [BoldOverviewText(Fact.Player), BoldOverviewText(Fact.OtherPlayer),
              BoldOverviewText(SongText), BoldOverviewText(IntToStr(Fact.Score))]);
        sffHiddenGem:
          FactText := FormatOverviewText('STAT_OVERVIEW_FUN_HIDDEN_GEM',
              [BoldOverviewText(SongText), BoldOverviewText(IntToStr(Fact.Count)),
              BoldOverviewText(IntToStr(Fact.Score))]);
        sffUnderdogFavorite:
          FactText := FormatOverviewText('STAT_OVERVIEW_FUN_UNDERDOG_FAVORITE',
              [BoldOverviewText(Fact.Player), BoldOverviewText(SongText),
              BoldOverviewText(IntToStr(Fact.Score)), BoldOverviewText(IntToStr(Fact.Delta))]);
        sffNarrowLead:
          FactText := FormatOverviewText('STAT_OVERVIEW_FUN_NARROW_LEAD',
              [BoldOverviewText(Fact.Player), BoldOverviewText(Fact.OtherPlayer),
              BoldOverviewText(SongText), BoldOverviewText(IntToStr(Fact.Delta))]);
        sffCloseChase:
          FactText := FormatOverviewText('STAT_OVERVIEW_FUN_CLOSE_CHASE',
              [BoldOverviewText(Fact.Player), BoldOverviewText(Fact.OtherPlayer),
              BoldOverviewText(SongText), BoldOverviewText(IntToStr(Fact.Delta))]);
        sffComeback:
          FactText := FormatOverviewText('STAT_OVERVIEW_FUN_COMEBACK',
              [BoldOverviewText(Fact.Player), BoldOverviewText(SongText),
              BoldOverviewText(IntToStr(Fact.Delta))]);
        sffSharedFavorite:
          FactText := FormatOverviewText('STAT_OVERVIEW_FUN_SHARED_FAVORITE',
              [BoldOverviewText(SongText), BoldOverviewText(IntToStr(Fact.Count))]);
      end;

      AppendOverviewLine(Overview, FactText);
    end;
  finally
    Database.FreeFunFacts(FunFacts);
  end;
end;

function TScreenStatMain.BoldOverviewText(const Text: UTF8String): UTF8String;
begin
  if (Text = '') then
    Result := ''
  else
    Result := '**' + Text + '**';
end;

function TScreenStatMain.FormatAverage(Numerator, Denominator: cardinal): UTF8String;
begin
  if (Denominator = 0) then
    Result := '0.0'
  else
    Result := FloatToStrF(Numerator / Denominator, ffFixed, 8, 1);
end;

function TScreenStatMain.FormatOverviewDate(Date: TDateTime): UTF8String;
var
  Day: word;
  Month: word;
  Year: word;
begin
  DecodeDate(Date, Year, Month, Day);
  Result := FormatOverviewText('STAT_FORMAT_DATE', [Day, Month, Year]);
end;

function TScreenStatMain.FormatOverviewText(const TextID: RawByteString; const Args: array of const): UTF8String;
begin
  Result := '';

  try
    Result := Format(Language.Translate(TextID), Args);
  except
    on E: EConvertError do
      Log.LogError('Error Parsing FormatString "' + string(TextID) + '": ' + E.Message);
  end;
end;

function TScreenStatMain.FormatPercent(Numerator, Denominator: cardinal): UTF8String;
var
  Percent: double;
begin
  if (Denominator = 0) then
  begin
    Result := '0%';
    Exit;
  end;

  Percent := Numerator * 100 / Denominator;
  if (Numerator > 0) and (Percent < 0.1) then
    Result := '<0.1%'
  else
    Result := FormatFloat('0.#', Percent) + '%';
end;

function TScreenStatMain.FormatSongFact(const Artist, Title: UTF8String): UTF8String;
begin
  if (Artist = '') then
    Result := Title
  else if (Title = '') then
    Result := Artist
  else
    Result := Artist + ' - ' + Title;
end;

function TScreenStatMain.FormatSignedDelta(Value: integer): UTF8String;
begin
  if (Value > 0) then
    Result := '+' + IntToStr(Value)
  else
    Result := IntToStr(Value);
end;

function TScreenStatMain.LoadLibraryOverview: TStatMainLibraryOverview;
var
  Artists: TStringList;
  Song: TSong;
  SongIndex: integer;
begin
  Result.SongCount := Songs.SongList.Count;
  Result.ArtistCount := 0;
  Result.SongsWithVideo := 0;

  Artists := TStringList.Create;
  try
    Artists.Sorted := true;
    Artists.Duplicates := dupIgnore;

    for SongIndex := 0 to Songs.SongList.Count - 1 do
    begin
      Song := TSong(Songs.SongList[SongIndex]);
      if (Song.Artist <> '') then
        Artists.Add(Song.Artist);
      if Song.Video.IsSet then
        Inc(Result.SongsWithVideo);
    end;

    Result.ArtistCount := Artists.Count;
  finally
    Artists.Free;
  end;
end;

function TScreenStatMain.LoadScoreOverview(SinceDate: TDateTime): TStatMainScoreOverview;
begin
  Result.ScoreCount := Database.GetScoreCount(SinceDate);
  Result.SingerCount := Database.GetSingerCount(SinceDate);
  Result.ScoredSongCount := Database.GetScoredSongCount(SinceDate);
  Result.AverageScore := Database.GetAverageScore(SinceDate);
end;

function TScreenStatMain.LoadRecentScoreOverview(out RecentPeriod: TThemeStatMainRecentPeriod): TStatMainScoreOverview;
var
  PeriodIndex: integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  RecentPeriod.TitleTextID := '';
  RecentPeriod.Days := 0;
  if (Length(Theme.StatMain.RecentPeriod) = 0) then
    Exit;

  RecentPeriod := Theme.StatMain.RecentPeriod[High(Theme.StatMain.RecentPeriod)];

  for PeriodIndex := Low(Theme.StatMain.RecentPeriod) to High(Theme.StatMain.RecentPeriod) do
  begin
    if (Theme.StatMain.RecentPeriod[PeriodIndex].Days <= 0) then
      Continue;

    RecentPeriod := Theme.StatMain.RecentPeriod[PeriodIndex];
    Result := LoadScoreOverview(IncDay(Now, -RecentPeriod.Days));
    if (Result.ScoreCount > 0) then
      Exit;
  end;
end;

procedure TScreenStatMain.SetOverview;
var
  Overview: UTF8String;
  SongsWithoutVideo: integer;
  LibraryOverview: TStatMainLibraryOverview;
  ScoreOverview: TStatMainScoreOverview;
  RecentPeriod: TThemeStatMainRecentPeriod;
  RecentScoreOverview: TStatMainScoreOverview;
begin
  Overview := '';

  LibraryOverview := LoadLibraryOverview;
  SongsWithoutVideo := integer(LibraryOverview.SongCount) -
      integer(LibraryOverview.SongsWithVideo);
  AppendLibraryOverview(Overview, LibraryOverview, SongsWithoutVideo);

  ScoreOverview := LoadScoreOverview;
  AppendOverallScoreOverview(Overview, ScoreOverview, LibraryOverview);

  RecentScoreOverview := LoadRecentScoreOverview(RecentPeriod);
  AppendRecentScoreOverview(Overview, ScoreOverview, RecentScoreOverview,
      RecentPeriod);
  AppendFunFacts(Overview);

  Text[TextOverview].Text := Overview;
end;

procedure TScreenStatMain.SetAnimationProgress(Progress: real);
var
  I: integer;
begin
  for I := 0 to high(Button) do
    Button[I].Texture.ScaleW := Progress;
end;

end.
