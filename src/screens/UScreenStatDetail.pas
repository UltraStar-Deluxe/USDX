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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/screens/UScreenStatDetail.pas $
 * $Id: UScreenStatDetail.pas 1972 2009-12-06 12:13:34Z s_alexander $
 *}

unit UScreenStatDetail;

interface

{$IFDEF FPC}
  {$mode objfpc}//{$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UDataBase,
  UDisplay,
  UIni,
  UMenu,
  UMusic,
  UThemes,
  sdl2,
  SysUtils;

type
  TScreenStatDetail = class(TMenu)
    private
      TextDescription: integer;
      TextPage: integer;
      EntriesPerPage: byte;

      procedure ChangeEntriesPerPage(Delta: integer);
      procedure RecalculatePages;
      procedure UpdateListLayout;

    public
      Typ:  TStatType;
      Page: cardinal;
      Reversed: boolean;

      TotEntrys: cardinal;
      TotPages:  cardinal;

      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure SetAnimationProgress(Progress: real); override;

      procedure SetTitle;
      procedure SetPage(NewPage: cardinal);
  end;

const
  ID='ID_051';   //for help system

implementation

uses
  UGraphic,
  UHelp,
  ULog,
  UUnicodeUtils,
  Math,
  Classes;

function TScreenStatDetail.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
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
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenStatMain);
        end;
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;
      SDLK_LEFT:
        begin
          if (Page > 0) then
            SetPage(Page - 1);
        end;
      SDLK_RIGHT:
        begin
          if (Page + 1 < TotPages) then
            SetPage(Page + 1);
        end;
      SDLK_UP:
        begin
          ChangeEntriesPerPage(-1);
        end;
      SDLK_DOWN:
        begin
          ChangeEntriesPerPage(1);
        end;
      SDLK_R:
        begin
          Reversed := not Reversed;
          SetPage(Page);
        end;
    end;
  end;
end;

constructor TScreenStatDetail.Create;
var
  I: integer;
begin
  inherited Create;

  for I := 0 to STAT_DETAIL_MAX_ENTRIES - 1 do
    AddText(Theme.StatDetail.TextList[0]);

  TextDescription := AddText(Theme.StatDetail.TextDescription);
  TextPage := AddText(Theme.StatDetail.TextPage);

  LoadFromTheme(Theme.StatDetail);

  EntriesPerPage := Ini.StatDetailCount;
  Typ := TStatType(0);
end;

procedure TScreenStatDetail.OnShow;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenStatDetail');

  //Set Tot Entrys and Pages
  TotEntrys := DataBase.GetTotalEntrys(Typ);
  RecalculatePages;
  UpdateListLayout;

  //Show correct Title
  SetTitle;

  //Show First Page
  Reversed := false;
  SetPage(0);
end;

procedure TScreenStatDetail.SetTitle;
begin
  if Reversed then
    Text[TextDescription].Text := Theme.StatDetail.DescriptionR[Ord(Typ)]
  else
    Text[TextDescription].Text := Theme.StatDetail.Description[Ord(Typ)];
end;

procedure TScreenStatDetail.RecalculatePages;
begin
  TotPages := Ceil(TotEntrys / EntriesPerPage);
  if TotPages = 0 then
    TotPages := 1;
end;

procedure TScreenStatDetail.UpdateListLayout;
var
  I: integer;
  TopY: real;
  BottomY: real;
  RowHeight: real;
  TextSize: real;
  ThemeText: TThemeText;
begin
  ThemeText := Theme.StatDetail.TextList[0];
  TopY := ThemeText.Y;

  if (Length(Theme.StatDetail.TextList) > 1) then
    BottomY := Theme.StatDetail.TextList[High(Theme.StatDetail.TextList)].Y
  else
    BottomY := TopY + 18 * (EntriesPerPage - 0.5);

  RowHeight := (BottomY - TopY) / Max(1.0, EntriesPerPage - 0.5);
  TextSize := Min(ThemeText.Size, Max(12.0, RowHeight - 2));

  for I := 0 to STAT_DETAIL_MAX_ENTRIES - 1 do
  begin
    Text[I].X := ThemeText.X;
    Text[I].Y := TopY + I * RowHeight;
    Text[I].W := ThemeText.W;
    Text[I].H := Max(1.0, RowHeight);
    Text[I].Size := TextSize;
    Text[I].Visible := I < EntriesPerPage;
  end;
end;

procedure TScreenStatDetail.ChangeEntriesPerPage(Delta: integer);
var
  FirstEntry: cardinal;
  NewCount: integer;
begin
  NewCount := EnsureRange(EntriesPerPage + Delta, STAT_DETAIL_MIN_ENTRIES, STAT_DETAIL_MAX_ENTRIES);
  if (NewCount = EntriesPerPage) then
    Exit;

  FirstEntry := Page * EntriesPerPage;
  EntriesPerPage := byte(NewCount);
  Ini.StatDetailCount := EntriesPerPage;
  Ini.SaveStatDetailCount;

  RecalculatePages;
  UpdateListLayout;
  SetPage(FirstEntry div EntriesPerPage);
end;

procedure TScreenStatDetail.SetPage(NewPage: cardinal);
var
  StatList: TList;
  I: integer;
  FormatStr: string;
  PerPage: byte;
  VisibleEntries: integer;
begin
  // reset texts
  for I := 0 to STAT_DETAIL_MAX_ENTRIES - 1 do
    Text[I].Text := '';

  FormatStr := Theme.StatDetail.FormatStr[Ord(Typ)];

  if (NewPage >= TotPages) then
    NewPage := TotPages - 1;
  Page := NewPage;

  // fetch statistics
  StatList := Database.GetStats(Typ, EntriesPerPage, NewPage, Reversed);
  if (StatList <> nil) then
  begin
    VisibleEntries := Min(StatList.Count, EntriesPerPage);

    //refresh Texts
    for I := 0 to VisibleEntries - 1 do
    begin
      try
        case Typ of
          stBestScores: begin //Best Scores
            with TStatResultBestScores(StatList[I]) do
            begin
              //Set Texts
              if (Score > 0) then
              begin
                Text[I].Text := Format(FormatStr, [Singer, Score, Theme.ILevel[Difficulty], SongArtist, SongTitle, Date]);
              end;
            end;
          end;

          stBestSingers: begin //Best Singers
            with TStatResultBestSingers(StatList[I]) do
            begin
              //Set Texts
              if (AverageScore > 0) then
                Text[I].Text := Format(FormatStr, [Player, AverageScore]);
            end;
          end;

          stMostSungSong: begin //Popular Songs
            with TStatResultMostSungSong(StatList[I]) do
            begin
              //Set Texts
              if (Artist <> '') then
                Text[I].Text := Format(FormatStr, [Artist, Title, TimesSung]);
            end;
          end;

          stMostPopBand: begin //Popular Bands
            with TStatResultMostPopBand(StatList[I]) do
            begin
              //Set Texts
              if (ArtistName <> '') then
                Text[I].Text := Format(FormatStr, [ArtistName, TimesSungtot]);
            end;
          end;
        end;
      except
        on E: EConvertError do
          Log.LogError('Error Parsing FormatString in UScreenStatDetail: ' + E.Message);
      end;
    end;
  end;

  if (TotEntrys = 0) then
    PerPage := 0
  else if (Page + 1 = TotPages) and (TotEntrys mod EntriesPerPage <> 0) then
    PerPage := (TotEntrys mod EntriesPerPage)
  else
    PerPage := EntriesPerPage;

  try
    Text[TextPage].Text := Format(Theme.StatDetail.PageStr,
        [Page + 1, TotPages, PerPage, TotEntrys]);
  except
    on E: EConvertError do
      Log.LogError('Error Parsing FormatString in UScreenStatDetail: ' + E.Message);
  end;

  //Show correct Title
  SetTitle;

  Database.FreeStats(StatList);
end;

procedure TScreenStatDetail.SetAnimationProgress(Progress: real);
var
  I: integer;
begin
  for I := 0 to High(Button) do
    Button[I].Texture.ScaleW := Progress;
end;

end.
