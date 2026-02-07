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
    public
      Typ:  TStatType;
      Page: cardinal;
      Count: byte;
      Reversed: boolean;

      TotEntrys: cardinal;
      TotPages:  cardinal;

      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure SetAnimationProgress(Progress: real); override;

      procedure SetTitle;
      Procedure SetPage(NewPage: cardinal);
  end;

const
  ID='ID_051';   //for help system

implementation

uses
  UGraphic,
  UHelp,
  ULanguage,
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
      SDLK_RETURN:
        begin
          if Interaction = 0 then
          begin
            //Next Page
            SetPage(Page+1);
          end;

          if Interaction = 1 then
          begin
            //Previous Page
            if (Page > 0) then
              SetPage(Page-1);
          end;

          if Interaction = 2 then
          begin
            //Reverse Order
            Reversed := not Reversed;
            SetPage(Page);
          end;

          if Interaction = 3 then
          begin
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(@ScreenStatMain);
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

constructor TScreenStatDetail.Create;
var
  I:    integer;
  DeltaTextX, DeltaTextY: integer;
begin
  inherited Create;

  if Ini.StatDetailCount < 2 then
    Ini.StatDetailCount := 2;

  DeltaTextX := Round((Theme.StatDetail.TextList[1].X - Theme.StatDetail.TextList[0].X) / (Ini.StatDetailCount - 1));
  DeltaTextY := Round((Theme.StatDetail.TextList[1].Y - Theme.StatDetail.TextList[0].Y) / (Ini.StatDetailCount - 1));

  for I := 0 to Ini.StatDetailCount-1 do
    AddText(Theme.StatDetail.TextList[0].X + I * DeltaTextX, Theme.StatDetail.TextList[0].Y + I * DeltaTextY, Theme.StatDetail.TextList[0]);

  Count := Ini.StatDetailCount;

  AddText(Theme.StatDetail.TextDescription);
  AddText(Theme.StatDetail.TextPage);

  LoadFromTheme(Theme.StatDetail);

  AddButton(Theme.StatDetail.ButtonNext);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Language.Translate('STAT_NEXT'));

  AddButton(Theme.StatDetail.ButtonPrev);
  if (Length(Button[1].Text)=0) then
    AddButtonText(14, 20, Language.Translate('STAT_PREV'));

  AddButton(Theme.StatDetail.ButtonReverse);
  if (Length(Button[2].Text)=0) then
    AddButtonText(14, 20, Language.Translate('STAT_REVERSE'));

  AddButton(Theme.StatDetail.ButtonExit);
  if (Length(Button[3].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[OPTIONS_DESC_INDEX_NETWORK]);

  Interaction := 0;
  Typ := TStatType(0);
end;

procedure TScreenStatDetail.OnShow;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenStatDetail');

  //Set Tot Entrys and Pages
  TotEntrys := DataBase.GetTotalEntrys(Typ);
  TotPages := Ceil(TotEntrys / Count);

  //Show correct Title
  SetTitle;

  //Show First Page
  Reversed := false;
  SetPage(0);
end;

procedure TScreenStatDetail.SetTitle;
begin
  if Reversed then
    Text[Count].Text := Theme.StatDetail.DescriptionR[Ord(Typ)]
  else
    Text[Count].Text := Theme.StatDetail.Description[Ord(Typ)];
end;

procedure TScreenStatDetail.SetPage(NewPage: cardinal);
var
  StatList: TList;
  I: integer;
  FormatStr: string;
  PerPage: byte;
  NewlinesInFormatStr: integer;
begin
  // reset texts
  for I := 0 to Ini.StatDetailCount-1 do
    Text[I].Text := '';

  FormatStr := Theme.StatDetail.FormatStr[Ord(Typ)];

  NewlinesInFormatStr := 1;
  for I := 1 to Length(FormatStr)-1 do
  begin
    if (FormatStr[I] = '\') and (FormatStr[I+1] = 'n') then
      Inc(NewlinesInFormatStr);
  end;

  Count := Ini.StatDetailCount div (NewlinesInFormatStr);

  // fetch statistics
  StatList := Database.GetStats(Typ, Count, NewPage, Reversed);
  if ((StatList <> nil) and (StatList.Count > 0)) then
  begin
    Page := NewPage;

    //refresh Texts
    for I := 0 to StatList.Count-1 do
    begin
      try
        case Typ of
          stBestScores: begin //Best Scores
            with TStatResultBestScores(StatList[I]) do
            begin
              //Set Texts
              if (Score > 0) then
              begin
                Text[I*2].Text := Format(FormatStr, [Singer, Score, Theme.ILevel[Difficulty], SongArtist, SongTitle, Date]);
              end;
            end;
          end;

          stBestSingers: begin //Best Singers
            with TStatResultBestSingers(StatList[I]) do
            begin
              //Set Texts
              if (AverageScore > 0) then
                Text[I].Text := Format(FormatStr, [Player, AverageScore, Count]);
            end;
          end;

          stMostSungSong: begin //Popular Songs
            with TStatResultMostSungSong(StatList[I]) do
            begin
              //Set Texts
              if (Artist <> '') then begin
                Text[I].Text := Format(FormatStr, [Artist, Title, TimesSung, AverageScore]);
                end;
            end;
          end;

          stMostPopBand: begin //Popular Bands
            with TStatResultMostPopBand(StatList[I]) do
            begin
              //Set Texts
              if (ArtistName <> '') then
                Text[I].Text := Format(FormatStr, [ArtistName, TimesSungtot, AverageScore]);
            end;
          end;
        end;
      except
        on E: EConvertError do
          Log.LogError('Error Parsing FormatString in UScreenStatDetail: ' + E.Message);
      end;
    end;

    if (Page + 1 = TotPages) and (TotEntrys mod Count <> 0) then
      PerPage := (TotEntrys mod Count)
    else
      PerPage := Count;

    Count := Ini.StatDetailCount;

    try      
      Text[Count+1].Text := Format(Theme.StatDetail.PageStr,
          [Page + 1, TotPages, PerPage, TotEntrys]);
    except
      on E: EConvertError do
        Log.LogError('Error Parsing FormatString in UScreenStatDetail: ' + E.Message);
    end;

    //Show correct Title
    SetTitle;
  end;

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
