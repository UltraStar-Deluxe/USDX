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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenSongJumpto.pas $
 * $Id: UScreenSongJumpto.pas 2199 2010-03-14 20:56:20Z brunzelchen $
 *}

unit UScreenSongJumpto;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UDisplay,
  UFiles,
  UMenu,
  UMusic,
  USongs,
  UThemes,
  sdl2,
  SysUtils;

type
  TScreenSongJumpto = class(TMenu)
    private
      //For ChangeMusic
      fLastPlayed: integer;
      fVisible: boolean;
      fSelectType: TSongFilter;
      fVisSongs: integer;

      procedure SetTextFound(Count: Cardinal);

      //Visible //Whether the Menu should be Drawn
      //Whether the Menu should be Drawn
      procedure SetVisible(Value: boolean);
    public
      constructor Create; override;

      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      function Draw: boolean; override;

      property Visible: boolean read fVisible write SetVisible;
  end;

const
  ID='ID_016';   //for help system

implementation

uses
  UGraphic,
  UHelp,
  UIni,
  ULanguage,
  ULog,
  UMain,
  UParty,
  UScreenSong,
  UTexture,
  UUnicodeUtils;

function TScreenSongJumpto.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check normal keys
    if (IsAlphaNumericChar(CharCode) or
        IsPunctuationChar(CharCode)) then
    begin
      if (Interaction = 0) then
      begin
        Button[0].Text[0].ColR := Theme.SongJumpto.ButtonSearchText.ColR;
        Button[0].Text[0].ColG := Theme.SongJumpto.ButtonSearchText.ColG;
        Button[0].Text[0].ColB := Theme.SongJumpto.ButtonSearchText.ColB;

        Button[0].Text[0].Text := Button[0].Text[0].Text + UCS4ToUTF8String(CharCode);
        SetTextFound(CatSongs.SetFilter(Button[0].Text[0].Text, fSelectType));
      end;
    end;

    // check special keys
    case PressedKey of
      SDLK_BACKSPACE:
        begin
          if (Interaction = 0) and (Length(Button[0].Text[0].Text) > 0) then
          begin
            Button[0].Text[0].DeleteLastLetter();
            SetTextFound(CatSongs.SetFilter(Button[0].Text[0].Text, fSelectType));
          end;
        end;

      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;

      SDLK_RETURN,
      SDLK_ESCAPE:
        begin
          Visible := false;
          AudioPlayback.PlaySound(SoundLib.Back);
          if (fVisSongs = 0) and (Length(Button[0].Text[0].Text) > 0) then
          begin
            //ScreenSong.UnLoadDetailedCover;
            Button[0].Text[0].Text := '';
            CatSongs.SetFilter('', fltAll);
            SetTextFound(0);
          end;
        end;

      SDLK_DOWN:
        begin
          {SelectNext;
          Button[0].Text[0].Selected := (Interaction = 0);}
        end;

      SDLK_UP:
        begin
          {SelectPrev;
          Button[0].Text[0].Selected := (Interaction = 0); }
        end;

      SDLK_RIGHT:
        begin
          Interaction := 1;
          InteractInc;
          if (Length(Button[0].Text[0].Text) > 0) then
            SetTextFound(CatSongs.SetFilter(Button[0].Text[0].Text, fSelectType));
          Interaction := 0;
        end;
      SDLK_LEFT:
        begin
          Interaction := 1;
          InteractDec;
          if (Length(Button[0].Text[0].Text) > 0) then
            SetTextFound(CatSongs.SetFilter(Button[0].Text[0].Text, fSelectType));
          Interaction := 0;
        end;
    end;
  end;
end;

constructor TScreenSongJumpto.Create;
var
  ButtonID: integer;
begin
  inherited Create;

  AddText(Theme.SongJumpto.TextFound);

  LoadFromTheme(Theme.SongJumpto);

  ButtonID := AddButton(Theme.SongJumpto.ButtonSearchText);

  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, '');

  Button[ButtonID].Text[0].Writable := true;

  fSelectType := fltAll;
  AddSelectSlide(Theme.SongJumpto.SelectSlideType, PInteger(@fSelectType)^, Theme.SongJumpto.IType);

  Interaction := 0;
  fLastPlayed  := 0;
end;

procedure TScreenSongJumpto.SetVisible(Value: boolean);
begin
//If change from invisible to Visible then OnShow
  if (fVisible = false) and (Value = true) then
    OnShow;

  fVisible := Value;
end;

procedure TScreenSongJumpto.OnShow;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenSongJumpTo)');

  //Reset Screen if no Old Search is Displayed
  if (CatSongs.CatNumShow <> -2) then
  begin
    SelectsS[0].SetSelectOpt(0);

    Button[0].Text[0].Text := '';
    Text[0].Text := Theme.SongJumpto.NoSongsFound;
  end;

  //Select Input
  Interaction := 0;
  Button[0].Text[0].Selected := true;

  fLastPlayed := ScreenSong.Interaction;
end;

function TScreenSongJumpto.Draw: boolean;
begin
  Result := inherited Draw;
end;

procedure TScreenSongJumpto.SetTextFound(Count: cardinal);
begin
  if (Count = 0) then
  begin
    Text[0].Text := Theme.SongJumpto.NoSongsFound;
    if (Length(Button[0].Text[0].Text) = 0) then
      ScreenSong.HideCatTL
    else
      ScreenSong.ShowCatTLCustom(Format(Theme.SongJumpto.CatText, [Button[0].Text[0].Text]));
  end
  else
  begin
    Text[0].Text := Format(Theme.SongJumpto.SongsFound, [Count]);

    //Set CatTopLeftText
    ScreenSong.ShowCatTLCustom(Format(Theme.SongJumpto.CatText, [Button[0].Text[0].Text]));
  end;

  //Set visSongs
  fVisSongs := Count;

  //Fix SongSelection
  if (TSongMenuMode(Ini.SongMenu) in [smRoulette, smCarousel, smSlide, smSlotMachine]) then
  begin
    ScreenSong.Interaction := high(CatSongs.Song);
  end;

  if (TSongMenuMode(Ini.SongMenu) in [smChessboard, smList, smMosaic]) then
  begin
    ScreenSong.Interaction := 0;
    ScreenSong.ChessboardMinLine := 0;
    ScreenSong.ListMinLine := 0;
  end;

  ScreenSong.SelectNext;
  ScreenSong.FixSelected;

  ScreenSong.SetScrollRefresh;

  //Play Correct Music
  if (ScreenSong.Interaction <> fLastPlayed) or (CatSongs.VisibleSongs = 0) then
  begin
    if (CatSongs.VisibleSongs > 0) then
      fLastPlayed := ScreenSong.Interaction
    else
      fLastPlayed := -1;

    ScreenSong.ChangeMusic;
  end;
end;

end.
