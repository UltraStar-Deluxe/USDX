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

unit UScreenPartyOptions;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu,
  SDL,
  UDisplay,
  UMusic,
  UFiles,
  SysUtils,
  UThemes;

type
  TScreenPartyOptions = class(TMenu)
    private
      SelectLevel:     cardinal;
      SelectPlayList:  cardinal;
      SelectPlayList2: cardinal;
      SelectRounds:    cardinal;

      IPlaylist: array[0..2] of UTF8String;
      IPlaylist2: array of UTF8String;

      PlayList:  integer;
      PlayList2: integer;
      
      procedure SetPlaylist2;
    public 
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

implementation

uses
  UGraphic,
  UMain,
  UIni,
  UTexture,
  ULanguage,
  UParty,
  USong,
  UDLLManager,
  UPlaylist,
  USongs,
  UUnicodeUtils;

function TScreenPartyOptions.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  I, J: integer;
  OnlyMultiPlayer: boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
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
      SDLK_BACKSPACE :
        begin
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenMain);
        end;

      SDLK_RETURN:
        begin
          //Don'T start when Playlist is Selected and there are no Playlists
          if (Playlist = 2) and (Length(PlaylistMan.Playlists) = 0) then
            Exit;

          //Save Difficulty
          Ini.Difficulty := SelectsS[SelectLevel].SelectedOption;
          Ini.SaveLevel;

          //Save Playlist
          PlaylistMan.Mode := TSingMode( Playlist );
          PlaylistMan.CurPlayList := High(cardinal);
          //if Category Selected Search Category ID
          if Playlist = 1 then
          begin
            J := -1;
            for I := 0 to high(CatSongs.Song) do
            begin
              if CatSongs.Song[I].Main then
                Inc(J);

              if J = Playlist2 then
              begin
                PlaylistMan.CurPlayList := I;
                Break;
              end;
            end;

            //No Categorys or Invalid Entry
            if PlaylistMan.CurPlayList = High(cardinal) then
              Exit;
          end
          else
            PlaylistMan.CurPlayList := Playlist2;

          AudioPlayback.PlaySound(SoundLib.Start);
          //Go to Player Screen
          FadeTo(@ScreenPartyPlayer);
        end;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:    InteractNext;
      SDLK_UP:      InteractPrev;
      SDLK_RIGHT:
        begin
          AudioPlayback.PlaySound(SoundLib.Option);
          InteractInc;

          //Change Playlist2 if Playlist is Changed
          if (Interaction = 1) then
          begin
            SetPlaylist2;
          end;
        end;
      SDLK_LEFT:
        begin
          AudioPlayback.PlaySound(SoundLib.Option);
          InteractDec;

          //Change Playlist2 if Playlist is Changed
          if (Interaction = 1) then
          begin
            SetPlaylist2;
          end;
        end;
    end;
  end;
end;

constructor TScreenPartyOptions.Create;
begin
  inherited Create;
  //Fill IPlaylist
  IPlaylist[0] := Language.Translate('PARTY_PLAYLIST_ALL');
  IPlaylist[1] := Language.Translate('PARTY_PLAYLIST_CATEGORY');
  IPlaylist[2] := Language.Translate('PARTY_PLAYLIST_PLAYLIST');

  //Fill IPlaylist2
  SetLength(IPlaylist2, 1);
  IPlaylist2[0] := '---';

  //Clear all Selects
  PlayList := 0;
  PlayList2 := 0;

  //Load Screen From Theme
  LoadFromTheme(Theme.PartyOptions);

  Theme.PartyOptions.SelectLevel.oneItemOnly := true;
  Theme.PartyOptions.SelectLevel.showArrows := true;
  SelectLevel     := AddSelectSlide(Theme.PartyOptions.SelectLevel, Ini.Difficulty, Theme.ILevel);

  Theme.PartyOptions.SelectPlayList.oneItemOnly := true;
  Theme.PartyOptions.SelectPlayList.showArrows := true;
  SelectPlayList  := AddSelectSlide(Theme.PartyOptions.SelectPlayList, PlayList, IPlaylist);

  Theme.PartyOptions.SelectPlayList2.oneItemOnly := true;
  Theme.PartyOptions.SelectPlayList2.showArrows := true;
  SelectPlayList2 := AddSelectSlide(Theme.PartyOptions.SelectPlayList2, PlayList2, IPlaylist2);

  Interaction := 0;
end;

procedure TScreenPartyOptions.SetPlaylist2;
var
  I: integer;
begin
  case Playlist of
    0:
      begin
        SetLength(IPlaylist2, 1);
        IPlaylist2[0] := '---';
      end;
    1:
      begin
        SetLength(IPlaylist2, 0);
        for I := 0 to high(CatSongs.Song) do
        begin
          if (CatSongs.Song[I].Main) then
          begin
            SetLength(IPlaylist2, Length(IPlaylist2) + 1);
            IPlaylist2[high(IPlaylist2)] := CatSongs.Song[I].Artist;
          end;
        end;

        if (Length(IPlaylist2) = 0) then
        begin
          SetLength(IPlaylist2, 1);
          IPlaylist2[0] := 'No Categories found';
        end;
      end;
    2:
      begin
        if (Length(PlaylistMan.Playlists) > 0) then
        begin
          SetLength(IPlaylist2, Length(PlaylistMan.Playlists));
          PlaylistMan.GetNames(IPlaylist2);
        end
        else
        begin
          SetLength(IPlaylist2, 1);
          IPlaylist2[0] := 'No Playlists found';
        end;
      end;
  end;

  Playlist2 := 0;
  UpdateSelectSlideOptions(Theme.PartyOptions.SelectPlayList2, 2, IPlaylist2, Playlist2);
end;

procedure TScreenPartyOptions.OnShow;
begin
  inherited;

  Party.Clear;

  // check if there are loaded modes
  if Party.ModesAvailable then
  begin
    // modes are loaded
    Randomize;
  end
  else
  begin // no modes found
    ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_PLUGINS'));
    Display.AbortScreenChange;
  end;
end;

procedure TScreenPartyOptions.SetAnimationProgress(Progress: real);
begin
  {for I := 0 to 6 do
    SelectS[I].Texture.ScaleW := Progress;}
end;

end.
