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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/screens/UScreenPartyOptions.pas $
 * $Id: UScreenPartyOptions.pas 2146 2010-02-22 18:27:15Z k-m_schindler $
 *}

unit UScreenPartyOptions;

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
  UNote,
  sdl2,
  SysUtils,
  UThemes;

type
  TScreenPartyOptions = class(TMenu)
    private
      SelectMode:     cardinal;
      SelectLevel:     cardinal;
      SelectPlayList:  cardinal;
      SelectPlayList2: cardinal;
      SelectRounds:    cardinal;

      ILevel:     array of UTF8String;
      IPlaylist:  array of UTF8String;
      IPlaylist2: array of UTF8String;

      Level:     integer;
      PlayList:  integer;
      PlayList2: integer;
      Mode:      integer;

      procedure FillLevel;
      procedure FillPlaylist;
      procedure FillPlaylistJukebox;
      procedure SetPlaylists;
      procedure SetPlaylist2;
    public
      constructor Create; override;
      function ParseInput(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean; override;
      procedure OnShow; override;
      procedure SetAnimationProgress(Progress: real); override;

      procedure InitClassic;
      procedure InitFree;
      procedure InitChallenge;
      procedure InitTournament;
  end;

const
  ID='ID_030';   //for help system

implementation

uses
  UGraphic,
  UHelp,
  UIni,
  ULanguage,
  ULog,
  UMain,
  UParty,
  UPlaylist,
  USong,
  USongs,
  UTexture,
  UUnicodeUtils;

function TScreenPartyOptions.ParseInput(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
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
          FadeTo(@ScreenMain);
        end;

      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;

      SDLK_RETURN:
        begin
          // restart time
          //if (ScreenSong.Mode = smPartyTournament) then
          //  ScreenSong.CurrentPartyTime := 0;

          //Don't start when Playlist is Selected and there are no Playlists
          if (Playlist = 3) and (Length(PlaylistMan.Playlists) = 0) then
            Exit;

          //Save Difficulty
          Ini.Difficulty := SelectsS[SelectLevel].SelectedOption;
          Ini.SaveLevel;

          case Mode of
            0: InitClassic;
            1: InitFree;
            2: InitChallenge;
            3: InitTournament;
          end;

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
          if (Interaction = SelectPlayList) then
          begin
            SetPlaylist2;
          end;

          //Change Playlists if Mode is Changed
          if (Interaction = SelectMode) then
          begin
            SetPlaylists;
          end;
        end;
      SDLK_LEFT:
        begin
          AudioPlayback.PlaySound(SoundLib.Option);
          InteractDec;

          //Change Playlist2 if Playlist is Changed
          if (Interaction = SelectPlayList) then
          begin
            SetPlaylist2;
          end;

          //Change Playlists if Mode is Changed
          if (Interaction = SelectMode) then
          begin
            SetPlaylists;
          end;
        end;
    end;
  end;
end;

constructor TScreenPartyOptions.Create;
begin
  inherited Create;

  //Fill ILevel
  FillLevel;

  //Fill IPlaylist
  FillPlaylist;

  //Fill IPlaylist2
  SetLength(IPlaylist2, 1);
  IPlaylist2[0] := '---';

  //Clear all Selects
  PlayList := 0;
  PlayList2 := 0;
  Mode := 0;

  //Load Screen From Theme
  LoadFromTheme(Theme.PartyOptions);

  Theme.PartyOptions.SelectMode.oneItemOnly := true;
  Theme.PartyOptions.SelectMode.showArrows := true;
  SelectMode     := AddSelectSlide(Theme.PartyOptions.SelectMode, Mode, Theme.IMode);

  Theme.PartyOptions.SelectLevel.oneItemOnly := true;
  Theme.PartyOptions.SelectLevel.showArrows := true;
  SelectLevel     := AddSelectSlide(Theme.PartyOptions.SelectLevel, Ini.Difficulty, Theme.ILevel);

  Theme.PartyOptions.SelectPlayList.oneItemOnly := true;
  Theme.PartyOptions.SelectPlayList.showArrows := true;
  SelectPlayList  := AddSelectSlide(Theme.PartyOptions.SelectPlayList, PlayList, IPlaylist);

  Theme.PartyOptions.SelectPlayList2.oneItemOnly := true;
  Theme.PartyOptions.SelectPlayList2.showArrows := true;
  SelectPlayList2 := AddSelectSlide(Theme.PartyOptions.SelectPlayList2, PlayList2, IPlaylist2);

  FillLevel;

  Interaction := 0;
end;

procedure TScreenPartyOptions.FillPlaylist;
begin
  SetLength(IPlaylist, 3);

  IPlaylist[0] := Language.Translate('PARTY_PLAYLIST_ALL');
  IPlaylist[1] := Language.Translate('PARTY_PLAYLIST_CATEGORY');
  IPlaylist[2] := Language.Translate('PARTY_PLAYLIST_PLAYLIST');
end;

procedure TScreenPartyOptions.FillPlaylistJukebox;
begin
  SetLength(IPlaylist, 3);

  IPlaylist[0] := Language.Translate('PARTY_PLAYLIST_ALL');
  IPlaylist[1] := Language.Translate('PARTY_PLAYLIST_CATEGORY');
  IPlaylist[2] := Language.Translate('PARTY_PLAYLIST_PLAYLIST');
end;

procedure TScreenPartyOptions.FillLevel;
begin
  SetLength(ILevel, 3);

  ILevel[0] := Language.Translate('SING_EASY');
  ILevel[1] := Language.Translate('SING_MEDIUM');
  ILevel[2] := Language.Translate('SING_HARD');
end;

procedure TScreenPartyOptions.SetPlaylists;
var
  I: integer;
begin
  if (Mode = 1) or (Mode = 2) or (Mode = 3) then
  begin
    SetLength(IPlaylist, 1);
    IPlaylist[0] := '---';

    SetLength(IPlaylist2, 1);
    IPlaylist2[0] := '---';

    Playlist  := 0;
    Playlist2 := 0;

    UpdateSelectSlideOptions(Theme.PartyOptions.SelectLevel, SelectLevel, ILevel, Level);
    UpdateSelectSlideOptions(Theme.PartyOptions.SelectPlayList, SelectPlayList, IPlaylist, Playlist);
    UpdateSelectSlideOptions(Theme.PartyOptions.SelectPlayList2, SelectPlayList2, IPlaylist2, Playlist2);
  end
  else
  begin
    UpdateSelectSlideOptions(Theme.PartyOptions.SelectLevel, SelectLevel, ILevel, Level);

    FillPlaylist;

    UpdateSelectSlideOptions(Theme.PartyOptions.SelectPlayList, SelectPlayList, IPlaylist, Playlist);

    SetPlaylist2;
  end;

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
    3:
      begin
        SetLength(IPlaylist2, 1);
        IPlaylist2[0] := '---';
      end;
  end;

  Playlist2 := 0;
  UpdateSelectSlideOptions(Theme.PartyOptions.SelectPlayList2, SelectPlayList2, IPlaylist2, Playlist2);
end;

procedure TScreenPartyOptions.OnShow;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenPartyOptions');

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

procedure TScreenPartyOptions.InitClassic;
var
  I, J: integer;
begin

  //Save Playlist
  PlaylistMan.Mode := TSongMode(Playlist);
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
  begin
    //if Playlist = 2 then
    //  PlayListMan.SetPlayList(Playlist2)
    //else
      PlaylistMan.CurPlayList := Playlist2;
  end;

  ScreenSong.Mode := smPartyClassic;

  AudioPlayback.PlaySound(SoundLib.Start);
  //Go to Player Screen
  FadeTo(@ScreenPartyPlayer);
end;

procedure TScreenPartyOptions.InitFree;
begin
  ScreenSong.Mode := smPartyFree;
  AudioPlayback.PlaySound(SoundLib.Start);
  FadeTo(@ScreenPartyPlayer);
end;

procedure TScreenPartyOptions.InitChallenge;
begin
  ScreenSong.Mode := smPartyChallenge;
  ScreenPopupError.ShowPopup(Language.Translate('PARTY_MODE_NOT_AVAILABLE'));
end;

procedure TScreenPartyOptions.InitTournament;
begin
  ScreenSong.Mode := smPartyTournament;
  AudioPlayback.PlaySound(SoundLib.Start);
  FadeTo(@ScreenPartyTournamentPlayer);
end;

procedure TScreenPartyOptions.SetAnimationProgress(Progress: real);
begin
  //for I := 0 to 6 do
  //  SelectS[I].Texture.ScaleW := Progress;
end;

end.

