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
 * $URL: $
 * $Id: $
 *}

unit UScreenJukeboxPlaylist;

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
  UThemes,
  sdl2,
  SysUtils;

type
  TScreenJukeboxPlaylist = class(TMenu)
    private
      SelectPlayList:  cardinal;
      SelectPlayList2: cardinal;

      IPlaylist:  array of UTF8String;
      IPlaylist2: array of UTF8String;

      PlayList:  integer;
      PlayList2: integer;

      procedure SetPlaylists;
    public
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure SetAnimationProgress(Progress: real); override;

      procedure InitJukebox;
  end;

const
  ID='ID_040';   //for help system

implementation

uses
  UGraphic,
  UHelp,
  UMain,
  UIni,
  UTexture,
  ULanguage,
  ULog,
  UParty,
  USong,
  UPlaylist,
  USongs,
  UUnicodeUtils;

function TScreenJukeboxPlaylist.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  Report: string;
  I: Integer;
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

      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;

      SDLK_RETURN:
        begin
          //Don'T start when Playlist is Selected and there are no Playlists
          if (Playlist = 3) and (Length(PlaylistMan.Playlists) = 0) then
            Exit;

          try
            InitJukebox;
          except
            on E : Exception do
            begin
	      Report := 'Starting jukebox failed. Most likely no folder / empty folder / paylist with not available songs was selected.' + LineEnding +
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
	      Log.LogWarn(Report, 'UScreenJokeboxPlaylist.ParseInput');
            end;
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
            SetPlaylists;
          end;
          
        end;
    end;
  end;
end;

constructor TScreenJukeboxPlaylist.Create;
begin
  inherited Create;

  //Clear all Selects
  PlayList := 0;
  PlayList2 := 0;

  // playlist modes
  SetLength(IPlaylist2, 1);
  IPlaylist2[0] := '---';

  SetLength(IPlaylist, 4);

  IPlaylist[0] := Language.Translate('PARTY_PLAYLIST_ALL');
  IPlaylist[1] := Language.Translate('PARTY_PLAYLIST_CATEGORY');
  IPlaylist[2] := Language.Translate('PARTY_PLAYLIST_PLAYLIST');
  IPlaylist[3] := Language.Translate('PARTY_PLAYLIST_MANUAL');

  //Load Screen From Theme
  LoadFromTheme(Theme.JukeboxPlaylist);

  Theme.JukeboxPlaylist.SelectPlayList.oneItemOnly := true;
  Theme.JukeboxPlaylist.SelectPlayList.showArrows := true;
  SelectPlayList  := AddSelectSlide(Theme.JukeboxPlaylist.SelectPlayList, PlayList, IPlaylist);

  Theme.JukeboxPlaylist.SelectPlayList2.oneItemOnly := true;
  Theme.JukeboxPlaylist.SelectPlayList2.showArrows := true;
  SelectPlayList2 := AddSelectSlide(Theme.JukeboxPlaylist.SelectPlayList2, PlayList2, IPlaylist2);

  Interaction := 0;
end;

procedure TScreenJukeboxPlaylist.SetPlaylists;
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

procedure TScreenJukeboxPlaylist.OnShow;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenJukeboxPlaylist)');

end;

procedure TScreenJukeboxPlaylist.InitJukebox;
var
  I, J: integer;
begin
  ScreenSong.Mode := smJukebox;
  AudioPlayback.PlaySound(SoundLib.Start);

  SetLength(ScreenJukebox.JukeboxSongsList, 0);
  SetLength(ScreenJukebox.JukeboxVisibleSongs, 0);

  ScreenJukebox.ActualInteraction := 0;
  ScreenJukebox.CurrentSongList := 0;
  ScreenJukebox.ListMin := 0;
  ScreenJukebox.Interaction := 0;

  if PlayList = 0 then
  begin
    for I := 0 to High(CatSongs.Song) do
    begin
      if not (CatSongs.Song[I].Main) then
        ScreenJukebox.AddSongToJukeboxList(I);
    end;

    ScreenJukebox.CurrentSongID := ScreenJukebox.JukeboxVisibleSongs[0];

    FadeTo(@ScreenJukebox);
  end;

  if Playlist = 1 then
  begin
    J := -1;
    for I := 0 to high(CatSongs.Song) do
    begin
      if CatSongs.Song[I].Main then
        Inc(J);

      if J = Playlist2 then
      begin
        ScreenJukebox.AddSongToJukeboxList(I);
      end;
    end;

    ScreenJukebox.CurrentSongID := ScreenJukebox.JukeboxVisibleSongs[0];

    FadeTo(@ScreenJukebox);
  end;

  if Playlist = 2 then
  begin
    if(High(PlaylistMan.PlayLists[Playlist2].Items)>0) then
    begin
    for I := 0 to High(PlaylistMan.PlayLists[Playlist2].Items) do
    begin
      ScreenJukebox.AddSongToJukeboxList(PlaylistMan.PlayLists[Playlist2].Items[I].SongID);
    end;

    ScreenJukebox.CurrentSongID := ScreenJukebox.JukeboxVisibleSongs[0];

    FadeTo(@ScreenJukebox);
    end
    else
    begin
      Log.LogWarn('Can not play selected playlist in JukeBox because playlist is empty or no song found.', 'ScreenJukeboxPlaylist.InitJukeBox');
    end;
  end;

  if PlayList = 3 then
  begin
    FadeTo(@ScreenSong);
  end;

end;

procedure TScreenJukeboxPlaylist.SetAnimationProgress(Progress: real);
begin
  //for I := 0 to 6 do
  //  SelectS[I].Texture.ScaleW := Progress;
end;

end.

