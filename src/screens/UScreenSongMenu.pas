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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/screens/UScreenSongMenu.pas $
 * $Id: UScreenSongMenu.pas 2071 2010-01-12 17:42:41Z s_alexander $
 *}

unit UScreenSongMenu;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UDisplay,
  UFiles,
  UIni,
  UMenu,
  UMusic,
  UThemes,
  sdl2,
  dglOpenGL,
  SysUtils;

type
  TScreenSongMenu = class(TMenu)
    private
      CurMenu: byte; // num of the cur. shown menu
      ID:       String; //for help-system
    public
      Visible: boolean; // whether the menu should be drawn

      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; Repeated: boolean = false): boolean; override;
      procedure OnShow; override;
      function Draw: boolean; override;
      procedure MenuShow(sMenu: byte);
      procedure HandleReturn;
      function CountMedleySongs: integer;
      procedure UpdateJukeboxButtons;
  end;

const
  SM_Main = 1;

  SM_PlayList         = 64 or 1;
  SM_Playlist_Add     = 64 or 2;
  SM_Playlist_New     = 64 or 3;

  SM_Playlist_DelItem = 64 or 5;

  SM_Playlist_Load    = 64 or 8 or 1;
  SM_Playlist_Del     = 64 or 8 or 5;

  SM_Party_Main       = 128 or 1;
  SM_Party_Joker      = 128 or 2;
  SM_Party_Free_Main  = 128 or 5;

  SM_Refresh_Scores   = 64 or 6;
  SM_Song             = 64 or 8;
  SM_Medley           = 64 or 16;
  SM_Extra            = 64 or 64;
  SM_Jukebox          = 64 or 128;

var
  ISelections1: array of UTF8String;
  SelectValue1: integer;

  ISelections2: array of UTF8String;
  SelectValue2: integer;

  ISelections3: array of UTF8String;
  SelectValue3: integer;

implementation

uses
  UDatabase,
  UGraphic,
  UHelp,
  ULanguage,
  ULog,
  UMain,
  UNote,
  UParty,
  UPlaylist,
  USong,
  USongs,
  UTexture,
  UUnicodeUtils;

function TScreenSongMenu.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; Repeated: boolean = false): boolean;
var
  SDL_ModState:  Word;

begin
  Result := true;
  if (PressedDown) then
  begin // key down
    if (CurMenu = SM_Playlist_New) and (Interaction=1) then
    begin
      // check normal keys
      if IsAlphaNumericChar(CharCode) or
         (CharCode in [Ord(' '), Ord('-'), Ord('_'), Ord('!'),
                       Ord(','), Ord('<'), Ord('/'), Ord('*'),
                       Ord('?'), Ord(''''), Ord('"')]) then
      begin
        Button[Interaction].Text[0].Text := Button[Interaction].Text[0].Text +
                                            UCS4ToUTF8String(CharCode);
        exit;
      end;

      // check special keys
      case PressedKey of
        SDLK_BACKSPACE:
          begin
            Button[Interaction].Text[0].DeleteLastLetter;
            exit;
          end;
      end;
    end;

    // check normal keys unless a text field is actively selected
    if not ((CurMenu = SM_Playlist_New) and Button[1].Selected) then
    begin
      case PressedKey of
        SDLK_Q:
          begin
            Result := false;
            Exit;
          end;
      end;
    end;

    SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);

    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE:
        begin
          StopTextInput;
          AudioPlayback.PlaySound(SoundLib.Back);
          Visible := false;
        end;

      SDLK_TAB:
        begin
          Help.SetHelpID(ID);
          ScreenPopupHelp.ShowPopup();
        end;

      SDLK_RETURN:
        begin
          StopTextInput;
          HandleReturn;
        end;

      SDLK_DOWN:
        begin
          InteractNext;
          SetTextInput((CurMenu = SM_Playlist_New) and (Button[1].Selected));
        end;

      SDLK_UP:
        begin
          InteractPrev;
          SetTextInput((CurMenu = SM_Playlist_New) and (Button[1].Selected));
        end;

      SDLK_RIGHT:
        begin
          if (ScreenSong.Mode <> smJukebox) then
          begin
            if (Interaction=3) or (Interaction=4) or (Interaction=5)
              or (Interaction=8) or (Interaction=9) or (Interaction=10) then
                InteractInc;
          end
          else
          begin
            AudioPlayback.PlaySound(SoundLib.Change);
            ScreenSong.SelectNext;
            ScreenSong.SetScrollRefresh;
          end;
        end;
      SDLK_LEFT:
        begin
          if (ScreenSong.Mode <> smJukebox) then
          begin
            if (Interaction=3) or (Interaction=4) or (Interaction=5)
              or (Interaction=8) or (Interaction=9) or (Interaction=10) then
                InteractDec;
          end
          else
          begin
            AudioPlayback.PlaySound(SoundLib.Change);
            ScreenSong.SelectPrev;
            ScreenSong.SetScrollRefresh;
          end;
        end;

      SDLK_1:
        begin // joker
            // use joker
          case CurMenu of
            SM_Party_Main:
            begin
              ScreenSong.DoJoker(0, SDL_ModState)
            end;
          end;
        end;
      SDLK_2:
        begin // joker
            // use joker
          case CurMenu of
            SM_Party_Main:
            begin
              ScreenSong.DoJoker(1, SDL_ModState)
            end;
          end;
        end;
      SDLK_3:
        begin // joker
            // use joker
          case CurMenu of
            SM_Party_Main:
            begin
              ScreenSong.DoJoker(2, SDL_ModState)
            end;
          end;
        end;
    end; // case
  end; // if
end;

constructor TScreenSongMenu.Create;
begin
  inherited Create;

  // create dummy selectslide entrys
  SetLength(ISelections1, 1);
  ISelections1[0] := 'Dummy';

  SetLength(ISelections2, 1);
  ISelections2[0] := 'Dummy';

  SetLength(ISelections3, 1);
  ISelections3[0] := 'Dummy';

  AddText(Theme.SongMenu.TextMenu);

  LoadFromTheme(Theme.SongMenu);

  AddButton(Theme.SongMenu.Button1);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, 'Button 1');

  AddButton(Theme.SongMenu.Button2);
  if (Length(Button[1].Text) = 0) then
    AddButtonText(14, 20, 'Button 2');

  AddButton(Theme.SongMenu.Button3);
  if (Length(Button[2].Text) = 0) then
    AddButtonText(14, 20, 'Button 3');

  AddSelectSlide(Theme.SongMenu.SelectSlide1, SelectValue1, ISelections1);
  AddSelectSlide(Theme.SongMenu.SelectSlide2, SelectValue2, ISelections2);
  AddSelectSlide(Theme.SongMenu.SelectSlide3, SelectValue3, ISelections3);

  AddButton(Theme.SongMenu.Button4);
  if (Length(Button[3].Text) = 0) then
    AddButtonText(14, 20, 'Button 4');

  AddButton(Theme.SongMenu.Button5);
  if (Length(Button[4].Text) = 0) then
    AddButtonText(14, 20, 'Button 5');

  Interaction := 0;
end;

function TScreenSongMenu.Draw: boolean;
begin
  glClear(GL_DEPTH_BUFFER_BIT);
  Result := inherited Draw;
end;

procedure TScreenSongMenu.OnShow;
begin
  inherited;
end;

function TScreenSongMenu.CountMedleySongs: integer;
var
  Count, I: integer;
begin

  Count := 0;

  for I:= 0 to High(CatSongs.Song) do
  begin

    if (CatSongs.Song[I].Visible) and (CatSongs.Song[I].Medley.Source <> msNone) then
      Count := Count + 1;

    if (Count = 5) then
      break;

  end;

  Result := Count;
end;

procedure TScreenSongMenu.UpdateJukeboxButtons();
begin
   Button[1].Visible := not (CatSongs.Song[ScreenSong.Interaction].Main);
   Button[2].Visible := (Length(ScreenJukebox.JukeboxSongsList) > 0);

   if (CatSongs.Song[ScreenSong.Interaction].Main) then
     Button[0].Text[0].Text := Language.Translate('SONG_MENU_OPEN_CATEGORY')
   else
     Button[0].Text[0].Text := Language.Translate('SONG_MENU_CLOSE_CATEGORY');
end;

procedure TScreenSongMenu.MenuShow(sMenu: byte);
var
  I, MSongs: integer;
begin
  Interaction := 0; // reset interaction
  Visible := true;  // set visible
  case sMenu of
    SM_Main:
      begin
        ID := 'ID_017';
        CurMenu := sMenu;

        Text[0].Text := Language.Translate('SONG_MENU_NAME_MAIN');

        Button[0].Visible := true;
        Button[1].Visible := ((Length(PlaylistMedley.Song) > 0) or (CatSongs.Song[ScreenSong.Interaction].Medley.Source > msNone));
        Button[2].Visible := false;
        Button[3].Visible := true;
        Button[4].Visible := true;

        SelectsS[0].Visible := false;
        SelectsS[1].Visible := false;
        SelectsS[2].Visible := false;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_SONG');
        Button[1].Text[0].Text := Language.Translate('SONG_MENU_MEDLEY');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_REFRESH_SCORES');
        Button[4].Text[0].Text := Language.Translate('SONG_MENU_EXTRA');
      end;
    SM_Song:
      begin
        ID := 'ID_017';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_SONG');

        Button[0].Visible := true;
        Button[1].Visible := true;
        Button[2].Visible := true;
        Button[3].Visible := true;
        Button[4].Visible := true;

        SelectsS[0].Visible := false;
        SelectsS[1].Visible := false;
        SelectsS[2].Visible := false;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_PLAY');
        Button[1].Text[0].Text := Language.Translate('SONG_MENU_CHANGEPLAYERS');
        Button[2].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_ADD');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_EDIT');
        Button[4].Text[0].Text := Language.Translate('SONG_MENU_CANCEL');
      end;

    SM_Medley:
      begin
        ID := 'ID_017';
        CurMenu := sMenu;
        MSongs := CountMedleySongs;

        Text[0].Text := Language.Translate('SONG_MENU_NAME_MEDLEY');

        Button[0].Visible := (CatSongs.Song[ScreenSong.Interaction].Medley.Source > msNone);
        Button[1].Visible := (Length(PlaylistMedley.Song)>0);
        Button[2].Visible := (Length(PlaylistMedley.Song)>0) or
          (CatSongs.Song[ScreenSong.Interaction].Medley.Source > msNone);
        Button[3].Visible := (not ScreenSong.MakeMedley) and (MSongs > 1);
        Button[4].Visible := true;

        SelectsS[0].Visible := false;
        SelectsS[1].Visible := false;
        SelectsS[2].Visible := false;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_ADD_SONG');
        Button[1].Text[0].Text := Language.Translate('SONG_MENU_DELETE_SONG');
        Button[2].Text[0].Text := Language.Translate('SONG_MENU_START_MEDLEY');
        Button[3].Text[0].Text := Format(Language.Translate('SONG_MENU_START_5_MEDLEY'), [MSongs]);
        Button[4].Text[0].Text := Language.Translate('SONG_MENU_CANCEL');
      end;

    SM_PlayList:
      begin
        ID := 'ID_017';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PLAYLIST');

        Button[0].Visible := true;
        Button[1].Visible := true;
        Button[2].Visible := true;
        Button[3].Visible := true;
        Button[4].Visible := false;

        SelectsS[0].Visible := false;
        SelectsS[1].Visible := false;
        SelectsS[2].Visible := false;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_PLAY');
        Button[1].Text[0].Text := Language.Translate('SONG_MENU_CHANGEPLAYERS');
        Button[2].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_DEL');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_EDIT');
      end;

    SM_Playlist_Add:
      begin
        ID := 'ID_017';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PLAYLIST_ADD');

        Button[0].Visible := true;
        Button[1].Visible := false;
        Button[2].Visible := false;
        Button[3].Visible := true;
        Button[4].Visible := true;

        SelectsS[0].Visible := false;
        SelectsS[1].Visible := false;
        SelectsS[2].Visible := true;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_ADD_NEW');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_ADD_EXISTING');
        Button[4].Text[0].Text := Language.Translate('SONG_MENU_CANCEL');

        SetLength(ISelections3, Length(PlaylistMan.Playlists));
        PlaylistMan.GetNames(ISelections3);

        if (Length(ISelections3)>=1) then
        begin
          UpdateSelectSlideOptions(2, ISelections3, SelectValue3);
        end
        else
        begin
          Button[3].Visible := false;
          SelectsS[0].Visible := false;
          SelectsS[1].Visible := false;
          SelectsS[2].Visible := false;
          Button[2].Visible := true;
          Button[2].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_NOEXISTING');
        end;
      end;

    SM_Playlist_New:
      begin
        ID := 'ID_017';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PLAYLIST_NEW');

        Button[0].Visible := false;
        Button[1].Visible := true;
        Button[2].Visible := false;
        Button[3].Visible := true;
        Button[4].Visible := true;

        SelectsS[0].Visible := false;
        SelectsS[1].Visible := false;
        SelectsS[2].Visible := false;

        Button[1].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_NEW_UNNAMED');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_NEW_CREATE');
        Button[4].Text[0].Text := Language.Translate('SONG_MENU_CANCEL');

        Interaction := 1;
        // button 1 = a text field and pre-selected
        // it's necessary to start text input manually because it doesn't get here by Up/Down keypresses
        StartTextInput;
      end;

    SM_Playlist_DelItem:
      begin
        ID := 'ID_017';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PLAYLIST_DELITEM');

        Button[0].Visible := true;
        Button[1].Visible := false;
        Button[2].Visible := false;
        Button[3].Visible := true;
        Button[4].Visible := false;

        SelectsS[0].Visible := false;
        SelectsS[1].Visible := false;
        SelectsS[2].Visible := false;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_YES');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_CANCEL');
      end;

    SM_Playlist_Load:
      begin
        ID := 'ID_017';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PLAYLIST_LOAD');

        // show delete curent playlist button when playlist is opened
        Button[0].Visible := (CatSongs.CatNumShow = -3);

        Button[1].Visible := false;
        Button[2].Visible := false;
        Button[3].Visible := true;
        Button[4].Visible := false;

        SelectsS[0].Visible := false;
        SelectsS[1].Visible := false;
        SelectsS[2].Visible := true;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_DELCURRENT');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_LOAD');

        SetLength(ISelections3, Length(PlaylistMan.Playlists));
        PlaylistMan.GetNames(ISelections3);

        if (Length(ISelections3)>=1) then
        begin
          UpdateSelectSlideOptions(2, ISelections3, SelectValue3);
          Interaction := 3;
        end
        else
        begin
          Button[3].Visible := false;
          SelectsS[0].Visible := false;
          SelectsS[1].Visible := false;
          SelectsS[2].Visible := false;
          Button[2].Visible := true;
          Button[2].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_NOEXISTING');
          Interaction := 2;
        end;
      end;

    SM_Playlist_Del:
      begin
        ID := 'ID_017';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PLAYLIST_DEL');

        Button[0].Visible := true;
        Button[1].Visible := false;
        Button[2].Visible := false;
        Button[3].Visible := true;
        Button[4].Visible := false;

        SelectsS[0].Visible := false;
        SelectsS[1].Visible := false;
        SelectsS[2].Visible := false;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_YES');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_CANCEL');
      end;

    SM_Party_Main:
      begin
        ID := 'ID_018';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PARTY_MAIN');

        Button[0].Visible := true;
        Button[1].Visible := false;
        Button[2].Visible := false;
        Button[3].Visible := true;
        Button[4].Visible := false;

        SelectsS[0].Visible := false;
        SelectsS[1].Visible := false;
        SelectsS[2].Visible := false;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_PLAY');
        //Button[1].Text[0].Text := Language.Translate('SONG_MENU_JOKER');
        //Button[2].Text[0].Text := Language.Translate('SONG_MENU_PLAYMODI');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_JOKER');
      end;

    SM_Party_Joker:
      begin
        ID := 'ID_018';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PARTY_JOKER');
        // to-do : Party
        Button[0].Visible := (Length(Party.Teams) >= 1) AND (Party.Teams[0].JokersLeft > 0);
        Button[1].Visible := (Length(Party.Teams) >= 2) AND (Party.Teams[1].JokersLeft > 0);
        Button[2].Visible := (Length(Party.Teams) >= 3) AND (Party.Teams[2].JokersLeft > 0);
        Button[3].Visible := True;
        Button[4].Visible := false;

        SelectsS[0].Visible := False;
        SelectsS[1].Visible := False;
        SelectsS[2].Visible := False;

        if (Button[0].Visible) then
          Button[0].Text[0].Text := UTF8String(Party.Teams[0].Name);
        if (Button[1].Visible) then
          Button[1].Text[0].Text := UTF8String(Party.Teams[1].Name);
        if (Button[2].Visible) then
          Button[2].Text[0].Text := UTF8String(Party.Teams[2].Name);
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_CANCEL');

        // set right interaction
        if (not Button[0].Visible) then
        begin
          if (not Button[1].Visible) then
          begin
            if (not Button[2].Visible) then
              Interaction := 4
            else
              Interaction := 2;
          end
          else
            Interaction := 1;
        end;

      end;

    SM_Refresh_Scores:
      begin
        ID := 'ID_019';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_REFRESH_SCORES_TITLE');

        Button[0].Visible := false;
        Button[1].Visible := false;
        Button[2].Visible := false;
        Button[3].Visible := false;
        Button[4].Visible := true;

        SelectsS[0].Visible := true;
        SelectsS[1].Visible := true;
        SelectsS[2].Visible := true;

        Button[4].Text[0].Text := Language.Translate('SONG_MENU_REFRESH_SCORES_REFRESH');

        if (High(DataBase.NetworkUser) > 0) then
          SetLength(ISelections3, Length(DataBase.NetworkUser) + 1)
        else
          SetLength(ISelections3, Length(DataBase.NetworkUser));

        if (Length(ISelections3) >= 1) then
        begin
          if (High(DataBase.NetworkUser) > 0) then
          begin
            ISelections3[0] := Language.Translate('SONG_MENU_REFRESH_SCORES_ALL_WEB');
            for I := 0 to High(DataBase.NetworkUser) do
              ISelections3[I + 1] := DataBase.NetworkUser[I].Website;
          end
          else
          begin
            for I := 0 to High(DataBase.NetworkUser) do
              ISelections3[I] := DataBase.NetworkUser[I].Website;
          end;

          UpdateSelectSlideOptions(0, [Language.Translate('SONG_MENU_REFRESH_SCORES_ONLINE'), Language.Translate('SONG_MENU_REFRESH_SCORES_FILE')], SelectValue1);
          UpdateSelectSlideOptions(1, [Language.Translate('SONG_MENU_REFRESH_SCORES_ONLY_SONG'), Language.Translate('SONG_MENU_REFRESH_SCORES_ALL_SONGS')], SelectValue2);
          UpdateSelectSlideOptions(2, ISelections3, SelectValue3);

          Interaction := 3;
        end
        else
        begin
          Button[3].Visible := false;
          SelectsS[0].Visible := false;
          SelectsS[1].Visible := false;
          SelectsS[2].Visible := false;
          Button[2].Visible := true;
          Button[2].Text[0].Text := Language.Translate('SONG_MENU_REFRESH_SCORES_NO_WEB');
          Button[2].Selectable := false;
          Button[3].Text[0].Text := Theme.Options.Description[OPTIONS_DESC_INDEX_NETWORK];
          Interaction := 7;
        end;
      end;

    SM_Party_Free_Main:
      begin
        ID := 'ID_017';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PARTY_MAIN');

        Button[0].Visible := true;
        Button[1].Visible := false;
        Button[2].Visible := false;
        Button[3].Visible := false;

        SelectsS[0].Visible := false;
        SelectsS[1].Visible := false;
        SelectsS[2].Visible := false;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_PLAY');
      end;
    SM_Extra:
      begin
        ID := 'ID_020';
      end;
    SM_Jukebox:
      begin
        ID := 'ID_021';
        CurMenu := sMenu;

        Text[0].Text := Language.Translate('SONG_MENU_NAME_JUKEBOX');

        UpdateJukeboxButtons();

        Button[0].Visible := (Ini.TabsAtStartup = 1);
        Button[3].Visible := false;
        Button[4].Visible := true;

        SelectsS[0].Visible := false;
        SelectsS[1].Visible := false;
        SelectsS[2].Visible := false;

        Button[1].Text[0].Text := Language.Translate('SONG_MENU_ADD_SONG');
        Button[2].Text[0].Text := Language.Translate('SONG_MENU_DELETE_SONG');

        Button[4].Text[0].Text := Language.Translate('SONG_MENU_START_JUKEBOX');

        if (Ini.TabsAtStartup = 1) then
          Interaction := 0
        else
          Interaction := 1;

      end;
  end;
  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenSongMenu');
end;

procedure TScreenSongMenu.HandleReturn;
var
  I: integer;
  SDL_ModState:  Word;
begin
  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);
  case CurMenu of
    SM_Main:
      begin
        case Interaction of
          0: // button 1
            begin
              MenuShow(SM_Song);
            end;

          1: // button 2
            begin
              MenuShow(SM_Medley);
            end;

          2: // button 3
            begin
              //Dummy
            end;

          3: // selectslide 1
            begin
              //Dummy
            end;

          4: // selectslide 2
            begin
              //Dummy
            end;

          5: // selectslide 3
            begin
              //Dummy
            end;

          6: // button 4
            begin
              // show refresh scores menu
              MenuShow(SM_Refresh_Scores);
              ScreenSong.StopMusicPreview();
              ScreenSong.StopVideoPreview();
            end;
          7: // button 5
            begin
              ScreenPopupError.ShowPopup(Language.Translate('PARTY_MODE_NOT_AVAILABLE'));
              // show extras
              //MenuShow(SM_Extra);
            end;
          end;
      end;

      SM_Song:
      begin
        case Interaction of
          0: // button 1
            begin

              //if (CatSongs.Song[ScreenSong.Interaction].isDuet and ((PlayersPlay=1) or
              //   (PlayersPlay=3) or (PlayersPlay=6))) then
              //  ScreenPopupError.ShowPopup(Language.Translate('SING_ERROR_DUET_NUM_PLAYERS'))
              //else
              //begin
                ScreenSong.StartSong;
                Visible := false;
              //end;
            end;

          1: // button 2
            begin
              // select new players then sing:
              ScreenSong.SelectPlayers;
              Visible := false;
            end;

          2: // button 3
            begin
              // show add to playlist menu
              MenuShow(SM_Playlist_Add);
            end;

          3: // selectslide 1
            begin
              //Dummy
            end;

          4: // selectslide 2
            begin
              //Dummy
            end;

          5: // selectslide 3
            begin
              //Dummy
            end;

          6: // button 4
            begin
              ScreenSong.OpenEditor;
              Visible := false;
            end;

          7: // button 5
            begin
              // show main menu
              MenuShow(SM_Main);
            end;
          end;
      end;

    SM_Medley:
      begin
        Case Interaction of
          0: //Button 1
            begin
              ScreenSong.MakeMedley := true;
              ScreenSong.StartMedley(99, msCalculated);

              Visible := False;
            end;

          1: //Button 2
            begin
              SetLength(PlaylistMedley.Song, Length(PlaylistMedley.Song)-1);
              PlaylistMedley.NumMedleySongs := Length(PlaylistMedley.Song);

              if Length(PlaylistMedley.Song)=0 then
                ScreenSong.MakeMedley := false;

              Visible := False;
            end;

          2: //Button 3
            begin

              if ScreenSong.MakeMedley then
              begin
                ScreenSong.Mode := smMedley;
                PlaylistMedley.CurrentMedleySong := 0;

                //Do the Action that is specified in Ini
                case Ini.OnSongClick of
                  0: FadeTo(@ScreenSing);
                  1: ScreenSong.SelectPlayers;
                  2: FadeTo(@ScreenSing);
                end;
              end
              else
                ScreenSong.StartMedley(0, msCalculated);

              Visible := False;

            end;

          6: //Button 4
            begin
              ScreenSong.StartMedley(5, msCalculated);
              Visible := False;
            end;

          7: // button 5
            begin
              // show main menu
              MenuShow(SM_Main);
            end;
        end;
      end;

    SM_PlayList:
      begin
        Visible := false;
        case Interaction of
          0: // button 1
            begin
              ScreenSong.StartSong;
              Visible := false;
            end;

          1: // button 2
            begin
              // select new players then sing:
              ScreenSong.SelectPlayers;
              Visible := false;
            end;

          2: // button 3
            begin
              // show add to playlist menu
              MenuShow(SM_Playlist_DelItem);
            end;

          3: // selectslide 1
            begin
              // dummy
            end;

          4: // selectslide 2
            begin
              // dummy
            end;

          5: // selectslide 3
            begin
              // dummy
            end;

          6: // button 4
            begin
              ScreenSong.OpenEditor;
              Visible := false;
            end;
        end;
      end;

    SM_Playlist_Add:
      begin
        case Interaction of
          0: // button 1
            begin
              MenuShow(SM_Playlist_New);
            end;

          4: // selectslide 3
            begin
              // dummy
            end;

          6: // button 4
            begin
              PlaylistMan.AddItem(ScreenSong.Interaction, SelectValue3);
              Visible := false;
            end;

          7: // button 5
            begin
              // show song menu
              MenuShow(SM_Song);
            end;
        end;
      end;

      SM_Playlist_New:
      begin
        case Interaction of
          1: // button 1
            begin
              // nothing, button for entering name
            end;

          6: // button 4
            begin
              // create playlist and add song
              PlaylistMan.AddItem(
              ScreenSong.Interaction,
              PlaylistMan.AddPlaylist(Button[1].Text[0].Text));
              Visible := false;
            end;

          7: // button 5
            begin
              // show add song menu
              MenuShow(SM_Playlist_Add);
            end;

        end;
      end;

    SM_Playlist_DelItem:
      begin
        Visible := false;
        case Interaction of
          0: // button 1
            begin
              // delete
              PlayListMan.DelItem(PlayListMan.GetIndexbySongID(ScreenSong.Interaction));
              Visible := false;
            end;

          6: // button 4
            begin
              MenuShow(SM_Playlist);
            end;
        end;
      end;

    SM_Playlist_Load:
      begin
        case Interaction of
          0: // button 1 (Delete playlist)
            begin
              MenuShow(SM_Playlist_Del);
            end;
          6: // button 4
            begin
              // load playlist
              PlaylistMan.ReloadPlaylist(SelectValue3);
              PlaylistMan.SetPlayList(SelectValue3);
              Visible := false;
              ScreenSong.SelectNext(false);
              ScreenSong.SetScrollRefresh;
            end;
        end;
      end;

    SM_Playlist_Del:
      begin
        Visible := false;
        case Interaction of
          0: // button 1
            begin
              // delete
              PlayListMan.DelPlaylist(PlaylistMan.CurPlayList);
              Visible := false;
            end;

          6: // button 4
            begin
              MenuShow(SM_Playlist_Load);
            end;
        end;
      end;

    SM_Party_Main:
      begin
        case Interaction of
          0: // button 1
            begin
              // start singing
              Party.CallAfterSongSelect;
              Visible := false;
            end;

          6: // button 4
            begin
              // joker
              MenuShow(SM_Party_Joker);
            end;
        end;
      end;

    SM_Party_Free_Main:
    begin
      case Interaction of
        0: // button 1
          begin
            // start singing
            Party.CallAfterSongSelect;
            Visible := false;
          end;
      end;
    end;

    SM_Party_Joker:
      begin
        Visible := false;
        case Interaction of
          0: // button 1
            begin
              // joker team 1
              ScreenSong.DoJoker(0, SDL_ModState);
            end;

          1: // button 2
            begin
              // joker team 2
              ScreenSong.DoJoker(1, SDL_ModState);
            end;

          2: // button 3
            begin
              // joker team 3
              ScreenSong.DoJoker(2, SDL_ModState);
            end;

          6: // button 4
            begin
              // cancel... (go back to old menu)
              MenuShow(SM_Party_Main);
            end;
        end;
      end;

    SM_Refresh_Scores:
      begin
        case Interaction of
          7: // button 5
            begin
              if (Length(ISelections3)>=1) then
              begin
                // Refresh Scores
                Visible := false;
                ScreenPopupScoreDownload.ShowPopup(SelectValue1, SelectValue2, SelectValue3);
              end
              else
              begin
                Button[2].Selectable := true;
                MenuShow(SM_Main);
              end;
            end;
        end;
      end;

    SM_Jukebox:
      begin
        Case Interaction of
          0: //Button 1
            begin

              if (Songs.SongList.Count > 0) then
              begin
                if CatSongs.Song[ScreenSong.Interaction].Main then
                begin // clicked on Category Button
                  //Show Cat in Top Left Mod
                  ScreenSong.ShowCatTL(ScreenSong.Interaction);

                  CatSongs.ClickCategoryButton(ScreenSong.Interaction);

                  //Show Wrong Song when Tabs on Fix
                  ScreenSong.SelectNext;
                  ScreenSong.FixSelected;
                end
                else
                begin
                  //Find Category
                  I := ScreenSong.Interaction;
                  while (not CatSongs.Song[I].Main) do
                  begin
                    Dec(I);
                    if (I < 0) then
                      break;
                  end;

                  if (I <= 1) then
                    ScreenSong.Interaction := High(CatSongs.Song)
                  else
                    ScreenSong.Interaction := I - 1;

                  //Stop Music
                  ScreenSong.StopMusicPreview();

                  CatSongs.ShowCategoryList;

                  //Show Cat in Top Left Mod
                  ScreenSong.HideCatTL;

                  //Show Wrong Song when Tabs on Fix
                  ScreenSong.SelectNext;
                  ScreenSong.FixSelected;
                end;
              end;

              UpdateJukeboxButtons;
            end;

          1: //Button 2
            begin
              if (not CatSongs.Song[Interaction].Main) then
                ScreenJukebox.AddSongToJukeboxList(ScreenSong.Interaction);

              UpdateJukeboxButtons;
            end;

          2: //Button 3
            begin
              SetLength(ScreenJukebox.JukeboxSongsList, Length(ScreenJukebox.JukeboxSongsList)-1);
              SetLength(ScreenJukebox.JukeboxVisibleSongs, Length(ScreenJukebox.JukeboxVisibleSongs)-1);

              if (Length(ScreenJukebox.JukeboxSongsList) = 0) then
                Interaction := 1;

              UpdateJukeboxButtons;
            end;

          7: //Button 4
            begin
              if (Length(ScreenJukebox.JukeboxSongsList) > 0) then
              begin
                ScreenJukebox.CurrentSongID := ScreenJukebox.JukeboxVisibleSongs[0];
                FadeTo(@ScreenJukebox);
                Visible := False;
              end
              else
                ScreenPopupError.ShowPopup(Language.Translate('PARTY_MODE_JUKEBOX_NO_SONGS'));
            end;
        end;
      end;

  end;
end;

end.
