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

unit UScreenSongMenu;

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
  TScreenSongMenu = class(TMenu)
    private
      CurMenu: Byte; //Num of the cur. Shown Menu
    public
      Visible: Boolean; //Whether the Menu should be Drawn

      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      function Draw: boolean; override;
      procedure MenuShow(sMenu: Byte);
      procedure HandleReturn;
  end;

const
  SM_Main = 1;
  
  SM_PlayList = 64 or 1;
  SM_Playlist_Add = 64 or 2;
  SM_Playlist_New = 64 or 3;

  SM_Playlist_DelItem = 64 or 5;

  SM_Playlist_Load = 64 or 8 or 1;
  SM_Playlist_Del  = 64 or 8 or 5;


  SM_Party_Main = 128 or 1;
  SM_Party_Joker = 128 or 2;

var
  ISelections: Array of String;
  SelectValue: Integer;


implementation

uses UGraphic,
     UMain,
     UIni,
     UTexture,
     ULanguage,
     UParty,
     UPlaylist,
     USongs;

function TScreenSongMenu.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    if (CurMenu = SM_Playlist_New) AND (Interaction=0) then
    begin
      // check normal keys
      case WideCharUpperCase(CharCode)[1] of
        '0'..'9', 'A'..'Z', ' ', '-', '_', '!', ',', '<', '/', '*', '?', '''', '"':
          begin
            Button[Interaction].Text[0].Text := Button[Interaction].Text[0].Text + CharCode;
            exit;
          end;
      end;

      // check special keys
      case PressedKey of
        SDLK_BACKSPACE:
          begin
            Button[Interaction].Text[0].DeleteLastL;
            exit;
          end;
      end;
    end;

    // check normal keys
    case WideCharUpperCase(CharCode)[1] of
      'Q':
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
          Visible := False;
        end;

      SDLK_RETURN:
        begin
          HandleReturn;
        end;

      SDLK_DOWN:    InteractNext;
      SDLK_UP:      InteractPrev;

      SDLK_RIGHT:
        begin
          if (Interaction=3) then
            InteractInc;
        end;
      SDLK_LEFT:
        begin
          if (Interaction=3) then
            InteractDec;
        end;

      SDLK_1:
        begin //Jocker
            //Use Joker
          case CurMenu of
            SM_Party_Main:
            begin
              ScreenSong.DoJoker(0)
            end;
          end;
        end;
      SDLK_2:
        begin //Jocker
            //Use Joker
          case CurMenu of
            SM_Party_Main:
            begin
              ScreenSong.DoJoker(1)
            end;
          end;
        end;
      SDLK_3:
        begin //Jocker
            //Use Joker
          case CurMenu of
            SM_Party_Main:
            begin
              ScreenSong.DoJoker(2)
            end;
          end;
        end;
    end; // case
  end; // if
end;

constructor TScreenSongMenu.Create;
begin
  inherited Create;
  
  //Create Dummy SelectSlide Entrys
  SetLength(ISelections, 1);
  ISelections[0] := 'Dummy';

  
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

  AddSelectSlide(Theme.SongMenu.SelectSlide3, SelectValue, ISelections);

  AddButton(Theme.SongMenu.Button4);
  if (Length(Button[3].Text) = 0) then
    AddButtonText(14, 20, 'Button 4');


  Interaction := 0;
end;

function TScreenSongMenu.Draw: boolean;
begin
  Result := inherited Draw;
end;

procedure TScreenSongMenu.onShow;
begin
  inherited;

end;

procedure TScreenSongMenu.MenuShow(sMenu: Byte);
begin
  Interaction := 0; //Reset Interaction
  Visible := True;  //Set Visible
  Case sMenu of
    SM_Main:
      begin
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_MAIN');

        Button[0].Visible := True;
        Button[1].Visible := True;
        Button[2].Visible := True;
        Button[3].Visible := True;
        SelectsS[0].Visible := False;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_PLAY');
        Button[1].Text[0].Text := Language.Translate('SONG_MENU_CHANGEPLAYERS');
        Button[2].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_ADD');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_EDIT');
      end;

    SM_PlayList:
      begin
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PLAYLIST');

        Button[0].Visible := True;
        Button[1].Visible := True;
        Button[2].Visible := True;
        Button[3].Visible := True;
        SelectsS[0].Visible := False;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_PLAY');
        Button[1].Text[0].Text := Language.Translate('SONG_MENU_CHANGEPLAYERS');
        Button[2].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_DEL');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_EDIT');
      end;

    SM_Playlist_Add:
      begin
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PLAYLIST_ADD');

        Button[0].Visible := True;
        Button[1].Visible := False;
        Button[2].Visible := False;
        Button[3].Visible := True;
        SelectsS[0].Visible := True;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_ADD_NEW');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_ADD_EXISTING');

        SetLength(ISelections, Length(PlaylistMan.Playlists));
        PlaylistMan.GetNames(ISelections);

        if (Length(ISelections)>=1) then
        begin
          UpdateSelectSlideOptions(Theme.SongMenu.SelectSlide3, 0, ISelections, SelectValue);
        end
        else
        begin
          Button[3].Visible := False;
          SelectsS[0].Visible := False;
          Button[2].Visible := True;
          Button[2].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_NOEXISTING');
        end;
      end;

    SM_Playlist_New:
      begin
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PLAYLIST_NEW');

        Button[0].Visible := True;
        Button[1].Visible := False;
        Button[2].Visible := True;
        Button[3].Visible := True;
        SelectsS[0].Visible := False;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_NEW_UNNAMED');
        Button[2].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_NEW_CREATE');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_CANCEL');
      end;

    SM_Playlist_DelItem:
      begin
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PLAYLIST_DELITEM');

        Button[0].Visible := True;
        Button[1].Visible := False;
        Button[2].Visible := False;
        Button[3].Visible := True;
        SelectsS[0].Visible := False;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_YES');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_CANCEL');
      end;

    SM_Playlist_Load:
      begin
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PLAYLIST_LOAD');

        //Show Delete Curent Playlist Button when Playlist is opened
        Button[0].Visible := (CatSongs.CatNumShow = -3);

        Button[1].Visible := False;
        Button[2].Visible := False;
        Button[3].Visible := True;
        SelectsS[0].Visible := True;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_DELCURRENT');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_LOAD');

        SetLength(ISelections, Length(PlaylistMan.Playlists));
        PlaylistMan.GetNames(ISelections);

        if (Length(ISelections)>=1) then
        begin
          UpdateSelectSlideOptions(Theme.SongMenu.SelectSlide3, 0, ISelections, SelectValue);
          Interaction := 3;
        end
        else
        begin
          Button[3].Visible := False;
          SelectsS[0].Visible := False;
          Button[2].Visible := True;
          Button[2].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_NOEXISTING');
          Interaction := 2;
        end;
      end;

    SM_Playlist_Del:
      begin
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PLAYLIST_DEL');

        Button[0].Visible := True;
        Button[1].Visible := False;
        Button[2].Visible := False;
        Button[3].Visible := True;
        SelectsS[0].Visible := False;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_YES');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_CANCEL');
      end;


    SM_Party_Main:
      begin
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PARTY_MAIN');

        Button[0].Visible := True;
        Button[1].Visible := False;
        Button[2].Visible := False;
        Button[3].Visible := True;
        SelectsS[0].Visible := False;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_PLAY');
        //Button[1].Text[0].Text := Language.Translate('SONG_MENU_JOKER');
        //Button[2].Text[0].Text := Language.Translate('SONG_MENU_PLAYMODI');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_JOKER');
      end;

    SM_Party_Joker:
      begin
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PARTY_JOKER');
        // to-do : Party
        {Button[0].Visible := (PartySession.Teams.NumTeams >= 1) AND (PartySession.Teams.Teaminfo[0].Joker > 0);
        Button[1].Visible := (PartySession.Teams.NumTeams >= 2) AND (PartySession.Teams.Teaminfo[1].Joker > 0);
        Button[2].Visible := (PartySession.Teams.NumTeams >= 3) AND (PartySession.Teams.Teaminfo[2].Joker > 0);}
        Button[3].Visible := True;
        SelectsS[0].Visible := False;

        {Button[0].Text[0].Text := String(PartySession.Teams.Teaminfo[0].Name);
        Button[1].Text[0].Text := String(PartySession.Teams.Teaminfo[1].Name);
        Button[2].Text[0].Text := String(PartySession.Teams.Teaminfo[2].Name);}
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_CANCEL');

        //Set right Interaction
        if (not Button[0].Visible) then
        begin
          if (not Button[1].Visible) then
          begin
            if (not Button[2].Visible) then
            begin
              Interaction := 4;
            end
            else Interaction := 2;
          end
          else Interaction := 1;
        end;
        
      end;
  end;
end;

procedure TScreenSongMenu.HandleReturn;
begin
  Case CurMenu of
    SM_Main:
      begin
        Case Interaction of
          0: //Button 1
            begin
              ScreenSong.StartSong;
              Visible := False;
            end;

          1: //Button 2
            begin
              //Select New Players then Sing:
              ScreenSong.SelectPlayers;
              Visible := False;
            end;

          2: //Button 3
            begin
              //Show add to Playlist Menu
              MenuShow(SM_Playlist_Add);
            end;

          3: //SelectSlide 3
            begin
              //Dummy
            end;

          4: //Button 4
            begin
              ScreenSong.OpenEditor;
              Visible := False;
            end;
        end;
      end;

    SM_PlayList:
      begin
        Visible := False;
        Case Interaction of
          0: //Button 1
            begin
              ScreenSong.StartSong;
              Visible := False;
            end;

          1: //Button 2
            begin
              //Select New Players then Sing:
              ScreenSong.SelectPlayers;
              Visible := False;
            end;

          2: //Button 3
            begin
              //Show add to Playlist Menu
              MenuShow(SM_Playlist_DelItem);
            end;

          3: //SelectSlide 3
            begin
              //Dummy
            end;

          4: //Button 4
            begin
              ScreenSong.OpenEditor;
              Visible := False;
            end;
        end;
      end;

    SM_Playlist_Add:
      begin
        Case Interaction of
          0: //Button 1
            begin
              MenuShow(SM_Playlist_New);
            end;

          3: //SelectSlide 3
            begin
              //Dummy
            end;

          4: //Button 4
            begin
              PlaylistMan.AddItem(ScreenSong.Interaction, SelectValue);
              Visible := False;
            end;
        end;
      end;

      SM_Playlist_New:
      begin
        Case Interaction of
          0: //Button 1
            begin
              //Nothing, Button for Entering Name
            end;

          2: //Button 3
            begin
              //Create Playlist and Add Song
              PlaylistMan.AddItem(
              ScreenSong.Interaction,
              PlaylistMan.AddPlaylist(Button[0].Text[0].Text));
              Visible := False;
            end;

          3: //SelectSlide 3
            begin
              //Cancel -> Go back to Add screen
              MenuShow(SM_Playlist_Add);
            end;

          4: //Button 4
            begin
              Visible := False;
            end;
        end;
      end;

    SM_Playlist_DelItem:
      begin
        Visible := False;
        Case Interaction of
          0: //Button 1
            begin
              //Delete
              PlayListMan.DelItem(PlayListMan.GetIndexbySongID(ScreenSong.Interaction));
              Visible := False;
            end;

          4: //Button 4
            begin
              MenuShow(SM_Playlist);
            end;
        end;
      end;

    SM_Playlist_Load:
      begin
        Case Interaction of
          0: //Button 1 (Delete Playlist)
            begin
              MenuShow(SM_Playlist_Del);
            end;
          4: //Button 4
            begin
              //Load Playlist
              PlaylistMan.SetPlayList(SelectValue);
              Visible := False;
            end;
        end;
      end;

    SM_Playlist_Del:
      begin
        Visible := False;
        Case Interaction of
          0: //Button 1
            begin
              //Delete
              PlayListMan.DelPlaylist(PlaylistMan.CurPlayList);
              Visible := False;
            end;

          4: //Button 4
            begin
              MenuShow(SM_Playlist_Load);
            end;
        end;
      end;

    SM_Party_Main:
      begin
        Case Interaction of
          0: //Button 1
            begin
              //Start Singing
              ScreenSong.StartSong;
              Visible := False;
            end;

          4: //Button 4
            begin
              //Joker
              MenuShow(SM_Party_Joker);
            end;
        end;
      end;

    SM_Party_Joker:
      begin
        Visible := False;
        Case Interaction of
          0: //Button 1
            begin
              //Joker Team 1
              ScreenSong.DoJoker(0);
            end;

          1: //Button 2
            begin
              //Joker Team 2
              ScreenSong.DoJoker(1);
            end;

          2: //Button 3
            begin
              //Joker Team 3
              ScreenSong.DoJoker(2);
            end;

          4: //Button 4
            begin
              //Cancel... (Fo back to old Menu)
              MenuShow(SM_Party_Main);
            end;
        end;
      end;
  end;
end;

end.
 
