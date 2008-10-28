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
  UMenu, SDL, UDisplay, UMusic, UFiles, SysUtils, UThemes;

type
  TScreenPartyOptions = class(TMenu)
    public
      SelectLevel: Cardinal;
      SelectPlayList: Cardinal;
      SelectPlayList2: Cardinal;
      SelectRounds: Cardinal;
      SelectTeams: Cardinal;
      SelectPlayers1: Cardinal;
      SelectPlayers2: Cardinal;
      SelectPlayers3: Cardinal;

      PlayList:  Integer;
      PlayList2: Integer;
      Rounds:    Integer;
      NumTeams:  Integer;
      NumPlayer1, NumPlayer2, NumPlayer3: Integer;
      
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure SetAnimationProgress(Progress: real); override;
      procedure SetPlaylist2;
  end;

var
  IPlaylist: array[0..2] of String;
  IPlaylist2: array of String;
const
  ITeams: array[0..1] of String =('2', '3');
  IPlayers: array[0..3] of String =('1', '2', '3', '4');
  IRounds: array[0..5] of String = ('2', '3', '4', '5', '6', '7');

implementation

uses UGraphic, UMain, UIni, UTexture, ULanguage, UParty, USong, UDLLManager, UPlaylist, USongs;

function TScreenPartyOptions.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
  var
    I, J: Integer;
    OnlyMultiPlayer: boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
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
          FadeTo(@ScreenMain);
        end;

      SDLK_RETURN:
        begin
          //Don'T start when Playlist is Selected and there are no Playlists
          If (Playlist = 2) and (Length(PlaylistMan.Playlists) = 0) then
            Exit;
          // Don't start when SinglePlayer Teams but only Multiplayer Plugins available
          OnlyMultiPlayer:=true;
          for I := 0 to High(DLLMan.Plugins) do begin
            OnlyMultiPlayer := (OnlyMultiPlayer AND DLLMan.Plugins[I].TeamModeOnly);
          end;
          if (OnlyMultiPlayer) AND ((NumPlayer1 = 0) OR (NumPlayer2 = 0) OR ((NumPlayer3 = 0) AND (NumTeams = 1))) then begin
            ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_PLUGINS'));
            Exit;
          end;
          //Save Difficulty
          Ini.Difficulty := SelectsS[SelectLevel].SelectedOption;
          Ini.SaveLevel;


          //Save Num Teams:
          {PartySession.Teams.NumTeams := NumTeams + 2;
          PartySession.Teams.Teaminfo[0].NumPlayers := NumPlayer1+1;
          PartySession.Teams.Teaminfo[1].NumPlayers := NumPlayer2+1;
          PartySession.Teams.Teaminfo[2].NumPlayers := NumPlayer3+1;}

          //Save Playlist
          PlaylistMan.Mode := TSingMode( Playlist );
          PlaylistMan.CurPlayList := High(Cardinal);
          //If Category Selected Search Category ID
          if Playlist = 1 then
          begin
            J := -1;
            For I := 0 to high(CatSongs.Song) do
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
            If PlaylistMan.CurPlayList = High(Cardinal) then
              Exit;
          end
          else
            PlaylistMan.CurPlayList := Playlist2;

          //Start Party
          // to-do : Party
          //PartySession.StartNewParty(Rounds + 2);

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
          If (Interaction = 1) then
          begin
            SetPlaylist2;
          end //Change Team3 Players visibility
          Else If (Interaction = 4) then
          begin
              SelectsS[7].Visible := (NumTeams = 1);
          end;
        end;
      SDLK_LEFT:
        begin
          AudioPlayback.PlaySound(SoundLib.Option);
          InteractDec;

          //Change Playlist2 if Playlist is Changed
          If (Interaction = 1) then
          begin
            SetPlaylist2;
          end //Change Team3 Players visibility
          Else If (Interaction = 4) then
          begin
            SelectsS[7].Visible := (NumTeams = 1);
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
  NumTeams := 0;
  NumPlayer1 := 0;
  NumPlayer2 := 0;
  NumPlayer3 := 0;
  Rounds := 5;
  PlayList := 0;
  PlayList2 := 0;

  //Load Screen From Theme
  LoadFromTheme(Theme.PartyOptions);

  SelectLevel := AddSelectSlide (Theme.PartyOptions.SelectLevel, Ini.Difficulty, Theme.ILevel);
  SelectPlayList := AddSelectSlide (Theme.PartyOptions.SelectPlayList, PlayList, IPlaylist);
  SelectPlayList2 := AddSelectSlide (Theme.PartyOptions.SelectPlayList2, PlayList2, IPlaylist2);
  SelectRounds := AddSelectSlide (Theme.PartyOptions.SelectRounds, Rounds, IRounds);
  SelectTeams := AddSelectSlide (Theme.PartyOptions.SelectTeams, NumTeams, ITeams);
  SelectPlayers1 := AddSelectSlide (Theme.PartyOptions.SelectPlayers1, NumPlayer1, IPlayers);
  SelectPlayers2 := AddSelectSlide (Theme.PartyOptions.SelectPlayers2, NumPlayer2, IPlayers);
  SelectPlayers3 := AddSelectSlide (Theme.PartyOptions.SelectPlayers3, NumPlayer3, IPlayers);

  Interaction := 0;

  //Hide Team3 Players
  SelectsS[7].Visible := False;
end;

procedure TScreenPartyOptions.SetPlaylist2;
var I: Integer;
begin
  Case Playlist of
    0:
      begin
        SetLength(IPlaylist2, 1);
        IPlaylist2[0] := '---';
      end;
    1:
      begin
        SetLength(IPlaylist2, 0);
        For I := 0 to high(CatSongs.Song) do
        begin
          If (CatSongs.Song[I].Main) then
          begin
            SetLength(IPlaylist2, Length(IPlaylist2) + 1);
            IPlaylist2[high(IPlaylist2)] := CatSongs.Song[I].Artist;
          end;
        end;

        If (Length(IPlaylist2) = 0) then
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

procedure TScreenPartyOptions.onShow;
begin
  inherited;

  Randomize;
end;

procedure TScreenPartyOptions.SetAnimationProgress(Progress: real);
begin
  {for I := 0 to 6 do
    SelectS[I].Texture.ScaleW := Progress;}
end;

end.
