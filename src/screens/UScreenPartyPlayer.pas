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

unit UScreenPartyPlayer;

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
  TScreenPartyPlayer = class(TMenu)
    public
      Team1Name: cardinal;
      Player1Name: cardinal;
      Player2Name: cardinal;
      Player3Name: cardinal;
      Player4Name: cardinal;

      Team2Name: cardinal;
      Player5Name: cardinal;
      Player6Name: cardinal;
      Player7Name: cardinal;
      Player8Name: cardinal;

      Team3Name: cardinal;
      Player9Name: cardinal;
      Player10Name: cardinal;
      Player11Name: cardinal;
      Player12Name: cardinal;

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
  UParty,
  UUnicodeUtils;

function TScreenPartyPlayer.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  SDL_ModState:  word;
  I, J: integer;

  procedure IntNext;
  begin
    repeat
      InteractNext;
    until Button[Interaction].Visible;
  end;
  procedure IntPrev;
  begin
    repeat
      InteractPrev;
    until Button[Interaction].Visible;
  end;
begin
  Result := true;

  if (PressedDown) then
    SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
        + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT)
  else
    SDL_ModState := 0;

  begin // Key Down
    // check normal keys
    case CharCode of
      Ord('0')..Ord('9'),
      Ord('a')..Ord('z'),
      Ord('A')..Ord('Z'),
      Ord(' '), Ord('-'), Ord('_'), Ord('!'), Ord(','), Ord('<'), Ord('/'),
      Ord('*'), Ord('?'), Ord(''''), Ord('"'):
        begin
          Button[Interaction].Text[0].Text := Button[Interaction].Text[0].Text +
                                              UCS4ToUTF8String(CharCode);
          Exit;
        end;
    end;

    // check special keys
    case PressedKey of
      // Templates for Names Mod
      SDLK_F1:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[0] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[0];
         end;
      SDLK_F2:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[1] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[1];
         end;
      SDLK_F3:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[2] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[2];
         end;
      SDLK_F4:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[3] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[3];
         end;
      SDLK_F5:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[4] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[4];
         end;
      SDLK_F6:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[5] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[5];
         end;
      SDLK_F7:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[6] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[6];
         end;
      SDLK_F8:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[7] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[7];
         end;
      SDLK_F9:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[8] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[8];
         end;
      SDLK_F10:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[9] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[9];
         end;
      SDLK_F11:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[10] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[10];
         end;
      SDLK_F12:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[11] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[11];
         end;

      SDLK_BACKSPACE:
        begin
          Button[Interaction].Text[0].DeleteLastLetter;
        end;

      SDLK_ESCAPE:
        begin
          Ini.SaveNames;
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenPartyOptions);
        end;

      SDLK_RETURN:
        begin

          //Save PlayerNames
          for I := 0 to PartySession.Teams.NumTeams-1 do
          begin
            PartySession.Teams.Teaminfo[I].Name := PChar(Button[I*5].Text[0].Text);
            for J := 0 to PartySession.Teams.Teaminfo[I].NumPlayers-1 do
            begin
              PartySession.Teams.Teaminfo[I].Playerinfo[J].Name := PChar(Button[I*5 + J+1].Text[0].Text);
              PartySession.Teams.Teaminfo[I].Playerinfo[J].TimesPlayed := 0;
            end;
          end;

          AudioPlayback.PlaySound(SoundLib.Start);
          FadeTo(@ScreenPartyNewRound);
        end;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:    IntNext;
      SDLK_UP:      IntPrev;
      SDLK_RIGHT:   IntNext;
      SDLK_LEFT:    IntPrev;
    end;
  end;
end;

constructor TScreenPartyPlayer.Create;
begin
  inherited Create;

  LoadFromTheme(Theme.PartyPlayer);

  Team1Name := AddButton(Theme.PartyPlayer.Team1Name);
  AddButton(Theme.PartyPlayer.Player1Name);
  AddButton(Theme.PartyPlayer.Player2Name);
  AddButton(Theme.PartyPlayer.Player3Name);
  AddButton(Theme.PartyPlayer.Player4Name);

  Team2Name := AddButton(Theme.PartyPlayer.Team2Name);
  AddButton(Theme.PartyPlayer.Player5Name);
  AddButton(Theme.PartyPlayer.Player6Name);
  AddButton(Theme.PartyPlayer.Player7Name);
  AddButton(Theme.PartyPlayer.Player8Name);

  Team3Name := AddButton(Theme.PartyPlayer.Team3Name);
  AddButton(Theme.PartyPlayer.Player9Name);
  AddButton(Theme.PartyPlayer.Player10Name);
  AddButton(Theme.PartyPlayer.Player11Name);
  AddButton(Theme.PartyPlayer.Player12Name);

  Interaction := 0;
end;

procedure TScreenPartyPlayer.OnShow;
var
  I:    integer;
begin
  inherited;

  // Templates for Names Mod
  for I := 1 to 4 do
    Button[I].Text[0].Text := Ini.Name[I-1];

  for I := 6 to 9 do
    Button[I].Text[0].Text := Ini.Name[I-2];

  for I := 11 to 14 do
    Button[I].Text[0].Text := Ini.Name[I-3];

    Button[0].Text[0].Text := Ini.NameTeam[0];
    Button[5].Text[0].Text := Ini.NameTeam[1];
    Button[10].Text[0].Text := Ini.NameTeam[2];
    // Templates for Names Mod end
  
  if (PartySession.Teams.NumTeams>=1) then
  begin
    Button[0].Visible := true;
    Button[1].Visible := (PartySession.Teams.Teaminfo[0].NumPlayers >=1);
    Button[2].Visible := (PartySession.Teams.Teaminfo[0].NumPlayers >=2);
    Button[3].Visible := (PartySession.Teams.Teaminfo[0].NumPlayers >=3);
    Button[4].Visible := (PartySession.Teams.Teaminfo[0].NumPlayers >=4);
  end
  else
  begin
    Button[0].Visible := false;
    Button[1].Visible := false;
    Button[2].Visible := false;
    Button[3].Visible := false;
    Button[4].Visible := false;
  end;

  if (PartySession.Teams.NumTeams>=2) then
  begin
    Button[5].Visible := true;
    Button[6].Visible := (PartySession.Teams.Teaminfo[1].NumPlayers >=1);
    Button[7].Visible := (PartySession.Teams.Teaminfo[1].NumPlayers >=2);
    Button[8].Visible := (PartySession.Teams.Teaminfo[1].NumPlayers >=3);
    Button[9].Visible := (PartySession.Teams.Teaminfo[1].NumPlayers >=4);
  end
  else
  begin
    Button[5].Visible := false;
    Button[6].Visible := false;
    Button[7].Visible := false;
    Button[8].Visible := false;
    Button[9].Visible := false;
  end;

  if (PartySession.Teams.NumTeams>=3) then
  begin
    Button[10].Visible := true;
    Button[11].Visible := (PartySession.Teams.Teaminfo[2].NumPlayers >=1);
    Button[12].Visible := (PartySession.Teams.Teaminfo[2].NumPlayers >=2);
    Button[13].Visible := (PartySession.Teams.Teaminfo[2].NumPlayers >=3);
    Button[14].Visible := (PartySession.Teams.Teaminfo[2].NumPlayers >=4);
  end
  else
  begin
    Button[10].Visible := false;
    Button[11].Visible := false;
    Button[12].Visible := false;
    Button[13].Visible := false;
    Button[14].Visible := false;
  end;

end;

procedure TScreenPartyPlayer.SetAnimationProgress(Progress: real);
var
  I:    integer;
begin
  for I := 0 to high(Button) do
    Button[I].Texture.ScaleW := Progress;
end;

end.
