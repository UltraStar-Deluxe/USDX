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
    private
      CountTeams: integer;
      CountPlayer: array [0..2] of integer;

      SelectTeams:     cardinal;
      SelectPlayers: array [0..2] of cardinal;
      procedure UpdateInterface;
      procedure UpdateParty;
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

const
  ITeams:   array[0..1] of UTF8String = ('2', '3');
  IPlayers: array[0..3] of UTF8String = ('1', '2', '3', '4');

implementation

uses
  UGraphic,
  UMain,
  UIni,
  UTexture,
  UParty,
  UUnicodeUtils,
  UScreenPartyOptions,
  ULanguage;

procedure TScreenPartyPlayer.UpdateInterface;
  var
    I: integer;
    Btn: integer;
begin
  SelectsS[SelectPlayers[2]].Visible := (CountTeams = 1);

  Btn := 0;
  for I := 0 to 2 do
  begin
    if (CountTeams + 1 >= I) then
    begin
      Button[Btn + 0].Visible := true;
      Button[Btn + 1].Visible := (CountPlayer[I] + 1 >= 1);
      Button[Btn + 2].Visible := (CountPlayer[I] + 1 >= 2);
      Button[Btn + 3].Visible := (CountPlayer[I] + 1 >= 3);
      Button[Btn + 4].Visible := (CountPlayer[I] + 1 >= 4);
    end
    else
    begin
      Button[Btn + 0].Visible := false;
      Button[Btn + 1].Visible := false;
      Button[Btn + 2].Visible := false;
      Button[Btn + 3].Visible := false;
      Button[Btn + 4].Visible := false;
    end;
    Inc(Btn, 5);
  end;
end;

procedure TScreenPartyPlayer.UpdateParty;
  var
    I, J: integer;
    Rounds: ARounds;
begin
  {//Save PlayerNames
  for I := 0 to PartySession.Teams.NumTeams-1 do
  begin
    PartySession.Teams.Teaminfo[I].Name := PChar(Button[I*5].Text[0].Text);
    for J := 0 to PartySession.Teams.Teaminfo[I].NumPlayers-1 do
    begin
      PartySession.Teams.Teaminfo[I].Playerinfo[J].Name := PChar(Button[I*5 + J+1].Text[0].Text);
      PartySession.Teams.Teaminfo[I].Playerinfo[J].TimesPlayed := 0;
    end;
  end; }

  // add teams to party

  for I := 0 to CountTeams + 1 do
  begin
    Party.AddTeam(Button[I * 5].Text[0].Text);

    for J := 0 to CountPlayer[I] do
      Party.AddPlayer(I, Button[I * 5 + 1 + J].Text[0].Text);
  end;

  if (Party.ModesAvailable) then
  begin //mode for current playersetup available
    FadeTo(@ScreenPartyRounds, SoundLib.Start);
  end
  else
  begin
    // no mode available for current player setup
    ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_MODES_FOR_CURRENT_SETUP'));
    Party.Clear;
  end;
end;

function TScreenPartyPlayer.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  SDL_ModState:  word;
  procedure IntNext;
  begin
    repeat
      InteractNext;
    until (Interactions[Interaction].Typ <> iButton) or (Button[Interactions[Interaction].Num].Visible);
  end;
  procedure IntPrev;
  begin
    repeat
      InteractPrev;
    until (Interactions[Interaction].Typ <> iButton) or (Button[Interactions[Interaction].Num].Visible);
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

      SDLK_RETURN: UpdateParty;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:    IntNext;
      SDLK_UP:      IntPrev;
      SDLK_RIGHT: begin
        if (Interaction in [0,2,8,14]) then
        begin
          AudioPlayback.PlaySound(SoundLib.Option);
          InteractInc;

          UpdateInterface;
        end;
      end;
      SDLK_LEFT: begin
        if (Interaction in [0,2,8,14]) then
        begin
          AudioPlayback.PlaySound(SoundLib.Option);
          InteractDec;

          UpdateInterface;
        end;
      end;
    end;
  end;
end;

constructor TScreenPartyPlayer.Create;
begin
  inherited Create;

  LoadFromTheme(Theme.PartyPlayer);

  Theme.PartyPlayer.SelectTeams.oneItemOnly := true;
  Theme.PartyPlayer.SelectTeams.showArrows := true;
  SelectTeams     := AddSelectSlide(Theme.PartyPlayer.SelectTeams, CountTeams, ITeams);

  Team1Name := AddButton(Theme.PartyPlayer.Team1Name);
  Theme.PartyPlayer.SelectPlayers1.oneItemOnly := true;
  Theme.PartyPlayer.SelectPlayers1.showArrows := true;
  SelectPlayers[0]  := AddSelectSlide(Theme.PartyPlayer.SelectPlayers1, CountPlayer[0], IPlayers);

  AddButton(Theme.PartyPlayer.Player1Name);
  AddButton(Theme.PartyPlayer.Player2Name);
  AddButton(Theme.PartyPlayer.Player3Name);
  AddButton(Theme.PartyPlayer.Player4Name);

  Team2Name := AddButton(Theme.PartyPlayer.Team2Name);
  Theme.PartyPlayer.SelectPlayers2.oneItemOnly := true;
  Theme.PartyPlayer.SelectPlayers2.showArrows := true;
  SelectPlayers[1]  := AddSelectSlide(Theme.PartyPlayer.SelectPlayers2, CountPlayer[1], IPlayers);

  AddButton(Theme.PartyPlayer.Player5Name);
  AddButton(Theme.PartyPlayer.Player6Name);
  AddButton(Theme.PartyPlayer.Player7Name);
  AddButton(Theme.PartyPlayer.Player8Name);

  Team3Name := AddButton(Theme.PartyPlayer.Team3Name);
  Theme.PartyPlayer.SelectPlayers3.oneItemOnly := true;
  Theme.PartyPlayer.SelectPlayers3.showArrows := true;
  SelectPlayers[2]  := AddSelectSlide(Theme.PartyPlayer.SelectPlayers3, CountPlayer[2], IPlayers);
  
  AddButton(Theme.PartyPlayer.Player9Name);
  AddButton(Theme.PartyPlayer.Player10Name);
  AddButton(Theme.PartyPlayer.Player11Name);
  AddButton(Theme.PartyPlayer.Player12Name);

  Interaction := 0;

  //Clear Selects
  CountTeams := 0;
  CountPlayer[0] := 0;
  CountPlayer[1] := 0;
  CountPlayer[2] := 0;
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

  Party.Clear;

  UpdateInterface;
end;

procedure TScreenPartyPlayer.SetAnimationProgress(Progress: real);
var
  I:    integer;
begin
  {for I := 0 to high(Button) do
    Button[I].Texture.ScaleW := Progress;   }
end;

end.
