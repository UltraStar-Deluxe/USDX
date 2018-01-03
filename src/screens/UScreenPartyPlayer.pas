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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/screens/UScreenPartyPlayer.pas $
 * $Id: UScreenPartyPlayer.pas 2201 2010-03-15 21:14:51Z brunzelchen $
 *}

unit UScreenPartyPlayer;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UCommon,
  UMenu,
  ULog,
  sdl2,
  UDisplay,
  UMusic,
  UNote,
  UFiles,
  SysUtils,
  UScreenSingController,
  UScreenPartyNewRound,
  UScreenPartyWin,
  UScreenPartyScore,
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
      function ShouldHandleInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; out SuppressKey: boolean): boolean; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      
      procedure SetAnimationProgress(Progress: real); override;
      function NoRepeatColors(ColorP:integer; Interaction:integer; Pos:integer):integer;
      procedure TeamColorButton(K: integer; Interact: integer);
      procedure ShuffleNames(var PlayerNames: array of UTF8String);

  end;

const
  ITeams:   array[0..1] of UTF8String = ('2', '3');
  IPlayers: array[0..3] of UTF8String = ('1', '2', '3', '4');

implementation

uses
  UAvatars,
  UGraphic,
  UMain,
  UIni,
  UTexture,
  UParty,
  UUnicodeUtils,
  UScreenPartyOptions,
  ULanguage;

var
  Num: array[0..2] of integer;

procedure TScreenPartyPlayer.UpdateInterface;
  var
    I: integer;
    Btn: integer;
begin
  SelectsS[SelectPlayers[2]].Visible := (CountTeams = 1);

  Btn := 0;
  for I := 0 to 2 do
  begin
    TeamColorButton(Num[I], I);

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
    Col: TRGB;
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
    Ini.SingColor[I] := Num[I];
    Ini.TeamColor[I] := Num[I];
    
    Party.AddTeam(Button[I * 5].Text[0].Text);

    for J := 0 to CountPlayer[I] do
      Party.AddPlayer(I, Button[I * 5 + 1 + J].Text[0].Text);

    // no avatar on Party
    AvatarPlayerTextures[I + 1] := NoAvatarTexture[I + 1];

    Col := GetPlayerColor(Num[I]);

    AvatarPlayerTextures[I + 1].ColR := Col.R;
    AvatarPlayerTextures[I + 1].ColG := Col.G;
    AvatarPlayerTextures[I + 1].ColB := Col.B;
  end;


  // MOD Colors
  Ini.SaveTeamColors;
  LoadTeamsColors;
  Theme.ThemePartyLoad;
  ScreenSong.ColorizeJokers;

  // Reload ScreenSing and ScreenScore because of player colors
  // TODO: do this better
  freeandnil(ScreenSing);
  freeandnil(ScreenPartyNewRound);
  freeandnil(ScreenPartyWin);
  freeandnil(ScreenPartyScore);

  Party.bPartyGame := true;
  PlayersPlay := Length(Party.Teams);

  ScreenSing := TScreenSingController.Create;
  ScreenPartyNewRound := TScreenPartyNewRound.Create;
  ScreenPartyWin := TScreenPartyWin.Create;
  ScreenPartyScore := TScreenPartyScore.Create;

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

function TScreenPartyPlayer.ShouldHandleInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; out SuppressKey: boolean): boolean;
begin
  Result := inherited;
  // only suppress special keys for now
  case PressedKey of
    // Templates for Names Mod
    SDLK_F1, SDLK_F2, SDLK_F3, SDLK_F4, SDLK_F5, SDLK_F6, SDLK_F7, SDLK_F8, SDLK_F9, SDLK_F10, SDLK_F11, SDLK_F12:
     if (Button[Interactions[Interaction].Num].Selected) then
     begin
       SuppressKey := true;
     end
     else
     begin
       Result := false;
     end;
  end;
end;

function TScreenPartyPlayer.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
  var
    SDL_ModState:  word;
    Team: integer;
    I, J, count: integer;
    randomNames: array of UTF8String; // new for randomizing names
  procedure IntNext;
  begin
    repeat
      InteractNext;
    until ((Interactions[Interaction].Typ = iSelectS) and
      SelectsS[Interactions[Interaction].Num].Visible) or
      (Button[Interactions[Interaction].Num].Visible);
  end;
  procedure IntPrev;
  begin
    repeat
      InteractPrev;
    until ((Interactions[Interaction].Typ = iSelectS) and
      SelectsS[Interactions[Interaction].Num].Visible) or
      (Button[Interactions[Interaction].Num].Visible);
  end;
  procedure HandleNameTemplate(const index: integer);
  var
    isAlternate: boolean;
  begin
    isAlternate := (SDL_ModState = KMOD_LSHIFT) or (SDL_ModState = KMOD_RSHIFT);
    isAlternate := isAlternate or (SDL_ModState = KMOD_LALT); // legacy key combination

    if isAlternate then Ini.NameTemplate[index] := Button[Interactions[Interaction].Num].Text[0].Text
    else Button[Interactions[Interaction].Num].Text[0].Text := Ini.NameTemplate[index];
  end;
begin
  Result := true;

  if (PressedDown) then
    SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
        + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT)
  else
    SDL_ModState := 0;

  // Key Down
  // check normal keys
  if (Interactions[Interaction].Typ = iButton) then
  begin

    // check normal keys
    if (IsPrintableChar(CharCode)) then
    begin
      Button[Interactions[Interaction].Num].Text[0].Text := Button[Interactions[Interaction].Num].Text[0].Text +
                                          UCS4ToUTF8String(CharCode);
      Exit;
    end;

    // check special keys
    case PressedKey of
      // Templates for Names Mod
      SDLK_F1: HandleNameTemplate(0);
      SDLK_F2: HandleNameTemplate(1);
      SDLK_F3: HandleNameTemplate(2);
      SDLK_F4: HandleNameTemplate(3);
      SDLK_F5: HandleNameTemplate(4);
      SDLK_F6: HandleNameTemplate(5);
      SDLK_F7: HandleNameTemplate(6);
      SDLK_F8: HandleNameTemplate(7);
      SDLK_F9: HandleNameTemplate(8);
      SDLK_F10: HandleNameTemplate(9);
      SDLK_F11: HandleNameTemplate(10);
      SDLK_F12: HandleNameTemplate(11);

      SDLK_BACKSPACE:
        begin
          Button[Interactions[Interaction].Num].Text[0].DeleteLastLetter;
        end;
    end;
  end
  else
  begin
    // check normal keys
    case UCS4UpperCase(CharCode) of
      Ord('Q'):
        begin
          Result := false;
          Exit;
        end;
    end;
  end;

  case PressedKey of
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
    SDLK_DOWN:
      begin
        if (Interaction in [1, 7, 13]) and (SDL_ModState = KMOD_LCTRL) then
        begin

          case Interaction of
            1: Team := 0;
            7: Team := 1;
            13: Team := 2;
          end;

          Num[Team] := Num[Team] - 1;
          Num[Team] := NoRepeatColors(Num[Team], Team, -1);
          TeamColorButton(Num[Team], Team);
        end
        else
          IntNext;
      end;
    SDLK_UP:
      begin
        if (Interaction in [1, 7, 13]) and (SDL_ModState = KMOD_LCTRL) then
        begin

          case Interaction of
            1: Team := 0;
            7: Team := 1;
            13: Team := 2;
          end;

          //Button[Team * 5].Text[0].Text := 'BUTTON ' + IntTostr(Team);

          Num[Team] := Num[Team] + 1;
          Num[Team] := NoRepeatColors(Num[Team], Team, 1);
          TeamColorButton(Num[Team], Team);
        end
        else
          IntPrev;
      end;
    SDLK_RIGHT:
      begin
        if (Interaction in [0,2,8,14]) then
        begin
          AudioPlayback.PlaySound(SoundLib.Option);
          InteractInc;
          UpdateInterface;

          if (Interaction = 0) then
          begin
            Num[2] := NoRepeatColors(Num[2], 2, 1);
            TeamColorButton(Num[2], 2);
          end;

        //CORRECT
        for I := 0 to 3 do
          IntNext;

        for I := 0 to 3 do
          IntPrev;

        end;
      end;
    SDLK_LEFT:
      begin
        if (Interaction in [0,2,8,14]) then
        begin
          AudioPlayback.PlaySound(SoundLib.Option);
          InteractDec;
          UpdateInterface;
        end;

        //CORRECT
        for I := 0 to 3 do
          IntNext;

        for I := 0 to 3 do
          IntPrev;

      end;
    SDLK_R:
      begin
        if (SDL_ModState = KMOD_LCTRL) then
        begin
          AudioPlayback.PlaySound(SoundLib.Option);
          //InteractDec;
          //UpdateInterface;

          count := 0;
          SetLength(randomNames, 12);

          // collect all player names
          for I := 0 to self.CountTeams+1 do
            for J := 0 to CountPlayer[I] do
            begin
              randomNames[count] := Button[5*I+J+1].Text[0].Text;
              inc(count);
            end;

          // shuffle player names
          SetLength(randomNames, count);
          ShuffleNames(randomNames);

          // set shuffled player names
          dec(count);
          for I := 0 to self.CountTeams+1 do
            for J := 0 to CountPlayer[I] do
            begin
              Button[5*I+J+1].Text[0].Text := randomNames[count];
              dec(count);
            end;
        end;
      end;
  end;
end;

constructor TScreenPartyPlayer.Create;
var
  ButtonID: integer;
begin
  inherited Create;

  LoadFromTheme(Theme.PartyPlayer);

  Theme.PartyPlayer.SelectTeams.oneItemOnly := true;
  Theme.PartyPlayer.SelectTeams.showArrows := true;
  SelectTeams     := AddSelectSlide(Theme.PartyPlayer.SelectTeams, CountTeams, ITeams);

  Team1Name := AddButton(Theme.PartyPlayer.Team1Name);
  Button[Team1Name].Text[0].Writable := true;

  Theme.PartyPlayer.SelectPlayers1.oneItemOnly := true;
  Theme.PartyPlayer.SelectPlayers1.showArrows := true;
  SelectPlayers[0]  := AddSelectSlide(Theme.PartyPlayer.SelectPlayers1, CountPlayer[0], IPlayers);

  ButtonID := AddButton(Theme.PartyPlayer.Player1Name);
  Button[ButtonID].Text[0].Writable := true;

  ButtonID := AddButton(Theme.PartyPlayer.Player2Name);
  Button[ButtonID].Text[0].Writable := true;

  ButtonID := AddButton(Theme.PartyPlayer.Player3Name);
  Button[ButtonID].Text[0].Writable := true;

  ButtonID := AddButton(Theme.PartyPlayer.Player4Name);
  Button[ButtonID].Text[0].Writable := true;

  Team2Name := AddButton(Theme.PartyPlayer.Team2Name);
  Button[Team2Name].Text[0].Writable := true;

  Theme.PartyPlayer.SelectPlayers2.oneItemOnly := true;
  Theme.PartyPlayer.SelectPlayers2.showArrows := true;
  SelectPlayers[1]  := AddSelectSlide(Theme.PartyPlayer.SelectPlayers2, CountPlayer[1], IPlayers);

  ButtonID := AddButton(Theme.PartyPlayer.Player5Name);
  Button[ButtonID].Text[0].Writable := true;

  ButtonID := AddButton(Theme.PartyPlayer.Player6Name);
  Button[ButtonID].Text[0].Writable := true;

  ButtonID := AddButton(Theme.PartyPlayer.Player7Name);
  Button[ButtonID].Text[0].Writable := true;

  ButtonID := AddButton(Theme.PartyPlayer.Player8Name);
  Button[ButtonID].Text[0].Writable := true;

  Team3Name := AddButton(Theme.PartyPlayer.Team3Name);
  Button[Team3Name].Text[0].Writable := true;

  Theme.PartyPlayer.SelectPlayers3.oneItemOnly := true;
  Theme.PartyPlayer.SelectPlayers3.showArrows := true;
  SelectPlayers[2]  := AddSelectSlide(Theme.PartyPlayer.SelectPlayers3, CountPlayer[2], IPlayers);

  ButtonID := AddButton(Theme.PartyPlayer.Player9Name);
  Button[ButtonID].Text[0].Writable := true;

  ButtonID := AddButton(Theme.PartyPlayer.Player10Name);
  Button[ButtonID].Text[0].Writable := true;

  ButtonID := AddButton(Theme.PartyPlayer.Player11Name);
  Button[ButtonID].Text[0].Writable := true;

  ButtonID := AddButton(Theme.PartyPlayer.Player12Name);
  Button[ButtonID].Text[0].Writable := true;

  Button[Team2Name].Text[0].Selected  := true;

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

  for I := 0 to 2 do
    Num[I] := NoRepeatColors(Ini.TeamColor[I], I, 1);

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

function TScreenPartyPlayer.NoRepeatColors(ColorP:integer; Interaction:integer; Pos:integer):integer;
var
  Z:integer;
begin

  if (ColorP >= 10) then
    ColorP := NoRepeatColors(1, Interaction, Pos);

  if (ColorP <= 0) then
    ColorP := NoRepeatColors(9, Interaction, Pos);

  for Z := 0 to CountTeams + 1 do
  begin
    if Z <> Interaction then
    begin
     if (Num[Z] = ColorP) then
       ColorP := NoRepeatColors(ColorP + Pos, Interaction, Pos)
    end;
  end;

  Result := ColorP;

end;

procedure TScreenPartyPlayer.TeamColorButton(K: integer; Interact: integer);
var
  Col, DesCol: TRGB;
  I: integer;
begin

  Col := GetPlayerLightColor(K);
  DesCol := GetPlayerColor(K);

  for I := 0 to 4 do
  begin
    Button[Interact * 5 + I].SelectColR:= Col.R;
    Button[Interact * 5 + I].SelectColG:= Col.G;
    Button[Interact * 5 + I].SelectColB:= Col.B;

    Button[Interact * 5 + I].DeselectColR:= DesCol.R;
    Button[Interact * 5 + I].DeselectColG:= DesCol.G;
    Button[Interact * 5 + I].DeselectColB:= DesCol.B;
  end;

  SelectsS[Interact + 1].ColR := Col.R;
  SelectsS[Interact + 1].ColG := Col.G;
  SelectsS[Interact + 1].ColB := Col.B;

  SelectsS[Interact + 1].DColR := DesCol.R;
  SelectsS[Interact + 1].DColG := DesCol.G;
  SelectsS[Interact + 1].DColB := DesCol.B;

  SelectsS[Interact + 1].SBGColR := Col.R;
  SelectsS[Interact + 1].SBGColG := Col.G;
  SelectsS[Interact + 1].SBGColB := Col.B;

  SelectsS[Interact + 1].SBGDColR := DesCol.R;
  SelectsS[Interact + 1].SBGDColG := DesCol.G;
  SelectsS[Interact + 1].SBGDColB := DesCol.B;

  //Interaction := Interact;

  //CORRECT
  for I := 0 to 15 do
    InteractNext;

  for I := 0 to 15 do
    InteractPrev;

end;

procedure TScreenPartyPlayer.SetAnimationProgress(Progress: real);
var
  I:    integer;
begin
  {for I := 0 to high(Button) do
    Button[I].Texture.ScaleW := Progress;   }
end;

// random permutation of player names according to Fisher-Yates
procedure TScreenPartyPlayer.ShuffleNames(var PlayerNames: array of UTF8String);
var
  I, J: Integer;
  tmp: UTF8String;
begin
  for I := Low(PlayerNames) to High(PlayerNames) do begin
    J := I + Random(Length(PlayerNames) - I + Low(PlayerNames));
    tmp := PlayerNames[J];
    PlayerNames[J] := PlayerNames[I];
    PlayerNames[I] := tmp;
  end;
end;

end.
