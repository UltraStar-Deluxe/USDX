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

unit UScreenPartyWin;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu, SDL, UDisplay, UMusic, SysUtils, UThemes;

type
  TScreenPartyWin = class(TMenu)
    public
      TextScoreTeam1:    Cardinal;
      TextScoreTeam2:    Cardinal;
      TextScoreTeam3:    Cardinal;
      TextNameTeam1:     Cardinal;
      TextNameTeam2:     Cardinal;
      TextNameTeam3:     Cardinal;
      StaticTeam1:       Cardinal;
      StaticTeam1BG:     Cardinal;
      StaticTeam1Deco:   Cardinal;
      StaticTeam2:       Cardinal;
      StaticTeam2BG:     Cardinal;
      StaticTeam2Deco:   Cardinal;
      StaticTeam3:       Cardinal;
      StaticTeam3BG:     Cardinal;
      StaticTeam3Deco:   Cardinal;
      TextWinner:        Cardinal;

      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

implementation

uses UGraphic, UMain, UParty, UScreenSingModi, ULanguage;

function TScreenPartyWin.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
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
          AudioPlayback.PlaySound(SoundLib.Start);
          FadeTo(@ScreenMain);
        end;

      SDLK_RETURN:
        begin
          AudioPlayback.PlaySound(SoundLib.Start);
          FadeTo(@ScreenMain);
        end;
    end;
  end;
end;

constructor TScreenPartyWin.Create;
//var
// I:    integer; // Auto Removed, Unused Variable
begin
  inherited Create;

  TextScoreTeam1 := AddText (Theme.PartyWin.TextScoreTeam1);
  TextScoreTeam2 := AddText (Theme.PartyWin.TextScoreTeam2);
  TextScoreTeam3 := AddText (Theme.PartyWin.TextScoreTeam3);
  TextNameTeam1 := AddText (Theme.PartyWin.TextNameTeam1);
  TextNameTeam2 := AddText (Theme.PartyWin.TextNameTeam2);
  TextNameTeam3 := AddText (Theme.PartyWin.TextNameTeam3);

  StaticTeam1 := AddStatic (Theme.PartyWin.StaticTeam1);
  StaticTeam1BG := AddStatic (Theme.PartyWin.StaticTeam1BG);
  StaticTeam1Deco := AddStatic (Theme.PartyWin.StaticTeam1Deco);
  StaticTeam2 := AddStatic (Theme.PartyWin.StaticTeam2);
  StaticTeam2BG := AddStatic (Theme.PartyWin.StaticTeam2BG);
  StaticTeam2Deco := AddStatic (Theme.PartyWin.StaticTeam2Deco);
  StaticTeam3 := AddStatic (Theme.PartyWin.StaticTeam3);
  StaticTeam3BG := AddStatic (Theme.PartyWin.StaticTeam3BG);
  StaticTeam3Deco := AddStatic (Theme.PartyWin.StaticTeam3Deco);

  TextWinner := AddText (Theme.PartyWin.TextWinner);

  LoadFromTheme(Theme.PartyWin);
end;

procedure TScreenPartyWin.onShow;
//var
// I: Integer; // Auto Removed, Unused Variable
// Placing: Integer; // Auto Removed, Unused Variable

  Function GetTeamColor(Team: Byte): Cardinal;
  var
    NameString: String;
  begin
    NameString := 'P' + InttoStr(Team+1) + 'Dark';

    Result := ColorExists(NameString);
  end;

begin
  inherited;

  // to-do : Party
  //Get Team Placing
  //Placing := PartySession.GetTeamOrder;

  //Set Winnertext
  //Text[TextWinner].Text := Format(Language.Translate('PARTY_SCORE_WINS'), [PartySession.Teams.Teaminfo[Placing[0]].Name]);
  {if (PartySession.Teams.NumTeams >= 1) then
  begin
    Text[TextScoreTeam1].Text := InttoStr(PartySession.Teams.TeamInfo[Placing[0]].Score);
    Text[TextNameTeam1].Text := String(PartySession.Teams.TeamInfo[Placing[0]].Name);

    Text[TextScoreTeam1].Visible := True;
    Text[TextNameTeam1].Visible := True;
    Static[StaticTeam1].Visible := True;
    Static[StaticTeam1BG].Visible := True;
    Static[StaticTeam1Deco].Visible := True;

    //Set Static Color to Team Color
    If (Theme.PartyWin.StaticTeam1BG.Color = 'TeamColor') then
    begin
      I := GetTeamColor(Placing[0]);
      if (I <> -1) then
      begin
        Static[StaticTeam1BG].Texture.ColR := Color[I].RGB.R;
        Static[StaticTeam1BG].Texture.ColG := Color[I].RGB.G;
        Static[StaticTeam1BG].Texture.ColB := Color[I].RGB.B;
      end;
    end;

    If (Theme.PartyWin.StaticTeam1.Color = 'TeamColor') then
    begin
      I := GetTeamColor(Placing[0]);
      if (I <> -1) then
      begin
        Static[StaticTeam1].Texture.ColR := Color[I].RGB.R;
        Static[StaticTeam1].Texture.ColG := Color[I].RGB.G;
        Static[StaticTeam1].Texture.ColB := Color[I].RGB.B;
      end;
    end;
  end
  else
  begin
    Text[TextScoreTeam1].Visible := False;
    Text[TextNameTeam1].Visible := False;
    Static[StaticTeam1].Visible := False;
    Static[StaticTeam1BG].Visible := False;
    Static[StaticTeam1Deco].Visible := False;
  end;

  if (PartySession.Teams.NumTeams >= 2) then
  begin
    Text[TextScoreTeam2].Text := InttoStr(PartySession.Teams.TeamInfo[Placing[1]].Score);
    Text[TextNameTeam2].Text := String(PartySession.Teams.TeamInfo[Placing[1]].Name);

    Text[TextScoreTeam2].Visible := True;
    Text[TextNameTeam2].Visible := True;
    Static[StaticTeam2].Visible := True;
    Static[StaticTeam2BG].Visible := True;
    Static[StaticTeam2Deco].Visible := True;

    //Set Static Color to Team Color
    If (Theme.PartyWin.StaticTeam2BG.Color = 'TeamColor') then
    begin
      I := GetTeamColor(Placing[1]);
      if (I <> -1) then
      begin
        Static[StaticTeam2BG].Texture.ColR := Color[I].RGB.R;
        Static[StaticTeam2BG].Texture.ColG := Color[I].RGB.G;
        Static[StaticTeam2BG].Texture.ColB := Color[I].RGB.B;
      end;
    end;

    If (Theme.PartyWin.StaticTeam2.Color = 'TeamColor') then
    begin
      I := GetTeamColor(Placing[1]);
      if (I <> -1) then
      begin
        Static[StaticTeam2].Texture.ColR := Color[I].RGB.R;
        Static[StaticTeam2].Texture.ColG := Color[I].RGB.G;
        Static[StaticTeam2].Texture.ColB := Color[I].RGB.B;
      end;
    end;
  end
  else
  begin
    Text[TextScoreTeam2].Visible := False;
    Text[TextNameTeam2].Visible := False;
    Static[StaticTeam2].Visible := False;
    Static[StaticTeam2BG].Visible := False;
    Static[StaticTeam2Deco].Visible := False;
  end;

  if (PartySession.Teams.NumTeams >= 3) then
  begin
    Text[TextScoreTeam3].Text := InttoStr(PartySession.Teams.TeamInfo[Placing[2]].Score);
    Text[TextNameTeam3].Text := String(PartySession.Teams.TeamInfo[Placing[2]].Name);

    Text[TextScoreTeam3].Visible := True;
    Text[TextNameTeam3].Visible := True;
    Static[StaticTeam3].Visible := True;
    Static[StaticTeam3BG].Visible := True;
    Static[StaticTeam3Deco].Visible := True;

    //Set Static Color to Team Color
    If (Theme.PartyWin.StaticTeam3BG.Color = 'TeamColor') then
    begin
      I := GetTeamColor(Placing[2]);
      if (I <> -1) then
      begin
        Static[StaticTeam3BG].Texture.ColR := Color[I].RGB.R;
        Static[StaticTeam3BG].Texture.ColG := Color[I].RGB.G;
        Static[StaticTeam3BG].Texture.ColB := Color[I].RGB.B;
      end;
    end;

    If (Theme.PartyWin.StaticTeam3.Color = 'TeamColor') then
    begin
      I := GetTeamColor(Placing[2]);
      if (I <> -1) then
      begin
        Static[StaticTeam3].Texture.ColR := Color[I].RGB.R;
        Static[StaticTeam3].Texture.ColG := Color[I].RGB.G;
        Static[StaticTeam3].Texture.ColB := Color[I].RGB.B;
      end;
    end;
  end
  else
  begin
    Text[TextScoreTeam3].Visible := False;
    Text[TextNameTeam3].Visible := False;
    Static[StaticTeam3].Visible := False;
    Static[StaticTeam3BG].Visible := False;
    Static[StaticTeam3Deco].Visible := False;
  end;   }
end;

procedure TScreenPartyWin.SetAnimationProgress(Progress: real);
begin
  {if (ScreenSingModi.PlayerInfo.NumPlayers >= 1) then
    Static[StaticTeam1].Texture.ScaleW := Progress * ScreenSingModi.PlayerInfo.Playerinfo[0].Score / maxScore;
  if (ScreenSingModi.PlayerInfo.NumPlayers >= 2) then
    Static[StaticTeam2].Texture.ScaleW := Progress * ScreenSingModi.PlayerInfo.Playerinfo[1].Score / maxScore;
  if (ScreenSingModi.PlayerInfo.NumPlayers >= 3) then
    Static[StaticTeam3].Texture.ScaleW := Progress * ScreenSingModi.PlayerInfo.Playerinfo[2].Score / maxScore;}
end;

end.
