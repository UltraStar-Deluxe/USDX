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
  SDL,
  SysUtils,
  UMenu,
  UDisplay,
  UMusic,
  UThemes;

type
  TScreenPartyWin = class(TMenu)
    public
      TextScoreTeam1:    cardinal;
      TextScoreTeam2:    cardinal;
      TextScoreTeam3:    cardinal;
      TextNameTeam1:     cardinal;
      TextNameTeam2:     cardinal;
      TextNameTeam3:     cardinal;
      StaticTeam1:       cardinal;
      StaticTeam1BG:     cardinal;
      StaticTeam1Deco:   cardinal;
      StaticTeam2:       cardinal;
      StaticTeam2BG:     cardinal;
      StaticTeam2Deco:   cardinal;
      StaticTeam3:       cardinal;
      StaticTeam3BG:     cardinal;
      StaticTeam3Deco:   cardinal;
      TextWinner:        cardinal;

      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

implementation

uses
  UGraphic,
  UMain,
  UParty,
  UScreenSingModi,
  ULanguage,
  UUnicodeUtils;

function TScreenPartyWin.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
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
      SDLK_BACKSPACE,
      SDLK_RETURN :
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

procedure TScreenPartyWin.OnShow;
var
 I, J: integer;
 Ranking: AParty_TeamRanking;

  Function GetTeamColor(Team: integer): cardinal;
  var
    NameString: string;
  begin
    NameString := 'P' + InttoStr(Team+1) + 'Dark';

    Result := ColorExists(NameString);
  end;

begin
  inherited;

  // get team ranking
  // Ranking is sorted by score
  Ranking := Party.GetTeamRanking;

  //Set Winnertext
  Text[TextWinner].Text := Format(Language.Translate('PARTY_SCORE_WINS'), [Party.GetWinnerString(-1)]);
  if (Length(Party.Teams) >= 1) then
  begin
    Text[TextScoreTeam1].Text := IntToStr(Party.Teams[Ranking[0].Team].Score);
    Text[TextNameTeam1].Text := Party.Teams[Ranking[0].Team].Name;

    Text[TextScoreTeam1].Visible := true;
    Text[TextNameTeam1].Visible := true;
    Static[StaticTeam1].Visible := true;
    Static[StaticTeam1BG].Visible := true;
    Static[StaticTeam1Deco].Visible := true;

    //Set Static Color to Team Color
    if (Theme.PartyWin.StaticTeam1BG.Color = 'TeamColor') then
    begin
      I := GetTeamColor(Ranking[0].Team);
      if (I <> -1) then
      begin
        Static[StaticTeam1BG].Texture.ColR := Color[I].RGB.R;
        Static[StaticTeam1BG].Texture.ColG := Color[I].RGB.G;
        Static[StaticTeam1BG].Texture.ColB := Color[I].RGB.B;
      end;
    end;

    if (Theme.PartyWin.StaticTeam1.Color = 'TeamColor') then
    begin
      I := GetTeamColor(Ranking[0].Team);
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
    Text[TextScoreTeam1].Visible := false;
    Text[TextNameTeam1].Visible := false;
    Static[StaticTeam1].Visible := false;
    Static[StaticTeam1BG].Visible := false;
    Static[StaticTeam1Deco].Visible := false;
  end;

  if (Length(Party.Teams) >= 2) then
  begin
    Text[TextScoreTeam2].Text := IntToStr(Party.Teams[Ranking[1].Team].Score);
    Text[TextNameTeam2].Text := Party.Teams[Ranking[1].Team].Name;

    Text[TextScoreTeam2].Visible := true;
    Text[TextNameTeam2].Visible := true;
    Static[StaticTeam2].Visible := true;
    Static[StaticTeam2BG].Visible := true;
    Static[StaticTeam2Deco].Visible := true;

    //Set Static Color to Team Color
    if (Theme.PartyWin.StaticTeam2BG.Color = 'TeamColor') then
    begin
      I := GetTeamColor(Ranking[1].Team);
      if (I <> -1) then
      begin
        Static[StaticTeam2BG].Texture.ColR := Color[I].RGB.R;
        Static[StaticTeam2BG].Texture.ColG := Color[I].RGB.G;
        Static[StaticTeam2BG].Texture.ColB := Color[I].RGB.B;
      end;
    end;

    if (Theme.PartyWin.StaticTeam2.Color = 'TeamColor') then
    begin
      I := GetTeamColor(Ranking[1].Team);
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
    Text[TextScoreTeam2].Visible := false;
    Text[TextNameTeam2].Visible := false;
    Static[StaticTeam2].Visible := false;
    Static[StaticTeam2BG].Visible := false;
    Static[StaticTeam2Deco].Visible := false;
  end;

  if (Length(Party.Teams) >= 3) then
  begin
    Text[TextScoreTeam3].Text := IntToStr(Party.Teams[Ranking[2].Team].Score);
    Text[TextNameTeam3].Text := Party.Teams[Ranking[2].Team].Name;

    Text[TextScoreTeam3].Visible := true;
    Text[TextNameTeam3].Visible := true;
    Static[StaticTeam3].Visible := true;
    Static[StaticTeam3BG].Visible := true;
    Static[StaticTeam3Deco].Visible := true;

    //Set Static Color to Team Color
    if (Theme.PartyWin.StaticTeam3BG.Color = 'TeamColor') then
    begin
      I := GetTeamColor(Ranking[2].Team);
      if (I <> -1) then
      begin
        Static[StaticTeam3BG].Texture.ColR := Color[I].RGB.R;
        Static[StaticTeam3BG].Texture.ColG := Color[I].RGB.G;
        Static[StaticTeam3BG].Texture.ColB := Color[I].RGB.B;
      end;
    end;

    if (Theme.PartyWin.StaticTeam3.Color = 'TeamColor') then
    begin
      I := GetTeamColor(Ranking[2].Team);
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
    Text[TextScoreTeam3].Visible := false;
    Text[TextNameTeam3].Visible := false;
    Static[StaticTeam3].Visible := false;
    Static[StaticTeam3BG].Visible := false;
    Static[StaticTeam3Deco].Visible := false;
  end;
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
