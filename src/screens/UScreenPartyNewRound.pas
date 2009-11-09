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

unit UScreenPartyNewRound;

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
  UFiles,
  UThemes;

type
  TScreenPartyNewRound = class(TMenu)
    public
      //Texts:
      TextRound1: cardinal;
      TextRound2: cardinal;
      TextRound3: cardinal;
      TextRound4: cardinal;
      TextRound5: cardinal;
      TextRound6: cardinal;
      TextRound7: cardinal;

      TextWinner1: cardinal;
      TextWinner2: cardinal;
      TextWinner3: cardinal;
      TextWinner4: cardinal;
      TextWinner5: cardinal;
      TextWinner6: cardinal;
      TextWinner7: cardinal;

      TextNextRound: cardinal;
      TextNextRoundNo: cardinal;
      TextNextPlayer1: cardinal;
      TextNextPlayer2: cardinal;
      TextNextPlayer3: cardinal;

      //Statics
      StaticRound1: cardinal;
      StaticRound2: cardinal;
      StaticRound3: cardinal;
      StaticRound4: cardinal;
      StaticRound5: cardinal;
      StaticRound6: cardinal;
      StaticRound7: cardinal;

      //Scores
      TextScoreTeam1: cardinal;
      TextScoreTeam2: cardinal;
      TextScoreTeam3: cardinal;
      TextNameTeam1: cardinal;
      TextNameTeam2: cardinal;
      TextNameTeam3: cardinal;

      TextTeam1Players: cardinal;
      TextTeam2Players: cardinal;
      TextTeam3Players: cardinal;

      StaticTeam1: cardinal;
      StaticTeam2: cardinal;
      StaticTeam3: cardinal;
      StaticNextPlayer1: cardinal;
      StaticNextPlayer2: cardinal;
      StaticNextPlayer3: cardinal;



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
  UDLLManager,
  ULanguage,
  USong,
  ULog,
  UUnicodeUtils;

function TScreenPartyNewRound.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
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
          CheckFadeTo(@ScreenMain,'MSG_END_PARTY');
        end;

      SDLK_RETURN:
        begin
          AudioPlayback.PlaySound(SoundLib.Start);
          if DLLMan.Selected.LoadSong then
          begin
            //Select PartyMode ScreenSong
            ScreenSong.Mode := smPartyMode;
            FadeTo(@ScreenSong);
          end
          else
          begin
            FadeTo(@ScreenSingModi);
          end;
        end;
    end;
  end;
end;

constructor TScreenPartyNewRound.Create;
begin
  inherited Create;

  TextRound1 := AddText (Theme.PartyNewRound.TextRound1);
  TextRound2 := AddText (Theme.PartyNewRound.TextRound2);
  TextRound3 := AddText (Theme.PartyNewRound.TextRound3);
  TextRound4 := AddText (Theme.PartyNewRound.TextRound4);
  TextRound5 := AddText (Theme.PartyNewRound.TextRound5);
  TextRound6 := AddText (Theme.PartyNewRound.TextRound6);
  TextRound7 := AddText (Theme.PartyNewRound.TextRound7);

  TextWinner1 := AddText (Theme.PartyNewRound.TextWinner1);
  TextWinner2 := AddText (Theme.PartyNewRound.TextWinner2);
  TextWinner3 := AddText (Theme.PartyNewRound.TextWinner3);
  TextWinner4 := AddText (Theme.PartyNewRound.TextWinner4);
  TextWinner5 := AddText (Theme.PartyNewRound.TextWinner5);
  TextWinner6 := AddText (Theme.PartyNewRound.TextWinner6);
  TextWinner7 := AddText (Theme.PartyNewRound.TextWinner7);

  TextNextRound := AddText (Theme.PartyNewRound.TextNextRound);
  TextNextRoundNo := AddText (Theme.PartyNewRound.TextNextRoundNo);
  TextNextPlayer1 := AddText (Theme.PartyNewRound.TextNextPlayer1);
  TextNextPlayer2 := AddText (Theme.PartyNewRound.TextNextPlayer2);
  TextNextPlayer3 := AddText (Theme.PartyNewRound.TextNextPlayer3);

  StaticRound1 := AddStatic (Theme.PartyNewRound.StaticRound1);
  StaticRound2 := AddStatic (Theme.PartyNewRound.StaticRound2);
  StaticRound3 := AddStatic (Theme.PartyNewRound.StaticRound3);
  StaticRound4 := AddStatic (Theme.PartyNewRound.StaticRound4);
  StaticRound5 := AddStatic (Theme.PartyNewRound.StaticRound5);
  StaticRound6 := AddStatic (Theme.PartyNewRound.StaticRound6);
  StaticRound7 := AddStatic (Theme.PartyNewRound.StaticRound7);

  //Scores
  TextScoreTeam1 := AddText (Theme.PartyNewRound.TextScoreTeam1);
  TextScoreTeam2 := AddText (Theme.PartyNewRound.TextScoreTeam2);
  TextScoreTeam3 := AddText (Theme.PartyNewRound.TextScoreTeam3);
  TextNameTeam1 := AddText (Theme.PartyNewRound.TextNameTeam1);
  TextNameTeam2 := AddText (Theme.PartyNewRound.TextNameTeam2);
  TextNameTeam3 := AddText (Theme.PartyNewRound.TextNameTeam3);

  //Players
  TextTeam1Players := AddText (Theme.PartyNewRound.TextTeam1Players);
  TextTeam2Players := AddText (Theme.PartyNewRound.TextTeam2Players);
  TextTeam3Players := AddText (Theme.PartyNewRound.TextTeam3Players);

  StaticTeam1 := AddStatic (Theme.PartyNewRound.StaticTeam1);
  StaticTeam2 := AddStatic (Theme.PartyNewRound.StaticTeam2);
  StaticTeam3 := AddStatic (Theme.PartyNewRound.StaticTeam3);
  StaticNextPlayer1 := AddStatic (Theme.PartyNewRound.StaticNextPlayer1);
  StaticNextPlayer2 := AddStatic (Theme.PartyNewRound.StaticNextPlayer2);
  StaticNextPlayer3 := AddStatic (Theme.PartyNewRound.StaticNextPlayer3);

  LoadFromTheme(Theme.PartyNewRound);
end;

procedure TScreenPartyNewRound.OnShow;
var
  I: integer;
  function GetTeamPlayers(const Num: byte): UTF8String;
  var
    Players: array of UTF8String;
    J: byte;
  begin
    if (Num-1 >= PartySession.Teams.NumTeams) then
      exit;

    //Create Players array
    SetLength(Players, PartySession.Teams.TeamInfo[Num-1].NumPlayers);
    for J := 0 to PartySession.Teams.TeamInfo[Num-1].NumPlayers-1 do
      Players[J] := UTF8String(PartySession.Teams.TeamInfo[Num-1].PlayerInfo[J].Name);

    //Implode and Return
    Result := Language.Implode(Players);
  end;
begin
  inherited;

  PartySession.StartRound;

  //Set Visibility of Round Infos
  I := Length(PartySession.Rounds);
  if (I >= 1) then
  begin
    Static[StaticRound1].Visible := true;
    Text[TextRound1].Visible := true;
    Text[TextWinner1].Visible := true;

    //Texts:
    Text[TextRound1].Text := Language.Translate(DllMan.Plugins[PartySession.Rounds[0].Plugin].Name);
    Text[TextWinner1].Text := PartySession.GetWinnerString(0);
  end
  else
  begin
    Static[StaticRound1].Visible := false;
    Text[TextRound1].Visible := false;
    Text[TextWinner1].Visible := false;
  end;

  if (I >= 2) then
  begin
    Static[StaticRound2].Visible := true;
    Text[TextRound2].Visible := true;
    Text[TextWinner2].Visible := true;

    //Texts:
    Text[TextRound2].Text := Language.Translate(DllMan.Plugins[PartySession.Rounds[1].Plugin].Name);
    Text[TextWinner2].Text := PartySession.GetWinnerString(1);
  end
  else
  begin
    Static[StaticRound2].Visible := false;
    Text[TextRound2].Visible := false;
    Text[TextWinner2].Visible := false;
  end;

  if (I >= 3) then
  begin
    Static[StaticRound3].Visible := true;
    Text[TextRound3].Visible := true;
    Text[TextWinner3].Visible := true;

    //Texts:
    Text[TextRound3].Text := Language.Translate(DllMan.Plugins[PartySession.Rounds[2].Plugin].Name);
    Text[TextWinner3].Text := PartySession.GetWinnerString(2);
  end
  else
  begin
    Static[StaticRound3].Visible := false;
    Text[TextRound3].Visible := false;
    Text[TextWinner3].Visible := false;
  end;

  if (I >= 4) then
  begin
    Static[StaticRound4].Visible := true;
    Text[TextRound4].Visible := true;
    Text[TextWinner4].Visible := true;

    //Texts:
    Text[TextRound4].Text := Language.Translate(DllMan.Plugins[PartySession.Rounds[3].Plugin].Name);
    Text[TextWinner4].Text := PartySession.GetWinnerString(3);
  end
  else
  begin
    Static[StaticRound4].Visible := false;
    Text[TextRound4].Visible := false;
    Text[TextWinner4].Visible := false;
  end;

  if (I >= 5) then
  begin
    Static[StaticRound5].Visible := true;
    Text[TextRound5].Visible := true;
    Text[TextWinner5].Visible := true;

    //Texts:
    Text[TextRound5].Text := Language.Translate(DllMan.Plugins[PartySession.Rounds[4].Plugin].Name);
    Text[TextWinner5].Text := PartySession.GetWinnerString(4);
  end
  else
  begin
    Static[StaticRound5].Visible := false;
    Text[TextRound5].Visible := false;
    Text[TextWinner5].Visible := false;
  end;

  if (I >= 6) then
  begin
    Static[StaticRound6].Visible := true;
    Text[TextRound6].Visible := true;
    Text[TextWinner6].Visible := true;

    //Texts:
    Text[TextRound6].Text := Language.Translate(DllMan.Plugins[PartySession.Rounds[5].Plugin].Name);
    Text[TextWinner6].Text := PartySession.GetWinnerString(5);
  end
  else
  begin
    Static[StaticRound6].Visible := false;
    Text[TextRound6].Visible := false;
    Text[TextWinner6].Visible := false;
  end;

  if (I >= 7) then
  begin
    Static[StaticRound7].Visible := true;
    Text[TextRound7].Visible := true;
    Text[TextWinner7].Visible := true;

    //Texts:
    Text[TextRound7].Text := Language.Translate(DllMan.Plugins[PartySession.Rounds[6].Plugin].Name);
    Text[TextWinner7].Text := PartySession.GetWinnerString(6);
  end
  else
  begin
    Static[StaticRound7].Visible := false;
    Text[TextRound7].Visible := false;
    Text[TextWinner7].Visible := false;
  end;

  //Display Scores
  if (PartySession.Teams.NumTeams >= 1) then
  begin
    Text[TextScoreTeam1].Text := InttoStr(PartySession.Teams.TeamInfo[0].Score);
    Text[TextNameTeam1].Text := UTF8String(PartySession.Teams.TeamInfo[0].Name);
    Text[TextTeam1Players].Text := GetTeamPlayers(1);

    Text[TextScoreTeam1].Visible := true;
    Text[TextNameTeam1].Visible := true;
    Text[TextTeam1Players].Visible := true;
    Static[StaticTeam1].Visible := true;
    Static[StaticNextPlayer1].Visible := true;
  end
  else
  begin
    Text[TextScoreTeam1].Visible := false;
    Text[TextNameTeam1].Visible := false;
    Text[TextTeam1Players].Visible := false;
    Static[StaticTeam1].Visible := false;
    Static[StaticNextPlayer1].Visible := false;
  end;

  if (PartySession.Teams.NumTeams >= 2) then
  begin
    Text[TextScoreTeam2].Text := InttoStr(PartySession.Teams.TeamInfo[1].Score);
    Text[TextNameTeam2].Text := UTF8String(PartySession.Teams.TeamInfo[1].Name);
    Text[TextTeam2Players].Text := GetTeamPlayers(2);

    Text[TextScoreTeam2].Visible := true;
    Text[TextNameTeam2].Visible := true;
    Text[TextTeam2Players].Visible := true;
    Static[StaticTeam2].Visible := true;
    Static[StaticNextPlayer2].Visible := true;
  end
  else
  begin
    Text[TextScoreTeam2].Visible := false;
    Text[TextNameTeam2].Visible := false;
    Text[TextTeam2Players].Visible := false;
    Static[StaticTeam2].Visible := false;
    Static[StaticNextPlayer2].Visible := false;
  end;

  if (PartySession.Teams.NumTeams >= 3) then
  begin
    Text[TextScoreTeam3].Text := InttoStr(PartySession.Teams.TeamInfo[2].Score);
    Text[TextNameTeam3].Text := UTF8String(PartySession.Teams.TeamInfo[2].Name);
    Text[TextTeam3Players].Text := GetTeamPlayers(3);

    Text[TextScoreTeam3].Visible := true;
    Text[TextNameTeam3].Visible := true;
    Text[TextTeam3Players].Visible := true;
    Static[StaticTeam3].Visible := true;
    Static[StaticNextPlayer3].Visible := true;
  end
  else
  begin
    Text[TextScoreTeam3].Visible := false;
    Text[TextNameTeam3].Visible := false;
    Text[TextTeam3Players].Visible := false;
    Static[StaticTeam3].Visible := false;
    Static[StaticNextPlayer3].Visible := false;
  end;

  //nextRound Texts
  Text[TextNextRound].Text := Language.Translate(DllMan.Selected.PluginDesc);
  Text[TextNextRoundNo].Text := InttoStr(PartySession.CurRound + 1);
  if (PartySession.Teams.NumTeams >= 1) then
  begin
    Text[TextNextPlayer1].Text := PartySession.Teams.Teaminfo[0].Playerinfo[PartySession.Teams.Teaminfo[0].CurPlayer].Name;
    Text[TextNextPlayer1].Visible := true;
  end
  else
    Text[TextNextPlayer1].Visible := false;
    
  if (PartySession.Teams.NumTeams >= 2) then
  begin
    Text[TextNextPlayer2].Text := PartySession.Teams.Teaminfo[1].Playerinfo[PartySession.Teams.Teaminfo[1].CurPlayer].Name;
    Text[TextNextPlayer2].Visible := true;
  end
  else
    Text[TextNextPlayer2].Visible := false;

  if (PartySession.Teams.NumTeams >= 3) then
  begin
    Text[TextNextPlayer3].Text := PartySession.Teams.Teaminfo[2].Playerinfo[PartySession.Teams.Teaminfo[2].CurPlayer].Name;
    Text[TextNextPlayer3].Visible := true;
  end
  else
    Text[TextNextPlayer3].Visible := false;
end;

procedure TScreenPartyNewRound.SetAnimationProgress(Progress: real);
begin
  {Button[0].Texture.ScaleW := Progress;
  Button[1].Texture.ScaleW := Progress;
  Button[2].Texture.ScaleW := Progress; }
end;

end.
