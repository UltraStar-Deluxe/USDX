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
  UMenu, SDL, UDisplay, UMusic, UFiles, SysUtils, UThemes;

type
  TScreenPartyNewRound = class(TMenu)
    public
      //Texts:
      TextRound1: Cardinal;
      TextRound2: Cardinal;
      TextRound3: Cardinal;
      TextRound4: Cardinal;
      TextRound5: Cardinal;
      TextRound6: Cardinal;
      TextRound7: Cardinal;

      TextWinner1: Cardinal;
      TextWinner2: Cardinal;
      TextWinner3: Cardinal;
      TextWinner4: Cardinal;
      TextWinner5: Cardinal;
      TextWinner6: Cardinal;
      TextWinner7: Cardinal;

      TextNextRound: Cardinal;
      TextNextRoundNo: Cardinal;
      TextNextPlayer1: Cardinal;
      TextNextPlayer2: Cardinal;
      TextNextPlayer3: Cardinal;

      //Statics
      StaticRound1: Cardinal;
      StaticRound2: Cardinal;
      StaticRound3: Cardinal;
      StaticRound4: Cardinal;
      StaticRound5: Cardinal;
      StaticRound6: Cardinal;
      StaticRound7: Cardinal;

      //Scores
      TextScoreTeam1: Cardinal;
      TextScoreTeam2: Cardinal;
      TextScoreTeam3: Cardinal;
      TextNameTeam1: Cardinal;
      TextNameTeam2: Cardinal;
      TextNameTeam3: Cardinal;

      TextTeam1Players: Cardinal;
      TextTeam2Players: Cardinal;
      TextTeam3Players: Cardinal;

      StaticTeam1: Cardinal;
      StaticTeam2: Cardinal;
      StaticTeam3: Cardinal;
      StaticNextPlayer1: Cardinal;
      StaticNextPlayer2: Cardinal;
      StaticNextPlayer3: Cardinal;




      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

implementation

uses UGraphic,
     UMain,
     UIni,
     UTexture,
     UParty,
     UDLLManager,
     ULanguage,
     USong,
     ULog;

function TScreenPartyNewRound.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
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

procedure TScreenPartyNewRound.onShow;
var
  I: Integer;
  function GetTeamPlayers(const Num: Byte): String;
  var
    Players: Array of String;
    //J: Byte;
  begin            // to-do : Party
    if (Num-1 >= {PartySession.Teams.NumTeams}0) then
      exit;

    {//Create Players Array
    SetLength(Players, PartySession.Teams.TeamInfo[Num-1].NumPlayers);
    For J := 0 to PartySession.Teams.TeamInfo[Num-1].NumPlayers-1 do
      Players[J] := String(PartySession.Teams.TeamInfo[Num-1].PlayerInfo[J].Name);}

    //Implode and Return
    Result := Language.Implode(Players);
  end;
begin
  inherited;

  // to-do : Party
  //PartySession.StartRound;

  //Set Visibility of Round Infos
  // to-do : Party
  I := {Length(PartySession.Rounds)}0;
  if (I >= 1) then
  begin
    Static[StaticRound1].Visible := True;
    Text[TextRound1].Visible := True;
    Text[TextWinner1].Visible := True;

    //Texts:
    //Text[TextRound1].Text := Language.Translate(DllMan.Plugins[PartySession.Rounds[0].Plugin].Name);
    //Text[TextWinner1].Text := PartySession.GetWinnerString(0);
  end
  else
  begin
    Static[StaticRound1].Visible := False;
    Text[TextRound1].Visible := False;
    Text[TextWinner1].Visible := False;
  end;

  if (I >= 2) then
  begin
    Static[StaticRound2].Visible := True;
    Text[TextRound2].Visible := True;
    Text[TextWinner2].Visible := True;

    //Texts:
    //Text[TextRound2].Text := Language.Translate(DllMan.Plugins[PartySession.Rounds[1].Plugin].Name);
    //Text[TextWinner2].Text := PartySession.GetWinnerString(1);
  end
  else
  begin
    Static[StaticRound2].Visible := False;
    Text[TextRound2].Visible := False;
    Text[TextWinner2].Visible := False;
  end;

  if (I >= 3) then
  begin
    Static[StaticRound3].Visible := True;
    Text[TextRound3].Visible := True;
    Text[TextWinner3].Visible := True;

    //Texts:
    //Text[TextRound3].Text := Language.Translate(DllMan.Plugins[PartySession.Rounds[2].Plugin].Name);
    //Text[TextWinner3].Text := PartySession.GetWinnerString(2);
  end
  else
  begin
    Static[StaticRound3].Visible := False;
    Text[TextRound3].Visible := False;
    Text[TextWinner3].Visible := False;
  end;

  if (I >= 4) then
  begin
    Static[StaticRound4].Visible := True;
    Text[TextRound4].Visible := True;
    Text[TextWinner4].Visible := True;

    //Texts:
    //Text[TextRound4].Text := Language.Translate(DllMan.Plugins[PartySession.Rounds[3].Plugin].Name);
    //Text[TextWinner4].Text := PartySession.GetWinnerString(3);
  end
  else
  begin
    Static[StaticRound4].Visible := False;
    Text[TextRound4].Visible := False;
    Text[TextWinner4].Visible := False;
  end;

  if (I >= 5) then
  begin
    Static[StaticRound5].Visible := True;
    Text[TextRound5].Visible := True;
    Text[TextWinner5].Visible := True;

    //Texts:
    //Text[TextRound5].Text := Language.Translate(DllMan.Plugins[PartySession.Rounds[4].Plugin].Name);
    //Text[TextWinner5].Text := PartySession.GetWinnerString(4);
  end
  else
  begin
    Static[StaticRound5].Visible := False;
    Text[TextRound5].Visible := False;
    Text[TextWinner5].Visible := False;
  end;

  if (I >= 6) then
  begin
    Static[StaticRound6].Visible := True;
    Text[TextRound6].Visible := True;
    Text[TextWinner6].Visible := True;

    //Texts:
    //Text[TextRound6].Text := Language.Translate(DllMan.Plugins[PartySession.Rounds[5].Plugin].Name);
    //Text[TextWinner6].Text := PartySession.GetWinnerString(5);
  end
  else
  begin
    Static[StaticRound6].Visible := False;
    Text[TextRound6].Visible := False;
    Text[TextWinner6].Visible := False;
  end;

  if (I >= 7) then
  begin
    Static[StaticRound7].Visible := True;
    Text[TextRound7].Visible := True;
    Text[TextWinner7].Visible := True;

    //Texts:
    //Text[TextRound7].Text := Language.Translate(DllMan.Plugins[PartySession.Rounds[6].Plugin].Name);
    //Text[TextWinner7].Text := PartySession.GetWinnerString(6);
  end
  else
  begin
    Static[StaticRound7].Visible := False;
    Text[TextRound7].Visible := False;
    Text[TextWinner7].Visible := False;
  end;

  //Display Scores
  {if (PartySession.Teams.NumTeams >= 1) then
  begin
    Text[TextScoreTeam1].Text := InttoStr(PartySession.Teams.TeamInfo[0].Score);
    Text[TextNameTeam1].Text := String(PartySession.Teams.TeamInfo[0].Name);
    Text[TextTeam1Players].Text := GetTeamPlayers(1);

    Text[TextScoreTeam1].Visible := True;
    Text[TextNameTeam1].Visible := True;
    Text[TextTeam1Players].Visible := True;
    Static[StaticTeam1].Visible := True;
    Static[StaticNextPlayer1].Visible := True;
  end
  else
  begin
    Text[TextScoreTeam1].Visible := False;
    Text[TextNameTeam1].Visible := False;
    Text[TextTeam1Players].Visible := False;
    Static[StaticTeam1].Visible := False;
    Static[StaticNextPlayer1].Visible := False;
  end;

  if (PartySession.Teams.NumTeams >= 2) then
  begin
    Text[TextScoreTeam2].Text := InttoStr(PartySession.Teams.TeamInfo[1].Score);
    Text[TextNameTeam2].Text := String(PartySession.Teams.TeamInfo[1].Name);
    Text[TextTeam2Players].Text := GetTeamPlayers(2);

    Text[TextScoreTeam2].Visible := True;
    Text[TextNameTeam2].Visible := True;
    Text[TextTeam2Players].Visible := True;
    Static[StaticTeam2].Visible := True;
    Static[StaticNextPlayer2].Visible := True;
  end
  else
  begin
    Text[TextScoreTeam2].Visible := False;
    Text[TextNameTeam2].Visible := False;
    Text[TextTeam2Players].Visible := False;
    Static[StaticTeam2].Visible := False;
    Static[StaticNextPlayer2].Visible := False;
  end;

  if (PartySession.Teams.NumTeams >= 3) then
  begin
    Text[TextScoreTeam3].Text := InttoStr(PartySession.Teams.TeamInfo[2].Score);
    Text[TextNameTeam3].Text := String(PartySession.Teams.TeamInfo[2].Name);
    Text[TextTeam3Players].Text := GetTeamPlayers(3);

    Text[TextScoreTeam3].Visible := True;
    Text[TextNameTeam3].Visible := True;
    Text[TextTeam3Players].Visible := True;
    Static[StaticTeam3].Visible := True;
    Static[StaticNextPlayer3].Visible := True;
  end
  else
  begin
    Text[TextScoreTeam3].Visible := False;
    Text[TextNameTeam3].Visible := False;
    Text[TextTeam3Players].Visible := False;
    Static[StaticTeam3].Visible := False;
    Static[StaticNextPlayer3].Visible := False;
  end;

  //nextRound Texts
  Text[TextNextRound].Text := Language.Translate(DllMan.Selected.PluginDesc);
  Text[TextNextRoundNo].Text := InttoStr(PartySession.CurRound + 1);
  if (PartySession.Teams.NumTeams >= 1) then
  begin
    Text[TextNextPlayer1].Text := PartySession.Teams.Teaminfo[0].Playerinfo[PartySession.Teams.Teaminfo[0].CurPlayer].Name;
    Text[TextNextPlayer1].Visible := True;
  end
  else
    Text[TextNextPlayer1].Visible := False;
    
  if (PartySession.Teams.NumTeams >= 2) then
  begin
    Text[TextNextPlayer2].Text := PartySession.Teams.Teaminfo[1].Playerinfo[PartySession.Teams.Teaminfo[1].CurPlayer].Name;
    Text[TextNextPlayer2].Visible := True;
  end
  else
    Text[TextNextPlayer2].Visible := False;

  if (PartySession.Teams.NumTeams >= 3) then
  begin
    Text[TextNextPlayer3].Text := PartySession.Teams.Teaminfo[2].Playerinfo[PartySession.Teams.Teaminfo[2].CurPlayer].Name;
    Text[TextNextPlayer3].Visible := True;
  end
  else
    Text[TextNextPlayer3].Visible := False;  }
end;

procedure TScreenPartyNewRound.SetAnimationProgress(Progress: real);
begin
  {Button[0].Texture.ScaleW := Progress;
  Button[1].Texture.ScaleW := Progress;
  Button[2].Texture.ScaleW := Progress; }
end;

end.
