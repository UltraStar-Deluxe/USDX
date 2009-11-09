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

unit UParty;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I switches.inc}

uses
  ModiSDK;

type
  TRoundInfo = record
    Plugin: word;
    Winner: byte;
  end;

  TeamOrderEntry = record
    TeamNum: byte;
    Score:   byte;
  end;

  TeamOrderArray = array[0..5] of byte;

  TPartyPlugin = record
    ID:          byte;
    TimesPlayed: byte;
  end;

  TPartySession = class
  private
    function GetRandomPlayer(Team: byte): byte;
    function GetRandomPlugin(Plugins: array of TPartyPlugin): byte;
    function IsWinner(Player, Winner: byte): boolean;
    procedure GenScores;
  public
    Teams:    TTeamInfo;
    Rounds:   array of TRoundInfo;
    CurRound: byte;

    constructor Create;

    procedure StartNewParty(NumRounds: byte);
    procedure StartRound;
    procedure EndRound;
    function  GetTeamOrder: TeamOrderArray;
    function  GetWinnerString(Round: byte): UTF8String;
  end;

var
  PartySession: TPartySession;

implementation

uses
  UDLLManager,
  UGraphic,
  UNote,
  ULanguage,
  ULog;

constructor TPartySession.Create;
begin
  inherited;
end;

//----------
// Returns a number of a random plugin
//----------
function TPartySession.GetRandomPlugin(Plugins: array of TPartyPlugin): byte;
var
  LowestTP:    byte;
  NumPwithLTP: word;
  I:           integer;
  R:           word;
begin
  LowestTP := high(byte);
  NumPwithLTP := 0;

  //Search for Plugins not often played yet
  for I := 0 to high(Plugins) do
  begin
    if (Plugins[I].TimesPlayed < lowestTP) then
    begin
      lowestTP := Plugins[I].TimesPlayed;
      NumPwithLTP := 1;
    end
    else if (Plugins[I].TimesPlayed = lowestTP) then
    begin
      Inc(NumPwithLTP);
    end;
  end;

  //Create random no
  R := Random(NumPwithLTP);

  //Search for random plugin
  for I := 0 to high(Plugins) do
  begin
    if Plugins[I].TimesPlayed = LowestTP then
    begin
      //Plugin found
      if (R = 0) then
      begin
        Result := Plugins[I].ID;
        Inc(Plugins[I].TimesPlayed);
        Break;
      end;
      Dec(R);
    end;
  end;
end;

//----------
//StartNewParty - Reset and prepares for new party
//----------
procedure TPartySession.StartNewParty(NumRounds: byte);
var
  Plugins:  array of TPartyPlugin;
  TeamMode: boolean;
  Len:      integer;
  I, J:     integer;
begin
  //Set current round to 1
  CurRound := 255;

  PlayersPlay := Teams.NumTeams;

  //Get team-mode and set joker, also set TimesPlayed
  TeamMode := true;
  for I := 0 to Teams.NumTeams - 1 do
  begin
    if Teams.Teaminfo[I].NumPlayers < 2 then
    begin
      TeamMode := false;
    end;
    //Set player attributes
    for J := 0 to Teams.TeamInfo[I].NumPlayers-1 do
    begin
      Teams.TeamInfo[I].Playerinfo[J].TimesPlayed := 0;
    end;
    Teams.Teaminfo[I].Joker := Round(NumRounds * 0.7);
    Teams.Teaminfo[I].Score := 0;
  end;

  //Fill plugin array
  SetLength(Plugins, 0);
  for I := 0 to high(DLLMan.Plugins) do
  begin
    if TeamMode or (not DLLMan.Plugins[I].TeamModeOnly) then
    begin 
      //Add only those plugins playable with current PlayerConfiguration
      Len := Length(Plugins);
      SetLength(Plugins, Len + 1);
      Plugins[Len].ID := I;
      Plugins[Len].TimesPlayed := 0;
    end;
  end;

  //Set rounds
  if (Length(Plugins) >= 1) then
  begin
    SetLength (Rounds, NumRounds);
    for I := 0 to NumRounds - 1 do
    begin
      PartySession.Rounds[I].Plugin := GetRandomPlugin(Plugins);
      PartySession.Rounds[I].Winner := 255;
    end;
  end
  else
    SetLength (Rounds, 0);
end;

{**
 * Returns a random player to play next round
 *}
function TPartySession.GetRandomPlayer(Team: byte): byte;
var
  I, R:        integer;
  LowestTP:    byte;
  NumPwithLTP: byte;
begin
  LowestTP    := high(byte);
  NumPwithLTP := 0;
  Result      := 0;

  //Search for players that have not often played yet
  for I := 0 to Teams.Teaminfo[Team].NumPlayers - 1 do
  begin
    if (Teams.Teaminfo[Team].Playerinfo[I].TimesPlayed < lowestTP) then
    begin
      lowestTP := Teams.Teaminfo[Team].Playerinfo[I].TimesPlayed;
      NumPwithLTP := 1;
    end
    else if (Teams.Teaminfo[Team].Playerinfo[I].TimesPlayed = lowestTP) then
    begin
      Inc(NumPwithLTP);
    end;
  end;

  //Create random number
  R := Random(NumPwithLTP);

  //Search for random player
  for I := 0 to Teams.Teaminfo[Team].NumPlayers - 1 do
  begin
    if Teams.Teaminfo[Team].Playerinfo[I].TimesPlayed = lowestTP then
    begin
      //Player found
      if (R = 0) then
      begin
        Result := I;
        Break;
      end;
      
      Dec(R);
    end;
  end;
end;

{**
 * Prepares ScreenSingModi for next round and loads plugin
 *}
procedure TPartySession.StartRound;
var
  I: integer;
begin
  if ((CurRound < high(Rounds)) or (CurRound = high(CurRound))) then
  begin
    // Increase Current Round but not beyond its limit
    // CurRound is set to 255 to begin with!
    // Ugly solution if you ask me.
    if CurRound < high(CurRound) then
      Inc(CurRound)
    else
      CurRound := 0;

    Rounds[CurRound].Winner := 255;
    DllMan.LoadPlugin(Rounds[CurRound].Plugin);

    //Select Players
    for I := 0 to Teams.NumTeams - 1 do
      Teams.Teaminfo[I].CurPlayer := GetRandomPlayer(I);

    //Set ScreenSingModie Variables
    ScreenSingModi.TeamInfo := Teams;
  end;
end;

//----------
//EndRound - Get Winner from ScreenSingModi and Save Data to RoundArray
//----------
procedure TPartySession.EndRound;
var
  I: Integer;
begin
  //Copy Winner
  Rounds[CurRound].Winner := ScreenSingModi.Winner;
  //Set Scores
  GenScores;

  //Increase TimesPlayed 4 all Players
  For I := 0 to Teams.NumTeams-1 do
    Inc(Teams.Teaminfo[I].Playerinfo[Teams.Teaminfo[I].CurPlayer].TimesPlayed);

end;

//----------
//IsWinner - returns true if the player's bit is set in the winner byte
//----------
function TPartySession.IsWinner(Player, Winner: byte): boolean;
var
  Mask: byte;
begin
  Mask := 1 shl Player;
  Result := (Winner and Mask) <> 0;
end;

//----------
//GenScores - increase scores for current round
//----------
procedure TPartySession.GenScores;
var
  I: byte;
begin
  for I := 0 to Teams.NumTeams - 1 do
  begin
    if isWinner(I, Rounds[CurRound].Winner) then
      Inc(Teams.Teaminfo[I].Score);
  end;
end;

//----------
//GetTeamOrder - returns the placement of each Team [First Position of Array is Teamnum of first placed Team, ...]
//----------
function TPartySession.GetTeamOrder: TeamOrderArray;
var
  I, J:     integer;
  ATeams:   array [0..5] of TeamOrderEntry;
  TempTeam: TeamOrderEntry;
begin
  // TODO: PartyMode: Write this in another way, so that teams with the same score get the same place
  //Fill Team array
  for I := 0 to Teams.NumTeams - 1 do
  begin
    ATeams[I].Teamnum := I;
    ATeams[I].Score := Teams.Teaminfo[I].Score;
  end;

  //Sort teams
  for J := 0 to Teams.NumTeams - 1 do
    for I := 1 to Teams.NumTeams - 1 do
      if ATeams[I].Score > ATeams[I-1].Score then
      begin
        TempTeam    := ATeams[I-1];
        ATeams[I-1] := ATeams[I];
        ATeams[I]   := TempTeam;
      end;

  //Copy to Result
  for I := 0 to Teams.NumTeams-1 do
    Result[I] := ATeams[I].TeamNum;
end;

//----------
//GetWinnerString - Get string with WinnerTeam Name, when there is more than one Winner than Connect with and or ,
//----------
function  TPartySession.GetWinnerString(Round: byte): UTF8String;
var
  Winners: array of UTF8String;
  I:       integer;
begin
  Result := Language.Translate('PARTY_NOBODY');
  
  if (Round > High(Rounds)) then
    exit;

  if (Rounds[Round].Winner = 0) then
  begin
    exit;
  end;

  if (Rounds[Round].Winner = 255) then
  begin
    Result := Language.Translate('PARTY_NOTPLAYEDYET');
    exit;
  end;

  SetLength(Winners, 0);
  for I := 0 to Teams.NumTeams - 1 do
  begin
    if isWinner(I, Rounds[Round].Winner) then
    begin
      SetLength(Winners, Length(Winners) + 1);
      Winners[high(Winners)] := Teams.TeamInfo[I].Name;
    end;
  end;
  Result := Language.Implode(Winners);
end;

end.
