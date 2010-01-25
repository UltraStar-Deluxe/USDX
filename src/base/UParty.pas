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
  ULua;

type
  { array holds ids of modes or Party_Round_Random
    its length defines the number of rounds
    it is used as argument for TPartyGame.StartParty }
  ARounds = array of integer;

  { element of APartyTeamRanking returned by TPartyGame.GetTeamRanking
    and parameter for TPartyGame.SetWinner }
  TParty_TeamRanking = record
    Team: Integer; //< id of team
    Rank: Integer; //< 1 to Length(Teams) e.g. 1 is for placed first
  end;
  AParty_TeamRanking = array of TParty_TeamRanking; //< returned by TPartyGame.GetTeamRanking

  TParty_RoundList = record
    Index: integer;
    Name: UTF8String;
  end;
  AParty_ModeList = array of TParty_RoundList;

  { record used by TPartyGame to store round specific data }
  TParty_Round = record
    Mode:   Integer;
    AlreadyPlayed: Boolean; //< true if round was already played
    Ranking: AParty_TeamRanking;
    RankingSet: Boolean; //< true if Self.Ranking is already set
  end;

  TParty_ModeInfo = record
    Name: String; // name of this mode
    Parent: Integer;   // Id of owning plugin

    CanNonParty: Boolean; //< is playable when not in party mode
    CanParty: Boolean;    //< is playable in party mode

    // one bit in the following settings stands for
    // a player or team count
    // PlayerCount = 2 or 4 indicates that the mode is playable with 2 and 3 players per team
    // TeamCount = 1 or 2 or 4 or 8 or 16 or 32 indicates that the mode is playable with 1 to 6 teams
    PlayerCount: Integer;   //< playable with one, two, three etc. players per team
    TeamCount: Integer;     //< playable with one, two, three etc. different teams


    Functions: record // lua functions that will be called at specific events
      BeforeSongSelect: String; // default actions are executed if functions = nil
      AfterSongSelect: String;

      BeforeSing: String;
      OnSing: String;
      AfterSing: String;
    end;
  end;

  { used by TPartyGame to store player specific data }
  TParty_PlayerInfo = record
    Name: String;         //< Playername
    TimesPlayed: Integer; //< How often this Player has Sung
  end;

  { used by TPartyGame to store team specific data }
  TParty_TeamInfo = record
    Name:  String;        //< name of the Team
    Score: Word;          //< current score
    JokersLeft: Integer;  //< jokers this team has left

    NextPlayer: Integer;  //Id of the player that plays the next (the current) song

    Players: array of TParty_PlayerInfo;
  end;

  TPartyGame = class
  private
    bPartyGame: boolean; //< are we playing party or standard mode
    CurRound: Integer;   //< indicates which of the elements of Rounds is played next (at the moment)

    bPartyStarted: Boolean;

    TimesPlayed: array of Integer; //< times every mode was played in current party game (for random mode calculation)

    procedure GenScores;
    function GetRandomMode: integer;
    function GetRandomPlayer(Team: integer): integer;

    { returns true if a mode is playable with current playerconfig }
    function ModePlayable(I: integer): boolean;

    function CallLua(Parent: Integer; Func: String):Boolean;

    procedure SetRankingByScore;
  public
    //Teams: TTeamInfo;
    Rounds: array of TParty_Round;    //< holds info which modes are played in this party game (if started)
    Teams: array of TParty_TeamInfo;  //< holds info of teams playing in current round (private for easy manipulation of lua functions)

    Modes: array of TParty_ModeInfo;  //< holds info of registred party modes

    property CurrentRound: Integer read CurRound;

    constructor Create;

    { set the attributes of Info to default values }
    procedure DefaultModeInfo(var Info: TParty_ModeInfo);

    { registers a new mode, returns true on success
      (mode name does not already exist) }
    function RegisterMode(Info: TParty_ModeInfo): Boolean;

    { returns true if modes are available for
      players and teams that are currently set
      up. if there are no teams set up it returns
      if there are any party modes available }
    function ModesAvailable: Boolean;

    { returns an array with the name of all available modes (that
      are playable with current player configuration }
    function GetAvailableModes: AParty_ModeList;

    { clears all party specific data previously stored }
    procedure Clear;

    { adds a team to the team array, returning its id
      can only be called when game is not already started }
    function AddTeam(Name: String): Integer;

    { adds a player to the player array, returning its id
      can only be called when game is not already started }
    function AddPlayer(Team: Integer; Name: String): Integer;

    { starts a new PartyGame, returns true on success
      before a call of this function teams and players
      has to be added by AddTeam and AddPlayer }

    function StartGame(Rounds: ARounds): Boolean;

    { sets the winner(s) of current round
      returns true on success }
    function SetRanking(Ranking: AParty_TeamRanking): Boolean;

    { increases round counter by 1 and clears all round specific information;
      returns the number of the current round or -1 if last round has already
      been played }
    function NextRound: integer;

    { indicates that current round has already been played }
    procedure RoundPlayed;

    { true if in a Party Game (not in standard mode) }
    property PartyGame: Boolean read BPartyGame;


    { returns true if last round was already played }
    function GameFinished: Boolean;

    { call plugins defined function and/or default procedure
      only default procedure is called when no function is defined by plugin
      if plugins function returns true then default is called after plugins
      function was executed}
    procedure CallBeforeSongSelect;
    procedure CallAfterSongSelect;
    procedure CallBeforeSing;
    procedure CallOnSing;
    procedure CallAfterSing;

    { returns an array[1..6] of TParty_TeamRanking.
      the index stands for the placing,
      team is the team number (in the team array)
      rank is correct rank if some teams have the
      same score. 
      }
    function GetTeamRanking: AParty_TeamRanking;

    { returns a string like "Team 1 (and Team 2) win" }
    function GetWinnerString(Round: integer): UTF8String;

    destructor  Destroy;
  end;

const
  { minimal amount of teams for party mode }
  Party_Teams_Min = 2;

  { maximal amount of teams for party mode }
  Party_Teams_Max = 3;

  { minimal amount of players for party mode }
  Party_Players_Min = 1;

  { maximal amount of players for party mode }
  Party_Players_Max = 4;

  { amount of jokers each team gets at the beginning of the game }
  Party_Count_Jokers = 5;

  { to indicate that element (mode) should set randomly in ARounds array }
  Party_Round_Random = -1;

  { values for TParty_TeamRanking.Rank }
  PR_First = 1;
  PR_Second = 2;
  PR_Third = 3;
  
  StandardModus = 0; //Modus Id that will be played in non-party mode

var
  Party: TPartyGame;

implementation

uses
  UGraphic,
  ULanguage,
  ULog,
  ULuaCore,
  UDisplay,
  USong,
  UNote,
  SysUtils;

//-------------
// Just the constructor
//-------------
constructor TPartyGame.Create;
begin
  inherited;

  Clear;
end;

destructor TPartyGame.Destroy;
begin
  inherited;
end;

{ clears all party specific data previously stored }
procedure TPartyGame.Clear;
  var
    I: Integer;
begin
  bPartyGame := false; // no party game
  CurRound := low(integer);

  bPartyStarted := false; //game not startet

  SetLength(Teams, 0); //remove team info
  SetLength(Rounds, 0); //remove round info

  // clear times played
  for I := 0 to High(TimesPlayed) do
    TimesPlayed[I] := 0;
end;

{ private: some intelligent randomnes for plugins }
function TPartyGame.GetRandomMode: integer;
var
  LowestTP: integer;
  NumPwithLTP: integer;
  I: integer;
  R: integer;
begin
  Result := 0; //If there are no matching modes, play first modus
  LowestTP := high(Integer);
  NumPwithLTP := 0;

  // search for the plugins less played yet
  for I := 0 to high(Modes) do
  begin
    if (ModePlayable(I)) then
    begin
      if (TimesPlayed[I] < lowestTP) then
      begin
        lowestTP := TimesPlayed[I];
        NumPwithLTP := 1;
      end
      else if (TimesPlayed[I] = lowestTP) then
      begin
        Inc(NumPwithLTP);
      end;
    end;
  end;

  // create random number
  R := Random(NumPwithLTP);

  // select the random mode from the modes with less timesplayed
  for I := 0 to high(Modes) do
  begin
    if (TimesPlayed[I] = lowestTP) and (ModePlayable(I)) then
    begin
      //Plugin found
      if (R = 0) then
      begin
        Result := I;
        Inc(TimesPlayed[I]);
        Break;
      end;

      Dec(R);
    end;
  end;
end;

{ private: GetRandomPlayer - returns a random player
                             that does not play to often ;) }
function TPartyGame.GetRandomPlayer(Team: integer): integer;
var
  I, R: integer;
  lowestTP: Integer;
  NumPwithLTP: Integer;
begin
  LowestTP := high(Integer);
  NumPwithLTP := 0;
  Result := 0;

  // search for players that have less played yet
  for I := 0 to High(Teams[Team].Players) do
  begin
    if (Teams[Team].Players[I].TimesPlayed < lowestTP) then
    begin
      lowestTP := Teams[Team].Players[I].TimesPlayed;
      NumPwithLTP := 1;
    end
    else if (Teams[Team].Players[I].TimesPlayed = lowestTP) then
    begin
      Inc(NumPwithLTP);
    end;
  end;

  // create random number
  R := Random(NumPwithLTP);

  // search for selected random player
  for I := 0 to High(Teams[Team].Players) do
  begin
    if Teams[Team].Players[I].TimesPlayed = lowestTP then
    begin
      if (R = 0) then
      begin // found selected player
        Result := I;
        Break;
      end;

      Dec(R);
    end;
  end;
end;

//----------
//GenScores - inc scores for cur. round
//----------
procedure TPartyGame.GenScores;
var
  I: Integer;
begin
  if (Length(Teams) = 2) then
  begin // score generation for 2 teams, winner gets 1 point
    for I := 0 to High(Rounds[CurRound].Ranking) do
      if (Rounds[CurRound].Ranking[I].Rank = PR_First) then
        Inc(Teams[Rounds[CurRound].Ranking[I].Team].Score);
  end
  else if (Length(Teams) = 3) then
  begin // score generation for 3 teams,
    // winner gets 3 points 2nd gets 1 point
    for I := 0 to High(Rounds[CurRound].Ranking) do
      if (Rounds[CurRound].Ranking[I].Rank = PR_First) then
        Inc(Teams[Rounds[CurRound].Ranking[I].Team].Score, 3)
      else if (Rounds[CurRound].Ranking[I].Rank = PR_Second) then
        Inc(Teams[Rounds[CurRound].Ranking[I].Team].Score);
  end
end;

{ set the attributes of Info to default values }
procedure TPartyGame.DefaultModeInfo(var Info: TParty_ModeInfo);
begin
  Info.Name := 'undefined';
  Info.Parent := -1; //< not loaded by plugin (e.g. Duell)
  Info.CanNonParty := false;
  Info.CanParty := false;
  Info.PlayerCount := High(Integer); //< no restrictions either on player count
  Info.TeamCount := High(Integer);   //< nor on team count
  Info.Functions.BeforeSongSelect := ''; //< use default functions
  Info.Functions.AfterSongSelect  := '';
  Info.Functions.BeforeSing       := '';
  Info.Functions.OnSing           := '';
  Info.Functions.AfterSing        := '';
end;

{ registers a new mode, returns true on success
  (mode name does not already exist) }
function TPartyGame.RegisterMode(Info: TParty_ModeInfo): Boolean;
  var
    Len: integer;
    LowerName: String;
    I: integer;
begin
  Result := false;

  if (Info.Name <> 'undefined') then
  begin
    // search for a plugin w/ same name
    LowerName := lowercase(Info.Name); // case sensitive search
    for I := 0 to high(Modes) do
      if (LowerName = lowercase(Modes[I].Name)) then
        exit; //< no success (name already exist)

    // add new mode to array and append and clear a new TimesPlayed element
    Len := Length(Modes);
    SetLength(Modes, Len + 1);
    SetLength(TimesPlayed, Len + 1);

    Modes[Len] := Info;
    TimesPlayed[Len] := 0;

    Result := True;
  end;
end;

{ returns true if a mode is playable with current playerconfig }
function TPartyGame.ModePlayable(I: integer): boolean;
  var
    J: integer;
begin
  if (Length(Teams) = 0) then
    Result := true
  else
  begin
    if (Modes[I].TeamCount and (1 shl (Length(Teams) - 1)) <> 0) then
    begin
      Result := true;

      for J := 0 to High(Teams) do
        Result := Result and (Modes[I].PlayerCount and (1 shl (Length(Teams[J].Players) - 1)) <> 0);
    end
    else
      Result := false;
  end;
end;

{ returns true if modes are available for
  players and teams that are currently set
  up. if there are no teams set up it returns
  if there are any party modes available }
function TPartyGame.ModesAvailable: Boolean;
  var
    I: integer;
    CountTeams: integer;
begin
  CountTeams := Length(Teams);
  if CountTeams = 0 then
  begin
    Result := (Length(Modes) > 0);
  end
  else
  begin
    Result := false;
    for I := 0 to High(Modes) do
    begin
      Result := ModePlayable(I);

      if Result then
        Exit;
    end;
  end;
end;       

{ returns an array with the name of all available modes (that
  are playable with current player configuration }
function TPartyGame.GetAvailableModes: AParty_ModeList;
  var
    I: integer;
    Len: integer;
begin
  Len := 0;
  SetLength(Result, Len + 1);
  Result[Len].Index := Party_Round_Random;
  Result[Len].Name := Language.Translate('MODE_RANDOM_NAME');

  for I := 0 to High(Modes) do
    if (ModePlayable(I)) then
    begin
      Inc(Len);
      SetLength(Result, Len + 1);
      Result[Len].Index := I;
      Result[Len].Name := Language.Translate('MODE_' + Uppercase(Modes[I].Name) + '_NAME');
    end;
end;

{ adds a team to the team array, returning its id
  can only be called when game is not already started }
function TPartyGame.AddTeam(Name: String): Integer;
begin
  Result := -1;
  if (not bPartyStarted) and (Length(Name) > 0) and (Length(Teams) < Party_Teams_Max) then
  begin
    Result := Length(Teams);
    SetLength(Teams, Result + 1);

    Teams[Result].Name := Name;
    Teams[Result].Score := 0;
    Teams[Result].JokersLeft := Party_Count_Jokers;
    Teams[Result].NextPlayer := -1;
  end;
end;

{ adds a player to the player array, returning its id
  can only be called when game is not already started }
function TPartyGame.AddPlayer(Team: Integer; Name: String): Integer;
begin
  Result := -1;

  if (not bPartyStarted) and (Team >= 0) and (Team <= High(Teams)) and (Length(Teams[Team].Players) < Party_Players_Max) and (Length(Name) > 0) then
  begin
    // append element to players array
    Result := Length(Teams[Team].Players);
    SetLength(Teams[Team].Players, Result + 1);

    // fill w/ data
    Teams[Team].Players[Result].Name := Name;
    Teams[Team].Players[Result].TimesPlayed := 0;
  end;
end;

{ starts a new PartyGame, returns true on success
  before a call of this function teams and players
  has to be added by AddTeam and AddPlayer }
function TPartyGame.StartGame(Rounds: ARounds): Boolean;
  var
    I: integer;
begin
  Result := false;

  if (not bPartyStarted) and (Length(Rounds) > 0) and (Length(Teams) >= Party_Teams_Min) then
  begin
    // check teams for minimal player count
    for I := 0 to High(Teams) do
      if (Length(Teams[I].Players) < Party_Players_Min) then
        exit;

    // create rounds array
    SetLength(Self.Rounds, Length(Rounds));

    for I := 0 to High(Rounds) do
    begin
      // copy round or select a random round
      if (Rounds[I] <> Party_Round_Random) and (Rounds[I] >= 0) and (Rounds[I] <= High(Modes)) then
        Self.Rounds[I].Mode := Rounds[I]
      else
        Self.Rounds[I].Mode := GetRandomMode;

      Self.Rounds[I].AlreadyPlayed := false;
      Self.Rounds[I].RankingSet := false;

      SetLength(Self.Rounds[I].Ranking, 0);
    end;

    // get the party started!11
    bPartyStarted := true;
    bPartyGame := true;
    CurRound := low(integer); //< set not to -1 to indicate that party game is not finished

    // first round
    NextRound;

    Result := True;
  end;
end;

{ sets the winner(s) of current round
  returns true on success }
function TPartyGame.SetRanking(Ranking: AParty_TeamRanking): Boolean;
  var
    I, J: Integer;
    TeamExists: Integer;
    Len: Integer;
    Temp: TParty_TeamRanking;
begin
  if (bPartyStarted) and (CurRound >= 0) and (CurRound <= High(Rounds)) then
  begin
    Rounds[CurRound].Ranking := Ranking;
    Result := true;

    // look for teams that don't exist
    TeamExists := 0;
    for I := 0 to High(Rounds[CurRound].Ranking) do
      TeamExists := TeamExists or (1 shl (Rounds[CurRound].Ranking[I].Team-1));

    // create teams that don't exist
    Len := Length(Rounds[CurRound].Ranking);
    for I := 0 to High(Teams) do
      if (TeamExists and (1 shl I) = 0) then
      begin
        Inc(Len);
        SetLength(Rounds[CurRound].Ranking, Len);
        Rounds[CurRound].Ranking[Len-1].Team := I + 1;
        Rounds[CurRound].Ranking[Len-1].Rank := Length(Teams);
      end;

    // we may remove rankings from invalid teams here to
    // but at the moment this is not necessary, because the
    // functions this function is called from don't create
    // invalid rankings

    // bubble sort rankings by team
    J := High(Rounds[CurRound].Ranking);
    repeat
      for I := 0 to J - 1 do
        if (Rounds[CurRound].Ranking[I].Team > Rounds[CurRound].Ranking[I+1].Team) then
        begin
          Temp := Rounds[CurRound].Ranking[I];
          Rounds[CurRound].Ranking[I] := Rounds[CurRound].Ranking[I+1];
          Rounds[CurRound].Ranking[I+1] := Temp;
        end;
      Dec(J);
    until J <= 0;

    //set rounds RankingSet to true
    Rounds[CurRound].RankingSet := true;
  end
  else
    Result := false;
end;

{ sets ranking of current round by score saved in players array }
procedure TPartyGame.SetRankingByScore;
  var
    I, J: Integer;
    Rank: Integer;
    Ranking: AParty_TeamRanking;
    Scores: array of Integer;
    TmpRanking: TParty_TeamRanking;
    TmpScore: Integer;
begin
  if (Length(Player) = Length(Teams)) then
  begin
    SetLength(Ranking, Length(Teams));
    SetLength(Scores, Length(Teams));

    // fill ranking array
    for I := 0 to High(Ranking) do
    begin
      Ranking[I].Team := I;
      Ranking[I].Rank := 0;
      Scores[I] := Player[I].ScoreTotalInt;
    end;

    // bubble sort by score
    J := High(Ranking);
    repeat
      for I := 0 to J - 1 do
        if (Scores[I] < Scores[I+1]) then
        begin
          TmpRanking := Ranking[I];
          Ranking[I] := Ranking[I+1];
          Ranking[I+1] := TmpRanking;

          TmpScore := Scores[I];
          Scores[I] := Scores[I+1];
          Scores[I+1] := TmpScore;
        end;
      Dec(J);
    until J <= 0;

    // set rank field
    Rank := 1; //first rank has id 1
    for I := 0 to High(Ranking) do
    begin
      Ranking[I].Rank := Rank;

      if (I < High(Ranking)) and (Scores[I] <> Scores[I+1]) then
        Inc(Rank); // next rank if next team has different score
    end;
  end
  else
    SetLength(Ranking, 0);

  SetRanking(Ranking);
end;

{ increases round counter by 1 and clears all round specific information;
  returns the number of the current round or -1 if last round has already
  been played }
function TPartyGame.NextRound: integer;
  var I: Integer;
begin
  // some lines concerning the previous round
  if (CurRound >= 0) then
  begin
    Rounds[CurRound].AlreadyPlayed := true;

    GenScores;
  end;

  // increase round counter
  Inc(CurRound);
  if (CurRound < -1) then // we start first round
    CurRound := 0;

  if (CurRound > High(Rounds)) then
    CurRound := -1; //< last round played

  Result := CurRound;

  // some lines concerning the next round
  if (CurRound >= 0) then
  begin
    // select player
    for I := 0 to High(Teams) do
      Teams[I].NextPlayer := GetRandomPlayer(I);
  end;
end;

{ indicates that current round has already been played }
procedure TPartyGame.RoundPlayed;
begin
  if (bPartyStarted) and (CurRound >= 0) and (CurRound <= High(Rounds)) then
  begin
    // set rounds ranking by score if it was not set by plugin
    if (not Rounds[CurRound].RankingSet) then
      SetRankingByScore;

    Rounds[CurRound].AlreadyPlayed := True;
  end;
end;

{ returns true if last round was already played }
function TPartyGame.GameFinished: Boolean;
begin
  Result := (bPartyStarted and (CurRound = -1));
end;

{ private: calls the specified function Func from lua plugin Parent
           if both exist.
           return true if default function should be called
           (function or plugin does not exist, or function returns
           true) }
function TPartyGame.CallLua(Parent: Integer; Func: String):Boolean;
  var
    P: TLuaPlugin;
begin
  // call default function by default
  Result := true;

  // check for core plugin and empty function name
  if (Parent >= 0) and (Length(Func) > 0) then
  begin
    // get plugin that registred the mode
    P := LuaCore.GetPluginById(Parent);

    if (P <> nil) then
    begin
      if (P.CallFunctionByName(Func, 0, 1)) then
        // check result
        Result := (lua_toboolean(P.LuaState, 1));
    end;
  end;
end;

{ call plugins defined function and/or default procedure
  only default procedure is called when no function is defined by plugin
  if plugins function returns true then default is called after plugins
  function was executed}
procedure TPartyGame.CallBeforeSongSelect;
  var
    ExecuteDefault: boolean;
begin
  if not bPartyStarted then
    ExecuteDefault := true
  else if (CurRound >= 0) then
  begin
    // we set screen song to party mode
    // plugin should not have to do this if it
    // don't want default procedure to be executed
    ScreenSong.Mode := smPartyMode;

    with Modes[Rounds[CurRound].Mode] do
      ExecuteDefault := (CallLua(Parent, Functions.BeforeSongSelect));
  end
  else
    ExecuteDefault := true;

  // execute default function:
  if ExecuteDefault then
  begin
    // display song select screen
    Display.FadeTo(@ScreenSong);
  end;
end;

procedure TPartyGame.CallAfterSongSelect;
  var
    ExecuteDefault: boolean; 
begin
  if not bPartyStarted then
    ExecuteDefault := true
  else if (CurRound >= 0) then
  begin
    with Modes[Rounds[CurRound].Mode] do
      ExecuteDefault := (CallLua(Parent, Functions.AfterSongSelect));
  end
  else
    ExecuteDefault := true;

  // execute default function:
  if ExecuteDefault then
  begin
    // display sing screen
    ScreenSong.StartSong;
  end;
end;

procedure TPartyGame.CallBeforeSing;
  var
    ExecuteDefault: boolean;
begin
  if not bPartyStarted then
    ExecuteDefault := true
  else if (CurRound >= 0) then
  begin
    with Modes[Rounds[CurRound].Mode] do
      ExecuteDefault := (CallLua(Parent, Functions.BeforeSing));
  end
  else
    ExecuteDefault := true;

  // execute default function:
  if ExecuteDefault then
  begin
    //nothing atm
    { to-do : compartmentalize TSingScreen.OnShow into
              functions for init of a specific part of
              sing screen.
              these functions should be called here before
              sing screen is shown, or it should be called
              by plugin if it wants to define a custom
              singscreen start up. }

    //set correct playersplay
    if (bPartyGame) then
      PlayersPlay := Length(Teams);
  end;
end;

procedure TPartyGame.CallOnSing;
  var
    ExecuteDefault: boolean;
begin
  if not bPartyStarted then
    ExecuteDefault := true
  else if (CurRound >= 0) then
  begin
    with Modes[Rounds[CurRound].Mode] do
      ExecuteDefault := (CallLua(Parent, Functions.OnSing));;
  end
  else
    ExecuteDefault := true;

  // execute default function:
  if ExecuteDefault then
  begin
    //nothing atm
  end;
end;

procedure TPartyGame.CallAfterSing;
  var
    ExecuteDefault: boolean;
begin
  if not bPartyStarted then
    ExecuteDefault := true
  else if (CurRound >= 0) then
  begin
    with Modes[Rounds[CurRound].Mode] do
      ExecuteDefault := (CallLua(Parent, Functions.AfterSing));
  end
  else
    ExecuteDefault := true;

  // execute default function:
  if ExecuteDefault then
  begin
    if (bPartyGame) then
      // display party score screen
      Display.FadeTo(@ScreenPartyScore)
    else //display standard score screen
      Display.FadeTo(@ScreenScore);
  end;
end;

{ returns an array[1..6] of integer. the index stands for the placing,
  value is the team number (in the team array) }
function TPartyGame.GetTeamRanking: AParty_TeamRanking;
  var
    I, J: Integer;
    Temp: TParty_TeamRanking;
    Rank: Integer;
begin
  SetLength(Result, Length(Teams));

  // fill ranking array
  for I := 0 to High(Result) do
  begin
    Result[I].Team := I;
    Result[I].Rank := 0;
  end;

  // bubble sort by score
  J := High(Result);
  repeat
    for I := 0 to J - 1 do
      if (Teams[Result[I].Team].Score < Teams[Result[I+1].Team].Score) then
      begin
        Temp := Result[I];
        Result[I] := Result[I+1];
        Result[I+1] := Temp;
      end;
    Dec(J);
  until J <= 0;

  // set rank field
  Rank := 1; //first rank has id 1
  for I := 0 to High(Result) do
  begin
    Result[I].Rank := Rank;

    if (I < High(Result)) and (Teams[Result[I].Team].Score <> Teams[Result[I+1].Team].Score) then
      Inc(Rank); // next rank if next team has different score 
  end; 
end;

{ returns a string like "Team 1 (and Team 2) win"
  if Round is in range from 0 to high(Rounds) then
  result is name of winners of specified round.
  if Round is -1 the result is name of winners of
  the whole party game}
function TPartyGame.GetWinnerString(Round: integer): UTF8String;
var
  Winners: array of UTF8String;
  I: integer;
  Ranking: AParty_TeamRanking;
begin
  Result := '';
  Ranking := nil;
  
  if (Round >= 0) and (Round <= High(Rounds)) then
  begin
    if (not Rounds[Round].AlreadyPlayed) then
      Result := Language.Translate('PARTY_NOTPLAYEDYET')
    else
      Ranking := Rounds[Round].Ranking;
  end
  else if (Round = -1) then
    Ranking := GetTeamRanking;


  if (Ranking <> nil) then
  begin
    SetLength(Winners, 0);
    for I := 0 to High(Ranking) do
    begin
      if (Ranking[I].Rank = PR_First) and (Ranking[I].Team >= 0) and (Ranking[I].Team <= High(Teams)) then
      begin
        SetLength(Winners, Length(Winners) + 1);
        Winners[high(Winners)] := UTF8String(Teams[Ranking[I].Team].Name);
      end;
    end;

    if (Length(Winners) > 0) then
      Result := Language.Implode(Winners);
  end;

  if (Length(Result) = 0) then
    Result := Language.Translate('PARTY_NOBODY');
end;

end.
