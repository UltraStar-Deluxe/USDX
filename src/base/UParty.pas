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
  UPartyDefs,
  UCoreModule,
  UPluginDefs;

type
  ARounds = array [0..252] of integer; //0..252 needed for
  PARounds = ^ARounds;
  
  TRoundInfo = record
    Modi:   cardinal;
    Winner: byte;
  end;

  TeamOrderEntry = record
    Teamnum: byte;
    Score: byte;
  end;

  TeamOrderArray = array[0..5] of byte;

  TUS_ModiInfoEx = record
    Info:        TUS_ModiInfo;
    Owner:       integer;
    TimesPlayed: byte; //Helper for setting round plugins
  end;

  TPartySession = class (TCoreModule)
  private
    bPartyMode: boolean; //Is this party or single player
    CurRound: byte;

    Modis: array of TUS_ModiInfoEx;
    Teams: TTeamInfo;

    function IsWinner(Player, Winner: byte): boolean;
    procedure GenScores;
    function GetRandomPlugin(TeamMode: boolean): cardinal;
    function GetRandomPlayer(Team: byte): byte;
  public
    //Teams: TTeamInfo;
    Rounds: array of TRoundInfo;

    //TCoreModule methods to inherit
    constructor Create; override;
    procedure Info(const pInfo: PModuleInfo); override;
    function Load: boolean; override;
    function Init: boolean; override;
    procedure DeInit; override;
    destructor Destroy; override;

    //Register modus service
    function RegisterModi(nothin: TwParam; pModiInfo: TlParam): integer; //Registers a new modus. wParam: Pointer to TUS_ModiInfo

    //Start new Party
    function StartParty(NumRounds: TwParam; PAofIRounds: TlParam): integer; //Starts new party mode. Returns non zero on success
    function GetCurModi(wParam: TwParam; lParam: TlParam): integer; //Returns pointer to cur. Modis TUS_ModiInfo (to Use with Singscreen)
    function StopParty(wParam: TwParam; lParam: TlParam): integer; //Stops party mode. Returns 1 if party mode was enabled before.
    function NextRound(wParam: TwParam; lParam: TlParam): integer; //Increases curround by 1; Returns num of round or -1 if last round is already played

    function CallModiInit(wParam: TwParam; lParam: TlParam): integer;    //Calls curmodis init proc. If an error occurs, returns nonzero. In this case a new plugin was selected. Please renew loading
    function CallModiDeInit(wParam: TwParam; lParam: TlParam): integer;  //Calls DeInitProc and ends the round

    function GetTeamInfo(wParam: TwParam; pTeamInfo: TlParam): integer;    //Writes TTeamInfo record to pointer at lParam. Returns zero on success
    function SetTeamInfo(wParam: TwParam; pTeamInfo: TlParam): integer;    //Read TTeamInfo record from pointer at lParam. Returns zero on success

    function  GetTeamOrder(wParam: TwParam; lParam: TlParam): integer;     //Returns team order. Structure: Bits 1..3: Team at place1; Bits 4..6: Team at place2 ...
    function  GetWinnerString(wParam: TwParam; lParam: TlParam): integer;  //wParam is roundnum. If (Pointer = nil) then return length of the string. Otherwise write the string to address at lParam
  end;

const
  StandardModus = 0; //Modus ID that will be played in non-party mode

implementation

uses
  UCore,
  UGraphic,
  ULanguage,
  ULog,
  UNote,
  SysUtils;

{*********************
  TPluginLoader
  Implentation
*********************}

//-------------
// function that gives some infos about the module to the core
//-------------
procedure TPartySession.Info(const pInfo: PModuleInfo);
begin
  pInfo^.Name := 'TPartySession';
  pInfo^.Version := MakeVersion(1,0,0,chr(0));
  pInfo^.Description := 'Manages party modi and party game';
end;

//-------------
// Just the constructor
//-------------
constructor TPartySession.Create;
begin
  inherited;
  //UnSet PartyMode
  bPartyMode := false;
end;

//-------------
//Is called on loading.
//In this method only events and services should be created
//to offer them to other modules or plugins during the init process
//If false is returned this will cause a forced exit
//-------------
function TPartySession.Load: boolean;
begin
  //Add register party modus service
  Result := true;
  Core.Services.AddService('Party/RegisterModi', nil, Self.RegisterModi);
  Core.Services.AddService('Party/StartParty', nil, Self.StartParty);
  Core.Services.AddService('Party/GetCurModi', nil, Self.GetCurModi);
end;

//-------------
//Is called on init process
//In this method you can hook some events and create + init
//your classes, variables etc.
//If false is returned this will cause a forced exit
//-------------
function TPartySession.Init: boolean;
begin
  //Just set private var to true.
  Result := true;
end;

//-------------
//Is called if this module has been inited and there is an exit.
//Deinit is in reverse initing order
//-------------
procedure TPartySession.DeInit;
begin
  //Force DeInit
end;

//-------------
//Is called if this module will be unloaded and has been created
//Should be used to free memory
//-------------
destructor TPartySession.Destroy;
begin
  //Just save some memory if it wasn't done now..
  SetLength(Modis, 0);
  inherited;
end;

//-------------
// Registers a new modus. wParam: Pointer to TUS_ModiInfo
// Service for plugins
//-------------
function TPartySession.RegisterModi(nothin: TwParam; pModiInfo: TlParam): integer;
var
  Len: integer;
  Info: PUS_ModiInfo;
begin
  Info := PModiInfo;
  //Copy Info if cbSize is correct
  if (Info.cbSize = SizeOf(TUS_ModiInfo)) then
  begin
    Len := Length(Modis);
    SetLength(Modis, Len + 1);

    Modis[Len].Info := Info^;
  end
  else
    Core.ReportError(integer(PChar('Plugins try to register modus with wrong pointer, or wrong TUS_ModiInfo record.')), PChar('TPartySession'));

  // FIXME: return a valid result
  Result := 0;
end;

//----------
// Returns a number of a random plugin
//----------
function TPartySession.GetRandomPlugin(TeamMode: boolean): cardinal;
var
  LowestTP: byte;
  NumPwithLTP: word;
  I: integer;
  R: word;
begin
  Result := StandardModus; //If there are no matching modi, play standard modus
  LowestTP := high(byte);
  NumPwithLTP := 0;

  //Search for Plugins not often played yet
  for I := 0 to high(Modis) do
  begin
    if (Modis[I].TimesPlayed < lowestTP) and (((Modis[I].Info.LoadingSettings and MLS_TeamOnly) <> 0) = TeamMode) then
    begin
      lowestTP := Modis[I].TimesPlayed;
      NumPwithLTP := 1;
    end
    else if (Modis[I].TimesPlayed = lowestTP) and (((Modis[I].Info.LoadingSettings and MLS_TeamOnly) <> 0) = TeamMode) then
    begin
      Inc(NumPwithLTP);
    end;
  end;

  //Create random no
  R := Random(NumPwithLTP);

  //Search for random plugin
  for I := 0 to high(Modis) do
  begin
    if (Modis[I].TimesPlayed = lowestTP) and (((Modis[I].Info.LoadingSettings and MLS_TeamOnly) <> 0) = TeamMode) then
    begin
      //Plugin found
      if (R = 0) then
      begin
        Result := I;
        Inc(Modis[I].TimesPlayed);
        Break;
      end;

      Dec(R);
    end;
  end;
end;

//----------
// Starts new party mode. Returns non zero on success
//----------
function TPartySession.StartParty(NumRounds: TwParam; PAofIRounds: TlParam): integer;
var
  I: integer;
  aiRounds: PARounds;
  TeamMode: boolean;
begin
  Result := 0;
  if (Teams.NumTeams >= 1) and (NumRounds < High(byte)-1) then
  begin
    bPartyMode := false;
    aiRounds := PAofIRounds;

    try
      //Is this team mode (More than one player per team) ?
      TeamMode := true;
      for I := 0 to Teams.NumTeams-1 do
        TeamMode := TeamMode and (Teams.Teaminfo[I].NumPlayers > 1);

      //Set Rounds
      SetLength(Rounds, NumRounds);

      for I := 0 to High(Rounds) do
      begin //Set plugins
        if (aiRounds[I] = -1) then
          Rounds[I].Modi := GetRandomPlugin(TeamMode)
        else if (aiRounds[I] >= 0) and (aiRounds[I] <= High(Modis)) and (TeamMode or ((Modis[aiRounds[I]].Info.LoadingSettings and MLS_TeamOnly) = 0))  then
          Rounds[I].Modi := aiRounds[I]
        else
          Rounds[I].Modi := StandardModus;

        Rounds[I].Winner := High(byte); //Set winner to not played
      end;

      CurRound := High(byte); //Set CurRound to not defined

      //Return true and set party mode
      bPartyMode := true;
      Result := 1;

    except
      Core.ReportError(integer(PChar('Can''t start party mode.')), PChar('TPartySession'));
    end;
  end;
end;

//----------
// Returns pointer to Cur. ModiInfoEx (to use with sing screen)
//----------
function TPartySession.GetCurModi(wParam: TwParam; lParam: TlParam): integer;
begin
  if (bPartyMode) and (CurRound <= High(Rounds)) then
  begin //If PartyMode is enabled:
    //Return the Plugin of the Cur Round
    Result := integer(@Modis[Rounds[CurRound].Modi]);
  end
  else
  begin //Return standard modus
    Result := integer(@Modis[StandardModus]);
  end;
end;

//----------
// Stops party mode. Returns 1 if party mode was enabled before and -1 if change was not possible
//----------
function TPartySession.StopParty(wParam: TwParam; lParam: TlParam): integer;
begin
  Result := -1;
  if (bPartyMode) then
  begin
    // to-do : Whitü: Check here if sing screen is not shown atm.
    bPartyMode := false;
    Result := 1;
  end
  else
    Result := 0;
end;

//----------
//GetRandomPlayer - gives back a random player to play next round
//----------
function TPartySession.GetRandomPlayer(Team: byte): byte;
var
  I, R: integer;
  lowestTP: byte;
  NumPwithLTP: byte;
begin
  LowestTP := high(byte);
  NumPwithLTP := 0;
  Result := 0;

  //Search for players that have not often played yet
  for I := 0 to Teams.Teaminfo[Team].NumPlayers-1 do
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

  //Create random no
  R := Random(NumPwithLTP);

  //Search for random player
  for I := 0 to Teams.Teaminfo[Team].NumPlayers-1 do
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

//----------
// NextRound - Increases CurRound by 1; Returns num of round or -1 if last round is already played
//----------
function TPartySession.NextRound(wParam: TwParam; lParam: TlParam): integer;
var
  I: integer;
begin
  if ((CurRound < high(Rounds)) or (CurRound = high(CurRound))) then
  begin //everythings OK! -> Start the Round, maaaaan
    Inc(CurRound);

    //Set Players to play this Round
    for I := 0 to Teams.NumTeams-1 do
      Teams.Teaminfo[I].CurPlayer := GetRandomPlayer(I);
      
    // FIXME: return a valid result
    Result := 0;
  end
  else
    Result := -1;
end;

//----------
//IsWinner - returns true if the players bit is set in the winner byte
//----------
function TPartySession.IsWinner(Player, Winner: byte): boolean;
var
  Bit: byte;
begin
  Bit := 1 shl Player;

  Result := ((Winner and Bit) = Bit);
end;

//----------
//GenScores - inc scores for cur. round
//----------
procedure TPartySession.GenScores;
var
  I: byte;
begin
  for I := 0 to Teams.NumTeams-1 do
  begin
    if isWinner(I, Rounds[CurRound].Winner) then
      Inc(Teams.Teaminfo[I].Score);
  end;
end;

//----------
// CallModiInit - calls CurModis Init Proc. If an error occurs, returns nonzero. In this case a new plugin was selected. Please renew loading
//----------
function TPartySession.CallModiInit(wParam: TwParam; lParam: TlParam): integer;
begin
  if (not bPartyMode) then
  begin //Set rounds if not in party mode
    SetLength(Rounds, 1);
    Rounds[0].Modi := StandardModus;
    Rounds[0].Winner := High(byte);
    CurRound := 0;
  end;

  try
    //Core.
  except
    on E : Exception do
    begin
      Core.ReportError(integer(PChar('Error starting modus: ' + Modis[Rounds[CurRound].Modi].Info.Name + ' ErrorStr: ' + E.Message)), PChar('TPartySession'));
      if (Rounds[CurRound].Modi = StandardModus) then
      begin
        Core.ReportError(integer(PChar('Can''t start standard modus, will exit now!')), PChar('TPartySession'));
        Halt;
      end
      else //Select standard modus
      begin
        Rounds[CurRound].Modi := StandardModus
      end;
    end;
  end;

  // FIXME: return a valid result
  Result := 0;
end;

//----------
// CallModiDeInit - calls DeInitProc and ends the round
//----------
function TPartySession.CallModiDeInit(wParam: TwParam; lParam: TlParam): integer;
var
  I: integer;
  MaxScore: word;
begin
  if (bPartyMode) then
  begin
    //Get Winner Byte!
    if (@Modis[Rounds[CurRound].Modi].Info.ModiDeInit <> nil) then //get winners from plugin
      Rounds[CurRound].Winner := Modis[Rounds[CurRound].Modi].Info.ModiDeInit(Modis[Rounds[CurRound].Modi].Info.ID)
    else
    begin //Create winners by score :/
      Rounds[CurRound].Winner := 0;
      MaxScore := 0;
      for I := 0 to Teams.NumTeams-1 do
      begin
        // to-do : recode percentage stuff
        //PlayerInfo.Playerinfo[I].Percentage := PlayerInfo.Playerinfo[I].Score div 9999;
        if (Player[I].ScoreTotalInt > MaxScore) then
        begin
          MaxScore := Player[I].ScoreTotalInt;
          Rounds[CurRound].Winner := 1 shl I;
        end
        else if (Player[I].ScoreTotalInt = MaxScore) and (Player[I].ScoreTotalInt <> 0) then
        begin
          Rounds[CurRound].Winner := Rounds[CurRound].Winner or (1 shl I);
        end;
      end;


      //When nobody has points -> everybody looses
      if (MaxScore = 0) then
        Rounds[CurRound].Winner := 0;

    end;

    //Generate the scores
    GenScores;

    //Inc players TimesPlayed
    if ((Modis[Rounds[CurRound-1].Modi].Info.LoadingSettings and MLS_IncTP) = MLS_IncTP) then
    begin
      for I := 0 to Teams.NumTeams-1 do
        Inc(Teams.TeamInfo[I].Playerinfo[Teams.TeamInfo[I].CurPlayer].TimesPlayed);
    end;
  end
  else if (@Modis[Rounds[CurRound].Modi].Info.ModiDeInit <> nil) then
    Modis[Rounds[CurRound].Modi].Info.ModiDeInit(Modis[Rounds[CurRound].Modi].Info.ID);

  // FIXME: return a valid result
  Result := 0;
end;

//----------
// GetTeamInfo - writes TTeamInfo record to pointer at lParam. Returns zero on success
//----------
function TPartySession.GetTeamInfo(wParam: TwParam; pTeamInfo: TlParam): integer;
var
  Info: ^TTeamInfo;
begin
  Result := -1;
  Info := pTeamInfo;
  if (Info <> nil) then
  begin
    try
      // to - do : Check Delphi memory management in this case
      //Not sure if i had to copy PChars to a new address or if delphi manages this o0
      Info^  := Teams;
      Result := 0;
    except
      Result := -2;
    end;
  end;
end;

//----------
// SetTeamInfo - read TTeamInfo record from pointer at lParam. Returns zero on success
//----------
function TPartySession.SetTeamInfo(wParam: TwParam; pTeamInfo: TlParam): integer;
var
  TeamInfobackup: TTeamInfo;
  Info: ^TTeamInfo;
begin
  Result := -1;
  Info := pTeamInfo;
  if (Info <> nil) then
  begin
    try
      TeamInfoBackup := Teams;
      // to - do : Check Delphi memory management in this case
      //Not sure if i had to copy PChars to a new address or if delphi manages this o0
      Teams := Info^;
      Result := 0;
    except
      Teams := TeamInfoBackup;
      Result := -2;
    end;
  end;
end;

//----------
// GetTeamOrder - returns team order. Structure: Bits 1..3: Team at place1; Bits 4..6: Team at place2 ...
//----------
function  TPartySession.GetTeamOrder(wParam: TwParam; lParam: TlParam): integer;
var
  I, J: integer;
  ATeams: array [0..5] of TeamOrderEntry;
  TempTeam: TeamOrderEntry;
begin
  // to-do : PartyMode: Write this in another way, so that teams with the same score get the same place
  //Fill Team array
  for I := 0 to Teams.NumTeams-1 do
  begin
    ATeams[I].Teamnum := I;
    ATeams[I].Score := Teams.Teaminfo[I].Score;
  end;

  //Sort teams
  for J := 0 to Teams.NumTeams-1 do
    for I := 1 to Teams.NumTeams-1 do
      if ATeams[I].Score > ATeams[I-1].Score then
      begin
        TempTeam    := ATeams[I-1];
        ATeams[I-1] := ATeams[I];
        ATeams[I]   := TempTeam;
      end;

  //Copy to Result
  Result := 0;
  for I := 0 to Teams.NumTeams-1 do
    Result := Result or (ATeams[I].TeamNum Shl I*3);
end;

//----------
// GetWinnerString - wParam is Roundnum. If (pointer = nil) then return length of the string. Otherwise write the string to address at lParam
//----------
function  TPartySession.GetWinnerString(wParam: TwParam; lParam: TlParam): integer;
var
  Winners: array of String;
  I: integer;
  ResultStr: String;
  S: ^String;
begin
  ResultStr := Language.Translate('PARTY_NOBODY');

  if (wParam <= High(Rounds)) then
  begin
    if (Rounds[wParam].Winner <> 0) then
    begin
      if (Rounds[wParam].Winner = 255) then
      begin
        ResultStr := Language.Translate('PARTY_NOTPLAYEDYET');
      end
      else
      begin
        SetLength(Winners, 0);
        for I := 0 to Teams.NumTeams-1 do
        begin
          if isWinner(I, Rounds[wParam].Winner) then
          begin
            SetLength(Winners, Length(Winners) + 1);
            Winners[high(Winners)] := Teams.TeamInfo[I].Name;
          end;
        end;
        ResultStr := Language.Implode(Winners);
      end;
    end;
  end;

  //Now return what we have got
  if (lParam = nil) then
  begin //Return string length
    Result := Length(ResultStr);
  end
  else
  begin //Return string
    try
      S := lParam;
      S^ := ResultStr;
      Result := 0;
    except
      Result := -1;

    end;
  end;
end;

end.
