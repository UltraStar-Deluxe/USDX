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
  ARounds = Array [0..252] of Integer; //0..252 needed for
  PARounds = ^ARounds;
  
  TRoundInfo = record
    Modi:   Cardinal;
    Winner: Byte;
  end;

  TeamOrderEntry = record
    Teamnum: Byte;
    Score: Byte;
  end;

  TeamOrderArray = Array[0..5] of Byte;

  TUS_ModiInfoEx = record
    Info:         TUS_ModiInfo;
    Owner:        Integer;
    TimesPlayed:  Byte; //Helper for setting Round Plugins
  end;

  TPartySession = class (TCoreModule)
  private
    bPartyMode: Boolean; //Is this Party or Singleplayer
    CurRound: Byte;

    Modis: Array of TUS_ModiInfoEx;
    Teams: TTeamInfo;

    function IsWinner(Player, Winner: Byte): boolean;
    procedure GenScores;
    function GetRandomPlugin(TeamMode: Boolean): Cardinal;
    function GetRandomPlayer(Team: Byte): Byte;
  public
    //Teams: TTeamInfo;
    Rounds: array of TRoundInfo;

    //TCoreModule methods to inherit
    Constructor Create; override;
    Procedure Info(const pInfo: PModuleInfo); override;
    Function Load: Boolean; override;
    Function Init: Boolean; override;
    Procedure DeInit; override;
    Destructor Destroy; override;

    //Register Modi Service
    Function RegisterModi(nothin: TwParam; pModiInfo: TlParam): integer; //Registers a new Modi. wParam: Pointer to TUS_ModiInfo

    //Start new Party
    Function StartParty(NumRounds: TwParam; PAofIRounds: TlParam): integer; //Starts new Party Mode. Returns Non Zero on Success
    Function GetCurModi(wParam: TwParam; lParam: TlParam): integer; //Returns Pointer to Cur. Modis TUS_ModiInfo (to Use with Singscreen)
    Function StopParty(wParam: TwParam; lParam: TlParam): integer; //Stops Party Mode. Returns 1 If Partymode was enabled before.
    Function NextRound(wParam: TwParam; lParam: TlParam): integer; //Increases CurRound by 1; Returns num of Round or -1 if last Round is already played

    Function CallModiInit(wParam: TwParam; lParam: TlParam): integer;    //Calls CurModis Init Proc. If an Error occurs, Returns Nonzero. In this Case a New Plugin was Selected. Please renew Loading
    Function CallModiDeInit(wParam: TwParam; lParam: TlParam): integer;  //Calls DeInitProc and does the RoundEnding

    Function GetTeamInfo(wParam: TwParam; pTeamInfo: TlParam): integer;    //Writes TTeamInfo Record to Pointer at lParam. Returns Zero on Success
    Function SetTeamInfo(wParam: TwParam; pTeamInfo: TlParam): integer;    //Read TTeamInfo Record from Pointer at lParam. Returns Zero on Success

    Function  GetTeamOrder(wParam: TwParam; lParam: TlParam): integer;     //Returns Team Order. Structure: Bits 1..3: Team at Place1; Bits 4..6: Team at Place2 ...
    Function  GetWinnerString(wParam: TwParam; lParam: TlParam): integer;  //wParam is Roundnum. If (Pointer = nil) then Return Length of the String. Otherwise Write the String to Address at lParam
  end;

const
  StandardModi = 0; //Modi ID that will be played in non party Mode

implementation

uses UCore, UGraphic, UMain, ULanguage, ULog, SysUtils;

{*********************
  TPluginLoader
  Implentation
*********************}

//-------------
// Function that gives some Infos about the Module to the Core
//-------------
Procedure TPartySession.Info(const pInfo: PModuleInfo);
begin
  pInfo^.Name := 'TPartySession';
  pInfo^.Version := MakeVersion(1,0,0,chr(0));
  pInfo^.Description := 'Manages Party Modi and Party Game';
end;

//-------------
// Just the Constructor
//-------------
Constructor TPartySession.Create;
begin
  inherited;
  //UnSet PartyMode
  bPartyMode := False;
end;

//-------------
//Is Called on Loading.
//In this Method only Events and Services should be created
//to offer them to other Modules or Plugins during the Init process
//If False is Returned this will cause a Forced Exit
//-------------
Function TPartySession.Load: Boolean;
begin
  //Add Register Party Modi Service
  Result := True;
  Core.Services.AddService('Party/RegisterModi', nil, Self.RegisterModi);
  Core.Services.AddService('Party/StartParty', nil, Self.StartParty);
  Core.Services.AddService('Party/GetCurModi', nil, Self.GetCurModi);
end;

//-------------
//Is Called on Init Process
//In this Method you can Hook some Events and Create + Init
//your Classes, Variables etc.
//If False is Returned this will cause a Forced Exit
//-------------
Function TPartySession.Init: Boolean;
begin
  //Just set Prvate Var to true.
  Result := true;
end;

//-------------
//Is Called if this Module has been Inited and there is a Exit.
//Deinit is in backwards Initing Order
//-------------
Procedure TPartySession.DeInit;
begin
  //Force DeInit

end;

//-------------
//Is Called if this Module will be unloaded and has been created
//Should be used to Free Memory
//-------------
Destructor TPartySession.Destroy;
begin
  //Just save some Memory if it wasn't done now..
  SetLength(Modis, 0);
  inherited;
end;

//-------------
// Registers a new Modi. wParam: Pointer to TUS_ModiInfo
// Service for Plugins
//-------------
Function TPartySession.RegisterModi(nothin: TwParam; pModiInfo: TlParam): integer;
var
  Len: Integer;
  Info: PUS_ModiInfo;
begin
  Info := PModiInfo;
  //Copy Info if cbSize is correct
  If (Info.cbSize = SizeOf(TUS_ModiInfo)) then
  begin
    Len := Length(Modis);
    SetLength(Modis, Len + 1);

    Modis[Len].Info := Info^;
  end
  else
    Core.ReportError(Integer(PChar('Plugins try to Register Modi with wrong Pointer, or wrong TUS_ModiInfo Record.')), PChar('TPartySession'));

  // FIXME: return a valid result
  Result := 0;
end;

//----------
// Returns a Number of a Random Plugin
//----------
Function TPartySession.GetRandomPlugin(TeamMode: Boolean): Cardinal;
var
  LowestTP: Byte;
  NumPwithLTP: Word;
  I: Integer;
  R: Word;
begin
  Result := StandardModi; //If there are no matching Modis, Play StandardModi
  LowestTP := high(Byte);
  NumPwithLTP := 0;

  //Search for Plugins not often played yet
  For I := 0 to high(Modis) do
  begin
    if (Modis[I].TimesPlayed < lowestTP) And (((Modis[I].Info.LoadingSettings AND MLS_TeamOnly) <> 0) = TeamMode) then
    begin
      lowestTP := Modis[I].TimesPlayed;
      NumPwithLTP := 1;
    end
    else if (Modis[I].TimesPlayed = lowestTP) And (((Modis[I].Info.LoadingSettings AND MLS_TeamOnly) <> 0) = TeamMode) then
    begin
      Inc(NumPwithLTP);
    end;
  end;

  //Create Random No
  R := Random(NumPwithLTP);

  //Search for Random Plugin
  For I := 0 to high(Modis) do
  begin
    if (Modis[I].TimesPlayed = lowestTP) And (((Modis[I].Info.LoadingSettings AND MLS_TeamOnly) <> 0) = TeamMode) then
    begin
      //Plugin Found
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
// Starts new Party Mode. Returns Non Zero on Success
//----------
Function TPartySession.StartParty(NumRounds: TwParam; PAofIRounds: TlParam): integer;
var
  I: Integer;
  aiRounds: PARounds;
  TeamMode: Boolean;
begin
  Result := 0;
  If (Teams.NumTeams >= 1) AND (NumRounds < High(Byte)-1) then
  begin
    bPartyMode := false;
    aiRounds := PAofIRounds;

    Try
      //Is this Teammode(More then one Player per Team) ?
      TeamMode := True;
      For I := 0 to Teams.NumTeams-1 do
        TeamMode := TeamMode AND (Teams.Teaminfo[I].NumPlayers > 1);

      //Set Rounds
      SetLength(Rounds, NumRounds);

      For I := 0 to High(Rounds) do
      begin //Set Plugins
        If (aiRounds[I] = -1) then
          Rounds[I].Modi := GetRandomPlugin(TeamMode)
        Else If (aiRounds[I] >= 0) AND (aiRounds[I] <= High(Modis)) AND (TeamMode OR ((Modis[aiRounds[I]].Info.LoadingSettings AND MLS_TeamOnly) = 0))  then
          Rounds[I].Modi := aiRounds[I]
        Else
          Rounds[I].Modi := StandardModi;

        Rounds[I].Winner := High(Byte); //Set Winner to Not Played
      end;

      CurRound := High(Byte); //Set CurRound to not defined

      //Return teh true and Set PartyMode
      bPartyMode := True;
      Result := 1;

    Except
      Core.ReportError(Integer(PChar('Can''t start PartyMode.')), PChar('TPartySession'));
    end;
  end;
end;

//----------
// Returns Pointer to Cur. ModiInfoEx (to Use with Singscreen)
//----------
Function TPartySession.GetCurModi(wParam: TwParam; lParam: TlParam): integer;
begin
  If (bPartyMode) AND (CurRound <= High(Rounds)) then
  begin //If PartyMode is enabled:
    //Return the Plugin of the Cur Round
    Result := Integer(@Modis[Rounds[CurRound].Modi]);
  end
  else
  begin //Return StandardModi
    Result := Integer(@Modis[StandardModi]);
  end;
end;

//----------
// Stops Party Mode. Returns 1 If Partymode was enabled before. And -1 if Change was not possible
//----------
Function TPartySession.StopParty(wParam: TwParam; lParam: TlParam): integer;
begin
  Result := -1;
  If (bPartyMode) then
  begin
    // to-do : Whitü: Check here if SingScreen is not Shown atm.
    bPartyMode := False;
    Result := 1;
  end
  else
    Result := 0;
end;

//----------
//GetRandomPlayer - Gives back a Random Player to Play next Round
//----------
function TPartySession.GetRandomPlayer(Team: Byte): Byte;
var
  I, R: Integer;
  lowestTP: Byte;
  NumPwithLTP: Byte;
begin
    LowestTP := high(Byte);
    NumPwithLTP := 0;
    Result := 0;

    //Search for Players that have not often played yet
    For I := 0 to Teams.Teaminfo[Team].NumPlayers-1 do
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

    //Create Random No
    R := Random(NumPwithLTP);

    //Search for Random Player
    For I := 0 to Teams.Teaminfo[Team].NumPlayers-1 do
    begin
      if Teams.Teaminfo[Team].Playerinfo[I].TimesPlayed = lowestTP then
      begin
        //Player Found
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
// NextRound - Increases CurRound by 1; Returns num of Round or -1 if last Round is already played
//----------
Function TPartySession.NextRound(wParam: TwParam; lParam: TlParam): integer;
var I: Integer;
begin
  If ((CurRound < high(Rounds)) OR (CurRound = high(CurRound))) then
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
//IsWinner - Returns True if the Players Bit is set in the Winner Byte
//----------
function TPartySession.IsWinner(Player, Winner: Byte): boolean;
var
  Bit: Byte;
begin
  Bit := 1 shl Player;

  Result := ((Winner AND Bit) = Bit);
end;

//----------
//GenScores - Inc Scores for Cur. Round
//----------
procedure TPartySession.GenScores;
var
  I: Byte;
begin
  for I := 0 to Teams.NumTeams-1 do
  begin
    if isWinner(I, Rounds[CurRound].Winner) then
      Inc(Teams.Teaminfo[I].Score);
  end;
end;

//----------
// CallModiInit - Calls CurModis Init Proc. If an Error occurs, Returns Nonzero. In this Case a New Plugin was Selected. Please renew Loading
//----------
Function TPartySession.CallModiInit(wParam: TwParam; lParam: TlParam): integer;
begin
  If (not bPartyMode) then
  begin //Set Rounds if not in PartyMode
    SetLength(Rounds, 1);
    Rounds[0].Modi := StandardModi;
    Rounds[0].Winner := High(Byte);
    CurRound := 0;
  end;

  Try
    //Core.
  Except
    on E : Exception do
    begin
      Core.ReportError(Integer(PChar('Error starting Modi: ' + Modis[Rounds[CurRound].Modi].Info.Name + ' ErrorStr: ' + E.Message)), PChar('TPartySession'));
      If (Rounds[CurRound].Modi = StandardModi) then
      begin
        Core.ReportError(Integer(PChar('Can''t start StandardModi, will exit now!')), PChar('TPartySession'));
        Halt;
      end
      Else //Select StandardModi
      begin
        Rounds[CurRound].Modi := StandardModi
      end;
    end;
  End;

  // FIXME: return a valid result
  Result := 0;
end;

//----------
// CallModiDeInit - Calls DeInitProc and does the RoundEnding
//----------
Function TPartySession.CallModiDeInit(wParam: TwParam; lParam: TlParam): integer;
var
  I: Integer;
  MaxScore: Word;
begin
  If (bPartyMode) then
  begin
    //Get Winner Byte!
    if (@Modis[Rounds[CurRound].Modi].Info.ModiDeInit <> nil) then //get Winners from Plugin
      Rounds[CurRound].Winner := Modis[Rounds[CurRound].Modi].Info.ModiDeInit(Modis[Rounds[CurRound].Modi].Info.ID)
    else
    begin //Create winners by Score :/
      Rounds[CurRound].Winner := 0;
      MaxScore := 0;
      for I := 0 to Teams.NumTeams-1 do
      begin
        // to-do : recode Percentage stuff
        //PlayerInfo.Playerinfo[I].Percentage := PlayerInfo.Playerinfo[I].Score div 9999;
        if (Player[I].ScoreTotalInt > MaxScore) then
        begin
          MaxScore := Player[I].ScoreTotalInt;
          Rounds[CurRound].Winner := 1 shl I;
        end
        else if (Player[I].ScoreTotalInt = MaxScore) AND (Player[I].ScoreTotalInt <> 0) then
        begin
          Rounds[CurRound].Winner := Rounds[CurRound].Winner or (1 shl I);
        end;
      end;


      //When nobody has Points -> Everybody loose
      if (MaxScore = 0) then
        Rounds[CurRound].Winner := 0;

    end;

    //Generate teh Scores
    GenScores;

    //Inc Players TimesPlayed
    If ((Modis[Rounds[CurRound-1].Modi].Info.LoadingSettings AND MLS_IncTP) = MLS_IncTP) then
    begin
      For I := 0 to Teams.NumTeams-1 do
        Inc(Teams.TeamInfo[I].Playerinfo[Teams.TeamInfo[I].CurPlayer].TimesPlayed);
    end;
  end
  else if (@Modis[Rounds[CurRound].Modi].Info.ModiDeInit <> nil) then
    Modis[Rounds[CurRound].Modi].Info.ModiDeInit(Modis[Rounds[CurRound].Modi].Info.ID);

  // FIXME: return a valid result
  Result := 0;
end;

//----------
// GetTeamInfo - Writes TTeamInfo Record to Pointer at lParam. Returns Zero on Success
//----------
Function TPartySession.GetTeamInfo(wParam: TwParam; pTeamInfo: TlParam): integer;
var Info: ^TTeamInfo;
begin
  Result := -1;
  Info := pTeamInfo;
  If (Info <> nil) then
  begin
    Try
      // to - do : Check Delphi memory management in this case
      //Not sure if i had to copy PChars to a new address or if delphi manages this o0
      Info^  := Teams;
      Result := 0;
    Except
      Result := -2;
    End;
  end;
end;

//----------
// SetTeamInfo - Read TTeamInfo Record from Pointer at lParam. Returns Zero on Success
//----------
Function TPartySession.SetTeamInfo(wParam: TwParam; pTeamInfo: TlParam): integer;
var
  TeamInfobackup: TTeamInfo;
  Info: ^TTeamInfo;
begin
  Result := -1;
  Info := pTeamInfo;
  If (Info <> nil) then
  begin
    Try
      TeamInfoBackup := Teams;
      // to - do : Check Delphi memory management in this case
      //Not sure if i had to copy PChars to a new address or if delphi manages this o0
      Teams := Info^;
      Result := 0;
    Except
      Teams := TeamInfoBackup;
      Result := -2;
    End;
  end;
end;

//----------
// GetTeamOrder - Returns Team Order. Structure: Bits 1..3: Team at Place1; Bits 4..6: Team at Place2 ...
//----------
Function  TPartySession.GetTeamOrder(wParam: TwParam; lParam: TlParam): integer;
var
  I, J: Integer;
  ATeams: array [0..5] of TeamOrderEntry;
  TempTeam: TeamOrderEntry;
begin
  // to-do : PartyMode: Write this in another way, so that teams with the same scire get the same Placing
  //Fill Team Array
  For I := 0 to Teams.NumTeams-1 do
  begin
    ATeams[I].Teamnum := I;
    ATeams[I].Score := Teams.Teaminfo[I].Score;
  end;

  //Sort Teams
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
  For I := 0 to Teams.NumTeams-1 do
    Result := Result or (ATeams[I].TeamNum Shl I*3);
end;

//----------
// GetWinnerString - wParam is Roundnum. If (Pointer = nil) then Return Length of the String. Otherwise Write the String to Address at lParam
//----------
Function  TPartySession.GetWinnerString(wParam: TwParam; lParam: TlParam): integer;
var
  Winners: Array of String;
  I: Integer;
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

  //Now Return what we have got
  If (lParam = nil) then
  begin //ReturnString Length
    Result := Length(ResultStr);
  end
  Else
  begin //Return String
    Try
      S := lParam;
      S^ := ResultStr;
      Result := 0;
    Except
      Result := -1;

    End;
  end;
end;

end.
