unit UParty;

interface

uses ModiSDK;

type
  TRoundInfo = record
    Plugin: Word;
    Winner: Byte;
  end;

  TParty_Session = class
  private
    function GetRandomPlayer(Team: Byte): Byte;
    function IsWinner(Player, Winner: Byte): boolean;
    procedure GenScores;
  public
    Teams: TTeamInfo;
    Rounds: array of TRoundInfo;
    CurRound: Byte;

    constructor Create;

    procedure StartNewParty(NumRounds: Byte);
    procedure StartRound;
    procedure EndRound;
    function  GetWinner: Byte;
    function  GetWinnerString(Round: Byte): String;
  end;

var
  PartySession: TParty_Session;

implementation

uses UDLLManager, UGraphic, UMain, ULanguage, ULog;

//----------
//Constructor -  Prepares the Class
//----------
constructor TParty_Session.Create;
begin
// - Nothing in here atm
end;

//----------
//StartNewParty - Clears the Class and Prepares for new Party
//----------
procedure TParty_Session.StartNewParty(NumRounds: Byte);
var
  Plugins: Array of record
    ID: Byte;
    TimesPlayed: Byte;
  end;
  TeamMode: Boolean;
  Len:  Integer;
  I:  Integer;

  function GetRandomPlugin: Byte;
  var
    LowestTP: Byte;
    NumPwithLTP: Word;
    I: Integer;
    R: Word;
  begin
    LowestTP := high(Byte);
    NumPwithLTP := 0;

    //Search for Plugins not often played yet
    For I := 0 to high(Plugins) do
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

    //Create Random No
    R := Random(NumPwithLTP);

    //Search for Random Plugin
    For I := 0 to high(Plugins) do
    begin
      if Plugins[I].TimesPlayed = lowestTP then
      begin
        //Plugin Found
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
begin
  //Set cur Round to Round 1
  CurRound := 255;

  PlayersPlay := Teams.NumTeams;

  TeamMode := True;
  For I := 0 to Teams.NumTeams-1 do
    if Teams.Teaminfo[I].NumPlayers < 2 then
    begin
      TeamMode := False;
      Break;
    end;

  //Fill Plugin Array
  SetLength(Plugins, 0);
  For I := 0 to high(DLLMan.Plugins) do
  begin
    if TeamMode or (Not DLLMan.Plugins[I].TeamModeOnly)  then
    begin //Add only Plugins Playable with cur. PlayerConfiguration
      Len := Length(Plugins);
      SetLength(Plugins, Len + 1);
      Plugins[Len].ID := I;
      Plugins[Len].TimesPlayed := 0;
    end;
  end;

  //Set Rounds
  If (Length(Plugins) >= 1) then
  begin
    SetLength (Rounds, NumRounds);
    For I := 0 to NumRounds-1 do
    begin
      PartySession.Rounds[I].Plugin := GetRandomPlugin;
      PartySession.Rounds[I].Winner := 255;
    end;
  end
  else SetLength (Rounds, 0);
end;

//----------
//GetRandomPlayer - Gives back a Random Player to Play next Round
//----------
function TParty_Session.GetRandomPlayer(Team: Byte): Byte;
var
  I, J: Integer;
  lowestTP: Byte;
begin
  //Get lowest TimesPlayed
  lowestTP := high(Byte);
  J := -1;
  for I := 0 to Teams.Teaminfo[Team].NumPlayers-1 do
  begin
    if (Teams.Teaminfo[Team].Playerinfo[I].TimesPlayed < lowestTP) then
    begin
      lowestTP := Teams.Teaminfo[Team].Playerinfo[I].TimesPlayed;
      J := I;
    end
    else if (Teams.Teaminfo[Team].Playerinfo[I].TimesPlayed = lowestTP) then
    begin
      J := -1;
    end;
  end;

  //If more than one Person has lowestTP then Select Random Player
  if (J < 0) then
    repeat
      Result := Random(Teams.Teaminfo[Team].NumPlayers);
    until (Teams.Teaminfo[Team].Playerinfo[Result].TimesPlayed = lowestTP)
  else //Else Select the one with lowest TP
    Result:= J;
end;

//----------
//StartNextRound - Prepares ScreenSingModi for Next Round And Load Plugin
//----------
procedure TParty_Session.StartRound;
var
  I: Integer;
begin
  if ((CurRound < high(Rounds)) OR (CurRound = high(CurRound))) then
  begin
    //Increase Current Round
    Inc (CurRound);

    Rounds[CurRound].Winner := 0;
    DllMan.LoadPlugin(Rounds[CurRound].Plugin);

    //Select Players
    for I := 0 to Teams.NumTeams-1 do
      Teams.Teaminfo[I].CurPlayer := GetRandomPlayer(I);

    //Set ScreenSingModie Variables
    ScreenSingModi.TeamInfo := Teams;

    //Set 
  end;
end;

//----------
//IsWinner - Returns True if the Players Bit is set in the Winner Byte
//----------
function TParty_Session.IsWinner(Player, Winner: Byte): boolean;
var
  Bit: Byte;
begin
  Case Player of
    0: Bit := 1;
    1: Bit := 2;
    2: Bit := 4;
    3: Bit := 8;
    4: Bit := 16;
    5: Bit := 32;
  end;

  Result := ((Winner AND Bit) = Bit);
end;

//----------
//GenScores - Inc Scores for Cur. Round
//----------
procedure TParty_Session.GenScores;
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
//GetWinnerString - Get String with WinnerTeam Name, when there is more than one Winner than Connect with and or ,
//----------
function  TParty_Session.GetWinnerString(Round: Byte): String;
var
  Winners: Array of String;
  I: Integer;
begin
  if (Rounds[Round].Winner = 0) then
  begin
    Result := Language.Translate('PARTY_NOBODY');
    exit;
  end;

  if (Rounds[Round].Winner = 255) then
  begin
    Result := Language.Translate('PARTY_NOTPLAYEDYET');
    exit;
  end;

  SetLength(Winners, 0);
  for I := 0 to Teams.NumTeams-1 do
  begin
    if isWinner(I, Rounds[Round].Winner) then
    begin
      SetLength(Winners, Length(Winners) + 1);
      Winners[high(Winners)] := Teams.TeamInfo[I].Name;
    end;
  end;
  Result := Language.Implode(Winners);
end;

//----------
//EndRound - Get Winner from ScreenSingModi and Save Data to RoundArray
//----------
procedure TParty_Session.EndRound;
var
  I: Integer;
begin
  //Copy Winner
  Rounds[CurRound].Winner := ScreenSingModi.Winner;
  //Set Scores
  GenScores;

  //Increase TimesPlayed 4 all Players
  For I := 0 to Teams.NumTeams-1 do
    Inc(Teams.Teaminfo[I].Playerinfo[Teams.Teaminfo[0].CurPlayer].TimesPlayed);

end;

//----------
//Get Winner - Gives back the Number of the total Winner
//----------
function TParty_Session.GetWinner: Byte;
begin

end;

end.
