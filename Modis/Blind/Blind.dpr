library Blind;

uses
  ModiSDK in '..\SDK\ModiSDK.pas';

//Gave the Plugins Info
procedure PluginInfo (var Info: TPluginInfo); stdcall;
begin
  Info.Name    := 'PLUGIN_BLIND_NAME';

  Info.Creator    := 'Whiteshark';
  Info.PluginDesc := 'PLUGIN_BLIND_DESC';

  //Set to Party Modi Plugin
  Info.Typ := 8;

  Info.NumPlayers := 31;

  //Options
  Info.LoadSong := True;  //Whether or not a Song should be Loaded
  //Only When Song is Loaded:
  Info.ShowScore := True; //Whether or not the Score should be shown
  Info.ShowNotes := False; //Whether the Note Lines should be displayed
  Info.LoadVideo := True; //Should the Video be loaded ?
  Info.LoadBack  := True; //Should the Background be loaded ?

  Info.BGShowFull := False;   //Whether the Background or the Video should be shown Fullsize
  Info.BGShowFull_O := True;  //Whether the Background or the Video should be shown Fullsize

  Info.ShowRateBar:= False;   //Whether the Bar that shows how good the player was sould be displayed
  Info.ShowRateBar_O := True; //Load from Ini whether the Bar should be Displayed

  Info.EnLineBonus := False;  //Whether LineBonus Should be enabled
  Info.EnLineBonus_O := True; //Load from Ini whether LineBonus Should be enabled

  //Options even when song is Not loaded
  Info.ShowBars := False; //Whether the White Bars on Top and Bottom should be Drawn
  Info.TeamModeOnly := False;  //If True the Plugin can only be Played in Team Mode
  Info.GetSoundData := False;  //If True the RData Procedure is called when new SoundData is available
  Info.Dummy := False;         //Should be Set to False... for Updateing Plugin Interface
end;

//Executed on Game Start //If True Game begins, else Failure
function Init (const TeamInfo: TTeamInfo; var Playerinfo: TPlayerinfo; const Sentences: TSentences; const Methods: TMethodRec): boolean; stdcall;
begin
Result := True;
end;

//Executed everytime the Screen is Drawed //If False The Game finishes
function Draw (var Playerinfo: TPlayerinfo; const CurSentence: Cardinal): boolean; stdcall;
var
I: Integer;
begin
Result := True;
end;

//Is Executed on Finish, Returns the Playernum of the Winner
function Finish (var Playerinfo: TPlayerinfo): byte; stdcall;
var
  I:Integer;
  MaxScore: Word;
begin
  Result := 0;
  MaxScore := 0;
  for I := 0 to PlayerInfo.NumPlayers-1 do
  begin
    PlayerInfo.Playerinfo[I].Percentage := PlayerInfo.Playerinfo[I].Score div 9999;
    if (PlayerInfo.Playerinfo[I].Score > MaxScore) then
    begin
      MaxScore := PlayerInfo.Playerinfo[I].Score;
      Case I of
        0: Result :=  1;
        1: Result :=  2;
        2: Result :=  4;
        3: Result :=  8;
        4: Result := 16;
        5: Result := 32;
      end;
    end
    else if (PlayerInfo.Playerinfo[I].Score = MaxScore) AND (PlayerInfo.Playerinfo[I].Score <> 0) then
    begin
      Case I of
        0: Result := Result OR 1;
        1: Result := Result OR 2;
        2: Result := Result OR 4;
        3: Result := Result OR 8;
        4: Result := Result OR 16;
        5: Result := Result OR 32;
      end;
    end;
  end;
  //If everybody has 0 Points nobody Wins
  If (MaxScore = 0) then
    Result := 0;
end;

exports
PluginInfo, Init, Draw, Finish;

begin

end.