library Until5000;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  ModiSDK in '..\SDK\ModiSDK.pas';

//Gave the Plugins Info
procedure PluginInfo (var Info: TPluginInfo); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  Info.Name    := 'PLUGIN_UNTIL5000_NAME';

  Info.Creator    := 'Whiteshark';
  Info.PluginDesc := 'PLUGIN_UNTIL5000_DESC';

  //Set to Party Modi Plugin
  Info.Typ := 8;

  Info.NumPlayers := 31;
  //Options
  Info.LoadSong := True;  //Whether or not a Song should be Loaded
  //Only When Song is Loaded:
  Info.ShowScore := True; //Whether or not the Score should be shown
  Info.ShowNotes := True; //Whether the Note Lines should be displayed
  Info.LoadVideo := True; //Should the Video be loaded ?
  Info.LoadBack  := True; //Should the Background be loaded ?

  Info.BGShowFull := False;   //Whether the Background or the Video should be shown Fullsize
  Info.BGShowFull_O := True;  //Whether the Background or the Video should be shown Fullsize

  Info.ShowRateBar:= True;   //Whether the Bar that shows how good the player was sould be displayed
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
function Init (const TeamInfo: TTeamInfo; var Playerinfo: TPlayerinfo; const Sentences: TSentences; const Methods: TMethodRec): boolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
Result := True;
end;

//Executed everytime the Screen is Drawed //If False The Game finishes
function Draw (var Playerinfo: TPlayerinfo; const CurSentence: Cardinal): boolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
var
I: Integer;
begin
Result := False;
  for I := 0 to PlayerInfo.NumPlayers-1 do
  begin
    PlayerInfo.Playerinfo[I].Bar := PlayerInfo.Playerinfo[I].Score div 50;
    PlayerInfo.Playerinfo[I].Percentage := PlayerInfo.Playerinfo[I].Bar;
    if (PlayerInfo.Playerinfo[I].Score >=5000) then
      Exit;
  end;
Result := True;
end;

//Is Executed on Finish, Returns the Playernum of the Winner
function Finish (var Playerinfo: TPlayerinfo): byte; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
var
  I:Integer;
begin
Result := 0;
for I := 0 to PlayerInfo.NumPlayers-1 do
  begin
    if (PlayerInfo.Playerinfo[I].Score >=5000) then
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
end;

exports
  PluginInfo,
  Init,
  Draw,
  Finish;

begin

end.