library Blind;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  ModiSDK in '..\SDK\ModiSDK.pas';

// give the plugin's info
procedure PluginInfo (var Info: TPluginInfo); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  Info.Name       := 'PLUGIN_BLIND_NAME';
  Info.Creator    := 'Whiteshark';
  Info.PluginDesc := 'PLUGIN_BLIND_DESC';

  // set to party modus plugin
  Info.Typ        := 8;

  Info.NumPlayers := 31;

  // options
  Info.LoadSong  := true;  // whether or not a song should be loaded
  // only when song is loaded:
  Info.ShowScore := true;  // whether or not the score should be shown
  Info.ShowNotes := false; // whether the note lines should be displayed
  Info.LoadVideo := true;  // should the video be loaded?
  Info.LoadBack  := true;  // should the background be loaded?

  Info.BGShowFull   := false;  // whether the background or the video should be shown in full size
  Info.BGShowFull_O := true;   // whether the background or the video should be shown in full size

  Info.ShowRateBar   := false; // whether the bar that shows how good the player was should be displayed
  Info.ShowRateBar_O := true;  // load from ini whether the bar should be displayed

  Info.EnLineBonus   := false; // whether line bonus should be enabled
  Info.EnLineBonus_O := true;  // load from ini whether line bonus should be enabled

  //  options even when song is not loaded
  Info.ShowBars     := false;  // whether the white bars on top and bottom should be drawn
  Info.TeamModeOnly := false;  // if true the plugin can only be played in team mode
  Info.GetSoundData := false;  // if true the rdata procedure is called when new sound data is available
  Info.Dummy        := false;  // should be set to false... for updating plugin interface
end;

// executed on game start. if true game begins, else failure
function Init (const TeamInfo:   TTeamInfo;
               var   Playerinfo: TPlayerinfo;
	       const Sentences:  TSentences;
	       const Methods:    TMethodRec)
	      : boolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  Result := true;
end;

// executed every time the screen is drawn. if false the game finishes
function Draw (var   Playerinfo:  TPlayerinfo; 
               const CurSentence: cardinal)
	      : boolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  Result := true;
end;

// is executed on finish, returns the player number of the winner
function Finish (var Playerinfo: TPlayerinfo): byte; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
var
  Index:    integer;
  MaxScore: word;
begin
  Result   := 0;
  MaxScore := 0;
  for Index := 0 to PlayerInfo.NumPlayers - 1 do
  begin
    PlayerInfo.Playerinfo[Index].Percentage := PlayerInfo.Playerinfo[Index].Score div 9999;
    if (PlayerInfo.Playerinfo[Index].Score > MaxScore) then
    begin
      MaxScore := PlayerInfo.Playerinfo[Index].Score;
      case Index of
        0: Result :=  1;
        1: Result :=  2;
        2: Result :=  4;
        3: Result :=  8;
        4: Result := 16;
        5: Result := 32;
      end;
    end
    else if (PlayerInfo.Playerinfo[Index].Score = MaxScore) and (PlayerInfo.Playerinfo[Index].Score <> 0) then
    begin
      case Index of
        0: Result := Result or 1;
        1: Result := Result or 2;
        2: Result := Result or 4;
        3: Result := Result or 8;
        4: Result := Result or 16;
        5: Result := Result or 32;
      end;
    end;
  end;
  // if everybody has 0 points nobody wins
  if (MaxScore = 0) then
    Result := 0;
end;

exports
  PluginInfo,
  Init,
  Draw,
  Finish;

begin

end.