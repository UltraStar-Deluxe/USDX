library Until5000;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  ModiSDK in '..\SDK\ModiSDK.pas';

// give the plugin's info
procedure PluginInfo (var Info: TPluginInfo); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  Info.Name       := 'PLUGIN_UNTIL5000_NAME';

  Info.Creator    := 'Whiteshark';
  Info.PluginDesc := 'PLUGIN_UNTIL5000_DESC';

  // set to party modus plugin
  Info.Typ        := 8;

  Info.NumPlayers := 31;
  // options
  Info.LoadSong   := true; // whether or not a song should be loaded
  // only when song is loaded:
  Info.ShowScore  := true; // whether or not the score should be shown
  Info.ShowNotes  := true; // whether the note lines should be displayed
  Info.LoadVideo  := true; // should the video be loaded?
  Info.LoadBack   := true; // should the background be loaded?

  Info.BGShowFull    := false; // whether the background or the video should be shown full size
  Info.BGShowFull_O  := true;  // whether the background or the video should be shown full size

  Info.ShowRateBar   := true;  // whether the bar that shows how good the player was should be displayed
  Info.ShowRateBar_O := true;  // load from ini whether the bar should be displayed

  Info.EnLineBonus   := false; // whether line bonus should be enabled
  Info.EnLineBonus_O := true;  // load from ini whether line bonus should be enabled

  // options even when song is not loaded
  Info.ShowBars      := false; // whether the white bars on top and bottom should be drawn
  Info.TeamModeOnly  := false; // if true the plugin can only be played in team mode
  Info.GetSoundData  := false; // if true the rdata procedure is called when new sound data is available
  Info.Dummy         := false; // should be set to false... for updateing plugin interface
end;

// executed on game start; if true game begins, else failure
function Init (const TeamInfo:   TTeamInfo;
               var   Playerinfo: TPlayerinfo;
	       const Sentences:  TSentences;
	       const Methods:    TMethodRec)
	      : boolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  Result := true;
end;

// executed everytime the screen is drawn; if false the game finishes
function Draw (var   Playerinfo:  TPlayerinfo; 
               const CurSentence: cardinal)
	      : boolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
var
  Index: integer;
begin
  Result := false;
  for Index := 0 to PlayerInfo.NumPlayers-1 do
  begin
    PlayerInfo.Playerinfo[Index].Bar := PlayerInfo.Playerinfo[Index].Score div 50;
    PlayerInfo.Playerinfo[Index].Percentage := PlayerInfo.Playerinfo[Index].Bar;
    if (PlayerInfo.Playerinfo[Index].Score >= 5000) then
      Exit;
  end;
  Result := true;
end;

// is executed on finish, returns the player number of the winner
function Finish (var Playerinfo: TPlayerinfo): byte; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
var
  Index: integer;
begin
  Result := 0;
  for Index := 0 to PlayerInfo.NumPlayers-1 do
  begin
    if (PlayerInfo.Playerinfo[Index].Score >= 5000) then
    begin
      case Index of
        0: Result := Result or  1;
        1: Result := Result or  2;
        2: Result := Result or  4;
        3: Result := Result or  8;
        4: Result := Result or 16;
        5: Result := Result or 32;
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