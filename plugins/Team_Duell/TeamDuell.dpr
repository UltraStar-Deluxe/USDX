library TeamDuell ;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  ModiSDK      in '..\SDK\ModiSDK.pas',
  StrUtils     in '..\SDK\StrUtils.pas',
  sdl          in '..\..\src\lib\JEDI-SDL\SDL\Pas\sdl.pas',
  moduleloader in '..\..\src\lib\JEDI-SDL\SDL\Pas\moduleloader.pas',
  gl           in '..\..\src\lib\JEDI-SDL\OpenGL\Pas\gl.pas',
  sysutils;

var
  TeamPlayer:            array of array of string;
  StartPoints:           array of integer;
  CurSinger, NextSinger: array[0..2] of integer;
  MethodRec:             TMethodRec;
  SPT, PlayerSelected:   array[0..2] of integer;
  TtoNextChange, starttick, endtick, ChangeOnSentence: cardinal;
  bps, RTtoNextChange:   double;
  firsttime, secondtime: boolean;


// Give the Plugin's Info
procedure PluginInfo (var Info: TPluginInfo); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  Info.Name       := 'PLUGIN_TEAMDUELL_NAME';

  Info.Creator    := 'jekatt';
  Info.PluginDesc := 'PLUGIN_TEAMDUELL_DESC';

  Info.Typ        := 8;

  Info.NumPlayers := 31;
  // Options
  Info.LoadSong   := true; // Whether or not a Song should be Loaded
  // Only When Song is Loaded:
  Info.ShowScore  := true; // Whether or not the Score should be shown
  Info.ShowNotes  := true; // Whether the Note Lines should be displayed
  Info.LoadVideo  := true; // Should the Video be loaded ?
  Info.LoadBack   := true; // Should the Background be loaded ?

  Info.BGShowFull    := false; // Whether the Background or the Video should be shown Full size
  Info.BGShowFull_O  := true;  // Whether the Background or the Video should be shown Full size

  Info.ShowRateBar   := true;  // Whether the Bar that shows how good the player was should be displayed
  Info.ShowRateBar_O := false; // Load from Ini whether the Bar should be Displayed

  Info.EnLineBonus   := false; // Whether Line Bonus Should be enabled
  Info.EnLineBonus_O := true;  // Load from Ini whether Line Bonus Should be enabled

  // Options even when song is Not loaded
  Info.ShowBars      := false; // Whether the White Bars on Top and Bottom should be Drawn
  Info.TeamModeOnly  := true;  // if true the Plugin can only be Played in Team Mode
  Info.GetSoundData  := false; // if true the RData Procedure is called when new SoundData is available
  Info.Dummy         := false; // Should be Set to false... for Updating Plugin Interface
end;

// Executed on Game Start; if true Game begins, else Failure
function Init (const TeamInfo:   TTeamInfo;
               var   Playerinfo: TPlayerinfo;
	       const Sentences:  TSentences;
	       const Methods:    TMethodRec)
	      : boolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
var
  Index, J: integer;
begin
  // Get beginning of sentences
  for Index := 0 to Sentences.High do
  begin
    SetLength(Startpoints, Index+1);
    Startpoints[Index]:=Sentences.Sentence[Index].Start;
  end;
  // Get Teams and Players
  for Index := 0 to TeamInfo.NumTeams-1 do
  begin
    SetLength(TeamPlayer, Index+1);
    for J := 0 to TeamInfo.Teaminfo[Index].NumPlayers-1 do
    begin
      SetLength(TeamPlayer[Index], J+1);
      TeamPlayer[Index,J] := Copy(string(TeamInfo.Teaminfo[Index].Playerinfo[J].Name), 1, 8);
      if (not(TeamPlayer[Index,J] = (string(TeamInfo.Teaminfo[Index].Playerinfo[J].Name)))) then
        TeamPlayer[Index,J] := TeamPlayer[Index,J]+'.';
      SPT[Index]:=J+1;
    end;
    CurSinger[Index] := TeamInfo.Teaminfo[Index].CurPlayer;
    repeat
      NextSinger[Index] := random(SPT[Index]);
    until not(NextSinger[Index] = CurSinger[Index]) or (SPT[Index] = 1);
  end;
  ChangeOnSentence := 8;
  starttick := SDL_GetTicks();
  firsttime := true;
  secondtime := true;
  bps := 1;
  MethodRec := Methods;
  Result := true;
end;

// Executed everytime the Screen is Drawed; if false The Game finishes
function Draw (var   Playerinfo:  TPlayerinfo;
               const CurSentence: cardinal)
	      : boolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
var
  Index, timeline, x, y: integer;
  display: PChar;
  start: boolean;
begin
  // TickCount(firstSentence) (not zero!)
  if (CurSentence = ChangeOnSentence - 7) and (firsttime) then
  begin
    firsttime := false;
    starttick := SDL_GetTicks();
  end;
  start := false;
  // show first singers for 5sec
  if  (CurSentence < 1) and ((starttick + 5000) > SDL_GetTicks()) then
    start := true;

  // TickCount(thirdSentence)
  if (CurSentence = 3) and (secondtime) then
  begin
    secondtime := false;
    firsttime := true;
    endtick := SDL_GetTicks();
    bps :=  (Startpoints[3]-Startpoints[1]) * 1000 / (endtick-starttick); // BeatsPerSecond
  end;

  // Time to next Change
  RTtoNextChange := ((Startpoints[ChangeOnSentence]-Startpoints[ChangeOnSentence - 7]) / bps) - ((SDL_GetTicks() - starttick) / 1000);
  TtoNextChange := Trunc(RTtoNextChange) +1;

  // Next Singer for Team I
  for Index := 0 to High(TeamPlayer) do
  begin
    if (CurSentence = ChangeOnSentence) and not(PlayerSelected[Index] = CurSentence) then
    begin
      PlayerSelected[Index] := CurSentence;
      CurSinger[Index] := NextSinger[Index];
      repeat
        NextSinger[Index] := random(SPT[Index]);
      until not(NextSinger[Index] = CurSinger[Index]) or (SPT[Index] = 1) ;
    end;

  // display bg
  glColor4f (0.8, 0.8, 0.8, 1);
  display := PChar(TeamPlayer[Index,CurSinger[Index]]);
  if (TtoNextChange <= 11) or (start = true) then
  begin
    glEnable(GL_TEXTURE_2D);
    glDisable(GL_BLEND);
    glColor4f (0, 0, 0, 1);
    glBegin(GL_QUADS);
    glVertex2f(PlayerInfo.Playerinfo[Index].PosX, PlayerInfo.Playerinfo[Index].PosY+8);
    glVertex2f(PlayerInfo.Playerinfo[Index].PosX, PlayerInfo.Playerinfo[Index].PosY + 30);
    glVertex2f(PlayerInfo.Playerinfo[Index].PosX + 100, PlayerInfo.Playerinfo[Index].PosY + 30);
    glVertex2f(PlayerInfo.Playerinfo[Index].PosX + 100, PlayerInfo.Playerinfo[Index].PosY+8);
    glEnd;
    display := 'Next Singer';

   // timeline
    x:= 270;
    y:= 472;
    if (TtoNextChange <= 5) and (RTtoNextChange > 0) then
    begin
      timeline := Trunc(RTtoNextChange*50);
      glColor3f (0, 0, 0);
      glBegin(GL_QUADS);
      glVertex2f(x, y);
      glVertex2f(x, y+18);
      glVertex2f(x+6+250, y+18);
      glVertex2f(x+6+250, y);
      glEnd;
      glColor3f (0.2, 0.2, 0.2);
      glBegin(GL_QUADS);
      glVertex2f(x+3, y+3);
      glVertex2f(x+3, y+15);
      glVertex2f(x+3+250, y+15);
      glVertex2f(x+3+250, y+3);
      glEnd;
      glColor3f (0.8, 0.2, 0.2);
      glBegin(GL_QUADS);
      glColor3f (0.9, 0, 0);     glVertex2f(x+3, y+3);
      glColor3f (0.8, 0.3, 0.3); glVertex2f(x+3, y+15);
      glColor3f (0.8, 0.3, 0.3); glVertex2f(x+3+timeline, y+15);
      glColor3f (0.9, 0, 0);     glVertex2f(x+3+timeline, y+3);
      glEnd;
    end;
    glDisable(GL_TEXTURE_2D);
  end;

  // Names, Timer
  if (TtoNextChange <= 9) then
  begin display := PChar(TeamPlayer[Index,NextSinger[Index]]);
    glColor4f (0.8, 0.1, 0.2, 1);
    MethodRec.Print (1, 18, PlayerInfo.Playerinfo[Index].PosX+85, PlayerInfo.Playerinfo[Index].PosY+10, CreateStr(PChar(IntToStr(Trunc(TtoNextChange)))));
  end;
  glColor4f (0.8, 0.8, 0.8, 1);
  if (CurSentence = 0) then
    display := PChar(TeamPlayer[Index,CurSinger[Index]]);
  if (TtoNextChange <= 11) or (start) then
    MethodRec.Print (1, 18, PlayerInfo.Playerinfo[Index].PosX+5, PlayerInfo.Playerinfo[Index].PosY+10, display);
  end;
  if (CurSentence = ChangeOnSentence) then
  begin
    ChangeOnSentence := CurSentence + 7;
    firsttime := true;
  end;
  Result := true;
end;

// Is Executed on Finish, Returns the Playernum of the Winner
function Finish (var Playerinfo: TPlayerinfo): byte; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
var
  Index: integer;
  MaxScore: word;
begin
  Result := 0;
  MaxScore := 0;
  for Index := 0 to PlayerInfo.NumPlayers-1 do
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
        0: Result := Result or  1;
        1: Result := Result or  2;
        2: Result := Result or  4;
        3: Result := Result or  8;
        4: Result := Result or 16;
        5: Result := Result or 32;
      end;
    end;
  end;

  // When nobody has Points -> Everybody loose
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


