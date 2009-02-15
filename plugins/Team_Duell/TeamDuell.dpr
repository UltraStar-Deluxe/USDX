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
 TeamPlayer: array of array of String;
 StartPoints: array of integer;
 CurSinger, NextSinger: array[0..2] of Integer;
 MethodRec: TMethodRec;
 SPT, PlayerSelected: array[0..2] of Integer;
 TtoNextChange, starttick, endtick, ChangeOnSentence : Cardinal;
 bps, RTtoNextChange: Double;
 firsttime, secondtime: boolean;


//Gave the Plugins Info
procedure PluginInfo (var Info: TPluginInfo); stdcall;
begin
  Info.Name    := 'PLUGIN_TEAMDUELL_NAME';

  Info.Creator    := 'jekatt';
  Info.PluginDesc := 'PLUGIN_TEAMDUELL_DESC';

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
  Info.ShowRateBar_O := false; //Load from Ini whether the Bar should be Displayed

  Info.EnLineBonus := False;  //Whether LineBonus Should be enabled
  Info.EnLineBonus_O := True; //Load from Ini whether LineBonus Should be enabled

  //Options even when song is Not loaded
  Info.ShowBars := False; //Whether the White Bars on Top and Bottom should be Drawn
  Info.TeamModeOnly := True;  //If True the Plugin can only be Played in Team Mode
  Info.GetSoundData := False;  //If True the RData Procedure is called when new SoundData is available
  Info.Dummy := False;         //Should be Set to False... for Updateing Plugin Interface
end;

//Executed on Game Start //If True Game begins, else Failure
function Init (const TeamInfo: TTeamInfo; var Playerinfo: TPlayerinfo; const Sentences: TSentences; const Methods: TMethodRec): boolean; stdcall;
var
I,J: Integer;
begin
  // Get beginning of sentences
 for I := 0 to Sentences.High do begin
    SetLength(Startpoints, I+1);
    Startpoints[I]:=Sentences.Sentence[I].Start;
  end;
  // Get Teams and Players
  for I := 0 to TeamInfo.NumTeams-1 do
    begin
      SetLength(TeamPlayer, I+1);
      for J := 0 to TeamInfo.Teaminfo[I].NumPlayers-1 do
        begin
          SetLength(TeamPlayer[I], J+1);
          TeamPlayer[I,J] := Copy(String(TeamInfo.Teaminfo[I].Playerinfo[J].Name),1,8);
          If (NOT(TeamPlayer[I,J] = (String(TeamInfo.Teaminfo[I].Playerinfo[J].Name)))) THEN TeamPlayer[I,J] := TeamPlayer[I,J]+'.';
          SPT[I]:=J+1;
        end;
        CurSinger[I] := TeamInfo.Teaminfo[I].CurPlayer;
        repeat
            NextSinger[I] := random(SPT[I]);
        until NOT(NextSinger[I] = CurSinger[I]) OR (SPT[I] = 1) ;
    end;
  ChangeOnSentence := 8;
  starttick := SDL_GetTicks();
  firsttime := true;
  secondtime := true;
  bps := 1;
  MethodRec := Methods;
  Result := True;
end;

//Executed everytime the Screen is Drawed //If False The Game finishes
function Draw (var Playerinfo: TPlayerinfo; const CurSentence: Cardinal): boolean; stdcall;
var
 I,timeline,x,y: Integer;
 display: PChar;
 start: boolean;
begin
  // TickCount(firstSentence) (not zero!)
  If (CurSentence = ChangeOnSentence - 7) AND (firsttime) then
  begin
    firsttime := false;
    starttick := SDL_GetTicks();
  end;
  start := false;
  // show first singers for 5sec
  if  (CurSentence < 1) AND ((starttick + 5000) > SDL_GetTicks()) then begin start := true; end;

  // TickCount(thirdSentence)
  If (CurSentence = 3) AND (secondtime) then
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
  for I := 0 to High(TeamPlayer) do begin
    if (CurSentence = ChangeOnSentence) AND NOT(PlayerSelected[I] = CurSentence) then begin
      PlayerSelected[I] := CurSentence;
      CurSinger[I] := NextSinger[I];
      repeat
        NextSinger[I] := random(SPT[I]);
      until NOT(NextSinger[I] = CurSinger[I]) OR (SPT[I] = 1) ;
    end;

  // display bg
  glColor4f (0.8, 0.8, 0.8, 1);
  display := PChar(TeamPlayer[I,CurSinger[I]]);
  if (TtoNextChange <= 11) OR (start = true) Then begin
   glEnable(GL_TEXTURE_2D);
   glDisable(GL_BLEND);
   glColor4f (0, 0, 0, 1);
   glBegin(GL_QUADS);
   glVertex2f(PlayerInfo.Playerinfo[I].PosX, PlayerInfo.Playerinfo[I].PosY+8);
   glVertex2f(PlayerInfo.Playerinfo[I].PosX, PlayerInfo.Playerinfo[I].PosY + 30);
   glVertex2f(PlayerInfo.Playerinfo[I].PosX + 100, PlayerInfo.Playerinfo[I].PosY + 30);
   glVertex2f(PlayerInfo.Playerinfo[I].PosX + 100, PlayerInfo.Playerinfo[I].PosY+8);
   glEnd;
   display := 'Next Singer';

   // timeline
   x:= 270; y:= 472;
  if (TtoNextChange <= 5) AND (RTtoNextChange > 0) then begin
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
   glColor3f (0.9, 0, 0); glVertex2f(x+3, y+3);
   glColor3f (0.8, 0.3, 0.3); glVertex2f(x+3, y+15);
   glColor3f (0.8, 0.3, 0.3); glVertex2f(x+3+timeline, y+15);
   glColor3f (0.9, 0, 0); glVertex2f(x+3+timeline, y+3);
   glEnd;
  end;
  glDisable(GL_TEXTURE_2D);
  end;

  // Names, Timer
  if (TtoNextChange <= 9) Then begin display := PChar(TeamPlayer[I,NextSinger[I]]);
    glColor4f (0.8, 0.1, 0.2, 1);
    MethodRec.Print (1, 18, PlayerInfo.Playerinfo[I].PosX+85, PlayerInfo.Playerinfo[I].PosY+10, CreateStr(PChar(IntToStr(Trunc(TtoNextChange)))));
  end;
  glColor4f (0.8, 0.8, 0.8, 1);
  if (CurSentence = 0) then display := PChar(TeamPlayer[I,CurSinger[I]]);
  if (TtoNextChange <= 11) OR (start) Then MethodRec.Print (1, 18, PlayerInfo.Playerinfo[I].PosX+5, PlayerInfo.Playerinfo[I].PosY+10, display);
  end;
  if (CurSentence = ChangeOnSentence) then  begin ChangeOnSentence := CurSentence + 7; firsttime := true; end;
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

  //When nobody has Points -> Everybody loose
  if (MaxScore = 0) then
    Result := 0;
end;

exports
PluginInfo, Init, Draw, Finish;

begin

end.


