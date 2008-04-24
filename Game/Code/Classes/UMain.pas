unit UMain;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SDL,
  UGraphic,
  UMusic,
  URecord,
  UTime,
  SysUtils,
  UDisplay,
  UIni,
  ULog,
  ULyrics,
  UScreenSing,
  USong,
  OpenGL12,
  UThemes;

type
  TPlayer = record
    Name:         string;

    // Index in Teaminfo record
    TeamID:       Byte;
    PlayerID:     Byte;

    // Scores
    Score:        real;
    ScoreLine:    real;
    ScoreGolden:  real;

    ScoreI:       integer;
    ScoreLineI:   integer;
    ScoreGoldenI: integer;
    ScoreTotalI:  integer;

    // LineBonus Mod
    ScoreLast:    Real;//Last Line Score

    // PerfectLineTwinkle Mod (effect)
    LastSentencePerfect: Boolean;

    //Meter:        real;

    HighNote:  integer;
    IlNut:     integer;
    Note:     array of record
      Start:     integer;
      Length:    integer;
      Detekt:    real;     // accurate place, detected in the note
      Tone:      real;
      Perfect:   boolean;  // true if the note matches the original one, lit the star

      // Half size Notes Patch
      Hit:        boolean; // true if the note Hits the Line
    end;
  end;


var
  // Absolute Paths
  GamePath:         string;
  SoundPath:        string;
  SongPath:         string;
  LogPath:          string;
  ThemePath:        string;
  SkinsPath:        string;
  ScreenshotsPath:  string;
  CoversPath:       string;
  LanguagesPath:    string;
  PluginPath:       string;
  VisualsPath:      string;
  ResourcesPath:    string;
  PlayListPath:     string;

  UserSongPath:     string = '';
  UserCoversPath:   string = '';
  UserPlaylistPath: string = '';

  OGL:      Boolean;
  Done:     Boolean;
  Event:    TSDL_event;
  FileName: string;
  Restart:  boolean;

  // player and music info
  Player:       array of TPlayer;
  PlayersPlay:  integer;

  CurrentSong : TSong;

procedure InitializePaths;

Procedure Main;
procedure MainLoop;
procedure CheckEvents;
procedure Sing(Sender: TScreenSing);
procedure NewSentence(Sender: TScreenSing);
procedure NewBeat(Sender: TScreenSing); // executed when on then new beat
procedure NewBeatC(Sender: TScreenSing); // executed when on then new beat for click
procedure NewBeatD(Sender: TScreenSing); // executed when on then new beat for detection
//procedure NewHalf; // executed when in the half between beats
procedure NewNote(Sender: TScreenSing); // detect note
function  GetMidBeat(Time: real): real;
function  GetTimeFromBeat(Beat: integer): real;
procedure ClearScores(PlayerNum: integer);

implementation

uses
  USongs,
  UJoystick,
  math,
  UCommandLine,
  ULanguage,
  SDL_ttf,
  USkins,
  UCovers,
  UCatCovers,
  UDataBase,
  UPlaylist,
  UDLLManager,
  UParty,
  UConfig,
  UCore,
  UCommon,
  UGraphicClasses,
  UPluginDefs,
  UPlatform;




procedure Main;
var
  WndTitle: string;
begin
  try
    WndTitle := USDXVersionStr;

    if Platform.TerminateIfAlreadyRunning( {var} WndTitle) then
      Exit;
      
    // fix floating-point exceptions (FPE)
    DisableFloatingPointExceptions();
    // fix the locale for string-to-float parsing in C-libs
    SetDefaultNumericLocale();

    //------------------------------
    //StartUp - Create Classes and Load Files
    //------------------------------
    USTime := TTime.Create;

    // Commandline Parameter Parser
    Params := TCMDParams.Create;

    // Log + Benchmark
    Log := TLog.Create;
    Log.Title := WndTitle;
    Log.FileOutputEnabled := not Params.NoLog;
    Log.BenchmarkStart(0);

    // Language
    Log.BenchmarkStart(1);
    Log.LogStatus('Initialize Paths', 'Initialization');
    InitializePaths;
    Log.LogStatus('Load Language', 'Initialization');
    Language := TLanguage.Create;
    
    // Add Const Values:
    Language.AddConst('US_VERSION', USDXVersionStr);
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Loading Language', 1);

    // SDL
    Log.BenchmarkStart(1);
    Log.LogStatus('Initialize SDL', 'Initialization');
    SDL_Init(SDL_INIT_VIDEO or SDL_INIT_TIMER);
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Initializing SDL', 1);

    // SDL_ttf
    Log.BenchmarkStart(1);
    Log.LogStatus('Initialize SDL_ttf', 'Initialization');
    TTF_Init();
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Initializing SDL_ttf', 1);

    // Skin
    Log.BenchmarkStart(1);
    Log.LogStatus('Loading Skin List', 'Initialization');
    Skin := TSkin.Create;
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Loading Skin List', 1);

    // Ini + Paths
    Log.BenchmarkStart(1);
    Log.LogStatus('Load Ini', 'Initialization');
    Ini := TIni.Create;
    Ini.Load;

    //Load Languagefile
    if (Params.Language <> -1) then
      Language.ChangeLanguage(ILanguage[Params.Language])
    else
      Language.ChangeLanguage(ILanguage[Ini.Language]);

    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Loading Ini', 1);

    // Sound
    Log.BenchmarkStart(1);
    Log.LogStatus('Initialize Sound', 'Initialization');
    InitializeSound();
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Initializing Sound', 1);

    // Load Sound Settings from Ini
    Log.BenchmarkStart(1);
    Log.LogStatus('Load Sound Settings', 'Initialization');
    Ini.LoadSoundSettings;
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Load Sound Settings', 1);

    // Theme
    Log.BenchmarkStart(1);
    Log.LogStatus('Load Themes', 'Initialization');
    Theme := TTheme.Create(ThemePath + ITheme[Ini.Theme] + '.ini', Ini.Color);
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Loading Themes', 1);

    // Covers Cache
    Log.BenchmarkStart(1);
    Log.LogStatus('Creating Covers Cache', 'Initialization');
    Covers := TCovers.Create;
    Log.LogBenchmark('Loading Covers Cache Array', 1);
    Log.BenchmarkStart(1);

    // Category Covers
    Log.BenchmarkStart(1);
    Log.LogStatus('Creating Category Covers Array', 'Initialization');
    CatCovers:= TCatCovers.Create;
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Loading Category Covers Array', 1);

    // Songs
    //Log.BenchmarkStart(1);
    Log.LogStatus('Creating Song Array', 'Initialization');
    Songs := TSongs.Create;
    //Songs.LoadSongList;

    Log.LogStatus('Creating 2nd Song Array', 'Initialization');
    CatSongs := TCatSongs.Create;

    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Loading Songs', 1);

    // PluginManager
    Log.BenchmarkStart(1);
    Log.LogStatus('PluginManager', 'Initialization');
    DLLMan := TDLLMan.Create;   // Load PluginList
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Loading PluginManager', 1);

    {// Party Mode Manager
    Log.BenchmarkStart(1);
    Log.LogStatus('PartySession Manager', 'Initialization');
    PartySession := TPartySession.Create;   //Load PartySession

    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Loading PartySession Manager', 1);      }

    // Graphics
    Log.BenchmarkStart(1);
    Log.LogStatus('Initialize 3D', 'Initialization');
    Initialize3D(WndTitle);
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Initializing 3D', 1);

    // Score Saving System
    Log.BenchmarkStart(1);
    Log.LogStatus('DataBase System', 'Initialization');
    DataBase := TDataBaseSystem.Create;

    if (Params.ScoreFile = '') then
      DataBase.Init ('Ultrastar.db')
    else
      DataBase.Init (Params.ScoreFile);

    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Loading DataBase System', 1);

    // Playlist Manager
    Log.BenchmarkStart(1);
    Log.LogStatus('Playlist Manager', 'Initialization');
    PlaylistMan := TPlaylistManager.Create;
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Loading Playlist Manager', 1);

    // GoldenStarsTwinkleMod
    Log.BenchmarkStart(1);
    Log.LogStatus('Effect Manager', 'Initialization');
    GoldenRec := TEffectManager.Create;
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Loading Particel System', 1);

    // Joypad
    if (Ini.Joypad = 1) OR (Params.Joypad) then
    begin
      Log.BenchmarkStart(1);
      Log.LogStatus('Initialize Joystick', 'Initialization');
      Joy := TJoy.Create;
      Log.BenchmarkEnd(1);
      Log.LogBenchmark('Initializing Joystick', 1);
    end;

    Log.BenchmarkEnd(0);
    Log.LogBenchmark('Loading Time', 0);

    Log.LogError('Creating Core');
    {Core := TCore.Create(
      USDXShortVersionStr,
      MakeVersion(USDX_VERSION_MAJOR,
                  USDX_VERSION_MINOR,
                  USDX_VERSION_RELEASE,
                  chr(0))
    );  }

    Log.LogError('Running Core');
    //Core.Run;

    //------------------------------
    //Start- Mainloop
    //------------------------------
    Log.LogStatus('Main Loop', 'Initialization');
    MainLoop;

  finally
    //------------------------------
    //Finish Application
    //------------------------------

    // TODO:
    // call an uninitialize routine for every initialize step
    // or at least use the corresponding Free-Methods

    UnloadOpenGL;
    //TTF_quit();
    SDL_Quit();

    (*
    {$ifdef WIN32}
      if assigned(LCD) and (Ini.LPT = 1) then
        LCD.Clear;
      if assigned(Light) and (Ini.LPT = 2) then
        Light.TurnOff;
    {$endif}
     *)

    if assigned(Log) then
    begin
      Log.LogStatus('Main Loop', 'Finished');
      Log.Free;
    end;
  end;
end;

procedure MainLoop;
var
  Delay:    integer;
begin
  Delay := 0;
  SDL_EnableKeyRepeat(125, 125);

  CountSkipTime();  // JB - for some reason this seems to be needed when we use the SDL Timer functions.
  while not Done do
  begin
    // joypad
    if (Ini.Joypad = 1) or (Params.Joypad) then
      Joy.Update;

    // keyboard events
    CheckEvents;

    // display
    done := not Display.Draw;
    SwapBuffers;

    // light
    //Light.Refresh;

    // delay
    CountMidTime;

    Delay := Floor(1000 / 100 - 1000 * TimeMid);

    if Delay >= 1 then
      SDL_Delay(Delay); // dynamic, maximum is 100 fps

    CountSkipTime;

    // reinitialization of graphics
    if Restart then
    begin
      Reinitialize3D;
      Restart := false;
    end;

  end;
End;

procedure CheckEvents;
begin
  if Assigned(Display.NextScreen) then
    Exit;
    
  while SDL_PollEvent( @event ) = 1 do
  begin
    case Event.type_ of
      SDL_QUITEV:
      begin
        Display.Fade := 0;
        Display.NextScreenWithCheck := nil;
        Display.CheckOK := True;
      end;
      {
      SDL_MOUSEBUTTONDOWN:
        with Event.button Do
        begin
          if State = SDL_BUTTON_LEFT Then
          begin
            //
          end;
        end;
      }
      SDL_VIDEORESIZE:
      begin
        ScreenW := Event.resize.w;
        ScreenH := Event.resize.h;

        screen  := SDL_SetVideoMode(ScreenW, ScreenH, (Ini.Depth+1) * 16, SDL_OPENGL or SDL_RESIZABLE);
      end;
      SDL_KEYDOWN:
        begin
          // remap the "keypad enter" key to the "standard enter" key
          if (Event.key.keysym.sym = SDLK_KP_ENTER) then
            Event.key.keysym.sym := SDLK_RETURN;

          if (Event.key.keysym.sym = SDLK_F11) or
             ((Event.key.keysym.sym = SDLK_RETURN) and
              ((Event.key.keysym.modifier and KMOD_ALT) <> 0)) then // toggle full screen
          begin
            Ini.FullScreen := integer( not boolean( Ini.FullScreen ) );

            if boolean( Ini.FullScreen ) then
            begin
              SDL_SetVideoMode(ScreenW, ScreenH, (Ini.Depth+1) * 16, SDL_OPENGL or SDL_FULLSCREEN);
              SDL_ShowCursor(0);
            end
            else
            begin
              SDL_SetVideoMode(ScreenW, ScreenH, (Ini.Depth+1) * 16, SDL_OPENGL or SDL_RESIZABLE);
              SDL_ShowCursor(1);
            end;

            glViewPort(0, 0, ScreenW, ScreenH);
          end
          // ScreenShot hack. If Print is pressed-> Make screenshot and Save to Screenshots Path
          else if (Event.key.keysym.sym = SDLK_SYSREQ) or (Event.key.keysym.sym = SDLK_PRINT) then
            Display.SaveScreenShot
          // popup hack... if there is a visible popup then let it handle input instead of underlying screen
          // shoud be done in a way to be sure the topmost popup has preference (maybe error, then check)
          else if (ScreenPopupError <> nil) and (ScreenPopupError.Visible) then
            done := not ScreenPopupError.ParseInput(Event.key.keysym.sym, WideChar(Event.key.keysym.unicode), True)
          else if (ScreenPopupCheck <> nil) and (ScreenPopupCheck.Visible) then
            done := not ScreenPopupCheck.ParseInput(Event.key.keysym.sym, WideChar(Event.key.keysym.unicode), True)
          // end of popup hack
          else
          begin
            // check for Screen want to Exit
            done := not Display.CurrentScreen^.ParseInput(Event.key.keysym.sym, WideChar(Event.key.keysym.unicode), True);

            // If Screen wants to Exit
            if done then
            begin
              // If Question Option is enabled then Show Exit Popup
              if (Ini.AskbeforeDel = 1) then
              begin
                Display.CurrentScreen^.CheckFadeTo(nil,'MSG_QUIT_USDX');
              end
              else // When asking for exit is disabled then simply exit
              begin
                Display.Fade := 0;
                Display.NextScreenWithCheck := nil;
                Display.CheckOK := True;
              end;
            end;

          end;
        end;
      {
      SDL_JOYAXISMOTION:
        begin
          // not implemented
        end;
      }
      SDL_JOYBUTTONDOWN:
        begin
          // not implemented
        end;
    end; // Case
  end; // While
end;

function GetTimeForBeats(BPM, Beats: real): real;
begin
  Result := 60 / BPM * Beats;
end;

function GetBeats(BPM, msTime: real): real;
begin
  Result := BPM * msTime / 60;
end;

procedure GetMidBeatSub(BPMNum: integer; var Time: real; var CurBeat: real);
var
  NewTime:  real;
begin
  if High(CurrentSong.BPM) = BPMNum then
  begin
    // last BPM
    CurBeat := CurrentSong.BPM[BPMNum].StartBeat + GetBeats(CurrentSong.BPM[BPMNum].BPM, Time);
    Time := 0;
  end
  else
  begin
    // not last BPM
    // count how much time is it for start of the new BPM and store it in NewTime
    NewTime := GetTimeForBeats(CurrentSong.BPM[BPMNum].BPM, CurrentSong.BPM[BPMNum+1].StartBeat - CurrentSong.BPM[BPMNum].StartBeat);

    // compare it to remaining time
    if (Time - NewTime) > 0 then
    begin
      // there is still remaining time
      CurBeat := CurrentSong.BPM[BPMNum].StartBeat;
      Time := Time - NewTime;
    end
    else
    begin
      // there is no remaining time
      CurBeat := CurrentSong.BPM[BPMNum].StartBeat + GetBeats(CurrentSong.BPM[BPMNum].BPM, Time);
      Time := 0;
    end; // if
  end; // if
end;

function GetMidBeat(Time: real): real;
var
  CurBeat:  real;
  CurBPM:   integer;
//  TopBeat:  real;
//  TempBeat: real;
//  TempTime: real;
begin
  Result := 0;
  if Length(CurrentSong.BPM) = 1 then
    Result := Time * CurrentSong.BPM[0].BPM / 60;

  (* 2 BPMs *)
{  if Length(CurrentSong.BPM) > 1 then begin
    (* new system *)
    CurBeat := 0;
    TopBeat := GetBeats(CurrentSong.BPM[0].BPM, Time);
    if TopBeat > CurrentSong.BPM[1].StartBeat then begin
      // analyze second BPM
      Time := Time - GetTimeForBeats(CurrentSong.BPM[0].BPM, CurrentSong.BPM[1].StartBeat - CurBeat);
      CurBeat := CurrentSong.BPM[1].StartBeat;
      TopBeat := GetBeats(CurrentSong.BPM[1].BPM, Time);
      Result := CurBeat + TopBeat;

    end
    else
    begin
      (* pierwszy przedzial *)
      Result := TopBeat;
    end;
  end;}

  (* more BPMs *)
  if Length(CurrentSong.BPM) > 1 then
  begin
    CurBeat := 0;
    CurBPM := 0;
    while (Time > 0) do
    begin
      GetMidBeatSub(CurBPM, Time, CurBeat);
      Inc(CurBPM);
    end;

    Result := CurBeat;
  end;
end;

function GetTimeFromBeat(Beat: integer): real;
var
  CurBPM:   integer;
begin
  Result := 0;
  if Length(CurrentSong.BPM) = 1 then
    Result := CurrentSong.GAP / 1000 + Beat * 60 / CurrentSong.BPM[0].BPM;

  (* more BPMs *)
  if Length(CurrentSong.BPM) > 1 then
  begin
    Result := CurrentSong.GAP / 1000;
    CurBPM := 0;
    while (CurBPM <= High(CurrentSong.BPM)) and
          (Beat > CurrentSong.BPM[CurBPM].StartBeat) do
    begin
      if (CurBPM < High(CurrentSong.BPM)) and
         (Beat >= CurrentSong.BPM[CurBPM+1].StartBeat) then
      begin
        // full range
        Result := Result + (60 / CurrentSong.BPM[CurBPM].BPM) *
                           (CurrentSong.BPM[CurBPM+1].StartBeat - CurrentSong.BPM[CurBPM].StartBeat);
      end;

      if (CurBPM = High(CurrentSong.BPM)) or
         (Beat < CurrentSong.BPM[CurBPM+1].StartBeat) then
      begin
        // in the middle
        Result := Result + (60 / CurrentSong.BPM[CurBPM].BPM) *
                           (Beat - CurrentSong.BPM[CurBPM].StartBeat);
      end;
      Inc(CurBPM);
    end;

    {
    while (Time > 0) do
    begin
      GetMidBeatSub(CurBPM, Time, CurBeat);
      Inc(CurBPM);
    end;
    }
  end; // if}
end;

procedure Sing(Sender: TScreenSing);
var
  Count:    integer;
  CountGr:  integer;
  CP:       integer;
  Done:     real;
  N:        integer;
begin
  LineState.CurrentTime := LineState.CurrentTime + TimeSkip;

  LineState.OldBeat := LineState.CurrentBeat;
  LineState.MidBeat := GetMidBeat(LineState.CurrentTime - (CurrentSong.Gap{ + 90 I've forgotten for what it is}) / 1000); // new system with variable BPM in function
  LineState.CurrentBeat := Floor(LineState.MidBeat);

//  LineState.OldHalf := LineState.AktHalf;
//  LineState.MidHalf := LineState.MidBeat + 0.5;
//  LineState.AktHalf := Floor(LineState.MidHalf);

  LineState.OldBeatC := LineState.CurrentBeatC;
  LineState.MidBeatC := GetMidBeat(LineState.CurrentTime - (CurrentSong.Gap) / 1000);
  LineState.CurrentBeatC := Floor(LineState.MidBeatC);

  LineState.OldBeatD := LineState.CurrentBeatD;
  LineState.MidBeatD := -0.5+GetMidBeat(LineState.CurrentTime - (CurrentSong.Gap + 120 + 20) / 1000); // MidBeat with addition GAP
  LineState.CurrentBeatD := Floor(LineState.MidBeatD);
  LineState.FracBeatD := Frac(LineState.MidBeatD);

  // sentences routines
  for CountGr := 0 to 0 do //High(Gracz)
  begin;
    CP := CountGr;
    // ustawianie starej parts
    LineState.OldLine := Lines[CP].Current;

    // wybieranie aktualnej parts
    for Count := 0 to Lines[CP].High do
    begin
      if LineState.CurrentBeat >= Lines[CP].Line[Count].Start then
        Lines[CP].Current := Count;
    end;

    // czysczenie nut gracza, gdy to jest nowa plansza
    // (optymizacja raz na halfbeat jest zla)
    if Lines[CP].Current <> LineState.OldLine then
      NewSentence(Sender);

  end; // for CountGr

  // wykonuje operacje raz na beat
  if (LineState.CurrentBeat >= 0) and (LineState.OldBeat <> LineState.CurrentBeat) then
    NewBeat(Sender);

  // make some operations on clicks
  if {(LineState.CurrentBeatC >= 0) and }(LineState.OldBeatC <> LineState.CurrentBeatC) then
    NewBeatC(Sender);

  // make some operations when detecting new voice pitch
  if (LineState.CurrentBeatD >= 0) and (LineState.OldBeatD <> LineState.CurrentBeatD) then
    NewBeatD(Sender);

  // wykonuje operacje w polowie beatu
//  if (LineState.AktHalf >= 1) and (LineState.OldHalf <> LineState.AktHalf) then
//    NewHalf;

  // plynnie przesuwa text
  Done := 1;
  for N := 0 to Lines[0].Line[Lines[0].Current].HighNote do
  begin
    if (Lines[0].Line[Lines[0].Current].Note[N].Start <= LineState.MidBeat) and
       (Lines[0].Line[Lines[0].Current].Note[N].Start + Lines[0].Line[Lines[0].Current].Note[N].Length >= LineState.MidBeat) then
    begin
      Done := (LineState.MidBeat - Lines[0].Line[Lines[0].Current].Note[N].Start) / (Lines[0].Line[Lines[0].Current].Note[N].Length);
    end;
  end;

  N := Lines[0].Line[Lines[0].Current].HighNote;

  // wylacza ostatnia nute po przejsciu
  {// todo: Lyrics
  if (Ini.LyricsEffect = 1) and (Done = 1) and
    (LineState.MidBeat > Lines[0].Line[Lines[0].Current].Note[N].Start + Lines[0].Line[Lines[0].Current].Note[N].Length)
    then Sender.LyricMain.Selected := -1;

  if Done > 1 then Done := 1;
  Sender.LyricMain.Done := Done;  }

  // use Done with LCD
{  with ScreenSing do begin
    if LyricMain.Selected >= 0 then begin
      LCD.MoveCursor(1, LyricMain.SelectedLetter + Round((LyricMain.SelectedLength-1) * Done));
      LCD.ShowCursor;
    end;
  end;}


end;

procedure NewSentence(Sender: TScreenSing);
var
G: Integer;
begin
  // czyszczenie nut graczy
  for G := 0 to High(Player) do
  begin
    Player[G].IlNut := 0;
    Player[G].HighNote := -1;
    SetLength(Player[G].Note, 0);
  end;

  // Add Words to Lyrics
  with Sender do
  begin
    {LyricMain.AddCzesc(Lines[0].Current);
    if Lines[0].Current < Lines[0].High then
      LyricSub.AddCzesc(Lines[0].Current+1)
    else
      LyricSub.Clear;}
    while (not Lyrics.LineinQueue) and (Lyrics.LineCounter <= High(Lines[0].Line)) do
      Lyrics.AddLine(@Lines[0].Line[Lyrics.LineCounter]);
  end;

  //Sender.UpdateLCD;
  
  //On Sentence Change...
  Sender.onSentenceChange(Lines[0].Current);
end;

procedure NewBeat(Sender: TScreenSing);
var
  Count:      integer;
//  TempBeat: integer;
begin
  // ustawia zaznaczenie tekstu
//  SingScreen.LyricMain.Selected := -1;
  for Count := 0 to Lines[0].Line[Lines[0].Current].HighNote do
    if (Lines[0].Line[Lines[0].Current].Note[Count].Start = LineState.CurrentBeat) then
    begin
      // operates on currently beated note
      //Todo: Lyrics
      //Sender.LyricMain.Selected := Count;

//      LCD.MoveCursor(1, ScreenSing.LyricMain.SelectedLetter);
//      LCD.ShowCursor;

      //LCD.MoveCursorBR(Sender.LyricMain.SelectedLetter);
      //LCD.ShowCursor;
    end;
end;

procedure NewBeatC;
var
  Count:    integer;
//  LPT_1:  integer;
//  LPT_2:  integer;
begin
//  LPT_1 := 1;
//  LPT_2 := 1;

  // beat click
  if (Ini.BeatClick = 1) and ((LineState.CurrentBeatC + Lines[0].Resolution + Lines[0].NotesGAP) mod Lines[0].Resolution = 0) then
    AudioPlayback.PlaySound(SoundLib.Click);

  // debug system on LPT
  if ((LineState.CurrentBeatC + Lines[0].Resolution + Lines[0].NotesGAP) mod Lines[0].Resolution = 0) then
  begin
    //LPT_1 := 0;
//    Light.LightOne(0, 150);

    (*
    Light.LightOne(1, 200); // beat light
    if ParamStr(1) = '-doublelights' then
      Light.LightOne(0, 200); // beat light
    *)


{    if ((LineState.CurrentBeatC + Lines[0].Resolution + Lines[0].NotesGAP) mod (Lines[0].Resolution * 2) = 0) then
      Light.LightOne(0, 150)
    else
      Light.LightOne(1, 150)}
  end;

  for Count := 0 to Lines[0].Line[Lines[0].Current].HighNote do
  begin
    if (Lines[0].Line[Lines[0].Current].Note[Count].Start = LineState.CurrentBeatC) then
    begin
      // click assist
      if Ini.ClickAssist = 1 then
        AudioPlayback.PlaySound(SoundLib.Click);

      //LPT_2 := 0;
      (*
      if ParamStr(1) <> '-doublelights' then
        Light.LightOne(0, 150); //125
      *)

      // drum machine
      (*
      TempBeat := LineState.CurrentBeat;// + 2;
      if (TempBeat mod 8 = 0) then Music.PlayDrum;
      if (TempBeat mod 8 = 4) then Music.PlayClap;
//      if (TempBeat mod 4 = 2) then Music.PlayHihat;
      if (TempBeat mod 4 <> 0) then Music.PlayHihat;
      *)
    end;
  end;

  {$IFDEF UseSerialPort}
    // PortWriteB($378, LPT_1 + LPT_2 * 2); // 0 zapala
  {$ENDIF}
end;

procedure NewBeatD(Sender: TScreenSing);
begin
  NewNote(Sender);
end;

procedure NewNote(Sender: TScreenSing);
var
  CP:     integer; // current player
  S:      integer; // sentence
  SMin:   integer;
  SMax:   integer;
  SDet:   integer; // temporary: sentence of detected note
  Count:  integer;
  Mozna:  boolean;
  New:    boolean;
  Range:  integer;
  NoteHit:boolean;
  MaxPoints: integer; // maximal points without line bonus
begin
  //  Log.LogStatus('Beat ' + IntToStr(LineState.CurrentBeat) + ' HalfBeat ' + IntToStr(LineState.AktHalf), 'NewBeat');

  // On linux we get an AV @ NEWNOTE,  line 600 of Classes/UMain.pas
  if not assigned( AudioInputProcessor.Sound ) then
    exit;

  // analizuje dla obu graczy ten sam sygnal (Sound.OneSrcForBoth)
  // albo juz lepiej nie
  for CP := 0 to PlayersPlay-1 do
  begin
    // analyze buffer
    AudioInputProcessor.Sound[CP].AnalyzeBuffer;

    // adds some noise
    //LineState.Tone := LineState.Tone + Round(Random(3)) - 1;

    // count min and max sentence range for checking (detection is delayed to the notes we see on the screen)
    SMin := Lines[0].Current-1;
    if SMin < 0 then
      SMin := 0;
    SMax := Lines[0].Current;

    // check if we can add new note
    Mozna := false;
    SDet:=SMin;
    for S := SMin to SMax do
    begin
      for Count := 0 to Lines[0].Line[S].HighNote do
      begin
        if ((Lines[0].Line[S].Note[Count].Start <= LineState.CurrentBeatD)
          and (Lines[0].Line[S].Note[Count].Start + Lines[0].Line[S].Note[Count].Length - 1 >= LineState.CurrentBeatD))
          and (Lines[0].Line[S].Note[Count].NoteType <> ntFreestyle) // but don't allow when it's FreeStyle note
          and (Lines[0].Line[S].Note[Count].Length > 0) then // and make sure the note lengths is at least 1
        begin
          SDet := S;
          Mozna := true;
          Break;
        end;
      end;
    end;

    S := SDet;

    //Czas.SzczytJest := true;
    //Czas.Tone := 27;

    // gdy moze, to dodaje nute - When Mozna, it adds note (?)
    if (AudioInputProcessor.Sound[CP].ToneValid) and (Mozna) then
    begin
      // operowanie na ostatniej nucie
      for Count := 0 to Lines[0].Line[S].HighNote do
      begin
        if (Lines[0].Line[S].Note[Count].Start <= LineState.OldBeatD+1) and
           (Lines[0].Line[S].Note[Count].Start +
            Lines[0].Line[S].Note[Count].Length > LineState.OldBeatD+1) then
        begin
          // to robi, tylko dla pary nut (oryginalnej i gracza)

          // przesuwanie tonu w odpowiednia game
          while (AudioInputProcessor.Sound[CP].Tone - Lines[0].Line[S].Note[Count].Tone > 6) do
            AudioInputProcessor.Sound[CP].Tone := AudioInputProcessor.Sound[CP].Tone - 12;

          while (AudioInputProcessor.Sound[CP].Tone - Lines[0].Line[S].Note[Count].Tone < -6) do
            AudioInputProcessor.Sound[CP].Tone := AudioInputProcessor.Sound[CP].Tone + 12;

          // Half size Notes Patch
          NoteHit := false;

          //if Ini.Difficulty = 0 then Range := 2;
          //if Ini.Difficulty = 1 then Range := 1;
          //if Ini.Difficulty = 2 then Range := 0;
          Range := 2 - Ini.Difficulty;

          if abs(Lines[0].Line[S].Note[Count].Tone - AudioInputProcessor.Sound[CP].Tone) <= Range then
          begin
            AudioInputProcessor.Sound[CP].Tone := Lines[0].Line[S].Note[Count].Tone;

            // Half size Notes Patch
            NoteHit := true;
            
            MaxPoints := 10000;
            if (Ini.LineBonus <> 0) then
              MaxPoints := 9000;
              
            case Lines[0].Line[S].Note[Count].NoteType of
              ntNormal:  Player[CP].Score := Player[CP].Score + MaxPoints / Lines[0].ScoreValue;
              ntGolden:  Player[CP].ScoreGolden := Player[CP].ScoreGolden + MaxPoints / Lines[0].ScoreValue;
            end;

            Player[CP].ScoreI := Floor(Player[CP].Score / 10) * 10;
            Player[CP].ScoreGoldenI := Floor(Player[CP].ScoreGolden / 10) * 10;

            Player[CP].ScoreTotalI := Player[CP].ScoreI + Player[CP].ScoreGoldenI + Player[CP].ScoreLineI;
          end;

        end; // operowanie
      end; // for

      // sprawdzanie czy to nowa nuta, czy przedluzenie
      if S = SMax then
      begin
        New := true;
        // if last has the same tone
        if (Player[CP].IlNut > 0 ) and
           (Player[CP].Note[Player[CP].HighNote].Tone = AudioInputProcessor.Sound[CP].Tone) and
           (Player[CP].Note[Player[CP].HighNote].Start + Player[CP].Note[Player[CP].HighNote].Length = LineState.CurrentBeatD) then
        begin
          New := false;
        end;

        // if is not as new note to control "beacie" (TODO: translate polish "beacie")
        for Count := 0 to Lines[0].Line[S].HighNote do
        begin
          if (Lines[0].Line[S].Note[Count].Start = LineState.CurrentBeatD) then
            New := true;
        end;

        // dodawanie nowej nuty
        if New then
        begin
          // New Note
          Player[CP].IlNut := Player[CP].IlNut + 1;
          Player[CP].HighNote := Player[CP].HighNote + 1;
          SetLength(Player[CP].Note, Player[CP].IlNut);
          Player[CP].Note[Player[CP].HighNote].Start   := LineState.CurrentBeatD;
          Player[CP].Note[Player[CP].HighNote].Length := 1;
          Player[CP].Note[Player[CP].HighNote].Tone     := AudioInputProcessor.Sound[CP].Tone; // Ton || TonDokl
          Player[CP].Note[Player[CP].HighNote].Detekt  := LineState.MidBeat;

          // Half Note Patch
          Player[CP].Note[Player[CP].HighNote].Hit := NoteHit;

          //Log.LogStatus('New Note ' + IntToStr(Gracz.Note[Gracz.HighNote].Start), 'NewBeat');
        end
        else
        begin
          // przedluzenie nuty
          Player[CP].Note[Player[CP].HighNote].Length := Player[CP].Note[Player[CP].HighNote].Length + 1;
        end;

        // check for perfect note and then lit the star (on Draw)
        for Count := 0 to Lines[0].Line[S].HighNote do
        begin
          if (Lines[0].Line[S].Note[Count].Start = Player[CP].Note[Player[CP].HighNote].Start) and
             (Lines[0].Line[S].Note[Count].Length = Player[CP].Note[Player[CP].HighNote].Length) and
             (Lines[0].Line[S].Note[Count].Tone = Player[CP].Note[Player[CP].HighNote].Tone) then
          begin
            Player[CP].Note[Player[CP].HighNote].Perfect := true;
          end;
        end;
      end; // if S = SMax

    end; // if moze
  end; // for CP
  //  Log.LogStatus('EndBeat', 'NewBeat');

  //On Sentence End -> For LineBonus + SingBar
  if (sDet >= low(Lines[0].Line)) and (sDet <= high(Lines[0].Line)) then
  begin
    if assigned( Sender ) and
       ((Lines[0].Line[SDet].Note[Lines[0].Line[SDet].HighNote].Start + Lines[0].Line[SDet].Note[Lines[0].Line[SDet].HighNote].Length - 1) = LineState.CurrentBeatD) then
    begin
      Sender.onSentenceEnd(sDet);
    end;
  end;

end;

procedure ClearScores(PlayerNum: integer);
begin
  Player[PlayerNum].Score := 0;
  Player[PlayerNum].ScoreI := 0;
  Player[PlayerNum].ScoreLine := 0;
  Player[PlayerNum].ScoreLineI := 0;
  Player[PlayerNum].ScoreGolden := 0;
  Player[PlayerNum].ScoreGoldenI := 0;
  Player[PlayerNum].ScoreTotalI := 0;
end;

//--------------------
// Function sets all Absolute Paths e.g. Song Path and makes sure the Directorys exist
//--------------------
procedure InitializePaths;

  // Initialize a Path Variable
  // After Setting Paths, make sure that Paths exist
  function initialize_path( out aPathVar : string; const aLocation : string ): boolean;
  var
    lWriteable: Boolean;
    lAttrib   : integer;
  begin
    lWriteable := false;
    aPathVar   := aLocation;

    // Make sure the directory is needex
    ForceDirectories(aPathVar);

    if DirectoryExists(aPathVar) then
    begin
      lAttrib := fileGetAttr(aPathVar);

      lWriteable := (lAttrib and faDirectory <> 0) and
                not (lAttrib and faReadOnly  <> 0)
    end;
    
    if not lWriteable then
      Log.LogError('Error: Dir ('+ aLocation +') is Readonly');

    result := lWriteable;
  end;

begin
  initialize_path( LogPath         , Platform.GetLogPath                             );
  initialize_path( SoundPath       , Platform.GetGameSharedPath + 'Sounds'      + PathDelim );
  initialize_path( ThemePath       , Platform.GetGameSharedPath + 'Themes'      + PathDelim );
  initialize_path( SkinsPath       , Platform.GetGameSharedPath + 'Themes'       + PathDelim );
  initialize_path( LanguagesPath   , Platform.GetGameSharedPath + 'Languages'   + PathDelim );
  initialize_path( PluginPath      , Platform.GetGameSharedPath + 'Plugins'     + PathDelim );
  initialize_path( VisualsPath     , Platform.GetGameSharedPath + 'Visuals'     + PathDelim );
  initialize_path( ResourcesPath   , Platform.GetGameSharedPath + 'Resources'   + PathDelim );
  initialize_path( ScreenshotsPath , Platform.GetGameUserPath + 'Screenshots' + PathDelim );

  // Users Song Path ....
  initialize_path( UserSongPath        , Platform.GetGameUserPath + 'Songs'       + PathDelim );
  initialize_path( UserCoversPath      , Platform.GetGameUserPath + 'Covers'      + PathDelim );
  initialize_path( UserPlaylistPath    , Platform.GetGameUserPath + 'Playlists'   + PathDelim );

  // Shared Song Path ....
  initialize_path( SongPath        , Platform.GetGameSharedPath + 'Songs'       + PathDelim );
  initialize_path( CoversPath      , Platform.GetGameSharedPath + 'Covers'      + PathDelim );
  initialize_path( PlaylistPath    , Platform.GetGameSharedPath + 'Playlists'   + PathDelim );

  DecimalSeparator := '.';
end;

end.
