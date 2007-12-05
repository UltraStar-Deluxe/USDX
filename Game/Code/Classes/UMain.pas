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
    OpenGL12,
    {$IFDEF UseSerialPort}
    zlportio {you can disable it and all PortWriteB calls},
    {$ENDIF}
    ULCD,
    ULight,
    UThemes;

type
  TPlayer = record
    Name:         string;

    //Index in Teaminfo record
    TeamID:       Byte;
    PlayerID:     Byte;

    //Scores
    Score:        real;
    ScoreLine:    real;
    ScoreGolden:  real;

    ScoreI:       integer;
    ScoreLineI:   integer;
    ScoreGoldenI: integer;
    ScoreTotalI:  integer;



    //LineBonus Mod
    ScoreLast:    Real;//Last Line Score

    //PerfectLineTwinkle Mod (effect)
    LastSentencePerfect: Boolean;
    //PerfectLineTwinkle Mod end


//    Meter:        real;

    HighNut:  integer;
    IlNut:    integer;
    Nuta:     array of record
      Start:      integer;
      Dlugosc:    integer;
      Detekt:     real;     // dokladne miejsce, w ktorym wykryto ta nute
      Ton:        real;
      Perfect:    boolean; // true if the note matches the original one, lit the star



      // Half size Notes Patch
      Hit:        boolean; // true if the note Hits the Line
      //end Half size Notes Patch



    end;
  end;


var
  //Absolute Paths
  GamePath:         string;
  SoundPath:        string;
  SongPath:         string;
  LogPath:          string;
  ThemePath:        string;
  ScreenshotsPath:  string;
  CoversPath:       string;
  LanguagesPath:    string;
  PluginPath:       string;
  PlayListPath:     string;

  OGL:      Boolean;
  Done:     Boolean;
  Event:    TSDL_event;
  FileName: string;
  Restart:  boolean;

  // gracz i jego nuty
  Player:       array of TPlayer;
  PlayersPlay:  integer;

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
function GetMidBeat(Time: real): real;
function GetTimeFromBeat(Beat: integer): real;
procedure ClearScores(PlayerNum: integer);

implementation

uses USongs, UJoystick, math, UCommandLine, ULanguage, SDL_ttf,
     USkins, UCovers, UCatCovers, UDataBase, UPlaylist, UDLLManager,
	 UParty, UCore, UGraphicClasses, UPluginDefs, UPlatform;

const
  Version = 'UltraStar Deluxe V 1.10 Alpha Build';

Procedure Main;
var
  WndTitle: string;
begin
  try

    WndTitle := Version;

    if Platform.TerminateIfAlreadyRunning( {var} WndTitle) then
      Exit;
      
    //------------------------------
    //StartUp - Create Classes and Load Files
    //------------------------------
    USTime := TTime.Create;

    // Commandline Parameter Parser
    Params := TCMDParams.Create;

    // Log + Benchmark
    Log := TLog.Create;
    Log.Title := WndTitle;
    Log.Enabled := Not Params.NoLog;
    Log.BenchmarkStart(0);

    // Language
    Log.BenchmarkStart(1);
    Log.LogStatus('Initialize Paths', 'Initialization');
    InitializePaths;
    Log.LogStatus('Load Language', 'Initialization');
    Language := TLanguage.Create;
    //Add Const Values:
      Language.AddConst('US_VERSION', Version);
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Loading Language', 1);

    // SDL
    Log.BenchmarkStart(1);
    Log.LogStatus('Initialize SDL', 'Initialization');
    SDL_Init(SDL_INIT_VIDEO or SDL_INIT_AUDIO);
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Initializing SDL', 1);

    // SDL_ttf
    Log.BenchmarkStart(1);
    Log.LogStatus('Initialize SDL_ttf', 'Initialization');
    TTF_Init();                      //ttf_quit();
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Initializing SDL_ttf', 1);

     // Skin
    Log.BenchmarkStart(1);
    Log.LogStatus('Loading Skin List', 'Initialization');
    Skin := TSkin.Create;
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Loading Skin List', 1);

    // Sound Card List
    Log.BenchmarkStart(1);
    Log.LogStatus('Loading Soundcard list', 'Initialization');
    Recording := TRecord.Create;
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Loading Soundcard list', 1);

    // Sound (+ fills Sound Card List)
    Log.BenchmarkStart(1);
    Log.LogStatus('Initialize Sound', 'Initialization');
    InitializeSound();
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Initializing Sound', 1);

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


    // LCD
    Log.BenchmarkStart(1);
    Log.LogStatus('Load LCD', 'Initialization');
    LCD := TLCD.Create;
    if Ini.LPT = 1 then begin
  //  LCD.HalfInterface := true;
      LCD.Enable;
      LCD.Clear;
      LCD.WriteText(1, '  UltraStar    ');
      LCD.WriteText(2, '  Loading...   ');
    end;
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Loading LCD', 1);

    // Light
    Log.BenchmarkStart(1);
    Log.LogStatus('Load Light', 'Initialization');
    Light := TLight.Create;
    if Ini.LPT = 2 then begin
      Light.Enable;
    end;
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Loading Light', 1);



    // Theme
    Log.BenchmarkStart(1);
    Log.LogStatus('Load Themes', 'Initialization');
    Theme := TTheme.Create('Themes\' + ITheme[Ini.Theme] + '.ini', Ini.Color);
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
//    Songs.LoadSongList;

    Log.LogStatus('Creating 2nd Song Array', 'Initialization');
    CatSongs := TCatSongs.Create;

    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Loading Songs', 1);

    // PluginManager
    Log.BenchmarkStart(1);
    Log.LogStatus('PluginManager', 'Initialization');
    DLLMan := TDLLMan.Create;   //Load PluginList
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

    //Playlist Manager
    Log.BenchmarkStart(1);
    Log.LogStatus('Playlist Manager', 'Initialization');
    PlaylistMan := TPlaylistManager.Create;
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Loading Playlist Manager', 1);

    //GoldenStarsTwinkleMod
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
    Core := TCore.Create('Ultrastar Deluxe Beta', MakeVersion(1,1,0, chr(0)));

    Log.LogError('Running Core');
    Core.Run;

    //------------------------------
    //Start- Mainloop
    //------------------------------
    //Music.SetLoop(true);
    //Music.SetVolume(50);
    //Music.Open(SkinPath + 'Menu Music 3.mp3');
    //Music.Play;
    Log.LogStatus('Main Loop', 'Initialization');
    MainLoop;

  finally
    //------------------------------
    //Finish Application
    //------------------------------
    if Ini.LPT = 1 then LCD.Clear;
    if Ini.LPT = 2 then Light.TurnOff;

    Log.LogStatus('Main Loop', 'Finished');
    Log.Free;
  end;
end;

procedure MainLoop;
var
  Delay:    integer;
begin
  try
    Delay := 0;
    SDL_EnableKeyRepeat(125, 125);

    CountSkipTime();  // JB - for some reason this seems to be needed when we use the SDL Timer functions.
    While not Done do
    Begin
      // joypad
      if (Ini.Joypad = 1) OR (Params.Joypad) then
        Joy.Update;

      // keyboard events
      CheckEvents;

      // display
      done := not Display.Draw;
      SwapBuffers;

      // light
      Light.Refresh;

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

    End;
    
  finally
    UnloadOpenGL;
  end;
End;

Procedure CheckEvents;
//var
//  p:    pointer;
Begin
  if not Assigned(Display.NextScreen) then
  While SDL_PollEvent( @event ) = 1 Do
  Begin
//    beep;
    Case Event.type_ Of
      SDL_QUITEV: begin
        Display.Fade := 0;
        Display.NextScreenWithCheck := nil;
        Display.CheckOK := True;
      end;
{      SDL_MOUSEBUTTONDOWN:
        With Event.button Do
        Begin
          If State = SDL_BUTTON_LEFT Then
          Begin
            //
          End;
        End; // With}
      SDL_KEYDOWN:
        begin
          //ScreenShot hack. If Print is pressed-> Make screenshot and Save to Screenshots Path
          if (Event.key.keysym.sym = SDLK_SYSREQ) then
            Display.ScreenShot

          // popup hack... if there is a visible popup then let it handle input instead of underlying screen
          // shoud be done in a way to be sure the topmost popup has preference (maybe error, then check)
          else if (ScreenPopupError <> NIL) and (ScreenPopupError.Visible) then
            done := not ScreenPopupError.ParseInput(Event.key.keysym.sym, Event.key.keysym.unicode, True)
          else if (ScreenPopupCheck <> NIL) AND (ScreenPopupCheck.Visible) then
            done := not ScreenPopupCheck.ParseInput(Event.key.keysym.sym, Event.key.keysym.unicode, True)
          // end of popup hack

          else
          begin
            // check for Screen want to Exit
            done := Not Display.ActualScreen^.ParseInput(Event.key.keysym.sym, Event.key.keysym.unicode, True);

            //If Screen wants to Exit
            if done then
            begin
              //If Question Option is enabled then Show Exit Popup
              if (Ini.AskbeforeDel = 1) then
              begin
                Display.ActualScreen^.CheckFadeTo(NIL,'MSG_QUIT_USDX');
              end
              else //When asking for exit is disabled then simply exit
              begin
                Display.Fade := 0;
                Display.NextScreenWithCheck := nil;
                Display.CheckOK := True;
              end;
            end;

          end;            //        if (Not Display.ActualScreen^.ParseInput(Event.key.keysym.scancode, True)) then
        end;
//      SDL_JOYAXISMOTION:
//        begin
//          beep
//        end;
      SDL_JOYBUTTONDOWN:
        begin
          beep
        end;
    End; // Case Event.type_
  End; // While
End; // CheckEvents

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
  if High(AktSong.BPM) = BPMNum then begin
    // last BPM
    CurBeat := AktSong.BPM[BPMNum].StartBeat + GetBeats(AktSong.BPM[BPMNum].BPM, Time);
    Time := 0;
  end else begin
    // not last BPM
    // count how much time is it for start of the new BPM and store it in NewTime
    NewTime := GetTimeForBeats(AktSong.BPM[BPMNum].BPM, AktSong.BPM[BPMNum+1].StartBeat - AktSong.BPM[BPMNum].StartBeat);

    // compare it to remaining time
    if (Time - NewTime) > 0 then begin
      // there is still remaining time
      CurBeat := AktSong.BPM[BPMNum].StartBeat;
      Time := Time - NewTime;
    end else begin
      // there is no remaining time
      CurBeat := AktSong.BPM[BPMNum].StartBeat + GetBeats(AktSong.BPM[BPMNum].BPM, Time);
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
  if Length(AktSong.BPM) = 1 then Result := Time * AktSong.BPM[0].BPM / 60;

  (* 2 BPMs *)
{  if Length(AktSong.BPM) > 1 then begin
    (* new system *)
    CurBeat := 0;
    TopBeat := GetBeats(AktSong.BPM[0].BPM, Time);
    if TopBeat > AktSong.BPM[1].StartBeat then begin
      // analyze second BPM
      Time := Time - GetTimeForBeats(AktSong.BPM[0].BPM, AktSong.BPM[1].StartBeat - CurBeat);
      CurBeat := AktSong.BPM[1].StartBeat;
      TopBeat := GetBeats(AktSong.BPM[1].BPM, Time);
      Result := CurBeat + TopBeat;

    end else begin
      (* pierwszy przedzial *)
      Result := TopBeat;
    end;
  end; // if}

  (* more BPMs *)
  if Length(AktSong.BPM) > 1 then begin

    CurBeat := 0;
    CurBPM := 0;
    while (Time > 0) do begin
      GetMidBeatSub(CurBPM, Time, CurBeat);
      Inc(CurBPM);
    end;

    Result := CurBeat;
  end; // if
end;

function GetTimeFromBeat(Beat: integer): real;
var
  CurBPM:   integer;
begin
  Result := 0;
  if Length(AktSong.BPM) = 1 then Result := AktSong.GAP / 1000 + Beat * 60 / AktSong.BPM[0].BPM;

  (* more BPMs *)
  if Length(AktSong.BPM) > 1 then begin
    Result := AktSong.GAP / 1000;
    CurBPM := 0;
    while (CurBPM <= High(AktSong.BPM)) and (Beat > AktSong.BPM[CurBPM].StartBeat) do begin
      if (CurBPM < High(AktSong.BPM)) and (Beat >= AktSong.BPM[CurBPM+1].StartBeat) then begin
        // full range
        Result := Result + (60 / AktSong.BPM[CurBPM].BPM) * (AktSong.BPM[CurBPM+1].StartBeat - AktSong.BPM[CurBPM].StartBeat);
      end;

      if (CurBPM = High(AktSong.BPM)) or (Beat < AktSong.BPM[CurBPM+1].StartBeat) then begin
        // in the middle
        Result := Result + (60 / AktSong.BPM[CurBPM].BPM) * (Beat - AktSong.BPM[CurBPM].StartBeat);
      end;
      Inc(CurBPM);
    end;

{    while (Time > 0) do begin
      GetMidBeatSub(CurBPM, Time, CurBeat);
      Inc(CurBPM);
    end;}
  end; // if}
end;

procedure Sing(Sender: TScreenSing);
var
  Pet:    integer;
  PetGr:  integer;
  CP:     integer;
  Done:   real;
  N:      integer;
begin
  Czas.Teraz := Czas.Teraz + TimeSkip;

  Czas.OldBeat := Czas.AktBeat;
  Czas.MidBeat := GetMidBeat(Czas.Teraz - (AktSong.Gap{ + 90 I've forgotten for what it is}) / 1000); // new system with variable BPM in function
  Czas.AktBeat := Floor(Czas.MidBeat);

//  Czas.OldHalf := Czas.AktHalf;
//  Czas.MidHalf := Czas.MidBeat + 0.5;
//  Czas.AktHalf := Floor(Czas.MidHalf);

  Czas.OldBeatC := Czas.AktBeatC;
  Czas.MidBeatC := GetMidBeat(Czas.Teraz - (AktSong.Gap) / 1000);
  Czas.AktBeatC := Floor(Czas.MidBeatC);

  Czas.OldBeatD := Czas.AktBeatD;
  Czas.MidBeatD := -0.5+GetMidBeat(Czas.Teraz - (AktSong.Gap + 120 + 20) / 1000); // MidBeat with addition GAP
  Czas.AktBeatD := Floor(Czas.MidBeatD);
  Czas.FracBeatD := Frac(Czas.MidBeatD);

  // sentences routines
  for PetGr := 0 to 0 do begin;//High(Gracz) do begin
    CP := PetGr;
    // ustawianie starej czesci
    Czas.OldCzesc := Czesci[CP].Akt;

    // wybieranie aktualnej czesci
    for Pet := 0 to Czesci[CP].High do
      if Czas.AktBeat >= Czesci[CP].Czesc[Pet].Start then Czesci[CP].Akt := Pet;

    // czysczenie nut gracza, gdy to jest nowa plansza
    // (optymizacja raz na halfbeat jest zla)
    if Czesci[CP].Akt <> Czas.OldCzesc then NewSentence(Sender);

  end; // for PetGr

  // wykonuje operacje raz na beat
  if (Czas.AktBeat >= 0) and (Czas.OldBeat <> Czas.AktBeat) then
    NewBeat(Sender);

  // make some operations on clicks
  if {(Czas.AktBeatC >= 0) and }(Czas.OldBeatC <> Czas.AktBeatC) then
    NewBeatC(Sender);

  // make some operations when detecting new voice pitch
  if (Czas.AktBeatD >= 0) and (Czas.OldBeatD <> Czas.AktBeatD) then
    NewBeatD(Sender);

  // wykonuje operacje w polowie beatu
//  if (Czas.AktHalf >= 1) and (Czas.OldHalf <> Czas.AktHalf) then
//    NewHalf;

  // plynnie przesuwa text
  Done := 1;
  for N := 0 to Czesci[0].Czesc[Czesci[0].Akt].HighNut do
    if (Czesci[0].Czesc[Czesci[0].Akt].Nuta[N].Start <= Czas.MidBeat)
    and (Czesci[0].Czesc[Czesci[0].Akt].Nuta[N].Start + Czesci[0].Czesc[Czesci[0].Akt].Nuta[N].Dlugosc >= Czas.MidBeat) then
      Done := (Czas.MidBeat - Czesci[0].Czesc[Czesci[0].Akt].Nuta[N].Start) / (Czesci[0].Czesc[Czesci[0].Akt].Nuta[N].Dlugosc);

  N := Czesci[0].Czesc[Czesci[0].Akt].HighNut;

  // wylacza ostatnia nute po przejsciu
  {// todo: Lyrics
  if (Ini.LyricsEffect = 1) and (Done = 1) and
    (Czas.MidBeat > Czesci[0].Czesc[Czesci[0].Akt].Nuta[N].Start + Czesci[0].Czesc[Czesci[0].Akt].Nuta[N].Dlugosc)
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
  for G := 0 to High(Player) do begin
    Player[G].IlNut := 0;
    Player[G].HighNut := -1;
    SetLength(Player[G].Nuta, 0);
  end;

  // Add Words to Lyrics
  with Sender do begin
    {LyricMain.AddCzesc(Czesci[0].Akt);
    if Czesci[0].Akt < Czesci[0].High then
      LyricSub.AddCzesc(Czesci[0].Akt+1)
    else
      LyricSub.Clear;}
    while (not Lyrics.LineinQueue) AND (Lyrics.LineCounter <= High(Czesci[0].Czesc)) do
      Lyrics.AddLine(@Czesci[0].Czesc[Lyrics.LineCounter]);
  end;

  Sender.UpdateLCD;
  
  //On Sentence Change...
  Sender.onSentenceChange(Czesci[0].Akt);
end;

procedure NewBeat(Sender: TScreenSing);
var
  Pet:      integer;
//  TempBeat: integer;
begin
  // ustawia zaznaczenie tekstu
//  SingScreen.LyricMain.Selected := -1;
  for Pet := 0 to Czesci[0].Czesc[Czesci[0].Akt].HighNut do
    if (Czesci[0].Czesc[Czesci[0].Akt].Nuta[Pet].Start = Czas.AktBeat) then begin
      // operates on currently beated note
      //Todo: Lyrics
      //Sender.LyricMain.Selected := Pet;

//      LCD.MoveCursor(1, ScreenSing.LyricMain.SelectedLetter);
//      LCD.ShowCursor;

      //LCD.MoveCursorBR(Sender.LyricMain.SelectedLetter);
      LCD.ShowCursor;

    end;
end;

procedure NewBeatC;
var
  Pet:    integer;
//  LPT_1:  integer;
//  LPT_2:  integer;
begin
//  LPT_1 := 1;
//  LPT_2 := 1;

  // beat click
  if (Ini.BeatClick = 1) and ((Czas.AktBeatC + Czesci[0].Resolution + Czesci[0].NotesGAP) mod Czesci[0].Resolution = 0) then
    AudioPlayback.PlayClick;

  // debug system on LPT
  if ((Czas.AktBeatC + Czesci[0].Resolution + Czesci[0].NotesGAP) mod Czesci[0].Resolution = 0) then begin
    //LPT_1 := 0;
//    Light.LightOne(0, 150);

    Light.LightOne(1, 200); // beat light
    if ParamStr(1) = '-doublelights' then
      Light.LightOne(0, 200); // beat light


{    if ((Czas.AktBeatC + Czesci[0].Resolution + Czesci[0].NotesGAP) mod (Czesci[0].Resolution * 2) = 0) then
      Light.LightOne(0, 150)
    else
      Light.LightOne(1, 150)}
  end;

  for Pet := 0 to Czesci[0].Czesc[Czesci[0].Akt].HighNut do
    if (Czesci[0].Czesc[Czesci[0].Akt].Nuta[Pet].Start = Czas.AktBeatC) then begin
      // click assist
      if Ini.ClickAssist = 1 then
        AudioPlayback.PlayClick;

        //LPT_2 := 0;
        if ParamStr(1) <> '-doublelights' then
        Light.LightOne(0, 150); //125


      // drum machine
(*      TempBeat := Czas.AktBeat;// + 2;
      if (TempBeat mod 8 = 0) then Music.PlayDrum;
      if (TempBeat mod 8 = 4) then Music.PlayClap;
//      if (TempBeat mod 4 = 2) then Music.PlayHihat;
      if (TempBeat mod 4 <> 0) then Music.PlayHihat;*)
    end;

    {$IFDEF UseSerialPort}
      // PortWriteB($378, LPT_1 + LPT_2 * 2); // 0 zapala
    {$ENDIF}
end;

procedure NewBeatD(Sender: TScreenSing);
begin
  NewNote(Sender);
end;

//procedure NewHalf;
//begin
//  NewNote;
//end;

procedure NewNote(Sender: TScreenSing);
var
  CP:     integer; // current player
  S:      integer; // sentence
  SMin:   integer;
  SMax:   integer;
  SDet:   integer; // temporary: sentence of detected note
  Pet:    integer;
  Mozna:  boolean;
  Nowa:   boolean;
  Range:  integer;
  NoteHit:boolean;
begin
//  Log.LogStatus('Beat ' + IntToStr(Czas.AktBeat) + ' HalfBeat ' + IntToStr(Czas.AktHalf), 'NewBeat');
//  beep;

  // On linux we get an AV @ NEWNOTE,  line 600 of Classes/UMain.pas
  if not assigned( Recording.Sound ) then  // TODO : JB_Linux ... why is this now not assigned... it was fine a few hours ago..
    exit;

  // analizuje dla obu graczy ten sam sygnal (Sound.OneSrcForBoth)
  // albo juz lepiej nie
  for CP := 0 to PlayersPlay-1 do
  begin

    // analyze buffer
    Recording.Sound[CP].AnalizujBufor;

    // adds some noise
//    Czas.Ton := Czas.Ton + Round(Random(3)) - 1;

    // 0.5.0: count min and max sentence range for checking (detection is delayed to the notes we see on the screen)
    SMin := Czesci[0].Akt-1;
    if SMin < 0 then SMin := 0;
    SMax := Czesci[0].Akt;

    // check if we can add new note
    Mozna := false;
    SDet:=SMin;
    for S := SMin to SMax do
      for Pet := 0 to Czesci[0].Czesc[S].HighNut do
        if ((Czesci[0].Czesc[S].Nuta[Pet].Start <= Czas.AktBeatD)
          and (Czesci[0].Czesc[S].Nuta[Pet].Start + Czesci[0].Czesc[S].Nuta[Pet].Dlugosc - 1 >= Czas.AktBeatD))
          and (not Czesci[0].Czesc[S].Nuta[Pet].FreeStyle) // but don't allow when it's FreeStyle note
          and (Czesci[0].Czesc[S].Nuta[Pet].Dlugosc > 0) // and make sure the note lenghts is at least 1
          then begin
            SDet := S;
            Mozna := true;
            Break;
          end;

    S := SDet;





//    Czas.SzczytJest := true;
//    Czas.Ton := 27;

    // gdy moze, to dodaje nute
    if (Recording.Sound[CP].SzczytJest) and (Mozna) then begin
      // operowanie na ostatniej nucie
      for Pet := 0 to Czesci[0].Czesc[S].HighNut do
        if (Czesci[0].Czesc[S].Nuta[Pet].Start <= Czas.OldBeatD+1)
        and (Czesci[0].Czesc[S].Nuta[Pet].Start +
        Czesci[0].Czesc[S].Nuta[Pet].Dlugosc > Czas.OldBeatD+1) then begin
          // to robi, tylko dla pary nut (oryginalnej i gracza)

          // przesuwanie tonu w odpowiednia game
          while (Recording.Sound[CP].Ton - Czesci[0].Czesc[S].Nuta[Pet].Ton > 6) do
            Recording.Sound[CP].Ton := Recording.Sound[CP].Ton - 12;
          while (Recording.Sound[CP].Ton - Czesci[0].Czesc[S].Nuta[Pet].Ton < -6) do
            Recording.Sound[CP].Ton := Recording.Sound[CP].Ton + 12;

          // Half size Notes Patch
          NoteHit := false;

          //if Ini.Difficulty = 0 then Range := 2;
          //if Ini.Difficulty = 1 then Range := 1;
          //if Ini.Difficulty = 2 then Range := 0;
          Range := 2 - Ini.Difficulty;
          if abs(Czesci[0].Czesc[S].Nuta[Pet].Ton - Recording.Sound[CP].Ton) <= Range then begin
            Recording.Sound[CP].Ton := Czesci[0].Czesc[S].Nuta[Pet].Ton;


            // Half size Notes Patch
            NoteHit := true;


            if (Ini.LineBonus = 0) then
            begin
            // add points without LineBonus
            case Czesci[0].Czesc[S].Nuta[Pet].Wartosc of
              1:  Player[CP].Score := Player[CP].Score + 10000 / Czesci[0].Wartosc *
                    Czesci[0].Czesc[S].Nuta[Pet].Wartosc;
              2:  Player[CP].ScoreGolden := Player[CP].ScoreGolden + 10000 / Czesci[0].Wartosc *
                    Czesci[0].Czesc[S].Nuta[Pet].Wartosc;
            end;
            end
            else
            begin
            // add points with Line Bonus
            case Czesci[0].Czesc[S].Nuta[Pet].Wartosc of
              1:  Player[CP].Score := Player[CP].Score + 9000 / Czesci[0].Wartosc *
                    Czesci[0].Czesc[S].Nuta[Pet].Wartosc;
              2:  Player[CP].ScoreGolden := Player[CP].ScoreGolden + 9000 / Czesci[0].Wartosc *
                    Czesci[0].Czesc[S].Nuta[Pet].Wartosc;
            end;
            end;

            Player[CP].ScoreI := Floor(Player[CP].Score / 10) * 10;
            Player[CP].ScoreGoldenI := Floor(Player[CP].ScoreGolden / 10) * 10;

            Player[CP].ScoreTotalI := Player[CP].ScoreI + Player[CP].ScoreGoldenI + Player[CP].ScoreLineI;
          end;

        end; // operowanie

      // sprawdzanie czy to nowa nuta, czy przedluzenie
      if S = SMax then begin
      Nowa := true;
      // jezeli ostatnia ma ten sam ton
      if (Player[CP].IlNut > 0 )
        and (Player[CP].Nuta[Player[CP].HighNut].Ton = Recording.Sound[CP].Ton)
        and (Player[CP].Nuta[Player[CP].HighNut].Start + Player[CP].Nuta[Player[CP].HighNut].Dlugosc = Czas.AktBeatD)
        then Nowa := false;
      // jezeli jest jakas nowa nuta na sprawdzanym beacie
      for Pet := 0 to Czesci[0].Czesc[S].HighNut do
        if (Czesci[0].Czesc[S].Nuta[Pet].Start = Czas.AktBeatD) then
          Nowa := true;

      // dodawanie nowej nuty
      if Nowa then begin
        // nowa nuta
        Player[CP].IlNut := Player[CP].IlNut + 1;
        Player[CP].HighNut := Player[CP].HighNut + 1;
        SetLength(Player[CP].Nuta, Player[CP].IlNut);
        Player[CP].Nuta[Player[CP].HighNut].Start := Czas.AktBeatD;
        Player[CP].Nuta[Player[CP].HighNut].Dlugosc := 1;
        Player[CP].Nuta[Player[CP].HighNut].Ton := Recording.Sound[CP].Ton; // Ton || TonDokl
        Player[CP].Nuta[Player[CP].HighNut].Detekt := Czas.MidBeat;


        // Half Note Patch
        Player[CP].Nuta[Player[CP].HighNut].Hit := NoteHit;


        //        Log.LogStatus('Nowa Nuta ' + IntToStr(Gracz.Nuta[Gracz.HighNut].Start), 'NewBeat');

      end else begin
        // przedluzenie nuty
        Player[CP].Nuta[Player[CP].HighNut].Dlugosc := Player[CP].Nuta[Player[CP].HighNut].Dlugosc + 1;
      end;


      // check for perfect note and then lit the star (on Draw)
      for Pet := 0 to Czesci[0].Czesc[S].HighNut do
        if (Czesci[0].Czesc[S].Nuta[Pet].Start = Player[CP].Nuta[Player[CP].HighNut].Start)
        and (Czesci[0].Czesc[S].Nuta[Pet].Dlugosc = Player[CP].Nuta[Player[CP].HighNut].Dlugosc)
        and (Czesci[0].Czesc[S].Nuta[Pet].Ton = Player[CP].Nuta[Player[CP].HighNut].Ton) then begin
          Player[CP].Nuta[Player[CP].HighNut].Perfect := true;
        end;

      end;// else beep; // if S = SMax

    end; // if moze
  end; // for CP
//  Log.LogStatus('EndBeat', 'NewBeat');

//On Sentence End -> For LineBonus + SingBar
if (sDet >= low(Czesci[0].Czesc)) AND (sDet <= high(Czesci[0].Czesc)) then
if ((Czesci[0].Czesc[SDet].Nuta[Czesci[0].Czesc[SDet].HighNut].Start + Czesci[0].Czesc[SDet].Nuta[Czesci[0].Czesc[SDet].HighNut].Dlugosc - 1) = Czas.AktBeatD) then
  Sender.onSentenceEnd(sDet);

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
  function initialize_path( out aPathVar : String; const aLocation : String ): boolean;
  var
    lWriteable: Boolean;
    lAttrib   : integer;
  begin
    lWriteable := false;
    aPathVar   := aLocation;

    // Make sure the directory is needex
    ForceDirectories(aPathVar);

    If DirectoryExists(aPathVar) then
    begin
      lAttrib := fileGetAttr(aPathVar);

      lWriteable :=     ( lAttrib and faDirectory <> 0 ) AND
                    NOT ( lAttrib and faReadOnly  <> 0 )
    end;
    
    if not lWriteable then
      Log.LogError('Error: Dir ('+ aLocation +') is Readonly');

    result := lWriteable;
  end;

begin

  GamePath := Platform.GetGamePath;

  initialize_path( LogPath         , GamePath                             );
  initialize_path( SoundPath       , GamePath + 'Sounds'      + PathDelim );
  initialize_path( SongPath        , GamePath + 'Songs'       + PathDelim );
  initialize_path( ThemePath       , GamePath + 'Themes'      + PathDelim );
  initialize_path( ScreenshotsPath , GamePath + 'Screenshots' + PathDelim );
  initialize_path( CoversPath      , GamePath + 'Covers'      + PathDelim );
  initialize_path( LanguagesPath   , GamePath + 'Languages'   + PathDelim );
  initialize_path( PluginPath      , GamePath + 'Plugins'     + PathDelim );
  initialize_path( PlaylistPath    , GamePath + 'Playlists'   + PathDelim );

  DecimalSeparator := ',';
end;

end.

