program UltraStar;

{$DEFINE TRANSLATE}

{$R 'UltraStar.res' 'UltraStar.rc'}

uses
  //------------------------------
  //Includes - Menu System
  //------------------------------
  UDisplay in 'Menu\UDisplay.pas',
  UMenu in 'Menu\UMenu.pas',
  UMenuStatic in 'Menu\UMenuStatic.pas',
  UMenuText in 'Menu\UMenuText.pas',
  UMenuButton in 'Menu\UMenuButton.pas',
  UMenuInteract in 'Menu\UMenuInteract.pas',
  UMenuSelect in 'Menu\UMenuSelect.pas',
  UMenuSelectSlide in 'Menu\UMenuSelectSlide.pas',
  UDrawTexture in 'Menu\UDrawTexture.pas',
  UMenuButtonCollection in 'Menu\UMenuButtonCollection.pas',

  //------------------------------
  //Includes - Classes
  //------------------------------
  UGraphic in 'Classes\UGraphic.pas',
  UTexture in 'Classes\UTexture.pas',
  UMusic in 'Classes\UMusic.pas',
  ULanguage in 'Classes\ULanguage.pas',
  UMain in 'Classes\UMain.pas',
  UDraw in 'Classes\UDraw.pas',
  URecord in 'Classes\URecord.pas',
  UTime in 'Classes\UTime.pas',
  TextGL in 'Classes\TextGL.pas',
  USongs in 'Classes\USongs.pas',
  UIni in 'Classes\UIni.pas',
  USmpeg in 'SMpeg\USmpeg.pas',
  ULyrics in 'Classes\ULyrics.pas',
  USkins in 'Classes\USkins.pas',
  UThemes in 'Classes\UThemes.pas',
  ULog in 'Classes\ULog.pas',
  UJoystick in 'Classes\UJoystick.pas',
  ULCD in 'Classes\ULCD.pas',
  ULight in 'Classes\ULight.pas',
  UDataBase in 'Classes\UDataBase.pas',
  UCovers in 'Classes\UCovers.pas',
  UCatCovers in 'Classes\UCatCovers.pas',
  UFiles in 'Classes\UFiles.pas',
  UGraphicClasses in 'Classes\UGraphicClasses.pas',
  UDLLManager in 'Classes\UDLLManager.pas',
  UParty in 'Classes\UParty.pas',
  UPlaylist in 'Classes\UPlaylist.pas',
  UCommandLine  in 'Classes\UCommandLine.pas',

  //------------------------------
  //Includes - Screens
  //------------------------------
  UScreenLoading in 'Screens\UScreenLoading.pas',
  UScreenWelcome in 'Screens\UScreenWelcome.pas',
  UScreenMain in 'Screens\UScreenMain.pas',
  UScreenName in 'Screens\UScreenName.pas',
  UScreenLevel in 'Screens\UScreenLevel.pas',
  UScreenSong in 'Screens\UScreenSong.pas',
  UScreenSing in 'Screens\UScreenSing.pas',
  UScreenScore in 'Screens\UScreenScore.pas',
  UScreenOptions in 'Screens\UScreenOptions.pas',
  UScreenOptionsGame in 'Screens\UScreenOptionsGame.pas',
  UScreenOptionsGraphics in 'Screens\UScreenOptionsGraphics.pas',
  UScreenOptionsSound in 'Screens\UScreenOptionsSound.pas',
  UScreenOptionsLyrics in 'Screens\UScreenOptionsLyrics.pas',
  UScreenOptionsThemes in 'Screens\UScreenOptionsThemes.pas',
  UScreenOptionsRecord in 'Screens\UScreenOptionsRecord.pas',
  UScreenOptionsAdvanced in 'Screens\UScreenOptionsAdvanced.pas',
  UScreenEditSub in 'Screens\UScreenEditSub.pas',
  UScreenEdit in 'Screens\UScreenEdit.pas',
  UScreenEditConvert in 'Screens\UScreenEditConvert.pas',
  UScreenEditHeader in 'Screens\UScreenEditHeader.pas',
  UScreenOpen in 'Screens\UScreenOpen.pas',
  UScreenTop5 in 'Screens\UScreenTop5.pas',
  UScreenSongMenu in 'Screens\UScreenSongMenu.pas',
  UScreenSongJumpto in 'Screens\UScreenSongJumpto.pas',
  UScreenStatMain in 'Screens\UScreenStatMain.pas',
  UScreenStatDetail in 'Screens\UScreenStatDetail.pas',
  UScreenCredits in 'Screens\UScreenCredits.pas',
  UScreenPopup in 'Screens\UScreenPopup.pas',

  //------------------------------
  //Includes - Screens PartyMode
  //------------------------------
  UScreenSingModi in 'Screens\UScreenSingModi.pas',
  UScreenPartyNewRound in 'Screens\UScreenPartyNewRound.pas',
  UScreenPartyScore in 'Screens\UScreenPartyScore.pas',
  UScreenPartyPlayer in 'Screens\UScreenPartyPlayer.pas',
  UScreenPartyOptions in 'Screens\UScreenPartyOptions.pas',
  UScreenPartyWin in 'Screens\UScreenPartyWin.pas',

  //------------------------------
  //Includes - Modi SDK
  //------------------------------
  ModiSDK in '..\..\Modis\SDK\ModiSDK.pas',

  //------------------------------
  //Includes - Delphi
  //------------------------------
  Windows,
  SDL,
  SysUtils;

const
  Version = 'UltraStar Deluxe V 1.00 RC1';

var
  WndTitle: string;
  hWnd: THandle;
  I: Integer;

begin
  WndTitle := Version;

  //------------------------------
  //Start more than One Time Prevention
  //------------------------------
  hWnd:= FindWindow(nil, PChar(WndTitle));
  //Programm already started
  if (hWnd <> 0) then
  begin
    I := Messagebox(0, PChar('Another Instance of Ultrastar is already running. Contìnue ?'), PChar(WndTitle), MB_ICONWARNING or MB_YESNO);
    if (I = IDYes) then
    begin
      I := 1;
      repeat
        Inc(I);
        hWnd := FindWindow(nil, PChar(WndTitle + ' Instance ' + InttoStr(I)));
      until (hWnd = 0);

      WndTitle := WndTitle + ' Instance ' + InttoStr(I);
    end
    else
      Exit;
  end;

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
  Log.LogStatus('Initialize Paths', 'Initialization');        InitializePaths;
  Log.LogStatus('Load Language', 'Initialization');           Language := TLanguage.Create;
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

   // Skin
  Log.BenchmarkStart(1);
  Log.LogStatus('Loading Skin List', 'Initialization');             Skin := TSkin.Create;
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading Skin List', 1);

  // Sound Card List
  Log.BenchmarkStart(1);
  Log.LogStatus('Loading Soundcard list', 'Initialization');
  Recording := TRecord.Create;
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading Soundcard list', 1);

  // Ini + Paths
  Log.BenchmarkStart(1);
  Log.LogStatus('Load Ini', 'Initialization');                Ini := TIni.Create;
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
  Log.LogStatus('Load LCD', 'Initialization');                LCD := TLCD.Create;
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
  Log.LogStatus('Load Light', 'Initialization');              Light := TLight.Create;
  if Ini.LPT = 2 then begin
    Light.Enable;
  end;
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading Light', 1);

  // Theme
  Log.BenchmarkStart(1);
  Log.LogStatus('Load Themes', 'Initialization');             Theme := TTheme.Create('Themes\' + ITheme[Ini.Theme] + '.ini', Ini.Color);
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading Themes', 1);

  // Covers Cache
  Log.BenchmarkStart(1);
  Log.LogStatus('Creating Covers Cache', 'Initialization');   Covers := TCovers.Create;
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
  Log.LogStatus('Creating Song Array', 'Initialization');     Songs := TSongs.Create;
  Songs.LoadSongList;
  Log.LogStatus('Creating 2nd Song Array', 'Initialization'); CatSongs := TCatSongs.Create;
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading Songs', 1);

  // PluginManager
  Log.BenchmarkStart(1);
  Log.LogStatus('PluginManager', 'Initialization');
  DLLMan := TDLLMan.Create;   //Load PluginList
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading PluginManager', 1);

  // Party Mode Manager
  Log.BenchmarkStart(1);
  Log.LogStatus('PartySession Manager', 'Initialization');
  PartySession := TParty_Session.Create;   //Load PartySession
  
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading PartySession Manager', 1);

  // Graphics
  Log.BenchmarkStart(1);
  Log.LogStatus('Initialize 3D', 'Initialization');           Initialize3D(WndTitle);
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Initializing 3D', 1);

  // Sound
  Log.BenchmarkStart(1);
  Log.LogStatus('Initialize Sound', 'Initialization');
  Log.LogStatus('Creating Music', 'InitializeSound');         Music := TMusic.Create;
  InitializeSound;
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Initializing Sound', 1);

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
  if (Ini.Joypad = 1) OR (Params.Joypad) then begin
    Log.BenchmarkStart(1);
    Log.LogStatus('Initialize Joystick', 'Initialization');   Joy := TJoy.Create;
    Log.BenchmarkEnd(1);
    Log.LogBenchmark('Initializing Joystick', 1);
  end;

  Log.BenchmarkEnd(0);
  Log.LogBenchmark('Loading Time', 0);


  //------------------------------
  //Start- Mainloop
  //------------------------------
  //Music.SetLoop(true);
  //Music.SetVolume(50);
  //Music.Open(SkinPath + 'Menu Music 3.mp3');
  //Music.Play;
  Log.LogStatus('Main Loop', 'Initialization');               MainLoop;

  //------------------------------
  //Finish Application
  //------------------------------
  if Ini.LPT = 1 then LCD.Clear;
  if Ini.LPT = 2 then Light.TurnOff;

  Log.Free;
end.
