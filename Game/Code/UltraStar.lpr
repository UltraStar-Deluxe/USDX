program UltraStar;

{$DEFINE TRANSLATE}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ELSE}
  {$R 'UltraStar.res' 'UltraStar.rc'}
{$ENDIF}

{$I switches.inc}

uses

  // ***************************************************************************
  //
  //                         Developers PLEASE NOTE !!!!!!!
  //
  //  As of september 2007, I am working towards porting Ultrastar-DX to run
  //  on Linux.  I will be modifiying the source to make it compile in lazarus
  //  on windows & linux and I will make sure that it compiles in delphi still
  //  To help me in this endevour, please can you make a point of remembering
  //  that linux is CASE SENSATIVE, and file / unit names must be as per
  //  the filename exactly.
  //
  //  EG :  opengl12.pas  must not be OpenGL in the uses cluase.
  //
  //  thanks for your help...
  //
  // ***************************************************************************

  // Interesting stuff... :)
  // http://burningsmell.org/sdl_audioin/


  //------------------------------
  //Includes - 3rd Party Libraries
  //------------------------------
  
  // SDL / OpenGL
  moduleloader           in 'lib\JEDI-SDLv1.0\SDL\Pas\moduleloader.pas',
  opengl12               in 'lib\JEDI-SDLv1.0\OpenGL\Pas\opengl12.pas',
  sdl                    in 'lib\JEDI-SDLv1.0\SDL\Pas\sdl.pas',
  sdl_image              in 'lib\JEDI-SDLv1.0\SDL_Image\Pas\sdl_image.pas',
  sdl_ttf                in 'lib\JEDI-SDLv1.0\SDL_ttf\Pas\sdl_ttf.pas',
  sdlutils               in 'lib\JEDI-SDLv1.0\SDL\Pas\sdlutils.pas',


  // Bass
  {$IFDEF UseBASS}
  bass                   in 'lib\bass\delphi\bass.pas',
  {$ENDIF}

  // Midi Units
  {$IFDEF UseMIDIPort}
  Circbuf                in 'lib\midi\CIRCBUF.PAS',
  Delphmcb               in 'lib\midi\Delphmcb.PAS',
  MidiCons               in 'lib\midi\MidiCons.PAS',
  MidiDefs               in 'lib\midi\MidiDefs.PAS',
  MidiFile               in 'lib\midi\MidiFile.PAS',
  midiin                 in 'lib\midi\midiin.pas',
  midiout                in 'lib\midi\midiout.pas',
  MidiType               in 'lib\midi\MidiType.PAS',
  {$ENDIF}

  // FFMpeg units
  avcodec                in 'lib\ffmpeg\avcodec.pas',
  avformat               in 'lib\ffmpeg\avformat.pas',
  avio                   in 'lib\ffmpeg\avio.pas',
  avutil                 in 'lib\ffmpeg\avutil.pas',
  opt                    in 'lib\ffmpeg\opt.pas',
  rational               in 'lib\ffmpeg\rational.pas',


  // Sql Lite
  SQLiteTable3           in 'lib\SQLite\SQLiteTable3.pas',
  SQLite3                in 'lib\SQLite\SQLite3.pas',


  //------------------------------
  //Includes - Menu System
  //------------------------------
  
  UDisplay               in 'Menu\UDisplay.pas',
  UDrawTexture           in 'Menu\UDrawTexture.pas',
  UMenu                  in 'Menu\UMenu.pas',
  UMenuButton            in 'Menu\UMenuButton.pas',
  UMenuButtonCollection  in 'Menu\UMenuButtonCollection.pas',
  UMenuInteract          in 'Menu\UMenuInteract.pas',
  UMenuSelect            in 'Menu\UMenuSelect.pas',
  UMenuSelectSlide       in 'Menu\UMenuSelectSlide.pas',
  UMenuStatic            in 'Menu\UMenuStatic.pas',
  UMenuText              in 'Menu\UMenuText.pas',

  //------------------------------
  //Includes - Classes
  //------------------------------
  
  {$IFDEF FPC}
  ulazjpeg               in 'Classes\Ulazjpeg.pas',
  {$ENDIF}

  TextGL                 in 'Classes\TextGL.pas',
  UCatCovers             in 'Classes\UCatCovers.pas',
  UCommandLine           in 'Classes\UCommandLine.pas',
  UCommon                in 'Classes\UCommon.pas',
  UCovers                in 'Classes\UCovers.pas',
  UDataBase              in 'Classes\UDataBase.pas',
  UDLLManager            in 'Classes\UDLLManager.pas',
  UDraw                  in 'Classes\UDraw.pas',
  UFiles                 in 'Classes\UFiles.pas',
  UGraphic               in 'Classes\UGraphic.pas',
  UGraphicClasses        in 'Classes\UGraphicClasses.pas',
  UIni                   in 'Classes\UIni.pas',
  UJoystick              in 'Classes\UJoystick.pas',
  ULanguage              in 'Classes\ULanguage.pas',
  ULCD                   in 'Classes\ULCD.pas',
  ULight                 in 'Classes\ULight.pas',
  ULog                   in 'Classes\ULog.pas',
  ULyrics                in 'Classes\ULyrics.pas',
  ULyrics_bak            in 'Classes\ULyrics_bak.pas',
  UMain                  in 'Classes\UMain.pas',


  UMusic                 in 'Classes\UMusic.pas',

  UMedia_dummy           in 'Classes\UMedia_dummy.pas',  
  UVideo                 in 'Classes\UVideo.pas',
//  UAudio_FFMpeg          in 'Classes\UAudio_FFMpeg.pas',
{$ifdef win32}
  UAudio_bass            in 'Classes\UAudio_bass.pas',
{$endif}

//   UAudio_fmod            in 'Classes\UAudio_fmod.pas', // this has not yet been developed.. :(

  UParty                 in 'Classes\UParty.pas',
  UPlaylist              in 'Classes\UPlaylist.pas',
  URecord                in 'Classes\URecord.pas',
  USkins                 in 'Classes\USkins.pas',
  USingScores            in 'Classes\USingScores.pas',
  USongs                 in 'Classes\USongs.pas',
  UTexture               in 'Classes\UTexture.pas',
  UThemes                in 'Classes\UThemes.pas',
  UTime                  in 'Classes\UTime.pas',
  USingNotes 		         in 'Classes\USingNotes.pas',



  //------------------------------
  //Includes - Screens
  //------------------------------
  UScreenCredits         in 'Screens\UScreenCredits.pas',
  UScreenEdit            in 'Screens\UScreenEdit.pas',
  UScreenEditConvert     in 'Screens\UScreenEditConvert.pas',
  UScreenEditHeader      in 'Screens\UScreenEditHeader.pas',
  UScreenEditSub         in 'Screens\UScreenEditSub.pas',
  UScreenLevel           in 'Screens\UScreenLevel.pas',
  UScreenLoading         in 'Screens\UScreenLoading.pas',
  UScreenMain            in 'Screens\UScreenMain.pas',
  UScreenName            in 'Screens\UScreenName.pas',
  UScreenOpen            in 'Screens\UScreenOpen.pas',
  UScreenOptions         in 'Screens\UScreenOptions.pas',
  UScreenOptionsAdvanced in 'Screens\UScreenOptionsAdvanced.pas',
  UScreenOptionsGame     in 'Screens\UScreenOptionsGame.pas',
  UScreenOptionsGraphics in 'Screens\UScreenOptionsGraphics.pas',
  UScreenOptionsLyrics   in 'Screens\UScreenOptionsLyrics.pas',
  UScreenOptionsRecord   in 'Screens\UScreenOptionsRecord.pas',
  UScreenOptionsSound    in 'Screens\UScreenOptionsSound.pas',
  UScreenOptionsThemes   in 'Screens\UScreenOptionsThemes.pas',
  UScreenPopup           in 'Screens\UScreenPopup.pas',
  UScreenScore           in 'Screens\UScreenScore.pas',
  UScreenSing            in 'Screens\UScreenSing.pas',
  UScreenSong            in 'Screens\UScreenSong.pas',
  UScreenSongJumpto      in 'Screens\UScreenSongJumpto.pas',
  UScreenSongMenu        in 'Screens\UScreenSongMenu.pas',
  UScreenStatDetail      in 'Screens\UScreenStatDetail.pas',
  UScreenStatMain        in 'Screens\UScreenStatMain.pas',
  UScreenTop5            in 'Screens\UScreenTop5.pas',
  UScreenWelcome         in 'Screens\UScreenWelcome.pas',

  //------------------------------
  //Includes - Screens PartyMode
  //------------------------------
  UScreenPartyNewRound   in 'Screens\UScreenPartyNewRound.pas',
  UScreenPartyOptions    in 'Screens\UScreenPartyOptions.pas',
  UScreenPartyPlayer     in 'Screens\UScreenPartyPlayer.pas',
  UScreenPartyScore      in 'Screens\UScreenPartyScore.pas',
  UScreenPartyWin        in 'Screens\UScreenPartyWin.pas',
  UScreenSingModi        in 'Screens\UScreenSingModi.pas',

  //------------------------------
  //Includes - Modi SDK
  //------------------------------
  ModiSDK                in '..\..\Modis\SDK\ModiSDK.pas',


  //------------------------------
  //Includes - Delphi
  //------------------------------
  {$IFDEF win32}
  Windows,
  {$ENDIF}
  SysUtils;

const
  Version = 'UltraStar Deluxe V 1.10 Alpha Build';

var
  WndTitle: string;
  hWnd: THandle;
  I: Integer;
  
  aFormatCtx : PAVFormatContext;//PAVCodecContext;
  aCodecCtx  : PAVCodecContext;
  VideoStreamIndex,
  AudioStreamIndex : integer;

begin



(*

av_register_all;
aFormatCtx := av_alloc_format_context();
if av_open_input_file( aFormatCtx, pchar( Paramstr(1) ), NIL, 0, NIL) = 0 then
begin
  if av_find_stream_info( aFormatCtx ) >= 0 then
  begin
    writeln('');
    dump_format(aFormatCtx, 0, pchar( Paramstr(1) ), 0);
    writeln('');

//    writeln( pchar( filename ) );
    
//    av_read_play( aFormatCtx );
    find_stream_ids( aFormatCtx , VideoStreamIndex , AudioStreamIndex );

    writeln( 'VideoStreamIndex : ' + inttostr(VideoStreamIndex) );
    writeln( 'AudioStreamIndex : ' + inttostr(AudioStreamIndex) );
    
    aCodecCtx := aFormatCtx.streams[ AudioStreamIndex ].codec;
    writeln( 'Audio Codec Channels: '+ inttostr( integer( aCodecCtx.channels ) ) );
    writeln( 'Audio Codec freq: '+ inttostr( integer( aCodecCtx.sample_rate ) ) );
    
    wanted_spec.freq = aCodecCtx->sample_rate;
    wanted_spec.format = AUDIO_S16SYS;
    wanted_spec.channels = aCodecCtx->channels;
    wanted_spec.silence = 0;
    wanted_spec.samples = SDL_AUDIO_BUFFER_SIZE;
    wanted_spec.callback = audio_callback;
    wanted_spec.userdata = aCodecCtx;
    
    if(SDL_OpenAudio(&wanted_spec, aCodecCtx) < 0) then
    begin
      writeln( 'Could not do SDL_OpenAudio' );
      exit;
    end;


  end;
end;

exit;
*)

  WndTitle := Version;


  {$ifdef Win32}
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
  {$endif}

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

  // Sound
  Log.BenchmarkStart(1);
  Log.LogStatus('Initialize Sound', 'Initialization');        InitializeSound();
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Initializing Sound', 1);
  
  
(*
  // This is jays debugging for FFMpeg audio output..
  singleton_MusicFFMpeg.PlaySwoosh();
  writeln( 'did you hear the sound ?? ' );
  halt(0);
*)

  // Graphics
  Log.BenchmarkStart(1);
  Log.LogStatus('Initialize 3D', 'Initialization');           Initialize3D(WndTitle);
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
  
  Log.LogStatus('Cleanup', 'Done');

  //------------------------------
  //Finish Application
  //------------------------------
  if Ini.LPT = 1 then LCD.Clear;
  if Ini.LPT = 2 then Light.TurnOff;

  // Insignificant change..

  Log.Free;
end.
