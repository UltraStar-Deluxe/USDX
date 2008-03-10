unit UGraphic;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SDL,
  OpenGL12,
  UTexture,
  TextGL,
  ULog,
  SysUtils,
  ULyrics,
  UScreenLoading,
  UScreenWelcome,
  UScreenMain,
  UScreenName,
  UScreenLevel,
  UScreenOptions,
  UScreenOptionsGame,
  UScreenOptionsGraphics,
  UScreenOptionsSound,
  UScreenOptionsLyrics,
  UScreenOptionsThemes,
  UScreenOptionsRecord,
  UScreenOptionsAdvanced,
  UScreenSong,
  UScreenSing,
  UScreenScore,
  UScreenTop5,
  UScreenEditSub,
  UScreenEdit,
  UScreenEditConvert,
  UScreenEditHeader,
  UScreenOpen,
  UThemes,
  USkins,
  UScreenSongMenu,
  UScreenSongJumpto,
  {Party Screens}
  UScreenSingModi,
  UScreenPartyNewRound,
  UScreenPartyScore,
  UScreenPartyOptions,
  UScreenPartyWin,
  UScreenPartyPlayer,
  {Stats Screens}
  UScreenStatMain,
  UScreenStatDetail,
  {CreditsScreen}
  UScreenCredits,
  {Popup for errors, etc.}
  UScreenPopup;

type
  TRecR = record
    Top:    real;
    Left:   real;
    Right:  real;
    Bottom: real;
  end;

var
  Screen:         PSDL_Surface;
  LoadingThread:  PSDL_Thread;
  Mutex:          PSDL_Mutex;

  RenderW:    integer;
  RenderH:    integer;
  ScreenW:    integer;
  ScreenH:    integer;
  Screens:    integer;
  ScreenAct:  integer;
  ScreenX:    integer;

  ScreenLoading:      TScreenLoading;
  ScreenWelcome:      TScreenWelcome;
  ScreenMain:         TScreenMain;
  ScreenName:         TScreenName;
  ScreenLevel:        TScreenLevel;
  ScreenSong:         TScreenSong;
  ScreenSing:         TScreenSing;
  ScreenScore:        TScreenScore;
  ScreenTop5:         TScreenTop5;
  ScreenOptions:          TScreenOptions;
  ScreenOptionsGame:      TScreenOptionsGame;
  ScreenOptionsGraphics:  TScreenOptionsGraphics;
  ScreenOptionsSound:     TScreenOptionsSound;
  ScreenOptionsLyrics:    TScreenOptionsLyrics;
  ScreenOptionsThemes:    TScreenOptionsThemes;
  ScreenOptionsRecord:    TScreenOptionsRecord;
  ScreenOptionsAdvanced:  TScreenOptionsAdvanced;
  ScreenEditSub:      TScreenEditSub;
  ScreenEdit:         TScreenEdit;
  ScreenEditConvert:  TScreenEditConvert;
  ScreenEditHeader:   TScreenEditHeader;
  ScreenOpen:         TScreenOpen;

  ScreenSongMenu:     TScreenSongMenu;
  ScreenSongJumpto:     TScreenSongJumpto;

  //Party Screens
  ScreenSingModi:         TScreenSingModi;
  ScreenPartyNewRound:    TScreenPartyNewRound;
  ScreenPartyScore:       TScreenPartyScore;
  ScreenPartyWin:         TScreenPartyWin;
  ScreenPartyOptions:     TScreenPartyOptions;
  ScreenPartyPlayer:      TScreenPartyPlayer;

  //StatsScreens
  ScreenStatMain:         TScreenStatMain;
  ScreenStatDetail:       TScreenStatDetail;

  //CreditsScreen
  ScreenCredits: TScreenCredits;

  //popup mod
  ScreenPopupCheck: TScreenPopupCheck;
  ScreenPopupError: TScreenPopupError;

  //Notes
  Tex_Left:        array[0..6] of TTexture;   //rename to tex_note_left
  Tex_Mid:         array[0..6] of TTexture;   //rename to tex_note_mid
  Tex_Right:       array[0..6] of TTexture;   //rename to tex_note_right

  Tex_plain_Left:  array[1..6] of TTexture;   //rename to tex_notebg_left
  Tex_plain_Mid:   array[1..6] of TTexture;   //rename to tex_notebg_mid
  Tex_plain_Right: array[1..6] of TTexture;   //rename to tex_notebg_right

  Tex_BG_Left:     array[1..6] of TTexture;   //rename to tex_noteglow_left
  Tex_BG_Mid:      array[1..6] of TTexture;   //rename to tex_noteglow_mid
  Tex_BG_Right:    array[1..6] of TTexture;   //rename to tex_noteglow_right

  Tex_Note_Star:  TTexture;
  Tex_Note_Perfect_Star: TTexture;


  Tex_Ball:       TTexture;
  Tex_Lyric_Help_Bar: TTexture;
  FullScreen:     boolean;

  Tex_TimeProgress: TTexture;

  //Sing Bar Mod
  Tex_SingBar_Back:  TTexture;
  Tex_SingBar_Bar:  TTexture;
  Tex_SingBar_Front:  TTexture;
  //end Singbar Mod

  //PhrasenBonus - Line Bonus Mod
  Tex_SingLineBonusBack: array[0..8] of TTexture;
  //End PhrasenBonus - Line Bonus Mod

  //ScoreBG Texs
  Tex_ScoreBG: array [0..5] of TTexture;

  //Score Screen Textures
    Tex_Score_NoteBarLevel_Dark     : array [1..6] of TTexture;
    Tex_Score_NoteBarRound_Dark     : array [1..6] of TTexture;

    Tex_Score_NoteBarLevel_Light    : array [1..6] of TTexture;
    Tex_Score_NoteBarRound_Light    : array [1..6] of TTexture;

    Tex_Score_NoteBarLevel_Lightest : array [1..6] of TTexture;
    Tex_Score_NoteBarRound_Lightest : array [1..6] of TTexture;

    Tex_Score_Ratings               : array [0..6] of TTexture;
    
const
  Skin_BGColorR = 1;
  Skin_BGColorG = 1;
  Skin_BGColorB = 1;

  Skin_SpectrumR = 0;
  Skin_SpectrumG = 0;
  Skin_SpectrumB = 0;

  Skin_Spectograph1R = 0.6;
  Skin_Spectograph1G = 0.8;
  Skin_Spectograph1B = 1;

  Skin_Spectograph2R = 0;
  Skin_Spectograph2G = 0;
  Skin_Spectograph2B = 0.2;

  Skin_SzczytR = 0.8;
  Skin_SzczytG = 0;
  Skin_SzczytB = 0;

  Skin_SzczytLimitR = 0;
  Skin_SzczytLimitG = 0.8;
  Skin_SzczytLimitB = 0;

  Skin_FontR = 0;
  Skin_FontG = 0;
  Skin_FontB = 0;

  Skin_FontHighlightR = 0.3; // 0.3
  Skin_FontHighlightG = 0.3; // 0.3
  Skin_FontHighlightB = 1;   // 1

  Skin_TimeR = 0.25; //0,0,0
  Skin_TimeG = 0.25;
  Skin_TimeB = 0.25;

  Skin_OscR = 0;
  Skin_OscG = 0;
  Skin_OscB = 0;

  Skin_LyricsT = 494; // 500 / 510 / 400
  Skin_SpectrumT = 470;
  Skin_SpectrumBot = 570;
  Skin_SpectrumH = 100;

  Skin_P1_LinesR = 0.5;  // 0.6 0.6 1
  Skin_P1_LinesG = 0.5;
  Skin_P1_LinesB = 0.5;

  Skin_P2_LinesR = 0.5; // 1 0.6 0.6
  Skin_P2_LinesG = 0.5;
  Skin_P2_LinesB = 0.5;

  Skin_P1_NotesB = 250;
  Skin_P2_NotesB = 430; // 430 / 300

  Skin_P1_ScoreT = 50;
  Skin_P1_ScoreL = 20;

  Skin_P2_ScoreT = 50;
  Skin_P2_ScoreL = 640;

procedure Initialize3D (Title: string);
procedure Reinitialize3D;
procedure SwapBuffers;

procedure LoadTextures;
procedure InitializeScreen;
procedure LoadLoadingScreen;
procedure LoadScreens;
procedure UnLoadScreens;

function LoadingThreadFunction: integer;


implementation

uses  UMain,
      UIni,
      UDisplay,
      UCommandLine,
      {$IFNDEF FPC}
      Graphics,
      {$ENDIF}
      {$IFDEF win32}
      windows,
      {$ENDIF}
      Classes;

procedure LoadFontTextures;
begin
  Log.LogStatus('Building Fonts', 'LoadTextures');
  BuildFont;
end;

procedure LoadTextures;


var
  P:        integer;
  R, G, B:  real;
  Col:      integer;
begin
   // zaladowanie tekstur
  Log.LogStatus('Loading Textures', 'LoadTextures');
  
  Tex_Left[0]  := Texture.LoadTexture(pchar(Skin.GetTextureFileName('GrayLeft')),  'BMP', 'Transparent', 0);     //brauch man die noch?
  Tex_Mid[0]   := Texture.LoadTexture(pchar(Skin.GetTextureFileName('GrayMid')),   'BMP', 'Plain', 0);           //brauch man die noch?
  Tex_Right[0] := Texture.LoadTexture(pchar(Skin.GetTextureFileName('GrayRight')), 'BMP', 'Transparent', 0);     //brauch man die noch?

  Log.LogStatus('Loading Textures - A', 'LoadTextures');
  
  // P1-6
  // TODO... do it once for each player... this is a bit crappy !!
  //                                       can we make it any better !?
  for P := 1 to 6 do
  begin
    LoadColor(R, G, B, 'P' + IntToStr(P) + 'Light');
    Col := $10000 * Round(R*255) + $100 * Round(G*255) + Round(B*255);
    
    Tex_Left[P]         := Texture.LoadTexture(pchar(Skin.GetTextureFileName('GrayLeft')),  'PNG', 'Colorized', Col);
    Tex_Mid[P]          := Texture.LoadTexture(pchar(Skin.GetTextureFileName('GrayMid')),   'PNG', 'Colorized', Col);
    Tex_Right[P]        := Texture.LoadTexture(pchar(Skin.GetTextureFileName('GrayRight')), 'PNG', 'Colorized', Col);

    Tex_plain_Left[P]   := Texture.LoadTexture(pchar(Skin.GetTextureFileName('NotePlainLeft')),  'PNG', 'Colorized', Col);
    Tex_plain_Mid[P]    := Texture.LoadTexture(pchar(Skin.GetTextureFileName('NotePlainMid')),   'PNG', 'Colorized', Col);
    Tex_plain_Right[P]  := Texture.LoadTexture(pchar(Skin.GetTextureFileName('NotePlainRight')), 'PNG', 'Colorized', Col);

    Tex_BG_Left[P]      := Texture.LoadTexture(pchar(Skin.GetTextureFileName('NoteBGLeft')),  'PNG', 'Colorized', Col);
    Tex_BG_Mid[P]       := Texture.LoadTexture(pchar(Skin.GetTextureFileName('NoteBGMid')),   'PNG', 'Colorized', Col);
    Tex_BG_Right[P]     := Texture.LoadTexture(pchar(Skin.GetTextureFileName('NoteBGRight')), 'PNG', 'Colorized', Col);
  end;

  Log.LogStatus('Loading Textures - B', 'LoadTextures');

  Tex_Note_Perfect_Star := Texture.LoadTexture(pchar(Skin.GetTextureFileName('NotePerfectStar')), 'PNG', 'Transparent', 0);
  Tex_Note_Star         := Texture.LoadTexture(pchar(Skin.GetTextureFileName('NoteStar')) , 'PNG', 'Transparent', $FFFFFF);
  Tex_Ball              := Texture.LoadTexture(pchar(Skin.GetTextureFileName('Ball')), 'BMP', 'Transparent', $FF00FF);
  Tex_Lyric_Help_Bar    := Texture.LoadTexture(pchar(Skin.GetTextureFileName('LyricHelpBar')), 'BMP', 'Transparent', $FF00FF);


  //TimeBar mod
  Tex_TimeProgress := Texture.LoadTexture(pchar(Skin.GetTextureFileName('TimeBar')));
  //eoa TimeBar mod

  //SingBar Mod
  Tex_SingBar_Back  := Texture.LoadTexture(pchar(Skin.GetTextureFileName('SingBarBack')),   'JPG', 'Plain', 0);
  Tex_SingBar_Bar   := Texture.LoadTexture(pchar(Skin.GetTextureFileName('SingBarBar')),   'JPG', 'Plain', 0);
  Tex_SingBar_Front := Texture.LoadTexture(pchar(Skin.GetTextureFileName('SingBarFront')),   'JPG', 'Font', 0);
  //end Singbar Mod

  Log.LogStatus('Loading Textures - C', 'LoadTextures');

  //Line Bonus PopUp
  for P := 0 to 8 do
    begin
      Case P of
        0: begin
          R := 1;
          G := 0;
          B := 0;
        end;
        1..3: begin
          R := 1;
          G := (P * 0.25);
          B := 0;
        end;
        4: begin
          R := 1;
          G := 1;
          B := 0;
        end;
        5..7: begin
          R := 1-((P-4)*0.25);
          G := 1;
          B := 0;
        end;
        8: begin
          R := 0;
          G := 1;
          B := 0;
        end;
      End;

      Col := $10000 * Round(R*255) + $100 * Round(G*255) + Round(B*255);
      Tex_SingLineBonusBack[P] :=  Texture.LoadTexture(pchar(Skin.GetTextureFileName('LineBonusBack')), 'PNG', 'Colorized', Col);
    end;

//## backgrounds for the scores ##
  for P := 0 to 5 do begin
    LoadColor(R, G, B, 'P' + IntToStr(P+1) + 'Light');
    Col := $10000 * Round(R*255) + $100 * Round(G*255) + Round(B*255);
    Tex_ScoreBG[P] := Texture.LoadTexture(pchar(Skin.GetTextureFileName('ScoreBG')),  'PNG', 'Colorized', Col);
  end;


  Log.LogStatus('Loading Textures - D', 'LoadTextures');

// ######################
// Score screen textures
// ######################

//## the bars that visualize the score ##
  for P := 1 to 6 do begin
//NoteBar ScoreBar
    LoadColor(R, G, B, 'P' + IntToStr(P) + 'Dark');
    Col := $10000 * Round(R*255) + $100 * Round(G*255) + Round(B*255);
    Tex_Score_NoteBarLevel_Dark[P] := Texture.LoadTexture(pchar(Skin.GetTextureFileName('ScoreLevel_Dark')),  'PNG', 'Colorized', Col);
    Tex_Score_NoteBarRound_Dark[P] := Texture.LoadTexture(pchar(Skin.GetTextureFileName('ScoreLevel_Dark_Round')),  'PNG', 'Colorized', Col);
//LineBonus ScoreBar
    LoadColor(R, G, B, 'P' + IntToStr(P) + 'Light');
    Col := $10000 * Round(R*255) + $100 * Round(G*255) + Round(B*255);
    Tex_Score_NoteBarLevel_Light[P] := Texture.LoadTexture(pchar(Skin.GetTextureFileName('ScoreLevel_Light')),  'PNG', 'Colorized', Col);
    Tex_Score_NoteBarRound_Light[P] := Texture.LoadTexture(pchar(Skin.GetTextureFileName('ScoreLevel_Light_Round')),  'PNG', 'Colorized', Col);
//GoldenNotes ScoreBar
    LoadColor(R, G, B, 'P' + IntToStr(P) + 'Lightest');
    Col := $10000 * Round(R*255) + $100 * Round(G*255) + Round(B*255);
    Tex_Score_NoteBarLevel_Lightest[P] := Texture.LoadTexture(pchar(Skin.GetTextureFileName('ScoreLevel_Lightest')),  'PNG', 'Colorized', Col);
    Tex_Score_NoteBarRound_Lightest[P] := Texture.LoadTexture(pchar(Skin.GetTextureFileName('ScoreLevel_Lightest_Round')),  'PNG', 'Colorized', Col);
  end;

//## rating pictures that show a picture according to your rate ##
    for P := 0 to 6 do begin
    Tex_Score_Ratings[P] := Texture.LoadTexture(pchar(Skin.GetTextureFileName('Rating_'+IntToStr(P))),  'PNG', 'Transparent', 0);
  end;

  Log.LogStatus('Loading Textures - Done', 'LoadTextures');
end;

procedure Initialize3D (Title: string);
//var
//  Icon: TIcon;
//  Res:  TResourceStream;
// ISurface: PSDL_Surface; // Auto Removed, Unused Variable
// Pixel: PByteArray; // Auto Removed, Unused Variable
// I: Integer; // Auto Removed, Unused Variable
begin
  Log.LogStatus('LoadOpenGL', 'UGraphic.Initialize3D');
//  Log.BenchmarkStart(2);

  LoadOpenGL;

  Log.LogStatus('SDL_Init', 'UGraphic.Initialize3D');
  if ( SDL_Init(SDL_INIT_VIDEO)= -1 ) then
  begin
    Log.LogError('SDL_Init Failed', 'UGraphic.Initialize3D');
    exit;
  end;

 { //Load Icon
  Res := TResourceStream.CreateFromID(HInstance, 3, RT_ICON);
  Icon := TIcon.Create;
  Icon.LoadFromStream(Res);
  Res.Free;
  Icon.
  //Create icon Surface
  SDL_CreateRGBSurfaceFrom (
  SDL_SWSURFACE,
  Icon.Width,
  Icon.Height,
  32,
  128 or 64,
  32 or 16,
  8 or 4,
  2 or 1);
  //SDL_BlitSurface(


  SDL_WM_SetIcon(SDL_LoadBMP('DEFAULT_WINDOW_ICON'), 0); //}

  SDL_WM_SetCaption(PChar(Title), nil);

  InitializeScreen;

//  Log.BenchmarkEnd(2);
//  Log.LogBenchmark('--> Setting Screen', 2);

  // ladowanie tekstur
//  Log.BenchmarkStart(2);
  Texture := TTextureUnit.Create;
  Texture.Limit := 1024*1024;

//  LoadTextures;
//  Log.BenchmarkEnd(2);
//  Log.LogBenchmark('--> Loading Textures', 2);

{  Log.BenchmarkStart(2);
  Lyric:= TLyric.Create;
  Log.BenchmarkEnd(2);
  Log.LogBenchmark('--> Loading Fonts', 2);
}

//  Log.BenchmarkStart(2);

  Log.LogStatus('TDisplay.Create', 'UGraphic.Initialize3D');
  Display := TDisplay.Create;
  
  Log.LogStatus('SDL_EnableUnicode', 'UGraphic.Initialize3D');
  SDL_EnableUnicode(1);
//  Log.BenchmarkEnd(2); Log.LogBenchmark('====> Creating Display', 2);

//  Log.LogStatus('Loading Screens', 'Initialize3D');
//  Log.BenchmarkStart(3);

  Log.LogStatus('Loading Font Textures', 'UGraphic.Initialize3D');
  LoadFontTextures();

  // Show the Loading Screen -------------
  Log.LogStatus('Loading Loading Screen', 'UGraphic.Initialize3D');
  LoadLoadingScreen;


  Log.LogStatus(' Loading Textures', 'UGraphic.Initialize3D');
  LoadTextures; // jb


  
  // now that we have something to display while loading,
  // start thread that loads the rest of ultrastar
//  Mutex   := SDL_CreateMutex;
//  SDL_UnLockMutex(Mutex);

  // funktioniert so noch nicht, da der ladethread unverändert auf opengl zugreifen will
  // siehe dazu kommentar unten
  // Englisch Translation:
  // is currently not working because the loading thread trys to accses opengl unchanged
  // look comment below

  //LoadingThread  := SDL_CreateThread(@LoadingThread, nil);

  // this would be run in the loadingthread
  Log.LogStatus(' Loading Screens', 'UGraphic.Initialize3D');
  LoadScreens;


  // TODO!!!!!!1
  // hier käme jetzt eine schleife, die
  // * den ladescreen malt (ab und zu)
  // * den "fortschritt" des ladescreens steuert
  // * zwischendrin schaut, ob der ladethread texturen geladen hat (mutex prüfen) und
  //   * die texturen in die opengl lädt, sowie
  //   * dem ladethread signalisiert, dass der speicher für die textur
  //     zum laden der nächsten textur weiterverwendet werden kann (über weiteren mutex)
  // * über einen 3. mutex so lange läuft, bis der ladethread signalisiert,
  //   dass er alles geladen hat fertig ist
  //
  // dafür muss loadtexture so umgeschrieben werden, dass es, statt selbst irgendwelche
  // opengl funktionen aufzurufen, entsprechend mutexe verändert
  // der hauptthread muss auch irgendwoher erfahren, was an opengl funktionen auszuführen ist,
  // mit welchen parametern (texturtyp, entspr. texturobjekt, textur-zwischenspeicher-adresse, ...
  //
  // English Translation:
  // here should be a loop witch
  // * draws the loading screen (form time to time)
  // * controlls the "process of the loading screen
  // * checks if the loadingthread has loaded textures (check mutex) and
  //   * load the textures into opengl
  //   * tells the loadingthread, that the memory for the texture can be reused
  //     to load the netx texture (over another mutex)
  // * runs as long as the loadingthread tells, that everything is loaded and ready (using a third mutex)
  //
  // therefor loadtexture have to be changed, that it, instat of caling some opengl functions
  // for itself, it should change mutex
  // the mainthread have to know somehow what opengl function have to be called with which parameters like
  // texturetype, textureobjekt, textur-buffer-adress, ...



  // wait for loading thread to finish
  // funktioniert so auch noch nicht - currently dos not work this way
  // SDL_WaitThread(LoadingThread, I);
  // SDL_DestroyMutex(Mutex);

  Display.CurrentScreen^.FadeTo( @ScreenMain );

  Log.BenchmarkEnd(2);
  Log.LogBenchmark('--> Loading Screens', 2);

  Log.LogStatus('Finish', 'Initialize3D');
end;

procedure SwapBuffers;
begin
  SDL_GL_SwapBuffers;
  glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    glOrtho(0, RenderW, RenderH, 0, -1, 100);
  glMatrixMode(GL_MODELVIEW);
end;

procedure Reinitialize3D;
begin
//  InitializeScreen;
//  LoadTextures;
//  LoadScreens;
end;

procedure InitializeScreen;
var
  S:      string;
  I:      integer;
  W, H:   integer;
  Depth:  Integer;
begin
  if (Params.Screens <> -1) then
    Screens := Params.Screens + 1
  else
    Screens := Ini.Screens + 1;

  SDL_GL_SetAttribute(SDL_GL_RED_SIZE,      5);
  SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE,    5);
  SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE,     5);
  SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE,    5);
  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE,    16);
  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER,  1);

  // If there is a resolution in Parameters, use it, else use the Ini value
  I := Params.Resolution;
  if (I <> -1) then
    S := IResolution[I]
  else
    S := IResolution[Ini.Resolution];

  I := Pos('x', S);
  W := StrToInt(Copy(S, 1, I-1)) * Screens;
  H := StrToInt(Copy(S, I+1, 1000));

  {if ParamStr(1) = '-fsblack' then begin
    W := 800;
    H := 600;
  end;
  if ParamStr(1) = '-320x240' then begin
    W := 320;
    H := 240;
  end; }

  If (Params.Depth <> -1) then
    Depth := Params.Depth
  else
    Depth := Ini.Depth;


  Log.LogStatus('SDL_SetVideoMode', 'Set Window Icon');

// Okay it's possible to set the title bar / taskbar icon here
// it's working this way, but just if the bmp is in your exe folder
  SDL_WM_SetIcon(SDL_LoadBMP('ustar-icon.bmp'), 0);

  Log.LogStatus('SDL_SetVideoMode', 'Initialize3D');
//  SDL_SetRefreshrate(85);
//  SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );

  {$IFDEF DARWIN}
    // Todo : eddie: remove before realease
    Ini.FullScreen := 0;
  {$ENDIF}

  if (Ini.FullScreen = 0) and (Not Params.FullScreen) then
  begin
    Log.LogStatus('SDL_SetVideoMode', 'Set Video Mode...   Windowed');
    screen := SDL_SetVideoMode(W, H, (Depth+1) * 16, SDL_OPENGL or SDL_RESIZABLE)
  end
  else
  begin
    Log.LogStatus('SDL_SetVideoMode', 'Set Video Mode...   Full Screen');
    screen := SDL_SetVideoMode(W, H, (Depth+1) * 16, SDL_OPENGL or SDL_FULLSCREEN );
    SDL_ShowCursor(0);
  end;
  
  if (screen = nil) then
  begin
    Log.LogError('SDL_SetVideoMode Failed', 'Initialize3D');
    exit;
  end;

  // clear screen once window is being shown
  glClearColor(1, 1, 1, 1);
  glClear(GL_COLOR_BUFFER_BIT);
  SwapBuffers;

  // zmienne
  RenderW := 800;
  RenderH := 600;
  ScreenW := W;
  ScreenH := H;
end;

procedure LoadLoadingScreen;
begin
  ScreenLoading := TScreenLoading.Create;
  ScreenLoading.onShow;
  
  Display.CurrentScreen := @ScreenLoading;

  swapbuffers;

  ScreenLoading.Draw;
  Display.Draw;

  SwapBuffers;
end;

procedure LoadScreens;
begin
{  ScreenLoading := TScreenLoading.Create;
  ScreenLoading.onShow;
  Display.CurrentScreen := @ScreenLoading;
  ScreenLoading.Draw;
  Display.Draw;
  SwapBuffers;
}
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Loading', 3); Log.BenchmarkStart(3);
{ ScreenWelcome :=          TScreenWelcome.Create; //'BG', 4, 3);
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Welcome', 3); Log.BenchmarkStart(3);}
  ScreenMain :=             TScreenMain.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Main', 3); Log.BenchmarkStart(3);
  ScreenName :=             TScreenName.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Name', 3); Log.BenchmarkStart(3);
  ScreenLevel :=            TScreenLevel.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Level', 3); Log.BenchmarkStart(3);
  ScreenSong :=             TScreenSong.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Song', 3); Log.BenchmarkStart(3);
  ScreenSongMenu :=             TScreenSongMenu.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Song Menu', 3); Log.BenchmarkStart(3);
  ScreenSing :=             TScreenSing.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Sing', 3); Log.BenchmarkStart(3);
  ScreenScore :=            TScreenScore.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Score', 3); Log.BenchmarkStart(3);
  ScreenTop5 :=             TScreenTop5.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Top5', 3); Log.BenchmarkStart(3);
  ScreenOptions :=          TScreenOptions.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Options', 3); Log.BenchmarkStart(3);
  ScreenOptionsGame :=      TScreenOptionsGame.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Options Game', 3); Log.BenchmarkStart(3);
  ScreenOptionsGraphics  :=  TScreenOptionsGraphics.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Options Graphics', 3); Log.BenchmarkStart(3);
  ScreenOptionsSound    :=     TScreenOptionsSound.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Options Sound', 3); Log.BenchmarkStart(3);
  ScreenOptionsLyrics   :=    TScreenOptionsLyrics.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Options Lyrics', 3); Log.BenchmarkStart(3);
  ScreenOptionsThemes   :=    TScreenOptionsThemes.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Options Themes', 3); Log.BenchmarkStart(3);
  ScreenOptionsRecord   :=    TScreenOptionsRecord.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Options Record', 3); Log.BenchmarkStart(3);
  ScreenOptionsAdvanced :=    TScreenOptionsAdvanced.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Options Advanced', 3); Log.BenchmarkStart(3);
  ScreenEditSub :=          TScreenEditSub.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Edit Sub', 3); Log.BenchmarkStart(3);
  ScreenEdit :=             TScreenEdit.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Edit', 3); Log.BenchmarkStart(3);
  ScreenEditConvert :=      TScreenEditConvert.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen EditConvert', 3); Log.BenchmarkStart(3);
//  ScreenEditHeader :=       TScreenEditHeader.Create(Skin.ScoreBG);
//  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Edit Header', 3); Log.BenchmarkStart(3);
  ScreenOpen :=             TScreenOpen.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Open', 3); Log.BenchmarkStart(3);
  ScreenSingModi :=         TScreenSingModi.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Sing with Modi support', 3); Log.BenchmarkStart(3);
  ScreenSongMenu :=         TScreenSongMenu.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen SongMenu', 3); Log.BenchmarkStart(3);
  ScreenSongJumpto :=         TScreenSongJumpto.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen SongJumpto', 3); Log.BenchmarkStart(3);
  ScreenPopupCheck := TScreenPopupCheck.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Popup (Check)', 3); Log.BenchmarkStart(3);
  ScreenPopupError := TScreenPopupError.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Popup (Error)', 3); Log.BenchmarkStart(3);
  ScreenPartyNewRound :=    TScreenPartyNewRound.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen PartyNewRound', 3); Log.BenchmarkStart(3);
  ScreenPartyScore :=       TScreenPartyScore.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen PartyScore', 3); Log.BenchmarkStart(3);
  ScreenPartyWin :=         TScreenPartyWin.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen PartyWin', 3); Log.BenchmarkStart(3);
  ScreenPartyOptions :=     TScreenPartyOptions.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen PartyOptions', 3); Log.BenchmarkStart(3);
  ScreenPartyPlayer :=      TScreenPartyPlayer.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen PartyPlayer', 3); Log.BenchmarkStart(3);
  ScreenStatMain :=         TScreenStatMain.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Stat Main', 3); Log.BenchmarkStart(3);
  ScreenStatDetail :=       TScreenStatDetail.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Stat Detail', 3); Log.BenchmarkStart(3);
  ScreenCredits    :=       TScreenCredits.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Credits', 3); Log.BenchmarkStart(3);

end;

function LoadingThreadFunction: integer;
begin
  LoadScreens;
  Result:= 1;
end;

procedure UnLoadScreens;
begin
  freeandnil( ScreenMain );
  freeandnil( ScreenName );
  freeandnil( ScreenLevel);
  freeandnil( ScreenSong );
  freeandnil( ScreenSongMenu );
  freeandnil( ScreenSing );
  freeandnil( ScreenScore);
  freeandnil( ScreenTop5 );
  freeandnil( ScreenOptions );
  freeandnil( ScreenOptionsGame );
  freeandnil( ScreenOptionsGraphics );
  freeandnil( ScreenOptionsSound );
  freeandnil( ScreenOptionsLyrics );
//  freeandnil( ScreenOptionsThemes );
  freeandnil( ScreenOptionsRecord );
  freeandnil( ScreenOptionsAdvanced );
  freeandnil( ScreenEditSub );
  freeandnil( ScreenEdit );
  freeandnil( ScreenEditConvert );
  freeandnil( ScreenOpen );
  freeandnil( ScreenSingModi );
  freeandnil( ScreenSongMenu );
  freeandnil( ScreenSongJumpto);
  freeandnil( ScreenPopupCheck );
  freeandnil( ScreenPopupError );
  freeandnil( ScreenPartyNewRound );
  freeandnil( ScreenPartyScore    );
  freeandnil( ScreenPartyWin     );
  freeandnil( ScreenPartyOptions  );
  freeandnil( ScreenPartyPlayer   );
  freeandnil( ScreenStatMain     );
  freeandnil( ScreenStatDetail    );
end;

end.
