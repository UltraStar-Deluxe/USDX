unit UGraphic;

interface
uses
  SDL, OpenGL12, UTexture, TextGL, ULog, SysUtils, ULyrics, UScreenLoading,
  UScreenWelcome, UScreenMain, UScreenName, UScreenLevel, UScreenOptions, UScreenOptionsGame,
  UScreenOptionsGraphics, UScreenOptionsSound, UScreenOptionsLyrics, UScreenOptionsThemes, UScreenOptionsRecord, UScreenOptionsAdvanced,
  UScreenSong, UScreenSing, UScreenScore, UScreenTop5, UScreenEditSub,
  UScreenEdit, UScreenEditConvert, UScreenEditHeader, UScreenOpen, UThemes, USkins, UScreenSongMenu, UScreenSongJumpto,
  {Party Screens} UScreenSingModi, UScreenPartyNewRound, UScreenPartyScore, UScreenPartyOptions, UScreenPartyWin, UScreenPartyPlayer,
  {Stats Screens} UScreenStatMain, UScreenStatDetail,
  {CreditsScreen} UScreenCredits,
  {Popup for errors, etc.} UScreenPopup;

type
  TRecR = record
    Top:    real;
    Left:   real;
    Right:  real;
    Bottom: real;
  end;

var
  Screen:             PSDL_Surface;

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
  Tex_Left:       array[0..6] of TTexture;
  Tex_Mid:        array[0..6] of TTexture;
  Tex_Right:      array[0..6] of TTexture;

  Tex_BG_Left:    array[1..6] of TTexture;
  Tex_BG_Mid:     array[1..6] of TTexture;
  Tex_BG_Right:   array[1..6] of TTexture;

  Tex_Note_Star:  TTexture;
  Tex_Note_Perfect_Star: TTexture;


  Tex_Ball:       TTexture;
  Tex_Lyric_Help_Bar: TTexture;
  FullScreen:     boolean;



  //Sing Bar Mod
  Tex_SingBar_Back:  TTexture;
  Tex_SingBar_Bar:  TTexture;
  Tex_SingBar_Front:  TTexture;
  //end Singbar Mod

  //PhrasenBonus - Line Bonus Mod
  Tex_SingLineBonusBack: TTexture;
  //End PhrasenBonus - Line Bonus Mod

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

  Skin_LyricsT = 500; // 510 / 400
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
procedure LoadScreens;


implementation
uses UMain, UIni, UDisplay, Graphics, Classes, Windows;

procedure LoadTextures;
var
  P:        integer;
  R, G, B:  real;
  Col:      integer;
begin
   // zaladowanie tekstur
  Log.LogStatus('Loading Textures', 'LoadTextures');
  Tex_Left[0] :=  Texture.LoadTexture(pchar(Skin.GetTextureFileName('GrayLeft')),  'BMP', 'Transparent', 0);
  Tex_Mid[0] :=   Texture.LoadTexture(pchar(Skin.GetTextureFileName('GrayMid')),   'BMP', 'Plain', 0);
  Tex_Right[0] := Texture.LoadTexture(pchar(Skin.GetTextureFileName('GrayRight')), 'BMP', 'Transparent', 0);

  // P1-6
  for P := 1 to 6 do begin
    LoadColor(R, G, B, 'P' + IntToStr(P) + 'Light');
    Col := $10000 * Round(R*255) + $100 * Round(G*255) + Round(B*255);
    Tex_Left[P] :=    Texture.LoadTexture(pchar(Skin.GetTextureFileName('GrayLeft')),  'BMP', 'Note Transparent', Col);
    Tex_Mid[P] :=     Texture.LoadTexture(pchar(Skin.GetTextureFileName('GrayMid')),   'BMP', 'Note Plain', Col);
    Tex_Right[P] :=   Texture.LoadTexture(pchar(Skin.GetTextureFileName('GrayRight')), 'BMP', 'Note Transparent', Col);

    Tex_BG_Left[P] :=  Texture.LoadTexture(pchar(Skin.GetTextureFileName('NoteBGLeft')),  'BMP', 'Alpha Black Colored', Col);
    Tex_BG_Mid[P] :=   Texture.LoadTexture(pchar(Skin.GetTextureFileName('NoteBGMid')),   'BMP', 'Alpha Black Colored', Col);
    Tex_BG_Right[P] := Texture.LoadTexture(pchar(Skin.GetTextureFileName('NoteBGRight')), 'BMP', 'Alpha Black Colored', Col);
  end;

  Tex_Note_Perfect_Star := Texture.LoadTexture(pchar(Skin.GetTextureFileName('NotePerfectStar')), 'JPG', 'Font Black', 0);
  Tex_Note_Star :=   Texture.LoadTexture(pchar(Skin.GetTextureFileName('NoteStar')) , 'JPG', 'Alpha Black Colored', $FFFFFF);
  Tex_Ball :=        Texture.LoadTexture(pchar(Skin.GetTextureFileName('Ball')), 'BMP', 'Transparent', $FF00FF);
  Tex_Lyric_Help_Bar := Texture.LoadTexture(pchar(Skin.GetTextureFileName('LyricHelpBar')), 'BMP', 'Transparent', $FF00FF);


  //SingBar Mod
  Tex_SingBar_Back :=  Texture.LoadTexture(pchar(Skin.GetTextureFileName('SingBarBack')),   'JPG', 'Plain', 0);
  Tex_SingBar_Bar :=  Texture.LoadTexture(pchar(Skin.GetTextureFileName('SingBarBar')),   'JPG', 'Plain', 0);
  Tex_SingBar_Front :=  Texture.LoadTexture(pchar(Skin.GetTextureFileName('SingBarFront')),   'JPG', 'Font', 0);
  //end Singbar Mod

  //PhrasenBonus - Line Bonus Mod
  Tex_SingLineBonusBack :=  Texture.LoadTexture(pchar(Skin.GetTextureFileName('LineBonusBack')), 'JPG', 'Font Black', 0);
  {//Set Texture to Font High
  Tex_SingLineBonusL.H := 32; Tex_SingLineBonusL.W := 8;
  Tex_SingLineBonusM.H := 32; //Tex_SingLineBonusM.TexW := Tex_SingLineBonusM.TexW/2;
  Tex_SingLineBonusR.H := 32; Tex_SingLineBonusR.W := 8;  }
  //PhrasenBonus - Line Bonus Mod End

  // tworzenie czcionek
  Log.LogStatus('Building Fonts', 'LoadTextures');
  BuildFont;
end;

procedure Initialize3D (Title: string);
var
  Icon: TIcon;
  Res:  TResourceStream;
  ISurface: PSDL_Surface;
  Pixel: PByteArray;
begin
  Log.LogStatus('LoadOpenGL', 'Initialize3D');
  Log.BenchmarkStart(2);

  LoadOpenGL;

  Log.LogStatus('SDL_Init', 'Initialize3D');
  if ( SDL_Init(SDL_INIT_VIDEO or SDL_INIT_AUDIO)= -1 ) then begin
    Log.LogError('SDL_Init Failed', 'Initialize3D');
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

  Log.BenchmarkEnd(2);
  Log.LogBenchmark('--> Setting Screen', 2);

  // ladowanie tekstur
  Log.BenchmarkStart(2);
  Texture := TTextureUnit.Create;
  Texture.Limit := 1024*1024;

  LoadTextures;
  Log.BenchmarkEnd(2);
  Log.LogBenchmark('--> Loading Textures', 2);

  Log.BenchmarkStart(2);
  Lyric := TLyric.Create;
  Log.BenchmarkEnd(2);
  Log.LogBenchmark('--> Loading Fonts', 2);

  Log.BenchmarkStart(2);
  Display := TDisplay.Create;
  SDL_EnableUnicode(1);
  Log.BenchmarkEnd(2); Log.LogBenchmark('====> Creating Display', 2);

  Log.LogStatus('Loading Screens', 'Initialize3D');
  Log.BenchmarkStart(3);

  LoadScreens;
  Display.ActualScreen^.FadeTo(@ScreenMain);

  Log.BenchmarkEnd(2);
  Log.LogBenchmark('--> Loading Screens', 2);

  Log.LogStatus('Finish', 'Initialize3D');
end;

procedure SwapBuffers;
begin
  SDL_GL_SwapBuffers;
  glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    glOrtho(0, 800, 600, 0, -1, 100);
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
begin
  Screens := Ini.Screens + 1;

  SDL_GL_SetAttribute(SDL_GL_RED_SIZE,      5);
  SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE,    5);
  SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE,     5);
  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE,    16);
  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER,  1);

  S := IResolution[Ini.Resolution];
  I := Pos('x', S);
  W := StrToInt(Copy(S, 1, I-1)) * Screens;
  H := StrToInt(Copy(S, I+1, 1000));

  if ParamStr(1) = '-fsblack' then begin
    W := 800;
    H := 600;
  end;
  if ParamStr(1) = '-320x240' then begin
    W := 320;
    H := 240;
  end;


  Log.LogStatus('SDL_SetVideoMode', 'Initialize3D');
//  SDL_SetRefreshrate(85);
//  SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );
  if (Ini.FullScreen = 0) and (ParamStr(1) <> '-fsblack') then
    screen := SDL_SetVideoMode(W, H, (Ini.Depth+1) * 16, SDL_OPENGL)
  else begin
    screen := SDL_SetVideoMode(W, H, (Ini.Depth+1) * 16, SDL_OPENGL or SDL_FULLSCREEN);
    SDL_ShowCursor(0);
  end;
  if (screen = nil) then begin
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

procedure LoadScreens;
begin
  ScreenLoading := TScreenLoading.Create;
  ScreenLoading.onShow;
  Display.ActualScreen := @ScreenLoading;
  ScreenLoading.Draw;
  Display.Draw;
  SwapBuffers;

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

end.
