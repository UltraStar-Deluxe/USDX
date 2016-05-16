{* UltraStar Deluxe - Karaoke Game
 *
 * UltraStar Deluxe is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/base/UGraphic.pas $
 * $Id: UGraphic.pas 2339 2010-05-05 07:22:17Z canni0 $
 *}

unit UGraphic;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  sdl2,
  dglOpenGL,
  UTexture,
  TextGL,
  UConfig,
  ULog,
  SysUtils,
  UImage,
  UCatCovers,
  USongs,
  UAvatars,
  UCovers,
  UMusic,
  UScreenLoading,
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
  UScreenOptionsNetwork,
  UScreenOptionsWebcam,
  UScreenOptionsJukebox,
  UScreenSong,
  UScreenSing,
  UScreenJukebox,
  UScreenJukeboxOptions,
  UScreenJukeboxPlaylist,
  UScreenScore,
  UScreenTop5,
  UScreenEditSub,
  UScreenEdit,
  UScreenEditHeader,
  UScreenEditConvert,
  UScreenOpen,
  UScreenAbout,
  USkins,
  UScreenSongMenu,
  UScreenSongJumpto,
  {Party Screens}
  UScreenPartyNewRound,
  UScreenPartyScore,
  UScreenPartyOptions,
  UScreenPartyWin,
  UScreenPartyPlayer,
  UScreenPartyRounds,
  UScreenPartyTournamentRounds,
  UScreenPartyTournamentPlayer,
  UScreenPartyTournamentOptions,
  UScreenPartyTournamentWin,
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
  Screen:         PSDL_Window;
  glcontext:      TSDL_GLContext;
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
  ScreenMain:         TScreenMain;
  ScreenName:         TScreenName;
  ScreenLevel:        TScreenLevel;
  ScreenSong:         TScreenSong;
  ScreenSing:         TScreenSing;

  ScreenJukebox:         TScreenJukebox;
  ScreenJukeboxOptions:  TScreenJukeboxOptions;
  ScreenJukeboxPlaylist: TScreenJukeboxPlaylist;

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
  ScreenOptionsNetwork:   TScreenOptionsNetwork;
  ScreenOptionsWebcam:    TScreenOptionsWebcam;
  ScreenOptionsJukebox:   TScreenOptionsJukebox;
  ScreenEditSub:      TScreenEditSub;
  ScreenEdit:         TScreenEdit;
  ScreenEditConvert:  TScreenEditConvert;
  ScreenEditHeader:   TScreenEditHeader;
  ScreenOpen:         TScreenOpen;
  ScreenAbout:        TScreenAbout;

  ScreenSongMenu:     TScreenSongMenu;
  ScreenSongJumpto:     TScreenSongJumpto;

  //Party Screens
  //ScreenSingModi:         TScreenSingModi;
  ScreenPartyNewRound:    TScreenPartyNewRound;
  ScreenPartyScore:       TScreenPartyScore;
  ScreenPartyWin:         TScreenPartyWin;
  ScreenPartyOptions:     TScreenPartyOptions;
  ScreenPartyPlayer:      TScreenPartyPlayer;
  ScreenPartyRounds:      TScreenPartyRounds;

  // Tournament
  ScreenPartyTournamentRounds:   TScreenPartyTournamentRounds;
  ScreenPartyTournamentPlayer:   TScreenPartyTournamentPlayer;
  ScreenPartyTournamentOptions:  TScreenPartyTournamentOptions;
  ScreenPartyTournamentWin:      TScreenPartyTournamentWin;

  //StatsScreens
  ScreenStatMain:         TScreenStatMain;
  ScreenStatDetail:       TScreenStatDetail;

  //CreditsScreen
  ScreenCredits: TScreenCredits;

  //popup mod
  ScreenPopupCheck: TScreenPopupCheck;
  ScreenPopupError: TScreenPopupError;
  ScreenPopupInfo:  TScreenPopupInfo;
  ScreenPopupInsertUser: TScreenPopupInsertUser;
  ScreenPopupSendScore:  TScreenPopupSendScore;
  ScreenPopupScoreDownload: TScreenPopupScoreDownload;

  //Notes
  Tex_Left:        array[1..6] of TTexture;   //rename to tex_note_left
  Tex_Mid:         array[1..6] of TTexture;   //rename to tex_note_mid
  Tex_Right:       array[1..6] of TTexture;   //rename to tex_note_right

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
  Tex_JukeboxTimeProgress: TTexture;
  
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

    Tex_Score_Ratings               : array [0..7] of TTexture;

  // arrows for SelectSlide
    Tex_SelectS_ArrowL:  TTexture;
    Tex_SelectS_ArrowR:  TTexture;

  // textures for software mouse cursor
    Tex_Cursor_Unpressed: TTexture;
    Tex_Cursor_Pressed:   TTexture;


  PboSupported: boolean;

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
procedure Finalize3D;
procedure Reinitialize3D;
procedure SwapBuffers;

procedure LoadTextures;
procedure InitializeScreen;
procedure LoadLoadingScreen;
procedure LoadScreens(Title: string);
procedure UnloadScreens;

function LoadingThreadFunction: integer;


implementation


uses
  Classes,
  UIni,
  UDisplay,
  UCommandLine,
  UPathUtils;

procedure LoadFontTextures;
begin
  Log.LogStatus('Building Fonts', 'LoadTextures');
  BuildFonts;
end;

procedure UnloadFontTextures;
begin
  Log.LogStatus('Kill Fonts', 'UnloadFontTextures');
  KillFonts;
end;

procedure LoadTextures;

var
  P:       integer;
  R, G, B: real;
  Col:     integer;
begin
  Log.LogStatus('Loading Textures', 'LoadTextures');

  Log.LogStatus('Loading Textures - A', 'LoadTextures');

  Tex_Note_Perfect_Star := Texture.LoadTexture(Skin.GetTextureFileName('NotePerfectStar'), TEXTURE_TYPE_TRANSPARENT, 0);
  Tex_Note_Star         := Texture.LoadTexture(Skin.GetTextureFileName('NoteStar') ,       TEXTURE_TYPE_TRANSPARENT, $FFFFFF);
  Tex_Ball              := Texture.LoadTexture(Skin.GetTextureFileName('Ball'),            TEXTURE_TYPE_TRANSPARENT, $FF00FF);
  Tex_Lyric_Help_Bar    := Texture.LoadTexture(Skin.GetTextureFileName('LyricHelpBar'),    TEXTURE_TYPE_TRANSPARENT, 0);

  Tex_SelectS_ArrowL    := Texture.LoadTexture(Skin.GetTextureFileName('Select_ArrowLeft'),    TEXTURE_TYPE_TRANSPARENT, 0);
  Tex_SelectS_ArrowR    := Texture.LoadTexture(Skin.GetTextureFileName('Select_ArrowRight'),    TEXTURE_TYPE_TRANSPARENT, 0);

  Tex_Cursor_Unpressed  := Texture.LoadTexture(Skin.GetTextureFileName('Cursor'), TEXTURE_TYPE_TRANSPARENT, 0);

  if (Skin.GetTextureFileName('Cursor_Pressed').IsSet) then
    Tex_Cursor_Pressed    := Texture.LoadTexture(Skin.GetTextureFileName('Cursor_Pressed'), TEXTURE_TYPE_TRANSPARENT, 0)
  else
    Tex_Cursor_Pressed.TexNum := 0;

  //TimeBar mod
  Tex_TimeProgress := Texture.LoadTexture(Skin.GetTextureFileName('TimeBar'));
  Tex_JukeboxTimeProgress := Texture.LoadTexture(Skin.GetTextureFileName('JukeboxTimeBar'));
  //eoa TimeBar mod

  //SingBar Mod
  Tex_SingBar_Back  := Texture.LoadTexture(Skin.GetTextureFileName('SingBarBack'),  TEXTURE_TYPE_PLAIN, 0);
  Tex_SingBar_Bar   := Texture.LoadTexture(Skin.GetTextureFileName('SingBarBar'),   TEXTURE_TYPE_PLAIN, 0);
  Tex_SingBar_Front := Texture.LoadTexture(Skin.GetTextureFileName('SingBarFront'), TEXTURE_TYPE_PLAIN, 0);
  //end Singbar Mod

  Log.LogStatus('Loading Textures - B', 'LoadTextures');

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
        else begin
          R := 1;
          G := 0;
          B := 0;
        end;

      End;

      Col := $10000 * Round(R*255) + $100 * Round(G*255) + Round(B*255);
      Tex_SingLineBonusBack[P] :=  Texture.LoadTexture(Skin.GetTextureFileName('LineBonusBack'), TEXTURE_TYPE_COLORIZED, Col);
    end;

    Log.LogStatus('Loading Textures - C', 'LoadTextures');

    //## rating pictures that show a picture according to your rate ##
    for P := 0 to 7 do begin
      Tex_Score_Ratings[P] := Texture.LoadTexture(Skin.GetTextureFileName('Rating_'+IntToStr(P)), TEXTURE_TYPE_TRANSPARENT, 0);
  end;

  Log.LogStatus('Loading Textures - Done', 'LoadTextures');
end;

(*
 * Load OpenGL extensions. Must be called after SDL_SetVideoMode() and each
 * time the pixel-format or render-context (RC) changes.
 *)
procedure LoadOpenGLExtensions;
begin
  // Load OpenGL 1.2 extensions for OpenGL 1.2 compatibility
  {// Load OpenGL 1.2 extensions for OpenGL 1.2 compatibility
  if (not Load_GL_version_2_0()) then
  begin
    Log.LogWarn('Failed loading OpenGL 1.2 or newer.' + sLineBreak +
    'Please check that your graphic drivers are up-to-date and get the newest drivers from the manufacturers website.' + sLineBreak + sLineBreak +
    'If that also fails, you could try to download and extract https://derpy.ws/builds/windows/trunk/latest/opengl32.7z to the UltraStar Deluxe folder and restart the game.', 'UGraphic.Initialize3D');
  end;

  // Other extensions e.g. OpenGL 1.3-2.0 or Framebuffer-Object might be loaded here
  // ...
  Load_GL_EXT_framebuffer_object();

  // PBO functions are loaded with VBO
  PboSupported := Load_GL_ARB_pixel_buffer_object()
      and Load_GL_ARB_vertex_buffer_object();
  Log.LogWarn('PBOSupported: ' + BoolToStr(PboSupported, true), 'LoadOpenGLExtensions');
  }//PboSupported := false;
end;

const
  WINDOW_ICON = 'icons/ultrastardx-icon.png';

procedure Initialize3D (Title: string);
var
  Icon: PSDL_Surface;
begin
  Log.LogStatus('SDL_Init', 'UGraphic.Initialize3D');
  if ( SDL_InitSubSystem(SDL_INIT_VIDEO) = -1 ) then
  begin
    Log.LogCritical('SDL_Init Failed', 'UGraphic.Initialize3D');
  end;
  InitializeScreen;
  // load icon image (must be 32x32 for win32)
  Icon := LoadImage(ResourcesPath.Append(WINDOW_ICON));
  if (Icon <> nil) then
    SDL_SetWindowIcon(Screen, Icon);

  SDL_SetWindowTitle(Screen, PChar(Title));

  { workaround for buggy Intel 3D driver on Linux }
  //SDL_putenv('texture_tiling=false');  //ToDo: on linux, check if this is still necessary with SDL 2

  SDL_SetWindowTitle(Screen, PChar(Title + ' - Initializing screen'));


  SDL_SetWindowTitle(Screen, PChar(Title + ' - Initializing texturizer'));
  Texture := TTextureUnit.Create;
  Texture.Limit :=1920; //currently, Full HD is all we want. switch to 64bit target before going further up

  //LoadTextures;
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Initializing video modules'));
  // Note: do not initialize video modules earlier. They might depend on some
  // SDL video functions or OpenGL extensions initialized in InitializeScreen()
  InitializeVideo();

  SDL_SetWindowTitle(Screen, PChar(Title + ' - Initializing 3D'));
  Log.LogStatus('TDisplay.Create', 'UGraphic.Initialize3D');
  Display := TDisplay.Create;
  //Display.SetCursor;

  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading font textures'));
  Log.LogStatus('Loading Font Textures', 'UGraphic.Initialize3D');
  LoadFontTextures();

  // Show the Loading Screen
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading first screen'));
  Log.LogStatus('Loading Loading Screen', 'UGraphic.Initialize3D');
  LoadLoadingScreen;

  // Covers Cache
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading and checking songs'));
  Log.LogStatus('Loading and checking songs', 'UGraphic.Initialize3D');
  Covers := TCoverDatabase.Create;

  // Category Covers
  Log.LogStatus('Creating Category Covers Array', 'Initialization');
  CatCovers:= TCatCovers.Create;

  // Avatars Cache
  Log.LogStatus('Creating Avatars Cache', 'Initialization');
  Avatars := TAvatarDatabase.Create;

  // Songs
  Log.LogStatus('Creating Song Array', 'Initialization');
  Songs := TSongs.Create;

  Log.LogStatus('Creating 2nd Song Array', 'Initialization');
  CatSongs := TCatSongs.Create;

  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading textures'));
  Log.LogStatus(' Loading Textures', 'UGraphic.Initialize3D');
  LoadTextures;

  // this would be run in the loadingthread
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading screens'));
  Log.LogStatus(' Loading Screens', 'UGraphic.Initialize3D');
  LoadScreens(Title);

  SDL_SetWindowTitle(Screen, PChar(Title));
  Display.CurrentScreen^.FadeTo( @ScreenMain );

  Log.BenchmarkEnd(2);
  Log.LogBenchmark('--> Loading Screens', 2);

  Log.LogStatus('Finish', 'Initialize3D');
end;

procedure SwapBuffers;
begin
  SDL_GL_SwapWindow(Screen);
  glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    glOrtho(0, RenderW, RenderH, 0, -1, 100);
  glMatrixMode(GL_MODELVIEW);
end;

procedure Finalize3D;
begin
  UnloadFontTextures;
  SDL_QuitSubSystem(SDL_INIT_VIDEO);
end;

procedure Reinitialize3D;
begin
  InitializeScreen;
end;

procedure InitializeScreen;
var
  S:      string;
  I:      integer;
  W, H:   integer;
  Depth:  Integer;
  Fullscreen: boolean;
  Split: boolean;
begin
  if (Params.Screens <> -1) then
    Screens := Params.Screens + 1
  else
    Screens := Ini.Screens + 1;
  case Params.Split of
    spmSplit:
      Split := True;
    spmNoSplit:
      Split := False;
    else
      Split := Ini.Split = 1;
  end; // case

  // If there is a resolution in Parameters, use it, else use the Ini value
  I := Params.Resolution;
  if (I <> -1) then
    S := IResolution[I]
  else
    S := IResolution[Ini.Resolution];

  I := Pos('x', S);
  W := StrToInt(Copy(S, 1, I-1));
  H := StrToInt(Copy(S, I+1, 1000));
  if ((Screens > 1) and not Split) then
  	W := W * Screens;

  Log.LogStatus('SDL_SetVideoMode', 'Initialize3D');

  // check whether to start in fullscreen or windowed mode.
  // The command-line parameters take precedence over the ini settings.
  Fullscreen := ((Ini.FullScreen = 1) or (Params.ScreenMode = scmFullscreen)) and
                not (Params.ScreenMode = scmWindowed);

  if Fullscreen then
  begin
    Log.LogStatus('SDL_SetVideoMode', 'Set Video Mode...   Full Screen');
    screen := SDL_CreateWindow('UltraStar Deluxe loading...',
           SDL_WINDOWPOS_CENTERED,SDL_WINDOWPOS_CENTERED, W, H, SDL_WINDOW_OPENGL or SDL_WINDOW_FULLSCREEN_DESKTOP or SDL_WINDOW_RESIZABLE);
  end
  else
  begin
    Log.LogStatus('SDL_SetVideoMode', 'Set Video Mode...   Windowed');
    screen := SDL_CreateWindow('UltraStar Deluxe loading...',
           SDL_WINDOWPOS_CENTERED,SDL_WINDOWPOS_CENTERED, W, H, SDL_Window_OPENGL or  SDL_RENDERER_ACCELERATED or SDL_WINDOW_RESIZABLE);
  end;

  //SDL_ShowCursor(0);    just to be able to debug while having mosue cursor

  if (screen = nil) then
  begin
    Log.LogCritical('SDL_SetVideoMode Failed', 'Initialize3D');
  end;

  //LoadOpenGL();
  glcontext := SDL_GL_CreateContext(Screen);
  InitOpenGL();

  //   ActivateRenderingContext(
  ReadExtensions;
  Log.LogInfo('OpenGL vendor ' + glGetString(GL_VENDOR), 'UGraphic.InitializeScreen');
  if not (glGetError = GL_NO_ERROR) then
  begin
    Log.LogInfo('an OpenGL Error happened.', 'UGraphic.InitializeScreen');
  end;
  Log.LogInfo('OpenGL renderer ' + glGetString(GL_RENDERER), 'UGraphic.InitializeScreen');
  Log.LogInfo('OpenGL version ' + glGetString(GL_VERSION), 'UGraphic.InitializeScreen');


  // define virtual (Render) and real (Screen) screen size
  RenderW := 800;
  RenderH := 600;
  ScreenW := Screen.w;
  ScreenH := Screen.h;
  // Ausganswerte für die State-Machine setzen
  SDL_GL_SetSwapInterval(1); // VSYNC (currently Windows only)
  glEnable(GL_TEXTURE_2D);	                // Aktiviert Texture Mapping
  glShadeModel(GL_SMOOTH);	                // Aktiviert weiches Shading
  glClearColor(0.0, 0.0, 0.0, 0.5);         // Bildschirm löschen (schwarz)
  glClearDepth(1.0);		                    // Depth Buffer Setup
  glEnable(GL_DEPTH_TEST);	                // Aktiviert Depth Testing
  glDepthFunc(GL_LEQUAL);	                  // Bestimmt den Typ des Depth Testing
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

  {LoadOpenGLExtensions();
  // define virtual (Render) and real (Screen) screen size
  RenderW := 800;
  RenderH := 600;
  ScreenW := Screen.w;
  ScreenH := Screen.h;

  // clear screen once window is being shown
  // Note: SwapBuffers uses RenderW/H, so they must be defined before
  glClearColor(1, 1, 1, 1);
  glClear(GL_COLOR_BUFFER_BIT);
  SwapBuffers;}
end;

procedure LoadLoadingScreen;
begin
  ScreenLoading := TScreenLoading.Create;
  ScreenLoading.OnShow;
  Display.CurrentScreen := @ScreenLoading;
  SwapBuffers;
  ScreenLoading.Draw;
  Display.Draw;
  SwapBuffers;
end;

procedure LoadScreens(Title: string);
begin
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading ScreenMain & ScreenName'));
  ScreenMain :=             TScreenMain.Create;
  ScreenName :=             TScreenName.Create;
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading ScreenLevel & ScreenSong'));
  ScreenLevel :=            TScreenLevel.Create;
  ScreenSong :=             TScreenSong.Create;
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading ScreenSongMenu & ScreenJukebox'));
  ScreenSongMenu :=             TScreenSongMenu.Create;
  ScreenJukebox :=             TScreenJukebox.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Jukebox', 3); Log.BenchmarkStart(3);
  ScreenJukeboxOptions :=   TScreenJukeboxOptions.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Jukebox Options', 3); Log.BenchmarkStart(3);
  ScreenJukeboxPlaylist :=   TScreenJukeboxPlaylist.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Jukebox Playlist', 3); Log.BenchmarkStart(3);
  ScreenTop5 :=             TScreenTop5.Create;
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading ScreenOptions & ScreenOptionsGame'));
  ScreenOptions :=          TScreenOptions.Create;
  ScreenOptionsGame :=      TScreenOptionsGame.Create;
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading ScreenOptionsGraphics & ScreenOptionsSound'));
  ScreenOptionsGraphics  :=  TScreenOptionsGraphics.Create;
  ScreenOptionsSound    :=     TScreenOptionsSound.Create;
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading ScreenOptionsLyrics & ScreenOptionsThemes'));
  ScreenOptionsLyrics   :=    TScreenOptionsLyrics.Create;
  ScreenOptionsThemes   :=    TScreenOptionsThemes.Create;
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading ScreenOptionsRecord & ScreenOptionsAdvanced'));
  ScreenOptionsRecord   :=    TScreenOptionsRecord.Create;
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading ScreenOptionsAdvanced'));
  ScreenOptionsAdvanced :=    TScreenOptionsAdvanced.Create;
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading ScreenOptionsNetwork'));
  ScreenOptionsNetwork :=    TScreenOptionsNetwork.Create;
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading ScreenOptionsWebCam'));
  ScreenOptionsWebcam  :=    TScreenOptionsWebcam.Create;
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading ScreenOptionsJukebox'));
  ScreenOptionsJukebox :=    TScreenOptionsJukebox.Create;
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading ScreenEditSub & ScreenEdit'));
  ScreenEditSub :=          TScreenEditSub.Create;
  ScreenEdit :=             TScreenEdit.Create;
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading ScreenOpen'));
  ScreenOpen :=             TScreenOpen.Create;
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading ScreenAbout'));
  ScreenAbout :=             TScreenAbout.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen About', 3); Log.BenchmarkStart(3);
  //ScreenSingModi :=         TScreenSingModi.Create;
  //Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Sing with Modi support', 3); Log.BenchmarkStart(3);
  ScreenSongJumpto :=         TScreenSongJumpto.Create;
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading ScreenPopupCheck & ScreenPopupError'));
  ScreenPopupCheck := TScreenPopupCheck.Create;
  ScreenPopupError := TScreenPopupError.Create;
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading ScreenPopupInfo & ScreenScoreX & ScreenPartyNewRound'));
  ScreenPopupInfo := TScreenPopupInfo.Create;
  ScreenPopupInsertUser := TScreenPopupInsertUser.Create;
  ScreenPopupSendScore := TScreenPopupSendScore.Create;
  ScreenPopupScoreDownload := TScreenPopupScoreDownload.Create;
  ScreenPartyNewRound :=    TScreenPartyNewRound.Create;
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading ScreenPartyScore & ScreenPartyWin'));
  ScreenPartyScore :=       TScreenPartyScore.Create;
  ScreenPartyWin :=         TScreenPartyWin.Create;
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading ScreenPartyOptions & ScreenPartyPlayer'));
  ScreenPartyOptions :=     TScreenPartyOptions.Create;
  ScreenPartyPlayer :=      TScreenPartyPlayer.Create;
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading ScreenPartyRounds & ScreenTournamentX & ScreenStatMain'));
  ScreenPartyRounds :=      TScreenPartyRounds.Create;
  ScreenPartyTournamentRounds :=      TScreenPartyTournamentRounds.Create;
  ScreenPartyTournamentPlayer :=      TScreenPartyTournamentPlayer.Create;
  ScreenPartyTournamentOptions :=      TScreenPartyTournamentOptions.Create;
  ScreenPartyTournamentWin :=      TScreenPartyTournamentWin.Create;
  ScreenStatMain :=         TScreenStatMain.Create;
  SDL_SetWindowTitle(Screen, PChar(Title + ' - Loading ScreenStatDetail & ScreenCredits'));
  ScreenStatDetail :=       TScreenStatDetail.Create;
  ScreenCredits    :=       TScreenCredits.Create;
  SDL_SetWindowTitle(Screen, PChar(Title));
end;

function LoadingThreadFunction: integer;
begin
  LoadScreens(USDXVersionStr);
  Result:= 1;
end;

procedure UnloadScreens;
begin
  ScreenMain.Free;
  ScreenName.Free;
  ScreenLevel.Free;
  ScreenSong.Free;
  //ScreenSing.Free;
  ScreenScore.Free;
  ScreenOptions.Free;
  ScreenOptionsGame.Free;
  ScreenOptionsGraphics.Free;
  ScreenOptionsSound.Free;
  ScreenOptionsLyrics.Free;
  ScreenOptionsThemes.Free;
  ScreenOptionsRecord.Free;
  ScreenOptionsAdvanced.Free;
  ScreenOptionsNetwork.Free;
  ScreenOptionsWebcam.Free;
  ScreenOptionsJukebox.Free;
  ScreenEditSub.Free;
  ScreenEdit.Free;
  ScreenJukebox.Free;
  ScreenJukeboxOptions.Free;
  ScreenJukeboxPlaylist.Free;
  ScreenTop5.Free;
  ScreenOpen.Free;
  ScreenAbout.Free;
  //ScreenSingModi.Free;
  ScreenSongMenu.Free;
  ScreenSongJumpto.Free;
  ScreenPopupCheck.Free;
  ScreenPopupError.Free;
  ScreenPopupInfo.Free;
  ScreenPopupInsertUser.Free;
  ScreenPopupSendScore.Free;
  ScreenPopupScoreDownload.Free;
  ScreenPartyNewRound.Free;
  ScreenPartyScore.Free;
  ScreenPartyWin.Free;
  ScreenPartyOptions.Free;
  ScreenPartyPlayer.Free;
  ScreenPartyRounds.Free;
  ScreenPartyTournamentRounds.Free;
  ScreenPartyTournamentPlayer.Free;
  ScreenPartyTournamentOptions.Free;
  ScreenPartyTournamentWin.Free;
  ScreenStatMain.Free;
  ScreenStatDetail.Free;
end;

end.
