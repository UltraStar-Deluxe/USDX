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
  UCommon,
  ULog,
  UIni,
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
  UScreenSingController,
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

const
  Mode_Windowed = 0;
  Mode_Borderless = 1;
  Mode_Fullscreen = 2;

type
  FullscreenModes = integer;


var
  Screen:         PSDL_Window;
  glcontext:      TSDL_GLContext;
  LoadingThread:  PSDL_Thread;
  Mutex:          PSDL_Mutex;

  CurrentWindowMode:      FullscreenModes;
  WindowModeDirty:        boolean;

  RenderW:    integer;
  RenderH:    integer;
  ScreenW:    integer;
  ScreenH:    integer;
  Screens:    integer;
  ScreenAct:  integer;
  ScreenX:    integer;
  LastX, LastY:    integer;
  LastW, LastH:    integer;
  HasValidPosition:     boolean;
  HasValidSize:         boolean;

  ScreenLoading:      TScreenLoading;
  ScreenMain:         TScreenMain;
  ScreenName:         TScreenName;
  ScreenLevel:        TScreenLevel;
  ScreenSong:         TScreenSong;
  ScreenSing:         TScreenSingController;

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
  Tex_Left:        array[1..UIni.IMaxPlayerCount] of TTexture;   //rename to tex_note_left
  Tex_Mid:         array[1..UIni.IMaxPlayerCount] of TTexture;   //rename to tex_note_mid
  Tex_Right:       array[1..UIni.IMaxPlayerCount] of TTexture;   //rename to tex_note_right

  Tex_plain_Left:  array[1..UIni.IMaxPlayerCount] of TTexture;   //rename to tex_notebg_left
  Tex_plain_Mid:   array[1..UIni.IMaxPlayerCount] of TTexture;   //rename to tex_notebg_mid
  Tex_plain_Right: array[1..UIni.IMaxPlayerCount] of TTexture;   //rename to tex_notebg_right

  Tex_BG_Left:     array[1..UIni.IMaxPlayerCount] of TTexture;   //rename to tex_noteglow_left
  Tex_BG_Mid:      array[1..UIni.IMaxPlayerCount] of TTexture;   //rename to tex_noteglow_mid
  Tex_BG_Right:    array[1..UIni.IMaxPlayerCount] of TTexture;   //rename to tex_noteglow_right

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
    Tex_Score_NoteBarLevel_Dark     : array [1..UIni.IMaxPlayerCount] of TTexture;
    Tex_Score_NoteBarRound_Dark     : array [1..UIni.IMaxPlayerCount] of TTexture;

    Tex_Score_NoteBarLevel_Light    : array [1..UIni.IMaxPlayerCount] of TTexture;
    Tex_Score_NoteBarRound_Light    : array [1..UIni.IMaxPlayerCount] of TTexture;

    Tex_Score_NoteBarLevel_Lightest : array [1..UIni.IMaxPlayerCount] of TTexture;
    Tex_Score_NoteBarRound_Lightest : array [1..UIni.IMaxPlayerCount] of TTexture;

    Tex_Score_Ratings               : array [0..7] of TTexture;  //stores all possible rating result images

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

function SwitchVideoMode(Mode: FullscreenModes): FullscreenModes;
function HasWindowState(Flag: integer): boolean;

// events
procedure OnWindowMoved(x,y: integer);
procedure OnWindowResized(w,h: integer);

implementation


uses
  Classes,
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
  X, Y:   integer; // offset for re-positioning
  Depth:  Integer;
  Borderless, Fullscreen: boolean;
  Split: boolean;
  Disp: TSDL_DisplayMode;
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

  // check whether to start in fullscreen, windowed mode or borderless mode (windowed fullscreen).
  // The command-line parameters take precedence over the ini settings.
  Borderless := (Ini.FullScreen = 2) and (Params.ScreenMode <> scmFullscreen);
  Fullscreen := ((Ini.FullScreen = 1) or (Params.ScreenMode = scmFullscreen)) and
                not (Params.ScreenMode = scmWindowed);

  // If there is a resolution in Parameters, use it, else use the Ini value
  // check for a custom resolution (in the format of WIDTHxHEIGHT) or try validating ID from TIni
  if ParseResolutionString(Params.CustomResolution, W, H) then
    Log.LogStatus(Format('Use custom resolution from Command line: %d x %d', [W, H]), 'SDL_SetVideoMode')
  else if Ini.GetResolution(Params.Resolution, S) and ParseResolutionString(S, W, H) then
    Log.LogStatus(Format('Use resolution by index from command line: %d x %d [%d]', [W, H, Params.Resolution]), 'SDL_SetVideoMode')
  else if Fullscreen then
  begin
    Log.LogStatus('Use config fullscreen resolution', 'SDL_SetVideoMode');
    S := Ini.GetResolutionFullscreen(W, H);
  end
  else
  begin
    Log.LogStatus('Use config resolution', 'SDL_SetVideoMode');
    S := Ini.GetResolution(W, H);
  end;

  if ((Screens > 1) and not Split) then
  	W := W * Screens;

  Log.LogStatus('Creating window', 'SDL_SetVideoMode');

  // TODO: use SDL renderer (for proper scale in "real fullscreen"). Able to choose rendering mode (OpenGL, OpenGL ES, Direct3D)
  if Borderless then
  begin
    Log.LogStatus('Set Video Mode...   Borderless fullscreen', 'SDL_SetVideoMode');
    CurrentWindowMode := Mode_Borderless;
    // TODO: use windowed resolution, in order to switch to proper windowed size (with F11); or apply size when switching mode
    screen := SDL_CreateWindow('UltraStar Deluxe loading...',
              SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, W, H, SDL_WINDOW_OPENGL or SDL_WINDOW_FULLSCREEN_DESKTOP or SDL_WINDOW_RESIZABLE);
  end
  else if Fullscreen then
  begin
    Log.LogStatus('Set Video Mode...   Fullscreen', 'SDL_SetVideoMode');
    CurrentWindowMode := Mode_Fullscreen;
    screen := SDL_CreateWindow('UltraStar Deluxe loading...',
              SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, W, H, SDL_WINDOW_OPENGL or SDL_WINDOW_FULLSCREEN or SDL_WINDOW_RESIZABLE);
  end
  else
  begin
    Log.LogStatus('Set Video Mode...   Windowed', 'SDL_SetVideoMode');
    CurrentWindowMode := Mode_Windowed;
    screen := SDL_CreateWindow('UltraStar Deluxe loading...',
              SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, W, H, SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE);
  end;

  //SDL_ShowCursor(0);    just to be able to debug while having mosue cursor

  if (screen = nil) then
  begin
    Log.LogCritical('Creating window failed', 'SDL_SetVideoMode');
  end
  else
  begin
    X:=0; Y:=0;

    // check if created window has the desired size, otherwise override the config resolution value
    if SDL_GetWindowDisplayMode(screen, @Disp) = 0 then
    begin
      if (Disp.w < W) or (Disp.h < H) then
      begin
        Log.LogStatus(Format('Video resolution (%s) exceeded possible size (%s). Override stored config resolution!', [BuildResolutionString(W,H), BuildResolutionString(Disp.w, Disp.h)]), 'SDL_SetVideoMode');
        Ini.SetResolution(Disp.w, Disp.h, true);
      end
      else if Fullscreen and ((Disp.w > W) or (Disp.h > H)) then
      begin
        Log.LogStatus(Format('Video resolution not used. Using native fullscreen resolution (%s)', [BuildResolutionString(Disp.w, Disp.h)]), 'SDL_SetVideoMode');
        Ini.SetResolution(Disp.w, Disp.h, false, true);
      end;

      X := Disp.w - Screen.w;
      Y := Disp.h - Screen.h;
    end;

    // if screen is out of the visisble desktop area, move it back
    // this likely happens when creating a Window bigger than the possible desktop size
    if (SDL_GetWindowFlags(screen) and SDL_WINDOW_FULLSCREEN = 0) and ((screen.x < 0) or (screen.Y < 0)) then
    begin
      // TODO: update SDL2
      //SDL_GetWindowBordersSize(screen, w, h, nil, nil);
      Log.LogStatus('Bad position for window. Re-position to (0,0)', 'SDL_SetVideoMode');
      SDL_SetWindowPosition(screen, x, y+x);
    end;
  end;

  //LoadOpenGL();
  glcontext := SDL_GL_CreateContext(Screen);
  InitOpenGL();

  //   ActivateRenderingContext(
  ReadExtensions;
  ReadImplementationProperties;
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
  //SDL_GL_SetSwapInterval(1); // VSYNC (currently Windows only)

  {// clear screen once window is being shown
  // Note: SwapBuffers uses RenderW/H, so they must be defined before
  glClearColor(1, 1, 1, 1);
  glClear(GL_COLOR_BUFFER_BIT);
  SwapBuffers;}
end;

function HasWindowState(Flag: integer): boolean;
begin
  Result := SDL_GetWindowFlags(screen) and Flag <> 0;
end;

function SwitchVideoMode(Mode: FullscreenModes): FullscreenModes;
var
  w,h: integer;
  Disp: TSDL_DisplayMode;
begin
  Result := CurrentWindowMode;
  Mode := CurrentWindowMode xor Mode;
  if Mode = CurrentWindowMode then Exit;

  if Mode >= Mode_Fullscreen then
  begin
    Mode := Mode and not Mode_Borderless;
    SDL_GetWindowDisplayMode(screen, @Disp);
    SDL_SetWindowFullscreen(screen, SDL_WINDOW_FULLSCREEN);

    Ini.GetResolutionFullscreen(Disp.W, Disp.H);
    SDL_SetWindowDisplayMode(screen, @Disp);
    SDL_SetWindowSize(screen, Disp.W, Disp.H);
  end
  else if Mode = Mode_Borderless then
  begin
    // calls window-resize event which updates screen sizes
    SDL_SetWindowFullscreen(screen, SDL_WINDOW_FULLSCREEN_DESKTOP);
  end
  else if Mode = Mode_Windowed then
  begin
    WindowModeDirty := true; // set window size dirty to restore old size after switching from fullscreen
    SDL_SetWindowFullscreen(screen, SDL_WINDOW_RESIZABLE); // calls window-resize event which updates screen sizes

    ScreenW := LastW; ScreenH := LastH;
    if not HasValidSize then Ini.GetResolution(ScreenW, ScreenH);
    SDL_SetWindowSize(screen, ScreenW, ScreenH);
  end;

  CurrentWindowMode := Mode;
  Result := CurrentWindowMode;
end;

procedure OnWindowMoved(x,y: integer);
begin
  if CurrentWindowMode <> Mode_Windowed then Exit;
  if (SDL_GetWindowFlags(screen) and (SDL_WINDOW_MINIMIZED or SDL_WINDOW_MAXIMIZED) <> 0) then Exit;

  if not WindowModeDirty then
  begin
    HasValidPosition := true;
    LastX := x;
    LastY := y;
  end;
end;

procedure OnWindowResized(w,h: integer);
begin
  if WindowModeDirty then
  begin
    if not HasWindowState(SDL_WINDOW_FULLSCREEN) then
    begin
      if not HasValidSize then
      begin
        LastH := ScreenH;
        LastW := ScreenW;
      end;

      // restoring from maximized state will additionally call a SDL_WINDOWEVENT_RESIZED event
      // we keep the dirty flag to still revert to the last none-maximized stored position and size
      if HasWindowState(SDL_WINDOW_MINIMIZED or SDL_WINDOW_MAXIMIZED) then SDL_RestoreWindow(screen)
      else WindowModeDirty := false;

      ScreenW := LastW; ScreenH := LastH; // override render size
      SDL_SetWindowPosition(screen, LastX, LastY);

      // if there wasn't a windowed mode before, center window
      if not HasValidPosition then
      begin
        SDL_SetWindowPosition(screen, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
      end;
    end;
  end
  else
  begin
    // override render size
    ScreenW := w; ScreenH := h;

    if not HasWindowState(SDL_WINDOW_MAXIMIZED or SDL_WINDOW_FULLSCREEN) then
    begin
      HasValidSize := true;
      LastW := w;
      LastH := h;
    end;
  end;

  if CurrentWindowMode = Mode_Fullscreen then
  begin
    Screen.W := ScreenW;
    Screen.H := ScreenH;
  end
  else
  begin
    SDL_SetWindowSize(screen, ScreenW, ScreenH);
  end;

  if assigned(Display) then
  begin
    Display.OnWindowResized(); // notify display window has changed
  end;
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
