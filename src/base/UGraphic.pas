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
  SDL,
  gl,
  glext,
  UTexture,
  TextGL,
  UConfig,
  ULog,
  SysUtils,
  ULyrics,
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
  UScreenOpen,
  UScreenAbout,
  UThemes,
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
  UMain,
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
  if (not Load_GL_version_1_2()) then
  begin
    Log.LogCritical('Failed loading OpenGL 1.2', 'UGraphic.Initialize3D');
  end;

  // Other extensions e.g. OpenGL 1.3-2.0 or Framebuffer-Object might be loaded here
  // ...
  //Load_GL_EXT_framebuffer_object();

  // PBO functions are loaded with VBO
  //PboSupported := Load_GL_ARB_pixel_buffer_object()
  //    and Load_GL_ARB_vertex_buffer_object();
  //Log.LogWarn('PBOSupported: ' + BoolToStr(PboSupported, true), 'LoadOpenGLExtensions');
  PboSupported := false;
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

  // load icon image (must be 32x32 for win32)
  Icon := LoadImage(ResourcesPath.Append(WINDOW_ICON));
  if (Icon <> nil) then
    SDL_WM_SetIcon(Icon, nil);

  SDL_WM_SetCaption(PChar(Title), nil);

  { center window }
  //SDL_putenv('SDL_VIDEO_WINDOW_POS=center');  //basisbit TODO
  { workaround for buggy Intel 3D driver on Linux }
  SDL_putenv('texture_tiling=false');

  SDL_WM_SetCaption(PChar(Title + ' - Initializing screen'), nil);
  InitializeScreen;

  SDL_WM_SetCaption(PChar(Title + ' - Initializing texturizer'), nil);
  Texture := TTextureUnit.Create;
  // FIXME: this does not seem to be correct as Limit.
  // Is the max. of either width or height.
  Texture.Limit :=1920; //currently, Full HD is all we want. switch to 64bit target before going further up

  //LoadTextures;
  SDL_WM_SetCaption(PChar(Title + ' - Initializing video modules'), nil);
  // Note: do not initialize video modules earlier. They might depend on some
  // SDL video functions or OpenGL extensions initialized in InitializeScreen()
  InitializeVideo();

  SDL_WM_SetCaption(PChar(Title + ' - Initializing 3D'), nil);
  Log.LogStatus('TDisplay.Create', 'UGraphic.Initialize3D');
  Display := TDisplay.Create;
  //Display.SetCursor;

  SDL_WM_SetCaption(PChar(Title + ' - Loading font textures'), nil);
  Log.LogStatus('Loading Font Textures', 'UGraphic.Initialize3D');
  LoadFontTextures();

  // Show the Loading Screen
  SDL_WM_SetCaption(PChar(Title + ' - Loading first screen'), nil);
  Log.LogStatus('Loading Loading Screen', 'UGraphic.Initialize3D');
  LoadLoadingScreen;

  // Covers Cache
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

  SDL_WM_SetCaption(PChar(Title + ' - Loading textures'), nil);
  Log.LogStatus(' Loading Textures', 'UGraphic.Initialize3D');
  LoadTextures;

  // now that we have something to display while loading,
  // start thread that loads the rest of ultrastar
  //Mutex   := SDL_CreateMutex;
  //SDL_UnLockMutex(Mutex);

  // does not work this way because the loading thread tries to access opengl.
  // See comment below
  //LoadingThread  := SDL_CreateThread(@LoadingThread, nil);

  // this would be run in the loadingthread
  SDL_WM_SetCaption(PChar(Title + ' - Loading screens'), nil);
  Log.LogStatus(' Loading Screens', 'UGraphic.Initialize3D');
  LoadScreens(Title);


  // TODO:
  // here should be a loop which
  // * draws the loading screen (form time to time)
  // * controlls the "process of the loading screen"
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
  // currently does not work this way
  // SDL_WaitThread(LoadingThread, I);
  // SDL_DestroyMutex(Mutex);
  SDL_WM_SetCaption(PChar(Title), nil);
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

procedure Finalize3D;
begin
  // TODO: finalize other stuff
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

  // Set minimum color component sizes
  // Note: do not request an alpha plane with SDL_GL_ALPHA_SIZE here as
  // some cards/implementations do not support them (SDL_SetVideoMode fails).
  // We do not the alpha plane anymore since offscreen rendering in back-buffer
  // was removed.
  SDL_GL_SetAttribute(SDL_GL_RED_SIZE,      5);
  SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE,    5);
  SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE,     5);

  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE,    16); // Z-Buffer depth
  //SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER,  1);


  // VSYNC works for windows only at the moment. SDL_GL_SWAP_CONTROL under
  // linux uses GLX_MESA_swap_control which is not supported by nvidea cards.
  // Maybe use glXSwapIntervalSGI(1) from the GLX_SGI_swap_control extension instead.
  SDL_GL_SetAttribute(SDL_GL_SWAP_CONTROL,  1); // VSYNC (currently Windows only)

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

  if (Params.Depth <> -1) then
    Depth := Params.Depth
  else
    Depth := Ini.Depth;

  Log.LogStatus('SDL_SetVideoMode', 'Initialize3D');

  // check whether to start in fullscreen or windowed mode.
  // The command-line parameters take precedence over the ini settings.
  Fullscreen := ((Ini.FullScreen = 1) or (Params.ScreenMode = scmFullscreen)) and
                not (Params.ScreenMode = scmWindowed);

  if Fullscreen then
  begin
    Log.LogStatus('SDL_SetVideoMode', 'Set Video Mode...   Full Screen');
    screen := SDL_SetVideoMode(W, H, (Depth+1) * 16, SDL_OPENGL or SDL_FULLSCREEN );
  end
  else
  begin
    Log.LogStatus('SDL_SetVideoMode', 'Set Video Mode...   Windowed');
    screen := SDL_SetVideoMode(W, H, 0, SDL_OPENGL or SDL_RESIZABLE);
  end;

  SDL_ShowCursor(0);

  if (screen = nil) then
  begin
    Log.LogCritical('SDL_SetVideoMode Failed', 'Initialize3D');
  end;

  LoadOpenGLExtensions();

  // define virtual (Render) and real (Screen) screen size
  RenderW := 800;
  RenderH := 600;
  ScreenW := W;
  ScreenH := H;

  // clear screen once window is being shown
  // Note: SwapBuffers uses RenderW/H, so they must be defined before
  glClearColor(1, 1, 1, 1);
  glClear(GL_COLOR_BUFFER_BIT);
  SwapBuffers;
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
{  ScreenLoading := TScreenLoading.Create;
    ScreenLoading.onShow;
  Display.CurrentScreen := @ScreenLoading;
    ScreenLoading.Draw;
  Display.Draw;
    SwapBuffers;
}
  SDL_WM_SetCaption(PChar(Title + ' - Loading ScreenMain & ScreenName'), nil);
  ScreenMain :=             TScreenMain.Create;
  ScreenName :=             TScreenName.Create;
  SDL_WM_SetCaption(PChar(Title + ' - Loading ScreenLevel & ScreenSong'), nil);
  ScreenLevel :=            TScreenLevel.Create;
  ScreenSong :=             TScreenSong.Create;
  SDL_WM_SetCaption(PChar(Title + ' - Loading ScreenSongMenu & ScreenJukebox'), nil);
  ScreenSongMenu :=             TScreenSongMenu.Create;
  ScreenJukebox :=             TScreenJukebox.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Jukebox', 3); Log.BenchmarkStart(3);
  ScreenJukeboxOptions :=   TScreenJukeboxOptions.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Jukebox Options', 3); Log.BenchmarkStart(3);
  ScreenJukeboxPlaylist :=   TScreenJukeboxPlaylist.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Jukebox Playlist', 3); Log.BenchmarkStart(3);
  ScreenTop5 :=             TScreenTop5.Create;
  SDL_WM_SetCaption(PChar(Title + ' - Loading ScreenOptions & ScreenOptionsGame'), nil);
  ScreenOptions :=          TScreenOptions.Create;
  ScreenOptionsGame :=      TScreenOptionsGame.Create;
  SDL_WM_SetCaption(PChar(Title + ' - Loading ScreenOptionsGraphics & ScreenOptionsSound'), nil);
  ScreenOptionsGraphics  :=  TScreenOptionsGraphics.Create;
  ScreenOptionsSound    :=     TScreenOptionsSound.Create;
  SDL_WM_SetCaption(PChar(Title + ' - Loading ScreenOptionsLyrics & ScreenOptionsThemes'), nil);
  ScreenOptionsLyrics   :=    TScreenOptionsLyrics.Create;
  ScreenOptionsThemes   :=    TScreenOptionsThemes.Create;
  SDL_WM_SetCaption(PChar(Title + ' - Loading ScreenOptionsRecord & ScreenOptionsAdvanced'), nil);
  ScreenOptionsRecord   :=    TScreenOptionsRecord.Create;
  SDL_WM_SetCaption(PChar(Title + ' - Loading ScreenOptionsAdvanced'), nil);
  ScreenOptionsAdvanced :=    TScreenOptionsAdvanced.Create;
  SDL_WM_SetCaption(PChar(Title + ' - Loading ScreenOptionsNetwork'), nil);  
  ScreenOptionsNetwork :=    TScreenOptionsNetwork.Create;
  SDL_WM_SetCaption(PChar(Title + ' - Loading ScreenOptionsWebCam'), nil);
  ScreenOptionsWebcam  :=    TScreenOptionsWebcam.Create;
  SDL_WM_SetCaption(PChar(Title + ' - Loading ScreenOptionsJukebox'), nil);
  ScreenOptionsJukebox :=    TScreenOptionsJukebox.Create;
  SDL_WM_SetCaption(PChar(Title + ' - Loading ScreenEditSub & ScreenEdit'), nil);
  ScreenEditSub :=          TScreenEditSub.Create;
  ScreenEdit :=             TScreenEdit.Create;
  SDL_WM_SetCaption(PChar(Title + ' - Loading ScreenOpen'), nil);
  ScreenOpen :=             TScreenOpen.Create;
  SDL_WM_SetCaption(PChar(Title + ' - Loading ScreenAbout'), nil);
  ScreenAbout :=             TScreenAbout.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen About', 3); Log.BenchmarkStart(3);
  //ScreenSingModi :=         TScreenSingModi.Create;
  //Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Sing with Modi support', 3); Log.BenchmarkStart(3);
  ScreenSongJumpto :=         TScreenSongJumpto.Create;
  SDL_WM_SetCaption(PChar(Title + ' - Loading ScreenPopupCheck & ScreenPopupError'), nil);
  ScreenPopupCheck := TScreenPopupCheck.Create;
  ScreenPopupError := TScreenPopupError.Create;
  SDL_WM_SetCaption(PChar(Title + ' - Loading ScreenPopupInfo & ScreenScoreX & ScreenPartyNewRound'), nil);
  ScreenPopupInfo := TScreenPopupInfo.Create;
  ScreenPopupInsertUser := TScreenPopupInsertUser.Create;
  ScreenPopupSendScore := TScreenPopupSendScore.Create;
  ScreenPopupScoreDownload := TScreenPopupScoreDownload.Create;
  ScreenPartyNewRound :=    TScreenPartyNewRound.Create;
  SDL_WM_SetCaption(PChar(Title + ' - Loading ScreenPartyScore & ScreenPartyWin'), nil);
  ScreenPartyScore :=       TScreenPartyScore.Create;
  ScreenPartyWin :=         TScreenPartyWin.Create;
  SDL_WM_SetCaption(PChar(Title + ' - Loading ScreenPartyOptions & ScreenPartyPlayer'), nil);
  ScreenPartyOptions :=     TScreenPartyOptions.Create;
  ScreenPartyPlayer :=      TScreenPartyPlayer.Create;
  SDL_WM_SetCaption(PChar(Title + ' - Loading ScreenPartyRounds & ScreenTournamentX & ScreenStatMain'), nil);
  ScreenPartyRounds :=      TScreenPartyRounds.Create;
  ScreenPartyTournamentRounds :=      TScreenPartyTournamentRounds.Create;
  ScreenPartyTournamentPlayer :=      TScreenPartyTournamentPlayer.Create;
  ScreenPartyTournamentOptions :=      TScreenPartyTournamentOptions.Create;
  ScreenPartyTournamentWin :=      TScreenPartyTournamentWin.Create;
  ScreenStatMain :=         TScreenStatMain.Create;
  SDL_WM_SetCaption(PChar(Title + ' - Loading ScreenStatDetail & ScreenCredits'), nil);
  ScreenStatDetail :=       TScreenStatDetail.Create;
  ScreenCredits    :=       TScreenCredits.Create;
  SDL_WM_SetCaption(PChar(Title), nil);
  //SDL_Delay(1);
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
  ScreenSing.Free;
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
