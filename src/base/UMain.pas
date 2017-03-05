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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/base/UMain.pas $
 * $Id: UMain.pas 2631 2010-09-05 15:26:08Z tobigun $
 *}

unit UMain;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  SDL;

var
  CheckMouseButton: boolean; // for checking mouse motion


procedure Main;
procedure MainLoop;
procedure CheckEvents;

type
  TMainThreadExecProc = procedure(Data: Pointer);

const
  MAINTHREAD_EXEC_EVENT = SDL_USEREVENT + 2;

{*
 * Delegates execution of procedure Proc to the main thread.
 * The Data pointer is passed to the procedure when it is called.
 * The main thread is notified by signaling a MAINTHREAD_EXEC_EVENT which
 * is handled in CheckEvents.
 * Note that Data must not be a pointer to local data. If you want to pass local
 * data, use Getmem() or New() or create a temporary object.
 *}
procedure MainThreadExec(Proc: TMainThreadExecProc; Data: Pointer);

implementation

uses
  Math,
  gl,
  UAvatars,
  UCatCovers,
  UCommandLine,
  UCommon,
  UConfig,
  UCovers,
  UDataBase,
  UDllManager,
  UDisplay,
  UGraphic,
  UGraphicClasses,
  UIni,
  UJoystick,
  ULanguage,
  ULog,
  UPathUtils,
  UPlaylist,
  UMusic,
  URecord,
  UBeatTimer,
  UPlatform,
  USkins,
  USongs,
  UThemes,
  UParty,
  UPartyTournament,
  ULuaCore,
  UHookableEvent,
  ULuaGl,
  ULuaLog,
  ULuaTexture,
  ULuaTextGL,
  ULuaParty,
  ULuaScreenSing,
  UTime,
  UWebcam;
  //UVideoAcinerella;

procedure Main;
var
  WindowTitle: string;
  BadPlayer: integer;
begin
  {$IFNDEF Debug}
  try
  {$ENDIF}
    WindowTitle := USDXVersionStr;

    Platform.Init;

    if Platform.TerminateIfAlreadyRunning(WindowTitle) then
      Exit;

    // fix floating-point exceptions (FPE)
    DisableFloatingPointExceptions();
    // fix the locale for string-to-float parsing in C-libs
    SetDefaultNumericLocale();

    // setup separators for parsing
    // Note: ThousandSeparator must be set because of a bug in TIniFile.ReadFloat
    ThousandSeparator := ',';
    DecimalSeparator := '.';

    //------------------------------
    // StartUp - create classes and load files
    //------------------------------

    // initialize SDL
    // without SDL_INIT_TIMER SDL_GetTicks() might return strange values
    SDL_Init(SDL_INIT_VIDEO or SDL_INIT_TIMER);
    SDL_EnableUnicode(1);

    // create luacore first so other classes can register their events
    LuaCore := TLuaCore.Create;


    USTime := TTime.Create;
    VideoBGTimer := TRelativeTimer.Create;

    // Commandline Parameter Parser
    Params := TCMDParams.Create;

    // Log + Benchmark
    Log := TLog.Create;
    Log.Title := WindowTitle;
    Log.FileOutputEnabled := not Params.NoLog;

    // Language
    Log.LogStatus('Initialize Paths', 'Initialization');
    InitializePaths;
    Log.SetLogFileLevel(50);
    Log.LogStatus('Load Language', 'Initialization');
    Language := TLanguage.Create;

    // add const values:
    Language.AddConst('US_VERSION', USDXVersionStr);

    // Skin
    Log.BenchmarkStart(1);
    Log.LogStatus('Loading Skin List', 'Initialization');
    Skin := TSkin.Create;

    Log.LogStatus('Loading Theme List', 'Initialization');
    Theme := TTheme.Create;
    Log.LogStatus('Website-Manager', 'Initialization');
    DLLMan := TDLLMan.Create;   // Load WebsiteList
    Log.LogStatus('DataBase System', 'Initialization');
    DataBase := TDataBaseSystem.Create;

    if (Params.ScoreFile.IsUnset) then
      DataBase.Init(Platform.GetGameUserPath.Append('Ultrastar.db'))
    else
      DataBase.Init(Params.ScoreFile);

    // Ini + Paths
    Log.LogStatus('Load Ini', 'Initialization');
    Ini := TIni.Create;
    Ini.Load;

    // it is possible that this is the first run, create a .ini file if neccessary
    Log.LogStatus('Write Ini', 'Initialization');
    Ini.Save;

    // Sound
    InitializeSound();

    // Lyrics-engine with media reference timer
    LyricsState := TLyricsState.Create();

    // Theme
    Theme.LoadTheme(Ini.Theme, Ini.Color);

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

    // Graphics
    Initialize3D(WindowTitle);

    // Playlist Manager
    Log.LogStatus('Playlist Manager', 'Initialization');
    PlaylistMan := TPlaylistManager.Create;

    // GoldenStarsTwinkleMod
    Log.LogStatus('Effect Manager', 'Initialization');
    GoldenRec := TEffectManager.Create;

    // Joypad
    if (Ini.Joypad = 1) or (Params.Joypad) then
    begin
      Log.LogStatus('Initialize Joystick', 'Initialization');
      Joy := TJoy.Create;
    end;
    
    // Webcam
    //Log.LogStatus('WebCam', 'Initialization');
    //Webcam := TWebcam.Create;
    UWebcam.IsEnabled:= false;

    // Lua
    Party := TPartyGame.Create;
    PartyTournament := TPartyTournament.Create;

    LuaCore.RegisterModule('Log', ULuaLog_Lib_f);
    LuaCore.RegisterModule('Gl', ULuaGl_Lib_f);
    LuaCore.RegisterModule('TextGl', ULuaTextGl_Lib_f);
    LuaCore.RegisterModule('Party', ULuaParty_Lib_f);
    LuaCore.RegisterModule('ScreenSing', ULuaScreenSing_Lib_f);

    LuaCore.LoadPlugins;

    LuaCore.DumpPlugins;

    { prepare software cursor }
    Display.SetCursor;

    {**
      * Start background music
      *}
    SoundLib.StartBgMusic;

    // check microphone settings, goto record options if they are corrupt
    BadPlayer := AudioInputProcessor.ValidateSettings;
    if (BadPlayer <> 0) then
    begin
      ScreenPopupError.ShowPopup(
          Format(Language.Translate('ERROR_PLAYER_DEVICE_ASSIGNMENT'),
          [BadPlayer]));
      Display.CurrentScreen^.FadeTo( @ScreenOptionsRecord );
    end;

    //------------------------------
    // Start Mainloop
    //------------------------------
    Log.LogStatus('Main Loop', 'Initialization');
    MainLoop;

  {$IFNDEF Debug}
  finally
  {$ENDIF}
    //------------------------------
    // Finish Application
    //------------------------------

    // TODO:
    // call an uninitialize routine for every initialize step
    // or at least use the corresponding Free methods

    Log.LogStatus('Finalize Media', 'Finalization');
    FinalizeMedia();

    Log.LogStatus('Uninitialize 3D', 'Finalization');
    Finalize3D();

    Log.LogStatus('Finalize SDL', 'Finalization');
    SDL_Quit();

    Log.LogStatus('Finalize Log', 'Finalization');
    Log.Free;
  {$IFNDEF Debug}
  end;
  {$ENDIF}
end;

procedure MainLoop;
const
  MAX_FPS = 60;
var
  Delay:            integer;
  TicksCurrent:     cardinal;
  TicksBeforeFrame: cardinal;
  Done:             boolean;
begin

  SDL_EnableKeyRepeat(125, 125);

  Done := false;

  CountSkipTime();  // JB - for some reason this seems to be needed when we use the SDL Timer functions.
  repeat
    TicksBeforeFrame := SDL_GetTicks;

    // joypad
    if (Ini.Joypad = 1) or (Params.Joypad) then
      Joy.Update;

    // keyboard events
    CheckEvents;

    // display
    Done := not Display.Draw;
    SwapBuffers;

    // FPS limiter
    TicksCurrent := SDL_GetTicks;
    Delay := 1000 div MAX_FPS - (TicksCurrent - TicksBeforeFrame);

    if Delay >= 1 then
      SDL_Delay(Delay); // dynamic, maximum is 100 fps

    CountSkipTime;

  until Done;

end;

procedure DoQuit;
begin
  // if question option is enabled then show exit popup
  if (Ini.AskbeforeDel = 1) then
  begin
    Display.CurrentScreen^.CheckFadeTo(nil,'MSG_QUIT_USDX');
  end
  else // if ask-for-exit is disabled then simply exit
  begin
    Display.Fade := 0;
    Display.NextScreenWithCheck := nil;
    Display.CheckOK := true;
  end;
end;

procedure CheckEvents;
var
  Event:     TSDL_event;
  mouseDown: boolean;
  mouseBtn:  integer;
  KeepGoing: boolean;
begin
  KeepGoing := true;
  while (SDL_PollEvent(@Event) <> 0) do
  begin
    case Event.type_ of
      SDL_QUITEV:
      begin
        Display.Fade := 0;
        Display.NextScreenWithCheck := nil;
        Display.CheckOK := true;
      end;

      SDL_MOUSEMOTION, SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP:
      begin
        if (Ini.Mouse > 0) then
        begin
          case Event.type_ of
            SDL_MOUSEBUTTONDOWN:
            begin
              mouseDown := true;
              mouseBtn  := Event.button.button;
              CheckMouseButton := true;
              if (mouseBtn = SDL_BUTTON_LEFT) or (mouseBtn = SDL_BUTTON_RIGHT) then
                Display.OnMouseButton(true);
            end;
            SDL_MOUSEBUTTONUP:
            begin
              mouseDown := false;
              mouseBtn  := Event.button.button;
              CheckMouseButton := false;
              if (mouseBtn = SDL_BUTTON_LEFT) or (mouseBtn = SDL_BUTTON_RIGHT) then
                Display.OnMouseButton(false);
            end;
            SDL_MOUSEMOTION:
            begin
              if (CheckMouseButton) then
                mouseDown := true
              else
                mouseDown := false;
              mouseBtn  := 0;
            end;
          end;

          Display.MoveCursor(Event.button.X * 800 * Screens / ScreenW,
                             Event.button.Y * 600 / ScreenH);

          if not Assigned(Display.NextScreen) then
          begin //drop input when changing screens
            if (ScreenPopupError <> nil) and (ScreenPopupError.Visible) then
              KeepGoing := ScreenPopupError.ParseMouse(mouseBtn, mouseDown, Event.button.x, Event.button.y)
            else if (ScreenPopupInfo <> nil) and (ScreenPopupInfo.Visible) then
              KeepGoing := ScreenPopupInfo.ParseMouse(mouseBtn, mouseDown, Event.button.x, Event.button.y)
            else if (ScreenPopupCheck <> nil) and (ScreenPopupCheck.Visible) then
              KeepGoing := ScreenPopupCheck.ParseMouse(mouseBtn, mouseDown, Event.button.x, Event.button.y)
            else if (ScreenPopupInsertUser <> nil) and (ScreenPopupInsertUser.Visible) then
              KeepGoing := ScreenPopupInsertUser.ParseMouse(mouseBtn, mouseDown, Event.button.x, Event.button.y)
            else if (ScreenPopupSendScore <> nil) and (ScreenPopupSendScore.Visible) then
              KeepGoing := ScreenPopupSendScore.ParseMouse(mouseBtn, mouseDown, Event.button.x, Event.button.y)
            else if (ScreenPopupScoreDownload <> nil) and (ScreenPopupScoreDownload.Visible) then
              KeepGoing := ScreenPopupScoreDownload.ParseMouse(mouseBtn, mouseDown, Event.button.x, Event.button.y)
            else
            begin
              KeepGoing := Display.CurrentScreen^.ParseMouse(mouseBtn, mouseDown, Event.button.x, Event.button.y);

              // if screen wants to exit
              if not KeepGoing then
                DoQuit;
            end;
          end;
        end;
      end;
      SDL_VIDEORESIZE:
      begin
        ScreenW := Event.resize.w;
        ScreenH := Event.resize.h;
        // Note: do NOT call SDL_SetVideoMode on Windows and MacOSX here.
        // This would create a new OpenGL render-context and all texture data
        // would be invalidated.
        // On Linux the mode MUST be reset, otherwise graphics will be corrupted.
        // Update: It seems to work now without creating a new OpenGL context. At least
        // with Win7 and SDL 1.2.14. Maybe it generally works now with SDL 1.2.14 and we
        // can switch it on for windows.
        // Important: Unless SDL_SetVideoMode() is called (it is not on Windows), Screen.w
        // and Screen.h are not valid after a resize and still contain the old size. Use
        // ScreenW and ScreenH instead.
        //////
        if boolean( Ini.FullScreen ) then
        begin
        {$IF Defined(Linux) or Defined(FreeBSD)}
          SDL_SetVideoMode(ScreenW, ScreenH, (Ini.Depth+1) * 16, SDL_OPENGL or SDL_FULLSCREEN);
        {$ELSE}
        Screen.W := ScreenW;
        Screen.H := ScreenH;
        {$IFEND}
        end
        else
          SDL_SetVideoMode(ScreenW, ScreenH, (Ini.Depth+1) * 16, SDL_OPENGL or SDL_RESIZABLE);
      end;
      SDL_KEYDOWN:
        begin
          // translate CTRL-A (ASCII 1) - CTRL-Z (ASCII 26) to correct charcodes.
          // keysyms (SDLK_A, ...) could be used instead but they ignore the
          // current key mapping (if 'a' is pressed on a French keyboard the
          // .unicode field will be 'a' and .sym SDLK_Q).
          // IMPORTANT: if CTRL is pressed with a key different than 'A'-'Z' SDL
          // will set .unicode to 0. There is no possibility to obtain a
          // translated charcode. Use keysyms instead.
          //if (Event.key.keysym.unicode in [1 .. 26]) then
          //  Event.key.keysym.unicode := Ord('A') + Event.key.keysym.unicode - 1;

          // remap the "keypad enter" key to the "standard enter" key
          if (Event.key.keysym.sym = SDLK_KP_ENTER) then
            Event.key.keysym.sym := SDLK_RETURN;

          if not Assigned(Display.NextScreen) then
          begin //drop input when changing screens
            { to-do : F11 was used for fullscreen toggle, too here
                      but we also use the key in screenname and some other
                      screens. It is droped although fullscreen toggle doesn't
                      even work on windows.
                      should we add (Event.key.keysym.sym = SDLK_F11) here
                      anyway? }
            if ((Event.key.keysym.sym = SDLK_RETURN) and
               ((Event.key.keysym.modifier and KMOD_ALT) <> 0)) then // toggle full screen
            begin
              Ini.FullScreen := integer( not boolean( Ini.FullScreen ) );

              // FIXME: SDL_SetVideoMode creates a new OpenGL RC so we have to
              // reload all texture data (-> whitescreen bug).
              // Only Linux and FreeBSD are able to handle screen-switching this way.
              {$IF Defined(Linux) or Defined(FreeBSD)}
              if boolean( Ini.FullScreen ) then
              begin
                SDL_SetVideoMode(ScreenW, ScreenH, (Ini.Depth+1) * 16, SDL_OPENGL or SDL_FULLSCREEN);
              end
              else
              begin
                SDL_SetVideoMode(ScreenW, ScreenH, (Ini.Depth+1) * 16, SDL_OPENGL or SDL_RESIZABLE);
              end;

              Display.SetCursor;

              glViewPort(0, 0, ScreenW, ScreenH);
              {$IFEND}
            end
            // if print is pressed -> make screenshot and save to screenshot path
            else if (Event.key.keysym.sym = SDLK_SYSREQ) or (Event.key.keysym.sym = SDLK_PRINT) then
              Display.SaveScreenShot
            // if there is a visible popup then let it handle input instead of underlying screen
            // shoud be done in a way to be sure the topmost popup has preference (maybe error, then check)
            else if (ScreenPopupError <> nil) and (ScreenPopupError.Visible) then
              KeepGoing := ScreenPopupError.ParseInput(Event.key.keysym.sym, Event.key.keysym.unicode, true)
            else if (ScreenPopupInfo <> nil) and (ScreenPopupInfo.Visible) then
              KeepGoing := ScreenPopupInfo.ParseInput(Event.key.keysym.sym, Event.key.keysym.unicode, true)
            else if (ScreenPopupCheck <> nil) and (ScreenPopupCheck.Visible) then
              KeepGoing := ScreenPopupCheck.ParseInput(Event.key.keysym.sym, Event.key.keysym.unicode, true)
            else if (ScreenPopupInsertUser <> nil) and (ScreenPopupInsertUser.Visible) then
              KeepGoing := ScreenPopupInsertUser.ParseInput(Event.key.keysym.sym, Event.key.keysym.unicode, true)
            else if (ScreenPopupSendScore <> nil) and (ScreenPopupSendScore.Visible) then
              KeepGoing := ScreenPopupSendScore.ParseInput(Event.key.keysym.sym, Event.key.keysym.unicode, true)
            else if (ScreenPopupScoreDownload <> nil) and (ScreenPopupScoreDownload.Visible) then
              KeepGoing := ScreenPopupScoreDownload.ParseInput(Event.key.keysym.sym, Event.key.keysym.unicode, true)
            else
            begin
              // check if screen wants to exit
              KeepGoing := Display.ParseInput(Event.key.keysym.sym, Event.key.keysym.unicode, true);

              // if screen wants to exit
              if not KeepGoing then
                DoQuit;

            end;
          end;
        end;
      SDL_JOYAXISMOTION:
        begin
          // not implemented
        end;
      SDL_JOYBUTTONDOWN:
        begin
          // not implemented
        end;
      MAINTHREAD_EXEC_EVENT:
        with Event.user do
        begin
          TMainThreadExecProc(data1)(data2);
        end;
    end; // case
  end; // while
end;

procedure MainThreadExec(Proc: TMainThreadExecProc; Data: Pointer);
var
  Event: TSDL_Event;
begin
  with Event.user do
  begin
    type_ := MAINTHREAD_EXEC_EVENT;
    code  := 0;     // not used at the moment
    data1 := @Proc;
    data2 := Data;
  end;
  SDL_PushEvent(@Event);
end;

end.
