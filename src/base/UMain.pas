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
  SDL2;

var
  CheckMouseButton: boolean; // for checking mouse motion
  MAX_FPS: Byte; // 0 to 255 is enough


procedure Main;
procedure MainLoop;
procedure CheckEvents;
procedure StartTextInput;
procedure StopTextInput;
procedure SetTextInput(enabled: boolean);

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
  math,
  dglOpenGL,
  UCommandLine,
  UCommon,
  UConfig,
  UDataBase,
  UDllManager,
  UDisplay,
  UGraphic,
  UGraphicClasses,
  UHelp,
  UKeyBindings,
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
  UThemes,
  UParty,
  UPartyTournament,
  ULuaCore,
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
    SetMultiByteConversionCodePage(CP_UTF8);
    WindowTitle := USDXVersionStr;

    Platform.Init;
    Log.Title := WindowTitle;
    Log.FileOutputEnabled := true;
    
    // Commandline Parameter Parser
    Params := TCMDParams.Create;

    if Platform.TerminateIfAlreadyRunning(WindowTitle) then
      Exit;

    // fix floating-point exceptions (FPE)
    DisableFloatingPointExceptions();
    // fix the locale for string-to-float parsing in C-libs
    SetDefaultNumericLocale();

    // setup separators for parsing
    // Note: ThousandSeparator must be set because of a bug in TIniFile.ReadFloat
    DefaultFormatSettings.ThousandSeparator := ',';
    DefaultFormatSettings.DecimalSeparator := '.';

    //------------------------------
    // StartUp - create classes and load files
    //------------------------------

    // initialize SDL
    // without SDL_INIT_TIMER SDL_GetTicks() might return strange values
    SDL_SetHint(SDL_HINT_WINDOWS_DISABLE_THREAD_NAMING, '1');
    SDL_Init(SDL_INIT_VIDEO or SDL_INIT_TIMER);
    //SDL_EnableUnicode(1);  //not necessary in SDL2 any more

    // create luacore first so other classes can register their events
    LuaCore := TLuaCore.Create;


    USTime := TTime.Create;
    VideoBGTimer := TRelativeTimer.Create;

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

    // Help
    Log.LogStatus('Load Help', 'Initialization');
    Help := THelp.Create;

    // it is possible that this is the first run, create a .ini file if neccessary
    Log.LogStatus('Write Ini', 'Initialization');
    Ini.Save;

    // Theme
    Theme.LoadTheme(Ini.Theme, Ini.Color);

    // Sound
    InitializeSound();

    // Lyrics-engine with media reference timer
    LyricsState := TLyricsState.Create();

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
      InitializeJoystick;
    end;
    
    // Webcam
    //Log.LogStatus('WebCam', 'Initialization');
    Webcam := TWebcam.Create;

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
    BadPlayer := AudioInputProcessor.CheckPlayersConfig(1);
    if (BadPlayer <> 0) then
    begin
      ScreenPopupError.ShowPopup(
          Format(Language.Translate('ERROR_PLAYER_NO_DEVICE_ASSIGNMENT'),
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

    Log.LogStatus('Closing DB file', 'Finalization');
    if (DataBase <> nil) then
    begin
         DataBase.Destroy();
    end;

    Log.LogStatus('Finalize Media', 'Finalization');
    FinalizeMedia();

    FinalizeJoyStick;

    Log.LogStatus('Uninitialize 3D', 'Finalization');
    Finalize3D();

    Log.LogStatus('Finalize SDL', 'Finalization');
    SDL_Quit();

    Log.LogStatus('Finalize Log', 'Finalization');
  {$IFNDEF Debug}
  end;
  {$ENDIF}
end;

procedure StartTextInput;
begin
  SDL_StartTextInput;
end;

procedure StopTextInput;
begin
  SDL_StopTextInput;
end;

procedure SetTextInput(enabled: boolean);
begin
  if enabled then StartTextInput else StopTextInput;
end;

procedure MainLoop;
var
  Delay:            integer;
  TicksCurrent:     cardinal;
  TicksBeforeFrame: cardinal;
  Done:             boolean;
  Report: string;
  I,J: Integer;
begin
  Max_FPS := Ini.MaxFramerateGet;
  // need to explicitly stop this because it appears to be started by default
  SDL_StopTextInput;
  Done := false;
  J := 1;
  CountSkipTime();
  repeat
    try
    begin
      TicksBeforeFrame := SDL_GetTicks;

      // keyboard/mouse/joystick events
      CheckEvents;

      // display
      Done := not Display.Draw;
      SwapBuffers;

      // FPS limiter
      TicksCurrent := SDL_GetTicks;
      Delay := 1000 div MAX_FPS - (TicksCurrent - TicksBeforeFrame);

      if Delay >= 1 then
        SDL_Delay(Delay);

      CountSkipTime;
      J:=1;
    end
    except
      on E : Exception do
      begin
        J := J+1;
        if J > 1 then
        begin
          Report := 'Sorry, an error ocurred! Please report this error to the game-developers. Also check the Error.log file in the game folder.' + LineEnding +
            'Stacktrace:' + LineEnding;
          if E <> nil then begin
            Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
            'Message: ' + E.Message + LineEnding;
          end;
          Report := Report + BackTraceStrFunc(ExceptAddr);
          for I := 0 to ExceptFrameCount - 1 do
            Report := Report + LineEnding + BackTraceStrFunc(ExceptFrames[I]);
          ShowMessage(Report);
          done := true;
        end
        else
        begin
          done := false;
        end;
      end;
    end;
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
  SimEvent:  TSDL_event;
  KeyCharUnicode: UCS4Char;
  SimKey: TCombinedKey;
  RawKey: cardinal;
  RawCombined: TCombinedKey;
  ModState: LongWord;
  s1: UTF8String;
  mouseDown: boolean;
  mouseBtn:  integer;
  mouseX, mouseY: PInt;
  KeepGoing: boolean;
  SuppressKey: boolean;
  UpdateMouse: boolean;
  MouseHandling: boolean;
begin
  KeepGoing := true;
  SuppressKey := false;
  while (SDL_PollEvent(@Event) <> 0) do
  begin
    case Event.type_ of
      SDL_QUITEV:
      begin
        Display.Fade := 0;
        Display.NextScreenWithCheck := nil;
        Display.CheckOK := true;
      end;

      SDL_MOUSEMOTION, SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP, SDL_MOUSEWHEEL:
      begin
        MouseHandling := (Ini.Mouse > 0) or
          ((ScreenPopupHelp <> nil) and ScreenPopupHelp.Visible);
        if MouseHandling then
        begin
          UpdateMouse := (Ini.Mouse > 0);
          case Event.type_ of
            SDL_MOUSEBUTTONDOWN:
            begin
              mouseDown := true;
              mouseBtn  := Event.button.button;
              CheckMouseButton := true;
              if (Ini.Mouse > 0) and ((mouseBtn = SDL_BUTTON_LEFT) or (mouseBtn = SDL_BUTTON_RIGHT)) then
                Display.OnMouseButton(true);
            end;
            SDL_MOUSEBUTTONUP:
            begin
              mouseDown := false;
              mouseBtn  := Event.button.button;
              CheckMouseButton := false;
              if (Ini.Mouse > 0) and ((mouseBtn = SDL_BUTTON_LEFT) or (mouseBtn = SDL_BUTTON_RIGHT)) then
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
            SDL_MOUSEWHEEL:
            begin
              UpdateMouse := false;
              mouseDown   := (Event.wheel.y <> 0);
              mouseBtn    := SDL_BUTTON_WHEELDOWN;
              if (Event.wheel.y > 0) then mouseBtn := SDL_BUTTON_WHEELUP;

              // some menu buttons require proper mouse location for trying to
              // react to mouse wheel navigation simulation (see UMenu.ParseMouse)
              SDL_GetMouseState(@mouseX, @mouseY);
              Event.button.x := longint(mouseX);
              Event.button.y := longint(mouseY);
            end;
          end;

          if UpdateMouse and (Ini.Mouse > 0) then
          begin
            // used to update mouse coords and allow the relative mouse emulated by joystick axis motion
            if assigned(Joy) then Joy.OnMouseMove(EnsureRange(Event.button.X, 0, 799),
                                                  EnsureRange(Event.button.Y, 0,599));

            Display.MoveCursor(Event.button.X * 800 * Screens / ScreenW,
                               Event.button.Y * 600 / ScreenH);
          end;

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
            else if (ScreenPopupHelp <> nil) and (ScreenPopupHelp.Visible) then
              KeepGoing := ScreenPopupHelp.ParseMouse(mouseBtn, mouseDown, Event.button.x, Event.button.y)
            else
            begin
              KeepGoing := Display.ParseMouse(mouseBtn, mouseDown, Event.button.x, Event.button.y);

              // if screen wants to exit
              if not KeepGoing then
                DoQuit;
            end;
          end;
        end;
      end;
      SDL_WINDOWEVENT://SDL_WINDOWEVENT_RESIZED:
      begin
        case Event.window.event of
          SDL_WINDOWEVENT_MOVED: OnWindowMoved(Event.window.data1, Event.window.data2);
          SDL_WINDOWEVENT_RESIZED: OnWindowResized(Event.window.data1, Event.window.data2);
        end
      end;
      SDL_KEYDOWN, SDL_TEXTINPUT:
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

          // toggle in-game console if allowed
          if boolean(Ini.Debug) and ((Event.key.keysym.sym = SInt32('~')) or (Event.key.keysym.sym = SDLK_CARET)) then
          begin
            Display.ToggleConsole;
          end;

          if Event.key.keysym.sym = SDLK_RETURN then
          begin
            if (SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT) = KMOD_LALT) then
            begin
              if SwitchVideoMode(Mode_Fullscreen) = Mode_Fullscreen then Ini.FullScreen := 1
              else Ini.FullScreen := 0;
              Ini.Save();

              Break;
            end;
          end;

          // remap the "keypad enter" key to the "standard enter" key
          if (Event.key.keysym.sym = SDLK_KP_ENTER) then Event.key.keysym.sym := SDLK_RETURN;

          if not Assigned(Display.NextScreen) then
          begin //drop input when changing screens
            if (Event.type_ = SDL_KEYDOWN) and
               (ScreenPopupHelp <> nil) and ScreenPopupHelp.Visible and
               ScreenPopupHelp.IsCapturing then
            begin
              if ScreenPopupHelp.HandleCapturedKey(
                   MakeCombinedKey(word(SDL_GetModState), cardinal(Event.key.keysym.sym))) then
                Continue;
            end;

            KeyCharUnicode:=0;
            if (Event.type_ = SDL_TEXTINPUT) and (Event.text.text <> '') then
            try
              KeyCharUnicode:=UnicodeStringToUCS4String(UnicodeString(UTF8String(Event.text.text)))[0];
              //KeyCharUnicode:=UnicodeStringToUCS4String(UnicodeString(Event.key.keysym.unicode))[1];//Event.text.text)[0];
            except
            end;

            RawKey := cardinal(Event.key.keysym.sym);
            ModState := SDL_GetModState;
            RawCombined := MakeCombinedKey(word(ModState), RawKey);

            if RawCombined = IGNORE_KEY then
              Continue;

            if Event.type_ = SDL_KEYDOWN then
            begin
              if Display <> nil then
                SimKey := Display.TranslateKeyForActiveScreen(RawCombined)
              else
                SimKey := RawCombined;
            end
            else
              SimKey := RawCombined;

            SimKey := NormalizeCombinedKey(SimKey);
            if SimKey = IGNORE_KEY then
              Continue;

            // if print is pressed -> make screenshot and save to screenshot path
            if (CombinedKeyToKeyCode(SimKey) = SDLK_SYSREQ) or
              (CombinedKeyToKeyCode(SimKey) = SDLK_PRINTSCREEN) then
              Display.SaveScreenShot
            // if there is a visible popup then let it handle input instead of underlying screen
            // shoud be done in a way to be sure the topmost popup has preference (maybe error, then check)
            else if (ScreenPopupError <> nil) and (ScreenPopupError.Visible) then
              KeepGoing := ScreenPopupError.ParseInput(SimKey, KeyCharUnicode, true, 0)
            else if (ScreenPopupInfo <> nil) and (ScreenPopupInfo.Visible) then
              KeepGoing := ScreenPopupInfo.ParseInput(SimKey, KeyCharUnicode, true, 0)
            else if (ScreenPopupCheck <> nil) and (ScreenPopupCheck.Visible) then
              KeepGoing := ScreenPopupCheck.ParseInput(SimKey, KeyCharUnicode, true, 0)
            else if (ScreenPopupInsertUser <> nil) and (ScreenPopupInsertUser.Visible) then
              KeepGoing := ScreenPopupInsertUser.ParseInput(SimKey, KeyCharUnicode, true, 0)
            else if (ScreenPopupSendScore <> nil) and (ScreenPopupSendScore.Visible) then
              KeepGoing := ScreenPopupSendScore.ParseInput(SimKey, KeyCharUnicode, true, 0)
            else if (ScreenPopupScoreDownload <> nil) and (ScreenPopupScoreDownload.Visible) then
              KeepGoing := ScreenPopupScoreDownload.ParseInput(SimKey, KeyCharUnicode, true, 0)
            else if (ScreenPopupHelp <> nil) and (ScreenPopupHelp.Visible) then
              KeepGoing := ScreenPopupHelp.ParseInput(SimKey, KeyCharUnicode, true, 0)
            else if (Display.ShouldHandleInput(QWord(SimKey), KeyCharUnicode, true, SuppressKey)) then
            begin
              // check if screen wants to exit
              KeepGoing := Display.ParseInput(SimKey, KeyCharUnicode, true, 0);

              // if screen wants to exit
              if not KeepGoing then
                DoQuit;

            end;

            if (not SuppressKey and (Event.key.keysym.sym = SDLK_F11)) then // toggle full screen
            begin
              if (CurrentWindowMode <> Mode_Fullscreen) then // only switch borderless fullscreen in windowed mode
              begin
                if SwitchVideoMode(Mode_Borderless) = Mode_Borderless then
                begin
                  Ini.FullScreen := 2;
                end
                else
                begin
                  Ini.FullScreen := 0;
                end;
                Ini.Save();
              end;

              //Display.SetCursor;

              //glViewPort(0, 0, ScreenW, ScreenH);
            end;
          end;
        end;
      SDL_CONTROLLERDEVICEADDED, SDL_CONTROLLERDEVICEREMOVED,
      SDL_CONTROLLERBUTTONDOWN, SDL_CONTROLLERBUTTONUP, SDL_CONTROLLERAXISMOTION,
      SDL_JOYAXISMOTION, SDL_JOYBALLMOTION, SDL_JOYBUTTONDOWN, SDL_JOYBUTTONUP,
      SDL_JOYDEVICEADDED, SDL_JOYDEVICEREMOVED, SDL_JOYHATMOTION:
        begin
          OnJoystickPollEvent(Event);
        end;
      MAINTHREAD_EXEC_EVENT:
        with Event.user do
        begin
          TMainThreadExecProc(data1)(data2);
        end;

      otherwise
      begin
        ;
      end;
    end; // case
  end; // while

  if Display.NeedsCursorUpdate() then
  begin


    // push a generated event onto the queue in order to simulate a mouse movement
    // the next tick will poll the motion event and handle it just like a real input
    SDL_GetMouseState(@mouseX, @mouseY);
    SimEvent.user.type_ := SDL_MOUSEMOTION;
    SimEvent.button.x := longint(mouseX);
    SimEvent.button.y := longint(mouseY);
    SDL_PushEvent(@SimEvent);
  end;
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
