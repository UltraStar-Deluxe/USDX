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
  PMainThreadExecData = ^TMainThreadExecData;
  TMainThreadExecData = record
    Proc: TMainThreadExecProc;
    Data: Pointer;
  end;

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
  Classes,
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
  UIni,
  UJoystick,
  ULanguage,
  ULog,
  UPathUtils,
  UPlaylist,
  UMusic,
  URecord,
  URemoteBridgeIPC,
  URemoteBridgeProcess,
  URemoteControl,
  UBeatTimer,
  UPlatform,
  USkins,
  USongs,
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

type
  PRemoteCommandData = ^TRemoteCommandData;
  TRemoteCommandData = record
    PlayerIndex: integer;
    PlayerId: UTF8String;
    Command: TRemoteControlCommand;
    CommandId: integer;
    ArgsText: UTF8String;
    ArgsIndex: integer;
    ArgsItemIndex: integer;
    EnqueuedTick: cardinal;
  end;

procedure EnqueueRemoteCommand(Data: Pointer); forward;
procedure RemoteBridgeCommandReceived(PlayerIndex: integer; const PlayerId: UTF8String;
  Command: TRemoteControlCommand; CommandId: integer; const ArgsText: UTF8String;
  ArgsIndex: integer; ArgsItemIndex: integer); forward;
procedure DoQuit; forward;

const
  REMOTE_COMMAND_MAX_QUEUE = 16;
  REMOTE_COMMAND_SPACING_MS = 25;
  REMOTE_COMMAND_MAX_AGE_MS = 750;

var
  RemoteCommandQueue: array of PRemoteCommandData;
  LastRemoteCommandTick: cardinal = 0;

procedure Main;
var
  WindowTitle: string;
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
    if (Length(ILanguage) > 0) then
    begin
      if (Ini.Language >= 0) and (Ini.Language < Length(ILanguage)) then
        Help.ChangeLanguage(ILanguage[Ini.Language])
      else
        Help.ChangeLanguage(ILanguage[0]);
    end;

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

    if (Ini.RemoteBridgeEnabled = 1) then
    begin
      Log.LogStatus('Remote Bridge IPC', 'Initialization');
      RemoteBridgeIPC.Host := Ini.RemoteBridgeIpcHost;
      RemoteBridgeIPC.Port := Ini.RemoteBridgeIpcPort;
      RemoteBridgeProcess.Start;
      if (RemoteBridgeProcess.LastError <> '') then
        Log.LogWarn(RemoteBridgeProcess.LastError, 'Remote Bridge Process');
      RemoteBridgeIPC.OnCommand := RemoteBridgeCommandReceived;
      RemoteBridgeIPC.Start;
      RemoteBridgeIPC.SendGameState('boot', 0, 0);
    end;

    {**
      * Start background music
      *}
    SoundLib.StartBgMusic;

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

    if (Ini.RemoteBridgeEnabled = 1) then
    begin
      Log.LogStatus('Remote Bridge IPC', 'Finalization');
      RemoteBridgeProcess.Stop;
      RemoteBridgeIPC.Stop;
    end;

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

function DispatchRemoteCommand(Command: TRemoteControlCommand; const ArgsText: UTF8String;
  ArgsIndex: integer; ArgsItemIndex: integer): boolean;
var
  PlaylistItemIndex: integer;
  Key: cardinal;

  function AddSongIdListToPlaylist(const SongIdsText: UTF8String; PlaylistIndex: integer): boolean;
  var
    Parts: TStringList;
    I: integer;
    SongId: integer;
  begin
    Result := false;
    if (PlaylistIndex < 0) or (PlaylistIndex > High(PlaylistMan.Playlists)) then
      Exit;

    Parts := TStringList.Create;
    try
      Parts.Delimiter := ',';
      Parts.StrictDelimiter := true;
      Parts.DelimitedText := string(SongIdsText);
      for I := 0 to Parts.Count - 1 do
      begin
        SongId := StrToIntDef(Trim(Parts[I]), -1);
        if (SongId >= 0) and (SongId <= High(CatSongs.Song)) and
           Assigned(CatSongs.Song[SongId]) and (not CatSongs.Song[SongId].Main) then
        begin
          if PlaylistMan.GetIndexbySongID(SongId, PlaylistIndex) < 0 then
            PlaylistMan.AddItem(SongId, PlaylistIndex);
          Result := true;
        end;
      end;
    finally
      Parts.Free;
    end;
  end;
begin
  Result := true;
  Key := 0;

  case Command of
    rccMenuUp:     Key := SDLK_UP;
    rccMenuDown:   Key := SDLK_DOWN;
    rccMenuLeft:   Key := SDLK_LEFT;
    rccMenuRight:  Key := SDLK_RIGHT;
    rccMenuSelect: Key := SDLK_RETURN;
    rccMenuBack:   Key := SDLK_ESCAPE;
    rccGamePause:
      begin
        Result := (Display.CurrentScreen = @ScreenSing);
        if Result and (not ScreenSing.Paused) then
          ScreenSing.Pause;
        Exit;
      end;
    rccGameResume:
      begin
        Result := (Display.CurrentScreen = @ScreenSing);
        if Result and ScreenSing.Paused then
          ScreenSing.Pause;
        Exit;
      end;
    rccSongSkipIntro:
      begin
        Result := (Display.CurrentScreen = @ScreenSing);
        if Result then
          ScreenSing.ParseInput(SDLK_S, 0, true);
        Exit;
      end;
    rccSongRestart:
      begin
        Result := (Display.CurrentScreen = @ScreenSing);
        if Result then
          ScreenSing.ParseInput(SDLK_R, 0, true);
        Exit;
      end;
    rccSongMenu:
      begin
        Result := (Display.CurrentScreen = @ScreenSong);
        if Result then
          Key := SDLK_M
        else
          Exit;
      end;
    rccSongPlaylistMenu:
      begin
        Result := (Display.CurrentScreen = @ScreenSong);
        if Result then
          Key := SDLK_P
        else
          Exit;
      end;
    rccSongJumpTo:
      begin
        Result := (Display.CurrentScreen = @ScreenSong);
        if Result then
          Key := SDLK_J
        else
          Exit;
      end;
    rccSongRandom:
      begin
        Result := (Display.CurrentScreen = @ScreenSong);
        if Result then
          Key := SDLK_R
        else
          Exit;
      end;
    rccSongSelect:
      begin
        Result := (Display.CurrentScreen = @ScreenSong) and (ArgsIndex >= 0) and
          (ArgsIndex <= High(CatSongs.Song)) and CatSongs.Song[ArgsIndex].Visible;
        if Result then
        begin
          ScreenSong.SkipTo(CatSongs.VisibleIndex(ArgsIndex), ArgsIndex, CatSongs.VisibleSongs);
          ScreenSong.SetScrollRefresh;
        end;
        Exit;
      end;
    rccSongStart:
      begin
        Result := (Display.CurrentScreen = @ScreenSong) and (ScreenSong.Interaction >= 0) and
          (ScreenSong.Interaction <= High(CatSongs.Song)) and (not CatSongs.Song[ScreenSong.Interaction].Main);
        if Result then
          ScreenSong.StartSong;
        Exit;
      end;
    rccPreviewStart:
      begin
        Result := (Display.CurrentScreen = @ScreenSong);
        if Result then
          ScreenSong.StartPreview;
        Exit;
      end;
    rccPreviewStop:
      begin
        Result := (Display.CurrentScreen = @ScreenSong);
        if Result then
          ScreenSong.StopPreview;
        Exit;
      end;
    rccSearchSetText:
      begin
        Result := (Display.CurrentScreen = @ScreenSong);
        if Result then
          ScreenSongJumpto.SetSearchText(ArgsText);
        Exit;
      end;
    rccPlaylistLoad:
      begin
        Result := (Display.CurrentScreen = @ScreenSong) and (ArgsIndex >= 0) and
          (ArgsIndex <= High(PlaylistMan.Playlists));
        if Result then
        begin
          PlaylistMan.ReloadPlaylist(ArgsIndex);
          PlaylistMan.SetPlaylist(ArgsIndex);
          ScreenSong.SetScrollRefresh;
          ScreenSong.SendRemoteSongSelectState(true);
        end;
        Exit;
      end;
    rccPlaylistCreate:
      begin
        Result := (Display.CurrentScreen = @ScreenSong) and (ArgsText <> '');
        if Result then
        begin
          PlaylistMan.AddPlaylist(ArgsText);
          ScreenSong.SendRemoteSongSelectState(true);
        end;
        Exit;
      end;
    rccPlaylistDelete:
      begin
        Result := (Display.CurrentScreen = @ScreenSong) and (ArgsIndex >= 0) and
          (ArgsIndex <= High(PlaylistMan.Playlists));
        if Result then
        begin
          PlaylistMan.DelPlaylist(ArgsIndex);
          ScreenSong.SendRemoteSongSelectState(true);
        end;
        Exit;
      end;
    rccPlaylistAddSelected:
      begin
        Result := (Display.CurrentScreen = @ScreenSong) and (ArgsIndex >= 0) and
          (ArgsIndex <= High(PlaylistMan.Playlists));
        if Result then
        begin
          PlaylistMan.AddItem(ScreenSong.Interaction, ArgsIndex);
          ScreenSong.SendRemoteSongSelectState(true);
        end;
        Exit;
      end;
    rccPlaylistRemoveSelected:
      begin
        Result := (Display.CurrentScreen = @ScreenSong) and (ArgsIndex >= 0) and
          (ArgsIndex <= High(PlaylistMan.Playlists));
        if Result then
        begin
          PlaylistItemIndex := PlaylistMan.GetIndexbySongID(ScreenSong.Interaction, ArgsIndex);
          Result := PlaylistItemIndex >= 0;
          if Result then
          begin
            PlaylistMan.DelItem(PlaylistItemIndex, ArgsIndex);
            ScreenSong.SendRemoteSongSelectState(true);
          end;
        end;
        Exit;
      end;
    rccPlaylistAddSongs:
      begin
        Result := (Display.CurrentScreen = @ScreenSong) and (ArgsIndex >= 0) and
          (ArgsIndex <= High(PlaylistMan.Playlists));
        if Result then
        begin
          Result := AddSongIdListToPlaylist(ArgsText, ArgsIndex);
          if Result then
            ScreenSong.SendRemoteSongSelectState(true);
        end;
        Exit;
      end;
    rccPlaylistRemoveSong:
      begin
        Result := (Display.CurrentScreen = @ScreenSong) and (ArgsIndex >= 0) and
          (ArgsIndex <= High(PlaylistMan.Playlists)) and (ArgsItemIndex >= 0);
        if Result then
        begin
          PlaylistItemIndex := PlaylistMan.GetIndexbySongID(ArgsItemIndex, ArgsIndex);
          Result := PlaylistItemIndex >= 0;
          if Result then
          begin
            PlaylistMan.DelItem(PlaylistItemIndex, ArgsIndex);
            ScreenSong.SendRemoteSongSelectState(true);
          end;
        end;
        Exit;
      end;
    rccPlaylistRemoveItem:
      begin
        Result := (Display.CurrentScreen = @ScreenSong) and (ArgsIndex >= 0) and
          (ArgsIndex <= High(PlaylistMan.Playlists)) and (ArgsItemIndex >= 0) and
          (ArgsItemIndex <= High(PlaylistMan.Playlists[ArgsIndex].Items));
        if Result then
        begin
          PlaylistMan.DelItem(ArgsItemIndex, ArgsIndex);
          ScreenSong.SendRemoteSongSelectState(true);
        end;
        Exit;
      end;
    rccPlaylistMoveItemUp:
      begin
        Result := (Display.CurrentScreen = @ScreenSong) and (ArgsIndex >= 0) and
          (ArgsIndex <= High(PlaylistMan.Playlists)) and (ArgsItemIndex > 0) and
          (ArgsItemIndex <= High(PlaylistMan.Playlists[ArgsIndex].Items));
        if Result then
        begin
          PlaylistMan.MoveItem(Cardinal(ArgsItemIndex), Cardinal(ArgsItemIndex - 1), ArgsIndex);
          ScreenSong.SendRemoteSongSelectState(true);
        end;
        Exit;
      end;
    rccPlaylistMoveItemDown:
      begin
        Result := (Display.CurrentScreen = @ScreenSong) and (ArgsIndex >= 0) and
          (ArgsIndex <= High(PlaylistMan.Playlists)) and (ArgsItemIndex >= 0) and
          (ArgsItemIndex < High(PlaylistMan.Playlists[ArgsIndex].Items));
        if Result then
        begin
          PlaylistMan.MoveItem(Cardinal(ArgsItemIndex), Cardinal(ArgsItemIndex + 1), ArgsIndex);
          ScreenSong.SendRemoteSongSelectState(true);
        end;
        Exit;
      end;
  else
    Result := false;
    Exit;
  end;

  if (Key <> 0) and (not Assigned(Display.NextScreen)) then
  begin
    Result := Display.ParseInput(Key, 0, true);
    if not Result then
      DoQuit;
  end;
  if (Key <> 0) and Assigned(Display.NextScreen) then
    Result := false;
end;

procedure FreeRemoteCommandData(CommandData: PRemoteCommandData);
begin
  if (CommandData <> nil) then
    Dispose(CommandData);
end;

procedure AckRemoteCommand(CommandData: PRemoteCommandData; Accepted: boolean; const Reason: UTF8String);
begin
  if (CommandData = nil) then
    Exit;

  Log.LogStatus(
    'Remote control ' + RemoteControlCommandToString(CommandData^.Command) +
    ' commandId=' + IntToStr(CommandData^.CommandId) +
    ' accepted=' + BoolToStr(Accepted, true) +
    ' reason=' + string(Reason),
    'Remote Control'
  );

  RemoteBridgeIPC.SendAck(
    CommandData^.PlayerId,
    CommandData^.CommandId,
    Accepted,
    Reason,
    RemoteBridgeIPC.GameStateSeq
  );
end;

procedure EnqueueRemoteCommand(Data: Pointer);
var
  CommandData: PRemoteCommandData;
  QueueIndex: integer;
  I: integer;
begin
  CommandData := PRemoteCommandData(Data);
  if (CommandData = nil) then
    Exit;

  if (Length(RemoteCommandQueue) >= REMOTE_COMMAND_MAX_QUEUE) then
  begin
    AckRemoteCommand(RemoteCommandQueue[0], false, 'remote_queue_full');
    FreeRemoteCommandData(RemoteCommandQueue[0]);
    for I := 1 to High(RemoteCommandQueue) do
      RemoteCommandQueue[I - 1] := RemoteCommandQueue[I];
    SetLength(RemoteCommandQueue, Length(RemoteCommandQueue) - 1);
  end;

  QueueIndex := Length(RemoteCommandQueue);
  CommandData^.EnqueuedTick := SDL_GetTicks;
  SetLength(RemoteCommandQueue, QueueIndex + 1);
  RemoteCommandQueue[QueueIndex] := CommandData;
  Log.LogStatus(
    'Queued remote control ' + RemoteControlCommandToString(CommandData^.Command) +
    ' commandId=' + IntToStr(CommandData^.CommandId) +
    ' queue=' + IntToStr(Length(RemoteCommandQueue)),
    'Remote Control'
  );
end;

procedure DropRemoteCommandQueueHead;
var
  I: integer;
begin
  if (Length(RemoteCommandQueue) = 0) then
    Exit;

  for I := 1 to High(RemoteCommandQueue) do
    RemoteCommandQueue[I - 1] := RemoteCommandQueue[I];
  SetLength(RemoteCommandQueue, Length(RemoteCommandQueue) - 1);
end;

procedure DropStaleRemoteCommands;
var
  Tick: cardinal;
  CommandData: PRemoteCommandData;
begin
  Tick := SDL_GetTicks;
  while (Length(RemoteCommandQueue) > 0) do
  begin
    CommandData := RemoteCommandQueue[0];
    if (CommandData <> nil) and (Tick - CommandData^.EnqueuedTick <= REMOTE_COMMAND_MAX_AGE_MS) then
      Exit;

    DropRemoteCommandQueueHead;
    AckRemoteCommand(CommandData, false, 'remote_command_stale');
    FreeRemoteCommandData(CommandData);
  end;
end;

procedure ProcessRemoteCommandQueue;
var
  Tick: cardinal;
  CommandData: PRemoteCommandData;
  Accepted: boolean;
begin
  DropStaleRemoteCommands;
  if (Length(RemoteCommandQueue) = 0) then
    Exit;

  if Assigned(Display.NextScreen) then
    Exit;

  Tick := SDL_GetTicks;
  if (LastRemoteCommandTick <> 0) and (Tick - LastRemoteCommandTick < REMOTE_COMMAND_SPACING_MS) then
    Exit;

  CommandData := RemoteCommandQueue[0];
  DropRemoteCommandQueueHead;
  LastRemoteCommandTick := Tick;

  Accepted := DispatchRemoteCommand(CommandData^.Command, CommandData^.ArgsText,
    CommandData^.ArgsIndex, CommandData^.ArgsItemIndex);
  if Accepted then
    AckRemoteCommand(CommandData, true, '')
  else
    AckRemoteCommand(CommandData, false, 'not_available');
  FreeRemoteCommandData(CommandData);
end;

procedure RemoteBridgeCommandReceived(PlayerIndex: integer; const PlayerId: UTF8String;
  Command: TRemoteControlCommand; CommandId: integer; const ArgsText: UTF8String;
  ArgsIndex: integer; ArgsItemIndex: integer);
var
  Data: PRemoteCommandData;
begin
  New(Data);
  Data^.PlayerIndex := PlayerIndex;
  Data^.PlayerId := PlayerId;
  Data^.Command := Command;
  Data^.CommandId := CommandId;
  Data^.ArgsText := ArgsText;
  Data^.ArgsIndex := ArgsIndex;
  Data^.ArgsItemIndex := ArgsItemIndex;
  MainThreadExec(EnqueueRemoteCommand, Data);
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
      ProcessRemoteCommandQueue;

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
  SimKey: LongWord;
  mouseDown: boolean;
  mouseBtn:  integer;
  mouseX, mouseY: PInt;
  KeepGoing: boolean;
  SuppressKey: boolean;
  UpdateMouse: boolean;
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
        if (Ini.Mouse > 0) then
        begin
          UpdateMouse := true;
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

          if UpdateMouse then
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
            KeyCharUnicode:=0;
            if (Event.type_ = SDL_TEXTINPUT) and (Event.text.text <> '') then
            try
              KeyCharUnicode:=UnicodeStringToUCS4String(UnicodeString(UTF8String(Event.text.text)))[0];
              //KeyCharUnicode:=UnicodeStringToUCS4String(UnicodeString(Event.key.keysym.unicode))[1];//Event.text.text)[0];
            except
            end;

            SimKey :=0;
            if((Event.key.keysym.sym > Low(LongWord)) and (Event.key.keysym.sym < High(LongWord))) then
            begin
              SimKey := Event.key.keysym.sym;
            end;

            // if print is pressed -> make screenshot and save to screenshot path
            if (SimKey = SDLK_SYSREQ) or (SimKey = SDLK_PRINTSCREEN) then
              Display.SaveScreenShot
            // if there is a visible popup then let it handle input instead of underlying screen
            // shoud be done in a way to be sure the topmost popup has preference (maybe error, then check)
            else if (ScreenPopupError <> nil) and (ScreenPopupError.Visible) then
              KeepGoing := ScreenPopupError.ParseInput(SimKey, KeyCharUnicode, true)
            else if (ScreenPopupInfo <> nil) and (ScreenPopupInfo.Visible) then
              KeepGoing := ScreenPopupInfo.ParseInput(SimKey, KeyCharUnicode, true)
            else if (ScreenPopupCheck <> nil) and (ScreenPopupCheck.Visible) then
              KeepGoing := ScreenPopupCheck.ParseInput(SimKey, KeyCharUnicode, true)
            else if (ScreenPopupInsertUser <> nil) and (ScreenPopupInsertUser.Visible) then
              KeepGoing := ScreenPopupInsertUser.ParseInput(SimKey, KeyCharUnicode, true)
            else if (ScreenPopupSendScore <> nil) and (ScreenPopupSendScore.Visible) then
              KeepGoing := ScreenPopupSendScore.ParseInput(SimKey, KeyCharUnicode, true)
            else if (ScreenPopupScoreDownload <> nil) and (ScreenPopupScoreDownload.Visible) then
              KeepGoing := ScreenPopupScoreDownload.ParseInput(SimKey, KeyCharUnicode, true)
            else if (ScreenPopupHelp <> nil) and (ScreenPopupHelp.Visible) then
              KeepGoing := ScreenPopupHelp.ParseInput(SimKey, KeyCharUnicode, true)
            else if (Display.ShouldHandleInput(LongWord(SimKey), KeyCharUnicode, true, SuppressKey)) then
            begin
              // check if screen wants to exit
              KeepGoing := Display.ParseInput(SimKey, KeyCharUnicode, true, Event.key._repeat > 0);

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
          if (data1 <> nil) then
          begin
            try
              PMainThreadExecData(data1)^.Proc(PMainThreadExecData(data1)^.Data);
            finally
              Dispose(PMainThreadExecData(data1));
            end;
          end;
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
  ExecData: PMainThreadExecData;
begin
  New(ExecData);
  ExecData^.Proc := Proc;
  ExecData^.Data := Data;
  with Event.user do
  begin
    type_ := MAINTHREAD_EXEC_EVENT;
    code  := 0;     // not used at the moment
    data1 := ExecData;
    data2 := nil;
  end;
  SDL_PushEvent(@Event);
end;

end.
