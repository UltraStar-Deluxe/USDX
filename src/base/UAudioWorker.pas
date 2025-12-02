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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/base/UAudioWorker.pas $
 * $Id: UAudioWorker.pas $
 *}

unit UAudioWorker;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

interface

uses
  SDL2;

const
  AUDIO_WORKER_OSCILLOSCOPE_SAMPLES = 512;

type
  TOscilloscopeSampleArray = array[0..AUDIO_WORKER_OSCILLOSCOPE_SAMPLES-1] of Smallint;

  TAudioPlayerState = record
    Sequence: UInt64;
    TimestampTicks: UInt64;
    ToneValid: boolean;
    Tone: integer;
    ToneAbs: integer;
    MaxVolume: single;
    ToneString: UTF8String;
    Oscilloscope: TOscilloscopeSampleArray;
  end;

  PAudioPlayerState = ^TAudioPlayerState;

  TAudioWorkerCommandType = (
    awcNoop,
    awcRefreshConfiguration,
    awcShutdown
  );

  TAudioWorkerCommand = record
    CommandType: TAudioWorkerCommandType;
  end;

  TAudioWorkerStatus = record
    Active: boolean;
    LastActivityTicks: UInt64;
    PendingCommands: integer;
  end;

  TAudioWorkerAudioCallback = procedure of object;

procedure InitAudioWorker;
procedure FinalizeAudioWorker;
procedure AudioWorkerPushCommand(const Command: TAudioWorkerCommand);
procedure AudioWorkerGetStatus(out Status: TAudioWorkerStatus);
function AudioWorkerLockPlayerState(PlayerIndex: integer; out State: PAudioPlayerState): boolean;
procedure AudioWorkerUnlockPlayerState;
function AudioWorkerCopyPlayerState(PlayerIndex: integer; out State: TAudioPlayerState): boolean;
procedure AudioWorkerDispatchExternalAudioCallback;
procedure AudioWorkerSetExternalAudioCallback(const Callback: TAudioWorkerAudioCallback);

implementation

uses
  SysUtils,
  Classes,
  SyncObjs,
  URecord,
  UIni;

type
  PAudioWorkerCommand = ^TAudioWorkerCommand;

  TAudioWorkerThread = class(TThread)
  private
    procedure ProcessCommands;
    procedure ProcessAudio;
    procedure ProcessExternalAudioCallback;
    procedure HandleCommand(const Command: TAudioWorkerCommand);
    procedure UpdateActivityTick;
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;

  TAudioPlayerWorker = class(TThread)
  private
    FPlayerIndex: integer;
    function BuildPlayerState(out State: TAudioPlayerState): boolean;
    procedure PublishPlayerState(const State: TAudioPlayerState);
    function WaitForPlayerData(TimeoutMs: Cardinal): TWaitResult;
  protected
    procedure Execute; override;
  public
    constructor Create(PlayerIndex: integer);
  end;

var
  WorkerThread: TAudioWorkerThread = nil;
  CommandQueue: TList = nil;
  CommandLock: TCriticalSection = nil;
  CommandEvent: TSimpleEvent = nil;
  StatusLock: TCriticalSection = nil;
  InternalStatus: TAudioWorkerStatus;
  PlayerStateLock: TCriticalSection = nil;
  PlayerStates: array of TAudioPlayerState;
  AudioCallbackLock: TCriticalSection = nil;
  ExternalAudioCallback: TAudioWorkerAudioCallback = nil;
  ExternalAudioCallbackPending: boolean = false;
  PlayerWorkerLock: TCriticalSection = nil;
  PlayerWorkers: array of TAudioPlayerWorker;

procedure EnsurePlayerStateCapacity(TargetLength: integer);
begin
  if TargetLength < 0 then
    Exit;
  if PlayerStateLock = nil then
    Exit;

  PlayerStateLock.Enter;
  try
    if Length(PlayerStates) <> TargetLength then
      SetLength(PlayerStates, TargetLength);
  finally
    PlayerStateLock.Leave;
  end;
end;

procedure EnsurePlayerWorkerCount(TargetLength: integer);
var
  CurrentCount: integer;
  RemovalCount: integer;
  Index: integer;
  WorkersToStop: array of TAudioPlayerWorker;
begin
  if TargetLength < 0 then
    Exit;
  if PlayerWorkerLock = nil then
    Exit;

  WorkersToStop := nil;

  PlayerWorkerLock.Enter;
  try
    CurrentCount := Length(PlayerWorkers);
    if CurrentCount = TargetLength then
      Exit;

    if CurrentCount > TargetLength then
    begin
      RemovalCount := CurrentCount - TargetLength;
      SetLength(WorkersToStop, RemovalCount);
      for Index := TargetLength to CurrentCount - 1 do
      begin
        WorkersToStop[Index - TargetLength] := PlayerWorkers[Index];
        PlayerWorkers[Index] := nil;
      end;
      SetLength(PlayerWorkers, TargetLength);
    end
    else
    begin
      SetLength(PlayerWorkers, TargetLength);
      for Index := CurrentCount to TargetLength - 1 do
      begin
        if PlayerWorkers[Index] = nil then
          PlayerWorkers[Index] := TAudioPlayerWorker.Create(Index);
      end;
    end;
  finally
    PlayerWorkerLock.Leave;
  end;

  for Index := 0 to High(WorkersToStop) do
  begin
    if WorkersToStop[Index] <> nil then
    begin
      WorkersToStop[Index].Terminate;
      WorkersToStop[Index].WaitFor;
      FreeAndNil(WorkersToStop[Index]);
    end;
  end;
end;

procedure TerminatePlayerWorkers;
var
  Workers: array of TAudioPlayerWorker;
  Index: integer;
begin
  if PlayerWorkerLock = nil then
    Exit;

  PlayerWorkerLock.Enter;
  try
    Workers := PlayerWorkers;
    PlayerWorkers := nil;
  finally
    PlayerWorkerLock.Leave;
  end;

  for Index := 0 to High(Workers) do
  begin
    if Workers[Index] <> nil then
    begin
      Workers[Index].Terminate;
      Workers[Index].WaitFor;
      FreeAndNil(Workers[Index]);
    end;
  end;
end;

procedure InitializeSynchronizationObjects;
begin
  if CommandLock = nil then
    CommandLock := TCriticalSection.Create;
  if StatusLock = nil then
    StatusLock := TCriticalSection.Create;
  if CommandQueue = nil then
    CommandQueue := TList.Create;
  if CommandEvent = nil then
    CommandEvent := TSimpleEvent.Create;
  if PlayerStateLock = nil then
    PlayerStateLock := TCriticalSection.Create;
  if AudioCallbackLock = nil then
    AudioCallbackLock := TCriticalSection.Create;
  if PlayerWorkerLock = nil then
    PlayerWorkerLock := TCriticalSection.Create;
end;

procedure FreeSynchronizationObjects;
var
  Index: integer;
  Cmd: PAudioWorkerCommand;
begin
  if CommandQueue <> nil then
  begin
    for Index := 0 to CommandQueue.Count - 1 do
    begin
      Cmd := PAudioWorkerCommand(CommandQueue[Index]);
      if Cmd <> nil then
        FreeMem(Cmd);
    end;
    CommandQueue.Free;
    CommandQueue := nil;
  end;

  FreeAndNil(CommandEvent);
  FreeAndNil(CommandLock);
  FreeAndNil(StatusLock);
  PlayerStates := nil;
  FreeAndNil(PlayerStateLock);
  ExternalAudioCallback := nil;
  FreeAndNil(AudioCallbackLock);
  FreeAndNil(PlayerWorkerLock);
end;

procedure AudioWorkerSetExternalAudioCallback(const Callback: TAudioWorkerAudioCallback);
begin
  InitializeSynchronizationObjects;

  if AudioCallbackLock = nil then
    Exit;

  AudioCallbackLock.Enter;
  try
    ExternalAudioCallback := Callback;
    ExternalAudioCallbackPending := false;
  finally
    AudioCallbackLock.Leave;
  end;
end;

procedure InitAudioWorker;
begin
  if Assigned(WorkerThread) then
    Exit;

  InitializeSynchronizationObjects;
  InternalStatus.Active := false;
  InternalStatus.LastActivityTicks := 0;
  InternalStatus.PendingCommands := 0;

  WorkerThread := TAudioWorkerThread.Create;
  EnsurePlayerStateCapacity(Length(URecord.AudioInputProcessor().Sound));
  EnsurePlayerWorkerCount(Length(URecord.AudioInputProcessor().Sound));
end;

procedure FinalizeAudioWorker;
var
  ShutdownCommand: TAudioWorkerCommand;
begin
  if WorkerThread = nil then
  begin
    TerminatePlayerWorkers;
    FreeSynchronizationObjects;
    Exit;
  end;

  ShutdownCommand.CommandType := awcShutdown;
  AudioWorkerPushCommand(ShutdownCommand);
  WorkerThread.Terminate;
  if CommandEvent <> nil then
    CommandEvent.SetEvent;

  WorkerThread.WaitFor;
  FreeAndNil(WorkerThread);

  TerminatePlayerWorkers;

  FreeSynchronizationObjects;
end;

procedure AudioWorkerPushCommand(const Command: TAudioWorkerCommand);
var
  CommandCopy: PAudioWorkerCommand;
begin
  if CommandLock = nil then
    Exit;

  GetMem(CommandCopy, SizeOf(TAudioWorkerCommand));
  CommandCopy^ := Command;

  CommandLock.Enter;
  try
    if CommandQueue <> nil then
      CommandQueue.Add(CommandCopy)
    else
    begin
      FreeMem(CommandCopy);
      Exit;
    end;
  finally
    CommandLock.Leave;
  end;

  if CommandEvent <> nil then
    CommandEvent.SetEvent;
end;

procedure AudioWorkerGetStatus(out Status: TAudioWorkerStatus);
begin
  Status.Active := Assigned(WorkerThread) and (not WorkerThread.Finished);

  if StatusLock <> nil then
  begin
    StatusLock.Enter;
    try
      Status.LastActivityTicks := InternalStatus.LastActivityTicks;
    finally
      StatusLock.Leave;
    end;
  end
  else
    Status.LastActivityTicks := 0;

  if CommandLock <> nil then
  begin
    CommandLock.Enter;
    try
      if CommandQueue <> nil then
        Status.PendingCommands := CommandQueue.Count
      else
        Status.PendingCommands := 0;
    finally
      CommandLock.Leave;
    end;
  end
  else
    Status.PendingCommands := 0;
end;

function AudioWorkerLockPlayerState(PlayerIndex: integer; out State: PAudioPlayerState): boolean;
begin
  State := nil;
  Result := false;

  if PlayerStateLock = nil then
    Exit;
  if PlayerIndex < 0 then
    Exit;

  PlayerStateLock.Enter;
  if (PlayerIndex <= High(PlayerStates)) and
     (PlayerStates[PlayerIndex].Sequence <> 0) then
  begin
    State := @PlayerStates[PlayerIndex];
    Result := true;
  end
  else
  begin
    PlayerStateLock.Leave;
  end;
end;

procedure AudioWorkerUnlockPlayerState;
begin
  if PlayerStateLock <> nil then
    PlayerStateLock.Leave;
end;

function AudioWorkerCopyPlayerState(PlayerIndex: integer; out State: TAudioPlayerState): boolean;
begin
  State.Sequence := 0;
  State.TimestampTicks := 0;
  State.ToneValid := false;
  State.Tone := -1;
  State.ToneAbs := -1;
  State.MaxVolume := 0;
  State.ToneString := '-';
  FillChar(State.Oscilloscope, SizeOf(State.Oscilloscope), 0);

  Result := false;

  if PlayerStateLock = nil then
    Exit;
  if PlayerIndex < 0 then
    Exit;

  PlayerStateLock.Enter;
  try
    if (PlayerIndex <= High(PlayerStates)) and
       (PlayerStates[PlayerIndex].Sequence <> 0) then
    begin
      State := PlayerStates[PlayerIndex];
      Result := true;
    end;
  finally
    PlayerStateLock.Leave;
  end;
end;

procedure TAudioWorkerThread.Execute;
begin
  StatusLock.Enter;
  try
    InternalStatus.Active := true;
  finally
    StatusLock.Leave;
  end;

  while not Terminated do
  begin
    if Assigned(CommandEvent) then
      CommandEvent.WaitFor(Ini.WorkerIntervalMs)
    else
      SDL_Delay(Ini.WorkerIntervalMs);

    ProcessCommands;
    ProcessAudio;
    ProcessExternalAudioCallback;
    UpdateActivityTick;
  end;

  ProcessCommands;

  StatusLock.Enter;
  try
    InternalStatus.Active := false;
  finally
    StatusLock.Leave;
  end;
end;

constructor TAudioWorkerThread.Create;
begin
  inherited Create(false);
  FreeOnTerminate := false;
end;

{ TAudioPlayerWorker }

constructor TAudioPlayerWorker.Create(PlayerIndex: integer);
begin
  FPlayerIndex := PlayerIndex;
  inherited Create(false);
  FreeOnTerminate := false;
end;

function TAudioPlayerWorker.BuildPlayerState(out State: TAudioPlayerState): boolean;
var
  Processor: TAudioInputProcessor;
  Sound: TCaptureBuffer;
  AvailableSamples: integer;
  SampleCount: integer;
  CopyOffset: integer;
begin
  Result := false;
  Processor := URecord.AudioInputProcessor();
  if Processor = nil then
    Exit;

  if (FPlayerIndex < 0) or (FPlayerIndex > High(Processor.Sound)) then
    Exit;

  Sound := Processor.Sound[FPlayerIndex];
  if Sound = nil then
    Exit;

  Sound.AnalyzeBuffer;

  State.Sequence := 0;
  State.TimestampTicks := UInt64(SDL_GetTicks);
  State.ToneValid := Sound.ToneValid;
  State.Tone := Sound.Tone;
  State.ToneAbs := Sound.ToneAbs;
  State.MaxVolume := Sound.MaxSampleVolume;
  if State.ToneValid then
    State.ToneString := Sound.ToneString
  else
    State.ToneString := '-';

  FillChar(State.Oscilloscope, SizeOf(State.Oscilloscope), 0);

  Sound.LockAnalysisBuffer;
  try
    AvailableSamples := Length(Sound.AnalysisBuffer);
    SampleCount := AUDIO_WORKER_OSCILLOSCOPE_SAMPLES;
    if SampleCount > AvailableSamples then
      SampleCount := AvailableSamples;

    if SampleCount > 0 then
    begin
      CopyOffset := AvailableSamples - SampleCount;
      Move(Sound.AnalysisBuffer[CopyOffset],
           State.Oscilloscope[0],
           SampleCount * SizeOf(Smallint));
    end;
  finally
    Sound.UnlockAnalysisBuffer;
  end;

  Result := true;
end;

procedure TAudioPlayerWorker.PublishPlayerState(const State: TAudioPlayerState);
var
  Sequence: UInt64;
begin
  if PlayerStateLock = nil then
    Exit;

  PlayerStateLock.Enter;
  try
    if FPlayerIndex > High(PlayerStates) then
      SetLength(PlayerStates, FPlayerIndex + 1);
    Sequence := PlayerStates[FPlayerIndex].Sequence + 1;
    PlayerStates[FPlayerIndex] := State;
    PlayerStates[FPlayerIndex].Sequence := Sequence;
  finally
    PlayerStateLock.Leave;
  end;
end;

function TAudioPlayerWorker.WaitForPlayerData(TimeoutMs: Cardinal): TWaitResult;
var
  Processor: TAudioInputProcessor;
  Buffer: TCaptureBuffer;
begin
  Result := wrTimeout;
  Processor := URecord.AudioInputProcessor();
  if Processor = nil then
  begin
    if TimeoutMs > 0 then
      SDL_Delay(TimeoutMs);
    Exit;
  end;

  if (FPlayerIndex < 0) or (FPlayerIndex > High(Processor.Sound)) then
  begin
    if TimeoutMs > 0 then
      SDL_Delay(TimeoutMs);
    Exit;
  end;

  Buffer := Processor.Sound[FPlayerIndex];
  if Buffer = nil then
  begin
    if TimeoutMs > 0 then
      SDL_Delay(TimeoutMs);
    Exit;
  end;

  Result := Buffer.WaitForData(TimeoutMs);
end;

procedure TAudioPlayerWorker.Execute;
var
  PlayerState: TAudioPlayerState;
  WaitResult: TWaitResult;
  WaitTimeout: Cardinal;
begin
  while not Terminated do
  begin
    WaitTimeout := Ini.WorkerIntervalMs;
    WaitResult := WaitForPlayerData(WaitTimeout);
    if Terminated then
      Break;

    if (WaitResult = wrSignaled) or (WaitResult = wrTimeout) then
    begin
      if BuildPlayerState(PlayerState) then
        PublishPlayerState(PlayerState);
    end
    else if (WaitResult = wrError) and (WaitTimeout > 0) then
      SDL_Delay(WaitTimeout);
  end;
end;

procedure TAudioWorkerThread.ProcessCommands;
var
  Cmd: PAudioWorkerCommand;
begin
  while true do
  begin
    Cmd := nil;

    CommandLock.Enter;
    try
      if (CommandQueue = nil) or (CommandQueue.Count = 0) then
      begin
        if CommandEvent <> nil then
          CommandEvent.ResetEvent;
        Exit;
      end;

      Cmd := PAudioWorkerCommand(CommandQueue[0]);
      CommandQueue.Delete(0);
    finally
      CommandLock.Leave;
    end;

    if Cmd <> nil then
    begin
      try
        HandleCommand(Cmd^);
      finally
        FreeMem(Cmd);
      end;
    end;
  end;
end;

procedure TAudioWorkerThread.ProcessExternalAudioCallback;
begin
  if AudioCallbackLock = nil then
    Exit;

  AudioCallbackLock.Enter;
  try
    if Assigned(ExternalAudioCallback) then
      ExternalAudioCallbackPending := true;
  finally
    AudioCallbackLock.Leave;
  end;
end;

procedure AudioWorkerDispatchExternalAudioCallback;
var
  Callback: TAudioWorkerAudioCallback;
begin
  Callback := nil;
  if AudioCallbackLock <> nil then
  begin
    AudioCallbackLock.Enter;
    try
      if ExternalAudioCallbackPending then
      begin
        ExternalAudioCallbackPending := false;
        Callback := ExternalAudioCallback;
      end;
    finally
      AudioCallbackLock.Leave;
    end;
  end;

  if Assigned(Callback) then
    Callback();
end;

procedure TAudioWorkerThread.ProcessAudio;
var
  Processor: TAudioInputProcessor;
begin
  Processor := URecord.AudioInputProcessor();
  if Processor = nil then
    Exit;

  EnsurePlayerStateCapacity(Length(Processor.Sound));
  EnsurePlayerWorkerCount(Length(Processor.Sound));
end;

procedure TAudioWorkerThread.HandleCommand(const Command: TAudioWorkerCommand);
begin
  case Command.CommandType of
    awcNoop: ;
    awcRefreshConfiguration:
      begin
        EnsurePlayerStateCapacity(Length(URecord.AudioInputProcessor().Sound));
        EnsurePlayerWorkerCount(Length(URecord.AudioInputProcessor().Sound));
      end;
    awcShutdown:
      Terminate;
  end;
end;

procedure TAudioWorkerThread.UpdateActivityTick;
begin
  StatusLock.Enter;
  try
    InternalStatus.LastActivityTicks := UInt64(SDL_GetTicks);
  finally
    StatusLock.Leave;
  end;
end;

end.
