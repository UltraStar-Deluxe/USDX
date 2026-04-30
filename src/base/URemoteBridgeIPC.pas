{* UltraStar Deluxe - Karaoke Game
 *
 * Local JSONL IPC reader for tools/usdx-bridge.mjs.
 * This connects to 127.0.0.1:8765 and consumes typed remote-control and
 * remote-pitch observations. It never handles raw microphone audio.
 *}

unit URemoteBridgeIPC;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
  SysUtils,
  URemoteControl,
  URemoteInput;

type
  TRemoteBridgeCommandEvent = procedure(PlayerIndex: integer;
    const PlayerId: UTF8String; Command: TRemoteControlCommand;
    CommandId: integer; const ArgsText: UTF8String; ArgsIndex: integer;
    ArgsItemIndex: integer);

  TRemoteBridgePlayer = record
    PlayerId: UTF8String;
    Name: UTF8String;
    Role: UTF8String;
    Slot: integer;
    Connected: boolean;
  end;

  TRemoteBridgeIPC = class
  private
    FHost: string;
    FPort: integer;
    FThread: TThread;
    FOnCommand: TRemoteBridgeCommandEvent;
    FStatusLock: TRTLCriticalSection;
    FConnected: boolean;
    FDisplayCode: UTF8String;
    FStatusSeq: integer;
    FGameStateSeq: integer;
    FPlayers: array of TRemoteBridgePlayer;
    procedure ClearRoomStatus;
    procedure SetRoomStatus(Connected: boolean; const DisplayCode: UTF8String);
    procedure ClearPlayers;
    procedure UpsertPlayer(const PlayerId, Name, Role: UTF8String; Slot: integer);
    procedure SetPlayerConnected(const PlayerId: UTF8String; Connected: boolean);
    procedure RemovePlayer(const PlayerId: UTF8String);
    function BuildPlayersJson(out ControllerPlayerId: UTF8String): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
    procedure SendAck(const PlayerId: UTF8String; CommandId: integer;
      Accepted: boolean; const Reason: UTF8String; GameStateSeq: integer);
    procedure SendSongStarted(SongSeq: integer; SongId: integer; MediaStartUs: int64;
      const Title, Artist: UTF8String);
    procedure SendSongPaused(SongSeq: integer; MediaStartUs: int64);
    procedure SendSongResumed(SongSeq: integer; MediaStartUs: int64);
    procedure SendSongEnded(SongSeq: integer; MediaStartUs: int64);
    procedure SendPlayerAssigned(const PlayerId, Role: UTF8String; Slot: integer);
    procedure SendJsonMessage(const JsonMessage: string);
    function SendGameState(const State: UTF8String; SongSeq: integer;
      PlaylistSize: integer): integer;
    procedure GetRoomStatus(out Connected: boolean; out DisplayCode: UTF8String;
      out StatusSeq: integer; out ConnectedPlayers: integer);

    property Host: string read FHost write FHost;
    property Port: integer read FPort write FPort;
    property OnCommand: TRemoteBridgeCommandEvent read FOnCommand write FOnCommand;
    property GameStateSeq: integer read FGameStateSeq;
  end;

function RemoteBridgeIPC(): TRemoteBridgeIPC;
function RemoteJsonEscape(const Value: UTF8String): string;

implementation

uses
  fpjson,
  jsonparser,
  ULog,
  ssockets;

const
  DEFAULT_BRIDGE_HOST = '127.0.0.1';
  DEFAULT_BRIDGE_PORT = 8765;
  READ_BUFFER_SIZE = 4096;
  RECONNECT_DELAY_MS = 1000;

type
  TRemoteBridgeIPCThread = class(TThread)
  private
    FOwner: TRemoteBridgeIPC;
    FSocket: TInetSocket;
    FLineBuffer: string;

    procedure HandleLine(const Line: string);
    procedure HandleMessage(Obj: TJSONObject);
    procedure HandlePlayerAssigned(Obj: TJSONObject);
    procedure HandlePlayerDisconnected(Obj: TJSONObject);
    procedure HandlePlayerLeft(Obj: TJSONObject);
    procedure HandleSongStarted(Obj: TJSONObject);
    procedure HandlePitchBatch(Obj: TJSONObject);
    procedure HandleControlCommand(Obj: TJSONObject);
    procedure SendAck(const PlayerId: UTF8String; CommandId: integer;
      Accepted: boolean; const Reason: UTF8String; GameStateSeq: integer);
    procedure SendSongStarted(SongSeq: integer; SongId: integer; MediaStartUs: int64;
      const Title, Artist: UTF8String);
    procedure SendPlayerAssigned(const PlayerId, Role: UTF8String; Slot: integer);
    procedure SendGameState(Seq: integer; const State: UTF8String;
      SongSeq: integer; PlaylistSize: integer; const PlayersJson: string;
      const ControllerPlayerId: UTF8String);
    procedure SendSongEvent(const EventType: string; SongSeq: integer; MediaStartUs: int64);
    procedure SendSongPaused(SongSeq: integer; MediaStartUs: int64);
    procedure SendSongResumed(SongSeq: integer; MediaStartUs: int64);
    procedure SendSongEnded(SongSeq: integer; MediaStartUs: int64);
    procedure SendLine(const Line: string);
    procedure CloseSocket;
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TRemoteBridgeIPC);
    destructor Destroy; override;
  end;

var
  singleton_RemoteBridgeIPC: TRemoteBridgeIPC = nil;
  remotePitchLogCounter: integer = 0;

function RemoteBridgeIPC(): TRemoteBridgeIPC;
begin
  if singleton_RemoteBridgeIPC = nil then
    singleton_RemoteBridgeIPC := TRemoteBridgeIPC.Create;

  Result := singleton_RemoteBridgeIPC;
end;

function JsonGetString(Obj: TJSONObject; const Name, Default: string): string;
var
  Data: TJSONData;
begin
  Result := Default;
  Data := Obj.Find(Name);
  if (Data <> nil) and (Data.JSONType = jtString) then
    Result := Data.AsString;
end;

function JsonGetInt(Obj: TJSONObject; const Name: string; Default: integer): integer;
var
  Data: TJSONData;
begin
  Result := Default;
  Data := Obj.Find(Name);
  if (Data <> nil) and (Data.JSONType <> jtNull) then
  begin
    try
      Result := Data.AsInteger;
    except
      Result := Default;
    end;
  end;
end;

function JsonGetInt64(Obj: TJSONObject; const Name: string; Default: int64): int64;
var
  Data: TJSONData;
begin
  Result := Default;
  Data := Obj.Find(Name);
  if (Data <> nil) and (Data.JSONType <> jtNull) then
  begin
    try
      Result := Data.AsInt64;
    except
      Result := Default;
    end;
  end;
end;

function JsonGetFloat(Obj: TJSONObject; const Name: string; Default: double): double;
var
  Data: TJSONData;
begin
  Result := Default;
  Data := Obj.Find(Name);
  if (Data <> nil) and (Data.JSONType <> jtNull) then
  begin
    try
      Result := Data.AsFloat;
    except
      Result := Default;
    end;
  end;
end;

function JsonGetBool(Obj: TJSONObject; const Name: string; Default: boolean): boolean;
var
  Data: TJSONData;
begin
  Result := Default;
  Data := Obj.Find(Name);
  if (Data <> nil) and (Data.JSONType <> jtNull) then
  begin
    try
      Result := Data.AsBoolean;
    except
      Result := Default;
    end;
  end;
end;

function JsonArrayGetInt(Arr: TJSONArray; Index: integer; Default: integer): integer;
begin
  Result := Default;
  if (Arr = nil) or (Index < 0) or (Index >= Arr.Count) or
     (Arr.Items[Index].JSONType = jtNull) then
    Exit;
  try
    Result := Arr.Items[Index].AsInteger;
  except
    Result := Default;
  end;
end;

function JsonArrayGetInt64(Arr: TJSONArray; Index: integer; Default: int64): int64;
begin
  Result := Default;
  if (Arr = nil) or (Index < 0) or (Index >= Arr.Count) or
     (Arr.Items[Index].JSONType = jtNull) then
    Exit;
  try
    Result := Arr.Items[Index].AsInt64;
  except
    Result := Default;
  end;
end;

function JsonFindArray(Obj: TJSONObject; const Name: string): TJSONArray;
var
  Data: TJSONData;
begin
  Result := nil;
  Data := Obj.Find(Name);
  if (Data <> nil) and (Data.JSONType = jtArray) then
    Result := TJSONArray(Data);
end;

function SlotToPlayerIndex(Slot: integer): integer;
begin
  Result := Slot - 1;
end;

function RemoteJsonEscape(const Value: UTF8String): string;
begin
  Result := StringReplace(string(Value), '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\r', [rfReplaceAll]);
end;

function JsonEscape(const Value: UTF8String): string;
begin
  Result := RemoteJsonEscape(Value);
end;

{ TRemoteBridgeIPC }

constructor TRemoteBridgeIPC.Create;
begin
  inherited;
  FHost := DEFAULT_BRIDGE_HOST;
  FPort := DEFAULT_BRIDGE_PORT;
  InitCriticalSection(FStatusLock);
end;

destructor TRemoteBridgeIPC.Destroy;
begin
  Stop;
  DoneCriticalSection(FStatusLock);
  inherited;
end;

procedure TRemoteBridgeIPC.ClearRoomStatus;
begin
  ClearPlayers;
  SetRoomStatus(false, '');
end;

procedure TRemoteBridgeIPC.SetRoomStatus(Connected: boolean; const DisplayCode: UTF8String);
begin
  EnterCriticalSection(FStatusLock);
  try
    if (FConnected <> Connected) or (FDisplayCode <> DisplayCode) then
    begin
      FConnected := Connected;
      FDisplayCode := DisplayCode;
      Inc(FStatusSeq);
    end;
  finally
    LeaveCriticalSection(FStatusLock);
  end;
end;

procedure TRemoteBridgeIPC.GetRoomStatus(out Connected: boolean;
  out DisplayCode: UTF8String; out StatusSeq: integer; out ConnectedPlayers: integer);
var
  I: integer;
begin
  EnterCriticalSection(FStatusLock);
  try
    Connected := FConnected;
    DisplayCode := FDisplayCode;
    StatusSeq := FStatusSeq;
    ConnectedPlayers := 0;
    for I := 0 to High(FPlayers) do
      if FPlayers[I].Connected then
        Inc(ConnectedPlayers);
  finally
    LeaveCriticalSection(FStatusLock);
  end;
end;

procedure TRemoteBridgeIPC.ClearPlayers;
begin
  EnterCriticalSection(FStatusLock);
  try
    SetLength(FPlayers, 0);
  finally
    LeaveCriticalSection(FStatusLock);
  end;
end;

procedure TRemoteBridgeIPC.UpsertPlayer(const PlayerId, Name, Role: UTF8String; Slot: integer);
var
  I: integer;
begin
  if (PlayerId = '') then
    Exit;

  EnterCriticalSection(FStatusLock);
  try
    for I := 0 to High(FPlayers) do
    begin
      if (FPlayers[I].PlayerId = PlayerId) then
      begin
        if (Name <> '') then
          FPlayers[I].Name := Name;
        if (Role <> '') then
          FPlayers[I].Role := Role;
        if (FPlayers[I].Slot > 0) and (FPlayers[I].Slot <> Slot) then
          RemoteInputProcessor.ClearPlayerSlot(SlotToPlayerIndex(FPlayers[I].Slot));
        if (Slot >= 0) then
          FPlayers[I].Slot := Slot;
        FPlayers[I].Connected := true;
        Exit;
      end;
    end;

    SetLength(FPlayers, Length(FPlayers) + 1);
    FPlayers[High(FPlayers)].PlayerId := PlayerId;
    FPlayers[High(FPlayers)].Name := Name;
    FPlayers[High(FPlayers)].Role := Role;
    FPlayers[High(FPlayers)].Slot := Slot;
    FPlayers[High(FPlayers)].Connected := true;
  finally
    LeaveCriticalSection(FStatusLock);
  end;
end;

procedure TRemoteBridgeIPC.SetPlayerConnected(const PlayerId: UTF8String; Connected: boolean);
var
  I: integer;
begin
  if (PlayerId = '') then
    Exit;

  EnterCriticalSection(FStatusLock);
  try
    for I := 0 to High(FPlayers) do
    begin
      if (FPlayers[I].PlayerId = PlayerId) then
      begin
        FPlayers[I].Connected := Connected;
        Exit;
      end;
    end;
  finally
    LeaveCriticalSection(FStatusLock);
  end;
end;

procedure TRemoteBridgeIPC.RemovePlayer(const PlayerId: UTF8String);
var
  I: integer;
  J: integer;
begin
  if (PlayerId = '') then
    Exit;

  EnterCriticalSection(FStatusLock);
  try
    for I := 0 to High(FPlayers) do
    begin
      if (FPlayers[I].PlayerId = PlayerId) then
      begin
        if (FPlayers[I].Slot > 0) then
          RemoteInputProcessor.ClearPlayerSlot(SlotToPlayerIndex(FPlayers[I].Slot));
        for J := I to High(FPlayers) - 1 do
          FPlayers[J] := FPlayers[J + 1];
        SetLength(FPlayers, Length(FPlayers) - 1);
        Exit;
      end;
    end;
  finally
    LeaveCriticalSection(FStatusLock);
  end;
end;

function TRemoteBridgeIPC.BuildPlayersJson(out ControllerPlayerId: UTF8String): string;
var
  I: integer;
  PlayerJson: string;
  SingingText: string;
  ConnectedText: string;
begin
  Result := '[';
  ControllerPlayerId := '';

  EnterCriticalSection(FStatusLock);
  try
    for I := 0 to High(FPlayers) do
    begin
      if (I > 0) then
        Result := Result + ',';

      if (FPlayers[I].Role = 'controller') and (ControllerPlayerId = '') then
        ControllerPlayerId := FPlayers[I].PlayerId;

      if (FPlayers[I].Role = 'singer') or (FPlayers[I].Role = 'controller') then
        SingingText := 'true'
      else
        SingingText := 'false';
      if FPlayers[I].Connected then
        ConnectedText := 'true'
      else
        ConnectedText := 'false';

      PlayerJson :=
        '{"slot":' + IntToStr(FPlayers[I].Slot) +
        ',"playerId":"' + JsonEscape(FPlayers[I].PlayerId) + '"' +
        ',"name":"' + JsonEscape(FPlayers[I].Name) + '"' +
        ',"connected":' + ConnectedText +
        ',"singing":' + SingingText +
        ',"role":"' + JsonEscape(FPlayers[I].Role) + '"' +
        '}';
      Result := Result + PlayerJson;
    end;
  finally
    LeaveCriticalSection(FStatusLock);
  end;

  Result := Result + ']';
end;

procedure TRemoteBridgeIPC.Start;
begin
  if (FThread <> nil) then
    Exit;

  FThread := TRemoteBridgeIPCThread.Create(Self);
  FThread.Start;
end;

procedure TRemoteBridgeIPC.Stop;
begin
  if (FThread = nil) then
    Exit;

  FThread.Terminate;
  TRemoteBridgeIPCThread(FThread).CloseSocket;
  FThread.WaitFor;
  FreeAndNil(FThread);
end;

procedure TRemoteBridgeIPC.SendAck(const PlayerId: UTF8String; CommandId: integer;
  Accepted: boolean; const Reason: UTF8String; GameStateSeq: integer);
begin
  if (FThread <> nil) and (FThread is TRemoteBridgeIPCThread) then
    TRemoteBridgeIPCThread(FThread).SendAck(PlayerId, CommandId, Accepted, Reason, GameStateSeq);
end;

procedure TRemoteBridgeIPC.SendSongStarted(SongSeq: integer; SongId: integer; MediaStartUs: int64;
  const Title, Artist: UTF8String);
begin
  RemoteInputProcessor.SetSongSeq(SongSeq);
  if (FThread <> nil) and (FThread is TRemoteBridgeIPCThread) then
    TRemoteBridgeIPCThread(FThread).SendSongStarted(SongSeq, SongId, MediaStartUs, Title, Artist);
end;

procedure TRemoteBridgeIPC.SendSongPaused(SongSeq: integer; MediaStartUs: int64);
begin
  if (FThread <> nil) and (FThread is TRemoteBridgeIPCThread) then
    TRemoteBridgeIPCThread(FThread).SendSongPaused(SongSeq, MediaStartUs);
end;

procedure TRemoteBridgeIPC.SendSongResumed(SongSeq: integer; MediaStartUs: int64);
begin
  if (FThread <> nil) and (FThread is TRemoteBridgeIPCThread) then
    TRemoteBridgeIPCThread(FThread).SendSongResumed(SongSeq, MediaStartUs);
end;

procedure TRemoteBridgeIPC.SendSongEnded(SongSeq: integer; MediaStartUs: int64);
begin
  if (FThread <> nil) and (FThread is TRemoteBridgeIPCThread) then
    TRemoteBridgeIPCThread(FThread).SendSongEnded(SongSeq, MediaStartUs);
end;

procedure TRemoteBridgeIPC.SendPlayerAssigned(const PlayerId, Role: UTF8String; Slot: integer);
begin
  if (FThread <> nil) and (FThread is TRemoteBridgeIPCThread) then
    TRemoteBridgeIPCThread(FThread).SendPlayerAssigned(PlayerId, Role, Slot);
end;

procedure TRemoteBridgeIPC.SendJsonMessage(const JsonMessage: string);
begin
  if (FThread <> nil) and (FThread is TRemoteBridgeIPCThread) and (JsonMessage <> '') then
    TRemoteBridgeIPCThread(FThread).SendLine(JsonMessage + #10);
end;

function TRemoteBridgeIPC.SendGameState(const State: UTF8String; SongSeq: integer;
  PlaylistSize: integer): integer;
var
  PlayersJson: string;
  ControllerPlayerId: UTF8String;
begin
  Inc(FGameStateSeq);
  Result := FGameStateSeq;
  PlayersJson := BuildPlayersJson(ControllerPlayerId);
  if (FThread <> nil) and (FThread is TRemoteBridgeIPCThread) then
    TRemoteBridgeIPCThread(FThread).SendGameState(
      Result,
      State,
      SongSeq,
      PlaylistSize,
      PlayersJson,
      ControllerPlayerId
    );
end;

{ TRemoteBridgeIPCThread }

constructor TRemoteBridgeIPCThread.Create(Owner: TRemoteBridgeIPC);
begin
  inherited Create(true);
  FreeOnTerminate := false;
  FOwner := Owner;
end;

destructor TRemoteBridgeIPCThread.Destroy;
begin
  CloseSocket;
  inherited;
end;

procedure TRemoteBridgeIPCThread.CloseSocket;
begin
  if (FSocket <> nil) then
    FreeAndNil(FSocket);
end;

procedure TRemoteBridgeIPCThread.SendLine(const Line: string);
begin
  if (FSocket = nil) then
    Exit;

  FSocket.Write(Pointer(Line)^, Length(Line));
end;

procedure TRemoteBridgeIPCThread.SendAck(const PlayerId: UTF8String; CommandId: integer;
  Accepted: boolean; const Reason: UTF8String; GameStateSeq: integer);
var
  AcceptedText: string;
  ReasonText: string;
begin
  if Accepted then
    AcceptedText := 'true'
  else
    AcceptedText := 'false';

  if (Reason = '') then
    ReasonText := 'null'
  else
    ReasonText := '"' + JsonEscape(Reason) + '"';

  SendLine(
    '{"type":"ack","protocol":1,"playerId":"' + JsonEscape(PlayerId) +
    '","commandId":' + IntToStr(CommandId) +
    ',"accepted":' + AcceptedText +
    ',"reason":' + ReasonText +
    ',"gameStateSeq":' + IntToStr(GameStateSeq) +
    '}' + #10
  );
end;

procedure TRemoteBridgeIPCThread.SendSongStarted(SongSeq: integer; SongId: integer; MediaStartUs: int64;
  const Title, Artist: UTF8String);
begin
  SendLine(
    '{"type":"song.started","protocol":1' +
    ',"songSeq":' + IntToStr(SongSeq) +
    ',"songId":' + IntToStr(SongId) +
    ',"mediaStartUs":' + IntToStr(MediaStartUs) +
    ',"title":"' + JsonEscape(Title) + '"' +
    ',"artist":"' + JsonEscape(Artist) + '"' +
    '}' + #10
  );
end;

procedure TRemoteBridgeIPCThread.SendPlayerAssigned(const PlayerId, Role: UTF8String; Slot: integer);
begin
  if (PlayerId = '') or (Slot < 0) then
    Exit;

  SendLine(
    '{"type":"player.assigned","protocol":1' +
    ',"playerId":"' + JsonEscape(PlayerId) + '"' +
    ',"slot":' + IntToStr(Slot) +
    ',"role":"' + JsonEscape(Role) + '"' +
    '}' + #10
  );
end;

procedure TRemoteBridgeIPCThread.SendGameState(Seq: integer; const State: UTF8String;
  SongSeq: integer; PlaylistSize: integer; const PlayersJson: string;
  const ControllerPlayerId: UTF8String);
var
  ControllerJson: string;
begin
  if (ControllerPlayerId = '') then
    ControllerJson := 'null'
  else
    ControllerJson := '"' + JsonEscape(ControllerPlayerId) + '"';

  SendLine(
    '{"type":"game.state","protocol":1' +
    ',"seq":' + IntToStr(Seq) +
    ',"state":"' + JsonEscape(State) + '"' +
    ',"songSeq":' + IntToStr(SongSeq) +
    ',"players":' + PlayersJson +
    ',"controllerPlayerId":' + ControllerJson +
    ',"playlistSize":' + IntToStr(PlaylistSize) +
    '}' + #10
  );
end;

procedure TRemoteBridgeIPCThread.SendSongEvent(const EventType: string; SongSeq: integer; MediaStartUs: int64);
begin
  SendLine(
    '{"type":"' + JsonEscape(UTF8String(EventType)) + '","protocol":1' +
    ',"songSeq":' + IntToStr(SongSeq) +
    ',"mediaStartUs":' + IntToStr(MediaStartUs) +
    '}' + #10
  );
end;

procedure TRemoteBridgeIPCThread.SendSongPaused(SongSeq: integer; MediaStartUs: int64);
begin
  SendSongEvent('song.paused', SongSeq, MediaStartUs);
end;

procedure TRemoteBridgeIPCThread.SendSongResumed(SongSeq: integer; MediaStartUs: int64);
begin
  SendSongEvent('song.resumed', SongSeq, MediaStartUs);
end;

procedure TRemoteBridgeIPCThread.SendSongEnded(SongSeq: integer; MediaStartUs: int64);
begin
  SendSongEvent('song.ended', SongSeq, MediaStartUs);
end;

procedure TRemoteBridgeIPCThread.Execute;
var
  Buffer: array[0..READ_BUFFER_SIZE - 1] of char;
  ReadCount: longint;
  Chunk: string;
  NewLinePos: integer;
  Line: string;
begin
  while (not Terminated) do
  begin
    try
      FSocket := TInetSocket.Create(FOwner.Host, FOwner.Port);
      SendLine('{"type":"bridge.ping","protocol":1}' + #10);
      FLineBuffer := '';

      while (not Terminated) do
      begin
        ReadCount := FSocket.Read(Buffer, SizeOf(Buffer));
        if (ReadCount <= 0) then
          Break;

        SetString(Chunk, PChar(@Buffer[0]), ReadCount);
        FLineBuffer := FLineBuffer + Chunk;
        NewLinePos := Pos(#10, FLineBuffer);
        while (NewLinePos > 0) do
        begin
          Line := Trim(Copy(FLineBuffer, 1, NewLinePos - 1));
          Delete(FLineBuffer, 1, NewLinePos);
          if (Line <> '') then
            HandleLine(Line);
          NewLinePos := Pos(#10, FLineBuffer);
        end;
      end;
    except
      // Connection failures are expected while the bridge is not running.
    end;

    CloseSocket;
    if (not Terminated) then
      Sleep(RECONNECT_DELAY_MS);
  end;
end;

procedure TRemoteBridgeIPCThread.HandleLine(const Line: string);
var
  Data: TJSONData;
begin
  Data := nil;
  try
    Data := GetJSON(Line);
    if (Data <> nil) and (Data.JSONType = jtObject) then
      HandleMessage(TJSONObject(Data));
  except
    // Ignore malformed bridge lines. The bridge/server handles diagnostics.
  end;
  FreeAndNil(Data);
end;

procedure TRemoteBridgeIPCThread.HandleMessage(Obj: TJSONObject);
var
  MessageType: string;
begin
  MessageType := JsonGetString(Obj, 'type', '');
  if (MessageType = 'bridge.ready') then
    FOwner.SetRoomStatus(true, UTF8String(JsonGetString(Obj, 'displayCode', '')))
  else if (MessageType = 'room.created') then
    FOwner.SetRoomStatus(true, UTF8String(JsonGetString(Obj, 'displayCode', '')))
  else if (MessageType = 'player.joined') or (MessageType = 'player.assigned') then
    HandlePlayerAssigned(Obj)
  else if (MessageType = 'player.disconnected') then
    HandlePlayerDisconnected(Obj)
  else if (MessageType = 'player.left') then
    HandlePlayerLeft(Obj)
  else if (MessageType = 'song.started') then
    HandleSongStarted(Obj)
  else if (MessageType = 'pitch.batch') then
    HandlePitchBatch(Obj)
  else if (MessageType = 'control.command') then
    HandleControlCommand(Obj)
  else if (MessageType = 'bridge.disconnected') then
  begin
    FOwner.ClearRoomStatus;
    RemoteInputProcessor.Clear;
  end;
end;

procedure TRemoteBridgeIPCThread.HandlePlayerAssigned(Obj: TJSONObject);
var
  Slot: integer;
  PlayerIndex: integer;
  PlayerId: UTF8String;
  Name: UTF8String;
  Role: UTF8String;
begin
  Slot := JsonGetInt(Obj, 'slot', 0);
  PlayerIndex := SlotToPlayerIndex(Slot);
  PlayerId := UTF8String(JsonGetString(Obj, 'playerId', ''));
  Name := UTF8String(JsonGetString(Obj, 'name', ''));
  Role := UTF8String(JsonGetString(Obj, 'role', 'singer'));
  FOwner.UpsertPlayer(PlayerId, Name, Role, Slot);
  if (Slot > 0) then
    RemoteInputProcessor.AssignPlayerSlot(
      PlayerIndex,
      PlayerId,
      Role,
      Slot
    );
  FOwner.SendPlayerAssigned(PlayerId, Role, Slot);
  FOwner.SendGameState('lobby', RemoteInputProcessor.CurrentSongSeq, 0);
end;

procedure TRemoteBridgeIPCThread.HandlePlayerDisconnected(Obj: TJSONObject);
begin
  FOwner.SetPlayerConnected(UTF8String(JsonGetString(Obj, 'playerId', '')), false);
  FOwner.SendGameState('lobby', RemoteInputProcessor.CurrentSongSeq, 0);
end;

procedure TRemoteBridgeIPCThread.HandlePlayerLeft(Obj: TJSONObject);
begin
  FOwner.RemovePlayer(UTF8String(JsonGetString(Obj, 'playerId', '')));
  FOwner.SendGameState('lobby', RemoteInputProcessor.CurrentSongSeq, 0);
end;

procedure TRemoteBridgeIPCThread.HandleSongStarted(Obj: TJSONObject);
begin
  RemoteInputProcessor.SetSongSeq(JsonGetInt(Obj, 'songSeq', 0));
end;

procedure TRemoteBridgeIPCThread.HandlePitchBatch(Obj: TJSONObject);
var
  Frames: TJSONArray;
  FrameObj: TJSONObject;
  FrameArr: TJSONArray;
  Frame: TRemotePitchFrame;
  I: integer;
  Slot: integer;
  PlayerIndex: integer;
  Added: integer;
  BatchSongSeq: integer;
  BaseSongTimeUs: int64;
  FrameDurUs: int64;
begin
  Slot := JsonGetInt(Obj, 'slot', 0);
  if (Slot <= 0) then
    Slot := JsonGetInt(Obj, 'playerSlot', 0);
  if (Slot <= 0) then
    Exit;

  PlayerIndex := SlotToPlayerIndex(Slot);
  Frames := JsonFindArray(Obj, 'frames');
  if (Frames = nil) then
    Exit;

  Added := 0;
  BatchSongSeq := JsonGetInt(Obj, 'songSeq', 0);
  BaseSongTimeUs := JsonGetInt64(Obj, 'baseSongTimeUs', -1);
  FrameDurUs := JsonGetInt64(Obj, 'frameDurUs', 0);
  for I := 0 to Frames.Count - 1 do
  begin
    Frame.SongSeq := BatchSongSeq;
    Frame.SongTimeUs := -1;
    Frame.DurUs := FrameDurUs;
    Frame.F0Hz := 0;
    Frame.F0Cents := 0;
    Frame.RmsDb := -120;
    Frame.Confidence := 0;
    Frame.Voiced := false;

    if (Frames.Items[I].JSONType = jtObject) then
    begin
      FrameObj := TJSONObject(Frames.Items[I]);
      Frame.SongTimeUs := JsonGetInt64(FrameObj, 'songTimeUs', -1);
      Frame.DurUs := JsonGetInt64(FrameObj, 'durUs', FrameDurUs);
      Frame.F0Hz := JsonGetFloat(FrameObj, 'f0Hz', 0);
      Frame.F0Cents := JsonGetInt(FrameObj, 'f0Cents', 0);
      Frame.RmsDb := JsonGetFloat(FrameObj, 'rmsDb', -120);
      Frame.Confidence := JsonGetFloat(FrameObj, 'confidence', 0);
      Frame.Voiced := JsonGetBool(FrameObj, 'voiced', false);
    end
    else if (Frames.Items[I].JSONType = jtArray) then
    begin
      FrameArr := TJSONArray(Frames.Items[I]);
      if (FrameArr.Count < 5) then
        Continue;
      Frame.SongTimeUs := JsonArrayGetInt64(FrameArr, 0, -1);
      if (BaseSongTimeUs >= 0) and (Frame.SongTimeUs >= 0) then
        Frame.SongTimeUs := BaseSongTimeUs + Frame.SongTimeUs;
      Frame.F0Cents := JsonArrayGetInt(FrameArr, 1, 0);
      Frame.RmsDb := JsonArrayGetInt(FrameArr, 2, -1200) / 10;
      Frame.Confidence := JsonArrayGetInt(FrameArr, 3, 0) / 255;
      Frame.Voiced := JsonArrayGetInt(FrameArr, 4, 0) <> 0;
    end
    else
      Continue;

    if (Frame.SongTimeUs >= 0) then
    begin
      RemoteInputProcessor.AddPitchFrame(PlayerIndex, Frame);
      Inc(Added);
    end;
  end;

  if (Added > 0) then
  begin
    Inc(remotePitchLogCounter);
  end;

  if (Added > 0) and ((remotePitchLogCounter = 1) or ((remotePitchLogCounter mod 100) = 0)) then
    Log.LogStatus(
      'pitch.batch slot=' + IntToStr(Slot) +
      ' playerIndex=' + IntToStr(PlayerIndex) +
      ' songSeq=' + IntToStr(JsonGetInt(Obj, 'songSeq', 0)) +
      ' frames=' + IntToStr(Added),
      'Remote Pitch'
    );
end;

procedure TRemoteBridgeIPCThread.HandleControlCommand(Obj: TJSONObject);
var
  Command: TRemoteControlCommand;
  Slot: integer;
  PlayerIndex: integer;
  ArgsData: TJSONData;
  Args: TJSONObject;
  SongIds: TJSONArray;
  ArgsText: UTF8String;
  ArgsIndex: integer;
  ArgsItemIndex: integer;
  I: integer;
begin
  if not TryParseRemoteControlCommand(
    UTF8String(JsonGetString(Obj, 'command', '')),
    Command
  ) then
    Exit;

  Slot := JsonGetInt(Obj, 'slot', 0);
  if (Slot <= 0) then
    Slot := JsonGetInt(Obj, 'playerSlot', 0);
  PlayerIndex := SlotToPlayerIndex(Slot);
  ArgsText := '';
  ArgsIndex := -1;
  ArgsItemIndex := -1;
  ArgsData := Obj.Find('args');
  if (ArgsData <> nil) and (ArgsData.JSONType = jtObject) then
  begin
    Args := TJSONObject(ArgsData);
    ArgsText := UTF8String(JsonGetString(Args, 'text', ''));
    SongIds := JsonFindArray(Args, 'songIds');
    if (SongIds <> nil) then
    begin
      ArgsText := '';
      for I := 0 to SongIds.Count - 1 do
      begin
        if (I > 0) then
          ArgsText := ArgsText + ',';
        ArgsText := ArgsText + UTF8String(IntToStr(SongIds.Items[I].AsInteger));
      end;
    end;
    ArgsIndex := JsonGetInt(Args, 'playlistIndex',
      JsonGetInt(Args, 'songId', JsonGetInt(Args, 'index', -1)));
    ArgsItemIndex := JsonGetInt(Args, 'itemIndex', JsonGetInt(Args, 'songId', -1));
  end;

  if Assigned(FOwner.OnCommand) then
    FOwner.OnCommand(
      PlayerIndex,
      UTF8String(JsonGetString(Obj, 'playerId', '')),
      Command,
      JsonGetInt(Obj, 'commandId', 0),
      ArgsText,
      ArgsIndex,
      ArgsItemIndex
    );
end;

end.
