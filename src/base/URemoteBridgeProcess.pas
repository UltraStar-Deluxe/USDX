{* UltraStar Deluxe - Karaoke Game
 *
 * Optional helper process launcher for game/webs/usdx-bridge.mjs.
 *}

unit URemoteBridgeProcess;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

type
  TRemoteBridgeProcess = class
  private
    FProcess: TObject;
    FStartedByGame: boolean;
    FLastError: string;
    function BridgeScriptPath: string;
    function NodeExecutablePath: string;
    function RemoteServerUrl: string;
    function IsIpcAvailable: boolean;
    procedure RequestBridgeShutdown;
  public
    destructor Destroy; override;

    procedure Start(const WebApp: string = '');
    procedure Stop;

    property StartedByGame: boolean read FStartedByGame;
    property LastError: string read FLastError;
  end;

function RemoteBridgeProcess(): TRemoteBridgeProcess;

implementation

uses
  Classes,
  Process,
  ssockets,
  SysUtils,
  UIni
  {$IFDEF UNIX}, BaseUnix{$ENDIF};

const
  IPC_CONNECT_TIMEOUT_MS = 100;
  IPC_WRITE_TIMEOUT_MS = 100;

var
  singleton_RemoteBridgeProcess: TRemoteBridgeProcess = nil;

function RemoteBridgeProcess(): TRemoteBridgeProcess;
begin
  if singleton_RemoteBridgeProcess = nil then
    singleton_RemoteBridgeProcess := TRemoteBridgeProcess.Create;

  Result := singleton_RemoteBridgeProcess;
end;

function TRemoteBridgeProcess.BridgeScriptPath: string;
begin
  Result := Ini.RemoteBridgeScriptPath;
  if (Result <> '') then
  begin
    Result := ExpandFileName(Result);
    Exit;
  end;

  Result := ExpandFileName(ExtractFilePath(ParamStr(0)) +
    'webs' + DirectorySeparator + 'usdx-bridge.mjs');
end;

function TRemoteBridgeProcess.NodeExecutablePath: string;
begin
  Result := GetEnvironmentVariable('USDX_REMOTE_NODE');
  if (Result <> '') then
    Exit;

  Result := Ini.RemoteBridgeNodeExecutable;
  if (Result <> '') then
    Exit;

  {$IF Defined(MSWindows)}
  Result := 'C:\Program Files\nodejs\node.exe';
  if FileExists(Result) then
    Exit;

  Result := 'C:\Program Files (x86)\nodejs\node.exe';
  if FileExists(Result) then
    Exit;
  {$IFEND}

  Result := 'node';
end;

function TRemoteBridgeProcess.RemoteServerUrl: string;
begin
  Result := GetEnvironmentVariable('USDX_REMOTE_SERVER');
  if (Result = '') then
    Result := Ini.RemoteBridgeServerUrl;
  if (Result = '') then
    Result := DEFAULT_REMOTE_BRIDGE_SERVER_URL;
end;

function TRemoteBridgeProcess.IsIpcAvailable: boolean;
var
  Socket: TInetSocket;
begin
  Result := false;
  Socket := nil;
  try
    Socket := TInetSocket.Create(Ini.RemoteBridgeIpcHost, Ini.RemoteBridgeIpcPort, IPC_CONNECT_TIMEOUT_MS);
    Socket.IOTimeout := IPC_WRITE_TIMEOUT_MS;
    Result := true;
  except
    Result := false;
  end;
  Socket.Free;
end;

procedure TRemoteBridgeProcess.RequestBridgeShutdown;
var
  Socket: TInetSocket;
  Line: string;
begin
  Socket := nil;
  try
    Socket := TInetSocket.Create(Ini.RemoteBridgeIpcHost, Ini.RemoteBridgeIpcPort, IPC_CONNECT_TIMEOUT_MS);
    Socket.IOTimeout := IPC_WRITE_TIMEOUT_MS;
    Line := '{"type":"bridge.shutdown"}' + #10;
    Socket.Write(Pointer(Line)^, Length(Line));
  except
    // The bridge may already be gone; Stop will still terminate the process handle.
  end;
  Socket.Free;
end;

procedure ApplyStartCommand(Proc: TProcess; const CommandLine: string);
var
  Args: TStringList;
  Current: string;
  I: integer;
  InQuote: boolean;

  procedure PushCurrent;
  begin
    if (Current <> '') then
    begin
      Args.Add(Current);
      Current := '';
    end;
  end;

begin
  Args := TStringList.Create;
  try
    Current := '';
    InQuote := false;
    for I := 1 to Length(CommandLine) do
    begin
      if (CommandLine[I] = '"') then
        InQuote := not InQuote
      else if (not InQuote) and (CommandLine[I] in [#9, ' ']) then
        PushCurrent
      else
        Current := Current + CommandLine[I];
    end;
    PushCurrent;

    if (Args.Count = 0) then
      Exit;

    Proc.Executable := Args[0];
    for I := 1 to Args.Count - 1 do
      Proc.Parameters.Add(Args[I]);
  finally
    Args.Free;
  end;
end;

procedure TRemoteBridgeProcess.Start(const WebApp: string);
var
  Proc: TProcess;
  ScriptPath: string;
  RequestedWebApp: string;
  I: integer;
begin
  if (FProcess <> nil) then
    Exit;

  if IsIpcAvailable then
  begin
    RequestBridgeShutdown;
    for I := 1 to 20 do
    begin
      Sleep(100);
      if not IsIpcAvailable then
        Break;
    end;

    if IsIpcAvailable then
    begin
      FLastError := 'Remote bridge IPC port is already in use';
      Exit;
    end;
  end;

  ScriptPath := '';
  if (Ini.RemoteBridgeStartCommand = '') then
    ScriptPath := BridgeScriptPath;
  if (ScriptPath <> '') and (not FileExists(ScriptPath)) then
  begin
    FLastError := 'Bridge script not found: ' + ScriptPath;
    Exit;
  end;

  RequestedWebApp := Trim(GetEnvironmentVariable('USDX_REMOTE_WEB_APP'));
  if (RequestedWebApp = '') then
    RequestedWebApp := Trim(WebApp);
  if (RequestedWebApp = '') then
    RequestedWebApp := Trim(Ini.RemoteBridgeWebApp);

  Proc := TProcess.Create(nil);
  try
    if (Ini.RemoteBridgeStartCommand <> '') then
      ApplyStartCommand(Proc, Ini.RemoteBridgeStartCommand)
    else
    begin
      Proc.Executable := NodeExecutablePath;
      Proc.Parameters.Add(ScriptPath);
      Proc.Parameters.Add('--server');
      Proc.Parameters.Add(RemoteServerUrl);
      Proc.Parameters.Add('--ipc-host');
      Proc.Parameters.Add(Ini.RemoteBridgeIpcHost);
      Proc.Parameters.Add('--ipc-port');
      Proc.Parameters.Add(IntToStr(Ini.RemoteBridgeIpcPort));
      Proc.Parameters.Add('--mock-song=false');
      Proc.Parameters.Add('--auto-ack=false');
      Proc.Parameters.Add('--auto-assign=false');
      Proc.Parameters.Add('--p2p=false');
      if (RequestedWebApp <> '') then
      begin
        Proc.Parameters.Add('--web-app');
        Proc.Parameters.Add(RequestedWebApp);
      end;
      Proc.Parameters.Add('--parent-pid');
      Proc.Parameters.Add(IntToStr(GetProcessID));
    end;
    Proc.Options := [poNoConsole];
    Proc.Execute;
    FProcess := Proc;
    FStartedByGame := true;
    FLastError := '';
  except
    on E: Exception do
    begin
      FLastError := E.Message;
      Proc.Free;
    end;
  end;
end;

procedure TRemoteBridgeProcess.Stop;
var
  Proc: TProcess;
begin
  if (FProcess = nil) then
    Exit;

  Proc := TProcess(FProcess);
  FProcess := nil;
  try
    if Proc.Running then
    begin
      RequestBridgeShutdown;
      if Proc.WaitOnExit(1500) then
        Exit;

      Proc.Terminate(0);
      if (not Proc.WaitOnExit(1000)) and Proc.Running then
      begin
        {$IFDEF UNIX}
        fpKill(Proc.ProcessID, SIGKILL);
        Proc.WaitOnExit(500);
        {$ELSE}
        Proc.Terminate(1);
        Proc.WaitOnExit(500);
        {$ENDIF}
      end;
    end;
  finally
    Proc.Free;
    FStartedByGame := false;
  end;
end;

destructor TRemoteBridgeProcess.Destroy;
begin
  Stop;
  inherited;
end;

initialization

finalization
  FreeAndNil(singleton_RemoteBridgeProcess);

end.
