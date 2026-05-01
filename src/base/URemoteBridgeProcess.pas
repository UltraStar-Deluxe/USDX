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
  public
    destructor Destroy; override;

    procedure Start;
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
  SysUtils;

const
  DEFAULT_REMOTE_SERVER = 'wss://usdx.at/ws/host';
  DEFAULT_IPC_HOST = '127.0.0.1';
  DEFAULT_IPC_PORT = 8765;

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
  Result := ExpandFileName(ExtractFilePath(ParamStr(0)) +
    'webs' + DirectorySeparator + 'usdx-bridge.mjs');
end;

function TRemoteBridgeProcess.NodeExecutablePath: string;
begin
  Result := GetEnvironmentVariable('USDX_REMOTE_NODE');
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
    Result := DEFAULT_REMOTE_SERVER;
end;

function TRemoteBridgeProcess.IsIpcAvailable: boolean;
var
  Socket: TInetSocket;
begin
  Result := false;
  Socket := nil;
  try
    Socket := TInetSocket.Create(DEFAULT_IPC_HOST, DEFAULT_IPC_PORT);
    Result := true;
  except
    Result := false;
  end;
  Socket.Free;
end;

procedure TRemoteBridgeProcess.Start;
var
  Proc: TProcess;
  ScriptPath: string;
begin
  if (FProcess <> nil) or IsIpcAvailable then
    Exit;

  ScriptPath := BridgeScriptPath;
  if (not FileExists(ScriptPath)) then
  begin
    FLastError := 'Bridge script not found: ' + ScriptPath;
    Exit;
  end;

  Proc := TProcess.Create(nil);
  try
    Proc.Executable := NodeExecutablePath;
    Proc.Parameters.Add(ScriptPath);
    Proc.Parameters.Add('--server');
    Proc.Parameters.Add(RemoteServerUrl);
    Proc.Parameters.Add('--mock-song=false');
    Proc.Parameters.Add('--auto-ack=false');
    Proc.Parameters.Add('--auto-assign=false');
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
      Proc.Terminate(0);
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
