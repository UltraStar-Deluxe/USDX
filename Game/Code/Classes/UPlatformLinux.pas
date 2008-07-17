unit UPlatformLinux;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
  UPlatform,
  UConfig;

type
  TPlatformLinux = class(TPlatform)
    private
      function GetHomeDir(): string;
    public
      function DirectoryFindFiles(Dir, Filter: WideString; ReturnAllSubDirs: Boolean): TDirectoryEntryArray; override;

      function GetLogPath        : WideString; override;
      function GetGameSharedPath : WideString; override;
      function GetGameUserPath   : WideString; override;
  end;

implementation

uses
  UCommandLine,
  BaseUnix,
  {$IF FPC_VERSION_INT >= 2002002}
  pwd,
  {$IFEND}
  SysUtils,
  ULog;

function TPlatformLinux.DirectoryFindFiles(Dir, Filter: WideString; ReturnAllSubDirs: Boolean): TDirectoryEntryArray;
var
  i: Integer;
  TheDir  : pDir;
  ADirent : pDirent;
  Entry   : Longint;
  lAttrib : integer;
begin
  i := 0;
  Filter := LowerCase(Filter);

  TheDir := FpOpenDir( Dir );
  if Assigned(TheDir) then
  begin
    repeat
      ADirent :=  FpReadDir(TheDir^);

      if Assigned(ADirent) and (ADirent^.d_name <> '.') and (ADirent^.d_name <> '..') then
      begin
        lAttrib := FileGetAttr(Dir + ADirent^.d_name);
        if ReturnAllSubDirs and ((lAttrib and faDirectory) <> 0) then
        begin
          SetLength( Result, i + 1);
          Result[i].Name        := ADirent^.d_name;
          Result[i].IsDirectory := true;
          Result[i].IsFile      := false;
          i := i + 1;
        end
        else if (Length(Filter) = 0) or (Pos( Filter, LowerCase(ADirent^.d_name)) > 0) then
        begin
          SetLength( Result, i + 1);
          Result[i].Name        := ADirent^.d_name;
          Result[i].IsDirectory := false;
          Result[i].IsFile      := true;
          i := i + 1;
        end;
      end;
    until (ADirent = nil);

    FpCloseDir(TheDir^);
  end;
end;

function TPlatformLinux.GetLogPath: WideString;
begin
  if FindCmdLineSwitch( cUseLocalPaths ) then
  begin
    Result := ExtractFilePath(ParamStr(0));
  end
  else
  begin
    {$IFDEF UseLocalDirs}
      Result := ExtractFilePath(ParamStr(0));
    {$ELSE}
      Result := LogPath + PathDelim;
    {$ENDIF}
  end;

  // create non-existing directories
  ForceDirectories(Result);
end;

function TPlatformLinux.GetGameSharedPath: WideString;
begin
  if FindCmdLineSwitch( cUseLocalPaths ) then
    Result := ExtractFilePath(ParamStr(0))
  else
  begin
    {$IFDEF UseLocalDirs}
    Result := ExtractFilePath(ParamStr(0));
    {$ELSE}
    Result := SharedPath + PathDelim;
    {$ENDIF}
  end;
end;

function TPlatformLinux.GetGameUserPath: WideString;
begin
  if FindCmdLineSwitch( cUseLocalPaths ) then
    Result := ExtractFilePath(ParamStr(0))
  else
  begin
    {$IFDEF UseLocalDirs}
    Result := ExtractFilePath(ParamStr(0));
    {$ELSE}
    Result := GetHomeDir() + '.'+PathSuffix + PathDelim;
    {$ENDIF}
  end;
end;

(**
 * Returns the user's home directory terminated by a path delimiter
 *)
function TPlatformLinux.GetHomeDir(): string;
{$IF FPC_VERSION_INT >= 2002002}
var
  PasswdEntry: PPasswd;
{$IFEND}
begin
  Result := '';

  {$IF FPC_VERSION_INT >= 2002002}
  // try to retrieve the info from passwd
  PasswdEntry := FpGetpwuid(FpGetuid());
  if (PasswdEntry <> nil) then
    Result := PasswdEntry.pw_dir;
  {$IFEND}
  // fallback if passwd does not contain the path
  if (Result = '') then
    Result := GetEnvironmentVariable('HOME');
  // add trailing path delimiter (normally '/')
  if (Result <> '') then
    Result := IncludeTrailingPathDelimiter(Result);

  {$IF FPC_VERSION_INT >= 2002002}
  // GetUserDir() is another function that returns a user path.
  // It uses env-var HOME or a fallback to a temp-dir.
  //Result := GetUserDir();
  {$IFEND}
end;

end.
