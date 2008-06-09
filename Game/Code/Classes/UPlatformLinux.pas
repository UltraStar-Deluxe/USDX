unit UPlatformLinux;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
  UPlatform;

type

  TPlatformLinux = class(TInterfacedObject, IPlatform)
    private
      function GetHomeDir(): string;
    public
      function DirectoryFindFiles(Dir, Filter : WideString; ReturnAllSubDirs : Boolean) : TDirectoryEntryArray;
      function TerminateIfAlreadyRunning(var WndTitle : String) : Boolean;
      function FindSongFile(Dir, Mask: widestring): widestring;

      procedure Halt;

      function GetLogPath        : WideString;
      function GetGameSharedPath : WideString;
      function GetGameUserPath   : WideString;
  end;

implementation

uses
  UCommandLine,
  BaseUnix,
  {$IFDEF FPC_VERSION_2_2_2_PLUS}
  pwd,
  {$ENDIF}
  SysUtils,
  ULog,
  UConfig;

function TPlatformLinux.DirectoryFindFiles(Dir, Filter : WideString; ReturnAllSubDirs : Boolean) : TDirectoryEntryArray;
var
  i : Integer;
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

function TPlatformLinux.GetLogPath        : WideString;
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

  forcedirectories( result );
end;

function TPlatformLinux.GetGameSharedPath : WideString;
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
{$IFDEF FPC_VERSION_2_2_2_PLUS}
var
  PasswdEntry: PPasswd;
{$ENDIF}
begin
  Result := '';

  {$IFDEF FPC_VERSION_2_2_2_PLUS}
  // try to retrieve the info from passwd
  PasswdEntry := FpGetpwuid(FpGetuid());
  if (PasswdEntry <> nil) then
    Result := PasswdEntry.pw_dir;
  {$ENDIF}
  // fallback if passwd does not contain the path
  if (Result = '') then
    Result := GetEnvironmentVariable('HOME');
  // add trailing path delimiter (normally '/')
  if (Result <> '') then
    Result := IncludeTrailingPathDelimiter(Result);

  {$IFDEF FPC_VERSION_2_2_2_PLUS}
  // GetUserDir() is another function that returns a user path.
  // It uses env-var HOME or a fallback to a temp-dir.
  //Result := GetUserDir();
  {$ENDIF}
end;

// FIXME: Maybe this should be TPlatformBase.Halt() for all platforms
procedure TPlatformLinux.Halt;
begin
  System.Halt;
end;

function TPlatformLinux.TerminateIfAlreadyRunning(var WndTitle : String) : Boolean;
begin
  // Linux does not check for running apps at the moment
  Result := false;
end;

// FIXME: just a dirty-fix to make the linux build work again.
//        This i the same as the corresponding function for windows
//        (and MacOSX?).
//        Maybe this should be TPlatformBase.FindSongFile()
function TPlatformLinux.FindSongFile(Dir, Mask: widestring): widestring;
var
  SR:     TSearchRec;   // for parsing song directory
begin
  Result := '';
  if SysUtils.FindFirst(Dir + Mask, faDirectory, SR) = 0 then 
  begin
    Result := SR.Name;
  end; // if
  SysUtils.FindClose(SR);
end;

end.
