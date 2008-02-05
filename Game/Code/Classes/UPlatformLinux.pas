unit UPlatformLinux;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses Classes, UPlatform;

type

  TPlatformLinux = class(TInterfacedObject, IPlatform)
    function get_homedir(): string;
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

// check for version of FPC >= 2.2.0
{$IFDEF FPC}
  {$IF (FPC_VERSION > 2) or ((FPC_VERSION = 2) and (FPC_RELEASE >= 2))}
    {$DEFINE FPC_VERSION_2_2_0_PLUS}
  {$IFEND}
{$ENDIF}
  
uses 
  libc,
  uCommandLine,
{$IFDEF FPC_VERSION_2_2_0_PLUS}
  BaseUnix,
{$ELSE}
  oldlinux,
{$ENDIF}
  SysUtils;

{$IFDEF FPC_VERSION_2_2_0_PLUS}
Function TPlatformLinux.DirectoryFindFiles(Dir, Filter : WideString; ReturnAllSubDirs : Boolean) : TDirectoryEntryArray;
var
    i : Integer;
    TheDir  : pDir;
    ADirent : pDirent;
    Entry   : Longint;
    //info    : oldlinux.stat;
    lAttrib : integer;
begin
  i := 0;
  Filter := LowerCase(Filter);

  TheDir := FpOpenDir( Dir );
  if Assigned(TheDir) then
  repeat
    ADirent :=  FpReadDir(TheDir^);

    If Assigned(ADirent) and (ADirent^.d_name <> '.') and (ADirent^.d_name <> '..') then
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
  Until ADirent = nil;

  FpCloseDir(TheDir^);
end;
{$ELSE}
Function TPlatformLinux.DirectoryFindFiles(Dir, Filter : WideString; ReturnAllSubDirs : Boolean) : TDirectoryEntryArray;
var
    i : Integer;
    TheDir  : oldlinux.pdir;
    ADirent : oldlinux.pDirent;
    Entry   : Longint;
    info    : oldlinux.stat;
    lAttrib   : integer;
begin
  i := 0;
  Filter := LowerCase(Filter);

  TheDir := oldlinux.opendir( Dir );
  if Assigned(TheDir) then
  repeat
    ADirent :=  oldlinux.ReadDir(TheDir);

    If Assigned(ADirent) and (ADirent^.name <> '.') and (ADirent^.name <> '..') then
    begin
      lAttrib := FileGetAttr(Dir + ADirent^.name);
      if ReturnAllSubDirs and ((lAttrib and faDirectory) <> 0) then
      begin
        SetLength( Result, i + 1);
        Result[i].Name        := ADirent^.name;
        Result[i].IsDirectory := true;
        Result[i].IsFile      := false;
        i := i + 1;
      end
      else if (Length(Filter) = 0) or (Pos( Filter, LowerCase(ADirent^.name)) > 0) then
      begin
        SetLength( Result, i + 1);
        Result[i].Name        := ADirent^.name;
        Result[i].IsDirectory := false;
        Result[i].IsFile      := true;
        i := i + 1;
      end;
    end;
  Until ADirent = nil;

  oldlinux.CloseDir(TheDir);
end;
{$ENDIF}


function TPlatformLinux.GetLogPath        : WideString;
begin
  if FindCmdLineSwitch( cUseLocalPaths ) then
    result := ExtractFilePath(ParamStr(0))
  else
    result := '/var/log/UltraStarDeluxe/';
end;

function TPlatformLinux.GetGameSharedPath : WideString;
begin
  if FindCmdLineSwitch( cUseLocalPaths ) then
    result := ExtractFilePath(ParamStr(0))
  else
    result := '/usr/share/UltraStarDeluxe/';
end;

function TPlatformLinux.GetGameUserPath   : WideString;
begin
  if FindCmdLineSwitch( cUseLocalPaths ) then
    result := ExtractFilePath(ParamStr(0))
  else
    result := get_homedir()+'/.UltraStarDeluxe/';
end;

function TPlatformLinux.get_homedir(): string;
var
  pPasswdEntry : Ppasswd;
  lUserName    : String;
begin
  pPasswdEntry := getpwuid( getuid() );
  result       := pPasswdEntry^.pw_dir;
end;

// FIXME: just a dirty-fix to make the linux build work again.
//        This i the same as the corresponding function for MacOSX.
//        Maybe this should be TPlatformBase.Halt()
procedure TPlatformLinux.Halt;
begin
  halt();
end;

function TPlatformLinux.TerminateIfAlreadyRunning(var WndTitle : String) : Boolean;
begin
  // Linux and Mac don't check for running apps at the moment
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
