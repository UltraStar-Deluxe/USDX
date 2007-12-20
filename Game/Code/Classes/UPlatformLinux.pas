unit UPlatformLinux;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses Classes, UPlatform;

type

  TPlatformLinux = class(TPlatform)
    function get_homedir(): string;
  public
    Function DirectoryFindFiles(Dir, Filter : WideString; ReturnAllSubDirs : Boolean) : TDirectoryEntryArray; override;
    function TerminateIfAlreadyRunning(var WndTitle : String) : Boolean; override;
    
    function GetLogPath        : WideString; override;
    function GetGameSharedPath : WideString; override;
    function GetGameUserPath   : WideString; override;
  end;

implementation

uses 
  libc,
{$IFNDEF FPC_V220}
  oldlinux,
{$ELSE}
  BaseUnix,
{$ENDIF}
  SysUtils;

{$IFDEF FPC_V220}
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
  result := '/var/log/UltraStarDeluxe/';
end;

function TPlatformLinux.GetGameSharedPath : WideString;
begin
  result := '/usr/share/UltraStarDeluxe/';
end;

function TPlatformLinux.GetGameUserPath   : WideString;
begin
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

function TPlatformLinux.TerminateIfAlreadyRunning(var WndTitle : String) : Boolean;
begin
  // Linux and Mac don't check for running apps at the moment
  Result := false;
end;

end.
