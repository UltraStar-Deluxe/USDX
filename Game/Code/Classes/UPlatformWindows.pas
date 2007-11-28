unit UPlatformWindows;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses Classes, UPlatform;

type

  TPlatformWindows = class(TPlatform)
  public
    Function DirectoryFindFiles(Dir, Filter : WideString; ReturnAllSubDirs : Boolean) : TDirectoryEntryArray; override;
    function TerminateIfAlreadyRunning(var WndTitle : String) : Boolean; override;
  end;

implementation

uses SysUtils, Windows;

type

  TSearchRecW = record
    Time: Integer;
    Size: Integer;
    Attr: Integer;
    Name: WideString;
    ExcludeAttr: Integer;
    FindHandle: THandle;
    FindData: TWin32FindDataW;
  end;

function  FindFirstW(const Path: WideString; Attr: Integer; var  F: TSearchRecW): Integer; forward;
function  FindNextW(var F: TSearchRecW): Integer; forward;
procedure FindCloseW(var F: TSearchRecW); forward;
function  FindMatchingFileW(var F: TSearchRecW): Integer; forward;
function  DirectoryExistsW(const Directory: widestring): Boolean; forward;

function FindFirstW(const Path: widestring; Attr: Integer; var  F: TSearchRecW): Integer;
const
  faSpecial = faHidden or faSysFile or faVolumeID or faDirectory;
begin
  F.ExcludeAttr := not Attr and faSpecial;
{$IFDEF Delphi}
  F.FindHandle  := FindFirstFileW(PWideChar(Path), F.FindData);
{$ELSE}
  F.FindHandle  := FindFirstFileW(PWideChar(Path), @F.FindData);
{$ENDIF}
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Result := FindMatchingFileW(F);
    if Result <> 0 then FindCloseW(F);
  end else
    Result := GetLastError;
end;

function FindNextW(var F: TSearchRecW): Integer;
begin
{$IFDEF Delphi}
  if FindNextFileW(F.FindHandle, F.FindData) then
{$ELSE}
  if FindNextFileW(F.FindHandle, @F.FindData) then
{$ENDIF}
    Result := FindMatchingFileW(F)
  else
    Result := GetLastError;
end;

procedure FindCloseW(var F: TSearchRecW);
begin
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(F.FindHandle);
    F.FindHandle := INVALID_HANDLE_VALUE;
  end;
end;

function FindMatchingFileW(var F: TSearchRecW): Integer;
var
  LocalFileTime: TFileTime;
begin
  with F do
  begin
    while FindData.dwFileAttributes and ExcludeAttr <> 0 do
{$IFDEF Delphi}
      if not FindNextFileW(FindHandle, FindData) then
{$ELSE}
      if not FindNextFileW(FindHandle, @FindData) then
{$ENDIF}
      begin
        Result := GetLastError;
        Exit;
      end;
    FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, LongRec(Time).Hi, LongRec(Time).Lo);
    Size := FindData.nFileSizeLow;
    Attr := FindData.dwFileAttributes;
    Name := FindData.cFileName;
  end;
  Result := 0;
end;

function DirectoryExistsW(const Directory: widestring): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributesW(PWideChar(Directory));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

//------------------------------
//Start more than One Time Prevention
//------------------------------
function TPlatformWindows.TerminateIfAlreadyRunning(var WndTitle : String) : Boolean;
var
  hWnd: THandle;
  I: Integer;
begin
    Result := false;
    hWnd:= FindWindow(nil, PChar(WndTitle));
    //Programm already started
    if (hWnd <> 0) then
    begin
      I := Messagebox(0, PChar('Another Instance of Ultrastar is already running. Continue ?'), PChar(WndTitle), MB_ICONWARNING or MB_YESNO);
      if (I = IDYes) then
      begin
        I := 1;
        repeat
          Inc(I);
          hWnd := FindWindow(nil, PChar(WndTitle + ' Instance ' + InttoStr(I)));
        until (hWnd = 0);
        WndTitle := WndTitle + ' Instance ' + InttoStr(I);
      end
      else
        Result := true;
    end;
end;

Function TPlatformWindows.DirectoryFindFiles(Dir, Filter : WideString; ReturnAllSubDirs : Boolean) : TDirectoryEntryArray;
var
    i : Integer;
    SR : TSearchRecW;
    lAttrib : Integer;
begin
  i := 0;
  Filter := LowerCase(Filter);

  if FindFirstW(Dir + '*', faAnyFile or faDirectory, SR) = 0 then
  repeat
    if (SR.Name <> '.') and (SR.Name <> '..') then
    begin
      lAttrib := FileGetAttr(Dir + SR.name);
      if ReturnAllSubDirs and ((lAttrib and faDirectory) <> 0) then
      begin
        SetLength( Result, i + 1);
        Result[i].Name        := SR.name;
        Result[i].IsDirectory := true;
        Result[i].IsFile      := false;
        i := i + 1;
      end
      else if (Length(Filter) = 0) or (Pos( Filter, LowerCase(SR.Name)) > 0) then
      begin
        SetLength( Result, i + 1);
        Result[i].Name        := SR.Name;
        Result[i].IsDirectory := false;
        Result[i].IsFile      := true;
        i := i + 1;
      end;
    end;
  until FindNextW(SR) <> 0;
  FindCloseW(SR);
end;

end.
