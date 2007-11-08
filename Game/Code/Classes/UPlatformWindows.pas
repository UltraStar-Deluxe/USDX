unit UPlatformWindows;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses Classes, UPlatform;

type
  
  TPlatform = class(TInterfacedObject, IPlatform)
  public
    Function DirectoryFindFiles(Dir, Filter : WideString; ReturnAllSubDirs : Boolean) : TDirectoryEntryArray; 
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
  F.FindHandle  := FindFirstFileW(PWideChar(Path), F.FindData);
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Result := FindMatchingFileW(F);
    if Result <> 0 then FindCloseW(F);
  end else
    Result := GetLastError;
end;

function FindNextW(var F: TSearchRecW): Integer;
begin
  if FindNextFileW(F.FindHandle, F.FindData) then
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
      if not FindNextFileW(FindHandle, FindData) then
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

Function TPlatform.DirectoryFindFiles(Dir, Filter : WideString; ReturnAllSubDirs : Boolean) : TDirectoryEntryArray; 
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
