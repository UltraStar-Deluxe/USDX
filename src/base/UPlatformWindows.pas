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
 * $URL$
 * $Id$
 *}

unit UPlatformWindows;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

// turn off messages for platform specific symbols
{$WARN SYMBOL_PLATFORM OFF}

uses
  Classes,
  UPlatform;

type
  TPlatformWindows = class(TPlatform)
    private
      function GetSpecialPath(CSIDL: integer): WideString;
    public
      function DirectoryFindFiles(Dir, Filter: WideString; ReturnAllSubDirs: Boolean): TDirectoryEntryArray; override;
      function TerminateIfAlreadyRunning(var WndTitle: String): Boolean; override;

      function GetLogPath: WideString; override;
      function GetGameSharedPath: WideString; override;
      function GetGameUserPath: WideString; override;

      function CopyFile(const Source, Target: WideString; FailIfExists: boolean): boolean; override;
  end;

implementation

uses
  SysUtils,
  ShlObj,
  Windows,
  UConfig;

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

function  FindFirstW(const Path: WideString; Attr: Integer; var F: TSearchRecW): Integer; forward;
function  FindNextW(var F: TSearchRecW): Integer; forward;
procedure FindCloseW(var F: TSearchRecW); forward;
function  FindMatchingFileW(var F: TSearchRecW): Integer; forward;
function  DirectoryExistsW(const Directory: widestring): Boolean; forward;

function FindFirstW(const Path: widestring; Attr: Integer; var F: TSearchRecW): Integer;
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
function TPlatformWindows.TerminateIfAlreadyRunning(var WndTitle: String): Boolean;
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

function TPlatformWindows.DirectoryFindFiles(Dir, Filter: WideString; ReturnAllSubDirs: Boolean): TDirectoryEntryArray;
var
    i : Integer;
    SR : TSearchRecW;
    Attrib : Integer;
begin
  i := 0;
  Filter := LowerCase(Filter);

  if FindFirstW(Dir + '*', faAnyFile or faDirectory, SR) = 0 then
  repeat
    if (SR.Name <> '.') and (SR.Name <> '..') then
    begin
      Attrib := FileGetAttr(Dir + SR.name);
      if ReturnAllSubDirs and ((Attrib and faDirectory) <> 0) then
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

(**
 * Returns the path of a special folder.
 *
 * Some Folder IDs:
 * CSIDL_APPDATA       (e.g. C:\Documents and Settings\username\Application Data)
 * CSIDL_LOCAL_APPDATA (e.g. C:\Documents and Settings\username\Local Settings\Application Data)
 * CSIDL_PROFILE       (e.g. C:\Documents and Settings\username)
 * CSIDL_PERSONAL      (e.g. C:\Documents and Settings\username\My Documents)
 * CSIDL_MYMUSIC       (e.g. C:\Documents and Settings\username\My Documents\My Music)
 *)
function TPlatformWindows.GetSpecialPath(CSIDL: integer): WideString;
var
  Buffer: array [0..MAX_PATH-1] of WideChar;
begin
{$IF Defined(Delphi) or (FPC_VERSION_INT >= 2002002)} // >= 2.2.2
  if (SHGetSpecialFolderPathW(0, @Buffer, CSIDL, false)) then
    Result := Buffer
  else
{$IFEND}
    Result := '';
end;

function TPlatformWindows.GetLogPath: WideString;
begin
  Result := GetExecutionDir();
end;

function TPlatformWindows.GetGameSharedPath: WideString;
begin
  Result := GetExecutionDir();
end;

function TPlatformWindows.GetGameUserPath: WideString;
begin
  //Result := GetSpecialPath(CSIDL_APPDATA) + PathDelim + 'UltraStarDX' + PathDelim;
  Result := GetExecutionDir();
end;

function TPlatformWindows.CopyFile(const Source, Target: WideString; FailIfExists: boolean): boolean;
begin
  Result := Windows.CopyFileW(PWideChar(Source), PWideChar(Target), FailIfExists);
end;

end.
