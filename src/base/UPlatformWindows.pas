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
  UPlatform,
  UPath;

type
  TPlatformWindows = class(TPlatform)
    private
      function GetSpecialPath(CSIDL: integer): IPath;
    public
      function TerminateIfAlreadyRunning(var WndTitle: String): Boolean; override;

      function GetLogPath: IPath; override;
      function GetGameSharedPath: IPath; override;
      function GetGameUserPath: IPath; override;
  end;

implementation

uses
  SysUtils,
  ShlObj,
  Windows,
  UConfig;

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
function TPlatformWindows.GetSpecialPath(CSIDL: integer): IPath;
var
  Buffer: array [0..MAX_PATH-1] of WideChar;
begin
  if (SHGetSpecialFolderPathW(0, @Buffer, CSIDL, false)) then
    Result := Path(Buffer)
  else
    Result := PATH_NONE;
end;

function TPlatformWindows.GetLogPath: IPath;
begin
  Result := GetExecutionDir();
end;

function TPlatformWindows.GetGameSharedPath: IPath;
begin
  Result := GetExecutionDir();
end;

function TPlatformWindows.GetGameUserPath: IPath;
begin
  //Result := GetSpecialPath(CSIDL_APPDATA).Append('UltraStarDX', pdAppend);
  Result := GetExecutionDir();
end;

end.
