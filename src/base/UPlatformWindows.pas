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
      UseLocalDirs: boolean;
      
      function GetSpecialPath(CSIDL: integer): IPath;
      procedure DetectLocalExecution();
    public
      procedure Init; override;
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

procedure TPlatformWindows.Init;
begin
  inherited Init();
  DetectLocalExecution();
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

{**
 * Detects whether the game was executed locally or globally.
 * - It is local if config.ini exists in the directory of the
 *   executable. Config files like config.ini or score db
 *   reside in the directory of the executable. This is useful
 *   to enable windows users to have a portable installation
 *   e.g. on an external hdd. This is also the default behaviour
 *   of usdx prior to version 1.1
 * - It is global if no config.ini exists in the directory of the
 *   executable. The config files are in a separate folder
 *   (e.g. %APPDATA%\ultrastardx)
 * On windows, resources (themes, language-files)
 * reside in the directory of the executable in any case
 *
 * Sets UseLocalDirs to true if the game is executed locally, false otherwise.
 *}
procedure TPlatformWindows.DetectLocalExecution();
var
  LocalDir, ConfigIni: IPath;
begin
  // we just check if 'config.ini' exists in the
  // directory of the executable or if the windows version
  // is less than Windows Vista (major version = 6). 
  // If so -> local execution.
  LocalDir := GetExecutionDir();
  ConfigIni := LocalDir.Append('config.ini');
  UseLocalDirs := (ConfigIni.IsFile and ConfigIni.Exists and (Win32MajorVersion < 6));
end;

function TPlatformWindows.GetLogPath: IPath;
begin
  Result := GetGameUserPath;
end;

function TPlatformWindows.GetGameSharedPath: IPath;
begin
  Result := GetExecutionDir();
end;

function TPlatformWindows.GetGameUserPath: IPath;
begin
  if UseLocalDirs then
    Result := GetExecutionDir()
  else
    Result := GetSpecialPath(CSIDL_APPDATA).Append('ultrastardx', pdAppend);
end;

end.
