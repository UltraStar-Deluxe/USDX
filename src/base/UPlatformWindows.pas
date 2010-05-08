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
 * Detects whether the was executed locally or globally.
 * - Local mode:
 *   - Condition:
 *     - config.ini is writable or creatable in the directory of the executable.
 *   - Examples:
 *     - The USDX zip-archive has been unpacked to a directory with write.
 *       permissions
 *     - XP: USDX was installed to %ProgramFiles% and the user is an admin.
 *     - USDX is started from an external HD- or flash-drive
 *   - Behavior:
 *     Config files like config.ini or score db reside in the directory of the
 *     executable. This is useful to enable windows users to have a portable
 *     installation e.g. on an external hdd.
 *     This is also the default behaviour of usdx prior to version 1.1
 * - Global mode:
 *   - Condition:
 *     - config.ini is not writable.
 *   - Examples:
 *     - Vista/7: USDX was installed to %ProgramFiles%.
 *     - XP: USDX was installed to %ProgramFiles% and the user is not an admin.
 *     - USDX is started from CD
 *   - Behavior:
 *     - The config files are in a separate folder (e.g. %APPDATA%\ultrastardx)
 *
 * On windows, resources (themes, language-files)
 * reside in the directory of the executable in any case
 *
 * Sets UseLocalDirs to true if the game is executed locally, false otherwise.
 *}
procedure TPlatformWindows.DetectLocalExecution();
var
  LocalDir, ConfigIni: IPath;
  Handle: TFileHandle;
begin
  LocalDir := GetExecutionDir();
  ConfigIni := LocalDir.Append('config.ini');

  // check if config.ini is writable or creatable, if so use local dirs
  UseLocalDirs := false;
  if (ConfigIni.Exists()) then
  begin
    // do not use a read-only config file
    if (not ConfigIni.IsReadOnly()) then
    begin
      // Just open the file in read-write mode to be sure that we have access
      // rights for it.
      // Note: Do not use IsReadOnly() as it does not check file privileges, so
      // a non-read-only file might not be writable for us.
      Handle := ConfigIni.Open(fmOpenReadWrite);
      if (Handle <> -1) then
      begin
        FileClose(Handle);
        UseLocalDirs := true;
      end;
    end;
  end
  else // config.ini does not exist
  begin
    // try to create config.ini
    Handle := ConfigIni.CreateFile();
    if (Handle <> -1) then
    begin
      FileClose(Handle);
      UseLocalDirs := true;
    end;
  end;
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
