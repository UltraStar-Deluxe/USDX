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

unit UPlatformLinux;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
  UPlatform,
  UConfig,
  UPath;

type
  TPlatformLinux = class(TPlatform)
    private
      UseLocalDirs: boolean;

      procedure DetectLocalExecution();
      function GetHomeDir(): IPath;
    public
      procedure Init; override;
      
      function GetLogPath        : IPath; override;
      function GetGameSharedPath : IPath; override;
      function GetGameUserPath   : IPath; override;
  end;

implementation

uses
  UCommandLine,
  BaseUnix,
  pwd,
  SysUtils,
  ULog;

const
  {$I paths.inc}

procedure TPlatformLinux.Init;
begin
  inherited Init();
  DetectLocalExecution();
end;

{**
 * Detects whether the game was executed locally or globally.
 * - It is local if it was not installed and directly executed from
 *   within the game folder. In this case resources (themes, language-files)
 *   reside in the directory of the executable.
 * - It is global if the game was installed (e.g. to /usr/bin) and
 *   the resources are in a separate folder (e.g. /usr/share/ultrastardx)
 *   which name is stored in the INSTALL_DATADIR constant in paths.inc.
 *
 * Sets UseLocalDirs to true if the game is executed locally, false otherwise.
 *}
procedure TPlatformLinux.DetectLocalExecution();
var
  LocalDir, LanguageDir: IPath;
begin
  // we just check if the 'languages' folder exists in the
  // directory of the executable. If so -> local execution.
  LocalDir := GetExecutionDir();
  LanguageDir := LocalDir.Append('languages');
  UseLocalDirs := LanguageDir.IsDirectory;
end;

function TPlatformLinux.GetLogPath: IPath;
begin
  if UseLocalDirs then
    Result := GetExecutionDir()
  else
    Result := GetGameUserPath().Append('logs', pdAppend);

  // create non-existing directories
  Result.CreateDirectory(true);
end;

function TPlatformLinux.GetGameSharedPath: IPath;
begin
  if UseLocalDirs then
    Result := GetExecutionDir()
  else
    Result := Path(INSTALL_DATADIR, pdAppend);
end;

function TPlatformLinux.GetGameUserPath: IPath;
begin
  if UseLocalDirs then
    Result := GetExecutionDir()
  else
    Result := GetHomeDir().Append('.ultrastardx', pdAppend);
end;

{**
 * Returns the user's home directory terminated by a path delimiter
 *}
function TPlatformLinux.GetHomeDir(): IPath;
var
  PasswdEntry: PPasswd;
begin
  Result := PATH_NONE;

  // try to retrieve the info from passwd
  PasswdEntry := FpGetpwuid(FpGetuid());
  if (PasswdEntry <> nil) then
    Result := Path(PasswdEntry.pw_dir);
  // fallback if passwd does not contain the path
  if (Result.IsUnset) then
    Result := Path(GetEnvironmentVariable('HOME'));
  // add trailing path delimiter (normally '/')
  if (Result.IsSet) then
    Result := Result.AppendPathDelim();

  // GetUserDir() is another function that returns a user path.
  // It uses env-var HOME or a fallback to a temp-dir.
  //Result := GetUserDir();
end;

end.
