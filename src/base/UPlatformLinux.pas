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
  UConfig;

type
  TPlatformLinux = class(TPlatform)
    private
      UseLocalDirs: boolean;

      procedure DetectLocalExecution();
      function GetHomeDir(): string;
    public
      procedure Init; override;

      function DirectoryFindFiles(Dir, Filter: WideString; ReturnAllSubDirs: Boolean): TDirectoryEntryArray; override;

      function GetLogPath        : WideString; override;
      function GetGameSharedPath : WideString; override;
      function GetGameUserPath   : WideString; override;
  end;

implementation

uses
  UCommandLine,
  BaseUnix,
  {$IF FPC_VERSION_INT >= 2002002}
  pwd,
  {$IFEND}
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
  LocalDir: string;
begin
  LocalDir := GetExecutionDir();
  
  // we just check if the 'languages' folder exists in the
  // directory of the executable. If so -> local execution.
  UseLocalDirs := (DirectoryExists(LocalDir + 'languages'));
end;

function TPlatformLinux.DirectoryFindFiles(Dir, Filter: WideString; ReturnAllSubDirs: Boolean): TDirectoryEntryArray;
var
  i: Integer;
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

function TPlatformLinux.GetLogPath: WideString;
begin
  if UseLocalDirs then
    Result := GetExecutionDir()
  else
    Result := GetGameUserPath() + 'logs/';

  // create non-existing directories
  ForceDirectories(Result);
end;

function TPlatformLinux.GetGameSharedPath: WideString;
begin
  if UseLocalDirs then
    Result := GetExecutionDir()
  else
    Result := IncludeTrailingPathDelimiter(INSTALL_DATADIR);
end;

function TPlatformLinux.GetGameUserPath: WideString;
begin
  if UseLocalDirs then
    Result := GetExecutionDir()
  else
    Result := GetHomeDir() + '.ultrastardx/';
end;

{**
 * Returns the user's home directory terminated by a path delimiter
 *}
function TPlatformLinux.GetHomeDir(): string;
{$IF FPC_VERSION_INT >= 2002002}
var
  PasswdEntry: PPasswd;
{$IFEND}
begin
  Result := '';

  {$IF FPC_VERSION_INT >= 2002002}
  // try to retrieve the info from passwd
  PasswdEntry := FpGetpwuid(FpGetuid());
  if (PasswdEntry <> nil) then
    Result := PasswdEntry.pw_dir;
  {$IFEND}
  // fallback if passwd does not contain the path
  if (Result = '') then
    Result := GetEnvironmentVariable('HOME');
  // add trailing path delimiter (normally '/')
  if (Result <> '') then
    Result := IncludeTrailingPathDelimiter(Result);

  {$IF FPC_VERSION_INT >= 2002002}
  // GetUserDir() is another function that returns a user path.
  // It uses env-var HOME or a fallback to a temp-dir.
  //Result := GetUserDir();
  {$IFEND}
end;

end.
