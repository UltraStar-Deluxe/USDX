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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/base/UPath.pas $
 * $Id: UPath.pas 1624 2009-03-06 23:45:10Z k-m_schindler $
 *}

unit UPath;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  Classes;

var
  // Absolute Paths
  GamePath:         string;
  SoundPath:        string;
  SongPaths:        TStringList;
  LogPath:          string;
  ThemePath:        string;
  SkinsPath:        string;
  ScreenshotsPath:  string;
  CoverPaths:       TStringList;
  LanguagesPath:    string;
  PluginPath:       string;
  VisualsPath:      string;
  FontPath:         string;
  ResourcesPath:    string;
  PlayListPath:     string;

function FindPath(out PathResult: string; const RequestedPath: string; NeedsWritePermission: boolean): boolean;
procedure InitializePaths;
procedure AddSongPath(const Path: string);

implementation

uses
  StrUtils,
  UPlatform,
  UCommandLine,
  ULog;

procedure AddSpecialPath(var PathList: TStringList; const Path: string);
var
  Index: integer;
  PathAbs, OldPathAbs: string;
begin
  if (PathList = nil) then
    PathList := TStringList.Create;

  if (Path = '') or not ForceDirectories(Path) then
    Exit;

  PathAbs := IncludeTrailingPathDelimiter(ExpandFileName(Path));

  // check if path or a part of the path was already added
  for Index := 0 to PathList.Count-1 do
  begin
    OldPathAbs := IncludeTrailingPathDelimiter(ExpandFileName(PathList[Index]));
    // check if the new directory is a sub-directory of a previously added one.
    // This is also true, if both paths point to the same directories.
    if (AnsiStartsText(OldPathAbs, PathAbs)) then
    begin
      // ignore the new path
      Exit;
    end;

    // check if a previously added directory is a sub-directory of the new one.
    if (AnsiStartsText(PathAbs, OldPathAbs)) then
    begin
      // replace the old with the new one.
      PathList[Index] := PathAbs;
      Exit;
    end;
  end;

  PathList.Add(PathAbs);
end;

procedure AddSongPath(const Path: string);
begin
  AddSpecialPath(SongPaths, Path);
end;

procedure AddCoverPath(const Path: string);
begin
  AddSpecialPath(CoverPaths, Path);
end;

(**
 * Initialize a path variable
 * After setting paths, make sure that paths exist
 *)
function FindPath(out   PathResult:           string; 
                  const RequestedPath:        string; 
		        NeedsWritePermission: boolean)
         : boolean;
begin
  Result := false;

  if (RequestedPath = '') then
    Exit;

  // Make sure the directory exists
  if (not ForceDirectories(RequestedPath)) then
  begin
    PathResult := '';
    Exit;
  end;

  PathResult := IncludeTrailingPathDelimiter(RequestedPath);

  if (NeedsWritePermission) and
     (FileIsReadOnly(RequestedPath)) then
  begin
    Exit;
  end;

  Result := true;
end;

(**
 * Function sets all absolute paths e.g. song path and makes sure the directorys exist
 *)
procedure InitializePaths;
begin
  // Log directory (must be writable)
  if (not FindPath(LogPath, Platform.GetLogPath, true)) then
  begin
    Log.FileOutputEnabled := false;
    Log.LogWarn('Log directory "'+ Platform.GetLogPath +'" not available', 'InitializePaths');
  end;

  FindPath(SoundPath,     Platform.GetGameSharedPath + 'sounds',    false);
  FindPath(ThemePath,     Platform.GetGameSharedPath + 'themes',    false);
  FindPath(SkinsPath,     Platform.GetGameSharedPath + 'themes',    false);
  FindPath(LanguagesPath, Platform.GetGameSharedPath + 'languages', false);
  FindPath(PluginPath,    Platform.GetGameSharedPath + 'plugins',   false);
  FindPath(VisualsPath,   Platform.GetGameSharedPath + 'visuals',   false);
  FindPath(FontPath,      Platform.GetGameSharedPath + 'fonts',     false);
  FindPath(ResourcesPath, Platform.GetGameSharedPath + 'resources', false);

  // Playlists are not shared as we need one directory to write too
  FindPath(PlaylistPath, Platform.GetGameUserPath + 'playlists', true);

  // Screenshot directory (must be writable)
  if (not FindPath(ScreenshotsPath,  Platform.GetGameUserPath + 'screenshots', true)) then
  begin
    Log.LogWarn('Screenshot directory "'+ Platform.GetGameUserPath +'" not available', 'InitializePaths');
  end;

  // Add song paths
  AddSongPath(Params.SongPath);
  AddSongPath(Platform.GetGameSharedPath + 'songs');
  AddSongPath(Platform.GetGameUserPath + 'songs');

  // Add category cover paths
  AddCoverPath(Platform.GetGameSharedPath + 'covers');
  AddCoverPath(Platform.GetGameUserPath + 'covers');
end;

end.
