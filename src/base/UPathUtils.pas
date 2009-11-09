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

unit UPathUtils;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  Classes,
  UPath;

var
  // Absolute Paths
  GamePath:         IPath;
  SoundPath:        IPath;
  SongPaths:        IInterfaceList;
  LogPath:          IPath;
  ThemePath:        IPath;
  SkinsPath:        IPath;
  ScreenshotsPath:  IPath;
  CoverPaths:       IInterfaceList;
  LanguagesPath:    IPath;
  PluginPath:       IPath;
  VisualsPath:      IPath;
  FontPath:         IPath;
  ResourcesPath:    IPath;
  PlaylistPath:     IPath;

function FindPath(out PathResult: IPath; const RequestedPath: IPath; NeedsWritePermission: boolean): boolean;
procedure InitializePaths;
procedure AddSongPath(const Path: IPath);

implementation

uses
  StrUtils,
  UPlatform,
  UCommandLine,
  ULog;

procedure AddSpecialPath(var PathList: IInterfaceList; const Path: IPath);
var
  Index: integer;
  PathAbs, PathTmp: IPath;
  OldPath, OldPathAbs, OldPathTmp: IPath;
begin
  if (PathList = nil) then
    PathList := TInterfaceList.Create;

  if Path.Equals(PATH_NONE) or not Path.CreateDirectory(true) then
    Exit;

  PathTmp := Path.GetAbsolutePath();
  PathAbs := PathTmp.AppendPathDelim();

  // check if path or a part of the path was already added
  for Index := 0 to PathList.Count-1 do
  begin
    OldPath := PathList[Index] as IPath;
    OldPathTmp := OldPath.GetAbsolutePath();
    OldPathAbs := OldPathTmp.AppendPathDelim();

    // check if the new directory is a sub-directory of a previously added one.
    // This is also true, if both paths point to the same directories.
    if (OldPathAbs.IsChildOf(PathAbs, false) or OldPathAbs.Equals(PathAbs)) then
    begin
      // ignore the new path
      Exit;
    end;

    // check if a previously added directory is a sub-directory of the new one.
    if (PathAbs.IsChildOf(OldPathAbs, false)) then
    begin
      // replace the old with the new one.
      PathList[Index] := PathAbs;
      Exit;
    end;
  end;

  PathList.Add(PathAbs);
end;

procedure AddSongPath(const Path: IPath);
begin
  AddSpecialPath(SongPaths, Path);
end;

procedure AddCoverPath(const Path: IPath);
begin
  AddSpecialPath(CoverPaths, Path);
end;

(**
 * Initialize a path variable
 * After setting paths, make sure that paths exist
 *)
function FindPath(
  out PathResult: IPath;
  const RequestedPath: IPath;
  NeedsWritePermission: boolean): boolean;
begin
  Result := false;

  if (RequestedPath.Equals(PATH_NONE)) then
    Exit;

  // Make sure the directory exists
  if (not RequestedPath.CreateDirectory(true)) then
  begin
    PathResult := PATH_NONE;
    Exit;
  end;

  PathResult := RequestedPath.AppendPathDelim();

  if (NeedsWritePermission) and RequestedPath.IsReadOnly() then
    Exit;

  Result := true;
end;

(**
 * Function sets all absolute paths e.g. song path and makes sure the directorys exist
 *)
procedure InitializePaths;
var
  SharedPath, UserPath: IPath;
begin
  // Log directory (must be writable)
  if (not FindPath(LogPath, Platform.GetLogPath, true)) then
  begin
    Log.FileOutputEnabled := false;
    Log.LogWarn('Log directory "'+ Platform.GetLogPath.ToNative +'" not available', 'InitializePaths');
  end;

  SharedPath := Platform.GetGameSharedPath;
  UserPath := Platform.GetGameUserPath;

  FindPath(SoundPath,     SharedPath.Append('sounds'),    false);
  FindPath(ThemePath,     SharedPath.Append('themes'),    false);
  FindPath(SkinsPath,     SharedPath.Append('themes'),    false);
  FindPath(LanguagesPath, SharedPath.Append('languages'), false);
  FindPath(PluginPath,    SharedPath.Append('plugins'),   false);
  FindPath(VisualsPath,   SharedPath.Append('visuals'),   false);
  FindPath(FontPath,      SharedPath.Append('fonts'),     false);
  FindPath(ResourcesPath, SharedPath.Append('resources'), false);

  // Playlists are not shared as we need one directory to write too
  FindPath(PlaylistPath, UserPath.Append('playlists'), true);

  // Screenshot directory (must be writable)
  if (not FindPath(ScreenshotsPath, UserPath.Append('screenshots'), true)) then
  begin
    Log.LogWarn('Screenshot directory "'+ UserPath.ToNative +'" not available', 'InitializePaths');
  end;

  // Add song paths
  AddSongPath(Params.SongPath);
  AddSongPath(SharedPath.Append('songs'));
  AddSongPath(UserPath.Append('songs'));

  // Add category cover paths
  AddCoverPath(SharedPath.Append('covers'));
  AddCoverPath(UserPath.Append('covers'));
end;

end.
