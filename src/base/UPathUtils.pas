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
  SoundPath:        IPath;
  SongPaths:        IInterfaceList;
  LogPath:          IPath;
  ThemePaths:       IInterfaceList;
  ScreenshotsPath:  IPath;
  CoverPaths:       IInterfaceList;
  LanguagesPath:    IPath;
  PluginPaths:      IInterfaceList;
  FontPaths:        IInterfaceList;
  ResourcesPath:    IPath;
  PlaylistPath:     IPath;
  WebsitePaths:     IInterfaceList;
  WebScoresPath:    IPath;
  AvatarsPaths:     IInterfaceList;
  {$IFDEF UseProjectM}
  VisualsPaths:     IInterfaceList;
  {$ENDIF}

function FindPath(out PathResult: IPath; const RequestedPath: IPath; NeedsWritePermission: boolean): boolean;
function FindPaths(out PathList: IInterfaceList; const Prefixes: IInterfaceList; Suffix: string): boolean;

procedure InitializePaths;
procedure AddSongPath(const Path: IPath; CreateMissing: boolean = true);

implementation

uses
  StrUtils,
  UPlatform,
  UCommandLine,
  ULog;

procedure AddSpecialPath(var PathList: IInterfaceList; const Path: IPath; CreateMissing: boolean);
var
  Index: integer;
  PathAbs, PathTmp: IPath;
  OldPath, OldPathAbs, OldPathTmp: IPath;
begin
  if (PathList = nil) then
    PathList := TInterfaceList.Create;

  if Path.Equals(PATH_NONE) then
    Exit;

  if CreateMissing then
  begin
    if not Path.CreateDirectory(true) then
    begin
      Log.LogWarn('Path "'+ Path.ToNative +'" not available', 'UPathUtils.AddSpecialPath');
      Exit;
    end;
  end
  else if not Path.IsDirectory() then
  begin
    Log.LogWarn('Path "'+ Path.ToNative +'" not available', 'UPathUtils.AddSpecialPath');
    Exit;
  end;

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
      Log.LogInfo('Path "'+ PathAbs.ToNative +'" is already added, ignoring duplicate', 'UPathUtils.AddSpecialPath');
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
  Log.LogInfo('Path "'+ PathAbs.ToNative +'" added', 'UPathUtils.AddSpecialPath');
end;

procedure AddSongPath(const Path: IPath; CreateMissing: boolean);
begin
  AddSpecialPath(SongPaths, Path, CreateMissing);
end;

procedure AddCoverPath(const Path: IPath);
begin
  AddSpecialPath(CoverPaths, Path, true);
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

  (*if (RequestedPath.Equals(PATH_NONE)) then
    Exit;*)

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

function FindPaths(out PathList: IInterfaceList; const Prefixes: IInterfaceList; Suffix: string): boolean;
var
  I: integer;
begin
  for I := 0 to Prefixes.Count - 1 do
    AddSpecialPath(PathList, IPath(Prefixes[I]).Append(Suffix), true);
end;

(**
 * Function sets all absolute paths e.g. song path and makes sure the directorys exist
 *)
procedure InitializePaths;
var
  SharedPath, UserPath: IPath;
  ModifiableAssetPaths: IInterfaceList;
begin
  // Log directory (must be writable)
  if (not FindPath(LogPath, Platform.GetLogPath, true)) then
  begin
    Log.FileOutputEnabled := false;
    Log.LogWarn('Log directory "'+ Platform.GetLogPath.ToNative +'" not available', 'UPathUtils.InitializePaths');
  end;

  SharedPath := Platform.GetGameSharedPath;
  UserPath := Platform.GetGameUserPath;
  ModifiableAssetPaths := Platform.GetModifiableAssetPaths;

  FindPath(SoundPath,     SharedPath.Append('sounds'),    false);
  FindPaths(ThemePaths, ModifiableAssetPaths, 'themes');
  FindPath(LanguagesPath, SharedPath.Append('languages'), false);
  FindPaths(PluginPaths, ModifiableAssetPaths, 'plugins');
  FindPaths(FontPaths, ModifiableAssetPaths, 'fonts');
  FindPath(ResourcesPath, SharedPath.Append('resources'), false);
  FindPaths(WebsitePaths, Platform.GetWebsitePaths, 'webs');
  FindPaths(AvatarsPaths, ModifiableAssetPaths, 'avatars');
  {$IFDEF UseProjectM}
  FindPaths(VisualsPaths, ModifiableAssetPaths, Path('visuals').Append('projectM').ToUTF8());
  {$ENDIF}

  // Playlists are not shared as we need one directory to write too
  FindPath(PlaylistPath, UserPath.Append('playlists'), true);

  // Screenshot directory (must be writable)
  if (not FindPath(ScreenshotsPath, UserPath.Append('screenshots'), true)) then
  begin
    Log.LogWarn('Screenshot directory "'+ UserPath.ToNative +'" not available', 'UPathUtils.InitializePaths');
  end;

  // Add song paths
  AddSongPath(Params.SongPath);
{$IF Defined(DARWIN)}
  AddSongPath(Platform.GetMusicPath);
  AddSongPath(UserPath.Append('songs'));
{$ELSE}
  AddSongPath(SharedPath.Append('songs'));
  AddSongPath(UserPath.Append('songs'));
{$IFEND}

  // Add category cover paths
  AddCoverPath(SharedPath.Append('covers'));
  AddCoverPath(UserPath.Append('covers'));
end;

end.
