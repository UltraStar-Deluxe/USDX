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

unit UPlatformMacOSX;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
  ULog,
  UPlatform;

type
  {**
   * @abstract(Provides Mac OS X specific details.)
   * @lastmod(August 1, 2008)
   * The UPlatformMacOSX unit takes care of setting paths to resource folders.
   *
   * (Note for non-Maccies: "folder" is the Mac name for directory.)
   *
   * Note on the resource folders:
   *  1. Installation of an application on the mac works as follows: Extract and copy an application
   *     and if you don't like or need the application anymore you move the folder
   *     to the trash - and you're done.
   *  2. The use folders in the user's home directory is against Apple's guidelines
   *     and strange to an average user.
   *  3. Even worse is using /usr/local/... since all lowercase folders in / are
   *     not visible to an average user in the Finder, at least not without some "tricks".
   *
   * The best way would be to store everything within the application bundle. However, this
   * requires USDX to offer the handling of the resources. Until this is implemented, the
   * second best solution is as follows:
   *
   * According to Aple guidelines handling of resources and folders should follow these lines:
   *
   * Acceptable places for files are folders named UltraStarDeluxe either in
   *   /Library/Application Support/
   * or
   *   ~/Library/Application Support/
   *
   * So
   * GetGameSharedPath could return
   *   /Library/Application Support/UltraStarDeluxe/Resources/.
   * GetGameUserPath could return
   *   ~/Library/Application Support/UltraStarDeluxe/Resources/.
   *
   * Right now, only $HOME/Library/Application Support/UltraStarDeluxe/Resources
   * is used. So every user needs the complete set of files and folders.
   * Future versions may also use shared resources in
   * /Library/Application Support/UltraStarDeluxe/Resources. However, this is not
   * treated yet in the code outside this unit.
   *
   * USDX checks, whether GetGameUserPath exists. If not, USDX creates it.
   * The existence of needed files is then checked and if a file is missing
   * it is copied to there from within the Resources folder in the Application
   * bundle, which contains the default files. USDX should not delete files or
   * folders in Application Support/UltraStarDeluxe automatically or without
   * user confirmation.
   *}
  TPlatformMacOSX = class(TPlatform)
    private
      {**
       * GetBundlePath returns the path to the application bundle UltraStarDeluxe.app.
       *}
      function GetBundlePath: WideString;

      {**
       * GetApplicationSupportPath returns the path to
       * $HOME/Library/Application Support/UltraStarDeluxe/Resources.
       *}
      function GetApplicationSupportPath: WideString;

      {**
       * see the description of @link(Init).
       *}
      procedure CreateUserFolders();
      
    public
      {**
       * Init simply calls @link(CreateUserFolders), which in turn scans the folder
       * UltraStarDeluxe.app/Contents/Resources for all files and folders.
       * $HOME/Library/Application Support/UltraStarDeluxe/Resources is then checked
       * for their presence and missing ones are copied.
       *}
      procedure Init; override;

      {**
       * DirectoryFindFiles returns all entries of a folder with names and booleans
       * about their type, i.e. file or directory.
       *}
      function  DirectoryFindFiles(Dir, Filter: WideString; ReturnAllSubDirs: boolean): TDirectoryEntryArray; override;

      {**
       * GetLogPath returns the path for log messages. Currently it is set to
       * $HOME/Library/Application Support/UltraStarDeluxe/Resources/Log.
       *}
      function  GetLogPath        : WideString; override;

      {**
       * GetGameSharedPath returns the path for shared resources. Currently it is set to
       * /Library/Application Support/UltraStarDeluxe/Resources.
       * However it is not used.
       *}
      function  GetGameSharedPath : WideString; override;

      {**
       * GetGameUserPath returns the path for user resources. Currently it is set to
       * $HOME/Library/Application Support/UltraStarDeluxe/Resources.
       * This is where a user can add songs, themes, ....
       *}
      function  GetGameUserPath   : WideString; override;
  end;

implementation

uses
  SysUtils,
  BaseUnix;

procedure TPlatformMacOSX.Init;
begin
  CreateUserFolders();
end;

procedure TPlatformMacOSX.CreateUserFolders();
var
  RelativePath: string;
  // BaseDir contains the path to the folder, where a search is performed.
  // It is set to the entries in @link(DirectoryList) one after the other.
  BaseDir: string;
  // OldBaseDir contains the path to the folder, where the search started.
  // It is used to return to it, when the search is completed in all folders.
  OldBaseDir: string;
  // This record contains the result of a file search with FindFirst or FindNext
  SearchInfo: TSearchRec;
  // These two lists contain all folder and file names found
  // within the folder @link(BaseDir).
  DirectoryList, FileList: TStringList;
  // DirectoryIsFinished contains the index of the folder in @link(DirectoryList),
  // which is the last one completely searched. Later folders are still to be
  // searched for additional files and folders.
  DirectoryIsFinished: longint;
  Counter: longint;

  UserPathName: string;
const
  // used to construct the @link(UserPathName)
  PathName: string = '/Library/Application Support/UltraStarDeluxe/Resources';
begin
  // Get the current folder and save it in OldBaseDir for returning to it, when
  // finished.
  GetDir(0, OldBaseDir);

  // UltraStarDeluxe.app/Contents/Resources contains all the default files and
  // folders.
  BaseDir := OldBaseDir + '/UltraStarDeluxe.app/Contents/Resources';
  ChDir(BaseDir);

  // Right now, only $HOME/Library/Application Support/UltraStarDeluxe/Resources
  // is used.
  UserPathName := GetEnvironmentVariable('HOME') + PathName;

  DirectoryIsFinished := 0;
  DirectoryList := TStringList.Create();
  FileList := TStringList.Create();
  DirectoryList.Add('.');
  
  // create the folder and file lists
  repeat

    RelativePath := DirectoryList[DirectoryIsFinished];
    ChDir(BaseDir + '/' + RelativePath);
    if (FindFirst('*', faAnyFile, SearchInfo) = 0) then
    begin
      repeat
        if DirectoryExists(SearchInfo.Name) then
        begin
          if (SearchInfo.Name <> '.') and (SearchInfo.Name <> '..')  then
            DirectoryList.Add(RelativePath + '/' + SearchInfo.Name);
        end
        else
          Filelist.Add(RelativePath + '/' + SearchInfo.Name);
      until (FindNext(SearchInfo) <> 0);
    end;
    FindClose(SearchInfo);
    Inc(DirectoryIsFinished);
  until (DirectoryIsFinished = DirectoryList.Count);

  // create missing folders
  ForceDirectories(UserPathName);        // should not be necessary since (UserPathName+'/.') is created.
  for Counter := 0 to DirectoryList.Count-1 do
  begin
    if not ForceDirectories(UserPathName + '/' + DirectoryList[Counter]) then
      Log.LogError('Failed to create the folder "'+ UserPathName + '/' + DirectoryList[Counter] +'"',
                   'TPlatformMacOSX.CreateUserFolders');
  end;
  DirectoryList.Free();

  // copy missing files
  for Counter := 0 to Filelist.Count-1 do
  begin
    CopyFile(BaseDir      + '/' + Filelist[Counter],
             UserPathName + '/' + Filelist[Counter], true);
  end;
  FileList.Free();

  // go back to the initial folder
  ChDir(OldBaseDir);
end;

function TPlatformMacOSX.GetBundlePath: WideString;
var
  i, pos : integer;
begin
  // Mac applications are packaged in folders.
  // We have to cut the last two folders
  // to get the application folder.

  Result := GetExecutionDir();
  for i := 1 to 2 do
  begin
    pos := Length(Result);
    repeat
      Delete(Result, pos, 1);
      pos := Length(Result);
    until (pos = 0) or (Result[pos] = '/');
  end;
end;

function TPlatformMacOSX.GetApplicationSupportPath: WideString;
const
  PathName : string = '/Library/Application Support/UltraStarDeluxe/Resources';
begin
  Result := GetEnvironmentVariable('HOME') + PathName + '/';
end;

function TPlatformMacOSX.GetLogPath: WideString;
begin
  Result := GetApplicationSupportPath + 'Logs';
end;

function TPlatformMacOSX.GetGameSharedPath: WideString;
begin
  Result := GetApplicationSupportPath;
end;

function TPlatformMacOSX.GetGameUserPath: WideString;
begin
  Result := GetApplicationSupportPath;
end;

function TPlatformMacOSX.DirectoryFindFiles(Dir, Filter: WideString; ReturnAllSubDirs: boolean): TDirectoryEntryArray;
var
  i       : integer;
  TheDir  : pdir;
  ADirent : pDirent;
  lAttrib : integer;
begin
  i := 0;
  Filter := LowerCase(Filter);

  TheDir := FPOpenDir(Dir);     
  if Assigned(TheDir) then
    repeat
      ADirent := FPReadDir(TheDir);

      if Assigned(ADirent) and (ADirent^.d_name <> '.') and (ADirent^.d_name <> '..') then
      begin
        lAttrib := FileGetAttr(Dir + ADirent^.d_name);
        if ReturnAllSubDirs and ((lAttrib and faDirectory) <> 0) then
        begin
          SetLength(Result, i + 1);
          Result[i].Name        := ADirent^.d_name;
          Result[i].IsDirectory := true;
          Result[i].IsFile      := false;
          i := i + 1;
        end
        else if (Length(Filter) = 0) or (Pos( Filter, LowerCase(ADirent^.d_name)) > 0) then
        begin
          SetLength(Result, i + 1);
          Result[i].Name        := ADirent^.d_name;
          Result[i].IsDirectory := false;
          Result[i].IsFile      := true;
          i := i + 1;
        end;
      end;
    until ADirent = nil;

  FPCloseDir(TheDir);
end;

end.
