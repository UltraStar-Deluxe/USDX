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
  UPlatform,
  UFilesystem,
  UPath;

type
  {**
   * @abstract(Provides Mac OS X specific details.)
   * @lastmod(August 1, 2008)
   * The UPlatformMacOSX unit takes care of setting paths to resource folders.
   *
   * (Note for non-Maccies: "folder" is the Mac name for directory.)
   *
   * Note on the resource folders:
   *  1. Installation of an application on the mac works as follows: Extract and
   *     copy an application and if you don't like or need the application
   *     anymore you move the folder to the trash - and you're done.
   *  2. The use of folders in the user's home directory is against Apple's
   *     guidelines and strange to an average user.
   *  3. Even worse is using /usr/local/... since all lowercase folders in / are
   *     not visible to an average user in the Finder, at least not without some
   *     "tricks".
   *
   * The best way would be to store everything within the application bundle.
   * However, this requires USDX to offer the handling of the resources. Until
   * this is implemented, the second best solution is as follows:
   *
   * According to Aple guidelines handling of resources and folders should follow
   * these lines:
   *
   * Acceptable places for files are folders named UltraStarDeluxe either in
   *   /Library/Application Support/
   * or
   *   ~/Library/Application Support/
   *
   * So
   * GetGameSharedPath could return
   *   /Library/Application Support/UltraStarDeluxe/.
   * GetGameUserPath could return
   *   ~/Library/Application Support/UltraStarDeluxe/.
   *
   * Right now, only $HOME/Library/Application Support/UltraStarDeluxe
   * is used. So every user needs the complete set of files and folders.
   * Future versions may also use shared resources in
   * /Library/Application Support/UltraStarDeluxe. However, this is
   * not treated yet in the code outside this unit.
   *
   * USDX checks, whether GetGameUserPath exists. If not, USDX creates it.
   * The existence of needed files is then checked and if a file is missing
   * it is copied to there from within the folder Contents in the Application
   * bundle, which contains the default files. USDX should not delete files or
   * folders in Application Support/UltraStarDeluxe automatically or without
   * user confirmation.
   *}
  TPlatformMacOSX = class(TPlatform)
    private
      {**
       * GetBundlePath returns the path to the application bundle
       * UltraStarDeluxe.app.
       *}
      function GetBundlePath: IPath;

      {**
       * GetApplicationSupportPath returns the path to
       * $HOME/Library/Application Support/UltraStarDeluxe.
       *}
      function GetApplicationSupportPath: IPath;

      {**
       * see the description of @link(Init).
       *}
      procedure CreateUserFolders();

      function GetHomeDir(): IPath;

    public
      {**
       * Init simply calls @link(CreateUserFolders), which in turn scans the
       * folder UltraStarDeluxe.app/Contents for all files and
       * folders. $HOME/Library/Application Support/UltraStarDeluxe
       * is then checked for their presence and missing ones are copied.
       *}
      procedure Init; override;

      {**
       * GetLogPath returns the path for log messages. Currently it is set to
       * $HOME/Library/Application Support/UltraStarDeluxe/Log.
       *}
      function  GetLogPath        : IPath; override;

      {**
       * GetGameSharedPath returns the path for shared resources. Currently it
       * is set to /Library/Application Support/UltraStarDeluxe.
       * However it is not used.
       *}
      function  GetGameSharedPath : IPath; override;

      {**
       * GetGameUserPath returns the path for user resources. Currently it is
       * set to $HOME/Library/Application Support/UltraStarDeluxe.
       * This is where a user can add songs, themes, ....
       *}
      function  GetGameUserPath   : IPath; override;
  end;

implementation

uses
  SysUtils;

procedure TPlatformMacOSX.Init;
begin
  CreateUserFolders();
end;

procedure TPlatformMacOSX.CreateUserFolders();
var
  RelativePath: IPath;
  // BaseDir contains the path to the folder, where a search is performed.
  // It is set to the entries in @link(DirectoryList) one after the other.
  BaseDir: IPath;
  // OldBaseDir contains the path to the folder, where the search started.
  // It is used to return to it, when the search is completed in all folders.
  OldBaseDir: IPath;
  Iter: IFileIterator;
  FileInfo: TFileInfo;
  CurPath: IPath;
  // These two lists contain all folder and file names found
  // within the folder @link(BaseDir).
  DirectoryList, FileList: IInterfaceList;
  // DirectoryIsFinished contains the index of the folder in @link(DirectoryList),
  // which is the last one completely searched. Later folders are still to be
  // searched for additional files and folders.
  DirectoryIsFinished: longint;
  I: longint;
  // These three are for creating directories, due to possible symlinks
  CreatedDirectory: boolean;
  FileAttrs:        integer;
  DirectoryPath:    IPath;
  UserPath: IPath;
  SrcFile, TgtFile: IPath;
begin
  // Get the current folder and save it in OldBaseDir for returning to it, when
  // finished.
  OldBaseDir := FileSystem.GetCurrentDir();

  // UltraStarDeluxe.app/Contents contains all the default files and folders.
  BaseDir := OldBaseDir.Append('UltraStarDeluxe.app/Contents');
  FileSystem.SetCurrentDir(BaseDir);

  // Right now, only $HOME/Library/Application Support/UltraStarDeluxe is used.
  UserPath := GetGameUserPath();

  DirectoryIsFinished := 0;
  // replace with IInterfaceList
  DirectoryList := TInterfaceList.Create();
  FileList := TInterfaceList.Create();
  DirectoryList.Add(Path('.'));

  // create the folder and file lists
  repeat
    RelativePath := (DirectoryList[DirectoryIsFinished] as IPath);
    FileSystem.SetCurrentDir(BaseDir.Append(RelativePath));
    Iter := FileSystem.FileFind(Path('*'), faAnyFile);
    while (Iter.HasNext) do    
    begin
      FileInfo := Iter.Next;
      CurPath := FileInfo.Name;
      if CurPath.IsDirectory() then
      begin
        if (not CurPath.Equals('.')) and (not CurPath.Equals('..')) then
          DirectoryList.Add(RelativePath.Append(CurPath));
      end
      else
        Filelist.Add(RelativePath.Append(CurPath));
    end;
    Inc(DirectoryIsFinished);
  until (DirectoryIsFinished = DirectoryList.Count);

  // create missing folders
  UserPath.CreateDirectory(true); // should not be necessary since (UserPathName+'/.') is created.
  for I := 0 to DirectoryList.Count-1 do
  begin
    CurPath          := DirectoryList[I] as IPath;
    DirectoryPath    := UserPath.Append(CurPath);
    CreatedDirectory := DirectoryPath.CreateDirectory();
    FileAttrs        := DirectoryPath.GetAttr();
    // Maybe analyse the target of the link with FpReadlink().
    // Let's assume the symlink is pointing to an existing directory.
    if (not CreatedDirectory) and (FileAttrs and faSymLink > 0) then
      Log.LogError('Failed to create the folder "'+ DirectoryPath.ToNative +'"',
                   'TPlatformMacOSX.CreateUserFolders');
  end;

  // copy missing files
  for I := 0 to Filelist.Count-1 do
  begin
    CurPath := Filelist[I] as IPath;
    SrcFile := BaseDir.Append(CurPath);
    TgtFile := UserPath.Append(CurPath);
    SrcFile.CopyFile(TgtFile, true);
  end;

  // go back to the initial folder
  FileSystem.SetCurrentDir(OldBaseDir);
end;

function TPlatformMacOSX.GetBundlePath: IPath;
begin
  // Mac applications are packaged in folders.
  // Cutting the last two folders yields the application folder.
  Result := GetExecutionDir().GetParent().GetParent();
end;

function TPlatformMacOSX.GetApplicationSupportPath: IPath;
const
  PathName: string = 'Library/Application Support/UltraStarDeluxe';
begin
  Result := GetHomeDir().Append(PathName, pdAppend);
end;

function TPlatformMacOSX.GetHomeDir(): IPath;
begin
  Result := Path(GetEnvironmentVariable('HOME'));
end;

function TPlatformMacOSX.GetLogPath: IPath;
begin
  Result := GetApplicationSupportPath.Append('Logs');
end;

function TPlatformMacOSX.GetGameSharedPath: IPath;
begin
  Result := GetApplicationSupportPath;
end;

function TPlatformMacOSX.GetGameUserPath: IPath;
begin
  Result := GetApplicationSupportPath;
end;

end.
