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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/base/UPlatformMacOSX.pas $
 * $Id: UPlatformMacOSX.pas 3018 2013-12-06 21:48:55Z k-m_schindler $
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
  UPath,
  UConfig;

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
   * According to Apple guidelines handling of resources and folders should follow
   * these lines:
   *
   * Acceptable places for files are folders named UltraStarDeluxe either in
   *   /Library/Application Support/
   * or
   *   ~/Library/Application Support/
   *
   * So
   * GetGameSharedPath could return
   *   /Library/Application Support/UltraStarDeluxe_[USDX_VERSION]/.
   * GetGameUserPath could return
   *   ~/Library/Application Support/UltraStarDeluxe_[USDX_VERSION]/.
   *
   * Right now, only $HOME/Library/Application Support/UltraStarDeluxe_[USDX_VERSION]
   * is used. So every user needs the complete set of files and folders.
   * Future versions may also use shared resources in
   * /Library/Application Support/UltraStarDeluxe_[USDX_VERSION]. However, this is
   * not treated yet in the code outside this unit.
   *
   * USDX checks, whether GetGameUserPath exists. If not, USDX creates it.
   * The existence of needed files is then checked and if a file is missing
   * it is copied to there from within the folder Contents in the Application
   * bundle, which contains the default files. USDX should not delete files or
   * folders in Application Support/UltraStarDeluxe_[USDX_VERSION] automatically or without
   * user confirmation.
   *
   * The log and benchmark files are stored in
   * $HOME/Library/Log/UltraStar Deluxe/
   *
   * Music should go into ~/Music/UltraStar Deluxe/
   *
   * ~/Library/Application Support/UltraStarDeluxe_[USDX_VERSION]/songs is also used.
   * The idea is to remove this at some time.
   *
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
       * $HOME/Library/Application Support/UltraStarDeluxe_[USDX_VERSION].
       *}
      function GetApplicationSupportPath: IPath;

      {**
       * see the description of @link(Init).
       *}
      procedure CreateUserFolders();

      {**
       * GetHomeDir returns the path to $HOME.
       *}
      function GetHomeDir: IPath;

    public
      {**
       * Init simply calls @link(CreateUserFolders), which in turn scans the
       * folder UltraStarDeluxe.app/Contents for all files and
       * folders. $HOME/Library/Application Support/UltraStarDeluxe_[USDX_VERSION]
       * is then checked for their presence and missing ones are copied.
       *}
      procedure Init; override;

      {**
       * GetLogPath returns the path for log messages. Currently it is set to
       * $HOME/Library/Logs/UltraStar Deluxe/.
       *}
      function  GetLogPath:        IPath; override;

      {**
       * GetMusicPath returns the path for music. Currently it is set to
       * $HOME/Music/UltraStar Deluxe/.
       *}
      function  GetMusicPath:      IPath; override;

      {**
       * GetGameSharedPath returns the path for shared resources. Currently it
       * is also set to $HOME/Library/Application Support/UltraStarDeluxe_[USDX_VERSION].
       * However it is not used.
       *}
      function  GetGameSharedPath: IPath; override;

      {**
       * GetGameUserPath returns the path for user resources. Currently it is
       * set to $HOME/Library/Application Support/UltraStarDeluxe_[USDX_VERSION].
       * This is where a user can add themes, ....
       *}
      function  GetGameUserPath:   IPath; override;
  end;

implementation

uses
  SysUtils,
  MacOSAll;

type
  TLogSwitch = (On, Off);
const
  LogSwitch: TLogSwitch = Off;

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
  Iter:       IFileIterator;
  FileInfo:   TFileInfo;
  CurPath:    IPath;
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
  UserPath:         IPath;
  SrcFile, TgtFile: IPath;
  mainBundle:       CFBundleRef;
  resourcesURL:     CFURLRef;
  bundlePath:       AnsiString;
  bundleName:       AnsiString;
  success:          boolean;
  Position:         integer;
const
  PATH_MAX = 500;

begin
  // Get the current folder and save it in OldBaseDir for returning to it, when finished.
  OldBaseDir := FileSystem.GetCurrentDir();
  if LogSwitch = On then
    writeln('Old base directory: ' + OldBaseDir.ToNative);

  // Try to get bundle path - UltraStarDeluxe.app/Contents contains all the default files and folders.
  bundleName := 'UltraStarDeluxe.app';
  mainBundle := CFBundleGetMainBundle();
  resourcesURL := CFBundleCopyResourcesDirectoryURL(mainBundle);
  SetLength(bundlePath, PATH_MAX);
  success := CFURLGetFileSystemRepresentation(resourcesURL, TRUE, PChar(bundlePath), PATH_MAX);
  if resourcesURL <> nil then
    CFRelease(resourcesURL);

  if success then
  begin
    if LogSwitch = On then
      writeln('BundlePath: ', bundlePath);
    Position := pos(bundleName, bundlePath);
    success := Position > 0;  // Only consider it a success if we found the bundle marker

    if success then
    begin
      setlength(bundlePath, Position + Length(bundleName) - 1);
      success := DirectoryExists(bundlePath);  // Verify the path exists

      if success then
      begin
        chdir(bundlePath);
        BaseDir := FileSystem.GetCurrentDir();
        BaseDir := BaseDir.Append('Contents');

        if LogSwitch = On then
          writeln('Running from app bundle');
      end;
    end;
  end;

  // Fallback: Use executable directory
  if not success then
  begin
    writeln('Warning: Not running from app bundle, using executable directory');
    BaseDir := GetExecutionDir();
    if LogSwitch = On then
      writeln('Using base directory: ' + BaseDir.ToNative);
  end;

  FileSystem.SetCurrentDir(BaseDir);

  // Right now, only $HOME/Library/Application Support/UltraStarDeluxe_[USDX_VERSION] is used.
  UserPath := GetGameUserPath();
  if LogSwitch = On then
    writeln('User path: ' + UserPath.ToNative);

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
      if LogSwitch = On then
        writeln('Current path: ' + CurPath.ToNative);
      if CurPath.IsDirectory() then
      begin
        if (not CurPath.Equals('.')) and
	   (not CurPath.Equals('..')) and
	   (not CurPath.Equals('MacOS')) then
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
    if LogSwitch = On then
      writeln('Current path: ' + CurPath.ToNative);
    DirectoryPath    := UserPath.Append(CurPath);
    if LogSwitch = On then
      writeln('Directory path: ' + DirectoryPath.ToNative);
    CreatedDirectory := DirectoryPath.CreateDirectory();
    FileAttrs        := DirectoryPath.GetAttr();
    // Maybe analyse the target of the link with FpReadlink().
    // Let's assume the symlink is pointing to an existing directory.
    if (not CreatedDirectory) and (FileAttrs and faSymLink > 0) then
      writeln('Failed to create the folder "' +
              DirectoryPath.ToNative +
              '" in PlatformMacOSX.CreateUserFolders');
  end;

  // copy missing files
  for I := 0 to Filelist.Count-1 do
  begin
    CurPath := Filelist[I] as IPath;
    if LogSwitch = On then
      writeln('Current path: ' + CurPath.ToNative);
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
  if LogSwitch = On then
    writeln('Bundle path: ' + Result.ToNative);
end;

function TPlatformMacOSX.GetHomeDir: IPath;
begin
  Result := Path(GetEnvironmentVariable('HOME'));
  if LogSwitch = On then
    writeln('Home path: ' + Result.ToNative);
end;

function TPlatformMacOSX.GetApplicationSupportPath: IPath;
begin
// append the version for conflict resolution
  Result := GetHomeDir.Append('Library/Application Support/UltraStarDeluxe' + USDX_VERSION, pdAppend);
end;

function TPlatformMacOSX.GetLogPath: IPath;
begin
  Result := GetHomeDir.Append('Library/Logs/UltraStar Deluxe', pdAppend);
end;

function TPlatformMacOSX.GetMusicPath: IPath;
begin
  Result := GetHomeDir.Append('Music/UltraStar Deluxe', pdAppend);
  if LogSwitch = On then
    writeln('Music path: ' + Result.ToNative);
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
