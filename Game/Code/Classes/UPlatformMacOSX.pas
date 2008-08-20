unit UPlatformMacOSX;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
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
  counter: longint;

  UserPathName: string;
  // SourceFile and TaretFile are used to copy a file from on folder to another.
  SourceFile, TargetFile: TFileStream;
  // FileCopyBuffer is the buffer for copying @link(SourceFile) to @link(TargetFile).
  // Its size (4096) has been choosen with little testing. A smaller size makes
  // copying slower. A larger size may make copying large files faster.
  FileCopyBuffer: array [1..4096] of byte;
  // number of bytes read from @link(SourceFile)
  NumberOfBytes: integer;
const
  // used to construct the @link(UserPathName)
  PathName: string = '/Library/Application Support/UltraStarDeluxe/Resources';
begin
  // Get the current folder and save it in OldBaseDir for returning to it, when
  // finished.
  getdir (0, OldBaseDir);

  // UltraStarDeluxe.app/Contents/Resources contains all the default files and
  // folders.
  BaseDir := OldBaseDir + '/UltraStarDeluxe.app/Contents/Resources';
  chdir (BaseDir);

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
    chdir (BaseDir + '/' + RelativePath);
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
    DirectoryIsFinished := succ(DirectoryIsFinished);
  until (DirectoryIsFinished = DirectoryList.Count);

  // create missing folders
  if not DirectoryExists(UserPathName) then
    mkdir (UserPathName);

  for counter := 0 to DirectoryList.Count-1 do
    if not DirectoryExists(UserPathName + '/' + DirectoryList[counter]) then
      mkdir (UserPathName + '/' + DirectoryList[counter]);
  DirectoryList.Free();

  // copy missing files
  for counter := 0 to Filelist.Count-1 do
    if not FileExists(UserPathName + '/' + Filelist[counter]) then
    begin
      SourceFile := TFileStream.Create(BaseDir      + '/' + Filelist[counter], fmOpenRead);
      TargetFile := TFileStream.Create(UserPathName + '/' + Filelist[counter], fmCreate);
      repeat
        NumberOfBytes := SourceFile.Read(FileCopyBuffer, SizeOf(FileCopyBuffer));
        TargetFile.Write(FileCopyBuffer, NumberOfBytes);
      until (NumberOfBytes < SizeOf(FileCopyBuffer));
      SourceFile.Free;
      TargetFile.Free;
    end;
  FileList.Free();

  // go back to the initial folder
  chdir (OldBaseDir);
end;

function TPlatformMacOSX.GetBundlePath: WideString;
var
  i, pos : integer;
begin
  // Mac applications are packaged in folders.
  // We have to cut the last two folders
  // to get the application folder.

  Result := ExtractFilePath(ParamStr(0));
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
