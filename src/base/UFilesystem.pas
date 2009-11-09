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

unit UFilesystem;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  Classes,
  {$IFDEF MSWINDOWS}
  Windows,
  TntSysUtils,
  {$ENDIF}
  UPath;

type
  {$IFDEF MSWINDOWS}
  TSytemSearchRec = TSearchRecW;
  {$ELSE}
  TSytemSearchRec = TSearchRec;
  {$ENDIF}

  TFileInfo = record
    Time: integer;  // timestamp
    Size: int64;    // file size (byte)
    Attr: integer;  // file attributes
    Name: IPath;    // basename with extension
  end;

  {**
   * Iterates through the search results retrieved by FileFind().
   * Example usage:
   *   while(Iter.HasNext()) do
   *     SearchRec := Iter.Next();
   *}
  IFileIterator = interface
    function HasNext(): boolean;
    function Next(): TFileInfo;
  end;

  {**
   * Wrapper for SysUtils file functions.
   * For documentation and examples, check the SysUtils equivalent.
   *}
  IFileSystem = interface
    function ExpandFileName(const FileName: IPath): IPath;
    function FileCreate(const FileName: IPath): THandle;
    function DirectoryCreate(const Dir: IPath): boolean;
    function FileOpen(const FileName: IPath; Mode: longword): THandle;
    function FileAge(const FileName: IPath): integer; overload;
    function FileAge(const FileName: IPath; out FileDateTime: TDateTime): boolean; overload;

    function DirectoryExists(const Name: IPath): boolean;

    {**
     * On Windows: returns true only for files (not directories)
     * On Apple/Unix: returns true for all kind of files (even directories)
     * @seealso SysUtils.FileExists()
     *}
    function FileExists(const Name: IPath): boolean;

    function FileGetAttr(const FileName: IPath): Cardinal;
    function FileSetAttr(const FileName: IPath; Attr: integer): boolean;
    function FileIsReadOnly(const FileName: IPath): boolean;
    function FileSetReadOnly(const FileName: IPath; ReadOnly: boolean): boolean;
    function FileIsAbsolute(const FileName: IPath): boolean;
    function ForceDirectories(const Dir: IPath): boolean;
    function RenameFile(const OldName, NewName: IPath): boolean;
    function DeleteFile(const FileName: IPath): boolean;
    function RemoveDir(const Dir: IPath): boolean;

    {**
     * Copies file Source to Target. If FailIfExists is true, the file is not
     * copied if it already exists.
     * Returns true if the file was successfully copied.
     *}
    function CopyFile(const Source, Target: IPath; FailIfExists: boolean): boolean;

    function ExtractFileDrive(const FileName: IPath): IPath;
    function ExtractFilePath(const FileName: IPath): IPath;
    function ExtractFileDir(const FileName: IPath): IPath;
    function ExtractFileName(const FileName: IPath): IPath;
    function ExtractFileExt(const FileName: IPath): IPath;
    function ExtractRelativePath(const BaseName: IPath; const FileName: IPath): IPath;

    function ChangeFileExt(const FileName: IPath; const Extension: IPath): IPath;

    function IncludeTrailingPathDelimiter(const FileName: IPath): IPath;
    function ExcludeTrailingPathDelimiter(const FileName: IPath): IPath;

    {**
     * Searches for a file with filename Name in the directories given in DirList.
     *}
    function FileSearch(const Name: IPath; DirList: array of IPath): IPath;

    {**
     * More convenient version of FindFirst/Next/Close with iterator support.
     *}
    function FileFind(const FilePattern: IPath; Attr: integer): IFileIterator;

    {**
     * Old style search functions. Use FileFind() instead.
     *}
    function FindFirst(const FilePattern: IPath; Attr: integer; var F: TSytemSearchRec): integer;
    function FindNext(var F: TSytemSearchRec): integer;
    procedure FindClose(var F: TSytemSearchRec);

    function GetCurrentDir: IPath;
    function SetCurrentDir(const Dir: IPath): boolean;

    {**
     * Returns true if the filesystem is case-sensitive.
     *}
    function IsCaseSensitive(): boolean;
  end;

  function FileSystem(): IFileSystem;

implementation

type
  TFileSystemImpl = class(TInterfacedObject, IFileSystem)
  public
    function ExpandFileName(const FileName: IPath): IPath;
    function FileCreate(const FileName: IPath): THandle;
    function DirectoryCreate(const Dir: IPath): boolean;
    function FileOpen(const FileName: IPath; Mode: longword): THandle;
    function FileAge(const FileName: IPath): integer; overload;
    function FileAge(const FileName: IPath; out FileDateTime: TDateTime): boolean; overload;
    function DirectoryExists(const Name: IPath): boolean;
    function FileExists(const Name: IPath): boolean;
    function FileGetAttr(const FileName: IPath): Cardinal;
    function FileSetAttr(const FileName: IPath; Attr: integer): boolean;
    function FileIsReadOnly(const FileName: IPath): boolean;
    function FileSetReadOnly(const FileName: IPath; ReadOnly: boolean): boolean;
    function FileIsAbsolute(const FileName: IPath): boolean;
    function ForceDirectories(const Dir: IPath): boolean;
    function RenameFile(const OldName, NewName: IPath): boolean;
    function DeleteFile(const FileName: IPath): boolean;
    function RemoveDir(const Dir: IPath): boolean;
    function CopyFile(const Source, Target: IPath; FailIfExists: boolean): boolean;

    function ExtractFileDrive(const FileName: IPath): IPath;
    function ExtractFilePath(const FileName: IPath): IPath;
    function ExtractFileDir(const FileName: IPath): IPath;
    function ExtractFileName(const FileName: IPath): IPath;
    function ExtractFileExt(const FileName: IPath): IPath;
    function ExtractRelativePath(const BaseName: IPath; const FileName: IPath): IPath;
    function ChangeFileExt(const FileName: IPath; const Extension: IPath): IPath;
    function IncludeTrailingPathDelimiter(const FileName: IPath): IPath;
    function ExcludeTrailingPathDelimiter(const FileName: IPath): IPath;

    function FileSearch(const Name: IPath; DirList: array of IPath): IPath;
    function FileFind(const FilePattern: IPath; Attr: integer): IFileIterator;

    function FindFirst(const FilePattern: IPath; Attr: integer; var F: TSytemSearchRec): integer;
    function FindNext(var F: TSytemSearchRec): integer;
    procedure FindClose(var F: TSytemSearchRec);

    function GetCurrentDir: IPath;
    function SetCurrentDir(const Dir: IPath): boolean;

    function IsCaseSensitive(): boolean;
  end;

  TFileIterator = class(TInterfacedObject, IFileIterator)
  private
    fHasNext: boolean;
    fSearchRec: TSytemSearchRec;
  public
    constructor Create(const FilePattern: IPath; Attr: integer);
    destructor Destroy(); override;

    function HasNext(): boolean;
    function Next(): TFileInfo;
  end;


var
  FileSystem_Singleton: IFileSystem;

function FileSystem(): IFileSystem;
begin
  Result := FileSystem_Singleton;
end;

function TFileSystemImpl.FileFind(const FilePattern: IPath; Attr: integer): IFileIterator;
begin
  Result := TFileIterator.Create(FilePattern, Attr);
end;

function TFileSystemImpl.IsCaseSensitive(): boolean;
begin
  // Windows and Mac OS X do not have case sensitive file systems
  {$IF Defined(MSWINDOWS) or Defined(DARWIN)}
    Result := false;
  {$ELSE}
    Result := true;
  {$IFEND}
end;

function TFileSystemImpl.FileIsAbsolute(const FileName: IPath): boolean;
var
  NameStr: UTF8String;
begin
  Result := true;
  NameStr := FileName.ToUTF8();

  {$IFDEF MSWINDOWS}
    // check if drive is given 'C:...'
    if (FileName.GetDrive().ToUTF8 <> '') then
      Exit;
    // check if path starts with '\\'
    if (Length(NameStr) >= 2) and
       (NameStr[1] = PathDelim) and (NameStr[2] = PathDelim) then
      Exit;
  {$ELSE} // Unix based systems
    // check if root dir given '/...'
    if (Length(NameStr) >= 1) and (NameStr[1] = PathDelim) then
      Exit;
  {$ENDIF}

  Result := false;
end;

{$IFDEF MSWINDOWS}

function TFileSystemImpl.ExpandFileName(const FileName: IPath): IPath;
begin
  Result := Path(WideExpandFileName(FileName.ToWide()));
end;

function TFileSystemImpl.FileCreate(const FileName: IPath): THandle;
begin
  Result := WideFileCreate(FileName.ToWide());
end;

function TFileSystemImpl.DirectoryCreate(const Dir: IPath): boolean;
begin
  Result := WideCreateDir(Dir.ToWide());
end;

function TFileSystemImpl.FileOpen(const FileName: IPath; Mode: longword): THandle;
begin
  Result := WideFileOpen(FileName.ToWide(), Mode);
end;

function TFileSystemImpl.FileAge(const FileName: IPath): integer;
begin
  Result := WideFileAge(FileName.ToWide());
end;

function TFileSystemImpl.FileAge(const FileName: IPath; out FileDateTime: TDateTime): boolean;
begin
  Result := WideFileAge(FileName.ToWide(), FileDateTime);
end;

function TFileSystemImpl.DirectoryExists(const Name: IPath): boolean;
begin
  Result := WideDirectoryExists(Name.ToWide());
end;

function TFileSystemImpl.FileExists(const Name: IPath): boolean;
begin
  Result := WideFileExists(Name.ToWide());
end;

function TFileSystemImpl.FileGetAttr(const FileName: IPath): Cardinal;
begin
  Result := WideFileGetAttr(FileName.ToWide());
end;

function TFileSystemImpl.FileSetAttr(const FileName: IPath; Attr: integer): boolean;
begin
  Result := WideFileSetAttr(FileName.ToWide(), Attr);
end;

function TFileSystemImpl.FileIsReadOnly(const FileName: IPath): boolean;
begin
  Result := WideFileIsReadOnly(FileName.ToWide());
end;

function TFileSystemImpl.FileSetReadOnly(const FileName: IPath; ReadOnly: boolean): boolean;
begin
  Result := WideFileSetReadOnly(FileName.ToWide(), ReadOnly);
end;

function TFileSystemImpl.ForceDirectories(const Dir: IPath): boolean;
begin
  Result := WideForceDirectories(Dir.ToWide());
end;

function TFileSystemImpl.FileSearch(const Name: IPath; DirList: array of IPath): IPath;
var
  I: integer;
  DirListStr: WideString;
begin
  DirListStr := '';
  for I := 0 to High(DirList) do
  begin
    if (I > 0) then
      DirListStr := DirListStr + PathSep;
    DirListStr := DirListStr + DirList[I].ToWide();
  end;
  Result := Path(WideFileSearch(Name.ToWide(), DirListStr));
end;

function TFileSystemImpl.RenameFile(const OldName, NewName: IPath): boolean;
begin
  Result := WideRenameFile(OldName.ToWide(), NewName.ToWide());
end;

function TFileSystemImpl.DeleteFile(const FileName: IPath): boolean;
begin
  Result := WideDeleteFile(FileName.ToWide());
end;

function TFileSystemImpl.RemoveDir(const Dir: IPath): boolean;
begin
  Result := WideRemoveDir(Dir.ToWide());
end;

function TFileSystemImpl.CopyFile(const Source, Target: IPath; FailIfExists: boolean): boolean;
begin
  Result := WideCopyFile(Source.ToWide(), Target.ToWide(), FailIfExists);
end;

function TFileSystemImpl.ExtractFileDrive(const FileName: IPath): IPath;
begin
  Result := Path(WideExtractFileDrive(FileName.ToWide()));
end;

function TFileSystemImpl.ExtractFilePath(const FileName: IPath): IPath;
begin
  Result := Path(WideExtractFilePath(FileName.ToWide()));
end;

function TFileSystemImpl.ExtractFileDir(const FileName: IPath): IPath;
begin
  Result := Path(WideExtractFileDir(FileName.ToWide()));
end;

function TFileSystemImpl.ExtractFileName(const FileName: IPath): IPath;
begin
  Result := Path(WideExtractFileName(FileName.ToWide()));
end;

function TFileSystemImpl.ExtractFileExt(const FileName: IPath): IPath;
begin
  Result := Path(WideExtractFileExt(FileName.ToWide()));
end;

function TFileSystemImpl.ExtractRelativePath(const BaseName: IPath; const FileName: IPath): IPath;
begin
  Result := Path(WideExtractRelativePath(BaseName.ToWide(), FileName.ToWide()));
end;

function TFileSystemImpl.ChangeFileExt(const FileName: IPath; const Extension: IPath): IPath;
begin
  Result := Path(WideChangeFileExt(FileName.ToWide(), Extension.ToWide()));
end;

function TFileSystemImpl.IncludeTrailingPathDelimiter(const FileName: IPath): IPath;
begin
  Result := Path(WideIncludeTrailingPathDelimiter(FileName.ToWide()));
end;

function TFileSystemImpl.ExcludeTrailingPathDelimiter(const FileName: IPath): IPath;
begin
  Result := Path(WideExcludeTrailingPathDelimiter(FileName.ToWide()));
end;

function TFileSystemImpl.FindFirst(const FilePattern: IPath; Attr: integer; var F: TSytemSearchRec): integer;
begin
  Result := WideFindFirst(FilePattern.ToWide(), Attr, F);
end;

function TFileSystemImpl.FindNext(var F: TSytemSearchRec): integer;
begin
  Result := WideFindNext(F);
end;

procedure TFileSystemImpl.FindClose(var F: TSytemSearchRec);
begin
  WideFindClose(F);
end;

function TFileSystemImpl.GetCurrentDir: IPath;
begin
  Result := Path(WideGetCurrentDir());
end;

function TFileSystemImpl.SetCurrentDir(const Dir: IPath): boolean;
begin
  Result := WideSetCurrentDir(Dir.ToWide());
end;

{$ELSE} // UNIX

function TFileSystemImpl.ExpandFileName(const FileName: IPath): IPath;
begin
  Result := Path(SysUtils.ExpandFileName(FileName.ToNative()));
end;

function TFileSystemImpl.FileCreate(const FileName: IPath): THandle;
begin
  Result := SysUtils.FileCreate(FileName.ToNative());
end;

function TFileSystemImpl.DirectoryCreate(const Dir: IPath): boolean;
begin
  Result := SysUtils.CreateDir(Dir.ToNative());
end;

function TFileSystemImpl.FileOpen(const FileName: IPath; Mode: longword): THandle;
begin
  Result := SysUtils.FileOpen(FileName.ToNative(), Mode);
end;

function TFileSystemImpl.FileAge(const FileName: IPath): integer;
begin
  Result := SysUtils.FileAge(FileName.ToNative());
end;

function TFileSystemImpl.FileAge(const FileName: IPath; out FileDateTime: TDateTime): boolean;
var
  FileDate: integer;
begin
  FileDate := SysUtils.FileAge(FileName.ToNative());
  Result := (FileDate <> -1);
  if (Result) then
    FileDateTime := FileDateToDateTime(FileDate);
end;

function TFileSystemImpl.DirectoryExists(const Name: IPath): boolean;
begin
  Result := SysUtils.DirectoryExists(Name.ToNative());
end;

function TFileSystemImpl.FileExists(const Name: IPath): boolean;
begin
  Result := SysUtils.FileExists(Name.ToNative());
end;

function TFileSystemImpl.FileGetAttr(const FileName: IPath): Cardinal;
begin
  Result := SysUtils.FileGetAttr(FileName.ToNative());
end;

function TFileSystemImpl.FileSetAttr(const FileName: IPath; Attr: integer): boolean;
begin
  Result := (SysUtils.FileSetAttr(FileName.ToNative(), Attr) = 0);
end;

function TFileSystemImpl.FileIsReadOnly(const FileName: IPath): boolean;
begin
  Result := SysUtils.FileIsReadOnly(FileName.ToNative());
end;

function TFileSystemImpl.FileSetReadOnly(const FileName: IPath; ReadOnly: boolean): boolean;
begin
  Result := (SysUtils.FileSetAttr(FileName.ToNative(), faReadOnly) = 0);
end;

function TFileSystemImpl.ForceDirectories(const Dir: IPath): boolean;
begin
  Result := SysUtils.ForceDirectories(Dir.ToNative());
end;

function TFileSystemImpl.FileSearch(const Name: IPath; DirList: array of IPath): IPath;
var
  I: integer;
  DirListStr: AnsiString;
begin
  DirListStr := '';
  for I := 0 to High(DirList) do
  begin
    if (I > 0) then
      DirListStr := DirListStr + PathSep;
    DirListStr := DirListStr + DirList[I].ToNative();
  end;
  Result := Path(SysUtils.FileSearch(Name.ToNative(), DirListStr));
end;

function TFileSystemImpl.RenameFile(const OldName, NewName: IPath): boolean;
begin
  Result := SysUtils.RenameFile(OldName.ToNative(), NewName.ToNative());
end;

function TFileSystemImpl.DeleteFile(const FileName: IPath): boolean;
begin
  Result := SysUtils.DeleteFile(FileName.ToNative());
end;

function TFileSystemImpl.RemoveDir(const Dir: IPath): boolean;
begin
  Result := SysUtils.RemoveDir(Dir.ToNative());
end;

function TFileSystemImpl.CopyFile(const Source, Target: IPath; FailIfExists: boolean): boolean;
const
  COPY_BUFFER_SIZE = 4096; // a good tradeoff between speed and memory consumption
var
  SourceFile, TargetFile: TFileStream;
  FileCopyBuffer: array [0..COPY_BUFFER_SIZE-1] of byte; // temporary copy-buffer.
  NumberOfBytes: integer; // number of bytes read from SourceFile
begin
  Result := false;
  SourceFile := nil;
  TargetFile := nil;

  // if overwrite is disabled return if the target file already exists
  if (FailIfExists and FileExists(Target)) then
    Exit;

  try
    try
      // open source and target file (might throw an exception on error)
      SourceFile := TFileStream.Create(Source.ToNative(), fmOpenRead);
      TargetFile := TFileStream.Create(Target.ToNative(), fmCreate or fmOpenWrite);

      while true do
      begin
        // read a block from the source file and check for errors or EOF
        NumberOfBytes := SourceFile.Read(FileCopyBuffer, SizeOf(FileCopyBuffer));
        if (NumberOfBytes <= 0) then
          Break;
        // write block to target file and check if everything was written
        if (TargetFile.Write(FileCopyBuffer, NumberOfBytes) <> NumberOfBytes) then
          Exit;
      end;
    except
      Exit;
    end;
  finally
    SourceFile.Free;
    TargetFile.Free;
  end;

  Result := true;
end;

function TFileSystemImpl.ExtractFileDrive(const FileName: IPath): IPath;
begin
  Result := Path(SysUtils.ExtractFileDrive(FileName.ToNative()));
end;

function TFileSystemImpl.ExtractFilePath(const FileName: IPath): IPath;
begin
  Result := Path(SysUtils.ExtractFilePath(FileName.ToNative()));
end;

function TFileSystemImpl.ExtractFileDir(const FileName: IPath): IPath;
begin
  Result := Path(SysUtils.ExtractFileDir(FileName.ToNative()));
end;

function TFileSystemImpl.ExtractFileName(const FileName: IPath): IPath;
begin
  Result := Path(SysUtils.ExtractFileName(FileName.ToNative()));
end;

function TFileSystemImpl.ExtractFileExt(const FileName: IPath): IPath;
begin
  Result := Path(SysUtils.ExtractFileExt(FileName.ToNative()));
end;

function TFileSystemImpl.ExtractRelativePath(const BaseName: IPath; const FileName: IPath): IPath;
begin
  Result := Path(SysUtils.ExtractRelativePath(BaseName.ToNative(), FileName.ToNative()));
end;

function TFileSystemImpl.ChangeFileExt(const FileName: IPath; const Extension: IPath): IPath;
begin
  Result := Path(SysUtils.ChangeFileExt(FileName.ToNative(), Extension.ToNative()));
end;

function TFileSystemImpl.IncludeTrailingPathDelimiter(const FileName: IPath): IPath;
begin
  Result := Path(SysUtils.IncludeTrailingPathDelimiter(FileName.ToNative()));
end;

function TFileSystemImpl.ExcludeTrailingPathDelimiter(const FileName: IPath): IPath;
begin
  Result := Path(SysUtils.ExcludeTrailingPathDelimiter(FileName.ToNative()));
end;

function TFileSystemImpl.FindFirst(const FilePattern: IPath; Attr: integer; var F: TSytemSearchRec): integer;
begin
  Result := SysUtils.FindFirst(FilePattern.ToNative(), Attr, F);
end;

function TFileSystemImpl.FindNext(var F: TSytemSearchRec): integer;
begin
  Result := SysUtils.FindNext(F);
end;

procedure TFileSystemImpl.FindClose(var F: TSytemSearchRec);
begin
  SysUtils.FindClose(F);
end;

function TFileSystemImpl.GetCurrentDir: IPath;
begin
  Result := Path(SysUtils.GetCurrentDir());
end;

function TFileSystemImpl.SetCurrentDir(const Dir: IPath): boolean;
begin
  Result := SysUtils.SetCurrentDir(Dir.ToNative());
end;

{$ENDIF}


{ TFileIterator }

constructor TFileIterator.Create(const FilePattern: IPath; Attr: integer);
begin
  inherited Create();
  fHasNext := (FileSystem.FindFirst(FilePattern, Attr, fSearchRec) = 0);
end;

destructor TFileIterator.Destroy();
begin
  FileSystem.FindClose(fSearchRec);
  inherited;
end;

function TFileIterator.HasNext(): boolean;
begin
  Result := fHasNext;
end;

function TFileIterator.Next(): TFileInfo;
begin
  if (not fHasNext) then
  begin
    // Note: do not use FillChar() on records with ref-counted fields
    Result.Time := 0;
    Result.Size := 0;
    Result.Attr := 0;
    Result.Name := nil;
    Exit;
  end;

  Result.Time := fSearchRec.Time;
  Result.Size := fSearchRec.Size;
  Result.Attr := fSearchRec.Attr;
  Result.Name := Path(fSearchRec.Name);

  // fetch next entry
  fHasNext := (FileSystem.FindNext(fSearchRec) = 0);
end;


initialization
  FileSystem_Singleton := TFileSystemImpl.Create;

finalization
  FileSystem_Singleton := nil;

end.
