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

unit UPath;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

interface

uses
  SysUtils,
  Classes,
  IniFiles,
  {$IFDEF MSWINDOWS}
  TntClasses,
  {$ENDIF}
  UConfig,
  UUnicodeUtils;

type
  IPath = interface;

  {**
   * TUnicodeMemoryStream
   *}
  TUnicodeMemoryStream = class(TMemoryStream)
  public
    procedure LoadFromFile(const FileName: IPath);
    procedure SaveToFile(const FileName: IPath);
  end;

  {**
   * Unicode capable IniFile implementation.
   * TMemIniFile and TIniFile are not able to handle INI-files with
   * an UTF-8 BOM. This implementation checks if an UTF-8 BOM exists
   * and removes it from the internal string-list.
   * UTF8Encoded is set accordingly.
   *}
  TUnicodeMemIniFile = class(TMemIniFile)
  private
    FFilename: IPath;
    FUTF8Encoded: boolean;
  public
    constructor Create(const FileName: IPath; UTF8Encoded: boolean = false); reintroduce;
    procedure UpdateFile; override;
    property UTF8Encoded: boolean READ FUTF8Encoded WRITE FUTF8Encoded;
  end;
  
  {**
   * TBinaryFileStream (inherited from THandleStream)
   *}
  {$IFDEF MSWINDOWS}
  TBinaryFileStream = class(TTntFileStream)
  {$ELSE}
  TBinaryFileStream = class(TFileStream)
  {$ENDIF}
  public
    {**
     * @seealso TFileStream.Create for valid Mode parameters
     *}
    constructor Create(const FileName: IPath; Mode: word);
  end;

  {**
   * TTextFileStream
   *}
  TTextFileStream = class(TStream)
  protected
    fLineBreak: RawByteString;
    fFilename: IPath;
    fMode: word;

    function ReadLine(var Success: boolean): RawByteString; overload; virtual; abstract;
  public
    constructor Create(Filename: IPath; Mode: word);

    function ReadString(): RawByteString; virtual; abstract;
    function ReadLine(var Line: UTF8String): boolean; overload;
    function ReadLine(var Line: AnsiString): boolean; overload;

    procedure WriteString(const Str: RawByteString); virtual;
    procedure WriteLine(const Line: RawByteString); virtual;

    property LineBreak: RawByteString read fLineBreak write fLineBreak;
    property Filename: IPath read fFilename;
  end;

  {**
   * TMemTextStream
   *}
  TMemTextFileStream = class(TTextFileStream)
  private
    fStream: TMemoryStream;
  protected
    function GetSize: int64; override;

    {**
     * Copies fStream.Memory from StartPos to EndPos-1 to the result string;
     *}
    function CopyMemString(StartPos: int64; EndPos: int64): RawByteString;
  public
    constructor Create(Filename: IPath; Mode: word);
    destructor Destroy(); override;

    function Read(var Buffer; Count: longint): longint; override;
    function Write(const Buffer; Count: longint): longint; override;
    function Seek(Offset: longint; Origin: word): longint; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;

    function ReadLine(var Success: boolean): RawByteString; override;
    function ReadString(): RawByteString; override;
  end;

  {**
  TUnicodeIniStream = class()
  end;
  *}

  {**
   * pdKeep:   Keep path as is, neither remove or append a delimiter
   * pdAppend: Append a delimiter if path does not have a trailing one 
   * pdRemove: Remove a trailing delimiter from the path 
   *}
  TPathDelimOption = (pdKeep, pdAppend, pdRemove);

  IPathDynArray = array of IPath;

  {**
   * An IPath represents a filename, a directory or a filesystem path in general.
   * It hides some of the operating system's specifics like path delimiters
   * and encodings and provides an easy to use interface to handle them.
   * Internally all paths are stored with the same path delimiter (PathDelim)
   * and encoding (UTF-8). The transformation is already done AT THE CREATION of
   * the IPath and hence calls to e.g. IPath.Equal() will not distinguish between
   * Unix and Windows style paths.
   *
   * Create new paths with one of the Path() functions.
   * If you need a string representation use IPath.ToNative/ToUTF8/ToWide.
   * Note that due to the path-delimiter and encoding transformation the string
   * might have changed. Path('one\test/path').ToUTF8() might return 'one/test/path'.
   *
   * It is recommended to use an IPath as long as possible without a string
   * conversion (IPath.To...()). The whole Delphi (< 2009) and FPC RTL is ANSI
   * only on Windows. If you would use for example FileExists(MyPath.ToNative)
   * it would not find a file which contains characters that are not in the
   * current locale. Same applies to AssignFile(), TFileStream.Create() and
   * everything else in the RTL that expects a filename.
   * As a rule of thumb: NEVER use any of the Delphi/FPC RTL filename functions
   * if the filename parameter is not of a UTF8String or WideString type.
   *
   * If you need to open a file use TBinaryStream or TFileStream instead. Many
   * of the RTL classes offer a LoadFromStream() method so ANSI Open() methods
   * can be workaround.
   *
   * If there is only a ANSI and no IPath/UTF-8/WideString version and you cannot
   * even pass a stream instead of a filename be aware that even if you know that
   * a filename is ASCII only, subdirectories in an absolute path might contain
   * some non-ASCII characters (for example the user's name) and hence might
   * fail (if the characters are not in the current locale).
   * It is rare but it happens.
   *
   * IMPORTANT:
   *   This interface needs the cwstring unit on Unix (Max OS X / Linux) systems.
   *   Cwstring functions (WideUpperCase, ...) cannot be used by external threads
   *   as FPC uses Thread-Local-Storage for the implementation. As a result do not
   *   call IPath stuff by external threads (e.g. in C callbacks or by SDL-threads).
   *}
  IPath = interface
  ['{686BF103-CE43-4598-B85D-A2C3AF950897}']
    {**
     * Returns the path as an UTF8 encoded string.
     * If UseNativeDelim is set to true, the native path delimiter ('\' on win32)
     * is used. If it is set to false the (more) portable '/' delimiter will used.
     *}
    function ToUTF8(UseNativeDelim: boolean = true): UTF8String;

    {**
     * Returns the path as an UTF-16 encoded string.
     * If UseNativeDelim is set to true, the native path delimiter ('\' on win32)
     * is used. If it is set to false the delimiter will be '/'.
     *}
    function ToWide(UseNativeDelim: boolean = true): WideString;

    {**
     * Returns the path with the system's native encoding and path delimiter.
     * Win32: ANSI (use the UTF-16 version IPath.ToWide() whenever possible)
     * Mac:   UTF8
     * Unix:  UTF8 or ANSI according to LC_CTYPE
     *}
    function ToNative(): RawByteString;

    {**
     * Note: File must be closed with FileClose(Handle) after usage
     * @seealso SysUtils.FileOpen()
     *}
    function Open(Mode: longword): THandle;

    {** @seealso SysUtils.ExtractFileDrive() *}
    function GetDrive(): IPath;

    {** @seealso SysUtils.ExtractFilePath() *}
    function GetPath(): IPath;

    {** @seealso SysUtils.ExtractFileDir() *}
    function GetDir(): IPath;

    {** @seealso SysUtils.ExtractFileName() *}
    function GetName(): IPath;

    {** @seealso SysUtils.ExtractFileExtension() *}
    function GetExtension(): IPath;

    {**
     * Returns a copy of the path with the extension changed to Extension.
     * The file itself is not changed, use Rename() for this task.
     * @seealso SysUtils.ChangeFileExt()
     *}
    function SetExtension(const Extension: IPath): IPath; overload;
    function SetExtension(const Extension: RawByteString): IPath; overload;
    function SetExtension(const Extension: WideString): IPath; overload;

    {**
     * Returns the representation of the path relative to Basename.
     * Note that the basename must be terminated with a path delimiter
     * otherwise the last path component will be ignored.
     * @seealso SysUtils.ExtractRelativePath()
     *}
    function GetRelativePath(const BaseName: IPath): IPath;

    {** @seealso SysUtils.ExpandFileName() *}
    function GetAbsolutePath(): IPath;

    {**
     * Returns the concatenation of this path with Child. If this path does not
     * end with a path delimiter one is inserted in front of the Child path.
     * Example: Path('parent').Append(Path('child')) -> Path('parent/child')
     *}
    function Append(const Child: IPath; DelimOption: TPathDelimOption = pdKeep): IPath; overload;
    function Append(const Child: RawByteString; DelimOption: TPathDelimOption = pdKeep): IPath; overload;
    function Append(const Child: WideString; DelimOption: TPathDelimOption = pdKeep): IPath; overload;

    {**
     * Splits the path into its components. Path delimiters are not removed from
     * components. 
     * Example: C:\test\my\dir -> ['C:\', 'test\', 'my\', 'dir']
     *}
    function SplitDirs(): IPathDynArray;

    {**
     * Returns the parent directory or PATH_NONE if none exists.
     *}
    function GetParent(): IPath;

    {**
     * Checks if this path is a subdir of or file inside Parent.
     * If Direct is true this path must be a direct child.
     * Example: C:\test\file is a direct child of C:\test and a child of C:\
     *}
    function IsChildOf(const Parent: IPath; Direct: boolean): boolean;

    {**
     * Adjusts the case of the path on case senstitive filesystems.
     * If the path does not exist or the filesystem is case insensitive
     * the original path will be returned. Otherwise a corrected copy.
     *}
    function AdjustCase(AdjustAllLevels: boolean): IPath;

    {** @seealso SysUtils.IncludeTrailingPathDelimiter() *}
    function AppendPathDelim(): IPath;

    {** @seealso SysUtils.ExcludeTrailingPathDelimiter() *}
    function RemovePathDelim(): IPath;

    function Exists(): boolean;
    function IsFile(): boolean;
    function IsDirectory(): boolean;
    function IsAbsolute(): boolean;
    function GetFileAge(): integer; overload;
    function GetFileAge(out FileDateTime: TDateTime): boolean; overload;
    function GetAttr(): cardinal;
    function SetAttr(Attr: Integer): boolean;
    function IsReadOnly(): boolean;
    function SetReadOnly(ReadOnly: boolean): boolean;

    {**
     * Checks if this path points to nothing, that means the path consists of
     * the empty string '' and hence equals PATH_NONE.
     * This is a shortcut for IPath.Equals('') or IPath.Equals(PATH_NONE).
     * If IsUnset() returns true this path and PATH_NONE are equal but they must
     * not be identical as the references might point to different objects.
     *
     * Example:
     *   Path('').Equals(PATH_EMPTY) -> true
     *   Path('') = PATH_EMPTY       -> false
     *}
    function IsUnset(): boolean;
    function IsSet(): boolean;

    {**
     * Compares this path with Other and returns true if both paths are
     * equal. Both paths are expanded and trailing slashes excluded before
     * comparison. If IgnoreCase is true, the case will be ignored on
     * case-sensitive filesystems.
     *}
    function Equals(const Other: IPath; IgnoreCase: boolean = false): boolean; overload;
    function Equals(const Other: RawByteString; IgnoreCase: boolean = false): boolean; overload;
    function Equals(const Other: WideString; IgnoreCase: boolean = false): boolean; overload;

    {**
     * Searches for a file in DirList. The Result is nil if the file was
     * not found. Use IFileSystem.FileFind() instead if you want to use
     * wildcards.
     * @seealso SysUtils.FileSearch()
     *}
    function FileSearch(const DirList: IPath): IPath;

    {** File must be closed with FileClose(Handle) after usage }
    function CreateFile(): THandle;
    function DeleteFile(): boolean;
    function CreateDirectory(Force: boolean = false): boolean;
    function DeleteEmptyDir(): boolean;
    function Rename(const NewName: IPath): boolean;
    function CopyFile(const Target: IPath; FailIfExists: boolean): boolean;

    // TODO: Dirwatch stuff
    // AddFileChangeListener(Listener: TFileChangeListener);

    {**
     * Internal string representation. For debugging only.
     *}
    function GetIntern: UTF8String;
    property Intern: UTF8String READ GetIntern;
  end;

{**
 * Creates a new path with the given pathname. PathName can be either in UTF8
 * or the local encoding.
 * Notes:
 * - On Apple only UTF8 is supported
 * - Same applies to Unix with LC_CTYPE set to UTF8 encoding (default on newer systems)
 *}
function Path(const PathName: RawByteString; DelimOption: TPathDelimOption = pdKeep): IPath; overload;

{**
 * Creates a new path with the given UTF-16 pathname.
 *}
function Path(const PathName: WideString; DelimOption: TPathDelimOption = pdKeep): IPath; overload;

{**
 * Returns a singleton for Path('').
 *}
function PATH_NONE(): IPath;

implementation

uses
  RTLConsts,
  UTextEncoding,
  UFilesystem;

{*
 * Due to a compiler bug in FPC <= 2.2.4 reference counting does not work
 * properly with interfaces (see http://bugs.freepascal.org/view.php?id=14019).
 *
 * There are two (probably more) scenarios causes a program to crash:
 *
 * 1. Assume we execute Path('fail').GetParent().ToUTF8(). The compiler will
 * internally create a temporary variable to hold the result of Path('fail').
 * This temporary var is then passed as Self to GetParent(). Unfortunately FPC
 * does already decrement the ref-count of the temporary var at the end of the
 * call to Path('fail') and the ref-count drops to zero and the temp object
 * is destroyed as FPC erroneously assumes that the temp is not used anymore.
 * As a result the Self variable in GetParent() will be invalid, the same
 * applies to TPathImpl.fName which reference count dropped to zero when the
 * temp was destroyed. Hence GetParent() will likely crash.
 * If it does not, ToUTF8() will either return some random string
 * (e.g. '' or stupid stuff like 'fhwkjehdk') or crash.
 * Either way the result of ToUTF8() is messed up.
 * This scenario applies whenever a function (or method) is called that returns
 * an interfaced object (e.g. an IPath) and the result is used without storing
 * a reference to it in a (temporary) variable first.
 *
 *  Tmp := Path('fail'); Tmp2 := Tmp.GetParent(); Tmp2.ToUTF8();
 *
 * will not crash but is very impractical and error-prone. Note that Tmp2 cannot
 * be replaced with Tmp (see scenario 2).
 *
 * 2. Another situation this bug will ruin our lives is when a variable to an
 * interfaced object is used at the left and right side of an assignment as in:
 *   MyPath := MyPath.GetParent()
 *
 * Although the bug is already fixed in the FPC development version 2.3.1
 * it will take quite some time till the next FPC release (> 2.2.4) in which
 * this issue is fixed.
 *
 * To workaround this bug we use some very simple and stupid kind of garbage
 * collection. New IPaths are stored in an IInterfaceList (call it GarbaegeList)
 * to artificially increase the ref-count of the newly created object.
 * This keeps the object alive when FPC's temporary variable comes to the end
 * of its lifetime and the object's ref-count is decremented
 * (and is now 1 instead of 0).
 * Later on, the object is either garbage or referenced by another variable.
 *
 * Look at
 *   MyPath := Path('SomeDir/SubDir').GetParent()
 *
 * (1) The result of Path('SomeDir/SubDir') is garbage as it is not used anymore.
 * (2) The result of GetParent() is referenced by MyPath
 * Object (1) has a reference count of 1 (as it is only referenced by the
 * GarbageList). Object (2) is referenced twice (MyPath + GarbageList).
 * When the reference to (2) is finally stored in MyPath we can safely remove
 * (1) and (2) from the GarbageList so (1) will be freed and the ref-count of
 * (2) will be decremented to 1.
 *
 * As we do not know when it is safe to remove an object from the GarbageList
 * we assume that there are max. GarbageMaxCount IPath elements created until
 * the execution of the expression is performed and a reference to the resulting
 * object is assigned to a variable so all temps can be safely deleted.
 *
 * Worst-case scenarios are recursive calls or calls with large call stacks with
 * functions that return an IPath. Also keep in mind that multiple threads might
 * be executing such functions at the same time.
 * A reasonable count might be a max. of 20.000 elements. With an average length
 * of 40 UTF8 chars (maybe 60 byte with class info, pointer etc.) per IPath
 * this will consume ~1.2MB.
 *}
{$IFDEF FPC}
{$IF FPC_VERSION_INT <= 002002004} // <= 2.2.4
  {$DEFINE HAVE_REFCNTBUG}
{$IFEND}
{$ENDIF}

{$IFDEF HAVE_REFCNTBUG}
const
  // when GarbageList.Count reaches GarbageMaxCount the oldest references in
  // GarbageList will be deleted until GarbageList.Count equals GarbageAfterCleanCount.
  GarbageMaxCount = 20000;
  GarbageAfterCleanCount = GarbageMaxCount-1000;

var
  GarbageList: IInterfaceList;
{$ENDIF}

type
  TPathImpl = class(TInterfacedObject, IPath)
    private
      fName: UTF8String; //<** internal filename string, always UTF8 with PathDelim

      {**
       * Unifies the filename. Path-delimiters are replaced by '/'.
       *}
      procedure Unify(DelimOption: TPathDelimOption);

      {**
       * Returns a copy of fName with path delimiters changed to '/'.
       *}
      function GetPortableString(): UTF8String;

      procedure AssertRefCount; {$IFDEF HasInline}inline;{$ENDIF}

    public
      constructor Create(const Name: UTF8String; DelimOption: TPathDelimOption);
      destructor Destroy(); override;

      function ToUTF8(UseNativeDelim: boolean): UTF8String;
      function ToWide(UseNativeDelim: boolean): WideString;
      function ToNative(): RawByteString;

      function Open(Mode: longword): THandle;

      function GetDrive(): IPath;
      function GetPath(): IPath;
      function GetDir(): IPath;
      function GetName(): IPath;
      function GetExtension(): IPath;

      function SetExtension(const Extension: IPath): IPath; overload;
      function SetExtension(const Extension: RawByteString): IPath; overload;
      function SetExtension(const Extension: WideString): IPath; overload;

      function GetRelativePath(const BaseName: IPath): IPath;
      function GetAbsolutePath(): IPath;
      function GetParent(): IPath;
      function SplitDirs(): IPathDynArray;

      function Append(const Child: IPath; DelimOption: TPathDelimOption): IPath; overload;
      function Append(const Child: RawByteString; DelimOption: TPathDelimOption): IPath; overload;
      function Append(const Child: WideString; DelimOption: TPathDelimOption): IPath; overload;

      function Equals(const Other: IPath; IgnoreCase: boolean): boolean; overload;
      function Equals(const Other: RawByteString; IgnoreCase: boolean): boolean; overload;
      function Equals(const Other: WideString; IgnoreCase: boolean): boolean; overload;

      function IsChildOf(const Parent: IPath; Direct: boolean): boolean;

      function AdjustCase(AdjustAllLevels: boolean): IPath;

      function AppendPathDelim(): IPath;
      function RemovePathDelim(): IPath;

      function GetFileAge(): integer; overload;
      function GetFileAge(out FileDateTime: TDateTime): boolean; overload;
      function Exists(): boolean;
      function IsFile(): boolean;
      function IsDirectory(): boolean;
      function IsAbsolute(): boolean;
      function GetAttr(): cardinal;
      function SetAttr(Attr: Integer): boolean;
      function IsReadOnly(): boolean;
      function SetReadOnly(ReadOnly: boolean): boolean;

      function IsUnset(): boolean;
      function IsSet(): boolean;

      function FileSearch(const DirList: IPath): IPath;

      function CreateFile(): THandle;
      function DeleteFile(): boolean;
      function CreateDirectory(Force: boolean): boolean;
      function DeleteEmptyDir(): boolean;
      function Rename(const NewName: IPath): boolean;
      function CopyFile(const Target: IPath; FailIfExists: boolean): boolean;

      function GetIntern(): UTF8String;
  end;

function Path(const PathName: RawByteString; DelimOption: TPathDelimOption): IPath;
begin
  if (IsUTF8String(PathName)) then
    Result := TPathImpl.Create(PathName, DelimOption)
  else if (IsNativeUTF8()) then
    Result := PATH_NONE
  else
    Result := TPathImpl.Create(AnsiToUtf8(PathName), DelimOption);
end;

function Path(const PathName: WideString; DelimOption: TPathDelimOption): IPath;
begin
  Result := TPathImpl.Create(UTF8Encode(PathName), DelimOption);
end;



procedure TPathImpl.AssertRefCount;
begin
  {$IFDEF HAVE_REFCNTBUG}
  if (FRefCount <= 0) then
    raise Exception.Create('RefCount error: ' + IntToStr(FRefCount));
  {$ENDIF}
end;

constructor TPathImpl.Create(const Name: UTF8String; DelimOption: TPathDelimOption);
begin
  inherited Create();
  fName := Name;
  Unify(DelimOption);
  {$IFDEF HAVE_REFCNTBUG}
  GarbageList.Lock;
  if (GarbageList.Count >= GarbageMaxCount) then
  begin
    while (GarbageList.Count > GarbageAfterCleanCount) do
      GarbageList.Delete(0);
  end;
  GarbageList.Add(Self);
  GarbageList.Unlock;
  {$ENDIF}
end;

destructor TPathImpl.Destroy();
begin
  inherited;
end;

procedure TPathImpl.Unify(DelimOption: TPathDelimOption);
var
  I: integer;
begin
  // convert all path delimiters to native ones
  for I := 1 to Length(fName) do
  begin
    if (fName[I] in ['\', '/']) and (fName[I] <> PathDelim) then
      fName[I] := PathDelim;
  end;

  // Include/ExcludeTrailingPathDelimiter need PathDelim as path delimiter 
  case DelimOption of
    pdAppend: fName := IncludeTrailingPathDelimiter(fName);
    pdRemove: fName := ExcludeTrailingPathDelimiter(fName);
  end;
end;

function TPathImpl.GetPortableString(): UTF8String;
var
  I: integer;
begin
  Result := fName;
  if (PathDelim = '/') then
    Exit;

  for I := 1 to Length(Result) do
  begin
    if (Result[I] = PathDelim) then
      Result[I] := '/';
  end;
end;

function TPathImpl.ToUTF8(UseNativeDelim: boolean): UTF8String;
begin
  AssertRefCount;

  if (UseNativeDelim) then
    Result := fName
  else
    Result := GetPortableString();
end;

function TPathImpl.ToWide(UseNativeDelim: boolean): WideString;
begin
  if (UseNativeDelim) then
    Result := UTF8Decode(fName)
  else
    Result := UTF8Decode(GetPortableString());
end;

function TPathImpl.ToNative(): RawByteString;
begin
  if (IsNativeUTF8()) then
    Result := fName
  else
    Result := Utf8ToAnsi(fName);
end;

function TPathImpl.GetDrive(): IPath;
begin
  AssertRefCount;
  Result := FileSystem.ExtractFileDrive(Self);
end;

function TPathImpl.GetPath(): IPath;
begin
  AssertRefCount;
  Result := FileSystem.ExtractFilePath(Self);
end;

function TPathImpl.GetDir(): IPath;
begin
  AssertRefCount;
  Result := FileSystem.ExtractFileDir(Self);
end;

function TPathImpl.GetName(): IPath;
begin
  AssertRefCount;
  Result := FileSystem.ExtractFileName(Self);
end;

function TPathImpl.GetExtension(): IPath;
begin
  AssertRefCount;
  Result := FileSystem.ExtractFileExt(Self);
end;

function TPathImpl.SetExtension(const Extension: IPath): IPath;
begin
  AssertRefCount;
  Result := FileSystem.ChangeFileExt(Self, Extension);
end;

function TPathImpl.SetExtension(const Extension: RawByteString): IPath;
begin
  Result := SetExtension(Path(Extension));
end;

function TPathImpl.SetExtension(const Extension: WideString): IPath;
begin
  Result := SetExtension(Path(Extension));
end;

function TPathImpl.GetRelativePath(const BaseName: IPath): IPath;
begin
  AssertRefCount;
  Result := FileSystem.ExtractRelativePath(BaseName, Self);
end;

function TPathImpl.GetAbsolutePath(): IPath;
begin
  AssertRefCount;
  Result := FileSystem.ExpandFileName(Self);
end;

function TPathImpl.GetParent(): IPath;
var
  CurPath, ParentPath: IPath;
begin
  AssertRefCount;

  Result := PATH_NONE;

  CurPath := Self.RemovePathDelim();
  // check if current path has a parent (no further '/')
  if (Pos(PathDelim, CurPath.ToUTF8()) = 0) then
    Exit;

  // set new path and check if it has changed to avoid endless loops
  // e.g. with invalid paths like '/C:' (GetPath() uses ':' as delimiter too)
  // on delphi/win32
  ParentPath := CurPath.GetPath();
  if (ParentPath.ToUTF8 = CurPath.ToUTF8)  then
    Exit;

  Result := ParentPath;
end;

function TPathImpl.SplitDirs(): IPathDynArray;
var
  CurPath: IPath;
  Components: array of IPath;
  CurPathStr: UTF8String;
  DelimPos: integer;
  I: integer;
begin
  SetLength(Result, 0);

  if (Length(Self.ToUTF8(true)) = 0) then
    Exit;

  CurPath := Self;
  SetLength(Components, 0);
  repeat
    SetLength(Components, Length(Components)+1);

    CurPathStr := CurPath.ToUTF8();
    DelimPos := LastDelimiter(PathDelim, SysUtils.ExcludeTrailingPathDelimiter(CurPathStr));
    Components[High(Components)] := Path(Copy(CurPathStr, DelimPos+1, Length(CurPathStr)));

    CurPath := CurPath.GetParent();
  until (CurPath = PATH_NONE);

  // reverse list
  SetLength(Result, Length(Components));
  for I := 0 to High(Components) do
    Result[I] := Components[High(Components)-I];
end;

function TPathImpl.Append(const Child: IPath; DelimOption: TPathDelimOption): IPath;
var
  TmpResult: IPath;
begin
  AssertRefCount;

  if (fName = '') then
    TmpResult := Child
  else
    TmpResult := Path(Self.AppendPathDelim().ToUTF8() + Child.ToUTF8());

  case DelimOption of
    pdKeep: Result := TmpResult;
    pdAppend: Result := TmpResult.AppendPathDelim;
    pdRemove: Result := TmpResult.RemovePathDelim;
  end;
end;

function TPathImpl.Append(const Child: RawByteString; DelimOption: TPathDelimOption): IPath;
begin
  AssertRefCount;
  Result := Append(Path(Child), DelimOption);
end;

function TPathImpl.Append(const Child: WideString; DelimOption: TPathDelimOption): IPath;
begin
  AssertRefCount;
  Result := Append(Path(Child), DelimOption);
end;

function TPathImpl.Equals(const Other: IPath; IgnoreCase: boolean): boolean;
var
  SelfPath, OtherPath: UTF8String;
begin
  SelfPath := Self.GetAbsolutePath().RemovePathDelim().ToUTF8();
  OtherPath := Other.GetAbsolutePath().RemovePathDelim().ToUTF8();
  if (FileSystem.IsCaseSensitive() and not IgnoreCase) then
    Result := (CompareStr(SelfPath, OtherPath) = 0)
  else
    Result := (CompareText(SelfPath, OtherPath) = 0);
end;

function TPathImpl.Equals(const Other: RawByteString; IgnoreCase: boolean): boolean;
begin
  Result := Equals(Path(Other), IgnoreCase);
end;

function TPathImpl.Equals(const Other: WideString; IgnoreCase: boolean): boolean;
begin
  Result := Equals(Path(Other), IgnoreCase);
end;

function TPathImpl.IsChildOf(const Parent: IPath; Direct: boolean): boolean;
var
  SelfPath, ParentPath: UTF8String;
begin
  Result := false;

  if (Direct) then
  begin
    SelfPath := Self.GetParent().GetAbsolutePath().AppendPathDelim().ToUTF8();
    ParentPath := Parent.GetAbsolutePath().AppendPathDelim().ToUTF8();

    // simply check if this paths parent path (SelfPath) equals ParentPath
    Result := (SelfPath = ParentPath);
  end
  else
  begin
    SelfPath := Self.GetAbsolutePath().AppendPathDelim().ToUTF8();
    ParentPath := Parent.GetAbsolutePath().AppendPathDelim().ToUTF8();

    if (Length(SelfPath) <= Length(ParentPath)) then
      Exit;

    // check if ParentPath is a substring of SelfPath
    if (FileSystem.IsCaseSensitive()) then
      Result := (StrLComp(PAnsiChar(SelfPath), PAnsiChar(ParentPath), Length(ParentPath)) = 0)
    else
      Result := (StrLIComp(PAnsiChar(SelfPath), PAnsiChar(ParentPath), Length(ParentPath)) = 0)
  end;
end;

function AdjustCaseRecursive(CurPath: IPath; AdjustAllLevels: boolean): IPath;
var
  OldParent, AdjustedParent: IPath;
  LocalName: IPath;
  PathFound: IPath;
  PathWithAdjParent: IPath;
  SearchInfo: TFileInfo;
  FileIter: IFileIterator;
  Pattern: IPath;
begin
  // if case-sensitive path exists there is no need to adjust case
  if (CurPath.Exists()) then
  begin
    Result := CurPath;
    Exit;
  end;

  LocalName := CurPath.RemovePathDelim().GetName();

  // try to adjust parent
  OldParent := CurPath.GetParent();
  if (OldParent <> PATH_NONE) then
  begin
    if (not AdjustAllLevels) then
    begin
      AdjustedParent := OldParent;
    end
    else
    begin
      AdjustedParent := AdjustCaseRecursive(OldParent, AdjustAllLevels);
      if (AdjustedParent = nil) then
      begin
        // parent path was not found case-insensitive
        Result := nil;
        Exit;
      end;

      // check if the path with adjusted parent can be found now
      PathWithAdjParent := AdjustedParent.Append(LocalName);
      if (PathWithAdjParent.Exists()) then
      begin
        Result := PathWithAdjParent;
        Exit;
      end;
    end;
    Pattern := AdjustedParent.Append(Path('*'));
  end
  else // path has no parent
  begin
    // the top path can either be absolute or relative
    if (CurPath.IsAbsolute) then
    begin
      // the only absolute directory at Unix without a parent is root ('/')
      // and hence does not need to be adjusted
      Result := CurPath;
      Exit;
    end;
    // this is a relative path, search in the current working dir
    AdjustedParent := nil;
    Pattern := Path('*');
  end;

  // compare name with all files in the current directory case-insensitive
  FileIter := FileSystem.FileFind(Pattern, faAnyFile);
  while (FileIter.HasNext()) do
  begin
    SearchInfo := FileIter.Next();
    PathFound := SearchInfo.Name;
    if (CompareText(LocalName.ToUTF8, PathFound.ToUTF8) = 0) then
    begin
      if (AdjustedParent <> nil) then
        Result := AdjustedParent.Append(PathFound)
      else
        Result := PathFound;
      Exit;
    end;
  end;

  // no matching file found
  Result := nil;
end;

function TPathImpl.AdjustCase(AdjustAllLevels: boolean): IPath;
begin
  AssertRefCount;

  Result := Self;

  if (FileSystem.IsCaseSensitive) then
  begin
    Result := AdjustCaseRecursive(Self, AdjustAllLevels);
    if (Result = nil) then
      Result := Self;
  end;
end;

function TPathImpl.AppendPathDelim(): IPath;
begin
  AssertRefCount;
  Result := FileSystem.IncludeTrailingPathDelimiter(Self);
end;

function TPathImpl.RemovePathDelim(): IPath;
begin
  AssertRefCount;
  Result := FileSystem.ExcludeTrailingPathDelimiter(Self);
end;

function TPathImpl.CreateFile(): THandle;
begin
  Result := FileSystem.FileCreate(Self);
end;

function TPathImpl.CreateDirectory(Force: boolean): boolean;
begin
  if (Force) then
    Result := FileSystem.ForceDirectories(Self)
  else
    Result := FileSystem.DirectoryCreate(Self);
end;

function TPathImpl.Open(Mode: longword): THandle;
begin
  Result := FileSystem.FileOpen(Self, Mode);
end;

function TPathImpl.GetFileAge(): integer;
begin
  Result := FileSystem.FileAge(Self);
end;

function TPathImpl.GetFileAge(out FileDateTime: TDateTime): boolean;
begin
  Result := FileSystem.FileAge(Self, FileDateTime);
end;

function TPathImpl.Exists(): boolean;
begin
  // note the different specifications of FileExists() on Win32 <> Unix
  {$IFDEF MSWINDOWS}
  Result := IsFile() or IsDirectory();
  {$ELSE}
  Result := FileSystem.FileExists(Self);
  {$ENDIF}
end;

function TPathImpl.IsFile(): boolean;
begin
  // note the different specifications of FileExists() on Win32 <> Unix
  {$IFDEF MSWINDOWS}
  Result := FileSystem.FileExists(Self);
  {$ELSE}
  Result := Exists() and not IsDirectory();
  {$ENDIF}
end;

function TPathImpl.IsDirectory(): boolean;
begin
  Result := FileSystem.DirectoryExists(Self);
end;

function TPathImpl.IsAbsolute(): boolean;
begin
  AssertRefCount;
  Result := FileSystem.FileIsReadOnly(Self);
end;

function TPathImpl.GetAttr(): cardinal;
begin
  Result := FileSystem.FileGetAttr(Self);
end;

function TPathImpl.SetAttr(Attr: Integer): boolean;
begin
  Result := FileSystem.FileSetAttr(Self, Attr);
end;

function TPathImpl.IsReadOnly(): boolean;
begin
  Result := FileSystem.FileIsReadOnly(Self);
end;

function TPathImpl.SetReadOnly(ReadOnly: boolean): boolean;
begin
  Result := FileSystem.FileSetReadOnly(Self, ReadOnly);
end;

function TPathImpl.IsUnset(): boolean;
begin
  Result := (fName = '');
end;

function TPathImpl.IsSet(): boolean;
begin
  Result := (fName <> '');
end;

function TPathImpl.FileSearch(const DirList: IPath): IPath;
begin
  AssertRefCount;
  Result := FileSystem.FileSearch(Self, DirList);
end;

function TPathImpl.Rename(const NewName: IPath): boolean;
begin
  Result := FileSystem.RenameFile(Self, NewName);
end;

function TPathImpl.DeleteFile(): boolean;
begin
  Result := FileSystem.DeleteFile(Self);
end;

function TPathImpl.DeleteEmptyDir(): boolean;
begin
  Result := FileSystem.RemoveDir(Self);
end;

function TPathImpl.CopyFile(const Target: IPath; FailIfExists: boolean): boolean;
begin
  Result := FileSystem.CopyFile(Self, Target, FailIfExists);
end;

function TPathImpl.GetIntern(): UTF8String;
begin
  Result := fName;
end;


{ TBinaryFileStream }

constructor TBinaryFileStream.Create(const FileName: IPath; Mode: word);
begin
{$IFDEF MSWINDOWS}
  inherited Create(FileName.ToWide(), Mode);
{$ELSE}
  inherited Create(FileName.ToNative(), Mode);
{$ENDIF}
end;

{ TTextStream }

constructor TTextFileStream.Create(Filename: IPath; Mode: word);
begin
  inherited Create();
  fMode := Mode;
  fFilename := Filename;
  fLineBreak := sLineBreak;
end;

function TTextFileStream.ReadLine(var Line: UTF8String): boolean;
begin
  Line := ReadLine(Result);
end;

function TTextFileStream.ReadLine(var Line: AnsiString): boolean;
begin
  Line := ReadLine(Result);
end;

procedure TTextFileStream.WriteString(const Str: RawByteString);
begin
  WriteBuffer(Str[1], Length(Str));
end;

procedure TTextFileStream.WriteLine(const Line: RawByteString);
begin
  WriteBuffer(Line[1], Length(Line));
  WriteBuffer(fLineBreak[1], Length(fLineBreak));
end;

{ TMemTextStream }

constructor TMemTextFileStream.Create(Filename: IPath; Mode: word);
var
  FileStream: TBinaryFileStream;
begin
  inherited Create(Filename, Mode);

  fStream := TMemoryStream.Create();

  // load data to memory in read mode
  if ((Mode and 3) in [fmOpenRead, fmOpenReadWrite]) then
  begin
    FileStream := TBinaryFileStream.Create(Filename, fmOpenRead);
    try
      fStream.LoadFromStream(FileStream);
    finally
      FileStream.Free;
    end;
  end
  // check if file exists for write-mode
  else if ((Mode and 3) = fmOpenWrite) and (not Filename.IsFile) then
  begin
    raise EFOpenError.CreateResFmt(@SFOpenError,
          [FileName.GetAbsolutePath.ToNative]);
  end;
end;

destructor TMemTextFileStream.Destroy();
var
  FileStream: TBinaryFileStream;
  SaveMode: word;
begin
  // save changes in write mode (= not read-only mode)
  if ((fMode and 3) <> fmOpenRead) then
  begin
    if (fMode = fmCreate) then
      SaveMode := fmCreate
    else
      SaveMode := fmOpenWrite;
    FileStream := TBinaryFileStream.Create(fFilename, SaveMode);
    try
      fStream.SaveToStream(FileStream);
    finally
      FileStream.Free;
    end;
  end;

  fStream.Free;
  inherited;
end;

function TMemTextFileStream.GetSize: int64;
begin
  Result := fStream.Size;
end;

function TMemTextFileStream.Read(var Buffer; Count: longint): longint;
begin
  Result := fStream.Read(Buffer, Count);
end;

function TMemTextFileStream.Write(const Buffer; Count: longint): longint;
begin
  Result := fStream.Write(Buffer, Count);
end;

function TMemTextFileStream.Seek(Offset: longint; Origin: word): longint;
begin
  Result := fStream.Seek(Offset, Origin);
end;

function TMemTextFileStream.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  Result := fStream.Seek(Offset, Origin);
end;

function TMemTextFileStream.CopyMemString(StartPos: int64; EndPos: int64): RawByteString;
var
  LineLength: cardinal;
  Temp: RawByteString;
begin
  LineLength := EndPos - StartPos;
  if (LineLength > 0) then
  begin
    // set string length to line-length (+ zero-terminator)
    SetLength(Temp, LineLength);
    StrLCopy(PAnsiChar(Temp),
             @PAnsiChar(fStream.Memory)[StartPos],
             LineLength);
    Result := Temp;
  end
  else
  begin
    Result := '';
  end;
end;

function TMemTextFileStream.ReadString(): RawByteString;
var
  TextPtr: PAnsiChar;
  CurPos, StartPos, FileSize: int64;
begin
  TextPtr := PAnsiChar(fStream.Memory);
  CurPos := Position;
  FileSize := Size;
  StartPos := -1;

  while (CurPos < FileSize) do
  begin
    // check for whitespace (tab, lf, cr, space)
    if (TextPtr[CurPos] in [#9, #10, #13, ' ']) then
    begin
      // check if we are at the end of a string
      if (StartPos > -1) then
        Break;
    end
    else if (StartPos = -1) then // start of string found
    begin
      StartPos := CurPos;
    end;
    Inc(CurPos);
  end;

  if (StartPos = -1) then
    Result := ''
  else
  begin
    Result := CopyMemString(StartPos, CurPos);
    fStream.Position := CurPos;
  end;
end;

{*
 * Implementation of ReadLine(). We need separate versions for UTF8String
 * and AnsiString as "var" parameter types have to fit exactly.
 * To avoid a var-parameter here, the internal version the Line parameter is
 * used as return value.
 *}
function TMemTextFileStream.ReadLine(var Success: boolean): RawByteString;
var
  TextPtr: PAnsiChar;
  CurPos, FileSize: int64;
begin
  TextPtr := PAnsiChar(fStream.Memory);
  CurPos := fStream.Position;
  FileSize := Size;

  // check for EOF
  if (CurPos >= FileSize) then
  begin
    Result := '';
    Success := false;
    Exit;
  end;

  Success := true;

  while (CurPos < FileSize) do
  begin
    if (TextPtr[CurPos] in [#10, #13]) then
    begin
      // copy text line
      Result := CopyMemString(fStream.Position, CurPos);

      // handle windows style #13#10 (\r\n) newlines
      if (TextPtr[CurPos] = #13) and
         (CurPos+1 < FileSize) and
         (TextPtr[CurPos+1] = #10) then
      begin
        Inc(CurPos);
      end;

      // update stream pos
      fStream.Position := CurPos+1;

      Exit;
    end;
    Inc(CurPos);
  end;

  Result := CopyMemString(fStream.Position, CurPos);
  fStream.Position := FileSize;
end;

{ TUnicodeMemoryStream }

procedure TUnicodeMemoryStream.LoadFromFile(const FileName: IPath);
var
  Stream: TStream;
begin
  Stream := TBinaryFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TUnicodeMemoryStream.SaveToFile(const FileName: IPath);
var
  Stream: TStream;
begin
  Stream := TBinaryFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

{ TUnicodeMemIniFile }

constructor TUnicodeMemIniFile.Create(const FileName: IPath; UTF8Encoded: boolean);
var
  List: TStringList;
  Stream: TBinaryFileStream;
  BOMBuf: array[0..2] of AnsiChar;
begin
  inherited Create('');
  FFilename := FileName;
  FUTF8Encoded := UTF8Encoded;

  if FileName.Exists() then
  begin
    List := nil;
    Stream := nil;
    try
      List := TStringList.Create;
      Stream := TBinaryFileStream.Create(FileName, fmOpenRead);
      if (Stream.Read(BOMBuf[0], SizeOf(BOMBuf)) = 3) and
         (CompareMem(PChar(UTF8_BOM), @BomBuf, Length(UTF8_BOM))) then
      begin
        // truncate BOM
        FUTF8Encoded := true;
      end
      else
      begin
        // rewind file
        Stream.Seek(0, soBeginning);
      end;
      List.LoadFromStream(Stream);
      SetStrings(List);
    finally
      Stream.Free;
      List.Free;
    end;
  end;
end;

procedure TUnicodeMemIniFile.UpdateFile;
var
  List: TStringList;
  Stream: TBinaryFileStream;
begin
  List := nil;
  Stream := nil;
  try
    List := TStringList.Create;
    GetStrings(List);
    Stream := TBinaryFileStream.Create(FFileName, fmCreate);
    if UTF8Encoded then
      Stream.Write(UTF8_BOM, Length(UTF8_BOM));
    List.SaveToStream(Stream);
  finally
    List.Free;
    Stream.Free;
  end;
end;


var
  PATH_NONE_Singelton: IPath;

function PATH_NONE(): IPath;
begin
  Result := PATH_NONE_Singelton;
end;

initialization
  {$IFDEF HAVE_REFCNTBUG}
  GarbageList := TInterfaceList.Create();
  GarbageList.Capacity := GarbageMaxCount;
  {$ENDIF}
  PATH_NONE_Singelton := Path('');

finalization
  PATH_NONE_Singelton := nil;

end.
