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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/base/USongs.pas $
 * $Id: USongs.pas 3103 2014-11-22 23:21:19Z k-m_schindler $
 *}

unit USongs;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

{$IFDEF DARWIN}
  {$IFDEF DEBUG}
    {$DEFINE USE_PSEUDO_THREAD}
  {$ENDIF}
{$ENDIF}

uses
  SysUtils,
  Classes,
  SyncObjs,
  {$IFDEF MSWINDOWS}
    Windows,
  {$ELSE}
    {$IFNDEF DARWIN}
    syscall,
    {$ENDIF}
    baseunix,
    UnixType,
  {$ENDIF}
  UPlatform,
  ULog,
  UTexture,
  UCommon,
  {$IFDEF USE_PSEUDO_THREAD}
  PseudoThread,
  {$ENDIF}
  UPath,
  UPlaylist,
  USong,
  UIni,
  UCatCovers;

type
  TSongFilter = (
    fltAll,
    fltTitle,
    fltArtist,
    fltLanguage,
    fltEdition,
    fltGenre,
    fltYear,
    fltCreator,
    fltTags
  );

  TBPM = record
    BPM:       real;
    StartBeat: real;
  end;

  TScore = record
    Name:   UTF8String;
    Score:  integer;
    Length: string;
  end;

  TPathDynArray = array of IPath;
  TThreadDynArray = array of TThread;

  {$IFDEF USE_PSEUDO_THREAD}
  TSongs = class(TPseudoThread)
  {$ELSE}
  TSongs = class(TThread)
  {$ENDIF}
  private
    fNotify, fWatch:     longint;
    fParseSongDirectory: boolean;
    fProcessing:         boolean;
    fTotalSongsToLoad:   integer;
    fSongsLoaded:        integer;
    fLoadingBaseTemplate: UTF8String;
    fLoadingBaseText:    UTF8String;
    fLastProgressRedrawTicks: cardinal;
    fSongQueue:          TInterfaceList;
    fQueueLock:          TCriticalSection;
    fDirQueue:           TInterfaceList;
    fDirQueueLock:       TCriticalSection;
    fSongListLock:       TCriticalSection;
    fProgressLock:       TCriticalSection;
    fQueueEvent:         TEvent;
    fDiscoveryFinished:  boolean;
    procedure int_LoadSongList;
    procedure DoDirChanged(Sender: TObject);
    procedure UpdateLoadingProgress;
    procedure RedrawLoadingScreen;
    procedure ResetSongQueue;
    procedure EnqueueSongFile(const FilePath: IPath);
    function TryDequeueSongFile(out FilePath: IPath): boolean;
    function HasPendingWork: boolean;
    procedure WaitForWork(const TimeoutMs: cardinal = 50);
    procedure MarkDiscoveryFinished;
    procedure DiscoverFilesInDirectoryQueue(const Ext: IPath; CancelThread: TThread = nil);
    procedure IncrementTotalSongs(const Delta: integer);
    procedure IncrementSongsLoaded(const Delta: integer = 1);
    procedure WaitForWorkerThreads(var Workers: array of TThread);
  protected
    procedure Execute; override;
  public
    SongList: TList;            // array of songs

    Selected: integer;        // selected song index
    constructor Create();
    destructor  Destroy(); override;

    procedure LoadSongList;     // load all songs
  procedure FindFilesByExtension(const Dir: IPath; const Ext: IPath; Recursive: Boolean; var Files: TPathDynArray);
    procedure Sort(Order: TSortingType);
    property  Processing: boolean read fProcessing;
  end;

  TCatSongs = class
    Song:       array of TSong; // array of categories with songs
    SongSort:   array of TSong;

    Selected:   integer; // selected song index
    Order:      integer; // order type (0=title)
    CatNumShow: integer; // Category Number being seen
    CatCount:   integer; // Number of Categorys
    LastVisChecked: integer; // The real index of the last song that had its VisibilityIndex updated
    LastVisIndex:   integer; // The VisibilityIndex of the last song that had this value updated

    procedure SortSongs();
    procedure Refresh;                                      // refreshes arrays by recreating them from Songs array
    procedure ShowCategory(Index: integer);                 // expands all songs in category
    procedure HideCategory(Index: integer);                 // hides all songs in category
    procedure ClickCategoryButton(Index: integer);          // uses ShowCategory and HideCategory when needed
    procedure ShowCategoryList;                             // Hides all Songs And Show the List of all Categorys
    function FindNextVisible(SearchFrom: integer): integer; // Find Next visible Song
    function FindPreviousVisible(SearchFrom: integer): integer; // Find Previous visible Song
    function VisibleSongs: integer;                         // returns number of visible songs (for tabs)
    function VisibleIndex(Index: integer): integer;         // returns visible song index (skips invisible)

    function SetFilter(FilterStr: UTF8String; Filter: TSongFilter): cardinal;
  end;

var
  Songs:    TSongs;    // all songs
  CatSongs: TCatSongs; // categorized songs

const
  IN_ACCESS        = $00000001; //* File was accessed */
  IN_MODIFY        = $00000002; //* File was modified */
  IN_ATTRIB        = $00000004; //* Metadata changed */
  IN_CLOSE_WRITE   = $00000008; //* Writtable file was closed */
  IN_CLOSE_NOWRITE = $00000010; //* Unwrittable file closed */
  IN_OPEN          = $00000020; //* File was opened */
  IN_MOVED_FROM    = $00000040; //* File was moved from X */
  IN_MOVED_TO      = $00000080; //* File was moved to Y */
  IN_CREATE        = $00000100; //* Subfile was created */
  IN_DELETE        = $00000200; //* Subfile was deleted */
  IN_DELETE_SELF   = $00000400; //* Self was deleted */


implementation

uses
  StrUtils,
  SDL2,
  UCovers,
  UFiles,
  UGraphic,
  UDisplay,
  UMenuText,
  UMain,
  UPathUtils,
  UNote,
  UFilesystem,
  UUnicodeUtils;

type
  TSongDiscoveryThread = class(TThread)
  private
    FOwner: TSongs;
    FThreadID: integer;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TSongs; ThreadID: integer);
  end;

  TSongLoaderWorker = class(TThread)
  private
    FOwner: TSongs;
    procedure LoadSongFromFile(const FilePath: IPath);
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TSongs);
  end;

{ TSongDiscoveryThread }

constructor TSongDiscoveryThread.Create(AOwner: TSongs; ThreadID: integer);
begin
  inherited Create(false);
  FreeOnTerminate := false;
  FOwner := AOwner;
  FThreadID := ThreadID;

end;

procedure TSongDiscoveryThread.Execute;
var
  Extension: IPath;
begin
  Extension := Path('.txt');
  FOwner.DiscoverFilesInDirectoryQueue(Extension, Self);
end;

{ TSongLoaderWorker }

constructor TSongLoaderWorker.Create(AOwner: TSongs);
begin
  inherited Create(false);
  FreeOnTerminate := false;
  FOwner := AOwner;
end;

procedure TSongLoaderWorker.Execute;
var
  FilePath: IPath;
  exitReason: string;
begin
{$IFNDEF USE_PSEUDO_THREAD}
  while not Terminated do
{$ELSE}
  while true do
{$ENDIF}
  begin
    if not FOwner.TryDequeueSongFile(FilePath) then
    begin
      // Only exit if queue is empty AND discovery is finished
      if (not FOwner.HasPendingWork) and FOwner.fDiscoveryFinished then
      begin
        exitReason := 'queue empty and discovery finished';
        Break;
      end;
      FOwner.WaitForWork(50);
      Continue;
    end;

    try
      LoadSongFromFile(FilePath);
    except
      on E: Exception do
      begin
        if FilePath <> nil then
          Log.LogError('Unhandled exception while loading "' + FilePath.ToNative + '": ' + E.Message + '; Addr=' + IntToHex(LongWord(ExceptAddr), 8) + '; Trace=' + BackTraceStrFunc(ExceptAddr), 'TSongLoaderWorker')
        else
          Log.LogError('Unhandled exception while loading song: ' + E.Message + '; Addr=' + IntToHex(LongWord(ExceptAddr), 8) + '; Trace=' + BackTraceStrFunc(ExceptAddr), 'TSongLoaderWorker');
      end;
      on E: TObject do
        Log.LogError('Unhandled non-exception error while loading song.', 'TSongLoaderWorker');
    end;

    FOwner.IncrementSongsLoaded(1);
    FilePath := nil;
  end;
end;


procedure TSongLoaderWorker.LoadSongFromFile(const FilePath: IPath);
var
  Song: TSong;
  Loaded: boolean;
begin
  Song := TSong.Create(FilePath);
  Loaded := Song.Analyse;
  if Loaded then
  begin
    FOwner.fSongListLock.Enter;
    try
      FOwner.SongList.Add(Song);
    finally
      FOwner.fSongListLock.Leave;
    end;
  end
  else
    Log.LogError('AnalyseFile failed for "' + FilePath.ToNative + '".');
  if not Loaded then
    FreeAndNil(Song);
end;

constructor TSongs.Create();
begin
  // do not start thread BEFORE initialization (suspended = true)
  inherited Create(true);
  Self.FreeOnTerminate := true;

  SongList           := TList.Create();
  fSongQueue         := TInterfaceList.Create;
  fQueueLock         := TCriticalSection.Create;
  fDirQueue          := TInterfaceList.Create;
  fDirQueueLock      := TCriticalSection.Create;
  fSongListLock      := TCriticalSection.Create;
  fProgressLock      := TCriticalSection.Create;
  fQueueEvent        := TEvent.Create(nil, true, false, '');

  // until it is fixed, simply load the song-list
  int_LoadSongList();
end;

destructor TSongs.Destroy();
begin
  if assigned(fQueueEvent) then
    fQueueEvent.Free;
  FreeAndNil(fProgressLock);
  FreeAndNil(fSongListLock);
  FreeAndNil(fQueueLock);
  FreeAndNil(fSongQueue);
  FreeAndNil(SongList);

  inherited;
end;

procedure TSongs.ResetSongQueue;
begin
  fQueueLock.Enter;
  try
    if assigned(fSongQueue) then
      fSongQueue.Clear;
    fDiscoveryFinished := false;
    if assigned(fQueueEvent) then
      fQueueEvent.ResetEvent;
  finally
    fQueueLock.Leave;
  end;
  fDirQueueLock.Enter;
  try
    if assigned(fDirQueue) then
      fDirQueue.Clear;
  finally
    fDirQueueLock.Leave;
  end;
end;

procedure TSongs.EnqueueSongFile(const FilePath: IPath);
var
  FilePathStr: UTF8String;
  FilePathCopy: IPath;
begin
  if FilePath = nil then
    Exit;

  FilePathStr := FilePath.ToUTF8();
  FilePathCopy := Path(FilePathStr);
  fQueueLock.Enter;
  try
    fSongQueue.Add(FilePathCopy);
    if assigned(fQueueEvent) then
      fQueueEvent.SetEvent;
  finally
    fQueueLock.Leave;
  end;

  IncrementTotalSongs(1);
end;

function TSongs.TryDequeueSongFile(out FilePath: IPath): boolean;
var
  OrigPath: IPath;
  FilePathStr: UTF8String;
begin
  FilePath := nil;
  Result := false;

  fQueueLock.Enter;
  try
    if (fSongQueue <> nil) and (fSongQueue.Count > 0) then
    begin
      OrigPath := fSongQueue[0] as IPath;
      FilePathStr := OrigPath.ToUTF8();
      FilePath := Path(FilePathStr);
      fSongQueue.Delete(0);
      Result := true;
      if (fSongQueue.Count = 0) and assigned(fQueueEvent) then
        fQueueEvent.ResetEvent;
    end
  finally
    fQueueLock.Leave;
  end;
end;

function TSongs.HasPendingWork: boolean;
begin
  fQueueLock.Enter;
  try
    Result := (not fDiscoveryFinished) or
              ((fSongQueue <> nil) and (fSongQueue.Count > 0));
  finally
    fQueueLock.Leave;
  end;
end;

procedure TSongs.WaitForWork(const TimeoutMs: cardinal);
begin
  if assigned(fQueueEvent) then
    fQueueEvent.WaitFor(TimeoutMs)
  else
    TThread.Sleep(TimeoutMs);
end;

procedure TSongs.MarkDiscoveryFinished;
begin
  fQueueLock.Enter;
  try
    fDiscoveryFinished := true;
    if assigned(fQueueEvent) then
      fQueueEvent.SetEvent;
  finally
    fQueueLock.Leave;
  end;
end;

procedure TSongs.DiscoverFilesInDirectoryQueue(const Ext: IPath; CancelThread: TThread);
var
  Dir: IPath;
  Iter: IFileIterator;
  FileInfo: TFileInfo;
  FileName: IPath;
  FilePathStr: UTF8String;
  FilePathCopy: IPath;
begin
  while true do
  begin
    // Dequeue a directory
    fDirQueueLock.Enter;
    try
      if (fDirQueue = nil) or (fDirQueue.Count = 0) then
        Exit;
      Dir := fDirQueue[0] as IPath;
      fDirQueue.Delete(0);
    finally
      fDirQueueLock.Leave;
    end;

    if Dir = nil then
      Continue;

    Iter := FileSystem.FileFind(Dir.Append('*'), faAnyFile);
    while (Iter <> nil) and Iter.HasNext do
    begin
      FileInfo := Iter.Next;
      FileName := FileInfo.Name;
      if ((FileInfo.Attr and faDirectory) <> 0) then
      begin
        if (not FileName.Equals('.')) and (not FileName.Equals('..')) and (not FileName.Equals('')) then
        begin
          // Enqueue subdirectory
          fDirQueueLock.Enter;
          try
            fDirQueue.Add(Dir.Append(FileName));
          finally
            fDirQueueLock.Leave;
          end;
        end;
      end
      else if (Ext.Equals(FileName.GetExtension(), true) and not (FileName.ToUTF8()[1] = '.')) then
      begin
        FilePathStr := Dir.Append(FileName).ToUTF8();
        FilePathCopy := Path(FilePathStr);
        EnqueueSongFile(FilePathCopy);
      end;
    end;
  end;
end;

procedure TSongs.IncrementTotalSongs(const Delta: integer);
begin
  if Delta = 0 then
    Exit;

  if assigned(fProgressLock) then
  begin
    fProgressLock.Enter;
    try
      Inc(fTotalSongsToLoad, Delta);
    finally
      fProgressLock.Leave;
    end;
  end
  else
    Inc(fTotalSongsToLoad, Delta);
end;

procedure TSongs.IncrementSongsLoaded(const Delta: integer);
begin
  if Delta = 0 then
    Exit;

  if assigned(fProgressLock) then
  begin
    fProgressLock.Enter;
    try
      Inc(fSongsLoaded, Delta);
    finally
      fProgressLock.Leave;
    end;
  end
  else
    Inc(fSongsLoaded, Delta);
end;

procedure TSongs.WaitForWorkerThreads(var Workers: array of TThread);
var
  Remaining, I: integer;
begin
  Remaining := Length(Workers);
  while Remaining > 0 do
  begin
    for I := 0 to High(Workers) do
    begin
      if Workers[I] = nil then
        Continue;

      if Workers[I].Finished then
      begin
        Workers[I].WaitFor;
        FreeAndNil(Workers[I]);
        Dec(Remaining);
      end;
    end;

    if Remaining > 0 then
    begin
      UpdateLoadingProgress;
      TThread.Sleep(10);
    end;
  end;
end;

procedure TSongs.DoDirChanged(Sender: TObject);
begin
  LoadSongList();
end;

procedure TSongs.Execute();
var
  fChangeNotify: THandle;
begin
end;

procedure TSongs.int_LoadSongList;
var
  I: integer;
  WorkerCount: integer;
  WorkerThreads: TThreadDynArray;
  DiscoveryThreads: TThreadDynArray;
begin
  try
    fProcessing := true;

    Log.LogStatus('Searching For Songs', 'SongList');

    fProgressLock.Enter;
    try
      fSongsLoaded := 0;
      fTotalSongsToLoad := 0;
    finally
      fProgressLock.Leave;
    end;
    fLoadingBaseTemplate := '';
    fLoadingBaseText := '';
    fLastProgressRedrawTicks := 0;
    ResetSongQueue;
    // Initialize directory queue with root song paths
    fDirQueueLock.Enter;
    try
      if assigned(fDirQueue) then
        fDirQueue.Clear;
      if (SongPaths <> nil) and (SongPaths.Count > 0) then
        for I := 0 to SongPaths.Count - 1 do
          fDirQueue.Add(SongPaths[I]);
    finally
      fDirQueueLock.Leave;
    end;
    UpdateLoadingProgress;

    WorkerCount := Ini.GameThreads;
    if WorkerCount < 1 then
      WorkerCount := 1;

    SetLength(DiscoveryThreads, WorkerCount);
    for I := 0 to WorkerCount - 1 do
      DiscoveryThreads[I] := TSongDiscoveryThread.Create(Self, I);

    SetLength(WorkerThreads, WorkerCount);
    for I := 0 to WorkerCount - 1 do
      WorkerThreads[I] := TSongLoaderWorker.Create(Self);

    WaitForWorkerThreads(DiscoveryThreads);

    MarkDiscoveryFinished;

    WaitForWorkerThreads(WorkerThreads);

    UpdateLoadingProgress;

    if assigned(CatSongs) then
      CatSongs.Refresh;

    if assigned(CatCovers) then
      CatCovers.Load;

    //if assigned(Covers) then
    //  Covers.Load;

    if assigned(ScreenSong)  then
    begin
      ScreenSong.GenerateThumbnails();
      ScreenSong.OnShow; // refresh ScreenSong
    end;

  finally
    Log.LogStatus('Search Complete', 'SongList');

    fParseSongDirectory := false;
    fProcessing         := false;
  end;
end;


procedure TSongs.LoadSongList;
begin
  fParseSongDirectory := true;
  Resume();
end;

procedure TSongs.FindFilesByExtension(const Dir: IPath; const Ext: IPath; Recursive: Boolean; var Files: TPathDynArray);
var
  Iter: IFileIterator;
  FileInfo: TFileInfo;
  FileName: IPath;
begin
  // search for all files and directories
  Iter := FileSystem.FileFind(Dir.Append('*'), faAnyFile);
  while (Iter.HasNext) do
  begin
    // the debug statements in this function have exactly the same message length before it prints the path
    FileInfo := Iter.Next;
    FileName := FileInfo.Name;
    if ((FileInfo.Attr and faDirectory) <> 0) then
    begin
      if Recursive and (not FileName.Equals('.')) and (not FileName.Equals('..')) and (not FileName.Equals('')) then begin
        Log.LogDebug('Recursing: ' + Dir.Append(FileName).ToWide, 'TSongs.FindFilesByExtension');
        FindFilesByExtension(Dir.Append(FileName), Ext, true, Files);
      end;
    end
    else
    begin
      // do not load files which either have wrong extension or start with a point
      if (Ext.Equals(FileName.GetExtension(), true) and not (FileName.ToUTF8()[1] = '.')) then
      begin
        Log.LogDebug('Found file ' + Dir.Append(FileName).ToWide, 'TSongs.FindFilesByExtension');
        SetLength(Files, Length(Files)+1);
        Files[High(Files)] := Dir.Append(FileName);
      end;
    end;
  end;
end;

procedure TSongs.UpdateLoadingProgress;
var
  ProgressText: UTF8String;
  LoadingText: TText;
  NowTicks: cardinal;
  LoadedValue, TotalValue: integer;
begin
  if (ScreenLoading = nil) or (Length(ScreenLoading.Text) = 0) then
    Exit;

  NowTicks := SDL_GetTicks();
  if (NowTicks - fLastProgressRedrawTicks < 100) and (fLastProgressRedrawTicks <> 0) then
    Exit;

  if assigned(fProgressLock) then
  begin
    fProgressLock.Enter;
    try
      LoadedValue := fSongsLoaded;
      TotalValue := fTotalSongsToLoad;
    finally
      fProgressLock.Leave;
    end;
  end
  else
  begin
    LoadedValue := fSongsLoaded;
    TotalValue := fTotalSongsToLoad;
  end;

  LoadingText := ScreenLoading.Text[0];
  if LoadingText = nil then
    Exit;

  if fLoadingBaseTemplate = '' then
    fLoadingBaseTemplate := LoadingText.Text;

  if fLoadingBaseText = '' then
    fLoadingBaseText := fLoadingBaseTemplate;

  ProgressText := fLoadingBaseText + UTF8String(' [' + IntToStr(LoadedValue) + '/' + IntToStr(TotalValue) + ']');

  LoadingText.Text := ProgressText;

  RedrawLoadingScreen;
  fLastProgressRedrawTicks := NowTicks;
end;

procedure TSongs.RedrawLoadingScreen;
begin
  if (ScreenLoading = nil) or (Display = nil) then
    Exit;

  if Display.CurrentScreen <> @ScreenLoading then
    Exit;

  ScreenLoading.Draw;
  Display.Draw;
  SwapBuffers;
end;

(*
 * Comparison functions for sorting
 *)

function CompareByEdition(Song1, Song2: Pointer): integer;
begin
  Result := UTF8CompareText(TSong(Song1).Edition, TSong(Song2).Edition);
end;

function CompareByGenre(Song1, Song2: Pointer): integer;
begin
  Result := UTF8CompareText(TSong(Song1).Genre, TSong(Song2).Genre);
end;

function CompareByTitle(Song1, Song2: Pointer): integer;
begin
  Result := UTF8CompareText(TSong(Song1).Title, TSong(Song2).Title);
end;

function CompareByArtist(Song1, Song2: Pointer): integer;
begin
  Result := UTF8CompareText(TSong(Song1).Artist, TSong(Song2).Artist);
end;

function CompareByFolder(Song1, Song2: Pointer): integer;
begin
  Result := UTF8CompareText(TSong(Song1).Folder, TSong(Song2).Folder);
end;

function CompareByLanguage(Song1, Song2: Pointer): integer;
begin
  Result := UTF8CompareText(TSong(Song1).Language, TSong(Song2).Language);
end;

function CompareByYear(Song1, Song2: Pointer): integer;
begin
  if (TSong(Song1).Year > TSong(Song2).Year) then
    Result := 1
  else
    Result := 0;
end;

function CompareByYearReversed(Song1, Song2: Pointer): integer;
begin
  if (TSong(Song1).Year < TSong(Song2).Year) then
    Result := 1
  else
    Result := 0;
end;

procedure TSongs.Sort(Order: TSortingType);
var
  CompareFunc: TListSortCompare;
begin
  // FIXME: what is the difference between artist and artist2, etc.?
  case Order of
    sEdition: // by edition
      CompareFunc := CompareByEdition;
    sGenre: // by genre
      CompareFunc := CompareByGenre;
    sTitle: // by title
      CompareFunc := CompareByTitle;
    sArtist: // by artist
      CompareFunc := CompareByArtist;
    sFolder: // by folder
      CompareFunc := CompareByFolder;
    sArtist2: // by artist2
      CompareFunc := CompareByArtist;
    sLanguage: // by Language
      CompareFunc := CompareByLanguage;
    sYear: // by Year
      CompareFunc := CompareByYear;
    sYearReversed: //by year reversed
      CompareFunc := CompareByYearReversed;
    sDecade: // by Decade
      CompareFunc := CompareByYear;
    else
      Log.LogCritical('Unsupported comparison', 'TSongs.Sort');
      Exit; // suppress warning
  end; // case

  // Note: Do not use TList.Sort() as it uses QuickSort which is instable.
  // For example, if a list is sorted by title first and
  // by artist afterwards, the songs of an artist will not be sorted by title anymore.
  // The stable MergeSort guarantees to maintain this order. 
  MergeSort(SongList, CompareFunc);
end;

procedure TCatSongs.SortSongs();
begin
  case TSortingType(Ini.Sorting) of
    sEdition: begin
        Songs.Sort(sTitle);
        Songs.Sort(sArtist);
        Songs.Sort(sEdition);
      end;
    sGenre: begin
        Songs.Sort(sTitle);
        Songs.Sort(sArtist);
        Songs.Sort(sGenre);
      end;
    sLanguage: begin
        Songs.Sort(sTitle);
        Songs.Sort(sArtist);
        Songs.Sort(sLanguage);
      end;
    sFolder: begin
        Songs.Sort(sTitle);
        Songs.Sort(sArtist);
        Songs.Sort(sFolder);
      end;
    sTitle: begin
        Songs.Sort(sTitle);
      end;
    sArtist: begin
        Songs.Sort(sTitle);
        Songs.Sort(sArtist);
      end;
    sArtist2: begin
        Songs.Sort(sTitle);
        Songs.Sort(sArtist2);
      end;
    sYear: begin
        Songs.Sort(sTitle);
        Songs.Sort(sArtist);
        Songs.Sort(sYear);
      end;
    sYearReversed: begin
        Songs.Sort(sTitle);
        Songs.Sort(sArtist);
        Songs.Sort(sYearReversed);
    end;
    sDecade: begin
        Songs.Sort(sTitle);
        Songs.Sort(sArtist);
        Songs.Sort(sYear);
      end;
    sPlaylist: begin
        Songs.Sort(sTitle);
        Songs.Sort(sArtist);
      end;
  end; // case
end;

procedure TCatSongs.Refresh;
var
  SongIndex:   integer;
  CurSong:     TSong;
  CatIndex:    integer;    // index of current song in Song
  Letter:      UCS4Char;   // current letter for sorting using letter
  CurCategory: UTF8String; // current edition for sorting using edition, genre etc.
  Order:       integer;    // number used for ordernum
  LetterTmp:   UCS4Char;
  CatNumber:   integer;    // Number of Song in Category
  tmpCategory: UTF8String; //
  I, J:        integer;
  StringIndex: integer;
  MainArtist:  UTF8String;

  procedure AddCategoryButton(const CategoryName: UTF8String; const cover: IPath = nil);
  var
    PrevCatBtnIndex: integer;
  begin
    Inc(Order);
    CatIndex := Length(Song);
    SetLength(Song, CatIndex+1);
    Song[CatIndex]          := TSong.Create();
    Song[CatIndex].Artist   := '[' + CategoryName + ']';
    Song[CatIndex].Main     := true;
    Song[CatIndex].OrderTyp := 0;
    Song[CatIndex].OrderNum := Order;
    if (cover <> nil) then
       Song[CatIndex].Cover := cover
    else
       Song[CatIndex].Cover := CatCovers.GetCover(TSortingType(Ini.Sorting), CategoryName);
    Song[CatIndex].Visible  := true;

    // set number of songs in previous category
    PrevCatBtnIndex := CatIndex - CatNumber - 1;
    if ((PrevCatBtnIndex >= 0) and Song[PrevCatBtnIndex].Main) then
      Song[PrevCatBtnIndex].CatNumber := CatNumber;

    CatNumber := 0;
  end;

begin
  CatNumShow  := -1;

  SortSongs();

  CurCategory := '';
  Order       := 0;
  CatNumber   := 0;

  // Note: do NOT set Letter to ' ', otherwise no category-button will be
  // created for songs beginning with ' ' if songs of this category exist.
  // TODO: trim song-properties so ' ' will not occur as first chararcter.
  Letter      := 0;

  // clear song-list
  for SongIndex := 0 to Songs.SongList.Count - 1 do
  begin
    // free category buttons
    // Note: do NOT delete songs, they are just references to Songs.SongList entries
    CurSong := TSong(Songs.SongList[SongIndex]);
    if (CurSong.Main) then
      CurSong.Free;
  end;
  SetLength(Song, 0);

  for SongIndex := 0 to Songs.SongList.Count - 1 do
  begin
    CurSong := TSong(Songs.SongList[SongIndex]);
    // if tabs are on, add section buttons for each new section
    if (Ini.Tabs = 1) then
    begin
      case (TSortingType(Ini.Sorting)) of
        sEdition: begin
          if (CompareText(CurCategory, CurSong.Edition) <> 0) then
          begin
            CurCategory := CurSong.Edition;

            // add Category Button
            AddCategoryButton(CurCategory);
          end;
        end;

        sGenre: begin
          if (CompareText(CurCategory, CurSong.Genre) <> 0) then
          begin
            CurCategory := CurSong.Genre;
            // add Genre Button
            AddCategoryButton(CurCategory);
          end;
        end;

        sLanguage: begin
          if (CompareText(CurCategory, CurSong.Language) <> 0) then
          begin
            CurCategory := CurSong.Language;
            // add Language Button
            AddCategoryButton(CurCategory);
          end
        end;

        sTitle: begin
          if (Length(CurSong.Title) >= 1) then
          begin
            LetterTmp := UCS4UpperCase(UTF8ToUCS4String(CurSong.Title)[0]);
            { all numbers and some punctuation chars are put into a
              category named '#'
              we can't put the other punctuation chars into this category
              because they are not in order, so there will be two different
              categories named '#' }
            if (LetterTmp in [Ord('!') .. Ord('?')]) then
              LetterTmp := Ord('#')
            else
              LetterTmp := UCS4UpperCase(LetterTmp);
            if (Letter <> LetterTmp) then
            begin
              Letter := LetterTmp;
              // add a letter Category Button
              AddCategoryButton(UCS4ToUTF8String(Letter));
            end;
          end;
        end;

        sArtist: begin
          if (Length(CurSong.Artist) >= 1) then
          begin
            LetterTmp := UCS4UpperCase(UTF8ToUCS4String(CurSong.Artist)[0]);
            { all numbers and some punctuation chars are put into a
              category named '#'
              we can't put the other punctuation chars into this category
              because they are not in order, so there will be two different
              categories named '#' }
            if (LetterTmp in [Ord('!') .. Ord('?')]) then
              LetterTmp := Ord('#')
            else
              LetterTmp := UCS4UpperCase(LetterTmp);

            if (Letter <> LetterTmp) then
            begin
              Letter := LetterTmp;
              // add a letter Category Button
              AddCategoryButton(UCS4ToUTF8String(Letter));
            end;
          end;
        end;

        sFolder: begin
          if (UTF8CompareText(CurCategory, CurSong.Folder) <> 0) then
          begin
            CurCategory := CurSong.Folder;
            // add folder tab
            AddCategoryButton(CurCategory);
          end;
        end;

        sArtist2: begin
          { this new sorting puts all songs by the same artist into
            a single category }
          //
          if (UTF8ContainsText(CurSong.Artist, ' feat.')) then
          begin
            StringIndex := UTF8Pos(' feat', CurSong.Artist);
            MainArtist := TrimRight(UTF8Copy(CurSong.Artist, 1, StringIndex-1));
          end
          else
            MainArtist := CurSong.Artist;
          //
          if (UTF8CompareText(CurCategory, MainArtist) <> 0) then
          begin
            CurCategory := MainArtist;
            // add folder tab with first song cover
            AddCategoryButton(CurCategory, CurSong.Path.Append(CurSong.Cover));
          end;
        end;

        sYear: begin
           if (CurSong.Year <> 0) then
             tmpCategory := IntToStr(CurSong.Year)
           else
             tmpCategory := 'Unknown';

           if (tmpCategory <> CurCategory) then
           begin
             CurCategory := tmpCategory;

             // add Category Button
             AddCategoryButton(CurCategory);
           end;
         end;

        sYearReversed: begin
           if (CurSong.Year <> 0) then
             tmpCategory := IntToStr(CurSong.Year)
           else
             tmpCategory := 'Unknown';

           if (tmpCategory <> CurCategory) then
           begin
             CurCategory := tmpCategory;

             // add Category Button
             AddCategoryButton(CurCategory);
           end;
         end;

        sDecade: begin
           if (CurSong.Year <> 0) then
             tmpCategory := IntToStr(Trunc(CurSong.Year/10)*10) + '-' + IntToStr(Trunc(CurSong.Year/10)*10+9)
           else
             tmpCategory := 'Unknown';

           if (tmpCategory <> CurCategory) then
           begin
             CurCategory := tmpCategory;

             // add Category Button
             AddCategoryButton(CurCategory);
           end;
        end;
      end; // case (Ini.Sorting)
    end; // if (Ini.Tabs = 1)

    CatIndex := Length(Song);
    SetLength(Song, CatIndex+1);

    Inc(CatNumber); // increase number of songs in category

    // copy reference to current song
    Song[CatIndex] := CurSong;

    // set song's category info
    CurSong.OrderNum := Order; // assigns category
    CurSong.CatNumber := CatNumber;

    if (Ini.Tabs = 0) then
    begin
      CurSong.Visible := true;
    end
    else if (Ini.Tabs = 1) then
    begin
      CurSong.Visible := false;
    end;
{
    if (Ini.Tabs = 1) and (Order = 1) then
    begin
      //open first tab
      CurSong.Visible := true;
    end;
    CurSong.Visible := true;
}
  end;
  LastVisChecked := 0;
  LastVisIndex := 0;

  // set CatNumber of last category
  if (Ini.TabsAtStartup = 1) and (High(Song) >= 1) then
  begin
    // set number of songs in previous category
    SongIndex := CatIndex - CatNumber;
    if ((SongIndex >= 0) and Song[SongIndex].Main) then
      Song[SongIndex].CatNumber := CatNumber;
  end;

  // update number of categories
  CatCount := Order;
end;

procedure TCatSongs.ShowCategory(Index: integer);
var
  S: integer; // song
begin
  CatNumShow := Index;
  for S := 0 to high(CatSongs.Song) do
  begin
{
    if (CatSongs.Song[S].OrderNum = Index) and (not CatSongs.Song[S].Main) then
      CatSongs.Song[S].Visible := true
    else
      CatSongs.Song[S].Visible := false;
}
//  KMS: This should be the same, but who knows :-)
    CatSongs.Song[S].Visible := ((CatSongs.Song[S].OrderNum = Index) and (not CatSongs.Song[S].Main));
  end;
  LastVisChecked := 0;
  LastVisIndex := 0;
end;

procedure TCatSongs.HideCategory(Index: integer); // hides all songs in category
var
  S: integer; // song
begin
  for S := 0 to high(CatSongs.Song) do
  begin
    if not CatSongs.Song[S].Main then
      CatSongs.Song[S].Visible := false // hides all at now
  end;
  LastVisChecked := 0;
  LastVisIndex := 0;
end;

procedure TCatSongs.ClickCategoryButton(Index: integer);
var
  Num: integer;
begin
  Num := CatSongs.Song[Index].OrderNum;
  if Num <> CatNumShow then
  begin
    ShowCategory(Num);
  end
  else
  begin
    ShowCategoryList;
  end;
end;

//Hide Categorys when in Category Hack
procedure TCatSongs.ShowCategoryList;
var
  S: integer;
begin
  // Hide All Songs Show All Cats
  for S := 0 to high(CatSongs.Song) do
    CatSongs.Song[S].Visible := CatSongs.Song[S].Main;
  CatSongs.Selected := CatNumShow; //Show last shown Category
  CatNumShow := -1;
  LastVisChecked := 0;
  LastVisIndex := 0;
end;
//Hide Categorys when in Category Hack End

// Wrong song selected when tabs on bug
function TCatSongs.FindNextVisible(SearchFrom:integer): integer;// Find next Visible Song
var
  I: integer;
begin
  Result := -1;
  I := SearchFrom;
  while (Result = -1) do
  begin
    Inc (I);

    if (I > High(CatSongs.Song)) then
      I := Low(CatSongs.Song);

    if (I = SearchFrom) then // Make One Round and no song found->quit
      Break;

    if (CatSongs.Song[I].Visible) then
      Result := I;
  end;
end;

function TCatSongs.FindPreviousVisible(SearchFrom:integer): integer;// Find previous Visible Song
var
  I: integer;
begin
  Result := -1;
  I := SearchFrom;
  while (Result = -1) do
  begin
    Dec (I);

    if (I < Low(CatSongs.Song)) then
      I := High(CatSongs.Song);

    if (I = SearchFrom) then // Make One Round and no song found->quit
      Break;

    if (CatSongs.Song[I].Visible) then
      Result := I;
  end;
end;

// Wrong song selected when tabs on bug End

(**
 * Returns the number of visible songs.
 *)
function TCatSongs.VisibleSongs: integer;
begin
  Result := VisibleIndex(High(Song));
  if Song[High(Song)].Visible then
    Inc(Result);
end;

(**
 * Returns the index of a song in the subset of all visible songs.
 * If all songs are visible, the result will be equal to the Index parameter. 
 *)
function TCatSongs.VisibleIndex(Index: integer): integer;
begin
  while LastVisChecked < Index do
  begin
    if Song[LastVisChecked].Visible then
      Inc(LastVisIndex);
    Inc(LastVisChecked);
    Song[LastVisChecked].VisibleIndex := LastVisIndex;
  end;
  Result := Song[Index].VisibleIndex;
end;

function TCatSongs.SetFilter(FilterStr: UTF8String; Filter: TSongFilter): cardinal;
var
  I, J:      integer;
  TmpString: UTF8String;
  WordArray: array of UTF8String;
begin

  FilterStr := Trim(LowerCase(TransliterateToASCII(FilterStr)));

  if (FilterStr <> '') then
  begin
    Result := 0;

    // initialize word array
    SetLength(WordArray, 1);

    // Copy words to SearchStr
    I := Pos(' ', FilterStr);
    while (I <> 0) do
    begin
      WordArray[High(WordArray)] := Copy(FilterStr, 1, I-1);
      SetLength(WordArray, Length(WordArray) + 1);

      FilterStr := TrimLeft(Copy(FilterStr, I+1, Length(FilterStr)-I));
      I := Pos(' ', FilterStr);
    end;

    // Copy last word
    WordArray[High(WordArray)] := FilterStr;

    for I := 0 to High(Song) do
    begin
      if not Song[i].Main then
      begin
        case Filter of
          fltAll:
            TmpString := Song[I].ArtistASCII + ' ' + Song[i].TitleASCII + ' ' + Song[i].LanguageASCII + ' ' + Song[i].EditionASCII + ' ' + Song[i].GenreASCII + ' ' + IntToStr(Song[i].Year) + ' ' + Song[i].CreatorASCII + ' ' + Song[i].TagsASCII; //+ ' ' + Song[i].Folder;
          fltTitle:
            TmpString := Song[I].TitleASCII;
          fltArtist:
            TmpString := Song[I].ArtistASCII;
          fltLanguage:
            TmpString := Song[I].LanguageASCII;
          fltEdition:
            TmpString := Song[I].EditionASCII;
          fltGenre:
            TmpString := Song[I].GenreASCII;
          fltYear:
            TmpString := IntToStr(Song[I].Year);
          fltCreator:
            TmpString := Song[I].CreatorASCII;
          fltTags:
            TmpString := Song[i].TagsASCII;
        end;
        Song[i].Visible := true;
        // Look for every searched word
        for J := 0 to High(WordArray) do
        begin
          Song[i].Visible := Song[i].Visible and
                             UTF8ContainsStr(TmpString, WordArray[J])
        end;
        if Song[i].Visible then
          Inc(Result);
      end
      else
        Song[i].Visible := false;
    end;
    CatNumShow := -2;
  end
  else
  begin
    for i := 0 to High(Song) do
    begin
      Song[i].Visible := (Ini.Tabs = 1) = Song[i].Main;
      CatNumShow := -1;
    end;
    Result := 0;
  end;
  LastVisChecked := 0;
  LastVisIndex := 0;
end;

// -----------------------------------------------------------------------------

end.
