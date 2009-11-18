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
  {$IFDEF MSWINDOWS}
    Windows,
    DirWatch,
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
  USong,
  UCatCovers;

type
  TSongFilter = (
    fltAll,
    fltTitle,
    fltArtist
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

  {$IFDEF USE_PSEUDO_THREAD}
  TSongs = class(TPseudoThread)
  {$ELSE}
  TSongs = class(TThread)
  {$ENDIF}
  private
    fNotify, fWatch:     longint;
    fParseSongDirectory: boolean;
    fProcessing:         boolean;
    {$ifdef MSWINDOWS}
    fDirWatch:           TDirectoryWatch;
    {$endif}
    procedure int_LoadSongList;
    procedure DoDirChanged(Sender: TObject);
  protected
    procedure Execute; override;
  public
    SongList: TList;          // array of songs
    Selected: integer;        // selected song index
    constructor Create();
    destructor  Destroy(); override;


    procedure LoadSongList;     // load all songs
    procedure FindFilesByExtension(const Dir: IPath; const Ext: IPath; Recursive: Boolean; var Files: TPathDynArray);
    procedure BrowseDir(Dir: IPath); // should return number of songs in the future
    procedure BrowseTXTFiles(Dir: IPath);
    procedure BrowseXMLFiles(Dir: IPath);
    procedure Sort(Order: integer);
    property  Processing: boolean read fProcessing;
  end;


  TCatSongs = class
    Song:       array of TSong; // array of categories with songs
    Selected:   integer; // selected song index
    Order:      integer; // order type (0=title)
    CatNumShow: integer; // Category Number being seen
    CatCount:   integer; // Number of Categorys

    procedure SortSongs();
    procedure Refresh;                                      // refreshes arrays by recreating them from Songs array
    procedure ShowCategory(Index: integer);                 // expands all songs in category
    procedure HideCategory(Index: integer);                 // hides all songs in category
    procedure ClickCategoryButton(Index: integer);          // uses ShowCategory and HideCategory when needed
    procedure ShowCategoryList;                             // Hides all Songs And Show the List of all Categorys
    function FindNextVisible(SearchFrom: integer): integer; // Find Next visible Song
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
  UCovers,
  UFiles,
  UGraphic,
  UMain,
  UIni,
  UPathUtils,
  UNote,
  UFilesystem,
  UUnicodeUtils;

constructor TSongs.Create();
begin
  // do not start thread BEFORE initialization (suspended = true)
  inherited Create(true);
  Self.FreeOnTerminate := true;

  SongList := TList.Create();

  // FIXME: threaded loading does not work this way.
  // It will just cause crashes but nothing else at the moment.
(*
  {$ifdef MSWINDOWS}
    fDirWatch := TDirectoryWatch.create(nil);
    fDirWatch.OnChange     := DoDirChanged;
    fDirWatch.Directory    := SongPath;
    fDirWatch.WatchSubDirs := true;
    fDirWatch.active       := true;
  {$ENDIF}

  // now we can start the thread
  Resume();
*)

  // until it is fixed, simply load the song-list
  int_LoadSongList();
end;

destructor TSongs.Destroy();
begin
  FreeAndNil(SongList);
  inherited;
end;

procedure TSongs.DoDirChanged(Sender: TObject);
begin
  LoadSongList();
end;

procedure TSongs.Execute();
var
  fChangeNotify: THandle;
begin
{$IFDEF USE_PSEUDO_THREAD}
  int_LoadSongList();
{$ELSE}
  fParseSongDirectory := true;

  while not terminated do
  begin

    if fParseSongDirectory then
    begin
      Log.LogStatus('Calling int_LoadSongList', 'TSongs.Execute');
      int_LoadSongList();
    end;

    Suspend();
  end;
{$ENDIF}
end;

procedure TSongs.int_LoadSongList;
var
  I: integer;
begin
  try
    fProcessing := true;

    Log.LogStatus('Searching For Songs', 'SongList');

    // browse directories
    for I := 0 to SongPaths.Count-1 do
      BrowseDir(SongPaths[I] as IPath);

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

procedure TSongs.BrowseDir(Dir: IPath);
begin
  BrowseTXTFiles(Dir);
  BrowseXMLFiles(Dir);
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
    FileInfo := Iter.Next;
    FileName := FileInfo.Name;
    if ((FileInfo.Attr and faDirectory) <> 0) then
    begin
      if Recursive and (not FileName.Equals('.')) and (not FileName.Equals('..')) then
        FindFilesByExtension(Dir.Append(FileName), Ext, true, Files);
    end
    else
    begin
      if (Ext.Equals(FileName.GetExtension(), true)) then
      begin
        SetLength(Files, Length(Files)+1);
        Files[High(Files)] := Dir.Append(FileName);
      end;
    end;
  end;
end;

procedure TSongs.BrowseTXTFiles(Dir: IPath);
var
  I: integer;
  Files: TPathDynArray;
  Song: TSong;
  Extension: IPath;
begin
  SetLength(Files, 0);
  Extension := Path('.txt');
  FindFilesByExtension(Dir, Extension, true, Files);

  for I := 0 to High(Files) do
  begin
    Song := TSong.Create(Files[I]);

    if Song.Analyse then
      SongList.Add(Song)
    else
    begin
      Log.LogError('AnalyseFile failed for "' + Files[I].ToNative + '".');
      FreeAndNil(Song);
    end;
  end;

  SetLength(Files, 0);
end;

procedure TSongs.BrowseXMLFiles(Dir: IPath);
var
  I: integer;
  Files: TPathDynArray;
  Song: TSong;
  Extension: IPath;
begin
  SetLength(Files, 0);
  Extension := Path('.xml');
  FindFilesByExtension(Dir, Extension, true, Files);

  for I := 0 to High(Files) do
  begin
    Song := TSong.Create(Files[I]);

    if Song.AnalyseXML then
      SongList.Add(Song)
    else
    begin
      Log.LogError('AnalyseFile failed for "' + Files[I].ToNative + '".');
      FreeAndNil(Song);
    end;
  end;

  SetLength(Files, 0);
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

procedure TSongs.Sort(Order: integer);
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
  case Ini.Sorting of
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

  procedure AddCategoryButton(const CategoryName: UTF8String);
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
    Song[CatIndex].Cover    := CatCovers.GetCover(Ini.Sorting, CategoryName);
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
      case (Ini.Sorting) of
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
          if (UTF8CompareText(CurCategory, CurSong.Artist) <> 0) then
          begin
            CurCategory := CurSong.Artist;
            // add folder tab
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
      CurSong.Visible := true
    else if (Ini.Tabs = 1) then
      CurSong.Visible := false;
{
    if (Ini.Tabs = 1) and (Order = 1) then
    begin
      //open first tab
      CurSong.Visible := true;
    end;
    CurSong.Visible := true;
}
  end;

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
// Wrong song selected when tabs on bug End

(**
 * Returns the number of visible songs.
 *)
function TCatSongs.VisibleSongs: integer;
var
  SongIndex: integer;
begin
  Result := 0;
  for SongIndex := 0 to High(CatSongs.Song) do
  begin
    if (CatSongs.Song[SongIndex].Visible) then
      Inc(Result);
  end;
end;

(**
 * Returns the index of a song in the subset of all visible songs.
 * If all songs are visible, the result will be equal to the Index parameter. 
 *)
function TCatSongs.VisibleIndex(Index: integer): integer;
var
  SongIndex: integer;
begin
  Result := 0;
  for SongIndex := 0 to Index - 1 do
  begin
    if (CatSongs.Song[SongIndex].Visible) then
      Inc(Result);
  end;
end;

function TCatSongs.SetFilter(FilterStr: UTF8String; Filter: TSongFilter): cardinal;
var
  I, J:      integer;
  TmpString: UTF8String;
  WordArray: array of UTF8String;
begin
  FilterStr := Trim(FilterStr);
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
            TmpString := Song[I].Artist + ' ' + Song[i].Title + ' ' + Song[i].Folder;
          fltTitle:
            TmpString := Song[I].Title;
          fltArtist:
            TmpString := Song[I].Artist;
        end;
        Song[i].Visible := true;
        // Look for every searched word
        for J := 0 to High(WordArray) do
        begin
          Song[i].Visible := Song[i].Visible and
                             UTF8ContainsText(TmpString, WordArray[J])
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
end;

// -----------------------------------------------------------------------------

end.
