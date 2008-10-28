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
  SysUtils,
  Classes,
  UPlatform,
  ULog,
  UTexture,
  UCommon,
  {$IFDEF DARWIN}
    cthreads,
  {$ENDIF}
  {$IFDEF USE_PSEUDO_THREAD}
    PseudoThread,
  {$ENDIF}
  USong,
  UCatCovers;

type

  TBPM = record
    BPM:        real;
    StartBeat:  real;
  end;

  TScore = record
    Name:       widestring;
    Score:      integer;
    Length:     string;
  end;

  {$IFDEF USE_PSEUDO_THREAD}
  TSongs = class( TPseudoThread )
  {$ELSE}
  TSongs = class( TThread )
  {$ENDIF}
  private
    fNotify, fWatch     : longint;
    fParseSongDirectory : boolean;
    fProcessing         : boolean;
    {$ifdef MSWINDOWS}
    fDirWatch           : TDirectoryWatch;
    {$endif}
    procedure int_LoadSongList;
    procedure DoDirChanged(Sender: TObject);
  protected
    procedure Execute; override;
  public
    SongList  : TList;          // array of songs
    Selected  : integer;        // selected song index
    constructor Create();
    destructor  Destroy(); override;


    procedure LoadSongList;     // load all songs
    procedure BrowseDir(Dir: widestring); // should return number of songs in the future
    procedure BrowseTXTFiles(Dir: widestring);
    procedure BrowseXMLFiles(Dir: widestring);
    procedure Sort(Order: integer);
    function  FindSongFile(Dir, Mask: widestring): widestring;
    property  Processing : boolean read fProcessing;
  end;


  TCatSongs = class
    Song:       array of TSong; // array of categories with songs
    Selected:   integer; // selected song index
    Order:      integer; // order type (0=title)
    CatNumShow: integer; // Category Number being seen
    CatCount:   integer; //Number of Categorys

    procedure SortSongs();
    procedure Refresh; // refreshes arrays by recreating them from Songs array
    procedure ShowCategory(Index: integer); // expands all songs in category
    procedure HideCategory(Index: integer); // hides all songs in category
    procedure ClickCategoryButton(Index: integer); // uses ShowCategory and HideCategory when needed
    procedure ShowCategoryList; //Hides all Songs And Show the List of all Categorys
    function FindNextVisible(SearchFrom:integer): integer; //Find Next visible Song
    function VisibleSongs: integer; // returns number of visible songs (for tabs)
    function VisibleIndex(Index: integer): integer; // returns visible song index (skips invisible)

    function SetFilter(FilterStr: string; const fType: Byte): Cardinal;
  end;

var
  Songs:      TSongs;    // all songs
  CatSongs:   TCatSongs; // categorized songs

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

uses StrUtils,
     UGraphic,
     UCovers,
     UFiles,
     UMain,
     UIni;

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
  FreeAndNil( SongList );
  inherited;
end;

procedure TSongs.DoDirChanged(Sender: TObject);
begin
  LoadSongList();
end;

procedure TSongs.Execute();
var
  fChangeNotify : THandle;
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
      BrowseDir(SongPaths[I]);

    if assigned( CatSongs ) then
      CatSongs.Refresh;

    if assigned( CatCovers ) then
      CatCovers.Load;

    //if assigned( Covers ) then
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

procedure TSongs.BrowseDir(Dir: widestring);
begin
 BrowseTXTFiles(Dir);
 BrowseXMLFiles(Dir);
end;

procedure TSongs.BrowseTXTFiles(Dir: widestring);
var
  i       : integer;
  Files   : TDirectoryEntryArray;
  lSong   : TSong;
begin

  Files    := Platform.DirectoryFindFiles( Dir, '.txt', true);

  for i := 0 to Length(Files)-1 do
  begin
    if Files[i].IsDirectory then
    begin
      BrowseTXTFiles( Dir + Files[i].Name + PathDelim );  //Recursive Call
    end
    else
    begin
      lSong := TSong.create( Dir + Files[i].Name );

      if lSong.Analyse then
        SongList.add( lSong )
      else
      begin
        Log.LogError('AnalyseFile failed for "' + Files[i].Name + '".');
        freeandnil( lSong );
      end;

    end;
  end;
  SetLength( Files, 0);

end;

procedure TSongs.BrowseXMLFiles(Dir: widestring);
var
  i       : integer;
  Files   : TDirectoryEntryArray;
  lSong   : TSong;
begin

  Files := Platform.DirectoryFindFiles( Dir, '.xml', true);

  for i := 0 to Length(Files)-1 do
  begin
    if Files[i].IsDirectory then
    begin
      BrowseXMLFiles( Dir + Files[i].Name + PathDelim ); //Recursive Call
    end
    else
    begin
      lSong := TSong.create( Dir + Files[i].Name );

      if lSong.AnalyseXML then
	SongList.add( lSong )
      else
      begin
	Log.LogError('AnalyseFile failed for "' + Files[i].Name + '".');
	freeandnil( lSong );
      end;

    end;
  end;
  SetLength( Files, 0);

end;

(*
 * Comparison functions for sorting
 *)

function CompareByEdition(Song1, Song2: Pointer): integer;
begin
  Result := CompareText(TSong(Song1).Edition, TSong(Song2).Edition);
end;

function CompareByGenre(Song1, Song2: Pointer): integer;
begin
  Result := CompareText(TSong(Song1).Genre, TSong(Song2).Genre);
end;

function CompareByTitle(Song1, Song2: Pointer): integer;
begin
  Result := CompareText(TSong(Song1).Title, TSong(Song2).Title);
end;

function CompareByArtist(Song1, Song2: Pointer): integer;
begin
  Result := CompareText(TSong(Song1).Artist, TSong(Song2).Artist);
end;

function CompareByFolder(Song1, Song2: Pointer): integer;
begin
  Result := CompareText(TSong(Song1).Folder, TSong(Song2).Folder);
end;

function CompareByLanguage(Song1, Song2: Pointer): integer;
begin
  Result := CompareText(TSong(Song1).Language, TSong(Song2).Language);
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
    sTitle2: // by title2
      CompareFunc := CompareByTitle;
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

function TSongs.FindSongFile(Dir, Mask: widestring): widestring;
var
  SR:     TSearchRec;   // for parsing song directory
begin
  Result := '';
  if FindFirst(Dir + Mask, faDirectory, SR) = 0 then
  begin
    Result := SR.Name;
  end; // if
  FindClose(SR);
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
    sTitle2: begin
        Songs.Sort(sArtist2);
        Songs.Sort(sTitle2);
      end;
    sArtist2: begin
        Songs.Sort(sTitle2);
        Songs.Sort(sArtist2);
      end;
  end; // case
end;

procedure TCatSongs.Refresh;
var
  SongIndex:   integer;
  CurSong:     TSong;
  CatIndex:    integer; // index of current song in Song
  Letter:      char;    // current letter for sorting using letter
  CurCategory: string;  // current edition for sorting using edition, genre etc.
  Order:       integer; // number used for ordernum
  LetterTmp:   char;
  CatNumber:   integer; // Number of Song in Category

  procedure AddCategoryButton(const CategoryName: string);
  var
    PrevCatBtnIndex: integer;
  begin
    Inc(Order);
    CatIndex := Length(Song);
    SetLength(Song, CatIndex+1);
    Song[CatIndex] := TSong.Create();
    Song[CatIndex].Artist := '[' + CategoryName + ']';
    Song[CatIndex].Main := true;
    Song[CatIndex].OrderTyp := 0;
    Song[CatIndex].OrderNum := Order;
    Song[CatIndex].Cover := CatCovers.GetCover(Ini.Sorting, CategoryName);
    Song[CatIndex].Visible := true;

    // set number of songs in previous category
    PrevCatBtnIndex := CatIndex - CatNumber - 1;
    if ((PrevCatBtnIndex >= 0) and Song[PrevCatBtnIndex].Main) then
      Song[PrevCatBtnIndex].CatNumber := CatNumber;

    CatNumber := 0;
 end;

begin
  CatNumShow := -1;

  SortSongs();

  CurCategory := '';
  Order := 0;
  CatNumber := 0;

  // Note: do NOT set Letter to ' ', otherwise no category-button will be
  // created for songs beginning with ' ' if songs of this category exist.
  // TODO: trim song-properties so ' ' will not occur as first chararcter. 
  Letter := #0;

  // clear song-list
  for SongIndex := 0 to Songs.SongList.Count-1 do
  begin
    // free category buttons
    // Note: do NOT delete songs, they are just references to Songs.SongList entries
    CurSong := TSong(Songs.SongList[SongIndex]);
    if (CurSong.Main) then
      CurSong.Free;
  end;
  SetLength(Song, 0);

  for SongIndex := 0 to Songs.SongList.Count-1 do
  begin
    CurSong := TSong(Songs.SongList[SongIndex]);
    // if tabs are on, add section buttons for each new section
    if (Ini.Tabs = 1) then
    begin
      if (Ini.Sorting = sEdition) and
         (CompareText(CurCategory, CurSong.Edition) <> 0) then
      begin
        CurCategory := CurSong.Edition;

        // TODO: remove this block if it is not needed anymore
        {
        if CurSection = 'Singstar Part 2' then CoverName := 'Singstar';
        if CurSection = 'Singstar German' then CoverName := 'Singstar';
        if CurSection = 'Singstar Spanish' then CoverName := 'Singstar';
        if CurSection = 'Singstar Italian' then CoverName := 'Singstar';
        if CurSection = 'Singstar French' then CoverName := 'Singstar';
        if CurSection = 'Singstar 80s Polish' then CoverName := 'Singstar 80s';
        }

        // add Category Button
        AddCategoryButton(CurCategory);
      end

      else if (Ini.Sorting = sGenre) and
              (CompareText(CurCategory, CurSong.Genre) <> 0) then
      begin
        CurCategory := CurSong.Genre;
        // add Genre Button
        AddCategoryButton(CurCategory);
      end

      else if (Ini.Sorting = sLanguage) and
              (CompareText(CurCategory, CurSong.Language) <> 0) then
      begin
        CurCategory := CurSong.Language;
        // add Language Button
        AddCategoryButton(CurCategory);
      end

      else if (Ini.Sorting = sTitle) and
              (Length(CurSong.Title) >= 1) and
              (Letter <> UpperCase(CurSong.Title)[1]) then
      begin
        Letter := Uppercase(CurSong.Title)[1];
        // add a letter Category Button
        AddCategoryButton(Letter);
      end

      else if (Ini.Sorting = sArtist) and
              (Length(CurSong.Artist) >= 1) and
              (Letter <> UpperCase(CurSong.Artist)[1]) then
      begin
        Letter := UpperCase(CurSong.Artist)[1];
        // add a letter Category Button
        AddCategoryButton(Letter);
      end

      else if (Ini.Sorting = sFolder) and
              (CompareText(CurCategory, CurSong.Folder) <> 0) then
      begin
        CurCategory := CurSong.Folder;
        // add folder tab
        AddCategoryButton(CurCategory);
      end

      else if (Ini.Sorting = sTitle2) and
              (Length(CurSong.Title) >= 1) then
      begin
        // pack all numbers into a category named '#'
        if (CurSong.Title[1] >= '0') and (CurSong.Title[1] <= '9') then
          LetterTmp := '#'
        else
          LetterTmp := UpperCase(CurSong.Title)[1];

        if (Letter <> LetterTmp) then
        begin
          Letter := LetterTmp;
          // add a letter Category Button
          AddCategoryButton(Letter);
        end;
      end

      else if (Ini.Sorting = sArtist2) and
              (Length(CurSong.Artist)>=1) then
      begin
        // pack all numbers into a category named '#'
        if (CurSong.Artist[1] >= '0') and (CurSong.Artist[1] <= '9') then
          LetterTmp := '#'
        else
          LetterTmp := UpperCase(CurSong.Artist)[1];

        if (Letter <> LetterTmp) then
        begin
          Letter := LetterTmp;
          // add a letter Category Button
          AddCategoryButton(Letter);
        end;
      end;
    end;

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
  if (Ini.Tabs_at_startup = 1) and (High(Song) >= 1) then
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
  S:    integer; // song
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
    CatSongs.Song[S].Visible := ( (CatSongs.Song[S].OrderNum = Index) and (not CatSongs.Song[S].Main) );
  end;
end;

procedure TCatSongs.HideCategory(Index: integer); // hides all songs in category
var
  S:    integer; // song
begin
  for S := 0 to high(CatSongs.Song) do
  begin
    if not CatSongs.Song[S].Main then
      CatSongs.Song[S].Visible := false // hides all at now
  end;
end;

procedure TCatSongs.ClickCategoryButton(Index: integer);
var
  Num:    integer;
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
  S:    integer;
begin
  // Hide All Songs Show All Cats
  for S := 0 to high(CatSongs.Song) do
    CatSongs.Song[S].Visible := CatSongs.Song[S].Main;
  CatSongs.Selected := CatNumShow; //Show last shown Category
  CatNumShow := -1;
end;
//Hide Categorys when in Category Hack End

//Wrong song selected when tabs on bug
function TCatSongs.FindNextVisible(SearchFrom:integer): integer;//Find next Visible Song
var
  I: integer;
begin
  Result := -1;
  I := SearchFrom + 1;
  while not CatSongs.Song[I].Visible do
  begin
    Inc (I);
    if (I>high(CatSongs.Song)) then
      I := low(CatSongs.Song);
    if (I = SearchFrom) then //Make One Round and no song found->quit
      break;
  end;
end;
//Wrong song selected when tabs on bug End

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
  for SongIndex := 0 to Index-1 do
  begin
    if (CatSongs.Song[SongIndex].Visible) then
      Inc(Result);
  end;
end;

function TCatSongs.SetFilter(FilterStr: string; const fType: Byte): Cardinal;
var
  I, J: integer;
  cString: string;
  SearchStr: array of string;
begin
  {fType: 0: All
          1: Title
          2: Artist}
  FilterStr := Trim(FilterStr);
  if FilterStr<>'' then
  begin
    Result := 0;
    //Create Search Array
    SetLength(SearchStr, 1);
    I := Pos (' ', FilterStr);
    while (I <> 0) do
    begin
      SetLength (SearchStr, Length(SearchStr) + 1);
      cString := Copy(FilterStr, 1, I-1);
      if (cString <> ' ') and (cString <> '') then
        SearchStr[High(SearchStr)-1] := cString;
      Delete (FilterStr, 1, I);

      I := Pos (' ', FilterStr);
    end;
    //Copy last Word
    if (FilterStr <> ' ') and (FilterStr <> '') then
      SearchStr[High(SearchStr)] := FilterStr;

    for I:=0 to High(Song) do
    begin
      if not Song[i].Main then
      begin
        case fType of
          0: cString := Song[I].Artist + ' ' + Song[i].Title + ' ' + Song[i].Folder;
          1: cString := Song[I].Title;
          2: cString := Song[I].Artist;
        end;
        Song[i].Visible:=True;
        //Look for every Searched Word
        for J := 0 to High(SearchStr) do
        begin
          Song[i].Visible := Song[i].Visible and AnsiContainsText(cString, SearchStr[J])
        end;
        if Song[i].Visible then
          Inc(Result);
      end
      else
        Song[i].Visible:=False;
    end;
    CatNumShow := -2;
  end
  else
  begin
    for i:=0 to High(Song) do
    begin
      Song[i].Visible := (Ini.Tabs=1) = Song[i].Main;
      CatNumShow := -1;
    end;
    Result := 0;
  end;
end;

// -----------------------------------------------------------------------------

end.
