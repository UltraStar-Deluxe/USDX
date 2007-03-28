unit USongs;

interface
uses SysUtils, ULog, UTexture, UCatCovers;

type
  TBPM = record
    BPM:        real;
    StartBeat:  real;
  end;

  TScore = record
    Name:       string;
    Score:      integer;
    Length:     string;
  end;

  TSong = record
    Path:       string;
    Folder:     string; // for sorting by folder
    FileName:   string;

    // sorting methods
    Category:   array of string; // I think I won't need this
    Genre:      string;
    Edition:    string;
    Language:   string; // 0.5.0: new

    Title:      string;
    Artist:     string;

    Text:       string;
    Creator:    string;

    Cover:      string;
    CoverTex:   TTexture;
    Mp3:        string;
    Background: string;
    Video:      string;
    VideoGAP:   real;
    VideoLoaded: boolean; // 0.5.0: true if the video has been loaded 
    NotesGAP:   integer;
    Start:      real; // in seconds
    Finish:     integer; // in miliseconds
    Relative:   boolean;
    Resolution: integer;
    BPM:        array of TBPM;
    GAP:        real; // in miliseconds

    Score:      array[0..2] of array of TScore;

    // these are used when sorting is enabled
    Visible:    boolean; // false if hidden, true if visible
    Main:       boolean; // false for songs, true for category buttons
    OrderNum:   integer; // has a number of category for category buttons and songs
    OrderTyp:   integer; // type of sorting for this button (0=name)
    CatNumber:  integer; // Count of Songs in Category for Cats and Number of Song in Category for Songs
  end;

  TSongs = class
    Song:       array of TSong; // array of songs
    Selected:   integer; // selected song index
    procedure LoadSongList; // load all songs
    procedure BrowseDir(Dir: string); // should return number of songs in the future
    procedure Sort(Order: integer);
    function FindSongFile(Dir, Mask: string): string;
  end;

  TCatSongs = class
    Song:       array of TSong; // array of categories with songs
    Selected:   integer; // selected song index
    Order:      integer; // order type (0=title)
    CatNumShow: integer; // Category Number being seen
    CatCount:   integer; //Number of Categorys

    procedure Refresh; // refreshes arrays by recreating them from Songs array
//    procedure Sort(Order: integer);
    procedure ShowCategory(Index: integer); // expands all songs in category
    procedure HideCategory(Index: integer); // hides all songs in category
    procedure ClickCategoryButton(Index: integer); // uses ShowCategory and HideCategory when needed
    procedure ShowCategoryList; //Hides all Songs And Show the List of all Categorys
    function FindNextVisible(SearchFrom:integer): integer; //Find Next visible Song
    function VisibleSongs: integer; // returns number of visible songs (for tabs)
    function VisibleIndex(Index: integer): integer; // returns visible song index (skips invisible)

    function SetFilter(FilterStr: String; const fType: Byte): Cardinal;
  end;

var
  Songs:      TSongs; // all songs
  CatSongs:   TCatSongs; // categorized songs
  AktSong:    TSong; // one song *unknown use)

implementation

uses UPliki, UIni, UFiles, StrUtils;

procedure TSongs.LoadSongList;
begin
  Log.LogStatus('Initializing', 'LoadSongList');

  // clear
  Setlength(Song, 0);

  // browse directories
  BrowseDir(SongPath);
//  if Ini.Debug = 1 then BrowseDir('D:\Extract\Songs\');
end;

procedure TSongs.BrowseDir(Dir: string);
var
  SR:     TSearchRec;   // for parsing Songs Directory
  SLen:   integer;
begin
  if FindFirst(Dir + '*', faDirectory, SR) = 0 then begin
    repeat
      if (SR.Name <> '.') and (SR.Name <> '..') then
        BrowseDir(Dir + Sr.Name + '\');
    until FindNext(SR) <> 0;
  end; // if
  FindClose(SR);

//        Log.LogStatus('Parsing directory: ' + Dir + SR.Name, 'LoadSongList');

 if FindFirst(Dir + '*.txt', 0, SR) = 0 then begin
//          Log.LogStatus('Parsing file:      ' + Dir + SR.Name + '\' + SRD.Name, 'LoadSongList');
    repeat
      SLen := Length(Song);
      SetLength(Song, SLen + 1);
      Song[SLen].Path := Dir;
      Song[SLen].Folder := Copy(Dir, Length(SongPath)+1, 10000);
      Song[SLen].Folder := Copy(Song[SLen].Folder, 1, Pos('\', Song[SLen].Folder)-1);
      Song[SLen].FileName := SR.Name;

      if (AnalyseFile(Song[SLen]) = false) then SetLength(Song, SLen)
      else begin
        // scanning complete, file is good
        // if there is no cover then try to find it
        if Song[SLen].Cover = '' then Song[SLen].Cover := FindSongFile(Dir, '*[CO].jpg');
//        if Song[SLen].Background = '' then begin
//          Song[SLen].Background := FindSongFile(Dir, '*[BG].jpg');
//        end; // no needed here}

        // fix by adding path. no, don't fix it.
//        if Song[SLen].Cover <> '' then
//          Song[SLen].Cover := Song[SLen].Path + Song[SLen].Cover;
      end;

    until FindNext(SR) <> 0;
    end; // if FindFirst
  FindClose(SR);
end;

procedure TSongs.Sort(Order: integer);
var
  S:    integer;
  S2:   integer;
  TempSong:   TSong;
begin
  case Order of
    sEdition: // by edition
      begin
        for S2 := 0 to Length(Song)-1 do
          for S := 1 to Length(Song)-1 do
            if Song[S].Edition < Song[S-1].Edition then begin
              // zamiana miejscami
              TempSong := Song[S-1];
              Song[S-1] := Song[S];
              Song[S] := TempSong;
            end;
      end;
    sGenre: // by genre
      begin
        for S2 := 0 to Length(Song)-1 do
          for S := 1 to Length(Song)-1 do
            if Song[S].Genre < Song[S-1].Genre then begin
              // zamiana miejscami
              TempSong := Song[S-1];
              Song[S-1] := Song[S];
              Song[S] := TempSong;
            end;
      end;
    sTitle: // by title
      begin
        for S2 := 0 to Length(Song)-1 do
          for S := 1 to Length(Song)-1 do
            if Song[S].Title < Song[S-1].Title then begin
              // zamiana miejscami
              TempSong := Song[S-1];
              Song[S-1] := Song[S];
              Song[S] := TempSong;
            end;

      end;
    sArtist: // by artist
      begin
        for S2 := 0 to Length(Song)-1 do
          for S := 1 to Length(Song)-1 do
            if Song[S].Artist < Song[S-1].Artist then begin
              // zamiana miejscami
              TempSong := Song[S-1];
              Song[S-1] := Song[S];
              Song[S] := TempSong;
            end;
      end;
    sFolder: // by folder
      begin
        for S2 := 0 to Length(Song)-1 do
          for S := 1 to Length(Song)-1 do
            if Song[S].Folder < Song[S-1].Folder then begin
              // zamiana miejscami
              TempSong := Song[S-1];
              Song[S-1] := Song[S];
              Song[S] := TempSong;
            end;
      end;
    sTitle2: // by title2
      begin
        for S2 := 0 to Length(Song)-1 do
          for S := 1 to Length(Song)-1 do
            if Song[S].Title < Song[S-1].Title then begin
              // zamiana miejscami
              TempSong := Song[S-1];
              Song[S-1] := Song[S];
              Song[S] := TempSong;
            end;

      end;
    sArtist2: // by artist2
      begin
        for S2 := 0 to Length(Song)-1 do
          for S := 1 to Length(Song)-1 do
            if Song[S].Artist < Song[S-1].Artist then begin
              // zamiana miejscami
              TempSong := Song[S-1];
              Song[S-1] := Song[S];
              Song[S] := TempSong;
            end;
      end;
    sLanguage: // by Language
      begin
        for S2 := 0 to Length(Song)-1 do
          for S := 1 to Length(Song)-1 do
            if Song[S].Language < Song[S-1].Language then begin
              TempSong := Song[S-1];
              Song[S-1] := Song[S];
              Song[S] := TempSong;
            end;
      end;

  end; // case
end;

function TSongs.FindSongFile(Dir, Mask: string): string;
var
  SR:     TSearchRec;   // for parsing song directory
begin
  Result := '';
  if FindFirst(Dir + Mask, faDirectory, SR) = 0 then begin
    Result := SR.Name;
  end; // if
  FindClose(SR);
end;

procedure TCatSongs.Refresh;
var
  S:        integer; // temporary song index
  CatLen:   integer; // length of CatSongs.Song
  Letter:   char; // current letter for sorting using letter
  SS:       string; // current edition for sorting using edition, genre etc.
  Order:    integer; // number used for ordernum
  Letter2:  char; //
  CatNumber:integer; // Number of Song in Category
begin
  CatNumShow := -1;
//  Songs.Sort(0); // by title

case Ini.Sorting of
    sEdition: begin
          Songs.Sort(sArtist);
          Songs.Sort(sEdition);
        end;
    sGenre: begin
          Songs.Sort(sArtist);
          Songs.Sort(sGenre);
        end;
    sLanguage: begin
          Songs.Sort(sArtist);
          Songs.Sort(sLanguage);
        end;
    sFolder:  begin
          Songs.Sort(sArtist);
          Songs.Sort(sFolder);
        end;
    sTitle:  Songs.Sort(sTitle);
    sArtist:  Songs.Sort(sArtist);
    sTitle2:  Songs.Sort(sTitle2); // by title2
    sArtist2:  Songs.Sort(sArtist2); // by artist2

  end; // case


  Letter := ' ';
  SS := '';
  Order := 0;
  CatNumber := 0;

  //Songs leeren
  SetLength (Song, 0);

  for S := Low(Songs.Song) to High(Songs.Song) do begin
    if (Ini.Tabs = 1) then
    if (Ini.Sorting = sEdition) and (SS <> Songs.Song[S].Edition) then begin
      // add Category Button
      Inc(Order);
      SS := Songs.Song[S].Edition;
      CatLen := Length(CatSongs.Song);
      SetLength(CatSongs.Song, CatLen+1);
      CatSongs.Song[CatLen].Artist := '[' + SS + ']';
      CatSongs.Song[CatLen].Main := true;
      CatSongs.Song[CatLen].OrderTyp := 0;
      CatSongs.Song[CatLen].OrderNum := Order;



      // 0.4.3
      // if SS = 'Singstar' then CatSongs.Song[CatLen].Cover := CoversPath + 'Singstar.jpg';
      // if SS = 'Singstar Part 2' then CatSongs.Song[CatLen].Cover := CoversPath + 'Singstar.jpg';
      // if SS = 'Singstar German' then CatSongs.Song[CatLen].Cover := CoversPath + 'Singstar.jpg';
      // if SS = 'Singstar Spanish' then CatSongs.Song[CatLen].Cover := CoversPath + 'Singstar.jpg';
      // if SS = 'Singstar Italian' then CatSongs.Song[CatLen].Cover := CoversPath + 'Singstar.jpg';
      // if SS = 'Singstar French' then CatSongs.Song[CatLen].Cover := CoversPath + 'Singstar.jpg';
      // if SS = 'Singstar Party' then CatSongs.Song[CatLen].Cover := CoversPath + 'Singstar Party.jpg';
      // if SS = 'Singstar Popworld' then CatSongs.Song[CatLen].Cover := CoversPath + 'Singstar Popworld.jpg';
      // if SS = 'Singstar 80s' then CatSongs.Song[CatLen].Cover := CoversPath + 'Singstar 80s.jpg';
      // if SS = 'Singstar 80s Polish' then CatSongs.Song[CatLen].Cover := CoversPath + 'Singstar 80s.jpg';
      // if SS = 'Singstar Rocks' then CatSongs.Song[CatLen].Cover := CoversPath + 'Singstar Rocks.jpg';
      // if SS = 'Singstar Anthems' then CatSongs.Song[CatLen].Cover := CoversPath + 'Singstar Anthems.jpg';

      {// cover-patch
      if FileExists(CoversPath + SS + '.jpg') then CatSongs.Song[CatLen].Cover := CoversPath + SS + '.jpg'
      else if FileExists(CoversPath + 'NoCover.jpg') then CatSongs.Song[CatLen].Cover := CoversPath + 'NoCover.jpg';//}

      CatSongs.Song[CatLen].Cover := CatCovers.GetCover(Ini.Sorting, SS);

      //CatNumber Patch
      if (SS <> '') then
      begin
        Song[CatLen - CatNumber - 1].CatNumber := CatNumber;//Set CatNumber of Categroy
        CatNumber := 0;
      end;

      CatSongs.Song[CatLen].Visible := true;
    end

    else if (Ini.Sorting = sGenre) and (SS <> Songs.Song[S].Genre) then begin
      // add Genre Button
      Inc(Order);
      SS := Songs.Song[S].Genre;
      CatLen := Length(CatSongs.Song);
      SetLength(CatSongs.Song, CatLen+1);
      CatSongs.Song[CatLen].Artist := SS;
      CatSongs.Song[CatLen].Main := true;
      CatSongs.Song[CatLen].OrderTyp := 0;
      CatSongs.Song[CatLen].OrderNum := Order;

      {// cover-patch
      if FileExists(CoversPath + SS + '.jpg') then CatSongs.Song[CatLen].Cover := CoversPath + SS + '.jpg'
      else if FileExists(CoversPath + 'NoCover.jpg') then CatSongs.Song[CatLen].Cover := CoversPath + 'NoCover.jpg';}
      CatSongs.Song[CatLen].Cover := CatCovers.GetCover(Ini.Sorting, SS);

      //CatNumber Patch
      if (SS <> '') then
      begin
        Song[CatLen - CatNumber - 1].CatNumber := CatNumber;//Set CatNumber of Categroy
        CatNumber := 0;
      end;

      CatSongs.Song[CatLen].Visible := true;
    end

    else if (Ini.Sorting = sLanguage) and (SS <> Songs.Song[S].Language) then begin
      // add Language Button
      Inc(Order);
      SS := Songs.Song[S].Language;
      CatLen := Length(CatSongs.Song);
      SetLength(CatSongs.Song, CatLen+1);
      CatSongs.Song[CatLen].Artist := SS;
      CatSongs.Song[CatLen].Main := true;
      CatSongs.Song[CatLen].OrderTyp := 0;
      CatSongs.Song[CatLen].OrderNum := Order;

      {// cover-patch
      if FileExists(CoversPath + SS + '.jpg') then CatSongs.Song[CatLen].Cover := CoversPath + SS + '.jpg'
      else if FileExists(CoversPath + 'NoCover.jpg') then CatSongs.Song[CatLen].Cover := CoversPath + 'NoCover.jpg';}
      CatSongs.Song[CatLen].Cover := CatCovers.GetCover(Ini.Sorting, SS);

      //CatNumber Patch
      if (SS <> '') then
      begin
        Song[CatLen - CatNumber - 1].CatNumber := CatNumber;//Set CatNumber of Categroy
        CatNumber := 0;
      end;

      CatSongs.Song[CatLen].Visible := true;
    end

    else if (Ini.Sorting = sTitle) and (Length(Songs.Song[S].Title)>=1) and (Letter <> Songs.Song[S].Title[1]) then begin
      // add a letter Category Button
      Inc(Order);
      Letter := Songs.Song[S].Title[1];
      CatLen := Length(CatSongs.Song);
      SetLength(CatSongs.Song, CatLen+1);
      CatSongs.Song[CatLen].Artist := '[' + Letter + ']';
      CatSongs.Song[CatLen].Main := true;
      CatSongs.Song[CatLen].OrderTyp := 0;
//      Order := ord(Letter);
      CatSongs.Song[CatLen].OrderNum := Order;


      {// cover-patch
      if FileExists(CoversPath + 'Title' + Letter + '.jpg') then CatSongs.Song[CatLen].Cover := CoversPath + 'Title' + Letter + '.jpg'
      else if FileExists(CoversPath + 'NoCover.jpg') then CatSongs.Song[CatLen].Cover := CoversPath + 'NoCover.jpg';}
      CatSongs.Song[CatLen].Cover := CatCovers.GetCover(Ini.Sorting, Letter);

      //CatNumber Patch
      if (Letter <> ' ') then
      begin
        Song[CatLen - CatNumber - 1].CatNumber := CatNumber;//Set CatNumber of Categroy
        CatNumber := 0;
      end;

      CatSongs.Song[CatLen].Visible := true;
    end

    else if (Ini.Sorting = sArtist) and (Length(Songs.Song[S].Artist)>=1) and (Letter <> Songs.Song[S].Artist[1]) then begin
      // add a letter Category Button
      Inc(Order);
      Letter := Songs.Song[S].Artist[1];
      CatLen := Length(CatSongs.Song);
      SetLength(CatSongs.Song, CatLen+1);
      CatSongs.Song[CatLen].Artist := '[' + Letter + ']';
      CatSongs.Song[CatLen].Main := true;
      CatSongs.Song[CatLen].OrderTyp := 0;
//      Order := ord(Letter);
      CatSongs.Song[CatLen].OrderNum := Order;

      {// cover-patch
      if FileExists(CoversPath + 'Artist' + Letter + '.jpg') then CatSongs.Song[CatLen].Cover := CoversPath + 'Artist' + Letter + '.jpg'
      else if FileExists(CoversPath + 'NoCover.jpg') then CatSongs.Song[CatLen].Cover := CoversPath + 'NoCover.jpg';}
      CatSongs.Song[CatLen].Cover := CatCovers.GetCover(Ini.Sorting, Letter);

      //CatNumber Patch
      if (Letter <> ' ') then
      begin
        Song[CatLen - CatNumber - 1].CatNumber := CatNumber;//Set CatNumber of Categroy
        CatNumber := 0;
      end;

      CatSongs.Song[CatLen].Visible := true;
    end

    else if (Ini.Sorting = sFolder) and (SS <> Songs.Song[S].Folder) then begin
      // 0.5.0: add folder tab
      Inc(Order);
      SS := Songs.Song[S].Folder;
      CatLen := Length(CatSongs.Song);
      SetLength(CatSongs.Song, CatLen+1);
      CatSongs.Song[CatLen].Artist := SS;
      CatSongs.Song[CatLen].Main := true;
      CatSongs.Song[CatLen].OrderTyp := 0;
      CatSongs.Song[CatLen].OrderNum := Order;

      {// cover-patch
      if FileExists(CoversPath + SS + '.jpg') then CatSongs.Song[CatLen].Cover := CoversPath + SS + '.jpg'
      else if FileExists(CoversPath + 'NoCover.jpg') then CatSongs.Song[CatLen].Cover := CoversPath + 'NoCover.jpg';}
      CatSongs.Song[CatLen].Cover := CatCovers.GetCover(Ini.Sorting, SS);

      //CatNumber Patch
      if (SS <> '') then
      begin
        Song[CatLen - CatNumber - 1].CatNumber := CatNumber;//Set CatNumber of Categroy
        CatNumber := 0;
      end;

      CatSongs.Song[CatLen].Visible := true;
    end

    else if (Ini.Sorting = sTitle2) AND (Length(Songs.Song[S].Title)>=1) then begin
      if (ord(Songs.Song[S].Title[1]) > 47) and (ord(Songs.Song[S].Title[1]) < 58) then Letter2 := '#' else Letter2 := Songs.Song[S].Title[1];
      if (Letter <> Letter2) then begin
        // add a letter Category Button
        Inc(Order);
        Letter := Letter2;
        CatLen := Length(CatSongs.Song);
        SetLength(CatSongs.Song, CatLen+1);
        CatSongs.Song[CatLen].Artist := '[' + Letter + ']';
        CatSongs.Song[CatLen].Main := true;
        CatSongs.Song[CatLen].OrderTyp := 0;
//      Order := ord(Letter);
        CatSongs.Song[CatLen].OrderNum := Order;

        {// cover-patch
        if FileExists(CoversPath + 'Title' + Letter + '.jpg') then CatSongs.Song[CatLen].Cover := CoversPath + 'Title' + Letter + '.jpg'
        else if FileExists(CoversPath + 'NoCover.jpg') then CatSongs.Song[CatLen].Cover := CoversPath + 'NoCover.jpg';}
        CatSongs.Song[CatLen].Cover := CatCovers.GetCover(Ini.Sorting, Letter);

        //CatNumber Patch
      if (Letter <> ' ') then
      begin
        Song[CatLen - CatNumber - 1].CatNumber := CatNumber;//Set CatNumber of Categroy
        CatNumber := 0;
      end;

        CatSongs.Song[CatLen].Visible := true;
      end;
    end

    else if (Ini.Sorting = sArtist2) AND (Length(Songs.Song[S].Artist)>=1) then begin
     if (ord(Songs.Song[S].Artist[1]) > 47) and (ord(Songs.Song[S].Artist[1]) < 58) then Letter2 := '#' else Letter2 := Songs.Song[S].Artist[1];
       if (Letter <> Letter2) then begin
        // add a letter Category Button
        Inc(Order);
        Letter := Letter2;
        CatLen := Length(CatSongs.Song);
        SetLength(CatSongs.Song, CatLen+1);
        CatSongs.Song[CatLen].Artist := '[' + Letter + ']';
        CatSongs.Song[CatLen].Main := true;
        CatSongs.Song[CatLen].OrderTyp := 0;
//      Order := ord(Letter);
        CatSongs.Song[CatLen].OrderNum := Order;

        {// cover-patch
        if FileExists(CoversPath + 'Artist' + Letter + '.jpg') then CatSongs.Song[CatLen].Cover := CoversPath + 'Artist' + Letter + '.jpg'
        else if FileExists(CoversPath + 'NoCover.jpg') then CatSongs.Song[CatLen].Cover := CoversPath + 'NoCover.jpg';}
        CatSongs.Song[CatLen].Cover := CatCovers.GetCover(Ini.Sorting, Letter);

        //CatNumber Patch
        if (Letter <> ' ') then
        begin
          Song[CatLen - CatNumber - 1].CatNumber := CatNumber;//Set CatNumber of Categroy
          CatNumber := 0;
        end;
        
        CatSongs.Song[CatLen].Visible := true;
      end;
    end;


    CatLen := Length(CatSongs.Song);
    SetLength(CatSongs.Song, CatLen+1);

    Inc (CatNumber); //Increase Number in Cat

    CatSongs.Song[CatLen] := Songs.Song[S];
    CatSongs.Song[CatLen].OrderNum := Order; // assigns category
    CatSongs.Song[CatLen].CatNumber := CatNumber;

    if (Ini.Tabs = 0) then CatSongs.Song[CatLen].Visible := true
    else if (Ini.Tabs = 1) then CatSongs.Song[CatLen].Visible := false;
//    if (Ini.Tabs = 1) and (Order = 1) then CatSongs.Song[CatLen].Visible := true; // open first tab
//CatSongs.Song[CatLen].Visible := true;

  end;
//CatNumber Patch - Set CatNumber of Last Category
if (ini.Tabs_at_startup = 1) And (high(Song) >=1) then
  Song[CatLen - CatNumber].CatNumber := CatNumber;//Set CatNumber of Categroy
//CatCount Patch
CatCount := Order;
end;

procedure TCatSongs.ShowCategory(Index: integer);
var
  S:    integer; // song
begin
  CatNumShow := Index;
  for S := 0 to high(CatSongs.Song) do
  begin
    if (CatSongs.Song[S].OrderNum = Index) AND (Not CatSongs.Song[S].Main) then
      CatSongs.Song[S].Visible := true
    else
      CatSongs.Song[S].Visible := false;
  end;
end;

procedure TCatSongs.HideCategory(Index: integer); // hides all songs in category
var
  S:    integer; // song
begin
  for S := 0 to high(CatSongs.Song) do begin
    if not CatSongs.Song[S].Main then
      CatSongs.Song[S].Visible := false // hides all at now
  end;
end;

procedure TCatSongs.ClickCategoryButton(Index: integer);
var
  Num, S:    integer;
begin
  Num := CatSongs.Song[Index].OrderNum;
  if Num <> CatNumShow then
    begin
    ShowCategory(Num);
    end
  else begin
    ShowCategoryList;
  end;
end;

//Hide Categorys when in Category Hack
procedure TCatSongs.ShowCategoryList;
var
  Num, S:    integer;
begin
  //Hide All Songs Show All Cats
  for S := 0 to high(CatSongs.Song) do begin
    if CatSongs.Song[S].Main then
      CatSongs.Song[S].Visible := true
    else
      CatSongs.Song[S].Visible := false
  end;
  CatSongs.Selected := CatNumShow; //Show last shown Category
  CatNumShow := -1;
end;
//Hide Categorys when in Category Hack End

//Wrong song selected when tabs on bug
function TCatSongs.FindNextVisible(SearchFrom:integer): integer;//Find next Visible Song
var
  I: Integer;
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

function TCatSongs.VisibleSongs: integer;
var
  S:    integer; // song
begin
  Result := 0;
  for S := 0 to high(CatSongs.Song) do
    if CatSongs.Song[S].Visible = true then Inc(Result);
end;

function TCatSongs.VisibleIndex(Index: integer): integer;
var
  S:    integer; // song
begin
  Result := 0;
  for S := 0 to Index-1 do
    if CatSongs.Song[S].Visible = true then Inc(Result);
end;

function TCatSongs.SetFilter(FilterStr: String; const fType: Byte): Cardinal;
var
  I, J: Integer;
  cString: String;
  SearchStr: Array of String;
begin
  {fType: 0: All
          1: Title
          2: Artist}
  if FilterStr<>'' then begin
    Result := 0;
    //Create Search Array
    SetLength(SearchStr, 1);
    I := Pos (' ', FilterStr);
    While (I <> 0) do
    begin
      SetLength (SearchStr, Length(SearchStr) + 1);
      cString := Copy(FilterStr, 1, I-1);
      if (cString <> ' ') AND (cString <> '') then
        SearchStr[High(SearchStr)-1] := cString;
      Delete (FilterStr, 1, I);

      I := Pos (' ', FilterStr);
    end;
    //Copy last Word
    if (FilterStr <> ' ') AND (FilterStr <> '') then
      SearchStr[High(SearchStr)] := FilterStr;

    for I:=0 to High(Song) do begin
      if not Song[i].Main then
      begin
        case fType of
          0: cString := Song[I].Artist + ' ' + Song[i].Title + ' ' + Song[i].Folder;
          1: cString := Song[I].Title;
          2: cString := Song[I].Artist;
        end;
        Song[i].Visible:=True;
        //Look for every Searched Word
        For J := 0 to High(SearchStr) do
        begin
          Song[i].Visible := Song[i].Visible AND AnsiContainsText(cString, SearchStr[J])
        end;
        if Song[i].Visible then
          Inc(Result);
      end
      else
        Song[i].Visible:=False;
    end;
    CatNumShow := -2;
  end
  else begin
    for i:=0 to High(Song) do begin
      Song[i].Visible:=(Ini.Tabs=1)=Song[i].Main;
      CatNumShow := -1;
    end;
    Result := 0;
  end;
end;

end.
