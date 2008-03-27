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
    fNotify   ,
    fWatch    : longint;
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
//    Song      : array of TSong; // array of songs
    SongList  : TList; // array of songs
    Selected  : integer;        // selected song index
    constructor create();
    destructor  destroy(); override;


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

const
     IN_ACCESS		    = $00000001;	//* File was accessed */
     IN_MODIFY		    = $00000002;	//* File was modified */
     IN_ATTRIB		    = $00000004;	//* Metadata changed */
     IN_CLOSE_WRITE		= $00000008;	//* Writtable file was closed */
     IN_CLOSE_NOWRITE	= $00000010;	//* Unwrittable file closed */
     IN_OPEN			    = $00000020;	//* File was opened */
     IN_MOVED_FROM		= $00000040;	//* File was moved from X */
     IN_MOVED_TO		  = $00000080;	//* File was moved to Y */
     IN_CREATE		    = $00000100;	//* Subfile was created */
     IN_DELETE		    = $00000200;	//* Subfile was deleted */
     IN_DELETE_SELF		= $00000400;	//* Self was deleted */


implementation

uses StrUtils,
     UGraphic,
     UCovers,
     UFiles,
     UMain,
     UIni;

{$IFDEF DARWIN}
function AnsiContainsText(const AText, ASubText: string): Boolean;
begin
  Result := AnsiPos(AnsiUppercase(ASubText), AnsiUppercase(AText)) > 0;
end;
{$ENDIF}

constructor TSongs.create();
begin
  // do not start thread BEFORE initialization (suspended = true)
  inherited create( true );
  self.freeonterminate := true;

  SongList := TList.create();

  {$ifdef MSWINDOWS}
    fDirWatch := TDirectoryWatch.create(nil);
    fDirWatch.OnChange     := DoDirChanged;
    fDirWatch.Directory    := SongPath;
    fDirWatch.WatchSubDirs := true;
    fDirWatch.active       := true;
  {$ENDIF}

  // now we can start the thread
  Resume();
end;

destructor TSongs.destroy();
begin
  freeandnil( SongList );
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

  while not self.terminated do
  begin

    if fParseSongDirectory then
    begin
      debugWriteln( 'int_LoadSongList' );
      int_LoadSongList();
    end;

    self.suspend;
  end;
{$ENDIF}
end;

procedure TSongs.int_LoadSongList;
const
  cUSNGPath = '/usr/share/games/ultrastar-ng/songs';
begin
  try
    fProcessing := true;

    Log.LogError('SongList', 'Searching For Songs');

    // browse directories
    BrowseDir(SongPath);

    if UserSongPath <> SongPath then
      BrowseDir(UserSongPath);

(*
    if ( cUSNGPath <> SongPath     ) AND
       ( cUSNGPath <> UserSongPath ) then
      BrowseDir( cUSNGPath );                       // todo : JB this is REAL messy,
*)                                                    // we should have some sort of path manager that lets us specify X number of extra paths to search

    if assigned( CatSongs ) then
      CatSongs.Refresh;

    if assigned( CatCovers ) then
      CatCovers.Load;

    if assigned( Covers ) then
      Covers.Load;

    if assigned(ScreenSong)  then
    begin
      ScreenSong.GenerateThumbnails();
      ScreenSong.OnShow; // refresh ScreenSong
    end;

  finally
    Log.LogError('SongList', 'Search Complete');

    fParseSongDirectory := false;
    fProcessing         := false;
  end;
end;


procedure TSongs.LoadSongList;
begin
  fParseSongDirectory := true;
  self.resume;
end;

procedure TSongs.BrowseDir(Dir: widestring);
begin
 BrowseTXTFiles(Dir);
 BrowseXMLFiles(Dir);
end;

procedure TSongs.BrowseTXTFiles(Dir: widestring);
var
  i       : Integer;
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

        if NOT lSong.Analyse then
				  begin
					  Log.LogError('AnalyseFile failed for "' + Files[i].Name + '".');
            freeandnil( lSong );
				  end
          else SongList.add( lSong );

			end;
		end;
		SetLength( Files, 0);

end;

procedure TSongs.BrowseXMLFiles(Dir: widestring);
var
  i       : Integer;
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

        if NOT lSong.AnalyseXML then
				  begin
					  Log.LogError('AnalyseFile failed for "' + Files[i].Name + '".');
            freeandnil( lSong );
				  end
          else SongList.add( lSong );

			end;
		end;
		SetLength( Files, 0);

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
        for S2 := 0 to SongList.Count -1 do
          for S := 1 to SongList.Count-1 do
            if CompareText(TSong( SongList[S] ).Edition, TSong( SongList[S-1] ).Edition) < 0 then
            begin
              // zamiana miejscami
              TempSong      := SongList[S-1];
              SongList[S-1] := SongList[S];
              SongList[S]   := TempSong;
            end;
      end;
    sGenre: // by genre
      begin
        for S2 := 0 to SongList.Count-1 do
          for S := 1 to SongList.Count-1 do
            if CompareText(TSong( SongList[S] ).Genre, TSong( SongList[S-1] ).Genre) < 0 then
            begin
              // zamiana miejscami
              TempSong      := SongList[S-1];
              SongList[S-1] := SongList[S];
              SongList[S]   := TempSong;
            end;
      end;
    sTitle: // by title
      begin
        for S2 := 0 to SongList.Count-1 do
          for S := 1 to SongList.Count-1 do
            if CompareText(TSong( SongList[S] ).Title, TSong( SongList[S-1] ).Title) < 0 then
            begin
              // zamiana miejscami
              TempSong      := SongList[S-1];
              SongList[S-1] := SongList[S];
              SongList[S]   := TempSong;
            end;

      end;
    sArtist: // by artist
      begin
        for S2 := 0 to SongList.Count-1 do
          for S := 1 to SongList.Count-1 do
            if CompareText(TSong( SongList[S] ).Artist, TSong( SongList[S-1] ).Artist) < 0 then
            begin
              // zamiana miejscami
              TempSong      := SongList[S-1];
              SongList[S-1] := SongList[S];
              SongList[S]   := TempSong;
            end;
      end;
    sFolder: // by folder
      begin
        for S2 := 0 to SongList.Count-1 do
          for S := 1 to SongList.Count-1 do
            if CompareText(TSong( SongList[S] ).Folder, TSong( SongList[S-1] ).Folder) < 0 then
            begin
              // zamiana miejscami
              TempSong      := SongList[S-1];
              SongList[S-1] := SongList[S];
              SongList[S]   := TempSong;
            end;
      end;
    sTitle2: // by title2
      begin
        for S2 := 0 to SongList.Count-1 do
          for S := 1 to SongList.Count-1 do
            if CompareText(TSong( SongList[S] ).Title, TSong( SongList[S-1] ).Title) < 0 then
            begin
              // zamiana miejscami
              TempSong      := SongList[S-1];
              SongList[S-1] := SongList[S];
              SongList[S]   := TempSong;
            end;

      end;
    sArtist2: // by artist2
      begin
        for S2 := 0 to SongList.Count-1 do
          for S := 1 to SongList.Count-1 do
            if CompareText(TSong( SongList[S] ).Artist, TSong( SongList[S-1] ).Artist) < 0 then
            begin
              // zamiana miejscami
              TempSong      := SongList[S-1];
              SongList[S-1] := SongList[S];
              SongList[S]   := TempSong;
            end;
      end;
    sLanguage: // by Language
      begin
        for S2 := 0 to SongList.Count-1 do
          for S := 1 to SongList.Count-1 do
            if CompareText(TSong( SongList[S] ).Language, TSong( SongList[S-1] ).Language) < 0 then
            begin
              TempSong      := SongList[S-1];
              SongList[S-1] := SongList[S];
              SongList[S]   := TempSong;
            end;
      end;

  end; // case
end;

function TSongs.FindSongFile(Dir, Mask: widestring): widestring;
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

  for S := 0 to Songs.SongList.Count-1 do
  begin
    if (Ini.Tabs = 1) then
    if (Ini.Sorting = sEdition) and (CompareText(SS, TSong( Songs.SongList[S] ).Edition) <> 0) then begin
      // add Category Button
      Inc(Order);
      SS := TSong( Songs.SongList[S] ).Edition;
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

    else if (Ini.Sorting = sGenre) and (CompareText(SS, TSong( Songs.SongList[S] ).Genre) <> 0) then begin
      // add Genre Button
      Inc(Order);
      SS := TSong( Songs.SongList[S] ).Genre;
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

    else if (Ini.Sorting = sLanguage) and (CompareText(SS, TSong( Songs.SongList[S] ).Language) <> 0) then begin
      // add Language Button
      Inc(Order);
      SS := TSong( Songs.SongList[S] ).Language;
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

    else if (Ini.Sorting = sTitle) and
            (Length(TSong( Songs.SongList[S] ).Title)>=1) and
            (Letter <> UpperCase(TSong( Songs.SongList[S] ).Title)[1]) then begin
      // add a letter Category Button
      Inc(Order);
      Letter := Uppercase(TSong( Songs.SongList[S] ).Title)[1];
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

    else if (Ini.Sorting = sArtist) and (Length(TSong( Songs.SongList[S] ).Artist)>=1) and
            (Letter <> UpperCase(TSong( Songs.SongList[S] ).Artist)[1]) then begin
      // add a letter Category Button
      Inc(Order);
      Letter := UpperCase(TSong( Songs.SongList[S] ).Artist)[1];
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

    else if (Ini.Sorting = sFolder) and (CompareText(SS, TSong( Songs.SongList[S] ).Folder) <> 0) then begin
      // 0.5.0: add folder tab
      Inc(Order);
      SS := TSong( Songs.SongList[S] ).Folder;
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

    else if (Ini.Sorting = sTitle2) AND (Length(TSong( Songs.SongList[S] ).Title)>=1) then begin
      if (ord(TSong( Songs.SongList[S] ).Title[1]) > 47) and (ord(TSong( Songs.SongList[S] ).Title[1]) < 58) then Letter2 := '#' else Letter2 := UpperCase(TSong( Songs.SongList[S] ).Title)[1];
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

    else if (Ini.Sorting = sArtist2) AND (Length(TSong( Songs.SongList[S] ).Artist)>=1) then begin
     if (ord(TSong( Songs.SongList[S] ).Artist[1]) > 47) and (ord(TSong( Songs.SongList[S] ).Artist[1]) < 58) then Letter2 := '#' else Letter2 := UpperCase(TSong( Songs.SongList[S] ).Artist)[1];
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

    CatSongs.Song[CatLen] := TSong( Songs.SongList[S] );
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
  FilterStr := Trim(FilterStr);
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



// -----------------------------------------------------------------------------




end.
