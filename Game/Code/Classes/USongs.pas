unit USongs;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
     {$IFDEF MSWINDOWS}
       Windows,
       {$ifdef Delphi}
       DirWatch,
       {$endif}
     {$ELSE}
       {$IFNDEF DARWIN}
         oldlinux,
         syscall,
       {$ENDIF}
      baseunix,
      UnixType,
     {$ENDIF}
     SysUtils,
     Classes,
     ULog,
     UTexture,
     UCommon,
	 {$IFDEF DARWIN}
	 cthreads,
	 {$ENDIF}
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

  TSong = record
    Path:       widestring;
    Folder:     widestring; // for sorting by folder
    FileName:   widestring;

    // sorting methods
    Category:   array of widestring; // I think I won't need this
    Genre:      widestring;
    Edition:    widestring;
    Language:   widestring; // 0.5.0: new

    Title:      widestring;
    Artist:     widestring;

    Text:       widestring;
    Creator:    widestring;

    Cover:      widestring;
    CoverTex:   TTexture;
    Mp3:        widestring;
    Background: widestring;
    Video:      widestring;
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

  TSongs = class( TThread )
  private
    BrowsePos : Cardinal; //Actual Pos in Song Array
    fNotify   ,
    fWatch    : longint;
    fParseSongDirectory : boolean;
    fProcessing         : boolean;
    {$ifdef Delphi}
      fDirWatch           : TDirectoryWatch;
    {$endif}
    procedure int_LoadSongList;
    procedure DoDirChanged(Sender: TObject);
  protected
    procedure Execute; override;
  public
    Song      : array of TSong; // array of songs
    Selected  : integer;        // selected song index
    constructor create();
    procedure LoadSongList;     // load all songs
    procedure BrowseDir(Dir: widestring); // should return number of songs in the future
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
  AktSong:    TSong; // one song *unknown use)

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
  inherited create( false );
  self.freeonterminate := true;

  {$ifdef Delphi}
    fDirWatch := TDirectoryWatch.create(nil);
    fDirWatch.OnChange     := DoDirChanged;
    fDirWatch.Directory    := SongPath;
    fDirWatch.WatchSubDirs := true;
    fDirWatch.active       := true;
  {$ENDIF}

  {$IFDEF linux}
  (*
    Thankyou to : http://www.linuxjournal.com/article/8478
                  http://www.tin.org/bin/man.cgi?section=2&topic=inotify_add_watch
  *)
(*
  fNotify := -1;
  fWatch  := -1;
  
  writeln( 'Calling inotify_init' );
  fNotify := Do_SysCall( syscall_nr_inotify_init );
  if ( fNotify < 0 ) then
    writeln( 'Filesystem change notification - disabled' );
  writeln( 'Calling inotify_init : '+ inttostr(fNotify)  );

  writeln( 'Calling syscall_nr_inotify_init ('+SongPath+')' );
  fWatch := Do_SysCall( syscall_nr_inotify_init , TSysParam( fNotify ), longint( pchar( SongPath ) ) , IN_MODIFY AND IN_CREATE AND IN_DELETE  );
  
  if (fWatch < 0) then
     writeln ('inotify_add_watch');
  writeln( 'Calling syscall_nr_inotify_init : '+ inttostr(fWatch)  );
*)
  {$endif}
  
  Setlength(Song, 0);
end;

procedure TSongs.DoDirChanged(Sender: TObject);
begin
  LoadSongList();
end;

procedure TSongs.Execute();
var
  fChangeNotify : THandle;
begin
  fParseSongDirectory := true;
  
  while not self.terminated do
  begin

    if fParseSongDirectory then
    begin
      writeln( 'int_LoadSongList' );
      int_LoadSongList();
    end;

    self.suspend;
  end;
    
end;

procedure TSongs.int_LoadSongList;
begin
  try
    fProcessing := true;
    Setlength(Song, 0);

    Log.LogError('SongList', 'Searching For Songs');

    Setlength(Song, 50);

    BrowsePos := 0;
    // browse directories
    BrowseDir(SongPath);

    //Set Correct SongArray Length
    SetLength(Song, BrowsePos);

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

// TODO : JB - THis whole function SUX ! and needs refactoring ! :P
procedure TSongs.BrowseDir(Dir: widestring);
var
  SLen:   integer;

  {$ifdef Delphi}
    SR:     TSearchRecW;   // for parsing Songs Directory
  {$ENDIF}	
  
  // eddie: can we merge that? is baseunix working on linux? oldlinux is
  //        not available on mac os x.
  {$IFDEF LINUX}
    TheDir  : oldlinux.pdir;
    ADirent : oldlinux.pDirent;
    Entry   : Longint;
    info    : oldlinux.stat;
  {$ENDIF}  
  {$IFDEF DARWIN}
    TheDir  : pdir;
    ADirent : pDirent;
    Entry   : Longint;
    info    : stat;
    lAttrib   : integer;		
  {$ENDIF}  
begin
  {$ifdef Delphi}
    if FindFirstW(Dir + '*', faDirectory, SR) = 0 then   // JB_Unicode - windows
    begin
      repeat
        if (SR.Name <> '.') and (SR.Name <> '..') then
        begin
          BrowseDir(Dir + Sr.Name + PathDelim);
        end
      until FindNextw(SR) <> 0;
    end; // if
    FindClosew(SR);
    
    if FindFirstW(Dir + '*.txt', 0, SR) = 0 then
    begin
      repeat
        SLen := BrowsePos;

        Song[SLen].Path     := Dir;
        Song[SLen].Folder   := Copy(Dir, Length(SongPath)+1, 10000);
        Song[SLen].Folder   := Copy(Song[SLen].Folder, 1, Pos( PathDelim , Song[SLen].Folder)-1);
        Song[SLen].FileName := SR.Name;

        if (AnalyseFile(Song[SLen]) = false) then
          Dec(BrowsePos)
        else
        begin
          if Song[SLen].Cover = '' then
            Song[SLen].Cover := FindSongFile(Dir, '*[CO].jpg');
        end;

        //Change Length Only every 50 Entrys
        Inc(BrowsePos);

        if (BrowsePos mod 50 = 0) AND (BrowsePos <> 0) then
        begin
            SetLength(Song, Length(Song) + 50);
        end;

      until FindNextW(SR) <> 0;
      end; // if FindFirst
    FindCloseW(SR);
    {$ENDIF}
	
   {$IFDEF LINUX}
    // Itterate the Songs Directory... ( With unicode capable functions for linux )
    TheDir := oldlinux.opendir( Dir );     // JB_Unicode - linux
    if TheDir <> nil then
    begin
      repeat
        ADirent := oldlinux.ReadDir(TheDir);

        If ADirent<>Nil then
        begin
          With ADirent^ do
            begin

              if ( name[0] <> '.') then
                BrowseDir( Dir + name + pathdelim );

            end;
        end;
      Until ADirent=Nil;
    end;
    
    

    TheDir := oldlinux.opendir( Dir );     // JB_Unicode - linux
    if TheDir <> nil then
    begin
      repeat
        ADirent := oldlinux.ReadDir(TheDir);
        
        if ( ADirent <> Nil                    ) AND
           ( pos( '.txt', ADirent^.name ) > 0 ) then
        begin
          writeln ('***** FOUND TXT' +  ADirent^.name );
        
          SLen := BrowsePos;

          Song[SLen].Path     := Dir;
          Song[SLen].Folder   := Copy(Dir, Length(SongPath)+1, 10000);
          Song[SLen].Folder   := Copy(Song[SLen].Folder, 1, Pos( PathDelim , Song[SLen].Folder)-1);
          Song[SLen].FileName := ADirent^.name;

          if (AnalyseFile(Song[SLen]) = false) then
            Dec(BrowsePos)
          else
          begin
            if Song[SLen].Cover = '' then
              Song[SLen].Cover := FindSongFile(Dir, '*[CO].jpg');
          end;

          //Change Length Only every 50 Entrys
          Inc(BrowsePos);
          if (BrowsePos mod 50 = 0) AND (BrowsePos <> 0) then
          begin
              SetLength(Song, Length(Song) + 50);
          end;
        end;

      Until ADirent=Nil;
    end; // if FindFirst
  {$endif}
  
  {$IFDEF DARWIN}
    // Itterate the Songs Directory... ( With unicode capable functions for linux )
    TheDir := FPOpenDir(Dir);     // JB_Unicode - linux
    if TheDir <> nil then
    begin
      repeat
        ADirent := FPReadDir(TheDir);

        If assigned(ADirent) and (ADirent^.d_name <> '.') and (ADirent^.d_name <> '..') then
        begin
			      lAttrib := FileGetAttr(Dir + ADirent^.d_name);
						if (lAttrib and faDirectory) <> 0 then
						begin
						  //Log.LogError('Entering dir "' + Dir + ADirent^.d_name + PathDelim + '" now.');
						  BrowseDir(Dir + ADirent^.d_name + PathDelim);						
						end	
						else if Pos( '.txt', LowerCase(ADirent^.d_name)) > 0 then
						begin
							SLen := BrowsePos;
							
							try
							Song[SLen].Path     := Dir;
							Song[SLen].Folder   := Copy(String(Dir), Length(String(SongPath))+1, 10000);
							Song[SLen].Folder   := Copy(String(Song[SLen].Folder), 1, Pos( PathDelim , Song[SLen].Folder)-1);
							Song[SLen].FileName := ADirent^.d_name;
							//Log.LogError( 'Song: ' + ADirent^.d_name + ', Length(Song) = ' + inttostr(Length(Song)) + ', BrowsePos = ' + IntToStr(BrowsePos) + ', Dir = "' + Dir + '"');

							if (AnalyseFile(Song[SLen]) = false) then
							begin
							  Log.LogError('AnalyseFile failed for "' + ADirent^.d_name + '".');
								Dec(BrowsePos);
							end
							else
							begin
								if Song[SLen].Cover = '' then
									Song[SLen].Cover := FindSongFile(Dir, '*[CO].jpg');
							end;
							except 
							end;

							//Change Length Only every 50 Entrys
							Inc(BrowsePos);

							if (BrowsePos mod 50 = 0) AND (BrowsePos <> 0) then
							begin
									SetLength(Song, Length(Song) + 50);
							end;
						end;
        end;
				
      Until ADirent=Nil;
			
			if (FPCloseDir(TheDir) <> 0) then
			begin
			  Log.LogError('TSongs.BrowseDir: Exception: Error closing dir: "' + Dir + '".')
			end;
    end;

  {$endif}
  
//  Log.LogStatus('Parsing directory: ' + Dir + SR.Name, 'LoadSongList');


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
            if CompareText(Song[S].Edition, Song[S-1].Edition) < 0 then begin
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
            if CompareText(Song[S].Genre, Song[S-1].Genre) < 0 then begin
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
            if CompareText(Song[S].Title, Song[S-1].Title) < 0 then begin
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
            if CompareText(Song[S].Artist, Song[S-1].Artist) < 0 then begin
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
            if CompareText(Song[S].Folder, Song[S-1].Folder) < 0 then begin
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
            if CompareText(Song[S].Title, Song[S-1].Title) < 0 then begin
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
            if CompareText(Song[S].Artist, Song[S-1].Artist) < 0 then begin
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
            if CompareText(Song[S].Language, Song[S-1].Language) < 0 then begin
              TempSong := Song[S-1];
              Song[S-1] := Song[S];
              Song[S] := TempSong;
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

  for S := Low(Songs.Song) to High(Songs.Song) do begin
    if (Ini.Tabs = 1) then
    if (Ini.Sorting = sEdition) and (CompareText(SS, Songs.Song[S].Edition) <> 0) then begin
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

    else if (Ini.Sorting = sGenre) and (CompareText(SS, Songs.Song[S].Genre) <> 0) then begin
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

    else if (Ini.Sorting = sLanguage) and (CompareText(SS, Songs.Song[S].Language) <> 0) then begin
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

    else if (Ini.Sorting = sTitle) and
            (Length(Songs.Song[S].Title)>=1) and
            (Letter <> UpperCase(Songs.Song[S].Title)[1]) then begin
      // add a letter Category Button
      Inc(Order);
      Letter := Uppercase(Songs.Song[S].Title)[1];
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

    else if (Ini.Sorting = sArtist) and (Length(Songs.Song[S].Artist)>=1) and (Letter <> UpperCase(Songs.Song[S].Artist)[1]) then begin
      // add a letter Category Button
      Inc(Order);
      Letter := UpperCase(Songs.Song[S].Artist)[1];
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

    else if (Ini.Sorting = sFolder) and (CompareText(SS, Songs.Song[S].Folder) <> 0) then begin
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
      if (ord(Songs.Song[S].Title[1]) > 47) and (ord(Songs.Song[S].Title[1]) < 58) then Letter2 := '#' else Letter2 := UpperCase(Songs.Song[S].Title)[1];
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
     if (ord(Songs.Song[S].Artist[1]) > 47) and (ord(Songs.Song[S].Artist[1]) < 58) then Letter2 := '#' else Letter2 := UpperCase(Songs.Song[S].Artist)[1];
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

end.
