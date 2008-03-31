unit UDataBase;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses USongs,
     USong,
     SQLiteTable3;

//--------------------
//DataBaseSystem - Class including all DB Methods
//--------------------
type
  TStatResult = record
    Case Typ: Byte of
      0: (Singer:     ShortString;
          Score:      Word;
          Difficulty: Byte;
          SongArtist: ShortString;
          SongTitle:  ShortString);

      1: (Player:     ShortString;
          AverageScore: Word);

      2: (Artist: ShortString;
          Title:  ShortString;
          TimesSung:  Word);

      3: (ArtistName:   ShortString;
          TimesSungtot: Word);
  end;
  AStatResult = Array of TStatResult;

  PDBSongInfo = ^TDBSongInfo;
  TDBSongInfo = record
    FileName:    String;
    FolderID:    Integer;
    FullPath:    String;
    LastChanged: Integer; //TimeStamp

    //Required Information
    Artist:     widestring;
    Title:      widestring;

    Mp3:        widestring;

    //Some Files
    Cover:      widestring; //Path to Cover
    CoverID:    Integer;    //ID of Cover in Covers Cache
    Video:      widestring;
    VideoGAP:   real;


    //Additional Information
    Start:      real; // in seconds

    //Sorting
    Genre:      widestring;
    Edition:    widestring;
    Language:   widestring;
    Year:       widestring;
    Creator:    widestring;
    TimesPlayed:Word;
  end;

  TSongListInfo = record //SongInfo used by Songscreen
    ID:         Integer;
    Title:      widestring;
    Artist:     widestring;

    Mp3:        widestring;
    Video:      widestring;
    CoverID:    Integer; //Cover ID in CoversCache
  end;
  ASongList = Array of TSongListInfo;

  TSongListFilter = record
    Field:    Byte;    //FieldID //See constants below
    isSearch: Boolean; //Search Mask(true) or exact Match(False)
    Filter:   String;
  end;
  ASongListFilter = Array of TSongListFilter;

  TDataBaseSystem = class
    private
      ScoreDB: TSqliteDatabase;
      sFilename: string;
      UpdateFromVer1: Boolean;

    public


    property Filename: String read sFilename;

    Destructor Free;

    Procedure Init(const Filename: string);
    Function ReadScore(const Song: Integer; const Difficulty: Byte): AScore;
    procedure AddScore(Song: Integer; Level: integer; Name: string; Score: integer);
    procedure WriteScore(Song: Integer);

    Function  GetIDbyPath(const Path: String): Integer;
    Function  GetIDbyFileName(const Filename: String; FolderID: Integer): Integer;
    Function  GetSongData(ID: Integer; const Info: PDBSongInfo): Boolean;
    Function  SetSongData(ID: Integer; const Info: PDBSongInfo): Boolean;
    Function  AddSong(ID: Integer; const Info: PDBSongInfo): Boolean;
    Function  GetLastChangedbyID(const Song: Integer): Integer;
    Function  GetLastChangedbyFileName(const Filename: String; FolderID: Integer): Integer;

    Function  GetFolderIDbyPath(Path: String): Integer;
    Function  GetFolderIDbyName(const Name: String; ParentFolder: Integer): Integer;

    Function  GetSongList(var List: ASongList; const Filter: ASongListFilter): Integer;

    Function  GetStats(var Stats: AStatResult; const Typ, Count: Byte; const Page: Cardinal; const Reversed: Boolean): Boolean;
    Function  GetTotalEntrys(const Typ: Byte): Cardinal;
  end;

const
  //Filter FieldIDs
  SLF_Edition   = 0;
  SLF_Genre     = 1;
  SLF_Language  = 2;
  SLF_Folder    = 3;
  SLF_Title     = 4;
  SLF_Artist    = 5;
  SLF_Year      = 6;
  SLF_Creator   = 7;
  SLF_Video     = 8; //Songs w/ Video are returned
  SLF_NoVideo   = 9; //Songs w/o Video are returned
  Max_FilterFieldID = SLF_NoVideo;

var
  DataBase: TDataBaseSystem;

implementation

uses
  IniFiles,
  ULog,
  SysUtils;

const
  cUS_Scores = 'us_scores';
  cUS_Songs  = 'us_songs';
  cUS_Info   = 'us_info';
  cUS_Directories = 'us_directories';
  cDB_Version = '2.0.0';

//--------------------
//Create - Opens Database and Create Tables if not Exist
//--------------------

Procedure TDataBaseSystem.Init(const Filename: string);
begin
  debugWriteln( 'TDataBaseSystem.Init ('+Filename+') @ '+ floattostr( now() ) );
  
  //Open Database
  ScoreDB        := TSqliteDatabase.Create( Filename );
  sFilename      := Filename;
  UpdateFromVer1 := False;

  try
    //Check for Database Version
    if not ScoreDB.TableExists( cUS_Info ) then
    begin
      If (ScoreDB.TableExists( cUS_Scores ) And ScoreDB.TableExists( cUS_Songs )) And (Not ScoreDB.TableExists( cUS_Directories )) then
      begin //Update from a Ver. 1.0 - 1.0.1a Database
        UpdateFromVer1 := True; //This table should be Updated from before Database ver 2.0
        ScoreDb.ExecSQL('ALTER TABLE `' + cUS_Songs + '` RENAME TO ''' + cUS_Songs + '_old'''); //Rename old Song Table, to achieve old SongIDs
        debugWriteln( 'TDataBaseSystem.Init - Switched to "Update from DB Version 1" Mode' );
      end;

      //Create Info Table
      ScoreDB.ExecSQL('CREATE TABLE `' + cUS_Info + '` ( `Ident` varchar(32) PRIMARY KEY, `Value` varchar(64) NOT NULL default '''');');

      //Write Database Version to Table
      ScoreDB.ExecSQL('INSERT INTO `' + cUS_Info + '` VALUES (''version'', ''' + cDB_Version + ''');');

      debugWriteln( 'TDataBaseSystem.Init - CREATED US_Info' );
    end;

    //Look for Tables => When not exist Create them

    if not ScoreDB.TableExists( cUS_Directories ) then
    begin
      ScoreDB.execsql('CREATE TABLE `' + cUS_Directories + '` ( `ID` integer NOT NULL PRIMARY KEY AUTOINCREMENT, `Name` varchar(255) NOT NULL default '''', `FullPath` text NOT NULL, `LastChange` int(10) NOT NULL default ''0'');');

      //Add Root Directory
      ScoreDB.ExecSQL('INSERT INTO `us_directories` VALUES (1, ''root'', '''', ''0'');');

      debugWriteln( 'TDataBaseSystem.Init - CREATED US_Directories' );
    end;

    if not ScoreDB.TableExists( cUS_Scores ) then
    begin
      ScoreDB.execsql('CREATE TABLE `'+cUS_Scores+'` (`SongID` INT( 10 ) NOT NULL , `Difficulty` INT( 1 ) NOT NULL , `Player` VARCHAR( 150 ) NOT NULL , `Score` INT( 5 ) NOT NULL );');

      debugWriteln( 'TDataBaseSystem.Init - CREATED US_Scores' );
    end;

    if not ScoreDB.TableExists( cUS_Songs ) then
    begin
      // Old 1.0 Style: ScoreDB.execsql('CREATE TABLE `'+cUS_Songs+'` (`ID` INTEGER PRIMARY KEY, `Artist` VARCHAR( 255 ) NOT NULL , `Title` VARCHAR( 255 ) NOT NULL , `TimesPlayed` int(5) NOT NULL );');
      //New: Dataabse 2.0 Style
      ScoreDb.ExecSQL('CREATE TABLE `' + cUS_Songs + '` ( ' +
                      '`ID` integer PRIMARY KEY AUTOINCREMENT, ' +
                      '`Filename` varchar(255) NOT NULL default '''', ' +
                      '`FolderID` int(10) NOT NULL default ''0'', ' +
                      '`FullPath` text NOT NULL, ' +
                      '`LastChanged` int(10) NOT NULL default ''0'', ' +
                      '`Artist` varchar(255) NOT NULL default '''', ' +
                      '`Title` varchar(255) NOT NULL default '''', ' +
                      '`Mp3` text NOT NULL, ' +
                      '`Cover` text NOT NULL, ' +
                      '`CoverID` int(10) NOT NULL default ''0'', ' +
                      '`Video` text NOT NULL, ' +
                      '`VideoGap` float NOT NULL default ''0'', ' +
                      '`Start` float NOT NULL default ''0'', ' +
                      '`Genre` varchar(255) NOT NULL default '''', ' +
                      '`Edition` varchar(255) NOT NULL default '''', ' +
                      '`Language` varchar(255) NOT NULL default '''', ' +
                      '`Year` varchar(255) NOT NULL default '''', ' +
                      '`Creator` varchar(255) NOT NULL default '''', ' +
                      '`TimesPlayed` int(5) NOT NULL default ''0'');');

      //Delete Score Table to avoid wrong IDS
      ScoreDb.ExecSQL('DELETE FROM `' + cUS_Scores + '`');

      debugWriteln( 'TDataBaseSystem.Init - CREATED US_Songs' );
    end;


  finally
    debugWriteln( cUS_Songs +' Exist : ' + inttostr( integer(ScoreDB.TableExists( cUS_Songs )) ) );
    debugWriteln( cUS_Scores +' Exist : ' + inttostr( integer(ScoreDB.TableExists( cUS_Scores )) ) );
  //ScoreDB.Free;
  end;

end;

//--------------------
//Free - Frees Database
//--------------------
Destructor TDataBaseSystem.Free;
begin
  debugWriteln( 'TDataBaseSystem.Free' );

  freeandnil( ScoreDB );
end;

//--------------------
//ReadScore - Read Scores into SongArray
//--------------------
Function TDataBaseSystem.ReadScore(const Song: Integer; const Difficulty: Byte): AScore;
var
  TableData: TSqliteTable;
  I: Integer;
begin
  if (assigned( ScoreDB )) AND (Difficulty < 2) then
  begin

    //ScoreDB := TSqliteDatabase.Create(sFilename);
    try
      try
        //Search Song in DB
        TableData := ScoreDB.GetTable('SELECT `Player`, `Score` FROM `'+cUS_Scores+'` WHERE (`SongID` = ''' + InttoStr(Song) + ''') AND (`Difficulty` = ''' + InttoStr(Difficulty) + ''') ORDER BY `Score` DESC  LIMIT 5');


        I := 0;
        while not TableData.Eof do //Go through all Entrys
        begin //Add one Entry to Array
            Result[I].Name  :=
                TableData.FieldAsString(TableData.FieldIndex['Player']);
            Result[I].Score :=
                StrtoInt(TableData.FieldAsString(TableData.FieldIndex['Score']));

          TableData.Next;
          Inc(I);
        end; // While not TableData.EOF

        If (I < 5) then
          Result[1].Score := 0; //Place ending Zero

      except
        Result[0].Name  := 'Error Reading ScoreDB';
        Result[0].Score := -1;
        Result[1].Score := 0;
      end;

    finally
      //ScoreDb.Free;
    end;
  end;
end;

//--------------------
//AddScore - Add one new Score to DB
//--------------------
procedure TDataBaseSystem.AddScore(Song: Integer; Level: integer; Name: string; Score: integer);
var
ID: Integer;
TableData: TSqliteTable;
begin
  if assigned( ScoreDB ) then
  begin

    //ScoreDB := TSqliteDatabase.Create(sFilename);
    try
      //Prevent 0 Scores from being added
      if (Score > 0) then
      begin
        {ID := ScoreDB.GetTableValue('SELECT `ID` FROM `'+cUS_Songs+'` WHERE `Artist` = "' + Song.Artist + '" AND `Title` = "' + Song.Title + '"');
        if ID = 0 then //Song doesn't exist -> Create
        begin
          ScoreDB.ExecSQL ('INSERT INTO `'+cUS_Songs+'` ( `ID` , `Artist` , `Title` , `TimesPlayed` ) VALUES (NULL , "' + Song.Artist + '", "' + Song.Title + '", "0");');
          ID := ScoreDB.GetTableValue('SELECT `ID` FROM `US_Songs` WHERE `Artist` = "' + Song.Artist + '" AND `Title` = "' + Song.Title + '"');
          if ID = 0 then //Could not Create Table
            exit;
        end; }
        //Create new Entry
        ScoreDB.ExecSQL('INSERT INTO `'+cUS_Scores+'` ( `SongID` , `Difficulty` , `Player` , `Score` ) VALUES ("' + InttoStr(Song) + '", "' + InttoStr(Level) + '", "' + Name + '", "' + InttoStr(Score) + '");');

        //Delete Last Position when there are more than 5 Entrys
        if ScoreDB.GetTableValue('SELECT COUNT(`SongID`) FROM `'+cUS_Scores+'` WHERE `SongID` = "' + InttoStr(ID) + '" AND `Difficulty` = "' + InttoStr(Level) +'"') > 5 then
        begin
          TableData := ScoreDB.GetTable('SELECT `Player`, `Score` FROM `'+cUS_Scores+'` WHERE SongID = "' + InttoStr(ID) + '" AND `Difficulty` = "' + InttoStr(Level) +'" ORDER BY `Score` ASC LIMIT 1');
          ScoreDB.ExecSQL('DELETE FROM `US_Scores` WHERE SongID = "' + InttoStr(ID) + '" AND `Difficulty` = "' + InttoStr(Level) +'" AND `Player` = "' + TableData.FieldAsString(TableData.FieldIndex['Player']) + '" AND `Score` = "' + TableData.FieldAsString(TableData.FieldIndex['Score']) + '"');
        end;

      end;
    finally
    //ScoreDB.Free;
    end;
  end;
end;

//--------------------
//WriteScore - Not needed with new System; But used for Increment Played Count
//--------------------
procedure TDataBaseSystem.WriteScore(Song: Integer);
begin
  if not assigned( ScoreDB ) then
  begin
    try
      //Increase TimesPlayed
      ScoreDB.ExecSQL ('UPDATE `'+cUS_Songs+'` SET `TimesPlayed` = `TimesPlayed` + ''1'' WHERE `ID` = ''' + InttoStr(Song) + ''';');
    except

    end;
  end;
end;

//--------------------
//GetStats - Write some Stats to Array, Returns True if Chossen Page has Entrys
//Case Typ of
//0 - Best Scores
//1 - Best Singers
//2 - Most sung Songs
//3 - Most popular Band
//--------------------
Function TDataBaseSystem.GetStats(var Stats: AStatResult; const Typ, Count: Byte; const Page: Cardinal; const Reversed: Boolean): Boolean;
var
  Query: String;
  TableData: TSqliteTable;
begin
  Result := False;

  if not assigned( ScoreDB ) then
    exit;

  if (Length(Stats) < Count) then
    Exit;

  {Todo:  Add Prevention that only Players with more than 5 Scores are Selected at Typ 2}

  //Create Query
  Case Typ of
    0: Query := 'SELECT `Player` , `Difficulty` , `Score` , `Artist` , `Title` FROM `'+cUS_Scores+'` INNER JOIN `US_Songs` ON (`SongID` = `ID`) ORDER BY `Score`';
    1: Query := 'SELECT `Player` , ROUND (Sum(`Score`) / COUNT(`Score`)) FROM `'+cUS_Scores+'` GROUP BY `Player` ORDER BY (Sum(`Score`) / COUNT(`Score`))';
    2: Query := 'SELECT `Artist` , `Title` , `TimesPlayed` FROM `'+cUS_Songs+'` ORDER BY `TimesPlayed`';
    3: Query := 'SELECT `Artist` , Sum(`TimesPlayed`) FROM `'+cUS_Songs+'` GROUP BY `Artist` ORDER BY Sum(`TimesPlayed`)';
  end;

  //Add Order Direction
  If Reversed then
    Query := Query + ' ASC'
  else
    Query := Query + ' DESC';

  //Add Limit
  Query := Query + ' LIMIT ' + InttoStr(Count * Page) + ', ' + InttoStr(Count) + ';';

  //Execute Query
  try
    TableData := ScoreDB.GetTable(Query);
  except
    exit;  // this has a try except, because ( on linux at least ) it seems that doing a GetTable, that returns nothing
           // causes an exception.   and in the case of a new Database file, with no scores stored yet... this seems to except here.
  end;

  //if Result empty -> Exit
  if (TableData.RowCount < 1) then
    exit;

  //Copy Result to Stats Array
  while not TableData.Eof do
  begin
    Stats[TableData.Row].Typ := Typ;

    Case Typ of
      0:begin
          Stats[TableData.Row].Singer := TableData.Fields[0];

          Stats[TableData.Row].Difficulty := StrtoIntDef(TableData.Fields[1], 0);

          Stats[TableData.Row].Score := StrtoIntDef(TableData.Fields[2], 0){TableData.FieldAsInteger(2)};
          Stats[TableData.Row].SongArtist := TableData.Fields[3];
          Stats[TableData.Row].SongTitle := TableData.Fields[4];
        end;

        1:begin
          Stats[TableData.Row].Player := TableData.Fields[0];
          Stats[TableData.Row].AverageScore := StrtoIntDef(TableData.Fields[1], 0);
        end;

        2:begin
          Stats[TableData.Row].Artist := TableData.Fields[0];
          Stats[TableData.Row].Title  := TableData.Fields[1];
          Stats[TableData.Row].TimesSung  := StrtoIntDef(TableData.Fields[2], 0);
        end;

        3:begin
          Stats[TableData.Row].ArtistName := TableData.Fields[0];
          Stats[TableData.Row].TimesSungtot := StrtoIntDef(TableData.Fields[1], 0);
        end;

    end;

    TableData.Next;
  end;

  Result := True;
end;

//--------------------
//GetTotalEntrys - Get Total Num of entrys for a Stats Query
//--------------------
Function  TDataBaseSystem.GetTotalEntrys(const Typ: Byte): Cardinal;
var Query: String;
begin
  Result := 0;

  if not assigned( ScoreDB ) then
    exit;

  try
    //Create Query
    Case Typ of
      0: begin
           Query := 'SELECT COUNT(`SongID`) FROM `'+cUS_Scores+'`;';
           if not ScoreDB.TableExists( cUS_Scores ) then
             exit;
         end;
      1: begin
           Query := 'SELECT COUNT(DISTINCT `Player`) FROM `'+cUS_Scores+'`;';
           if not ScoreDB.TableExists( cUS_Scores ) then
             exit;
         end;
      2: begin
           Query := 'SELECT COUNT(`ID`) FROM `'+cUS_Songs+'`;';
           if not ScoreDB.TableExists( cUS_Songs ) then
             exit;
         end;
      3: begin
           Query := 'SELECT COUNT(DISTINCT `Artist`) FROM `'+cUS_Songs+'`;';
           if not ScoreDB.TableExists( cUS_Songs ) then
             exit;
         end;
    end;
  
    Result := ScoreDB.GetTableValue(Query);
  except
    // TODO : JB_Linux - Why do we get these exceptions on linux !!
    on E:ESQLiteException DO  // used to handle : Could not retrieve data "SELECT COUNT(`ID`) FROM `US_Songs`;" : SQL logic error or missing database
                              // however, we should pre-empt this error... and make sure the database DOES exist.
    begin
      exit;
    end;
  end;

end;

Function  TDataBaseSystem.GetIDbyPath(const Path: String): Integer;
begin

end;

Function  TDataBaseSystem.GetIDbyFileName(const Filename: String; FolderID: Integer): Integer;
begin
  try
    Result := ScoreDB.GetTableValue('SELECT `ID` FROM `' + cUS_Songs + '` WHERE (`Filename` = ''' + Filename + ''') AND (`FolderID` = ''' + InttoStr(FolderID) + ''')');
  except
    Result := 0;
  end;
end;

Function TDataBaseSystem.GetSongData(ID: Integer; const Info: PDBSongInfo): Boolean;
var
  TableData: TSqliteTable;
begin
  Result := True;
  try
    TableData := ScoreDB.GetTable('SELECT Filename`, `FolderID`, `FullPath`, `LastChanged`, `Artist`, `Title`, `Mp3`, `Cover`, `CoverID`, `Video`, `VideoGap`, `Start`, `Genre`, `Edition`, `Language`, `Year`, `Creator`, `TimesPlayed` FROM `' + cUS_Songs + '` WHERE `ID` = ''' + InttoStr(ID) + ''' ');
    If (TableData.RowCount > 0) then //All Fieldnames are listed to ensure Field order
    begin //
      Info^.FileName    := TableData.Fields[0];
      Info^.FolderID    := StrToIntDef(TableData.Fields[1], -1);
      Info^.FullPath    := TableData.Fields[2];
      Info^.LastChanged := StrToIntDef(TableData.Fields[3], 0);
      Info^.Artist      := TableData.Fields[4];
      Info^.Title       := TableData.Fields[5];
      Info^.Mp3         := TableData.Fields[6];
      Info^.Cover       := TableData.Fields[7];
      Info^.CoverID     := StrToIntDef(TableData.Fields[8], -1);
      Info^.Video       := TableData.Fields[9];
      Info^.VideoGAP    := StrToFloatDef(TableData.Fields[10], 0);
      Info^.Start       := StrToFloatDef(TableData.Fields[11], 0);
      Info^.Genre       := TableData.Fields[12];
      Info^.Edition     := TableData.Fields[13];
      Info^.Language    := TableData.Fields[14];
      Info^.Year        := TableData.Fields[15];
      Info^.Creator     := TableData.Fields[16];
      Info^.TimesPlayed := StrToIntDef(TableData.Fields[17], 0);
    end;
  except
    Result := False;
  end;
end;

Function  TDataBaseSystem.AddSong(ID: Integer; const Info: PDBSongInfo): Boolean;
var
  OldID: Integer;
begin
  Result := True;
  try
    ScoreDB.ExecSQL('INSERT INTO `us_songs` ( `ID` , `Filename` , `FolderID` , `FullPath` , `LastChanged` , `Artist` , `Title` , `Mp3` , `Cover` , `CoverID` , `Video` , `VideoGap` , `Start` , `Genre` , `Edition` , `Language` , `Year` , `Creator` , `TimesPlayed` ) ' +
                    'VALUES ( '''', '''+ Info^.FileName +''', '''+ InttoStr(Info^.FolderID) +''', '''+ Info^.FullPath +''', '''+ InttoStr(Info^.LastChanged) +''', '''+ Info^.Artist +''', '''+ Info^.Title +''', '''+ Info^.Mp3 +''', '''+ Info^.Cover +''', '''+ InttoStr(Info^.CoverID) +''', '''+ Info^.Video + ''', '''+ FloattoStr(Info^.VideoGAP) + ''', '''+ FloattoStr(Info^.Start) +''', '''+ Info^.Genre +''', '''+ Info^.Edition +''', '''+ Info^.Language +''', '''+ Info^.Year +''', '''+ Info^.Creator +''', ''0'');');

    //Version 1.0 to 2.0 Update
    If UpdateFromVer1 then
    begin //Search for Song w/ same Artist and Title in Old DB
      OldID := ScoreDB.GetTableValue('SELECT `ID` FROM `US_Songs` WHERE `Artist` = "' + Info^.Artist + '" AND `Title` = "' + Info^.Title + '"');
    end;
  except
    Result := False;
  end;
end;

Function  TDataBaseSystem.SetSongData(ID: Integer; const Info: PDBSongInfo): Boolean;
begin

end;

Function  TDataBaseSystem.GetLastChangedbyID(const Song: Integer): Integer;
begin

end;

Function  TDataBaseSystem.GetLastChangedbyFileName(const Filename: String; FolderID: Integer): Integer;
begin

end;


Function  TDataBaseSystem.GetFolderIDbyPath(Path: String): Integer;
begin

end;

Function  TDataBaseSystem.GetFolderIDbyName(const Name: String; ParentFolder: Integer): Integer;
begin

end;


Function  TDataBaseSystem.GetSongList(var List: ASongList; const Filter: ASongListFilter): Integer;
begin

end;


end.
