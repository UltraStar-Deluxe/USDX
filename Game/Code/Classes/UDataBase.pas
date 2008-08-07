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
  TStatType = (
    stBestScores,   // Best Scores
    stBestSingers,  // Best Singers
    stMostSungSong, // Most sung Songs
    stMostPopBand   // Most popular Band
  );

  TStatResult = record
    case Typ: TStatType of
      stBestScores: (
        Singer:       ShortString;
        Score:        Word;
        Difficulty:   Byte;
        SongArtist:   ShortString;
        SongTitle:    ShortString
      );
      stBestSingers: (
        Player:       ShortString;
        AverageScore: Word
      );
      stMostSungSong: (
        Artist:       ShortString;
        Title:        ShortString;
        TimesSung:    Word
      );
      stMostPopBand: (
        ArtistName:   ShortString;
        TimesSungTot: Word
      );
  end;
  AStatResult = array of TStatResult;

(*
      0: (Singer:       WideString;
          Score:        Word;
          Difficulty:   Byte;
          SongArtist:   WideString;
          SongTitle:    WideString);

      1: (Player:       WideString;
          AverageScore: Word);

      2: (Artist:       WideString;
          Title:        WideString;
          TimesSung:    Word);

      3: (ArtistName:   WideString;
          TimesSungTot: Word);
*)  

  TDataBaseSystem = class
    private
      ScoreDB: TSqliteDatabase;
      fFilename: string;

      function GetVersion(): integer;
      procedure SetVersion(Version: integer);
    public
      property Filename: string read fFilename;

      destructor Destroy; override;

      procedure Init(const Filename: string);
      procedure ReadScore(Song: TSong);
      procedure AddScore(Song: TSong; Level: integer; const Name: WideString; Score: integer);
      procedure WriteScore(Song: TSong);

      function GetStats(var Stats: AStatResult; Typ: TStatType; Count: Byte; Page: Cardinal; Reversed: Boolean): Boolean;
      function GetTotalEntrys(Typ: TStatType): Cardinal;
      function GetStatReset: TDateTime;
  end;

var
  DataBase: TDataBaseSystem;

implementation

uses
  ULog,
  DateUtils,
  StrUtils,
  SysUtils;

const
  cDBVersion = 01; // 0.1
  cUS_Scores = 'us_scores';
  cUS_Songs  = 'us_songs';
  cUS_Statistics_Info  = 'us_statistics_info';

(**
 * Opens Database and Create Tables if not Exist
 *)
procedure TDataBaseSystem.Init(const Filename: string);
var
  Version: integer;
begin
  if Assigned(ScoreDB) then
    Exit;

  Log.LogStatus('Initializing database: "'+Filename+'"', 'TDataBaseSystem.Init');

  try
  
    // Open Database
    ScoreDB   := TSQLiteDatabase.Create(Filename);
    fFilename := Filename;

    // Close and delete outdated file
    Version := GetVersion();
    if ((Version <> 0) and (Version <> cDBVersion)) then
    begin
      Log.LogInfo('Outdated cover-database file found', 'TDataBaseSystem.Init');
      // Close and delete outdated file
      ScoreDB.Free;
      if (not DeleteFile(Filename)) then
        raise Exception.Create('Could not delete ' + Filename);
      // Reopen
      ScoreDB := TSQLiteDatabase.Create(Filename);
      Version := 0;
    end;
    
    // Set version number after creation
    if (Version = 0) then
      SetVersion(cDBVersion);
    

    // SQLite does not handle VARCHAR(n) or INT(n) as expected.
    // Texts do not have a restricted length, no matter which type is used,
    // so use the native TEXT type. INT(n) is always INTEGER.
    // In addition, SQLiteTable3 will fail if other types than the native SQLite
    // types are used (especially FieldAsInteger). Also take care to write the
    // types in upper-case letters although SQLite does not care about this -
    // SQLiteTable3 is very sensitive in this regard.
     
    ScoreDB.ExecSQL('CREATE TABLE IF NOT EXISTS ['+cUS_Scores+'] (' +
                      '[SongID] INTEGER NOT NULL, ' +
                      '[Difficulty] INTEGER NOT NULL, ' +
                      '[Player] TEXT NOT NULL, ' +
                      '[Score] INTEGER NOT NULL' +
                    ');');

    ScoreDB.ExecSQL('CREATE TABLE IF NOT EXISTS ['+cUS_Songs+'] (' +
                      '[ID] INTEGER PRIMARY KEY, ' +
                      '[Artist] TEXT NOT NULL, ' +
                      '[Title] TEXT NOT NULL, ' +
                      '[TimesPlayed] INTEGER NOT NULL' +
                    ');');

    if not ScoreDB.TableExists(cUS_Statistics_Info) then
    begin
      ScoreDB.ExecSQL('CREATE TABLE IF NOT EXISTS ['+cUS_Statistics_Info+'] (' +
                        '[ResetTime] INTEGER' +
                      ');');
      // insert creation timestamp
      ScoreDB.ExecSQL(Format('INSERT INTO ['+cUS_Statistics_Info+'] ' +
                            '([ResetTime]) VALUES(%d);',
                            [DateTimeToUnix(Now())]));
    end;

  except
    on E: Exception do
    begin
      Log.LogError(E.Message, 'TDataBaseSystem.Init');
      FreeAndNil(ScoreDB);
    end;
  end;

end;

(**
 * Frees Database
 *)
destructor TDataBaseSystem.Destroy;
begin
  Log.LogInfo('TDataBaseSystem.Free', 'TDataBaseSystem.Destroy');
  ScoreDB.Free;
  inherited;
end;

(**
 * Read Scores into SongArray
 *)
procedure TDataBaseSystem.ReadScore(Song: TSong);
var
  TableData: TSQLiteUniTable;
  Difficulty: Integer;
begin
  if not Assigned(ScoreDB) then
    Exit;

  TableData := nil;

  try
    // Search Song in DB
    TableData := ScoreDB.GetUniTable(
      'SELECT [Difficulty], [Player], [Score] FROM ['+cUS_Scores+'] ' +
      'WHERE [SongID] = (' +
        'SELECT [ID] FROM ['+cUS_Songs+'] ' +
        'WHERE [Artist] = ? AND [Title] = ? ' +
        'LIMIT 1) ' +
      'ORDER BY [Score] DESC  LIMIT 15',
      [UTF8Encode(Song.Artist), UTF8Encode(Song.Title)]);

    // Empty Old Scores
    SetLength(Song.Score[0], 0);
    SetLength(Song.Score[1], 0);
    SetLength(Song.Score[2], 0);

    // Go through all Entrys
    while (not TableData.EOF) do
    begin
      // Add one Entry to Array
      Difficulty := TableData.FieldAsInteger(TableData.FieldIndex['Difficulty']);
      if ((Difficulty >= 0) and (Difficulty <= 2)) and
         (Length(Song.Score[Difficulty]) < 5) then
      begin
        SetLength(Song.Score[Difficulty], Length(Song.Score[Difficulty]) + 1);

        Song.Score[Difficulty, High(Song.Score[Difficulty])].Name  :=
            UTF8Decode(TableData.FieldByName['Player']);
        Song.Score[Difficulty, High(Song.Score[Difficulty])].Score :=
            TableData.FieldAsInteger(TableData.FieldIndex['Score']);
      end;

      TableData.Next;
    end; // while

  except
    for Difficulty := 0 to 2 do
    begin
      SetLength(Song.Score[Difficulty], 1);
      Song.Score[Difficulty, 1].Name := 'Error Reading ScoreDB';
    end;
  end;

  TableData.Free;
end;

(**
 * Adds one new score to DB
 *)
procedure TDataBaseSystem.AddScore(Song: TSong; Level: integer; const Name: WideString; Score: integer);
var
  ID: Integer;
  TableData: TSQLiteTable;
begin
  if not Assigned(ScoreDB) then
    Exit;

  // Prevent 0 Scores from being added
  if (Score <= 0) then
    Exit;

  TableData := nil;

  try

    ID := ScoreDB.GetTableValue(
        'SELECT [ID] FROM ['+cUS_Songs+'] ' +
        'WHERE [Artist] = ? AND [Title] = ?',
        [UTF8Encode(Song.Artist), UTF8Encode(Song.Title)]);
    if (ID = 0) then
    begin
      // Create song if it does not exist
      ScoreDB.ExecSQL(
          'INSERT INTO ['+cUS_Songs+'] ' +
          '([ID], [Artist], [Title], [TimesPlayed]) VALUES ' +
          '(NULL, ?, ?, 0);',
          [UTF8Encode(Song.Artist), UTF8Encode(Song.Title)]);
      // Get song-ID
      ID := ScoreDB.GetLastInsertRowID();
    end;
    // Create new entry
    ScoreDB.ExecSQL(
        'INSERT INTO ['+cUS_Scores+'] ' +
        '([SongID] ,[Difficulty], [Player], [Score]) VALUES ' +
        '(?, ?, ?, ?);',
        [ID, Level, UTF8Encode(Name), Score]);

    // Delete last position when there are more than 5 entrys.
    // Fixes crash when there are > 5 ScoreEntrys
    // Note: GetUniTable is not applicable here, as the results are used while
    // table entries are deleted.
    TableData := ScoreDB.GetTable(
        'SELECT [Player], [Score] FROM ['+cUS_Scores+'] ' +
        'WHERE [SongID] = ' + InttoStr(ID) + ' AND ' +
              '[Difficulty] = ' + InttoStr(Level) +' ' +
        'ORDER BY [Score] DESC LIMIT -1 OFFSET 5');

    while (not TableData.EOF) do
    begin
      // Note: Score is an int-value, so in contrast to Player, we do not bind
      // this value. Otherwise we had to convert the string to an int to avoid
      // an automatic cast of this field to the TEXT type (although it might even
      // work that way).
      ScoreDB.ExecSQL(
          'DELETE FROM ['+cUS_Scores+'] ' +
          'WHERE [SongID] = ' + InttoStr(ID) + ' AND ' +
                '[Difficulty] = ' + InttoStr(Level) +' AND ' +
                '[Player] = ? AND ' +
                '[Score] = ' + TableData.FieldByName['Score'],
          [TableData.FieldByName['Player']]);

      TableData.Next;
    end;

  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.AddScore');
  end;

  TableData.Free;
end;

(**
 * Not needed with new System.
 * Used for increment played count
 *)
procedure TDataBaseSystem.WriteScore(Song: TSong);
begin
  if not Assigned(ScoreDB) then
    Exit;
    
  try
    // Increase TimesPlayed
    ScoreDB.ExecSQL(
        'UPDATE ['+cUS_Songs+'] ' +
        'SET [TimesPlayed] = [TimesPlayed] + 1 ' +
        'WHERE [Title] = ? AND [Artist] = ?;',
        [UTF8Encode(Song.Title), UTF8Encode(Song.Artist)]);
  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.WriteScore');
  end;
end;

(**
 * Writes some stats to array.
 * Returns true if choosen page has entrys
 *)
function TDataBaseSystem.GetStats(var Stats: AStatResult; Typ: TStatType; Count: Byte; Page: Cardinal; Reversed: Boolean): Boolean;
var
  Query: String;
  TableData: TSQLiteUniTable;
begin
  Result := False;

  if not Assigned(ScoreDB) then
    Exit;

  if (Length(Stats) < Count) then
    Exit;

  {Todo:  Add Prevention that only players with more than 5 scores are selected at type 2}

  // Create query
  case Typ of
    stBestScores: begin
      Query := 'SELECT [Player], [Difficulty], [Score], [Artist], [Title] FROM ['+cUS_Scores+'] ' +
               'INNER JOIN ['+cUS_Songs+'] ON ([SongID] = [ID]) ORDER BY [Score]';
    end;
    stBestSingers: begin
      Query := 'SELECT [Player], ROUND(AVG([Score])) FROM ['+cUS_Scores+'] ' +
               'GROUP BY [Player] ORDER BY AVG([Score])';
    end;
    stMostSungSong: begin
      Query := 'SELECT [Artist], [Title], [TimesPlayed] FROM ['+cUS_Songs+'] ' +
               'ORDER BY [TimesPlayed]';
    end;
    stMostPopBand: begin
      Query := 'SELECT [Artist], SUM([TimesPlayed]) FROM ['+cUS_Songs+'] ' +
               'GROUP BY [Artist] ORDER BY SUM([TimesPlayed])';
    end;
  end;

  // Add order direction
  Query := Query + IfThen(Reversed, ' ASC', ' DESC');

  // Add limit
  Query := Query + ' LIMIT ' + InttoStr(Count * Page) + ', ' + InttoStr(Count) + ';';

  // Execute query
  try
    TableData := ScoreDB.GetUniTable(Query);
  except
    on E: Exception do
    begin
      Log.LogError(E.Message, 'TDataBaseSystem.GetStats');
      Exit;
    end;
  end;

  if (TableData.EOF) then
  begin
    TableData.Free;
    Exit;
  end;

  // Copy result to stats array
  while not TableData.EOF do
  begin
    Stats[TableData.Row].Typ := Typ;

    case Typ of
      stBestScores: begin
        Stats[TableData.Row].Singer := UTF8Decode(TableData.Fields[0]);
        Stats[TableData.Row].Difficulty := TableData.FieldAsInteger(1);
        Stats[TableData.Row].Score := TableData.FieldAsInteger(2);
        Stats[TableData.Row].SongArtist := UTF8Decode(TableData.Fields[3]);
        Stats[TableData.Row].SongTitle := UTF8Decode(TableData.Fields[4]);
      end;
      stBestSingers: begin
        Stats[TableData.Row].Player := UTF8Decode(TableData.Fields[0]);
        Stats[TableData.Row].AverageScore := TableData.FieldAsInteger(1);
      end;
      stMostSungSong: begin
        Stats[TableData.Row].Artist := UTF8Decode(TableData.Fields[0]);
        Stats[TableData.Row].Title  := UTF8Decode(TableData.Fields[1]);
        Stats[TableData.Row].TimesSung  := TableData.FieldAsInteger(2);
      end;
      stMostPopBand: begin
        Stats[TableData.Row].ArtistName := UTF8Decode(TableData.Fields[0]);
        Stats[TableData.Row].TimesSungTot := TableData.FieldAsInteger(1);
      end;
    end;

    TableData.Next;
  end;

  TableData.Free;
  Result := True;
end;

(**
 * Gets total number of entrys for a stats query
 *)
function  TDataBaseSystem.GetTotalEntrys(Typ: TStatType): Cardinal;
var
  Query: String;
begin
  Result := 0;

  if not Assigned(ScoreDB) then
    Exit;

  try
    // Create query
    case Typ of
      stBestScores:
        Query := 'SELECT COUNT([SongID]) FROM ['+cUS_Scores+'];';
      stBestSingers:
        Query := 'SELECT COUNT(DISTINCT [Player]) FROM ['+cUS_Scores+'];';
      stMostSungSong:
        Query := 'SELECT COUNT([ID]) FROM ['+cUS_Songs+'];';
      stMostPopBand:
        Query := 'SELECT COUNT(DISTINCT [Artist]) FROM ['+cUS_Songs+'];';
    end;

    Result := ScoreDB.GetTableValue(Query);
  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.GetTotalEntrys');
  end;

end;

(**
 * Gets reset date of statistic data
 *)
function TDataBaseSystem.GetStatReset: TDateTime;
var
  Query: string;
  ResetTime: int64;
begin
  Result := 0;

  if not Assigned(ScoreDB) then
    Exit;

  try
    Query := 'SELECT [ResetTime] FROM ['+cUS_Statistics_Info+'];';
    Result := UnixToDateTime(ScoreDB.GetTableValue(Query));
  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.GetStatReset');
  end;
end;

function TDataBaseSystem.GetVersion(): integer;
begin
  Result := ScoreDB.GetTableValue('PRAGMA user_version');
end;

procedure TDataBaseSystem.SetVersion(Version: integer);
begin
  ScoreDB.ExecSQL(Format('PRAGMA user_version = %d', [Version]));
end;

end.
