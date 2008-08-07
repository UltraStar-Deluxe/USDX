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
    case Typ: Byte of
      0: (Singer:       ShortString;
          Score:        Word;
          Difficulty:   Byte;
          SongArtist:   ShortString;
          SongTitle:    ShortString);

      1: (Player:       ShortString;
          AverageScore: Word);

      2: (Artist:       ShortString;
          Title:        ShortString;
          TimesSung:    Word);

      3: (ArtistName:   ShortString;
          TimesSungTot: Word);
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
    public
      property Filename: string read fFilename;

      destructor Destroy; override;

      procedure Init(const Filename: string);
      procedure ReadScore(Song: TSong);
      procedure AddScore(Song: TSong; Level: integer; const Name: WideString; Score: integer);
      procedure WriteScore(Song: TSong);

      function GetStats(var Stats: AStatResult; Typ, Count: Byte; Page: Cardinal; Reversed: Boolean): Boolean;
      function GetTotalEntrys(Typ: Byte): Cardinal;
      function GetStatReset: TDateTime;
  end;

var
  DataBase: TDataBaseSystem;

implementation

uses
  ULog,
  StrUtils,
  SysUtils;

const
  cUS_Scores = 'us_scores';
  cUS_Songs  = 'us_songs';
  cUS_Statistics_Info  = 'us_statistics_info';

//--------------------
//Create - Opens Database and Create Tables if not Exist
//--------------------

procedure TDataBaseSystem.Init(const Filename: string);
begin
  if Assigned(ScoreDB) then
    Exit;

  Log.LogStatus('Initializing database: "'+Filename+'"', 'TDataBaseSystem.Init');

  try
  
    //Open Database
    ScoreDB   := TSQLiteDatabase.Create(Filename);
    fFilename := Filename;

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
                        '[ResetTime] TEXT' +
                      ');');
      ScoreDB.ExecSQL('INSERT INTO ['+cUS_Statistics_Info+'] ' +
                      '([ResetTime]) VALUES ' +
                      '('''+DateTimeToStr(now)+''');');
    end;

  except
    on E: Exception do
    begin
      Log.LogError(E.Message, 'TDataBaseSystem.Init');
      FreeAndNil(ScoreDB);
    end;
  end;

end;

//--------------------
//Destroy - Frees Database
//--------------------
destructor TDataBaseSystem.Destroy;
begin
  Log.LogInfo('TDataBaseSystem.Free', 'TDataBaseSystem.Destroy');
  ScoreDB.Free;
  inherited;
end;

//--------------------
//ReadScore - Read Scores into SongArray
//--------------------
procedure TDataBaseSystem.ReadScore(Song: TSong);
var
  TableData: TSqliteTable;
  Difficulty: Integer;
begin
  if not Assigned(ScoreDB) then
    Exit;

  TableData := nil;

  try
    // Search Song in DB
    TableData := ScoreDB.GetTable(
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
      Difficulty := StrToIntDef(TableData.FieldAsString(TableData.FieldIndex['Difficulty']), -1);
      if ((Difficulty >= 0) and (Difficulty <= 2)) and
         (Length(Song.Score[Difficulty]) < 5) then
      begin
        SetLength(Song.Score[Difficulty], Length(Song.Score[Difficulty]) + 1);

        Song.Score[Difficulty, High(Song.Score[Difficulty])].Name  :=
            UTF8Decode(TableData.FieldAsString(TableData.FieldIndex['Player']));
        Song.Score[Difficulty, High(Song.Score[Difficulty])].Score :=
            StrtoInt(TableData.FieldAsString(TableData.FieldIndex['Score']));
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

//--------------------
//AddScore - Add one new Score to DB
//--------------------
procedure TDataBaseSystem.AddScore(Song: TSong; Level: integer; const Name: WideString; Score: integer);
var
  ID: Integer;
  TableData: TSQLiteTable;
begin
  if not Assigned(ScoreDB) then
    Exit;

  //Prevent 0 Scores from being added
  if (Score <= 0) then
    Exit;

  TableData := nil;

  try

    ID := ScoreDB.GetTableValue(
        'SELECT [ID] FROM ['+cUS_Songs+'] ' +
        'WHERE [Artist] = ? AND [Title] = ?',
        [UTF8Encode(Song.Artist), UTF8Encode(Song.Title)]);
    if ID = 0 then //Song doesn't exist -> Create
    begin
      ScoreDB.ExecSQL (
          'INSERT INTO ['+cUS_Songs+'] ' +
          '([ID], [Artist], [Title], [TimesPlayed]) VALUES ' +
          '(NULL, ?, ?, 0);',
          [UTF8Encode(Song.Artist), UTF8Encode(Song.Title)]);
      ID := ScoreDB.GetTableValue(
          'SELECT [ID] FROM ['+cUS_Songs+'] ' +
          'WHERE [Artist] = ? AND [Title] = ?',
          [UTF8Encode(Song.Artist), UTF8Encode(Song.Title)]);
      if ID = 0 then //Could not Create Table
        Exit;
    end;
    //Create new Entry
    ScoreDB.ExecSQL(
        'INSERT INTO ['+cUS_Scores+'] ' +
        '([SongID] ,[Difficulty], [Player], [Score]) VALUES ' +
        '(?, ?, ?, ?);',
        [ID, Level, UTF8Encode(Name), Score]);

    //Delete Last Position when there are more than 5 Entrys - Fixes Crash when there are > 5 ScoreEntrys
    TableData := ScoreDB.GetTable(
        'SELECT [Player], [Score] FROM ['+cUS_Scores+'] ' +
        'WHERE [SongID] = ' + InttoStr(ID) + ' AND ' +
              '[Difficulty] = ' + InttoStr(Level) +' ' +
        'ORDER BY [Score] DESC LIMIT -1 OFFSET 5');

    while not TableData.EOF do
    begin
      ScoreDB.ExecSQL(
          'DELETE FROM ['+cUS_Scores+'] ' +
          'WHERE [SongID] = ' + InttoStr(ID) + ' AND ' +
                '[Difficulty] = ' + InttoStr(Level) +' AND ' +
                '[Player] = ? AND [Score] = ?',
          [TableData.FieldAsString(TableData.FieldIndex['Player']),
           TableData.FieldAsString(TableData.FieldIndex['Score'])]);

      TableData.Next;
    end;

  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.AddScore');
  end;

  TableData.Free;
end;

//--------------------
//WriteScore - Not needed with new System; But used for Increment Played Count
//--------------------
procedure TDataBaseSystem.WriteScore(Song: TSong);
begin
  if not Assigned(ScoreDB) then
    Exit;
    
  try
    //Increase TimesPlayed
    ScoreDB.ExecSQL(
        'UPDATE ['+cUS_Songs+'] ' +
        'SET [TimesPlayed] = [TimesPlayed] + 1 ' +
        'WHERE [Title] = ? AND [Artist] = ?;',
        [UTF8Encode(Song.Title), UTF8Encode(Song.Artist)]);
  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.WriteScore');
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
function TDataBaseSystem.GetStats(var Stats: AStatResult; Typ, Count: Byte; Page: Cardinal; Reversed: Boolean): Boolean;
var
  Query: String;
  TableData: TSqliteTable;
begin
  Result := False;

  if not Assigned(ScoreDB) then
    Exit;

  if (Length(Stats) < Count) then
    Exit;

  {Todo:  Add Prevention that only Players with more than 5 Scores are Selected at Typ 2}

  //Create Query
  case Typ of
    0: begin
      Query := 'SELECT [Player], [Difficulty], [Score], [Artist], [Title] FROM ['+cUS_Scores+'] ' +
               'INNER JOIN ['+cUS_Songs+'] ON ([SongID] = [ID]) ORDER BY [Score]';
    end;
    1: begin
      Query := 'SELECT [Player], ROUND (Sum([Score]) / COUNT([Score])) FROM ['+cUS_Scores+'] ' +
               'GROUP BY [Player] ORDER BY (Sum([Score]) / COUNT([Score]))';
    end;
    2: begin
      Query := 'SELECT [Artist], [Title], [TimesPlayed] FROM ['+cUS_Songs+'] ' +
               'ORDER BY [TimesPlayed]';
    end;
    3: begin
      Query := 'SELECT [Artist], Sum([TimesPlayed]) FROM ['+cUS_Songs+'] ' +
               'GROUP BY [Artist] ORDER BY Sum([TimesPlayed])';
    end;
  end;

  //Add Order Direction
  Query := Query + IfThen(Reversed, ' ASC', ' DESC');

  //Add Limit
  Query := Query + ' LIMIT ' + InttoStr(Count * Page) + ', ' + InttoStr(Count) + ';';

  //Execute Query
  try
    TableData := ScoreDB.GetTable(Query);
  except
    on E: Exception do
    begin
      Log.LogError(E.Message, 'TDataBaseSystem.GetStats');
      Exit;
    end;
  end;

  //if Result empty -> Exit
  if (TableData.RowCount < 1) then
    Exit;

  //Copy Result to Stats Array
  while not TableData.EOF do
  begin
    Stats[TableData.Row].Typ := Typ;

    case Typ of
      0:begin
          Stats[TableData.Row].Singer := UTF8Decode(TableData.Fields[0]);

          Stats[TableData.Row].Difficulty := StrtoIntDef(TableData.Fields[1], 0);

          Stats[TableData.Row].Score := StrtoIntDef(TableData.Fields[2], 0){TableData.FieldAsInteger(2)};
          Stats[TableData.Row].SongArtist := UTF8Decode(TableData.Fields[3]);
          Stats[TableData.Row].SongTitle := UTF8Decode(TableData.Fields[4]);
        end;

        1:begin
          Stats[TableData.Row].Player := UTF8Decode(TableData.Fields[0]);
          Stats[TableData.Row].AverageScore := StrtoIntDef(TableData.Fields[1], 0);
        end;

        2:begin
          Stats[TableData.Row].Artist := UTF8Decode(TableData.Fields[0]);
          Stats[TableData.Row].Title  := UTF8Decode(TableData.Fields[1]);
          Stats[TableData.Row].TimesSung  := StrtoIntDef(TableData.Fields[2], 0);
        end;

        3:begin
          Stats[TableData.Row].ArtistName := UTF8Decode(TableData.Fields[0]);
          Stats[TableData.Row].TimesSungTot := StrtoIntDef(TableData.Fields[1], 0);
        end;

    end;

    TableData.Next;
  end;

  TableData.Free;
  Result := True;
end;

//--------------------
//GetTotalEntrys - Get Total Num of entrys for a Stats Query
//--------------------
function  TDataBaseSystem.GetTotalEntrys(Typ: Byte): Cardinal;
var
  Query: String;
begin
  Result := 0;

  if not Assigned(ScoreDB) then
    Exit;

  try
    //Create Query
    case Typ of
      0: Query := 'SELECT COUNT([SongID]) FROM ['+cUS_Scores+'];';
      1: Query := 'SELECT COUNT(DISTINCT [Player]) FROM ['+cUS_Scores+'];';
      2: Query := 'SELECT COUNT([ID]) FROM ['+cUS_Songs+'];';
      3: Query := 'SELECT COUNT(DISTINCT [Artist]) FROM ['+cUS_Songs+'];';
    end;

    Result := ScoreDB.GetTableValue(Query);
  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.GetTotalEntrys');
  end;

end;

//--------------------
//GetStatReset - Get reset date of statistic data
//--------------------
function TDataBaseSystem.GetStatReset: TDateTime;
var
  Query: string;
  TableData: TSQLiteTable;
begin
  Result := 0;

  if not Assigned(ScoreDB) then
    Exit;

  TableData := nil;

  try
    Query := 'SELECT [ResetTime] FROM ['+cUS_Statistics_Info+'];';
    TableData := ScoreDB.GetTable(Query);
    Result := StrToDateTime(TableData.Fields[0]);
  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.GetStatReset');
  end;

  TableData.Free;
end;

end.
