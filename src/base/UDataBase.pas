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

unit UDataBase;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
  SQLiteTable3,
  UPath,
  USong,
  USongs,
  UTextEncoding;

//--------------------
//DataBaseSystem - Class including all DB methods
//--------------------
type
  TStatType = (
    stBestScores,   // Best scores
    stBestSingers,  // Best singers
    stMostSungSong, // Most sung songs
    stMostPopBand   // Most popular band
  );

  // abstract super-class for statistic results
  TStatResult = class
    public
      Typ: TStatType;
  end;

  TStatResultBestScores = class(TStatResult)
    public
      Singer:       UTF8String;
      Score:        word;
      Difficulty:   byte;
      SongArtist:   UTF8String;
      SongTitle:    UTF8String;
      Date:         UTF8String;
  end;

  TStatResultBestSingers = class(TStatResult)
    public
      Player:       UTF8String;
      AverageScore: word;
  end;

  TStatResultMostSungSong = class(TStatResult)
    public
      Artist:       UTF8String;
      Title:        UTF8String;
      TimesSung:    word;
  end;

  TStatResultMostPopBand = class(TStatResult)
    public
      ArtistName:   UTF8String;
      TimesSungTot: word;
  end;


  TDataBaseSystem = class
    private
      ScoreDB:      TSQLiteDatabase;
      fFilename: IPath;

      function GetVersion(): integer;
      procedure SetVersion(Version: integer);
    public
      property Filename: IPath read fFilename;

      destructor Destroy; override;

      procedure Init(const Filename: IPath);
      procedure ConvertFrom101To110();
      procedure ReadScore(Song: TSong);
      procedure AddScore(Song: TSong; Level: integer; const Name: UTF8String; Score: integer);
      procedure WriteScore(Song: TSong);

      function GetStats(Typ: TStatType; Count: byte; Page: cardinal; Reversed: boolean): TList;
      procedure FreeStats(StatList: TList);
      function GetTotalEntrys(Typ: TStatType): cardinal;
      function GetStatReset: TDateTime;
      function FormatDate(time_stamp: integer): UTF8String;
  end;

var
  DataBase: TDataBaseSystem;

implementation

uses
  DateUtils,
  ULanguage,
  StrUtils,
  SysUtils,
  ULog;

{
 cDBVersion - history
 0 = USDX 1.01 or no Database
 01 = USDX 1.1
}
const
  cDBVersion = 01; // 0.1
  cUS_Scores = 'us_scores';
  cUS_Songs  = 'us_songs';
  cUS_Statistics_Info = 'us_statistics_info';

(**
 * Open database and create tables if they do not exist
 *)
procedure TDataBaseSystem.Init(const Filename: IPath);
var
  Version:            integer;
  finalizeConversion: boolean;
begin
  if Assigned(ScoreDB) then
    Exit;

  Log.LogStatus('Initializing database: "' + Filename.ToNative + '"', 'TDataBaseSystem.Init');

  try

    // open database
    ScoreDB   := TSQLiteDatabase.Create(Filename.ToUTF8);
    fFilename := Filename;

    Version := GetVersion();

    // add Table cUS_Statistics_Info
    // needed in the conversion from 1.01 to 1.1
    if not ScoreDB.TableExists(cUS_Statistics_Info) then
    begin
      Log.LogInfo('Outdated song database found - missing table"' + cUS_Statistics_Info + '"', 'TDataBaseSystem.Init');
      ScoreDB.ExecSQL('CREATE TABLE IF NOT EXISTS [' + cUS_Statistics_Info + '] (' +
                      '[ResetTime] INTEGER' +
                      ');');
      // insert creation timestamp
      ScoreDB.ExecSQL(Format('INSERT INTO [' + cUS_Statistics_Info + '] ' +
                             '([ResetTime]) VALUES(%d);',
                             [DateTimeToUnix(Now())]));
    end;

    // convert data from 1.01 to 1.1
    // part #1 - prearrangement
    finalizeConversion := false;
    if (Version = 0) AND ScoreDB.TableExists('US_Scores') then
    begin
      // rename old tables - to be able to insert new table structures
      ScoreDB.ExecSQL('ALTER TABLE US_Scores RENAME TO us_scores_101;');
      ScoreDB.ExecSQL('ALTER TABLE US_Songs RENAME TO us_songs_101;');
      finalizeConversion := true; // means: conversion has to be done!
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
    ScoreDB.ExecSQL('CREATE TABLE IF NOT EXISTS [' + cUS_Scores + '] (' +
                      '[SongID] INTEGER NOT NULL, ' +
                      '[Difficulty] INTEGER NOT NULL, ' +
                      '[Player] TEXT NOT NULL, ' +
                      '[Score] INTEGER NOT NULL, ' +
                      '[Date] INTEGER NULL' +
                    ');');

    ScoreDB.ExecSQL('CREATE TABLE IF NOT EXISTS [' + cUS_Songs + '] (' +
                      '[ID] INTEGER PRIMARY KEY, ' +
                      '[Artist] TEXT NOT NULL, ' +
                      '[Title] TEXT NOT NULL, ' +
                      '[TimesPlayed] INTEGER NOT NULL, ' +
                      '[Rating] INTEGER NULL' +
                    ');');

    //add column date to cUS-Scores
    if not ScoreDB.ContainsColumn(cUS_Scores, 'Date') then
    begin
      Log.LogInfo('adding column date to "' + cUS_Scores + '"', 'TDataBaseSystem.Init');
      ScoreDB.ExecSQL('ALTER TABLE ' + cUS_Scores + ' ADD COLUMN [Date] INTEGER NULL');
    end;

    // add column rating to cUS_Songs
    // just for users of nightly builds and developers!
    if not ScoreDB.ContainsColumn(cUS_Songs, 'Rating') then
    begin
      Log.LogInfo('Outdated song database found - adding column rating to "' + cUS_Songs + '"', 'TDataBaseSystem.Init');
      ScoreDB.ExecSQL('ALTER TABLE ' + cUS_Songs + ' ADD COLUMN [Rating] INTEGER NULL');
    end;

    // convert data from previous versions
    // part #2 - accomplishment
    if finalizeConversion then
    begin
      //convert data from 1.01 to 1.1
      if ScoreDB.TableExists('us_scores_101') then
        ConvertFrom101To110();
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
 * Convert Database from 1.01 to 1.1
 *)
procedure TDataBaseSystem.ConvertFrom101To110();
var
  TableData:      TSQLiteUniTable;
  tempUTF8String: UTF8String;
begin
  if not ScoreDB.ContainsColumn('us_scores_101', 'Date') then
  begin
    Log.LogInfo(
      'Outdated song database found - ' +
      'begin conversion from V1.01 to V1.1', 'TDataBaseSystem.Convert101To110');

    // insert old values into new db-schemes (/tables)
    ScoreDB.ExecSQL(
      'INSERT INTO ' + cUS_Scores +
      ' SELECT  SongID, Difficulty, Player, Score FROM us_scores_101;');
  end else
  begin
    Log.LogInfo(
      'Outdated song database  found - ' +
      'begin conversion from V1.01 Challenge Mod to V1.1', 'TDataBaseSystem.Convert101To110');

    // insert old values into new db-schemes (/tables)
    ScoreDB.ExecSQL(
      'INSERT INTO ' + cUS_Scores +
      ' SELECT  SongID, Difficulty, Player, Score, Date FROM us_scores_101;');
  end;

    ScoreDB.ExecSQL(
      'INSERT INTO ' + cUS_Songs +
      ' SELECT  ID, Artist, Title, TimesPlayed, ''NULL'' FROM us_songs_101;');

    // now we have to convert all the texts for unicode support:

    // player names
    TableData := nil;
    try
      TableData := ScoreDB.GetUniTable(
        'SELECT [rowid], [Player] ' +
        'FROM [' + cUS_Scores + '];');

      // Go through all Entrys
      while (not TableData.EOF) do
      begin
        // Convert name into UTF8 and alter all entrys
        DecodeStringUTF8(TableData.FieldByName['Player'], tempUTF8String, encCP1252);
        ScoreDB.ExecSQL(
          'UPDATE [' + cUS_Scores + '] ' +
          'SET [Player] = ? ' +
          'WHERE [rowid] = ? ',
          [tempUTF8String,
          TableData.FieldAsInteger(TableData.FieldIndex['rowid'])]);

        TableData.Next;
      end; // while

    except
      on E: Exception do
        Log.LogError(E.Message, 'TDataBaseSystem.Convert101To110');
    end;

    TableData.Free;

    // song artist and song title
    TableData := nil;
    try
      TableData := ScoreDB.GetUniTable(
        'SELECT [ID], [Artist], [Title] ' +
        'FROM [' + cUS_Songs + '];');

      // Go through all Entrys
      while (not TableData.EOF) do
      begin
        // Convert Artist into UTF8 and alter all entrys
        DecodeStringUTF8(TableData.FieldByName['Artist'], tempUTF8String, encCP1252);
        //Log.LogError(TableData.FieldByName['Artist']+' -> '+tempUTF8String+' (encCP1252)');
        ScoreDB.ExecSQL(
          'UPDATE [' + cUS_Songs + '] ' +
          'SET [Artist] = ? ' +
          'WHERE [ID] = ?',
          [tempUTF8String,
          TableData.FieldAsInteger(TableData.FieldIndex['ID'])]);

        // Convert Title into UTF8 and alter all entrys
        DecodeStringUTF8(TableData.FieldByName['Title'], tempUTF8String, encCP1252);
        ScoreDB.ExecSQL(
          'UPDATE [' + cUS_Songs + '] ' +
          'SET [Title] = ? ' +
          'WHERE [ID] = ? ',
          [tempUTF8String,
          TableData.FieldAsInteger(TableData.FieldIndex['ID'])]);

        TableData.Next;
      end; // while

    except
      on E: Exception do
        Log.LogError(E.Message, 'TDataBaseSystem.Convert101To110');
    end;

    TableData.Free;

    //now drop old tables
    ScoreDB.ExecSQL('DROP TABLE us_scores_101;');
    ScoreDB.ExecSQL('DROP TABLE us_songs_101;');
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
 * Format a UNIX-Timestamp into DATE (If 0 then '')
 *)
function TDataBaseSystem.FormatDate(time_stamp: integer): UTF8String;
var
  Year, Month, Day: word;
begin
  Result:='';
  try
    if time_stamp<>0 then
    begin
      DecodeDate(UnixToDateTime(time_stamp), Year, Month, Day);
      Result := Format(Language.Translate('STAT_FORMAT_DATE'), [Day, Month, Year]);
    end;
  except
    on E: EConvertError do
    Log.LogError('Error Parsing FormatString "STAT_FORMAT_DATE": ' + E.Message);
  end;
end;


(**
 * Read Scores into SongArray
 *)
procedure TDataBaseSystem.ReadScore(Song: TSong);
var
  TableData:  TSQLiteUniTable;
  Difficulty: integer;
  I: integer;
  PlayerListed: boolean;
begin
  if not Assigned(ScoreDB) then
    Exit;

  TableData := nil;
  try
    // Search Song in DB
    TableData := ScoreDB.GetUniTable(
      'SELECT [Difficulty], [Player], [Score], [Date] FROM [' + cUS_Scores + '] ' +
      'WHERE [SongID] = (' +
        'SELECT [ID] FROM [' + cUS_Songs + '] ' +
        'WHERE [Artist] = ? AND [Title] = ? ' +
        'LIMIT 1) ' +
      'ORDER BY [Score] DESC;', //no LIMIT! see filter below!
      [Song.Artist, Song.Title]);

    // Empty Old Scores
    SetLength(Song.Score[0], 0); //easy
    SetLength(Song.Score[1], 0); //medium
    SetLength(Song.Score[2], 0); //hard

    // Go through all Entrys
    while (not TableData.EOF) do
    begin
      // Add one Entry to Array
      Difficulty := TableData.FieldAsInteger(TableData.FieldIndex['Difficulty']);
      if ((Difficulty >= 0) and (Difficulty <= 2)) and
         (Length(Song.Score[Difficulty]) < 5) then
      begin
        //filter player
        PlayerListed:=false;
        if (Length(Song.Score[Difficulty])>0) then
        begin
          for I := 0 to Length(Song.Score[Difficulty]) - 1 do
          begin
            if (Song.Score[Difficulty, I].Name = TableData.FieldByName['Player']) then
            begin
              PlayerListed:=true;
              break;
            end;
          end;
        end;

        if not PlayerListed then
        begin
          SetLength(Song.Score[Difficulty], Length(Song.Score[Difficulty]) + 1);

          Song.Score[Difficulty, High(Song.Score[Difficulty])].Name  :=
            TableData.FieldByName['Player'];
          Song.Score[Difficulty, High(Song.Score[Difficulty])].Score :=
            TableData.FieldAsInteger(TableData.FieldIndex['Score']);
          Song.Score[Difficulty, High(Song.Score[Difficulty])].Date :=
            FormatDate(TableData.FieldAsInteger(TableData.FieldIndex['Date']));
        end;
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
procedure TDataBaseSystem.AddScore(Song: TSong; Level: integer; const Name: UTF8String; Score: integer);
var
  ID:        integer;
  TableData: TSQLiteTable;
begin
  if not Assigned(ScoreDB) then
    Exit;

  // Prevent 0 Scores from being added EDIT: ==> UScreenTop5.pas!
  //if (Score <= 0) then
  //  Exit;

  TableData := nil;

  try

    ID := ScoreDB.GetTableValue(
        'SELECT [ID] FROM [' + cUS_Songs + '] ' +
        'WHERE [Artist] = ? AND [Title] = ?',
        [Song.Artist, Song.Title]);
    if (ID = 0) then
    begin
      // Create song if it does not exist
      ScoreDB.ExecSQL(
          'INSERT INTO [' + cUS_Songs + '] ' +
          '([ID], [Artist], [Title], [TimesPlayed]) VALUES ' +
          '(NULL, ?, ?, 0);',
          [Song.Artist, Song.Title]);
      // Get song-ID
      ID := ScoreDB.GetLastInsertRowID();
    end;
    // Create new entry
    ScoreDB.ExecSQL(
      'INSERT INTO [' + cUS_Scores + '] ' +
      '([SongID] ,[Difficulty], [Player], [Score], [Date]) VALUES ' +
      '(?, ?, ?, ?, ?);',
      [ID, Level, Name, Score, DateTimeToUnix(Now())]);

  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.AddScore');
  end;

  TableData.Free;
end;

(**
 * Not needed with new system.
 * Used to increment played count
 *)
procedure TDataBaseSystem.WriteScore(Song: TSong);
begin
  if not Assigned(ScoreDB) then
    Exit;

  try
    // Increase TimesPlayed
    ScoreDB.ExecSQL(
        'UPDATE [' + cUS_Songs + '] ' +
        'SET [TimesPlayed] = [TimesPlayed] + 1 ' +
        'WHERE [Title] = ? AND [Artist] = ?;',
        [Song.Title, Song.Artist]);
  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.WriteScore');
  end;
end;

(**
 * Writes some stats to array.
 * Returns nil if the database is not ready or a list with zero or more statistic
 * entries.
 * Free the result-list with FreeStats() after usage to avoid memory leaks.
 *)
function TDataBaseSystem.GetStats(Typ: TStatType; Count: byte; Page: cardinal; Reversed: boolean): TList;
var
  Query:     string;
  TableData: TSQLiteUniTable;
  Stat:      TStatResult;
begin
  Result := nil;

  if not Assigned(ScoreDB) then
    Exit;

  {Todo:  Add Prevention that only players with more than 5 scores are selected at type 2}

  // Create query
  case Typ of
    stBestScores: begin
      Query := 'SELECT [Player], [Difficulty], [Score], [Artist], [Title], [Date] FROM [' + cUS_Scores + '] ' +
               'INNER JOIN [' + cUS_Songs + '] ON ([SongID] = [ID]) ORDER BY [Score]';
    end;
    stBestSingers: begin
      Query := 'SELECT [Player], ROUND(AVG([Score])) FROM [' + cUS_Scores + '] ' +
               'GROUP BY [Player] ORDER BY AVG([Score])';
    end;
    stMostSungSong: begin
      Query := 'SELECT [Artist], [Title], [TimesPlayed] FROM [' + cUS_Songs + '] ' +
               'ORDER BY [TimesPlayed]';
    end;
    stMostPopBand: begin
      Query := 'SELECT [Artist], SUM([TimesPlayed]) FROM [' + cUS_Songs + '] ' +
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

  Result := TList.Create;
  Stat := nil;

  // Copy result to stats array
  while not TableData.EOF do
  begin
    case Typ of
      stBestScores: begin
        Stat := TStatResultBestScores.Create;
        with TStatResultBestScores(Stat) do
        begin
          Singer := TableData.Fields[0];
          Difficulty := TableData.FieldAsInteger(1);
          Score := TableData.FieldAsInteger(2);
          SongArtist := TableData.Fields[3];
          SongTitle := TableData.Fields[4];
          Date := FormatDate(TableData.FieldAsInteger(5));
        end;
      end;
      stBestSingers: begin
        Stat := TStatResultBestSingers.Create;
        with TStatResultBestSingers(Stat) do
        begin
          Player := TableData.Fields[0];
          AverageScore := TableData.FieldAsInteger(1);
        end;
      end;
      stMostSungSong: begin
        Stat := TStatResultMostSungSong.Create;
        with TStatResultMostSungSong(Stat) do
        begin
          Artist := TableData.Fields[0];
          Title  := TableData.Fields[1];
          TimesSung  := TableData.FieldAsInteger(2);
        end;
      end;
      stMostPopBand: begin
        Stat := TStatResultMostPopBand.Create;
        with TStatResultMostPopBand(Stat) do
        begin
          ArtistName := TableData.Fields[0];
          TimesSungTot := TableData.FieldAsInteger(1);
        end;
      end
      else
        Log.LogCritical('Unknown stat-type', 'TDataBaseSystem.GetStats');
    end;

    Stat.Typ := Typ;
    Result.Add(Stat);

    TableData.Next;
  end;

  TableData.Free;
end;

procedure TDataBaseSystem.FreeStats(StatList: TList);
var
  Index: integer;
begin
  if (StatList = nil) then
    Exit;
  for Index := 0 to StatList.Count-1 do
    TStatResult(StatList[Index]).Free;
  StatList.Free;
end;

(**
 * Gets total number of entrys for a stats query
 *)
function TDataBaseSystem.GetTotalEntrys(Typ: TStatType): cardinal;
var
  Query: string;
begin
  Result := 0;

  if not Assigned(ScoreDB) then
    Exit;

  try
    // Create query
    case Typ of
      stBestScores:
        Query := 'SELECT COUNT([SongID]) FROM [' + cUS_Scores + '];';
      stBestSingers:
        Query := 'SELECT COUNT(DISTINCT [Player]) FROM [' + cUS_Scores + '];';
      stMostSungSong:
        Query := 'SELECT COUNT([ID]) FROM [' + cUS_Songs + '];';
      stMostPopBand:
        Query := 'SELECT COUNT(DISTINCT [Artist]) FROM [' + cUS_Songs + '];';
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
begin
  Result := 0;

  if not Assigned(ScoreDB) then
    Exit;

  try
    Query := 'SELECT [ResetTime] FROM [' + cUS_Statistics_Info + '];';
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
