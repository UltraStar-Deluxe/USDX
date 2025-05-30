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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/base/UDataBase.pas $
 * $Id: UDataBase.pas 2574 2010-07-07 19:52:32Z brunzelchen $
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
  UDllManager,
  UIni,
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

  PUserInfo = ^TUserInfo;
  TUserInfo = record
    Username:     UTF8String;
    Password:     UTF8String;
    SendSavePlayer:   integer;
    AutoMode:      integer;
    AutoPlayer:      integer;
    AutoScoreEasy:   integer;
    AutoScoreMedium: integer;
    AutoScoreHard:   integer;
    Save:                boolean;
  end;

  PNetworkUser = ^TNetworkUser;
  TNetworkUser = record
    Website:  UTF8String;
    UserList: array of TUserInfo;
  end;

  TDataBaseSystem = class
    private
      ScoreDB:      TSQLiteDatabase;
      fFilename: IPath;

      function GetVersion(): integer;
      procedure SetVersion(Version: integer);
    public
      // Network
      NetworkUser: array of TNetworkUser;

      property Filename: IPath read fFilename;

      destructor Destroy; override;

      procedure Init(const Filename: IPath);
      procedure ConvertFrom101To110();
      procedure ReadScore(Song: TSong);
      procedure AddScore(Song: TSong; Level: integer; const Name: UTF8String; Score: integer);
      procedure WriteScore(Song: TSong);

      procedure ReadUsers;
      procedure UpdateUsers;
      procedure DeleteUser(Website: UTF8String; Username: UTF8String);
      procedure NewUser(Website: UTF8String; Username, Password: UTF8String);

      procedure AddWebsite;

      procedure AddSong(Song: TSong);

      procedure AddMax_Score (Song: TSong; WebID: integer; Receive_Max_Score: integer; Level: integer);
      procedure AddMedia_Score (Song: TSong; WebID: integer; Receive_Media_Score: integer; Level: integer);
      procedure AddUser_Score (Song: TSong; WebID: integer; Receive_User_Score: string; Level: integer);

      function ReadMax_Score(Artist, Title: UTF8String; WebID, Level: integer): integer;
      function ReadMedia_Score(Artist, Title: UTF8String; WebID, Level: integer): integer;
      function ReadUser_Score(Artist, Title: UTF8String; WebID, Level: integer): string;

      function ReadMax_ScoreLocal(Artist, Title: UTF8String; Level: integer): integer;
      function ReadMedia_ScoreLocal(Artist, Title: UTF8String; Level: integer): integer;
      function ReadUser_ScoreLocal(Artist, Title: UTF8String; Level: integer): string;

      function Delete_Score(Song: TSong; WebID: integer): integer;

      function GetStats(Typ: TStatType; Count: byte; Page: cardinal; Reversed: boolean): TList;
      procedure FreeStats(StatList: TList);
      function GetTotalEntrys(Typ: TStatType): cardinal;
      function GetStatReset: TDateTime;
      function FormatDate(time_stamp: integer): UTF8String;

      procedure SaveSongOptions(Song: TSong; Options: TSongOptions);
      function GetSongOptions(Song: TSong): TSongOptions;
  end;

var
  DataBase: TDataBaseSystem;

implementation

uses
  DateUtils,
  ULanguage,
  UUnicodeUtils,
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
  cUS_Users_Info = 'us_users_info';
  cUS_Webs = 'us_webs';
  cUS_Webs_Stats = 'us_webs_stats';

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
                      '[Rating] INTEGER NULL, ' +
                      '[VideoRatioAspect] INTEGER NULL, ' +
                      '[VideoWidth] INTEGER NULL, ' +
                      '[VideoHeight] INTEGER NULL, ' +
                      '[LyricPosition] INTEGER NULL, ' +
                      '[LyricAlpha] INTEGER NULL, ' +
                      '[LyricSingFillColor] TEXT NULL, ' +
                      '[LyricActualFillColor] TEXT NULL, ' +
                      '[LyricNextFillColor] TEXT NULL, ' +
                      '[LyricSingOutlineColor] TEXT NULL, ' +
                      '[LyricActualOutlineColor] TEXT NULL, ' +
                      '[LyricNextOutlineColor] TEXT NULL' +
                    ');');

    ScoreDB.ExecSQL('CREATE TABLE IF NOT EXISTS [' + cUS_Webs + '] (' +
                      '[ID] INTEGER PRIMARY KEY, ' +
                      '[Name] TEXT NOT NULL ' +
                     ');');

    ScoreDB.ExecSQL('CREATE TABLE IF NOT EXISTS [' + cUS_Webs_Stats + '] (' +
                      '[WebID] INTEGER NOT NULL, ' +
                      '[SongID] INTEGER NOT NULL, ' +
                      '[Max_Score_0] INTEGER NULL,' +
                      '[Media_Score_0] INTEGER NULL,' +
                      '[User_Score_0] TEXT NULL,' +
                      '[Max_Score_1] INTEGER NULL,' +
                      '[Media_Score_1] INTEGER NULL,' +
                      '[User_Score_1] TEXT NULL,' +
                      '[Max_Score_2] INTEGER NULL,' +
                      '[Media_Score_2] INTEGER NULL,' +
                      '[User_Score_2] TEXT NULL ' +
                    ');');

    ScoreDB.ExecSQL('CREATE TABLE IF NOT EXISTS [' + cUS_Users_Info + '] (' +
                      '[WebID] INTEGER NOT NULL, ' +
                      '[Username] TEXT NOT NULL, ' +
                      '[Password] TEXT NOT NULL, ' +
                      '[SendSavePlayer] INTEGER NOT NULL,' +
                      '[AutoMode] INTEGER NOT NULL,' +
                      '[AutoPlayer] INTEGER NOT NULL,' +
                      '[AutoScoreEasy] INTEGER NOT NULL,' +
                      '[AutoScoreMedium] INTEGER NOT NULL,' +
                      '[AutoScoreHard] INTEGER NOT NULL' +
                    ');');

    //add column for options jukebox
    if not ScoreDB.ContainsColumn(cUS_Songs, 'VideoRatioAspect') then
    begin
      Log.LogInfo('adding columns to "' + cUS_Songs + ' for jukebox options"', 'TDataBaseSystem.Init');

      ScoreDB.ExecSQL('ALTER TABLE ' + cUS_Songs + ' ADD COLUMN [VideoRatioAspect] INTEGER NULL');
      ScoreDB.ExecSQL('ALTER TABLE ' + cUS_Songs + ' ADD COLUMN [VideoWidth] INTEGER NULL');
      ScoreDB.ExecSQL('ALTER TABLE ' + cUS_Songs + ' ADD COLUMN [VideoHeight] INTEGER NULL');
      ScoreDB.ExecSQL('ALTER TABLE ' + cUS_Songs + ' ADD COLUMN [LyricPosition] INTEGER NULL');
      ScoreDB.ExecSQL('ALTER TABLE ' + cUS_Songs + ' ADD COLUMN [LyricAlpha] INTEGER NULL');
      ScoreDB.ExecSQL('ALTER TABLE ' + cUS_Songs + ' ADD COLUMN [LyricSingFillColor] TEXT NULL');
      ScoreDB.ExecSQL('ALTER TABLE ' + cUS_Songs + ' ADD COLUMN [LyricActualFillColor] TEXT NULL');
      ScoreDB.ExecSQL('ALTER TABLE ' + cUS_Songs + ' ADD COLUMN [LyricNextFillColor] TEXT NULL');
      ScoreDB.ExecSQL('ALTER TABLE ' + cUS_Songs + ' ADD COLUMN [LyricSingOutlineColor] TEXT NULL');
      ScoreDB.ExecSQL('ALTER TABLE ' + cUS_Songs + ' ADD COLUMN [LyricActualOutlineColor] TEXT NULL');
      ScoreDB.ExecSQL('ALTER TABLE ' + cUS_Songs + ' ADD COLUMN [LyricNextOutlineColor] TEXT NULL');
    end;
    
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
      Log.LogError(E.Message, 'TDataBaseSystem.Init');;
      FreeAndNil(ScoreDB);
      DeleteFile(Filename.ToNative());
      Log.LogCritical(E.Message, 'TDataBaseSystem.Init');
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
      ' SELECT  SongID, Difficulty, Player, Score, ''NULL'' FROM us_scores_101;');
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
         (Length(Song.Score[Difficulty]) < Ini.TopScreenSize) then
      begin
        //filter player
        PlayerListed:=false;
        if (Ini.TopScores = 1) then
        begin
          if (Length(Song.Score[Difficulty])>0) then
          begin
            for I := 0 to High(Song.Score[Difficulty]) do
            begin
              if (Song.Score[Difficulty, I].Name = TableData.FieldByName['Player']) then
              begin
                PlayerListed:=true;
                break;
              end;
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


(*
 * Add Website
 *)
procedure TDataBaseSystem.AddWebsite;
var
  I, WebID: Integer;
begin

  if not Assigned(ScoreDB) then
    Exit;

  try

    for I := 0 to High(DLLMan.Websites) do
    begin

      // load functions
      DLLMan.LoadWebsite(I);

      WebID := ScoreDB.GetTableValue(
               'SELECT [ID] FROM [' + cUS_Webs + '] ' +
               'WHERE [Name] = ? ',
               [UTF8Encode(DLLMan.Websites[I].Name)]);

      if (WebID = 0) then
      begin
        // Create website if it does not exist
        ScoreDB.ExecSQL(
          'INSERT INTO [' + cUS_Webs + '] ' +
          '([ID], [Name]) VALUES ' +
          '(NULL, ?);',
          [UTF8Encode(DLLMan.Websites[I].Name)]);
          DLLMan.Websites[I].ID := ScoreDB.GetLastInsertRowID();
      end
      else
        DLLMan.Websites[I].ID := WebID;
    end;

  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.AddWebsite');
  end;

end;

(**
 * Read Users Info to Array
 *)
procedure TDataBaseSystem.ReadUsers;
var
  TableData_Web, TableData_User: TSQLiteUniTable;
  I, WebID: Integer;
  Website: UTF8String;
begin

  if not Assigned(ScoreDB) then
    Exit;

  NetworkUser := nil;

  SetLength(NetworkUser, Length(DLLMan.Websites));
  for I := 0 to High(DLLMan.Websites) do
  begin
    Website := DLLMan.Websites[I].Name;
    NetworkUser[I].Website := UTF8Encode(Website);
  end;

  TableData_Web := nil;
  TableData_User := nil;

  try

    // Search User's in DB
    TableData_User := ScoreDB.GetUniTable('SELECT * FROM ['+cUS_Users_Info+'] ORDER BY [Username]');

    // Go through all Entrys
    while (not TableData_User.EOF) do
    begin

      // Add one Entry to Array of WebSite->UserList
      for I:=0 to High(NetworkUser) do
      begin
        WebID := ScoreDB.GetTableValue(
                 'SELECT [ID] FROM [' + cUS_Webs + '] ' +
                 'WHERE [Name] = ? ',
                 [NetworkUser[I].Website]);

        if (WebID = StrToInt(TableData_User.Fields[0])) then
        begin
          SetLength(NetworkUser[I].UserList, Length(NetworkUser[I].UserList) + 1);
          NetworkUser[I].UserList[High(NetworkUser[I].UserList)].Username := TableData_User.Fields[1];
          NetworkUser[I].UserList[High(NetworkUser[I].UserList)].Password := TableData_User.Fields[2];
          NetworkUser[I].UserList[High(NetworkUser[I].UserList)].SendSavePlayer := StrToInt(TableData_User.Fields[3]);
          NetworkUser[I].UserList[High(NetworkUser[I].UserList)].AutoMode := StrToInt(TableData_User.Fields[4]);
          NetworkUser[I].UserList[High(NetworkUser[I].UserList)].AutoPlayer := StrToInt(TableData_User.Fields[5]);
          NetworkUser[I].UserList[High(NetworkUser[I].UserList)].AutoScoreEasy := StrToInt(TableData_User.Fields[6]);
          NetworkUser[I].UserList[High(NetworkUser[I].UserList)].AutoScoreMedium := StrToInt(TableData_User.Fields[7]);
          NetworkUser[I].UserList[High(NetworkUser[I].UserList)].AutoScoreHard := StrToInt(TableData_User.Fields[8]);
        end;
      end;
      TableData_User.Next;
    end; // while

  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.ReadUser');
  end;

  TableData_User.Free;
  TableData_Web.Free;

end;

(**
 * Save User to DB
 *)
procedure TDataBaseSystem.NewUser(Website: UTF8String; Username, Password: UTF8String);
var
  WebID: integer;
begin

  if not Assigned(ScoreDB) then
    Exit;


  try
    WebID := ScoreDB.GetTableValue(
        'SELECT [ID] FROM [' + cUS_Webs + '] ' +
        'WHERE [Name] = ? ',
        [Website]);

    // Insert New entry's
    ScoreDB.ExecSQL('INSERT INTO [' + cUS_Users_Info + ']('+
    '[WebID],[Username],[Password],[SendSavePlayer],[AutoMode],[AutoPlayer],[AutoScoreEasy],[AutoScoreMedium],[AutoScoreHard])' +
    'VALUES (?,?,?,?,?,?,?,?,?)',
    [WebID, Username, Password, 0, 0, 0, 0, 0, 0]);

  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.NewUser');
  end;

end;

(**
 * Save Users Info to DB
 *)
procedure TDataBaseSystem.UpdateUsers;
var
  I, J, WebID: Integer;
begin

  if not Assigned(ScoreDB) then
    Exit;

  try
  begin
    for I:=0 to High(NetworkUser) do
    begin
      for J:=0 to High(NetworkUser[I].UserList) do
      begin
        if (NetworkUser[I].UserList[J].Save = true) then
        begin
          WebID := ScoreDB.GetTableValue(
          'SELECT [ID] FROM [' + cUS_Webs + '] ' +
          'WHERE [Name] = ? ',
          [NetworkUser[I].Website]);

          // Update entry's
          ScoreDB.ExecSQL(
          'UPDATE [' + cUS_Users_Info + '] ' +
          'SET [SendSavePlayer]=' + IntToStr(NetworkUser[I].UserList[J].SendSavePlayer) +
          ', [AutoMode]=' + IntToStr(NetworkUser[I].UserList[J].AutoMode) +
          ', [AutoPlayer]=' + IntToStr(NetworkUser[I].UserList[J].AutoPlayer) +
          ', [AutoScoreEasy]=' + IntToStr(NetworkUser[I].UserList[J].AutoScoreEasy) +
          ', [AutoScoreMedium]=' + IntToStr(NetworkUser[I].UserList[J].AutoScoreMedium) +
          ', [AutoScoreHard]=' +  IntToStr(NetworkUser[I].UserList[J].AutoScoreHard) +
          ' WHERE [WebID] = ? AND [Username] = ? ; ',
          [WebID, NetworkUser[I].UserList[J].Username]);
        end;
      end;
    end;
  end;
  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.UpdateUser');
  end;

end;

(**
 * Delete User Info on DB
 *)
procedure TDataBaseSystem.DeleteUser(Website: UTF8String; Username: UTF8String);
var
  WebID: integer;
begin
  if not Assigned(ScoreDB) then
    Exit;

  try
    WebID := ScoreDB.GetTableValue(
             'SELECT [ID] FROM [' + cUS_Webs + '] ' +
             'WHERE [Name] = ? ',
             [Website]);

    ScoreDB.ExecSQL('DELETE FROM [' + cUS_Users_Info + '] ' +
        'WHERE [WebID] = ? AND [Username] = ?;',
        [WebID, Username]);

  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.DeleteUser');
  end;

end;

(**
 * Adds songs to DB
 *)
procedure TDataBaseSystem.AddSong(Song: TSong);
var
  ID:        integer;
  TableData: TSQLiteTable;
begin

  if not Assigned(ScoreDB) then
    Exit;

  TableData := nil;

  if (Song.Artist <> '') and (Song.Title <> '') then
  begin
  try
    ID := ScoreDB.GetTableValue(
        'SELECT [ID] FROM ['+cUS_Songs+'] ' +
        'WHERE [Artist] = ? AND [Title] = ?',
        [Song.Artist, Song.Title]);

    if (ID = 0) then
    begin
      // Create song if it does not exist
      ScoreDB.ExecSQL(
          'INSERT INTO ['+cUS_Songs+'] ' +
          '([Artist], [Title], [TimesPlayed]) VALUES ' +
          '(?, ?, 0);',
          [Song.Artist, Song.Title]);
    end;

  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.AddSong');
  end;
  end;

  TableData.Free;
end;

(**
 * Add Max_Score to Song
 *)
procedure TDataBaseSystem.AddMax_Score (Song: TSong; WebID: integer; Receive_Max_Score: integer; Level: integer);
var
  Max_Score, ID, Count: integer;
  TableData: TSQLiteTable;
begin

  if not Assigned(ScoreDB) then
    Exit;

  TableData := nil;

  try

    ID := ScoreDB.GetTableValue(
        'SELECT [ID] FROM ['+cUS_Songs+'] ' +
        'WHERE [Artist] = ? AND [Title] = ?',
        [Song.Artist, Song.Title]);

    Count := ScoreDB.GetTableValue(
        'SELECT COUNT(*) FROM [' + cUS_Webs_Stats + '] ' +
        'WHERE [WebID] = ? AND [SongID] = ?',
        [WebID, ID]);

    if (Count = 0) then
    begin
      // Insert Max Score
      ScoreDB.ExecSQL(
         'INSERT INTO ['+cUS_Webs_Stats+'] ' +
         '([WebID], [SongID], [Max_Score_' + IntToStr(Level) + ']) VALUES(?,?,?);',
         [WebID, ID, Receive_Max_Score]);
    end
    else
    begin

      Max_Score := ScoreDB.GetTableValue(
        'SELECT [Max_Score_' + IntToStr(Level) + '] FROM [' + cUS_Webs_Stats + '] ' +
        'WHERE [WebID] = ? AND [SongID] = ?',
        [WebID, ID]);

      if (Max_Score <> Receive_Max_Score) then
      begin
        // Update Max Score
        ScoreDB.ExecSQL(
            'UPDATE ['+cUS_Webs_Stats+'] ' +
            'SET [Max_Score_' + IntToStr(Level) + '] = ' + IntToStr(Receive_Max_Score) +
            ' WHERE [WebID] = ? AND [SongID] = ?;',
            [WebID, ID]);
      end;
    end;

  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.AddMax_Score');
  end;

  TableData.Free;

end;

(**
 * Add Media_Score to Song
 *)
procedure TDataBaseSystem.AddMedia_Score (Song: TSong; WebID: integer; Receive_Media_Score: integer; Level: integer);
var
  Media_Score, ID: integer;
  TableData: TSQLiteTable;
begin

  if not Assigned(ScoreDB) then
    Exit;

  TableData := nil;

  try

    ID := ScoreDB.GetTableValue(
        'SELECT [ID] FROM ['+cUS_Songs+'] ' +
        'WHERE [Artist] = ? AND [Title] = ?',
        [Song.Artist, Song.Title]);

    Media_Score := ScoreDB.GetTableValue(
        'SELECT [Media_Score_' + IntToStr(Level) + '] FROM [' + cUS_Webs_Stats + '] ' +
        'WHERE [WebID] = ? AND [SongID] = ?',
        [WebID, ID]);

    if (Media_Score <> Receive_Media_Score) then
    begin
      // Update Max Score
      ScoreDB.ExecSQL(
          'UPDATE ['+cUS_Webs_Stats+'] ' +
          'SET [Media_Score_' + IntToStr(Level) + '] = ' + IntToStr(Receive_Media_Score) +
          ' WHERE [WebID] = ? AND [SongID] = ?;',
          [WebID, ID]);
    end;

  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.AddMedia_Score');
  end;

  TableData.Free;

end;


(**
 * Add User to Song
 *)
procedure TDataBaseSystem.AddUser_Score (Song: TSong; WebID: integer; Receive_User_Score: string; Level: integer);
var
  User_Score: string;
  ID: integer;
  TableData: TSQLiteTable;
begin

  if not Assigned(ScoreDB) then
    Exit;

  TableData := nil;

  try

    ID := ScoreDB.GetTableValue(
        'SELECT [ID] FROM ['+cUS_Songs+'] ' +
        'WHERE [Artist] = ? AND [Title] = ?',
        [Song.Artist, Song.Title]);

    User_Score := ScoreDB.GetTableString(
        'SELECT [User_Score_' + IntToStr(Level) + '] FROM [' + cUS_Webs_Stats + '] ' +
        'WHERE [WebID] = ? AND [SongID] = ?',
        [WebID, ID]);

    if (User_Score <> Receive_User_Score) then
    begin
      // Update User Score
      ScoreDB.ExecSQL(
          'UPDATE ['+cUS_Webs_Stats+'] ' +
          'SET [User_Score_' + IntToStr(Level) + '] = ? ' +
          ' WHERE [WebID] = ? AND [SongID] = ?;',
          [UTF8Encode(Receive_User_Score), WebID, ID]);
    end;

  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.AddUser_Score');
  end;

  TableData.Free;

end;

(**
 * Read Max_Score
 *)
function TDataBaseSystem.ReadMax_Score(Artist, Title: UTF8String; WebID, Level: integer): integer;
var
  Max_Score, SongID: integer;
  TableData: TSQLiteTable;
begin

  Max_Score := 0;

  if not Assigned(ScoreDB) then
    Exit;

  TableData := nil;

  try
    SongID := ScoreDB.GetTableValue(
        'SELECT [ID] FROM ['+cUS_Songs+'] ' +
        'WHERE [Artist] = ? AND [Title] = ?',
        [Artist, Title]);

    Max_Score := ScoreDB.GetTableValue(
        'SELECT [Max_Score_' + IntToStr(Level) + '] FROM ['+cUS_Webs_Stats+'] ' +
        'WHERE [WebID] = ? AND [SongID] = ?',
        [WebID, SongID]);

  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.ReadMax_Score');
  end;

  TableData.Free;

  result := Max_Score;

end;

(**
 * Read Media_Score
 *)
function TDataBaseSystem.ReadMedia_Score(Artist, Title: UTF8String; WebID, Level: integer): integer;
var
  Media_Score, SongID: integer;
  TableData: TSQLiteTable;
begin
  Media_Score := 0;

  if not Assigned(ScoreDB) then
    Exit;

  TableData := nil;

  try
    SongID := ScoreDB.GetTableValue(
        'SELECT [ID] FROM ['+cUS_Songs+'] ' +
        'WHERE [Artist] = ? AND [Title] = ?',
        [Artist, Title]);

    Media_Score := ScoreDB.GetTableValue(
        'SELECT [Media_Score_' + IntToStr(Level) + '] FROM ['+cUS_Webs_Stats+'] ' +
        'WHERE [WebID] = ? AND [SongID] = ?',
        [WebID, SongID]);

  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.ReadMedia_Score');
  end;

  TableData.Free;

  Result := Media_Score;

end;

(**
 * Read User_Score
 *)
function TDataBaseSystem.ReadUser_Score(Artist, Title: UTF8String; WebID, Level: integer): string;
var
  User_Score: string;
  SongID: integer;
  TableData: TSQLiteTable;
begin

  if not Assigned(ScoreDB) then
    Exit;

  TableData := nil;

  try
    SongID := ScoreDB.GetTableValue(
        'SELECT [ID] FROM ['+cUS_Songs+'] ' +
        'WHERE [Artist] = ? AND [Title] = ?',
        [Artist, Title]);

    User_Score := ScoreDB.GetTableString(
        'SELECT [User_Score_' + IntToStr(Level) + '] FROM ['+cUS_Webs_Stats+'] ' +
        'WHERE [WebID] = ? AND [SongID] = ?',
        [WebID, SongID]);

  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.ReadUser_Score');
  end;

  TableData.Free;

  Result := UTF8Decode(User_Score);

end;


(**
 * Read Max_Score Local
 *)
function TDataBaseSystem.ReadMax_ScoreLocal(Artist, Title: UTF8String; Level: integer): integer;
var
  Max_Score, ID: integer;
  TableData: TSQLiteTable;
begin

  Max_Score := 0;

  if not Assigned(ScoreDB) then
    Exit;

  TableData := nil;

  try

    ID := ScoreDB.GetTableValue(
          'SELECT [ID] FROM ['+cUS_Songs+'] ' +
          'WHERE [Artist] = ? AND [Title] = ?',
          [Artist, Title]);

    Max_Score := ScoreDB.GetTableValue(
        'SELECT MAX([Score]) FROM ['+cUS_Scores+'] ' +
        'WHERE [SongID] = ? AND [Difficulty] = ?',
        [ID, Level]);

  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.ReadMax_ScoreLocal');
 end;

  TableData.Free;

  Result := Max_Score;

end;

(**
 * Read Media_Score
 *)
function TDataBaseSystem.ReadMedia_ScoreLocal(Artist, Title: UTF8String; Level: integer): integer;
var
  Media_Score, ID: integer;
  TableData: TSQLiteTable;
begin

  Media_Score := 0;

  if not Assigned(ScoreDB) then
    Exit;

  TableData := nil;

  try
    ID := ScoreDB.GetTableValue(
          'SELECT [ID] FROM ['+cUS_Songs+'] ' +
          'WHERE [Artist] = ? AND [Title] = ?',
          [Artist, Title]);

    Media_Score := ScoreDB.GetTableValue(
        'SELECT AVG([Score]) FROM ['+cUS_Scores+'] ' +
        'WHERE [SongID] = ? AND [Difficulty] = ?',
        [ID, Level]);

  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.ReadMedia_ScoreLocal');
  end;

  TableData.Free;

  result := Media_Score;

end;

(**
 * Read User_Score
 *)
function TDataBaseSystem.ReadUser_ScoreLocal(Artist, Title: UTF8String; Level: integer): string;
var
  User_Score: string;
  ID: integer;
  TableData: TSQLiteTable;
begin

  if not Assigned(ScoreDB) then
    Exit;

  TableData := nil;

  try
    ID := ScoreDB.GetTableValue(
          'SELECT [ID] FROM ['+cUS_Songs+'] ' +
          'WHERE [Artist] = ? AND [Title] = ?',
          [Artist, Title]);

    User_Score := ScoreDB.GetTableString(
                 'SELECT [Player] FROM ['+cUS_Scores+'] ' +
                 'WHERE [SongID] = ? and [Difficulty] = ? ORDER BY [Score] DESC LIMIT 1',
                 [ID, Level]);

  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.ReadUser_Score');
  end;

  TableData.Free;

  Result := User_Score;

end;

(**
 * Delete Score (Maybe song not exist anymore (or update))
 *)
function TDataBaseSystem.Delete_Score(Song: TSong; WebID: integer): integer;
var
  Score: byte;
  ID: integer;
  TableData: TSQLiteTable;
begin

  Score := 0;

  if not Assigned(ScoreDB) then
    Exit;

  TableData := nil;

  try

    ID := ScoreDB.GetTableValue(
        'SELECT [ID] FROM ['+cUS_Songs+'] ' +
        'WHERE [Artist] = ? AND [Title] = ?',
        [Song.Artist, Song.Title]);

    Score := ScoreDB.GetTableValue(
        'SELECT COUNT(*) FROM [' + cUS_Webs_Stats + '] ' +
        'WHERE [WebID] = ? AND [SongID] = ?',
        [WebID, ID]);

    if (Score <> 0) then
    begin
      // Delete Score
      ScoreDB.ExecSQL(
          'DELETE FROM ['+cUS_Webs_Stats+'] ' +
          ' WHERE [WebID] = ? AND [SongID] = ?;',
          [WebID, ID]);
    end;

  except on E: Exception do
    Log.LogError(E.Message, 'TDataBaseSystem.Delete_Score');
  end;

  TableData.Free;

  Result := Score;
end;

(*
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

(**
 * SaveSongOptions to DB
 *)
procedure TDataBaseSystem.SaveSongOptions(Song: TSong; Options: TSongOptions);
begin

  AddSong(Song);

  ScoreDB.ExecSQL(
          'UPDATE [' + cUS_Songs + '] ' +
          'SET [VideoRatioAspect] = ?, ' +
          '[VideoWidth] = ?, ' +
          '[VideoHeight] = ?, ' +
          '[LyricPosition] = ?, ' +
          '[LyricAlpha] = ?, ' +
          '[LyricSingFillColor] = ?, ' +
          '[LyricActualFillColor] = ?, ' +
          '[LyricNextFillColor] = ?, ' +
          '[LyricSingOutlineColor] = ?, ' +
          '[LyricActualOutlineColor] = ?, ' +
          '[LyricNextOutlineColor] = ? ' +
          'WHERE [Artist] = ? AND [Title] = ?;',
          [Options.VideoRatioAspect, Options.VideoWidth, Options.VideoHeight,
          Options.LyricPosition, Options.LyricAlpha,
          Options.LyricSingFillColor, Options.LyricActualFillColor, Options.LyricNextFillColor,
          Options.LyricSingOutlineColor, Options.LyricActualOutlineColor, Options.LyricNextOutlineColor,
          Song.Artist, Song.Title]);

  end;

function TDataBaseSystem.GetSongOptions(Song: TSong): TSongOptions;
var
  TableData: TSQLiteUniTable;
  SongOptions: TSongOptions;
begin
  Result := nil;

  if not Assigned(ScoreDB) then
    Exit;

  // Execute query
  try
    TableData := ScoreDB.GetUniTable('SELECT VideoRatioAspect, VideoWidth, VideoHeight, LyricPosition, LyricAlpha, ' +
                                            'LyricSingFillColor, LyricActualFillColor, LyricNextFillColor,' +
                                            'LyricSingOutlineColor, LyricActualOutlineColor, LyricNextOutlineColor ' +
                                     'FROM [' + cUS_Songs + '] ' +
                                     'WHERE [Artist] = ? AND [Title] = ?',
                                     [Song.Artist, Song.Title]);
  except
    on E: Exception do
    begin
      Log.LogError(E.Message, 'TDataBaseSystem.GetSongOptions');
      Exit;
    end;
  end;

  if (TableData.EOF = false) then
  begin
    SongOptions := TSongOptions.Create(TableData.FieldAsInteger(0), TableData.FieldAsInteger(1), TableData.FieldAsInteger(2), TableData.FieldAsInteger(3), TableData.FieldAsInteger(4),
      TableData.FieldAsString(5), TableData.FieldAsString(6), TableData.FieldAsString(7), TableData.FieldAsString(8), TableData.FieldAsString(9), TableData.FieldAsString(10));

    Result := SongOptions;
  end
  else
    Result := nil;

  TableData.Free;
end;

end.
