unit UDataBase;

interface

uses USongs, SQLiteTable3;

//--------------------
//DataBaseSystem - Class including all DB Methods
//--------------------
type
  TDataBaseSystem = class
    private
      ScoreDB: TSqliteDatabase;
      sFilename: string;
    public


    Destructor Free;

    Procedure Init(const Filename: string);
    procedure ReadScore(var Song: TSong);
    procedure AddScore(var Song: TSong; Level: integer; Name: string; Score: integer);
    procedure WriteScore(var Song: TSong);
  end;

var
  DataBase: TDataBaseSystem;

implementation

uses IniFiles, SysUtils;

//--------------------
//Create - Opens Database and Create Tables if not Exist
//--------------------

Procedure TDataBaseSystem.Init(const Filename: string);
begin
  //Open Database
  ScoreDB := TSqliteDatabase.Create(Filename);
  sFilename := Filename;

  try
  //Look for Tables => When not exist Create them
  if not ScoreDB.TableExists('US_Scores') then
    ScoreDB.execsql('CREATE TABLE `US_Scores` (`SongID` INT( 11 ) NOT NULL , `Difficulty` INT( 1 ) NOT NULL , `Player` VARCHAR( 150 ) NOT NULL , `Score` INT( 5 ) NOT NULL );');

  if not ScoreDB.TableExists('US_Songs') then
    ScoreDB.execsql('CREATE TABLE `US_Songs` (`ID` INTEGER PRIMARY KEY, `Artist` VARCHAR( 255 ) NOT NULL , `Title` VARCHAR( 255 ) NOT NULL , `TimesPlayed` int(5) NOT NULL );');
   //Not possible because of String Limitation to 255 Chars //Need to rewrite Wrapper 
  {if not ScoreDB.TableExists('US_SongCache') then
    ScoreDB.ExecSQL('CREATE TABLE `US_SongCache` (`Path` VARCHAR( 255 ) NOT NULL , `Filename` VARCHAR( 255 ) NOT NULL , `Title` VARCHAR( 255 ) NOT NULL , `Artist` VARCHAR( 255 ) NOT NULL , `Folder` VARCHAR( 255 ) NOT NULL , `Genre` VARCHAR( 255 ) NOT NULL , `Edition` VARCHAR( 255 ) NOT NULL , `Language` VARCHAR( 255 ) NOT NULL , `Creator` VARCHAR( 255 ) NOT NULL , `Cover` VARCHAR( 255 ) NOT NULL , `Background` VARCHAR( 255 ) NOT NULL , `Video` VARCHAR( 255 ) NOT NULL , `VideoGap` FLOAT NOT NULL , `Gap` FLOAT NOT NULL , `Start` FLOAT NOT NULL , `Finish` INT( 11 ) NOT NULL , `BPM` INT( 5 ) NOT NULL , `Relative` BOOLEAN NOT NULL , `NotesGap` INT( 11 ) NOT NULL);');}


  finally
  //ScoreDB.Free;
  end;

end;

//--------------------
//Free - Frees Database
//--------------------
Destructor TDataBaseSystem.Free;
begin
  ScoreDB.Free;
end;

//--------------------
//ReadScore - Read Scores into SongArray
//--------------------
procedure TDataBaseSystem.ReadScore(var Song: TSong);
var
  TableData: TSqliteTable;
  Dif: Byte;
begin
  //ScoreDB := TSqliteDatabase.Create(sFilename);
  try
  try
  //Search Song in DB
  TableData := ScoreDB.GetTable('SELECT `Difficulty`, `Player`, `Score` FROM `us_scores` WHERE `SongID` = (SELECT `ID` FROM `us_songs` WHERE `Artist` = "' + Song.Artist + '" AND `Title` = "' + Song.Title + '" LIMIT 1) ORDER BY `Score` DESC  LIMIT 15');
  //Empty Old Scores
  SetLength (Song.Score[0], 0);
  SetLength (Song.Score[1], 0);
  SetLength (Song.Score[2], 0);

  while not TableData.Eof do//Go through all Entrys
  begin//Add one Entry to Array
    Dif := StrtoInt(TableData.FieldAsString(TableData.FieldIndex['Difficulty']));
    if (Dif>=0) AND (Dif<=2) then
    begin
      SetLength(Song.Score[Dif], Length(Song.Score[Dif]) + 1);

      Song.Score[Dif, high(Song.Score[Dif])].Name := TableData.FieldAsString(TableData.FieldIndex['Player']);
      Song.Score[Dif, high(Song.Score[Dif])].Score:= StrtoInt(TableData.FieldAsString(TableData.FieldIndex['Score']));
    end;
    TableData.Next;
  end;

  except //Im Fehlerfall
  for Dif := 0 to 2 do
  begin
  SetLength(Song.Score[Dif], 1);
  Song.Score[Dif, 1].Name := 'Error Reading ScoreDB';
  end;
  end;
  finally
  //ScoreDb.Free;
  end;
end;

//--------------------
//AddScore - Add one new Score to DB
//--------------------
procedure TDataBaseSystem.AddScore(var Song: TSong; Level: integer; Name: string; Score: integer);
var
ID: Integer;
TableData: TSqliteTable;
begin
  //ScoreDB := TSqliteDatabase.Create(sFilename);
  try
  //Prevent 0 Scores from being added
  if (Score > 0) then
  begin

    ID := ScoreDB.GetTableValue('SELECT `ID` FROM `US_Songs` WHERE `Artist` = "' + Song.Artist + '" AND `Title` = "' + Song.Title + '"');
    if ID = 0 then //Song doesn't exist -> Create
    begin
      ScoreDB.ExecSQL ('INSERT INTO `US_Songs` ( `ID` , `Artist` , `Title` , `TimesPlayed` ) VALUES (NULL , "' + Song.Artist + '", "' + Song.Title + '", "0");');
      ID := ScoreDB.GetTableValue('SELECT `ID` FROM `US_Songs` WHERE `Artist` = "' + Song.Artist + '" AND `Title` = "' + Song.Title + '"');
      if ID = 0 then //Could not Create Table
        exit;
    end;
    //Create new Entry
    ScoreDB.ExecSQL('INSERT INTO `US_Scores` ( `SongID` , `Difficulty` , `Player` , `Score` ) VALUES ("' + InttoStr(ID) + '", "' + InttoStr(Level) + '", "' + Name + '", "' + InttoStr(Score) + '");');

    //Delete Last Position when there are more than 5 Entrys
    if ScoreDB.GetTableValue('SELECT COUNT(`SongID`) FROM `US_Scores` WHERE `SongID` = "' + InttoStr(ID) + '" AND `Difficulty` = "' + InttoStr(Level) +'"') > 5 then
    begin
      TableData := ScoreDB.GetTable('SELECT `Player`, `Score` FROM `US_Scores` WHERE SongID = "' + InttoStr(ID) + '" AND `Difficulty` = "' + InttoStr(Level) +'" ORDER BY `Score` ASC LIMIT 1');
      ScoreDB.ExecSQL('DELETE FROM `US_Scores` WHERE SongID = "' + InttoStr(ID) + '" AND `Difficulty` = "' + InttoStr(Level) +'" AND `Player` = "' + TableData.FieldAsString(TableData.FieldIndex['Player']) + '" AND `Score` = "' + TableData.FieldAsString(TableData.FieldIndex['Score']) + '"');
    end;

  end;
  finally
  //ScoreDB.Free;
  end;
end;

//--------------------
//WriteScore - Not needed with new System; But used for Increment Played Count
//--------------------
procedure TDataBaseSystem.WriteScore(var Song: TSong);
begin
  try
    //Increase TimesPlayed
    ScoreDB.ExecSQL ('UPDATE `us_songs` SET `TimesPlayed` = `TimesPlayed` + "1" WHERE `Title` = "' + Song.Title + '" AND `Artist` = "' + Song.Artist + '";');
  except

  end;
end;

end.
