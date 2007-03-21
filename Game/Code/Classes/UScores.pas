unit UScores;

interface

uses USongs, SQLiteTable3;

procedure InitScore(const Filename: string);
procedure ReadScore(var Song: TSong);
procedure WriteScore(var Song: TSong);
procedure AddScore(var Song: TSong; Level: integer; Name: string; Score: integer);

var
ScoreDB: TSqliteDatabase;
sFilename: string;

implementation

uses IniFiles, SysUtils;

procedure InitScore(const Filename: string);
//var
  //TableData: TSqliteTable;
begin
  //Open Database
  ScoreDB := TSqliteDatabase.Create(Filename);
  sFilename := Filename;

  try
  //Look for Tables => When not exist Create them
  if not ScoreDB.TableExists('US_Scores') then
    ScoreDB.execsql('CREATE TABLE `US_Scores` (`SongID` INT( 11 ) NOT NULL , `Difficulty` INT( 1 ) NOT NULL , `Player` VARCHAR( 150 ) NOT NULL , `Score` INT( 5 ) NOT NULL );');

  if not ScoreDB.TableExists('US_Songs') then
    ScoreDB.execsql('CREATE TABLE `US_Songs` (`ID` INTEGER PRIMARY KEY, `Artist` VARCHAR( 255 ) NOT NULL , `Title` VARCHAR( 255 ) NOT NULL );');

  finally
  //ScoreDB.Free;
  end;

end;

procedure ReadScore(var Song: TSong);
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

procedure AddScore(var Song: TSong; Level: integer; Name: string; Score: integer);
var
ID: Integer;
TableData: TSqliteTable;
begin
  //ScoreDB := TSqliteDatabase.Create(sFilename);
  try

  ID := ScoreDB.GetTableValue('SELECT `ID` FROM `US_Songs` WHERE `Artist` = "' + Song.Artist + '" AND `Title` = "' + Song.Title + '"');
  if ID = 0 then //Song doesn't exist -> Create
  begin
    ScoreDB.ExecSQL ('INSERT INTO `US_Songs` ( `ID` , `Artist` , `Title` ) VALUES (NULL , "' + Song.Artist + '", "' + Song.Title + '");');
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

  finally
  //ScoreDB.Free;
  end;
end;

//Not used with new SQLLite DB System
procedure WriteScore(var Song: TSong);
{var
  F:    TIniFile;
  S:    integer;
  Lev:      integer;
  LevS:     string;
  FileName: string;}
begin
  {FileName := Song.Path + ChangeFileExt(Song.FileName, '.sco');
  if (not FileExists(FileName)) or (FileExists(FileName) and  DeleteFile(FileName)) then begin
    // file has been deleted -> creating new file
    F := TIniFile.Create(FileName);

    for Lev := 0 to 2 do begin
      case Lev of
        0:  LevS := 'Easy';
        1:  LevS := 'Normal';
        2:  LevS := 'Hard';
      end;

      for S := 0 to high(Song.Score[Lev]) do begin
        F.WriteString(LevS + IntToStr(S+1), 'Name', Song.Score[Lev, S].Name);
        F.WriteInteger(LevS + IntToStr(S+1), 'Score', Song.Score[Lev, S].Score);

      end; // for S
    end; // for Lev
    F.Free;
  end; // if}
end;

end.
