unit UFiles;

interface

uses USongs, SysUtils, ULog, UMusic;

//procedure     InitializePaths; //Function sets All Absolute Paths eg. for Songs
function    ReadTXTHeader(var Song: TSong): boolean; //Reads Standard TXT Header
function    AnalyseFile(var Song: TSong): boolean; //Analyse Song File and Read Header
procedure   ClearSong(var Song: TSong); //Clears Song Header values

//procedure CzyscNuty;
//function WczytajCzesci(Name: string): boolean;
//function SaveSong(Song: TSong; Czesc: TCzesci; Name: string; Relative: boolean): boolean;
//function SaveSongDebug(Song: TSong; Czesc: TCzesci; Name: string; Relative: boolean): boolean;

var
  {//Absolute Paths
  GamePath:   string;
  SoundPath:  string;
  SongPath:   string;
  LogPath:    string;
  ThemePath:  string;
  ScreenshotsPath:  string;
  CoversPath:       string;
  LanguagesPath:    string; //}

  SongFile: TextFile;   // all procedures in this unit operates on this file
  FileLineNo: integer;  //Line which is readed at Last, for error reporting

  {// variables available for all procedures
  Base:       array[0..1] of integer;
  Rel:        array[0..1] of integer;//}
  Mult:     integer = 1;
  MultBPM:  integer = 4;

implementation
uses TextGL, UIni, UMain, UPliki;

  //Function sets All Absolute Paths eg. for Songs
{procedure InitializePaths;
begin
  GamePath :=   ExtractFilePath(ParamStr(0));
  SoundPath :=  GamePath + 'Sounds\';
  SongPath :=   GamePath + 'Songs\';
  LogPath := GamePath;
  ThemePath := GamePath + 'Themes\';
  ScreenshotsPath := GamePath + 'Screenshots\';
  CoversPath := GamePath + 'Covers\';
  LanguagesPath := GamePath + 'Languages\';

  DecimalSeparator := ',';
end;}

  //Clears Song Header values
procedure ClearSong(var Song: TSong);
begin
  //Main Information
  Song.Title := '';
  Song.Artist := '';

  //Sortings:
  Song.Genre := 'Unknown';
  Song.Edition := 'Unknown';
  Song.Language := 'Unknown'; //Language Patch

  //Required Information
  Song.Mp3 := '';
  Song.BPM := 0;
  Song.GAP := 0;
  Song.Start := 0;
  Song.Finish := 0;

  //Additional Information
  Song.Background := '';
  Song.Video := '';
  Song.VideoGAP := 0;
  Song.NotesGAP := 0;
  Song.Resolution := 4;
  Song.Creator := '';
end;

  //Reads Standard TXT Header
function ReadTXTHeader(var Song: TSong): boolean;
var
Line, Identifier, Value: String;
Temp: word;
Done: byte;
begin
  Result := true;

  //Read first Line
  ReadLn (SongFile, Line);

  if (Length(Line)<=0) then
  begin
    Log.LogError('File Starts with Empty Line: ' + Song.FileName);
    Result := False;
    Exit;
  end;

  //Read Lines while Line starts with #
  While (Line[1] = '#') do
  begin
    //Increase Line Number
    Inc (FileLineNo);
    Temp := Pos(':', Line);

    //Line has a Seperator-> Headerline
    if (Temp <> 0) then
    begin
      //Read Identifier and Value
      Identifier  := Uppercase(Trim(Copy(Line, 2, Temp - 2))); //Uppercase is for Case Insensitive Checks
      Value       := Trim(Copy(Line, Temp + 1,Length(Line) - Temp));

      //Check the Identifier (If Value is given)
      if (Length(Value) <> 0) then
      begin

        //-----------
        //Required Attributes
        //-----------

        //Title
        if (Identifier = 'TITLE') then
        begin
          Song.Title := Value;

          //Add Title Flag to Done
          Done := Done or 1;
        end

        //Artist
        else if (Identifier = 'ARTIST') then
        begin
          Song.Artist := Value;

          //Add Artist Flag to Done
          Done := Done or 2;
        end

        //MP3 File //Test if Exists
        else if (Identifier = 'MP3') AND (FileExists(Song.Path + Value)) then
        begin
          Song.Mp3 := Value;

          //Add Mp3 Flag to Done
          Done := Done or 4;
        end

        //Beats per Minute
        else if (Identifier = 'BPM') then
        begin
          // Replace . with ,
          if (Pos('.', Value) <> 0) then
            Value[Pos('.', Value)] := ',';

          SetLength(Song.BPM, 1);
          Song.BPM[0].StartBeat := 0;

          Song.BPM[0].BPM := StrtoFloatDef(Value, 0) * Mult * MultBPM;

          if Song.BPM[0].BPM <> 0 then
          begin
            //Add BPM Flag to Done
            Done := Done or 8;
          end;
        end

        //---------
        //Additional Header Information
        //---------

        //Cover Picture
        else if (Identifier = 'COVER') then
        begin
          Song.Cover := Value;
        end

        //Background Picture
        else if (Identifier = 'BACKGROUND') then
        begin
          Song.Background := Value;
        end

        // Video File
        else if (Identifier = 'VIDEO') then
        begin
          Song.Video := Value;
        end

        // Video Gap
        else if (Identifier = 'VIDEOGAP') then
        begin
          // Replace . with ,
          if (Pos('.', Value) <> 0) then
            Value[Pos('.', Value)] := ',';

          Song.VideoGAP := StrtoFloatDef (Value, 0);
        end

        //Genre Sorting
        else if (Identifier = 'GENRE') then
        begin
          Song.Genre := Value;
        end

        //Edition Sorting
        else if (Identifier = 'EDITION') then
        begin
          Song.Edition := Value;
        end

        //Creator Tag
        else if (Identifier = 'CREATOR') then
        begin
          Song.Creator := Value;
        end

        //Language Sorting
        else if (Identifier = 'LANGUAGE') then
        begin
          Song.Language := Value;
        end

        // Song Start
        else if (Identifier = 'START') then
        begin
          // Replace . with ,
          if (Pos('.', Value) <> 0) then
            Value[Pos('.', Value)] := ',';

          Song.Start := StrtoFloatDef(Value, 0);
        end

        // Song Ending
        else if (Identifier = 'END') then
        begin
          TryStrtoInt(Value, Song.Finish);
        end

        // Resolution
        else if (Identifier = 'RESOLUTION') then
        begin
          TryStrtoInt(Value, Song.Resolution);
        end

        // Notes Gap
        else if (Identifier = 'NOTESGAP') then
        begin
          TryStrtoInt(Value, Song.NotesGAP);
        end

        // Relative Notes
        else if (Identifier = 'RELATIVE') AND (uppercase(Value) = 'YES') then
        begin
          Song.Relative := True;
        end;

      end;
    end;

    if not EOf(SongFile) then
      ReadLn (SongFile, Line)
    else
    begin
      Result := False;
      Log.LogError('File Incomplete or not Ultrastar TxT: ' + Song.FileName);
      break;
    end;

    //End on first empty Line
    if (Length(Line) = 0) then
      break;
  end;

  //Check if all Required Values are given
  if (Done <> 15) then
  begin
    Result := False;
    if (Done and 8) = 0 then      //No BMP Flag
    Log.LogError('BMP Tag Missing: ' + Song.FileName)
    else if (Done and 4) = 0 then //No MP3 Flag
    Log.LogError('MP3 Tag/File Missing: ' + Song.FileName)
    else if (Done and 2) = 0 then //No Artist Flag
    Log.LogError('Artist Tag Missing: ' + Song.FileName)
    else if (Done and 1) = 0 then //No Title Flag
    Log.LogError('Title Tag Missing: ' + Song.FileName)
    else //unknown Error
    Log.LogError('File Incomplete or not Ultrastar TxT: ' + Song.FileName);
  end;

end;

  //Analyse Song File and Read Header
function AnalyseFile(var Song: TSong): boolean;
begin
Result := False;
{try }
  //Reset LineNo
  FileLineNo := 0;

  //Open File and set File Pointer to the beginning
  AssignFile(SongFile, Song.Path + Song.FileName);
  Reset(SongFile);

  //Clear old Song Header
  ClearSong(Song);

  //Read Header
  Result := ReadTxTHeader(Song);

  //And Close File
  CloseFile(SongFile);
{except
  CloseFile(SongFile);

  Result := False;
  //Error Reporting
  Log.LogError('An Error occured reading Line ' + inttostr(FileLineNo) + ' from SongHeader: ' + Song.FileName);
end;}
end;



end.