unit USongs;

interface

type
  TScore = record
    Name:       string;
    Score:      integer;
    Length:     string;
  end;
  
  TSong = record
    Path:       string;
    FileName:   string;

    Title:      string;
    Artist:     string;

    Score:      array[0..2] of array of TScore;
  end;

  TSongs = class
    LastCount: Integer;
    Song:       array of TSong; // array of songs

    function  ReadHeader(var rSong: TSong): boolean;
    procedure BrowseDir(Dir: string); // Browse a dir + subdirs for songfiles
  end;

  var Songs: TSongs;

implementation
uses Sysutils, UMainForm, Dialogs;

function TSongs.ReadHeader(var rSong: TSong): boolean;
var
  Line, Identifier, Value: String;
  Temp: word;
  Done: byte;
  SongFile: Textfile;
begin
  Result := False;


  //Open File and set File Pointer to the beginning
  AssignFile(SongFile, rSong.Path + rSong.FileName);
  Reset(SongFile);

  //Read Header
  Result := true;

  //Read first Line
  ReadLn (SongFile, Line);

  if (Length(Line)<=0) then
  begin
    Result := False;
    Exit;
  end;
  Done := 0;
  //Read Lines while Line starts with #
  While (Line[1] = '#') do
  begin
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
          rSong.Title := Value;

          //Add Title Flag to Done
          Done := Done or 1;
        end

        //Artist
        else if (Identifier = 'ARTIST') then
        begin
          rSong.Artist := Value;

          //Add Artist Flag to Done
          Done := Done or 2;
        end;

      end;
    end;

    if not EOf(SongFile) then
      ReadLn (SongFile, Line)
    else
    begin
      Result := False;
      break;
    end;

    //End on first empty Line
    if (Length(Line) = 0) then
      break;
  end;

  //Check if all Required Values are given
  if (Done <> 3) then
  begin
    Result := False;
  end;

  //And Close File
  CloseFile(SongFile);
end;

procedure TSongs.BrowseDir(Dir: string);
var
  SR:     TSearchRec;   // for parsing Songs Directory
  SLen:   integer;
begin
  if FindFirst(Dir + '*', faDirectory, SR) = 0 then begin
    repeat
      if (SR.Name <> '.') and (SR.Name <> '..') then
        BrowseDir(Dir + Sr.Name + '\');
    until FindNext(SR) <> 0;
  end;
  FindClose(SR);

 if FindFirst(Dir + '*.txt', 0, SR) = 0 then begin
    repeat
      SLen := Length(Song);
      SetLength(Song, SLen + 1);

      Song[SLen].Path := Dir;
      Song[SLen].FileName := SR.Name;

      if (ReadHeader(Song[SLen]) = false) then SetLength(Song, SLen);

      //update Songs Label
      if LastCount <> SLen div 30 then
      begin
        LastCount := SLen div 30;
        MainForm.UpdateLoadedSongs(Dir, SLen);
      end;

    until FindNext(SR) <> 0;
    end; // if FindFirst
  FindClose(SR);
end;

end.
