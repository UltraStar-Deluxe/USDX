unit UFiles;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
{$I switches.inc}

uses SysUtils,
     ULog,
     UMusic,
     USongs,
     USong;

//procedure   InitializePaths; //Function sets All Absolute Paths eg. for Songs
//function    ReadTXTHeader(var Song: TSong): boolean; //Reads Standard TXT Header
//function    AnalyseFile(var Song: TSong): boolean; //Analyse Song File and Read Header
//procedure   ClearSong(var Song: TSong); //Clears Song Header values

//Methodes Loading and Saving Songfiles
procedure ResetSingTemp;
//procedure ParseNote(NrCzesci: integer; TypeP: char; StartP, DurationP, NoteP: integer; LyricS: string);
//procedure NewSentence(NrCzesciP: integer; Param1, Param2: integer);

//function  LoadSong(Name: string): boolean;
function  SaveSong(Song: TSong; Czesc: TCzesci; Name: string; Relative: boolean): boolean;



var
  SongFile: TextFile;   // all procedures in this unit operates on this file
  FileLineNo: integer;  //Line which is readed at Last, for error reporting

  // variables available for all procedures
  Base    : array[0..1] of integer;
  Rel     : array[0..1] of integer;
  Mult    : integer = 1;
  MultBPM : integer = 4;

implementation

uses TextGL,
     UIni,
		 UPlatform,
     UMain;

(*
//--------------------
<<<<<<< .mine
=======
// Clears Song Header values
//--------------------
procedure ClearSong(var Song: TSong);
begin
  //Main Information
  Song.Title  := '';
  Song.Artist := '';

  //Sortings:
  Song.Genre    := 'Unknown';
  Song.Edition  := 'Unknown';
  Song.Language := 'Unknown'; //Language Patch

  //Required Information
  Song.Mp3    := '';
  {$IFDEF FPC}
  setlength( Song.BPM, 0 );
  {$ELSE}
  Song.BPM    := 0;
  {$ENDIF}

  Song.GAP    := 0;
  Song.Start  := 0;
  Song.Finish := 0;

  //Additional Information
  Song.Background := '';
  Song.Cover      := '';
  Song.Video      := '';
  Song.VideoGAP   := 0;
  Song.NotesGAP   := 0;
  Song.Resolution := 4;
  Song.Creator    := '';
end;

//--------------------
// Reads Standard TXT Header
//--------------------
function ReadTXTHeader(var Song: TSong): boolean;
var
  Line, Identifier, Value: String;
  Temp: word;
  Done: byte;
begin
  Result := true;
  Done   := 0;

  //Read first Line
  ReadLn (SongFile, Line);

  if (Length(Line)<=0) then
  begin
    Log.LogError('File Starts with Empty Line: ' + Song.FileName);
    Result := False;
    Exit;
  end;

  //Read Lines while Line starts with #
  While (Length(Line) = 0) OR (Line[1] = '#') do
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
			
				{$IFDEF DARWIN}
				if ((Identifier = 'MP3') or (Identifier = 'COVER') or (Identifier = 'BACKGROUND') or (Identifier = 'VIDEO')) then
				begin
  			  // Filenames on OS X must be UTF8:
				  Value := Utf8Encode(Value);				
				end;
				{$ENDIF}
			

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

        // Video Gap
        else if (Identifier = 'GAP') then
        begin
          // Replace . with ,
          if (Pos('.', Value) <> 0) then
            Value[Pos('.', Value)] := ',';

          Song.GAP := StrtoFloatDef (Value, 0);
        end

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
          if (FileExists(Song.Path + Value)) then
            Song.Video := Value
          else
            Log.LogError('Can''t find Video File in Song: ' + Song.Path + Song.FileName);
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
      Log.LogError('File Incomplete or not Ultrastar TxT (A): ' + Song.FileName);
      break;
    end;

    {//End on first empty Line
    if (Length(Line) = 0) then
      break;}
  end;

  //Check if all Required Values are given
  if (Done <> 15) then
  begin
    Result := False;
    if (Done and 8) = 0 then      //No BPM Flag
    Log.LogError('BPM Tag Missing: ' + Song.FileName)
    else if (Done and 4) = 0 then //No MP3 Flag
    Log.LogError('MP3 Tag/File Missing: ' + Song.FileName)
    else if (Done and 2) = 0 then //No Artist Flag
    Log.LogError('Artist Tag Missing: ' + Song.FileName)
    else if (Done and 1) = 0 then //No Title Flag
    Log.LogError('Title Tag Missing: ' + Song.FileName)
    else //unknown Error
    Log.LogError('File Incomplete or not Ultrastar TxT (B - '+ inttostr(Done) +'): ' + Song.FileName);
  end;

end;

//--------------------
// Analyse Song File and Read Header
//--------------------
function AnalyseFile(var Song: TSong): boolean;
begin
Result := False;
{try }
  //Reset LineNo
  FileLineNo := 0;

  //Open File and set File Pointer to the beginning
  AssignFile(SongFile, Song.Path + Song.FileName);
	
//  if assinged( SongFile ) then
  begin
    try
    Reset(SongFile);

    //Clear old Song Header
    ClearSong(Song);

    //Read Header
    Result := ReadTxTHeader(Song);

    //And Close File
    finally
      CloseFile(SongFile);
    end;
  end;
{except
  CloseFile(SongFile);

  Result := False;
  //Error Reporting
  Log.LogError('An Error occured reading Line ' + inttostr(FileLineNo) + ' from SongHeader: ' + Song.FileName);
end;}
end;
              *)
//--------------------
// Resets the temporary Sentence Arrays for each Player and some other Variables
//--------------------
procedure ResetSingTemp;
var
  Pet:  integer;
begin
  SetLength(Czesci, Length(Player));
  for Pet := 0 to High(Player) do begin
    SetLength(Czesci[Pet].Czesc, 1);
    SetLength(Czesci[Pet].Czesc[0].Nuta, 0);
    Czesci[Pet].Czesc[0].Lyric := '';
    Czesci[Pet].Czesc[0].LyricWidth := 0;
    Player[pet].Score := 0;
    Player[pet].IlNut := 0;
    Player[pet].HighNut := -1;
  end;

  //Reset Path and Filename Values to Prevent Errors in Editor
  if assigned( CurrentSong ) then
  begin
    SetLength(CurrentSong.BPM, 0);
    CurrentSong.Path := '';
    CurrentSong.FileName := '';
  end;
  
//  CurrentSong := nil;
end;


//--------------------
// Saves a Song
//--------------------
function SaveSong(Song: TSong; Czesc: TCzesci; Name: string; Relative: boolean): boolean;
var
  C:      integer;
  N:      integer;
  S:      string;
  B:      integer;
  RelativeSubTime:    integer;
  NoteState: String;

begin
//  Relative := true; // override (idea - use shift+S to save with relative)
  AssignFile(SongFile, Name);
  Rewrite(SongFile);

  WriteLn(SongFile, '#TITLE:' + Song.Title + '');
  WriteLn(SongFile, '#ARTIST:' + Song.Artist);

  if Song.Creator     <> '' then    WriteLn(SongFile, '#CREATOR:'     + Song.Creator);
  if Song.Edition     <> 'Unknown' then WriteLn(SongFile, '#EDITION:' + Song.Edition);
  if Song.Genre       <> 'Unknown' then   WriteLn(SongFile, '#GENRE:' + Song.Genre);
  if Song.Language    <> 'Unknown' then    WriteLn(SongFile, '#LANGUAGE:'    + Song.Language);

  WriteLn(SongFile, '#MP3:' + Song.Mp3);

  if Song.Cover       <> '' then    WriteLn(SongFile, '#COVER:'       + Song.Cover);
  if Song.Background  <> '' then    WriteLn(SongFile, '#BACKGROUND:'  + Song.Background);
  if Song.Video       <> '' then    WriteLn(SongFile, '#VIDEO:'       + Song.Video);
  if Song.VideoGAP    <> 0  then    WriteLn(SongFile, '#VIDEOGAP:'    + FloatToStr(Song.VideoGAP));
  if Song.Resolution  <> 4  then    WriteLn(SongFile, '#RESOLUTION:'  + IntToStr(Song.Resolution));
  if Song.NotesGAP    <> 0  then    WriteLn(SongFile, '#NOTESGAP:'    + IntToStr(Song.NotesGAP));
  if Song.Start       <> 0  then    WriteLn(SongFile, '#START:'       + FloatToStr(Song.Start));
  if Song.Finish      <> 0  then    WriteLn(SongFile, '#END:'         + IntToStr(Song.Finish));
  if Relative               then    WriteLn(SongFile, '#RELATIVE:yes');

  WriteLn(SongFile, '#BPM:' + FloatToStr(Song.BPM[0].BPM / 4));
  WriteLn(SongFile, '#GAP:' + FloatToStr(Song.GAP));

  RelativeSubTime := 0;
  for B := 1 to High(CurrentSong.BPM) do
    WriteLn(SongFile, 'B ' + FloatToStr(CurrentSong.BPM[B].StartBeat) + ' ' + FloatToStr(CurrentSong.BPM[B].BPM/4));

  for C := 0 to Czesc.High do begin
    for N := 0 to Czesc.Czesc[C].HighNut do begin
      with Czesc.Czesc[C].Nuta[N] do begin


        //Golden + Freestyle Note Patch
        case Czesc.Czesc[C].Nuta[N].Wartosc of
          0: NoteState := 'F ';
          1: NoteState := ': ';
          2: NoteState := '* ';
        end; // case
        S := NoteState + IntToStr(Start-RelativeSubTime) + ' ' + IntToStr(Dlugosc) + ' ' + IntToStr(Ton) + ' ' + Tekst;


        WriteLn(SongFile, S);
      end; // with
    end; // N

    if C < Czesc.High then begin      // don't write end of last sentence
      if not Relative then
        S := '- ' + IntToStr(Czesc.Czesc[C+1].Start)
      else begin
        S := '- ' + IntToStr(Czesc.Czesc[C+1].Start - RelativeSubTime) +
          ' ' + IntToStr(Czesc.Czesc[C+1].Start - RelativeSubTime);
        RelativeSubTime := Czesc.Czesc[C+1].Start;
      end;
      WriteLn(SongFile, S);
    end;

  end; // C


  WriteLn(SongFile, 'E');
  CloseFile(SongFile);
end;

end.
