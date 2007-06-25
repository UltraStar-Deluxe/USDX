unit UFiles;

interface

uses USongs, SysUtils, ULog, UMusic;

procedure     InitializePaths; //Function sets All Absolute Paths eg. for Songs
function    ReadTXTHeader(var Song: TSong): boolean; //Reads Standard TXT Header
function    AnalyseFile(var Song: TSong): boolean; //Analyse Song File and Read Header
procedure   ClearSong(var Song: TSong); //Clears Song Header values

//Methodes Loading and Saving Songfiles
procedure ResetSingTemp;
procedure ParseNote(NrCzesci: integer; TypeP: char; StartP, DurationP, NoteP: integer; LyricS: string);
procedure NewSentence(NrCzesciP: integer; Param1, Param2: integer);
function LoadSong(Name: string): boolean;
function SaveSong(Song: TSong; Czesc: TCzesci; Name: string; Relative: boolean): boolean;



var
  //Absolute Paths
  GamePath:         string;
  SoundPath:        string;
  SongPath:         string;
  LogPath:          string;
  ThemePath:        string;
  ScreenshotsPath:  string;
  CoversPath:       string;
  LanguagesPath:    string;
  PluginPath:       string;
  PlayListPath:     string;

  SongFile: TextFile;   // all procedures in this unit operates on this file
  FileLineNo: integer;  //Line which is readed at Last, for error reporting

  // variables available for all procedures
  Base:       array[0..1] of integer;
  Rel:        array[0..1] of integer;
  Mult:     integer = 1;
  MultBPM:  integer = 4;

implementation
uses TextGL, UIni, UMain;

//--------------------
// Function sets all Absolute Paths e.g. Song Path and makes sure the Directorys exist
//--------------------
procedure InitializePaths;
var
  Writeable: Boolean;
begin
  GamePath :=   ExtractFilePath(ParamStr(0));

  SoundPath :=  GamePath + 'Sounds\';
  SongPath :=   GamePath + 'Songs\';
  LogPath := GamePath;
  ThemePath := GamePath + 'Themes\';
  ScreenshotsPath := GamePath + 'Screenshots\';
  CoversPath := GamePath + 'Covers\';
  LanguagesPath := GamePath + 'Languages\';
  PluginPath := GamePath + 'Plugins\';
  PlaylistPath := GamePath + 'Playlists\';

  //After Setting Paths, make sure that Paths exist
  If not DirectoryExists(SoundPath) then
    Writeable := ForceDirectories(SoundPath);

  If Writeable And (not DirectoryExists(SongPath)) then
    Writeable := ForceDirectories(SongPath);

  If Writeable And (not DirectoryExists(ThemePath)) then
    Writeable := ForceDirectories(ThemePath);

  If Writeable And (not DirectoryExists(ScreenshotsPath)) then
    Writeable := ForceDirectories(ScreenshotsPath);

  If Writeable And (not DirectoryExists(CoversPath)) then
    Writeable := ForceDirectories(CoversPath);

  If Writeable And (not DirectoryExists(LanguagesPath)) then
    Writeable := ForceDirectories(LanguagesPath);

  If Writeable And (not DirectoryExists(PluginPath)) then
    Writeable := ForceDirectories(PluginPath);

  If Writeable And (not DirectoryExists(PlaylistPath)) then
    Writeable := ForceDirectories(PlaylistPath);

  if not Writeable then
    Log.LogError('Error: Dir is Readonly');

  DecimalSeparator := ',';
end;

//--------------------
// Clears Song Header values
//--------------------
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
  Song.Cover := '';
  Song.Video := '';
  Song.VideoGAP := 0;
  Song.NotesGAP := 0;
  Song.Resolution := 4;
  Song.Creator := '';
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
      Log.LogError('File Incomplete or not Ultrastar TxT: ' + Song.FileName);
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
    Log.LogError('File Incomplete or not Ultrastar TxT: ' + Song.FileName);
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

//--------------------
// Resets the temporary Sentence Arrays for each Player and some other Variables
//--------------------
procedure ResetSingTemp;
var
  Pet:  integer;
begin
  SetLength(Czesci, Length(Player));
  SetLength(AktSong.BPM, 0);
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
  AktSong.Path := '';
  AktSong.FileName := '';
end;

//--------------------
// Parses Note Infos and save them to Array
//--------------------
procedure ParseNote(NrCzesci: integer; TypeP: char; StartP, DurationP, NoteP: integer; LyricS: string);
var
  Space:  boolean;
begin
  case Ini.Solmization of
    1:  // european
      begin
        case (NoteP mod 12) of
          0..1:  LyricS := ' do ';
          2..3:  LyricS := ' re ';
          4:  LyricS := ' mi ';
          5..6:  LyricS := ' fa ';
          7..8:  LyricS := ' sol ';
          9..10:  LyricS := ' la ';
          11:  LyricS := ' si ';
        end;
      end;
    2:  // japanese
      begin
        case (NoteP mod 12) of
          0..1:  LyricS := ' do ';
          2..3:  LyricS := ' re ';
          4:  LyricS := ' mi ';
          5..6:  LyricS := ' fa ';
          7..8:  LyricS := ' so ';
          9..10:  LyricS := ' la ';
          11:  LyricS := ' shi ';
        end;
      end;
    3:  // american
      begin
        case (NoteP mod 12) of
          0..1:  LyricS := ' do ';
          2..3:  LyricS := ' re ';
          4:  LyricS := ' mi ';
          5..6:  LyricS := ' fa ';
          7..8:  LyricS := ' sol ';
          9..10:  LyricS := ' la ';
          11:  LyricS := ' ti ';
        end;
      end;
  end; // case

  with Czesci[NrCzesci].Czesc[Czesci[NrCzesci].High] do begin
    SetLength(Nuta, Length(Nuta) + 1);
    IlNut := IlNut + 1;
    HighNut := HighNut + 1;
    Muzyka.IlNut := Muzyka.IlNut + 1;

    Nuta[HighNut].Start := StartP;
    if IlNut = 1 then begin
      StartNote := Nuta[HighNut].Start;
      if Czesci[NrCzesci].Ilosc = 1 then
        Start := -100;
//        Start := Nuta[HighNut].Start;
    end;

    Nuta[HighNut].Dlugosc := DurationP;
    Muzyka.DlugoscNut := Muzyka.DlugoscNut + Nuta[HighNut].Dlugosc;

    // back to the normal system with normal, golden and now freestyle notes
    case TypeP of
      'F':  Nuta[HighNut].Wartosc := 0;
      ':':  Nuta[HighNut].Wartosc := 1;
      '*':  Nuta[HighNut].Wartosc := 2;
    end;

    Czesci[NrCzesci].Wartosc := Czesci[NrCzesci].Wartosc + Nuta[HighNut].Dlugosc * Nuta[HighNut].Wartosc;

    Nuta[HighNut].Ton := NoteP;
    if Nuta[HighNut].Ton < Base[NrCzesci] then Base[NrCzesci] := Nuta[HighNut].Ton;
    Nuta[HighNut].TonGamy := Nuta[HighNut].TonGamy mod 12;

    Nuta[HighNut].Tekst := Copy(LyricS, 2, 100);
    Lyric := Lyric + Nuta[HighNut].Tekst;

    if TypeP = 'F' then
      Nuta[HighNut].FreeStyle := true;

    Koniec := Nuta[HighNut].Start + Nuta[HighNut].Dlugosc;
  end; // with
end;

//--------------------
// Called when a new Sentence is found in the TXT File
//--------------------
procedure NewSentence(NrCzesciP: integer; Param1, Param2: integer);
var
I: Integer;
begin

  // stara czesc //Alter Satz //Update Old Part
  Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].BaseNote := Base[NrCzesciP];
  Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].LyricWidth := glTextWidth(PChar(Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Lyric));

  //Total Notes Patch
  Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].TotalNotes := 0;
  for I := low(Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Nuta) to high(Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Nuta) do
  begin
    Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].TotalNotes := Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].TotalNotes + Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Nuta[I].Dlugosc * Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Nuta[I].Wartosc;
  end;
  //Total Notes Patch End


  // nowa czesc //Neuer Satz //Update New Part
  SetLength(Czesci[NrCzesciP].Czesc, Czesci[NrCzesciP].Ilosc + 1);
  Czesci[NrCzesciP].High := Czesci[NrCzesciP].High + 1;
  Czesci[NrCzesciP].Ilosc := Czesci[NrCzesciP].Ilosc + 1;
  Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].HighNut := -1;

  if not AktSong.Relative then
    Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Start := Param1;

  if AktSong.Relative then begin
    Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Start := Param1;
    Rel[NrCzesciP] := Rel[NrCzesciP] + Param2;
  end;

  Base[NrCzesciP] := 100; // high number
end;

//--------------------
// Load a Song
//--------------------
function LoadSong(Name: string): boolean;
var
  TempC:    char;
  Tekst:    string;
  CP:       integer; // Current Player (0 or 1)
  Pet:      integer;
  Both:     boolean;
  Param1:   integer;
  Param2:   integer;
  Param3:   integer;
  ParamS:   string;
  I: Integer;
begin
  Result := false;

  if not FileExists(Name) then begin
    Log.LogError('File not found: "' + Name + '"', 'WczytajCzesci');
    exit;
  end;

  try
  MultBPM := 4; // 4 - mnoznik dla czasu nut
  Mult := 1; // 4 - dokladnosc pomiaru nut
  Base[0] := 100; // high number
//  Base[1] := 100; // high number
  Czesci[0].Wartosc := 0;
//  Czesci[1].Wartosc := 0; // here was the error in 0.3.2
  AktSong.Relative := false;

  Rel[0] := 0;
//  Rel[1] := 0;
  CP := 0;
  Both := false;
  if Length(Player) = 2 then Both := true;

  FileMode := fmOpenRead;
  AssignFile(SongFile, Name);
  Reset(SongFile);

  //Clear old Song Header
  ClearSong(AktSong);

  if (AktSong.Path = '') then
    AktSong.Path := ExtractFilePath(Name);

  if (AktSong.FileName = '') then
    AktSong.Filename := ExtractFileName(Name);
  //Read Header
  Result := ReadTxTHeader(AktSong);
  if not Result then
  begin
    CloseFile(SongFile);
    Log.LogError('Error Loading SongHeader, abort Song Loading');
    Exit;
  end;

  Result := False;

  Reset(SongFile);
  FileLineNo := 0;
  //Search for Note Begining
  repeat
    ReadLn(SongFile, Tekst);
    Inc(FileLineNo);
    
    if (EoF(SongFile)) then
    begin //Song File Corrupted - No Notes
      CloseFile(SongFile);
      Log.LogError('Could not load txt File, no Notes found: ' + Name);
      Result := False;
      Exit;
    end;
    Read(SongFile, TempC);
  until ((TempC = ':') or (TempC = 'F') or (TempC = '*'));

  SetLength(Czesci, 2);
  for Pet := 0 to High(Czesci) do begin
    SetLength(Czesci[Pet].Czesc, 1);
    Czesci[Pet].High := 0;
    Czesci[Pet].Ilosc := 1;
    Czesci[Pet].Akt := 0;
    Czesci[Pet].Resolution := AktSong.Resolution;
    Czesci[Pet].NotesGAP := AktSong.NotesGAP;
    Czesci[Pet].Czesc[0].IlNut := 0;
    Czesci[Pet].Czesc[0].HighNut := -1;
  end;

//  TempC := ':';
//  TempC := Tekst[1]; // read from backup variable, don't use default ':' value

  while (TempC <> 'E') AND (not EOF(SongFile)) do begin
    if (TempC = ':') or (TempC = '*') or (TempC = 'F') then begin
      // wczytuje nute
      Read(SongFile, Param1);
      Read(SongFile, Param2);
      Read(SongFile, Param3);
      Read(SongFile, ParamS);

      // dodaje nute
      if not Both then
        // P1
        ParseNote(0, TempC, (Param1+Rel[0]) * Mult, Param2 * Mult, Param3, ParamS)
      else begin
        // P1 + P2
        ParseNote(0, TempC, (Param1+Rel[0]) * Mult, Param2 * Mult, Param3, ParamS);
        ParseNote(1, TempC, (Param1+Rel[1]) * Mult, Param2 * Mult, Param3, ParamS);
      end;
    end; // if
    if TempC = '-' then begin
      // reads sentence
      Read(SongFile, Param1);
      if AktSong.Relative then Read(SongFile, Param2); // read one more data for relative system

      // new sentence
      if not Both then
        // P1
        NewSentence(0, (Param1 + Rel[0]) * Mult, Param2)
      else begin
        // P1 + P2
        NewSentence(0, (Param1 + Rel[0]) * Mult, Param2);
        NewSentence(1, (Param1 + Rel[1]) * Mult, Param2);
      end;

    end; // if

    if TempC = 'B' then begin
      SetLength(AktSong.BPM, Length(AktSong.BPM) + 1);
      Read(SongFile, AktSong.BPM[High(AktSong.BPM)].StartBeat);
      AktSong.BPM[High(AktSong.BPM)].StartBeat := AktSong.BPM[High(AktSong.BPM)].StartBeat + Rel[0];

      Read(SongFile, Tekst);
      AktSong.BPM[High(AktSong.BPM)].BPM := StrToFloat(Tekst);
      AktSong.BPM[High(AktSong.BPM)].BPM := AktSong.BPM[High(AktSong.BPM)].BPM * Mult * MultBPM;
    end;


    if not Both then begin
      Czesci[CP].Czesc[Czesci[CP].High].BaseNote := Base[CP];
      Czesci[CP].Czesc[Czesci[CP].High].LyricWidth := glTextWidth(PChar(Czesci[CP].Czesc[Czesci[CP].High].Lyric));
      //Total Notes Patch
      Czesci[CP].Czesc[Czesci[CP].High].TotalNotes := 0;
      for I := low(Czesci[CP].Czesc[Czesci[CP].High].Nuta) to high(Czesci[CP].Czesc[Czesci[CP].High].Nuta) do
      begin
       Czesci[CP].Czesc[Czesci[CP].High].TotalNotes := Czesci[CP].Czesc[Czesci[CP].High].TotalNotes + Czesci[CP].Czesc[Czesci[CP].High].Nuta[I].Dlugosc * Czesci[CP].Czesc[Czesci[CP].High].Nuta[I].Wartosc;
      end;
      //Total Notes Patch End
    end else begin
      for Pet := 0 to High(Czesci) do begin
        Czesci[Pet].Czesc[Czesci[Pet].High].BaseNote := Base[Pet];
        Czesci[Pet].Czesc[Czesci[Pet].High].LyricWidth := glTextWidth(PChar(Czesci[Pet].Czesc[Czesci[Pet].High].Lyric));
        //Total Notes Patch
        Czesci[Pet].Czesc[Czesci[Pet].High].TotalNotes := 0;
        for I := low(Czesci[Pet].Czesc[Czesci[Pet].High].Nuta) to high(Czesci[Pet].Czesc[Czesci[Pet].High].Nuta) do
        begin
          Czesci[Pet].Czesc[Czesci[Pet].High].TotalNotes := Czesci[Pet].Czesc[Czesci[Pet].High].TotalNotes + Czesci[Pet].Czesc[Czesci[Pet].High].Nuta[I].Dlugosc * Czesci[Pet].Czesc[Czesci[Pet].High].Nuta[I].Wartosc;
        end;
        //Total Notes Patch End
      end;
    end;

    Read(SongFile, TempC);
    Inc(FileLineNo);
  end; // while}

  CloseFile(SongFile);
  except
    try
      CloseFile(SongFile);
    except

    end;

    Log.LogError('Error Loading File: "' + Name + '" in Line ' + inttostr(FileLineNo));
    exit;
  end;

  Result := true;
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
  for B := 1 to High(AktSong.BPM) do
    WriteLn(SongFile, 'B ' + FloatToStr(AktSong.BPM[B].StartBeat) + ' ' + FloatToStr(AktSong.BPM[B].BPM/4));

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