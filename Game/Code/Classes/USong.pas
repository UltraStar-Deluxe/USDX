unit USong;

interface

uses
     {$IFDEF MSWINDOWS}
       Windows,
       {$ifdef Delphi}
       DirWatch,
       {$endif}
     {$ELSE}
       {$IFNDEF DARWIN}
//         oldlinux,
         syscall,
       {$ENDIF}
      baseunix,
      UnixType,
     {$ENDIF}
     SysUtils,
     Classes,
  	 UPlatform,
     ULog,
     UTexture,
     UCommon,
	 {$IFDEF DARWIN}
	 cthreads,
	 {$ENDIF}
	 {$IFDEF USE_PSEUDO_THREAD}
	 PseudoThread,
	 {$ENDIF}
     UCatCovers;

type


  TBPM = record
    BPM:        real;
    StartBeat:  real;
  end;

  TScore = record
    Name:       widestring;
    Score:      integer;
    Length:     string;
  end;

  TSong = class

    FileLineNo  : integer;  //Line which is readed at Last, for error reporting

    procedure ParseNote(NrCzesci: integer; TypeP: char; StartP, DurationP, NoteP: integer; LyricS: string);
    procedure NewSentence(NrCzesciP: integer; Param1, Param2: integer);

    function ReadTXTHeader( const aFileName : WideString ): boolean;
  public

    Path:       widestring;
    Folder:     widestring; // for sorting by folder
    fFileName,
    FileName:   widestring;

    // sorting methods
    Category:   array of widestring; // I think I won't need this
    Genre:      widestring;
    Edition:    widestring;
    Language:   widestring; // 0.5.0: new

    Title:      widestring;
    Artist:     widestring;

    Text:       widestring;
    Creator:    widestring;

    Cover:      widestring;
    CoverTex:   TTexture;
    Mp3:        widestring;
    Background: widestring;
    Video:      widestring;
    VideoGAP:   real;
    VideoLoaded: boolean; // 0.5.0: true if the video has been loaded
    NotesGAP:   integer;
    Start:      real; // in seconds
    Finish:     integer; // in miliseconds
    Relative:   boolean;
    Resolution: integer;
    BPM:        array of TBPM;
    GAP:        real; // in miliseconds

    Score:      array[0..2] of array of TScore;

    // these are used when sorting is enabled
    Visible:    boolean; // false if hidden, true if visible
    Main:       boolean; // false for songs, true for category buttons
    OrderNum:   integer; // has a number of category for category buttons and songs
    OrderTyp:   integer; // type of sorting for this button (0=name)
    CatNumber:  integer; // Count of Songs in Category for Cats and Number of Song in Category for Songs

    SongFile: TextFile;   // all procedures in this unit operates on this file

    Base    : array[0..1] of integer;
    Rel     : array[0..1] of integer;
    Mult    : integer;
    MultBPM : integer;

    constructor create  ( const aFileName : WideString );
    function    LoadSong: boolean;
    function    Analyse(): boolean;
    procedure   clear();
  end;

implementation

uses
  TextGL,
  UIni,
  UMusic,  // needed for Czesci .. ( whatever that is ) 
  UMain;   //needed for Player

constructor TSong.create( const aFileName : WideString );
begin
  Mult    := 1;
  MultBPM := 4;

  fFileName := aFileName;

  if fileexists( aFileName ) then
  begin
    self.Path     := ExtractFilePath( aFileName );
    self.Folder   := ExtractFilePath( aFileName );
    self.FileName := ExtractFileName( aFileName );

(*
    if ReadTXTHeader( aFileName ) then
    begin
      LoadSong();
    end
    else
    begin
      Log.LogError('Error Loading SongHeader, abort Song Loading');
      Exit;
    end;
*)
  end;

end;

function TSong.LoadSong(): boolean;
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

  if not FileExists(Path + PathDelim + FileName) then
  begin
    Log.LogError('File not found: "' + Path + PathDelim + FileName + '"', 'WczytajCzesci');
    exit;
  end;

  try
    MultBPM           := 4; // 4 - mnoznik dla czasu nut
    Mult              := 1; // 4 - dokladnosc pomiaru nut
    Base[0]           := 100; // high number
    Czesci[0].Wartosc := 0;
    self.Relative     := false;
    Rel[0]            := 0;
    CP                := 0;
    Both              := false;
    
    if Length(Player) = 2 then
      Both := true;

    // Open song file for reading.....
    FileMode := fmOpenRead;
    AssignFile(SongFile, fFileName);
    Reset(SongFile);

    //Clear old Song Header
    if (self.Path = '') then
      self.Path := ExtractFilePath(FileName);

    if (self.FileName = '') then
      self.Filename := ExtractFileName(FileName);
      
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
        Log.LogError('Could not load txt File, no Notes found: ' + FileName);
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
      Czesci[Pet].Resolution := self.Resolution;
      Czesci[Pet].NotesGAP   := self.NotesGAP;
      Czesci[Pet].Czesc[0].IlNut := 0;
      Czesci[Pet].Czesc[0].HighNut := -1;
    end;

  //  TempC := ':';
  //  TempC := Tekst[1]; // read from backup variable, don't use default ':' value

    while (TempC <> 'E') AND (not EOF(SongFile)) do
    begin

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

      if TempC = '-' then
      begin
        // reads sentence
        Read(SongFile, Param1);
        if self.Relative then Read(SongFile, Param2); // read one more data for relative system

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

      if TempC = 'B' then
      begin
        SetLength(self.BPM, Length(self.BPM) + 1);
        Read(SongFile, self.BPM[High(self.BPM)].StartBeat);
        self.BPM[High(self.BPM)].StartBeat := self.BPM[High(self.BPM)].StartBeat + Rel[0];

        Read(SongFile, Tekst);
        self.BPM[High(self.BPM)].BPM := StrToFloat(Tekst);
        self.BPM[High(self.BPM)].BPM := self.BPM[High(self.BPM)].BPM * Mult * MultBPM;
      end;


      if not Both then
      begin
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

    Log.LogError('Error Loading File: "' + fFileName + '" in Line ' + inttostr(FileLineNo));
    exit;
  end;

  Result := true;
end;

function TSong.ReadTXTHeader(const aFileName : WideString): boolean;
var
  Line, Identifier, Value: String;
  Temp        : word;
  Done        : byte;
begin
  Result := true;
  Done   := 0;

  //Read first Line
  ReadLn (SongFile, Line);

  if (Length(Line)<=0) then
  begin
    Log.LogError('File Starts with Empty Line: ' + aFileName);
    Result := False;
    Exit;
  end;

  //Read Lines while Line starts with # or its empty
  While ( Length(Line) = 0   ) OR
        ( Line[1]      = '#' ) DO
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
          self.Title := Value;

          //Add Title Flag to Done
          Done := Done or 1;
        end

        //Artist
        else if (Identifier = 'ARTIST') then
        begin
          self.Artist := Value;

          //Add Artist Flag to Done
          Done := Done or 2;
        end

        //MP3 File //Test if Exists
        else if (Identifier = 'MP3') AND
            (FileExists(self.Path + Value)) then
        begin
          self.Mp3 := Value;

          //Add Mp3 Flag to Done
          Done := Done or 4;
        end

        //Beats per Minute
        else if (Identifier = 'BPM') then
        begin
          // Replace . with ,
          if (Pos('.', Value) <> 0) then
            Value[Pos('.', Value)] := ',';

          SetLength(self.BPM, 1);
          self.BPM[0].StartBeat := 0;

          self.BPM[0].BPM := StrtoFloatDef(Value, 0) * Mult * MultBPM;

          if self.BPM[0].BPM <> 0 then
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

          self.GAP := StrtoFloatDef (Value, 0);
        end

        //Cover Picture
        else if (Identifier = 'COVER') then
          self.Cover := Value

        //Background Picture
        else if (Identifier = 'BACKGROUND') then
          self.Background := Value

        // Video File
        else if (Identifier = 'VIDEO') then
        begin
          if (FileExists(self.Path + Value)) then
            self.Video := Value
          else
            Log.LogError('Can''t find Video File in Song: ' + aFileName);
        end

        // Video Gap
        else if (Identifier = 'VIDEOGAP') then
        begin
          // Replace . with ,
          if (Pos('.', Value) <> 0) then
            Value[Pos('.', Value)] := ',';

          self.VideoGAP := StrtoFloatDef (Value, 0);
        end

        //Genre Sorting
        else if (Identifier = 'GENRE') then
          self.Genre := Value

        //Edition Sorting
        else if (Identifier = 'EDITION') then
          self.Edition := Value

        //Creator Tag
        else if (Identifier = 'CREATOR') then
          self.Creator := Value

        //Language Sorting
        else if (Identifier = 'LANGUAGE') then
          self.Language := Value

        // Song Start
        else if (Identifier = 'START') then
        begin
          // Replace . with ,
          if (Pos('.', Value) <> 0) then
            Value[Pos('.', Value)] := ',';

          self.Start := StrtoFloatDef(Value, 0);
        end

        // Song Ending
        else if (Identifier = 'END') then
          TryStrtoInt(Value, self.Finish)

        // Resolution
        else if (Identifier = 'RESOLUTION') then
          TryStrtoInt(Value, self.Resolution)

        // Notes Gap
        else if (Identifier = 'NOTESGAP') then
          TryStrtoInt(Value, self.NotesGAP)
        // Relative Notes
        else if (Identifier = 'RELATIVE') AND (uppercase(Value) = 'YES') then
          self.Relative := True;

      end;
    end;

    if not EOf(SongFile) then
      ReadLn (SongFile, Line)
    else
    begin
      Result := False;
      Log.LogError('File Incomplete or not Ultrastar TxT (A): ' + aFileName);
      break;
    end;

  end;

  if self.Cover = '' then
    self.Cover := platform.FindSongFile(Path, '*[CO].jpg');

  //Check if all Required Values are given
  if (Done <> 15) then
  begin
    Result := False;
    if (Done and 8) = 0 then      //No BPM Flag
      Log.LogError('BPM Tag Missing: ' + self.FileName)
    else if (Done and 4) = 0 then //No MP3 Flag
      Log.LogError('MP3 Tag/File Missing: ' + self.FileName)
    else if (Done and 2) = 0 then //No Artist Flag
      Log.LogError('Artist Tag Missing: ' + self.FileName)
    else if (Done and 1) = 0 then //No Title Flag
      Log.LogError('Title Tag Missing: ' + self.FileName)
    else //unknown Error
      Log.LogError('File Incomplete or not Ultrastar TxT (B - '+ inttostr(Done) +'): ' + aFileName);
  end;

end;

procedure TSong.ParseNote(NrCzesci: integer; TypeP: char; StartP, DurationP, NoteP: integer; LyricS: string);
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

procedure TSong.NewSentence(NrCzesciP: integer; Param1, Param2: integer);
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

  if self.Relative then
  begin
    Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Start := Param1;
    Rel[NrCzesciP] := Rel[NrCzesciP] + Param2;
  end
  else
    Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Start := Param1;

  Base[NrCzesciP] := 100; // high number
end;

procedure TSong.clear();
begin
  //Main Information
  Title  := '';
  Artist := '';

  //Sortings:
  Genre    := 'Unknown';
  Edition  := 'Unknown';
  Language := 'Unknown'; //Language Patch

  //Required Information
  Mp3    := '';
  {$IFDEF FPC}
  setlength( BPM, 0 );
  {$ELSE}
  BPM    := 0;
  {$ENDIF}

  GAP    := 0;
  Start  := 0;
  Finish := 0;

  //Additional Information
  Background := '';
  Cover      := '';
  Video      := '';
  VideoGAP   := 0;
  NotesGAP   := 0;
  Resolution := 4;
  Creator    := '';

end;

function TSong.Analyse(): boolean;
begin
  Result := False;

  //Reset LineNo
  FileLineNo := 0;

  //Open File and set File Pointer to the beginning
  AssignFile(SongFile, self.Path + self.FileName);

  try
  Reset(SongFile);

  //Clear old Song Header
  self.clear;

  //Read Header
  Result := self.ReadTxTHeader( FileName )

  //And Close File
  finally
    CloseFile(SongFile);
  end;
end;



end.
