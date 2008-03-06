unit USong;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
    {$IFDEF MSWINDOWS}
    Windows,
    {$ELSE}
      {$IFNDEF DARWIN}
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

  TSingMode = ( smNormal, smPartyMode, smPlaylistRandom );

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
  UMusic,  // needed for Lines .. ( whatever that is ) 
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
    Log.LogError('File not found: "' + Path + PathDelim + FileName + '"', 'TSong.LoadSong()');
    exit;
  end;

  MultBPM           := 4; // multiply beat-count of note by 4
  Mult              := 1; // accuracy of measurement of note
  Base[0]           := 100; // high number
  Lines[0].NoteType := 0;
  self.Relative     := false;
  Rel[0]            := 0;
  CP                := 0;
  Both              := false;

  if Length(Player) = 2 then
    Both := true;

  try
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

    SetLength(Lines, 2);
    for Pet := 0 to High(Lines) do begin
      SetLength(Lines[Pet].Line, 1);
      Lines[Pet].High := 0;
      Lines[Pet].Ilosc := 1;
      Lines[Pet].Current := 0;
      Lines[Pet].Resolution := self.Resolution;
      Lines[Pet].NotesGAP   := self.NotesGAP;
      Lines[Pet].Line[0].IlNut := 0;
      Lines[Pet].Line[0].HighNote := -1;
    end;

  //  TempC := ':';
  //  TempC := Tekst[1]; // read from backup variable, don't use default ':' value

    while (TempC <> 'E') AND (not EOF(SongFile)) do
    begin

      if (TempC = ':') or (TempC = '*') or (TempC = 'F') then begin
        // read notes
        Read(SongFile, Param1);
        Read(SongFile, Param2);
        Read(SongFile, Param3);
        Read(SongFile, ParamS);

        // add notes
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
        Lines[CP].Line[Lines[CP].High].BaseNote := Base[CP];
        Lines[CP].Line[Lines[CP].High].LyricWidth := glTextWidth(PChar(Lines[CP].Line[Lines[CP].High].Lyric));
        //Total Notes Patch
        Lines[CP].Line[Lines[CP].High].TotalNotes := 0;
        for I := low(Lines[CP].Line[Lines[CP].High].Note) to high(Lines[CP].Line[Lines[CP].High].Note) do
        begin
         Lines[CP].Line[Lines[CP].High].TotalNotes := Lines[CP].Line[Lines[CP].High].TotalNotes + Lines[CP].Line[Lines[CP].High].Note[I].Lenght * Lines[CP].Line[Lines[CP].High].Note[I].NoteType;
        end;
        //Total Notes Patch End
      end else begin
        for Pet := 0 to High(Lines) do begin
          Lines[Pet].Line[Lines[Pet].High].BaseNote := Base[Pet];
          Lines[Pet].Line[Lines[Pet].High].LyricWidth := glTextWidth(PChar(Lines[Pet].Line[Lines[Pet].High].Lyric));
          //Total Notes Patch
          Lines[Pet].Line[Lines[Pet].High].TotalNotes := 0;
          for I := low(Lines[Pet].Line[Lines[Pet].High].Note) to high(Lines[Pet].Line[Lines[Pet].High].Note) do
          begin
            Lines[Pet].Line[Lines[Pet].High].TotalNotes := Lines[Pet].Line[Lines[Pet].High].TotalNotes + Lines[Pet].Line[Lines[Pet].High].Note[I].Lenght * Lines[Pet].Line[Lines[Pet].High].Note[I].NoteType;
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

  function song_StrtoFloat( aValue : String ) : Extended;
  var
    lValue : String;
    lOldDecimalSeparator : Char;
  begin
    lValue := aValue;
    
    if (Pos(',', lValue) <> 0) then
      lValue[Pos(',', lValue)] := '.';

    Result := StrToFloatDef(lValue, 0);
  end;

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

        {$IFDEF UTF8_FILENAMES}
		if ((Identifier = 'MP3') or (Identifier = 'BACKGROUND') or (Identifier = 'COVER') or (Identifier = 'VIDEO')) then
		  Value := Utf8Encode(Value);  
        {$ENDIF}

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
          SetLength(self.BPM, 1);
          self.BPM[0].StartBeat := 0;

          self.BPM[0].BPM := song_StrtoFloat( Value ) * Mult * MultBPM;

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
          self.GAP := song_StrtoFloat( Value )

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
          self.VideoGAP := song_StrtoFloat( Value )

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
          self.Start := song_StrtoFloat( Value )

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

  with Lines[NrCzesci].Line[Lines[NrCzesci].High] do begin
    SetLength(Note, Length(Note) + 1);
    IlNut := IlNut + 1;
    HighNote := HighNote + 1;
    Melody.IlNut := Melody.IlNut + 1;

    Note[HighNote].Start := StartP;
    if IlNut = 1 then begin
      StartNote := Note[HighNote].Start;
      if Lines[NrCzesci].Ilosc = 1 then
        Start := -100;
//        Start := Note[HighNote].Start;
    end;

    Note[HighNote].Lenght := DurationP;
    Melody.NoteLenght := Melody.NoteLenght + Note[HighNote].Lenght;

    // back to the normal system with normal, golden and now freestyle notes
    case TypeP of
      'F':  Note[HighNote].NoteType := 0;
      ':':  Note[HighNote].NoteType := 1;
      '*':  Note[HighNote].NoteType := 2;
    end;

    Lines[NrCzesci].NoteType := Lines[NrCzesci].NoteType + Note[HighNote].Lenght * Note[HighNote].NoteType;

    Note[HighNote].Tone := NoteP;
    if Note[HighNote].Tone < Base[NrCzesci] then Base[NrCzesci] := Note[HighNote].Tone;
    Note[HighNote].ToneGamus := Note[HighNote].ToneGamus mod 12;

    Note[HighNote].Text := Copy(LyricS, 2, 100);
    Lyric := Lyric + Note[HighNote].Text;

    if TypeP = 'F' then
      Note[HighNote].FreeStyle := true;

    End_ := Note[HighNote].Start + Note[HighNote].Lenght;
  end; // with
end;

procedure TSong.NewSentence(NrCzesciP: integer; Param1, Param2: integer);
var
I: Integer;
begin

  // stara czesc //Alter Satz //Update Old Part
  Lines[NrCzesciP].Line[Lines[NrCzesciP].High].BaseNote := Base[NrCzesciP];
  Lines[NrCzesciP].Line[Lines[NrCzesciP].High].LyricWidth := glTextWidth(PChar(Lines[NrCzesciP].Line[Lines[NrCzesciP].High].Lyric));

  //Total Notes Patch
  Lines[NrCzesciP].Line[Lines[NrCzesciP].High].TotalNotes := 0;
  for I := low(Lines[NrCzesciP].Line[Lines[NrCzesciP].High].Note) to high(Lines[NrCzesciP].Line[Lines[NrCzesciP].High].Note) do
  begin
    Lines[NrCzesciP].Line[Lines[NrCzesciP].High].TotalNotes := Lines[NrCzesciP].Line[Lines[NrCzesciP].High].TotalNotes + Lines[NrCzesciP].Line[Lines[NrCzesciP].High].Note[I].Lenght * Lines[NrCzesciP].Line[Lines[NrCzesciP].High].Note[I].NoteType;
  end;
  //Total Notes Patch End


  // nowa czesc //Neuer Satz //Update New Part
  SetLength(Lines[NrCzesciP].Line, Lines[NrCzesciP].Ilosc + 1);
  Lines[NrCzesciP].High := Lines[NrCzesciP].High + 1;
  Lines[NrCzesciP].Ilosc := Lines[NrCzesciP].Ilosc + 1;
  Lines[NrCzesciP].Line[Lines[NrCzesciP].High].HighNote := -1;

  if self.Relative then
  begin
    Lines[NrCzesciP].Line[Lines[NrCzesciP].High].Start := Param1;
    Rel[NrCzesciP] := Rel[NrCzesciP] + Param2;
  end
  else
    Lines[NrCzesciP].Line[Lines[NrCzesciP].High].Start := Param1;

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
