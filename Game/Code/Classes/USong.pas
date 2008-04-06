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
    UCatCovers,
    UXMLSong;

type

  TSingMode = ( smNormal, smPartyMode, smPlaylistRandom );

  TBPM = record
    BPM:        real;
    StartBeat:  real;
  end;

  {TSong = class
    FileLineNo  : integer;  //Line which is readed at Last, for error reporting

    procedure ParseNote(LineNumber: integer; TypeP: char; StartP, DurationP, NoteP: integer; LyricS: string);
    procedure NewSentence(LineNumberP: integer; Param1, Param2: integer);

    function ReadTXTHeader( const aFileName : WideString ): boolean;
    function ReadXMLHeader( const aFileName : WideString ): boolean;
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
    function    LoadXMLSong: boolean;
    function    Analyse(): boolean;
    function    AnalyseXML(): boolean;
    procedure   clear();
  end;    }

  TScore = record
    Name:       widestring;
    Score:      integer;
    Length:     string;
  end;

  AScore = Array[0..4] of TScore;

  {*******************
    New TSong Class.
    Containing some Methods for DB communication
    but none for Song laoding, saving, these should be
    implemented by child classes
   *******************}
  TSong = class
    protected
      SongID:   Integer; //ID of this Song in the Song Database
      FolderID: Integer; //ID of the Folder containing this Song
      FileName: String;  //Filename of this Song w/o Path
      FilePath: String;  //Path of this Song

      Procedure ResetAttributes; virtual;  		//Reset all Attributes of this object
      Procedure ReadHeader; virtual; abstract;  //Reads Fileheader (Implemented by Child only)
      Procedure ReadFile;   virtual; abstract;  //Reads complete File (Header + Notes) (Implemented by Child only)
      Procedure WriteFile;  virtual; abstract;  //Writes complete File (Header + Notes) (Implemented by Child only)
      Procedure ReadFromDB; virtual;       		//Reads all available Information from DB
      Function  CheckDB: Integer; virtual; 		//Checks DB for Song. Result > 0 SongExists (ID Returned)
      Function  UpdateDB: Integer; virtual;		//Writes all Header Information set in this Song Object to DB. Returns ID (Required when Updated first Time)
      
      // procedures to manage the lyrics
      Procedure ResetLyrics;
      Procedure AddLyricLine(const PlayerID: Integer; const StartBeat: Integer; const RelativeBeat: Integer = -1);
      Procedure AddNote(const PlayerID: Integer; const NoteType: Char; const NoteStart, NoteLength, NoteTone: Integer; const NoteText: WideString);
      Function SolmizatLyrics(const NoteTone: Integer; const NoteText: WideString): WideString;
  public
      //Required Information
      Title:      widestring;
      Artist:     widestring;

      Mp3:        widestring; //Full Path to MP3

      Creator:    widestring;

      Resolution: integer;
      BPM:        array of TBPM;
      GAP:        real; // in miliseconds

      Base    : array[0..1] of integer;
      Mult    : integer;
      MultBPM : integer;

      //Some Files
      Cover:      widestring; //Full Path to Cover
      CoverID:    Integer;    //ID of Cover in Covers Cache
      Background: widestring; //Full Path to BG
      Video:      widestring; //Full Path to Video
      VideoGAP:   real;


      //Additional Information
      NotesGAP:   integer;
      Start:      real; // in seconds
      Finish:     integer; // in miliseconds
      Relative:   boolean;

      //Sorting
      Genre:      widestring;
      Edition:    widestring;
      Language:   widestring;


      constructor Create(const Path: String = ''; const FolderID: Integer = 0);
      Procedure LoadbyFile(const Path: String);
      Procedure LoadbyID(const ID: Integer);
      Procedure SavetoFile(const Filename: String = '');

      Function Score(const Difficulty: Byte): AScore;

      Procedure Clear;
  end;

implementation

uses
  TextGL,
  UIni,
  UMusic;  //needed for Lines

var
  RelativPosition: array [0..1] of Integer;

constructor TSong.Create(const Path: String = ''; const FolderID: Integer = 0);
begin
  If (Length(Path) > 0) AND (FolderID > 0) then
  begin //Read Song Infos from File or DB
    FilePath := ExtractFilePath(Path);
    FileName := ExtractFileName(Path);
    Self.FolderID := FolderID;
    SongID := CheckDB;

    If (SongID = 0) then //Check if File has changed
    begin
      ResetAttributes;
      ReadHeader;
      SongID := UpdateDB;
      //Get CoverID from Covers by Covers.AddCover(Coverscache checks if the Cover requires update
    end;
  end
  else
  begin
    ResetAttributes;
  end;
end;

Procedure TSong.LoadbyFile(const Path: String);
begin
  //Call all Functions to Load From File
  //Set Song and Folder ID and Update DB if required
  //Get CoverID from Covers by Covers.AddCover(Coverscache checks if the Cover requires update
end;

Procedure TSong.LoadbyID(const ID: Integer);
begin
  //Call all Functions to Load Song by ID
  //Read all Information from file
  //Get CoverID from Covers by Covers.AddCover(Coverscache checks if the Cover requires update
end;

Procedure TSong.SavetoFile(const Filename: String = '');
begin
  //Save HeaderInformation and Notes to File. If File = '' use File that was read from
end;

Function TSong.Score(const Difficulty: Byte): AScore;
begin
  //Return Score of Difficulty(0..2) Easy to Difficult
end;

Procedure TSong.Clear;
begin
  ResetAttributes;
end;

//--------
// Reset all Attributes of this object
//--------
Procedure TSong.ResetAttributes;
begin
  SongID		:= 0;
  FolderID		:= 0;
  FileName		:= '';
  FilePath		:= '';

  Title			:= '';
  Artist		:= '';
  Mp3			:= '';
  SetLength(BPM, 0);

  Creator		:= '';
  Resolution	:= 0;
  GAP			:= 0;

  Base[0]    	:= 100;
  Base[1]		:= 100;
  
  Mult    		:= 1;
  MultBPM 		:= 4;

  Cover			:= '';
  CoverID		:= 0;
  Background	:= '';
  Video			:= '';
  VideoGAP		:= 0;

  NotesGAP		:= 0;
  Start			:= 0;
  Finish		:= 0;
  Relative		:= False;

  Genre			:= '';
  Edition		:= '';
  Language		:= '';
end;

//--------
// Reads all available Information from DB
//--------
Procedure TSong.ReadFromDB;
begin
  // to- do
end;


//--------
// Checks DB for Song. Result > 0 SongExists (ID Returned)
//--------
Function  TSong.CheckDB: Integer;
begin
  // to- do
end;


//--------
// Writes all Header Information set in this Song Object to DB. Returns ID (Required when Updated first Time)
//--------
Function  TSong.UpdateDB: Integer;
begin
  // to- do
end;

Procedure TSong.ResetLyrics;
var
  i: Integer;
begin
  for i := 0 to High(Lines) do
  begin
    SetLength(Lines[i].Line, 0);
    Lines[i].High := -1;
  end;
  
  for i := 0 to High(RelativPosition) do
  begin
    RelativPosition[i] := 0;
  end;
end;

Procedure TSong.AddLyricLine(const PlayerID: Integer; const StartBeat: Integer; const RelativeBeat: Integer = -1);
var
  NewLineIdx: Integer;
begin
  NewLineIdx := High(Lines[PlayerID].Line) + 1;
  
  with Lines[PlayerID] do
  begin
    // recent added line is not the last of the song
    if (NewLineIdx > 0) then
      Lines[PlayerID].Line[NewLineIdx - 1].LastLine := False;
    
    // if last line, has no notes, ignore the new line (except for the RelativeBeat)
    if (NewLineIdx < 1) OR (Lines[PlayerID].Line[NewLineIdx - 1].HighNote > -1) then
    begin
      // create lyric line and update references
      SetLength(Line, NewLineIdx);
      High :=   NewLineIdx;
      Number := Number + 1;
      with Line[High] do
      begin  
        // default values
        TotalNotes := 0;
        HighNote := -1;
        LastLine := True;
        BaseNote := 100;
          
        // set start beat count of this new line
        Start := (RelativPosition[PlayerID] + StartBeat) * Mult;
      end;
    end;
    
    if Relative then
    begin
      if RelativeBeat >= 0 then
        // if this is a relativ song, we have to update the relativ offset
        RelativPosition[PlayerID] := (RelativPosition[PlayerID] + RelativeBeat) * Mult
      else
        RelativPosition[PlayerID] := (RelativPosition[PlayerID] + StartBeat) * Mult;
    end;
  end;
end;  

Procedure TSong.AddNote(const PlayerID: Integer; const NoteType: Char; const NoteStart, NoteLength, NoteTone: Integer; const NoteText: WideString);
begin
  if (High(Lines[PlayerID].Line) < 0) then
    AddLyricLine(PlayerID, NoteStart, 0);
  
  with Lines[PlayerID].Line[Lines[PlayerID].High] do begin
    // array of Notes expand to have space for new Note
    HighNote := HighNote + 1;
    TotalNotes := TotalNotes + 1;
    SetLength(Note, HighNote + 1);
    
    with Note[HighNote] do
    begin
      Start := (RelativPosition[PlayerID] + NoteStart) * Mult;
      Length := NoteLength * Mult;
      Tone := NoteTone;
      Text := SolmizatLyrics(NoteTone, NoteText);
      Lyric := Lyric + Text;
      
      // identify lowest note of line
      if Tone < BaseNote then
        BaseNote := Tone;
    end;
    
    case NoteType of
      'F': Note[HighNote].NoteType := ntFreestyle;      
      ':': Note[HighNote].NoteType := ntNormal;
      '*': Note[HighNote].NoteType := ntGolden;
    end;
    
    // calculate total score value
    if (Note[HighNote].NoteType = ntNormal) then
    begin
      // normal notes
      Lines[PlayerID].ScoreValue := Lines[PlayerID].ScoreValue + Note[HighNote].Length;
      TotalNotes := TotalNotes + Note[HighNote].Length;
    end    
    else if (Note[HighNote].NoteType = ntGolden) then
    begin
      // golden notes
      Lines[PlayerID].ScoreValue := Lines[PlayerID].ScoreValue + (Note[HighNote].Length * 2);
      TotalNotes := TotalNotes + (Note[HighNote].Length * 2);
    end;
    
    // finish of the line
    End_ := Note[HighNote].Start + Note[HighNote].Length;
  end; // with
end;

Function TSong.SolmizatLyrics(const NoteTone: Integer; const NoteText: WideString): WideString;
begin
  Result := NoteText;
  
  case Ini.Solmization of
    1:  // european
      case (NoteTone mod 12) of
        0..1:  	Result := 'do ';
        2..3:  	Result := 're ';
        4:  	Result := 'mi ';
        5..6:  	Result := 'fa ';
        7..8:  	Result := 'sol ';
        9..10:  Result := 'la ';
        11:  	Result := 'si ';
      end;
    
    2:  // japanese
      case (NoteTone mod 12) of
        0..1:  	Result := 'do ';
        2..3:  	Result := 're ';
        4:     	Result := 'mi ';
        5..6:  	Result := 'fa ';
        7..8:  	Result := 'so ';
        9..10: 	Result := 'la ';
        11:    	Result := 'shi ';
      end;

    3:  // american
      case (NoteTone mod 12) of
        0..1:  	Result := 'do ';
        2..3:  	Result := 're ';
        4:  	Result := 'mi ';
        5..6:  	Result := 'fa ';
        7..8:  	Result := 'sol ';
        9..10:  Result := 'la ';
        11:  	Result := 'ti ';
      end;
  end; // case Ini.Solmization
end;


{constructor TSong.create( const aFileName : WideString );
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
}
(*
//Load TXT Song
function TSong.LoadSong(): boolean;

var
  TempC:    char;
  Text:    string;
  CP:       integer; // Current Player (0 or 1)
  Count:      integer;
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
      ReadLn(SongFile, Text);
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
    for Count := 0 to High(Lines) do begin
      SetLength(Lines[Count].Line, 1);
      Lines[Count].High := 0;
      Lines[Count].Number := 1;
      Lines[Count].Current := 0;
      Lines[Count].Resolution := self.Resolution;
      Lines[Count].NotesGAP   := self.NotesGAP;
      Lines[Count].Line[0].IlNut := 0;
      Lines[Count].Line[0].HighNote := -1;
      Lines[Count].Line[0].LastLine := False;
    end;

  //  TempC := ':';
  //  TempC := Text[1]; // read from backup variable, don't use default ':' value

    while (TempC <> 'E') AND (not EOF(SongFile)) do
    begin

      if (TempC = ':') or (TempC = '*') or (TempC = 'F') then begin
        // read notes
        Read(SongFile, Param1);
        Read(SongFile, Param2);
        Read(SongFile, Param3);
        Read(SongFile, ParamS);


       //Check for ZeroNote
       if Param2 = 0 then Log.LogError('Found ZeroNote at "'+TempC+' '+IntToStr(Param1)+' '+IntToStr(Param2)+' '+IntToStr(Param3)+ParamS+'" -> Note ignored!') else
       begin
        // add notes
        if not Both then
          // P1
          ParseNote(0, TempC, (Param1+Rel[0]) * Mult, Param2 * Mult, Param3, ParamS)
        else begin
          // P1 + P2
          ParseNote(0, TempC, (Param1+Rel[0]) * Mult, Param2 * Mult, Param3, ParamS);
          ParseNote(1, TempC, (Param1+Rel[1]) * Mult, Param2 * Mult, Param3, ParamS);
        end;
       end; //Zeronote check
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

        Read(SongFile, Text);
        self.BPM[High(self.BPM)].BPM := StrToFloat(Text);
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
         Lines[CP].Line[Lines[CP].High].TotalNotes := Lines[CP].Line[Lines[CP].High].TotalNotes + Lines[CP].Line[Lines[CP].High].Note[I].Length * Lines[CP].Line[Lines[CP].High].Note[I].NoteType;
        end;
        //Total Notes Patch End
      end else begin
        for Count := 0 to High(Lines) do begin
          Lines[Count].Line[Lines[Count].High].BaseNote := Base[Count];
          Lines[Count].Line[Lines[Count].High].LyricWidth := glTextWidth(PChar(Lines[Count].Line[Lines[Count].High].Lyric));
          //Total Notes Patch
          Lines[Count].Line[Lines[Count].High].TotalNotes := 0;
          for I := low(Lines[Count].Line[Lines[Count].High].Note) to high(Lines[Count].Line[Lines[Count].High].Note) do
          begin
            Lines[Count].Line[Lines[Count].High].TotalNotes := Lines[Count].Line[Lines[Count].High].TotalNotes + Lines[Count].Line[Lines[Count].High].Note[I].Length * Lines[Count].Line[Lines[Count].High].Note[I].NoteType;
          end;
          //Total Notes Patch End
        end;
      end;
      Read(SongFile, TempC);
      Inc(FileLineNo);
    end; // while} {

    for Count := 0 to High(Lines) do begin
      Lines[Count].Line[High(Lines[Count].Line)].LastLine := True;
    end;

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

//Load XML Song
function TSong.LoadXMLSong(): boolean;
var
  //TempC:    char;
  Text:    string;
  CP:       integer; // Current Player (0 or 1)
  Count:      integer;
  Both:     boolean;
  Param1:   integer;
  Param2:   integer;
  Param3:   integer;
  ParamS:   string;
  I,J,X:      Integer;

  NoteType: Char;
  SentenceEnd, Rest, Time: Integer;
  Parser: TParser;
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


  Parser := TParser.Create;
  Parser.Settings.DashReplacement := '~';


    for Count := 0 to High(Lines) do begin
      SetLength(Lines[Count].Line, 1);
      Lines[Count].High := 0;
      Lines[Count].Number := 1;
      Lines[Count].Current := 0;
      Lines[Count].Resolution := self.Resolution;
      Lines[Count].NotesGAP   := self.NotesGAP;
      Lines[Count].Line[0].IlNut := 0;
      Lines[Count].Line[0].HighNote := -1;
      Lines[Count].Line[0].LastLine := False;
    end;


//Try to Parse the Song
If Parser.ParseSong(Path + PathDelim + FileName) then
    begin
//      Writeln('XML Inputfile Parsed succesful');
    //Start write parsed information to Song
      //Notes Part
      For I := 0 to High(Parser.SongInfo.Sentences) do
      begin
        //Add Notes
        For J := 0 to High(Parser.SongInfo.Sentences[I].Notes) do
        begin
          Case Parser.SongInfo.Sentences[I].Notes[J].NoteTyp of
            NT_Normal: NoteType := ':';
            NT_Golden: NoteType := '*';
            NT_Freestyle: NoteType := 'F';
          end;

           Param1:=Parser.SongInfo.Sentences[I].Notes[J].Start;       //Note Start
           Param2:=Parser.SongInfo.Sentences[I].Notes[J].Duration;    //Note Duration
           Param3:=Parser.SongInfo.Sentences[I].Notes[J].Tone;        //Note Tone
           ParamS:=' ' + Parser.SongInfo.Sentences[I].Notes[J].Lyric; //Note Lyric


           if not Both then
            // P1
            ParseNote(0, NoteType, (Param1+Rel[0]) * Mult, Param2 * Mult, Param3, ParamS)
           else begin
            // P1 + P2
            ParseNote(0, NoteType, (Param1+Rel[0]) * Mult, Param2 * Mult, Param3, ParamS);
            ParseNote(1, NoteType, (Param1+Rel[1]) * Mult, Param2 * Mult, Param3, ParamS);
           end;


           if not Both then
           begin
            Lines[CP].Line[Lines[CP].High].BaseNote := Base[CP];
            Lines[CP].Line[Lines[CP].High].LyricWidth := glTextWidth(PChar(Lines[CP].Line[Lines[CP].High].Lyric));
            //Total Notes Patch
            Lines[CP].Line[Lines[CP].High].TotalNotes := 0;
            for X := low(Lines[CP].Line[Lines[CP].High].Note) to high(Lines[CP].Line[Lines[CP].High].Note) do
            begin
             Lines[CP].Line[Lines[CP].High].TotalNotes := Lines[CP].Line[Lines[CP].High].TotalNotes + Lines[CP].Line[Lines[CP].High].Note[X].Length * Lines[CP].Line[Lines[CP].High].Note[X].NoteType;
            end;
            //Total Notes Patch End
          end else begin
            for Count := 0 to High(Lines) do begin
              Lines[Count].Line[Lines[Count].High].BaseNote := Base[Count];
              Lines[Count].Line[Lines[Count].High].LyricWidth := glTextWidth(PChar(Lines[Count].Line[Lines[Count].High].Lyric));
              //Total Notes Patch
              Lines[Count].Line[Lines[Count].High].TotalNotes := 0;
              for X := low(Lines[Count].Line[Lines[Count].High].Note) to high(Lines[Count].Line[Lines[Count].High].Note) do
              begin
                Lines[Count].Line[Lines[Count].High].TotalNotes := Lines[Count].Line[Lines[Count].High].TotalNotes + Lines[Count].Line[Lines[Count].High].Note[X].Length * Lines[Count].Line[Lines[Count].High].Note[X].NoteType;
              end;
              //Total Notes Patch End
           end;
          end;



        end; //J Forloop

        //Add Sentence break
        If (I < High(Parser.SongInfo.Sentences)) then
        begin

          SentenceEnd := Parser.SongInfo.Sentences[I].Notes[High(Parser.SongInfo.Sentences[I].Notes)].Start + Parser.SongInfo.Sentences[I].Notes[High(Parser.SongInfo.Sentences[I].Notes)].Duration;
          Rest := Parser.SongInfo.Sentences[I+1].Notes[0].Start - SentenceEnd;

          //Calculate Time
          Case Rest of
            0, 1: Time := Parser.SongInfo.Sentences[I+1].Notes[0].Start;
            2: Time := Parser.SongInfo.Sentences[I+1].Notes[0].Start - 1;
            3: Time := Parser.SongInfo.Sentences[I+1].Notes[0].Start - 2;
            else
              If (Rest >= 4) then
                Time := SentenceEnd + 2
              Else //Sentence overlapping :/
                Time := Parser.SongInfo.Sentences[I+1].Notes[0].Start;
          end;
          // new sentence
          if not Both then
            // P1
            NewSentence(0, (Time + Rel[0]) * Mult, Param2)
          else begin
            // P1 + P2
            NewSentence(0, (Time + Rel[0]) * Mult, Param2);
            NewSentence(1, (Time + Rel[1]) * Mult, Param2);
         end;

        end;
      end;
    //End write parsed information to Song
	Parser.Free;
	end
    else
    begin
      Log.LogError('Could not parse Inputfile: ' + Path + PathDelim + FileName);
      exit;
    end;

    for Count := 0 to High(Lines) do begin
      Lines[Count].Line[High(Lines[Count].Line)].LastLine := True;
    end;

  Result := true;
end;

function TSong.ReadXMLHeader(const aFileName : WideString): boolean;
var
  Line, Identifier, Value: String;
  Temp        : word;
  Done        : byte;
  Parser      : TParser;
begin
  Result := true;
  Done   := 0;

//Parse XML
    Parser := TParser.Create;
    Parser.Settings.DashReplacement := '~';


    If Parser.ParseSong(self.Path + self.FileName) then
    begin
        //-----------
        //Required Attributes
        //-----------

        //Title
          self.Title := Parser.SongInfo.Header.Title;

          //Add Title Flag to Done
          Done := Done or 1;

        //Artist
          self.Artist := Parser.SongInfo.Header.Artist;

          //Add Artist Flag to Done
          Done := Done or 2;

        //MP3 File //Test if Exists
        self.Mp3 := platform.FindSongFile(Path, '*.mp3');
        if (FileExists(self.Path + self.Mp3)) then
          //Add Mp3 Flag to Done
          Done := Done or 4;

        //Beats per Minute
        SetLength(self.BPM, 1);
          self.BPM[0].StartBeat := 0;

          self.BPM[0].BPM := (Parser.SongInfo.Header.BPM * Parser.SongInfo.Header.Resolution/4  ) * Mult * MultBPM;

          if self.BPM[0].BPM <> 0 then
            //Add BPM Flag to Done
            Done := Done or 8;

        //---------
        //Additional Header Information
        //---------

        // Gap
          self.GAP := Parser.SongInfo.Header.Gap;

        //Cover Picture
          self.Cover := platform.FindSongFile(Path, '*[CO].jpg');

        //Background Picture
          self.Background := platform.FindSongFile(Path, '*[BG].jpg');

        // Video File
        //    self.Video := Value

        // Video Gap
        //  self.VideoGAP := song_StrtoFloat( Value )

        //Genre Sorting
          self.Genre := Parser.SongInfo.Header.Genre;

        //Edition Sorting
          self.Edition := Parser.SongInfo.Header.Edition;

        //Year Sorting
        //Parser.SongInfo.Header.Year

        //Language Sorting
        self.Language := Parser.SongInfo.Header.Language;
    end else
          Log.LogError('File Incomplete or not SingStar XML (A): ' + aFileName);

    Parser.Free;

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
      Log.LogError('File Incomplete or not SingStar XML (B - '+ inttostr(Done) +'): ' + aFileName);
  end;

end;


function TSong.ReadTXTHeader(const aFileName : WideString): boolean;

  function song_StrtoFloat( aValue : String ) : Extended;
  var
    lValue : String;
// lOldDecimalSeparator : Char; // Auto Removed, Unused Variable
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

        {$IFDEF UTF8_FILENAMES}   {
		if ((Identifier = 'MP3') or (Identifier = 'BACKGROUND') or (Identifier = 'COVER') or (Identifier = 'VIDEO')) then
		  Value := Utf8Encode(Value);
        {$ENDIF}   {

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

        // Gap
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
  {$IFDEF FPC} {
  setlength( BPM, 0 );
  {$ELSE} {
  BPM    := nil;
  {$ENDIF} {

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


function TSong.AnalyseXML(): boolean;
begin
  Result := False;

  //Reset LineNo
  FileLineNo := 0;

  //Clear old Song Header
  self.clear;

  //Read Header
  Result := self.ReadXMLHeader( FileName );

end;  *)

end.
