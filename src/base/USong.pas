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
 * $URL$
 * $Id$
 *}

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

  TScore = record
    Name:       WideString;
    Score:      integer;
    Length:     string;
  end;

  TSong = class
    FileLineNo  : integer;  // line, which is read last, for error reporting

    procedure ParseNote(LineNumber: integer; TypeP: char; StartP, DurationP, NoteP: integer; LyricS: string);
    procedure NewSentence(LineNumberP: integer; Param1, Param2: integer);

    function ReadTXTHeader( const aFileName : WideString ): boolean;
    function ReadXMLHeader( const aFileName : WideString ): boolean;
  public
    Path:       WideString;
    Folder:     WideString; // for sorting by folder
    fFileName,
    FileName:   WideString;

    // sorting methods
    //Category:   array of WideString; // TODO: do we need this?
    Genre:      WideString;
    Edition:    WideString;
    Language:   WideString;

    Title:      WideString;
    Artist:     WideString;

    Text:       WideString;
    Creator:    WideString;

    Cover:      WideString;
    CoverTex:   TTexture;
    Mp3:        WideString;
    Background: WideString;
    Video:      WideString;
    VideoGAP:   real;
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

    SongFile: TextFile;   // all procedures in this unit operate on this file

    Base    : array[0..1] of integer;
    Rel     : array[0..1] of integer;
    Mult    : integer;
    MultBPM : integer;

    LastError: String;
    function  GetErrorLineNo: integer;
    property  ErrorLineNo: integer read GetErrorLineNo;


    constructor Create  (); overload;
    constructor Create  ( const aFileName : WideString ); overload;
    function    LoadSong: boolean;
    function    LoadXMLSong: boolean;
    function    Analyse(): boolean;
    function    AnalyseXML(): boolean;
    procedure   Clear();
  end;

implementation

uses
  TextGL,
  UIni,
  UMusic,  //needed for Lines
  UMain;   //needed for Player

constructor TSong.Create();
begin
  inherited;
end;

constructor TSong.Create( const aFileName : WideString );
begin
  inherited Create();

  Mult    := 1;
  MultBPM := 4;
  fFileName := aFileName;

  LastError := '';

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

{function TSong.LoadSong(): boolean;
begin

end;   }

//Load TXT Song
function TSong.LoadSong(): boolean;

var
  TempC:    char;
  Text:     string;
  CP:       integer; // Current Player (0 or 1)
  Count:    integer;
  Both:     boolean;
  Param1:   integer;
  Param2:   integer;
  Param3:   integer;
  ParamS:   string;
  I:        integer;

begin
  Result := false;
  LastError := '';

  if not FileExists(Path + PathDelim + FileName) then
  begin
    LastError := 'ERROR_CORRUPT_SONG_FILE_NOT_FOUND';
    Log.LogError('File not found: "' + Path + PathDelim + FileName + '"', 'TSong.LoadSong()');
    exit;
  end;

  MultBPM           := 4; // multiply beat-count of note by 4
  Mult              := 1; // accuracy of measurement of note
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

    FileLineNo := 0;
    //Search for Note Begining
    repeat
      ReadLn(SongFile, Text);
      Inc(FileLineNo);

      if (EoF(SongFile)) then
      begin //Song File Corrupted - No Notes
        CloseFile(SongFile);
        Log.LogError('Could not load txt File, no Notes found: ' + FileName);
        LastError := 'ERROR_CORRUPT_SONG_NO_NOTES';
        Exit;
      end;
      Read(SongFile, TempC);
    until ((TempC = ':') or (TempC = 'F') or (TempC = '*'));

    SetLength(Lines, 2);
    for Count := 0 to High(Lines) do
    begin
      Lines[Count].High := 0;
      Lines[Count].Number := 1;
      Lines[Count].Current := 0;
      Lines[Count].Resolution := self.Resolution;
      Lines[Count].NotesGAP   := self.NotesGAP;
      Lines[Count].ScoreValue := 0;

      //Add first line and set some standard values to fields
      //see procedure NewSentence for further explantation
      //concerning most of these values
      SetLength(Lines[Count].Line, 1);
      Lines[Count].Line[0].HighNote := -1;
      Lines[Count].Line[0].LastLine := false;
      Lines[Count].Line[0].BaseNote := High(Integer);
      Lines[Count].Line[0].TotalNotes := 0;
    end;

    while (TempC <> 'E') and (not EOF(SongFile)) do
    begin

      if (TempC = ':') or (TempC = '*') or (TempC = 'F') then
      begin
        // read notes
        Read(SongFile, Param1);
        Read(SongFile, Param2);
        Read(SongFile, Param3);
        Read(SongFile, ParamS);

        //Check for ZeroNote
        if Param2 = 0 then
          Log.LogError('Found ZeroNote at "'+TempC+' '+IntToStr(Param1)+' '+IntToStr(Param2)+' '+IntToStr(Param3)+ParamS+'" -> Note ignored!')
        else
        begin
         // add notes
         if not Both then
           // P1
           ParseNote(0, TempC, (Param1+Rel[0]) * Mult, Param2 * Mult, Param3, ParamS)
         else
         begin
           // P1 + P2
           ParseNote(0, TempC, (Param1+Rel[0]) * Mult, Param2 * Mult, Param3, ParamS);
           ParseNote(1, TempC, (Param1+Rel[1]) * Mult, Param2 * Mult, Param3, ParamS);
         end;
        end; //Zeronote check
      end // if

      else if TempC = '-' then
      begin
        // reads sentence
        Read(SongFile, Param1);
        if self.Relative then
          Read(SongFile, Param2); // read one more data for relative system

        // new sentence
        if not Both then
          // P1
          NewSentence(0, (Param1 + Rel[0]) * Mult, Param2)
        else
        begin
          // P1 + P2
          NewSentence(0, (Param1 + Rel[0]) * Mult, Param2);
          NewSentence(1, (Param1 + Rel[1]) * Mult, Param2);
        end;
      end // if

      else if TempC = 'B' then
      begin
        SetLength(self.BPM, Length(self.BPM) + 1);
        Read(SongFile, self.BPM[High(self.BPM)].StartBeat);
        self.BPM[High(self.BPM)].StartBeat := self.BPM[High(self.BPM)].StartBeat + Rel[0];

        Read(SongFile, Text);
        self.BPM[High(self.BPM)].BPM := StrToFloat(Text);
        self.BPM[High(self.BPM)].BPM := self.BPM[High(self.BPM)].BPM * Mult * MultBPM;
      end;

      ReadLn(SongFile); //Jump to next line in File, otherwise the next Read would catch the linebreak(e.g. #13 #10 on win32)

      Read(SongFile, TempC);
      Inc(FileLineNo);
    end; // while}

    CloseFile(SongFile);

    for I := 0 to High(Lines) do
    begin
      if ((Both) or (I = 0)) then
      begin
        if (Length(Lines[I].Line) < 2) then
        begin
          LastError := 'ERROR_CORRUPT_SONG_NO_BREAKS';
          Log.LogError('Error Loading File, Can''t find any Linebreaks: "' + fFileName + '"');
          exit;
        end;

        if (Lines[I].Line[Lines[I].High].HighNote < 0) then
        begin
          SetLength(Lines[I].Line, Lines[I].Number - 1);
          Lines[I].High := Lines[I].High - 1;
          Lines[I].Number := Lines[I].Number - 1;
          Log.LogError('Error loading Song, sentence w/o note found in last line before E: ' + Filename);
        end;
      end;
    end;

    for Count := 0 to High(Lines) do
    begin
      if (High(Lines[Count].Line) >= 0) then
        Lines[Count].Line[High(Lines[Count].Line)].LastLine := true;
    end;
  except
    try
      CloseFile(SongFile);
    except

    end;

    LastError := 'ERROR_CORRUPT_SONG_ERROR_IN_LINE';
    Log.LogError('Error Loading File: "' + fFileName + '" in Line ' + inttostr(FileLineNo));
    exit;
  end;

  Result := true;
end;

//Load XML Song
function TSong.LoadXMLSong(): boolean;

var
  //TempC:     char;
  Text:      string;
  CP:        integer; // Current Player (0 or 1)
  Count:     integer;
  Both:      boolean;
  Param1:    integer;
  Param2:    integer;
  Param3:    integer;
  ParamS:    string;
  I, J:      integer;
  NoteIndex: integer;

  NoteType:  char;
  SentenceEnd, Rest, Time: integer;
  Parser: TParser;

begin
  Result := false;
  LastError := '';

  if not FileExists(Path + PathDelim + FileName) then
  begin
    Log.LogError('File not found: "' + Path + PathDelim + FileName + '"', 'TSong.LoadSong()');
    exit;
  end;

  MultBPM           := 4; // multiply beat-count of note by 4
  Mult              := 1; // accuracy of measurement of note
  Lines[0].ScoreValue := 0;
  self.Relative     := false;
  Rel[0]            := 0;
  CP                := 0;
  Both              := false;

  if Length(Player) = 2 then
    Both := true;

  Parser := TParser.Create;
  Parser.Settings.DashReplacement := '~';

  for Count := 0 to High(Lines) do
  begin
    Lines[Count].High := 0;
      Lines[Count].Number := 1;
      Lines[Count].Current := 0;
      Lines[Count].Resolution := self.Resolution;
      Lines[Count].NotesGAP   := self.NotesGAP;
      Lines[Count].ScoreValue := 0;

      //Add first line and set some standard values to fields
      //see procedure NewSentence for further explantation
      //concerning most of these values
      SetLength(Lines[Count].Line, 1);
      Lines[Count].Line[0].HighNote := -1;
      Lines[Count].Line[0].LastLine := false;
      Lines[Count].Line[0].BaseNote := High(Integer);
      Lines[Count].Line[0].TotalNotes := 0;
  end;

  //Try to Parse the Song

  if Parser.ParseSong(Path + PathDelim + FileName) then
  begin
    //Writeln('XML Inputfile Parsed succesful');

    //Start write parsed information to Song
    //Notes Part
    for I := 0 to High(Parser.SongInfo.Sentences) do
    begin
      //Add Notes
      for J := 0 to High(Parser.SongInfo.Sentences[I].Notes) do
      begin
        case Parser.SongInfo.Sentences[I].Notes[J].NoteTyp of
          NT_Normal:    NoteType := ':';
          NT_Golden:    NoteType := '*';
          NT_Freestyle: NoteType := 'F';
        end;

        Param1:=Parser.SongInfo.Sentences[I].Notes[J].Start;       //Note Start
        Param2:=Parser.SongInfo.Sentences[I].Notes[J].Duration;    //Note Duration
        Param3:=Parser.SongInfo.Sentences[I].Notes[J].Tone;        //Note Tone
        ParamS:=' ' + Parser.SongInfo.Sentences[I].Notes[J].Lyric; //Note Lyric

        if not Both then
          // P1
          ParseNote(0, NoteType, (Param1+Rel[0]) * Mult, Param2 * Mult, Param3, ParamS)
        else
        begin
          // P1 + P2
          ParseNote(0, NoteType, (Param1+Rel[0]) * Mult, Param2 * Mult, Param3, ParamS);
          ParseNote(1, NoteType, (Param1+Rel[1]) * Mult, Param2 * Mult, Param3, ParamS);
        end;

      end; //J Forloop

      //Add Sentence break
      if (I < High(Parser.SongInfo.Sentences)) then
      begin
        SentenceEnd := Parser.SongInfo.Sentences[I].Notes[High(Parser.SongInfo.Sentences[I].Notes)].Start + Parser.SongInfo.Sentences[I].Notes[High(Parser.SongInfo.Sentences[I].Notes)].Duration;
        Rest := Parser.SongInfo.Sentences[I+1].Notes[0].Start - SentenceEnd;

        //Calculate Time
        case Rest of
          0, 1: Time := Parser.SongInfo.Sentences[I+1].Notes[0].Start;
          2:    Time := Parser.SongInfo.Sentences[I+1].Notes[0].Start - 1;
          3:    Time := Parser.SongInfo.Sentences[I+1].Notes[0].Start - 2;
          else
            if (Rest >= 4) then
              Time := SentenceEnd + 2
            else //Sentence overlapping :/
              Time := Parser.SongInfo.Sentences[I+1].Notes[0].Start;
        end;
        // new sentence
        if not Both then // P1
          NewSentence(0, (Time + Rel[0]) * Mult, Param2)
        else
        begin // P1 + P2
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

  for Count := 0 to High(Lines) do
  begin
    Lines[Count].Line[High(Lines[Count].Line)].LastLine := true;
  end;

  Result := true;
end;

function TSong.ReadXMLHeader(const aFileName : WideString): boolean;

var
  //Line, Identifier, Value: string;
  //Temp        : word;
  Done        : byte;
  Parser      : TParser;

begin
  Result := true;
  Done   := 0;

  //Parse XML
  Parser := TParser.Create;
  Parser.Settings.DashReplacement := '~';

  if Parser.ParseSong(self.Path + self.FileName) then
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
    //Add Mp3 Flag to Done
    if (FileExists(self.Path + self.Mp3)) then
      Done := Done or 4;

    //Beats per Minute
    SetLength(self.BPM, 1);
    self.BPM[0].StartBeat := 0;

    self.BPM[0].BPM := (Parser.SongInfo.Header.BPM * Parser.SongInfo.Header.Resolution/4  ) * Mult * MultBPM;

    //Add BPM Flag to Done
    if self.BPM[0].BPM <> 0 then
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
  end
  else
    Log.LogError('File Incomplete or not SingStar XML (A): ' + aFileName);

  Parser.Free;

  //Check if all Required Values are given
  if (Done <> 15) then
  begin
    Result := false;
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

  function song_StrtoFloat( aValue : string ) : extended;

  var
    lValue : string;

  begin
    lValue := aValue;

    if (Pos(',', lValue) <> 0) then
      lValue[Pos(',', lValue)] := '.';

    Result := StrToFloatDef(lValue, 0);
  end;

var
  Line, Identifier, Value: string;
  Temp        : word;
  Done        : byte;

begin
  Result := true;
  Done   := 0;

  //Read first Line
  ReadLn (SongFile, Line);

  if (Length(Line) <= 0) then
  begin
    Log.LogError('File Starts with Empty Line: ' + aFileName);
    Result := false;
    Exit;
  end;

  //Read Lines while Line starts with # or its empty
  while (Length(Line) = 0) or (Line[1] = '#') do
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
        else if (Identifier = 'MP3') and (FileExists(self.Path + Value)) then
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
        else if (Identifier = 'RELATIVE') and (uppercase(Value) = 'YES') then
          self.Relative := true;

      end;
    end;

    if not EOF(SongFile) then
      ReadLn (SongFile, Line)
    else
    begin
      Result := false;
      Log.LogError('File Incomplete or not Ultrastar TxT (A): ' + aFileName);
      break;
    end;

  end;

  if self.Cover = '' then
    self.Cover := platform.FindSongFile(Path, '*[CO].jpg');

  //Check if all Required Values are given
  if (Done <> 15) then
  begin
    Result := false;
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

function  TSong.GetErrorLineNo: integer;
begin
  if (LastError='ERROR_CORRUPT_SONG_ERROR_IN_LINE') then
    Result := FileLineNo
  else
    Result := -1;
end;

procedure TSong.ParseNote(LineNumber: integer; TypeP: char; StartP, DurationP, NoteP: integer; LyricS: string);

begin
  case Ini.Solmization of
    1:  // european
      begin
        case (NoteP mod 12) of
          0..1:   LyricS := ' do ';
          2..3:   LyricS := ' re ';
          4:      LyricS := ' mi ';
          5..6:   LyricS := ' fa ';
          7..8:   LyricS := ' sol ';
          9..10:  LyricS := ' la ';
          11:     LyricS := ' si ';
        end;
      end;
    2:  // japanese
      begin
        case (NoteP mod 12) of
          0..1:   LyricS := ' do ';
          2..3:   LyricS := ' re ';
          4:      LyricS := ' mi ';
          5..6:   LyricS := ' fa ';
          7..8:   LyricS := ' so ';
          9..10:  LyricS := ' la ';
          11:     LyricS := ' shi ';
        end;
      end;
    3:  // american
      begin
        case (NoteP mod 12) of
          0..1:   LyricS := ' do ';
          2..3:   LyricS := ' re ';
          4:      LyricS := ' mi ';
          5..6:   LyricS := ' fa ';
          7..8:   LyricS := ' sol ';
          9..10:  LyricS := ' la ';
          11:     LyricS := ' ti ';
        end;
      end;
  end; // case

  with Lines[LineNumber].Line[Lines[LineNumber].High] do
  begin
    SetLength(Note, Length(Note) + 1);
    HighNote := High(Note);

    Note[HighNote].Start := StartP;
    if HighNote = 0 then
    begin
      if Lines[LineNumber].Number = 1 then
        Start := -100;
        //Start := Note[HighNote].Start;
    end;

    Note[HighNote].Length := DurationP;

    // back to the normal system with normal, golden and now freestyle notes
    case TypeP of
      'F':  Note[HighNote].NoteType := ntFreestyle;
      ':':  Note[HighNote].NoteType := ntNormal;
      '*':  Note[HighNote].NoteType := ntGolden;
    end;

    //add this notes value ("notes length" * "notes scorefactor") to the current songs entire value
    Inc(Lines[LineNumber].ScoreValue, Note[HighNote].Length * ScoreFactor[Note[HighNote].NoteType]);

    //and to the current lines entire value
    Inc(TotalNotes, Note[HighNote].Length * ScoreFactor[Note[HighNote].NoteType]);


    Note[HighNote].Tone := NoteP;

    //if a note w/ a deeper pitch then the current basenote is found
    //we replace the basenote w/ the current notes pitch
    if Note[HighNote].Tone < BaseNote then
      BaseNote := Note[HighNote].Tone;

    //delete the space that seperates the notes pitch from its lyrics
    //it is left in the LyricS string because Read("some ordinal type") will
    //set the files pointer to the first whitespace character after the
    //ordinal string. Trim is no solution because it would cut the spaces
    //that seperate the words of the lyrics, too.
    Delete(LyricS, 1, 1);

    Note[HighNote].Text := LyricS;
    Lyric := Lyric + Note[HighNote].Text;

    End_ := Note[HighNote].Start + Note[HighNote].Length;
  end; // with
end;

procedure TSong.NewSentence(LineNumberP: integer; Param1, Param2: integer);

var
  I: integer;

begin

  if (Lines[LineNumberP].Line[Lines[LineNumberP].High].HighNote  <> -1) then
  begin //create a new line
    SetLength(Lines[LineNumberP].Line, Lines[LineNumberP].Number + 1);
    Inc(Lines[LineNumberP].High);
    Inc(Lines[LineNumberP].Number);
  end
  else
  begin //use old line if it there were no notes added since last call of NewSentence
    Log.LogError('Error loading Song, sentence w/o note found in line ' + InttoStr(FileLineNo) + ': ' + Filename);
  end;

  Lines[LineNumberP].Line[Lines[LineNumberP].High].HighNote := -1;

  //set the current lines value to zero
  //it will be incremented w/ the value of every added note
  Lines[LineNumberP].Line[Lines[LineNumberP].High].TotalNotes := 0;

  //basenote is the pitch of the deepest note, it is used for note drawing.
  //if a note with a less value than the current sentences basenote is found,
  //basenote will be set to this notes pitch. Therefore the initial value of
  //this field has to be very high.
  Lines[LineNumberP].Line[Lines[LineNumberP].High].BaseNote := High(Integer);


  if self.Relative then
  begin
    Lines[LineNumberP].Line[Lines[LineNumberP].High].Start := Param1;
    Rel[LineNumberP] := Rel[LineNumberP] + Param2;
  end
  else
    Lines[LineNumberP].Line[Lines[LineNumberP].High].Start := Param1;

  Lines[LineNumberP].Line[Lines[LineNumberP].High].LastLine := false;
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
  BPM    := nil;
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

  Relative := false;
end;

function TSong.Analyse(): boolean;

begin
  Result := false;

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
  Result := false;

  //Reset LineNo
  FileLineNo := 0;

  //Clear old Song Header
  self.clear;

  //Read Header
  Result := self.ReadXMLHeader( FileName );

end;

end.
