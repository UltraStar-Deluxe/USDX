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
  UXMLSong,
  UUnicodeUtils,
  UTextEncoding,
  UFilesystem,
  UPath;

type

  TSingMode = ( smNormal, smPartyMode, smPlaylistRandom );

  TBPM = record
    BPM:        real;
    StartBeat:  real;
  end;

  TScore = record
    Name:       UTF8String;
    Score:      integer;
    Date:       UTF8String;
  end;

  { used to hold header tags that are not supported by this version of
    usdx (e.g. some tags from ultrastar 0.7.0) when songs are loaded in
    songeditor. They will be written the end of the song header }
  TCustomHeaderTag = record
    Tag: UTF8String;
    Content: UTF8String;
  end;

  TSong = class
  private
    FileLineNo  : integer;  // line, which is read last, for error reporting

    function DecodeFilename(Filename: RawByteString): IPath;
    function Solmizate(Note: integer; Type_: integer): string;
    procedure ParseNote(LineNumber: integer; TypeP: char; StartP, DurationP, NoteP: integer; LyricS: UTF8String);
    procedure NewSentence(LineNumberP: integer; Param1, Param2: integer);

    function ParseLyricStringParam(const Line: RawByteString; var LinePos: integer): RawByteString;
    function ParseLyricIntParam(const Line: RawByteString; var LinePos: integer): integer;
    function ParseLyricFloatParam(const Line: RawByteString; var LinePos: integer): extended;
    function ParseLyricCharParam(const Line: RawByteString; var LinePos: integer): AnsiChar;
    function ParseLyricText(const Line: RawByteString; var LinePos: integer): RawByteString;

    function ReadTXTHeader(SongFile: TTextFileStream; ReadCustomTags: Boolean): boolean;
    function ReadXMLHeader(const aFileName: IPath): boolean;

    function GetFolderCategory(const aFileName: IPath): UTF8String;
    function FindSongFile(Dir: IPath; Mask: UTF8String): IPath;
  public
    Path:         IPath; // kust path component of file (only set if file was found)
    Folder:       UTF8String; // for sorting by folder (only set if file was found)
    FileName:     IPath; // just name component of file (only set if file was found)

    // filenames
    Cover:      IPath;
    Mp3:        IPath;
    Background: IPath;
    Video:      IPath;

    // sorting methods
    Genre:      UTF8String;
    Edition:    UTF8String;
    Language:   UTF8String;
    Year:       Integer;

    Title:      UTF8String;
    Artist:     UTF8String;

    Creator:    UTF8String;

    CoverTex:   TTexture;

    VideoGAP:   real;
    NotesGAP:   integer;
    Start:      real; // in seconds
    Finish:     integer; // in miliseconds
    Relative:   boolean;
    Resolution: integer;
    BPM:        array of TBPM;
    GAP:        real; // in miliseconds

    Encoding:   TEncoding;

    CustomTags: array of TCustomHeaderTag;

    Score:      array[0..2] of array of TScore;

    // these are used when sorting is enabled
    Visible:    boolean; // false if hidden, true if visible
    Main:       boolean; // false for songs, true for category buttons
    OrderNum:   integer; // has a number of category for category buttons and songs
    OrderTyp:   integer; // type of sorting for this button (0=name)
    CatNumber:  integer; // Count of Songs in Category for Cats and Number of Song in Category for Songs

    Base    : array[0..1] of integer;
    Rel     : array[0..1] of integer;
    Mult    : integer;
    MultBPM : integer;

    LastError: AnsiString;
    function  GetErrorLineNo: integer;
    property  ErrorLineNo: integer read GetErrorLineNo;


    constructor Create(); overload;
    constructor Create(const aFileName : IPath); overload;
    function    LoadSong: boolean;
    function    LoadXMLSong: boolean;
    function    Analyse(const ReadCustomTags: Boolean = false): boolean;
    function    AnalyseXML(): boolean;
    procedure   Clear();
  end;

implementation

uses
  StrUtils,
  TextGL,
  UIni,
  UPathUtils,
  UMusic,  //needed for Lines
  UNote;   //needed for Player

const
  DEFAULT_ENCODING = encAuto;

constructor TSong.Create();
begin
  inherited;

  // to-do : special create for category "songs"
  //dirty fix to fix folders=on
  Self.Path     := PATH_NONE();
  Self.FileName := PATH_NONE();
  Self.Cover    := PATH_NONE();
  Self.Mp3      := PATH_NONE();
  Self.Background:= PATH_NONE();
  Self.Video    := PATH_NONE();
end;

// This may be changed, when we rewrite song select code.
// it is some kind of dirty, but imho the best possible
// solution as we do atm not support nested categorys.
// it works like the folder sorting in 1.0.1a
// folder is set to the first folder under the songdir
// so songs ~/.ultrastardx/songs/punk is in the same
// category as songs in shared/ultrastardx/songs are.
// note: folder is just the name of a category it has
//       nothing to do with the path used for file loading
function TSong.GetFolderCategory(const aFileName: IPath): UTF8String;
var
  I: Integer;
  CurSongPath: IPath;
  CurSongPathRel: IPath;
begin
  Result := 'Unknown'; //default folder category, if we can't locate the song dir

  for I := 0 to SongPaths.Count-1 do
  begin
    CurSongPath := SongPaths[I] as IPath;
    if (aFileName.IsChildOf(CurSongPath, false)) then
    begin
      if (aFileName.IsChildOf(CurSongPath, true)) then
      begin
        // songs are in the "root" of the songdir => use songdir for the categorys name
        Result := CurSongPath.RemovePathDelim.ToUTF8;
      end
      else
      begin
        // use the first subdirectory below CurSongPath as the category name
        CurSongPathRel := aFileName.GetRelativePath(CurSongPath.AppendPathDelim);
        Result := CurSongPathRel.SplitDirs[0].RemovePathDelim.ToUTF8;
      end;
      Exit;
    end;
  end;
end;

constructor TSong.Create(const aFileName: IPath);
begin
  inherited Create();

  Mult    := 1;
  MultBPM := 4;

  LastError := '';

  Self.Path     := aFileName.GetPath;
  Self.FileName := aFileName.GetName;
  Self.Folder   := GetFolderCategory(aFileName);

  (*
  if (aFileName.IsFile) then
  begin
    if ReadTXTHeader(aFileName) then
    begin
      LoadSong();
    end
    else
    begin
      Log.LogError('Error Loading SongHeader, abort Song Loading');
      Exit;
    end;
  end;
  *)
end;

function TSong.FindSongFile(Dir: IPath; Mask: UTF8String): IPath;
var
  Iter: IFileIterator;
  FileInfo: TFileInfo;
  FileName: IPath;
begin
  Iter := FileSystem.FileFind(Dir.Append(Mask), faDirectory);
  if (Iter.HasNext) then
    Result := Iter.Next.Name
  else
    Result := PATH_NONE;
end;

function TSong.DecodeFilename(Filename: RawByteString): IPath;
begin
  Result := UPath.Path(DecodeStringUTF8(Filename, Encoding));
end;

type
  EUSDXParseException = class(Exception);

{**
 * Parses the Line string starting from LinePos for a parameter.
 * Leading whitespace is trimmed, same applies to the first trailing whitespace.
 * After the call LinePos will point to the position after the first trailing
 * whitespace.
 *
 * Raises an EUSDXParseException if no string was found.
 *
 * Example:
 *   ParseLyricParam(Line:'Param0  Param1 Param2', LinePos:8, ...)
 *   -> Param:'Param1', LinePos:16 (= start of 'Param2')
 *}
function TSong.ParseLyricStringParam(const Line: RawByteString; var LinePos: integer): RawByteString;
var
  Start: integer;
  OldLinePos: integer;
const
  Whitespace = [#9, ' '];
begin
  OldLinePos := LinePos;

  Start := 0;
  while (LinePos <= Length(Line)) do
  begin
    if (Line[LinePos] in Whitespace) then
    begin
      // check for end of param
      if (Start > 0) then
        Break;
    end
    // check for beginning of param
    else if (Start = 0) then
    begin
      Start := LinePos;
    end;
    Inc(LinePos);
  end;

  // check if param was found
  if (Start = 0) then
  begin
    LinePos := OldLinePos;
    raise EUSDXParseException.Create('String expected');
  end
  else
  begin
    // copy param without trailing whitespace
    Result := Copy(Line, Start, LinePos-Start);
    // skip first trailing whitespace (if not at EOL)
    if (LinePos <= Length(Line)) then
      Inc(LinePos);
  end;
end;

function TSong.ParseLyricIntParam(const Line: RawByteString; var LinePos: integer): integer;
var
  Str: RawByteString;
  OldLinePos: integer;
begin
  OldLinePos := LinePos;
  Str := ParseLyricStringParam(Line, LinePos);

  if not TryStrToInt(Str, Result) then
  begin // on convert error
    Result := 0;
    LinePos := OldLinePos;
    raise EUSDXParseException.Create('Integer expected');
  end;
end;

function TSong.ParseLyricFloatParam(const Line: RawByteString; var LinePos: integer): extended;
var
  Str: RawByteString;
  OldLinePos: integer;
begin
  OldLinePos := LinePos;
  Str := ParseLyricStringParam(Line, LinePos);

  if not TryStrToFloat(Str, Result) then
  begin // on convert error
    Result := 0;
    LinePos := OldLinePos;
    raise EUSDXParseException.Create('Float expected');
  end;
end;

function TSong.ParseLyricCharParam(const Line: RawByteString; var LinePos: integer): AnsiChar;
var
  Str: RawByteString;
  OldLinePos: integer;
begin
  OldLinePos := LinePos;
  Str := ParseLyricStringParam(Line, LinePos);
  if (Length(Str) <> 1) then
  begin
    { to-do : decide what to do here
              usdx < 1.1 does not nead a whitespace after a char param
              so we may just write a warning to error.log and use the
              first non whitespace character instead of raising an
              exception that causes the song not to load. So the more
              error resistant code is:
                LinePos := OldLinePos + 1;
                // raise EUSDXParseException.Create('Character expected'); }
    LinePos := OldLinePos;
    raise EUSDXParseException.Create('Character expected');
  end;
  Result := Str[1];
end;

{**
 * Returns the rest of the line from LinePos as lyric text.
 * Leading and trailing whitespace is not trimmed.
 *}
function TSong.ParseLyricText(const Line: RawByteString; var LinePos: integer): RawByteString;
begin
  if (LinePos > Length(Line)) then
    Result := ''
  else
  begin
    Result := Copy(Line, LinePos, Length(Line)-LinePos+1);
    LinePos := Length(Line)+1;
  end;
end;

//Load TXT Song
function TSong.LoadSong(): boolean;
var
  CurLine: RawByteString;
  LinePos:  integer;
  Count:    integer;
  Both:     boolean;

  Param0:    AnsiChar;
  Param1:    integer;
  Param2:    integer;
  Param3:    integer;
  ParamLyric: UTF8String;

  I:        integer;
  NotesFound: boolean;
  SongFile: TTextFileStream;
  FileNamePath: IPath;
begin
  Result := false;
  LastError := '';

  FileNamePath := Path.Append(FileName);
  if not FileNamePath.IsFile() then
  begin
    LastError := 'ERROR_CORRUPT_SONG_FILE_NOT_FOUND';
    Log.LogError('File not found: "' + FileNamePath.ToNative + '"', 'TSong.LoadSong()');
    Exit;
  end;

  MultBPM           := 4; // multiply beat-count of note by 4
  Mult              := 1; // accuracy of measurement of note
  Rel[0]            := 0;
  Both              := false;

  if Length(Player) = 2 then
    Both := true;

  try
    // Open song file for reading.....
    SongFile := TMemTextFileStream.Create(FileNamePath, fmOpenRead);
    try
      //Search for Note Beginning
      FileLineNo := 0;
      NotesFound := false;
      while (SongFile.ReadLine(CurLine)) do
      begin
        Inc(FileLineNo);
        if (Length(CurLine) > 0) and (CurLine[1] in [':', 'F', '*']) then
        begin
          NotesFound := true;
          Break;
        end;
      end;

      if (not NotesFound) then
      begin //Song File Corrupted - No Notes
        Log.LogError('Could not load txt File, no notes found: ' + FileNamePath.ToNative);
        LastError := 'ERROR_CORRUPT_SONG_NO_NOTES';
        Exit;
      end;

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

      while true do
      begin
        LinePos := 1;

        Param0 := ParseLyricCharParam(CurLine, LinePos);
        if (Param0 = 'E') then
        begin
          Break
        end
        else if (Param0 in [':', '*', 'F']) then
        begin
          // read notes
          Param1 := ParseLyricIntParam(CurLine, LinePos);
          Param2 := ParseLyricIntParam(CurLine, LinePos);
          Param3 := ParseLyricIntParam(CurLine, LinePos);
          ParamLyric := ParseLyricText(CurLine, LinePos);

          //Check for ZeroNote
          if Param2 = 0 then
            Log.LogWarn(Format('"%s" in line %d: %s',
                  [FileNamePath.ToNative, FileLineNo, 'found note with length zero -> note ignored']), 'TSong.LoadSong')
            //Log.LogError('Found zero-length note at "'+Param0+' '+IntToStr(Param1)+' '+IntToStr(Param2)+' '+IntToStr(Param3)+ParamLyric+'" -> Note ignored!')
          else
          begin
           // add notes
           if not Both then
             // P1
             ParseNote(0, Param0, (Param1+Rel[0]) * Mult, Param2 * Mult, Param3, ParamLyric)
           else
           begin
             // P1 + P2
             ParseNote(0, Param0, (Param1+Rel[0]) * Mult, Param2 * Mult, Param3, ParamLyric);
             ParseNote(1, Param0, (Param1+Rel[1]) * Mult, Param2 * Mult, Param3, ParamLyric);
           end;
          end; //Zeronote check
        end // if

        else if Param0 = '-' then
        begin
          // reads sentence
          Param1 := ParseLyricIntParam(CurLine, LinePos);
          if self.Relative then
            Param2 := ParseLyricIntParam(CurLine, LinePos); // read one more data for relative system

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

        else if Param0 = 'B' then
        begin
          SetLength(self.BPM, Length(self.BPM) + 1);
          self.BPM[High(self.BPM)].StartBeat := ParseLyricFloatParam(CurLine, LinePos);
          self.BPM[High(self.BPM)].StartBeat := self.BPM[High(self.BPM)].StartBeat + Rel[0];

          self.BPM[High(self.BPM)].BPM := ParseLyricFloatParam(CurLine, LinePos);
          self.BPM[High(self.BPM)].BPM := self.BPM[High(self.BPM)].BPM * Mult * MultBPM;
        end;

        // Read next line in File
        if (not SongFile.ReadLine(CurLine)) then
          Break;

        Inc(FileLineNo);
      end; // while
    finally
      SongFile.Free;
    end;
  except
    on E: Exception do
    begin
      Log.LogError(Format('Error loading file: "%s" in line %d,%d: %s',
                  [FileNamePath.ToNative, FileLineNo, LinePos, E.Message]));
      Exit;
    end;
  end;

  for I := 0 to High(Lines) do
  begin
    if ((Both) or (I = 0)) then
    begin
      if (Length(Lines[I].Line) < 2) then
      begin
        LastError := 'ERROR_CORRUPT_SONG_NO_BREAKS';
        Log.LogError('Error loading file: Can''t find any linebreaks in "' + FileNamePath.ToNative + '"');
        exit;
      end;

      if (Lines[I].Line[Lines[I].High].HighNote < 0) then
      begin
        SetLength(Lines[I].Line, Lines[I].Number - 1);
        Lines[I].High := Lines[I].High - 1;
        Lines[I].Number := Lines[I].Number - 1;
        Log.LogError('Error loading Song, sentence w/o note found in last line before E: ' + FileNamePath.ToNative);
      end;
    end;
  end;

  for Count := 0 to High(Lines) do
  begin
    if (High(Lines[Count].Line) >= 0) then
      Lines[Count].Line[High(Lines[Count].Line)].LastLine := true;
  end;

  Result := true;
end;

//Load XML Song
function TSong.LoadXMLSong(): boolean;
var
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
  FileNamePath: IPath;
begin
  Result := false;
  LastError := '';

  FileNamePath := Path.Append(FileName);
  if not FileNamePath.IsFile() then
  begin
    Log.LogError('File not found: "' + FileNamePath.ToNative + '"', 'TSong.LoadSong()');
    exit;
  end;

  MultBPM           := 4; // multiply beat-count of note by 4
  Mult              := 1; // accuracy of measurement of note
  Lines[0].ScoreValue := 0;
  self.Relative     := false;
  Rel[0]            := 0;
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

  if Parser.ParseSong(FileNamePath) then
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
    Log.LogError('Could not parse inputfile: ' + FileNamePath.ToNative);
    exit;
  end;

  for Count := 0 to High(Lines) do
  begin
    Lines[Count].Line[High(Lines[Count].Line)].LastLine := true;
  end;

  Result := true;
end;

function TSong.ReadXMLHeader(const aFileName : IPath): boolean;
var
  Done        : byte;
  Parser      : TParser;
  FileNamePath: IPath;
begin
  Result := true;
  Done   := 0;

  //Parse XML
  Parser := TParser.Create;
  Parser.Settings.DashReplacement := '~';

  FileNamePath := Self.Path.Append(Self.FileName);
  if Parser.ParseSong(FileNamePath) then
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
    Self.Mp3 := FindSongFile(Self.Path, '*.mp3');
    //Add Mp3 Flag to Done
    if (Self.Path.Append(Self.Mp3).IsFile()) then
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
    self.Cover := FindSongFile(Path, '*[CO].jpg');

    //Background Picture
    self.Background := FindSongFile(Path, '*[BG].jpg');

    // Video File
    //    self.Video := Value

    // Video Gap
    //  self.VideoGAP := StrtoFloatI18n( Value )

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
    Log.LogError('File incomplete or not SingStar XML (A): ' + aFileName.ToNative);

  Parser.Free;

  //Check if all Required Values are given
  if (Done <> 15) then
  begin
    Result := false;
    if (Done and 8) = 0 then      //No BPM Flag
      Log.LogError('BPM tag missing: ' + self.FileName.ToNative)
    else if (Done and 4) = 0 then //No MP3 Flag
      Log.LogError('MP3 tag/file missing: ' + self.FileName.ToNative)
    else if (Done and 2) = 0 then //No Artist Flag
      Log.LogError('Artist tag missing: ' + self.FileName.ToNative)
    else if (Done and 1) = 0 then //No Title Flag
      Log.LogError('Title tag missing: ' + self.FileName.ToNative)
    else //unknown Error
      Log.LogError('File incomplete or not SingStar XML (B - '+ inttostr(Done) +'): ' + aFileName.ToNative);
  end;

end;

{**
 * "International" StrToFloat variant. Uses either ',' or '.' as decimal
 * separator.
 *}
function StrToFloatI18n(const Value: string): extended;
var
  TempValue : string;
begin
  TempValue := Value;
  if (Pos(',', TempValue) <> 0) then
    TempValue[Pos(',', TempValue)] := '.';
  Result := StrToFloatDef(TempValue, 0);
end;

function TSong.ReadTXTHeader(SongFile: TTextFileStream; ReadCustomTags: Boolean): boolean;
var
  Line, Identifier: string;
  Value: string;
  SepPos: integer; // separator position
  Done: byte;      // bit-vector of mandatory fields
  EncFile: IPath; // encoded filename
  FullFileName: string;

  { adds a custom header tag to the song
    if there is no ':' in the read line, Tag should be empty
    and the whole line should be in Content }
  procedure AddCustomTag(const Tag, Content: String);
    var Len: Integer;
  begin
    if ReadCustomTags then
    begin
      Len := Length(CustomTags);
      SetLength(CustomTags, Len + 1);
      CustomTags[Len].Tag := DecodeStringUTF8(Tag, Encoding);
      CustomTags[Len].Content := DecodeStringUTF8(Content, Encoding);
    end;
  end;
begin
  Result := true;
  Done   := 0;

  FullFileName := Path.Append(Filename).ToNative;

  //Read first Line
  SongFile.ReadLine(Line);
  if (Length(Line) <= 0) then
  begin
    Log.LogError('File starts with empty line: ' + FullFileName,
                 'TSong.ReadTXTHeader');
    Result := false;
    Exit;
  end;

  // check if file begins with a UTF-8 BOM, if so set encoding to UTF-8
  if (CheckReplaceUTF8BOM(Line)) then
    Encoding := encUTF8;

  //Read Lines while Line starts with # or its empty
  while (Length(Line) = 0) or (Line[1] = '#') do
  begin
    //Increase Line Number
    Inc (FileLineNo);
    SepPos := Pos(':', Line);

    //Line has no Seperator, ignore non header field
    if (SepPos = 0) then
    begin
      AddCustomTag('', Copy(Line, 2, Length(Line) - 1));
      // read next line
      if (not SongFile.ReadLine(Line)) then
      begin
        Result := false;
        Log.LogError('File incomplete or not Ultrastar txt (A): ' + FullFileName);
        Break;
      end;
      Continue;
    end;

    //Read Identifier and Value
    Identifier  := UpperCase(Trim(Copy(Line, 2, SepPos - 2))); //Uppercase is for Case Insensitive Checks
    Value       := Trim(Copy(Line, SepPos + 1, Length(Line) - SepPos));

    //Check the Identifier (If Value is given)
    if (Length(Value) = 0) then
    begin
      Log.LogWarn('Empty field "'+Identifier+'" in file ' + FullFileName,
                   'TSong.ReadTXTHeader');
      AddCustomTag(Identifier, '');
    end
    else
    begin

      //-----------
      //Required Attributes
      //-----------

      if (Identifier = 'TITLE') then
      begin
        DecodeStringUTF8(Value, Title, Encoding);
        //Add Title Flag to Done
        Done := Done or 1;
      end

      else if (Identifier = 'ARTIST') then
      begin
        DecodeStringUTF8(Value, Artist, Encoding);
        //Add Artist Flag to Done
        Done := Done or 2;
      end

      //MP3 File
      else if (Identifier = 'MP3') then
      begin
        EncFile := DecodeFilename(Value);
        if (Self.Path.Append(EncFile).IsFile) then
        begin
          self.Mp3 := EncFile;

          //Add Mp3 Flag to Done
          Done := Done or 4;
        end;
      end

      //Beats per Minute
      else if (Identifier = 'BPM') then
      begin
        SetLength(self.BPM, 1);
        self.BPM[0].StartBeat := 0;

        self.BPM[0].BPM := StrToFloatI18n( Value ) * Mult * MultBPM;

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
      begin
        self.GAP := StrToFloatI18n(Value);
      end

      //Cover Picture
      else if (Identifier = 'COVER') then
      begin
        self.Cover := DecodeFilename(Value);
      end

      //Background Picture
      else if (Identifier = 'BACKGROUND') then
      begin
        self.Background := DecodeFilename(Value);
      end

      // Video File
      else if (Identifier = 'VIDEO') then
      begin
        EncFile := DecodeFilename(Value);
        if (self.Path.Append(EncFile).IsFile) then
          self.Video := EncFile
        else
          Log.LogError('Can''t find video file in song: ' + FullFileName);
      end

      // Video Gap
      else if (Identifier = 'VIDEOGAP') then
      begin
        self.VideoGAP := StrToFloatI18n( Value )
      end

      //Genre Sorting
      else if (Identifier = 'GENRE') then
      begin
        DecodeStringUTF8(Value, Genre, Encoding)
      end

      //Edition Sorting
      else if (Identifier = 'EDITION') then
      begin
        DecodeStringUTF8(Value, Edition, Encoding)
      end

      //Creator Tag
      else if (Identifier = 'CREATOR') then
      begin
        DecodeStringUTF8(Value, Creator, Encoding)
      end

      //Language Sorting
      else if (Identifier = 'LANGUAGE') then
      begin
        DecodeStringUTF8(Value, Language, Encoding)
      end

      //Language Sorting
      else if (Identifier = 'YEAR') then
      begin
        TryStrtoInt(Value, self.Year)
      end

      // Song Start
      else if (Identifier = 'START') then
      begin
        self.Start := StrToFloatI18n( Value )
      end

      // Song Ending
      else if (Identifier = 'END') then
      begin
        TryStrtoInt(Value, self.Finish)
      end

      // Resolution
      else if (Identifier = 'RESOLUTION') then
      begin
        TryStrtoInt(Value, self.Resolution)
      end

      // Notes Gap
      else if (Identifier = 'NOTESGAP') then
      begin
        TryStrtoInt(Value, self.NotesGAP)
      end

      // Relative Notes
      else if (Identifier = 'RELATIVE') then
      begin
        if (UpperCase(Value) = 'YES') then
          self.Relative := true;
      end

      // File encoding
      else if (Identifier = 'ENCODING') then
      begin
        self.Encoding := ParseEncoding(Value, DEFAULT_ENCODING);
      end

      // unsupported tag
      else
      begin
        AddCustomTag(Identifier, Value);
      end;

    end; // End check for non-empty Value

    // read next line
    if (not SongFile.ReadLine(Line)) then
    begin
      Result := false;
      Log.LogError('File incomplete or not Ultrastar txt (A): ' + FullFileName);
      Break;
    end;
  end; // while

  if self.Cover.IsUnset then
    self.Cover := FindSongFile(Path, '*[CO].jpg');

  //Check if all Required Values are given
  if (Done <> 15) then
  begin
    Result := false;
    if (Done and 8) = 0 then      //No BPM Flag
      Log.LogError('BPM tag missing: ' + FullFileName)
    else if (Done and 4) = 0 then //No MP3 Flag
      Log.LogError('MP3 tag/file missing: ' + FullFileName)
    else if (Done and 2) = 0 then //No Artist Flag
      Log.LogError('Artist tag missing: ' + FullFileName)
    else if (Done and 1) = 0 then //No Title Flag
      Log.LogError('Title tag missing: ' + FullFileName)
    else //unknown Error
      Log.LogError('File incomplete or not Ultrastar txt (B - '+ inttostr(Done) +'): ' + FullFileName);
  end;
end;

function  TSong.GetErrorLineNo: integer;
begin
  if (LastError='ERROR_CORRUPT_SONG_ERROR_IN_LINE') then
    Result := FileLineNo
  else
    Result := -1;
end;

function TSong.Solmizate(Note: integer; Type_: integer): string;
begin
  case (Type_) of
    1:  // european
      begin
        case (Note mod 12) of
          0..1:   Result := ' do ';
          2..3:   Result := ' re ';
          4:      Result := ' mi ';
          5..6:   Result := ' fa ';
          7..8:   Result := ' sol ';
          9..10:  Result := ' la ';
          11:     Result := ' si ';
        end;
      end;
    2:  // japanese
      begin
        case (Note mod 12) of
          0..1:   Result := ' do ';
          2..3:   Result := ' re ';
          4:      Result := ' mi ';
          5..6:   Result := ' fa ';
          7..8:   Result := ' so ';
          9..10:  Result := ' la ';
          11:     Result := ' shi ';
        end;
      end;
    3:  // american
      begin
        case (Note mod 12) of
          0..1:   Result := ' do ';
          2..3:   Result := ' re ';
          4:      Result := ' mi ';
          5..6:   Result := ' fa ';
          7..8:   Result := ' sol ';
          9..10:  Result := ' la ';
          11:     Result := ' ti ';
        end;
      end;
  end; // case
end;

procedure TSong.ParseNote(LineNumber: integer; TypeP: char; StartP, DurationP, NoteP: integer; LyricS: UTF8String);
begin
  if (Ini.Solmization <> 0) then
    LyricS := Solmizate(NoteP, Ini.Solmization);

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

    Note[HighNote].Color := 1; // default color to 1 for editor

    DecodeStringUTF8(LyricS, Note[HighNote].Text, Encoding);
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
    Log.LogError('Error loading Song, sentence w/o note found in line ' +
                 InttoStr(FileLineNo) + ': ' + Filename.ToNative);
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

procedure TSong.Clear();
begin
  //Main Information
  Title  := '';
  Artist := '';

  //Sortings:
  Genre    := 'Unknown';
  Edition  := 'Unknown';
  Language := 'Unknown';
  Year := 0;

  // set to default encoding
  Encoding := DEFAULT_ENCODING;

  // clear custom header tags
  SetLength(CustomTags, 0);

  //Required Information
  Mp3    := PATH_NONE;
  SetLength(BPM, 0);

  GAP    := 0;
  Start  := 0;
  Finish := 0;

  //Additional Information
  Background := PATH_NONE;
  Cover      := PATH_NONE;
  Video      := PATH_NONE;
  VideoGAP   := 0;
  NotesGAP   := 0;
  Resolution := 4;
  Creator    := '';

  Relative := false;
end;

function TSong.Analyse(const ReadCustomTags: Boolean): boolean;
var
  SongFile: TTextFileStream;
begin
  Result := false;

  //Reset LineNo
  FileLineNo := 0;

  //Open File and set File Pointer to the beginning
  SongFile := TMemTextFileStream.Create(Self.Path.Append(Self.FileName), fmOpenRead);
  try
    //Clear old Song Header
    Self.clear;

    //Read Header
    Result := Self.ReadTxTHeader(SongFile, ReadCustomTags)
  finally
    SongFile.Free;
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
