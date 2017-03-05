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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/base/USong.pas $
 * $Id: USong.pas 3135 2015-09-12 00:48:54Z basisbit $
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
    UMD5,
  {$ELSE}
    {$IFNDEF DARWIN}
      syscall,
    {$ENDIF}
    baseunix,
    UnixType,
  {$ENDIF}
  SysUtils,
  Classes,
  {$IFDEF DARWIN}
    cthreads,
  {$ENDIF}
  {$IFDEF USE_PSEUDO_THREAD}
    PseudoThread,
  {$ENDIF}
  UCatCovers,
  UCommon,
  UFilesystem,
  ULog,
  UPath,
  UPlatform,
  UTexture,
  UTextEncoding,
  UUnicodeUtils,
  UXMLSong;

type

  TSingMode = ( smNormal, smPartyClassic, smPartyFree, smPartyChallenge, smPartyTournament, smJukebox, smPlaylistRandom , smMedley );
  TSongMode = ( smAll, smCategory, smPlaylist);

  TMedleySource = ( msNone, msCalculated, msTag );

  TMedley = record
    Source:       TMedleySource;  //source of the information
    StartBeat:    integer;        //start beat of medley
    EndBeat:      integer;        //end beat of medley
    FadeIn_time:  real;           //FadeIn-Time in seconds
    FadeOut_time: real;           //FadeOut-Time in seconds
  end;

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
    procedure ParseNote(LineNumber: integer; TypeP: char; StartP, DurationP, NoteP: integer; LyricS: UTF8String);
    procedure NewSentence(LineNumberP: integer; Param1, Param2: integer);
    procedure FindRefrain(); // tries to find a refrain for the medley mode and preview start

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
    MD5:          string; //MD5 Hash of Current Song

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

    // use in search
    TitleNoAccent:  UTF8String;
    ArtistNoAccent: UTF8String;

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
    PreviewStart: real;   // in seconds
    CalcMedley: boolean;  // if true => do not calc medley for that song
    Medley:     TMedley;  // medley params

    isDuet: boolean;
    DuetNames:  array of UTF8String; // duet singers name

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
    function    LoadSong(DuetChange: boolean): boolean;
    function    LoadXMLSong: boolean;
    function    Analyse(const ReadCustomTags: Boolean = false; DuetChange: boolean = false): boolean;
    function    AnalyseXML(): boolean;
    procedure   SetMedleyMode();
    procedure   Clear();
    function    MD5SongFile(SongFileR: TTextFileStream): string;
  end;

  TSongOptions = class
    public
      VideoRatioAspect:        integer;
      VideoWidth :             integer;
      VideoHeight:             integer;
      LyricPosition:           integer;
      LyricAlpha:              integer;
      LyricSingFillColor:      string;
      LyricActualFillColor:    string;
      LyricNextFillColor:      string;
      LyricSingOutlineColor:   string;
      LyricActualOutlineColor: string;
      LyricNextOutlineColor:   string;

      constructor Create(RatioAspect, Width, Height, Position, Alpha: integer;
                SingFillColor, ActualFillColor, NextFillColor, SingOutlineColor, ActualOutlineColor, NextOutlineColor: string);
  end;

implementation

uses
  StrUtils,
  TextGL,
  UIni,
  UPathUtils,
  {$IFDEF Delphi}
  UMD5,
  {$ENDIF}
  USongs,
  UMusic,  //needed for Lines
  UNote;   //needed for Player

const
  DEFAULT_FADE_IN_TIME = 8;   // for medley fade-in
  DEFAULT_FADE_OUT_TIME = 2;  // for medley fade-out

constructor TSongOptions.Create(RatioAspect, Width, Height, Position, Alpha: integer;
                SingFillColor, ActualFillColor, NextFillColor, SingOutlineColor, ActualOutlineColor, NextOutlineColor: string);
begin
  inherited Create();

  VideoRatioAspect := RatioAspect;
  VideoWidth := Width;
  VideoHeight := Height;
  LyricPosition := Position;
  LyricAlpha := Alpha;
  LyricSingFillColor := SingFillColor;
  LyricActualFillColor := ActualFillColor;
  LyricNextFillColor := NextFillColor;
  LyricSingOutlineColor := SingOutlineColor;
  LyricActualOutlineColor := ActualOutlineColor;
  LyricNextOutlineColor := NextOutlineColor;

end;

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

  if (Length(Str) < 1) then
  begin
    LinePos := OldLinePos;
    raise EUSDXParseException.Create('Character expected');
  end
  else if (Length(Str) > 1) then
  begin
    Log.LogWarn(Format('"%s" in line %d: %s',
        [FileName.ToNative, FileLineNo, 'character expected but found "' + Str + '"']),
        'TSong.ParseLyricCharParam');
  end;

  LinePos := OldLinePos + 1;
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
function TSong.LoadSong(DuetChange: boolean): boolean;
var
  CurLine: RawByteString;
  LinePos:  integer;
  Count:    integer;
  Both:     boolean;

  CP:       integer; // Current Player (0 or 1)

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
  CP:=0;

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
  Rel[1]            := 0;
  Both              := false;

  if Length(Player) = 2 then
    Both := true;

  try
    // Open song file for reading.....
    SongFile := TMemTextFileStream.Create(FileNamePath, fmOpenRead);
    MD5 := MD5SongFile(SongFile);
    SongFile.Position := 0;

    try
      //Search for Note Beginning
      FileLineNo := 0;
      NotesFound := false;
      while (SongFile.ReadLine(CurLine)) do
      begin
        Inc(FileLineNo);
        if (Length(CurLine) > 0) and (CurLine[1] in [':', 'F', '*', 'P']) then
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

      SetLength(Lines, 0);
      if (CurLine[1] = 'P') then
      begin
        CurrentSong.isDuet := true;
        SetLength(Lines, 2);
        CP := -1;
      end
      else
        SetLength(Lines, 1);

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

        if (Param0 = 'P') then
        begin

          if (CurLine[2] = ' ') then
            Param1 := StrToInt(CurLine[3])
          else
            Param1 := StrToInt(CurLine[2]);

          if (Param1 = 1) then
          begin
            if not(DuetChange) then
              CP := 0
            else
              CP := 1
          end
          else
          begin
            if (Param1 = 2) then
            begin
              if not(DuetChange) then
                CP := 1
              else
                CP := 0;
            end
            else
              if (Param1 = 3) then
                CP := 2
            else
            begin
              Log.LogError('Wrong P-Number in file: "' + FileName.ToNative + '"; Line '+IntToStr(FileLineNo)+' (LoadSong)');
              Result := False;
              Exit;
            end;
          end;
        end;

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
          begin
            Log.LogWarn(Format('"%s" in line %d: %s',
              [FileNamePath.ToNative, FileLineNo,
              'found note with length zero -> converted to FreeStyle']),
              'TSong.LoadSong');
            //Log.LogError('Found zero-length note at "'+Param0+' '+IntToStr(Param1)+' '+IntToStr(Param2)+' '+IntToStr(Param3)+ParamLyric+'" -> Note ignored!')
            Param0 := 'F';
          end;

          // add notes
          if (CP <> 2) then
          begin
            // P1
            if (Lines[CP].High < 0) or (Lines[CP].High > 5000) then
            begin
              Log.LogError('Found faulty song. Did you forget a P1 or P2 tag? "'+Param0+' '+IntToStr(Param1)+
              ' '+IntToStr(Param2)+' '+IntToStr(Param3)+ParamLyric+'" -> '+
              FileNamePath.ToNative+' Line:'+IntToStr(FileLineNo));
              Break;
            end;
            ParseNote(CP, Param0, (Param1+Rel[CP]) * Mult, Param2 * Mult, Param3, ParamLyric);
          end
          else
          begin
            // P1 + P2
            ParseNote(0, Param0, (Param1+Rel[0]) * Mult, Param2 * Mult, Param3, ParamLyric);
            ParseNote(1, Param0, (Param1+Rel[1]) * Mult, Param2 * Mult, Param3, ParamLyric);
          end;
        end // if

        else
        if Param0 = '-' then
        begin
          // reads sentence
          Param1 := ParseLyricIntParam(CurLine, LinePos);
          if self.Relative then
            Param2 := ParseLyricIntParam(CurLine, LinePos); // read one more data for relative system

          // new sentence
          if not CurrentSong.isDuet then
            // one singer
            NewSentence(CP, (Param1 + Rel[CP]) * Mult, Param2)
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
        // HACK DUET ERROR
        if not (CurrentSong.isDuet) then
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
  Lines[1].ScoreValue := 0;
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
    self.TitleNoAccent := LowerCase(GetStringWithNoAccents(UTF8Decode(Parser.SongInfo.Header.Title)));

    //Add Title Flag to Done
    Done := Done or 1;

    //Artist
    self.Artist := Parser.SongInfo.Header.Artist;
    self.ArtistNoAccent := LowerCase(GetStringWithNoAccents(UTF8Decode(Parser.SongInfo.Header.Artist)));

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
    //self.Year := Parser.SongInfo.Header.Year

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
  MedleyFlags: byte; //bit-vector for medley/preview tags
  EncFile: IPath; // encoded filename
  FullFileName: string;
  I, P: integer;

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
  MedleyFlags := 0;

  //SetLength(tmpEdition, 0);

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
      Log.LogInfo('Empty field "'+Identifier+'" in file ' + FullFileName,
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
        self.TitleNoAccent := LowerCase(GetStringWithNoAccents(UTF8Decode(Title)));

        //Add Title Flag to Done
        Done := Done or 1;
      end

      else if (Identifier = 'ARTIST') then
      begin
        DecodeStringUTF8(Value, Artist, Encoding);
        self.ArtistNoAccent := LowerCase(GetStringWithNoAccents(UTF8Decode(Artist)));

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
        end
        else
        begin
          Log.LogError('Can''t find audio file in song: ' + DecodeStringUTF8(FullFileName, Encoding));
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

      //Year Sorting
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
        self.Encoding := ParseEncoding(Value, Ini.DefaultEncoding);
      end

      // PreviewStart
      else if (Identifier = 'PREVIEWSTART') then
      begin
        self.PreviewStart := StrToFloatI18n( Value );
        if (self.PreviewStart>0) then
          MedleyFlags := MedleyFlags or 1;
      end

      // MedleyStartBeat
      else if (Identifier = 'MEDLEYSTARTBEAT') and not self.Relative then
      begin
        if TryStrtoInt(Value, self.Medley.StartBeat) then
          MedleyFlags := MedleyFlags or 2;
      end

      // MedleyEndBeat
      else if (Identifier = 'MEDLEYENDBEAT') and not self.Relative then
      begin
        if TryStrtoInt(Value, self.Medley.EndBeat) then
          MedleyFlags := MedleyFlags or 4;
      end

      // Medley
      else if (Identifier = 'CALCMEDLEY') then
      begin
        if Uppercase(Value) = 'OFF' then
          self.CalcMedley := false;
      end

      // Duet Singer Name P1
      else if (Identifier = 'DUETSINGERP1') then
      begin
        DecodeStringUTF8(Value, DuetNames[0], Encoding);
      end

      // Duet Singer Name P2
      else if (Identifier = 'DUETSINGERP2') then
      begin
        DecodeStringUTF8(Value, DuetNames[1], Encoding);
      end

      // unsupported tag
      else
      begin
        AddCustomTag(Identifier, Value);
      end;
    end; // End check for non-empty Value

    // read next line
    if not SongFile.ReadLine(Line) then
    begin
      Result := false;
      Log.LogError('File incomplete or not Ultrastar txt (A): ' + FullFileName);
      Break;
    end;
  end; // while

  //MD5 of File
  self.MD5 := MD5SongFile(SongFile);

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
  end
  else
  begin //check medley tags
    if (MedleyFlags and 6) = 6 then //MedleyStartBeat and MedleyEndBeat are both set
    begin
      if self.Medley.StartBeat >= self.Medley.EndBeat then
        MedleyFlags := MedleyFlags - 6;
    end;

    if ((MedleyFlags and 1) = 0) or (self.PreviewStart <= 0) then //PreviewStart is not set or <=0
    begin
      if (MedleyFlags and 2) = 2 then
        self.PreviewStart := GetTimeFromBeat(self.Medley.StartBeat, self)  //fallback to MedleyStart
      else
        self.PreviewStart := 0; //else set it to 0, it will be set in FindRefrainStart
    end;

    if (MedleyFlags and 6) = 6 then
    begin
      self.Medley.Source := msTag;

      //calculate fade time
      self.Medley.FadeIn_time := DEFAULT_FADE_IN_TIME;

      self.Medley.FadeOut_time := DEFAULT_FADE_OUT_TIME;
    end else
      self.Medley.Source := msNone;
  end;

end;

function  TSong.GetErrorLineNo: integer;
begin
  if (LastError='ERROR_CORRUPT_SONG_ERROR_IN_LINE') then
    Result := FileLineNo
  else
    Result := -1;
end;

procedure TSong.ParseNote(LineNumber: integer; TypeP: char; StartP, DurationP, NoteP: integer; LyricS: UTF8String);
begin

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
begin

  if (Lines[LineNumberP].Line[Lines[LineNumberP].High].HighNote  <> -1) then
  begin //create a new line
    SetLength(Lines[LineNumberP].Line, Lines[LineNumberP].Number + 1);
    Inc(Lines[LineNumberP].High);
    Inc(Lines[LineNumberP].Number);
  end
  else
  begin //use old line if it there were no notes added since last call of NewSentence
    // HACK DUET ERROR
    if not (CurrentSong.isDuet) then
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

{* new procedure for preview
   tries find out the beginning of a refrain
   and the end... *}
procedure TSong.FindRefrain();
Const
  MEDLEY_MIN_DURATION = 40;   //minimum duration of a medley-song in seconds

Type
  TSeries = record
    start:    integer; //Start sentence of series
    end_:     integer; //End sentence of series
    len:      integer; //Length of sentence series
  end;

var
  I, J, K, num_lines:   integer;
  sentences:            array of UTF8String;
  series:               array of TSeries;
  temp_series:          TSeries;
  max:                  integer;
  len_lines, len_notes: integer;
  found_end:            boolean;
begin
  if self.Medley.Source = msTag then
    Exit;

  if not self.CalcMedley then
    Exit;

  //relative is not supported for medley!
  if self.Relative then
  begin
    Log.LogError('Song '+self.Artist+'-' + self.Title + ' contains #Relative, this is not supported by medley-function!');
    self.Medley.Source := msNone;
    Exit;
  end;

  num_lines := Length(Lines[0].Line);
  SetLength(sentences, num_lines);

  //build sentences array
  for I := 0 to num_lines - 1 do
  begin
    sentences[I] := '';
    for J := 0 to Length(Lines[0].Line[I].Note) - 1 do
    begin
      if (Lines[0].Line[I].Note[J].NoteType <> ntFreestyle) then
        sentences[I] := sentences[I] + Lines[0].Line[I].Note[J].Text;
    end;
  end;

  //find equal sentences series
  SetLength(series, 0);

  for I := 0 to num_lines - 2 do
  begin
    for J := I + 1 to num_lines - 1 do
    begin
      if sentences[I] = sentences[J] then
      begin
        temp_series.start := I;
        temp_series.end_  := I;

        if (J + J - I - 1 > num_lines - 1) then
          max:=num_lines-1-J
        else
          max:=J-I-1;

        for K := 1 to max do
        begin
          if sentences[I+K] = sentences[J+K] then
            temp_series.end_ := I+K
          else
            break;
        end;
        temp_series.len := temp_series.end_ - temp_series.start + 1;
        SetLength(series, Length(series)+1);
        series[Length(series)-1] := temp_series;
      end;
    end;
  end;

  //search for longest sequence
  max := 0;
  if Length(series) > 0 then
  begin
    for I := 0 to Length(series) - 1 do
    begin
      if series[I].len > series[max].len then
        max := I;
    end;
  end;

  len_lines := length(Lines[0].Line);

  if (Length(series) > 0) and (series[max].len > 3) then
  begin
    self.Medley.StartBeat := Lines[0].Line[series[max].start].Note[0].Start;
    len_notes := length(Lines[0].Line[series[max].end_].Note);
    self.Medley.EndBeat := Lines[0].Line[series[max].end_].Note[len_notes - 1].Start +
      Lines[0].Line[series[max].end_].Note[len_notes - 1].Length;

    found_end := false;

    //set end if duration > MEDLEY_MIN_DURATION
    if GetTimeFromBeat(self.Medley.StartBeat) + MEDLEY_MIN_DURATION >
      GetTimeFromBeat(self.Medley.EndBeat) then
    begin
      found_end := true;
    end;

    //estimate the end: just go MEDLEY_MIN_DURATION
    //ahead an set to a line end (if possible)
    if not found_end then
    begin
      for I := series[max].start + 1 to len_lines - 1 do
      begin
        len_notes := length(Lines[0].Line[I].Note);
        for J := 0 to len_notes - 1 do
        begin
          if GetTimeFromBeat(self.Medley.StartBeat) + MEDLEY_MIN_DURATION >
            GetTimeFromBeat(Lines[0].Line[I].Note[J].Start +
            Lines[0].Line[I].Note[J].Length) then
          begin
            found_end := true;
            self.Medley.EndBeat := Lines[0].Line[I].Note[len_notes-1].Start +
              Lines[0].Line[I].Note[len_notes - 1].Length;
            break;
          end;
        end;
      end;
    end;

    if found_end then
    begin
      self.Medley.Source := msCalculated;

      //calculate fade time
      self.Medley.FadeIn_time := DEFAULT_FADE_IN_TIME;
      self.Medley.FadeOut_time := DEFAULT_FADE_OUT_TIME;
    end;
  end;

  //set PreviewStart if not set
  if self.PreviewStart = 0 then
  begin
    if self.Medley.Source = msCalculated then
      self.PreviewStart := GetTimeFromBeat(self.Medley.StartBeat);
  end;
end;

//sets a song to medley-mode:
//converts all unneeded notes into freestyle
//updates score values
procedure TSong.SetMedleyMode();
var
  pl, line, note: integer;
  cut_line: array of integer;
  foundcut: array of boolean;
  start:          integer;
  end_:           integer;

begin
  start := self.Medley.StartBeat;
  end_  := self.Medley.EndBeat;
  SetLength(cut_line, Length(Lines));
  SetLength(foundcut, Length(Lines));

  for pl := 0 to Length(Lines) - 1 do
  begin
    foundcut[pl] := false;
    cut_line[pl] := high(Integer);
    Lines[pl].ScoreValue := 0;
    for line := 0 to Length(Lines[pl].Line) - 1 do
    begin
      Lines[pl].Line[line].TotalNotes := 0;
      for note := 0 to Length(Lines[pl].Line[line].Note) - 1 do
      begin
        if Lines[pl].Line[line].Note[note].Start < start then      //check start
        begin
          Lines[pl].Line[line].Note[note].NoteType := ntFreeStyle;
        end else if Lines[pl].Line[line].Note[note].Start>= end_ then  //check end
        begin
          Lines[pl].Line[line].Note[note].NoteType := ntFreeStyle;
          if not foundcut[pl] then
          begin
            if (note=0) then
              cut_line[pl] := line
            else
              cut_line[pl] := line + 1;
          end;
          foundcut[pl] := true;
        end
	else
        begin
          //add this notes value ("notes length" * "notes scorefactor") to the current songs entire value
          Inc(Lines[pl].ScoreValue, Lines[pl].Line[line].Note[note].Length * ScoreFactor[Lines[pl].Line[line].Note[note].NoteType]);
          //and to the current lines entire value
          Inc(Lines[pl].Line[line].TotalNotes, Lines[pl].Line[line].Note[note].Length * ScoreFactor[Lines[pl].Line[line].Note[note].NoteType]);
        end;
      end;
    end;
  end;

  for pl := 0 to Length(Lines) - 1 do
  begin
    if (foundcut[pl]) and (Length(Lines[pl].Line) > cut_line[pl]) then
    begin
      SetLength(Lines[pl].Line, cut_line[pl]);
      Lines[pl].High := cut_line[pl]-1;
      Lines[pl].Number := Lines[pl].High+1;
    end;
  end;
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
  Encoding := Ini.DefaultEncoding;

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
  PreviewStart := 0;
  CalcMedley := true;
  Medley.Source := msNone;

  isDuet := false;

  SetLength(DuetNames, 2);
  DuetNames[0] := 'P1';
  DuetNames[1] := 'P2';

  Relative := false;
end;

function TSong.Analyse(const ReadCustomTags: Boolean; DuetChange: boolean): boolean;
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
    Result := Self.ReadTxTHeader(SongFile, ReadCustomTags);

    //Load Song for Medley Tags
    CurrentSong := self;
    Result := Result and LoadSong(DuetChange);

    if Result then
    begin
      //Medley and Duet - is it possible? Perhaps later...
      if not Self.isDuet then
        Self.FindRefrain()
      else
        Self.Medley.Source := msNone;
    end;

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

function TSong.MD5SongFile(SongFileR: TTextFileStream): string;
var
  TextFile: string;
  Line: string;
  FileLineNo2: integer;
begin

  TextFile := '';
  SongFileR.Position := 0;

  //Search for Note Beginning
  FileLineNo2 := 0;
  while (SongFileR.ReadLine(Line)) do
  begin
    Inc(FileLineNo2);

    if (Length(Line) > 0) and (Line[1] <> '#') then
    begin
      TextFile := TextFile + Line;
    end;
  end;
  {$IFDEF MSWINDOWS}
    Result := MD5Print(MD5String(TextFile)); //basisbit TODO
  {$ELSE}
    Result := 'unknown';
  {$ENDIF}
end;

end.

