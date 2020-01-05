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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/base/ULog.pas $
 * $Id: ULog.pas 3117 2015-08-15 01:23:56Z basisbit $
 *}

unit ULog;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
  UPath;

(*
 * LOG_LEVEL_[TYPE] defines the "minimum" index for logs of type TYPE. Each
 * level greater than this BUT less or equal than LOG_LEVEL_[TYPE]_MAX is of this type.  
 * This means a level "LOG_LEVEL_ERROR >= Level <= LOG_LEVEL_ERROR_MAX" e.g.
 * "Level := LOG_LEVEL_ERROR+2" is considered an error level.
 * This is nice for debugging if you have more or less important debug messages.
 * For example you can assign LOG_LEVEL_DEBUG+10 for the more important ones and
 * LOG_LEVEL_DEBUG+20 for less important ones and so on. By changing the log-level
 * you can hide the less important ones.  
 *)
const
  LOG_LEVEL_DEBUG_MAX    = MaxInt;
  LOG_LEVEL_DEBUG        = 50;
  LOG_LEVEL_INFO_MAX     = LOG_LEVEL_DEBUG-1;
  LOG_LEVEL_INFO         = 40;
  LOG_LEVEL_STATUS_MAX   = LOG_LEVEL_INFO-1;
  LOG_LEVEL_STATUS       = 30;
  LOG_LEVEL_WARN_MAX     = LOG_LEVEL_STATUS-1;
  LOG_LEVEL_WARN         = 20;
  LOG_LEVEL_ERROR_MAX    = LOG_LEVEL_WARN-1;
  LOG_LEVEL_ERROR        = 10;
  LOG_LEVEL_CRITICAL_MAX = LOG_LEVEL_ERROR-1;
  LOG_LEVEL_CRITICAL     =  0;
  LOG_LEVEL_NONE         = -1;

  // define level that Log(File)Level is initialized with
  LOG_LEVEL_DEFAULT      = LOG_LEVEL_WARN;
  LOG_FILE_LEVEL_DEFAULT = LOG_LEVEL_ERROR;

  CONSOLE_SCROLLBACK_SIZE = 512;

type
  TLog = class
  private
    LogFile:             TextFile;
    LogFileOpened:       boolean;
    BenchmarkFile:       TextFile;
    BenchmarkFileOpened: boolean;
    ConsoleBuffer: TStringList; // stores logged messages for in-game console, capped to CONSOLE_SCROLLBACK_SIZE
    Lock:                TRTLCriticalSection;

    LogLevel: integer;
    // level of messages written to the log-file
    LogFileLevel: integer;

    procedure LogToFile(const Text: string);

    function GetConsoleCount: integer;

  public
    BenchmarkTimeStart:   array[0..31] of real;
    BenchmarkTimeLength:  array[0..31] of real;//TDateTime;

    Title: String; //Application Title

    // Write log message to log-file
    FileOutputEnabled: Boolean;

    constructor Create;

    // destuctor
    destructor Destroy; override;

    // benchmark
    procedure BenchmarkStart(Number: integer);
    procedure BenchmarkEnd(Number: integer);
    procedure LogBenchmark(const Text: string; Number: integer);

    procedure SetLogLevel(Level: integer);
    function GetLogLevel(): integer;
    procedure SetLogFileLevel(Level: integer);
    function GetLogFileLevel(): integer;

    procedure LogMsg(const Text: string; Level: integer); overload;
    procedure LogMsg(const Msg, Context: string; Level: integer); overload; {$IFDEF HasInline}inline;{$ENDIF}
    procedure LogDebug(const Msg, Context: string); {$IFDEF HasInline}inline;{$ENDIF}
    procedure LogInfo(const Msg, Context: string); {$IFDEF HasInline}inline;{$ENDIF}
    procedure LogStatus(const Msg, Context: string); {$IFDEF HasInline}inline;{$ENDIF}
    procedure LogWarn(const Msg, Context: string); {$IFDEF HasInline}inline;{$ENDIF}
    procedure LogError(const Text: string); overload; {$IFDEF HasInline}inline;{$ENDIF}
    procedure LogError(const Msg, Context: string); overload; {$IFDEF HasInline}inline;{$ENDIF}
    //Critical Error (Halt + MessageBox)
    procedure LogCritical(const Msg, Context: string); {$IFDEF HasInline}inline;{$ENDIF}
    procedure CriticalError(const Text: string); {$IFDEF HasInline}inline;{$ENDIF}

    // voice
    procedure LogVoice(SoundNr: integer);
    // buffer
    procedure LogBuffer(const buf : Pointer; const bufLength : Integer; const filename : IPath);

    // console
    property ConsoleCount: integer read GetConsoleCount;
    function GetConsole(const index: integer; FromTheBeginning: boolean = false): string;
    procedure LogConsole(const Text: string);
    procedure ClearConsoleLog;

  end;

procedure DebugWriteln(const aString: String);

var
  Log:    TLog;

implementation

uses
  SysUtils,
  DateUtils,
  URecord,
  UMain,
  UMusic,  
  UTime,
  UCommon,
  UCommandLine,
  UPathUtils;

(*
 * Write to console if in debug mode (Thread-safe).
 * If debug-mode is disabled nothing is done. 
 *)
procedure DebugWriteln(const aString: string);
begin
  {$IFNDEF DEBUG}
  if Params.Debug then
  begin
  {$ENDIF}
    ConsoleWriteLn(aString);
  {$IFNDEF DEBUG}
  end;
  {$ENDIF}
end;


constructor TLog.Create;
begin
  ConsoleBuffer := TStringList.Create;
  inherited;
  LogLevel := LOG_LEVEL_DEFAULT;
  LogFileLevel := LOG_FILE_LEVEL_DEFAULT;
  FileOutputEnabled := false;
  InitCriticalSection(Lock);
end;

destructor TLog.Destroy;
begin
  DoneCriticalSection(Lock);
  if BenchmarkFileOpened then
    CloseFile(BenchmarkFile);
  //if AnalyzeFileOpened then
  //  CloseFile(AnalyzeFile);
  if LogFileOpened then
    CloseFile(LogFile);

  ConsoleBuffer.Free;
  inherited;
end;

procedure TLog.BenchmarkStart(Number: integer);
begin
  BenchmarkTimeStart[Number] := USTime.GetTime; //Time;
end;

procedure TLog.BenchmarkEnd(Number: integer);
begin
  BenchmarkTimeLength[Number] := USTime.GetTime {Time} - BenchmarkTimeStart[Number];
end;

procedure TLog.LogBenchmark(const Text: string; Number: integer);
var
  Minutes:      integer;
  Seconds:      integer;
  Miliseconds:  integer;

  MinutesS:     string;
  SecondsS:     string;
  MilisecondsS: string;

  ValueText:    string;
begin
  EnterCriticalSection(Lock);
  if (FileOutputEnabled and Params.Benchmark) then
  begin
    if not BenchmarkFileOpened then
    begin
      BenchmarkFileOpened := true;
      AssignFile(BenchmarkFile, LogPath.Append('Benchmark.log').ToNative);
      {$I-}
      Rewrite(BenchmarkFile);
      if IOResult = 0 then
        BenchmarkFileOpened := true;
      {$I+}

      //If File is opened write Date to Benchmark File
      If (BenchmarkFileOpened) then
      begin
        WriteLn(BenchmarkFile, Title + ' Benchmark File');
        WriteLn(BenchmarkFile, 'Date: ' + DatetoStr(Now) + ' Time: ' + TimetoStr(Now));
        WriteLn(BenchmarkFile, '-------------------');

        Flush(BenchmarkFile);
      end;
    end;

    if BenchmarkFileOpened then
    begin
      Miliseconds := Trunc(Frac(BenchmarkTimeLength[Number]) * 1000);
      Seconds := Trunc(BenchmarkTimeLength[Number]) mod 60;
      Minutes := Trunc((BenchmarkTimeLength[Number] - Seconds) / 60);
      //ValueText := FloatToStr(BenchmarkTimeLength[Number]);

      {
      ValueText := FloatToStr(SecondOf(BenchmarkTimeLength[Number]) +
                              MilliSecondOf(BenchmarkTimeLength[Number])/1000);
      if MinuteOf(BenchmarkTimeLength[Number]) >= 1 then
        ValueText := IntToStr(MinuteOf(BenchmarkTimeLength[Number])) + ':' + ValueText;
      WriteLn(FileBenchmark, Text + ': ' + ValueText + ' seconds');
      }

      if (Minutes = 0) and (Seconds = 0) then begin
        MilisecondsS := IntToStr(Miliseconds);
        ValueText := MilisecondsS + ' miliseconds';
      end;

      if (Minutes = 0) and (Seconds >= 1) then begin
        MilisecondsS := IntToStr(Miliseconds);
        while Length(MilisecondsS) < 3 do
          MilisecondsS := '0' + MilisecondsS;

        SecondsS := IntToStr(Seconds);

        ValueText := SecondsS + ',' + MilisecondsS + ' seconds';
      end;

      if Minutes >= 1 then begin
        MilisecondsS := IntToStr(Miliseconds);
        while Length(MilisecondsS) < 3 do
          MilisecondsS := '0' + MilisecondsS;

        SecondsS := IntToStr(Seconds);
        while Length(SecondsS) < 2 do
          SecondsS := '0' + SecondsS;

        MinutesS := IntToStr(Minutes);

        ValueText := MinutesS + ':' + SecondsS + ',' + MilisecondsS + ' minutes';
      end;

      WriteLn(BenchmarkFile, Text + ': ' + ValueText);
      Flush(BenchmarkFile);
    end;
  end;
  LeaveCriticalSection(Lock);
end;

procedure TLog.LogToFile(const Text: string);
begin
  EnterCriticalSection(Lock);
  if (FileOutputEnabled and not LogFileOpened) then
  begin
    AssignFile(LogFile, LogPath.Append('Error.log').ToNative);
    {$I-}
    Rewrite(LogFile);
    if IOResult = 0 then
      LogFileOpened := true;
    {$I+}

    //If File is opened write Date to Error File
    if (LogFileOpened) then
    begin
      WriteLn(LogFile, Title + ' Error Log');
      WriteLn(LogFile, 'Date: ' + DatetoStr(Now) + ' Time: ' + TimetoStr(Now));
      WriteLn(LogFile, '-------------------');

      Flush(LogFile);
    end;
  end;

  if LogFileOpened then
  begin
    try
      WriteLn(LogFile, Text);
      Flush(LogFile);
    except
      LogFileOpened := false;
    end;
  end;
  LeaveCriticalSection(Lock);
end;

procedure TLog.SetLogLevel(Level: integer);
begin
  LogLevel := Level;
end;

function TLog.GetLogLevel(): integer;
begin
  Result := LogLevel;
end;

procedure TLog.SetLogFileLevel(Level: integer);
begin
  LogFileLevel := Level;
end;

function TLog.GetLogFileLevel(): integer;
begin
  Result := LogFileLevel;
end;

procedure TLog.LogMsg(const Text: string; Level: integer);
var
  LogMsg: string;
begin
  if ((Level <= LogLevel) or (Level <= LogFileLevel)) then
  begin
    if (Level <= LOG_LEVEL_CRITICAL_MAX) then
      LogMsg := 'CRITICAL: ' + Text
    else if (Level <= LOG_LEVEL_ERROR_MAX) then
      LogMsg := 'ERROR:  ' + Text
    else if (Level <= LOG_LEVEL_WARN_MAX) then
      LogMsg := 'WARN:   ' + Text
    else if (Level <= LOG_LEVEL_STATUS_MAX) then
      LogMsg := 'STATUS: ' + Text
    else if (Level <= LOG_LEVEL_INFO_MAX) then
      LogMsg := 'INFO:   ' + Text
    else
      LogMsg := 'DEBUG:  ' + Text;

    // output log-message
    if (Level <= LogLevel) then
    begin
      DebugWriteLn(LogMsg);
      LogConsole(LogMsg);
    end;
    
    // write message to log-file
    if (Level <= LogFileLevel) then
    begin
      LogToFile(LogMsg);
    end;
  end;

  // exit application on criticial errors (cannot be turned off)
  if (Level <= LOG_LEVEL_CRITICAL_MAX) then
  begin
    // Show information (window)
    ShowMessage(Text, mtError);
    Halt;
  end;
end;

procedure TLog.LogMsg(const Msg, Context: string; Level: integer);
begin
  LogMsg(Msg + ' ['+Context+']', Level);
end;

procedure TLog.LogDebug(const Msg, Context: string);
begin
  LogMsg(Msg, Context, LOG_LEVEL_DEBUG);
end;

procedure TLog.LogInfo(const Msg, Context: string);
begin
  LogMsg(Msg, Context, LOG_LEVEL_INFO);
end;

procedure TLog.LogStatus(const Msg, Context: string);
begin
  LogMsg(Msg, Context, LOG_LEVEL_STATUS);
end;

procedure TLog.LogWarn(const Msg, Context: string);
begin
  LogMsg(Msg, Context, LOG_LEVEL_WARN);
end;

procedure TLog.LogError(const Msg, Context: string);
begin
  LogMsg(Msg, Context, LOG_LEVEL_ERROR);
end;

procedure TLog.LogError(const Text: string);
begin
  LogMsg(Text, LOG_LEVEL_ERROR);
end;

procedure TLog.CriticalError(const Text: string);
begin
  LogMsg(Text, LOG_LEVEL_CRITICAL);
end;

procedure TLog.LogCritical(const Msg, Context: string);
begin
  LogMsg(Msg, Context, LOG_LEVEL_CRITICAL);
end;

type
  TRiffChunkID = array[0..3] of byte;

  TRiffChunk = packed record
    ID: TRiffChunkID;
    DataSize: cardinal;
  end;

  TRiffHeader = packed record
    ChunkInfo: TRiffChunk;
    RiffType: TRiffChunkID;
  end;

  TWaveFmtChunk = packed record
    ChunkInfo: TRiffChunk;
    FormatTag: word;
    NumChannels: word;
    SamplesPerSec: cardinal;
    AvgBytesPerSec: cardinal;
    BlockAlign: word;
    BitsPerSample: word;
  end;

procedure TLog.LogVoice(SoundNr: integer);
var
  Stream: TBinaryFileStream;
  Prefix: string;
  FileName: IPath;
  Num: integer;
  CaptureBuffer: TCaptureBuffer;
  Buffer: TMemoryStream;
  FormatInfo: TAudioFormatInfo;
  WaveHdr: TRiffHeader;
  WaveFmt: TWaveFmtChunk;
  DataChunk: TRiffChunk;
  UseWavFile: boolean;
  FileExt: string;
const
  Channels = 1;
  SampleRate = 44100;
  RIFF_CHUNK_HDR: TRiffChunkID = (Ord('R'), Ord('I'), Ord('F'), Ord('F'));
  RIFF_CHUNK_FMT: TRiffChunkID = (Ord('f'), Ord('m'), Ord('t'), Ord(' '));
  RIFF_CHUNK_DATA: TRiffChunkID = (Ord('d'), Ord('a'), Ord('t'), Ord('a'));
  RIFF_TYPE_WAVE: TRiffChunkID = (Ord('W'), Ord('A'), Ord('V'), Ord('E'));
  WAVE_FORMAT_PCM = 1; // PCM (uncompressed)
begin
  CaptureBuffer := AudioInputProcessor.Sound[SoundNr];
  Buffer := CaptureBuffer.LogBuffer;
  FormatInfo := CaptureBuffer.AudioFormat;

  // not all formats can be stored in a wav-file
  UseWavFile := (FormatInfo.Format in [asfU8, asfS16, asfS16LSB]);

  // create output filename
  for Num := 1 to 9999 do begin
    Prefix := Format('Voice%.4d', [Num]);
    if (UseWavFile) then
      FileExt := '.wav'
    else
      FileExt := '.raw';
    FileName := LogPath.Append(Prefix + FileExt);
    if not FileName.Exists() then
      break
  end;

  // open output file
  Stream := TBinaryFileStream.Create(FileName, fmCreate);
  
  // write wav-file header
  if (UseWavFile) then
  begin
    WaveHdr.ChunkInfo.ID := RIFF_CHUNK_HDR;
    WaveHdr.ChunkInfo.DataSize := (SizeOf(TRiffHeader) - 8) +
        SizeOf(TWaveFmtChunk) + SizeOf(TRiffChunk) + Buffer.Size;
    WaveHdr.RiffType := RIFF_TYPE_WAVE;
    Stream.Write(WaveHdr, SizeOf(TRiffHeader));

    WaveFmt.ChunkInfo.ID := RIFF_CHUNK_FMT;
    WaveFmt.ChunkInfo.DataSize := SizeOf(TWaveFmtChunk) - 8;
    WaveFmt.FormatTag := WAVE_FORMAT_PCM;
    WaveFmt.NumChannels := FormatInfo.Channels;
    WaveFmt.SamplesPerSec := Round(FormatInfo.SampleRate);
    WaveFmt.AvgBytesPerSec := Round(FormatInfo.BytesPerSec);
    WaveFmt.BlockAlign := FormatInfo.FrameSize;
    WaveFmt.BitsPerSample := FormatInfo.SampleSize * 8;
    Stream.Write(WaveFmt, SizeOf(TWaveFmtChunk));

    DataChunk.ID := RIFF_CHUNK_DATA;
    DataChunk.DataSize := Buffer.Size;
    Stream.Write(DataChunk, SizeOf(TRiffChunk));
  end;

  Buffer.Seek(0, soBeginning);
  Stream.CopyFrom(Buffer, Buffer.Size);

  Stream.Free;
end;

procedure TLog.LogBuffer(const buf: Pointer; const bufLength: Integer; const filename: IPath);
var
  f : TBinaryFileStream;
begin
  try
    f := TBinaryFileStream.Create( filename, fmCreate);
    try
      f.Write( buf^, bufLength);
    finally
      f.Free;
    end;
  except on e : Exception do
    Log.LogError('TLog.LogBuffer: Failed to log buffer into file "' + filename.ToNative + '". ErrMsg: ' + e.Message);
  end;
end;

procedure TLog.ClearConsoleLog;
begin
  EnterCriticalSection(Lock);
  ConsoleBuffer.Clear;
  LeaveCriticalSection(Lock);
end;

function TLog.GetConsole(const index: integer; FromTheBeginning: boolean = false): string;
begin
  EnterCriticalSection(Lock);
  if FromTheBeginning then Result := ConsoleBuffer[index]
  else Result := ConsoleBuffer[ConsoleBuffer.Count-1-index];
  LeaveCriticalSection(Lock);
end;

function TLog.GetConsoleCount: integer;
begin
  EnterCriticalSection(Lock);
  Result := ConsoleBuffer.Count;
  LeaveCriticalSection(Lock);
end;

procedure TLog.LogConsole(const Text: string);
begin
  EnterCriticalSection(Lock);
  ConsoleBuffer.Insert(0, Text);
  if ConsoleBuffer.Count > CONSOLE_SCROLLBACK_SIZE then ConsoleBuffer.Capacity:=CONSOLE_SCROLLBACK_SIZE;
  LeaveCriticalSection(Lock);
end;

end.


