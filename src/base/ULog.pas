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

unit ULog;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes;

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

type
  TLog = class
  private
    LogFile:             TextFile;
    LogFileOpened:       boolean;
    BenchmarkFile:       TextFile;
    BenchmarkFileOpened: boolean;

    LogLevel: integer;
    // level of messages written to the log-file
    LogFileLevel: integer;

    procedure LogToFile(const Text: string);
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
    procedure LogBuffer(const buf : Pointer; const bufLength : Integer; const filename : string);
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
  UTime,
  UCommon,
  UCommandLine;

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
  inherited;
  LogLevel := LOG_LEVEL_DEFAULT;
  LogFileLevel := LOG_FILE_LEVEL_DEFAULT;
  FileOutputEnabled := true;
end;

destructor TLog.Destroy;
begin
  if BenchmarkFileOpened then
    CloseFile(BenchmarkFile);
  //if AnalyzeFileOpened then
  //  CloseFile(AnalyzeFile);
  if LogFileOpened then
    CloseFile(LogFile);
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
  if (FileOutputEnabled and Params.Benchmark) then
  begin
    if not BenchmarkFileOpened then
    begin
      BenchmarkFileOpened := true;
      AssignFile(BenchmarkFile, LogPath + 'Benchmark.log');
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
end;

procedure TLog.LogToFile(const Text: string);
begin
  if (FileOutputEnabled and not LogFileOpened) then
  begin
    AssignFile(LogFile, LogPath + 'Error.log');
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
end;

procedure TLog.SetLogLevel(Level: integer);
begin
  LogLevel := Level;
end;

function TLog.GetLogLevel(): integer;
begin
  Result := LogLevel;
end;

procedure TLog.LogMsg(const Text: string; Level: integer);
var
  LogMsg: string;
begin
  // TODO: what if (LogFileLevel < LogLevel)? Log to file without printing to
  //  console or do not log at all? At the moment nothing is logged.
  if (Level <= LogLevel) then
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

procedure TLog.LogVoice(SoundNr: integer);
var
  FS:           TFileStream;
  FileName:     string;
  Num:          integer;
begin
  for Num := 1 to 9999 do begin
    FileName := IntToStr(Num);
    while Length(FileName) < 4 do
      FileName := '0' + FileName;
    FileName := LogPath + 'Voice' + FileName + '.raw';
    if not FileExists(FileName) then
      break
  end;

  FS := TFileStream.Create(FileName, fmCreate);

  AudioInputProcessor.Sound[SoundNr].LogBuffer.Seek(0, soBeginning);
  FS.CopyFrom(AudioInputProcessor.Sound[SoundNr].LogBuffer, AudioInputProcessor.Sound[SoundNr].LogBuffer.Size);

  FS.Free;
end;

procedure TLog.LogBuffer(const buf: Pointer; const bufLength: Integer; const filename: string);
var
  f : TFileStream;
begin
  f := nil;

  try
    f := TFileStream.Create( filename, fmCreate);
    f.Write( buf^, bufLength);
    f.Free;
  except
    on e : Exception do begin
      Log.LogError('TLog.LogBuffer: Failed to log buffer into file "' + filename + '". ErrMsg: ' + e.Message);
      f.Free;
    end;
  end;
end;

end.


