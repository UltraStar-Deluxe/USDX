unit ULog;

interface

uses Classes;

type
  TLog = class
    BenchmarkTimeStart:   array[0..7] of real;
    BenchmarkTimeLength:  array[0..7] of real;//TDateTime;

    FileBenchmark:    TextFile;
    FileBenchmarkO:   boolean; // opened
    FileAnalyze:      TextFile;
    FileAnalyzeO:     boolean; // opened
    FileError:        TextFile;
    FileErrorO:       boolean; // opened

    Title: String; //Application Title

    // destuctor
    destructor Free;

    // benchmark
    procedure BenchmarkStart(Number: integer);
    procedure BenchmarkEnd(Number: integer);
    procedure LogBenchmark(Text: string; Number: integer);

    // analyze
    procedure LogAnalyze(Text: string);

    // error
    procedure LogError(Text: string); overload;

    //Critical Error (Halt + MessageBox)
    procedure CriticalError(Text: string);

    // voice
    procedure LogVoice(SoundNr: integer);

    // compability
    procedure LogStatus(Log1, Log2: string);
    procedure LogError(Log1, Log2: string); overload;
  end;

var
  Log:    TLog;

implementation
uses UPliki, SysUtils, DateUtils, URecord, UTime, UIni, Windows;

destructor TLog.Free;
begin
  if FileBenchmarkO then CloseFile(FileBenchmark);
//  if FileAnalyzeO then CloseFile(FileAnalyze);
  if FileErrorO then CloseFile(FileError);
end;

procedure TLog.BenchmarkStart(Number: integer);
begin
  BenchmarkTimeStart[Number] := USTime.GetTime; //Time;
end;

procedure TLog.BenchmarkEnd(Number: integer);
begin
  BenchmarkTimeLength[Number] := USTime.GetTime {Time} - BenchmarkTimeStart[Number];
end;

procedure TLog.LogBenchmark(Text: string; Number: integer);
var
  Minutes:      integer;
  Seconds:      integer;
  Miliseconds:  integer;

  MinutesS:     string;
  SecondsS:     string;
  MilisecondsS: string;

  ValueText:    string;
begin
  if (ParamStr(1) = '-benchmark') then begin
    if not FileBenchmarkO then begin
      FileBenchmarkO := true;
      AssignFile(FileBenchmark, LogPath + 'Benchmark.log');
      {$I-}
      Rewrite(FileBenchmark);
      if IOResult = 0 then FileBenchmarkO := true;
      {$I+}
    end;

  if FileBenchmarkO then begin
    Miliseconds := Trunc(Frac(BenchmarkTimeLength[Number]) * 1000);
    Seconds := Trunc(BenchmarkTimeLength[Number]) mod 60;
    Minutes := Trunc((BenchmarkTimeLength[Number] - Seconds) / 60);
//    ValueText := FloatToStr(BenchmarkTimeLength[Number]);

{    ValueText := FloatToStr(
      SecondOf(BenchmarkTimeLength[Number]) + MilliSecondOf(BenchmarkTimeLength[Number])/1000
      );
    if MinuteOf(BenchmarkTimeLength[Number]) >= 1 then
      ValueText := IntToStr(MinuteOf(BenchmarkTimeLength[Number])) + ':' + ValueText;
    WriteLn(FileBenchmark, Text + ': ' + ValueText + ' seconds');}

    if (Minutes = 0) and (Seconds = 0) then begin
      MilisecondsS := IntToStr(Miliseconds);
      ValueText := MilisecondsS + ' miliseconds';
    end;

    if (Minutes = 0) and (Seconds >= 1) then begin
      MilisecondsS := IntToStr(Miliseconds);
      while Length(MilisecondsS) < 3 do MilisecondsS := '0' + MilisecondsS;

      SecondsS := IntToStr(Seconds);

      ValueText := SecondsS + ',' + MilisecondsS + ' seconds';
    end;

    if Minutes >= 1 then begin
      MilisecondsS := IntToStr(Miliseconds);
      while Length(MilisecondsS) < 3 do MilisecondsS := '0' + MilisecondsS;

      SecondsS := IntToStr(Seconds);
      while Length(SecondsS) < 2 do SecondsS := '0' + SecondsS;

      MinutesS := IntToStr(Minutes);

      ValueText := MinutesS + ':' + SecondsS + ',' + MilisecondsS + ' minutes';
    end;

    WriteLn(FileBenchmark, Text + ': ' + ValueText);
    Flush(FileBenchmark);
    end;
  end;
end;

procedure TLog.LogAnalyze(Text: string);
var
  Seconds:      integer;
  Miliseconds:  integer;
  ValueText:    string;
begin
  if Ini.Debug = 1 then begin

  if not FileAnalyzeO then begin
    AssignFile(FileAnalyze, LogPath + 'Analyze.log');
    {$I-}
    Rewrite(FileAnalyze);
    if IOResult = 0 then FileAnalyzeO := true;
    {$I+}
  end;

  if FileAnalyzeO then begin
    WriteLn(FileAnalyze, Text);
    Flush(FileAnalyze); // try to speed up
  end;

  end;
end;

procedure TLog.LogError(Text: string);
begin
  if not FileErrorO then begin
    FileErrorO := true;
    AssignFile(FileError, LogPath + 'Error.log');
    {$I-}
    Rewrite(FileError);
    if IOResult = 0 then FileErrorO := true;
    {$I+}
  end;

  if FileErrorO then begin
    WriteLn(FileError, Text);
    Flush(FileError);
  end;
end;

procedure TLog.LogVoice(SoundNr: integer);
var
  FileVoice:    File;
  FS:           TFileStream;
  FileName:     string;
  Num:          integer;
  BL:           integer;
begin
  for Num := 1 to 9999 do begin
    FileName := IntToStr(Num);
    while Length(FileName) < 4 do FileName := '0' + FileName;
    FileName := LogPath + 'Voice' + FileName + '.raw';
    if not FileExists(FileName) then break
  end;


  FS := TFileStream.Create(FileName, fmCreate);

  for BL := 0 to High(Sound[SoundNr].BufferLong) do begin
    Sound[SoundNr].BufferLong[BL].Seek(0, soBeginning);
    FS.CopyFrom(Sound[SoundNr].BufferLong[BL], Sound[SoundNr].BufferLong[BL].Size);
  end;

  FS.Free;
end;

procedure TLog.LogStatus(Log1, Log2: string);
begin
//asd
  LogError (Log2 + ': ' + Log1);
end;

procedure TLog.LogError(Log1, Log2: string);
begin
//asd
end;

procedure TLog.CriticalError(Text: string);
begin
  //Write Error to Logfile:
  LogError (Text);

  //Show Errormessage
  Messagebox(0, PChar(Text), PChar(Title), MB_ICONERROR or MB_OK);

  //Exit Application
  Halt;
end;

end.
 