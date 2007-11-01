unit URecord;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses Classes,
     Math,
     SysUtils,
     {$IFDEF useBASS}
     bass,
     {$ENDIF}
     UCommon,
     UMusic,
     UIni;
     
type
  TSound = class
    BufferNew:    TMemoryStream; // buffer for newest sample
    BufferArray:  array[1..4096] of smallint; // (Signal) newest 4096 samples
    BufferLong:   array of TMemoryStream;     // full buffer

    Num:          integer;
    n:            integer; // length of Signal to analyze

    // pitch detection
    SzczytJest:   boolean;       // czy jest szczyt
    Szczyt:       integer;    // pozycja szczytu na osi poziomej
    TonDokl:      real;       // ton aktualnego szczytu
    Ton:          integer;    // ton bez ulamka
    TonGamy:      integer;    // ton w gamie. wartosci: 0-11
    Skala:        real;       // skala FFT

    // procedures
    procedure ProcessNewBuffer;
    procedure AnalizujBufor;    // use to analyze sound from buffers to get new pitch
    procedure AnalizujByAutocorrelation;    // we call it to analyze sound by checking Autocorrelation
    function  AnalyzeAutocorrelationFreq(Freq: real): real;   // use this to check one frequency by Autocorrelation
  end;

  TSoundCardInput = record
    Name:   string;
  end;

  TSoundCard = record
    // here can be the soundcard information - whole database from which user will select recording source
    Description:    string;
    Input:          array of TSoundCardInput;
    InputSelected:  integer;
    MicInput:       Integer;

    // bass record
    BassRecordStream: hStream;
  end;

  TRecord = class
    SoundCard:  array of TSoundCard;
    constructor Create;
  end;

  smallintarray = array [0..maxInt shr 1-1] of smallInt;
  psmallintarray = ^smallintarray;

  // procedures - bass record
  function GetMicrophone(handle: HSTREAM; buffer: Pointer; len: DWORD; user: DWORD): boolean; stdcall;


var
  Sound:      array of TSound;
  SoundCard:  array of TSoundCard;
  Poz:        integer;
  Recording:  TRecord;

implementation

uses UMain;

procedure TSound.ProcessNewBuffer;
var
  S:    integer;
  L:    integer;
  A:    integer;
begin
  // process BufferArray
  S := 0;
  L := BufferNew.Size div 2;
  if L > n then begin
    S := L - n;
    L := n;
  end;

  // copy to array
  for A := L+1 to n do
    BufferArray[A-L] := BufferArray[A];

  BufferNew.Seek(2*S, soBeginning);
  BufferNew.ReadBuffer(BufferArray[1+n-L], 2*L);

  // process BufferLong
  if Ini.SavePlayback = 1 then
  begin
    BufferNew.Seek(0, soBeginning);
    BufferLong[0].CopyFrom(BufferNew, BufferNew.Size);
  end;
end;

procedure TSound.AnalizujBufor;
begin
  AnalizujByAutocorrelation;
end;

procedure TSound.AnalizujByAutocorrelation;
var
  T:        integer;  // tone
  F:        real; // freq
  Wages:    array[0..35] of real; // wages
  MaxT:     integer; // max tone
  MaxW:     real; // max wage
  V:        real; // volume
  MaxV:     real; // max volume
  S:        integer; // Signal
  Threshold:  real; // threshold
begin
  SzczytJest := false;

  // find maximum volume of first 1024 words of signal
  MaxV := 0;
  for S := 1 to 1024 do // 0.5.2: fix. was from 0 to 1023
  begin
    V  := Abs(BufferArray[S]) / $10000;

    if V > MaxV then
       MaxV := V;
  end;

  // prepare to analyze
  MaxW := 0;

  // analyze all 12 halftones
  for T := 0 to 35 do // to 11, then 23, now 35 (for Whitney and my high voice)
  begin
    F := 130.81*Power(1.05946309436, T)/2; // let's analyze below 130.81
    Wages[T] := AnalyzeAutocorrelationFreq(F);

    if Wages[T] > MaxW then
    begin // this frequency has better wage
      MaxW := Wages[T];
      MaxT := T;
    end;
  end; // for T

  Threshold := 0.1;
  case Ini.Threshold of
    0:  Threshold := 0.05;
    1:  Threshold := 0.1;
    2:  Threshold := 0.15;
    3:  Threshold := 0.2;
  end;

  if MaxV >= Threshold then
  begin // found acceptable volume // 0.1
    SzczytJest := true;
    TonGamy := MaxT mod 12;
    Ton := MaxT mod 12;
  end;

end;

function TSound.AnalyzeAutocorrelationFreq(Freq: real): real; // result medium difference
var
  Count:      real;
  Src:        integer;
  Dst:        integer;
  Move:       integer;
  Il:         integer; // how many counts were done
begin
  // we use Signal as source
  Count := 0;
  Il    := 0;
  Src   := 1;
  Move  := Round(44100/Freq);
  Dst   := Src + Move;

  // ver 2 - compare in vertical
  while (Dst < n) do
  begin // process up to n (4KB) of Signal
    Count := Count + Abs(BufferArray[Src] - BufferArray[Dst]) / $10000;
    Inc(Src);
    Inc(Dst);
    Inc(Il);
  end;

  Result := 1 - Count / Il;
end;

function GetMicrophone(handle: HSTREAM; buffer: Pointer; len: DWORD; user: DWORD): boolean; stdcall;
var
  L:    integer;
  S:    integer;
  PB:   pbytearray;
  PSI:  psmallintarray;
  I:    integer;
  Skip: integer;
  P1:   integer;
  P2:   integer;
  Boost:  byte;
begin
  // set boost
  case Ini.MicBoost of
    0:  Boost := 1;
    1:  Boost := 2;
    2:  Boost := 4;
    3:  Boost := 8;
  end;

  // boost buffer
  L := Len div 2; // number of samples
  PSI := Buffer;
  for S := 0 to L-1 do
  begin
    I := PSI^[S] * Boost;
    if I > 32767 then I := 32767; // 0.5.0: limit
    if I < -32768 then I := -32768; // 0.5.0: limit
    PSI^[S] := I;
  end;

  // decode user
  P1 := (user and 255) - 1;
  P2 := (user div 256) - 1;


  // 2 players USB mic, left channel
  if P1 >= 0 then
  begin
    L  := Len div 4; // number of samples
    PB := Buffer;

    Sound[P1].BufferNew.Clear; // 0.5.2: problem on exiting
    for S := 1 to L do
    begin
      Sound[P1].BufferNew.Write(PB[(S-1)*4], 2);
    end;
    Sound[P1].ProcessNewBuffer;
  end;

  // 2 players USB mic, right channel
  Skip := 2;

  if P2 >= 0 then
  begin
    L := Len div 4; // number of samples
    PB := Buffer;
    Sound[P2].BufferNew.Clear;
    for S := 1 to L do
    begin
      Sound[P2].BufferNew.Write(PB[Skip + (S-1)*4], 2);
    end;
    Sound[P2].ProcessNewBuffer;
  end;

  Result := true;
end;

constructor TRecord.Create;
var
  SC:         integer; // soundcard
  SCI:        integer; // soundcard input
  Descr:      string;
  InputName:  PChar;
  Flags:      integer;
  No:         integer;
  
  function isDuplicate(Desc: String): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    //Check for Soundcard with same Description
    For I := 0 to SC-1 do
    begin
      if (SoundCard[I].Description = Desc) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;

begin
  // TODO : JB_linux - Reimplement recording, without bass for linux
  {$IFDEF useBASS}
  // checks for recording devices and puts them into array;
  SetLength(SoundCard, 0);

  SC := 0;
  Descr := BASS_RecordGetDeviceDescription(SC);

  while (Descr <> '') do
  begin

    //If there is another SoundCard with the Same ID, Search an available Name
    if (IsDuplicate(Descr)) then
    begin
      No:= 1; //Count of SoundCards with  same Name
      Repeat
        Inc(No)
      Until not IsDuplicate(Descr + ' (' + InttoStr(No) + ')');

      //Set Description
      Descr := Descr + ' (' + InttoStr(No) + ')';
    end;

    SetLength(SoundCard, SC+1);

    SoundCard[SC].Description := Descr;

    //Get Recording Inputs
    SCI := 0;
    BASS_RecordInit(SC);

    InputName := BASS_RecordGetInputName(SCI);

    SetLength(SoundCard[SC].Input, 1);
    SoundCard[SC].Input[SCI].Name := InputName;

    // process each input
    while (InputName <> nil) do
    begin
      Flags := BASS_RecordGetInput(SCI);
      if (SCI >= 1) {AND (Flags AND BASS_INPUT_OFF = 0)}  then
      begin
        SetLength(SoundCard[SC].Input, SCI+1);
        SoundCard[SC].Input[SCI].Name := InputName;
      end;

      //Set Mic Index
      if ((Flags and BASS_INPUT_TYPE_MIC) = 1) then
        SoundCard[SC].MicInput := SCI;

      Inc(SCI);
      InputName := BASS_RecordGetInputName(SCI);
    end;

    BASS_RecordFree;

    Inc(SC);
    Descr := BASS_RecordGetDeviceDescription(SC);
  end; // while
  {$ENDIF}
end;


end.



