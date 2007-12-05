unit URecord;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses Classes,
     Math,
     SysUtils,
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

  TGenericSoundCard = class
    // here can be the soundcard information - whole database from which user will select recording source
    Description:    string;    // soundcard name/description
    Input:          array of TSoundCardInput; // soundcard input(-source)s
    InputSelected:  integer;   // unused. What is this good for?
    MicInput:       integer;   // unused. What is this good for?
    //SampleRate:     integer; // TODO: for sample-rate conversion (for devices that do not support 44.1kHz)
    CaptureSoundLeft:  TSound; // sound(-buffer) used for left channel capture data
    CaptureSoundRight: TSound; // sound(-buffer) used for right channel capture data
  end;

  TRecord = class
    Sound:      array of TSound;
    SoundCard:  array of TGenericSoundCard;
    constructor Create;
    
    // handle microphone input
    procedure HandleMicrophoneData(Buffer: Pointer; Length: Cardinal;
        InputDevice: TGenericSoundCard);
  end;

  smallintarray = array [0..maxInt shr 1-1] of smallInt;
  psmallintarray = ^smallintarray;

var
  Poz:        integer;
  Recording:  TRecord;

implementation

uses UMain;

// FIXME: Race-Conditions between Callback-thread and main-thread
//        on BufferArray (maybe BufferNew also).
//        Use SDL-mutexes to solve this problem.

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

{*
 * Handle captured microphone input data.
 * Params:
 *   Buffer - buffer of signed 16bit interleaved stereo PCM-samples.
 *     Interleaved means that a right-channel sample follows a left-
 *     channel sample and vice versa (0:left[0],1:right[0],2:left[1],...).
 *   Length - number of bytes in Buffer
 *   Input - Soundcard-Input used for capture
 *}
procedure TRecord.HandleMicrophoneData(Buffer: Pointer; Length: Cardinal;
    InputDevice: TGenericSoundCard);
var
  L:    integer;
  S:    integer;
  PB:   pbytearray;
  PSI:  psmallintarray;
  I:    integer;
  Skip: integer;
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
  L := Length div 2; // number of samples
  PSI := Buffer;
  for S := 0 to L-1 do
  begin
    I := PSI^[S] * Boost;
    if I > 32767 then I := 32767; // 0.5.0: limit
    if I < -32768 then I := -32768; // 0.5.0: limit
    PSI^[S] := I;
  end;

  // 2 players USB mic, left channel
  if InputDevice.CaptureSoundLeft <> nil then
  begin
    L  := Length div 4; // number of samples
    PB := Buffer;

    InputDevice.CaptureSoundLeft.BufferNew.Clear; // 0.5.2: problem on exiting
    for S := 0 to L-1 do
    begin
      InputDevice.CaptureSoundLeft.BufferNew.Write(PB[S*4], 2);
    end;
    InputDevice.CaptureSoundLeft.ProcessNewBuffer;
  end;

  // 2 players USB mic, right channel
  Skip := 2;

  if InputDevice.CaptureSoundRight <> nil then
  begin
    L := Length div 4; // number of samples
    PB := Buffer;
    InputDevice.CaptureSoundRight.BufferNew.Clear;
    for S := 0 to L-1 do
    begin
      InputDevice.CaptureSoundRight.BufferNew.Write(PB[Skip + S*4], 2);
    end;
    InputDevice.CaptureSoundRight.ProcessNewBuffer;
  end;
end;

constructor TRecord.Create;
var
  S:        integer;
begin
  SetLength(Sound, 6 {max players});//Ini.Players+1);
  for S := 0 to High(Sound) do begin //Ini.Players do begin
    Sound[S] := TSound.Create;
    Sound[S].Num := S;
    Sound[S].BufferNew := TMemoryStream.Create;
    SetLength(Sound[S].BufferLong, 1);
    Sound[S].BufferLong[0] := TMemoryStream.Create;
    Sound[S].n := 4*1024;
  end;
end;


end.



