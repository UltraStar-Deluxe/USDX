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
    private
      BufferNew:    TMemoryStream;              // buffer for newest samples
    public
      BufferArray:  array[0..4095] of smallint; // newest 4096 samples
      BufferLong:   array of TMemoryStream;     // full buffer

      Index: integer;           // index in TAudioInputProcessor.Sound[] (TODO: Remove if not used)

      AnalysisBufferSize: integer; // number of samples to analyze

      // pitch detection
      ToneValid:    boolean;    // true if Tone contains a valid value (otherwise it contains noise)
      //Peak:       integer;    // position of peak on horizontal pivot (TODO: Remove if not used)
      //ToneAccuracy: real;     // tone accuracy (TODO: Remove if not used)
      Tone:         integer;    // TODO: should be a non-unified full range tone (e.g. C2<>C3). Range: 0..NumHalftones-1
                                // Note: at the moment it is the same as ToneUnified
      ToneUnified:  integer;    // tone unified to one octave (e.g. C2=C3=C4). Range: 0-11
      //Scale:      real;       // FFT scale (TODO: Remove if not used)

      // procedures
      procedure ProcessNewBuffer;
      procedure AnalyzeBuffer;    // use to analyze sound from buffers to get new pitch
      procedure AnalyzeByAutocorrelation;    // we call it to analyze sound by checking Autocorrelation
      function  AnalyzeAutocorrelationFreq(Freq: real): real;   // use this to check one frequency by Autocorrelation
  end;

  TAudioInputDeviceSource = record
    Name:   string;
  end;

  // soundcard input-devices information
  TAudioInputDevice = class
    public
      CfgIndex:        integer;   // index of this device in Ini.InputDeviceConfig
      Description:     string;    // soundcard name/description
      Source:          array of TAudioInputDeviceSource; // soundcard input(-source)s
      SourceSelected:  integer;  // unused. What is this good for?
      MicInput:        integer;  // unused. What is this good for?
      SampleRate:      integer;  // capture sample-rate (e.g. 44.1kHz -> 44100)
      CaptureChannel:  array[0..1] of TSound; // sound(-buffers) used for left/right channel's capture data

      procedure Start(); virtual; abstract;
      procedure Stop();  virtual; abstract;

      destructor Destroy; override;
  end;

  TAudioInputProcessor = class
    Sound:  array of TSound;
    Device: array of TAudioInputDevice;

    constructor Create;

    // handle microphone input
    procedure HandleMicrophoneData(Buffer: Pointer; Size: Cardinal;
                                   InputDevice: TAudioInputDevice);

    function Volume( aChannel : byte ): byte;
  end;

  TAudioInputBase = class( TInterfacedObject, IAudioInput )
    private
      Started: boolean;
    protected
      function UnifyDeviceName(const name: string; deviceIndex: integer): string;
      function UnifyDeviceSourceName(const name: string; const deviceName: string): string;
    public
      function GetName: String;           virtual; abstract;
      function InitializeRecord: boolean; virtual; abstract;

      procedure CaptureStart;
      procedure CaptureStop;
  end;


  SmallIntArray = array [0..maxInt shr 1-1] of smallInt;
  PSmallIntArray = ^SmallIntArray;

  function AudioInputProcessor(): TAudioInputProcessor;

implementation

uses
  ULog,
  UMain;

const
  CaptureFreq = 44100;
  BaseToneFreq = 65.4064; // lowest (half-)tone to analyze (C2 = 65.4064 Hz)
  NumHalftones = 36; // C2-B4 (for Whitney and my high voice)

var
  singleton_AudioInputProcessor : TAudioInputProcessor = nil;


// FIXME: Race-Conditions between Callback-thread and main-thread
//        on BufferArray (maybe BufferNew also).
//        Use SDL-mutexes to solve this problem.


{ Global }

function AudioInputProcessor(): TAudioInputProcessor;
begin
  if singleton_AudioInputProcessor = nil then
    singleton_AudioInputProcessor := TAudioInputProcessor.create();

  result := singleton_AudioInputProcessor;
end;


{ TAudioInputDevice }

destructor TAudioInputDevice.Destroy;
var
  i: integer;
begin
  Stop();
  Source := nil;
  for i := 0 to High(CaptureChannel) do
    CaptureChannel[i] := nil;
  inherited Destroy;
end;


{ TSound }

procedure TSound.ProcessNewBuffer;
var
  SkipCount:   integer;
  NumSamples:  integer;
  SampleIndex: integer;
begin
  // process BufferArray
  SkipCount := 0;
  NumSamples := BufferNew.Size div 2;

  // check if we have more new samples than we can store
  if NumSamples > Length(BufferArray) then
  begin
    // discard the oldest of the new samples
    SkipCount := NumSamples - Length(BufferArray);
    NumSamples := Length(BufferArray);
  end;

  // move old samples to the beginning of the array (if necessary)
  for SampleIndex := NumSamples to High(BufferArray) do
    BufferArray[SampleIndex-NumSamples] := BufferArray[SampleIndex];

  // skip samples if necessary
  BufferNew.Seek(2*SkipCount, soBeginning);
  // copy samples
  BufferNew.ReadBuffer(BufferArray[Length(BufferArray)-NumSamples], 2*NumSamples);

  // save capture-data to BufferLong if neccessary
  if Ini.SavePlayback = 1 then
  begin
    BufferNew.Seek(0, soBeginning);
    BufferLong[0].CopyFrom(BufferNew, BufferNew.Size);
  end;
end;

procedure TSound.AnalyzeBuffer;
begin
  AnalyzeByAutocorrelation;
end;

procedure TSound.AnalyzeByAutocorrelation;
var
  ToneIndex: integer;
  Freq:      real;
  Wages:     array[0..NumHalftones-1] of real;
  MaxTone:   integer;
  MaxWage:   real;
  Volume:    real;
  MaxVolume: real;
  SampleIndex: integer;
  Threshold: real;
const
  HalftoneBase = 1.05946309436; // 2^(1/12) -> HalftoneBase^12 = 2 (one octave)
begin
  ToneValid := false;

  // find maximum volume of first 1024 samples
  MaxVolume := 0;
  for SampleIndex := 0 to 1023 do
  begin
    Volume := Abs(BufferArray[SampleIndex]) /
              -Low(Smallint); // was $10000 (65536) before but must be 32768

    if Volume > MaxVolume then
       MaxVolume := Volume;
  end;

  // prepare to analyze
  MaxWage := 0;

  // analyze halftones
  for ToneIndex := 0 to NumHalftones-1 do
  begin
    Freq := BaseToneFreq * Power(HalftoneBase, ToneIndex);
    Wages[ToneIndex] := AnalyzeAutocorrelationFreq(Freq);

    if Wages[ToneIndex] > MaxWage then
    begin
      // this frequency has better wage
      MaxWage := Wages[ToneIndex];
      MaxTone := ToneIndex;
    end;
  end;

  Threshold := 0.2;
  case Ini.Threshold of
    0:  Threshold := 0.1;
    1:  Threshold := 0.2;
    2:  Threshold := 0.3;
    3:  Threshold := 0.4;
  end;

  // check if signal has an acceptable volume (ignore background-noise)
  if MaxVolume >= Threshold then
  begin
    ToneValid   := true;
    ToneUnified := MaxTone mod 12;
    Tone        := MaxTone mod 12;
  end;

end;

function TSound.AnalyzeAutocorrelationFreq(Freq: real): real; // result medium difference
var
  Dist:                   real;    // distance (0=equal .. 1=totally different) between correlated samples 
  AccumDist:              real;    // accumulated distances
  SampleIndex:            integer; // index of sample to analyze
  CorrelatingSampleIndex: integer; // index of sample one period ahead
  SamplesPerPeriod:       integer; // samples in one period
begin
  SampleIndex := 0;
  SamplesPerPeriod := Round(CaptureFreq/Freq);
  CorrelatingSampleIndex := SampleIndex + SamplesPerPeriod;

  AccumDist := 0;

  // compare correlating samples
  while (CorrelatingSampleIndex < AnalysisBufferSize) do
  begin
    // calc distance (correlation: 1-dist) to corresponding sample in next period
    Dist := Abs(BufferArray[SampleIndex] - BufferArray[CorrelatingSampleIndex]) /
            High(Word); // was $10000 (65536) before but must be 65535
    AccumDist := AccumDist + Dist;
    Inc(SampleIndex);
    Inc(CorrelatingSampleIndex);
  end;

  // return "inverse" average distance (=correlation)
  Result := 1 - AccumDist / AnalysisBufferSize;
end;


{ TAudioInputProcessor }

{*
 * Handle captured microphone input data.
 * Params:
 *   Buffer - buffer of signed 16bit interleaved stereo PCM-samples.
 *     Interleaved means that a right-channel sample follows a left-
 *     channel sample and vice versa (0:left[0],1:right[0],2:left[1],...).
 *   Length - number of bytes in Buffer
 *   Input - Soundcard-Input used for capture
 *}
procedure TAudioInputProcessor.HandleMicrophoneData(Buffer: Pointer; Size: Cardinal; InputDevice: TAudioInputDevice);
var
  NumSamples:   integer; // number of samples
  SampleIndex:  integer;
  Value:        integer;
  ByteBuffer:   PByteArray;     // buffer handled as array of bytes
  SampleBuffer: PSmallIntArray; // buffer handled as array of samples
  Offset: integer;
  Boost:  byte;
  ChannelCount: integer;
  ChannelIndex: integer;
  CaptureChannel: TSound;
  SampleSize: integer;
begin
  // set boost
  case Ini.MicBoost of
    0:  Boost := 1;
    1:  Boost := 2;
    2:  Boost := 4;
    3:  Boost := 8;
  end;

  // boost buffer
  NumSamples := Size div 2;
  SampleBuffer := Buffer;
  for SampleIndex := 0 to NumSamples-1 do
  begin
    Value := SampleBuffer^[SampleIndex] * Boost;

    // TODO :  JB -  This will clip the audio... cant we reduce the "Boost" if the data clips ??
    if Value > High(Smallint) then
      Value := High(Smallint);

    if Value < Low(Smallint) then
      Value := Low(Smallint);

    SampleBuffer^[SampleIndex] := Value;
  end;

  // number of channels
  ChannelCount := Length(InputDevice.CaptureChannel);
  // size of one sample
  SampleSize := ChannelCount * SizeOf(SmallInt);
  // samples per channel
  NumSamples := Size div SampleSize;

  // interpret buffer as buffer of bytes
  ByteBuffer := Buffer;

  // process channels
  for ChannelIndex := 0 to High(InputDevice.CaptureChannel) do
  begin
    CaptureChannel := InputDevice.CaptureChannel[ChannelIndex];
    if (CaptureChannel <> nil) then
    begin
      Offset := ChannelIndex * SizeOf(SmallInt);

      // TODO: remove BufferNew and write to BufferArray directly

      CaptureChannel.BufferNew.Clear;
      for SampleIndex := 0 to NumSamples-1 do
      begin
        CaptureChannel.BufferNew.Write(ByteBuffer^[Offset + SampleIndex*SampleSize],
                                       SizeOf(SmallInt));
      end;
      CaptureChannel.ProcessNewBuffer();
    end;
  end;
end;

constructor TAudioInputProcessor.Create;
var
  i:        integer;
begin
  SetLength(Sound, 6 {max players});//Ini.Players+1);
  for i := 0 to High(Sound) do
  begin
    Sound[i] := TSound.Create;
    Sound[i].Index := i;
    Sound[i].BufferNew := TMemoryStream.Create;
    SetLength(Sound[i].BufferLong, 1);
    Sound[i].BufferLong[0] := TMemoryStream.Create;
    Sound[i].AnalysisBufferSize := Min(4*1024, Length(Sound[i].BufferArray));
  end;
end;

function TAudioInputProcessor.Volume( aChannel : byte ): byte;
var
  lSampleIndex: Integer;
  lMaxVol : Word;
begin;
  with AudioInputProcessor.Sound[aChannel] do
  begin
    lMaxVol := BufferArray[0];
    for lSampleIndex := 1 to High(BufferArray) do
    begin
      if Abs(BufferArray[lSampleIndex]) > lMaxVol then
        lMaxVol := Abs(BufferArray[lSampleIndex]);
    end;
  end;

  result := trunc( ( 255 / -Low(Smallint) ) * lMaxVol );
end;


{ TAudioInputBase }

{*
 * Start capturing on all used input-device.
 *}
procedure TAudioInputBase.CaptureStart;
var
  S:  integer;
  DeviceIndex: integer;
  ChannelIndex: integer;
  Device: TAudioInputDevice;
  DeviceCfg: PInputDeviceConfig;
  DeviceUsed: boolean;
  Player: integer;
begin
  if (Started) then
    CaptureStop();

  Log.BenchmarkStart(1);

  // reset buffers
  for S := 0 to High(AudioInputProcessor.Sound) do
    AudioInputProcessor.Sound[S].BufferLong[0].Clear;

  // start capturing on each used device
  for DeviceIndex := 0 to High(AudioInputProcessor.Device) do begin
    Device := AudioInputProcessor.Device[DeviceIndex];
    if not assigned(Device) then
      continue;
    DeviceCfg := @Ini.InputDeviceConfig[Device.CfgIndex];

    DeviceUsed := false;

    // check if device is used
    for ChannelIndex := 0 to High(DeviceCfg.ChannelToPlayerMap) do
    begin
      Player := DeviceCfg.ChannelToPlayerMap[ChannelIndex]-1;
      if (Player < 0) or (Player >= PlayersPlay) then
      begin
        Device.CaptureChannel[ChannelIndex] := nil;
      end
      else
      begin
        Device.CaptureChannel[ChannelIndex] := AudioInputProcessor.Sound[Player];
        DeviceUsed := true;
      end;
    end;

    // start device if used
    if (DeviceUsed) then begin
  Log.BenchmarkStart(2);
      Device.Start();
  Log.BenchmarkEnd(2);
  Log.LogBenchmark('Device.Start', 2) ;
    end;
  end;

  Log.BenchmarkEnd(1);
  Log.LogBenchmark('CaptureStart', 1) ;

  Started := true;
end;

{*
 * Stop input-capturing on all soundcards.
 *}
procedure TAudioInputBase.CaptureStop;
var
  DeviceIndex: integer;
  Player:  integer;
  Device: TAudioInputDevice;
  DeviceCfg: PInputDeviceConfig;
begin
  for DeviceIndex := 0 to High(AudioInputProcessor.Device) do begin
    Device := AudioInputProcessor.Device[DeviceIndex];
    if not assigned(Device) then
      continue;
    Device.Stop();
  end;

  Started := false;
end;

function TAudioInputBase.UnifyDeviceName(const name: string; deviceIndex: integer): string;
var
  count: integer; // count of devices with this name

  function IsDuplicate(const name: string): boolean;
  var
    i: integer;
  begin
    Result := False;
    // search devices with same description
    For i := 0 to deviceIndex-1 do
    begin
      if (AudioInputProcessor.Device[i].Description = name) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
begin
  count := 1;
  result := name;

  // if there is another device with the same ID, search for an available name
  while (IsDuplicate(result)) do
  begin
    Inc(count);
    // set description
    result := name + ' ('+IntToStr(count)+')';
  end;
end;

{*
 * Unifies an input-device's source name.
 * Note: the description member of the device must already be set when
 * calling this function.
 *}
function TAudioInputBase.UnifyDeviceSourceName(const name: string; const deviceName: string): string;
var
  Descr: string;
begin
  result := name;

  {$IFDEF DARWIN}
    // Under MacOSX the SingStar Mics have an empty
    // InputName. So, we have to add a hard coded
    // Workaround for this problem
    if (name = '') and (Pos( 'USBMIC Serial#', deviceName) > 0) then
    begin
      result := 'Microphone';
    end;
  {$ENDIF}
end;

end.



