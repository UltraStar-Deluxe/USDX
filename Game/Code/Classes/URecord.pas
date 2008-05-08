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

const
  BaseToneFreq = 65.4064; // lowest (half-)tone to analyze (C2 = 65.4064 Hz)
  NumHalftones = 36; // C2-B4 (for Whitney and my high voice)

type
  TCaptureBuffer = class
    private
      BufferNew:    TMemoryStream; // buffer for newest samples

      function GetToneString: string; // converts a tone to its string represenatation;
    public
      BufferArray:  array[0..4095] of smallint; // newest 4096 samples
      BufferLong:   TMemoryStream;              // full buffer
      AnalysisBufferSize: integer; // number of samples of BufferArray to analyze

      AudioFormat: TAudioFormatInfo;

      // pitch detection
      ToneValid:    boolean;    // true if Tone contains a valid value (otherwise it contains noise)
      Tone:         integer;    // tone relative to one octave (e.g. C2=C3=C4). Range: 0-11
      ToneAbs:      integer;    // absolute (full range) tone (e.g. C2<>C3). Range: 0..NumHalftones-1

      // methods
      constructor Create;
      destructor Destroy; override;

      procedure Clear;

      procedure ProcessNewBuffer;
      // use to analyze sound from buffers to get new pitch
      procedure AnalyzeBuffer;
      // we call it to analyze sound by checking Autocorrelation
      procedure AnalyzeByAutocorrelation;
      // use this to check one frequency by Autocorrelation
      function AnalyzeAutocorrelationFreq(Freq: real): real;
      function MaxSampleVolume: Single;

      property ToneString: string READ GetToneString;
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
      MicSource:       integer;  // unused. What is this good for?

      AudioFormat:     TAudioFormatInfo; // capture format info (e.g. 44.1kHz SInt16 stereo)
      CaptureChannel:  array of TCaptureBuffer; // sound-buffer references used for mono or stereo channel's capture data

      destructor Destroy; override;

      procedure LinkCaptureBuffer(ChannelIndex: integer; Sound: TCaptureBuffer);
      
      function Start(): boolean; virtual; abstract;
      procedure Stop();  virtual; abstract;
  end;

  TAudioInputProcessor = class
    public
      Sound:  array of TCaptureBuffer; // sound-buffers for every player
      Device: array of TAudioInputDevice;

      constructor Create;

      // handle microphone input
      procedure HandleMicrophoneData(Buffer: Pointer; Size: Cardinal;
                                     InputDevice: TAudioInputDevice);
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
//var
// i: integer; // Auto Removed, Unused Variable
begin
  Stop();
  Source := nil;
  CaptureChannel := nil;
  FreeAndNil(AudioFormat);
  inherited Destroy;
end;

procedure TAudioInputDevice.LinkCaptureBuffer(ChannelIndex: integer; Sound: TCaptureBuffer);
begin
  // check bounds
  if ((ChannelIndex < 0) or (ChannelIndex > High(CaptureChannel))) then
    Exit;

  // reset audio-format of old capture-buffer
  if (CaptureChannel[ChannelIndex] <> nil) then
    CaptureChannel[ChannelIndex].AudioFormat := nil;

  // set audio-format of new capture-buffer
  if (Sound <> nil) then
    Sound.AudioFormat := AudioFormat;

  // replace old with new buffer
  CaptureChannel[ChannelIndex] := Sound;
end;

{ TSound }

constructor TCaptureBuffer.Create;
begin
  inherited;
  BufferNew := TMemoryStream.Create;
  BufferLong := TMemoryStream.Create;
  AnalysisBufferSize := Min(4*1024, Length(BufferArray));
end;

destructor TCaptureBuffer.Destroy;
begin
  AudioFormat := nil;
  FreeAndNil(BufferNew);
  FreeAndNil(BufferLong);
  inherited;
end;

procedure TCaptureBuffer.Clear;
begin
  if assigned(BufferNew) then
    BufferNew.Clear;
  if assigned(BufferLong) then
    BufferLong.Clear;
  FillChar(BufferArray[0], Length(BufferArray) * SizeOf(SmallInt), 0);
end;

procedure TCaptureBuffer.ProcessNewBuffer;
var
  SkipCount:   integer;
  NumSamples:  integer;
  SampleIndex: integer;
begin
  // process BufferArray
  SkipCount := 0;
  NumSamples := BufferNew.Size div 2;

  // check if we have more new samples than we can store
  if (NumSamples > Length(BufferArray)) then
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
  if (Ini.SavePlayback = 1) then
  begin
    BufferNew.Seek(0, soBeginning);
    BufferLong.CopyFrom(BufferNew, BufferNew.Size);
  end;
end;

procedure TCaptureBuffer.AnalyzeBuffer;
var
  Volume:    real;
  MaxVolume: real;
  SampleIndex: integer;
  Threshold: real;
begin
  ToneValid := false;
  ToneAbs := -1;
  Tone    := -1;

  // find maximum volume of first 1024 samples
  MaxVolume := 0;
  for SampleIndex := 0 to 1023 do
  begin
    Volume := Abs(BufferArray[SampleIndex]) / -Low(Smallint);
    if Volume > MaxVolume then
       MaxVolume := Volume;
  end;

  case Ini.Threshold of
    0:   Threshold := 0.05;
    1:   Threshold := 0.1;
    2:   Threshold := 0.15;
    3:   Threshold := 0.2;
    else Threshold := 0.1;
  end;

  // check if signal has an acceptable volume (ignore background-noise)
  if MaxVolume >= Threshold then
  begin
    // analyse the current voice pitch
    AnalyzeByAutocorrelation;
    ToneValid := true;
  end;
end;

procedure TCaptureBuffer.AnalyzeByAutocorrelation;
var
  ToneIndex: integer;
  CurFreq:   real;
  CurWeight: real;
  MaxWeight: real;
  MaxTone:   integer;
const
  HalftoneBase = 1.05946309436; // 2^(1/12) -> HalftoneBase^12 = 2 (one octave)
begin
  // prepare to analyze
  MaxWeight := -1;

  // analyze halftones
  // Note: at the lowest tone (~65Hz) and a buffer-size of 4096
  // at 44.1 (or 48kHz) only 6 (or 5) samples are compared, this might be
  // too few samples -> use a bigger buffer-size
  for ToneIndex := 0 to NumHalftones-1 do
  begin
    CurFreq := BaseToneFreq * Power(HalftoneBase, ToneIndex);
    CurWeight := AnalyzeAutocorrelationFreq(CurFreq);

    // TODO: prefer higher frequencies (use >= or use downto)
    if (CurWeight > MaxWeight) then
    begin
      // this frequency has a higher weight
      MaxWeight := CurWeight;
      MaxTone   := ToneIndex;
    end;
  end;

  ToneAbs := MaxTone;
  Tone    := MaxTone mod 12;
end;

// result medium difference
function TCaptureBuffer.AnalyzeAutocorrelationFreq(Freq: real): real;
var
  Dist:                   real;    // distance (0=equal .. 1=totally different) between correlated samples
  AccumDist:              real;    // accumulated distances
  SampleIndex:            integer; // index of sample to analyze
  CorrelatingSampleIndex: integer; // index of sample one period ahead
  SamplesPerPeriod:       integer; // samples in one period
begin
  SampleIndex := 0;
  SamplesPerPeriod := Round(AudioFormat.SampleRate/Freq);
  CorrelatingSampleIndex := SampleIndex + SamplesPerPeriod;

  AccumDist := 0;

  // compare correlating samples
  while (CorrelatingSampleIndex < AnalysisBufferSize) do
  begin
    // calc distance (correlation: 1-dist) to corresponding sample in next period
    Dist := Abs(BufferArray[SampleIndex] - BufferArray[CorrelatingSampleIndex]) /
            High(Word);
    AccumDist := AccumDist + Dist;
    Inc(SampleIndex);
    Inc(CorrelatingSampleIndex);
  end;

  // return "inverse" average distance (=correlation)
  Result := 1 - AccumDist / AnalysisBufferSize;
end;

function TCaptureBuffer.MaxSampleVolume: Single;
var
  lSampleIndex: Integer;
  lMaxVol : Longint;
begin;
  // FIXME: lock buffer to avoid race-conditions
  lMaxVol := 0;
  for lSampleIndex := 0 to High(BufferArray) do
  begin
    if Abs(BufferArray[lSampleIndex]) > lMaxVol then
      lMaxVol := Abs(BufferArray[lSampleIndex]);
  end;

  result := lMaxVol / -Low(Smallint);
end;

const
  ToneStrings: array[0..11] of string = (
    'C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'
  );

function TCaptureBuffer.GetToneString: string;
begin
  if (ToneValid) then
    Result := ToneStrings[Tone] + IntToStr(ToneAbs div 12 + 2)
  else
    Result := '-';
end;


{ TAudioInputProcessor }

constructor TAudioInputProcessor.Create;
var
  i:        integer;
begin
  inherited;
  SetLength(Sound, 6 {max players});//Ini.Players+1);
  for i := 0 to High(Sound) do
  begin
    Sound[i] := TCaptureBuffer.Create;
  end;
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
procedure TAudioInputProcessor.HandleMicrophoneData(Buffer: Pointer; Size: Cardinal; InputDevice: TAudioInputDevice);
var
  Value:        integer;
  ChannelBuffer: PChar;         // buffer handled as array of bytes (offset relative to channel)
  SampleBuffer: PSmallIntArray; // buffer handled as array of samples
  Boost:  byte;
// ChannelCount: integer; // Auto Removed, Unused Variable
  ChannelIndex: integer;
// ChannelOffset: integer; // Auto Removed, Unused Variable
  CaptureChannel: TCaptureBuffer;
  AudioFormat: TAudioFormatInfo;
  FrameSize: integer;
  NumSamples: integer;
  NumFrames: integer; // number of frames (stereo: 2xsamples)
  i: integer;
begin
  // set boost
  case Ini.MicBoost of
    0:   Boost := 1;
    1:   Boost := 2;
    2:   Boost := 4;
    3:   Boost := 8;
    else Boost := 1;
  end;

  AudioFormat := InputDevice.AudioFormat;

  // FIXME: At the moment we assume a SInt16 format
  // TODO:  use SDL_AudioConvert to convert to SInt16 but do NOT change the
  // samplerate (SDL does not convert 44.1kHz to 48kHz so we might get wrong
  // results in the analysis phase otherwise)
  if (AudioFormat.Format <> asfS16) then
  begin
    // this only occurs if a developer choosed a wrong input sample-format
    Log.CriticalError('TAudioInputProcessor.HandleMicrophoneData: Wrong sample-format');
    Exit;
  end;

  // interpret buffer as buffer of bytes
  SampleBuffer := Buffer;

  NumSamples := Size div SizeOf(Smallint);

  // boost buffer
  // TODO: remove this senseless stuff - adjust the threshold instead
  for i := 0 to NumSamples-1 do
  begin
    Value := SampleBuffer^[i] * Boost;

    // TODO :  JB -  This will clip the audio... cant we reduce the "Boost" if the data clips ??
    if Value > High(Smallint) then
      Value := High(Smallint);

    if Value < Low(Smallint) then
      Value := Low(Smallint);

    SampleBuffer^[i] := Value;
  end;

  // samples per channel
  FrameSize := AudioFormat.Channels * SizeOf(SmallInt);
  NumFrames := Size div FrameSize;

  // process channels
  for ChannelIndex := 0 to High(InputDevice.CaptureChannel) do
  begin
    CaptureChannel := InputDevice.CaptureChannel[ChannelIndex];
    if (CaptureChannel <> nil) then
    begin
      // set offset according to channel index
      ChannelBuffer := @PChar(Buffer)[ChannelIndex * SizeOf(SmallInt)];

      // TODO: remove BufferNew and write to BufferArray directly

      CaptureChannel.BufferNew.Clear;
      for i := 0 to NumFrames-1 do
      begin
        CaptureChannel.BufferNew.Write(ChannelBuffer[i*FrameSize], SizeOf(SmallInt));
      end;
      CaptureChannel.ProcessNewBuffer();
    end;
  end;
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

  // reset buffers
  for S := 0 to High(AudioInputProcessor.Sound) do
    AudioInputProcessor.Sound[S].Clear;

  // start capturing on each used device
  for DeviceIndex := 0 to High(AudioInputProcessor.Device) do
  begin
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
        Device.LinkCaptureBuffer(ChannelIndex, nil);
      end
      else
      begin
        Device.LinkCaptureBuffer(ChannelIndex, AudioInputProcessor.Sound[Player]);
        DeviceUsed := true;
      end;
    end;

    // start device if used
    if (DeviceUsed) then
    begin
      //Log.BenchmarkStart(2);
      Device.Start();
      //Log.BenchmarkEnd(2);
      //Log.LogBenchmark('Device.Start', 2) ;
    end;
  end;

  Started := true;
end;

{*
 * Stop input-capturing on all soundcards.
 *}
procedure TAudioInputBase.CaptureStop;
var
  DeviceIndex: integer;
// Player:  integer; // Auto Removed, Unused Variable
  Device: TAudioInputDevice;
// DeviceCfg: PInputDeviceConfig; // Auto Removed, Unused Variable
begin
  for DeviceIndex := 0 to High(AudioInputProcessor.Device) do
  begin
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
//var
// Descr: string; // Auto Removed, Unused Variable
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



