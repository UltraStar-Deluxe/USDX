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

unit URecord;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
  Math,
  sdl,
  SysUtils,
  UCommon,
  UMusic,
  UIni;

const
  BaseToneFreq = 65.4064; // lowest (half-)tone to analyze (C2 = 65.4064 Hz)
  NumHalftones = 36;      // C2-B4 (for Whitney and my high voice)

type
  TCaptureBuffer = class
    private
      VoiceStream: TAudioVoiceStream; // stream for voice passthrough
      AnalysisBufferLock: PSDL_Mutex;

      function GetToneString: string; // converts a tone to its string represenatation;

      procedure BoostBuffer(Buffer: PByteArray; Size: integer);
      procedure ProcessNewBuffer(Buffer: PByteArray; BufferSize: integer);

      // we call it to analyze sound by checking Autocorrelation
      procedure AnalyzeByAutocorrelation;
      // use this to check one frequency by Autocorrelation
      function AnalyzeAutocorrelationFreq(Freq: real): real;
    public
      AnalysisBuffer:  array[0..4095] of smallint; // newest 4096 samples
      AnalysisBufferSize: integer; // number of samples of BufferArray to analyze

      LogBuffer:   TMemoryStream;              // full buffer

      AudioFormat: TAudioFormatInfo;

      // pitch detection
      // TODO: remove ToneValid, set Tone/ToneAbs=-1 if invalid instead
      ToneValid:    boolean;    // true if Tone contains a valid value (otherwise it contains noise)
      Tone:         integer;    // tone relative to one octave (e.g. C2=C3=C4). Range: 0-11
      ToneAbs:      integer;    // absolute (full range) tone (e.g. C2<>C3). Range: 0..NumHalftones-1

      // methods
      constructor Create;
      destructor Destroy; override;

      procedure Clear;

      // use to analyze sound from buffers to get new pitch
      procedure AnalyzeBuffer;
      procedure LockAnalysisBuffer();   {$IFDEF HasInline}inline;{$ENDIF}
      procedure UnlockAnalysisBuffer(); {$IFDEF HasInline}inline;{$ENDIF}

      function MaxSampleVolume: single;
      property ToneString: string READ GetToneString;
  end;

const
  DEFAULT_SOURCE_NAME = '[Default]';

type
  TAudioInputSource = record
    Name: UTF8String;
  end;

  // soundcard input-devices information
  TAudioInputDevice = class
    public
      CfgIndex:      integer;   // index of this device in Ini.InputDeviceConfig
      Name:          UTF8String;    // soundcard name
      Source:        array of TAudioInputSource; // soundcard input-sources
      SourceRestore: integer;  // source-index that will be selected after capturing (-1: not detected)
      MicSource:     integer;  // source-index of mic (-1: none detected)

      AudioFormat:     TAudioFormatInfo; // capture format info (e.g. 44.1kHz SInt16 stereo)
      CaptureChannel:  array of TCaptureBuffer; // sound-buffer references used for mono or stereo channel's capture data

      destructor Destroy; override;

      procedure LinkCaptureBuffer(ChannelIndex: integer; Sound: TCaptureBuffer);

      // TODO: add Open/Close functions so Start/Stop becomes faster
      //function Open():    boolean; virtual; abstract;
      //function Close():   boolean; virtual; abstract;
      function Start():   boolean; virtual; abstract;
      function Stop():    boolean; virtual; abstract;

      function GetVolume(): single;        virtual; abstract;
      procedure SetVolume(Volume: single); virtual; abstract;
  end;

  TAudioInputProcessor = class
    public
      Sound:  array of TCaptureBuffer; // sound-buffers for every player
      DeviceList: array of TAudioInputDevice;

      constructor Create;
      destructor Destroy; override;

      procedure UpdateInputDeviceConfig;
      function ValidateSettings: boolean;

      // handle microphone input
      procedure HandleMicrophoneData(Buffer: PByteArray; Size: integer;
                                     InputDevice: TAudioInputDevice);
  end;

  TAudioInputBase = class( TInterfacedObject, IAudioInput )
    private
      Started: boolean;
    protected
      function UnifyDeviceName(const name: UTF8String; deviceIndex: integer): UTF8String;
    public
      function GetName: String;           virtual; abstract;
      function InitializeRecord: boolean; virtual; abstract;
      function FinalizeRecord: boolean;   virtual;

      procedure CaptureStart;
      procedure CaptureStop;
  end;

  TSmallIntArray = array [0..(MaxInt div SizeOf(SmallInt))-1] of SmallInt;
  PSmallIntArray = ^TSmallIntArray;

  function AudioInputProcessor(): TAudioInputProcessor;

implementation

uses
  ULog,
  UGraphic,
  ULanguage,
  UNote;

var
  singleton_AudioInputProcessor : TAudioInputProcessor = nil;

{ Global }

function AudioInputProcessor(): TAudioInputProcessor;
begin
  if singleton_AudioInputProcessor = nil then
    singleton_AudioInputProcessor := TAudioInputProcessor.create();

  result := singleton_AudioInputProcessor;
end;

{ TAudioInputDevice }

destructor TAudioInputDevice.Destroy;
begin
  Stop();
  Source := nil;
  CaptureChannel := nil;
  FreeAndNil(AudioFormat);
  inherited Destroy;
end;

procedure TAudioInputDevice.LinkCaptureBuffer(ChannelIndex: integer; Sound: TCaptureBuffer);
var
  DeviceCfg: PInputDeviceConfig;
  OldSound: TCaptureBuffer;
begin
  // check bounds
  if ((ChannelIndex < 0) or (ChannelIndex > High(CaptureChannel))) then
    Exit;

  // reset previously assigned (old) capture-buffer
  OldSound := CaptureChannel[ChannelIndex];
  if (OldSound <> nil) then
  begin
    // close voice stream
    FreeAndNil(OldSound.VoiceStream);
    // free old audio-format info
    FreeAndNil(OldSound.AudioFormat);
  end;

  // set audio-format of new capture-buffer
  if (Sound <> nil) then
  begin
    // copy the input-device audio-format ...
    Sound.AudioFormat := AudioFormat.Copy;
    // and adjust it because capture buffers are always mono
    Sound.AudioFormat.Channels := 1;
    DeviceCfg := @Ini.InputDeviceConfig[CfgIndex];

    if (Ini.VoicePassthrough = 1) then
    begin
      // TODO: map odd players to the left and even players to the right speaker
      Sound.VoiceStream := AudioPlayback.CreateVoiceStream(CHANNELMAP_FRONT, AudioFormat);
    end;
  end;

  // replace old with new buffer (Note: Sound might be nil)
  CaptureChannel[ChannelIndex] := Sound;
end;

{ TSound }

constructor TCaptureBuffer.Create;
begin
  inherited;
  LogBuffer := TMemoryStream.Create;
  AnalysisBufferLock := SDL_CreateMutex();
  AnalysisBufferSize := Length(AnalysisBuffer);
end;

destructor TCaptureBuffer.Destroy;
begin
  FreeAndNil(LogBuffer);
  FreeAndNil(VoiceStream);
  FreeAndNil(AudioFormat);
  SDL_DestroyMutex(AnalysisBufferLock);
  inherited;
end;

procedure TCaptureBuffer.LockAnalysisBuffer();
begin
  SDL_mutexP(AnalysisBufferLock);
end;

procedure TCaptureBuffer.UnlockAnalysisBuffer();
begin
  SDL_mutexV(AnalysisBufferLock);
end;

procedure TCaptureBuffer.Clear;
begin
  if assigned(LogBuffer) then
    LogBuffer.Clear;
  LockAnalysisBuffer();
  FillChar(AnalysisBuffer[0], Length(AnalysisBuffer) * SizeOf(SmallInt), 0);
  UnlockAnalysisBuffer();
end;

procedure TCaptureBuffer.ProcessNewBuffer(Buffer: PByteArray; BufferSize: integer);
var
  BufferOffset: integer;
  SampleCount:  integer;
  i:            integer;
begin
  // apply software boost
  BoostBuffer(Buffer, BufferSize);

  // voice passthrough (send data to playback-device)
  if (assigned(VoiceStream)) then
    VoiceStream.WriteData(Buffer, BufferSize);

  // we assume that samples are in S16Int format
  // TODO: support float too
  if (AudioFormat.Format <> asfS16) then
    Exit;

  // process BufferArray
  BufferOffset := 0;

  SampleCount := BufferSize div SizeOf(SmallInt);

  // check if we have more new samples than we can store
  if (SampleCount > Length(AnalysisBuffer)) then
  begin
    // discard the oldest of the new samples
    BufferOffset := (SampleCount - Length(AnalysisBuffer)) * SizeOf(SmallInt);
    SampleCount := Length(AnalysisBuffer);
  end;

  LockAnalysisBuffer();
  try

    // move old samples to the beginning of the array (if necessary)
    for i := 0 to High(AnalysisBuffer)-SampleCount do
      AnalysisBuffer[i] := AnalysisBuffer[i+SampleCount];

    // copy new samples to analysis buffer
    Move(Buffer[BufferOffset], AnalysisBuffer[Length(AnalysisBuffer)-SampleCount],
         SampleCount * SizeOf(SmallInt));

  finally
    UnlockAnalysisBuffer();
  end;

  // save capture-data to BufferLong if enabled
  if (Ini.SavePlayback = 1) then
  begin
    // this is just for debugging (approx 15MB per player for a 3min song!!!)
    // For an in-game replay-mode we need to compress data so we do not
    // waste that much memory. Maybe ogg-vorbis with voice-preset in fast-mode?
    // Or we could use a faster but not that efficient lossless compression.
    LogBuffer.WriteBuffer(Buffer, BufferSize);
  end;
end;

procedure TCaptureBuffer.AnalyzeBuffer;
var
  Volume:      single;
  MaxVolume:   single;
  SampleIndex: integer;
  Threshold:   single;
begin
  ToneValid := false;
  ToneAbs := -1;
  Tone    := -1;

  LockAnalysisBuffer();
  try

    // find maximum volume of first 1024 samples
    MaxVolume := 0;
    for SampleIndex := 0 to 1023 do
    begin
      Volume := Abs(AnalysisBuffer[SampleIndex]) / -Low(Smallint);
      if Volume > MaxVolume then
         MaxVolume := Volume;
    end;

    Threshold := IThresholdVals[Ini.ThresholdIndex];

    // check if signal has an acceptable volume (ignore background-noise)
    if MaxVolume >= Threshold then
    begin
      // analyse the current voice pitch
      AnalyzeByAutocorrelation;
      ToneValid := true;
    end;

  finally
    UnlockAnalysisBuffer();
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
  MaxTone := 0; // this is not needed, but it satifies the compiler

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
    Dist := Abs(AnalysisBuffer[SampleIndex] - AnalysisBuffer[CorrelatingSampleIndex]) /
            High(Word);
    AccumDist := AccumDist + Dist;
    Inc(SampleIndex);
    Inc(CorrelatingSampleIndex);
  end;

  // return "inverse" average distance (=correlation)
  Result := 1 - AccumDist / AnalysisBufferSize;
end;

function TCaptureBuffer.MaxSampleVolume: single;
var
  lSampleIndex: integer;
  lMaxVol:      longint;
begin;
  LockAnalysisBuffer();
  try
    lMaxVol := 0;
    for lSampleIndex := 0 to High(AnalysisBuffer) do
    begin
      if Abs(AnalysisBuffer[lSampleIndex]) > lMaxVol then
        lMaxVol := Abs(AnalysisBuffer[lSampleIndex]);
    end;
  finally
    UnlockAnalysisBuffer();
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

procedure TCaptureBuffer.BoostBuffer(Buffer: PByteArray; Size: integer);
var
  i:            integer;
  Value:        longint;
  SampleCount:  integer;
  SampleBuffer: PSmallIntArray; // buffer handled as array of samples
  Boost:        byte;
begin
  // TODO: set boost per device
  case Ini.MicBoost of
    0:   Boost := 1;
    1:   Boost := 2;
    2:   Boost := 4;
    3:   Boost := 8;
    else Boost := 1;
  end;

  // at the moment we will boost SInt16 data only
  if (AudioFormat.Format = asfS16) then
  begin
    // interpret buffer as buffer of bytes
    SampleBuffer := PSmallIntArray(Buffer);
    SampleCount := Size div AudioFormat.FrameSize;

    // boost buffer
    for i := 0 to SampleCount-1 do
    begin
      Value := SampleBuffer^[i] * Boost;

      if Value > High(Smallint) then
        Value := High(Smallint);

      if Value < Low(Smallint) then
        Value := Low(Smallint);

      SampleBuffer^[i] := Value;
    end;
  end;
end;

{ TAudioInputProcessor }

constructor TAudioInputProcessor.Create;
var
  i: integer;
begin
  inherited;
  SetLength(Sound, 6 {max players});//Ini.Players+1);
  for i := 0 to High(Sound) do
    Sound[i] := TCaptureBuffer.Create;
end;

destructor TAudioInputProcessor.Destroy;
var
  i: integer;
begin
  for i := 0 to High(Sound) do
    Sound[i].Free;
  SetLength(Sound, 0);
  inherited;
end;

// updates InputDeviceConfig with current input-device information
// See: TIni.LoadInputDeviceCfg()
procedure TAudioInputProcessor.UpdateInputDeviceConfig;
var
  deviceIndex:    integer;
  newDevice:      boolean;
  deviceIniIndex: integer;
  deviceCfg:      PInputDeviceConfig;
  device:         TAudioInputDevice;
  channelCount:   integer;
  channelIndex:   integer;
  i:              integer;
begin
  // Input devices - append detected soundcards
  for deviceIndex := 0 to High(DeviceList) do
  begin
    newDevice := true;
    //Search for Card in List
    for deviceIniIndex := 0 to High(Ini.InputDeviceConfig) do
    begin
      deviceCfg := @Ini.InputDeviceConfig[deviceIniIndex];
      device := DeviceList[deviceIndex];

      if (deviceCfg.Name = Trim(device.Name)) then
      begin
        newDevice := false;

        // store highest channel index as an offset for the new channels
        channelIndex := High(deviceCfg.ChannelToPlayerMap);
        // add missing channels or remove non-existing ones
        SetLength(deviceCfg.ChannelToPlayerMap, device.AudioFormat.Channels);
        // initialize added channels to 0
        for i := channelIndex+1 to High(deviceCfg.ChannelToPlayerMap) do
        begin
          deviceCfg.ChannelToPlayerMap[i] := 0;
        end;

        // associate ini-index with device
        device.CfgIndex := deviceIniIndex;
        break;
      end;
    end;

    //If not in List -> Add
    if newDevice then
    begin
      // resize list
      SetLength(Ini.InputDeviceConfig, Length(Ini.InputDeviceConfig)+1);
      deviceCfg := @Ini.InputDeviceConfig[High(Ini.InputDeviceConfig)];
      device := DeviceList[deviceIndex];

      // associate ini-index with device
      device.CfgIndex := High(Ini.InputDeviceConfig);

      deviceCfg.Name := Trim(device.Name);
      deviceCfg.Input := 0;
      deviceCfg.Latency := LATENCY_AUTODETECT;

      channelCount := device.AudioFormat.Channels;
      SetLength(deviceCfg.ChannelToPlayerMap, channelCount);

      for channelIndex := 0 to channelCount-1 do
      begin
        // set default at first start of USDX (1st device, 1st channel -> player1)
        if ((channelIndex = 0) and (device.CfgIndex = 0)) then
          deviceCfg.ChannelToPlayerMap[0] := 1
        else
          deviceCfg.ChannelToPlayerMap[channelIndex] := 0;
      end;
    end;
  end;
end;

function TAudioInputProcessor.ValidateSettings: boolean;
const
  MAX_PLAYER_COUNT = 6; // FIXME: there should be a global variable for this
var
  I, J: integer;
  PlayerID: integer;
  PlayerMap: array [0 .. MAX_PLAYER_COUNT] of boolean;
  InputDevice: TAudioInputDevice;
  InputDeviceCfg: PInputDeviceConfig;
begin
  // mark all players as unassigned
  for I := 0 to High(PlayerMap) do
    PlayerMap[I] := false;

  // iterate over all active devices
  for I := 0 to High(DeviceList) do
  begin
    InputDevice := DeviceList[I];
    InputDeviceCfg := @Ini.InputDeviceConfig[InputDevice.CfgIndex];
    // iterate over all channels of the current devices
    for J := 0 to High(InputDeviceCfg.ChannelToPlayerMap) do
    begin
      // get player that was mapped to the current device channel
      PlayerID := InputDeviceCfg.ChannelToPlayerMap[J];
      if (PlayerID <> 0) then
      begin
        // check if player is already assigned to another device/channel
        if (PlayerMap[PlayerID]) then
        begin
          ScreenPopupError.ShowPopup(
              Format(Language.Translate('ERROR_PLAYER_DEVICE_ASSIGNMENT'),
              [PlayerID]));
          Result := false;
          Exit;
        end;

        // mark player as assigned to a device
        PlayerMap[PlayerID] := true;
      end;
    end;
  end;
  Result := true;
end;

{*
 * Handles captured microphone input data.
 * Params:
 *   Buffer - buffer of signed 16bit interleaved stereo PCM-samples.
 *     Interleaved means that a right-channel sample follows a left-
 *     channel sample and vice versa (0:left[0],1:right[0],2:left[1],...).
 *   Length - number of bytes in Buffer
 *   Input - Soundcard-Input used for capture
 *}
procedure TAudioInputProcessor.HandleMicrophoneData(Buffer: PByteArray; Size: integer; InputDevice: TAudioInputDevice);
var
  MultiChannelBuffer:      PByteArray;  // buffer handled as array of bytes (offset relative to channel)
  SingleChannelBuffer:     PByteArray;  // temporary buffer for new samples per channel
  SingleChannelBufferSize: integer;
  ChannelIndex:            integer;
  CaptureChannel:          TCaptureBuffer;
  AudioFormat:             TAudioFormatInfo;
  SampleSize:              integer;
  SamplesPerChannel:       integer;
  i:                       integer;
begin
  AudioFormat := InputDevice.AudioFormat;
  SampleSize := AudioSampleSize[AudioFormat.Format];
  SamplesPerChannel := Size div AudioFormat.FrameSize;

  SingleChannelBufferSize := SamplesPerChannel * SampleSize;
  GetMem(SingleChannelBuffer, SingleChannelBufferSize);

  // process channels
  for ChannelIndex := 0 to High(InputDevice.CaptureChannel) do
  begin
    CaptureChannel := InputDevice.CaptureChannel[ChannelIndex];
    // check if a capture buffer was assigned, otherwise there is nothing to do
    if (CaptureChannel <> nil) then
    begin
      // set offset according to channel index
      MultiChannelBuffer := @Buffer[ChannelIndex * SampleSize];
      // separate channel-data from interleaved multi-channel (e.g. stereo) data
      for i := 0 to SamplesPerChannel-1 do
      begin
        Move(MultiChannelBuffer[i*AudioFormat.FrameSize],
             SingleChannelBuffer[i*SampleSize],
             SampleSize);
      end;
      CaptureChannel.ProcessNewBuffer(SingleChannelBuffer, SingleChannelBufferSize);
    end;
  end;

  FreeMem(SingleChannelBuffer);
end;

{ TAudioInputBase }

function TAudioInputBase.FinalizeRecord: boolean;
var
  i: integer;
begin
  for i := 0 to High(AudioInputProcessor.DeviceList) do
    AudioInputProcessor.DeviceList[i].Free();
  AudioInputProcessor.DeviceList := nil;
  Result := true;
end;

{*
 * Start capturing on all used input-device.
 *}
procedure TAudioInputBase.CaptureStart;
var
  S:            integer;
  DeviceIndex:  integer;
  ChannelIndex: integer;
  Device:       TAudioInputDevice;
  DeviceCfg:    PInputDeviceConfig;
  DeviceUsed:   boolean;
  Player:       integer;
begin
  if (Started) then
    CaptureStop();

  // reset buffers
  for S := 0 to High(AudioInputProcessor.Sound) do
    AudioInputProcessor.Sound[S].Clear;

  // start capturing on each used device
  for DeviceIndex := 0 to High(AudioInputProcessor.DeviceList) do
  begin
    Device := AudioInputProcessor.DeviceList[DeviceIndex];
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
  DeviceIndex:  integer;
  ChannelIndex: integer;
  Device:       TAudioInputDevice;
  DeviceCfg:    PInputDeviceConfig;
begin
  for DeviceIndex := 0 to High(AudioInputProcessor.DeviceList) do
  begin
    Device := AudioInputProcessor.DeviceList[DeviceIndex];
    if not assigned(Device) then
      continue;

    Device.Stop();

    // disconnect capture buffers
    DeviceCfg := @Ini.InputDeviceConfig[Device.CfgIndex];
    for ChannelIndex := 0 to High(DeviceCfg.ChannelToPlayerMap) do
      Device.LinkCaptureBuffer(ChannelIndex, nil);
  end;

  Started := false;
end;

function TAudioInputBase.UnifyDeviceName(const name: UTF8String; deviceIndex: integer): UTF8String;
var
  count: integer; // count of devices with this name

  function IsDuplicate(const name: UTF8String): boolean;
  var
    i: integer;
  begin
    Result := false;
    // search devices with same description
    for i := 0 to deviceIndex-1 do
    begin
      if (AudioInputProcessor.DeviceList[i] <> nil) then
      begin
        if (AudioInputProcessor.DeviceList[i].Name = name) then
        begin
          Result := true;
          Break;
        end;
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

end.
