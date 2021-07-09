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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/base/URecord.pas $
 * $Id: URecord.pas 2814 2011-04-06 23:31:15Z k-m_schindler $
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
  sdl2,
  SysUtils,
  UCommon,
  UMusic,
  UIni;

const
  BaseToneFreq = 440; // A4 concert pitch of 440 Hz
  NumHalftones = 49;  // C2 to C6 (1046.5023 Hz)

type
  // pitch detection algorithm (PDA)
  TPDAType = (
    PDA_AMDF,   // average magnitude difference function
    PDA_CAMDF  // circular average magnitude difference function
  );

  TFrequencyArray   = array[0..NumHalftones-1] of real;
  TDelayArray       = array[0..NumHalftones-1] of integer;
  TCorrelationArray = array[0..NumHalftones-1] of real;

  TCaptureBuffer = class
    private
      fVoiceStream: TAudioVoiceStream; // stream for voice passthrough
      fAnalysisBufferLock: PSDL_Mutex;
      fAudioFormat: TAudioFormatInfo;
      Frequencies: TFrequencyArray;
      Delays: TDelayArray;

      function GetToneString: string; // converts a tone to its string representation;

      procedure BoostBuffer(Buffer: PByteArray; Size: integer);
      procedure ProcessNewBuffer(Buffer: PByteArray; BufferSize: integer);

      procedure StartCapture(Format: TAudioFormatInfo);
      procedure StopCapture();

      procedure SetFrequenciesAndDelays;
      // we call it to analyze sound by checking AMDF
      function AnalyzePitch(PDA: TPDAType): boolean;
      // use this to check one frequency by AMDF
      function AverageMagnitudeDifference(): TCorrelationArray;
      function CircularAverageMagnitudeDifference(): TCorrelationArray;
      function ArrayIndexOfMinimum(const AValues: array of real): Integer;
    public
      AnalysisBuffer:  array[0..4095] of smallint; // newest 4096 samples (MUST BE POWER OF TWO!)
      AnalysisBufferSize: integer; // number of samples of BufferArray to analyze

      LogBuffer:   TMemoryStream;              // full buffer

      // pitch detection
      // TODO: remove ToneValid, set Tone/ToneAbs=-1 if invalid instead
      ToneValid: boolean;    // true if Tone contains a valid value (otherwise it contains noise)
      Tone:      integer;    // tone relative to one octave (e.g. C2=C3=C4). Range: 0-11
      ToneAbs:   integer;    // absolute (full range) tone (e.g. C2<>C3). Range: 0..NumHalftones-1

      // methods
      constructor Create;
      destructor Destroy; override;

      procedure Clear;

      // use to analyze sound from buffers to get new pitch
      procedure AnalyzeBuffer;
      // Beat detection: short burst of sound above background noise level, within a short time window back from the
      // current time point (timeBack parametr, unit seconds, depends on the current time point)

      procedure AnalyzeBufferBeatOnly(timeBack: real; Threshold: integer; RiseRate: integer;
                MinPeakDuration: integer; DropAfterPeak: integer; TestTimeAfterPeak: integer);
      procedure LockAnalysisBuffer();   {$IFDEF HasInline}inline;{$ENDIF}
      procedure UnlockAnalysisBuffer(); {$IFDEF HasInline}inline;{$ENDIF}

      function MaxSampleVolume: single;
      property ToneString: string READ GetToneString;
      property AudioFormat: TAudioFormatInfo READ fAudioFormat;
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

  TBooleanDynArray = array of boolean;

  TAudioInputProcessor = class
    public
      Sound:  array of TCaptureBuffer; // sound-buffers for every player
      DeviceList: array of TAudioInputDevice;

      constructor Create;
      destructor Destroy; override;

      procedure UpdateInputDeviceConfig;

      {**
       * Validates the mic settings.
       * If a player was assigned to multiple mics a popup will be displayed
       * with the ID of the player.
       * The return value is the player number of the first player that is not
       * configured correctly or 0 if all players are correct.
       *}
      function ValidateSettings: integer;

      {**
       * Checks if players 1 to PlayerCount are configured correctly.
       * A player is configured if a device's channel is assigned to him.
       * For each player (up to PlayerCount) the state will be in PlayerState.
       * If a player's state is true the player is configured, otherwise not.
       * The return value is the player number of the first player that is not
       * configured correctly or 0 if all players are correct.
       * The PlayerState array is zero based (index 0 for player 1).
       *}
      function CheckPlayersConfig(PlayerCount: cardinal;
          var PlayerState: TBooleanDynArray): integer; overload;

      {**
       * Same as the array version but it does not output a state for each player.
       *}
      function CheckPlayersConfig(PlayerCount: cardinal): integer; overload;

      {**
       * Handle microphone input
       *}
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
  OldSound: TCaptureBuffer;
begin
  // check bounds
  if ((ChannelIndex < 0) or (ChannelIndex > High(CaptureChannel))) then
    Exit;

  // reset previously assigned (old) capture-buffer
  OldSound := CaptureChannel[ChannelIndex];
  if (OldSound <> nil) then
  begin
    OldSound.StopCapture();
  end;

  // set audio-format of new capture-buffer
  if (Sound <> nil) then
  begin
    Sound.StartCapture(AudioFormat);
  end;

  // replace old with new buffer (Note: Sound might be nil)
  CaptureChannel[ChannelIndex] := Sound;
end;

{ TSound }

constructor TCaptureBuffer.Create;
begin
  inherited;
  LogBuffer := TMemoryStream.Create;
  fAnalysisBufferLock := SDL_CreateMutex();
  AnalysisBufferSize := Length(AnalysisBuffer);
end;

destructor TCaptureBuffer.Destroy;
begin
  FreeAndNil(LogBuffer);
  FreeAndNil(fVoiceStream);
  FreeAndNil(fAudioFormat);
  SDL_UnlockMutex(fAnalysisBufferLock);
  SDL_DestroyMutex(fAnalysisBufferLock);
  fAnalysisBufferLock:=nil;
  inherited;
end;

procedure TCaptureBuffer.LockAnalysisBuffer();
begin
  SDL_LockMutex(fAnalysisBufferLock);
end;

procedure TCaptureBuffer.UnlockAnalysisBuffer();
begin
  SDL_UnlockMutex(fAnalysisBufferLock);
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
  if (assigned(fVoiceStream)) then
    fVoiceStream.WriteData(Buffer, BufferSize);

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
    LogBuffer.Write(Buffer^, BufferSize);
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
      //ToneValid := AnalyzePitch(PDA_AMDF);
      //Write('AMDF: ',ToneAbs:2,' (f: ',Frequencies[ToneAbs]:7:2,' Hz)');
      //Write(Tone:2);
      ToneValid := AnalyzePitch(PDA_CAMDF);
      //Writeln('   /   CAMDF: ',ToneAbs:2,' (f: ',Frequencies[ToneAbs]:7:2,' Hz)');
      //Writeln(' ',Tone:2);
    end;

  finally
    UnlockAnalysisBuffer();
  end;
end;



procedure TCaptureBuffer.AnalyzeBufferBeatOnly(timeBack: real; Threshold: integer; RiseRate: integer; MinPeakDuration: integer;
                               DropAfterPeak: integer; TestTimeAfterPeak: integer);
var
  Volume:      single;
  MaxVolume:   single;
  MeanVolume:  single;
  SampleIndex, SampleInterval, SampleIndexPeak, StartSampleIndex: integer;
  BaselineStart, BaselineInterval, BaselineSampleIndex: integer; // To compare the peak sample values to baseline
  SampleLowerLimit, SampleUpperLimit: integer;
  detected, maximumdetected:    Boolean;
  RMSVolume, RMSVolumeBaseline, riserate_evaluated:   single;
  // Four potential criteria for a succesful beat note detetion (this is to discriminate against background noise)
  passesThreshold: Boolean; // 1) Passing an absolute sound intensity threshold (anyways mandatory)
  passesRiseRate: Boolean; // 2) Sufficiently quick rise rate (depending on user configuration in Ini variable)
  passesDuration: Boolean; // 3) Sufficient duration (not just single off measurement, depends on configuration in Ini variable)
  passesDropAfterPeak: Boolean; // 4) Sufficient quick fall after peak (depending on user configuration in Ini variable)

begin



  ToneValid := false;
  ToneAbs := -1;
  Tone    := -1;
  detected := false;


  passesThreshold:=false;
  passesRiseRate:=false;
  passesDuration:=false;
  passesDropAfterPeak:=false;

  if RiseRate = 0 then
     passesRiseRate := true; // No rise rate requirement, so passes anyways

  if MinPeakDuration = 0 then
     passesDuration := true; // No minimal duration, so test passed anyways

  if DropAfterPeak =0 then
     passesDropAfterPeak:= true;

  LockAnalysisBuffer();
  try

  StartSampleIndex:=High(AnalysisBuffer)-Round(timeBack*AudioFormat.SampleRate);

    if(StartSampleIndex < 0) then
      StartSampleIndex := 0;

  for SampleIndex := StartSampleIndex to High(AnalysisBuffer) do
  begin

       passesThreshold:=false;
       passesRiseRate:=false;
       passesDuration:=false;
       passesDropAfterPeak:=false;

       if RiseRate = 0 then
          passesRiseRate := true; // No rise rate requirement, so passes anyways

       if MinPeakDuration = 0 then
          passesDuration := true; // No minimal duration, so test passed anyways

       if DropAfterPeak =0 then
          passesDropAfterPeak:= true;

      Volume := Abs(AnalysisBuffer[SampleIndex]) / (-Low(smallint)) *100;
      if  Volume > Threshold then
      begin
        passesThreshold:=true;
      end;

      // Before going further, check whether by any chance we already pass all criteria
      if passesThreshold and passesRiseRate and passesDuration and passesDropAfterPeak then
      begin
           detected:=true;
           Break;
      end;

      // First test passed, check for rise rate if necessary
      if passesThreshold and (not passesRiseRate) then begin
         BaselineStart:=SampleIndex-Round(0.005*AudioFormat.SampleRate);
         if BaselineStart >= Low(AnalysisBuffer) then
           begin
              BaselineInterval:=Round(0.004*AudioFormat.SampleRate);
              RMSVolumeBaseline:=0;
              for BaselineSampleIndex := BaselineStart to BaselineStart+BaselineInterval do
              begin
                  RMSVolumeBaseline :=
                     RMSVolumeBaseline+(AnalysisBuffer[BaselineSampleIndex])*(AnalysisBuffer[BaselineSampleIndex])/(-Low(smallint))/(-Low(smallint));
              end;
              RMSVolumeBaseline:=Sqrt(RMSVolumeBaseline/(BaselineInterval+1));
              BaselineStart:=SampleIndex;
              BaselineInterval:=Round(0.003*AudioFormat.SampleRate);
              if BaselineStart+BaselineInterval <= High(AnalysisBuffer) then
              begin

                RMSVolume:=0;
                for BaselineSampleIndex := BaselineStart to BaselineStart+BaselineInterval do
                begin
                  RMSVolume :=
                     RMSVolume+(AnalysisBuffer[BaselineSampleIndex])*(AnalysisBuffer[BaselineSampleIndex])/(-Low(smallint))/(-Low(smallint));
                end;
                  RMSVolume:=Sqrt(RMSVolume/(BaselineInterval+1));



                 riserate_evaluated:=(RMSVolume-RMSVolumeBaseline)*100;
                 // The idea is that we want to have quick rise but then also something that stays a bit constant or continues to rise
                 if (riserate_evaluated>=RiseRate) and (RMSVolume*100.0 > Volume/2.0) then
                 begin
                   passesRiseRate:=true;

                end;

              end; // End we can get the peak RMS
           end;

      end;

      // Again, check whether by any chance we already pass all criteria
      if passesThreshold and passesRiseRate and passesDuration and passesDropAfterPeak then
      begin
           detected:=true;
           Break;
      end;

      // First two tests OK, but not (yet) the third one
      if passesThreshold and passesRiseRate and (not passesDuration) then
      begin
         SampleUpperLimit:=SampleIndex+Round(0.001*AudioFormat.SampleRate*MinPeakDuration*2);
         if SampleUpperLimit > High(AnalysisBuffer) then
             SampleUpperLimit := High(AnalysisBuffer);
         SampleLowerLimit:=SampleIndex+Round(0.001*AudioFormat.SampleRate*MinPeakDuration);
         if SampleLowerLimit > High(AnalysisBuffer) then
            SampleLowerLimit := High(AnalysisBuffer);
         maximumdetected:=false;
         for BaselineSampleIndex := SampleLowerLimit to SampleUpperLimit do
         begin
           if Abs(AnalysisBuffer[BaselineSampleIndex]) / (-Low(smallint)) *100 > Threshold then
           begin
                maximumdetected:=true;
                Break;
           end;
         end;
         if maximumdetected then begin
            passesDuration:=true;
         end;

      end;

      // Again, check whether by any chance we already pass all criteria
      if passesThreshold and passesRiseRate and passesDuration and passesDropAfterPeak then
      begin
           detected:=true;
           Break;
      end;

      //
      if passesThreshold and passesRiseRate and passesDuration and (not passesDropAfterPeak) then
      begin


          BaselineStart:=SampleIndex;
          BaselineInterval:=Round(0.003*AudioFormat.SampleRate);
          if BaselineStart+BaselineInterval <= High(AnalysisBuffer) then
           begin

              RMSVolumeBaseline:=0;
              for BaselineSampleIndex := BaselineStart to BaselineStart+BaselineInterval do
              begin
                  RMSVolumeBaseline :=
                     RMSVolumeBaseline+(AnalysisBuffer[BaselineSampleIndex])*(AnalysisBuffer[BaselineSampleIndex])/(-Low(smallint))/(-Low(smallint));
              end;
              RMSVolumeBaseline:=Sqrt(RMSVolumeBaseline/(BaselineInterval+1));
              SampleUpperLimit:=SampleIndex+Round((TestTimeAfterPeak/1000.0+0.005)*AudioFormat.SampleRate);
              SampleLowerLimit:=SampleIndex+Round(TestTimeAfterPeak/1000.0*AudioFormat.SampleRate);
              // Avoid indexing error by accessing non-existent points
              if SampleUpperLimit <= High(AnalysisBuffer) then
              begin
                 RMSVolume:=0;
                 for BaselineSampleIndex := SampleLowerLimit to SampleUpperLimit do
                 begin
                      RMSVolume:=RMSVolume+(AnalysisBuffer[BaselineSampleIndex])*(AnalysisBuffer[BaselineSampleIndex])/(-Low(smallint))/(-Low(smallint));
                 end;
                 RMSVolume:=Sqrt(RMSVolume/(SampleUpperLimit-SampleLowerLimit+1));


                 // Fall rate is relative to max peak intensity (here, RMSVolumeBaseline taken on 1ms peak from initial detection on)
                 if ((RMSVolumeBaseline - RMSVolume)/RMSVolumeBaseline)*100.0 >= DropAfterPeak then
                 begin
                   passesDropAfterPeak:=true;
                 end;
              end;



           end;



      end;

      // Final test if everything passes
      if passesThreshold and passesRiseRate and passesDuration and passesDropAfterPeak then
      begin
           detected:=true;
           Break;
      end;




  end;







  if detected then
    begin
       ToneValid := true;
       ToneAbs:=48;
       Tone:=0;



    end;

  finally
    UnlockAnalysisBuffer();
  end;

end;



function TCaptureBuffer.ArrayIndexOfMinimum(const AValues: array of real): Integer;
var
  LValIdx: Integer;
  LMinIdx: Integer = 0;
begin
  if Length(AValues) = 0 then Exit(-1);

  for LValIdx := 1 to High(AValues) do
    if AValues[LValIdx] <= AValues[LMinIdx] then
      LMinIdx := LValIdx;

  Result := LMinIdx;
end;

procedure TCaptureBuffer.SetFrequenciesAndDelays;
var
  ToneIndex:   integer;
begin
  for ToneIndex := 0 to NumHalftones-1 do
  begin
    // Freq(ToneIndex) = 440 Hz * 2^((ToneIndex-33)/12) --> Freq(ToneIndex=0) = 65.4064 Hz = C2
    Frequencies[ToneIndex] := BaseToneFreq * Power(2, (ToneIndex-33)/12);
    Delays[ToneIndex] := Round(AudioFormat.SampleRate/Frequencies[ToneIndex]);
  end;
end;

function TCaptureBuffer.AnalyzePitch(PDA: TPDAType): boolean;
var
  Correlation: TCorrelationArray;
begin
  // prepare to analyze
  SetFrequenciesAndDelays;

  // analyze halftones
  // Note: at the lowest tone (~65 Hz) and a buffer-size of 4096
  // at 44.1 (or 48 kHz) only 6 (or 5) samples are compared, this might be
  // too few samples -> use a bigger buffer-size
  case PDA of
    PDA_AMDF:
      begin
        Correlation := AverageMagnitudeDifference();
        ToneAbs := ArrayIndexOfMinimum(Correlation);
      end;
    PDA_CAMDF:
      begin
        Correlation := CircularAverageMagnitudeDifference();
        ToneAbs := ArrayIndexOfMinimum(Correlation);
      end;
  end;

  Tone := ToneAbs mod 12;
  Result := true;
end;

// Average Magnitude Difference Function (AMDF) is defined as
//   D(\tau)=\frac{1}{N-\tau-1}\sum_{n=0}^{N-\tau-1}|x(n) - x(n+\tau)|
// where \tau = Delay, n = SampleIndex, N = AnalysisBufferSize, x = AnalysisBuffer
// See: Equation (1) in http://www.utdallas.edu/~hxb076000/citing_papers/Muhammad%20Extended%20Average%20Magnitude%20Difference.pdf
function TCaptureBuffer.AverageMagnitudeDifference(): TCorrelationArray;
var
  ToneIndex:   integer;
  Correlation: TCorrelationArray;
  SampleIndex: integer; // index of sample to analyze
begin
  // accumulate the magnitude differences for samples in AnalysisBuffer
  for ToneIndex := 0 to NumHalftones-1 do
    begin
      Correlation[ToneIndex] := 0;
      for SampleIndex := 0 to (AnalysisBufferSize-Delays[ToneIndex]-1) do
      begin
        Correlation[ToneIndex] := Correlation[ToneIndex] + Abs(AnalysisBuffer[SampleIndex] - AnalysisBuffer[SampleIndex+Delays[ToneIndex]]);
      end;
      Correlation[ToneIndex] := Correlation[ToneIndex] / (AnalysisBufferSize-Delays[ToneIndex]-1);
    end;

  // return average magnitude difference
  Result := Correlation;
end;


// Circular Average Magnitude Difference Function (CAMDF) is defined as
//   D_C(\tau)=\sum_{n=0}^{N-1}|x(mod(n+\tau, N)) - x(n)|
// where \tau = Delay, n = SampleIndex, N = AnalysisBufferSize, x = AnalysisBuffer
// See: Equation (4) in http://www.utdallas.edu/~hxb076000/citing_papers/Muhammad%20Extended%20Average%20Magnitude%20Difference.pdf
function TCaptureBuffer.CircularAverageMagnitudeDifference(): TCorrelationArray;
var
  ToneIndex:   integer;
  Correlation: TCorrelationArray;
  SampleIndex: integer; // index of sample to analyze
begin
  // accumulate the magnitude differences for samples in AnalysisBuffer
  for ToneIndex := 0 to NumHalftones-1 do
  begin
    Correlation[ToneIndex] := 0;
    for SampleIndex := 0 to (AnalysisBufferSize-1) do
    begin
      // Suggestion for calculation efficiency improvement from deuteragenie:
      // Replacing 'i mod buffersize' by 'i & (buffersize-1)' when i is positive and buffersize is a power of two should speed the modulo compuation by 5x-10x
      //Correlation[ToneIndex] += Abs(AnalysisBuffer[(SampleIndex+Delays[ToneIndex]) mod AnalysisBufferSize] - AnalysisBuffer[SampleIndex]);
      Correlation[ToneIndex] := Correlation[ToneIndex] + Abs(AnalysisBuffer[(SampleIndex+Delays[ToneIndex]) and (AnalysisBufferSize-1)] - AnalysisBuffer[SampleIndex]);
    end;
    Correlation[ToneIndex] := Correlation[ToneIndex] / AnalysisBufferSize;
  end;

  // return circular average magnitude difference
  Result := Correlation;
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

procedure TCaptureBuffer.StartCapture(Format: TAudioFormatInfo);
begin
  // free old audio-format info
  FreeAndNil(fAudioFormat);
  // copy the new input-device audio-format ...
  fAudioFormat := Format.Copy;
  // and adjust it because capture buffers are always mono
  fAudioFormat.Channels := 1;

  if (Ini.VoicePassthrough = 1) then
  begin
    // TODO: map odd players to the left and even players to the right speaker
    fVoiceStream := AudioPlayback.CreateVoiceStream(CHANNELMAP_FRONT, fAudioFormat);
  end;
end;

procedure TCaptureBuffer.StopCapture();
begin
  FreeAndNil(fVoiceStream);
end;

{ TAudioInputProcessor }

constructor TAudioInputProcessor.Create;
var
  i: integer;
begin
  inherited;
  SetLength(Sound, UIni.IMaxPlayerCount);
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
        // assign added channels to no player
        for i := channelIndex+1 to High(deviceCfg.ChannelToPlayerMap) do
        begin
          deviceCfg.ChannelToPlayerMap[i] := CHANNEL_OFF;
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
        // Do not set any default on first start of USDX.
        // Otherwise most probably the wrong device (internal sound card)
        // will be selected.
        // It is better to force the user to configure the mics himself.
        deviceCfg.ChannelToPlayerMap[channelIndex] := CHANNEL_OFF;
      end;
    end;
  end;
end;

function TAudioInputProcessor.ValidateSettings: integer;
var
  I, J: integer;
  PlayerID: integer;
  PlayerMap: array [0 .. UIni.IMaxPlayerCount - 1] of boolean;
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
      if (PlayerID <> CHANNEL_OFF) then
      begin
        // check if player is already assigned to another device/channel
        if (PlayerMap[PlayerID - 1]) then
        begin
          Result := PlayerID;
          Exit;
        end;

        // mark player as assigned to a device
        PlayerMap[PlayerID - 1] := true;
      end;
    end;
  end;
  Result := 0;
end;

function TAudioInputProcessor.CheckPlayersConfig(PlayerCount: cardinal;
  var PlayerState: TBooleanDynArray): integer;
var
  DeviceIndex:  integer;
  ChannelIndex: integer;
  Device:       TAudioInputDevice;
  DeviceCfg:    PInputDeviceConfig;
  PlayerIndex:  integer;
  I: integer;
begin
  SetLength(PlayerState, PlayerCount);
  // set all entries to "not configured"
  for I := 0 to High(PlayerState) do
  begin
    PlayerState[I] := false;
  end;

  // check each used device
  for DeviceIndex := 0 to High(AudioInputProcessor.DeviceList) do
  begin
    Device := AudioInputProcessor.DeviceList[DeviceIndex];
    if not assigned(Device) then
      continue;
    DeviceCfg := @Ini.InputDeviceConfig[Device.CfgIndex];

    // check if device is used
    for ChannelIndex := 0 to High(DeviceCfg.ChannelToPlayerMap) do
    begin
      PlayerIndex := DeviceCfg.ChannelToPlayerMap[ChannelIndex] - 1;
      if (PlayerIndex >= 0) and (PlayerIndex < PlayerCount) then
        PlayerState[PlayerIndex] := true;
    end;
  end;

  Result := 0;
  for I := 0 to High(PlayerState) do
  begin
    if (PlayerState[I] = false) then
    begin
      Result := I + 1;
      Break;
    end;
  end;
end;

function TAudioInputProcessor.CheckPlayersConfig(PlayerCount: cardinal): integer;
var
  PlayerState: TBooleanDynArray;
begin
  Result := CheckPlayersConfig(PlayerCount, PlayerState);
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
      Player := DeviceCfg.ChannelToPlayerMap[ChannelIndex] - 1;
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
