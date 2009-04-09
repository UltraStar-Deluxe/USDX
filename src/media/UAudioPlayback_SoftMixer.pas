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

unit UAudioPlayback_SoftMixer;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
  sdl,
  SysUtils,
  URingBuffer,
  UMusic,
  UAudioPlaybackBase;

type
  TAudioPlayback_SoftMixer = class;

  TGenericPlaybackStream = class(TAudioPlaybackStream)
    private
      Engine: TAudioPlayback_SoftMixer;

      SampleBuffer:      PByteArray;
      SampleBufferSize:  integer;
      SampleBufferCount: integer; // number of available bytes in SampleBuffer
      SampleBufferPos:   integer;

      SourceBuffer:      PByteArray;
      SourceBufferSize:  integer;
      SourceBufferCount: integer; // number of available bytes in SourceBuffer

      Converter: TAudioConverter;
      Status:   TStreamStatus;
      InternalLock: PSDL_Mutex;
      SoundEffects: TList;
      fVolume: single;

      FadeInStartTime, FadeInTime: cardinal;
      FadeInStartVolume, FadeInTargetVolume: single;

      NeedsRewind: boolean;

      procedure Reset();

      procedure ApplySoundEffects(Buffer: PByteArray; BufferSize: integer);
      function InitFormatConversion(): boolean;
      procedure FlushBuffers();

      procedure LockSampleBuffer(); {$IFDEF HasInline}inline;{$ENDIF}
      procedure UnlockSampleBuffer(); {$IFDEF HasInline}inline;{$ENDIF}
    protected
      function GetLatency(): double;        override;
      function GetStatus(): TStreamStatus;  override;
      function GetVolume(): single;         override;
      procedure SetVolume(Volume: single);  override;
      function GetLength(): real;           override;
      function GetLoop(): boolean;          override;
      procedure SetLoop(Enabled: boolean);  override;
      function GetPosition: real;           override;
      procedure SetPosition(Time: real);    override;
    public
      constructor Create(Engine: TAudioPlayback_SoftMixer);
      destructor Destroy(); override;

      function Open(SourceStream: TAudioSourceStream): boolean; override;
      procedure Close();                    override;

      procedure Play();                     override;
      procedure Pause();                    override;
      procedure Stop();                     override;
      procedure FadeIn(Time: real; TargetVolume: single); override;

      function GetAudioFormatInfo(): TAudioFormatInfo; override;

      function ReadData(Buffer: PByteArray; BufferSize: integer): integer;

      function GetPCMData(var Data: TPCMData): Cardinal; override;
      procedure GetFFTData(var Data: TFFTData);          override;

      procedure AddSoundEffect(Effect: TSoundEffect);    override;
      procedure RemoveSoundEffect(Effect: TSoundEffect); override;
  end;

  TAudioMixerStream = class
    private
      Engine: TAudioPlayback_SoftMixer;

      ActiveStreams: TList;
      MixerBuffer: PByteArray;
      InternalLock: PSDL_Mutex;

      AppVolume: single;

      procedure Lock(); {$IFDEF HasInline}inline;{$ENDIF}
      procedure Unlock(); {$IFDEF HasInline}inline;{$ENDIF}

      function GetVolume(): single;
      procedure SetVolume(Volume: single);
    public
      constructor Create(Engine: TAudioPlayback_SoftMixer);
      destructor Destroy(); override;
      procedure AddStream(Stream: TAudioPlaybackStream);
      procedure RemoveStream(Stream: TAudioPlaybackStream);
      function ReadData(Buffer: PByteArray; BufferSize: integer): integer;

      property Volume: single read GetVolume write SetVolume;
  end;

  TAudioPlayback_SoftMixer = class(TAudioPlaybackBase)
    private
      MixerStream: TAudioMixerStream;
    protected
      FormatInfo: TAudioFormatInfo;

      function InitializeAudioPlaybackEngine(): boolean; virtual; abstract;
      function StartAudioPlaybackEngine(): boolean;      virtual; abstract;
      procedure StopAudioPlaybackEngine();               virtual; abstract;
      function FinalizeAudioPlaybackEngine(): boolean;   virtual; abstract;
      procedure AudioCallback(Buffer: PByteArray; Size: integer); {$IFDEF HasInline}inline;{$ENDIF}

      function CreatePlaybackStream(): TAudioPlaybackStream; override;
    public
      function GetName: String; override; abstract;
      function InitializePlayback(): boolean; override;
      function FinalizePlayback: boolean; override;

      procedure SetAppVolume(Volume: single); override;

      function CreateVoiceStream(ChannelMap: integer; FormatInfo: TAudioFormatInfo): TAudioVoiceStream; override;

      function GetMixer(): TAudioMixerStream; {$IFDEF HasInline}inline;{$ENDIF}
      function GetAudioFormatInfo(): TAudioFormatInfo;

      procedure MixBuffers(DstBuffer, SrcBuffer: PByteArray; Size: Cardinal; Volume: Single); virtual;
  end;

type
  TGenericVoiceStream = class(TAudioVoiceStream)
    private
      VoiceBuffer: TRingBuffer;
      BufferLock: PSDL_Mutex;
      PlaybackStream: TGenericPlaybackStream;
      Engine: TAudioPlayback_SoftMixer;
    public
      constructor Create(Engine: TAudioPlayback_SoftMixer);

      function Open(ChannelMap: integer; FormatInfo: TAudioFormatInfo): boolean; override;
      procedure Close(); override;
      procedure WriteData(Buffer: PByteArray; BufferSize: integer); override;
      function ReadData(Buffer: PByteArray; BufferSize: integer): integer; override;
      function IsEOF(): boolean; override;
      function IsError(): boolean; override;
  end;

const
  SOURCE_BUFFER_FRAMES = 4096;

const
  MAX_VOICE_DELAY = 0.500; // 20ms

implementation

uses
  Math,
  ULog,
  UIni,
  UFFT,
  UAudioConverter,
  UMain;

{ TAudioMixerStream }

constructor TAudioMixerStream.Create(Engine: TAudioPlayback_SoftMixer);
begin
  inherited Create();

  Self.Engine := Engine;

  ActiveStreams := TList.Create;
  InternalLock := SDL_CreateMutex();
  AppVolume := 1.0;
end;

destructor TAudioMixerStream.Destroy();
begin
  if assigned(MixerBuffer) then
    Freemem(MixerBuffer);
  ActiveStreams.Free;
  SDL_DestroyMutex(InternalLock);
  inherited;
end;

procedure TAudioMixerStream.Lock();
begin
  SDL_mutexP(InternalLock);
end;

procedure TAudioMixerStream.Unlock();
begin
  SDL_mutexV(InternalLock);
end;

function TAudioMixerStream.GetVolume(): single;
begin
  Lock();
  Result := AppVolume;
  Unlock();
end;

procedure TAudioMixerStream.SetVolume(Volume: single);
begin
  Lock();
  AppVolume := Volume;
  Unlock();
end;

procedure TAudioMixerStream.AddStream(Stream: TAudioPlaybackStream);
begin
  if not assigned(Stream) then
    Exit;

  Lock();
  // check if stream is already in list to avoid duplicates
  if (ActiveStreams.IndexOf(Pointer(Stream)) = -1) then
    ActiveStreams.Add(Pointer(Stream));
  Unlock();
end;

(*
 * Sets the entry of stream in the ActiveStreams-List to nil
 * but does not remove it from the list (Count is not changed!).
 * Otherwise iterations over the elements might fail due to a
 * changed Count-property.
 * Call ActiveStreams.Pack() to remove the nil-pointers
 * or check for nil-pointers when accessing ActiveStreams.
 *)
procedure TAudioMixerStream.RemoveStream(Stream: TAudioPlaybackStream);
var
  Index: integer;
begin
  Lock();
  Index := activeStreams.IndexOf(Pointer(Stream));
  if (Index <> -1) then
  begin
    // remove entry but do not decrease count-property
    ActiveStreams[Index] := nil;
  end;
  Unlock();
end;

function TAudioMixerStream.ReadData(Buffer: PByteArray; BufferSize: integer): integer;
var
  i: integer;
  Size: integer;
  Stream: TGenericPlaybackStream;
  NeedsPacking: boolean;
begin
  Result := BufferSize;

  // zero target-buffer (silence)
  FillChar(Buffer^, BufferSize, 0);

  // resize mixer-buffer if necessary
  ReallocMem(MixerBuffer, BufferSize);
  if not assigned(MixerBuffer) then
    Exit;

  Lock();

  NeedsPacking := false;

  // mix streams to one stream
  for i := 0 to ActiveStreams.Count-1 do
  begin
    if (ActiveStreams[i] = nil) then
    begin
      NeedsPacking := true;
      continue;
    end;

    Stream := TGenericPlaybackStream(ActiveStreams[i]);
    // fetch data from current stream
    Size := Stream.ReadData(MixerBuffer, BufferSize);
    if (Size > 0) then
    begin
      // mix stream-data with mixer-buffer
      // Note: use Self.appVolume instead of Self.Volume to prevent recursive locking
      Engine.MixBuffers(Buffer, MixerBuffer, Size, AppVolume * Stream.Volume);
    end;
  end;

  // remove nil-pointers from list
  if (NeedsPacking) then
  begin
    ActiveStreams.Pack();
  end;

  Unlock();
end;


{ TGenericPlaybackStream }

constructor TGenericPlaybackStream.Create(Engine: TAudioPlayback_SoftMixer);
begin
  inherited Create();
  Self.Engine := Engine;
  InternalLock := SDL_CreateMutex();
  SoundEffects := TList.Create;
  Status := ssStopped;
  Reset();
end;

destructor TGenericPlaybackStream.Destroy();
begin
  Close();
  SDL_DestroyMutex(InternalLock);
  FreeAndNil(SoundEffects);
  inherited;
end;

procedure TGenericPlaybackStream.Reset();
begin
  SourceStream := nil;

  FreeAndNil(Converter);

  FreeMem(SampleBuffer);
  SampleBuffer := nil;
  SampleBufferPos := 0;
  SampleBufferSize := 0;
  SampleBufferCount := 0;

  FreeMem(SourceBuffer);
  SourceBuffer := nil;
  SourceBufferSize := 0;
  SourceBufferCount := 0;

  NeedsRewind := false;

  fVolume := 0;
  SoundEffects.Clear;
  FadeInTime := 0;
end;

function TGenericPlaybackStream.Open(SourceStream: TAudioSourceStream): boolean;
begin
  Result := false;

  Close();

  if (not assigned(SourceStream)) then
    Exit;
  Self.SourceStream := SourceStream;

  if (not InitFormatConversion()) then
  begin
    // reset decode-stream so it will not be freed on destruction
    Self.SourceStream := nil;
    Exit;
  end;

  SourceBufferSize := SOURCE_BUFFER_FRAMES * SourceStream.GetAudioFormatInfo().FrameSize;
  GetMem(SourceBuffer, SourceBufferSize);
  fVolume := 1.0;

  Result := true;
end;

procedure TGenericPlaybackStream.Close();
begin
  // stop audio-callback on this stream
  Stop();

  // Note: PerformOnClose must be called before SourceStream is invalidated
  PerformOnClose();
  // and free data
  Reset();
end;

procedure TGenericPlaybackStream.LockSampleBuffer();
begin
  SDL_mutexP(InternalLock);
end;

procedure TGenericPlaybackStream.UnlockSampleBuffer();
begin
  SDL_mutexV(InternalLock);
end;

function TGenericPlaybackStream.InitFormatConversion(): boolean;
var
  SrcFormatInfo: TAudioFormatInfo;
  DstFormatInfo: TAudioFormatInfo;
begin
  Result := false;

  SrcFormatInfo := SourceStream.GetAudioFormatInfo();
  DstFormatInfo := GetAudioFormatInfo();

  // TODO: selection should not be done here, use a factory (TAudioConverterFactory) instead 
  {$IF Defined(UseFFmpegResample)}
  Converter := TAudioConverter_FFmpeg.Create();
  {$ELSEIF Defined(UseSRCResample)}
  Converter := TAudioConverter_SRC.Create();
  {$ELSE}
  Converter := TAudioConverter_SDL.Create();
  {$IFEND}

  Result := Converter.Init(SrcFormatInfo, DstFormatInfo);
end;

procedure TGenericPlaybackStream.Play();
var
  Mixer: TAudioMixerStream;
begin
  // only paused streams are not flushed
  if (Status = ssPaused) then
    NeedsRewind := false;

  // rewind if necessary. Cases that require no rewind are:
  // - stream was created and never played
  // - stream was paused and is resumed now
  // - stream was stopped and set to a new position already
  if (NeedsRewind) then
    SetPosition(0);

  // update status
  Status := ssPlaying;

  NeedsRewind := true;

  // add this stream to the mixer
  Mixer := Engine.GetMixer();
  if (Mixer <> nil) then
    Mixer.AddStream(Self);
end;

procedure TGenericPlaybackStream.FadeIn(Time: real; TargetVolume: single);
begin
  FadeInTime := Trunc(Time * 1000);
  FadeInStartTime := SDL_GetTicks();
  FadeInStartVolume := fVolume;
  FadeInTargetVolume := TargetVolume;
  Play();
end;

procedure TGenericPlaybackStream.Pause();
var
  Mixer: TAudioMixerStream;
begin
  if (Status <> ssPlaying) then
    Exit;

  Status := ssPaused;

  Mixer := Engine.GetMixer();
  if (Mixer <> nil) then
    Mixer.RemoveStream(Self);
end;

procedure TGenericPlaybackStream.Stop();
var
  Mixer: TAudioMixerStream;
begin
  if (Status = ssStopped) then
    Exit;

  Status := ssStopped;

  Mixer := Engine.GetMixer();
  if (Mixer <> nil) then
    Mixer.RemoveStream(Self);
end;

function TGenericPlaybackStream.GetLoop(): boolean;
begin
  if assigned(SourceStream) then
    Result := SourceStream.Loop
  else
    Result := false;
end;

procedure TGenericPlaybackStream.SetLoop(Enabled: boolean);
begin
  if assigned(SourceStream) then
    SourceStream.Loop := Enabled;
end;

function TGenericPlaybackStream.GetLength(): real;
begin
  if assigned(SourceStream) then
    Result := SourceStream.Length
  else
    Result := -1;
end;

function TGenericPlaybackStream.GetLatency(): double;
begin
  Result := Engine.GetLatency();
end;

function TGenericPlaybackStream.GetStatus(): TStreamStatus;
begin
  Result := Status;
end;

function TGenericPlaybackStream.GetAudioFormatInfo(): TAudioFormatInfo;
begin
  Result := Engine.GetAudioFormatInfo();
end;

procedure TGenericPlaybackStream.FlushBuffers();
begin
  SampleBufferCount := 0;
  SampleBufferPos := 0;
  SourceBufferCount := 0;
end;

procedure TGenericPlaybackStream.ApplySoundEffects(Buffer: PByteArray; BufferSize: integer);
var
  i: integer;
begin
  for i := 0 to SoundEffects.Count-1 do
  begin
    if (SoundEffects[i] <> nil) then
    begin
      TSoundEffect(SoundEffects[i]).Callback(Buffer, BufferSize);
    end;
  end;
end;

function TGenericPlaybackStream.ReadData(Buffer: PByteArray; BufferSize: integer): integer;
var
  ConversionInputCount: integer;
  ConversionOutputSize: integer;   // max. number of converted data (= buffer size)
  ConversionOutputCount: integer;  // actual number of converted data
  SourceSize: integer;
  NeededSampleBufferSize: integer;
  BytesNeeded: integer;
  SourceFormatInfo, OutputFormatInfo: TAudioFormatInfo;
  SourceFrameSize, OutputFrameSize: integer;
  SkipOutputCount: integer;  // number of output-data bytes to skip
  SkipSourceCount: integer;  // number of source-data bytes to skip
  FillCount: integer;  // number of bytes to fill with padding data
  CopyCount: integer;
  PadFrame: PByteArray;
begin
  Result := -1;

  // sanity check for the source-stream
  if (not assigned(SourceStream)) then
    Exit;
  
  SkipOutputCount := 0;
  SkipSourceCount := 0;
  FillCount := 0;

  SourceFormatInfo := SourceStream.GetAudioFormatInfo();
  SourceFrameSize := SourceFormatInfo.FrameSize;
  OutputFormatInfo := GetAudioFormatInfo();
  OutputFrameSize := OutputFormatInfo.FrameSize;

  // synchronize (adjust buffer size)
  BytesNeeded := Synchronize(BufferSize, OutputFormatInfo);
  if (BytesNeeded > BufferSize) then
  begin
    SkipOutputCount := BytesNeeded - BufferSize;
    BytesNeeded := BufferSize;
  end
  else if (BytesNeeded < BufferSize) then
  begin
    FillCount := BufferSize - BytesNeeded;
  end;

  // lock access to sample-buffer
  LockSampleBuffer();
  try

    // skip sample-buffer data
    SampleBufferPos := SampleBufferPos + SkipOutputCount;
    // size of available bytes in SampleBuffer after skipping
    SampleBufferCount := SampleBufferCount - SampleBufferPos;
    // update byte skip-count
    SkipOutputCount := -SampleBufferCount;

    // now that we skipped all buffered data from the last pass, we have to skip
    // data directly after fetching it from the source-stream.
    if (SkipOutputCount > 0) then
    begin
      SampleBufferCount := 0;
      // convert skip-count to source-format units and resize to a multiple of
      // the source frame-size.
      SkipSourceCount := Round((SkipOutputCount * OutputFormatInfo.GetRatio(SourceFormatInfo)) /
                               SourceFrameSize) * SourceFrameSize;
      SkipOutputCount := 0;
    end;

    // copy data to front of buffer
    if ((SampleBufferCount > 0) and (SampleBufferPos > 0)) then
      Move(SampleBuffer[SampleBufferPos], SampleBuffer[0], SampleBufferCount);
    SampleBufferPos := 0;

    // resize buffer to a reasonable size
    if (BufferSize > SampleBufferCount) then
    begin
      // Note: use BufferSize instead of BytesNeeded to minimize the need for resizing
      SampleBufferSize := BufferSize;
      ReallocMem(SampleBuffer, SampleBufferSize);
      if (not assigned(SampleBuffer)) then
        Exit;
    end;

    // fill sample-buffer (fetch and convert one block of source data per loop)
    while (SampleBufferCount < BytesNeeded) do
    begin
      // move remaining source data from the previous pass to front of buffer
      if (SourceBufferCount > 0) then
      begin
        Move(SourceBuffer[SourceBufferSize-SourceBufferCount],
             SourceBuffer[0],
             SourceBufferCount);
      end;

      SourceSize := SourceStream.ReadData(
          @SourceBuffer[SourceBufferCount], SourceBufferSize-SourceBufferCount);
      // break on error (-1) or if no data is available (0), e.g. while seeking
      if (SourceSize <= 0) then
      begin
        // if we do not have data -> exit
        if (SourceBufferCount = 0) then
        begin
          FlushBuffers();
          Exit;
        end;
        // if we have some data, stop retrieving data from the source stream
        // and use the data we have so far
        Break;
      end;

      SourceBufferCount := SourceBufferCount + SourceSize;

      // end-of-file reached -> stop playback
      if (SourceStream.EOF) then
      begin
        if (Loop) then
          SourceStream.Position := 0
        else
          Stop();
      end;

      if (SkipSourceCount > 0) then
      begin
        // skip data and update source buffer count
        SourceBufferCount := SourceBufferCount - SkipSourceCount;
        SkipSourceCount := -SourceBufferCount;
        // continue with next pass if we skipped all data
        if (SourceBufferCount <= 0) then
        begin
          SourceBufferCount := 0;
          Continue;
        end;
      end;

      // calc buffer size (might be bigger than actual resampled byte count)
      ConversionOutputSize := Converter.GetOutputBufferSize(SourceBufferCount);
      NeededSampleBufferSize := SampleBufferCount + ConversionOutputSize;

      // resize buffer if necessary
      if (SampleBufferSize < NeededSampleBufferSize) then
      begin
        SampleBufferSize := NeededSampleBufferSize;
        ReallocMem(SampleBuffer, SampleBufferSize);
        if (not assigned(SampleBuffer)) then
        begin
          FlushBuffers();
          Exit;
        end;
      end;

      // resample source data (Note: ConversionInputCount might be adjusted by Convert())
      ConversionInputCount := SourceBufferCount;
      ConversionOutputCount := Converter.Convert(
          SourceBuffer, @SampleBuffer[SampleBufferCount], ConversionInputCount);
      if (ConversionOutputCount = -1) then
      begin
        FlushBuffers();
        Exit;
      end;

      // adjust sample- and source-buffer count by the number of converted bytes
      SampleBufferCount := SampleBufferCount + ConversionOutputCount;
      SourceBufferCount := SourceBufferCount - ConversionInputCount;
    end;

    // apply effects
    ApplySoundEffects(SampleBuffer, SampleBufferCount);

    // copy data to result buffer
    CopyCount := Min(BytesNeeded, SampleBufferCount);
    Move(SampleBuffer[0], Buffer[BufferSize - BytesNeeded], CopyCount);
    Dec(BytesNeeded, CopyCount);
    SampleBufferPos := CopyCount;

  // release buffer lock
  finally
    UnlockSampleBuffer();
  end;

  // pad the buffer with the last frame if we are to fast
  if (FillCount > 0) then
  begin
    if (CopyCount >= OutputFrameSize) then
      PadFrame := @Buffer[CopyCount-OutputFrameSize]
    else
      PadFrame := nil;
    FillBufferWithFrame(@Buffer[CopyCount], FillCount,
                        PadFrame, OutputFrameSize);
  end;

  // BytesNeeded now contains the number of remaining bytes we were not able to fetch
  Result := BufferSize - BytesNeeded;
end;

function TGenericPlaybackStream.GetPCMData(var Data: TPCMData): Cardinal;
var
  ByteCount: integer;
begin
  Result := 0;

  // just SInt16 stereo support for now
  if ((Engine.GetAudioFormatInfo().Format <> asfS16) or
      (Engine.GetAudioFormatInfo().Channels <> 2)) then
  begin
    Exit;
  end;

  // zero memory
  FillChar(Data, SizeOf(Data), 0);

  // TODO: At the moment just the first samples of the SampleBuffer
  // are returned, even if there is newer data in the upper samples.

  LockSampleBuffer();
  ByteCount := Min(SizeOf(Data), SampleBufferCount);
  if (ByteCount > 0) then
  begin
    Move(SampleBuffer[0], Data, ByteCount);
  end;
  UnlockSampleBuffer();
  
  Result := ByteCount div SizeOf(TPCMStereoSample);
end;

procedure TGenericPlaybackStream.GetFFTData(var Data: TFFTData);
var
  i: integer;
  Frames: integer;
  DataIn: PSingleArray;
  AudioFormat: TAudioFormatInfo;
begin
  // only works with SInt16 and Float values at the moment
  AudioFormat := GetAudioFormatInfo();

  DataIn := AllocMem(FFTSize * SizeOf(Single));
  if (DataIn = nil) then
    Exit;

  LockSampleBuffer();
  // TODO: We just use the first Frames frames, the others are ignored.
  Frames := Min(FFTSize, SampleBufferCount div AudioFormat.FrameSize);
  // use only first channel and convert data to float-values
  case AudioFormat.Format of
    asfS16:
    begin
      for i := 0 to Frames-1 do
        DataIn[i] := PSmallInt(@SampleBuffer[i*AudioFormat.FrameSize])^ / -Low(SmallInt);
    end;
    asfFloat:
    begin
      for i := 0 to Frames-1 do
        DataIn[i] := PSingle(@SampleBuffer[i*AudioFormat.FrameSize])^;
    end;
  end;
  UnlockSampleBuffer();

  WindowFunc(fwfHanning, FFTSize, DataIn);
  PowerSpectrum(FFTSize, DataIn, @Data);
  FreeMem(DataIn);

  // resize data to a 0..1 range
  for i := 0 to High(TFFTData) do
  begin
    Data[i] := Sqrt(Data[i]) / 100;
  end;
end;

procedure TGenericPlaybackStream.AddSoundEffect(Effect: TSoundEffect);
begin
  if (not assigned(Effect)) then
    Exit;

  LockSampleBuffer();
  // check if effect is already in list to avoid duplicates
  if (SoundEffects.IndexOf(Pointer(Effect)) = -1) then
    SoundEffects.Add(Pointer(Effect));
  UnlockSampleBuffer();
end;

procedure TGenericPlaybackStream.RemoveSoundEffect(Effect: TSoundEffect);
begin
  LockSampleBuffer();
  SoundEffects.Remove(Effect);
  UnlockSampleBuffer();
end;

function TGenericPlaybackStream.GetPosition: real;
var
  BufferedTime: double;
begin
  if assigned(SourceStream) then
  begin
    LockSampleBuffer();

    // calc the time of source data that is buffered (in the SampleBuffer and SourceBuffer)
    // but not yet outputed
    BufferedTime := (SampleBufferCount - SampleBufferPos) / Engine.FormatInfo.BytesPerSec +
                    SourceBufferCount / SourceStream.GetAudioFormatInfo().BytesPerSec;
    // and subtract it from the source position
    Result := SourceStream.Position - BufferedTime;

    UnlockSampleBuffer();
  end
  else
  begin
    Result := -1;
  end;
end;

procedure TGenericPlaybackStream.SetPosition(Time: real);
begin
  if assigned(SourceStream) then
  begin
    LockSampleBuffer();

    SourceStream.Position := Time;
    if (Status = ssStopped) then
      NeedsRewind := false;
    // do not use outdated data
    FlushBuffers();

    AvgSyncDiff := -1;
    
    UnlockSampleBuffer();
  end;
end;

function TGenericPlaybackStream.GetVolume(): single;
var
  FadeAmount: Single;
begin
  LockSampleBuffer();
  // adjust volume if fading is enabled
  if (FadeInTime > 0) then
  begin
    FadeAmount := (SDL_GetTicks() - FadeInStartTime) / FadeInTime;
    // check if fade-target is reached
    if (FadeAmount >= 1) then
    begin
      // target reached -> stop fading
      FadeInTime := 0;
      fVolume := FadeInTargetVolume;
    end
    else
    begin
      // fading in progress
      fVolume := FadeAmount*FadeInTargetVolume + (1-FadeAmount)*FadeInStartVolume;
    end;
  end;
  // return current volume
  Result := fVolume;
  UnlockSampleBuffer();
end;

procedure TGenericPlaybackStream.SetVolume(Volume: single);
begin
  LockSampleBuffer();
  // stop fading
  FadeInTime := 0;
  // clamp volume
  if (Volume > 1.0) then
    fVolume := 1.0
  else if (Volume < 0) then
    fVolume := 0
  else
    fVolume := Volume;
  UnlockSampleBuffer();
end;


{ TGenericVoiceStream }

constructor TGenericVoiceStream.Create(Engine: TAudioPlayback_SoftMixer);
begin
  inherited Create();
  Self.Engine := Engine;
end;

function TGenericVoiceStream.Open(ChannelMap: integer; FormatInfo: TAudioFormatInfo): boolean;
var
  BufferSize: integer;
begin
  Result := false;

  Close();

  if (not inherited Open(ChannelMap, FormatInfo)) then
    Exit;

  // Note:
  // - use Self.FormatInfo instead of FormatInfo as the latter one might have a
  //   channel size of 2.
  // - the buffer-size must be a multiple of the FrameSize
  BufferSize := (Ceil(MAX_VOICE_DELAY * Self.FormatInfo.BytesPerSec) div Self.FormatInfo.FrameSize) *
                Self.FormatInfo.FrameSize;
  VoiceBuffer := TRingBuffer.Create(BufferSize);

  BufferLock := SDL_CreateMutex();


  // create a matching playback stream for the voice-stream
  PlaybackStream := TGenericPlaybackStream.Create(Engine);
  // link voice- and playback-stream
  if (not PlaybackStream.Open(Self)) then
  begin
    PlaybackStream.Free;
    Exit;
  end;

  // start voice passthrough
  PlaybackStream.Play();

  Result := true;
end;

procedure TGenericVoiceStream.Close();
begin
  // stop and free the playback stream
  FreeAndNil(PlaybackStream);

  // free data
  FreeAndNil(VoiceBuffer);
  if (BufferLock <> nil) then
    SDL_DestroyMutex(BufferLock);

  inherited Close();
end;

procedure TGenericVoiceStream.WriteData(Buffer: PByteArray; BufferSize: integer);
begin
  // lock access to buffer
  SDL_mutexP(BufferLock);
  try
    if (VoiceBuffer = nil) then
      Exit;
    VoiceBuffer.Write(Buffer, BufferSize);
  finally
    SDL_mutexV(BufferLock);
  end;
end;

function TGenericVoiceStream.ReadData(Buffer: PByteArray; BufferSize: integer): integer;
begin
  Result := -1;

  // lock access to buffer
  SDL_mutexP(BufferLock);
  try
    if (VoiceBuffer = nil) then
      Exit;
    Result := VoiceBuffer.Read(Buffer, BufferSize);
  finally
    SDL_mutexV(BufferLock);
  end;
end;

function TGenericVoiceStream.IsEOF(): boolean;
begin
  SDL_mutexP(BufferLock);
  Result := (VoiceBuffer = nil);
  SDL_mutexV(BufferLock);
end;

function TGenericVoiceStream.IsError(): boolean;
begin
  Result := false;
end;


{ TAudioPlayback_SoftMixer }

function TAudioPlayback_SoftMixer.InitializePlayback: boolean;
begin
  Result := false;

  //Log.LogStatus('InitializePlayback', 'UAudioPlayback_SoftMixer');

  if(not InitializeAudioPlaybackEngine()) then
    Exit;

  MixerStream := TAudioMixerStream.Create(Self);

  if(not StartAudioPlaybackEngine()) then
    Exit;

  Result := true;
end;

function TAudioPlayback_SoftMixer.FinalizePlayback: boolean;
begin
  Close;
  StopAudioPlaybackEngine();

  FreeAndNil(MixerStream);
  FreeAndNil(FormatInfo);

  FinalizeAudioPlaybackEngine();
  inherited FinalizePlayback;
  Result := true;
end;

procedure TAudioPlayback_SoftMixer.AudioCallback(Buffer: PByteArray; Size: integer);
begin
  MixerStream.ReadData(Buffer, Size);
end;

function TAudioPlayback_SoftMixer.GetMixer(): TAudioMixerStream;
begin
  Result := MixerStream;
end;

function TAudioPlayback_SoftMixer.GetAudioFormatInfo(): TAudioFormatInfo;
begin
  Result := FormatInfo;
end;

function TAudioPlayback_SoftMixer.CreatePlaybackStream(): TAudioPlaybackStream;
begin
  Result := TGenericPlaybackStream.Create(Self);
end;

function TAudioPlayback_SoftMixer.CreateVoiceStream(ChannelMap: integer; FormatInfo: TAudioFormatInfo): TAudioVoiceStream;
var
  VoiceStream: TGenericVoiceStream;
begin
  Result := nil;

  // create a voice stream
  VoiceStream := TGenericVoiceStream.Create(Self);
  if (not VoiceStream.Open(ChannelMap, FormatInfo)) then
  begin
    VoiceStream.Free;
    Exit;
  end;

  Result := VoiceStream;
end;

procedure TAudioPlayback_SoftMixer.SetAppVolume(Volume: single);
begin
  // sets volume only for this application
  MixerStream.Volume := Volume;
end;

procedure TAudioPlayback_SoftMixer.MixBuffers(DstBuffer, SrcBuffer: PByteArray; Size: Cardinal; Volume: Single);
var
  SampleIndex: Cardinal;
  SampleInt: Integer;
  SampleFlt: Single;
begin
  SampleIndex := 0;
  case FormatInfo.Format of
    asfS16:
    begin
      while (SampleIndex < Size) do
      begin
        // apply volume and sum with previous mixer value
        SampleInt := PSmallInt(@DstBuffer[SampleIndex])^ +
                     Round(PSmallInt(@SrcBuffer[SampleIndex])^ * Volume);
        // clip result
        if (SampleInt > High(SmallInt)) then
          SampleInt := High(SmallInt)
        else if (SampleInt < Low(SmallInt)) then
          SampleInt := Low(SmallInt);
        // assign result
        PSmallInt(@DstBuffer[SampleIndex])^ := SampleInt;
        // increase index by one sample
        Inc(SampleIndex, SizeOf(SmallInt));
      end;
    end;
    asfFloat:
    begin
      while (SampleIndex < Size) do
      begin
        // apply volume and sum with previous mixer value
        SampleFlt := PSingle(@DstBuffer[SampleIndex])^ +
                     PSingle(@SrcBuffer[SampleIndex])^ * Volume;
        // clip result
        if (SampleFlt > 1.0) then
          SampleFlt := 1.0
        else if (SampleFlt < -1.0) then
          SampleFlt := -1.0;
        // assign result
        PSingle(@DstBuffer[SampleIndex])^ := SampleFlt;
        // increase index by one sample
        Inc(SampleIndex, SizeOf(Single));
      end;
    end;
    else
    begin
      Log.LogError('Incompatible format', 'TAudioMixerStream.MixAudio');
    end;
  end;
end;

end.
