unit UAudioPlayback_SoftMixer;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}


uses
  Classes,
  SysUtils,
  sdl,
  UMusic,
  UAudioPlaybackBase;

type
  TAudioPlayback_SoftMixer = class;

  TGenericPlaybackStream = class(TAudioPlaybackStream)
    private
      Engine: TAudioPlayback_SoftMixer;

      DecodeStream: TAudioDecodeStream;
      SampleBuffer    : PChar;
      SampleBufferCount: integer; // number of available bytes in SampleBuffer
      SampleBufferPos : cardinal;
      BytesAvail: integer;
      cvt: TSDL_AudioCVT;

      Status:   TStreamStatus;
      Loop:     boolean;

      InternalLock: PSDL_Mutex;

      SoundEffects: TList;

      _volume: single;

      FadeInStartTime, FadeInTime: cardinal;
      FadeInStartVolume, FadeInTargetVolume: single;

      procedure Reset();

      class function ConvertAudioFormatToSDL(Format: TAudioSampleFormat; out SDLFormat: UInt16): boolean;
      function InitFormatConversion(): boolean;

      procedure Lock(); {$IFDEF HasInline}inline;{$ENDIF}
      procedure Unlock(); {$IFDEF HasInline}inline;{$ENDIF}
    public
      constructor Create(Engine: TAudioPlayback_SoftMixer);
      destructor Destroy(); override;

      function SetDecodeStream(decodeStream: TAudioDecodeStream): boolean;

      procedure Play();                     override;
      procedure Pause();                    override;
      procedure Stop();                     override;
      procedure FadeIn(Time: real; TargetVolume: single); override;

      procedure Close();                    override;

      function GetLength(): real;           override;
      function GetStatus(): TStreamStatus;  override;
      function GetVolume(): single;         override;
      procedure SetVolume(Volume: single);  override;
      function GetLoop(): boolean;          override;
      procedure SetLoop(Enabled: boolean);  override;
      function GetPosition: real;           override;
      procedure SetPosition(Time: real);    override;

      function ReadData(Buffer: PChar; BufSize: integer): integer;

      function GetPCMData(var data: TPCMData): Cardinal; override;
      procedure GetFFTData(var data: TFFTData);          override;

      procedure AddSoundEffect(effect: TSoundEffect);    override;
      procedure RemoveSoundEffect(effect: TSoundEffect); override;
  end;

  TAudioMixerStream = class
    private
      Engine: TAudioPlayback_SoftMixer;

      activeStreams: TList;
      mixerBuffer: PChar;
      internalLock: PSDL_Mutex;

      appVolume: single;

      procedure Lock(); {$IFDEF HasInline}inline;{$ENDIF}
      procedure Unlock(); {$IFDEF HasInline}inline;{$ENDIF}

      function GetVolume(): single;
      procedure SetVolume(volume: single);
    public
      constructor Create(Engine: TAudioPlayback_SoftMixer);
      destructor Destroy(); override;
      procedure AddStream(stream: TAudioPlaybackStream);
      procedure RemoveStream(stream: TAudioPlaybackStream);
      function ReadData(Buffer: PChar; BufSize: integer): integer;

      property Volume: single READ GetVolume WRITE SetVolume;
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
      procedure AudioCallback(buffer: PChar; size: integer); {$IFDEF HasInline}inline;{$ENDIF}

      function OpenStream(const Filename: String): TAudioPlaybackStream; override;
    public
      function GetName: String; override; abstract;
      function InitializePlayback(): boolean; override;
      function FinalizePlayback: boolean; override;

      procedure SetAppVolume(Volume: single); override;

      function GetMixer(): TAudioMixerStream; {$IFDEF HasInline}inline;{$ENDIF}
      function GetAudioFormatInfo(): TAudioFormatInfo;

      procedure MixBuffers(dst, src: PChar; size: Cardinal; volume: Single); virtual;
  end;

implementation

uses
  Math,
  //samplerate,
  UFFT,
  ULog,
  UIni,
  UMain;

{ TAudioMixerStream }

constructor TAudioMixerStream.Create(Engine: TAudioPlayback_SoftMixer);
begin
  inherited Create();

  Self.Engine := Engine;

  activeStreams := TList.Create;
  internalLock := SDL_CreateMutex();
  appVolume := 1.0;
end;

destructor TAudioMixerStream.Destroy();
begin
  if assigned(mixerBuffer) then
    Freemem(mixerBuffer);
  activeStreams.Free;
  SDL_DestroyMutex(internalLock);
  inherited;
end;

procedure TAudioMixerStream.Lock();
begin
  SDL_mutexP(internalLock);
end;

procedure TAudioMixerStream.Unlock();
begin
  SDL_mutexV(internalLock);
end;

function TAudioMixerStream.GetVolume(): single;
begin
  Lock();
  result := appVolume;
  Unlock();
end;

procedure TAudioMixerStream.SetVolume(volume: single);
begin
  Lock();
  appVolume := volume;
  Unlock();
end;

procedure TAudioMixerStream.AddStream(stream: TAudioPlaybackStream);
begin
  if not assigned(stream) then
    Exit;

  Lock();
  // check if stream is already in list to avoid duplicates
  if (activeStreams.IndexOf(Pointer(stream)) = -1) then
    activeStreams.Add(Pointer(stream));
  Unlock();
end;

(*
 * Sets the entry of stream in the activeStreams-List to nil
 * but does not remove it from the list (count is not changed!).
 * Otherwise iterations over the elements might fail due to a
 * changed count-property.
 * Call activeStreams.Pack() to remove the nil-pointers
 * or check for nil-pointers when accessing activeStreams.
 *)
procedure TAudioMixerStream.RemoveStream(stream: TAudioPlaybackStream);
var
  index: integer;
begin
  Lock();
  index := activeStreams.IndexOf(Pointer(stream));
  if (index <> -1) then
  begin
    // remove entry but do not decrease count-property
    activeStreams[index] := nil;
  end;
  Unlock();
end;

function TAudioMixerStream.ReadData(Buffer: PChar; BufSize: integer): integer;
var
  i: integer;
  size: integer;
  stream: TGenericPlaybackStream;
  needsPacking: boolean;
begin
  result := BufSize;

  // zero target-buffer (silence)
  FillChar(Buffer^, BufSize, 0);

  // resize mixer-buffer if necessary
  ReallocMem(mixerBuffer, BufSize);
  if not assigned(mixerBuffer) then
    Exit;

  Lock();

  needsPacking := false;

  // mix streams to one stream
  for i := 0 to activeStreams.Count-1 do
  begin
    if (activeStreams[i] = nil) then
    begin
      needsPacking := true;
      continue;
    end;

    stream := TGenericPlaybackStream(activeStreams[i]);
    // fetch data from current stream
    size := stream.ReadData(mixerBuffer, BufSize);
    if (size > 0) then
    begin
      // mix stream-data with mixer-buffer
      // Note: use Self.appVolume instead of Self.Volume to prevent recursive locking
      Engine.MixBuffers(Buffer, mixerBuffer, size, appVolume * stream.Volume);
    end;
  end;

  // remove nil-pointers from list
  if (needsPacking) then
  begin
    activeStreams.Pack();
  end;

  Unlock();
end;


{ TGenericPlaybackStream }

constructor TGenericPlaybackStream.Create(Engine: TAudioPlayback_SoftMixer);
begin
  inherited Create();
  Self.Engine := Engine;
  internalLock := SDL_CreateMutex();
  SoundEffects := TList.Create;
  Status := ssStopped;
  Reset();
end;

destructor TGenericPlaybackStream.Destroy();
begin
  Close();
  SDL_DestroyMutex(internalLock);
  FreeAndNil(SoundEffects);
  inherited;
end;

procedure TGenericPlaybackStream.Reset();
begin
  // wake-up sleeping audio-callback threads in the ReadData()-function
  if assigned(decodeStream) then
    decodeStream.Close();

  // stop audio-callback on this stream
  Stop();

  // reset and/or free data
  
  Loop := false;

  // TODO: use DecodeStream.Unref() instead of Free();
  FreeAndNil(DecodeStream);

  FreeMem(SampleBuffer);
  SampleBuffer := nil;
  SampleBufferPos := 0;
  BytesAvail := 0;
  
  _volume := 0;
  SoundEffects.Clear;
  FadeInTime := 0;
end;

procedure TGenericPlaybackStream.Lock();
begin
  SDL_mutexP(internalLock);
end;

procedure TGenericPlaybackStream.Unlock();
begin
  SDL_mutexV(internalLock);
end;

class function TGenericPlaybackStream.ConvertAudioFormatToSDL(Format: TAudioSampleFormat; out SDLFormat: UInt16): boolean;
begin
  case Format of
    asfU8:     SDLFormat := AUDIO_U8;
    asfS8:     SDLFormat := AUDIO_S8;
    asfU16LSB: SDLFormat := AUDIO_U16LSB;
    asfS16LSB: SDLFormat := AUDIO_S16LSB;
    asfU16MSB: SDLFormat := AUDIO_U16MSB;
    asfS16MSB: SDLFormat := AUDIO_S16MSB;
    asfU16:    SDLFormat := AUDIO_U16;
    asfS16:    SDLFormat := AUDIO_S16;
    else begin
      Result := false;
      Exit;
    end;
  end;
  Result := true;
end;

function TGenericPlaybackStream.InitFormatConversion(): boolean;
var
  srcFormat: UInt16;
  dstFormat: UInt16;
  srcFormatInfo: TAudioFormatInfo;
  dstFormatInfo: TAudioFormatInfo;
begin
  Result := false;

  srcFormatInfo := DecodeStream.GetAudioFormatInfo();
  dstFormatInfo := Engine.GetAudioFormatInfo();

  if (not ConvertAudioFormatToSDL(srcFormatInfo.Format, srcFormat) or
      not ConvertAudioFormatToSDL(dstFormatInfo.Format, dstFormat)) then
  begin
    Log.LogError('Audio-format not supported by SDL', 'TSoftMixerPlaybackStream.InitFormatConversion');
    Exit;
  end;

  if (SDL_BuildAudioCVT(@cvt,
    srcFormat, srcFormatInfo.Channels, Round(srcFormatInfo.SampleRate),
    dstFormat, dstFormatInfo.Channels, Round(dstFormatInfo.SampleRate)) = -1) then
  begin
    Log.LogError(SDL_GetError(), 'TSoftMixerPlaybackStream.InitFormatConversion');
    Exit;
  end;

  Result := true;
end;

function TGenericPlaybackStream.SetDecodeStream(decodeStream: TAudioDecodeStream): boolean;
begin
  result := false;

  Reset();

  if not assigned(decodeStream) then
    Exit;
  Self.DecodeStream := decodeStream;
  if not InitFormatConversion() then
    Exit;

  _volume := 1.0;

  result := true;
end;

procedure TGenericPlaybackStream.Close();
begin
  Reset();
end;

procedure TGenericPlaybackStream.Play();
var
  mixer: TAudioMixerStream;
begin
  if (status = ssPlaying) then
  begin
    // rewind
    if assigned(DecodeStream) then
      DecodeStream.Position := 0;
  end;
  status := ssPlaying;

  mixer := Engine.GetMixer();
  if (mixer <> nil) then
    mixer.AddStream(Self);
end;

procedure TGenericPlaybackStream.FadeIn(Time: real; TargetVolume: single);
begin
  FadeInTime := Trunc(Time * 1000);
  FadeInStartTime := SDL_GetTicks();
  FadeInStartVolume := _volume;
  FadeInTargetVolume := TargetVolume;
  Play();
end;

procedure TGenericPlaybackStream.Pause();
var
  mixer: TAudioMixerStream;
begin
  status := ssPaused;

  mixer := Engine.GetMixer();
  if (mixer <> nil) then
    mixer.RemoveStream(Self);
end;

procedure TGenericPlaybackStream.Stop();
var
  mixer: TAudioMixerStream;
begin
  if (status = ssStopped) then
    Exit;

  status := ssStopped;

  mixer := Engine.GetMixer();
  if (mixer <> nil) then
    mixer.RemoveStream(Self);

  // rewind (note: DecodeStream might be closed already, but this is not a problem)
  if assigned(DecodeStream) then
    DecodeStream.Position := 0;
end;

function TGenericPlaybackStream.GetLoop(): boolean;
begin
  result := Loop;
end;

procedure TGenericPlaybackStream.SetLoop(Enabled: boolean);
begin
  Loop := Enabled;
end;

function TGenericPlaybackStream.GetLength(): real;
begin
  if assigned(DecodeStream) then
    result := DecodeStream.Length
  else
    result := -1;
end;

function TGenericPlaybackStream.GetStatus(): TStreamStatus;
begin
  result := status;
end;

{*
 * Note: 44.1kHz to 48kHz conversion or vice versa is not supported
 *  by SDL at the moment. No conversion takes place in this cases.
 *  This is because SDL just converts differences in powers of 2.
 *  So the result might not be that accurate. Although this is not
 *  audible in most cases it needs synchronization with the video
 *  or the lyrics timer.
 *  Using libsamplerate might give better results.
 *}
function TGenericPlaybackStream.ReadData(Buffer: PChar; BufSize: integer): integer;
var
  decodeBufSize: integer;
  sampleBufSize: integer;
  nBytesDecoded: integer;
  frameSize: integer;
  remFrameBytes: integer;
  copyCnt: integer;
  BytesNeeded: integer;
  i: integer;
begin
  Result := -1;

  BytesNeeded := BufSize;

  // copy remaining data from the last call to the result-buffer
  if (BytesAvail > 0) then
  begin
    copyCnt := Min(BufSize, BytesAvail);
    Move(SampleBuffer[SampleBufferPos], Buffer[0], copyCnt);
    Dec(BytesAvail,  copyCnt);
    Dec(BytesNeeded, copyCnt);
    if (BytesNeeded = 0) then
    begin
      // Result-Buffer is full -> no need to decode more data.
      // The sample-buffer might even contain some data for the next call
      Inc(SampleBufferPos, copyCnt);
      Result := BufSize;
      Exit;
    end;
  end;

  if not assigned(DecodeStream) then
    Exit;

  // calc number of bytes to decode
  decodeBufSize := Ceil(BufSize / cvt.len_ratio);
  // assure that the decode-size is a multiple of the frame size
  frameSize := DecodeStream.GetAudioFormatInfo().FrameSize;
  remFrameBytes := decodeBufSize mod frameSize;
  if (remFrameBytes > 0) then
    decodeBufSize := decodeBufSize + (frameSize - remFrameBytes);

  Lock();
  try
    // calc buffer size
    sampleBufSize := decodeBufSize * cvt.len_mult;

    // resize buffer if necessary.
    // The required buffer-size will be smaller than the result-buffer
    // in most cases (if the decoded signal is mono or has a lesser bitrate).
    // If the output-rate is 44.1kHz and the decode-rate is 48kHz or 96kHz it
    // will be ~1.09 or ~2.18 times bigger. Those extra memory consumption
    // should be reasonable. If not we should call TDecodeStream.ReadData()
    // multiple times.
    // Note: we do not decrease the buffer by the count of bytes used from
    //   the previous call of this function (bytesAvail). Otherwise the
    //   buffer will be reallocated each time this function is called just to
    //   add or remove a few bytes from the buffer.
    //   By not doing this the buffer's size should be rather stable and it
    //   will not be reallocated/resized at all if the BufSize params does not
    //   change in consecutive calls.
    ReallocMem(SampleBuffer, sampleBufSize);
    if not assigned(SampleBuffer) then
      Exit;

    // decode data
    nBytesDecoded := DecodeStream.ReadData(SampleBuffer, decodeBufSize);
    if (nBytesDecoded = -1) then
      Exit;

    // end-of-file reached -> stop playback
    if (DecodeStream.EOF) then
    begin
      Stop();
    end;

    // resample decoded data
    cvt.buf := PUint8(SampleBuffer);
    cvt.len := nBytesDecoded;
    if (SDL_ConvertAudio(@cvt) = -1) then
      Exit;

    SampleBufferCount := cvt.len_cvt;

    // apply effects
    for i := 0 to SoundEffects.Count-1 do
    begin
      if (SoundEffects[i] <> nil) then
      begin
        TSoundEffect(SoundEffects[i]).Callback(SampleBuffer, SampleBufferCount);
      end;
    end;
  finally
    Unlock();
  end;

  BytesAvail := SampleBufferCount;
  SampleBufferPos := 0;

  // copy data to result buffer
  copyCnt := Min(BytesNeeded, BytesAvail);
  Move(SampleBuffer[0], Buffer[BufSize - BytesNeeded], copyCnt);
  Dec(BytesAvail,  copyCnt);
  Dec(BytesNeeded, copyCnt);
  Inc(SampleBufferPos, copyCnt);

  Result := BufSize - BytesNeeded;
end;

(* TODO: libsamplerate support
function TGenericPlaybackStream.ReadData(Buffer: PChar; BufSize: integer): integer;
var
  convState: PSRC_STATE;
  convData: SRC_DATA;
  error: integer;
begin
  // Note: needs mono->stereo conversion, multi-channel->stereo, etc.
  // maybe we should use SDL for the channel-conversion stuff
  // and use libsamplerate afterwards for the frequency-conversion

  //convState := src_new(SRC_SINC_MEDIUM_QUALITY, 2, @error);
  //src_short_to_float_array(input, output, len);
  convData.
  if (src_process(convState, @convData) <> 0) then
  begin
    Log.LogError(src_strerror(src_error(convState)), 'TSoftMixerPlaybackStream.ReadData');
    Exit;
  end;
  src_float_to_short_array();
  //src_delete(convState);
end;
*)

function TGenericPlaybackStream.GetPCMData(var data: TPCMData): Cardinal;
var
  nBytes: integer;
begin
  Result := 0;

  // just SInt16 stereo support for now
  if ((Engine.GetAudioFormatInfo().Format <> asfS16) or
      (Engine.GetAudioFormatInfo().Channels <> 2)) then
  begin
    Exit;
  end;

  // zero memory
  FillChar(data, SizeOf(data), 0);

  // TODO: At the moment just the first samples of the SampleBuffer
  // are returned, even if there is newer data in the upper samples.

  Lock();
  nBytes := Min(SizeOf(data), SampleBufferCount);
  if (nBytes > 0) then
  begin
    Move(SampleBuffer[0], data, nBytes);
  end;
  Unlock();
  
  Result := nBytes div SizeOf(TPCMStereoSample);
end;

procedure TGenericPlaybackStream.GetFFTData(var data: TFFTData);
var
  i: integer;
  Frames: integer;
  DataIn: PSingleArray;
  AudioFormat: TAudioFormatInfo;
begin
  // only works with SInt16 and Float values at the moment
  AudioFormat := Engine.GetAudioFormatInfo();

  DataIn := AllocMem(FFTSize * SizeOf(Single));
  if (DataIn = nil) then
    Exit;

  Lock();
  // TODO: We just use the first Frames frames, the others are ignored.
  // This is OK for the equalizer display but not if we want to use
  // this function for voice-analysis someday (I don't think we want).
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
  Unlock();

  WindowFunc(fwfHanning, FFTSize, DataIn);
  PowerSpectrum(FFTSize, DataIn, @data);
  FreeMem(DataIn);

  // resize data to a 0..1 range
  for i := 0 to High(TFFTData) do
  begin
    data[i] := Sqrt(data[i]) / 100;
  end;
end;

procedure TGenericPlaybackStream.AddSoundEffect(effect: TSoundEffect);
begin
  if (not assigned(effect)) then
    Exit;
  Lock();
  // check if effect is already in list to avoid duplicates
  if (SoundEffects.IndexOf(Pointer(effect)) = -1) then
    SoundEffects.Add(Pointer(effect));
  Unlock();
end;

procedure TGenericPlaybackStream.RemoveSoundEffect(effect: TSoundEffect);
begin
  Lock();
  SoundEffects.Remove(effect);
  Unlock();
end;

function TGenericPlaybackStream.GetPosition: real;
begin
  if assigned(DecodeStream) then
    result := DecodeStream.Position
  else
    result := -1;
end;

procedure TGenericPlaybackStream.SetPosition(Time: real);
begin
  if assigned(DecodeStream) then
    DecodeStream.Position := Time;
end;

function TGenericPlaybackStream.GetVolume(): single;
var
  FadeAmount: Single;
begin
  Lock();
  // adjust volume if fading is enabled
  if (FadeInTime > 0) then
  begin
    FadeAmount := (SDL_GetTicks() - FadeInStartTime) / FadeInTime;
    // check if fade-target is reached
    if (FadeAmount >= 1) then
    begin
      // target reached -> stop fading
      FadeInTime := 0;
      _volume := FadeInTargetVolume;
    end
    else
    begin
      // fading in progress
      _volume := FadeAmount*FadeInTargetVolume + (1-FadeAmount)*FadeInStartVolume;
    end;
  end;
  // return current volume
  Result := _volume;
  Unlock();
end;

procedure TGenericPlaybackStream.SetVolume(volume: single);
begin
  Lock();
  // stop fading
  FadeInTime := 0;
  // clamp volume
  if (volume > 1.0) then
    _volume := 1.0
  else if (volume < 0) then
    _volume := 0
  else
    _volume := volume;
  Unlock();
end;


{ TAudioPlayback_SoftMixer }

function TAudioPlayback_SoftMixer.InitializePlayback: boolean;
begin
  result := false;

  //Log.LogStatus('InitializePlayback', 'UAudioPlayback_SoftMixer');

  if(not InitializeAudioPlaybackEngine()) then
    Exit;

  MixerStream := TAudioMixerStream.Create(Self);

  if(not StartAudioPlaybackEngine()) then
    Exit;

  result := true;
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

procedure TAudioPlayback_SoftMixer.AudioCallback(buffer: PChar; size: integer);
begin
  MixerStream.ReadData(buffer, size);
end;

function TAudioPlayback_SoftMixer.GetMixer(): TAudioMixerStream;
begin
  Result := MixerStream;
end;

function TAudioPlayback_SoftMixer.GetAudioFormatInfo(): TAudioFormatInfo;
begin
  Result := FormatInfo;
end;

function TAudioPlayback_SoftMixer.OpenStream(const Filename: String): TAudioPlaybackStream;
var
  decodeStream: TAudioDecodeStream;
  playbackStream: TGenericPlaybackStream;
begin
  Result := nil;

  if (AudioDecoder = nil) then
    Exit;

  decodeStream := AudioDecoder.Open(Filename);
  if not assigned(decodeStream) then
  begin
    Log.LogStatus('LoadSoundFromFile: Sound not found "' + Filename + '"', 'UAudioPlayback_SoftMixer');
    Exit;
  end;

  playbackStream := TGenericPlaybackStream.Create(Self);
  if (not assigned(playbackStream)) then
  begin
    FreeAndNil(decodeStream);
    Exit;
  end;

  if (not playbackStream.SetDecodeStream(decodeStream)) then
  begin
    FreeAndNil(playbackStream);
    FreeAndNil(decodeStream);
    Exit;
  end;

  result := playbackStream;
end;

procedure TAudioPlayback_SoftMixer.SetAppVolume(Volume: single);
begin
  // sets volume only for this application
  MixerStream.Volume := Volume;
end;

procedure TAudioPlayback_SoftMixer.MixBuffers(dst, src: PChar; size: Cardinal; volume: Single);
var
  SampleIndex: Cardinal;
  SampleInt: Integer;
  SampleFlt: Single;
begin

  // TODO: optimize this code, e.g. with assembler (MMX)

  SampleIndex := 0;
  case FormatInfo.Format of
    asfS16:
    begin
      while (SampleIndex < size) do
      begin
        // apply volume and sum with previous mixer value
        SampleInt := PSmallInt(@dst[SampleIndex])^ + Round(PSmallInt(@src[SampleIndex])^ * volume);
        // clip result
        if (SampleInt > High(SmallInt)) then
          SampleInt := High(SmallInt)
        else if (SampleInt < Low(SmallInt)) then
          SampleInt := Low(SmallInt);
        // assign result
        PSmallInt(@dst[SampleIndex])^ := SampleInt;
        // increase index by one sample
        Inc(SampleIndex, SizeOf(SmallInt));
      end;
    end;
    asfFloat:
    begin
      while (SampleIndex < size) do
      begin
        // apply volume and sum with previous mixer value
        SampleFlt := PSingle(@dst[SampleIndex])^ + PSingle(@src[SampleIndex])^ * volume;
        // clip result
        if (SampleFlt > 1.0) then
          SampleFlt := 1.0
        else if (SampleFlt < -1.0) then
          SampleFlt := -1.0;
        // assign result
        PSingle(@dst[SampleIndex])^ := SampleFlt;
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
