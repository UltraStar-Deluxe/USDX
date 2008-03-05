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
  UMusic;

type
  TAudioPlayback_SoftMixer = class;

  TSoftMixerPlaybackStream = class(TAudioPlaybackStream)
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
      _volume: integer;

      InternalLock: PSDL_Mutex;

      procedure Reset();

      class function ConvertAudioFormatToSDL(fmt: TAudioSampleFormat): UInt16;
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
      procedure Close();                    override;
      function GetLoop(): boolean;          override;
      procedure SetLoop(Enabled: boolean);  override;
      function GetLength(): real;           override;
      function GetStatus(): TStreamStatus;  override;

      function IsLoaded(): boolean;

      function GetVolume(): integer;        override;
      procedure SetVolume(Volume: integer); override;

      // functions delegated to the decode stream
      function GetPosition: real;
      procedure SetPosition(Time: real);
      function ReadData(Buffer: PChar; BufSize: integer): integer;

      procedure GetFFTData(var data: TFFTData);
  end;

  TAudioMixerStream = class
    private
      activeStreams: TList;
      mixerBuffer: PChar;
      internalLock: PSDL_Mutex;

      _volume: integer;

      procedure Lock(); {$IFDEF HasInline}inline;{$ENDIF}
      procedure Unlock(); {$IFDEF HasInline}inline;{$ENDIF}

      function GetVolume(): integer;
      procedure SetVolume(volume: integer);
    public
      constructor Create();
      destructor Destroy(); override;
      procedure AddStream(stream: TAudioPlaybackStream);
      procedure RemoveStream(stream: TAudioPlaybackStream);
      function ReadData(Buffer: PChar; BufSize: integer): integer;

      property Volume: integer READ GetVolume WRITE SetVolume;
  end;

  TAudioPlayback_SoftMixer = class( TInterfacedObject, IAudioPlayback )
    private
      MusicStream: TSoftMixerPlaybackStream;
      MixerStream: TAudioMixerStream;
    protected
      FormatInfo: TAudioFormatInfo;

      function InitializeAudioPlaybackEngine(): boolean; virtual; abstract;
      function StartAudioPlaybackEngine(): boolean;      virtual; abstract;
      procedure StopAudioPlaybackEngine();               virtual; abstract;
      procedure AudioCallback(buffer: PChar; size: integer); {$IFDEF HasInline}inline;{$ENDIF}
    public
      function  GetName: String;                         virtual; abstract;

      function InitializePlayback(): boolean;
      destructor Destroy; override;

      function Load(const Filename: String): TSoftMixerPlaybackStream;

      procedure SetVolume(Volume: integer);
      procedure SetMusicVolume(Volume: integer);
      procedure SetLoop(Enabled: boolean);
      function Open(Filename: string): boolean; // true if succeed
      procedure Rewind;
      procedure SetPosition(Time: real);
      procedure Play;
      procedure Pause;

      procedure Stop;
      procedure Close;
      function Finished: boolean;
      function Length: real;
      function GetPosition: real;

      // Equalizer
      procedure GetFFTData(var data: TFFTData);

      // Interface for Visualizer
      function GetPCMData(var data: TPCMData): Cardinal;

      function GetMixer(): TAudioMixerStream; {$IFDEF HasInline}inline;{$ENDIF}
      function GetAudioFormatInfo(): TAudioFormatInfo;

      // Sounds
      function OpenSound(const Filename: String): TAudioPlaybackStream;
      procedure PlaySound(stream: TAudioPlaybackStream);
      procedure StopSound(stream: TAudioPlaybackStream);
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

constructor TAudioMixerStream.Create();
begin
  activeStreams := TList.Create;
  internalLock := SDL_CreateMutex();
  _volume := 100;
end;

destructor TAudioMixerStream.Destroy();
begin
  if assigned(mixerBuffer) then
    Freemem(mixerBuffer);
  activeStreams.Free;
  SDL_DestroyMutex(internalLock);
end;

procedure TAudioMixerStream.Lock();
begin
  SDL_mutexP(internalLock);
end;

procedure TAudioMixerStream.Unlock();
begin
  SDL_mutexV(internalLock);
end;

function TAudioMixerStream.GetVolume(): integer;
begin
  Lock();
  result := _volume;
  Unlock();
end;

procedure TAudioMixerStream.SetVolume(volume: integer);
begin
  Lock();
  _volume := volume;
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
  stream: TSoftMixerPlaybackStream;
  appVolume: single;
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

  // calc application volume
  // use _volume instead of Volume to prevent recursive locking
  appVolume := _volume / 100 * SDL_MIX_MAXVOLUME;

  needsPacking := false;

  // mix streams to one stream
  for i := 0 to activeStreams.Count-1 do
  begin
    if (activeStreams[i] = nil) then
    begin
      needsPacking := true;
      continue;
    end;

    stream := TSoftMixerPlaybackStream(activeStreams[i]);
    // fetch data from current stream
    size := stream.ReadData(mixerBuffer, BufSize);
    if (size > 0) then
    begin
      SDL_MixAudio(PUInt8(Buffer), PUInt8(mixerBuffer), size,
        Trunc(appVolume * stream.Volume / 100));
    end;
  end;

  // remove nil-pointers from list
  if (needsPacking) then
  begin
    activeStreams.Pack();
  end;

  Unlock();
end;


{ TSoftMixerPlaybackStream }

constructor TSoftMixerPlaybackStream.Create(Engine: TAudioPlayback_SoftMixer);
begin
  inherited Create();
  Self.Engine := Engine;
  internalLock := SDL_CreateMutex();
  Reset();
end;

destructor TSoftMixerPlaybackStream.Destroy();
begin
  Close();
  SDL_DestroyMutex(internalLock);
  inherited Destroy();
end;

procedure TSoftMixerPlaybackStream.Reset();
begin
  Stop();
  Loop := false;
  // TODO: use DecodeStream.Unref() instead of Free();
  FreeAndNil(DecodeStream);
  FreeMem(SampleBuffer);
  SampleBuffer := nil;
  SampleBufferPos := 0;
  BytesAvail := 0;
  _volume := 0;
end;

procedure TSoftMixerPlaybackStream.Lock();
begin
  SDL_mutexP(internalLock);
end;

procedure TSoftMixerPlaybackStream.Unlock();
begin
  SDL_mutexV(internalLock);
end;

class function TSoftMixerPlaybackStream.ConvertAudioFormatToSDL(fmt: TAudioSampleFormat): UInt16;
begin
  case fmt of
    asfU8:     Result := AUDIO_U8;
    asfS8:     Result := AUDIO_S8;
    asfU16LSB: Result := AUDIO_U16LSB;
    asfS16LSB: Result := AUDIO_S16LSB;
    asfU16MSB: Result := AUDIO_U16MSB;
    asfS16MSB: Result := AUDIO_S16MSB;
    asfU16:    Result := AUDIO_U16;
    asfS16:    Result := AUDIO_S16;
    else       Result := 0;
  end;
end;

function TSoftMixerPlaybackStream.InitFormatConversion(): boolean;
var
  err: integer;
  srcFormat: UInt16;
  dstFormat: UInt16;
  srcFormatInfo: TAudioFormatInfo;
  dstFormatInfo: TAudioFormatInfo;
begin
  Result := false;

  srcFormatInfo := DecodeStream.GetAudioFormatInfo();
  dstFormatInfo := Engine.GetAudioFormatInfo();

  srcFormat := ConvertAudioFormatToSDL(srcFormatInfo.Format);
  dstFormat := ConvertAudioFormatToSDL(dstFormatInfo.Format);

  if ((srcFormat = 0) or (dstFormat = 0)) then
  begin
    Log.LogError('Audio-format not supported by SDL', 'TSoftMixerPlaybackStream.InitFormatConversion');
    Exit;
  end;

  if (SDL_BuildAudioCVT(@cvt,
    srcFormat, srcFormatInfo.Channels, srcFormatInfo.SampleRate,
    dstFormat, dstFormatInfo.Channels, dstFormatInfo.SampleRate) = -1) then
  begin
    Log.LogError(SDL_GetError(), 'TSoftMixerPlaybackStream.InitFormatConversion');
    Exit;
  end;

  Result := true;
end;

function TSoftMixerPlaybackStream.SetDecodeStream(decodeStream: TAudioDecodeStream): boolean;
begin
  result := false;

  Reset();

  if not assigned(decodeStream) then
    Exit;
  Self.DecodeStream := decodeStream;
  if not InitFormatConversion() then
    Exit;

  _volume := 100;

  result := true;
end;

procedure TSoftMixerPlaybackStream.Close();
begin
  Reset();
end;

procedure TSoftMixerPlaybackStream.Play();
var
  mixer: TAudioMixerStream;
begin
  if (status <> ssPaused) then
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

procedure TSoftMixerPlaybackStream.Pause();
var
  mixer: TAudioMixerStream;
begin
  status := ssPaused;

  mixer := Engine.GetMixer();
  if (mixer <> nil) then
    mixer.RemoveStream(Self);
end;

procedure TSoftMixerPlaybackStream.Stop();
var
  mixer: TAudioMixerStream;
begin
  status := ssStopped;

  mixer := Engine.GetMixer();
  if (mixer <> nil) then
    mixer.RemoveStream(Self);
end;

function TSoftMixerPlaybackStream.IsLoaded(): boolean;
begin
  result := assigned(DecodeStream);
end;

function TSoftMixerPlaybackStream.GetLoop(): boolean;
begin
  result := Loop;
end;

procedure TSoftMixerPlaybackStream.SetLoop(Enabled: boolean);
begin
  Loop := Enabled;
end;

function TSoftMixerPlaybackStream.GetLength(): real;
begin
  if assigned(DecodeStream) then
    result := DecodeStream.Length
  else
    result := -1;
end;

function TSoftMixerPlaybackStream.GetStatus(): TStreamStatus;
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
function TSoftMixerPlaybackStream.ReadData(Buffer: PChar; BufSize: integer): integer;
var
  decodeBufSize: integer;
  sampleBufSize: integer;
  nBytesDecoded: integer;
  frameSize: integer;
  remFrameBytes: integer;
  copyCnt: integer;
  BytesNeeded: integer;
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
      Stop();

    // resample decoded data
    cvt.buf := PUint8(SampleBuffer);
    cvt.len := nBytesDecoded;
    if (SDL_ConvertAudio(@cvt) = -1) then
      Exit;

    SampleBufferCount := cvt.len_cvt;
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

procedure TSoftMixerPlaybackStream.GetFFTData(var data: TFFTData);
var
  i: integer;
  Frames: integer;
  DataIn: PSingleArray;
  AudioFormat: TAudioFormatInfo;
begin
  // only works with SInt16 and Float values at the moment
  AudioFormat := Engine.GetAudioFormatInfo();

  GetMem(DataIn, FFTSize * SizeOf(Single));

  Lock();
  // FIXME: We just use the first Frames frames, the others are ignored.
  // This is OK for the equalizer display but not if we want to use
  // this function for voice-analysis.
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

  WindowFunc(FFTSize, DataIn);
  PowerSpectrum(FFTSize, DataIn, @data);
  FreeMem(DataIn);

  // resize data to a 0..1 range
  for i := 0 to High(TFFTData) do
  begin
    // TODO: this might need some work
    data[i] := Sqrt(data[i]) / 100;
  end;
end;

(* TODO: libsamplerate support
function TSoftMixerPlaybackStream.ReadData(Buffer: PChar; BufSize: integer): integer;
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

function TSoftMixerPlaybackStream.GetPosition: real;
begin
  if assigned(DecodeStream) then
    result := DecodeStream.Position
  else
    result := -1;
end;

procedure TSoftMixerPlaybackStream.SetPosition(Time: real);
begin
  if assigned(DecodeStream) then
    DecodeStream.Position := Time;
end;

function TSoftMixerPlaybackStream.GetVolume(): integer;
begin
  result := _volume;
end;

procedure TSoftMixerPlaybackStream.SetVolume(volume: integer);
begin
  // clamp volume
  if (volume > 100) then
    _volume := 100
  else if (volume < 0) then
    _volume := 0
  else
    _volume := volume;
end;


{ TAudioPlayback_SoftMixer }

function TAudioPlayback_SoftMixer.InitializePlayback: boolean;
begin
  result := false;

  //Log.LogStatus('InitializePlayback', 'UAudioPlayback_SoftMixer');

  if(not InitializeAudioPlaybackEngine()) then
    Exit;

  MixerStream := TAudioMixerStream.Create;

  if(not StartAudioPlaybackEngine()) then
    Exit;

  result := true;
end;

destructor TAudioPlayback_SoftMixer.Destroy;
begin
  StopAudioPlaybackEngine();

  FreeAndNil(MusicStream);
  FreeAndNil(MixerStream);
  FreeAndNil(FormatInfo);

  inherited Destroy();
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

function TAudioPlayback_SoftMixer.Load(const Filename: String): TSoftMixerPlaybackStream;
var
  decodeStream: TAudioDecodeStream;
  playbackStream: TSoftMixerPlaybackStream;
begin
  Result := nil;

  decodeStream := AudioDecoder.Open(Filename);
  if not assigned(decodeStream) then
  begin
    Log.LogStatus('LoadSoundFromFile: Sound not found "' + Filename + '"', 'UAudioPlayback_SoftMixer');
    Exit;
  end;

  playbackStream := TSoftMixerPlaybackStream.Create(Self);
  if (not playbackStream.SetDecodeStream(decodeStream)) then
    Exit;

  result := playbackStream;
end;

procedure TAudioPlayback_SoftMixer.SetVolume(Volume: integer);
begin
  // sets volume only for this application
  MixerStream.Volume := Volume;
end;

procedure TAudioPlayback_SoftMixer.SetMusicVolume(Volume: Integer);
begin
  if assigned(MusicStream) then
    MusicStream.Volume := Volume;
end;

procedure TAudioPlayback_SoftMixer.SetLoop(Enabled: boolean);
begin
  if assigned(MusicStream) then
    MusicStream.SetLoop(Enabled);
end;

function TAudioPlayback_SoftMixer.Open(Filename: string): boolean;
var
  decodeStream: TAudioDecodeStream;
begin
  Result := false;

  // free old MusicStream
  MusicStream.Free();
  // and load new one
  MusicStream := Load(Filename);
  if not assigned(MusicStream) then
    Exit;

  //Set Max Volume
  SetMusicVolume(100);

  Result := true;
end;

procedure TAudioPlayback_SoftMixer.Rewind;
begin
  SetPosition(0);
end;

procedure TAudioPlayback_SoftMixer.SetPosition(Time: real);
begin
  if assigned(MusicStream) then
    MusicStream.SetPosition(Time);
end;

function TAudioPlayback_SoftMixer.GetPosition: real;
begin
  if assigned(MusicStream) then
    Result := MusicStream.GetPosition()
  else
    Result := -1;
end;

function TAudioPlayback_SoftMixer.Length: real;
begin
  if assigned(MusicStream) then
    Result := MusicStream.GetLength()
  else
    Result := -1;
end;

procedure TAudioPlayback_SoftMixer.Play;
begin
  if assigned(MusicStream) then
    MusicStream.Play();
end;

procedure TAudioPlayback_SoftMixer.Pause;
begin
  if assigned(MusicStream) then
    MusicStream.Pause();
end;

procedure TAudioPlayback_SoftMixer.Stop;
begin
  if assigned(MusicStream) then
    MusicStream.Stop();
end;

procedure TAudioPlayback_SoftMixer.Close;
begin
  if assigned(MusicStream) then
  begin
    MusicStream.Close();
  end;
end;

function TAudioPlayback_SoftMixer.Finished: boolean;
begin
  if assigned(MusicStream) then
    Result := (MusicStream.GetStatus() = ssStopped)
  else
    Result := true;
end;

//Equalizer
procedure TAudioPlayback_SoftMixer.GetFFTData(var data: TFFTData);
begin
  if assigned(MusicStream) then
    MusicStream.GetFFTData(data);
end;

// Interface for Visualizer
function TAudioPlayback_SoftMixer.GetPCMData(var data: TPCMData): Cardinal;
begin
  result := 0;
end;

function TAudioPlayback_SoftMixer.OpenSound(const Filename: String): TAudioPlaybackStream;
begin
  result := Load(Filename);
end;

procedure TAudioPlayback_SoftMixer.PlaySound(stream: TAudioPlaybackStream);
begin
  if assigned(stream) then
    stream.Play();
end;

procedure TAudioPlayback_SoftMixer.StopSound(stream: TAudioPlaybackStream);
begin
  if assigned(stream) then
    stream.Stop();
end;


end.
