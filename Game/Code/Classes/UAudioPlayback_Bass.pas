unit UAudioPlayback_Bass;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

implementation

uses
  Classes,
  SysUtils,
  UIni,
  UMain,
  UMusic,
  UAudioPlaybackBase,
  UAudioCore_Bass,
  ULog,
  bass;

type
  PHDSP = ^HDSP;

type
  // Playback-stream decoded internally by BASS
  TBassPlaybackStream = class(TAudioPlaybackStream)
    private
      Handle: HSTREAM;
    public
      constructor Create(stream: HSTREAM);
      destructor Destroy(); override;

      procedure Reset();

      procedure Play();                     override;
      procedure Pause();                    override;
      procedure Stop();                     override;
      procedure FadeIn(Time: real; TargetVolume: integer); override;

      procedure Close();                    override;

      function GetLoop(): boolean;          override;
      procedure SetLoop(Enabled: boolean);  override;
      function GetLength(): real;           override;
      function GetStatus(): TStreamStatus;  override;
      function GetVolume(): integer;        override;
      procedure SetVolume(volume: integer); override;

      procedure AddSoundEffect(effect: TSoundEffect);    override;
      procedure RemoveSoundEffect(effect: TSoundEffect); override;

      function GetPosition: real;           override;
      procedure SetPosition(Time: real);    override;

      procedure GetFFTData(var data: TFFTData);           override;
      function  GetPCMData(var data: TPCMData): Cardinal; override;
  end;

  // Playback-stream decoded by an external decoder e.g. FFmpeg
  TBassExtDecoderPlaybackStream = class(TBassPlaybackStream)
    private
      DecodeStream: TAudioDecodeStream;
    public
      procedure Stop();                     override;
      procedure Close();                    override;
      function GetLength(): real;           override;
      function GetPosition: real;           override;
      procedure SetPosition(Time: real);    override;

      function SetDecodeStream(decodeStream: TAudioDecodeStream): boolean;
  end;

type
  TAudioPlayback_Bass = class(TAudioPlaybackBase)
    private
      function EnumDevices(): boolean;
    protected
      function OpenStream(const Filename: string): TAudioPlaybackStream; override;
    public
      function GetName: String; override;
      function InitializePlayback(): boolean; override;
      function FinalizePlayback: boolean; override;
      procedure SetAppVolume(Volume: integer); override;
  end;

  TBassOutputDevice = class(TAudioOutputDevice)
    private
      BassDeviceID: DWORD; // DeviceID used by BASS
  end;

var
  singleton_AudioPlaybackBass : IAudioPlayback;


{ TBassPlaybackStream }

constructor TBassPlaybackStream.Create(stream: HSTREAM);
begin
  inherited Create();
  Reset();
  Handle := stream;
end;

destructor TBassPlaybackStream.Destroy();
begin
  Close();
  inherited;
end;

procedure TBassPlaybackStream.Reset();
begin
  if (Handle <> 0) then
  begin
    Bass_StreamFree(Handle);
  end;
  Handle := 0;
end;

procedure TBassPlaybackStream.Play();
var
  restart: boolean;
begin
  if (BASS_ChannelIsActive(Handle) = BASS_ACTIVE_PAUSED) then
    restart := false // resume from last position
  else
    restart := true; // start from the beginning

  BASS_ChannelPlay(Handle, restart);
end;

procedure TBassPlaybackStream.FadeIn(Time: real; TargetVolume: integer);
begin
  // start stream
  BASS_ChannelPlay(Handle, true);

  // start fade-in: slide from fadeStart- to fadeEnd-volume in FadeInTime
  BASS_ChannelSlideAttributes(Handle, -1, TargetVolume, -101, Trunc(Time * 1000));
end;

procedure TBassPlaybackStream.Pause();
begin
  BASS_ChannelPause(Handle);
end;

procedure TBassPlaybackStream.Stop();
begin
  BASS_ChannelStop(Handle);
end;

procedure TBassPlaybackStream.Close();
begin
  Reset();
end;

function TBassPlaybackStream.GetVolume(): integer;
var
  volume: cardinal;
begin
  BASS_ChannelGetAttributes(Handle, PCardinal(nil)^, volume, PInteger(nil)^);
  Result := volume;
end;

procedure TBassPlaybackStream.SetVolume(volume: integer);
begin
  // clamp volume
  if volume < 0 then
    volume := 0;
  if volume > 100 then
    volume := 100;
  // set volume
  BASS_ChannelSetAttributes(Handle, -1, volume, -101);
end;

function TBassPlaybackStream.GetPosition: real;
var
  bytes:    integer;
begin
  bytes  := BASS_ChannelGetPosition(Handle);
  Result := BASS_ChannelBytes2Seconds(Handle, bytes);
end;

procedure TBassPlaybackStream.SetPosition(Time: real);
var
  bytes:    integer;
begin
  bytes := BASS_ChannelSeconds2Bytes(Handle, Time);
  BASS_ChannelSetPosition(Handle, bytes);
end;

function TBassPlaybackStream.GetLength(): real;
var
  bytes: integer;
begin
  bytes  := BASS_ChannelGetLength(Handle);
  Result := BASS_ChannelBytes2Seconds(Handle, bytes);
end;

function TBassPlaybackStream.GetStatus(): TStreamStatus;
var
  state: DWORD;
begin
  state := BASS_ChannelIsActive(Handle);
  case state of
    BASS_ACTIVE_PLAYING:
      result := ssPlaying;
    BASS_ACTIVE_PAUSED:
      result := ssPaused;
    BASS_ACTIVE_STALLED:
      result := ssBlocked;
    BASS_ACTIVE_STOPPED:
      result := ssStopped;
    else
      result := ssUnknown;
  end;
end;

function TBassPlaybackStream.GetLoop(): boolean;
var
  info: BASS_CHANNELINFO;
begin
  if not BASS_ChannelGetInfo(Handle, info) then
  begin
    Log.LogError('BASS_ChannelGetInfo: ' + TAudioCore_Bass.ErrorGetString(), 'TBassPlaybackStream.GetLoop');
    Result := false;
    Exit;
  end;
  Result := (info.flags and BASS_SAMPLE_LOOP) <> 0;
end;

procedure TBassPlaybackStream.SetLoop(Enabled: boolean);
var
  info: BASS_CHANNELINFO;
begin
  // retrieve old flag-bits
  if not BASS_ChannelGetInfo(Handle, info) then
  begin
    Log.LogError('BASS_ChannelGetInfo:' + TAudioCore_Bass.ErrorGetString(), 'TBassPlaybackStream.SetLoop');
    Exit;
  end;

  // set/unset loop-flag
  if (Enabled) then
    info.flags := info.flags or BASS_SAMPLE_LOOP
  else
    info.flags := info.flags and not BASS_SAMPLE_LOOP;

  // set new flag-bits
  if not BASS_ChannelSetFlags(Handle, info.flags) then
  begin
    Log.LogError('BASS_ChannelSetFlags: ' + TAudioCore_Bass.ErrorGetString(), 'TBassPlaybackStream.SetLoop');
    Exit;
  end;
end;

procedure DSPProcHandler(handle: HDSP; channel: DWORD; buffer: Pointer; length: DWORD; user: DWORD); stdcall;
var
  effect: TSoundEffect;
begin
  effect := TSoundEffect(user);
  if assigned(effect) then
    effect.Callback(buffer, length);
end;

procedure TBassPlaybackStream.AddSoundEffect(effect: TSoundEffect);
var
  dspHandle: HDSP;
begin
  if assigned(effect.engineData) then
  begin
    Log.LogError('TSoundEffect.engineData already set', 'TBassPlaybackStream.AddSoundEffect');
    Exit;
  end;

  // FIXME: casting of a pointer to Uint32 will fail on 64bit systems
  dspHandle := BASS_ChannelSetDSP(Handle, @DSPProcHandler, DWORD(effect), 0);
  if (dspHandle = 0) then
  begin
    Log.LogError(TAudioCore_Bass.ErrorGetString(), 'TBassPlaybackStream.AddSoundEffect');
    Exit;
  end;

  GetMem(effect.engineData, SizeOf(HDSP));
  PHDSP(effect.engineData)^ := dspHandle;
end;

procedure TBassPlaybackStream.RemoveSoundEffect(effect: TSoundEffect);
begin
  if not assigned(effect.EngineData) then
  begin
    Log.LogError('TSoundEffect.engineData invalid', 'TBassPlaybackStream.RemoveSoundEffect');
    Exit;
  end;

  if not BASS_ChannelRemoveDSP(Handle, PHDSP(effect.EngineData)^) then
  begin
    Log.LogError(TAudioCore_Bass.ErrorGetString(), 'TBassPlaybackStream.RemoveSoundEffect');
    Exit;
  end;

  FreeMem(effect.engineData);
  effect.engineData := nil;
end;

procedure TBassPlaybackStream.GetFFTData(var data: TFFTData);
begin
  // Get Channel Data Mono and 256 Values
  BASS_ChannelGetData(Handle, @data, BASS_DATA_FFT512);
end;

{*
 * Copies interleaved PCM SInt16 stereo samples into data.
 * Returns the number of frames
 *}
function TBassPlaybackStream.GetPCMData(var data: TPCMData): Cardinal;
var
  info: BASS_CHANNELINFO;
  nBytes: DWORD;
begin
  Result := 0;

  // Get Channel Data Mono and 256 Values
  BASS_ChannelGetInfo(Handle, info);
  FillChar(data, sizeof(TPCMData), 0);

  // no support for non-stereo files at the moment
  if (info.chans <> 2) then
    Exit;

  nBytes := BASS_ChannelGetData(Handle, @data, sizeof(TPCMData));
  if(nBytes <= 0) then
    result := 0
  else
    result := nBytes div sizeof(TPCMStereoSample);
end;


{ TBassExtDecoderPlaybackStream }

procedure TBassExtDecoderPlaybackStream.Stop();
begin
  inherited;
  // rewind
  if assigned(DecodeStream) then
    DecodeStream.Position := 0;
end;

procedure TBassExtDecoderPlaybackStream.Close();
begin
  // wake-up waiting audio-callback threads in the ReadData()-function
  if assigned(decodeStream) then
    DecodeStream.Close();
  // stop audio-callback on this stream
  inherited;
  // free decoder-data
  FreeAndNil(DecodeStream);
end;

function TBassExtDecoderPlaybackStream.GetLength(): real;
begin
  if assigned(DecodeStream) then
    result := DecodeStream.Length
  else
    result := -1;
end;

function TBassExtDecoderPlaybackStream.GetPosition: real;
begin
  if assigned(DecodeStream) then
    result := DecodeStream.Position
  else
    result := -1;
end;

procedure TBassExtDecoderPlaybackStream.SetPosition(Time: real);
begin
  if assigned(DecodeStream) then
    DecodeStream.Position := Time;
end;

function TBassExtDecoderPlaybackStream.SetDecodeStream(decodeStream: TAudioDecodeStream): boolean;
begin
  result := false;

  BASS_ChannelStop(Handle);

  if not assigned(decodeStream) then
    Exit;
  Self.DecodeStream := decodeStream;

  result := true;
end;


{ TAudioPlayback_Bass }

function  TAudioPlayback_Bass.GetName: String;
begin
  result := 'BASS_Playback';
end;

function TAudioPlayback_Bass.EnumDevices(): boolean;
var
  BassDeviceID: DWORD;
  DeviceIndex: integer;
  Device: TBassOutputDevice;
  Description: PChar;
begin
  ClearOutputDeviceList();

  // skip "no sound"-device (ID = 0)
  BassDeviceID := 1;

  while true do
  begin
    // Check for device
    Description := BASS_GetDeviceDescription(BassDeviceID);
    if (Description = nil) then
      break;

    // Set device info
    Device := TBassOutputDevice.Create();
    Device.Name := Description;
    Device.BassDeviceID := BassDeviceID;

    // Add device to list
    SetLength(OutputDeviceList, BassDeviceID);
    OutputDeviceList[BassDeviceID-1] := Device;

    Inc(BassDeviceID);
  end;
end;

function TAudioPlayback_Bass.InitializePlayback(): boolean;
var
  Pet:  integer;
  S:    integer;
begin
  result := false;

  EnumDevices();

  //Log.BenchmarkStart(4);
  //Log.LogStatus('Initializing Playback Subsystem', 'Music Initialize');

  if not BASS_Init(1, 44100, 0, 0, nil) then
  begin
    Log.LogError('Could not initialize BASS', 'Error');
    Exit;
  end;

  //Log.BenchmarkEnd(4); Log.LogBenchmark('--> Bass Init', 4);

  // config playing buffer
  //BASS_SetConfig(BASS_CONFIG_UPDATEPERIOD, 10);
  //BASS_SetConfig(BASS_CONFIG_BUFFER, 100);

  result := true;
end;

function DecodeStreamHandler(handle: HSTREAM; buffer: Pointer; length: DWORD; user: DWORD): DWORD; stdcall;
var
  decodeStream: TAudioDecodeStream;
  bytes: integer;
begin
  decodeStream := TAudioDecodeStream(user);
  bytes := decodeStream.ReadData(buffer, length);
  // handle errors
  if (bytes < 0) then
    Result := BASS_STREAMPROC_END
  // handle EOF
  else if (DecodeStream.EOF) then
    Result := bytes or BASS_STREAMPROC_END
  else
    Result := bytes;
end;

function TAudioPlayback_Bass.FinalizePlayback(): boolean;
begin
  Close;
  BASS_Free;
  inherited FinalizePlayback();
  Result := true;
end;

function TAudioPlayback_Bass.OpenStream(const Filename: string): TAudioPlaybackStream;
var
  L: Integer;
  stream: HSTREAM;
  playbackStream: TBassExtDecoderPlaybackStream;
  decodeStream: TAudioDecodeStream;
  formatInfo: TAudioFormatInfo;
  formatFlags: DWORD;
  channelInfo: BASS_CHANNELINFO;
  fileExt: string;
begin
  Result := nil;

  //Log.LogStatus('Loading Sound: "' + Filename + '"', 'LoadSoundFromFile');
  stream := BASS_StreamCreateFile(False, PChar(Filename), 0, 0, 0);

  // check if BASS opened some erroneously recognized file-formats
  if (stream <> 0) then
  begin
    if BASS_ChannelGetInfo(stream, channelInfo) then
    begin
      fileExt := ExtractFileExt(Filename);
      // BASS opens FLV-files although it cannot handle them
      if ((fileExt = '.flv') and (channelInfo.ctype = BASS_CTYPE_STREAM_MP1)) then
      begin
        BASS_StreamFree(stream);
        stream := 0;
      end;
    end;
  end;

  // Check if BASS can handle the format or try another decoder otherwise
  if (stream <> 0) then
  begin
    Result := TBassPlaybackStream.Create(stream);
  end
  else
  begin
    if (AudioDecoder = nil) then
    begin
      Log.LogError('Failed to open "' + Filename + '", ' +
                   TAudioCore_Bass.ErrorGetString(BASS_ErrorGetCode()), 'TAudioPlayback_Bass.Load');
      Exit;
    end;

    decodeStream := AudioDecoder.Open(Filename);
    if not assigned(decodeStream) then
    begin
      Log.LogStatus('Sound not found "' + Filename + '"', 'TAudioPlayback_Bass.Load');
      Exit;
    end;

    formatInfo := decodeStream.GetAudioFormatInfo();
    if (not TAudioCore_Bass.ConvertAudioFormatToBASSFlags(formatInfo.Format, formatFlags)) then
    begin
      Log.LogError('Unhandled sample-format in "' + Filename + '"', 'TAudioPlayback_Bass.Load');
      FreeAndNil(decodeStream);
      Exit;
    end;

    // FIXME: casting of a pointer to Uint32 will fail on 64bit systems
    stream := BASS_StreamCreate(Round(formatInfo.SampleRate), formatInfo.Channels, formatFlags,
        @DecodeStreamHandler, DWORD(decodeStream));
    if (stream = 0) then
    begin
      Log.LogError('Failed to open "' + Filename + '", ' +
                   TAudioCore_Bass.ErrorGetString(BASS_ErrorGetCode()), 'TAudioPlayback_Bass.Load');
      FreeAndNil(decodeStream);
      Exit;
    end;

    playbackStream := TBassExtDecoderPlaybackStream.Create(stream);
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

    Result := playbackStream;
  end;
end;

procedure TAudioPlayback_Bass.SetAppVolume(Volume: integer);
begin
  // Sets Volume only for this Application
  BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, Volume);
end;


initialization
  singleton_AudioPlaybackBass := TAudioPlayback_Bass.create();
  AudioManager.add( singleton_AudioPlaybackBass );

finalization
  AudioManager.Remove( singleton_AudioPlaybackBass );

end.
