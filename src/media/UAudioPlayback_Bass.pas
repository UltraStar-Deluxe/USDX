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
  Math,
  UIni,
  UMain,
  UMusic,
  UAudioPlaybackBase,
  UAudioCore_Bass,
  ULog,
  sdl,
  bass;

type
  PHDSP = ^HDSP;

type
  TBassPlaybackStream = class(TAudioPlaybackStream)
    private
      Handle: HSTREAM;
      NeedsRewind: boolean;
      PausedSeek: boolean; // true if a seek was performed in pause state

      procedure Reset();
      function IsEOF(): boolean;
    protected
      function GetLatency(): double;        override;
      function GetLoop(): boolean;          override;
      procedure SetLoop(Enabled: boolean);  override;
      function GetLength(): real;           override;
      function GetStatus(): TStreamStatus;  override;
      function GetVolume(): single;         override;
      procedure SetVolume(Volume: single);  override;
      function GetPosition: real;           override;
      procedure SetPosition(Time: real);    override;
    public
      constructor Create();
      destructor Destroy(); override;

      function Open(SourceStream: TAudioSourceStream): boolean; override;
      procedure Close();                    override;

      procedure Play();                     override;
      procedure Pause();                    override;
      procedure Stop();                     override;
      procedure FadeIn(Time: real; TargetVolume: single); override;

      procedure AddSoundEffect(Effect: TSoundEffect);    override;
      procedure RemoveSoundEffect(Effect: TSoundEffect); override;

      procedure GetFFTData(var Data: TFFTData);           override;
      function  GetPCMData(var Data: TPCMData): Cardinal; override;

      function GetAudioFormatInfo(): TAudioFormatInfo; override;

      function ReadData(Buffer: PChar; BufferSize: integer): integer;

      property EOF: boolean READ IsEOF;
  end;

const
  MAX_VOICE_DELAY = 0.020; // 20ms

type
  TBassVoiceStream = class(TAudioVoiceStream)
    private
      Handle: HSTREAM;
    public
      function Open(ChannelMap: integer; FormatInfo: TAudioFormatInfo): boolean; override;
      procedure Close(); override;

      procedure WriteData(Buffer: PChar; BufferSize: integer); override;
      function ReadData(Buffer: PChar; BufferSize: integer): integer; override;
      function IsEOF(): boolean; override;
      function IsError(): boolean; override;
  end;

type
  TAudioPlayback_Bass = class(TAudioPlaybackBase)
    private
      function EnumDevices(): boolean;
    protected
      function GetLatency(): double; override;
      function CreatePlaybackStream(): TAudioPlaybackStream; override;
    public
      function GetName: String; override;
      function InitializePlayback(): boolean; override;
      function FinalizePlayback: boolean; override;
      procedure SetAppVolume(Volume: single); override;
      function CreateVoiceStream(ChannelMap: integer; FormatInfo: TAudioFormatInfo): TAudioVoiceStream; override;
  end;

  TBassOutputDevice = class(TAudioOutputDevice)
    private
      BassDeviceID: DWORD; // DeviceID used by BASS
  end;

var
  BassCore: TAudioCore_Bass;


{ TBassPlaybackStream }

function PlaybackStreamHandler(handle: HSTREAM; buffer: Pointer; length: DWORD; user: Pointer): DWORD;
{$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
var
  PlaybackStream: TBassPlaybackStream;
  BytesRead: integer;
begin
  PlaybackStream := TBassPlaybackStream(user);
  if (not assigned (PlaybackStream)) then
  begin
    Result := BASS_STREAMPROC_END;
    Exit;
  end;

  BytesRead := PlaybackStream.ReadData(buffer, length);
  // check for errors
  if (BytesRead < 0) then
    Result := BASS_STREAMPROC_END
  // check for EOF
  else if (PlaybackStream.EOF) then
    Result := BytesRead or BASS_STREAMPROC_END
  // no error/EOF
  else
    Result := BytesRead;
end;

function TBassPlaybackStream.ReadData(Buffer: PChar; BufferSize: integer): integer;
var
  AdjustedSize: integer;
  RequestedSourceSize, SourceSize: integer;
  SkipCount: integer;
  SourceFormatInfo: TAudioFormatInfo;
  FrameSize: integer;
  PadFrame: PChar;
  //Info: BASS_INFO;
  //Latency: double;
begin
  Result := -1;

  if (not assigned(SourceStream)) then
    Exit;

  // sanity check
  if (BufferSize = 0) then
  begin
    Result := 0;
    Exit;
  end;

  SourceFormatInfo := SourceStream.GetAudioFormatInfo();
  FrameSize := SourceFormatInfo.FrameSize;

  // check how much data to fetch to be in synch
  AdjustedSize := Synchronize(BufferSize, SourceFormatInfo);

  // skip data if we are too far behind
  SkipCount := AdjustedSize - BufferSize;
  while (SkipCount > 0) do
  begin
    RequestedSourceSize := Min(SkipCount, BufferSize);
    SourceSize := SourceStream.ReadData(Buffer, RequestedSourceSize);
    // if an error or EOF occured stop skipping and handle error/EOF with the next ReadData()
    if (SourceSize <= 0) then
      break;
    Dec(SkipCount, SourceSize);
  end;

  // get source data (e.g. from a decoder)
  RequestedSourceSize := Min(AdjustedSize, BufferSize);
  SourceSize := SourceStream.ReadData(Buffer, RequestedSourceSize);
  if (SourceSize < 0) then
    Exit;

  // set preliminary result
  Result := SourceSize;

  // if we are to far ahead, fill output-buffer with last frame of source data
  // Note that AdjustedSize is used instead of SourceSize as the SourceSize might
  // be less than expected because of errors etc.
  if (AdjustedSize < BufferSize) then
  begin
    // use either the last frame for padding or fill with zero
    if (SourceSize >= FrameSize) then
      PadFrame := @Buffer[SourceSize-FrameSize]
    else
      PadFrame := nil;

    FillBufferWithFrame(@Buffer[SourceSize], BufferSize - SourceSize,
                        PadFrame, FrameSize);
    Result := BufferSize;
  end;
end;

constructor TBassPlaybackStream.Create();
begin
  inherited;
  Reset();
end;

destructor TBassPlaybackStream.Destroy();
begin
  Close();
  inherited;
end;

function TBassPlaybackStream.Open(SourceStream: TAudioSourceStream): boolean;
var
  FormatInfo: TAudioFormatInfo;
  FormatFlags: DWORD;
begin
  Result := false;

  // close previous stream and reset state
  Reset();

  // sanity check if stream is valid
  if not assigned(SourceStream) then
    Exit;

  Self.SourceStream := SourceStream;
  FormatInfo := SourceStream.GetAudioFormatInfo();
  if (not BassCore.ConvertAudioFormatToBASSFlags(FormatInfo.Format, FormatFlags)) then
  begin
    Log.LogError('Unhandled sample-format', 'TBassPlaybackStream.Open');
    Exit;
  end;

  // create matching playback stream
  Handle := BASS_StreamCreate(Round(FormatInfo.SampleRate), FormatInfo.Channels, formatFlags,
      @PlaybackStreamHandler, Self);
  if (Handle = 0) then
  begin
    Log.LogError('BASS_StreamCreate failed: ' + BassCore.ErrorGetString(BASS_ErrorGetCode()),
                 'TBassPlaybackStream.Open');
    Exit;
  end;

  Result := true;
end;

procedure TBassPlaybackStream.Close();
begin
  // stop and free stream
  if (Handle <> 0) then
  begin
    Bass_StreamFree(Handle);
    Handle := 0;
  end;

  // Note: PerformOnClose must be called before SourceStream is invalidated
  PerformOnClose();
  // unset source-stream
  SourceStream := nil;
end;

procedure TBassPlaybackStream.Reset();
begin
  Close();
  NeedsRewind := false;
  PausedSeek := false;
end;

procedure TBassPlaybackStream.Play();
var
  NeedsFlush: boolean;
begin
  if (not assigned(SourceStream)) then
    Exit;

  NeedsFlush := true;

  if (BASS_ChannelIsActive(Handle) = BASS_ACTIVE_PAUSED) then
  begin
    // only paused (and not seeked while paused) streams are not flushed
    if (not PausedSeek) then
      NeedsFlush := false;
    // paused streams do not need a rewind
    NeedsRewind := false;
  end;

  // rewind if necessary. Cases that require no rewind are:
  // - stream was created and never played
  // - stream was paused and is resumed now
  // - stream was stopped and set to a new position already
  if (NeedsRewind) then
    SourceStream.Position := 0;

  NeedsRewind := true;
  PausedSeek := false;

  // start playing and flush buffers on rewind
  BASS_ChannelPlay(Handle, NeedsFlush);
end;

procedure TBassPlaybackStream.FadeIn(Time: real; TargetVolume: single);
begin
  // start stream
  Play();
  // start fade-in: slide from fadeStart- to fadeEnd-volume in FadeInTime
  BASS_ChannelSlideAttribute(Handle, BASS_ATTRIB_VOL, TargetVolume, Trunc(Time * 1000));
end;

procedure TBassPlaybackStream.Pause();
begin
  BASS_ChannelPause(Handle);
end;

procedure TBassPlaybackStream.Stop();
begin
  BASS_ChannelStop(Handle);
end;

function TBassPlaybackStream.IsEOF(): boolean;
begin
  if (assigned(SourceStream)) then
    Result := SourceStream.EOF
  else
    Result := true;
end;

function TBassPlaybackStream.GetLatency(): double;
begin
  // TODO: should we consider output latency for synching (needs BASS_DEVICE_LATENCY)?
  //if (BASS_GetInfo(Info)) then
  //  Latency := Info.latency / 1000
  //else
  //  Latency := 0;
  Result := 0; 
end;

function TBassPlaybackStream.GetVolume(): single;
var
  lVolume: single;
begin
  if (not BASS_ChannelGetAttribute(Handle, BASS_ATTRIB_VOL, lVolume)) then
  begin
    Log.LogError('BASS_ChannelGetAttribute: ' + BassCore.ErrorGetString(),
      'TBassPlaybackStream.GetVolume');
    Result := 0;
    Exit;
  end;
  Result := Round(lVolume);
end;

procedure TBassPlaybackStream.SetVolume(Volume: single);
begin
  // clamp volume
  if Volume < 0 then
    Volume := 0;
  if Volume > 1.0 then
    Volume := 1.0;
  // set volume
  BASS_ChannelSetAttribute(Handle, BASS_ATTRIB_VOL, Volume);
end;

function TBassPlaybackStream.GetPosition: real;
var
  BufferPosByte: QWORD;
  BufferPosSec: double;
begin
  if assigned(SourceStream) then
  begin
    BufferPosByte := BASS_ChannelGetData(Handle, nil, BASS_DATA_AVAILABLE);
    BufferPosSec := BASS_ChannelBytes2Seconds(Handle, BufferPosByte);
    // decrease the decoding position by the amount buffered (and hence not played)
    // in the BASS playback stream.
    Result := SourceStream.Position - BufferPosSec;
  end
  else
  begin
    Result := -1;
  end;
end;

procedure TBassPlaybackStream.SetPosition(Time: real);
var
  ChannelState: DWORD;
begin
  if assigned(SourceStream) then
  begin
    ChannelState := BASS_ChannelIsActive(Handle);
    if (ChannelState = BASS_ACTIVE_STOPPED) then
    begin
      // if the stream is stopped, do not rewind when the stream is played next time
      NeedsRewind := false
    end
    else if (ChannelState = BASS_ACTIVE_PAUSED) then
    begin
      // buffers must be flushed if in paused state but there is no
      // BASS_ChannelFlush() function so we have to use BASS_ChannelPlay() called in Play().
      PausedSeek := true;
    end;

    // set new position
    SourceStream.Position := Time;
  end;
end;

function TBassPlaybackStream.GetLength(): real;
begin
  if assigned(SourceStream) then
    Result := SourceStream.Length
  else
    Result := -1;
end;

function TBassPlaybackStream.GetStatus(): TStreamStatus;
var
  State: DWORD;
begin
  State := BASS_ChannelIsActive(Handle);
  case State of
    BASS_ACTIVE_PLAYING,
    BASS_ACTIVE_STALLED:
      Result := ssPlaying;
    BASS_ACTIVE_PAUSED:
      Result := ssPaused;
    BASS_ACTIVE_STOPPED:
      Result := ssStopped;
    else
    begin
      Log.LogError('Unknown status', 'TBassPlaybackStream.GetStatus');
      Result := ssStopped;
    end;
  end;
end;

function TBassPlaybackStream.GetLoop(): boolean;
begin
  if assigned(SourceStream) then
    Result := SourceStream.Loop
  else
    Result := false;
end;

procedure TBassPlaybackStream.SetLoop(Enabled: boolean);
begin
  if assigned(SourceStream) then
    SourceStream.Loop := Enabled;
end;

procedure DSPProcHandler(handle: HDSP; channel: DWORD; buffer: Pointer; length: DWORD; user: Pointer);
{$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
var
  Effect: TSoundEffect;
begin
  Effect := TSoundEffect(user);
  if assigned(Effect) then
    Effect.Callback(buffer, length);
end;

procedure TBassPlaybackStream.AddSoundEffect(Effect: TSoundEffect);
var
  DspHandle: HDSP;
begin
  if assigned(Effect.engineData) then
  begin
    Log.LogError('TSoundEffect.engineData already set', 'TBassPlaybackStream.AddSoundEffect');
    Exit;
  end;

  DspHandle := BASS_ChannelSetDSP(Handle, @DSPProcHandler, Effect, 0);
  if (DspHandle = 0) then
  begin
    Log.LogError(BassCore.ErrorGetString(), 'TBassPlaybackStream.AddSoundEffect');
    Exit;
  end;

  GetMem(Effect.EngineData, SizeOf(HDSP));
  PHDSP(Effect.EngineData)^ := DspHandle;
end;

procedure TBassPlaybackStream.RemoveSoundEffect(Effect: TSoundEffect);
begin
  if not assigned(Effect.EngineData) then
  begin
    Log.LogError('TSoundEffect.engineData invalid', 'TBassPlaybackStream.RemoveSoundEffect');
    Exit;
  end;

  if not BASS_ChannelRemoveDSP(Handle, PHDSP(Effect.EngineData)^) then
  begin
    Log.LogError(BassCore.ErrorGetString(), 'TBassPlaybackStream.RemoveSoundEffect');
    Exit;
  end;

  FreeMem(Effect.EngineData);
  Effect.EngineData := nil;
end;

procedure TBassPlaybackStream.GetFFTData(var Data: TFFTData);
begin
  // get FFT channel data (Mono, FFT512 -> 256 values)
  BASS_ChannelGetData(Handle, @Data, BASS_DATA_FFT512);
end;

{*
 * Copies interleaved PCM SInt16 stereo samples into data.
 * Returns the number of frames
 *}
function TBassPlaybackStream.GetPCMData(var Data: TPCMData): Cardinal;
var
  Info: BASS_CHANNELINFO;
  nBytes: DWORD;
begin
  Result := 0;

  FillChar(Data, SizeOf(TPCMData), 0);

  // no support for non-stereo files at the moment
  BASS_ChannelGetInfo(Handle, Info);
  if (Info.chans <> 2) then
    Exit;

  nBytes := BASS_ChannelGetData(Handle, @Data, SizeOf(TPCMData));
  if(nBytes <= 0) then
    Result := 0
  else
    Result := nBytes div SizeOf(TPCMStereoSample);
end;

function TBassPlaybackStream.GetAudioFormatInfo(): TAudioFormatInfo;
begin
  if assigned(SourceStream) then
    Result := SourceStream.GetAudioFormatInfo()
  else
    Result := nil;
end;


{ TBassVoiceStream }

function TBassVoiceStream.Open(ChannelMap: integer; FormatInfo: TAudioFormatInfo): boolean;
var
  Flags: DWORD;
begin
  Result := false;

  Close();

  if (not inherited Open(ChannelMap, FormatInfo)) then
    Exit;

  // get channel flags
  BassCore.ConvertAudioFormatToBASSFlags(FormatInfo.Format, Flags);

  (*
  // distribute the mics equally to both speakers
  if ((ChannelMap and CHANNELMAP_LEFT) <> 0) then
    Flags := Flags or BASS_SPEAKER_FRONTLEFT;
  if ((ChannelMap and CHANNELMAP_RIGHT) <> 0) then
    Flags := Flags or BASS_SPEAKER_FRONTRIGHT;
  *)
  
  // create the channel
  Handle := BASS_StreamCreate(Round(FormatInfo.SampleRate), 1, Flags, STREAMPROC_PUSH, nil);

  // start the channel
  BASS_ChannelPlay(Handle, true);

  Result := true;
end;

procedure TBassVoiceStream.Close();
begin
  if (Handle <> 0) then
  begin
    BASS_ChannelStop(Handle);
    BASS_StreamFree(Handle);
  end;
  inherited Close();
end;

procedure TBassVoiceStream.WriteData(Buffer: PChar; BufferSize: integer);
var QueueSize: DWORD;
begin
  if ((Handle <> 0) and (BufferSize > 0)) then
  begin
    // query the queue size (normally 0)
    QueueSize := BASS_StreamPutData(Handle, nil, 0);
    // flush the buffer if the delay would be too high
    if (QueueSize > MAX_VOICE_DELAY * FormatInfo.BytesPerSec) then
      BASS_ChannelPlay(Handle, true);
    // send new data to playback buffer
    BASS_StreamPutData(Handle, Buffer, BufferSize);
  end;
end;

// Note: we do not need the read-function for the BASS implementation
function TBassVoiceStream.ReadData(Buffer: PChar; BufferSize: integer): integer;
begin
  Result := -1;
end;

function TBassVoiceStream.IsEOF(): boolean;
begin
  Result := false;
end;

function TBassVoiceStream.IsError(): boolean;
begin
  Result := false;
end;


{ TAudioPlayback_Bass }

function TAudioPlayback_Bass.GetName: String;
begin
  Result := 'BASS_Playback';
end;

function TAudioPlayback_Bass.EnumDevices(): boolean;
var
  BassDeviceID: DWORD;
  DeviceIndex: integer;
  Device: TBassOutputDevice;
  DeviceInfo: BASS_DEVICEINFO;
begin
  Result := true;

  ClearOutputDeviceList();

  // skip "no sound"-device (ID = 0)
  BassDeviceID := 1;

  while (true) do
  begin
    // check for device
    if (not BASS_GetDeviceInfo(BassDeviceID, DeviceInfo)) then
      Break;

    // set device info
    Device := TBassOutputDevice.Create();
    Device.Name := DeviceInfo.name;
    Device.BassDeviceID := BassDeviceID;

    // add device to list
    SetLength(OutputDeviceList, BassDeviceID);
    OutputDeviceList[BassDeviceID-1] := Device;

    Inc(BassDeviceID);
  end;
end;

function TAudioPlayback_Bass.InitializePlayback(): boolean;
begin
  result := false;

  BassCore := TAudioCore_Bass.GetInstance();

  EnumDevices();

  //Log.BenchmarkStart(4);
  //Log.LogStatus('Initializing Playback Subsystem', 'Music Initialize');

  // TODO: use BASS_DEVICE_LATENCY to determine the latency
  if not BASS_Init(-1, 44100, 0, 0, nil) then
  begin
    Log.LogError('Could not initialize BASS', 'TAudioPlayback_Bass.InitializePlayback');
    Exit;
  end;

  //Log.BenchmarkEnd(4); Log.LogBenchmark('--> Bass Init', 4);

  // config playing buffer
  //BASS_SetConfig(BASS_CONFIG_UPDATEPERIOD, 10);
  //BASS_SetConfig(BASS_CONFIG_BUFFER, 100);

  result := true;
end;

function TAudioPlayback_Bass.FinalizePlayback(): boolean;
begin
  Close;
  BASS_Free;
  inherited FinalizePlayback();
  Result := true;
end;

function TAudioPlayback_Bass.CreatePlaybackStream(): TAudioPlaybackStream;
begin
  Result := TBassPlaybackStream.Create();
end;

procedure TAudioPlayback_Bass.SetAppVolume(Volume: single);
begin
  // set volume for this application (ranges from 0..10000 since BASS 2.4)
  BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, Round(Volume*10000));
end;

function TAudioPlayback_Bass.CreateVoiceStream(ChannelMap: integer; FormatInfo: TAudioFormatInfo): TAudioVoiceStream;
var
  VoiceStream: TAudioVoiceStream;
begin
  Result := nil;

  VoiceStream := TBassVoiceStream.Create();
  if (not VoiceStream.Open(ChannelMap, FormatInfo)) then
  begin
    VoiceStream.Free;
    Exit;
  end;

  Result := VoiceStream;
end;

function TAudioPlayback_Bass.GetLatency(): double;
begin
  Result := 0;
end;


initialization
  MediaManager.Add(TAudioPlayback_Bass.Create);

end.
