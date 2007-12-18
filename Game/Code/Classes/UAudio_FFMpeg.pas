unit UAudio_FFMpeg;

(*******************************************************************************

This unit is primarily based upon -
    http://www.dranger.com/ffmpeg/ffmpegtutorial_all.html
    
    and tutorial03.c

    http://www.inb.uni-luebeck.de/~boehme/using_libavcodec.html

*******************************************************************************)

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}


uses Classes,
     {$IFDEF win32}
     windows,
     {$ENDIF}
     Messages,
     SysUtils,
     {$IFNDEF FPC}
     Forms,
     {$ENDIF}
     SDL,       // Used for Audio output Interface
     avcodec,   // FFMpeg Audio file decoding
     avformat,
     avutil,
     ULog,
     UMusic;

implementation

uses
     {$IFDEF LAZARUS}
     lclintf,
       {$ifndef win32}
       libc, // not available in win32
       {$endif}
     {$ENDIF}
     portaudio,
     UIni,
     UMain,
     UThemes;


type
  PPacketQueue = ^TPacketQueue;
  TPacketQueue = class
    private
      firstPkt,
      lastPkt    : PAVPacketList;
      nbPackets  : integer;
      size       : integer;
      mutex      : PSDL_Mutex;
      cond       : PSDL_Cond;
      quit       : boolean;

    public
      constructor Create();

      function Put(pkt : PAVPacket): integer;
      function Get(var pkt: TAVPacket; block: boolean): integer;
  end;

type
  TStreamStatus = (sNotReady, sStopped, sPlaying, sSeeking, sPaused, sOpen);

const
  StreamStatusStr:  array[TStreamStatus] of string = ('Not ready', 'Stopped', 'Playing', 'Seeking', 'Paused', 'Open');

type
  PAudioBuffer = ^TAudioBuffer;
  TAudioBuffer = array[0 .. (AVCODEC_MAX_AUDIO_FRAME_SIZE * 3 div 2)-1] of byte;

const
  SDL_AUDIO_BUFFER_SIZE = 1024;

type
  TFFMpegOutputStream = class(TAudioOutputStream)
    private
      parseThread: PSDL_Thread;
      packetQueue: TPacketQueue;

      status: TStreamStatus;

      // FFMpeg internal data
      pFormatCtx     : PAVFormatContext;
      pCodecCtx      : PAVCodecContext;
      pCodec         : PAVCodec;
      ffmpegStreamID : Integer;
      ffmpegStream   : PAVStream;

      // static vars for AudioDecodeFrame
      pkt             : TAVPacket;
      audio_pkt_data  : PChar;
      audio_pkt_size  : integer;

      // static vars for AudioCallback
      audio_buf_index : cardinal;
      audio_buf_size  : cardinal;
      audio_buf       : TAudioBuffer;

      paStream        : PPaStream;
      paFrameSize     : integer;
    public
      constructor Create(); overload;
      constructor Create(pFormatCtx: PAVFormatContext;
                         pCodecCtx: PAVCodecContext; pCodec: PAVCodec;
                         ffmpegStreamID : Integer; ffmpegStream: PAVStream); overload;

      procedure Play();
      procedure Pause();
      procedure Stop();
      procedure Close();

      function AudioDecodeFrame(buffer : PUInt8; bufSize: integer): integer;
  end;

type
  TAudio_FFMpeg = class( TInterfacedObject, IAudioPlayback )
    private
      MusicStream:        TFFMpegOutputStream;

      StartSoundStream:   TFFMpegOutputStream;
      BackSoundStream:    TFFMpegOutputStream;
      SwooshSoundStream:  TFFMpegOutputStream;
      ChangeSoundStream:  TFFMpegOutputStream;
      OptionSoundStream:  TFFMpegOutputStream;
      ClickSoundStream:   TFFMpegOutputStream;
      DrumSoundStream:    TFFMpegOutputStream;
      HihatSoundStream:   TFFMpegOutputStream;
      ClapSoundStream:    TFFMpegOutputStream;
      ShuffleSoundStream: TFFMpegOutputStream;

      //Custom Sounds
      CustomSounds: array of TCustomSoundEntry;
      Loaded:   boolean;
      Loop:     boolean;

      function FindAudioStreamID(pFormatCtx : PAVFormatContext): integer;
    public
      function  GetName: String;
      procedure InitializePlayback;
      procedure SetVolume(Volume: integer);
      procedure SetMusicVolume(Volume: integer);
      procedure SetLoop(Enabled: boolean);
      function Open(Name: string): boolean; // true if succeed
      procedure Rewind;
      procedure MoveTo(Time: real);
      procedure Play;
      procedure Pause; //Pause Mod
      procedure Stop;
      procedure Close;
      function Finished: boolean;
      function Length: real;
      function getPosition: real;
      procedure PlayStart;
      procedure PlayBack;
      procedure PlaySwoosh;
      procedure PlayChange;
      procedure PlayOption;
      procedure PlayClick;
      procedure PlayDrum;
      procedure PlayHihat;
      procedure PlayClap;
      procedure PlayShuffle;
      procedure StopShuffle;

      function LoadSoundFromFile(Name: string): TFFMpegOutputStream;

      //Equalizer
      function GetFFTData: TFFTData;

      // Interface for Visualizer
      function GetPCMData(var data: TPCMData): Cardinal;

      //Custom Sounds
      function LoadCustomSound(const Filename: String): Cardinal;
      procedure PlayCustomSound(const Index: Cardinal );
  end;

var
  test: TFFMpegOutputStream;
  it: integer;

var
  singleton_MusicFFMpeg : IAudioPlayback = nil;


function ParseAudio(data: Pointer): integer; cdecl; forward;
procedure SDL_AudioCallback( userdata: Pointer; stream: PUInt8; len: Integer ); cdecl; forward;
function Pa_AudioCallback(input: Pointer; output: Pointer; frameCount: Longword;
      timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags;
      userData: Pointer): Integer; cdecl; forward;

constructor TFFMpegOutputStream.Create();
begin
  inherited;

  packetQueue := TPacketQueue.Create();

  FillChar(pkt, sizeof(TAVPacket), #0);

  status := sStopped;

  audio_pkt_data := nil;
  audio_pkt_size := 0;

  audio_buf_index := 0;
  audio_buf_size  := 0;
end;

constructor TFFMpegOutputStream.Create(pFormatCtx: PAVFormatContext;
                   pCodecCtx: PAVCodecContext; pCodec: PAVCodec;
                   ffmpegStreamID : Integer; ffmpegStream: PAVStream);
begin
  Create();

  Self.pFormatCtx := pFormatCtx;
  Self.pCodecCtx := pCodecCtx;
  Self.pCodec    := pCodec;
  Self.ffmpegStreamID := ffmpegStreamID;
  Self.ffmpegStream   := ffmpegStream;

  test := Self;
  it:=0;
end;

procedure TFFMpegOutputStream.Play();
var
  err: TPaError;
begin
  writeln('Play request');
  if(status = sStopped) then
  begin
    writeln('Play ok');
    status := sPlaying;
    Self.parseThread := SDL_CreateThread(@ParseAudio, Self);
    //SDL_PauseAudio(0);

    err := Pa_StartStream(Self.paStream);
    if(err <> paNoError) then
    begin
      Writeln('Play: '+ Pa_GetErrorText(err));
    end;
  end;
end;

procedure TFFMpegOutputStream.Pause();
begin
end;

procedure TFFMpegOutputStream.Stop();
begin
end;

procedure TFFMpegOutputStream.Close();
begin
  // Close the codec
  avcodec_close(pCodecCtx);

  // Close the video file
  av_close_input_file(pFormatCtx);
end;


function TAudio_FFMpeg.GetName: String;
begin
  result := 'FFMpeg';
end;

procedure TAudio_FFMpeg.InitializePlayback;
begin
  Log.LogStatus('InitializePlayback', 'UAudio_FFMpeg');

  Loaded := false;
  Loop   := false;

  av_register_all();
  //SDL_Init(SDL_INIT_AUDIO);
  Pa_Initialize();

  StartSoundStream   := LoadSoundFromFile(SoundPath + 'Common start.mp3');
  BackSoundStream    := LoadSoundFromFile(SoundPath + 'Common back.mp3');
  SwooshSoundStream  := LoadSoundFromFile(SoundPath + 'menu swoosh.mp3');
  ChangeSoundStream  := LoadSoundFromFile(SoundPath + 'select music change music 50.mp3');
  OptionSoundStream  := LoadSoundFromFile(SoundPath + 'option change col.mp3');
  ClickSoundStream   := LoadSoundFromFile(SoundPath + 'rimshot022b.mp3');

//  DrumSoundStream  := LoadSoundFromFile(SoundPath + 'bassdrumhard076b.mp3');
//  HihatSoundStream := LoadSoundFromFile(SoundPath + 'hihatclosed068b.mp3');
//  ClapSoundStream  := LoadSoundFromFile(SoundPath + 'claps050b.mp3');

//  ShuffleSoundStream := LoadSoundFromFile(SoundPath + 'Shuffle.mp3');
end;


procedure TAudio_FFMpeg.SetVolume(Volume: integer);
begin
  //New: Sets Volume only for this Application
(*
  BASS_SetConfig(BASS_CONFIG_GVOL_SAMPLE, Volume);
  BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, Volume);
  BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, Volume);
*)
end;

procedure TAudio_FFMpeg.SetMusicVolume(Volume: Integer);
begin
  //Max Volume Prevention
  if Volume > 100 then
    Volume := 100;

  if Volume < 0 then
    Volume := 0;


  //Set Volume
//  BASS_ChannelSetAttributes (Bass, -1, Volume, -101);
end;

procedure TAudio_FFMpeg.SetLoop(Enabled: boolean);
begin
  Loop := Enabled;
end;

function TAudio_FFMpeg.Open(Name: string): boolean;
begin
  Loaded := false;
  if FileExists(Name) then
  begin
//    Bass := Bass_StreamCreateFile(false, pchar(Name), 0, 0, 0);

    Loaded := true;
    //Set Max Volume
    SetMusicVolume (100);
  end;

  Result := Loaded;
end;

procedure TAudio_FFMpeg.Rewind;
begin
  if Loaded then
  begin
  end;
end;

procedure TAudio_FFMpeg.MoveTo(Time: real);
var
  bytes:    integer;
begin
//  bytes := BASS_ChannelSeconds2Bytes(Bass, Time);
//  BASS_ChannelSetPosition(Bass, bytes);
end;

procedure TAudio_FFMpeg.Play;
begin
  if MusicStream <> nil then
  if Loaded then
  begin
    if Loop then
    begin
    end;
    // start from beginning...
    // actually bass itself does not loop, nor does this TAudio_FFMpeg Class
    MusicStream.Play();
  end;
end;

procedure TAudio_FFMpeg.Pause; //Pause Mod
begin
  if MusicStream <> nil then
  if Loaded then begin
    MusicStream.Pause(); // Pauses Song
  end;
end;

procedure TAudio_FFMpeg.Stop;
begin
  if MusicStream <> nil then
  MusicStream.Stop();
end;

procedure TAudio_FFMpeg.Close;
begin
  if MusicStream <> nil then
  MusicStream.Close();
end;

function TAudio_FFMpeg.Length: real;
var
  bytes: integer;
begin
  Result := 0;
  // Todo : why is Music stream always nil !?

  if assigned( MusicStream ) then
  begin
    Result := MusicStream.pFormatCtx^.duration / AV_TIME_BASE;
  end;
end;

function TAudio_FFMpeg.getPosition: real;
var
  bytes: integer;
begin
  Result := 0;

(*
  bytes  := BASS_ChannelGetPosition(BASS);
  Result := BASS_ChannelBytes2Seconds(BASS, bytes);
*)
end;

function TAudio_FFMpeg.Finished: boolean;
begin
  Result := false;

(*
  if BASS_ChannelIsActive(BASS) = BASS_ACTIVE_STOPPED then
  begin
    Result := true;
  end;
*)
end;

procedure TAudio_FFMpeg.PlayStart;
begin
  if StartSoundStream <> nil then
  StartSoundStream.Play();
end;

procedure TAudio_FFMpeg.PlayBack;
begin
  if BackSoundStream <> nil then
  BackSoundStream.Play();
end;

procedure TAudio_FFMpeg.PlaySwoosh;
begin
  if SwooshSoundStream <> nil then
  SwooshSoundStream.Play();
end;

procedure TAudio_FFMpeg.PlayChange;
begin
  if ChangeSoundStream <> nil then
  ChangeSoundStream.Play();
end;

procedure TAudio_FFMpeg.PlayOption;
begin
  if OptionSoundStream <> nil then
  OptionSoundStream.Play();
end;

procedure TAudio_FFMpeg.PlayClick;
begin
  if ClickSoundStream <> nil then
  ClickSoundStream.Play();
end;

procedure TAudio_FFMpeg.PlayDrum;
begin
  if DrumSoundStream <> nil then
  DrumSoundStream.Play();
end;

procedure TAudio_FFMpeg.PlayHihat;
begin
  if HihatSoundStream <> nil then
  HihatSoundStream.Play();
end;

procedure TAudio_FFMpeg.PlayClap;
begin
  if ClapSoundStream <> nil then
  ClapSoundStream.Play();
end;

procedure TAudio_FFMpeg.PlayShuffle;
begin
  if ShuffleSoundStream <> nil then
  ShuffleSoundStream.Play();
end;

procedure TAudio_FFMpeg.StopShuffle;
begin
  if ShuffleSoundStream <> nil then
  ShuffleSoundStream.Stop();
end;


function TFFMpegOutputStream.AudioDecodeFrame(buffer : PUInt8; bufSize: integer): integer;
var
  len1,
  data_size: integer;
begin
  result := -1;

  if (buffer = nil)  then
    exit;

  while true do
  begin
    while (audio_pkt_size > 0) do
    begin
//      writeln( 'got audio packet' );
      data_size := bufSize;

      if(pCodecCtx = nil) then begin
//        writeln('Das wars');
        exit;
      end;

      // TODO: should be avcodec_decode_audio2 but this wont link on my ubuntu box.
      len1 := avcodec_decode_audio(pCodecCtx, Pointer(buffer),
                  data_size, audio_pkt_data, audio_pkt_size);

      //writeln('avcodec_decode_audio : ' + inttostr( len1 ));

      if(len1 < 0) then
      begin
	      // if error, skip frame
//       	writeln( 'Skip audio frame' );
        audio_pkt_size := 0;
       	break;
      end;

      Inc(audio_pkt_data, len1);
      Dec(audio_pkt_size, len1);

      if (data_size <= 0) then
      begin
    	  // No data yet, get more frames
     	  continue;
      end;

      // We have data, return it and come back for more later
      result := data_size;
      exit;
    end;

    inc(it);
    if (pkt.data <> nil) then
    begin
      av_free_packet(pkt);
    end;

    if (packetQueue.quit) then
    begin
      result := -1;
      exit;
    end;
//    writeln(it);
    if (packetQueue.Get(pkt, true) < 0) then
    begin
      result := -1;
      exit;
    end;

    audio_pkt_data := PChar(pkt.data);
    audio_pkt_size := pkt.size;
//    writeln( 'Audio Packet Size - ' + inttostr(audio_pkt_size) );
  end;
end;

procedure SDL_AudioCallback(userdata: Pointer; stream: PUInt8; len: Integer); cdecl;
var
  outStream       : TFFMpegOutputStream;
  len1,
  audio_size      : integer;
  pSrc            : Pointer;
begin
  outStream := TFFMpegOutputStream(userdata);

  while (len > 0) do
  with outStream do begin
    if (audio_buf_index >= audio_buf_size) then
    begin
      // We have already sent all our data; get more
      audio_size := AudioDecodeFrame(@audio_buf, sizeof(TAudioBuffer));
      //writeln('audio_decode_frame : '+ inttostr(audio_size));

      if(audio_size < 0) then
      begin
      	// If error, output silence
        audio_buf_size := 1024;
        FillChar(audio_buf, audio_buf_size, #0);
        //writeln( 'Silence' );
      end
      else
      begin
      	audio_buf_size := audio_size;
      end;
      audio_buf_index := 0;
    end;

    len1 := audio_buf_size - audio_buf_index;
    if (len1 > len) then
      len1 := len;

    pSrc := PChar(@audio_buf) + audio_buf_index;
    {$ifdef WIN32}
      CopyMemory(stream, pSrc , len1);
    {$else}
      memcpy(stream, pSrc , len1);
    {$endif}

    Dec(len, len1);
    Inc(stream, len1);
    Inc(audio_buf_index, len1);
  end;
end;

function Pa_AudioCallback(input: Pointer; output: Pointer; frameCount: Longword;
      timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags;
      userData: Pointer): Integer; cdecl;
var
  outStream       : TFFMpegOutputStream;
  len1,
  audio_size      : integer;
  pSrc            : Pointer;
  len             : integer;
begin
  outStream := TFFMpegOutputStream(userData);
  len := frameCount * outStream.paFrameSize;

  while (len > 0) do
  with outStream do begin
    if (audio_buf_index >= audio_buf_size) then
    begin
      // We have already sent all our data; get more
      audio_size := AudioDecodeFrame(@audio_buf, sizeof(TAudioBuffer));
      //writeln('audio_decode_frame : '+ inttostr(audio_size));

      if(audio_size < 0) then
      begin
      	// If error, output silence
        audio_buf_size := 1024;
        FillChar(audio_buf, audio_buf_size, #0);
        //writeln( 'Silence' );
      end
      else
      begin
      	audio_buf_size := audio_size;
      end;
      audio_buf_index := 0;  // Todo : jb - SegFault ?
    end;

    len1 := audio_buf_size - audio_buf_index;
    if (len1 > len) then
      len1 := len;

    pSrc := PChar(@audio_buf) + audio_buf_index;
    {$ifdef WIN32}
      CopyMemory(output, pSrc , len1);
    {$else}
      memcpy(output, pSrc , len1);
    {$endif}

    Dec(len, len1);
    Inc(PChar(output), len1);
    Inc(audio_buf_index, len1);
  end;

  result := paContinue;
end;

function TAudio_FFMpeg.FindAudioStreamID(pFormatCtx : PAVFormatContext): integer;
var
  i : integer;
  streamID: integer;
  stream : PAVStream;
begin
  // Find the first audio stream
  streamID := -1;

  for i := 0 to pFormatCtx^.nb_streams-1 do
  begin
    //Log.LogStatus('aFormatCtx.streams[i] : ' + inttostr(i), 'UAudio_FFMpeg');
    stream := pFormatCtx^.streams[i];

    if ( stream.codec^.codec_type = CODEC_TYPE_AUDIO ) then
    begin
      Log.LogStatus('Found Audio Stream', 'UAudio_FFMpeg');
      streamID := i;
      break;
    end;
  end;

  result := streamID;
end;

function ParseAudio(data: Pointer): integer; cdecl;
var
  packet: TAVPacket;
  stream: TFFMpegOutputStream;
begin
  stream := TFFMpegOutputStream(data);

  while (av_read_frame(stream.pFormatCtx, packet) >= 0) do
  begin
    //writeln( 'ffmpeg - av_read_frame' );

    if (packet.stream_index = stream.ffmpegStreamID) then
    begin
      //writeln( 'packet_queue_put' );
      stream.packetQueue.put(@packet);
    end
    else
    begin
      av_free_packet(packet);
    end;
  end;

  //Writeln('Done: ' + inttostr(stream.packetQueue.nbPackets));

  result := 0;
end;


function TAudio_FFMpeg.LoadSoundFromFile(Name: string): TFFMpegOutputStream;
var
  pFormatCtx     : PAVFormatContext;
  pCodecCtx      : PAVCodecContext;
  pCodec         : PAVCodec;
  ffmpegStreamID : Integer;
  ffmpegStream   : PAVStream;
  wanted_spec,
  spec           : TSDL_AudioSpec;
  csIndex        : integer;
  stream         : TFFMpegOutputStream;
  err            : TPaError;
  // move this to a portaudio specific section
  paOutParams     : TPaStreamParameters;
  paApiInfo       : PPaHostApiInfo;
  paApi           : TPaHostApiIndex;
  paOutDevice     : TPaDeviceIndex;
  paOutDeviceInfo : PPaDeviceInfo;
begin
  result := nil;

  if (not FileExists(Name)) then
  begin
    Log.LogStatus('LoadSoundFromFile: Sound not found "' + Name + '"', 'UAudio_FFMpeg');
    writeln('ERROR : LoadSoundFromFile: Sound not found "' + Name + '"', 'UAudio_FFMpeg');
    exit;
  end;

  // Open audio file
  if (av_open_input_file(pFormatCtx, PChar(Name), nil, 0, nil) > 0) then
    exit;

  // Retrieve stream information
  if (av_find_stream_info(pFormatCtx) < 0) then
    exit;

  dump_format(pFormatCtx, 0, pchar(Name), 0);

  ffmpegStreamID := FindAudioStreamID(pFormatCtx);
  if (ffmpegStreamID < 0) then
    exit;

  //Log.LogStatus('Audio Stream ID is : '+ inttostr(ffmpegStreamID), 'UAudio_FFMpeg');

  ffmpegStream := pFormatCtx.streams[ffmpegStreamID];
  pCodecCtx := ffmpegStream^.codec;

  pCodec := avcodec_find_decoder(pCodecCtx^.codec_id);
  if (pCodec = nil) then
  begin
    Log.LogStatus('Unsupported codec!', 'UAudio_FFMpeg');
    exit;
  end;

  avcodec_open(pCodecCtx, pCodec);
  //writeln( 'Opened the codec' );

  stream := TFFMpegOutputStream.Create(pFormatCtx, pCodecCtx, pCodec,
              ffmpegStreamID, ffmpegStream);

  {
  // Set SDL audio settings from codec info
  wanted_spec.freq     := pCodecCtx^.sample_rate;
  wanted_spec.format   := AUDIO_S16SYS;
  wanted_spec.channels := pCodecCtx^.channels;
  wanted_spec.silence  := 0;
  wanted_spec.samples  := SDL_AUDIO_BUFFER_SIZE;
  wanted_spec.callback := AudioCallback;
  wanted_spec.userdata := stream;

  // TODO: this works only one time (?)
  if (SDL_OpenAudio(@wanted_spec, @spec) < 0) then
  begin
    Log.LogStatus('SDL_OpenAudio: '+SDL_GetError(), 'UAudio_FFMpeg');
    stream.Free();
    exit;
  end;
  }
  
  paApi     := Pa_HostApiTypeIdToHostApiIndex(paALSA);
  paApiInfo := Pa_GetHostApiInfo(paApi);
  paOutDevice     := paApiInfo^.defaultOutputDevice;
  paOutDeviceInfo := Pa_GetDeviceInfo(paOutDevice);
  
  with paOutParams do begin
    device := paOutDevice;
    channelCount := pCodecCtx^.channels;
    sampleFormat := paInt16;
    suggestedLatency := paOutDeviceInfo^.defaultHighOutputLatency;
    hostApiSpecificStreamInfo := nil;
  end;

  stream.paFrameSize := sizeof(Smallint) * pCodecCtx^.channels;

  err := Pa_OpenStream(stream.paStream, nil, @paOutParams, pCodecCtx^.sample_rate,
          paFramesPerBufferUnspecified, //SDL_AUDIO_BUFFER_SIZE div stream.paFrameSize
          paNoFlag, @PA_AudioCallback, stream);
  if(err <> paNoError) then begin
    Log.LogStatus('Pa_OpenDefaultStream: '+Pa_GetErrorText(err), 'UAudio_FFMpeg');
    stream.Free();
    exit;
  end;

  Log.LogStatus('Opened audio device', 'UAudio_FFMpeg');

  //Add CustomSound
  csIndex := High(CustomSounds) + 1;
  SetLength (CustomSounds, csIndex + 1);
  CustomSounds[csIndex].Filename := Name;
  CustomSounds[csIndex].Stream := stream;

  result := stream;
end;

//Equalizer
function TAudio_FFMpeg.GetFFTData: TFFTData;
var
  data: TFFTData;
begin
  //Get Channel Data Mono and 256 Values
//  BASS_ChannelGetData(Bass, @Result, BASS_DATA_FFT512);
  result := data;
end;

// Interface for Visualizer
function TAudio_FFMpeg.GetPCMData(var data: TPCMData): Cardinal;
begin
  result := 0;
end;

function TAudio_FFMpeg.LoadCustomSound(const Filename: String): Cardinal;
var
  S: TFFMpegOutputStream;
  I: Integer;
  F: String;
begin
  //Search for Sound in already loaded Sounds
  F := UpperCase(SoundPath + FileName);
  For I := 0 to High(CustomSounds) do
  begin
    if (UpperCase(CustomSounds[I].Filename) = F) then
    begin
      Result := I;
      Exit;
    end;
  end;

  S := LoadSoundFromFile(SoundPath + Filename);
  if (S <> nil) then
    Result := High(CustomSounds)
  else
    Result := 0;
end;

procedure TAudio_FFMpeg.PlayCustomSound(const Index: Cardinal );
begin
  if Index <= High(CustomSounds) then
    (CustomSounds[Index].Stream as TFFMpegOutputStream).Play();
end;


constructor TPacketQueue.Create();
begin
  inherited;

  firstPkt := nil;
  lastPkt  := nil;
  nbPackets := 0;
  size := 0;

  mutex := SDL_CreateMutex();
  cond  := SDL_CreateCond();
end;

function TPacketQueue.Put(pkt : PAVPacket): integer;
var
  pkt1 : PAVPacketList;
begin
  result := -1;

  if (av_dup_packet(pkt) < 0) then
    exit;
    
  pkt1 := av_malloc(sizeof(TAVPacketList));
  if (pkt1 = nil) then
    exit;

  pkt1^.pkt  := pkt^;
  pkt1^.next := nil;


  SDL_LockMutex(Self.mutex);
  try

    if (Self.lastPkt = nil) then
      Self.firstPkt := pkt1
    else
      Self.lastPkt^.next := pkt1;
      
    Self.lastPkt := pkt1;
    inc(Self.nbPackets);

//    Writeln('Put: ' + inttostr(nbPackets));

    Self.size := Self.size + pkt1^.pkt.size;
    SDL_CondSignal(Self.cond);

  finally
    SDL_UnlockMutex(Self.mutex);
  end;

  result := 0;
end;

function TPacketQueue.Get(var pkt: TAVPacket; block: boolean): integer;
var
  pkt1 : PAVPacketList;
begin
  result := -1;

  SDL_LockMutex(Self.mutex);
  try
    while true do
    begin
      if (quit) then
        exit;

      pkt1 := Self.firstPkt;

      if (pkt1 <> nil) then
      begin
        Self.firstPkt := pkt1.next;
        if (Self.firstPkt = nil) then
      	  Self.lastPkt := nil;
        dec(Self.nbPackets);

//        Writeln('Get: ' + inttostr(nbPackets));

        Self.size := Self.size - pkt1^.pkt.size;
        pkt := pkt1^.pkt;
        av_free(pkt1);

        result := 1;
        break;
      end
      else
      if (not block) then
      begin
        result := 0;
        break;
      end
      else
      begin
        SDL_CondWait(Self.cond, Self.mutex);
      end;
    end;
  finally
    SDL_UnlockMutex(Self.mutex);
  end;
end;



initialization
  singleton_MusicFFMpeg := TAudio_FFMpeg.create();

  writeln( 'UAudio_FFMpeg - Register Playback' );
  AudioManager.add( IAudioPlayback( singleton_MusicFFMpeg ) );

finalization
  AudioManager.Remove( IAudioPlayback( singleton_MusicFFMpeg ) );


end.
