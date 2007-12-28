unit UAudioDecoder_FFMpeg;

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
     SysUtils,
     SDL,
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
       libc,
       {$endif}
     {$ENDIF}
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
      destructor Destroy(); override;

      function Put(pkt : PAVPacket): integer;
      function Get(var pkt: TAVPacket; block: boolean): integer;
  end;

var
  EOSPacket: TAVPacket;

type
  PAudioBuffer = ^TAudioBuffer;
  TAudioBuffer = array[0 .. (AVCODEC_MAX_AUDIO_FRAME_SIZE * 3 div 2)-1] of byte;

type
  TFFMpegDecodeStream = class(TAudioDecodeStream)
    private
      status: TStreamStatus;

      EOS_Flag: boolean; // end-of-stream flag

      parseThread: PSDL_Thread;
      packetQueue: TPacketQueue;

      // FFMpeg internal data
      pFormatCtx     : PAVFormatContext;
      pCodecCtx      : PAVCodecContext;
      pCodec         : PAVCodec;
      ffmpegStreamID : Integer;
      ffmpegStream   : PAVStream;

      // "static" vars for DecodeFrame
      pkt             : TAVPacket;
      audio_pkt_data  : PChar;
      audio_pkt_size  : integer;

      // "static" vars for AudioCallback
      audio_buf_index : cardinal;
      audio_buf_size  : cardinal;
      audio_buf       : TAudioBuffer;

      function DecodeFrame(var buffer: TAudioBuffer; bufSize: integer): integer;
    public
      constructor Create(pFormatCtx: PAVFormatContext;
                         pCodecCtx: PAVCodecContext; pCodec: PAVCodec;
                         ffmpegStreamID : Integer; ffmpegStream: PAVStream);
      destructor Destroy(); override;

      procedure Close();                     override;

      function GetLength(): real;            override;
      function GetChannelCount(): cardinal;  override;
      function GetSampleRate(): cardinal;    override;
      function GetPosition: real;            override;
      procedure SetPosition(Time: real);     override;
      function IsEOS(): boolean;             override;

      function ReadData(Buffer: PChar; BufSize: integer): integer; override;
  end;

type
  TAudioDecoder_FFMpeg = class( TInterfacedObject, IAudioDecoder )
    private
      class function FindAudioStreamID(pFormatCtx : PAVFormatContext): integer;
    public
      function GetName: String;

      function InitializeDecoder(): boolean;
      function Open(const Filename: string): TAudioDecodeStream;
  end;

function ParseAudio(streamPtr: Pointer): integer; cdecl; forward;

var
  singleton_AudioDecoderFFMpeg : IAudioDecoder;


{ TFFMpegDecodeStream }

constructor TFFMpegDecodeStream.Create(pFormatCtx: PAVFormatContext;
                   pCodecCtx: PAVCodecContext; pCodec: PAVCodec;
                   ffmpegStreamID : Integer; ffmpegStream: PAVStream);
begin
  inherited Create();

  status := sStopped;
  packetQueue := TPacketQueue.Create();

  audio_pkt_data := nil;
  audio_pkt_size := 0;

  audio_buf_index := 0;
  audio_buf_size  := 0;

  FillChar(pkt, sizeof(TAVPacket), #0);

  Self.pFormatCtx := pFormatCtx;
  Self.pCodecCtx := pCodecCtx;
  Self.pCodec    := pCodec;
  Self.ffmpegStreamID := ffmpegStreamID;
  Self.ffmpegStream   := ffmpegStream;

  EOS_Flag := false;

  parseThread := SDL_CreateThread(@ParseAudio, Self);
end;

destructor TFFMpegDecodeStream.Destroy();
begin
  packetQueue.Free();
  //SDL_WaitThread(parseThread, nil);
  inherited;
end;

procedure TFFMpegDecodeStream.Close();
begin
  // Close the codec
  avcodec_close(pCodecCtx);

  // Close the video file
  av_close_input_file(pFormatCtx);

  // TODO: abort thread
end;

function TFFMpegDecodeStream.GetLength(): real;
begin
  result := pFormatCtx^.duration / AV_TIME_BASE;
end;

function TFFMpegDecodeStream.GetChannelCount(): cardinal;
begin
  result := pCodecCtx^.channels;
end;

function TFFMpegDecodeStream.GetSampleRate(): cardinal;
begin
  result := pCodecCtx^.sample_rate;
end;

function TFFMpegDecodeStream.IsEOS(): boolean;
begin
  result := EOS_Flag;
end;


function ParseAudio(streamPtr: Pointer): integer; cdecl;
var
  packet: TAVPacket;
  stream: TFFMpegDecodeStream;
begin
  stream := TFFMpegDecodeStream(streamPtr);

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

  // signal end-of-stream
  stream.packetQueue.put(@EOSPacket);

  result := 0;
end;

function TFFMpegDecodeStream.DecodeFrame(var buffer: TAudioBuffer; bufSize: integer): integer;
var
  len1,
  data_size: integer;
begin
  result := -1;

  if (EOS_Flag) then
    exit;

  while true do
  begin
    while (audio_pkt_size > 0) do
    begin
      //writeln( 'got audio packet' );
      data_size := bufSize;

      // TODO: should be avcodec_decode_audio2 but this wont link on my ubuntu box.
      len1 := avcodec_decode_audio(pCodecCtx, @buffer,
                  data_size, audio_pkt_data, audio_pkt_size);

      //writeln('avcodec_decode_audio : ' + inttostr( len1 ));

      if(len1 < 0) then
      begin
	      // if error, skip frame
        //writeln( 'Skip audio frame' );
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

    if (pkt.data <> nil) then
    begin
      av_free_packet(pkt);
    end;

    if (packetQueue.quit) then
      exit;

    if (packetQueue.Get(pkt, true) < 0) then
      exit;

    audio_pkt_data := PChar(pkt.data);
    audio_pkt_size := pkt.size;

    // check for end-of-stream
    if (audio_pkt_data = PChar(EOSPacket.data)) then
    begin
      // end-of-stream reached -> set EOS-flag
      EOS_Flag := true;
      // note: buffer is not (even partially) filled -> no data to return
      exit;
    end;

    //writeln( 'Audio Packet Size - ' + inttostr(audio_pkt_size) );
  end;
end;

function TFFMpegDecodeStream.ReadData(Buffer : PChar; BufSize: integer): integer;
var
  outStream       : TFFMpegDecodeStream;
  len1,
  audio_size      : integer;
  pSrc            : Pointer;
  len             : integer;
begin
  len := BufSize;

  // end-of-stream reached
  if (EOS_Flag) then
    exit;

  while (len > 0) do begin
    if (audio_buf_index >= audio_buf_size) then
    begin
      // We have already sent all our data; get more
      audio_size := DecodeFrame(audio_buf, sizeof(TAudioBuffer));
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
      CopyMemory(Buffer, pSrc , len1);
    {$else}
      memcpy(Buffer, pSrc , len1);
    {$endif}

    Dec(len, len1);
    Inc(PChar(Buffer), len1);
    Inc(audio_buf_index, len1);
  end;

  result := BufSize;
end;

function TFFMpegDecodeStream.GetPosition(): real;
var
  bytes: integer;
begin
  Result := 0;
end;

procedure TFFMpegDecodeStream.SetPosition(Time: real);
var
  bytes:    integer;
begin
end;


{ TAudioDecoder_FFMpeg }

function TAudioDecoder_FFMpeg.GetName: String;
begin
  result := 'FFMpeg_Decoder';
end;

function TAudioDecoder_FFMpeg.InitializeDecoder: boolean;
begin
  //Log.LogStatus('InitializeDecoder', 'UAudioDecoder_FFMpeg');

  av_register_all();

  // init end-of-stream package
  av_init_packet(EOSPacket);
  EOSPacket.data := Pointer(PChar('EOS'));

  result := true;
end;

class function TAudioDecoder_FFMpeg.FindAudioStreamID(pFormatCtx : PAVFormatContext): integer;
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
      //Log.LogStatus('Found Audio Stream', 'UAudio_FFMpeg');
      streamID := i;
      break;
    end;
  end;

  result := streamID;
end;

function TAudioDecoder_FFMpeg.Open(const Filename: string): TAudioDecodeStream;
var
  pFormatCtx     : PAVFormatContext;
  pCodecCtx      : PAVCodecContext;
  pCodec         : PAVCodec;
  ffmpegStreamID : Integer;
  ffmpegStream   : PAVStream;
  wanted_spec,
  csIndex        : integer;
  stream         : TFFMpegDecodeStream;
begin
  result := nil;

  if (not FileExists(Filename)) then
  begin
    Log.LogStatus('LoadSoundFromFile: Sound not found "' + Filename + '"', 'UAudio_FFMpeg');
    exit;
  end;

  // Open audio file
  if (av_open_input_file(pFormatCtx, PChar(Filename), nil, 0, nil) > 0) then
    exit;

  // Retrieve stream information
  if (av_find_stream_info(pFormatCtx) < 0) then
    exit;

  dump_format(pFormatCtx, 0, pchar(Filename), 0);

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

  stream := TFFMpegDecodeStream.Create(pFormatCtx, pCodecCtx, pCodec,
              ffmpegStreamID, ffmpegStream);

  result := stream;
end;


{ TPacketQueue }

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

destructor TPacketQueue.Destroy();
begin
  SDL_DestroyMutex(mutex);
  SDL_DestroyCond(cond);
  inherited;
end;

function TPacketQueue.Put(pkt : PAVPacket): integer;
var
  pkt1 : PAVPacketList;
begin
  result := -1;

  if (pkt <> @EOSPacket) then
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

    //Writeln('Put: ' + inttostr(nbPackets));

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

        //Writeln('Get: ' + inttostr(nbPackets));

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
  singleton_AudioDecoderFFMpeg := TAudioDecoder_FFMpeg.create();

  //writeln( 'UAudioDecoder_FFMpeg - Register Decoder' );
  AudioManager.add( singleton_AudioDecoderFFMpeg );

finalization
  AudioManager.Remove( singleton_AudioDecoderFFMpeg );


end.
