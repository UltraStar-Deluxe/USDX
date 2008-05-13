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

//{$DEFINE DebugFFMpegDecode}

uses
  Classes,
  SysUtils,
  UMusic;

implementation

uses
  UIni,
  UMain,
  avcodec,     // FFMpeg Audio file decoding
  avformat,
  avutil,
  avio,        // used for url_ferror
  mathematics, // used for av_rescale_q
  rational,
  SDL,
  ULog,
  UCommon,
  UConfig;

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
      abortRequest: boolean;

    public
      constructor Create();
      destructor Destroy(); override;

      function Put(pkt : PAVPacket): integer;
      function PutStatus(statusFlag: integer; statusInfo: Pointer): integer;
      function Get(var pkt: TAVPacket; block: boolean): integer;
      procedure Flush();
      procedure Abort();
  end;

const
  MAX_AUDIOQ_SIZE = (5 * 16 * 1024);

const
  STATUS_PACKET: PChar = 'STATUS_PACKET';
const
  PKT_STATUS_FLAG_EOF   = 1;
  PKT_STATUS_FLAG_FLUSH = 2;
  PKT_STATUS_FLAG_ERROR = 3;

type
  PAudioBuffer = ^TAudioBuffer;
  // TODO: should (or must?) be aligned at a 2-byte boundary.
  //   ffmpeg provides a C-macro called DECLARE_ALIGNED for this.
  //   Records can be aligned with the $PACKRECORDS compiler-directive but
  //   are already aligned at a 2-byte boundary by default in FPC.
  //   But what about arrays, are they aligned by a 2-byte boundary too?
  //   Or maybe we have to define a fake record with only an array in it?
  TAudioBuffer = array[0 .. (AVCODEC_MAX_AUDIO_FRAME_SIZE * 3 div 2)-1] of byte;

type
  TFFMpegDecodeStream = class(TAudioDecodeStream)
    private
      decoderLock : PSDL_Mutex;
      parserLock : PSDL_Mutex;
      myint: integer;

      EOFState: boolean; // end-of-stream flag
      ErrorState: boolean;

      resumeCond   : PSDL_Cond;

      quitRequest : boolean;

      seekRequest: boolean;
      seekFlags  : integer;
      seekPos    : int64;
      seekCond   : PSDL_Cond;

      parseThread: PSDL_Thread;
      packetQueue: TPacketQueue;

      formatInfo : TAudioFormatInfo;

      // FFMpeg internal data
      pFormatCtx     : PAVFormatContext;
      pCodecCtx      : PAVCodecContext;
      pCodec         : PAVCodec;
      ffmpegStreamIndex : Integer;
      ffmpegStream      : PAVStream;

      audioClock: double; // stream position in seconds

      // state-vars for DecodeFrame
      pkt             : TAVPacket;
      audio_pkt_data  : PChar;
      audio_pkt_size  : integer;

      // state-vars for AudioCallback
      audio_buf_index : integer;
      audio_buf_size  : integer;
      audio_buf       : TAudioBuffer;

      procedure LockParser();   {$IFDEF HasInline}inline;{$ENDIF}
      procedure UnlockParser(); {$IFDEF HasInline}inline;{$ENDIF}
      function GetParserMutex(): PSDL_Mutex; {$IFDEF HasInline}inline;{$ENDIF}

      procedure LockDecoder();   {$IFDEF HasInline}inline;{$ENDIF}
      procedure UnlockDecoder(); {$IFDEF HasInline}inline;{$ENDIF}

      procedure SetEOF(state: boolean);   {$IFDEF HasInline}inline;{$ENDIF}
      procedure SetError(state: boolean); {$IFDEF HasInline}inline;{$ENDIF}

      procedure ParseAudio();
      function DecodeFrame(var buffer: TAudioBuffer; bufSize: integer): integer;
    public
      constructor Create(pFormatCtx: PAVFormatContext;
                         pCodecCtx: PAVCodecContext; pCodec: PAVCodec;
                         ffmpegStreamIndex: Integer; ffmpegStream: PAVStream);
      destructor Destroy(); override;

      procedure Close();                     override;

      function GetLength(): real;            override;
      function GetAudioFormatInfo(): TAudioFormatInfo; override;
      function GetPosition: real;            override;
      procedure SetPosition(Time: real);     override;
      function IsEOF(): boolean;             override;
      function IsError(): boolean;           override;

      function ReadData(Buffer: PChar; BufSize: integer): integer; override;
  end;

type
  TAudioDecoder_FFMpeg = class( TInterfacedObject, IAudioDecoder )
    private
      class function FindAudioStreamIndex(pFormatCtx : PAVFormatContext): integer;
    public
      function GetName: String;

      function InitializeDecoder(): boolean;
      function FinalizeDecoder(): boolean;
      function Open(const Filename: string): TAudioDecodeStream;
  end;

function DecodeThreadMain(streamPtr: Pointer): integer; cdecl; forward;

var
  singleton_AudioDecoderFFMpeg : IAudioDecoder;


{ TFFMpegDecodeStream }

constructor TFFMpegDecodeStream.Create(pFormatCtx: PAVFormatContext;
                   pCodecCtx: PAVCodecContext; pCodec: PAVCodec;
                   ffmpegStreamIndex : Integer; ffmpegStream: PAVStream);
var
  sampleFormat: TAudioSampleFormat;
begin
  inherited Create();

  packetQueue := TPacketQueue.Create();

  audio_pkt_data := nil;
  audio_pkt_size := 0;

  audio_buf_index := 0;
  audio_buf_size  := 0;

  FillChar(pkt, sizeof(TAVPacket), 0);

  Self.pFormatCtx := pFormatCtx;
  Self.pCodecCtx := pCodecCtx;
  Self.pCodec    := pCodec;
  Self.ffmpegStreamIndex := ffmpegStreamIndex;
  Self.ffmpegStream      := ffmpegStream;

  case pCodecCtx^.sample_fmt of
    SAMPLE_FMT_U8:  sampleFormat := asfU8;
    SAMPLE_FMT_S16: sampleFormat := asfS16;
    SAMPLE_FMT_S24: sampleFormat := asfS24;
    SAMPLE_FMT_S32: sampleFormat := asfS32;
    SAMPLE_FMT_FLT: sampleFormat := asfFloat;
    else            sampleFormat := asfS16; // try standard format
  end;

  formatInfo := TAudioFormatInfo.Create(
    pCodecCtx^.channels,
    pCodecCtx^.sample_rate,
    sampleFormat
  );

  EOFState := false;
  ErrorState := false;
  decoderLock := SDL_CreateMutex();
  parserLock := SDL_CreateMutex();
  resumeCond := SDL_CreateCond();
  seekCond := SDL_CreateCond();

  parseThread := SDL_CreateThread(@DecodeThreadMain, Self);
end;

{*
 * Frees the decode-stream data.
 * IMPORTANT: call Close() before freeing the decode-stream to avoid dead-locks.
 * This wakes-up every waiting audio-thread waiting in the packet-queue while
 * performing a ReadData() request.
 * Then assure that no thread uses ReadData anymore (e.g. by stopping the audio-callback).
 * Now you can free the decode-stream.
 *}
destructor TFFMpegDecodeStream.Destroy();
begin
  // wake-up and terminate threads
  // Note: should be called by the caller before Destroy() was called instead
  //   to wake-up a waiting audio-callback thread in the packet-queue.
  //   Otherwise dead-locks are possible.
  Close();

  // Close the codec
  if (pCodecCtx <> nil) then
  begin
    avcodec_close(pCodecCtx);
    pCodecCtx := nil;
  end;

  // Close the video file
  if (pFormatCtx <> nil) then
  begin
    av_close_input_file(pFormatCtx);
    pFormatCtx := nil;
  end;

  FreeAndNil(packetQueue);
  FreeAndNil(formatInfo);

  SDL_DestroyMutex(decoderLock);
  decoderLock := nil;
  SDL_DestroyMutex(parserLock);
  parserLock := nil;
  SDL_DestroyCond(resumeCond);
  resumeCond := nil;

  inherited;
end;

procedure TFFMpegDecodeStream.Close();
var
  status: integer;
begin
  // wake threads waiting for packet-queue data
  packetQueue.Abort();

  // abort parse-thread
  LockParser();
  quitRequest := true;
  SDL_CondBroadcast(resumeCond);
  UnlockParser();
  // and wait until it terminates
  if (parseThread <> nil) then
  begin
    SDL_WaitThread(parseThread, status);
    parseThread := nil;
  end;

  // NOTE: we cannot free the codecCtx or formatCtx here because
  // a formerly waiting thread in the packet-queue might require them
  // and crash if it tries to access them.
end;

procedure TFFMpegDecodeStream.LockParser();
begin
  SDL_mutexP(parserLock);
end;

procedure TFFMpegDecodeStream.UnlockParser();
begin
  SDL_mutexV(parserLock);
end;

function TFFMpegDecodeStream.GetParserMutex(): PSDL_Mutex;
begin
  Result := parserLock;
end;

procedure TFFMpegDecodeStream.LockDecoder();
begin
  SDL_mutexP(decoderLock);
end;

procedure TFFMpegDecodeStream.UnlockDecoder();
begin
  SDL_mutexV(decoderLock);
end;

function TFFMpegDecodeStream.GetLength(): real;
begin
  Result := pFormatCtx^.duration / AV_TIME_BASE;
end;

function TFFMpegDecodeStream.GetAudioFormatInfo(): TAudioFormatInfo;
begin
  Result := formatInfo;
end;

function TFFMpegDecodeStream.IsEOF(): boolean;
begin
  LockDecoder();
  Result := EOFState;
  UnlockDecoder();
end;

procedure TFFMpegDecodeStream.SetEOF(state: boolean);
begin
  LockDecoder();
  EOFState := state;
  UnlockDecoder();
end;

function TFFMpegDecodeStream.IsError(): boolean;
begin
  LockDecoder();
  Result := ErrorState;
  UnlockDecoder();
end;

procedure TFFMpegDecodeStream.SetError(state: boolean);
begin
  LockDecoder();
  ErrorState := state;
  UnlockDecoder();
end;

(*
procedure TFFMpegDecodeStream.SetError(state: boolean);
begin
  LockDecoder();
  ErrorState := state;
  UnlockDecoder();
end;

function TFFMpegDecodeStream.IsSeeking(): boolean;
begin
  LockDecoder();
  Result := seekRequest;
  UnlockDecoder();
end;
*)

function TFFMpegDecodeStream.GetPosition(): real;
begin
  // FIXME: the audio-clock might not be that accurate
  // see: tutorial on synching (audio-clock)
  Result := audioClock;
end;

procedure TFFMpegDecodeStream.SetPosition(Time: real);
begin
  LockParser();

  seekPos := Trunc(Time * AV_TIME_BASE);

  seekFlags := 0;
  // Note: the BACKWARD-flag seeks to the first position <= the position
  // searched for. Otherwise e.g. position 0 might not be seeked correct.
  // For some reason ffmpeg sometimes doesn't use position 0 but the key-frame
  // following. In streams with few key-frames (like many flv-files) the next
  // key-frame after 0 might be 5secs ahead.
  if (Time < audioClock) then
    seekFlags := AVSEEK_FLAG_BACKWARD;
  seekFlags := AVSEEK_FLAG_ANY;

LockDecoder();
  seekRequest := true;
UnlockDecoder();
  SDL_CondSignal(resumeCond);
  (*
  while ((not quitRequest) and seekRequest) do
    SDL_CondWait(seekCond, GetParserMutex());
  *)
  UnlockParser();
end;

function DecodeThreadMain(streamPtr: Pointer): integer; cdecl;
var
  stream: TFFMpegDecodeStream;
begin
  stream := TFFMpegDecodeStream(streamPtr);
  stream.ParseAudio();
  result := 0;
end;

procedure TFFMpegDecodeStream.ParseAudio();
var
  packet: TAVPacket;
  statusPacket: PAVPacket;
  seekTarget: int64;
  stopParsing: boolean;
  pbIOCtx: PByteIOContext;
  err: integer;
  index: integer;
begin
  stopParsing := false;

  while (true) do
  begin
    LockParser();

    // wait if end-of-file reached
    if (stopParsing) then
    begin
      // wait for reuse or destruction of stream
      while not (seekRequest or quitRequest) do
        SDL_CondWait(resumeCond, GetParserMutex());
    end;

    if (quitRequest) then
    begin
      UnlockParser();
      break;
    end;

    // handle seek-request
    if (seekRequest) then
    begin
      // reset status
      SetEOF(false);
      SetError(false);
      stopParsing := false;

      seekTarget := av_rescale_q(seekPos, AV_TIME_BASE_Q, ffmpegStream^.time_base);
      err := av_seek_frame(pFormatCtx, ffmpegStreamIndex, seekTarget, seekFlags);
      // seeking failed -> retry with the default stream (necessary for flv-videos and some ogg-files)
      if (err < 0) then
        err := av_seek_frame(pFormatCtx, -1, seekPos, seekFlags);
      // check if seeking failed
      if (err < 0) then
      begin
        Log.LogStatus('Seek Error in "'+pFormatCtx^.filename+'"', 'UAudioDecoder_FFMpeg');
      end
      else
      begin
        packetQueue.Flush();
        packetQueue.PutStatus(PKT_STATUS_FLAG_FLUSH, nil);
      end;
  LockDecoder();
      seekRequest := false;
  UnlockDecoder();
      SDL_CondSignal(seekCond);
    end;

    UnlockParser();

    if (packetQueue.size > MAX_AUDIOQ_SIZE) then
    begin
      SDL_Delay(10);
      continue;
    end;

    if (av_read_frame(pFormatCtx, packet) < 0) then
    begin
      // failed to read a frame, check reason

      {$IF (LIBAVFORMAT_VERSION_MAJOR >= 52)}
      pbIOCtx := pFormatCtx^.pb;
      {$ELSE}
      pbIOCtx := @pFormatCtx^.pb;
      {$IFEND}

      // check for end-of-file (eof is not an error)
      if (url_feof(pbIOCtx) <> 0) then
      begin
        // signal end-of-file
        packetQueue.putStatus(PKT_STATUS_FLAG_EOF, nil);
        stopParsing := true;
        continue;
      end;

      // check for errors
      if (url_ferror(pbIOCtx) <> 0) then
      begin
        // an error occured -> abort and wait for repositioning or termination
        packetQueue.putStatus(PKT_STATUS_FLAG_ERROR, nil);
        stopParsing := true;
        continue;
      end;

      // no error -> wait for user input
      SDL_Delay(100);
      continue;
    end;

    if (packet.stream_index = ffmpegStreamIndex) then
    begin
      packetQueue.put(@packet);
    end
    else
    begin
      av_free_packet(@packet);
    end;
  end;
end;

function TFFMpegDecodeStream.DecodeFrame(var buffer: TAudioBuffer; bufSize: integer): integer;
var
  len1,
  data_size: integer;
begin
  result := -1;

  if EOF then
    exit;

  while(true) do
  begin
    while (audio_pkt_size > 0) do
    begin
      data_size := bufSize;

      {$IF LIBAVCODEC_VERSION >= 51030000} // 51.30.0
      len1 := avcodec_decode_audio2(pCodecCtx, @buffer,
                  data_size, audio_pkt_data, audio_pkt_size);
      {$ELSE}
      // FIXME: with avcodec_decode_audio a package could contain several frames
      //        this is not handled yet
      len1 := avcodec_decode_audio(pCodecCtx, @buffer,
                  data_size, audio_pkt_data, audio_pkt_size);
      {$IFEND}

      if(len1 < 0) then
      begin
        // if error, skip frame
        {$IFDEF DebugFFMpegDecode}
        DebugWriteln( 'Skip audio frame' );
        {$ENDIF}
        audio_pkt_size := 0;
        break;
      end;

      Inc(audio_pkt_data, len1);
      Dec(audio_pkt_size, len1);

      if (data_size <= 0) then
      begin
        // no data yet, get more frames
        continue;
      end;

      //pts := audioClock;
      audioClock := audioClock + data_size /
          (1.0 * formatInfo.FrameSize * formatInfo.SampleRate);

      // we have data, return it and come back for more later
      result := data_size;
      exit;
    end;

    if (pkt.data <> nil) then
    begin
      av_free_packet(@pkt);
    end;

    // do not use an aborted queue
    if (packetQueue.abortRequest) then
      exit;

    // request a new packet and block if non available.
    // If this fails, the queue was aborted.
    if (packetQueue.Get(pkt, true) < 0) then
      exit;

    // handle Status-packet
    if (PChar(pkt.data) = STATUS_PACKET) then
    begin
      pkt.data := nil;
      audio_pkt_data := nil;
      audio_pkt_size := 0;

      case (pkt.flags) of
        PKT_STATUS_FLAG_FLUSH:
        begin
          avcodec_flush_buffers(pCodecCtx);
        end;
        PKT_STATUS_FLAG_EOF: // end-of-file
        begin
          SetEOF(true);
          // buffer contains no data -> result = -1
          exit;
        end;
        PKT_STATUS_FLAG_ERROR:
        begin
          SetError(true);
          Log.LogStatus('I/O Error', 'TFFMpegDecodeStream.DecodeFrame');
          exit;
        end;
        else
        begin
          Log.LogStatus('Unknown status', 'TFFMpegDecodeStream.DecodeFrame');
        end;
      end;

      continue;
    end;

    audio_pkt_data := PChar(pkt.data);
    audio_pkt_size := pkt.size;

    // if available, update the audio clock with pts
    if(pkt.pts <> AV_NOPTS_VALUE) then
    begin
      audioClock := av_q2d(ffmpegStream^.time_base) * pkt.pts;
    end;
  end;
end;

function TFFMpegDecodeStream.ReadData(Buffer : PChar; BufSize: integer): integer;
var
  nBytesCopy:   integer; // number of bytes to copy
  nBytesRemain: integer; // number of bytes left (remaining) to read
begin
  result := -1;

  // init number of bytes left to copy to the output buffer
  nBytesRemain := BufSize;

  // leave if end-of-file was reached previously
  if EOF then
    exit;

  LockDecoder();
  try
    if (seekRequest) then
    begin
      Result := 0;
      Exit;
    end;
  finally
    UnlockDecoder();
  end;

  // copy data to output buffer
  while (nBytesRemain > 0) do begin
    // check if we need more data
    if (audio_buf_index >= audio_buf_size) then
    begin
      // we have already sent all our data; get more
      audio_buf_size := DecodeFrame(audio_buf, sizeof(TAudioBuffer));
      // check for errors or EOF
      if(audio_buf_size < 0) then
      begin
        // fill decode-buffer with silence
        audio_buf_size := 1024;
        FillChar(audio_buf, audio_buf_size, #0);
      end;
      audio_buf_index := 0;
    end;

    // calc number of new bytes in the decode-buffer
    nBytesCopy := audio_buf_size - audio_buf_index;
    // resize copy-count if more bytes available than needed (remaining bytes are used the next time)
    if (nBytesCopy > nBytesRemain) then
      nBytesCopy := nBytesRemain;

    Move(audio_buf[audio_buf_index], Buffer[0], nBytesCopy);

    Dec(nBytesRemain,    nBytesCopy);
    Inc(Buffer,          nBytesCopy);
    Inc(audio_buf_index, nBytesCopy);
  end;

  Result := BufSize;
end;


{ TAudioDecoder_FFMpeg }

function TAudioDecoder_FFMpeg.GetName: String;
begin
  Result := 'FFMpeg_Decoder';
end;

function TAudioDecoder_FFMpeg.InitializeDecoder: boolean;
begin
  //Log.LogStatus('InitializeDecoder', 'UAudioDecoder_FFMpeg');

  av_register_all();

  Result := true;
end;

function TAudioDecoder_FFMpeg.FinalizeDecoder(): boolean;
begin
  Result := true;
end;

class function TAudioDecoder_FFMpeg.FindAudioStreamIndex(pFormatCtx : PAVFormatContext): integer;
var
  i : integer;
  streamIndex: integer;
  stream : PAVStream;
begin
  // find the first audio stream
  streamIndex := -1;

  for i := 0 to pFormatCtx^.nb_streams-1 do
  begin
    //Log.LogStatus('aFormatCtx.streams[i] : ' + inttostr(i), 'UAudio_FFMpeg');
    stream := pFormatCtx^.streams[i];

    if ( stream.codec^.codec_type = CODEC_TYPE_AUDIO ) then
    begin
      //Log.LogStatus('Found Audio Stream', 'UAudio_FFMpeg');
      streamIndex := i;
      break;
    end;
  end;

  Result := streamIndex;
end;

function TAudioDecoder_FFMpeg.Open(const Filename: string): TAudioDecodeStream;
var
  pFormatCtx     : PAVFormatContext;
  pCodecCtx      : PAVCodecContext;
  pCodec         : PAVCodec;
  ffmpegStreamID : Integer;
  ffmpegStream   : PAVStream;
  stream         : TFFMpegDecodeStream;
begin
  Result := nil;

  if (not FileExists(Filename)) then
  begin
    Log.LogError('Audio-file does not exist: "' + Filename + '"', 'UAudio_FFMpeg');
    exit;
  end;

  // open audio file
  if (av_open_input_file(pFormatCtx, PChar(Filename), nil, 0, nil) <> 0) then
  begin
    Log.LogError('av_open_input_file failed: "' + Filename + '"', 'UAudio_FFMpeg');
    exit;
  end;

  // TODO: do we need to generate PTS values if they do not exist?
  //pFormatCtx^.flags := pFormatCtx^.flags or AVFMT_FLAG_GENPTS;

  // retrieve stream information
  if (av_find_stream_info(pFormatCtx) < 0) then
  begin
    Log.LogError('av_find_stream_info failed: "' + Filename + '"', 'UAudio_FFMpeg');
    av_close_input_file(pFormatCtx);
    exit;
  end;

  // FIXME: hack used by ffplay. Maybe should not use url_feof() to test for the end
  pFormatCtx^.pb.eof_reached := 0;

  {$IFDEF DebugFFMpegDecode}
  dump_format(pFormatCtx, 0, pchar(Filename), 0);
  {$ENDIF}

  ffmpegStreamID := FindAudioStreamIndex(pFormatCtx);
  if (ffmpegStreamID < 0) then
  begin
    Log.LogError('FindAudioStreamIndex: No Audio-stream found "' + Filename + '"', 'UAudio_FFMpeg');
    av_close_input_file(pFormatCtx);
    exit;
  end;

  //Log.LogStatus('AudioStreamIndex is: '+ inttostr(ffmpegStreamID), 'UAudio_FFMpeg');

  ffmpegStream := pFormatCtx.streams[ffmpegStreamID];
  pCodecCtx := ffmpegStream^.codec;

  pCodec := avcodec_find_decoder(pCodecCtx^.codec_id);
  if (pCodec = nil) then
  begin
    Log.LogError('Unsupported codec!', 'UAudio_FFMpeg');
    av_close_input_file(pFormatCtx);
    exit;
  end;

  // set debug options
  pCodecCtx^.debug_mv := 0;
  pCodecCtx^.debug := 0;

  // detect bug-workarounds automatically
  pCodecCtx^.workaround_bugs := FF_BUG_AUTODETECT;
  // error resilience strategy (careful/compliant/agressive/very_aggressive)
  //pCodecCtx^.error_resilience := FF_ER_CAREFUL; //FF_ER_COMPLIANT;
  // allow non spec compliant speedup tricks.
  //pCodecCtx^.flags2 := pCodecCtx^.flags2 or CODEC_FLAG2_FAST;

  // Note: avcodec_open() is not thread-safe!
  if (avcodec_open(pCodecCtx, pCodec) < 0) then
  begin
    Log.LogError('avcodec_open failed!', 'UAudio_FFMpeg');
    exit;
  end;

  // TODO: what about pCodecCtx^.start_time? Should we seek to this position here?
  // ...

  stream := TFFMpegDecodeStream.Create(pFormatCtx, pCodecCtx, pCodec,
              ffmpegStreamID, ffmpegStream);

  Result := stream;
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
  Flush();
  SDL_DestroyMutex(mutex);
  SDL_DestroyCond(cond);
  inherited;
end;

procedure TPacketQueue.Abort();
begin
  SDL_LockMutex(mutex);

  abortRequest := true;

  SDL_CondSignal(cond);
  SDL_UnlockMutex(mutex);
end;

function TPacketQueue.Put(pkt : PAVPacket): integer;
var
  pkt1 : PAVPacketList;
begin
  result := -1;

  if (pkt = nil) then
    exit;
  
  if (PChar(pkt^.data) <> STATUS_PACKET) then
  begin
    if (av_dup_packet(pkt) < 0) then
      exit;
  end;

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

    Self.size := Self.size + pkt1^.pkt.size;
    SDL_CondSignal(Self.cond);
  finally
    SDL_UnlockMutex(Self.mutex);
  end;

  Result := 0;
end;

function TPacketQueue.PutStatus(statusFlag: integer; statusInfo: Pointer): integer;
var
  pkt: PAVPacket;
begin
  // create temp. package
  pkt := av_malloc(SizeOf(TAVPacket));
  if (pkt = nil) then
  begin
    Result := -1;
    Exit;
  end;
  // init package
  av_init_packet(pkt^);
  pkt^.data  := Pointer(STATUS_PACKET);
  pkt^.flags := statusFlag;
  pkt^.priv  := statusInfo;
  // put a copy of the package into the queue
  Result := Put(pkt);
  // data has been copied -> delete temp. package
  av_free(pkt);
end;

function TPacketQueue.Get(var pkt: TAVPacket; block: boolean): integer;
var
  pkt1 : PAVPacketList;
begin
  Result := -1;

  SDL_LockMutex(Self.mutex);
  try
    while true do
    begin
      if (abortRequest) then
        exit;

      pkt1 := Self.firstPkt;
      if (pkt1 <> nil) then
      begin
        Self.firstPkt := pkt1^.next;
        if (Self.firstPkt = nil) then
          Self.lastPkt := nil;
        dec(Self.nbPackets);

        Self.size := Self.size - pkt1^.pkt.size;
        pkt := pkt1^.pkt;
        av_free(pkt1);

        result := 1;
        break;
      end
      else if (not block) then
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

procedure TPacketQueue.Flush();
var
  pkt, pkt1: PAVPacketList;
begin
  SDL_LockMutex(Self.mutex);

  pkt := Self.firstPkt;
  while(pkt <> nil) do
  begin
    pkt1 := pkt^.next;
    av_free_packet(@pkt^.pkt);
    // Note: param must be a pointer to a pointer!
    av_freep(@pkt);
    pkt := pkt1;
  end;
  Self.lastPkt := nil;
  Self.firstPkt := nil;
  Self.nbPackets := 0;
  Self.size := 0;

  SDL_UnlockMutex(Self.mutex);
end;


initialization
  singleton_AudioDecoderFFMpeg := TAudioDecoder_FFMpeg.create();

  //writeln( 'UAudioDecoder_FFMpeg - Register Decoder' );
  AudioManager.add( singleton_AudioDecoderFFMpeg );

finalization
  AudioManager.Remove( singleton_AudioDecoderFFMpeg );


end.
