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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/media/UAudioDecoder_FFmpeg.pas $
 * $Id: UAudioDecoder_FFmpeg.pas 3107 2014-11-23 00:02:56Z k-m_schindler $
 *}

unit UAudioDecoder_FFmpeg;

(*******************************************************************************
 *
 * This unit is primarily based upon -
 *   http://www.dranger.com/ffmpeg/ffmpegtutorial_all.html
 *
 *   and tutorial03.c
 *
 *   http://www.inb.uni-luebeck.de/~boehme/using_libavcodec.html
 *
 *******************************************************************************)

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

// show FFmpeg specific debug output
{.$DEFINE DebugFFmpegDecode}

// FFmpeg is very verbose and shows a bunch of errors.
// Those errors (they can be considered as warnings by us) can be ignored
// as they do not give any useful information.
// There is no solution to fix this except for turning them off.
{.$DEFINE EnableFFmpegErrorOutput}

implementation

uses
  sdl2, // SDL redefines some base types -> include before SysUtils to ignore them
  Classes,
  Math,
  SysUtils,
  avcodec,
  avformat,
  avutil,
  avio,
	ctypes,
  rational,
  swresample,
  UMusic,
  UIni,
  UMain,
  UMediaCore_FFmpeg,
  ULog,
  UCommon,
  UConfig,
  UPath;

const
  MAX_AUDIOQ_SIZE = (5 * 16 * 1024);

const
  // TODO: The factor 3/2 might not be necessary as we do not need extra
  // space for synchronizing as in the tutorial.
  AUDIO_BUFFER_SIZE = (192000 * 3) div 2;

type
  TFFmpegDecodeStream = class(TAudioDecodeStream)
    private
      fStateLock:   PSDL_Mutex;

      fEOFState:   boolean; // end-of-stream flag (locked by StateLock)
      fErrorState: boolean; // error flag (locked by StateLock)

      fQuitRequest: boolean; // (locked by StateLock)
      fParserIdleCond: PSDL_Cond;

      // parser pause/resume data
      fParserLocked:            boolean;
      fParserPauseRequestCount: integer;
      fParserUnlockedCond:      PSDL_Cond;
      fParserResumeCond:        PSDL_Cond;

      fSeekRequest: boolean; // (locked by StateLock)
      fSeekFlags:   integer; // (locked by StateLock)
      fSeekPos:     double;    // stream position to seek for (in secs) (locked by StateLock)
      fSeekFlush:   boolean;   // true if the buffers should be flushed after seeking (locked by StateLock)
      SeekFinishedCond: PSDL_Cond;

      fLoop: boolean; // (locked by StateLock)

      fParseThread: PSDL_Thread;
      fPacketQueue: TPacketQueue;

      fFormatInfo: TAudioFormatInfo;
      fBytesPerSample: integer;
      fSwrContext: PSwrContext;

      // FFmpeg specific data
      fFormatCtx: PAVFormatContext;
      fCodecCtx:  PAVCodecContext;
      fCodec:     PAVCodec;

      fAudioStreamIndex: integer;
      fAudioStream: PAVStream;
      fAudioStreamPos: double; // stream position in seconds (locked by DecoderLock)

      // decoder pause/resume data
      fDecoderLocked:            boolean;
      fDecoderPauseRequestCount: integer;
      fDecoderUnlockedCond:      PSDL_Cond;
      fDecoderResumeCond:        PSDL_Cond;

      // state-vars for DecodeFrame (locked by DecoderLock)
      fAudioPaket:        PAVPacket;
      fAudioPaketSize:    integer;
      fAudioPaketSilence: integer; // number of bytes of silence to return

      // state-vars for AudioCallback (locked by DecoderLock)
      fAudioBufferPos:  integer;
      fAudioBufferSize: integer;
      fAudioBuffer:     PByteArray;
      fAudioBufferFrame: PAVFrame;

      fFilename: IPath;

      procedure SetPositionIntern(Time: real; Flush: boolean);
      procedure SetEOF(State: boolean);   {$IFDEF HasInline}inline;{$ENDIF}
      procedure SetError(State: boolean); {$IFDEF HasInline}inline;{$ENDIF}
      function IsSeeking(): boolean;
      function IsQuit(): boolean;
      function GetLoopInternal(): boolean;

      procedure Reset();

      procedure Parse();
      function ParseLoop(): boolean;
      function PauseParser(): boolean;
      procedure ResumeParser();

      function DecodeFrame(): integer;
      procedure FlushCodecBuffers();
      procedure PauseDecoderUnlocked();
      procedure ResumeDecoderUnlocked();
      procedure PauseDecoder();
      procedure ResumeDecoder();
    public
      constructor Create();
      destructor Destroy(); override;

      function Open(const Filename: IPath): boolean;
      procedure Close();                     override;

      function GetLength(): real;            override;
      function GetAudioFormatInfo(): TAudioFormatInfo; override;
      function GetPosition: real;            override;
      procedure SetPosition(Time: real);     override;
      function GetLoop(): boolean;           override;
      procedure SetLoop(Enabled: boolean);   override;
      function IsEOF(): boolean;             override;
      function IsError(): boolean;           override;

      function ReadData(Buffer: PByte; BufferSize: integer): integer; override;
  end;

type
  TAudioDecoder_FFmpeg = class(TInterfacedObject, IAudioDecoder)
    public
      function GetName: string;

      function InitializeDecoder(): boolean;
      function FinalizeDecoder(): boolean;
      function Open(const Filename: IPath): TAudioDecodeStream;
  end;

var
  FFmpegCore: TMediaCore_FFmpeg;

function ParseThreadMain(Data: Pointer): integer; cdecl; forward;

function CodecCanFlush(ctx: PAVCodecContext): boolean;
begin
{$IF LIBAVCODEC_VERSION < 60000000}
	Result := @ctx.codec.flush <> nil;
{$ELSE}
	Result := true;
{$ENDIF}
end;

{ TFFmpegDecodeStream }

constructor TFFmpegDecodeStream.Create();
begin
  inherited Create();

  fStateLock := SDL_CreateMutex();
  fParserUnlockedCond := SDL_CreateCond();
  fParserResumeCond := SDL_CreateCond();
  fParserIdleCond := SDL_CreateCond();
  SeekFinishedCond := SDL_CreateCond();
  fDecoderUnlockedCond := SDL_CreateCond();
  fDecoderResumeCond := SDL_CreateCond();
  fAudioBufferFrame := av_frame_alloc();

  Reset();
end;

procedure TFFmpegDecodeStream.Reset();
begin
  fParseThread := nil;

  fEOFState := false;
  fErrorState := false;
  fLoop := false;
  fQuitRequest := false;

  fAudioPaket := nil;
  fAudioPaketSize := 0;
  fAudioPaketSilence := 0;

  fAudioBufferPos := 0;
  fAudioBufferSize := 0;

  fParserLocked := false;
  fParserPauseRequestCount := 0;
  fDecoderLocked := false;
  fDecoderPauseRequestCount := 0;
end;

{*
 * Frees the decode-stream data.
 *}
destructor TFFmpegDecodeStream.Destroy();
begin
  Close();

  SDL_DestroyMutex(fStateLock);
  fStateLock:=nil;
  SDL_DestroyCond(fParserUnlockedCond);
  SDL_DestroyCond(fParserResumeCond);
  SDL_DestroyCond(fParserIdleCond);
  SDL_DestroyCond(SeekFinishedCond);
  SDL_DestroyCond(fDecoderUnlockedCond);
  SDL_DestroyCond(fDecoderResumeCond);
  av_frame_free(@fAudioBufferFrame);

  inherited;
end;

function TFFmpegDecodeStream.Open(const Filename: IPath): boolean;
var
  SampleFormat: TAudioSampleFormat;
  PackedSampleFormat: TAVSampleFormat;
  AVResult: integer;
  CodecID: TAVCodecID;
  NumChannels: cint;
  {$IF LIBAVUTIL_VERSION >= 59000000}
  RequestChannelLayout: TAVChannelLayout;
  {$ENDIF}
begin
  Result := false;

  Close();
  Reset();

  if (not Filename.IsFile) then
  begin
    Log.LogError('Audio-file does not exist: "' + Filename.ToNative + '"', 'UAudio_FFmpeg');
    Exit;
  end;

  Self.fFilename := Filename;

  // use custom 'ufile' protocol for UTF-8 support
  AVResult := FFmpegCore.AVFormatOpenInput(@fFormatCtx, PAnsiChar('ufile:'+FileName.ToUTF8));
  if (AVResult <> 0) then
  begin
    Log.LogError('Failed to open file "' + Filename.ToNative + '" ('+FFmpegCore.GetErrorString(AVResult)+')', 'UAudio_FFmpeg');
    Exit;
  end;

  // generate PTS values if they do not exist
  fFormatCtx^.flags := fFormatCtx^.flags or AVFMT_FLAG_GENPTS;

  // retrieve stream information
  AVResult := avformat_find_stream_info(fFormatCtx, nil);
  if (AVResult < 0) then
  begin
    Log.LogError('No stream info found: "' + Filename.ToNative + '"', 'UAudio_FFmpeg');
    Close();
    Exit;
  end;

  // FIXME: hack used by ffplay. Maybe should not use url_feof() to test for the end
  fFormatCtx^.pb.eof_reached := 0;

  {$IFDEF DebugFFmpegDecode}
  dump_format(fFormatCtx, 0, PAnsiChar(Filename.ToNative), 0);
  {$ENDIF}

  fAudioStreamIndex := FFmpegCore.FindAudioStreamIndex(fFormatCtx);
  if (fAudioStreamIndex < 0) then
  begin
    Log.LogError('FindAudioStreamIndex: No Audio-stream found "' + Filename.ToNative + '"', 'UAudio_FFmpeg');
    Close();
    Exit;
  end;

  //Log.LogStatus('AudioStreamIndex is: '+ inttostr(ffmpegStreamID), 'UAudio_FFmpeg');

  fAudioStream := PPAVStream(PtrUInt(fFormatCtx.streams) + fAudioStreamIndex * Sizeof(pointer))^;
  fAudioStreamPos := 0;

{$IF LIBAVFORMAT_VERSION < 59000000}
  CodecID := fAudioStream^.codec^.codec_id;
{$ELSE}
  CodecID := fAudioStream^.codecpar^.codec_id;
{$ENDIF}

  fCodec := avcodec_find_decoder(CodecID);
  if (fCodec = nil) then
  begin
    Log.LogError('Unsupported codec!', 'UAudio_FFmpeg');
    fCodecCtx := nil;
    Close();
    Exit;
  end;

  fCodecCtx := FFmpegCore.GetCodecContext(fAudioStream, fCodec);
  if fCodecCtx = nil then
  begin
    Close();
    Exit;
  end;

  {$IF LIBAVUTIL_VERSION >= 59000000}
  NumChannels := fCodecCtx^.ch_layout.nb_channels;
  {$ELSE}
  NumChannels := fCodecCtx^.channels;
  {$IFEND}

  // TODO: should we use this or not? Should we allow 5.1 channel audio?
  {$IF LIBAVUTIL_VERSION >= 59000000}
  if av_channel_layout_from_string(@RequestChannelLayout, 'downmix') = 0 then
    av_opt_set_chlayout(fCodecCtx, 'downmix', @RequestChannelLayout, 0); // ignore errors, few codecs support downmix
  {$ELSE}
  fCodecCtx^.request_channel_layout := ($20000000 or $40000000); //avcodec.c AV_CH_LAYOUT_STEREO_DOWNMIX;
  {$IFEND}

  // set debug options
  fCodecCtx^.debug := 0;

  // request required sample format
  // reference:
  // http://stackoverflow.com/questions/16479662/ffmpeg-1-0-causing-audio-playback-issues
  // without this avcodec_open2 returns AV_SAMPLE_FMT_S16P
  fCodecCtx^.request_sample_fmt := AV_SAMPLE_FMT_S16;

  // detect bug-workarounds automatically
  fCodecCtx^.workaround_bugs := FF_BUG_AUTODETECT;
  // error resilience strategy (careful/compliant/agressive/very_aggressive)
  //CodecCtx^.error_resilience := FF_ER_CAREFUL; //FF_ER_COMPLIANT;
  // allow non spec compliant speedup tricks.
  //CodecCtx^.flags2 := CodecCtx^.flags2 or CODEC_FLAG2_FAST;

  // Note: avcodec_open() and avcodec_close() are not thread-safe and will
  // fail if called concurrently by different threads.
  FFmpegCore.LockAVCodec();
  try
    AVResult := avcodec_open2(fCodecCtx, fCodec, nil);
  finally
    FFmpegCore.UnlockAVCodec();
  end;
  if (AVResult < 0) then
  begin
    Log.LogError('avcodec_open failed!', 'UAudio_FFmpeg');
    Close();
    Exit;
  end;

  // now initialize the audio-format
  PackedSampleFormat := av_get_packed_sample_fmt(fCodecCtx^.sample_fmt);
  if (PackedSampleFormat <> fCodecCtx^.sample_fmt) then
  begin
    // There is no point in leaving PackedSampleFormat as is.
    // av_audio_resample_init as used by TAudioConverter_FFmpeg will internally
    // convert to AV_SAMPLE_FMT_S16 anyway and most architectures have assembly
    // optimized conversion routines from AV_SAMPLE_FMT_FLTP to AV_SAMPLE_FMT_S16.
    PackedSampleFormat := AV_SAMPLE_FMT_S16;
    {$IF LIBAVUTIL_VERSION >= 59000000}
    fSwrContext := nil;
    swr_alloc_set_opts2(@fSwrContext, @fCodecCtx^.ch_layout, PackedSampleFormat, fCodecCtx^.sample_rate,
                        @fCodecCtx^.ch_layout, fCodecCtx^.sample_fmt, fCodecCtx^.sample_rate, 0, nil);
    {$ELSE}
    fSwrContext := swr_alloc_set_opts(nil, fCodecCtx^.channel_layout, PackedSampleFormat, fCodecCtx^.sample_rate,
                                      fCodecCtx^.channel_layout, fCodecCtx^.sample_fmt, fCodecCtx^.sample_rate, 0, nil);
    {$IFEND}
    if (fSwrContext = nil) then
      Log.LogStatus('Error: Failed to create SwrContext', 'TFFmpegDecodeStream.Open')
    else
    begin
      {$IF LIBAVUTIL_VERSION < 59000000}
      // Not necessary when channel_layout has been set correctly
      av_opt_set_int(fSwrContext, 'ich', NumChannels, 0);
      av_opt_set_int(fSwrContext, 'och', NumChannels, 0);
      {$IFEND}
      if (swr_init(fSwrContext) < 0) then
      begin
        swr_free(@fSwrContext);
        Log.LogStatus('Error: Failed to initialize SwrContext', 'TFFmpegDecodeStream.Open');
      end;
    end;
  end;

  if (not FFmpegCore.ConvertFFmpegToAudioFormat(PackedSampleFormat, SampleFormat)) then
  begin
    // try standard format
    SampleFormat := asfS16;
  end;
  if NumChannels > 255 then
    Log.LogStatus('Error: Number of channels > 255', 'TFFmpegDecodeStream.Open');
  fFormatInfo := TAudioFormatInfo.Create(
    byte(NumChannels),
    fCodecCtx^.sample_rate,
    SampleFormat
  );
  fBytesPerSample := av_get_bytes_per_sample(PackedSampleFormat) * NumChannels;
  fPacketQueue := TPacketQueue.Create();

  // finally start the decode thread
  fParseThread := SDL_CreateThread(@ParseThreadMain, nil, Self);

  Result := true;
end;

procedure TFFmpegDecodeStream.Close();
var
  ThreadResult: integer;
begin
  // wake threads waiting for packet-queue data
  // Note: normally, there are no waiting threads. If there were waiting
  // ones, they would block the audio-callback thread.
  if (assigned(fPacketQueue)) then
    fPacketQueue.Abort();

  // send quit request (to parse-thread etc)
  SDL_LockMutex(fStateLock);
  fQuitRequest := true;
  SDL_CondBroadcast(fParserIdleCond);
  SDL_UnlockMutex(fStateLock);

  // abort parse-thread
  if (fParseThread <> nil) then
  begin
    // and wait until it terminates
    SDL_WaitThread(fParseThread, @ThreadResult);
    SDL_LockMutex(fStateLock);
    fParseThread := nil;
    SDL_CondBroadcast(SeekFinishedCond);
    SDL_UnlockMutex(fStateLock);
  end;

  // Free the swresample context
  if (fSwrContext <> nil) then
    swr_free(@fSwrContext);

  // Close the codec
  if (fCodecCtx <> nil) then
  begin
    // avcodec_close() is not thread-safe
    FFmpegCore.LockAVCodec();
    try
      {$IF LIBAVFORMAT_VERSION < 59000000}
      avcodec_close(fCodecCtx);
      {$ELSE}
      avcodec_free_context(@fCodecCtx);
      {$ENDIF}
    finally
      FFmpegCore.UnlockAVCodec();
    end;
    {$IF LIBAVFORMAT_VERSION < 59000000}
    fCodecCtx := nil;
    {$ENDIF}
  end;

  // Close the video file
  if (fFormatCtx <> nil) then
  begin
    FFmpegCore.AVFormatCloseInput(@fFormatCtx);
    fFormatCtx := nil;
  end;

  av_packet_free(@fAudioPaket);

  PerformOnClose();
  
  FreeAndNil(fPacketQueue);
  FreeAndNil(fFormatInfo);
end;

function TFFmpegDecodeStream.GetLength(): real;
var
  start_time: cint64;
begin
  start_time := fFormatCtx^.start_time;
  // AV_NOPTS_VALUE is returned if no explicit start_time is available.
  if start_time = AV_NOPTS_VALUE then
    start_time := 0;

  // there is a type size mismatch warnign because start_time and duration are cint64.
  // So, in principle there could be an overflow when doing the sum.
  Result := (start_time + fFormatCtx^.duration) / AV_TIME_BASE;
end;

function TFFmpegDecodeStream.GetAudioFormatInfo(): TAudioFormatInfo;
begin
  Result := fFormatInfo;
end;

function TFFmpegDecodeStream.IsEOF(): boolean;
begin
  SDL_LockMutex(fStateLock);
  Result := fEOFState;
  SDL_UnlockMutex(fStateLock);
end;

procedure TFFmpegDecodeStream.SetEOF(State: boolean);
begin
  SDL_LockMutex(fStateLock);
  fEOFState := State;
  SDL_UnlockMutex(fStateLock);
end;

function TFFmpegDecodeStream.IsError(): boolean;
begin
  SDL_LockMutex(fStateLock);
  Result := fErrorState;
  SDL_UnlockMutex(fStateLock);
end;

procedure TFFmpegDecodeStream.SetError(State: boolean);
begin
  SDL_LockMutex(fStateLock);
  fErrorState := State;
  SDL_UnlockMutex(fStateLock);
end;

function TFFmpegDecodeStream.IsSeeking(): boolean;
begin
  SDL_LockMutex(fStateLock);
  Result := fSeekRequest;
  SDL_UnlockMutex(fStateLock);
end;

function TFFmpegDecodeStream.IsQuit(): boolean;
begin
  Result := fQuitRequest;
end;

function TFFmpegDecodeStream.GetPosition(): real;
var
  BufferSizeSec: double;
begin
  PauseDecoder();

  // ReadData() does not return all of the buffer retrieved by DecodeFrame().
  // Determine the size of the unused part of the decode-buffer.
  BufferSizeSec := (fAudioBufferSize - fAudioBufferPos) /
                   fFormatInfo.BytesPerSec;

  // subtract the size of unused buffer-data from the audio clock.
  Result := fAudioStreamPos - BufferSizeSec;

  ResumeDecoder();
end;

procedure TFFmpegDecodeStream.SetPosition(Time: real);
begin
  // - Pause the parser first to prevent it from putting obsolete packages
  //   into the queue after the queue was flushed and before seeking is done.
  //   Otherwise we will hear fragments of old data, if the stream was seeked
  //   in stopped mode and resumed afterwards (applies to non-blocking mode only).
  SDL_LockMutex(fStateLock);
  if (PauseParser()) then
  begin
    SetPositionIntern(Time, true);
    ResumeParser();

    // wait until seeking is done
    while ((fParseThread <> nil) and fSeekRequest) do
      SDL_CondWait(SeekFinishedCond, fStateLock);
  end;
  SDL_UnlockMutex(fStateLock);
end;

function TFFmpegDecodeStream.GetLoopInternal(): boolean;
begin
  Result := fLoop;
end;

function TFFmpegDecodeStream.GetLoop(): boolean;
begin
  SDL_LockMutex(fStateLock);
  Result := GetLoopInternal();
  SDL_UnlockMutex(fStateLock);
end;

procedure TFFmpegDecodeStream.SetLoop(Enabled: boolean);
begin
  SDL_LockMutex(fStateLock);
  fLoop := Enabled;
  SDL_UnlockMutex(fStateLock);
end;


(********************************************
 * Parser section
 ********************************************)

function TFFmpegDecodeStream.PauseParser(): boolean;
begin
  Result := true;
  Inc(fParserPauseRequestCount);
  while (fParserLocked) do
    SDL_CondWait(fParserUnlockedCond, fStateLock);
  if (fParseThread = nil) then
  begin
    Dec(fParserPauseRequestCount);
    Result := false;
  end;
end;

procedure TFFmpegDecodeStream.ResumeParser();
begin
  Dec(fParserPauseRequestCount);
  SDL_CondSignal(fParserResumeCond);
end;

procedure TFFmpegDecodeStream.SetPositionIntern(Time: real; Flush: boolean);
begin
  // - The state lock has already been locked.
  // - Pause the decoder to avoid race-condition that might occur otherwise.
  PauseDecoderUnlocked();
  try
    fEOFState := false;
    fErrorState := false;

    // do not seek if we are already at the correct position.
    // This is important especially for seeking to position 0 if we already are
    // at the beginning. Although seeking with AVSEEK_FLAG_BACKWARD for pos 0 works,
    // it is still a bit choppy (although much better than w/o AVSEEK_FLAG_BACKWARD).
    if (Time = fAudioStreamPos) then
      Exit;    

    // configure seek parameters
    fSeekPos := Time;
    fSeekFlush := Flush;
    fSeekFlags := AVSEEK_FLAG_ANY;
    fSeekRequest := true;

    // Note: the BACKWARD-flag seeks to the first position <= the position
    // searched for. Otherwise e.g. position 0 might not be seeked correct.
    // For some reason ffmpeg sometimes doesn't use position 0 but the key-frame
    // following. In streams with few key-frames (like many flv-files) the next
    // key-frame after 0 might be 5secs ahead.
    if (Time <= fAudioStreamPos) then
      fSeekFlags := fSeekFlags or AVSEEK_FLAG_BACKWARD;

    // send a reuse signal in case the parser was stopped (e.g. because of an EOF)
    SDL_CondSignal(fParserIdleCond);
  finally
    ResumeDecoderUnlocked();
  end;
end;

function ParseThreadMain(Data: Pointer): integer; cdecl;
var
  Stream: TFFmpegDecodeStream;
begin
  Stream := TFFmpegDecodeStream(Data);
  if (Stream <> nil) then
    Stream.Parse();
  Result := 0;
end;

procedure TFFmpegDecodeStream.Parse();
begin
  // reuse thread as long as the stream is not terminated
  SDL_LockMutex(fStateLock);
  while (ParseLoop()) do
  begin
    // wait for reuse or destruction of stream
    while (not (fSeekRequest or fQuitRequest)) do
      SDL_CondWait(fParserIdleCond, fStateLock);
  end;
  SDL_UnlockMutex(fStateLock);
end;

(**
 * Parser main loop.
 * Will not return until parsing of the stream is finished.
 * Reasons for the parser to return are:
 * - the end-of-file is reached
 * - an error occured
 * - the stream was quited (received a quit-request)
 * Returns true if the stream can be resumed or false if the stream has to
 * be terminated.
 * Must be called and returns with fStateLock locked but temporarily
 * unlocks it.
 *)
function TFFmpegDecodeStream.ParseLoop(): boolean;
var
  Packet: PAVPacket;
  SeekTarget: int64;
  ErrorCode: integer;
  StartSilence: double;       // duration of silence at start of stream
  StartSilencePtr: PDouble;  // pointer for the EMPTY status packet 
  fileSize: integer;
  urlError: integer;
  errnum: integer;

  // Note: pthreads wakes threads waiting on a mutex in the order of their
  // priority and not in FIFO order. SDL does not provide any option to
  // control priorities. This might (and already did) starve threads waiting
  // on the mutex (e.g. SetPosition) making usdx look like it was froozen.
  // Instead of simply locking the critical section we set a ParserLocked flag
  // instead and give priority to the threads requesting the parser to pause.
  function LockParser(): boolean;
  begin
    while ((fParserPauseRequestCount > 0) and not fQuitRequest) do
      SDL_CondWait(fParserResumeCond, fStateLock);
    if (not fQuitRequest) then
      fParserLocked := true;
    Result := fParserLocked;
  end;

  procedure UnlockParser();
  begin
    fParserLocked := false;
    SDL_CondBroadcast(fParserUnlockedCond);
  end;

begin
  Result := true;
  Packet := nil;

  while LockParser() do
  begin
    SDL_UnlockMutex(fStateLock);
    try
      // handle seek-request (Note: no need to lock SeekRequest here)
      if (fSeekRequest) then
      begin
        // first try: seek on the audio stream
        SeekTarget := Round(fSeekPos / av_q2d(fAudioStream^.time_base));
        StartSilence := 0;
        if (SeekTarget < fAudioStream^.start_time) then
          StartSilence := (fAudioStream^.start_time - SeekTarget) * av_q2d(fAudioStream^.time_base);
        ErrorCode := av_seek_frame(fFormatCtx, fAudioStreamIndex, SeekTarget, fSeekFlags);

        if (ErrorCode < 0) then
        begin
          // second try: seek on the default stream (necessary for flv-videos and some ogg-files)
          SeekTarget := Round(fSeekPos * AV_TIME_BASE);
          StartSilence := 0;
          if (SeekTarget < fFormatCtx^.start_time) then
            StartSilence := (fFormatCtx^.start_time - SeekTarget) / AV_TIME_BASE;
          ErrorCode := av_seek_frame(fFormatCtx, -1, SeekTarget, fSeekFlags);
        end;

        // pause decoder and lock state (keep the lock-order to avoid deadlocks).
        // Note that the decoder does not block in the packet-queue in seeking state,
        // so locking the decoder here does not cause a dead-lock.
        SDL_LockMutex(fStateLock);
        PauseDecoderUnlocked();
        try
          if (ErrorCode < 0) then
          begin
            // seeking failed
            fErrorState := true;
            Log.LogError('Seek Error in "'+fFormatCtx^.url+'"', 'UAudioDecoder_FFmpeg');
          end
          else
          begin
            if (fSeekFlush) then
            begin
              // flush queue (we will send a Flush-Packet when seeking is finished)
              fPacketQueue.Flush();

              // flush the decode buffers
              fAudioBufferSize := 0;
              fAudioBufferPos := 0;
              fAudioPaketSize := 0;
              fAudioPaketSilence := 0;
              FlushCodecBuffers();
              
              // Set preliminary stream position. The position will be set to
              // the correct value as soon as the first packet is decoded.
              fAudioStreamPos := fSeekPos;
            end
            else
            begin
              // request avcodec buffer flush
              fPacketQueue.PutStatus(PKT_STATUS_FLAG_FLUSH, nil);
            end;

            // fill the gap between position 0 and start_time with silence
            // but not if we are in loop mode
            if ((StartSilence > 0) and (not fLoop)) then
            begin
              GetMem(StartSilencePtr, SizeOf(StartSilence));
              StartSilencePtr^ := StartSilence;
              fPacketQueue.PutStatus(PKT_STATUS_FLAG_EMPTY, StartSilencePtr);
            end;
          end;

          fSeekRequest := false;
          SDL_CondBroadcast(SeekFinishedCond);
        finally
          ResumeDecoderUnlocked();
          SDL_UnlockMutex(fStateLock);
        end;
      end;

      if (fPacketQueue.GetSize() > MAX_AUDIOQ_SIZE) then
      begin
        SDL_Delay(10);
        Continue;
      end;

      if (Packet = nil) then
        Packet := av_packet_alloc();
      errnum := av_read_frame(fFormatCtx, Packet);
      if (errnum < 0) then
      begin

        // check for end-of-file (eof is not an error)
        if (errnum = AVERROR_EOF) then
        begin
          SDL_LockMutex(fStateLock);
          if (GetLoopInternal()) then
          begin
            // rewind stream (but do not flush)
            SetPositionIntern(0, false);
            SDL_UnlockMutex(fStateLock);
            Continue;
          end
          else
          begin
            SDL_UnlockMutex(fStateLock);
            // signal end-of-file
            fPacketQueue.PutStatus(PKT_STATUS_FLAG_EOF, nil);
            Break;
          end;
        end;

        // check for errors
        urlError := fFormatCtx^.pb^.error;
        if (urlError <> 0) then
        begin
          // an error occured -> abort and wait for repositioning or termination
          fPacketQueue.PutStatus(PKT_STATUS_FLAG_ERROR, nil);
          Break;
        end;

        // unknown error occured, exit
        fPacketQueue.PutStatus(PKT_STATUS_FLAG_ERROR, nil);
        Break;
      end;

      if (Packet^.stream_index = fAudioStreamIndex) then
      begin;
        fPacketQueue.Put(Packet);
        Packet := nil;
      end

      else
        av_packet_unref(Packet);

    finally
      SDL_LockMutex(fStateLock);
      UnlockParser();
    end;
  end;
  av_packet_free(@Packet);
  if (IsQuit()) then
  begin
    Result := false;
  end;
end;


(********************************************
 * Decoder section
 ********************************************)

procedure TFFmpegDecodeStream.PauseDecoderUnlocked();
begin
  Inc(fDecoderPauseRequestCount);
  while (fDecoderLocked) do
    SDL_CondWait(fDecoderUnlockedCond, fStateLock);
end;

procedure TFFmpegDecodeStream.PauseDecoder();
begin
  SDL_LockMutex(fStateLock);
  PauseDecoderUnlocked();
  SDL_UnlockMutex(fStateLock);
end;

procedure TFFmpegDecodeStream.ResumeDecoderUnlocked();
begin
  Dec(fDecoderPauseRequestCount);
  SDL_CondSignal(fDecoderResumeCond);
end;

procedure TFFmpegDecodeStream.ResumeDecoder();
begin
  SDL_LockMutex(fStateLock);
  ResumeDecoderUnlocked();
  SDL_UnlockMutex(fStateLock);
end;

procedure TFFmpegDecodeStream.FlushCodecBuffers();
{$IF LIBAVFORMAT_VERSION >= 59000000}
var
  NewCtx: PAVCodecContext;
{$ENDIF}
begin
  // if no flush operation is specified, avcodec_flush_buffers will not do anything.
  if CodecCanFlush(fCodecCtx) then
  begin
    // flush buffers used by avcodec_decode_audio, etc.
    avcodec_flush_buffers(fCodecCtx);
  end
  else
  begin
    // we need a Workaround to avoid plopping noise with ogg-vorbis and
    // mp3 (in older versions of FFmpeg).
    // We will just reopen the codec.
    FFmpegCore.LockAVCodec();
    try
      {$IF LIBAVFORMAT_VERSION < 59000000}
      avcodec_close(fCodecCtx);
      avcodec_open2(fCodecCtx, fCodec, nil);
      {$ELSE}
      NewCtx := FFmpegCore.GetCodecContext(fAudioStream, fCodec);
      if NewCtx <> nil then
      begin
        avcodec_free_context(@fCodecCtx);
        fCodecCtx := NewCtx;
        avcodec_open2(fCodecCtx, fCodec, nil);
      end
      else
        avcodec_flush_buffers(fCodecCtx);
      {$ENDIF}
    finally
      FFmpegCore.UnlockAVCodec();
    end;
  end;
end;

function TFFmpegDecodeStream.DecodeFrame(): integer;
var
  PaketDecodedSize: integer; // size of packet data used for decoding
  DataSize: integer;         // size of output data decoded by FFmpeg
  BlockQueue: boolean;
  SilenceDuration: double;
  got_frame_ptr: integer;
  {$IFDEF DebugFFmpegDecode}
  TmpPos: double;
  {$ENDIF}
begin
  Result := -1;

  if (EOF) then
    Exit;

  while(true) do
  begin

    // for titles with start_time > 0 we have to generate silence
    // until we reach the pts of the first data packet.
    if (fAudioPaketSilence > 0) then
    begin
      DataSize := Min(fAudioPaketSilence, AUDIO_BUFFER_SIZE);
      FillChar(fAudioBuffer[0], DataSize, 0);
      Dec(fAudioPaketSilence, DataSize);
      fAudioStreamPos := fAudioStreamPos + DataSize / fFormatInfo.BytesPerSec;
      Result := DataSize;
      Exit;
    end;

    // read packet data
    while (fAudioPaketSize > 0) do
    begin
      DataSize := AUDIO_BUFFER_SIZE;

      got_frame_ptr := avcodec_receive_frame(fCodecCtx, fAudioBufferFrame);
      if (got_frame_ptr = AVERROR(EAGAIN)) then
        PaketDecodedSize := fAudioPaketSize
      else
        PaketDecodedSize := 0;
      got_frame_ptr := ord(got_frame_ptr = 0);
      if(got_frame_ptr <> 0) then
      begin
        DataSize := fAudioBufferFrame.nb_samples * fBytesPerSample;
        fAudioBuffer := PByteArray(fAudioBufferFrame.data[0]);
      end
      else
        DataSize := 0;

      if(PaketDecodedSize < 0) then
      begin
        // if error, skip frame
        {$IFDEF DebugFFmpegDecode}
        DebugWriteln('Skip audio frame');
        {$ENDIF}
        fAudioPaketSize := 0;
        Break;
      end;

      Dec(fAudioPaketSize, PaketDecodedSize);

      // check if avcodec_decode_audio returned data, otherwise fetch more frames
      if (DataSize <= 0) then
        Continue;

      // update stream position by the amount of fetched data
      fAudioStreamPos := fAudioStreamPos + DataSize / fFormatInfo.BytesPerSec;
      
      // we have data, return it and come back for more later
      Result := DataSize;
      Exit;
    end;

    // do not block queue on seeking (to avoid deadlocks on the DecoderLock)
    if (IsSeeking()) then
      BlockQueue := false
    else
      BlockQueue := true;

    // request a new packet and block if none available.
    // If this fails, the queue was aborted.
    if (fPacketQueue.Get(fAudioPaket, BlockQueue) <= 0) then
      Exit;

    // handle Status-packet
    if (PAnsiChar(fAudioPaket^.data) = STATUS_PACKET) then
    begin
      fAudioPaketSize := 0;

      case (fAudioPaket^.flags) of
        PKT_STATUS_FLAG_FLUSH:
        begin
          // just used if SetPositionIntern was called without the flush flag.
          FlushCodecBuffers;
        end;
        PKT_STATUS_FLAG_EOF: // end-of-file
        begin
          // ignore EOF while seeking
          if (not IsSeeking()) then
            SetEOF(true);
          // buffer contains no data -> result = -1
          av_packet_free(@fAudioPaket);
          Exit;
        end;
        PKT_STATUS_FLAG_ERROR:
        begin
          SetError(true);
          Log.LogStatus('I/O Error', 'TFFmpegDecodeStream.DecodeFrame');
          av_packet_free(@fAudioPaket);
          Exit;
        end;
        PKT_STATUS_FLAG_EMPTY:
        begin
          SilenceDuration := PDouble(fPacketQueue.GetStatusInfo(fAudioPaket))^;
          fAudioPaketSilence := Round(SilenceDuration * fFormatInfo.SampleRate) * fFormatInfo.FrameSize;
          fPacketQueue.FreeStatusInfo(fAudioPaket);
        end
        else
        begin
          Log.LogStatus('Unknown status', 'TFFmpegDecodeStream.DecodeFrame');
        end;
      end;

      av_packet_free(@fAudioPaket);
      Continue;
    end;

    fAudioPaketSize := fAudioPaket^.size;

    avcodec_send_packet(fCodecCtx, fAudioPaket);

    // if available, update the stream position to the presentation time of this package
    if(fAudioPaket^.pts <> AV_NOPTS_VALUE) then
    begin
      {$IFDEF DebugFFmpegDecode}
      TmpPos := fAudioStreamPos;
      {$ENDIF}
      fAudioStreamPos := av_q2d(fAudioStream^.time_base) * fAudioPaket^.pts;
      {$IFDEF DebugFFmpegDecode}
      DebugWriteln('Timestamp: ' + floattostrf(fAudioStreamPos, ffFixed, 15, 3) + ' ' +
                   '(Calc: ' + floattostrf(TmpPos, ffFixed, 15, 3) + '), ' +
                   'Diff: ' + floattostrf(fAudioStreamPos-TmpPos, ffFixed, 15, 3));
      {$ENDIF}
    end;

    av_packet_free(@fAudioPaket);
  end;
end;

function TFFmpegDecodeStream.ReadData(Buffer: PByte; BufferSize: integer): integer;
var
  CopyByteCount:   integer; // number of bytes to copy
  RemainByteCount: integer; // number of bytes left (remain) to read
  BufferPos: integer;
  BufferPtr: PByte;

  // prioritize pause requests
  procedure LockDecoder();
  begin
    SDL_LockMutex(fStateLock);
    while (fDecoderPauseRequestCount > 0) do
      SDL_CondWait(fDecoderResumeCond, fStateLock);
    fDecoderLocked := true;
    SDL_UnlockMutex(fStateLock);
  end;

  procedure UnlockDecoder();
  begin
    SDL_LockMutex(fStateLock);
    fDecoderLocked := false;
    SDL_CondBroadcast(fDecoderUnlockedCond);
    SDL_UnlockMutex(fStateLock);
  end;

begin
  Result := -1;

  // set number of bytes to copy to the output buffer
  BufferPos := 0;

  if (fSwrContext <> nil) then
  begin
    RemainByteCount := BufferSize mod fBytesPerSample;
    BufferSize := BufferSize - RemainByteCount;
  end;

  LockDecoder();
  try
    // leave if end-of-file is reached
    if (EOF) then
      Exit;

    BufferPtr := nil;
    if ((fSwrContext <> nil) and (fAudioBufferSize > 0)) then
    begin
      BufferPtr := @Buffer[0];
      BufferPos := swr_convert(fSwrContext, BufferPtr, BufferSize div fBytesPerSample,
                               fAudioBufferFrame.extended_data^, 0);
      if (BufferPos < 0) then // might happen if out of memory
        Exit;
      BufferPos := BufferPos * fBytesPerSample;
      Inc(fAudioBufferPos, BufferPos);
    end;

    // copy data to output buffer
    while (BufferPos < BufferSize) do
    begin
      // check if we need more data
      if ((fAudioBufferPos >= fAudioBufferSize) or (BufferPtr <> nil)) then
      begin
        fAudioBufferPos := 0;

        // we have already sent all our data; get more
        fAudioBufferSize := DecodeFrame();

        // check for errors or EOF
        if(fAudioBufferSize < 0) then
        begin
          Result := BufferPos;
          Exit;
        end;
      end;

      RemainByteCount := BufferSize - BufferPos;

      if (fSwrContext <> nil) then
      begin
        BufferPtr := @Buffer[BufferPos];
        CopyByteCount := swr_convert(fSwrContext, BufferPtr, RemainByteCount div fBytesPerSample,
                                     fAudioBufferFrame.extended_data^, fAudioBufferFrame.nb_samples);
        if (CopyByteCount < 0) then
          Exit;
        CopyByteCount := CopyByteCount * fBytesPerSample;

        Inc(BufferPos,       CopyByteCount);
        Inc(fAudioBufferPos, CopyByteCount);
        continue;
      end;

      // calc number of new bytes in the decode-buffer
      CopyByteCount := fAudioBufferSize - fAudioBufferPos;
      // resize copy-count if more bytes available than needed (remaining bytes are used the next time)
      if (CopyByteCount > RemainByteCount) then
        CopyByteCount := RemainByteCount;

      Move(fAudioBuffer[fAudioBufferPos], Buffer[BufferPos], CopyByteCount);

      Inc(BufferPos,      CopyByteCount);
      Inc(fAudioBufferPos, CopyByteCount);
    end;
  finally
    UnlockDecoder();
  end;

  Result := BufferSize;
end;


{ TAudioDecoder_FFmpeg }

function TAudioDecoder_FFmpeg.GetName: String;
begin
  Result := 'FFmpeg_Decoder';
end;

function TAudioDecoder_FFmpeg.InitializeDecoder: boolean;
begin
  //Log.LogStatus('InitializeDecoder', 'UAudioDecoder_FFmpeg');
  FFmpegCore := TMediaCore_FFmpeg.GetInstance();

  // Do not show uninformative error messages by default.
  // FFmpeg prints all error-infos on the console by default what
  // is very confusing as the playback of the files is correct.
  // We consider these errors to be internal to FFMpeg. They can be fixed
  // by the FFmpeg guys only and do not provide any useful information in
  // respect to USDX.
  {$IFNDEF EnableFFmpegErrorOutput}
    av_log_set_level(AV_LOG_FATAL);
  {$ENDIF}

  Result := true;
end;

function TAudioDecoder_FFmpeg.FinalizeDecoder(): boolean;
begin
  Result := true;
end;

function TAudioDecoder_FFmpeg.Open(const Filename: IPath): TAudioDecodeStream;
var
  Stream: TFFmpegDecodeStream;
begin
  Result := nil;

  Stream := TFFmpegDecodeStream.Create();
  if (not Stream.Open(Filename)) then
  begin
    Stream.Free;
    Exit;
  end;

  Result := Stream;
end;

initialization
  MediaManager.Add(TAudioDecoder_FFmpeg.Create);

end.
