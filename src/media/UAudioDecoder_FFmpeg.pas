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
{$IFDEF UseSWResample}
  swresample,
{$ENDIF}
  UMusic,
  UIni,
  UMain,
  UMediaCore_FFmpeg,
  ULog,
  UCommon,
  UConfig,
  UPath;

{$IFDEF UseSWResample}
  {$IF LIBAVCODEC_VERSION >= 54041100}
    {$DEFINE UseFrameDecoderAPI}
    {$DEFINE ConvertPlanar}
  {$ENDIF}
{$ELSE}
  {$IF LIBAVCODEC_VERSION >= 57000000}
    {$DEFINE UseFrameDecoderAPI}
  {$ENDIF}
{$ENDIF}

const
  MAX_AUDIOQ_SIZE = (5 * 16 * 1024);

const
  // TODO: The factor 3/2 might not be necessary as we do not need extra
  // space for synchronizing as in the tutorial.
{$IF FFMPEG_VERSION_INT >= 2000000}
  AUDIO_BUFFER_SIZE = (192000 * 3) div 2;
{$ELSE}
  AUDIO_BUFFER_SIZE = (AVCODEC_MAX_AUDIO_FRAME_SIZE * 3) div 2;
{$ENDIF}

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
      {$IFDEF UseFrameDecoderAPI}
      fBytesPerSample: integer;
      {$IFEND}
      {$IFDEF ConvertPlanar}
      fSwrContext: PSwrContext;
      {$IFEND}

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
      fAudioPaket:        TAVPacket;
      fAudioPaketData:    PByteArray;
      fAudioPaketSize:    integer;
      fAudioPaketSilence: integer; // number of bytes of silence to return

      // state-vars for AudioCallback (locked by DecoderLock)
      fAudioBufferPos:  integer;
      fAudioBufferSize: integer;
      fAudioBuffer:     PByteArray;
      {$IFDEF UseFrameDecoderAPI}
      fAudioBufferFrame: PAVFrame;
      {$IFEND}

      fFilename: IPath;

      procedure SetPositionIntern(Time: real; Flush: boolean; Blocking: boolean);
      procedure SetEOF(State: boolean);   {$IFDEF HasInline}inline;{$ENDIF}
      procedure SetError(State: boolean); {$IFDEF HasInline}inline;{$ENDIF}
      function IsSeeking(): boolean;
      function IsQuit(): boolean;

      procedure Reset();

      procedure Parse();
      function ParseLoop(): boolean;
      procedure PauseParser();
      procedure ResumeParser();

      function DecodeFrame(): integer;
      procedure FlushCodecBuffers();
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

      function ReadData(Buffer: PByteArray; BufferSize: integer): integer; override;
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

  {$IFDEF UseFrameDecoderAPI}
    {$IF LIBAVUTIL_VERSION >= 52019101}
  fAudioBufferFrame := av_frame_alloc();
    {$ELSE}
  fAudioBufferFrame := avcodec_alloc_frame();
    {$IFEND}
  {$ELSE}
  // according to the documentation of avcodec_decode_audio(2), sample-data
  // should be aligned on a 16 byte boundary. Otherwise internal calls
  // (e.g. to SSE or Altivec operations) might fail or lack performance on some
  // CPUs. Although GetMem() in Delphi and FPC seems to use a 16 byte or higher
  // alignment for buffers of this size (alignment depends on the size of the
  // requested buffer), we will set the alignment explicitly as the minimum
  // alignment used by Delphi and FPC is on an 8 byte boundary.
  // 
  // Note: AudioBuffer was previously defined as a field of type TAudioBuffer
  // (array[0..AUDIO_BUFFER_SIZE-1] of byte) and hence statically allocated.
  // Fields of records are aligned different to memory allocated with GetMem(),
  // aligning depending on the type but will be at least 2 bytes.
  // AudioBuffer was not aligned to a 16 byte boundary. The {$ALIGN x} directive
  // was not applicable as Delphi in contrast to FPC provides at most 8 byte
  // alignment ({$ALIGN 16} is not supported) by this directive.
  fAudioBuffer := GetAlignedMem(AUDIO_BUFFER_SIZE, 16);
  {$IFEND}

  Reset();
end;

procedure TFFmpegDecodeStream.Reset();
begin
  fParseThread := nil;

  fEOFState := false;
  fErrorState := false;
  fLoop := false;
  fQuitRequest := false;

  fAudioPaketData := nil;
  fAudioPaketSize := 0;
  fAudioPaketSilence := 0;

  fAudioBufferPos := 0;
  fAudioBufferSize := 0;

  fParserLocked := false;
  fParserPauseRequestCount := 0;
  fDecoderLocked := false;
  fDecoderPauseRequestCount := 0;

  FillChar(fAudioPaket, SizeOf(TAVPacket), 0);
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

  {$IFDEF UseFrameDecoderAPI}
    {$IF LIBAVUTIL_VERSION >= 52019101}
  av_frame_free(@fAudioBufferFrame);
    {$ELSE}
  avcodec_free_frame(@fAudioBufferFrame);
    {$IFEND}
  {$ELSE}
  FreeAlignedMem(fAudioBuffer);
  {$IFEND}

  inherited;
end;

function TFFmpegDecodeStream.Open(const Filename: IPath): boolean;
var
  SampleFormat: TAudioSampleFormat;
  PackedSampleFormat: TAVSampleFormat;
  TestFrame: TAVPacket;
  AVResult: integer;
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
  {$IF LIBAVFORMAT_VERSION < 53001003}
  AVResult := av_open_input_file(fFormatCtx, PAnsiChar('ufile:'+FileName.ToUTF8), nil, 0, nil);
  {$ELSEIF LIBAVFORMAT_VERSION < 54029104}
  AVResult := avformat_open_input(@fFormatCtx, PAnsiChar('ufile:'+FileName.ToUTF8), nil, nil);
  {$ELSE}
  AVResult := FFmpegCore.AVFormatOpenInput(@fFormatCtx, PAnsiChar('ufile:'+FileName.ToUTF8));
  {$IFEND}
  if (AVResult <> 0) then
  begin
    Log.LogError('Failed to open file "' + Filename.ToNative + '" ('+FFmpegCore.GetErrorString(AVResult)+')', 'UAudio_FFmpeg');
    Exit;
  end;

  // generate PTS values if they do not exist
  fFormatCtx^.flags := fFormatCtx^.flags or AVFMT_FLAG_GENPTS;

  // retrieve stream information
  //{$IF LIBAVFORMAT_VERSION >= 54006000)}
  // av_find_stream_info is deprecated and should be replaced by av_read_frame. Untested.
  //AVResult := av_read_frame(fFormatCtx, TestFrame);

  {$IF LIBAVFORMAT_VERSION >= 53002000)}
  AVResult := avformat_find_stream_info(fFormatCtx, nil);
  {$ELSE}
  AVResult := av_find_stream_info(fFormatCtx);
  {$IFEND}
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

{$IF LIBAVFORMAT_VERSION <= 52111000} // <= 52.111.0
  fAudioStream := fFormatCtx.streams[fAudioStreamIndex];
{$ELSE}
  fAudioStream := PPAVStream(PtrUInt(fFormatCtx.streams) + fAudioStreamIndex * Sizeof(pointer))^;
{$IFEND}
  fAudioStreamPos := 0;
  fCodecCtx := fAudioStream^.codec;

  // TODO: should we use this or not? Should we allow 5.1 channel audio?

  {$IF LIBAVCODEC_VERSION >= 56042000}
  fCodecCtx^.request_channel_layout := ($20000000 or $40000000); //avcodec.c AV_CH_LAYOUT_STEREO_DOWNMIX;
  {$ELSEIF LIBAVCODEC_VERSION >= 51042000}
  if (fCodecCtx^.channels > 0) then
    fCodecCtx^.request_channels := Min(2, fCodecCtx^.channels)
  else
    fCodecCtx^.request_channels := 2;
  {$IFEND}

  fCodec := avcodec_find_decoder(fCodecCtx^.codec_id);
  if (fCodec = nil) then
  begin
    Log.LogError('Unsupported codec!', 'UAudio_FFmpeg');
    fCodecCtx := nil;
    Close();
    Exit;
  end;

  // set debug options
  fCodecCtx^.debug_mv := 0;
  fCodecCtx^.debug := 0;

  {$IF FFMPEG_VERSION_INT >= 1001000}
  // request required sample format
  // reference:
  // http://stackoverflow.com/questions/16479662/ffmpeg-1-0-causing-audio-playback-issues
  // without this avcodec_open2 returns AV_SAMPLE_FMT_S16P
  fCodecCtx^.request_sample_fmt := AV_SAMPLE_FMT_S16;
  {$IFEND}

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
    {$IF LIBAVCODEC_VERSION >= 53005000}
    AVResult := avcodec_open2(fCodecCtx, fCodec, nil);
    {$ELSE}
    AVResult := avcodec_open(fCodecCtx, fCodec);
    {$IFEND}
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
  {$IFDEF ConvertPlanar}
  PackedSampleFormat := av_get_packed_sample_fmt(fCodecCtx^.sample_fmt);
  if (PackedSampleFormat <> fCodecCtx^.sample_fmt) then
  begin
    // There is no point in leaving PackedSampleFormat as is.
    // av_audio_resample_init as used by TAudioConverter_FFmpeg will internally
    // convert to AV_SAMPLE_FMT_S16 anyway and most architectures have assembly
    // optimized conversion routines from AV_SAMPLE_FMT_FLTP to AV_SAMPLE_FMT_S16.
    PackedSampleFormat := AV_SAMPLE_FMT_S16;
    fSwrContext := swr_alloc_set_opts(nil, fCodecCtx^.channel_layout, PackedSampleFormat, fCodecCtx^.sample_rate,
                                      fCodecCtx^.channel_layout, fCodecCtx^.sample_fmt, fCodecCtx^.sample_rate, 0, nil);
    if (fSwrContext = nil) then
      Log.LogStatus('Error: Failed to create SwrContext', 'TFFmpegDecodeStream.Open')
    else
    begin
      av_opt_set_int(fSwrContext, 'ich', fCodecCtx^.channels, 0);
      av_opt_set_int(fSwrContext, 'och', fCodecCtx^.channels, 0);
      if (swr_init(fSwrContext) < 0) then
      begin
        swr_free(@fSwrContext);
        Log.LogStatus('Error: Failed to initialize SwrContext', 'TFFmpegDecodeStream.Open');
      end;
    end;
  end;
  {$ELSE}
  PackedSampleFormat := fCodecCtx^.sample_fmt;
  {$IFEND}

  if (not FFmpegCore.ConvertFFmpegToAudioFormat(PackedSampleFormat, SampleFormat)) then
  begin
    // try standard format
    SampleFormat := asfS16;
  end;
  if fCodecCtx^.channels > 255 then
    Log.LogStatus('Error: CodecCtx^.channels > 255', 'TFFmpegDecodeStream.Open');
  fFormatInfo := TAudioFormatInfo.Create(
    byte(fCodecCtx^.channels),
    fCodecCtx^.sample_rate,
    SampleFormat
  );
  {$IFDEF UseFrameDecoderAPI}
  fBytesPerSample := av_get_bytes_per_sample(PackedSampleFormat) * fCodecCtx^.channels;
  {$IFEND}

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
    //SDL_WaitThread(fParseThread, PInt(ThreadResult));
    fParseThread := nil;
  end;

  {$IFDEF ConvertPlanar}
  // Free the swresample context
  if (fSwrContext <> nil) then
    swr_free(@fSwrContext);
  {$IFEND}

  // Close the codec
  if (fCodecCtx <> nil) then
  begin
    // avcodec_close() is not thread-safe
    FFmpegCore.LockAVCodec();
    try
      avcodec_close(fCodecCtx);
    finally
      FFmpegCore.UnlockAVCodec();
    end;
    fCodecCtx := nil;
  end;

  // Close the video file
  if (fFormatCtx <> nil) then
  begin
    {$IF LIBAVFORMAT_VERSION < 53024002}
    av_close_input_file(fFormatCtx);
    {$ELSEIF LIBAVFORMAT_VERSION < 54029104}
    avformat_close_input(@fFormatCtx);
    {$ELSE}
    FFmpegCore.AVFormatCloseInput(@fFormatCtx);
    {$IFEND}
    fFormatCtx := nil;
  end;

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
  SetPositionIntern(Time, true, true);
end;

function TFFmpegDecodeStream.GetLoop(): boolean;
begin
  SDL_LockMutex(fStateLock);
  Result := fLoop;
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

procedure TFFmpegDecodeStream.PauseParser();
begin
  if (fParseThread = nil) or (SDL_ThreadID() = fParseThread.threadid) then
  begin
    SDL_UnlockMutex(fStateLock);
    Exit;
  end;

  SDL_LockMutex(fStateLock);
  Inc(fParserPauseRequestCount);
  while (fParserLocked) do
    SDL_CondWait(fParserUnlockedCond, fStateLock);
  SDL_UnlockMutex(fStateLock);
end;

procedure TFFmpegDecodeStream.ResumeParser();
begin
  if (fParseThread = nil) or (SDL_ThreadID() = fParseThread.threadid) then
    begin
      SDL_UnlockMutex(fStateLock);
      Exit;
    end;

  SDL_LockMutex(fStateLock);
  Dec(fParserPauseRequestCount);
  SDL_CondSignal(fParserResumeCond);
  SDL_UnlockMutex(fStateLock);
end;

procedure TFFmpegDecodeStream.SetPositionIntern(Time: real; Flush: boolean; Blocking: boolean);
begin
  if (fParseThread = nil) then //if thread is killed but doesn't know of it yet, leave the sinking ship.
    begin
      SDL_UnlockMutex(fStateLock);
      ResumeDecoder();
      ResumeParser();
      Exit;
    end;
  // - Pause the parser first to prevent it from putting obsolete packages
  //   into the queue after the queue was flushed and before seeking is done.
  //   Otherwise we will hear fragments of old data, if the stream was seeked
  //   in stopped mode and resumed afterwards (applies to non-blocking mode only).
  // - Pause the decoder to avoid race-condition that might occur otherwise.
  // - Last lock the state lock because we are manipulating some shared state-vars.
  PauseParser();
  PauseDecoder();
  SDL_LockMutex(fStateLock);
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
    SDL_UnlockMutex(fStateLock);
    ResumeDecoder();
    ResumeParser();
  end;

  // in blocking mode, wait until seeking is done
  if (Blocking) then
  begin
    SDL_LockMutex(fStateLock);
    while (fSeekRequest) do
      SDL_CondWait(SeekFinishedCond, fStateLock);
    SDL_UnlockMutex(fStateLock);
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
  while (ParseLoop()) do
  begin
    if(fQuitRequest = false) then
    begin
    // wait for reuse or destruction of stream
    SDL_LockMutex(fStateLock);
    while (not (fSeekRequest or fQuitRequest)) do
      SDL_CondWait(fParserIdleCond, fStateLock);
    SDL_UnlockMutex(fStateLock);
    end
    else
    begin
      Break;
    end;
  end;
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
 *)
function TFFmpegDecodeStream.ParseLoop(): boolean;
var
  Packet: TAVPacket;
  SeekTarget: int64;
  {$IF FFMPEG_VERSION_INT < 1001000}
  ByteIOCtx: PByteIOContext;
  {$ELSE}
  ByteIOCtx: PAVIOContext;
  {$ENDIF}
  ErrorCode: integer;
  StartSilence: double;       // duration of silence at start of stream
  StartSilencePtr: PDouble;  // pointer for the EMPTY status packet 
  fileSize: integer;
  urlError: integer;

  // Note: pthreads wakes threads waiting on a mutex in the order of their
  // priority and not in FIFO order. SDL does not provide any option to
  // control priorities. This might (and already did) starve threads waiting
  // on the mutex (e.g. SetPosition) making usdx look like it was froozen.
  // Instead of simply locking the critical section we set a ParserLocked flag
  // instead and give priority to the threads requesting the parser to pause.
  procedure LockParser();
  begin
    if fQuitRequest then Exit;
    SDL_LockMutex(fStateLock);
    while (fParserPauseRequestCount > 0) do
      SDL_CondWait(fParserResumeCond, fStateLock);
    fParserLocked := true;
    SDL_UnlockMutex(fStateLock);
  end;

  procedure UnlockParser();
  begin
    if fQuitRequest then Exit;
    SDL_LockMutex(fStateLock);
    fParserLocked := false;
    SDL_CondBroadcast(fParserUnlockedCond);
    SDL_UnlockMutex(fStateLock);
  end;

begin
  Result := true;

  while (true) do
  begin
    LockParser();
    try

      if (IsQuit()) then
      begin
        Result := false;
        Exit;
      end;

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
        PauseDecoder();
        SDL_LockMutex(fStateLock);
        try
          if (ErrorCode < 0) then
          begin
            // seeking failed
            fErrorState := true;
            Log.LogError('Seek Error in "'+fFormatCtx^.filename+'"', 'UAudioDecoder_FFmpeg');
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
          SDL_UnlockMutex(fStateLock);
          ResumeDecoder();
        end;
      end;

      if (fPacketQueue.GetSize() > MAX_AUDIOQ_SIZE) then
      begin
        SDL_Delay(10);
        Continue;
      end;

      if (av_read_frame(fFormatCtx, Packet) < 0) then
      begin
        // failed to read a frame, check reason
        {$IF (LIBAVFORMAT_VERSION_MAJOR >= 52)}
        ByteIOCtx := fFormatCtx^.pb;
        {$ELSE}
        ByteIOCtx := @fFormatCtx^.pb;
        {$IFEND}

        // check for end-of-file (eof is not an error)
        {$IF (LIBAVFORMAT_VERSION_MAJOR < 56)}
        if (url_feof(ByteIOCtx) <> 0) then
        {$ELSE}
        if (avio_feof(ByteIOCtx) <> 0) then
        {$IFEND}
        begin
          if (GetLoop()) then
          begin
            // rewind stream (but do not flush)
            SetPositionIntern(0, false, false);
            Continue;
          end
          else
          begin
            // signal end-of-file
            fPacketQueue.PutStatus(PKT_STATUS_FLAG_EOF, nil);
            Exit;
          end;
        end;

        // check for errors
        {$IF (LIBAVFORMAT_VERSION >= 52103000)}
        urlError := ByteIOCtx^.error;
        {$ELSE}
        urlError := url_ferror(ByteIOCtx);
        {$IFEND}
        if (urlError <> 0) then
        begin
          // an error occured -> abort and wait for repositioning or termination
          fPacketQueue.PutStatus(PKT_STATUS_FLAG_ERROR, nil);
          Exit;
        end;

        // url_feof() does not detect an EOF for some files
        // so we have to do it this way.
        {$IF (LIBAVFORMAT_VERSION >= 53009000)}
        fileSize := avio_size(fFormatCtx^.pb);
        {$ELSE}
        fileSize := fFormatCtx^.file_size;
        {$IFEND}
        if ((fileSize <> 0) and (ByteIOCtx^.pos >= fileSize)) then
        begin
          fPacketQueue.PutStatus(PKT_STATUS_FLAG_EOF, nil);
          Exit;
        end;

        // unknown error occured, exit
        fPacketQueue.PutStatus(PKT_STATUS_FLAG_ERROR, nil);
        Exit;
      end;

      if (Packet.stream_index = fAudioStreamIndex) then
        fPacketQueue.Put(@Packet)
      else
        av_free_packet(@Packet);

    finally
      UnlockParser();
    end;
  end;
end;


(********************************************
 * Decoder section
 ********************************************)

procedure TFFmpegDecodeStream.PauseDecoder();
begin
  SDL_LockMutex(fStateLock);
  Inc(fDecoderPauseRequestCount);
  while (fDecoderLocked) do
    SDL_CondWait(fDecoderUnlockedCond, fStateLock);
  SDL_UnlockMutex(fStateLock);
end;

procedure TFFmpegDecodeStream.ResumeDecoder();
begin
  SDL_LockMutex(fStateLock);
  Dec(fDecoderPauseRequestCount);
  SDL_CondSignal(fDecoderResumeCond);
  SDL_UnlockMutex(fStateLock);
end;

procedure TFFmpegDecodeStream.FlushCodecBuffers();
begin
  // if no flush operation is specified, avcodec_flush_buffers will not do anything.
  if (@fCodecCtx.codec.flush <> nil) then
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
      avcodec_close(fCodecCtx);
      {$IF LIBAVCODEC_VERSION >= 53005000}
      avcodec_open2(fCodecCtx, fCodec, nil);
      {$ELSE}
      avcodec_open(fCodecCtx, fCodec);
      {$IFEND}
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
  {$IF (LIBAVCODEC_VERSION >= 52122000) and (LIBAVCODEC_VERSION < 57037100)}
  AVPacket: TAVPacket;
  {$IFEND}
  {$IFDEF UseFrameDecoderAPI}
  got_frame_ptr: integer;
  {$IFEND}
  {$IFDEF DebugFFmpegDecode}
  TmpPos: double;
  {$ENDIF}
begin
  Result := -1;

  if (EOF) then
    Exit;

  while(true) do
  begin
    {$IF (LIBAVCODEC_VERSION >= 52122000) and (LIBAVCODEC_VERSION < 57037100)}
    AVPacket := fAudioPaket;
    {$IFEND}

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
      {$IF (LIBAVCODEC_VERSION >= 52122000) and (LIBAVCODEC_VERSION < 57037100)}
      AVPacket.data := fAudioPaketData;
      AVPacket.size := fAudioPaketSize;
      {$IFEND}

      {$IFDEF UseFrameDecoderAPI}
        {$IF LIBAVCODEC_VERSION >= 57037100}
      got_frame_ptr := avcodec_receive_frame(fCodecCtx, fAudioBufferFrame);
      if (got_frame_ptr = AVERROR(EAGAIN)) then
        PaketDecodedSize := fAudioPaketSize
      else
        PaketDecodedSize := 0;
      got_frame_ptr := ord(got_frame_ptr = 0);
        {$ELSE}
      PaketDecodedSize := avcodec_decode_audio4(fCodecCtx, fAudioBufferFrame,
            @got_frame_ptr, @AVPacket);
        {$IFEND}
      if(got_frame_ptr <> 0) then
      begin
        DataSize := fAudioBufferFrame.nb_samples * fBytesPerSample;
        fAudioBuffer := PByteArray(fAudioBufferFrame.data[0]);
      end
      else
        DataSize := 0;
      {$ELSE}
        {$IF LIBAVCODEC_VERSION >= 52122000} // 52.122.0
      PaketDecodedSize := avcodec_decode_audio3(fCodecCtx, PSmallint(fAudioBuffer),
                  DataSize, @AVPacket);
        {$ELSEIF LIBAVCODEC_VERSION >= 51030000} // 51.30.0
      PaketDecodedSize := avcodec_decode_audio2(fCodecCtx, PSmallint(fAudioBuffer),
                  DataSize, fAudioPaketData, fAudioPaketSize);
        {$ELSE}
      PaketDecodedSize := avcodec_decode_audio(fCodecCtx, PSmallint(fAudioBuffer),
                  DataSize, fAudioPaketData, fAudioPaketSize);
        {$IFEND}
      {$IFEND}

      if(PaketDecodedSize < 0) then
      begin
        // if error, skip frame
        {$IFDEF DebugFFmpegDecode}
        DebugWriteln('Skip audio frame');
        {$ENDIF}
        fAudioPaketSize := 0;
        Break;
      end;

      Inc(PByte(fAudioPaketData), PaketDecodedSize);
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

    // free old packet data
    if (fAudioPaket.data <> nil) then
      av_free_packet(@fAudioPaket);

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
    if (PAnsiChar(fAudioPaket.data) = STATUS_PACKET) then
    begin
      fAudioPaket.data := nil;
      fAudioPaketData := nil;
      fAudioPaketSize := 0;

      case (fAudioPaket.flags) of
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
          Exit;
        end;
        PKT_STATUS_FLAG_ERROR:
        begin
          SetError(true);
          Log.LogStatus('I/O Error', 'TFFmpegDecodeStream.DecodeFrame');
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

      Continue;
    end;

    fAudioPaketData := fAudioPaket.data;
    fAudioPaketSize := fAudioPaket.size;

    {$IF LIBAVCODEC_VERSION >= 57037100}
    avcodec_send_packet(fCodecCtx, @fAudioPaket);
    {$IFEND}

    // if available, update the stream position to the presentation time of this package
    if(fAudioPaket.pts <> AV_NOPTS_VALUE) then
    begin
      {$IFDEF DebugFFmpegDecode}
      TmpPos := fAudioStreamPos;
      {$ENDIF}
      fAudioStreamPos := av_q2d(fAudioStream^.time_base) * fAudioPaket.pts;
      {$IFDEF DebugFFmpegDecode}
      DebugWriteln('Timestamp: ' + floattostrf(fAudioStreamPos, ffFixed, 15, 3) + ' ' +
                   '(Calc: ' + floattostrf(TmpPos, ffFixed, 15, 3) + '), ' +
                   'Diff: ' + floattostrf(fAudioStreamPos-TmpPos, ffFixed, 15, 3));
      {$ENDIF}
    end;
  end;
end;

function TFFmpegDecodeStream.ReadData(Buffer: PByteArray; BufferSize: integer): integer;
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

  {$IFDEF ConvertPlanar}
  if (fSwrContext <> nil) then
  begin
    RemainByteCount := BufferSize mod fBytesPerSample;
    BufferSize := BufferSize - RemainByteCount;
  end;
  {$IFEND}

  LockDecoder();
  try
    // leave if end-of-file is reached
    if (EOF) then
      Exit;

    BufferPtr := nil;
    {$IFDEF ConvertPlanar}
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
    {$IFEND}

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

      {$IFDEF ConvertPlanar}
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
      {$IFEND}

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
  av_register_all();

  // Do not show uninformative error messages by default.
  // FFmpeg prints all error-infos on the console by default what
  // is very confusing as the playback of the files is correct.
  // We consider these errors to be internal to FFMpeg. They can be fixed
  // by the FFmpeg guys only and do not provide any useful information in
  // respect to USDX.
  {$IFNDEF EnableFFmpegErrorOutput}
    {$IF LIBAVUTIL_VERSION_MAJOR >= 50}
    av_log_set_level(AV_LOG_FATAL);
    {$ELSE}
    // FATAL and ERROR share one log-level, so we have to use QUIET
    av_log_set_level(AV_LOG_QUIET);
    {$IFEND}
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
