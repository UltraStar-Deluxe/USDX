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
  Classes,
  Math,
  UMusic,
  UIni,
  UMain,
  avcodec,
  avformat,
  avutil,
  avio,
  mathematics, // used for av_rescale_q
  rational,
  SDL,
  SysUtils,
  UMediaCore_FFmpeg,
  ULog,
  UCommon,
  UConfig;

const
  MAX_AUDIOQ_SIZE = (5 * 16 * 1024);

const
  // TODO: The factor 3/2 might not be necessary as we do not need extra
  // space for synchronizing as in the tutorial.
  AUDIO_BUFFER_SIZE = (AVCODEC_MAX_AUDIO_FRAME_SIZE * 3) div 2;

type
  TFFmpegDecodeStream = class(TAudioDecodeStream)
    private
      StateLock:   PSDL_Mutex;

      EOFState:   boolean; // end-of-stream flag (locked by StateLock)
      ErrorState: boolean; // error flag (locked by StateLock)

      QuitRequest: boolean; // (locked by StateLock)
      ParserIdleCond: PSDL_Cond;

      // parser pause/resume data
      ParserLocked:            boolean;
      ParserPauseRequestCount: integer;
      ParserUnlockedCond:      PSDL_Cond;
      ParserResumeCond:        PSDL_Cond;

      SeekRequest: boolean; // (locked by StateLock)
      SeekFlags:   integer; // (locked by StateLock)
      SeekPos:     double;    // stream position to seek for (in secs) (locked by StateLock)
      SeekFlush:   boolean;   // true if the buffers should be flushed after seeking (locked by StateLock)
      SeekFinishedCond: PSDL_Cond;

      Loop: boolean; // (locked by StateLock)

      ParseThread: PSDL_Thread;
      PacketQueue: TPacketQueue;

      FormatInfo: TAudioFormatInfo;

      // FFmpeg specific data
      FormatCtx: PAVFormatContext;
      CodecCtx:  PAVCodecContext;
      Codec:     PAVCodec;

      AudioStreamIndex: integer;
      AudioStream: PAVStream;
      AudioStreamPos: double; // stream position in seconds (locked by DecoderLock)

      // decoder pause/resume data
      DecoderLocked:            boolean;
      DecoderPauseRequestCount: integer;
      DecoderUnlockedCond:      PSDL_Cond;
      DecoderResumeCond:        PSDL_Cond;

      // state-vars for DecodeFrame (locked by DecoderLock)
      AudioPaket:        TAVPacket;
      AudioPaketData:    PByteArray;
      AudioPaketSize:    integer;
      AudioPaketSilence: integer; // number of bytes of silence to return

      // state-vars for AudioCallback (locked by DecoderLock)
      AudioBufferPos:  integer;
      AudioBufferSize: integer;
      AudioBuffer:     PByteArray;

      Filename: string;

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

      function DecodeFrame(Buffer: PByteArray; BufferSize: integer): integer;
      procedure FlushCodecBuffers();
      procedure PauseDecoder();
      procedure ResumeDecoder();
    public
      constructor Create();
      destructor Destroy(); override;

      function Open(const Filename: string): boolean;
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
      function Open(const Filename: string): TAudioDecodeStream;
  end;

var
  FFmpegCore: TMediaCore_FFmpeg;

function ParseThreadMain(Data: Pointer): integer; cdecl; forward;


{ TFFmpegDecodeStream }

constructor TFFmpegDecodeStream.Create();
begin
  inherited Create();

  StateLock := SDL_CreateMutex();
  ParserUnlockedCond := SDL_CreateCond();
  ParserResumeCond := SDL_CreateCond();
  ParserIdleCond := SDL_CreateCond();
  SeekFinishedCond := SDL_CreateCond();
  DecoderUnlockedCond := SDL_CreateCond();
  DecoderResumeCond := SDL_CreateCond();

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
  AudioBuffer := GetAlignedMem(AUDIO_BUFFER_SIZE, 16);

  Reset();
end;

procedure TFFmpegDecodeStream.Reset();
begin
  ParseThread := nil;

  EOFState := false;
  ErrorState := false;
  Loop := false;
  QuitRequest := false;

  AudioPaketData := nil;
  AudioPaketSize := 0;
  AudioPaketSilence := 0;

  AudioBufferPos := 0;
  AudioBufferSize := 0;

  ParserLocked := false;
  ParserPauseRequestCount := 0;
  DecoderLocked := false;
  DecoderPauseRequestCount := 0;

  FillChar(AudioPaket, SizeOf(TAVPacket), 0);
end;

{*
 * Frees the decode-stream data.
 *}
destructor TFFmpegDecodeStream.Destroy();
begin
  Close();

  SDL_DestroyMutex(StateLock);
  SDL_DestroyCond(ParserUnlockedCond);
  SDL_DestroyCond(ParserResumeCond);
  SDL_DestroyCond(ParserIdleCond);
  SDL_DestroyCond(SeekFinishedCond);
  SDL_DestroyCond(DecoderUnlockedCond);
  SDL_DestroyCond(DecoderResumeCond);

  FreeAlignedMem(AudioBuffer);

  inherited;
end;

function TFFmpegDecodeStream.Open(const Filename: string): boolean;
var
  SampleFormat: TAudioSampleFormat;
  AVResult: integer;
begin
  Result := false;

  Close();
  Reset();

  if (not FileExists(Filename)) then
  begin
    Log.LogError('Audio-file does not exist: "' + Filename + '"', 'UAudio_FFmpeg');
    Exit;
  end;

  Self.Filename := Filename;

  // open audio file
  if (av_open_input_file(FormatCtx, PAnsiChar(Filename), nil, 0, nil) <> 0) then
  begin
    Log.LogError('av_open_input_file failed: "' + Filename + '"', 'UAudio_FFmpeg');
    Exit;
  end;

  // generate PTS values if they do not exist
  FormatCtx^.flags := FormatCtx^.flags or AVFMT_FLAG_GENPTS;

  // retrieve stream information
  if (av_find_stream_info(FormatCtx) < 0) then
  begin
    Log.LogError('av_find_stream_info failed: "' + Filename + '"', 'UAudio_FFmpeg');
    Close();
    Exit;
  end;

  // FIXME: hack used by ffplay. Maybe should not use url_feof() to test for the end
  FormatCtx^.pb.eof_reached := 0;

  {$IFDEF DebugFFmpegDecode}
  dump_format(FormatCtx, 0, PAnsiChar(Filename), 0);
  {$ENDIF}

  AudioStreamIndex := FFmpegCore.FindAudioStreamIndex(FormatCtx);
  if (AudioStreamIndex < 0) then
  begin
    Log.LogError('FindAudioStreamIndex: No Audio-stream found "' + Filename + '"', 'UAudio_FFmpeg');
    Close();
    Exit;
  end;

  //Log.LogStatus('AudioStreamIndex is: '+ inttostr(ffmpegStreamID), 'UAudio_FFmpeg');

  AudioStream := FormatCtx.streams[AudioStreamIndex];
  CodecCtx := AudioStream^.codec;

  // TODO: should we use this or not? Should we allow 5.1 channel audio?
  (*
  {$IF LIBAVCODEC_VERSION >= 51042000}
  if (CodecCtx^.channels > 0) then
    CodecCtx^.request_channels := Min(2, CodecCtx^.channels)
  else
    CodecCtx^.request_channels := 2;
  {$IFEND}
  *)

  Codec := avcodec_find_decoder(CodecCtx^.codec_id);
  if (Codec = nil) then
  begin
    Log.LogError('Unsupported codec!', 'UAudio_FFmpeg');
    CodecCtx := nil;
    Close();
    Exit;
  end;

  // set debug options
  CodecCtx^.debug_mv := 0;
  CodecCtx^.debug := 0;

  // detect bug-workarounds automatically
  CodecCtx^.workaround_bugs := FF_BUG_AUTODETECT;
  // error resilience strategy (careful/compliant/agressive/very_aggressive)
  //CodecCtx^.error_resilience := FF_ER_CAREFUL; //FF_ER_COMPLIANT;
  // allow non spec compliant speedup tricks.
  //CodecCtx^.flags2 := CodecCtx^.flags2 or CODEC_FLAG2_FAST;

  // Note: avcodec_open() and avcodec_close() are not thread-safe and will
  // fail if called concurrently by different threads.
  FFmpegCore.LockAVCodec();
  try
    AVResult := avcodec_open(CodecCtx, Codec);
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

  if (not FFmpegCore.ConvertFFmpegToAudioFormat(CodecCtx^.sample_fmt, SampleFormat)) then
  begin
    // try standard format
    SampleFormat := asfS16;
  end;

  FormatInfo := TAudioFormatInfo.Create(
    CodecCtx^.channels,
    CodecCtx^.sample_rate,
    SampleFormat
  );


  PacketQueue := TPacketQueue.Create();

  // finally start the decode thread
  ParseThread := SDL_CreateThread(@ParseThreadMain, Self);

  Result := true;
end;

procedure TFFmpegDecodeStream.Close();
var
  ThreadResult: integer;
begin
  // wake threads waiting for packet-queue data
  // Note: normally, there are no waiting threads. If there were waiting
  // ones, they would block the audio-callback thread.
  if (assigned(PacketQueue)) then
    PacketQueue.Abort();

  // send quit request (to parse-thread etc)
  SDL_mutexP(StateLock);
  QuitRequest := true;
  SDL_CondBroadcast(ParserIdleCond);
  SDL_mutexV(StateLock);

  // abort parse-thread
  if (ParseThread <> nil) then
  begin
    // and wait until it terminates
    SDL_WaitThread(ParseThread, ThreadResult);
    ParseThread := nil;
  end;

  // Close the codec
  if (CodecCtx <> nil) then
  begin
    // avcodec_close() is not thread-safe
    FFmpegCore.LockAVCodec();
    try
      avcodec_close(CodecCtx);
    finally
      FFmpegCore.UnlockAVCodec();
    end;
    CodecCtx := nil;
  end;

  // Close the video file
  if (FormatCtx <> nil) then
  begin
    av_close_input_file(FormatCtx);
    FormatCtx := nil;
  end;

  PerformOnClose();
  
  FreeAndNil(PacketQueue);
  FreeAndNil(FormatInfo);
end;

function TFFmpegDecodeStream.GetLength(): real;
begin
  // do not forget to consider the start_time value here 
  Result := (FormatCtx^.start_time + FormatCtx^.duration) / AV_TIME_BASE;
end;

function TFFmpegDecodeStream.GetAudioFormatInfo(): TAudioFormatInfo;
begin
  Result := FormatInfo;
end;

function TFFmpegDecodeStream.IsEOF(): boolean;
begin
  SDL_mutexP(StateLock);
  Result := EOFState;
  SDL_mutexV(StateLock);
end;

procedure TFFmpegDecodeStream.SetEOF(State: boolean);
begin
  SDL_mutexP(StateLock);
  EOFState := State;
  SDL_mutexV(StateLock);
end;

function TFFmpegDecodeStream.IsError(): boolean;
begin
  SDL_mutexP(StateLock);
  Result := ErrorState;
  SDL_mutexV(StateLock);
end;

procedure TFFmpegDecodeStream.SetError(State: boolean);
begin
  SDL_mutexP(StateLock);
  ErrorState := State;
  SDL_mutexV(StateLock);
end;

function TFFmpegDecodeStream.IsSeeking(): boolean;
begin
  SDL_mutexP(StateLock);
  Result := SeekRequest;
  SDL_mutexV(StateLock);
end;

function TFFmpegDecodeStream.IsQuit(): boolean;
begin
  SDL_mutexP(StateLock);
  Result := QuitRequest;
  SDL_mutexV(StateLock);
end;

function TFFmpegDecodeStream.GetPosition(): real;
var
  BufferSizeSec: double;
begin
  PauseDecoder();

  // ReadData() does not return all of the buffer retrieved by DecodeFrame().
  // Determine the size of the unused part of the decode-buffer.
  BufferSizeSec := (AudioBufferSize - AudioBufferPos) /
                   FormatInfo.BytesPerSec;

  // subtract the size of unused buffer-data from the audio clock.
  Result := AudioStreamPos - BufferSizeSec;

  ResumeDecoder();
end;

procedure TFFmpegDecodeStream.SetPosition(Time: real);
begin
  SetPositionIntern(Time, true, true);
end;

function TFFmpegDecodeStream.GetLoop(): boolean;
begin
  SDL_mutexP(StateLock);
  Result := Loop;
  SDL_mutexV(StateLock);
end;

procedure TFFmpegDecodeStream.SetLoop(Enabled: boolean);
begin
  SDL_mutexP(StateLock);
  Loop := Enabled;
  SDL_mutexV(StateLock);
end;


(********************************************
 * Parser section
 ********************************************)

procedure TFFmpegDecodeStream.PauseParser();
begin
  if (SDL_ThreadID() = ParseThread.threadid) then
    Exit;
    
  SDL_mutexP(StateLock);
  Inc(ParserPauseRequestCount);
  while (ParserLocked) do
    SDL_CondWait(ParserUnlockedCond, StateLock);
  SDL_mutexV(StateLock);
end;

procedure TFFmpegDecodeStream.ResumeParser();
begin
  if (SDL_ThreadID() = ParseThread.threadid) then
    Exit;

  SDL_mutexP(StateLock);
  Dec(ParserPauseRequestCount);
  SDL_CondSignal(ParserResumeCond);
  SDL_mutexV(StateLock);
end;

procedure TFFmpegDecodeStream.SetPositionIntern(Time: real; Flush: boolean; Blocking: boolean);
begin
  // - Pause the parser first to prevent it from putting obsolete packages
  //   into the queue after the queue was flushed and before seeking is done.
  //   Otherwise we will hear fragments of old data, if the stream was seeked
  //   in stopped mode and resumed afterwards (applies to non-blocking mode only).
  // - Pause the decoder to avoid race-condition that might occur otherwise.
  // - Last lock the state lock because we are manipulating some shared state-vars.
  PauseParser();
  PauseDecoder();
  SDL_mutexP(StateLock);

  // configure seek parameters
  SeekPos := Time;
  SeekFlush := Flush;
  SeekFlags := AVSEEK_FLAG_ANY;
  SeekRequest := true;

  // Note: the BACKWARD-flag seeks to the first position <= the position
  // searched for. Otherwise e.g. position 0 might not be seeked correct.
  // For some reason ffmpeg sometimes doesn't use position 0 but the key-frame
  // following. In streams with few key-frames (like many flv-files) the next
  // key-frame after 0 might be 5secs ahead.
  if (Time < AudioStreamPos) then
    SeekFlags := SeekFlags or AVSEEK_FLAG_BACKWARD;

  EOFState := false;
  ErrorState := false;

  // send a reuse signal in case the parser was stopped (e.g. because of an EOF)
  SDL_CondSignal(ParserIdleCond);

  SDL_mutexV(StateLock);
  ResumeDecoder();
  ResumeParser();

  // in blocking mode, wait until seeking is done
  if (Blocking) then
  begin
    SDL_mutexP(StateLock);
    while (SeekRequest) do
      SDL_CondWait(SeekFinishedCond, StateLock);
    SDL_mutexV(StateLock);
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
    // wait for reuse or destruction of stream
    SDL_mutexP(StateLock);
    while (not (SeekRequest or QuitRequest)) do
      SDL_CondWait(ParserIdleCond, StateLock);
    SDL_mutexV(StateLock);
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
  StatusPacket: PAVPacket;
  SeekTarget: int64;
  ByteIOCtx: PByteIOContext;
  ErrorCode: integer;
  StartSilence: double;       // duration of silence at start of stream
  StartSilencePtr: PDouble;  // pointer for the EMPTY status packet 

  // Note: pthreads wakes threads waiting on a mutex in the order of their
  // priority and not in FIFO order. SDL does not provide any option to
  // control priorities. This might (and already did) starve threads waiting
  // on the mutex (e.g. SetPosition) making usdx look like it was froozen.
  // Instead of simply locking the critical section we set a ParserLocked flag
  // instead and give priority to the threads requesting the parser to pause.
  procedure LockParser();
  begin
    SDL_mutexP(StateLock);
    while (ParserPauseRequestCount > 0) do
      SDL_CondWait(ParserResumeCond, StateLock);
    ParserLocked := true;
    SDL_mutexV(StateLock);
  end;

  procedure UnlockParser();
  begin
    SDL_mutexP(StateLock);
    ParserLocked := false;
    SDL_CondBroadcast(ParserUnlockedCond);
    SDL_mutexV(StateLock);
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
      if (SeekRequest) then
      begin
        // first try: seek on the audio stream
        SeekTarget := Round(SeekPos / av_q2d(AudioStream^.time_base));
        StartSilence := 0;
        if (SeekTarget < AudioStream^.start_time) then
          StartSilence := (AudioStream^.start_time - SeekTarget) * av_q2d(AudioStream^.time_base);
        ErrorCode := av_seek_frame(FormatCtx, AudioStreamIndex, SeekTarget, SeekFlags);

        if (ErrorCode < 0) then
        begin
          // second try: seek on the default stream (necessary for flv-videos and some ogg-files)
          SeekTarget := Round(SeekPos * AV_TIME_BASE);
          StartSilence := 0;
          if (SeekTarget < FormatCtx^.start_time) then
            StartSilence := (FormatCtx^.start_time - SeekTarget) / AV_TIME_BASE;
          ErrorCode := av_seek_frame(FormatCtx, -1, SeekTarget, SeekFlags);
        end;

        // pause decoder and lock state (keep the lock-order to avoid deadlocks).
        // Note that the decoder does not block in the packet-queue in seeking state,
        // so locking the decoder here does not cause a dead-lock.
        PauseDecoder();
        SDL_mutexP(StateLock);
        try
          if (ErrorCode < 0) then
          begin
            // seeking failed
            ErrorState := true;
            Log.LogStatus('Seek Error in "'+FormatCtx^.filename+'"', 'UAudioDecoder_FFmpeg');
          end
          else
          begin
            if (SeekFlush) then
            begin
              // flush queue (we will send a Flush-Packet when seeking is finished)
              PacketQueue.Flush();

              // flush the decode buffers
              AudioBufferSize := 0;
              AudioBufferPos := 0;
              AudioPaketSize := 0;
              AudioPaketSilence := 0;
              FlushCodecBuffers();
              
              // Set preliminary stream position. The position will be set to
              // the correct value as soon as the first packet is decoded.
              AudioStreamPos := SeekPos;
            end
            else
            begin
              // request avcodec buffer flush
              PacketQueue.PutStatus(PKT_STATUS_FLAG_FLUSH, nil);
            end;

            // fill the gap between position 0 and start_time with silence
            // but not if we are in loop mode
            if ((StartSilence > 0) and (not Loop)) then
            begin
              GetMem(StartSilencePtr, SizeOf(StartSilence));
              StartSilencePtr^ := StartSilence;
              PacketQueue.PutStatus(PKT_STATUS_FLAG_EMPTY, StartSilencePtr);
            end;
          end;

          SeekRequest := false;
          SDL_CondBroadcast(SeekFinishedCond);
        finally
          SDL_mutexV(StateLock);
          ResumeDecoder();
        end;
      end;

      if (PacketQueue.GetSize() > MAX_AUDIOQ_SIZE) then
      begin
        SDL_Delay(10);
        Continue;
      end;

      if (av_read_frame(FormatCtx, Packet) < 0) then
      begin
        // failed to read a frame, check reason
        {$IF (LIBAVFORMAT_VERSION_MAJOR >= 52)}
        ByteIOCtx := FormatCtx^.pb;
        {$ELSE}
        ByteIOCtx := @FormatCtx^.pb;
        {$IFEND}

        // check for end-of-file (eof is not an error)
        if (url_feof(ByteIOCtx) <> 0) then
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
            PacketQueue.PutStatus(PKT_STATUS_FLAG_EOF, nil);
            Exit;
          end;
        end;

        // check for errors
        if (url_ferror(ByteIOCtx) <> 0) then
        begin
          // an error occured -> abort and wait for repositioning or termination
          PacketQueue.PutStatus(PKT_STATUS_FLAG_ERROR, nil);
          Exit;
        end;

        // no error -> wait for user input
        SDL_Delay(100);
        Continue;
      end;

      if (Packet.stream_index = AudioStreamIndex) then
        PacketQueue.Put(@Packet)
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
  SDL_mutexP(StateLock);
  Inc(DecoderPauseRequestCount);
  while (DecoderLocked) do
    SDL_CondWait(DecoderUnlockedCond, StateLock);
  SDL_mutexV(StateLock);
end;

procedure TFFmpegDecodeStream.ResumeDecoder();
begin
  SDL_mutexP(StateLock);
  Dec(DecoderPauseRequestCount);
  SDL_CondSignal(DecoderResumeCond);
  SDL_mutexV(StateLock);
end;

procedure TFFmpegDecodeStream.FlushCodecBuffers();
begin
  // if no flush operation is specified, avcodec_flush_buffers will not do anything.
  if (@CodecCtx.codec.flush <> nil) then
  begin
    // flush buffers used by avcodec_decode_audio, etc.
    avcodec_flush_buffers(CodecCtx);
  end
  else
  begin
    // we need a Workaround to avoid plopping noise with ogg-vorbis and
    // mp3 (in older versions of FFmpeg).
    // We will just reopen the codec.
    FFmpegCore.LockAVCodec();
    try
      avcodec_close(CodecCtx);
      avcodec_open(CodecCtx, Codec);
    finally
      FFmpegCore.UnlockAVCodec();
    end;
  end;
end;

function TFFmpegDecodeStream.DecodeFrame(Buffer: PByteArray; BufferSize: integer): integer;
var
  PaketDecodedSize: integer; // size of packet data used for decoding
  DataSize: integer;         // size of output data decoded by FFmpeg
  BlockQueue: boolean;
  SilenceDuration: double;
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
    if (AudioPaketSilence > 0) then
    begin
      DataSize := Min(AudioPaketSilence, BufferSize);
      FillChar(Buffer[0], DataSize, 0);
      Dec(AudioPaketSilence, DataSize);
      AudioStreamPos := AudioStreamPos + DataSize / FormatInfo.BytesPerSec; 
      Result := DataSize;
      Exit;
    end;

    // read packet data
    while (AudioPaketSize > 0) do
    begin
      DataSize := BufferSize;

      {$IF LIBAVCODEC_VERSION >= 51030000} // 51.30.0
      PaketDecodedSize := avcodec_decode_audio2(CodecCtx, PSmallint(Buffer),
                  DataSize, AudioPaketData, AudioPaketSize);
      {$ELSE}
      PaketDecodedSize := avcodec_decode_audio(CodecCtx, PSmallint(Buffer),
                  DataSize, AudioPaketData, AudioPaketSize);
      {$IFEND}

      if(PaketDecodedSize < 0) then
      begin
        // if error, skip frame
        {$IFDEF DebugFFmpegDecode}
        DebugWriteln('Skip audio frame');
        {$ENDIF}
        AudioPaketSize := 0;
        Break;
      end;

      Inc(AudioPaketData, PaketDecodedSize);
      Dec(AudioPaketSize, PaketDecodedSize);

      // check if avcodec_decode_audio returned data, otherwise fetch more frames
      if (DataSize <= 0) then
        Continue;

      // update stream position by the amount of fetched data
      AudioStreamPos := AudioStreamPos + DataSize / FormatInfo.BytesPerSec;
      
      // we have data, return it and come back for more later
      Result := DataSize;
      Exit;
    end;

    // free old packet data
    if (AudioPaket.data <> nil) then
      av_free_packet(@AudioPaket);

    // do not block queue on seeking (to avoid deadlocks on the DecoderLock)
    if (IsSeeking()) then
      BlockQueue := false
    else
      BlockQueue := true;

    // request a new packet and block if none available.
    // If this fails, the queue was aborted.
    if (PacketQueue.Get(AudioPaket, BlockQueue) <= 0) then
      Exit;

    // handle Status-packet
    if (PAnsiChar(AudioPaket.data) = STATUS_PACKET) then
    begin
      AudioPaket.data := nil;
      AudioPaketData := nil;
      AudioPaketSize := 0;

      case (AudioPaket.flags) of
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
          SilenceDuration := PDouble(PacketQueue.GetStatusInfo(AudioPaket))^;
          AudioPaketSilence := Round(SilenceDuration * FormatInfo.SampleRate) * FormatInfo.FrameSize;
          PacketQueue.FreeStatusInfo(AudioPaket);
        end
        else
        begin
          Log.LogStatus('Unknown status', 'TFFmpegDecodeStream.DecodeFrame');
        end;
      end;

      Continue;
    end;

    AudioPaketData := AudioPaket.data;
    AudioPaketSize := AudioPaket.size;

    // if available, update the stream position to the presentation time of this package
    if(AudioPaket.pts <> AV_NOPTS_VALUE) then
    begin
      {$IFDEF DebugFFmpegDecode}
      TmpPos := AudioStreamPos;
      {$ENDIF}
      AudioStreamPos := av_q2d(AudioStream^.time_base) * AudioPaket.pts;
      {$IFDEF DebugFFmpegDecode}
      DebugWriteln('Timestamp: ' + floattostrf(AudioStreamPos, ffFixed, 15, 3) + ' ' +
                   '(Calc: ' + floattostrf(TmpPos, ffFixed, 15, 3) + '), ' +
                   'Diff: ' + floattostrf(AudioStreamPos-TmpPos, ffFixed, 15, 3));
      {$ENDIF}
    end;
  end;
end;

function TFFmpegDecodeStream.ReadData(Buffer: PByteArray; BufferSize: integer): integer;
var
  CopyByteCount:   integer; // number of bytes to copy
  RemainByteCount: integer; // number of bytes left (remain) to read
  BufferPos: integer;

  // prioritize pause requests
  procedure LockDecoder();
  begin
    SDL_mutexP(StateLock);
    while (DecoderPauseRequestCount > 0) do
      SDL_CondWait(DecoderResumeCond, StateLock);
    DecoderLocked := true;
    SDL_mutexV(StateLock);
  end;

  procedure UnlockDecoder();
  begin
    SDL_mutexP(StateLock);
    DecoderLocked := false;
    SDL_CondBroadcast(DecoderUnlockedCond);
    SDL_mutexV(StateLock);
  end;

begin
  Result := -1;

  // set number of bytes to copy to the output buffer
  BufferPos := 0;

  LockDecoder();
  try
    // leave if end-of-file is reached
    if (EOF) then
      Exit;

    // copy data to output buffer
    while (BufferPos < BufferSize) do
    begin
      // check if we need more data
      if (AudioBufferPos >= AudioBufferSize) then
      begin
        AudioBufferPos := 0;

        // we have already sent all our data; get more
        AudioBufferSize := DecodeFrame(AudioBuffer, AUDIO_BUFFER_SIZE);

        // check for errors or EOF
        if(AudioBufferSize < 0) then
        begin
          Result := BufferPos;
          Exit;
        end;
      end;

      // calc number of new bytes in the decode-buffer
      CopyByteCount := AudioBufferSize - AudioBufferPos;
      // resize copy-count if more bytes available than needed (remaining bytes are used the next time)
      RemainByteCount := BufferSize - BufferPos;
      if (CopyByteCount > RemainByteCount) then
        CopyByteCount := RemainByteCount;

      Move(AudioBuffer[AudioBufferPos], Buffer[BufferPos], CopyByteCount);

      Inc(BufferPos,      CopyByteCount);
      Inc(AudioBufferPos, CopyByteCount);
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

function TAudioDecoder_FFmpeg.Open(const Filename: string): TAudioDecodeStream;
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
