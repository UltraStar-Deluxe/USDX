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

unit UVideo;

{*
 * based on 'An ffmpeg and SDL Tutorial' (http://www.dranger.com/ffmpeg/)
 *}

// uncomment if you want to see the debug stuff
{.$define DebugDisplay}
{.$define DebugFrames}
{.$define VideoBenchmark}
{.$define Info}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

// use BGR-format for accelerated colorspace conversion with swscale
{$IFDEF UseSWScale}
  {$DEFINE PIXEL_FMT_BGR}
{$ENDIF}

type
  {**
   * vacStretch: Stretch to screen width and height
   *   - ignores aspect
   *   + no borders
   *   + no image data loss
   * vacCrop: Stretch to screen width or height, crop the other dimension
   *   + keeps aspect
   *   + no borders
   *   - frame borders are cropped (image data loss)
   * vacLetterBox: Stretch to screen width, add bars at or crop top and bottom
   *   + keeps aspect
   *   - borders at top and bottom
   *   o top/bottom is cropped if width < height (unusual)
   *}
  TAspectCorrection = (acoStretch, acoCrop, acoLetterBox);


implementation

uses
  SysUtils,
  Math,
  SDL,
  avcodec,
  avformat,
  avutil,
  avio,
  rational,
  {$IFDEF UseSWScale}
  swscale,
  {$ENDIF}
  gl,
  glu,
  glext,
  textgl,
  UMediaCore_FFmpeg,
  UCommon,
  UConfig,
  ULog,
  UMusic,
  UGraphicClasses,
  UGraphic,
  UPath;

{$DEFINE PIXEL_FMT_BGR}

const
{$IFDEF PIXEL_FMT_BGR}
  PIXEL_FMT_OPENGL = GL_BGR;
  PIXEL_FMT_FFMPEG = PIX_FMT_BGR24;
  PIXEL_FMT_SIZE   = 3;

  // looks strange on linux:
  //PIXEL_FMT_OPENGL = GL_RGBA;
  //PIXEL_FMT_FFMPEG = PIX_FMT_BGR32;
  //PIXEL_FMT_SIZE   = 4;
{$ELSE}
  // looks strange on linux:
  PIXEL_FMT_OPENGL = GL_RGB;
  PIXEL_FMT_FFMPEG = PIX_FMT_RGB24;
  PIXEL_FMT_SIZE   = 3;
{$ENDIF}

type
  TRectCoords = record
    Left, Right:  double;
    Upper, Lower: double;
  end;

  IVideo_FFmpeg = interface (IVideo)
  ['{E640E130-C8C0-4399-AF02-67A3569313AB}']
    function Open(const FileName: IPath): boolean;
  end;

  TVideo_FFmpeg = class( TInterfacedObject, IVideo_FFmpeg )
  private
    fOpened: boolean;     //**< stream successfully opened
    fPaused: boolean;     //**< stream paused
    fEOF: boolean;        //**< end-of-file state

    fLoop: boolean;       //**< looping enabled

    fStream:        PAVStream;
    fStreamIndex :  integer;
    fFormatContext: PAVFormatContext;
    fCodecContext:  PAVCodecContext;
    fCodec:         PAVCodec;

    fAVFrame:     PAVFrame;
    fAVFrameRGB:  PAVFrame;

    fFrameBuffer: PByte;  //**< stores a FFmpeg video frame
    fFrameTex:    GLuint; //**< OpenGL texture for FrameBuffer
    fFrameTexValid: boolean; //**< if true, fFrameTex contains the current frame
    fTexWidth, fTexHeight: cardinal;

    {$IFDEF UseSWScale}
    fSwScaleContext: PSwsContext;
    {$ENDIF}

    fAspect: real;        //**< width/height ratio
    fAspectCorrection: TAspectCorrection;

    fTimeBase: extended;  //**< FFmpeg time base per time unit
    fFrameTime: extended;  //**< video time position (absolute)
    fLoopTime: extended;  //**< start time of the current loop

    fPboEnabled: boolean;
    fPboId:      GLuint;
    procedure Reset();
    function DecodeFrame(): boolean;
    procedure SynchronizeTime(Frame: PAVFrame; var pts: double);

    procedure GetVideoRect(var ScreenRect, TexRect: TRectCoords);

    procedure ShowDebugInfo();

  public
    constructor Create;
    destructor Destroy; override;

    function Open(const FileName: IPath): boolean;
    procedure Close;

    procedure Play;
    procedure Pause;
    procedure Stop;

    procedure SetLoop(Enable: boolean);
    function GetLoop(): boolean;

    procedure SetPosition(Time: real);
    function GetPosition: real;

    procedure GetFrame(Time: Extended);
    procedure DrawGL(Screen: integer);
  end;

  TVideoPlayback_FFmpeg = class( TInterfacedObject, IVideoPlayback )
  private
    fInitialized: boolean;

  public
    function GetName: String;

    function Init(): boolean;
    function Finalize: boolean;

    function Open(const FileName : IPath): IVideo;
  end;

var
  FFmpegCore: TMediaCore_FFmpeg;


// These are called whenever we allocate a frame buffer.
// We use this to store the global_pts in a frame at the time it is allocated.
function PtsGetBuffer(CodecCtx: PAVCodecContext; Frame: PAVFrame): integer; cdecl;
var
  pts: Pint64;
  VideoPktPts: Pint64;
begin
  Result := avcodec_default_get_buffer(CodecCtx, Frame);
  VideoPktPts := CodecCtx^.opaque;
  if (VideoPktPts <> nil) then
  begin
    // Note: we must copy the pts instead of passing a pointer, because the packet
    // (and with it the pts) might change before a frame is returned by av_decode_video.
    pts := av_malloc(sizeof(int64));
    pts^ := VideoPktPts^;
    Frame^.opaque := pts;
  end;
end;

procedure PtsReleaseBuffer(CodecCtx: PAVCodecContext; Frame: PAVFrame); cdecl;
begin
  if (Frame <> nil) then
    av_freep(@Frame^.opaque);
  avcodec_default_release_buffer(CodecCtx, Frame);
end;


{*------------------------------------------------------------------------------
 * TVideoPlayback_ffmpeg
 *------------------------------------------------------------------------------}

function  TVideoPlayback_FFmpeg.GetName: String;
begin
  result := 'FFmpeg_Video';
end;

function TVideoPlayback_FFmpeg.Init(): boolean;
begin
  Result := true;

  if (fInitialized) then
    Exit;
  fInitialized := true;

  FFmpegCore := TMediaCore_FFmpeg.GetInstance();

  av_register_all();
end;

function TVideoPlayback_FFmpeg.Finalize(): boolean;
begin
  Result := true;
end;

function TVideoPlayback_FFmpeg.Open(const FileName : IPath): IVideo;
var
  Video: IVideo_FFmpeg;
begin
  Video := TVideo_FFmpeg.Create;
  if Video.Open(FileName) then
    Result := Video
  else
    Result := nil;
end;


{* TVideo_FFmpeg *}

constructor TVideo_FFmpeg.Create;
begin
  glGenTextures(1, PGLuint(@fFrameTex));
  Reset();
end;

destructor TVideo_FFmpeg.Destroy;
begin
  Close();
  glDeleteTextures(1, PGLuint(@fFrameTex));
end;

function TVideo_FFmpeg.Open(const FileName : IPath): boolean;
var
  errnum: Integer;
  glErr: GLenum;
  AudioStreamIndex: integer;
begin
  Result := false;
  Reset();

  fPboEnabled := PboSupported;

  // use custom 'ufile' protocol for UTF-8 support
  errnum := av_open_input_file(fFormatContext, PAnsiChar('ufile:'+FileName.ToUTF8), nil, 0, nil);
  if (errnum <> 0) then
  begin
    Log.LogError('Failed to open file "'+ FileName.ToNative +'" ('+FFmpegCore.GetErrorString(errnum)+')');
    Exit;
  end;

  // update video info
  if (av_find_stream_info(fFormatContext) < 0) then
  begin
    Log.LogError('No stream info found', 'TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;
  Log.LogInfo('VideoStreamIndex : ' + inttostr(fStreamIndex), 'TVideoPlayback_ffmpeg.Open');

  // find video stream
  FFmpegCore.FindStreamIDs(fFormatContext, fStreamIndex, AudioStreamIndex);
  if (fStreamIndex < 0) then
  begin
    Log.LogError('No video stream found', 'TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;

  fStream := fFormatContext^.streams[fStreamIndex];
  fCodecContext := fStream^.codec;

  fCodec := avcodec_find_decoder(fCodecContext^.codec_id);
  if (fCodec = nil) then
  begin
    Log.LogError('No matching codec found', 'TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;

  // set debug options
  fCodecContext^.debug_mv := 0;
  fCodecContext^.debug := 0;

  // detect bug-workarounds automatically
  fCodecContext^.workaround_bugs := FF_BUG_AUTODETECT;
  // error resilience strategy (careful/compliant/agressive/very_aggressive)
  //fCodecContext^.error_resilience := FF_ER_CAREFUL; //FF_ER_COMPLIANT;
  // allow non spec compliant speedup tricks.
  //fCodecContext^.flags2 := fCodecContext^.flags2 or CODEC_FLAG2_FAST;

  // Note: avcodec_open() and avcodec_close() are not thread-safe and will
  // fail if called concurrently by different threads.
  FFmpegCore.LockAVCodec();
  try
    errnum := avcodec_open(fCodecContext, fCodec);
  finally
    FFmpegCore.UnlockAVCodec();
  end;
  if (errnum < 0) then
  begin
    Log.LogError('No matching codec found', 'TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;

  // register custom callbacks for pts-determination
  fCodecContext^.get_buffer := PtsGetBuffer;
  fCodecContext^.release_buffer := PtsReleaseBuffer;

  {$ifdef DebugDisplay}
  DebugWriteln('Found a matching Codec: '+ fCodecContext^.Codec.Name + sLineBreak +
    sLineBreak +
    '  Width = '+inttostr(fCodecContext^.width) +
    ', Height='+inttostr(fCodecContext^.height) + sLineBreak +
    '  Aspect    : '+inttostr(fCodecContext^.sample_aspect_ratio.num) + '/' +
                     inttostr(fCodecContext^.sample_aspect_ratio.den) + sLineBreak +
    '  Framerate : '+inttostr(fCodecContext^.time_base.num) + '/' +
                     inttostr(fCodecContext^.time_base.den));
  {$endif}

  // allocate space for decoded frame and rgb frame
  fAVFrame := avcodec_alloc_frame();
  fAVFrameRGB := avcodec_alloc_frame();
  fFrameBuffer := av_malloc(avpicture_get_size(PIXEL_FMT_FFMPEG,
      fCodecContext^.width, fCodecContext^.height));

  if ((fAVFrame = nil) or (fAVFrameRGB = nil) or (fFrameBuffer = nil)) then
  begin
    Log.LogError('Failed to allocate buffers', 'TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;

  // TODO: pad data for OpenGL to GL_UNPACK_ALIGNMENT
  // (otherwise video will be distorted if width/height is not a multiple of the alignment)
  errnum := avpicture_fill(PAVPicture(fAVFrameRGB), fFrameBuffer, PIXEL_FMT_FFMPEG,
      fCodecContext^.width, fCodecContext^.height);
  if (errnum < 0) then
  begin
    Log.LogError('avpicture_fill failed: ' + FFmpegCore.GetErrorString(errnum), 'TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;

  // calculate some information for video display
  fAspect := av_q2d(fCodecContext^.sample_aspect_ratio);
  if (fAspect = 0) then
    fAspect := fCodecContext^.width /
               fCodecContext^.height
  else
    fAspect := fAspect * fCodecContext^.width /
                         fCodecContext^.height;

  fTimeBase := 1/av_q2d(fStream^.r_frame_rate);

  // hack to get reasonable timebase (for divx and others)
  if (fTimeBase < 0.02) then // 0.02 <-> 50 fps
  begin
    fTimeBase := av_q2d(fStream^.r_frame_rate);
    while (fTimeBase > 50) do
      fTimeBase := fTimeBase/10;
    fTimeBase := 1/fTimeBase;
  end;

  Log.LogInfo('VideoTimeBase: ' + floattostr(fTimeBase), 'TVideoPlayback_ffmpeg.Open');
  Log.LogInfo('Framerate: '+inttostr(floor(1/fTimeBase))+'fps', 'TVideoPlayback_ffmpeg.Open');

  {$IFDEF UseSWScale}
  // if available get a SWScale-context -> faster than the deprecated img_convert().
  // SWScale has accelerated support for PIX_FMT_RGB32/PIX_FMT_BGR24/PIX_FMT_BGR565/PIX_FMT_BGR555.
  // Note: PIX_FMT_RGB32 is a BGR- and not an RGB-format (maybe a bug)!!!
  // The BGR565-formats (GL_UNSIGNED_SHORT_5_6_5) is way too slow because of its
  // bad OpenGL support. The BGR formats have MMX(2) implementations but no speed-up
  // could be observed in comparison to the RGB versions.
  fSwScaleContext := sws_getContext(
      fCodecContext^.width, fCodecContext^.height,
      fCodecContext^.pix_fmt,
      fCodecContext^.width, fCodecContext^.height,
      PIXEL_FMT_FFMPEG,
      SWS_FAST_BILINEAR, nil, nil, nil);
  if (fSwScaleContext = nil) then
  begin
    Log.LogError('Failed to get swscale context', 'TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;
  {$ENDIF}

  fTexWidth   := Round(Power(2, Ceil(Log2(fCodecContext^.width))));
  fTexHeight  := Round(Power(2, Ceil(Log2(fCodecContext^.height))));

  if (fPboEnabled) then
  begin
    glGetError();

    glGenBuffersARB(1, @fPboId);
    glBindBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB, fPboId);
    glBufferDataARB(
        GL_PIXEL_UNPACK_BUFFER_ARB,
        fCodecContext^.width * fCodecContext^.height * PIXEL_FMT_SIZE,
        nil,
        GL_STREAM_DRAW_ARB);
    glBindBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB, 0);

    glErr := glGetError();
    if (glErr <> GL_NO_ERROR) then
    begin
      fPboEnabled := false;
      Log.LogError('PBO initialization failed: ' + gluErrorString(glErr), 'TVideo_FFmpeg.Open');
    end;
  end;

  // we retrieve a texture just once with glTexImage2D and update it with glTexSubImage2D later.
  // Benefits: glTexSubImage2D is faster and supports non-power-of-two widths/height.
  glBindTexture(GL_TEXTURE_2D, fFrameTex);
  glTexImage2D(GL_TEXTURE_2D, 0, 3, fTexWidth, fTexHeight, 0,
      PIXEL_FMT_OPENGL, GL_UNSIGNED_BYTE, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  fOpened := true;
  Result := true;
end;

procedure TVideo_FFmpeg.Reset();
begin
  // close previously opened video
  Close();

  fOpened := False;
  fPaused := False;
  fTimeBase := 0;
  fFrameTime := 0;
  fStream := nil;
  fStreamIndex := -1;
  fFrameTexValid := false;

  fEOF := false;

  fLoop := false;
  fLoopTime := 0;

  fPboId := 0;

  fAspectCorrection := acoCrop;
end;

procedure TVideo_FFmpeg.Close;
begin
  if (fFrameBuffer <> nil) then
    av_free(fFrameBuffer);
  if (fAVFrameRGB <> nil) then
    av_free(fAVFrameRGB);
  if (fAVFrame <> nil) then
    av_free(fAVFrame);

  fAVFrame     := nil;
  fAVFrameRGB  := nil;
  fFrameBuffer := nil;

  if (fCodecContext <> nil) then
  begin
    // avcodec_close() is not thread-safe
    FFmpegCore.LockAVCodec();
    try
      avcodec_close(fCodecContext);
    finally
      FFmpegCore.UnlockAVCodec();
    end;
  end;

  if (fFormatContext <> nil) then
    av_close_input_file(fFormatContext);

  fCodecContext  := nil;
  fFormatContext := nil;

  if (fPboId <> 0) then
    glDeleteBuffersARB(1, @fPboId);

  fOpened := False;
end;

procedure TVideo_FFmpeg.SynchronizeTime(Frame: PAVFrame; var pts: double);
var
  FrameDelay: double;
begin
  if (pts <> 0) then
  begin
    // if we have pts, set video clock to it
    fFrameTime := pts;
  end else
  begin
    // if we aren't given a pts, set it to the clock
    pts := fFrameTime;
  end;
  // update the video clock
  FrameDelay := av_q2d(fCodecContext^.time_base);
  // if we are repeating a frame, adjust clock accordingly
  FrameDelay := FrameDelay + Frame^.repeat_pict * (FrameDelay * 0.5);
  fFrameTime := fFrameTime + FrameDelay;
end;

{**
 * Decode a new frame from the video stream.
 * The decoded frame is stored in fAVFrame. fFrameTime is updated to the new frame's
 * time.
 * @param pts will be updated to the presentation time of the decoded frame.
 * returns true if a frame could be decoded. False if an error or EOF occured.
 *}
function TVideo_FFmpeg.DecodeFrame(): boolean;
var
  FrameFinished: Integer;
  VideoPktPts: int64;
  pbIOCtx: PByteIOContext;
  errnum: integer;
  AVPacket: TAVPacket;
  pts: double;
begin
  Result := false;
  FrameFinished := 0;

  if fEOF then
    Exit;

  // read packets until we have a finished frame (or there are no more packets)
  while (FrameFinished = 0) do
  begin
    errnum := av_read_frame(fFormatContext, AVPacket);
    if (errnum < 0) then
    begin
      // failed to read a frame, check reason

      {$IF (LIBAVFORMAT_VERSION_MAJOR >= 52)}
      pbIOCtx := fFormatContext^.pb;
      {$ELSE}
      pbIOCtx := @fFormatContext^.pb;
      {$IFEND}

      // check for end-of-file (EOF is not an error)
      if (url_feof(pbIOCtx) <> 0) then
      begin
        fEOF := true;
        Exit;
      end;

      // check for errors
      if (url_ferror(pbIOCtx) <> 0) then
      begin
        Log.LogError('Video decoding file error', 'TVideoPlayback_FFmpeg.DecodeFrame');
        Exit;
      end;

      // url_feof() does not detect an EOF for some mov-files (e.g. deluxe.mov)
      // so we have to do it this way.
      if ((fFormatContext^.file_size <> 0) and
          (pbIOCtx^.pos >= fFormatContext^.file_size)) then
      begin
        fEOF := true;
        Exit;
      end;

      // error occured, log and exit
      Log.LogError('Video decoding error', 'TVideoPlayback_FFmpeg.DecodeFrame');
      Exit;
    end;

    // if we got a packet from the video stream, then decode it
    if (AVPacket.stream_index = fStreamIndex) then
    begin
      // save pts to be stored in pFrame in first call of PtsGetBuffer()
      VideoPktPts := AVPacket.pts;
      fCodecContext^.opaque := @VideoPktPts;

      // decode packet
      avcodec_decode_video(fCodecContext, fAVFrame,
          frameFinished, AVPacket.data, AVPacket.size);

      // reset opaque data
      fCodecContext^.opaque := nil;

      // update pts
      if (AVPacket.dts <> AV_NOPTS_VALUE) then
      begin
        pts := AVPacket.dts;
      end
      else if ((fAVFrame^.opaque <> nil) and
               (Pint64(fAVFrame^.opaque)^ <> AV_NOPTS_VALUE)) then
      begin
        pts := Pint64(fAVFrame^.opaque)^;
      end
      else
      begin
        pts := 0;
      end;

      if fStream^.start_time <> AV_NOPTS_VALUE then
        pts := pts - fStream^.start_time;

      pts := pts * av_q2d(fStream^.time_base);

      // synchronize time on each complete frame
      if (frameFinished <> 0) then
        SynchronizeTime(fAVFrame, pts);
    end;

    // free the packet from av_read_frame
    av_free_packet( @AVPacket );
  end;

  Result := true;
end;

procedure TVideo_FFmpeg.GetFrame(Time: Extended);
var
  errnum: Integer;
  glErr: GLenum;
  CurrentTime: Extended;
  TimeDiff: Extended;
  DropFrameCount: Integer;
  i: Integer;
  Success: boolean;
  BufferPtr: PGLvoid;
const
  SKIP_FRAME_DIFF = 0.010; // start skipping if we are >= 10ms too late
begin
  if not fOpened then
    Exit;

  if fPaused then
    Exit;

  {*
   * TODO:
   * Check if it is correct to assume that fTimeBase is the time of one frame?
   * The tutorial and FFPlay do not make this assumption.
   *}

  {*
   * Synchronization - begin
   *}

  // requested stream position (relative to the last loop's start)
  if (fLoop) then
    CurrentTime := Time - fLoopTime
  else
    CurrentTime := Time;

  // check if current texture still contains the active frame
  if (fFrameTexValid) then
  begin
    // time since the last frame was returned
    TimeDiff := CurrentTime - fFrameTime;

    {$IFDEF DebugDisplay}
    DebugWriteln('Time:      '+inttostr(floor(Time*1000)) + sLineBreak +
                 'VideoTime: '+inttostr(floor(fFrameTime*1000)) + sLineBreak +
                 'TimeBase:  '+inttostr(floor(fTimeBase*1000)) + sLineBreak +
                 'TimeDiff:  '+inttostr(floor(TimeDifference*1000)));
    {$endif}

    // check if time has reached the next frame
    if (TimeDiff < fTimeBase) then
    begin
      {$ifdef DebugFrames}
      // frame delay debug display
      GoldenRec.Spawn(200,15,1,16,0,-1,ColoredStar,$00ff00);
      {$endif}

      {$IFDEF DebugDisplay}
      DebugWriteln('not getting new frame' + sLineBreak +
          'Time:      '+inttostr(floor(Time*1000)) + sLineBreak +
          'VideoTime: '+inttostr(floor(fFrameTime*1000)) + sLineBreak +
          'TimeBase:  '+inttostr(floor(fTimeBase*1000)) + sLineBreak +
          'TimeDiff:  '+inttostr(floor(TimeDifference*1000)));
      {$endif}

      // we do not need a new frame now
      Exit;
    end;
  end;

  {$IFDEF VideoBenchmark}
  Log.BenchmarkStart(15);
  {$ENDIF}

  // fetch new frame (updates fFrameTime)
  Success := DecodeFrame();
  TimeDiff := CurrentTime - fFrameTime;

  // check if we have to skip frames
  // Either if we are one frame behind or if the skip threshold has been reached.
  // Do not skip if the difference is less than fTimeBase as there is no next frame.
  // Note: We assume that fTimeBase is the length of one frame.
  if (TimeDiff >= Max(fTimeBase, SKIP_FRAME_DIFF)) then
  begin
    {$IFDEF DebugFrames}
    //frame drop debug display
    GoldenRec.Spawn(200,55,1,16,0,-1,ColoredStar,$ff0000);
    {$ENDIF}
    {$IFDEF DebugDisplay}
    DebugWriteln('skipping frames' + sLineBreak +
        'TimeBase:  '+inttostr(floor(fTimeBase*1000)) + sLineBreak +
        'TimeDiff:  '+inttostr(floor(TimeDifference*1000)));
    {$endif}

    // update video-time
    DropFrameCount := Trunc(TimeDiff / fTimeBase);
    fFrameTime := fFrameTime + DropFrameCount*fTimeBase;

    // skip frames
    for i := 1 to DropFrameCount do
      Success := DecodeFrame();
  end;

  // check if we got an EOF or error
  if (not Success) then
  begin
    if fLoop then
    begin
      // we have to loop, so rewind
      SetPosition(0);
      // record the start-time of the current loop, so we can
      // determine the position in the stream (fFrameTime-fLoopTime) later.
      fLoopTime := Time;
    end;
    Exit;
  end;

  {*
   * Synchronization - end
   *}

  // TODO: support for pan&scan
  //if (fAVFrame.pan_scan <> nil) then
  //begin
  //  Writeln(Format('PanScan: %d/%d', [fAVFrame.pan_scan.width, fAVFrame.pan_scan.height]));
  //end;

  // otherwise we convert the pixeldata from YUV to RGB
  {$IFDEF UseSWScale}
  errnum := sws_scale(fSwScaleContext, @fAVFrame.data, @fAVFrame.linesize,
          0, fCodecContext^.Height,
          @fAVFrameRGB.data, @fAVFrameRGB.linesize);
  {$ELSE}
  // img_convert from lib/ffmpeg/avcodec.pas is actually deprecated.
  // If ./configure does not find SWScale then this gives the error
  // that the identifier img_convert is not known or similar.
  // I think this should be removed, but am not sure whether there should
  // be some other replacement or a warning, Therefore, I leave it for now.
  // April 2009, mischi
  errnum := img_convert(PAVPicture(fAVFrameRGB), PIXEL_FMT_FFMPEG,
            PAVPicture(fAVFrame), fCodecContext^.pix_fmt,
            fCodecContext^.width, fCodecContext^.height);
  {$ENDIF}

  if (errnum < 0) then
  begin
    Log.LogError('Image conversion failed', 'TVideoPlayback_ffmpeg.GetFrame');
    Exit;
  end;

  {$IFDEF VideoBenchmark}
  Log.BenchmarkEnd(15);
  Log.BenchmarkStart(16);
  {$ENDIF}

  // TODO: data is not padded, so we will need to tell OpenGL.
  //   Or should we add padding with avpicture_fill? (check which one is faster)
  //glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

  // glTexEnvi with GL_REPLACE might give a small speed improvement
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

  if (not fPboEnabled) then
  begin
    glBindTexture(GL_TEXTURE_2D, fFrameTex);
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0,
        fCodecContext^.width, fCodecContext^.height,
        PIXEL_FMT_OPENGL, GL_UNSIGNED_BYTE, fAVFrameRGB^.data[0]);
  end
  else // fPboEnabled
  begin
    glGetError();

    glBindBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB, fPboId);
    glBufferDataARB(GL_PIXEL_UNPACK_BUFFER_ARB,
        fCodecContext^.height * fCodecContext^.width * PIXEL_FMT_SIZE,
        nil,
        GL_STREAM_DRAW_ARB);

    bufferPtr := glMapBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB, GL_WRITE_ONLY_ARB);
    if(bufferPtr <> nil) then
    begin
      Move(fAVFrameRGB^.data[0]^, bufferPtr^,
           fCodecContext^.height * fCodecContext^.width * PIXEL_FMT_SIZE);

      // release pointer to mapping buffer
      glUnmapBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB);
    end;

    glBindTexture(GL_TEXTURE_2D, fFrameTex);
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0,
        fCodecContext^.width, fCodecContext^.height,
        PIXEL_FMT_OPENGL, GL_UNSIGNED_BYTE, nil);

    glBindBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB, 0);
    glBindTexture(GL_TEXTURE_2D, 0);

    glErr := glGetError();
    if (glErr <> GL_NO_ERROR) then
      Log.LogError('PBO texture stream error: ' + gluErrorString(glErr), 'TVideo_FFmpeg.GetFrame');
  end;

  // reset to default
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

  if (not fFrameTexValid) then
    fFrameTexValid := true;

  {$ifdef DebugFrames}
  //frame decode debug display
  GoldenRec.Spawn(200, 35, 1, 16, 0, -1, ColoredStar, $ffff00);
  {$endif}

  {$IFDEF VideoBenchmark}
  Log.BenchmarkEnd(16);
  Log.LogBenchmark('FFmpeg', 15);
  Log.LogBenchmark('Texture', 16);
  {$ENDIF}
end;

procedure TVideo_FFmpeg.GetVideoRect(var ScreenRect, TexRect: TRectCoords);
var
  ScreenAspect: double;  // aspect of screen resolution
  ScaledVideoWidth, ScaledVideoHeight: double;
begin
  // Three aspects to take into account:
  //  1. Screen/display resolution (e.g. 1920x1080 -> 16:9)
  //  2. Render aspect (fixed to 800x600 -> 4:3)
  //  3. Movie aspect (video frame aspect stored in fAspect)
  ScreenAspect := ScreenW / ScreenH;

  case fAspectCorrection of
    acoStretch: begin
      ScaledVideoWidth  := RenderW;
      ScaledVideoHeight := RenderH;
    end;
    acoCrop: begin
      if (ScreenAspect >= fAspect) then
      begin
        ScaledVideoWidth  := RenderW;
        ScaledVideoHeight := RenderH * ScreenAspect/fAspect;
      end
      else
      begin
        ScaledVideoHeight := RenderH;
        ScaledVideoWidth  := RenderW * fAspect/ScreenAspect;
      end;
    end;
    acoLetterBox: begin
      ScaledVideoWidth  := RenderW;
      ScaledVideoHeight := RenderH * ScreenAspect/fAspect;
    end
    else
      raise Exception.Create('Unhandled aspect correction!');
  end;

  // center video
  ScreenRect.Left  := (RenderW - ScaledVideoWidth) / 2;
  ScreenRect.Right := ScreenRect.Left + ScaledVideoWidth;
  ScreenRect.Upper := (RenderH - ScaledVideoHeight) / 2;
  ScreenRect.Lower := ScreenRect.Upper + ScaledVideoHeight;

  // texture contains right/lower (power-of-2) padding.
  // Determine the texture coords of the video frame.
  TexRect.Left  := 0;
  TexRect.Right := fCodecContext^.width  / fTexWidth;
  TexRect.Upper := 0;
  TexRect.Lower := fCodecContext^.height / fTexHeight;
end;

procedure TVideo_FFmpeg.DrawGL(Screen: integer);
var
  ScreenRect: TRectCoords;
  TexRect: TRectCoords;
begin
  // have a nice black background to draw on
  // (even if there were errors opening the vid)
  // TODO: Philipp: IMO TVideoPlayback should not clear the screen at
  //       all, because clearing is already done by the background class
  //       at this moment.
  if (Screen = 1) then
  begin
    // It is important that we just clear once before we start
    // drawing the first screen otherwise the first screen
    // would be cleared by the drawgl called when the second
    // screen is drawn
    glClearColor(0, 0, 0, 0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  end;

  // exit if there's nothing to draw
  if (not fOpened) then
    Exit;

  {$IFDEF VideoBenchmark}
  Log.BenchmarkStart(15);
  {$ENDIF}

  // get texture and screen positions
  GetVideoRect(ScreenRect, TexRect);

  // we could use blending for brightness control, but do we need this?
  glDisable(GL_BLEND);

  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, fFrameTex);
  glColor3f(1, 1, 1);
  glBegin(GL_QUADS);
    // upper-left coord
    glTexCoord2f(TexRect.Left, TexRect.Upper);
    glVertex2f(ScreenRect.Left, ScreenRect.Upper);
    // lower-left coord
    glTexCoord2f(TexRect.Left, TexRect.Lower);
    glVertex2f(ScreenRect.Left, ScreenRect.Lower);
    // lower-right coord
    glTexCoord2f(TexRect.Right, TexRect.Lower);
    glVertex2f(ScreenRect.Right, ScreenRect.Lower);
    // upper-right coord
    glTexCoord2f(TexRect.Right, TexRect.Upper);
    glVertex2f(ScreenRect.Right, ScreenRect.Upper);
  glEnd;
  glDisable(GL_TEXTURE_2D);

  {$IFDEF VideoBenchmark}
  Log.BenchmarkEnd(15);
  Log.LogBenchmark('DrawGL', 15);
  {$ENDIF}

  {$IF Defined(Info) or Defined(DebugFrames)}
  ShowDebugInfo();
  {$IFEND}
end;

procedure TVideo_FFmpeg.ShowDebugInfo();
begin
  {$IFDEF Info}
  if (fFrameTime+fTimeBase < 0) then
  begin
    glColor4f(0.7, 1, 0.3, 1);
    SetFontStyle (1);
    SetFontItalic(False);
    SetFontSize(27);
    SetFontPos (300, 0);
    glPrint('Delay due to negative VideoGap');
    glColor4f(1, 1, 1, 1);
  end;
  {$ENDIF}

  {$IFDEF DebugFrames}
    glColor4f(0, 0, 0, 0.2);
    glbegin(GL_QUADS);
      glVertex2f(0, 0);
      glVertex2f(0, 70);
      glVertex2f(250, 70);
      glVertex2f(250, 0);
    glEnd;

    glColor4f(1, 1, 1, 1);
    SetFontStyle (1);
    SetFontItalic(False);
    SetFontSize(27);
    SetFontPos (5, 0);
    glPrint('delaying frame');
    SetFontPos (5, 20);
    glPrint('fetching frame');
    SetFontPos (5, 40);
    glPrint('dropping frame');
  {$ENDIF}
end;

procedure TVideo_FFmpeg.Play;
begin
end;

procedure TVideo_FFmpeg.Pause;
begin
  fPaused := not fPaused;
end;

procedure TVideo_FFmpeg.Stop;
begin
end;

procedure TVideo_FFmpeg.SetLoop(Enable: boolean);
begin
  fLoop := Enable;
  fLoopTime := 0;
end;

function TVideo_FFmpeg.GetLoop(): boolean;
begin
  Result := fLoop;
end;

{**
 * Sets the stream's position.
 * The stream is set to the first keyframe with timestamp <= Time.
 * Note that fFrameTime is set to Time no matter if the actual position seeked to is
 * at Time or the time of a preceding keyframe. fFrameTime will be updated to the
 * actual frame time when GetFrame() is called the next time.
 * @param Time new position in seconds
 *}
procedure TVideo_FFmpeg.SetPosition(Time: real);
var
  SeekFlags: integer;
begin
  if not fOpened then
    Exit;

  if (Time < 0) then
    Time := 0;

  // TODO: handle fLoop-times
  //Time := Time mod VideoDuration;

  // Do not use the AVSEEK_FLAG_ANY here. It will seek to any frame, even
  // non keyframes (P-/B-frames). It will produce corrupted video frames as
  // FFmpeg does not use the information of the preceding I-frame.
  // The picture might be gray or green until the next keyframe occurs.
  // Instead seek the first keyframe smaller than the requested time
  // (AVSEEK_FLAG_BACKWARD). As this can be some seconds earlier than the
  // requested time, let the sync in GetFrame() do its job.
  SeekFlags := AVSEEK_FLAG_BACKWARD;

  fFrameTime := Time;
  fEOF := false;
  fFrameTexValid := false;

  if (av_seek_frame(fFormatContext, fStreamIndex, Floor(Time/fTimeBase), SeekFlags) < 0) then
  begin
    Log.LogError('av_seek_frame() failed', 'TVideoPlayback_ffmpeg.SetPosition');
    Exit;
  end;

  avcodec_flush_buffers(fCodecContext);
end;

function  TVideo_FFmpeg.GetPosition: real;
begin
  Result := fFrameTime;
end;

initialization
  MediaManager.Add(TVideoPlayback_FFmpeg.Create);

end.
