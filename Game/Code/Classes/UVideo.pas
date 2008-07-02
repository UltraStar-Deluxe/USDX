{##############################################################################
 #                    FFmpeg support for UltraStar deluxe                     #
 #                                                                            #
 #    Created by b1indy                                                       #
 #    based on 'An ffmpeg and SDL Tutorial' (http://www.dranger.com/ffmpeg/)  #
 #    with modifications by Jay Binks <jaybinks@gmail.com>                    #
 #                                                                            #
 # http://www.mail-archive.com/fpc-pascal@lists.freepascal.org/msg09949.html  #
 # http://www.nabble.com/file/p11795857/mpegpas01.zip                         #
 #                                                                            #
 ##############################################################################}

unit UVideo;

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

implementation

uses
  SDL,
  textgl,
  avcodec,
  avformat,
  avutil,
  avio,
  rational,
  {$IFDEF UseSWScale}
  swscale,
  {$ENDIF}
  UMediaCore_FFMpeg,
  math,
  gl,
  glext,
  SysUtils,
  UCommon,
  UConfig,
  ULog,
  UMusic,
  UGraphicClasses,
  UGraphic;

const
{$IFDEF PIXEL_FMT_BGR}
  PIXEL_FMT_OPENGL = GL_BGR;
  PIXEL_FMT_FFMPEG = PIX_FMT_BGR24;
{$ELSE}
  PIXEL_FMT_OPENGL = GL_RGB;
  PIXEL_FMT_FFMPEG = PIX_FMT_RGB24;
{$ENDIF}

type
  TVideoPlayback_FFMpeg = class( TInterfacedObject, IVideoPlayback )
  private
    fVideoOpened,
    fVideoPaused: Boolean;

    VideoStream: PAVStream;
    VideoStreamIndex : Integer;
    VideoFormatContext: PAVFormatContext;
    VideoCodecContext: PAVCodecContext;
    VideoCodec: PAVCodec;

    AVFrame: PAVFrame;
    AVFrameRGB: PAVFrame;
    FrameBuffer: PByte;

    {$IFDEF UseSWScale}
    SoftwareScaleContext: PSwsContext;
    {$ENDIF}

    fVideoTex: GLuint;
    TexWidth, TexHeight: Cardinal;

    VideoAspect: Real;
    VideoTimeBase, VideoTime: Extended;
    fLoopTime: Extended;

    EOF: boolean;
    Loop: boolean;

    Initialized: boolean;
    
    procedure Reset();
    function DecodeFrame(var AVPacket: TAVPacket; out pts: double): boolean;
    procedure SynchronizeVideo(Frame: PAVFrame; var pts: double);
  public
    function    GetName: String;

    function    Init(): boolean;
    function    Finalize: boolean;

    function    Open(const aFileName : string): boolean; // true if succeed
    procedure   Close;

    procedure   Play;
    procedure   Pause;
    procedure   Stop;

    procedure   SetPosition(Time: real);
    function    GetPosition: real;

    procedure   GetFrame(Time: Extended);
    procedure   DrawGL(Screen: integer);
  end;

var
  FFMpegCore: TMediaCore_FFMpeg;

  
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

function  TVideoPlayback_FFMpeg.GetName: String;
begin
  result := 'FFMpeg_Video';
end;

function TVideoPlayback_FFMpeg.Init(): boolean;
begin
  Result := true;

  if (Initialized) then
    Exit;
  Initialized := true;

  FFMpegCore := TMediaCore_FFMpeg.GetInstance();

  Reset();
  av_register_all();
  glGenTextures(1, PGLuint(@fVideoTex));
end;

function TVideoPlayback_FFMpeg.Finalize(): boolean;
begin
  Close();
  glDeleteTextures(1, PGLuint(@fVideoTex));
  Result := true;
end;

procedure TVideoPlayback_FFMpeg.Reset();
begin
  // close previously opened video
  Close();

  fVideoOpened       := False;
  fVideoPaused       := False;
  VideoTimeBase      := 0;
  VideoTime          := 0;
  VideoStream := nil;
  VideoStreamIndex := -1;

  EOF := false;

  // TODO: do we really want this by default?
  Loop := true;
  fLoopTime := 0;
end;

function TVideoPlayback_FFMpeg.Open(const aFileName : string): boolean; // true if succeed
var
  errnum: Integer;
  AudioStreamIndex: integer;
begin
  Result := false;

  Reset();

  errnum := av_open_input_file(VideoFormatContext, PChar(aFileName), nil, 0, nil);
  if (errnum <> 0) then
  begin
    Log.LogError('Failed to open file "'+aFileName+'" ('+FFMpegCore.GetErrorString(errnum)+')');
    Exit;
  end;

  // update video info
  if (av_find_stream_info(VideoFormatContext) < 0) then
  begin
    Log.LogError('No stream info found', 'TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;
  Log.LogInfo('VideoStreamIndex : ' + inttostr(VideoStreamIndex), 'TVideoPlayback_ffmpeg.Open');

  // find video stream
  FFMpegCore.FindStreamIDs(VideoFormatContext, VideoStreamIndex, AudioStreamIndex);
  if (VideoStreamIndex < 0) then
  begin
    Log.LogError('No video stream found', 'TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;

  VideoStream := VideoFormatContext^.streams[VideoStreamIndex];
  VideoCodecContext := VideoStream^.codec;

  VideoCodec := avcodec_find_decoder(VideoCodecContext^.codec_id);
  if (VideoCodec = nil) then
  begin
    Log.LogError('No matching codec found', 'TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;

  // set debug options
  VideoCodecContext^.debug_mv := 0;
  VideoCodecContext^.debug := 0;

  // detect bug-workarounds automatically
  VideoCodecContext^.workaround_bugs := FF_BUG_AUTODETECT;
  // error resilience strategy (careful/compliant/agressive/very_aggressive)
  //VideoCodecContext^.error_resilience := FF_ER_CAREFUL; //FF_ER_COMPLIANT;
  // allow non spec compliant speedup tricks.
  //VideoCodecContext^.flags2 := VideoCodecContext^.flags2 or CODEC_FLAG2_FAST;

  // Note: avcodec_open() and avcodec_close() are not thread-safe and will
  // fail if called concurrently by different threads.
  FFMpegCore.LockAVCodec();
  try
    errnum := avcodec_open(VideoCodecContext, VideoCodec);
  finally
    FFMpegCore.UnlockAVCodec();
  end;
  if (errnum < 0) then
  begin
    Log.LogError('No matching codec found', 'TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;

  // register custom callbacks for pts-determination
  VideoCodecContext^.get_buffer := PtsGetBuffer;
  VideoCodecContext^.release_buffer := PtsReleaseBuffer;

  {$ifdef DebugDisplay}
  DebugWriteln('Found a matching Codec: '+ VideoCodecContext^.Codec.Name + sLineBreak +
    sLineBreak +
    '  Width = '+inttostr(VideoCodecContext^.width) +
    ', Height='+inttostr(VideoCodecContext^.height) + sLineBreak +
    '  Aspect    : '+inttostr(VideoCodecContext^.sample_aspect_ratio.num) + '/' +
                     inttostr(VideoCodecContext^.sample_aspect_ratio.den) + sLineBreak +
    '  Framerate : '+inttostr(VideoCodecContext^.time_base.num) + '/' +
                     inttostr(VideoCodecContext^.time_base.den));
  {$endif}

  // allocate space for decoded frame and rgb frame
  AVFrame := avcodec_alloc_frame();
  AVFrameRGB := avcodec_alloc_frame();
  FrameBuffer := av_malloc(avpicture_get_size(PIXEL_FMT_FFMPEG,
      VideoCodecContext^.width, VideoCodecContext^.height));

  if ((AVFrame = nil) or (AVFrameRGB = nil) or (FrameBuffer = nil)) then
  begin
    Log.LogError('Failed to allocate buffers', 'TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;

  // TODO: pad data for OpenGL to GL_UNPACK_ALIGNMENT
  // (otherwise video will be distorted if width/height is not a multiple of the alignment)
  errnum := avpicture_fill(PAVPicture(AVFrameRGB), FrameBuffer, PIXEL_FMT_FFMPEG,
      VideoCodecContext^.width, VideoCodecContext^.height);
  if (errnum < 0) then
  begin
    Log.LogError('avpicture_fill failed: ' + FFMpegCore.GetErrorString(errnum), 'TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;

  // calculate some information for video display
  VideoAspect := av_q2d(VideoCodecContext^.sample_aspect_ratio);
  if (VideoAspect = 0) then
    VideoAspect := VideoCodecContext^.width /
                   VideoCodecContext^.height
  else
    VideoAspect := VideoAspect * VideoCodecContext^.width /
                                 VideoCodecContext^.height;

  VideoTimeBase := 1/av_q2d(VideoStream^.r_frame_rate);

  // hack to get reasonable timebase (for divx and others)
  if (VideoTimeBase < 0.02) then // 0.02 <-> 50 fps
  begin
    VideoTimeBase := av_q2d(VideoStream^.r_frame_rate);
    while (VideoTimeBase > 50) do
      VideoTimeBase := VideoTimeBase/10;
    VideoTimeBase := 1/VideoTimeBase;
  end;

  Log.LogInfo('VideoTimeBase: ' + floattostr(VideoTimeBase), 'TVideoPlayback_ffmpeg.Open');
  Log.LogInfo('Framerate: '+inttostr(floor(1/VideoTimeBase))+'fps', 'TVideoPlayback_ffmpeg.Open');

  {$IFDEF UseSWScale}
  // if available get a SWScale-context -> faster than the deprecated img_convert().
  // SWScale has accelerated support for PIX_FMT_RGB32/PIX_FMT_BGR24/PIX_FMT_BGR565/PIX_FMT_BGR555.
  // Note: PIX_FMT_RGB32 is a BGR- and not an RGB-format (maybe a bug)!!!
  // The BGR565-formats (GL_UNSIGNED_SHORT_5_6_5) is way too slow because of its
  // bad OpenGL support. The BGR formats have MMX(2) implementations but no speed-up
  // could be observed in comparison to the RGB versions.
  SoftwareScaleContext := sws_getContext(
      VideoCodecContext^.width, VideoCodecContext^.height,
      integer(VideoCodecContext^.pix_fmt),
      VideoCodecContext^.width, VideoCodecContext^.height,
      integer(PIXEL_FMT_FFMPEG),
      SWS_FAST_BILINEAR, nil, nil, nil);
  if (SoftwareScaleContext = nil) then
  begin
    Log.LogError('Failed to get swscale context', 'TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;
  {$ENDIF}

  TexWidth   := Round(Power(2, Ceil(Log2(VideoCodecContext^.width))));
  TexHeight  := Round(Power(2, Ceil(Log2(VideoCodecContext^.height))));

  // we retrieve a texture just once with glTexImage2D and update it with glTexSubImage2D later.
  // Benefits: glTexSubImage2D is faster and supports non-power-of-two widths/height.
  glBindTexture(GL_TEXTURE_2D, fVideoTex);
  glTexEnvi(GL_TEXTURE_2D, GL_TEXTURE_ENV_MODE, GL_REPLACE);
  glTexImage2D(GL_TEXTURE_2D, 0, 3, TexWidth, TexHeight, 0,
      PIXEL_FMT_OPENGL, GL_UNSIGNED_BYTE, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);


  fVideoOpened := True;

  Result := true;
end;

procedure TVideoPlayback_FFMpeg.Close;
begin
  if (FrameBuffer <> nil) then
    av_free(FrameBuffer);
  if (AVFrameRGB <> nil) then
    av_free(AVFrameRGB);
  if (AVFrame <> nil) then
    av_free(AVFrame);

  AVFrame     := nil;
  AVFrameRGB  := nil;
  FrameBuffer := nil;
    
  if (VideoCodecContext <> nil) then
  begin
    // avcodec_close() is not thread-safe
    FFMpegCore.LockAVCodec();
    try
      avcodec_close(VideoCodecContext);
    finally
      FFMpegCore.UnlockAVCodec();
    end;
  end;

  if (VideoFormatContext <> nil) then
    av_close_input_file(VideoFormatContext);

  VideoCodecContext  := nil;
  VideoFormatContext := nil;

  fVideoOpened := False;
end;

procedure TVideoPlayback_FFMpeg.SynchronizeVideo(Frame: PAVFrame; var pts: double);
var
  FrameDelay: double;
begin
  if (pts <> 0) then
  begin
    // if we have pts, set video clock to it
    VideoTime := pts;
  end else
  begin
    // if we aren't given a pts, set it to the clock
    pts := VideoTime;
  end;
  // update the video clock
  FrameDelay := av_q2d(VideoCodecContext^.time_base);
  // if we are repeating a frame, adjust clock accordingly
  FrameDelay := FrameDelay + Frame^.repeat_pict * (FrameDelay * 0.5);
  VideoTime := VideoTime + FrameDelay;
end;

function TVideoPlayback_FFMpeg.DecodeFrame(var AVPacket: TAVPacket; out pts: double): boolean;
var
  FrameFinished: Integer;
  VideoPktPts: int64;
  pbIOCtx: PByteIOContext;
  errnum: integer;
begin
  Result := false;
  FrameFinished := 0;

  if EOF then
    Exit;

  // read packets until we have a finished frame (or there are no more packets)
  while (FrameFinished = 0) do
  begin
    errnum := av_read_frame(VideoFormatContext, AVPacket);
    if (errnum < 0) then
    begin
      // failed to read a frame, check reason

      {$IF (LIBAVFORMAT_VERSION_MAJOR >= 52)}
      pbIOCtx := VideoFormatContext^.pb;
      {$ELSE}
      pbIOCtx := @VideoFormatContext^.pb;
      {$IFEND}

      // check for end-of-file (eof is not an error)
      if (url_feof(pbIOCtx) <> 0) then
      begin
        EOF := true;
        Exit;
      end;

      // check for errors
      if (url_ferror(pbIOCtx) <> 0) then
        Exit;

      // url_feof() does not detect an EOF for some mov-files (e.g. deluxe.mov)
      // so we have to do it this way.
      if ((VideoFormatContext^.file_size <> 0) and
          (pbIOCtx^.pos >= VideoFormatContext^.file_size)) then
      begin
        EOF := true;
        Exit;
      end;

      // no error -> wait for user input
      SDL_Delay(100);
      continue;
    end;

    // if we got a packet from the video stream, then decode it
    if (AVPacket.stream_index = VideoStreamIndex) then
    begin
      // save pts to be stored in pFrame in first call of PtsGetBuffer()
      VideoPktPts := AVPacket.pts;
      VideoCodecContext^.opaque := @VideoPktPts;

      // decode packet
      avcodec_decode_video(VideoCodecContext, AVFrame,
          frameFinished, AVPacket.data, AVPacket.size);

      // reset opaque data
      VideoCodecContext^.opaque := nil;

      // update pts
      if (AVPacket.dts <> AV_NOPTS_VALUE) then
      begin
        pts := AVPacket.dts;
      end
      else if ((AVFrame^.opaque <> nil) and
               (Pint64(AVFrame^.opaque)^ <> AV_NOPTS_VALUE)) then
      begin
        pts := Pint64(AVFrame^.opaque)^;
      end
      else
      begin
        pts := 0;
      end;
      pts := pts * av_q2d(VideoStream^.time_base);

      // synchronize on each complete frame
      if (frameFinished <> 0) then
        SynchronizeVideo(AVFrame, pts);
    end;

    // free the packet from av_read_frame
    av_free_packet( @AVPacket );
  end;

  Result := true;
end;

procedure TVideoPlayback_FFMpeg.GetFrame(Time: Extended);
var
  AVPacket: TAVPacket;
  errnum: Integer;
  myTime: Extended;
  TimeDifference: Extended;
  DropFrameCount: Integer;
  pts: double;
  i: Integer;
const
  FRAME_DROPCOUNT = 3;
begin
  if not fVideoOpened then
    Exit;

  if fVideoPaused then
    Exit;

  // current time, relative to last loop (if any)
  myTime := Time - fLoopTime;
  // time since the last frame was returned
  TimeDifference := myTime - VideoTime;

  {$IFDEF DebugDisplay}
  DebugWriteln('Time:      '+inttostr(floor(Time*1000)) + sLineBreak +
               'VideoTime: '+inttostr(floor(VideoTime*1000)) + sLineBreak +
               'TimeBase:  '+inttostr(floor(VideoTimeBase*1000)) + sLineBreak +
               'TimeDiff:  '+inttostr(floor(TimeDifference*1000)));
  {$endif}

  // check if a new frame is needed
  if (VideoTime <> 0) and (TimeDifference < VideoTimeBase) then
  begin
    {$ifdef DebugFrames}
    // frame delay debug display
    GoldenRec.Spawn(200,15,1,16,0,-1,ColoredStar,$00ff00);
    {$endif}

    {$IFDEF DebugDisplay}
    DebugWriteln('not getting new frame' + sLineBreak +
        'Time:      '+inttostr(floor(Time*1000)) + sLineBreak +
        'VideoTime: '+inttostr(floor(VideoTime*1000)) + sLineBreak +
        'TimeBase:  '+inttostr(floor(VideoTimeBase*1000)) + sLineBreak +
        'TimeDiff:  '+inttostr(floor(TimeDifference*1000)));
    {$endif}

    // we do not need a new frame now
    Exit;
  end;

  // update video-time to the next frame
  VideoTime := VideoTime + VideoTimeBase;
  TimeDifference := myTime - VideoTime;

  // check if we have to skip frames
  if (TimeDifference >= FRAME_DROPCOUNT*VideoTimeBase) then
  begin
    {$IFDEF DebugFrames}
    //frame drop debug display
    GoldenRec.Spawn(200,55,1,16,0,-1,ColoredStar,$ff0000);
    {$ENDIF}
    {$IFDEF DebugDisplay}
    DebugWriteln('skipping frames' + sLineBreak +
        'TimeBase:  '+inttostr(floor(VideoTimeBase*1000)) + sLineBreak +
        'TimeDiff:  '+inttostr(floor(TimeDifference*1000)));
    {$endif}

    // update video-time
    DropFrameCount := Trunc(TimeDifference / VideoTimeBase);
    VideoTime := VideoTime + DropFrameCount*VideoTimeBase;

    // skip half of the frames, this is much smoother than to skip all at once
    for i := 1 to DropFrameCount (*div 2*) do
      DecodeFrame(AVPacket, pts);
  end;

  {$IFDEF VideoBenchmark}
  Log.BenchmarkStart(15);
  {$ENDIF}

  if (not DecodeFrame(AVPacket, pts)) then
  begin
    if Loop then
    begin
      // Record the time we looped. This is used to keep the loops in time. otherwise they speed
      SetPosition(0);
      fLoopTime := Time;
    end;
    Exit;
  end;

  // otherwise we convert the pixeldata from YUV to RGB
  {$IFDEF UseSWScale}
  errnum := sws_scale(SoftwareScaleContext, @(AVFrame.data), @(AVFrame.linesize),
          0, VideoCodecContext^.Height,
          @(AVFrameRGB.data), @(AVFrameRGB.linesize));
  {$ELSE}
  errnum := img_convert(PAVPicture(AVFrameRGB), PIXEL_FMT_FFMPEG,
            PAVPicture(AVFrame), VideoCodecContext^.pix_fmt,
            VideoCodecContext^.width, VideoCodecContext^.height);
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

  glBindTexture(GL_TEXTURE_2D, fVideoTex);
  glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0,
      VideoCodecContext^.width, VideoCodecContext^.height,
      PIXEL_FMT_OPENGL, GL_UNSIGNED_BYTE, AVFrameRGB^.data[0]);

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

procedure TVideoPlayback_FFMpeg.DrawGL(Screen: integer);
var
  TexVideoRightPos, TexVideoLowerPos: Single;
  ScreenLeftPos,  ScreenRightPos: Single;
  ScreenUpperPos, ScreenLowerPos: Single;
  ScaledVideoWidth, ScaledVideoHeight: Single;
  ScreenMidPosX, ScreenMidPosY: Single;
  ScreenAspect, RenderAspect: Single;
begin
  // have a nice black background to draw on (even if there were errors opening the vid)
  if (Screen = 1) then
  begin
    glClearColor(0, 0, 0, 0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  end;

  // exit if there's nothing to draw
  if (not fVideoOpened) then
    Exit;

  {$IFDEF VideoBenchmark}
  Log.BenchmarkStart(15);
  {$ENDIF}

  // TODO: add a SetAspectCorrectionMode() function so we can switch
  // aspect correction. The screens video backgrounds look very ugly with aspect
  // correction because of the white bars at the top and bottom.
  
  ScreenAspect := ScreenW / ScreenH;
  RenderAspect := RenderW / RenderH;
  ScaledVideoWidth := RenderW;
  ScaledVideoHeight := ScaledVideoWidth/VideoAspect * ScreenAspect/RenderAspect;

  // Note: Scaling the width does not look good because the video might contain
  // black borders at the top already 
  //ScaledVideoHeight := RenderH;
  //ScaledVideoWidth := ScaledVideoHeight*VideoAspect * RenderAspect/ScreenAspect;

  // center the video
  ScreenMidPosX := RenderW/2;
  ScreenMidPosY := RenderH/2;
  ScreenLeftPos  := ScreenMidPosX - ScaledVideoWidth/2;
  ScreenRightPos := ScreenMidPosX + ScaledVideoWidth/2;
  ScreenUpperPos := ScreenMidPosY - ScaledVideoHeight/2;
  ScreenLowerPos := ScreenMidPosY + ScaledVideoHeight/2;
  // the video-texture contains empty borders because its width and height must be
  // a power of 2. So we have to determine the texture coords of the video.
  TexVideoRightPos := VideoCodecContext^.width  / TexWidth;
  TexVideoLowerPos := VideoCodecContext^.height / TexHeight;

  // we could use blending for brightness control, but do we need this?
  glDisable(GL_BLEND);

  // TODO: disable other stuff like lightning, etc.

  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, fVideoTex);
  glColor3f(1, 1, 1);
  glBegin(GL_QUADS);
    // upper-left coord
    glTexCoord2f(0, 0);
    glVertex2f(ScreenLeftPos, ScreenUpperPos);
    // lower-left coord
    glTexCoord2f(0, TexVideoLowerPos);
    glVertex2f(ScreenLeftPos, ScreenLowerPos);
    // lower-right coord
    glTexCoord2f(TexVideoRightPos, TexVideoLowerPos);
    glVertex2f(ScreenRightPos, ScreenLowerPos);
    // upper-right coord
    glTexCoord2f(TexVideoRightPos, 0);
    glVertex2f(ScreenRightPos, ScreenUpperPos);
  glEnd;
  glDisable(GL_TEXTURE_2D);

  {$IFDEF VideoBenchmark}
  Log.BenchmarkEnd(15);
  Log.LogBenchmark('DrawGL', 15);
  {$ENDIF}

  {$IFDEF Info}
  if (fVideoSkipTime+VideoTime+VideoTimeBase < 0) then
  begin
    glColor4f(0.7, 1, 0.3, 1);
    SetFontStyle (1);
    SetFontItalic(False);
    SetFontSize(9);
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
    SetFontSize(9);
    SetFontPos (5, 0);
    glPrint('delaying frame');
    SetFontPos (5, 20);
    glPrint('fetching frame');
    SetFontPos (5, 40);
    glPrint('dropping frame');
  {$ENDIF}
end;

procedure TVideoPlayback_FFMpeg.Play;
begin
end;

procedure TVideoPlayback_FFMpeg.Pause;
begin
  fVideoPaused := not fVideoPaused;
end;

procedure TVideoPlayback_FFMpeg.Stop;
begin
end;

procedure TVideoPlayback_FFMpeg.SetPosition(Time: real);
var
  SeekFlags: integer;
begin
  if (Time < 0) then
    Time := 0;

  // TODO: handle loop-times
  //Time := Time mod VideoDuration;

  // backward seeking might fail without AVSEEK_FLAG_BACKWARD
  SeekFlags := AVSEEK_FLAG_ANY;
  if (Time < VideoTime) then
    SeekFlags := SeekFlags or AVSEEK_FLAG_BACKWARD;

  VideoTime := Time;
  EOF := false;

  if (av_seek_frame(VideoFormatContext, VideoStreamIndex, Floor(Time/VideoTimeBase), SeekFlags) < 0) then
  begin
    Log.LogError('av_seek_frame() failed', 'TVideoPlayback_ffmpeg.SetPosition');
    Exit;
  end;

  avcodec_flush_buffers(VideoCodecContext);
end;

function  TVideoPlayback_FFMpeg.GetPosition: real;
begin
  // TODO: return video-position in seconds
  Result := VideoTime;
end;

initialization
  MediaManager.Add(TVideoPlayback_FFMpeg.Create);

end.
