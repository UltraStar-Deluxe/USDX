{############################################################################
#                   FFmpeg support for UltraStar deluxe                     #
#                                                                           #
#   Created by b1indy                                                       #
#   based on 'An ffmpeg and SDL Tutorial' (http://www.dranger.com/ffmpeg/)  #
#############################################################################}

//{$define DebugDisplay}  // uncomment if u want to see the debug stuff
//{$define DebugFrames}
{$define Info}


unit Uffmpeg;

interface
uses SDL, UGraphicClasses, textgl, avcodec, avformat, avutil, math, OpenGL12, SysUtils, UIni, dialogs;

procedure Init;
procedure FFmpegOpenFile(FileName: pAnsiChar);
procedure FFmpegClose;
procedure FFmpegGetFrame(Time: Extended);
procedure FFmpegDrawGL(Screen: integer);
procedure FFmpegTogglePause;
procedure FFmpegSkip(Time: Single);

var
  VideoOpened, VideoPaused: Boolean;
  VideoFormatContext: PAVFormatContext;
  VideoStreamIndex: Integer;
  VideoCodecContext: PAVCodecContext;
  VideoCodec: PAVCodec;
  AVFrame: PAVFrame;
  AVFrameRGB: PAVFrame;
  myBuffer: pByte;
  VideoTex: glUint;
  TexX, TexY, dataX, dataY: Cardinal;
  TexData: array of Byte;
  ScaledVideoWidth, ScaledVideoHeight: Real;
  VideoAspect: Real;
  VideoTextureU, VideoTextureV: Real;
  VideoTimeBase, VideoTime, LastFrameTime, TimeDifference: Extended;
  VideoSkipTime: Single;

implementation

procedure Init;
begin
  av_register_all;
  VideoOpened:=False;
  VideoPaused:=False;
  glGenTextures(1, PglUint(@VideoTex));
  SetLength(TexData,0);
end;

procedure FFmpegOpenFile(FileName: pAnsiChar);
var errnum, i, x,y: Integer;
begin
  VideoOpened:=False;
  VideoPaused:=False;
  VideoTimeBase:=0;
  VideoTime:=0;
  LastFrameTime:=0;
  TimeDifference:=0;
  errnum:=av_open_input_file(VideoFormatContext, FileName, Nil, 0, Nil);
  if(errnum <> 0)
  then begin
    case errnum of
      AVERROR_UNKNOWN: showmessage('failed to open file '+Filename+#13#10+'AVERROR_UNKNOWN');
      AVERROR_IO: showmessage('failed to open file '+Filename+#13#10+'AVERROR_IO');
      AVERROR_NUMEXPECTED: showmessage('failed to open file '+Filename+#13#10+'AVERROR_NUMEXPECTED');
      AVERROR_INVALIDDATA: showmessage('failed to open file '+Filename+#13#10+'AVERROR_INVALIDDATA');
      AVERROR_NOMEM: showmessage('failed to open file '+Filename+#13#10+'AVERROR_NOMEM');
      AVERROR_NOFMT: showmessage('failed to open file '+Filename+#13#10+'AVERROR_NOFMT');
      AVERROR_NOTSUPP: showmessage('failed to open file '+Filename+#13#10+'AVERROR_NOTSUPP');
    else showmessage('failed to open file '+Filename+#13#10+'Error number: '+inttostr(Errnum));
    end;
    Exit;
  end
  else begin
    VideoStreamIndex:=-1;
    if(av_find_stream_info(VideoFormatContext)>=0) then
    begin
      for i:=0 to VideoFormatContext^.nb_streams-1 do
        if(VideoFormatContext^.streams[i]^.codec^.codec_type=CODEC_TYPE_VIDEO) then begin
          VideoStreamIndex:=i;
        end else
    end;
    if(VideoStreamIndex >= 0) then
    begin
      VideoCodecContext:=VideoFormatContext^.streams[VideoStreamIndex]^.codec;
      VideoCodec:=avcodec_find_decoder(VideoCodecContext^.codec_id);
    end else begin
      showmessage('found no video stream');
      av_close_input_file(VideoFormatContext);
      Exit;
    end;
    if(VideoCodec<>Nil) then
    begin
      errnum:=avcodec_open(VideoCodecContext, VideoCodec);
    end else begin
      showmessage('no matching codec found');
      avcodec_close(VideoCodecContext);
      av_close_input_file(VideoFormatContext);
      Exit;
    end;
    if(errnum >=0) then
    begin
{$ifdef DebugDisplay}
       showmessage('Found a matching Codec:'+#13#10#13#10+
        'Width='+inttostr(VideoCodecContext^.width)+
        ', Height='+inttostr(VideoCodecContext^.height)+#13#10+
        'Aspect: '+inttostr(VideoCodecContext^.sample_aspect_ratio.num)+'/'+inttostr(VideoCodecContext^.sample_aspect_ratio.den)+#13#10+
        'Framerate: '+inttostr(VideoCodecContext^.time_base.num)+'/'+inttostr(VideoCodecContext^.time_base.den));
{$endif}
      // allocate space for decoded frame and rgb frame
      AVFrame:=avcodec_alloc_frame;
      AVFrameRGB:=avcodec_alloc_frame;
    end;
    myBuffer:=Nil;
    if(AVFrame <> Nil) and (AVFrameRGB <> Nil) then
    begin
      myBuffer:=av_malloc(avpicture_get_size(PIX_FMT_RGB24, VideoCodecContext^.width,
                            VideoCodecContext^.height));
    end;
    if myBuffer <> Nil then errnum:=avpicture_fill(PAVPicture(AVFrameRGB), myBuffer, PIX_FMT_RGB24,
                VideoCodecContext^.width, VideoCodecContext^.height)
    else begin
      showmessage('failed to allocate video buffer');
      av_free(AVFrameRGB);
      av_free(AVFrame);
      avcodec_close(VideoCodecContext);
      av_close_input_file(VideoFormatContext);
      Exit;
    end;
    if errnum >=0 then
    begin
      VideoOpened:=True;

      TexX := VideoCodecContext^.width;
      TexY := VideoCodecContext^.height;
      dataX := Round(Power(2, Ceil(Log2(TexX))));
      dataY := Round(Power(2, Ceil(Log2(TexY))));
      SetLength(TexData,dataX*dataY*3);
      // calculate some information for video display
      VideoAspect:=VideoCodecContext^.sample_aspect_ratio.num/VideoCodecContext^.sample_aspect_ratio.den;
      if (VideoAspect = 0) then
        VideoAspect:=VideoCodecContext^.width/VideoCodecContext^.height
      else
        VideoAspect:=VideoAspect*VideoCodecContext^.width/VideoCodecContext^.height;
      if VideoAspect >= 4/3 then
      begin
        ScaledVideoWidth:=800.0;
        ScaledVideoHeight:=800.0/VideoAspect;
      end else
      begin
        ScaledVideoHeight:=600.0;
        ScaledVideoWidth:=600.0*VideoAspect;
      end;
      VideoTimeBase:=VideoCodecContext^.time_base.num/VideoCodecContext^.time_base.den;
      // hack to get reasonable timebase for divx
{$ifdef DebugDisplay}
      showmessage('framerate: '+inttostr(floor(1/videotimebase))+'fps');
{$endif}
      if VideoTimeBase < 0.02 then // 0.02 <-> 50 fps
      begin
        VideoTimeBase:=VideoCodecContext^.time_base.den/VideoCodecContext^.time_base.num;
        while VideoTimeBase > 50 do VideoTimeBase:=VideoTimeBase/10;
        VideoTimeBase:=1/VideoTimeBase;
      end;
{$ifdef DebugDisplay}
      showmessage('corrected framerate: '+inttostr(floor(1/videotimebase))+'fps');
      if ((VideoAspect*VideoCodecContext^.width*VideoCodecContext^.height)>200000) then
        showmessage('you are trying to play a rather large video'+#13#10+
                    'be prepared to experience some timing problems');
{$endif}
    end;
  end;
end;

procedure FFmpegClose;
begin
  if VideoOpened then begin
    av_free(myBuffer);
    av_free(AVFrameRGB);
    av_free(AVFrame);
    avcodec_close(VideoCodecContext);
    av_close_input_file(VideoFormatContext);
    SetLength(TexData,0);
    VideoOpened:=False;
  end;
end;

procedure FFmpegTogglePause;
begin
  if VideoPaused then VideoPaused:=False
  else VideoPaused:=True;
end;

procedure FFmpegSkip(Time: Single);
begin
  VideoSkiptime:=Time;
end;

procedure FFmpegGetFrame(Time: Extended);
var
  FrameFinished: Integer;
  AVPacket: TAVPacket;
  errnum, x, y: Integer;
  FrameDataPtr: PByteArray;
  linesize: integer;
  myTime: Extended;
begin
  if not VideoOpened then Exit;
  if VideoPaused then Exit;
  myTime:=Time+VideoSkipTime;
  TimeDifference:=myTime-VideoTime;
{  showmessage('Time:      '+inttostr(floor(Time*1000))+#13#10+
    'VideoTime: '+inttostr(floor(VideoTime*1000))+#13#10+
    'TimeBase:  '+inttostr(floor(VideoTimeBase*1000))+#13#10+
    'TimeDiff:  '+inttostr(floor(TimeDifference*1000)));
}
  if (VideoTime <> 0) and (TimeDifference <= VideoTimeBase) then begin
{$ifdef DebugFrames}
    // frame delay debug display
    GoldenRec.Spawn(200,15,1,16,0,-1,ColoredStar,$00ff00);
{$endif}
{    showmessage('not getting new frame'+#13#10+
    'Time:      '+inttostr(floor(Time*1000))+#13#10+
    'VideoTime: '+inttostr(floor(VideoTime*1000))+#13#10+
    'TimeBase:  '+inttostr(floor(VideoTimeBase*1000))+#13#10+
    'TimeDiff:  '+inttostr(floor(TimeDifference*1000)));
}
    Exit;// we don't need a new frame now
  end;
  VideoTime:=VideoTime+VideoTimeBase;
  TimeDifference:=myTime-VideoTime;
  if TimeDifference >= 3*VideoTimeBase then begin // skip frames
{$ifdef DebugFrames}
    //frame drop debug display
    GoldenRec.Spawn(200,55,1,16,0,-1,ColoredStar,$ff0000);
{$endif}
//    showmessage('skipping frames'+#13#10+
//    'TimeBase:  '+inttostr(floor(VideoTimeBase*1000))+#13#10+
//    'TimeDiff:  '+inttostr(floor(TimeDifference*1000))+#13#10+
//    'Time2Skip: '+inttostr(floor((Time-LastFrameTime)*1000)));
//    av_seek_frame(VideoFormatContext,VideoStreamIndex,Floor(Time*VideoTimeBase),0);
    av_seek_frame(VideoFormatContext,-1,Floor((myTime)*1100000),0);
    VideoTime:=floor(myTime/VideoTimeBase)*VideoTimeBase;
  end;

  FrameFinished:=0;
  // read packets until we have a finished frame (or there are no more packets)
  while (FrameFinished=0) and (av_read_frame(VideoFormatContext, @AVPacket)>=0) do
  begin
    // if we got a packet from the video stream, then decode it
    if (AVPacket.stream_index=VideoStreamIndex) then
      errnum:=avcodec_decode_video(VideoCodecContext, AVFrame, @frameFinished,
                                   AVPacket.data, AVPacket.size);
    // release internal packet structure created by av_read_frame
    av_free_packet(PAVPacket(@AVPacket));
  end;
  // if we did not get an new frame, there's nothing more to do
  if Framefinished=0 then begin
    GoldenRec.Spawn(220,15,1,16,0,-1,ColoredStar,$0000ff);
    Exit;
  end;
  // otherwise we convert the pixeldata from YUV to RGB
  errnum:=img_convert(PAVPicture(AVFrameRGB), PIX_FMT_RGB24,
            PAVPicture(AVFrame), VideoCodecContext^.pix_fmt,
			      VideoCodecContext^.width, VideoCodecContext^.height);
//errnum:=1;
  if errnum >=0 then begin
    // copy RGB pixeldata to our TextureBuffer
    // (line by line)
    FrameDataPtr:=AVFrameRGB^.data[0];
    linesize:=AVFrameRGB^.linesize[0];
    for y:=0 to TexY-1 do begin
      System.Move(FrameDataPtr[y*linesize],TexData[3*y*dataX],linesize);
    end;

    // generate opengl texture out of whatever we got
    glBindTexture(GL_TEXTURE_2D, VideoTex);
    glTexImage2D(GL_TEXTURE_2D, 0, 3, dataX, dataY, 0, GL_RGB, GL_UNSIGNED_BYTE, TexData);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
{$ifdef DebugFrames}
    //frame decode debug display
    GoldenRec.Spawn(200,35,1,16,0,-1,ColoredStar,$ffff00);
{$endif}

  end;
end;

procedure FFmpegDrawGL(Screen: integer);
begin
  // have a nice black background to draw on (even if there were errors opening the vid)
  if Screen=1 then begin
    glClearColor(0,0,0,0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  end;
  // exit if there's nothing to draw
  if not VideoOpened then Exit;

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glColor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, VideoTex);
  glbegin(gl_quads);
    glTexCoord2f(         0,          0); glVertex2f(400-ScaledVideoWidth/2, 300-ScaledVideoHeight/2);
    glTexCoord2f(         0, TexY/dataY); glVertex2f(400-ScaledVideoWidth/2, 300+ScaledVideoHeight/2);
    glTexCoord2f(TexX/dataX, TexY/dataY); glVertex2f(400+ScaledVideoWidth/2, 300+ScaledVideoHeight/2);
    glTexCoord2f(TexX/dataX,          0); glVertex2f(400+ScaledVideoWidth/2, 300-ScaledVideoHeight/2);
  glEnd;
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);

{$ifdef Info}
  if VideoSkipTime+VideoTime+VideoTimeBase < 0 then begin
    glColor4f(0.7, 1, 0.3, 1);
    SetFontStyle (1);
    SetFontItalic(False);
    SetFontSize(9);
    SetFontPos (300, 0);
    glPrint('Delay due to negative VideoGap');
    glColor4f(1, 1, 1, 1);
  end;
{$endif}

{$ifdef DebugFrames}
    glColor4f(0, 0, 0, 0.2);
    glbegin(gl_quads);
      glVertex2f(0, 0);
      glVertex2f(0, 70);
      glVertex2f(250, 70);
      glVertex2f(250, 0);
    glEnd;

    glColor4f(1,1,1,1);
    SetFontStyle (1);
    SetFontItalic(False);
    SetFontSize(9);
    SetFontPos (5, 0);
    glPrint('delaying frame');
    SetFontPos (5, 20);
    glPrint('fetching frame');
    SetFontPos (5, 40);
    glPrint('dropping frame');
{$endif}
end;

end.
