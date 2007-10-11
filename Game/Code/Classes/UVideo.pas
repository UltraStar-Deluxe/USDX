unit UVideo;
{< #############################################################################
#                    FFmpeg support for UltraStar deluxe                       #
#                                                                              #
#    Created by b1indy                                                         #
#    based on 'An ffmpeg and SDL Tutorial' (http://www.dranger.com/ffmpeg/)    #
#                                                                              #
# http://www.mail-archive.com/fpc-pascal@lists.freepascal.org/msg09949.html    #
# http://www.nabble.com/file/p11795857/mpegpas01.zip                           #
#                                                                              #
############################################################################## }

{$define DebugDisplay}  // uncomment if u want to see the debug stuff
//{$define DebugFrames}
//{$define Info}
{}


interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}


uses SDL,
     UGraphicClasses,
     textgl,
     avcodec,
     avformat,
     avutil,
     math,
     OpenGL12,
     SysUtils,
     {$ifdef DebugDisplay}
     {$ifdef win32}
     dialogs,
     {$endif}
     {$ENDIF}
     UIni;

procedure Init;
procedure FFmpegOpenFile(FileName: pAnsiChar);
procedure FFmpegClose;
procedure FFmpegGetFrame(Time: Extended);
procedure FFmpegDrawGL(Screen: integer);
procedure FFmpegTogglePause;
procedure FFmpegSkip(Time: Single);

{
  @author(Jay Binks <jaybinks@gmail.com>)
  @created(2007-10-09)
  @lastmod(2007-10-09)

  @param(aFormatCtx is a PAVFormatContext returned from av_open_input_file )
  @param(aFirstVideoStream is an OUT value of type integer, this is the index of the video stream)
  @param(aFirstAudioStream is an OUT value of type integer, this is the index of the audio stream)
  @returns(@true on success, @false otherwise)

  translated from "Setting Up the Audio" section at
  http://www.dranger.com/ffmpeg/ffmpegtutorial_all.html
 }
function find_stream_ids( const aFormatCtx : PAVFormatContext; Out aFirstVideoStream, aFirstAudioStream : integer ): boolean;

var
  VideoOpened, VideoPaused: Boolean;
  VideoFormatContext: PAVFormatContext;
  VideoStreamIndex ,
  AudioStreamIndex : Integer;
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
  
  
  WantedAudioCodecContext,
  AudioCodecContext : PSDL_AudioSpec;
  aCodecCtx         : PAVCodecContext;
  
implementation

{$ifdef DebugDisplay}
{$ifNdef win32}

procedure showmessage( aMessage : String );
begin
  writeln( aMessage );
end;

{$endif}
{$ENDIF}

{ ------------------------------------------------------------------------------
asdf
------------------------------------------------------------------------------ }
procedure Init;
begin
  av_register_all;

  VideoOpened:=False;
  VideoPaused:=False;
  
  glGenTextures(1, PglUint(@VideoTex));
  SetLength(TexData,0);
end;

{
  @author(Jay Binks <jaybinks@gmail.com>)
  @created(2007-10-09)
  @lastmod(2007-10-09)

  @param(aFormatCtx is a PAVFormatContext returned from av_open_input_file )
  @param(aFirstVideoStream is an OUT value of type integer, this is the index of the video stream)
  @param(aFirstAudioStream is an OUT value of type integer, this is the index of the audio stream)
  @returns(@true on success, @false otherwise)

  translated from "Setting Up the Audio" section at
  http://www.dranger.com/ffmpeg/ffmpegtutorial_all.html
}
function find_stream_ids( const aFormatCtx : PAVFormatContext; Out aFirstVideoStream, aFirstAudioStream : integer ): boolean;
var
  i : integer;
  st : pAVStream;
begin
  // Find the first video stream
  aFirstAudioStream := -1;
  aFirstVideoStream := -1;

  writeln( ' aFormatCtx.nb_streams : ' + inttostr( aFormatCtx.nb_streams ) );
  writeln( ' length( aFormatCtx.streams ) : ' + inttostr( length(aFormatCtx.streams) ) );
  
  i := 0;
  while ( i < aFormatCtx.nb_streams ) do
//  while ( i < length(aFormatCtx.streams)-1 ) do
  begin
    writeln( ' aFormatCtx.streams[i] : ' + inttostr( i ) );
    st := aFormatCtx.streams[i];
    
    if(st.codec.codec_type = CODEC_TYPE_VIDEO ) AND
      (aFirstVideoStream < 0) THEN
    begin
      aFirstVideoStream := i;
    end;

    if ( st.codec.codec_type = CODEC_TYPE_AUDIO ) AND
       ( aFirstAudioStream < 0) THEN
    begin
      aFirstAudioStream := i;
    end;
    
    inc( i );
  end; // while
  
  result := (aFirstAudioStream > -1) OR
            (aFirstVideoStream > -1) ;  // Didn't find a video stream
end;

procedure FFmpegOpenFile(FileName: pAnsiChar);
var errnum, i, x,y: Integer;
  lStreamsCount : Integer;

begin
  VideoOpened    := False;
  VideoPaused    := False;
  VideoTimeBase  := 0;
  VideoTime      := 0;
  LastFrameTime  := 0;
  TimeDifference := 0;
  VideoFormatContext := 0;

  writeln( Filename );
  
  errnum         := av_open_input_file(VideoFormatContext, FileName, Nil, 0, Nil);
  writeln( 'Errnum : ' +inttostr( errnum ));
  if(errnum <> 0) then
  begin
{$ifdef DebugDisplay}
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
{$ENDIF}
    Exit;
  end
  else
  begin
    VideoStreamIndex := -1;
    AudioStreamIndex := -1;

    // Find which stream contains the video
    if( av_find_stream_info(VideoFormatContext) >= 0 ) then
    begin
      find_stream_ids( VideoFormatContext, VideoStreamIndex, AudioStreamIndex );
    
      writeln( 'VideoStreamIndex : ' + inttostr(VideoStreamIndex) );
      writeln( 'AudioStreamIndex : ' + inttostr(AudioStreamIndex) );
    end;
(*
    aCodecCtx := VideoFormatContext.streams[ AudioStreamIndex ].codec;

    WantedAudioCodecContext.freq     := aCodecCtx^.sample_rate;
    WantedAudioCodecContext.format   := AUDIO_S16SYS;
    WantedAudioCodecContext.channels := aCodecCtx^.channels;
    WantedAudioCodecContext.silence  := 0;
    WantedAudioCodecContext.samples  := 1024;//SDL_AUDIO_BUFFER_SIZE;
//    WantedAudioCodecContext.callback := audio_callback;
    WantedAudioCodecContext.userdata := aCodecCtx;



    if(SDL_OpenAudio(WantedAudioCodecContext, AudioCodecContext) < 0) then
    begin
      writeln( 'Could not do SDL_OpenAudio' );
      exit;
    end;
*)
    
    if(VideoStreamIndex >= 0) then
    begin
      VideoCodecContext:=VideoFormatContext^.streams[VideoStreamIndex]^.codec;
      VideoCodec:=avcodec_find_decoder(VideoCodecContext^.codec_id);
    end
    else
    begin
{$ifdef DebugDisplay}
      showmessage('found no video stream');
{$ENDIF}
      av_close_input_file(VideoFormatContext);
      Exit;
    end;

    
    if(VideoCodec<>Nil) then
    begin
      errnum:=avcodec_open(VideoCodecContext, VideoCodec);
    end else begin
{$ifdef DebugDisplay}
      showmessage('no matching codec found');
{$ENDIF}
      avcodec_close(VideoCodecContext);
      av_close_input_file(VideoFormatContext);
      Exit;
    end;
    if(errnum >=0) then
    begin
{$ifdef DebugDisplay}
       showmessage('Found a matching Codec: '+ VideoCodecContext^.Codec.Name +#13#10#13#10+
        '  Width = '+inttostr(VideoCodecContext^.width)+ ', Height='+inttostr(VideoCodecContext^.height)+#13#10+
        '  Aspect    : '+inttostr(VideoCodecContext^.sample_aspect_ratio.num)+'/'+inttostr(VideoCodecContext^.sample_aspect_ratio.den)+#13#10+
        '  Framerate : '+inttostr(VideoCodecContext^.time_base.num)+'/'+inttostr(VideoCodecContext^.time_base.den));
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
{$ifdef DebugDisplay}
      showmessage('failed to allocate video buffer');
{$endif}
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
  if VideoSkipTime > 0 then begin
    av_seek_frame(VideoFormatContext,-1,Floor((VideoSkipTime)*1500000),0);
    VideoTime:=VideoSkipTime;
  end;
end;

procedure FFmpegGetFrame(Time: Extended);
var
  FrameFinished: Integer;
  AVPacket: TAVPacket;
  errnum, x, y: Integer;
  FrameDataPtr: PByteArray;
  linesize: integer;
  myTime: Extended;
  DropFrame: Boolean;
  droppedFrames: Integer;
const
  FRAMEDROPCOUNT=3;
begin
  if not VideoOpened then Exit;

  if VideoPaused then Exit;
  
  myTime:=Time+VideoSkipTime;
  TimeDifference:=myTime-VideoTime;
  DropFrame:=False;

{$IFDEF DebugDisplay}
  showmessage('Time:      '+inttostr(floor(Time*1000))+#13#10+
    'VideoTime: '+inttostr(floor(VideoTime*1000))+#13#10+
    'TimeBase:  '+inttostr(floor(VideoTimeBase*1000))+#13#10+
    'TimeDiff:  '+inttostr(floor(TimeDifference*1000)));
{$endif}

  if (VideoTime <> 0) and (TimeDifference <= VideoTimeBase) then
  begin
{$ifdef DebugFrames}
    // frame delay debug display
    GoldenRec.Spawn(200,15,1,16,0,-1,ColoredStar,$00ff00);
{$endif}

{$IFDEF DebugDisplay}
    showmessage('not getting new frame'+#13#10+
    'Time:      '+inttostr(floor(Time*1000))+#13#10+
    'VideoTime: '+inttostr(floor(VideoTime*1000))+#13#10+
    'TimeBase:  '+inttostr(floor(VideoTimeBase*1000))+#13#10+
    'TimeDiff:  '+inttostr(floor(TimeDifference*1000)));
{$endif}

    Exit;// we don't need a new frame now
  end;
  
  VideoTime:=VideoTime+VideoTimeBase;
  TimeDifference:=myTime-VideoTime;
  if TimeDifference >= (FRAMEDROPCOUNT-1)*VideoTimeBase then  // skip frames
  begin
{$ifdef DebugFrames}
    //frame drop debug display
    GoldenRec.Spawn(200,55,1,16,0,-1,ColoredStar,$ff0000);
{$endif}

{$IFDEF DebugDisplay}
    showmessage('skipping frames'+#13#10+
    'TimeBase:  '+inttostr(floor(VideoTimeBase*1000))+#13#10+
    'TimeDiff:  '+inttostr(floor(TimeDifference*1000))+#13#10+
    'Time2Skip: '+inttostr(floor((Time-LastFrameTime)*1000)));
{$endif}
//    av_seek_frame(VideoFormatContext,VideoStreamIndex,Floor(Time*VideoTimeBase),0);
{    av_seek_frame(VideoFormatContext,-1,Floor((myTime+VideoTimeBase)*1500000),0);
    VideoTime:=floor(myTime/VideoTimeBase)*VideoTimeBase;}
    VideoTime:=VideoTime+FRAMEDROPCOUNT*VideoTimeBase;
    DropFrame:=True;
  end;

//  av_init_packet(@AVPacket);
  av_init_packet( AVPacket ); // JB-ffmpeg
  
  FrameFinished:=0;
  // read packets until we have a finished frame (or there are no more packets)
//  while (FrameFinished=0) and (av_read_frame(VideoFormatContext, @AVPacket)>=0) do
  while (FrameFinished=0) and (av_read_frame(VideoFormatContext, AVPacket)>=0) do     // JB-ffmpeg
  begin
    // if we got a packet from the video stream, then decode it
    if (AVPacket.stream_index=VideoStreamIndex) then
//      errnum:=avcodec_decode_video(VideoCodecContext, AVFrame,  @frameFinished , AVPacket.data, AVPacket.size);
      errnum := avcodec_decode_video(VideoCodecContext, AVFrame,  frameFinished , AVPacket.data, AVPacket.size); // JB-ffmpeg

      
    // release internal packet structure created by av_read_frame
//      av_free_packet(PAVPacket(@AVPacket));
      av_free_packet( AVPacket );  // JB-ffmpeg
  end;
  
  if DropFrame then
    for droppedFrames:=1 to FRAMEDROPCOUNT do begin
      FrameFinished:=0;
      // read packets until we have a finished frame (or there are no more packets)
//      while (FrameFinished=0) and (av_read_frame(VideoFormatContext, @AVPacket)>=0) do
      while (FrameFinished=0) and (av_read_frame(VideoFormatContext, AVPacket)>=0) do  // JB-ffmpeg
      begin
        // if we got a packet from the video stream, then decode it
        if (AVPacket.stream_index=VideoStreamIndex) then
//          errnum:=avcodec_decode_video(VideoCodecContext, AVFrame, @frameFinished, AVPacket.data, AVPacket.size);
      errnum:=avcodec_decode_video(VideoCodecContext, AVFrame,  frameFinished , AVPacket.data, AVPacket.size); // JB-ffmpeg


        // release internal packet structure created by av_read_frame
//          av_free_packet(PAVPacket(@AVPacket)); // JB-ffmpeg
          av_free_packet( AVPacket );
      end;
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

  if errnum >=0 then
  begin
    // copy RGB pixeldata to our TextureBuffer
    // (line by line)

    FrameDataPtr := pointer( AVFrameRGB^.data[0] );
    linesize     := AVFrameRGB^.linesize[0];
    for y:=0 to TexY-1 do
    begin
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
