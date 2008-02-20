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

//{$define DebugDisplay}  // uncomment if u want to see the debug stuff
//{$define DebugFrames}
//{$define Info}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

(*

  look into
  av_read_play

*)

implementation

uses SDL,
     UGraphicClasses,
     textgl,
     avcodec,
     avformat,
     avutil,
     {$IFDEF UseSWScale}
     swscale,
     {$ENDIF}
     math,
     OpenGL12,
     SysUtils,
     {$ifdef DebugDisplay}
     {$ifdef win32}
     dialogs,
     {$endif}
     {$ENDIF}
     (* FIXME
     {$ifdef UseFFMpegAudio}
     UAudioDecoder_FFMpeg,
     {$endif}
     *)
     UIni,
     UMusic,
     UGraphic;


var
  singleton_VideoFFMpeg : IVideoPlayback;

type
    TVideoPlayback_ffmpeg = class( TInterfacedObject, IVideoPlayback )
    private
      fVideoOpened   ,
      fVideoPaused   : Boolean;

      fVideoTex      : glUint;
      fVideoSkipTime : Single;

      VideoFormatContext: PAVFormatContext;

      VideoStreamIndex ,
      AudioStreamIndex : Integer;
      VideoCodecContext: PAVCodecContext;
      VideoCodec: PAVCodec;
      AVFrame: PAVFrame;
      AVFrameRGB: PAVFrame;
      myBuffer: pByte;

      {$IFDEF UseSWScale}
      SoftwareScaleContext: PSwsContext;
      {$ENDIF}

      TexX, TexY, dataX, dataY: Cardinal;

      ScaledVideoWidth, ScaledVideoHeight: Real;
      VideoAspect: Real;
      VideoTextureU, VideoTextureV: Real;
      VideoTimeBase, VideoTime, LastFrameTime, TimeDifference, flooptime: Extended;


      WantedAudioCodecContext,
      AudioCodecContext : PSDL_AudioSpec;
      aCodecCtx         : PAVCodecContext;

      function find_stream_ids( const aFormatCtx : PAVFormatContext; Out aFirstVideoStream, aFirstAudioStream : integer ): boolean;

    public
      constructor create();
      function    GetName: String;
      procedure   init();

      function    Open( aFileName : string): boolean; // true if succeed
      procedure   Close;

      procedure   Play;
      procedure   Pause;
      procedure   Stop;

      procedure   SetPosition(Time: real);
      function    GetPosition: real;

      procedure   GetFrame(Time: Extended);
      procedure   DrawGL(Screen: integer);

    end;

    const
  SDL_AUDIO_BUFFER_SIZE = 1024;

{$ifdef DebugDisplay}
//{$ifNdef win32}

procedure showmessage( aMessage : String );
begin
  writeln( aMessage );
end;

//{$endif}
{$ENDIF}

{ ------------------------------------------------------------------------------
asdf
------------------------------------------------------------------------------ }

function  TVideoPlayback_ffmpeg.GetName: String;
begin
  result := 'FFMpeg';
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
function TVideoPlayback_ffmpeg.find_stream_ids( const aFormatCtx : PAVFormatContext; Out aFirstVideoStream, aFirstAudioStream : integer ): boolean;
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




procedure TVideoPlayback_ffmpeg.GetFrame(Time: Extended);
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
  if not fVideoOpened then Exit;

  if fVideoPaused then Exit;

  myTime         := ( Time - flooptime )   + fVideoSkipTime;
  TimeDifference := myTime - VideoTime;
  DropFrame      := False;

{$IFDEF DebugDisplay}
  showmessage('Time:      '+inttostr(floor(Time*1000))+#13#10+
              'VideoTime: '+inttostr(floor(VideoTime*1000))+#13#10+
              'TimeBase:  '+inttostr(floor(VideoTimeBase*1000))+#13#10+
              'TimeDiff:  '+inttostr(floor(TimeDifference*1000)));
{$endif}

  if (VideoTime <> 0) and (TimeDifference+flooptime <= VideoTimeBase) then
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
    VideoTime:=VideoTime+FRAMEDROPCOUNT*VideoTimeBase;
    DropFrame:=True;
  end;

  AVPacket.data := nil;
  av_init_packet( AVPacket ); // JB-ffmpeg

  FrameFinished:=0;
  // read packets until we have a finished frame (or there are no more packets)
  while ( FrameFinished = 0 ) do
  begin
    if ( av_read_frame(VideoFormatContext, AVPacket) < 0 ) then
    begin
      // Record the Time we looped, this is used to keep the loops, in time. otherwise they speed
      flooptime      := time;

      // Dont use SetPosition() it dosnt let us go back to frame 0... can we / should we fix this ??
      fVideoSkipTime := 0;
      VideoTime      := 0;

      // Free the packet we just got from av_read_frame
      av_free_packet( @AVPacket );

      // Seek to frame 0 in the video stream
      av_seek_frame(VideoFormatContext,VideoStreamIndex,0,AVSEEK_FLAG_ANY);
      break;
    end;


    // if we got a packet from the video stream, then decode it
    if (AVPacket.stream_index=VideoStreamIndex) then
    begin
      errnum := avcodec_decode_video(VideoCodecContext, AVFrame,  frameFinished , AVPacket.data, AVPacket.size); // JB-ffmpeg
    (* FIXME
    {$ifdef UseFFMpegAudio}
    end
    else
    if (AVPacket.stream_index = AudioStreamIndex ) then
    begin
      writeln('Encue Audio packet');
      audioq.put(AVPacket);
    {$endif}
    *)
    end;

      try
//        if AVPacket.data <> nil then
          av_free_packet( @AVPacket );  // JB-ffmpeg
      except
        // TODO : JB_FFMpeg ... why does this now AV sometimes ( or always !! )
      end;

  end;

  if DropFrame then
    for droppedFrames:=1 to FRAMEDROPCOUNT do begin
      FrameFinished:=0;
      // read packets until we have a finished frame (or there are no more packets)
      while (FrameFinished=0) do
      begin
        if (av_read_frame(VideoFormatContext, AVPacket)<0) then
          Break;
        // if we got a packet from the video stream, then decode it
        if (AVPacket.stream_index=VideoStreamIndex) then
      errnum:=avcodec_decode_video(VideoCodecContext, AVFrame,  frameFinished , AVPacket.data, AVPacket.size); // JB-ffmpeg

        // release internal packet structure created by av_read_frame
        try
//          if AVPacket.data <> nil then
            av_free_packet( @AVPacket );  // JB-ffmpeg
        except
          // TODO : JB_FFMpeg ... why does this now AV sometimes ( or always !! )
        end;
      end;
    end;

  // if we did not get an new frame, there's nothing more to do
  if Framefinished=0 then begin
    Exit;
  end;

  // otherwise we convert the pixeldata from YUV to RGB
  {$IFDEF UseSWScale}
  errnum:=sws_scale(SoftwareScaleContext,@(AVFrame.data),@(AVFrame.linesize),
          0,VideoCodecContext^.Height,
          @(AVFrameRGB.data),@(AVFrameRGB.linesize));
  {$ELSE}
  errnum:=img_convert(PAVPicture(AVFrameRGB), PIX_FMT_RGB24,
            PAVPicture(AVFrame), VideoCodecContext^.pix_fmt,
			      VideoCodecContext^.width, VideoCodecContext^.height);
  {$ENDIF}

  if errnum >=0 then
  begin
  glBindTexture(GL_TEXTURE_2D, fVideoTex);
  glTexImage2D(GL_TEXTURE_2D, 0, 3, dataX, dataY, 0, GL_RGB, GL_UNSIGNED_BYTE, AVFrameRGB^.data[0]);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
{$ifdef DebugFrames}
    //frame decode debug display
    GoldenRec.Spawn(200,35,1,16,0,-1,ColoredStar,$ffff00);
{$endif}
  end;
end;

procedure TVideoPlayback_ffmpeg.DrawGL(Screen: integer);
begin
  // have a nice black background to draw on (even if there were errors opening the vid)
  if Screen=1 then
  begin
    glClearColor(0,0,0,0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  end;
  // exit if there's nothing to draw
  if not fVideoOpened then Exit;

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glColor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, fVideoTex);
  glbegin(gl_quads);
    glTexCoord2f(         0,          0); glVertex2f(400-ScaledVideoWidth/2, 300-ScaledVideoHeight/2);
    glTexCoord2f(         0, TexY/dataY); glVertex2f(400-ScaledVideoWidth/2, 300+ScaledVideoHeight/2);
    glTexCoord2f(TexX/dataX, TexY/dataY); glVertex2f(400+ScaledVideoWidth/2, 300+ScaledVideoHeight/2);
    glTexCoord2f(TexX/dataX,          0); glVertex2f(400+ScaledVideoWidth/2, 300-ScaledVideoHeight/2);
  glEnd;
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);

{$ifdef Info}
  if VideoSkipTime+VideoTime+VideoTimeBase < 0 then
  begin
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

constructor TVideoPlayback_ffmpeg.create();
begin
  av_register_all;

  fVideoOpened := False;
  fVideoPaused := False;
end;

procedure TVideoPlayback_ffmpeg.init();
begin
  glGenTextures(1, PglUint(@fVideoTex));
end;


function TVideoPlayback_ffmpeg.Open( aFileName : string): boolean; // true if succeed
var
  errnum, i, x,y: Integer;
  lStreamsCount : Integer;

  wanted_spec   ,
  spec          : TSDL_AudioSpec;
  aCodec        : pAVCodec;

  sws_dst_w, sws_dst_h: Integer;

begin
  fVideoOpened       := False;
  fVideoPaused       := False;
  VideoTimeBase      := 0;
  VideoTime          := 0;
  LastFrameTime      := 0;
  TimeDifference     := 0;
  VideoFormatContext := nil;

//  writeln( aFileName );

  errnum         := av_open_input_file(VideoFormatContext, pchar( aFileName ), Nil, 0, Nil);
//  writeln( 'Errnum : ' +inttostr( errnum ));
  if(errnum <> 0) then
  begin
{$ifdef DebugDisplay}
    case errnum of
      AVERROR_UNKNOWN: showmessage('failed to open file '+aFileName+#13#10+'AVERROR_UNKNOWN');
      AVERROR_IO: showmessage('failed to open file '+aFileName+#13#10+'AVERROR_IO');
      AVERROR_NUMEXPECTED: showmessage('failed to open file '+aFileName+#13#10+'AVERROR_NUMEXPECTED');
      AVERROR_INVALIDDATA: showmessage('failed to open file '+aFileName+#13#10+'AVERROR_INVALIDDATA');
      AVERROR_NOMEM: showmessage('failed to open file '+aFileName+#13#10+'AVERROR_NOMEM');
      AVERROR_NOFMT: showmessage('failed to open file '+aFileName+#13#10+'AVERROR_NOFMT');
      AVERROR_NOTSUPP: showmessage('failed to open file '+aFileName+#13#10+'AVERROR_NOTSUPP');
    else showmessage('failed to open file '+aFileName+#13#10+'Error number: '+inttostr(Errnum));
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
    // FIXME: AudioStreamIndex is -1 if video has no sound -> memory access error
    // Just a temporary workaround for now
    aCodecCtx := nil;
    if( AudioStreamIndex >= 0) then
      aCodecCtx := VideoFormatContext.streams[ AudioStreamIndex ].codec;

    (* FIXME
    {$ifdef UseFFMpegAudio}
  // This is the audio ffmpeg audio support Jay is working on.
    if aCodecCtx <> nil then
    begin
      wanted_spec.freq     := aCodecCtx.sample_rate;
      wanted_spec.format   := AUDIO_S16SYS;
      wanted_spec.channels := aCodecCtx.channels;
      wanted_spec.silence  := 0;
      wanted_spec.samples  := SDL_AUDIO_BUFFER_SIZE;
      wanted_spec.callback := UAudio_FFMpeg.audio_callback;
      wanted_spec.userdata := aCodecCtx;


      if (SDL_OpenAudio(@wanted_spec, @spec) < 0) then
      begin
        writeln('SDL_OpenAudio: '+SDL_GetError());
        exit;
      end;

      writeln( 'SDL opened audio device' );

      aCodec := avcodec_find_decoder(aCodecCtx.codec_id);
      if (aCodec = nil) then
      begin
        writeln('Unsupported codec!');
        exit;
      end;

      avcodec_open(aCodecCtx, aCodec);

      writeln( 'Opened the codec' );

      packet_queue_init( audioq );
      SDL_PauseAudio(0);

      writeln( 'SDL_PauseAudio' );


    end;
    {$endif}
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
      if (VideoCodecContext^.width >1024) or (VideoCodecContext^.height >1024) then
      begin
        ScreenPopupError.ShowPopup('Video dimensions\nmust not exceed\n1024 pixels\n\nvideo disabled'); //show error message
        avcodec_close(VideoCodecContext);
        av_close_input_file(VideoFormatContext);
        Exit;
      end;
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

    dataX := Round(Power(2, Ceil(Log2(VideoCodecContext^.width))));
    dataY := Round(Power(2, Ceil(Log2(VideoCodecContext^.height))));
    myBuffer:=Nil;
    if(AVFrame <> Nil) and (AVFrameRGB <> Nil) then
    begin
      myBuffer:=av_malloc(avpicture_get_size(PIX_FMT_RGB24, dataX, dataY));
    end;
    if myBuffer <> Nil then errnum:=avpicture_fill(PAVPicture(AVFrameRGB), myBuffer, PIX_FMT_RGB24,
                dataX, dataY)
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

    {$IFDEF UseSWScale}
    SoftwareScaleContext:=sws_getContext(VideoCodecContext^.width,VideoCodecContext^.height,integer(VideoCodecContext^.pix_fmt),
                                         dataX, dataY, integer(PIX_FMT_RGB24),
                                         SWS_FAST_BILINEAR, nil, nil, nil);
    if SoftwareScaleContext <> Nil then
        writeln('got swscale context')
    else begin
      writeln('ERROR: didn´t get swscale context');
      av_free(AVFrameRGB);
      av_free(AVFrame);
      avcodec_close(VideoCodecContext);
      av_close_input_file(VideoFormatContext);
      Exit;
    end;
    {$ENDIF}

    // this is the errnum from avpicture_fill
    if errnum >=0 then
    begin
      fVideoOpened:=True;

      TexX := VideoCodecContext^.width;
      TexY := VideoCodecContext^.height;
      dataX := Round(Power(2, Ceil(Log2(TexX))));
      dataY := Round(Power(2, Ceil(Log2(TexY))));
      // calculate some information for video display
      VideoAspect:=VideoCodecContext^.sample_aspect_ratio.num/VideoCodecContext^.sample_aspect_ratio.den;
      if (VideoAspect = 0) then
        VideoAspect:=VideoCodecContext^.width/VideoCodecContext^.height
      else
        VideoAspect:=VideoAspect*VideoCodecContext^.width/VideoCodecContext^.height;
        ScaledVideoWidth:=800.0;
        ScaledVideoHeight:=800.0/VideoAspect;
      VideoTimeBase:=VideoFormatContext^.streams[VideoStreamIndex]^.r_frame_rate.den/VideoFormatContext^.streams[VideoStreamIndex]^.r_frame_rate.num;
{$ifdef DebugDisplay}
      showmessage('framerate: '+inttostr(floor(1/videotimebase))+'fps');
{$endif}
      // hack to get reasonable timebase (for divx and others)
      if VideoTimeBase < 0.02 then // 0.02 <-> 50 fps
      begin
        VideoTimeBase:=VideoFormatContext^.streams[VideoStreamIndex]^.r_frame_rate.num/VideoFormatContext^.streams[VideoStreamIndex]^.r_frame_rate.den;
        while VideoTimeBase > 50 do VideoTimeBase:=VideoTimeBase/10;
        VideoTimeBase:=1/VideoTimeBase;
      end;
    end;
  end;
end;

procedure TVideoPlayback_ffmpeg.Close;
begin
  if fVideoOpened then
  begin
    av_free(myBuffer);
    av_free(AVFrameRGB);
    av_free(AVFrame);

    avcodec_close(VideoCodecContext);
    av_close_input_file(VideoFormatContext);

    fVideoOpened:=False;
  end;
end;

procedure TVideoPlayback_ffmpeg.Play;
begin
end;

procedure TVideoPlayback_ffmpeg.Pause;
begin
  fVideoPaused := not fVideoPaused;
end;

procedure TVideoPlayback_ffmpeg.Stop;
begin
end;

procedure TVideoPlayback_ffmpeg.SetPosition(Time: real);
begin
  fVideoSkipTime := Time;

  if fVideoSkipTime > 0 then
  begin
    av_seek_frame(VideoFormatContext,VideoStreamIndex,Floor(Time/VideoTimeBase),AVSEEK_FLAG_ANY);

    VideoTime := fVideoSkipTime;
  end;
end;

// what is this supposed to do? return VideoTime?
function  TVideoPlayback_ffmpeg.GetPosition: real;
begin
  result := 0;
end;

initialization
  singleton_VideoFFMpeg := TVideoPlayback_ffmpeg.create();
  AudioManager.add( singleton_VideoFFMpeg );

finalization
  AudioManager.Remove( singleton_VideoFFMpeg );

end.
