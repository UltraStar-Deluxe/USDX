{############################################################################
#                   FFmpeg support for UltraStar deluxe                     #
#   now uses acinerella                                                     #
#                                                                           #
#   Created by b1indy, modified by brunzel                                  #
#   based on 'An ffmpeg and SDL Tutorial' (http://www.dranger.com/ffmpeg/)  #
#   and acinerella demo  (http://sourceforge.net/projects/acinerella/)      #
#############################################################################}

//{$define Info}

unit UVideoAcinerella;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}


uses SDL,
     Classes, //for TFilestream
     UGraphicClasses,
     textgl,
     acinerella,
     math,
     gl,
     glu,
     glext,
     SysUtils,
     SyncObjs,
     {$ifdef DebugDisplay}
     {$ifdef win32}
     dialogs,
     {$endif}
     {$ENDIF}
     UIni,
     UTime,
     windows;

type
  TAspectCorrection = (acoStretch, acoCrop, acoLetterBox); //from 1.1

  TRectCoords = record         //from 1.1
    Left, Right:        double;
    Upper, Lower:       double;
    windowed:           boolean;
    Reflection:         boolean;
    ReflectionSpacing:  real;
    TargetAspect:       TAspectCorrection;
    ZoomFaktor:         real;
  end;

  TFrameBuffer = record
    frame:      Pointer;
    time:       Extended;
    displayed:  Boolean;
  end;

  TFrameThread = class(TThread)
  private
    Time:               Extended;
    Gap:                Single;
    Start:              Single;

    iSkip:              Boolean;
    waiting:            Boolean;

    procedure   DoDecode;
    procedure   DoSkip;
    procedure   Copy;
    procedure   Update();
  protected
    constructor Create;
    procedure   Execute; override;
  end;

procedure Init;
procedure acOpenFile(FileName: pAnsiChar);
procedure acClose;
procedure acGetFrame(Time: Extended);
procedure acDrawGL(Screen: integer; DoDraw: boolean);
procedure acDrawGLi(Screen: integer; Window: TRectCoords; Blend: real; DoDraw: boolean);
procedure acTogglePause;
procedure acSkip2(Gap: Single; Start: Single);
procedure ToggleAspectCorrection;
procedure GetVideoRect(var ScreenRect, TexRect: TRectCoords; Window: TRectCoords);
procedure SetAspectCorrection(aspect: TAspectCorrection);
procedure ResetAspectCorrection;
procedure UploadNewFrame;

Const
  MIN_FPS = 40;
  MAX_FPS = 55;

var
  VideoOpened:        Boolean;
  VideoPaused:        Boolean;
  VideoTex:           glUint;

  VideoStreamIndex:   Integer;
  SkipLines:          Integer;
  LastSkipLines:      Integer;
  mmfps:              Real;
  Counter:            Integer;

  TexX, TexY:         Integer;
  dataX, dataY:       Integer;

  VideoTimeBase:      Extended;
  VideoTime:          Extended;
  LastFrameTime:      Extended;
  TimeDifference:     Extended;
  NegativeSkipTime:   Extended;

  ScaledVideoWidth:   Real;
  ScaledVideoHeight:  Real;

  VideoAspect:        Real;
  VideoSkipTime:      Single;
  ActualH, ActualW:   Integer;

  inst:               PAc_instance;
  pack:               PAc_package;
  info:               TAc_stream_info;
  videodecoder:       PAc_decoder;

  fAspect:            Real;               //**< width/height ratio (from 1.1)
  fAspectCorrection:  TAspectCorrection; //from 1.1

  fs:                 TFileStream;
  fName:              String;

  timediff_str:       String;   //for debug
  mtime_str:          String;      //for debug

  pbo:                glUint;
  pbo_supported:      Boolean;

  PIXEL_FORMAT:       glUint;
  numBytes:           Integer;

  EnableVideoDraw:    boolean;

  FrameData:          Pointer;
  NewFrame:           boolean;
  SetTime:            Extended;
  SetGap:             Single;
  SetStart:           Single;
  SetSkip:            boolean;

  FrameThread:        TFrameThread;

  EventDecode:        TEvent;
  FrameBuffer:        array [0..49] of TFrameBuffer;
  MutexFramebuffer:   PSDL_Mutex;
  MutexSyncSignals:   PSDL_Mutex;

implementation

uses
  UGraphic,
  ULog,
  UDisplay;

function read_proc(sender: Pointer; buf: PByte; size: integer): integer; cdecl;
begin
  result := fs.Read(buf^, size);
end;

function seek_proc(sender: Pointer; pos: int64; whence: integer): int64; cdecl;
begin
  result := fs.Seek(pos, TSeekOrigin(whence));
end;

procedure Init;
var
  i:  integer;

begin
  inst := nil;
  videodecoder := nil;

  VideoOpened:=False;
  VideoPaused:=False;
  fName := '';

  glGenTextures(1, @VideoTex);

  if (pbo_supported) then
    glGenBuffers(1, @pbo);

  fAspectCorrection := TAspectCorrection(0);
  SkipLines := 0;
  LastSkipLines := 0;
  Counter := 0;
  numBytes := 4;
  if (numBytes=3) then
    PIXEL_FORMAT := GL_BGR
  else
    PIXEL_FORMAT := GL_RGBA;

  EnableVideoDraw := true;

  SetTime := 0;
  if(FrameData<>nil) then
    FreeMem(FrameData);
  FrameData := nil;

  for i := 0 to High(FrameBuffer) do
  begin
    if(FrameBuffer[i].frame <> nil) then
      FreeMem(FrameBuffer[i].frame);
    FrameBuffer[i].frame := nil;

    FrameBuffer[i].time := -1;
    FrameBuffer[i].displayed := true;
  end;

  if (FrameThread<>nil) then
  begin
    FrameThread.Terminate;
    FrameThread.WaitFor;
    FrameThread.Free;
    FrameThread := nil;
  end;

  MutexFramebuffer   := SDL_CreateMutex;
  MutexSyncSignals  := SDL_CreateMutex;
end;

procedure acOpenFile(FileName: pAnsiChar);
var
  I: integer;

begin
  acClose;

  VideoPaused    := False;
  VideoTimeBase  := 0;
  VideoTime      := 0;
  LastFrameTime  := 0;
  TimeDifference := 0;
  Counter := 0;

  if not FileExists(FileName) then
    Exit; //TODO: error.log

  fs := TFileStream.Create(FileName, fmOpenRead);
  fs.Position := 0;

  inst := ac_init();
  videodecoder := nil;
  ac_open(inst, nil, nil, @read_proc, @seek_proc, nil, nil);

  if not inst^.opened then
  begin
    fs.Free;
    Exit; //TODO: error.log
  end;

  //find VideoStreamIndex
  VideoStreamIndex:=-1;
  for I := 0 to inst^.stream_count - 1 do
  begin
    ac_get_stream_info(inst, I, @info);
    case info.stream_type of
      AC_STREAM_TYPE_VIDEO:
      begin

        if videodecoder = nil then
        begin
          VideoStreamIndex:=I;
          if (numBytes=3) then
            inst^.output_format := AC_OUTPUT_BGR24
          else
            inst^.output_format := AC_OUTPUT_BGRA32;

          videodecoder := ac_create_decoder(inst, I);
        end;
      end;
    end;
  end;

  if(VideoStreamIndex < 0) then
  begin
    if videodecoder <> nil then
      ac_free_decoder(videodecoder);
    ac_close(inst);
    ac_free(inst);
    fs.Free;
    Exit;
  end;

  VideoOpened:=True;
  fName := FileName;

  TexX := videodecoder^.stream_info.additional_info.video_info.frame_width;
  TexY := videodecoder^.stream_info.additional_info.video_info.frame_height;
  dataX := Round(Power(2, Ceil(Log2(TexX))));
  dataY := Round(Power(2, Ceil(Log2(TexY))));

  GetMem(FrameData, numBytes*TexX*TexY);
  for I := 0 to high(FrameBuffer) do
  begin
    GetMem(FrameBuffer[I].frame, numBytes*TexX*TexY);
  end;

  // calculate some information for video display
  VideoAspect:=videodecoder^.stream_info.additional_info.video_info.pixel_aspect;
  if (VideoAspect = 0) then
    VideoAspect:=TexX/TexY
  else
    VideoAspect:=VideoAspect*TexX/TexY;

  fAspect := VideoAspect;

  if (info.additional_info.video_info.frames_per_second>0) then
    VideoTimeBase:=1/info.additional_info.video_info.frames_per_second;

  glBindTexture(GL_TEXTURE_2D, VideoTex);

  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_PRIORITY, 1.0);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  glTexImage2D(GL_TEXTURE_2D, 0, 3, dataX, dataY, 0,
    PIXEL_FORMAT, GL_UNSIGNED_BYTE, nil);
  glBindTexture(GL_TEXTURE_2D, 0);

  if(pbo_supported) then
  begin
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER_ARB, pbo);
    glBufferData(GL_PIXEL_UNPACK_BUFFER_ARB, numBytes*TexX*TexY, nil, GL_STREAM_DRAW);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER_ARB, 0);
  end;

  mmfps := (MAX_FPS-MIN_FPS)/2;

  SetTime := 0;
  NewFrame := false;
  SetSkip := false;

  if (EventDecode = nil) then
    EventDecode := TEvent.Create(nil, false, false, '');

  if (FrameThread = nil) then
    FrameThread := TFrameThread.Create();

  FrameThread.Resume;
end;

procedure acClose;
var
  I:  Integer;

begin
  if VideoOpened then
  begin
    if (FrameThread<>nil) then
    begin
      FrameThread.Terminate;
      FrameThread.WaitFor;
      FrameThread.Free;
      FrameThread := nil;
    end;

    FreeAndNil(EventDecode);

    NewFrame := false;
    SetSkip := false;

    if videodecoder <> nil then
      ac_free_decoder(videodecoder);

    videodecoder:=nil;
    ac_close(inst);
    ac_free(inst);
    inst := nil;
    fs.Free;
    fs:=nil;

    VideoOpened:=False;
    fName := '';

    if(FrameData<>nil) then
      FreeMem(FrameData);
    FrameData := nil;

    for I := 0 to High(FrameBuffer) do
    begin
      if(FrameBuffer[i].frame <> nil) then
        FreeMem(FrameBuffer[i].frame);
      FrameBuffer[i].frame := nil;

      FrameBuffer[i].time := -1;
      FrameBuffer[i].displayed := false;
    end;
  end;
end;

procedure acTogglePause;
begin
  if VideoPaused then VideoPaused:=False
  else VideoPaused:=True;
end;

procedure acSkip2(Gap: Single; Start: Single);
begin
  SDL_LockMutex(MutexSyncSignals);
  try
    SetGap := Gap;
    SetStart := Start;
    SetSkip := true;
  finally
    SDL_UnlockMutex(MutexSyncSignals);
  end;
  EventDecode.SetEvent;
end;

procedure acGetFrame(Time: Extended);
begin
  if (Time-SetTime>=0) or (SetTime>Time) then
  begin
    SDL_LockMutex(MutexSyncSignals);
    try
      SetTime := Time;
    finally
      SDL_UnlockMutex(MutexSyncSignals);
    end;
    UploadNewFrame();
    EventDecode.SetEvent;
  end;
end;

procedure TFrameThread.DoDecode;
Const
  MAX = 3000;
  FRAMEDROPCOUNT=4;

var
  FrameFinished:  Integer;
  myTime:         Extended;
  DropFrame:      Boolean;
  droppedFrames:  Integer;
  I:              Integer;

begin
  {mmfps := (Display.mFPS+mmfps)/2;
  if(Ini.PerformanceMode=1) then
  begin
    if (mmfps<MIN_FPS) then
    begin
      if(SkipLines<3) and (Counter<MAX) then
        Counter := Counter + round(TimeSkip*1000)
      else if (SkipLines<3) and (Counter>=MAX) then
      begin
        Inc(SkipLines);
        mmfps:=(MAX_FPS-MIN_FPS)/2;
        Counter := 0;
      end;
    end else if (mmfps>MAX_FPS) then
    begin
      if(SkipLines>0) and (Counter<MAX) then
        Counter := Counter + round(TimeSkip*1000)
      else if (SkipLines>0) and (Counter>=MAX) then
      begin
        Dec(SkipLines);
        LastSkipLines := SkipLines;
        Counter := 0;
      end;
    end else Counter := 0;
  end;

  if (Counter>MAX) then
    Counter := MAX;}

  if (NegativeSkipTime < 0)and(Time+NegativeSkipTime>=0) then NegativeSkipTime:=0;

  myTime:=Time+VideoSkipTime;
  TimeDifference:=myTime-VideoTime;

  DropFrame:=False;

  if TimeDifference >= (FRAMEDROPCOUNT-1)*VideoTimeBase then
  begin // skip frames
    DropFrame:=True;
  end;

  if terminated then
    Exit;

  pack := ac_read_package(inst);
  FrameFinished:=0;
  // read packets until we have a finished frame (or there are no more packets)
  while (FrameFinished=0) and (pack <> nil) and not DropFrame do
  begin
    if terminated then
      Exit;

    // if we got a packet from the video stream, then decode it
    if (videodecoder^.stream_index = pack^.stream_index) then
    begin
      FrameFinished := ac_decode_package(pack, videodecoder);
      ac_free_package(pack);
      if (FrameFinished=0) then
        pack := ac_read_package(inst);
    end else
    begin
      ac_free_package(pack);
      pack := ac_read_package(inst);
    end;
  end;


  if DropFrame then
  begin
    for droppedFrames:=1 to FRAMEDROPCOUNT-1 do
    begin
      if terminated then
        Exit;

      if (droppedFrames>1) then
        pack := ac_read_package(inst);
      FrameFinished:=0;
      // read packets until we have a finished frame (or there are no more packets)
      while (FrameFinished=0) and (pack <> nil) do
      begin
        if terminated then
          Exit;

        // if we got a packet from the video stream, then decode it (without scaling)
        if (videodecoder^.stream_index = pack^.stream_index) then
        begin
          FrameFinished := ac_drop_decode_package(pack, videodecoder);
          ac_free_package(pack);
          if (FrameFinished=0) then
            pack := ac_read_package(inst);
        end else
        begin
          ac_free_package(pack);
          pack := ac_read_package(inst);
        end;
      end;
    end;

    for I:=droppedFrames to FRAMEDROPCOUNT do
    begin
      if terminated then
        Exit;

      if (droppedFrames>1) then
        pack := ac_read_package(inst);

      FrameFinished:=0;
      // read packets until we have a finished frame (or there are no more packets)
      while (FrameFinished=0) and (pack <> nil) do
      begin
        if terminated then
          Exit;

        // if we got a packet from the video stream, then decode it (with scaling)
        if (videodecoder^.stream_index = pack^.stream_index) then
        begin
          FrameFinished := ac_decode_package(pack, videodecoder);
          ac_free_package(pack);
          if (FrameFinished=0) then
            pack := ac_read_package(inst);
        end else
        begin
          ac_free_package(pack);
          pack := ac_read_package(inst);
        end;
      end;
    end;
  end;

  // if we did not get an new frame, there's nothing more to do
  if Framefinished=0 then
    Exit;

  //cope the decoded frame into buffer
  Copy;
end;

procedure TFrameThread.Copy();
var
  num:            Integer;
  FrameDataPtr:   PByteArray;
  FrameDataPtr2:  PByteArray;
  I:              Integer;

begin
  num := -1;
  SDL_LockMutex(MutexFramebuffer);
  try

    for I := 0 to high(FrameBuffer) do
    begin
      if FrameBuffer[I].displayed then
      begin
        num := I;
        break;
      end;
    end;

    if (num = -1) then
    begin
      waiting := true;
    end else
    begin
      FrameDataPtr := FrameBuffer[num].frame;
      FrameBuffer[num].time := VideoTime;
      FrameBuffer[num].displayed := false;

      FrameDataPtr2:=Pointer(videodecoder^.buffer);

      if terminated then
        Exit;

      move(FrameDataPtr2[0], FrameDataPtr[0], numBytes*TexX*TexY);

      VideoTime := videodecoder^.timecode;

      waiting := false;
    end;
  finally
    SDL_UnLockMutex(MutexFramebuffer);
  end;
end;

procedure TFrameThread.DoSkip;
var
  SkipTime: Extended;
  I:        Integer;

begin
  SDL_LockMutex(MutexSyncSignals);
  try
    VideoSkiptime:=Gap;
    NegativeSkipTime:=Start+Gap;
    SkipTime := Start+Gap;
  finally
    SDL_UnlockMutex(MutexSyncSignals);
  end;

  if SkipTime > 0 then
  begin
    VideoTime:=SkipTime;
    try
      ac_seek(videodecoder, -1, Floor((SkipTime)*1000));
    except
      Log.LogError('Error seeking Video "acSkip2" on video ('+fName+')');
    end;
  end else
  begin
    try
      ac_seek(videodecoder, 0, 0);
    except
      Log.LogError('Error seeking Video "acSkip2" on video ('+fName+')');
    end;
    VideoTime:=0;
  end;

  waiting := false;

  SDL_LockMutex(MutexFramebuffer);
  try
    for I := 0 to High(FrameBuffer) do
    begin
      FrameBuffer[i].time := -1;
      FrameBuffer[i].displayed := true;
    end;
  finally
    SDL_UnLockMutex(MutexFramebuffer);
  end;
end;

constructor TFrameThread.Create;
begin
  inherited Create(true);
  Self.Priority := tpNormal;
  Self.FreeOnTerminate := false;

  Time := 0;
  iSkip := false;
  waiting := false;

  Self.Resume;
end;

procedure TFrameThread.Execute;
begin
  while not terminated do
  begin
    if (EventDecode.WaitFor(1) = wrSignaled) or not waiting then
    begin
      try
        if iSkip then
        begin
          DoSkip;
          iSkip := false;
        end;

        if not waiting then
          DoDecode();

        if waiting then
          Copy();
      except

      end;
    end;
    if not terminated then
      Update;
  end;
end;

procedure TFrameThread.Update;
begin
  SDL_LockMutex(MutexSyncSignals);
  try
    Time := SetTime;
    if (SetSkip) then
      iSkip := true;

    SetSkip := false;
    Gap := SetGap;
    Start := SetStart;
  finally
    SDL_UnlockMutex(MutexSyncSignals);
  end;
end;

procedure ToggleAspectCorrection();
begin
  case fAspectCorrection of
    acoCrop      : fAspectCorrection := acoStretch;
    acoStretch   : fAspectCorrection := acoLetterBox;
    acoLetterBox : fAspectCorrection := acoCrop;
  end;
end;

procedure SetAspectCorrection(aspect: TAspectCorrection);
begin
  fAspectCorrection := aspect;
end;

procedure ResetAspectCorrection;
begin
  fAspectCorrection := acoCrop;

  //case Ini.AspectCorrect of
  //  integer(acoCrop)      : fAspectCorrection := acoCrop;
  //  integer(acoStretch)   : fAspectCorrection := acoStretch;
  //  integer(acoLetterBox) : fAspectCorrection := acoLetterBox;
  //end;
end;

procedure GetVideoRect(var ScreenRect, TexRect: TRectCoords; Window: TRectCoords);
var
  RectS, RectT: TRectCoords;


  procedure GetCoords(var SRect: TRectCoords; Win: TRectCoords; Aspect: TAspectCorrection);
  var
    ScreenAspect:       double;  // aspect of screen resolution
    ScaledVideoWidth:   double;
    ScaledVideoHeight:  double;
    rW, rH:             double;

  begin
    // Three aspects to take into account:
    //  1. Screen/display resolution (e.g. 1920x1080 -> 16:9)
    //  2. Render aspect (fixed to 800x600 -> 4:3)
    //  3. Movie aspect (video frame aspect stored in fAspect)
    if (Win.windowed) then
    begin
      rW := (Win.Right-Win.Left);
      rH := (Win.Lower-Win.Upper);
      ScreenAspect := rW*((ScreenW/Screens)/RenderW)/(rH*(ScreenH/RenderH));
    end else
    begin
      rW := RenderW;
      rH := RenderH;
      ScreenAspect := (ScreenW/Screens) / ScreenH;
    end;

    case Aspect of
      acoStretch: begin
        ScaledVideoWidth  := rW;
        ScaledVideoHeight := rH;
      end;

      acoCrop: begin
        if (ScreenAspect >= fAspect) then
        begin
          ScaledVideoWidth  := rW;
          ScaledVideoHeight := rH * ScreenAspect/fAspect;
        end
        else
        begin
          ScaledVideoHeight := rH;
          ScaledVideoWidth  := rW * fAspect/ScreenAspect;
        end;
      end;

      acoLetterBox: begin
        if (ScreenAspect <= fAspect) then
        begin
          ScaledVideoWidth  := rW;
          ScaledVideoHeight := rH * ScreenAspect/fAspect;
        end
        else
        begin
          ScaledVideoHeight := rH;
          ScaledVideoWidth  := rW * fAspect/ScreenAspect;
        end;
      end
      else
        raise Exception.Create('Unhandled aspect correction!');
    end;

    SRect.Left := (rW - ScaledVideoWidth) / 2 + Win.Left;
    SRect.Right := SRect.Left + ScaledVideoWidth;
    SRect.Upper := (rH - ScaledVideoHeight) / 2 + Win.Upper;
    SRect.Lower := SRect.Upper + ScaledVideoHeight;
  end;
begin
  if (Window.TargetAspect = fAspectCorrection) then
    GetCoords(ScreenRect, Window, fAspectCorrection)
  else
  begin
    GetCoords(RectS, Window, fAspectCorrection);
    GetCoords(RectT, Window, Window.TargetAspect);

    ScreenRect.Left := RectS.Left + (RectT.Left-RectS.Left)*Window.ZoomFaktor;
    ScreenRect.Right := RectS.Right + (RectT.Right-RectS.Right)*Window.ZoomFaktor;
    ScreenRect.Upper := RectS.Upper + (RectT.Upper-RectS.Upper)*Window.ZoomFaktor;
    ScreenRect.Lower := RectS.Lower + (RectT.Lower-RectS.Lower)*Window.ZoomFaktor;
  end;


  // texture contains right/lower (power-of-2) padding.
  // Determine the texture coords of the video frame.
  TexRect.Left  := 0;
  TexRect.Right := TexX  / dataX;
  TexRect.Upper := 0;
  if (not pbo_supported) then
    TexRect.Lower := (TexY/(SkipLines+1)) / dataY
  else
    TexRect.Lower := TexY/ dataY;
end;

procedure acDrawGL(Screen: integer; DoDraw: boolean);
var
  Window: TRectCoords;
begin
  Window.Left := 0;
  Window.Right := RenderW;
  Window.Upper := 0;
  Window.Lower := RenderH;
  Window.windowed := false;
  Window.Reflection := false;
  Window.TargetAspect := fAspectCorrection;
  acDrawGLi(Screen, Window, 1, DoDraw);
end;

procedure UploadNewFrame;
var
  FrameDataPtr:   PByteArray;
  FrameDataPtr2:  PByteArray;
  I:              Integer;
  glError:        glEnum;
  glErrorStr:     String;
  num:            Integer;

  function FindFrame(): Integer;
  var
    I:        Integer;
    diff, td: Extended;
  begin
    Result := -1;
    diff := 10000000;

    for I := 0 to high(FrameBuffer) do
    begin
      td := SetTime + VideoSkipTime - FrameBuffer[I].time;
      if not FrameBuffer[I].displayed and (td < diff) and
        (td > VideoTimeBase) then
      begin
        diff := Abs(FrameBuffer[I].time - SetTime - VideoSkipTime);
        Result := I;
      end;

      if (td > VideoTimeBase*2) then
        FrameBuffer[I].displayed := true;
    end;

    if (Result<>-1) then
      FrameBuffer[Result].displayed := true
  end;

begin
  if VideoOpened then
  begin
    if (not pbo_supported) then
    begin
      SDL_LockMutex(MutexFramebuffer);
      try
        num := FindFrame();

        if (num>=0) then
        begin
          FrameDataPtr:=FrameBuffer[num].frame;

          glBindTexture(GL_TEXTURE_2D, VideoTex);

          if(SkipLines>0)then
          begin
            for I := 0 to TexY - 1 do
            begin
              if(I mod (SkipLines+1) = 0) then
                glTexSubImage2D(GL_TEXTURE_2D, 0, 0, (I div (SkipLines+1)), TexX, 1,
                  PIXEL_FORMAT, GL_UNSIGNED_BYTE, @FrameDataPtr[I*numBytes*TexX]);
            end;
          end else
              glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, TexX, TexY,
                PIXEL_FORMAT, GL_UNSIGNED_BYTE, @FrameDataPtr[0]);
        end;
      finally
        SDL_UnLockMutex(MutexFramebuffer);
      end;
    end else
    begin
      glGetError();
      glBindBuffer(GL_PIXEL_UNPACK_BUFFER_ARB, pbo);

      glError := glGetError;
      if glError <> GL_NO_ERROR then
      begin
        acClose;
        Log.LogError('Error drawing Video "glBindBuffer"');
        Exit;
      end;

      FrameDataPtr := glMapBuffer(GL_PIXEL_UNPACK_BUFFER_ARB, GL_WRITE_ONLY);
      glError := glGetError;
      if glError <> GL_NO_ERROR then
      begin
        acClose;
        Log.LogError('Error drawing Video pbo "glMapBuffer"');
        Exit;
      end;

      SDL_LockMutex(MutexFramebuffer);
      try
        num := FindFrame();
        if (num>=0) then
        begin
          FrameDataPtr2:=FrameBuffer[num].frame;
          move(FrameDataPtr2[0], FrameDataPtr[0], numBytes*TexX*TexY);
        end;
      finally
        SDL_UnLockMutex(MutexFramebuffer);
      end;

      glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER_ARB);
      glError := glGetError;
      if glError <> GL_NO_ERROR then
      begin
        acClose;
        Log.LogError('Error drawing Video pbo "glUnmapBuffer"');
        Exit;
      end;

      glBindTexture(GL_TEXTURE_2D, VideoTex);
      glError := glGetError;
      if glError <> GL_NO_ERROR then
      begin
        acClose;
        Log.LogError('Error drawing Video pbo "glBindTexture"');
        Exit;
      end;

      glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, TexX, TexY,
        PIXEL_FORMAT, GL_UNSIGNED_BYTE, nil);

      glError := glGetError;
      if glError <> GL_NO_ERROR then
      begin
        acClose;
        case glError of
              GL_INVALID_ENUM: glErrorStr:='INVALID_ENUM';
              GL_INVALID_VALUE: glErrorStr:='INVALID_VALUE';
              GL_INVALID_OPERATION: glErrorStr:='INVALID_OPERATION';
              GL_STACK_OVERFLOW: glErrorStr:='STACK_OVERFLOW';
              GL_STACK_UNDERFLOW: glErrorStr:='STACK_UNDERFLOW';
              GL_OUT_OF_MEMORY: glErrorStr:='OUT_OF_MEMORY';
              else glErrorStr:='unknown error';
            end;
        Log.LogError('Error drawing Video pbo "glTexSubImage2D" ('+glErrorStr+')');
        Exit;
      end;
      glBindTexture(GL_TEXTURE_2D, 0);
      glBindBuffer(GL_PIXEL_UNPACK_BUFFER_ARB, 0);
    end;
    EventDecode.SetEvent;
  end;
end;

procedure acDrawGLi(Screen: integer; Window: TRectCoords; Blend: real; DoDraw: boolean);
var
  ScreenRect, TexRect: TRectCoords;

begin
  if not DoDraw then
    Exit;

  // have a nice black background to draw on (even if there were errors opening the vid)
  if Not Window.windowed then
  begin
    //glDisable(GL_BLEND);

    glScissor(round((ScreenW/Screens)*(Screen-1)),
      0,
      round(ScreenW/Screens),
      round(ScreenH));
    glEnable(GL_SCISSOR_TEST);

    glClearColor(0,0,0,1);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    glDisable(GL_SCISSOR_TEST);
  end else if EnableVideoDraw then
    glEnable(GL_BLEND);

  if not EnableVideoDraw then
    Exit;

  // exit if there's nothing to draw
  if not VideoOpened then
    Exit;

  GetVideoRect(ScreenRect, TexRect, Window);

  if Window.windowed then
  begin
    glScissor(round((Window.Left)*(ScreenW/Screens)/RenderW+(ScreenW/Screens)*(Screen-1)),
      round((RenderH-Window.Lower)*ScreenH/RenderH),
      round((Window.Right-Window.Left)*(ScreenW/Screens)/RenderW),
      round((Window.Lower-Window.Upper)*ScreenH/RenderH));
    glEnable(GL_SCISSOR_TEST);
  end;

  glEnable(GL_TEXTURE_2D);
  glColor4f(1, 1, 1, Blend);
  glBindTexture(GL_TEXTURE_2D, VideoTex);
  //glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
  glbegin(gl_quads);
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

  if Window.windowed then
    glDisable(GL_SCISSOR_TEST);

  glDisable(GL_BLEND);

  //Draw Reflection
  if Window.Reflection then
  begin
    glScissor(round((Window.Left)*(ScreenW/Screens)/RenderW+(ScreenW/Screens)*(Screen-1)),
      round((RenderH-Window.Lower-Window.ReflectionSpacing-(Window.Lower-Window.Upper)*0.5)*ScreenH/RenderH),
      round((Window.Right-Window.Left)*(ScreenW/Screens)/RenderW),
      round((Window.Lower-Window.Upper)*ScreenH/RenderH*0.5));
    glEnable(GL_SCISSOR_TEST);

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    //Draw
    glBegin(GL_QUADS);//Top Left
      glColor4f(1, 1, 1, Blend-0.3);
      glTexCoord2f(TexRect.Left, TexRect.Lower);
      glVertex2f(ScreenRect.Left, Window.Lower + Window.ReflectionSpacing);

      //Bottom Left
      glColor4f(1, 1, 1, 0);
      glTexCoord2f(TexRect.Left, (TexRect.Lower-TexRect.Upper)*0.5);
      glVertex2f(ScreenRect.Left,
        Window.Lower + (ScreenRect.Lower-ScreenRect.Upper)*0.5 + Window.ReflectionSpacing);

      //Bottom Right
      glColor4f(1, 1, 1, 0);
      glTexCoord2f(TexRect.Right, (TexRect.Lower-TexRect.Upper)*0.5);
      glVertex2f(ScreenRect.Right,
        Window.Lower + (ScreenRect.Lower-ScreenRect.Upper)*0.5 + Window.ReflectionSpacing);

      //Top Right
      glColor4f(1, 1, 1, Blend-0.3);
      glTexCoord2f(TexRect.Right, TexRect.Lower);
      glVertex2f(ScreenRect.Right, Window.Lower + Window.ReflectionSpacing);
    glEnd;

    glDisable(GL_BLEND);
    glDisable(GL_SCISSOR_TEST);
  end;

  glDisable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, 0);

  if (Ini.MovieSize < 1) and not Window.windowed then  //HalfSize BG
  begin
    // draw fading bars over top and bottom of background/video
    glEnable(GL_BLEND);
    glBegin(GL_QUADS);
      (* black top *)
      glColor4f(0, 0, 0, 1);
      glVertex2f(0, 0);
      glVertex2f(RenderW, 0);
      glVertex2f(RenderW, 110);
      glVertex2f(0, 110);

      (* top gradient *)
      glVertex2f(0, 110);
      glVertex2f(RenderW, 110);
      glColor4f(0, 0, 0, 0);
      glVertex2f(RenderW, 130);
      glVertex2f(0, 130);

      (* bottom gradient *)
      glColor4f(0, 0, 0, 0);
      glVertex2f(0, RenderH-130);
      glVertex2f(RenderW, RenderH-130);
      glColor4f(0, 0, 0, 1);
      glVertex2f(RenderW, RenderH-110);
      glVertex2f(0, RenderH-110);
      
      (* black bottom *)
      glVertex2f(0, RenderH-110);
      glVertex2f(RenderW, RenderH-110);
      glVertex2f(RenderW, RenderH);
      glVertex2f(0, RenderH);

    glEnd;
    glDisable(GL_BLEND);
  end;

// info-stuff

{$ifdef Info}
  if VideoSkipTime+videodecoder^.timecode+VideoTimeBase < 0 then begin
    glColor4f(0.7, 1, 0.3, 1);
    SetFontStyle (1);
    SetFontItalic(False);
    SetFontSize(15);
    SetFontPos (300, 0);
    glPrint('Delay due to negative VideoGap');
    glColor4f(1, 1, 1, 1);
  end;
{$endif}

  if (Ini.Debug = 1) then
  begin
    glColor4f(0, 0, 0, 0.2);
    glbegin(gl_quads);
      glVertex2f(0, 50);
      glVertex2f(0, 170);
      glVertex2f(250, 170);
      glVertex2f(250, 50);
    glEnd;

    glColor4f(1,1,1,1);
    SetFontStyle (1);
    SetFontItalic(False);
    SetFontSize(15);
    SetFontPos (5, 50);
    glPrint(PChar('tdiff: ' + FormatFloat('#0.00', TimeDifference)));

    SetFontPos (5, 65);
    glPrint(PChar(timediff_str));

    SetFontPos (5, 80);
    glPrint(PChar(mtime_str));

    SetFontPos (5, 95);
    case fAspectCorrection of
      acoCrop      : glPrint('Crop');
      acoStretch   : glPrint('Stretch');
      acoLetterBox : glPrint('LetterBox');
    end;

    SetFontPos (5, 110);
    glPrint(PChar('mmfps: '+FormatFloat('#0.00', mmfps)));

    SetFontPos (5, 125);
    glPrint(PChar('skipL: '+inttostr(SkipLines)));

    SetFontPos (5, 140);
    glPrint(PChar('Counter: '+inttostr(Counter)));
  end;
end;

end.