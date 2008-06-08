unit UDisplay;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  ucommon,
  SDL,
  UMenu,
  gl,
  glu,
  SysUtils;

type
  TDisplay = class
    private
      //fade-to-black-hack
      BlackScreen: Boolean;

      doFade   : Boolean;
      canFade  : Boolean;
      myFade   : integer;
      lastTime : Cardinal;
      pTexData : Pointer;
      pTex     : array[1..2] of glUInt;

      FPSCounter    : Cardinal;
      LastFPS       : Cardinal;
      NextFPSSwap   : Cardinal;

      OSD_LastError : String;

      procedure DrawDebugInformation;
    public
      NextScreen   : PMenu;
      CurrentScreen : PMenu;

      //popup hack
      NextScreenWithCheck: Pmenu;
      CheckOK  : Boolean;

      Fade     : Real;

      constructor Create;
      destructor  Destroy; override;

      procedure SaveScreenShot;

      function  Draw: Boolean;
  end;

var
  Display:          TDisplay;

implementation

uses
  UImage,
  TextGL,
  ULog,
  UMain,
  UTexture,
  UIni,
  UGraphic,
  UTime,
  UCommandLine;

constructor TDisplay.Create;
var
  i: integer;
  
begin
  inherited Create;

  //popup hack
  CheckOK             := False;
  NextScreen          := NIL;
  NextScreenWithCheck := NIL;
  BlackScreen         := False;

  // fade mod
  myfade:=0;

  if Ini.ScreenFade=1 then
    doFade:=True
  else
    doFade:=False;

  canFade:=True;
  // generate texture for fading between screens
  GetMem(pTexData, 512*512*4);

  if pTexData <> NIL then
  for i:= 1 to 2 do
  begin
  
    glGenTextures(1, @pTex[i] );

    if glGetError <> GL_NO_ERROR then
      canFade := False;

    glBindTexture(GL_TEXTURE_2D, pTex[i]);
    if glGetError <> GL_NO_ERROR then
      canFade := False;

    glTexImage2D(GL_TEXTURE_2D, 0, 4, 512, 512, 0, GL_RGBA, GL_UNSIGNED_BYTE, pTexData);
    if glGetError <> GL_NO_ERROR then
      canFade := False;
      
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    if glGetError <> GL_NO_ERROR then
      canFade := False;
      
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    if glGetError <> GL_NO_ERROR then
      canFade := False;
  end
  else
  begin
    canFade:=False;
  end;

  FreeMem(pTexData);

  //Set LastError for OSD to No Error
  OSD_LastError := 'No Errors';
end;

destructor TDisplay.Destroy;
begin
  if canFade then
    glDeleteTextures(1,@pTex);
    
  inherited Destroy;
end;

function TDisplay.Draw: Boolean;
var
  S:    integer;
  Col: Real;
  myFade2: Real;
  currentTime: Cardinal;
  glError: glEnum;
  glErrorStr: String;
begin
  Result := True;

  Col := 1;
  {if (ParamStr(1) = '-black') or (ParamStr(1) = '-fsblack') then
    Col := 0;    }

  glClearColor(Col, Col, Col , 0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  for S := 1 to Screens do begin
    ScreenAct := S;

//    if Screens = 1 then ScreenX := 0;
//    if (Screens = 2) and (S = 1) then ScreenX := -1;
//    if (Screens = 2) and (S = 2) then ScreenX := 1;
    ScreenX := 0;

    glViewPort((S-1) * ScreenW div Screens, 0, ScreenW div Screens, ScreenH);

    //popup hack
    // check was successful... move on
    if CheckOK then
    begin
      if assigned (NextScreenWithCheck)then
      begin
        NextScreen:=NextScreenWithCheck;
        NextScreenWithCheck := NIL;
        CheckOk:=False;
      end
      else
      begin
        BlackScreen:=True; // end of game - fade to black before exit
      end;
    end;

//    CurrentScreen.SetAnimationProgress(1);
    if (not assigned (NextScreen)) and (not BlackScreen) then
    begin
      CurrentScreen.Draw;

      //popup mod
      if (ScreenPopupError <> NIL) and ScreenPopupError.Visible then
        ScreenPopupError.Draw
      else if (ScreenPopupCheck <> NIL) and ScreenPopupCheck.Visible then
        ScreenPopupCheck.Draw;

      // fade mod
      myfade:=0;
      if (Ini.ScreenFade=1) and canFade then
        doFade:=True
      else if Ini.ScreenFade=0 then
        doFade:=False;
    end
    else
    begin
      // check if we had an initialization error (canfade=false, dofade=true)
      if doFade and not canFade then
      begin
        doFade:=False; //disable fading
//        ScreenPopupError.ShowPopup('Error initializing\nfade texture\n\nfading\ndisabled'); //show error message
      end;
      if doFade and canFade then
      begin
        // fade mod
        //Create Fading texture if we're just starting
        if myfade = 0 then
        begin
          glViewPort(0, 0, 512, 512);
          CurrentScreen.Draw;
          glBindTexture(GL_TEXTURE_2D, pTex[S]);
          glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 0, 0, 512, 512, 0);
          glError:=glGetError;
          if (glError <> GL_NO_ERROR) then
          begin
            canFade := False;
            glErrorStr := gluErrorString(glError);
//            ScreenPopupError.ShowPopup('Error copying\nfade texture\n('+glErrorStr+')\nfading\ndisabled'); //show error message
          end;
          glViewPort((S-1) * ScreenW div Screens, 0, ScreenW div Screens, ScreenH);
          // blackscreen-hack
          if not BlackScreen then
            NextScreen.onShow;

          lastTime:=SDL_GetTicks();
          if (S=2) or (Screens = 1) then
            myfade:=myfade+1;
        end; // end texture creation in first fading step

        //do some time-based fading
        currentTime:=SDL_GetTicks();
        if (currentTime > lastTime+30) and (S=1) then
        begin
          myfade:=myfade+4;
          lastTime:=currentTime;
        end;

//      LastFade := Fade;   // whatever
//      Fade := Fade -0.999; // start fading out


//      CurrentScreen.ShowFinish := false; // no purpose?

//      CurrentScreen.SetAnimationProgress(Fade-1); // nop?

        // blackscreen-hack
        if not BlackScreen then
          NextScreen.Draw // draw next screen
        else if ScreenAct=1 then
        begin
          glClearColor(0, 0, 0 , 0);
          glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
        end;

      // and draw old screen over it... slowly fading out
        myfade2:=(myfade*myfade)/10000;
        glBindTexture(GL_TEXTURE_2D, pTex[S]);
        glColor4f(1, 1, 1, (1000-myfade*myfade)/1000); // strange calculation - alpha gets negative... but looks good this way
        glEnable(GL_TEXTURE_2D);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glEnable(GL_BLEND);
        glBegin(GL_QUADS);
          glTexCoord2f(0+myfade2,0+myfade2);glVertex2f(0,   600);
          glTexCoord2f(0+myfade2,1-myfade2);glVertex2f(0,   0);
          glTexCoord2f(1-myfade2,1-myfade2);glVertex2f(800, 0);
          glTexCoord2f(1-myfade2,0+myfade2);glVertex2f(800, 600);
        glEnd;
        glDisable(GL_BLEND);
        glDisable(GL_TEXTURE_2D);
      end
      // blackscreen hack
      else if not BlackScreen then
      begin
        NextScreen.OnShow;
      end;

      if ((myfade > 40) or (not doFade) or (not canFade)) And (S = 1) then
      begin // fade out complete...
        myFade:=0;
        CurrentScreen.onHide;
        CurrentScreen.ShowFinish:=False;
        CurrentScreen:=NextScreen;
        NextScreen := nil;
        if not blackscreen then
        begin
          CurrentScreen.onShowFinish;
          CurrentScreen.ShowFinish := true;
        end
        else
        begin
          Result:=False;
          Break;
        end;
      // end of fade mod
      end;
    end; // if

    //Draw OSD only on first Screen if Debug Mode is enabled
    if ((Ini.Debug = 1) or (Params.Debug)) and (S=1) then
      DrawDebugInformation;
      
  end; // for

  // SDL_GL_SwapBuffers();
end;

{function TDisplay.Fade(FadeIn : Boolean; Steps : UInt8): UInt8;
begin
  Self.FadeIn := FadeIn;
  FadeStep := (SizeOf(FadeStep) * $FF) div Steps;
  ActualStep := $FF;
  Result := $FF div FadeStep;
end;}

procedure TDisplay.SaveScreenShot;
var
  Num:        integer;
  FileName:   string;
  ScreenData: PChar;
  Surface:    PSDL_Surface;
  Success:    boolean;
  Align:      integer;
  RowSize:    integer;
begin
  for Num := 1 to 9999 do
  begin
    FileName := IntToStr(Num);
    while Length(FileName) < 4 do
      FileName := '0' + FileName;
    FileName := ScreenshotsPath + 'screenshot' + FileName + '.png';
    if not FileExists(FileName) then
      break
  end;

  // we must take the row-alignment (4byte by default) into account
  glGetIntegerv(GL_PACK_ALIGNMENT, @Align);
  // calc aligned row-size
  RowSize := ((ScreenW*3 + (Align-1)) div Align) * Align;

  GetMem(ScreenData, RowSize * ScreenH);
  glReadPixels(0, 0, ScreenW, ScreenH, GL_RGB, GL_UNSIGNED_BYTE, ScreenData);
  Surface := SDL_CreateRGBSurfaceFrom(
      ScreenData, ScreenW, ScreenH, 24, RowSize,
      $0000FF, $00FF00, $FF0000, 0);

  //Success := WriteJPGImage(FileName, Surface, 95);
  //Success := WriteBMPImage(FileName, Surface);
  Success := WritePNGImage(FileName, Surface);
  if Success then
    ScreenPopupError.ShowPopup('Screenshot saved: ' + ExtractFileName(FileName))
  else
    ScreenPopupError.ShowPopup('Screenshot failed');

  SDL_FreeSurface(Surface);
  FreeMem(ScreenData);
end;

//------------
// DrawDebugInformation - Procedure draw FPS and some other Informations on Screen
//------------
procedure TDisplay.DrawDebugInformation;
var Ticks: Cardinal;
begin
  //Some White Background for information
  glEnable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
  glColor4f(1, 1, 1, 0.5);
  glBegin(GL_QUADS);
    glVertex2f(690, 44);
    glVertex2f(690, 0);
    glVertex2f(800, 0);
    glVertex2f(800, 44);
  glEnd;
  glDisable(GL_BLEND);

  //Set Font Specs
  SetFontStyle(0);
  SetFontSize(7);
  SetFontItalic(False);
  glColor4f(0, 0, 0, 1);

  //Calculate FPS
  Ticks := SDL_GetTicks();
  if (Ticks >= NextFPSSwap) then
  begin
    LastFPS := FPSCounter * 4;
    FPSCounter := 0;
    NextFPSSwap := Ticks + 250;
  end;

  Inc(FPSCounter);

  //Draw Text

  //FPS
  SetFontPos(695, 0);
  glPrint (PChar('FPS: ' + InttoStr(LastFPS)));

  //RSpeed
  SetFontPos(695, 13);
  glPrint (PChar('RSpeed: ' + InttoStr(Round(1000 * TimeMid))));

  //LastError
  SetFontPos(695, 26);
  glColor4f(1, 0, 0, 1);
  glPrint (PChar(OSD_LastError));

  glColor4f(1, 1, 1, 1);
end;

end.
