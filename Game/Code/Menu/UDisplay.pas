unit UDisplay;

interface

uses Windows, SDL, UMenu, OpenGL12, SysUtils, dialogs;

type
  TDisplay = class
    ActualScreen:     PMenu;
    NextScreen:       PMenu;

    h_DC:     HDC;
    h_RC:     HGLRC;

    Fade: Real;
    // fade-mod
    doFade: Boolean;
    canFade: Boolean;
    myFade: integer;
    lastTime: Cardinal;
    pTexData : Pointer;
    pTex : array[1..2] of glUInt;
    // end

    function Draw: Boolean;
    procedure PrintScreen;
    constructor Create;
    // fade mod
    destructor Destroy;
    // end
    procedure ScreenShot;
  end;

var
  Display:          TDisplay;
//  ActualScreen:     PMenu;
//  NextScreen:       PMenu;

implementation

uses UGraphic, UTime, Graphics, Jpeg, UPliki, UTexture, UIni;

constructor TDisplay.Create;
var i: integer;
begin
  inherited Create;

  // fade mod
  myfade:=0;

  if Ini.ScreenFade=1 then
    doFade:=True
  else
    doFade:=False;

  canFade:=True;
  // generate texture for fading between screens
  GetMem(pTexData, 1024*1024*3);
  if pTexData <> NIL then
  for i:= 1 to 2 do
  begin
    glGenTextures(1, pTex[i]);
    if glGetError <> GL_NO_ERROR then canFade := False;
    glBindTexture(GL_TEXTURE_2D, pTex[i]);
    if glGetError <> GL_NO_ERROR then canFade := False;
    glTexImage2D(GL_TEXTURE_2D, 0, 3, 1024, 1024, 0, GL_RGB, GL_UNSIGNED_BYTE, pTexData);
    if glGetError <> GL_NO_ERROR then canFade := False;
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    if glGetError <> GL_NO_ERROR then canFade := False;
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    if glGetError <> GL_NO_ERROR then canFade := False;
  end
  else
  begin
    canFade:=False;
  end;
  FreeMem(pTexData);
  if not canFade then begin
    showmessage('Fehler beim Initialisieren der Fading-Textur... Fading deaktiviert');
    doFade:=False;
  end
  // end
end;

// fade mod
destructor TDisplay.Destroy;
begin
  if canFade then
    glDeleteTextures(1,@pTex);
  inherited Destroy;
end;
// end

function TDisplay.Draw: Boolean;
var
  S:    integer;
  Col: Real;
  // fade mod
  myFade2: Real;
  currentTime: Cardinal;
  // end
begin
  Result := True;

  Col := 1;
  if (ParamStr(1) = '-black') or (ParamStr(1) = '-fsblack') then
    Col := 0;

  glClearColor(Col, Col, Col , 0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  for S := 1 to Screens do begin
    ScreenAct := S;

//    if Screens = 1 then ScreenX := 0;
//    if (Screens = 2) and (S = 1) then ScreenX := -1;
//    if (Screens = 2) and (S = 2) then ScreenX := 1;
    ScreenX := 0;


    glViewPort((S-1) * ScreenW div Screens, 0, ScreenW div Screens, ScreenH);

//    ActualScreen.SetAnimationProgress(1);
    if not assigned (NextScreen) then begin
      Result := ActualScreen.Draw;
      // fade mod
      myfade:=0;
      if (Ini.ScreenFade=1) and canFade then
        doFade:=True
      else if Ini.ScreenFade=0 then
        doFade:=False;
      // end
    end
    else
    begin
      if doFade and canFade then
      begin
        // fade mod
        //Create Fading texture if we're just starting
        if myfade = 0 then
        begin
          ActualScreen.Draw;
          glBindTexture(GL_TEXTURE_2D, pTex[S]);
          glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, (S-1)*ScreenW div Screens, 0, 1024, 1024, 0);
          if glGetError <> GL_NO_ERROR then
          begin
            canFade := False;
            showmessage('Fehler beim Kopieren der Fade-Textur... Fading deaktiviert');
          end;
          NextScreen.onShow;
          lastTime:=GetTickCount;
          if (S=2) or (Screens = 1) then
            myfade:=myfade+1;
        end; // end texture creation in first fading step

        //do some time-based fading
        currentTime:=GetTickCount;
        if (currentTime > lastTime+30) and (S=1) then
        begin
          myfade:=myfade+4;
          lastTime:=currentTime;
        end;

//      LastFade := Fade;   // whatever
//      Fade := Fade -0.999; // start fading out


//      ActualScreen.ShowFinish := false; // no purpose?

//      ActualScreen.SetAnimationProgress(Fade-1); // nop?

        NextScreen.Draw; // draw next screen

      // and draw old screen over it... slowly fading out
        myfade2:=(myfade*myfade)/10000;
        glBindTexture(GL_TEXTURE_2D, pTex[S]);
        glColor4f(1, 1, 1, (1000-myfade*myfade)/1000); // strange calculation - alpha gets negative... but looks good this way
        glEnable(GL_TEXTURE_2D);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glEnable(GL_BLEND);
        glBegin(GL_QUADS);
          glTexCoord2f(0+myfade2,0+myfade2);glVertex2f(0,   600);
          glTexCoord2f(0+myfade2,ScreenH/1024-myfade2);glVertex2f(0,   0);
          glTexCoord2f((ScreenW div Screens)/1024-myfade2,ScreenH/1024-myfade2);glVertex2f(800, 0);
          glTexCoord2f((ScreenW div Screens)/1024-myfade2,0+myfade2);glVertex2f(800, 600);
        glEnd;
        glDisable(GL_BLEND);
        glDisable(GL_TEXTURE_2D);
      end
        else NextScreen.OnShow;


      if (myfade > 40) or (not doFade) or (not canFade) then begin // fade out complete...
        myFade:=0;
        ActualScreen.onHide;
        ActualScreen.ShowFinish:=False;
        ActualScreen:=NextScreen;
        NextScreen := nil;
        ActualScreen.onShowFinish;
        ActualScreen.ShowFinish := true;
      // end of fade mod
      end;
    end; // if
  end; // for
//  SwapBuffers(h_DC);
end;

{function TDisplay.Fade(FadeIn : Boolean; Steps : UInt8): UInt8;
begin
  Self.FadeIn := FadeIn;
  FadeStep := (SizeOf(FadeStep) * $FF) div Steps;
  ActualStep := $FF;
  Result := $FF div FadeStep;
end;}

procedure TDisplay.PrintScreen;
var
  Bitmap:     TBitmap;
  Jpeg:       TJpegImage;
  X, Y:       integer;
  Num:        integer;
  FileName:   string;
begin
  for Num := 1 to 9999 do begin
    FileName := IntToStr(Num);
    while Length(FileName) < 4 do FileName := '0' + FileName;
    FileName := ScreenshotsPath + 'screenshot' + FileName + '.jpg';
    if not FileExists(FileName) then break
  end;

  glReadPixels(0, 0, ScreenW, ScreenH, GL_RGBA, GL_UNSIGNED_BYTE, @PrintScreenData[0]);
  Bitmap := TBitmap.Create;
  Bitmap.Width := ScreenW;
  Bitmap.Height := ScreenH;

  for Y := 0 to ScreenH-1 do
    for X := 0 to ScreenW-1 do
      Bitmap.Canvas.Pixels[X, Y] := PrintScreenData[(ScreenH-1-Y) * ScreenW + X] and $00FFFFFF;

  Jpeg := TJpegImage.Create;
  Jpeg.Assign(Bitmap);
  Bitmap.Free;
  Jpeg.CompressionQuality := 95;//90;
  ForceDirectories(ScreenshotsPath);
  Jpeg.SaveToFile(FileName);
  Jpeg.Free;
end;

procedure TDisplay.ScreenShot;
 var F : file;
     FileInfo: BITMAPINFOHEADER;
     FileHeader : BITMAPFILEHEADER;
     pPicData:Pointer;
     FileName: String;
     Num: Integer;
begin
  //bilddatei Suchen
  for Num := 1 to 9999 do begin
    FileName := IntToStr(Num);
    while Length(FileName) < 4 do FileName := '0' + FileName;
    FileName := {ScreenshotsPath + }'screenshot' + FileName + '.BMP';
    if not FileExists(FileName) then break
  end;

 //Speicher für die Speicherung der Header-Informationen vorbereiten
 ZeroMemory(@FileHeader, SizeOf(BITMAPFILEHEADER));
 ZeroMemory(@FileInfo, SizeOf(BITMAPINFOHEADER));
 
 //Initialisieren der Daten des Headers
 FileHeader.bfType := 19778; //$4D42 = 'BM'
 FileHeader.bfOffBits := SizeOf(BITMAPINFOHEADER)+SizeOf(BITMAPFILEHEADER);
 
 //Schreiben der Bitmap-Informationen
 FileInfo.biSize := SizeOf(BITMAPINFOHEADER);
 FileInfo.biWidth := ScreenH;
 FileInfo.biHeight := ScreenW;
 FileInfo.biPlanes := 1;
 FileInfo.biBitCount := 32;
 FileInfo.biSizeImage := FileInfo.biWidth*FileInfo.biHeight*(FileInfo.biBitCount div 8);
 
 //Größenangabe auch in den Header übernehmen
 FileHeader.bfSize := FileHeader.bfOffBits + FileInfo.biSizeImage;
 
 //Speicher für die Bilddaten reservieren
 GetMem(pPicData, FileInfo.biSizeImage);
 try
  //Bilddaten von OpenGL anfordern (siehe oben)
  glReadPixels(0, 0, ScreenW, ScreenH, GL_BGRA, GL_UNSIGNED_BYTE, pPicData);
 
  //Und den ganzen Müll in die Datei schieben ;-)
  //Moderne Leute nehmen dafür auch Streams ...
  AssignFile(f, Filename);
  Rewrite( f,1 );
  try
   BlockWrite(F, FileHeader, SizeOf(BITMAPFILEHEADER));
   BlockWrite(F, FileInfo, SizeOf(BITMAPINFOHEADER));
   BlockWrite(F, pPicData^, FileInfo.biSizeImage );
  finally
   CloseFile(f);
  end;
 finally
  //Und den angeforderten Speicher wieder freigeben ...
  FreeMem(pPicData, FileInfo.biSizeImage);
 end;
end;


end.
