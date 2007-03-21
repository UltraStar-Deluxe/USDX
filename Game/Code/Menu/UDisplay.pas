unit UDisplay;

interface

uses Windows, SDL, UMenu, OpenGL12, SysUtils;

type
  TDisplay = class
    ActualScreen:     PMenu;
    NextScreen:       PMenu;

    h_DC:     HDC;
    h_RC:     HGLRC;

//    FadeType:   integer;
    FadeTex:    glUInt;
    LastFade:   real;
    Fade:       real;
    function Draw: Boolean;
    procedure PrintScreen;
    constructor Create;
    procedure ScreenShot;
  end;

var
  Display:          TDisplay;
//  ActualScreen:     PMenu;
//  NextScreen:       PMenu;

implementation

uses UGraphic, UTime, Graphics, Jpeg, UPliki, UTexture;

constructor TDisplay.Create;
begin
  inherited Create;
//  FadeType := 0;
  Fade := 0;
end;

function TDisplay.Draw: Boolean;
var
  S:    integer;
  Col:  real;
  Surface: PSDL_Surface;
begin
  Result := True;

  Col := 1;
  if (ParamStr(1) = '-black') or (ParamStr(1) = '-fsblack') then
    Col := 0;

  glClearColor(Col, Col, Col , 1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  for S := 1 to Screens do begin
    ScreenAct := S;

//    if Screens = 1 then ScreenX := 0;
//    if (Screens = 2) and (S = 1) then ScreenX := -1;
//    if (Screens = 2) and (S = 2) then ScreenX := 1;
    ScreenX := 0;


    if S = 2 then TimeSkip := 0; // it's easier than rewriting code
    glViewPort((S-1) * ScreenW div Screens, 0, ScreenW div Screens, ScreenH);

    ActualScreen.SetAnimationProgress(1);
    if not assigned (NextScreen) then Result := ActualScreen.Draw
    else begin
      LastFade := Fade;
      Fade := Fade + TimeSkip * 6; // * 4

      {//Create Fading texture
      if Fade = 0 then
      begin
        Surface := SDL_GetVideoSurface;
        glGenTextures(1, FadeTex);

        glBindTexture(GL_TEXTURE_2D, FadeTex);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glPixelStorei(GL_UNPACK_ROW_LENGTH, Surface.pitch div Surface.format.BytesPerPixel);
        glTexImage2D(GL_TEXTURE_2D, 0, 3, 800, 600, 0, GL_RGBA, GL_UNSIGNED_BYTE, Surface.pixels);
        glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
      end;}

      ActualScreen.ShowFinish := false;

      //Fade = 2 (Belnding) Mod
      if (FadeTex > 0) then
      begin
        ActualScreen.SetAnimationProgress(Fade-1);
        ActualScreen.Draw;
        glBindTexture(GL_TEXTURE_2D, FadeTex);
        glColor4f(Col, Col, Col, 1-Fade); // 0
        glEnable(GL_BLEND);
        glBegin(GL_QUADS);
          glVertex2f(0,   0);
          glVertex2f(0,   600);
          glVertex2f(800, 600);
          glVertex2f(800, 0);
        glEnd;
        glDisable(GL_BLEND);
      end
      else
      begin
      case ActualScreen.Fade of
      0:  begin
            if Fade < 1 then begin
              ActualScreen.SetAnimationProgress(1-Fade);
              ActualScreen.Draw;
              glColor4f(Col, Col, Col, Fade); // 0
            end else begin
              ActualScreen.SetAnimationProgress(Fade-1);
              ActualScreen.Draw;
              glColor4f(Col, Col, Col, 1-Fade); // 0
            end;
            glEnable(GL_BLEND);
            glBegin(GL_QUADS);
              glVertex2f(0,   0);
              glVertex2f(0,   600);
              glVertex2f(800, 600);
              glVertex2f(800, 0);
            glEnd;
            glDisable(GL_BLEND);
          end;
      2:  begin
            if Fade < 1 then begin
              ActualScreen.SetAnimationProgress(1-Fade);
              ActualScreen.Draw;
              //glColor4f(Col, Col, Col, Fade); // 0
              glColor4f(1, 1, 1, 1); // 0
              //Fade := 1
            end;
            glEnable(GL_BLEND);
            glBegin(GL_QUADS);
              glVertex2f(0,   0);
              glVertex2f(0,   600);
              glVertex2f(800, 600);
              glVertex2f(800, 0);
            glEnd;
            glDisable(GL_BLEND);
          end;
      end; // case
      end;

      if (LastFade < 1 ) and (Fade >= 1) then begin
        if (ActualScreen.Fade = 2) then
        begin
          ScreenShot;
          //Create Fading Texture
          Surface := SDL_GetVideoSurface;
          glGenTextures(1, FadeTex);

          glBindTexture(GL_TEXTURE_2D, FadeTex);
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
          glPixelStorei(GL_UNPACK_ROW_LENGTH, Surface.pitch div Surface.format.BytesPerPixel);
          glTexImage2D(GL_TEXTURE_2D, 0, 3, 800, 600, 0, GL_RGBA, GL_UNSIGNED_BYTE, Surface.pixels);
          glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
        end;

        // pokazuje 2 ekran, ale nie kasuje informacji o NextScreen
        ActualScreen.onHide;
        ActualScreen := NextScreen;
        ActualScreen.onShow;
      end;

      if Fade >= 2 then begin
        if (FadeTex > 0) then //Delete Fade Tex
        begin
          glDeleteTextures(1, @FadeTex);
          FadeTex := 0;
        end;

        // koniec fade'a
        ActualScreen := NextScreen;
        NextScreen := nil;
        ActualScreen.onShowFinish;
        ActualScreen.ShowFinish := true;
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
