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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/base/UPlaylist.pas $
 * $Id: $
 *}

unit UWebcam;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
  UTexture,
  opencv_highgui,
  opencv_core,
  opencv_imgproc,
  opencv_types;

type

  TWebcam = class
    private
      LastTickFrame: integer;
      LastFrame:     PIplImage;
      RGBFrame:      PIplImage;

    public
      Capture: PCvCapture;
      TextureCam: TTexture;

      constructor Create;
      procedure Release;
      procedure Restart;
      procedure GetWebcamFrame();
      function FrameEffect(Nr_Effect: integer; Frame: PIplImage): PIplImage;
      function FrameAdjust(Frame: PIplImage): PIplImage;
   end;

var
  Webcam:    TWebcam;
  IsEnabled: Boolean;


implementation

uses
  dglOpenGL,
  SysUtils,
  sdl2,
  ULog,
  UIni;

//----------
//Create - Construct Class - Dummy for now
//----------
constructor TWebcam.Create;
var
  H, W, I: integer;
  s: string;
begin
  inherited;

  if (Ini.WebCamID <> 0) then
  begin
    try
      Capture := cvCreateCameraCapture(Ini.WebCamID - 1);
      IsEnabled := true;
    except
      IsEnabled:=false;
    end;

    if (IsEnabled = true) and (Capture <> nil) then
    begin
      S := IWebcamResolution[Ini.WebcamResolution];

      I := Pos('x', S);
      W := StrToInt(Copy(S, 1, I-1));
      H := StrToInt(Copy(S, I+1, 1000));

      cvSetCaptureProperty(Capture, CV_CAP_PROP_FRAME_WIDTH, W);
      cvSetCaptureProperty(Capture, CV_CAP_PROP_FRAME_HEIGHT, H);
    end;
  end;

end;

procedure TWebcam.Release;
begin
  try
  if (IsEnabled = true) and (Capture <> nil) then
    cvReleaseCapture(@Capture);
  except
    ;
  end;
  IsEnabled:=false;
end;

procedure TWebcam.Restart;
begin
  Release;
  try
    Webcam := TWebcam.Create;
  except
    ;
  end;
end;

procedure TWebcam.GetWebcamFrame();
var
  WebcamFrame: PIplImage;
begin
  if (IsEnabled = true) and((SDL_GetTicks() - LastTickFrame) >= 1000/StrToInt(IWebcamFPS[Ini.WebCamFPS])) then
  begin
    if (TextureCam.TexNum > 0) then
    begin
      glDeleteTextures(1, PGLuint(@TextureCam.TexNum));
      TextureCam.TexNum := 0;
    end;

    WebcamFrame := cvQueryFrame(Capture);

    if (Ini.WebCamFlip = 0) then
      cvFlip(WebcamFrame, nil, 1);

    WebcamFrame := FrameAdjust(WebcamFrame);
    WebcamFrame := FrameEffect(Ini.WebCamEffect, WebcamFrame);

    TextureCam := Texture.CreateTexture(WebcamFrame.imageData, nil, WebcamFrame.Width, WebcamFrame.Height, WebcamFrame.depth);

    WebcamFrame := nil;
    cvReleaseImage(@WebcamFrame);
    cvReleaseImage(@RGBFrame);

    LastTickFrame := SDL_GetTicks();

    // wait for a key
    cvWaitKey(0);
  end;

end;

// 0  -> NORMAL
// 1  -> GRAYSCALE
// 2  -> BLACK & WHITE
// 3  -> NEGATIVE
// 4  -> BINARY IMAGE
// 5  -> DILATE
// 6  -> THRESHOLD
// 7  -> EDGES
// 8  -> GAUSSIAN BLUR
// 9  -> EQUALIZED
// 10 -> ERODE
function TWebcam.FrameEffect(Nr_Effect: integer; Frame: PIplImage): PIplImage;
var
  Size: CvSize;
  HalfSize: CvSize;
  CamEffectParam: integer;
  ImageFrame, EffectFrame, DiffFrame: PIplImage;
begin

  // params values
  case Nr_Effect of
    4: CamEffectParam := 20; // binary image
    5: CamEffectParam := 2;  // dilate
    6: CamEffectParam := 60; // threshold
    7: CamEffectParam := 70; // edges
    8: CamEffectParam := 11; // gaussian blur
   10: CamEffectParam := 2; // erode
   else
      CamEffectParam := 0;
  end;

  Size  := cvSizeV(Frame.width, Frame.height);
  HalfSize  := cvSizeV(Frame.width/2, Frame.height/2);

  ImageFrame := cvCreateImage(Size, Frame.depth, 1);
  EffectFrame := cvCreateImage(Size, Frame.depth, 1);
  DiffFrame := cvCreateImage (Size, Frame.depth, 1);
  RGBFrame := cvCreateImage(Size, Frame.depth, 3);

  case Nr_Effect of
    1: begin // Grayscale
         cvCvtColor(Frame, EffectFrame, CV_BGR2GRAY);
         cvCvtColor(EffectFrame, RGBFrame, CV_GRAY2RGB);
         Result := RGBFrame;
       end;

    2: begin // Black & White
         cvCvtColor(Frame, ImageFrame, CV_BGR2GRAY );
         cvThreshold(ImageFrame, EffectFrame, 128, 255, CV_THRESH_OTSU);
         cvCvtColor(EffectFrame, RGBFrame, CV_GRAY2RGB);
         Result := RGBFrame;
       end;

    3: begin // Negative
         cvCvtColor(Frame, RGBFrame, CV_BGR2RGB);
         cvNot(RGBFrame, RGBFrame);
         Result := RGBFrame;
       end;

    4: begin // Binary Image

         //Convert frame to gray and store in image
         cvCvtColor(Frame, ImageFrame, CV_BGR2GRAY);
         cvEqualizeHist(ImageFrame, ImageFrame);

         //Copy Image
         if(LastFrame = nil) then
           LastFrame := cvCloneImage(ImageFrame);

         //Differences with actual and last image
         cvAbsDiff(ImageFrame, LastFrame, DiffFrame);

         //threshold image
         cvThreshold(DiffFrame, EffectFrame, CamEffectParam, 255, 0);

         cvReleaseImage(@LastFrame);

         //Change datas;
         LastFrame := cvCloneImage(ImageFrame);

         cvCvtColor(EffectFrame, RGBFrame, CV_GRAY2RGB);
         Result := RGBFrame;
       end;

    5: begin // Dilate
         cvDilate(Frame, Frame, nil, CamEffectParam);
         cvCvtColor(Frame, RGBFrame, CV_BGR2RGB);
         Result := RGBFrame;
       end;

    6: begin //threshold
         cvCvtColor(Frame, ImageFrame, CV_BGR2GRAY);
         cvThreshold(ImageFrame, EffectFrame, CamEffectParam, 100, 3);
         cvCvtColor(EffectFrame, RGBFrame, CV_GRAY2RGB);
         Result := RGBFrame;
       end;

    7: begin // Edges
         cvCvtColor(Frame, ImageFrame, CV_BGR2GRAY);
         cvCanny(ImageFrame, EffectFrame, CamEffectParam, CamEffectParam, 3);
         cvCvtColor(EffectFrame, RGBFrame, CV_GRAY2RGB);
         Result := RGBFrame;
       end;

    8: begin // Gaussian Blur
         cvSmooth(Frame, Frame, CV_BLUR, CamEffectParam, CamEffectParam);
         cvCvtColor(Frame, RGBFrame, CV_BGR2RGB);
         Result := RGBFrame;
       end;

    9: begin // Equalized
         cvCvtColor(Frame, ImageFrame, CV_BGR2GRAY);
         cvEqualizeHist(ImageFrame, EffectFrame);
         cvCvtColor(EffectFrame, RGBFrame, CV_GRAY2RGB);
         Result := RGBFrame;
       end;

    10: begin // Erode
         cvErode(Frame, Frame, nil, CamEffectParam);
         cvCvtColor(Frame, RGBFrame, CV_BGR2RGB);
         Result := RGBFrame;
       end;
    {
    11:begin // Color
         RGBFrame := cvCreateImage(Size, Frame.depth, 3);

         cvAddS(Frame, CV_RGB(255, 0, 0), Frame);
         cvCvtColor(Frame, RGBFrame, CV_BGR2RGB);
         Result := RGBFrame;
       end;

    12:begin // Value
         ImageFrame := cvCreateImage(Size, Frame.depth, 1);
         RGBFrame := cvCreateImage(Size, Frame.depth, 3);

         // Convert from Red-Green-Blue to Hue-Saturation-Value
         cvCvtColor(Frame, RGBFrame, CV_BGR2HSV );

         // Split hue, saturation and value of hsv on them
         cvSplit(RGBFrame, nil, nil, ImageFrame, 0);
         cvCvtColor(ImageFrame, RGBFrame, CV_GRAY2RGB);
         Result := RGBFrame;
       end;
       }
    else
    begin
      //normal
      cvCvtColor(Frame, RGBFrame, CV_BGR2RGB);
      Result := RGBFrame;
    end;
  end;

  cvReleaseImage(@ImageFrame);
  cvReleaseImage(@DiffFrame);
  cvReleaseImage(@EffectFrame);

end;

function TWebcam.FrameAdjust(Frame: PIplImage): PIplImage;
var
  I, J: integer;
  Size: CvSize;
  HalfSize: CvSize;
  BrightValue, SaturationValue, HueValue: integer;
  BrightValueConvt, SaturationValueConvt, HueValueConvt: real;
  ImageFrame, TmpFrame, HueFrame, SaturationFrame, ValueFrame: PIplImage;
begin

  Size  := cvSizeV(Frame.width, Frame.height);
  HalfSize  := cvSizeV(Frame.width/2, Frame.height/2);

  ImageFrame := cvCreateImage(Size, Frame.depth, 1);
  TmpFrame := cvCreateImage(Size, Frame.depth, 3);

  HueFrame := cvCreateImage(Size, Frame.depth, 1);
  SaturationFrame := cvCreateImage(Size, Frame.depth, 1);
  ValueFrame := cvCreateImage(Size, Frame.depth, 1);

  BrightValue := Ini.WebcamBrightness;

  // Brightness
  if (BrightValue <> 100) then
  begin
    if (BrightValue > 100) then
      BrightValueConvt := (BrightValue - 100) * 255/100
    else
      BrightValueConvt := -((BrightValue - 100) * -255/100);

    cvAddS(Frame, CV_RGB(BrightValueConvt, BrightValueConvt, BrightValueConvt), Frame);
  end;

  SaturationValue := Ini.WebCamSaturation;

  // Saturation
  if (SaturationValue <> 100) then
  begin
    if (SaturationValue > 100) then
      SaturationValueConvt := (SaturationValue - 100) * 255/100
    else
      SaturationValueConvt := -((SaturationValue - 100) * -255/100);

    // Convert from Red-Green-Blue to Hue-Saturation-Value
//    cvCvtColor(Frame, TmpFrame, CV_BGR2HSV );

    // Split hue, saturation and value of hsv on them
  //  cvSplit(TmpFrame, nil, ImageFrame, nil, nil);
    //cvCvtColor(ImageFrame, Frame, CV_GRAY2RGB);

    cvConvertScale(Frame, Frame, 10);
    //     cvCvtColor(Frame, RGBFrame, CV_BGR2RGB);

  end;

  HueValue := Ini.WebCamHue;

  // Hue
  if (HueValue <> 180) then
  begin
    if (HueValue > 100) then
      HueValueConvt := (HueValue - 100) * 255/100
    else
      HueValueConvt := -((HueValue - 100) * -255/100);

    // Convert from Red-Green-Blue to Hue-Saturation-Value
    cvCvtColor(Frame, TmpFrame, CV_BGR2RGB );

    // Split hue, saturation and value of hsv on them
    cvSplit(TmpFrame, HueFrame, SaturationFrame, ValueFrame, nil);
    //cvCvtColor(ImageFrame, Frame, CV_GRAY2RGB);

    cvMerge(SaturationFrame, HueFrame, ValueFrame, nil, Frame);

    // convert back for displaying
    //cvCvtColor(TmpFrame, Frame, CV_BGR2RGB);
  end;

  cvReleaseImage(@ImageFrame);
  cvReleaseImage(@TmpFrame);

  cvReleaseImage(@HueFrame);
  cvReleaseImage(@SaturationFrame);
  cvReleaseImage(@ValueFrame);

  Result := Frame;

  {    11:begin // Contrast
         RGBFrame := cvCreateImage(Size, Frame.depth, 3);

         cvConvertScale(Frame, Frame, CamEffectParam/10);
         cvCvtColor(Frame, RGBFrame, CV_BGR2RGB);
         Result := RGBFrame;
       end;
       }

end;

//----------

end.
