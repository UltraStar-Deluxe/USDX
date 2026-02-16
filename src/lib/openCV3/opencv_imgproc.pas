unit opencv_imgproc;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

{$IFNDEF OpenCVWrapperDLL}
  {$L ApiWrapper.o}
{$ELSE}
  {$IFNDEF OpenCVWrapperManualLink}
    {$LINKLIB opencvwrapper}
  {$ENDIF}
{$ENDIF}
{$IFDEF OpenCVImgprocStandalone}
  {$IFNDEF OpenCVManualLink}
    {$LINKLIB opencv_imgproc}
  {$ENDIF}
{$ELSE}
  {$LINKLIB opencv_world}
{$ENDIF}

uses
  CTypes, SysUtils, opencv_types;

const
 CV_BLUR_NO_SCALE =0;
 CV_BLUR          =1;
 CV_GAUSSIAN      =2;
 CV_MEDIAN        =3;
 CV_BILATERAL     =4;


procedure USDX_cvSmooth(src, dst: PUMatWrapper; smoothtype: cint; size1: cint; size2: cint; sigma1: cdouble; sigma2: cdouble); cdecl; external;
procedure cvSmooth(src, dst: PIplImage; smoothtype: integer = CV_GAUSSIAN; size1: integer = 3; size2: integer = 0; sigma1: double = 0; sigma2: double = 0); {$IFDEF HasInline}inline;{$ENDIF}

const
  CV_BGR2BGRA  =  0;
  CV_RGB2RGBA  =  CV_BGR2BGRA;

  CV_BGRA2BGR  =  1;
  CV_RGBA2RGB  =  CV_BGRA2BGR;

  CV_BGR2RGBA  =  2;
  CV_RGB2BGRA  =  CV_BGR2RGBA;

  CV_RGBA2BGR  =  3;
  CV_BGRA2RGB  =  CV_RGBA2BGR;

  CV_BGR2RGB   =  4;
  CV_RGB2BGR   =  CV_BGR2RGB;

  CV_BGRA2RGBA =  5;
  CV_RGBA2BGRA =  CV_BGRA2RGBA;

  CV_BGR2GRAY  =  6;
  CV_RGB2GRAY  =  7;
  CV_GRAY2BGR  =  8;
  CV_GRAY2RGB  =  CV_GRAY2BGR;
  CV_GRAY2BGRA =  9;
  CV_GRAY2RGBA =  CV_GRAY2BGRA;
  CV_BGRA2GRAY =  10;
  CV_RGBA2GRAY =  11;

  CV_BGR2BGR565 = 12;
  CV_RGB2BGR565 = 13;
  CV_BGR5652BGR = 14;
  CV_BGR5652RGB = 15;
  CV_BGRA2BGR565= 16;
  CV_RGBA2BGR565 =17;
  CV_BGR5652BGRA =18;
  CV_BGR5652RGBA =19;

  CV_GRAY2BGR565 =20;
  CV_BGR5652GRAY =21;

  CV_BGR2BGR555  =22;
  CV_RGB2BGR555  =23;
  CV_BGR5552BGR  =24;
  CV_BGR5552RGB  =25;
  CV_BGRA2BGR555 =26;
  CV_RGBA2BGR555 =27;
  CV_BGR5552BGRA =28;
  CV_BGR5552RGBA =29;

  CV_GRAY2BGR555 =30;
  CV_BGR5552GRAY =31;

  CV_BGR2XYZ     =32;
  CV_RGB2XYZ     =33;
  CV_XYZ2BGR     =34;
  CV_XYZ2RGB     =35;

  CV_BGR2YCrCb   =36;
  CV_RGB2YCrCb   =37;
  CV_YCrCb2BGR   =38;
  CV_YCrCb2RGB   =39;

  CV_BGR2HSV     =40;
  CV_RGB2HSV     =41;

  CV_BGR2Lab     =44;
  CV_RGB2Lab     =45;

  CV_BayerBG2BGR =46;
  CV_BayerGB2BGR =47;
  CV_BayerRG2BGR =48;
  CV_BayerGR2BGR =49;

  CV_BayerBG2RGB =CV_BayerRG2BGR;
  CV_BayerGB2RGB =CV_BayerGR2BGR;
  CV_BayerRG2RGB =CV_BayerBG2BGR;
  CV_BayerGR2RGB =CV_BayerGB2BGR;

  CV_BGR2Luv     =50;
  CV_RGB2Luv     =51;
  CV_BGR2HLS     =52;
  CV_RGB2HLS     =53;

  CV_HSV2BGR     =54;
  CV_HSV2RGB     =55;

  CV_Lab2BGR     =56;
  CV_Lab2RGB     =57;
  CV_Luv2BGR     =58;
  CV_Luv2RGB     =59;
  CV_HLS2BGR     =60;
  CV_HLS2RGB     =61;

  CV_COLORCVT_MAX  =100;

procedure USDX_cvCvtColor(src: PUMatWrapper; dst: PUMatWrapper; code: cint); cdecl; external;
procedure cvCvtColor(src: PIplImage; dst: PIplImage; code: integer); {$IFDEF HasInline}inline;{$ENDIF}
procedure USDX_cvEqualizeHist(src: PUMatWrapper; dst: PUMatWrapper); cdecl; external;
procedure cvEqualizeHist(src: PIplImage; dst: PIplImage); {$IFDEF HasInline}inline;{$ENDIF}
procedure USDX_cvCanny(image: PUMatWrapper; edges: PUMatWrapper; threshold1: cdouble; threshold2: cdouble; aperture_size: cint); cdecl; external;
procedure cvCanny(image: PIplImage; edges: PIplImage; threshold1: double; threshold2: double; aperture_size: integer = 3); {$IFDEF HasInline}inline;{$ENDIF}
procedure USDX_cvDilate(src: PUMatWrapper; dst: PUMatWrapper; element: pointer; iterations: cint); cdecl; external;
procedure cvDilate(src: PIplImage; dst: PIplImage; element: pointer = nil; iterations: integer = 1); {$IFDEF HasInline}inline;{$ENDIF}
procedure USDX_cvErode(src: PUMatWrapper; dst: PUMatWrapper; element: pointer; iterations: cint); cdecl; external;
procedure cvErode(src: PIplImage; dst: PIplImage; element: pointer = nil; iterations: integer = 1); {$IFDEF HasInline}inline;{$ENDIF}

const
 CV_THRESH_BINARY     = 0;
 CV_THRESH_BINARY_INV = 1;
 CV_THRESH_TRUNC      = 2;
 CV_THRESH_TOZERO     = 3;
 CV_THRESH_TOZERO_INV = 4;
 CV_THRESH_MASK       = 7;
 CV_THRESH_OTSU       = 8;

procedure USDX_cvThreshold(src: PUMatWrapper; dst: PUMatWrapper; threshold: cdouble; max_value: cdouble; threshold_type: cint); cdecl; external;
procedure cvThreshold(src: PIplImage; dst: PIplImage; threshold: double; max_value: double; threshold_type: integer); {$IFDEF HasInline}inline;{$ENDIF}

implementation

procedure cvSmooth(src, dst: PIplImage; smoothtype: integer; size1: integer; size2: integer; sigma1: double; sigma2: double); {$IFDEF HasInline}inline;{$ENDIF}
begin
	USDX_cvSmooth(src.CppObject, dst.CppObject, smoothtype, size1, size2, sigma1, sigma2);
end;

procedure cvCvtColor(src: PIplImage; dst: PIplImage; code: integer); {$IFDEF HasInline}inline;{$ENDIF}
begin
	USDX_cvCvtColor(src.CppObject, dst.CppObject, code);
end;

procedure cvEqualizeHist(src: PIplImage; dst: PIplImage); {$IFDEF HasInline}inline;{$ENDIF}
begin
	USDX_cvEqualizeHist(src.CppObject, dst.CppObject);
end;

procedure cvCanny(image: PIplImage; edges: PIplImage; threshold1: double; threshold2: double; aperture_size: integer); {$IFDEF HasInline}inline;{$ENDIF}
begin
	USDX_cvCanny(image.CppObject, edges.CppObject, threshold1, threshold2, aperture_size);
end;

procedure cvDilate(src: PIplImage; dst: PIplImage; element: pointer; iterations: integer); {$IFDEF HasInline}inline;{$ENDIF}
begin
	USDX_cvDilate(src.CppObject, dst.CppObject, element, iterations);
end;

procedure cvErode(src: PIplImage; dst: PIplImage; element: pointer; iterations: integer); {$IFDEF HasInline}inline;{$ENDIF}
begin
	USDX_cvErode(src.CppObject, dst.CppObject, element, iterations);
end;

procedure cvThreshold(src: PIplImage; dst: PIplImage; threshold: double; max_value: double; threshold_type: integer); cdecl; {$IFDEF HasInline}inline;{$ENDIF}
begin
	USDX_cvThreshold(src.CppObject, dst.CppObject, threshold, max_value, threshold_type);
end;

end.

