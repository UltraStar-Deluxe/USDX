unit opencv_types;

interface
{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

{$L ApiWrapper.o}
{$IFDEF OpenCVCoreStandalone}
  {$LINKLIB opencv_core}
{$ELSE}
  {$LINKLIB opencv_world}
{$ENDIF}

uses
  CTypes;

type
  PPUMatWrapper = ^PUMatWrapper;
  PUMatWrapper = pointer;

  PPIplImage = ^PIplImage;
  PIplImage = class
    public
      CppObject: PUMatWrapper;
      constructor Create(_CppObject: PUMatWrapper);
      destructor Destroy; override;
      function imageData: pointer; {$IFDEF HasInline}inline;{$ENDIF}
      function width: integer; {$IFDEF HasInline}inline;{$ENDIF}
      function height: integer; {$IFDEF HasInline}inline;{$ENDIF}
      function depth: integer; {$IFDEF HasInline}inline;{$ENDIF}
  end;

  PPCvMat = ^PCvMat;
  PCvMat = pointer;

  PCvSize = ^CvSize;
  CvSize = record
    width: integer;
    height: integer;
  end;

function CvSizeV(p_width, p_height: integer):CvSize; overload;
function CvSizeV(p_width, p_height: extended):CvSize; overload;

function Get_UMat_depth(w: PUMatWrapper): cint; cdecl; external;
function Get_UMat_as_8UC3(w: PUMatWrapper): pointer; cdecl; external;

// The type casts needed for the old API are not needed/supported by this wrapper.
// These functions pass through their parameter unchanged to allow the type casts
// to stay in place without sacrificing the type checking.
function PCvArr(a: PIplImage): PIplImage; overload; {$IFDEF HasInline}inline;{$ENDIF}
function PCvArr(a: PCvMat): PCvMat; overload; {$IFDEF HasInline}inline;{$ENDIF}

const
  CV_CN_MAX     = 64;
  CV_CN_SHIFT   = 3;
  CV_DEPTH_MAX  = (1 shl CV_CN_SHIFT);

  CV_8U   = 0;
  CV_8S   = 1;
  CV_16U  = 2;
  CV_16S  = 3;
  CV_32S  = 4;
  CV_32F  = 5;
  CV_64F  = 6;
  CV_16F  = 7;

  CV_MAT_DEPTH_MASK = CV_DEPTH_MAX - 1;

  CV_32FC1 = ((CV_32F) and CV_MAT_DEPTH_MASK) + (((1)-1) shl CV_CN_SHIFT);

implementation

uses opencv_core;

constructor PIplImage.Create(_CppObject: PUMatWrapper);
begin
  inherited Create;
  CppObject := _CppObject;
end;

destructor PIplImage.Destroy;
begin
	USDX_cvReleaseImage(@CppObject);
	inherited;
end;

function PIplImage.width: integer; {$IFDEF HasInline}inline;{$ENDIF}
begin
  Result := cvGetDimSize(self, 1);
end;

function PIplImage.height: integer; {$IFDEF HasInline}inline;{$ENDIF}
begin
  Result := cvGetDimSize(self, 0);
end;

function PIplImage.depth: integer; {$IFDEF HasInline}inline;{$ENDIF}
begin
  Result := Get_UMat_depth(CppObject);
  // Result := cvIplDepth(cvGetElemType(self));
end;

function PIplImage.imageData: pointer; {$IFDEF HasInline}inline;{$ENDIF}
begin
  Result := Get_UMat_as_8UC3(CppObject);
end;

function CvSizeV(p_width,p_height: integer):CvSize;
begin
  result.width:=p_width;
  Result.height:= p_height;
end;

function CvSizeV(p_width,p_height: extended):CvSize; overload;
begin
  result.width  := round(p_width);
  Result.height := round(p_height);
end;

function PCvArr(a: PIplImage): PIplImage; overload; {$IFDEF HasInline}inline;{$ENDIF}
begin
	Result := a;
end;

function PCvArr(a: PCvMat): PCvMat; overload; {$IFDEF HasInline}inline;{$ENDIF}
begin
	Result := a;
end;

end.
