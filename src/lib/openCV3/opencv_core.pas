unit opencv_core;

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
  CTypes, SysUtils, opencv_types;

function USDX_cvCreateImage(width: cint; height: cint; depth: cint; channels: cint): PUMatWrapper; cdecl; external;
function cvCreateImage(size: CvSize; depth: integer; channels: integer): PIplImage;

function USDX_cvCloneImage(img: PUMatWrapper): PUMatWrapper; cdecl; external;
function cvCloneImage(img: PIplImage): PIplImage;

procedure USDX_cvReleaseImage(image: PPUMatWrapper); cdecl; external;
procedure cvReleaseImage(image: PPIplImage);

function cvCreateMat(rows: cint; cols: cint; _type: cint): PCvMat; cdecl; external name 'USDX_cvCreateMat';
procedure cvReleaseMat(mat: PPCvMat); cdecl; external name 'USDX_cvReleaseMat';
procedure cvSetReal2D(arr: PCvMat; idx0: cint; idx1: cint; value: cdouble); cdecl; external name 'USDX_cvSetReal2D';
procedure cvSetZero(arr: PCvMat); cdecl; external name 'USDX_cvSetZero';

const
 CV_GEMM_A_T = 1;
 CV_GEMM_B_T = 2;
 CV_GEMM_C_T = 4;

procedure cvGEMM(src1: PCvMat; src2: PCvMat; alpha: cdouble; src3: PCvMat; beta: cdouble; dst: PCvMat; tABC: cint = 0); cdecl; external name 'USDX_cvGEMM';

const
  CV_LU = 0;
  CV_SVD = 1;
  CV_SVD_SYM = 2;
  CV_CHOLESKY = 3;
  CV_QR = 4;
  CV_NORMAL = 16;

function cvInvert(A: PCvMat; B: PCvMat; method: cint = CV_LU): cdouble; cdecl; external name 'USDX_cvInvert';

procedure USDX_cvNot(src: PUMatWrapper; dst: PUMatWrapper); cdecl; external;
procedure cvNot(src: PIplImage; dst: PIplImage); {$IFDEF HasInline}inline;{$ENDIF}
procedure USDX_cvFlip(src: PUMatWrapper; dst: PUMatwrapper; flipmode: cint); cdecl; external;
procedure cvFlip(src: PIplImage; dst: PIplImage = nil; flipmode: integer = 0); {$IFDEF HasInline}inline;{$ENDIF}
procedure USDX_cvAbsDiff(src1: PUMatWrapper; src2: PUMatWrapper; dst: PUMatWrapper); cdecl; external;
procedure cvAbsDiff(src1: PIplImage; src2: PIplImage; dst: PIplImage); {$IFDEF HasInline}inline;{$ENDIF}
procedure USDX_cvTransform(src: PUMatWrapper; dst: PUMatWrapper; transmat: PCvMat; shiftvec: PCvMat); cdecl; external;
procedure cvTransform(src: PIplImage; dst: PIplImage; transmat: PCvMat; shiftvec: PCvMat = nil); {$IFDEF HasInline}inline;{$ENDIF}

function USDX_cvGetDimSize(w: PUMatWrapper; index: cint): cint; cdecl; external;
function cvGetDimSize(img: PIplImage; index: integer): integer; {$IFDEF HasInline}inline;{$ENDIF}


implementation

function cvGetDimSize(img: PIplImage; index: integer): integer; {$IFDEF HasInline}inline;{$ENDIF}
begin
	Result := USDX_cvGetDimSize(img.CppObject, index);
end;

function cvCreateImage(size: CvSize; depth: integer; channels: integer): PIplImage;
var
	w: PUMatWrapper;
begin
	w := USDX_cvCreateImage(size.width, size.height, depth, channels);
	Result := nil;
	if w <> nil then
		Result := PIplImage.Create(w);
end;

function cvCloneImage(img: PIplImage): PIplImage;
var
	w: PUMatWrapper;
begin
	w := USDX_cvCloneImage(img.CppObject);
	Result := nil;
	if w <> nil then
		Result := PIplImage.Create(w);
end;

procedure cvReleaseImage(image: PPIplImage);
begin
	if (image <> nil) and (image^ <> nil) then
	begin
		image^.Free;
		image^ := nil;
	end;
end;

procedure cvNot(src: PIplImage; dst: PIplImage); {$IFDEF HasInline}inline;{$ENDIF}
begin
	USDX_cvNot(src.CppObject, dst.CppObject);
end;

procedure cvFlip(src: PIplImage; dst: PIplImage; flipmode: integer); {$IFDEF HasInline}inline;{$ENDIF}
var
	dstObj: PUMatWrapper;
begin
	dstObj := nil;
	if dst <> nil then
		dstObj := dst.CppObject;
	USDX_cvFlip(src.CppObject, dstObj, flipmode);
end;

procedure cvAbsDiff(src1: PIplImage; src2: PIplImage; dst: PIplImage); {$IFDEF HasInline}inline;{$ENDIF}
begin
	USDX_cvAbsDiff(src1.CppObject, src2.CppObject, dst.CppObject);
end;

procedure cvTransform(src: PIplImage; dst: PIplImage; transmat: PCvMat; shiftvec: PCvMat); {$IFDEF HasInline}inline;{$ENDIF}
begin
	USDX_cvTransform(src.CppObject, dst.CppObject, transmat, shiftvec);
end;

end.
