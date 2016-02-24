unit opencv_imgproc;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  SysUtils, opencv_types,
  {$IFDEF WIN32}
    Windows, Dialogs;
  {$ELSE}
    dynlibs;
  {$ENDIF}

///****************************************************************************************\
//*                                    Image Processing                                    *
//\****************************************************************************************/

///* Copies source 2D array inside of the larger destination array and
//   makes a border of the specified type (IPL_BORDER_*) around the copied area. */
var cvCopyMakeBorder: procedure (src,dst:PCvArr; offset: CvPoint;
                             bordertype: integer; value: CvScalar{CV_DEFAULT(cvScalarAll(0)} ); cdecl;

const
 CV_BLUR_NO_SCALE =0;
 CV_BLUR          =1;
 CV_GAUSSIAN      =2;
 CV_MEDIAN        =3;
 CV_BILATERAL     =4;

///* Smoothes array (removes noise) */
var cvSmooth: procedure( src, dst: PIplImage;
                         smoothtype: integer = CV_GAUSSIAN;
                         size1: integer = 3;
                         size2: integer = 0;
                         sigma1: double = 0;
                         sigma2: double = 0);cdecl;

///* Convolves the image with the kernel */
var cvFilter2D: procedure(src: PCvArr; dst:PCvArr; kernel:PCvMat;
                          anchor:CvPoint  );cdecl;

///* Finds integral image: SUM(X,Y) = sum(x<X,y<Y)I(x,y) */
var cvIntegral:procedure(image:PCvArr; sum: PCvArr;
                       sqsum:PCvArr = nil;
                       tilted_sum:PCvArr = nil);cdecl;

///*
//   Smoothes the input image with gaussian kernel and then down-samples it.
//   dst_width = floor(src_width/2)[+1],
//   dst_height = floor(src_height/2)[+1]//*/
var cvPyrDown: procedure(src: PCvArr; dst: PCvArr;
                     filter: integer = 7);cdecl;

///*
//   Up-samples image and smoothes the result with gaussian kernel.
//   dst_width = src_width*2,
//   dst_height = src_height*2//*/
var cvPyrUp: procedure (src: PCvArr; dst: PCvArr;
                    filter: integer = 7);cdecl;


///* Builds pyramid for an image */
var cvCreatePyramid: function(  img: PCvArr; extra_layers: integer; rate:  double;
                                layer_sizes: PCvSize = 0;
                                bufarr: PCvArr = 0;
                                calc: integer = 1;
                                filter: integer = 7 { CV_GAUSSIAN_5x5} ):PPCvMat;
///* Releases pyramid */
var cvReleasePyramid: procedure(pyramid:PPPCvMat; extra_layers: integer);cdecl;



///* Splits color or grayscale image into multiple connected components
//   of nearly the same color/brightness using modification of Burt algorithm.
//   comp with contain a pointer to sequence (CvSeq)
//   of connected components (CvConnectedComp) */
var cvPyrSegmentation: procedure( src: PIplImage; dst:PIplImage;
                              storage: PCvMemStorage; comp:ppCvSeq;
                              level: integer; threshold1:double;
                              threshold2:double  );cdecl;

///* Filters image using meanshift algorithm */
var cvPyrMeanShiftFiltering: procedure(src: PCvArr;dst:PCvArr;
                          sp: double;sr: double;max_level: integer{=1};
                          termcrit: CvTermCriteria);cdecl;

const

///* Constants for color conversion */
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

var
  cvCvtColor: procedure(src: pointer; dst: pointer; code: integer );cdecl;
  cvEqualizeHist: procedure(src: PIplImage; dst: PIplImage);cdecl;

const
 CV_HAAR_DO_CANNY_PRUNING    = 1;
 CV_HAAR_SCALE_IMAGE         = 2;
 CV_HAAR_FIND_BIGGEST_OBJECT = 4;
 CV_HAAR_DO_ROUGH_SEARCH     = 8;

var
  cvHaarDetectObjects : function( image: pointer;
                                  cascade: PCvHaarClassifierCascade;
                                  storage: PCvMemStorage;
                                  scale_factor: double;
                                  min_neighbors: integer;
                                  flags: integer;
                                  min_size: CvSize  ): PCvSeq;cdecl;

const
  CV_INTER_NN       = 0;
  CV_INTER_LINEAR   = 1;
  CV_INTER_CUBIC    = 2;
  CV_INTER_AREA     = 3;

  CV_WARP_FILL_OUTLIERS =8;
  CV_WARP_INVERSE_MAP  =16;

///* Resizes image (input array is resized to fit the destination array) */
var cvResize: procedure ( src: pointer;
                      dst:pointer;
                      interpolation: integer= CV_INTER_LINEAR  ); cdecl;


///* Runs canny edge detector */
var cvCanny: procedure ( image: PIplImage; edges: PIplImage;
                         threshold1: double;
                         threshold2: double;
                         aperture_size: integer = 3);cdecl;

///* dilates input image (applies maximum filter) one or more times.
//   If element pointer is NULL, 3x3 rectangular element is used */
var cvDilate: procedure(src: PIplImage; dst: PIplImage;
                    element: pointer{ PIplConvKernel} = nil;
                    iterations: integer = 1); cdecl;

var cvErode: procedure(src: PIplImage; dst: PIplImage;
                    element: pointer{ PIplConvKernel} = nil;
                    iterations: integer = 1); cdecl;

var cvLaplace: procedure (src: PCvArr; dst: PCvArr;
                          aperture_size: integer = 3);cdecl;

var cvSobel: procedure (src: PIplImage; dst: PIplImage;
                         xorder: integer;
                         yorder: integer;
                         aperture_size: integer = 3);cdecl;

const
///* Types of thresholding */
 CV_THRESH_BINARY     = 0;  ///* value = value > threshold ? max_value : 0       */
 CV_THRESH_BINARY_INV = 1;  ///* value = value > threshold ? 0 : max_value       */
 CV_THRESH_TRUNC      = 2;  ///* value = value > threshold ? threshold : value   */
 CV_THRESH_TOZERO     = 3;  ///* value = value > threshold ? value : 0           */
 CV_THRESH_TOZERO_INV = 4;  ///* value = value > threshold ? 0 : value           */
 CV_THRESH_MASK       = 7;

 CV_THRESH_OTSU       = 8;  ///* use Otsu algorithm to choose the optimal threshold value;
                            //        combine the flag with one of the above CV_THRESH_* values */

///* Applies fixed-level threshold to grayscale image.
//   This is a basic operation applied before retrieving contours */
var cvThreshold: procedure(src: PIplImage; dst: PIplImage;
                       threshold: double; max_value: double;
                       threshold_type: integer);  cdecl;


///****************************************************************************************\
//*                              Contours retrieving                                       *
//\****************************************************************************************/

///* Retrieves outer and optionally inner boundaries of white (non-zero) connected
//   components in the black (zero) background */

var cvFindContours:function(image:pointer{PCvArr}; storage: PCvMemStorage;
                            first_contour:PPCvSeq;
                            header_size: integer {= sizeof(CvContour)};
                            mode: integer{=CV_RETR_LIST};
                            method: integer{=CV_CHAIN_APPROX_SIMPLE};
                            offset: CvPoint  {CV_DEFAULT(cvPoint(0,0))} ) :integer; cdecl;



///****************************************************************************************\
//*                            Contour Processing and Shape Analysis                       *
//\****************************************************************************************/
//
const
  CV_POLY_APPROX_DP = 0;
//
///* Approximates a single polygonal curve (contour) or
//   a tree of polygonal curves (contours) */
var cvApproxPoly: function(src_seq: pointer;
                           header_size: integer; storage:PCvMemStorage;
                           method: integer; parameter: double;
                           parameter2: integer = 0  ):PCvSeq;  cdecl;

const
  CV_DOMINANT_IPAN = 1;

///* Finds high-curvature points of the contour */
var cvFindDominantPoints: function(contour:PCvSeq; storage: PCvMemStorage;
                           method:integer = CV_DOMINANT_IPAN;
                           parameter1: double = 0;
                           parameter2: double = 0;
                           parameter3: double = 0;
                           parameter4: double = 0):PCvSeq;cdecl;

///* Calculates perimeter of a contour or length of a part of contour */
cvArcLength:function(const curve: pointer;
                     slice: CvSlice {= CV_DEFAULT(CV_WHOLE_SEQ),};
                     is_closed: integer = -1):double; cdecl;

function cvContourPerimeter(contour: PCvSeq): double;cdecl;

///* Calculates contour boundning rectangle (update=1) or
//   just retrieves pre-calculated rectangle (update=0) */
var cvBoundingRect:function(points:PCvArr;update:integer =0):CvRect;cdecl;

///* Calculates area of a contour or contour segment */
var cvContourArea: function(contour: pCvArr; slice: CvSlice {= CV_DEFAULT(CV_WHOLE_SEQ)} ): double;cdecl;

///* Checks whether the contour is convex or not (returns 1 if convex, 0 if not) */
var cvCheckContourConvexity: function(contour: PCvArr):integer;cdecl;
implementation

const
  {$IF Defined(MSWINDOWS)}
    DLL_CV='cv210.dll';
  {$ELSEIF Defined(DARWIN)}
    DLL_CV='libopencv_imgproc.dylib';
  {$ELSEIF Defined(UNIX)}
    DLL_CV='libopencv_imgproc.so';
  {$IFEND}

var
  DLLHandle: THandle;

procedure LoadDLL;
begin
  DLLHandle := LoadLibrary(DLL_CV);
  if DLLHandle >= 32 then
  begin
    //@cvPyrMeanShiftFiltering := GetProcAddress(DLLHandle,'cvPyrMeanShiftFiltering');
    {$IFDEF WIN32}
    //Assert(@cvPyrMeanShiftFiltering <> nil);
    {$ENDIF}
    //
    // @cvPyrSegmentation := GetProcAddress(DLLHandle,'cvPyrSegmentation');
    {$IFDEF WIN32}
    //Assert(@cvPyrSegmentation <> nil);
    {$ENDIF}
    //
    //@cvReleasePyramid := GetProcAddress(DLLHandle,'cvReleasePyramid');
    {$IFDEF WIN32}
    //Assert(@cvReleasePyramid <> nil);
    {$ENDIF}
    //
    //@cvCreatePyramid := GetProcAddress(DLLHandle,'cvCreatePyramid');
    {$IFDEF WIN32}
    //Assert(@cvCreatePyramid <> nil);
    {$ENDIF}
    //
    //@cvIntegral := GetProcAddress(DLLHandle,'cvIntegral');
    {$IFDEF WIN32}
    //Assert(@cvIntegral <> nil);
    {$ENDIF}
    //

    @cvSmooth := GetProcAddress(DLLHandle,'cvSmooth');
    {$IFDEF WIN32}
    Assert(@cvSmooth <> nil);
    {$ENDIF}
    //
    //@cvCopyMakeBorder := GetProcAddress(DLLHandle,'cvCopyMakeBorder');
    {$IFDEF WIN32}
    //Assert(@cvCopyMakeBorder <> nil);
    {$ENDIF}
    //

    //@cvFilter2D := GetProcAddress(DLLHandle,'cvFilter2D');
    {$IFDEF WIN32}
    //Assert(@cvFilter2D <> nil);
    {$ENDIF}
    //

    @cvCvtColor := GetProcAddress(DLLHandle,'cvCvtColor');
    {$IFDEF WIN32}
    Assert(@cvCvtColor <> nil);
    {$ENDIF}
    //
    @cvEqualizeHist := GetProcAddress(DLLHandle,'cvEqualizeHist');
    {$IFDEF WIN32}
    Assert(@cvEqualizeHist <> nil);
    {$ENDIF}
    //
    //@cvResize := GetProcAddress(DLLHandle,'cvResize');
    {$IFDEF WIN32}
    //Assert(@cvResize <> nil);
    {$ENDIF}
    //
    //@cvHaarDetectObjects := GetProcAddress(DLLHandle,'cvHaarDetectObjects');
    {$IFDEF WIN32}
    //Assert(@cvHaarDetectObjects <> nil);
    {$ENDIF}
    //
    //@cvPyrDown := GetProcAddress(DLLHandle,'cvPyrDown');
    {$IFDEF WIN32}
    //Assert(@cvPyrDown <> nil);
    {$ENDIF}
    //
    //@cvPyrUp := GetProcAddress(DLLHandle,'cvPyrUp');
    {$IFDEF WIN32}
    //Assert(@cvPyrUp <> nil);
    {$ENDIF}
    //
    @cvCanny := GetProcAddress(DLLHandle,'cvCanny');
    {$IFDEF WIN32}
    Assert(@cvCanny <> nil);
    {$ENDIF}

    //
    @cvDilate := GetProcAddress(DLLHandle,'cvDilate');
    {$IFDEF WIN32}
    Assert(@cvDilate <> nil);
    {$ENDIF}
    //
    @cvThreshold := GetProcAddress(DLLHandle,'cvThreshold');
    {$IFDEF WIN32}
    Assert(@cvThreshold <> nil);
    {$ENDIF}

    //
    //@cvFindContours := GetProcAddress(DLLHandle,'cvFindContours');
    {$IFDEF WIN32}
    //Assert(@cvFindContours <> nil);
    {$ENDIF}
    //
    //@cvApproxPoly := GetProcAddress(DLLHandle,'cvApproxPoly');
    {$IFDEF WIN32}
    //Assert(@cvApproxPoly <> nil);
   {$ENDIF}
    //
    //@cvArcLength := GetProcAddress(DLLHandle,'cvArcLength');
    {$IFDEF WIN32}
    //Assert(@cvArcLength <> nil);
   {$ENDIF}
    //
    //@cvContourArea := GetProcAddress(DLLHandle,'cvContourArea');
    {$IFDEF WIN32}
    //Assert(@cvContourArea <> nil);
   {$ENDIF}
    //
    //@cvCheckContourConvexity := GetProcAddress(DLLHandle,'cvCheckContourConvexity');
    {$IFDEF WIN32}
    //Assert(@cvCheckContourConvexity <> nil);
   {$ENDIF}

    //
    @cvErode := GetProcAddress(DLLHandle,'cvErode');
    {$IFDEF WIN32}
    Assert(@cvErode <> nil);
    {$ENDIF}

    //
    //@cvSobel := GetProcAddress(DLLHandle,'cvSobel');
    {$IFDEF WIN32}
    //Assert(@cvSobel <> nil);
   {$ENDIF}

       //
    //@cvLaplace := GetProcAddress(DLLHandle,'cvLaplace');
    {$IFDEF WIN32}
    //Assert(@cvLaplace <> nil);
   {$ENDIF}

  end
  else
  begin
    //showmessage(format('Error: %s could not be loaded !!',[DLL_CV]));
  end;
end;

function cvContourPerimeter(contour: PCvSeq): double;
begin
  try
  result := cvArcLength(contour,CV_WHOLE_SEQ,1);
  except
    //showmessage('aaaaaa1');

  end;
end;

initialization
  LoadDLL;


finalization
;

end.

