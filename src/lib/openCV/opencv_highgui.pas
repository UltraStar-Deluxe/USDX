unit opencv_highgui;

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
//*                                  Basic GUI functions                                   *
//\****************************************************************************************/

///* this function is used to set some external parameters in case of X Window */
var cvInitSystem:function (argc: integer;argv: ppchar): integer;  cdecl;

var cvStartWindowThread:function (): integer; cdecl;

const
  CV_WINDOW_AUTOSIZE = 1;
var
///* create window */
  cvNamedWindow: function (name:pansichar; flags: integer): integer; cdecl;

///* display image within window (highgui windows remember their content) */
  cvShowImage: procedure(name:pansichar; image: pointer); cdecl;

///* resize/move window */
  cvResizeWindow: procedure(name:pansichar; width, height: integer);  cdecl;
  cvMoveWindow: procedure(name:pansichar; width, height: integer);  cdecl;


///* destroy window and all the trackers associated with it */
  cvDestroyWindow: procedure (name:pansichar);  cdecl;
  cvDestroyAllWindows: procedure ;  cdecl;

///* get native window handle (HWND in case of Win32 and Widget in case of X Window) */
  cvGetWindowHandle: function(name:pansichar): LongInt; cdecl;

///* get name of highgui window given its native handle */
  cvGetWindowName: function(window_handle:LongInt): pansichar; cdecl;

//typedef void (CV_CDECL *CvTrackbarCallback)(int pos);

///* create trackbar and display it on top of given window, set callback */
  cvCreateTrackbar: function(trackbar_name,window_name:pansichar; value: PInteger; count: integer; on_change: pointer ): integer; cdecl;

///* retrieve or set trackbar position */
  cvGetTrackbarPos: function(trackbar_name,window_name:pansichar): integer; cdecl;
  cvSetTrackbarPos: procedure(trackbar_name,window_name:pansichar; pos: integer); cdecl;


const
  CV_EVENT_MOUSEMOVE     = 0;
  CV_EVENT_LBUTTONDOWN   = 1;
  CV_EVENT_RBUTTONDOWN   = 2;
  CV_EVENT_MBUTTONDOWN   = 3;
  CV_EVENT_LBUTTONUP     = 4;
  CV_EVENT_RBUTTONUP     = 5;
  CV_EVENT_MBUTTONUP     = 6;
  CV_EVENT_LBUTTONDBLCLK = 7;
  CV_EVENT_RBUTTONDBLCLK = 8;
  CV_EVENT_MBUTTONDBLCLK = 9;

  CV_EVENT_FLAG_LBUTTON  = 1;
  CV_EVENT_FLAG_RBUTTON  = 2;
  CV_EVENT_FLAG_MBUTTON  = 4;
  CV_EVENT_FLAG_CTRLKEY  = 8;
  CV_EVENT_FLAG_SHIFTKEY = 16;
  CV_EVENT_FLAG_ALTKEY   = 32;

///* assign callback for mouse events */
//CVAPI(void) cvSetMouseCallback( const char* window_name, CvMouseCallback on_mouse,
//                                void* param CV_DEFAULT(NULL));

(*/* 8bit, color or not */
#define CV_LOAD_IMAGE_UNCHANGED  -1
/* 8bit, gray */
#define CV_LOAD_IMAGE_GRAYSCALE   0
/* ?, color */
#define CV_LOAD_IMAGE_COLOR       1
/* any depth, ? */
#define CV_LOAD_IMAGE_ANYDEPTH    2
/* ?, any color */
#define CV_LOAD_IMAGE_ANYCOLOR    4*)
const
  CV_LOAD_IMAGE_UNCHANGED =  -1;
  CV_LOAD_IMAGE_GRAYSCALE =   0;
  CV_LOAD_IMAGE_COLOR     =   1;
  CV_LOAD_IMAGE_ANYDEPTH  =   2;
  CV_LOAD_IMAGE_ANYCOLOR  =   4;

///* load image from file
//  iscolor can be a combination of above flags where CV_LOAD_IMAGE_UNCHANGED
//  overrides the other flags
//  using CV_LOAD_IMAGE_ANYCOLOR alone is equivalent to CV_LOAD_IMAGE_UNCHANGED
//  unless CV_LOAD_IMAGE_ANYDEPTH is specified images are converted to 8bit
//*/
var
  cvLoadImage: function(name:pansichar; flags: integer):PIplImage cdecl;
  cvLoadImageM: function(name:pansichar; flags: integer):PCvMat cdecl;

///* save image to file */
  cvSaveImage:  function (filename: PansiChar; image: PIplImage): integer; cdecl;

const
 CV_CVTIMG_FLIP   =   1;
 CV_CVTIMG_SWAP_RB=   2;

///* utility function: convert one image to another with optional vertical flip */
var  cvConvertImage: procedure( src: pointer; dst: pointer; flags: integer =0); cdecl;

///* wait for key event infinitely (delay<=0) or for "delay" milliseconds */
var  cvWaitKey: function(delay: integer = 0): integer;cdecl;



(*/****************************************************************************************\
*                         Working with Video Files and Cameras                           *
\****************************************************************************************/*)
type
  ///* "black box" capture structure */
  PPCvCapture = ^PCvCapture;
  PCvCapture = ^CvCapture;
  CvCapture = record
  end;

///* start capturing frames from video file */
var cvCreateFileCapture:function(filename: pansiChar ):PCvCapture;cdecl;


const
  CV_CAP_ANY   =   0  ;   // autodetect

  CV_CAP_MIL   =   100;   // MIL proprietary drivers

  CV_CAP_VFW   =   200;   // platform native
  CV_CAP_V4L   =   200;
  CV_CAP_V4L2  =   200;

  CV_CAP_FIREWARE= 300;   // IEEE 1394 drivers
  CV_CAP_FIREWIRE= 300;
  CV_CAP_IEEE1394= 300;
  CV_CAP_DC1394  = 300;
  CV_CAP_CMU1394 = 300;

  CV_CAP_STEREO  = 400;   // TYZX proprietary drivers
  CV_CAP_TYZX    = 400;
  CV_TYZX_LEFT   = 400;
  CV_TYZX_RIGHT  = 401;
  CV_TYZX_COLOR  = 402;
  CV_TYZX_Z      = 403;

  CV_CAP_QT      = 500;   // QuickTime

  CV_CAP_UNICAP  = 600;   // Unicap drivers

  CV_CAP_DSHOW   = 700;   // DirectShow (via videoInput)

var
///* start capturing frames from camera: index = camera_index + domain_offset (CV_CAP_*) */
cvCreateCameraCapture: function (index: integer): PCvCapture;cdecl;

///* grab a frame, return 1 on success, 0 on fail.
//  this function is thought to be fast               */
cvGrabFrame: function (capture: PCvCapture):integer;cdecl;


///* get the frame grabbed with cvGrabFrame(..)
//  This function may apply some frame processing like
//  frame decompression, flipping etc.
//  !!!DO NOT RELEASE or MODIFY the retrieved frame!!! */
cvRetrieveFrame: function (capture: PCvCapture):PIplImage;cdecl;

///* Just a combination of cvGrabFrame and cvRetrieveFrame
//   !!!DO NOT RELEASE or MODIFY the retrieved frame!!!      */
cvQueryFrame: function(capture: PCvCapture):PIplImage;cdecl;

///* stop capturing/reading and free resources */
cvReleaseCapture: procedure(capture: PPCvCapture);cdecl;

//cvcamGetCamerasCount: function():integer;cdecl;

const
    // modes of the controlling registers (can be: auto, manual, auto single push, absolute Latter allowed with any other mode)
    // every feature can have only one mode turned on at a time
    CV_CAP_PROP_DC1394_OFF         = -4;  //turn the feature off (not controlled manually nor automatically)
    CV_CAP_PROP_DC1394_MODE_MANUAL = -3; //set automatically when a value of the feature is set by the user
    CV_CAP_PROP_DC1394_MODE_AUTO = -2;
    CV_CAP_PROP_DC1394_MODE_ONE_PUSH_AUTO = -1;
    CV_CAP_PROP_POS_MSEC       =0;
    CV_CAP_PROP_POS_FRAMES     =1;
    CV_CAP_PROP_POS_AVI_RATIO  =2;
    CV_CAP_PROP_FRAME_WIDTH    =3;
    CV_CAP_PROP_FRAME_HEIGHT   =4;
    CV_CAP_PROP_FPS            =5;
    CV_CAP_PROP_FOURCC         =6;
    CV_CAP_PROP_FRAME_COUNT    =7;
    CV_CAP_PROP_FORMAT         =8;
    CV_CAP_PROP_MODE           =9;
    CV_CAP_PROP_BRIGHTNESS    =10;
    CV_CAP_PROP_CONTRAST      =11;
    CV_CAP_PROP_SATURATION    =12;
    CV_CAP_PROP_HUE           =13;
    CV_CAP_PROP_GAIN          =14;
    CV_CAP_PROP_EXPOSURE      =15;
    CV_CAP_PROP_CONVERT_RGB   =16;
    CV_CAP_PROP_WHITE_BALANCE_BLUE_U =17;
    CV_CAP_PROP_RECTIFICATION =18;
    CV_CAP_PROP_MONOCROME     =19;
    CV_CAP_PROP_SHARPNESS     =20;
    CV_CAP_PROP_AUTO_EXPOSURE =21; // exposure control done by camera,
                                   // user can adjust refernce level
                                   // using this feature
    CV_CAP_PROP_GAMMA         =22;
    CV_CAP_PROP_TEMPERATURE   =23;
    CV_CAP_PROP_TRIGGER       =24;
    CV_CAP_PROP_TRIGGER_DELAY =25;
    CV_CAP_PROP_WHITE_BALANCE_RED_V =26;
    CV_CAP_PROP_MAX_DC1394    =27;
    CV_CAP_PROP_AUTOGRAB      =1024; // property for highgui class CvCapture_Android only
    CV_CAP_PROP_SUPPORTED_PREVIEW_SIZES_STRING=1025; // readonly, tricky property, returns cpnst char* indeed
    CV_CAP_PROP_PREVIEW_FORMAT=1026; // readonly, tricky property, returns cpnst char* indeed
    // OpenNI map generators
    CV_CAP_OPENNI_DEPTH_GENERATOR = 0;
    CV_CAP_OPENNI_IMAGE_GENERATOR = 1 shl 31;
    CV_CAP_OPENNI_GENERATORS_MASK = 1 shl 31;

    // Properties of cameras available through OpenNI interfaces
    CV_CAP_PROP_OPENNI_OUTPUT_MODE      = 100;
    CV_CAP_PROP_OPENNI_FRAME_MAX_DEPTH  = 101; // in mm
    CV_CAP_PROP_OPENNI_BASELINE         = 102; // in mm
    CV_CAP_PROP_OPENNI_FOCAL_LENGTH     = 103; // in pixels
    CV_CAP_PROP_OPENNI_REGISTRATION_ON  = 104; // flag
    CV_CAP_PROP_OPENNI_REGISTRATION     = CV_CAP_PROP_OPENNI_REGISTRATION_ON; // flag that synchronizes the remapping depth map to image map
                                                                              // by changing depth generator's view point (if the flag is "on") or
                                                                              // sets this view point to its normal one (if the flag is "off").
    CV_CAP_OPENNI_IMAGE_GENERATOR_OUTPUT_MODE = CV_CAP_OPENNI_IMAGE_GENERATOR + CV_CAP_PROP_OPENNI_OUTPUT_MODE;
    CV_CAP_OPENNI_DEPTH_GENERATOR_BASELINE = CV_CAP_OPENNI_DEPTH_GENERATOR + CV_CAP_PROP_OPENNI_BASELINE;
    CV_CAP_OPENNI_DEPTH_GENERATOR_FOCAL_LENGTH = CV_CAP_OPENNI_DEPTH_GENERATOR + CV_CAP_PROP_OPENNI_FOCAL_LENGTH;
    CV_CAP_OPENNI_DEPTH_GENERATOR_REGISTRATION_ON = CV_CAP_OPENNI_DEPTH_GENERATOR + CV_CAP_PROP_OPENNI_REGISTRATION_ON;

    // Properties of cameras available through GStreamer interface
    CV_CAP_GSTREAMER_QUEUE_LENGTH   = 200; // default is 1
    CV_CAP_PROP_PVAPI_MULTICASTIP   = 300; // ip for anable multicast master mode. 0 for disable multicast

    // Properties of cameras available through XIMEA SDK interface
    CV_CAP_PROP_XI_DOWNSAMPLING  = 400;      // Change image resolution by binning or skipping.
    CV_CAP_PROP_XI_DATA_FORMAT   = 401;       // Output data format.
    CV_CAP_PROP_XI_OFFSET_X      = 402;      // Horizontal offset from the origin to the area of interest (in pixels).
    CV_CAP_PROP_XI_OFFSET_Y      = 403;      // Vertical offset from the origin to the area of interest (in pixels).
    CV_CAP_PROP_XI_TRG_SOURCE    = 404;      // Defines source of trigger.
    CV_CAP_PROP_XI_TRG_SOFTWARE  = 405;      // Generates an internal trigger. PRM_TRG_SOURCE must be set to TRG_SOFTWARE.
    CV_CAP_PROP_XI_GPI_SELECTOR  = 406;      // Selects general purpose input
    CV_CAP_PROP_XI_GPI_MODE      = 407;      // Set general purpose input mode
    CV_CAP_PROP_XI_GPI_LEVEL     = 408;      // Get general purpose level
    CV_CAP_PROP_XI_GPO_SELECTOR  = 409;      // Selects general purpose output
    CV_CAP_PROP_XI_GPO_MODE      = 410;      // Set general purpose output mode
    CV_CAP_PROP_XI_LED_SELECTOR  = 411;      // Selects camera signalling LED
    CV_CAP_PROP_XI_LED_MODE      = 412;      // Define camera signalling LED functionality
    CV_CAP_PROP_XI_MANUAL_WB     = 413;      // Calculates White Balance(must be called during acquisition)
    CV_CAP_PROP_XI_AUTO_WB       = 414;      // Automatic white balance
    CV_CAP_PROP_XI_AEAG          = 415;      // Automatic exposure/gain
    CV_CAP_PROP_XI_EXP_PRIORITY  = 416;      // Exposure priority (0.5 - exposure 50%, gain 50%).
    CV_CAP_PROP_XI_AE_MAX_LIMIT  = 417;      // Maximum limit of exposure in AEAG procedure
    CV_CAP_PROP_XI_AG_MAX_LIMIT  = 418;      // Maximum limit of gain in AEAG procedure
    CV_CAP_PROP_XI_AEAG_LEVEL    = 419;       // Average intensity of output signal AEAG should achieve(in %)
    CV_CAP_PROP_XI_TIMEOUT       = 420;      // Image capture timeout in milliseconds


///* retrieve or set capture properties */
var cvGetCaptureProperty: function(capture: PCvCapture; property_id: integer):integer; cdecl;
var cvSetCaptureProperty: function(capture: PCvCapture; property_id: integer; value: double):integer; cdecl;

///* "black box" video file writer structure */
type
  PPCvVideoWriter = ^PCvVideoWriter;
  PCvVideoWriter= ^CvVideoWriter;
  CvVideoWriter = record
  end;

function CV_FOURCC(c1,c2,c3,c4: char): integer; cdecl;

const
  CV_FOURCC_PROMPT = -1; ///* Open Codec Selection Dialog (Windows only) */
  CV_FOURCC_DEFAULT = -1;///* Use default codec for specified filename (Linux only) */
  ///

///* initialize video file writer */
var cvCreateVideoWriter: function(filename:pchar; fourcc: integer;
                                  fps: double; frame_size: CvSize;
                                  is_color: integer = 0):PCvVideoWriter; cdecl;


//CVAPI(CvVideoWriter*) cvCreateImageSequenceWriter( const char* filename,
//                                                   int is_color CV_DEFAULT(1));

///* write frame to video file */
cvWriteFrame: function(writer: PCvVideoWriter; image: PIplImage): integer;cdecl;

///* close video file writer */
cvReleaseVideoWriter:procedure (writer: PPCvVideoWriter);cdecl;

///****************************************************************************************\
//*                              Obsolete functions/synonyms                               *
//\****************************************************************************************/


implementation


const
  {$IF Defined(MSWINDOWS)}
    DLL_HIGHGUI='highgui210.dll';//'opencv_highgui231.dll';
  {$ELSEIF Defined(DARWIN)}
    DLL_HIGHGUI='libopencv_highgui.dylib';
  {$ELSEIF Defined(UNIX)}
    DLL_HIGHGUI='libopencv_highgui.so';
  {$IFEND}

var
  DLLHandle: THandle;

procedure LoadDLL;
begin
  DLLHandle := LoadLibrary(DLL_HIGHGUI);
  if DLLHandle >= 32 then
  begin

//    @cvInitSystem := GetProcAddress(DLLHandle,'cvInitSystem');
//    {$IFDEF WIN32}
//    Assert(@cvInitSystem <> nil);
//    {$ENDIF}
//    //
//    @cvStartWindowThread := GetProcAddress(DLLHandle,'cvStartWindowThread');
//    {$IFDEF WIN32}
//    Assert(@cvStartWindowThread <> nil);
//    {$ENDIF}
//    //
//
//    @cvNamedWindow := GetProcAddress(DLLHandle,'cvNamedWindow');
//    {$IFDEF WIN32}
//    Assert(@cvNamedWindow <> nil);
//    {$ENDIF}
//    //
//    @cvDestroyWindow := GetProcAddress(DLLHandle,'cvDestroyWindow');
//    {$IFDEF WIN32}
//    Assert(@cvDestroyWindow <> nil);
//    {$ENDIF}
////    //
//    @cvDestroyAllWindows := GetProcAddress(DLLHandle,'cvDestroyAllWindows');
//    {$IFDEF WIN32}
//    Assert(@cvDestroyAllWindows <> nil);
//    {$ENDIF}
//    //
//    @cvResizeWindow := GetProcAddress(DLLHandle,'cvResizeWindow');
//    {$IFDEF WIN32}
//    Assert(@cvResizeWindow <> nil);
//    {$ENDIF}
//    //
//    @cvMoveWindow := GetProcAddress(DLLHandle,'cvMoveWindow');
//    {$IFDEF WIN32}
//    Assert(@cvMoveWindow <> nil);
//    {$ENDIF}
//    //
//    @cvLoadImage := GetProcAddress(DLLHandle,'cvLoadImage');
//    {$IFDEF WIN32}
//    Assert(@cvLoadImage <> nil);
//    {$ENDIF}
//    //
//    @cvLoadImageM := GetProcAddress(DLLHandle,'cvLoadImageM');
//    {$IFDEF WIN32}
//    Assert(@cvLoadImageM <> nil);
//    {$ENDIF}
//    //
//    @cvShowImage := GetProcAddress(DLLHandle,'cvShowImage');
//    {$IFDEF WIN32}
//    Assert(@cvShowImage <> nil);
////    {$ENDIF}
//    //
    @cvWaitKey := GetProcAddress(DLLHandle,'cvWaitKey');
    {$IFDEF WIN32}
    Assert(@cvWaitKey <> nil);
    {$ENDIF}
//    //
//    @cvGetWindowHandle := GetProcAddress(DLLHandle,'cvGetWindowHandle');
//    {$IFDEF WIN32}
//    Assert(@cvGetWindowHandle <> nil);
//    {$ENDIF}
////    //
//    @cvGetWindowName := GetProcAddress(DLLHandle,'cvGetWindowName');
//    {$IFDEF WIN32}
//    Assert(@cvGetWindowName <> nil);
//    {$ENDIF}
//    //
//    @cvCreateTrackbar := GetProcAddress(DLLHandle,'cvCreateTrackbar');
//    {$IFDEF WIN32}
//    Assert(@cvCreateTrackbar <> nil);
//    {$ENDIF}
//    //
//    @cvGetTrackbarPos := GetProcAddress(DLLHandle,'cvGetTrackbarPos');
//    {$IFDEF WIN32}
//    Assert(@cvGetTrackbarPos <> nil);
//    {$ENDIF}
//    //
//    @cvSetTrackbarPos := GetProcAddress(DLLHandle,'cvSetTrackbarPos');
//    {$IFDEF WIN32}
//    Assert(@cvSetTrackbarPos <> nil);
//    {$ENDIF}
//    //
//    @cvCreateFileCapture := GetProcAddress(DLLHandle,'cvCreateFileCapture');
//    {$IFDEF WIN32}
//    Assert(@cvCreateFileCapture <> nil);
////    {$ENDIF}
//    //
    @cvCreateCameraCapture := GetProcAddress(DLLHandle,'cvCreateCameraCapture');
    {$IFDEF WIN32}
    Assert(@cvCreateCameraCapture <> nil);
    {$ENDIF}
//    //
//    @cvConvertImage := GetProcAddress(DLLHandle,'cvConvertImage');
//    {$IFDEF WIN32}
//    Assert(@cvConvertImage <> nil);
//    {$ENDIF}

    //
//    @cvSaveImage := GetProcAddress(DLLHandle,'cvSaveImage');
//    {$IFDEF WIN32}
//    Assert(@cvSaveImage <> nil);
//    {$ENDIF}

    //
//    @cvCreateVideoWriter := GetProcAddress(DLLHandle,'cvCreateVideoWriter');
//    {$IFDEF WIN32}
//    Assert(@cvCreateVideoWriter <> nil);
//    {$ENDIF}

    //
//    @cvReleaseVideoWriter := GetProcAddress(DLLHandle,'cvReleaseVideoWriter');
//    {$IFDEF WIN32}
//    Assert(@cvReleaseVideoWriter <> nil);
//    {$ENDIF}

    //
    @cvQueryFrame := GetProcAddress(DLLHandle,'cvQueryFrame');
    {$IFDEF WIN32}
    Assert(@cvQueryFrame <> nil);
    {$ENDIF}

    //
    @cvGrabFrame := GetProcAddress(DLLHandle,'cvGrabFrame');
    {$IFDEF WIN32}
    Assert(@cvGrabFrame <> nil);
    {$ENDIF}

    //
    @cvReleaseCapture := GetProcAddress(DLLHandle,'cvReleaseCapture');
    {$IFDEF WIN32}
    Assert(@cvReleaseCapture <> nil);
    {$ENDIF}

    //
    @cvGetCaptureProperty := GetProcAddress(DLLHandle,'cvGetCaptureProperty');
    {$IFDEF WIN32}
    Assert(@cvGetCaptureProperty <> nil);
    {$ENDIF}

    //
    @cvSetCaptureProperty := GetProcAddress(DLLHandle,'cvSetCaptureProperty');
    {$IFDEF WIN32}
    Assert(@cvSetCaptureProperty <> nil);
    {$ENDIF}

    //
//    @cvWriteFrame := GetProcAddress(DLLHandle,'cvWriteFrame');
//    {$IFDEF WIN32}
//    Assert(@cvWriteFrame <> nil);
//    {$ENDIF}

  end
  else
  begin
    //showmessage(format('Error: %s could not be loaded !!',[DLL_HIGHGUI]));
    { Error: WINUSB.DLL could not be loaded !! }
  end;
end;

function CV_FOURCC(c1,c2,c3,c4 : char): integer;
begin
  Result := ((byte(c1) and 255) or ((byte(c2) and 255)shl 8) + ((byte(c3) and 255) shl 16) + ((byte(c4) and 255)shl 24)) ;
end;

initialization
  LoadDLL;


finalization
;
end.
