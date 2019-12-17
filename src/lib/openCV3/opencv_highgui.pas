unit opencv_highgui;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

{$L ApiWrapper.o}
{$IFDEF OpenCVVideoioStandalone}
  {$LINKLIB opencv_videoio}
{$ELSE}
  {$LINKLIB opencv_world}
{$ENDIF}

uses
  CTypes, SysUtils, opencv_types;

type
  PPCvCapture = ^PCvCapture;
  PCvCapture = pointer;

const
  CV_CAP_ANY   =   0  ;

  CV_CAP_MIL   =   100;

  CV_CAP_VFW   =   200;
  CV_CAP_V4L   =   200;
  CV_CAP_V4L2  =   200;

  CV_CAP_FIREWARE= 300;
  CV_CAP_FIREWIRE= 300;
  CV_CAP_IEEE1394= 300;
  CV_CAP_DC1394  = 300;
  CV_CAP_CMU1394 = 300;

  CV_CAP_STEREO  = 400;
  CV_CAP_TYZX    = 400;
  CV_TYZX_LEFT   = 400;
  CV_TYZX_RIGHT  = 401;
  CV_TYZX_COLOR  = 402;
  CV_TYZX_Z      = 403;

  CV_CAP_QT      = 500;

  CV_CAP_UNICAP  = 600;

  CV_CAP_DSHOW   = 700;

function cvCreateCameraCapture(index: cint): PCvCapture; cdecl; external name 'USDX_cvCreateCameraCapture';
function USDX_cvQueryFrame(capture: PCvCapture): PUMatWrapper; cdecl; external;
function cvQueryFrame(capture: PCvCapture): PIplImage;
procedure cvReleaseCapture(capture: PPCvCapture); cdecl; external name 'USDX_cvReleaseCapture';

const
    CV_CAP_PROP_DC1394_OFF         = -4;
    CV_CAP_PROP_DC1394_MODE_MANUAL = -3;
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
    CV_CAP_PROP_AUTO_EXPOSURE =21;


    CV_CAP_PROP_GAMMA         =22;
    CV_CAP_PROP_TEMPERATURE   =23;
    CV_CAP_PROP_TRIGGER       =24;
    CV_CAP_PROP_TRIGGER_DELAY =25;
    CV_CAP_PROP_WHITE_BALANCE_RED_V =26;
    CV_CAP_PROP_MAX_DC1394    =27;
    CV_CAP_PROP_AUTOGRAB      =1024;
    CV_CAP_PROP_SUPPORTED_PREVIEW_SIZES_STRING=1025;
    CV_CAP_PROP_PREVIEW_FORMAT=1026;

    CV_CAP_OPENNI_DEPTH_GENERATOR = 0;
    CV_CAP_OPENNI_IMAGE_GENERATOR = 1 shl 31;
    CV_CAP_OPENNI_GENERATORS_MASK = 1 shl 31;


    CV_CAP_PROP_OPENNI_OUTPUT_MODE      = 100;
    CV_CAP_PROP_OPENNI_FRAME_MAX_DEPTH  = 101;
    CV_CAP_PROP_OPENNI_BASELINE         = 102;
    CV_CAP_PROP_OPENNI_FOCAL_LENGTH     = 103;
    CV_CAP_PROP_OPENNI_REGISTRATION_ON  = 104;
    CV_CAP_PROP_OPENNI_REGISTRATION     = CV_CAP_PROP_OPENNI_REGISTRATION_ON;


    CV_CAP_OPENNI_IMAGE_GENERATOR_OUTPUT_MODE = CV_CAP_OPENNI_IMAGE_GENERATOR + CV_CAP_PROP_OPENNI_OUTPUT_MODE;
    CV_CAP_OPENNI_DEPTH_GENERATOR_BASELINE = CV_CAP_OPENNI_DEPTH_GENERATOR + CV_CAP_PROP_OPENNI_BASELINE;
    CV_CAP_OPENNI_DEPTH_GENERATOR_FOCAL_LENGTH = CV_CAP_OPENNI_DEPTH_GENERATOR + CV_CAP_PROP_OPENNI_FOCAL_LENGTH;
    CV_CAP_OPENNI_DEPTH_GENERATOR_REGISTRATION_ON = CV_CAP_OPENNI_DEPTH_GENERATOR + CV_CAP_PROP_OPENNI_REGISTRATION_ON;


    CV_CAP_GSTREAMER_QUEUE_LENGTH   = 200;
    CV_CAP_PROP_PVAPI_MULTICASTIP   = 300;


    CV_CAP_PROP_XI_DOWNSAMPLING  = 400;
    CV_CAP_PROP_XI_DATA_FORMAT   = 401;
    CV_CAP_PROP_XI_OFFSET_X      = 402;
    CV_CAP_PROP_XI_OFFSET_Y      = 403;
    CV_CAP_PROP_XI_TRG_SOURCE    = 404;
    CV_CAP_PROP_XI_TRG_SOFTWARE  = 405;
    CV_CAP_PROP_XI_GPI_SELECTOR  = 406;
    CV_CAP_PROP_XI_GPI_MODE      = 407;
    CV_CAP_PROP_XI_GPI_LEVEL     = 408;
    CV_CAP_PROP_XI_GPO_SELECTOR  = 409;
    CV_CAP_PROP_XI_GPO_MODE      = 410;
    CV_CAP_PROP_XI_LED_SELECTOR  = 411;
    CV_CAP_PROP_XI_LED_MODE      = 412;
    CV_CAP_PROP_XI_MANUAL_WB     = 413;
    CV_CAP_PROP_XI_AUTO_WB       = 414;
    CV_CAP_PROP_XI_AEAG          = 415;
    CV_CAP_PROP_XI_EXP_PRIORITY  = 416;
    CV_CAP_PROP_XI_AE_MAX_LIMIT  = 417;
    CV_CAP_PROP_XI_AG_MAX_LIMIT  = 418;
    CV_CAP_PROP_XI_AEAG_LEVEL    = 419;
    CV_CAP_PROP_XI_TIMEOUT       = 420;

function cvSetCaptureProperty(capture: PCvCapture; property_id: cint; value: cdouble): cint; cdecl; external name 'USDX_cvSetCaptureProperty';

implementation

function cvQueryFrame(capture: PCvCapture): PIplImage;
var
	w: PUMatWrapper;
begin
	w := USDX_cvQueryFrame(capture);
	Result := nil;
	if w <> nil then
		Result := PIplImage.Create(w);
end;

end.
