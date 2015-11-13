(*
               INTEL CORPORATION PROPRIETARY INFORMATION
This software is supplied under the terms of a license agreement or
nondisclosure agreement with Intel Corporation and may not be copied
or disclosed except in accordance with the terms of that agreement.
  Copyright (c) 1998-2000 Intel Corporation. All Rights Reserved.

From:
Purpose: IPL Common Header file
*)

unit IPL;

{$A+,Z+}

interface

uses
  Windows;

type
  Float  = Single;
  PFloat = ^Float;
  P2PFloat = ^PFloat;
  Short  = SmallInt;
  PShort = ^Short;

  PDouble = ^Double;
  P2PDouble = ^PDouble;

  IPLStatus = Integer;

{---------------------------  Library Version  ----------------------------}
type
  PIPLLibVersion = ^TIPLLibVersion;
  TIPLLibVersion = record
    Major           : Integer;       // e.g. 1
    Minor           : Integer;       // e.g. 00
    Build           : Integer;       // e.g. 01
    Name            : PChar;         // e.g. "ipl6l.lib","iplm5.dll"
    Version         : PChar;         // e.g. "v1.00"
    InternalVersion : PChar;         // e.g. "[1.00.01, 07/25/96]"
    BuildDate       : PChar;         // e.g. "Jun 1 96"
    CallConv        : PChar;         // e.g. "DLL"
  end;

function iplGetLibVersion : PIPLLibVersion; stdcall;
{
Returns:    the structure of information about  version of IPL library
}

{==========================================================================
      Section: Error Handling Functions
 ==========================================================================}

{------------------------  Error Call definition  -------------------------}

type
  TIPLErrorCallBack = function(
    Status : IPLStatus;
    FuncName : PChar;
    Context  : PChar;
    FileName : PChar;
    Line     : Integer) : IplStatus; stdcall;

{----------------------  IPLErrStatus Declaration  ------------------------}

const
  _StsMaskImg_              = (-100);

  IPL_StsOk                 =  0;  // everithing is ok
  IPL_StsBackTrace          = -1;  // pseudo error for back trace
  IPL_StsError              = -2;  // unknown /uiplecified error
  IPL_StsInternal           = -3;  // internal error (bad state)
  IPL_StsNoMem              = -4;  // insufficient memory
  IPL_StsBadArg             = -5;  // function arg/param is bad
  IPL_StsBadFunc            = -6;  // unsupported function
  IPL_StsNoConv             = -7;  // iter. didn't converge
  IPL_StsAutoTrace          = -8;  // Tracing through ipltrace.h

  IPL_HeaderIsNull          = -9;  // image header is NULL
  IPL_BadImageSize          = -10; // Image size is invalid
  IPL_BadOffset             = -11; // Offset is invalid
  IPL_BadDataPtr            = -12; // Image must be tiled or must have
                                   // nonzero data pointer
  IPL_BadStep               = -13;
  IPL_BadModelOrChSeq       = -14;
  IPL_BadNumChannels        = -15;
  IPL_BadNumChannel1U       = -16;
  IPL_BadDepth              = -17;
  IPL_BadAlphaChannel       = -18;
  IPL_BadOrder              = -19;
  IPL_BadOrigin             = -20;
  IPL_BadAlign              = -21;
  IPL_BadCallBack           = -22;
  IPL_BadTileSize           = -23;
  IPL_BadCOI                = -24;
  IPL_BadROISize            = -25;

  IPL_MaskIsTiled           = -26;

  IPL_StsNullPtr                = -27; // Null pointer
  IPL_StsVecLengthErr           = -28; // Incorrect vector length
  IPL_StsFilterStructContentErr = -29; // Incorr. filter structure content
  IPL_StsKernelStructContentErr = -30; // Incorr. transform kernel content
  IPL_StsFilterOffsetErr        = -31; // Incorrect filter ofset value

  IPL_MaskBadImageSize      = IPL_BadImageSize    + _StsMaskImg_;
  IPL_MaskBadOffset         = IPL_BadOffset       + _StsMaskImg_;
  IPL_MaskBadDataPtr        = IPL_BadDataPtr      + _StsMaskImg_;
  IPL_MaskBadStep           = IPL_BadStep         + _StsMaskImg_;
  IPL_MaskBadModelOrChSeq   = IPL_BadModelOrChSeq + _StsMaskImg_;
  IPL_MaskBadNumChannels    = IPL_BadNumChannels  + _StsMaskImg_;
  IPL_MaskBadDepth          = IPL_BadDepth        + _StsMaskImg_;
  IPL_MaskBadAlphaChannel   = IPL_BadAlphaChannel + _StsMaskImg_;
  IPL_MaskBadOrder          = IPL_BadOrder        + _StsMaskImg_;
  IPL_MaskBadOrigin         = IPL_BadOrigin       + _StsMaskImg_;
  IPL_MaskBadAlign          = IPL_BadAlign        + _StsMaskImg_;
  IPL_MaskBadCOI            = IPL_BadCOI          + _StsMaskImg_;
  IPL_MaskBadROISize        = IPL_BadROISize      + _StsMaskImg_;

function  iplGetErrStatus        : IPLStatus;  stdcall;
{
Purpose:    Gets last error status
}

procedure iplSetErrStatus(Status : IPLStatus); stdcall;
{
Purpose:    Sets error status
}

{----------------------  IPLStdErrMode Declaration  ------------------------}

const
  IPL_ErrModeLeaf   = 0;           // Print error and exit program
  IPL_ErrModeParent = 1;           // Print error and continue
  IPL_ErrModeSilent = 2;           // Don't print and continue

function  iplGetErrMode      : Integer;  stdcall;
procedure iplSetErrMode(Mode : Integer); stdcall;
{
Name:       iplGetErrMode, iplSetErrMode
Purpose:    gets/sets error mode
}

function iplError(Status   : IPLStatus;
                  Func     : PChar;
                  Context  : PChar;
                  FileName : PChar;
                  Line     : Integer) : IPLStatus; stdcall;
{
Purpose:    performs basic error handling
Returns:    last status
}

function iplErrorStr(Status : IPLStatus) : PChar; stdcall;
{
Purpose:    translates an error status code into a textual description
}

function iplRedirectError(
  iplErrorFunc : TIPLErrorCallBack) : TIPLErrorCallBack;  stdcall;
{
Purpose:    assigns a new error-handling function
Returns:    old error-handling function
Parameters: new error-handling function
}

{-----------------  Predefined error-handling functions  ------------------}

{
    Output to:
        iplNulDevReport - nothing
        iplStdErrReport - console (printf)
        iplGuiBoxReport - MessageBox (Win32)
}
function iplNulDevReport(Status   : IPLStatus;
                         FuncName : PChar;
                         Context  : PChar;
                         FileName : PChar;
                         Line     : Integer) : IPLStatus; stdcall;
function iplStdErrReport(Status   : IPLStatus;
                         FuncName : PChar;
                         Context  : PChar;
                         FileName : PChar;
                         Line     : Integer) : IPLStatus; stdcall;
function iplGuiBoxReport(Status   : IPLStatus;
                         FuncName : PChar;
                         Context  : PChar;
                         FileName : PChar;
                         Line     : Integer) : IPLStatus; stdcall;

{==========================================================================
      Section: Memory Allocation Functions
 ==========================================================================}

function  iplMalloc( Len : Integer) : Pointer;  stdcall;
function  iplwMalloc(Len : Integer) : PShort;   stdcall;
function  ipliMalloc(Len : Integer) : PInteger; stdcall;
function  iplsMalloc(Len : Integer) : PFloat;   stdcall;
function  ipldMalloc(Len : Integer) : PDouble;  stdcall;
{
Name:       iplMalloc, iplwMalloc, ipliMalloc, iplsMalloc, ipldMalloc
Purpose:    Allocate memory aligned on 32 bytes
Returns:    a pointer to an aligned memory block or NULL if no memory.
Parameters: Len - size of required memory block in elements.
            For iplMalloc length in bytes, for others a type of an
            element depends on behalf of function (see type of return).

Notes:     The only function to free the memory allocated by any of
           these functions is iplFree().
}

procedure iplFree(   P   : Pointer); stdcall;
{
Purpose:    Frees memory allocated by one of the ipl?Malloc functions.
Notes:      The function iplFree() cannot be used to free memory
            allocated by standard functions like malloc() or calloc().
}

{==========================================================================
      Section: Misc macros and definitions
 ==========================================================================}

function IPL_DegToRad(Deg : Extended) : Extended;
function IPLsDegToRad(Deg : Float)    : Float;
function IPLdDegToRad(Deg : Double)   : Double;

const
  IPL_EPS  = 1.0E-12;
  IPL_PI   = 3.14159265358979324;
  IPL_2PI  = 6.28318530717958648;
  IPL_PI_2 = 1.57079632679489662;
  IPL_PI_4 = 0.785398163397448310;

{----------------------  Code for channel sequence  -----------------------}
const
  IPL_CSEQ_G     = $00000047;      //* "G"    */
  IPL_CSEQ_GRAY  = $59415247;      //* "GRAY" */
  IPL_CSEQ_BGR   = $00524742;      //* "BGR"  */
  IPL_CSEQ_BGRA  = $41524742;      //* "BGRA" */
  IPL_CSEQ_RGB   = $00424752;      //* "RGB"  */
  IPL_CSEQ_RGBA  = $41424752;      //* "RGBA" */

  {==== IPLibrary Definitions ===================================================}
  IPL_DEPTH_SIGN = Integer($80000000);
  IPL_DEPTH_MASK = $7FFFFFFF;

  IPL_DEPTH_1U   =  1;
  IPL_DEPTH_8U   =  8;
  IPL_DEPTH_16U  = 16;
  IPL_DEPTH_32F  = 32;

  IPL_DEPTH_8S   = IPL_DEPTH_SIGN or  8;
  IPL_DEPTH_16S  = IPL_DEPTH_SIGN or 16;
  IPL_DEPTH_32S  = IPL_DEPTH_SIGN or 32;

  IPL_DATA_ORDER_PIXEL = 0;
  IPL_DATA_ORDER_PLANE = 1;

  IPL_ORIGIN_TL  = 0;
  IPL_ORIGIN_BL  = 1;

  IPL_ALIGN_4BYTES  =  4;
  IPL_ALIGN_8BYTES  =  8;
  IPL_ALIGN_16BYTES = 16;
  IPL_ALIGN_32BYTES = 32;

  IPL_ALIGN_DWORD   = IPL_ALIGN_4BYTES;
  IPL_ALIGN_QWORD   = IPL_ALIGN_8BYTES;

  IPL_GET_TILE_TO_READ  = 1;
  IPL_GET_TILE_TO_WRITE = 2;
  IPL_RELEASE_TILE      = 4;

  IPL_LUT_LOOKUP = 0;
  IPL_LUT_INTER  = 1;

(*
{==== Code for channel sequence ============================================}
  IPL_CSEQ_G     = $00000047;      //* "G"    */
  IPL_CSEQ_GRAY  = $59415247;      //* "GRAY" */
  IPL_CSEQ_BGR   = $00524742;      //* "BGR"  */
  IPL_CSEQ_BGRA  = $41524742;      //* "BGRA" */
  IPL_CSEQ_RGB   = $00424752;      //* "RGB"  */
  IPL_CSEQ_RGBA  = $41424752;      //* "RGBA" */
*)

{==== Common Types =========================================================}
type


  PIplImage = ^TIplImage; // defined later
  P2PIplImage = ^PIplImage;
  TIplCallBack = procedure(const Img    : PIplImage;
                                 XIndex : Integer;
                                 YIndex : Integer;
                                 Mode   : Integer); stdcall;
{
   Purpose:        Type of functions for access to external manager of tile
   Parameters:
     Img           - header provided for the parent image
     XIndex,YIndex - indices of the requested tile. They refer to the tile
                     number not pixel number, and count from the origin at (0,0)
     Mode          - one of the following:
        IPL_GET_TILE_TO_READ  - get a tile for reading;
                                tile data is returned in "img->tileInfo->tileData",
                                and must not be changed
        IPL_GET_TILE_TO_WRITE - get a tile for writing;
                                tile data is returned in "img->tileInfo->tileData"
                                and may be changed;
                                changes will be reflected in the image
        IPL_RELEASE_TILE      - release tile; commit writes
   Notes: Memory pointers provided by a get function will not be used after the
          corresponding release function has been called.
}

  PIplTileInfo = ^TIplTileInfo;
  TIplTileInfo = record
    CallBack : TIplCallBack; // callback function
    Id       : Pointer;      // additional identification field
    TileData : PByte;        // pointer on tile data
    Width    : Integer;      // width of tile
    Height   : Integer;      // height of tile
  end;

  PIplROI = ^TIplROI;
  TIplROI = record
    Coi     : Integer;
    XOffset : Integer;
    YOffset : Integer;
    Width   : Integer;
    Height  : Integer;
  end;

  TIplImage = record
    NSize           : Integer;                 // size of iplImage struct
    ID              : Integer;                 // version
    NChannels       : Integer;
    AlphaChannel    : Integer;
    Depth           : Integer;                 // pixel depth in bits
    ColorModel      : array [0..3] of Char;
    ChannelSeq      : array [0..3] of Char;
    DataOrder       : Integer;
    Origin          : Integer;
    Align           : Integer;                 // 4 or 8 byte align
    Width           : Integer;
    Height          : Integer;
    Roi             : PIplROI;
    MaskROI         : PIplImage;               // poiner to maskROI if any
    ImageId         : Pointer;                 // use of the application
    TileInfo        : PIplTileInfo;            // contains information on tiling
    ImageSize       : Integer;                 // useful size in bytes
    ImageData       : PByte;                   // pointer to aligned image
    WidthStep       : Integer;                 // size of aligned line in bytes
    BorderMode      : array [0..3] of Integer;
    BorderConst     : array [0..3] of Integer;
    ImageDataOrigin : PByte;                   // ptr to full, nonaligned image
  end;

  PIplLUT = ^TIplLUT;
  TIplLUT = record
    Num             : Integer;
    Key             : PInteger;
    Value           : PInteger;
    Factor          : PInteger;
    InterpolateType : Integer;
  end;

  PIplColorTwist = ^TIplColorTwist;
  TIplColorTwist = record
    Data         : array [0..15] of Integer;
    ScalingValue : Integer;
  end;

  PIplConvKernel = ^TIplConvKernel;
  P2PIplConvKernel = ^PIplConvKernel;
  TIplConvKernel = record
    NCols   : Integer;
    NRows   : Integer;
    AnchorX : Integer;
    AnchorY : Integer;
    Values  : PInteger;
    NShiftR : Integer;
  end;

  PIplConvKernelFP = ^TIplConvKernelFP;
  TIplConvKernelFP = record
    NCols   : Integer;
    NRows   : Integer;
    AnchorX : Integer;
    AnchorY : Integer;
    Values  : PFloat;
  end;

  TIplFilter = (
    IPL_PREWITT_3x3_V,
    IPL_PREWITT_3x3_H,
    IPL_SOBEL_3x3_V,   //* vertical */
    IPL_SOBEL_3x3_H,   //* horizontal */
    IPL_LAPLACIAN_3x3,
    IPL_LAPLACIAN_5x5,
    IPL_GAUSSIAN_3x3,
    IPL_GAUSSIAN_5x5,
    IPL_HIPASS_3x3,
    IPL_HIPASS_5x5,
    IPL_SHARPEN_3x3);

  POwnMoment = ^TOwnMoment;
  TOwnMoment = record // spatial moment structure:
    Scale : Double;   // value to scale (m,n)th moment
    Value : Double;   // spatial (m,n)th moment
  end;

// spatial moments array
  PIplMomentState = ^TIplMomentState;
  TIplMomentState = array [0..3,0..3] of TOwnMoment;

{==========================================================================
      Section: Wavelet transform constants and types.
 =========================================================================}


{--------------------  Types of wavelet transforms.  ---------------------}
  TIplWtType = (
    IPL_WT_HAAR,
    IPL_WT_DAUBLET,
    IPL_WT_SYMMLET,
    IPL_WT_COIFLET,
    IPL_WT_VAIDYANATHAN,
    IPL_WT_BSPLINE,
    IPL_WT_BSPLINEDUAL,
    IPL_WT_LINSPLINE,
    IPL_WT_QUADSPLINE,
    IPL_WT_TYPE_UNKNOWN
  );

{-----------------------  Filters symmetry type.  ------------------------}
  TIplWtFiltSymm = (
    IPL_WT_SYMMETRIC,
    IPL_WT_ANTISYMMETRIC,
    IPL_WT_ASYMMETRIC,
    IPL_WT_SYMM_UNKNOWN
  );

{---------------------  Filter bank orthogonality.  ----------------------}
  TIplWtOrthType = (
    IPL_WT_ORTHOGONAL,
    IPL_WT_BIORTHOGONAL,
    IPL_WT_NOORTHOGONAL,
    IPL_WT_ORTH_UNKNOWN
  );

{--------------------------  Filter structure  ---------------------------}
  PIplWtFilter = ^TIplWtFilter;
  TIplWtFilter = record
    Taps     : PFloat;         // filter taps
    Len      : Integer;        // length of filter
    Offset   : Integer;        // offset of filter
    Symmetry : TIplWtFiltSymm; // filter symmetry property
  end;

{---------------  Wavelet functions interchange structure  ---------------}
  PIplWtKernel = ^TIplWtKernel;
  TIplWtKernel = record
    WtType      : TIplWtType;     // type of wavelet transform
    Par1        : Integer;        // first param.  (transform order)
    Par2        : Integer;        // second param. (only for biorth. tr.)
    Orth        : TIplWtOrthType; // orthogonality property
    FiltDecLow  : TIplWtFilter;   // low-pass decomposition filter
    FiltDecHigh : TIplWtFilter;   // high-pass decomposition filter
    FiltRecLow  : TIplWtFilter;   // low-pass reconstruction filter
    FiltRecHigh : TIplWtFilter;   // high-pass reconstruction filter
  end;

{---------------------  Noise generators structure  ----------------------}
  TIplNoise = (
    IPL_NOISE_UNIFORM,
    IPL_NOISE_GAUSSIAN
  );

  PIplNoiseParam = ^TIplNoiseParam;
  TIplNoiseParam = record
    Noise   : TIplNoise;
    Seed    : UINT;
    LowInt  : Integer;
    HighInt : Integer;
    LowFlt  : Float;
    HighFlt : Float;
  end;

{==========================================================================
      Section: User function types.
 =========================================================================}

{/////////////////////////////////////////////////////////////////////////
type IplUserFunc
Purpose:    Type of callback functions for user point operation.
            Provides user to write his own code to process each channel of
            srcImage pixel. This function would be passed to iplUserProcess
            function as its last parameter.
Parameters:
  src         - value of src image pixel's channel converted to integer
Returns:        value of dst image pixel's channel. You wouldn't support
                value saturatation, it will be done by iplUserProcess function

Notes: For more information see iplUserProcess function description
/////////////////////////////////////////////////////////////////////////}
TIplUserFunc = function(Src : Integer) : Integer; stdcall;

{/////////////////////////////////////////////////////////////////////////
type IplUserFuncFP
Purpose:    Type of callback functions for user point operation.
            Provides user to write his own code to process each channel
            of srcImage pixel. This function would be passed to
            iplUserProcessFP function as its last parameter.
Parameters:
  src         - value of src image pixel's channel converted to float
Returns:        value of dst image pixel's channel. You wouldn't support
                value saturatation for integer data types, it will be done
                by iplUserProcessFP function

Notes: For more information see iplUserProcessFP function description
/////////////////////////////////////////////////////////////////////////}
TIplUserFuncFP = function(Src : Float) : Float; stdcall;

{/////////////////////////////////////////////////////////////////////////
type IplUserFuncPixel
Purpose:    Type of callback functions for user point operation.
            Provides user to write his own code to process all channels
            of srcImage pixel simultaneously. This function would be
            passed to iplUserProcessPixel function as its last parameter.
            Function may call IPL_ERROR to set IplError status.
Returns:  None
Parameters:
  srcImage  - src image header to access image depth and number of channels
  srcPixel  - pointer to array of src image pixel values.
              Function ought to convert this pointer to an array of src depth.
  dstImage  - dst image header to access image depth and number of channels
  dstPixel  - pointer to array of dst image pixel values.
              Function ought to convert this pointer to an array of dst depth.

Notes: For more information see iplUserProcessPixel function description
/////////////////////////////////////////////////////////////////////////}
TIplUserFuncPixel = procedure(srcImg : PIplImage; srcPixel : Pointer;
                              dstImg : PIplImage; dstPixel : Pointer); stdcall;

{==========================================================================
      Section: Image Creation Functions
 ==========================================================================}

const
  IPL_BORDER_CONSTANT   = 0;
  IPL_BORDER_REPLICATE  = 1;
  IPL_BORDER_REFLECT    = 2;
  IPL_BORDER_WRAP       = 3;

{ Indexes to access IplImage.BorderMode[],IplImage.BorderConst[]          }
  IPL_SIDE_TOP_INDEX    = 0;
  IPL_SIDE_BOTTOM_INDEX = 1;
  IPL_SIDE_LEFT_INDEX   = 2;
  IPL_SIDE_RIGHT_INDEX  = 3;

{ values of argument of iplSetBorderMode(,,border,)                       }
  IPL_SIDE_TOP    = 1 shl IPL_SIDE_TOP_INDEX;
  IPL_SIDE_BOTTOM = 1 shl IPL_SIDE_BOTTOM_INDEX;
  IPL_SIDE_LEFT   = 1 shl IPL_SIDE_LEFT_INDEX;
  IPL_SIDE_RIGHT  = 1 shl IPL_SIDE_RIGHT_INDEX;
  IPL_SIDE_ALL    = IPL_SIDE_RIGHT or
                    IPL_SIDE_TOP or
                    IPL_SIDE_LEFT or
                    IPL_SIDE_BOTTOM;

procedure iplSetBorderMode(Src    : PIplImage; Mode     : Integer;
                           Border : Integer;   ConstVal : Integer); stdcall;
{
Mode    The following modes are supported.
        IPL_BORDER_CONSTANT     The value ConstVal will be used for all pixels.
        IPL_BORDER_REPLICATE    The last row or column will be replicated
                                for the border.
        IPL_BORDER_REFLECT      The last n rows or columns will be reflected
                                in reverse order to create the border.
        IPL_BORDER_WRAP         The required border rows or columns are taken
                                from the opposite side of  the image.
Border  The side that this function is called for. Can be an OR of one or more
        of the following four sides of an image:
        IPL_SIDE_TOP            Top side.
        IPL_SIDE_BOTTOM         Bottom side.
        IPL_SIDE_LEFT           Left side.
        IPL_SIDE_RIGHT          Right side.
        IPL_SIDE_ALL            All sides
        If  no mode has been set for a side, the default IPL_BORDER_CONSTANT
ConstVal   The value to use for the border when the Mode is set
           to IPL_BORDER_CONSTANT.
}

function iplCreateImageHeader(NChannels  : Integer; AlphaChannel : Integer;
                              Depth      : Integer; ColorModel   : PChar;
                              ChannelSeq : PChar;   DataOrder    : Integer;
                              Origin     : Integer; Align        : Integer;
                              Width      : Integer; Height       : Integer;
                              Roi        : PIplROI; MaskROI      : PIplImage;
                              ImageId    : Pointer;
                              TileInfo   : PIplTileInfo) : PIplImage; stdcall;
{
Purpose:    Creates an IPL image header according to the specified attributes.
Returns:    The newly constructed IPL image header.
Parameters:
  NChannels       Number of channels in the image.
  AlphaChannel    Alpha channel number (0 if no alpha channel in image).
  Depth           Bit depth of  pixels. Can be one of
                  IPL_DEPTH_1U,
                  IPL_DEPTH_8U,
                  IPL_DEPTH_8S,
                  IPL_DEPTH_16U,
                  IPL_DEPTH_16S,
                  IPL_DEPTH_32S.
                  IPL_DEPTH_32F.
  ColorModel      A four character array describing the color model,
                  e.g. "RGB", "GRAY", "MSI" etc.
  ChannelSeq      The sequence of channels in the image,
                  e.g. "BGR" for an RGB image.
  DataOrder       IPL_DATA_ORDER_PIXEL or IPL_DATA_ORDER_PLANE.
  Origin          The origin of the image.
                  Can be IPL_ORIGIN_TL or IPL_ORIGIN_BL.
  Align           Alignment of image data.
                  Can be IPL_ALIGN_4BYTES (IPL_ALIGN_DWORD) or
                         IPL_ALIGN_8BYTES (IPL_ALIGN_QWORD) or
                         IPL_ALIGN_16BYTES IPL_ALIGN_32BYTES.
  Width           Width of  the image in pixels.
  Height          Height of  the image in pixels.
  Roi             Pointer to an ROI (region of interest) structure.
                  This can be NULL (implying a region of interest comprising
                  all channels and the entire image area).
  MaskROI         Pointer on mask image
  ImageId         use of the application
  TileInfo        contains information on tiling
}

procedure iplAllocateImage(Image     : PIplImage; DoFill : Integer;
                           FillValue : Integer);           stdcall;
{
Purpose:    Allocates memory for image data according to the specified header.
Parameters:
  Image           An IPL image header with a NULL image data pointer.
                  The image data pointer will be set to newly allocated
                  image data memory after calling this function.
  DoFill          Use a 0 to leave the pixel data uninitialized.
                  Use a not 0 to initialized the pixel data of FillValue
  FillValue       The initial value to use for pixel data.
}

procedure iplAllocateImageFP(Image     : PIplImage; DoFill : Integer;
                             FillValue : Float);             stdcall;
{
Purpose:    Allocates memory for image data according to the specified header.
Parameters:
  Image           An IPL image header (IPL_DEPTH_32F) with a NULL image data
                  pointer.
                  The image data pointer will be set to newly allocated
                  image data memory after calling this function.
  DoFill          Use a 0 to leave the pixel data uninitialized.
                  Use a not 0 to initialized the pixel data of FillValue
  FillValue       The initial value to use for pixel data.
}

function iplCreateImageJaehne(Depth, Width, Height : Integer) : PIplImage; stdcall;
{
Purpose:        Creates a gray (one channel) test image
Returns:        IPL image or NULL
Parameters:
    depth       depth of the image to be created.
                All IPL depths are possible including 32f.
                For the 32f depth a data range is [0..1)
    width       width of the image to be created
    height      height of the image to be created

Notes:    This test image was seen in
          B.Jaehne, Digital Image Processing, third edition, 1995
}

function iplCloneImage(Img : PIplImage) : PIplImage; stdcall;
{
Purpose:    Creates a clone of an image
Returns:    IPL image or NULL
Parameters: img - image to be cloned.

Notes:      The function creates a copy of an image img including
            the field roi by copying. The following fields of the
            created image are set by function
            ID = 0, imageId = NULL, maskROI = NULL, tileInfo = NULL
            All IPL depths are possible including 32f.
}

procedure iplDeallocateHeader(Image : PIplImage); stdcall;
{
Purpose:    deallocate IPL header

Notes:      if image^ImageData!=NULL, then first frees imageData
}

procedure iplDeallocateImage(Image : PIplImage); stdcall;
{
Purpose:    Deallocates (frees) memory for image data pointed to in
            the image header.
Parameters:
  Image     An IPL image header with a pointer to allocated image data memory.

Notes:      The image data pointer will be set to NULL after this
            function executes.
}

const
  IPL_IMAGE_HEADER =  1;
  IPL_IMAGE_DATA   =  2;
  IPL_IMAGE_ROI    =  4;
  IPL_IMAGE_TILE   =  8;
  IPL_IMAGE_MASK   = 16;
  IPL_IMAGE_ALL    =  IPL_IMAGE_HEADER or
                      IPL_IMAGE_DATA   or
                      IPL_IMAGE_TILE   or
                      IPL_IMAGE_ROI    or
                      IPL_IMAGE_MASK;
  IPL_IMAGE_ALL_WITHOUT_MASK = IPL_IMAGE_HEADER or
                               IPL_IMAGE_DATA   or
                               IPL_IMAGE_TILE   or
                               IPL_IMAGE_ROI;

procedure iplDeallocate(Image : PIplImage; Flag : Integer); stdcall;
{
Purpose:    Deallocates or frees memory for image header or data or
            mask ROI or rectangular ROI, etc or all.
Parameters:
  Image         An IPL image header
  Flag          what item to free:
     IPL_IMAGE_HEADER - free header structure
     IPL_IMAGE_DATA   - free image data, set pointer to NULL
     IPL_IMAGE_ROI    - free image roi, set pointer to NULL
     IPL_IMAGE_TILE   - free image tile, set pointer to NULL
     IPL_IMAGE_MASK   - free image maskROI, set pointer to NULL
     IPL_IMAGE_ALL    - free image data, roi, header, maskROI, tile
     IPL_IMAGE_ALL_WITHOUT_MASK
                      - as well as IPL_IMAGE_ALL, but maskROI does not release
}

function iplCreateROI(Coi     : Integer;
                      XOffset : Integer; YOffset :  Integer;
                      Width   : Integer;
                      Height  : Integer) : PIplROI; stdcall;
{
Purpose:    Allocates and sets the region of interest (ROI) structure.
Returns:    Newly constructed ROI structure.
Parameters:
  Coi            The channel region of interest.
                 It can be set to 0 (for all channels) or a specific channel number.
  XOffset,
  YOffset        The offset from the origin of the rectangular region.
  Height,
  Width          The size of the rectangular region.
}

procedure iplSetROI(Roi     : PIplROI; Coi     : Integer;
                    XOffset : Integer; YOffset : Integer;
                    Width   : Integer;
                    Height  : Integer);          stdcall;
{
Purpose:    Sets the IplROI structure fields.
Parameters:
  Roi            The ROI structure to modify.
  Coi            The channel region of interest.
                 It can be set to 0 (for all channels) or a specific
                 channel number.
  XOffset,
  YOffset        The offset from the origin of the rectangular region.
  Height,
  Width          The size of the rectangular region.
}

procedure iplDeleteROI(Roi : PIplROI); stdcall;
{
Purpose:    Deletes ROI structure
Parameters:
  Roi         The ROI structure to be deleted.
}

function iplCreateTileInfo(CallBack : TIplCallBack;
                           Id       : Pointer;
                           Width    : Integer;
                           Height   : Integer) : PIplTileInfo; stdcall;
{
Purpose:    Creates the IplTileInfo structure.
Returns:    Newly constructed TileInfo structure.
Parameters:
  CallBack           callback function
  Id                 additional identification field
  Width              width of tile
  Height             height of tile
}

procedure iplSetTileInfo(TileInfo : PIplTileInfo;
                         CallBack : TIplCallBack;
                         Id       : Pointer;
                         Width    : Integer;
                         Height   : Integer); stdcall;
{
Purpose:    Sets attributes for an existing IplTileInfo structure.
Parameters:
  TileInfo           The TileInfo structure to modify.
  CallBack           callback function
  Id                 additional identification field
  Width              width of tile
  Height             height of tile
}

procedure iplDeleteTileInfo(TileInfo : PIplTileInfo); stdcall;
{
Purpose:    Deletes the IplTileInfo structure.
Parameters:
  TileInfo         The pointer to the TIplTileInfo structure.
}

{==========================================================================
      Section: Windows* DIB Conversion Functions
 ==========================================================================}

function iplTranslateDIB(Dib       : PBitmapInfoHeader;
                     var CloneData : BOOL) : PIplImage; stdcall;
{
Purpose:    Translates a DIB image into an IPL image.
Returns:    The constructed IPL image.
Parameters:
  Dib            The DIB image.
  CloneData      A boolean, result of work of the function. If true, the image
                 data pointer in IPL image is made to point to the DIB image data.
                 Otherwise the DIB image data was converting to the IPL image data.
}

procedure iplConvertFromDIB(Dib   : PBitmapInfoHeader;
                            Image : PIplImage); stdcall;
{
Purpose:    Converts a DIB image to an IPL image with specified attributes.
Parameters:
  Dib            The input DIB image.
  Image          The IPL image header with specified attributes.

Notes:           If the data pointer is NULL, image data memory
                 will be allocated and the pointer set to it.
}

{----------  Consts of Palette conversion for iplConvertToDIB*  ----------}
type
  TIplPalConversion = (
    IPL_PALCONV_NONE,
    IPL_PALCONV_POPULATE,
    IPL_PALCONV_MEDCUT);

procedure iplConvertToDIB(Image             : PIplImage;
                          Dib               : PBitmapInfoHeader;
                          Dither            : Integer;
                          PaletteConversion : TIplPalConversion); stdcall;
{
Purpose:    Converts an IPL image to a DIB image with specified attributes.
Parameters:
  Image          The input IPL image.
  Dib            The output DIB image.
  Dither         The dithering algorithm to use if applicable.
                 Dithering will be done if  the bit depth in the DIB
                 is less than that of the IPL image.
                 The following algorithms are supported:
                 IPL_DITHER_NONE     No dithering is done.
                                     The most significant bits in the IPL
                                     image pixel data are retained.
                 IPL_DITHER_STUCKEY  The stuckey dithering algorithm is used.
  PaletteConversion
                 Applicable when the DIB is a palette image.
                 Specifies the palette algorithm to use when converting
                 the IPL absolute color image.
                 The following options are supported:
                 IPL_PALCONV_NONE     The existing palette in the DIB is used.
                 IPL_PALCONV_POPULATE The popularity palette conversion
                                      algorithm is used.
                 IPL_PALCONV_MEDCUT   The median cut algorithm palette conversion
                                      algorithm is used.
}

function iplConvertFromDIBSep(Dib               : PBitmapInfoHeader;
                              DibData           : PByte;
                              Image             : PIplImage) : IPLStatus; stdcall;
{
Purpose:    Same as iplConvertFromDIB, but uses separate
            parameters for DIB header and data.
Returns:    IPLStatus
Parameters:
     Dib       - The input DIB image header.
     DibData   - The input DIB image data.
     Image     - The IPL image header with specified attributes.
                 If the data pointer is NULL, image data memory
                 will be allocated and the pointer set to it.
}

function iplConvertToDIBSep(Image : PIplImage;
                            Dib               : PBitmapInfoHeader;
                            DibData           : PByte;
                            Dither            : Integer;
                            PaletteConversion : TIplPalConversion) : IPLStatus; stdcall;
{
Purpose:    Same as iplConvertToDIB, but uses separate
            parameters for DIB header and data.
Returns:    IPLStatus
Parameters:
  Image     - The input IPL image.
  Dib       - The output DIB image header.
  DibData   - The output DIB image data.
  Dither -
         The dithering algorithm to use if applicable.
         Dithering will be done if  the bit depth in the DIB
         is less than that of the IPL image.
         The following algorithms are supported for all dither
         type (see iplReduceBits).
  PaletteConversion -
         Applicable when the DIB is a palette image.
         Specifies the palette algorithm to use when converting
         the IPL absolute color image.
         The following options are supported:
             IPL_PALCONV_NONE     - The existing palette in the DIB is used.
             IPL_PALCONV_POPULATE - The popularity palette conversion
                                    algorithm is used.
             IPL_PALCONV_MEDCUT   - The median cut algorithm palette conversion
                                    algorithm is used.
}

{==========================================================================
      Section: Conversion and Data Exchange Functions
 ==========================================================================}

procedure iplCopy(SrcImage, DstImage : PIplImage); stdcall;
{
Purpose:    Copies image data from one image to another.
Parameters:
     SrcImage - The source image.
     DstImage - The resultant image.
}

procedure iplExchange(Image1, Image2 : PIplImage); stdcall;
{
Purpose:    Exchanges image data between two images.
Parameters:
     Image1 - The first image.
     Image2 - The second image.
}

procedure iplSet(  Image : PIplImage; Value : Integer); stdcall;
procedure iplSetFP(Image : PIplImage; Value : Float);   stdcall;
{
Purpose:    Sets a value for an image’s pixel data.
Parameters:
     Image     - An image header with allocated image data.
     Value     - The value to set the pixel data.
}

procedure iplPutPixel(Img : PIplImage; X, Y : Integer; var Pixel); stdcall;
{
Purpose:     Sets a value of an image's pixel.
Parameters:
  Img        - rezult image in IPL-format.
  X,Y        - indexis of pixel.
  Pixel      - pointer for values of pixel
Notes:
  ignored:
               img->colorModel
               img->channelSeq
               img->roi
               img->maskROI
}

procedure iplGetPixel(Img : PIplImage; X, Y : Integer; var Pixel); stdcall;
{
Purpose:     Retrieves a value of an image's pixel.
Parameters:
  Img        - source image in IPL-format.
  X,Y        - indexis of the requested pixel.
  Pixel      - pointer for values of pixel
Notes:
  ignored:
               img->colorModel
               img->channelSeq
               img->roi
               img->maskROI
}

procedure iplConvert(SrcImage, DstImage : PIplImage); stdcall;
{
Purpose:    Converts source image data to resultant image according to
            the image headers.
Parameters:
     SrcImage - The source image.
     DstImage - The resultant image.
}

function iplScale(SrcImage, DstImage : PIplImage) : IPLStatus; stdcall;
{
Purpose:
       1)  dst = a + b * src;
           a = type_min_dst - b * type_min_src;
           b = (type_max_dst - type_min_dst) / (type_max_src - type_min_src).

       2)  src(8s,8u,16s,16u,32s) ==> dst(8s,8u,16s,16u,32s);
           [type_src_min...type_src_max] ==> [type_dst_min...type_dst_max];
           src_depth != dst_depth.
Parameters:
       srcImage        The source image.
       dstImage        The resultant image.
}

function iplScaleFP(SrcImage, DstImage : PIplImage;
                    MinVal, MaxVal     : Float) : IPLStatus; stdcall;
{
Purpose:
                 1)  dst = a + b*src;
                     a = min_dst - b*min_src;
                     b = (max_dst - min_dst) / (max_src - min_src).

                 2)  src(32f) ==> dst(8s,8u,16s,16u,32s) + saturation;
                         [minVal...maxVal] ==> [type_dst_min...type_dst_max].

                 3)  src(8s,8u,16s,16u,32s) ==> dst(32f);
                         [type_src_min...type_src_max] ==> [minVal...maxVal].

                 4)  src_depth != dst_depth.
Parameters:
        srcImage                The source image.
        dstImage                The resultant image.
        [minVal...maxVal]       Range for depth 32f.
}

{==========================================================================
      Section: Arithmetic Functions
 ==========================================================================}

{-------------------------  Monadic Operations  ---------------------------}

procedure iplAddS(SrcImage, DstImage : PIplImage; Value : Integer); stdcall;
{
Purpose:    Adds a constant to pixel values of the source image.
Parameters:
  SrcImage       The source image.
  DstImage       The resultant image.
  Value          The value to increase the pixel values by.
}

procedure iplAddSFP(SrcImage, DstImage : PIplImage; Value : Float); stdcall;
{
Purpose:    Adds a constant to pixel values of the source image.
Parameters:
  SrcImage       The source image.
  DstImage       The resultant image.
  Value          The value to increase the pixel values by.
}

procedure iplSubtractS(SrcImage, DstImage : PIplImage;
                       Value : Integer; Flip : BOOL); stdcall;
{
Purpose:    Subtracts a constant from pixel values, or pixel values
            from a constant.
Parameters:
  SrcImage       The source image.
  DstImage       The resultant image.
  Value          The value to decrease the pixel values by.
  Flip           A boolean that is used to change the order of subtraction.
                 If false the result pixel value is computed as
                    result = pixel_value - value, where pixel_value is the input
                                                  pixel value.
                 If true, the result pixel value is computed as
                    result = value - pixel_value.
}

procedure iplSubtractSFP(SrcImage, DstImage : PIplImage;
                         Value : Float; Flip : BOOL); stdcall;
{
Purpose:    Subtracts a constant from pixel values, or pixel values
            from a constant.
Parameters:
  SrcImage       The source image.
  DstImage       The resultant image.
  Value          The value to decrease the pixel values by.
  Flip           A boolean that is used to change the order of subtraction.
                 If false the result pixel value is computed as
                    result = pixel_value - value, where pixel_value is the input
                                                  pixel value.
                 If true, the result pixel value is computed as
                    result = value - pixel_value.
}

procedure iplMultiplyS(SrcImage, DstImage : PIplImage; Value : Integer); stdcall;
{
Purpose:    Multiplies pixel values by a constant.
Parameters:
  SrcImage       The source image.
  DstImage       The resultant image.
  Value          An integer value by which to multiply the pixel values with.
}

procedure iplMultiplySFP(SrcImage, DstImage : PIplImage;
                         Value : Float); stdcall;
{
Purpose:    Multiplies pixel values by a constant.
Parameters:
  SrcImage       The source image.
  DstImage       The resultant image.
  Value          A float value by which to multiply the pixel values with.
}

procedure iplMultiplySScale(SrcImage, DstImage : PIplImage; Value : Integer); stdcall;
{
Purpose:    Multiplies pixel values by a constant and scales the products.
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
  Value           A positive value to multiply the pixel values with
                  DstImage  = (SrcImage  * Value) / VAL_MAX
Notes:            Value becomes 0 <= value <= VAL_MAX
}

procedure iplAbs(SrcImage, DstImage : PIplImage); stdcall;
{
Purpose:    Computes absolute pixel values of the image.
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
}

procedure iplSquare(SrcImage, DstImage : PIplImage); stdcall;
{
Purpose:    Squares the pixel values of the image.
Parameters:
  SrcImage       The source image.
  DstImage       The resultant image.
}

{--------------------------  Dyadic Operations  ---------------------------}

procedure iplAdd(SrcImageA, SrcImageB, DstImage : PIplImage); stdcall;
{
Purpose:    Combines corresponding pixels of two images by addition.
Parameters:
  SrcImageA       The first source image.
  SrcImageB       The second source image.
  DstImage        The resultant image obtained as
                  DstImage  = SrcImageA  + SrcImageB.
}

procedure iplSubtract(SrcImageA, SrcImageB, DstImage : PIplImage); stdcall;
{
Purpose:    Combines corresponding pixels of two images by subtraction.
Parameters:
  SrcImageA       The first source image.
  SrcImageB       The second source image.
  DstImage        The resultant image obtained as:
                  DstImage  = SrcImageA  - SrcImageB
}

procedure iplMultiply(SrcImageA, SrcImageB, DstImage : PIplImage); stdcall;
{
Purpose:    Combines corresponding pixels of two images by multiplication.
Parameters:
  SrcImageA       The first source image.
  SrcImageB       The second source image.
  DstImage        The resultant image obtained as
                  DstImage  = SrcImageA  * SrcImageB.
}

procedure iplMultiplyScale(SrcImageA, SrcImageB, DstImage : PIplImage); stdcall;
{
Purpose:    Multiplies pixel values of two images and scales the products.
Parameters:
  SrcImageA       The first source image.
  SrcImageB       The second source image.
  DstImage        The resultant image obtained as
                  DstImage  = (SrcImageA  * SrcImageB) / VAL_MAX
Notes:      The function is implemented only for 8-bit and 16-bit
            unsigned data types.
}

{==========================================================================
      Section: Logical Functions
 ==========================================================================}

{-------------------------  Monadic Operations  ---------------------------}

procedure iplNot(SrcImage, DstImage : PIplImage); stdcall;
{
Purpose:    Performs a bitwise NOT operation on each pixel.
Parameters:
  SrcImage       The source image.
  DstImage       The resultant image.
}

procedure iplLShiftS(SrcImage, DstImage : PIplImage; nShift : UINT); stdcall;
{
Purpose:    Shifts pixel values’ bits to the left.
Parameters:
  SrcImage       The source image.
  DstImage       The resultant image.
  nShift         The number of bits to shift each pixel value to the left to.
}

procedure iplRShiftS(SrcImage, DstImage : PIplImage; nShift : UINT); stdcall;
{
Purpose:    Shifts pixel values’ bits to the right.
Parameters:
  SrcImage       The source image.
  DstImage       The resultant image.
  nShift         The number of bits to shift each pixel value to the right to.
}

procedure iplAndS(SrcImage, DstImage : PIplImage; Value : UINT); stdcall;
{
Purpose:    Performs a bitwise AND operation of each pixel with a constant.
Parameters:
  SrcImage       The source image.
  DstImage       The resultant image.
  Value          The bit sequence used to perform the bitwise operation
                 on each pixel. Only the number of bits corresponding to the data
                 type of the image are used.
}

procedure iplOrS(SrcImage, DstImage : PIplImage; Value : UINT); stdcall;
{
Purpose:    Performs a bitwise OR operation of each pixel with a constant.
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
  Value           The bit sequence used to perform the bitwise operation
                  on each pixel. Only the number of bits corresponding
                  to the data type of the image are used.
}

procedure iplXorS(SrcImage, DstImage : PIplImage; Value : UINT); stdcall;
{
Purpose:    Performs a bitwise XOR operation of each pixel with a constant.
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
  Value           The bit sequence used to perform the bitwise operation
                  on each pixel. Only the number of bits corresponding to the data
                  type of the image are used.
}

{--------------------------  Dyadic Operations  ---------------------------} 

procedure iplAnd(SrcImageA, SrcImageB, DstImage : PIplImage); stdcall;
{
Purpose:    Combines corresponding pixels of two images by a bitwise AND
            operation.
Parameters:
  SrcImageA       The first source image.
  SrcImageB       The second source image.
  DstImage        The image resulting from the bitwise operation between input
                  images SrcImageA and SrcImageB.
}

procedure iplOr(SrcImageA, SrcImageB, DstImage : PIplImage); stdcall;
{
Purpose:    Combines corresponding pixels of two images by a
            bitwise OR operation.
Parameters:
  SrcImageA       The first source image.
  SrcImageB       The second source image.
  DstImage        The image resulting from the bitwise operation
                  between input images SrcImageA and SrcImageB.
}

procedure iplXor(SrcImageA, SrcImageB, DstImage : PIplImage); stdcall;
{
Purpose:    Combines corresponding pixels of two images by a
            bitwise XOR operation.
Parameters:
  SrcImageA       The first source image.
  SrcImageB       The second source image.
  DstImage        The image resulting from the bitwise operation
                  between input images SrcImageA and SrcImageB.
}

{==========================================================================
      Section: Alpha-blending Functions
 ==========================================================================}

const
  IPL_COMPOSITE_OVER   = 0;
  IPL_COMPOSITE_IN     = 1;
  IPL_COMPOSITE_OUT    = 2;
  IPL_COMPOSITE_ATOP   = 3;
  IPL_COMPOSITE_XOR    = 4;
  IPL_COMPOSITE_PLUS   = 5;

procedure iplAlphaComposite(SrcImageA, SrcImageB, DstImage : PIplImage;
                            CompositeType : Integer;
                            AlphaImageA, AlphaImageB, AlphaImageDst : PIplImage;
                            PremulAlpha, DivideMode : BOOL); stdcall;

procedure iplAlphaCompositeC(SrcImageA, SrcImageB, DstImage : PIplImage;
                             CompositeType : Integer;
                             aA, aB        : Integer;
                             PremulAlpha, DivideMode : BOOL); stdcall;

{
Name:       iplAlphaComposite, iplAlphaCompositeC
Purpose:    Composite two images using alpha (opacity) values.
Parameters:
  SrcImageA       The foreground image.
  SrcImageB       The background image.
  DstImage        The resultant image.
  CompositeType   The type of composition to perform.
  aA              The constant alpha value to use for the source image SrcImageA.
                  Should be a positive number.
  aB              The constant alpha value to use for the source image SrcImageB.
                  Should be a positive number.
  AlphaImageA     The image to use as the alpha channel for SrcImageA.
  AlphaImageB     The image to use as the alpha channel for SrcImageB.
  AlphaImageDst   The image to use as the alpha channel for DstImage.
  PremulAlpha     Indicates that the input images contain
                  premultiplied alpha values.
  DivideMode      Normally set to false. When true, the result pixel color
                  (in table "Image Compositing Operations") is further divided
                  by the result alpha value to get the final result pixel color.
Notes:            value becomes aA <= VAL_MAX, aB <= VAL_MAX
}

procedure iplPreMultiplyAlpha(Image : PIplImage; AlphaValue : Integer); stdcall;
{
Purpose:    Pre-multiplies alpha values of an image.
Parameters:
  Image           The image for which the alpha premultiplication is done.
  AlphaValue      The global alpha value to use in the range 0 to VAL_MAX.
                  If this value is negative (e.g. -1) the internal alpha channel
                  of the image is used (it is an error if an alpha channel does
                  not exist).
Notes:            value becomes alphaValue <= VAL_MAX
}

{==========================================================================
      Section: Filtering Functions
 ==========================================================================}

{------------------------  Management of kernels  -------------------------}

function iplCreateConvKernel(NCols,
                             NRows     : Integer;
                             AnchorX,
                             AnchorY   : Integer;
                             Values    : PInteger;
                             NShiftR   : Integer) : PIplConvKernel;   stdcall;
function iplCreateConvKernelFP(NCols,
                               NRows   : Integer;
                               AnchorX,
                               AnchorY : Integer;
                               Values  : PFloat)  : PIplConvKernelFP; stdcall;
function iplCreateConvKernelChar(NCols,
                                 NRows    : Integer;
                                 AnchorX,
                                 AnchorY  : Integer;
                                 Values   : PChar;
                                 NShiftR  : Integer) : PIplConvKernel; stdcall;
{
Name:       iplCreateConvKernel, iplCreateConvKernelFP,
            iplCreateConvKernelChar
Purpose:    Creates a convolution kernel.
Returns:    A pointer to the convolution kernel structure IplConvKernel.
Parameters:
  NCols           The number of columns in the convolution kernel.
  NRows           The number of rows in the convolution kernel.
  AnchorX,
  AnchorY         The [x,y] coordinates of the anchor cell in the kernel.
  NShiftR         The number of  bits to shift (to the right) the resulting
                  output pixel of each convolution.
Notes:      iplCreateConvKernelChar used only to convert from IPL1.1 to
            IPL2.0. It just convert the char kernel values into integer
            which now used in IPL2.0 to create kernel structures.
}

procedure iplGetConvKernel(Kernel           : PIplConvKernel;
                       var NCols, NRows     : Integer;
                       var AnchorX, AnchorY : Integer;
                           Values           : PInteger;
                       var NShiftR          : Integer); stdcall;
procedure iplGetConvKernelFP(Kernel         : PIplConvKernelFP;
                       var NCols, NRows     : Integer;
                       var AnchorX, AnchorY : Integer;
                           Values           : PFloat);  stdcall;
procedure iplGetConvKernelChar(Kernel           : PIplConvKernel;
                           var NCols, NRows     : Integer;
                           var AnchorX, AnchorY : Integer;
                               Values           : PChar;
                           var NShiftR          : Integer); stdcall;
{
Name:       iplGetConvKernel, iplGetConvKernelFP, iplGetConvKernelChar
Purpose:    Reads the attributes of a convolution kernel.
Parameters:
  Kernel          The kernel to get the attributes for.
                  The attributes are returned in the remaining arguments.
  NCols           A pointer to the the number of columns in the convolution kernel.
                  Set by the function.
  NRows           A pointer to the number of rows in the convolution kernel.
                  Set by the function.
  AnchorX,
  AnchorY         Pointers to the [x,y] coordinates of the anchor cell
                  in the kernel.
  NShiftR         A pointer to the number of  bits to shift (to the right)
                  the resulting output pixel of each convolution.
                  Set by the function.
Notes:      iplGetConvKernelChar used only to convert from IPL1.0 to IPL2.0.
            It just convert the integer kernel values into char which
            earlier used in IPL1.1 while getting kernel values.
            Function works correctly only if kernel values lies
            in interval [-256,255].
}

procedure iplDeleteConvKernel(  Kernel : PIplConvKernel);   stdcall;
procedure iplDeleteConvKernelFP(Kernel : PIplConvKernelFP); stdcall;
{
Name:       iplDeleteConvKernel, iplDeleteConvKernelFP
Purpose:    Deletes a convolution kernel.
Parameters:
  Kernel   - The kernel to delete.
}

{---------------------------  Linear Filters  -----------------------------}

procedure iplBlur(SrcImage, DstImage : PIplImage;
                  NCols, NRows       : Integer;
                  AnchorX, AnchorY   : Integer); stdcall;
{
Purpose:    Applies simple neighborhood averaging filter to blur the image.
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
  NCols           Number of columns in the neighbourhood to use.
  NRows           Number of rows in the neighbourhood to use.
  AnchorX,
  AnchorY         The [x,y] coordinates of the anchor cell in the neighbourhood.
}

const
  IPL_SUM       = 0;
  IPL_SUMSQ     = 1;
  IPL_SUMSQROOT = 2;
  IPL_MAX       = 3;
  IPL_MIN       = 4;

procedure iplConvolve2D(SrcImage, DstImage : PIplImage;
                        Kernel             : PIplConvKernel;
                        NKernels           : Integer;
                        CombineMethod      : Integer); stdcall;
procedure iplConvolve2DFP(SrcImage, DstImage : PIplImage;
                          Kernel             : PIplConvKernelFP;
                          NKernels           : Integer;
                          CombineMethod      : Integer); stdcall;
{
Name:       iplConvolve2D, iplConvolve2DFP
Purpose:    Convolves an image with one or more convolution kernels.
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
  Kernel          A pointer to an array of pointers to convolution kernels.
                  The length of the array is NKernels. Can be one or more kernels.
  NKernels        The number of kernels in the array kernel.
  CombineMethod   The way in which the results of applying each kernel
                  should be combined. This argument is ignored
                  when a single kernel is used. The following combinations
                  are supported:
                  IPL_SUM         Sums the results.
                  IPL_SUMSQ       Sums the squares of the  results.
                  IPL_SUMSQROOT   Sums the squares of the results
                                  and then takes the sqaure root.
                  IPL_MAX         Takes the maximum of the results.
                  IPL_MIN         Takes the minimum of the results.
}

function iplFixedFilter(SrcImage, DstImage : PIplImage;
                        Filter             : TIplFilter) : IPLStatus; stdcall;
{
Purpose:    It is used to convolve an image with one of the predefined kernels.
Returns:    IPLStatus, zero if the execution is completed successfully,
            and a non-zero integer if an error occurred.
Parameters:
  SrcImage      The source image.
  DstImage      The resultant image.
  Filter        One of the predefined kernels:
                       IPL_PREWITT_3x3_V=0,
                       IPL_PREWITT_3x3_H,
                       IPL_SOBEL_3x3_V,
                       IPL_SOBEL_3x3_H,
                       IPL_LAPLACIAN_3x3,
                       IPL_LAPLACIAN_5x5,
                       IPL_GAUSSIAN_3x3,
                       IPL_GAUSSIAN_5x5,
                       IPL_HIPASS_3x3,
                       IPL_HIPASS_5x5,
                       IPL_SHARPEN_3x3
}

procedure iplConvolveSep2D(  SrcImage, DstImage : PIplImage;
                             xKernel, yKernel   : PIplConvKernel); stdcall;
procedure iplConvolveSep2DFP(SrcImage, DstImage : PIplImage;
                             xKernel, yKernel   : PIplConvKernelFP); stdcall;
{
Name:       iplConvolveSep2D, iplConvolveSep2DFP
Purpose:    Convolves an image with a separable convolution kernel.
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
  xKernel         The x or row kernel. Should contain only one row.
  ykernel         The y or column kernel. Should contain only one column.
}

{-------------------------  Non Linear Filters  ---------------------------}

procedure iplMedianFilter(SrcImage, DstImage : PIplImage;
                          NCols, NRows       : Integer;
                          AnchorX, AnchorY   : Integer); stdcall;
{
Purpose:    Apply a median filter to the image.
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
  NCols           Number of columns in the neighbourhood to use.
  NRows           Number of rows in the neighbourhood to use.
  AnchorX,
  AnchorY         The [x,y] coordinates of the anchor cell in the neighbourhood.
}

procedure iplColorMedianFilter(SrcImage, DstImage : PIplImage;
                               NCols, NRows       : Integer;
                               AnchorX, AnchorY   : Integer); stdcall;
{
Purpose:    Apply a color median filter to the image.
Parameters:
  SrcImage - The source image.
  DstImage - The resultant image.
  NCols    - Number of columns in the neighbourhood to use.
  NRows    - Number of rows in the neighbourhood to use.
  AnchorX,
  AnchorY  - The [x,y] coordinates of the anchor cell in the neighbourhood.
}

procedure iplMaxFilter(SrcImage, DstImage : PIplImage;
                       NCols, NRows       : Integer;
                       AnchorX, AnchorY   : Integer); stdcall;
{
Purpose:    Apply a max filter to the image.
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
  NCols           Number of columns in the neighbourhood to use.
  NRows           Number of rows in the neighbourhood to use.
  AnchorX,
  AnchorY         The [x,y] coordinates of the anchor cell in the neighbourhood.
}

procedure iplMinFilter(SrcImage, DstImage : PIplImage;
                       NCols, NRows       : Integer;
                       AnchorX, AnchorY   : Integer); stdcall;
{
Purpose:    Apply a min filter to the image.
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
  NCols           Number of columns in the neighbourhood to use.
  NRows           Number of rows in the neighbourhood to use.
  AnchorX,
  AnchorY         The [x,y] coordinates of the anchor cell in the neighbourhood.
}

{==========================================================================
      Section: Fast Fourier and Discrete Cosine Transforms
 ==========================================================================}

{-----------------------  Fast Fourier Transform  -------------------------}

const
  IPL_FFT_Forw      =   1;
  IPL_FFT_Inv       =   2;
  IPL_FFT_NoScale   =   4;
  IPL_FFT_UseInt    =  16;
  IPL_FFT_UseFloat  =  32;
  IPL_FFT_Free      = 128;

procedure iplRealFft2D(SrcImage, DstImage : PIplImage; Flags : Integer); stdcall;
{
Purpose:    Computes the forward or inverse 2D FFT of an image.
Parameters:
  SrcImage        The source image. Any mask ROI specified will be ignored.
  DstImage        The resultant image in CcsPerm2D format containing
                  the Fourier coefficients. This image cannot be the same
                  as the input image SrcImage. Any mask ROI specified will be
                  ignored. This image should be a multi-channel image containing
                  as many channels as SrcImage. The data type for the image
                  should be 8, 16 or 32 bits.
  Flags           Specifies how the FFT should be performed.
                  Integer number in which every bit have next specifications:
                  IPL_FFT_Forw       - forward transform,
                  IPL_FFT_Inv        - inverse transform,
                  IPL_FFT_NoScale    - in inverse transform absent to scale,
                  IPL_FFT_UseInt     - use only int   core,
                  IPL_FFT_UseFloat   - use only float core,
                  IPL_FFT_Free       - only free all working arrays and exit,
}

procedure iplCcsFft2D(SrcImage, DstImage : PIplImage; Flags : Integer); stdcall;
{
Purpose:    Computes the forward or inverse 2D FFT of an image
            in complex-conjugate format.
Parameters:
  SrcImage        The source image in CcsPerm2D format.
                  Any mask ROI specified will be ignored.
  DstImage        The resultant image. This image cannot be the same as
                  the input image SrcImage.  Any mask ROI specified will be ignored.
                  This image should be a multi-channel image containing as many
                  channels as SrcImage.
  Flags           Specifies how the FFT should be performed.
                  Integer number in which every bit have next specifications:
                  IPL_FFT_Forw       - forward transform,
                  IPL_FFT_Inv        - inverse transform,
                  IPL_FFT_NoScale    - in inverse transform absent to scale,
                  IPL_FFT_UseInt     - use only int   core,
                  IPL_FFT_UseFloat   - use only float core,
                  IPL_FFT_Free       - only free all working arrays and exit,
}

procedure iplMpyRCPack2D(SrcA, SrcB, Dst : PIplImage); stdcall;
{
Purpose:    Myltiply two IPL-images in "RCPack2D" format
            and put result into destination image in "RCPack2D" format,
            for 8S, 16S, 32S, 32F depth,
            without ROI, and with ROI,
            without maskROI, for not tiled images
Parameters:
  SrcA   - First  source image in "RCPack2D" format.
  SrcB   - Second sourse image in "RCPack2D" format.
  Dst    - Destination image   in "RCPack2D" format.
}

{----------------------  Discrete Cosine Transform  -----------------------}

const
  IPL_DCT_Forward   =  1;
  IPL_DCT_Inverse   =  2;
  IPL_DCT_Free      =  8;
  IPL_DCT_UseInpBuf = 16;

procedure iplDCT2D(SrcImage, DstImage : PIplImage; Flags : Integer); stdcall;
{
Purpose:    Computes the forward or inverse 2D DCT of an image.
Parameters:
  SrcImage        The source image. Any mask ROI specified will be ignored.
  DstImage        The resultant image containing the DCT coefficients.
                  This image cannot be the same as the input image SrcImage.
                  Any mask ROI specified will be ignored.
                  This image should be a multi-channel image containing
                  as many channels as SrcImage. The data type for the image
                  should be 8, 16 or 32 bits.
  Flags    - Specifies how to perform the DCT. This is an
             integer whose bits can be assigned the following
             values using bitwise logical OR:
                  IPL_DCT_Forward   - Do forward transform.
                  IPL_DCT_Inverse   - Do inverse transform.
                  IPL_DCT_Free      - Only free all working arrays and exit.
                  IPL_DCT_UseInpBuf - Use the input image array for the
                                      intermediate calculations.
                                      The performance of DCT increases, but
                                      the input image is destroyed. You may
                                      use this value only if both the source
                                      and destination image data types are
                                      16-bit signed.
}

{==========================================================================
      Section: Morphological Operations
 ==========================================================================}

procedure iplErode(SrcImage, DstImage : PIplImage;
                   NIterations        : Integer); stdcall;
{
Purpose:    Erodes the image.
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
  NIterations     The number of times to erode the image.
}

procedure iplDilate(SrcImage, DstImage : PIplImage;
                    NIterations        : Integer); stdcall;
{
Purpose:    Dilates the image.
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
  NIterations     The number of times to erode the image.
}

procedure iplOpen(SrcImage, DstImage : PIplImage;
                  NIterations        : Integer); stdcall;
{
Purpose:    Opens the image by performing erosions followed by dilations.
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
  NIterations     The number of times to erode and dilate the image.
}

procedure iplClose(SrcImage, DstImage : PIplImage;
                   NIterations        : Integer); stdcall;
{
Purpose:    Closes the image by performing dilations followed by erosions.
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
  NIterations     The number of times to dilate and erode the image.
}

{==========================================================================
      Section: Color Space Conversion
 ==========================================================================}

const
  IPL_JITTER_NONE    =  0;
  IPL_DITHER_NONE    =  1;
  IPL_DITHER_FS      =  2;
  IPL_DITHER_JJH     =  4;
  IPL_DITHER_STUCKEY =  8;
  IPL_DITHER_BAYER   = 16;

procedure iplReduceBits(SrcImage, DstImage : PIplImage;
                        Noise, DitherType  : Integer;
                        Levels             : Integer); stdcall;
{
Purpose:    Transforms Image of a higher bit resolution to a lower bit resolution ,or
            halftoning to multiple output levels.
Arguments:
  SrcImage  - The source image.
  DstImage  - The resultant image.
  Noise     - the number specifying the noise added,is set in percentage of a range [0..100]
  Levels    - the number of output levels for halftoning (dithering)[2.. MAX_LEVELS - 1],
              where  MAX_LEVELS is  0x01 << depth and depth is depth of the destination image
  DitherType  -  the type of dithering to be used. The following are allowed
        IPL_DITHER_NONE     no dithering is done
        IPL_DITHER_STUCKEY  Stuckey's dithering algorithm
        IPL_DITHER_FS       Floid-Steinberg's dithering algorithm
        IPL_DITHER_JJH      Jarvice-Judice-Hinke's dithering algorithm
        IPL_DITHER_BAYER    Bayer's dithering algorithm
  32 bit per channel  -> 32,16, 8, 1  bit per channel
  16 bit per channel  -> 16, 8, 1     bit per channel
  8  bit per channel  ->  8, 1        bit per channel( 1u for Gray only)
Notes: Algorithm taken from "Graphics Gems, Vol.1,1990.
  reducing uses the equation dst = src*(((1<<n) -1)/((1<<m) - 1)),
  where m-source bit depth, n - destination bit depth.
}

procedure iplBitonalToGray(SrcImage, DstImage : PIplImage;
                           ZeroScale : Integer;
                           OneScale  : Integer); stdcall;
{
Purpose:    Converts a bitonal image to gray scale.
Parameters:
  SrcImage  - The source image.
  DstImage  - The resultant image.
  ZeroScale - The value that zero pixels of the source image
              should have in the resultant image.
  OneScale  - The value given to a resultant pixel if the
              corresponding input pixel is 1.
}

procedure iplColorToGray(SrcImage, DstImage : PIplImage); stdcall;
{
Purpose:    Converts a color image to gray scale.
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
}

procedure iplGrayToColor(SrcImage, DstImage     : PIplImage;
                         FractR, FractG, FractB : Float); stdcall;
{
Purpose:    Converts a gray scale to color image (pseudo color).
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
  FractR,
  FractG,
  FractB          The red, green and blue intensities for image
                  reconstruction.
}

{------------------  Color model conversion functions  --------------------}

{
Purpose:    Converts from one color model to other
Notes:      See documentation for information about color models,
  and also, the iplRGB2YUV and iplYUV2RGB functions have the special features:
  - a destination image of the iplRGB2YUV function can be of the plane order if a
  source image is of the pixel order and both are of the 8u depth;
  - a destination image of the iplYUV2RGB2 function can be of the pixel order if a
  source image is of the plane order and both are of the 8u depth;
}

procedure iplRGB2HSV(rgbImage, hsvImage : PIplImage); stdcall;
procedure iplHSV2RGB(hsvImage, rgbImage : PIplImage); stdcall;

procedure iplRGB2HLS(rgbImage, hlsImage : PIplImage); stdcall;
procedure iplHLS2RGB(hlsImage, rgbImage : PIplImage); stdcall;

procedure iplRGB2XYZ(rgbImage, xyzImage : PIplImage); stdcall;
procedure iplXYZ2RGB(xyzImage, rgbImage : PIplImage); stdcall;

procedure iplRGB2LUV(rgbImage, luvImage : PIplImage); stdcall;
procedure iplLUV2RGB(luvImage, rgbImage : PIplImage); stdcall;

procedure iplRGB2YUV(rgbImage, yuvImage : PIplImage); stdcall;
procedure iplYUV2RGB(yuvImage, rgbImage : PIplImage); stdcall;

procedure iplRGB2YCrCb(rgbImage, YCrCbImage : PIplImage); stdcall;
procedure iplYCrCb2RGB(YCrCbImage, rgbImage : PIplImage); stdcall;

procedure iplYCC2RGB(YCCImage, rgbImage : PIplImage); stdcall;

{------------------------  Color Twist Matrices  --------------------------}

function iplCreateColorTwist(Data         : PInteger;
                             ScalingValue : Integer) : PIplColorTwist; stdcall;
{
Purpose:    Creates a color-twist matrix data structure.
Returns:    A pointer to the iplColorTwist data structure containing
            the color twist matrix in form suitable for efficient
            computation. This structure can then be used in the function
            iplApplyColorTwist().
Parameters:
  Data            An array containing the sixteen values that constitute
                  the color twist matrix. The values are in row wise order.
                  Color twist values that lie in the range -1 to 1 should be
                  scaled up to lie in the range -231 to 231
                  (Simply multiply the floating point number in the -1 to 1
                  range by 231)
  ScalingValue    The scaling value - the exponent of a power of 2 -
                  that was used (e.g. 31 if 231  was used to multiply the values)
                  to convert to integer values. Will be used for normalization.
}

procedure iplSetColorTwist(cTwist       : PIplColorTwist;
                           Data         : PInteger;
                           ScalingValue : Integer); stdcall;
{
Purpose:    Sets a color-twist matrix data structure.
Parameters:
  Data            An array containing the sixteen values that constitute
                  the color twist matrix. The values are in row wise order.
                  Color twist values that lie in the range -1 to 1 should be
                  scaled up to lie in the range -231 to 231
                  (Simply multiply the floating point number in the -1 to 1
                  range by 231)
  ScalingValue    The scaling value - the exponent of a power of 2 -
                  that was used (e.g. 31 if 231  was used to multiply the values)
                  to convert to integer values. Will be used for normalization.
  cTwist          The color  twist matrix data structure that was created
                  by a call to the function iplCreateColorTwist().
}

procedure iplDeleteColorTwist(cTwist : PIplColorTwist); stdcall;
{
Purpose:    Frees memory used for a color-twist matrix.
Parameters:
  cTwist          The color  twist matrix data structure that was created
                  by a call to the function iplCreateColorTwist().
}

procedure iplApplyColorTwist(SrcImage, DstImage : PIplImage;
                             cTwist             : PIplColorTwist;
                             Offset             : Integer); stdcall;
{
Purpose:    Applies a color-twist matrix to an image.
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
  cTwist          The color  twist matrix data structure that was prepared
                  by a call to the function iplSetColorTwist().
  Offset          An offset value that will be added to each pixel channel
                  after multiplication with the color twist matrix.
}

function iplColorTwistFP(SrcImage, DstImage : PIplImage;
                         TwistFP : PFloat)  : IPLStatus; stdcall;
{
Purpose:    Applies a color-twist matrix to an image.
Parameters:
  SrcImage                The source image.
  DstImage                The resultant image.
  TwistFP                 The matrix.
Notes:
                |Y11|   |T11 T12 T13|   |X11|   |T14|
                |Y21| = |T21 T22 T23| * |X21| + |T24|
                |Y31|   |T31 T32 T33|   |X31|   |T34|

                TwistFP[12] = (T11,T12,T13,T14,T21,T22,T23,T24,T31,T32,T33,T34);

                X11(Y11) - The first channel of the srcImage(dstImage);
                X21(Y21) - The second channel of the srcImage(dstImage);
                X31(Y31) - The third channel of the srcImage(dstImage);

                srcImage->depth == dstImage->depth == 32F.
}

{==========================================================================
      Section: Histogram and Thresholding Functions
 ==========================================================================}

procedure iplThreshold(SrcImage, DstImage : PIplImage;
                       Threshold          : Integer); stdcall;
{
Purpose:    Performs a simple thresholding of an image.
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
  Threshold       The threshold value to use for each pixel.
                  The pixel value in the output is set to the maximum
                  representable value if it is greater than or equal to the
                  threshold value (for each channel). Otherwise the pixel value
                  in the output is set to minimum representable value.
}

procedure iplContrastStretch(SrcImage, DstImage : PIplImage;
                             Lut                : PIplLUT); stdcall;
{
Purpose:    Stretches the contrast of an image using an intensity
            transformation.
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
  Lut             An array of pointers to LUTs - one for each channel.
                  Each lookup table should have the key and value arrays
                  fully initialized. One or more channels may share the same LUT.
                  Specifies an intensity transformation.
}

procedure iplComputeHisto(SrcImage : PIplImage;
                          Lut      : PIplLUT); stdcall;
{
Purpose:    Computes the intensity histogram of an image.
Parameters:
  SrcImage        The source image for which the Histogram will be computed.
  Lut             An array of pointers to LUTs - one for each channel.
                  Each lookup table should have  the key array fully initialized.
                  The value array will be filled by this function.
                  The same LUT can be shared by one or more channels.
}

procedure iplHistoEqualize(SrcImage, DstImage : PIplImage;
                           Lut                : PIplLUT); stdcall;
{
Purpose:    Enhances an image by flattening its intensity histogram.
Parameters:
  SrcImage        The source image for which the Histogram will be computed.
  DstImage        The resultant image after equalization.
  Lut             The Histogram of the image represented as an array of pointers
                  to LUTs - one for each channel. Each  lookup table should have
                  the key and value arrays fully initialized.
                  This LUTs will contain flattened Histograms after this function
                  is executed.
}

{==========================================================================
      Section: Geometric Transformation Functions
 ==========================================================================}

{------------------------  Kind of Interpolation  -------------------------}

const
  IPL_INTER_NN     =  0;
  IPL_INTER_LINEAR =  1;
  IPL_INTER_CUBIC  =  2;
  IPL_INTER_SUPER  =  3;
  IPL_SMOOTH_EDGE  = 16;

procedure iplWarpAffine(SrcImage, DstImage : PIplImage;
                        Coeff              : PDouble;
                        Interpolate        : Integer); stdcall;
{
Purpose:        makes Affine transform of image.
                |X'|   |a11 a12| |X| |a13|
                |  | = |       |*| |+|   |
                |Y'|   |a21 a22| |Y| |a23|
Parameters:
    SrcImage         The source image.
    DstImage         The resultant image.
    Coeffs           The transform matrix
    Interpolate      The type of interpolation to perform for resampling
                     the input image. The following are currently supported:
                     IPL_INTER_NN       Nearest neighbour interpolation.
                     IPL_INTER_LINEAR   Linear interpolation.
                     IPL_INTER_CUBIC    Cubic convolution interpolation.
                     IPL_SMOOTH_EDGE    Smooth edges. Can be added to
                                        interpolation by using bitwise logical OR.
}

procedure iplRemap(SrcImage, XMap, YMap, DstImage : PIplImage;
                   Interpolate                    : Integer); stdcall;
{
  Purpose:        Remap srcImage with map.
                  dst[i,j] = src[xMap[i], yMap[j]]
  Parameters:
      SrcImage         The source image.
      XMap             The image with x coords of map.
      YMap             The image with y coords of map.
      DstImage         The resultant image.
      Interpolate      The type of interpolation to perform for resampling
                       the input image. The following are currently supported.
                       IPL_INTER_NN       Nearest neighbour interpolation.
                       IPL_INTER_LINEAR   Linear interpolation.
                       IPL_INTER_CUBIC    Cubic convolution interpolation.
}

procedure iplShear(SrcImage, DstImage : PIplImage;
                   XShear, YShear     : Double;
                   XShift, YShift     : Double;
                   Interpolate        : Integer); stdcall;
{
Purpose:        Makes shear transform of image.
                |X'|   |1    xShear| |X|
                |  | = |           |*| |
                |Y'|   |yShear   1 | |Y|
Parameters:
    SrcImage         The source image.
    DstImage         The resultant image.
    Interpolate      The type of interpolation to perform for resampling
                     the input image. The following are currently supported.
                     IPL_INTER_NN       Nearest neighbour interpolation.
                     IPL_INTER_LINEAR   Linear interpolation.
                     IPL_INTER_CUBIC    Cubic convolution interpolation.
                     IPL_SMOOTH_EDGE    Smooth edges. Can be added to
                                        interpolation by using bitwise logical OR.
}

procedure iplRotate(SrcImage,DstImage : PIplImage; Angle : Double;
                    XShift,  YShift   : Double;
                    Interpolate       : Integer); stdcall;
{
Purpose:        rotates image about (0, 0) on angle.
Parameters:
    SrcImage         The source image.
    DstImage         The resultant image.
    Interpolate      The type of interpolation to perform for resampling
                     the input image. The following are currently supported.
                     IPL_INTER_NN       Nearest neighbour interpolation.
                     IPL_INTER_LINEAR   Linear interpolation.
                     IPL_INTER_CUBIC    Cubic convolution interpolation.
                     IPL_SMOOTH_EDGE    Smooth edges. Can be added to
                                        interpolation by using bitwise logical OR.
}

procedure iplGetRotateShift(XCenter, YCenter : Double; Angle : Double;
                        var XShift, YShift   : Double); stdcall;
{
Purpose:        recalculates shifts for rotation around point (x, y).
Parameters:
                xCenter, yCenter    new center of rotation
                angle               the angle of rotation
}

procedure iplGetAffineQuad(Image  : PIplImage;
                           Coeffs : PDouble;
                           Quad   : PDouble); stdcall;
{
Purpose:        calculates coordinates of quadrangle from transformed image ROI.
Parameters:
    Image    The source image.
    Coeffs      The transform matrix
                |X'|   |a11 a12| |X| |a13|
                |  | = |       |*| |+|   |
                |Y'|   |a21 a22| |Y| |a23|
    Quadr       resultant quadrangle
}

procedure iplGetAffineQuadROI(Roi    : PIplROI;
                              Coeffs : PDouble;
                              Quad   : PDouble); stdcall;

procedure iplGetAffineBound(Image  : PIplImage;
                            Coeffs : PDouble;
                            Rect   : PDouble); stdcall;
{
Purpose:        calculates bounding rectangle of the transformed image ROI.
Parameters:
    Image    The source image.
    Coeffs      The transform matrix
                |X'|   |a11 a12| |X| |a13|
                |  | = |       |*| |+|   |
                |Y'|   |a21 a22| |Y| |a23|
    Rect        resultant bounding rectangle
}

procedure iplGetAffineBoundROI(Roi    : PIplROI;
                               Coeffs : PDouble;
                               Rect   : PDouble); stdcall;

procedure iplGetAffineTransform(Image  : PIplImage;
                                Coeffs : PDouble;
                                Quad   : PDouble); stdcall;
{
Purpose:        calculates transform matrix from vertexes of quadrangle.
Parameters:
    Image    The source image.
    Coeffs      The resultant transform matrix
                |X'|   |a11 a12| |X| |a13|
                |  | = |       |*| |+|   |
                |Y'|   |a21 a22| |Y| |a23|
    Quadr       quadrangle
}

procedure iplGetAffineTransformROI(Roi    : PIplROI;
                                   Coeffs : PDouble;
                                   Quad   : PDouble); stdcall;

const
  IPL_WARP_R_TO_Q = 0;
  IPL_WARP_Q_TO_R = 1;

procedure iplWarpBilinear( SrcImage, DstImage : PIplImage;
                           Coeffs             : PDouble;
                           WarpFlag           : Integer;
                           Interpolate        : Integer); stdcall;
procedure iplWarpBilinearQ(SrcImage, DstImage : PIplImage;
                           Coeffs             : PDouble;
                           WarpFlag           : Integer;
                           Interpolate        : Integer); stdcall;
{
Name:           iplWarpBilinear, iplWarpBilinearQ
Purpose:        makes bilinear transform of image.
                from rectangle to quadrangle use transform
                |X|   |a11|      |a12 a13| |J|   |a14|
                | | = |   |*JI + |       |*| | + |   |
                |Y|   |a21|      |a22 a23| |I|   |a24|
                from quadrangle to rectungle use inverse transform
                0 = J*J * (-(x1-x0)*(y3-y2)+(x3-x2)*(y1-y0)) +
                    J * (X*(y3-y2+y1-y0) - Y*(x3-x2+x1-x0) +
                    ((x0-x3)*(y1-y0)-(x1-x0)*(y0-y3))) +
                    (X*(y0-y3) - Y*(x0-x3))
                0 = I*I * (-(x0-x3)*(y2-y1)+(x2-x1)*(y0-y3)) +
                    I * (X*(y3-y2+y1-y0) - Y*(x3-x2+x1-x0) +
                    (-(x0-x3)*(y1-y0)+(y0-y3)*(x1-x0))) +
                    (-X*(y1-y0) + Y*(x1-x0))
               In addition, I & J are bounded by the relation:
               0 = I * (-(x0-x3)*(y2-y1)+(x2-x1)*(y0-y3)) +
                   J * (-(x1-x0)*(y3-y2)+(x3-x2)*(y1-y0)) +
                  (X * (y3-y2+y1-y0) - Y * (x3-x2+x1-x0))
               J=0..1, I=0..1;
               J=j/jmax, I=i/imax;
               (j,i) - coordinates of a pixel in the square rectangle
               X=x-x0, Y=y-y0;
               (x,y) - coordinates of the pixel in the qudrangle
Parameters:
    SrcImage         The source image.
    DstImage         The resultant image.
    Quad             The vertexes of quadrangle.
    WarpFlag         If WarpFlag is IPL_WARP_R_TO_Q, the transform is from rectangle
                     SrcImage->roi to quadrangle in DstImage.
                     If WarpFlag is IPL_WARP_Q_TO_R, the transform is from quadrangle
                     in SrcImage to rectangle DstImage->roi.
    Interpolate      The type of interpolation to perform for resampling
                     the input image. The following are currently supported.
                     IPL_INTER_NN       Nearest neighbour interpolation.
                     IPL_INTER_LINEAR   Linear interpolation.
                     IPL_INTER_CUBIC    Cubic convolution interpolation.
                     IPL_SMOOTH_EDGE    Smooth edges. Can be added to
                                        interpolation by using bitwise logical
                                        OR (for warpFlag == IPL_WARP_R_TO_Q).
}

procedure iplWarpPerspective( SrcImage, DstImage : PIplImage;
                              Coeffs             : PDouble;
                              WarpFlag           : Integer;
                              Interpolate        : Integer); stdcall;
procedure iplWarpPerspectiveQ(SrcImage, DstImage : PIplImage;
                              Coeffs             : PDouble;
                              WarpFlag           : Integer;
                              Interpolate        : Integer); stdcall;
{
Name:           iplWarpPerspective, iplWarpPerspectiveQ
Purpose:        makes perspective transform of image.
                from rectangle to quadrangle use transform
                    a11*j + a12*i + a13
                x = -------------------
                    a31*j + a32*i + a33

                    a21*j + a22*i + a23
                y = -------------------
                    a31*j + a32*i + a33

                      |x0-x1+x2-x3  x3-x2|   | x1-x2  x3-x2|
                a31 = |                  | / |             |
                      |y0-y1+y2-y3  y3-y2|   | y1-y2  y3-y2|

                      |x1-x2  x0-x1+x2-x3|   | x1-x2  x3-x2|
                a21 = |                  | / |             |
                      |y1-y2  y0-y1+y2-y3|   | y1-y2  y3-y2|

                a11 = (x1-x0+a31*x1)/jmax
                a12 = (x3-x0+a32*x3)/imax
                a13 = x0
                a21 = (y1-y0+a31*y1)/jmax
                a22 = (y3-y0+a32*y3)/imax
                a23 = y0
                from quadrangle to rectangle use inverse transform
                |a22*a33-a32*a23  a32*a13-a12*a33  a12*a23-a22*a13|
                |a31*a23-a21*a33  a11*a33-a31*a13  a21*a13-a11*a23|
                |a21*a32-a31*a22  a31*a12-a11*a32  a11*a22-a21*a12|
Parameters:
    SrcImage         The source image.
    DstImage         The resultant image.
    Quad             The vertexes of quadrangle.
    WarpFlag         If WarpFlag is IPL_WARP_R_TO_Q, the transform is from rectangle
                     SrcImage->roi to quadrangle (x0,y0)(x1,y1)(x2,y2)(x3,y3) in DstImage.
                     If warpFlag is IPL_WARP_Q_TO_R, the transform is from quadrangle
                     (x0,y0)(x1,y1)(x2,y2)(x3,y3) in SrcImage to rectangle DstImage->roi.
    Interpolate      The type of interpolation to perform for resampling
                     the input image. The following are currently supported.
                     IPL_INTER_NN       Nearest neighbour interpolation.
                     IPL_INTER_LINEAR   Linear interpolation.
                     IPL_INTER_CUBIC    Cubic convolution interpolation.
                   IPL_SMOOTH_EDGE      Smooth edges. Can be added to
                                        interpolation by using bitwise logical
                                        OR (for warpFlag == IPL_WARP_R_TO_Q).
}

procedure iplGetBilinearQuad(Image  : PIplImage;
                             Coeffs : PDouble;
                             Quad   : PDouble); stdcall;
{
Purpose:        calculates coordinates of quadrangle from transformed image ROI.
Parameters:
    Image       The image.
    Coeffs      The transform matrix
                |X|   |a11|      |a12 a13| |J|   |a14|
                | | = |   |*JI + |       |*| | + |   |
                |Y|   |a21|      |a22 a23| |I|   |a24|
    Quadr       resultant quadrangle
}

procedure iplGetBilinearQuadROI(Roi    : PIplROI;
                                Coeffs : PDouble;
                                Quad   : PDouble); stdcall;

procedure iplGetBilinearBound(Image  : PIplImage;
                              Coeffs : PDouble;
                              Rect   : PDouble); stdcall;
{
Purpose:        calculates bounding rectangle of the transformed image ROI.
Parameters:
    Image    The source image.
    Coeffs      The transform matrix
                |X|   |a11|      |a12 a13| |J|   |a14|
                | | = |   |*JI + |       |*| | + |   |
                |Y|   |a21|      |a22 a23| |I|   |a24|
    Rect        resultant bounding rectangle
}

procedure iplGetBilinearBoundROI(Roi    : PIplROI;
                                 Coeffs : PDouble;
                                 Rect   : PDouble); stdcall;

procedure iplGetBilinearTransform(Image  : PIplImage;
                                  Coeffs : PDouble;
                                  Quad   : PDouble); stdcall;
{
Purpose:        calculates transform matrix from vertexes of quadrangle.
Parameters:
    Image       The image.
    Coeffs      The resultant transform matrix
                |X|   |a11|      |a12 a13| |J|   |a14|
                | | = |   |*JI + |       |*| | + |   |
                |Y|   |a21|      |a22 a23| |I|   |a24|
    Quadr       quadrangle
}

procedure iplGetBilinearTransformROI(Roi    : PIplROI;
                                     Coeffs : PDouble;
                                     Quad   : PDouble); stdcall;

procedure iplGetPerspectiveQuad(Image  : PIplImage;
                                Coeffs : PDouble;
                                Quad   : PDouble); stdcall;
{
Purpose:        calculates coordinates of quadrangle from transformed image ROI.
Parameters:
    Image       The image.
    Coeffs      The transform matrix
                    a11*j + a12*i + a13
                x = -------------------
                    a31*j + a32*i + 1

                    a21*j + a22*i + a23
                y = -------------------
                    a31*j + a32*i + 1
    Quadr       resultant quadrangle
}

procedure iplGetPerspectiveQuadROI(Roi    : PIplROI;
                                   Coeffs : PDouble;
                                   Quad   : PDouble); stdcall;

procedure iplGetPerspectiveBound(Image  : PIplImage;
                                 Coeffs : PDouble;
                                 Rect   : PDouble); stdcall;
{
Purpose:        calculates bounding rectangle of the transformed image ROI.
Parameters:
    Image    The source image.
    Coeffs      The transform matrix
                    a11*j + a12*i + a13
                x = -------------------
                    a31*j + a32*i + 1

                    a21*j + a22*i + a23
                y = -------------------
                    a31*j + a32*i + 1
    Rect        resultant bounding rectangle
}

procedure iplGetPerspectiveBoundROI(Roi    : PIplROI;
                                    Coeffs : PDouble;
                                    Rect   : PDouble); stdcall;

procedure iplGetPerspectiveTransform(Image  : PIplImage;
                                     Coeffs : PDouble;
                                     Quad   : PDouble); stdcall;
{
Purpose:        calculates transform matrix from vertexes of quadrangle.
Parameters:
    Image       The image.
    Coeffs      The resultant transform matrix
                    a11*j + a12*i + a13
                x = -------------------
                    a31*j + a32*i + 1

                    a21*j + a22*i + a23
                y = -------------------
                    a31*j + a32*i + 1
    Quadr       quadrangle
}

procedure iplGetPerspectiveTransformROI(Roi    : PIplROI;
                                        Coeffs : PDouble;
                                        Quad   : PDouble); stdcall;

procedure iplResize(SrcImage, DstImage : PIplImage;
                    XDst, XSrc         : Integer;
                    YDst, YSrc         : Integer;
                    Interpolate        : Integer); stdcall;
{
Purpose:        Resize image.
Parameters:
    SsrcImage        The source image.
    DstImage         The resultant image.
    XDst, XSrc,
    YDst, YSrc       They specify the fractions xDst\xSrc and yDst\ySrc.
    Interpolate      The type of interpolation to perform for resampling
                     the input image.
                     The following are currently supported.
                     IPL_INTER_NN       Nearest neighbour interpolation.
                     IPL_INTER_LINEAR   Linear interpolation.
                     IPL_INTER_CUBIC    Cubic convolution interpolation.
                     IPL_INTER_SUPER    Super sampling interpolation.
}

procedure iplZoom(SrcImage, DstImage : PIplImage;
                  XDst, XSrc         : Integer;
                  YDst, YSrc         : Integer;
                  Interpolate        : Integer); stdcall;
{
Purpose:    Zooms or expands an image.
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
  XDst, XSrc,
  YDst, YSrc      All these should be positive integers with xDst >= xSrc and
                  yDst >= ySrc. They specify the fractions xDst/xSrc and yDst/ySrc
                  to expand the image in the X and Y directions.
                  For example xDst = 2, xSrc = 1, yDst = 2, ySrc = 1 doubles the
                  image size in each dimension to give an image 4 times larger in
                  area.
  Interpolate     The type of interpolation to perform for resampling.
                  The following are currently supported.
                  IPL_INTER_NN            Nearest neighbour interpolation.
                  IPL_INTER_LINEAR        Linear interpolation.
                  IPL_INTER_CUBIC         Cubic convolution interpolation.
}

procedure iplDecimate(SrcImage, DstImage : PIplImage;
                      XDst, XSrc         : Integer;
                      YDst, YSrc         : Integer;
                      Interpolate        : Integer); stdcall;
{
Purpose:    Decimates or shrinks an image.
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
  XDst, XSrc,
  YDst, YSrc      All these should be positive integers with xDst <= xSrc
                  and yDst <= ySrc. They specify the fractions xDst/xSrc
                  and yDst/ySrc to shrink the image in the X and Y directions.
                  For example xDst = 1, xSrc = 2, yDst = 1, ySrc = 2 halves
                  the image size in each dimension to give an image 1/4 times
                  smaller in area.
  Interpolate     The type of interpolation to perform for resampling.
                  The following are currently supported.
                  IPL_INTER_NN            Nearest neighbour interpolation.
                  IPL_INTER_LINEAR        Linear interpolation.
                  IPL_INTER_CUBIC         Cubic convolution interpolation.
                  IPL_INTER_SUPER         Supersampling interpolation.
}

procedure iplDecimateBlur(SrcImage, DstImage   : PIplImage;
                          XDst, XSrc           : Integer;
                          YDst, YSrc           : Integer;
                          Interpolate          : Integer;
                          XMaskSize, YMaskSize : Integer); stdcall;
{
Purpose:    Decimation of an image with "pre-blur"
Parameters:
  SrcImage   - the source Image.
  DstImage   - the resultant Image.
  xDst, xSrc,
  yDst, ySrc    They specify the fractions xDst\xSrc and yDst\ySrc.
  interpolate   The type of interpolation to perform for resampling.
                The following are currently supported.
                IPL_INTER_NN            Nearest neighbour interpolation.
                IPL_INTER_LINEAR        Linear interpolation.
                IPL_INTER_CUBIC         Cubic convolution interpolation.
  XMaskSize  -  Number of columns in the neighbourhood(cell)to use.
  yMaskSize  -  Number of rows in the neighbourhood(cell) to use.
                The coordinates of an anchor are in middle of a cell.
                (In this coordinate system,
                 the top left corner would be [0, 0] and the
                 bottom right corner would be [ xMaskSize,yMaskSize].
                 For a 3 by 3 neighbourhood the coordinates
                 of the geometric center would be [1, 1]).
Notes: no in-place
}

procedure iplMirror(SrcImage, DstImage : PIplImage;
                    FlipAxis           : Integer); stdcall;
{
Purpose:    Mirrors an image about a horizontal or vertical axis.
Parameters:
  SrcImage        The source image.
  DstImage        The resultant image.
  FlipAxis        Specifies the axis to mirror the image.
                  Use 0 for the horizontal axis,
                      1 for a vertical axis
                     -1 for both horizontal and vertical axis.
}

{==========================================================================
      Section: Image Features functions
 ==========================================================================}

{------------------  Definitions of the type of norms  --------------------}
const
  IPL_C          = 1;
  IPL_L1         = 2;
  IPL_L2         = 4;
  IPL_RELATIVE   = 8;
  IPL_RELATIVEC  = IPL_RELATIVE or IPL_C;
  IPL_RELATIVEL1 = IPL_RELATIVE or IPL_L1;
  IPL_RELATIVEL2 = IPL_RELATIVE or IPL_L2;

function iplNorm(SrcImageA, SrcImageB : PIplImage;
                 NormType             : Integer) : Double; stdcall;
{
Name:        iplNorm
Purpose:     Calculates C,L1,L2-norms for one IPL image, absolute and relative
             errors between two IPL-images.
Parameters:
 SrcImageA - The source image, tiled or not, with maskROI or not.
 SrcImageB - The source image, tiled or not, with maskROI or not, may be NULL.
 NormType  - Specifies the type of norms and errors.
Notes:
 If pointer of SrcImageB is NULL,the next type of norms for SrcImageA defined:
   NormType = IPL_C  for C -norm: ||A||= max ( abs(a[i,j]) );
   NormType = IPL_L1 for L1-norm: ||A||= sum ( abs(a[i,j]) )
   NormType = IPL_L2 for L2-norm: ||A||= sqrt( sum (a[i,j]**2) );

 If pointer of SrcImageB is not NULL, the next type of absolute errors defined:
   NormType = IPL_C  for C -absolute error: ||A-B||=max ( abs(a[i,j]-b[i,j]) );
   NormType = IPL_L1 for L1-absolute error: ||A-B||=sum ( abs(a[i,j]-b[i,j]) );
   NormType = IPL_L2 for L2-absolute error: ||A-B||=sqrt( sum (a[i,j]-b[i,j])**2 );

 If pointer of SrcImageB is not NULL, the next type of relative errors defined:
   NormType = IPL_RELATIVEC  for C -relative error:
              ||A-B||/||B||= max(abs (a[i,j]-b[i,j]))/max(abs(a[i,j]));
   NormType = IPL_RELATIVEL1 for L1-relative error:
              ||A-B||/||B||= sum(abs (a[i,j]-b[i,j]))/sum(abs(a[i,j]));
   NormType = IPL_RELATIVEL2 for L2-relative error:
              ||A-B||/||B||= sqrt(sum((a[i,j]-b[i,j])**2)/sum((a[i,j])**2)));
}

procedure iplMoments(Img : PIplImage; Stt : TIplMomentState); stdcall;
{
Purpose:    Computes all image moments of order 0 to 3.
Parameters:
  Img - The image for which the moments will be computed.
  Stt - The structure for storing the image moments.
}

function iplGetSpatialMoment(          Stt  : TIplMomentState;
                                       M, N : Integer) : Double; stdcall;
function iplGetNormalizedSpatialMoment(Stt  : TIplMomentState;
                                       M, N : Integer) : Double; stdcall;
function iplGetCentralMoment(          Stt  : TIplMomentState;
                                       M, N : Integer) : Double; stdcall;
function iplGetNormalizedCentralMoment(Stt  : TIplMomentState;
                                       M, N : Integer) : Double; stdcall;
{
Name:       iplGet*Moment
Purpose:    Get a specific moments, computed by iplMoments.
Returns:    Specific moments  (see names of functions).
Parameters:
   Stt   - The structure storing the image moments.
   m, n  - The integer exponents m and n.
           These arguments must satisfy the condition
           0 <= (m + n) <= 3.
}

function iplSpatialMoment(          Img  : PIplImage;
                                    M, N : Integer) : Double; stdcall;
function iplNormalizedSpatialMoment(Img  : PIplImage;
                                    M, N : Integer) : Double; stdcall;
function iplCentralMoment(          Img  : PIplImage;
                                    M, N : Integer) : Double; stdcall;
function iplNormalizedCentralMoment(Img : PIplImage;
                                    M, N : Integer) : Double; stdcall;
{
Name:       ipl*Moment
Purpose:    Computes a specific moments.
Returns:    Specific moments  (see names of functions).
Parameters:
    Img - The image for which the moment will be computed.
    m,n - The integer exponents m and n.
          These arguments must satisfy the condition
          0 <= (m + n) <= 3.
}

function iplGreater(Img1, Img2, Res : PIplImage) : IPLStatus; stdcall;
function iplLess(   Img1, Img2, Res : PIplImage) : IPLStatus; stdcall;
function iplEqual(  Img1, Img2, Res : PIplImage) : IPLStatus; stdcall;
function iplEqualFPEps(Img1, Img2, Res : PIplImage; Eps : Float) : IPLStatus; stdcall;

function iplLessS(   Img : PIplImage; S : integer; Res : PIplImage) : IPLStatus; stdcall;
function iplGreaterS(Img : PIplImage; S : Integer; Res : PIplImage) : IPLStatus; stdcall;
function iplEqualS(  Img : PIplImage; S : Integer; Res : PIplImage) : IPLStatus; stdcall;

function iplLessSFP(   Img : PIplImage; S : Float; Res : PIplImage) : IPLStatus; stdcall;
function iplGreaterSFP(Img : PIplImage; S : Float; Res : PIplImage) : IPLStatus; stdcall;
function iplEqualSFP(  Img : PIplImage; S : Float; Res : PIplImage) : IPLStatus; stdcall;

function iplEqualSFPEps(Img : PIplImage; S : Float; Res : PIplImage;
                        Eps : Float) : IPLStatus; stdcall;
{
Name:       iplGreater, iplEqualFPEps, iplGreaterS, iplLessS, iplEqualS,
            iplGreaterSFP, iplLessSFP, iplEqualSFP, iplEqualSFPEps
Purpose:
Returns:
    IPL_StsOk    - success
    other status - images are not compatible, null pointer, etc.
Parameters:
 Destination image
       res.depth = 1U
       res.nChannels = 1
       roi +
       tile +
       mask +

 Source images:
       if two images are compared, then img1 must be compatible with img2;
       images are compatible if their origins, depths, coi and nChannels
       are equal.

       img.depth = [ 1u, 8u, 8s, 16u, 16s, 32s, 32f ]
       nChannels = 1..n
       roi +
       coi +
       tile +
       mask +
       in-place -

Notes:
 o If (img.coi == img.alphaChannel) then
   alpha-channel is compared, it is the operation is executed.

 o Compare operation is "fabs( a - b ) < eps" in iplEqual(S)FPEps and is
   direct compare in iplEqual(SFP).

 o Example (iplGreater):

 If (img.coi == n)
    plane: Greater(x,y) = img1.coi(x,y) > img2.coi(x,y)
    pixel: Greater(x,y) = img1(x,y).coi > img2(x,y).coi

 If (img.coi == 0) and (img.nChannels > 1) then
    plane: Greater(x,y) = img1.red(x,y) > img2.red(x,y) && img1.green(x,y) >
 img2.green(x,y) ...
    pixel: Greater(x,y) = img1(x,y).red > img2(x,y).red && img1(x,y).green >
 img2(x,y).green ...
}

function iplMinMaxFP(SrcImage : PIplImage;
                 var Min, Max : Float) : IPLStatus; stdcall;
{
Purpose:    To find minimum and maximum value of pixels of the srcImage.
Returns:    IPLStatus
Parameters:
     SrcImage - The source image.
     Min      - Pointer to minimum.
     Max      - Pointer to maximum.
}

function iplNormCrossCorr(TplImage, SrcImage, DstImage : PIplImage) : IPLStatus; stdcall;
{
Purpose:    Produce probability image based on Normalized Cross Correlation
Returns:    IPLStatus
Parameters:
     SrcImage - source image
     TplImage - template image
     DstImage - probability image based on Normalized Cross Correlation

Notes:      Template image should be a non-tiled image only.
}

{==========================================================================
      Section: Declaration of IPL Wavelet Functions
               and Initialization Routines
 =========================================================================}

function iplWtInit(WtType   : TIplWtType; Par1, Par2 : Integer;
               var WtKernel : TIplWtKernel) : IPLStatus; stdcall;
{
Purpose:    Wavelet transform initialization by fixed wavelet type
            from IplWtType enum.
Context:
Returns:    IPLStatus
Parameters:
     WtType   - type of wavelet transform;
     Par1     - first parameter (order of wavelet transform);
     Par2     - second parameter (only for biorth. wavelets);
     WtKernel - wavelet functions interchange structure;

Notes:

 Wavelet transform initialization parameters:

     Par1, Par2 - the parameters of wavelet,
                  dependent from the type of wavelet.
       IPL_WT_HAAR           Par1 - dummy
                             Par2 - dummy
       IPL_WT_DAUBLET        Par1 = 1,2,3,4,5,6,7,8,9,10.
                             Par2 - dummy
       IPL_WT_SYMMLET        Par1 = 1,2,3,4,5,6,7.
                             Par2 - dummy
       IPL_WT_COIFLET        Par1 = 1,2,3,4,5.
                             Par2 - dummy
       IPL_WT_VAIDYANATHAN   Par1 - dummy
                             Par2 - dummy

       IPL_WT_BSPLINE        B - spline,
       IPL_WT_BSPLINEDUAL    (Par1, Par2) must be:
                             box -
                              (1, 1), (1, 3), (1, 5);
                             lin. spline -
                              (2, 2), (2, 4), (2, 6), (2, 8);
                             quad. spline -
                              (3, 1), (3, 3), (3, 5), (3, 7), (3, 9).

       IPL_WT_LINSPLINE      (eq. case IPL_WT_BSPLINE with Par1=2, Par2=2.)
                             Par1 - dummy
                             Par2 - dummy
       IPL_WT_QUADSPLINE     (eq. case IPL_WT_BSPLINE with Par1=3, Par2=3.)
                             Par1 - dummy
                             Par2 - dummy
}

type
  TIntArray4      = array [0..3] of Integer;
  TPFloatArray4   = array [0..3] of PFloat;
  TWtFilterArray4 = array [0..3] of TIplWtFilter;

function iplWtInitUserTaps(Tap_Filt : TPFloatArray4;
                           Len_Filt : TIntArray4;
                           Ofs_Filt : TIntArray4;
                       var WtKernel : TIplWtKernel) : IPLStatus; stdcall;
{
Purpose:    Initialization of wavelet decomposition or reconstruction
            by user filters. Filter parameters passed by C arrays.
Context:
Returns:    IPLStatus
Parameters:
     Tap_Filt  - filter taps array;
     Len_Filt  - filter length array;
     Ofs_Filt  - filter offset array;
     WtKernel  - wavelet functions interchange structure;

Notes:
 0 - low  pass decomposition  filter;
 1 - high pass decomposition  filter;
 2 - low  pass reconstruction filter;
 3 - high pass reconstruction filter;
}

function iplWtInitUserFilter(DecLow   : PIplWtFilter;
                             DecHigh  : PIplWtFilter;
                             RecLow   : PIplWtFilter;
                             RecHigh  : PIplWtFilter;
                         var WtKernel : TIplWtKernel) : IPLStatus; stdcall;
{
Purpose:    Initialization of wavelet decomposition or reconstruction
            by user filters. Filter parameters passed by
            separate IplWtFilter structures.
Context:
Returns:    IPLStatus
Parameters:
     DecLow   - decompostion low-pass filter;
     DecHigh  - decomposition high-pass fitler;
     RecLow   - reconstruction low-pass filter;
     RecHigh  - reconstruction high-pass filter;
     WtKernel - wavelet functions interchange structure;

Notes:
}

function iplWtInitUserFilter4(Filt     : TWtFilterArray4;
                          var WtKernel : TIplWtKernel) : IPLStatus; stdcall;
{IPLAPI(IPLStatus, iplWtInitUserFilter4, (IplWtFilter filt[4],
                               IplWtKernel *wtKernel))}
{
Purpose:    Initialization of wavelet decomposition or reconstruction
            by user filters. Filter parameters passed by
            IplWtFilter structures in array.
Context:
Returns:    IPLStatus
Parameters:
     Filt     - filter bank;
     WtKernel - wavelet functions interchange structure;

Notes:
 0 - low  pass decomposition  filter;
 1 - high pass decomposition  filter;
 2 - low  pass reconstruction filter;
 3 - high pass reconstruction filter;
}

function iplWtDecompose(var WtKernel   : TIplWtKernel;
                            Src        : PIplImage;
                            ApproxDst  : PIplImage;
                            XDetailDst : PIplImage;
                            YDetailDst : PIplImage;
                            DDetailDst : PIplImage) : IPLStatus; stdcall;
{
Purpose:    One level wavelet decomposition.
Context:
Returns:    IPLStatus
Parameters:
     WtKernel  - wavelet functions interchange structure;
     Src    - source image;
     ApproxDst    - destination image for approximation;
     XDetailDst   - destination image for horizontal details;
     YDetailDst   - destination image for vertical details;
     DDetailDst   - destination image for diagonal details;

Notes:
}

function iplWtReconstruct(var WtKernel   : TIplWtKernel;
                              ApproxSrc  : PIplImage;
                              XDetailSrc : PIplImage;
                              YDetailSrc : PIplImage;
                              DDetailSrc : PIplImage;
                              Dst        : PIplImage) : IPLStatus; stdcall;
{
Purpose:    One level wavelet reconstruction.
Context:
Returns:    IPLStatus
Parameters:
     WtKernel   - wavelet functions interchange structure;
     ApproxSrc  - source approximation image;
     XDetailSrc - source horizontal details image;
     YDetailSrc - source vertical details image;
     DDetailSrc - source diagonal details image;
     Dst        - destination for reconstructed image;

Notes:
}

function iplWtFree(var WtKernel : TIplWtKernel) : IPLStatus; stdcall;
{
Purpose:    Final routine for wavelet transform.
            Free internal allocated memory and reset wtKernel structure.
Context:
Returns:    IPLStatus
Parameters:
     WtKernel  - wavelet functions interchange structure

Notes:
}

{==========================================================================
      Section: User process functions
 =========================================================================}

procedure iplUserProcess(srcImage, dstImage : PIplImage;
                         cbFunc : TIplUserFunc); stdcall;
{
Name:     iplUserProcess
Purpose:  Calls user IplUserFunc function for each channel of pixels
          which would be processed. Converts channel value to integer,
          passes it to IplUserFunc function and saturates function result
          to dstImage depth (except of 32s depth). To provide 32s
          saturation use iplUserProcessFP.
               Supports:
          8u, 8s, 16u, 16s, 32s depth;
          roi, maskROI, tiled images, in-place;
          srcImage->depth must be equal to dstImage->depth;
          srcImage and dstImage number of channels to process must be equal.
Returns:  None
Parameters:
    srcImage - source image
    dstImage - destination image
    cbFunc   - pointer to user function

Notes:
}

procedure iplUserProcessFP(srcImage, dstImage : PIplImage;
                           cbFunc : TIplUserFuncFP); stdcall;
{
Name:     iplUserProcessFP
Purpose:  Calls user IplUserFuncFP function for each channel of pixels
          which would be processed. Converts channel value to float,
          passes it to IplUserFuncFP function and saturates function result
          to dstImage depth (except of 32f depth of course).
               Supports:
          8u, 8s, 16u, 16s, 32s, 32f depth;
          roi, maskROI, tiled images, in-place;
          srcImage->depth must be equal to dstImage->depth;
          srcImage and dstImage number of channels to process must be equal.
Returns:  None
Parameters:
    srcImage - source image
    dstImage - destination image
    cbFunc   - pointer to user function

Notes:
}

procedure iplUserProcessPixel(srcImage, dstImage : PIplImage;
                              cbFunc : TIplUserFuncPixel); stdcall;
{
Name:     iplUserProcessPixel
Purpose:  Calls user IplUserFuncPixel function for each pixel which
          would be processed. Set src pixel and dst pixel values to arrays,
          passes these two arrays to IplUserFuncPixel function and test
          IPL Error after IplUserFuncPixel return.
               Supports:
          8u, 8s, 16u, 16s, 32s ,32f depth;
          rectangle roi, maskROI, tiled images;
          srcImage->depth may not be equal to dstImage->depth;
          srcImage->nChannels and dstImage->nChannels may be different.
               Not supports:
          dst saturation; it should be support by user function.
          channel roi; it should be support by user function if necessary.
Returns:  None
Parameters:
    srcImage - source image
    dstImage - destination image
    cbFunc   - pointer to user function

Notes:
}

{==========================================================================
      Section: Noise generators
 ==========================================================================}

procedure iplNoiseUniformInit(var NoiseParam : TIplNoiseParam;
                                  Seed       : UINT;
                                  Low, High  : Integer); stdcall;
procedure iplNoiseUniformInitFP( var NoiseParam : TIplNoiseParam;
                                  Seed          : UINT;
                                  Low, High     : Float); stdcall;
{
Name:       iplNoiseUniformInit,  iplNoiseUniformInitFP.
Purpose:    Initialization of parameters for the generator of noise.
Returns:
Parameters:
 seed     - The seed value used by the pseudo-random number generation
            algorithm.
 low      - The lower bounds of the uniform distribution’s range.
 high     - The upper bounds of the uniform distribution’s range.
}

procedure iplNoiseGaussianInit(var NoiseParam  : TIplNoiseParam;
                                   Seed        : UINT;
                                   Mean, StDev : Integer); stdcall;
procedure iplNoiseGaussianInitFP(var NoiseParam  : TIplNoiseParam;
                                     Seed        : UINT;
                                     Mean, StDev : Float); stdcall;
{
Name:       iplNoiseGaussianInit,  iplNoiseGaussianInitFP.
Purpose:    Initialization of parameters for the generator of noise.
Returns:
Parameters:
 seed     - The seed value used by the pseudo-random number generation
            algorithm.
 mean     - The mean of the Gaussian distribution.
 stDev    - The standard deviation of the Gaussian distribution.
}

function iplNoiseImage(Image : PIplImage;
                 var NoiseParam : TIplNoiseParam) : IPLStatus; stdcall;
{
Name:         iplNoiseImage
Purpose:      Addition of the generated noise to the image with saturation.
Returns:      IPLStatus
Parameters:
 image      - The image on which is imposed noise.
 noiseParam - A pointer to the structure containing parameters for the
              generator of noise.
}

{==========================================================================
      Section: Misc functions
 ==========================================================================}

function iplCheckImageHeader(Hdr : PIplImage) : IPLStatus; stdcall;
{
Purpose:    Checks image header
Returns:    returns IPL_StsOk if header is valid,
            else returns error status.
Parameters: image header
}

function iplCopyImageHeader(SrcImage : PIplImage; Depth : Integer) : PIplImage;
                                                                     stdcall;

procedure iplResizeFit(SrcImage, DstImage : PIplImage;
                       Interpolate        : Integer); stdcall;

procedure iplDecimateFit(SrcImage, DstImage : PIplImage;
                         Interpolate        : Integer); stdcall;

procedure iplRotateCenter(SrcImage, DstImage : PIplImage;
                          Angle              : Double;
                          XCenter, YCenter   : Double;
                          Interpolate        : Integer); stdcall;

implementation

const
  iplDLL = 'IPL.DLL';

function IPL_DegToRad(Deg : Extended) : Extended;
begin
  Result := (Deg)/180.0 * IPL_PI;
end;

function IPLsDegToRad(Deg : Float)    : Float;
begin
  Result := (Deg)/180.0 * IPL_PI;
end;

function IPLdDegToRad(Deg : Double)   : Double;
begin
  Result := (Deg)/180.0 * IPL_PI;
end;

function iplGetLibVersion;              external iplDLL;

function  iplGetErrStatus;              external iplDLL;
procedure iplSetErrStatus;              external iplDLL;

function  iplGetErrMode;                external iplDLL;
procedure iplSetErrMode;                external iplDLL;

function  iplError;                     external iplDLL;
function  iplErrorStr;                  external iplDLL;

function  iplNulDevReport;              external iplDLL;
function  iplStdErrReport;              external iplDLL;
function  iplGuiBoxReport;              external iplDLL;
function  iplRedirectError;             external iplDLL;

function  iplMalloc;                    external iplDLL;
function  iplwMalloc;                   external iplDLL;
function  ipliMalloc;                   external iplDLL;
function  iplsMalloc;                   external iplDLL;
function  ipldMalloc;                   external iplDLL;
procedure iplFree;                      external iplDLL;

procedure iplSetBorderMode;             external iplDLL;
function  iplCreateImageHeader;         external iplDLL;
function  iplCreateImageJaehne;         external iplDLL;
function  iplCloneImage;                external iplDLL;
procedure iplDeallocateHeader;          external iplDLL;
procedure iplAllocateImage;             external iplDLL;
procedure iplAllocateImageFP;           external iplDLL;
procedure iplDeallocateImage;           external iplDLL;
procedure iplDeallocate;                external iplDLL;
function  iplCreateTileInfo;            external iplDLL;
procedure iplSetTileInfo;               external iplDLL;
procedure iplDeleteTileInfo;            external iplDLL;
function  iplCreateROI;                 external iplDLL;
procedure iplSetROI;                    external iplDLL;
procedure iplDeleteROI;                 external iplDLL;
function  iplTranslateDIB;              external iplDLL;
procedure iplConvertFromDIB;            external iplDLL;
procedure iplConvertToDIB;              external iplDLL;
function  iplConvertFromDIBSep;         external iplDLL;
function  iplConvertToDIBSep;            external iplDLL;

procedure iplCopy;                      external iplDLL;
procedure iplExchange;                  external iplDLL;

procedure iplSet;                       external iplDLL;
procedure iplSetFP;                     external iplDLL;

procedure iplConvert;                   external iplDLL;

procedure iplBitonalToGray;             external iplDLL;

procedure iplAddS;                      external iplDLL;
procedure iplAddSFP;                    external iplDLL;
procedure iplSubtractS;                 external iplDLL;
procedure iplSubtractSFP;               external iplDLL;
procedure iplMultiplyS;                 external iplDLL;
procedure iplMultiplySFP;               external iplDLL;
procedure iplAbs;                       external iplDLL;
procedure iplNot;                       external iplDLL;
procedure iplLShiftS;                   external iplDLL;
procedure iplRShiftS;                   external iplDLL;
procedure iplSquare;                    external iplDLL;
procedure iplAndS;                      external iplDLL;
procedure iplOrS;                       external iplDLL;
procedure iplXorS;                      external iplDLL;
procedure iplAnd;                       external iplDLL;
procedure iplOr;                        external iplDLL;
procedure iplXor;                       external iplDLL;
procedure iplAdd;                       external iplDLL;
procedure iplSubtract;                  external iplDLL;
procedure iplMultiply;                  external iplDLL;
procedure iplMultiplySScale;            external iplDLL;
procedure iplMultiplyScale;             external iplDLL;
procedure iplAlphaComposite;            external iplDLL;
procedure iplAlphaCompositeC;           external iplDLL;
procedure iplPreMultiplyAlpha;          external iplDLL;
procedure iplBlur;                      external iplDLL;
function  iplCreateConvKernel;          external iplDLL;
function  iplCreateConvKernelFP;        external iplDLL;
function  iplCreateConvKernelChar;      external iplDLL;
procedure iplDeleteConvKernel;          external iplDLL;
procedure iplDeleteConvKernelFP;        external iplDLL;
procedure iplGetConvKernel;             external iplDLL;
procedure iplGetConvKernelFP;           external iplDLL;
procedure iplGetConvKernelChar;         external iplDLL;
procedure iplConvolve2D;                external iplDLL;
procedure iplConvolve2DFP;              external iplDLL;
function  iplFixedFilter;               external iplDLL;
procedure iplConvolveSep2D;             external iplDLL;
procedure iplConvolveSep2DFP;           external iplDLL;

procedure iplMedianFilter;              external iplDLL;
procedure iplColorMedianFilter;         external iplDLL;
procedure iplMaxFilter;                 external iplDLL;
procedure iplMinFilter;                 external iplDLL;

procedure iplRealFft2D;                 external iplDLL;
procedure iplCcsFft2D;                  external iplDLL;
procedure iplMpyRCPack2D;               external iplDLL;

procedure iplDCT2D;                     external iplDLL;

procedure iplErode;                     external iplDLL;
procedure iplDilate;                    external iplDLL;
procedure iplOpen;                      external iplDLL;
procedure iplClose;                     external iplDLL;

procedure iplReduceBits;                external iplDLL;
procedure iplColorToGray;               external iplDLL;
procedure iplGrayToColor;               external iplDLL;
procedure iplRGB2HSV;                   external iplDLL;
procedure iplHSV2RGB;                   external iplDLL;
procedure iplRGB2HLS;                   external iplDLL;
procedure iplHLS2RGB;                   external iplDLL;
procedure iplRGB2XYZ;                   external iplDLL;
procedure iplXYZ2RGB;                   external iplDLL;
procedure iplRGB2LUV;                   external iplDLL;
procedure iplLUV2RGB;                   external iplDLL;
procedure iplRGB2YUV;                   external iplDLL;
procedure iplYUV2RGB;                   external iplDLL;
procedure iplRGB2YCrCb;                 external iplDLL;
procedure iplYCrCb2RGB;                 external iplDLL;
procedure iplYCC2RGB;                   external iplDLL;

procedure iplThreshold;                 external iplDLL;
procedure iplContrastStretch;           external iplDLL;
procedure iplComputeHisto;              external iplDLL;
procedure iplHistoEqualize;             external iplDLL;

function  iplCreateColorTwist;          external iplDLL;
procedure iplSetColorTwist;             external iplDLL;
procedure iplDeleteColorTwist;          external iplDLL;
procedure iplApplyColorTwist;           external iplDLL;
function  iplColorTwistFP;              external iplDLL;

procedure iplWarpAffine;                external iplDLL;
procedure iplRemap;                     external iplDLL;
procedure iplShear;                     external iplDLL;
procedure iplRotate;                    external iplDLL;
procedure iplGetRotateShift;            external iplDLL;
procedure iplGetAffineQuad;             external iplDLL;
procedure iplGetAffineQuadROI;          external iplDLL;
procedure iplGetAffineBound;            external iplDLL;
procedure iplGetAffineBoundROI;         external iplDLL;
procedure iplGetAffineTransform;        external iplDLL;
procedure iplGetAffineTransformROI;     external iplDLL;
procedure iplWarpBilinear;              external iplDLL;
procedure iplWarpBilinearQ;             external iplDLL;
procedure iplWarpPerspective;           external iplDLL;
procedure iplWarpPerspectiveQ;          external iplDLL;
procedure iplGetBilinearQuad;           external iplDLL;
procedure iplGetBilinearQuadROI;        external iplDLL;
procedure iplGetBilinearBound;          external iplDLL;
procedure iplGetBilinearBoundROI;       external iplDLL;
procedure iplGetBilinearTransform;      external iplDLL;
procedure iplGetBilinearTransformROI;   external iplDLL;
procedure iplGetPerspectiveQuad;        external iplDLL;
procedure iplGetPerspectiveQuadROI;     external iplDLL;
procedure iplGetPerspectiveBound;       external iplDLL;
procedure iplGetPerspectiveBoundROI;    external iplDLL;
procedure iplGetPerspectiveTransform;   external iplDLL;
procedure iplGetPerspectiveTransformROI;external iplDLL;

procedure iplResize;                    external iplDLL;
procedure iplZoom;                      external iplDLL;
procedure iplDecimateBlur;              external iplDLL;
procedure iplDecimate;                  external iplDLL;
procedure iplMirror;                    external iplDLL;

function  iplNorm;                      external iplDLL;

procedure iplMoments;                   external iplDLL;

function iplGetSpatialMoment;           external iplDLL;
function iplGetNormalizedSpatialMoment; external iplDLL;
function iplGetCentralMoment;           external iplDLL;
function iplGetNormalizedCentralMoment; external iplDLL;

function iplSpatialMoment;              external iplDLL;
function iplNormalizedSpatialMoment;    external iplDLL;
function iplCentralMoment;              external iplDLL;
function iplNormalizedCentralMoment;    external iplDLL;

function  iplCheckImageHeader;          external iplDLL;

procedure iplGetPixel;                  external iplDLL;
procedure iplPutPixel;                  external iplDLL;

function iplGreater;                    external iplDLL;
function iplLess;                       external iplDLL;
function iplEqual;                      external iplDLL;
function iplEqualFPEps;                 external iplDLL;

function iplLessS;                      external iplDLL;
function iplGreaterS;                   external iplDLL;
function iplEqualS;                     external iplDLL;

function iplLessSFP;                    external iplDLL;
function iplGreaterSFP;                 external iplDLL;
function iplEqualSFP;                   external iplDLL;

function iplEqualSFPEps;                external iplDLL;

function iplScale;                      external iplDLL;
function iplScaleFP;                    external iplDLL;
function iplMinMaxFP;                   external iplDLL;

function iplNormCrossCorr;              external iplDLL;

function iplWtInit;                     external iplDLL;
function iplWtInitUserTaps;             external iplDLL;
function iplWtInitUserFilter;           external iplDLL;
function iplWtInitUserFilter4;          external iplDLL;
function iplWtDecompose;                external iplDLL;
function iplWtReconstruct;              external iplDLL;
function iplWtFree;                     external iplDLL;

procedure iplUserProcess;               external iplDLL;
procedure iplUserProcessFP;             external iplDLL;
procedure iplUserProcessPixel;          external iplDLL;

procedure iplNoiseUniformInit;          external iplDLL;
procedure iplNoiseUniformInitFP;        external iplDLL;

procedure iplNoiseGaussianInit;         external iplDLL;
procedure iplNoiseGaussianInitFP;       external iplDLL;

function  iplNoiseImage;                external iplDLL;

function iplCopyImageHeader(SrcImage : PIplImage; Depth : Integer) : PIplImage;
begin
  Result := iplCreateImageHeader(
    SrcImage^.NChannels,
    SrcImage^.AlphaChannel,
    Depth,
    SrcImage^.ColorModel,
    SrcImage^.ChannelSeq,
    SrcImage^.DataOrder,
    SrcImage^.Origin,
    SrcImage^.Align,
    SrcImage^.Width,
    SrcImage^.Height,
    nil,
    nil,
    nil,
    nil);
end;

procedure iplResizeFit(SrcImage, DstImage : PIplImage;
                       Interpolate        : Integer);
var
  XDst, YDst : Integer;
  XSrc, YSrc : Integer;
begin
  if DstImage^.Roi <> nil then
  begin
    XDst := DstImage^.Roi^.Width;
    YDst := DstImage^.Roi^.Height;
  end
  else
  begin
    XDst := DstImage^.Width;
    YDst := DstImage^.Height;
  end;
  if SrcImage^.Roi <> nil then
  begin
    XSrc := SrcImage^.Roi^.Width;
    YSrc := SrcImage^.Roi^.Height;
  end
  else
  begin
    XSrc := SrcImage^.Width;
    YSrc := SrcImage^.Height;
  end;
  iplResize(SrcImage,DstImage,XDst,XSrc,YDst,YSrc,Interpolate);
end;

procedure iplDecimateFit(SrcImage, DstImage : PIplImage;
                         Interpolate        : Integer);
var
  XDst, YDst : Integer;
  XSrc, YSrc : Integer;
begin
  if DstImage^.Roi <> nil then
  begin
    XDst := DstImage^.Roi^.Width;
    YDst := DstImage^.Roi^.Height;
  end
  else
  begin
    XDst := DstImage^.Width;
    YDst := DstImage^.Height;
  end;
  if SrcImage^.Roi <> nil then
  begin
    XSrc := SrcImage^.Roi^.Width;
    YSrc := SrcImage^.Roi^.Height;
  end
  else
  begin
    XSrc := SrcImage^.Width;
    YSrc := SrcImage^.Height;
  end;
  iplDecimate(SrcImage,DstImage,XDst,XSrc,YDst,YSrc,Interpolate);
end;

procedure iplZoomFit(SrcImage, DstImage : PIplImage;
                     Interpolate        : Integer);
var
  XDst, YDst : Integer;
  XSrc, YSrc : Integer;
begin
  if DstImage^.Roi <> nil then
  begin
    XDst := DstImage^.Roi^.Width;
    YDst := DstImage^.Roi^.Height;
  end
  else
  begin
    XDst := DstImage^.Width;
    YDst := DstImage^.Height;
  end;
  if SrcImage^.Roi <> nil then
  begin
    XSrc := SrcImage^.Roi^.Width;
    YSrc := SrcImage^.Roi^.Height;
  end
  else
  begin
    XSrc := SrcImage^.Width;
    YSrc := SrcImage^.Height;
  end;
  iplZoom(SrcImage,DstImage,XDst,XSrc,YDst,YSrc,Interpolate);
end;

procedure iplRotateCenter(SrcImage, DstImage : PIplImage;
                          Angle              : Double;
                          XCenter, YCenter   : Double;
                          Interpolate        : Integer);
var
  XShift, YShift : Double;
begin
  XShift := 0;
  YShift := 0;
  iplGetRotateShift(XCenter,YCenter,Angle,XShift,YShift);
  iplRotate(SrcImage,DstImage,Angle,XShift,YShift,Interpolate);
end;

end.
