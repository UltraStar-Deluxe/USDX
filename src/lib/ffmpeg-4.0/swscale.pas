unit swscale;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

{$IFDEF DARWIN}
  {$linklib libswscale}
{$ENDIF}

interface

uses
  ctypes,
  avutil,
  UConfig;

const
  (* Max. supported version by this header *)
  LIBSWSCALE_MAX_VERSION_MAJOR   =  5;
  LIBSWSCALE_MAX_VERSION_MINOR   =  9;
  LIBSWSCALE_MAX_VERSION_RELEASE =  100;
  LIBSWSCALE_MAX_VERSION = (LIBSWSCALE_MAX_VERSION_MAJOR * VERSION_MAJOR) +
                           (LIBSWSCALE_MAX_VERSION_MINOR * VERSION_MINOR) +
                           (LIBSWSCALE_MAX_VERSION_RELEASE * VERSION_RELEASE);

(* Check if linked versions are supported *)
{$IF (LIBSWSCALE_VERSION > LIBSWSCALE_MAX_VERSION)}
  {$MESSAGE Error 'Linked version of libswscale is not yet supported!'}
{$IFEND}


const
  SWS_FAST_BILINEAR = 1;
type
  PSwsContext = ^TSwsContext;
  TSwsContext = record
    do_not_instantiate_this_record: incomplete_record;
  end;
  PSwsFilter = ^TSwsFilter;
  TSwsFilter = record
    do_not_instantiate_this_record: incomplete_record;
  end;
function swscale_version(): cuint; cdecl; external sw__scale;
function sws_isSupportedInput(pix_fmt: TAVPixelFormat): cint; cdecl; external sw__scale;
function sws_scale(c: PSwsContext; srcSlice: ppcuint8; srcStride: pcint; srcSliceY: cint; srcSliceH: cint; dst: ppcuint8; dstStride: pcint): cint; cdecl; external sw__scale;
function sws_getContext(srcW: cint; srcH: cint; srcFormat: TAVPixelFormat; dstW: cint; dstH: cint; dstFormat: TAVPixelFormat; flags: cint; srcFilter: PSwsFilter; dstFilter: PSwsFilter; param: pcdouble): PSwsContext; cdecl; external sw__scale;
procedure sws_freeContext(swsContext: PSwsContext); cdecl; external sw__scale;
implementation
end.
