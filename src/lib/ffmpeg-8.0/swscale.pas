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
