(*
 * Copyright (C) 2001-2003 Michael Niedermayer <michaelni@gmx.at>
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *)

(*
 * FFmpeg Pascal port
 * - Ported by the UltraStar Deluxe Team
 *)

(*
 * Conversion of libswscale/swscale.h
 * revision 27592, Fri Sep 12 21:46:53 2008 UTC 
 *)
 
unit swscale;

{$IFDEF FPC}
  {$MODE DELPHI }
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
  LIBSWSCALE_MAX_VERSION_MAJOR   = 0;
  LIBSWSCALE_MAX_VERSION_MINOR   = 6;
  LIBSWSCALE_MAX_VERSION_RELEASE = 1;
  LIBSWSCALE_MAX_VERSION = (LIBSWSCALE_MAX_VERSION_MAJOR * VERSION_MAJOR) +
                           (LIBSWSCALE_MAX_VERSION_MINOR * VERSION_MINOR) +
                           (LIBSWSCALE_MAX_VERSION_RELEASE * VERSION_RELEASE);

(* Check if linked versions are supported *)
{$IF (LIBSWSCALE_VERSION > LIBSWSCALE_MAX_VERSION)}
  {$MESSAGE Error 'Linked version of libswscale is not yet supported!'}
{$IFEND}

type
  TQuadCintArray = array[0..3] of cint;
  PQuadCintArray = ^TQuadCintArray;
  TCintArray = array[0..0] of cint;
  PCintArray = ^TCintArray;
  TPCuint8Array = array[0..0] of PCuint8;
  PPCuint8Array = ^TPCuint8Array;

{$IF LIBSWSCALE_VERSION >= 000006001} // 0.6.1
(**
 * Returns the LIBSWSCALE_VERSION_INT constant.
 *)
function swscale_version(): cuint;
  cdecl; external sw__scale;
{$IFEND}

const
  {* values for the flags, the stuff on the command line is different *}
  SWS_FAST_BILINEAR =    1;
  SWS_BILINEAR      =    2;
  SWS_BICUBIC       =    4;
  SWS_X             =    8;
  SWS_POINT         =  $10;
  SWS_AREA          =  $20;
  SWS_BICUBLIN      =  $40;
  SWS_GAUSS         =  $80;
  SWS_SINC          = $100;
  SWS_LANCZOS       = $200;
  SWS_SPLINE        = $400;

  SWS_SRC_V_CHR_DROP_MASK  = $30000;
  SWS_SRC_V_CHR_DROP_SHIFT = 16;

  SWS_PARAM_DEFAULT        = 123456;

  SWS_PRINT_INFO           = $1000;

  //the following 3 flags are not completely implemented
  //internal chrominace subsampling info
  SWS_FULL_CHR_H_INT    = $2000;
  //input subsampling info
  SWS_FULL_CHR_H_INP    = $4000;
  SWS_DIRECT_BGR        = $8000;
  SWS_ACCURATE_RND      = $40000;
  SWS_BITEXACT          = $80000;

  SWS_CPU_CAPS_MMX      = $80000000;
  SWS_CPU_CAPS_MMX2     = $20000000;
  SWS_CPU_CAPS_3DNOW    = $40000000;
  SWS_CPU_CAPS_ALTIVEC  = $10000000;
  SWS_CPU_CAPS_BFIN     = $01000000;

  SWS_MAX_REDUCE_CUTOFF = 0.002;

  SWS_CS_ITU709         = 1;
  SWS_CS_FCC            = 4;
  SWS_CS_ITU601         = 5;
  SWS_CS_ITU624         = 5;
  SWS_CS_SMPTE170M      = 5;
  SWS_CS_SMPTE240M      = 7;
  SWS_CS_DEFAULT        = 5;


type

  // when used for filters they must have an odd number of elements
  // coeffs cannot be shared between vectors
  PSwsVector = ^TSwsVector;
  TSwsVector = record
    coeff: PCdouble;
    length: cint;
  end;

  // vectors can be shared
  PSwsFilter = ^TSwsFilter;
  TSwsFilter = record
    lumH: PSwsVector;
    lumV: PSwsVector;
    chrH: PSwsVector;
    chrV: PSwsVector;
  end;

  PSwsContext = ^TSwsContext;
  TSwsContext = record
    {internal structure}
  end;


procedure sws_freeContext(swsContext: PSwsContext);
  cdecl; external sw__scale;

function sws_getContext(srcW: cint; srcH: cint; srcFormat: TAVPixelFormat;
              dstW: cint; dstH: cint; dstFormat: TAVPixelFormat; flags: cint;
              srcFilter: PSwsFilter; dstFilter: PSwsFilter; param: PCdouble): PSwsContext;
  cdecl; external sw__scale;
function sws_scale(context: PSwsContext; src: PPCuint8Array; srcStride: PCintArray; srcSliceY: cint; srcSliceH: cint;
              dst: PPCuint8Array; dstStride: PCintArray): cint;
  cdecl; external sw__scale;
function sws_scale_ordered(context: PSwsContext; src: PPCuint8Array; srcStride: PCintArray; srcSliceY: cint;
              srcSliceH: cint; dst: PPCuint8Array; dstStride: PCintArray): cint;
  cdecl; external sw__scale; deprecated;

function sws_setColorspaceDetails(c: PSwsContext; inv_table: PQuadCintArray; srcRange: cint; table: PQuadCintArray; dstRange: cint;
              brightness: cint; contrast: cint; saturation: cint): cint;
  cdecl; external sw__scale;
function sws_getColorspaceDetails(c: PSwsContext; var inv_table: PQuadCintArray; var srcRange: cint; var table: PQuadCintArray; var dstRange: cint;
              var brightness: cint; var contrast: cint; var saturation: cint): cint;
  cdecl; external sw__scale;
function sws_getGaussianVec(variance: cdouble; quality: cdouble): PSwsVector;
  cdecl; external sw__scale;
function sws_getConstVec(c: cdouble; length: cint): PSwsVector;
  cdecl; external sw__scale;
function sws_getIdentityVec: PSwsVector;
  cdecl; external sw__scale;
procedure sws_scaleVec(a: PSwsVector; scalar: cdouble);
  cdecl; external sw__scale;
procedure sws_normalizeVec(a: PSwsVector; height: cdouble);
  cdecl; external sw__scale;
procedure sws_convVec(a: PSwsVector; b: PSwsVector);
  cdecl; external sw__scale;
procedure sws_addVec(a: PSwsVector; b: PSwsVector);
  cdecl; external sw__scale;
procedure sws_subVec(a: PSwsVector; b: PSwsVector);
  cdecl; external sw__scale;
procedure sws_shiftVec(a: PSwsVector; shift: cint);
  cdecl; external sw__scale;
function sws_cloneVec(a: PSwsVector): PSwsVector;
  cdecl; external sw__scale;

procedure sws_printVec(a: PSwsVector);
  cdecl; external sw__scale;
procedure sws_freeVec(a: PSwsVector);
  cdecl; external sw__scale;

function sws_getDefaultFilter(lumaGBlur: cfloat; chromaGBlur: cfloat; lumaSarpen: cfloat; chromaSharpen: cfloat; chromaHShift: cfloat;
              chromaVShift: cfloat; verbose: cint): PSwsFilter;
  cdecl; external sw__scale;
procedure sws_freeFilter(filter: PSwsFilter);
  cdecl; external sw__scale;

function sws_getCachedContext(context: PSwsContext;
              srcW: cint; srcH: cint; srcFormat: cint;
              dstW: cint; dstH: cint; dstFormat: cint; flags: cint;
              srcFilter: PSwsFilter; dstFilter: PSwsFilter; param: PCdouble): PSwsContext;
  cdecl; external sw__scale;

implementation

end.
