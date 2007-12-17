{
 * Copyright (C) 2001-2003 Michael Niedermayer <michaelni@gmx.at>
 *
 * This file is part of FFmpeg.
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
}
{
 * @file swscale.h
 * @brief
 *     external api for the swscale stuff
}

unit swscale;

{$IFDEF FPC}
  {$IFNDEF win32}
  {$LINKLIB libavutil}
  {$LINKLIB libswscale}
  {$ENDIF}
  {$MODE DELPHI } (* CAT *)
  {$PACKENUM 4}    (* every enum type variables uses 4 bytes, CAT *)
  {$PACKRECORDS C}    (* GCC compatible, Record Packing, CAT *)
{$ENDIF}

interface

uses
  avutil;

const
{$IFDEF win32}
  sw__scale = 'swscale-0.dll';
{$ELSE}
  sw__scale = 'libswscale.so'; // .0.5.0
{$ENDIF}

type
  TQuadIntArray = array[0..3] of integer;
  PQuadIntArray = ^TQuadIntArray;
  TIntArray = array[0..0] of integer;
  PIntArray = ^TIntArray;
  TPByteArray = array[0..0] of Pbyte;
  PPByteArray = ^TPByteArray;

const
  LIBSWSCALE_VERSION_INT = ((0 shl 16)+(5 shl 8))+0;
  LIBSWSCALE_VERSION = '0.5.0';
  LIBSWSCALE_BUILD = LIBSWSCALE_VERSION_INT;

  LIBSWSCALE_IDENT = 'SwS'+LIBSWSCALE_VERSION;

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
    coeff: Pdouble;
    length: integer;
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
    av_class: Pointer;
    swScale: Pointer;
    srxW,srcH, dstH: integer;
    chrSrcW, chrSrcH, chrDstW, chrDstH: integer;
    lumXInc, chrXInc, lumYInc, chrYInc: integer;
    dstFormat, srcFormat, origDstFormat, origSrcFormat: integer;
    chrSrcHSubSample, chrSrcVSubSample, chrIntHSubSample, chrIntVSubSample: integer;
    chrDstHSubSample, chrDstVSubSample: integer;
    vChrDrop, sliceDir: integer;
    param: array[0..1] of double;
    {internal structure}
  end;


procedure sws_freeContext(swsContext: PSwsContext);
  cdecl; external sw__scale;

function sws_getContext(srcW: integer; srcH: integer; srcFormat: TAVPixelFormat; dstW: integer; dstH: integer;dstFormat: TAVPixelFormat; flags: integer;
              srcFilter: PSwsFilter; dstFilter: PSwsFilter; param: Pdouble): PSwsContext;
  cdecl; external sw__scale;
function sws_scale(context: PSwsContext; src: {PPByteArray}array of pbyte; srcStride: {PIntArray} array of integer; srcSliceY: integer; srcSliceH: integer;
              dst: {PPByteArray} array of pbyte; dstStride: {PIntArray}array of integer): integer;
  cdecl; external sw__scale;
function sws_scale_ordered(context: PSwsContext; src: PPByteArray; srcStride: PIntArray; srcSliceY: integer;
              srcSliceH: integer; dst: PPByteArray; dstStride: PIntArray): integer;
  cdecl; external sw__scale; deprecated;

function sws_setColorspaceDetails(c: PSwsContext; inv_table: PQuadIntArray; srcRange: integer; table: PQuadIntArray; dstRange: integer;
              brightness: integer; contrast: integer; saturation: integer): integer;
  cdecl; external sw__scale;
function sws_getColorspaceDetails(c: PSwsContext; var inv_table: PQuadIntArray; var srcRange: integer; var table: PQuadIntArray; var dstRange: integer;
              var brightness: integer; var contrast: integer; var saturation: integer): integer;
  cdecl; external sw__scale;
function sws_getGaussianVec(variance: double; quality: double): PSwsVector;
  cdecl; external sw__scale;
function sws_getConstVec(c: double; length: integer): PSwsVector;
  cdecl; external sw__scale;
function sws_getIdentityVec: PSwsVector;
  cdecl; external sw__scale;
procedure sws_scaleVec(a: PSwsVector; scalar: double);
  cdecl; external sw__scale;
procedure sws_normalizeVec(a: PSwsVector; height: double);
  cdecl; external sw__scale;
procedure sws_convVec(a: PSwsVector; b: PSwsVector);
  cdecl; external sw__scale;
procedure sws_addVec(a: PSwsVector; b: PSwsVector);
  cdecl; external sw__scale;
procedure sws_subVec(a: PSwsVector; b: PSwsVector);
  cdecl; external sw__scale;
procedure sws_shiftVec(a: PSwsVector; shift: integer);
  cdecl; external sw__scale;
function sws_cloneVec(a: PSwsVector): PSwsVector;
  cdecl; external sw__scale;

procedure sws_printVec(a: PSwsVector);
  cdecl; external sw__scale;
procedure sws_freeVec(a: PSwsVector);
  cdecl; external sw__scale;

function sws_getDefaultFilter(lumaGBlur: single; chromaGBlur: single; lumaSarpen: single; chromaSharpen: single; chromaHShift: single;
              chromaVShift: single; verbose: integer): PSwsFilter;
  cdecl; external sw__scale;
procedure sws_freeFilter(filter: PSwsFilter);
  cdecl; external sw__scale;

function sws_getCachedContext(context: PSwsContext;
              srcW: integer; srcH: integer; srcFormat: integer;
              dstW: integer; dstH: integer; dstFormat: integer; flags: integer;
              srcFilter: PSwsFilter; dstFilter: PSwsFilter; param: Pdouble): PSwsContext;
  cdecl; external sw__scale;

implementation

end.
