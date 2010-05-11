(*
 * copyright (c) 2005 Michael Niedermayer <michaelni@gmx.at>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *)

(*
 * This is a part of Pascal porting of ffmpeg.
 * - Originally by Victor Zinetz for Delphi and Free Pascal on Windows.
 * - For Mac OS X, some modifications were made by The Creative CAT, denoted as CAT
 *   in the source codes.
 * - Changes and updates by the UltraStar Deluxe Team
 *)

(*
 * Conversion of libavutil/mathematics.h
 * avutil max. version 50.15.2, revision 23059, Tue May 11 22:10:00 2010 CET 
 *
 *)

unit mathematics;

{$IFDEF FPC}
  {$MODE DELPHI }
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

interface

uses
  ctypes,
  rational,
  UConfig;

const
  M_E          = 2.7182818284590452354;   // e
  M_LN2        = 0.69314718055994530942;  // log_e 2
  M_LN10       = 2.30258509299404568402;  // log_e 10
{$IF LIBAVUTIL_VERSION >= 50009000} // >= 50.9.0
  M_LOG2_10    = 3.32192809488736234787;  // log_2 10
{$IFEND}
  M_PI         = 3.14159265358979323846;  // pi
  M_SQRT1_2    = 0.70710678118654752440;  // 1/sqrt(2)
{$IF LIBAVUTIL_VERSION >= 50014000} // >= 50.14.0
  M_SQRT2      = 1.41421356237309504880;  // sqrt(2)
{$IFEND}
{$IF LIBAVUTIL_VERSION >= 50005001} // >= 50.5.1
  NAN          = 0.0/0.0;     
  INFINITY     = 1.0/0.0;     
{$IFEND}

type
  TAVRounding = (
    AV_ROUND_ZERO     = 0, ///< Round toward zero.
    AV_ROUND_INF      = 1, ///< Round away from zero.
    AV_ROUND_DOWN     = 2, ///< Round toward -infinity.
    AV_ROUND_UP       = 3, ///< Round toward +infinity.
    AV_ROUND_NEAR_INF = 5  ///< Round to nearest and halfway cases away from zero.
  );

{$IF LIBAVUTIL_VERSION >= 49013000} // 49.13.0
(**
 * Returns the greatest common divisor of a and b.
 * If both a or b are 0 or either or both are <0 then behavior is
 * undefined.
 *)
function av_gcd(a: cint64; b: cint64): cint64;
  cdecl; external av__util; {av_const}
{$IFEND}

(**
 * Rescales a 64-bit integer with rounding to nearest.
 * A simple a*b/c isn't possible as it can overflow.
 *)
function av_rescale (a, b, c: cint64): cint64;
  cdecl; external av__util; {av_const}

(**
 * Rescales a 64-bit integer with specified rounding.
 * A simple a*b/c isn't possible as it can overflow.
 *)
function av_rescale_rnd (a, b, c: cint64; enum: TAVRounding): cint64;
  cdecl; external av__util; {av_const}

(**
 * Rescales a 64-bit integer by 2 rational numbers.
 *)
function av_rescale_q (a: cint64; bq, cq: TAVRational): cint64;
  cdecl; external av__util; {av_const}

{$IF LIBAVUTIL_VERSION >= 50008000} // 50.8.0
(**
 * Compares 2 timestamps each in its own timebases.
 * The result of the function is undefined if one of the timestamps
 * is outside the int64_t range when represented in the others timebase.
 * @return -1 if ts_a is before ts_b, 1 if ts_a is after ts_b or 0 if they represent the same position
 *)
function av_compare_ts(ts_a: cint64; tb_a: TAVRational; ts_b: cint64; tb_b: TAVRational): cint;
  cdecl; external av__util;
{$IFEND}

implementation

end.
