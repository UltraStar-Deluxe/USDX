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
 * revision 15120, Sun Aug 31 07:39:47 2008 UTC 
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
  M_PI         = 3.14159265358979323846;  // pi
  M_SQRT1_2    = 0.70710678118654752440;  // 1/sqrt(2)

type
  TAVRounding = (
    AV_ROUND_ZERO     = 0, ///< round toward zero
    AV_ROUND_INF      = 1, ///< round away from zero
    AV_ROUND_DOWN     = 2, ///< round toward -infinity
    AV_ROUND_UP       = 3, ///< round toward +infinity
    AV_ROUND_NEAR_INF = 5  ///< round to nearest and halfway cases away from zero
  );

(**
 * rescale a 64bit integer with rounding to nearest.
 * a simple a*b/c isn't possible as it can overflow
 *)
function av_rescale (a, b, c: cint64): cint64;
  cdecl; external av__util; {av_const}

(**
 * rescale a 64bit integer with specified rounding.
 * a simple a*b/c isn't possible as it can overflow
 *)
function av_rescale_rnd (a, b, c: cint64; enum: TAVRounding): cint64;
  cdecl; external av__util; {av_const}

(**
 * rescale a 64bit integer by 2 rational numbers.
 *)
function av_rescale_q (a: cint64; bq, cq: TAVRational): cint64;
  cdecl; external av__util; {av_const}

implementation

end.
 
