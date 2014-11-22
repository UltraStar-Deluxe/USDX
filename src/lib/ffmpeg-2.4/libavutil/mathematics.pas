(*
 * copyright (c) 2005-2012 Michael Niedermayer <michaelni@gmx.at>
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
 *
 * This is a part of Pascal porting of ffmpeg.
 * - Originally by Victor Zinetz for Delphi and Free Pascal on Windows.
 * - For Mac OS X, some modifications were made by The Creative CAT, denoted as CAT
 *   in the source codes.
 * - Changes and updates by the UltraStar Deluxe Team
 *
 * Conversion of libavutil/mathematics.h
 * avutil version 54.7.100
 *
 *)

const
  M_E          = 2.7182818284590452354;   // e
  M_LN2        = 0.69314718055994530942;  // log_e 2
  M_LN10       = 2.30258509299404568402;  // log_e 10
  M_LOG2_10    = 3.32192809488736234787;  // log_2 10
  M_PHI        = 1.61803398874989484820;  // phi / golden ratio
  M_PI         = 3.14159265358979323846;  // pi
  M_PI_2       = 1.57079632679489661923;  // pi/2
  M_SQRT1_2    = 0.70710678118654752440;  // 1/sqrt(2)
  M_SQRT2      = 1.41421356237309504880;  // sqrt(2)
  NAN          = $7fc00000;     
  INFINITY     = $7f800000;     

(**
 * @addtogroup lavu_math
 * @
 *)

type
  TAVRounding = (
    AV_ROUND_ZERO     = 0, ///< Round toward zero.
    AV_ROUND_INF      = 1, ///< Round away from zero.
    AV_ROUND_DOWN     = 2, ///< Round toward -infinity.
    AV_ROUND_UP       = 3, ///< Round toward +infinity.
    AV_ROUND_NEAR_INF = 5, ///< Round to nearest and halfway cases away from zero.
    AV_ROUND_PASS_MINMAX = 8192  ///< Flag to pass INT64_MIN/MAX through instead of rescaling, this avoids special cases for AV_NOPTS_VALUE
  );

(**
 * Return the greatest common divisor of a and b.
 * If both a or b are 0 or either or both are <0 then behavior is
 * undefined.
 *)
function av_gcd(a, b: cint64): cint64;
  cdecl; external av__util; {av_const}

(**
 * Rescale a 64-bit integer with rounding to nearest.
 * A simple a*b/c isn't possible as it can overflow.
 *)
function av_rescale (a, b, c: cint64): cint64;
  cdecl; external av__util; {av_const}

(**
 * Rescale a 64-bit integer with specified rounding.
 * A simple a*b/c isn't possible as it can overflow.
 *
 * @return rescaled value a, or if AV_ROUND_PASS_MINMAX is set and a is
 *         INT64_MIN or INT64_MAX then a is passed through unchanged.
 *)
function av_rescale_rnd (a, b, c: cint64; d: TAVRounding): cint64;
  cdecl; external av__util; {av_const}

(**
 * Rescale a 64-bit integer by 2 rational numbers.
 *)
function av_rescale_q (a: cint64; bq, cq: TAVRational): cint64;
  cdecl; external av__util; {av_const}

(**
 * Rescale a 64-bit integer by 2 rational numbers with specified rounding.
 *
 * @return rescaled value a, or if AV_ROUND_PASS_MINMAX is set and a is
 *         INT64_MIN or INT64_MAX then a is passed through unchanged.
 *)
function av_rescale_q_rnd(a: cint64; bq, cq: TAVRational;
                          d: TAVRounding): cint64;
  cdecl; external av__util; {av_const}

(**
 * Compare 2 timestamps each in its own timebases.
 * The result of the function is undefined if one of the timestamps
 * is outside the int64_t range when represented in the others timebase.
 * @return -1 if ts_a is before ts_b, 1 if ts_a is after ts_b or 0 if they represent the same position
 *)
function av_compare_ts(ts_a: cint64; tb_a: TAVRational; ts_b: cint64; tb_b: TAVRational): cint;
  cdecl; external av__util;
 
(**
 * Compare 2 integers modulo mod.
 * That is we compare integers a and b for which only the least
 * significant log2(mod) bits are known.
 *
 * @param mod must be a power of 2
 * @return a negative value if a is smaller than b
 *         a positiv  value if a is greater than b
 *         0                if a equals          b
 *)
function av_compare_mod(a, b, modVar: cuint64): cint64;
  cdecl; external av__util;

(**
 * Rescale a timestamp while preserving known durations.
 *
 * @param in_ts Input timestamp
 * @param in_tb Input timebase
 * @param fs_tb Duration and *last timebase
 * @param duration duration till the next call
 * @param out_tb Output timebase
 *)
function av_rescale_delta(in_tb: TAVRational; in_ts: cint64;  fs_tb: TAVRational; duration: cint; last: Pcint64; out_tb: TAVRational): cint64;
  cdecl; external av__util;

(**
 * Add a value to a timestamp.
 *
 * This function guarantees that when the same value is repeatly added that
 * no accumulation of rounding errors occurs.
 *
 * @param ts Input timestamp
 * @param ts_tb Input timestamp timebase
 * @param inc value to add to ts
 * @param inc_tb inc timebase
 *)
function av_add_stable(ts_tb: TAVRational; ts: cint64; inc_tb: TAVRational; inc: cint64): cint64;
  cdecl; external av__util;
