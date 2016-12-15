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

(**
 * @file
 * @addtogroup lavu_math
 * Mathematical utilities for working with timestamp and time base.
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
  (**
   * Rounding methods.
   *)
  TAVRounding = (
    AV_ROUND_ZERO     = 0, ///< Round toward zero.
    AV_ROUND_INF      = 1, ///< Round away from zero.
    AV_ROUND_DOWN     = 2, ///< Round toward -infinity.
    AV_ROUND_UP       = 3, ///< Round toward +infinity.
    AV_ROUND_NEAR_INF = 5, ///< Round to nearest and halfway cases away from zero.
    (**
     * Flag telling rescaling functions to pass `INT64_MIN`/`MAX` through
     * unchanged, avoiding special cases for #AV_NOPTS_VALUE.
     *
     * Unlike other values of the enumeration AVRounding, this value is a
     * bitmask that must be used in conjunction with another value of the
     * enumeration through a bitwise OR, in order to set behavior for normal
     * cases.
     *
     * @code{.c}
     * av_rescale_rnd(3, 1, 2, AV_ROUND_UP | AV_ROUND_PASS_MINMAX);
     * // Rescaling 3:
     * //     Calculating 3 * 1 / 2
     * //     3 / 2 is rounded up to 2
     * //     => 2
     *
     * av_rescale_rnd(AV_NOPTS_VALUE, 1, 2, AV_ROUND_UP | AV_ROUND_PASS_MINMAX);
     * // Rescaling AV_NOPTS_VALUE:
     * //     AV_NOPTS_VALUE == INT64_MIN
     * //     AV_NOPTS_VALUE is passed through
     * //     => AV_NOPTS_VALUE
     * @endcode
     *)
    AV_ROUND_PASS_MINMAX = 8192
  );

(**
 * Compute the greatest common divisor of two integer operands.
 *
 * @param a,b Operands
 * @return GCD of a and b up to sign; if a >= 0 and b >= 0, return value is >= 0;
 * if a == 0 and b == 0, returns 0.
 *)
function av_gcd(a, b: cint64): cint64;
  cdecl; external av__util; {av_const}

(**
 * Rescale a 64-bit integer with rounding to nearest.
 *
 * The operation is mathematically equivalent to `a * b / c`, but writing that
 * directly can overflow.
 *
 * This function is equivalent to av_rescale_rnd() with #AV_ROUND_NEAR_INF.
 *
 * @see av_rescale_rnd(), av_rescale_q(), av_rescale_q_rnd()
 *)
function av_rescale (a, b, c: cint64): cint64;
  cdecl; external av__util; {av_const}

(**
 * Rescale a 64-bit integer with specified rounding.
 *
 * The operation is mathematically equivalent to `a * b / c`, but writing that
 * directly can overflow, and does not support different rounding methods.
 *
 * @see av_rescale(), av_rescale_q(), av_rescale_q_rnd()
 *)
function av_rescale_rnd (a, b, c: cint64; rnd: TAVRounding): cint64;
  cdecl; external av__util; {av_const}

(**
 * Rescale a 64-bit integer by 2 rational numbers.
 *
 * The operation is mathematically equivalent to `a * bq / cq`.
 *
 * This function is equivalent to av_rescale_q_rnd() with #AV_ROUND_NEAR_INF.
 *
 * @see av_rescale(), av_rescale_rnd(), av_rescale_q_rnd()
 *)
function av_rescale_q (a: cint64; bq, cq: TAVRational): cint64;
  cdecl; external av__util; {av_const}

(**
 * Rescale a 64-bit integer by 2 rational numbers with specified rounding.
 *
 * The operation is mathematically equivalent to `a * bq / cq`.
 *
 * @see av_rescale(), av_rescale_rnd(), av_rescale_q()
 *)
function av_rescale_q_rnd(a: cint64; bq, cq: TAVRational;
                          rnd: TAVRounding): cint64;
  cdecl; external av__util; {av_const}

(**
 * Compare two timestamps each in its own time base.
 *
 * @return One of the following values:
 *         - -1 if `ts_a` is before `ts_b`
 *         - 1 if `ts_a` is after `ts_b`
 *         - 0 if they represent the same position
 *
 * @warning
 * The result of the function is undefined if one of the timestamps is outside
 * the `int64_t` range when represented in the other's timebase.
 *)
function av_compare_ts(ts_a: cint64; tb_a: TAVRational; ts_b: cint64; tb_b: TAVRational): cint;
  cdecl; external av__util;
 
(**
 * Compare the remainders of two integer operands divided by a common divisor.
 *
 * In other words, compare the least significant `log2(mod)` bits of integers
 * `a` and `b`.
 *
 * @code{.c}
 * av_compare_mod(0x11, 0x02, 0x10) < 0 // since 0x11 % 0x10  (0x1) < 0x02 % 0x10  (0x2)
 * av_compare_mod(0x11, 0x02, 0x20) > 0 // since 0x11 % 0x20 (0x11) > 0x02 % 0x20 (0x02)
 * @endcode
 *
 * @param a,b Operands
 * @param mod Divisor; must be a power of 2
 * @return
 *         - a negative value if `a % mod < b % mod`
 *         - a positive value if `a % mod > b % mod`
 *         - zero             if `a % mod == b % mod`
 *)
function av_compare_mod(a, b, modVar: cuint64): cint64;
  cdecl; external av__util;

(**
 * Rescale a timestamp while preserving known durations.
 *
 * This function is designed to be called per audio packet to scale the input
 * timestamp to a different time base. Compared to a simple av_rescale_q()
 * call, this function is robust against possible inconsistent frame durations.
 *
 * The `last` parameter is a state variable that must be preserved for all
 * subsequent calls for the same stream. For the first call, `*last` should be
 * initialized to #AV_NOPTS_VALUE.
 *
 * @param[in]     in_tb    Input time base
 * @param[in]     in_ts    Input timestamp
 * @param[in]     fs_tb    Duration time base; typically this is finer-grained
 *                         (greater) than `in_tb` and `out_tb`
 * @param[in]     duration Duration till the next call to this function (i.e.
 *                         duration of the current packet/frame)
 * @param[in,out] last     Pointer to a timestamp expressed in terms of
 *                         `fs_tb`, acting as a state variable
 * @param[in]     out_tb   Output timebase
 * @return        Timestamp expressed in terms of `out_tb`
 *
 * @note In the context of this function, "duration" is in term of samples, not
 *       seconds.
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
