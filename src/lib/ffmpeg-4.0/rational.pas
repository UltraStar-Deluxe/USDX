(*
 * rational numbers
 * Copyright (c) 2003 Michael Niedermayer <michaelni@gmx.at>
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
 * Conversion of libavutil/rational.h
 * avutil version 54.7.100
 *
 *)

unit rational;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

{$I switches.inc}

interface

uses
  ctypes,
  UConfig;

(**
 * @addtogroup lavu_math
 * @{
 *)

type
(**
 * Rational number (pair of numerator and denominator).
 *)
  PAVRational = ^TAVRational;
  TAVRational = record
    num: cint; ///< Numerator
    den: cint; ///< Denominator
  end;

  TAVRationalArray = array[0 .. (MaxInt div SizeOf(TAVRational))-1] of TAVRational;
  PAVRationalArray = ^TAVRationalArray;

(**
 * Create an AVRational.
 *
 * Useful for compilers that do not support compound literals.
 *
 * @note The return value is not reduced.
 * @see av_reduce()
 */
static inline AVRational av_make_q(int num, int den)
{
      AVRational r = { num, den };
      return r;
}

/**)  
  
(**
 * Create an AVRational.
 *
 * Useful for compilers that do not support compound literals.
 *
 * @note The return value is not reduced.
 * @see av_reduce()
 *)
function av_make_q(num, den: cint): TAVRational; {$IFDEF HasInline}inline;{$ENDIF}
  
(**
 * Compare two rationals.
 *
 * @param a First rational
 * @param b Second rational
 *
 * @return One of the following values:
 *         - 0 if `a == b`
 *         - 1 if `a > b`
 *         - -1 if `a < b`
 *         - `INT_MIN` if one of the values is of the form `0 / 0`
 *)
function av_cmp_q(a: TAVRational; b: TAVRational): cint; {$IFDEF HasInline}inline;{$ENDIF}

(**
 * Convert an AVRational to a `double`.
 * @param a AVRational to convert
 * @return `a` in floating-point form
 * @see av_d2q()
 *)
function av_q2d(a: TAVRational): cdouble; {$IFDEF HasInline}inline;{$ENDIF}

(**
 * Reduce a fraction.
 *
 * This is useful for framerate calculations.
 *
 * @param[out] dst_num Destination numerator
 * @param[out] dst_den Destination denominator
 * @param[in]      num Source numerator
 * @param[in]      den Source denominator
 * @param[in]      max Maximum allowed values for `dst_num` & `dst_den`
 * @return 1 if the operation is exact, 0 otherwise
 *)
function av_reduce(dst_num: PCint; dst_den: PCint; num: cint64; den: cint64; max: cint64): cint;
  cdecl; external av__util;

(**
 * Multiply two rationals.
 * @param b First rational
 * @param c Second rational
 * @return b*c
 *)
function av_mul_q(b: TAVRational; c: TAVRational): TAVRational;
  cdecl; external av__util; {av_const}

(**
 * Divide one rational by another.
 * @param b First rational
 * @param c Second rational
 * @return b/c
 *)
function av_div_q(b: TAVRational; c: TAVRational): TAVRational;
  cdecl; external av__util; {av_const}

(**
 * Add two rationals.
 * @param b First rational
 * @param c Second rational
 * @return b+c
 *)
function av_add_q(b: TAVRational; c: TAVRational): TAVRational;
  cdecl; external av__util; {av_const}

(**
 * Subtract one rational from another.
 * @param b First rational
 * @param c Second rational
 * @return b-c
 *)
function av_sub_q(b: TAVRational; c: TAVRational): TAVRational;
  cdecl; external av__util; {av_const}

(**
 * Invert a rational.
 * @param q value
 * @return 1 / q
 *)
function av_inv_q(q: TAVRational): TAVRational; {$IFDEF HasInline}inline;{$ENDIF}

(**
 * Convert a double precision floating point number to a rational.
 *
 * In case of infinity, the returned value is expressed as `{1, 0}` or
 * `{-1, 0}` depending on the sign.
 *
 * @param d   `double` to convert
 * @param max Maximum allowed numerator and denominator
 * @return `d` in AVRational form
 * @see av_q2d()
 *)
function av_d2q(d: cdouble; max: cint): TAVRational;
  cdecl; external av__util; {av_const}

(**
 * Find which of the two rationals is closer to another rational.
 *
 * @param q     Rational to be compared against
 * @param q1,q2 Rationals to be tested
 * @return One of the following values:
 *         - 1 if `q1` is nearer to `q` than `q2`
 *         - -1 if `q2` is nearer to `q` than `q1`
 *         - 0 if they have the same distance
 *)
function av_nearer_q(q, q1, q2: TAVRational): cint;
  cdecl; external av__util;

(**
 * Find the value in a list of rationals nearest a given reference rational.
 *
 * @param q      Reference rational
 * @param q_list Array of rationals terminated by `{0, 0}`
 * @return Index of the nearest value found in the array
 *)
function av_find_nearest_q_idx(q: TAVRational; q_list: {const} PAVRationalArray): cint;
  cdecl; external av__util;

(**
 * Convert an AVRational to a IEEE 32-bit `float` expressed in fixed-point
 * format.
 *
 * @param q Rational to be converted
 * @return Equivalent floating-point value, expressed as an unsigned 32-bit
 *         integer.
 * @note The returned value is platform-indepedant.
 *)
function av_q2intfloat(q: TAVRational): cuint32;
  cdecl; external av__util;

implementation

function av_cmp_q (a: TAVRational; b: TAVRational): cint; {$IFDEF HasInline}inline;{$ENDIF}
var
  tmp: cint64;
begin
  tmp := a.num * cint64(b.den) - b.num * cint64(a.den);

{ old version
  if (tmp <> 0) then
    Result := (tmp shr 63) or 1
  else
    Result := 0;
}
{ C original:
    if(tmp) return ((tmp ^ a.den ^ b.den)>>63)|1;
    else if(b.den && a.den) return 0;
    else if(a.num && b.num) return (a.num>>31) - (b.num>>31);
    else                    return INT_MIN;
}

  if tmp <> 0 then
    Result := ((tmp xor a.den xor b.den) shr 63) or 1
  else if (b.den and a.den) <> 0 then
    Result := 0
  else if (a.num and b.num) <> 0 then
    Result := (a.num shr 31) - (b.num shr 31)
  else
    Result := low(cint);

end;

function av_q2d(a: TAVRational): cdouble; {$IFDEF HasInline}inline;{$ENDIF}
begin
  Result := a.num / a.den;
end;

function av_inv_q(q: TAVRational): TAVRational; {$IFDEF HasInline}inline;{$ENDIF}
begin
  Result.num := q.den;
  Result.den := q.num;
end;

function av_make_q(num, den: cint): TAVRational; {$IFDEF HasInline}inline;{$ENDIF}
begin
  Result.num := num;
  Result.den := den;
end;

end.
