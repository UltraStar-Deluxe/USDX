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
 *)
 
(*
 * This is a part of Pascal porting of ffmpeg.
 * - Originally by Victor Zinetz for Delphi and Free Pascal on Windows.
 * - For Mac OS X, some modifications were made by The Creative CAT, denoted as CAT
 *   in the source codes.
 * - Changes and updates by the UltraStar Deluxe Team
 *)

(*
 * Conversion of libavutil/rational.h
 * revision 16912, Sun Feb 1 02:00:19 2009 UTC 
 *
 * update, MiSchi, no code change
 * Fri Jun 12 2009 22:20:00 UTC
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

type
  (*
   * rational number numerator/denominator
   *)
  PAVRational = ^TAVRational;
  TAVRational = record
    num: cint; ///< numerator
    den: cint; ///< denominator
  end;

  TAVRationalArray = array[0 .. (MaxInt div SizeOf(TAVRational))-1] of TAVRational;
  PAVRationalArray = ^TAVRationalArray;

(**
 * Compares two rationals.
 * @param a first rational
 * @param b second rational
 * @return 0 if a==b, 1 if a>b and -1 if a<b
 *)
function av_cmp_q(a: TAVRational; b: TAVRational): cint; {$IFDEF HasInline}inline;{$ENDIF}

(**
 * Converts rational to double.
 * @param a rational to convert
 * @return (double) a
 *)
function av_q2d(a: TAVRational): cdouble; {$IFDEF HasInline}inline;{$ENDIF}

(**
 * Reduces a fraction.
 * This is useful for framerate calculations.
 * @param dst_num destination numerator
 * @param dst_den destination denominator
 * @param num source numerator
 * @param den source denominator
 * @param max the maximum allowed for dst_num & dst_den
 * @return 1 if exact, 0 otherwise
 *)
function av_reduce(dst_num: PCint; dst_den: PCint; num: cint64; den: cint64; max: cint64): cint;
  cdecl; external av__util;

(**
 * Multiplies two rationals.
 * @param b first rational
 * @param c second rational
 * @return b*c
 *)
function av_mul_q(b: TAVRational; c: TAVRational): TAVRational;
  cdecl; external av__util; {av_const}

(**
 * Divides one rational by another.
 * @param b first rational
 * @param c second rational
 * @return b/c
 *)
function av_div_q(b: TAVRational; c: TAVRational): TAVRational;
  cdecl; external av__util; {av_const}

(**
 * Adds two rationals.
 * @param b first rational
 * @param c second rational
 * @return b+c
 *)
function av_add_q(b: TAVRational; c: TAVRational): TAVRational;
  cdecl; external av__util; {av_const}

(**
 * Subtracts one rational from another.
 * @param b first rational
 * @param c second rational
 * @return b-c
 *)
function av_sub_q(b: TAVRational; c: TAVRational): TAVRational;
  cdecl; external av__util; {av_const}

(**
 * Converts a double precision floating point number to a rational.
 * @param d double to convert
 * @param max the maximum allowed numerator and denominator
 * @return (AVRational) d
 *)
function av_d2q(d: cdouble; max: cint): TAVRational;
  cdecl; external av__util; {av_const}

{$IF LIBAVUTIL_VERSION >= 49011000} // 49.11.0
(**
 * @return 1 if q1 is nearer to q than q2, -1 if q2 is nearer
 * than q1, 0 if they have the same distance.
 *)
function av_nearer_q(q, q1, q2: TAVRational): cint;
  cdecl; external av__util;

(**
 * Finds the nearest value in q_list to q.
 * @param q_list an array of rationals terminated by {0, 0}
 * @return the index of the nearest value found in the array
 *)
function av_find_nearest_q_idx(q: TAVRational; q_list: {const} PAVRationalArray): cint;
  cdecl; external av__util;
{$IFEND}

implementation

function av_cmp_q (a: TAVRational; b: TAVRational): cint;
var
  tmp: cint64;
begin
  tmp := a.num * cint64(b.den) - b.num * cint64(a.den);

  if (tmp <> 0) then
    Result := (tmp shr 63) or 1
  else
    Result := 0
end;

function av_q2d(a: TAVRational): cdouble;
begin
  Result := a.num / a.den;
end;

end.
