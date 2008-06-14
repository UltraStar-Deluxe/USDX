(*
 * Rational numbers
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
 
(* This is a part of Pascal porting of ffmpeg.
 * Originally by Victor Zinetz for Delphi and Free Pascal on Windows.
 * For Mac OS X, some modifications were made by The Creative CAT, denoted as CAT
 * in the source codes *)

(*
 * Conversion of libavutil/rational.h
 * revision 12498, Wed Mar 19 06:17:43 2008 UTC
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
  UConfig;

type
(*
 * Rational number num/den.
 *)
  PAVRational = ^TAVRational;
  TAVRational = record
    num: integer; ///< numerator
    den: integer; ///< denominator
  end;

(**
 * Compare two rationals.
 * @param a first rational
 * @param b second rational
 * @return 0 if a==b, 1 if a>b and -1 if a<b.
 *)
function av_cmp_q(a: TAVRational; b: TAVRational): integer; {$IFDEF HasInline}inline;{$ENDIF}

(**
 * Rational to double conversion.
 * @param a rational to convert
 * @return (double) a
 *)
function av_q2d(a: TAVRational): double; {$IFDEF HasInline}inline;{$ENDIF}

(**
 * Reduce a fraction.
 * This is useful for framerate calculations.
 * @param dst_nom destination numerator
 * @param dst_den destination denominator
 * @param nom source numerator
 * @param den source denominator
 * @param max the maximum allowed for dst_nom & dst_den
 * @return 1 if exact, 0 otherwise
 *)
function av_reduce(dst_nom: PInteger; dst_den: PInteger; nom: int64; den: int64; max: int64): integer;
  cdecl; external av__util;

(**
 * Multiplies two rationals.
 * @param b first rational.
 * @param c second rational.
 * @return b*c.
 *)
function av_mul_q(b: TAVRational; c: TAVRational): TAVRational;
  cdecl; external av__util; {av_const}

(**
 * Divides one rational by another.
 * @param b first rational.
 * @param c second rational.
 * @return b/c.
 *)
function av_div_q(b: TAVRational; c: TAVRational): TAVRational;
  cdecl; external av__util; {av_const}

(**
 * Adds two rationals.
 * @param b first rational.
 * @param c second rational.
 * @return b+c.
 *)
function av_add_q(b: TAVRational; c: TAVRational): TAVRational;
  cdecl; external av__util; {av_const}

(**
 * Subtracts one rational from another.
 * @param b first rational.
 * @param c second rational.
 * @return b-c.
 *)
function av_sub_q(b: TAVRational; c: TAVRational): TAVRational;
  cdecl; external av__util; {av_const}

(**
 * Converts a double precision floating point number to a rational.
 * @param d double to convert
 * @param max the maximum allowed numerator and denominator
 * @return (AVRational) d.
 *)
function av_d2q(d: double; max: integer): TAVRational;
  cdecl; external av__util; {av_const}

implementation

function av_cmp_q (a: TAVRational; b: TAVRational): integer;
var
  tmp: int64;
begin
  tmp := a.num * int64(b.den) - b.num * int64(a.den);

  if (tmp <> 0) then
    Result := (tmp shr 63) or 1
  else
    Result := 0
end;

function av_q2d(a: TAVRational): double;
begin
  Result := a.num / a.den;
end;

end.
