unit MATHEMATICS;

interface

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

uses
  windows, rational;

const
  av__util = 'avutil-49.dll';

type
  TAVRounding = (
    AV_ROUND_ZERO     = 0, ///< round toward zero
    AV_ROUND_INF      = 1, ///< round away from zero
    AV_ROUND_DOWN     = 2, ///< round toward -infinity
    AV_ROUND_UP       = 3, ///< round toward +infinity
    AV_ROUND_NEAR_INF = 5, ///< round to nearest and halfway cases away from zero
    AV_ROUND_FUCKING = $FFFFFF
  );

(** * rescale a 64bit integer with rounding to nearest.
 * a simple a*b/c isn't possible as it can overflow *)
function av_rescale (a, b, c: int64): int64;
  cdecl; external av__util;

(**
 * rescale a 64bit integer with specified rounding.
 * a simple a*b/c isn't possible as it can overflow *)
function av_rescale_rnd (a, b, c: int64; enum: TAVRounding): int64;
  cdecl; external av__util;

(**
 * rescale a 64bit integer by 2 rational numbers. *)
function av_rescale_q (a: int64; bq, cq: TAVRational): int64;
  cdecl; external av__util;

implementation

end.
 