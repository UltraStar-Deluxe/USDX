(*
 * AVOptions
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

(* This is a part of Pascal porting of ffmpeg.
 * Originally by Victor Zinetz for Delphi and Free Pascal on Windows.
 * For Mac OS X, some modifications were made by The Creative CAT, denoted as CAT
 * in the source codes *)

(*
 * Conversion of libavcodec/opt.h
 * revision 13669, Fri Jun 6 07:00:42 2008 UTC
 *)

unit opt;

{$IFDEF FPC}
  {$MODE DELPHI}
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

type
  TAVOptionType = (
    FF_OPT_TYPE_FLAGS,
    FF_OPT_TYPE_INT,
    FF_OPT_TYPE_INT64,
    FF_OPT_TYPE_DOUBLE,
    FF_OPT_TYPE_FLOAT,
    FF_OPT_TYPE_STRING,
    FF_OPT_TYPE_RATIONAL,
    FF_OPT_TYPE_BINARY,  ///< offset must point to a pointer immediately followed by an int for the length
    FF_OPT_TYPE_CONST = 128
  );

const
  AV_OPT_FLAG_ENCODING_PARAM  = 1;   ///< a generic parameter which can be set by the user for muxing or encoding
  AV_OPT_FLAG_DECODING_PARAM  = 2;   ///< a generic parameter which can be set by the user for demuxing or decoding
  AV_OPT_FLAG_METADATA        = 4;   ///< some data extracted or inserted into the file like title, comment, ...
  AV_OPT_FLAG_AUDIO_PARAM     = 8;
  AV_OPT_FLAG_VIDEO_PARAM     = 16;
  AV_OPT_FLAG_SUBTITLE_PARAM  = 32;

type
  (**
   * AVOption
   *)
  PAVOption = ^TAVOption;
  TAVOption = record
    name: {const} PChar;
    
    (**
     * short English help text
     * @todo What about other languages?
     *)
    help: {const} PChar;
    offset: cint;             ///< offset to context structure where the parsed value should be stored
    type_: TAVOptionType;

    default_val: cdouble;
    min: cdouble;
    max: cdouble;

    flags: cint;
//FIXME think about enc-audio, ... style flags
    unit_: {const} PChar;
  end;

{$IF LIBAVCODEC_VERSION >= 51039000} // 51.39.0
function av_find_opt (obj: Pointer; {const} name: {const} PChar; {const} unit_: PChar; mask: cint; flags: cint): {const} PAVOption;
  cdecl; external av__codec;
{$IFEND}

function av_set_string (obj: pointer; name: {const} pchar; val: {const} pchar): PAVOption;
  cdecl; external av__codec;

function av_set_double (obj: pointer; name: {const} pchar; n: cdouble): PAVOption;
  cdecl; external av__codec;

function av_set_q (obj: pointer; name: {const} pchar; n: TAVRational): PAVOption;
  cdecl; external av__codec;

function av_set_int (obj: pointer; name: {const} pchar; n: cint64): PAVOption;
  cdecl; external av__codec;

function av_get_double (obj: pointer; name: {const} pchar; var o_out: PAVOption): cdouble;
  cdecl; external av__codec;

function av_get_q (obj: pointer; name: {const} pchar; var o_out: PAVOption): TAVRational;
  cdecl; external av__codec;

function av_get_int (obj: pointer; name: {const} pchar; var o_out: {const} PAVOption): cint64;
  cdecl; external av__codec;

function av_get_string (obj: pointer; name: {const} pchar; var o_out: {const} PAVOption; buf: pchar; buf_len: cint): pchar;
  cdecl; external av__codec;

function av_next_option (obj: pointer; last: {const} PAVOption): PAVOption;
  cdecl; external av__codec;

function av_opt_show (obj: pointer; av_log_obj: pointer): cint;
  cdecl; external av__codec;

procedure av_opt_set_defaults (s: pointer);
  cdecl; external av__codec;

{$IF LIBAVCODEC_VERSION >= 51039000} // 51.39.0
procedure av_opt_set_defaults2 (s: Pointer; mask: cint; flags: cint);
  cdecl; external av__codec;
{$IFEND}

implementation

end.
