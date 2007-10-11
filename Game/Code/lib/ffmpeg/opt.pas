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
(* This is a part of Pascal porting of ffmpeg.  Originally by Victor Zinetz for Delphi and Free Pascal on Windows.
For Mac OS X, some modifications were made by The Creative CAT, denoted as CAT
in the source codes *)

unit opt;
{$MODE DELPHI} (* CAT *)
{$PACKENUM 4}    (* every enum type variables uses 4 bytes, CAT *)
{$PACKRECORDS C}    (* GCC compatible, Record Packing, CAT *)

interface

uses
  rational;  (* CAT *)

type
  TAVOptionType = (
    FF_OPT_TYPE_FLAGS,
    FF_OPT_TYPE_INT,
    FF_OPT_TYPE_INT64,
    FF_OPT_TYPE_DOUBLE,
    FF_OPT_TYPE_FLOAT,
    FF_OPT_TYPE_STRING,
    FF_OPT_TYPE_RATIONAL,
    FF_OPT_TYPE_CONST = 128
  );

const
(* version numbers are changed by The Creative CAT *)
  av__codec = 'libavcodec.51';


  AV_OPT_FLAG_ENCODING_PARAM  = 1;   ///< a generic parameter which can be set by the user for muxing or encoding
  AV_OPT_FLAG_DECODING_PARAM  = 2;   ///< a generic parameter which can be set by the user for demuxing or decoding
  AV_OPT_FLAG_METADATA        = 4;   ///< some data extracted or inserted into the file like title, comment, ...
  AV_OPT_FLAG_AUDIO_PARAM     = 8;
  AV_OPT_FLAG_VIDEO_PARAM     = 16;
  AV_OPT_FLAG_SUBTITLE_PARAM  = 32;

type
  PAVOption = ^TAVOption;
  TAVOption = record
    name: pchar;
    help: pchar;
    offset: integer;             ///< offset to context structure where the parsed value should be stored
    _type: TAVOptionType;

    default_val: double;
    min: double;
    max: double;

    flags: integer;
//FIXME think about enc-audio, ... style flags
    _unit: pchar;
  end;


function av_set_string (obj: pointer; name: pchar; val: pchar): PAVOption;
  cdecl; external av__codec;

function av_set_double (obj: pointer; name: pchar; n: double): PAVOption;
  cdecl; external av__codec;

function av_set_q (obj: pointer; name: pchar; n: TAVRational): PAVOption;
  cdecl; external av__codec;

function av_set_int (obj: pointer; name: pchar; n: int64): PAVOption;
  cdecl; external av__codec;

function av_get_double (obj: pointer; name: pchar; o_out: PPointer): double;
  cdecl; external av__codec;

function av_get_q (obj: pointer; name: pchar; o_out: PPointer): TAVRational;
  cdecl; external av__codec;

function av_get_int (obj: pointer; name: pchar; o_out: PPointer): int64;
  cdecl; external av__codec;

function av_get_string (obj: pointer; name: pchar; o_out: PPOinter; buf: pchar; buf_len: integer): pchar;
  cdecl; external av__codec;

function av_next_option (obj: pointer; last: PAVOption): PAVOption;
  cdecl; external av__codec;

function av_opt_show (obj: pointer; av_log_obj: pointer): integer;
  cdecl; external av__codec;

procedure av_opt_set_defaults (s: pointer);
  cdecl; external av__codec;

implementation

end.
