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

(*
 * This is a part of Pascal porting of ffmpeg.
 * - Originally by Victor Zinetz for Delphi and Free Pascal on Windows.
 * - For Mac OS X, some modifications were made by The Creative CAT, denoted as CAT
 *   in the source codes.
 * - Changes and updates by the UltraStar Deluxe Team
 *
 * Conversion of libavcodec/opt.h
 * revision 16912, Sun Feb 1 02:00:19 2009 UTC 
 *
 * update, MiSchi, no code change
 * Fri Jun 12 2009 21:50:00 UTC
 *
 * update to
 * Max. avcodec version: 52.45.0, Mon Jan 4 2010 19:20:00 CET 
 * MiSchi
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
    name: {const} PAnsiChar;
    
    (**
     * short English help text
     * @todo What about other languages?
     *)
    help: {const} PAnsiChar;

    (**
     * The offset relative to the context structure where the option
     * value is stored. It should be 0 for named constants.
     *)
    offset: cint;
    type_: TAVOptionType;

    (**
     * the default value for scalar options
     *)
    default_val: cdouble;
    min: cdouble;                ///< minimum valid value for the option
    max: cdouble;                ///< maximum valid value for the option

    flags: cint;
//FIXME think about enc-audio, ... style flags

    (**
     * The logical unit to which the option belongs. Non-constant
     * options and corresponding named constants share the same
     * unit. May be NULL.
     *)
    unit_: {const} PAnsiChar;
  end;

{$IF LIBAVCODEC_VERSION >= 52042000} // >= 52.42.0
(**
 * AVOption2.
 * THIS IS NOT PART OF THE API/ABI YET!
 * This is identical to AVOption except that default_val was replaced by
 * an union, it should be compatible with AVOption on normal platforms.
 *)
type
  PAVOption2 = ^TAVOption2;
  TAVOption2 = record
    name   : {const} PAnsiChar;

    (**
     * short English help text
     * @todo What about other languages?
     *)
    help   : {const} PAnsiChar;

    (**
     * The offset relative to the context structure where the option
     * value is stored. It should be 0 for named constants.
     *)
    offset : cint;
    type_  : TAVOptionType;

    (**
     * the default value for scalar options
     *)
    default_val : record
      case cint of
        0 : (dbl: cdouble);
        1 : (str: PAnsiChar);
      end;
    min   : cdouble;
    max   : cdouble;
    flags : cint;
//FIXME think about enc-audio, ... style flags

    (**
     * The logical unit to which the option belongs. Non-constant
     * options and corresponding named constants share the same
     * unit. May be NULL.
     *)
    unit_: {const} PAnsiChar;
  end;
{$IFEND}

{$IF LIBAVCODEC_VERSION >= 51039000} // 51.39.0
(**
 * Looks for an option in obj. Looks only for the options which
 * have the flags set as specified in mask and flags (that is,
 * for which it is the case that opt->flags & mask == flags).
 *
 * @param[in] obj a pointer to a struct whose first element is a
 * pointer to an AVClass
 * @param[in] name the name of the option to look for
 * @param[in] unit the unit of the option to look for, or any if NULL
 * @return a pointer to the option found, or NULL if no option
 * has been found
 *)
function av_find_opt(obj: Pointer; {const} name: {const} PAnsiChar; {const} unit_: PAnsiChar; mask: cint; flags: cint): {const} PAVOption;
  cdecl; external av__codec;
{$IFEND}

{$IF LIBAVCODEC_VERSION_MAJOR < 53}

(**
 * @see av_set_string2()
 *)
function av_set_string(obj: pointer; name: {const} PAnsiChar; val: {const} PAnsiChar): {const} PAVOption;
  cdecl; external av__codec; deprecated;

{$IF LIBAVCODEC_VERSION >= 51059000} // 51.59.0
(**
 * @return a pointer to the AVOption corresponding to the field set or
 * NULL if no matching AVOption exists, or if the value val is not
 * valid
 * @see av_set_string3()
 *)
function av_set_string2(obj: Pointer; name: {const} PAnsiChar; val: {const} PAnsiChar; alloc: cint): {const} PAVOption;
  cdecl; external av__codec; deprecated;
{$IFEND}

{$IFEND}

{$IF LIBAVCODEC_VERSION >= 52007000} // 52.7.0
(**
 * Sets the field of obj with the given name to value.
 *
 * @param[in] obj A struct whose first element is a pointer to an
 * AVClass.
 * @param[in] name the name of the field to set
 * @param[in] val The value to set. If the field is not of a string
 * type, then the given string is parsed.
 * SI postfixes and some named scalars are supported.
 * If the field is of a numeric type, it has to be a numeric or named
 * scalar. Behavior with more than one scalar and +- infix operators
 * is undefined.
 * If the field is of a flags type, it has to be a sequence of numeric
 * scalars or named flags separated by '+' or '-'. Prefixing a flag
 * with '+' causes it to be set without affecting the other flags;
 * similarly, '-' unsets a flag.
 * @param[out] o_out if non-NULL put here a pointer to the AVOption
 * found
 * @param alloc when 1 then the old value will be av_freed() and the
 *                     new av_strduped()
 *              when 0 then no av_free() nor av_strdup() will be used
 * @return 0 if the value has been set, or an AVERROR code in case of
 * error:
 * AVERROR(ENOENT) if no matching option exists
 * AVERROR(ERANGE) if the value is out of range
 * AVERROR(EINVAL) if the value is not valid
 *)
function av_set_string3(obj: Pointer; name: {const} PAnsiChar; val: {const} PAnsiChar; alloc: cint; out o_out: {const} PAVOption): cint;
  cdecl; external av__codec;
{$IFEND}

function av_set_double(obj: pointer; name: {const} PAnsiChar; n: cdouble): PAVOption;
  cdecl; external av__codec;

function av_set_q(obj: pointer; name: {const} PAnsiChar; n: TAVRational): PAVOption;
  cdecl; external av__codec;

function av_set_int(obj: pointer; name: {const} PAnsiChar; n: cint64): PAVOption;
  cdecl; external av__codec;

function av_get_double(obj: pointer; name: {const} PAnsiChar; var o_out: PAVOption): cdouble;
  cdecl; external av__codec;

function av_get_q(obj: pointer; name: {const} PAnsiChar; var o_out: PAVOption): TAVRational;
  cdecl; external av__codec;

function av_get_int(obj: pointer; name: {const} PAnsiChar; var o_out: {const} PAVOption): cint64;
  cdecl; external av__codec;

function av_get_string(obj: pointer; name: {const} PAnsiChar; var o_out: {const} PAVOption; buf: PAnsiChar; buf_len: cint): PAnsiChar;
  cdecl; external av__codec;

function av_next_option(obj: pointer; last: {const} PAVOption): PAVOption;
  cdecl; external av__codec;

function av_opt_show(obj: pointer; av_log_obj: pointer): cint;
  cdecl; external av__codec;

procedure av_opt_set_defaults(s: pointer);
  cdecl; external av__codec;

{$IF LIBAVCODEC_VERSION >= 51039000} // 51.39.0
procedure av_opt_set_defaults2(s: Pointer; mask: cint; flags: cint);
  cdecl; external av__codec;
{$IFEND}

implementation

end.
