(*
 * copyright (c) 2006 Michael Niedermayer <michaelni@gmx.at>
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
 * Conversions of
 *
 * libavutil/avutil.h:
 * version: 51.22.1
 *
 *)

unit avutil;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

{$IFDEF DARWIN}
  {$linklib libavutil}
{$ENDIF}

interface

uses
  ctypes,
  rational,
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  UConfig;

const
  (* Max. supported version by this header *)
  LIBAVUTIL_MAX_VERSION_MAJOR   = 51;
  LIBAVUTIL_MAX_VERSION_MINOR   = 22;
  LIBAVUTIL_MAX_VERSION_RELEASE = 2;
  LIBAVUTIL_MAX_VERSION = (LIBAVUTIL_MAX_VERSION_MAJOR * VERSION_MAJOR) +
                          (LIBAVUTIL_MAX_VERSION_MINOR * VERSION_MINOR) +
                          (LIBAVUTIL_MAX_VERSION_RELEASE * VERSION_RELEASE);

  (* Min. supported version by this header *)
  LIBAVUTIL_MIN_VERSION_MAJOR   = 51;
  LIBAVUTIL_MIN_VERSION_MINOR   = 9;
  LIBAVUTIL_MIN_VERSION_RELEASE = 1;
  LIBAVUTIL_MIN_VERSION = (LIBAVUTIL_MIN_VERSION_MAJOR * VERSION_MAJOR) +
                          (LIBAVUTIL_MIN_VERSION_MINOR * VERSION_MINOR) +
                          (LIBAVUTIL_MIN_VERSION_RELEASE * VERSION_RELEASE);

(* Check if linked versions are supported *)
{$IF (LIBAVUTIL_VERSION < LIBAVUTIL_MIN_VERSION)}
  {$MESSAGE Error 'Linked version of libavutil is too old!'}
{$IFEND}

{$IF (LIBAVUTIL_VERSION > LIBAVUTIL_MAX_VERSION)}
  {$MESSAGE Error 'Linked version of libavutil is not yet supported!'}
{$IFEND}

{$IFNDEF FPC}
type
  // defines for Delphi
  size_t = cardinal;
{$ENDIF}

{$INCLUDE libavutil/cpu.pas}

{$INCLUDE libavutil/dict.pas}

{$INCLUDE libavutil/error.pas}

{$INCLUDE libavutil/mathematics.pas}

{$INCLUDE libavutil/mem.pas}

{$INCLUDE libavutil/opt.pas}

{$INCLUDE libavutil/log.pas}

{$INCLUDE libavutil/pixfmt.pas}

{$INCLUDE libavutil/samplefmt.pas}

(* libavutil/common.h *) // until now MKTAG and MKBETAG is all from common.h KMS 19/5/2010

(**
 * MKTAG and MKBETAG are usually used to convert a magic string to an enumeration index.
 * In Pascal this can probably not be used and the functions could be removed.
 * KMS 8/6/2012
 *)
function MKTAG  (a, b, c, d: AnsiChar): integer; {$IFDEF HasInline}inline;{$ENDIF}
function MKBETAG(a, b, c, d: AnsiChar): integer; {$IFDEF HasInline}inline;{$ENDIF}

(**
 * Return the LIBAVUTIL_VERSION_INT constant.
 *)
function avutil_version(): cuint;
  cdecl; external av__util;

(**
 * Return the libavutil build-time configuration.
 *)
function avutil_configuration(): PAnsiChar;
  cdecl; external av__util;

(**
 * Return the libavutil license.
 *)
function avutil_license(): PAnsiChar;
  cdecl; external av__util;

type
  TAVMediaType = (
    AVMEDIA_TYPE_UNKNOWN = -1,
    AVMEDIA_TYPE_VIDEO,
    AVMEDIA_TYPE_AUDIO,
    AVMEDIA_TYPE_DATA,
    AVMEDIA_TYPE_SUBTITLE,
    AVMEDIA_TYPE_ATTACHMENT,
    AVMEDIA_TYPE_NB
  );

const
  FF_LAMBDA_SHIFT = 7;
  FF_LAMBDA_SCALE = (1<<FF_LAMBDA_SHIFT);
  FF_QP2LAMBDA    = 118; ///< factor to convert from H.263 QP to lambda
  FF_LAMBDA_MAX   = (256*128-1);
 
  FF_QUALITY_SCALE = FF_LAMBDA_SCALE; //FIXME maybe remove
 
  AV_NOPTS_VALUE   = $8000000000000000;
  AV_TIME_BASE     = 1000000;
  AV_TIME_BASE_Q   : TAVRational = (num: 1; den: AV_TIME_BASE);

type
  TAVPictureType = (
    AV_PICTURE_TYPE_I = 1, ///< Intra
    AV_PICTURE_TYPE_P,     ///< Predicted
    AV_PICTURE_TYPE_B,     ///< Bi-dir predicted
    AV_PICTURE_TYPE_S,     ///< S(GMC)-VOP MPEG4
    AV_PICTURE_TYPE_SI,    ///< Switching Intra
    AV_PICTURE_TYPE_SP,    ///< Switching Predicted
    AV_PICTURE_TYPE_BI     ///< BI type
  );

(**
 * Return a single letter to describe the given picture type
 * pict_type.
 *
 * @param[in] pict_type the picture type @return a single character
 * representing the picture type, '?' if pict_type is unknown
 *)
function av_get_picture_type_char(pict_type: TAVPictureType): Pchar;
  cdecl; external av__util;

implementation

(* libavutil/common.h *)

function MKTAG(a, b, c, d: AnsiChar): integer; {$IFDEF HasInline}inline;{$ENDIF}
begin
  Result := (ord(a) or (ord(b) shl 8) or (ord(c) shl 16) or (ord(d) shl 24));
end;

function MKBETAG(a, b, c, d: AnsiChar): integer; {$IFDEF HasInline}inline;{$ENDIF}
begin
  Result := (ord(d) or (ord(c) shl 8) or (ord(b) shl 16) or (ord(a) shl 24));
end;

end.
