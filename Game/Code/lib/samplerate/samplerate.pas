{*
** Copyright (C) 2002-2004 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
*}

{*
** API documentation is available here:
** http://www.mega-nerd.com/SRC/api.html
*}

unit samplerate;

{$IFDEF FPC}
  {$IFNDEF win32}
  {$LINKLIB libsamplerate}
  {$ENDIF}
  {$PACKRECORDS C} (* GCC/Visual C/C++ compatible record packing *)
  {$MODE DELPHI}
{$ENDIF}

interface

const
{$IFDEF WIN32}
  LibName = 'libsamplerate.dll';
{$ENDIF}
{$IFDEF LINUX}
  LibName = 'samplerate';
{$ENDIF}
{$IFDEF MACOS}
  {LibName = 'unknown';}
{$ENDIF}

{ Opaque data type SRC_STATE. }
type
  PSRC_STATE = ^SRC_STATE;
  SRC_STATE = record
  end;

{ SRC_DATA is used to pass data to src_simple() and src_process().  }
type
  PSRC_DATA = ^SRC_DATA;
  SRC_DATA = record
    data_in: Pdouble;
    data_out: Pdouble;
    input_frames: longint;
    output_frames: longint;
    input_frames_used: longint;
    output_frames_gen: longint;
    end_of_input: integer;
    src_ratio: double;
  end;

{ SRC_CB_DATA is used with callback based API.  }
type
  SRC_CB_DATA = record
    frames: longint;
    data_in: Psingle;
  end;

type
  PPsingle = ^Psingle;

{*
** User supplied callback function type for use with src_callback_new()
** and src_callback_read(). First parameter is the same pointer that was
** passed into src_callback_new(). Second parameter is pointer to a
** pointer. The user supplied callback function must modify *data to
** point to the start of the user supplied float array. The user supplied
** function must return the number of frames that **data points to.
*}
src_callback_t = function (cb_data: pointer; data: PPsingle): longint; cdecl;

{*
** Standard initialisation function : return an anonymous pointer to the
** internal state of the converter. Choose a converter from the enums below.
** Error returned in *error.
*}
function src_new(converter_type: integer; channels: integer; error: Pinteger): PSRC_STATE; cdecl; external LibName;

{*
** Initilisation for callback based API : return an anonymous pointer to the
** internal state of the converter. Choose a converter from the enums below.
** The cb_data pointer can point to any data or be set to NULL. Whatever the
** value, when processing, user supplied function "func" gets called with
** cb_data as first parameter.
*}
function src_callback_new(func: src_callback_t; converter_type: integer; channels: integer;
            error: Pinteger; cb_data: pointer): PSRC_STATE; cdecl; external LibName;

{*
** Cleanup all internal allocations.
** Always returns NULL.
*}
function src_delete(state: PSRC_STATE): PSRC_STATE; cdecl; external LibName;

{*
** Standard processing function.
** Returns non zero on error.
*}
function src_process(state: PSRC_STATE; data: PSRC_DATA): integer; cdecl; external LibName;

{*
** Callback based processing function. Read up to frames worth of data from
** the converter int *data and return frames read or -1 on error.
*}
function src_callback_read(state: PSRC_STATE; src_ratio: double;
            frames: longint; data: Psingle): longint; cdecl; external LibName;

{*
** Simple interface for performing a single conversion from input buffer to
** output buffer at a fixed conversion ratio.
** Simple interface does not require initialisation as it can only operate on
** a single buffer worth of audio.
*}
function src_simple(data: PSRC_DATA; converter_type: integer; channels: integer): integer; cdecl; external LibName;

{*
** This library contains a number of different sample rate converters,
** numbered 0 through N.
**
** Return a string giving either a name or a more full description of each
** sample rate converter or NULL if no sample rate converter exists for
** the given value. The converters are sequentially numbered from 0 to N.
*}
(* Const before type ignored *)
function src_get_name(converter_type: integer): Pchar; cdecl; external LibName;

(* Const before type ignored *)
function src_get_description(converter_type: integer): Pchar; cdecl; external LibName;

(* Const before type ignored *)
function src_get_version(): Pchar; cdecl; external LibName;

{*
** Set a new SRC ratio. This allows step responses
** in the conversion ratio.
** Returns non zero on error.
*}
function src_set_ratio(state: PSRC_STATE; new_ratio: double): integer; cdecl; external LibName;

{*
** Reset the internal SRC state.
** Does not modify the quality settings.
** Does not free any memory allocations.
** Returns non zero on error.
*}
function src_reset(state: PSRC_STATE): integer; cdecl; external LibName;

{*
** Return TRUE if ratio is a valid conversion ratio, FALSE
** otherwise.
*}
function src_is_valid_ratio(ratio: double): integer; cdecl; external LibName;

{*
** Return an error number.
*}
function src_error(state: PSRC_STATE): integer; cdecl; external LibName;

{*
** Convert the error number into a string.
*}
(* Const before type ignored *)
function src_strerror(error: integer): Pchar; cdecl; external LibName;

{*
** The following enums can be used to set the interpolator type
** using the function src_set_converter().
*}
type TConverterType = {enum}integer; const
{enum_begin TConverterType}
  SRC_SINC_BEST_QUALITY   = 0;
  SRC_SINC_MEDIUM_QUALITY = 1;
  SRC_SINC_FASTEST        = 2;
  SRC_ZERO_ORDER_HOLD     = 3;
  SRC_LINEAR              = 4;
{enum_end TConverterType}

{*
** Extra helper functions for converting from short to float and
** back again.
*}
(* Const before type ignored *)
procedure src_short_to_float_array(input: Psmallint; output: Psingle; len: integer); cdecl; external LibName;

(* Const before type ignored *)
procedure src_float_to_short_array(input: Psingle; output: Psmallint; len: integer); cdecl; external LibName;

implementation

end.
