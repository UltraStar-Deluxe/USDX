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
 * libswresample/swresample.h:
 * version: 0.5.0
 *
 *)


unit swresample;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

{$IFDEF DARWIN}
  {$linklib swresample}
{$ENDIF}

interface

uses
    ctypes,
    avutil,
    rational,
    {$IFDEF UNIX}
    BaseUnix,
    {$ENDIF}
    UConfig;

const
  SWR_CH_MAX = 16;  (* < Maximum number of channels *)
  SWR_FLAG_RESAMPLE = 1; (* < Force resampling even if equal sample rate *)

type
  PPSwrContext= ^PSwrContext;
  PSwrContext = ^TSwrContext;
  TSwrContext = record
  end;

(**
 * Allocate SwrContext.
 *
 * If you use this function you will need to set the parameters (manually or
 * with swr_alloc_set_opts()) before calling swr_init().
 *
 * @see swr_alloc_set_opts(), swr_init(), swr_free()
 * @return NULL on error, allocated context otherwise
 *)
function swr_alloc(): PSwrContext;
  cdecl; external sw__resample;

(**
 * Initialize context after user parameters have been set.
 *
 * @return AVERROR error code in case of failure.
 *)
function swr_init(s: PSwrContext): cint;
  cdecl; external sw__resample;

(**
 * Allocate SwrContext if needed and set/reset common parameters.
 *
 * This function does not require s to be allocated with swr_alloc(). On the
 * other hand, swr_alloc() can use swr_alloc_set_opts() to set the parameters
 * on the allocated context.
 *
 * @param s               Swr context, can be NULL
 * @param out_ch_layout   output channel layout (AV_CH_LAYOUT_* )
 * @param out_sample_fmt  output sample format (AV_SAMPLE_FMT_* ).
 * @param out_sample_rate output sample rate (frequency in Hz)
 * @param in_ch_layout    input channel layout (AV_CH_LAYOUT_* )
 * @param in_sample_fmt   input sample format (AV_SAMPLE_FMT_* ).
 * @param in_sample_rate  input sample rate (frequency in Hz)
 * @param log_offset      logging level offset
 * @param log_ctx         parent logging context, can be NULL
 *
 * @see swr_init(), swr_free()
 * @return NULL on error, allocated context otherwise
 *)
function swr_alloc_set_opts(s: PSwrContext;
                            out_ch_layout: cint64; out_sample_fmt: TAVSampleFormat; out_sample_rate: cint;
                            in_ch_layout:  cint64; in_sample_fmt:  TAVSampleFormat; in_sample_rate:  cint;
                            log_offset: cint; log_ctx: pointer): PSwrContext;
  cdecl; external sw__resample;

(**
 * Free the given SwrContext and set the pointer to NULL.
 *)
procedure swr_free(s: PPSwrContext);
  cdecl; external sw__resample;

(**
 * Convert audio.
 *
 * in and in_count can be set to 0 to flush the last few samples out at the
 * end.
 *
 * If more input is provided than output space then the input will be buffered.
 * You can avoid this buffering by providing more output space than input.
 * Convertion will run directly without copying whenever possible.
 *
 * @param s         allocated Swr context, with parameters set
 * @param out       output buffers, only the first one need be set in case of packed audio
 * @param out_count amount of space available for output in samples per channel
 * @param in        input buffers, only the first one need to be set in case of packed audio
 * @param in_count  number of input samples available in one channel
 *
 * @return number of samples output per channel, negative value on error
 *)
function swr_convert(s: PSwrContext; var out_: PByte; out_count: cint;
                             {const} var in_:  PByte; in_count:  cint): cint;
  cdecl; external sw__resample;

(**
 * Activate resampling compensation.
 *)
procedure swr_compensate(s: PSwrContext; sample_delta: cint; compensation_distance: cint);
  cdecl; external sw__resample;

(**
 * Set a customized input channel mapping.
 *
 * @param s           allocated Swr context, not yet initialized
 * @param channel_map customized input channel mapping (array of channel
 *                    indexes, -1 for a muted channel)
 * @return AVERROR error code in case of failure.
 *)
function swr_set_channel_mapping(s: PSwrContext; {const} channel_map: pcint): cint;
  cdecl; external sw__resample;

(**
 * @
 *)

implementation

end.

