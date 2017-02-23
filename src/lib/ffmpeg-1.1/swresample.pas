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
 * version: 0.17.102
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
  {$IF LIBSWRESAMPLE_VERSION_MAJOR <  1}
  SWR_CH_MAX = 32;  (* < Maximum number of channels *)
  {$ENDIF}
  SWR_FLAG_RESAMPLE = 1; (* < Force resampling even if equal sample rate *)

type
  TSwrDitherType = (
    SWR_DITHER_NONE = 0,
    SWR_DITHER_RECTANGULAR,
    SWR_DITHER_TRIANGULAR,
    SWR_DITHER_TRIANGULAR_HIGHPASS,
    SWR_DITHER_NB               (* < not part of API/ABI *)
  );

  TSwrEngine = (
    SWR_ENGINE_SWR,             (* < SW Resampler *)
    SWR_ENGINE_SOXR,            (* < SoX Resampler *)
    SWR_ENGINE_NB               (* < not part of API/ABI *)
  );

  TSwrFilterType = (
    SWR_FILTER_TYPE_CUBIC,              (* < Cubic *)
    SWR_FILTER_TYPE_BLACKMAN_NUTTALL,   (* < Blackman Nuttall Windowed Sinc *)
    SWR_FILTER_TYPE_KAISER              (* < Kaiser Windowed Sinc *)
  );

  PPSwrContext= ^PSwrContext;
  PSwrContext = ^TSwrContext;
  TSwrContext = record
  end;

(**
 * Get the AVClass for swrContext. It can be used in combination with
 * AV_OPT_SEARCH_FAKE_OBJ for examining options.
 *
 * @see av_opt_find().
 *)
function swr_get_class(): PAVClass;
  cdecl; external sw__resample;

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
 * Convert the next timestamp from input to output
 * timestamps are in 1/(in_sample_rate * out_sample_rate) units.
 *
 * @note There are 2 slightly differently behaving modes.
 *       First is when automatic timestamp compensation is not used, (min_compensation >= FLT_MAX)
 *              in this case timestamps will be passed through with delays compensated
 *       Second is when automatic timestamp compensation is used, (min_compensation < FLT_MAX)
 *              in this case the output timestamps will match output sample numbers
 *
 * @param pts   timestamp for the next input sample, INT64_MIN if unknown
 * @return the output timestamp for the next output sample
 *)
function swr_next_pts(s: PSwrContext; pts: cint64): cint64;
  cdecl; external sw__resample;

(**
 * Activate resampling compensation.
 *)
function swr_set_compensation(s: PSwrContext; sample_delta: cint; compensation_distance: cint): cint;
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
 * Set a customized remix matrix.
 *
 * @param s       allocated Swr context, not yet initialized
 * @param matrix  remix coefficients; matrix[i + stride * o] is
 *                the weight of input channel i in output channel o
 * @param stride  offset between lines of the matrix
 * @return  AVERROR error code in case of failure.
 *)
function swr_set_matrix(s: PSwrContext; {const} matrix: pcdouble; stride: cint): cint;
  cdecl; external sw__resample;

(**
 * Drops the specified number of output samples.
 *)
function swr_drop_output(s: PSwrContext; count: cint): cint;
  cdecl; external sw__resample;

(**
 * Injects the specified number of silence samples.
 *)
function swr_inject_silence(s: PSwrContext; count: cint): cint;
  cdecl; external sw__resample;

(**
 * Gets the delay the next input sample will experience relative to the next output sample.
 *
 * Swresample can buffer data if more input has been provided than available
 * output space, also converting between sample rates needs a delay.
 * This function returns the sum of all such delays.
 * The exact delay is not necessarily an integer value in either input or
 * output sample rate. Especially when downsampling by a large value, the
 * output sample rate may be a poor choice to represent the delay, similarly
 * for upsampling and the input sample rate.
 *
 * @param s     swr context
 * @param base  timebase in which the returned delay will be
 *              if its set to 1 the returned delay is in seconds
 *              if its set to 1000 the returned delay is in milli seconds
 *              if its set to the input sample rate then the returned delay is in input samples
 *              if its set to the output sample rate then the returned delay is in output samples
 *              an exact rounding free delay can be found by using LCM(in_sample_rate, out_sample_rate)
 * @returns     the delay in 1/base units.
 *)
function swr_get_delay(s: PSwrContext; base: cint64): cint64;
  cdecl; external sw__resample;

(**
 * Return the LIBSWRESAMPLE_VERSION_INT constant.
 *)
function swresample_version(): cuint;
  cdecl; external sw__resample;

(**
 * Return the swr build-time configuration.
 *)
function swresample_configuration(): cuchar;
  cdecl; external sw__resample;

(**
 * Return the swr license.
 *)
function swresample_license(): cuchar;
  cdecl; external sw__resample;

(**
 * @
 *)

implementation

end.

