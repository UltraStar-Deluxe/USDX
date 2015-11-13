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
 * Conversion of
 *
 * libswresample/swresample.h:
 * version: 0.18.100
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
    rational,
    {$IFDEF UNIX}
    BaseUnix,
    {$ENDIF}
    UConfig;

const
  (*
   * IMPORTANT: The official FFmpeg C headers change very quickly. Often some
   * of the data structures are changed so that they become incompatible with
   * older header files. The Pascal headers have to be adjusted to those changes,
   * otherwise the application might crash randomly or strange bugs (not
   * necessarily related to video or audio due to buffer overflows etc.) might
   * occur.
   *
   * In the past users reported problems with USDX that took hours to fix and
   * the problem was an unsupported version of FFmpeg. So we decided to disable
   * support for future versions of FFmpeg until the headers are revised by us
   * for that version as they otherwise most probably will break USDX.
   *
   * If the headers do not yet support your FFmpeg version you may want to
   * adjust the max. version numbers manually but please note: it may work but
   * in many cases it does not. The USDX team does NOT PROVIDE ANY SUPPORT
   * for the game if the MAX. VERSION WAS CHANGED.
   *
   * The only safe way to support new versions of FFmpeg is to add the changes
   * of the FFmpeg git repository C headers to the Pascal headers.
   * You can accelerate this process by posting a patch with the git changes
   * translated to Pascal to our bug tracker (please join our IRC chat before
   * you start working on it). Simply adjusting the max. versions is NOT a valid
   * fix.
   *)

  (* Supported version by this header *)
  LIBSWRESAMPLE_MAX_VERSION_MAJOR   = 0;
  LIBSWRESAMPLE_MAX_VERSION_MINOR   = 18;
  LIBSWRESAMPLE_MAX_VERSION_RELEASE = 100;
  LIBSWRESAMPLE_MAX_VERSION = (LIBSWRESAMPLE_MAX_VERSION_MAJOR * VERSION_MAJOR) +
                           (LIBSWRESAMPLE_MAX_VERSION_MINOR * VERSION_MINOR) +
                           (LIBSWRESAMPLE_VERSION_RELEASE * VERSION_RELEASE);

  (* Min. supported version by this header *)
  LIBSWRESAMPLE_MIN_VERSION_MAJOR   = 0;
  LIBSWRESAMPLE_MIN_VERSION_MINOR   = 18;
  LIBSWRESAMPLE_MIN_VERSION_RELEASE = 100;
  LIBSWRESAMPLE_MIN_VERSION = (LIBSWRESAMPLE_MIN_VERSION_MAJOR * VERSION_MAJOR) +
                            (LIBSWRESAMPLE_MIN_VERSION_MINOR * VERSION_MINOR) +
                            (LIBSWRESAMPLE_MIN_VERSION_RELEASE * VERSION_RELEASE);

(* Check if linked versions are supported *)
{$IF (LIBSWRESAMPLE_VERSION < LIBSWRESAMPLE_MIN_VERSION)}
  {$MESSAGE Error 'Linked version of libswresample is too old!'}
{$IFEND}

(* Check if linked version is supported *)
{$IF (LIBSWRESAMPLE_VERSION > LIBSWRESAMPLE_MAX_VERSION)}
  {$MESSAGE Error 'Linked version of libswresample is not yet supported!'}
{$IFEND}
  
{$IF LIBRESAMPLE_VERSION_MAJOR < 1}
  SWR_CH_MAX = 32;  (* < Maximum number of channels *)
{$ENDIF}
  SWR_FLAG_RESAMPLE = 1; (* < Force resampling even if equal sample rate *)

type
(** Dithering algorithms *)
  TSwrDitherType = (
    SWR_DITHER_NONE = 0,
    SWR_DITHER_RECTANGULAR,
    SWR_DITHER_TRIANGULAR,
    SWR_DITHER_TRIANGULAR_HIGHPASS,

    SWR_DITHER_NS = 64,         (* < not part of API/ABI *)
    SWR_DITHER_NS_LIPSHITZ,
    SWR_DITHER_NS_F_WEIGHTED,
    SWR_DITHER_NS_MODIFIED_E_WEIGHTED,
    SWR_DITHER_NS_IMPROVED_E_WEIGHTED,
    SWR_DITHER_NS_SHIBATA,
    SWR_DITHER_NS_LOW_SHIBATA,
    SWR_DITHER_NS_HIGH_SHIBATA,
    SWR_DITHER_NB               (* < not part of API/ABI *)
  );

(** Resampling Engines *)
  TSwrEngine = (
    SWR_ENGINE_SWR,             (* < SW Resampler *)
    SWR_ENGINE_SOXR,            (* < SoX Resampler *)
    SWR_ENGINE_NB               (* < not part of API/ABI *)
  );

(** Resampling Filter Types *)
  TSwrFilterType = (
    SWR_FILTER_TYPE_CUBIC,              (* < Cubic *)
    SWR_FILTER_TYPE_BLACKMAN_NUTTALL,   (* < Blackman Nuttall Windowed Sinc *)
    SWR_FILTER_TYPE_KAISER              (* < Kaiser Windowed Sinc *)
  );

(**
 * The libswresample context. Unlike libavcodec and libavformat, this structure
 * is opaque. This means that if you would like to set options, you must use
 * the @ref avoptions API and cannot directly set values to members of the
 * structure.
 *)
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
  cdecl; external swresample;

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
  cdecl; external swresample;

(**
 * Initialize context after user parameters have been set.
 *
 * @see av_opt_set_int()
 * @see av_opt_set_dict()
 *
 * @param[in,out]   s Swr context to initialize
 * @return AVERROR error code in case of failure.
 *)
function swr_init(s: PSwrContext): cint;
  cdecl; external swresample;

(**
 * Check whether an swr context has been initialized or not.
 *
 * @param[in]       s Swr context to check
 * @see swr_init()
 * @return positive if it has been initialized, 0 if not initialized
 *)
function swr_is_initialized(s: PSwrContext): cint;
  cdecl; external swresample;
  
(**
 * Allocate SwrContext if needed and set/reset common parameters.
 *
 * This function does not require s to be allocated with swr_alloc(). On the
 * other hand, swr_alloc() can use swr_alloc_set_opts() to set the parameters
 * on the allocated context.
 *
 * @param s               existing Swr context if available, or NULL if not
 * @param out_ch_layout   output channel layout (AV_CH_LAYOUT_*)
 * @param out_sample_fmt  output sample format (AV_SAMPLE_FMT_*).
 * @param out_sample_rate output sample rate (frequency in Hz)
 * @param in_ch_layout    input channel layout (AV_CH_LAYOUT_*)
 * @param in_sample_fmt   input sample format (AV_SAMPLE_FMT_*).
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
  cdecl; external swresample;

(**
 * Free the given SwrContext and set the pointer to NULL.
 *
 * @param[in] s a pointer to a pointer to Swr context
 *)
procedure swr_free(s: PPSwrContext);
  cdecl; external swresample;

(**
 * Closes the context so that swr_is_initialized() returns 0.
 *
 * The context can be brought back to life by running swr_init(),
 * swr_init() can also be used without swr_close().
 * This function is mainly provided for simplifying the usecase
 * where one tries to support libavresample and libswresample.
 *
 * @param[in,out] s Swr context to be closed
 *)
procedure swr_close(s: PSwrContext);
  cdecl; external swresample;

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
                     var in_: {const} PByte; in_count: cint): cint;
  cdecl; external swresample;

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
  cdecl; external swresample;

(**
 * Activate resampling compensation ("soft" compensation). This function is
 * internally called when needed in swr_next_pts().
 *
 * @param[in,out] s             allocated Swr context. If it is not initialized,
 *                              or SWR_FLAG_RESAMPLE is not set, swr_init() is
 *                              called with the flag set.
 * @param[in]     sample_delta  delta in PTS per sample
 * @param[in]     compensation_distance number of samples to compensate for
 * @return    >= 0 on success, AVERROR error codes if:
 *            @li @c s is NULL,
 *            @li @c compensation_distance is less than 0,
 *            @li @c compensation_distance is 0 but sample_delta is not,
 *            @li compensation unsupported by resampler, or
 *            @li swr_init() fails when called.
 *)
function swr_set_compensation(s: PSwrContext; sample_delta: cint; compensation_distance: cint): cint;
  cdecl; external swresample;

(**
 * Set a customized input channel mapping.
 *
 * @param[in,out] s           allocated Swr context, not yet initialized
 * @param[in]     channel_map customized input channel mapping (array of channel
 *                            indexes, -1 for a muted channel)
 * @return >= 0 on success, or AVERROR error code in case of failure.
 *)
function swr_set_channel_mapping(s: PSwrContext; {const} channel_map: pcint): cint;
  cdecl; external swresample;

(**
 * Set a customized remix matrix.
 *
 * @param s       allocated Swr context, not yet initialized
 * @param matrix  remix coefficients; matrix[i + stride * o] is
 *                the weight of input channel i in output channel o
 * @param stride  offset between lines of the matrix
 * @return  >= 0 on success, or AVERROR error code in case of failure.
 *)
function swr_set_matrix(s: PSwrContext; {const} matrix: pcdouble; stride: cint): cint;
  cdecl; external swresample;

(**
 * Drops the specified number of output samples.
 *
 * This function, along with swr_inject_silence(), is called by swr_next_pts()
 * if needed for "hard" compensation.
 *
 * @param s     allocated Swr context
 * @param count number of samples to be dropped
 *
 * @return >= 0 on success, or a negative AVERROR code on failure
 *)
function swr_drop_output(s: PSwrContext; count: cint): cint;
  cdecl; external swresample;

(**
 * Injects the specified number of silence samples.
 *
 * This function, along with swr_drop_output(), is called by swr_next_pts()
 * if needed for "hard" compensation.
 *
 * @param s     allocated Swr context
 * @param count number of samples to be dropped
 *
 * @return >= 0 on success, or a negative AVERROR code on failure
 *)
function swr_inject_silence(s: PSwrContext; count: cint): cint;
  cdecl; external swresample;

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
 * @param base  timebase in which the returned delay will be:
 *              @li if it's set to 1 the returned delay is in seconds
 *              @li if it's set to 1000 the returned delay is in milliseconds
 *              @li if it's set to the input sample rate then the returned
 *                  delay is in input samples
 *              @li if it's set to the output sample rate then the returned
 *                  delay is in output samples
 *              @li if it's the least common multiple of in_sample_rate and
 *                  out_sample_rate then an exact rounding-free delay will be
 *                  returned
 * @returns     the delay in 1 / @c base units.
 *)
function swr_get_delay(s: PSwrContext; base: cint64): cint64;
  cdecl; external swresample;

(**
 * Return the @ref LIBSWRESAMPLE_VERSION_INT constant.
 *
 * This is useful to check if the build-time libswresample has the same version
 * as the run-time one.
 *
 * @returns     the unsigned int-typed version
 *)
function swresample_version(): cuint;
  cdecl; external swresample;

(**
 * Return the swr build-time configuration.
 *
 * @returns     the build-time @c ./configure flags
 *)
function swresample_configuration(): PAnsiChar;
  cdecl; external swresample;

(**
 * Return the swr license.
 *
 * @returns     the license of libswresample, determined at build-time
 *)
function swresample_license(): PAnsiChar;
  cdecl; external swresample;

implementation

end.

