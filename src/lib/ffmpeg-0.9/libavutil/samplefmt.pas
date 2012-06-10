(*
 * SampleFormat
 * copyright (c) 2011 Karl-Michael Schindler <karl-michael.schindler@web.de>
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
 * This is a part of the Pascal port of ffmpeg.
 *
 * Conversion of libavutil/samplefmt.h
 * avutil version 51.32.0
 *
 *)

type
(**
 * all in native-endian format
 *)
  TAVSampleFormat = (
    AV_SAMPLE_FMT_NONE = -1,
    AV_SAMPLE_FMT_U8,          ///< unsigned 8 bits
    AV_SAMPLE_FMT_S16,         ///< signed 16 bits
    AV_SAMPLE_FMT_S32,         ///< signed 32 bits
    AV_SAMPLE_FMT_FLT,         ///< float
    AV_SAMPLE_FMT_DBL,         ///< double

    AV_SAMPLE_FMT_U8P,         ///< unsigned 8 bits, planar
    AV_SAMPLE_FMT_S16P,        ///< signed 16 bits, planar
    AV_SAMPLE_FMT_S32P,        ///< signed 32 bits, planar
    AV_SAMPLE_FMT_FLTP,        ///< float, planar
    AV_SAMPLE_FMT_DBLP,        ///< double, planar

    AV_SAMPLE_FMT_NB           ///< Number of sample formats. DO NOT USE if linking dynamically
  );
  TAVSampleFormatArray = array [0 .. MaxInt div SizeOf(TAVSampleFormat) - 1] of TAVSampleFormat;
  PAVSampleFormatArray = ^TAVSampleFormatArray;

(**
 * Return the name of sample_fmt, or NULL if sample_fmt is not
 * recognized.
 *)
function av_get_sample_fmt_name(sample_fmt: TAVSampleFormat): {const} PAnsiChar;
  cdecl; external av__util;

(**
 * Return a sample format corresponding to name, or AV_SAMPLE_FMT_NONE
 * on error.
 *)
function av_get_sample_fmt(name: {const} PAnsiChar): TAVSampleFormat;
  cdecl; external av__util;

(**
 * Return the planar<->packed alternative form of the given sample format, or
 * AV_SAMPLE_FMT_NONE on error. If the passed sample_fmt is already in the
 * requested planar/packed format, the format returned is the same as the
 * input.
 *)
function av_get_alt_sample_fmt(sample_fmt: TAVSampleFormat; planar: cint): TAVSampleFormat;
  cdecl; external av__util;

(**
 * Generate a string corresponding to the sample format with
 * sample_fmt, or a header if sample_fmt is negative.
 *
 * @param buf the buffer where to write the string
 * @param buf_size the size of buf
 * @param sample_fmt the number of the sample format to print the
 * corresponding info string, or a negative value to print the
 * corresponding header.
 * @return the pointer to the filled buffer or NULL if sample_fmt is
 * unknown or in case of other errors
 *)
function av_get_sample_fmt_string(buf: PAnsiChar; buf_size: cint; sample_fmt: TAVSampleFormat): PAnsiChar;
  cdecl; external av__util;

{$IFDEF FF_API_GET_BITS_PER_SAMPLE_FMT}
(**
 * @deprecated Use av_get_bytes_per_sample() instead.
 *)
function av_get_bits_per_sample_fmt(sample_fmt: TAVSampleFormat): cint; deprecated;
  cdecl; external av__util;
{$ENDIF}

(**
 * Return number of bytes per sample.
 *
 * @param sample_fmt the sample format
 * @return number of bytes per sample or zero if unknown for the given
 * sample format
 *)
function av_get_bytes_per_sample(sample_fmt: TAVSampleFormat): cint;
  cdecl; external av__util;

type
  OctArrayOfPcuint8 = array[0..7] of Pcuint8;
  OctArrayOfcint    = array[0..7] of cint;

(**
 * Check if the sample format is planar.
 *
 * @param sample_fmt the sample format to inspect
 * @return 1 if the sample format is planar, 0 if it is interleaved
 *)
function av_sample_fmt_is_planar(sample_fmt: TAVSampleFormat): cint;
  cdecl; external av__util;

(**
 * Get the required buffer size for the given audio parameters.
 *
 * @param[out] linesize calculated linesize, may be NULL
 * @param nb_channels   the number of channels
 * @param nb_samples    the number of samples in a single channel
 * @param sample_fmt    the sample format
 * @return              required buffer size, or negative error code on failure
 *)
function av_samples_get_buffer_size(linesize: Pcint; nb_channels: cint; nb_samples: cint;
                                    sample_fmt: TAVSampleFormat; align: cint): cint;
  cdecl; external av__util;

(**
 * Fill channel data pointers and linesizes for samples with sample
 * format sample_fmt.
 *
 * The pointers array is filled with the pointers to the samples data:
 * for planar, set the start point of each channel's data within the buffer,
 * for packed, set the start point of the entire buffer only.
 *
 * The linesize array is filled with the aligned size of each channel's data
 * buffer for planar layout, or the aligned size of the buffer for all channels
 * for packed layout.
 *
 * @param[out] audio_data  array to be filled with the pointer for each channel
 * @param[out] linesize    calculated linesize
 * @param buf              the pointer to a buffer containing the samples
 * @param nb_channels      the number of channels
 * @param nb_samples       the number of samples in a single channel
 * @param sample_fmt       the sample format
 * @param align            buffer size alignment (1 = no alignment required)
 * @return                 0 on success or a negative error code on failure
 *)
function av_samples_fill_arrays(audio_data: pointer; linesize: Pcint;
                                buf: Pcuint8; nb_channels: cint; nb_samples: cint; 
                                sample_fmt: TAVSampleFormat; align: cint): cint;
  cdecl; external av__util;

(**
 * Allocate a samples buffer for nb_samples samples, and fill data pointers and
 * linesize accordingly.
 * The allocated samples buffer can be freed by using av_freep(&audio_data[0])
 *
 * @param[out] audio_data  array to be filled with the pointer for each channel
 * @param[out] linesize    aligned size for audio buffer(s)
 * @param nb_channels      number of audio channels
 * @param nb_samples       number of samples per channel
 * @param align            buffer size alignment (1 = no alignment required)
 * @return                 0 on success or a negative error code on failure
 * @see av_samples_fill_arrays()
 *)
function av_samples_alloc(audio_data: pointer; linesize: Pcint;
                          nb_channels: cint; nb_samples: cint;
			  sample_fmt: TAVSampleFormat; align: cint): cint;
  cdecl; external av__util;
