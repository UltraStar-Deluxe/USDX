(*
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *
 * This is a part of the Pascal port of ffmpeg.
 * - Changes and updates by the UltraStar Deluxe Team
 *
 * Conversion of libavutil/audioconvert.h and libavcodec/audioconvert.h
 * avutil version 51.73.101; avcodec version 54.59.100
 *
 *)

(**
 * @file
 * audio conversion routines
 *)

const
  {* Audio channel masks *}
  AV_CH_FRONT_LEFT             = $00000001;
  AV_CH_FRONT_RIGHT            = $00000002;
  AV_CH_FRONT_CENTER           = $00000004;
  AV_CH_LOW_FREQUENCY          = $00000008;
  AV_CH_BACK_LEFT              = $00000010;
  AV_CH_BACK_RIGHT             = $00000020;
  AV_CH_FRONT_LEFT_OF_CENTER   = $00000040;
  AV_CH_FRONT_RIGHT_OF_CENTER  = $00000080;
  AV_CH_BACK_CENTER            = $00000100;
  AV_CH_SIDE_LEFT              = $00000200;
  AV_CH_SIDE_RIGHT             = $00000400;
  AV_CH_TOP_CENTER             = $00000800;
  AV_CH_TOP_FRONT_LEFT         = $00001000;
  AV_CH_TOP_FRONT_CENTER       = $00002000;
  AV_CH_TOP_FRONT_RIGHT        = $00004000;
  AV_CH_TOP_BACK_LEFT          = $00008000;
  AV_CH_TOP_BACK_CENTER        = $00010000;
  AV_CH_TOP_BACK_RIGHT         = $00020000;
  AV_CH_STEREO_LEFT            = $20000000;  ///< Stereo downmix.
  AV_CH_STEREO_RIGHT           = $40000000;  ///< See AV_CH_STEREO_LEFT.
  AV_CH_WIDE_LEFT: cuint64             = $0000000080000000;
  AV_CH_WIDE_RIGHT: cuint64            = $0000000100000000;
  AV_CH_SURROUND_DIRECT_LEFT: cuint64  = $0000000200000000;
  AV_CH_SURROUND_DIRECT_RIGHT: cuint64 = $0000000400000000;
  AV_CH_LOW_FREQUENCY_2: cuint64       = $0000000800000000;

(** Channel mask value used for AVCodecContext.request_channel_layout
 *  to indicate that the user requests the channel order of the decoder output
 *  to be the native codec channel order.
 *)
  AV_CH_LAYOUT_NATIVE          = $8000000000000000;

(**
 * @}
 * @defgroup channel_mask_c Audio channel convenience macros
 * @{
 * *)
  AV_CH_LAYOUT_MONO            = (AV_CH_FRONT_CENTER);
  AV_CH_LAYOUT_STEREO          = (AV_CH_FRONT_LEFT or AV_CH_FRONT_RIGHT);
  AV_CH_LAYOUT_2POINT1         = (AV_CH_LAYOUT_STEREO or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_2_1             = (AV_CH_LAYOUT_STEREO or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_SURROUND        = (AV_CH_LAYOUT_STEREO or AV_CH_FRONT_CENTER);
  AV_CH_LAYOUT_3POINT1         = (AV_CH_LAYOUT_SURROUND or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_4POINT0         = (AV_CH_LAYOUT_SURROUND or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_4POINT1         = (AV_CH_LAYOUT_4POINT0 or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_2_2             = (AV_CH_LAYOUT_STEREO or AV_CH_SIDE_LEFT or AV_CH_SIDE_RIGHT);
  AV_CH_LAYOUT_QUAD            = (AV_CH_LAYOUT_STEREO or AV_CH_BACK_LEFT or AV_CH_BACK_RIGHT);
  AV_CH_LAYOUT_5POINT0         = (AV_CH_LAYOUT_SURROUND or AV_CH_SIDE_LEFT or AV_CH_SIDE_RIGHT);
  AV_CH_LAYOUT_5POINT1         = (AV_CH_LAYOUT_5POINT0 or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_5POINT0_BACK    = (AV_CH_LAYOUT_SURROUND or AV_CH_BACK_LEFT or 
                                  AV_CH_BACK_RIGHT);
  AV_CH_LAYOUT_5POINT1_BACK    = (AV_CH_LAYOUT_5POINT0_BACK or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_6POINT0         = (AV_CH_LAYOUT_5POINT0 or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_6POINT0_FRONT   = (AV_CH_LAYOUT_2_2 or AV_CH_FRONT_LEFT_OF_CENTER or 
                                  AV_CH_FRONT_RIGHT_OF_CENTER);
  AV_CH_LAYOUT_HEXAGONAL       = (AV_CH_LAYOUT_5POINT0_BACK or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_6POINT1         = (AV_CH_LAYOUT_5POINT1 or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_6POINT1_BACK    = (AV_CH_LAYOUT_5POINT1_BACK or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_6POINT1_FRONT   = (AV_CH_LAYOUT_6POINT0_FRONT or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_7POINT0         = (AV_CH_LAYOUT_5POINT0 or AV_CH_BACK_LEFT or AV_CH_BACK_RIGHT);
  AV_CH_LAYOUT_7POINT0_FRONT   = (AV_CH_LAYOUT_5POINT0 or AV_CH_FRONT_LEFT_OF_CENTER or 
                                  AV_CH_FRONT_RIGHT_OF_CENTER);
  AV_CH_LAYOUT_7POINT1         = (AV_CH_LAYOUT_5POINT1 or AV_CH_BACK_LEFT or AV_CH_BACK_RIGHT);
  AV_CH_LAYOUT_7POINT1_WIDE    = (AV_CH_LAYOUT_5POINT1 or AV_CH_FRONT_LEFT_OF_CENTER or 
                                  AV_CH_FRONT_RIGHT_OF_CENTER);
  AV_CH_LAYOUT_OCTAGONAL       = (AV_CH_LAYOUT_5POINT0 or AV_CH_BACK_LEFT or AV_CH_BACK_CENTER or 
                                  AV_CH_BACK_RIGHT);
  AV_CH_LAYOUT_STEREO_DOWNMIX  = (AV_CH_STEREO_LEFT or AV_CH_STEREO_RIGHT);

type
  AVMatrixEncoding = (
    AV_MATRIX_ENCODING_NONE,
    AV_MATRIX_ENCODING_DOLBY,
    AV_MATRIX_ENCODING_DPLII,
    AV_MATRIX_ENCODING_NB
    );

(**
 * Return a channel layout id that matches name, or 0 if no match is found.
 *
 * name can be one or several of the following notations,
 * separated by '+' or '|':
 * - the name of an usual channel layout (mono, stereo, 4.0, quad, 5.0,
 *   5.0(side), 5.1, 5.1(side), 7.1, 7.1(wide), downmix);
 * - the name of a single channel (FL, FR, FC, LFE, BL, BR, FLC, FRC, BC,
 *   SL, SR, TC, TFL, TFC, TFR, TBL, TBC, TBR, DL, DR);
 * - a number of channels, in decimal, optionally followed by 'c', yielding
 *   the default channel layout for that number of channels (@see
 *   av_get_default_channel_layout);
 * - a channel layout mask, in hexadecimal starting with "0x" (see the
 *   AV_CH_* macros).
 *
 * Example: "stereo+FC" = "2+FC" = "2c+1c" = "0x7"
 *)
function av_get_channel_layout(name: {const} PAnsiChar): cuint64;
  cdecl; external av__util;

(**
 * Return a description of a channel layout.
 * If nb_channels is <= 0, it is guessed from the channel_layout.
 *
 * @param buf put here the string containing the channel layout
 * @param buf_size size in bytes of the buffer
 *)
procedure av_get_channel_layout_string(buf: PAnsiChar; buf_size: cint; nb_channels: cint; channel_layout: cuint64);
  cdecl; external av__util;

type
  PAVBPrint = ^TAVBPrint;
  TAVBPrint = record
  end;
(**
 * Append a description of a channel layout to a bprint buffer.
 *)
procedure av_bprint_channel_layout(bp: PAVBPrint; nb_channels: cint; channel_layout: cuint64);
  cdecl; external av__util;

(**
 * Return the number of channels in the channel layout.
 *)
function av_get_channel_layout_nb_channels(channel_layout: cuint64): cint;
  cdecl; external av__util;

(**
 * Return default channel layout for a given number of channels.
 *)
function av_get_default_channel_layout(nb_channels: cint): cint64;
  cdecl; external av__util;

(**
 * Get the index of a channel in channel_layout.
 *
 * @param channel a channel layout describing exactly one channel which must be
 *                present in channel_layout.
 *
 * @return index of channel in channel_layout on success, a negative AVERROR
 *         on error.
 *)
function av_get_channel_layout_channel_index(channel_layout: cuint64;
                                        channel: cuint64): cint;
  cdecl; external av__util;

(**
 * Get the channel with the given index in channel_layout.
 *)
function av_channel_layout_extract_channel(channel_layout: cuint64; index: cint): cuint64;
  cdecl; external av__util;

(**
 * Get the name of a given channel.
 *
 * @return channel name on success, NULL on error.
 *)
function av_get_channel_name(uint64_t channel: cuint64): PAnsiChar;
  cdecl; external av__util;

(**
 * Get the description of a given channel.
 *
 * @param channel  a channel layout with a single channel
 * @return  channel description on success, NULL on error
 *)
function av_get_channel_description(uint64_t channel: cuint64): PAnsiChar;
  cdecl; external av__util;

(**
 * Get the value and name of a standard channel layout.
 *
 * @param[in]  index   index in an internal list, starting at 0
 * @param[out] layout  channel layout mask
 * @param[out] name    name of the layout
 * @return  0  if the layout exists,
 *          <0 if index is beyond the limits
 *)
function av_get_standard_channel_layout(index: cuint; layout: Pcuint64;
                                        name: {const} PPAnsiChar): cint;
  cdecl; external av__util;

(** avcodec from here **)

type
  TAVAudioConvert = record
  end;

(**
 * Create an audio sample format converter context
 * @param out_fmt Output sample format
 * @param out_channels Number of output channels
 * @param in_fmt Input sample format
 * @param in_channels Number of input channels
 * @param[in] matrix Channel mixing matrix (of dimension in_channel*out_channels). Set to NULL to ignore.
 * @param flags See AV_CPU_FLAG_xx
 * @return NULL on error
 *)
AVAudioConvert *av_audio_convert_alloc(out_fmt: TAVSampleFormat; out_channels: cint;
                                       in_fmt:  TAVSampleFormat; in_channels:  cint;
                                       matrix: {const} Pcfloat; flags: cint);
  cdecl; external av__codec;

(**
 * Free audio sample format converter context
 *)
procedure av_audio_convert_free(ctx: PAVAudioConvert);
  cdecl; external av__codec;

type
  P6 = array [1..6] of pointer;
  I6 = array [1..6] of cint;
(**
 * Convert between audio sample formats
 * @param[in] out array of output buffers for each channel. set to NULL to ignore processing of the given channel.
 * @param[in] out_stride distance between consecutive output samples (measured in bytes)
 * @param[in] in array of input buffers for each channel
 * @param[in] in_stride distance between consecutive input samples (measured in bytes)
 * @param len length of audio frame size (measured in samples)
 *)
int av_audio_convert(ctx: PAVAudioConvert;
                     out[6]: {const} P6; out_stride[6]: {const} I6;
                      in[6]: {const} P6;  in_stride[6]: {const} I6; len: cint);
  cdecl; external av__codec;
