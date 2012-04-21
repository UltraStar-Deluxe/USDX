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
 * version: 51.34.101
 *
 * libavutil/mem.h:
 *
 * libavutil/log.h:
 *
 * include/keep pixfmt.h (change in revision 50.01.0)
 * Maybe, the pixelformats are not needed, but it has not been checked.
 * log.h is only partial.
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
  mathematics,
  opt,
  rational,
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  UConfig;

const
  (* Max. supported version by this header *)
  LIBAVUTIL_MAX_VERSION_MAJOR   = 51;
  LIBAVUTIL_MAX_VERSION_MINOR   = 34;
  LIBAVUTIL_MAX_VERSION_RELEASE = 101;
  LIBAVUTIL_MAX_VERSION = (LIBAVUTIL_MAX_VERSION_MAJOR * VERSION_MAJOR) +
                          (LIBAVUTIL_MAX_VERSION_MINOR * VERSION_MINOR) +
                          (LIBAVUTIL_MAX_VERSION_RELEASE * VERSION_RELEASE);

  (* Min. supported version by this header *)
  LIBAVUTIL_MIN_VERSION_MAJOR   = 51;
  LIBAVUTIL_MIN_VERSION_MINOR   = 34;
  LIBAVUTIL_MIN_VERSION_RELEASE = 101;
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

(**
 * @addtogroup lavu_media Media Type
 * @brief Media Type
 *)

type
  TAVMediaType = (
    AVMEDIA_TYPE_UNKNOWN = -1,  ///< Usually treated as AVMEDIA_TYPE_DATA
    AVMEDIA_TYPE_VIDEO,
    AVMEDIA_TYPE_AUDIO,
    AVMEDIA_TYPE_DATA,          ///< Opaque data information usually continuous
    AVMEDIA_TYPE_SUBTITLE,
    AVMEDIA_TYPE_ATTACHMENT,    ///< Opaque data information usually sparse
    AVMEDIA_TYPE_NB
  );

(**
 * Return a string describing the media_type enum, NULL if media_type
 * is unknown.
 *)
function av_get_media_type_string(media_type: PAVMediaType): Pchar;
  cdecl; external av__util;

const
  FF_LAMBDA_SHIFT = 7;
  FF_LAMBDA_SCALE = (1<<FF_LAMBDA_SHIFT);
  FF_QP2LAMBDA    = 118; ///< factor to convert from H.263 QP to lambda
  FF_LAMBDA_MAX   = (256*128-1);
 
  FF_QUALITY_SCALE = FF_LAMBDA_SCALE; //FIXME maybe remove
 
(**
 * @brief Undefined timestamp value
 *
 * Usually reported by demuxer that work on containers that do not provide
 * either pts or dts.
 *)

  AV_NOPTS_VALUE   = $8000000000000000;

(**
 * Internal time base represented as integer
 *)

  AV_TIME_BASE     = 1000000;
  
(**
 * Internal time base represented as fractional value
 *)

  AV_TIME_BASE_Q   : TAVRational = (num: 1; den: AV_TIME_BASE);

(**
 * @}
 * @}
 * @defgroup lavu_picture Image related
 *
 * AVPicture types, pixel formats and basic image planes manipulation.
 *
 * @{
 *)

type
  TAVPictureType = (
    AV_PICTURE_TYPE_NONE = 0, ///< Undefined
    AV_PICTURE_TYPE_I,     ///< Intra
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

(**
 * Return x default pointer in case p is NULL.
 *)
function av_x_if_null(p: {const} pointer; x: {const} pointer): pointer;

{$INCLUDE error.pas}

(* libavutil/pixfmt.h version: 50.43.0 *)

type
(**
 * Pixel format.
 *
 * @note
 * PIX_FMT_RGB32 is handled in an endian-specific manner. An RGBA
 * color is put together as:
 *  (A << 24) | (R << 16) | (G << 8) | B
 * This is stored as BGRA on little-endian CPU architectures and ARGB on
 * big-endian CPUs.
 *
 * @par
 * When the pixel format is palettized RGB (PIX_FMT_PAL8), the palettized
 * image data is stored in AVFrame.data[0]. The palette is transported in
 * AVFrame.data[1], is 1024 bytes long (256 4-byte entries) and is
 * formatted the same as in PIX_FMT_RGB32 described above (i.e., it is
 * also endian-specific). Note also that the individual RGB palette
 * components stored in AVFrame.data[1] should be in the range 0..255.
 * This is important as many custom PAL8 video codecs that were designed
 * to run on the IBM VGA graphics adapter use 6-bit palette components.
 *
 * @par
 * For all the 8bit per pixel formats, an RGB32 palette is in data[1] like
 * for pal8. This palette is filled in automatically by the function
 * allocating the picture.
 *
 * @note
 * make sure that all newly added big endian formats have pix_fmt&1==1
 * and that all newly added little endian formats have pix_fmt&1==0
 * this allows simpler detection of big vs little endian.
 *)

  PAVPixelFormat = ^TAVPixelFormat;
  TAVPixelFormat = (
    PIX_FMT_NONE= -1,
    PIX_FMT_YUV420P,   ///< planar YUV 4:2:0, 12bpp, (1 Cr & Cb sample per 2x2 Y samples)
    PIX_FMT_YUYV422,   ///< packed YUV 4:2:2, 16bpp, Y0 Cb Y1 Cr
    PIX_FMT_RGB24,     ///< packed RGB 8:8:8, 24bpp, RGBRGB...
    PIX_FMT_BGR24,     ///< packed RGB 8:8:8, 24bpp, BGRBGR...
    PIX_FMT_YUV422P,   ///< planar YUV 4:2:2, 16bpp, (1 Cr & Cb sample per 2x1 Y samples)
    PIX_FMT_YUV444P,   ///< planar YUV 4:4:4, 24bpp, (1 Cr & Cb sample per 1x1 Y samples)
    PIX_FMT_YUV410P,   ///< planar YUV 4:1:0,  9bpp, (1 Cr & Cb sample per 4x4 Y samples)
    PIX_FMT_YUV411P,   ///< planar YUV 4:1:1, 12bpp, (1 Cr & Cb sample per 4x1 Y samples)
    PIX_FMT_GRAY8,     ///<        Y        ,  8bpp
    PIX_FMT_MONOWHITE, ///<        Y        ,  1bpp, 0 is white, 1 is black, in each byte pixels are ordered from the msb to the lsb
    PIX_FMT_MONOBLACK, ///<        Y        ,  1bpp, 0 is black, 1 is white, in each byte pixels are ordered from the msb to the lsb
    PIX_FMT_PAL8,      ///< 8 bit with PIX_FMT_RGB32 palette
    PIX_FMT_YUVJ420P,  ///< planar YUV 4:2:0, 12bpp, full scale (JPEG), deprecated in favor of PIX_FMT_YUV420P and setting color_range
    PIX_FMT_YUVJ422P,  ///< planar YUV 4:2:2, 16bpp, full scale (JPEG), deprecated in favor of PIX_FMT_YUV422P and setting color_range
    PIX_FMT_YUVJ444P,  ///< planar YUV 4:4:4, 24bpp, full scale (JPEG), deprecated in favor of PIX_FMT_YUV444P and setting color_range
    PIX_FMT_XVMC_MPEG2_MC,///< XVideo Motion Acceleration via common packet passing
    PIX_FMT_XVMC_MPEG2_IDCT,
    PIX_FMT_UYVY422,   ///< packed YUV 4:2:2, 16bpp, Cb Y0 Cr Y1
    PIX_FMT_UYYVYY411, ///< packed YUV 4:1:1, 12bpp, Cb Y0 Y1 Cr Y2 Y3
    PIX_FMT_BGR8,      ///< packed RGB 3:3:2,  8bpp, (msb)2B 3G 3R(lsb)
    PIX_FMT_BGR4,      ///< packed RGB 1:2:1 bitstream,  4bpp, (msb)1B 2G 1R(lsb), a byte contains two pixels, the first pixel in the byte is the one composed by the 4 msb bits
    PIX_FMT_BGR4_BYTE, ///< packed RGB 1:2:1,  8bpp, (msb)1B 2G 1R(lsb)
    PIX_FMT_RGB8,      ///< packed RGB 3:3:2,  8bpp, (msb)2R 3G 3B(lsb)
    PIX_FMT_RGB4,      ///< packed RGB 1:2:1 bitstream,  4bpp, (msb)1R 2G 1B(lsb), a byte contains two pixels, the first pixel in the byte is the one composed by the 4 msb bits
    PIX_FMT_RGB4_BYTE, ///< packed RGB 1:2:1,  8bpp, (msb)1R 2G 1B(lsb)
    PIX_FMT_NV12,      ///< planar YUV 4:2:0, 12bpp, 1 plane for Y and 1 plane for the UV components, which are interleaved (first byte U and the following byte V)
    PIX_FMT_NV21,      ///< as above, but U and V bytes are swapped

    PIX_FMT_ARGB,      ///< packed ARGB 8:8:8:8, 32bpp, ARGBARGB...
    PIX_FMT_RGBA,      ///< packed RGBA 8:8:8:8, 32bpp, RGBARGBA...
    PIX_FMT_ABGR,      ///< packed ABGR 8:8:8:8, 32bpp, ABGRABGR...
    PIX_FMT_BGRA,      ///< packed BGRA 8:8:8:8, 32bpp, BGRABGRA...

    PIX_FMT_GRAY16BE,  ///<        Y        , 16bpp, big-endian
    PIX_FMT_GRAY16LE,  ///<        Y        , 16bpp, little-endian
    PIX_FMT_YUV440P,   ///< planar YUV 4:4:0 (1 Cr & Cb sample per 1x2 Y samples)
    PIX_FMT_YUVJ440P,  ///< planar YUV 4:4:0 full scale (JPEG), deprecated in favor of PIX_FMT_YUV440P and setting color_range
    PIX_FMT_YUVA420P,  ///< planar YUV 4:2:0, 20bpp, (1 Cr & Cb sample per 2x2 Y & A samples)
    PIX_FMT_VDPAU_H264,///< H.264 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
    PIX_FMT_VDPAU_MPEG1,///< MPEG-1 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
    PIX_FMT_VDPAU_MPEG2,///< MPEG-2 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
    PIX_FMT_VDPAU_WMV3,///< WMV3 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
    PIX_FMT_VDPAU_VC1, ///< VC-1 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
    PIX_FMT_RGB48BE,   ///< packed RGB 16:16:16, 48bpp, 16R, 16G, 16B, the 2-byte value for each R/G/B component is stored as big-endian
    PIX_FMT_RGB48LE,   ///< packed RGB 16:16:16, 48bpp, 16R, 16G, 16B, the 2-byte value for each R/G/B component is stored as little-endian

    PIX_FMT_RGB565BE,  ///< packed RGB 5:6:5, 16bpp, (msb)   5R 6G 5B(lsb), big-endian
    PIX_FMT_RGB565LE,  ///< packed RGB 5:6:5, 16bpp, (msb)   5R 6G 5B(lsb), little-endian
    PIX_FMT_RGB555BE,  ///< packed RGB 5:5:5, 16bpp, (msb)1A 5R 5G 5B(lsb), big-endian, most significant bit to 0
    PIX_FMT_RGB555LE,  ///< packed RGB 5:5:5, 16bpp, (msb)1A 5R 5G 5B(lsb), little-endian, most significant bit to 0

    PIX_FMT_BGR565BE,  ///< packed BGR 5:6:5, 16bpp, (msb)   5B 6G 5R(lsb), big-endian
    PIX_FMT_BGR565LE,  ///< packed BGR 5:6:5, 16bpp, (msb)   5B 6G 5R(lsb), little-endian
    PIX_FMT_BGR555BE,  ///< packed BGR 5:5:5, 16bpp, (msb)1A 5B 5G 5R(lsb), big-endian, most significant bit to 1
    PIX_FMT_BGR555LE,  ///< packed BGR 5:5:5, 16bpp, (msb)1A 5B 5G 5R(lsb), little-endian, most significant bit to 1

    PIX_FMT_VAAPI_MOCO, ///< HW acceleration through VA API at motion compensation entry-point, Picture.data[3] contains a vaapi_render_state struct which contains macroblocks as well as various fields extracted from headers
    PIX_FMT_VAAPI_IDCT, ///< HW acceleration through VA API at IDCT entry-point, Picture.data[3] contains a vaapi_render_state struct which contains fields extracted from headers
    PIX_FMT_VAAPI_VLD,  ///< HW decoding through VA API, Picture.data[3] contains a vaapi_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers

    PIX_FMT_YUV420P16LE,  ///< planar YUV 4:2:0, 24bpp, (1 Cr & Cb sample per 2x2 Y samples), little-endian
    PIX_FMT_YUV420P16BE,  ///< planar YUV 4:2:0, 24bpp, (1 Cr & Cb sample per 2x2 Y samples), big-endian
    PIX_FMT_YUV422P16LE,  ///< planar YUV 4:2:2, 32bpp, (1 Cr & Cb sample per 2x1 Y samples), little-endian
    PIX_FMT_YUV422P16BE,  ///< planar YUV 4:2:2, 32bpp, (1 Cr & Cb sample per 2x1 Y samples), big-endian
    PIX_FMT_YUV444P16LE,  ///< planar YUV 4:4:4, 48bpp, (1 Cr & Cb sample per 1x1 Y samples), little-endian
    PIX_FMT_YUV444P16BE,  ///< planar YUV 4:4:4, 48bpp, (1 Cr & Cb sample per 1x1 Y samples), big-endian
    PIX_FMT_VDPAU_MPEG4,  ///< MPEG4 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
    PIX_FMT_DXVA2_VLD,    ///< HW decoding through DXVA2, Picture.data[3] contains a LPDIRECT3DSURFACE9 pointer

    PIX_FMT_RGB444LE,  ///< packed RGB 4:4:4, 16bpp, (msb)4A 4R 4G 4B(lsb), little-endian, most significant bits to 0
    PIX_FMT_RGB444BE,  ///< packed RGB 4:4:4, 16bpp, (msb)4A 4R 4G 4B(lsb), big-endian, most significant bits to 0
    PIX_FMT_BGR444LE,  ///< packed BGR 4:4:4, 16bpp, (msb)4A 4B 4G 4R(lsb), little-endian, most significant bits to 1
    PIX_FMT_BGR444BE,  ///< packed BGR 4:4:4, 16bpp, (msb)4A 4B 4G 4R(lsb), big-endian, most significant bits to 1
    PIX_FMT_GRAY8A,    ///< 8bit gray, 8bit alpha
    PIX_FMT_BGR48BE,   ///< packed RGB 16:16:16, 48bpp, 16B, 16G, 16R, the 2-byte value for each R/G/B component is stored as big-endian
    PIX_FMT_BGR48LE,   ///< packed RGB 16:16:16, 48bpp, 16B, 16G, 16R, the 2-byte value for each R/G/B component is stored as little-endian

    //the following 10 formats have the disadvantage of needing 1 format for each bit depth, thus
    //If you want to support multiple bit depths, then using PIX_FMT_YUV420P16* with the bpp stored seperately
    //is better
    PIX_FMT_YUV420P9BE, ///< planar YUV 4:2:0, 13.5bpp, (1 Cr & Cb sample per 2x2 Y samples), big-endian
    PIX_FMT_YUV420P9LE, ///< planar YUV 4:2:0, 13.5bpp, (1 Cr & Cb sample per 2x2 Y samples), little-endian
    PIX_FMT_YUV420P10BE,///< planar YUV 4:2:0, 15bpp, (1 Cr & Cb sample per 2x2 Y samples), big-endian
    PIX_FMT_YUV420P10LE,///< planar YUV 4:2:0, 15bpp, (1 Cr & Cb sample per 2x2 Y samples), little-endian
    PIX_FMT_YUV422P10BE,///< planar YUV 4:2:2, 20bpp, (1 Cr & Cb sample per 2x1 Y samples), big-endian
    PIX_FMT_YUV422P10LE,///< planar YUV 4:2:2, 20bpp, (1 Cr & Cb sample per 2x1 Y samples), little-endian
    PIX_FMT_YUV444P9BE, ///< planar YUV 4:4:4, 27bpp, (1 Cr & Cb sample per 1x1 Y samples), big-endian
    PIX_FMT_YUV444P9LE, ///< planar YUV 4:4:4, 27bpp, (1 Cr & Cb sample per 1x1 Y samples), little-endian
    PIX_FMT_YUV444P10BE,///< planar YUV 4:4:4, 30bpp, (1 Cr & Cb sample per 1x1 Y samples), big-endian
    PIX_FMT_YUV444P10LE,///< planar YUV 4:4:4, 30bpp, (1 Cr & Cb sample per 1x1 Y samples), little-endian
    PIX_FMT_YUV422P9BE, ///< planar YUV 4:2:2, 18bpp, (1 Cr & Cb sample per 2x1 Y samples), big-endian
    PIX_FMT_YUV422P9LE, ///< planar YUV 4:2:2, 18bpp, (1 Cr & Cb sample per 2x1 Y samples), little-endian
    PIX_FMT_VDA_VLD,    ///< hardware decoding through VDA

{$ifdef AV_PIX_FMT_ABI_GIT_MASTER}
    PIX_FMT_RGBA64BE,  ///< packed RGBA 16:16:16:16, 64bpp, 16R, 16G, 16B, 16A, the 2-byte value for each R/G/B/A component is stored as big-endian
    PIX_FMT_RGBA64LE,  ///< packed RGBA 16:16:16:16, 64bpp, 16R, 16G, 16B, 16A, the 2-byte value for each R/G/B/A component is stored as little-endian
    PIX_FMT_BGRA64BE,  ///< packed RGBA 16:16:16:16, 64bpp, 16B, 16G, 16R, 16A, the 2-byte value for each R/G/B/A component is stored as big-endian
    PIX_FMT_BGRA64LE,  ///< packed RGBA 16:16:16:16, 64bpp, 16B, 16G, 16R, 16A, the 2-byte value for each R/G/B/A component is stored as little-endian
{$endif}
    PIX_FMT_GBRP,      ///< planar GBR 4:4:4 24bpp
    PIX_FMT_GBRP9BE,   ///< planar GBR 4:4:4 27bpp, big endian
    PIX_FMT_GBRP9LE,   ///< planar GBR 4:4:4 27bpp, little endian
    PIX_FMT_GBRP10BE,  ///< planar GBR 4:4:4 30bpp, big endian
    PIX_FMT_GBRP10LE,  ///< planar GBR 4:4:4 30bpp, little endian
    PIX_FMT_GBRP16BE,  ///< planar GBR 4:4:4 48bpp, big endian
    PIX_FMT_GBRP16LE,  ///< planar GBR 4:4:4 48bpp, little endian

{$ifndef AV_PIX_FMT_ABI_GIT_MASTER}
    PIX_FMT_RGBA64BE = $123,  ///< packed RGBA 16:16:16:16, 64bpp, 16R, 16G, 16B, 16A, the 2-byte value for each R/G/B/A component is stored as big-endian
    PIX_FMT_RGBA64LE,  ///< packed RGBA 16:16:16:16, 64bpp, 16R, 16G, 16B, 16A, the 2-byte value for each R/G/B/A component is stored as little-endian
    PIX_FMT_BGRA64BE,  ///< packed RGBA 16:16:16:16, 64bpp, 16B, 16G, 16R, 16A, the 2-byte value for each R/G/B/A component is stored as big-endian
    PIX_FMT_BGRA64LE,  ///< packed RGBA 16:16:16:16, 64bpp, 16B, 16G, 16R, 16A, the 2-byte value for each R/G/B/A component is stored as little-endian
{$endif}
    PIX_FMT_0RGB = $123+4,      ///< packed RGB 8:8:8, 32bpp, 0RGB0RGB...
    PIX_FMT_RGB0,      ///< packed RGB 8:8:8, 32bpp, RGB0RGB0...
    PIX_FMT_0BGR,      ///< packed BGR 8:8:8, 32bpp, 0BGR0BGR...
    PIX_FMT_BGR0,      ///< packed BGR 8:8:8, 32bpp, BGR0BGR0...
    PIX_FMT_NB         ///< number of pixel formats, DO NOT USE THIS if you want to link with shared libav* because the number of formats might differ between versions
  );

const
    PIX_FMT_Y400A   = PIX_FMT_GRAY8A;
    PIX_FMT_GBR24P  = PIX_FMT_GBRP;

{$ifdef WORDS_BIGENDIAN}
    PIX_FMT_RGB32   = PIX_FMT_ARGB;
    PIX_FMT_RGB32_1 = PIX_FMT_RGBA;
    PIX_FMT_BGR32   = PIX_FMT_ABGR;
    PIX_FMT_BGR32_1 = PIX_FMT_BGRA;
    PIX_FMT_0RGB32  = PIX_FMT_0RGB;
    PIX_FMT_0BGR32  = PIX_FMT_0BGR;

    PIX_FMT_GRAY16  = PIX_FMT_GRAY16BE;
    PIX_FMT_RGB48   = PIX_FMT_RGB48BE;
    PIX_FMT_RGB565  = PIX_FMT_RGB565BE;
    PIX_FMT_RGB555  = PIX_FMT_RGB555BE;
    PIX_FMT_RGB444  = PIX_FMT_RGB444BE;
    PIX_FMT_BGR48   = PIX_FMT_BGR48BE;
    PIX_FMT_BGR565  = PIX_FMT_BGR565BE;
    PIX_FMT_BGR555  = PIX_FMT_BGR555BE;
    PIX_FMT_BGR444  = PIX_FMT_BGR444BE;

    PIX_FMT_YUV420P9  = PIX_FMT_YUV420P9BE;
    PIX_FMT_YUV422P9  = PIX_FMT_YUV422P9BE;
    PIX_FMT_YUV444P9  = PIX_FMT_YUV444P9BE;
    PIX_FMT_YUV420P10 = PIX_FMT_YUV420P10BE;
    PIX_FMT_YUV422P10 = PIX_FMT_YUV422P10BE;
    PIX_FMT_YUV444P10 = PIX_FMT_YUV444P10BE;
    PIX_FMT_YUV420P16 = PIX_FMT_YUV420P16BE;
    PIX_FMT_YUV422P16 = PIX_FMT_YUV422P16BE;
    PIX_FMT_YUV444P16 = PIX_FMT_YUV444P16BE;

    PIX_FMT_RGBA64    = PIX_FMT_RGBA64BE;
    PIX_FMT_BGRA64    = PIX_FMT_BGRA64BE;
    PIX_FMT_GBRP9     = PIX_FMT_GBRP9BE;
    PIX_FMT_GBRP10    = PIX_FMT_GBRP10BE;
    PIX_FMT_GBRP16    = PIX_FMT_GBRP16BE;

{$else}
    PIX_FMT_RGB32   = PIX_FMT_BGRA;
    PIX_FMT_RGB32_1 = PIX_FMT_ABGR;
    PIX_FMT_BGR32   = PIX_FMT_RGBA;
    PIX_FMT_BGR32_1 = PIX_FMT_ARGB;
    PIX_FMT_0RGB32  = PIX_FMT_BGR0;
    PIX_FMT_0BGR32  = PIX_FMT_RGB0;

    PIX_FMT_GRAY16  = PIX_FMT_GRAY16LE;
    PIX_FMT_RGB48   = PIX_FMT_RGB48LE;
    PIX_FMT_RGB565  = PIX_FMT_RGB565LE;
    PIX_FMT_RGB555  = PIX_FMT_RGB555LE;
    PIX_FMT_RGB444  = PIX_FMT_RGB444LE;
    PIX_FMT_BGR48   = PIX_FMT_BGR48LE;
    PIX_FMT_BGR565  = PIX_FMT_BGR565LE;
    PIX_FMT_BGR555  = PIX_FMT_BGR555LE;
    PIX_FMT_BGR444  = PIX_FMT_BGR444LE;

    PIX_FMT_YUV420P9  = PIX_FMT_YUV420P9LE;
    PIX_FMT_YUV422P9  = PIX_FMT_YUV422P9LE;
    PIX_FMT_YUV444P9  = PIX_FMT_YUV444P9LE;
    PIX_FMT_YUV420P10 = PIX_FMT_YUV420P10LE;
    PIX_FMT_YUV422P10 = PIX_FMT_YUV422P10LE;
    PIX_FMT_YUV444P10 = PIX_FMT_YUV444P10LE;
    PIX_FMT_YUV420P16 = PIX_FMT_YUV420P16LE;
    PIX_FMT_YUV422P16 = PIX_FMT_YUV422P16LE;
    PIX_FMT_YUV444P16 = PIX_FMT_YUV444P16LE;

    PIX_FMT_RGBA64    = PIX_FMT_RGBA64LE;
    PIX_FMT_BGRA64    = PIX_FMT_BGRA64LE;
    PIX_FMT_GBRP9     = PIX_FMT_GBRP9LE;
    PIX_FMT_GBRP10    = PIX_FMT_GBRP10LE;
    PIX_FMT_GBRP16    = PIX_FMT_GBRP16LE;

{$ENDIF}

(* libavutil/common.h *) // until now MKTAG and MKBETAG is all from common.h KMS 19/5/2010

function MKTAG  (a, b, c, d: AnsiChar): integer;
function MKBETAG(a, b, c, d: AnsiChar): integer;

(* libavutil/mem.h *)

(* memory handling functions *)

(**
 * Allocate a block of size bytes with alignment suitable for all
 * memory accesses (including vectors if available on the CPU).
 * @param size Size in bytes for the memory block to be allocated.
 * @return Pointer to the allocated block, NULL if the block cannot
 * be allocated.
 * @see av_mallocz()
 *)
function av_malloc(size: size_t): pointer;
  cdecl; external av__util; {av_malloc_attrib av_alloc_size(1)}

(**
 * Allocate or reallocate a block of memory.
 * If ptr is NULL and size > 0, allocate a new block. If 
 * size is zero, free the memory block pointed to by ptr.
 * @param size Size in bytes for the memory block to be allocated or
 * reallocated.
 * @param ptr Pointer to a memory block already allocated with
 * av_malloc(z)() or av_realloc() or NULL.
 * @return Pointer to a newly reallocated block or NULL if the block
 * cannot be allocated or the function is used to free the memory block.
 * @see av_fast_realloc()
 *)
function av_realloc(ptr: pointer; size: size_t): pointer;
  cdecl; external av__util; {av_alloc_size(2)}

(**
 * Allocate or reallocate a block of memory.
 * This function does the same thing as av_realloc, except:
 * - It takes two arguments and checks the result of the multiplication for
 *   integer overflow.
 * - It frees the input block in case of failure, thus avoiding the memory
 *   leak with the classic "buf = realloc(buf); if (!buf) return -1;".
 *)
function av_realloc_f(ptr: pointer; nelem: size_t; elsize: size_t): pointer;
  cdecl; external av__util;

(**
 * Free a memory block which has been allocated with av_malloc(z)() or
 * av_realloc().
 * @param ptr Pointer to the memory block which should be freed.
 * @note ptr = NULL is explicitly allowed.
 * @note It is recommended that you use av_freep() instead.
 * @see av_freep()
 *)
procedure av_free(ptr: pointer);
  cdecl; external av__util;

(**
 * Allocate a block of size bytes with alignment suitable for all
 * memory accesses (including vectors if available on the CPU) and
 * zeroes all the bytes of the block.
 * @param size Size in bytes for the memory block to be allocated.
 * @return Pointer to the allocated block, NULL if it cannot be allocated.
 * @see av_malloc()
 *)
function av_mallocz(size: size_t): pointer;
  cdecl; external av__util; {av_malloc_attrib av_alloc_size(1)}

(**
 * Allocate a block of nmemb * size bytes with alignment suitable for all
 * memory accesses (including vectors if available on the CPU) and
 * zero all the bytes of the block.
 * The allocation will fail if nmemb * size is greater than or equal
 * to INT_MAX.
 * @param nmemb
 * @param size
 * @return Pointer to the allocated block, NULL if it cannot be allocated.
 *)
function av_calloc(nmemb: size_t; size: size_t): pointer;
  cdecl; external av__util; {av_malloc_attrib}

(**
 * Duplicate the string s.
 * @param s string to be duplicated.
 * @return Pointer to a newly allocated string containing a
 * copy of s or NULL if the string cannot be allocated.
 *)
function av_strdup({const} s: PAnsiChar): PAnsiChar;
  cdecl; external av__util; {av_malloc_attrib}

(**
 * Free a memory block which has been allocated with av_malloc(z)() or
 * av_realloc() and set the pointer pointing to it to NULL.
 * @param ptr Pointer to the pointer to the memory block which should
 * be freed.
 * @see av_free()
 *)
procedure av_freep (ptr: pointer);
  cdecl; external av__util;

(**
 * Add an element to a dynamic array.
 *
 * @param tab_ptr Pointer to the array.
 * @param nb_ptr  Pointer to the number of elements in the array.
 * @param elem    Element to be added.
 *)
procedure av_dynarray_add(tab_ptr: pointer; nb_ptr: PCint; elem: pointer);
  cdecl; external av__util;

(**
 * Multiply two size_t values checking for overflow.
 * @return  0 if success, AVERROR(EINVAL) if overflow.
 *)
function av_size_mult(a: size_t; b: size_t; r: ^size_t): cint;

(* libavutil/log.h *)

type
(**
 * Describe the class of an AVClass context structure. That is an
 * arbitrary struct of which the first field is a pointer to an
 * AVClass struct (e.g. AVCodecContext, AVFormatContext etc.).
 *)
  PAVClass = ^TAVClass;
  TAVClass = record
    (**
     * The name of the class; usually it is the same name as the
     * context structure type to which the AVClass is associated.
     *)
    class_name: PAnsiChar;

    (**
     * A pointer to a function which returns the name of a context
     * instance ctx associated with the class.
     *)
    item_name: function(): PAnsiChar; cdecl;

    (**
     * a pointer to the first option specified in the class if any or NULL
     *
     * @see av_set_default_options()
     *)
    option: PAVOption;

    (**
     * LIBAVUTIL_VERSION with which this structure was created.
     * This is used to allow fields to be added without requiring major
     * version bumps everywhere.
     *)
    version: cint;

    (**
     * Offset in the structure where log_level_offset is stored.
     * 0 means there is no such variable
     *)
    log_level_offset_offset: cint;

    (**
     * Offset in the structure where a pointer to the parent context for loging is stored.
     * for example a decoder that uses eval.c could pass its AVCodecContext to eval as such
     * parent context. And a av_log() implementation could then display the parent context
     * can be NULL of course
     *)
    parent_log_context_offset: cint;
    
    (**
     * Return next AVOptions-enabled child or NULL
     *)
    child_next: function(obj: pointer; prev: pointer): Pointer; cdecl;
    
    (**
     * Return an AVClass corresponding to next potential
     * AVOptions-enabled child.
     *
     * The difference between child_next and this is that
     * child_next iterates over _already existing_ objects, while
     * child_class_next iterates over _all possible_ children.
     *)
    child_class_next: function(prev: PAVClass): PAVClass; cdecl;
  end;

const
  AV_LOG_QUIET   = -8;

(**
 * Something went really wrong and we will crash now.
 *)
  AV_LOG_PANIC   =  0;

(**
 * Something went wrong and recovery is not possible.
 * For example, no header was found for a format which depends
 * on headers or an illegal combination of parameters is used.
 *)
  AV_LOG_FATAL   =  8;

(**
 * Something went wrong and cannot losslessly be recovered.
 * However, not all future data is affected.
 *)
  AV_LOG_ERROR   = 16;

(**
 * Something somehow does not look correct. This may or may not
 * lead to problems. An example would be the use of '-vstrict -2'.
 *)
  AV_LOG_WARNING = 24;

  AV_LOG_INFO    = 32;
  AV_LOG_VERBOSE = 40;

(**
 * Stuff which is only useful for libav* developers.
 *)
  AV_LOG_DEBUG   = 48;

(**
 * Send the specified message to the log if the level is less than or equal
 * to the current av_log_level. By default, all logging messages are sent to
 * stderr. This behavior can be altered by setting a different av_vlog callback
 * function.
 *
 * @param avcl A pointer to an arbitrary struct of which the first field is a
 * pointer to an AVClass struct.
 * @param level The importance level of the message, lower values signifying
 * higher importance.
 * @param fmt The format string (printf-compatible) that specifies how
 * subsequent arguments are converted to output.
 * @see av_vlog
 *)

{** to be translated if needed
void av_log(void *avcl, int level, const char *fmt, ...) av_printf_format(3, 4);
**}

type
  va_list = pointer;

procedure av_vlog(avcl: pointer; level: cint; fmt: {const} PAnsiChar; dummy: va_list);
  cdecl; external av__util;
function av_log_get_level(): cint;
  cdecl; external av__util;
procedure av_log_set_level(level: cint);
  cdecl; external av__util;

{** to be translated if needed
void av_log_set_callback(void (*)(void*, int, const char*, va_list));
void av_log_default_callback(void* ptr, int level, const char* fmt, va_list vl);
**}

function av_default_item_name (ctx: pointer): PAnsiChar;
  cdecl; external av__util;

(**
 * Format a line of log the same way as the default callback.
 * @param line          buffer to receive the formated line
 * @param line_size     size of the buffer
 * @param print_prefix  used to store whether the prefix must be printed;
 *                      must point to a persistent integer initially set to 1
 *)
procedure av_log_format_line(ptr: pointer; level: cint;  fmt: {const} Pchar; vl: va_list;
                             line: Pchar; line_size: cint; print_prefix: Pcint);
  cdecl; external av__util;

(**
 * av_dlog macros
 * Useful to print debug messages that shouldn't get compiled in normally.
 *)
(** to be translated if needed
#ifdef DEBUG
#    define av_dlog(pctx, ...) av_log(pctx, AV_LOG_DEBUG, __VA_ARGS__)
#else
#    define av_dlog(pctx, ...) do { if (0) av_log(pctx, AV_LOG_DEBUG, __VA_ARGS__); } while (0)
#endif
**)

(**
 * Skip repeated messages, this requires the user app to use av_log() instead of
 * (f)printf as the 2 would otherwise interfere and lead to
 * "Last message repeated x times" messages below (f)printf messages with some
 * bad luck.
 * Also to receive the last, "last repeated" line if any, the user app must
 * call av_log(NULL, AV_LOG_QUIET, "%s", ""); at the end
 *)
const
  AV_LOG_SKIP_REPEATED = 1;
procedure av_log_set_flags(arg: cint);
  cdecl; external av__util;

implementation

(* To Be Implemented, March 2012 KMS *)
function av_x_if_null(p: {const} pointer; x: {const} pointer): pointer;
begin
    //return (void *)(intptr_t)(p ? p : x);
end;

(* libavutil/common.h *)

function MKTAG(a, b, c, d: AnsiChar): integer;
begin
  Result := (ord(a) or (ord(b) shl 8) or (ord(c) shl 16) or (ord(d) shl 24));
end;

function MKBETAG(a, b, c, d: AnsiChar): integer;
begin
  Result := (ord(d) or (ord(c) shl 8) or (ord(b) shl 16) or (ord(a) shl 24));
end;

(* To Be Implemented, March 2012 KMS *)
function av_size_mult(a: size_t; b: size_t; r: ^size_t): cint;
begin
    {
    size_t t = a * b;
    /* Hack inspired from glibc: only try the division if nelem and elsize
     * are both greater than sqrt(SIZE_MAX). */
    if ((a | b) >= ((size_t)1 << (sizeof(size_t) * 4)) && a && t / a != b)
        return AVERROR(EINVAL);
    *r = t;
    return 0;
}
end;

end.
