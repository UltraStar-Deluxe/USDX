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
 *)

(*
 * This is a part of Pascal porting of ffmpeg.
 * - Originally by Victor Zinetz for Delphi and Free Pascal on Windows.
 * - For Mac OS X, some modifications were made by The Creative CAT, denoted as CAT
 *   in the source codes.
 * - Changes and updates by the UltraStar Deluxe Team
 *)

(*
 * Conversions of
 *
 * libavutil/avutil.h:
 *  Min. version: 49.0.1, revision 6577,  Sat Oct 7 15:30:46 2006 UTC
 *  Max. version: 49.14.0, revision 16912, Sun Feb 1 02:00:19 2009 UTC
 *
 * libavutil/mem.h:
 *  revision 16590, Tue Jan 13 23:44:16 2009 UTC
 *
 * libavutil/log.h:
 *  revision 16571, Tue Jan 13 00:14:43 2009 UTC
 *)
{
 Update changes auf avutil.h, mem.h and log.h
 Max. version 50.7.0, Tue, Dec 29 0:30:00 2009 UTC 
 include/keep pixfmt.h (change in revision 50.01.0)
 Maybe, the pixelformats are not needed, but it has not been checked.
 log.h is only partial.
}

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
  rational,
  UConfig;

const
  (* Max. supported version by this header *)
  LIBAVUTIL_MAX_VERSION_MAJOR   = 50;
  LIBAVUTIL_MAX_VERSION_MINOR   = 7;
  LIBAVUTIL_MAX_VERSION_RELEASE = 0;
  LIBAVUTIL_MAX_VERSION = (LIBAVUTIL_MAX_VERSION_MAJOR * VERSION_MAJOR) +
                          (LIBAVUTIL_MAX_VERSION_MINOR * VERSION_MINOR) +
                          (LIBAVUTIL_MAX_VERSION_RELEASE * VERSION_RELEASE);

  (* Min. supported version by this header *)
  LIBAVUTIL_MIN_VERSION_MAJOR   = 49;
  LIBAVUTIL_MIN_VERSION_MINOR   = 0;
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

{$IF LIBAVUTIL_VERSION >= 49008000} // 49.8.0
(**
 * Returns the LIBAVUTIL_VERSION_INT constant.
 *)
function avutil_version(): cuint;
  cdecl; external av__format;
{$IFEND}

{$IF LIBAVUTIL_VERSION >= 50004000} // >= 50.4.0
(**
 * Returns the libavutil build-time configuration.
 *)
function avutil_configuration(): PAnsiChar;
  cdecl; external av__format;

(**
 * Returns the libavutil license.
 *)
function avutil_license(): PAnsiChar;
  cdecl; external av__format;
{$IFEND}

type
(**
 * Pixel format. Notes:
 *
 * PIX_FMT_RGB32 is handled in an endian-specific manner. An RGBA
 * color is put together as:
 *  (A << 24) | (R << 16) | (G << 8) | B
 * This is stored as BGRA on little-endian CPU architectures and ARGB on
 * big-endian CPUs.
 *
 * When the pixel format is palettized RGB (PIX_FMT_PAL8), the palettized
 * image data is stored in AVFrame.data[0]. The palette is transported in
 * AVFrame.data[1], is 1024 bytes long (256 4-byte entries) and is
 * formatted the same as in PIX_FMT_RGB32 described above (i.e., it is
 * also endian-specific). Note also that the individual RGB palette
 * components stored in AVFrame.data[1] should be in the range 0..255.
 * This is important as many custom PAL8 video codecs that were designed
 * to run on the IBM VGA graphics adapter use 6-bit palette components.
 *
 * For all the 8bit per pixel formats, an RGB32 palette is in data[1] like
 * for pal8. This palette is filled in automatically by the function
 * allocating the picture.
 *
 * Note, make sure that all newly added big endian formats have pix_fmt&1==1
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
{$IF LIBAVUTIL_VERSION <= 50001000} // 50.01.0
    PIX_FMT_RGB32,     ///< packed RGB 8:8:8, 32bpp, (msb)8A 8R 8G 8B(lsb), in CPU endianness
{$IFEND}
    PIX_FMT_YUV410P,   ///< planar YUV 4:1:0,  9bpp, (1 Cr & Cb sample per 4x4 Y samples)
    PIX_FMT_YUV411P,   ///< planar YUV 4:1:1, 12bpp, (1 Cr & Cb sample per 4x1 Y samples)
{$IF LIBAVUTIL_VERSION <= 50000000} // 50.00.0
    PIX_FMT_RGB565,    ///< packed RGB 5:6:5, 16bpp, (msb)   5R 6G 5B(lsb), in CPU endianness
    PIX_FMT_RGB555,    ///< packed RGB 5:5:5, 16bpp, (msb)1A 5R 5G 5B(lsb), in CPU endianness, most significant bit to 0
{$IFEND}
    PIX_FMT_GRAY8,     ///<        Y        ,  8bpp
    PIX_FMT_MONOWHITE, ///<        Y        ,  1bpp, 0 is white, 1 is black
    PIX_FMT_MONOBLACK, ///<        Y        ,  1bpp, 0 is black, 1 is white
    PIX_FMT_PAL8,      ///< 8 bit with PIX_FMT_RGB32 palette
    PIX_FMT_YUVJ420P,  ///< planar YUV 4:2:0, 12bpp, full scale (JPEG)
    PIX_FMT_YUVJ422P,  ///< planar YUV 4:2:2, 16bpp, full scale (JPEG)
    PIX_FMT_YUVJ444P,  ///< planar YUV 4:4:4, 24bpp, full scale (JPEG)
    PIX_FMT_XVMC_MPEG2_MC,///< XVideo Motion Acceleration via common packet passing
    PIX_FMT_XVMC_MPEG2_IDCT,
    PIX_FMT_UYVY422,   ///< packed YUV 4:2:2, 16bpp, Cb Y0 Cr Y1
    PIX_FMT_UYYVYY411, ///< packed YUV 4:1:1, 12bpp, Cb Y0 Y1 Cr Y2 Y3
{$IF LIBAVUTIL_VERSION <= 50001000} // 50.01.0
    PIX_FMT_BGR32,     ///< packed RGB 8:8:8, 32bpp, (msb)8A 8B 8G 8R(lsb), in CPU endianness
{$IFEND}
{$IF LIBAVUTIL_VERSION <= 50000000} // 50.00.0
    PIX_FMT_BGR565,    ///< packed RGB 5:6:5, 16bpp, (msb)   5B 6G 5R(lsb), in CPU endianness
    PIX_FMT_BGR555,    ///< packed RGB 5:5:5, 16bpp, (msb)1A 5B 5G 5R(lsb), in CPU endianness, most significant bit to 1
{$IFEND}
    PIX_FMT_BGR8,      ///< packed RGB 3:3:2,  8bpp, (msb)2B 3G 3R(lsb)
    PIX_FMT_BGR4,      ///< packed RGB 1:2:1,  4bpp, (msb)1B 2G 1R(lsb)
    PIX_FMT_BGR4_BYTE, ///< packed RGB 1:2:1,  8bpp, (msb)1B 2G 1R(lsb)
    PIX_FMT_RGB8,      ///< packed RGB 3:3:2,  8bpp, (msb)2R 3G 3B(lsb)
    PIX_FMT_RGB4,      ///< packed RGB 1:2:1,  4bpp, (msb)1R 2G 1B(lsb)
    PIX_FMT_RGB4_BYTE, ///< packed RGB 1:2:1,  8bpp, (msb)1R 2G 1B(lsb)
    PIX_FMT_NV12,      ///< planar YUV 4:2:0, 12bpp, 1 plane for Y and 1 for UV
    PIX_FMT_NV21,      ///< as above, but U and V bytes are swapped
{$IF LIBAVUTIL_VERSION <= 50001000} // 50.01.0
    PIX_FMT_RGB32_1,   ///< packed RGB 8:8:8, 32bpp, (msb)8R 8G 8B 8A(lsb), in CPU endianness
    PIX_FMT_BGR32_1,   ///< packed RGB 8:8:8, 32bpp, (msb)8B 8G 8R 8A(lsb), in CPU endianness
{$ELSE} // 50.02.0
    PIX_FMT_ARGB,      ///< packed ARGB 8:8:8:8, 32bpp, ARGBARGB...
    PIX_FMT_RGBA,      ///< packed RGBA 8:8:8:8, 32bpp, RGBARGBA...
    PIX_FMT_ABGR,      ///< packed ABGR 8:8:8:8, 32bpp, ABGRABGR...
    PIX_FMT_BGRA,      ///< packed BGRA 8:8:8:8, 32bpp, BGRABGRA...
{$IFEND}
    PIX_FMT_GRAY16BE,  ///<        Y        , 16bpp, big-endian
    PIX_FMT_GRAY16LE,  ///<        Y        , 16bpp, little-endian
    PIX_FMT_YUV440P,   ///< planar YUV 4:4:0 (1 Cr & Cb sample per 1x2 Y samples)
    PIX_FMT_YUVJ440P,  ///< planar YUV 4:4:0 full scale (JPEG)
    PIX_FMT_YUVA420P,  ///< planar YUV 4:2:0, 20bpp, (1 Cr & Cb sample per 2x2 Y & A samples)
    PIX_FMT_VDPAU_H264,///< H.264 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
    PIX_FMT_VDPAU_MPEG1,///< MPEG-1 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
    PIX_FMT_VDPAU_MPEG2,///< MPEG-2 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
    PIX_FMT_VDPAU_WMV3,///< WMV3 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
    PIX_FMT_VDPAU_VC1, ///< VC-1 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
{$IF LIBAVUTIL_VERSION >= 49015000} // 49.15.0
    PIX_FMT_RGB48BE,   ///< packed RGB 16:16:16, 48bpp, 16R, 16G, 16B, big-endian
    PIX_FMT_RGB48LE,   ///< packed RGB 16:16:16, 48bpp, 16R, 16G, 16B, little-endian
{$IFEND}
{$IF LIBAVUTIL_VERSION >= 50001000} // 50.01.0
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
{$IFEND}
    PIX_FMT_NB         ///< number of pixel formats, DO NOT USE THIS if you want to link with shared libav* because the number of formats might differ between versions
  );

const
{$ifdef WORDS_BIGENDIAN}
  {$IF LIBAVUTIL_VERSION <= 50001000} // 50.01.0
    PIX_FMT_RGBA    = PIX_FMT_RGB32_1;
    PIX_FMT_BGRA    = PIX_FMT_BGR32_1;
    PIX_FMT_ARGB    = PIX_FMT_RGB32;
    PIX_FMT_ABGR    = PIX_FMT_BGR32;
  {$ELSE} // 50.02.0
    PIX_FMT_RGB32   = PIX_FMT_ARGB;
    PIX_FMT_RGB32_1 = PIX_FMT_RGBA;
    PIX_FMT_BGR32   = PIX_FMT_ABGR;
    PIX_FMT_BGR32_1 = PIX_FMT_BGRA;
  {$IFEND}
  PIX_FMT_GRAY16  = PIX_FMT_GRAY16BE;
  {$IF LIBAVUTIL_VERSION >= 49015000} // 49.15.0
    PIX_FMT_RGB48   = PIX_FMT_RGB48BE;
  {$IFEND}
  {$IF LIBAVUTIL_VERSION >= 50001000} // 50.01.0
    PIX_FMT_RGB565  = PIX_FMT_RGB565BE;
    PIX_FMT_RGB555  = PIX_FMT_RGB555BE;
    PIX_FMT_BGR565  = PIX_FMT_BGR565BE;
    PIX_FMT_BGR555  = PIX_FMT_BGR555BE
  {$IFEND}
{$else}
  {$IF LIBAVUTIL_VERSION <= 50001000} // 50.01.0
    PIX_FMT_RGBA    = PIX_FMT_BGR32;
    PIX_FMT_BGRA    = PIX_FMT_RGB32;
    PIX_FMT_ARGB    = PIX_FMT_BGR32_1;
    PIX_FMT_ABGR    = PIX_FMT_RGB32_1;
  {$ELSE} // 50.02.0
    PIX_FMT_RGB32   = PIX_FMT_BGRA;
    PIX_FMT_RGB32_1 = PIX_FMT_ABGR;
    PIX_FMT_BGR32   = PIX_FMT_RGBA;
    PIX_FMT_BGR32_1 = PIX_FMT_ARGB;
  {$IFEND}
  PIX_FMT_GRAY16  = PIX_FMT_GRAY16LE;
  {$IF LIBAVUTIL_VERSION >= 49015000} // 49.15.0
    PIX_FMT_RGB48   = PIX_FMT_RGB48LE;
  {$IFEND}
  {$IF LIBAVUTIL_VERSION >= 50001000} // 50.01.0
    PIX_FMT_RGB565  = PIX_FMT_RGB565LE;
    PIX_FMT_RGB555  = PIX_FMT_RGB555LE;
    PIX_FMT_BGR565  = PIX_FMT_BGR565LE;
    PIX_FMT_BGR555  = PIX_FMT_BGR555LE;
  {$IFEND}
{$ENDIF}

{$IF LIBAVUTIL_VERSION_MAJOR < 50} // 50.0.0
  PIX_FMT_UYVY411 = PIX_FMT_UYYVYY411;
  PIX_FMT_RGBA32  = PIX_FMT_RGB32;
  PIX_FMT_YUV422  = PIX_FMT_YUYV422;
{$IFEND}

(* libavutil/common.h *) // until now MKTAG is all from common.h KMS 9/6/2009

function MKTAG(a, b, c, d: AnsiChar): integer;

(* libavutil/mem.h *)
(* memory handling functions *)

(**
 * Allocates a block of size bytes with alignment suitable for all
 * memory accesses (including vectors if available on the CPU).
 * @param size Size in bytes for the memory block to be allocated.
 * @return Pointer to the allocated block, NULL if the block cannot
 * be allocated.
 * @see av_mallocz()
 *)
function av_malloc(size: cuint): pointer;
  cdecl; external av__util; {av_malloc_attrib av_alloc_size(1)}

(**
 * Allocates or reallocates a block of memory.
 * If ptr is NULL and size > 0, allocates a new block. If 
 * size is zero, frees the memory block pointed to by ptr.
 * @param size Size in bytes for the memory block to be allocated or
 * reallocated.
 * @param ptr Pointer to a memory block already allocated with
 * av_malloc(z)() or av_realloc() or NULL.
 * @return Pointer to a newly reallocated block or NULL if the block
 * cannot be allocated or the function is used to free the memory block.
 * @see av_fast_realloc()
 *)
function av_realloc(ptr: pointer; size: cuint): pointer;
  cdecl; external av__util; {av_alloc_size(2)}

(**
 * Frees a memory block which has been allocated with av_malloc(z)() or
 * av_realloc().
 * @param ptr Pointer to the memory block which should be freed.
 * @note ptr = NULL is explicitly allowed.
 * @note It is recommended that you use av_freep() instead.
 * @see av_freep()
 *)
procedure av_free(ptr: pointer);
  cdecl; external av__util;

(**
 * Allocates a block of size bytes with alignment suitable for all
 * memory accesses (including vectors if available on the CPU) and
 * zeroes all the bytes of the block.
 * @param size Size in bytes for the memory block to be allocated.
 * @return Pointer to the allocated block, NULL if it cannot be allocated.
 * @see av_malloc()
 *)
function av_mallocz(size: cuint): pointer;
  cdecl; external av__util; {av_malloc_attrib av_alloc_size(1)}

(**
 * Duplicates the string s.
 * @param s string to be duplicated.
 * @return Pointer to a newly allocated string containing a
 * copy of s or NULL if the string cannot be allocated.
 *)
function av_strdup({const} s: PAnsiChar): PAnsiChar;
  cdecl; external av__util; {av_malloc_attrib}

(**
 * Frees a memory block which has been allocated with av_malloc(z)() or
 * av_realloc() and set the pointer pointing to it to NULL.
 * @param ptr Pointer to the pointer to the memory block which should
 * be freed.
 * @see av_free()
 *)
procedure av_freep (ptr: pointer);
  cdecl; external av__util;

(* libavutil/log.h *)

const
{$IF LIBAVUTIL_VERSION_MAJOR < 50}
  AV_LOG_QUIET   = -1;
  AV_LOG_FATAL   =  0;
  AV_LOG_ERROR   =  0;
  AV_LOG_WARNING =  1;
  AV_LOG_INFO    =  1;
  AV_LOG_VERBOSE =  1;
  AV_LOG_DEBUG   =  2;
{$ELSE}
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
{$IFEND}

function av_log_get_level(): cint;
  cdecl; external av__util;
procedure av_log_set_level(level: cint);
  cdecl; external av__util;


implementation

(* libavutil/common.h *)

function MKTAG(a, b, c, d: AnsiChar): integer;
begin
  Result := (ord(a) or (ord(b) shl 8) or (ord(c) shl 16) or (ord(d) shl 24));
end;

end.
