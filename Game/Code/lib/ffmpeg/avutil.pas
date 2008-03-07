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

(* This is a part of Pascal porting of ffmpeg.
 * Originally by Victor Zinetz for Delphi and Free Pascal on Windows.
 * For Mac OS X, some modifications were made by The Creative CAT, denoted as CAT
 * in the source codes *)

(*
 * avutil.h:
 *  Min. version: ?
 *  Max. version: revision 12246, Tue Feb 26 20:37:59 2008 UTC
 *
 * mem.h:
 *  revision 12218, Mon Feb 25 18:32:55 2008 UTC
 *)

unit avutil;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

interface

uses
  mathematics,
  rational,
  UConfig;

const
  (* Max. supported version by this header *)
  LIBAVUTIL_MAX_VERSION_MAJOR   = 49;
  LIBAVUTIL_MAX_VERSION_MINOR   = 6;
  LIBAVUTIL_MAX_VERSION_RELEASE = 0;
  LIBAVUTIL_MAX_VERSION = (LIBAVUTIL_MAX_VERSION_MAJOR * VERSION_MAJOR) +
                          (LIBAVUTIL_MAX_VERSION_MINOR * VERSION_MINOR) +
                          (LIBAVUTIL_MAX_VERSION_RELEASE * VERSION_RELEASE);

(* Check if linked versions are supported *)
{$IF (LIBAVUTIL_VERSION > LIBAVUTIL_MAX_VERSION)}
  {$MESSAGE Warn 'Linked version of libavutil may be unsupported!'}
{$IFEND}

{$IFDEF FPC}
  // check for version of FPC < 2.2.0
  {$IF (FPC_VERSION < 2) or ((FPC_VERSION = 2) and (FPC_RELEASE < 2))}
    type uint64 = QWord;
  {$IFEND}
{$ENDIF}

type
(**
 * Pixel format. Notes:
 *
 * PIX_FMT_RGB32 is handled in an endian-specific manner. A RGBA
 * color is put together as:
 *  (A << 24) | (R << 16) | (G << 8) | B
 * This is stored as BGRA on little endian CPU architectures and ARGB on
 * big endian CPUs.
 *
 * When the pixel format is palettized RGB (PIX_FMT_PAL8), the palettized
 * image data is stored in AVFrame.data[0]. The palette is transported in
 * AVFrame.data[1] and, is 1024 bytes long (256 4-byte entries) and is
 * formatted the same as in PIX_FMT_RGB32 described above (i.e., it is
 * also endian-specific). Note also that the individual RGB palette
 * components stored in AVFrame.data[1] should be in the range 0..255.
 * This is important as many custom PAL8 video codecs that were designed
 * to run on the IBM VGA graphics adapter use 6-bit palette components.
 *)

  PAVPixelFormat = ^TAVPixelFormat;
  TAVPixelFormat = (
    PIX_FMT_NONE= -1,
    PIX_FMT_YUV420P,   ///< Planar YUV 4:2:0, 12bpp, (1 Cr & Cb sample per 2x2 Y samples)
    PIX_FMT_YUYV422,   ///< Packed YUV 4:2:2, 16bpp, Y0 Cb Y1 Cr
    PIX_FMT_RGB24,     ///< Packed RGB 8:8:8, 24bpp, RGBRGB...
    PIX_FMT_BGR24,     ///< Packed RGB 8:8:8, 24bpp, BGRBGR...
    PIX_FMT_YUV422P,   ///< Planar YUV 4:2:2, 16bpp, (1 Cr & Cb sample per 2x1 Y samples)
    PIX_FMT_YUV444P,   ///< Planar YUV 4:4:4, 24bpp, (1 Cr & Cb sample per 1x1 Y samples)
    PIX_FMT_RGB32,     ///< Packed RGB 8:8:8, 32bpp, (msb)8A 8R 8G 8B(lsb), in cpu endianness
    PIX_FMT_YUV410P,   ///< Planar YUV 4:1:0,  9bpp, (1 Cr & Cb sample per 4x4 Y samples)
    PIX_FMT_YUV411P,   ///< Planar YUV 4:1:1, 12bpp, (1 Cr & Cb sample per 4x1 Y samples)
    PIX_FMT_RGB565,    ///< Packed RGB 5:6:5, 16bpp, (msb)   5R 6G 5B(lsb), in cpu endianness
    PIX_FMT_RGB555,    ///< Packed RGB 5:5:5, 16bpp, (msb)1A 5R 5G 5B(lsb), in cpu endianness most significant bit to 1
    PIX_FMT_GRAY8,     ///<        Y        ,  8bpp
    PIX_FMT_MONOWHITE, ///<        Y        ,  1bpp, 0 is white, 1 is black
    PIX_FMT_MONOBLACK, ///<        Y        ,  1bpp, 0 is black, 1 is white
    PIX_FMT_PAL8,      ///< 8 bit with PIX_FMT_RGB32 palette
    PIX_FMT_YUVJ420P,  ///< Planar YUV 4:2:0, 12bpp, full scale (jpeg)
    PIX_FMT_YUVJ422P,  ///< Planar YUV 4:2:2, 16bpp, full scale (jpeg)
    PIX_FMT_YUVJ444P,  ///< Planar YUV 4:4:4, 24bpp, full scale (jpeg)
    PIX_FMT_XVMC_MPEG2_MC,///< XVideo Motion Acceleration via common packet passing(xvmc_render.h)
    PIX_FMT_XVMC_MPEG2_IDCT,
    PIX_FMT_UYVY422,   ///< Packed YUV 4:2:2, 16bpp, Cb Y0 Cr Y1
    PIX_FMT_UYYVYY411, ///< Packed YUV 4:1:1, 12bpp, Cb Y0 Y1 Cr Y2 Y3
    PIX_FMT_BGR32,     ///< Packed RGB 8:8:8, 32bpp, (msb)8A 8B 8G 8R(lsb), in cpu endianness
    PIX_FMT_BGR565,    ///< Packed RGB 5:6:5, 16bpp, (msb)   5B 6G 5R(lsb), in cpu endianness
    PIX_FMT_BGR555,    ///< Packed RGB 5:5:5, 16bpp, (msb)1A 5B 5G 5R(lsb), in cpu endianness most significant bit to 1
    PIX_FMT_BGR8,      ///< Packed RGB 3:3:2,  8bpp, (msb)2B 3G 3R(lsb)
    PIX_FMT_BGR4,      ///< Packed RGB 1:2:1,  4bpp, (msb)1B 2G 1R(lsb)
    PIX_FMT_BGR4_BYTE, ///< Packed RGB 1:2:1,  8bpp, (msb)1B 2G 1R(lsb)
    PIX_FMT_RGB8,      ///< Packed RGB 3:3:2,  8bpp, (msb)2R 3G 3B(lsb)
    PIX_FMT_RGB4,      ///< Packed RGB 1:2:1,  4bpp, (msb)2R 3G 3B(lsb)
    PIX_FMT_RGB4_BYTE, ///< Packed RGB 1:2:1,  8bpp, (msb)2R 3G 3B(lsb)
    PIX_FMT_NV12,      ///< Planar YUV 4:2:0, 12bpp, 1 plane for Y and 1 for UV
    PIX_FMT_NV21,      ///< as above, but U and V bytes are swapped

    PIX_FMT_RGB32_1,   ///< Packed RGB 8:8:8, 32bpp, (msb)8R 8G 8B 8A(lsb), in cpu endianness
    PIX_FMT_BGR32_1,   ///< Packed RGB 8:8:8, 32bpp, (msb)8B 8G 8R 8A(lsb), in cpu endianness

    PIX_FMT_GRAY16BE,  ///<        Y        , 16bpp, big-endian
    PIX_FMT_GRAY16LE,  ///<        Y        , 16bpp, little-endian
    PIX_FMT_YUV440P,   ///< Planar YUV 4:4:0 (1 Cr & Cb sample per 1x2 Y samples)
    PIX_FMT_YUVJ440P,  ///< Planar YUV 4:4:0 full scale (jpeg)
    PIX_FMT_YUVA420P,  ///< Planar YUV 4:2:0, 20bpp, (1 Cr & Cb sample per 2x2 Y & A samples)
    PIX_FMT_NB         ///< number of pixel formats, DO NOT USE THIS if you want to link with shared libav* because the number of formats might differ between versions
  );

const
{$ifdef WORDS_BIGENDIAN}
  PIX_FMT_RGBA = PIX_FMT_RGB32_1;
  PIX_FMT_BGRA = PIX_FMT_BGR32_1;
  PIX_FMT_ARGB = PIX_FMT_RGB32;
  PIX_FMT_ABGR = PIX_FMT_BGR32;
  PIX_FMT_GRAY16 = PIX_FMT_GRAY16BE;
{$else}
  PIX_FMT_RGBA = PIX_FMT_BGR32;
  PIX_FMT_BGRA = PIX_FMT_RGB32;
  PIX_FMT_ARGB = PIX_FMT_BGR32_1;
  PIX_FMT_ABGR = PIX_FMT_RGB32_1;
  PIX_FMT_GRAY16 = PIX_FMT_GRAY16LE;
{$endif}

{$IF LIBAVUTIL_VERSION_MAJOR < 50} // 50.0.0
  PIX_FMT_UYVY411 = PIX_FMT_UYYVYY411;
  PIX_FMT_RGBA32  = PIX_FMT_RGB32;
  PIX_FMT_YUV422  = PIX_FMT_YUYV422;
{$IFEND}

(* mem.h *)

(**
 * Allocate a block of \p size bytes with alignment suitable for all
 * memory accesses (including vectors if available on the CPU).
 * @param size Size in bytes for the memory block to be allocated.
 * @return Pointer to the allocated block, NULL if it cannot allocate
 * it.
 * @see av_mallocz()
 *)
function av_malloc (size: cardinal): pointer;
  cdecl; external av__util;

(**
 * Allocate or reallocate a block of memory.
 * If \p ptr is NULL and \p size > 0, allocate a new block. If \p
 * size is zero, free the memory block pointed by \p ptr.
 * @param size Size in bytes for the memory block to be allocated or
 * reallocated.
 * @param ptr Pointer to a memory block already allocated with
 * av_malloc(z)() or av_realloc() or NULL.
 * @return Pointer to a newly reallocated block or NULL if it cannot
 * reallocate or the function is used to free the memory block.
 * @see av_fast_realloc()
 *)
function av_realloc (ptr: pointer; size: cardinal): pointer;
  cdecl; external av__util;

(**
 * Free a memory block which has been allocated with av_malloc(z)() or
 * av_realloc().
 * @param ptr Pointer to the memory block which should be freed.
 * @note ptr = NULL is explicitly allowed.
 * @note It is recommended that you use av_freep() instead.
 * @see av_freep()
 *)
procedure av_free (ptr: pointer);
  cdecl; external av__util;

(**
 * Allocate a block of \p size bytes with alignment suitable for all
 * memory accesses (including vectors if available on the CPU) and
 * set to zeroes all the bytes of the block.
 * @param size Size in bytes for the memory block to be allocated.
 * @return Pointer to the allocated block, NULL if it cannot allocate
 * it.
 * @see av_malloc()
 *)
function av_mallocz (size: cardinal): pointer;
  cdecl; external av__util;

(**
 * Duplicate the string \p s.
 * @param s String to be duplicated.
 * @return Pointer to a newly allocated string containing a
 * copy of \p s or NULL if it cannot be allocated.
 *)
function av_strdup({const} s: pchar): pchar;
  cdecl; external av__util;

(**
 * Free a memory block which has been allocated with av_malloc(z)() or
 * av_realloc() and set to NULL the pointer to it.
 * @param ptr Pointer to the pointer to the memory block which should
 * be freed.
 * @see av_free()
 *)
procedure av_freep (ptr: pointer);
  cdecl; external av__util;

implementation

end.
