(*
 * Copyright (C) 2001-2011 Michael Niedermayer <michaelni@gmx.at>
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
 * FFmpeg Pascal port
 * - Ported by the UltraStar Deluxe Team
 *
 * Conversion of libswscale/swscale.h
 * version: 3.0.100
 *)
 
unit swscale;

{$IFDEF FPC}
  {$MODE DELPHI }
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
  {$MACRO ON}      (* Turn macro support on *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

{$I switches.inc}  (* for ffmpeg defines *)
{$I ff_api-defines.inc}  (* FF_API_* defines *)

{$IFDEF DARWIN}
  {$linklib libswscale}
{$ENDIF}

interface

uses
  ctypes,
  avutil,
  avcodec,
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

  (* Max. supported version by this header *)
  LIBSWSCALE_MAX_VERSION_MAJOR   =  3;
  LIBSWSCALE_MAX_VERSION_MINOR   =  1;
  LIBSWSCALE_MAX_VERSION_RELEASE =  101;
  LIBSWSCALE_MAX_VERSION = (LIBSWSCALE_MAX_VERSION_MAJOR * VERSION_MAJOR) +
                           (LIBSWSCALE_MAX_VERSION_MINOR * VERSION_MINOR) +
			   (LIBSWSCALE_MAX_VERSION_RELEASE * VERSION_RELEASE);

  

(* Check if linked versions are supported *)
{$IF (LIBSWSCALE_VERSION > LIBSWSCALE_MAX_VERSION)}
  {$MESSAGE Error 'Linked version of libswscale is not yet supported!'}
{$IFEND}

type
  TQuadCintArray = array[0..3] of cint;
  PQuadCintArray = ^TQuadCintArray;
  TCintArray = array[0..0] of cint;
  PCintArray = ^TCintArray;
  TPCuint8Array = array[0..0] of PCuint8;
  PPCuint8Array = ^TPCuint8Array;

(* libswscale/version.h start *)

(**
 * FF_API_* defines may be placed below to indicate public API that will be
 * dropped at a future version bump. The defines themselves are not part of
 * the public API and may change, break or disappear at any time.
 *)
{$ifndef FF_API_SWS_GETCONTEXT}
{$define FF_API_SWS_GETCONTEXT        := (LIBSWSCALE_VERSION_MAJOR < 3)}
{$endif}
{$ifndef FF_API_SWS_CPU_CAPS}
{$define FF_API_SWS_CPU_CAPS          := (LIBSWSCALE_VERSION_MAJOR < 3)}
{$endif}
{$ifndef FF_API_SWS_FORMAT_NAME}
{$define FF_API_SWS_FORMAT_NAME       := (LIBSWSCALE_VERSION_MAJOR < 3)}
{$endif}

(* libswscale/version.h end *)

(**
 * Return the LIBSWSCALE_VERSION_INT constant.
 *)
function swscale_version(): cuint;
  cdecl; external sw__scale;

(**
 * Return the libswscale build-time configuration.
 *)
function swscale_configuration(): PAnsiChar;
  cdecl; external sw__scale;

(**
 * Return the libswscale license.
 *)
function swscale_license(): PAnsiChar;
  cdecl; external sw__scale;

const
  (* values for the flags, the stuff on the command line is different *)
  SWS_FAST_BILINEAR =    1;
  SWS_BILINEAR      =    2;
  SWS_BICUBIC       =    4;
  SWS_X             =    8;
  SWS_POINT         =  $10;
  SWS_AREA          =  $20;
  SWS_BICUBLIN      =  $40;
  SWS_GAUSS         =  $80;
  SWS_SINC          = $100;
  SWS_LANCZOS       = $200;
  SWS_SPLINE        = $400;

  SWS_SRC_V_CHR_DROP_MASK  = $30000;
  SWS_SRC_V_CHR_DROP_SHIFT = 16;

  SWS_PARAM_DEFAULT        = 123456;

  SWS_PRINT_INFO           = $1000;

  // the following 3 flags are not completely implemented
  // internal chrominace subsampling info
  SWS_FULL_CHR_H_INT    = $2000;
  // input subsampling info
  SWS_FULL_CHR_H_INP    = $4000;
  SWS_DIRECT_BGR        = $8000;
  SWS_ACCURATE_RND      = $40000;
  SWS_BITEXACT          = $80000;

{$IFDEF FF_API_SWS_CPU_CAPS}
(**
 * CPU caps are autodetected now, those flags
 * are only provided for API compatibility.
 *)
  SWS_CPU_CAPS_MMX      = $80000000;
  SWS_CPU_CAPS_MMXEXT   = $20000000;
  SWS_CPU_CAPS_MMX2     = $20000000;
  SWS_CPU_CAPS_3DNOW    = $40000000;
  SWS_CPU_CAPS_ALTIVEC  = $10000000;
{$IFDEF  FF_API_ARCH_BFIN}
  SWS_CPU_CAPS_BFIN     = $01000000;
{$IFEND}
  SWS_CPU_CAPS_SSE2     = $02000000;
{$IFEND}

  SWS_MAX_REDUCE_CUTOFF = 0.002;

  SWS_CS_ITU709         = 1;
  SWS_CS_FCC            = 4;
  SWS_CS_ITU601         = 5;
  SWS_CS_ITU624         = 5;
  SWS_CS_SMPTE170M      = 5;
  SWS_CS_SMPTE240M      = 7;
  SWS_CS_DEFAULT        = 5;

(**
 * Return a pointer to yuv<->rgb coefficients for the given colorspace
 * suitable for sws_setColorspaceDetails().
 *
 * @param colorspace One of the SWS_CS_* macros. If invalid,
 * SWS_CS_DEFAULT is used.
 *)
function sws_getCoefficients(colorspace: cint): Pcint;
  cdecl; external sw__scale;
  
type

  // when used for filters they must have an odd number of elements
  // coeffs cannot be shared between vectors
  PSwsVector = ^TSwsVector;
  TSwsVector = record
    coeff: PCdouble;    // pointer to the list of coefficients
    length: cint;       // number of coefficients in the vector
  end;

  // vectors can be shared
  PSwsFilter = ^TSwsFilter;
  TSwsFilter = record
    lumH: PSwsVector;
    lumV: PSwsVector;
    chrH: PSwsVector;
    chrV: PSwsVector;
  end;

  PSwsContext = ^TSwsContext;
  TSwsContext = record
    {internal structure}
  end;

(**
 * Return a positive value if pix_fmt is a supported input format, 0
 * otherwise.
 *)
function sws_isSupportedInput(pix_fmt: TAVPixelFormat): cint;
  cdecl; external sw__scale;

(**
 * Return a positive value if pix_fmt is a supported output format, 0
 * otherwise.
 *)
function sws_isSupportedOutput(pix_fmt: TAVPixelFormat): cint;
  cdecl; external sw__scale;

(**
 * @param[in]  pix_fmt the pixel format
 * @return a positive value if an endianness conversion for pix_fmt is
 * supported, 0 otherwise.
 *)
function sws_isSupportedEndiannessConversion(pix_fmt: TAVPixelFormat): cint;
  cdecl; external sw__scale;

(**
 * Allocate an empty SwsContext. This must be filled and passed to
 * sws_init_context(). For filling see AVOptions, options.c and
 * sws_setColorspaceDetails().
 *)
function sws_alloc_context(): PSwsContext;
  cdecl; external sw__scale;

(**
 * Initialize the swscaler context sws_context.
 *
 * @return zero or positive value on success, a negative value on
 * error
 *)
function sws_init_context(sws_context: PSwsContext; srcFilter: PSwsFilter; dstFilter: PSwsFilter): cint;
  cdecl; external sw__scale;

(**
 * Free the swscaler context swsContext.
 * If swsContext is NULL, then does nothing.
 *)
procedure sws_freeContext(swsContext: PSwsContext);
  cdecl; external sw__scale;

(**
 * Allocate and return a SwsContext. You need it to perform
 * scaling/conversion operations using sws_scale().
 *
 * @param srcW the width of the source image
 * @param srcH the height of the source image
 * @param srcFormat the source image format
 * @param dstW the width of the destination image
 * @param dstH the height of the destination image
 * @param dstFormat the destination image format
 * @param flags specify which algorithm and options to use for rescaling
 * @return a pointer to an allocated context, or NULL in case of error
 * @note this function is to be removed after a saner alternative is
 *       written
 *)
function sws_getContext(srcW: cint; srcH: cint; srcFormat: TAVPixelFormat;
                        dstW: cint; dstH: cint; dstFormat: TAVPixelFormat;
                        flags: cint; srcFilter: PSwsFilter;
                        dstFilter: PSwsFilter; param: {const} PCdouble): PSwsContext;
  cdecl; external sw__scale;

(**
 * Scale the image slice in srcSlice and put the resulting scaled
 * slice in the image in dst. A slice is a sequence of consecutive
 * rows in an image.
 *
 * Slices have to be provided in sequential order, either in
 * top-bottom or bottom-top order. If slices are provided in
 * non-sequential order the behavior of the function is undefined.
 *
 * @param c         the scaling context previously created with
 *                  sws_getContext()
 * @param srcSlice  the array containing the pointers to the planes of
 *                  the source slice
 * @param srcStride the array containing the strides for each plane of
 *                  the source image
 * @param srcSliceY the position in the source image of the slice to
 *                  process, that is the number (counted starting from
 *                  zero) in the image of the first row of the slice
 * @param srcSliceH the height of the source slice, that is the number
 *                  of rows in the slice
 * @param dst       the array containing the pointers to the planes of
 *                  the destination image
 * @param dstStride the array containing the strides for each plane of
 *                  the destination image
 * @return          the height of the output slice
 *)
function sws_scale(c: PSwsContext; {const} srcSlice: PPCuint8Array;
              {const} srcStride: PCintArray; srcSliceY: cint; srcSliceH: cint; 
              {const} dst: PPCuint8Array; {const} dstStride: PCintArray): cint;
  cdecl; external sw__scale;

(**
 * @param dstRange flag indicating the while-black range of the output (1=jpeg / 0=mpeg)
 * @param srcRange flag indicating the while-black range of the input (1=jpeg / 0=mpeg)
 * @param table the yuv2rgb coefficients describing the output yuv space, normally ff_yuv2rgb_coeffs[x]
 * @param inv_table the yuv2rgb coefficients describing the input yuv space, normally ff_yuv2rgb_coeffs[x]
 * @param brightness 16.16 fixed point brightness correction
 * @param contrast 16.16 fixed point contrast correction
 * @param saturation 16.16 fixed point saturation correction
 * @return -1 if not supported
 *)
function sws_setColorspaceDetails(c: PSwsContext; inv_table: PQuadCintArray; 
              srcRange: cint; table: PQuadCintArray; dstRange: cint;
              brightness: cint; contrast: cint; saturation: cint): cint;
  cdecl; external sw__scale;

(**
 * @return -1 if not supported
 *)
function sws_getColorspaceDetails(c: PSwsContext; var inv_table: PQuadCintArray;
              var srcRange: cint; var table: PQuadCintArray; var dstRange: cint;
              var brightness: cint; var contrast: cint; var saturation: cint): cint;
  cdecl; external sw__scale;

(**
 * Allocate and return an uninitialized vector with length coefficients.
 *)
function sws_allocVec(length: cint): PSwsVector;
  cdecl; external sw__scale;

(**
 * Return a normalized Gaussian curve used to filter stuff
 * quality = 3 is high quality, lower is lower quality.
 *)
function sws_getGaussianVec(variance: cdouble; quality: cdouble): PSwsVector;
  cdecl; external sw__scale;

(**
 * Allocate and return a vector with length coefficients, all
 * with the same value c.
 *)
function sws_getConstVec(c: cdouble; length: cint): PSwsVector;
  cdecl; external sw__scale;
  
(**
 * Allocate and return a vector with just one coefficient, with
 * value 1.0.
 *)
function sws_getIdentityVec: PSwsVector;
  cdecl; external sw__scale;

(**
 * Scale all the coefficients of a by the scalar value.
 *)
procedure sws_scaleVec(a: PSwsVector; scalar: cdouble);
  cdecl; external sw__scale;

(**
 * Scale all the coefficients of a so that their sum equals height.
 *)
procedure sws_normalizeVec(a: PSwsVector; height: cdouble);
  cdecl; external sw__scale;

procedure sws_convVec(a: PSwsVector; b: PSwsVector);
  cdecl; external sw__scale;

procedure sws_addVec(a: PSwsVector; b: PSwsVector);
  cdecl; external sw__scale;

procedure sws_subVec(a: PSwsVector; b: PSwsVector);
  cdecl; external sw__scale;

procedure sws_shiftVec(a: PSwsVector; shift: cint);
  cdecl; external sw__scale;

(**
 * Allocate and return a clone of the vector a, that is a vector
 * with the same coefficients as a.
 *)
function sws_cloneVec(a: PSwsVector): PSwsVector;
  cdecl; external sw__scale;

(**
 * Print with av_log() a textual representation of the vector a
 * if log_level <= av_log_level.
 *)
procedure sws_printVec2(a:         PSwsVector;
                        log_ctx:   PAVClass; // PAVClass is declared in avcodec.pas
                        log_level: cint);
  cdecl; external sw__scale;

procedure sws_freeVec(a: PSwsVector);
  cdecl; external sw__scale;

function sws_getDefaultFilter(lumaGBlur: cfloat; chromaGBlur: cfloat; 
                              lumaSharpen: cfloat; chromaSharpen: cfloat;
                              chromaHShift: cfloat; chromaVShift: cfloat;
                              verbose: cint): PSwsFilter;
  cdecl; external sw__scale;

procedure sws_freeFilter(filter: PSwsFilter);
  cdecl; external sw__scale;

(**
 * Check if context can be reused, otherwise reallocates a new one.
 * 
 * If context is NULL, just calls sws_getContext() to get a new
 * context. Otherwise, checks if the parameters are the ones already
 * saved in context. If that is the case, returns the current
 * context. Otherwise, frees context and gets a new context with
 * the new parameters.
 * 
 * Be warned that srcFilter and dstFilter are not checked, they
 * are assumed to remain the same.
 *)
function sws_getCachedContext(context: PSwsContext;
              srcW: cint; srcH: cint; srcFormat: TAVPixelFormat;
              dstW: cint; dstH: cint; dstFormat: TAVPixelFormat;
              flags: cint; srcFilter: PSwsFilter; 
              dstFilter: PSwsFilter; param: PCdouble): PSwsContext;
  cdecl; external sw__scale;

(**
 * Convert an 8bit paletted frame into a frame with a color depth of 32-bits.
 *
 * The output frame will have the same packed format as the palette.
 *
 * @param src        source frame buffer
 * @param dst        destination frame buffer
 * @param num_pixels number of pixels to convert
 * @param palette    array with [256] entries, which must match color arrangement (RGB or BGR) of src
 *)
procedure sws_convertPalette8ToPacked32({const} src:     PPCuint8Array;
                                        dst:             PPCuint8Array;
                                        num_pixels:      cint;
                                        {const} palette: PPCuint8Array);
  cdecl; external sw__scale;

(**
 * Convert an 8bit paletted frame into a frame with a color depth of 24 bits.
 *
 * With the palette format "ABCD", the destination frame ends up with the format "ABC".
 *
 * @param src        source frame buffer
 * @param dst        destination frame buffer
 * @param num_pixels number of pixels to convert
 * @param palette    array with [256] entries, which must match color arrangement (RGB or BGR) of src
 *)
procedure sws_convertPalette8ToPacked24({const} src:     PPCuint8Array;
                                        dst:             PPCuint8Array;
                                        num_pixels:      cint;
                                        {const} palette: PPCuint8Array);
  cdecl; external sw__scale;

(**
 * Get the AVClass for swsContext. It can be used in combination with
 * AV_OPT_SEARCH_FAKE_OBJ for examining options.
 *
 * @see av_opt_find().
 *)
function sws_get_class(): {const} PAVClass;
  cdecl; external sw__scale;

implementation

end.
