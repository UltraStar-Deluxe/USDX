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

{$INCLUDE libavutil/cpu.pas}

{$INCLUDE libavutil/dict.pas}

{$INCLUDE libavutil/error.pas}

{$INCLUDE libavutil/mathematics.pas}

{$INCLUDE libavutil/mem.pas}

{$INCLUDE libavutil/opt.pas}

{$INCLUDE libavutil/log.pas}

{$INCLUDE libavutil/pixfmt.pas}

{$INCLUDE libavutil/samplefmt.pas}

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
