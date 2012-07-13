(*
 * copyright (c) 2001 Fabrice Bellard
 *
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
 * This is a part of Pascal porting of ffmpeg.
 * - Originally by Victor Zinetz for Delphi and Free Pascal on Windows.
 * - For Mac OS X, some modifications were made by The Creative CAT, denoted as CAT
 *   in the source codes.
 * - Changes and updates by the UltraStar Deluxe Team
 *
 * Conversion of libavformat/avio.h
 * unbuffered I/O operations
 * @warning This file has to be considered an internal but installed
 * header, so it should not be directly included in your projects.
 *
 * update to
 * avformat version: 52.110.0
 *)

unit avio;

{$IFDEF FPC}
  {$MODE DELPHI }
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

{$I switches.inc}

interface

uses
  ctypes,
  {$IFDEF UNIX}
  BaseUnix,   // for SEEK_CUR
  {$ENDIF}
  avcodec,
  avutil,
  SysUtils,
  UConfig;

(**
 * Those FF_API_* defines are not part of public API.
 * They may change, break or disappear at any time.
 *)
const
  FF_API_MAX_STREAMS          = (LIBAVFORMAT_VERSION_MAJOR < 53);
  FF_API_OLD_METADATA         = (LIBAVFORMAT_VERSION_MAJOR < 53);
  FF_API_OLD_METADATA2        = (LIBAVFORMAT_VERSION_MAJOR < 54);
  FF_API_URL_CLASS            = (LIBAVFORMAT_VERSION_MAJOR >= 53);
  FF_API_URL_RESETBUF         = (LIBAVFORMAT_VERSION_MAJOR < 53);
  FF_API_REGISTER_PROTOCOL    = (LIBAVFORMAT_VERSION_MAJOR < 53);
  FF_API_GUESS_FORMAT         = (LIBAVFORMAT_VERSION_MAJOR < 53);
  FF_API_UDP_GET_FILE         = (LIBAVFORMAT_VERSION_MAJOR < 53);
  FF_API_URL_SPLIT            = (LIBAVFORMAT_VERSION_MAJOR < 53);
  FF_API_ALLOC_FORMAT_CONTEXT = (LIBAVFORMAT_VERSION_MAJOR < 53);
  FF_API_PARSE_FRAME_PARAM    = (LIBAVFORMAT_VERSION_MAJOR < 53);
  FF_API_READ_SEEK            = (LIBAVFORMAT_VERSION_MAJOR < 54);
  FF_API_LAVF_UNUSED          = (LIBAVFORMAT_VERSION_MAJOR < 53);
  FF_API_PARAMETERS_CODEC_ID  = (LIBAVFORMAT_VERSION_MAJOR < 53);
  FF_API_FIRST_FORMAT         = (LIBAVFORMAT_VERSION_MAJOR < 53);
  FF_API_SYMVER               = (LIBAVFORMAT_VERSION_MAJOR < 53);
  FF_API_OLD_AVIO             = (LIBAVFORMAT_VERSION_MAJOR < 54);
  FF_API_INDEX_BUILT          = (LIBAVFORMAT_VERSION_MAJOR < 53);
  FF_API_DUMP_FORMAT          = (LIBAVFORMAT_VERSION_MAJOR < 54);
  FF_API_PARSE_DATE           = (LIBAVFORMAT_VERSION_MAJOR < 54);
  FF_API_FIND_INFO_TAG        = (LIBAVFORMAT_VERSION_MAJOR < 54);
  FF_API_PKT_DUMP             = (LIBAVFORMAT_VERSION_MAJOR < 54);
  FF_API_GUESS_IMG2_CODEC     = (LIBAVFORMAT_VERSION_MAJOR < 54);
  FF_API_SDP_CREATE           = (LIBAVFORMAT_VERSION_MAJOR < 54);
  FF_API_ALLOC_OUTPUT_CONTEXT = (LIBAVFORMAT_VERSION_MAJOR < 54);
  FF_API_FORMAT_PARAMETERS    = (LIBAVFORMAT_VERSION_MAJOR < 54);
  FF_API_FLAG_RTP_HINT        = (LIBAVFORMAT_VERSION_MAJOR < 54);

const
  AVIO_SEEKABLE_NORMAL = 0001; (**< Seeking works like for a local file *)
  URL_PROTOCOL_FLAG_NESTED_SCHEME = 1; (*< The protocol name can be the first part of a nested protocol scheme *)

type
  TReadWriteFunc = function(opaque: Pointer; buf: PByteArray; buf_size: cint): cint; cdecl;
  TSeekFunc = function(opaque: Pointer; offset: cint64; whence: cint): cint64; cdecl;
  Tcallback = function(p: pointer): cint; cdecl;

type
(**
 * Callback for checking whether to abort blocking functions.
 * AVERROR_EXIT is returned in this case by the interrupted
 * function. During blocking operations, callback is called with
 * opaque as parameter. If the callback returns 1, the
 * blocking operation will be aborted.
 *
 * No members can be added to this struct without a major bump, if
 * new elements have been added after this struct in AVFormatContext
 * or AVIOContext.
 *)
  PAVIOInterruptCB = ^TAVIOInterruptCB;
  TAVIOInterruptCB = record
    callback: Tcallback;
    opaque: pointer;
  end; (*TAVIOInterruptCB*)

(**
 * Bytestream IO Context.
 * New fields can be added to the end with minor version bumps.
 * Removal, reordering and changes to existing fields require a major
 * version bump.
 * sizeof(AVIOContext) must not be used outside libav*.
 *
 * @note None of the function pointers in AVIOContext should be called
 *       directly, they should only be set by the client application
 *       when implementing custom I/O. Normally these are set to the
 *       function pointers specified in avio_alloc_context()
 *)
  PAVIOContext = ^TAVIOContext;
  TAVIOContext = record
{$IF FF_API_OLD_AVIO}
    (**
     * A class for private options.
     *
     * If this AVIOContext is created by avio_open2(), av_class is set and
     * passes the options down to protocols.
     *
     * If this AVIOContext is manually allocated, then av_class may be set by
     * the caller.
     *
     * warning -- this field can be NULL, be sure to not pass this AVIOContext
     * to any av_opt_* functions in that case.
     *)
    av_class: PAVClass;
{$IFEND}
    buffer: PByteArray;  (**< Start of the buffer. *)
    buffer_size: cint;   (**< Maximum buffer size *)
    buf_ptr: PByteArray; (**< Current position in the buffer *)
    buf_end: PByteArray; (**< End of the data, may be less than
                              buffer+buffer_size if the read function returned
                              less data than requested, e.g. for streams where
                              no more data has been received yet. *)
    opaque: pointer;     (**< A private pointer, passed to the read/write/seek/...
                              functions. *)
    read_packet: TReadWriteFunc;
    write_packet: TReadWriteFunc;
    seek: TSeekFunc;
    pos: cint64;         (**< position in the file of the current buffer *)
    must_flush: cint;    (**< true if the next seek should flush *)
    eof_reached: cint;   (**< true if eof reached *)
    write_flag: cint;    (**< true if open for writing *)
{$IF FF_API_OLD_AVIO}
    is_streamed: cint; { deprecated }
{$IFEND}
    max_packet_size: cint;
    checksum: culong;
    checksum_ptr: PByteArray;
    update_checksum: function (checksum: culong; buf: {const} PAnsiChar; size: cuint): culong; cdecl;
    error: cint;         (**< contains the error code or 0 if no error happened *)
    (**
     * Pause or resume playback for network streaming protocols - e.g. MMS.
     *)
    read_pause: function(opaque: Pointer; pause: cint): cint; cdecl;
    (**
     * Seek to a given timestamp in stream with the specified stream_index.
     * Needed for some network streaming protocols which don't support seeking
     * to byte position.
     *)
    read_seek: function(opaque: Pointer; stream_index: cint;
                        timestamp: cint64; flags: cint): cint64; cdecl;
    (**
     * A combination of AVIO_SEEKABLE_ flags or 0 when the stream is not seekable.
     *)
    seekable: cint;
  end;

(* unbuffered I/O *)

{$IF FF_API_OLD_AVIO}
  PURLProtocol = ^TURLProtocol;

(**
 * URL Context.
 * New fields can be added to the end with minor version bumps.
 * Removal, reordering and changes to existing fields require a major
 * version bump.
 * sizeof(URLContext) must not be used outside libav*.
 * @deprecated This struct will be made private
 *)
  PPURLContext = ^PURLContext;
  PURLContext = ^TURLContext;
  TURLContext = record
    av_class: {const} PAVClass; ///< information for av_log(). Set by url_open().
    prot: PURLProtocol;
    flags: cint;
    is_streamed: cint;  (**< true if streamed (no seek possible), default = false *)
    max_packet_size: cint;  (**< if non zero, the stream is packetized with this max packet size *)
    priv_data: pointer;
    filename: PAnsiChar; (**< specified URL *)
    is_connected: cint;
    interrupt_callback: TAVIOInterruptCB;
  end;

(**
 * @deprecated This struct is to be made private. Use the higher-level
 *             AVIOContext-based API instead.
 *)
  TURLProtocol = record
    name: PAnsiChar;
    url_open: function (h: PURLContext; url: {const} PAnsiChar; flags: cint): cint; cdecl;
    url_read: function (h: PURLContext; buf: PByteArray; size: cint): cint; cdecl;
    url_write: function (h: PURLContext; {const} buf: PByteArray; size: cint): cint; cdecl;
    url_seek: function (h: PURLContext; pos: cint64; whence: cint): cint64; cdecl;
    url_close: function (h: PURLContext): cint; cdecl;
    next: PURLProtocol;
    url_read_pause: function (h: PURLContext; pause: cint): cint; cdecl;
    url_read_seek: function (h: PURLContext; stream_index: cint;
                             timestamp: cint64; flags: cint): cint64; cdecl;
    url_get_file_handle: function (h: PURLContext): cint; cdecl;
    priv_data_size: cint;
    {const} priv_data_class: PAVClass;
    flags: cint;
    url_check: function (h: PURLContext; mask: cint): cint; cdecl;
  end;

  PURLPollEntry = ^TURLPollEntry;
  TURLPollEntry = record
    handle: PURLContext;
    events: cint;
    revents: cint;
  end;

(* not implemented *)
function url_poll(poll_table: PURLPollEntry; n: cint; timeout: cint): cint;
  cdecl; external av__format; deprecated;

const
(**
 * @defgroup open_modes URL open modes
 * The flags argument to url_open and cosins must be one of the following
 * constants, optionally ORed with other flags.
 * @
 *)
  URL_RDONLY = 1; (**< read-only *)
  URL_WRONLY = 2; (**< write-only *)
  URL_RDWR   = URL_RDONLY or URL_WRONLY; (**< read-write *)
(**
 * @
 *)

(**
 * Use non-blocking mode.
 * If this flag is set, operations on the context will return
 * AVERROR(EAGAIN) if they can not be performed immediately.
 * If this flag is not set, operations on the context will never return
 * AVERROR(EAGAIN).
 * Note that this flag does not affect the opening/connecting of the
 * context. Connecting a protocol will always block if necessary (e.g. on
 * network protocols) but never hang (e.g. on busy devices).
 * Warning: non-blocking protocols is work-in-progress; this flag may be
 * silently ignored.
 *)
  URL_FLAG_NONBLOCK = 4;

type
  PURLInterruptCB = ^TURLInterruptCB;
  TURLInterruptCB = function (): cint; cdecl;

{
var
  url_interrupt_cb: PURLInterruptCB; cvar; external: av__format;
}

(**
 * @defgroup old_url_funcs Old url_* functions
 * @deprecated use the buffered API based on AVIOContext instead
 * @{
 * @ingroup lavf_io
 *)
function url_open_protocol(puc: PPURLContext; up: PURLProtocol;
                           url: {const} PAnsiChar; flags: cint): cint;
  cdecl; external av__format; deprecated;
function url_alloc(h: PPURLContext; {const} url: PAnsiChar; flags: cint): cint;
  cdecl; external av__format; deprecated;
function url_connect(h: PURLContext): cint;
  cdecl; external av__format; deprecated;
function url_open(h: PPointer; url: {const} PAnsiChar; flags: cint): cint;
  cdecl; external av__format; deprecated;
function url_read (h: PURLContext; buf: PByteArray; size: cint): cint;
  cdecl; external av__format; deprecated;
function url_read_complete (h: PURLContext; buf: PByteArray; size: cint): cint;
  cdecl; external av__format; deprecated;
function url_write (h: PURLContext; {const} buf: PByteArray; size: cint): cint;
  cdecl; external av__format; deprecated;
function url_seek (h: PURLContext; pos: cint64; whence: cint): cint64;
  cdecl; external av__format; deprecated;
function url_close (h: PURLContext): cint;
  cdecl; external av__format; deprecated;
function url_filesize (h: PURLContext): cint64;
  cdecl; external av__format; deprecated;
function url_get_file_handle(h: PURLContext): cint;
  cdecl; external av__format; deprecated;
function url_get_max_packet_size(h: PURLContext): cint;
  cdecl; external av__format; deprecated;
procedure url_get_filename(h: PURLContext; buf: PAnsiChar; buf_size: cint);
  cdecl; external av__format; deprecated;
function av_url_read_pause(h: PURLContext; pause: cint): cint;
  cdecl; external av__format; deprecated;
function av_url_read_seek(h: PURLContext; stream_index: cint;
                          timestamp: cint64; flags: cint): cint64;
  cdecl; external av__format; deprecated;
procedure url_set_interrupt_cb (interrupt_cb: TURLInterruptCB);
  cdecl; external av__format; deprecated;

(**
 * returns the next registered protocol after the given protocol (the first if
 * NULL is given), or NULL if protocol is the last one.
 *)
function av_protocol_next(p: PURLProtocol): PURLProtocol;
  cdecl; external av__format;

(**
 * Register the URLProtocol protocol.
 *
 * @param size the size of the URLProtocol struct referenced
 *)
function av_register_protocol2(protocol: PURLProtocol; size: cint): cint;
  cdecl; external av__format; deprecated;
(**
 * @
 *)

type
  PByteIOContext = ^TByteIOContext;
  TByteIOContext = TAVIOContext; { deprecated }

function init_put_byte(s: PAVIOContext;
                buffer: PByteArray;
                buffer_size: cint;
		write_flag: cint;
                opaque: pointer;
                read_packet: TReadWriteFunc;
                write_packet: TReadWriteFunc;
                seek: TSeekFunc): cint;
  cdecl; external av__format; deprecated;
function av_alloc_put_byte(
                  buffer: PByteArray;
                  buffer_size: cint;
                  write_flag: cint;
                  opaque: Pointer;
                  read_packet: TReadWriteFunc;
                  write_packet: TReadWriteFunc;
                  seek: TSeekFunc): PAVIOContext;
  cdecl; external av__format; deprecated;

(**
 * @defgroup old_avio_funcs Old put_/get_*() functions
 * @deprecated use the avio_ -prefixed functions instead.
 * @{
 *)
function get_buffer(s: PAVIOContext; buf: PByteArray; size: cint): cint;
  cdecl; external av__format; deprecated;
function get_partial_buffer(s: PAVIOContext; buf: PByteArray; size: cint): cint;
  cdecl; external av__format; deprecated;
function get_byte(s: PAVIOContext): cint;
  cdecl; external av__format; deprecated;
function get_le16(s: PAVIOContext): cuint;
  cdecl; external av__format; deprecated;
function get_le24(s: PAVIOContext): cuint;
  cdecl; external av__format; deprecated;
function get_le32(s: PAVIOContext): cuint;
  cdecl; external av__format; deprecated;
function get_le64(s: PAVIOContext): cuint64;
  cdecl; external av__format; deprecated;
function get_be16(s: PAVIOContext): cuint;
  cdecl; external av__format; deprecated;
function get_be24(s: PAVIOContext): cuint;
  cdecl; external av__format; deprecated;
function get_be32(s: PAVIOContext): cuint;
  cdecl; external av__format; deprecated;
function get_be64(s: PAVIOContext): cuint64;
  cdecl; external av__format; deprecated;

procedure put_byte(s: PAVIOContext; b: cint);
  cdecl; external av__format; deprecated;
procedure put_nbyte(s: PAVIOContext; b: cint; count: cint);
  cdecl; external av__format; deprecated;
procedure put_buffer (s: PAVIOContext; buf: {const} PByteArray; size: cint);
  cdecl; external av__format; deprecated;
procedure put_le64(s: PAVIOContext; val: cuint64);
  cdecl; external av__format; deprecated;
procedure put_be64(s: PAVIOContext; val: cuint64);
  cdecl; external av__format; deprecated;
procedure put_le32(s: PAVIOContext; val: cuint);
  cdecl; external av__format; deprecated;
procedure put_be32(s: PAVIOContext; val: cuint);
  cdecl; external av__format; deprecated;
procedure put_le24(s: PAVIOContext; val: cuint);
  cdecl; external av__format; deprecated;
procedure put_be24(s: PAVIOContext; val: cuint);
  cdecl; external av__format; deprecated;
procedure put_le16(s: PAVIOContext; val: cuint);
  cdecl; external av__format; deprecated;
procedure put_be16(s: PAVIOContext; val: cuint);
  cdecl; external av__format; deprecated;
procedure put_tag(s: PAVIOContext; tag: {const} PAnsiChar);
  cdecl; external av__format; deprecated;
(**
 * @
 *)

function av_url_read_fpause(h: PAVIOContext; pause: cint): cint;
  cdecl; external av__format; deprecated;
function av_url_read_fseek(h: PAVIOContext; stream_index: cint;
                           timestamp: cint64; flags: cint): cint64;
  cdecl; external av__format; deprecated;

(**
 * @defgroup old_url_f_funcs Old url_f* functions
 * @deprecated use the avio_ -prefixed functions instead.
 * @{
 *)
function url_fopen(var s: PAVIOContext; url: {const} PAnsiChar; flags: cint): cint;
  cdecl; external av__format; deprecated;
function url_fclose(s: PAVIOContext): cint;
  cdecl; external av__format; deprecated;
function url_fseek(s: PAVIOContext; offset: cint64; whence: cint): cint64;
  cdecl; external av__format; deprecated;
function url_fskip(s: PAVIOContext; offset: cint64): cint;
  cdecl; external av__format; deprecated;
function url_ftell(s: PAVIOContext): cint64;
  cdecl; external av__format; deprecated;
function url_fsize(s: PAVIOContext): cint64;
  cdecl; external av__format; deprecated;
const
  URL_EOF = -1;
(** @note return URL_EOF (-1) if EOF *)
function url_fgetc(s: PAVIOContext): cint;
  cdecl; external av__format; deprecated;
function url_setbufsize (s: PAVIOContext; buf_size: cint): cint;
  cdecl; external av__format; deprecated;
function url_fprintf(s: PAVIOContext; fmt: {const} PAnsiChar; args: array of const): cint;
  cdecl; external av__format; deprecated;
procedure put_flush_packet (s: PAVIOContext);
  cdecl; external av__format; deprecated;
function url_open_dyn_buf(var s: PAVIOContext): cint;
  cdecl; external av__format; deprecated;
function url_open_dyn_packet_buf(var s: PAVIOContext; max_packet_size: cint): cint;
  cdecl; external av__format; deprecated;
function url_close_dyn_buf(s: PAVIOContext; pbuffer:PPointer): cint;
  cdecl; external av__format; deprecated;
function url_fdopen (var s: PAVIOContext; h: PURLContext): cint;
  cdecl; external av__format; deprecated;
(**
 * @
 *)

function url_ferror(s: PAVIOContext): cint;
  cdecl; external av__format; deprecated;

function udp_set_remote_url(h: PURLContext; uri: {const} PAnsiChar): cint;
  cdecl; external av__format; deprecated;
function udp_get_local_port(h: PURLContext): cint;
  cdecl; external av__format; deprecated;

type
  Tupdate_checksum = function (c: culong; p: Pcuint8; len: cuint): culong; cdecl;
procedure init_checksum(s: PAVIOContext;
                        update_checksum: Tupdate_checksum;
			checksum: culong);
  cdecl; external av__format; deprecated;
function get_checksum(s: PAVIOContext): culong;
  cdecl; external av__format; deprecated;
procedure put_strz(s: PAVIOContext; buf: {const} PAnsiChar);
  cdecl; external av__format; deprecated;
(** @note unlike fgets, the EOL character is not returned and a whole
   line is parsed. return NULL if first char read was EOF *)
function url_fgets(s: PAVIOContext; buf: PAnsiChar; buf_size: cint): PAnsiChar;
  cdecl; external av__format; deprecated;
(**
 * @deprecated use avio_get_str instead
 *)
function get_strz(s: PAVIOContext; buf: PAnsiChar; maxlen: cint): PAnsiChar;
  cdecl; external av__format; deprecated;
(**
 * @deprecated Use AVIOContext.seekable field directly.
 *)
function url_is_streamed(s: PAVIOContext): cint; {$IFDEF HasInline}inline;{$ENDIF} deprecated;

function url_fileno(s: PAVIOContext): PURLContext;
  cdecl; external av__format; deprecated;

(**
 * @deprecated use AVIOContext.max_packet_size directly.
 *)
function url_fget_max_packet_size (s: PAVIOContext): cint;
  cdecl; external av__format; deprecated;

function url_open_buf(var s: PAVIOContext; buf: PAnsiChar; buf_size: cint; flags: cint): cint;
  cdecl; external av__format; deprecated;

(** return the written or read size *)
function url_close_buf(s: PAVIOContext): cint;
  cdecl; external av__format; deprecated;

(**
 * Return a non-zero value if the resource indicated by url
 * exists, 0 otherwise.
 * @deprecated Use avio_check instead.
 *)
function url_exist(url: {const} PAnsiChar): cint;
  cdecl; external av__format; deprecated;
{$IFEND} // FF_API_OLD_AVIO

(**
 * Return AVIO_FLAG_* access flags corresponding to the access permissions
 * of the resource in url, or a negative value corresponding to an
 * AVERROR code in case of failure. The returned access flags are
 * masked by the value in flags.
 *
 * @note This function is intrinsically unsafe, in the sense that the
 * checked resource may change its existence or permission status from
 * one call to another. Thus you should not trust the returned value,
 * unless you are sure that no other processes are accessing the
 * checked resource.
 *
 *)
function avio_check(url: {const} PAnsiChar; flags: cint): cint;
  cdecl; external av__format;

{$IF FF_API_OLD_INTERRUPT_CB}
(**
 * The callback is called in blocking functions to test regulary if
 * asynchronous interruption is needed. AVERROR_EXIT is returned
 * in this case by the interrupted function. 'NULL' means no interrupt
 * callback is given.
 * @deprecated Use interrupt_callback in AVFormatContext/avio_open2
 *             instead.
 *)
procedure avio_set_interrupt_cb(interrupt_cb: Pointer);
  cdecl; external av__format; deprecated;
{$IFEND}

(**
 * Allocate and initialize an AVIOContext for buffered I/O. It must be later
 * freed with av_free().
 *
 * @param buffer Memory block for input/output operations via AVIOContext.
 *        The buffer must be allocated with av_malloc() and friends.
 * @param buffer_size The buffer size is very important for performance.
 *        For protocols with fixed blocksize it should be set to this blocksize.
 *        For others a typical size is a cache page, e.g. 4kb.
 * @param write_flag Set to 1 if the buffer should be writable, 0 otherwise.
 * @param opaque An opaque pointer to user-specific data.
 * @param read_packet  A function for refilling the buffer, may be NULL.
 * @param write_packet A function for writing the buffer contents, may be NULL.
 * @param seek A function for seeking to specified byte position, may be NULL.
 *
 * @return Allocated AVIOContext or NULL on failure.
 *)
function avio_alloc_context(
                  buffer: PAnsiChar;
                  buffer_size: cint;
                  write_flag: cint;
                  opaque: Pointer;
                  read_packet: TReadWriteFunc;
                  write_packet: TReadWriteFunc;
                  seek: TSeekFunc): PAVIOContext;
  cdecl; external av__format;
		  
procedure avio_w8(s: PAVIOContext; b: cint);
  cdecl; external av__format;
procedure avio_write(s: PAVIOContext; buf: {const} PAnsiChar; size: cint);
  cdecl; external av__format;
procedure avio_wl64(s: PAVIOContext; val: cuint64);
  cdecl; external av__format;
procedure avio_wb64(s: PAVIOContext; val: cuint64);
  cdecl; external av__format;
procedure avio_wl32(s: PAVIOContext; val: cuint);
  cdecl; external av__format;
procedure avio_wb32(s: PAVIOContext; val: cuint);
  cdecl; external av__format;
procedure avio_wl24(s: PAVIOContext; val: cuint);
  cdecl; external av__format;
procedure avio_wb24(s: PAVIOContext; val: cuint);
  cdecl; external av__format;
procedure avio_wl16(s: PAVIOContext; val: cuint);
  cdecl; external av__format;
procedure avio_wb16(s: PAVIOContext; val: cuint);
  cdecl; external av__format;

(**
 * Write a NULL-terminated string.
 * @return number of bytes written.
 *)
function avio_put_str(s: PAVIOContext; str: {const} PAnsiChar): cint;
  cdecl; external av__format;

(**
 * Convert an UTF-8 string to UTF-16LE and write it.
 * @return number of bytes written.
 *)
function avio_put_str16le(s: PAVIOContext; str: {const} PAnsiChar): cint;
  cdecl; external av__format;

const
 (**
  * Passing this as the "whence" parameter to a seek function causes it to
  * return the filesize without seeking anywhere. Supporting this is optional.
  * If it is not supported then the seek function will return <0.
  *)
  AVSEEK_SIZE = $10000;

 (**
  * Oring this flag as into the "whence" parameter to a seek function causes it to
  * seek by any means (like reopening and linear reading) or other normally unreasonble
  * means that can be extreemly slow.
  * This may be ignored by the seek code.
  *)
  AVSEEK_FORCE = $20000;

(**
 * fseek() equivalent for AVIOContext.
 * @return new position or AVERROR.
 *)
function avio_seek(s: PAVIOContext; offset: cint64; whence: cint): cint64;
  cdecl; external av__format;

(**
 * Skip given number of bytes forward
 * @return new position or AVERROR.
 *)
function avio_skip(s: PAVIOContext; offset: cint64): cint64;
  cdecl; external av__format;

{$IFDEF UNIX}
(**
 * ftell() equivalent for AVIOContext.
 * @return position or AVERROR.
 *)
function avio_tell(s: PAVIOContext): cint64; {$IFDEF HasInline}inline;{$ENDIF}
{$ELSE}
{$ENDIF}

(**
 * Get the filesize.
 * @return filesize or AVERROR
 *)
function avio_size(s: PAVIOContext): cint64;
  cdecl; external av__format;

(**
 * feof() equivalent for AVIOContext.
 * @return non zero if and only if end of file
 *)
function url_feof(s: PAVIOContext): cint;
  cdecl; external av__format;
  
(** @warning currently size is limited *)
function avio_printf(s: PAVIOContext; fmt: {const} PAnsiChar; args: array of const): cint;
  cdecl; external av__format;

procedure avio_flush(s: PAVIOContext);
  cdecl; external av__format;

(**
 * Read size bytes from AVIOContext into buf.
 * @return number of bytes read or AVERROR
 *)
function avio_read(s: PAVIOContext; buf: Pbyte; size: cint): cint;
  cdecl; external av__format;

(**
 * @name Functions for reading from AVIOContext
 * @
 *
 * @note return 0 if EOF, so you cannot use it if EOF handling is
 *       necessary
 *)
function avio_r8(s: PAVIOContext): cint;
  cdecl; external av__format;

function avio_rl16(s: PAVIOContext): cuint;
  cdecl; external av__format;

function avio_rl24(s: PAVIOContext): cuint;
  cdecl; external av__format;

function avio_rl32(s: PAVIOContext): cuint;
  cdecl; external av__format;

function avio_rl64(s: PAVIOContext): cuint64;
  cdecl; external av__format;

function avio_rb16(s: PAVIOContext): cuint;
  cdecl; external av__format;

function avio_rb24(s: PAVIOContext): cuint;
  cdecl; external av__format;

function avio_rb32(s: PAVIOContext): cuint;
  cdecl; external av__format;

function avio_rb64(s: PAVIOContext): cuint64;
  cdecl; external av__format;

(**
 * @
 *)
(**
 * Read a string from pb into buf. The reading will terminate when either
 * a NULL character was encountered, maxlen bytes have been read, or nothing
 * more can be read from pb. The result is guaranteed to be NULL-terminated, it
 * will be truncated if buf is too small.
 * Note that the string is not interpreted or validated in any way, it
 * might get truncated in the middle of a sequence for multi-byte encodings.
 *
 * @return number of bytes read (is always <= maxlen).
 * If reading ends on EOF or error, the return value will be one more than
 * bytes actually read.
 *)
function avio_get_str(pb: PAVIOContext; maxlen: cint; buf: PAnsiChar; buflen: cint): cint;
  cdecl; external av__format;

(**
 * Read a UTF-16 string from pb and convert it to UTF-8.
 * The reading will terminate when either a null or invalid character was
 * encountered or maxlen bytes have been read.
 * @return number of bytes read (is always <= maxlen)
 *)
function avio_get_str16le(pb: PAVIOContext; maxlen: cint; buf: PAnsiChar; buflen: cint): cint;
  cdecl; external av__format;

function avio_get_str16be(pb: PAVIOContext; maxlen: cint; buf: PAnsiChar; buflen: cint): cint;
  cdecl; external av__format;

(**
 * @name URL open modes
 * The flags argument to avio_open must be one of the following
 * constants, optionally ORed with other flags.
 * @
 *)

const
  AVIO_FLAG_READ  = 1;    (**< read-only *)
  AVIO_FLAG_WRITE = 2;    (**< write-only *)
  AVIO_FLAG_READ_WRITE = AVIO_FLAG_READ or AVIO_FLAG_WRITE; (**< read-write *)
(**
 * @
 *)

(**
 * Use non-blocking mode.
 * If this flag is set, operations on the context will return
 * AVERROR(EAGAIN) if they can not be performed immediately.
 * If this flag is not set, operations on the context will never return
 * AVERROR(EAGAIN).
 * Note that this flag does not affect the opening/connecting of the
 * context. Connecting a protocol will always block if necessary (e.g. on
 * network protocols) but never hang (e.g. on busy devices).
 * Warning:  non-blocking protocols is work-in-progress; this flag may be
 * silently ignored.
 *)
const
  AVIO_FLAG_NONBLOCK = 8;    

(**
 * Create and initialize a AVIOContext for accessing the
 * resource indicated by url.
 * @note When the resource indicated by url has been opened in
 * read+write mode, the AVIOContext can be used only for writing.
 *
 * @param s Used to return the pointer to the created AVIOContext.
 * In case of failure the pointed to value is set to NULL.
 * @param flags flags which control how the resource indicated by url
 * is to be opened
 * @return 0 in case of success, a negative value corresponding to an
 * AVERROR code in case of failure
 *)
function avio_open(var s: PAVIOContext; url: {const} PAnsiChar; flags: cint): cint;
  cdecl; external av__format;

(**
 * Create and initialize a AVIOContext for accessing the
 * resource indicated by url.
 * @note When the resource indicated by url has been opened in
 * read+write mode, the AVIOContext can be used only for writing.
 *
 * @param s Used to return the pointer to the created AVIOContext.
 * In case of failure the pointed to value is set to NULL.
 * @param flags flags which control how the resource indicated by url
 * is to be opened
 * @param int_cb an interrupt callback to be used at the protocols level
 * @param options  A dictionary filled with protocol-private options. On return
 * this parameter will be destroyed and replaced with a dict containing options
 * that were not found. May be NULL.
 * @return 0 in case of success, a negative value corresponding to an
 * AVERROR code in case of failure
 *)
function avio_open2(s: {PPAVIOContext} pointer; {const} url: PAnsiChar; flags: cint;
	{const} int_cb: PAVIOInterruptCB; options: {PPAVDictionary} pointer): cint;
  cdecl; external av__format;

(**
 * Close the resource accessed by the AVIOContext s and free it.
 * This function can only be used if s was opened by avio_open().
 *
 * @return 0 on success, an AVERROR < 0 on error.
 *)
function avio_close(s: PAVIOContext): cint;
  cdecl; external av__format;

(**
 * Open a write only memory stream.
 *
 * @param s new IO context
 * @return zero if no error.
 *)
function avio_open_dyn_buf(var s: PAVIOContext): cint;
  cdecl; external av__format;

(**
 * Return the written size and a pointer to the buffer. The buffer
 * must be freed with av_free().
 * Padding of FF_INPUT_BUFFER_PADDING_SIZE is added to the buffer.
 *
 * @param s IO context
 * @param pbuffer pointer to a byte buffer
 * @return the length of the byte buffer
 *)
function avio_close_dyn_buf(s: PAVIOContext; var pbuffer: Pcuint8): cint;
  cdecl; external av__format;

(**
 * Iterate through names of available protocols.
 * @note it is recommanded to use av_protocol_next() instead of this
 *
 * @param opaque A private pointer representing current protocol.
 *        It must be a pointer to NULL on first iteration and will
 *        be updated by successive calls to avio_enum_protocols.
 * @param output If set to 1, iterate over output protocols,
 *               otherwise over input protocols.
 *
 * @return A static string containing the name of current protocol or NULL
 *)
function avio_enum_protocols(var opaque: Pointer; output: cint): {const} PAnsiChar;
  cdecl; external av__format;

(**
 * Pause and resume playing - only meaningful if using a network streaming
 * protocol (e.g. MMS).
 * @param pause 1 for pause, 0 for resume
 *)
function avio_pause(h: PAVIOContext; pause: cint): cint;
  cdecl; external av__format;

(**
 * Seek to a given timestamp relative to some component stream.
 * Only meaningful if using a network streaming protocol (e.g. MMS.).
 * @param stream_index The stream index that the timestamp is relative to.
 *        If stream_index is (-1) the timestamp should be in AV_TIME_BASE
 *        units from the beginning of the presentation.
 *        If a stream_index >= 0 is used and the protocol does not support
 *        seeking based on component streams, the call will fail with ENOTSUP.
 * @param timestamp timestamp in AVStream.time_base units
 *        or if there is no stream specified then in AV_TIME_BASE units.
 * @param flags Optional combination of AVSEEK_FLAG_BACKWARD, AVSEEK_FLAG_BYTE
 *        and AVSEEK_FLAG_ANY. The protocol may silently ignore
 *        AVSEEK_FLAG_BACKWARD and AVSEEK_FLAG_ANY, but AVSEEK_FLAG_BYTE will
 *        fail with ENOTSUP if used and not supported.
 * @return >= 0 on success
 * @see AVInputFormat: : read_seek
 *)
function avio_seek_time(h: PAVIOContext; stream_index: cint;
                        timestamp: cint64; flags: cint): cint64;
  cdecl; external av__format;

implementation

{$IF (LIBAVFORMAT_VERSION_MAJOR < 54)}
function url_is_streamed(s: PAVIOContext): cint;
begin
  Result := s^.is_streamed;
end;
{$IFEND}

(**
 * For SEEK_CUR on Windows
 * values taken from stdio.h of C
 *)
{$IFNDEF SEEK_SET}
const
  SEEK_SET = 0;
{$ENDIF}

{$IFNDEF SEEK_CUR}
const
  SEEK_CUR = 1;
{$ENDIF}

{$IFNDEF SEEK_END}
const
  SEEK_END = 2;
{$ENDIF}

function avio_tell(s: PAVIOContext): cint64; {$IFDEF HasInline}inline;{$ENDIF}
begin
  Result := avio_seek(s, 0, SEEK_CUR);
end;

end.
