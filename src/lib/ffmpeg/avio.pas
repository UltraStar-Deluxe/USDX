(*
 * unbuffered io for ffmpeg system
 * copyright (c) 2001 Fabrice Bellard
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
 *)

(*
 * This is a part of Pascal porting of ffmpeg.
 * - Originally by Victor Zinetz for Delphi and Free Pascal on Windows.
 * - For Mac OS X, some modifications were made by The Creative CAT, denoted as CAT
 *   in the source codes.
 * - Changes and updates by the UltraStar Deluxe Team
 *)

(*
 * Conversion of libavformat/avio.h
 * unbuffered I/O operations
 * @warning This file has to be considered an internal but installed
 * header, so it should not be directly included in your projects.
 *
 * update to
 * Max. avformat version: 52.62.0, revision 23004, Tue May 11 19:29:00 2010 CET 
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
  avutil,
  avcodec,
  SysUtils,
  UConfig;

(* unbuffered I/O *)

const
  URL_RDONLY = 0;
  URL_WRONLY = 1;
  URL_RDWR   = 2;

 (**
  * Passing this as the "whence" parameter to a seek function causes it to
  * return the filesize without seeking anywhere. Supporting this is optional.
  * If it is not supported then the seek function will return <0.
  *)
  AVSEEK_SIZE = $10000;

{$IF LIBAVFORMAT_VERSION >= 52056000} // 52.56.0
 (**
  * Oring this flag as into the "whence" parameter to a seek function causes it to
  * seek by any means (like reopening and linear reading) or other normally unreasonble
  * means that can be extreemly slow.
  * This may be ignored by the seek code.
  *)
  AVSEEK_FORCE = $20000;
{$IFEND}

type
  TURLInterruptCB = function (): cint; cdecl;

type
  PURLProtocol = ^TURLProtocol;

 (**
  * URL Context.
  * New fields can be added to the end with minor version bumps.
  * Removal, reordering and changes to existing fields require a major
  * version bump.
  * sizeof(URLContext) must not be used outside libav*.
  *)
  PURLContext = ^TURLContext;
  TURLContext = record
    {$IF LIBAVFORMAT_VERSION_MAJOR >= 53}
    av_class: {const} PAVClass; ///< information for av_log(). Set by url_open().
    {$IFEND}
    prot: PURLProtocol;
    flags: cint;
    is_streamed: cint;  (**< true if streamed (no seek possible), default = false *)
    max_packet_size: cint;  (**< if non zero, the stream is packetized with this max packet size *)
    priv_data: pointer;
    filename: PAnsiChar; (**< specified URL *)
  end;
  PPURLContext = ^PURLContext;

  PURLPollEntry = ^TURLPollEntry;
  TURLPollEntry = record
    handle: PURLContext;
    events: cint;
    revents: cint;
  end;

  TURLProtocol = record
    name: PAnsiChar;
{$IF LIBAVFORMAT_VERSION < 52047000} // 52.47.0
    url_open: function (h: PURLContext; filename: {const} PAnsiChar; flags: cint): cint; cdecl;
{$ELSE}
    url_open: function (h: PURLContext; url: {const} PAnsiChar; flags: cint): cint; cdecl;
{$IFEND}

(**
 * Reads up to size bytes from the resource accessed by h, and stores
 * the read bytes in buf.
 *
 * @return The number of bytes actually read, or a negative value
 * corresponding to an AVERROR code in case of error. A value of zero
 * indicates that it is not possible to read more from the accessed
 * resource (except if the value of the size argument is also zero).
 *)
    url_read: function (h: PURLContext; buf: PByteArray; size: cint): cint; cdecl;

(**
 * Read as many bytes as possible (up to size), calling the
 * read function multiple times if necessary.
 * Will also retry if the read function returns AVERROR(EAGAIN).
 * This makes special short-read handling in applications
 * unnecessary, if the return value is < size then it is
 * certain there was either an error or the end of file was reached.
 *)
    url_write: function (h: PURLContext; buf: PByteArray; size: cint): cint; cdecl;

(**
 * Changes the position that will be used by the next read/write
 * operation on the resource accessed by h.
 *
 * @param pos specifies the new position to set
 * @param whence specifies how pos should be interpreted, it must be
 * one of SEEK_SET (seek from the beginning), SEEK_CUR (seek from the
 * current position), SEEK_END (seek from the end), or AVSEEK_SIZE
 * (return the filesize of the requested resource, pos is ignored).
 * @return a negative value corresponding to an AVERROR code in case
 * of failure, or the resulting file position, measured in bytes from
 * the beginning of the file. You can use this feature together with
 * SEEK_CUR to read the current file position.
 *)
    url_seek: function (h: PURLContext; pos: cint64; whence: cint): cint64; cdecl;

    url_close: function (h: PURLContext): cint; cdecl;
    next: PURLProtocol;
    {$IF (LIBAVFORMAT_VERSION >= 52001000) and (LIBAVFORMAT_VERSION < 52004000)} // 52.1.0 .. 52.4.0
    url_read_play: function (h: PURLContext): cint; cdecl;
    url_read_pause: function (h: PURLContext): cint; cdecl;
    {$IFEND}
    {$IF LIBAVFORMAT_VERSION >= 52004000} // 52.4.0
    url_read_pause: function (h: PURLContext; pause: cint): cint; cdecl;
    {$IFEND}
    {$IF LIBAVFORMAT_VERSION >= 52001000} // 52.1.0
    url_read_seek: function (h: PURLContext; stream_index: cint;
                             timestamp: cint64; flags: cint): cint64; cdecl;
    {$IFEND}
    {$IF LIBAVFORMAT_VERSION >= 52031000} // 52.31.0
    url_get_file_handle: function (h: PURLContext): cint; cdecl;
    {$IFEND}
  end;

 (**
  * Bytestream IO Context.
  * New fields can be added to the end with minor version bumps.
  * Removal, reordering and changes to existing fields require a major
  * version bump.
  * sizeof(ByteIOContext) must not be used outside libav*.
  *)
  PByteIOContext = ^TByteIOContext;
  TByteIOContext = record
    buffer: PByteArray;
    buffer_size: cint;
    buf_ptr: PByteArray;
    buf_end: PByteArray;
    opaque: pointer;
    read_packet: function (opaque: pointer; buf: PByteArray; buf_size: cint): cint; cdecl;
    write_packet: function (opaque: pointer; buf: PByteArray; buf_size: cint): cint; cdecl;
    seek: function (opaque: pointer; offset: cint64; whence: cint): cint64; cdecl;
    pos: cint64; (* position in the file of the current buffer *)
    must_flush: cint; (* true if the next seek should flush *)
    eof_reached: cint; (* true if eof reached *)
    write_flag: cint;  (* true if open for writing *)
    is_streamed: cint;
    max_packet_size: cint;
    checksum: culong;
    checksum_ptr: PByteArray;
    update_checksum: function (checksum: culong; buf: {const} PByteArray; size: cuint): culong; cdecl;
    error: cint;         ///< contains the error code or 0 if no error happened
    {$IF (LIBAVFORMAT_VERSION >= 52001000) and (LIBAVFORMAT_VERSION < 52004000)} // 52.1.0 .. 52.4.0
    read_play: function(opaque: Pointer): cint; cdecl;
    read_pause: function(opaque: Pointer): cint; cdecl;
    {$IFEND}
    {$IF LIBAVFORMAT_VERSION >= 52004000} // 52.4.0
    read_pause: function(opaque: Pointer; pause: cint): cint; cdecl;
    {$IFEND}
    {$IF LIBAVFORMAT_VERSION >= 52001000} // 52.1.0
    read_seek: function(opaque: Pointer; stream_index: cint;
                        timestamp: cint64; flags: cint): cint64; cdecl;
    {$IFEND}
  end;


{$IF LIBAVFORMAT_VERSION >= 52021000} // 52.21.0
(**
 * Creates an URLContext for accessing to the resource indicated by
 * URL, and opens it using the URLProtocol up.
 *
 * @param puc pointer to the location where, in case of success, the
 * function puts the pointer to the created URLContext
 * @param flags flags which control how the resource indicated by URL
 * is to be opened
 * @return 0 in case of success, a negative value corresponding to an
 * AVERROR code in case of failure
 *)
function url_open_protocol(puc: PPURLContext; up: PURLProtocol;
{$IF LIBAVFORMAT_VERSION < 52047000} // 52.47.0
                           filename: {const} PAnsiChar; flags: cint): cint;
{$ELSE}
                           url: {const} PAnsiChar; flags: cint): cint;
{$IFEND}
  cdecl; external av__format;
{$IFEND}

(**
 * Creates an URLContext for accessing to the resource indicated by
 * url, and opens it.
 *
 * @param puc pointer to the location where, in case of success, the
 * function puts the pointer to the created URLContext
 * @param flags flags which control how the resource indicated by url
 * is to be opened
 * @return 0 in case of success, a negative value corresponding to an
 * AVERROR code in case of failure
 *)
{$IF LIBAVFORMAT_VERSION < 52047000} // 52.47.0
function url_open(h: PPointer; filename: {const} PAnsiChar; flags: cint): cint;
{$ELSE}
function url_open(h: PPointer; url: {const} PAnsiChar; flags: cint): cint;
{$IFEND}
  cdecl; external av__format;
function url_read (h: PURLContext; buf: PByteArray; size: cint): cint;
  cdecl; external av__format;
{$IF LIBAVFORMAT_VERSION >= 52034000} // 52.34.0
function url_read_complete (h: PURLContext; buf: PByteArray; size: cint): cint; cdecl;
  cdecl; external av__format;
{$IFEND}
function url_write (h: PURLContext; buf: PByteArray; size: cint): cint;
  cdecl; external av__format;
function url_seek (h: PURLContext; pos: cint64; whence: cint): cint64;
  cdecl; external av__format;

(**
 * Closes the resource accessed by the URLContext h, and frees the
 * memory used by it.
 *
 * @return a negative value if an error condition occurred, 0
 * otherwise
 *)
function url_close (h: PURLContext): cint;
  cdecl; external av__format;

(**
 * Returns a non-zero value if the resource indicated by url
 * exists, 0 otherwise.
 *)
{$IF LIBAVFORMAT_VERSION < 52047000} // 52.47.0
function url_exist(filename: {const} PAnsiChar): cint;
{$ELSE}
function url_exist(url: {const} PAnsiChar): cint;
{$IFEND}
  cdecl; external av__format;

function url_filesize (h: PURLContext): cint64;
  cdecl; external av__format;

(**
 * Return the file descriptor associated with this URL. For RTP, this
 * will return only the RTP file descriptor, not the RTCP file descriptor.
 * To get both, use rtp_get_file_handles().
 *
 * @return the file descriptor associated with this URL, or <0 on error.
 *)
(* not implemented *)
function url_get_file_handle(h: PURLContext): cint;
  cdecl; external av__format;

(**
 * Return the maximum packet size associated to packetized file
 * handle. If the file is not packetized (stream like HTTP or file on
 * disk), then 0 is returned.
 *
 * @param h file handle
 * @return maximum packet size in bytes
 *)
function url_get_max_packet_size(h: PURLContext): cint;
  cdecl; external av__format;
procedure url_get_filename(h: PURLContext; buf: PAnsiChar; buf_size: cint);
  cdecl; external av__format;

(**
 * The callback is called in blocking functions to test regulary if
 * asynchronous interruption is needed. AVERROR(EINTR) is returned
 * in this case by the interrupted function. 'NULL' means no interrupt
 * callback is given.
 *)
procedure url_set_interrupt_cb (interrupt_cb: TURLInterruptCB);
  cdecl; external av__format;

(* not implemented *)
function url_poll(poll_table: PURLPollEntry; n: cint; timeout: cint): cint;
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 52004000} // 52.4.0
(**
 * Pause and resume playing - only meaningful if using a network streaming
 * protocol (e.g. MMS).
 * @param pause 1 for pause, 0 for resume
 *)
function av_url_read_pause(h: PURLContext; pause: cint): cint;
  cdecl; external av__format;
{$IFEND}

{$IF LIBAVFORMAT_VERSION >= 52001000} // 52.1.0
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
 * @see AVInputFormat::read_seek
 *)
function av_url_read_seek(h: PURLContext; stream_index: cint;
                          timestamp: cint64; flags: cint): cint64;
  cdecl; external av__format;
{$IFEND}

(**
var
{$IF LIBAVFORMAT_VERSION_MAJOR < 53}
  first_protocol: PURLProtocol; external av__format;
{$IFEND}
  url_interrupt_cb: PURLInterruptCB; external av__format;
**)

{$IF LIBAVFORMAT_VERSION >= 52002000} // 52.2.0
(**
 * If protocol is NULL, returns the first registered protocol,
 * if protocol is non-NULL, returns the next registered protocol after protocol,
 * or NULL if protocol is the last one.
 *)
function av_protocol_next(p: PURLProtocol): PURLProtocol;
  cdecl; external av__format;
{$IFEND}

{$IF LIBAVFORMAT_VERSION <= 52028000} // 52.28.0
(**
 * Registers the URLProtocol protocol.
 *)
(**
 * @deprecated Use av_register_protocol() instead.
 *)
function register_protocol(protocol: PURLProtocol): cint;
  cdecl; external av__format;
(** Alias for register_protocol() *)
function av_register_protocol(protocol: PURLProtocol): cint;
  cdecl; external av__format name 'register_protocol';
{$ELSE}
function av_register_protocol(protocol: PURLProtocol): cint;
  cdecl; external av__format;
{$IFEND}

type
  TReadWriteFunc = function(opaque: Pointer; buf: PByteArray; buf_size: cint): cint; cdecl;
  TSeekFunc = function(opaque: Pointer; offset: cint64; whence: cint): cint64; cdecl;

function init_put_byte(s: PByteIOContext;
                buffer: PByteArray;
                buffer_size: cint; write_flag: cint;
                opaque: pointer;
                read_packet: TReadWriteFunc;
                write_packet: TReadWriteFunc;
                seek: TSeekFunc): cint;
  cdecl; external av__format;
{$IF LIBAVFORMAT_VERSION >= 52004000} // 52.4.0
function av_alloc_put_byte(
                  buffer: PByteArray;
                  buffer_size: cint;
                  write_flag: cint;
                  opaque: Pointer;
                  read_packet: TReadWriteFunc;
                  write_packet: TReadWriteFunc;
                  seek: TSeekFunc): PByteIOContext;
  cdecl; external av__format;
{$IFEND}

procedure put_byte(s: PByteIOContext; b: cint);
  cdecl; external av__format;
procedure put_buffer (s: PByteIOContext; buf: {const} PByteArray; size: cint);
  cdecl; external av__format;
procedure put_le64(s: PByteIOContext; val: cuint64);
  cdecl; external av__format;
procedure put_be64(s: PByteIOContext; val: cuint64);
  cdecl; external av__format;
procedure put_le32(s: PByteIOContext; val: cuint);
  cdecl; external av__format;
procedure put_be32(s: PByteIOContext; val: cuint);
  cdecl; external av__format;
procedure put_le24(s: PByteIOContext; val: cuint);
  cdecl; external av__format;
procedure put_be24(s: PByteIOContext; val: cuint);
  cdecl; external av__format;
procedure put_le16(s: PByteIOContext; val: cuint);
  cdecl; external av__format;
procedure put_be16(s: PByteIOContext; val: cuint);
  cdecl; external av__format;
procedure put_tag(s: PByteIOContext; tag: {const} PAnsiChar);
  cdecl; external av__format;

procedure put_strz(s: PByteIOContext; buf: {const} PAnsiChar);
  cdecl; external av__format;

(**
 * fseek() equivalent for ByteIOContext.
 * @return new position or AVERROR.
 *)
function url_fseek(s: PByteIOContext; offset: cint64; whence: cint): cint64;
  cdecl; external av__format;

(**
 * Skip given number of bytes forward.
 * @param offset number of bytes
 *)
procedure url_fskip(s: PByteIOContext; offset: cint64);
  cdecl; external av__format;

(**
 * ftell() equivalent for ByteIOContext.
 * @return position or AVERROR.
 *)
function url_ftell(s: PByteIOContext): cint64;
  cdecl; external av__format;

(**
 * Gets the filesize.
 * @return filesize or AVERROR
 *)
function url_fsize(s: PByteIOContext): cint64;
  cdecl; external av__format;

(**
 * feof() equivalent for ByteIOContext.
 * @return non zero if and only if end of file
 *)
function url_feof(s: PByteIOContext): cint;
  cdecl; external av__format;

function url_ferror(s: PByteIOContext): cint;
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 52004000} // 52.4.0
function av_url_read_fpause(h: PByteIOContext; pause: cint): cint;
  cdecl; external av__format;
{$IFEND}
{$IF LIBAVFORMAT_VERSION >= 52001000} // 52.1.0
function av_url_read_fseek(h: PByteIOContext; stream_index: cint;
                           timestamp: cint64; flags: cint): cint64;
  cdecl; external av__format;
{$IFEND}

const
  URL_EOF = -1;
(** @note return URL_EOF (-1) if EOF *)
function url_fgetc(s: PByteIOContext): cint;
  cdecl; external av__format;

(** @warning currently size is limited *)
function url_fprintf(s: PByteIOContext; fmt: {const} PAnsiChar; args: array of const): cint;
  cdecl; external av__format;

(** @note unlike fgets, the EOL character is not returned and a whole
   line is parsed. return NULL if first char read was EOF *)
function url_fgets(s: PByteIOContext; buf: PAnsiChar; buf_size: cint): PAnsiChar;
  cdecl; external av__format;

procedure put_flush_packet (s: PByteIOContext);
  cdecl; external av__format;

(**
 * Reads size bytes from ByteIOContext into buf.
 * @return number of bytes read or AVERROR
 *)
function get_buffer(s: PByteIOContext; buf: PByteArray; size: cint): cint;
  cdecl; external av__format;

(**
 * Reads size bytes from ByteIOContext into buf.
 * This reads at most 1 packet. If that is not enough fewer bytes will be
 * returned.
 * @return number of bytes read or AVERROR
 *)
function get_partial_buffer(s: PByteIOContext; buf: PByteArray; size: cint): cint;
  cdecl; external av__format;

(** @note return 0 if EOF, so you cannot use it if EOF handling is
   necessary *)
function get_byte(s: PByteIOContext): cint;
  cdecl; external av__format;
function get_le24(s: PByteIOContext): cuint;
  cdecl; external av__format;
function get_le32(s: PByteIOContext): cuint;
  cdecl; external av__format;
function get_le64(s: PByteIOContext): cuint64;
  cdecl; external av__format;
function get_le16(s: PByteIOContext): cuint;
  cdecl; external av__format;

function get_strz(s: PByteIOContext; buf: PAnsiChar; maxlen: cint): PAnsiChar;
  cdecl; external av__format;
function get_be16(s: PByteIOContext): cuint;
  cdecl; external av__format;
function get_be24(s: PByteIOContext): cuint;
  cdecl; external av__format;
function get_be32(s: PByteIOContext): cuint;
  cdecl; external av__format;
function get_be64(s: PByteIOContext): cuint64;
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 51017001} // 51.17.1
function ff_get_v(bc: PByteIOContext): cuint64;
  cdecl; external av__format;
{$IFEND}

function url_is_streamed(s: PByteIOContext): cint; {$IFDEF HasInline}inline;{$ENDIF}

(**
 * Creates and initializes a ByteIOContext for accessing the
 * resource referenced by the URLContext h.
 * @note When the URLContext h has been opened in read+write mode, the
 * ByteIOContext can be used only for writing.
 *
 * @param s Used to return the pointer to the created ByteIOContext.
 * In case of failure the pointed to value is set to NULL.
 * @return 0 in case of success, a negative value corresponding to an
 * AVERROR code in case of failure
 *)
{$IF LIBAVFORMAT_VERSION >= 52000000} // 52.0.0
function url_fdopen (var s: PByteIOContext; h: PURLContext): cint;
{$ELSE}
function url_fdopen (s: PByteIOContext; h: PURLContext): cint;
{$IFEND}
  cdecl; external av__format;

(** @warning must be called before any I/O *)
function url_setbufsize (s: PByteIOContext; buf_size: cint): cint;
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION_MAJOR < 53}
{$IF LIBAVFORMAT_VERSION >= 51015000} // 51.15.0
(** Reset the buffer for reading or writing.
 * @note Will drop any data currently in the buffer without transmitting it.
 * @param flags URL_RDONLY to set up the buffer for reading, or URL_WRONLY
 *        to set up the buffer for writing. *)
function url_resetbuf(s: PByteIOContext; flags: cint): cint;
  cdecl; external av__format;
{$IFEND}
{$IFEND}

{$IF LIBAVFORMAT_VERSION >= 52061000} // 52.61.0
(**
 * Rewinds the ByteIOContext using the specified buffer containing the first buf_size bytes of the file.
 * Used after probing to avoid seeking.
 * Joins buf and s->buffer, taking any overlap into consideration.
 * @note s->buffer must overlap with buf or they can't be joined and the function fails
 * @note This function is NOT part of the public API
 *
 * @param s The read-only ByteIOContext to rewind
 * @param buf The probe buffer containing the first buf_size bytes of the file
 * @param buf_size The size of buf
 * @return 0 in case of success, a negative value corresponding to an
 * AVERROR code in case of failure
 *)
function ff_rewind_with_probe_data(s: PByteIOContext;  buf: PAnsiChar; buf_size: cint): cint;
  cdecl; external av__format;
{$IFEND}

(**
 * Creates and initializes a ByteIOContext for accessing the
 * resource indicated by url.
 * @note When the resource indicated by url has been opened in
 * read+write mode, the ByteIOContext can be used only for writing.
 *
 * @param s Used to return the pointer to the created ByteIOContext.
 * In case of failure the pointed to value is set to NULL.
 * @param flags flags which control how the resource indicated by url
 * is to be opened
 * @return 0 in case of success, a negative value corresponding to an
 * AVERROR code in case of failure
 *)
{$IF LIBAVFORMAT_VERSION < 52047000} // 52.47.0
{$IF LIBAVFORMAT_VERSION >= 52000000} // 52.0.0
function url_fopen(var s: PByteIOContext; filename: {const} PAnsiChar; flags: cint): cint;
{$ELSE}
function url_fopen(s: PByteIOContext; filename: {const} PAnsiChar; flags: cint): cint;
{$IFEND}
{$ELSE}
function url_fopen(var s: PByteIOContext; url: {const} PAnsiChar; flags: cint): cint;
{$IFEND}
  cdecl; external av__format;
function url_fclose(s: PByteIOContext): cint;
  cdecl; external av__format;
function url_fileno(s: PByteIOContext): PURLContext;
  cdecl; external av__format;

(**
 * Return the maximum packet size associated to packetized buffered file
 * handle. If the file is not packetized (stream like http or file on
 * disk), then 0 is returned.
 *
 * @param s buffered file handle
 * @return maximum packet size in bytes
 *)
function url_fget_max_packet_size (s: PByteIOContext): cint;
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 52000000} // 52.0.0
function url_open_buf(var s: PByteIOContext; buf: PAnsiChar; buf_size: cint; flags: cint): cint;
{$ELSE}
function url_open_buf(s: PByteIOContext; buf: PAnsiChar; buf_size: cint; flags: cint): cint;
{$IFEND}
  cdecl; external av__format;

(** return the written or read size *)
function url_close_buf(s: PByteIOContext): cint;
  cdecl; external av__format;

(**
 * Open a write only memory stream.
 *
 * @param s new IO context
 * @return zero if no error.
 *)
{$IF LIBAVFORMAT_VERSION >= 52000000} // 52.0.0
function url_open_dyn_buf(var s: PByteIOContext): cint;
{$ELSE}
function url_open_dyn_buf(s: PByteIOContext): cint;
{$IFEND}
  cdecl; external av__format;

(**
 * Open a write only packetized memory stream with a maximum packet
 * size of 'max_packet_size'.  The stream is stored in a memory buffer
 * with a big endian 4 byte header giving the packet size in bytes.
 *
 * @param s new IO context
 * @param max_packet_size maximum packet size (must be > 0)
 * @return zero if no error.
 *)
{$IF LIBAVFORMAT_VERSION >= 52000000} // 52.0.0
function url_open_dyn_packet_buf(var s: PByteIOContext; max_packet_size: cint): cint;
{$ELSE}
function url_open_dyn_packet_buf(s: PByteIOContext; max_packet_size: cint): cint;
{$IFEND}
  cdecl; external av__format;

(**
 * Return the written size and a pointer to the buffer. The buffer
 *  must be freed with av_free().
 * @param s IO context
 * @param pbuffer pointer to a byte buffer
 * @return the length of the byte buffer
 *)
function url_close_dyn_buf(s: PByteIOContext; pbuffer:PPointer): cint;
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 51017001} // 51.17.1
function ff_crc04C11DB7_update(checksum: culong; buf: {const} PByteArray;
                               len: cuint): culong;
  cdecl; external av__format;
{$IFEND}
function get_checksum(s: PByteIOContext): culong;
  cdecl; external av__format;
procedure init_gsum(s: PByteIOContext;
                        update_checksum: pointer;
                        checksum: culong);
  cdecl; external av__format;

(* udp.c *)
function udp_set_remote_url(h: PURLContext; uri: {const} PAnsiChar): cint;
  cdecl; external av__format;
function udp_get_local_port(h: PURLContext): cint;
  cdecl; external av__format;
{$IF LIBAVFORMAT_VERSION_MAJOR <= 52}
function udp_get_file_handle(h: PURLContext): cint;
  cdecl; external av__format;
{$IFEND}
  
implementation

function url_is_streamed(s: PByteIOContext): cint;
begin
  Result := s^.is_streamed;
end;

end.
