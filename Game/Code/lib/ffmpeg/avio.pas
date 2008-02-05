(*
 * unbuffered io for ffmpeg system
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
 *)

(* This is a part of Pascal porting of ffmpeg.  Originally by Victor Zinetz for Delphi and Free Pascal on Windows.
For Mac OS X, some modifications were made by The Creative CAT, denoted as CAT
in the source codes *)

// Revision: 11295

unit avio;

{$IFDEF FPC}
  {$MODE DELPHI }
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

interface

uses
  UConfig;

(* output byte stream handling *)

type
  TOffset = int64;

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

type
  TURLInterruptCB = function (): integer; cdecl;

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
    prot: PURLProtocol;
    flags: integer;
    is_streamed: integer;  (**< true if streamed (no seek possible), default = false *)
    max_packet_size: integer;  (**< if non zero, the stream is packetized with this max packet size *)
    priv_data: pointer;
    filename: PChar; (**< specified filename *)
  end;

  PURLPollEntry = ^TURLPollEntry;
  TURLPollEntry = record
    handle: PURLContext;
    events: integer;
    revents: integer;
  end;

  TURLProtocol = record
    name: pchar;
    url_open: function (h: PURLContext; {const} filename: pchar; flags: integer): integer; cdecl;
    url_read: function (h: PURLContext; buf: pchar; size: integer): integer; cdecl;
    url_write: function (h: PURLContext; buf: pchar; size: integer): integer; cdecl;
    url_seek: function (h: PURLContext; pos: TOffset; whence: integer): TOffset; cdecl;
    url_close: function (h: PURLContext): integer; cdecl;
    next: PURLProtocol;
    {$IF (LIBAVFORMAT_VERSION >= 52001000) and (LIBAVFORMAT_VERSION < 52004000)} // 52.1.0 .. 52.4.0
    url_read_play: function (h: PURLContext): integer;
    url_read_pause: function (h: PURLContext): integer;
    {$IFEND}
    {$IF LIBAVFORMAT_VERSION >= 52004000} // 52.4.0
    url_read_pause: function (h: PURLContext; pause: integer): integer; cdecl;
    {$IFEND}
    {$IF LIBAVFORMAT_VERSION >= 52001000} // 52.1.0
    url_read_seek: function (h: PURLContext;
                         stream_index: integer; timestamp: int64; flags: integer): TOffset; cdecl;
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
    buffer: pchar;
    buffer_size: integer;
    buf_ptr: pchar;
    buf_end: pchar;
    opaque: pointer;
    read_packet: function (opaque: pointer; buf: pchar; buf_size: integer): integer; cdecl;
    write_packet: function (opaque: pointer; buf: pchar; buf_size: integer): integer; cdecl;
    seek: function (opaque: pointer; offset: TOffset; whence: integer): TOffset; cdecl;
    pos: TOffset; (* position in the file of the current buffer *)
    must_flush: integer; (* true if the next seek should flush *)
    eof_reached: integer; (* true if eof reached *)
    write_flag: integer;  (* true if open for writing *)
    is_streamed: integer;
    max_packet_size: integer;
    checksum: longword;
    checksum_ptr: pchar;
    update_checksum: function (checksum: Longword; {const} buf: pchar; size: cardinal): Longword; cdecl;
    error: integer;         ///< contains the error code or 0 if no error happened
    {$IF (LIBAVFORMAT_VERSION >= 52001000) and (LIBAVFORMAT_VERSION < 52004000)} // 52.1.0 .. 52.4.0
    read_play: function(opaque: Pointer): integer; cdecl;
    read_pause: function(opaque: Pointer): integer; cdecl;
    {$IFEND}
    {$IF LIBAVFORMAT_VERSION >= 52004000} // 52.4.0
    read_pause: function(opaque: Pointer; pause: integer): integer; cdecl;
    {$IFEND}
    {$IF LIBAVFORMAT_VERSION >= 52001000} // 52.1.0
    read_seek: function(opaque: Pointer;
                     stream_index: integer; timestamp: int64; flags: integer): TOffset; cdecl;
    {$IFEND}
  end;

function url_open(h: PPointer; {const} filename: pchar; flags: integer): integer;
  cdecl; external av__format;
function url_read (h: PURLContext; buf: pchar; size: integer): integer;
  cdecl; external av__format;
function url_write (h: PURLContext; buf: pchar; size: integer): integer;
  cdecl; external av__format;
function url_seek (h: PURLContext; pos: TOffset; whence: integer): TOffset;
  cdecl; external av__format;
function url_close (h: PURLContext): integer;
  cdecl; external av__format;
function url_exist(const filename: pchar): integer;
  cdecl; external av__format;
function url_filesize (h: PURLContext): TOffset;
  cdecl; external av__format;

(**
 * Return the maximum packet size associated to packetized file
 * handle. If the file is not packetized (stream like HTTP or file on
 * disk), then 0 is returned.
 *
 * @param h file handle
 * @return maximum packet size in bytes
 *)
function url_get_max_packet_size(h: PURLContext): integer;
  cdecl; external av__format;
procedure url_get_filename(h: PURLContext; buf: pchar; buf_size: integer);
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
function url_poll(poll_table: PURLPollEntry; n: integer; timeout: integer): integer;
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 52004000} // 52.4.0
(**
 * Pause and resume playing - only meaningful if using a network streaming
 * protocol (e.g. MMS).
 * @param pause 1 for pause, 0 for resume
 *)
function av_url_read_pause(h: PURLContext; pause: integer): integer;
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
function av_url_read_seek(h: PURLContext;
                     stream_index: integer; timestamp: int64; flags: integer): TOffset;
  cdecl; external av__format;
{$IFEND}

{
var
  first_protocol: PURLProtocol; external av__format;
  url_interrupt_cb: PURLInterruptCB; external av__format;
}

{$IF LIBAVFORMAT_VERSION >= 52002000} // 52.2.0
function av_protocol_next(p: PURLProtocol): PURLProtocol;
  cdecl; external av__format;
{$IFEND}

function register_protocol (protocol: PURLProtocol): integer;
  cdecl; external av__format;

type
  TReadWriteFunc = function (opaque: Pointer; buf: PChar; buf_size: integer): integer; cdecl;
  TSeekFunc = function (opaque: Pointer; offset: TOffset; whence: integer): TOffset; cdecl;

function init_put_byte(s: PByteIOContext;
                buffer: pchar;
                buffer_size: integer; write_flag: integer;
                opaque: pointer;
                read_packet: TReadWriteFunc;
                write_packet: TReadWriteFunc;
                seek: TSeekFunc): integer;
  cdecl; external av__format;
{$IF LIBAVFORMAT_VERSION >= 52004000} // 52.4.0
function av_alloc_put_byte(
                  buffer: PChar;
                  buffer_size: integer;
                  write_flag: integer;
                  opaque: Pointer;
                  read_packet: TReadWriteFunc,
                  write_packet: TReadWriteFunc,
                  seek: TSeekFunc): PByteIOContext;
  cdecl; external av__format;
{$IFEND}

procedure put_byte(s: PByteIOContext; b: integer);
  cdecl; external av__format;
procedure put_buffer (s: PByteIOContext; {const} buf: pchar; size: integer);
  cdecl; external av__format;
procedure put_le64(s: PByteIOContext; val: int64);
  cdecl; external av__format;
procedure put_be64(s: PByteIOContext; val: int64);
  cdecl; external av__format;
procedure put_le32(s: PByteIOContext; val: cardinal);
  cdecl; external av__format;
procedure put_be32(s: PByteIOContext; val: cardinal);
  cdecl; external av__format;
procedure put_le24(s: PByteIOContext; val: cardinal);
  cdecl; external av__format;
procedure put_be24(s: PByteIOContext; val: cardinal);
  cdecl; external av__format;
procedure put_le16(s: PByteIOContext; val: cardinal);
  cdecl; external av__format;
procedure put_be16(s: PByteIOContext; val: cardinal);
  cdecl; external av__format;
procedure put_tag(s: PByteIOContext; {const} tag: pchar);
  cdecl; external av__format;

procedure put_strz(s: PByteIOContext; {const} buf: pchar);
  cdecl; external av__format;

function url_fseek(s: PByteIOContext; offset: TOffset; whence: integer): TOffset;
  cdecl; external av__format;
procedure url_fskip(s: PByteIOContext; offset: TOffset);
  cdecl; external av__format;
function url_ftell(s: PByteIOContext): TOffset;
  cdecl; external av__format;
function url_fsize(s: PByteIOContext): TOffset;
  cdecl; external av__format;
function url_feof(s: PByteIOContext): integer;
  cdecl; external av__format;
function url_ferror(s: PByteIOContext): integer;
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 52004000} // 52.4.0
function av_url_read_fpause(h: PByteIOContext; pause: integer): integer;
  cdecl; external av__format;
{$IFEND}
{$IF LIBAVFORMAT_VERSION >= 52001000} // 52.1.0
function av_url_read_fseek(h: PByteIOContext;
                      stream_index: integer; timestamp: int64; flags: integer): TOffset;
  cdecl; external av__format;
{$IFEND}

const
  URL_EOF = -1;
(** @note return URL_EOF (-1) if EOF *)
function url_fgetc(s: PByteIOContext): integer;
  cdecl; external av__format;

(** @warning currently size is limited *)
function url_fprintf(s: PByteIOContext; {const} fmt: PChar; args: array of const): integer;
  cdecl; external av__format;

(** @note unlike fgets, the EOL character is not returned and a whole
   line is parsed. return NULL if first char read was EOF *)
function url_fgets(s: PByteIOContext; buf: PChar; buf_size: integer): PChar;
  cdecl; external av__format;

procedure put_flush_packet (s: PByteIOContext);
  cdecl; external av__format;
  
function get_buffer(s: PByteIOContext; buf: pchar; size: integer): integer;
  cdecl; external av__format;
function get_partial_buffer(s: PByteIOContext; buf: pchar; size: integer): integer;
  cdecl; external av__format;

(** @note return 0 if EOF, so you cannot use it if EOF handling is
   necessary *)
function get_byte(s: PByteIOContext): integer;
  cdecl; external av__format;
function get_le24(s: PByteIOContext): cardinal;
  cdecl; external av__format;
function get_le32(s: PByteIOContext): cardinal;
  cdecl; external av__format;
function get_le64(s: PByteIOContext): uint64;
  cdecl; external av__format;
function get_le16(s: PByteIOContext): cardinal;
  cdecl; external av__format;

function get_strz(s: PByteIOContext; buf: pchar; maxlen: integer): pchar;
  cdecl; external av__format;
function get_be16(s: PByteIOContext): cardinal;
  cdecl; external av__format;
function get_be24(s: PByteIOContext): cardinal;
  cdecl; external av__format;
function get_be32(s: PByteIOContext): cardinal;
  cdecl; external av__format;
function get_be64(s: PByteIOContext): uint64;
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 51017001} // 51.17.1
function ff_get_v(bc: PByteIOContext): uint64;
  cdecl; external av__format;
{$IFEND}

function url_is_streamed(s: PByteIOContext): integer; inline;

(** @note when opened as read/write, the buffers are only used for
   writing *)
{$IF LIBAVFORMAT_VERSION >= 52000000} // 52.0.0
function url_fdopen (var s: PByteIOContext; h: PURLContext): integer;
{$ELSE}
function url_fdopen (s: PByteIOContext; h: PURLContext): integer;
{$IFEND}
  cdecl; external av__format;

(** @warning must be called before any I/O *)
function url_setbufsize (s: PByteIOContext; buf_size: integer): integer;
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 51015000} // 51.15.0
(** Reset the buffer for reading or writing.
 * @note Will drop any data currently in the buffer without transmitting it.
 * @param flags URL_RDONLY to set up the buffer for reading, or URL_WRONLY
 *        to set up the buffer for writing. *)
function url_resetbuf(s: PByteIOContext; flags: integer): integer;
  cdecl; external av__format;
{$IFEND}

(** @note when opened as read/write, the buffers are only used for
   writing *)
{$IF LIBAVFORMAT_VERSION >= 52000000} // 52.0.0
function url_fopen(var s: PByteIOContext; const filename: pchar; flags: integer): integer;
{$ELSE}
function url_fopen(s: PByteIOContext; const filename: pchar; flags: integer): integer;
{$IFEND}
  cdecl; external av__format;
function url_fclose(s: PByteIOContext): integer;
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
function url_fget_max_packet_size (s: PByteIOContext): integer;
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 52000000} // 52.0.0
function url_open_buf(var s: PByteIOContext; buf: pchar; buf_size: integer; flags: integer): integer;
{$ELSE}
function url_open_buf(s: PByteIOContext; buf: pchar; buf_size: integer; flags: integer): integer;
{$IFEND}
  cdecl; external av__format;

(** return the written or read size *)
function url_close_buf(s: PByteIOContext): integer;
  cdecl; external av__format;

(**
 * Open a write only memory stream.
 *
 * @param s new IO context
 * @return zero if no error.
 *)
{$IF LIBAVFORMAT_VERSION >= 52000000} // 52.0.0
function url_open_dyn_buf(var s: PByteIOContext): integer;
{$ELSE}
function url_open_dyn_buf(s: PByteIOContext): integer;
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
function url_open_dyn_packet_buf(var s: PByteIOContext; max_packet_size: integer): integer;
{$ELSE}
function url_open_dyn_packet_buf(s: PByteIOContext; max_packet_size: integer): integer;
{$IFEND}
  cdecl; external av__format;

(**
 * Return the written size and a pointer to the buffer. The buffer
 *  must be freed with av_free().
 * @param s IO context
 * @param pbuffer pointer to a byte buffer
 * @return the length of the byte buffer
 *)
function url_close_dyn_buf(s: PByteIOContext; pbuffer:PPointer): integer;
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 51017001} // 51.17.1
function ff_crc04C11DB7_update(checksum: longword; {const} buf: PChar; len: cardinal): longword;
  cdecl; external av__format;
{$IFEND}
function get_checksum(s: PByteIOContext): cardinal;
  cdecl; external av__format;
procedure init_checksum (s: PByteIOContext; update_checksum: pointer; checksum: cardinal);
  cdecl; external av__format;

(* udp.c *)
function udp_set_remote_url(h: PURLContext; {const} uri: pchar): integer;
  cdecl; external av__format;
function udp_get_local_port(h: PURLContext): integer;
  cdecl; external av__format;
function udp_get_file_handle(h: PURLContext): integer;
  cdecl; external av__format;

implementation

function url_is_streamed(s: PByteIOContext): integer;
begin
  Result := s^.is_streamed;
end;

end.
