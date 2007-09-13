                                                                              (*
 * unbuffered io for ffmpeg system
 * copyright (c) 2001 Fabrice Bellard
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
unit avio;

interface

uses
  windows;

const
  av__format = 'avformat-50.dll';

  URL_RDONLY = 0;
  URL_WRONLY = 1;
  URL_RDWR   = 2;

(* output byte stream handling *)

type
  offset_t = int64;
  int = integer;

(* unbuffered I/O *)
  PURLProtocol = ^TURLProtocol;
  PURLContext = ^TURLContext;
  TURLContext = record
    prot: PURLProtocol;
    flags: int;
    is_streamed: int;  //* true if streamed (no seek possible), default = false */
    max_packet_size: int;  //* if non zero, the stream is packetized with this max packet size */
    priv_data: pointer;
    filename: array [0..0] of char; (* specified filename *)
  end;

  PURLPollEntry = ^TURLPollEntry;
  TURLPollEntry = record
    handle: PURLContext;
    events: integer;
    revents: integer;
  end;

  TURLProtocol = record
    name: pchar;
    url_open: function (h: PURLContext; const filename: pchar; flags: integer): integer; cdecl;
    url_read: function (h: PURLContext; buf: pchar; size: integer): integer; cdecl;
    url_write: function (h: PURLContext; buf: pchar; size: integer): integer; cdecl;
    url_seek: function (h: PURLContext; pos: int64; whence: integer): int64; cdecl;
    url_close: function (h: PURLContext): integer; cdecl;
    next: PURLProtocol;
  end;

  PByteIOContext = ^TByteIOContext;
  TByteIOContext = record
    buffer: pchar;
    buffer_size: integer;
    buf_ptr: pchar;
    buf_end: pchar;
    opaque: pointer;
    read_packet: function (opaque: pointer; buf: pchar; buf_size: integer): integer; cdecl;
    write_packet: function (opaque: pointer; buf: pchar; buf_size: integer): integer; cdecl;
    seek: function (opaque: pointer; offset: int64; whence: integer): int64; cdecl;
    pos: int64; (* position in the file of the current buffer *)
    must_flush: integer; (* true if the next seek should flush *)
    eof_reached: integer; (* true if eof reached *)
    write_flag: integer;  (* true if open for writing *)
    is_streamed: integer;
    max_packet_size: integer;
    checksum: longword;
    checksum_ptr: pchar;
    update_checksum: function (checksum: cardinal; const buf: pchar; size: cardinal): LongWord; cdecl;
    error: integer;         ///< contains the error code or 0 if no error happened
  end;

  function url_open(h: PPointer; const filename: pchar; flags: integer): integer;
    cdecl; external av__format;
  function url_read (h: PURLContext; buf: pchar; size: integer): integer;
    cdecl; external av__format;
  function url_write (h: PURLContext; buf: pchar; size: integer): integer;
    cdecl; external av__format;
  function url_seek (h: PURLContext; pos: int64; whence: integer): int64;
    cdecl; external av__format;
  function url_close (h: PURLContext): integer;
    cdecl; external av__format;
  function url_exist(const filename: pchar): integer;
    cdecl; external av__format;
  function url_filesize (h: PURLContext): int64;
    cdecl; external av__format;
  function url_get_max_packet_size(h: PURLContext): integer;
    cdecl; external av__format;
  procedure url_get_filename(h: PURLContext; buf: pchar; buf_size: integer);
    cdecl; external av__format;

(* the callback is called in blocking functions to test regulary if
   asynchronous interruption is needed. -EINTR is returned in this
   case by the interrupted function. 'NULL' means no interrupt
   callback is given. *)
  procedure url_set_interrupt_cb (interrupt_cb: pinteger);
    cdecl; external av__format;

(* not implemented *)
//int url_poll(URLPollEntry *poll_table, int n, int timeout);

  function register_protocol (protocol: PURLProtocol): integer;
    cdecl; external av__format;

  function init_put_byte(s: PByteIOContext;
                  buffer: pchar;
                  buffer_size: integer; write_flag: integer;
                  opaque: pointer;
                  read_packet: pointer; //int (*read_packet)(void *opaque, uint8_t *buf, int buf_size),
                  write_packet: pointer; //int (*write_packet)(void *opaque, uint8_t *buf, int buf_size),
                  seek: pointer //offset_t (*seek)(void *opaque, offset_t offset, int whence)
                  ): integer;
    cdecl; external av__format;
  procedure put_byte(s: PByteIOContext; b: integer);
    cdecl; external av__format;
  procedure put_buffer (s: PByteIOContext; const buf: pchar; size: integer);
    cdecl; external av__format;
  procedure put_le64(s: PByteIOContext; val: int64);
    cdecl; external av__format;
  procedure put_be64(s: PByteIOContext; val: int64);
    cdecl; external av__format;
  procedure put_le32(s: PByteIOContext; val: cardinal);
    cdecl; external av__format;
  procedure put_be32(s: PByteIOContext; val: cardinal);
    cdecl; external av__format;
  procedure put_be24(s: PByteIOContext; val: cardinal);
    cdecl; external av__format;
  procedure put_le16(s: PByteIOContext; val: cardinal);
    cdecl; external av__format;
  procedure put_be16(s: PByteIOContext; val: cardinal);
    cdecl; external av__format;
  procedure put_tag(s: PByteIOContext; const tag: pchar);
    cdecl; external av__format;

  procedure put_strz(s: PByteIOContext; const buf: pchar);
    cdecl; external av__format;

  function url_fseek(s: PByteIOContext; offset: int64; whence: integer): int64;
    cdecl; external av__format;
  procedure url_fskip(s: PByteIOContext; offset: int64);
    cdecl; external av__format;
  function url_ftell(s: PByteIOContext): int64;
    cdecl; external av__format;
  function url_fsize(s: PByteIOContext): int64;
    cdecl; external av__format;
  function url_feof(s: PByteIOContext): integer;
    cdecl; external av__format;
  function url_ferror(s: PByteIOContext): integer;
    cdecl; external av__format;

  procedure put_flush_packet (s: PByteIOContext);
    cdecl; external av__format;
  function get_buffer(s: PByteIOContext; buf: pchar; size: integer): integer;
    cdecl; external av__format;
  function get_partial_buffer(s: PByteIOContext; buf: pchar; size: integer): integer;
    cdecl; external av__format;
  function get_byte(s: PByteIOContext): integer;
    cdecl; external av__format;
  function get_le32(s: PByteIOContext): cardinal;
    cdecl; external av__format;
  function get_le64(s: PByteIOContext): int64;
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
  function get_be64(s: PByteIOContext): int64;
    cdecl; external av__format;

  function url_is_streamed(s: PByteIOContext): integer;

  function url_fdopen (s: PByteIOContext; h: PURLContext): integer;
    cdecl; external av__format;
  function url_setbufsize (s: PByteIOContext; buf_size: integer): integer;
    cdecl; external av__format;
  function url_fopen(s: PByteIOContext; const filename: pchar; flags: integer): integer;
    cdecl; external av__format;
  function url_fclose(s: PByteIOContext): integer;
    cdecl; external av__format;

  function url_fileno(s: PByteIOContext): PURLContext;
    cdecl; external av__format;
  function url_fget_max_packet_size (s: PByteIOContext): integer;
    cdecl; external av__format;
  function url_open_buf(s: PByteIOContext; buf: pchar; buf_size: integer; flags: integer): integer;
    cdecl; external av__format;
  function url_close_buf(s: PByteIOContext): integer;
    cdecl; external av__format;

  function url_open_dyn_buf(s: PByteIOContext): integer;
    cdecl; external av__format;
  function url_open_dyn_packet_buf(s: PByteIOContext; max_packet_size: integer): integer;
    cdecl; external av__format;
  function url_close_dyn_buf(s: PByteIOContext; pbuffer:PPointer): integer;
    cdecl; external av__format;

  function get_checksum(s: PByteIOContext): cardinal;
    cdecl; external av__format;

  procedure init_checksum (s: PByteIOContext; update_checksum: pointer; checksum: cardinal);
    cdecl; external av__format;

  {$IFNDEF FPC}
  function udp_set_remote_url(h: PURLContext; const uri: pchar): integer;
    cdecl; external av__format;
  function udp_get_local_port(h: PURLContext): integer;
    cdecl; external av__format;
  function udp_get_file_handle(h: PURLContext): integer;
    cdecl; external av__format;
  {$ENDIF}

implementation

function url_is_streamed(s: PByteIOContext): integer;
begin
  Result := s^.is_streamed;
end;

end.
