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
 * avformat version: 56.4.101
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
  FF_API_URL_CLASS            = (LIBAVFORMAT_VERSION_MAJOR >= 53);

const
  AVIO_SEEKABLE_NORMAL = 0001; (**< Seeking works like for a local file *)

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
  PPAVIOContext = ^PAVIOContext;
  PAVIOContext = ^TAVIOContext;
  TAVIOContext = record
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
    av_class: {const} PAVClass;
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

    (**
     * max filesize, used to limit allocations
     * This field is internal to libavformat and access from outside is not allowed.
     *)
    maxsize: cint64;

    (**
     * avio_read and avio_write should if possible be satisfied directly
     * instead of going through a buffer, and avio_seek will always
     * call the underlying seek function directly.
     *)
    direct: cint;

    (**
     * Bytes read statistic
     * This field is internal to libavformat and access from outside is not allowed.
     *)
    bytes_read: cint64;

    (**
     * seek statistic
     * This field is internal to libavformat and access from outside is not allowed.
     *)
    seek_count: cint;

    (**
     * writeout statistic
     * This field is internal to libavformat and access from outside is not allowed.
     *)
    writeout_count: cint;

    (**
     * Original buffer size
     * used internally after probing and ensure seekback to reset the buffer size
     * This field is internal to libavformat and access from outside is not allowed.
     *)
    orig_buffer_size: cint;
  end;

(* unbuffered I/O *)

(**
 * Return AVIO_* access flags corresponding to the access permissions
 * of the resource in url, or a negative value corresponding to an
 * AVERROR code in case of failure. The returned access flags are
 * masked by the value in flags.
 *
 * @note This function is intrinsically unsafe, in the sense that the
 * checked resource may change its existence or permission status from
 * one call to another. Thus you should not trust the returned value,
 * unless you are sure that no other processes are accessing the
 * checked resource.
 *)
function avio_check(url: {const} PAnsiChar; flags: cint): cint;
  cdecl; external av__format;

(**
 * Allocate and initialize an AVIOContext for buffered I/O. It must be later
 * freed with av_free().
 *
 * @param buffer Memory block for input/output operations via AVIOContext.
 *        The buffer must be allocated with av_malloc() and friends.
 *        It may be freed and replaced with a new buffer by libavformat.
 *        AVIOContext.buffer holds the buffer currently in use,
 *        which must be later freed with av_free().
 * @param buffer_size The buffer size is very important for performance.
 *        For protocols with fixed blocksize it should be set to this blocksize.
 *        For others a typical size is a cache page, e.g. 4kb.
 * @param write_flag Set to 1 if the buffer should be writable, 0 otherwise.
 * @param opaque An opaque pointer to user-specific data.
 * @param read_packet  A function for refilling the buffer, may be NULL.
 * @param write_packet A function for writing the buffer contents, may be NULL.
 *        The function may not change the input buffers content.
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
 * seek by any means (like reopening and linear reading) or other normally unreasonable
 * means that can be extremely slow.
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
function avio_feof(s: PAVIOContext): cint;
  cdecl; external av__format;
{$IFDEF FF_API_URL_FEOF}
(**
 * @deprecated use avio_feof()
 *)
function url_feof(s: PAVIOContext): cint;
  cdecl; external av__format; {attribute_deprecated}
{$ENDIF}
 
(** @warning currently size is limited *)
function avio_printf(s: PAVIOContext; fmt: {const} PAnsiChar; args: array of const): cint;
  cdecl; external av__format;

(**
 * Force flushing of buffered data.
 *
 * For write streams, force the buffered data to be immediately written to the output,
 * without to wait to fill the internal buffer.
 *
 * For read streams, discard all currently buffered data, and advance the
 * reported file position to that of the underlying stream. This does not
 * read new data, and does not perform any seeks.
 *)
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
 * @{
 *)
const
  AVIO_FLAG_READ  = 1;                                      (**< read-only *)
  AVIO_FLAG_WRITE = 2;                                      (**< write-only *)
  AVIO_FLAG_READ_WRITE = {(AVIO_FLAG_READ|AVIO_FLAG_WRITE)} 3;  (**< read-write pseudo flag *)
(**
 * @
 *)

const
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
  AVIO_FLAG_NONBLOCK = 8;    

(**
 * Use direct mode.
 * avio_read and avio_write should if possible be satisfied directly
 * instead of going through a buffer, and avio_seek will always
 * call the underlying seek function directly.
 *)
  AVIO_FLAG_DIRECT = $8000;

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
function avio_open(s: PPAVIOContext; url: {const} PAnsiChar; flags: cint): cint;
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
function avio_open2(s: PPAVIOContext; {const} url: PAnsiChar; flags: cint;
	{const} int_cb: PAVIOInterruptCB; options: PPAVDictionary): cint;
  cdecl; external av__format;

(**
 * Close the resource accessed by the AVIOContext s and free it.
 * This function can only be used if s was opened by avio_open().
 *
 * The internal buffer is automatically flushed before closing the
 * resource.
 *
 * @return 0 on success, an AVERROR < 0 on error.
 * @see avio_close
 *)
function avio_close(s: PAVIOContext): cint;
  cdecl; external av__format;

(**
 * Close the resource accessed by the AVIOContext *s, free it
 * and set the pointer pointing to it to NULL.
 * This function can only be used if s was opened by avio_open().
 *
 * The internal buffer is automatically flushed before closing the
 * resource.
 *
 * @return 0 on success, an AVERROR < 0 on error.
 * @see avio_close
 *)
function avio_closep(s: PPAVIOContext): cint;
  cdecl; external av__format;

(**
 * Open a write only memory stream.
 *
 * @param s new IO context
 * @return zero if no error.
 *)
function avio_open_dyn_buf(s: PPAVIOContext): cint;
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

(* Avoid a warning. The header can not be included because it breaks c++. *)
type
  PAVBPrint = ^TAVBPrint;
  TAVBPrint = record
	end;	

(**
 * Read contents of h into print buffer, up to max_size bytes, or up to EOF.
 *
 * @return 0 for success (max_size bytes read or EOF reached), negative error
 * code otherwise
 *)
function avio_read_to_bprint(h: PAVIOContext; pb: PAVBPrint; max_size: size_t): cint;
  cdecl; external av__format;

implementation

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
