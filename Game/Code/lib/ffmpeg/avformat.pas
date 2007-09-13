                                                                              (*
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
unit avformat;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  windows, avcodec, avio, rational, avutil;

const
  av__format = 'avformat-50.dll';

  LIBAVUTIL_VERSION_INT   =  ((49 shl 16) + (0 shl 8) + 1);
  LIBAVUTIL_VERSION       = '49.0.1';
  LIBAVUTIL_BUILD         = LIBAVUTIL_VERSION_INT;

  MAXINT64 = $7fffffffffffffff;
  MININT64 = $8000000000000000;

  PKT_FLAG_KEY = $0001;

  (* AVImageFormat.flags field constants *)
  AVIMAGE_INTERLEAVED = $0001; (* image format support interleaved output *)
  MAX_STREAMS         = 20;
type
  int = integer;

  PAVPacket = ^TAVPacket;
  PAVImageFormat = ^TAVImageFormat;
  PAVFormatContext = ^TAVFormatContext;
  PAVFormatParameters = ^TAVFormatParameters;
  PAVOutputFormat = ^TAVOutputFormat;
  PAVProbeData = ^TAVProbeData;
  PAVInputFormat = ^TAVInputFormat;
  PAVIndexEntry = ^TAVIndexEntry;
  PAVStream = ^TAVStream;
  PAVPacketList = ^TAVPacketList;
  PAVImageInfo = ^TAVImageInfo;

  TAVPacket = record {56}
    pts: int64;                            ///< presentation time stamp in time_base units
    dts: int64;                            ///< decompression time stamp in time_base units
    data: PByte;
    size: integer;
    stream_index: integer;
    flags: integer;
    duration: integer;                         ///< presentation duration in time_base units (0 if not available)
    destruct: procedure (p: PAVPacket);
    priv: pointer;
    pos: int64;                            ///< byte position in stream, -1 if unknown
  end;

(*************************************************)
(* fractional numbers for exact pts handling *)

(* the exact value of the fractional number is: 'val + num / den'. num
   is assumed to be such as 0 <= num < den *)
  PAVFrac = ^TAVFrac;
  TAVFrac = record
    val, num, den: int64;
  end;

(*************************************************)
(* input/output formats *)

(* this structure contains the data a format has to probe a file *)
  TAVProbeData = record {12}
    filename: pchar;
    buf: pchar;
    buf_size: integer;
  end;

  TAVFormatParameters = record {56}
    time_base: TAVRational; (* 8 bytes *)
    sample_rate: integer;
    channels: integer;
    width: integer;
    height: integer;
    pix_fmt: TAVPixelFormat;
    image_format: PAVImageFormat; (* 4 bytes *)
    channel: integer; (* used to select dv channel *)
    device: pchar; (* video, audio or DV device *)
    standard: pchar; (* tv standard, NTSC, PAL, SECAM *)
//    int mpeg2ts_raw:1;  (* force raw MPEG2 transport stream output, if possible *)
//    int mpeg2ts_compute_pcr:1; (* compute exact PCR for each transport
//                                  stream packet (only meaningful if
//                                  mpeg2ts_raw is TRUE *)
//    int initial_pause:1;       (* do not begin to play the stream
//                                  immediately (RTSP only) *)
    dummy: byte;
    video_codec_id: TCodecID;
    audio_codec_id: TCodecID;
  end;

  TAVOutputFormat = record {56}
    name: pchar;
    long_name: pchar;
    mime_type: pchar;
    extensions: pchar; (* comma separated extensions *)
    (* size of private data so that it can be allocated in the wrapper *)
    priv_data_size: integer;
    (* output support *)
    audio_codec: TCodecID; (* default audio codec *)
    video_codec: TCodecID; (* default video codec *)
    write_header: function (c: PAVFormatContext): integer; cdecl;
    write_packet: function (c: PAVFormatContext; pkt: PAVPacket): integer; cdecl;
    write_trailer: function (c: PAVFormatContext): integer; cdecl;
    (* can use flags: AVFMT_NOFILE, AVFMT_NEEDNUMBER, AVFMT_GLOBALHEADER *)
    flags: integer;
    (* currently only used to set pixel format if not YUV420P *)
    set_parameters: function (c: PAVFormatContext; f: PAVFormatParameters): integer; cdecl;
    interleave_packet: function (s: PAVFormatContext; _out: PAVPacket; _in: PAVPacket; flush: integer): integer; cdecl;
    (* private fields *)
    next: PAVOutputFormat;
  end;

  TAVInputFormat = record {60}
    name: pchar;
    long_name: pchar;
    (* size of private data so that it can be allocated in the wrapper *)
    priv_data_size: integer;
    (* tell if a given file has a chance of being parsing by this format *)
    read_probe: function (p: PAVProbeData): integer; cdecl;
    (* read the format header and initialize the AVFormatContext
       structure. Return 0 if OK. 'ap' if non NULL contains
       additionnal paramters. Only used in raw format right
       now. 'av_new_stream' should be called to create new streams.  *)
    read_header: function (c: PAVFormatContext; ap: PAVFormatParameters): integer; cdecl;
    (* read one packet and put it in 'pkt'. pts and flags are also
       set. 'av_new_stream' can be called only if the flag
       AVFMTCTX_NOHEADER is used. *)
    read_packet: function (c: PAVFormatContext; pkt: PAVPacket): integer; cdecl;
    (* close the stream. The AVFormatContext and AVStreams are not
       freed by this function *)
    read_close: function (c: PAVFormatContext): integer; cdecl;
    (*** seek to a given timestamp relative to the frames in
     * stream component stream_index
     * @param stream_index must not be -1
     * @param flags selects which direction should be preferred if no exact
     *              match is available     *)
    read_seek: function (c: PAVFormatContext; stream_index: integer;
                  timestamp: int64; flags: integer): integer; cdecl;
    (*** gets the next timestamp in AV_TIME_BASE units.     *)
    read_timestamp: function (s: PAVFormatContext; stream_index: integer;
                              pos: pint64; pos_limit: int64): integer; cdecl;
    (* can use flags: AVFMT_NOFILE, AVFMT_NEEDNUMBER *)
    flags: integer;
    (* if extensions are defined, then no probe is done. You should
       usually not use extension format guessing because it is not
       reliable enough *)
    extensions: pchar;
    (* general purpose read only value that the format can use *)
    value: integer;

    (* start/resume playing - only meaningful if using a network based format (RTSP) *)
    read_play: function (c: PAVFormatContext): integer; cdecl;

    (* pause playing - only meaningful if using a network based format  (RTSP) *)
    read_pause: function (c: PAVFormatContext): integer; cdecl;

    (* private fields *)
    next: PAVInputFormat;
  end;

  TAVIndexEntry = record {24}
    pos: int64;
    timestamp: int64;
(* the following 2 flags indicate that the next/prev keyframe is known, and scaning for it isnt needed *)
    flags: integer;
    min_distance: integer;         (* min distance between this and the previous keyframe, used to avoid unneeded searching *)
  end;

  TAVStream = record {168}
    index: integer;    (* stream index in AVFormatContext *)
    id: integer;       (* format specific stream id *)
    codec: PAVCodecContext; (* codec context *)
    (*** real base frame rate of the stream.
     * for example if the timebase is 1/90000 and all frames have either
     * approximately 3600 or 1800 timer ticks then r_frame_rate will be 50/1     *)
    r_frame_rate: TAVRational;
    priv_data: pointer;
    (* internal data used in av_find_stream_info() *)
    codec_info_duration: int64;
    codec_info_nb_frames: integer;
    (* encoding: PTS generation when outputing stream *)
    pts: TAVFrac;

    (*** this is the fundamental unit of time (in seconds) in terms
     * of which frame timestamps are represented. for fixed-fps content,
     * timebase should be 1/framerate and timestamp increments should be
     * identically 1.     *)
    time_base: TAVRational;
    pts_wrap_bits: integer; (* number of bits in pts (used for wrapping control) *)
    (* ffmpeg.c private use *)
    stream_copy: integer; (* if TRUE, just copy stream *)
    discard: TAVDiscard; ///< selects which packets can be discarded at will and dont need to be demuxed
    //FIXME move stuff to a flags field?
    (* quality, as it has been removed from AVCodecContext and put in AVVideoFrame
     * MN:dunno if thats the right place, for it *)
    quality: single;
    (* decoding: position of the first frame of the component, in AV_TIME_BASE fractional seconds. *)
    start_time: int64;
    (* decoding: duration of the stream, in AV_TIME_BASE fractional seconds. *)
    duration: int64;

    language: array [0..3] of char; (* ISO 639 3-letter language code (empty string if undefined) *)

    (* av_read_frame() support *)
    need_parsing: integer;                  ///< 1->full parsing needed, 2->only parse headers dont repack
    parser: PAVCodecParserContext;

    cur_dts: int64;
    last_IP_duration: integer;
    last_IP_pts: int64;
    (* av_seek_frame() support *)
    index_entries: PAVIndexEntry; (* only used if the format does not support seeking natively *)
    nb_index_entries: integer;
    index_entries_allocated_size: integer;

    nb_frames: int64;                 ///< number of frames in this stream if known or 0
    pts_buffer: array [0..4] of int64;
  end;

(* format I/O context *)
  TAVFormatContext = record {3960}
    av_class: PAVClass; (* set by av_alloc_format_context *)
    (* can only be iformat or oformat, not both at the same time *)
    iformat: PAVInputFormat;
    oformat: PAVOutputFormat;
    priv_data: pointer;
    pb: TByteIOContext;
    nb_streams: integer;
    streams: array [0..MAX_STREAMS - 1] of PAVStream;
    filename: array [0..1023] of char; (* input or output filename *)
    (* stream info *)
    timestamp: int64;
    title: array [0..511] of char;
    author: array [0..511] of char;
    copyright: array [0..511] of char;
    comment: array [0..511] of char;
    album: array [0..511] of char;
    year: integer;  (* ID3 year, 0 if none *)
    track: integer; (* track number, 0 if none *)
    genre: array [0..31] of char; (* ID3 genre *)

    ctx_flags: integer; (* format specific flags, see AVFMTCTX_xx *)
    (* private data for pts handling (do not modify directly) *)
    (* This buffer is only needed when packets were already buffered but
       not decoded, for example to get the codec parameters in mpeg
       streams *)
    packet_buffer: PAVPacketList;

    (* decoding: position of the first frame of the component, in
       AV_TIME_BASE fractional seconds. NEVER set this value directly:
       it is deduced from the AVStream values.  *)
    start_time: int64;
    (* decoding: duration of the stream, in AV_TIME_BASE fractional
       seconds. NEVER set this value directly: it is deduced from the
       AVStream values.  *)
    duration: int64;
    (* decoding: total file size. 0 if unknown *)
    file_size: int64;
    (* decoding: total stream bitrate in bit/s, 0 if not
       available. Never set it directly if the file_size and the
       duration are known as ffmpeg can compute it automatically. *)
    bit_rate: integer;

    (* av_read_frame() support *)
    cur_st: PAVStream;
    cur_ptr: pchar;
    cur_len: integer;
    cur_pkt: TAVPacket;

    (* av_seek_frame() support *)
    data_offset: int64; (* offset of the first packet *)
    index_built: integer;

    mux_rate: integer;
    packet_size: integer;
    preload: integer;
    max_delay: integer;

    (* number of times to loop output in formats that support it *)
    loop_output: integer;

    flags: integer;
    loop_input: integer;
    (* decoding: size of data to probe; encoding unused *)
    probesize: cardinal;
  end;

  TAVPacketList = record {64}
    pkt: TAVPacket;
    next: PAVPacketList;
  end;

(* still image support *)
  PAVInputImageContext = pointer; // attribute_deprecated;
//  PAVInputImageContext = pointer; //AVInputImageContext attribute_deprecated;

(* still image support *)
  TAVImageInfo = record {48}
    pix_fmt: TAVPixelFormat; (* requested pixel format *)
    width: integer; (* requested width *)
    height: integer; (* requested height *)
    interleaved: integer; (* image is interleaved (e.g. interleaved GIF) *)
    pict: TAVPicture; (* returned allocated image *)
  end;

  TAVImageFormat = record {32}
    name: pchar;
    extensions: pchar;
    (* tell if a given file has a chance of being parsing by this format *)
    img_probe: function (d: PAVProbeData): integer; cdecl;
    (* read a whole image. 'alloc_cb' is called when the image size is
       known so that the caller can allocate the image. If 'allo_cb'
       returns non zero, then the parsing is aborted. Return '0' if
       OK. *)
    img_read: function (b: PByteIOContext; alloc_cb: pointer; ptr: pointer): integer; cdecl;
    (* write the image *)
    supported_pixel_formats: integer; (* mask of supported formats for output *)
    img_write: function (b: PByteIOContext; i: PAVImageInfo): integer; cdecl;
    flags: integer;
    next: PAVImageFormat;
  end;

procedure av_destruct_packet_nofree (pkt: PAVPacket);
  cdecl; external av__format;
procedure av_destruct_packet (pkt: PAVPacket);
  cdecl; external av__format;

(* initialize optional fields of a packet *)
procedure av_init_packet (pkt: PAVPacket);

function av_new_packet(pkt: PAVPacket; size: integer): integer;
  cdecl; external av__format;

function av_get_packet (s: PByteIOContext; pkt: PAVPacket; size: integer): integer;
  cdecl; external av__format;

function av_dup_packet (pkt: PAVPacket): integer;
  cdecl; external av__format;

(** * Free a packet
 *
 * @param pkt packet to free *)
procedure av_free_packet (pkt: PAVPacket);

procedure av_register_image_format (img_fmt: PAVImageFormat);
    cdecl; external av__format;

function av_probe_image_format (pd: PAVProbeData): PAVImageFormat;
    cdecl; external av__format;

function guess_image_format (filename: pchar): PAVImageFormat;
    cdecl; external av__format;

function av_guess_image2_codec(filename: pchar): TCodecID;
    cdecl; external av__format;

function av_read_image (pb: PByteIOContext; filename: pchar;
                  fmt: PAVImageFormat;
                  alloc_cb: pointer; opaque: pointer): integer;
    cdecl; external av__format;

function av_write_image(pb: PByteIOContext; fmt: PAVImageFormat; img: PAVImageInfo): integer;
    cdecl; external av__format;

(* XXX: use automatic init with either ELF sections or C file parser *)
(* modules *)

//#include "rtp.h"

//#include "rtsp.h"

(* utils.c *)
  procedure av_register_input_format (format: PAVInputFormat);
    cdecl; external av__format;

  procedure av_register_output_format (format: PAVOutputFormat);
    cdecl; external av__format;

  function guess_stream_format (short_name: pchar; filename: pchar; mime_type: pchar): PAVOutputFormat;
    cdecl; external av__format;

  function guess_format(short_name: pchar; filename: pchar; mime_type: pchar): PAVOutputFormat;
    cdecl; external av__format;

  function av_guess_codec(fmt: PAVOutputFormat; short_name: pchar;
                          filename: pchar; mime_type: pchar; _type: TCodecType): TCodecID;
    cdecl; external av__format;

  procedure av_hex_dump (f: HFILE; buf: pchar; size: integer);
    cdecl; external av__format;
  procedure av_pkt_dump(f: HFILE; pkt: pAVPacket; dump_payload: integer);
    cdecl; external av__format;

  procedure av_register_all ();
    cdecl; external av__format;


(* media file input *)
  function av_find_input_format (short_name: pchar): PAVInputFormat;
    cdecl; external av__format;
  function av_probe_input_format (pd: PAVProbeData; is_opened: integer): PAVInputFormat;
    cdecl; external av__format;
  function av_open_input_stream (ic_ptr: PAVFormatContext;
                         pb: PByteIOContext; filename: pchar;
                         fmt: PAVInputFormat; ap: PAVFormatParameters): integer;
    cdecl; external av__format;
(*** Open a media file as input. The codec are not opened. Only the file
 * header (if present) is read.
 *
 * @param ic_ptr the opened media file handle is put here
 * @param filename filename to open.
 * @param fmt if non NULL, force the file format to use
 * @param buf_size optional buffer size (zero if default is OK)
 * @param ap additionnal parameters needed when opening the file (NULL if default)
 * @return 0 if OK. AVERROR_xxx otherwise. *)

  function av_open_input_file (var ic_ptr: PAVFormatContext; filename: pchar;
                       fmt: PAVInputFormat; buf_size: integer;
                       ap: PAVFormatParameters): integer;
    cdecl; external av__format;

  (* no av_open for output, so applications will need this: *)
  function av_alloc_format_context (): PAVFormatContext;
    cdecl; external av__format;

const
  AVERROR_UNKNOWN     =(-1);  (* unknown error *)
  AVERROR_IO          =(-2);  (* i/o error *)
  AVERROR_NUMEXPECTED =(-3);  (* number syntax expected in filename *)
  AVERROR_INVALIDDATA =(-4);  (* invalid data found *)
  AVERROR_NOMEM       =(-5);  (* not enough memory *)
  AVERROR_NOFMT       =(-6);  (* unknown format *)
  AVERROR_NOTSUPP     =(-7);  (* operation not supported *)

(*** Read the beginning of a media file to get stream information. This
 * is useful for file formats with no headers such as MPEG. This
 * function also compute the real frame rate in case of mpeg2 repeat
 * frame mode.
 *
 * @param ic media file handle
 * @return >=0 if OK. AVERROR_xxx if error.
 * @todo let user decide somehow what information is needed so we dont waste time geting stuff the user doesnt need *)

  function av_find_stream_info (ic: PAVFormatContext): integer;
    cdecl; external av__format;
  function av_read_packet (s: PAVFormatContext; pkt: PAVPacket): integer;
    cdecl; external av__format;
(*** Return the next frame of a stream.
 *
 * The returned packet is valid
 * until the next av_read_frame() or until av_close_input_file() and
 * must be freed with av_free_packet. For video, the packet contains
 * exactly one frame. For audio, it contains an integer number of
 * frames if each frame has a known fixed size (e.g. PCM or ADPCM
 * data). If the audio frames have a variable size (e.g. MPEG audio),
 * then it contains one frame.
 *
 * pkt->pts, pkt->dts and pkt->duration are always set to correct
 * values in AV_TIME_BASE unit (and guessed if the format cannot
 * provided them). pkt->pts can be AV_NOPTS_VALUE if the video format
 * has B frames, so it is better to rely on pkt->dts if you do not
 * decompress the payload.
 *
 * @return 0 if OK, < 0 if error or end of file. *)

  function av_read_frame (s: PAVFormatContext; pkt: PAVPacket): integer;
    cdecl; external av__format;
  function av_seek_frame (s: PAVFormatContext; stream_index: integer; timestamp: int64; flags: integer): integer;
    cdecl; external av__format;
  function av_read_play (s: PAVFormatContext): integer;
    cdecl; external av__format;
  function av_read_pause (s: PAVFormatContext): integer;
    cdecl; external av__format;
  procedure av_close_input_file (s: PAVFormatContext);
    cdecl; external av__format;
  function av_new_stream (s: PAVFormatContext; id: integer): PAVStream;
    cdecl; external av__format;
  procedure av_set_pts_info (s: PAVStream; pts_wrap_bits: integer;
                     pts_num: integer; pts_den: integer);
    cdecl; external av__format;

const
  AVSEEK_FLAG_BACKWARD =1; ///< seek backward
  AVSEEK_FLAG_BYTE     =2; ///< seeking based on position in bytes
  AVSEEK_FLAG_ANY      =4; ///< seek to any frame, even non keyframes

  function av_find_default_stream_index (s: PAVFormatContext): integer;
    cdecl; external av__format;
  function av_index_search_timestamp (st: PAVStream; timestamp: int64; flags: integer): integer;
    cdecl; external av__format;
  function av_add_index_entry (st: PAVStream; pos: int64; timestamp: int64;
                    distance: integer; flags: integer): integer;
    cdecl; external av__format;
  function av_seek_frame_binary (s: PAVFormatContext; stream_index: integer;
                   target_ts: int64; flags: integer): integer;
    cdecl; external av__format;

  procedure av_update_cur_dts (s: PAVFormatContext; ref_st: PAVStream;
                               timestamp: int64);
    cdecl; external av__format;

(* media file output *)
  function av_set_parameters (s: PAVFormatContext; ap: PAVFormatParameters): integer;
    cdecl; external av__format;

  function av_write_header (s: PAVFormatContext): integer;
    cdecl; external av__format;

  function av_write_frame(s: PAVFormatContext; pkt: PAVPacket): integer;
    cdecl; external av__format;

  function av_interleaved_write_frame (s: PAVFormatContext; pkt: PAVPacket): integer;
    cdecl; external av__format;

  function av_interleave_packet_per_dts(s: PAVFormatContext; _out: PAVPacket;
                                        pkt: PAVPacket; flush: integer): integer;
    cdecl; external av__format;
  function av_write_trailer(s: pAVFormatContext): integer;
    cdecl; external av__format;

  procedure dump_format(ic: PAVFormatContext; index: integer; url: pchar;
                 is_output: integer);
    cdecl; external av__format;

  function parse_image_size(width_ptr: PInteger; height_ptr: PInteger; str: pchar): integer;
    cdecl; external av__format;
  function parse_frame_rate (frame_rate: PInteger; frame_rate_base: PInteger; arg: pchar): integer;
    cdecl; external av__format;
  function parse_date(datestr: pchar; duration: integer): int64;
    cdecl; external av__format;
  function av_gettime (): int64;
    cdecl; external av__format;

(* ffm specific for ffserver *)
const
  FFM_PACKET_SIZE = 4096;

  {$IFNDEF FPC}
  function ffm_read_write_index (fd: integer): int64;
    cdecl; external av__format;

  procedure ffm_write_write_index(fd: integer; pos: int64);
    cdecl; external av__format;

  procedure ffm_set_write_index (s: PAVFormatContext; pos: int64; file_size: int64);
    cdecl; external av__format;
  {$ENDIF}
  
  function find_info_tag (arg: pchar; arg_size: integer; tag1: pchar; info: pchar): integer;
    cdecl; external av__format;

  {$IFNDEF FPC}
  function get_frame_filename(buf: pchar; buf_size: integer;
                       path: pchar; number: integer): integer;
    cdecl; external av__format;

  function filename_number_test (filename: pchar): integer;
    cdecl; external av__format;

(* grab specific *)
  function video_grab_init (): integer;
    cdecl; external av__format;

  function audio_init (): integer;
    cdecl; external av__format;

(* DV1394 *)
  function dv1394_init (): integer;
    cdecl; external av__format;
  function dc1394_init (): integer;
    cdecl; external av__format;
  {$ENDIF}

  function strstart(str: pchar; val: pchar; ptr: PPointer): integer;
    cdecl; external av__format;
  function stristart(str: pchar; val: pchar; ptr: PPointer): integer;
    cdecl; external av__format;
  procedure pstrcpy(buf: pchar; buf_size: integer; str: pchar);
    cdecl; external av__format;
  function pstrcat(buf: pchar; buf_size: integer; s: pchar): pchar;
    cdecl; external av__format;

  procedure __dynarray_add (tab_ptr: PPointer; nb_ptr: PInteger; elem: cardinal);
    cdecl; external av__format;


implementation

procedure av_init_packet (pkt: PAVPacket);
begin
  pkt.pts   := AV_NOPTS_VALUE;
  pkt.dts   := AV_NOPTS_VALUE;
  pkt.pos   := -1;
  pkt.duration := 0;
  pkt.flags := 0;
  pkt.stream_index := 0;
  pkt.destruct := @av_destruct_packet_nofree;
end;

procedure av_free_packet (pkt: PAVPacket);
begin
  if (pkt <> nil) and (@pkt^.destruct <> nil) then
    pkt^.destruct (pkt);
end;

end.
