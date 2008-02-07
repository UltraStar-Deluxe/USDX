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
 *)

(* This is a part of Pascal porting of ffmpeg.  Originally by Victor Zinetz for Delphi and Free Pascal on Windows.
 * For Mac OS X, some modifications were made by The Creative CAT, denoted as CAT
 * in the source codes *)

(*
 * Min. version: 50.5.0
 * Max. version: 52.7.0, Revision: 11661, Tue Jan 29 09:25:49 2008 UTC
 *)

unit avformat;

{$IFDEF FPC}
  {$MODE DELPHI }
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

interface

uses
  avcodec,
  avutil,
  avio,
  rational,
  UConfig;

const
  (* Max. supported version by this header *)
  LIBAVFORMAT_MAX_VERSION_MAJOR   = 52;
  LIBAVFORMAT_MAX_VERSION_MINOR   = 7;
  LIBAVFORMAT_MAX_VERSION_RELEASE = 0;
  LIBAVFORMAT_MAX_VERSION = (LIBAVFORMAT_MAX_VERSION_MAJOR * VERSION_MAJOR) +
                            (LIBAVFORMAT_MAX_VERSION_MINOR * VERSION_MINOR) +
                            (LIBAVFORMAT_MAX_VERSION_RELEASE * VERSION_RELEASE);

(* Check if linked versions are supported *)
{$IF (LIBAVFORMAT_VERSION > LIBAVFORMAT_MAX_VERSION)}
  {$MESSAGE Warn 'Linked version of libavformat may be unsupported!'}
{$IFEND}

type
  PAVFile = Pointer;

(* packet functions *)

type
  PAVPacket = ^TAVPacket;
  TAVPacket = record
    pts: int64;                            ///< presentation time stamp in time_base units
    dts: int64;                            ///< decompression time stamp in time_base units
    data: PByte;
    size: integer;
    stream_index: integer;
    flags: integer;
    duration: integer;                     ///< presentation duration in time_base units (0 if not available)
    destruct: procedure (p: PAVPacket); cdecl; 
    priv: pointer;
    pos: int64;                            ///< byte position in stream, -1 if unknown
  end;
const
  PKT_FLAG_KEY = $0001;

procedure av_destruct_packet_nofree (var pkt: TAVPacket);
  cdecl; external av__format;

(**
 * Default packet destructor.
 *)
procedure av_destruct_packet (var pkt: TAVPacket);
  cdecl; external av__format;

(**
 * Initialize optional fields of a packet to default values.
 *
 * @param pkt packet
 *)
procedure av_init_packet (var pkt: TAVPacket);
{$IF LIBAVFORMAT_VERSION >= 51012002} // 51.12.2
  cdecl; external av__format;
{$IFEND}

(**
 * Allocate the payload of a packet and initialize its fields to default values.
 *
 * @param pkt packet
 * @param size wanted payload size
 * @return 0 if OK. AVERROR_xxx otherwise.
 *)
function av_new_packet(var pkt: TAVPacket; size: integer): integer;
  cdecl; external av__format;

(**
 * Allocate and read the payload of a packet and initialize its fields to default values.
 *
 * @param pkt packet
 * @param size wanted payload size
 * @return >0 (read size) if OK. AVERROR_xxx otherwise.
 *)
function av_get_packet (s: PByteIOContext; var pkt: TAVPacket; size: integer): integer;
  cdecl; external av__format;

(**
 * @warning This is a hack - the packet memory allocation stuff is broken. The
 * packet is allocated if it was not really allocated
 *)
function av_dup_packet (pkt: PAVPacket): integer;
  cdecl; external av__format;

(**
 * Free a packet
 *
 * @param pkt packet to free
 *)
procedure av_free_packet (pkt: PAVPacket); inline;

(*************************************************)
(* fractional numbers for exact pts handling *)

type
 (**
  * the exact value of the fractional number is: 'val + num / den'.
  * num is assumed to be such as 0 <= num < den
  * @deprecated Use AVRational instead
  *)
  PAVFrac = ^TAVFrac;
  TAVFrac = record
    val, num, den: int64;
  end; {deprecated}

(*************************************************)
(* input/output formats *)

type
  (* this structure contains the data a format has to probe a file *)
  TAVProbeData = record
    filename: pchar;
    buf: pchar;
    buf_size: integer;
  end;

const
  AVPROBE_SCORE_MAX   = 100;   ///< max score, half of that is used for file extension based detection
  AVPROBE_PADDING_SIZE = 32;   ///< extra allocated bytes at the end of the probe buffer

  //! demuxer will use url_fopen, no opened file should be provided by the caller
  AVFMT_NOFILE        = $0001;
  AVFMT_NEEDNUMBER    = $0002; (**< needs '%d' in filename *)
  AVFMT_SHOW_IDS      = $0008; (**< show format stream IDs numbers *)
  AVFMT_RAWPICTURE    = $0020; (**< format wants AVPicture structure for
                                      raw picture data *)
  AVFMT_GLOBALHEADER  = $0040; (**< format wants global header *)
  AVFMT_NOTIMESTAMPS  = $0080; (**< format does not need / have any timestamps *)
  AVFMT_GENERIC_INDEX = $0100; (**< use generic index building code *)

  // used by AVIndexEntry
  AVINDEX_KEYFRAME = $0001;

  AVFMTCTX_NOHEADER = $0001; (**< signal that no header is present
                                         (streams are added dynamically) *)
  MAX_STREAMS = 20;

  AVFMT_NOOUTPUTLOOP = -1;
  AVFMT_INFINITEOUTPUTLOOP = 0;
  AVFMT_FLAG_GENPTS = $0001; ///< generate pts if missing even if it requires parsing future frames
  AVFMT_FLAG_IGNIDX = $0002; ///< ignore index
  AVFMT_FLAG_NONBLOCK = $0004; ///< do not block when reading packets from input

  // used by AVStream
  MAX_REORDER_DELAY = 4;

  // used by TAVProgram
  AV_PROGRAM_RUNNING = 1;

type
  PPAVCodecTag = ^PAVCodecTag;
  PAVCodecTag = Pointer;

  PPAVFormatContext = ^PAVFormatContext;
  PAVFormatContext = ^TAVFormatContext;

  PAVFormatParameters = ^TAVFormatParameters;

  PAVOutputFormat = ^TAVOutputFormat;
  PAVProbeData = ^TAVProbeData;

  PAVInputFormat = ^TAVInputFormat;
  PAVIndexEntry = ^TAVIndexEntry;

  PAVStream = ^TAVStream;
  PAVPacketList = ^TAVPacketList;

  PPAVProgram = ^PAVProgram;
  PAVProgram = ^TAVProgram;

  {$IF LIBAVFORMAT_VERSION < 51006000} // 51.6.0
  PAVImageFormat = ^TAVImageFormat;
  PAVImageInfo = ^TAVImageInfo;
  {$IFEND}

  TAVFormatParameters = record
    time_base: TAVRational;
    sample_rate: integer;
    channels: integer;
    width: integer;
    height: integer;
    pix_fmt: TAVPixelFormat;
    {$IF LIBAVFORMAT_VERSION < 51006000} // 51.6.0
    image_format: PAVImageFormat;
    {$IFEND}
    channel: integer; (* used to select dv channel *)
    {$IF LIBAVFORMAT_VERSION_MAJOR < 52}
    device: pchar; (* video, audio or DV device, if LIBAVFORMAT_VERSION_INT < (52<<16) *)
    {$IFEND}
    standard: pchar; (* tv standard, NTSC, PAL, SECAM *)
    { Delphi does not support bit fields -> use bf_flags instead
    int mpeg2ts_raw:1;  (* force raw MPEG2 transport stream output, if possible *)
    int mpeg2ts_compute_pcr:1; (* compute exact PCR for each transport
                                  stream packet (only meaningful if
                                  mpeg2ts_raw is TRUE *)
    int initial_pause:1;       (* do not begin to play the stream
                                  immediately (RTSP only) *)
    int prealloced_context:1;
    }
    bf_flags: byte; // 0:mpeg2ts_raw/1:mpeg2ts_compute_pcr/2:initial_pause/3:prealloced_context
    {$IF LIBAVFORMAT_VERSION_MAJOR < 53}
    video_codec_id: TCodecID;
    audio_codec_id: TCodecID;
    {$IFEND}
  end;

  TAVOutputFormat = record
    name: pchar;
    long_name: pchar;
    mime_type: pchar;
    extensions: pchar; (*< comma separated filename extensions *)
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
    interleave_packet: function (s: PAVFormatContext; out_: PAVPacket; in_: PAVPacket; flush: integer): integer; cdecl;

    {$IF LIBAVFORMAT_VERSION >= 51008000} // 51.8.0
    (**
     * list of supported codec_id-codec_tag pairs, ordered by "better choice first"
     * the arrays are all CODEC_ID_NONE terminated
     *)
    codec_tag: {const} PPAVCodecTag;
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 51012002} // 51.12.2
    subtitle_codec: TCodecID; (**< default subtitle codec *)
    {$IFEND}

    (* private fields *)
    next: PAVOutputFormat;
  end;

  TAVInputFormat = record
    name: pchar;
    long_name: pchar;
    (* size of private data so that it can be allocated in the wrapper *)
    priv_data_size: integer;
    (**
     * Tell if a given file has a chance of being parsed by this format.
     * The buffer provided is guaranteed to be AVPROBE_PADDING_SIZE bytes
     * big so you do not have to check for that unless you need more.
     *)
    read_probe: function (p: PAVProbeData): integer; cdecl;
    (* read the format header and initialize the AVFormatContext
       structure. Return 0 if OK. 'ap' if non NULL contains
       additionnal paramters. Only used in raw format right
       now. 'av_new_stream' should be called to create new streams.  *)
    read_header: function (c: PAVFormatContext; ap: PAVFormatParameters): integer; cdecl;
    (* read one packet and put it in 'pkt'. pts and flags are also
       set. 'av_new_stream' can be called only if the flag
       AVFMTCTX_NOHEADER is used. *)
    read_packet: function (c: PAVFormatContext; var pkt: TAVPacket): integer; cdecl;
    (* close the stream. The AVFormatContext and AVStreams are not
       freed by this function *)
    read_close: function (c: PAVFormatContext): integer; cdecl;
    (**
     * seek to a given timestamp relative to the frames in
     * stream component stream_index
     * @param stream_index must not be -1
     * @param flags selects which direction should be preferred if no exact
     *              match is available
     * @return >= 0 on success (but not necessarily the new offset)
     *)
    read_seek: function (c: PAVFormatContext; stream_index: integer;
                  timestamp: int64; flags: integer): integer; cdecl;
    (**
     * gets the next timestamp in stream[stream_index].time_base units.
     * @return the timestamp or AV_NOPTS_VALUE if an error occured
     *)
    read_timestamp: function (s: PAVFormatContext; stream_index: integer;
                              pos: pint64; pos_limit: int64): int64; cdecl;
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

    {$IF LIBAVFORMAT_VERSION >= 51008000} // 51.8.0
    codec_tag: {const} PPAVCodecTag;
    {$IFEND}

    (* private fields *)
    next: PAVInputFormat;
  end;

  TAVStreamParseType = (
    AVSTREAM_PARSE_NONE,
    AVSTREAM_PARSE_FULL,       (**< full parsing and repack *)
    AVSTREAM_PARSE_HEADERS,    (**< only parse headers, don't repack *)
    AVSTREAM_PARSE_TIMESTAMPS  (**< full parsing and interpolation of timestamps for frames not starting on packet boundary *)
  );

  TAVIndexEntry = record
    pos: int64;
    timestamp: int64;
    { Delphi doesn't support bitfields -> use flags_size instead
    int flags:2;
    int size:30; //Yeah, trying to keep the size of this small to reduce memory requirements (it is 24 vs 32 byte due to possible 8byte align).
    }
    flags_size: cardinal; // 0..1: flags, 2..31: size
    min_distance: integer;         (* min distance between this and the previous keyframe, used to avoid unneeded searching *)
  end;

 (**
  * Stream structure.
  * New fields can be added to the end with minor version bumps.
  * Removal, reordering and changes to existing fields require a major
  * version bump.
  * sizeof(AVStream) must not be used outside libav*.
  *)
  TAVStream = record
    index: integer;    (* stream index in AVFormatContext *)
    id: integer;       (* format specific stream id *)
    codec: PAVCodecContext; (* codec context *)
    (**
     * Real base frame rate of the stream.
     * This is the lowest frame rate with which all timestamps can be
     * represented accurately (it is the least common multiple of all
     * frame rates in the stream), Note, this value is just a guess!
     * For example if the timebase is 1/90000 and all frames have either
     * approximately 3600 or 1800 timer ticks then r_frame_rate will be 50/1.
     *)
    r_frame_rate: TAVRational;
    priv_data: pointer;

    (* internal data used in av_find_stream_info() *)
    first_dts: int64;
    {$IF LIBAVFORMAT_VERSION_MAJOR < 52}
    codec_info_nb_frames: integer;
    {$IFEND}

    (** encoding: PTS generation when outputing stream *)
    pts: TAVFrac;
    (**
     * This is the fundamental unit of time (in seconds) in terms
     * of which frame timestamps are represented. For fixed-fps content,
     * timebase should be 1/frame rate and timestamp increments should be
     * identically 1.
     *)
    time_base: TAVRational;
    pts_wrap_bits: integer; (* number of bits in pts (used for wrapping control) *)
    (* ffmpeg.c private use *)
    stream_copy: integer; (**< if set, just copy stream *)
    discard: TAVDiscard; ///< selects which packets can be discarded at will and dont need to be demuxed
    //FIXME move stuff to a flags field?
    (* quality, as it has been removed from AVCodecContext and put in AVVideoFrame
     * MN:dunno if thats the right place, for it *)
    quality: single;
    (**
     * Decoding: pts of the first frame of the stream, in stream time base.
     * Only set this if you are absolutely 100% sure that the value you set
     * it to really is the pts of the first frame.
     * This may be undefined (AV_NOPTS_VALUE).
     * @note The ASF header does NOT contain a correct start_time the ASF
     * demuxer must NOT set this.
     *)
    start_time: int64;
    (**
     * Decoding: duration of the stream, in stream time base.
     * If a source file does not specify a duration, but does specify
     * a bitrate, this value will be estimates from bit rate and file size.
     *)
    duration: int64;

    language: array [0..3] of char; (* ISO 639 3-letter language code (empty string if undefined) *)

    (* av_read_frame() support *)
    need_parsing: TAVStreamParseType;
    parser: PAVCodecParserContext;

    cur_dts: int64;
    last_IP_duration: integer;
    last_IP_pts: int64;
    (* av_seek_frame() support *)
    index_entries: PAVIndexEntry; (* only used if the format does not support seeking natively *)
    nb_index_entries: integer;
    index_entries_allocated_size: cardinal;

    nb_frames: int64;                 ///< number of frames in this stream if known or 0
    
    {$IF LIBAVFORMAT_VERSION >= 50006000} // 50.6.0
    pts_buffer: array [0..MAX_REORDER_DELAY] of int64;
    {$IFEND}
    
    {$IF LIBAVFORMAT_VERSION >= 52006000} // 52.6.0
    filename: PChar; (**< source filename of the stream *)
    {$IFEND}
  end;

 (**
  * format I/O context.
  * New fields can be added to the end with minor version bumps.
  * Removal, reordering and changes to existing fields require a major
  * version bump.
  * sizeof(AVFormatContext) must not be used outside libav*.
  *)
  TAVFormatContext = record
    av_class: PAVClass; (* set by av_alloc_format_context *)
    (* can only be iformat or oformat, not both at the same time *)
    iformat: PAVInputFormat;
    oformat: PAVOutputFormat;
    priv_data: pointer;

    {$IF LIBAVFORMAT_VERSION_MAJOR >= 52}
    pb: PByteIOContext;
    {$ELSE}
    pb: TByteIOContext;
    {$IFEND}

    nb_streams: cardinal;
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
    cur_ptr: pbyte;
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

    {$IF LIBAVFORMAT_VERSION >= 50006000} // 50.6.0
    (* decoding: size of data to probe; encoding unused *)
    probesize: cardinal;
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 51009000} // 51.9.0
    (**
     * maximum duration in AV_TIME_BASE units over which the input should be analyzed in av_find_stream_info()
     *)
    max_analyze_duration: integer;

    key: pbyte;
    keylen : integer;
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 51014000} // 51.14.0
    nb_programs: cardinal;
    programs: PPAVProgram;
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 52003000} // 52.3.0
    (**
     * Forced video codec_id.
     * demuxing: set by user
     *)
    video_codec_id: TCodecID;
    (**
     * Forced audio codec_id.
     * demuxing: set by user
     *)
    audio_codec_id: TCodecID;
    (**
     * Forced subtitle codec_id.
     * demuxing: set by user
     *)
    subtitle_codec_id: TCodecID;
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 52004000} // 52.4.0
    (**
     * Maximum amount of memory in bytes to use per stream for the index.
     * If the needed index exceeds this size entries will be discarded as
     * needed to maintain a smaller size. This can lead to slower or less
     * accurate seeking (depends on demuxer).
     * Demuxers for which a full in memory index is mandatory will ignore
     * this.
     * muxing  : unused
     * demuxing: set by user
     *)
    max_index_size: cardinal;
    {$IFEND}
  end;

  (**
   * New fields can be added to the end with minor version bumps.
   * Removal, reordering and changes to existing fields require a major
   * version bump.
   * sizeof(AVProgram) must not be used outside libav*.
   *)
  TAVProgram = record
      id                : integer;
      provider_name     : PChar;      ///< Network name for DVB streams
      name              : PChar;      ///< Service name for DVB streams
      flags             : integer;
      discard           : TAVDiscard; ///< selects which program to discard and which to feed to the caller
      {$IF LIBAVFORMAT_VERSION >= 51016000} // 51.16.0
      stream_index      : PCardinal;
      nb_stream_indexes : PCardinal;
      {$IFEND}
  end;

  TAVPacketList = record
    pkt: TAVPacket;
    next: PAVPacketList;
  end;

{$IF LIBAVFORMAT_VERSION < 51006000} // 51.6.0
  (* still image support *)
  PAVInputImageContext = pointer; {deprecated}

  (* still image support *)
  TAVImageInfo = record
    pix_fmt: TAVPixelFormat; (* requested pixel format *)
    width: integer; (* requested width *)
    height: integer; (* requested height *)
    interleaved: integer; (* image is interleaved (e.g. interleaved GIF) *)
    pict: TAVPicture; (* returned allocated image *)
  end; {deprecated}

  TAVImageFormat = record
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
  end; {deprecated}

procedure av_register_image_format (img_fmt: PAVImageFormat);
    cdecl; external av__format; deprecated;

function av_probe_image_format (pd: PAVProbeData): PAVImageFormat;
    cdecl; external av__format; deprecated;

function guess_image_format (filename: pchar): PAVImageFormat;
    cdecl; external av__format; deprecated;

function av_read_image (pb: PByteIOContext; filename: pchar;
                  fmt: PAVImageFormat;
                  alloc_cb: pointer; opaque: pointer): integer;
    cdecl; external av__format; deprecated;

function av_write_image(pb: PByteIOContext; fmt: PAVImageFormat; img: PAVImageInfo): integer;
    cdecl; external av__format; deprecated;
{$IFEND}

{$IF LIBAVFORMAT_VERSION_MAJOR < 53}
{
var
  first_iformat: PAVInputFormat; external av__format;
  first_oformat: PAVOutputFormat; external av__format;
}
{$IFEND}

{$IF LIBAVFORMAT_VERSION >= 52003000} // 52.3.0
function av_iformat_next(f: PAVInputFormat): PAVInputFormat;
    cdecl; external av__format;
function av_oformat_next(f: PAVOutputFormat): PAVOutputFormat;
    cdecl; external av__format;
{$IFEND}

function av_guess_image2_codec({const} filename: PChar): TCodecID;
    cdecl; external av__format;

(* XXX: use automatic init with either ELF sections or C file parser *)
(* modules *)

(* utils.c *)
procedure av_register_input_format (format: PAVInputFormat);
  cdecl; external av__format;

procedure av_register_output_format (format: PAVOutputFormat);
  cdecl; external av__format;

function guess_stream_format (short_name: pchar; filename: pchar; mime_type: pchar): PAVOutputFormat;
  cdecl; external av__format;

function guess_format(short_name: pchar; filename: pchar; mime_type: pchar): PAVOutputFormat;
  cdecl; external av__format;

(**
 * Guesses the codec id based upon muxer and filename.
 *)
function av_guess_codec(fmt: PAVOutputFormat; short_name: pchar;
                        filename: pchar; mime_type: pchar; type_: TCodecType): TCodecID;
  cdecl; external av__format;

(**
 * Send a nice hexadecimal dump of a buffer to the specified file stream.
 *
 * @param f The file stream pointer where the dump should be sent to.
 * @param buf buffer
 * @param size buffer size
 *
 * @see av_hex_dump_log, av_pkt_dump, av_pkt_dump_log
 *)
procedure av_hex_dump (f: PAVFile; buf: pchar; size: integer);
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 51011000} // 51.11.0
(**
 * Send a nice hexadecimal dump of a buffer to the log.
 *
 * @param avcl A pointer to an arbitrary struct of which the first field is a
 * pointer to an AVClass struct.
 * @param level The importance level of the message, lower values signifying
 * higher importance.
 * @param buf buffer
 * @param size buffer size
 *
 * @see av_hex_dump, av_pkt_dump, av_pkt_dump_log
 *)
procedure av_hex_dump_log(avcl: Pointer; level: integer; buf: PChar; size: integer);
  cdecl; external av__format;
{$IFEND}

(**
 * Send a nice dump of a packet to the specified file stream.
 *
 * @param f The file stream pointer where the dump should be sent to.
 * @param pkt packet to dump
 * @param dump_payload true if the payload must be displayed too
 *)
procedure av_pkt_dump(f: PAVFile; pkt: PAVPacket; dump_payload: integer);
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 51011000} // 51.11.0
(**
 * Send a nice dump of a packet to the log.
 *
 * @param avcl A pointer to an arbitrary struct of which the first field is a
 * pointer to an AVClass struct.
 * @param level The importance level of the message, lower values signifying
 * higher importance.
 * @param pkt packet to dump
 * @param dump_payload true if the payload must be displayed too
 *)
procedure av_pkt_dump_log(avcl: Pointer; level: integer; pkt: PAVPacket; dump_payload: integer);
  cdecl; external av__format;
{$IFEND}

procedure av_register_all ();
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 51008000} // 51.8.0
(** codec tag <-> codec id *)
function av_codec_get_id(var tags: PAVCodecTag; tag: cardinal): TCodecID;
  cdecl; external av__format;
function av_codec_get_tag(var tags: PAVCodecTag; id: TCodecID): cardinal;
  cdecl; external av__format;
{$IFEND}

(* media file input *)

(**
 * finds AVInputFormat based on input format's short name.
 *)
function av_find_input_format (short_name: pchar): PAVInputFormat;
  cdecl; external av__format;

(**
 * Guess file format.
 *
 * @param is_opened whether the file is already opened, determines whether
 *                  demuxers with or without AVFMT_NOFILE are probed
 *)
function av_probe_input_format (pd: PAVProbeData; is_opened: integer): PAVInputFormat;
  cdecl; external av__format;

(**
 * Allocates all the structures needed to read an input stream.
 *        This does not open the needed codecs for decoding the stream[s].
 *)
function av_open_input_stream (ic_ptr: PAVFormatContext;
                       pb: PByteIOContext; filename: pchar;
                       fmt: PAVInputFormat; ap: PAVFormatParameters): integer;
  cdecl; external av__format;

(**
 * Open a media file as input. The codecs are not opened. Only the file
 * header (if present) is read.
 *
 * @param ic_ptr the opened media file handle is put here
 * @param filename filename to open.
 * @param fmt if non NULL, force the file format to use
 * @param buf_size optional buffer size (zero if default is OK)
 * @param ap additional parameters needed when opening the file (NULL if default)
 * @return 0 if OK. AVERROR_xxx otherwise.
 *)
function av_open_input_file (var ic_ptr: PAVFormatContext; filename: pchar;
                     fmt: PAVInputFormat; buf_size: integer;
                     ap: PAVFormatParameters): integer;
  cdecl; external av__format;

(**
 * Allocate an AVFormatContext.
 * Can be freed with av_free() but do not forget to free everything you
 * explicitly allocated as well!
 *)
function av_alloc_format_context (): PAVFormatContext;
  cdecl; external av__format;

(**
 * Read packets of a media file to get stream information. This
 * is useful for file formats with no headers such as MPEG. This
 * function also computes the real frame rate in case of mpeg2 repeat
 * frame mode.
 * The logical file position is not changed by this function;
 * examined packets may be buffered for later processing.
 *
 * @param ic media file handle
 * @return >=0 if OK. AVERROR_xxx if error.
 * @todo Let user decide somehow what information is needed so we do not waste time getting stuff the user does not need.
 *)
function av_find_stream_info (ic: PAVFormatContext): integer;
  cdecl; external av__format;

(**
 * Read a transport packet from a media file.
 *
 * This function is obsolete and should never be used.
 * Use av_read_frame() instead.
 *
 * @param s media file handle
 * @param pkt is filled
 * @return 0 if OK. AVERROR_xxx if error.
 *)
function av_read_packet (s: PAVFormatContext; var pkt: TAVPacket): integer;
  cdecl; external av__format;

(**
 * Return the next frame of a stream.
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
 * values in AVStream.timebase units (and guessed if the format cannot
 * provided them). pkt->pts can be AV_NOPTS_VALUE if the video format
 * has B frames, so it is better to rely on pkt->dts if you do not
 * decompress the payload.
 *
 * @return 0 if OK, < 0 if error or end of file.
 *)
function av_read_frame (s: PAVFormatContext; var pkt: TAVPacket): integer; (* CAT#2 *)
  cdecl; external av__format;

(**
 * Seek to the key frame at timestamp.
 * 'timestamp' in 'stream_index'.
 * @param stream_index If stream_index is (-1), a default
 * stream is selected, and timestamp is automatically converted
 * from AV_TIME_BASE units to the stream specific time_base.
 * @param timestamp timestamp in AVStream.time_base units
 *        or if there is no stream specified then in AV_TIME_BASE units
 * @param flags flags which select direction and seeking mode
 * @return >= 0 on success
 *)
function av_seek_frame (s: PAVFormatContext; stream_index: integer; timestamp: int64; flags: integer): integer;
  cdecl; external av__format;

(**
 * start playing a network based stream (e.g. RTSP stream) at the
 * current position
 *)
function av_read_play (s: PAVFormatContext): integer;
  cdecl; external av__format;

(**
 * Pause a network based stream (e.g. RTSP stream).
 *
 * Use av_read_play() to resume it.
 *)
function av_read_pause (s: PAVFormatContext): integer;
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 52003000} // 52.3.0
(**
 * Free a AVFormatContext allocated by av_open_input_stream.
 * @param s context to free
 *)
procedure av_close_input_stream(s: PAVFormatContext);
  cdecl; external av__format;
{$IFEND}

(**
 * Close a media file (but not its codecs).
 *
 * @param s media file handle
 *)
procedure av_close_input_file (s: PAVFormatContext);
  cdecl; external av__format;

(**
 * Add a new stream to a media file.
 *
 * Can only be called in the read_header() function. If the flag
 * AVFMTCTX_NOHEADER is in the format context, then new streams
 * can be added in read_packet too.
 *
 * @param s media file handle
 * @param id file format dependent stream id
 *)
function av_new_stream (s: PAVFormatContext; id: integer): PAVStream;
  cdecl; external av__format;
{$IF LIBAVFORMAT_VERSION >= 51014000} // 51.14.0
function av_new_program(s: PAVFormatContext; id: integer): PAVProgram;
  cdecl; external av__format;
{$IFEND}

(**
 * Set the pts for a given stream.
 *
 * @param s stream
 * @param pts_wrap_bits number of bits effectively used by the pts
 *        (used for wrap control, 33 is the value for MPEG)
 * @param pts_num numerator to convert to seconds (MPEG: 1)
 * @param pts_den denominator to convert to seconds (MPEG: 90000)
 *)
procedure av_set_pts_info (s: PAVStream; pts_wrap_bits: integer;
                   pts_num: integer; pts_den: integer);
  cdecl; external av__format;

const
  AVSEEK_FLAG_BACKWARD = 1; ///< seek backward
  AVSEEK_FLAG_BYTE     = 2; ///< seeking based on position in bytes
  AVSEEK_FLAG_ANY      = 4; ///< seek to any frame, even non keyframes

function av_find_default_stream_index (s: PAVFormatContext): integer;
  cdecl; external av__format;

(**
 * Gets the index for a specific timestamp.
 * @param flags if AVSEEK_FLAG_BACKWARD then the returned index will correspond to
 *                 the timestamp which is <= the requested one, if backward is 0
 *                 then it will be >=
 *              if AVSEEK_FLAG_ANY seek to any frame, only keyframes otherwise
 * @return < 0 if no such timestamp could be found
 *)
function av_index_search_timestamp (st: PAVStream; timestamp: int64; flags: integer): integer;
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 52004000} // 52.4.0
(**
 * Ensures the index uses less memory than the maximum specified in
 * AVFormatContext.max_index_size, by discarding entries if it grows
 * too large.
 * This function is not part of the public API and should only be called
 * by demuxers.
 *)
procedure ff_reduce_index(s: PAVFormatContext; stream_index: integer);
  cdecl; external av__format;
{$IFEND}

(**
 * Add a index entry into a sorted list updateing if it is already there.
 *
 * @param timestamp timestamp in the timebase of the given stream
 *)
function av_add_index_entry (st: PAVStream; pos: int64; timestamp: int64;
                  distance: integer; flags: integer): integer;
  cdecl; external av__format;

(**
 * Does a binary search using av_index_search_timestamp() and AVCodec.read_timestamp().
 * This is not supposed to be called directly by a user application, but by demuxers.
 * @param target_ts target timestamp in the time base of the given stream
 * @param stream_index stream number
 *)
function av_seek_frame_binary (s: PAVFormatContext; stream_index: integer;
                 target_ts: int64; flags: integer): integer;
  cdecl; external av__format;


(**
 * Updates cur_dts of all streams based on given timestamp and AVStream.
 *
 * Stream ref_st unchanged, others set cur_dts in their native timebase
 * only needed for timestamp wrapping or if (dts not set and pts!=dts).
 * @param timestamp new dts expressed in time_base of param ref_st
 * @param ref_st reference stream giving time_base of param timestamp
 *)
procedure av_update_cur_dts (s: PAVFormatContext; ref_st: PAVStream;
                             timestamp: int64);
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 51007000} // 51.7.0
type
  TReadTimestampFunc = function (pavfc: PAVFormatContext;
    arg2: integer; arg3: Pint64; arg4: int64): int64; cdecl;

(**
 * Does a binary search using read_timestamp().
 * This is not supposed to be called directly by a user application, but by demuxers.
 * @param target_ts target timestamp in the time base of the given stream
 * @param stream_index stream number
 *)
function av_gen_search(s: PAVFormatContext; stream_index: integer; target_ts: int64;
    pos_min: int64; pos_max: int64; pos_limit: int64; ts_min: int64; ts_max: int64;
    flags: integer; ts_ret: Pint64; read_timestamp: TReadTimestampFunc): int64;
  cdecl; external av__format;
{$IFEND}

(* media file output *)
function av_set_parameters (s: PAVFormatContext; ap: PAVFormatParameters): integer;
  cdecl; external av__format;

(**
 * Allocate the stream private data and write the stream header to an
 * output media file.
 *
 * @param s media file handle
 * @return 0 if OK. AVERROR_xxx if error.
 *)
function av_write_header (s: PAVFormatContext): integer;
  cdecl; external av__format;

(**
 * Write a packet to an output media file.
 *
 * The packet shall contain one audio or video frame.
 * The packet must be correctly interleaved according to the container specification,
 * if not then av_interleaved_write_frame must be used
 *
 * @param s media file handle
 * @param pkt the packet, which contains the stream_index, buf/buf_size, dts/pts, ...
 * @return < 0 if error, = 0 if OK, 1 if end of stream wanted.
 *)
function av_write_frame(s: PAVFormatContext; var pkt: TAVPacket): integer;
  cdecl; external av__format; (* CAT#2 *)

(**
 * Writes a packet to an output media file ensuring correct interleaving.
 *
 * The packet must contain one audio or video frame.
 * If the packets are already correctly interleaved the application should
 * call av_write_frame() instead as it is slightly faster. It is also important
 * to keep in mind that completely non-interleaved input will need huge amounts
 * of memory to interleave with this, so it is preferable to interleave at the
 * demuxer level.
 *
 * @param s media file handle
 * @param pkt the packet, which contains the stream_index, buf/buf_size, dts/pts, ...
 * @return < 0 if error, = 0 if OK, 1 if end of stream wanted.
 *)
function av_interleaved_write_frame (s: PAVFormatContext; var pkt: TAVPacket): integer;
  cdecl; external av__format; (* CAT#2 *)

(**
 * Interleave a packet per DTS in an output media file.
 *
 * Packets with pkt->destruct == av_destruct_packet will be freed inside this function,
 * so they cannot be used after it, note calling av_free_packet() on them is still safe.
 *
 * @param s media file handle
 * @param out the interleaved packet will be output here
 * @param in the input packet
 * @param flush 1 if no further packets are available as input and all
 *              remaining packets should be output
 * @return 1 if a packet was output, 0 if no packet could be output,
 *         < 0 if an error occured
 *)
function av_interleave_packet_per_dts(s: PAVFormatContext; _out: PAVPacket;
                                      pkt: PAVPacket; flush: integer): integer;
  cdecl; external av__format;

(**
 * @brief Write the stream trailer to an output media file and
 *        free the file private data.
 *
 * @param s media file handle
 * @return 0 if OK. AVERROR_xxx if error.
 *)
function av_write_trailer(s: pAVFormatContext): integer;
  cdecl; external av__format;

procedure dump_format(ic: PAVFormatContext; index: integer; url: pchar;
               is_output: integer);
  cdecl; external av__format;

(**
 * parses width and height out of string str.
 * @deprecated Use av_parse_video_frame_size instead.
 *)
function parse_image_size(width_ptr: PInteger; height_ptr: PInteger; str: pchar): integer;
  cdecl; external av__format; deprecated;

(**
 * Converts frame rate from string to a fraction.
 * @deprecated Use av_parse_video_frame_rate instead.
 *)
function parse_frame_rate (frame_rate: PInteger; frame_rate_base: PInteger; arg: pchar): integer;
  cdecl; external av__format; deprecated;

(**
 * Parses \p datestr and returns a corresponding number of microseconds.
 * @param datestr String representing a date or a duration.
 * - If a date the syntax is:
 * @code
 *  [{YYYY-MM-DD|YYYYMMDD}]{T| }{HH[:MM[:SS[.m...]]][Z]|HH[MM[SS[.m...]]][Z]}
 * @endcode
 * Time is localtime unless Z is appended, in which case it is
 * interpreted as UTC.
 * If the year-month-day part isn't specified it takes the current
 * year-month-day.
 * Returns the number of microseconds since 1st of January, 1970 up to
 * the time of the parsed date or INT64_MIN if \p datestr cannot be
 * successfully parsed.
 * - If a duration the syntax is:
 * @code
 *  [-]HH[:MM[:SS[.m...]]]
 *  [-]S+[.m...]
 * @endcode
 * Returns the number of microseconds contained in a time interval
 * with the specified duration or INT64_MIN if \p datestr cannot be
 * successfully parsed.
 * @param duration Flag which tells how to interpret \p datestr, if
 * not zero \p datestr is interpreted as a duration, otherwise as a
 * date.
 *)
function parse_date(datestr: pchar; duration: integer): int64;
  cdecl; external av__format;

function av_gettime (): int64;
  cdecl; external av__format;

(* ffm specific for ffserver *)
const
  FFM_PACKET_SIZE = 4096;

function ffm_read_write_index (fd: integer): int64;
  cdecl; external av__format;

procedure ffm_write_write_index(fd: integer; pos: int64);
  cdecl; external av__format;

procedure ffm_set_write_index (s: PAVFormatContext; pos: int64; file_size: int64);
  cdecl; external av__format;

(**
 * Attempts to find a specific tag in a URL.
 *
 * syntax: '?tag1=val1&tag2=val2...'. Little URL decoding is done.
 * Return 1 if found.
 *)
function find_info_tag (arg: pchar; arg_size: integer; tag1: pchar; info: pchar): integer;
  cdecl; external av__format;

(**
 * Returns in 'buf' the path with '%d' replaced by number.

 * Also handles the '%0nd' format where 'n' is the total number
 * of digits and '%%'.
 *
 * @param buf destination buffer
 * @param buf_size destination buffer size
 * @param path numbered sequence string
 * @param number frame number
 * @return 0 if OK, -1 if format error.
 *)
function av_get_frame_filename(buf: pchar; buf_size: integer;
                               path: pchar; number: integer): integer;
  cdecl; external av__format
  {$IF LIBAVFORMAT_VERSION <= 50006000} // 50.6.0
  name 'get_frame_filename'
  {$IFEND};

(**
 * Check whether filename actually is a numbered sequence generator.
 *
 * @param filename possible numbered sequence string
 * @return 1 if a valid numbered sequence string, 0 otherwise.
 *)
function av_filename_number_test(filename: pchar): integer;
  cdecl; external av__format
  {$IF LIBAVFORMAT_VERSION <= 50006000} // 50.6.0
  name 'filename_number_test'
  {$IFEND};

{$IF LIBAVFORMAT_VERSION >= 51012002} // 51.12.2
(**
 * Generate an SDP for an RTP session.
 *
 * @param ac array of AVFormatContexts describing the RTP streams. If the
 *           array is composed by only one context, such context can contain
 *           multiple AVStreams (one AVStream per RTP stream). Otherwise,
 *           all the contexts in the array (an AVCodecContext per RTP stream)
 *           must contain only one AVStream
 * @param n_files number of AVCodecContexts contained in ac
 * @param buff buffer where the SDP will be stored (must be allocated by
 *             the caller
 * @param size the size of the buffer
 * @return 0 if OK. AVERROR_xxx if error.
 *)
function avf_sdp_create(ac: PPAVFormatContext; n_files: integer; buff: PChar; size: integer): integer;
  cdecl; external av__format;
{$IFEND}

implementation

{$IF LIBAVFORMAT_VERSION < 51012002} // 51.12.2
procedure av_init_packet (var pkt: TAVPacket);
begin
  with pkt do begin
    pts   := AV_NOPTS_VALUE;
    dts   := AV_NOPTS_VALUE;
    pos   := -1;
    duration := 0;
    flags := 0;
    stream_index := 0;
    destruct := @av_destruct_packet_nofree
  end
end;
{$IFEND}

procedure av_free_packet (pkt: PAVPacket);
begin
  if ((pkt <> nil) and (@pkt^.destruct <> nil)) then
    pkt^.destruct(pkt);
end;

end.
