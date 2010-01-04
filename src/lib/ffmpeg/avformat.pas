(*
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
 * Conversion of libavformat/avformat.h
 * Min. version: 50.5.0 , revision 6577,  Sat Oct 7 15:30:46 2006 UTC
 * Max. version: 52.25.0, revision 16986, Wed Feb 4 05:56:39 2009 UTC 
 *
 * update to
 * Max. version: 52.46.0, Mo Jan 4 2010 0:40:00 CET 
 * MiSchi
 *)

unit avformat;

{$IFDEF FPC}
  {$MODE DELPHI }
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

{$I switches.inc}  (* for the HasInline define *)

{$IFDEF DARWIN}
  {$linklib libavformat}
{$ENDIF}

interface

uses
  ctypes,
  avcodec,
  avutil,
  avio,
  rational,
  SysUtils,
  UConfig;

const
  (* Max. supported version by this header *)
  LIBAVFORMAT_MAX_VERSION_MAJOR   = 52;
  LIBAVFORMAT_MAX_VERSION_MINOR   = 46;
  LIBAVFORMAT_MAX_VERSION_RELEASE = 0;
  LIBAVFORMAT_MAX_VERSION = (LIBAVFORMAT_MAX_VERSION_MAJOR * VERSION_MAJOR) +
                            (LIBAVFORMAT_MAX_VERSION_MINOR * VERSION_MINOR) +
                            (LIBAVFORMAT_MAX_VERSION_RELEASE * VERSION_RELEASE);

  (* Min. supported version by this header *)
  LIBAVFORMAT_MIN_VERSION_MAJOR   = 50;
  LIBAVFORMAT_MIN_VERSION_MINOR   = 5;
  LIBAVFORMAT_MIN_VERSION_RELEASE = 0;
  LIBAVFORMAT_MIN_VERSION = (LIBAVFORMAT_MIN_VERSION_MAJOR * VERSION_MAJOR) +
                            (LIBAVFORMAT_MIN_VERSION_MINOR * VERSION_MINOR) +
                            (LIBAVFORMAT_MIN_VERSION_RELEASE * VERSION_RELEASE);

(* Check if linked versions are supported *)
{$IF (LIBAVFORMAT_VERSION < LIBAVFORMAT_MIN_VERSION)}
  {$MESSAGE Error 'Linked version of libavformat is too old!'}
{$IFEND}

(* Check if linked versions are supported *)
{$IF (LIBAVFORMAT_VERSION > LIBAVFORMAT_MAX_VERSION)}
  {$MESSAGE Error 'Linked version of libavformat is not yet supported!'}
{$IFEND}

{$IF LIBAVFORMAT_VERSION >= 52020000} // 52.20.0
(**
 * Returns the LIBAVFORMAT_VERSION_INT constant.
 *)
function avformat_version(): cuint;
  cdecl; external av__format;
{$IFEND}

{$IF LIBAVFORMAT_VERSION >= 52039002} // 52.39.2
(**
 * Returns the libavformat build-time configuration.
 *)
function avformat_configuration(): {const} PansiChar;
  cdecl; external av__format;

(**
 * Returns the libavformat license.
 *)
function avformat_license(): {const} PansiChar;
  cdecl; external av__format;
{$IFEND}

type
  PAVFile = Pointer;

(*
 * Public Metadata API.
 * The metadata API allows libavformat to export metadata tags to a client
 * application using a sequence of key/value pairs.
 * Important concepts to keep in mind:
 * 1. Keys are unique; there can never be 2 tags with the same key. This is
 *    also meant semantically, i.e., a demuxer should not knowingly produce
 *    several keys that are literally different but semantically identical.
 *    E.g., key=Author5, key=Author6. In this example, all authors must be
 *    placed in the same tag.
 * 2. Metadata is flat, not hierarchical; there are no subtags. If you
 *    want to store, e.g., the email address of the child of producer Alice
 *    and actor Bob, that could have key=alice_and_bobs_childs_email_address.
 * 3. A tag whose value is localized for a particular language is appended
 *    with a dash character ('-') and the ISO 639-2/B 3-letter language code.
 *    For example: Author-ger=Michael, Author-eng=Mike
 *    The original/default language is in the unqualified "Author" tag.
 *    A demuxer should set a default if it sets any translated tag.
 *)
const
  AV_METADATA_MATCH_CASE    = 1;
  AV_METADATA_IGNORE_SUFFIX = 2;
{$IF LIBAVFORMAT_VERSION >= 52043000} // >= 52.43.0
  AV_METADATA_DONT_STRDUP_KEY = 4;
  AV_METADATA_DONT_STRDUP_VAL = 8;
{$IFEND}

type
  PAVMetadataTag = ^TAVMetadataTag;
  TAVMetadataTag = record
    key:   PAnsiChar;
    value: PAnsiChar;
  end;

  PAVMetadata = Pointer;

{$IF LIBAVFORMAT_VERSION > 52024001} // > 52.24.1
{$IF LIBAVFORMAT_VERSION_MAJOR = 52}
(**
 * Gets a metadata element with matching key.
 * @param prev Set to the previous matching element to find the next.
 * @param flags Allows case as well as suffix-insensitive comparisons.
 * @return Found tag or NULL, changing key or value leads to undefined behavior.
 *)
function av_metadata_get(m: PAVMetadata; key: {const} PAnsiChar;
                         prev: {const} PAVMetadataTag ; flags: cint): PAVMetadataTag;
  cdecl; external av__format;

(**
 * Sets the given tag in m, overwriting an existing tag.
 * @param key tag key to add to m (will be av_strduped)
 * @param value tag value to add to m (will be av_strduped)
 * @return >= 0 on success otherwise an error code <0
 *)
function av_metadata_set(var pm: PAVMetadata; key: {const} PAnsiChar; value: {const} PAnsiChar): cint;
  cdecl; external av__format;
{$IFEND}

{$IF LIBAVFORMAT_VERSION >= 52043000} // >= 52.43.0
(**
 * Sets the given tag in m, overwriting an existing tag.
 * @param key tag key to add to m (will be av_strduped depending on flags)
 * @param value tag value to add to m (will be av_strduped depending on flags)
 * @return >= 0 on success otherwise an error code <0
 *)
function av_metadata_set2(var pm: PAVMetadata; key: {const} PAnsiChar; value: {const} PAnsiChar; flags: cint): cint;
  cdecl; external av__format;
{$IFEND}

(**
 * Frees all the memory allocated for an AVMetadata struct.
 *)
procedure av_metadata_free(var m: PAVMetadata);
  cdecl; external av__format;
{$IFEND}

(* packet functions *)

{$IF LIBAVFORMAT_VERSION < 52032000} // < 52.32.0
type
  PAVPacket = ^TAVPacket;
  TAVPacket = record
    (**
     * Presentation timestamp in time_base units; the time at which the
     * decompressed packet will be presented to the user.
     * Can be AV_NOPTS_VALUE if it is not stored in the file.
     * pts MUST be larger or equal to dts as presentation can not happen before
     * decompression, unless one wants to view hex dumps. Some formats misuse
     * the terms dts and pts/cts to mean something different. Such timestamps
     * must be converted to true pts/dts before they are stored in AVPacket.
     *)
    pts: cint64;
    (**
     * Decompression timestamp in time_base units; the time at which the
     * packet is decompressed.
     * Can be AV_NOPTS_VALUE if it is not stored in the file.
     *)
    dts: cint64;
    data: PByteArray;
    size: cint;
    stream_index: cint;
    flags: cint;
    (**
     * Duration of this packet in time_base units, 0 if unknown.
     * Equals next_pts - this_pts in presentation order.
     *)
    duration: cint;
    destruct: procedure (p: PAVPacket); cdecl;
    priv: pointer;
    pos: cint64;                            ///< byte position in stream, -1 if unknown

    {$IF LIBAVFORMAT_VERSION >= 52022000} // 52.22.0
    (**
     * Time difference in stream time base units from the pts of this
     * packet to the point at which the output from the decoder has converged
     * independent from the availability of previous frames. That is, the
     * frames are virtually identical no matter if decoding started from
     * the very first frame or from this keyframe.
     * Is AV_NOPTS_VALUE if unknown.
     * This field is not the display duration of the current packet.
     *
     * The purpose of this field is to allow seeking in streams that have no
     * keyframes in the conventional sense. It corresponds to the
     * recovery point SEI in H.264 and match_time_delta in NUT. It is also
     * essential for some types of subtitle streams to ensure that all
     * subtitles are correctly displayed after seeking.
     *)
    convergence_duration: cint64;
    {$IFEND}
  end;
  
const
  PKT_FLAG_KEY = $0001;

procedure av_destruct_packet_nofree(var pkt: TAVPacket);
  cdecl; external av__format;

(**
 * Default packet destructor.
 *)
procedure av_destruct_packet(var pkt: TAVPacket);
  cdecl; external av__format;

(**
 * Initialize optional fields of a packet with default values.
 *
 * @param pkt packet
 *)
procedure av_init_packet(var pkt: TAVPacket);
{$IF LIBAVFORMAT_VERSION >= 51012002} // 51.12.2
  cdecl; external av__format;
{$IFEND}

(**
 * Allocate the payload of a packet and initialize its fields with
 * default values.
 *
 * @param pkt packet
 * @param size wanted payload size
 * @return 0 if OK, AVERROR_xxx otherwise
 *)
function av_new_packet(var pkt: TAVPacket; size: cint): cint;
  cdecl; external av__format;
{$IFEND}

(**
 * Allocate and read the payload of a packet and initialize its fields with
 * default values.
 *
 * @param pkt packet
 * @param size desired payload size
 * @return >0 (read size) if OK, AVERROR_xxx otherwise
 *)
function av_get_packet(s: PByteIOContext; var pkt: TAVPacket; size: cint): cint;
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION < 52032000} // < 52.32.0
(**
 * @warning This is a hack - the packet memory allocation stuff is broken. The
 * packet is allocated if it was not really allocated.
 *)
function av_dup_packet(pkt: PAVPacket): cint;
  cdecl; external av__format;

(**
 * Free a packet.
 *
 * @param pkt packet to free
 *)
procedure av_free_packet(pkt: PAVPacket); {$IFDEF HasInline}inline;{$ENDIF}
{$IFEND}

(*************************************************)
(* fractional numbers for exact pts handling *)

type
 (**
  * The exact value of the fractional number is: 'val + num / den'.
  * num is assumed to be 0 <= num < den.
  *)
  PAVFrac = ^TAVFrac;
  TAVFrac = record
    val, num, den: cint64;
  end;

(*************************************************)
(* input/output formats *)

type
  (** This structure contains the data a format has to probe a file. *)
  TAVProbeData = record
    filename: PAnsiChar;
    buf: PByteArray;  (**< Buffer must have AVPROBE_PADDING_SIZE of extra allocated bytes filled with zero. *)
    buf_size: cint;   (**< Size of buf except extra allocated bytes *)
  end;

const
  AVPROBE_SCORE_MAX   = 100;   ///< Maximum score, half of that is used for file-extension-based detection
  AVPROBE_PADDING_SIZE = 32;   ///< extra allocated bytes at the end of the probe buffer

  //! Demuxer will use url_fopen, no opened file should be provided by the caller.
  AVFMT_NOFILE        = $0001;
  AVFMT_NEEDNUMBER    = $0002; (**< Needs '%d' in filename. *)
  AVFMT_SHOW_IDS      = $0008; (**< Show format stream IDs numbers. *)
  AVFMT_RAWPICTURE    = $0020; (**< Format wants AVPicture structure for
                                      raw picture data. *)
  AVFMT_GLOBALHEADER  = $0040; (**< Format wants global header. *)
  AVFMT_NOTIMESTAMPS  = $0080; (**< Format does not need / have any timestamps. *)
  AVFMT_GENERIC_INDEX = $0100; (**< Use generic index building code. *)
  AVFMT_TS_DISCONT    = $0200; (**< Format allows timestamp discontinuities. *)
  {$IF LIBAVFORMAT_VERSION >= 52029002} // 52.29.2
  AVFMT_VARIABLE_FPS  = $0400; (**< Format allows variable fps. *)
  {$IFEND}

  // used by AVIndexEntry
  AVINDEX_KEYFRAME = $0001;

  AVFMTCTX_NOHEADER = $0001; (**< signal that no header is present
                                         (streams are added dynamically) *)
  MAX_STREAMS = 20;

  AVFMT_NOOUTPUTLOOP = -1;
  AVFMT_INFINITEOUTPUTLOOP = 0;
  AVFMT_FLAG_GENPTS = $0001;   ///< Generate missing pts even if it requires parsing future frames.
  AVFMT_FLAG_IGNIDX = $0002;   ///< Ignore index.
  AVFMT_FLAG_NONBLOCK = $0004; ///< Do not block when reading packets from input.

  // used by AVStream
  MAX_REORDER_DELAY = 16;

  // used by TAVProgram
  AV_PROGRAM_RUNNING = 1;


  AV_DISPOSITION_DEFAULT   = $0001;
  AV_DISPOSITION_DUB       = $0002;
  AV_DISPOSITION_ORIGINAL  = $0004;
  AV_DISPOSITION_COMMENT   = $0008;
  AV_DISPOSITION_LYRICS    = $0010;
  AV_DISPOSITION_KARAOKE   = $0020;

  // used by TAVFormatContext.debug
  FF_FDEBUG_TS = 0001;

  {$IF LIBAVFORMAT_VERSION >= 52034000}  // >= 52.34.0
    {$IF LIBAVFORMAT_VERSION < 52039000} // <  52.39.0
  MAX_PROBE_PACKETS = 100;
    {$ELSE}
  MAX_PROBE_PACKETS = 2500;
    {$IFEND}
  {$IFEND}

  {$IF LIBAVFORMAT_VERSION >= 52035000}  // >= 52.35.0
    {$IF LIBAVFORMAT_VERSION < 52039000} // <  52.39.0
  RAW_PACKET_BUFFER_SIZE = 32000;
    {$ELSE}
  RAW_PACKET_BUFFER_SIZE = 2500000;
    {$IFEND}
  {$IFEND}

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

{$IF LIBAVFORMAT_VERSION >= 52030001} // >= 52.30.1
(**
 * Convert all the metadata sets from ctx according to the source and
 * destination conversion tables.
 * @param d_conv destination tags format conversion table
 * @param s_conv source tags format conversion table
 *)
  PAVMetadataConv = ^TAVMetadataConv;
  TAVMetadataConv = record
    ctx:            PAVFormatContext;
    d_conv: {const} PAVMetadataConv;
    s_conv: {const} PAVMetadataConv;
  end;
{$IFEND}

  PAVChapter = ^TAVChapter;
  TAVChapter = record
    id: cint;                 ///< unique ID to identify the chapter
    time_base: TAVRational;   ///< time base in which the start/end timestamps are specified
    start, end_: cint64;      ///< chapter start/end time in time_base units
    {$IF LIBAVFORMAT_VERSION < 53000000}  // 53.00.0
    title: PAnsiChar;         ///< chapter title
    {$IFEND}
    {$IF LIBAVFORMAT_VERSION >= 52024001} // 52.24.1
    metadata: PAVMetadata;
    {$IFEND}
  end;
  TAVChapterArray = array[0..(MaxInt div SizeOf(TAVChapter))-1] of TAVChapter;
  PAVChapterArray = ^TAVChapterArray;

  TAVFormatParameters = record
    time_base: TAVRational;
    sample_rate: cint;
    channels: cint;
    width: cint;
    height: cint;
    pix_fmt: TAVPixelFormat;
    {$IF LIBAVFORMAT_VERSION < 51006000} // 51.6.0
    image_format: PAVImageFormat;
    {$IFEND}
    channel: cint; (**< Used to select DV channel. *)
    {$IF LIBAVFORMAT_VERSION_MAJOR < 52}
    device: PAnsiChar; (* video, audio or DV device, if LIBAVFORMAT_VERSION_INT < (52<<16) *)
    {$IFEND}
    standard: PAnsiChar; (**< TV standard, NTSC, PAL, SECAM *)
    { Delphi does not support bit fields -> use bf_flags instead
    unsigned int mpeg2ts_raw:1;  (**< Force raw MPEG-2 transport stream output, if possible. *)
    unsigned int mpeg2ts_compute_pcr:1; (**< Compute exact PCR for each transport
                                  stream packet (only meaningful if
                                  mpeg2ts_raw is TRUE). *)
    unsigned int initial_pause:1;       (**< Do not begin to play the stream
                                        immediately (RTSP only). *)
    unsigned int prealloced_context:1;
    }
    bf_flags: byte; // 0:mpeg2ts_raw/1:mpeg2ts_compute_pcr/2:initial_pause/3:prealloced_context
    {$IF LIBAVFORMAT_VERSION_MAJOR < 53}
    video_codec_id: TCodecID;
    audio_codec_id: TCodecID;
    {$IFEND}
  end;

  TAVOutputFormat = record
    name: PAnsiChar;
    (**
     * Descriptive name for the format, meant to be more human-readable
     * than name. You should use the NULL_IF_CONFIG_SMALL() macro
     * to define it.
     *)
    long_name: PAnsiChar;
    mime_type: PAnsiChar;
    extensions: PAnsiChar; (**< comma-separated filename extensions *)
    (** size of private data so that it can be allocated in the wrapper *)
    priv_data_size: cint;
    (* output support *)
    audio_codec: TCodecID; (**< default audio codec *)
    video_codec: TCodecID; (**< default video codec *)
    write_header: function (c: PAVFormatContext): cint; cdecl;
    write_packet: function (c: PAVFormatContext; pkt: PAVPacket): cint; cdecl;
    write_trailer: function (c: PAVFormatContext): cint; cdecl;
    (** can use flags: AVFMT_NOFILE, AVFMT_NEEDNUMBER, AVFMT_GLOBALHEADER *)
    flags: cint;
    (** Currently only used to set pixel format if not YUV420P. *)
    set_parameters: function (c: PAVFormatContext; f: PAVFormatParameters): cint; cdecl;
    interleave_packet: function (s: PAVFormatContext; out_: PAVPacket;
                                 in_: PAVPacket; flush: cint): cint; cdecl;

    {$IF LIBAVFORMAT_VERSION >= 51008000} // 51.8.0
    (**
     * List of supported codec_id-codec_tag pairs, ordered by "better
     * choice first". The arrays are all terminated by CODEC_ID_NONE.
     *)
    codec_tag: {const} PPAVCodecTag;
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 51012002} // 51.12.2
    subtitle_codec: TCodecID; (**< default subtitle codec *)
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 52030001} // 52.30.1
    {const} metadata_conv: PAVMetadataConv;
    {$IFEND}

    (* private fields *)
    next: PAVOutputFormat;
  end;

  TAVInputFormat = record
    name: PAnsiChar;
    (**
     * Descriptive name for the format, meant to be more human-readable
     * than name. You should use the NULL_IF_CONFIG_SMALL() macro
     * to define it.
     *)
    long_name: PAnsiChar;
    (** Size of private data so that it can be allocated in the wrapper. *)
    priv_data_size: cint;
    (**
     * Tell if a given file has a chance of being parsed as this format.
     * The buffer provided is guaranteed to be AVPROBE_PADDING_SIZE bytes
     * big so you do not have to check for that unless you need more.
     *)
    read_probe: function (p: PAVProbeData): cint; cdecl;
    (** Read the format header and initialize the AVFormatContext
       structure. Return 0 if OK. 'ap' if non-NULL contains
       additional parameters. Only used in raw format right
       now. 'av_new_stream' should be called to create new streams.  *)
    read_header: function (c: PAVFormatContext; ap: PAVFormatParameters): cint; cdecl;
    (** Read one packet and put it in 'pkt'. pts and flags are also
       set. 'av_new_stream' can be called only if the flag
       AVFMTCTX_NOHEADER is used.
       @return 0 on success, < 0 on error.
               When returning an error, pkt must not have been allocated
               or must be freed before returning *)
    read_packet: function (c: PAVFormatContext; var pkt: TAVPacket): cint; cdecl;
    (** Close the stream. The AVFormatContext and AVStreams are not
       freed by this function *)
    read_close: function (c: PAVFormatContext): cint; cdecl;

{$IF LIBAVFORMAT_VERSION_MAJOR < 53}
    (**
     * Seek to a given timestamp relative to the frames in
     * stream component stream_index.
     * @param stream_index Must not be -1.
     * @param flags Selects which direction should be preferred if no exact
     *              match is available.
     * @return >= 0 on success (but not necessarily the new offset)
     *)
    read_seek: function (c: PAVFormatContext; stream_index: cint;
                  timestamp: cint64; flags: cint): cint; cdecl;
{$IFEND}

    (**
     * Gets the next timestamp in stream[stream_index].time_base units.
     * @return the timestamp or AV_NOPTS_VALUE if an error occurred
     *)
    read_timestamp: function (s: PAVFormatContext; stream_index: cint;
                              pos: pint64; pos_limit: cint64): cint64; cdecl;
    (** Can use flags: AVFMT_NOFILE, AVFMT_NEEDNUMBER. *)
    flags: cint;
    (** If extensions are defined, then no probe is done. You should
       usually not use extension format guessing because it is not
       reliable enough *)
    extensions: PAnsiChar;
    (** General purpose read-only value that the format can use. *)
    value: cint;

    (** Starts/resumes playing - only meaningful if using a network-based format
       (RTSP). *)
    read_play: function (c: PAVFormatContext): cint; cdecl;

    (** Pauses playing - only meaningful if using a network-based format
      (RTSP). *)  
    read_pause: function (c: PAVFormatContext): cint; cdecl;

    {$IF LIBAVFORMAT_VERSION >= 51008000} // 51.8.0
    codec_tag: {const} PPAVCodecTag;
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 52030000} // 52.30.0
    (**
     * Seeks to timestamp ts.
     * Seeking will be done so that the point from which all active streams
     * can be presented successfully will be closest to ts and within min/max_ts.
     * Active streams are all streams that have AVStream.discard < AVDISCARD_ALL.
     *)
    read_seek2: function (s:            PAVFormatContext;
                          stream_index: cint;
			  min_ts:       cint64;
			  ts:           cint64; 
			  max_ts:       cint64;
			  flags:        cint): cint; cdecl;
    {$IFEND}
    
    {$IF LIBAVFORMAT_VERSION >= 52030001} // 52.30.1
    {const} metadata_conv: PAVMetadataConv;
    {$IFEND}

    (* private fields *)
    next: PAVInputFormat;
  end;

  TAVStreamParseType = (
    AVSTREAM_PARSE_NONE,
    AVSTREAM_PARSE_FULL,       (**< full parsing and repack *)
    AVSTREAM_PARSE_HEADERS,    (**< Only parse headers, do not repack. *)
    AVSTREAM_PARSE_TIMESTAMPS  (**< full parsing and interpolation of timestamps for frames not starting on a packet boundary *)
  );

  TAVIndexEntry = record
    pos: cint64;
    timestamp: cint64;
    { Delphi doesn't support bitfields -> use flags_size instead
    int flags:2;
    int size:30; //Yeah, trying to keep the size of this small to reduce memory requirements (it is 24 vs. 32 bytes due to possible 8-byte alignment).
    }
    flags_size: cint; // 0..1: flags, 2..31: size
    min_distance: cint;         (**< Minimum distance between this and the previous keyframe, used to avoid unneeded searching. *)
  end;

 (**
  * Stream structure.
  * New fields can be added to the end with minor version bumps.
  * Removal, reordering and changes to existing fields require a major
  * version bump.
  * sizeof(AVStream) must not be used outside libav*.
  *)
  TAVStream = record
    index: cint;    (**< stream index in AVFormatContext *)
    id: cint;       (**< format-specific stream ID *)
    codec: PAVCodecContext; (**< codec context *)
    (**
     * Real base framerate of the stream.
     * This is the lowest framerate with which all timestamps can be
     * represented accurately (it is the least common multiple of all
     * framerates in the stream). Note, this value is just a guess!
     * For example, if the time base is 1/90000 and all frames have either
     * approximately 3600 or 1800 timer ticks, then r_frame_rate will be 50/1.
     *)
    r_frame_rate: TAVRational;
    priv_data: pointer;

    (* internal data used in av_find_stream_info() *)
    first_dts: cint64;
    {$IF LIBAVFORMAT_VERSION_MAJOR < 52}
    codec_info_nb_frames: cint;
    {$IFEND}

    (** encoding: pts generation when outputting stream *)
    pts: TAVFrac;
    (**
     * This is the fundamental unit of time (in seconds) in terms
     * of which frame timestamps are represented. For fixed-fps content,
     * time base should be 1/framerate and timestamp increments should be 1.
     *)
    time_base: TAVRational;
    pts_wrap_bits: cint; (* number of bits in pts (used for wrapping control) *)
    (* ffmpeg.c private use *)
    stream_copy: cint; (**< If set, just copy stream. *)
    discard: TAVDiscard; ///< Selects which packets can be discarded at will and do not need to be demuxed.
    //FIXME move stuff to a flags field?
    (** Quality, as it has been removed from AVCodecContext and put in AVVideoFrame.
     * MN:dunno if thats the right place, for it *)
    quality: cfloat;
    (**
     * Decoding: pts of the first frame of the stream, in stream time base.
     * Only set this if you are absolutely 100% sure that the value you set
     * it to really is the pts of the first frame.
     * This may be undefined (AV_NOPTS_VALUE).
     * @note The ASF header does NOT contain a correct start_time the ASF
     * demuxer must NOT set this.
     *)
    start_time: cint64;
    (**
     * Decoding: duration of the stream, in stream time base.
     * If a source file does not specify a duration, but does specify
     * a bitrate, this value will be estimated from bitrate and file size.
     *)
    duration: cint64;

    {$IF LIBAVFORMAT_VERSION_MAJOR < 53}
    language: array [0..3] of PAnsiChar; (* ISO 639-2/B 3-letter language code (empty string if undefined) *)
    {$IFEND}

    (* av_read_frame() support *)
    need_parsing: TAVStreamParseType;
    parser: PAVCodecParserContext;

    cur_dts: cint64;
    last_IP_duration: cint;
    last_IP_pts: cint64;
    (* av_seek_frame() support *)
    index_entries: PAVIndexEntry; (**< Only used if the format does not
                                       support seeking natively. *)
    nb_index_entries: cint;
    index_entries_allocated_size: cuint;

    nb_frames: cint64;                 ///< number of frames in this stream if known or 0
    
    {$IF (LIBAVFORMAT_VERSION >= 50006000) and (LIBAVFORMAT_VERSION_MAJOR < 53)} // 50.6.0 - 53.0.0
    unused: array [0..4] of cint64;
    {$IFEND}

    {$IF (LIBAVFORMAT_VERSION >= 52006000) and (LIBAVFORMAT_VERSION_MAJOR < 53)} // 52.6.0 - 53.0.0
    filename: PAnsiChar; (**< source filename of the stream *)
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 52008000} // 52.8.0
    disposition: cint; (**< AV_DISPOSITION_* bitfield *)
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 52019000} // 52.19.0
    probe_data: TAVProbeData;
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 52021000} // 52.21.0
    pts_buffer: array [0..MAX_REORDER_DELAY] of cint64;

    (**
     * sample aspect ratio (0 if unknown)
     * - encoding: Set by user.
     * - decoding: Set by libavformat.
     *)
    sample_aspect_ratio: TAVRational;
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 52024001} // 52.24.1
    metadata: PAVMetadata;
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION > 52024001} // > 52.24.1
    {* av_read_frame() support *}
    cur_ptr: {const} PCuint8;
    cur_len: cint;
    cur_pkt: TAVPacket;
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 52030000} // > 52.30.0
    // Timestamp generation support:
    (**
     * Timestamp corresponding to the last dts sync point.
     *
     * Initialized when AVCodecParserContext.dts_sync_point >= 0 and
     * a DTS is received from the underlying container. Otherwise set to
     * AV_NOPTS_VALUE by default.
     *)
    reference_dts: cint64;
    {$IFEND}
    {$IF LIBAVFORMAT_VERSION >= 52034000} // >= 52.34.0
    (**
     * Number of packets to buffer for codec probing
     * NOT PART OF PUBLIC API
     *)
    probe_packets: cint;
    {$IFEND}
    {$IF LIBAVFORMAT_VERSION >= 52038000} // >= 52.38.0
    (**
     * last packet in packet_buffer for this stream when muxing.
     * used internally, NOT PART OF PUBLIC API, dont read or write from outside of libav*
     *)
    last_in_packet_buffer: PAVPacketList;
    {$IFEND}
    {$IF LIBAVFORMAT_VERSION >= 52041000} // >= 52.41.0
    (**
     * Average framerate
     *)
    avg_frame_rate: TAVRational;
    {$IFEND}
  end;

 (**
  * Format I/O context.
  * New fields can be added to the end with minor version bumps.
  * Removal, reordering and changes to existing fields require a major
  * version bump.
  * sizeof(AVFormatContext) must not be used outside libav*.
  *)
  TAVFormatContext = record
    av_class: PAVClass; (**< Set by avformat_alloc_context. *)
    (* Can only be iformat or oformat, not both at the same time. *)
    iformat: PAVInputFormat;
    oformat: PAVOutputFormat;
    priv_data: pointer;

    {$IF LIBAVFORMAT_VERSION_MAJOR >= 52}
    pb: PByteIOContext;
    {$ELSE}
    pb: TByteIOContext;
    {$IFEND}

    nb_streams: cuint;
    streams: array [0..MAX_STREAMS - 1] of PAVStream;
    filename: array [0..1023] of AnsiChar; (* input or output filename *)
    (* stream info *)
    timestamp: cint64;
    {$IF LIBAVFORMAT_VERSION < 53000000} // 53.00.0
    title: array [0..511] of AnsiChar;
    author: array [0..511] of AnsiChar;
    copyright: array [0..511] of AnsiChar;
    comment: array [0..511] of AnsiChar;
    album: array [0..511] of AnsiChar;
    year: cint;  (**< ID3 year, 0 if none *)
    track: cint; (**< track number, 0 if none *)
    genre: array [0..31] of AnsiChar; (**< ID3 genre *)
    {$IFEND}

    ctx_flags: cint; (**< Format-specific flags, see AVFMTCTX_xx *)
    (* private data for pts handling (do not modify directly). *)
    (** This buffer is only needed when packets were already buffered but
       not decoded, for example to get the codec parameters in MPEG
       streams. *)
    packet_buffer: PAVPacketList;

    (** Decoding: position of the first frame of the component, in
       AV_TIME_BASE fractional seconds. NEVER set this value directly:
       It is deduced from the AVStream values. *)
    start_time: cint64;
    (** Decoding: duration of the stream, in AV_TIME_BASE fractional
       seconds. NEVER set this value directly: it is deduced from the
       AVStream values.  *)
    duration: cint64;
    (** decoding: total file size, 0 if unknown *)
    file_size: cint64;
    (** Decoding: total stream bitrate in bit/s, 0 if not
       available. Never set it directly if the file_size and the
       duration are known as ffmpeg can compute it automatically. *)
    bit_rate: cint;

    (* av_read_frame() support *)
    cur_st: PAVStream;
    {$IF LIBAVFORMAT_VERSION_MAJOR < 53}
    cur_ptr_deprecated: pbyte;
    cur_len_deprecated: cint;
    cur_pkt_deprecated: TAVPacket;
    {$IFEND}

    (* av_seek_frame() support *)
    data_offset: cint64; (* offset of the first packet *)
    index_built: cint;

    mux_rate: cint;
    {$IF LIBAVFORMAT_VERSION < 52034001} // < 52.34.1
    packet_size: cint;
    {$ELSE}
    packet_size: cuint;
    {$IFEND}
    preload: cint;
    max_delay: cint;

    (* number of times to loop output in formats that support it *)
    loop_output: cint;

    flags: cint;
    loop_input: cint;

    {$IF LIBAVFORMAT_VERSION >= 50006000} // 50.6.0
    (** decoding: size of data to probe; encoding: unused. *)
    probesize: cuint;
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 51009000} // 51.9.0
    (**
     * Maximum time (in AV_TIME_BASE units) during which the input should
     * be analyzed in av_find_stream_info().
     *)
    max_analyze_duration: cint;

    key: pbyte;
    keylen : cint;
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 51014000} // 51.14.0
    nb_programs: cuint;
    programs: PPAVProgram;
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 52003000} // 52.3.0
    (**
     * Forced video codec_id.
     * Demuxing: Set by user.
     *)
    video_codec_id: TCodecID;
    (**
     * Forced audio codec_id.
     * Demuxing: Set by user.
     *)
    audio_codec_id: TCodecID;
    (**
     * Forced subtitle codec_id.
     * Demuxing: Set by user.
     *)
    subtitle_codec_id: TCodecID;
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 52004000} // 52.4.0
    (**
     * Maximum amount of memory in bytes to use for the index of each stream.
     * If the index exceeds this size, entries will be discarded as
     * needed to maintain a smaller size. This can lead to slower or less
     * accurate seeking (depends on demuxer).
     * Demuxers for which a full in-memory index is mandatory will ignore
     * this.
     * muxing  : unused
     * demuxing: set by user
     *)
    max_index_size: cuint;
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 52009000} // 52.9.0
    (**
     * Maximum amount of memory in bytes to use for buffering frames
     * obtained from realtime capture devices.
     *)
    max_picture_buffer: cuint;
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 52014000} // 52.14.0
    nb_chapters: cuint;
    chapters: PAVChapterArray;
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 52016000} // 52.16.0
    (**
     * Flags to enable debugging.
     *)
    debug: cint;
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 52019000} // 52.19.0
    (**
     * Raw packets from the demuxer, prior to parsing and decoding.
     * This buffer is used for buffering packets until the codec can
     * be identified, as parsing cannot be done without knowing the
     * codec.
     *)
    raw_packet_buffer: PAVPacketList;
    raw_packet_buffer_end: PAVPacketList;

    packet_buffer_end: PAVPacketList;
    {$IFEND}

    {$IF LIBAVFORMAT_VERSION >= 52024001} // 52.24.1
    metadata: PAVMetadata;
    {$IFEND}
    
    {$IF LIBAVFORMAT_VERSION >= 52035000} // 52.35.0
    (**
     * Remaining size available for raw_packet_buffer, in bytes.
     * NOT PART OF PUBLIC API
     *)
    raw_packet_buffer_remaining_size: cint;
    {$IFEND}

  end;

  (**
   * New fields can be added to the end with minor version bumps.
   * Removal, reordering and changes to existing fields require a major
   * version bump.
   * sizeof(AVProgram) must not be used outside libav*.
   *)
  TAVProgram = record
      id                : cint;
      {$IF LIBAVFORMAT_VERSION < 53000000} // 53.00.0
      provider_name     : PAnsiChar;  ///< network name for DVB streams
      name              : PAnsiChar;  ///< service name for DVB streams
      {$IFEND}
      flags             : cint;
      discard           : TAVDiscard; ///< selects which program to discard and which to feed to the caller
      {$IF LIBAVFORMAT_VERSION >= 51016000} // 51.16.0
      stream_index      : PCardinal;
      nb_stream_indexes : PCardinal;
      {$IFEND}
      {$IF LIBAVFORMAT_VERSION >= 52024001} // 52.24.1
      metadata: PAVMetadata;
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
    width: cint; (* requested width *)
    height: cint; (* requested height *)
    interleaved: cint; (* image is interleaved (e.g. interleaved GIF) *)
    pict: TAVPicture; (* returned allocated image *)
  end; {deprecated}

  TAVImageFormat = record
    name: PAnsiChar;
    extensions: PAnsiChar;
    (* tell if a given file has a chance of being parsing by this format *)
    img_probe: function (d: PAVProbeData): cint; cdecl;
    (* read a whole image. 'alloc_cb' is called when the image size is
       known so that the caller can allocate the image. If 'allo_cb'
       returns non zero, then the parsing is aborted. Return '0' if
       OK. *)
    img_read: function (b: PByteIOContext; alloc_cb: pointer; ptr: pointer): cint; cdecl;
    (* write the image *)
    supported_pixel_formats: cint; (* mask of supported formats for output *)
    img_write: function (b: PByteIOContext; i: PAVImageInfo): cint; cdecl;
    flags: cint;
    next: PAVImageFormat;
  end; {deprecated}

procedure av_register_image_format(img_fmt: PAVImageFormat);
    cdecl; external av__format; deprecated;

function av_probe_image_format(pd: PAVProbeData): PAVImageFormat;
    cdecl; external av__format; deprecated;

function guess_image_format(filename: PAnsiChar): PAVImageFormat;
    cdecl; external av__format; deprecated;

function av_read_image(pb: PByteIOContext; filename: PAnsiChar;
                  fmt: PAVImageFormat;
                  alloc_cb: pointer; opaque: pointer): cint;
    cdecl; external av__format; deprecated;

function av_write_image(pb: PByteIOContext; fmt: PAVImageFormat; img: PAVImageInfo): cint;
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
(**
 * If f is NULL, returns the first registered input format,
 * if f is non-NULL, returns the next registered input format after f
 * or NULL if f is the last one.
 *)
function av_iformat_next(f: PAVInputFormat): PAVInputFormat;
    cdecl; external av__format;
(**
 * If f is NULL, returns the first registered output format,
 * if f is non-NULL, returns the next registered input format after f
 * or NULL if f is the last one.
 *)
function av_oformat_next(f: PAVOutputFormat): PAVOutputFormat;
    cdecl; external av__format;
{$IFEND}

function av_guess_image2_codec(filename: {const} PAnsiChar): TCodecID;
    cdecl; external av__format;

(* XXX: Use automatic init with either ELF sections or C file parser *)
(* modules. *)

(* utils.c *)
procedure av_register_input_format(format: PAVInputFormat);
  cdecl; external av__format;

procedure av_register_output_format(format: PAVOutputFormat);
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION_MAJOR < 53} // < 53
function guess_stream_format(short_name: PAnsiChar;
                             filename: PAnsiChar;
                             mime_type: PAnsiChar): PAVOutputFormat;
  cdecl; external av__format; deprecated;
{$IFEND}

(**
 * Returns the output format in the list of registered output formats
 * which best matches the provided parameters, or returns NULL if
 * there is no match.
 *
 * @param short_name if non-NULL checks if short_name matches with the
 * names of the registered formats
 * @param filename if non-NULL checks if filename terminates with the
 * extensions of the registered formats
 * @param mime_type if non-NULL checks if mime_type matches with the
 * MIME type of the registered formats
 *)
(**
 * @deprecated Use av_guess_format() instead.
 *)
function guess_format(short_name: PAnsiChar;
                      filename: PAnsiChar;
                      mime_type: PAnsiChar): PAVOutputFormat;
  cdecl; external av__format;
{$IF LIBAVFORMAT_VERSION >= 52045000} // >= 52.45.0
                              deprecated;
function av_guess_format(short_name: PAnsiChar;
                         filename: PAnsiChar;
                         mime_type: PAnsiChar): PAVOutputFormat;
  cdecl; external av__format;
{$IFEND}

(**
 * Guesses the codec ID based upon muxer and filename.
 *)
function av_guess_codec(fmt: PAVOutputFormat; short_name: PAnsiChar;
                        filename: PAnsiChar; mime_type: PAnsiChar;
                        type_: TCodecType): TCodecID;
  cdecl; external av__format;

(**
 * Sends a nice hexadecimal dump of a buffer to the specified file stream.
 *
 * @param f The file stream pointer where the dump should be sent to.
 * @param buf buffer
 * @param size buffer size
 *
 * @see av_hex_dump_log, av_pkt_dump, av_pkt_dump_log
 *)
procedure av_hex_dump(f: PAVFile; buf: PByteArray; size: cint);
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 51011000} // 51.11.0
(**
 * Sends a nice hexadecimal dump of a buffer to the log.
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
procedure av_hex_dump_log(avcl: Pointer; level: cint; buf: PByteArray; size: cint);
  cdecl; external av__format;
{$IFEND}

(**
 * Sends a nice dump of a packet to the specified file stream.
 *
 * @param f The file stream pointer where the dump should be sent to.
 * @param pkt packet to dump
 * @param dump_payload True if the payload must be displayed, too.
 *)
procedure av_pkt_dump(f: PAVFile; pkt: PAVPacket; dump_payload: cint);
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 51011000} // 51.11.0
(**
 * Sends a nice dump of a packet to the log.
 *
 * @param avcl A pointer to an arbitrary struct of which the first field is a
 * pointer to an AVClass struct.
 * @param level The importance level of the message, lower values signifying
 * higher importance.
 * @param pkt packet to dump
 * @param dump_payload True if the payload must be displayed, too.
 *)
procedure av_pkt_dump_log(avcl: Pointer; level: cint; pkt: PAVPacket; dump_payload: cint);
  cdecl; external av__format;
{$IFEND}

(**
 * Initializes libavformat and registers all the muxers, demuxers and
 * protocols. If you do not call this function, then you can select
 * exactly which formats you want to support.
 *
 * @see av_register_input_format()
 * @see av_register_output_format()
 * @see av_register_protocol()
 *)
procedure av_register_all();
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 51008000} // 51.8.0
(** codec tag <-> codec id *)
function av_codec_get_id(var tags: PAVCodecTag; tag: cuint): TCodecID;
  cdecl; external av__format;
function av_codec_get_tag(var tags: PAVCodecTag; id: TCodecID): cuint;
  cdecl; external av__format;
{$IFEND}

(* media file input *)

(**
 * Finds AVInputFormat based on the short name of the input format.
 *)
function av_find_input_format(short_name: PAnsiChar): PAVInputFormat;
  cdecl; external av__format;

(**
 * Guesses file format.
 *
 * @param is_opened Whether the file is already opened; determines whether
 *                  demuxers with or without AVFMT_NOFILE are probed.
 *)
function av_probe_input_format(pd: PAVProbeData; is_opened: cint): PAVInputFormat;
  cdecl; external av__format;

(**
 * Allocates all the structures needed to read an input stream.
 *        This does not open the needed codecs for decoding the stream[s].
 *)
function av_open_input_stream(var ic_ptr: PAVFormatContext;
                       pb: PByteIOContext; filename: PAnsiChar;
                       fmt: PAVInputFormat; ap: PAVFormatParameters): cint;
  cdecl; external av__format;

(**
 * Opens a media file as input. The codecs are not opened. Only the file
 * header (if present) is read.
 *
 * @param ic_ptr The opened media file handle is put here.
 * @param filename filename to open
 * @param fmt If non-NULL, force the file format to use.
 * @param buf_size optional buffer size (zero if default is OK)
 * @param ap Additional parameters needed when opening the file
 *           (NULL if default).
 * @return 0 if OK, AVERROR_xxx otherwise
 *)
function av_open_input_file(var ic_ptr: PAVFormatContext; filename: PAnsiChar;
                     fmt: PAVInputFormat; buf_size: cint;
                     ap: PAVFormatParameters): cint;
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 52026000} // 52.26.0
(**
 * Allocates an AVFormatContext.
 * Can be freed with av_free() but do not forget to free everything you
 * explicitly allocated as well!
 *)
function avformat_alloc_context(): PAVFormatContext;
  cdecl; external av__format;
{$ELSE}
  {$IF LIBAVFORMAT_VERSION_MAJOR < 53}
(**
 * @deprecated Use avformat_alloc_context() instead.
 *)
function av_alloc_format_context(): PAVFormatContext;
  cdecl; external av__format;
  {$IFEND}
{$IFEND}

(**
 * Reads packets of a media file to get stream information. This
 * is useful for file formats with no headers such as MPEG. This
 * function also computes the real framerate in case of MPEG-2 repeat
 * frame mode.
 * The logical file position is not changed by this function;
 * examined packets may be buffered for later processing.
 *
 * @param ic media file handle
 * @return >=0 if OK, AVERROR_xxx on error
 * @todo Let the user decide somehow what information is needed so that
 *       we do not waste time getting stuff the user does not need.
 *)
function av_find_stream_info(ic: PAVFormatContext): cint;
  cdecl; external av__format;

(**
 * Reads a transport packet from a media file.
 *
 * This function is obsolete and should never be used.
 * Use av_read_frame() instead.
 *
 * @param s media file handle
 * @param pkt is filled
 * @return 0 if OK, AVERROR_xxx on error
 *)
function av_read_packet(s: PAVFormatContext; var pkt: TAVPacket): cint;
  cdecl; external av__format;

(**
 * Returns the next frame of a stream.
 *
 * The returned packet is valid
 * until the next av_read_frame() or until av_close_input_file() and
 * must be freed with av_free_packet. For video, the packet contains
 * exactly one frame. For audio, it contains an cint number of
 * frames if each frame has a known fixed size (e.g. PCM or ADPCM
 * data). If the audio frames have a variable size (e.g. MPEG audio),
 * then it contains one frame.
 *
 * pkt->pts, pkt->dts and pkt->duration are always set to correct
 * values in AVStream.time_base units (and guessed if the format cannot
 * provide them). pkt->pts can be AV_NOPTS_VALUE if the video format
 * has B-frames, so it is better to rely on pkt->dts if you do not
 * decompress the payload.
 *
 * @return 0 if OK, < 0 on error or end of file
 *)
function av_read_frame(s: PAVFormatContext; var pkt: TAVPacket): cint;
  cdecl; external av__format;

(**
 * Seeks to the keyframe at timestamp.
 * 'timestamp' in 'stream_index'.
 * @param stream_index If stream_index is (-1), a default
 * stream is selected, and timestamp is automatically converted
 * from AV_TIME_BASE units to the stream specific time_base.
 * @param timestamp Timestamp in AVStream.time_base units
 *        or, if no stream is specified, in AV_TIME_BASE units.
 * @param flags flags which select direction and seeking mode
 * @return >= 0 on success
 *)
function av_seek_frame(s: PAVFormatContext; stream_index: cint; timestamp: cint64;
                       flags: cint): cint;
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 52026000} // 52.26.0
(**
 * Seeks to timestamp ts.
 * Seeking will be done so that the point from which all active streams
 * can be presented successfully will be closest to ts and within min/max_ts.
 * Active streams are all streams that have AVStream.discard < AVDISCARD_ALL.
 *
 * If flags contain AVSEEK_FLAG_BYTE, then all timestamps are in byte and
 * are the file position (this may not be supported by all demuxers).
 * If flags contain AVSEEK_FLAG_FRAME then all timestamps are in frames
 * in the stream with stream_index (this may not be supported by all demuxers).
 * Otherwise all timestamps are in units of the stream selected by stream_index
 * or if stream_index is -1, in AV_TIME_BASE units.
 * If flags contain AVSEEK_FLAG_ANY, then non-keyframes are treated as
 * keyframes (this may not be supported by all demuxers).
 *
 * @param stream_index index of the stream which is used as time base reference.
 * @param min_ts smallest acceptable timestamp
 * @param ts target timestamp
 * @param max_ts largest acceptable timestamp
 * @param flags flags
 * @returns >=0 on success, error code otherwise
 *
 * @NOTE This is part of the new seek API which is still under construction.
 *       Thus do not use this yet. It may change at any time, do not expect
 *       ABI compatibility yet!
 *)
function avformat_seek_file(s:            PAVFormatContext;
			    stream_index: cint;
			    min_ts:       cint64;
			    ts:           cint64;
			    max_ts:       cint64;
			    flags:        cint): cint;
  cdecl; external av__format;
{$IFEND}

(**
 * Starts playing a network-based stream (e.g. RTSP stream) at the
 * current position.
 *)
function av_read_play(s: PAVFormatContext): cint;
  cdecl; external av__format;

(**
 * Pauses a network-based stream (e.g. RTSP stream).
 *
 * Use av_read_play() to resume it.
 *)
function av_read_pause(s: PAVFormatContext): cint;
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 52003000} // 52.3.0
(**
 * Frees a AVFormatContext allocated by av_open_input_stream.
 * @param s context to free
 *)
procedure av_close_input_stream(s: PAVFormatContext);
  cdecl; external av__format;
{$IFEND}

(**
 * Closes a media file (but not its codecs).
 *
 * @param s media file handle
 *)
procedure av_close_input_file(s: PAVFormatContext);
  cdecl; external av__format;

(**
 * Adds a new stream to a media file.
 *
 * Can only be called in the read_header() function. If the flag
 * AVFMTCTX_NOHEADER is in the format context, then new streams
 * can be added in read_packet too.
 *
 * @param s media file handle
 * @param id file-format-dependent stream ID
 *)
function av_new_stream(s: PAVFormatContext; id: cint): PAVStream;
  cdecl; external av__format;
{$IF LIBAVFORMAT_VERSION >= 51014000} // 51.14.0
function av_new_program(s: PAVFormatContext; id: cint): PAVProgram;
  cdecl; external av__format;
{$IFEND}

{$IF LIBAVFORMAT_VERSION >= 52014000} // 52.14.0
(**
 * Adds a new chapter.
 * This function is NOT part of the public API
 * and should ONLY be used by demuxers.
 *
 * @param s media file handle
 * @param id unique ID for this chapter
 * @param start chapter start time in time_base units
 * @param end chapter end time in time_base units
 * @param title chapter title
 *
 * @return AVChapter or NULL on error
 *)
function ff_new_chapter(s: PAVFormatContext; id: cint; time_base: TAVRational;
                        start, end_: cint64; title: {const} PAnsiChar): PAVChapter;
  cdecl; external av__format;
{$IFEND}

(**
 * Sets the pts for a given stream.
 *
 * @param s stream
 * @param pts_wrap_bits number of bits effectively used by the pts
 *        (used for wrap control, 33 is the value for MPEG)
 * @param pts_num numerator to convert to seconds (MPEG: 1)
 * @param pts_den denominator to convert to seconds (MPEG: 90000)
 *)
procedure av_set_pts_info(s: PAVStream; pts_wrap_bits: cint;
{$IF LIBAVFORMAT_VERSION < 52036000} // < 52.36.0
                   pts_num: cint; pts_den: cint);
{$ELSE}
                   pts_num: cuint; pts_den: cuint);
{$IFEND}
  cdecl; external av__format;

const
  AVSEEK_FLAG_BACKWARD = 1; ///< seek backward
  AVSEEK_FLAG_BYTE     = 2; ///< seeking based on position in bytes
  AVSEEK_FLAG_ANY      = 4; ///< seek to any frame, even non-keyframes
{$IF LIBAVFORMAT_VERSION >= 52037000} // >= 52.37.0
  AVSEEK_FLAG_FRAME    = 8;
{$IFEND}

function av_find_default_stream_index(s: PAVFormatContext): cint;
  cdecl; external av__format;

(**
 * Gets the index for a specific timestamp.
 * @param flags if AVSEEK_FLAG_BACKWARD then the returned index will correspond
 *                 to the timestamp which is <= the requested one, if backward
 *                 is 0, then it will be >=
 *              if AVSEEK_FLAG_ANY seek to any frame, only keyframes otherwise
 * @return < 0 if no such timestamp could be found
 *)
function av_index_search_timestamp(st: PAVStream; timestamp: cint64; flags: cint): cint;
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 52004000} // 52.4.0
(**
 * Ensures the index uses less memory than the maximum specified in
 * AVFormatContext.max_index_size by discarding entries if it grows
 * too large.
 * This function is not part of the public API and should only be called
 * by demuxers.
 *)
procedure ff_reduce_index(s: PAVFormatContext; stream_index: cint);
  cdecl; external av__format;
{$IFEND}

(**
 * Adds an index entry into a sorted list. Updates the entry if the list
 * already contains it.
 *
 * @param timestamp timestamp in the timebase of the given stream
 *)
function av_add_index_entry(st: PAVStream; pos: cint64; timestamp: cint64;
                  size: cint; distance: cint; flags: cint): cint;
  cdecl; external av__format;

(**
 * Does a binary search using av_index_search_timestamp() and
 * AVCodec.read_timestamp().
 * This is not supposed to be called directly by a user application,
 * but by demuxers.
 * @param target_ts target timestamp in the time base of the given stream
 * @param stream_index stream number
 *)
function av_seek_frame_binary(s: PAVFormatContext; stream_index: cint;
                 target_ts: cint64; flags: cint): cint;
  cdecl; external av__format;


(**
 * Updates cur_dts of all streams based on the given timestamp and AVStream.
 *
 * Stream ref_st unchanged, others set cur_dts in their native time base.
 * Only needed for timestamp wrapping or if (dts not set and pts!=dts).
 * @param timestamp new dts expressed in time_base of param ref_st
 * @param ref_st reference stream giving time_base of param timestamp
 *)
procedure av_update_cur_dts(s: PAVFormatContext; ref_st: PAVStream;
                            timestamp: cint64);
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 51007000} // 51.7.0
type
  TReadTimestampFunc = function (pavfc: PAVFormatContext;
    arg2: cint; arg3: Pint64; arg4: cint64): cint64; cdecl;

(**
 * Does a binary search using read_timestamp().
 * This is not supposed to be called directly by a user application,
 * but by demuxers.
 * @param target_ts target timestamp in the time base of the given stream
 * @param stream_index stream number
 *)
function av_gen_search(s: PAVFormatContext; stream_index: cint;
                       target_ts: cint64; pos_min: cint64;
                       pos_max: cint64; pos_limit: cint64;
                       ts_min: cint64; ts_max: cint64;
                       flags: cint; ts_ret: Pint64;
                       read_timestamp: TReadTimestampFunc): cint64;
  cdecl; external av__format;
{$IFEND}

(* media file output *)
function av_set_parameters(s: PAVFormatContext; ap: PAVFormatParameters): cint;
  cdecl; external av__format;

(**
 * Allocates the stream private data and writes the stream header to an
 * output media file.
 *
 * @param s media file handle
 * @return 0 if OK, AVERROR_xxx on error
 *)
function av_write_header(s: PAVFormatContext): cint;
  cdecl; external av__format;

(**
 * Writes a packet to an output media file.
 *
 * The packet shall contain one audio or video frame.
 * The packet must be correctly interleaved according to the container
 * specification, if not then av_interleaved_write_frame must be used.
 *
 * @param s media file handle
 * @param pkt The packet, which contains the stream_index, buf/buf_size,
 *            dts/pts, ...
 * @return < 0 on error, = 0 if OK, 1 if end of stream wanted
 *)
function av_write_frame(s: PAVFormatContext; var pkt: TAVPacket): cint;
  cdecl; external av__format;

(**
 * Writes a packet to an output media file ensuring correct interleaving.
 *
 * The packet must contain one audio or video frame.
 * If the packets are already correctly interleaved, the application should
 * call av_write_frame() instead as it is slightly faster. It is also important
 * to keep in mind that completely non-interleaved input will need huge amounts
 * of memory to interleave with this, so it is preferable to interleave at the
 * demuxer level.
 *
 * @param s media file handle
 * @param pkt The packet, which contains the stream_index, buf/buf_size,
 *            dts/pts, ...
 * @return < 0 on error, = 0 if OK, 1 if end of stream wanted
 *)
function av_interleaved_write_frame(s: PAVFormatContext; var pkt: TAVPacket): cint;
  cdecl; external av__format;

(**
 * Interleaves a packet per dts in an output media file.
 *
 * Packets with pkt->destruct == av_destruct_packet will be freed inside this
 * function, so they cannot be used after it. Note that calling av_free_packet()
 * on them is still safe.
 *
 * @param s media file handle
 * @param out the interleaved packet will be output here
 * @param in the input packet
 * @param flush 1 if no further packets are available as input and all
 *              remaining packets should be output
 * @return 1 if a packet was output, 0 if no packet could be output,
 *         < 0 if an error occurred
 *)
function av_interleave_packet_per_dts(s: PAVFormatContext; _out: PAVPacket;
                                      pkt: PAVPacket; flush: cint): cint;
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION >= 52025000} // 52.25.0
(**
 * Add packet to AVFormatContext->packet_buffer list, determining its
 * interleaved position using compare() function argument.
 *
 * This function is not part of the public API and should only be called
 * by muxers using their own interleave function.
 *)
{
procedure ff_interleave_add_packet(s:   PAVFormatContext;
                                   pkt: PAVPacket;
		   compare: function(para1: PAVFormatContext;
				     para2: PAVPacket;
				     para3: PAVPacket): cint);
  cdecl; external av__format;
}
{$IFEND}

(**
 * Writes the stream trailer to an output media file and frees the
 * file private data.
 *
 * May only be called after a successful call to av_write_header.
 *
 * @param s media file handle
 * @return 0 if OK, AVERROR_xxx on error
 *)
function av_write_trailer(s: pAVFormatContext): cint;
  cdecl; external av__format;

procedure dump_format(ic: PAVFormatContext; index: cint; url: PAnsiChar;
               is_output: cint);
  cdecl; external av__format;

(**
 * Parses width and height out of string str.
 * @deprecated Use av_parse_video_frame_size instead.
 *)
function parse_image_size(width_ptr: PCint; height_ptr: PCint;
                          str: PAnsiChar): cint;
  cdecl; external av__format; deprecated;

{$IF LIBAVFORMAT_VERSION_MAJOR < 53}
(**
 * Converts framerate from a string to a fraction.
 * @deprecated Use av_parse_video_frame_rate instead.
 *)
function parse_frame_rate(frame_rate: PCint; frame_rate_base: PCint;
                          arg: PByteArray): cint;
  cdecl; external av__format; deprecated;
{$IFEND}

(**
 * Parses datestr and returns a corresponding number of microseconds.
 * @param datestr String representing a date or a duration.
 * - If a date the syntax is:
 * @code
 *  [{YYYY-MM-DD|YYYYMMDD}]{T| }{HH[:MM[:SS[.m...]]][Z]|HH[MM[SS[.m...]]][Z]}
 * @endcode
 * Time is localtime unless Z is appended, in which case it is
 * interpreted as UTC.
 * If the year-month-day part is not specified it takes the current
 * year-month-day.
 * Returns the number of microseconds since 1st of January, 1970 up to
 * the time of the parsed date or INT64_MIN if datestr cannot be
 * successfully parsed.
 * - If a duration the syntax is:
 * @code
 *  [-]HH[:MM[:SS[.m...]]]
 *  [-]S+[.m...]
 * @endcode
 * Returns the number of microseconds contained in a time interval
 * with the specified duration or INT64_MIN if datestr cannot be
 * successfully parsed.
 * @param duration Flag which tells how to interpret datestr, if
 * not zero datestr is interpreted as a duration, otherwise as a
 * date.
 *)
function parse_date(datestr: PAnsiChar; duration: cint): cint64;
  cdecl; external av__format;

(** Gets the current time in microseconds. *)
function av_gettime(): cint64;
  cdecl; external av__format;

(* ffm-specific for ffserver *)
const
  FFM_PACKET_SIZE = 4096;

function ffm_read_write_index(fd: cint): cint64;
  cdecl; external av__format;

{$IF LIBAVFORMAT_VERSION < 52027000} // 52.27.0
procedure ffm_write_write_index(fd: cint; pos: cint64);
{$ELSE}
function ffm_write_write_index(fd: cint; pos: cint64): cint;
{$IFEND}
  cdecl; external av__format;

procedure ffm_set_write_index(s: PAVFormatContext; pos: cint64; file_size: cint64);
  cdecl; external av__format;

(**
 * Attempts to find a specific tag in a URL.
 *
 * syntax: '?tag1=val1&tag2=val2...'. Little URL decoding is done.
 * Return 1 if found.
 *)
function find_info_tag(arg: PAnsiChar; arg_size: cint; tag1: PAnsiChar; info: PAnsiChar): cint;
  cdecl; external av__format;

(**
 * Returns in 'buf' the path with '%d' replaced by a number.
 *
 * Also handles the '%0nd' format where 'n' is the total number
 * of digits and '%%'.
 *
 * @param buf destination buffer
 * @param buf_size destination buffer size
 * @param path numbered sequence string
 * @param number frame number
 * @return 0 if OK, -1 on format error
 *)
function av_get_frame_filename(buf: PAnsiChar; buf_size: cint;
                               path: PAnsiChar; number: cint): cint;
  cdecl; external av__format
  {$IF LIBAVFORMAT_VERSION <= 50006000} // 50.6.0
  name 'get_frame_filename'
  {$IFEND};

(**
 * Checks whether filename actually is a numbered sequence generator.
 *
 * @param filename possible numbered sequence string
 * @return 1 if a valid numbered sequence string, 0 otherwise
 *)
function av_filename_number_test(filename: PAnsiChar): cint;
  cdecl; external av__format
  {$IF LIBAVFORMAT_VERSION <= 50006000} // 50.6.0
  name 'filename_number_test'
  {$IFEND};

{$IF LIBAVFORMAT_VERSION >= 51012002} // 51.12.2
(**
 * Generates an SDP for an RTP session.
 *
 * @param ac array of AVFormatContexts describing the RTP streams. If the
 *           array is composed by only one context, such context can contain
 *           multiple AVStreams (one AVStream per RTP stream). Otherwise,
 *           all the contexts in the array (an AVCodecContext per RTP stream)
 *           must contain only one AVStream.
 * @param n_files number of AVCodecContexts contained in ac
 * @param buff buffer where the SDP will be stored (must be allocated by
 *             the caller)
 * @param size the size of the buffer
 * @return 0 if OK, AVERROR_xxx on error
 *)
function avf_sdp_create(ac: PPAVFormatContext; n_files: cint; buff: PByteArray; size: cint): cint;
  cdecl; external av__format;
{$IFEND}

implementation

{$IF LIBAVFORMAT_VERSION < 51012002} // 51.12.2
procedure av_init_packet(var pkt: TAVPacket);
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

{$IF LIBAVFORMAT_VERSION < 52032000} // < 52.32.0
procedure av_free_packet(pkt: PAVPacket);
begin
  if ((pkt <> nil) and (@pkt^.destruct <> nil)) then
    pkt^.destruct(pkt);
end;
{$IFEND}

end.
