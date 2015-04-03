(*
 * AVOptions
 * copyright (c) 2005 Michael Niedermayer <michaelni@gmx.at>
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
 * Conversion of libavutil/frame.h
 * avutil version 54.7.100
 *
*)

const
  AV_NUM_DATA_POINTERS = 8;

    (** from the definitions of TAVFrame *)

    (**
     * The frame data may be corrupted, e.g. due to decoding errors.
     *)
  AV_FRAME_FLAG_CORRUPT = (1 << 0);

  FF_DECODE_ERROR_INVALID_BITSTREAM = 1;
  FF_DECODE_ERROR_MISSING_REFERENCE = 2;


type
(* is already in pixfmt.pas
  TAVColorSpace = (
    AVCOL_SPC_RGB         = 0,
    AVCOL_SPC_BT709       = 1,  ///< also ITU-R BT1361 / IEC 61966-2-4 xvYCC709 / SMPTE RP177 Annex B
    AVCOL_SPC_UNSPECIFIED = 2,
    AVCOL_SPC_FCC         = 4,
    AVCOL_SPC_BT470BG     = 5,  ///< also ITU-R BT601-6 625 / ITU-R BT1358 625 / ITU-R BT1700 625 PAL & SECAM / IEC 61966-2-4 xvYCC601
    AVCOL_SPC_SMPTE170M   = 6,  ///< also ITU-R BT601-6 525 / ITU-R BT1358 525 / ITU-R BT1700 NTSC / functionally identical to above
    AVCOL_SPC_SMPTE240M_  = 7,
    AVCOL_SPC_YCGCO       = 8,  ///< Used by Dirac / VC-2 and H.264 FRext, see ITU-T SG16
    AVCOL_SPC_BT2020_NCL  = 9,  ///< ITU-R BT2020 non-constant luminance system
    AVCOL_SPC_BT2020_CL   = 10, ///< ITU-R BT2020 constant luminance system
    AVCOL_SPC_NB                ///< Not part of ABI
  );

  TAVColorRange = (
    AVCOL_RANGE_UNSPECIFIED = 0,
    AVCOL_RANGE_MPEG        = 1, ///< the normal 219*2^(n-8) "MPEG" YUV ranges
    AVCOL_RANGE_JPEG        = 2, ///< the normal     2^n-1   "JPEG" YUV ranges
    AVCOL_RANGE_NB               ///< Not part of ABI
  );
*)

(* Note: AVPanScan is defined in avcodec.h but is here to avoid reference problems - Brian-ch 28/09/2014
 *
 * Pan Scan area.
 * This specifies the area which should be displayed.
 * Note there may be multiple such areas for one frame.
 *)
  PAVPanScan = ^TAVPanScan;
  TAVPanScan = record {24}
    (*** id.
     * - encoding: set by user.
     * - decoding: set by libavcodec. *)
    id: cint;

    (*** width and height in 1/16 pel
     * - encoding: set by user.
     * - decoding: set by libavcodec. *)
    width: cint;
    height: cint;

    (*** position of the top left corner in 1/16 pel for up to 3 fields/frames.
     * - encoding: set by user.
     * - decoding: set by libavcodec. *)
    position: array [0..2] of array [0..1] of cint16;
  end; {TAVPanScan}
  
  (**
   * @defgroup lavu_frame AVFrame
   * @ingroup lavu_data
   *
   * @
   * AVFrame is an abstraction for reference-counted raw multimedia data.
   *)  
  TAVFrameSideDataType = (
    (**
     * The data is the AVPanScan struct defined in libavcodec.
     *)
    AV_FRAME_DATA_PANSCAN,
    (**
     * ATSC A53 Part 4 Closed Captions.
     * A53 CC bitstream is stored as uint8_t in AVFrameSideData.data.
     * The number of bytes of CC data is AVFrameSideData.size.
     *)
    AV_FRAME_DATA_A53_CC,
    (**
     * Stereoscopic 3d metadata.
     * The data is the AVStereo3D struct defined in libavutil/stereo3d.h.
     *)
    AV_FRAME_DATA_STEREO3D,
    (**
     * The data is the AVMatrixEncoding enum defined in libavutil/channel_layout.h.
     *)
    AV_FRAME_DATA_MATRIXENCODING,
    (**
     * Metadata relevant to a downmix procedure.
     * The data is the AVDownmixInfo struct defined in libavutil/downmix_info.h.
     *)
    AV_FRAME_DATA_DOWNMIX_INFO,
    (**
     * ReplayGain information in the form of the AVReplayGain struct.
     *)
    AV_FRAME_DATA_REPLAYGAIN,
    (**
     * This side data contains a 3x3 transformation matrix describing an affine
     * transformation that needs to be applied to the frame for correct
     * presentation.
     *
     * See libavutil/display.h for a detailed description of the data.
     *)
    AV_FRAME_DATA_DISPLAYMATRIX,
    (**
     * Active Format Description data consisting of a single byte as specified
     * in ETSI TS 101 154 using AVActiveFormatDescription enum.
     *)
    AV_FRAME_DATA_AFD,
    (**
     * Motion vectors exported by some codecs (on demand through the export_mvs
     * flag set in the libavcodec AVCodecContext flags2 option).
     * The data is the AVMotionVector struct defined in
     * libavutil/motion_vector.h.
     *)
    AV_FRAME_DATA_MOTION_VECTORS,
    (**
     * Recommmends skipping the specified number of samples. This is exported
     * only if the "skip_manual" AVOption is set in libavcodec.
     * This has the same format as AV_PKT_DATA_SKIP_SAMPLES.
     * @code
     * u32le number of samples to skip from start of this packet
     * u32le number of samples to skip from end of this packet
     * u8    reason for start skip
     * u8    reason for end   skip (0=padding silence, 1=convergence)
     * @endcode
     *)
    AV_FRAME_DATA_SKIP_SAMPLES
  );

	TAVActiveFormatDescription = (
    AV_AFD_SAME         = 8,
    AV_AFD_4_3          = 9,
    AV_AFD_16_9         = 10,
    AV_AFD_14_9         = 11,
    AV_AFD_4_3_SP_14_9  = 13,
    AV_AFD_16_9_SP_14_9 = 14,
    AV_AFD_SP_4_3       = 15
	); {TAVActiveFormatDescription}

  PAVFrameSideData = ^TAVFrameSideData;
  TAVFrameSideData = record
    type_: TAVFrameSideDataType;
    data:  PByte;
    size:  cint;
    metadata: TAVDictionary;
  end; {TAVFrameSideData}

(**
 * This structure describes decoded (raw) audio or video data.
 *
 * AVFrame must be allocated using av_frame_alloc(). Note that this only
 * allocates the AVFrame itself, the buffers for the data must be managed
 * through other means (see below).
 * AVFrame must be freed with av_frame_free().
 *
 * AVFrame is typically allocated once and then reused multiple times to hold
 * different data (e.g. a single AVFrame to hold frames received from a
 * decoder). In such a case, av_frame_unref() will free any references held by
 * the frame and reset it to its original clean state before it
 * is reused again.
 *
 * The data described by an AVFrame is usually reference counted through the
 * AVBuffer API. The underlying buffer references are stored in AVFrame.buf /
 * AVFrame.extended_buf. An AVFrame is considered to be reference counted if at
 * least one reference is set, i.e. if AVFrame.buf[0] != NULL. In such a case,
 * every single data plane must be contained in one of the buffers in
 * AVFrame.buf or AVFrame.extended_buf.
 * There may be a single buffer for all the data, or one separate buffer for
 * each plane, or anything in between.
 *
 * sizeof(AVFrame) is not a part of the public ABI, so new fields may be added
 * to the end with a minor bump.
 * Similarly fields that are marked as to be only accessed by
 * av_opt_ptr() can be reordered. This allows 2 forks to add fields
 * without breaking compatibility with each other.
*)
  PPAVFrame = ^PAVFrame;
  PAVFrame = ^TAVFrame;
  TAVFrame = record
    (**
     * pointer to the picture/channel planes.
     * This might be different from the first allocated byte
     *
     * Some decoders access areas outside 0,0 - width,height, please
     * see avcodec_align_dimensions2(). Some filters and swscale can read
     * up to 16 bytes beyond the planes, if these filters are to be used,
     * then 16 extra bytes must be allocated.
     *)
    data: array [0..AV_NUM_DATA_POINTERS - 1] of pbyte;

    (**
     * For video, size in bytes of each picture line.
     * For audio, size in bytes of each plane.
     *
     * For audio, only linesize[0] may be set. For planar audio, each channel
     * plane must be the same size.
     *
     * For video the linesizes should be multiples of the CPUs alignment
     * preference, this is 16 or 32 for modern desktop CPUs.
     * Some code requires such alignment other code can be slower without
     * correct alignment, for yet other it makes no difference.
     *
     * @note The linesize may be larger than the size of usable data -- there
     * may be extra padding present for performance reasons.
     *)
    linesize: array [0..AV_NUM_DATA_POINTERS - 1] of cint;

    (**
     * pointers to the data planes/channels.
     *
     * For video, this should simply point to data[].
     *
     * For planar audio, each channel has a separate data pointer, and
     * linesize[0] contains the size of each channel buffer.
     * For packed audio, there is just one data pointer, and linesize[0]
     * contains the total size of the buffer for all channels.
     *
     * Note: Both data and extended_data will always be set by get_buffer(),
     * but for planar audio with more channels that can fit in data,
     * extended_data must be used in order to access all channels.
     *)
    extended_data: ^pbyte;

    (**
     * width and height of the video frame
     *)
    width, height: cint;
    (**
     * number of audio samples (per channel) described by this frame
     *)
    nb_samples: cint;

    (**
     * format of the frame, -1 if unknown or unset
     * Values correspond to enum AVPixelFormat for video frames,
     * enum AVSampleFormat for audio)
     *)
    format: cint;

    (**
     * 1 -> keyframe, 0-> not
     *)
    key_frame: cint;

    (**
     * Picture type of the frame
     *)
    pict_type: TAVPictureType;

{$IFDEF FF_API_AVFRAME_LAVC}
    base: array [0..AV_NUM_DATA_POINTERS - 1] of pbyte; {deprecated}
{$ENDIF}

    (**
     * sample aspect ratio for the video frame, 0/1 if unknown/unspecified
     *)
    sample_aspect_ratio: TAVRational;

    (**
     * presentation timestamp in time_base units (time when frame should be shown to user)
     *)
    pts: cint64;

    (**
     * pts copied from the AVPacket that was decoded to produce this frame
     *)
    pkt_pts: cint64;

    (**
     * DTS copied from the AVPacket that triggered returning this frame. (if frame threading isn't used)
     * This is also the Presentation time of this AVFrame calculated from
     * only AVPacket.dts values without pts values.
     *)
    pkt_dts: cint64;

    (**
     * picture number in bitstream order
     *)
    coded_picture_number: cint;

    (**
     * picture number in display order
     *)
    display_picture_number: cint;

    (**
     * quality (between 1 (good) and FF_LAMBDA_MAX (bad))
     *)
    quality: cint;

{$IFDEF FF_API_AVFRAME_LAVC}
    reference: cint; {deprecated}

    (**
     * QP table
     *)
    qscale_table: pbyte; {deprecated}

    (**
     * QP store stride
     *)
    qstride: cint; {deprecated}
    qscale_type: cint; {deprecated}

    (**
     * mbskip_table[mb]>=1 if MB didn't change
     * stride= mb_width = (width+15)>>4
     *)
    mbskip_table: pbyte; {deprecated}

    (**
     * motion vector table
     * @code
     * example:
     * int mv_sample_log2= 4 - motion_subsample_log2;
     * int mb_width= (width+15)>>4;
     * int mv_stride= (mb_width << mv_sample_log2) + 1;
     * motion_val[direction][x + y*mv_stride][0->mv_x, 1->mv_y];
     * @endcode
     *)
    motion_val: array [0..1] of pointer;

    (**
     * macroblock type table
     * mb_type_base + mb_width + 2
     *)
    mb_type: PCuint; {deprecated}

    (**
     * DCT coefficients
     *)
    dct_coeff: PsmallInt; {deprecated}

    (**
     * motion reference frame index
     * the order in which these are stored can depend on the codec.
     *)
    ref_index: array [0..1] of pbyte; {deprecated}
{$ENDIF}

    (**
     * for some private data of the user
     *)
    opaque: pointer;

    (**
     * error
     *)
    error: array [0..AV_NUM_DATA_POINTERS - 1] of cuint64;

{$IFDEF FF_API_AVFRAME_LAVC}
    type_: cint; {deprecated}
{$ENDIF}

    (**
     * When decoding, this signals how much the picture must be delayed.
     * extra_delay = repeat_pict / (2*fps)
     *)
    repeat_pict: cint;

    (**
     * The content of the picture is interlaced.
     *)
    interlaced_frame: cint;

    (**
     * If the content is interlaced, is top field displayed first.
     *)
    top_field_first: cint;

    (**
     * Tell user application that palette has changed from previous frame.
     *)
    palette_has_changed: cint;

{$IFDEF FF_API_AVFRAME_LAVC}
    buffer_hints: cint; {deprecated}
    (**
     * Pan scan.
     *)
    pan_scan: PAVPanScan; {deprecated}
{$ENDIF}

    (**
     * reordered opaque 64bit (generally an integer or a double precision float
     * PTS but can be anything).
     * The user sets AVCodecContext.reordered_opaque to represent the input at
     * that time,
     * the decoder reorders values as needed and sets AVFrame.reordered_opaque
     * to exactly one of the values provided by the user through AVCodecContext.reordered_opaque
     * @deprecated in favor of pkt_pts
     *)
    reordered_opaque: cint64;

{$IFDEF FF_API_AVFRAME_LAVC}
    (**
     * @deprecated this field is unused
     *)
    hwaccel_picture_private: pointer; {deprecated}
    owner: pointer; {deprecated} (** Note: Should be a PAVCodecContext, but a type pointer is used to avoid a reference problem. *)
    thread_opaque: pointer; {deprecated}

    (**
     * log2 of the size of the block which a single vector in motion_val represents:
     * (4->16x16, 3->8x8, 2-> 4x4, 1-> 2x2)
     *)
    motion_subsample_log2: cuint8; {deprecated}
{$ENDIF}

    (**
     * Sample rate of the audio data.
     *)
    sample_rate: cint;

    (**
     * Channel layout of the audio data.
     *)
    channel_layout: cuint64;

    (**
     * AVBuffer references backing the data for this frame. If all elements of
     * this array are NULL, then this frame is not reference counted.
     *
     * There may be at most one AVBuffer per data plane, so for video this array
     * always contains all the references. For planar audio with more than
     * AV_NUM_DATA_POINTERS channels, there may be more buffers than can fit in
     * this array. Then the extra AVBufferRef pointers are stored in the
     * extended_buf array.
     *)
    buf: array [0..AV_NUM_DATA_POINTERS - 1] of PAVBufferRef;

    (**
     * For planar audio which requires more than AV_NUM_DATA_POINTERS
     * AVBufferRef pointers, this array will hold all the references which
     * cannot fit into AVFrame.buf.
     *
     * Note that this is different from AVFrame.extended_data, which always
     * contains all the pointers. This array only contains the extra pointers,
     * which cannot fit into AVFrame.buf.
     *
     * This array is always allocated using av_malloc() by whoever constructs
     * the frame. It is freed in av_frame_unref().
     *)
    extended_buf: PPAVBufferRef;

    (**
     * Number of elements in extended_buf.
     *)
    nb_extended_buf: cint;

    side_data: ^PAVFrameSideData;
    nb_side_data: cint;

    (**
     * @defgroup lavu_frame_flags AV_FRAME_FLAGS
     * Flags describing additional frame properties.
     *
     * @
     *)

    (**
     * Frame flags, a combination of @ref lavu_frame_flags
     *)
    flags: cint;

    (**
     * MPEG vs JPEG YUV range.
     * It must be accessed using av_frame_get_color_range() and
     * av_frame_set_color_range().
     * - encoding: Set by user
     * - decoding: Set by libavcodec
     *)
    color_range: TAVColorRange;

    color_primaries: TAVColorPrimaries;

    color_trc: TAVColorTransferCharacteristic;

    (**
     * YUV colorspace type.
     * It must be accessed using av_frame_get_colorspace() and
     * av_frame_set_colorspace().
     * - encoding: Set by user
     * - decoding: Set by libavcodec
     *)
    colorspace: TAVColorSpace;

    chroma_location: TAVChromaLocation;

    (**
     * frame timestamp estimated using various heuristics, in stream time base
     * Code outside libavcodec should access this field using:
     * av_frame_get_best_effort_timestamp(frame)
     * - encoding: unused
     * - decoding: set by libavcodec, read by user.
     *)
    best_effort_timestamp: cint64;

    (**
     * reordered pos from the last AVPacket that has been input into the decoder
     * Code outside libavcodec should access this field using:
     * av_frame_get_pkt_pos(frame)
     * - encoding: unused
     * - decoding: Read by user.
     *)
    pkt_pos: cint64;

    (**
     * duration of the corresponding packet, expressed in
     * AVStream->time_base units, 0 if unknown.
     * Code outside libavcodec should access this field using:
     * av_frame_get_pkt_duration(frame)
     * - encoding: unused
     * - decoding: Read by user.
     *)
    pkt_duration: cint64;

    (**
     * metadata.
     * Code outside libavcodec should access this field using:
     * av_frame_get_metadata(frame)
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    metadata: PAVDictionary;

    (**
     * decode error flags of the frame, set to a combination of
     * FF_DECODE_ERROR_xxx flags if the decoder produced a frame, but there
     * were errors during the decoding.
     * Code outside libavcodec should access this field using:
     * av_frame_get_decode_error_flags(frame)
     * - encoding: unused
     * - decoding: set by libavcodec, read by user.
     *)
    decode_error_flags: cint;

    (**
     * number of audio channels, only used for audio.
     * Code outside libavcodec should access this field using:
     * av_frame_get_channels(frame)
     * - encoding: unused
     * - decoding: Read by user.
     *)
    channels: cint;

    (**
     * size of the corresponding packet containing the compressed
     * frame. It must be accessed using av_frame_get_pkt_size() and
     * av_frame_set_pkt_size().
     * It is set to a negative value if unknown.
     * - encoding: unused
     * - decoding: set by libavcodec, read by user.
     *)
    pkt_size: cint;

    (**
     * Not to be accessed directly from outside libavutil
     *)
    qp_table_buf: PAVBufferRef;
  end; {TAVFrame}

(**
 * Accessors for some AVFrame fields.
 * The position of these field in the structure is not part of the ABI,
 * they should not be accessed directly outside libavcodec.
 *)
function  av_frame_get_best_effort_timestamp(frame: {const} PAVFrame): cint64;
  cdecl; external av__codec; overload;
procedure av_frame_set_best_effort_timestamp(frame: PAVFrame; val: cint64);
  cdecl; external av__codec; overload;
function  av_frame_get_pkt_duration         (frame: {const} PAVFrame): cint64;
  cdecl; external av__codec; overload;
procedure av_frame_get_pkt_duration         (frame: PAVFrame; val: cint64);
  cdecl; external av__codec; overload;
function  av_frame_get_pkt_pos              (frame: {const} PAVFrame): cint64;
  cdecl; external av__codec; overload;
procedure av_frame_get_pkt_pos              (frame: PAVFrame; val: cint64);
  cdecl; external av__codec; overload;
function  av_frame_get_channel_layout       (frame: {const} PAVFrame): cint64;
  cdecl; external av__codec; overload;
procedure av_frame_get_channel_layout       (frame: PAVFrame; val: cint64);
  cdecl; external av__codec; overload;
function  av_frame_get_channels             (frame: {const} PAVFrame): cint;
  cdecl; external av__codec;
procedure av_frame_set_channels             (frame: PAVFrame; val: cint);
  cdecl; external av__codec;
function  av_frame_get_sample_rate          (frame: {const} PAVFrame): cint;
  cdecl; external av__codec;
procedure av_frame_set_sample_rate          (frame: PAVFrame; val: cint);
  cdecl; external av__codec;
function  av_frame_get_metadata             (frame: {const} PAVFrame): PAVDictionary;
  cdecl; external av__codec;
procedure av_frame_set_metadata             (frame: PAVFrame; val: PAVDictionary);
  cdecl; external av__codec;
function  av_frame_get_decode_error_flags   (frame: {const} PAVFrame): cint;
  cdecl; external av__codec;
procedure av_frame_set_decode_error_flags   (frame: PAVFrame; val: cint);
  cdecl; external av__codec;
function  av_frame_get_pkt_size             (frame: {const} PAVFrame): cint;
  cdecl; external av__codec;
procedure av_frame_set_pkt_size             (frame: PAVFrame; val: cint);
  cdecl; external av__codec;
function avpriv_frame_get_metadatap(frame: PAVFrame): PPAVDictionary;
  cdecl; external av__codec;
function av_frame_get_qp_table(f: PAVFrame; stride: pcint; type_: pcint): PByte;
  cdecl; external av__codec;
function av_frame_set_qp_table(f: PAVFrame; buf: PAVBufferRef; stride: cint; type_: cint): cint;
  cdecl; external av__codec;
function av_frame_get_colorspace(frame: {const} PAVFrame): TAVColorSpace;
  cdecl; external av__codec;
procedure av_frame_set_colorspace(frame: PAVFrame; val: TAVColorSpace);
  cdecl; external av__codec;
procedure av_frame_set_color_range(frame: PAVFrame; val: TAVColorSpace);
  cdecl; external av__codec;

(**
 * Get the name of a colorspace.
 * @return a static string identifying the colorspace; can be NULL.
 *)
function av_get_colorspace_name(val: TAVColorSpace): PAnsiChar;
  cdecl; external av__codec;

(**
 * Allocate an AVFrame and set its fields to default values.  The resulting
 * struct must be freed using av_frame_free().
 *
 * @return An AVFrame filled with default values or NULL on failure.
 *
 * @note this only allocates the AVFrame itself, not the data buffers. Those
 * must be allocated through other means, e.g. with av_frame_get_buffer() or
 * manually.
 *)
function av_frame_alloc(): PAVFrame;
  cdecl; external av__codec;

(**
 * Free the frame and any dynamically allocated objects in it,
 * e.g. extended_data. If the frame is reference counted, it will be
 * unreferenced first.
 *
 * @param frame frame to be freed. The pointer will be set to NULL.
 *)
procedure av_frame_free(frame: PPAVFrame);
  cdecl; external av__codec;

(**
 * Set up a new reference to the data described by the source frame.
 *
 * Copy frame properties from src to dst and create a new reference for each
 * AVBufferRef from src.
 *
 * If src is not reference counted, new buffers are allocated and the data is
 * copied.
 *
 * @return 0 on success, a negative AVERROR on error
 *)
function av_frame_ref(dst: PAVFrame; src: {const} PAVFrame): cint;
  cdecl; external av__codec;

(**
 * Create a new frame that references the same data as src.
 *
 * This is a shortcut for av_frame_alloc()+av_frame_ref().
 *
 * @return newly created AVFrame on success, NULL on error.
 *)
function av_frame_clone(src: {const} PAVFrame): PAVFrame;
  cdecl; external av__codec;

(**
 * Unreference all the buffers referenced by frame and reset the frame fields.
 *)
procedure av_frame_unref(frame: PAVFrame);
  cdecl; external av__codec;

(**
 * Move everythnig contained in src to dst and reset src.
 *)
procedure av_frame_move_ref(dst, src: PAVFrame);
  cdecl; external av__codec;

(**
 * Allocate new buffer(s) for audio or video data.
 *
 * The following fields must be set on frame before calling this function:
 * - format (pixel format for video, sample format for audio)
 * - width and height for video
 * - nb_samples and channel_layout for audio
 *
 * This function will fill AVFrame.data and AVFrame.buf arrays and, if
 * necessary, allocate and fill AVFrame.extended_data and AVFrame.extended_buf.
 * For planar formats, one buffer will be allocated for each plane.
 *
 * @param frame frame in which to store the new buffers.
 * @param align required buffer size alignment
 *
 * @return 0 on success, a negative AVERROR on error.
 *)
function av_frame_get_buffer(frame: PAVFrame; align: cint): cint;
  cdecl; external av__codec;

(**
 * Check if the frame data is writable.
 *
 * @return A positive value if the frame data is writable (which is true if and
 * only if each of the underlying buffers has only one reference, namely the one
 * stored in this frame). Return 0 otherwise.
 *
 * If 1 is returned the answer is valid until av_buffer_ref() is called on any
 * of the underlying AVBufferRefs (e.g. through av_frame_ref() or directly).
 *
 * @see av_frame_make_writable(), av_buffer_is_writable()
 *)
function av_frame_is_writable(frame: PAVFrame): cint;
  cdecl; external av__codec;

(**
 * Ensure that the frame data is writable, avoiding data copy if possible.
 *
 * Do nothing if the frame is writable, allocate new buffers and copy the data
 * if it is not.
 *
 * @return 0 on success, a negative AVERROR on error.
 *
 * @see av_frame_is_writable(), av_buffer_is_writable(),
 * av_buffer_make_writable()
 *)
function av_frame_make_writable(frame: PAVFrame): cint;
  cdecl; external av__codec;

(**
 * Copy the frame data from src to dst.
 *
 * This function does not allocate anything, dst must be already initialized and
 * allocated with the same parameters as src.
 *
 * This function only copies the frame data (i.e. the contents of the data /
 * extended data arrays), not any other properties.
 *
 * @return >= 0 on success, a negative AVERROR on error.
 *)
function av_frame_copy(dst: PAVFrame; src: {const} PAVFrame): cint;
  cdecl; external av__codec;
  
(**
 * Copy only "metadata" fields from src to dst.
 *
 * Metadata for the purpose of this function are those fields that do not affect
 * the data layout in the buffers.  E.g. pts, sample rate (for audio) or sample
 * aspect ratio (for video), but not width/height or channel layout.
 * Side data is also copied.
 *)
function av_frame_copy_props(dst: PAVFrame; src: {const} PAVFrame): cint;
  cdecl; external av__codec;

(**
 * Get the buffer reference a given data plane is stored in.
 *
 * @param plane index of the data plane of interest in frame->extended_data.
 *
 * @return the buffer reference that contains the plane or NULL if the input
 * frame is not valid.
 *)
function av_frame_get_plane_buffer(frame: PAVFrame; plane: cint): PAVBufferRef;
  cdecl; external av__codec;

(**
 * Add a new side data to a frame.
 *
 * @param frame a frame to which the side data should be added
 * @param type type of the added side data
 * @param size size of the side data
 *
 * @return newly added side data on success, NULL on error
 *)
function av_frame_new_side_data(frame: PAVFrame;
                                          type_: TAVFrameSideDataType;
                                          size: cint): PAVFrameSideData;
  cdecl; external av__codec;

(**
 * @return a pointer to the side data of a given type on success, NULL if there
 * is no side data with such type in this frame.
 *)
function av_frame_get_side_data(frame: {const} PAVFrame; type_: TAVFrameSideDataType): PAVFrameSideData;
  cdecl; external av__codec;

(**
 * If side data of the supplied type exists in the frame, free it and remove it
 * from the frame.
 *)
procedure av_frame_remove_side_data(frame: PAVFrame; type_: TAVFrameSideDataType);
  cdecl; external av__codec;

(**
 * @return a string identifying the side data type
 *)
function av_frame_side_data_name(type_: TAVFrameSideDataType): PAnsiChar;
  cdecl; external av__codec;

