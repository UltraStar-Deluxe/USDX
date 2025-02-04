unit avcodec;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

{$IFDEF DARWIN}
  {$linklib libavcodec}
{$ENDIF}

interface

uses
  ctypes,
  avutil,
  rational,
  SysUtils,
  UConfig;

const
  (* Supported version by this header *)
  LIBAVCODEC_MAX_VERSION_MAJOR   = 58;
  LIBAVCODEC_MAX_VERSION_MINOR   = 134;
  LIBAVCODEC_MAX_VERSION_RELEASE = 100;
  LIBAVCODEC_MAX_VERSION = (LIBAVCODEC_MAX_VERSION_MAJOR * VERSION_MAJOR) +
                           (LIBAVCODEC_MAX_VERSION_MINOR * VERSION_MINOR) +
                           (LIBAVCODEC_MAX_VERSION_RELEASE * VERSION_RELEASE);

  (* Min. supported version by this header *)
  LIBAVCODEC_MIN_VERSION_MAJOR   = 58;
  LIBAVCODEC_MIN_VERSION_MINOR   = 18;
  LIBAVCODEC_MIN_VERSION_RELEASE = 100;
  LIBAVCODEC_MIN_VERSION = (LIBAVCODEC_MIN_VERSION_MAJOR * VERSION_MAJOR) +
                            (LIBAVCODEC_MIN_VERSION_MINOR * VERSION_MINOR) +
                            (LIBAVCODEC_MIN_VERSION_RELEASE * VERSION_RELEASE);

(* Check if linked versions are supported *)
{$IF (LIBAVCODEC_VERSION < LIBAVCODEC_MIN_VERSION)}
  {$MESSAGE Error 'Linked version of libavcodec is too old!'}
{$IFEND}

(* Check if linked version is supported *)
{$IF (LIBAVCODEC_VERSION > LIBAVCODEC_MAX_VERSION)}
  {$MESSAGE Error 'Linked version of libavcodec is not yet supported!'}
{$IFEND}

const
  FF_BUG_AUTODETECT = 1;
  AV_PKT_DATA_SKIP_SAMPLES = 11;
type
  TAVCodecID = (
    AV_CODEC_ID_NONE
  );
  PAVPicture = ^TAVPicture;
  TAVPicture = record
    do_not_instantiate_this_record: incomplete_record;
  end;
  TAVPacketSideDataType = cenum;
  PAVPacket = ^TAVPacket;
  PPAVPacket = ^PAVPacket;
  TAVPacket = record
    we_do_not_use_buf: pointer;
    pts: cint64;
    we_do_not_use_dts: cint64;
    data: PByteArray;
    size: cint;
    stream_index: cint;
    flags: cint;
    side_data: pointer;
    side_data_elems: cint;
    we_do_not_use_duration: cint64;
    we_do_not_use_pos: cint64;
    we_do_not_use_convergence_duration: cint64;
  end;
  PAVCodecDescriptor = ^TAVCodecDescriptor;
  TAVCodecDescriptor = record
    we_do_not_use_id: TAVCodecID;
    we_do_not_use_type: TAVMediaType;
    name: ^AnsiChar;
    long_name: ^AnsiChar;
    do_not_instantiate_this_record: incomplete_record;
  end;
  PAVCodecParameters = ^TAVCodecParameters;
  TAVCodecParameters = record
    codec_type: TAVMediaType;
    codec_id: TAVCodecID;
    do_not_instantiate_this_record: incomplete_record;
  end;
  PAVCodecContext = ^TAVCodecContext;
  PPAVCodecContext = ^PAVCodecContext;
  PAVCodec = ^TAVCodec;
  TAVCodec = record
    name: ^AnsiChar;
    we_do_not_use_long_name: ^AnsiChar;
    type_: TAVMediaType;
    id: TAVCodecID;
    we_do_not_use_capabilities: cint;
    we_do_not_use_supported_framerates: ^TAVRational;
    pix_fmts: ^TAVPixelFormat;
    we_do_not_use_supported_samplerates: pcint;
    we_do_not_use_sample_fmts: ^TAVSampleFormat;
    we_do_not_use_channel_layouts: pcuint64;
    we_do_not_use_max_lowres: cuint8;
    we_do_not_use_priv_class: pointer;
    we_do_not_use_profiles: pointer;
    we_do_not_use_wrapper_name: ^AnsiChar;
    we_do_not_use_priv_data_size: cint;
    we_do_not_use_next: PAVCodec;
{$IF LIBAVCODEC_VERSION < 58079100}
    we_do_not_use_init_thread_copy: cfunctionpointer;
{$ENDIF}
    we_do_not_use_update_thread_context: cfunctionpointer;
    we_do_not_use_defaults: pointer;
    we_do_not_use_init_static_data: cfunctionpointer;
    we_do_not_use_init: cfunctionpointer;
    we_do_not_use_encode_sub: cfunctionpointer;
    we_do_not_use_encode2: cfunctionpointer;
    we_do_not_use_decode: cfunctionpointer;
    we_do_not_use_close: cfunctionpointer;
{$IF LIBAVCODEC_VERSION < 58093100}
    we_do_not_use_send_frame: cfunctionpointer;
{$ENDIF}
    we_do_not_use_receive_packet: cfunctionpointer;
    we_do_not_use_receive_frame: cfunctionpointer;
    flush: procedure(avctx: PAVCodecContext); cdecl;
    do_not_instantiate_this_record: incomplete_record;
  end;
  TAVCodecContext = record
    we_do_not_use_av_class: pointer;
    we_do_not_use_log_level_offset: cint;
    codec_type: TAVMediaType;
    codec: ^TAVCodec;
    codec_id: TAVCodecID;
    we_do_not_use_codec_tag: cuint;
    we_do_not_use_priv_data: pointer;
    we_do_not_use_internal: pointer;
    we_do_not_use_opaque: pointer;
    we_do_not_use_bit_rate: cint64;
    we_do_not_use_bit_rate_tolerance: cint;
    we_do_not_use_global_quality: cint;
    we_do_not_use_compression_level: cint;
    we_do_not_use_flags: cint;
    we_do_not_use_flags2: cint;
    we_do_not_use_extradata: pcuint8;
    we_do_not_use_extradata_size: cint;
    time_base: TAVRational;
    we_do_not_use_ticks_per_frame: cint;
    we_do_not_use_delay: cint;
    width: cint;
    height: cint;
    we_do_not_use_coded_width: cint;
    we_do_not_use_coded_height: cint;
    we_do_not_use_gop_size: cint;
    pix_fmt: TAVPixelFormat;
    we_do_not_use_draw_horiz_band: cfunctionpointer;
    get_format: function(s: PAVCodecContext; fmt: PAVPixelFormat): TAVPixelFormat; cdecl;
    we_do_not_use_max_b_frames: cint;
    we_do_not_use_b_quant_factor: cfloat;
    we_do_not_use_b_frame_strategy: cint;
    we_do_not_use_b_quant_offset: cfloat;
    we_do_not_use_has_b_frames: cint;
    we_do_not_use_mpeg_quant: cint;
    we_do_not_use_i_quant_factor: cfloat;
    we_do_not_use_i_quant_offset: cfloat;
    we_do_not_use_lumi_masking: cfloat;
    we_do_not_use_temporal_cplx_masking: cfloat;
    we_do_not_use_spatial_cplx_masking: cfloat;
    we_do_not_use_p_masking: cfloat;
    we_do_not_use_dark_masking: cfloat;
    we_do_not_use_slice_count: cint;
    we_do_not_use_prediction_method: cint;
    we_do_not_use_slice_offset: pcint;
    sample_aspect_ratio: TAVRational;
    we_do_not_use_me_cmp: cint;
    we_do_not_use_me_sub_cmp: cint;
    we_do_not_use_mb_cmp: cint;
    we_do_not_use_ildct_cmp: cint;
    we_do_not_use_dia_size: cint;
    we_do_not_use_last_predictor_count: cint;
    we_do_not_use_pre_me: cint;
    we_do_not_use_me_pre_cmp: cint;
    we_do_not_use_pre_dia_size: cint;
    we_do_not_use_me_subpel_quality: cint;
    we_do_not_use_me_range: cint;
    we_do_not_use_slice_flags: cint;
    we_do_not_use_mb_decision: cint;
    we_do_not_use_intra_matrix: pcuint16;
    we_do_not_use_inter_matrix: pcuint16;
    we_do_not_use_scenechange_threshold: cint;
    we_do_not_use_noise_reduction: cint;
    we_do_not_use_intra_dc_precision: cint;
    we_do_not_use_skip_top: cint;
    we_do_not_use_skip_bottom: cint;
    we_do_not_use_mb_lmin: cint;
    we_do_not_use_mb_lmax: cint;
    we_do_not_use_me_penalty_compensation: cint;
    we_do_not_use_bidir_refine: cint;
    we_do_not_use_brd_scale: cint;
    we_do_not_use_keyint_min: cint;
    we_do_not_use_refs: cint;
    we_do_not_use_chromaoffset: cint;
    we_do_not_use_mv0_threshold: cint;
    we_do_not_use_b_sensitivity: cint;
    we_do_not_use_color_primaries: cenum;
    we_do_not_use_color_trc: cenum;
    we_do_not_use_colorspace: cenum;
    we_do_not_use_color_range: cenum;
    we_do_not_use_chroma_sample_location: cenum;
    we_do_not_use_slices: cint;
    we_do_not_use_field_order: cenum;
    sample_rate: cint;
    channels: cint;
    sample_fmt: TAVSampleFormat;
    we_do_not_use_frame_size: cint;
    we_do_not_use_frame_number: cint;
    we_do_not_use_block_align: cint;
    we_do_not_use_cutoff: cint;
    channel_layout: cuint64;
    request_channel_layout: cuint64;
    we_do_not_use_audio_service_type: cenum;
    request_sample_fmt: TAVSampleFormat;
    we_do_not_use_get_buffer2: cfunctionpointer;
    we_do_not_use_refcounted_frames: cint;
    we_do_not_use_qcompress: cfloat;
    we_do_not_use_qblur: cfloat;
    we_do_not_use_qmin: cint;
    we_do_not_use_qmax: cint;
    we_do_not_use_max_qdiff: cint;
    we_do_not_use_rc_buffer_size: cint;
    we_do_not_use_rc_override_count: cint;
    we_do_not_use_rc_override: pointer;
    we_do_not_use_rc_max_rate: cint64;
    we_do_not_use_rc_min_rate: cint64;
    we_do_not_use_rc_max_available_vbv_use: cfloat;
    we_do_not_use_rc_min_vbv_overflow_use: cfloat;
    we_do_not_use_rc_initial_buffer_occupancy: cint;
    we_do_not_use_coder_type: cint;
    we_do_not_use_context_model: cint;
    we_do_not_use_frame_skip_threshold: cint;
    we_do_not_use_frame_skip_factor: cint;
    we_do_not_use_frame_skip_exp: cint;
    we_do_not_use_frame_skip_cmp: cint;
    we_do_not_use_trellis: cint;
    we_do_not_use_min_prediction_order: cint;
    we_do_not_use_max_prediction_order: cint;
    we_do_not_use_timecode_frame_start: cint64;
    we_do_not_use_rtp_callback: cfunctionpointer;
    we_do_not_use_rtp_payload_size: cint;
    we_do_not_use_mv_bits: cint;
    we_do_not_use_header_bits: cint;
    we_do_not_use_i_tex_bits: cint;
    we_do_not_use_p_tex_bits: cint;
    we_do_not_use_i_count: cint;
    we_do_not_use_p_count: cint;
    we_do_not_use_skip_count: cint;
    we_do_not_use_misc_bits: cint;
    we_do_not_use_frame_bits: cint;
    we_do_not_use_stats_out: pcchar;
    we_do_not_use_stats_in: pcchar;
    workaround_bugs: cint;
    we_do_not_use_strict_std_compliance: cint;
    we_do_not_use_error_concealment: cint;
    debug: cint;
    we_do_not_use_err_recognition: cint;
    we_do_not_use_reordered_opaque: cint64;
    we_do_not_use_hwaccel: pointer;
    we_do_not_use_hwaccel_context: pointer;
    we_do_not_use_error: array [0..AV_NUM_DATA_POINTERS-1] of cuint64;
    we_do_not_use_dct_algo: cint;
    we_do_not_use_idct_algo: cint;
    we_do_not_use_bits_per_coded_sample: cint;
    we_do_not_use_bits_per_raw_sample: cint;
    we_do_not_use_lowres: cint;
    we_do_not_use_coded_frame: PAVFrame;
    thread_count: cint;
    we_do_not_use_thread_type: cint;
    we_do_not_use_active_thread_type: cint;
    we_do_not_use_thread_safe_callbacks: cint;
    we_do_not_use_execute: cfunctionpointer;
    we_do_not_use_execute2: cfunctionpointer; 
    we_do_not_use_nsse_weight: cint;
    we_do_not_use_profile: cint;
    we_do_not_use_level: cint;
    we_do_not_use_skip_loop_filter: cenum;
    we_do_not_use_skip_idct: cenum;
    we_do_not_use_skip_frame: cenum;
    we_do_not_use_subtitle_header: pcuint8;
    we_do_not_use_subtitle_header_size: cint;
    we_do_not_use_vbv_delay: cuint64;
    we_do_not_use_side_data_only_packets: cint;
    we_do_not_use_initial_padding: cint;
    framerate: TAVRational;
    we_do_not_use_sw_pix_fmt: TAVPixelFormat;
    we_do_not_use_pkt_timebase: TAVRational;
    we_do_not_use_codec_descriptor: PAVCodecDescriptor;
    we_do_not_use_pts_correction_num_faulty_pts: cint64;
    we_do_not_use_pts_correction_num_faulty_dts: cint64;
    we_do_not_use_pts_correction_last_pts: cint64;
    we_do_not_use_pts_correction_last_dts: cint64;
    we_do_not_use_sub_charenc: pcchar;
    we_do_not_use_sub_charenc_mode: cint;
    we_do_not_use_skip_alpha: cint;
    we_do_not_use_seek_preroll: cint;
    debug_mv: cint;
    do_not_instantiate_this_record: incomplete_record;
  end;
function av_dup_packet(pkt: PAVPacket): cint; cdecl; external av__codec; deprecated;
procedure av_free_packet(pkt: PAVPacket); cdecl; external av__codec; deprecated;
function avcodec_version(): cuint; cdecl; external av__codec;
function av_codec_is_decoder(codec: PAVCodec): cint; cdecl; external av__codec;
function av_codec_iterate(opaque: ppointer): PAVCodec; cdecl; external av__codec;
function avcodec_find_decoder(id: TAVCodecID): PAVCodec; cdecl; external av__codec;
function avcodec_find_decoder_by_name(name: PAnsiChar): PAVCodec; cdecl; external av__codec;
function avcodec_descriptor_get(id: TAVCodecID): PAVCodecDescriptor; cdecl; external av__codec;
function avcodec_open2(avctx: PAVCodecContext; codec: PAVCodec; options: PPAVDictionary): cint; cdecl; external av__codec;
function avcodec_close(avctx: PAVCodecContext): cint; cdecl; external av__codec;
function avcodec_decode_video2(avctx: PAVCodecContext; picture: PAVFrame; var got_picture: cint; avpkt: PAVPacket): cint; cdecl; external av__codec; deprecated;
procedure avcodec_flush_buffers(avctx: PAVCodecContext); cdecl; external av__codec;
function avpicture_fill(picture: PAVPicture; ptr: pcuint8; pix_fmt: TAVPixelFormat; width: cint; height: cint): cint; cdecl; external av__codec; deprecated;
function avpicture_get_size(pix_fmt: TAVPixelFormat; width: cint; height: cint): cint; cdecl; external av__codec; deprecated;
function avcodec_receive_frame(avctx: PAVCodecContext; frame: PAVFrame): cint; cdecl; external av__codec;
function avcodec_send_packet(avctx: PAVCodecContext; avpkt: PAVPacket): cint; cdecl; external av__codec;
function avcodec_alloc_context3(codec: PAVCodec): PAVCodecContext; cdecl; external av__codec;
procedure avcodec_free_context(avctx: PPAVCodecContext); cdecl; external av__codec;
function avcodec_parameters_to_context(codec: PAVCodecContext; par: PAVCodecParameters): cint; cdecl; external av__codec;
function av_packet_alloc(): PAVPacket; cdecl; external av__codec;
procedure av_packet_free(pkt: PPAVPacket); cdecl; external av__codec;
procedure av_packet_unref(pkt: PAVPacket); cdecl; external av__codec;
function av_packet_new_side_data(pkt: PAVPacket; type_data: TAVPacketSideDataType; size: csize_t): pcuint8; cdecl; external av__codec;
function av_packet_get_side_data(const pkt: PAVPacket; type_data: TAVPacketSideDataType; size: pcsize_t): pcuint8; cdecl; external av__codec;
implementation
end.
