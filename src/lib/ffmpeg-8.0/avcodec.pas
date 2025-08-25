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
  FF_BUG_AUTODETECT = 1;
  AV_PKT_DATA_SKIP_SAMPLES = 11;
type
  TAVCodecID = (
    AV_CODEC_ID_NONE,
    AV_CODEC_ID_OPUS = $1503c
  );
  TAVCodecConfig = (
    AV_CODEC_CONFIG_PIX_FORMAT
  );
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
    we_do_not_use_side_data: pointer;
    side_data_elems: cint;
    we_do_not_use_duration: cint64;
    we_do_not_use_pos: cint64;
    opaque: pointer;
    we_do_not_use_opaque_ref: pointer;
    we_do_not_use_time_base: TAVRational;
  (* According to the FFmpeg documentation, sizeof(AVPacket) is
   * deprecated for the public ABI. However, TAVPacket is still a member of
   * TAVStream. So we can't put the incomplete record member because that will
   * change the memory layout of TAVStream *)
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
    we_do_not_use_flags: cint;
    we_do_not_use_flags2: cint;
    we_do_not_use_extradata: pcuint8;
    we_do_not_use_extradata_size: cint;
    time_base: TAVRational;
    we_do_not_use_pkt_timebase: TAVRational;
    framerate: TAVRational;
    we_do_not_use_delay: cint;
    width: cint;
    height: cint;
    we_do_not_use_coded_width: cint;
    we_do_not_use_coded_height: cint;
    sample_aspect_ratio: TAVRational;
    pix_fmt: TAVPixelFormat;
    we_do_not_use_sw_pix_fmt: TAVPixelFormat;
    we_do_not_use_color_primaries: cenum;
    we_do_not_use_color_trc: cenum;
    we_do_not_use_colorspace: cenum;
    we_do_not_use_color_range: cenum;
    we_do_not_use_chroma_sample_location: cenum;
    we_do_not_use_field_order: cenum;
    we_do_not_use_refs: cint;
    we_do_not_use_has_b_frames: cint;
    we_do_not_use_slice_flags: cint;
    we_do_not_use_draw_horiz_band: cfunctionpointer;
    get_format: function(s: PAVCodecContext; fmt: PAVPixelFormat): TAVPixelFormat; cdecl;
    we_do_not_use_max_b_frames: cint;
    we_do_not_use_b_quant_factor: cfloat;
    we_do_not_use_b_quant_offset: cfloat;
    we_do_not_use_i_quant_factor: cfloat;
    we_do_not_use_i_quant_offset: cfloat;
    we_do_not_use_lumi_masking: cfloat;
    we_do_not_use_temporal_cplx_masking: cfloat;
    we_do_not_use_spatial_cplx_masking: cfloat;
    we_do_not_use_p_masking: cfloat;
    we_do_not_use_dark_masking: cfloat;
    we_do_not_use_nsse_weight: cint;
    we_do_not_use_me_cmp: cint;
    we_do_not_use_me_sub_cmp: cint;
    we_do_not_use_mb_cmp: cint;
    we_do_not_use_ildct_cmp: cint;
    we_do_not_use_dia_size: cint;
    we_do_not_use_last_predictor_count: cint;
    we_do_not_use_me_pre_cmp: cint;
    we_do_not_use_pre_dia_size: cint;
    we_do_not_use_me_subpel_quality: cint;
    we_do_not_use_me_range: cint;
    we_do_not_use_mb_decision: cint;
    we_do_not_use_intra_matrix: pcuint16;
    we_do_not_use_inter_matrix: pcuint16;
    we_do_not_use_chroma_intra_matrix: pcuint16;
    we_do_not_use_intra_dc_precision: cint;
    we_do_not_use_mb_lmin: cint;
    we_do_not_use_mb_lmax: cint;
    we_do_not_use_bidir_refine: cint;
    we_do_not_use_keyint_min: cint;
    we_do_not_use_gop_size: cint;
    we_do_not_use_mv0_threshold: cint;
    we_do_not_use_slices: cint;
    sample_rate: cint;
    sample_fmt: TAVSampleFormat;
    ch_layout: TAVChannelLayout;
    we_do_not_use_frame_size: cint;
    we_do_not_use_block_align: cint;
    we_do_not_use_cutoff: cint;
    we_do_not_use_audio_service_type: cenum;
    request_sample_fmt: TAVSampleFormat;
    we_do_not_use_initial_padding: cint;
    we_do_not_use_trailing_padding: cint;
    we_do_not_use_seek_preroll: cint;
    we_do_not_use_get_buffer2: cfunctionpointer;
    we_do_not_use_bit_rate_tolerance: cint;
    we_do_not_use_global_quality: cint;
    we_do_not_use_compression_level: cint;
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
    we_do_not_use_trellis: cint;
    we_do_not_use_stats_out: pcchar;
    we_do_not_use_stats_in: pcchar;
    workaround_bugs: cint;
    we_do_not_use_strict_std_compliance: cint;
    we_do_not_use_error_concealment: cint;
    debug: cint;
    we_do_not_use_err_recognition: cint;
    we_do_not_use_hwaccel: pointer;
    we_do_not_use_hwaccel_context: pointer;
    we_do_not_use_hw_frames_ctx: pointer;
    we_do_not_use_hw_device_ctx: pointer;
    we_do_not_use_hwaccel_flags: cint;
    we_do_not_use_extra_hw_frames: cint;
    we_do_not_use_error: array [0..AV_NUM_DATA_POINTERS-1] of cuint64;
    we_do_not_use_dct_algo: cint;
    we_do_not_use_idct_algo: cint;
    we_do_not_use_bits_per_coded_sample: cint;
    we_do_not_use_bits_per_raw_sample: cint;
    thread_count: cint;
    do_not_instantiate_this_record: incomplete_record;
  end;
function av_packet_ref(dst: PAVPacket; src: PAVPacket): cint; cdecl; external av__codec;
procedure av_packet_unref(pkt: PAVPacket); cdecl; external av__codec;
function avcodec_version(): cuint; cdecl; external av__codec;
function av_codec_is_decoder(codec: PAVCodec): cint; cdecl; external av__codec;
function av_codec_iterate(opaque: ppointer): PAVCodec; cdecl; external av__codec;
function avcodec_find_decoder(id: TAVCodecID): PAVCodec; cdecl; external av__codec;
function avcodec_find_decoder_by_name(name: PAnsiChar): PAVCodec; cdecl; external av__codec;
function avcodec_descriptor_get(id: TAVCodecID): PAVCodecDescriptor; cdecl; external av__codec;
function avcodec_open2(avctx: PAVCodecContext; codec: PAVCodec; options: PPAVDictionary): cint; cdecl; external av__codec;
procedure avcodec_flush_buffers(avctx: PAVCodecContext); cdecl; external av__codec;
function avcodec_receive_frame(avctx: PAVCodecContext; frame: PAVFrame): cint; cdecl; external av__codec;
function avcodec_send_packet(avctx: PAVCodecContext; avpkt: PAVPacket): cint; cdecl; external av__codec;
function avcodec_alloc_context3(codec: PAVCodec): PAVCodecContext; cdecl; external av__codec;
procedure avcodec_free_context(avctx: PPAVCodecContext); cdecl; external av__codec;
function avcodec_parameters_to_context(codec: PAVCodecContext; par: PAVCodecParameters): cint; cdecl; external av__codec;
function av_packet_alloc(): PAVPacket; cdecl; external av__codec;
procedure av_packet_free(pkt: PPAVPacket); cdecl; external av__codec;
function av_packet_new_side_data(pkt: PAVPacket; type_data: TAVPacketSideDataType; size: csize_t): pcuint8; cdecl; external av__codec;
function av_packet_get_side_data(const pkt: PAVPacket; type_data: TAVPacketSideDataType; size: pcsize_t): pcuint8; cdecl; external av__codec;
function avcodec_get_supported_config(const avctx: PAVCodecContext; const Codec: PAVCodec; config: TAVCodecConfig; flags: cuint; out_configs: PPointer; out_num_configs: pcint): cint; cdecl; external av__codec;

implementation
end.
