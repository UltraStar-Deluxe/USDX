// prevent pasdoc from parsing this file
{$IFNDEF PASDOC}

(**
 * FF_API_* defines may be placed below to indicate public API that will be
 * dropped at a future version bump. The defines themselves are not part of
 * the public API and may change, break or disappear at any time.
 *)

(** avcodec defines *)
{$ifndef FF_API_VIMA_DECODER}
{$define FF_API_VIMA_DECODER     := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_AUDIO_CONVERT}
{$define FF_API_AUDIO_CONVERT    := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_AVCODEC_RESAMPLE}
{$define FF_API_AVCODEC_RESAMPLE := FF_API_AUDIO_CONVERT}
{$endif}
{$ifndef FF_API_MISSING_SAMPLE}
{$define FF_API_MISSING_SAMPLE   := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_LOWRES}
{$define FF_API_LOWRES           := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_CAP_VDPAU}
{$define FF_API_CAP_VDPAU        := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_BUFS_VDPAU}
{$define FF_API_BUFS_VDPAU       := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_VOXWARE}
{$define FF_API_VOXWARE          := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_SET_DIMENSIONS}
{$define FF_API_SET_DIMENSIONS   := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_DEBUG_MV}
{$define FF_API_DEBUG_MV         := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_AC_VLC}
{$define FF_API_AC_VLC           := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_OLD_MSMPEG4}
{$define FF_API_OLD_MSMPEG4      := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_ASPECT_EXTENDED}
{$define FF_API_ASPECT_EXTENDED  := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_ARCH_ALPHA}
{$define FF_API_ARCH_ALPHA       := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_XVMC}
{$define FF_API_XVMC             := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_ERROR_RATE}
{$define FF_API_ERROR_RATE       := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_QSCALE_TYPE}
{$define FF_API_QSCALE_TYPE      := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_MB_TYPE}
{$define FF_API_MB_TYPE          := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_MAX_BFRAMES}
{$define FF_API_MAX_BFRAMES      := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_NEG_LINESIZES}
{$define FF_API_NEG_LINESIZES    := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_EMU_EDGE}
{$define FF_API_EMU_EDGE         := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_ARCH_SH4}
{$define FF_API_ARCH_SH4         := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_ARCH_SPARC}
{$define FF_API_ARCH_SPARC       := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_UNUSED_MEMBERS}
{$define FF_API_UNUSED_MEMBERS   := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_IDCT_XVIDMMX}
{$define FF_API_IDCT_XVIDMMX     := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_INPUT_PRESERVED}
{$define FF_API_INPUT_PRESERVED  := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_NORMALIZE_AQP}
{$define FF_API_NORMALIZE_AQP    := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_GMC}
{$define FF_API_GMC              := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_MV0}
{$define FF_API_MV0              := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_CODEC_NAME}
{$define FF_API_CODEC_NAME       := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_AFD}
{$define FF_API_AFD              := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_VISMV}
(* XXX: don't forget to drop the -vismv documentation *)
{$define FF_API_VISMV            := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_AUDIOENC_DELAY}
{$define FF_API_AUDIOENC_DELAY   := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_VAAPI_CONTEXT}
{$define FF_API_VAAPI_CONTEXT    := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_MERGE_SD}
{$define FF_API_MERGE_SD         := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_AVCTX_TIMEBASE}
{$define FF_API_AVCTX_TIMEBASE   := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_MPV_OPT}
{$define FF_API_MPV_OPT          := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_STREAM_CODEC_TAG}
{$define FF_API_STREAM_CODEC_TAG := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_QUANT_BIAS}
{$define FF_API_QUANT_BIAS       := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_RC_STRATEGY}
{$define FF_API_RC_STRATEGY      := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_CODED_FRAME}
{$define FF_API_CODED_FRAME      := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_MOTION_EST}
{$define FF_API_MOTION_EST       := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_WITHOUT_PREFIX}
{$define FF_API_WITHOUT_PREFIX   := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_SIDEDATA_ONLY_PKT}
{$define FF_API_SIDEDATA_ONLY_PKT:= (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_VDPAU_PROFILE}
{$define FF_API_VDPAU_PROFILE    := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_CONVERGENCE_DURATION}
{$define FF_API_CONVERGENCE_DURATION := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_AVPICTURE}
{$define FF_API_AVPICTURE        := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_AVPACKET_OLD_API}
{$define FF_API_AVPACKET_OLD_API := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_RTP_CALLBACK}
{$define FF_API_RTP_CALLBACK     := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_VBV_DELAY}
{$define FF_API_VBV_DELAY        := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_CODER_TYPE}
{$define FF_API_CODER_TYPE       := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_STAT_BITS}
{$define FF_API_STAT_BITS        := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_PRIVATE_OPT}
{$define FF_API_PRIVATE_OPT      := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_ASS_TIMING}
{$define FF_API_ASS_TIMING       := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_OLD_BSF}
{$define FF_API_OLD_BSF          := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_COPY_CONTEXT}
{$define FF_API_COPY_CONTEXT     := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_GET_CONTEXT_DEFAULTS}
{$define FF_API_GET_CONTEXT_DEFAULTS := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_NVENC_OLD_NAME}
{$define FF_API_NVENC_OLD_NAME   := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_STRUCT_VAAPI_CONTEXT}
{$define FF_API_STRUCT_VAAPI_CONTEXT := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_MERGE_SD_API}
{$define FF_API_MERGE_SD_API     := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_TAG_STRING}
{$define FF_API_TAG_STRING       := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_GETCHROMA}
{$define FF_API_GETCHROMA        := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}

(* avutil defines *)
{$ifndef FF_API_VDPAU}
{$define FF_API_VDPAU                    := (LIBAVUTIL_VERSION_MAJOR < 56)}
{$endif}
{$ifndef FF_API_XVMC}
{$define FF_API_XVMC                     := (LIBAVUTIL_VERSION_MAJOR < 56)}
{$endif}
{$ifndef FF_API_OPT_TYPE_METADATA}
{$define FF_API_OPT_TYPE_METADATA        := (LIBAVUTIL_VERSION_MAJOR < 56)}
{$endif}
{$ifndef FF_API_DLOG}
{$define FF_API_DLOG                     := (LIBAVUTIL_VERSION_MAJOR < 56)}
{$endif}
{$ifndef FF_API_VAAPI}
{$define FF_API_VAAPI                    := (LIBAVUTIL_VERSION_MAJOR < 56)}
{$endif}
{$ifndef FF_API_FRAME_QP}
{$define FF_API_FRAME_QP                 := (LIBAVUTIL_VERSION_MAJOR < 56)}
{$endif}
{$ifndef FF_API_PLUS1_MINUS1}
{$define FF_API_PLUS1_MINUS1             := (LIBAVUTIL_VERSION_MAJOR < 56)}
{$endif}
{$ifndef FF_API_ERROR_FRAME}
{$define FF_API_ERROR_FRAME              := (LIBAVUTIL_VERSION_MAJOR < 56)}
{$endif}
{$ifndef FF_API_CRC_BIG_TABLE}
{$define FF_API_CRC_BIG_TABLE            := (LIBAVUTIL_VERSION_MAJOR < 56)}
{$endif}
{$ifndef FF_API_PKT_PTS}
{$define FF_API_PKT_PTS                  := (LIBAVUTIL_VERSION_MAJOR < 56)}
{$endif}
{$ifndef FF_API_CRYPTO_SIZE_T}
{$define FF_API_CRYPTO_SIZE_T            := (LIBAVUTIL_VERSION_MAJOR < 56)}
{$endif}

(** avformat FF_API defines *)
{$ifndef FF_API_LAVF_BITEXACT}
{$define FF_API_LAVF_BITEXACT         := (LIBAVFORMAT_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_LAVF_FRAC}
{$define FF_API_LAVF_FRAC             := (LIBAVFORMAT_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_LAVF_CODEC_TB}
{$define FF_API_LAVF_CODEC_TB         := (LIBAVFORMAT_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_URL_FEOF}
{$define FF_API_URL_FEOF              := (LIBAVFORMAT_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_LAVF_FMT_RAWPICTURE}
{$define FF_API_LAVF_FMT_RAWPICTURE   := (LIBAVFORMAT_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_COMPUTE_PKT_FIELDS2}
{$define FF_API_COMPUTE_PKT_FIELDS2   := (LIBAVFORMAT_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_OLD_OPEN_CALLBACKS}
{$define FF_API_OLD_OPEN_CALLBACKS    := (LIBAVFORMAT_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_LAVF_AVCTX}
{$define FF_API_LAVF_AVCTX            := (LIBAVFORMAT_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_NOCONST_GET_SIDE_DATA}
{$define FF_API_NOCONST_GET_SIDE_DATA := (LIBAVFORMAT_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_HTTP_USER_AGENT}
{$define FF_API_HTTP_USER_AGENT       := (LIBAVFORMAT_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_HLS_WRAP}
{$define FF_API_HLS_WRAP              := (LIBAVFORMAT_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_LAVF_MERGE_SD}
{$define FF_API_LAVF_MERGE_SD         := (LIBAVFORMAT_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_LAVF_KEEPSIDE_FLAG}
{$define FF_API_LAVF_KEEPSIDE_FLAG    := (LIBAVFORMAT_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_OLD_ROTATE_API}
{$define FF_API_OLD_ROTATE_API        := (LIBAVFORMAT_VERSION_MAJOR < 58)}
{$endif}

{$ifndef FF_API_R_FRAME_RATE}
{$define FF_API_R_FRAME_RATE          := 1}
{$endif}

(** swscale FF_API defines *)
{$ifndef FF_API_SWS_VECTOR}
{$define FF_API_SWS_VECTOR            := (LIBSWSCALE_VERSION_MAJOR < 6)}
{$endif}

{$ENDIF PASDOC}
