// prevent pasdoc from parsing this file
{$IFNDEF PASDOC}

(**
 * FF_API_* defines may be placed below to indicate public API that will be
 * dropped at a future version bump. The defines themselves are not part of
 * the public API and may change, break or disappear at any time.
 *)

(** avcodec defines *)
{$ifndef FF_API_LOWRES}
{$define FF_API_LOWRES           := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_DEBUG_MV}
{$define FF_API_DEBUG_MV         := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_AVCTX_TIMEBASE}
{$define FF_API_AVCTX_TIMEBASE   := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_CODED_FRAME}
{$define FF_API_CODED_FRAME      := (LIBAVCODEC_VERSION_MAJOR < 59)}
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
{$ifndef FF_API_CODEC_GET_SET}
{$define FF_API_CODEC_GET_SET    := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_USER_VISIBLE_AVHWACCEL}
{$define FF_API_USER_VISIBLE_AVHWACCEL := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_LOCKMGR}
{$define FF_API_LOCKMGR          := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_NEXT}
{$define FF_API_NEXT             := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}

(* avutil defines *)
{$ifndef FF_API_VAAPI}
{$define FF_API_VAAPI                    := (LIBAVUTIL_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_FRAME_QP}
{$define FF_API_FRAME_QP                 := (LIBAVUTIL_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_PLUS1_MINUS1}
{$define FF_API_PLUS1_MINUS1             := (LIBAVUTIL_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_ERROR_FRAME}
{$define FF_API_ERROR_FRAME              := (LIBAVUTIL_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_PKT_PTS}
{$define FF_API_PKT_PTS                  := (LIBAVUTIL_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_CRYPTO_SIZE_T}
{$define FF_API_CRYPTO_SIZE_T            := (LIBAVUTIL_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_FRAME_GET_SET}
{$define FF_API_FRAME_GET_SET            := (LIBAVUTIL_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_PSEUDOPAL}
{$define FF_API_PSEUDOPAL                := (LIBAVUTIL_VERSION_MAJOR < 57)}
{$endif}

(** avformat FF_API defines *)
{$ifndef FF_API_COMPUTE_PKT_FIELDS2}
{$define FF_API_COMPUTE_PKT_FIELDS2   := (LIBAVFORMAT_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_OLD_OPEN_CALLBACKS}
{$define FF_API_OLD_OPEN_CALLBACKS    := (LIBAVFORMAT_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_LAVF_AVCTX}
{$define FF_API_LAVF_AVCTX            := (LIBAVFORMAT_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_HTTP_USER_AGENT}
{$define FF_API_HTTP_USER_AGENT       := (LIBAVFORMAT_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_HLS_WRAP}
{$define FF_API_HLS_WRAP              := (LIBAVFORMAT_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_LAVF_KEEPSIDE_FLAG}
{$define FF_API_LAVF_KEEPSIDE_FLAG    := (LIBAVFORMAT_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_OLD_ROTATE_API}
{$define FF_API_OLD_ROTATE_API        := (LIBAVFORMAT_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_FORMAT_GET_SET}
{$define FF_API_FORMAT_GET_SET        := (LIBAVFORMAT_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_OLD_AVIO_EOF_0}
{$define FF_API_OLD_AVIO_EOF_0        := (LIBAVFORMAT_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_LAVF_FFSERVER}
{$define FF_API_LAVF_FFSERVER         := (LIBAVFORMAT_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_FORMAT_FILENAME}
{$define FF_API_FORMAT_FILENAME       := (LIBAVFORMAT_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_OLD_RTSP_OPTIONS}
{$define FF_API_OLD_RTSP_OPTIONS      := (LIBAVFORMAT_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_NEXT}
{$define FF_API_NEXT                  := (LIBAVFORMAT_VERSION_MAJOR < 59)}
{$endif}

{$ifndef FF_API_R_FRAME_RATE}
{$define FF_API_R_FRAME_RATE          := 1}
{$endif}

(** swscale FF_API defines *)
{$ifndef FF_API_SWS_VECTOR}
{$define FF_API_SWS_VECTOR            := (LIBSWSCALE_VERSION_MAJOR < 6)}
{$endif}

{$ENDIF PASDOC}
