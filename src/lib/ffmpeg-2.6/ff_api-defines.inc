// prevent pasdoc from parsing this file
{$IFNDEF PASDOC}

(**
 * FF_API_* defines may be placed below to indicate public API that will be
 * dropped at a future version bump. The defines themselves are not part of
 * the public API and may change, break or disappear at any time.
 *)

(** avcodec defines *)
{$ifndef FF_API_REQUEST_CHANNELS}
{$define FF_API_REQUEST_CHANNELS := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_OLD_DECODE_AUDIO}
{$define FF_API_OLD_DECODE_AUDIO := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_OLD_ENCODE_AUDIO}
{$define FF_API_OLD_ENCODE_AUDIO := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_OLD_ENCODE_VIDEO}
{$define FF_API_OLD_ENCODE_VIDEO := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_CODEC_ID}
{$define FF_API_CODEC_ID         := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_AUDIO_CONVERT}
{$define FF_API_AUDIO_CONVERT    := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_AVCODEC_RESAMPLE}
{$define FF_API_AVCODEC_RESAMPLE := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_DEINTERLACE}
{$define FF_API_DEINTERLACE      := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_DESTRUCT_PACKET}
{$define FF_API_DESTRUCT_PACKET  := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_GET_BUFFER}
{$define FF_API_GET_BUFFER       := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_MISSING_SAMPLE}
{$define FF_API_MISSING_SAMPLE   := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_LOWRES}
{$define FF_API_LOWRES           := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_CAP_VDPAU}
{$define FF_API_CAP_VDPAU        := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_BUFS_VDPAU}
{$define FF_API_BUFS_VDPAU       := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_VOXWARE}
{$define FF_API_VOXWARE          := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_SET_DIMENSIONS}
{$define FF_API_SET_DIMENSIONS   := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_DEBUG_MV}
{$define FF_API_DEBUG_MV         := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_AC_VLC}
{$define FF_API_AC_VLC           := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_OLD_MSMPEG4}
{$define FF_API_OLD_MSMPEG4      := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_ASPECT_EXTENDED}
{$define FF_API_ASPECT_EXTENDED  := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_THREAD_OPAQUE}
{$define FF_API_THREAD_OPAQUE    := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_CODEC_PKT}
{$define FF_API_CODEC_PKT        := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_ARCH_ALPHA}
{$define FF_API_ARCH_ALPHA       := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_XVMC}
{$define FF_API_XVMC             := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_ERROR_RATE}
{$define FF_API_ERROR_RATE       := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_QSCALE_TYPE}
{$define FF_API_QSCALE_TYPE      := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_MB_TYPE}
{$define FF_API_MB_TYPE          := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_MAX_BFRAMES}
{$define FF_API_MAX_BFRAMES      := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_NEG_LINESIZES}
{$define FF_API_NEG_LINESIZES    := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_EMU_EDGE}
{$define FF_API_EMU_EDGE         := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_ARCH_SH4}
{$define FF_API_ARCH_SH4         := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_ARCH_SPARC}
{$define FF_API_ARCH_SPARC       := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_UNUSED_MEMBERS}
{$define FF_API_UNUSED_MEMBERS   := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_IDCT_XVIDMMX}
{$define FF_API_IDCT_XVIDMMX     := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_INPUT_PRESERVED}
{$define FF_API_INPUT_PRESERVED  := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_NORMALIZE_AQP}
{$define FF_API_NORMALIZE_AQP    := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_GMC}
{$define FF_API_GMC              := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_MV0}
{$define FF_API_MV0              := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_CODEC_NAME}
{$define FF_API_CODEC_NAME       := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_AFD}
{$define FF_API_AFD              := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_VISMV}
(* XXX: don't forget to drop the -vismv documentation *)
{$define FF_API_VISMV            := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_DV_FRAME_PROFILE}
{$define FF_API_DV_FRAME_PROFILE := (LIBAVCODEC_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_AUDIOENC_DELAY}
{$define FF_API_AUDIOENC_DELAY   := (LIBAVCODEC_VERSION_MAJOR < 58)}
{$endif}
{$ifndef FF_API_AVCTX_TIMEBASE}
{$define FF_API_AVCTX_TIMEBASE   := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}
{$ifndef FF_API_MPV_OPT}
{$define FF_API_MPV_OPT          := (LIBAVCODEC_VERSION_MAJOR < 59)}
{$endif}

(* avutil defines *)
{$ifndef FF_API_GET_BITS_PER_SAMPLE_FMT}
{$define FF_API_GET_BITS_PER_SAMPLE_FMT := (LIBAVUTIL_VERSION_MAJOR < 54)}
{$endif}
{$ifndef FF_API_FIND_OPT}
{$define FF_API_FIND_OPT                := (LIBAVUTIL_VERSION_MAJOR < 54)}
{$endif}
{$ifndef FF_API_OLD_AVOPTIONS}
{$define FF_API_OLD_AVOPTIONS           := (LIBAVUTIL_VERSION_MAJOR < 55)}
{$endif}
{$ifndef FF_API_PIX_FMT}
{$define FF_API_PIX_FMT                 := (LIBAVUTIL_VERSION_MAJOR < 55)}
{$endif}
{$ifndef FF_API_CONTEXT_SIZE}
{$define FF_API_CONTEXT_SIZE            := (LIBAVUTIL_VERSION_MAJOR < 55)}
{$endif}
{$ifndef FF_API_PIX_FMT_DESC}
{$define FF_API_PIX_FMT_DESC            := (LIBAVUTIL_VERSION_MAJOR < 55)}
{$endif}
{$ifndef FF_API_AV_REVERSE}
{$define FF_API_AV_REVERSE              := (LIBAVUTIL_VERSION_MAJOR < 55)}
{$endif}
{$ifndef FF_API_AUDIOCONVERT}
{$define FF_API_AUDIOCONVERT            := (LIBAVUTIL_VERSION_MAJOR < 55)}
{$endif}
{$ifndef FF_API_CPU_FLAG_MMX2}
{$define FF_API_CPU_FLAG_MMX2           := (LIBAVUTIL_VERSION_MAJOR < 55)}
{$endif}
{$ifndef FF_API_SAMPLES_UTILS_RETURN_ZERO}
{$define FF_API_SAMPLES_UTILS_RETURN_ZERO := (LIBAVUTIL_VERSION_MAJOR < 54)}
{$endif}
{$ifndef FF_API_LLS_PRIVATE}
{$define FF_API_LLS_PRIVATE              := (LIBAVUTIL_VERSION_MAJOR < 55)}
{$endif}
{$ifndef FF_API_LLS1}
{$define FF_API_LLS1                     :=(LIBAVUTIL_VERSION_MAJOR < 54)}
{$endif}
{$ifndef FF_API_AVFRAME_LAVC}
{$define FF_API_AVFRAME_LAVC             := (LIBAVUTIL_VERSION_MAJOR < 55)}
{$endif}
{$ifndef FF_API_VDPAU}
{$define FF_API_VDPAU                    := (LIBAVUTIL_VERSION_MAJOR < 55)}
{$endif}
{$ifndef FF_API_GET_CHANNEL_LAYOUT_COMPAT}
{$define FF_API_GET_CHANNEL_LAYOUT_COMPAT := (LIBAVUTIL_VERSION_MAJOR < 55)}
{$endif}
{$ifndef FF_API_OLD_OPENCL}
{$define FF_API_OLD_OPENCL               := (LIBAVUTIL_VERSION_MAJOR < 54)}
{$endif}
{$ifndef FF_API_XVMC}
{$define FF_API_XVMC                     := (LIBAVUTIL_VERSION_MAJOR < 55)}
{$endif}
{$ifndef FF_API_INTFLOAT
{$define FF_API_INTFLOAT                 := (LIBAVUTIL_VERSION_MAJOR < 54)}
{$endif}
{$ifndef FF_API_OPT_TYPE_METADATA}
{$define FF_API_OPT_TYPE_METADATA        := (LIBAVUTIL_VERSION_MAJOR < 55)}
{$endif}
{$ifndef FF_CONST_AVUTIL55}
{$IFDEF LIBAVUTIL_VERSION_MAJOR >= 55}
  {$define FF_CONST_AVUTIL55 		:= const}
{$ELSE}
  {$define FF_CONST_AVUTIL55}
{$ENDIF}
{$endif}

(** avformat FF_API defines *)
{$ifndef FF_API_LAVF_BITEXACT}
{$define FF_API_LAVF_BITEXACT         := (LIBAVFORMAT_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_LAVF_FRAC}
{$define FF_API_LAVF_FRAC             := (LIBAVFORMAT_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_LAVF_CODEC_TB}
{$define FF_API_LAVF_CODEC_TB         := (LIBAVFORMAT_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_URL_FEOF}
{$define FF_API_URL_FEOF              := (LIBAVFORMAT_VERSION_MAJOR < 57)}
{$endif}
{$ifndef FF_API_ALLOC_OUTPUT_CONTEXT}
{$define FF_API_ALLOC_OUTPUT_CONTEXT  := (LIBAVFORMAT_VERSION_MAJOR < 56)}
{$endif}
{$ifndef FF_API_FORMAT_PARAMETERS}
{$define FF_API_FORMAT_PARAMETERS     := (LIBAVFORMAT_VERSION_MAJOR < 56)}
{$endif}
{$ifndef FF_API_NEW_STREAM}
{$define FF_API_NEW_STREAM            := (LIBAVFORMAT_VERSION_MAJOR < 56)}
{$endif}
{$ifndef FF_API_SET_PTS_INFO}
{$define FF_API_SET_PTS_INFO          := (LIBAVFORMAT_VERSION_MAJOR < 56)}
{$endif}
{$ifndef FF_API_CLOSE_INPUT_FILE}
{$define FF_API_CLOSE_INPUT_FILE      := (LIBAVFORMAT_VERSION_MAJOR < 56)}
{$endif}
{$ifndef FF_API_READ_PACKET}
{$define FF_API_READ_PACKET           := (LIBAVFORMAT_VERSION_MAJOR < 56)}
{$endif}
{$ifndef FF_API_ASS_SSA}
{$define FF_API_ASS_SSA               := (LIBAVFORMAT_VERSION_MAJOR < 56)}
{$endif}
{$ifndef FF_API_R_FRAME_RATE}
{$define FF_API_R_FRAME_RATE          := 1}
{$endif}

(** swscale FF_API defines *)
{$ifndef FF_API_SWS_CPU_CAPS}
{$define FF_API_SWS_CPU_CAPS          := (LIBSWSCALE_VERSION_MAJOR < 4)}
{$endif}
{$ifndef FF_API_SWS_FORMAT_NAME}
{$define FF_API_SWS_FORMAT_NAME       := (LIBSWSCALE_VERSION_MAJOR < 3)}
{$endif}
{$ifndef FF_API_ARCH_BFIN}
{$define FF_API_ARCH_BFIN             := (LIBSWSCALE_VERSION_MAJOR < 4)}
{$endif}

{$ENDIF PASDOC}