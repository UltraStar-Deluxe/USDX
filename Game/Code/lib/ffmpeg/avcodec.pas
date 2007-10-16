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
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
                                                                              *)

(* This is a part of Pascal porting of ffmpeg.  Originally by Victor Zinetz for Delphi and Free Pascal on Windows.
For Mac OS X, some modifications were made by The Creative CAT, denoted as CAT
in the source codes *)

unit avcodec;

{$IFDEF FPC}
  {$IFNDEF win32}
  {$LINKLIB libavutil}
  {$LINKLIB libavcodec}
  {$ENDIF}
  {$MODE DELPHI } (* CAT *)
  {$PACKENUM 4}    (* every enum type variables uses 4 bytes, CAT *)
  {$PACKRECORDS C}    (* GCC compatible, Record Packing, CAT *)
{$ENDIF}

interface

uses
  avutil, rational, opt;  // CAT

const
{$IFDEF win32}
  av__format = 'avformat-50.dll';
{$ELSE}
  av__format = 'libavformat.so';   // .0d
//  av__format = 'libavformat.51';
{$ENDIF}


  LIBAVUTIL_VERSION_INT   =  ((51 shl 16) + (12 shl 8) + 1);
  LIBAVUTIL_VERSION       = '51.12.1';
  LIBAVUTIL_BUILD         = LIBAVUTIL_VERSION_INT;

  AV_NOPTS_VALUE: int64   = $8000000000000000;
  AV_TIME_BASE            = 1000000;
  AV_TIME_BASE_Q          : TAVRational = (num:1; den:AV_TIME_BASE); (* added by CAT *)

type
  TCodecID = (
    CODEC_ID_NONE, CODEC_ID_MPEG1VIDEO,
    CODEC_ID_MPEG2VIDEO, //* prefered ID for MPEG Video 1 or 2 decoding */
    CODEC_ID_MPEG2VIDEO_XVMC, CODEC_ID_H261, CODEC_ID_H263, CODEC_ID_RV10,
    CODEC_ID_RV20, CODEC_ID_MJPEG, CODEC_ID_MJPEGB, CODEC_ID_LJPEG,
    CODEC_ID_SP5X, CODEC_ID_JPEGLS, CODEC_ID_MPEG4, CODEC_ID_RAWVIDEO,
    CODEC_ID_MSMPEG4V1, CODEC_ID_MSMPEG4V2, CODEC_ID_MSMPEG4V3,
    CODEC_ID_WMV1, CODEC_ID_WMV2, CODEC_ID_H263P,
    CODEC_ID_H263I, CODEC_ID_FLV1, CODEC_ID_SVQ1, CODEC_ID_SVQ3,
    CODEC_ID_DVVIDEO, CODEC_ID_HUFFYUV, CODEC_ID_CYUV, CODEC_ID_H264,
    CODEC_ID_INDEO3, CODEC_ID_VP3, CODEC_ID_THEORA, CODEC_ID_ASV1,
    CODEC_ID_ASV2, CODEC_ID_FFV1, CODEC_ID_4XM, CODEC_ID_VCR1,
    CODEC_ID_CLJR, CODEC_ID_MDEC, CODEC_ID_ROQ, CODEC_ID_INTERPLAY_VIDEO,
    CODEC_ID_XAN_WC3, CODEC_ID_XAN_WC4, CODEC_ID_RPZA, CODEC_ID_CINEPAK,
    CODEC_ID_WS_VQA, CODEC_ID_MSRLE, CODEC_ID_MSVIDEO1, CODEC_ID_IDCIN,
    CODEC_ID_8BPS, CODEC_ID_SMC, CODEC_ID_FLIC, CODEC_ID_TRUEMOTION1,
    CODEC_ID_VMDVIDEO, CODEC_ID_MSZH, CODEC_ID_ZLIB, CODEC_ID_QTRLE,
    CODEC_ID_SNOW, CODEC_ID_TSCC, CODEC_ID_ULTI, CODEC_ID_QDRAW,
    CODEC_ID_VIXL, CODEC_ID_QPEG, CODEC_ID_XVID, CODEC_ID_PNG,
    CODEC_ID_PPM, CODEC_ID_PBM, CODEC_ID_PGM, CODEC_ID_PGMYUV,
    CODEC_ID_PAM, CODEC_ID_FFVHUFF, CODEC_ID_RV30, CODEC_ID_RV40,
    CODEC_ID_VC1, CODEC_ID_WMV3, CODEC_ID_LOCO, CODEC_ID_WNV1,
    CODEC_ID_AASC, CODEC_ID_INDEO2, CODEC_ID_FRAPS, CODEC_ID_TRUEMOTION2,
    CODEC_ID_BMP, CODEC_ID_CSCD, CODEC_ID_MMVIDEO, CODEC_ID_ZMBV,
    CODEC_ID_AVS, CODEC_ID_SMACKVIDEO, CODEC_ID_NUV, CODEC_ID_KMVC,
    CODEC_ID_FLASHSV, CODEC_ID_CAVS, CODEC_ID_JPEG2000, CODEC_ID_VMNC,
    CODEC_ID_VP5, CODEC_ID_VP6, CODEC_ID_VP6F,

    //* various pcm "codecs" */
    CODEC_ID_PCM_S16LE= $10000,  CODEC_ID_PCM_S16BE, CODEC_ID_PCM_U16LE,
    CODEC_ID_PCM_U16BE, CODEC_ID_PCM_S8, CODEC_ID_PCM_U8, CODEC_ID_PCM_MULAW,
    CODEC_ID_PCM_ALAW, CODEC_ID_PCM_S32LE, CODEC_ID_PCM_S32BE, CODEC_ID_PCM_U32LE,
    CODEC_ID_PCM_U32BE, CODEC_ID_PCM_S24LE, CODEC_ID_PCM_S24BE, CODEC_ID_PCM_U24LE,
    CODEC_ID_PCM_U24BE, CODEC_ID_PCM_S24DAUD,
    //* various adpcm codecs */
    CODEC_ID_ADPCM_IMA_QT= $11000, CODEC_ID_ADPCM_IMA_WAV, CODEC_ID_ADPCM_IMA_DK3,
    CODEC_ID_ADPCM_IMA_DK4, CODEC_ID_ADPCM_IMA_WS, CODEC_ID_ADPCM_IMA_SMJPEG,
    CODEC_ID_ADPCM_MS, CODEC_ID_ADPCM_4XM, CODEC_ID_ADPCM_XA, CODEC_ID_ADPCM_ADX,
    CODEC_ID_ADPCM_EA, CODEC_ID_ADPCM_G726, CODEC_ID_ADPCM_CT, CODEC_ID_ADPCM_SWF,
    CODEC_ID_ADPCM_YAMAHA, CODEC_ID_ADPCM_SBPRO_4, CODEC_ID_ADPCM_SBPRO_3,
    CODEC_ID_ADPCM_SBPRO_2,
    //* AMR */
    CODEC_ID_AMR_NB= $12000, CODEC_ID_AMR_WB,
    //* RealAudio codecs*/
    CODEC_ID_RA_144= $13000, CODEC_ID_RA_288,
    //* various DPCM codecs */
    CODEC_ID_ROQ_DPCM= $14000, CODEC_ID_INTERPLAY_DPCM, CODEC_ID_XAN_DPCM,
    CODEC_ID_SOL_DPCM, CODEC_ID_MP2= $15000,
    CODEC_ID_MP3, //* prefered ID for MPEG Audio layer 1, 2 or3 decoding */
    CODEC_ID_AAC, CODEC_ID_MPEG4AAC, CODEC_ID_AC3, CODEC_ID_DTS, CODEC_ID_VORBIS,
    CODEC_ID_DVAUDIO, CODEC_ID_WMAV1, CODEC_ID_WMAV2, CODEC_ID_MACE3,
    CODEC_ID_MACE6, CODEC_ID_VMDAUDIO, CODEC_ID_SONIC, CODEC_ID_SONIC_LS,
    CODEC_ID_FLAC, CODEC_ID_MP3ADU, CODEC_ID_MP3ON4, CODEC_ID_SHORTEN,
    CODEC_ID_ALAC, CODEC_ID_WESTWOOD_SND1, CODEC_ID_GSM, CODEC_ID_QDM2,
    CODEC_ID_COOK, CODEC_ID_TRUESPEECH, CODEC_ID_TTA, CODEC_ID_SMACKAUDIO,
    CODEC_ID_QCELP, CODEC_ID_WAVPACK,
    //* subtitle codecs */
    CODEC_ID_DVD_SUBTITLE= $17000, CODEC_ID_DVB_SUBTITLE,

    CODEC_ID_MPEG2TS= $20000 //* _FAKE_ codec to indicate a raw MPEG2 transport
                              //  stream (only used by libavformat) */
  );

//* CODEC_ID_MP3LAME is absolete */
const
  CODEC_ID_MP3LAME = CODEC_ID_MP3;

  AVPALETTE_SIZE = 1024;
  AVPALETTE_COUNT = 256;

type
  TCodecType = (
    CODEC_TYPE_UNKNOWN = -1,
    CODEC_TYPE_VIDEO,
    CODEC_TYPE_AUDIO,
    CODEC_TYPE_DATA,
    CODEC_TYPE_SUBTITLE,
	CODEC_TYPE_NB    (* CAT#3 *)
  );

//* currently unused, may be used if 24/32 bits samples ever supported */
//* all in native endian */
  TSampleFormat = (
    SAMPLE_FMT_NONE = -1,
    SAMPLE_FMT_U8,              ///< unsigned 8 bits
    SAMPLE_FMT_S16,             ///< signed 16 bits
    SAMPLE_FMT_S24,             ///< signed 24 bits
    SAMPLE_FMT_S32,             ///< signed 32 bits
    SAMPLE_FMT_FLT             ///< float
  );

const
//* in bytes */
  AVCODEC_MAX_AUDIO_FRAME_SIZE = 192000; // 1 second of 48khz 32bit audio

(**
 * Required number of additionally allocated bytes at the end of the input bitstream for decoding.
 * this is mainly needed because some optimized bitstream readers read
 * 32 or 64 bit at once and could read over the end<br>
 * Note, if the first 23 bits of the additional bytes are not 0 then damaged
 * MPEG bitstreams could cause overread and segfault
 *)
  FF_INPUT_BUFFER_PADDING_SIZE = 8;

(**
 * minimum encoding buffer size.
 * used to avoid some checks during header writing
 *)
  FF_MIN_BUFFER_SIZE = 16384;

type
//* motion estimation type, EPZS by default */
  TMotion_Est_ID = (
    ME_ZERO = 1,
    ME_FULL,
    ME_LOG,
    ME_PHODS,
    ME_EPZS,
    ME_X1,
    ME_HEX,
    ME_UMH,
    ME_ITER
  );

  TAVDiscard = (
//we leave some space between them for extensions (drop some keyframes for intra only or drop just some bidir frames)
    AVDISCARD_NONE   = -16, ///< discard nothing
    AVDISCARD_DEFAULT=  0, ///< discard useless packets like 0 size packets in avi
    AVDISCARD_NONREF =  8, ///< discard all non reference
    AVDISCARD_BIDIR  = 16, ///< discard all bidirectional frames
    AVDISCARD_NONKEY = 32, ///< discard all frames except keyframes
    AVDISCARD_ALL    = 48, ///< discard all
    AVDISCARD_FUCK = $FFFFFF
  );

  PRcOverride = ^TRcOverride;
  TRcOverride = record {16}
    start_frame: integer;
    end_frame: integer;
    qscale: integer; // if this is 0 then quality_factor will be used instead
    quality_factor: single;
  end;

const
  FF_MAX_B_FRAMES = 16;

(* encoding support
   these flags can be passed in AVCodecContext.flags before initing
   Note: not everything is supported yet.
*)

  CODEC_FLAG_QSCALE = $0002;  ///< use fixed qscale
  CODEC_FLAG_4MV    = $0004;  ///< 4 MV per MB allowed / Advanced prediction for H263
  CODEC_FLAG_QPEL   = $0010;  ///< use qpel MC
  CODEC_FLAG_GMC    = $0020;  ///< use GMC
  CODEC_FLAG_MV0    = $0040;  ///< always try a MB with MV=<0,0>
  CODEC_FLAG_PART   = $0080;  ///< use data partitioning
//* parent program gurantees that the input for b-frame containing streams is not written to
//   for at least s->max_b_frames+1 frames, if this is not set than the input will be copied */
  CODEC_FLAG_INPUT_PRESERVED = $0100;
  CODEC_FLAG_PASS1 = $0200;   ///< use internal 2pass ratecontrol in first  pass mode
  CODEC_FLAG_PASS2 = $0400;   ///< use internal 2pass ratecontrol in second pass mode
  CODEC_FLAG_EXTERN_HUFF = $1000; ///< use external huffman table (for mjpeg)
  CODEC_FLAG_GRAY  = $2000;   ///< only decode/encode grayscale
  CODEC_FLAG_EMU_EDGE = $4000; ///< don't draw edges
  CODEC_FLAG_PSNR           = $8000; ///< error[?] variables will be set during encoding
  CODEC_FLAG_TRUNCATED  = $00010000; //** input bitstream might be truncated at a random location instead
                                   //         of only at frame boundaries */
  CODEC_FLAG_NORMALIZE_AQP  = $00020000; ///< normalize adaptive quantization
  CODEC_FLAG_INTERLACED_DCT = $00040000; ///< use interlaced dct
  CODEC_FLAG_LOW_DELAY      = $00080000; ///< force low delay
  CODEC_FLAG_ALT_SCAN       = $00100000; ///< use alternate scan
  CODEC_FLAG_TRELLIS_QUANT  = $00200000; ///< use trellis quantization
  CODEC_FLAG_GLOBAL_HEADER  = $00400000; ///< place global headers in extradata instead of every keyframe
  CODEC_FLAG_BITEXACT       = $00800000; ///< use only bitexact stuff (except (i)dct)
//* Fx : Flag for h263+ extra options */
  CODEC_FLAG_H263P_AIC      = $01000000; ///< H263 Advanced intra coding / MPEG4 AC prediction (remove this)
  CODEC_FLAG_AC_PRED        = $01000000; ///< H263 Advanced intra coding / MPEG4 AC prediction
  CODEC_FLAG_H263P_UMV      = $02000000; ///< Unlimited motion vector
  CODEC_FLAG_CBP_RD         = $04000000; ///< use rate distortion optimization for cbp
  CODEC_FLAG_QP_RD          = $08000000; ///< use rate distortion optimization for qp selectioon
  CODEC_FLAG_H263P_AIV      = $00000008; ///< H263 Alternative inter vlc
  CODEC_FLAG_OBMC           = $00000001; ///< OBMC
  CODEC_FLAG_LOOP_FILTER    = $00000800; ///< loop filter
  CODEC_FLAG_H263P_SLICE_STRUCT = $10000000;
  CODEC_FLAG_INTERLACED_ME  = $20000000; ///< interlaced motion estimation
  CODEC_FLAG_SVCD_SCAN_OFFSET = $40000000; ///< will reserve space for SVCD scan offset user data
  CODEC_FLAG_CLOSED_GOP     = $80000000;
  CODEC_FLAG2_FAST          = $00000001; ///< allow non spec compliant speedup tricks
  CODEC_FLAG2_STRICT_GOP    = $00000002; ///< strictly enforce GOP size
  CODEC_FLAG2_NO_OUTPUT     = $00000004; ///< skip bitstream encoding
  CODEC_FLAG2_LOCAL_HEADER  = $00000008; ///< place global headers at every keyframe instead of in extradata
  CODEC_FLAG2_BPYRAMID      = $00000010; ///< H.264 allow b-frames to be used as references
  CODEC_FLAG2_WPRED         = $00000020; ///< H.264 weighted biprediction for b-frames
  CODEC_FLAG2_MIXED_REFS    = $00000040; ///< H.264 multiple references per partition
  CODEC_FLAG2_8X8DCT        = $00000080; ///< H.264 high profile 8x8 transform
  CODEC_FLAG2_FASTPSKIP     = $00000100; ///< H.264 fast pskip
  CODEC_FLAG2_AUD           = $00000200; ///< H.264 access unit delimiters
  CODEC_FLAG2_BRDO          = $00000400; ///< b-frame rate-distortion optimization
  CODEC_FLAG2_INTRA_VLC     = $00000800; ///< use MPEG-2 intra VLC table
  CODEC_FLAG2_MEMC_ONLY     = $00001000; ///< only do ME/MC (I frames -> ref, P frame -> ME+MC)

(* Unsupported options :
 *              Syntax Arithmetic coding (SAC)
 *              Reference Picture Selection
 *              Independant Segment Decoding */
/* /Fx */
/* codec capabilities *)

const
  CODEC_CAP_DRAW_HORIZ_BAND = $001; ///< decoder can use draw_horiz_band callback
(**
 * Codec uses get_buffer() for allocating buffers.
 * direct rendering method 1 *)
  CODEC_CAP_DR1             = $002;
(* if 'parse_only' field is true, then avcodec_parse_frame() can be   used *)
  CODEC_CAP_PARSE_ONLY      = $004;
  CODEC_CAP_TRUNCATED       = $008;
//* codec can export data for HW decoding (XvMC) */
  CODEC_CAP_HWACCEL         = $010;

(**
 * codec has a non zero delay and needs to be feeded with NULL at the end to get the delayed data.
 * if this is not set, the codec is guranteed to never be feeded with NULL data *)
  CODEC_CAP_DELAY           = $0020;
(**
 * Codec can be fed a final frame with a smaller size.
 * This can be used to prevent truncation of the last audio samples. *)
  CODEC_CAP_SMALL_LAST_FRAME = $0040;

//the following defines may change, don't expect compatibility if you use them
   MB_TYPE_INTRA4x4   = $001;
   MB_TYPE_INTRA16x16 = $002; //FIXME h264 specific
   MB_TYPE_INTRA_PCM  = $004; //FIXME h264 specific
   MB_TYPE_16x16      = $008;
   MB_TYPE_16x8       = $010;
   MB_TYPE_8x16       = $020;
   MB_TYPE_8x8        = $040;
   MB_TYPE_INTERLACED = $080;
   MB_TYPE_DIRECT2     = $100; //FIXME
   MB_TYPE_ACPRED     = $200;
   MB_TYPE_GMC        = $400;
   MB_TYPE_SKIP       = $800;
   MB_TYPE_P0L0       = $1000;
   MB_TYPE_P1L0       = $2000;
   MB_TYPE_P0L1       = $4000;
   MB_TYPE_P1L1       = $8000;
   MB_TYPE_L0         = (MB_TYPE_P0L0 or MB_TYPE_P1L0);
   MB_TYPE_L1         = (MB_TYPE_P0L1 or MB_TYPE_P1L1);
   MB_TYPE_L0L1       = (MB_TYPE_L0   or MB_TYPE_L1);
   MB_TYPE_QUANT      = $0010000;
   MB_TYPE_CBP        = $0020000;
//Note bits 24-31 are reserved for codec specific use (h264 ref0, mpeg1 0mv, ...)

type
(**
 * Pan Scan area.
 * this specifies the area which should be displayed. Note there may be multiple such areas for one frame *)
  PAVPanScan = ^TAVPanScan;
  TAVPanScan = record {24}
    (*** id.
     * - encoding: set by user.
     * - decoding: set by lavc     *)
    id: integer;

    (*** width and height in 1/16 pel
     * - encoding: set by user.
     * - decoding: set by lavc     *)
    width: integer;
    height: integer;

    (*** position of the top left corner in 1/16 pel for up to 3 fields/frames.
     * - encoding: set by user.
     * - decoding: set by lavc     *)
    position: array [0..2] of array [0..1] of smallint;
  end;

const
  FF_QSCALE_TYPE_MPEG1	= 0;
  FF_QSCALE_TYPE_MPEG2	= 1;

  FF_BUFFER_TYPE_INTERNAL = 1;
  FF_BUFFER_TYPE_USER     = 2; ///< Direct rendering buffers (image is (de)allocated by user)
  FF_BUFFER_TYPE_SHARED   = 4; ///< buffer from somewhere else, don't dealloc image (data/base), all other tables are not shared
  FF_BUFFER_TYPE_COPY     = 8; ///< just a (modified) copy of some other buffer, don't dealloc anything


  FF_I_TYPE = 1; // Intra
  FF_P_TYPE = 2; // Predicted
  FF_B_TYPE = 3; // Bi-dir predicted
  FF_S_TYPE = 4; // S(GMC)-VOP MPEG4
  FF_SI_TYPE = 5;
  FF_SP_TYPE = 6;

  FF_BUFFER_HINTS_VALID    = $01; // Buffer hints value is meaningful (if 0 ignore)
  FF_BUFFER_HINTS_READABLE = $02; // Codec will read from buffer
  FF_BUFFER_HINTS_PRESERVE = $04; // User must not alter buffer content
  FF_BUFFER_HINTS_REUSABLE = $08; // Codec will reuse the buffer (update)

type
  (*** Audio Video Frame. *)
  PAVFrame = ^TAVFrame;
  TAVFrame = record {200}
    (*** pointer to the picture planes.
     * this might be different from the first allocated byte *)
    data: array [0..3] of pbyte;
    linesize: array [0..3] of integer;
    (*** pointer to the first allocated byte of the picture. can be used in get_buffer/release_buffer
     * this isn't used by lavc unless the default get/release_buffer() is used*)
    base: array [0..3] of pbyte;
    (*** 1 -> keyframe, 0-> not *)
    key_frame: integer;
    (*** picture type of the frame, see ?_TYPE below.*)
    pict_type: integer;
    (*** presentation timestamp in time_base units (time when frame should be shown to user)
     * if AV_NOPTS_VALUE then frame_rate = 1/time_base will be assumed*)
    pts: int64;
    (*** picture number in bitstream order.*)
    coded_picture_number: integer;
    (*** picture number in display order.*)
    display_picture_number: integer;
    (*** quality (between 1 (good) and FF_LAMBDA_MAX (bad)) *)
    quality: integer;
    (*** buffer age (1->was last buffer and dint change, 2->..., ...).*)
    age: integer;
    (*** is this picture used as reference*)
    reference: integer;
    (*** QP table*)
    qscale_table: pchar;
    (*** QP store stride*)
    qstride: integer;
    (*** mbskip_table[mb]>=1 if MB didnt change*)
    mbskip_table: pbyte;
    (**
     * Motion vector table.
     * @code
     * example:
     * int mv_sample_log2= 4 - motion_subsample_log2;
     * int mb_width= (width+15)>>4;
     * int mv_stride= (mb_width << mv_sample_log2) + 1;
     * motion_val[direction][x + y*mv_stride][0->mv_x, 1->mv_y];
     * @endcode
     * - encoding: set by user
     * - decoding: set by lavc     *)
    motion_val: array [0..1] of pointer;
    (*** Macroblock type table
     * mb_type_base + mb_width + 2 *)
    mb_type: PCardinal;
    (*** log2 of the size of the block which a single vector in motion_val represents:
     * (4->16x16, 3->8x8, 2-> 4x4, 1-> 2x2)*)
    motion_subsample_log2: byte;
    (*** for some private data of the user*)
    opaque: pointer;
    (*** error*)
    error: array [0..3] of int64;
    (*** type of the buffer (to keep track of who has to dealloc data[*])
     * Note: user allocated (direct rendering) & internal buffers can not coexist currently*)
    _type: integer;
    (*** when decoding, this signal how much the picture must be delayed.
     * extra_delay = repeat_pict / (2*fps)*)
    repeat_pict: integer;
    qscale_type: integer;
    (*** The content of the picture is interlaced.*)
    interlaced_frame: integer;
    (*** if the content is interlaced, is top field displayed first.*)
    top_field_first: integer;
    (*** Pan scan.*)
    pan_scan: PAVPanScan;
    (*** tell user application that palette has changed from previous frame.*)
    palette_has_changed: integer;
    (*** Codec suggestion on buffer type if != 0
     * - decoding: set by lavc (before get_buffer() call))*)
    buffer_hints: integer;
    (*** DCT coeffitients*)
    dct_coeff: PsmallInt;
    (*** Motion referece frame index*)
    ref_index: array [0..1] of pshortint;
  end;

const
  DEFAULT_FRAME_RATE_BASE = 1001000;

  FF_BUG_AUTODETECT       = 1;  ///< autodetection
  FF_BUG_OLD_MSMPEG4      = 2;
  FF_BUG_XVID_ILACE       = 4;
  FF_BUG_UMP4             = 8;
  FF_BUG_NO_PADDING       = 16;
  FF_BUG_AMV              = 32;
  FF_BUG_AC_VLC           = 0;  ///< will be removed, libavcodec can now handle these non compliant files by default
  FF_BUG_QPEL_CHROMA      = 64;
  FF_BUG_STD_QPEL         = 128;
  FF_BUG_QPEL_CHROMA2     = 256;
  FF_BUG_DIRECT_BLOCKSIZE = 512;
  FF_BUG_EDGE             = 1024;
  FF_BUG_HPEL_CHROMA      = 2048;
  FF_BUG_DC_CLIP          = 4096;
  FF_BUG_MS               = 8192; ///< workaround various bugs in microsofts broken decoders

  FF_COMPLIANCE_VERY_STRICT   = 2; ///< strictly conform to a older more strict version of the spec or reference software
  FF_COMPLIANCE_STRICT        = 1; ///< strictly conform to all the things in the spec no matter what consequences
  FF_COMPLIANCE_NORMAL        = 0;
  FF_COMPLIANCE_INOFFICIAL    = -1; ///< allow inofficial extensions
  FF_COMPLIANCE_EXPERIMENTAL  = -2; ///< allow non standarized experimental things

  FF_ER_CAREFUL         = 1;
  FF_ER_COMPLIANT       = 2;
  FF_ER_AGGRESSIVE      = 3;
  FF_ER_VERY_AGGRESSIVE = 4;

  FF_DCT_AUTO    = 0;
  FF_DCT_FASTINT = 1;
  FF_DCT_INT     = 2;
  FF_DCT_MMX     = 3;
  FF_DCT_MLIB    = 4;
  FF_DCT_ALTIVEC = 5;
  FF_DCT_FAAN    = 6;

  FF_IDCT_AUTO         = 0;
  FF_IDCT_INT          = 1;
  FF_IDCT_SIMPLE       = 2;
  FF_IDCT_SIMPLEMMX    = 3;
  FF_IDCT_LIBMPEG2MMX  = 4;
  FF_IDCT_PS2          = 5;
  FF_IDCT_MLIB         = 6;
  FF_IDCT_ARM          = 7;
  FF_IDCT_ALTIVEC      = 8;
  FF_IDCT_SH4          = 9;
  FF_IDCT_SIMPLEARM    = 10;
  FF_IDCT_H264         = 11;
  FF_IDCT_VP3          = 12;
  FF_IDCT_IPP          = 13;
  FF_IDCT_XVIDMMX      = 14;

  FF_EC_GUESS_MVS   = 1;
  FF_EC_DEBLOCK     = 2;

  FF_MM_FORCE	      = $80000000; (* force usage of selected flags (OR) *)
    (* lower 16 bits - CPU features *)

  FF_MM_MMX	        = $0001; (* standard MMX *)
  FF_MM_3DNOW	      = $0004; (* AMD 3DNOW *)
  FF_MM_MMXEXT	    = $0002; (* SSE integer functions or AMD MMX ext *)
  FF_MM_SSE	        = $0008; (* SSE functions *)
  FF_MM_SSE2	      = $0010; (* PIV SSE2 functions *)
  FF_MM_3DNOWEXT	  = $0020; (* AMD 3DNowExt *)
  FF_MM_IWMMXT	    = $0100; (* XScale IWMMXT *)

  FF_PRED_LEFT   = 0;
  FF_PRED_PLANE  = 1;
  FF_PRED_MEDIAN = 2;

  FF_DEBUG_PICT_INFO    = 1;
  FF_DEBUG_RC           = 2;
  FF_DEBUG_BITSTREAM    = 4;
  FF_DEBUG_MB_TYPE      = 8;
  FF_DEBUG_QP           = 16;
  FF_DEBUG_MV           = 32;
  FF_DEBUG_DCT_COEFF    = $00000040;
  FF_DEBUG_SKIP         = $00000080;
  FF_DEBUG_STARTCODE    = $00000100;
  FF_DEBUG_PTS          = $00000200;
  FF_DEBUG_ER           = $00000400;
  FF_DEBUG_MMCO         = $00000800;
  FF_DEBUG_BUGS         = $00001000;
  FF_DEBUG_VIS_QP       = $00002000;
  FF_DEBUG_VIS_MB_TYPE  = $00004000;

  FF_DEBUG_VIS_MV_P_FOR  = $00000001; //visualize forward predicted MVs of P frames
  FF_DEBUG_VIS_MV_B_FOR  = $00000002; //visualize forward predicted MVs of B frames
  FF_DEBUG_VIS_MV_B_BACK = $00000004; //visualize backward predicted MVs of B frames

  FF_CMP_SAD  = 0;
  FF_CMP_SSE  = 1;
  FF_CMP_SATD = 2;
  FF_CMP_DCT  = 3;
  FF_CMP_PSNR = 4;
  FF_CMP_BIT  = 5;
  FF_CMP_RD   = 6;
  FF_CMP_ZERO = 7;
  FF_CMP_VSAD = 8;
  FF_CMP_VSSE = 9;
  FF_CMP_NSSE = 10;
  FF_CMP_W53  = 11;
  FF_CMP_W97  = 12;
  FF_CMP_DCTMAX = 13;
  FF_CMP_CHROMA = 256;

  FF_DTG_AFD_SAME         = 8;
  FF_DTG_AFD_4_3          = 9;
  FF_DTG_AFD_16_9         = 10;
  FF_DTG_AFD_14_9         = 11;
  FF_DTG_AFD_4_3_SP_14_9  = 13;
  FF_DTG_AFD_16_9_SP_14_9 = 14;
  FF_DTG_AFD_SP_4_3       = 15;

  FF_DEFAULT_QUANT_BIAS   = 999999;

  FF_LAMBDA_SHIFT   = 7;
  FF_LAMBDA_SCALE   = (1 shl FF_LAMBDA_SHIFT);
  FF_QP2LAMBDA      = 118; ///< factor to convert from H.263 QP to lambda
  FF_LAMBDA_MAX     = (256 * 128 - 1);

  FF_QUALITY_SCALE  = FF_LAMBDA_SCALE; //FIXME maybe remove

  FF_CODER_TYPE_VLC   = 0;
  FF_CODER_TYPE_AC    = 1;

  SLICE_FLAG_CODED_ORDER    = $0001; ///< draw_horiz_band() is called in coded order instead of display
  SLICE_FLAG_ALLOW_FIELD    = $0002; ///< allow draw_horiz_band() with field slices (MPEG2 field pics)
  SLICE_FLAG_ALLOW_PLANE    = $0004; ///< allow draw_horiz_band() with 1 component at a time (SVQ1)

  FF_MB_DECISION_SIMPLE = 0;        ///< uses mb_cmp
  FF_MB_DECISION_BITS   = 1;        ///< chooses the one which needs the fewest bits
  FF_MB_DECISION_RD     = 2;        ///< rate distoration

  FF_AA_AUTO    = 0;
  FF_AA_FASTINT = 1; //not implemented yet
  FF_AA_INT     = 2;
  FF_AA_FLOAT   = 3;

  FF_PROFILE_UNKNOWN  = -99;

  FF_LEVEL_UNKNOWN    = -99;

  X264_PART_I4X4 = $001;  (* Analyse i4x4 *)
  X264_PART_I8X8 = $002;  (* Analyse i8x8 (requires 8x8 transform) *)
  X264_PART_P8X8 = $010;  (* Analyse p16x8, p8x16 and p8x8 *)
  X264_PART_P4X4 = $020;  (* Analyse p8x4, p4x8, p4x4 *)
  X264_PART_B8X8 = $100;  (* Analyse b16x8, b8x16 and b8x8 *)

type
  PAVClass = ^TAVClass;
  PAVCodecContext = ^TAVCodecContext;
  PAVCodec = ^TAVCodec;
  PAVPaletteControl = ^TAVPaletteControl;

  TAVclass = record {12}
    class_name: pchar;
    (* actually passing a pointer to an AVCodecContext
					or AVFormatContext, which begin with an AVClass.
					Needed because av_log is in libavcodec and has no visibility
					of AVIn/OutputFormat *)
    item_name: function (): pchar; cdecl;
    option: PAVOption;
  end;

  TAVCodecContext = record {720}
    (*** Info on struct for av_log
     * - set by avcodec_alloc_context     *)
    av_class: PAVClass;
    (*** the average bitrate.
     * - encoding: set by user. unused for constant quantizer encoding
     * - decoding: set by lavc. 0 or some bitrate if this info is available in the stream     *)
    bit_rate: integer;
    (*** number of bits the bitstream is allowed to diverge from the reference.
     *           the reference can be CBR (for CBR pass1) or VBR (for pass2)
     * - encoding: set by user. unused for constant quantizer encoding
     * - decoding: unused     *)
    bit_rate_tolerance: integer;
    (*** CODEC_FLAG_*.
     * - encoding: set by user.
     * - decoding: set by user.     *)
    flags: integer;
    (*** some codecs needs additionnal format info. It is stored here
     * - encoding: set by user.
     * - decoding: set by lavc. (FIXME is this ok?)     *)
    sub_id: integer;

    (**
     * motion estimation algorithm used for video coding.
     * 1 (zero), 2 (full), 3 (log), 4 (phods), 5 (epzs), 6 (x1), 7 (hex),
     * 8 (umh), 9 (iter) [7, 8 are x264 specific, 9 is snow specific]
     * - encoding: MUST be set by user.
     * - decoding: unused     *)
    me_method: integer;

    (**
     * some codecs need / can use extra-data like huffman tables.
     * mjpeg: huffman tables
     * rv10: additional flags
     * mpeg4: global headers (they can be in the bitstream or here)
     * the allocated memory should be FF_INPUT_BUFFER_PADDING_SIZE bytes larger
     * then extradata_size to avoid prolems if its read with the bitstream reader
     * the bytewise contents of extradata must not depend on the architecture or cpu endianness
     * - encoding: set/allocated/freed by lavc.
     * - decoding: set/allocated/freed by user.
     *)
    extradata: pbyte;
    extradata_size: integer;

    (**
     * this is the fundamental unit of time (in seconds) in terms
     * of which frame timestamps are represented. for fixed-fps content,
     * timebase should be 1/framerate and timestamp increments should be
     * identically 1.
     * - encoding: MUST be set by user
     * - decoding: set by lavc.     *)
    time_base: TAVRational;

    (* video only *)
    (*** picture width / height.
     * - encoding: MUST be set by user.
     * - decoding: set by lavc.
     * Note, for compatibility its possible to set this instead of
     * coded_width/height before decoding     *)
    width, height: integer;
    (*** the number of pictures in a group of pitures, or 0 for intra_only.
     * - encoding: set by user.
     * - decoding: unused     *)
    gop_size: integer;
    (*** pixel format, see PIX_FMT_xxx.
     * - encoding: set by user.
     * - decoding: set by lavc.     *)
    pix_fmt: TAVPixelFormat;
    (*** Frame rate emulation. If not zero lower layer (i.e. format handler)
     * has to read frames at native frame rate.
     * - encoding: set by user.
     * - decoding: unused.     *)
    rate_emu: integer;
    (*** if non NULL, 'draw_horiz_band' is called by the libavcodec
     * decoder to draw an horizontal band. It improve cache usage. Not
     * all codecs can do that. You must check the codec capabilities
     * before
     * - encoding: unused
     * - decoding: set by user.
     * @param height the height of the slice
     * @param y the y position of the slice
     * @param type 1->top field, 2->bottom field, 3->frame
     * @param offset offset into the AVFrame.data from which the slice should be read *)
    draw_horiz_band: procedure (s: PAVCodecContext;
                                const src: PAVFrame; offset: PInteger;
                                y: integer; _type: integer; height: integer); cdecl;

    (* audio only *)
    sample_rate: integer; ///< samples per sec
    channels: integer;
    (*** audio sample format.
     * - encoding: set by user.
     * - decoding: set by lavc.     *)
    sample_fmt: TSampleFormat;  ///< sample format, currenly unused

    (* the following data should not be initialized *)
    (*** samples per packet. initialized when calling 'init'     *)
    frame_size: integer;
    frame_number: integer;   ///< audio or video frame number
    real_pict_num: integer;  ///< returns the real picture number of previous encoded frame

    (*** number of frames the decoded output will be delayed relative to
     * the encoded input.
     * - encoding: set by lavc.
     * - decoding: unused     *)
    delay: integer;

    (* - encoding parameters *)
    qcompress: single;  ///< amount of qscale change between easy & hard scenes (0.0-1.0)
    qblur: single;      ///< amount of qscale smoothing over time (0.0-1.0)

    (*** minimum quantizer.
     * - encoding: set by user.
     * - decoding: unused     *)
    qmin: integer;

    (*** maximum quantizer.
     * - encoding: set by user.
     * - decoding: unused     *)
    qmax: integer;

    (*** maximum quantizer difference etween frames.
     * - encoding: set by user.
     * - decoding: unused     *)
    max_qdiff: integer;

    (*** maximum number of b frames between non b frames.
     * note: the output will be delayed by max_b_frames+1 relative to the input
     * - encoding: set by user.
     * - decoding: unused     *)
    max_b_frames: integer;

    (*** qscale factor between ip and b frames.
     * - encoding: set by user.
     * - decoding: unused     *)
    b_quant_factor: single;

    (** obsolete FIXME remove *)
    rc_strategy: integer;
    b_frame_strategy: integer;

    (*** hurry up amount.
     * deprecated in favor of skip_idct and skip_frame
     * - encoding: unused
     * - decoding: set by user. 1-> skip b frames, 2-> skip idct/dequant too, 5-> skip everything except header     *)
    hurry_up: integer;

    codec: PAVCodec;

    priv_data: pointer;

    (* unused, FIXME remove*)
    rtp_mode: integer;

    rtp_payload_size: integer;  (* The size of the RTP payload: the coder will  *)
                                (* do it's best to deliver a chunk with size    *)
                                (* below rtp_payload_size, the chunk will start *)
                                (* with a start code on some codecs like H.263  *)
                                (* This doesn't take account of any particular  *)
                                (* headers inside the transmited RTP payload    *)


    (* The RTP callback: This function is called   *)
    (* every time the encoder has a packet to send *)
    (* Depends on the encoder if the data starts   *)
    (* with a Start Code (it should) H.263 does.   *)
    (* mb_nb contains the number of macroblocks    *)
    (* encoded in the RTP payload                  *)
    rtp_callback: procedure (avctx: PAVCodecContext; data: pointer;
                             size: integer; mb_nb: integer); cdecl;

    (* statistics, used for 2-pass encoding *)
    mv_bits: integer;
    header_bits: integer;
    i_tex_bits: integer;
    p_tex_bits: integer;
    i_count: integer;
    p_count: integer;
    skip_count: integer;
    misc_bits: integer;

    (*** number of bits used for the previously encoded frame.
     * - encoding: set by lavc
     * - decoding: unused     *)
    frame_bits: integer;

    (*** private data of the user, can be used to carry app specific stuff.
     * - encoding: set by user
     * - decoding: set by user     *)
    opaque: pointer;

    codec_name: array [0..31] of char;
    codec_type: TCodecType; (* see CODEC_TYPE_xxx *)
    codec_id: TCodecID; (* see CODEC_ID_xxx *)

    (*** fourcc (LSB first, so "ABCD" -> ('D'<<24) + ('C'<<16) + ('B'<<8) + 'A').
     * this is used to workaround some encoder bugs
     * - encoding: set by user, if not then the default based on codec_id will be used
     * - decoding: set by user, will be converted to upper case by lavc during init     *)
    codec_tag: cardinal;  // можно array [0..3] of char - тогда видно FOURCC
//    codec_tag: array [0..3] of char; 

    (*** workaround bugs in encoders which sometimes cannot be detected automatically.
     * - encoding: set by user
     * - decoding: set by user     *)
    workaround_bugs: integer;

    (*** luma single coeff elimination threshold.
     * - encoding: set by user
     * - decoding: unused     *)
    luma_elim_threshold: integer;

    (*** chroma single coeff elimination threshold.
     * - encoding: set by user
     * - decoding: unused     *)
    chroma_elim_threshold: integer;

    (*** strictly follow the std (MPEG4, ...).
     * - encoding: set by user
     * - decoding: unused     *)
    strict_std_compliance: integer;

    (*** qscale offset between ip and b frames.
     * if > 0 then the last p frame quantizer will be used (q= lastp_q*factor+offset)
     * if < 0 then normal ratecontrol will be done (q= -normal_q*factor+offset)
     * - encoding: set by user.
     * - decoding: unused     *)
    b_quant_offset: single;

    (*** error resilience higher values will detect more errors but may missdetect
     * some more or less valid parts as errors.
     * - encoding: unused
     * - decoding: set by user     *)
    error_resilience: integer;

    (*** called at the beginning of each frame to get a buffer for it.
     * if pic.reference is set then the frame will be read later by lavc
     * avcodec_align_dimensions() should be used to find the required width and
     * height, as they normally need to be rounded up to the next multiple of 16
     * - encoding: unused
     * - decoding: set by lavc, user can override     *)
    get_buffer: function (c: PAVCodecContext; pic: PAVFrame): integer; cdecl;

    (*** called to release buffers which where allocated with get_buffer.
     * a released buffer can be reused in get_buffer()
     * pic.data[*] must be set to NULL
     * - encoding: unused
     * - decoding: set by lavc, user can override     *)
    release_buffer: procedure (c: PAVCodecContext; pic: PAVFrame); cdecl;

    (*** if 1 the stream has a 1 frame delay during decoding.
     * - encoding: set by lavc
     * - decoding: set by lavc     *)
    has_b_frames: integer;

    (*** number of bytes per packet if constant and known or 0
     * used by some WAV based audio codecs     *)
    block_align: integer;

    parse_only: integer; (* - decoding only: if true, only parsing is done
                       (function avcodec_parse_frame()). The frame
                       data is returned. Only MPEG codecs support this now. *)

    (*** 0-> h263 quant 1-> mpeg quant.
     * - encoding: set by user.
     * - decoding: unused     *)
    mpeg_quant: integer;

    (*** pass1 encoding statistics output buffer.
     * - encoding: set by lavc
     * - decoding: unused     *)
    stats_out: pchar;

    (*** pass2 encoding statistics input buffer.
     * concatenated stuff from stats_out of pass1 should be placed here
     * - encoding: allocated/set/freed by user
     * - decoding: unused     *)
    stats_in: pchar;

    (*** ratecontrol qmin qmax limiting method.
     * 0-> clipping, 1-> use a nice continous function to limit qscale wthin qmin/qmax
     * - encoding: set by user.
     * - decoding: unused     *)
    rc_qsquish: single;

    rc_qmod_amp: single;
    rc_qmod_freq: integer;

    (*** ratecontrol override, see RcOverride.
     * - encoding: allocated/set/freed by user.
     * - decoding: unused     *)
    rc_override: PRcOverride;
    rc_override_count: integer;

    (*** rate control equation.
     * - encoding: set by user
     * - decoding: unused     *)
    rc_eq: pchar;

    (*** maximum bitrate.
     * - encoding: set by user.
     * - decoding: unused     *)
    rc_max_rate: integer;

    (*** minimum bitrate.
     * - encoding: set by user.
     * - decoding: unused     *)
    rc_min_rate: integer;

    (*** decoder bitstream buffer size.
     * - encoding: set by user.
     * - decoding: unused     *)
    rc_buffer_size: integer;
    rc_buffer_aggressivity: single;

    (*** qscale factor between p and i frames.
     * if > 0 then the last p frame quantizer will be used (q= lastp_q*factor+offset)
     * if < 0 then normal ratecontrol will be done (q= -normal_q*factor+offset)
     * - encoding: set by user.
     * - decoding: unused     *)
    i_quant_factor: single;

    (*** qscale offset between p and i frames.
     * - encoding: set by user.
     * - decoding: unused     *)
    i_quant_offset: single;

    (*** initial complexity for pass1 ratecontrol.
     * - encoding: set by user.
     * - decoding: unused     *)
    rc_initial_cplx: single;

    (*** dct algorithm, see FF_DCT_* below.
     * - encoding: set by user
     * - decoding: unused     *)
    dct_algo: integer;

    (*** luminance masking (0-> disabled).
     * - encoding: set by user
     * - decoding: unused     *)
    lumi_masking: single;

    (*** temporary complexity masking (0-> disabled).
     * - encoding: set by user
     * - decoding: unused     *)
    temporal_cplx_masking: single;

    (*** spatial complexity masking (0-> disabled).
     * - encoding: set by user
     * - decoding: unused     *)
    spatial_cplx_masking: single;

    (**     * p block masking (0-> disabled).
     * - encoding: set by user
     * - decoding: unused     *)
    p_masking: single;

    (*** darkness masking (0-> disabled).
     * - encoding: set by user
     * - decoding: unused     *)
    dark_masking: single;

    (* for binary compatibility *)
    unused: integer;

    (*** idct algorithm, see FF_IDCT_* below.
     * - encoding: set by user
     * - decoding: set by user     *)
    idct_algo: integer;

    (*** slice count.
     * - encoding: set by lavc
     * - decoding: set by user (or 0)     *)
    slice_count: integer;

    (*** slice offsets in the frame in bytes.
     * - encoding: set/allocated by lavc
     * - decoding: set/allocated by user (or NULL)     *)
    slice_offset: Pinteger;

    (*** error concealment flags.
     * - encoding: unused
     * - decoding: set by user     *)
    error_concealment: integer;

    (*** dsp_mask could be add used to disable unwanted CPU features
     * CPU features (i.e. MMX, SSE. ...)
     *
     * with FORCE flag you may instead enable given CPU features
     * (Dangerous: usable in case of misdetection, improper usage however will
     * result into program crash)     *)
    dsp_mask: cardinal;

    (*** bits per sample/pixel from the demuxer (needed for huffyuv).
     * - encoding: set by lavc
     * - decoding: set by user     *)
     bits_per_sample: integer;

    (*** prediction method (needed for huffyuv).
     * - encoding: set by user
     * - decoding: unused     *)
     prediction_method: integer;

    (*** sample aspect ratio (0 if unknown).
     * numerator and denominator must be relative prime and smaller then 256 for some video standards
     * - encoding: set by user.
     * - decoding: set by lavc.     *)
    sample_aspect_ratio: TAVRational;

    (*** the picture in the bitstream.
     * - encoding: set by lavc
     * - decoding: set by lavc     *)
    coded_frame: PAVFrame;

    (*** debug.
     * - encoding: set by user.
     * - decoding: set by user.     *)
    debug: integer;

    (*** debug.
     * - encoding: set by user.
     * - decoding: set by user.     *)
    debug_mv: integer;

    (** error.
     * - encoding: set by lavc if flags&CODEC_FLAG_PSNR
     * - decoding: unused     *)
    error: array [0..3] of int64;

    (*** minimum MB quantizer.
     * - encoding: unused
     * - decoding: unused     *)
    mb_qmin: integer;

    (*** maximum MB quantizer.
     * - encoding: unused
     * - decoding: unused     *)
    mb_qmax: integer;

    (*** motion estimation compare function.
     * - encoding: set by user.
     * - decoding: unused     *)
    me_cmp: integer;

    (*** subpixel motion estimation compare function.
     * - encoding: set by user.
     * - decoding: unused     *)
    me_sub_cmp: integer;
    (*** macroblock compare function (not supported yet).
     * - encoding: set by user.
     * - decoding: unused     *)
    mb_cmp: integer;
    (*** interlaced dct compare function
     * - encoding: set by user.
     * - decoding: unused     *)
    ildct_cmp: integer;
    (*** ME diamond size & shape.
     * - encoding: set by user.
     * - decoding: unused     *)
    dia_size: integer;

    (*** amount of previous MV predictors (2a+1 x 2a+1 square).
     * - encoding: set by user.
     * - decoding: unused     *)
    last_predictor_count: integer;

    (*** pre pass for motion estimation.
     * - encoding: set by user.
     * - decoding: unused     *)
    pre_me: integer;

    (*** motion estimation pre pass compare function.
     * - encoding: set by user.
     * - decoding: unused     *)
    me_pre_cmp: integer;

    (*** ME pre pass diamond size & shape.
     * - encoding: set by user.
     * - decoding: unused     *)
    pre_dia_size: integer;

    (*** subpel ME quality.
     * - encoding: set by user.
     * - decoding: unused     *)
    me_subpel_quality: integer;

    (*** callback to negotiate the pixelFormat.
     * @param fmt is the list of formats which are supported by the codec,
     * its terminated by -1 as 0 is a valid format, the formats are ordered by quality
     * the first is allways the native one
     * @return the choosen format
     * - encoding: unused
     * - decoding: set by user, if not set then the native format will always be choosen
     *)
    get_format: function (s: PAVCodecContext; const fmt: PAVPixelFormat): TAVPixelFormat; cdecl;

    (*** DTG active format information (additionnal aspect ratio
     * information only used in DVB MPEG2 transport streams). 0 if
     * not set.
     * - encoding: unused.
     * - decoding: set by decoder     *)
    dtg_active_format: integer;

    (*** Maximum motion estimation search range in subpel units.
     * if 0 then no limit
     * - encoding: set by user.
     * - decoding: unused.     *)
    me_range: integer;

    (*** intra quantizer bias.
     * - encoding: set by user.
     * - decoding: unused     *)
    intra_quant_bias: integer;

    (*** inter quantizer bias.
     * - encoding: set by user.
     * - decoding: unused     *)
    inter_quant_bias: integer;

    (*** color table ID.
     * - encoding: unused.
     * - decoding: which clrtable should be used for 8bit RGB images
     *             table have to be stored somewhere FIXME     *)
    color_table_id: integer;

    (*** internal_buffer count.
     * Don't touch, used by lavc default_get_buffer()     *)
    internal_buffer_count: integer;

    (*** internal_buffers.
     * Don't touch, used by lavc default_get_buffer()     *)
    internal_buffer: pointer;

    (*** global quality for codecs which cannot change it per frame.
     * this should be proportional to MPEG1/2/4 qscale.
     * - encoding: set by user.
     * - decoding: unused     *)
    global_quality: integer;

    (*** coder type
     * - encoding: set by user.
     * - decoding: unused     *)
    coder_type: integer;

    (*** context model
     * - encoding: set by user.
     * - decoding: unused     *)
    context_model: integer;

    (*** slice flags
     * - encoding: unused
     * - decoding: set by user.     *)
    slice_flags: integer;

    (*** XVideo Motion Acceleration
     * - encoding: forbidden
     * - decoding: set by decoder     *)
    xvmc_acceleration: integer;

    (*** macroblock decision mode
     * - encoding: set by user.
     * - decoding: unused     *)
    mb_decision: integer;

    (*** custom intra quantization matrix
     * - encoding: set by user, can be NULL
     * - decoding: set by lavc     *)
    intra_matrix: Pword;

    (*** custom inter quantization matrix
     * - encoding: set by user, can be NULL
     * - decoding: set by lavc     *)
    inter_matrix: Pword;

    (*** fourcc from the AVI stream header (LSB first, so "ABCD" -> ('D'<<24) + ('C'<<16) + ('B'<<8) + 'A').
     * this is used to workaround some encoder bugs
     * - encoding: unused
     * - decoding: set by user, will be converted to upper case by lavc during init     *)
    stream_codec_tag: array [0..3] of char; //cardinal;

    (*** scene change detection threshold.
     * 0 is default, larger means fewer detected scene changes
     * - encoding: set by user.
     * - decoding: unused     *)
    scenechange_threshold: integer;

    (*** minimum lagrange multipler
     * - encoding: set by user.
     * - decoding: unused     *)
    lmin: integer;

    (*** maximum lagrange multipler
     * - encoding: set by user.
     * - decoding: unused     *)
    lmax: integer;

    (*** Palette control structure
     * - encoding: ??? (no palette-enabled encoder yet)
     * - decoding: set by user.     *)
    palctrl: PAVPaletteControl;

    (*** noise reduction strength
     * - encoding: set by user.
     * - decoding: unused     *)
    noise_reduction: integer;

    (*** called at the beginning of a frame to get cr buffer for it.
     * buffer type (size, hints) must be the same. lavc won't check it.
     * lavc will pass previous buffer in pic, function should return
     * same buffer or new buffer with old frame "painted" into it.
     * if pic.data[0] == NULL must behave like get_buffer().
     * - encoding: unused
     * - decoding: set by lavc, user can override     *)
    reget_buffer: function (c: PAVCodecContext; pic: PAVFrame): integer; cdecl;

    (*** number of bits which should be loaded into the rc buffer before decoding starts
     * - encoding: set by user.
     * - decoding: unused     *)
    rc_initial_buffer_occupancy: integer;
    inter_threshold: integer;

    (*** CODEC_FLAG2_*.
     * - encoding: set by user.
     * - decoding: set by user.     *)
    flags2: integer;

    (*** simulates errors in the bitstream to test error concealment.
     * - encoding: set by user.
     * - decoding: unused.     *)
    error_rate: integer;

    (*** MP3 antialias algorithm, see FF_AA_* below.
     * - encoding: unused
     * - decoding: set by user     *)
    antialias_algo: integer;

    (*** Quantizer noise shaping.
     * - encoding: set by user
     * - decoding: unused     *)
    quantizer_noise_shaping: integer;

    (*** Thread count.
     * is used to decide how many independant tasks should be passed to execute()
     * - encoding: set by user
     * - decoding: set by user     *)
    thread_count: integer;

    (*** the codec may call this to execute several independant things. it will return only after
     * finishing all tasks, the user may replace this with some multithreaded implementation, the
     * default implementation will execute the parts serially
     * @param count the number of things to execute
     * - encoding: set by lavc, user can override
     * - decoding: set by lavc, user can override     *)
    execute: function (c: PAVCodecContext; func: pointer; arg: PPointer; ret: PInteger; count: integer): integer; cdecl;

    (*** Thread opaque.
     * can be used by execute() to store some per AVCodecContext stuff.
     * - encoding: set by execute()
     * - decoding: set by execute()     *)
    thread_opaque: pointer;

    (*** Motion estimation threshold. under which no motion estimation is
     * performed, but instead the user specified motion vectors are used
     * - encoding: set by user
     * - decoding: unused     *)
     me_threshold: integer;

    (*** Macroblock threshold. under which the user specified macroblock types will be used
     * - encoding: set by user
     * - decoding: unused    *)
     mb_threshold: integer;

    (*** precision of the intra dc coefficient - 8.
     * - encoding: set by user
     * - decoding: unused     *)
     intra_dc_precision: integer;

    (*** noise vs. sse weight for the nsse comparsion function.
     * - encoding: set by user
     * - decoding: unused     *)
     nsse_weight: integer;

    (*** number of macroblock rows at the top which are skipped.
     * - encoding: unused
     * - decoding: set by user     *)
     skip_top: integer;

    (*** number of macroblock rows at the bottom which are skipped.
     * - encoding: unused
     * - decoding: set by user     *)
     skip_bottom: integer;

    (*** profile
     * - encoding: set by user
     * - decoding: set by lavc     *)
     profile: integer;

    (*** level
     * - encoding: set by user
     * - decoding: set by lavc     *)
     level: integer;

    (*** low resolution decoding. 1-> 1/2 size, 2->1/4 size
     * - encoding: unused
     * - decoding: set by user     *)
     lowres: integer;

    (*** bitsream width / height. may be different from width/height if lowres
     * or other things are used
     * - encoding: unused
     * - decoding: set by user before init if known, codec should override / dynamically change if needed     *)
    coded_width, coded_height: integer;

    (*** frame skip threshold
     * - encoding: set by user
     * - decoding: unused     *)
    frame_skip_threshold: integer;

    (*** frame skip factor
     * - encoding: set by user
     * - decoding: unused     *)
    frame_skip_factor: integer;

    (*** frame skip exponent
     * - encoding: set by user
     * - decoding: unused     *)
    frame_skip_exp: integer;

    (*** frame skip comparission function
     * - encoding: set by user.
     * - decoding: unused     *)
    frame_skip_cmp: integer;

    (*** border processing masking. raises the quantizer for mbs on the borders
     * of the picture.
     * - encoding: set by user
     * - decoding: unused     *)
    border_masking: single;

    (*** minimum MB lagrange multipler.
     * - encoding: set by user.
     * - decoding: unused     *)
    mb_lmin: integer;

    (*** maximum MB lagrange multipler.
     * - encoding: set by user.
     * - decoding: unused     *)
    mb_lmax: integer;

    (***
     * - encoding: set by user.
     * - decoding: unused     *)
    me_penalty_compensation: integer;

    (***
     * - encoding: unused
     * - decoding: set by user.     *)
    skip_loop_filter: TAVDiscard;

    (**     *
     * - encoding: unused
     * - decoding: set by user.     *)
    skip_idct: TAVDiscard;

    (**     *
     * - encoding: unused
     * - decoding: set by user.     *)
    skip_frame: TAVDiscard;

    (**     *
     * - encoding: set by user.
     * - decoding: unused     *)
    bidir_refine: integer;

    (**     *
     * - encoding: set by user.
     * - decoding: unused     *)
    brd_scale: integer;

    (**
     * constant rate factor - quality-based VBR - values ~correspond to qps
     * - encoding: set by user.
     * - decoding: unused     *)
    crf: integer;

    (**
     * constant quantization parameter rate control method
     * - encoding: set by user.
     * - decoding: unused     *)
    cqp: integer;

    (**
     * minimum gop size
     * - encoding: set by user.
     * - decoding: unused     *)
    keyint_min: integer;

    (**
     * number of reference frames
     * - encoding: set by user.
     * - decoding: unused     *)
    refs: integer;

    (**
     * chroma qp offset from luma
     * - encoding: set by user.
     * - decoding: unused     *)
    chromaoffset: integer;

    (**
     * influences how often b-frames are used
     * - encoding: set by user.
     * - decoding: unused     *)
    bframebias: integer;

    (**
     * trellis RD quantization
     * - encoding: set by user.
     * - decoding: unused     *)
    trellis: integer;

    (**
     * reduce fluctuations in qp (before curve compression)
     * - encoding: set by user.
     * - decoding: unused     *)
    complexityblur: single;

    (**
     * in-loop deblocking filter alphac0 parameter
     * alpha is in the range -6...6
     * - encoding: set by user.
     * - decoding: unused     *)
    deblockalpha: integer;

    (**
     * in-loop deblocking filter beta parameter
     * beta is in the range -6...6
     * - encoding: set by user.
     * - decoding: unused     *)
    deblockbeta: integer;

    (**
     * macroblock subpartition sizes to consider - p8x8, p4x4, b8x8, i8x8, i4x4
     * - encoding: set by user.
     * - decoding: unused     *)
    partitions: integer;

    (**
     * direct mv prediction mode - 0 (none), 1 (spatial), 2 (temporal)
     * - encoding: set by user.
     * - decoding: unused     *)
    directpred: integer;

    (**
     * audio cutoff bandwidth (0 means "automatic") . Currently used only by FAAC
     * - encoding: set by user.
     * - decoding: unused     *)
    cutoff: integer;

    (**
     * multiplied by qscale for each frame and added to scene_change_score
     * - encoding: set by user.
     * - decoding: unused     *)
    scenechange_factor: integer;

    (**     *
     * note: value depends upon the compare functin used for fullpel ME
     * - encoding: set by user.
     * - decoding: unused     *)
    mv0_threshold: integer;

    (**
     * adjusts sensitivity of b_frame_strategy 1
     * - encoding: set by user.
     * - decoding: unused      *)
    b_sensitivity: integer;

    (**
     * - encoding: set by user.
     * - decoding: unused      *)
    compression_level: integer;

    (**
     * sets whether to use LPC mode - used by FLAC encoder
     * - encoding: set by user.
     * - decoding: unused.     *)
    use_lpc: integer;

    (**
     * LPC coefficient precision - used by FLAC encoder
     * - encoding: set by user.
     * - decoding: unused.     *)
    lpc_coeff_precision: integer;

    (**
     * - encoding: set by user.
     * - decoding: unused.           *)
    min_prediction_order: integer;

    (**
     * - encoding: set by user.
     * - decoding: unused.            *)
    max_prediction_order: integer;

    (**
     * search method for selecting prediction order
     * - encoding: set by user.
     * - decoding: unused.            *)
    prediction_order_method: integer;

    (**
     * - encoding: set by user.
     * - decoding: unused.               *)
    min_partition_order: integer;

    (**
     * - encoding: set by user.
     * - decoding: unused.           *)
    max_partition_order: integer;
  end;

(**
 * AVCodec.
 *)
  TAVCodec = record
    name: pchar;
    _type: TCodecType;
    id: TCodecID;
    priv_data_size: integer;
    init: function (avctx: PAVCodecContext): integer; cdecl; (* typo corretion by the Creative CAT *)
    encode: function (avctx: PAVCodecContext; buf: pchar; buf_size: integer; data: pointer): integer; cdecl;
    close: function (avctx: PAVCodecContext): integer; cdecl;
    decode: function (avctx: PAVCodecContext; outdata: pointer; outdata_size: PInteger;
                  buf: pchar; buf_size: integer): integer; cdecl;
    capabilities: integer;
// todo: check this ->
//    void *dummy; // FIXME remove next time we break binary compatibility
    next: PAVCodec;
    flush: procedure (avctx: PAVCodecContext); cdecl;
    supported_framerates: PAVRational; ///array of supported framerates, or NULL if any, array is terminated by {0,0}
    pix_fmts: PAVPixelFormat;       ///array of supported pixel formats, or NULL if unknown, array is terminanted by -1
  end;

(**
 * four components are given, that's all.
 * the last component is alpha
 *)
  PAVPicture = ^TAVPicture;
  TAVPicture = record
    data: array [0..3] of pchar;
    linesize: array [0..3] of integer;       ///< number of bytes per line
  end;

(**
 * AVPaletteControl
 * This structure defines a method for communicating palette changes
 * between and demuxer and a decoder.
 * this is totally broken, palette changes should be sent as AVPackets
 *)
  TAVPaletteControl = record
    (* demuxer sets this to 1 to indicate the palette has changed;
     * decoder resets to 0 *)
    palette_changed: integer;

    (* 4-byte ARGB palette entries, stored in native byte order; note that
     * the individual palette components should be on a 8-bit scale; if
     * the palette data comes from a IBM VGA native format, the component
     * data is probably 6 bits in size and needs to be scaled *)
    palette: array [0..AVPALETTE_COUNT - 1] of cardinal;
  end;

  PAVSubtitleRect = ^TAVSubtitleRect;
  TAVSubtitleRect = record
    x: word;
    y: word;
    w: word;
    h: word;
    nb_colors: word;
    linesize: integer;
    rgba_palette: PCardinal;
    bitmap: pchar;
  end;

  PAVSubtitle = ^TAVSubtitle;
  TAVSubtitle = record {20}
    format: word; (* 0 = graphics *)
    start_display_time: cardinal; (* relative to packet pts, in ms *)
    end_display_time: cardinal; (* relative to packet pts, in ms *)
    num_rects: cardinal;
    rects: PAVSubtitleRect;
  end;


(* resample.c *)

  PReSampleContext = pointer;
  PAVResampleContext = pointer;
  PImgReSampleContext = pointer;

function audio_resample_init (output_channels: integer; input_channels: integer;
                              output_rate: integer; input_rate: integer): PReSampleContext;
  cdecl; external av__codec;

function audio_resample (s: PReSampleContext; output: PWord; input: PWord; nb_samples: integer): integer;
  cdecl; external av__codec;

procedure audio_resample_close (s: PReSampleContext);
  cdecl; external av__codec;


function av_resample_init (out_rate: integer; in_rate: integer; filter_length: integer;
                           log2_phase_count: integer; linear: integer; cutoff: double): PAVResampleContext;
  cdecl; external av__codec;

function av_resample (c: PAVResampleContext; dst: PWord; src: PWord; consumed: PInteger;
                      src_size: integer; dst_size: integer; update_ctx: integer): integer;
  cdecl; external av__codec;

procedure av_resample_compensate (c: PAVResampleContext; sample_delta: integer;
                                  compensation_distance: integer);
  cdecl; external av__codec;

procedure av_resample_close (c: PAVResampleContext);
  cdecl; external av__codec;


(* YUV420 format is assumed ! *)

  function img_resample_init (output_width: integer; output_height: integer;
                             input_width: integer; input_height: integer): PImgReSampleContext;
    cdecl; external av__codec;

  function img_resample_full_init (owidth: integer; oheight: integer;
                                      iwidth: integer; iheight: integer;
                                      topBand: integer; bottomBand: integer;
                                      leftBand: integer; rightBand: integer;
                                      padtop: integer; padbottom: integer;
                                      padleft: integer; padright: integer): PImgReSampleContext;
    cdecl; external av__codec;

  procedure img_resample (s: PImgReSampleContext; output: PAVPicture; const input: PAVPicture);
    cdecl; external av__codec;

  procedure img_resample_close (s: PImgReSampleContext);
    cdecl; external av__codec;

(**
 * Allocate memory for a picture.  Call avpicture_free to free it.
 *
 * @param picture the picture to be filled in.
 * @param pix_fmt the format of the picture.
 * @param width the width of the picture.
 * @param height the height of the picture.
 * @return 0 if successful, -1 if not.
 *)
  function avpicture_alloc (picture: PAVPicture; pix_fmt: TAVPixelFormat;
                            width: integer; height: integer): integer;
    cdecl; external av__codec;


(* Free a picture previously allocated by avpicture_alloc. *)
  procedure avpicture_free (picture: PAVPicture);
    cdecl; external av__codec;

  function avpicture_fill (picture: PAVPicture; ptr: pointer;
                   pix_fmt: TAVPixelFormat; width: integer; height: integer): integer;
    cdecl; external av__codec;

  function avpicture_layout (const src: PAVPicture; pix_fmt: TAVPixelFormat;
                     width: integer; height: integer;
                     dest: pchar; dest_size: integer): integer;
    cdecl; external av__codec;

  function avpicture_get_size (pix_fmt: TAVPixelFormat; width: integer; height: integer): integer;
    cdecl; external av__codec;

  procedure avcodec_get_chroma_sub_sample (pix_fmt: TAVPixelFormat; h_shift: Pinteger; v_shift: pinteger);
    cdecl; external av__codec;

  function avcodec_get_pix_fmt_name(pix_fmt: TAVPixelFormat): pchar;
    cdecl; external av__codec;

  procedure avcodec_set_dimensions(s: PAVCodecContext; width: integer; height: integer);
    cdecl; external av__codec;

  function avcodec_get_pix_fmt(const name: pchar): TAVPixelFormat;
    cdecl; external av__codec;

  function avcodec_pix_fmt_to_codec_tag(p: TAVPixelFormat): cardinal;
    cdecl; external av__codec;

  function avcodec_get_pix_fmt_loss (dst_pix_fmt: TAVPixelFormat; src_pix_fmt: TAVPixelFormat;
                             has_alpha: integer): integer;
    cdecl; external av__codec;

  function avcodec_find_best_pix_fmt (pix_fmt_mask: integer; src_pix_fmt: TAVPixelFormat;
                              has_alpha: integer; loss_ptr: pinteger): integer;
    cdecl; external av__codec;

  function img_get_alpha_info (const src: PAVPicture;
		                           pix_fmt: TAVPixelFormat;
                               width: integer; height: integer): integer;
    cdecl; external av__codec;


(* convert among pixel formats *)
  function img_convert (dst: PAVPicture; dst_pix_fmt: TAVPixelFormat;
                const src: PAVPicture; pix_fmt: TAVPixelFormat;
                width: integer; height: integer): integer;
    cdecl; external av__codec;

(* deinterlace a picture *)
  function avpicture_deinterlace (dst: PAVPicture; const src: PAVPicture;
                          pix_fmt: TAVPixelFormat; width: integer; height: integer): integer;
    cdecl; external av__codec;

(* returns LIBAVCODEC_VERSION_INT constant *)
  function avcodec_version (): cardinal;
    cdecl; external av__codec;

(* returns LIBAVCODEC_BUILD constant *)
  function avcodec_build (): cardinal;
    cdecl; external av__codec;

  procedure avcodec_init ();
    cdecl; external av__codec;

  procedure register_avcodec (format: PAVCodec);
    cdecl; external av__codec;

  function avcodec_find_encoder (id: TCodecID): PAVCodec;
    cdecl; external av__codec;
  function avcodec_find_encoder_by_name (name: pchar): PAVCodec;
    cdecl; external av__codec;
  function avcodec_find_decoder(id: TCodecID): PAVCodec;
    cdecl; external av__codec;
  function avcodec_find_decoder_by_name (name: pchar): PAVCodec;
    cdecl; external av__codec;
  procedure avcodec_string(buf: pchar; buf_size: integer; enc: PAVCodecContext; encode: integer);
    cdecl; external av__codec;

  procedure avcodec_get_context_defaults (s: PAVCodecContext);
    cdecl; external av__codec;
  function avcodec_alloc_context : PAVCodecContext;
    cdecl; external av__codec;
(* favourite of The Creative CAT
  function avcodec_alloc_context (): PAVCodecContext;
    cdecl; external av__codec; *)
  procedure avcodec_get_frame_defaults (pic: PAVFrame);
    cdecl; external av__codec;
  function avcodec_alloc_frame : PAVFrame;
    cdecl; external av__codec;
(* favourite of The Creative CAT
  function avcodec_alloc_frame (): PAVFrame;
    cdecl; external av__codec; *)

  function avcodec_default_get_buffer (s: PAVCodecContext; pic: PAVFrame): integer;
    cdecl; external av__codec;
  procedure avcodec_default_release_buffer (s: PAVCodecContext; pic: PAVFrame);
    cdecl; external av__codec;
  function avcodec_default_reget_buffer (s: PAVCodecContext; pic: PAVFrame): integer;
    cdecl; external av__codec;
  procedure avcodec_align_dimensions(s: PAVCodecContext; width: Pinteger; height: PInteger);
    cdecl; external av__codec;
  function avcodec_check_dimensions (av_log_ctx: pointer; w: cardinal; h: cardinal): integer;
    cdecl; external av__codec;
  function avcodec_default_get_format(s: PAVCodecContext; const fmt: PAVPixelFormat): TAVPixelFormat;
    cdecl; external av__codec;

  function avcodec_thread_init (s: PAVCodecContext; thread_count: integer): integer;
    cdecl; external av__codec;
  procedure avcodec_thread_free (s: PAVCodecContext);
    cdecl; external av__codec;
  function avcodec_thread_execute (s: PAVCodecContext; func: pointer; arg: PPointer; ret: Pinteger; count: integer): integer;
    cdecl; external av__codec;
  function avcodec_default_execute (s: PAVCodecContext; func: pointer; arg: PPointer; ret: Pinteger; count: integer): integer;
    cdecl; external av__codec;


//FIXME func typedef

(**
 * opens / inits the AVCodecContext.
 * not thread save!
 *)
  function avcodec_open (avctx: PAVCodecContext; codec: PAVCodec): integer;
    cdecl; external av__codec;


(**
 * Decode an audio frame.
 *
 * @param avctx the codec context.
 * @param samples output buffer, 16 byte aligned
 * @param frame_size_ptr the output buffer size in bytes, zero if no frame could be compressed
 * @param buf input buffer, 16 byte aligned
 * @param buf_size the input buffer size
 * @return 0 if successful, -1 if not.
 *)

(** This comment was added by the Creative CAT.  frame_size_ptr was changed to
 variable refference.
 
 * @deprecated Use avcodec_decode_audio2() instead.
 *)

  function avcodec_decode_audio (avctx: PAVCodecContext; samples: Pword;
                         		 var frame_size_ptr: integer;
                         		 buf: pchar; buf_size: integer): integer;
    cdecl; external av__codec;
(* decode a frame.
 * @param buf bitstream buffer, must be FF_INPUT_BUFFER_PADDING_SIZE larger then the actual read bytes
 * because some optimized bitstream readers read 32 or 64 bit at once and could read over the end
 * @param buf_size the size of the buffer in bytes
 * @param got_picture_ptr zero if no frame could be decompressed, Otherwise, it is non zero
 * @return -1 if error, otherwise return the number of
 * bytes used. *)

  function avcodec_decode_audio2(avctx : PAVCodecContext; samples : PWord;
								 var frame_size_ptr : integer;
                         		 buf: pchar; buf_size: integer): integer;
    cdecl; external av__codec;
(* Added by The Creative CAT
/**
 * Decodes a video frame from \p buf into \p picture.
 * The avcodec_decode_video() function decodes a video frame from the input
 * buffer \p buf of size \p buf_size. To decode it, it makes use of the
 * video codec which was coupled with \p avctx using avcodec_open(). The
 * resulting decoded frame is stored in \p picture.
 *
 * @warning The input buffer must be \c FF_INPUT_BUFFER_PADDING_SIZE larger than
 * the actual read bytes because some optimized bitstream readers read 32 or 64
 * bits at once and could read over the end.
 *
 * @warning The end of the input buffer \p buf should be set to 0 to ensure that
 * no overreading happens for damaged MPEG streams.
 *
 * @note You might have to align the input buffer \p buf and output buffer \p
 * samples. The alignment requirements depend on the CPU: on some CPUs it isn't
 * necessary at all, on others it won't work at all if not aligned and on others
 * it will work but it will have an impact on performance. In practice, the
 * bitstream should have 4 byte alignment at minimum and all sample data should
 * be 16 byte aligned unless the CPU doesn't need it (AltiVec and SSE do). If
 * the linesize is not a multiple of 16 then there's no sense in aligning the
 * start of the buffer to 16.
 *
 * @param avctx the codec context
 * @param[out] picture The AVFrame in which the decoded video frame will be stored.
 * @param[in] buf the input buffer
 * @param[in] buf_size the size of the input buffer in bytes
 * @param[in,out] got_picture_ptr Zero if no frame could be decompressed, otherwise, it is nonzero.
 * @return On error a negative value is returned, otherwise the number of bytes
 * used or zero if no frame could be decompressed.
 */
*)

  function avcodec_decode_video (avctx: PAVCodecContext; picture: PAVFrame;
                         var got_picture_ptr: integer; (* favour of The Creative CAT *)
                         buf: PByte; buf_size: integer): integer;
    cdecl; external av__codec;

  function avcodec_decode_subtitle (avctx: PAVCodecContext; sub: PAVSubtitle;
                            got_sub_ptr: pinteger;
                            const buf: pchar; buf_size: integer): integer;
    cdecl; external av__codec;
  function avcodec_parse_frame (avctx: PAVCodecContext; pdata: PPointer;
                        data_size_ptr: pinteger;
                        buf: pchar; buf_size: integer): integer;
    cdecl; external av__codec;

  function avcodec_encode_audio (avctx: PAVCodecContext; buf: PByte;
                        buf_size: integer; const samples: PWord): integer;
    cdecl; external av__codec;

  (* avcodec_encode_video: -1 if error *)
  (* type of the second argument is changed by The Creative CAT *)
  function avcodec_encode_video (avctx: PAVCodecContext; buf: PByte;
                        buf_size: integer; pict: PAVFrame): integer;
    cdecl; external av__codec;
  function avcodec_encode_subtitle (avctx: PAVCodecContext; buf: pchar;
                        buf_size: integer; const sub: PAVSubtitle): integer;
    cdecl; external av__codec;
  function avcodec_close (avctx: PAVCodecContext): integer;
    cdecl; external av__codec;

  procedure avcodec_register_all ();
    cdecl; external av__codec;

  procedure avcodec_flush_buffers (avctx: PAVCodecContext);
    cdecl; external av__codec;
  procedure avcodec_default_free_buffers (s: PAVCodecContext);
    cdecl; external av__codec;

(* misc usefull functions *)

(**
 * returns a single letter to describe the picture type
 *)
  function av_get_pict_type_char (pict_type: integer): char;
    cdecl; external av__codec;


(**
 * returns codec bits per sample
 *)
function av_get_bits_per_sample (codec_id: TCodecID): integer;
  cdecl; external av__codec;

const
  AV_PARSER_PTS_NB      = 4;
  PARSER_FLAG_COMPLETE_FRAMES = $0001;

type
  PAVCodecParserContext = ^TAVCodecParserContext;
  PAVCodecParser = ^TAVCodecParser;

  TAVCodecParserContext = record
    priv_data: pointer;
    parser: PAVCodecParser;
    frame_offset: int64; (* offset of the current frame *)
    cur_offset: int64; (* current offset (incremented by each av_parser_parse()) *)
    last_frame_offset: int64; (* offset of the last frame *)
    (* video info *)
    pict_type: integer; (* XXX: put it back in AVCodecContext *)
    repeat_pict: integer; (* XXX: put it back in AVCodecContext *)
    pts: int64;     (* pts of the current frame *)
    dts: int64;     (* dts of the current frame *)

    (* private data *)
    last_pts: int64;
    last_dts: int64;
    fetch_timestamp: integer;

    cur_frame_start_index: integer;
    cur_frame_offset: array [0..AV_PARSER_PTS_NB - 1] of int64;
    cur_frame_pts: array [0..AV_PARSER_PTS_NB - 1] of int64;
    cur_frame_dts: array [0..AV_PARSER_PTS_NB - 1] of int64;

    flags: integer;
  end;

  TAVCodecParser = record
    codec_ids: array [0..4] of integer; (* several codec IDs are permitted *)
    priv_data_size: integer;
    parser_init: function (s: PAVCodecParserContext): integer; cdecl;
    parser_parse: function (s: PAVCodecParserContext; avctx: PAVCodecContext;
                            poutbuf: PPointer; poutbuf_size: PInteger;
                            const buf: pchar; buf_size: integer): integer; cdecl;
    parser_close: procedure (s: PAVCodecParserContext); cdecl;
    split: function (avctx: PAVCodecContext; const buf: pchar;
                     buf_size: integer): integer; cdecl;
    next: PAVCodecParser;
  end;

  procedure av_register_codec_parser (parser: PAVCodecParser); cdecl;
    cdecl; external av__codec;

  function av_parser_init (codec_id: integer): PAVCodecParserContext;
    cdecl; external av__codec;

  function av_parser_parse (s: PAVCodecParserContext;
                    avctx: PAVCodecContext;
                    poutbuf: PPointer; poutbuf_size: pinteger;
                    const buf: pchar; buf_size: integer;
                    pts: int64; dts: int64): integer;
    cdecl; external av__codec;
  function av_parser_change (s: PAVCodecParserContext;
                     avctx: PAVCodecContext;
                     poutbuf: PPointer; poutbuf_size: PInteger;
                     const buf: pchar; buf_size: integer; keyframe: integer): integer;
    cdecl; external av__codec;
  procedure av_parser_close (s: PAVCodecParserContext);
    cdecl; external av__codec;

type
  PAVBitStreamFilterContext = ^TAVBitStreamFilterContext;
  PAVBitStreamFilter = ^TAVBitStreamFilter;

  TAVBitStreamFilterContext = record
    priv_data: pointer;
    filter: PAVBitStreamFilter;
    parser: PAVCodecParserContext;
    next: PAVBitStreamFilterContext;
  end;

  TAVBitStreamFilter = record
    name: pchar;
    priv_data_size: integer;
    filter: function (bsfc: PAVBitStreamFilterContext;
                  avctx: PAVCodecContext; args: pchar;
                  poutbuf: PPointer; poutbuf_size: PInteger;
                  buf: PByte; buf_size: integer; keyframe: integer): integer; cdecl;
    next: PAVBitStreamFilter;
  end;

procedure av_register_bitstream_filter (bsf: PAVBitStreamFilter);
  cdecl; external av__codec;

function av_bitstream_filter_init (name: pchar): PAVBitStreamFilterContext;
  cdecl; external av__codec;

function av_bitstream_filter_filter (bsfc: PAVBitStreamFilterContext;
                               avctx: PAVCodecContext; args: pchar;
                               poutbuf: PPointer; poutbuf_size: PInteger;
                               buf: PByte; buf_size: integer; keyframe: integer): integer;
  cdecl; external av__codec;
procedure av_bitstream_filter_close (bsf: PAVBitStreamFilterContext);
  cdecl; external av__codec;


(* memory *)
  procedure av_fast_realloc (ptr: pointer; size: PCardinal; min_size: Cardinal);
    cdecl; external av__codec;
(* for static data only *)
(* call av_free_static to release all staticaly allocated tables *)
  procedure  av_free_static ();
    cdecl; external av__codec;

  procedure av_mallocz_static(size: cardinal);
    cdecl; external av__codec;

  procedure av_realloc_static(ptr: pointer; size: Cardinal);
    cdecl; external av__codec;

  procedure img_copy (dst: PAVPicture; const src: PAVPicture;
                      pix_fmt: TAVPixelFormat; width: integer; height: integer);
    cdecl; external av__codec;

  function img_crop (dst: PAVPicture; const src: PAVPicture;
             pix_fmt: TAVPixelFormat; top_band, left_band: integer): integer;
    cdecl; external av__codec;

  function img_pad (dst: PAVPicture; const src: PAVPicture; height, width: integer;
                    pix_fmt: TAVPixelFormat; padtop, padbottom, padleft, padright: integer;
                    color: PInteger): integer;
    cdecl; external av__codec;

implementation

end.
