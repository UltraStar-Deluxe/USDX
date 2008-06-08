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

(* This is a part of Pascal porting of ffmpeg.
 * Originally by Victor Zinetz for Delphi and Free Pascal on Windows.
 * For Mac OS X, some modifications were made by The Creative CAT, denoted as CAT
 * in the source codes *)

(*
 * Min. version: 51.16.0
 * Max. version: 51.56.0, revision 13019, Tue Apr 29 14:08:01 2008 UTC
 *)

unit avcodec;

{$IFDEF FPC}
  {$MODE DELPHI }
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
  avutil,
  rational,
  opt,
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  UConfig;

const
  (* Max. supported version by this header *)
  LIBAVCODEC_MAX_VERSION_MAJOR   = 51;
  LIBAVCODEC_MAX_VERSION_MINOR   = 56;
  LIBAVCODEC_MAX_VERSION_RELEASE = 0;
  LIBAVCODEC_MAX_VERSION = (LIBAVCODEC_MAX_VERSION_MAJOR * VERSION_MAJOR) +
                           (LIBAVCODEC_MAX_VERSION_MINOR * VERSION_MINOR) +
                           (LIBAVCODEC_MAX_VERSION_RELEASE * VERSION_RELEASE);

(* Check if linked version is supported *)
{$IF (LIBAVCODEC_VERSION > LIBAVCODEC_MAX_VERSION)}
  {$MESSAGE Warn 'Linked version of libavcodec may be unsupported!'}
{$IFEND}

const
  AV_NOPTS_VALUE: int64   = $8000000000000000;
  AV_TIME_BASE            = 1000000;
  AV_TIME_BASE_Q          : TAVRational = (num:1; den:AV_TIME_BASE); (* added by CAT *)

(**
 * Identifies the syntax and semantics of the bitstream.
 * The principle is roughly:
 * Two decoders with the same ID can decode the same streams.
 * Two encoders with the same ID can encode compatible streams.
 * There may be slight deviations from the principle due to implementation
 * details.
 *
 * If you add a codec ID to this list, add it so that
 * 1. no value of a existing codec ID changes (that would break ABI),
 * 2. it is as close as possible to similar codecs.
 *)
type
  TCodecID = (
    CODEC_ID_NONE,
    
    (* video codecs *)
    CODEC_ID_MPEG1VIDEO,
    CODEC_ID_MPEG2VIDEO, //* prefered ID for MPEG Video 1/2 decoding */
    CODEC_ID_MPEG2VIDEO_XVMC,
    CODEC_ID_H261,
    CODEC_ID_H263,
    CODEC_ID_RV10,
    CODEC_ID_RV20,
    CODEC_ID_MJPEG,
    CODEC_ID_MJPEGB,
    CODEC_ID_LJPEG,
    CODEC_ID_SP5X,
    CODEC_ID_JPEGLS,
    CODEC_ID_MPEG4,
    CODEC_ID_RAWVIDEO,
    CODEC_ID_MSMPEG4V1,
    CODEC_ID_MSMPEG4V2,
    CODEC_ID_MSMPEG4V3,
    CODEC_ID_WMV1,
    CODEC_ID_WMV2,
    CODEC_ID_H263P,
    CODEC_ID_H263I,
    CODEC_ID_FLV1,
    CODEC_ID_SVQ1,
    CODEC_ID_SVQ3,
    CODEC_ID_DVVIDEO,
    CODEC_ID_HUFFYUV,
    CODEC_ID_CYUV,
    CODEC_ID_H264,
    CODEC_ID_INDEO3,
    CODEC_ID_VP3,
    CODEC_ID_THEORA,
    CODEC_ID_ASV1,
    CODEC_ID_ASV2,
    CODEC_ID_FFV1,
    CODEC_ID_4XM,
    CODEC_ID_VCR1,
    CODEC_ID_CLJR,
    CODEC_ID_MDEC,
    CODEC_ID_ROQ,
    CODEC_ID_INTERPLAY_VIDEO,
    CODEC_ID_XAN_WC3,
    CODEC_ID_XAN_WC4,
    CODEC_ID_RPZA,
    CODEC_ID_CINEPAK,
    CODEC_ID_WS_VQA,
    CODEC_ID_MSRLE,
    CODEC_ID_MSVIDEO1,
    CODEC_ID_IDCIN,
    CODEC_ID_8BPS,
    CODEC_ID_SMC,
    CODEC_ID_FLIC,
    CODEC_ID_TRUEMOTION1,
    CODEC_ID_VMDVIDEO,
    CODEC_ID_MSZH,
    CODEC_ID_ZLIB,
    CODEC_ID_QTRLE,
    CODEC_ID_SNOW,
    CODEC_ID_TSCC,
    CODEC_ID_ULTI,
    CODEC_ID_QDRAW,
    CODEC_ID_VIXL,
    CODEC_ID_QPEG,
    CODEC_ID_XVID,
    CODEC_ID_PNG,
    CODEC_ID_PPM,
    CODEC_ID_PBM,
    CODEC_ID_PGM,
    CODEC_ID_PGMYUV,
    CODEC_ID_PAM,
    CODEC_ID_FFVHUFF,
    CODEC_ID_RV30,
    CODEC_ID_RV40,
    CODEC_ID_VC1,
    CODEC_ID_WMV3,
    CODEC_ID_LOCO,
    CODEC_ID_WNV1,
    CODEC_ID_AASC,
    CODEC_ID_INDEO2,
    CODEC_ID_FRAPS,
    CODEC_ID_TRUEMOTION2,
    CODEC_ID_BMP,
    CODEC_ID_CSCD,
    CODEC_ID_MMVIDEO,
    CODEC_ID_ZMBV,
    CODEC_ID_AVS,
    CODEC_ID_SMACKVIDEO,
    CODEC_ID_NUV,
    CODEC_ID_KMVC,
    CODEC_ID_FLASHSV,
    CODEC_ID_CAVS,
    CODEC_ID_JPEG2000,
    CODEC_ID_VMNC,
    CODEC_ID_VP5,
    CODEC_ID_VP6,
    CODEC_ID_VP6F,
    CODEC_ID_TARGA,
    CODEC_ID_DSICINVIDEO,
    CODEC_ID_TIERTEXSEQVIDEO,
    CODEC_ID_TIFF,
    CODEC_ID_GIF,
    CODEC_ID_FFH264,
    CODEC_ID_DXA,
    CODEC_ID_DNXHD,
    CODEC_ID_THP,
    CODEC_ID_SGI,
    CODEC_ID_C93,
    CODEC_ID_BETHSOFTVID,
    CODEC_ID_PTX,
    CODEC_ID_TXD,
    CODEC_ID_VP6A,
    CODEC_ID_AMV,
    CODEC_ID_VB,
    CODEC_ID_PCX,
    CODEC_ID_SUNRAST,
    CODEC_ID_INDEO4,
    CODEC_ID_INDEO5,
    CODEC_ID_MIMIC,
    CODEC_ID_RL2,
    CODEC_ID_8SVX_EXP,
    CODEC_ID_8SVX_FIB,
    CODEC_ID_ESCAPE124,
    CODEC_ID_DIRAC,
    CODEC_ID_BFI,

    //* various PCM "codecs" */
    CODEC_ID_PCM_S16LE= $10000,
    CODEC_ID_PCM_S16BE,
    CODEC_ID_PCM_U16LE,
    CODEC_ID_PCM_U16BE,
    CODEC_ID_PCM_S8,
    CODEC_ID_PCM_U8,
    CODEC_ID_PCM_MULAW,
    CODEC_ID_PCM_ALAW,
    CODEC_ID_PCM_S32LE,
    CODEC_ID_PCM_S32BE,
    CODEC_ID_PCM_U32LE,
    CODEC_ID_PCM_U32BE,
    CODEC_ID_PCM_S24LE,
    CODEC_ID_PCM_S24BE,
    CODEC_ID_PCM_U24LE,
    CODEC_ID_PCM_U24BE,
    CODEC_ID_PCM_S24DAUD,
    CODEC_ID_PCM_ZORK,
    CODEC_ID_PCM_S16LE_PLANAR,

    //* various ADPCM codecs */
    CODEC_ID_ADPCM_IMA_QT= $11000,
    CODEC_ID_ADPCM_IMA_WAV,
    CODEC_ID_ADPCM_IMA_DK3,
    CODEC_ID_ADPCM_IMA_DK4,
    CODEC_ID_ADPCM_IMA_WS,
    CODEC_ID_ADPCM_IMA_SMJPEG,
    CODEC_ID_ADPCM_MS,
    CODEC_ID_ADPCM_4XM,
    CODEC_ID_ADPCM_XA,
    CODEC_ID_ADPCM_ADX,
    CODEC_ID_ADPCM_EA,
    CODEC_ID_ADPCM_G726,
    CODEC_ID_ADPCM_CT,
    CODEC_ID_ADPCM_SWF,
    CODEC_ID_ADPCM_YAMAHA,
    CODEC_ID_ADPCM_SBPRO_4,
    CODEC_ID_ADPCM_SBPRO_3,
    CODEC_ID_ADPCM_SBPRO_2,
    CODEC_ID_ADPCM_THP,
    CODEC_ID_ADPCM_IMA_AMV,
    CODEC_ID_ADPCM_EA_R1,
    CODEC_ID_ADPCM_EA_R3,
    CODEC_ID_ADPCM_EA_R2,
    CODEC_ID_ADPCM_IMA_EA_SEAD,
    CODEC_ID_ADPCM_IMA_EA_EACS,
    CODEC_ID_ADPCM_EA_XAS,
    CODEC_ID_ADPCM_EA_MAXIS_XA,

    //* AMR */
    CODEC_ID_AMR_NB= $12000,
    CODEC_ID_AMR_WB,

    //* RealAudio codecs*/
    CODEC_ID_RA_144= $13000,
    CODEC_ID_RA_288,

    //* various DPCM codecs */
    CODEC_ID_ROQ_DPCM= $14000,
    CODEC_ID_INTERPLAY_DPCM,
    CODEC_ID_XAN_DPCM,
    CODEC_ID_SOL_DPCM,

    (* audio codecs *)
    CODEC_ID_MP2= $15000,
    CODEC_ID_MP3, ///< preferred ID for decoding MPEG audio layer 1, 2 or 3
    CODEC_ID_AAC,
    {$IF LIBAVCODEC_VERSION < 52000000} // 52.0.0
    _CODEC_ID_MPEG4AAC, // will be redefined to CODEC_ID_AAC below
    {$IFEND}
    CODEC_ID_AC3,
    CODEC_ID_DTS,
    CODEC_ID_VORBIS,
    CODEC_ID_DVAUDIO,
    CODEC_ID_WMAV1,
    CODEC_ID_WMAV2,
    CODEC_ID_MACE3,
    CODEC_ID_MACE6,
    CODEC_ID_VMDAUDIO,
    CODEC_ID_SONIC,
    CODEC_ID_SONIC_LS,
    CODEC_ID_FLAC,
    CODEC_ID_MP3ADU,
    CODEC_ID_MP3ON4,
    CODEC_ID_SHORTEN,
    CODEC_ID_ALAC,
    CODEC_ID_WESTWOOD_SND1,
    CODEC_ID_GSM, ///< as in Berlin toast format
    CODEC_ID_QDM2,
    CODEC_ID_COOK,
    CODEC_ID_TRUESPEECH,
    CODEC_ID_TTA,
    CODEC_ID_SMACKAUDIO,
    CODEC_ID_QCELP,
    CODEC_ID_WAVPACK,
    CODEC_ID_DSICINAUDIO,
    CODEC_ID_IMC,
    CODEC_ID_MUSEPACK7,
    CODEC_ID_MLP,
    CODEC_ID_GSM_MS, { as found in WAV }
    CODEC_ID_ATRAC3,
    CODEC_ID_VOXWARE,
    CODEC_ID_APE,
    CODEC_ID_NELLYMOSER,
    CODEC_ID_MUSEPACK8,
    CODEC_ID_SPEEX,
    CODEC_ID_WMAVOICE,
    CODEC_ID_WMAPRO,
    CODEC_ID_WMALOSSLESS,

    //* subtitle codecs */
    CODEC_ID_DVD_SUBTITLE= $17000,
    CODEC_ID_DVB_SUBTITLE,
    CODEC_ID_TEXT,  ///< raw UTF-8 text
    CODEC_ID_XSUB,
    CODEC_ID_SSA,
    CODEC_ID_MOV_TEXT,

    (* other specific kind of codecs (generally used for attachments) *)
    CODEC_ID_TTF= $18000,
    
    CODEC_ID_MPEG2TS= $20000, {*< _FAKE_ codec to indicate a raw MPEG-2 TS
                              * stream (only used by libavformat) *}
    __CODEC_ID_4BYTE = $FFFFF  // ensure 4-byte enum
  );

{$IF LIBAVCODEC_VERSION < 52000000} // 52.0.0
{* CODEC_ID_MP3LAME is obsolete *}
const
  CODEC_ID_MP3LAME = CODEC_ID_MP3;
  CODEC_ID_MPEG4AAC = CODEC_ID_AAC;
{$IFEND}

type
  TCodecType = (
    CODEC_TYPE_UNKNOWN = -1,
    CODEC_TYPE_VIDEO,
    CODEC_TYPE_AUDIO,
    CODEC_TYPE_DATA,
    CODEC_TYPE_SUBTITLE,
    CODEC_TYPE_ATTACHMENT,
	  CODEC_TYPE_NB
  );

{**
 * currently unused, may be used if 24/32 bits samples ever supported */
 * all in native endian
 *}
type
  TSampleFormat = (
    SAMPLE_FMT_NONE = -1,
    SAMPLE_FMT_U8,              ///< unsigned 8 bits
    SAMPLE_FMT_S16,             ///< signed 16 bits
    SAMPLE_FMT_S24,             ///< signed 24 bits
    SAMPLE_FMT_S32,             ///< signed 32 bits
    SAMPLE_FMT_FLT              ///< float
  );

const
  {* in bytes *}
  AVCODEC_MAX_AUDIO_FRAME_SIZE = 192000; // 1 second of 48khz 32bit audio

{**
 * Required number of additionally allocated bytes at the end of the input bitstream for decoding.
 * This is mainly needed because some optimized bitstream readers read
 * 32 or 64 bit at once and could read over the end.<br>
 * Note: If the first 23 bits of the additional bytes are not 0, then damaged
 * MPEG bitstreams could cause overread and segfault.
 *}
  FF_INPUT_BUFFER_PADDING_SIZE = 8;

{**
 * minimum encoding buffer size.
 * Used to avoid some checks during header writing.
 *}
  FF_MIN_BUFFER_SIZE = 16384;

type
{*
 * motion estimation type.
 *}
  TMotion_Est_ID = (
    ME_ZERO = 1,  ///< no search, that is use 0,0 vector whenever one is needed
    ME_FULL,
    ME_LOG,
    ME_PHODS,
    ME_EPZS,      ///< enhanced predictive zonal search
    ME_X1,        ///< reserved for experiments
    ME_HEX,       ///< hexagon based search
    ME_UMH,       ///< uneven multi-hexagon search
    ME_ITER       ///< iterative search
  );

  TAVDiscard = (
    {* We leave some space between them for extensions (drop some
     * keyframes for intra-only or drop just some bidir frames). *}
    AVDISCARD_NONE   =-16, ///< discard nothing
    AVDISCARD_DEFAULT=  0, ///< discard useless packets like 0 size packets in avi
    AVDISCARD_NONREF =  8, ///< discard all non reference
    AVDISCARD_BIDIR  = 16, ///< discard all bidirectional frames
    AVDISCARD_NONKEY = 32, ///< discard all frames except keyframes
    AVDISCARD_ALL    = 48  ///< discard all
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

{* encoding support
   These flags can be passed in AVCodecContext.flags before initialization.
   Note: Not everything is supported yet.
*}

  CODEC_FLAG_QSCALE = $0002;  ///< Use fixed qscale.
  CODEC_FLAG_4MV    = $0004;  ///< 4 MV per MB allowed / advanced prediction for H263.
  CODEC_FLAG_QPEL   = $0010;  ///< use qpel MC.
  CODEC_FLAG_GMC    = $0020;  ///< use GMC.
  CODEC_FLAG_MV0    = $0040;  ///< always try a MB with MV=<0,0>.
  CODEC_FLAG_PART   = $0080;  ///< Use data partitioning.
  {**
   * The parent program guarantees that the input for B-frames containing
   * streams is not written to for at least s->max_b_frames+1 frames, if
   * this is not set the input will be copied.
   *}
  CODEC_FLAG_INPUT_PRESERVED    = $0100;
  CODEC_FLAG_PASS1              = $0200; ///< use internal 2pass ratecontrol in first  pass mode
  CODEC_FLAG_PASS2              = $0400; ///< use internal 2pass ratecontrol in second pass mode
  CODEC_FLAG_EXTERN_HUFF        = $1000; ///< use external huffman table (for mjpeg)
  CODEC_FLAG_GRAY               = $2000; ///< only decode/encode grayscale
  CODEC_FLAG_EMU_EDGE           = $4000; ///< don't draw edges
  CODEC_FLAG_PSNR               = $8000; ///< error[?] variables will be set during encoding
  CODEC_FLAG_TRUNCATED      = $00010000; //** input bitstream might be truncated at a random location instead
                                   //         of only at frame boundaries */
  CODEC_FLAG_NORMALIZE_AQP  = $00020000; ///< normalize adaptive quantization
  CODEC_FLAG_INTERLACED_DCT = $00040000; ///< use interlaced dct
  CODEC_FLAG_LOW_DELAY      = $00080000; ///< force low delay
  CODEC_FLAG_ALT_SCAN       = $00100000; ///< use alternate scan
  CODEC_FLAG_TRELLIS_QUANT  = $00200000; ///< use trellis quantization
  CODEC_FLAG_GLOBAL_HEADER  = $00400000; ///< place global headers in extradata instead of every keyframe
  CODEC_FLAG_BITEXACT       = $00800000; ///< use only bitexact stuff (except (i)dct)
  {* Fx : Flag for h263+ extra options *}
  {$IF LIBAVCODEC_VERSION < 52000000} // 52.0.0
  CODEC_FLAG_H263P_AIC      = $01000000; ///< H263 Advanced intra coding / MPEG4 AC prediction (remove this)
  {$IFEND}
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
  CODEC_FLAG2_DROP_FRAME_TIMECODE = $00002000; ///< timecode is in drop frame format.
  CODEC_FLAG2_SKIP_RD       = $00004000; ///< RD optimal MB level residual skipping
  CODEC_FLAG2_CHUNKS        = $00008000; ///< Input bitstream might be truncated at a packet boundaries instead of only at frame boundaries.
  CODEC_FLAG2_NON_LINEAR_QUANT = $00010000; ///< Use MPEG-2 nonlinear quantizer.
  CODEC_FLAG2_BIT_RESERVOIR = $00020000; ///< Use a bit reservoir when encoding if possible

(* Unsupported options :
 *              Syntax Arithmetic coding (SAC)
 *              Reference Picture Selection
 *              Independant Segment Decoding *)
(* /Fx *)
(* codec capabilities *)

const
  CODEC_CAP_DRAW_HORIZ_BAND = $0001; ///< decoder can use draw_horiz_band callback
  (**
   * Codec uses get_buffer() for allocating buffers.
   * direct rendering method 1
   *)
  CODEC_CAP_DR1             = $0002;
  (* if 'parse_only' field is true, then avcodec_parse_frame() can be used *)
  CODEC_CAP_PARSE_ONLY      = $0004;
  CODEC_CAP_TRUNCATED       = $0008;
  (* codec can export data for HW decoding (XvMC) *)
  CODEC_CAP_HWACCEL         = $0010;
  (**
   * codec has a non zero delay and needs to be feeded with NULL at the end to get the delayed data.
   * if this is not set, the codec is guranteed to never be feeded with NULL data
   *)
  CODEC_CAP_DELAY           = $0020;
  (**
   * Codec can be fed a final frame with a smaller size.
   * This can be used to prevent truncation of the last audio samples.
   *)
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
 * This specifies the area which should be displayed.
 * Note there may be multiple such areas for one frame.
 *)
  PAVPanScan = ^TAVPanScan;
  TAVPanScan = record {24}
    (*** id.
     * - encoding: set by user.
     * - decoding: set by libavcodec. *)
    id: integer;

    (*** width and height in 1/16 pel
     * - encoding: set by user.
     * - decoding: set by libavcodec. *)
    width: integer;
    height: integer;

    (*** position of the top left corner in 1/16 pel for up to 3 fields/frames.
     * - encoding: set by user.
     * - decoding: set by libavcodec. *)
    position: array [0..2] of array [0..1] of smallint;
  end;

const
  FF_QSCALE_TYPE_MPEG1	= 0;
  FF_QSCALE_TYPE_MPEG2	= 1;
  FF_QSCALE_TYPE_H264   = 2;

  FF_BUFFER_TYPE_INTERNAL = 1;
  FF_BUFFER_TYPE_USER     = 2; ///< Direct rendering buffers (image is (de)allocated by user)
  FF_BUFFER_TYPE_SHARED   = 4; ///< buffer from somewhere else, don't dealloc image (data/base), all other tables are not shared
  FF_BUFFER_TYPE_COPY     = 8; ///< just a (modified) copy of some other buffer, don't dealloc anything.


  FF_I_TYPE  = 1; ///< Intra
  FF_P_TYPE  = 2; ///< Predicted
  FF_B_TYPE  = 3; ///< Bi-dir predicted
  FF_S_TYPE  = 4; ///< S(GMC)-VOP MPEG4
  FF_SI_TYPE = 5; ///< Switching Intra
  FF_SP_TYPE = 6; ///< Switching Predicted
  FF_BI_TYPE = 7;

  FF_BUFFER_HINTS_VALID    = $01; // Buffer hints value is meaningful (if 0 ignore)
  FF_BUFFER_HINTS_READABLE = $02; // Codec will read from buffer
  FF_BUFFER_HINTS_PRESERVE = $04; // User must not alter buffer content
  FF_BUFFER_HINTS_REUSABLE = $08; // Codec will reuse the buffer (update)

type
  {**
   * Audio Video Frame.
   * New fields can be added to the end of FF_COMMON_FRAME with minor version
   * bumps.
   * Removal, reordering and changes to existing fields require a major
   * version bump. No fields should be added into AVFrame before or after
   * FF_COMMON_FRAME!
   * sizeof(AVFrame) must not be used outside libav*.
   *}
  PAVFrame = ^TAVFrame;
  TAVFrame = record {200}
    (**
     * pointer to the picture planes.
     * This might be different from the first allocated byte
     * - encoding:
     * - decoding:
     *)
    data: array [0..3] of pbyte;
    linesize: array [0..3] of integer;
    (**
     * pointer to the first allocated byte of the picture. Can be used in get_buffer/release_buffer.
     * This isn't used by libavcodec unless the default get/release_buffer() is used.
     * - encoding:
     * - decoding:
     *)
    base: array [0..3] of pbyte;
    (**
     * 1 -> keyframe, 0-> not
     * - encoding: Set by libavcodec.
     * - decoding: Set by libavcodec.
     *)
    key_frame: integer;
    (**
     * Picture type of the frame, see ?_TYPE below.
     * - encoding: Set by libavcodec. for coded_picture (and set by user for input).
     * - decoding: Set by libavcodec.
     *)
    pict_type: integer;
    (**
     * presentation timestamp in time_base units (time when frame should be shown to user)
     * If AV_NOPTS_VALUE then frame_rate = 1/time_base will be assumed.
     * - encoding: MUST be set by user.
     * - decoding: Set by libavcodec.
     *)
    pts: int64;
    (**\
     * picture number in bitstream order
     * - encoding: set by
     * - decoding: Set by libavcodec.
     *)
    coded_picture_number: integer;
    (**
     * picture number in display order
     * - encoding: set by
     * - decoding: Set by libavcodec.
     *)
    display_picture_number: integer;
    (**
     * quality (between 1 (good) and FF_LAMBDA_MAX (bad))
     * - encoding: Set by libavcodec. for coded_picture (and set by user for input).
     * - decoding: Set by libavcodec.
     *)
    quality: integer;
    (**
     * buffer age (1->was last buffer and dint change, 2->..., ...).
     * Set to INT_MAX if the buffer has not been used yet.
     * - encoding: unused
     * - decoding: MUST be set by get_buffer().
     *)
    age: integer;
    (**
     * is this picture used as reference
     * The values for this are the same as the MpegEncContext.picture_structure
     * variable, that is 1->top field, 2->bottom field, 3->frame/both fields.
     * - encoding: unused
     * - decoding: Set by libavcodec. (before get_buffer() call)).
     *)
    reference: integer;
    (**
     * QP table
     * - encoding: unused
     * - decoding: Set by libavcodec.
     *)
    qscale_table: PShortint;
    (**
     * QP store stride
     * - encoding: unused
     * - decoding: Set by libavcodec.
     *)
    qstride: integer;
    (**
     * mbskip_table[mb]>=1 if MB didn't change
     * stride= mb_width = (width+15)>>4
     * - encoding: unused
     * - decoding: Set by libavcodec.
     *)
    mbskip_table: pbyte;
    (**
     * motion vector table
     * @code
     * example:
     * int mv_sample_log2= 4 - motion_subsample_log2;
     * int mb_width= (width+15)>>4;
     * int mv_stride= (mb_width << mv_sample_log2) + 1;
     * motion_val[direction][x + y*mv_stride][0->mv_x, 1->mv_y];
     * @endcode
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    //int16_t (*motion_val[2])[2];
    motion_val: array [0..1] of pointer;
    (**
     * macroblock type table
     * mb_type_base + mb_width + 2
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    mb_type: PCardinal;
    (**
     * log2 of the size of the block which a single vector in motion_val represents:
     * (4->16x16, 3->8x8, 2-> 4x4, 1-> 2x2)
     * - encoding: unused
     * - decoding: Set by libavcodec.
     *)
    motion_subsample_log2: byte;
    (**
     * for some private data of the user
     * - encoding: unused
     * - decoding: Set by user.
     *)
    opaque: pointer;
    (**
     * error
     * - encoding: Set by libavcodec. if flags&CODEC_FLAG_PSNR.
     * - decoding: unused
     *)
    error: array [0..3] of uint64;
    (**
     * type of the buffer (to keep track of who has to deallocate data[*])
     * - encoding: Set by the one who allocates it.
     * - decoding: Set by the one who allocates it.
     * Note: User allocated (direct rendering) & internal buffers cannot coexist currently.
     *)
    type_: integer;
    (**
     * When decoding, this signals how much the picture must be delayed.
     * extra_delay = repeat_pict / (2*fps)
     * - encoding: unused
     * - decoding: Set by libavcodec.
     *)
    repeat_pict: integer;
    (**
     *
     *)
    qscale_type: integer;
    (**
     * The content of the picture is interlaced.
     * - encoding: Set by user.
     * - decoding: Set by libavcodec. (default 0)
     *)
    interlaced_frame: integer;
    (**
     * If the content is interlaced, is top field displayed first.
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    top_field_first: integer;
    (**
     * Pan scan.
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    pan_scan: PAVPanScan;
    (**
     * Tell user application that palette has changed from previous frame.
     * - encoding: ??? (no palette-enabled encoder yet)
     * - decoding: Set by libavcodec. (default 0).
     *)
    palette_has_changed: integer;
    (**
     * codec suggestion on buffer type if != 0
     * - encoding: unused
     * - decoding: Set by libavcodec. (before get_buffer() call)).
     *)
    buffer_hints: integer;
    (**
     * DCT coefficients
     * - encoding: unused
     * - decoding: Set by libavcodec.
     *)
    dct_coeff: PsmallInt;
    (**
     * motion referece frame index
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    ref_index: array [0..1] of PShortint;
  end;

const
  DEFAULT_FRAME_RATE_BASE = 1001000;

  FF_ASPECT_EXTENDED = 15;

  FF_RC_STRATEGY_XVID = 1;

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
  //FF_BUG_FAKE_SCALABILITY = 16 //Autodetection should work 100%.

  FF_COMPLIANCE_VERY_STRICT   =  2; ///< strictly conform to a older more strict version of the spec or reference software
  FF_COMPLIANCE_STRICT        =  1; ///< strictly conform to all the things in the spec no matter what consequences
  FF_COMPLIANCE_NORMAL        =  0;
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
  FF_IDCT_CAVS         = 15;
  FF_IDCT_SIMPLEARMV5TE= 16;
  FF_IDCT_SIMPLEARMV6  = 17;
  FF_IDCT_SIMPLEVIS    = 18;
  FF_IDCT_WMV2         = 19;
  FF_IDCT_FAAN         = 20;

  FF_EC_GUESS_MVS   = 1;
  FF_EC_DEBLOCK     = 2;

  FF_MM_FORCE	      = $80000000; (* force usage of selected flags (OR) *)
  (* lower 16 bits - CPU features *)
  FF_MM_MMX	        = $0001; ///< standard MMX
  FF_MM_3DNOW	      = $0004; ///< AMD 3DNOW
  FF_MM_MMXEXT	    = $0002; ///< SSE integer functions or AMD MMX ext
  FF_MM_SSE	        = $0008; ///< SSE functions
  FF_MM_SSE2	      = $0010; ///< PIV SSE2 functions
  FF_MM_3DNOWEXT	  = $0020; ///< AMD 3DNowExt
  FF_MM_SSE3        = $0040; ///< Prescott SSE3 functions
  FF_MM_SSSE3       = $0080; ///< Conroe SSSE3 functions
  FF_MM_IWMMXT	    = $0100; ///< XScale IWMMXT

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

  FF_CMP_SAD    = 0;
  FF_CMP_SSE    = 1;
  FF_CMP_SATD   = 2;
  FF_CMP_DCT    = 3;
  FF_CMP_PSNR   = 4;
  FF_CMP_BIT    = 5;
  FF_CMP_RD     = 6;
  FF_CMP_ZERO   = 7;
  FF_CMP_VSAD   = 8;
  FF_CMP_VSSE   = 9;
  FF_CMP_NSSE   = 10;
  FF_CMP_W53    = 11;
  FF_CMP_W97    = 12;
  FF_CMP_DCTMAX = 13;
  FF_CMP_DCT264 = 14;
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

  FF_CODER_TYPE_VLC     = 0;
  FF_CODER_TYPE_AC      = 1;
  FF_CODER_TYPE_RAW     = 2;
  FF_CODER_TYPE_RLE     = 3;
  FF_CODER_TYPE_DEFLATE = 4;

  SLICE_FLAG_CODED_ORDER    = $0001; ///< draw_horiz_band() is called in coded order instead of display
  SLICE_FLAG_ALLOW_FIELD    = $0002; ///< allow draw_horiz_band() with field slices (MPEG2 field pics)
  SLICE_FLAG_ALLOW_PLANE    = $0004; ///< allow draw_horiz_band() with 1 component at a time (SVQ1)

  FF_MB_DECISION_SIMPLE = 0;        ///< uses mb_cmp
  FF_MB_DECISION_BITS   = 1;        ///< chooses the one which needs the fewest bits
  FF_MB_DECISION_RD     = 2;        ///< rate distortion

  FF_AA_AUTO    = 0;
  FF_AA_FASTINT = 1; //not implemented yet
  FF_AA_INT     = 2;
  FF_AA_FLOAT   = 3;

  FF_PROFILE_UNKNOWN  = -99;
  FF_PROFILE_AAC_MAIN = 0;
  FF_PROFILE_AAC_LOW  = 1;
  FF_PROFILE_AAC_SSR  = 2;
  FF_PROFILE_AAC_LTP  = 3;

  FF_LEVEL_UNKNOWN    = -99;

  X264_PART_I4X4 = $001;  (* Analyse i4x4 *)
  X264_PART_I8X8 = $002;  (* Analyse i8x8 (requires 8x8 transform) *)
  X264_PART_P8X8 = $010;  (* Analyse p16x8, p8x16 and p8x8 *)
  X264_PART_P4X4 = $020;  (* Analyse p8x4, p4x8, p4x4 *)
  X264_PART_B8X8 = $100;  (* Analyse b16x8, b8x16 and b8x8 *)

  FF_COMPRESSION_DEFAULT = -1;

const
  AVPALETTE_SIZE = 1024;
  AVPALETTE_COUNT = 256;

type
(**
 * AVPaletteControl
 * This structure defines a method for communicating palette changes
 * between and demuxer and a decoder.
 *
 * @deprecated Use AVPacket to send palette changes instead.
 * This is totally broken.
 *)
  PAVPaletteControl = ^TAVPaletteControl;
  TAVPaletteControl = record
    (* demuxer sets this to 1 to indicate the palette has changed;
     * decoder resets to 0 *)
    palette_changed: integer;

    (* 4-byte ARGB palette entries, stored in native byte order; note that
     * the individual palette components should be on a 8-bit scale; if
     * the palette data comes from a IBM VGA native format, the component
     * data is probably 6 bits in size and needs to be scaled *)
    palette: array [0..AVPALETTE_COUNT - 1] of cardinal;
  end; {deprecated;}

type
  PAVClass = ^TAVClass; {const}
  PAVCodecContext = ^TAVCodecContext;

  PAVCodec = ^TAVCodec;

  // int[4]
  PQuadIntArray = ^TQuadIntArray;
  TQuadIntArray = array[0..3] of integer;
  // int (*func)(struct AVCodecContext *c2, void *arg)
  TExecuteFunc = function(c2: PAVCodecContext; arg: Pointer): integer; cdecl;

  TAVClass = record {12}
    class_name: pchar;
    (* actually passing a pointer to an AVCodecContext
					or AVFormatContext, which begin with an AVClass.
					Needed because av_log is in libavcodec and has no visibility
					of AVIn/OutputFormat *)
    item_name: function (): pchar; cdecl;
    option: PAVOption;
  end;

  (**
   * main external API structure.
   * New fields can be added to the end with minor version bumps.
   * Removal, reordering and changes to existing fields require a major
   * version bump.
   * sizeof(AVCodecContext) must not be used outside libav*.
   *)
  TAVCodecContext = record {720}
    (**
     * information on struct for av_log
     * - set by avcodec_alloc_context
     *)
    av_class: PAVClass;
    (**
     * the average bitrate
     * - encoding: Set by user; unused for constant quantizer encoding.
     * - decoding: Set by libavcodec. 0 or some bitrate if this info is available in the stream.
     *)
    bit_rate: integer;

    (**
     * number of bits the bitstream is allowed to diverge from the reference.
     *           the reference can be CBR (for CBR pass1) or VBR (for pass2)
     * - encoding: Set by user; unused for constant quantizer encoding.
     * - decoding: unused
     *)
    bit_rate_tolerance: integer;

    (**
     * CODEC_FLAG_*.
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    flags: integer;

    (**
     * Some codecs need additional format info. It is stored here.
     * If any muxer uses this then ALL demuxers/parsers AND encoders for the
     * specific codec MUST set it correctly otherwise stream copy breaks.
     * In general use of this field by muxers is not recommanded.
     * - encoding: Set by libavcodec.
     * - decoding: Set by libavcodec. (FIXME: Is this OK?)
     *)
    sub_id: integer;

    (**
     * Motion estimation algorithm used for video coding.
     * 1 (zero), 2 (full), 3 (log), 4 (phods), 5 (epzs), 6 (x1), 7 (hex),
     * 8 (umh), 9 (iter) [7, 8 are x264 specific, 9 is snow specific]
     * - encoding: MUST be set by user.
     * - decoding: unused
     *)
    me_method: integer;

    (**
     * some codecs need / can use extradata like Huffman tables.
     * mjpeg: Huffman tables
     * rv10: additional flags
     * mpeg4: global headers (they can be in the bitstream or here)
     * The allocated memory should be FF_INPUT_BUFFER_PADDING_SIZE bytes larger
     * than extradata_size to avoid prolems if it is read with the bitstream reader.
     * The bytewise contents of extradata must not depend on the architecture or CPU endianness.
     * - encoding: Set/allocated/freed by libavcodec.
     * - decoding: Set/allocated/freed by user.
     *)
    extradata: pbyte;
    extradata_size: integer;

    (**
     * This is the fundamental unit of time (in seconds) in terms
     * of which frame timestamps are represented. For fixed-fps content,
     * timebase should be 1/framerate and timestamp increments should be
     * identically 1.
     * - encoding: MUST be set by user.
     * - decoding: Set by libavcodec.
     *)
    time_base: TAVRational;

    (* video only *)
    (**
     * picture width / height.
     * - encoding: MUST be set by user.
     * - decoding: Set by libavcodec.
     * Note: For compatibility it is possible to set this instead of
     * coded_width/height before decoding.
     *)
    width, height: integer;

    (**
     * the number of pictures in a group of pictures, or 0 for intra_only
     * - encoding: Set by user.
     * - decoding: unused
     *)
    gop_size: integer;

    (**
     * Pixel format, see PIX_FMT_xxx.
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    pix_fmt: TAVPixelFormat;

    (**
     * Frame rate emulation. If not zero, the lower layer (i.e. format handler)
     * has to read frames at native frame rate.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    rate_emu: integer;

    (**
     * If non NULL, 'draw_horiz_band' is called by the libavcodec
     * decoder to draw a horizontal band. It improves cache usage. Not
     * all codecs can do that. You must check the codec capabilities
     * beforehand.
     * - encoding: unused
     * - decoding: Set by user.
     * @param height the height of the slice
     * @param y the y position of the slice
     * @param type 1->top field, 2->bottom field, 3->frame
     * @param offset offset into the AVFrame.data from which the slice should be read
     *)
    draw_horiz_band: procedure (s: PAVCodecContext;
                                {const} src: PAVFrame; offset: PQuadIntArray;
                                y: integer; type_: integer; height: integer); cdecl;

    (* audio only *)
    sample_rate: integer; ///< samples per second
    channels: integer;    ///< number of audio channels
    
    (**
     * audio sample format
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    sample_fmt: TSampleFormat;  ///< sample format, currenly unused

    (* The following data should not be initialized. *)
    (**
     * Samples per packet, initialized when calling 'init'.
     *)
    frame_size: integer;
    frame_number: integer;   ///< audio or video frame number
    real_pict_num: integer;  ///< returns the real picture number of previous encoded frame

    (**
     * Number of frames the decoded output will be delayed relative to
     * the encoded input.
     * - encoding: Set by libavcodec.
     * - decoding: unused
     *)
    delay: integer;

    (* - encoding parameters *)
    qcompress: single;  ///< amount of qscale change between easy & hard scenes (0.0-1.0)
    qblur: single;      ///< amount of qscale smoothing over time (0.0-1.0)

    (**
     * minimum quantizer
     * - encoding: Set by user.
     * - decoding: unused
     *)
    qmin: integer;

   (**
     * maximum quantizer
     * - encoding: Set by user.
     * - decoding: unused
     *)
   qmax: integer;

    (**
     * maximum quantizer difference between frames
     * - encoding: Set by user.
     * - decoding: unused
     *)
    max_qdiff: integer;

    (**
     * maximum number of B-frames between non-B-frames
     * Note: The output will be delayed by max_b_frames+1 relative to the input.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    max_b_frames: integer;

    (**
     * qscale factor between IP and B-frames
     * - encoding: Set by user.
     * - decoding: unused
     *)
    b_quant_factor: single;

    (** obsolete FIXME remove *)
    rc_strategy: integer;

    b_frame_strategy: integer;

    (**
     * hurry up amount
     * - encoding: unused
     * - decoding: Set by user. 1-> Skip B-frames, 2-> Skip IDCT/dequant too, 5-> Skip everything except header
     * @deprecated Deprecated in favor of skip_idct and skip_frame.
     *)
    hurry_up: integer;

    codec: PAVCodec;

    priv_data: pointer;

    {$IF LIBAVCODEC_VERSION < 52000000} // 52.0.0
    (* unused, FIXME remove*)
    rtp_mode: integer;
    {$IFEND}

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

    (**
     * number of bits used for the previously encoded frame
     * - encoding: Set by libavcodec.
     * - decoding: unused
     *)
    frame_bits: integer;

    (**
     * Private data of the user, can be used to carry app specific stuff.
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    opaque: pointer;

    codec_name: array [0..31] of char;
    codec_type: TCodecType; (* see CODEC_TYPE_xxx *)
    codec_id: TCodecID; (* see CODEC_ID_xxx *)

    (**
     * fourcc (LSB first, so "ABCD" -> ('D'<<24) + ('C'<<16) + ('B'<<8) + 'A').
     * This is used to work around some encoder bugs.
     * A demuxer should set this to what is stored in the field used to identify the codec.
     * If there are multiple such fields in a container then the demuxer should choose the one
     * which maximizes the information about the used codec.
     * If the codec tag field in a container is larger then 32 bits then the demuxer should
     * remap the longer ID to 32 bits with a table or other structure. Alternatively a new
     * extra_codec_tag + size could be added but for this a clear advantage must be demonstrated
     * first.
     * - encoding: Set by user, if not then the default based on codec_id will be used.
     * - decoding: Set by user, will be converted to uppercase by libavcodec during init.
     *)
    codec_tag: cardinal;  // ����� array [0..3] of char - ����� ����� FOURCC

    (**
     * Work around bugs in encoders which sometimes cannot be detected automatically.
     * - encoding: Set by user
     * - decoding: Set by user
     *)
    workaround_bugs: integer;

    (**
     * luma single coefficient elimination threshold
     * - encoding: Set by user.
     * - decoding: unused
     *)
    luma_elim_threshold: integer;

    (**
     * chroma single coeff elimination threshold
     * - encoding: Set by user.
     * - decoding: unused
     *)
    chroma_elim_threshold: integer;

    (**
     * strictly follow the standard (MPEG4, ...).
     * - encoding: Set by user.
     * - decoding: unused
     *)
    strict_std_compliance: integer;

    (**
     * qscale offset between IP and B-frames
     * If > 0 then the last P-frame quantizer will be used (q= lastp_q*factor+offset).
     * If < 0 then normal ratecontrol will be done (q= -normal_q*factor+offset).
     * - encoding: Set by user.
     * - decoding: unused
     *)
    b_quant_offset: single;

    (**
     * Error resilience; higher values will detect more errors but may
     * misdetect some more or less valid parts as errors.
     * - encoding: unused
     * - decoding: Set by user.
     *)
    error_resilience: integer;

    (**
     * Called at the beginning of each frame to get a buffer for it.
     * If pic.reference is set then the frame will be read later by libavcodec.
     * avcodec_align_dimensions() should be used to find the required width and
     * height, as they normally need to be rounded up to the next multiple of 16.
     * - encoding: unused
     * - decoding: Set by libavcodec., user can override.
     *)
    get_buffer: function (c: PAVCodecContext; pic: PAVFrame): integer; cdecl;

    (**
     * Called to release buffers which were allocated with get_buffer.
     * A released buffer can be reused in get_buffer().
     * pic.data[*] must be set to NULL.
     * - encoding: unused
     * - decoding: Set by libavcodec., user can override.
     *)
    release_buffer: procedure (c: PAVCodecContext; pic: PAVFrame); cdecl;

    (**
     * If 1 the stream has a 1 frame delay during decoding.
     * - encoding: Set by libavcodec.
     * - decoding: Set by libavcodec.
     *)
    has_b_frames: integer;

    (**
     * number of bytes per packet if constant and known or 0
     * Used by some WAV based audio codecs.
     *)
    block_align: integer;

    parse_only: integer; (* - decoding only: if true, only parsing is done
                       (function avcodec_parse_frame()). The frame
                       data is returned. Only MPEG codecs support this now. *)

    (**
     * 0-> h263 quant 1-> mpeg quant
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mpeg_quant: integer;

    (**
     * pass1 encoding statistics output buffer
     * - encoding: Set by libavcodec.
     * - decoding: unused
     *)
    stats_out: pchar;

    (**
     * pass2 encoding statistics input buffer
     * Concatenated stuff from stats_out of pass1 should be placed here.
     * - encoding: Allocated/set/freed by user.
     * - decoding: unused
     *)
    stats_in: pchar;

    (**
     * ratecontrol qmin qmax limiting method
     * 0-> clipping, 1-> use a nice continous function to limit qscale wthin qmin/qmax.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    rc_qsquish: single;

    rc_qmod_amp: single;
    rc_qmod_freq: integer;

    (**
     * ratecontrol override, see RcOverride
     * - encoding: Allocated/set/freed by user.
     * - decoding: unused
     *)
    rc_override: PRcOverride;
    rc_override_count: integer;

    (**
     * rate control equation
     * - encoding: Set by user
     * - decoding: unused
     *)
    rc_eq: pchar; {const char*}

    (**
     * maximum bitrate
     * - encoding: Set by user.
     * - decoding: unused
     *)
    rc_max_rate: integer;

    (**
     * minimum bitrate
     * - encoding: Set by user.
     * - decoding: unused
     *)
    rc_min_rate: integer;

    (**
     * decoder bitstream buffer size
     * - encoding: Set by user.
     * - decoding: unused
     *)
    rc_buffer_size: integer;
    rc_buffer_aggressivity: single;

    (**
     * qscale factor between P and I-frames
     * If > 0 then the last p frame quantizer will be used (q= lastp_q*factor+offset).
     * If < 0 then normal ratecontrol will be done (q= -normal_q*factor+offset).
     * - encoding: Set by user.
     * - decoding: unused
     *)
    i_quant_factor: single;

    (**
     * qscale offset between P and I-frames
     * - encoding: Set by user.
     * - decoding: unused
     *)
    i_quant_offset: single;

    (**
     * initial complexity for pass1 ratecontrol
     * - encoding: Set by user.
     * - decoding: unused
     *)
    rc_initial_cplx: single;

    (**
     * DCT algorithm, see FF_DCT_* below
     * - encoding: Set by user.
     * - decoding: unused
     *)
    dct_algo: integer;

    (**
     * luminance masking (0-> disabled)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    lumi_masking: single;

    (**
     * temporary complexity masking (0-> disabled)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    temporal_cplx_masking: single;

    (**
     * spatial complexity masking (0-> disabled)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    spatial_cplx_masking: single;

    (**
     * p block masking (0-> disabled)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    p_masking: single;

    (**
     * darkness masking (0-> disabled)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    dark_masking: single;

    {$IF LIBAVCODEC_VERSION < 52000000} // 52.0.0
    (* for binary compatibility *)
    unused: integer;
    {$IFEND}

    (**
     * IDCT algorithm, see FF_IDCT_* below.
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    idct_algo: integer;

    (**
     * slice count
     * - encoding: Set by libavcodec.
     * - decoding: Set by user (or 0).
     *)
    slice_count: integer;

    (**
     * slice offsets in the frame in bytes
     * - encoding: Set/allocated by libavcodec.
     * - decoding: Set/allocated by user (or NULL).
     *)
    slice_offset: Pinteger;

    (**
     * error concealment flags
     * - encoding: unused
     * - decoding: Set by user.
     *)
    error_concealment: integer;

    (**
     * dsp_mask could be add used to disable unwanted CPU features
     * CPU features (i.e. MMX, SSE. ...)
     *
     * With the FORCE flag you may instead enable given CPU features.
     * (Dangerous: Usable in case of misdetection, improper usage however will
     * result into program crash.)
     *)
    dsp_mask: cardinal;

    (**
     * bits per sample/pixel from the demuxer (needed for huffyuv).
     * - encoding: Set by libavcodec.
     * - decoding: Set by user.
     *)
     bits_per_sample: integer;

    (**
     * prediction method (needed for huffyuv)
     * - encoding: Set by user.
     * - decoding: unused
     *)
     prediction_method: integer;

    (**
     * sample aspect ratio (0 if unknown)
     * Numerator and denominator must be relatively prime and smaller than 256 for some video standards.
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    sample_aspect_ratio: TAVRational;

    (**
     * the picture in the bitstream
     * - encoding: Set by libavcodec.
     * - decoding: Set by libavcodec.
     *)
    coded_frame: PAVFrame;

    (**
     * debug
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    debug: integer;

    (**
     * debug
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    debug_mv: integer;

    (**
     * error
     * - encoding: Set by libavcodec if flags&CODEC_FLAG_PSNR.
     * - decoding: unused
     *)
    error: array [0..3] of uint64;

    (**
     * minimum MB quantizer
     * - encoding: unused
     * - decoding: unused
     *)
    mb_qmin: integer;

    (**
     * maximum MB quantizer
     * - encoding: unused
     * - decoding: unused
     *)
    mb_qmax: integer;

    (**
     * motion estimation comparison function
     * - encoding: Set by user.
     * - decoding: unused
     *)
    me_cmp: integer;

    (**
     * subpixel motion estimation comparison function
     * - encoding: Set by user.
     * - decoding: unused
     *)
    me_sub_cmp: integer;
    (**
     * macroblock comparison function (not supported yet)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mb_cmp: integer;
    (**
     * interlaced DCT comparison function
     * - encoding: Set by user.
     * - decoding: unused
     *)
    ildct_cmp: integer;

    (**
     * ME diamond size & shape
     * - encoding: Set by user.
     * - decoding: unused
     *)
    dia_size: integer;

    (**
     * amount of previous MV predictors (2a+1 x 2a+1 square)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    last_predictor_count: integer;

    (**
     * prepass for motion estimation
     * - encoding: Set by user.
     * - decoding: unused
     *)
    pre_me: integer;

    (**
     * motion estimation prepass comparison function
     * - encoding: Set by user.
     * - decoding: unused
     *)
    me_pre_cmp: integer;

    (**
     * ME prepass diamond size & shape
     * - encoding: Set by user.
     * - decoding: unused
     *)
    pre_dia_size: integer;

    (**
     * subpel ME quality
     * - encoding: Set by user.
     * - decoding: unused
     *)
    me_subpel_quality: integer;

    (**
     * callback to negotiate the pixelFormat
     * @param fmt is the list of formats which are supported by the codec,
     * it is terminated by -1 as 0 is a valid format, the formats are ordered by quality.
     * The first is always the native one.
     * @return the chosen format
     * - encoding: unused
     * - decoding: Set by user, if not set the native format will be chosen.
     *)
    get_format: function (s: PAVCodecContext; {const} fmt: PAVPixelFormat): TAVPixelFormat; cdecl;

    (**
     * DTG active format information (additional aspect ratio
     * information only used in DVB MPEG-2 transport streams)
     * 0 if not set.
     *
     * - encoding: unused
     * - decoding: Set by decoder.
     *)
    dtg_active_format: integer;

    (**
     * maximum motion estimation search range in subpel units
     * If 0 then no limit.
     *
     * - encoding: Set by user.
     * - decoding: unused
     *)
    me_range: integer;

    (**
     * intra quantizer bias
     * - encoding: Set by user.
     * - decoding: unused
     *)
    intra_quant_bias: integer;

    (**
     * inter quantizer bias
     * - encoding: Set by user.
     * - decoding: unused
     *)
    inter_quant_bias: integer;

    (**
     * color table ID
     * - encoding: unused
     * - decoding: Which clrtable should be used for 8bit RGB images.
     *             Tables have to be stored somewhere. FIXME
     *)
    color_table_id: integer;

    (**
     * internal_buffer count
     * Don't touch, used by libavcodec default_get_buffer().
     *)
    internal_buffer_count: integer;

    (**
     * internal_buffers
     * Don't touch, used by libavcodec default_get_buffer().
     *)
    internal_buffer: pointer;

    (**
     * Global quality for codecs which cannot change it per frame.
     * This should be proportional to MPEG-1/2/4 qscale.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    global_quality: integer;

    (**
     * coder type
     * - encoding: Set by user.
     * - decoding: unused
     *)
    coder_type: integer;

    (**
     * context model
     * - encoding: Set by user.
     * - decoding: unused
     *)
    context_model: integer;

    {
    (**
     *
     * - encoding: unused
     * - decoding: Set by user.
     *)
    realloc: function (s: PAVCodecContext; buf: Pbyte; buf_size: integer): Pbyte; cdecl;
    }

    (**
     * slice flags
     * - encoding: unused
     * - decoding: Set by user.
     *)
    slice_flags: integer;

    (**
     * XVideo Motion Acceleration
     * - encoding: forbidden
     * - decoding: set by decoder
     *)
    xvmc_acceleration: integer;

    (**
     * macroblock decision mode
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mb_decision: integer;

    (**
     * custom intra quantization matrix
     * - encoding: Set by user, can be NULL.
     * - decoding: Set by libavcodec.
     *)
    intra_matrix: Pword;

    (**
     * custom inter quantization matrix
     * - encoding: Set by user, can be NULL.
     * - decoding: Set by libavcodec.
     *)
    inter_matrix: Pword;

    (**
     * fourcc from the AVI stream header (LSB first, so "ABCD" -> ('D'<<24) + ('C'<<16) + ('B'<<8) + 'A').
     * This is used to work around some encoder bugs.
     * - encoding: unused
     * - decoding: Set by user, will be converted to uppercase by libavcodec during init.
     *)
    stream_codec_tag: array [0..3] of char; //cardinal;

    (**
     * scene change detection threshold
     * 0 is default, larger means fewer detected scene changes.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    scenechange_threshold: integer;

    (**
     * minimum Lagrange multipler
     * - encoding: Set by user.
     * - decoding: unused
     *)
    lmin: integer;

    (**
     * maximum Lagrange multipler
     * - encoding: Set by user.
     * - decoding: unused
     *)
    lmax: integer;

    (**
     * palette control structure
     * - encoding: ??? (no palette-enabled encoder yet)
     * - decoding: Set by user.
     *)
    palctrl: PAVPaletteControl;

    (**
     * noise reduction strength
     * - encoding: Set by user.
     * - decoding: unused
     *)
    noise_reduction: integer;

    (**
     * Called at the beginning of a frame to get cr buffer for it.
     * Buffer type (size, hints) must be the same. libavcodec won't check it.
     * libavcodec will pass previous buffer in pic, function should return
     * same buffer or new buffer with old frame "painted" into it.
     * If pic.data[0] == NULL must behave like get_buffer().
     * - encoding: unused
     * - decoding: Set by libavcodec., user can override
     *)
    reget_buffer: function (c: PAVCodecContext; pic: PAVFrame): integer; cdecl;

    (**
     * Number of bits which should be loaded into the rc buffer before decoding starts.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    rc_initial_buffer_occupancy: integer;

    (**
     *
     * - encoding: Set by user.
     * - decoding: unused
     *)
    inter_threshold: integer;

    (**
     * CODEC_FLAG2_*
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    flags2: integer;

    (**
     * Simulates errors in the bitstream to test error concealment.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    error_rate: integer;

    (**
     * MP3 antialias algorithm, see FF_AA_* below.
     * - encoding: unused
     * - decoding: Set by user.
     *)
    antialias_algo: integer;

    (**
     * quantizer noise shaping
     * - encoding: Set by user.
     * - decoding: unused
     *)
    quantizer_noise_shaping: integer;

    (**
     * thread count
     * is used to decide how many independent tasks should be passed to execute()
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    thread_count: integer;

    (**
     * The codec may call this to execute several independent things.
     * It will return only after finishing all tasks.
     * The user may replace this with some multithreaded implementation,
     * the default implementation will execute the parts serially.
     * @param count the number of things to execute
     * - encoding: Set by libavcodec, user can override.
     * - decoding: Set by libavcodec, user can override.
     *)
    execute: function (c: PAVCodecContext; func: TExecuteFunc; arg: PPointer; ret: PInteger; count: integer): integer; cdecl;

    (**
     * thread opaque
     * Can be used by execute() to store some per AVCodecContext stuff.
     * - encoding: set by execute()
     * - decoding: set by execute()
     *)
    thread_opaque: pointer;

    (**
     * Motion estimation threshold below which no motion estimation is
     * performed, but instead the user specified motion vectors are used.
     *
     * - encoding: Set by user.
     * - decoding: unused
     *)
     me_threshold: integer;

    (**
     * Macroblock threshold below which the user specified macroblock types will be used.
     * - encoding: Set by user.
     * - decoding: unused
     *)
     mb_threshold: integer;

    (**
     * precision of the intra DC coefficient - 8
     * - encoding: Set by user.
     * - decoding: unused
     *)
     intra_dc_precision: integer;

    (**
     * noise vs. sse weight for the nsse comparsion function
     * - encoding: Set by user.
     * - decoding: unused
     *)
     nsse_weight: integer;

    (**
     * Number of macroblock rows at the top which are skipped.
     * - encoding: unused
     * - decoding: Set by user.
     *)
     skip_top: integer;

    (**
     * Number of macroblock rows at the bottom which are skipped.
     * - encoding: unused
     * - decoding: Set by user.
     *)
     skip_bottom: integer;

    (**
     * profile
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
     profile: integer;

    (**
     * level
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
     level: integer;

    (**
     * low resolution decoding, 1-> 1/2 size, 2->1/4 size
     * - encoding: unused
     * - decoding: Set by user.
     *)
     lowres: integer;

    (**
     * Bitstream width / height, may be different from width/height if lowres
     * or other things are used.
     * - encoding: unused
     * - decoding: Set by user before init if known. Codec should override / dynamically change if needed.
     *)
    coded_width, coded_height: integer;

    (**
     * frame skip threshold
     * - encoding: Set by user.
     * - decoding: unused
     *)
    frame_skip_threshold: integer;

    (**
     * frame skip factor
     * - encoding: Set by user.
     * - decoding: unused
     *)
    frame_skip_factor: integer;

    (**
     * frame skip exponent
     * - encoding: Set by user.
     * - decoding: unused
     *)
    frame_skip_exp: integer;

    (**
     * frame skip comparison function
     * - encoding: Set by user.
     * - decoding: unused
     *)
    frame_skip_cmp: integer;

    (**
     * Border processing masking, raises the quantizer for mbs on the borders
     * of the picture.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    border_masking: single;

    (**
     * minimum MB lagrange multipler
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mb_lmin: integer;

    (**
     * maximum MB lagrange multipler
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mb_lmax: integer;

    (**
     *
     * - encoding: Set by user.
     * - decoding: unused
     *)
    me_penalty_compensation: integer;

    (**
     *
     * - encoding: unused
     * - decoding: Set by user.
     *)
    skip_loop_filter: TAVDiscard;

    (**
     *
     * - encoding: unused
     * - decoding: Set by user.
     *)
    skip_idct: TAVDiscard;

    (**
     *
     * - encoding: unused
     * - decoding: Set by user.
     *)
    skip_frame: TAVDiscard;

    (**
     *
     * - encoding: Set by user.
     * - decoding: unused
     *)
    bidir_refine: integer;

    (**
     *
     * - encoding: Set by user.
     * - decoding: unused
     *)
    brd_scale: integer;

    (**
     * constant rate factor - quality-based VBR - values ~correspond to qps
     * - encoding: Set by user.
     * - decoding: unused
     *)
    {$IF LIBAVCODEC_VERSION >= 51021000} // 51.21.0
    crf: single;
    {$ELSE}
    crf: integer;
    {$IFEND}

    (**
     * constant quantization parameter rate control method
     * - encoding: Set by user.
     * - decoding: unused
     *)
    cqp: integer;

    (**
     * minimum GOP size
     * - encoding: Set by user.
     * - decoding: unused
     *)
    keyint_min: integer;

    (**
     * number of reference frames
     * - encoding: Set by user.
     * - decoding: unused
     *)
    refs: integer;

    (**
     * chroma qp offset from luma
     * - encoding: Set by user.
     * - decoding: unused
     *)
    chromaoffset: integer;

    (**
     * Influences how often B-frames are used.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    bframebias: integer;

    (**
     * trellis RD quantization
     * - encoding: Set by user.
     * - decoding: unused
     *)
    trellis: integer;

    (**
     * Reduce fluctuations in qp (before curve compression).
     * - encoding: Set by user.
     * - decoding: unused
     *)
    complexityblur: single;

    (**
     * in-loop deblocking filter alphac0 parameter
     * alpha is in the range -6...6
     * - encoding: Set by user.
     * - decoding: unused
     *)
    deblockalpha: integer;

    (**
     * in-loop deblocking filter beta parameter
     * beta is in the range -6...6
     * - encoding: Set by user.
     * - decoding: unused
     *)
    deblockbeta: integer;

    (**
     * macroblock subpartition sizes to consider - p8x8, p4x4, b8x8, i8x8, i4x4
     * - encoding: Set by user.
     * - decoding: unused
     *)
    partitions: integer;

    (**
     * direct MV prediction mode - 0 (none), 1 (spatial), 2 (temporal)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    directpred: integer;

    (**
     * Audio cutoff bandwidth (0 means "automatic")
     * - encoding: Set by user.
     * - decoding: unused
     *)
    cutoff: integer;

    (**
     * Multiplied by qscale for each frame and added to scene_change_score.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    scenechange_factor: integer;

    (**
     *
     * Note: Value depends upon the compare function used for fullpel ME.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mv0_threshold: integer;

    (**
     * Adjusts sensitivity of b_frame_strategy 1.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    b_sensitivity: integer;

    (**
     * - encoding: Set by user.
     * - decoding: unused
     *)
    compression_level: integer;

    (**
     * Sets whether to use LPC mode - used by FLAC encoder.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    use_lpc: integer;

    (**
     * LPC coefficient precision - used by FLAC encoder
     * - encoding: Set by user.
     * - decoding: unused
     *)
    lpc_coeff_precision: integer;

    (**
     * - encoding: Set by user.
     * - decoding: unused
     *)
    min_prediction_order: integer;

    (**
     * - encoding: Set by user.
     * - decoding: unused
     *)
    max_prediction_order: integer;

    (**
     * search method for selecting prediction order
     * - encoding: Set by user.
     * - decoding: unused
     *)
    prediction_order_method: integer;

    (**
     * - encoding: Set by user.
     * - decoding: unused
     *)
    min_partition_order: integer;

    (**
     * - encoding: Set by user.
     * - decoding: unused
     *)
    max_partition_order: integer;

    {$IF LIBAVCODEC_VERSION >= 51026000} // 51.26.0
    (**
     * GOP timecode frame start number, in non drop frame format
     * - encoding: Set by user.
     * - decoding: unused
     *)
    timecode_frame_start: int64;
    {$IFEND}

    {$IF LIBAVCODEC_VERSION >= 51042000} // 51.42.0
    (**
     * Decoder should decode to this many channels if it can (0 for default)
     * - encoding: unused
     * - decoding: Set by user.
     *)
    request_channels: integer;
    {$IFEND}

    {$IF LIBAVCODEC_VERSION > 51049000} // > 51.49.0
    (**
     * Percentage of dynamic range compression to be applied by the decoder.
     * The default value is 1.0, corresponding to full compression.
     * - encoding: unused
     * - decoding: Set by user.
     *)
    drc_scale: Single;
    {$IFEND}
  end;

(**
 * AVCodec.
 *)
  TAVCodec = record
    name: pchar;
    type_: TCodecType;
    id: TCodecID;
    priv_data_size: integer;
    init: function (avctx: PAVCodecContext): integer; cdecl; (* typo corretion by the Creative CAT *)
    encode: function (avctx: PAVCodecContext; buf: pchar; buf_size: integer; data: pointer): integer; cdecl;
    close: function (avctx: PAVCodecContext): integer; cdecl;
    decode: function (avctx: PAVCodecContext; outdata: pointer; outdata_size: PInteger;
                  {const} buf: pchar; buf_size: integer): integer; cdecl;
    (**
     * Codec capabilities.
     * see CODEC_CAP_*
     *)
    capabilities: integer;
    next: PAVCodec;
    (**
     * Flush buffers.
     * Will be called when seeking
     *)
    flush: procedure (avctx: PAVCodecContext); cdecl;
    {const} supported_framerates: PAVRational; ///< array of supported framerates, or NULL if any, array is terminated by {0,0}
    {const} pix_fmts: PAVPixelFormat;       ///< array of supported pixel formats, or NULL if unknown, array is terminated by -1
    {$IF LIBAVCODEC_VERSION >= 51055000} // 51.55.0
    {const} long_name: PChar;                  ///< descriptive name for the codec, meant to be more human readable than \p name
    {$IFEND}
    {$IF LIBAVCODEC_VERSION >= 51056000} // 51.56.0
    {const} supported_samplerates: PInteger;       ///< array of supported audio samplerates, or NULL if unknown, array is terminated by 0
    {$IFEND}
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

type
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

function audio_resample (s: PReSampleContext; output: PSmallint; input: PSmallint; nb_samples: integer): integer;
  cdecl; external av__codec;

procedure audio_resample_close (s: PReSampleContext);
  cdecl; external av__codec;


function av_resample_init (out_rate: integer; in_rate: integer; filter_length: integer;
                           log2_phase_count: integer; linear: integer; cutoff: double): PAVResampleContext;
  cdecl; external av__codec;

function av_resample (c: PAVResampleContext; dst: PSmallint; src: PSmallint; consumed: PInteger;
                      src_size: integer; dst_size: integer; update_ctx: integer): integer;
  cdecl; external av__codec;

procedure av_resample_compensate (c: PAVResampleContext; sample_delta: integer;
                                  compensation_distance: integer);
  cdecl; external av__codec;

procedure av_resample_close (c: PAVResampleContext);
  cdecl; external av__codec;


{$IF LIBAVCODEC_VERSION < 52000000} // 52.0.0
(* YUV420 format is assumed ! *)

(**
 * @deprecated Use the software scaler (swscale) instead.
 *)
function img_resample_init (output_width: integer; output_height: integer;
                           input_width: integer; input_height: integer): PImgReSampleContext;
  cdecl; external av__codec; deprecated;

(**
 * @deprecated Use the software scaler (swscale) instead.
 *)
function img_resample_full_init (owidth: integer; oheight: integer;
                                    iwidth: integer; iheight: integer;
                                    topBand: integer; bottomBand: integer;
                                    leftBand: integer; rightBand: integer;
                                    padtop: integer; padbottom: integer;
                                    padleft: integer; padright: integer): PImgReSampleContext;
  cdecl; external av__codec; deprecated;

(**
 * @deprecated Use the software scaler (swscale) instead.
 *)
procedure img_resample (s: PImgReSampleContext; output: PAVPicture; {const} input: PAVPicture);
  cdecl; external av__codec; deprecated;

(**
 * @deprecated Use the software scaler (swscale) instead.
 *)
procedure img_resample_close (s: PImgReSampleContext);
  cdecl; external av__codec; deprecated;

{$IFEND}

(**
 * Allocate memory for a picture.  Call avpicture_free to free it.
 *
 * @param picture the picture to be filled in.
 * @param pix_fmt the format of the picture.
 * @param width the width of the picture.
 * @param height the height of the picture.
 * @return Zero if successful, a negative value if not.
 *)
function avpicture_alloc (picture: PAVPicture; pix_fmt: TAVPixelFormat;
                          width: integer; height: integer): integer;
  cdecl; external av__codec;

(**
 * Free a picture previously allocated by avpicture_alloc().
 *
 * @param picture the AVPicture to be freed
 *)
procedure avpicture_free (picture: PAVPicture);
  cdecl; external av__codec;

(**
 * Fill in the AVPicture fields.
 * The fields of the given AVPicture are filled in by using the 'ptr' address
 * which points to the image data buffer. Depending on the specified picture
 * format, one or multiple image data pointers and line sizes will be set.
 * If a planar format is specified, several pointers will be set pointing to
 * the different picture planes and the line sizes of the different planes
 * will be stored in the lines_sizes array.
 *
 * @param picture AVPicture whose fields are to be filled in
 * @param ptr Buffer which will contain or contains the actual image data
 * @param pix_fmt The format in which the picture data is stored.
 * @param width the width of the image in pixels
 * @param height the height of the image in pixels
 * @return size of the image data in bytes
 *)
function avpicture_fill (picture: PAVPicture; ptr: pointer;
                 pix_fmt: TAVPixelFormat; width: integer; height: integer): integer;
  cdecl; external av__codec;

function avpicture_layout ({const} src: PAVPicture; pix_fmt: TAVPixelFormat;
                   width: integer; height: integer;
                   dest: pchar; dest_size: integer): integer;
  cdecl; external av__codec;

(**
 * Calculate the size in bytes that a picture of the given width and height
 * would occupy if stored in the given picture format.
 *
 * @param pix_fmt the given picture format
 * @param width the width of the image
 * @param height the height of the image
 * @return Image data size in bytes
 *)
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

const
  FF_LOSS_RESOLUTION  = $0001; {**< loss due to resolution change *}
  FF_LOSS_DEPTH       = $0002; {**< loss due to color depth change *}
  FF_LOSS_COLORSPACE  = $0004; {**< loss due to color space conversion *}
  FF_LOSS_ALPHA       = $0008; {**< loss of alpha bits *}
  FF_LOSS_COLORQUANT  = $0010; {**< loss due to color quantization *}
  FF_LOSS_CHROMA      = $0020; {**< loss of chroma (e.g. RGB to gray conversion) *}

(**
 * Computes what kind of losses will occur when converting from one specific
 * pixel format to another.
 * When converting from one pixel format to another, information loss may occur.
 * For example, when converting from RGB24 to GRAY, the color information will
 * be lost. Similarly, other losses occur when converting from some formats to
 * other formats. These losses can involve loss of chroma, but also loss of
 * resolution, loss of color depth, loss due to the color space conversion, loss
 * of the alpha bits or loss due to color quantization.
 * avcodec_get_fix_fmt_loss() informs you about the various types of losses
 * which will occur when converting from one pixel format to another.
 *
 * @param[in] dst_pix_fmt destination pixel format
 * @param[in] src_pix_fmt source pixel format
 * @param[in] has_alpha Whether the source pixel format alpha channel is used.
 * @return Combination of flags informing you what kind of losses will occur.
 *)
function avcodec_get_pix_fmt_loss (dst_pix_fmt: TAVPixelFormat; src_pix_fmt: TAVPixelFormat;
                           has_alpha: integer): integer;
  cdecl; external av__codec;

(**
 * Finds the best pixel format to convert to given a certain source pixel
 * format.  When converting from one pixel format to another, information loss
 * may occur.  For example, when converting from RGB24 to GRAY, the color
 * information will be lost. Similarly, other losses occur when converting from
 * some formats to other formats. avcodec_find_best_pix_fmt() searches which of
 * the given pixel formats should be used to suffer the least amount of loss.
 * The pixel formats from which it chooses one, are determined by the
 * \p pix_fmt_mask parameter.
 *
 * @code
 * src_pix_fmt = PIX_FMT_YUV420P;
 * pix_fmt_mask = (1 << PIX_FMT_YUV422P) || (1 << PIX_FMT_RGB24);
 * dst_pix_fmt = avcodec_find_best_pix_fmt(pix_fmt_mask, src_pix_fmt, alpha, &loss);
 * @endcode
 *
 * @param[in] pix_fmt_mask bitmask determining which pixel format to choose from
 * @param[in] src_pix_fmt source pixel format
 * @param[in] has_alpha Whether the source pixel format alpha channel is used.
 * @param[out] loss_ptr Combination of flags informing you what kind of losses will occur.
 * @return The best pixel format to convert to or -1 if none was found.
 *)
function avcodec_find_best_pix_fmt (pix_fmt_mask: integer; src_pix_fmt: TAVPixelFormat;
                            has_alpha: integer; loss_ptr: pinteger): integer;
  cdecl; external av__codec;

{$IF LIBAVCODEC_VERSION >= 51041000} // 51.41.0
(**
 * Print in buf the string corresponding to the pixel format with
 * number pix_fmt, or an header if pix_fmt is negative.
 *
 * @param[in] buf the buffer where to write the string
 * @param[in] buf_size the size of buf
 * @param[in] pix_fmt the number of the pixel format to print the corresponding info string, or
 * a negative value to print the corresponding header.
 * Meaningful values for obtaining a pixel format info vary from 0 to PIX_FMT_NB -1.
 *)
procedure avcodec_pix_fmt_string (buf: PChar; buf_size: integer; pix_fmt: integer);
  cdecl; external av__codec;
{$IFEND}

const
  FF_ALPHA_TRANSP      = $0001; {* image has some totally transparent pixels *}
  FF_ALPHA_SEMI_TRANSP = $0002; {* image has some transparent pixels *}

(**
 * Tell if an image really has transparent alpha values.
 * @return ored mask of FF_ALPHA_xxx constants
 *)
function img_get_alpha_info ({const} src: PAVPicture;
                             pix_fmt: TAVPixelFormat;
                             width: integer; height: integer): integer;
  cdecl; external av__codec;

{$IF LIBAVCODEC_VERSION < 52000000} // 52.0.0
(**
 * convert among pixel formats
 * @deprecated Use the software scaler (swscale) instead.
 *)
function img_convert (dst: PAVPicture; dst_pix_fmt: TAVPixelFormat;
              const src: PAVPicture; pix_fmt: TAVPixelFormat;
              width: integer; height: integer): integer;
  cdecl; external av__codec; deprecated;
{$IFEND}

(* deinterlace a picture *)
(* deinterlace - if not supported return -1 *)
function avpicture_deinterlace (dst: PAVPicture; const src: PAVPicture;
                        pix_fmt: TAVPixelFormat; width: integer; height: integer): integer;
  cdecl; external av__codec;

{* external high level API *}

{$IF LIBAVCODEC_VERSION < 52000000} // 52.0.0
{
var
  first_avcodec: PAVCodec; external av__codec;
}
{$IFEND}

{$IF LIBAVCODEC_VERSION >= 51049000} // 51.49.0
function av_codec_next(c: PAVCodec): PAVCodec;
  cdecl; external av__codec;
{$IFEND}

(* returns LIBAVCODEC_VERSION_INT constant *)
function avcodec_version (): cardinal;
  cdecl; external av__codec;

(* returns LIBAVCODEC_BUILD constant *)
function avcodec_build (): cardinal;
  cdecl; external av__codec;

(**
 * Initializes libavcodec.
 *
 * @warning This function \e must be called before any other libavcodec
 * function.
 *)
procedure avcodec_init ();
  cdecl; external av__codec;

procedure register_avcodec (format: PAVCodec);
  cdecl; external av__codec;

(**
 * Finds a registered encoder with a matching codec ID.
 *
 * @param id CodecID of the requested encoder
 * @return An encoder if one was found, NULL otherwise.
 *)
function avcodec_find_encoder (id: TCodecID): PAVCodec;
  cdecl; external av__codec;

(**
 * Finds a registered encoder with the specified name.
 *
 * @param name name of the requested encoder
 * @return An encoder if one was found, NULL otherwise.
 *)
function avcodec_find_encoder_by_name (name: pchar): PAVCodec;
  cdecl; external av__codec;

(**
 * Finds a registered decoder with a matching codec ID.
 *
 * @param id CodecID of the requested decoder
 * @return A decoder if one was found, NULL otherwise.
 *)
function avcodec_find_decoder(id: TCodecID): PAVCodec;
  cdecl; external av__codec;

(**
 * Finds a registered decoder with the specified name.
 *
 * @param name name of the requested decoder
 * @return A decoder if one was found, NULL otherwise.
 *)
function avcodec_find_decoder_by_name (name: pchar): PAVCodec;
  cdecl; external av__codec;
procedure avcodec_string(buf: pchar; buf_size: integer; enc: PAVCodecContext; encode: integer);
  cdecl; external av__codec;

(**
 * Sets the fields of the given AVCodecContext to default values.
 *
 * @param s The AVCodecContext of which the fields should be set to default values.
 *)
procedure avcodec_get_context_defaults (s: PAVCodecContext);
  cdecl; external av__codec;

{$IF LIBAVCODEC_VERSION >= 51039000} // 51.39.0
(** THIS FUNCTION IS NOT YET PART OF THE PUBLIC API!
 *  we WILL change its arguments and name a few times! *)
procedure avcodec_get_context_defaults2(s: PAVCodecContext; ctype: TCodecType);
  cdecl; external av__codec;
{$IFEND}

(**
 * Allocates an AVCodecContext and sets its fields to default values.  The
 * resulting struct can be deallocated by simply calling av_free().
 *
 * @return An AVCodecContext filled with default values or NULL on failure.
 * @see avcodec_get_context_defaults
 *)
function avcodec_alloc_context(): PAVCodecContext;
  cdecl; external av__codec;

{$IF LIBAVCODEC_VERSION >= 51039000} // 51.39.0
(** THIS FUNCTION IS NOT YET PART OF THE PUBLIC API!
 *  we WILL change its arguments and name a few times! *)
function avcodec_alloc_context2(ctype: TCodecType): PAVCodecContext;
  cdecl; external av__codec;
{$IFEND}

(**
 * Sets the fields of the given AVFrame to default values.
 *
 * @param pic The AVFrame of which the fields should be set to default values.
 *)
procedure avcodec_get_frame_defaults (pic: PAVFrame);
  cdecl; external av__codec;

(**
 * Allocates an AVFrame and sets its fields to default values.  The resulting
 * struct can be deallocated by simply calling av_free().
 *
 * @return An AVFrame filled with default values or NULL on failure.
 * @see avcodec_get_frame_defaults
 *)
function avcodec_alloc_frame(): PAVFrame;
  cdecl; external av__codec;

function avcodec_default_get_buffer (s: PAVCodecContext; pic: PAVFrame): integer;
  cdecl; external av__codec;
procedure avcodec_default_release_buffer (s: PAVCodecContext; pic: PAVFrame);
  cdecl; external av__codec;
function avcodec_default_reget_buffer (s: PAVCodecContext; pic: PAVFrame): integer;
  cdecl; external av__codec;
procedure avcodec_align_dimensions(s: PAVCodecContext; width: Pinteger; height: PInteger);
  cdecl; external av__codec;

(**
 * Checks if the given dimension of a picture is valid, meaning that all
 * bytes of the picture can be addressed with a signed int.
 *
 * @param[in] w Width of the picture.
 * @param[in] h Height of the picture.
 * @return Zero if valid, a negative value if invalid.
 *)
function avcodec_check_dimensions (av_log_ctx: pointer; w: cardinal; h: cardinal): integer;
  cdecl; external av__codec;
function avcodec_default_get_format(s: PAVCodecContext; const fmt: PAVPixelFormat): TAVPixelFormat;
  cdecl; external av__codec;

function avcodec_thread_init (s: PAVCodecContext; thread_count: integer): integer;
  cdecl; external av__codec;
procedure avcodec_thread_free (s: PAVCodecContext);
  cdecl; external av__codec;
function avcodec_thread_execute (s: PAVCodecContext; func: TExecuteFunc; arg: PPointer; ret: Pinteger; count: integer): integer;
  cdecl; external av__codec;
function avcodec_default_execute (s: PAVCodecContext; func: TExecuteFunc; arg: PPointer; ret: Pinteger; count: integer): integer;
  cdecl; external av__codec;
//FIXME func typedef

(**
 * Initializes the AVCodecContext to use the given AVCodec. Prior to using this
 * function the context has to be allocated.
 *
 * The functions avcodec_find_decoder_by_name(), avcodec_find_encoder_by_name(),
 * avcodec_find_decoder() and avcodec_find_encoder() provide an easy way for
 * retrieving a codec.
 *
 * @warning This function is not thread safe!
 *
 * @code
 * avcodec_register_all();
 * codec = avcodec_find_decoder(CODEC_ID_H264);
 * if (!codec)
 *     exit(1);
 *
 * context = avcodec_alloc_context();
 *
 * if (avcodec_open(context, codec) < 0)
 *     exit(1);
 * @endcode
 *
 * @param avctx The context which will be set up to use the given codec.
 * @param codec The codec to use within the context.
 * @return zero on success, a negative value on error
 * @see avcodec_alloc_context, avcodec_find_decoder, avcodec_find_encoder
 *)
function avcodec_open (avctx: PAVCodecContext; codec: PAVCodec): integer;
  cdecl; external av__codec;

(**
 * @deprecated Use avcodec_decode_audio2() instead.
 *)
function avcodec_decode_audio (avctx: PAVCodecContext; samples: Pword;
                           var frame_size_ptr: integer;
                           {const} buf: pchar; buf_size: integer): integer;
  cdecl; external av__codec;

{$IF LIBAVCODEC_VERSION >= 51030000} // 51.30.0
(**
 * Decodes an audio frame from \p buf into \p samples.
 * The avcodec_decode_audio2() function decodes an audio frame from the input
 * buffer \p buf of size \p buf_size. To decode it, it makes use of the
 * audio codec which was coupled with \p avctx using avcodec_open(). The
 * resulting decoded frame is stored in output buffer \p samples.  If no frame
 * could be decompressed, \p frame_size_ptr is zero. Otherwise, it is the
 * decompressed frame size in \e bytes.
 *
 * @warning You \e must set \p frame_size_ptr to the allocated size of the
 * output buffer before calling avcodec_decode_audio2().
 *
 * @warning The input buffer must be \c FF_INPUT_BUFFER_PADDING_SIZE larger than
 * the actual read bytes because some optimized bitstream readers read 32 or 64
 * bits at once and could read over the end.
 *
 * @warning The end of the input buffer \p buf should be set to 0 to ensure that
 * no overreading happens for damaged MPEG streams.
 *
 * @note You might have to align the input buffer \p buf and output buffer \p
 * samples. The alignment requirements depend on the CPU: On some CPUs it isn't
 * necessary at all, on others it won't work at all if not aligned and on others
 * it will work but it will have an impact on performance. In practice, the
 * bitstream should have 4 byte alignment at minimum and all sample data should
 * be 16 byte aligned unless the CPU doesn't need it (AltiVec and SSE do). If
 * the linesize is not a multiple of 16 then there's no sense in aligning the
 * start of the buffer to 16.
 *
 * @param avctx the codec context
 * @param[out] samples the output buffer
 * @param[in,out] frame_size_ptr the output buffer size in bytes
 * @param[in] buf the input buffer
 * @param[in] buf_size the input buffer size in bytes
 * @return On error a negative value is returned, otherwise the number of bytes
 * used or zero if no frame could be decompressed.
 *)
function avcodec_decode_audio2(avctx : PAVCodecContext; samples : PSmallint;
               var frame_size_ptr : integer;
               {const} buf: pchar; buf_size: integer): integer;
  cdecl; external av__codec;
{$IFEND}

(**
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
 *)
function avcodec_decode_video (avctx: PAVCodecContext; picture: PAVFrame;
                       var got_picture_ptr: integer;
                       {const} buf: PByte; buf_size: integer): integer;
  cdecl; external av__codec;

(* Decode a subtitle message. Return -1 if error, otherwise return the
 * number of bytes used. If no subtitle could be decompressed,
 * got_sub_ptr is zero. Otherwise, the subtitle is stored in *sub. *)
function avcodec_decode_subtitle (avctx: PAVCodecContext; sub: PAVSubtitle;
                          var got_sub_ptr: integer;
                          {const} buf: pchar; buf_size: integer): integer;
  cdecl; external av__codec;
function avcodec_parse_frame (avctx: PAVCodecContext; pdata: PPointer;
                      data_size_ptr: pinteger;
                      buf: pchar; buf_size: integer): integer;
  cdecl; external av__codec;

(**
 * Encodes an audio frame from \p samples into \p buf.
 * The avcodec_encode_audio() function encodes an audio frame from the input
 * buffer \p samples. To encode it, it makes use of the audio codec which was
 * coupled with \p avctx using avcodec_open(). The resulting encoded frame is
 * stored in output buffer \p buf.
 *
 * @note The output buffer should be at least \c FF_MIN_BUFFER_SIZE bytes large.
 *
 * @param avctx the codec context
 * @param[out] buf the output buffer
 * @param[in] buf_size the output buffer size
 * @param[in] samples the input buffer containing the samples
 * The number of samples read from this buffer is frame_size*channels,
 * both of which are defined in \p avctx.
 * @return On error a negative value is returned, on success zero or the number
 * of bytes used to encode the data read from the input buffer.
 *)
function avcodec_encode_audio (avctx: PAVCodecContext; buf: PByte;
                      buf_size: integer; {const} samples: PWord): integer;
  cdecl; external av__codec;

(**
 * Encodes a video frame from \p pict into \p buf.
 * The avcodec_encode_video() function encodes a video frame from the input
 * \p pict. To encode it, it makes use of the video codec which was coupled with
 * \p avctx using avcodec_open(). The resulting encoded bytes representing the
 * frame are stored in the output buffer \p buf. The input picture should be
 * stored using a specific format, namely \c avctx.pix_fmt.
 *
 * @param avctx the codec context
 * @param[out] buf the output buffer for the bitstream of encoded frame
 * @param[in] buf_size the size of the output buffer in bytes
 * @param[in] pict the input picture to encode
 * @return On error a negative value is returned, on success zero or the number
 * of bytes used from the input buffer.
 *)
function avcodec_encode_video (avctx: PAVCodecContext; buf: PByte;
                      buf_size: integer; pict: PAVFrame): integer;
  cdecl; external av__codec;
function avcodec_encode_subtitle (avctx: PAVCodecContext; buf: pchar;
                      buf_size: integer; {const} sub: PAVSubtitle): integer;
  cdecl; external av__codec;

function avcodec_close (avctx: PAVCodecContext): integer;
  cdecl; external av__codec;

procedure avcodec_register_all ();
  cdecl; external av__codec;

(**
 * Flush buffers, should be called when seeking or when switching to a different stream.
 *)
procedure avcodec_flush_buffers (avctx: PAVCodecContext);
  cdecl; external av__codec;
  
procedure avcodec_default_free_buffers (s: PAVCodecContext);
  cdecl; external av__codec;

(* misc useful functions *)

(**
 * Returns a single letter to describe the given picture type \p pict_type.
 *
 * @param[in] pict_type the picture type
 * @return A single character representing the picture type.
 *)
function av_get_pict_type_char (pict_type: integer): char;
  cdecl; external av__codec;

(**
 * Returns codec bits per sample.
 *
 * @param[in] codec_id the codec
 * @return Number of bits per sample or zero if unknown for the given codec.
 *)
function av_get_bits_per_sample (codec_id: TCodecID): integer;
  cdecl; external av__codec;

{$IF LIBAVCODEC_VERSION >= 51041000} // 51.41.0
(**
 * Returns sample format bits per sample.
 *
 * @param[in] sample_fmt the sample format
 * @return Number of bits per sample or zero if unknown for the given sample format.
 *)
function av_get_bits_per_sample_format(sample_fmt: TSampleFormat): integer;
  cdecl; external av__codec;
{$IFEND}

const
  AV_PARSER_PTS_NB      = 4;
  PARSER_FLAG_COMPLETE_FRAMES = $0001;

type
  {* frame parsing *}
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

    {$IF LIBAVCODEC_VERSION >= 51040003} // 51.40.3
    offset: int64;      ///< byte offset from starting packet start
    last_offset: int64;
    {$IFEND}
  end;

  TAVCodecParser = record
    codec_ids: array [0..4] of integer; (* several codec IDs are permitted *)
    priv_data_size: integer;
    parser_init: function (s: PAVCodecParserContext): integer; cdecl;
    parser_parse: function (s: PAVCodecParserContext; avctx: PAVCodecContext;
                            {const} poutbuf: PPointer; poutbuf_size: PInteger;
                            {const} buf: pchar; buf_size: integer): integer; cdecl;
    parser_close: procedure (s: PAVCodecParserContext); cdecl;
    split: function (avctx: PAVCodecContext; {const} buf: pchar;
                     buf_size: integer): integer; cdecl;
    next: PAVCodecParser;
  end;


{$IF LIBAVCODEC_VERSION < 52000000} // 52.0.0
{
var
  av_first_parser: PAVCodecParser; external av__codec;
}
{$IFEND}

{$IF LIBAVCODEC_VERSION >= 51049000} // 51.49.0
function av_parser_next(c: PAVCodecParser): PAVCodecParser;
  cdecl; external av__codec;
{$IFEND}

procedure av_register_codec_parser (parser: PAVCodecParser);
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
    {$IF LIBAVCODEC_VERSION >= 51043000} // 51.43.0
    close: procedure (bsfc: PAVBitStreamFilterContext);
    {$IFEND}
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

{$IF LIBAVCODEC_VERSION >= 51049000} // 51.49.0
function av_bitstream_filter_next(f: PAVBitStreamFilter): PAVBitStreamFilter;
  cdecl; external av__codec;
{$IFEND}

(* memory *)

(**
 * Reallocates the given block if it is not large enough, otherwise it
 * does nothing.
 *
 * @see av_realloc
 *)
procedure av_fast_realloc (ptr: pointer; size: PCardinal; min_size: Cardinal);
  cdecl; external av__codec;
  
(* for static data only *)

(**
 * Frees all static arrays and resets their pointers to 0.
 * Call this function to release all statically allocated tables.
 *
 * @deprecated. Code which uses av_free_static is broken/misdesigned
 * and should correctly use static arrays
 *
 *)
procedure  av_free_static ();
  cdecl; external av__codec; deprecated;

(**
 * Allocation of static arrays.
 *
 * @warning Do not use for normal allocation.
 *
 * @param[in] size The amount of memory you need in bytes.
 * @return block of memory of the requested size
 * @deprecated. Code which uses av_mallocz_static is broken/misdesigned
 * and should correctly use static arrays
 *)
procedure av_mallocz_static(size: cardinal);
  cdecl; external av__codec; deprecated; {av_malloc_attrib av_alloc_size(1)}

{$IF LIBAVCODEC_VERSION < 51035000} // 51.35.0
procedure av_realloc_static(ptr: pointer; size: Cardinal);
  cdecl; external av__codec;
{$IFEND}

{$IF LIBAVCODEC_VERSION >= 51039000} // 51.39.0
(**
 * Copy image 'src' to 'dst'.
 *)
procedure av_picture_copy(dst: PAVPicture; {const} src: PAVPicture;
              pix_fmt: integer; width: integer; height: integer);
  cdecl; external av__codec;

(**
 * Crop image top and left side.
 *)
function av_picture_crop(dst: PAVPicture; {const} src: PAVPicture;
             pix_fmt: integer; top_band: integer; left_band: integer): integer;
  cdecl; external av__codec;

(**
 * Pad image.
 *)
function av_picture_pad(dst: PAVPicture; {const} src: PAVPicture; height: integer; width: integer; pix_fmt: integer;
            padtop: integer; padbottom: integer; padleft: integer; padright: integer; color: PInteger): integer;
  cdecl; external av__codec;
{$IFEND}

{$IF LIBAVCODEC_VERSION < 52000000} // 52.0.0
(**
 * @deprecated Use the software scaler (swscale) instead.
 *)
procedure img_copy (dst: PAVPicture; const src: PAVPicture;
                    pix_fmt: TAVPixelFormat; width: integer; height: integer);
  cdecl; external av__codec; deprecated;

(**
 * @deprecated Use the software scaler (swscale) instead.
 *)
function img_crop (dst: PAVPicture; const src: PAVPicture;
           pix_fmt: TAVPixelFormat; top_band, left_band: integer): integer;
  cdecl; external av__codec; deprecated;

(**
 * @deprecated Use the software scaler (swscale) instead.
 *)
function img_pad (dst: PAVPicture; const src: PAVPicture; height, width: integer;
                  pix_fmt: TAVPixelFormat; padtop, padbottom, padleft, padright: integer;
                  color: PInteger): integer;
  cdecl; external av__codec; deprecated;
{$IFEND}

function av_xiphlacing(s: PByte; v: Cardinal): Cardinal;
  cdecl; external av__codec;

{$IF LIBAVCODEC_VERSION >= 51041000} // 51.41.0
(**
 * Parses \p str and put in \p width_ptr and \p height_ptr the detected values.
 *
 * @return 0 in case of a successful parsing, a negative value otherwise
 * @param[in] str the string to parse: it has to be a string in the format
 * <width>x<height> or a valid video frame size abbreviation.
 * @param[in,out] width_ptr pointer to the variable which will contain the detected
 * frame width value
 * @param[in,out] height_ptr pointer to the variable which will contain the detected
 * frame height value
 *)
function av_parse_video_frame_size(width_ptr: PInteger; height_ptr: PInteger; {const} str: PChar): integer;
  cdecl; external av__codec;

(**
 * Parses \p str and put in \p frame_rate the detected values.
 *
 * @return 0 in case of a successful parsing, a negative value otherwise
 * @param[in] str the string to parse: it has to be a string in the format
 * <frame_rate_nom>/<frame_rate_den>, a float number or a valid video rate abbreviation
 * @param[in,out] frame_rate pointer to the AVRational which will contain the detected
 * frame rate
 *)
function av_parse_video_frame_rate(frame_rate: PAVRational; {const} str: PChar): integer;
  cdecl; external av__codec;
{$IFEND}

{* error handling *}

const
{$IFDEF UNIX}
  ENOENT = ESysENOENT;
  EIO    = ESysEIO;
  ENOMEM = ESysENOMEM;
  EINVAL = ESysEINVAL;
  EDOM   = ESysEDOM;
  ENOSYS = ESysENOSYS;
  EILSEQ = ESysEILSEQ;
{$ELSE}
  ENOENT = 2;
  EIO    = 5;
  ENOMEM = 12;
  EINVAL = 22;
  EDOM   = 33;
  {$IFDEF MSWINDOWS}
  // Note: we assume that ffmpeg was compiled with MinGW.
  // This must be changed if DLLs were compiled with cygwin.
  ENOSYS = 40;  // MSVC/MINGW: 40, CYGWIN: 88,  LINUX/FPC: 38
  EILSEQ = 42;  // MSVC/MINGW: 42, CYGWIN: 138, LINUX/FPC: 84
  {$ENDIF}
{$ENDIF}

const
{$IF EINVAL > 0}
  AVERROR_SIGN = -1;
{$ELSE}
  {* Some platforms have E* and errno already negated. *}
  AVERROR_SIGN =  1;
{$IFEND}

(*
#if EINVAL > 0
#define AVERROR(e) (-(e)) {**< Returns a negative error code from a POSIX error code, to return from library functions. *}
#define AVUNERROR(e) (-(e)) {**< Returns a POSIX error code from a library function error return value. *}
#else
{* Some platforms have E* and errno already negated. *}
#define AVERROR(e) (e)
#define AVUNERROR(e) (e)
#endif
*)

const
  AVERROR_UNKNOWN     = AVERROR_SIGN * EINVAL;  (**< unknown error *)
  AVERROR_IO          = AVERROR_SIGN * EIO;     (**< I/O error *)
  AVERROR_NUMEXPECTED = AVERROR_SIGN * EDOM;    (**< Number syntax expected in filename. *)
  AVERROR_INVALIDDATA = AVERROR_SIGN * EINVAL;  (**< invalid data found *)
  AVERROR_NOMEM       = AVERROR_SIGN * ENOMEM;  (**< not enough memory *)
  AVERROR_NOFMT       = AVERROR_SIGN * EILSEQ;  (**< unknown format *)
  AVERROR_NOTSUPP     = AVERROR_SIGN * ENOSYS;  (**< Operation not supported. *)
  AVERROR_NOENT       = AVERROR_SIGN * ENOENT;  {**< No such file or directory. *}
  // Note: function calls as constant-initializers are invalid
  //AVERROR_PATCHWELCOME = -MKTAG('P','A','W','E'); {**< Not yet implemented in FFmpeg. Patches welcome. *}
  AVERROR_PATCHWELCOME = -(ord('P') or (ord('A') shl 8) or (ord('W') shl 16) or (ord('E') shl 24));

implementation

end.
