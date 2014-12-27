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
 *
 * This is a part of Pascal porting of ffmpeg.
 * - Originally by Victor Zinetz for Delphi and Free Pascal on Windows.
 * - For Mac OS X, some modifications were made by The Creative CAT, denoted as CAT
 *   in the source codes.
 * - Changes and updates by the UltraStar Deluxe Team
 *
 * Conversion of libavcodec/avcodec.h
 * version: 56.1.100
 *
 *)

unit avcodec;

{$IFDEF FPC}
  {$MODE DELPHI }
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

{$I switches.inc}  (* for ffmpeg defines *)
{$I ff_api-defines.inc}  (* FF_API_* defines *)

{$IFDEF DARWIN}
  {$linklib libavcodec}
{$ENDIF}

interface

uses
  ctypes,
  avutil,
  rational,
  SysUtils,
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  UConfig;

const
  (*
   * IMPORTANT: The official FFmpeg C headers change very quickly. Often some
   * of the data structures are changed so that they become incompatible with
   * older header files. The Pascal headers have to be adjusted to those changes,
   * otherwise the application might crash randomly or strange bugs (not
   * necessarily related to video or audio due to buffer overflows etc.) might
   * occur.
   *
   * In the past users reported problems with USDX that took hours to fix and
   * the problem was an unsupported version of FFmpeg. So we decided to disable
   * support for future versions of FFmpeg until the headers are revised by us
   * for that version as they otherwise most probably will break USDX.
   *
   * If the headers do not yet support your FFmpeg version you may want to
   * adjust the max. version numbers manually but please note: it may work but
   * in many cases it does not. The USDX team does NOT PROVIDE ANY SUPPORT
   * for the game if the MAX. VERSION WAS CHANGED.
   *
   * The only safe way to support new versions of FFmpeg is to add the changes
   * of the FFmpeg git repository C headers to the Pascal headers.
   * You can accelerate this process by posting a patch with the git changes
   * translated to Pascal to our bug tracker (please join our IRC chat before
   * you start working on it). Simply adjusting the max. versions is NOT a valid
   * fix.
   *)

  (* Supported version by this header *)
  LIBAVCODEC_MAX_VERSION_MAJOR   = 56;
  LIBAVCODEC_MAX_VERSION_MINOR   = 13;
  LIBAVCODEC_MAX_VERSION_RELEASE = 100;
  LIBAVCODEC_MAX_VERSION = (LIBAVCODEC_MAX_VERSION_MAJOR * VERSION_MAJOR) +
                           (LIBAVCODEC_MAX_VERSION_MINOR * VERSION_MINOR) +
                           (LIBAVCODEC_MAX_VERSION_RELEASE * VERSION_RELEASE);

  (* Min. supported version by this header *)
  LIBAVCODEC_MIN_VERSION_MAJOR   = 56;
  LIBAVCODEC_MIN_VERSION_MINOR   = 13;
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

{$IFNDEF FPC}
type
  // defines for Delphi
  size_t = cardinal;
{$ENDIF}

type
  FF_INTERNALC_MEM_TYPE = cuint;

const
  AV_NOPTS_VALUE: cint64  = $8000000000000000;
  AV_TIME_BASE            = 1000000;
  AV_TIME_BASE_Q: TAVRational = (num: 1; den: AV_TIME_BASE);

(**
 * @defgroup lavc_core Core functions/structures.
 * @ingroup libavc
 *
 * Basic definitions, functions for querying libavcodec capabilities,
 * allocating core structures, etc.
 * @
 *)

(**
 * Identify the syntax and semantics of the bitstream.
 * The principle is roughly:
 * Two decoders with the same ID can decode the same streams.
 * Two encoders with the same ID can encode compatible streams.
 * There may be slight deviations from the principle due to implementation
 * details.
 *
 * If you add a codec ID to this list, add it so that
 * 1. no value of a existing codec ID changes (that would break ABI),
 * 2. Give it a value which when taken as ASCII is recognized uniquely by a human as this specific codec.
 *    This ensures that 2 forks can independently add AVCodecIDs without producing conflicts.
 *
 * After adding new codec IDs, do not forget to add an entry to the codec
 * descriptor list and bump libavcodec minor version.
 *)
type
  TAVCodecID = (
    AV_CODEC_ID_NONE,

    //* video codecs */
    AV_CODEC_ID_MPEG1VIDEO,
    AV_CODEC_ID_MPEG2VIDEO, ///< preferred ID for MPEG-1/2 video decoding
{$IFDEF FF_API_XVMC}
    AV_CODEC_ID_MPEG2VIDEO_XVMC,
{$IFEND}
    AV_CODEC_ID_H261,
    AV_CODEC_ID_H263,
    AV_CODEC_ID_RV10,
    AV_CODEC_ID_RV20,
    AV_CODEC_ID_MJPEG,
    AV_CODEC_ID_MJPEGB,
    AV_CODEC_ID_LJPEG,
    AV_CODEC_ID_SP5X,
    AV_CODEC_ID_JPEGLS,
    AV_CODEC_ID_MPEG4,
    AV_CODEC_ID_RAWVIDEO,
    AV_CODEC_ID_MSMPEG4V1,
    AV_CODEC_ID_MSMPEG4V2,
    AV_CODEC_ID_MSMPEG4V3,
    AV_CODEC_ID_WMV1,
    AV_CODEC_ID_WMV2,
    AV_CODEC_ID_H263P,
    AV_CODEC_ID_H263I,
    AV_CODEC_ID_FLV1,
    AV_CODEC_ID_SVQ1,
    AV_CODEC_ID_SVQ3,
    AV_CODEC_ID_DVVIDEO,
    AV_CODEC_ID_HUFFYUV,
    AV_CODEC_ID_CYUV,
    AV_CODEC_ID_H264,
    AV_CODEC_ID_INDEO3,
    AV_CODEC_ID_VP3,
    AV_CODEC_ID_THEORA,
    AV_CODEC_ID_ASV1,
    AV_CODEC_ID_ASV2,
    AV_CODEC_ID_FFV1,
    AV_CODEC_ID_4XM,
    AV_CODEC_ID_VCR1,
    AV_CODEC_ID_CLJR,
    AV_CODEC_ID_MDEC,
    AV_CODEC_ID_ROQ,
    AV_CODEC_ID_INTERPLAY_VIDEO,
    AV_CODEC_ID_XAN_WC3,
    AV_CODEC_ID_XAN_WC4,
    AV_CODEC_ID_RPZA,
    AV_CODEC_ID_CINEPAK,
    AV_CODEC_ID_WS_VQA,
    AV_CODEC_ID_MSRLE,
    AV_CODEC_ID_MSVIDEO1,
    AV_CODEC_ID_IDCIN,
    AV_CODEC_ID_8BPS,
    AV_CODEC_ID_SMC,
    AV_CODEC_ID_FLIC,
    AV_CODEC_ID_TRUEMOTION1,
    AV_CODEC_ID_VMDVIDEO,
    AV_CODEC_ID_MSZH,
    AV_CODEC_ID_ZLIB,
    AV_CODEC_ID_QTRLE,
    AV_CODEC_ID_TSCC,
    AV_CODEC_ID_ULTI,
    AV_CODEC_ID_QDRAW,
    AV_CODEC_ID_VIXL,
    AV_CODEC_ID_QPEG,
    AV_CODEC_ID_PNG,
    AV_CODEC_ID_PPM,
    AV_CODEC_ID_PBM,
    AV_CODEC_ID_PGM,
    AV_CODEC_ID_PGMYUV,
    AV_CODEC_ID_PAM,
    AV_CODEC_ID_FFVHUFF,
    AV_CODEC_ID_RV30,
    AV_CODEC_ID_RV40,
    AV_CODEC_ID_VC1,
    AV_CODEC_ID_WMV3,
    AV_CODEC_ID_LOCO,
    AV_CODEC_ID_WNV1,
    AV_CODEC_ID_AASC,
    AV_CODEC_ID_INDEO2,
    AV_CODEC_ID_FRAPS,
    AV_CODEC_ID_TRUEMOTION2,
    AV_CODEC_ID_BMP,
    AV_CODEC_ID_CSCD,
    AV_CODEC_ID_MMVIDEO,
    AV_CODEC_ID_ZMBV,
    AV_CODEC_ID_AVS,
    AV_CODEC_ID_SMACKVIDEO,
    AV_CODEC_ID_NUV,
    AV_CODEC_ID_KMVC,
    AV_CODEC_ID_FLASHSV,
    AV_CODEC_ID_CAVS,
    AV_CODEC_ID_JPEG2000,
    AV_CODEC_ID_VMNC,
    AV_CODEC_ID_VP5,
    AV_CODEC_ID_VP6,
    AV_CODEC_ID_VP6F,
    AV_CODEC_ID_TARGA,
    AV_CODEC_ID_DSICINVIDEO,
    AV_CODEC_ID_TIERTEXSEQVIDEO,
    AV_CODEC_ID_TIFF,
    AV_CODEC_ID_GIF,
    AV_CODEC_ID_DXA,
    AV_CODEC_ID_DNXHD,
    AV_CODEC_ID_THP,
    AV_CODEC_ID_SGI,
    AV_CODEC_ID_C93,
    AV_CODEC_ID_BETHSOFTVID,
    AV_CODEC_ID_PTX,
    AV_CODEC_ID_TXD,
    AV_CODEC_ID_VP6A,
    AV_CODEC_ID_AMV,
    AV_CODEC_ID_VB,
    AV_CODEC_ID_PCX,
    AV_CODEC_ID_SUNRAST,
    AV_CODEC_ID_INDEO4,
    AV_CODEC_ID_INDEO5,
    AV_CODEC_ID_MIMIC,
    AV_CODEC_ID_RL2,
    AV_CODEC_ID_ESCAPE124,
    AV_CODEC_ID_DIRAC,
    AV_CODEC_ID_BFI,
    AV_CODEC_ID_CMV,
    AV_CODEC_ID_MOTIONPIXELS,
    AV_CODEC_ID_TGV,
    AV_CODEC_ID_TGQ,
    AV_CODEC_ID_TQI,
    AV_CODEC_ID_AURA,
    AV_CODEC_ID_AURA2,
    AV_CODEC_ID_V210X,
    AV_CODEC_ID_TMV,
    AV_CODEC_ID_V210,
    AV_CODEC_ID_DPX,
    AV_CODEC_ID_MAD,
    AV_CODEC_ID_FRWU,
    AV_CODEC_ID_FLASHSV2,
    AV_CODEC_ID_CDGRAPHICS,
    AV_CODEC_ID_R210,
    AV_CODEC_ID_ANM,
    AV_CODEC_ID_BINKVIDEO,
    AV_CODEC_ID_IFF_ILBM,
    AV_CODEC_ID_IFF_BYTERUN1,
    AV_CODEC_ID_KGV1,
    AV_CODEC_ID_YOP,
    AV_CODEC_ID_VP8,
    AV_CODEC_ID_PICTOR,
    AV_CODEC_ID_ANSI,
    AV_CODEC_ID_A64_MULTI,
    AV_CODEC_ID_A64_MULTI5,
    AV_CODEC_ID_R10K,
    AV_CODEC_ID_MXPEG,
    AV_CODEC_ID_LAGARITH,
    AV_CODEC_ID_PRORES,
    AV_CODEC_ID_JV,
    AV_CODEC_ID_DFA,
    AV_CODEC_ID_WMV3IMAGE,
    AV_CODEC_ID_VC1IMAGE,
    AV_CODEC_ID_UTVIDEO,
    AV_CODEC_ID_BMV_VIDEO,
    AV_CODEC_ID_VBLE,
    AV_CODEC_ID_DXTORY,
    AV_CODEC_ID_V410,
    AV_CODEC_ID_XWD,
    AV_CODEC_ID_CDXL,
    AV_CODEC_ID_XBM,
    AV_CODEC_ID_ZEROCODEC,
    AV_CODEC_ID_MSS1,
    AV_CODEC_ID_MSA1,
    AV_CODEC_ID_TSCC2,
    AV_CODEC_ID_MTS2,
    AV_CODEC_ID_CLLC,
    AV_CODEC_ID_MSS2,
    AV_CODEC_ID_VP9,
    AV_CODEC_ID_AIC,
    AV_CODEC_ID_ESCAPE130_DEPRECATED,
    AV_CODEC_ID_G2M_DEPRECATED,
    AV_CODEC_ID_HNM4_VIDEO,
    AV_CODEC_ID_HEVC_DEPRECATED,
    AV_CODEC_ID_FIC,
    AV_CODEC_ID_ALIAS_PIX,
    AV_CODEC_ID_BRENDER_PIX_DEPRECATED,
    AV_CODEC_ID_PAF_VIDEO_DEPRECATED,
    AV_CODEC_ID_EXR_DEPRECATED,
    AV_CODEC_ID_VP7_DEPRECATED,
    AV_CODEC_ID_SANM_DEPRECATED,
    AV_CODEC_ID_SGIRLE_DEPRECATED,
    AV_CODEC_ID_MVC1_DEPRECATED,
    AV_CODEC_ID_MVC2_DEPRECATED,
    
(** see below. they need to be hard coded.
    AV_CODEC_ID_BRENDER_PIX= MKBETAG('B','P','I','X'),
    AV_CODEC_ID_Y41P       = MKBETAG('Y','4','1','P'),
    AV_CODEC_ID_ESCAPE130  = MKBETAG('E','1','3','0'),
    AV_CODEC_ID_EXR        = MKBETAG('0','E','X','R'),
    AV_CODEC_ID_AVRP       = MKBETAG('A','V','R','P'),

    AV_CODEC_ID_012V       = MKBETAG('0','1','2','V'),
    AV_CODEC_ID_G2M        = MKBETAG( 0 ,'G','2','M'),
    AV_CODEC_ID_AVUI       = MKBETAG('A','V','U','I'),
    AV_CODEC_ID_AYUV       = MKBETAG('A','Y','U','V'),
    AV_CODEC_ID_TARGA_Y216 = MKBETAG('T','2','1','6'),
    AV_CODEC_ID_V308       = MKBETAG('V','3','0','8'),
    AV_CODEC_ID_V408       = MKBETAG('V','4','0','8'),
    AV_CODEC_ID_YUV4       = MKBETAG('Y','U','V','4'),
    AV_CODEC_ID_SANM       = MKBETAG('S','A','N','M'),
    AV_CODEC_ID_PAF_VIDEO  = MKBETAG('P','A','F','V'),
    AV_CODEC_ID_AVRN       = MKBETAG('A','V','R','n'),
    AV_CODEC_ID_CPIA       = MKBETAG('C','P','I','A'),
    AV_CODEC_ID_XFACE      = MKBETAG('X','F','A','C'),
    AV_CODEC_ID_SGIRLE     = MKBETAG('S','G','I','R'),
    AV_CODEC_ID_MVC1       = MKBETAG('M','V','C','1'),
    AV_CODEC_ID_MVC2       = MKBETAG('M','V','C','2'),
    AV_CODEC_ID_SNOW       = MKBETAG('S','N','O','W'),
    AV_CODEC_ID_WEBP       = MKBETAG('W','E','B','P'),
    AV_CODEC_ID_SMVJPEG    = MKBETAG('S','M','V','J'),
    AV_CODEC_ID_HEVC       = MKBETAG('H','2','6','5'),
    AV_CODEC_ID_H265       = AV_CODEC_ID_HEVC,
    AV_CODEC_ID_VP7        = MKBETAG('V','P','7','0'),
    AV_CODEC_ID_APNG       = MKBETAG('A','P','N','G')
  *)
    //* various PCM "codecs" */
//    AV_CODEC_ID_FIRST_AUDIO = 0x10000,     ///< A dummy id pointing at the start of audio codecs
    AV_CODEC_ID_PCM_S16LE = $10000,
    AV_CODEC_ID_PCM_S16BE,
    AV_CODEC_ID_PCM_U16LE,
    AV_CODEC_ID_PCM_U16BE,
    AV_CODEC_ID_PCM_S8,
    AV_CODEC_ID_PCM_U8,
    AV_CODEC_ID_PCM_MULAW,
    AV_CODEC_ID_PCM_ALAW,
    AV_CODEC_ID_PCM_S32LE,
    AV_CODEC_ID_PCM_S32BE,
    AV_CODEC_ID_PCM_U32LE,
    AV_CODEC_ID_PCM_U32BE,
    AV_CODEC_ID_PCM_S24LE,
    AV_CODEC_ID_PCM_S24BE,
    AV_CODEC_ID_PCM_U24LE,
    AV_CODEC_ID_PCM_U24BE,
    AV_CODEC_ID_PCM_S24DAUD,
    AV_CODEC_ID_PCM_ZORK,
    AV_CODEC_ID_PCM_S16LE_PLANAR,
    AV_CODEC_ID_PCM_DVD,
    AV_CODEC_ID_PCM_F32BE,
    AV_CODEC_ID_PCM_F32LE,
    AV_CODEC_ID_PCM_F64BE,
    AV_CODEC_ID_PCM_F64LE,
    AV_CODEC_ID_PCM_BLURAY,
    AV_CODEC_ID_PCM_LXF,
    AV_CODEC_ID_S302M,
    AV_CODEC_ID_PCM_S8_PLANAR,
(** see below. they need to be hard coded.
    AV_CODEC_ID_PCM_S24LE_PLANAR = MKBETAG(24,'P','S','P'),
    AV_CODEC_ID_PCM_S32LE_PLANAR = MKBETAG(32,'P','S','P'),
    AV_CODEC_ID_PCM_S16BE_PLANAR = MKBETAG('P','S','P',16),
  *)

    //* various ADPCM codecs */
    AV_CODEC_ID_ADPCM_IMA_QT = $11000,
    AV_CODEC_ID_ADPCM_IMA_WAV,
    AV_CODEC_ID_ADPCM_IMA_DK3,
    AV_CODEC_ID_ADPCM_IMA_DK4,
    AV_CODEC_ID_ADPCM_IMA_WS,
    AV_CODEC_ID_ADPCM_IMA_SMJPEG,
    AV_CODEC_ID_ADPCM_MS,
    AV_CODEC_ID_ADPCM_4XM,
    AV_CODEC_ID_ADPCM_XA,
    AV_CODEC_ID_ADPCM_ADX,
    AV_CODEC_ID_ADPCM_EA,
    AV_CODEC_ID_ADPCM_G726,
    AV_CODEC_ID_ADPCM_CT,
    AV_CODEC_ID_ADPCM_SWF,
    AV_CODEC_ID_ADPCM_YAMAHA,
    AV_CODEC_ID_ADPCM_SBPRO_4,
    AV_CODEC_ID_ADPCM_SBPRO_3,
    AV_CODEC_ID_ADPCM_SBPRO_2,
    AV_CODEC_ID_ADPCM_THP,
    AV_CODEC_ID_ADPCM_IMA_AMV,
    AV_CODEC_ID_ADPCM_EA_R1,
    AV_CODEC_ID_ADPCM_EA_R3,
    AV_CODEC_ID_ADPCM_EA_R2,
    AV_CODEC_ID_ADPCM_IMA_EA_SEAD,
    AV_CODEC_ID_ADPCM_IMA_EA_EACS,
    AV_CODEC_ID_ADPCM_EA_XAS,
    AV_CODEC_ID_ADPCM_EA_MAXIS_XA,
    AV_CODEC_ID_ADPCM_IMA_ISS,
    AV_CODEC_ID_ADPCM_G722,
    AV_CODEC_ID_ADPCM_IMA_APC,
    AV_CODEC_ID_ADPCM_VIMA_DEPRECATED,
(** see below. they need to be hard coded.
    AV_CODEC_ID_ADPCM_VIMA = MKBETAG('V','I','M','A'),
    AV_CODEC_ID_VIMA       = MKBETAG('V','I','M','A'),
    AV_CODEC_ID_ADPCM_AFC  = MKBETAG('A','F','C',' '),
    AV_CODEC_ID_ADPCM_IMA_OKI = MKBETAG('O','K','I',' '),
    AV_CODEC_ID_ADPCM_DTK  = MKBETAG('D','T','K',' '),
    AV_CODEC_ID_ADPCM_IMA_RAD = MKBETAG('R','A','D',' '),
    AV_CODEC_ID_ADPCM_G726LE = MKBETAG('6','2','7','G'),
  *)

    //* AMR */
    AV_CODEC_ID_AMR_NB = $12000,
    AV_CODEC_ID_AMR_WB,

    //* RealAudio codecs*/
    AV_CODEC_ID_RA_144 = $13000,
    AV_CODEC_ID_RA_288,

    //* various DPCM codecs */
    AV_CODEC_ID_ROQ_DPCM = $14000,
    AV_CODEC_ID_INTERPLAY_DPCM,
    AV_CODEC_ID_XAN_DPCM,
    AV_CODEC_ID_SOL_DPCM,

    //* audio codecs */
    AV_CODEC_ID_MP2 = $15000,
    AV_CODEC_ID_MP3, ///< preferred ID for decoding MPEG audio layer 1, 2 or 3
    AV_CODEC_ID_AAC,
    AV_CODEC_ID_AC3,
    AV_CODEC_ID_DTS,
    AV_CODEC_ID_VORBIS,
    AV_CODEC_ID_DVAUDIO,
    AV_CODEC_ID_WMAV1,
    AV_CODEC_ID_WMAV2,
    AV_CODEC_ID_MACE3,
    AV_CODEC_ID_MACE6,
    AV_CODEC_ID_VMDAUDIO,
    AV_CODEC_ID_FLAC,
    AV_CODEC_ID_MP3ADU,
    AV_CODEC_ID_MP3ON4,
    AV_CODEC_ID_SHORTEN,
    AV_CODEC_ID_ALAC,
    AV_CODEC_ID_WESTWOOD_SND1,
    AV_CODEC_ID_GSM, ///< as in Berlin toast format
    AV_CODEC_ID_QDM2,
    AV_CODEC_ID_COOK,
    AV_CODEC_ID_TRUESPEECH,
    AV_CODEC_ID_TTA,
    AV_CODEC_ID_SMACKAUDIO,
    AV_CODEC_ID_QCELP,
    AV_CODEC_ID_WAVPACK,
    AV_CODEC_ID_DSICINAUDIO,
    AV_CODEC_ID_IMC,
    AV_CODEC_ID_MUSEPACK7,
    AV_CODEC_ID_MLP,
    AV_CODEC_ID_GSM_MS, { as found in WAV }
    AV_CODEC_ID_ATRAC3,
{$IFDEF FF_API_VOXWARE}
    AV_CODEC_ID_VOXWARE,
{$IFEND}
    AV_CODEC_ID_APE,
    AV_CODEC_ID_NELLYMOSER,
    AV_CODEC_ID_MUSEPACK8,
    AV_CODEC_ID_SPEEX,
    AV_CODEC_ID_WMAVOICE,
    AV_CODEC_ID_WMAPRO,
    AV_CODEC_ID_WMALOSSLESS,
    AV_CODEC_ID_ATRAC3P,
    AV_CODEC_ID_EAC3,
    AV_CODEC_ID_SIPR,
    AV_CODEC_ID_MP1,
    AV_CODEC_ID_TWINVQ,
    AV_CODEC_ID_TRUEHD,
    AV_CODEC_ID_MP4ALS,
    AV_CODEC_ID_ATRAC1,
    AV_CODEC_ID_BINKAUDIO_RDFT,
    AV_CODEC_ID_BINKAUDIO_DCT,
    AV_CODEC_ID_AAC_LATM,
    AV_CODEC_ID_QDMC,
    AV_CODEC_ID_CELT,
    AV_CODEC_ID_G723_1,
    AV_CODEC_ID_G729,
    AV_CODEC_ID_8SVX_EXP,
    AV_CODEC_ID_8SVX_FIB,
    AV_CODEC_ID_BMV_AUDIO,
    AV_CODEC_ID_RALF,
    AV_CODEC_ID_IAC,
    AV_CODEC_ID_ILBC,
    AV_CODEC_ID_OPUS_DEPRECATED,
    AV_CODEC_ID_COMFORT_NOISE,
    AV_CODEC_ID_TAK_DEPRECATED,
    AV_CODEC_ID_PAF_AUDIO_DEPRECATED,
    AV_CODEC_ID_ON2AVC,
(** see below. they need to be hard coded.
    AV_CODEC_ID_FFWAVESYNTH = MKBETAG('F','F','W','S'),
    AV_CODEC_ID_SONIC       = MKBETAG('S','O','N','C'),
    AV_CODEC_ID_SONIC_LS    = MKBETAG('S','O','N','L'),
    AV_CODEC_ID_PAF_AUDIO   = MKBETAG('P','A','F','A'),
    AV_CODEC_ID_OPUS        = MKBETAG('O','P','U','S'),
    AV_CODEC_ID_TAK         = MKBETAG('t','B','a','K'),
    AV_CODEC_ID_EVRC        = MKBETAG('s','e','v','c'),
    AV_CODEC_ID_SMV         = MKBETAG('s','s','m','v'),
    AV_CODEC_ID_DSD_LSBF    = MKBETAG('D','S','D','L'),
    AV_CODEC_ID_DSD_MSBF    = MKBETAG('D','S','D','M'),
    AV_CODEC_ID_DSD_LSBF_PLANAR = MKBETAG('D','S','D','1'),
    AV_CODEC_ID_DSD_MSBF_PLANAR = MKBETAG('D','S','D','8'),
 *)

    //* subtitle codecs */
//    AV_CODEC_ID_FIRST_SUBTITLE = 0x17000,          ///< A dummy ID pointing at the start of subtitle codecs.
    AV_CODEC_ID_DVD_SUBTITLE = $17000,
    AV_CODEC_ID_DVB_SUBTITLE,
    AV_CODEC_ID_TEXT,  ///< raw UTF-8 text
    AV_CODEC_ID_XSUB,
    AV_CODEC_ID_SSA,
    AV_CODEC_ID_MOV_TEXT,
    AV_CODEC_ID_HDMV_PGS_SUBTITLE,
    AV_CODEC_ID_DVB_TELETEXT,
    AV_CODEC_ID_SRT,
(** see below. they need to be hard coded.
    AV_CODEC_ID_MICRODVD   = MKBETAG('m','D','V','D'),
    AV_CODEC_ID_EIA_608    = MKBETAG('c','6','0','8'),
    AV_CODEC_ID_JACOSUB    = MKBETAG('J','S','U','B'),
    AV_CODEC_ID_SAMI       = MKBETAG('S','A','M','I'),
    AV_CODEC_ID_REALTEXT   = MKBETAG('R','T','X','T'),
    AV_CODEC_ID_STL        = MKBETAG('S','p','T','L'),
    AV_CODEC_ID_SUBVIEWER1 = MKBETAG('S','b','V','1'),
    AV_CODEC_ID_SUBVIEWER  = MKBETAG('S','u','b','V'),
    AV_CODEC_ID_SUBRIP     = MKBETAG('S','R','i','p'),
    AV_CODEC_ID_WEBVTT     = MKBETAG('W','V','T','T'),
    AV_CODEC_ID_MPL2       = MKBETAG('M','P','L','2'),
    AV_CODEC_ID_VPLAYER    = MKBETAG('V','P','l','r'),
    AV_CODEC_ID_PJS        = MKBETAG('P','h','J','S'),
    AV_CODEC_ID_ASS        = MKBETAG('A','S','S',' '),  ///< ASS as defined in Matroska
 *)

    //* other specific kind of codecs (generally used for attachments) */
//    AV_CODEC_ID_FIRST_UNKNOWN = 0x18000,           ///< A dummy ID pointing at the start of various fake codecs.
    AV_CODEC_ID_TTF = $18000,
(** see below. they need to be hard coded.
    AV_CODEC_ID_BINTEXT    = MKBETAG('B','T','X','T'),
    AV_CODEC_ID_XBIN       = MKBETAG('X','B','I','N'),
    AV_CODEC_ID_IDF        = MKBETAG( 0 ,'I','D','F'),
    AV_CODEC_ID_OTF        = MKBETAG( 0 ,'O','T','F'),
    AV_CODEC_ID_SMPTE_KLV  = MKBETAG('K','L','V','A'),
    AV_CODEC_ID_DVD_NAV    = MKBETAG('D','N','A','V'),
    AV_CODEC_ID_TIMED_ID3   = MKBETAG('T','I','D','3'),    
 *)

    AV_CODEC_ID_PROBE = $19000, ///< codec_id is not known (like AV_CODEC_ID_NONE) but lavf should attempt to identify it

    AV_CODEC_ID_MPEG2TS = $20000, (**< _FAKE_ codec to indicate a raw MPEG-2 TS
                               * stream (only used by libavformat) *)
    AV_CODEC_ID_MPEG4SYSTEMS = $20001, (**< _FAKE_ codec to indicate a MPEG-4 Systems
                                 * stream (only used by libavformat) *)
    AV_CODEC_ID_FFMETADATA = $21000,   ///< Dummy codec for streams containing only metadata information.

(** hardcoded codecs from above. pascal needs them to be ordered **)
    AV_CODEC_ID_G2M         = $0047324D, // MKBETAG( 0 ,'G','2','M'),
    AV_CODEC_ID_IDF         = $00494446, // MKBETAG( 0 ,'I','D','F'),
    AV_CODEC_ID_OTF         = $004F5446, // MKBETAG( 0 ,'O','T','F'),
    AV_CODEC_ID_PCM_S24LE_PLANAR = $18505350, // MKBETAG(24,'P','S','P'),
    AV_CODEC_ID_PCM_S32LE_PLANAR = $20505350, // MKBETAG(32,'P','S','P'),
    AV_CODEC_ID_012V        = $30313256, // MKBETAG('0','1','2','V'),
    AV_CODEC_ID_EXR         = $30455852, // MKBETAG('0','E','X','R'),
    AV_CODEC_ID_ADPCM_G726LE= $36323747, // MKBETAG('6','2','7','G'),
    AV_CODEC_ID_ADPCM_AFC   = $41464320, // MKBETAG('A','F','C',' '),
    AV_CODEC_ID_ASS         = $41534B20, // MKBETAG('A','S','S',' '),  ///< ASS as defined in Matroska
    AV_CODEC_ID_AVRP        = $41565250, // MKBETAG('A','V','R','P'),
    AV_CODEC_ID_AVRN        = $4156526E, // MKBETAG('A','V','R','n'),
    AV_CODEC_ID_AVUI        = $41565549, // MKBETAG('A','V','U','I'),
    AV_CODEC_ID_AYUV        = $41595556, // MKBETAG('A','Y','U','V'),
    AV_CODEC_ID_BRENDER_PIX = $42504958, // MKBETAG('B','P','I','X'),
    AV_CODEC_ID_BINTEXT     = $42545854, // MKBETAG('B','T','X','T'),
    AV_CODEC_ID_CPIA        = $43504941, // MKBETAG('C','P','I','A'),
    AV_CODEC_ID_DVD_NAV     = $444E4156, // MKBETAG('D','N','A','V'),
    AV_CODEC_ID_DSD_LSBF_PLANAR = $44534431, // MKBETAG('D','S','D','1'),
    AV_CODEC_ID_DSD_MSBF_PLANAR = $44534438, // MKBETAG('D','S','D','8'),
    AV_CODEC_ID_DSD_LSBF    = $4453444C, // MKBETAG('D','S','D','L'),
    AV_CODEC_ID_DSD_MSBF    = $4453444D, // MKBETAG('D','S','D','M'),
    AV_CODEC_ID_ADPCM_DTK   = $44544B20, // MKBETAG('D','T','K',' '),
    AV_CODEC_ID_ESCAPE130   = $45313330, // MKBETAG('E','1','3','0'),
    AV_CODEC_ID_FFWAVESYNTH = $46465753, // MKBETAG('F','F','W','S'),
    AV_CODEC_ID_JACOSUB     = $4A535542, // MKBETAG('J','S','U','B'),
    AV_CODEC_ID_SMPTE_KLV   = $4B4C5641, // MKBETAG('K','L','V','A'),
    AV_CODEC_ID_MPL2        = $4D504C32, // MKBETAG('M','P','L','2'),
    AV_CODEC_ID_MVC1        = $4D564331, // MKBETAG('M','V','C','1'),
    AV_CODEC_ID_MVC2        = $4D564332, // MKBETAG('M','V','C','2'),
    AV_CODEC_ID_ADPCM_IMA_OKI=$4F4B4920, // MKBETAG('O','K','I',' '),
    AV_CODEC_ID_OPUS        = $4F505553, // MKBETAG('O','P','U','S'),
    AV_CODEC_ID_PAF_AUDIO   = $50414641, // MKBETAG('P','A','F','A'),
    AV_CODEC_ID_PAF_VIDEO   = $50414656, // MKBETAG('P','A','F','V'),
    AV_CODEC_ID_PCM_S16BE_PLANAR = $50535010, // MKBETAG('P','S','P',16),
    AV_CODEC_ID_PJS         = $50684A53, // MKBETAG('P','h','J','S'),
    AV_CODEC_ID_ADPCM_IMA_RAD = $52414420, // MKBETAG('R','A','D',' '),
    AV_CODEC_ID_REALTEXT    = $52545854, // MKBETAG('R','T','X','T'),
    AV_CODEC_ID_SAMI        = $53414D49, // MKBETAG('S','A','M','I'),
    AV_CODEC_ID_SANM        = $53414E4D, // MKBETAG('S','A','N','M'),
    AV_CODEC_ID_SGIRLE      = $53474952, // MKBETAG('S','G','I','R'),
    AV_CODEC_ID_SMVJPEG     = $534D564A, // MKBETAG('S','M','V','J'),
    AV_CODEC_ID_SNOW        = $534E4F57, // MKBETAG('S','N','O','W'),
    AV_CODEC_ID_SONIC       = $534F4E43, // MKBETAG('S','O','N','C'),
    AV_CODEC_ID_SONIC_LS    = $534F4E4C, // MKBETAG('S','O','N','L'),
    AV_CODEC_ID_SUBRIP      = $53526970, // MKBETAG('S','R','i','p'),
    AV_CODEC_ID_SUBVIEWER1  = $53625631, // MKBETAG('S','b','V','1'),
    AV_CODEC_ID_SUBVIEWER   = $53756256, // MKBETAG('S','u','b','V'),
    AV_CODEC_ID_TARGA_Y216  = $54323136, // MKBETAG('T','2','1','6'),
    AV_CODEC_ID_TIMED_ID3   = $54494433, // MKBETAG('T','I','D','3'),
    AV_CODEC_ID_V308        = $56333038, // MKBETAG('V','3','0','8'),
    AV_CODEC_ID_V408        = $56413038, // MKBETAG('V','4','0','8'),
    AV_CODEC_ID_ADPCM_VIMA  = $56494D41, // MKBETAG('V','I','M','A'),
    AV_CODEC_ID_VIMA        = $56494D41, // MKBETAG('V','I','M','A'),
    AV_CODEC_ID_VPLAYER     = $56506C72, // MKBETAG('V','P','l','r'),
    AV_CODEC_ID_WEBP        = $57454250, // MKBETAG('W','E','B','P'),
    AV_CODEC_ID_WEBVTT      = $57565454, // MKBETAG('W','V','T','T'),
    AV_CODEC_ID_XBIN        = $5842494E, // MKBETAG('X','B','I','N'),
    AV_CODEC_ID_XFACE       = $58464143, // MKBETAG('X','F','A','C'),
    AV_CODEC_ID_Y41P        = $59343150, // MKBETAG('Y','4','1','P'),
    AV_CODEC_ID_YUV4        = $59555634, // MKBETAG('Y','U','V','4'),
    AV_CODEC_ID_EIA_608     = $63363038, // MKBETAG('c','6','0','8'),
    AV_CODEC_ID_MICRODVD    = $6D445644, // MKBETAG('m','D','V','D')
    AV_CODEC_ID_EVRC        = $73657663, // MKBETAG('s','e','v','c'),
    AV_CODEC_ID_SMV         = $73736D76, // MKBETAG('s','s','m','v'),
    AV_CODEC_ID_TAK         = $7442614B // MKBETAG('t','B','a','K'),

  );

type
  TCodecType = TAVMediaType;

const
  CODEC_TYPE_UNKNOWN    = AVMEDIA_TYPE_UNKNOWN;
  CODEC_TYPE_VIDEO      = AVMEDIA_TYPE_VIDEO;
  CODEC_TYPE_AUDIO      = AVMEDIA_TYPE_AUDIO;
  CODEC_TYPE_DATA       = AVMEDIA_TYPE_DATA;
  CODEC_TYPE_SUBTITLE   = AVMEDIA_TYPE_SUBTITLE;
  CODEC_TYPE_ATTACHMENT = AVMEDIA_TYPE_ATTACHMENT;
  CODEC_TYPE_NB         = AVMEDIA_TYPE_NB;

(**
 * This struct describes the properties of a single codec described by an
 * AVCodecID.
 * @see avcodec_descriptor_get()
 *)
type
  PAVCodecDescriptor = ^TAVCodecDescriptor;
  TAVCodecDescriptor = record
    id: TAVCodecID;
    type_: TAVMediaType;
    (**
     * Name of the codec described by this descriptor. It is non-empty and
     * unique for each codec descriptor. It should contain alphanumeric
     * characters and '_' only.
     *)
    name: PAnsiChar;
    (**
     * A more descriptive name for this codec. May be NULL.
     *)
    long_name: PAnsiChar;
    (**
     * Codec properties, a combination of AV_CODEC_PROP_* flags.
     *)
    props: cint;

    (**
     * MIME type(s) associated with the codec.
     * May be NULL; if not, a NULL-terminated array of MIME types.
     * The first item is always non-NULL and is the preferred MIME type.
     *)
    mime_types: PAnsiChar;
  end;

const
(**
 * Codec uses only intra compression.
 * Video codecs only.
 *)
  AV_CODEC_PROP_INTRA_ONLY     = 1 << 0;
(**
 * Codec supports lossy compression. Audio and video codecs only.
 * @note a codec may support both lossy and lossless
 * compression modes
 *)
  AV_CODEC_PROP_LOSSY          = 1 << 1;
(**
 * Codec supports lossless compression. Audio and video codecs only.
 *)
  AV_CODEC_PROP_LOSSLESS       = 1 << 2;
(**
 * Codec supports frame reordering. That is, the coded order (the order in which
 * the encoded packets are output by the encoders / stored / input to the
 * decoders) may be different from the presentation order of the corresponding
 * frames.
 *
 * For codecs that do not have this property set, PTS and DTS should always be
 * equal.
 *)
  AV_CODEC_PROP_REORDER        = 1 << 3;
(**
 * Subtitle codec is bitmap based
 * Decoded AVSubtitle data can be read from the AVSubtitleRect->pict field.
 *)
  AV_CODEC_PROP_BITMAP_SUB     = 1 << 16;
(**
 * Subtitle codec is text based.
 * Decoded AVSubtitle data can be read from the AVSubtitleRect->ass field.
 *)
  AV_CODEC_PROP_TEXT_SUB       = 1 << 17;

(**
 * @ingroup lavc_decoding
 * Required number of additionally allocated bytes at the end of the input bitstream for decoding.
 * This is mainly needed because some optimized bitstream readers read
 * 32 or 64 bit at once and could read over the end.<br>
 * Note: If the first 23 bits of the additional bytes are not 0, then damaged
 * MPEG bitstreams could cause overread and segfault.
 *)
  FF_INPUT_BUFFER_PADDING_SIZE = 32;

(**
 * @ingroup lavc_decoding
 * minimum encoding buffer size.
 * Used to avoid some checks during header writing.
 *)
  FF_MIN_BUFFER_SIZE = 16384;

type
(*
 * @ingroup lavc_decoding
 * motion estimation type.
 *)
  TMotion_Est_ID = (
    ME_ZERO = 1,  ///< no search, that is use 0,0 vector whenever one is needed
    ME_FULL,
    ME_LOG,
    ME_PHODS,
    ME_EPZS,      ///< enhanced predictive zonal search
    ME_X1,        ///< reserved for experiments
    ME_HEX,       ///< hexagon based search
    ME_UMH,       ///< uneven multi-hexagon search
    ME_ITER = 50, ///< iterative search
    ME_TESA       ///< transformed exhaustive search algorithm
  );

(**
 * @ingroup lavc_decoding
 *)
  TAVDiscard = (
    (* We leave some space between them for extensions (drop some
     * keyframes for intra-only or drop just some bidir frames).
     *)
    AVDISCARD_NONE    = -16, ///< discard nothing
    AVDISCARD_DEFAULT =   0, ///< discard useless packets like 0 size packets in avi
    AVDISCARD_NONREF  =   8, ///< discard all non reference
    AVDISCARD_BIDIR   =  16, ///< discard all bidirectional frames
    AVDISCARD_NONINTRA=  24, ///< discard all non intra frames
    AVDISCARD_NONKEY  =  32, ///< discard all frames except keyframes
    AVDISCARD_ALL     =  48  ///< discard all
  );

{ removed, kept for the moment
  TAVColorPrimaries = (
    AVCOL_PRI_BT709       = 1, ///< also ITU-R BT1361 / IEC 61966-2-4 / SMPTE RP177 Annex B
    AVCOL_PRI_UNSPECIFIED = 2,
    AVCOL_PRI_BT470M      = 4,
    AVCOL_PRI_BT470BG     = 5, ///< also ITU-R BT601-6 625 / ITU-R BT1358 625 / ITU-R BT1700 625 PAL & SECAM
    AVCOL_PRI_SMPTE170M   = 6, ///< also ITU-R BT601-6 525 / ITU-R BT1358 525 / ITU-R BT1700 NTSC
    AVCOL_PRI_SMPTE240M   = 7, ///< functionally identical to above
    AVCOL_PRI_FILM        = 8,
    AVCOL_PRI_BT2020      = 9, ///< ITU-R BT2020
    AVCOL_PRI_NB               ///< Not part of ABI
  );

  TAVColorTransferCharacteristic = (
    AVCOL_TRC_BT709        =  1, ///< also ITU-R BT1361
    AVCOL_TRC_UNSPECIFIED  =  2,
    AVCOL_TRC_GAMMA22      =  4, ///< also ITU-R BT470M / ITU-R BT1700 625 PAL & SECAM
    AVCOL_TRC_GAMMA28      =  5, ///< also ITU-R BT470BG
    AVCOL_TRC_SMPTE170M    =  6, ///< also ITU-R BT601-6 525 or 625 / ITU-R BT1358 525 or 625 / ITU-R BT1700 NTSC
    AVCOL_TRC_SMPTE240M    =  7,
    AVCOL_TRC_LINEAR       =  8, ///< "Linear transfer characteristics"
    AVCOL_TRC_LOG          =  9, ///< "Logarithmic transfer characteristic (100:1 range)"
    AVCOL_TRC_LOG_SQRT     = 10, ///< "Logarithmic transfer characteristic (100 * Sqrt( 10 ) : 1 range)"
    AVCOL_TRC_IEC61966_2_4 = 11, ///< IEC 61966-2-4
    AVCOL_TRC_BT1361_ECG   = 12, ///< ITU-R BT1361 Extended Colour Gamut
    AVCOL_TRC_IEC61966_2_1 = 13, ///< IEC 61966-2-1 (sRGB or sYCC)
    AVCOL_TRC_BT2020_10    = 14, ///< ITU-R BT2020 for 10 bit system
    AVCOL_TRC_BT2020_12    = 15, ///< ITU-R BT2020 for 12 bit system
    AVCOL_TRC_NB                 ///< Not part of ABI
  );

(**
 *  X   X      3 4 X      X are luma samples,
 *             1 2        1-6 are possible chroma positions
 *  X   X      5 6 X      0 is undefined/unknown position
 *)
  TAVChromaLocation = (
    AVCHROMA_LOC_UNSPECIFIED = 0,
    AVCHROMA_LOC_LEFT        = 1, ///< mpeg2/4, h264 default
    AVCHROMA_LOC_CENTER      = 2, ///< mpeg1, jpeg, h263
    AVCHROMA_LOC_TOPLEFT     = 3, ///< DV
    AVCHROMA_LOC_TOP         = 4,
    AVCHROMA_LOC_BOTTOMLEFT  = 5,
    AVCHROMA_LOC_BOTTOM      = 6,
    AVCHROMA_LOC_NB               ///< Not part of ABI
  );
}
  TAVAudioServiceType =(
    AV_AUDIO_SERVICE_TYPE_MAIN              = 0,
    AV_AUDIO_SERVICE_TYPE_EFFECTS           = 1,
    AV_AUDIO_SERVICE_TYPE_VISUALLY_IMPAIRED = 2,
    AV_AUDIO_SERVICE_TYPE_HEARING_IMPAIRED  = 3,
    AV_AUDIO_SERVICE_TYPE_DIALOGUE          = 4,
    AV_AUDIO_SERVICE_TYPE_COMMENTARY        = 5,
    AV_AUDIO_SERVICE_TYPE_EMERGENCY         = 6,
    AV_AUDIO_SERVICE_TYPE_VOICE_OVER        = 7,
    AV_AUDIO_SERVICE_TYPE_KARAOKE           = 8,
    AV_AUDIO_SERVICE_TYPE_NB                     ///< Not part of ABI
  );

(**
 * @ingroup lavc_decoding
 *)
  PRcOverride = ^TRcOverride;
  TRcOverride = record {16}
    start_frame:    cint;
    end_frame:      cint;
    qscale:         cint; // if this is 0 then quality_factor will be used instead
    quality_factor: cfloat;
  end;

{$IFDEF FF_API_MAX_BFRAMES}
  
(**
 * @deprecated there is no libavcodec-wide limit on the number of B-frames
 *)
const
  FF_MAX_B_FRAMES = 16;
  
{$IFEND}
  
(* encoding support
   These flags can be passed in AVCodecContext.flags before initialization.
   Note: Not everything is supported yet.
*)

(**
 * Allow decoders to produce frames with data planes that are not aligned
 * to CPU requirements (e.g. due to cropping).
 *)
  CODEC_FLAG_UNALIGNED = $0001;
  CODEC_FLAG_QSCALE = $0002;  ///< Use fixed qscale.
  CODEC_FLAG_4MV    = $0004;  ///< 4 MV per MB allowed / advanced prediction for H263.
  CODEC_FLAG_OUTPUT_CORRUPT = $0008; ///< Output even those frames that might be corrupted
  CODEC_FLAG_QPEL   = $0010;  ///< use qpel MC.
{$IFDEF FF_API_GMC}
(**
 * @deprecated use the "gmc" private option of the libxvid encoder
 *)
  CODEC_FLAG_GMC    = $0020;  ///< use GMC.
{$IFEND}
{$IFDEF FF_API_MV0}
(**
 * @deprecated use the flag "mv0" in the "mpv_flags" private option of the
 * mpegvideo encoders
 *)
  CODEC_FLAG_MV0    = $0040;  ///< always try a MB with MV=<0,0>.
{$IFEND}
{$IFDEF FF_API_INPUT_PRESERVED}
(**
 * @deprecated passing reference-counted frames to the encoders replaces this
 * flag
 *)
  CODEC_FLAG_INPUT_PRESERVED    = $0100;
{$IFEND}
  CODEC_FLAG_PASS1              = $0200; ///< use internal 2pass ratecontrol in first  pass mode
  CODEC_FLAG_PASS2              = $0400; ///< use internal 2pass ratecontrol in second pass mode
  CODEC_FLAG_GRAY               = $2000; ///< only decode/encode grayscale
{$IFDEF FF_API_EMU_EDGE}
  (**
   * @deprecated edges are not used/required anymore. I.e. this flag is now always
   * set.
   *)
  CODEC_FLAG_EMU_EDGE           = $4000; ///< don't draw edges
{$IFEND}
  CODEC_FLAG_PSNR               = $8000; ///< error[?] variables will be set during encoding
  CODEC_FLAG_TRUNCATED      = $00010000; //** input bitstream might be truncated at a random
                                         //   location instead of only at frame boundaries */
{$IFDEF FF_API_NORMALIZE_AQP}
(**
 * @deprecated use the flag "naq" in the "mpv_flags" private option of the
 * mpegvideo encoders
 *)
  CODEC_FLAG_NORMALIZE_AQP  = $00020000; ///< normalize adaptive quantization
{$IFEND}
  CODEC_FLAG_INTERLACED_DCT = $00040000; ///< use interlaced dct
  CODEC_FLAG_LOW_DELAY      = $00080000; ///< force low delay
  CODEC_FLAG_GLOBAL_HEADER  = $00400000; ///< place global headers in extradata instead of every keyframe
  CODEC_FLAG_BITEXACT       = $00800000; ///< use only bitexact stuff (except (i)dct)
  (* Fx : Flag for h263+ extra options *)
  CODEC_FLAG_AC_PRED        = $01000000; ///< H263 Advanced intra coding / MPEG4 AC prediction
  CODEC_FLAG_LOOP_FILTER    = $00000800; ///< loop filter
  CODEC_FLAG_INTERLACED_ME  = $20000000; ///< interlaced motion estimation
  CODEC_FLAG_CLOSED_GOP     = $80000000;
  CODEC_FLAG2_FAST          = $00000001; ///< allow non spec compliant speedup tricks
  CODEC_FLAG2_NO_OUTPUT     = $00000004; ///< skip bitstream encoding
  CODEC_FLAG2_LOCAL_HEADER  = $00000008; ///< place global headers at every keyframe instead of in extradata
  CODEC_FLAG2_DROP_FRAME_TIMECODE = $00002000; ///< timecode is in drop frame format. DEPRECATED!!!!
  CODEC_FLAG2_IGNORE_CROP   = $00010000; ///< Discard cropping information from SPS.

  CODEC_FLAG2_CHUNKS        = $00008000; ///< Input bitstream might be truncated at a packet boundaries instead of only at frame boundaries.
  CODEC_FLAG2_SHOW_ALL      = $00400000; ///< Show all frames before the first keyframe
  CODEC_FLAG2_EXPORT_MVS    = $10000000; ///< Export motion vectors through frame side data
  CODEC_FLAG2_SKIP_MANUAL   = $20000000; ///< Do not skip samples and export skip information as frame side data

(* Unsupported options :
 *              Syntax Arithmetic coding (SAC)
 *              Reference Picture Selection
 *              Independant Segment Decoding *)
(* /Fx *)
(* codec capabilities *)

  CODEC_CAP_DRAW_HORIZ_BAND = $0001; ///< decoder can use draw_horiz_band callback

(**
 * Codec uses get_buffer() for allocating buffers and supports custom allocators.
 * If not set, it might not use get_buffer() at all or use operations that
 * assume the buffer was allocated by avcodec_default_get_buffer.
 *)
  CODEC_CAP_DR1             = $0002;
  CODEC_CAP_TRUNCATED       = $0008;
{$IFDEF FF_API_XVMC}
(* Codec can export data for HW decoding. This flag indicates that
 * the codec would call get_format() with list that might contain HW accelerated
 * pixel formats (XvMC, VDPAU, VAAPI, etc). The application can pick any of them
 * including raw image format.
 * The application can use the passed context to determine bitstream version,
 * chroma format, resolution etc.
 *)
  CODEC_CAP_HWACCEL         = $0010;
{$IFEND}
  
(**
 * Encoder or decoder requires flushing with NULL input at the end in order to
 * give the complete and correct output.
 *
 * NOTE: If this flag is not set, the codec is guaranteed to never be fed with
 *       with NULL data. The user can still send NULL data to the public encode
 *       or decode function, but libavcodec will not pass it along to the codec
 *       unless this flag is set.
 *
 * Decoders:
 * The decoder has a non-zero delay and needs to be fed with avpkt->data=NULL,
 * avpkt->size=0 at the end to get the delayed data until the decoder no longer
 * returns frames.
 *
 * Encoders:
 * The encoder needs to be fed with NULL data at the end of encoding until the
 * encoder no longer returns data.
 *
 * NOTE: For encoders implementing the AVCodec.encode2() function, setting this
 *       flag also means that the encoder must set the pts and duration for
 *       each output packet. If this flag is not set, the pts and duration will
 *       be determined by libavcodec from the input frame.
 *)
  CODEC_CAP_DELAY           = $0020;

(**
 * Codec can be fed a final frame with a smaller size.
 * This can be used to prevent truncation of the last audio samples.
 *)
  CODEC_CAP_SMALL_LAST_FRAME = $0040;

(**
 * Codec can export data for HW decoding (VDPAU).
 *)
  CODEC_CAP_HWACCEL_VDPAU    = $0080;

(**
 * Codec can output multiple frames per AVPacket
 * Normally demuxers return one frame at a time, demuxers which do not do
 * are connected to a parser to split what they return into proper frames.
 * This flag is reserved to the very rare category of codecs which have a
 * bitstream that cannot be split into frames without timeconsuming
 * operations like full decoding. Demuxers carring such bitstreams thus
 * may return multiple frames in a packet. This has many disadvantages like
 * prohibiting stream copy in many cases thus it should only be considered
 * as a last resort.
 *)
  CODEC_CAP_SUBFRAMES        = $0100;

(**
 * Codec is experimental and is thus avoided in favor of non experimental
 * encoders
 *)
  CODEC_CAP_EXPERIMENTAL     = $0200;

(**
 * Codec should fill in channel configuration and samplerate instead of container
 *)
  CODEC_CAP_CHANNEL_CONF     = $0400;

{$IFDEF FF_API_NEG_LINESIZES}
(**
 * @deprecated no codecs use this capability
 *)
  CODEC_CAP_NEG_LINESIZES    = $0800;
{$IFEND}

(**
 * Codec supports frame-level multithreading.
 *)
  CODEC_CAP_FRAME_THREADS    = $1000;

(**
 * Codec supports slice-based (or partition-based) multithreading.
 *)
  CODEC_CAP_SLICE_THREADS    = $2000;

(**
 * Codec supports changed parameters at any point.
 *)
  CODEC_CAP_PARAM_CHANGE     = $4000;

(**
 * Codec supports avctx->thread_count == 0 (auto).
 *)
  CODEC_CAP_AUTO_THREADS     = $8000;

(**
 * Audio encoder supports receiving a different number of samples in each call.
 *)
  CODEC_CAP_VARIABLE_FRAME_SIZE = $10000;

(**
 * Codec is lossless.
 *)
  CODEC_CAP_LOSSLESS         = $80000000;

{$IFDEF FF_API_MB_TYPE}
   //the following defines may change, don't expect compatibility if you use them
   MB_TYPE_INTRA4x4   = $001;
   MB_TYPE_INTRA16x16 = $002; //FIXME h264 specific
   MB_TYPE_INTRA_PCM  = $004; //FIXME h264 specific
   MB_TYPE_16x16      = $008;
   MB_TYPE_16x8       = $010;
   MB_TYPE_8x16       = $020;
   MB_TYPE_8x8        = $040;
   MB_TYPE_INTERLACED = $080;
   MB_TYPE_DIRECT2    = $100; //FIXME
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
{$IFEND}

(** Note: AVPanScan is now (28/09/2014) defined in libavutil/frame.pas to workaround a reference problem *)
   
{$IFDEF FF_API_QSCALE_TYPE}  
  FF_QSCALE_TYPE_MPEG1  = 0;
  FF_QSCALE_TYPE_MPEG2  = 1;
  FF_QSCALE_TYPE_H264   = 2;
  FF_QSCALE_TYPE_VP56   = 3;
{$ENDIF}
  
{$IFDEF FF_API_GET_BUFFER}
  FF_BUFFER_TYPE_INTERNAL = 1;
  FF_BUFFER_TYPE_USER     = 2; ///< Direct rendering buffers (image is (de)allocated by user)
  FF_BUFFER_TYPE_SHARED   = 4; ///< buffer from somewhere else, don't dealloc image (data/base), all other tables are not shared
  FF_BUFFER_TYPE_COPY     = 8; ///< just a (modified) copy of some other buffer, don't dealloc anything.

  FF_BUFFER_HINTS_VALID    = $01; // Buffer hints value is meaningful (if 0 ignore)
  FF_BUFFER_HINTS_READABLE = $02; // Codec will read from buffer
  FF_BUFFER_HINTS_PRESERVE = $04; // User must not alter buffer content
  FF_BUFFER_HINTS_REUSABLE = $08; // Codec will reuse the buffer (update)
{$ENDIF}

(**
 * The decoder will keep a reference to the frame and may reuse it later.
 *)
  AV_GET_BUFFER_FLAG_REF = 1 << 0;

  FF_COMPRESSION_DEFAULT = -1;
{$IFDEF FF_API_ASPECT_EXTENDED}
  FF_ASPECT_EXTENDED = 15;
{$ENDIF}
  
  FF_RC_STRATEGY_XVID = 1;

  FF_PRED_LEFT   = 0;
  FF_PRED_PLANE  = 1;
  FF_PRED_MEDIAN = 2;

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

{$IFDEF FF_API_AFD}
    {attribute_deprecated}
  FF_DTG_AFD_SAME         = 8;
  FF_DTG_AFD_4_3          = 9;
  FF_DTG_AFD_16_9         = 10;
  FF_DTG_AFD_14_9         = 11;
  FF_DTG_AFD_4_3_SP_14_9  = 13;
  FF_DTG_AFD_16_9_SP_14_9 = 14;
  FF_DTG_AFD_SP_4_3       = 15;
{$IFEND}

  FF_DEFAULT_QUANT_BIAS   = 999999;

  SLICE_FLAG_CODED_ORDER    = $0001; ///< draw_horiz_band() is called in coded order instead of display
  SLICE_FLAG_ALLOW_FIELD    = $0002; ///< allow draw_horiz_band() with field slices (MPEG2 field pics)
  SLICE_FLAG_ALLOW_PLANE    = $0004; ///< allow draw_horiz_band() with 1 component at a time (SVQ1)

  FF_MB_DECISION_SIMPLE = 0;        ///< uses mb_cmp
  FF_MB_DECISION_BITS   = 1;        ///< chooses the one which needs the fewest bits
  FF_MB_DECISION_RD     = 2;        ///< rate distortion

  FF_CODER_TYPE_VLC       = 0;
  FF_CODER_TYPE_AC        = 1;
  FF_CODER_TYPE_RAW       = 2;
  FF_CODER_TYPE_RLE       = 3;
{$IFDEF FF_API_UNUSED_MEMBERS}
  FF_CODER_TYPE_DEFLATE   = 4;
{$ENDIF}

  FF_BUG_AUTODETECT       = 1;  ///< autodetection
{$IFDEF FF_API_OLD_MSMPEG4}
  FF_BUG_OLD_MSMPEG4      = 2;
{$ENDIF}
  FF_BUG_XVID_ILACE       = 4;
  FF_BUG_UMP4             = 8;
  FF_BUG_NO_PADDING       = 16;
  FF_BUG_AMV              = 32;
{$IFDEF FF_API_AC_VLC}
  FF_BUG_AC_VLC           = 0;  ///< will be removed, libavcodec can now handle these non compliant files by default
{$ENDIF}  
  FF_BUG_QPEL_CHROMA      = 64;
  FF_BUG_STD_QPEL         = 128;
  FF_BUG_QPEL_CHROMA2     = 256;
  FF_BUG_DIRECT_BLOCKSIZE = 512;
  FF_BUG_EDGE             = 1024;
  FF_BUG_HPEL_CHROMA      = 2048;
  FF_BUG_DC_CLIP          = 4096;
  FF_BUG_MS               = 8192; ///< workaround various bugs in microsofts broken decoders
  FF_BUG_TRUNCATED        = 16384;

  FF_COMPLIANCE_VERY_STRICT   =  2; ///< strictly conform to an older more strict version of the spec or reference software
  FF_COMPLIANCE_STRICT        =  1; ///< strictly conform to all the things in the spec no matter what consequences
  FF_COMPLIANCE_NORMAL        =  0;
  FF_COMPLIANCE_UNOFFICIAL    = -1; ///< Allow unofficial extensions
  FF_COMPLIANCE_EXPERIMENTAL  = -2; ///< Allow nonstandardized experimental things.

  FF_EC_GUESS_MVS   = 1;
  FF_EC_DEBLOCK     = 2;
  FF_EC_FAVOR_INTER = 256;

  FF_DEBUG_PICT_INFO    = 1;
  FF_DEBUG_RC           = 2;
  FF_DEBUG_BITSTREAM    = 4;
  FF_DEBUG_MB_TYPE      = 8;
  FF_DEBUG_QP           = 16;
{$IFDEF FF_API_DEBUG_MV}
(**
 * @deprecated this option does nothing
 *)
  FF_DEBUG_MV           = 32;
{$ENDIF}  
  FF_DEBUG_DCT_COEFF    = $00000040;
  FF_DEBUG_SKIP         = $00000080;
  FF_DEBUG_STARTCODE    = $00000100;
{$IFDEF FF_API_UNUSED_MEMBERS}
  FF_DEBUG_PTS          = $00000200;
{$ENDIF}  
  FF_DEBUG_ER           = $00000400;
  FF_DEBUG_MMCO         = $00000800;
  FF_DEBUG_BUGS         = $00001000;
{$IFDEF FF_API_DEBUG_MV}
  FF_DEBUG_VIS_QP       = $00002000; ///< only access through AVOptions from outside libavcodec
  FF_DEBUG_VIS_MB_TYPE  = $00004000; ///< only access through AVOptions from outside libavcodec
{$ENDIF}
  FF_DEBUG_BUFFERS      = $00008000;
  FF_DEBUG_THREADS      = $00010000;
  FF_DEBUG_NOMC         = $01000000;
  
  FF_DEBUG_VIS_MV_P_FOR  = $00000001; //visualize forward predicted MVs of P frames
  FF_DEBUG_VIS_MV_B_FOR  = $00000002; //visualize forward predicted MVs of B frames
  FF_DEBUG_VIS_MV_B_BACK = $00000004; //visualize backward predicted MVs of B frames

(**
 * Verify checksums embedded in the bitstream (could be of either encoded or
 * decoded data, depending on the codec) and print an error message on mismatch.
 * If AV_EF_EXPLODE is also set, a mismatching checksum will result in the
 * decoder returning an error.
 *)  
  AV_EF_CRCCHECK  = 1;
  AV_EF_BITSTREAM = 2;          ///< detect bitstream specification deviations
  AV_EF_BUFFER    = 4;          ///< detect improper bitstream length
  AV_EF_EXPLODE   = 8;          ///< abort decoding on minor error detection

  AV_EF_IGNORE_ERR = 32768;     ///< ignore errors and continue
  AV_EF_CAREFUL    = 65536;     ///< consider things that violate the spec, are fast to calculate and have not been seen in the wild as errors
  AV_EF_COMPLIANT  = 131072;    ///< consider all spec non compliances as errors
  AV_EF_AGGRESSIVE = 262144;    ///< consider things that a sane encoder should not do as an error

  FF_DCT_AUTO    = 0;
  FF_DCT_FASTINT = 1;
{$IFDEF FF_API_UNUSED_MEMBERS}  
  FF_DCT_INT     = 2;
{$ENDIF}
  FF_DCT_MMX     = 3;
  FF_DCT_MLIB    = 4;
  FF_DCT_ALTIVEC = 5;
  FF_DCT_FAAN    = 6;

  FF_IDCT_AUTO         = 0;
  FF_IDCT_INT          = 1;
  FF_IDCT_SIMPLE       = 2;
  FF_IDCT_SIMPLEMMX    = 3;
  FF_IDCT_ARM          = 7;
  FF_IDCT_ALTIVEC      = 8;
{$IFDEF FF_API_ARCH_SH4}  
  FF_IDCT_SH4          = 9;
{$ENDIF}
  FF_IDCT_SIMPLEARM    = 10;
{$IFDEF FF_API_UNUSED_MEMBERS}  
  FF_IDCT_IPP          = 13;
{$ENDIF}
  FF_IDCT_XVID         = 14;
{$IFDEF FF_API_IDCT_XVIDMMX}  
  FF_IDCT_XVIDMMX      = 14;
{$ENDIF}
  FF_IDCT_SIMPLEARMV5TE= 16;
  FF_IDCT_SIMPLEARMV6  = 17;
{$IFDEF FF_API_ARCH_SPARC}  
  FF_IDCT_SIMPLEVIS    = 18;
{$ENDIF}
  FF_IDCT_FAAN         = 20;
  FF_IDCT_SIMPLENEON   = 22;
{$IFDEF FF_API_ARCH_ALPHA}  
  FF_IDCT_SIMPLEALPHA  = 23;
{$ENDIF}
  FF_IDCT_SIMPLEAUTO   = 128;

  FF_THREAD_FRAME   = 1; ///< Decode more than one frame at once
  FF_THREAD_SLICE   = 2; ///< Decode more than one part of a single frame at once

  FF_PROFILE_UNKNOWN  = -99;
  FF_PROFILE_RESERVED = -100;

  FF_PROFILE_AAC_MAIN = 0;
  FF_PROFILE_AAC_LOW  = 1;
  FF_PROFILE_AAC_SSR  = 2;
  FF_PROFILE_AAC_LTP  = 3;
  FF_PROFILE_AAC_HE   = 4;
  FF_PROFILE_AAC_HE_V2 = 28;
  FF_PROFILE_AAC_LD   = 22;
  FF_PROFILE_AAC_ELD  = 38;
  FF_PROFILE_MPEG2_AAC_LOW = 128;
  FF_PROFILE_MPEG2_AAC_HE  = 131;

  FF_PROFILE_DTS         = 20;
  FF_PROFILE_DTS_ES      = 30;
  FF_PROFILE_DTS_96_24   = 40;
  FF_PROFILE_DTS_HD_HRA  = 50;
  FF_PROFILE_DTS_HD_MA   = 60;

  FF_PROFILE_MPEG2_422   =  0;
  FF_PROFILE_MPEG2_HIGH  =  1;
  FF_PROFILE_MPEG2_SS    =  2;
  FF_PROFILE_MPEG2_SNR_SCALABLE = 3;
  FF_PROFILE_MPEG2_MAIN  =  4;
  FF_PROFILE_MPEG2_SIMPLE=  5;

  FF_PROFILE_H264_CONSTRAINED  = (1 shl 9);  // 8+1; constraint_set1_flag
  FF_PROFILE_H264_INTRA        = (1 shl 11); // 8+3; constraint_set3_flag

  FF_PROFILE_H264_BASELINE             = 66;
  FF_PROFILE_H264_CONSTRAINED_BASELINE = (66 or FF_PROFILE_H264_CONSTRAINED);
  FF_PROFILE_H264_MAIN                 = 77;
  FF_PROFILE_H264_EXTENDED             = 88;
  FF_PROFILE_H264_HIGH                 = 100;
  FF_PROFILE_H264_HIGH_10              = 110;
  FF_PROFILE_H264_HIGH_10_INTRA        = (110 or FF_PROFILE_H264_INTRA);
  FF_PROFILE_H264_HIGH_422             = 122;
  FF_PROFILE_H264_HIGH_422_INTRA       = (122 or FF_PROFILE_H264_INTRA);
  FF_PROFILE_H264_HIGH_444             = 144;
  FF_PROFILE_H264_HIGH_444_PREDICTIVE  = 244;
  FF_PROFILE_H264_HIGH_444_INTRA       = (244 or FF_PROFILE_H264_INTRA);
  FF_PROFILE_H264_CAVLC_444            = 44;

  FF_PROFILE_VC1_SIMPLE   = 0;
  FF_PROFILE_VC1_MAIN     = 1;
  FF_PROFILE_VC1_COMPLEX  = 2;
  FF_PROFILE_VC1_ADVANCED = 3;

  FF_PROFILE_MPEG4_SIMPLE                    = 0;
  FF_PROFILE_MPEG4_SIMPLE_SCALABLE           = 1;
  FF_PROFILE_MPEG4_CORE                      = 2;
  FF_PROFILE_MPEG4_MAIN                      = 3;
  FF_PROFILE_MPEG4_N_BIT                     = 4;
  FF_PROFILE_MPEG4_SCALABLE_TEXTURE          = 5;
  FF_PROFILE_MPEG4_SIMPLE_FACE_ANIMATION     = 6;
  FF_PROFILE_MPEG4_BASIC_ANIMATED_TEXTURE    = 7;
  FF_PROFILE_MPEG4_HYBRID                    = 8;
  FF_PROFILE_MPEG4_ADVANCED_REAL_TIME        = 9;
  FF_PROFILE_MPEG4_CORE_SCALABLE             = 10;
  FF_PROFILE_MPEG4_ADVANCED_CODING           = 11;
  FF_PROFILE_MPEG4_ADVANCED_CORE             = 12;
  FF_PROFILE_MPEG4_ADVANCED_SCALABLE_TEXTURE = 13;
  FF_PROFILE_MPEG4_SIMPLE_STUDIO             = 14;
  FF_PROFILE_MPEG4_ADVANCED_SIMPLE           = 15;

  FF_PROFILE_JPEG2000_CSTREAM_RESTRICTION_0  = 0;
  FF_PROFILE_JPEG2000_CSTREAM_RESTRICTION_1  = 1;
  FF_PROFILE_JPEG2000_CSTREAM_NO_RESTRICTION = 2;
  FF_PROFILE_JPEG2000_DCINEMA_2K             = 3;
  FF_PROFILE_JPEG2000_DCINEMA_4K             = 4;

  FF_PROFILE_HEVC_MAIN                       = 1;
  FF_PROFILE_HEVC_MAIN_10                    = 2;
  FF_PROFILE_HEVC_MAIN_STILL_PICTURE         = 3;
  FF_PROFILE_HEVC_REXT                       = 4;

  FF_LEVEL_UNKNOWN    = -99;

type
  TAVPacketSideDataType = (
    AV_PKT_DATA_PALETTE,
    AV_PKT_DATA_NEW_EXTRADATA,

    (**
     * An AV_PKT_DATA_PARAM_CHANGE side data packet is laid out as follows:
     * @code
     * u32le param_flags
     * if (param_flags & AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_COUNT)
     *     s32le channel_count
     * if (param_flags & AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_LAYOUT)
     *     u64le channel_layout
     * if (param_flags & AV_SIDE_DATA_PARAM_CHANGE_SAMPLE_RATE)
     *     s32le sample_rate
     * if (param_flags & AV_SIDE_DATA_PARAM_CHANGE_DIMENSIONS)
     *     s32le width
     *     s32le height
     * @endcode
     *)
    AV_PKT_DATA_PARAM_CHANGE,

    (**
     * An AV_PKT_DATA_H263_MB_INFO side data packet contains a number of
     * structures with info about macroblocks relevant to splitting the
     * packet into smaller packets on macroblock edges (e.g. as for RFC 2190).
     * That is, it does not necessarily contain info about all macroblocks,
     * as long as the distance between macroblocks in the info is smaller
     * than the target payload size.
     * Each MB info structure is 12 bytes, and is laid out as follows:
     * @code
     * u32le bit offset from the start of the packet
     * u8    current quantizer at the start of the macroblock
     * u8    GOB number
     * u16le macroblock address within the GOB
     * u8    horizontal MV predictor
     * u8    vertical MV predictor
     * u8    horizontal MV predictor for block number 3
     * u8    vertical MV predictor for block number 3
     * @endcode
     *)
    AV_PKT_DATA_H263_MB_INFO,

    (**
     * This side data should be associated with an audio stream and contains
     * ReplayGain information in form of the AVReplayGain struct.
     *9
    AV_PKT_DATA_REPLAYGAIN,

    (**
     * This side data contains a 3x3 transformation matrix describing an affine
     * transformation that needs to be applied to the decoded video frames for
     * correct presentation.
     *
     * See libavutil/display.h for a detailed description of the data.
     *)
    AV_PKT_DATA_DISPLAYMATRIX,

    (**
     * This side data should be associated with a video stream and contains
     * Stereoscopic 3D information in form of the AVStereo3D struct.
     *)
    AV_PKT_DATA_STEREO3D,

    (**
     * Recommmends skipping the specified number of samples
     * @code
     * u32le number of samples to skip from start of this packet
     * u32le number of samples to skip from end of this packet
     * u8    reason for start skip
     * u8    reason for end   skip (0=padding silence, 1=convergence)
     * @endcode
     *)
    AV_PKT_DATA_SKIP_SAMPLES=70,

    (**
     * An AV_PKT_DATA_JP_DUALMONO side data packet indicates that
     * the packet may contain "dual mono" audio specific to Japanese DTV
     * and if it is true, recommends only the selected channel to be used.
     * @code
     * u8    selected channels (0=mail/left, 1=sub/right, 2=both)
     * @endcode
     *)
    AV_PKT_DATA_JP_DUALMONO,

    (**
     * A list of zero terminated key/value strings. There is no end marker for
     * the list, so it is required to rely on the side data size to stop.
     *)
    AV_PKT_DATA_STRINGS_METADATA,

    (**
     * Subtitle event position
     * @code
     * u32le x1
     * u32le y1
     * u32le x2
     * u32le y2
     * @endcode
     *)
    AV_PKT_DATA_SUBTITLE_POSITION,

    (**
     * Data found in BlockAdditional element of matroska container. There is
     * no end marker for the data, so it is required to rely on the side data
     * size to recognize the end. 8 byte id (as found in BlockAddId) followed
     * by data.
     *)
    AV_PKT_DATA_MATROSKA_BLOCKADDITIONAL,

    (**
     * The optional first identifier line of a WebVTT cue.
     *)
    AV_PKT_DATA_WEBVTT_IDENTIFIER,

    (**
     * The optional settings (rendering instructions) that immediately
     * follow the timestamp specifier of a WebVTT cue.
     *)
    AV_PKT_DATA_WEBVTT_SETTINGS,
    
    (**
     * A list of zero terminated key/value strings. There is no end marker for
     * the list, so it is required to rely on the side data size to stop. This
     * side data includes updated metadata which appeared in the stream.
     *)
    AV_PKT_DATA_METADATA_UPDATE
  );

  PAVPacketSideData = ^TAVPacketSideData;
  TAVPacketSideData = record
    data:  PByte;
    size:  cint;
    type_: TAVPacketSideDataType;
  end;

(**
 * This structure stores compressed data. It is typically exported by demuxers
 * and then passed as input to decoders, or received as output from encoders and
 * then passed to muxers.
 *
 * For video, it should typically contain one compressed frame. For audio it may
 * contain several compressed frames.
 *
 * AVPacket is one of the few structs in FFmpeg, whose size is a part of public
 * ABI. Thus it may be allocated on stack and no new fields can be added to it
 * without libavcodec and libavformat major bump.
 *
 * The semantics of data ownership depends on the buf or destruct (deprecated)
 * fields. If either is set, the packet data is dynamically allocated and is
 * valid indefinitely until av_free_packet() is called (which in turn calls
 * av_buffer_unref()/the destruct callback to free the data). If neither is set,
 * the packet data is typically backed by some static buffer somewhere and is
 * only valid for a limited time (e.g. until the next read call when demuxing).
 *
 * The side data is always allocated with av_malloc() and is freed in
 * av_free_packet().
 *)
  PAVPacket = ^TAVPacket;
  TAVPacket = record
    (**
     * A reference to the reference-counted buffer where the packet data is
     * stored.
     * May be NULL, then the packet data is not reference-counted.
     *)
    buf:          PAVBufferRef;
    (*
     * Presentation timestamp in AVStream->time_base units; the time at which
     * the decompressed packet will be presented to the user.
     * Can be AV_NOPTS_VALUE if it is not stored in the file.
     * pts MUST be larger or equal to dts as presentation cannot happen before
     * decompression, unless one wants to view hex dumps. Some formats misuse
     * the terms dts and pts/cts to mean something different. Such timestamps
     * must be converted to true pts/dts before they are stored in AVPacket.
     *)
    pts:          cint64;
    (*
     * Decompression timestamp in AVStream->time_base units; the time at which
     * the packet is decompressed.
     * Can be AV_NOPTS_VALUE if it is not stored in the file.
     *)
    dts:          cint64;
    data:         PByteArray;
    size:         cint;
    stream_index: cint;
    (**
     * A combination of AV_PKT_FLAG values
     *)
    flags:        cint;
    (**
     * Additional packet data that can be provided by the container.
     * Packet can contain several types of side information.
     *)
    side_data: PAVPacketSideData;
    side_data_elems: cint;
    (*
     * Duration of this packet in AVStream->time_base units, 0 if unknown.
     * Equals next_pts - this_pts in presentation order.
     *)
    duration:     cint;
{$IFDEF FF_API_DESTRUCT_PACKET}
    destruct:     procedure (para1: PAVPacket); cdecl;
    priv:         pointer;
{$ENDIF}

    pos:          cint64;       // byte position in stream, -1 if unknown

    (*
     * Time difference in AVStream->time_base units from the pts of this
     * packet to the point at which the output from the decoder has converged
     * independent from the availability of previous frames. That is, the
     * frames are virtually identical no matter if decoding started from
     * the very first frame or from this keyframe.
     * Is AV_NOPTS_VALUE if unknown.
     * This field has no meaning if the packet does not have AV_PKT_FLAG_KEY
     * set.
     *
     * The purpose of this field is to allow seeking in streams that have no
     * keyframes in the conventional sense. It corresponds to the
     * recovery point SEI in H.264 and match_time_delta in NUT. It is also
     * essential for some types of subtitle streams to ensure that all
     * subtitles are correctly displayed after seeking.
     *)
    convergence_duration: cint64;
  end; {TAVPacket}

const
  AV_PKT_FLAG_KEY     = $0001; ///< The packet contains a keyframe
  AV_PKT_FLAG_CORRUPT = $0002; ///< The packet content is corrupted

  AV_NUM_DATA_POINTERS = 8;

type
  AVSideDataParamChangeFlags = (
    AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_COUNT  = $0001,
    AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_LAYOUT = $0002,
    AV_SIDE_DATA_PARAM_CHANGE_SAMPLE_RATE    = $0004,
    AV_SIDE_DATA_PARAM_CHANGE_DIMENSIONS     = $0008
  );

{ This is removed in avcodec.h. For the time being (2013) only commented
const
  FF_DECODE_ERROR_INVALID_BITSTREAM = 1;
  FF_DECODE_ERROR_MISSING_REFERENCE = 2;
}


{ This is removed in avcodec.h. For the time being (2013) only commented
(**
 * This structure describes decoded (raw) audio or video data.
 *
 * AVFrame must be allocated using avcodec_alloc_frame() and freed with
 * avcodec_free_frame(). Note that this allocates only the AVFrame itself. The
 * buffers for the data must be managed through other means.
 *
 * AVFrame is typically allocated once and then reused multiple times to hold
 * different data (e.g. a single AVFrame to hold frames received from a
 * decoder). In such a case, avcodec_get_frame_defaults() should be used to
 * reset the frame to its original clean state before it is reused again.
 *
 * sizeof(AVFrame) is not a part of the public ABI, so new fields may be added
 * to the end with a minor bump.
 * Similarly fields that are marked as to be only accessed by
 * av_opt_ptr() can be reordered. This allows 2 forks to add fields
 * without breaking compatibility with each other.
 *)
  PAVFrame = ^TAVFrame;
  TAVFrame = record
    (**
     * pointer to the picture/channel planes.
     * This might be different from the first allocated byte
     * - encoding: Set by user
     * - decoding: set by AVCodecContext.get_buffer()
     *)
    data: array [0..AV_NUM_DATA_POINTERS - 1] of pbyte;

    (**
     * Size, in bytes, of the data for each picture/channel plane.
     *
     * For audio, only linesize[0] may be set. For planar audio, each channel
     * plane must be the same size.
     *
     * - encoding: Set by user
     * - decoding: set by AVCodecContext.get_buffer()
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
     * extended_data must be used by the decoder in order to access all
     * channels.
     *
     * encoding: set by user
     * decoding: set by AVCodecContext.get_buffer()
     *)
    extended_data: pointer of pbyte;

    (**
     * width and height of the video frame
     * - encoding: unused
     * - decoding: Read by user.
     *)
    width, height: cint;

    (**
     * number of audio samples (per channel) described by this frame
     * - encoding: Set by user
     * - decoding: Set by libavcodec
     *)
    nb_samples: cint;

    (**
     * format of the frame, -1 if unknown or unset
     * Values correspond to enum AVPixelFormat for video frames,
     * enum AVSampleFormat for audio)
     * - encoding: unused
     * - decoding: Read by user.
     *)
    format: cint;

    (**
     * 1 -> keyframe, 0-> not
     * - encoding: Set by libavcodec.
     * - decoding: Set by libavcodec.
     *)
    key_frame: cint;

    (**
     * Picture type of the frame, see ?_TYPE below.
     * - encoding: Set by libavcodec. for coded_picture (and set by user for input).
     * - decoding: Set by libavcodec.
     *)
    pict_type: TAVPictureType;

    (**
     * pointer to the first allocated byte of the picture. Can be used in get_buffer/release_buffer.
     * This isn't used by libavcodec unless the default get/release_buffer() is used.
     * - encoding:
     * - decoding:
     *)
    base: array [0..AV_NUM_DATA_POINTERS - 1] of pbyte;

    (**
     * sample aspect ratio for the video frame, 0/1 if unknown/unspecified
     * - encoding: unused
     * - decoding: Read by user.
     *)
    sample_aspect_ratio: TAVRational;

    (**
     * presentation timestamp in time_base units (time when frame should be shown to user)
     * If AV_NOPTS_VALUE then frame_rate = 1/time_base will be assumed.
     * - encoding: MUST be set by user.
     * - decoding: Set by libavcodec.
     *)
    pts: cint64;

    (**
     * pts copied from the AVPacket that was decoded to produce this frame
     * - encoding: unused
     * - decoding: Read by user.
     *)
    pkt_pts: cint64;

    (**
     * dts copied from the AVPacket that triggered returning this frame
     * - encoding: unused
     * - decoding: Read by user.
     *)
    pkt_dts: cint64;

    (**
     * picture number in bitstream order
     * - encoding: set by
     * - decoding: Set by libavcodec.
     *)
    coded_picture_number: cint;

    (**
     * picture number in display order
     * - encoding: set by
     * - decoding: Set by libavcodec.
     *)
    display_picture_number: cint;

    (**
     * quality (between 1 (good) and FF_LAMBDA_MAX (bad))
     * - encoding: Set by libavcodec. for coded_picture (and set by user for input).
     * - decoding: Set by libavcodec.
     *)
    quality: cint;

    (**
     * is this picture used as reference
     * The values for this are the same as the MpegEncContext.picture_structure
     * variable, that is 1->top field, 2->bottom field, 3->frame/both fields.
     * Set to 4 for delayed, non-reference frames.
     * - encoding: unused
     * - decoding: Set by libavcodec. (before get_buffer() call)).
     *)
    reference: cint;

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
    qstride: cint;

    (**
     *
     *)
    qscale_type: cint;

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
    mb_type: PCuint;

    (**
     * DCT coefficients
     * - encoding: unused
     * - decoding: Set by libavcodec.
     *)
    dct_coeff: PsmallInt;

    (**
     * motion reference frame index
     * the order in which these are stored can depend on the codec.
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    ref_index: array [0..1] of PShortint;

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
    error: array [0..AV_NUM_DATA_POINTERS - 1] of cuint64;

    (**
     * type of the buffer (to keep track of who has to deallocate data[*])
     * - encoding: Set by the one who allocates it.
     * - decoding: Set by the one who allocates it.
     * Note: User allocated (direct rendering) & internal buffers cannot coexist currently.
     *)
    type_: cint;

    (**
     * When decoding, this signals how much the picture must be delayed.
     * extra_delay = repeat_pict / (2*fps)
     * - encoding: unused
     * - decoding: Set by libavcodec.
     *)
    repeat_pict: cint;

    (**
     * The content of the picture is interlaced.
     * - encoding: Set by user.
     * - decoding: Set by libavcodec. (default 0)
     *)
    interlaced_frame: cint;

    (**
     * If the content is interlaced, is top field displayed first.
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    top_field_first: cint;

    (**
     * Tell user application that palette has changed from previous frame.
     * - encoding: ??? (no palette-enabled encoder yet)
     * - decoding: Set by libavcodec. (default 0).
     *)
    palette_has_changed: cint;

    (**
     * codec suggestion on buffer type if != 0
     * - encoding: unused
     * - decoding: Set by libavcodec. (before get_buffer() call)).
     *)
    buffer_hints: cint;

    (**
     * Pan scan.
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    pan_scan: PAVPanScan;

    (**
     * reordered opaque 64bit number (generally a PTS) from AVCodecContext.reordered_opaque
     * PTS but can be anything).
     * The user sets AVCodecContext.reordered_opaque to represent the input at
     * that time,
     * the decoder reorders values as needed and sets AVFrame.reordered_opaque
     * to exactly one of the values provided by the user through AVCodecContext.reordered_opaque
     * @deprecated in favor of pkt_pts
     * - encoding: unused
     * - decoding: Read by user.
     *)
    reordered_opaque: cint64;

    (**
     * hardware accelerator private data (FFmpeg allocated)
     * - encoding: unused
     * - decoding: Set by libavcodec
     *)
    hwaccel_picture_private: pointer;

    (**
     * the AVCodecContext which ff_thread_get_buffer() was last called on
     * - encoding: Set by libavcodec.
     * - decoding: Set by libavcodec.
     *)
    owner: PAVCodecContext;

    (**
     * used by multithreading to store frame-specific info
     * - encoding: Set by libavcodec.
     * - decoding: Set by libavcodec.
     *)
    thread_opaque: pointer;

    (**
     * log2 of the size of the block which a single vector in motion_val represents:
     * (4->16x16, 3->8x8, 2-> 4x4, 1-> 2x2)
     * - encoding: unused
     * - decoding: Set by libavcodec.
     *)
    motion_subsample_log2: cuint8;

    (**
     * Sample rate of the audio data.
     *
     * - encoding: unused
     * - decoding: read by user
     *)
    sample_rate: cint;

    (**
     * Channel layout of the audio data.
     *
     * - encoding: unused
     * - decoding: read by user.
     *)
    channel_layout: cuint64;

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
  end; (** TAVFrame **)

(**
 * Accessors for some AVFrame fields.
 * The position of these field in the structure is not part of the ABI,
 * they should not be accessed directly outside libavcodec.
 *)
function  av_frame_get_best_effort_timestamp(frame: {const PAVFrame): cint64;
  cdecl; external av__codec;
procedure av_frame_set_best_effort_timestamp(frame: PAVFrame; val: cint64);
  cdecl; external av__codec;
function  av_frame_get_pkt_duration         (frame: {const PAVFrame): cint64;
  cdecl; external av__codec;
procedure av_frame_get_pkt_duration         (frame: PAVFrame; val: cint64);
  cdecl; external av__codec;
function  av_frame_get_pkt_pos              (frame: {const PAVFrame): cint64;
  cdecl; external av__codec;
procedure av_frame_get_pkt_pos              (frame: PAVFrame; val: cint64);
  cdecl; external av__codec;
function  av_frame_get_channel_layout       (frame: {const PAVFrame): cint64;
  cdecl; external av__codec;
procedure av_frame_get_channel_layout       (frame: PAVFrame; val: cint64);
  cdecl; external av__codec;
function  av_frame_get_channels             (frame: {const PAVFrame): cint;
  cdecl; external av__codec;
procedure av_frame_set_channels             (frame: PAVFrame; val: cint);
  cdecl; external av__codec;
function  av_frame_get_sample_rate          (frame: {const PAVFrame): cint;
  cdecl; external av__codec;
procedure av_frame_set_sample_rate          (frame: PAVFrame; val: cint);
  cdecl; external av__codec;
function  av_frame_get_metadata             (frame: {const PAVFrame): PAVDictionary;
  cdecl; external av__codec;
procedure av_frame_set_metadata             (frame: PAVFrame; val: PAVDictionary);
  cdecl; external av__codec;
function  av_frame_get_decode_error_flags   (frame: {const PAVFrame): cint;
  cdecl; external av__codec;
procedure av_frame_set_decode_error_flags   (frame: PAVFrame; val: cint);
  cdecl; external av__codec;
function  av_frame_get_pkt_size(frame: {const PAVFrame): cint;
  cdecl; external av__codec;
procedure av_frame_set_pkt_size(frame: PAVFrame; val: cint);
  cdecl; external av__codec;
}

type
  TAVCodecInternal = record
  end;

  PAVCodecInternal = ^TAVCodecInternal;
  PAVCodecContext = ^TAVCodecContext;

  PAVClass = ^TAVClass;

  PPAVCodec = ^PAVCodec;
  PAVCodec = ^TAVCodec;

  PAVHWAccel = ^TAVHWAccel;

  // int[4]
  PAVNDPArray = ^TAVNDPArray;
  TAVNDPArray = array [0..AV_NUM_DATA_POINTERS - 1] of cint;
  // int (*func)(struct AVCodecContext *c2, void *arg)
  TExecuteFunc = function(c2: PAVCodecContext; arg: Pointer): cint; cdecl;
  // int (*func)(struct AVCodecContext *c2, void *arg, int jobnr, int threadnr)
  TExecute2Func = function(c2: PAVCodecContext; arg: Pointer; jobnr: cint; threadnr: cint): cint; cdecl;

  TAVFieldOrder = (
    AV_FIELD_UNKNOWN,
    AV_FIELD_PROGRESSIVE,
    AV_FIELD_TT,          //< Top coded_first, top displayed first
    AV_FIELD_BB,          //< Bottom coded first, bottom displayed first
    AV_FIELD_TB,          //< Top coded first, bottom displayed first
    AV_FIELD_BT          //< Bottom coded first, top displayed first
    );

(**
 * main external API structure.
 * New fields can be added to the end with minor version bumps.
 * Removal, reordering and changes to existing fields require a major
 * version bump.
 * Please use AVOptions (av_opt* / av_set/get*()) to access these fields from user
 * applications.
 * sizeof(AVCodecContext) must not be used outside libav*.
 *)
  TAVCodecContext = record {720}
    (**
     * information on struct for av_log
     * - set by avcodec_alloc_context3
     *)
    av_class: PAVClass;
    log_level_offset: cint;

    codec_type: TAVMediaType; (* see AVMEDIA_TYPE_xxx *)
    codec:      PAVCodec;
{$IFDEF FF_API_CODEC_NAME}
    (**
     * @deprecated this field is not used for anything in libavcodec
     *)
    {attribute_deprecated}
    codec_name: array [0..31] of AnsiChar;
{$IFEND}
    codec_id:   TAVCodecID; (* see AV_CODEC_ID_xxx *)

    (**
     * fourcc (LSB first, so "ABCD" -> ('D'<<24) + ('C'<<16) + ('B'<<8) + 'A').
     * This is used to work around some encoder bugs.
     * A demuxer should set this to what is stored in the field used to identify the codec.
     * If there are multiple such fields in a container then the demuxer should choose the one
     * which maximizes the information about the used codec.
     * If the codec tag field in a container is larger than 32 bits then the demuxer should
     * remap the longer ID to 32 bits with a table or other structure. Alternatively a new
     * extra_codec_tag + size could be added but for this a clear advantage must be demonstrated
     * first.
     * - encoding: Set by user, if not then the default based on codec_id will be used.
     * - decoding: Set by user, will be converted to uppercase by libavcodec during init.
     *)
    codec_tag: cuint;

    (**
     * fourcc from the AVI stream header (LSB first, so "ABCD" -> ('D'<<24) + ('C'<<16) + ('B'<<8) + 'A').
     * This is used to work around some encoder bugs.
     * - encoding: unused
     * - decoding: Set by user, will be converted to uppercase by libavcodec during init.
     *)
    stream_codec_tag: cuint;

    priv_data: pointer;

    (**
     * Private context used for internal data.
     *
     * Unlike priv_data, this is not codec-specific. It is used in general
     * libavcodec functions.
     *)
    internal: PAVCodecInternal;

    (**
     * Private data of the user, can be used to carry app specific stuff.
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    opaque: pointer;

    (**
     * the average bitrate
     * - encoding: Set by user; unused for constant quantizer encoding.
     * - decoding: Set by libavcodec. 0 or some bitrate if this info is available in the stream.
     *)
    bit_rate: cint;

    (**
     * number of bits the bitstream is allowed to diverge from the reference.
     *           the reference can be CBR (for CBR pass1) or VBR (for pass2)
     * - encoding: Set by user; unused for constant quantizer encoding.
     * - decoding: unused
     *)
    bit_rate_tolerance: cint;

    (**
     * Global quality for codecs which cannot change it per frame.
     * This should be proportional to MPEG-1/2/4 qscale.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    global_quality: cint;

    (**
     * - encoding: Set by user.
     * - decoding: unused
     *)
    compression_level: cint;

    (**
     * CODEC_FLAG_*.
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    flags: cint;

    (**
     * CODEC_FLAG2_*
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    flags2: cint;

    (**
     * some codecs need / can use extradata like Huffman tables.
     * mjpeg: Huffman tables
     * rv10: additional flags
     * mpeg4: global headers (they can be in the bitstream or here)
     * The allocated memory should be FF_INPUT_BUFFER_PADDING_SIZE bytes larger
     * than extradata_size to avoid problems if it is read with the bitstream reader.
     * The bytewise contents of extradata must not depend on the architecture or CPU endianness.
     * - encoding: Set/allocated/freed by libavcodec.
     * - decoding: Set/allocated/freed by user.
     *)
    extradata: pbyte;
    extradata_size: cint;

    (**
     * This is the fundamental unit of time (in seconds) in terms
     * of which frame timestamps are represented. For fixed-fps content,
     * timebase should be 1/framerate and timestamp increments should be
     * identically 1.
     * This often, but not always is the inverse of the frame rate or field rate
     * for video.
     * - encoding: MUST be set by user.
     * - decoding: the use of this field for decoding is deprecated.
     *             Use framerate instead.
     *)
    time_base: TAVRational;

    (**
     * For some codecs, the time base is closer to the field rate than the frame rate.
     * Most notably, H.264 and MPEG-2 specify time_base as half of frame duration
     * if no telecine is used ...
     *
     * Set to time_base ticks per frame. Default 1, e.g., H.264/MPEG-2 set it to 2.
     *)
    ticks_per_frame: cint;

    (**
     * Codec delay.
     *
     * Encoding: Number of frames delay there will be from the encoder input to
     *           the decoder output. (we assume the decoder matches the spec)
     * Decoding: Number of frames delay in addition to what a standard decoder
     *           as specified in the spec would produce.
     *
     * Video:
     *   Number of frames the decoded output will be delayed relative to the
     *   encoded input.
     *
     * Audio:
     *   For encoding, this field is unused (see initial_padding).
     *
     *   For decoding, this is the number of samples the decoder needs to
     *   output before the decoder's output is valid. When seeking, you should
     *   start decoding this many samples prior to your desired seek point.
     *
     * - encoding: Set by libavcodec.
     * - decoding: Set by libavcodec.
     *)
    delay: cint;

    (* video only *)
    (**
     * picture width / height.
     * - encoding: MUST be set by user.
     * - decoding: May be set by the user before opening the decoder if known e.g.
     *             from the container. Some decoders will require the dimensions
     *             to be set by the caller. During decoding, the decoder may
     *             overwrite those values as required.
     *)
    width, height: cint;

    (**
     * Bitstream width / height, may be different from width/height e.g. when
     * the decoded frame is cropped before being output or lowres is enabled.
     * - encoding: unused
     * - decoding: May be set by the user before opening the decoder if known
     *             e.g. from the container. During decoding, the decoder may
     *             overwrite those values as required.
     *)
    coded_width, coded_height: cint;

    (**
     * the number of pictures in a group of pictures, or 0 for intra_only
     * - encoding: Set by user.
     * - decoding: unused
     *)
    gop_size: cint;

    (**
     * Pixel format, see AV_PIX_FMT_xxx.
     * May be set by the demuxer if known from headers.
     * May be overridden by the decoder if it knows better.
     * - encoding: Set by user.
     * - decoding: Set by user if known, overridden by libavcodec if known
     *)
    pix_fmt: TAVPixelFormat;

    (**
     * Motion estimation algorithm used for video coding.
     * 1 (zero), 2 (full), 3 (log), 4 (phods), 5 (epzs), 6 (x1), 7 (hex),
     * 8 (umh), 9 (iter), 10 (tesa) [7, 8, 10 are x264 specific, 9 is snow specific]
     * - encoding: MUST be set by user.
     * - decoding: unused
     *)
    me_method: cint;

    (**
     * If non NULL, 'draw_horiz_band' is called by the libavcodec
     * decoder to draw a horizontal band. It improves cache usage. Not
     * all codecs can do that. You must check the codec capabilities
     * beforehand.
     * The function is also used by hardware acceleration APIs.
     * It is called at least once during frame decoding to pass
     * the data needed for hardware render.
     * In that mode instead of pixel data, AVFrame points to
     * a structure specific to the acceleration API. The application
     * reads the structure and can change some fields to indicate progress
     * or mark state.
     * - encoding: unused
     * - decoding: Set by user.
     * @param height the height of the slice
     * @param y the y position of the slice
     * @param type 1->top field, 2->bottom field, 3->frame
     * @param offset offset into the AVFrame.data from which the slice should be read
     *)
    draw_horiz_band: procedure (s: PAVCodecContext;
                                src: {const} PAVFrame; offset: PAVNDPArray;
                                y: cint; type_: cint; height: cint); cdecl;

    (**
     * callback to negotiate the pixelFormat
     * @param fmt is the list of formats which are supported by the codec,
     * it is terminated by -1 as 0 is a valid format, the formats are ordered by quality.
     * The first is always the native one.
     * @note The callback may be called again immediately if initialization for
     * the selected (hardware-accelerated) pixel format failed.
     * @warning Behavior is undefined if the callback returns a value not
     * in the fmt list of formats.
     * @return the chosen format
     * - encoding: unused
     * - decoding: Set by user, if not set the native format will be chosen.
     *)
    get_format: function (s: PAVCodecContext; fmt: {const} PAVPixelFormat): TAVPixelFormat; cdecl;

    (**
     * maximum number of B-frames between non-B-frames
     * Note: The output will be delayed by max_b_frames+1 relative to the input.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    max_b_frames: cint;

    (**
     * qscale factor between IP and B-frames
     * If > 0 then the last P-frame quantizer will be used (q= lastp_q*factor+offset).
     * If < 0 then normal ratecontrol will be done (q= -normal_q*factor+offset).
     * - encoding: Set by user.
     * - decoding: unused
     *)
    b_quant_factor: cfloat;

    (** obsolete FIXME remove *)
    rc_strategy: cint;

    b_frame_strategy: cint;

    (**
     * qscale offset between IP and B-frames
     * - encoding: Set by user.
     * - decoding: unused
     *)
    b_quant_offset: cfloat;

    (**
     * Size of the frame reordering buffer in the decoder.
     * For MPEG-2 it is 1 IPB or 0 low delay IP.
     * - encoding: Set by libavcodec.
     * - decoding: Set by libavcodec.
     *)
    has_b_frames: cint;

    (**
     * 0-> h263 quant 1-> mpeg quant
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mpeg_quant: cint;

    (**
     * qscale factor between P and I-frames
     * If > 0 then the last p frame quantizer will be used (q= lastp_q*factor+offset).
     * If < 0 then normal ratecontrol will be done (q= -normal_q*factor+offset).
     * - encoding: Set by user.
     * - decoding: unused
     *)
    i_quant_factor: cfloat;

    (**
     * qscale offset between P and I-frames
     * - encoding: Set by user.
     * - decoding: unused
     *)
    i_quant_offset: cfloat;

    (**
     * luminance masking (0-> disabled)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    lumi_masking: cfloat;

    (**
     * temporary complexity masking (0-> disabled)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    temporal_cplx_masking: cfloat;

    (**
     * spatial complexity masking (0-> disabled)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    spatial_cplx_masking: cfloat;

    (**
     * p block masking (0-> disabled)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    p_masking: cfloat;

    (**
     * darkness masking (0-> disabled)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    dark_masking: cfloat;

    (**
     * slice count
     * - encoding: Set by libavcodec.
     * - decoding: Set by user (or 0).
     *)
    slice_count: cint;

    (**
     * prediction method (needed for huffyuv)
     * - encoding: Set by user.
     * - decoding: unused
     *)
     prediction_method: cint;

    (**
     * slice offsets in the frame in bytes
     * - encoding: Set/allocated by libavcodec.
     * - decoding: Set/allocated by user (or NULL).
     *)
    slice_offset: PCint;

    (**
     * sample aspect ratio (0 if unknown)
     * That is the width of a pixel divided by the height of the pixel.
     * Numerator and denominator must be relatively prime and smaller than 256 for some video standards.
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    sample_aspect_ratio: TAVRational;

    (**
     * motion estimation comparison function
     * - encoding: Set by user.
     * - decoding: unused
     *)
    me_cmp: cint;

    (**
     * subpixel motion estimation comparison function
     * - encoding: Set by user.
     * - decoding: unused
     *)
    me_sub_cmp: cint;
    (**
     * macroblock comparison function (not supported yet)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mb_cmp: cint;
    (**
     * interlaced DCT comparison function
     * - encoding: Set by user.
     * - decoding: unused
     *)
    ildct_cmp: cint;

    (**
     * ME diamond size & shape
     * - encoding: Set by user.
     * - decoding: unused
     *)
    dia_size: cint;

    (**
     * amount of previous MV predictors (2a+1 x 2a+1 square)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    last_predictor_count: cint;

    (**
     * prepass for motion estimation
     * - encoding: Set by user.
     * - decoding: unused
     *)
    pre_me: cint;

    (**
     * motion estimation prepass comparison function
     * - encoding: Set by user.
     * - decoding: unused
     *)
    me_pre_cmp: cint;

    (**
     * ME prepass diamond size & shape
     * - encoding: Set by user.
     * - decoding: unused
     *)
    pre_dia_size: cint;

    (**
     * subpel ME quality
     * - encoding: Set by user.
     * - decoding: unused
     *)
    me_subpel_quality: cint;

{$IFDEF FF_API_AFD}
    (**
     * DTG active format information (additional aspect ratio
     * information only used in DVB MPEG-2 transport streams)
     * 0 if not set.
     *
     * - encoding: unused
     * - decoding: Set by decoder.
     * @deprecated Deprecated in favor of AVSideData
     *)
    {attribute_deprecated}
    dtg_active_format: cint;
{$IFEND}

    (**
     * maximum motion estimation search range in subpel units
     * If 0 then no limit.
     *
     * - encoding: Set by user.
     * - decoding: unused
     *)
    me_range: cint;

    (**
     * intra quantizer bias
     * - encoding: Set by user.
     * - decoding: unused
     *)
    intra_quant_bias: cint;

    (**
     * inter quantizer bias
     * - encoding: Set by user.
     * - decoding: unused
     *)
    inter_quant_bias: cint;

    (**
     * slice flags
     * - encoding: unused
     * - decoding: Set by user.
     *)
    slice_flags: cint;

{$IFDEF FF_API_XVMC}
    (**
     * XVideo Motion Acceleration
     * - encoding: forbidden
     * - decoding: set by decoder
     * @deprecated XvMC doesn't need it anymore.    
     *)
    xvmc_acceleration: cint;
{$ENDIF}
    
    (**
     * macroblock decision mode
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mb_decision: cint;

    (**
     * custom intra quantization matrix
     * - encoding: Set by user, can be NULL.
     * - decoding: Set by libavcodec.
     *)
    intra_matrix: PWord;

    (**
     * custom inter quantization matrix
     * - encoding: Set by user, can be NULL.
     * - decoding: Set by libavcodec.
     *)
    inter_matrix: PWord;

    (**
     * scene change detection threshold
     * 0 is default, larger means fewer detected scene changes.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    scenechange_threshold: cint;

    (**
     * noise reduction strength
     * - encoding: Set by user.
     * - decoding: unused
     *)
    noise_reduction: cint;

{$IFDEF FF_API_MPV_OPT}
    (**
     * @deprecated this field is unused
     *)
     {attribute_deprecated}
     me_threshold: cint;

    (**
     * @deprecated this field is unused
     *)
     {attribute_deprecated}
     mb_threshold: cint;
{$ENDIF}
    (**
     * precision of the intra DC coefficient - 8
     * - encoding: Set by user.
     * - decoding: unused
     *)
    intra_dc_precision: cint;

    (**
     * Number of macroblock rows at the top which are skipped.
     * - encoding: unused
     * - decoding: Set by user.
     *)
    skip_top: cint;

    (**
     * Number of macroblock rows at the bottom which are skipped.
     * - encoding: unused
     * - decoding: Set by user.
     *)
    skip_bottom: cint;

{$IFDEF FF_API_MPV_OPT}
    (**
     * @deprecated use encoder private options instead
     *)
    {attribute_deprecated}
    border_masking: cfloat;
{$ENDIF}
    (**
     * minimum MB lagrange multipler
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mb_lmin: cint;

    (**
     * maximum MB lagrange multipler
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mb_lmax: cint;

    (**
     *
     * - encoding: Set by user.
     * - decoding: unused
     *)
    me_penalty_compensation: cint;

    (**
     *
     * - encoding: Set by user.
     * - decoding: unused
     *)
    bidir_refine: cint;

    (**
     *
     * - encoding: Set by user.
     * - decoding: unused
     *)
    brd_scale: cint;

    (**
     * minimum GOP size
     * - encoding: Set by user.
     * - decoding: unused
     *)
    keyint_min: cint;

    (**
     * number of reference frames
     * - encoding: Set by user.
     * - decoding: Set by lavc.
     *)
    refs: cint;

    (**
     * chroma qp offset from luma
     * - encoding: Set by user.
     * - decoding: unused
     *)
    chromaoffset: cint;

{$IFDEF FF_API_UNUSED_MEMBERS}
    (**
     * Multiplied by qscale for each frame and added to scene_change_score.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    scenechange_factor: cint; {attribute_deprecated}
{$IFEND}

    (**
     *
     * Note: Value depends upon the compare function used for fullpel ME.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mv0_threshold: cint;

    (**
     * Adjust sensitivity of b_frame_strategy 1.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    b_sensitivity: cint;

    (**
     * Chromaticity coordinates of the source primaries.
     * - encoding: Set by user
     * - decoding: Set by libavcodec
     *)
    color_primaries: TAVColorPrimaries;

    (**
     * Color Transfer Characteristic.
     * - encoding: Set by user
     * - decoding: Set by libavcodec
     *)
    color_trc: TAVColorTransferCharacteristic;

    (**
     * YUV colorspace type.
     * - encoding: Set by user
     * - decoding: Set by libavcodec
     *)
    colorspace: TAVColorSpace;

    (**
     * MPEG vs JPEG YUV range.
     * - encoding: Set by user
     * - decoding: Set by libavcodec
     *)
    color_range: TAVColorRange;

    (**
     * This defines the location of chroma samples.
     * - encoding: Set by user
     * - decoding: Set by libavcodec
     *)
     chroma_sample_location: TAVChromaLocation;

    (**
     * Number of slices.
     * Indicates number of picture subdivisions. Used for parallelized
     * decoding.
     * - encoding: Set by user
     * - decoding: unused
     *)
    slices: cint;

    (** Field order
     * - encoding: set by libavcodec
     * - decoding: Set by user.
     *)
    field_order: TAVFieldOrder;

    (* audio only *)
    sample_rate: cint; ///< samples per second
    channels: cint;    ///< number of audio channels

    (**
     * audio sample format
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    sample_fmt: TAVSampleFormat;  ///< sample format

    (* The following data should not be initialized. *)
    (**
     * Number of samples per channel in an audio frame.
     *
     * - encoding: set by libavcodec in avcodec_open2(). Each submitted frame
     *   except the last must contain exactly frame_size samples per channel.
     *   May be 0 when the codec has CODEC_CAP_VARIABLE_FRAME_SIZE set, then the
     *   frame size is not restricted.
     * - decoding: may be set by some decoders to indicate constant frame size
     *)
    frame_size: cint;

    (**
     * Frame counter, set by libavcodec.
     *
     * - decoding: total number of frames returned from the decoder so far.
     * - encoding: total number of frames passed to the encoder so far.
     *
     *   @note the counter is not incremented if encoding/decoding resulted in
     *   an error.
     *)
    frame_number: cint;   ///< audio or video frame number

    (**
     * number of bytes per packet if constant and known or 0
     * Used by some WAV based audio codecs.
     *)
    block_align: cint;

    (**
     * Audio cutoff bandwidth (0 means "automatic")
     * - encoding: Set by user.
     * - decoding: unused
     *)
    cutoff: cint;

{$IFDEF FF_API_REQUEST_CHANNELS}
    (**
     * Decoder should decode to this many channels if it can (0 for default)
     * - encoding: unused
     * - decoding: Set by user.
     * @deprecated Deprecated in favor of request_channel_layout.
     *)
    request_channels: cint; {deprecated}
{$IFEND}

    (**
     * Audio channel layout.
     * - encoding: set by user.
     * - decoding: set by user, may be overwritten by libavcodec.
     *)
    channel_layout: cuint64;

    (**
     * Request decoder to use this channel layout if it can (0 for default)
     * - encoding: unused
     * - decoding: Set by user.
     *)
    request_channel_layout: cuint64;

    (**
     * Type of service that the audio stream conveys.
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    audio_service_type: TAVAudioServiceType;

    (**
     * desired sample format
     * - encoding: Not used.
     * - decoding: Set by user.
     * Decoder will decode to this format if it can.
     *)
    request_sample_fmt: TAVSampleFormat;

{$IFDEF FF_API_GET_BUFFER}
    (**
     * Called at the beginning of each frame to get a buffer for it.
     *
     * The function will set AVFrame.data[], AVFrame.linesize[].
     * AVFrame.extended_data[] must also be set, but it should be the same as
     * AVFrame.data[] except for planar audio with more channels than can fit
     * in AVFrame.data[]. In that case, AVFrame.data[] shall still contain as
     * many data pointers as it can hold.
     *
     * if CODEC_CAP_DR1 is not set then get_buffer() must call
     * avcodec_default_get_buffer() instead of providing buffers allocated by
     * some other means.
     *
     * AVFrame.data[] should be 32- or 16-byte-aligned unless the CPU doesn't
     * need it. avcodec_default_get_buffer() aligns the output buffer properly,
     * but if get_buffer() is overridden then alignment considerations should
     * be taken into account.
     *
     * @see avcodec_default_get_buffer()
     *
     * Video:
     *
     * If pic.reference is set then the frame will be read later by libavcodec.
     * avcodec_align_dimensions2() should be used to find the required width and
     * height, as they normally need to be rounded up to the next multiple of 16.
     *
     * If frame multithreading is used and thread_safe_callbacks is set,
     * it may be called from a different thread, but not from more than one at
     * once. Does not need to be reentrant.
     *
     * @see release_buffer(), reget_buffer()
     * @see avcodec_align_dimensions2()
     *
     * Audio:
     *
     * Decoders request a buffer of a particular size by setting
     * AVFrame.nb_samples prior to calling get_buffer(). The decoder may,
     * however, utilize only part of the buffer by setting AVFrame.nb_samples
     * to a smaller value in the output frame.
     *
     * Decoders cannot use the buffer after returning from
     * avcodec_decode_audio4(), so they will not call release_buffer(), as it
     * is assumed to be released immediately upon return. In some rare cases,
     * a decoder may need to call get_buffer() more than once in a single
     * call to avcodec_decode_audio4(). In that case, when get_buffer() is
     * called again after it has already been called once, the previously
     * acquired buffer is assumed to be released at that time and may not be
     * reused by the decoder.
     *
     * As a convenience, av_samples_get_buffer_size() and
     * av_samples_fill_arrays() in libavutil may be used by custom get_buffer()
     * functions to find the required data size and to fill data pointers and
     * linesize. In AVFrame.linesize, only linesize[0] may be set for audio
     * since all planes must be the same size.
     *
     * @see av_samples_get_buffer_size(), av_samples_fill_arrays()
     *
     * - encoding: unused
     * - decoding: Set by libavcodec, user can override.
     *
     * @deprecated use get_buffer2()
     *)
    get_buffer: function (c: PAVCodecContext; pic: PAVFrame): cint; cdecl; {deprecated;}

    (**
     * Called to release buffers which were allocated with get_buffer.
     * A released buffer can be reused in get_buffer().
     * pic.data[*] must be set to NULL.
     * - encoding: unused
     * - decoding: Set by libavcodec, user can override.
     *
     * @deprecated custom freeing callbacks should be set from get_buffer2()
     *)
    release_buffer: procedure (c: PAVCodecContext; pic: PAVFrame); cdecl; {deprecated;}

    (**
     * Called at the beginning of a frame to get cr buffer for it.
     * Buffer type (size, hints) must be the same. libavcodec won't check it.
     * libavcodec will pass previous buffer in pic, function should return
     * same buffer or new buffer with old frame "painted" into it.
     * If pic.data[0] == NULL must behave like get_buffer().
     * if CODEC_CAP_DR1 is not set then reget_buffer() must call
     * avcodec_default_reget_buffer() instead of providing buffers allocated by
     * some other means.
     * - encoding: unused
     * - decoding: Set by libavcodec, user can override
     *)
    reget_buffer: function (c: PAVCodecContext; pic: PAVFrame): cint; cdecl; {deprecated;}
{$ENDIF}

    (**
     * This callback is called at the beginning of each frame to get data
     * buffer(s) for it. There may be one contiguous buffer for all the data or
     * there may be a buffer per each data plane or anything in between. What
     * this means is, you may set however many entries in buf[] you feel necessary.
     * Each buffer must be reference-counted using the AVBuffer API (see description
     * of buf[] below).
     *
     * The following fields will be set in the frame before this callback is
     * called:
     * - format
     * - width, height (video only)
     * - sample_rate, channel_layout, nb_samples (audio only)
     * Their values may differ from the corresponding values in
     * AVCodecContext. This callback must use the frame values, not the codec
     * context values, to calculate the required buffer size.
     *
     * This callback must fill the following fields in the frame:
     * - data[]
     * - linesize[]
     * - extended_data:
     *   * if the data is planar audio with more than 8 channels, then this
     *     callback must allocate and fill extended_data to contain all pointers
     *     to all data planes. data[] must hold as many pointers as it can.
     *     extended_data must be allocated with av_malloc() and will be freed in
     *     av_frame_unref().
     *   * otherwise exended_data must point to data
     * - buf[] must contain one or more pointers to AVBufferRef structures. Each of
     *   the frame's data and extended_data pointers must be contained in these. That
     *   is, one AVBufferRef for each allocated chunk of memory, not necessarily one
     *   AVBufferRef per data[] entry. See: av_buffer_create(), av_buffer_alloc(),
     *   and av_buffer_ref().
     * - extended_buf and nb_extended_buf must be allocated with av_malloc() by
     *   this callback and filled with the extra buffers if there are more
     *   buffers than buf[] can hold. extended_buf will be freed in
     *   av_frame_unref().
     *
     * If CODEC_CAP_DR1 is not set then get_buffer2() must call
     * avcodec_default_get_buffer2() instead of providing buffers allocated by
     * some other means.
     *
     * Each data plane must be aligned to the maximum required by the target
     * CPU.
     *
     * @see avcodec_default_get_buffer2()
     *
     * Video:
     *
     * If AV_GET_BUFFER_FLAG_REF is set in flags then the frame may be reused
     * (read and/or written to if it is writable) later by libavcodec.
     *
     * avcodec_align_dimensions2() should be used to find the required width and
     * height, as they normally need to be rounded up to the next multiple of 16.
     *
     * Some decoders do not support linesizes changing between frames.
     *
     * If frame multithreading is used and thread_safe_callbacks is set,
     * this callback may be called from a different thread, but not from more
     * than one at once. Does not need to be reentrant.
     *
     * @see avcodec_align_dimensions2()
     *
     * Audio:
     *
     * Decoders request a buffer of a particular size by setting
     * AVFrame.nb_samples prior to calling get_buffer2(). The decoder may,
     * however, utilize only part of the buffer by setting AVFrame.nb_samples
     * to a smaller value in the output frame.
     *
     * As a convenience, av_samples_get_buffer_size() and
     * av_samples_fill_arrays() in libavutil may be used by custom get_buffer2()
     * functions to find the required data size and to fill data pointers and
     * linesize. In AVFrame.linesize, only linesize[0] may be set for audio
     * since all planes must be the same size.
     *
     * @see av_samples_get_buffer_size(), av_samples_fill_arrays()
     *
     * - encoding: unused
     * - decoding: Set by libavcodec, user can override.
     *)
    get_buffer2: function (s: PAVCodecContext; frame: PAVFrame; flags: cint): cint; cdecl;

    (**
     * If non-zero, the decoded audio and video frames returned from
     * avcodec_decode_video2() and avcodec_decode_audio4() are reference-counted
     * and are valid indefinitely. The caller must free them with
     * av_frame_unref() when they are not needed anymore.
     * Otherwise, the decoded frames must not be freed by the caller and are
     * only valid until the next decode call.
     *
     * - encoding: unused
     * - decoding: set by the caller before avcodec_open2().
     *)
    refcounted_frames: cint;

    (* - encoding parameters *)
    qcompress: cfloat;  ///< amount of qscale change between easy & hard scenes (0.0-1.0)
    qblur: cfloat;      ///< amount of qscale smoothing over time (0.0-1.0)

    (**
     * minimum quantizer
     * - encoding: Set by user.
     * - decoding: unused
     *)
    qmin: cint;

   (**
     * maximum quantizer
     * - encoding: Set by user.
     * - decoding: unused
     *)
   qmax: cint;

    (**
     * maximum quantizer difference between frames
     * - encoding: Set by user.
     * - decoding: unused
     *)
    max_qdiff: cint;
{$IFDEF FF_API_MPV_OPT}
    (**
     * @deprecated use encoder private options instead
     *)
    {attribute_deprecated}
    rc_qsquish: cfloat;

    {attribute_deprecated}
    rc_qmod_amp: cfloat;
    {attribute_deprecated}
    rc_qmod_freq: cint;
{$ENDIF}
    (**
     * decoder bitstream buffer size
     * - encoding: Set by user.
     * - decoding: unused
     *)
    rc_buffer_size: cint;
    (**
     * ratecontrol override, see RcOverride
     * - encoding: Allocated/set/freed by user.
     * - decoding: unused
     *)
    rc_override_count: cint;
    rc_override: PRcOverride;
{$IFDEF FF_API_MPV_OPT}
    (**
     * @deprecated use encoder private options instead
     *)
    {attribute_deprecated}
    rc_eq: {const} PAnsiChar;
{$ENDIF}
    (**
     * maximum bitrate
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    rc_max_rate: cint;

    (**
     * minimum bitrate
     * - encoding: Set by user.
     * - decoding: unused
     *)
    rc_min_rate: cint;

{$IFDEF FF_API_MPV_OPT}

    (**
     * @deprecated use encoder private options instead
     *)
    {attribute_deprecated}
    rc_buffer_aggressivity: cfloat;

    {attribute_deprecated}
    rc_initial_cplx: cfloat;
{$ENDIF}
    (**
     * Ratecontrol attempt to use, at maximum, <value> of what can be used without an underflow.
     * - encoding: Set by user.
     * - decoding: unused.
     *)
    rc_max_available_vbv_use: cfloat;

    (**
     * Ratecontrol attempt to use, at least, <value> times the amount needed to prevent a vbv overflow.
     * - encoding: Set by user.
     * - decoding: unused.
     *)
    rc_min_vbv_overflow_use: cfloat;

    (**
     * Number of bits which should be loaded into the rc buffer before decoding starts.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    rc_initial_buffer_occupancy: cint;

    (**
     * coder type
     * - encoding: Set by user.
     * - decoding: unused
     *)
    coder_type: cint;

    (**
     * context model
     * - encoding: Set by user.
     * - decoding: unused
     *)
    context_model: cint;

{$IFDEF FF_API_MPV_OPT}
    (**
     * @deprecated use encoder private options instead
     *)
    {attribute_deprecated}
    lmin: cint;

    (**
     * @deprecated use encoder private options instead
     *)
    {attribute_deprecated}
    lmax: cint;
{$ENDIF}
    (**
     * frame skip threshold
     * - encoding: Set by user.
     * - decoding: unused
     *)
    frame_skip_threshold: cint;

    (**
     * frame skip factor
     * - encoding: Set by user.
     * - decoding: unused
     *)
    frame_skip_factor: cint;

    (**
     * frame skip exponent
     * - encoding: Set by user.
     * - decoding: unused
     *)
    frame_skip_exp: cint;

    (**
     * frame skip comparison function
     * - encoding: Set by user.
     * - decoding: unused
     *)
    frame_skip_cmp: cint;

    (**
     * trellis RD quantization
     * - encoding: Set by user.
     * - decoding: unused
     *)
    trellis: cint;

    (**
     * - encoding: Set by user.
     * - decoding: unused
     *)
    min_prediction_order: cint;

    (**
     * - encoding: Set by user.
     * - decoding: unused
     *)
    max_prediction_order: cint;

    (**
     * GOP timecode frame start number, in non drop frame format
     * - encoding: Set by user, in non drop frame format
     * - decoding: Set by libavcodec (timecode in the 25 bits format, -1 if unset)
     *)
    timecode_frame_start: cint64;

    (* The RTP callback: This function is called   *)
    (* every time the encoder has a packet to send *)
    (* Depends on the encoder if the data starts   *)
    (* with a Start Code (it should) H.263 does.   *)
    (* mb_nb contains the number of macroblocks    *)
    (* encoded in the RTP payload                  *)
    rtp_callback: procedure (avctx: PAVCodecContext; data: pointer;
                             size: cint; mb_nb: cint); cdecl;

    rtp_payload_size: cint;     (* The size of the RTP payload: the coder will  *)
                                (* do it's best to deliver a chunk with size    *)
                                (* below rtp_payload_size, the chunk will start *)
                                (* with a start code on some codecs like H.263  *)
                                (* This doesn't take account of any particular  *)
                                (* headers inside the transmited RTP payload    *)

    (* statistics, used for 2-pass encoding *)
    mv_bits: cint;
    header_bits: cint;
    i_tex_bits: cint;
    p_tex_bits: cint;
    i_count: cint;
    p_count: cint;
    skip_count: cint;
    misc_bits: cint;

    (**
     * number of bits used for the previously encoded frame
     * - encoding: Set by libavcodec.
     * - decoding: unused
     *)
    frame_bits: cint;

    (**
     * pass1 encoding statistics output buffer
     * - encoding: Set by libavcodec.
     * - decoding: unused
     *)
    stats_out: PAnsiChar;

    (**
     * pass2 encoding statistics input buffer
     * Concatenated stuff from stats_out of pass1 should be placed here.
     * - encoding: Allocated/set/freed by user.
     * - decoding: unused
     *)
    stats_in: PAnsiChar;

    (**
     * Work around bugs in encoders which sometimes cannot be detected automatically.
     * - encoding: Set by user
     * - decoding: Set by user
     *)
    workaround_bugs: cint;

    (**
     * strictly follow the standard (MPEG4, ...).
     * - encoding: Set by user.
     * - decoding: Set by user.
     * Setting this to STRICT or higher means the encoder and decoder will
     * generally do stupid things, whereas setting it to unofficial or lower
     * will mean the encoder might produce output that is not supported by all
     * spec-compliant decoders. Decoders don't differentiate between normal,
     * unofficial and experimental (that is, they always try to decode things
     * when they can) unless they are explicitly asked to behave stupidly
     * (=strictly conform to the specs)
     *)
    strict_std_compliance: cint;

    (**
     * error concealment flags
     * - encoding: unused
     * - decoding: Set by user.
     *)
    error_concealment: cint;

    (**
     * debug
     * Code outside libavcodec should access this field using AVOptions    
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    debug: cint;

{$IFDEF FF_API_DEBUG_MV}
    (**
     * debug
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    debug_mv: cint;
{$ENDIF}

    (**
     * Error recognition; may misdetect some more or less valid parts as errors.
     * - encoding: unused
     * - decoding: Set by user.
     *)
    err_recognition: cint;

    (**
     * opaque 64bit number (generally a PTS) that will be reordered and
     * output in AVFrame.reordered_opaque
     * - encoding: unused
     * - decoding: Set by user.
     *)
    reordered_opaque: cint64;

    (**
     * Hardware accelerator in use
     * - encoding: unused.
     * - decoding: Set by libavcodec
     *)
    hwaccel: PAVHWAccel;

    (**
     * Hardware accelerator context.
     * For some hardware accelerators, a global context needs to be
     * provided by the user. In that case, this holds display-dependent
     * data FFmpeg cannot instantiate itself. Please refer to the
     * FFmpeg HW accelerator documentation to know how to fill this
     * is. e.g. for VA API, this is a struct vaapi_context.
     * - encoding: unused
     * - decoding: Set by user
     *)
    hwaccel_context: pointer;

    (**
     * error
     * - encoding: Set by libavcodec if flags&CODEC_FLAG_PSNR.
     * - decoding: unused
     *)
    error: array [0..AV_NUM_DATA_POINTERS - 1] of cuint64;

    (**
     * DCT algorithm, see FF_DCT_* below
     * - encoding: Set by user.
     * - decoding: unused
     *)
    dct_algo: cint;

    (**
     * IDCT algorithm, see FF_IDCT_* below.
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    idct_algo: cint;

    (**
     * bits per sample/pixel from the demuxer (needed for huffyuv).
     * - encoding: Set by libavcodec.
     * - decoding: Set by user.
     *)
    bits_per_coded_sample: cint;

    (**
     * Bits per sample/pixel of internal libavcodec pixel/sample format.
     * - encoding: set by user.
     * - decoding: set by libavcodec.
     *)
    bits_per_raw_sample: cint;

{$IFDEF FF_API_LOWRES}
    (**
     * low resolution decoding, 1-> 1/2 size, 2->1/4 size
     * - encoding: unused
     * - decoding: Set by user.
     * Code outside libavcodec should access this field using:
     * av_codec_{get,set}_lowres(avctx)
     *)
    lowres: cint;
{$ENDIF}

    (**
     * the picture in the bitstream
     * - encoding: Set by libavcodec.
     * - decoding: unused
     *)
    coded_frame: PAVFrame;

    (**
     * thread count
     * is used to decide how many independent tasks should be passed to execute()
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    thread_count: cint;

    (**
     * Which multithreading methods to use.
     * Use of FF_THREAD_FRAME will increase decoding delay by one frame per thread,
     * so clients which cannot provide future frames should not use it.
     *
     * - encoding: Set by user, otherwise the default is used.
     * - decoding: Set by user, otherwise the default is used.
     *)
    thread_type: cint;

    (**
     * Which multithreading methods are in use by the codec.
     * - encoding: Set by libavcodec.
     * - decoding: Set by libavcodec.
     *)
    active_thread_type: cint;

    (**
     * Set by the client if its custom get_buffer() callback can be called
     * from another thread, which allows faster multithreaded decoding.
     * draw_horiz_band() will be called from other threads regardless of this setting.
     * Ignored if the default get_buffer() is used.
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    thread_safe_callbacks: cint;

    (**
     * The codec may call this to execute several independent things.
     * It will return only after finishing all tasks.
     * The user may replace this with some multithreaded implementation,
     * the default implementation will execute the parts serially.
     * @param count the number of things to execute
     * - encoding: Set by libavcodec, user can override.
     * - decoding: Set by libavcodec, user can override.
     *)
    execute: function (c: PAVCodecContext; func: TExecuteFunc; arg: Pointer; ret: PCint; count: cint; size: cint): cint; cdecl;

    (**
     * The codec may call this to execute several independent things.
     * It will return only after finishing all tasks.
     * The user may replace this with some multithreaded implementation,
     * the default implementation will execute the parts serially.
     * Also see avcodec_thread_init and e.g. the --enable-pthread configure option.
     * @param c context passed also to func
     * @param count the number of things to execute
     * @param arg2 argument passed unchanged to func
     * @param ret return values of executed functions, must have space for "count" values. May be NULL.
     * @param func function that will be called count times, with jobnr from 0 to count-1.
     *             threadnr will be in the range 0 to c->thread_count-1 < MAX_THREADS and so that no
     *             two instances of func executing at the same time will have the same threadnr.
     * @return always 0 currently, but code should handle a future improvement where when any call to func
     *         returns < 0 no further calls to func may be done and < 0 is returned.
     * - encoding: Set by libavcodec, user can override.
     * - decoding: Set by libavcodec, user can override.
     *)
      execute2: function (c: PAVCodecContext; func: TExecute2Func; arg2: Pointer; ret: Pcint; count: cint): cint; cdecl;

{$IFDEF FF_API_THREAD_OPAQUE}      
    (**
     * @deprecated this field should not be used from outside of lavc
     *)
    thread_opaque: pointer;
{$ENDIF}
    
    (**
     * noise vs. sse weight for the nsse comparison function
     * - encoding: Set by user.
     * - decoding: unused
     *)
    nsse_weight: cint;

    (**
     * profile
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    profile: cint;

    (**
     * level
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    level: cint;

    (**
     * Skip loop filtering for selected frames.
     * - encoding: unused
     * - decoding: Set by user.
     *)
    skip_loop_filter: TAVDiscard;

    (**
     * Skip IDCT/dequantization for selected frames.
     * - encoding: unused
     * - decoding: Set by user.
     *)
    skip_idct: TAVDiscard;

    (**
     * Skip decoding for selected frames.
     * - encoding: unused
     * - decoding: Set by user.
     *)
    skip_frame: TAVDiscard;

    (**
     * Header containing style information for text subtitles.
     * For SUBTITLE_ASS subtitle type, it should contain the whole ASS
     * [Script Info] and [V4+ Styles] section, plus the [Events] line and
     * the Format line following. It shouldn't include any Dialogue line.
     * - encoding: Set/allocated/freed by user (before avcodec_open2())
     * - decoding: Set/allocated/freed by libavcodec (by avcodec_open2())
     *)
    subtitle_header: Pcuint8;
    subtitle_header_size: cint;

{$IFDEF FF_API_ERROR_RATE}    
    (**
     * @deprecated use the 'error_rate' private AVOption of the mpegvideo
     * encoders
     *)
    error_rate: cint;
{$ENDIF}    

{$IFDEF FF_API_CODEC_PKT}    
    (**
     * @deprecated this field is not supposed to be accessed from outside lavc
     *)
    pkt: PAVPacket;
{$ENDIF}    

    (**
     * VBV delay coded in the last frame (in periods of a 27 MHz clock).
     * Used for compliant TS muxing.
     * - encoding: Set by libavcodec.
     * - decoding: unused.
     *)
    vbv_delay: cuint64;

    (**
     * Encoding only. Allow encoders to output packets that do not contain any
     * encoded data, only side data.
     *
     * Some encoders need to output such packets, e.g. to update some stream
     * parameters at the end of encoding.
     *
     * All callers are strongly recommended to set this option to 1 and update
     * their code to deal with such packets, since this behaviour may become
     * always enabled in the future (then this option will be deprecated and
     * later removed). To avoid ABI issues when this happens, the callers should
     * use AVOptions to set this field.
     *)
    side_data_only_packets: cint;

    (**
     * Audio only. The number of "priming" samples (padding) inserted by the
     * encoder at the beginning of the audio. I.e. this number of leading
     * decoded samples must be discarded by the caller to get the original audio
     * without leading padding.
     *
     * - decoding: unused
     * - encoding: Set by libavcodec. The timestamps on the output packets are
     *             adjusted by the encoder so that they always refer to the
     *             first sample of the data actually contained in the packet,
     *             including any added padding.  E.g. if the timebase is
     *             1/samplerate and the timestamp of the first input sample is
     *             0, the timestamp of the first output packet will be
     *             -initial_padding.
     *)
    initial_padding: cint;

    (**
     * - decoding: For codecs that store a framerate value in the compressed
     *             bitstream, the decoder may export it here. { 0, 1} when
     *             unknown.
     * - encoding: unused
     *)
    framerate: TAVRational;

    (**
     * Timebase in which pkt_dts/pts and AVPacket.dts/pts are.
     * Code outside libavcodec should access this field using:
     * av_codec_{get,set}_pkt_timebase(avctx)
     * - encoding unused.
     * - decoding set by user
     *)
    pkt_timebase: TAVRational;

    (**
     * AVCodecDescriptor
     * Code outside libavcodec should access this field using:
     * av_codec_{get,set}_codec_descriptor(avctx)
     * - encoding: unused.
     * - decoding: set by libavcodec.
     *)
    codec_descriptor: PAVCodecDescriptor;

{$IFNDEF FF_API_LOWRES}
    (**
     * low resolution decoding, 1-> 1/2 size, 2->1/4 size
     * - encoding: unused
     * - decoding: Set by user.
     * Code outside libavcodec should access this field using:
     * av_codec_{get,set}_lowres(avctx)
     *)
    lowres: cint;
{$ENDIF}

    (**
     * Current statistics for PTS correction.
     * - decoding: maintained and used by libavcodec, not intended to be used by user apps
     * - encoding: unused
     *)
    pts_correction_num_faulty_pts: cint64; /// Number of incorrect PTS values so far
    pts_correction_num_faulty_dts: cint64; /// Number of incorrect DTS values so far
    pts_correction_last_pts: cint64;       /// PTS of the last frame
    pts_correction_last_dts: cint64;       /// DTS of the last frame

    (**
     * Character encoding of the input subtitles file.
     * - decoding: set by user
     * - encoding: unused
     *)
    sub_charenc: PAnsiChar;

    (**
     * Subtitles character encoding mode. Formats or codecs might be adjusting
     * this setting (if they are doing the conversion themselves for instance).
     * - decoding: set by libavcodec
     * - encoding: unused
     *)
    sub_charenc_mode: cint;

    (**
     * Skip processing alpha if supported by codec.
     * Note that if the format uses pre-multiplied alpha (common with VP6,
     * and recommended due to better video quality/compression)
     * the image will look as if alpha-blended onto a black background.
     * However for formats that do not use pre-multiplied alpha
     * there might be serious artefacts (though e.g. libswscale currently
     * assumes pre-multiplied alpha anyway).
     * Code outside libavcodec should access this field using AVOptions
     *
     * - decoding: set by user
     * - encoding: unused
     *)
    skip_alpha: cint;

    (**
     * Number of samples to skip after a discontinuity
     * - decoding: unused
     * - encoding: set by libavcodec
     *)
    seek_preroll: cint;

{$IFNDEF FF_API_DEBUG_MV}
    (**
     * debug motion vectors
     * Code outside libavcodec should access this field using AVOptions
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    debug_mv: cint;
{$ENDIF}

    (**
     * custom intra quantization matrix
     * Code outside libavcodec should access this field using av_codec_g/set_chroma_intra_matrix()
     * - encoding: Set by user, can be NULL.
     * - decoding: unused.
     *)
    chroma_intra_matrix: PWord;

    (**
     * dump format separator.
     * can be ", " or "\n      " or anything else
     * Code outside libavcodec should access this field using AVOptions
     * (NO direct access).
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    dump_separator: Pcuint8;

    (**
     * ',' separated list of allowed decoders.
     * If NULL then all are allowed
     * - encoding: unused
     * - decoding: set by user through AVOPtions (NO direct access)
     *)
    codec_whitelist: PAnsiChar;
  end; {TAVCodecContext}

  (**
   * AVProfile.
   *)
  PAVProfile = ^TAVProfile;
  TAVProfile = record
    profile: cint;
    name: {const} PAnsiChar; ///< short name for the profile
  end; {TAVProfile}

  TAVSubtitleType = (
    SUBTITLE_NONE,

    SUBTITLE_BITMAP,                ///< A bitmap, pict will be set

    (**
     * Plain text, the text field must be set by the decoder and is
     * authoritative. ass and pict fields may contain approximations.
     *)
    SUBTITLE_TEXT,

    (**
     * Formatted text, the ass field must be set by the decoder and is
     * authoritative. pict and text fields may contain approximations.
     *)
    SUBTITLE_ASS
  ); {TAVSubtitleType}

  (**
   * @defgroup lavc_picture AVPicture
   *
   * Functions for working with AVPicture
   * @{
   *)

  (**
   * four components are given, that's all.
   * the last component is alpha
   *)
    PAVPicture = ^TAVPicture;
    TAVPicture = record
      data: array [0..AV_NUM_DATA_POINTERS - 1] of PByteArray;
      linesize: array [0..AV_NUM_DATA_POINTERS - 1] of cint;       ///< number of bytes per line
    end; {TAVPicture}

  PPAVSubtitleRect = ^PAVSubtitleRect;
  PAVSubtitleRect = ^TAVSubtitleRect;
  TAVSubtitleRect = record
    x: cint;        ///< top left corner  of pict, undefined when pict is not set
    y: cint;        ///< top left corner  of pict, undefined when pict is not set
    w: cint;        ///< width            of pict, undefined when pict is not set
    h: cint;        ///< height           of pict, undefined when pict is not set
    nb_colors: cint; ///< number of colors in pict, undefined when pict is not set

    (**
     * data+linesize for the bitmap of this subtitle.
     * can be set for text/ass as well once they where rendered
     *)
    pict: TAVPicture;
    type_: TAVSubtitleType;

    text: PAnsiChar;                     ///< 0 terminated plain UTF-8 text

    (**
     * 0 terminated ASS/SSA compatible event line.
     * The presentation of this is unaffected by the other values in this
     * struct.
     *)
    ass: PAnsiChar;

    flags: cint;
  end; {TAVSubtitleRect}

  PPAVSubtitle = ^PAVSubtitle;
  PAVSubtitle = ^TAVSubtitle;
  TAVSubtitle = record
    format: cuint16; (* 0 = graphics *)
    start_display_time: cuint32; (* relative to packet pts, in ms *)
    end_display_time: cuint32; (* relative to packet pts, in ms *)
    num_rects: cuint;
    rects: PPAVSubtitleRect;
    pts: cint64;     ///< Same as packet pts, in AV_TIME_BASE
  end; {TAVSubtitle}

(**
 * AVCodec.
 *)
  TAVCodec = record
    (**
     * Name of the codec implementation.
     * The name is globally unique among encoders and among decoders (but an
     * encoder and a decoder can share the same name).
     * This is the primary way to find a codec from the user perspective.
     *)
    name: PAnsiChar;
    (**
     * Descriptive name for the codec, meant to be more human readable than name.
     * You should use the NULL_IF_CONFIG_SMALL() macro to define it.
     *)
    long_name: {const} PAnsiChar;
    type_: TAVMediaType;
    id: TAVCodecID;
    (**
     * Codec capabilities.
     * see CODEC_CAP_*
     *)
    capabilities: cint;
    supported_framerates: {const} PAVRational; ///< array of supported framerates, or NULL if any, array is terminated by {0,0}
    pix_fmts: {const} PAVPixelFormat;          ///< array of supported pixel formats, or NULL if unknown, array is terminated by -1
    supported_samplerates: {const} PCint;      ///< array of supported audio samplerates, or NULL if unknown, array is terminated by 0
    sample_fmts: {const} PAVSampleFormatArray; ///< array of supported sample formats, or NULL if unknown, array is terminated by -1
    channel_layouts: {const} PCuint64;         ///< array of support channel layouts, or NULL if unknown. array is terminated by 0
    max_lowres: byte;                          ///< array of support channel layouts, or NULL if unknown. array is terminated by 0
    priv_class: {const} PAVClass;              ///< AVClass for the private context
    profiles: {const} PAVProfile;              ///< array of recognized profiles, or NULL if unknown, array is terminated by {FF_PROFILE_UNKNOWN}

    (*****************************************************************
     * No fields below this line are part of the public API. They
     * may not be used outside of libavcodec and can be changed and
     * removed at will.
     * New public fields should be added right above.
     *****************************************************************
     *)
    priv_data_size: cint;
    next: PAVCodec;
    (**
     * @name Frame-level threading support functions
     * @{
     *)
    (**
     * If defined, called on thread contexts when they are created.
     * If the codec allocates writable tables in init(), re-allocate them here.
     * priv_data will be set to a copy of the original.
     *)
    init_thread_copy: function (avctx: PAVCodecContext): Pcint; cdecl;
    (**
     * Copy necessary context variables from a previous thread context to the current one.
     * If not defined, the next thread will start automatically; otherwise, the codec
     * must call ff_thread_finish_setup().
     *
     * dst and src will (rarely) point to the same context, in which case memcpy should be skipped.
     *)
    update_thread_context: function (dst: PAVCodecContext; src: {const} PAVCodecContext): cint; cdecl;
    (** @} *)

    (**
     * Private codec-specific defaults.
     *)
    defaults: {const} pointer;

    (**
     * Initialize codec static data, called from avcodec_register().
     *)
    init_static_data: procedure (codec: PAVCodec); cdecl;

    init: function (avctx: PAVCodecContext): cint; cdecl;
    encode_sub: function (avctx: PAVCodecContext; buf: PByteArray; buf_size: cint;
                          sub: {const} PAVSubtitle): cint; cdecl;
    (**
     * Encode data to an AVPacket.
     *
     * @param      avctx          codec context
     * @param      avpkt          output AVPacket (may contain a user-provided buffer)
     * @param[in]  frame          AVFrame containing the raw data to be encoded
     * @param[out] got_packet_ptr encoder sets to 0 or 1 to indicate that a
     *                            non-empty packet was returned in avpkt.
     * @return 0 on success, negative error code on failure
     *)
    encode2: function (avctx: PAVCodecContext; avpkt: PAVPacket; frame: {const} PAVFrame;
                   got_packet_ptr: Pcint): cint; cdecl;
    decode: function (avctx: PAVCodecContext; outdata: pointer; var outdata_size: cint; avpkt: PAVPacket): cint; cdecl;
    close: function (avctx: PAVCodecContext): cint; cdecl;
    (**
     * Flush buffers.
     * Will be called when seeking
     *)
    flush: procedure (avctx: PAVCodecContext); cdecl;
  end; {TAVCodec}

  PMpegEncContext = ^TMpegEncContext;
  // To be expanded if needed.
  TMpegEncContext = record
  end;
  
(**
 * @defgroup lavc_hwaccel AVHWAccel
 * @{
 *)
  TAVHWAccel = record
    (**
     * Name of the hardware accelerated codec.
     * The name is globally unique among encoders and among decoders (but an
     * encoder and a decoder can share the same name).
     *)
    name: PAnsiChar;

    (**
     * Type of codec implemented by the hardware accelerator.
     *
     * See AVMediaType_xxx
     *)
    type_: TAVMediaType;

    (**
     * Codec implemented by the hardware accelerator.
     *
     * See AV_CODEC_ID_xxx
     *)
    id: TAVCodecID;

    (**
     * Supported pixel format.
     *
     * Only hardware accelerated formats are supported here.
     *)
    pix_fmt: PAVPixelFormat;

    (**
     * Hardware accelerated codec capabilities.
     * see FF_HWACCEL_CODEC_CAP_*
     *)
    capabilities: cint;

    (*****************************************************************
     * No fields below this line are part of the public API. They
     * may not be used outside of libavcodec and can be changed and
     * removed at will.
     * New public fields should be added right above.
     *****************************************************************
     *)
    next: PAVHWAccel;

    (**
     * Allocate a custom buffer
     *)
    alloc_frame: function (avctx: PAVCodecContext; frame: PAVFrame): cint; cdecl;

    (**
     * Called at the beginning of each frame or field picture.
     *
     * Meaningful frame information (codec specific) is guaranteed to
     * be parsed at this point. This function is mandatory.
     * The only exception is XvMC, that works on MB level.
     *
     * Note that buf can be NULL along with buf_size set to 0.
     * Otherwise, this means the whole frame is available at this point.
     *
     * @param avctx the codec context
     * @param buf the frame data buffer base
     * @param buf_size the size of the frame in bytes
     * @return zero if successful, a negative value otherwise
     *)
    start_frame: function (avctx:       PAVCodecContext;
                           buf: {const} PByteArray;
                           buf_size:    cuint): cint; cdecl;

    (**
     * Callback for each slice.
     *
     * Meaningful slice information (codec specific) is guaranteed to
     * be parsed at this point. This function is mandatory.
     *
     * @param avctx the codec context
     * @param buf the slice data buffer base
     * @param buf_size the size of the slice in bytes
     * @return zero if successful, a negative value otherwise
     *)
    decode_slice: function (avctx:       PAVCodecContext;
                            buf: {const} PByteArray;
                            buf_size:    cuint): cint; cdecl;

    (**
     * Called at the end of each frame or field picture.
     *
     * The whole picture is parsed at this point and can now be sent
     * to the hardware accelerator. This function is mandatory.
     *
     * @param avctx the codec context
     * @return zero if successful, a negative value otherwise
     *)
    end_frame: function (avctx: PAVCodecContext): cint; cdecl;

    (**
     * Size of per-frame hardware accelerator private data.
     *
     * Private data is allocated with av_mallocz() before
     * AVCodecContext.get_buffer() and deallocated after
     * AVCodecContext.release_buffer().
     *)
    frame_priv_data_size: cint;

    (**
     * Called for every Macroblock in a slice.
     *
     * XvMC uses it to replace the ff_mpv_decode_mb().
     * Instead of decoding to raw picture, MB parameters are
     * stored in an array provided by the video driver.
     *
     * @param s the mpeg context
     *)
    decode_mb: procedure(s: PMpegEncContext); cdecl;

    (**
     * Initialize the hwaccel private data.
     *
     * This will be called from ff_get_format(), after hwaccel and
     * hwaccel_context are set and the hwaccel private data in AVCodecInternal
     * is allocated.
     *)
    init: function (avctx: PAVCodecContext): cint; cdecl;

    (**
     * Uninitialize the hwaccel private data.
     *
     * This will be called from get_format() or avcodec_close(), after hwaccel
     * and hwaccel_context are already uninitialized.
     *)
    uninit: function (avctx: PAVCodecContext): cint; cdecl;

    (**
     * Size of the private data to allocate in
     * AVCodecInternal.hwaccel_priv_data.
     *)
    priv_data_size: cint;
  end; {TAVHWAccel}

const
  (**
   * Hardware acceleration should be used for decoding even if the codec level
   * used is unknown or higher than the maximum supported level reported by the
   * hardware driver.
   *)
  AV_HWACCEL_FLAG_IGNORE_LEVEL 	  = (1 << 0);

  FF_SUB_CHARENC_MODE_DO_NOTHING  = -1;  ///< do nothing (demuxer outputs a stream supposed to be already in UTF-8, or the codec is bitmap for instance)
  FF_SUB_CHARENC_MODE_AUTOMATIC   = 0;   ///< libavcodec will select the mode itself
  FF_SUB_CHARENC_MODE_PRE_DECODER = 1;   ///< the AVPacket data needs to be recoded to UTF-8 before being fed to the decoder, requires iconv

  function  av_codec_get_pkt_timebase(avctx: {const} PAVCodecContext): TAVRational;
    cdecl; external av__codec;
  procedure av_codec_set_pkt_timebase(avctx: {const} PAVCodecContext; val: TAVRational);
    cdecl; external av__codec;

  function  av_codec_get_codec_descriptor(avctx: {const} PAVCodecContext): PAVCodecDescriptor;
    cdecl; external av__codec;
  procedure av_codec_set_codec_descriptor(avctx: {const} PAVCodecContext; desc: {const} PAVCodecDescriptor);
    cdecl; external av__codec;

  function  av_codec_get_lowres(avctx: {const} PAVCodecContext): cint;
    cdecl; external av__codec;
  procedure av_codec_set_lowres(avctx: PAVCodecContext; val: cint);
    cdecl; external av__codec;  

  function av_codec_get_seek_preroll(avctx: {const} PAVCodecContext): cint;
    cdecl; external av__codec;
  procedure av_codec_set_seek_preroll(avctx: PAVCodecContext; val: cint);
    cdecl; external av__codec;

  function av_codec_get_max_lowres(codec: {const} PAVCodec): cint;
    cdecl; external av__codec;
    
  function av_codec_get_chroma_intra_matrix(avctx: {const} PAVCodecContext): PWord;
    cdecl; external av__codec;    
  procedure av_codec_set_chroma_intra_matrix(avctx: PAVCodecContext; val: PWord);
    cdecl; external av__codec;

(**
 * @
 *)

const
  AV_SUBTITLE_FLAG_FORCED = $00000001;

(**
 * If c is NULL, returns the first registered codec,
 * if c is non-NULL, returns the next registered codec after c,
 * or NULL if c is the last one.
 *)
function av_codec_next(c: {const} PAVCodec): PAVCodec;
  cdecl; external av__codec;

(**
 * Return the LIBAVCODEC_VERSION_INT constant.
 *)
function avcodec_version(): cuint;
  cdecl; external av__codec;

(**
 * Return the libavcodec build-time configuration.
 *)
function avcodec_configuration(): PAnsiChar;
  cdecl; external av__codec;

(**
 * Return the libavcodec license.
 *)
function avcodec_license(): PAnsiChar;
  cdecl; external av__codec;

(**
 * Register the codec codec and initialize libavcodec.
 *
 * @warning either this function or avcodec_register_all() must be called
 * before any other libavcodec functions.
 *
 * @see avcodec_register_all()
 *)
procedure avcodec_register(codec: PAVCodec);
  cdecl; external av__codec;

(**
 * Register all the codecs, parsers and bitstream filters which were enabled at
 * configuration time. If you do not call this function you can select exactly
 * which formats you want to support, by using the individual registration
 * functions.
 *
 * @see register_avcodec
 * @see avcodec_register
 * @see av_register_codec_parser
 * @see av_register_bitstream_filter
 *)
procedure avcodec_register_all();
  cdecl; external av__codec;

(**
 * Allocate an AVCodecContext and set its fields to default values. The
 * resulting struct should be freed with avcodec_free_context().
 *
 * @param codec if non-NULL, allocate private data and initialize defaults
 *              for the given codec. It is illegal to then call avcodec_open2()
 *              with a different codec.
 *
 * @return An AVCodecContext filled with default values or NULL on failure.
 * @see avcodec_get_context_defaults
 *)
function avcodec_alloc_context3(codec: {const} PAVCodec): PAVCodecContext;
  cdecl; external av__codec;

(**
 * Free the codec context and everything associated with it and write NULL to
 * the provided pointer.
 *)
procedure avcodec_free_context(var avctx: PAVCodecContext);
  cdecl; external av__codec;

(**
 * Set the fields of the given AVCodecContext to default values corresponding
 * to the given codec (defaults may be codec-dependent).
 *
 * Do not call this function if a non-NULL codec has been passed
 * to avcodec_alloc_context3() that allocated this AVCodecContext.
 * If codec is non-NULL, it is illegal to call avcodec_open2() with a
 * different codec on this AVCodecContext.
 *)
procedure avcodec_get_context_defaults3(s: PAVCodecContext; codec: {const} PAVCodec);
  cdecl; external av__codec;

(**
 * Get the AVClass for AVCodecContext. It can be used in combination with
 * AV_OPT_SEARCH_FAKE_OBJ for examining options.
 *
 * @see av_opt_find().
 *)
function avcodec_get_class(): {const} PAVClass;
  cdecl; external av__codec;

(**
 * Get the AVClass for AVFrame. It can be used in combination with
 * AV_OPT_SEARCH_FAKE_OBJ for examining options.
 *
 * @see av_opt_find().
 *)
function avcodec_get_frame_class(): {const} PAVClass;
  cdecl; external av__codec;

(**
 * Get the AVClass for AVSubtitleRect. It can be used in combination with
 * AV_OPT_SEARCH_FAKE_OBJ for examining options.
 *
 * @see av_opt_find().
 *)
function avcodec_get_subtitle_rect_class(): {const} PAVClass;
  cdecl; external av__codec;

(**
 * Copy the settings of the source AVCodecContext into the destination
 * AVCodecContext. The resulting destination codec context will be
 * unopened, i.e. you are required to call avcodec_open2() before you
 * can use this AVCodecContext to decode/encode video/audio data.
 *
 * @param dest target codec context, should be initialized with
 *             avcodec_alloc_context3(), but otherwise uninitialized
 * @param src source codec context
 * @return AVERROR() on error (e.g. memory allocation error), 0 on success
 *)
function avcodec_copy_context(dest: PAVCodecContext; src: {const} PAVCodecContext): cint;
  cdecl; external av__codec;

{$IFDEF FF_API_AVFRAME_LAVC}
(**
 * @deprecated use av_frame_alloc()
 *)
function avcodec_alloc_frame(): PAVFrame;
  cdecl; external av__codec;
{$ENDIF}

(**
 * Set the fields of the given AVFrame to default values.
 *
 * @param frame The AVFrame of which the fields should be set to default values.
 *
 * @deprecated use av_frame_unref()
 *)
procedure avcodec_get_frame_defaults(frame: PAVFrame);
  cdecl; external av__codec;

(**
 * Free the frame and any dynamically allocated objects in it,
 * e.g. extended_data.
 *
 * @param frame frame to be freed. The pointer will be set to NULL.
 *
 * @warning this function does NOT free the data buffers themselves
 * (it does not know how, since they might have been allocated with
 *  a custom get_buffer()).
 *
 * @deprecated use av_frame_free()
 *)
procedure avcodec_free_frame(frame: PPAVFrame);
  cdecl; external av__codec;


(**
 * Initialize the AVCodecContext to use the given AVCodec. Prior to using this
 * function the context has to be allocated with avcodec_alloc_context3().
 *
 * The functions avcodec_find_decoder_by_name(), avcodec_find_encoder_by_name(),
 * avcodec_find_decoder() and avcodec_find_encoder() provide an easy way for
 * retrieving a codec.
 *
 * @warning This function is not thread safe!
 *
 * @code
 * avcodec_register_all();
 * av_dict_set(&opts, "b", "2.5M", 0);
 * codec = avcodec_find_decoder(AV_CODEC_ID_H264);
 * if (!codec)
 *     exit(1);
 *
 * context = avcodec_alloc_context3(codec);
 *
 * if (avcodec_open2(context, codec, opts) < 0)
 *     exit(1);
 * @endcode
 *
 * @param avctx The context to initialize.
 * @param codec The codec to open this context for. If a non-NULL codec has been
 *              previously passed to avcodec_alloc_context3() or
 *              avcodec_get_context_defaults3() for this context, then this
 *              parameter MUST be either NULL or equal to the previously passed
 *              codec.
 * @param options A dictionary filled with AVCodecContext and codec-private options.
 *                On return this object will be filled with options that were not found.
 *
 * @return zero on success, a negative value on error
 * @see avcodec_alloc_context3(), avcodec_find_decoder(), avcodec_find_encoder(),
 *      av_dict_set(), av_opt_find().
 *)
function avcodec_open2(avctx: PAVCodecContext; codec: {const} PAVCodec; options: PPAVDictionary): cint;
  cdecl; external av__codec;

(**
 * Close a given AVCodecContext and free all the data associated with it
 * (but not the AVCodecContext itself).
 *
 * Calling this function on an AVCodecContext that hasn't been opened will free
 * the codec-specific data allocated in avcodec_alloc_context3() /
 * avcodec_get_context_defaults3() with a non-NULL codec. Subsequent calls will
 * do nothing.
 *)
function avcodec_close(avctx: PAVCodecContext): cint;
  cdecl; external av__codec;

(**
 * Free all allocated data in the given subtitle struct.
 *
 * @param sub AVSubtitle to free.
 *)
procedure avsubtitle_free(sub: PAVSubtitle);
  cdecl; external av__codec;

(**
 * @}
 *)

(**
 * @addtogroup lavc_packet
 * @{
 *)

{$IFDEF FF_API_DESTRUCT_PACKET}
(*
 * Default packet destructor.
 * @deprecated use the AVBuffer API instead
 *)
procedure av_destruct_packet(pkt: PAVPacket);
  cdecl; external av__codec; deprecated;
{$ENDIF}

(*
 * Initialize optional fields of a packet with default values.
 *
 * Note, this does not touch the data and size members, which have to be
 * initialized separately.
 *
 * @param pkt packet
 *)
procedure av_init_packet(var pkt: TAVPacket);
  cdecl; external av__codec;

(*
 * Allocate the payload of a packet and initialize its fields with
 * default values.
 *
 * @param pkt packet
 * @param size wanted payload size
 * @return 0 if OK, AVERROR_xxx otherwise
 *)
function av_new_packet(pkt: PAVPacket; size: cint): cint;
  cdecl; external av__codec;

(*
 * Reduce packet size, correctly zeroing padding
 *
 * @param pkt packet
 * @param size new size
 *)
procedure av_shrink_packet(pkt: PAVPacket; size: cint);
  cdecl; external av__codec;

(**
 * Increase packet size, correctly zeroing padding
 *
 * @param pkt packet
 * @param grow_by number of bytes by which to increase the size of the packet
 *)
function av_grow_packet(pkt: PAVPacket; grow_by: cint): cint;
  cdecl; external av__codec;

(**
 * Initialize a reference-counted packet from av_malloc()ed data.
 *
 * @param pkt packet to be initialized. This function will set the data, size,
 *        buf and destruct fields, all others are left untouched.
 * @param data Data allocated by av_malloc() to be used as packet data. If this
 *        function returns successfully, the data is owned by the underlying AVBuffer.
 *        The caller may not access the data through other means.
 * @param size size of data in bytes, without the padding. I.e. the full buffer
 *        size is assumed to be size + FF_INPUT_BUFFER_PADDING_SIZE.
 *
 * @return 0 on success, a negative AVERROR on error
 *)
function av_packet_from_data(pkt: PAVPacket; data: PByte; size: cint): cint;
  cdecl; external av__codec;

(*
 * @warning This is a hack - the packet memory allocation stuff is broken. The
 * packet is allocated if it was not really allocated.
 *)
function av_dup_packet(pkt: PAVPacket): cint;
  cdecl; external av__codec;

(**
 * Copy packet, including contents
 *
 * @return 0 on success, negative AVERROR on fail
 *)
function av_copy_packet(dst: PAVPacket; src: {const} PAVPacket): cint;
  cdecl; external av__codec;

(**
 * Copy packet side data
 *
 * @return 0 on success, negative AVERROR on fail
 *)
function av_copy_packet_side_data(dst: PAVPacket; src: {const} PAVPacket): cint;
  cdecl; external av__codec;

(*
 * Free a packet.
 *
 * @param pkt packet to free
 *)
procedure av_free_packet(pkt: PAVPacket);
  cdecl; external av__codec;

(**
 * Allocate new information of a packet.
 *
 * @param pkt packet
 * @param type side information type
 * @param size side information size
 * @return pointer to fresh allocated data or NULL otherwise
 *)
function av_packet_new_side_data(pkt: PAVPacket; type_: TAVPacketSideDataType;
                                 size: cint): PByte;
  cdecl; external av__codec;

(**
 * Shrink the already allocated side data buffer
 *
 * @param pkt packet
 * @param type side information type
 * @param size new side information size
 * @return 0 on success, < 0 on failure
 *)
function av_packet_shrink_side_data(pkt: PAVPacket; type_: TAVPacketSideDataType;
                                size: cint): cint;
  cdecl; external av__codec;

(**
 * Get side information from packet.
 *
 * @param pkt packet
 * @param type desired side information type
 * @param size pointer for side information size to store (optional)
 * @return pointer to data if present or NULL otherwise
 *)
function av_packet_get_side_data(pkt: PAVPacket; type_: TAVPacketSideDataType;
                                 size: Pcint): PByte;
  cdecl; external av__codec;

function av_packet_merge_side_data(pkt: PAVPacket): cint;
  cdecl; external av__codec;

function av_packet_split_side_data(pkt: PAVPacket): cint;
  cdecl; external av__codec;

(**
 * Pack a dictionary for use in side_data.
 *
 * @param dict The dictionary to pack.
 * @param size pointer to store the size of the returned data
 * @return pointer to data if successful, NULL otherwise
 *)
function av_packet_pack_dictionary(dict: PAVDictionary; size: PCuint): PByte;
  cdecl; external av__codec;

(**
 * Unpack a dictionary from side_data.
 *
 * @param data data from side_data
 * @param size size of the data
 * @param dict the metadata storage dictionary
 * @return 0 on success, < 0 on failure
 *)
function av_packet_unpack_dictionary(data: {const} PByte; size: cint; dict: PPAVDictionary): cint;
  cdecl; external av__codec;

(**
 * Convenience function to free all the side data stored.
 * All the other fields stay untouched.
 *
 * @param pkt packet
 *)
procedure av_packet_free_side_data(pkt: PAVPacket);
  cdecl; external av__codec;

(**
 * Setup a new reference to the data described by a given packet
 *
 * If src is reference-counted, setup dst as a new reference to the
 * buffer in src. Otherwise allocate a new buffer in dst and copy the
 * data from src into it.
 *
 * All the other fields are copied from src.
 *
 * @see av_packet_unref
 *
 * @param dst Destination packet
 * @param src Source packet
 *
 * @return 0 on success, a negative AVERROR on error.
 *)
function av_packet_ref(dst: PAVPacket; src: {const} PAVPacket): cint;
  cdecl; external av__codec;

(**
 * Wipe the packet.
 *
 * Unreference the buffer referenced by the packet and reset the
 * remaining packet fields to their default values.
 *
 * @param pkt The packet to be unreferenced.
 *)
procedure av_packet_unref(pkt: PAVPacket);
  cdecl; external av__codec;

(**
 * Move every field in src to dst and reset src.
 *
 * @see av_packet_unref
 *
 * @param src Source packet, will be reset
 * @param dst Destination packet
 *)
procedure av_packet_move_ref(dst: PAVPacket; src: PAVPacket);
  cdecl; external av__codec;

(**
 * Copy only "properties" fields from src to dst.
 *
 * Properties for the purpose of this function are all the fields
 * beside those related to the packet data (buf, data, size)
 *
 * @param dst Destination packet
 * @param src Source packet
 *
 * @return 0 on success AVERROR on failure.
 *
 *)
function av_packet_copy_props(dst: PAVPacket; src: {const} PAVPacket): cint;
  cdecl; external av__codec;

(**
 * Convert valid timing fields (timestamps / durations) in a packet from one
 * timebase to another. Timestamps with unknown values (AV_NOPTS_VALUE) will be
 * ignored.
 *
 * @param pkt packet on which the conversion will be performed
 * @param tb_src source timebase, in which the timing fields in pkt are
 *               expressed
 * @param tb_dst destination timebase, to which the timing fields will be
 *               converted
 *)
procedure av_packet_rescale_ts(pkt: PAVPacket; tb_src, tb_dst: TAVRational);
  cdecl; external av__codec;

(**
 * @
 *)

(**
 * @addtogroup lavc_decoding
 * @
 *)

(**
 * Find a registered decoder with a matching codec ID.
 *
 * @param id CodecID of the requested decoder
 * @return A decoder if one was found, NULL otherwise.
 *)
function avcodec_find_decoder(id: TAVCodecID): PAVCodec;
  cdecl; external av__codec;

(**
 * Find a registered decoder with the specified name.
 *
 * @param name name of the requested decoder
 * @return A decoder if one was found, NULL otherwise.
 *)
function avcodec_find_decoder_by_name(name: PAnsiChar): PAVCodec;
  cdecl; external av__codec;

{$IFDEF FF_API_GET_BUFFER}
function avcodec_default_get_buffer (s: PAVCodecContext; pic: PAVFrame): cint;
  cdecl; external av__codec; deprecated;
procedure avcodec_default_release_buffer (s: PAVCodecContext; pic: PAVFrame);
  cdecl; external av__codec; deprecated;
function avcodec_default_reget_buffer (s: PAVCodecContext; pic: PAVFrame): cint;
  cdecl; external av__codec; deprecated;
{$ENDIF}

(**
 * The default callback for AVCodecContext.get_buffer2(). It is made public so
 * it can be called by custom get_buffer2() implementations for decoders without
 * CODEC_CAP_DR1 set.
 *)
function avcodec_default_get_buffer2(s: PAVCodecContext; frame: PAVFrame; flags: cint): cint;
  cdecl; external av__codec;

{$IFDEF FF_API_EMU_EDGE}
(**
 * Return the amount of padding in pixels which the get_buffer callback must
 * provide around the edge of the image for codecs which do not have the
 * CODEC_FLAG_EMU_EDGE flag.
 *
 * @return Required padding in pixels.
 *
 * @deprecated CODEC_FLAG_EMU_EDGE is deprecated, so this function is no longer
 * needed
 *)
function avcodec_get_edge_width(): cuint;
  cdecl; external av__codec;
{$ENDIF}

(**
 * Modify width and height values so that they will result in a memory
 * buffer that is acceptable for the codec if you do not use any horizontal
 * padding.
 *
 * May only be used if a codec with CODEC_CAP_DR1 has been opened.
 *)
procedure avcodec_align_dimensions(s: PAVCodecContext; width: PCint; height: PCint);
  cdecl; external av__codec;

(**
 * Modifiy width and height values so that they will result in a memory
 * buffer that is acceptable for the codec if you also ensure that all
 * line sizes are a multiple of the respective linesize_align[i].
 *
 * May only be used if a codec with CODEC_CAP_DR1 has been opened.
 *)
procedure avcodec_align_dimensions2(s: PAVCodecContext; width: PCint; height: PCint;
                                    linesize_align: PAVNDPArray);
  cdecl; external av__codec;

{$IFDEF FF_API_OLD_DECODE_AUDIO}
(**
 * Wrapper function which calls avcodec_decode_audio4.
 *
 * @deprecated Use avcodec_decode_audio4 instead.
 *
 * Decode the audio frame of size avpkt->size from avpkt->data into samples.
 * Some decoders may support multiple frames in a single AVPacket, such
 * decoders would then just decode the first frame. In this case,
 * avcodec_decode_audio3 has to be called again with an AVPacket that contains
 * the remaining data in order to decode the second frame etc.
 * If no frame
 * could be outputted, frame_size_ptr is zero. Otherwise, it is the
 * decompressed frame size in bytes.
 *
 * @warning You must set frame_size_ptr to the allocated size of the
 * output buffer before calling avcodec_decode_audio3().
 *
 * @warning The input buffer must be FF_INPUT_BUFFER_PADDING_SIZE larger than
 * the actual read bytes because some optimized bitstream readers read 32 or 64
 * bits at once and could read over the end.
 *
 * @warning The end of the input buffer avpkt->data should be set to 0 to ensure that
 * no overreading happens for damaged MPEG streams.
 *
 * @warning You must not provide a custom get_buffer() when using
 * avcodec_decode_audio3().  Doing so will override it with
 * avcodec_default_get_buffer.  Use avcodec_decode_audio4() instead,
 * which does allow the application to provide a custom get_buffer().
 *
 * @note You might have to align the input buffer avpkt->data and output buffer
 * samples. The alignment requirements depend on the CPU: On some CPUs it isn't
 * necessary at all, on others it won't work at all if not aligned and on others
 * * it will work but it will have an impact on performance.
 *
 * In practice, avpkt->data should have 4 byte alignment at minimum and
 * samples should be 16 byte aligned unless the CPU doesn't need it
 * (AltiVec and SSE do).
 *
 * @note Codecs which have the CODEC_CAP_DELAY capability set have a delay
 * between input and output, these need to be fed with avpkt->data=NULL,
 * avpkt->size=0 at the end to return the remaining frames.
 *
 * @param avctx the codec context
 * @param[out] samples the output buffer, sample type in avctx->sample_fmt
 *                     If the sample format is planar, each channel plane will
 *                     be the same size, with no padding between channels.
 * @param[in,out] frame_size_ptr the output buffer size in bytes
 * @param[in] avpkt The input AVPacket containing the input buffer.
 *            You can create such packet with av_init_packet() and by then setting
 *            data and size, some decoders might in addition need other fields.
 *            All decoders are designed to use the least fields possible though.
 * @return On error a negative value is returned, otherwise the number of bytes
 * used or zero if no frame data was decompressed (used) from the input AVPacket.
 *)
function avcodec_decode_audio3(avctx: PAVCodecContext; samples: PSmallint;
               var frame_size_ptr: cint;
               avpkt: PAVPacket): cint;
  cdecl; external av__codec; deprecated;
{$IFEND}

(**
 * Decode the audio frame of size avpkt->size from avpkt->data into frame.
 *
 * Some decoders may support multiple frames in a single AVPacket. Such
 * decoders would then just decode the first frame. In this case,
 * avcodec_decode_audio4 has to be called again with an AVPacket containing
 * the remaining data in order to decode the second frame, etc...
 * Even if no frames are returned, the packet needs to be fed to the decoder
 * with remaining data until it is completely consumed or an error occurs.
 *
 * @warning The input buffer, avpkt->data must be FF_INPUT_BUFFER_PADDING_SIZE
 *          larger than the actual read bytes because some optimized bitstream
 *          readers read 32 or 64 bits at once and could read over the end.
 *
 * @note You might have to align the input buffer. The alignment requirements
 *       depend on the CPU and the decoder.
 *
 * @param      avctx the codec context
 * @param[out] frame The AVFrame in which to store decoded audio samples.
 *                   The decoder will allocate a buffer for the decoded frame by
 *                   calling the AVCodecContext.get_buffer2() callback.
 *                   When AVCodecContext.refcounted_frames is set to 1, the frame is
 *                   reference counted and the returned reference belongs to the
 *                   caller. The caller must release the frame using av_frame_unref()
 *                   when the frame is no longer needed. The caller may safely write
 *                   to the frame if av_frame_is_writable() returns 1.
 *                   When AVCodecContext.refcounted_frames is set to 0, the returned
 *                   reference belongs to the decoder and is valid only until the
 *                   next call to this function or until closing the decoder.
 *                   The caller may not write to it.
 * @param[out] got_frame_ptr Zero if no frame could be decoded, otherwise it is
 *                           non-zero.
 * @param[in]  avpkt The input AVPacket containing the input buffer.
 *                   At least avpkt->data and avpkt->size should be set. Some
 *                   decoders might also require additional fields to be set.
 * @return A negative error code is returned if an error occurred during
 *         decoding, otherwise the number of bytes consumed from the input
 *         AVPacket is returned.
 *)
function avcodec_decode_audio4(avctx: PAVCodecContext; frame: PAVFrame;
                       got_frame_ptr: Pcint; avpkt: PAVPacket): cint;
  cdecl; external av__codec;

(**
 * Decode the video frame of size avpkt->size from avpkt->data into picture.
 * Some decoders may support multiple frames in a single AVPacket, such
 * decoders would then just decode the first frame.
 *
 * @warning The input buffer must be FF_INPUT_BUFFER_PADDING_SIZE larger than
 * the actual read bytes because some optimized bitstream readers read 32 or 64
 * bits at once and could read over the end.
 *
 * @warning The end of the input buffer buf should be set to 0 to ensure that
 * no overreading happens for damaged MPEG streams.
 *
 * @note You might have to align the input buffer avpkt->data.
 * The alignment requirements depend on the CPU: on some CPUs it isn't
 * necessary at all, on others it won't work at all if not aligned and on others
 * it will work but it will have an impact on performance.
 *
 * In practice, avpkt->data should have 4 byte alignment at minimum.
 *
 * @note Codecs which have the CODEC_CAP_DELAY capability set have a delay
 * between input and output, these need to be fed with avpkt->data=NULL,
 * avpkt->size=0 at the end to return the remaining frames.
 *
 * @param avctx the codec context
 * @param[out] picture The AVFrame in which the decoded video frame will be stored.
 *             Use av_frame_alloc() to get an AVFrame. The codec will
 *             allocate memory for the actual bitmap by calling the
 *             AVCodecContext.get_buffer2() callback.
 *             When AVCodecContext.refcounted_frames is set to 1, the frame is
 *             reference counted and the returned reference belongs to the
 *             caller. The caller must release the frame using av_frame_unref()
 *             when the frame is no longer needed. The caller may safely write
 *             to the frame if av_frame_is_writable() returns 1.
 *             When AVCodecContext.refcounted_frames is set to 0, the returned
 *             reference belongs to the decoder and is valid only until the
 *             next call to this function or until closing the decoder. The
 *             caller may not write to it.
 *
 * @param[in] avpkt The input AVpacket containing the input buffer.
 *            You can create such packet with av_init_packet() and by then setting
 *            data and size, some decoders might in addition need other fields like
 *            flags&AV_PKT_FLAG_KEY. All decoders are designed to use the least
 *            fields possible.
 * @param[in,out] got_picture_ptr Zero if no frame could be decompressed, otherwise, it is nonzero.
 * @return On error a negative value is returned, otherwise the number of bytes
 * used or zero if no frame could be decompressed.
 *)
function avcodec_decode_video2(avctx: PAVCodecContext; picture: PAVFrame;
                       var got_picture_ptr: cint;
                       avpkt: {const} PAVPacket): cint;
  cdecl; external av__codec;

(* Decode a subtitle message.
 * Return a negative value on error, otherwise return the number of bytes used.
 * If no subtitle could be decompressed, got_sub_ptr is zero.
 * Otherwise, the subtitle is stored in *sub.
 * Note that CODEC_CAP_DR1 is not available for subtitle codecs. This is for
 * simplicity, because the performance difference is expect to be negligible
 * and reusing a get_buffer written for video codecs would probably perform badly
 * due to a potentially very different allocation pattern.
 *
 * @param avctx the codec context
 * @param[out] sub The Preallocated AVSubtitle in which the decoded subtitle will be stored,
 *                 must be freed with avsubtitle_free if *got_sub_ptr is set.
 * @param[in,out] got_sub_ptr Zero if no subtitle could be decompressed, otherwise, it is nonzero.
 * @param[in] avpkt The input AVPacket containing the input buffer.
 *)
function avcodec_decode_subtitle2(avctx: PAVCodecContext; sub: PAVSubtitle;
                          var got_sub_ptr: cint;
                          avpkt: PAVPacket): cint;
  cdecl; external av__codec;

(**
 * @defgroup lavc_parsing Frame parsing
 * @
 *)

type
  TAVPictureStructure = (
    AV_PICTURE_STRUCTURE_UNKNOWN,      //< unknown
    AV_PICTURE_STRUCTURE_TOP_FIELD,    //< coded as top field
    AV_PICTURE_STRUCTURE_BOTTOM_FIELD, //< coded as bottom field
    AV_PICTURE_STRUCTURE_FRAME         //< coded as frame
  );

const
  AV_PARSER_PTS_NB      = 4;
  PARSER_FLAG_COMPLETE_FRAMES = $0001;
  PARSER_FLAG_ONCE            = $0002;
/// Set if the parser has a valid file offset
  PARSER_FLAG_FETCHED_OFFSET  = $0004;
  PARSER_FLAG_USE_CODEC_TS    = $1000;

type
  (* frame parsing *)
  PAVCodecParserContext = ^TAVCodecParserContext;
  PAVCodecParser = ^TAVCodecParser;

  TAVCodecParserContext = record
    priv_data: pointer;
    parser: PAVCodecParser;
    frame_offset: cint64; (* offset of the current frame *)
    cur_offset: cint64; (* current offset (incremented by each av_parser_parse()) *)
    next_frame_offset: cint64; (* offset of the next frame *)
    (* video info *)
    pict_type: cint; (* XXX: put it back in AVCodecContext *)
    (**
     * This field is used for proper frame duration computation in lavf.
     * It signals, how much longer the frame duration of the current frame
     * is compared to normal frame duration.
     *
     * frame_duration = (1 + repeat_pict) * time_base
     *
     * It is used by codecs like H.264 to display telecined material.
     *)
    repeat_pict: cint; (* XXX: put it back in AVCodecContext *)
    pts: cint64;     (* pts of the current frame *)
    dts: cint64;     (* dts of the current frame *)

    (* private data *)
    last_pts: cint64;
    last_dts: cint64;
    fetch_timestamp: cint;

    cur_frame_start_index: cint;
    cur_frame_offset: array [0..AV_PARSER_PTS_NB - 1] of cint64;
    cur_frame_pts: array [0..AV_PARSER_PTS_NB - 1] of cint64;
    cur_frame_dts: array [0..AV_PARSER_PTS_NB - 1] of cint64;

    flags: cint;

    offset: cint64;      ///< byte offset from starting packet start
    cur_frame_end: array [0..AV_PARSER_PTS_NB - 1] of cint64;

    (**
     * Set by parser to 1 for key frames and 0 for non-key frames.
     * It is initialized to -1, so if the parser doesn't set this flag,
     * old-style fallback using FF_I_TYPE picture type as key frames
     * will be used.
     *)
    key_frame: cint;

    (**
     * Time difference in stream time base units from the pts of this
     * packet to the point at which the output from the decoder has converged
     * independent from the availability of previous frames. That is, the
     * frames are virtually identical no matter if decoding started from
     * the very first frame or from this keyframe.
     * Is AV_NOPTS_VALUE if unknown.
     * This field has no meaning if the packet does not have AV_PKT_FLAG_KEY
     * set.
     *
     * The purpose of this field is to allow seeking in streams that have no
     * keyframes in the conventional sense. It corresponds to the
     * recovery point SEI in H.264 and match_time_delta in NUT. It is also
     * essential for some types of subtitle streams to ensure that all
     * subtitles are correctly displayed after seeking.
     *)
    convergence_duration: cint64;

    // Timestamp generation support:
    (**
     * Synchronization point for start of timestamp generation.
     *
     * Set to >0 for sync point, 0 for no sync point and <0 for undefined
     * (default).
     *
     * For example, this corresponds to presence of H.264 buffering period
     * SEI message.
     *)
    dts_sync_point: cint;

    (**
     * Offset of the current timestamp against last timestamp sync point in
     * units of AVCodecContext.time_base.
     *
     * Set to INT_MIN when dts_sync_point unused. Otherwise, it must
     * contain a valid timestamp offset.
     *
     * Note that the timestamp of sync point has usually a nonzero
     * dts_ref_dts_delta, which refers to the previous sync point. Offset of
     * the next frame after timestamp sync point will be usually 1.
     *
     * For example, this corresponds to H.264 cpb_removal_delay.
     *)
    dts_ref_dts_delta: cint;

    (**
     * Presentation delay of current frame in units of AVCodecContext.time_base.
     *
     * Set to INT_MIN when dts_sync_point unused. Otherwise, it must
     * contain valid non-negative timestamp delta (presentation time of a frame
     * must not lie in the past).
     *
     * This delay represents the difference between decoding and presentation
     * time of the frame.
     *
     * For example, this corresponds to H.264 dpb_output_delay.
     *)
    pts_dts_delta: cint;

    (**
     * Position of the packet in file.
     *
     * Analogous to cur_frame_pts/dts
     *)
    cur_frame_pos: array [0..AV_PARSER_PTS_NB - 1] of cint64;

    (**
     * Byte position of currently parsed frame in stream.
     *)
    pos: cint64;

    (**
     * Previous frame byte position.
     *)
    last_pos: cint64;

    (**
     * Duration of the current frame.
     * For audio, this is in units of 1 / AVCodecContext.sample_rate.
     * For all other types, this is in units of AVCodecContext.time_base.
     *)
    duration: cint;

    field_order: TAVFieldOrder;

    (**
     * Indicate whether a picture is coded as a frame, top field or bottom field.
     *
     * For example, H.264 field_pic_flag equal to 0 corresponds to
     * AV_PICTURE_STRUCTURE_FRAME. An H.264 picture with field_pic_flag
     * equal to 1 and bottom_field_flag equal to 0 corresponds to
     * AV_PICTURE_STRUCTURE_TOP_FIELD.
     *)
    picture_structure: TAVPictureStructure;

  end; {AVCodecParserContext}

  TAVCodecParser = record
    codec_ids: array [0..4] of cint; (* several codec IDs are permitted *)
    priv_data_size: cint;
    parser_init: function(s: PAVCodecParserContext): cint; cdecl;
    parser_parse: function(s: PAVCodecParserContext;
                           avctx: PAVCodecContext;
                           poutbuf: {const} PPointer; poutbuf_size: PCint;
                           buf: {const} PByteArray; buf_size: cint): cint; cdecl;
    parser_close: procedure(s: PAVCodecParserContext); cdecl;
    split: function(avctx: PAVCodecContext; buf: {const} PByteArray;
                    buf_size: cint): cint; cdecl;
    next: PAVCodecParser;
  end; {AVCodecParser}

function av_parser_next(c: {const} PAVCodecParser): PAVCodecParser;
  cdecl; external av__codec;

procedure av_register_codec_parser(parser: PAVCodecParser);
  cdecl; external av__codec;

function av_parser_init(codec_id: cint): PAVCodecParserContext;
  cdecl; external av__codec;

(**
 * Parse a packet.
 *
 * @param s             parser context.
 * @param avctx         codec context.
 * @param poutbuf       set to pointer to parsed buffer or NULL if not yet finished.
 * @param poutbuf_size  set to size of parsed buffer or zero if not yet finished.
 * @param buf           input buffer.
 * @param buf_size      input length, to signal EOF, this should be 0 (so that the last frame can be output).
 * @param pts           input presentation timestamp.
 * @param dts           input decoding timestamp.
 * @param pos           input byte position in stream.
 * @return the number of bytes of the input bitstream used.
 *
 * Example:
 * @code
 *   while (in_len) do
 *   begin
 *     len := av_parser_parse2(myparser, AVCodecContext, data, size,
 *                                       in_data, in_len,
 *                                       pts, dts, pos);
 *      in_data := in_data + len;
 *      in_len  := in_len  - len;
 *
 *      if (size) then
 *        decode_frame(data, size);
 *   end;
 * @endcode
 *)
function av_parser_parse2(s:    PAVCodecParserContext;
                  avctx:        PAVCodecContext;
                  poutbuf:      PPointer;
                  poutbuf_size: PCint;
                  buf: {const}  PByteArray;
                  buf_size:     cint;
                  pts:          cint64;
                  dts:          cint64;
                  pos:          cint64): cint;
   cdecl; external av__codec;

(**
 * @return 0 if the output buffer is a subset of the input, 1 if it is allocated and must be freed
 * @deprecated use AVBitStreamFilter
 *)
function av_parser_change(s: PAVCodecParserContext;
                   avctx: PAVCodecContext;
                   poutbuf: PPointer; poutbuf_size: PCint;
                   buf: {const} PByteArray; buf_size: cint; keyframe: cint): cint;
  cdecl; external av__codec;

procedure av_parser_close(s: PAVCodecParserContext);
  cdecl; external av__codec;

(**
 * @}
 * @}
 *)

(**
 * @addtogroup lavc_encoding
 * @{
 *)

(**
 * Find a registered encoder with a matching codec ID.
 *
 * @param id AVCodecID of the requested encoder
 * @return An encoder if one was found, NULL otherwise.
 *)
function avcodec_find_encoder(id: TAVCodecID): PAVCodec;
  cdecl; external av__codec;

(**
 * Find a registered encoder with the specified name.
 *
 * @param name name of the requested encoder
 * @return An encoder if one was found, NULL otherwise.
 *)
function avcodec_find_encoder_by_name(name: PAnsiChar): PAVCodec;
  cdecl; external av__codec;

{$IFDEF FF_API_OLD_ENCODE_AUDIO}
(**
 * Encode an audio frame from samples into buf.
 *
 * @deprecated Use avcodec_encode_audio2 instead.
 *
 * @note The output buffer should be at least FF_MIN_BUFFER_SIZE bytes large.
 * However, for codecs with avctx->frame_size equal to 0 (e.g. PCM) the user
 * will know how much space is needed because it depends on the value passed
 * in buf_size as described below. In that case a lower value can be used.
 *
 * @param avctx the codec context
 * @param[out] buf the output buffer
 * @param[in] buf_size the output buffer size
 * @param[in] samples the input buffer containing the samples
 * The number of samples read from this buffer is frame_size*channels,
 * both of which are defined in avctx.
 * For codecs which have avctx->frame_size equal to 0 (e.g. PCM) the number of
 * samples read from samples is equal to:
 * buf_size * 8 / (avctx->channels * av_get_bits_per_sample(avctx->codec_id))
 * This also implies that av_get_bits_per_sample() must not return 0 for these
 * codecs.
 * @return On error a negative value is returned, on success zero or the number
 * of bytes used to encode the data read from the input buffer.
 *)
function avcodec_encode_audio(avctx: PAVCodecContext; buf: PByte;
                      buf_size: cint; samples: {const} PSmallint): cint;
  cdecl; external av__codec; deprecated;
{$IFEND}

(**
 * Encode a frame of audio.
 *
 * Takes input samples from frame and writes the next output packet, if
 * available, to avpkt. The output packet does not necessarily contain data for
 * the most recent frame, as encoders can delay, split, and combine input frames
 * internally as needed.
 *
 * @param avctx     codec context
 * @param avpkt     output AVPacket.
 *                  The user can supply an output buffer by setting
 *                  avpkt->data and avpkt->size prior to calling the
 *                  function, but if the size of the user-provided data is not
 *                  large enough, encoding will fail. If avpkt->data and
 *                  avpkt->size are set, avpkt->destruct must also be set. All
 *                  other AVPacket fields will be reset by the encoder using
 *                  av_init_packet(). If avpkt->data is NULL, the encoder will
 *                  allocate it. The encoder will set avpkt->size to the size
 *                  of the output packet.
 *
 *                  If this function fails or produces no output, avpkt will be
 *                  freed using av_free_packet() (i.e. avpkt->destruct will be
 *                  called to free the user supplied buffer).
 * @param[in] frame AVFrame containing the raw audio data to be encoded.
 *                  May be NULL when flushing an encoder that has the
 *                  CODEC_CAP_DELAY capability set.
 *                  If CODEC_CAP_VARIABLE_FRAME_SIZE is set, then each frame
 *                  can have any number of samples.
 *                  If it is not set, frame->nb_samples must be equal to
 *                  avctx->frame_size for all frames except the last.
 *                  The final frame may be smaller than avctx->frame_size.
 * @param[out] got_packet_ptr This field is set to 1 by libavcodec if the
 *                            output packet is non-empty, and to 0 if it is
 *                            empty. If the function returns an error, the
 *                            packet can be assumed to be invalid, and the
 *                            value of got_packet_ptr is undefined and should
 *                            not be used.
 * @return          0 on success, negative error code on failure
 *)
function avcodec_encode_audio2(avctx: PAVCodecContext; avpkt: PAVPacket;
                          frame: {const} PAVFrame; got_packet_ptr: Pcint): cint;
  cdecl; external av__codec;

{$IFDEF FF_API_OLD_ENCODE_AUDIO}
(**
 * @deprecated use avcodec_encode_video2() instead.
 *
 * Encode a video frame from pict into buf.
 * The input picture should be
 * stored using a specific format, namely avctx.pix_fmt.
 *
 * @param avctx the codec context
 * @param[out] buf the output buffer for the bitstream of encoded frame
 * @param[in] buf_size the size of the output buffer in bytes
 * @param[in] pict the input picture to encode
 * @return On error a negative value is returned, on success zero or the number
 * of bytes used from the output buffer.
 *)
function avcodec_encode_video(avctx: PAVCodecContext; buf: PByte;
                      buf_size: cint; pict: {const} PAVFrame): cint;
  cdecl; external av__codec; deprecated;
{$IFEND}

(**
 * Encode a frame of video.
 *
 * Takes input raw video data from frame and writes the next output packet, if
 * available, to avpkt. The output packet does not necessarily contain data for
 * the most recent frame, as encoders can delay and reorder input frames
 * internally as needed.
 *
 * @param avctx     codec context
 * @param avpkt     output AVPacket.
 *                  The user can supply an output buffer by setting
 *                  avpkt->data and avpkt->size prior to calling the
 *                  function, but if the size of the user-provided data is not
 *                  large enough, encoding will fail. All other AVPacket fields
 *                  will be reset by the encoder using av_init_packet(). If
 *                  avpkt->data is NULL, the encoder will allocate it.
 *                  The encoder will set avpkt->size to the size of the
 *                  output packet. The returned data (if any) belongs to the
 *                  caller, he is responsible for freeing it.
 *
 *                  If this function fails or produces no output, avpkt will be
 *                  freed using av_free_packet() (i.e. avpkt->destruct will be
 *                  called to free the user supplied buffer).
 * @param[in] frame AVFrame containing the raw video data to be encoded.
 *                  May be NULL when flushing an encoder that has the
 *                  CODEC_CAP_DELAY capability set.
 * @param[out] got_packet_ptr This field is set to 1 by libavcodec if the
 *                            output packet is non-empty, and to 0 if it is
 *                            empty. If the function returns an error, the
 *                            packet can be assumed to be invalid, and the
 *                            value of got_packet_ptr is undefined and should
 *                            not be used.
 * @return          0 on success, negative error code on failure
 *)
function avcodec_encode_video2(avctx: PAVCodecContext; avpkt: PAVPacket;
                      frame: {const} PAVFrame; got_packet_ptr: Pcint): cint;
  cdecl; external av__codec;

function avcodec_encode_subtitle(avctx: PAVCodecContext; buf: PByteArray;
                      buf_size: cint; sub: {const} PAVSubtitle): cint;
  cdecl; external av__codec;

(**
 * @}
 *)

{$IFDEF FF_API_AVCODEC_RESAMPLE}
(**
 * @defgroup lavc_resample Audio resampling
 * @ingroup libavc
 * @deprecated use libswresample instead
 *
 * @{
 *)
type
  PReSampleContext = pointer;
  PAVResampleContext = pointer;
  PImgReSampleContext = pointer;

(**
 *  Initialize audio resampling context.
 *
 * @param output_channels  number of output channels
 * @param input_channels   number of input channels
 * @param output_rate      output sample rate
 * @param input_rate       input sample rate
 * @param sample_fmt_out   requested output sample format
 * @param sample_fmt_in    input sample format
 * @param filter_length    length of each FIR filter in the filterbank relative to the cutoff frequency
 * @param log2_phase_count log2 of the number of entries in the polyphase filterbank
 * @param linear           if 1 then the used FIR filter will be linearly interpolated
                           between the 2 closest, if 0 the closest will be used
 * @param cutoff           cutoff frequency, 1.0 corresponds to half the output sampling rate
 * @return allocated ReSampleContext, NULL if error occurred
 *)
 function av_audio_resample_init(output_channels: cint; input_channels: cint;
                                 output_rate: cint; input_rate: cint;
                                 sample_fmt_out: TAVSampleFormat;
                                 sample_fmt_in: TAVSampleFormat;
                                 filter_length: cint; log2_phase_count: cint;
                                 linear: cint; cutoff: cdouble): PReSampleContext;
  cdecl; external av__codec; deprecated;
          
function audio_resample (s: PReSampleContext; output: PSmallint; input: PSmallint; nb_samples: cint): cint;
  cdecl; external av__codec; deprecated;

(**
 * Free resample context.
 *
 * @param s a non-NULL pointer to a resample context previously
 *          created with av_audio_resample_init()
 *)
procedure audio_resample_close (s: PReSampleContext);
  cdecl; external av__codec; deprecated;

(**
 * Initialize an audio resampler.
 * Note, if either rate is not an integer then simply scale both rates up so they are.
 * @param filter_length length of each FIR filter in the filterbank relative to the cutoff freq
 * @param log2_phase_count log2 of the number of entries in the polyphase filterbank
 * @param linear If 1 then the used FIR filter will be linearly interpolated
                 between the 2 closest, if 0 the closest will be used
 * @param cutoff cutoff frequency, 1.0 corresponds to half the output sampling rate
 *)
function av_resample_init (out_rate: cint; in_rate: cint; filter_length: cint;
                           log2_phase_count: cint; linear: cint; cutoff: cdouble): PAVResampleContext;
  cdecl; external av__codec; deprecated;

(**
 * Resample an array of samples using a previously configured context.
 * @param src an array of unconsumed samples
 * @param consumed the number of samples of src which have been consumed are returned here
 * @param src_size the number of unconsumed samples available
 * @param dst_size the amount of space in samples available in dst
 * @param update_ctx If this is 0 then the context will not be modified, that way several channels can be resampled with the same context.
 * @return the number of samples written in dst or -1 if an error occurred
 *)
function av_resample (c: PAVResampleContext; dst: PSmallint; src: PSmallint; var consumed: cint;
                      src_size: cint; dst_size: cint; update_ctx: cint): cint;
  cdecl; external av__codec; deprecated;

(**
 * Compensate samplerate/timestamp drift. The compensation is done by changing
 * the resampler parameters, so no audible clicks or similar distortions occur
 * @param compensation_distance distance in output samples over which the compensation should be performed
 * @param sample_delta number of output samples which should be output less
 *
 * example: av_resample_compensate(c, 10, 500)
 * here instead of 510 samples only 500 samples would be output
 *
 * note, due to rounding the actual compensation might be slightly different,
 * especially if the compensation_distance is large and the in_rate used during init is small
 *)
procedure av_resample_compensate (c: PAVResampleContext; sample_delta: cint;
                                  compensation_distance: cint);
  cdecl; external av__codec; deprecated;

procedure av_resample_close (c: PAVResampleContext);
  cdecl; external av__codec; deprecated;

(**
 * @}
 *)
{$ENDIF}

(**
 * @addtogroup lavc_picture
 * @{
 *)

(**
 * Allocate memory for a picture.  Call avpicture_free to free it.
 *
 * @see avpicture_fill()
 *
 * @param picture the picture to be filled in.
 * @param pix_fmt the format of the picture.
 * @param width the width of the picture.
 * @param height the height of the picture.
 * @return Zero if successful, a negative value if not.
 *)
function avpicture_alloc (picture: PAVPicture; pix_fmt: TAVPixelFormat;
                          width: cint; height: cint): cint;
  cdecl; external av__codec;

(**
 * Free a picture previously allocated by avpicture_alloc().
 *
 * @param picture the AVPicture to be freed
 *)
procedure avpicture_free (picture: PAVPicture);
  cdecl; external av__codec;

(**
 * Fill in the AVPicture fields, always assume a linesize alignment of
 * 1.
 *
 * @see av_image_fill_arrays()
 *)
function avpicture_fill (picture: PAVPicture; ptr: pcuint8;
                 pix_fmt: TAVPixelFormat; width: cint; height: cint): cint;
  cdecl; external av__codec;

(**
 * Copy pixel data from an AVPicture into a buffer, always assume a
 * linesize alignment of 1.
 *
 * @see av_image_copy_to_buffer()
 *)
function avpicture_layout (src: {const} PAVPicture; pix_fmt: TAVPixelFormat;
                   width: cint; height: cint;
                   dest: PByteArray; dest_size: cint): cint;
  cdecl; external av__codec;

(**
 * Calculate the size in bytes that a picture of the given width and height
 * would occupy if stored in the given picture format.
 * Always assume a linesize alignment of 1.
 *
 * @see av_image_get_buffer_size().
 *)
function avpicture_get_size (pix_fmt: TAVPixelFormat; width: cint; height: cint): cint;
  cdecl; external av__codec;

{$IFDEF FF_API_DEINTERLACE}
(**
 * deinterlace - if not supported return -1
 *
 * @deprecated - use yadif (in libavfilter) instead
 *)
function avpicture_deinterlace (dst: PAVPicture; src: {const} PAVPicture;
                        pix_fmt: TAVPixelFormat; width: cint; height: cint): cint;
  cdecl; external av__codec; deprecated;
{$ENDIF}

(**
 * Copy image src to dst. Wraps av_image_copy().
 *)
procedure av_picture_copy(dst: PAVPicture; src: {const} PAVPicture;
              pix_fmt: TAVPixelFormat; width: cint; height: cint);
  cdecl; external av__codec;

(**
 * Crop image top and left side.
 *)
function av_picture_crop(dst: PAVPicture; src: {const} PAVPicture;
              pix_fmt: TAVPixelFormat; top_band: cint; left_band: cint): cint;
  cdecl; external av__codec;

(**
 * Pad image.
 *)
function av_picture_pad(dst: PAVPicture; src: {const} PAVPicture;
            height: cint; width: cint; pix_fmt: TAVPixelFormat;
            padtop: cint;  padbottom: cint; padleft: cint;
            padright: cint; color: PCint): cint;
  cdecl; external av__codec;

(**
 * @}
 *)

(**
 * @defgroup lavc_misc Utility functions
 * @ingroup libavc
 *
 * Miscellaneous utility functions related to both encoding and decoding
 * (or neither).
 * @
 *)

(**
 * @defgroup lavc_misc_pixfmt Pixel formats
 *
 * Functions for working with pixel formats.
 * @
 *)

(**
 * Utility function to access log2_chroma_w log2_chroma_h from
 * the pixel format AVPixFmtDescriptor.
 *
 * This function asserts that pix_fmt is valid. See av_pix_fmt_get_chroma_sub_sample
 * for one that returns a failure code and continues in case of invalid
 * pix_fmts.
 *
 * @param[in]  pix_fmt the pixel format
 * @param[out] h_shift store log2_chroma_w
 * @param[out] v_shift store log2_chroma_h
 *
 * @see av_pix_fmt_get_chroma_sub_sample
 *)

procedure avcodec_get_chroma_sub_sample (pix_fmt: TAVPixelFormat; var h_shift: cint; var v_shift: cint);
  cdecl; external av__codec;

(**
 * Return a value representing the fourCC code associated to the
 * pixel format pix_fmt, or 0 if no associated fourCC code can be
 * found.
 *)
function avcodec_pix_fmt_to_codec_tag(pix_fmt: TAVPixelFormat): cuint;
  cdecl; external av__codec;

(**
 * @deprecated see av_get_pix_fmt_loss()
 *)
function avcodec_get_pix_fmt_loss (dst_pix_fmt: TAVPixelFormat; src_pix_fmt: TAVPixelFormat;
                           has_alpha: cint): cint;
  cdecl; external av__codec;

(**
 * Find the best pixel format to convert to given a certain source pixel
 * format.  When converting from one pixel format to another, information loss
 * may occur.  For example, when converting from RGB24 to GRAY, the color
 * information will be lost. Similarly, other losses occur when converting from
 * some formats to other formats. avcodec_find_best_pix_fmt_of_2() searches which of
 * the given pixel formats should be used to suffer the least amount of loss.
 * The pixel formats from which it chooses one, are determined by the
 * pix_fmt_list parameter.
 *
 *
 * @param[in] pix_fmt_list AV_PIX_FMT_NONE terminated array of pixel formats to choose from
 * @param[in] src_pix_fmt source pixel format
 * @param[in] has_alpha Whether the source pixel format alpha channel is used.
 * @param[out] loss_ptr Combination of flags informing you what kind of losses will occur.
 * @return The best pixel format to convert to or -1 if none was found.
 *)
function avcodec_find_best_pix_fmt_of_list(pix_fmt_list: PAVPixelFormat;
                                           src_pix_fmt: TAVPixelFormat;
                                           has_alpha: cint; loss_ptr: Pcint): TAVPixelFormat;
  cdecl; external av__codec;

(**
 * @deprecated see av_find_best_pix_fmt_of_2()
 *)
function avcodec_find_best_pix_fmt_of_2(dst_pix_fmt1: TAVPixelFormat; dst_pix_fmt2: TAVPixelFormat;
                                        src_pix_fmt: TAVPixelFormat; has_alpha: cint; loss_ptr: Pcint): TAVPixelFormat;
  cdecl; external av__codec;

{$IFDEF AV_HAVE_INCOMPATIBLE_LIBAV_ABI}
function avcodec_find_best_pix_fmt2(pix_fmt_list: PAVPixelFormat;
                                    src_pix_fmt: TAVPixelFormat;
                                    has_alpha: cint; loss_ptr: Pcint): TAVPixelFormat;
  cdecl; external av__codec; deprecated;
{$ELSE}
function avcodec_find_best_pix_fmt2(dst_pix_fmt1: TAVPixelFormat; dst_pix_fmt2: TAVPixelFormat;
                      src_pix_fmt: TAVPixelFormat; has_alpha: cint; loss_ptr: Pcint): TAVPixelFormat;
  cdecl; external av__codec; deprecated;
{$ENDIF}

function avcodec_default_get_format(s: PAVCodecContext; fmt: {const} PAVPixelFormat): TAVPixelFormat;
  cdecl; external av__codec;

(**
 * @}
 *)

{$IFDEF FF_API_SET_DIMENSIONS}
(**
 * @deprecated this function is not supposed to be used from outside of lavc
 *)  
procedure avcodec_set_dimensions(s: PAVCodecContext; width: cint; height: cint);
  cdecl; external av__codec;
{$ENDIF}

(* Put a string representing the codec tag codec_tag in buf.
 *
 * @param buf       buffer to place codec tag in
 * @param buf_size size in bytes of buf
 * @param codec_tag codec tag to assign
 * @return the length of the string that would have been generated if
 * enough space had been available, excluding the trailing null
 *)
function av_get_codec_tag_string(buf: PAnsiChar; buf_size: size_t; codec_tag: cuint): size_t;
  cdecl; external av__codec;

procedure avcodec_string(buf: PAnsiChar; buf_size: cint; enc: PAVCodecContext; encode: cint);
  cdecl; external av__codec;

(**
 * Return a name for the specified profile, if available.
 *
 * @param codec the codec that is searched for the given profile
 * @param profile the profile value for which a name is requested
 * @return A name for the profile if found, NULL otherwise.
 *)
function av_get_profile_name(codec: {const} PAVCodec; profile: cint): {const} PAnsiChar;
  cdecl; external av__codec;

function avcodec_default_execute(s: PAVCodecContext; func: TExecuteFunc; arg: Pointer; var ret: cint; count: cint; size: cint): cint;
  cdecl; external av__codec;

function avcodec_default_execute2(s: PAVCodecContext; func: TExecuteFunc; arg: Pointer; var ret: cint; count: cint): cint;
  cdecl; external av__codec;
//FIXME func typedef

(**
 * Fill AVFrame audio data and linesize pointers.
 *
 * The buffer buf must be a preallocated buffer with a size big enough
 * to contain the specified samples amount. The filled AVFrame data
 * pointers will point to this buffer.
 *
 * AVFrame extended_data channel pointers are allocated if necessary for
 * planar audio.
 *
 * @param frame       the AVFrame
 *                    frame->nb_samples must be set prior to calling the
 *                    function. This function fills in frame->data,
 *                    frame->extended_data, frame->linesize[0].
 * @param nb_channels channel count
 * @param sample_fmt  sample format
 * @param buf         buffer to use for frame data
 * @param buf_size    size of buffer
 * @param align       plane size sample alignment
 * @return            >=0 on success, negative error code on failure
 * @todo return the size in bytes required to store the samples in
 * case of success, at the next libavutil bump
 *)
function avcodec_fill_audio_frame(frame: PAVFrame; nb_channels: cint;
                             sample_fmt: TAVSampleFormat; buf: {const} PByte;
                             buf_size: cint; align: cint): cint;
  cdecl; external av__codec;

(**
 * Flush buffers, should be called when seeking or when switching to a different stream.
 *)
procedure avcodec_flush_buffers(avctx: PAVCodecContext);
  cdecl; external av__codec;

(**
 * Return codec bits per sample.
 *
 * @param[in] codec_id the codec
 * @return Number of bits per sample or zero if unknown for the given codec.
 *)
function av_get_bits_per_sample(codec_id: TAVCodecID): cint;
  cdecl; external av__codec;

(**
 * Return the PCM codec associated with a sample format.
 * @param be  endianness, 0 for little, 1 for big,
 *            -1 (or anything else) for native
 * @return  AV_CODEC_ID_PCM_* or AV_CODEC_ID_NONE
 *)
function av_get_pcm_codec(fmt: TAVSampleFormat; be: cint): TAVCodecID;
  cdecl; external av__codec;

(**
 * Return codec bits per sample.
 * Only return non-zero if the bits per sample is exactly correct, not an
 * approximation.
 *
 * @param[in] codec_id the codec
 * @return Number of bits per sample or zero if unknown for the given codec.
 *)
function av_get_exact_bits_per_sample(codec_id: TAVCodecID): cint;
  cdecl; external av__codec;

(**
 * Return audio frame duration.
 *
 * @param avctx        codec context
 * @param frame_bytes  size of the frame, or 0 if unknown
 * @return             frame duration, in samples, if known. 0 if not able to
 *                     determine.
 *)
function av_get_audio_frame_duration(avctx: PAVCodecContext; frame_bytes: cint): cint;
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
    name: PAnsiChar;
    priv_data_size: cint;
    filter: function(bsfc: PAVBitStreamFilterContext;
                  avctx: PAVCodecContext; args: {const} PAnsiChar;
                  poutbuf: PPointer; poutbuf_size: PCint;
                  buf: {const} PByte; buf_size: cint; keyframe: cint): cint; cdecl;
    close: procedure(bsfc: PAVBitStreamFilterContext);
    next: PAVBitStreamFilter;
  end;

(**
 * Register a bitstream filter.
 *
 * The filter will be accessible to the application code through
 * av_bitstream_filter_next() or can be directly initialized with
 * av_bitstream_filter_init().
 *
 * @see avcodec_register_all()
 *)
procedure av_register_bitstream_filter(bsf: PAVBitStreamFilter);
  cdecl; external av__codec;

(**
 * Create and initialize a bitstream filter context given a bitstream
 * filter name.
 *
 * The returned context must be freed with av_bitstream_filter_close().
 *
 * @param name    the name of the bitstream filter
 * @return a bitstream filter context if a matching filter was found
 * and successfully initialized, NULL otherwise
 *)
function av_bitstream_filter_init(name: {const} PAnsiChar): PAVBitStreamFilterContext;
  cdecl; external av__codec;

(**
 * Filter bitstream.
 *
 * This function filters the buffer buf with size buf_size, and places the
 * filtered buffer in the buffer pointed to by poutbuf.
 *
 * The output buffer must be freed by the caller.
 *
 * @param bsfc            bitstream filter context created by av_bitstream_filter_init()
 * @param avctx           AVCodecContext accessed by the filter, may be NULL.
 *                        If specified, this must point to the encoder context of the
 *                        output stream the packet is sent to.
 * @param args            arguments which specify the filter configuration, may be NULL
 * @param poutbuf         pointer which is updated to point to the filtered buffer
 * @param poutbuf_size    pointer which is updated to the filtered buffer size in bytes
 * @param buf             buffer containing the data to filter
 * @param buf_size        size in bytes of buf
 * @param keyframe        set to non-zero if the buffer to filter corresponds to a key-frame packet data
 * @return >= 0 in case of success, or a negative error code in case of failure
 *
 * If the return value is positive, an output buffer is allocated and
 * is available in *poutbuf, and is distinct from the input buffer.
 *
 * If the return value is 0, the output output buffer is not allocated
 * and the output buffer should be considered identical to the input
 * buffer, or in case *poutbuf was set it points to the input buffer
 * (not necessarily to its starting address).
 *)
function av_bitstream_filter_filter(bsfc: PAVBitStreamFilterContext;
                               avctx: PAVCodecContext; args: {const} PAnsiChar;
                               poutbuf: PPointer; poutbuf_size: PCint;
                               buf: {const} PByte; buf_size: cint; keyframe: cint): cint;
  cdecl; external av__codec;

(**
 * Release bitstream filter context.
 *
 * @param bsf the bitstream filter context created with
 * av_bitstream_filter_init(), can be NULL
 *)
procedure av_bitstream_filter_close(bsf: PAVBitStreamFilterContext);
  cdecl; external av__codec;

(**
 * If f is NULL, return the first registered bitstream filter,
 * if f is non-NULL, return the next registered bitstream filter
 * after f, or NULL if f is the last one.
 *
 * This function can be used to iterate over all registered bitstream
 * filters.
 *)
function av_bitstream_filter_next(f: {const} PAVBitStreamFilter): PAVBitStreamFilter;
  cdecl; external av__codec;

(* memory *)

(**
 * Same behaviour av_fast_malloc but the buffer has additional
 * FF_INPUT_BUFFER_PADDING_SIZE at the end which will will always be 0.
 *
 * In addition the whole buffer will initially and after resizes
 * be 0-initialized so that no uninitialized data will ever appear.
 *)
procedure av_fast_padded_malloc(ptr: pointer; size: Pcuint; min_size: size_t);
  cdecl; external av__codec;

(**
 * Same behaviour av_fast_padded_malloc except that buffer will always
 * be 0-initialized after call.
 *)
procedure av_fast_padded_mallocz(ptr: pointer; size: Pcuint; min_size: size_t);
  cdecl; external av__codec;

(**
 * Encode extradata length to a buffer. Used by xiph codecs.
 *
 * @param s buffer to write to; must be at least (v/255+1) bytes long
 * @param v size of extradata in bytes
 * @return number of bytes written to the buffer.
 *)
function av_xiphlacing(s: PByte; v: cuint): cuint;
  cdecl; external av__codec;

{$IFDEF FF_API_MISSING_SAMPLE}
(**
 * Log a generic warning message about a missing feature. This function is
 * intended to be used internally by FFmpeg (libavcodec, libavformat, etc.)
 * only, and would normally not be used by applications.
 * @param[in] avc a pointer to an arbitrary struct of which the first field is
 * a pointer to an AVClass struct
 * @param[in] feature string containing the name of the missing feature
 * @param[in] want_sample indicates if samples are wanted which exhibit this feature.
 * If want_sample is non-zero, additional verbage will be added to the log
 * message which tells the user how to report samples to the development
 * mailing list.
 * @deprecated Use avpriv_report_missing_feature() instead.
 *)
procedure av_log_missing_feature(avc: Pointer; feature: {const} PAnsiChar; want_sample: cint);
  cdecl; external av__codec; deprecated;

(**
 * Log a generic warning message asking for a sample. This function is
 * intended to be used internally by FFmpeg (libavcodec, libavformat, etc.)
 * only, and would normally not be used by applications.
 * @param[in] avc a pointer to an arbitrary struct of which the first field is
 * a pointer to an AVClass struct
 * @param[in] msg string containing an optional message, or NULL if no message
 * @deprecated Use avpriv_request_sample() instead.
 *)
procedure av_log_ask_for_sample(avc: Pointer; msg: {const} PAnsiChar); {todo: av_printf_format(2, 3);}
  cdecl; external av__codec; deprecated;
{$ENDIF}

(**
 * Register the hardware accelerator hwaccel.
 *)
procedure av_register_hwaccel (hwaccel: PAVHWAccel)
  cdecl; external av__codec;

(**
 * If hwaccel is NULL, returns the first registered hardware accelerator,
 * if hwaccel is non-NULL, returns the next registered hardware accelerator
 * after hwaccel, or NULL if hwaccel is the last one.
 *)
function av_hwaccel_next (hwaccel: {const} PAVHWAccel): PAVHWAccel;
  cdecl; external av__codec;

(**
 * Lock operation used by lockmgr
 *)
type
  TAVLockOp = (
    AV_LOCK_CREATE,  ///< Create a mutex
    AV_LOCK_OBTAIN,  ///< Lock the mutex
    AV_LOCK_RELEASE, ///< Unlock the mutex
    AV_LOCK_DESTROY  ///< Free mutex resources
  );

(**
 * Register a user provided lock manager supporting the operations
 * specified by AVLockOp. The "mutex" argument to the function points
 * to a (void * ) where the lockmgr should store/get a pointer to a user
 * allocated mutex. It is NULL upon AV_LOCK_CREATE and equal to the
 * value left by the last call for all other ops. If the lock manager is
 * unable to perform the op then it should leave the mutex in the same
 * state as when it was called and return a non-zero value. However,
 * when called with AV_LOCK_DESTROY the mutex will always be assumed to
 * have been successfully destroyed. If av_lockmgr_register succeeds
 * it will return a non-negative value, if it fails it will return a
 * negative value and destroy all mutex and unregister all callbacks.
 * av_lockmgr_register is not thread-safe, it must be called from a
 * single thread before any calls which make use of locking are used.
 *
 * @param cb User defined callback. av_lockmgr_register invokes calls
 *           to this callback and the previously registered callback.
 *           The callback will be used to create more than one mutex
 *           each of which must be backed by its own underlying locking
 *           mechanism (i.e. do not use a single static object to
 *           implement your lock manager). If cb is set to NULL the
 *           lockmgr will be unregistered.
 *)
// ToDo: Implement and test this
//function av_lockmgr_register(cb: function (mutex: Ppointer; op: TAVLockOp)): cint;
//  cdecl; external av__codec;

(**
 * Get the type of the given codec.
 *)
function avcodec_get_type(codec_id: TAVCodecID): TAVMediaType;
  cdecl; external av__codec;

(**
 * Get the name of a codec.
 * @return  a static string identifying the codec; never NULL
 *)
function avcodec_get_name(id: TAVCodecID): PAnsiChar;
  cdecl; external av__codec;

(**
 * @return a positive value if s is open (i.e. avcodec_open2() was called on it
 * with no corresponding avcodec_close()), 0 otherwise.
 *)
function avcodec_is_open(s: PAVCodecContext): cint;
  cdecl; external av__codec;

(**
 * @return a non-zero number if codec is an encoder, zero otherwise
 *)
function av_codec_is_encoder(codec: {const} PAVCodec): cint;
  cdecl; external av__codec;

(**
 * @return a non-zero number if codec is a decoder, zero otherwise
 *)
function av_codec_is_decoder(codec: {const} PAVCodec): cint;
  cdecl; external av__codec;

(**
 * @return descriptor for given codec ID or NULL if no descriptor exists.
 *)
function avcodec_descriptor_get(id: TAVCodecID): PAVCodecDescriptor;
  cdecl; external av__codec;

(**
 * Iterate over all codec descriptors known to libavcodec.
 *
 * @param prev previous descriptor. NULL to get the first descriptor.
 *
 * @return next descriptor or NULL after the last descriptor
 *)
function avcodec_descriptor_next(prev: {const} PAVCodecDescriptor): PAVCodecDescriptor;
  cdecl; external av__codec;

(**
 * @return codec descriptor with the given name or NULL if no such descriptor
 *         exists.
 *)
function avcodec_descriptor_get_by_name(name: {const} PAnsiChar): PAVCodecDescriptor;
  cdecl; external av__codec;

(**
 * @}
 *)

implementation

end.
