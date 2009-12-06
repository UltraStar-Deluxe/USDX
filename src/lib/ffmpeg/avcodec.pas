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

(*
 * This is a part of Pascal porting of ffmpeg.
 * - Originally by Victor Zinetz for Delphi and Free Pascal on Windows.
 * - For Mac OS X, some modifications were made by The Creative CAT, denoted as CAT
 *   in the source codes.
 * - Changes and updates by the UltraStar Deluxe Team
 *)

(*
 * Conversion of libavcodec/avcodec.h
 * Min. version: 51.16.0, revision 6577, Sat Oct 7 15:30:46 2006 UTC 
 * Max. version: 52.11.0, revision 16912, Sun Feb 1 02:00:19 2009 UTC 
 *)
{
 * update to
 * Max. version: 52.42.0, Sun Dec 6 19:20:00 2009 CET 
 * MiSchi
}

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
  ctypes,
  avutil,
  rational,
  opt,
  SysUtils,
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  UConfig;

const
  (* Max. supported version by this header *)
  LIBAVCODEC_MAX_VERSION_MAJOR   = 52;
  LIBAVCODEC_MAX_VERSION_MINOR   = 42;
  LIBAVCODEC_MAX_VERSION_RELEASE = 0;
  LIBAVCODEC_MAX_VERSION = (LIBAVCODEC_MAX_VERSION_MAJOR * VERSION_MAJOR) +
                           (LIBAVCODEC_MAX_VERSION_MINOR * VERSION_MINOR) +
                           (LIBAVCODEC_MAX_VERSION_RELEASE * VERSION_RELEASE);

  (* Min. supported version by this header *)
  LIBAVCODEC_MIN_VERSION_MAJOR   = 51;
  LIBAVCODEC_MIN_VERSION_MINOR   = 16;
  LIBAVCODEC_MIN_VERSION_RELEASE = 0;
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
  AV_NOPTS_VALUE: cint64  = $8000000000000000;
  AV_TIME_BASE            = 1000000;
  AV_TIME_BASE_Q: TAVRational = (num: 1; den: AV_TIME_BASE);

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
    CODEC_ID_CMV,
    CODEC_ID_MOTIONPIXELS,
    CODEC_ID_TGV,
    CODEC_ID_TGQ,
{$IF LIBAVCODEC_VERSION >= 52012000}  // >= 52.12.0
    CODEC_ID_TQI,
{$IFEND}
{$IF LIBAVCODEC_VERSION >= 52022002}  // >= 52.22.2
    CODEC_ID_AURA,
    CODEC_ID_AURA2,
{$IFEND}
{$IF LIBAVCODEC_VERSION >= 52027000}  // >= 52.27.0
    CODEC_ID_V210X,
{$IFEND}
{$IF LIBAVCODEC_VERSION >= 52028000}  // >= 52.28.0
    CODEC_ID_TMV,
{$IFEND}
{$IF LIBAVCODEC_VERSION >= 52029000}  // >= 52.29.0
    CODEC_ID_V210,
{$IFEND}
{$IF LIBAVCODEC_VERSION >= 52030002}  // >= 52.30.2
    CODEC_ID_DPX,
{$IFEND}
{$IF LIBAVCODEC_VERSION >= 52031002}  // >= 52.31.2
    CODEC_ID_MAD,
{$IFEND}
{$IF LIBAVCODEC_VERSION >= 52037000}  // >= 52.37.0
    CODEC_ID_FRWU,
{$IFEND}
{$IF LIBAVCODEC_VERSION >= 52041000}  // >= 52.41.0
    CODEC_ID_FLASHSV2,
{$IFEND}

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
    CODEC_ID_PCM_DVD,
    CODEC_ID_PCM_F32BE,
    CODEC_ID_PCM_F32LE,
    CODEC_ID_PCM_F64BE,
    CODEC_ID_PCM_F64LE,
{$IF LIBAVCODEC_VERSION >= 52034000} // >= 52.34.0
    CODEC_ID_PCM_BLURAY,
{$IFEND}

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
    CODEC_ID_ADPCM_IMA_ISS,

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
    {$IF LIBAVCODEC_VERSION < 52000000} // < 52.0.0
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
    CODEC_ID_ATRAC3P,
    CODEC_ID_EAC3,
    CODEC_ID_SIPR,
    CODEC_ID_MP1,
{$IF LIBAVCODEC_VERSION >= 52020000} // >= 52.20.0
    CODEC_ID_TWINVQ,
{$IFEND}
{$IF LIBAVCODEC_VERSION >= 52022000} // >= 52.22.0
    CODEC_ID_TRUEHD,
{$IFEND}
{$IF LIBAVCODEC_VERSION >= 52026000} // >= 52.26.0
    CODEC_ID_MP4ALS,
{$IFEND}
{$IF LIBAVCODEC_VERSION >= 52035000} // >= 52.35.0
    CODEC_ID_ATRAC1,
{$IFEND}

    //* subtitle codecs */
    CODEC_ID_DVD_SUBTITLE= $17000,
    CODEC_ID_DVB_SUBTITLE,
    CODEC_ID_TEXT,  ///< raw UTF-8 text
    CODEC_ID_XSUB,
    CODEC_ID_SSA,
    CODEC_ID_MOV_TEXT,
{$IF LIBAVCODEC_VERSION >= 52033000} // >= 52.33.0
    CODEC_ID_HDMV_PGS_SUBTITLE,
{$IFEND}
{$IF LIBAVCODEC_VERSION >= 52037001} // >= 52.37.1
    CODEC_ID_DVB_TELETEXT,
{$IFEND}

    (* other specific kind of codecs (generally used for attachments) *)
    CODEC_ID_TTF= $18000,

    CODEC_ID_PROBE= $19000, ///< codec_id is not known (like CODEC_ID_NONE) but lavf should attempt to identify it

    CODEC_ID_MPEG2TS= $20000, {*< _FAKE_ codec to indicate a raw MPEG-2 TS
                               * stream (only used by libavformat) *}
    __CODEC_ID_4BYTE = $FFFFF  // ensure 4-byte enum
  );

{$IF LIBAVCODEC_VERSION < 52000000} // < 52.0.0
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
 * all in native endian
 *}
type
  TSampleFormat = (
    SAMPLE_FMT_NONE = -1,
    SAMPLE_FMT_U8,              ///< unsigned 8 bits
    SAMPLE_FMT_S16,             ///< signed 16 bits
    SAMPLE_FMT_S32,             ///< signed 32 bits
    SAMPLE_FMT_FLT,             ///< float
    SAMPLE_FMT_DBL,             ///< double
    SAMPLE_FMT_NB               ///< Number of sample formats. DO NOT USE if dynamically linking to libavcodec
  );
  _TSampleFormatArray = array [0 .. MaxInt div SizeOf(TSampleFormat)-1] of TSampleFormat;
  PSampleFormatArray = ^_TSampleFormatArray;

const
  {* Audio channel masks *}
  CH_FRONT_LEFT             = $00000001;
  CH_FRONT_RIGHT            = $00000002;
  CH_FRONT_CENTER           = $00000004;
  CH_LOW_FREQUENCY          = $00000008;
  CH_BACK_LEFT              = $00000010;
  CH_BACK_RIGHT             = $00000020;
  CH_FRONT_LEFT_OF_CENTER   = $00000040;
  CH_FRONT_RIGHT_OF_CENTER  = $00000080;
  CH_BACK_CENTER            = $00000100;
  CH_SIDE_LEFT              = $00000200;
  CH_SIDE_RIGHT             = $00000400;
  CH_TOP_CENTER             = $00000800;
  CH_TOP_FRONT_LEFT         = $00001000;
  CH_TOP_FRONT_CENTER       = $00002000;
  CH_TOP_FRONT_RIGHT        = $00004000;
  CH_TOP_BACK_LEFT          = $00008000;
  CH_TOP_BACK_CENTER        = $00010000;
  CH_TOP_BACK_RIGHT         = $00020000;
  CH_STEREO_LEFT            = $20000000;  ///< Stereo downmix.
  CH_STEREO_RIGHT           = $40000000;  ///< See CH_STEREO_LEFT.
{** Channel mask value used for AVCodecContext.request_channel_layout
 *  to indicate that the user requests the channel order of the decoder output
 *  to be the native codec channel order.
 *}
{$IF LIBAVCODEC_VERSION >= 52038001} // >= 52.38.1
  CH_LAYOUT_NATIVE          = $8000000000000000LL
{$IFEND}
  {* Audio channel convenience macros *}
  CH_LAYOUT_MONO            = (CH_FRONT_CENTER);
  CH_LAYOUT_STEREO          = (CH_FRONT_LEFT or CH_FRONT_RIGHT);
  CH_LAYOUT_SURROUND        = (CH_LAYOUT_STEREO or CH_FRONT_CENTER);
{$IF LIBAVCODEC_VERSION >= 52027000} // >= 52.27.0
  CH_LAYOUT_2_1             = (CH_LAYOUT_STEREO or CH_BACK_CENTER);
  CH_LAYOUT_4POINT0         = (CH_LAYOUT_SURROUND or CH_BACK_CENTER);
  CH_LAYOUT_2_2             = (CH_LAYOUT_STEREO or CH_SIDE_LEFT or CH_SIDE_RIGHT);
{$IFEND}
  CH_LAYOUT_QUAD            = (CH_LAYOUT_STEREO or CH_BACK_LEFT or CH_BACK_RIGHT);
  CH_LAYOUT_5POINT0         = (CH_LAYOUT_SURROUND or CH_SIDE_LEFT or CH_SIDE_RIGHT);
  CH_LAYOUT_5POINT1         = (CH_LAYOUT_5POINT0 or CH_LOW_FREQUENCY);
{$IF LIBAVCODEC_VERSION >= 52025000} // >= 52.25.0
  CH_LAYOUT_5POINT0_BACK    = (CH_LAYOUT_SURROUND or CH_BACK_LEFT or 
                               CH_BACK_RIGHT);
  CH_LAYOUT_5POINT1_BACK    = (CH_LAYOUT_5POINT0_BACK or CH_LOW_FREQUENCY);
{$IFEND}
{$IF LIBAVCODEC_VERSION >= 52034000} // >= 52.34.0
  CH_LAYOUT_7POINT0         = (CH_LAYOUT_5POINT0 or CH_BACK_LEFT or CH_BACK_RIGHT);
{$IFEND}
  CH_LAYOUT_7POINT1         = (CH_LAYOUT_5POINT1 or CH_BACK_LEFT or CH_BACK_RIGHT);
{$IF LIBAVCODEC_VERSION < 52025000} // < 52.25.0
  CH_LAYOUT_7POINT1_WIDE    = (CH_LAYOUT_SURROUND or CH_LOW_FREQUENCY or
                               CH_BACK_LEFT or CH_BACK_RIGHT or
{$ELSE}
  CH_LAYOUT_7POINT1_WIDE    = (CH_LAYOUT_5POINT1_BACK or 
{$IFEND}
                               CH_FRONT_LEFT_OF_CENTER or
                               CH_FRONT_RIGHT_OF_CENTER);
  CH_LAYOUT_STEREO_DOWNMIX  = (CH_STEREO_LEFT or CH_STEREO_RIGHT);


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
    ME_ITER,      ///< iterative search
    ME_TESA       ///< transformed exhaustive search algorithm
  );

  TAVDiscard = (
    {* We leave some space between them for extensions (drop some
     * keyframes for intra-only or drop just some bidir frames).
     *}
    AVDISCARD_NONE    = -16, ///< discard nothing
    AVDISCARD_DEFAULT =   0, ///< discard useless packets like 0 size packets in avi
    AVDISCARD_NONREF  =   8, ///< discard all non reference
    AVDISCARD_BIDIR   =  16, ///< discard all bidirectional frames
    AVDISCARD_NONKEY  =  32, ///< discard all frames except keyframes
    AVDISCARD_ALL     =  48  ///< discard all
  );

{$IF LIBAVCODEC_VERSION >= 52028000} // >= 52.28.0
  TAVColorPrimaries = (
    AVCOL_PRI_BT709       = 1, ///< also ITU-R BT1361 / IEC 61966-2-4 / SMPTE RP177 Annex B
    AVCOL_PRI_UNSPECIFIED = 2,
    AVCOL_PRI_BT470M      = 4,
    AVCOL_PRI_BT470BG     = 5, ///< also ITU-R BT601-6 625 / ITU-R BT1358 625 / ITU-R BT1700 625 PAL & SECAM
    AVCOL_PRI_SMPTE170M   = 6, ///< also ITU-R BT601-6 525 / ITU-R BT1358 525 / ITU-R BT1700 NTSC
    AVCOL_PRI_SMPTE240M   = 7, ///< functionally identical to above
    AVCOL_PRI_FILM        = 8,
    AVCOL_PRI_NB               ///< Not part of ABI
  );

  TAVColorTransferCharacteristic = (
    AVCOL_TRC_BT709       = 1, ///< also ITU-R BT1361
    AVCOL_TRC_UNSPECIFIED = 2,
    AVCOL_TRC_GAMMA22     = 4, ///< also ITU-R BT470M / ITU-R BT1700 625 PAL & SECAM
    AVCOL_TRC_GAMMA28     = 5, ///< also ITU-R BT470BG
    AVCOL_TRC_NB               ///< Not part of ABI
  );

  TAVColorSpace = (
    AVCOL_SPC_RGB         = 0,
    AVCOL_SPC_BT709       = 1, ///< also ITU-R BT1361 / IEC 61966-2-4 xvYCC709 / SMPTE RP177 Annex B
    AVCOL_SPC_UNSPECIFIED = 2,
    AVCOL_SPC_FCC         = 4,
    AVCOL_SPC_BT470BG     = 5, ///< also ITU-R BT601-6 625 / ITU-R BT1358 625 / ITU-R BT1700 625 PAL & SECAM / IEC 61966-2-4 xvYCC601
    AVCOL_SPC_SMPTE170M   = 6, ///< also ITU-R BT601-6 525 / ITU-R BT1358 525 / ITU-R BT1700 NTSC / functionally identical to above
    AVCOL_SPC_SMPTE240M   = 7,
    AVCOL_SPC_NB               ///< Not part of ABI
  );

  TAVColorRange = (
    AVCOL_RANGE_UNSPECIFIED = 0,
    AVCOL_RANGE_MPEG        = 1, ///< the normal 219*2^(n-8) "MPEG" YUV ranges
    AVCOL_RANGE_JPEG        = 2, ///< the normal     2^n-1   "JPEG" YUV ranges
    AVCOL_RANGE_NB               ///< Not part of ABI
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
{$IFEND}

  PRcOverride = ^TRcOverride;
  TRcOverride = record {16}
    start_frame:    cint;
    end_frame:      cint;
    qscale:         cint; // if this is 0 then quality_factor will be used instead
    quality_factor: cfloat;
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
  {$IF LIBAVCODEC_VERSION < 52000000} // < 52.0.0
  CODEC_FLAG_TRELLIS_QUANT  = $00200000; ///< use trellis quantization
  {$IFEND}
  CODEC_FLAG_GLOBAL_HEADER  = $00400000; ///< place global headers in extradata instead of every keyframe
  CODEC_FLAG_BITEXACT       = $00800000; ///< use only bitexact stuff (except (i)dct)
  {* Fx : Flag for h263+ extra options *}
  {$IF LIBAVCODEC_VERSION < 52000000} // < 52.0.0
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

  (**
   * Codec can export data for HW decoding (VDPAU).
   *)
  CODEC_CAP_HWACCEL_VDPAU    = $0080;

  {$IF LIBAVCODEC_VERSION >= 52035000} // >= 52.35.0
  (**
   * Codec can output multiple frames per AVPacket
   *)
  CODEC_CAP_SUBFRAMES        = $0100;
  {$IFEND}

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
    id: cint;

    (*** width and height in 1/16 pel
     * - encoding: set by user.
     * - decoding: set by libavcodec. *)
    width: cint;
    height: cint;

    (*** position of the top left corner in 1/16 pel for up to 3 fields/frames.
     * - encoding: set by user.
     * - decoding: set by libavcodec. *)
    position: array [0..2] of array [0..1] of smallint;
  end;

const
  FF_QSCALE_TYPE_MPEG1  = 0;
  FF_QSCALE_TYPE_MPEG2  = 1;
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

const
  {$IF LIBAVCODEC_VERSION < 52000000} // < 52.0.0
  DEFAULT_FRAME_RATE_BASE = 1001000;
  {$IFEND}

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
  FF_IDCT_EA           = 21;
  FF_IDCT_SIMPLENEON   = 22;
  FF_IDCT_SIMPLEALPHA  = 23;

  FF_EC_GUESS_MVS   = 1;
  FF_EC_DEBLOCK     = 2;

  FF_MM_FORCE       = $80000000; (* force usage of selected flags (OR) *)
  (* lower 16 bits - CPU features *)
  FF_MM_MMX         = $0001; ///< standard MMX
  FF_MM_3DNOW       = $0004; ///< AMD 3DNOW
  {$IF LIBAVCODEC_MAX_VERSION_MAJOR < 53}
  FF_MM_MMXEXT      = $0002; ///< SSE integer functions or AMD MMX ext
  {$IFEND}
  {$IF LIBAVCODEC_VERSION >= 52024000} // >= 52.24.0
  FF_MM_MMX2        = $0002; ///< SSE integer functions or AMD MMX ext
  {$IFEND}
  FF_MM_SSE         = $0008; ///< SSE functions
  FF_MM_SSE2        = $0010; ///< PIV SSE2 functions
  FF_MM_3DNOWEXT    = $0020; ///< AMD 3DNowExt
  FF_MM_SSE3        = $0040; ///< Prescott SSE3 functions
  FF_MM_SSSE3       = $0080; ///< Conroe SSSE3 functions
  {$IF LIBAVCODEC_VERSION >= 52022003} // >= 52.22.3
  FF_MM_SSE4        = $0100; ///< Penryn SSE4.1 functions
  FF_MM_SSE42       = $0200; ///< Nehalem SSE4.2 functions
  {$IFEND}
  FF_MM_IWMMXT      = $0100; ///< XScale IWMMXT
  FF_MM_ALTIVEC     = $0001; ///< standard AltiVec

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
  FF_DEBUG_BUFFERS      = $00008000;

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

{$IF LIBAVCODEC_MAX_VERSION_MAJOR < 53}
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
    palette_changed: cint;

    (* 4-byte ARGB palette entries, stored in native byte order; note that
     * the individual palette components should be on a 8-bit scale; if
     * the palette data comes from a IBM VGA native format, the component
     * data is probably 6 bits in size and needs to be scaled *)
    palette: array [0..AVPALETTE_COUNT - 1] of cuint;
  end; {deprecated;}
{$IFEND}

{$IF LIBAVCODEC_VERSION >= 52023000} // >= 52.23.0
type
  PAVPacket = ^TAVPacket;
  TAVPacket = record
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
    flags:        cint;
(*
 * Duration of this packet in AVStream->time_base units, 0 if unknown.
 * Equals next_pts - this_pts in presentation order.
 *)
    duration:     cint;
    destruct:     procedure (para1: PAVPacket); cdecl;
    priv:         pointer;
    pos:          cint64;       // byte position in stream, -1 if unknown

(*
 * Time difference in AVStream->time_base units from the pts of this
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
  end;

const
  {$IF LIBAVCODEC_VERSION >= 52030002} // >= 52.30.2
  PKT_FLAG_KEY = $0001;
  {$ELSE}
  AV_PKT_FLAG_KEY = $0001;
    {$IF LIBAVCODEC_VERSION_MAJOR < 53}
  PKT_FLAG_KEY = AV_PKT_FLAG_KEY;
    {$IFEND}
  {$IFEND}
{$IFEND}

type
  PAVClass = ^TAVClass; {const}
  PAVCodecContext = ^TAVCodecContext;

  PAVCodec = ^TAVCodec;

{$IF LIBAVCODEC_VERSION >= 52018000} // >= 52.18.0
  PAVHWAccel = ^TAVHWAccel;
{$IFEND}

  // int[4]
  PQuadIntArray = ^TQuadIntArray;
  TQuadIntArray = array[0..3] of cint;
  // int (*func)(struct AVCodecContext *c2, void *arg)
  TExecuteFunc = function(c2: PAVCodecContext; arg: Pointer): cint; cdecl;

  TAVClass = record
    class_name: PAnsiChar;
    (* actually passing a pointer to an AVCodecContext
       or AVFormatContext, which begin with an AVClass.
       Needed because av_log is in libavcodec and has no visibility
       of AVIn/OutputFormat *)
    item_name: function(): PAnsiChar; cdecl;
    option: PAVOption;
  end;

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
    linesize: array [0..3] of cint;
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
    key_frame: cint;
    (**
     * Picture type of the frame, see ?_TYPE below.
     * - encoding: Set by libavcodec. for coded_picture (and set by user for input).
     * - decoding: Set by libavcodec.
     *)
    pict_type: cint;
    (**
     * presentation timestamp in time_base units (time when frame should be shown to user)
     * If AV_NOPTS_VALUE then frame_rate = 1/time_base will be assumed.
     * - encoding: MUST be set by user.
     * - decoding: Set by libavcodec.
     *)
    pts: cint64;
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
     * buffer age (1->was last buffer and dint change, 2->..., ...).
     * Set to INT_MAX if the buffer has not been used yet.
     * - encoding: unused
     * - decoding: MUST be set by get_buffer().
     *)
    age: cint;
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
    error: array [0..3] of cuint64;
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
     *
     *)
    qscale_type: cint;
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
    palette_has_changed: cint;
    (**
     * codec suggestion on buffer type if != 0
     * - encoding: unused
     * - decoding: Set by libavcodec. (before get_buffer() call)).
     *)
    buffer_hints: cint;
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

    {$IF LIBAVCODEC_VERSION >= 51068000} // >= 51.68.0
    (**
     * reordered opaque 64bit number (generally a PTS) from AVCodecContext.reordered_opaque
     * output in AVFrame.reordered_opaque
     * - encoding: unused
     * - decoding: Read by user.
     *)
    reordered_opaque: cint64;
    {$IFEND}
    
    {$IF LIBAVCODEC_VERSION = 52021000} // = 52.21.0
    (**
     * hardware accelerator private data (FFmpeg allocated)
     * - encoding: unused
     * - decoding: Set by libavcodec
     *)
    hwaccel_data_private: pointer;
    {$IFEND}
    {$IF LIBAVCODEC_VERSION >= 52022000} // >= 52.22.0
    hwaccel_picture_private: pointer;
    {$IFEND}

    {$IF LIBAVCODEC_VERSION >= 51070000} // >= 51.70.0
    (**
     * Bits per sample/pixel of internal libavcodec pixel/sample format.
     * This field is applicable only when sample_fmt is SAMPLE_FMT_S32.
     * - encoding: set by user.
     * - decoding: set by libavcodec.
     *)
    bits_per_raw_sample: cint;
    {$IFEND}

    {$IF LIBAVCODEC_VERSION >= 52002000} // >= 52.2.0
    (**
     * Audio channel layout.
     * - encoding: set by user.
     * - decoding: set by libavcodec.
     *)
    channel_layout: cint64;

    (**
     * Request decoder to use this channel layout if it can (0 for default)
     * - encoding: unused
     * - decoding: Set by user.
     *)
    request_channel_layout: cint64;
    {$IFEND}

    {$IF LIBAVCODEC_VERSION >= 52004000} // >= 52.4.0
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
    {$IFEND}
    {$IF LIBAVCODEC_VERSION >= 52018000} // >= 52.18.0
    (**
     * Hardware accelerator in use
     * - encoding: unused.
     * - decoding: Set by libavcodec
     *)
    hwaccel: PAVHWAccel;
    {$IFEND}
    {$IF LIBAVCODEC_VERSION >= 52020000} // >= 52.20.0
    (**
     * For some codecs, the time base is closer to the field rate than the frame rate.
     * Most notably, H.264 and MPEG-2 specify time_base as half of frame duration
     * if no telecine is used ...
     *
     * Set to time_base ticks per frame. Default 1, e.g., H.264/MPEG-2 set it to 2.
     *)
    ticks_per_frame: cint;
    {$IFEND}
    {$IF LIBAVCODEC_VERSION >= 52021000} // >= 52.21.0
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
    {$IFEND}
    {$IF LIBAVCODEC_VERSION >= 52028000} // >= 52.28.0
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
    {$IFEND}
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
    bit_rate: cint;

    (**
     * number of bits the bitstream is allowed to diverge from the reference.
     *           the reference can be CBR (for CBR pass1) or VBR (for pass2)
     * - encoding: Set by user; unused for constant quantizer encoding.
     * - decoding: unused
     *)
    bit_rate_tolerance: cint;

    (**
     * CODEC_FLAG_*.
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    flags: cint;

    (**
     * Some codecs need additional format info. It is stored here.
     * If any muxer uses this then ALL demuxers/parsers AND encoders for the
     * specific codec MUST set it correctly otherwise stream copy breaks.
     * In general use of this field by muxers is not recommanded.
     * - encoding: Set by libavcodec.
     * - decoding: Set by libavcodec. (FIXME: Is this OK?)
     *)
    sub_id: cint;

    (**
     * Motion estimation algorithm used for video coding.
     * 1 (zero), 2 (full), 3 (log), 4 (phods), 5 (epzs), 6 (x1), 7 (hex),
     * 8 (umh), 9 (iter), 10 (tesa) [7, 8, 10 are x264 specific, 9 is snow specific]
     * - encoding: MUST be set by user.
     * - decoding: unused
     *)
    me_method: cint;

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
    extradata_size: cint;

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
    width, height: cint;

    (**
     * the number of pictures in a group of pictures, or 0 for intra_only
     * - encoding: Set by user.
     * - decoding: unused
     *)
    gop_size: cint;

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
    rate_emu: cint;

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
                                src: {const} PAVFrame; offset: PQuadIntArray;
                                y: cint; type_: cint; height: cint); cdecl;

    (* audio only *)
    sample_rate: cint; ///< samples per second
    channels: cint;    ///< number of audio channels
    
    (**
     * audio sample format
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    sample_fmt: TSampleFormat;  ///< sample format

    (* The following data should not be initialized. *)
    (**
     * Samples per packet, initialized when calling 'init'.
     *)
    frame_size: cint;
    frame_number: cint;   ///< audio or video frame number
{$IF LIBAVCODEC_MAX_VERSION_MAJOR < 53}
    real_pict_num: cint;  ///< returns the real picture number of previous encoded frame
{$IFEND}

    (**
     * Number of frames the decoded output will be delayed relative to
     * the encoded input.
     * - encoding: Set by libavcodec.
     * - decoding: unused
     *)
    delay: cint;

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
     * hurry up amount
     * - encoding: unused
     * - decoding: Set by user. 1-> Skip B-frames, 2-> Skip IDCT/dequant too, 5-> Skip everything except header
     * @deprecated Deprecated in favor of skip_idct and skip_frame.
     *)
    hurry_up: cint;

    codec: PAVCodec;

    priv_data: pointer;

    {$IF LIBAVCODEC_VERSION < 52000000} // 52.0.0
    (* unused, FIXME remove*)
    rtp_mode: cint;
    {$IFEND}

    rtp_payload_size: cint;  (* The size of the RTP payload: the coder will  *)
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
                             size: cint; mb_nb: cint); cdecl;

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
     * Private data of the user, can be used to carry app specific stuff.
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    opaque: pointer;

    codec_name: array [0..31] of AnsiChar;
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
    codec_tag: cuint;

    (**
     * Work around bugs in encoders which sometimes cannot be detected automatically.
     * - encoding: Set by user
     * - decoding: Set by user
     *)
    workaround_bugs: cint;

    (**
     * luma single coefficient elimination threshold
     * - encoding: Set by user.
     * - decoding: unused
     *)
    luma_elim_threshold: cint;

    (**
     * chroma single coeff elimination threshold
     * - encoding: Set by user.
     * - decoding: unused
     *)
    chroma_elim_threshold: cint;

    (**
     * strictly follow the standard (MPEG4, ...).
     * - encoding: Set by user.
     * - decoding: Set by user.
     * Setting this to STRICT or higher means the encoder and decoder will
     * generally do stupid things. While setting it to inofficial or lower
     * will mean the encoder might use things that are not supported by all
     * spec compliant decoders. Decoders make no difference between normal,
     * inofficial and experimental, that is they always try to decode things
     * when they can unless they are explicitly asked to behave stupid
     * (=strictly conform to the specs)
     *)
    strict_std_compliance: cint;

    (**
     * qscale offset between IP and B-frames
     * - encoding: Set by user.
     * - decoding: unused
     *)
    b_quant_offset: cfloat;

    (**
     * Error recognization; higher values will detect more errors but may
     * misdetect some more or less valid parts as errors.
     * - encoding: unused
     * - decoding: Set by user.
     *)
    error_recognition: cint;

    (**
     * Called at the beginning of each frame to get a buffer for it.
     * If pic.reference is set then the frame will be read later by libavcodec.
     * avcodec_align_dimensions() should be used to find the required width and
     * height, as they normally need to be rounded up to the next multiple of 16.
     * if CODEC_CAP_DR1 is not set then get_buffer() must call
     * avcodec_default_get_buffer() instead of providing buffers allocated by
     * some other means.
     * - encoding: unused
     * - decoding: Set by libavcodec., user can override.
     *)
    get_buffer: function (c: PAVCodecContext; pic: PAVFrame): cint; cdecl;

    (**
     * Called to release buffers which were allocated with get_buffer.
     * A released buffer can be reused in get_buffer().
     * pic.data[*] must be set to NULL.
     * - encoding: unused
     * - decoding: Set by libavcodec., user can override.
     *)
    release_buffer: procedure (c: PAVCodecContext; pic: PAVFrame); cdecl;

    (**
     * Size of the frame reordering buffer in the decoder.
     * For MPEG-2 it is 1 IPB or 0 low delay IP.
     * - encoding: Set by libavcodec.
     * - decoding: Set by libavcodec.
     *)
    has_b_frames: cint;

    (**
     * number of bytes per packet if constant and known or 0
     * Used by some WAV based audio codecs.
     *)
    block_align: cint;

    parse_only: cint; (* - decoding only: if true, only parsing is done
                       (function avcodec_parse_frame()). The frame
                       data is returned. Only MPEG codecs support this now. *)

    (**
     * 0-> h263 quant 1-> mpeg quant
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mpeg_quant: cint;

    (**
     * pass1 encoding statistics output buffer
     * - encoding: Set by libavcodec.
     * - decoding: unused
     *)
    stats_out: PByteArray;

    (**
     * pass2 encoding statistics input buffer
     * Concatenated stuff from stats_out of pass1 should be placed here.
     * - encoding: Allocated/set/freed by user.
     * - decoding: unused
     *)
    stats_in: PByteArray;

    (**
     * ratecontrol qmin qmax limiting method
     * 0-> clipping, 1-> use a nice continous function to limit qscale wthin qmin/qmax.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    rc_qsquish: cfloat;

    rc_qmod_amp: cfloat;
    rc_qmod_freq: cint;

    (**
     * ratecontrol override, see RcOverride
     * - encoding: Allocated/set/freed by user.
     * - decoding: unused
     *)
    rc_override: PRcOverride;
    rc_override_count: cint;

    (**
     * rate control equation
     * - encoding: Set by user
     * - decoding: unused
     *)
    rc_eq: {const} PByteArray; 

    (**
     * maximum bitrate
     * - encoding: Set by user.
     * - decoding: unused
     *)
    rc_max_rate: cint;

    (**
     * minimum bitrate
     * - encoding: Set by user.
     * - decoding: unused
     *)
    rc_min_rate: cint;

    (**
     * decoder bitstream buffer size
     * - encoding: Set by user.
     * - decoding: unused
     *)
    rc_buffer_size: cint;
    rc_buffer_aggressivity: cfloat;

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
     * initial complexity for pass1 ratecontrol
     * - encoding: Set by user.
     * - decoding: unused
     *)
    rc_initial_cplx: cfloat;

    (**
     * DCT algorithm, see FF_DCT_* below
     * - encoding: Set by user.
     * - decoding: unused
     *)
    dct_algo: cint;

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

    {$IF LIBAVCODEC_VERSION < 52000000} // 52.0.0
    (* for binary compatibility *)
    unused: cint;
    {$IFEND}

    (**
     * IDCT algorithm, see FF_IDCT_* below.
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    idct_algo: cint;

    (**
     * slice count
     * - encoding: Set by libavcodec.
     * - decoding: Set by user (or 0).
     *)
    slice_count: cint;

    (**
     * slice offsets in the frame in bytes
     * - encoding: Set/allocated by libavcodec.
     * - decoding: Set/allocated by user (or NULL).
     *)
    slice_offset: PCint;

    (**
     * error concealment flags
     * - encoding: unused
     * - decoding: Set by user.
     *)
    error_concealment: cint;

    (**
     * dsp_mask could be add used to disable unwanted CPU features
     * CPU features (i.e. MMX, SSE. ...)
     *
     * With the FORCE flag you may instead enable given CPU features.
     * (Dangerous: Usable in case of misdetection, improper usage however will
     * result into program crash.)
     *)
    dsp_mask: cuint;

    (**
     * bits per sample/pixel from the demuxer (needed for huffyuv).
     * - encoding: Set by libavcodec.
     * - decoding: Set by user.
     *)
     bits_per_coded_sample: cint;

    (**
     * prediction method (needed for huffyuv)
     * - encoding: Set by user.
     * - decoding: unused
     *)
     prediction_method: cint;

    (**
     * sample aspect ratio (0 if unknown)
     * That is the width of a pixel divided by the height of the pixel.
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
    debug: cint;

    (**
     * debug
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    debug_mv: cint;

    (**
     * error
     * - encoding: Set by libavcodec if flags&CODEC_FLAG_PSNR.
     * - decoding: unused
     *)
    error: array [0..3] of cuint64;

    (**
     * minimum MB quantizer
     * - encoding: unused
     * - decoding: unused
     *)
    mb_qmin: cint;

    (**
     * maximum MB quantizer
     * - encoding: unused
     * - decoding: unused
     *)
    mb_qmax: cint;

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

    (**
     * callback to negotiate the pixelFormat
     * @param fmt is the list of formats which are supported by the codec,
     * it is terminated by -1 as 0 is a valid format, the formats are ordered by quality.
     * The first is always the native one.
     * @return the chosen format
     * - encoding: unused
     * - decoding: Set by user, if not set the native format will be chosen.
     *)
    get_format: function (s: PAVCodecContext; fmt: {const} PAVPixelFormat): TAVPixelFormat; cdecl;

    (**
     * DTG active format information (additional aspect ratio
     * information only used in DVB MPEG-2 transport streams)
     * 0 if not set.
     *
     * - encoding: unused
     * - decoding: Set by decoder.
     *)
    dtg_active_format: cint;

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
     * color table ID
     * - encoding: unused
     * - decoding: Which clrtable should be used for 8bit RGB images.
     *             Tables have to be stored somewhere. FIXME
     *)
    color_table_id: cint;

    (**
     * internal_buffer count
     * Don't touch, used by libavcodec default_get_buffer().
     *)
    internal_buffer_count: cint;

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
    global_quality: cint;

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

    {
    (**
     *
     * - encoding: unused
     * - decoding: Set by user.
     *)
    realloc: function (s: PAVCodecContext; buf: Pbyte; buf_size: cint): Pbyte; cdecl;
    }

    (**
     * slice flags
     * - encoding: unused
     * - decoding: Set by user.
     *)
    slice_flags: cint;

    (**
     * XVideo Motion Acceleration
     * - encoding: forbidden
     * - decoding: set by decoder
     *)
    xvmc_acceleration: cint;

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
     * fourcc from the AVI stream header (LSB first, so "ABCD" -> ('D'<<24) + ('C'<<16) + ('B'<<8) + 'A').
     * This is used to work around some encoder bugs.
     * - encoding: unused
     * - decoding: Set by user, will be converted to uppercase by libavcodec during init.
     *)
    stream_codec_tag: array [0..3] of AnsiChar; //cuint;

    (**
     * scene change detection threshold
     * 0 is default, larger means fewer detected scene changes.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    scenechange_threshold: cint;

    (**
     * minimum Lagrange multipler
     * - encoding: Set by user.
     * - decoding: unused
     *)
    lmin: cint;

    (**
     * maximum Lagrange multipler
     * - encoding: Set by user.
     * - decoding: unused
     *)
    lmax: cint;

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
    noise_reduction: cint;

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
     * - decoding: Set by libavcodec., user can override
     *)
    reget_buffer: function (c: PAVCodecContext; pic: PAVFrame): cint; cdecl;

    (**
     * Number of bits which should be loaded into the rc buffer before decoding starts.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    rc_initial_buffer_occupancy: cint;

    (**
     *
     * - encoding: Set by user.
     * - decoding: unused
     *)
    inter_threshold: cint;

    (**
     * CODEC_FLAG2_*
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    flags2: cint;

    (**
     * Simulates errors in the bitstream to test error concealment.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    error_rate: cint;

    (**
     * MP3 antialias algorithm, see FF_AA_* below.
     * - encoding: unused
     * - decoding: Set by user.
     *)
    antialias_algo: cint;

    (**
     * quantizer noise shaping
     * - encoding: Set by user.
     * - decoding: unused
     *)
    quantizer_noise_shaping: cint;

    (**
     * thread count
     * is used to decide how many independent tasks should be passed to execute()
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    thread_count: cint;

    (**
     * The codec may call this to execute several independent things.
     * It will return only after finishing all tasks.
     * The user may replace this with some multithreaded implementation,
     * the default implementation will execute the parts serially.
     * @param count the number of things to execute
     * - encoding: Set by libavcodec, user can override.
     * - decoding: Set by libavcodec, user can override.
     *)
    {$IF LIBAVCODEC_VERSION < 52004000} // < 52.4.0
    execute: function (c: PAVCodecContext; func: TExecuteFunc; arg: PPointer; ret: PCint; count: cint): cint; cdecl;
    {$ELSE}
    execute: function (c: PAVCodecContext; func: TExecuteFunc; arg: Pointer; ret: PCint; count: cint; size: cint): cint; cdecl;
    {$IFEND}

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
     me_threshold: cint;

    (**
     * Macroblock threshold below which the user specified macroblock types will be used.
     * - encoding: Set by user.
     * - decoding: unused
     *)
     mb_threshold: cint;

    (**
     * precision of the intra DC coefficient - 8
     * - encoding: Set by user.
     * - decoding: unused
     *)
     intra_dc_precision: cint;

    (**
     * noise vs. sse weight for the nsse comparsion function
     * - encoding: Set by user.
     * - decoding: unused
     *)
     nsse_weight: cint;

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
     * low resolution decoding, 1-> 1/2 size, 2->1/4 size
     * - encoding: unused
     * - decoding: Set by user.
     *)
     lowres: cint;

    (**
     * Bitstream width / height, may be different from width/height if lowres
     * or other things are used.
     * - encoding: unused
     * - decoding: Set by user before init if known. Codec should override / dynamically change if needed.
     *)
    coded_width, coded_height: cint;

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
     * Border processing masking, raises the quantizer for mbs on the borders
     * of the picture.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    border_masking: cfloat;

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
    bidir_refine: cint;

    (**
     *
     * - encoding: Set by user.
     * - decoding: unused
     *)
    brd_scale: cint;

    (**
     * constant rate factor - quality-based VBR - values ~correspond to qps
     * - encoding: Set by user.
     * - decoding: unused
     *)
    {$IF LIBAVCODEC_VERSION >= 51021000} // 51.21.0
    crf: cfloat;
    {$ELSE}
    crf: cint;
    {$IFEND}

    (**
     * constant quantization parameter rate control method
     * - encoding: Set by user.
     * - decoding: unused
     *)
    cqp: cint;

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

    (**
     * Influences how often B-frames are used.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    bframebias: cint;

    (**
     * trellis RD quantization
     * - encoding: Set by user.
     * - decoding: unused
     *)
    trellis: cint;

    (**
     * Reduce fluctuations in qp (before curve compression).
     * - encoding: Set by user.
     * - decoding: unused
     *)
    complexityblur: cfloat;

    (**
     * in-loop deblocking filter alphac0 parameter
     * alpha is in the range -6...6
     * - encoding: Set by user.
     * - decoding: unused
     *)
    deblockalpha: cint;

    (**
     * in-loop deblocking filter beta parameter
     * beta is in the range -6...6
     * - encoding: Set by user.
     * - decoding: unused
     *)
    deblockbeta: cint;

    (**
     * macroblock subpartition sizes to consider - p8x8, p4x4, b8x8, i8x8, i4x4
     * - encoding: Set by user.
     * - decoding: unused
     *)
    partitions: cint;

    (**
     * direct MV prediction mode - 0 (none), 1 (spatial), 2 (temporal), 3 (auto)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    directpred: cint;

    (**
     * Audio cutoff bandwidth (0 means "automatic")
     * - encoding: Set by user.
     * - decoding: unused
     *)
    cutoff: cint;

    (**
     * Multiplied by qscale for each frame and added to scene_change_score.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    scenechange_factor: cint;

    (**
     *
     * Note: Value depends upon the compare function used for fullpel ME.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mv0_threshold: cint;

    (**
     * Adjusts sensitivity of b_frame_strategy 1.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    b_sensitivity: cint;

    (**
     * - encoding: Set by user.
     * - decoding: unused
     *)
    compression_level: cint;

    (**
     * Sets whether to use LPC mode - used by FLAC encoder.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    use_lpc: cint;

    (**
     * LPC coefficient precision - used by FLAC encoder
     * - encoding: Set by user.
     * - decoding: unused
     *)
    lpc_coeff_precision: cint;

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
     * search method for selecting prediction order
     * - encoding: Set by user.
     * - decoding: unused
     *)
    prediction_order_method: cint;

    (**
     * - encoding: Set by user.
     * - decoding: unused
     *)
    min_partition_order: cint;

    (**
     * - encoding: Set by user.
     * - decoding: unused
     *)
    max_partition_order: cint;

    {$IF LIBAVCODEC_VERSION >= 51026000} // 51.26.0
    (**
     * GOP timecode frame start number, in non drop frame format
     * - encoding: Set by user.
     * - decoding: unused
     *)
    timecode_frame_start: cint64;
    {$IFEND}

    {$IF LIBAVCODEC_VERSION >= 51042000} // 51.42.0
    {$IF LIBAVCODEC_MAX_VERSION_MAJOR < 53}
    (**
     * Decoder should decode to this many channels if it can (0 for default)
     * - encoding: unused
     * - decoding: Set by user.
     * @deprecated Deprecated in favor of request_channel_layout.
     *)
    request_channels: cint;
    {$IFEND}
    {$IFEND}

    {$IF LIBAVCODEC_VERSION > 51049000} // > 51.49.0
    (**
     * Percentage of dynamic range compression to be applied by the decoder.
     * The default value is 1.0, corresponding to full compression.
     * - encoding: unused
     * - decoding: Set by user.
     *)
    drc_scale: cfloat;
    {$IFEND}

    {$IF LIBAVCODEC_VERSION >= 51068000} // 51.68.0
    (**
     * opaque 64bit number (generally a PTS) that will be reordered and
     * output in AVFrame.reordered_opaque
     * - encoding: unused
     * - decoding: Set by user.
     *)
    reordered_opaque: cint64;
    {$IFEND}
    
    {$IF LIBAVCODEC_VERSION >= 52028000} // 52.28.0
    (**
     * This defines the location of chroma samples.
     * - encoding: Set by user
     * - decoding: Set by libavcodec
     *)
     chroma_sample_location: TAVChromaLocation;
    {$IFEND}
    {$IF LIBAVCODEC_VERSION >= 52037000} // >= 52.37.0
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
    execute2: function (c: PAVCodecContext; func: function (c2: PAVCodecContext; arg: Pointer; jobnr: cint; threadnr: cint): cint; cdecl; arg2: Pointer; ret: Pcint; count: cint): cint; cdecl;
    {$IFEND}
    {$IF LIBAVCODEC_VERSION >= 52042000} // >= 52.42.0
    (**
     * explicit P-frame weighted prediction analysis method
     * 0: off
     * 1: fast blind weighting (one reference duplicate with -1 offset)
     * 2: smart weighting (full fade detection analysis)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    weighted_p_pred: cint;
    {$IFEND}
  end;

(**
 * AVCodec.
 *)
  TAVCodec = record
    name: PAnsiChar;
    type_: TCodecType;
    id: TCodecID;
    priv_data_size: cint;
    init: function (avctx: PAVCodecContext): cint; cdecl; (* typo corretion by the Creative CAT *)
    encode: function (avctx: PAVCodecContext; buf: PByteArray; buf_size: cint; data: pointer): cint; cdecl;
    close: function (avctx: PAVCodecContext): cint; cdecl;
    decode: function (avctx: PAVCodecContext; outdata: pointer; var outdata_size: cint;
    {$IF LIBAVCODEC_VERSION < 52025000} // 52.25.0
                      buf: {const} PByteArray; buf_size: cint): cint; cdecl;
    {$ELSE}
                      avpkt: PAVPacket): cint; cdecl;
    {$IFEND}
    (**
     * Codec capabilities.
     * see CODEC_CAP_*
     *)
    capabilities: cint;
    next: PAVCodec;
    (**
     * Flush buffers.
     * Will be called when seeking
     *)
    flush: procedure (avctx: PAVCodecContext); cdecl;
    supported_framerates: {const} PAVRational; ///< array of supported framerates, or NULL if any, array is terminated by {0,0}
    pix_fmts: {const} PAVPixelFormat;       ///< array of supported pixel formats, or NULL if unknown, array is terminated by -1
    {$IF LIBAVCODEC_VERSION >= 51055000} // 51.55.0
    (**
     * Descriptive name for the codec, meant to be more human readable than name.
     * You should use the NULL_IF_CONFIG_SMALL() macro to define it.
     *)
    long_name: {const} PAnsiChar;
    {$IFEND}
    {$IF LIBAVCODEC_VERSION >= 51056000} // 51.56.0
    supported_samplerates: {const} PCint;       ///< array of supported audio samplerates, or NULL if unknown, array is terminated by 0
    {$IFEND}
    {$IF LIBAVCODEC_VERSION >= 51062000} // 51.62.0
    sample_fmts: {const} PSampleFormatArray;   ///< array of supported sample formats, or NULL if unknown, array is terminated by -1
    {$IFEND}
    {$IF LIBAVCODEC_VERSION >= 52002000} // 52.2.0
    channel_layouts: {const} PCint64;         ///< array of support channel layouts, or NULL if unknown. array is terminated by 0
    {$IFEND}
  end;

{$IF LIBAVCODEC_VERSION >= 52018000} // 52.18.0
(**
 * AVHWAccel.
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
     * See CODEC_TYPE_xxx
     *)
    type_: TCodecType;

    (**
     * Codec implemented by the hardware accelerator.
     *
     * See CODEC_ID_xxx
     *)
    id: TCodecID;

    (**
     * Supported pixel format.
     *
     * Only hardware accelerated formats are supported here.
     *)
    pix_fmt: {const} PAVPixelFormat; 

    (**
     * Hardware accelerated codec capabilities.
     * see FF_HWACCEL_CODEC_CAP_*
     *)
    capabilities: cint;

    next: PAVCodec;

    (**
     * Called at the beginning of each frame or field picture.
     *
     * Meaningful frame information (codec specific) is guaranteed to
     * be parsed at this point. This function is mandatory.
     *
     * Note that buf can be NULL along with buf_size set to 0.
     * Otherwise, this means the whole frame is available at this point.
     *
     * @param avctx the codec context
     * @param buf the frame data buffer base
     * @param buf_size the size of the frame in bytes
     * @return zero if successful, a negative value otherwise
     *)
    start_frame: function (avctx:    PAVCodecContext; 
                           buf:      PByteArray; 
                           buf_size: cint): cint; cdecl;

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
    decode_slice: function (avctx:    PAVCodecContext;
                            buf:      PByteArray; 
                            buf_size: cint): cint; cdecl;

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
        
{$IF LIBAVCODEC_VERSION >= 52021000} // >= 52.21.0
    (**
     * Size of HW accelerator private data.
     *
     * Private data is allocated with av_mallocz() before
     * AVCodecContext.get_buffer() and deallocated after
     * AVCodecContext.release_buffer().
     *)
    priv_data_size: cint;
{$IFEND}
        
  end;
{$IFEND}

(**
 * four components are given, that's all.
 * the last component is alpha
 *)
  PAVPicture = ^TAVPicture;
  TAVPicture = record
    data: array [0..3] of PByteArray;
    linesize: array [0..3] of cint;       ///< number of bytes per line
  end;

type
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
  );

type
  PPAVSubtitleRect = ^PAVSubtitleRect;
  PAVSubtitleRect = ^TAVSubtitleRect;
  {$IF LIBAVCODEC_VERSION < 52010000} // < 52.10.0
  TAVSubtitleRect = record
    x: cuint16;
    y: cuint16;
    w: cuint16;
    h: cuint16;
    nb_colors: cuint16;
    linesize: cint;
    rgba_palette: PCuint32;
    bitmap: PCuint8;
  end;
  {$ELSE}
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
     * The pressentation of this is unaffected by the other values in this
     * struct.
     *)
    ass: PByteArray;
  end;
  {$IFEND}

  PPAVSubtitle = ^PAVSubtitle;
  PAVSubtitle = ^TAVSubtitle;
  TAVSubtitle = record
    format: cuint16; (* 0 = graphics *)
    start_display_time: cuint32; (* relative to packet pts, in ms *)
    end_display_time: cuint32; (* relative to packet pts, in ms *)
    num_rects: cuint;
    {$IF LIBAVCODEC_VERSION < 52010000} // < 52.10.0
    rects: PAVSubtitleRect;
    {$ELSE}
    rects: PPAVSubtitleRect;
    {$IFEND}
    {$IF LIBAVCODEC_VERSION >= 52032000} // >= 52.32.0
    pts: cint64;     ///< Same as packet pts, in AV_TIME_BASE
    {$IFEND}
  end;

{$IF LIBAVCODEC_VERSION >= 52025000} // 52.25.0
{ packet functions  }

(**
 * @deprecated use NULL instead
 *)
procedure av_destruct_packet_nofree(pkt: PAVPacket);
  cdecl; external av__codec;

(*
 * Default packet destructor.
 *)
procedure av_destruct_packet(pkt: PAVPacket);
  cdecl; external av__codec;

(*
 * Initialize optional fields of a packet with default values.
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

(*
 * @warning This is a hack - the packet memory allocation stuff is broken. The
 * packet is allocated if it was not really allocated.
 *)
function av_dup_packet(pkt: PAVPacket): cint;
  cdecl; external av__codec;

(*
 * Free a packet.
 *
 * @param pkt packet to free
 *)
procedure av_free_packet(pkt: PAVPacket);
{$IF LIBAVCODEC_VERSION >= 52028000} // 52.28.0
  cdecl; external av__codec;
{$IFEND}
{$IFEND}

(* resample.c *)
type
  PReSampleContext = pointer;
  PAVResampleContext = pointer;
  PImgReSampleContext = pointer;

function audio_resample_init (output_channels: cint; input_channels: cint;
                              output_rate: cint; input_rate: cint): PReSampleContext;
  cdecl; external av__codec;

function audio_resample (s: PReSampleContext; output: PSmallint; input: PSmallint; nb_samples: cint): cint;
  cdecl; external av__codec;

procedure audio_resample_close (s: PReSampleContext);
  cdecl; external av__codec;

(**
 * Initializes an audio resampler.
 * Note, if either rate is not an integer then simply scale both rates up so they are.
 * @param filter_length length of each FIR filter in the filterbank relative to the cutoff freq
 * @param log2_phase_count log2 of the number of entries in the polyphase filterbank
 * @param linear If 1 then the used FIR filter will be linearly interpolated
                 between the 2 closest, if 0 the closest will be used
 * @param cutoff cutoff frequency, 1.0 corresponds to half the output sampling rate
 *)
function av_resample_init (out_rate: cint; in_rate: cint; filter_length: cint;
                           log2_phase_count: cint; linear: cint; cutoff: cdouble): PAVResampleContext;
  cdecl; external av__codec;

(**
 * resamples.
 * @param src an array of unconsumed samples
 * @param consumed the number of samples of src which have been consumed are returned here
 * @param src_size the number of unconsumed samples available
 * @param dst_size the amount of space in samples available in dst
 * @param update_ctx If this is 0 then the context will not be modified, that way several channels can be resampled with the same context.
 * @return the number of samples written in dst or -1 if an error occurred
 *)
function av_resample (c: PAVResampleContext; dst: PSmallint; src: PSmallint; var consumed: cint;
                      src_size: cint; dst_size: cint; update_ctx: cint): cint;
  cdecl; external av__codec;

(**
 * Compensates samplerate/timestamp drift. The compensation is done by changing
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
  cdecl; external av__codec;

procedure av_resample_close (c: PAVResampleContext);
  cdecl; external av__codec;

{$IF LIBAVCODEC_VERSION < 52000000} // 52.0.0
(* YUV420 format is assumed ! *)

(**
 * @deprecated Use the software scaler (swscale) instead.
 *)
function img_resample_init (output_width: cint; output_height: cint;
                           input_width: cint; input_height: cint): PImgReSampleContext;
  cdecl; external av__codec; deprecated;

(**
 * @deprecated Use the software scaler (swscale) instead.
 *)
function img_resample_full_init (owidth: cint; oheight: cint;
                                    iwidth: cint; iheight: cint;
                                    topBand: cint; bottomBand: cint;
                                    leftBand: cint; rightBand: cint;
                                    padtop: cint; padbottom: cint;
                                    padleft: cint; padright: cint): PImgReSampleContext;
  cdecl; external av__codec; deprecated;

(**
 * @deprecated Use the software scaler (swscale) instead.
 *)
procedure img_resample (s: PImgReSampleContext; output: PAVPicture; input: {const} PAVPicture);
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
 * Fill in the AVPicture fields.
 * The fields of the given AVPicture are filled in by using the 'ptr' address
 * which points to the image data buffer. Depending on the specified picture
 * format, one or multiple image data pointers and line sizes will be set.
 * If a planar format is specified, several pointers will be set pointing to
 * the different picture planes and the line sizes of the different planes
 * will be stored in the lines_sizes array.
 * Call with ptr == NULL to get the required size for the ptr buffer.
 *
 * @param picture AVPicture whose fields are to be filled in
 * @param ptr Buffer which will contain or contains the actual image data
 * @param pix_fmt The format in which the picture data is stored.
 * @param width the width of the image in pixels
 * @param height the height of the image in pixels
 * @return size of the image data in bytes
 *)
function avpicture_fill (picture: PAVPicture; ptr: pointer;
                 pix_fmt: TAVPixelFormat; width: cint; height: cint): cint;
  cdecl; external av__codec;

function avpicture_layout (src: {const} PAVPicture; pix_fmt: TAVPixelFormat;
                   width: cint; height: cint;
                   dest: PByteArray; dest_size: cint): cint;
  cdecl; external av__codec;

(**
 * Calculate the size in bytes that a picture of the given width and height
 * would occupy if stored in the given picture format.
 * Note that this returns the size of a compact representation as generated
 * by avpicture_layout, which can be smaller than the size required for e.g.
 * avpicture_fill.
 *
 * @param pix_fmt the given picture format
 * @param width the width of the image
 * @param height the height of the image
 * @return Image data size in bytes or -1 on error (e.g. too large dimensions).
 *)
function avpicture_get_size (pix_fmt: TAVPixelFormat; width: cint; height: cint): cint;
  cdecl; external av__codec;

procedure avcodec_get_chroma_sub_sample (pix_fmt: TAVPixelFormat; var h_shift: cint; var v_shift: cint);
  cdecl; external av__codec;

(**
 * Returns the pixel format corresponding to the name \p name.
 *
 * If there is no pixel format with name \p name, then looks for a
 * pixel format with the name corresponding to the native endian
 * format of \p name.
 * For example in a little-endian system, first looks for "gray16",
 * then for "gray16le".
 *
 * Finally if no pixel format has been found, returns \c PIX_FMT_NONE.
 *)
function avcodec_get_pix_fmt_name(pix_fmt: TAVPixelFormat): PAnsiChar;
  cdecl; external av__codec;

procedure avcodec_set_dimensions(s: PAVCodecContext; width: cint; height: cint);
  cdecl; external av__codec;

(**
 * Returns the pixel format corresponding to the name name.
 *
 * If there is no pixel format with name name, then looks for a
 * pixel format with the name corresponding to the native endian
 * format of name.
 * For example in a little-endian system, first looks for "gray16",
 * then for "gray16le".
 *
 * Finally if no pixel format has been found, returns PIX_FMT_NONE.
 *)
function avcodec_get_pix_fmt(name: {const} PAnsiChar): TAVPixelFormat;
  cdecl; external av__codec;

function avcodec_pix_fmt_to_codec_tag(p: TAVPixelFormat): cuint;
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
                           has_alpha: cint): cint;
  cdecl; external av__codec;

(**
 * Finds the best pixel format to convert to given a certain source pixel
 * format.  When converting from one pixel format to another, information loss
 * may occur.  For example, when converting from RGB24 to GRAY, the color
 * information will be lost. Similarly, other losses occur when converting from
 * some formats to other formats. avcodec_find_best_pix_fmt() searches which of
 * the given pixel formats should be used to suffer the least amount of loss.
 * The pixel formats from which it chooses one, are determined by the
 * pix_fmt_mask parameter.
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
{$IF LIBAVCODEC_VERSION >= 52000000} // 52.0.0
function avcodec_find_best_pix_fmt(pix_fmt_mask: cint64; src_pix_fmt: TAVPixelFormat;
                            has_alpha: cint; loss_ptr: PCint): cint;
  cdecl; external av__codec;
{$ELSEIF LIBAVCODEC_VERSION < 52022001}
function avcodec_find_best_pix_fmt(pix_fmt_mask: cint; src_pix_fmt: TAVPixelFormat;
                            has_alpha: cint; loss_ptr: PCint): cint;
  cdecl; external av__codec;
{$ELSE}
function avcodec_find_best_pix_fmt(pix_fmt_mask: cint; src_pix_fmt: TAVPixelFormat;
                            has_alpha: cint; loss_ptr: PCint): TAVPixelFormat;
  cdecl; external av__codec;
{$IFEND}

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
{$IF LIBAVCODEC_VERSION < 52022001} // 52.22.1
procedure avcodec_pix_fmt_string (buf: PAnsiChar; buf_size: cint; pix_fmt: cint);
  cdecl; external av__codec;
{$ELSE}
procedure avcodec_pix_fmt_string (buf: PAnsiChar; buf_size: cint; pix_fmt: TAVPixelFormat);
  cdecl; external av__codec;
{$IFEND}
{$IFEND}

const
  FF_ALPHA_TRANSP      = $0001; {* image has some totally transparent pixels *}
  FF_ALPHA_SEMI_TRANSP = $0002; {* image has some transparent pixels *}

(**
 * Tell if an image really has transparent alpha values.
 * @return ored mask of FF_ALPHA_xxx constants
 *)
function img_get_alpha_info (src: {const} PAVPicture;
                             pix_fmt: TAVPixelFormat;
                             width:   cint;
                             height:  cint): cint;
  cdecl; external av__codec;

{$IF LIBAVCODEC_VERSION < 52000000} // 52.0.0
(**
 * convert among pixel formats
 * @deprecated Use the software scaler (swscale) instead.
 *)
function img_convert (dst: PAVPicture; dst_pix_fmt: TAVPixelFormat;
              src: {const} PAVPicture; pix_fmt: TAVPixelFormat;
              width: cint; height: cint): cint;
  cdecl; external av__codec; deprecated;
{$IFEND}

(* deinterlace a picture *)
(* deinterlace - if not supported return -1 *)
function avpicture_deinterlace (dst: PAVPicture;
                        src: {const} PAVPicture;
                        pix_fmt: TAVPixelFormat;
                        width:   cint;
                        height:  cint): cint;
  cdecl; external av__codec;

{* external high level API *}

{$IF LIBAVCODEC_VERSION < 52000000} // 52.0.0
{
var
  first_avcodec: PAVCodec; external av__codec;
}
{$IFEND}

{$IF LIBAVCODEC_VERSION >= 51049000} // 51.49.0
(**
 * If c is NULL, returns the first registered codec,
 * if c is non-NULL, returns the next registered codec after c,
 * or NULL if c is the last one.
 *)
function av_codec_next(c: PAVCodec): PAVCodec;
  cdecl; external av__codec;
{$IFEND}

(**
 * Returns the LIBAVCODEC_VERSION_INT constant.
 *)
function avcodec_version(): cuint;
  cdecl; external av__codec;

{$IF LIBAVCODEC_VERSION < 52008000} // 52.8.0
(* returns LIBAVCODEC_BUILD constant *)
function avcodec_build(): cuint;
  cdecl; external av__codec; deprecated;
{$IFEND}

{$IF LIBAVCODEC_VERSION >= 52041000} // 52.41.0
(**
 * Returns the libavcodec build-time configuration.
 *)
function avcodec_configuration(): PAnsiChar;
  cdecl; external av__codec;

(**
 * Returns the libavcodec license.
 *)
function avcodec_license(): PAnsiChar;
  cdecl; external av__codec;
{$IFEND}

(**
 * Initializes libavcodec.
 *
 * @warning This function must be called before any other libavcodec
 * function.
 *)
procedure avcodec_init();
  cdecl; external av__codec;

(**
 * Register the codec codec and initialize libavcodec.
 *
 * @see avcodec_init()
 *)
{$IF LIBAVCODEC_VERSION >= 52014000} // 52.14.0
procedure avcodec_register(codec: PAVCodec);
  cdecl; external av__codec;
// Deprecated in favor of avcodec_register.
procedure register_avcodec(codec: PAVCodec);
  cdecl; external av__codec; deprecated;
{$ELSEIF LIBAVCODEC_VERSION_MAJOR < 53}
procedure register_avcodec(codec: PAVCodec);
  cdecl; external av__codec;
{$IFEND}
(**
 * Finds a registered encoder with a matching codec ID.
 *
 * @param id CodecID of the requested encoder
 * @return An encoder if one was found, NULL otherwise.
 *)
function avcodec_find_encoder(id: TCodecID): PAVCodec;
  cdecl; external av__codec;

(**
 * Finds a registered encoder with the specified name.
 *
 * @param name name of the requested encoder
 * @return An encoder if one was found, NULL otherwise.
 *)
function avcodec_find_encoder_by_name(name: PAnsiChar): PAVCodec;
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
function avcodec_find_decoder_by_name(name: PAnsiChar): PAVCodec;
  cdecl; external av__codec;
procedure avcodec_string(buf: PAnsiChar; buf_size: cint; enc: PAVCodecContext; encode: cint);
  cdecl; external av__codec;

(**
 * Sets the fields of the given AVCodecContext to default values.
 *
 * @param s The AVCodecContext of which the fields should be set to default values.
 *)
procedure avcodec_get_context_defaults(s: PAVCodecContext);
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

function avcodec_default_get_buffer (s: PAVCodecContext; pic: PAVFrame): cint;
  cdecl; external av__codec;
procedure avcodec_default_release_buffer (s: PAVCodecContext; pic: PAVFrame);
  cdecl; external av__codec;
function avcodec_default_reget_buffer (s: PAVCodecContext; pic: PAVFrame): cint;
  cdecl; external av__codec;
procedure avcodec_align_dimensions(s: PAVCodecContext; width: PCint; height: PCint);
  cdecl; external av__codec;

(**
 * Checks if the given dimension of a picture is valid, meaning that all
 * bytes of the picture can be addressed with a signed int.
 *
 * @param[in] w Width of the picture.
 * @param[in] h Height of the picture.
 * @return Zero if valid, a negative value if invalid.
 *)
function avcodec_check_dimensions(av_log_ctx: pointer; w: cuint; h: cuint): cint;
  cdecl; external av__codec;
function avcodec_default_get_format(s: PAVCodecContext; fmt: {const} PAVPixelFormat): TAVPixelFormat;
  cdecl; external av__codec;

function avcodec_thread_init(s: PAVCodecContext; thread_count: cint): cint;
  cdecl; external av__codec;
procedure avcodec_thread_free(s: PAVCodecContext);
  cdecl; external av__codec;


{$IF LIBAVCODEC_VERSION < 52004000} // < 52.4.0
function avcodec_thread_execute(s: PAVCodecContext; func: TExecuteFunc; arg: PPointer; var ret: cint; count: cint): cint;
  cdecl; external av__codec;
{$ELSE}
function avcodec_thread_execute(s: PAVCodecContext; func: TExecuteFunc; arg: Pointer; var ret: cint; count: cint; size: cint): cint;
  cdecl; external av__codec;
{$IFEND}

{$IF LIBAVCODEC_VERSION < 52004000} // < 52.4.0
function avcodec_default_execute(s: PAVCodecContext; func: TExecuteFunc; arg: PPointer; var ret: cint; count: cint): cint;
  cdecl; external av__codec;
{$ELSE}
function avcodec_default_execute(s: PAVCodecContext; func: TExecuteFunc; arg: Pointer; var ret: cint; count: cint; size: cint): cint;
  cdecl; external av__codec;
{$IFEND}
{$IF LIBAVCODEC_VERSION >= 52037000} // >= 52.37.0
function avcodec_default_execute2(s: PAVCodecContext; func: TExecuteFunc; arg: Pointer; var ret: cint; count: cint): cint;
  cdecl; external av__codec;
{$IFEND}
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
function avcodec_open(avctx: PAVCodecContext; codec: PAVCodec): cint;
  cdecl; external av__codec;

{$IF LIBAVCODEC_VERSION < 52000000} // < 52.0.0
(**
 * @deprecated Use avcodec_decode_audio2 instead.
 *)
function avcodec_decode_audio(avctx: PAVCodecContext; samples: PSmallint;
                           var frame_size_ptr: cint;
                           buf: {const} PByteArray; buf_size: cint): cint;
  cdecl; external av__codec; {deprecated;}
{$IFEND}

{$IF LIBAVCODEC_MAX_VERSION_MAJOR < 53}
{$IF LIBAVCODEC_VERSION >= 51030000} // 51.30.0
(**
 * Decodes an audio frame from buf into samples.
 * Wrapper function which calls avcodec_decode_audio3.
 *
 * @deprecated Use avcodec_decode_audio3 instead.
 * @param avctx the codec context
 * @param[out] samples the output buffer, sample type in avctx->sample_fmt
 * @param[in,out] frame_size_ptr the output buffer size in bytes
 * @param[in] buf the input buffer
 * @param[in] buf_size the input buffer size in bytes
 * @return On error a negative value is returned, otherwise the number of bytes
 * used or zero if no frame could be decompressed.
 *)
function avcodec_decode_audio2(avctx: PAVCodecContext; samples: PSmallint;
               var frame_size_ptr: cint;
               buf: {const} PByteArray; buf_size: cint): cint;
  cdecl; external av__codec; {deprecated;}
{$IFEND}
{$IFEND}

{$IF LIBAVCODEC_VERSION >= 52025000} // 52.25.0
(**
 * Decodes the audio frame of size avpkt->size from avpkt->data into samples.
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
 * @note You might have to align the input buffer avpkt->data and output buffer
 * samples. The alignment requirements depend on the CPU: On some CPUs it isn't
 * necessary at all, on others it won't work at all if not aligned and on others
 * * it will work but it will have an impact on performance.
 *
 * In practice, avpkt->data should have 4 byte alignment at minimum and
 * samples should be 16 byte aligned unless the CPU doesn't need it
 * (AltiVec and SSE do).
 *
 * @note Some codecs have a delay between input and output, these need to be
 * feeded with avpkt->data=NULL, avpkt->size=0 at the end to return the remaining frames.
 *
 * @param avctx the codec context
 * @param[out] samples the output buffer
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
  cdecl; external av__codec;
{$IFEND}

{$IF LIBAVCODEC_MAX_VERSION_MAJOR < 53}
(**
 * Decodes a video frame from buf into picture.
 * Wrapper function which calls avcodec_decode_video2.
 *
 * @deprecated Use avcodec_decode_video2 instead.
 * @param avctx the codec context
 * @param[out] picture The AVFrame in which the decoded video frame will be stored.
 * @param[in] buf the input buffer
 * @param[in] buf_size the size of the input buffer in bytes
 * @param[in,out] got_picture_ptr Zero if no frame could be decompressed, otherwise, it is nonzero.
 * @return On error a negative value is returned, otherwise the number of bytes
 * used or zero if no frame could be decompressed.
 *)
function avcodec_decode_video(avctx: PAVCodecContext; picture: PAVFrame;
                       var got_picture_ptr: cint;
                       buf: {const} PByteArray; buf_size: cint): cint;
  cdecl; external av__codec; {deprecated;}
{$IFEND}

{$IF LIBAVCODEC_VERSION >= 52025000} // 52.25.0
(**
 * Decodes the video frame of size avpkt->size from avpkt->data into picture.
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
 * @param avctx the codec context
 * @param[out] picture The AVFrame in which the decoded video frame will be stored.
 * @param[in] avpkt The input AVpacket containing the input buffer.
 *            You can create such packet with av_init_packet() and by then setting
 *            data and size, some decoders might in addition need other fields like
 *            flags&PKT_FLAG_KEY. All decoders are designed to use the least
 *            fields possible.
 * @param[in,out] got_picture_ptr Zero if no frame could be decompressed, otherwise, it is nonzero.
 * @return On error a negative value is returned, otherwise the number of bytes
 * used or zero if no frame could be decompressed.
 *)
function avcodec_decode_video2(avctx: PAVCodecContext; picture: PAVFrame;
                       var got_picture_ptr: cint;
                       avpkt: PAVPacket): cint;
  cdecl; external av__codec;
{$IFEND}

{$IF LIBAVCODEC_MAX_VERSION_MAJOR < 53}
(* Decode a subtitle message. Return -1 if error, otherwise return the
 * number of bytes used. If no subtitle could be decompressed,
 * got_sub_ptr is zero. Otherwise, the subtitle is stored in*sub.
 *)
function avcodec_decode_subtitle(avctx: PAVCodecContext; sub: PAVSubtitle;
                          var got_sub_ptr: cint;
                          buf: {const} PByteArray; buf_size: cint): cint;
  cdecl; external av__codec;
{$IFEND}
  
{$IF LIBAVCODEC_VERSION >= 52025000} // 52.25.0
(* Decodes a subtitle message.
 * Returns a negative value on error, otherwise returns the number of bytes used.
 * If no subtitle could be decompressed, got_sub_ptr is zero.
 * Otherwise, the subtitle is stored in sub.
 *
 * @param avctx the codec context
 * @param[out] sub The AVSubtitle in which the decoded subtitle will be stored.
 * @param[in,out] got_sub_ptr Zero if no subtitle could be decompressed, otherwise, it is nonzero.
 * @param[in] avpkt The input AVPacket containing the input buffer.
 *)
function avcodec_decode_subtitle2(avctx: PAVCodecContext; sub: PAVSubtitle;
                          var got_sub_ptr: cint;
                          avpkt: PAVPacket): cint;
  cdecl; external av__codec;
{$IFEND}
  
function avcodec_parse_frame(avctx: PAVCodecContext; pdata: PPointer;
                      data_size_ptr: PCint;
                      buf: PByteArray; buf_size: cint): cint;
  cdecl; external av__codec;

(**
 * Encodes an audio frame from samples into buf.
 *
 * @note The output buffer should be at least FF_MIN_BUFFER_SIZE bytes large.
 * However, for PCM audio the user will know how much space is needed
 * because it depends on the value passed in buf_size as described
 * below. In that case a lower value can be used.
 *
 * @param avctx the codec context
 * @param[out] buf the output buffer
 * @param[in] buf_size the output buffer size
 * @param[in] samples the input buffer containing the samples
 * The number of samples read from this buffer is frame_size*channels,
 * both of which are defined in avctx.
 * For PCM audio the number of samples read from samples is equal to
 * buf_size * input_sample_size / output_sample_size.
 * @return On error a negative value is returned, on success zero or the number
 * of bytes used to encode the data read from the input buffer.
 *)
function avcodec_encode_audio(avctx: PAVCodecContext; buf: PByte;
                      buf_size: cint; samples: {const} PSmallint): cint;
  cdecl; external av__codec;

(**
 * Encodes a video frame from pict into buf.
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
                      buf_size: cint; pict: PAVFrame): cint;
  cdecl; external av__codec;
function avcodec_encode_subtitle(avctx: PAVCodecContext; buf: PByteArray;
                      buf_size: cint; sub: {const} PAVSubtitle): cint;
  cdecl; external av__codec;

function avcodec_close(avctx: PAVCodecContext): cint;
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
 * Flush buffers, should be called when seeking or when switching to a different stream.
 *)
procedure avcodec_flush_buffers(avctx: PAVCodecContext);
  cdecl; external av__codec;
  
procedure avcodec_default_free_buffers(s: PAVCodecContext);
  cdecl; external av__codec;

(* misc useful functions *)

(**
 * Returns a single letter to describe the given picture type pict_type.
 *
 * @param[in] pict_type the picture type
 * @return A single character representing the picture type.
 *)
function av_get_pict_type_char(pict_type: cint): AnsiChar;
  cdecl; external av__codec;

(**
 * Returns codec bits per sample.
 *
 * @param[in] codec_id the codec
 * @return Number of bits per sample or zero if unknown for the given codec.
 *)
function av_get_bits_per_sample(codec_id: TCodecID): cint;
  cdecl; external av__codec;

{$IF LIBAVCODEC_VERSION >= 51041000} // 51.41.0
(**
 * Returns sample format bits per sample.
 *
 * @param[in] sample_fmt the sample format
 * @return Number of bits per sample or zero if unknown for the given sample format.
 *)
function av_get_bits_per_sample_format(sample_fmt: TSampleFormat): cint;
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

    {$IF LIBAVCODEC_VERSION >= 51040003} // 51.40.3
    offset: cint64;      ///< byte offset from starting packet start
    {$IFEND}
    {$IF LIBAVCODEC_VERSION >= 51057001} // 51.57.1
    cur_frame_end: array [0..AV_PARSER_PTS_NB - 1] of cint64;
    {$IFEND}
    {$IF LIBAVCODEC_VERSION >= 52016000} // 52.16.0
    (*!
     * Set by parser to 1 for key frames and 0 for non-key frames.
     * It is initialized to -1, so if the parser doesn't set this flag,
     * old-style fallback using FF_I_TYPE picture type as key frames
     * will be used.
     *)
    key_frame: cint;
    {$IFEND}
    {$IF LIBAVCODEC_VERSION >= 52018000} // 52.18.0
    (**
     * Time difference in stream time base units from the pts of this
     * packet to the point at which the output from the decoder has converged
     * independent from the availability of previous frames. That is, the
     * frames are virtually identical no matter if decoding started from
     * the very first frame or from this keyframe.
     * Is AV_NOPTS_VALUE if unknown.
     * This field is not the display duration of the current frame.
     *
     * The purpose of this field is to allow seeking in streams that have no
     * keyframes in the conventional sense. It corresponds to the
     * recovery point SEI in H.264 and match_time_delta in NUT. It is also
     * essential for some types of subtitle streams to ensure that all
     * subtitles are correctly displayed after seeking.
     *)
    convergence_duration: cint64;
    {$IFEND}
    {$IF LIBAVCODEC_VERSION >= 52019000} // 52.19.0
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
    {$IFEND}

    {$IF LIBAVCODEC_VERSION >= 52021000} // 52.21.0
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
    {$IFEND}
  end;

  TAVCodecParser = record
    codec_ids: array [0..4] of cint; (* several codec IDs are permitted *)
    priv_data_size: cint;
    parser_init: function(s: PAVCodecParserContext): cint; cdecl;
    parser_parse: function(s: PAVCodecParserContext; avctx: PAVCodecContext;
                            poutbuf: {const} PPointer; poutbuf_size: PCint;
                            buf: {const} PByteArray; buf_size: cint): cint; cdecl;
    parser_close: procedure(s: PAVCodecParserContext); cdecl;
    split: function(avctx: PAVCodecContext; buf: {const} PByteArray;
                    buf_size: cint): cint; cdecl;
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

procedure av_register_codec_parser(parser: PAVCodecParser);
  cdecl; external av__codec;

function av_parser_init(codec_id: cint): PAVCodecParserContext;
  cdecl; external av__codec;

{$IF LIBAVCODEC_MAX_VERSION_MAJOR < 53}
function av_parser_parse(s: PAVCodecParserContext;
                  avctx: PAVCodecContext;
                  poutbuf: PPointer;
                  poutbuf_size: PCint;
                  buf: {const} PByteArray;
                  buf_size: cint;
                  pts: cint64;
                  dts: cint64): cint;
  cdecl; external av__codec; deprecated;
{$IFEND}

{$IF LIBAVCODEC_VERSION >= 52021000} // 52.21.0
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
{$IFEND}

function av_parser_change(s: PAVCodecParserContext;
                   avctx: PAVCodecContext;
                   poutbuf: PPointer; poutbuf_size: PCint;
                   buf: {const} PByteArray; buf_size: cint; keyframe: cint): cint;
  cdecl; external av__codec;
procedure av_parser_close(s: PAVCodecParserContext);
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
                  avctx: PAVCodecContext; args: PByteArray;
                  poutbuf: PPointer; poutbuf_size: PCint;
                  buf: PByte; buf_size: cint; keyframe: cint): cint; cdecl;
    {$IF LIBAVCODEC_VERSION >= 51043000} // 51.43.0
    close: procedure(bsfc: PAVBitStreamFilterContext);
    {$IFEND}
    next: PAVBitStreamFilter;
  end;

procedure av_register_bitstream_filter(bsf: PAVBitStreamFilter);
  cdecl; external av__codec;

function av_bitstream_filter_init(name: PAnsiChar): PAVBitStreamFilterContext;
  cdecl; external av__codec;

function av_bitstream_filter_filter(bsfc: PAVBitStreamFilterContext;
                               avctx: PAVCodecContext; args: PByteArray;
                               poutbuf: PPointer; poutbuf_size: PCint;
                               buf: PByte; buf_size: cint; keyframe: cint): cint;
  cdecl; external av__codec;
procedure av_bitstream_filter_close(bsf: PAVBitStreamFilterContext);
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
procedure av_fast_realloc(ptr: pointer; size: PCuint; min_size: cuint);
  cdecl; external av__codec;

{$IF LIBAVCODEC_VERSION >= 52025000} // >= 52.25.0
(**
 * Allocates a buffer, reusing the given one if large enough.
 *
 * Contrary to av_fast_realloc the current buffer contents might not be
 * preserved and on error the old buffer is freed, thus no special
 * handling to avoid memleaks is necessary.
 *
 * @param ptr pointer to pointer to already allocated buffer, overwritten with pointer to new buffer
 * @param size size of the buffer *ptr points to
 * @param min_size minimum size of *ptr buffer after returning, *ptr will be NULL and
 *                 *size 0 if an error occurred.
 *)
procedure av_fast_malloc(ptr: pointer; size: PCuint; min_size: cuint);
  cdecl; external av__codec;
{$IFEND}

{$IF LIBAVCODEC_VERSION < 51057000} // 51.57.0
(* for static data only *)

(**
 * Frees all static arrays and resets their pointers to 0.
 * Call this function to release all statically allocated tables.
 *
 * @deprecated. Code which uses av_free_static is broken/misdesigned
 * and should correctly use static arrays
 *
 *)
procedure av_free_static();
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
procedure av_mallocz_static(size: cuint);
  cdecl; external av__codec; deprecated; {av_malloc_attrib av_alloc_size(1)}
{$IFEND}

{$IF LIBAVCODEC_VERSION < 51035000} // 51.35.0
procedure av_realloc_static(ptr: pointer; size: cuint);
  cdecl; external av__codec;
{$IFEND}

{$IF LIBAVCODEC_VERSION >= 51039000} // 51.39.0
(**
 * Copy image 'src' to 'dst'.
 *)
procedure av_picture_copy(dst: PAVPicture; 
              src: {const} PAVPicture;
{$IF LIBAVCODEC_VERSION < 52022001} // 52.22.1
              pix_fmt: cint;
{$ELSE}
              pix_fmt: TAVPixelFormat;
{$IFEND}
              width:  cint;
              height: cint);
  cdecl; external av__codec;

(**
 * Crop image top and left side.
 *)
function av_picture_crop(dst: PAVPicture;
              src: {const} PAVPicture;
{$IF LIBAVCODEC_VERSION < 52022001} // 52.22.1
              pix_fmt: cint;
{$ELSE}
              pix_fmt: TAVPixelFormat;
{$IFEND}
              top_band:  cint;
              left_band: cint): cint;
  cdecl; external av__codec;

(**
 * Pad image.
 *)
function av_picture_pad(dst: PAVPicture;
            src: {const} PAVPicture;
            height: cint;
            width: cint;
{$IF LIBAVCODEC_VERSION < 52022001} // 52.22.1
            pix_fmt: cint;
{$ELSE}
            pix_fmt: TAVPixelFormat;
{$IFEND}
            padtop: cint;
            padbottom: cint;
            padleft: cint;
            padright:
            cint;
            color: PCint): cint;
  cdecl; external av__codec;
{$IFEND}

{$IF LIBAVCODEC_VERSION < 52000000} // 52.0.0
(**
 * @deprecated Use the software scaler (swscale) instead.
 *)
procedure img_copy(dst: PAVPicture; src: {const} PAVPicture;
                    pix_fmt: TAVPixelFormat; width: cint; height: cint);
  cdecl; external av__codec; deprecated;

(**
 * @deprecated Use the software scaler (swscale) instead.
 *)
function img_crop(dst: PAVPicture; src: {const} PAVPicture;
           pix_fmt: TAVPixelFormat; top_band, left_band: cint): cint;
  cdecl; external av__codec; deprecated;

(**
 * @deprecated Use the software scaler (swscale) instead.
 *)
function img_pad(dst: PAVPicture; src: {const} PAVPicture; height, width: cint;
                  pix_fmt: TAVPixelFormat; padtop, padbottom, padleft, padright: cint;
                  color: PCint): cint;
  cdecl; external av__codec; deprecated;
{$IFEND}

function av_xiphlacing(s: PByte; v: cuint): cuint;
  cdecl; external av__codec;

{$IF LIBAVCODEC_VERSION >= 51041000} // 51.41.0
(**
 * Parses str and put in width_ptr and height_ptr the detected values.
 *
 * @return 0 in case of a successful parsing, a negative value otherwise
 * @param[in] str the string to parse: it has to be a string in the format
 * <width>x<height> or a valid video frame size abbreviation.
 * @param[in,out] width_ptr pointer to the variable which will contain the detected
 * frame width value
 * @param[in,out] height_ptr pointer to the variable which will contain the detected
 * frame height value
 *)
function av_parse_video_frame_size(width_ptr: PCint; height_ptr: PCint; str: {const} PAnsiChar): cint;
  cdecl; external av__codec;

(**
 * Parses str and put in frame_rate the detected values.
 *
 * @return 0 in case of a successful parsing, a negative value otherwise
 * @param[in] str the string to parse: it has to be a string in the format
 * <frame_rate_num>/<frame_rate_den>, a float number or a valid video rate abbreviation
 * @param[in,out] frame_rate pointer to the AVRational which will contain the detected
 * frame rate
 *)
function av_parse_video_frame_rate(frame_rate: PAVRational; str: {const} PAnsiChar): cint;
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
  EPIPE  = ESysEPIPE;
{$ELSE}
  ENOENT = 2;
  EIO    = 5;
  ENOMEM = 12;
  EINVAL = 22;
  EPIPE  = 32;  // just an assumption. needs to be checked.
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
  AVERROR_NOENT       = AVERROR_SIGN * ENOENT;  (**< No such file or directory. *)
{$IF LIBAVCODEC_VERSION >= 52017000} // 52.17.0
  AVERROR_EOF         = AVERROR_SIGN * EPIPE;   (**< End of file. *)
{$IFEND}
  // Note: function calls as constant-initializers are invalid
  //AVERROR_PATCHWELCOME = -MKTAG('P','A','W','E'); {**< Not yet implemented in FFmpeg. Patches welcome. *}
  AVERROR_PATCHWELCOME = -(ord('P') or (ord('A') shl 8) or (ord('W') shl 16) or (ord('E') shl 24));

{$IF LIBAVCODEC_VERSION >= 52032000} // >= 52.32.0
(**
 * Logs a generic warning message about a missing feature. This function is
 * intended to be used internally by FFmpeg (libavcodec, libavformat, etc.)
 * only, and would normally not be used by applications.
 * @param[in] avc a pointer to an arbitrary struct of which the first field is
 * a pointer to an AVClass struct
 * @param[in] feature string containing the name of the missing feature
 * @param[in] want_sample indicates if samples are wanted which exhibit this feature.
 * If want_sample is non-zero, additional verbage will be added to the log
 * message which tells the user how to report samples to the development
 * mailing list.
 *)
procedure av_log_missing_feature(avc: Pointer; feature: {const} Pchar; want_sample: cint);
  cdecl; external av__codec;

(**
 * Logs a generic warning message asking for a sample. This function is
 * intended to be used internally by FFmpeg (libavcodec, libavformat, etc.)
 * only, and would normally not be used by applications.
 * @param[in] avc a pointer to an arbitrary struct of which the first field is
 * a pointer to an AVClass struct
 * @param[in] msg string containing an optional message, or NULL if no message
 *)
procedure av_log_ask_for_sample(avc: Pointer; msg: {const} Pchar);
  cdecl; external av__codec;
{$IFEND}

{$IF LIBAVCODEC_VERSION >= 52018000} // 52.18.0
(**
 * Registers the hardware accelerator hwaccel.
 *)
procedure av_register_hwaccel (hwaccel: PAVHWAccel)
  cdecl; external av__codec;

(**
 * If hwaccel is NULL, returns the first registered hardware accelerator,
 * if hwaccel is non-NULL, returns the next registered hardware accelerator
 * after hwaccel, or NULL if hwaccel is the last one.
 *)
function av_hwaccel_next (hwaccel: PAVHWAccel): PAVHWAccel;
  cdecl; external av__codec;
{$IFEND}

{$IF LIBAVCODEC_VERSION >= 52030000} // 52.30.0
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
 * specified by AVLockOp. mutex points to a (void) where the
 * lockmgr should store/get a pointer to a user allocated mutex. It's
 * NULL upon AV_LOCK_CREATE and != NULL for all other ops.
 *
 * @param cb User defined callback. Note: FFmpeg may invoke calls to this
 *           callback during the call to av_lockmgr_register().
 *           Thus, the application must be prepared to handle that.
 *           If cb is set to NULL the lockmgr will be unregistered.
 *           Also note that during unregistration the previously registered
 *           lockmgr callback may also be invoked.
 *)
// ToDo: Implement and test this
//function av_lockmgr_register(cb: function (mutex: pointer; op: TAVLockOp)): cint;
//  cdecl; external av__codec;
{$IFEND}

implementation

{$IF (LIBAVCODEC_VERSION >= 52025000) and (LIBAVCODEC_VERSION <= 52027000)} // 52.25.0 - 52.27.0
procedure av_free_packet(pkt: PAVPacket);{$IFDEF HASINLINE} inline; {$ENDIF} 
begin
  if assigned(pkt) then
  begin
    if assigned(pkt^.destruct) then
      pkt^.destruct(pkt);
    pkt^.data := NIL;
    pkt^.size := 0;
  end;
end;
{$IFEND}

end.
