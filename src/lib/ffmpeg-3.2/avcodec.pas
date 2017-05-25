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
  LIBAVCODEC_MAX_VERSION_MAJOR   = 57;
  LIBAVCODEC_MAX_VERSION_MINOR   = 92; (* even up to FFmpeg commit 1fd762777 *)
  LIBAVCODEC_MAX_VERSION_RELEASE = 100;
  LIBAVCODEC_MAX_VERSION = (LIBAVCODEC_MAX_VERSION_MAJOR * VERSION_MAJOR) +
                           (LIBAVCODEC_MAX_VERSION_MINOR * VERSION_MINOR) +
                           (LIBAVCODEC_MAX_VERSION_RELEASE * VERSION_RELEASE);

  (* Min. supported version by this header *)
  LIBAVCODEC_MIN_VERSION_MAJOR   = 57;
  LIBAVCODEC_MIN_VERSION_MINOR   = 64;
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
 * @ingroup libavc
 * @defgroup lavc_encdec send/receive encoding and decoding API overview
 * @{
 *
 * The avcodec_send_packet()/avcodec_receive_frame()/avcodec_send_frame()/
 * avcodec_receive_packet() functions provide an encode/decode API, which
 * decouples input and output.
 *
 * The API is very similar for encoding/decoding and audio/video, and works as
 * follows:
 * - Set up and open the AVCodecContext as usual.
 * - Send valid input:
 *   - For decoding, call avcodec_send_packet() to give the decoder raw
 *     compressed data in an AVPacket.
 *   - For encoding, call avcodec_send_frame() to give the decoder an AVFrame
 *     containing uncompressed audio or video.
 *   In both cases, it is recommended that AVPackets and AVFrames are
 *   refcounted, or libavcodec might have to copy the input data. (libavformat
 *   always returns refcounted AVPackets, and av_frame_get_buffer() allocates
 *   refcounted AVFrames.)
 * - Receive output in a loop. Periodically call one of the avcodec_receive_*()
 *   functions and process their output:
 *   - For decoding, call avcodec_receive_frame(). On success, it will return
 *     an AVFrame containing uncompressed audio or video data.
 *   - For encoding, call avcodec_receive_packet(). On success, it will return
 *     an AVPacket with a compressed frame.
 *   Repeat this call until it returns AVERROR(EAGAIN) or an error. The
 *   AVERROR(EAGAIN) return value means that new input data is required to
 *   return new output. In this case, continue with sending input. For each
 *   input frame/packet, the codec will typically return 1 output frame/packet,
 *   but it can also be 0 or more than 1.
 *
 * At the beginning of decoding or encoding, the codec might accept multiple
 * input frames/packets without returning a frame, until its internal buffers
 * are filled. This situation is handled transparently if you follow the steps
 * outlined above.
 *
 * End of stream situations. These require "flushing" (aka draining) the codec,
 * as the codec might buffer multiple frames or packets internally for
 * performance or out of necessity (consider B-frames).
 * This is handled as follows:
 * - Instead of valid input, send NULL to the avcodec_send_packet() (decoding)
 *   or avcodec_send_frame() (encoding) functions. This will enter draining
 *   mode.
 * - Call avcodec_receive_frame() (decoding) or avcodec_receive_packet()
 *   (encoding) in a loop until AVERROR_EOF is returned. The functions will
 *   not return AVERROR(EAGAIN), unless you forgot to enter draining mode.
 * - Before decoding can be resumed again, the codec has to be reset with
 *   avcodec_flush_buffers().
 *
 * Using the API as outlined above is highly recommended. But it is also
 * possible to call functions outside of this rigid schema. For example, you can
 * call avcodec_send_packet() repeatedly without calling
 * avcodec_receive_frame(). In this case, avcodec_send_packet() will succeed
 * until the codec's internal buffer has been filled up (which is typically of
 * size 1 per output frame, after initial input), and then reject input with
 * AVERROR(EAGAIN). Once it starts rejecting input, you have no choice but to
 * read at least some output.
 *
 * Not all codecs will follow a rigid and predictable dataflow; the only
 * guarantee is that an AVERROR(EAGAIN) return value on a send/receive call on
 * one end implies that a receive/send call on the other end will succeed. In
 * general, no codec will permit unlimited buffering of input or output.
 *
 * This API replaces the following legacy functions:
 * - avcodec_decode_video2() and avcodec_decode_audio4():
 *   Use avcodec_send_packet() to feed input to the decoder, then use
 *   avcodec_receive_frame() to receive decoded frames after each packet.
 *   Unlike with the old video decoding API, multiple frames might result from
 *   a packet. For audio, splitting the input packet into frames by partially
 *   decoding packets becomes transparent to the API user. You never need to
 *   feed an AVPacket to the API twice.
 *   Additionally, sending a flush/draining packet is required only once.
 * - avcodec_encode_video2()/avcodec_encode_audio2():
 *   Use avcodec_send_frame() to feed input to the encoder, then use
 *   avcodec_receive_packet() to receive encoded packets.
 *   Providing user-allocated buffers for avcodec_receive_packet() is not
 *   possible.
 * - The new API does not handle subtitles yet.
 *
 * Mixing new and old function calls on the same AVCodecContext is not allowed,
 * and will result in undefined behavior.
 *
 * Some codecs might require using the new API; using the old API will return
 * an error when calling it.
 * @}
 *)
 
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
 * 1. no value of an existing codec ID changes (that would break ABI),
 * 2. it is as close as possible to similar codecs
 * 
 * After adding new codec IDs, do not forget to add an entry to the codec
 * descriptor list and bump libavcodec minor version.
 *)
type
  PAVCodecID = ^TAVCodecID;
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
    AV_CODEC_ID_IFF_BYTERUN1 = AV_CODEC_ID_IFF_ILBM,
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
    AV_CODEC_ID_ESCAPE130,
    AV_CODEC_ID_G2M,
    AV_CODEC_ID_WEBP,
    AV_CODEC_ID_HNM4_VIDEO,
    AV_CODEC_ID_HEVC,
    AV_CODEC_ID_FIC,
    AV_CODEC_ID_ALIAS_PIX,
    AV_CODEC_ID_BRENDER_PIX,
    AV_CODEC_ID_PAF_VIDEO,
    AV_CODEC_ID_EXR,
    AV_CODEC_ID_VP7,
    AV_CODEC_ID_SANM,
    AV_CODEC_ID_SGIRLE,
    AV_CODEC_ID_MVC1,
    AV_CODEC_ID_MVC2,
    AV_CODEC_ID_HQX,
    AV_CODEC_ID_TDSC,
    AV_CODEC_ID_HQ_HQA,
    AV_CODEC_ID_HAP,
    AV_CODEC_ID_DDS,
    AV_CODEC_ID_DXV,
    AV_CODEC_ID_SCREENPRESSO,
    AV_CODEC_ID_RSCC,

    AV_CODEC_ID_Y41P = $8000,
    AV_CODEC_ID_AVRP,
    AV_CODEC_ID_012V,
    AV_CODEC_ID_AVUI,
    AV_CODEC_ID_AYUV,
    AV_CODEC_ID_TARGA_Y216,
    AV_CODEC_ID_V308,
    AV_CODEC_ID_V408,
    AV_CODEC_ID_YUV4,
    AV_CODEC_ID_AVRN,
    AV_CODEC_ID_CPIA,
    AV_CODEC_ID_XFACE,
    AV_CODEC_ID_SNOW,
    AV_CODEC_ID_SMVJPEG,
    AV_CODEC_ID_APNG,
    AV_CODEC_ID_DAALA,
    AV_CODEC_ID_CFHD,
    AV_CODEC_ID_TRUEMOTION2RT,
    AV_CODEC_ID_M101,
    AV_CODEC_ID_MAGICYUV,
    AV_CODEC_ID_SHEERVIDEO,
    AV_CODEC_ID_YLC,

    //* various PCM "codecs" */
    AV_CODEC_ID_FIRST_AUDIO = $10000,     ///< A dummy id pointing at the start of audio codecs
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
    AV_CODEC_ID_PCM_S24LE_PLANAR,
    AV_CODEC_ID_PCM_S32LE_PLANAR,
    AV_CODEC_ID_PCM_S16BE_PLANAR,

    AV_CODEC_ID_PCM_S64LE = $10800,
    AV_CODEC_ID_PCM_S64BE,

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
    AV_CODEC_ID_ADPCM_VIMA,
{$IFDEF FF_API_VIMA_DECODER}
    AV_CODEC_ID_VIMA       = AV_CODEC_ID_ADPCM_VIMA,
{$IFEND}
    AV_CODEC_ID_ADPCM_AFC  = $11800,
    AV_CODEC_ID_ADPCM_IMA_OKI,
    AV_CODEC_ID_ADPCM_DTK,
    AV_CODEC_ID_ADPCM_IMA_RAD,
    AV_CODEC_ID_ADPCM_G726LE,
    AV_CODEC_ID_ADPCM_THP_LE,
    AV_CODEC_ID_ADPCM_PSX,
    AV_CODEC_ID_ADPCM_AICA,
    AV_CODEC_ID_ADPCM_IMA_DAT4,
    AV_CODEC_ID_ADPCM_MTAF,

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

    AV_CODEC_ID_SDX2_DPCM = $14800,
    
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
    AV_CODEC_ID_OPUS,
    AV_CODEC_ID_COMFORT_NOISE,
    AV_CODEC_ID_TAK,
    AV_CODEC_ID_PAF_AUDIO,
    AV_CODEC_ID_ON2AVC,
    AV_CODEC_ID_DSS_SP,

    AV_CODEC_ID_FFWAVESYNTH = $15800,
    AV_CODEC_ID_SONIC,
    AV_CODEC_ID_SONIC_LS,
    AV_CODEC_ID_EVRC,
    AV_CODEC_ID_SMV,
    AV_CODEC_ID_DSD_LSBF,
    AV_CODEC_ID_DSD_MSBF,
    AV_CODEC_ID_DSD_LSBF_PLANAR,
    AV_CODEC_ID_DSD_MSBF_PLANAR,
    AV_CODEC_ID_4GV,
    AV_CODEC_ID_INTERPLAY_ACM,
    AV_CODEC_ID_XMA1,
    AV_CODEC_ID_XMA2,
    AV_CODEC_ID_DST,

    //* subtitle codecs */
    AV_CODEC_ID_FIRST_SUBTITLE = $17000,          ///< A dummy ID pointing at the start of subtitle codecs.
    AV_CODEC_ID_DVD_SUBTITLE   = $17000,
    AV_CODEC_ID_DVB_SUBTITLE,
    AV_CODEC_ID_TEXT,  ///< raw UTF-8 text
    AV_CODEC_ID_XSUB,
    AV_CODEC_ID_SSA,
    AV_CODEC_ID_MOV_TEXT,
    AV_CODEC_ID_HDMV_PGS_SUBTITLE,
    AV_CODEC_ID_DVB_TELETEXT,
    AV_CODEC_ID_SRT,
    AV_CODEC_ID_MICRODVD   = $17800,
    AV_CODEC_ID_EIA_608,
    AV_CODEC_ID_JACOSUB,
    AV_CODEC_ID_SAMI,
    AV_CODEC_ID_REALTEXT,
    AV_CODEC_ID_STL,
    AV_CODEC_ID_SUBVIEWER1,
    AV_CODEC_ID_SUBVIEWER,
    AV_CODEC_ID_SUBRIP,
    AV_CODEC_ID_WEBVTT,
    AV_CODEC_ID_MPL2,
    AV_CODEC_ID_VPLAYER,
    AV_CODEC_ID_PJS,
    AV_CODEC_ID_ASS,
    AV_CODEC_ID_HDMV_TEXT_SUBTITLE,

    //* other specific kind of codecs (generally used for attachments) */
    AV_CODEC_ID_FIRST_UNKNOWN = $18000,           ///< A dummy ID pointing at the start of various fake codecs.
    AV_CODEC_ID_TTF = $18000,

    AV_CODEC_ID_SCTE_35, ///< Contain timestamp estimated through PCR of program stream.
    AV_CODEC_ID_BINTEXT    = $18800,
    AV_CODEC_ID_XBIN,
    AV_CODEC_ID_IDF,
    AV_CODEC_ID_OTF,
    AV_CODEC_ID_SMPTE_KLV,
    AV_CODEC_ID_DVD_NAV,
    AV_CODEC_ID_TIMED_ID3,
    AV_CODEC_ID_BIN_DATA,

    AV_CODEC_ID_PROBE = $19000, ///< codec_id is not known (like AV_CODEC_ID_NONE) but lavf should attempt to identify it

    AV_CODEC_ID_MPEG2TS = $20000, (**< _FAKE_ codec to indicate a raw MPEG-2 TS
                               * stream (only used by libavformat) *)
    AV_CODEC_ID_MPEG4SYSTEMS = $20001, (**< _FAKE_ codec to indicate a MPEG-4 Systems
                                 * stream (only used by libavformat) *)
    AV_CODEC_ID_FFMETADATA = $21000,   ///< Dummy codec for streams containing only metadata information.
    AV_CODEC_ID_WRAPPED_AVFRAME = $21001 ///< Passthrough codec, AVFrames wrapped in AVPacket
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

  (**
   * AVProfile.
   *)
  PAVProfile = ^TAVProfile;
  TAVProfile = record
    profile: cint;
    name: {const} PAnsiChar; ///< short name for the profile
  end; {TAVProfile}
  
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

    (**
     * If non-NULL, an array of profiles recognized for this codec.
     * Terminated with FF_PROFILE_UNKNOWN.
     *)
    profiles: PAVProfile;
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
  AV_INPUT_BUFFER_PADDING_SIZE = 32;

(**
 * @ingroup lavc_decoding
 * minimum encoding buffer size.
 * Used to avoid some checks during header writing.
 *)
  AV_INPUT_BUFFER_MIN_SIZE = 16384;

{$IFDEF FF_API_WITHOUT_PREFIX}
(**
 * @deprecated use AV_INPUT_BUFFER_PADDING_SIZE instead
 *)
  FF_INPUT_BUFFER_PADDING_SIZE = 32;

(**
 * @deprecated use AV_INPUT_BUFFER_MIN_SIZE instead
 *)
  FF_MIN_BUFFER_SIZE = 16384;
{$ENDIF}

type
(*
 * @ingroup lavc_decoding
 * motion estimation type.
 * @deprecated use codec private option instead
 *)
{$IFDEF FF_API_MOTION_EST}
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
{$ENDIF}

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
  AV_CODEC_FLAG_UNALIGNED      = $0001; // (1 <<  0)
(**
 * Use fixed qscale.
 *)
  AV_CODEC_FLAG_QSCALE         = $0002; // (1 <<  1)
(**
 * 4 MV per MB allowed / advanced prediction for H.263.
 *)
  AV_CODEC_FLAG_4MV            = $0004; // (1 <<  2)
(**
 * Output even those frames that might be corrupted.
 *)
  AV_CODEC_FLAG_OUTPUT_CORRUPT = $0008; // (1 <<  3)
(**
 * Use qpel MC.
 *)
  AV_CODEC_FLAG_QPEL           = $0010; // (1 <<  4)
(**
 * Use internal 2pass ratecontrol in first pass mode.
 *)
  AV_CODEC_FLAG_PASS1          = $0200; // (1 <<  9)
(**
 * Use internal 2pass ratecontrol in second pass mode.
 *)
  AV_CODEC_FLAG_PASS2          = $0400; // (1 << 10)
(**
 * loop filter.
 *)
  AV_CODEC_FLAG_LOOP_FILTER    = $0800; // (1 << 11)
(**
 * Only decode/encode grayscale.
 *)
  AV_CODEC_FLAG_GRAY           = $2000; // (1 << 13)
(**
 * error[?] variables will be set during encoding.
 *)
  AV_CODEC_FLAG_PSNR           = $8000; // (1 << 15)
(**
 * Input bitstream might be truncated at a random location
 * instead of only at frame boundaries.
 *)
  AV_CODEC_FLAG_TRUNCATED      = $10000; // (1 << 16)
(**
 * Use interlaced DCT.
 *)
  AV_CODEC_FLAG_INTERLACED_DCT = $40000; // (1 << 18)
(**
 * Force low delay.
 *)
  AV_CODEC_FLAG_LOW_DELAY      = $80000; // (1 << 19)
(**
 * Place global headers in extradata instead of every keyframe.
 *)
  AV_CODEC_FLAG_GLOBAL_HEADER  = $400000; // (1 << 22)
(**
 * Use only bitexact stuff (except (I)DCT).
 *)
  AV_CODEC_FLAG_BITEXACT       = $800000; // (1 << 23)
(* Fx : Flag for H.263+ extra options *)
(**
 * H.263 advanced intra coding / MPEG-4 AC prediction
 *)
  AV_CODEC_FLAG_AC_PRED        = $1000000; // (1 << 24)
(**
 * interlaced motion estimation
 *)
  AV_CODEC_FLAG_INTERLACED_ME  = $20000000; // (1 << 29)
  AV_CODEC_FLAG_CLOSED_GOP     = $80000000; // (1U << 31)

(**
 * Allow non spec compliant speedup tricks.
 *)
  AV_CODEC_FLAG2_FAST          = $0001; // (1 <<  0)
(**
 * Skip bitstream encoding.
 *)
  AV_CODEC_FLAG2_NO_OUTPUT     = $0004; // (1 <<  2)
(**
 * Place global headers at every keyframe instead of in extradata.
 *)
  AV_CODEC_FLAG2_LOCAL_HEADER  = $0008; // (1 <<  3)

(**
 * timecode is in drop frame format. DEPRECATED!!!!
 *)
  AV_CODEC_FLAG2_DROP_FRAME_TIMECODE = $2000; // (1 << 13)

(**
 * Input bitstream might be truncated at a packet boundaries
 * instead of only at frame boundaries.
 *)
  AV_CODEC_FLAG2_CHUNKS        = $8000; // (1 << 15)
(**
 * Discard cropping information from SPS.
 *)
  AV_CODEC_FLAG2_IGNORE_CROP   = $10000; // (1 << 16)

(**
 * Show all frames before the first keyframe
 *)
  AV_CODEC_FLAG2_SHOW_ALL      = $400000; // (1 << 22)
(**
 * Export motion vectors through frame side data
 *)
  AV_CODEC_FLAG2_EXPORT_MVS    = $10000000; // (1 << 28)
(**
 * Do not skip samples and export skip information as frame side data
 *)
  AV_CODEC_FLAG2_SKIP_MANUAL   = $20000000; // (1 << 29)
(**
 * Do not reset ASS ReadOrder field on flush (subtitles decoding)
 *)
  AV_CODEC_FLAG2_RO_FLUSH_NOOP = $40000000;

(* Unsupported options :
 *              Syntax Arithmetic coding (SAC)
 *              Reference Picture Selection
 *              Independent Segment Decoding *)
(* /Fx *)
(* codec capabilities *)

(**
 * Decoder can use draw_horiz_band callback.
 *)
  AV_CODEC_CAP_DRAW_HORIZ_BAND = $0001; // (1 <<  0)
(**
 * Codec uses get_buffer() for allocating buffers and supports custom allocators.
 * If not set, it might not use get_buffer() at all or use operations that
 * assume the buffer was allocated by avcodec_default_get_buffer.
 *)
  AV_CODEC_CAP_DR1             = $0002; // (1 <<  1)
  AV_CODEC_CAP_TRUNCATED       = $0008; // (1 <<  3)
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
  AV_CODEC_CAP_DELAY           = $0020; // (1 <<  5)
(**
 * Codec can be fed a final frame with a smaller size.
 * This can be used to prevent truncation of the last audio samples.
 *)
  AV_CODEC_CAP_SMALL_LAST_FRAME = $0040; // (1 <<  6)

{$IFDEF FF_API_CAP_VDPAU}
(**
 * Codec can export data for HW decoding (VDPAU).
 *)
  AV_CODEC_CAP_HWACCEL_VDPAU   = $0080; // (1 <<  7)
{$ENDIF}

(**
 * Codec can output multiple frames per AVPacket
 * Normally demuxers return one frame at a time, demuxers which do not do
 * are connected to a parser to split what they return into proper frames.
 * This flag is reserved to the very rare category of codecs which have a
 * bitstream that cannot be split into frames without timeconsuming
 * operations like full decoding. Demuxers carrying such bitstreams thus
 * may return multiple frames in a packet. This has many disadvantages like
 * prohibiting stream copy in many cases thus it should only be considered
 * as a last resort.
 *)
  AV_CODEC_CAP_SUBFRAMES       = $0100; // (1 <<  8)
(**
 * Codec is experimental and is thus avoided in favor of non experimental
 * encoders
 *)
  AV_CODEC_CAP_EXPERIMENTAL    = $0200; // (1 <<  9)
(**
 * Codec should fill in channel configuration and samplerate instead of container
 *)
  AV_CODEC_CAP_CHANNEL_CONF    = $0400; // (1 << 10)
(**
 * Codec supports frame-level multithreading.
 *)
  AV_CODEC_CAP_FRAME_THREADS   = $1000; // (1 << 12)
(**
 * Codec supports slice-based (or partition-based) multithreading.
 *)
  AV_CODEC_CAP_SLICE_THREADS   = $2000; // (1 << 13)
(**
 * Codec supports changed parameters at any point.
 *)
  AV_CODEC_CAP_PARAM_CHANGE    = $4000; // (1 << 14)
(**
 * Codec supports avctx->thread_count == 0 (auto).
 *)
  AV_CODEC_CAP_AUTO_THREADS    = $8000; // (1 << 15)
(**
 * Audio encoder supports receiving a different number of samples in each call.
 *)
  AV_CODEC_CAP_VARIABLE_FRAME_SIZE = $10000; // (1 << 16)
(**
 * Decoder is not a preferred choice for probing.
 * This indicates that the decoder is not a good choice for probing.
 * It could for example be an expensive to spin up hardware decoder,
 * or it could simply not provide a lot of useful information about
 * the stream.
 * A decoder marked with this flag should only be used as last resort
 * choice for probing.
 *)
  AV_CODEC_CAP_AVOID_PROBING   = $20000;  // (1 << 17)

(**
 * Codec is intra only.
 *)
  AV_CODEC_CAP_INTRA_ONLY    = $40000000;
(**
 * Codec is lossless.
 *)
  AV_CODEC_CAP_LOSSLESS      = $80000000;


{$IFDEF FF_API_WITHOUT_PREFIX}
(**
 * Allow decoders to produce frames with data planes that are not aligned
 * to CPU requirements (e.g. due to cropping).
 *)
  CODEC_FLAG_UNALIGNED = AV_CODEC_FLAG_UNALIGNED;
  CODEC_FLAG_QSCALE    = AV_CODEC_FLAG_QSCALE;
  CODEC_FLAG_4MV       = AV_CODEC_FLAG_4MV;
  CODEC_FLAG_OUTPUT_CORRUPT = AV_CODEC_FLAG_OUTPUT_CORRUPT;
  CODEC_FLAG_QPEL      = AV_CODEC_FLAG_QPEL;
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
  CODEC_FLAG_PASS1              = AV_CODEC_FLAG_PASS1;
  CODEC_FLAG_PASS2              = AV_CODEC_FLAG_PASS2;
  CODEC_FLAG_GRAY               = AV_CODEC_FLAG_GRAY;
{$IFDEF FF_API_EMU_EDGE}
  (**
   * @deprecated edges are not used/required anymore. I.e. this flag is now always
   * set.
   *)
  CODEC_FLAG_EMU_EDGE           = $4000; ///< don't draw edges
{$IFEND}
  CODEC_FLAG_PSNR               = AV_CODEC_FLAG_PSNR;
  CODEC_FLAG_TRUNCATED          = AV_CODEC_FLAG_TRUNCATED;

{$IFDEF FF_API_NORMALIZE_AQP}
(**
 * @deprecated use the flag "naq" in the "mpv_flags" private option of the
 * mpegvideo encoders
 *)
  CODEC_FLAG_NORMALIZE_AQP  = $00020000; ///< normalize adaptive quantization
{$IFEND}
  CODEC_FLAG_INTERLACED_DCT = AV_CODEC_FLAG_INTERLACED_DCT;
  CODEC_FLAG_LOW_DELAY      = AV_CODEC_FLAG_LOW_DELAY;
  CODEC_FLAG_GLOBAL_HEADER  = AV_CODEC_FLAG_GLOBAL_HEADER;
  CODEC_FLAG_BITEXACT       = AV_CODEC_FLAG_BITEXACT;
  CODEC_FLAG_AC_PRED        = AV_CODEC_FLAG_AC_PRED;
  CODEC_FLAG_LOOP_FILTER    = AV_CODEC_FLAG_LOOP_FILTER;
  CODEC_FLAG_INTERLACED_ME  = AV_CODEC_FLAG_INTERLACED_ME;
  CODEC_FLAG_CLOSED_GOP     = AV_CODEC_FLAG_CLOSED_GOP;
  CODEC_FLAG2_FAST          = AV_CODEC_FLAG2_FAST;
  CODEC_FLAG2_NO_OUTPUT     = AV_CODEC_FLAG2_NO_OUTPUT;
  CODEC_FLAG2_LOCAL_HEADER  = AV_CODEC_FLAG2_LOCAL_HEADER;
  CODEC_FLAG2_DROP_FRAME_TIMECODE = AV_CODEC_FLAG2_DROP_FRAME_TIMECODE;
  CODEC_FLAG2_IGNORE_CROP   = AV_CODEC_FLAG2_IGNORE_CROP;

  CODEC_FLAG2_CHUNKS        = AV_CODEC_FLAG2_CHUNKS;
  CODEC_FLAG2_SHOW_ALL      = AV_CODEC_FLAG2_SHOW_ALL;
  CODEC_FLAG2_EXPORT_MVS    = AV_CODEC_FLAG2_EXPORT_MVS;
  CODEC_FLAG2_SKIP_MANUAL   = AV_CODEC_FLAG2_SKIP_MANUAL;

(* Unsupported options :
 *              Syntax Arithmetic coding (SAC)
 *              Reference Picture Selection
 *              Independant Segment Decoding *)
(* /Fx *)
(* codec capabilities *)

  CODEC_CAP_DRAW_HORIZ_BAND = AV_CODEC_CAP_DRAW_HORIZ_BAND; ///< decoder can use draw_horiz_band callback

(**
 * Codec uses get_buffer() for allocating buffers and supports custom allocators.
 * If not set, it might not use get_buffer() at all or use operations that
 * assume the buffer was allocated by avcodec_default_get_buffer.
 *)
  CODEC_CAP_DR1             = AV_CODEC_CAP_DR1;
  CODEC_CAP_TRUNCATED       = AV_CODEC_CAP_TRUNCATED;
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
  CODEC_CAP_DELAY           = AV_CODEC_CAP_DELAY;

(**
 * Codec can be fed a final frame with a smaller size.
 * This can be used to prevent truncation of the last audio samples.
 *)
  CODEC_CAP_SMALL_LAST_FRAME = AV_CODEC_CAP_SMALL_LAST_FRAME;

(**
 * Codec can export data for HW decoding (VDPAU).
 *)
  CODEC_CAP_HWACCEL_VDPAU    = AV_CODEC_CAP_HWACCEL_VDPAU;

(**
 * Codec can output multiple frames per AVPacket
 * Normally demuxers return one frame at a time, demuxers which do not do
 * are connected to a parser to split what they return into proper frames.
 * This flag is reserved to the very rare category of codecs which have a
 * bitstream that cannot be split into frames without timeconsuming
 * operations like full decoding. Demuxers carrying such bitstreams thus
 * may return multiple frames in a packet. This has many disadvantages like
 * prohibiting stream copy in many cases thus it should only be considered
 * as a last resort.
 *)
  CODEC_CAP_SUBFRAMES        = AV_CODEC_CAP_SUBFRAMES;

(**
 * Codec is experimental and is thus avoided in favor of non experimental
 * encoders
 *)
  CODEC_CAP_EXPERIMENTAL     = AV_CODEC_CAP_EXPERIMENTAL;

(**
 * Codec should fill in channel configuration and samplerate instead of container
 *)
  CODEC_CAP_CHANNEL_CONF     = AV_CODEC_CAP_CHANNEL_CONF;

{$IFDEF FF_API_NEG_LINESIZES}
(**
 * @deprecated no codecs use this capability
 *)
  CODEC_CAP_NEG_LINESIZES    = $0800;
{$IFEND}

(**
 * Codec supports frame-level multithreading.
 *)
  CODEC_CAP_FRAME_THREADS    = AV_CODEC_CAP_FRAME_THREADS;

(**
 * Codec supports slice-based (or partition-based) multithreading.
 *)
  CODEC_CAP_SLICE_THREADS    = AV_CODEC_CAP_SLICE_THREADS;

(**
 * Codec supports changed parameters at any point.
 *)
  CODEC_CAP_PARAM_CHANGE     = AV_CODEC_CAP_PARAM_CHANGE;

(**
 * Codec supports avctx->thread_count == 0 (auto).
 *)
  CODEC_CAP_AUTO_THREADS     = AV_CODEC_CAP_AUTO_THREADS;

(**
 * Audio encoder supports receiving a different number of samples in each call.
 *)
  CODEC_CAP_VARIABLE_FRAME_SIZE = AV_CODEC_CAP_VARIABLE_FRAME_SIZE;

(**
 * Codec is intra only.
 *)
  CODEC_CAP_INTRA_ONLY       = AV_CODEC_CAP_INTRA_ONLY;

(**
 * Codec is lossless.
 *)
  CODEC_CAP_LOSSLESS         = AV_CODEC_CAP_LOSSLESS;
{$ENDIF} (* FF_API_WITHOUT_PREFIX *)

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
   // Note bits 24-31 are reserved for codec specific use (h264 ref0, mpeg1 0mv, ...)
{$IFEND}

(** Note bits 24-31 are reserved for codec specific use (H.264 ref0, MPEG-1 0mv, ...) *)
   
{$IFDEF FF_API_QSCALE_TYPE}  
  FF_QSCALE_TYPE_MPEG1  = 0;
  FF_QSCALE_TYPE_MPEG2  = 1;
  FF_QSCALE_TYPE_H264   = 2;
  FF_QSCALE_TYPE_VP56   = 3;
{$ENDIF}
  
(**
 * The decoder will keep a reference to the frame and may reuse it later.
 *)
  AV_GET_BUFFER_FLAG_REF = 1 << 0;

  FF_COMPRESSION_DEFAULT = -1;
{$IFDEF FF_API_ASPECT_EXTENDED}
  FF_ASPECT_EXTENDED = 15;
{$ENDIF}
  
{$IFDEF FF_API_RC_STRATEGY}
  FF_RC_STRATEGY_XVID = 1;
{$ENDIF}

{$IFDEF FF_API_PRIVATE_OPT}
  FF_PRED_LEFT   = 0;
  FF_PRED_PLANE  = 1;
  FF_PRED_MEDIAN = 2;
{$ENDIF}

  FF_CMP_SAD        = 0;
  FF_CMP_SSE        = 1;
  FF_CMP_SATD       = 2;
  FF_CMP_DCT        = 3;
  FF_CMP_PSNR       = 4;
  FF_CMP_BIT        = 5;
  FF_CMP_RD         = 6;
  FF_CMP_ZERO       = 7;
  FF_CMP_VSAD       = 8;
  FF_CMP_VSSE       = 9;
  FF_CMP_NSSE       = 10;
  FF_CMP_W53        = 11;
  FF_CMP_W97        = 12;
  FF_CMP_DCTMAX     = 13;
  FF_CMP_DCT264     = 14;
  FF_CMP_MEDIAN_SAD = 15;
  FF_CMP_CHROMA     = 256;

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

{$IFDEF FF_API_QUANT_BIAS}
  FF_DEFAULT_QUANT_BIAS   = 999999;
{$ENDIF}

  SLICE_FLAG_CODED_ORDER    = $0001; ///< draw_horiz_band() is called in coded order instead of display
  SLICE_FLAG_ALLOW_FIELD    = $0002; ///< allow draw_horiz_band() with field slices (MPEG-2 field pics)
  SLICE_FLAG_ALLOW_PLANE    = $0004; ///< allow draw_horiz_band() with 1 component at a time (SVQ1)

  FF_MB_DECISION_SIMPLE = 0;        ///< uses mb_cmp
  FF_MB_DECISION_BITS   = 1;        ///< chooses the one which needs the fewest bits
  FF_MB_DECISION_RD     = 2;        ///< rate distortion

{$IFDEF FF_API_CODER_TYPE}
  FF_CODER_TYPE_VLC       = 0;
  FF_CODER_TYPE_AC        = 1;
  FF_CODER_TYPE_RAW       = 2;
  FF_CODER_TYPE_RLE       = 3;
{$IFDEF FF_API_UNUSED_MEMBERS}
  FF_CODER_TYPE_DEFLATE   = 4;
{$ENDIF} {FF_API_UNUSED_MEMBERS}
{$ENDIF} {FF_API_CODER_TYPE}
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
  FF_BUG_IEDGE            = 32768;

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
  FF_DEBUG_GREEN_MD     = $00800000;
  FF_DEBUG_NOMC         = $01000000;
  
  FF_DEBUG_VIS_MV_P_FOR  = $00000001; // visualize forward predicted MVs of P-frames
  FF_DEBUG_VIS_MV_B_FOR  = $00000002; // visualize forward predicted MVs of B-frames
  FF_DEBUG_VIS_MV_B_BACK = $00000004; // visualize backward predicted MVs of B-frames

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
  FF_DCT_INT     = 2;
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

  FF_PROFILE_DNXHD     = 0;
  FF_PROFILE_DNXHR_LB  = 1;
  FF_PROFILE_DNXHR_SQ  = 2;
  FF_PROFILE_DNXHR_HQ  = 3;
  FF_PROFILE_DNXHR_HQX = 4;
  FF_PROFILE_DNXHR_444 = 5;

  FF_PROFILE_DTS         = 20;
  FF_PROFILE_DTS_ES      = 30;
  FF_PROFILE_DTS_96_24   = 40;
  FF_PROFILE_DTS_HD_HRA  = 50;
  FF_PROFILE_DTS_HD_MA   = 60;
  FF_PROFILE_DTS_EXPRESS = 70;

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
  FF_PROFILE_H264_MULTIVIEW_HIGH       = 118;
  FF_PROFILE_H264_HIGH_422             = 122;
  FF_PROFILE_H264_HIGH_422_INTRA       = (122 or FF_PROFILE_H264_INTRA);
  FF_PROFILE_H264_STEREO_HIGH          = 128;
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

  FF_PROFILE_JPEG2000_CSTREAM_RESTRICTION_0  = 1;
  FF_PROFILE_JPEG2000_CSTREAM_RESTRICTION_1  = 2;
  FF_PROFILE_JPEG2000_CSTREAM_NO_RESTRICTION = 32768;
  FF_PROFILE_JPEG2000_DCINEMA_2K             = 3;
  FF_PROFILE_JPEG2000_DCINEMA_4K             = 4;

  FF_PROFILE_VP9_0                           = 0;
  FF_PROFILE_VP9_1                           = 1;
  FF_PROFILE_VP9_2                           = 2;
  FF_PROFILE_VP9_3                           = 3;

  FF_PROFILE_HEVC_MAIN                       = 1;
  FF_PROFILE_HEVC_MAIN_10                    = 2;
  FF_PROFILE_HEVC_MAIN_STILL_PICTURE         = 3;
  FF_PROFILE_HEVC_REXT                       = 4;


  FF_LEVEL_UNKNOWN    = -99;

type

  (**
   * This structure describes the bitrate properties of an encoded bitstream. It
   * roughly corresponds to a subset the VBV parameters for MPEG-2 or HRD
   * parameters for H.264/HEVC.
   *)
  PAVCPBProperties = ^TAVCPBProperties;
  TAVCPBProperties = record
    (**
     * Maximum bitrate of the stream, in bits per second.
     * Zero if unknown or unspecified.
     *)
    max_bitrate: cint;
    (**
     * Minimum bitrate of the stream, in bits per second.
     * Zero if unknown or unspecified.
     *)
    min_bitrate: cint;
    (**
     * Average bitrate of the stream, in bits per second.
     * Zero if unknown or unspecified.
     *)
    avg_bitrate: cint;

    (**
     * The size of the buffer to which the ratecontrol is applied, in bits.
     * Zero if unknown or unspecified.
     *)
    buffer_size: cint;

    (**
     * The delay between the time the packet this structure is associated with
     * is received and the time when it should be decoded, in periods of a 27MHz
     * clock.
     *
     * UINT64_MAX when unknown or unspecified.
     *)
    vbv_delay: cuint64;
  end; {TAVCPBProperties}

  TAVPacketSideDataType = (
    AV_PKT_DATA_PALETTE,

    (**
     * The AV_PKT_DATA_NEW_EXTRADATA is used to notify the codec or the format
     * that the extradata buffer was changed and the receiving side should
     * act upon it appropriately. The new extradata is embedded in the side
     * data buffer and should be immediately used for processing the current
     * frame or packet.
     *)
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
     *)
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
     * This side data should be associated with an audio stream and corresponds
     * to enum AVAudioServiceType.
     *)
    AV_PKT_DATA_AUDIO_SERVICE_TYPE,

    (**
     * This side data contains quality related information from the encoder.
     * @code
     * u32le quality factor of the compressed frame. Allowed range is between 1 (good) and FF_LAMBDA_MAX (bad).
     * u8    picture type
     * u8    error count
     * u16   reserved
     * u64le[error count] sum of squared differences between encoder in and output
     * @endcode
     *)
    AV_PKT_DATA_QUALITY_STATS,

    (**
     * This side data contains an integer value representing the stream index
     * of a "fallback" track.  A fallback track indicates an alternate
     * track to use when the current track can not be decoded for some reason.
     * e.g. no decoder available for codec.
     *)
    AV_PKT_DATA_FALLBACK_TRACK,

    (**
     * This side data corresponds to the AVCPBProperties struct.
     *)
    AV_PKT_DATA_CPB_PROPERTIES,

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
    AV_PKT_DATA_METADATA_UPDATE,

    (**
     * MPEGTS stream ID, this is required to pass the stream ID
     * information from the demuxer to the corresponding muxer.
     *)
    AV_PKT_DATA_MPEGTS_STREAM_ID,

    (**
     * Mastering display metadata (based on SMPTE-2086:2014). This metadata
     * should be associated with a video stream and containts data in the form
     * of the AVMasteringDisplayMetadata struct.
     *)
    AV_PKT_DATA_MASTERING_DISPLAY_METADATA
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
 * contain several compressed frames. Encoders are allowed to output empty
 * packets, with no compressed data, containing only side data
 * (e.g. to update some stream parameters at the end of encoding).
 *
 * AVPacket is one of the few structs in FFmpeg, whose size is a part of public
 * ABI. Thus it may be allocated on stack and no new fields can be added to it
 * without libavcodec and libavformat major bump.
 *
 * The semantics of data ownership depends on the buf field.
 * If it is set, the packet data is dynamically allocated and is
 * valid indefinitely until a call to av_packet_unref() reduces the
 * reference count to 0.
 *
 * If the buf field is not set av_packet_ref() would make a copy instead
 * of increasing the reference count.
 *
 * The side data is always allocated with av_malloc(), copied by
 * av_packet_ref() and freed by av_packet_unref().
 *
 * @see av_packet_ref
 * @see av_packet_unref
 *)
  PPAVPacket= ^PAVPacket;
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
    duration:     cint64;

    pos:          cint64;       // byte position in stream, -1 if unknown
    
{$IFDEF FF_API_CONVERGENCE_DURATION}
    (*
     * @deprecated Same as the duration field, but as int64_t. This was required
     * for Matroska subtitles, whose duration values could overflow when the
     * duration field was still an int.
     *)
    convergence_duration: cint64; {deprecated}
{$ENDIF}
  end; {TAVPacket}

const
  AV_PKT_FLAG_KEY     = $0001; ///< The packet contains a keyframe
  AV_PKT_FLAG_CORRUPT = $0002; ///< The packet content is corrupted
  (**
   * Flag is used to discard packets which are required to maintain valid
   * decoder state but are not required for output and should be dropped
   * after decoding.
   **)
  AV_PKT_FLAG_DISCARD = $0004;

  AV_NUM_DATA_POINTERS = 8;

  AV_PKT_DATA_QUALITY_FACTOR = AV_PKT_DATA_QUALITY_STATS; //DEPRECATED

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
 * The name string for AVOptions options matches the associated command line
 * parameter name and can be found in libavcodec/options_table.h
 * The AVOption/command line parameter names differ in some cases from the C
 * structure field names for historic reasons or brevity.
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

{$IFDEF FF_API_STREAM_CODEC_TAG}
    (**
     * @deprecated this field is unused
     *)
    {attribute_deprecated}
    stream_codec_tag: cuint;
{$IFEND}

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
     * - decoding: Set by user, may be overwritten by libavcodec
     *             if this info is available in the stream
     *)
    bit_rate: cint64;

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
     * AV_CODEC_FLAG_*.
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    flags: cint;

    (**
     * AV_CODEC_FLAG2_*
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    flags2: cint;

    (**
     * some codecs need / can use extradata like Huffman tables.
     * MJPEG: Huffman tables
     * rv10: additional flags
     * MPEG-4: global headers (they can be in the bitstream or here)
     * The allocated memory should be AV_FF_INPUT_BUFFER_PADDING_SIZE bytes larger
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
     * for video. 1/time_base is not the average frame rate if the frame rate is not
     * constant.
     *
     * Like containers, elementary streams also can store timestamps, 1/time_base
     * is the unit in which these timestamps are specified.
     * As example of such codec time base see ISO/IEC 14496-2:2001(E)
     * vop_time_increment_resolution and fixed_vop_rate
     * (fixed_vop_rate == 0 implies that it is different from the framerate)
     *
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
     *
     * @note Those fields may not match the values of the last
     * AVFrame output by avcodec_decode_video2 due frame
     * reordering.
     *
     * - encoding: MUST be set by user.
     * - decoding: May be set by the user before opening the decoder if known e.g.
     *             from the container. Some decoders will require the dimensions
     *             to be set by the caller. During decoding, the decoder may
     *             overwrite those values as required while parsing the data.
     *)
    width, height: cint;

    (**
     * Bitstream width / height, may be different from width/height e.g. when
     * the decoded frame is cropped before being output or lowres is enabled.
     *
     * @note Those field may not match the value of the last
     * AVFrame output by avcodec_receive_frame() due frame
     * reordering.
     *
     * - encoding: unused
     * - decoding: May be set by the user before opening the decoder if known
     *             e.g. from the container. During decoding, the decoder may
     *             overwrite those values as required while parsing the data.
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
     *
     * @note This field may not match the value of the last
     * AVFrame output by avcodec_receive_frame() due frame
     * reordering.
     *
     * - encoding: Set by user.
     * - decoding: Set by user if known, overridden by libavcodec while
     *             parsing the data.
     *)
    pix_fmt: TAVPixelFormat;

{$IFDEF FF_API_MOTION_EST}
    (**
     * This option does nothing
     *@deprecated use codec private options instead
     *)
    {attribute_deprecated}
    me_method: cint;
{$ENDIF}

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

{$IFDEF FF_API_RC_STRATEGY}
    (** @deprecated use codec private option instead *)
    {attribute_deprecated}
    rc_strategy: cint;
{$ENDIF}

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    b_frame_strategy: cint; {deprecated}
{$ENDIF}

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

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    mpeg_quant: cint; {deprecated}
{$ENDIF}

    (**
     * qscale factor between P- and I-frames
     * If > 0 then the last P- frame quantizer will be used (q = lastp_q * factor + offset).
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

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    prediction_method: cint; {deprecated}
{$ENDIF}

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

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    pre_me: cint; {deprecated}
{$ENDIF}

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

{$IFDEF FF_API_QUANT_BIAS}
    (**
     * @deprecated use encoder private option instead
     *)
    {attribute_deprecated}
    intra_quant_bias: cint;

    (**
     * @deprecated use encoder private option instead
     *)
    {attribute_deprecated}
    inter_quant_bias: cint;
{$ENDIF}

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

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    scenechange_threshold: cint; {deprecated}

    (** @deprecated use encoder private options instead *)
    noise_reduction: cint; {deprecated}
{$ENDIF}

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
     * - decoding: Set by libavcodec
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
     * minimum MB Lagrange multiplier
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mb_lmin: cint;

    (**
     * maximum MB Lagrange multiplier
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mb_lmax: cint;

{$IFDEF FF_API_PRIVATE_OPT}
    (**
     * @deprecated use encoder private options instead
     *)
    me_penalty_compensation: cint; {deprecated}
{$ENDIF}

    (**
     * - encoding: Set by user.
     * - decoding: unused
     *)
    bidir_refine: cint;

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    brd_scale: cint; {deprecated}
{$ENDIF}

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

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    chromaoffset: cint; {deprecated}
{$ENDIF}

{$IFDEF FF_API_UNUSED_MEMBERS}
    (**
     * Multiplied by qscale for each frame and added to scene_change_score.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    scenechange_factor: cint; {attribute_deprecated}
{$IFEND}

    (**
     * Note: Value depends upon the compare function used for fullpel ME.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mv0_threshold: cint;

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    b_sensitivity: cint; {deprecated}
{$ENDIF}

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
     *   May be 0 when the codec has AV_CODEC_CAP_VARIABLE_FRAME_SIZE set, then the
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
     *   * otherwise extended_data must point to data
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
     * If AV_CODEC_CAP_DR1 is not set then get_buffer2() must call
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
     * This is always automatically enabled if avcodec_receive_frame() is used.
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
     * - decoding: Set by user, may be overwritten by libavcodec.
     *)
    rc_max_rate: cint64;

    (**
     * minimum bitrate
     * - encoding: Set by user.
     * - decoding: unused
     *)
    rc_min_rate: cint64;

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

{$IFDEF FF_API_CODER_TYPE}
    (**
     * @deprecated use encoder private options instead
     *)
    coder_type: cint; {deprecated}
{$ENDIF}

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    context_model: cint; {deprecated}
{$ENDIF}

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

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    frame_skip_threshold: cint; {deprecated}

    (** @deprecated use encoder private options instead *)
    frame_skip_factor: cint; {deprecated}

    (** @deprecated use encoder private options instead *)
    frame_skip_exp: cint; {deprecated}

    (** @deprecated use encoder private options instead *)
    frame_skip_cmp: cint; {deprecated}
{$ENDIF}

    (**
     * trellis RD quantization
     * - encoding: Set by user.
     * - decoding: unused
     *)
    trellis: cint;

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    min_prediction_order: cint; {deprecated}

    (** @deprecated use encoder private options instead *)
    max_prediction_order: cint; {deprecated}

    (** @deprecated use encoder private options instead *)
    timecode_frame_start: cint64; {deprecated}
{$ENDIF}

{$IFDEF FF_API_RTP_CALLBACK}
    (**
     * @deprecated unused
     *)
    (* The RTP callback: This function is called   *)
    (* every time the encoder has a packet to send *)
    (* Depends on the encoder if the data starts   *)
    (* with a Start Code (it should) H.263 does.   *)
    (* mb_nb contains the number of macroblocks    *)
    (* encoded in the RTP payload                  *)
    rtp_callback: procedure (avctx: PAVCodecContext; data: pointer;
                             size: cint; mb_nb: cint); cdecl; {deprecated}
{$ENDIF}

{$IFDEF FF_API_PRIVATE_OPT}
    rtp_payload_size: cint; {deprecated} (* The size of the RTP payload: the coder will  *)
                                         (* do it's best to deliver a chunk with size    *)
                                         (* below rtp_payload_size, the chunk will start *)
                                         (* with a start code on some codecs like H.263  *)
                                         (* This doesn't take account of any particular  *)
                                         (* headers inside the transmited RTP payload    *)
{$ENDIF}

{$IFDEF FF_API_STAT_BITS}
    (* statistics, used for 2-pass encoding *)
    mv_bits: cint;  {deprecated}
    header_bits: cint; {deprecated}
    i_tex_bits: cint; {deprecated}
    p_tex_bits: cint; {deprecated}
    i_count: cint; {deprecated}
    p_count: cint; {deprecated}
    skip_count: cint; {deprecated}
    misc_bits: cint; {deprecated}

    (** @deprecated this field is unused *)
    frame_bits: cint; {deprecated}
{$ENDIF}

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
     * strictly follow the standard (MPEG-4, ...).
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
     * opaque 64-bit number (generally a PTS) that will be reordered and
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
     * - encoding: Set by libavcodec if flags & AV_CODEC_FLAG_PSNR.
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

{$IFDEF FF_API_CODED_FRAME}
    (**
     * the picture in the bitstream
     * - encoding: Set by libavcodec.
     * - decoding: unused
     *
     * @deprecated use the quality factor packet side data instead
     *)
    {attribute_deprecated}
    coded_frame: PAVFrame;
{$ENDIF}

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

{$IFDEF FF_API_VBV_DELAY}
    (**
     * VBV delay coded in the last frame (in periods of a 27 MHz clock).
     * Used for compliant TS muxing.
     * - encoding: Set by libavcodec.
     * - decoding: unused.
     * @deprecated this value is now exported as a part of
     * AV_PKT_DATA_CPB_PROPERTIES packet side data
     *)
    vbv_delay: cuint64; {deprecated}
{$ENDIF}

{$IFDEF FF_API_SIDEDATA_ONLY_PKT}
    (**
     * Encoding only and set by default. Allow encoders to output packets
     * that do not contain any encoded data, only side data.
     *
     * Some encoders need to output such packets, e.g. to update some stream
     * parameters at the end of encoding.
     *
     * @deprecated this field disables the default behaviour and
     *             it is kept only for compatibility.
     *)
    side_data_only_packets: cint; {deprecated}
{$ENDIF}

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
     * - encoding: May be used to signal the framerate of CFR content to an
     *             encoder.
     *)
    framerate: TAVRational;

    (**
     * Nominal unaccelerated pixel format, see AV_PIX_FMT_xxx.
     * - encoding: unused.
     * - decoding: Set by libavcodec before calling get_format()
     *)
    sw_pix_fmt: TAVPixelFormat;

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

    (*
     * Properties of the stream that gets decoded
     * To be accessed through av_codec_get_properties() (NO direct access)
     * - encoding: unused
     * - decoding: set by libavcodec
     *)
    properties: cuint;
    
    (**
     * Additional data associated with the entire coded stream.
     *
     * - decoding: unused
     * - encoding: may be set by libavcodec after avcodec_open2().
     *)
    coded_side_data: PAVPacketSideData;
    nb_coded_side_data: cint;

    (**
     * A reference to the AVHWFramesContext describing the input (for encoding)
     * or output (decoding) frames. The reference is set by the caller and
     * afterwards owned (and freed) by libavcodec.
     *
     * - decoding: This field should be set by the caller from the get_format()
     *             callback. The previous reference (if any) will always be
     *             unreffed by libavcodec before the get_format() call.
     *
     *             If the default get_buffer2() is used with a hwaccel pixel
     *             format, then this AVHWFramesContext will be used for
     *             allocating the frame buffers.
     *
     * - encoding: For hardware encoders configured to use a hwaccel pixel
     *             format, this field should be set by the caller to a reference
     *             to the AVHWFramesContext describing input frames.
     *             AVHWFramesContext.format must be equal to
     *             AVCodecContext.pix_fmt.
     *
     *             This field should be set before avcodec_open2() is called.
     *)
    hw_frames_ctx: PAVBufferRef;

    (**
     * Control the form of AVSubtitle.rects[N]->ass
     * - decoding: set by user
     * - encoding: unused
     *)
    sub_text_format: cint;

    (**
     * Audio only. The amount of padding (in samples) appended by the encoder to
     * the end of the audio. I.e. this number of decoded samples must be
     * discarded by the caller from the end of the stream to get the original
     * audio without any trailing padding.
     *
     * - decoding: unused
     * - encoding: unused
     *)
    trailing_padding: cint;

  end; {TAVCodecContext}

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

{$IFDEF FF_API_AVPICTURE}
  (**
   * @defgroup lavc_picture AVPicture
   *
   * Functions for working with AVPicture
   * @{
   *)

  (**
    * Picture data structure.
    *
    * Up to four components can be stored into it, the last component is
    * alpha.
    * @deprecated use AVFrame or imgutils functions instead
    *)
    PAVPicture = ^TAVPicture;
    TAVPicture = record
      data: array [0..AV_NUM_DATA_POINTERS - 1] of PByteArray; {deprecated}
      linesize: array [0..AV_NUM_DATA_POINTERS - 1] of cint; {deprecated} ///< number of bytes per line
    end; {TAVPicture}
{$ENDIF}

  PPAVSubtitleRect = ^PAVSubtitleRect;
  PAVSubtitleRect = ^TAVSubtitleRect;
  TAVSubtitleRect = record
    x: cint;        ///< top left corner  of pict, undefined when pict is not set
    y: cint;        ///< top left corner  of pict, undefined when pict is not set
    w: cint;        ///< width            of pict, undefined when pict is not set
    h: cint;        ///< height           of pict, undefined when pict is not set
    nb_colors: cint; ///< number of colors in pict, undefined when pict is not set

{$IFDEF FF_API_AVPICTURE}
    (**
     * @deprecated unused
     *)
    pict: TAVPicture; {deprecated}
{$ENDIF}
    (**
     * data+linesize for the bitmap of this subtitle.
     * Can be set for text/ass as well once they are rendered.
     *)
    data: Array [0..4] of PByte;
    linesize: Array [0..4] of cint;
    
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
   * This struct describes the properties of an encoded stream.
   *
   * sizeof(AVCodecParameters) is not a part of the public ABI, this struct must
   * be allocated with avcodec_parameters_alloc() and freed with
   * avcodec_parameters_free().
   *)
  PPAVCodecParameters = ^PAVCodecParameters;
  PAVCodecParameters  = ^TAVCodecParameters;
  TAVCodecParameters = record
    (**
     * General type of the encoded data.
     *)
    codec_type: TAVMediaType;
    (**
     * Specific type of the encoded data (the codec used).
     *)
    codec_id: TAVCodecID;
    (**
     * Additional information about the codec (corresponds to the AVI FOURCC).
     *)
    codec_tag: cuint32;

    (**
     * Extra binary data needed for initializing the decoder, codec-dependent.
     *
     * Must be allocated with av_malloc() and will be freed by
     * avcodec_parameters_free(). The allocated size of extradata must be at
     * least extradata_size + AV_INPUT_BUFFER_PADDING_SIZE, with the padding
     * bytes zeroed.
     *)
    extradata: Pcuint8;
    (**
     * Size of the extradata content in bytes.
     *)
    extradata_size: cint;

    (**
     * - video: the pixel format, the value corresponds to enum AVPixelFormat.
     * - audio: the sample format, the value corresponds to enum AVSampleFormat.
     *)
    format: cint;

    (**
     * The average bitrate of the encoded data (in bits per second).
     *)
    bit_rate: cint64;

    (**
     * The number of bits per sample in the codedwords.
     *
     * This is basically the bitrate per sample. It is mandatory for a bunch of
     * formats to actually decode them. It's the number of bits for one sample in
     * the actual coded bitstream.
     *
     * This could be for example 4 for ADPCM
     * For PCM formats this matches bits_per_raw_sample
     * Can be 0
     *)
    bits_per_coded_sample: cint;

    (**
     * This is the number of valid bits in each output sample. If the
     * sample format has more bits, the least significant bits are additional
     * padding bits, which are always 0. Use right shifts to reduce the sample
     * to its actual size. For example, audio formats with 24 bit samples will
     * have bits_per_raw_sample set to 24, and format set to AV_SAMPLE_FMT_S32.
     * To get the original sample use "(int32_t)sample >> 8"."
     *
     * For ADPCM this might be 12 or 16 or similar
     * Can be 0
     *)
    bits_per_raw_sample: cint;

    (**
     * Codec-specific bitstream restrictions that the stream conforms to.
     *)
    profile: cint;
    level: cint;

    (**
     * Video only. The dimensions of the video frame in pixels.
     *)
    width: cint;
    height: cint;

    (**
     * Video only. The aspect ratio (width / height) which a single pixel
     * should have when displayed.
     *
     * When the aspect ratio is unknown / undefined, the numerator should be
     * set to 0 (the denominator may have any value).
     *)
    sample_aspect_ratio: TAVRational;

    (**
     * Video only. The order of the fields in interlaced video.
     *)
    field_order: TAVFieldOrder;

    (**
     * Video only. Additional colorspace characteristics.
     *)
    color_range: TAVColorRange;
    color_primaries: TAVColorPrimaries;
    color_trc: TAVColorTransferCharacteristic;
    color_space: TAVColorSpace;
    chroma_location: TAVChromaLocation;

    (**
     * Video only. Number of delayed frames.
     *)
    video_delay: cint;

    (**
     * Audio only. The channel layout bitmask. May be 0 if the channel layout is
     * unknown or unspecified, otherwise the number of bits set must be equal to
     * the channels field.
     *)
    channel_layout: cuint64;
    (**
     * Audio only. The number of audio channels.
     *)
    channels: cint;
    (**
     * Audio only. The number of audio samples per second.
     *)
    sample_rate: cint;
    (**
     * Audio only. The number of bytes per coded audio frame, required by some
     * formats.
     *
     * Corresponds to nBlockAlign in WAVEFORMATEX.
     *)
    block_align: cint;
    (**
     * Audio only. Audio frame size, if known. Required by some formats to be static.
     *)
    frame_size: cint;

    (**
     * Audio only. The amount of padding (in samples) inserted by the encoder at
     * the beginning of the audio. I.e. this number of leading decoded samples
     * must be discarded by the caller to get the original audio without leading
     * padding.
     *)
    initial_padding: cint;
    (**
     * Audio only. The amount of padding (in samples) appended by the encoder to
     * the end of the audio. I.e. this number of decoded samples must be
     * discarded by the caller from the end of the stream to get the original
     * audio without any trailing padding.
     *)
    trailing_padding: cint;
    (**
     * Audio only. Number of samples to skip after a discontinuity.
     *)
    seek_preroll: cint;
  end; {TAVCodecParameters}

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
     * see AV_CODEC_CAP_*
     *)
    capabilities: cint;
    supported_framerates: {const} PAVRational; ///< array of supported framerates, or NULL if any, array is terminated by {0,0}
    pix_fmts: {const} PAVPixelFormat;          ///< array of supported pixel formats, or NULL if unknown, array is terminated by -1
    supported_samplerates: {const} PCint;      ///< array of supported audio samplerates, or NULL if unknown, array is terminated by 0
    sample_fmts: {const} PAVSampleFormatArray; ///< array of supported sample formats, or NULL if unknown, array is terminated by -1
    channel_layouts: {const} PCuint64;         ///< array of support channel layouts, or NULL if unknown. array is terminated by 0
    max_lowres: byte;                          ///< maximum value for lowres supported by the decoder, no direct access, use av_codec_get_max_lowres()
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
     * Decode/encode API with decoupled packet/frame dataflow. The API is the
     * same as the avcodec_ prefixed APIs (avcodec_send_frame() etc.), except
     * that:
     * - never called if the codec is closed or the wrong type,
     * - AVPacket parameter change side data is applied right before calling
     *   AVCodec->send_packet,
     * - if AV_CODEC_CAP_DELAY is not set, drain packets or frames are never sent,
     * - only one drain packet is ever passed down (until the next flush()),
     * - a drain AVPacket is always NULL (no need to check for avpkt->size).
     *)
    send_frame: function(avctx: PAVCodecContext; frame: {const} PAVFrame): cint; cdecl;
    send_packet: function(avctx: PAVCodecContext; frame: {const} PAVPacket): cint; cdecl;
    receive_frame: function(avctx: PAVCodecContext; frame: PAVFrame): cint; cdecl;
    receive_packet: function(avctx: PAVCodecContext; avpkt: PAVPacket): cint; cdecl;
    (**
     * Flush buffers.
     * Will be called when seeking
     *)
    flush: procedure (avctx: PAVCodecContext); cdecl;
    (**
     * Internal codec capabilities.
     * See FF_CODEC_CAP_* in internal.h
     *)
    caps_internal: cint;
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
     * see HWACCEL_CODEC_CAP_*
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
   *
   * It's generally a good idea to pass this flag unless you have a specific
   * reason not to, as hardware tends to under-report supported levels.
   *)
  AV_HWACCEL_FLAG_IGNORE_LEVEL 	  = (1 << 0);

  (**
   * Hardware acceleration can output YUV pixel formats with a different chroma
   * sampling than 4:2:0 and/or other than 8 bits per component.
   *)
  AV_HWACCEL_FLAG_ALLOW_HIGH_DEPTH = (1 << 1);

  FF_SUB_CHARENC_MODE_DO_NOTHING  = -1;  ///< do nothing (demuxer outputs a stream supposed to be already in UTF-8, or the codec is bitmap for instance)
  FF_SUB_CHARENC_MODE_AUTOMATIC   = 0;   ///< libavcodec will select the mode itself
  FF_SUB_CHARENC_MODE_PRE_DECODER = 1;   ///< the AVPacket data needs to be recoded to UTF-8 before being fed to the decoder, requires iconv

  FF_CODEC_PROPERTY_LOSSLESS        = $00000001;
  FF_CODEC_PROPERTY_CLOSED_CAPTIONS = $00000002;
  FF_SUB_TEXT_FMT_ASS               = 0;
{$IFDEF FF_API_ASS_TIMING}
  FF_SUB_TEXT_FMT_ASS_WITH_TIMINGS  = 1;
{$ENDIF}

  function  av_codec_get_pkt_timebase(avctx: {const} PAVCodecContext): TAVRational;
    cdecl; external av__codec;
  procedure av_codec_set_pkt_timebase(avctx: {const} PAVCodecContext; val: TAVRational);
    cdecl; external av__codec;

  function  av_codec_get_codec_descriptor(avctx: {const} PAVCodecContext): PAVCodecDescriptor;
    cdecl; external av__codec;
  procedure av_codec_set_codec_descriptor(avctx: {const} PAVCodecContext; desc: {const} PAVCodecDescriptor);
    cdecl; external av__codec;

  function av_codec_get_codec_properties(avctx: {const} PAVCodecContext): cuint;
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
 *)
function avcodec_alloc_context3(codec: {const} PAVCodec): PAVCodecContext;
  cdecl; external av__codec;

(**
 * Free the codec context and everything associated with it and write NULL to
 * the provided pointer.
 *)
procedure avcodec_free_context(var avctx: PAVCodecContext);
  cdecl; external av__codec;

{$IFDEF FF_API_GET_CONTEXT_DEFAULTS}
(**
 * @deprecated This function should not be used, as closing and opening a codec
 * context multiple time is not supported. A new codec context should be
 * allocated for each new use.
 *)
procedure avcodec_get_context_defaults3(s: PAVCodecContext; codec: {const} PAVCodec);
  cdecl; external av__codec; deprecated;
{$ENDIF}

(**
 * Get the AVClass for AVCodecContext. It can be used in combination with
 * AV_OPT_SEARCH_FAKE_OBJ for examining options.
 *
 * @see av_opt_find().
 *)
function avcodec_get_class(): {const} PAVClass;
  cdecl; external av__codec;

{$IFDEF FF_API_COPY_CONTEXT}
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
 *
 * @deprecated The semantics of this function are ill-defined and it should not
 * be used. If you need to transfer the stream parameters from one codec context
 * to another, use an intermediate AVCodecParameters instance and the
 * avcodec_parameters_from_context() / avcodec_parameters_to_context()
 * functions.
 *)
function avcodec_copy_context(dest: PAVCodecContext; src: {const} PAVCodecContext): cint;
  cdecl; external av__codec; deprecated;
{$ENDIF}

(**
 * Allocate a new AVCodecParameters and set its fields to default values
 * (unknown/invalid/0). The returned struct must be freed with
 * avcodec_parameters_free().
 *)
function avcodec_parameters_alloc(): PAVCodecParameters;
  cdecl; external av__codec;

(**
 * Free an AVCodecParameters instance and everything associated with it and
 * write NULL to the supplied pointer.
 *)
procedure avcodec_parameters_free(par: PPAVCodecParameters);
  cdecl; external av__codec;

(**
 * Copy the contents of src to dst. Any allocated fields in dst are freed and
 * replaced with newly allocated duplicates of the corresponding fields in src.
 *
 * @return >= 0 on success, a negative AVERROR code on failure.
 *)
function avcodec_parameters_copy(dst: PAVCodecParameters; src: {const} PAVCodecParameters): cint;
  cdecl; external av__codec;

(**
 * Fill the parameters struct based on the values from the supplied codec
 * context. Any allocated fields in par are freed and replaced with duplicates
 * of the corresponding fields in codec.
 *
 * @return >= 0 on success, a negative AVERROR code on failure
 *)
function avcodec_parameters_from_context(par: PAVCodecParameters;
                                         codec: {const} PAVCodecContext): cint;
  cdecl; external av__codec;

(**
 * Fill the codec context based on the values from the supplied codec
 * parameters. Any allocated fields in codec that have a corresponding field in
 * par are freed and replaced with duplicates of the corresponding field in par.
 * Fields in codec that do not have a counterpart in par are not touched.
 *
 * @return >= 0 on success, a negative AVERROR code on failure.
 *)
function avcodec_parameters_to_context(codec: PAVCodecContext;
                                       par: {const} PAVCodecParameters): cint;
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
 * @note Always call this function before using decoding routines (such as
 * @ref avcodec_receive_frame()).
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
 *              for this context, then this parameter MUST be either NULL or
 *              equal to the previously passed codec.
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
 * the codec-specific data allocated in avcodec_alloc_context3() with a non-NULL
 * codec. Subsequent calls will do nothing.
 *
 * @note Do not use this function. Use avcodec_free_context() to destroy a
 * codec context (either open or closed). Opening and closing a codec context
 * multiple times is not supported anymore -- use multiple codec contexts
 * instead.
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

(**
 * Allocate an AVPacket and set its fields to default values.  The resulting
 * struct must be freed using av_packet_free().
 *
 * @return An AVPacket filled with default values or NULL on failure.
 *
 * @note this only allocates the AVPacket itself, not the data buffers. Those
 * must be allocated through other means such as av_new_packet.
 *
 * @see av_new_packet
 *)
function av_packet_alloc(): PAVPacket;
  cdecl; external av__codec;
  
(**
 * Create a new packet that references the same data as src.
 *
 * This is a shortcut for av_packet_alloc()+av_packet_ref().
 *
 * @return newly created AVPacket on success, NULL on error.
 *
 * @see av_packet_alloc
 * @see av_packet_ref
 *)
function av_packet_clone(src: PAVPacket): PAVPacket;
  cdecl; external av__codec;

(**
 * Free the packet, if the packet is reference counted, it will be
 * unreferenced first.
 *
 * @param packet packet to be freed. The pointer will be set to NULL.
 * @note passing NULL is a no-op.
 *)
procedure av_packet_free(pkt: PPAVPacket);
  cdecl; external av__codec;

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
 *        size is assumed to be size + AV_INPUT_BUFFER_PADDING_SIZE.
 *
 * @return 0 on success, a negative AVERROR on error
 *)
function av_packet_from_data(pkt: PAVPacket; data: PByte; size: cint): cint;
  cdecl; external av__codec;

{$IFDEF FF_API_AVPACKET_OLD_API}
(*
 * @warning This is a hack - the packet memory allocation stuff is broken. The
 * packet is allocated if it was not really allocated.
 *
 * @deprecated Use av_packet_ref
 *)
function av_dup_packet(pkt: PAVPacket): cint;
  cdecl; external av__codec; deprecated;
{$ENDIF}

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
 * @deprecated Use av_packet_unref
 *
 * @param pkt packet to free
 *)
procedure av_free_packet(pkt: PAVPacket);
  cdecl; external av__codec; deprecated;

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
 * Wrap an existing array as a packet side data.
 *
 * @param pkt packet
 * @param type side information type
 * @param data the side data array. It must be allocated with the av_malloc()
 *             family of functions. The ownership of the data is transferred to
 *             pkt.
 * @param size side information size
 * @return a non-negative number on success, a negative AVERROR code on
 *         failure. On failure, the packet is unchanged and the data remains
 *         owned by the caller.
 *)
function av_packet_add_side_data(pkt: PAVPacket; type_: TAVPacketSideDataType;
                                 data: Pcuint8; size: size_t): cint;
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

function av_packet_side_data_name(type_: TAVPacketSideDataType): PAnsiChar;
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

(**
 * The default callback for AVCodecContext.get_buffer2(). It is made public so
 * it can be called by custom get_buffer2() implementations for decoders without
 * AV_CODEC_CAP_DR1 set.
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
 * May only be used if a codec with AV_CODEC_CAP_DR1 has been opened.
 *)
procedure avcodec_align_dimensions(s: PAVCodecContext; width: PCint; height: PCint);
  cdecl; external av__codec;

(**
 * Modifiy width and height values so that they will result in a memory
 * buffer that is acceptable for the codec if you also ensure that all
 * line sizes are a multiple of the respective linesize_align[i].
 *
 * May only be used if a codec with AV_CODEC_CAP_DR1 has been opened.
 *)
procedure avcodec_align_dimensions2(s: PAVCodecContext; width: PCint; height: PCint;
                                    linesize_align: PAVNDPArray);
  cdecl; external av__codec;

(**
 * Decode the audio frame of size avpkt->size from avpkt->data into frame.
 *
 * Some decoders may support multiple frames in a single AVPacket. Such
 * decoders would then just decode the first frame and the return value would be
 * less than the packet size. In this case, avcodec_decode_audio4 has to be
 * called again with an AVPacket containing the remaining data in order to
 * decode the second frame, etc...  Even if no frames are returned, the packet
 * needs to be fed to the decoder with remaining data until it is completely
 * consumed or an error occurs.
 *
 * Some decoders (those marked with AV_CODEC_CAP_DELAY) have a delay between input
 * and output. This means that for some packets they will not immediately
 * produce decoded output and need to be flushed at the end of decoding to get
 * all the decoded data. Flushing is done by calling this function with packets
 * with avpkt->data set to NULL and avpkt->size set to 0 until it stops
 * returning samples. It is safe to flush even those decoders that are not
 * marked with AV_CODEC_CAP_DELAY, then no samples will be returned.
 *
 * @warning The input buffer, avpkt->data must be AV_INPUT_BUFFER_PADDING_SIZE
 *          larger than the actual read bytes because some optimized bitstream
 *          readers read 32 or 64 bits at once and could read over the end.
 *
 * @note The AVCodecContext MUST have been opened with @ref avcodec_open2()
 * before packets may be fed to the decoder.
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
 *                   next call to this function or until closing or flushing the
 *                   decoder. The caller may not write to it.
 * @param[out] got_frame_ptr Zero if no frame could be decoded, otherwise it is
 *                           non-zero. Note that this field being set to zero
 *                           does not mean that an error has occurred. For
 *                           decoders with AV_CODEC_CAP_DELAY set, no given decode
 *                           call is guaranteed to produce a frame.
 * @param[in]  avpkt The input AVPacket containing the input buffer.
 *                   At least avpkt->data and avpkt->size should be set. Some
 *                   decoders might also require additional fields to be set.
 * @return A negative error code is returned if an error occurred during
 *         decoding, otherwise the number of bytes consumed from the input
 *         AVPacket is returned.
 *
 * @deprecated Use avcodec_send_packet() and avcodec_receive_frame().
 *)
function avcodec_decode_audio4(avctx: PAVCodecContext; frame: PAVFrame;
                       got_frame_ptr: Pcint; avpkt: PAVPacket): cint;
  cdecl; external av__codec; deprecated;

(**
 * Decode the video frame of size avpkt->size from avpkt->data into picture.
 * Some decoders may support multiple frames in a single AVPacket, such
 * decoders would then just decode the first frame.
 *
 * @warning The input buffer must be AV_INPUT_BUFFER_PADDING_SIZE larger than
 * the actual read bytes because some optimized bitstream readers read 32 or 64
 * bits at once and could read over the end.
 *
 * @warning The end of the input buffer buf should be set to 0 to ensure that
 * no overreading happens for damaged MPEG streams.
 *
 * @note Codecs which have the AV_CODEC_CAP_DELAY capability set have a delay
 * between input and output, these need to be fed with avpkt->data=NULL,
 * avpkt->size=0 at the end to return the remaining frames.
 *
 * @note The AVCodecContext MUST have been opened with @ref avcodec_open2()
 * before packets may be fed to the decoder.
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
 *             next call to this function or until closing or flushing the
 *             decoder. The caller may not write to it.
 *
 * @param[in] avpkt The input AVPacket containing the input buffer.
 *            You can create such packet with av_init_packet() and by then setting
 *            data and size, some decoders might in addition need other fields like
 *            flags&AV_PKT_FLAG_KEY. All decoders are designed to use the least
 *            fields possible.
 * @param[in,out] got_picture_ptr Zero if no frame could be decompressed, otherwise, it is nonzero.
 * @return On error a negative value is returned, otherwise the number of bytes
 * used or zero if no frame could be decompressed.
 *
 * @deprecated Use avcodec_send_packet() and avcodec_receive_frame().
 *)
function avcodec_decode_video2(avctx: PAVCodecContext; picture: PAVFrame;
                       var got_picture_ptr: cint;
                       avpkt: {const} PAVPacket): cint;
  cdecl; external av__codec; deprecated;

(*
 * Decode a subtitle message.
 * Return a negative value on error, otherwise return the number of bytes used.
 * If no subtitle could be decompressed, got_sub_ptr is zero.
 * Otherwise, the subtitle is stored in *sub.
 * Note that AV_CODEC_CAP_DR1 is not available for subtitle codecs. This is for
 * simplicity, because the performance difference is expect to be negligible
 * and reusing a get_buffer written for video codecs would probably perform badly
 * due to a potentially very different allocation pattern.
 *
 * Some decoders (those marked with CODEC_CAP_DELAY) have a delay between input
 * and output. This means that for some packets they will not immediately
 * produce decoded output and need to be flushed at the end of decoding to get
 * all the decoded data. Flushing is done by calling this function with packets
 * with avpkt->data set to NULL and avpkt->size set to 0 until it stops
 * returning subtitles. It is safe to flush even those decoders that are not
 * marked with CODEC_CAP_DELAY, then no subtitles will be returned.
 *
 * @note The AVCodecContext MUST have been opened with @ref avcodec_open2()
 * before packets may be fed to the decoder.
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
 * Supply raw packet data as input to a decoder.
 *
 * Internally, this call will copy relevant AVCodecContext fields, which can
 * influence decoding per-packet, and apply them when the packet is actually
 * decoded. (For example AVCodecContext.skip_frame, which might direct the
 * decoder to drop the frame contained by the packet sent with this function.)
 *
 * @warning The input buffer, avpkt->data must be AV_INPUT_BUFFER_PADDING_SIZE
 *          larger than the actual read bytes because some optimized bitstream
 *          readers read 32 or 64 bits at once and could read over the end.
 *
 * @warning Do not mix this API with the legacy API (like avcodec_decode_video2())
 *          on the same AVCodecContext. It will return unexpected results now
 *          or in future libavcodec versions.
 *
 * @note The AVCodecContext MUST have been opened with @ref avcodec_open2()
 *       before packets may be fed to the decoder.
 *
 * @param avctx codec context
 * @param[in] avpkt The input AVPacket. Usually, this will be a single video
 *                  frame, or several complete audio frames.
 *                  Ownership of the packet remains with the caller, and the
 *                  decoder will not write to the packet. The decoder may create
 *                  a reference to the packet data (or copy it if the packet is
 *                  not reference-counted).
 *                  Unlike with older APIs, the packet is always fully consumed,
 *                  and if it contains multiple frames (e.g. some audio codecs),
 *                  will require you to call avcodec_receive_frame() multiple
 *                  times afterwards before you can send a new packet.
 *                  It can be NULL (or an AVPacket with data set to NULL and
 *                  size set to 0); in this case, it is considered a flush
 *                  packet, which signals the end of the stream. Sending the
 *                  first flush packet will return success. Subsequent ones are
 *                  unnecessary and will return AVERROR_EOF. If the decoder
 *                  still has frames buffered, it will return them after sending
 *                  a flush packet.
 *
 * @return 0 on success, otherwise negative error code:
 *      AVERROR(EAGAIN):   input is not accepted right now - the packet must be
 *                         resent after trying to read output
 *      AVERROR_EOF:       the decoder has been flushed, and no new packets can
 *                         be sent to it (also returned if more than 1 flush
 *                         packet is sent)
 *      AVERROR(EINVAL):   codec not opened, it is an encoder, or requires flush
 *      AVERROR(ENOMEM):   failed to add packet to internal queue, or similar
 *      other errors: legitimate decoding errors
 *)
function avcodec_send_packet(avctx: PAVCodecContext; avpkt: {const} PAVPacket): cint;
  cdecl; external av__codec;

(**
 * Return decoded output data from a decoder.
 *
 * @param avctx codec context
 * @param frame This will be set to a reference-counted video or audio
 *              frame (depending on the decoder type) allocated by the
 *              decoder. Note that the function will always call
 *              av_frame_unref(frame) before doing anything else.
 *
 * @return
 *      0:                 success, a frame was returned
 *      AVERROR(EAGAIN):   output is not available right now - user must try
 *                         to send new input
 *      AVERROR_EOF:       the decoder has been fully flushed, and there will be
 *                         no more output frames
 *      AVERROR(EINVAL):   codec not opened, or it is an encoder
 *      other negative values: legitimate decoding errors
 *)
function avcodec_receive_frame(avctx: PAVCodecContext; frame: PAVFrame): cint;
  cdecl; external av__codec;

(**
 * Supply a raw video or audio frame to the encoder. Use avcodec_receive_packet()
 * to retrieve buffered output packets.
 *
 * @param avctx     codec context
 * @param[in] frame AVFrame containing the raw audio or video frame to be encoded.
 *                  Ownership of the frame remains with the caller, and the
 *                  encoder will not write to the frame. The encoder may create
 *                  a reference to the frame data (or copy it if the frame is
 *                  not reference-counted).
 *                  It can be NULL, in which case it is considered a flush
 *                  packet.  This signals the end of the stream. If the encoder
 *                  still has packets buffered, it will return them after this
 *                  call. Once flushing mode has been entered, additional flush
 *                  packets are ignored, and sending frames will return
 *                  AVERROR_EOF.
 *
 *                  For audio:
 *                  If AV_CODEC_CAP_VARIABLE_FRAME_SIZE is set, then each frame
 *                  can have any number of samples.
 *                  If it is not set, frame->nb_samples must be equal to
 *                  avctx->frame_size for all frames except the last.
 *                  The final frame may be smaller than avctx->frame_size.
 * @return 0 on success, otherwise negative error code:
 *      AVERROR(EAGAIN):   input is not accepted right now - the frame must be
 *                         resent after trying to read output packets
 *      AVERROR_EOF:       the encoder has been flushed, and no new frames can
 *                         be sent to it
 *      AVERROR(EINVAL):   codec not opened, refcounted_frames not set, it is a
 *                         decoder, or requires flush
 *      AVERROR(ENOMEM):   failed to add packet to internal queue, or similar
 *      other errors: legitimate decoding errors
 *)
function avcodec_send_frame(avctx: PAVCodecContext; frame: {const} PAVFrame): cint;
  cdecl; external av__codec;

(**
 * Read encoded data from the encoder.
 *
 * @param avctx codec context
 * @param avpkt This will be set to a reference-counted packet allocated by the
 *              encoder. Note that the function will always call
 *              av_frame_unref(frame) before doing anything else.
 * @return 0 on success, otherwise negative error code:
 *      AVERROR(EAGAIN):   output is not available right now - user must try
 *                         to send input
 *      AVERROR_EOF:       the encoder has been fully flushed, and there will be
 *                         no more output packets
 *      AVERROR(EINVAL):   codec not opened, or it is an encoder
 *      other errors: legitimate decoding errors
 *)
function avcodec_receive_packet(avctx: PAVCodecContext; avpkt: PAVPacket): cint;
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

{$IFDEF FF_API_CONVERGENCE_DURATION}
    (**
     * @deprecated unused
     *)
    convergence_duration: cint64; {deprecated}
{$ENDIF}

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

    (**
     * Picture number incremented in presentation or output order.
     * This field may be reinitialized at the first picture of a new sequence.
     *
     * For example, this corresponds to H.264 PicOrderCnt.
     *)
    output_picture_number: cint;

    (**
     * Dimensions of the decoded video intended for presentation.
     *)
    width: cint;
    height: cint;

    (**
     * Dimensions of the coded video.
     *)
    coded_width: cint;
    coded_height: cint;

    (**
     * The format of the coded data, corresponds to enum AVPixelFormat for video
     * and for enum AVSampleFormat for audio.
     *
     * Note that a decoder can have considerable freedom in how exactly it
     * decodes the data, so the format reported here might be different from the
     * one returned by a decoder.
     *)
    format: cint;
  end; {AVCodecParserContext}

  TAVCodecParser = record
    codec_ids: array [0..4] of cint; (* several codec IDs are permitted *)
    priv_data_size: cint;
    parser_init: function(s: PAVCodecParserContext): cint; cdecl;
    (* This callback never returns an error, a negative value means that
     * the frame start was in a previous packet. *)
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
 * @param buf_size      buffer size in bytes without the padding. I.e. the full buffer
                        size is assumed to be buf_size + AV_INPUT_BUFFER_PADDING_SIZE.
                        To signal EOF, this should be 0 (so that the last frame
                        can be output).
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
 *                  freed using av_packet_unref().
 * @param[in] frame AVFrame containing the raw audio data to be encoded.
 *                  May be NULL when flushing an encoder that has the
 *                  AV_CODEC_CAP_DELAY capability set.
 *                  If AV_CODEC_CAP_VARIABLE_FRAME_SIZE is set, then each frame
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
 *
 * @deprecated use avcodec_send_frame()/avcodec_receive_packet() instead
 *)
function avcodec_encode_audio2(avctx: PAVCodecContext; avpkt: PAVPacket;
                          frame: {const} PAVFrame; got_packet_ptr: Pcint): cint;
  cdecl; external av__codec; deprecated;

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
 *                  freed using av_packet_unref().
 * @param[in] frame AVFrame containing the raw video data to be encoded.
 *                  May be NULL when flushing an encoder that has the
 *                  AV_CODEC_CAP_DELAY capability set.
 * @param[out] got_packet_ptr This field is set to 1 by libavcodec if the
 *                            output packet is non-empty, and to 0 if it is
 *                            empty. If the function returns an error, the
 *                            packet can be assumed to be invalid, and the
 *                            value of got_packet_ptr is undefined and should
 *                            not be used.
 * @return          0 on success, negative error code on failure
 *
 * @deprecated use avcodec_send_frame()/avcodec_receive_packet() instead
 *)
function avcodec_encode_video2(avctx: PAVCodecContext; avpkt: PAVPacket;
                      frame: {const} PAVFrame; got_packet_ptr: Pcint): cint;
  cdecl; external av__codec; deprecated;

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

{$IFDEF FF_API_AVPICTURE}
(**
 * @addtogroup lavc_picture
 * @{
 *)

(**
  * @deprecated unused
  *)
function avpicture_alloc (picture: PAVPicture; pix_fmt: TAVPixelFormat;
                          width: cint; height: cint): cint;
  cdecl; external av__codec; deprecated;

(**
  * @deprecated unused
  *)
procedure avpicture_free (picture: PAVPicture);
  cdecl; external av__codec; deprecated;

(**
  * @deprecated use av_image_fill_arrays() instead.
  *)
function avpicture_fill (picture: PAVPicture; ptr: pcuint8;
                 pix_fmt: TAVPixelFormat; width: cint; height: cint): cint;
  cdecl; external av__codec; deprecated;

(**
  * @deprecated use av_image_copy_to_buffer() instead.
  *)
function avpicture_layout (src: {const} PAVPicture; pix_fmt: TAVPixelFormat;
                   width: cint; height: cint;
                   dest: PByteArray; dest_size: cint): cint;
  cdecl; external av__codec; deprecated;

(**
  * @deprecated use av_image_get_buffer_size() instead.
  *)
function avpicture_get_size (pix_fmt: TAVPixelFormat; width: cint; height: cint): cint;
  cdecl; external av__codec; deprecated;

(**
  * @deprecated av_image_copy() instead.
  *)
procedure av_picture_copy(dst: PAVPicture; src: {const} PAVPicture;
              pix_fmt: TAVPixelFormat; width: cint; height: cint);
  cdecl; external av__codec; deprecated;

(**
  * @deprecated unused
  *)
function av_picture_crop(dst: PAVPicture; src: {const} PAVPicture;
              pix_fmt: TAVPixelFormat; top_band: cint; left_band: cint): cint;
  cdecl; external av__codec; deprecated;

(**
  * @deprecated unused
  *)
function av_picture_pad(dst: PAVPicture; src: {const} PAVPicture;
            height: cint; width: cint; pix_fmt: TAVPixelFormat;
            padtop: cint;  padbottom: cint; padleft: cint;
            padright: cint; color: PCint): cint;
  cdecl; external av__codec; deprecated;

(**
 * @}
 *)
{$ENDIF}

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

function avcodec_find_best_pix_fmt2(dst_pix_fmt1: TAVPixelFormat; dst_pix_fmt2: TAVPixelFormat;
                      src_pix_fmt: TAVPixelFormat; has_alpha: cint; loss_ptr: Pcint): TAVPixelFormat;
  cdecl; external av__codec; deprecated;

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

(**
 * Return a name for the specified profile, if available.
 *
 * @param codec_id the ID of the codec to which the requested profile belongs
 * @param profile the profile value for which a name is requested
 * @return A name for the profile if found, NULL otherwise.
 *
 * @note unlike av_get_profile_name(), which searches a list of profiles
 *       supported by a specific decoder or encoder implementation, this
 *       function searches the list of profiles from the AVCodecDescriptor
 *)
function avcodec_profile_name(codec_id: TAVCodecID; profile: cint): {const} PChar;
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

(**
 * This function is the same as av_get_audio_frame_duration(), except it works
 * with AVCodecParameters instead of an AVCodecContext.
 *)
function av_get_audio_frame_duration2(par: PAVCodecParameters; frame_bytes: cint): cint;
  cdecl; external av__codec;

type
{$IFDEF FF_API_OLD_BSF}
  PAVBitStreamFilterContext = ^TAVBitStreamFilterContext;
  PAVBitStreamFilter = ^TAVBitStreamFilter;

  TAVBitStreamFilterContext = record
    priv_data: pointer;
    filter: PAVBitStreamFilter;
    parser: PAVCodecParserContext;
    next: PAVBitStreamFilterContext;
    (**
     * Internal default arguments, used if NULL is passed to av_bitstream_filter_filter().
     * Not for access by library users.
     *)
    args: PChar;
  end;
{$ENDIF}

  PAVBSFInternal = ^TAVBSFInternal;
  TAVBSFInternal = record
  end;
  
  (**
   * The bitstream filter state.
   *
   * This struct must be allocated with av_bsf_alloc() and freed with
   * av_bsf_free().
   *
   * The fields in the struct will only be changed (by the caller or by the
   * filter) as described in their documentation, and are to be considered
   * immutable otherwise.
   *)
  PPAVBSFContext = ^PAVBSFContext;
  PAVBSFContext  = ^TAVBSFContext;
  TAVBSFContext  = record
    (**
     * A class for logging and AVOptions
     *)
    av_class: {const} PAVClass;

    (**
     * The bitstream filter this context is an instance of.
     *)
    filter: {const} PAVBitStreamFilter;

    (**
     * Opaque libavcodec internal data. Must not be touched by the caller in any
     * way.
     *)
    internal: PAVBSFInternal;

    (**
     * Opaque filter-specific private data. If filter->priv_class is non-NULL,
     * this is an AVOptions-enabled struct.
     *)
    priv_data: pointer;

    (**
     * Parameters of the input stream. Set by the caller before av_bsf_init().
     *)
    par_in: PAVCodecParameters;

    (**
     * Parameters of the output stream. Set by the filter in av_bsf_init().
     *)
    par_out: PAVCodecParameters;

    (**
     * The timebase used for the timestamps of the input packets. Set by the
     * caller before av_bsf_init().
     *)
    time_base_in: TAVRational;

    (**
     * The timebase used for the timestamps of the output packets. Set by the
     * filter in av_bsf_init().
     *)
    time_base_out: TAVRational;
  end; {TAVBSFContext}

  TAVBitStreamFilter = record
    name: PAnsiChar;

    (**
     * A list of codec ids supported by the filter, terminated by
     * AV_CODEC_ID_NONE.
     * May be NULL, in that case the bitstream filter works with any codec id.
     *)
    codec_ids: PAVCodecID;

    (**
     * A class for the private data, used to declare bitstream filter private
     * AVOptions. This field is NULL for bitstream filters that do not declare
     * any options.
     *
     * If this field is non-NULL, the first member of the filter private data
     * must be a pointer to AVClass, which will be set by libavcodec generic
     * code to this class.
     *)
    priv_class: PAVClass;

    (*****************************************************************
     * No fields below this line are part of the public API. They
     * may not be used outside of libavcodec and can be changed and
     * removed at will.
     * New public fields should be added right above.
     *****************************************************************
     *)

    priv_data_size: cint;
    init: function(ctx: PAVBSFContext): cint; cdecl;
    filter: function(ctx: PAVBSFContext; pkt: PAVPacket): cint; cdecl;
    close: procedure(ctx: PAVBSFContext); cdecl;
  end;

  PPAVBSFList = ^PAVBSFList;
  PAVBSFList  = ^TAVBSFList;
  TAVBSFList  = record
  end;

{$IFDEF FF_API_OLD_BSF}
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
  cdecl; external av__codec; deprecated;

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
  cdecl; external av__codec; deprecated;

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
 * If the return value is 0, the output buffer is not allocated and
 * should be considered identical to the input buffer, or in case
 * *poutbuf was set it points to the input buffer (not necessarily to
 * its starting address). A special case is if *poutbuf was set to NULL and
 * *poutbuf_size was set to 0, which indicates the packet should be dropped.
 *)
function av_bitstream_filter_filter(bsfc: PAVBitStreamFilterContext;
                               avctx: PAVCodecContext; args: {const} PAnsiChar;
                               poutbuf: PPointer; poutbuf_size: PCint;
                               buf: {const} PByte; buf_size: cint; keyframe: cint): cint;
  cdecl; external av__codec; deprecated;

(**
 * Release bitstream filter context.
 *
 * @param bsf the bitstream filter context created with
 * av_bitstream_filter_init(), can be NULL
 *)
procedure av_bitstream_filter_close(bsf: PAVBitStreamFilterContext);
  cdecl; external av__codec; deprecated;

(**
 * If f is NULL, return the first registered bitstream filter,
 * if f is non-NULL, return the next registered bitstream filter
 * after f, or NULL if f is the last one.
 *
 * This function can be used to iterate over all registered bitstream
 * filters.
 *)
function av_bitstream_filter_next(f: {const} PAVBitStreamFilter): PAVBitStreamFilter;
  cdecl; external av__codec; deprecated;
{$ENDIF}

(**
 * @return a bitstream filter with the specified name or NULL if no such
 *         bitstream filter exists.
 *)
function av_bsf_get_by_name(name: {const} PAnsiChar): {const} PAVBitStreamFilter;
  cdecl; external av__codec;

(**
 * Iterate over all registered bitstream filters.
 *
 * @param opaque a pointer where libavcodec will store the iteration state. Must
 *               point to NULL to start the iteration.
 *
 * @return the next registered bitstream filter or NULL when the iteration is
 *         finished
 *)
function av_bsf_next(opaque: pointer): {const} PAVBitStreamFilter;
  cdecl; external av__codec;

(**
 * Allocate a context for a given bitstream filter. The caller must fill in the
 * context parameters as described in the documentation and then call
 * av_bsf_init() before sending any data to the filter.
 *
 * @param filter the filter for which to allocate an instance.
 * @param ctx a pointer into which the pointer to the newly-allocated context
 *            will be written. It must be freed with av_bsf_free() after the
 *            filtering is done.
 *
 * @return 0 on success, a negative AVERROR code on failure
 *)
function av_bsf_alloc(filter: {const} PAVBitStreamFilter; ctx: PPAVBSFContext): cint;
  cdecl; external av__codec;

(**
 * Prepare the filter for use, after all the parameters and options have been
 * set.
 *)
function av_bsf_init(ctx: PAVBSFContext): cint;
  cdecl; external av__codec;

(**
 * Submit a packet for filtering.
 *
 * After sending each packet, the filter must be completely drained by calling
 * av_bsf_receive_packet() repeatedly until it returns AVERROR(EAGAIN) or
 * AVERROR_EOF.
 *
 * @param pkt the packet to filter. pkt must contain some payload (i.e data or
 * side data must be present in pkt). The bitstream filter will take ownership of
 * the packet and reset the contents of pkt. pkt is not touched if an error occurs.
 * This parameter may be NULL, which signals the end of the stream (i.e. no more
 * packets will be sent). That will cause the filter to output any packets it
 * may have buffered internally.
 *
 * @return 0 on success, a negative AVERROR on error.
 *)
function av_bsf_send_packet(ctx: PAVBSFContext; pkt: PAVPacket): cint;
  cdecl; external av__codec;

(**
 * Retrieve a filtered packet.
 *
 * @param[out] pkt this struct will be filled with the contents of the filtered
 *                 packet. It is owned by the caller and must be freed using
 *                 av_packet_unref() when it is no longer needed.
 *                 This parameter should be "clean" (i.e. freshly allocated
 *                 with av_packet_alloc() or unreffed with av_packet_unref())
 *                 when this function is called. If this function returns
 *                 successfully, the contents of pkt will be completely
 *                 overwritten by the returned data. On failure, pkt is not
 *                 touched.
 *
 * @return 0 on success. AVERROR(EAGAIN) if more packets need to be sent to the
 * filter (using av_bsf_send_packet()) to get more output. AVERROR_EOF if there
 * will be no further output from the filter. Another negative AVERROR value if
 * an error occurs.
 *
 * @note one input packet may result in several output packets, so after sending
 * a packet with av_bsf_send_packet(), this function needs to be called
 * repeatedly until it stops returning 0. It is also possible for a filter to
 * output fewer packets than were sent to it, so this function may return
 * AVERROR(EAGAIN) immediately after a successful av_bsf_send_packet() call.
 *)
function av_bsf_receive_packet(ctx: PAVBSFContext; pkt: PAVPacket): cint;
  cdecl; external av__codec;

(**
 * Free a bitstream filter context and everything associated with it; write NULL
 * into the supplied pointer.
 *)
procedure av_bsf_free(ctx: PPAVBSFContext);
  cdecl; external av__codec;

(**
 * Get the AVClass for AVBSFContext. It can be used in combination with
 * AV_OPT_SEARCH_FAKE_OBJ for examining options.
 *
 * @see av_opt_find().
 *)
function av_bsf_get_class(): {const} PAVClass;
  cdecl; external av__codec;

(**
 * Allocate empty list of bitstream filters.
 * The list must be later freed by av_bsf_list_free()
 * or finalized by av_bsf_list_finalize().
 *
 * @return Pointer to @ref AVBSFList on success, NULL in case of failure
 *)
function av_bsf_list_alloc(): PAVBSFList;
  cdecl; external av__codec;

(**
 * Free list of bitstream filters.
 *
 * @param lst Pointer to pointer returned by av_bsf_list_alloc()
 *)
procedure av_bsf_list_free(lst: PPAVBSFList);
  cdecl; external av__codec;

(**
 * Append bitstream filter to the list of bitstream filters.
 *
 * @param lst List to append to
 * @param bsf Filter context to be appended
 *
 * @return >=0 on success, negative AVERROR in case of failure
 *)
function av_bsf_list_append(lst: PAVBSFList; bsf: PAVBSFContext): cint;
  cdecl; external av__codec;

(**
 * Construct new bitstream filter context given it's name and options
 * and append it to the list of bitstream filters.
 *
 * @param lst      List to append to
 * @param bsf_name Name of the bitstream filter
 * @param options  Options for the bitstream filter, can be set to NULL
 *
 * @return >=0 on success, negative AVERROR in case of failure
 *)
function av_bsf_list_append2(lst: PAVBSFList; bsf_name: {const} Pcchar; options: PPAVDictionary): cint;
  cdecl; external av__codec;

(**
 * Finalize list of bitstream filters.
 *
 * This function will transform @ref AVBSFList to single @ref AVBSFContext,
 * so the whole chain of bitstream filters can be treated as single filter
 * freshly allocated by av_bsf_alloc().
 * If the call is successful, @ref AVBSFList structure is freed and lst
 * will be set to NULL. In case of failure, caller is responsible for
 * freeing the structure by av_bsf_list_free()
 *
 * @param      lst Filter list structure to be transformed
 * @param[out] bsf Pointer to be set to newly created @ref AVBSFContext structure
 *                 representing the chain of bitstream filters
 *
 * @return >=0 on success, negative AVERROR in case of failure
 *)
function av_bsf_list_finalize(lst: PPAVBSFList; bsf: PPAVBSFContext): cint;
  cdecl; external av__codec;

(**
 * Parse string describing list of bitstream filters and create single
 * @ref AVBSFContext describing the whole chain of bitstream filters.
 * Resulting @ref AVBSFContext can be treated as any other @ref AVBSFContext freshly
 * allocated by av_bsf_alloc().
 *
 * @param      str String describing chain of bitstream filters in format
 *                 `bsf1[=opt1=val1:opt2=val2][,bsf2]`
 * @param[out] bsf Pointer to be set to newly created @ref AVBSFContext structure
 *                 representing the chain of bitstream filters
 *
 * @return >=0 on success, negative AVERROR in case of failure
 *)
function av_bsf_list_parse_str(str: {const} Pcchar; bsf: PPAVBSFContext): cint;
  cdecl; external av__codec;

(**
 * Get null/pass-through bitstream filter.
 *
 * @param[out] bsf Pointer to be set to new instance of pass-through bitstream filter
 *
 * @return
 *)
function av_bsf_get_null_filter(bsf: PPAVBSFContext): cint;
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
 * If want_sample is non-zero, additional verbiage will be added to the log
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
 * Allocate a CPB properties structure and initialize its fields to default
 * values.
 *
 * @param size if non-NULL, the size of the allocated struct will be written
 *             here. This is useful for embedding it in side data.
 *
 * @return the newly allocated struct or NULL on failure
 *)
function av_cpb_properties_alloc(size: Psize_t): PAVCPBProperties;
  cdecl; external av__codec;

(**
 * @}
 *)

implementation

end.
