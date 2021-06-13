unit avformat;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

{$IFDEF DARWIN}
  {$linklib libavformat}
{$ENDIF}

interface

uses
  ctypes,
  avcodec,
  avio,
  avutil,
  rational,
  UConfig;

const
  (* Max. supported version by this header *)
  LIBAVFORMAT_MAX_VERSION_MAJOR   = 58;
  LIBAVFORMAT_MAX_VERSION_MINOR   = 76;
  LIBAVFORMAT_MAX_VERSION_RELEASE = 100;
  LIBAVFORMAT_MAX_VERSION = (LIBAVFORMAT_MAX_VERSION_MAJOR * VERSION_MAJOR) +
                            (LIBAVFORMAT_MAX_VERSION_MINOR * VERSION_MINOR) +
                            (LIBAVFORMAT_MAX_VERSION_RELEASE * VERSION_RELEASE);

  (* Min. supported version by this header *)
  LIBAVFORMAT_MIN_VERSION_MAJOR   = 58;
  LIBAVFORMAT_MIN_VERSION_MINOR   = 12;
  LIBAVFORMAT_MIN_VERSION_RELEASE = 100;
  LIBAVFORMAT_MIN_VERSION = (LIBAVFORMAT_MIN_VERSION_MAJOR * VERSION_MAJOR) +
                            (LIBAVFORMAT_MIN_VERSION_MINOR * VERSION_MINOR) +
                            (LIBAVFORMAT_MIN_VERSION_RELEASE * VERSION_RELEASE);

(* Check if linked versions are supported *)
{$IF (LIBAVFORMAT_VERSION < LIBAVFORMAT_MIN_VERSION)}
  {$MESSAGE Error 'Linked version of libavformat is too old!'}
{$IFEND}

(* Check if linked versions are supported *)
{$IF (LIBAVFORMAT_VERSION > LIBAVFORMAT_MAX_VERSION)}
  {$MESSAGE Error 'Linked version of libavformat is not yet supported!'}
{$IFEND}

const
  AVFMT_FLAG_GENPTS = 1;
  AVSEEK_FLAG_ANY = 4;
  AVSEEK_FLAG_BACKWARD = 1;
type
  PAVPacketList = ^TAVPacketList;
  TAVPacketList = record
    pkt: TAVPacket;
    next: ^TAVPacketList;
  end;
  PAVInputFormat = ^TAVInputFormat;
  PAVStream = ^TAVStream;
  PPAVStream = ^PAVStream;
  PAVFormatContext = ^TAVFormatContext;
  PPAVFormatContext = ^PAVFormatContext;
  TAVFormatContext = record
    we_do_not_use_av_class: pointer;
    iformat: PAVInputFormat;
    we_do_not_use_oformat: pointer;
    we_do_not_use_priv_data: pointer;
    pb: ^TAVIOContext;
    we_do_not_use_ctx_flags: cint;
    nb_streams: cuint;
    streams: PPAVStream;
    filename: array [0..1023] of AnsiChar; 
    url: ^AnsiChar;
    start_time: cint64;
    duration: cint64;
    we_do_not_use_bit_rate: cint64;
    we_do_not_use_packet_size: cuint;
    we_do_not_use_max_delay: cint;
    flags: cint;
    do_not_instantiate_this_record: incomplete_record;
  end;
  TAVStream = record
    we_do_not_use_index: cint;
    we_do_not_use_id: cint;
    codec: ^TAVCodecContext;
    we_do_not_use_priv_data: pointer;
    time_base: TAVRational; 
    start_time: cint64;
    we_do_not_use_duration: cint64;
    we_do_not_use_nb_frames: cint64;
    we_do_not_use_disposition: cint;
    we_do_not_use_discard: cenum;
    we_do_not_use_sample_aspect_ratio: TAVRational;
    we_do_not_use_metadata: PAVDictionary;
    we_do_not_use_avg_frame_rate: TAVRational;
    we_do_not_use_atached_pic: TAVPacket;
    we_do_not_use_side_data: pointer;
    we_do_not_use_nb_side_data: cint;
    we_do_not_use_event_flags: cint;
    r_frame_rate: TAVRational;
    we_do_not_use_recommended_encoder_configuration: ^AnsiChar;
    codecpar: ^TAVCodecParameters;
    do_not_instantiate_this_record: incomplete_record;
  end;
  TAVInputFormat = record
    name: ^AnsiChar;
    do_not_instantiate_this_record: incomplete_record;
  end;
function avformat_alloc_context(): PAVFormatContext; cdecl; external 'libavformat';
function avformat_open_input(ps: PPAVFormatContext; url: PAnsiChar; fmt: PAVInputFormat; options: PPAVDictionary): cint; cdecl; external 'libavformat';
procedure avformat_close_input(s: PPAVFormatContext); cdecl; external 'libavformat';
function avformat_version(): cuint; cdecl; external 'libavformat';
procedure av_register_all(); cdecl; external 'libavformat';
function avformat_find_stream_info(ic: PAVFormatContext; options: PPAVDictionary): cint; cdecl; external 'libavformat';
function av_stream_get_r_frame_rate(s: PAVStream): TAVRational; cdecl; external 'libavformat';
function av_read_frame(s: PAVFormatContext; var pkt: TAVPacket): cint; cdecl; external 'libavformat';
function av_seek_frame(s: PAVFormatContext; stream_index: cint; timestamp: cint64; flags: cint): cint; cdecl; external 'libavformat';
implementation
end.
