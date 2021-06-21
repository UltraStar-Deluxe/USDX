unit avutil;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$MODESWITCH ADVANCEDRECORDS}
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

{$IFDEF DARWIN}
  {$linklib libavutil}
{$ENDIF}

interface

uses
  ctypes,
  rational,
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  SysUtils,
  UConfig;


const
  (* Max. supported version by this header *)
  LIBAVUTIL_MAX_VERSION_MAJOR   = 56;
  LIBAVUTIL_MAX_VERSION_MINOR   = 70;
  LIBAVUTIL_MAX_VERSION_RELEASE = 100;
  LIBAVUTIL_MAX_VERSION = (LIBAVUTIL_MAX_VERSION_MAJOR * VERSION_MAJOR) +
                          (LIBAVUTIL_MAX_VERSION_MINOR * VERSION_MINOR) +
                          (LIBAVUTIL_MAX_VERSION_RELEASE * VERSION_RELEASE);

  (* Min. supported version by this header *)
  LIBAVUTIL_MIN_VERSION_MAJOR   = 56;
  LIBAVUTIL_MIN_VERSION_MINOR   = 14;
  LIBAVUTIL_MIN_VERSION_RELEASE = 100;
  LIBAVUTIL_MIN_VERSION = (LIBAVUTIL_MIN_VERSION_MAJOR * VERSION_MAJOR) +
                          (LIBAVUTIL_MIN_VERSION_MINOR * VERSION_MINOR) +
                          (LIBAVUTIL_MIN_VERSION_RELEASE * VERSION_RELEASE);

(* Check if linked versions are supported *)
{$IF (LIBAVUTIL_VERSION < LIBAVUTIL_MIN_VERSION)}
  {$MESSAGE Error 'Linked version of libavutil is too old!'}
{$IFEND}

{$IF (LIBAVUTIL_VERSION > LIBAVUTIL_MAX_VERSION)}
  {$MESSAGE Error 'Linked version of libavutil is not yet supported!'}
{$IFEND}

type
  (*
   * We use this record as the last element of records that are not supposed
   * to be allocated by pascal code either because the declaration of the
   * record has never been completed or because FFmpeg does the allocation
   * internally.
   *
   * The exceed_stack array has been declared as 10MB array because the default
   * soft stack limit on Linux is less than that. So every time someone tries
   * to allocate this structure on the stack, it will crash unless the soft
   * limit has been raised.
   *
   * The existence of a constructor will cause Free Pascal to issue a warning
   * if an element of this type has been created without calling the
   * constructor.
   *)
  incomplete_record = record
    exceed_stack: array [0..10000000] of byte;
    constructor Never_Call(dummy: integer);
  end;

type
  (*
   * The C standard allows for function pointers to have a different size than
   * data pointers. In practice the size differs only on exotic architectures.
   *)
  cfunctionpointer = procedure;cdecl;
  (*
   * Here we assume that the size of an enum is the same regardless of the
   * values it needs to hold. This is not always the case. The AMD64 System V
   * ABI says:
   *
   *     C++ and some implementations of C permit enums larger than an int.
   *     The underlying type is bumped to an unsigned int, long int or
   *     unsigned long int, in that order.
   *
   * Apart from that there are systems where the default enum size is smaller
   * than an int. But these are unlikely to run FFmpeg.
   *
   * So don't use this type to replace enums that don't fit into 32 bits.
   *)
  cenum = cint;

const
  AV_LOG_FATAL = 8;
  AV_NOPTS_VALUE = -9223372036854775808;
  AV_NUM_DATA_POINTERS = 8;
  AV_TIME_BASE = 1000000;
{$IFDEF UNIX}
  EAGAIN = ESysEAGAIN;
{$ELSE}
  EAGAIN = 11;
{$ENDIF}

type
  TAVSampleFormat = (
    AV_SAMPLE_FMT_U8,
    AV_SAMPLE_FMT_S16,
    AV_SAMPLE_FMT_S32,
    AV_SAMPLE_FMT_FLT,
    AV_SAMPLE_FMT_DBL
  );
  PAVPixelFormat = ^TAVPixelFormat;
  TAVPixelFormat = (
    AV_PIX_FMT_RGB24 = 2,
    AV_PIX_FMT_BGR24,
    AV_PIX_FMT_RGBA = 26,
    AV_PIX_FMT_BGRA = 28
  );
  TAVMediaType = (
    AVMEDIA_TYPE_VIDEO,
    AVMEDIA_TYPE_AUDIO
  );
  ppcuint8 = ^pcuint8;
  PAVFrame = ^TAVFrame;
  PPAVFrame = ^PAVFrame;
  TAVFrame = record
    data: array [0..AV_NUM_DATA_POINTERS-1] of ^cuint8;
    linesize: array [0..AV_NUM_DATA_POINTERS-1] of cint;
    extended_data: ^pcuint8;
    we_do_not_use_width: cint;
    we_do_not_use_height: cint;
    nb_samples: cint;
    we_do_not_use_format: cint;
    we_do_not_use_key_frame: cint;
    we_do_not_use_pict_type: cenum;
    we_do_not_use_sample_aspect_ratio: TAVRational;
    pts: cint64;
    pkt_pts: cint64;
    pkt_dts: cint64;
    we_do_not_use_coded_picture_number: cint;
    we_do_not_use_display_picture_number: cint;
    we_do_not_use_quality: cint;
    we_do_not_use_opaque: pointer;
    we_do_not_use_error: array [0..AV_NUM_DATA_POINTERS-1] of cuint64;
    repeat_pict: cint;
    do_not_instantiate_this_record: incomplete_record;
  end;
  PAVDictionary = ^TAVDictionary;
  PPAVDictionary = ^PAVDictionary;
  TAVDictionary = record
    do_not_instantiate_this_record: incomplete_record;
  end;
procedure av_free(ptr: pointer); cdecl; external 'libavutil';
procedure av_freep(ptr: pointer); cdecl; external 'libavutil';
function av_malloc(size: csize_t): pointer; cdecl; external 'libavutil';
function avutil_version(): cuint; cdecl; external 'libavutil';
function av_frame_alloc(): PAVFrame; cdecl; external 'libavutil';
procedure av_frame_free(frame: PPAVFrame); cdecl; external 'libavutil';
function av_image_alloc(pointers: ppcuint8; linesizes: pcint; w: cint; h: cint; pix_fmt: TAVPixelFormat; align: cint): cint; cdecl; external 'libavutil';
function AVERROR(e: cint): cint; {$IFDEF HasInline}inline;{$ENDIF}
function av_opt_set_int(obj: pointer; name: PAnsiChar; val: cint64; search_flags: cint): cint; cdecl; external 'libavutil';
function av_opt_set_channel_layout(obj: pointer; name: PAnsiChar; ch_layout: cint64; search_flags: cint): cint; cdecl; external 'libavutil';
function av_opt_set_sample_fmt(obj: pointer; name: PAnsiChar; fmt: TAVSampleFormat; search_flags: cint): cint; cdecl; external 'libavutil';
function av_samples_alloc(var audio_data: pcuint8; linesize: pcint; nb_channels: cint; nb_samples: cint; sample_fmt: TAVSampleFormat; align: cint): cint; cdecl; external 'libavutil';
function av_get_packed_sample_fmt(sample_fmt: TAVSampleFormat): TAVSampleFormat; cdecl; external 'libavutil';
function av_get_bytes_per_sample(sample_fmt: TAVSampleFormat): cint; cdecl; external 'libavutil';
procedure av_log_set_level(level: cint); cdecl; external 'libavutil';
implementation
function AVERROR(e: cint): cint; {$IFDEF HasInline}inline;{$ENDIF}
begin
  if EAGAIN < 0 then
    AVERROR := e
  else
    AVERROR := -e;
end;

constructor incomplete_record.Never_Call(dummy: integer);
begin
  abort;
end;

end.
