unit swresample;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

{$IFDEF DARWIN}
  {$linklib libswresample}
{$ENDIF}

interface

uses
  ctypes,
  avutil,
  UConfig;

const
  (* Supported version by this header *)
  LIBSWRESAMPLE_MAX_VERSION_MAJOR   = 4;
  LIBSWRESAMPLE_MAX_VERSION_MINOR   = 12;
  LIBSWRESAMPLE_MAX_VERSION_RELEASE = 100;
  LIBSWRESAMPLE_MAX_VERSION = (LIBSWRESAMPLE_MAX_VERSION_MAJOR * VERSION_MAJOR) +
                           (LIBSWRESAMPLE_MAX_VERSION_MINOR * VERSION_MINOR) +
                           (LIBSWRESAMPLE_MAX_VERSION_RELEASE * VERSION_RELEASE);

  (* Min. supported version by this header *)
  LIBSWRESAMPLE_MIN_VERSION_MAJOR   = 0;
  LIBSWRESAMPLE_MIN_VERSION_MINOR   = 15;
  LIBSWRESAMPLE_MIN_VERSION_RELEASE = 100;
  LIBSWRESAMPLE_MIN_VERSION = (LIBSWRESAMPLE_MIN_VERSION_MAJOR * VERSION_MAJOR) +
                            (LIBSWRESAMPLE_MIN_VERSION_MINOR * VERSION_MINOR) +
                            (LIBSWRESAMPLE_MIN_VERSION_RELEASE * VERSION_RELEASE);

(* Check if linked versions are supported *)
{$IF (LIBSWRESAMPLE_VERSION < LIBSWRESAMPLE_MIN_VERSION)}
  {$MESSAGE Error 'Linked version of libswresample is too old!'}
{$IFEND}

(* Check if linked version is supported *)
{$IF (LIBSWRESAMPLE_VERSION > LIBSWRESAMPLE_MAX_VERSION)}
  {$MESSAGE Error 'Linked version of libswresample is not yet supported!'}
{$IFEND}

type
  PSwrContext = ^TSwrContext;
  PPSwrContext = ^PSwrContext;
  TSwrContext = record
    do_not_instantiate_this_record: incomplete_record;
  end;
function swr_alloc(): PSwrContext; cdecl; external sw__resample;
function swr_alloc_set_opts(s: PSwrContext; out_ch_layout: cint64; out_sample_fmt: TAVSampleFormat; out_sample_rate: cint; in_ch_layout: cint64; in_sample_fmt: TAVSampleFormat; in_sample_rate: cint; log_offset: cint; log_ctx: pointer): PSwrContext; cdecl; external sw__resample;
function swr_init(s: PSwrContext): cint; cdecl; external sw__resample;
procedure swr_free(s: PPSwrContext); cdecl; external sw__resample;
function swr_convert(s: PSwrContext; var out: pcuint8; out_count: cint; var in_: pcuint8; in_count: cint): cint; cdecl; external sw__resample;
implementation
end.
