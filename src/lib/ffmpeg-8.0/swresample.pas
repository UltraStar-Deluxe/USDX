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

type
  PSwrContext = ^TSwrContext;
  PPSwrContext = ^PSwrContext;
  TSwrContext = record
    do_not_instantiate_this_record: incomplete_record;
  end;
function swr_alloc(): PSwrContext; cdecl; external sw__resample;
function swr_alloc_set_opts2(s: PPSwrContext; out_ch_layout: PAVChannelLayout; out_sample_fmt: TAVSampleFormat; out_sample_rate: cint; in_ch_layout: PAVChannelLayout; in_sample_fmt: TAVSampleFormat; in_sample_rate: cint; log_offset: cint; log_ctx: pointer): cint; cdecl; external sw__resample;
function swr_init(s: PSwrContext): cint; cdecl; external sw__resample;
procedure swr_free(s: PPSwrContext); cdecl; external sw__resample;
function swr_convert(s: PSwrContext; var out: pcuint8; out_count: cint; var in_: pcuint8; in_count: cint): cint; cdecl; external sw__resample;
implementation
end.
