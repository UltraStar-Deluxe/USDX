unit avio;

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
  avutil,
  SysUtils;
const
  AVSEEK_SIZE = 65536;
type
  PAVIOContext = ^TAVIOContext;
  TAVIOContext = record
    we_do_not_use_av_class: pointer;
    buffer: ^cuchar;
    we_do_not_use_buffer_size: cint;
    we_do_not_use_buf_ptr: pcuchar;
    we_do_not_use_buf_end: pcuchar;
    opaque: pointer;
    we_do_not_use_read_packet: cfunctionpointer;
    we_do_not_use_write_packet: cfunctionpointer;
    we_do_not_use_seek: cfunctionpointer;
    pos: cint64;
    eof_reached: cint;
    we_do_not_use_write_flag: cint;
    we_do_not_use_max_packet_size: cint;
    we_do_not_use_checksum: culong;
    we_do_not_use_checksum_ptr: pcuchar;
    we_do_not_use_update_checksum: cfunctionpointer;
    error: cint;
    do_not_instantiate_this_record: incomplete_record;
  end;
  TReadWriteFunc = function(opaque: pointer; buf: pbytearray; buf_size: cint): cint; cdecl;
  TSeekFunc = function(opaque: pointer; offset: cint64; whence: cint): cint64; cdecl;
function avio_alloc_context(buffer: pcuint8; buffer_size: cint; write_flag: cint; opaque: pointer; read_packet: TReadWriteFunc; write_packet: TReadWriteFunc; seek: TSeekFunc): PAVIOContext; cdecl; external 'libavformat';
function avio_feof(s: PAVIOContext): cint; cdecl; external 'libavformat';
function avio_size(s: PAVIOContext): cint64; cdecl; external 'libavformat';
implementation
end.
