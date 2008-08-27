(*
 * zlib pascal headers
 * This file is part of Free Pascal, released under the LGPL.
 *)

{$ifdef FPC}
  {$ifndef NO_SMART_LINK}
  {$smartlink on}
  {$endif}
{$endif}
unit zlib;

interface

{$ifdef FPC}
  {$mode objfpc} // Needed for array of const
  {$H+} // use AnsiString
  {$PACKRECORDS C}
{$endif}

uses
  ctypes;

const
  ZLIB_VERSION = '1.2.3';

{$ifdef MSWINDOWS}
  libz = 'zlib1';
{$else}
  libz = 'z';
  {$IFDEF DARWIN}
    {$linklib libz}
  {$ENDIF}
{$endif}

type
  { Compatible with paszlib }
  uInt    = cuint;
  uLong   = culong;
  uLongf  = uLong; {FAR}
  PuLongf = ^uLongf;
  z_off_t = clong;
  pbyte   = ^byte;
  bytef   = byte; {FAR}
  pbytef  = ^byte; 
  voidpf  = pointer;

  TAllocfunc = function (opaque: voidpf; items: uInt; size: uInt): voidpf; cdecl;
  TFreeFunc = procedure (opaque: voidpf; address: voidpf); cdecl;

  TInternalState = record
  end;
  PInternalState = ^TInternalstate;

  TZStream = record
    next_in:   pbytef;
    avail_in:  uInt;
    total_in:  uLong;
    next_out:  pbytef;
    avail_out: uInt;
    total_out: uLong;
    msg:       pchar;
    state:     PInternalState;
    zalloc:    TAllocFunc;
    zfree:     TFreeFunc;
    opaque:    voidpf;
    data_type: cint;
    adler:     uLong;
    reserved:  uLong;
  end;
  TZStreamRec = TZStream;
  PZstream = ^TZStream;
  gzFile = pointer;


const
  Z_NO_FLUSH      = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH        = 4;
  Z_BLOCK         = 5;

  Z_OK            = 0;
  Z_STREAM_END    = 1;
  Z_NEED_DICT     = 2;
  Z_ERRNO         = -(1);
  Z_STREAM_ERROR  = -(2);
  Z_DATA_ERROR    = -(3);
  Z_MEM_ERROR     = -(4);
  Z_BUF_ERROR     = -(5);
  Z_VERSION_ERROR = -(6);

  Z_NO_COMPRESSION      = 0;
  Z_BEST_SPEED          = 1;
  Z_BEST_COMPRESSION    = 9;
  Z_DEFAULT_COMPRESSION = -(1);

  Z_FILTERED         = 1;
  Z_HUFFMAN_ONLY     = 2;
  Z_RLE              = 3;
  Z_FIXED            = 4;
  Z_DEFAULT_STRATEGY = 0;

  Z_BINARY  = 0;
  Z_TEXT    = 1;
  Z_ASCII   = Z_TEXT;
  Z_UNKNOWN = 2;

  Z_DEFLATED = 8;

  Z_NULL = 0;

function zlibVersionpchar(): pchar; cdecl; external libz name 'zlibVersion';
function zlibVersion(): string;

function deflate(var strm: TZStream; flush: integer): integer; cdecl; external libz name 'deflate';
function deflateEnd(var strm: TZStream): integer; cdecl; external libz name 'deflateEnd';
function inflate(var strm: TZStream; flush: integer): integer; cdecl; external libz name 'inflate';
function inflateEnd(var strm: TZStream): integer; cdecl; external libz name 'inflateEnd';
function deflateSetDictionary(var strm: TZStream; dictionary: pbytef; dictLength: uInt): integer; cdecl; external libz name 'deflateSetDictionary';
function deflateCopy(var dest, source: TZstream): integer; cdecl; external libz name 'deflateCopy';
function deflateReset(var strm: TZStream): integer; cdecl; external libz name 'deflateReset';
function deflateParams(var strm: TZStream; level: integer; strategy: integer): integer; cdecl; external libz name 'deflateParams';
//...
function inflateSetDictionary(var strm: TZStream; dictionary: pbytef; dictLength: uInt): integer; cdecl; external libz name 'inflateSetDictionary';
function inflateSync(var strm: TZStream): integer; cdecl; external libz name 'inflateSync';
//...
function inflateReset(var strm: TZStream): integer; cdecl; external libz name 'inflateReset';

function compress(dest: pbytef; destLen: puLongf; source : pbytef; sourceLen: uLong): integer; cdecl; external libz name 'compress';
function compress2(dest: pbytef; destLen: puLongf; source : pbytef; sourceLen: uLong; level: integer): integer; cdecl; external libz name 'compress2';
function uncompress(dest: pbytef; destLen: puLongf; source : pbytef; sourceLen: uLong): integer; cdecl; external libz name 'uncompress';

function gzopen(path: pchar; mode: pchar): gzFile; cdecl; external libz name 'gzopen';
function gzdopen(fd: integer; mode: pchar): gzFile; cdecl; external libz name 'gzdopen';
function gzsetparams(thefile: gzFile; level: integer; strategy: integer): integer; cdecl; external libz name 'gzsetparams';
function gzread(thefile: gzFile; buf: pointer; len: cardinal): integer; cdecl; external libz name 'gzread';
function gzwrite(thefile: gzFile; buf: pointer; len: cardinal): integer; cdecl; external libz name 'gzwrite';
function gzprintf(thefile: gzFile; format: pbytef; args: array of const): integer; cdecl; external libz name 'gzprintf';
function gzputs(thefile: gzFile; s: pbytef): integer; cdecl; external libz name 'gzputs';
function gzgets(thefile: gzFile; buf: pbytef; len: integer): pchar; cdecl; external libz name 'gzgets';
function gzputc(thefile: gzFile; c: integer): integer; cdecl; external libz name 'gzputc';
function gzgetc(thefile: gzFile): integer; cdecl; external libz name 'gzgetc';
function gzflush(thefile: gzFile; flush: integer): integer; cdecl; external libz name 'gzflush';
function gzseek(thefile: gzFile; offset: z_off_t; whence: integer): z_off_t; cdecl; external libz name 'gzseek';
function gzrewind(thefile: gzFile): integer; cdecl; external libz name 'gzrewind';
function gztell(thefile: gzFile): z_off_t; cdecl; external libz name 'gztell';
function gzeof(thefile: gzFile): integer; cdecl; external libz name 'gzeof';
function gzclose(thefile: gzFile): integer; cdecl; external libz name 'gzclose';
function gzerror(thefile: gzFile; var errnum: integer): pchar; cdecl; external libz name 'gzerror';

function adler32(adler: uLong; buf: pbytef; len: uInt): uLong; cdecl; external libz name 'adler32';
function crc32(crc: uLong; buf: pbytef; len: uInt): uLong; cdecl; external libz name 'crc32';

function deflateInit_(var strm: TZStream; level: integer; version: pchar; stream_size: integer): integer; cdecl; external libz name 'deflateInit_';
function deflateInit(var strm: TZStream; level : integer) : integer;
function inflateInit_(var strm: TZStream; version: pchar; stream_size: integer): integer; cdecl; external libz name 'inflateInit_';
function inflateInit(var strm:TZStream) : integer;
function deflateInit2_(var strm: TZStream; level: integer; method: integer; windowBits: integer; memLevel: integer; strategy: integer; version: pchar; stream_size: integer): integer; cdecl; external libz name 'deflateInit2_';
function deflateInit2(var strm: TZStream; level, method, windowBits, memLevel, strategy: integer): integer;
function inflateInit2_(var strm: TZStream; windowBits: integer; version: pchar; stream_size: integer): integer; cdecl; external libz name 'inflateInit2_';
function inflateInit2(var strm: TZStream; windowBits: integer): integer;

function zErrorpchar(err: integer): pchar; cdecl; external libz name 'zError';
function zError(err: integer): string;
function inflateSyncPoint(z: PZstream): integer; cdecl; external libz name 'inflateSyncPoint';
function get_crc_table(): pointer; cdecl; external libz name 'get_crc_table';

function zlibAllocMem(AppData: Pointer; Items, Size: Integer):  Pointer; cdecl;
procedure zlibFreeMem(AppData, Block: Pointer);  cdecl;

implementation

function zlibversion(): string;
begin
   zlibversion := string(zlibversionpchar);
end;

function deflateInit(var strm: TZStream; level: integer) : integer;
begin
   deflateInit := deflateInit_(strm, level, ZLIB_VERSION, sizeof(TZStream));
end;

function inflateInit(var strm: TZStream): integer;
begin
   inflateInit := inflateInit_(strm, ZLIB_VERSION, sizeof(TZStream));
end;

function deflateInit2(var strm: TZStream; level, method, windowBits, memLevel, strategy: integer) : integer;
begin
   deflateInit2 := deflateInit2_(strm, level, method, windowBits, memLevel, strategy, ZLIB_VERSION, sizeof(TZStream));
end;

function inflateInit2(var strm: TZStream; windowBits: integer): integer;
begin
   inflateInit2 := inflateInit2_(strm, windowBits, ZLIB_VERSION, sizeof(TZStream));
end;

function zError(err: integer): string;
begin
   zerror := string(zErrorpchar(err));
end;

function zlibAllocMem(AppData: Pointer; Items, Size: Integer): Pointer; cdecl;
begin
  Result := GetMemory(Items * Size);
end;

procedure zlibFreeMem(AppData, Block: Pointer); cdecl;
begin
  FreeMem(Block);
end;

end.
