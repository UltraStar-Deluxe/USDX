{* UltraStar Deluxe - Karaoke Game
 *
 * UltraStar Deluxe is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/media/UMediaCore_FFmpeg.pas $
 * $Id: UMediaCore_FFmpeg.pas 3103 2014-11-22 23:21:19Z k-m_schindler $
 *}

unit UMediaCore_FFmpeg;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
  ctypes,
  sdl,
  avcodec,
  avformat,
  avutil,
  avio,
  swscale,
  UMusic,
  ULog,
  UPath;

type
  PPacketQueue = ^TPacketQueue;
  TPacketQueue = class
    private
      FirstListEntry: PAVPacketList;
      LastListEntry:  PAVPacketList;
      PacketCount: integer;
      Mutex:     PSDL_Mutex;
      Condition: PSDL_Cond;
      Size: integer;
      AbortRequest: boolean;
    public
      constructor Create();
      destructor Destroy(); override;

      function Put(Packet : PAVPacket): integer;
      function PutStatus(StatusFlag: integer; StatusInfo: Pointer): integer;
      procedure FreeStatusInfo(var Packet: TAVPacket);
      function GetStatusInfo(var Packet: TAVPacket): Pointer;
      function Get(var Packet: TAVPacket; Blocking: boolean): integer;
      function GetSize(): integer;
      procedure Flush();
      procedure Abort();
      function IsAborted(): boolean;
  end;

const
  STATUS_PACKET: PChar = 'STATUS_PACKET';
const
  PKT_STATUS_FLAG_EOF     = 1; // signal end-of-file
  PKT_STATUS_FLAG_FLUSH   = 2; // request the decoder to flush its avcodec decode buffers
  PKT_STATUS_FLAG_ERROR   = 3; // signal an error state
  PKT_STATUS_FLAG_EMPTY   = 4; // request the decoder to output empty data (silence or black frames)

type
  TMediaCore_FFmpeg = class
    private
      AVCodecLock: PSDL_Mutex;
    public
      constructor Create();
      destructor Destroy(); override;
      class function GetInstance(): TMediaCore_FFmpeg;

      function GetErrorString(ErrorNum: integer): string;
      function FindStreamIDs(FormatCtx: PAVFormatContext; out FirstVideoStream, FirstAudioStream: integer ): boolean;
      function FindAudioStreamIndex(FormatCtx: PAVFormatContext): integer;
      function ConvertFFmpegToAudioFormat(FFmpegFormat: TAVSampleFormat; out Format: TAudioSampleFormat): boolean;
      procedure LockAVCodec();
      procedure UnlockAVCodec();
      function AVFormatOpenInput(ps: PPAVFormatContext; filename: {const} PAnsiChar): Integer;
      procedure AVFormatCloseInput(ps: PPAVFormatContext);
  end;

implementation

uses
  SysUtils,
  UConfig;

{$IF LIBAVFORMAT_VERSION >= 54029104}
{ redeclaration of constants with the same names as deprecated
  constants in order to reuse old callback definitions }
const
  URL_RDONLY = 0; (**< read-only *)
  URL_WRONLY = 1; (**< write-only *)
  URL_RDWR   = 2; (**< read-write *)
  BLOCKSIZE  = 4 * 1024;
{$ELSE}
function FFmpegStreamOpen(h: PURLContext; filename: PAnsiChar; flags: cint): cint; cdecl; forward;
function FFmpegStreamRead(h: PURLContext; buf: PByteArray; size: cint): cint; cdecl; forward;
function FFmpegStreamWrite(h: PURLContext; buf: PByteArray; size: cint): cint; cdecl; forward;
function FFmpegStreamSeek(h: PURLContext; pos: cint64; whence: cint): cint64; cdecl; forward;
function FFmpegStreamClose(h: PURLContext): cint; cdecl; forward;

const
  UTF8FileProtocol: TURLProtocol = (
      name:      'ufile';
      url_open:  FFmpegStreamOpen;
      url_read:  FFmpegStreamRead;
      url_write: FFmpegStreamWrite;
      url_seek:  FFmpegStreamSeek;
      url_close: FFmpegStreamClose;
  );
{$ENDIF}

var
  Instance: TMediaCore_FFmpeg;

function AV_VERSION_INT(a, b, c: cardinal): cuint;
begin
  Result := (a shl 16) or (b shl 8) or c;
end;

procedure CheckVersions();
var
  libVersion: cuint;
  headerVersion: cuint;

  function hexVerToStr(Version: cuint): string;
  var
    Major, Minor, Release: cardinal;
  begin
    Major   := (Version shr 16) and $FF;;
    Minor   := (Version shr 8) and $FF;
    Release := Version and $FF;
    Result := Format('%d.%d.%d', [Major, Minor, Release]);
  end;

begin
  libVersion := avcodec_version();
  headerVersion := AV_VERSION_INT(
      LIBAVCODEC_VERSION_MAJOR,
      LIBAVCODEC_VERSION_MINOR,
      LIBAVCODEC_VERSION_RELEASE);
  if (libVersion <> headerVersion) then
  begin
    Log.LogError(Format('%s header (%s) and DLL (%s) versions do not match.',
        ['libavcodec', hexVerToStr(headerVersion), hexVerToStr(libVersion)]));
  end;

  {$IF LIBAVFORMAT_VERSION >= 52020000} // 52.20.0
  libVersion := avformat_version();
  headerVersion := AV_VERSION_INT(
      LIBAVFORMAT_VERSION_MAJOR,
      LIBAVFORMAT_VERSION_MINOR,
      LIBAVFORMAT_VERSION_RELEASE);
  if (libVersion <> headerVersion) then
  begin
    Log.LogError(Format('%s header (%s) and DLL (%s) versions do not match.',
        ['libavformat', hexVerToStr(headerVersion), hexVerToStr(libVersion)]));
  end;
  {$IFEND}

  {$IF LIBAVUTIL_VERSION >= 49008000} // 49.8.0
  libVersion := avutil_version();
  headerVersion := AV_VERSION_INT(
      LIBAVUTIL_VERSION_MAJOR,
      LIBAVUTIL_VERSION_MINOR,
      LIBAVUTIL_VERSION_RELEASE);
  if (libVersion <> headerVersion) then
  begin
    Log.LogError(Format('%s header (%s) and DLL (%s) versions do not match.',
        ['libavutil', hexVerToStr(headerVersion), hexVerToStr(libVersion)]));
  end;
  {$IFEND}
  
  {$IF LIBSWSCALE_VERSION >= 000006001} // 0.6.1
  libVersion := swscale_version();
  headerVersion := AV_VERSION_INT(
      LIBSWSCALE_VERSION_MAJOR,
      LIBSWSCALE_VERSION_MINOR,
      LIBSWSCALE_VERSION_RELEASE);
  if (libVersion <> headerVersion) then
  begin
    Log.LogError(Format('%s header (%s) and DLL (%s) versions do not match.',
        ['libswscale', hexVerToStr(headerVersion), hexVerToStr(libVersion)]));
  end;
  {$IFEND}
end;

constructor TMediaCore_FFmpeg.Create();
begin
  inherited;

  CheckVersions();

  {$IF LIBAVFORMAT_VERSION <= 52111000} // 52.110.0
  av_register_protocol(@UTF8FileProtocol);
  {$ELSEIF LIBAVFORMAT_VERSION < 54029104}
  av_register_protocol2(@UTF8FileProtocol, sizeof(UTF8FileProtocol));
  {$IFEND}

  AVCodecLock := SDL_CreateMutex();
end;

destructor TMediaCore_FFmpeg.Destroy();
begin
  SDL_DestroyMutex(AVCodecLock);
  inherited;
end;

class function TMediaCore_FFmpeg.GetInstance(): TMediaCore_FFmpeg;
begin
  if (not Assigned(Instance)) then
    Instance := TMediaCore_FFmpeg.Create();
  Result := Instance;
end;

procedure TMediaCore_FFmpeg.LockAVCodec();
begin
  SDL_mutexP(AVCodecLock);
end;

procedure TMediaCore_FFmpeg.UnlockAVCodec();
begin
  SDL_mutexV(AVCodecLock);
end;

function TMediaCore_FFmpeg.GetErrorString(ErrorNum: integer): string;
begin
{$IF LIBAVUTIL_VERSION < 50043000} // < 50.43.0
  case ErrorNum of
    AVERROR_IO:           Result := 'AVERROR_IO';
    AVERROR_NUMEXPECTED:  Result := 'AVERROR_NUMEXPECTED';
    AVERROR_INVALIDDATA:  Result := 'AVERROR_INVALIDDATA';
    AVERROR_NOMEM:        Result := 'AVERROR_NOMEM';
    AVERROR_NOFMT:        Result := 'AVERROR_NOFMT';
    AVERROR_NOTSUPP:      Result := 'AVERROR_NOTSUPP';
    AVERROR_NOENT:        Result := 'AVERROR_NOENT';
    AVERROR_PATCHWELCOME: Result := 'AVERROR_PATCHWELCOME';
    else                  Result := 'AVERROR_#'+inttostr(ErrorNum);
  end;
{$ELSE}
{$IFEND}
end;

{
  @param(FormatCtx is a PAVFormatContext returned from av_open_input_file )
  @param(FirstVideoStream is an OUT value of type integer, this is the index of the video stream)
  @param(FirstAudioStream is an OUT value of type integer, this is the index of the audio stream)
  @returns(@true on success, @false otherwise)
}
function TMediaCore_FFmpeg.FindStreamIDs(FormatCtx: PAVFormatContext; out FirstVideoStream, FirstAudioStream: integer): boolean;
var
  i: integer;
  Stream: PAVStream;
begin
  // find the first video stream
  FirstAudioStream := -1;
  FirstVideoStream := -1;

  for i := 0 to FormatCtx.nb_streams-1 do
  begin
{$IF LIBAVFORMAT_VERSION <= 52111000} // <= 52.111.0
    Stream := FormatCtx.streams[i];
{$ELSE}
    Stream := PPAVStream(PtrUInt(FormatCtx.streams) + i * Sizeof(pointer))^;
{$IFEND}

{$IF LIBAVCODEC_VERSION < 52064000} // < 52.64.0
    if (Stream.codec.codec_type = CODEC_TYPE_VIDEO) and
       (FirstVideoStream < 0) then
    begin
      FirstVideoStream := i;
    end;

    if (Stream.codec.codec_type = CODEC_TYPE_AUDIO) and
       (FirstAudioStream < 0) then
    begin
      FirstAudioStream := i;
    end;
  end;
{$ELSE}
    if (Stream.codec.codec_type = AVMEDIA_TYPE_VIDEO) and
       (FirstVideoStream < 0) then
    begin
      FirstVideoStream := i;
    end;

    if (Stream.codec.codec_type = AVMEDIA_TYPE_AUDIO) and
       (FirstAudioStream < 0) then
    begin
      FirstAudioStream := i;
    end;
  end;
{$IFEND}

  // return true if either an audio- or video-stream was found
  Result := (FirstAudioStream > -1) or
            (FirstVideoStream > -1) ;
end;

function TMediaCore_FFmpeg.FindAudioStreamIndex(FormatCtx: PAVFormatContext): integer;
var
  i: integer;
  StreamIndex: integer;
  Stream: PAVStream;
begin
  // find the first audio stream
  StreamIndex := -1;

  for i := 0 to FormatCtx^.nb_streams-1 do
  begin
{$IF LIBAVFORMAT_VERSION <= 52111000} // <= 52.111.0
    Stream := FormatCtx^.streams[i];
{$ELSE}
    Stream := PPAVStream(PtrUInt(FormatCtx^.streams) + i * Sizeof(pointer))^;
{$IFEND}

{$IF LIBAVCODEC_VERSION < 52064000} // < 52.64.0
    if (Stream.codec^.codec_type = CODEC_TYPE_AUDIO) then
{$ELSE}
    if (Stream.codec^.codec_type = AVMEDIA_TYPE_AUDIO) then
{$IFEND}
    begin
      StreamIndex := i;
      Break;
    end;
  end;

  Result := StreamIndex;
end;

function TMediaCore_FFmpeg.ConvertFFmpegToAudioFormat(FFmpegFormat: TAVSampleFormat; out Format: TAudioSampleFormat): boolean;
begin
	Result := true;
  case FFmpegFormat of
    AV_SAMPLE_FMT_U8:  Format := asfU8;
    AV_SAMPLE_FMT_S16: Format := asfS16;
    AV_SAMPLE_FMT_S32: Format := asfS32;
    AV_SAMPLE_FMT_FLT: Format := asfFloat;
    AV_SAMPLE_FMT_DBL: Format := asfDouble;
    else               Result := false;
    end;
end;


{**
 * UTF-8 Filename wrapper based on:
 * http://www.mail-archive.com/libav-user@mplayerhq.hu/msg02460.html
 *}

{$IF LIBAVFORMAT_VERSION >= 54029104}
function FFmpegStreamOpen(Out h: Pointer; filename: PAnsiChar; flags: Integer): Integer;
{$ELSE}
function FFmpegStreamOpen(h: PURLContext; filename: PAnsiChar; flags: cint): cint; cdecl;
{$ENDIF}
var
  Stream: TStream;
  Mode: word;
  ProtPrefix: string;
  FilePath: IPath;
begin
  // check for protocol prefix ('ufile:') and strip it
  ProtPrefix := Format('%s:', ['ufile']);
  if (StrLComp(filename, PChar(ProtPrefix), Length(ProtPrefix)) = 0) then
  begin
    Inc(filename, Length(ProtPrefix));
  end;

  FilePath := Path(filename);

  if ((flags and URL_RDWR) = URL_RDWR) then
    Mode := fmCreate
  else if ((flags and URL_WRONLY) = URL_WRONLY) then
    Mode := fmCreate // TODO: fmCreate is Read+Write -> reopen with fmOpenWrite
  else
    Mode := fmOpenRead or fmShareDenyWrite;

  Result := 0;

  try
    Stream := TBinaryFileStream.Create(FilePath, Mode);
    {$IF LIBAVFORMAT_VERSION >= 54029104}
    h := Stream;
    {$ELSE}
    h.priv_data := Stream;
    {$ENDIF}
  except
{$IF LIBAVUTIL_VERSION < 50043000} // < 50.43.0
    Result := AVERROR_NOENT;
{$ELSE}
    Result := -1;
{$IFEND}
  end;
end;

{$IF LIBAVFORMAT_VERSION >= 54029104}
function FFmpegStreamRead(h: Pointer; buf: PByteArray; size: cint): cint; cdecl;
{$ELSE}
function FFmpegStreamRead(h: PURLContext; buf: PByteArray; size: cint): cint; cdecl;
{$ENDIF}
var
  Stream: TStream;
begin
  {$IF LIBAVFORMAT_VERSION >= 54029104}
  Stream := TStream(h);
  {$ELSE}
  Stream := TStream(h.priv_data);
  {$ENDIF}
  if (Stream = nil) then
    raise EInvalidContainer.Create('FFmpegStreamRead on nil');
  try
    Result := Stream.Read(buf[0], size);
  except
    Result := -1;
  end;
end;

{$IF LIBAVFORMAT_VERSION >= 54029104}
function FFmpegStreamWrite(h: Pointer; buf: PByteArray; size: cint): cint; cdecl;
{$ELSE}
function FFmpegStreamWrite(h: PURLContext; buf: PByteArray; size: cint): cint; cdecl;
{$ENDIF}
var
  Stream: TStream;
begin
  {$IF LIBAVFORMAT_VERSION >= 54029104}
  Stream := TStream(h);
  {$ELSE}
  Stream := TStream(h.priv_data);
  {$ENDIF}
  if (Stream = nil) then
    raise EInvalidContainer.Create('FFmpegStreamWrite on nil');
  try
    Result := Stream.Write(buf[0], size);
  except
    Result := -1;
  end;
end;

{$IF LIBAVFORMAT_VERSION >= 54029104}
function FFmpegStreamSeek(h: Pointer; pos: cint64; whence: cint): cint64; cdecl;
{$ELSE}
function FFmpegStreamSeek(h: PURLContext; pos: cint64; whence: cint): cint64; cdecl;
{$ENDIF}
var
  Stream : TStream;
  Origin : TSeekOrigin;
begin
  {$IF LIBAVFORMAT_VERSION >= 54029104}
  Stream := TStream(h);
  {$ELSE}
  Stream := TStream(h.priv_data);
  {$ENDIF}
  if (Stream = nil) then
    raise EInvalidContainer.Create('FFmpegStreamSeek on nil');
  case whence of
    0 {SEEK_SET}: Origin := soBeginning;
    1 {SEEK_CUR}: Origin := soCurrent;
    2 {SEEK_END}: Origin := soEnd;
    AVSEEK_SIZE: begin
      Result := Stream.Size;
      Exit;
    end
  else
    Origin := soBeginning;
  end;
  Result := Stream.Seek(pos, Origin);
end;

{$IF LIBAVFORMAT_VERSION >= 54029104}
function FFmpegStreamClose(h: Pointer): Integer;
{$ELSE}
function FFmpegStreamClose(h: PURLContext): cint; cdecl;
{$ENDIF}
var
  Stream : TStream;
begin
  {$IF LIBAVFORMAT_VERSION >= 54029104}
  Stream := TStream(h);
  {$ELSE}
  Stream := TStream(h.priv_data);
  {$ENDIF}
  Stream.Free;
  Result := 0;
end;

function TMediaCore_FFmpeg.AVFormatOpenInput(ps: PPAVFormatContext; filename: {const} PAnsiChar): Integer;
{$IF LIBAVFORMAT_VERSION >= 54029104}
var
  h: Pointer;
  buffer: Pointer;
{$ENDIF}
begin
  {$IF LIBAVFORMAT_VERSION >= 54029104}
  ps^ := avformat_alloc_context();
  buffer := av_malloc(BLOCKSIZE);
  FFmpegStreamOpen(h, filename, URL_RDONLY);
  ps^^.pb := avio_alloc_context(buffer, BLOCKSIZE, 0, h, FFmpegStreamRead, FFmpegStreamWrite, FFmpegStreamSeek);
  Result := avformat_open_input(ps, filename, nil, nil);
  {$ELSE}
  Result := 0;
  {$ENDIF}
end;

procedure TMediaCore_FFmpeg.AVFormatCloseInput(ps: PPAVFormatContext);
begin
  {$IF LIBAVFORMAT_VERSION >= 54029104}
  av_free(ps^^.pb.buffer);
  FFmpegStreamClose(ps^^.pb.opaque);
  { avformat_close_input frees AVIOContext pb, no avio_close needed }
  { avformat_close_input frees AVFormatContext, no additional avformat_free_context needed }
  avformat_close_input(ps);
  {$ENDIF}
end;

{ TPacketQueue }

constructor TPacketQueue.Create();
begin
  inherited;

  FirstListEntry := nil;
  LastListEntry  := nil;
  PacketCount := 0;
  Size := 0;

  Mutex := SDL_CreateMutex();
  Condition := SDL_CreateCond();
end;

destructor TPacketQueue.Destroy();
begin
  Flush();
  SDL_DestroyMutex(Mutex);
  SDL_DestroyCond(Condition);
  inherited;
end;

procedure TPacketQueue.Abort();
begin
  SDL_LockMutex(Mutex);

  AbortRequest := true;

  SDL_CondBroadcast(Condition);
  SDL_UnlockMutex(Mutex);
end;

function TPacketQueue.IsAborted(): boolean;
begin
  SDL_LockMutex(Mutex);
  Result := AbortRequest;
  SDL_UnlockMutex(Mutex);
end;

function TPacketQueue.Put(Packet : PAVPacket): integer;
var
  CurrentListEntry : PAVPacketList;
begin
  Result := -1;

  if (Packet = nil) then
    Exit;

  if (PChar(Packet^.data) <> STATUS_PACKET) then
  begin
    if (av_dup_packet(Packet) < 0) then
      Exit;
  end;

  CurrentListEntry := av_malloc(SizeOf(TAVPacketList));
  if (CurrentListEntry = nil) then
    Exit;

  CurrentListEntry^.pkt  := Packet^;
  CurrentListEntry^.next := nil;

  SDL_LockMutex(Mutex);
  try
    if (LastListEntry = nil) then
      FirstListEntry := CurrentListEntry
    else
      LastListEntry^.next := CurrentListEntry;

    LastListEntry := CurrentListEntry;
    Inc(PacketCount);

    Size := Size + CurrentListEntry^.pkt.size;
    SDL_CondSignal(Condition);
  finally
    SDL_UnlockMutex(Mutex);
  end;

  Result := 0;
end;

(**
 * Adds a status packet (EOF, Flush, etc.) to the end of the queue.
 * StatusInfo can be used to pass additional information to the decoder.
 * Only assign nil or a valid pointer to data allocated with Getmem() to
 * StatusInfo because the pointer will be disposed with Freemem() on a call
 * to Flush(). If the packet is removed from the queue it is the decoder's
 * responsibility to free the StatusInfo data with FreeStatusInfo().
 *)
function TPacketQueue.PutStatus(StatusFlag: integer; StatusInfo: Pointer): integer;
var
  TempPacket: PAVPacket;
begin
  // create temp. package
  TempPacket := av_malloc(SizeOf(TAVPacket));
  if (TempPacket = nil) then
  begin
    Result := -1;
    Exit;
  end;
  // init package
  av_init_packet(TempPacket^);
  TempPacket^.data  := Pointer(STATUS_PACKET);
  TempPacket^.flags := StatusFlag;
{$IF FFMPEG_VERSION_INT < 2000000}
  TempPacket^.priv  := StatusInfo;
{$ENDIF}
  // put a copy of the package into the queue
  Result := Put(TempPacket);
  // data has been copied -> delete temp. package
  av_free(TempPacket);
end;

procedure TPacketQueue.FreeStatusInfo(var Packet: TAVPacket);
begin
{$IF FFMPEG_VERSION_INT < 2000000}
  if (Packet.priv <> nil) then
    FreeMem(Packet.priv);
{$ENDIF}
end;

function TPacketQueue.GetStatusInfo(var Packet: TAVPacket): Pointer;
begin
{$IF FFMPEG_VERSION_INT < 2000000}
  Result := Packet.priv;
{$ENDIF}
end;

function TPacketQueue.Get(var Packet: TAVPacket; Blocking: boolean): integer;
var
  CurrentListEntry: PAVPacketList;
const
  WAIT_TIMEOUT = 10; // timeout in ms
begin
  Result := -1;

  SDL_LockMutex(Mutex);
  try
    while (true) do
    begin
      if (AbortRequest) then
        Exit;

      CurrentListEntry := FirstListEntry;
      if (CurrentListEntry <> nil) then
      begin
        FirstListEntry := CurrentListEntry^.next;
        if (FirstListEntry = nil) then
          LastListEntry := nil;
        Dec(PacketCount);

        Size := Size - CurrentListEntry^.pkt.size;
        Packet := CurrentListEntry^.pkt;
        av_free(CurrentListEntry);

        Result := 1;
        Break;
      end
      else if (not Blocking) then
      begin
        Result := 0;
        Break;
      end
      else
      begin
        // block until a new package arrives,
        // but do not wait till infinity to avoid deadlocks
        if (SDL_CondWaitTimeout(Condition, Mutex, WAIT_TIMEOUT) = SDL_MUTEX_TIMEDOUT) then
        begin
          Result := 0;
          Break;
        end;
      end;
    end;
  finally
    SDL_UnlockMutex(Mutex);
  end;
end;

function TPacketQueue.GetSize(): integer;
begin
  SDL_LockMutex(Mutex);
  Result := Size;
  SDL_UnlockMutex(Mutex);
end;

procedure TPacketQueue.Flush();
var
  CurrentListEntry, TempListEntry: PAVPacketList;
begin
  SDL_LockMutex(Mutex);

  CurrentListEntry := FirstListEntry;
  while(CurrentListEntry <> nil) do
  begin
    TempListEntry := CurrentListEntry^.next;
    // free status data
    if (PChar(CurrentListEntry^.pkt.data) = STATUS_PACKET) then
      FreeStatusInfo(CurrentListEntry^.pkt);
    // free packet data
    av_free_packet(@CurrentListEntry^.pkt);
    // Note: param must be a pointer to a pointer!
    av_freep(@CurrentListEntry);
    CurrentListEntry := TempListEntry;
  end;
  LastListEntry := nil;
  FirstListEntry := nil;
  PacketCount := 0;
  Size := 0;

  SDL_UnlockMutex(Mutex);
end;

end.
