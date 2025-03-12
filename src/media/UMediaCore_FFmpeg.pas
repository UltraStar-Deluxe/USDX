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
  sdl2,
  avcodec,
  avformat,
  avutil,
  avio,
  swscale,
  UMusic,
  ULog,
  UPath;

type
  PPacketList = ^TPacketList;
  TPacketList = record
    pkt: PAVPacket;
    next: ^TPacketList;
  end;
  PPacketQueue = ^TPacketQueue;
  TPacketQueue = class
    private
      FirstListEntry: PPacketList;
      LastListEntry:  PPacketList;
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
      procedure FreeStatusInfo(Packet: PAVPacket);
      function GetStatusInfo(Packet: PAVPacket): Pointer;
      function Get(var Packet: PAVPacket; Blocking: boolean): integer;
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
      class function ConvertAudioFormatToFFmpeg(Format: TAudioSampleFormat; out FFmpegFormat: TAVSampleFormat): boolean;
      procedure LockAVCodec();
      procedure UnlockAVCodec();
      function AVFormatOpenInput(ps: PPAVFormatContext; filename: {const} PAnsiChar): Integer;
      procedure AVFormatCloseInput(ps: PPAVFormatContext);
      function GetCodecContext(stream: PAVStream; codec: PAVCodec): PAVCodecContext;
  end;

implementation

uses
  SysUtils,
  UConfig;

{ redeclaration of constants with the same names as deprecated
  constants in order to reuse old callback definitions }
const
  URL_RDONLY = 0; (**< read-only *)
  URL_WRONLY = 1; (**< write-only *)
  URL_RDWR   = 2; (**< read-write *)
  BLOCKSIZE  = 4 * 1024;

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
end;

constructor TMediaCore_FFmpeg.Create();
begin
  inherited;

  CheckVersions();
  AVCodecLock := SDL_CreateMutex();
end;

destructor TMediaCore_FFmpeg.Destroy();
begin
  SDL_DestroyMutex(AVCodecLock);
  AVCodecLock:=nil;
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
  SDL_LockMutex(AVCodecLock);
end;

procedure TMediaCore_FFmpeg.UnlockAVCodec();
begin
  SDL_UnlockMutex(AVCodecLock);
end;

function TMediaCore_FFmpeg.GetErrorString(ErrorNum: integer): string;
var
  ErrorBuf: array[0..255] of AnsiChar;
begin
  av_strerror(ErrorNum, @ErrorBuf[0], SizeOf(ErrorBuf));
  Result := ErrorBuf;
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
    Stream := PPAVStream(PtrUInt(FormatCtx.streams) + i * Sizeof(pointer))^;

{$IF LIBAVFORMAT_VERSION < 59000000}
    if (Stream.codec.codec_type = AVMEDIA_TYPE_VIDEO) and
       (FirstVideoStream < 0) and
       ((Stream.disposition and AV_DISPOSITION_ATTACHED_PIC) <> AV_DISPOSITION_ATTACHED_PIC) then
    begin
      FirstVideoStream := i;
    end;

    if (Stream.codec.codec_type = AVMEDIA_TYPE_AUDIO) and
       (FirstAudioStream < 0) then
    begin
      FirstAudioStream := i;
    end;
  end;
{$ELSE}
    if (Stream.codecpar.codec_type = AVMEDIA_TYPE_VIDEO) and
       (FirstVideoStream < 0) and
       ((Stream.disposition and AV_DISPOSITION_ATTACHED_PIC) <> AV_DISPOSITION_ATTACHED_PIC) then
    begin
      FirstVideoStream := i;
    end;

    if (Stream.codecpar.codec_type = AVMEDIA_TYPE_AUDIO) and
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
    Stream := PPAVStream(PtrUInt(FormatCtx^.streams) + i * Sizeof(pointer))^;

{$IF LIBAVFORMAT_VERSION < 59000000}
    if (Stream.codec^.codec_type = AVMEDIA_TYPE_AUDIO) then
{$ELSE}
    if (Stream.codecpar^.codec_type = AVMEDIA_TYPE_AUDIO) then
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

class function TMediaCore_FFmpeg.ConvertAudioFormatToFFmpeg(Format: TAudioSampleFormat; out FFmpegFormat: TAVSampleFormat): boolean;
begin
  Result := true;
  case Format of
    asfU8:     FFmpegFormat := AV_SAMPLE_FMT_U8;
    asfS16:    FFmpegFormat := AV_SAMPLE_FMT_S16;
    asfS32:    FFmpegFormat := AV_SAMPLE_FMT_S32;
    asfFloat:  FFmpegFormat := AV_SAMPLE_FMT_FLT;
    asfDouble: FFmpegFormat := AV_SAMPLE_FMT_DBL;
    else       Result := false;
    end;
end;


{**
 * UTF-8 Filename wrapper based on:
 * http://www.mail-archive.com/libav-user@mplayerhq.hu/msg02460.html
 *}

function FFmpegStreamOpen(Out h: Pointer; filename: PAnsiChar; flags: Integer): Integer;
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
    h := Stream;
  except
    Result := -1;
  end;
end;

function FFmpegStreamRead(h: Pointer; buf: PByteArray; size: cint): cint; cdecl;
var
  Stream: TStream;
begin
  Stream := TStream(h);
  if (Stream = nil) then
    raise EInvalidContainer.Create('FFmpegStreamRead on nil');
  try
    Result := Stream.Read(buf[0], size);
  except
    Result := -1;
  end;
  if Result = 0 then
    Result := AVERROR_EOF;
end;

function FFmpegStreamWrite(h: Pointer; buf: PByteArray; size: cint): cint; cdecl;
var
  Stream: TStream;
begin
  Stream := TStream(h);
  if (Stream = nil) then
    raise EInvalidContainer.Create('FFmpegStreamWrite on nil');
  try
    Result := Stream.Write(buf[0], size);
  except
    Result := -1;
  end;
end;

function FFmpegStreamSeek(h: Pointer; pos: cint64; whence: cint): cint64; cdecl;
var
  Stream : TStream;
  Origin : TSeekOrigin;
begin
  Stream := TStream(h);
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

function FFmpegStreamClose(h: Pointer): Integer;
var
  Stream : TStream;
begin
  Stream := TStream(h);
  try
    Stream.Free;
  except
    ;
  end;
  Result := 0;
end;

function TMediaCore_FFmpeg.AVFormatOpenInput(ps: PPAVFormatContext; filename: {const} PAnsiChar): Integer;
var
  h: Pointer;
  buffer: Pointer;
begin
  ps^ := avformat_alloc_context();
  buffer := av_malloc(BLOCKSIZE);
  FFmpegStreamOpen(h, filename, URL_RDONLY);
  ps^^.pb := avio_alloc_context(buffer, BLOCKSIZE, 0, h, FFmpegStreamRead, FFmpegStreamWrite, FFmpegStreamSeek);
  Result := avformat_open_input(ps, filename, nil, nil);
end;

procedure TMediaCore_FFmpeg.AVFormatCloseInput(ps: PPAVFormatContext);
var
  pb: PAVIOContext;
begin
  pb := ps^^.pb;
  { avformat_close_input frees AVFormatContext, no additional avformat_free_context needed }
  avformat_close_input(ps);
  av_free(pb^.buffer);
  FFmpegStreamClose(pb^.opaque);
  av_free(pb);
end;

function TMediaCore_FFmpeg.GetCodecContext(stream: PAVStream; codec: PAVCodec): PAVCodecContext;
begin
  {$IF LIBAVFORMAT_VERSION < 59000000}
  Result := stream^.codec;
  {$ELSE}
  Result := avcodec_alloc_context3(codec);
  if (Result <> nil) and (avcodec_parameters_to_context(Result, stream^.codecpar) < 0) then
    avcodec_free_context(@Result);
  if Result = nil then
    Log.LogError('Failed to allocate context', 'TMediaCore_FFmpeg.GetCodecContext');
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
  Mutex:=nil;
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

// The queue will take over memory management of Packet (should not be freed by caller)
function TPacketQueue.Put(Packet : PAVPacket): integer;
var
  CurrentListEntry : PPacketList;
begin
  Result := -1;

  if (Packet = nil) then
    Exit;

  New(CurrentListEntry);
  if (CurrentListEntry = nil) then
    Exit;

  CurrentListEntry^.pkt := Packet;
  {$IF LIBAVFORMAT_VERSION < 59000000}
  if (PChar(Packet^.data) <> STATUS_PACKET) then
  begin
    // duplicate data if not valid beyond next av_read_frame
    if (av_dup_packet(@(CurrentListEntry^.pkt)) < 0) then
    begin
      av_free(CurrentListEntry);
      Exit;
    end;
  end;
  {$ENDIF}

  CurrentListEntry^.next := nil;

  SDL_LockMutex(Mutex);
  try
    if (LastListEntry = nil) then
      FirstListEntry := CurrentListEntry
    else
      LastListEntry^.next := CurrentListEntry;

    LastListEntry := CurrentListEntry;
    Inc(PacketCount);

    Size := Size + CurrentListEntry^.pkt^.size;
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
  Packet: PAVPacket;
begin
  Packet := av_packet_alloc();
  if (Packet = nil) then
  begin
    Result := -1;
    Exit;
  end;

  Packet^.data  := Pointer(STATUS_PACKET);
  Packet^.flags := StatusFlag;
  {$IF LIBAVCODEC_VERSION_MAJOR >= 59}
  Packet^.opaque  := StatusInfo;
  {$ELSE}
  Packet^.side_data := StatusInfo;
  {$ENDIF}

  Result := Put(Packet);
end;

procedure TPacketQueue.FreeStatusInfo(Packet: PAVPacket);
begin
  {$IF LIBAVCODEC_VERSION_MAJOR >= 59}
    FreeMemAndNil(Packet^.opaque);
  {$ELSE}
    FreeMemAndNil(Packet^.side_data);
  {$ENDIF}

end;

function TPacketQueue.GetStatusInfo(Packet: PAVPacket): Pointer;
begin
  {$IF LIBAVCODEC_VERSION_MAJOR >= 59}
  Result := Packet^.opaque;
  {$ELSE}
  Result := Packet^.side_data;
  {$ENDIF}
end;

// The caller takes over memory management of Packet
function TPacketQueue.Get(var Packet: PAVPacket; Blocking: boolean): integer;
var
  CurrentListEntry: PPacketList;
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

        Size := Size - CurrentListEntry^.pkt^.size;
        Packet := CurrentListEntry^.pkt;
        Dispose(CurrentListEntry);

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
  CurrentListEntry, TempListEntry: PPacketList;
begin
  SDL_LockMutex(Mutex);

  CurrentListEntry := FirstListEntry;
  while(CurrentListEntry <> nil) do
  begin
    TempListEntry := CurrentListEntry^.next;
    // free status data
    if (PChar(CurrentListEntry^.pkt^.data) = STATUS_PACKET) then
      FreeStatusInfo(CurrentListEntry^.pkt);

    // free packet data
    av_packet_free(@(CurrentListEntry^.pkt));

    Dispose(CurrentListEntry);
    CurrentListEntry := TempListEntry;
  end;
  LastListEntry := nil;
  FirstListEntry := nil;
  PacketCount := 0;
  Size := 0;

  SDL_UnlockMutex(Mutex);
end;

end.
