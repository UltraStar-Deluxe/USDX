unit UMediaCore_FFMpeg;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMusic,
  avcodec,
  avformat,
  avutil,
  ULog,
  sdl;

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
  TMediaCore_FFMpeg = class
    private
      AVCodecLock: PSDL_Mutex;
    public
      constructor Create();
      destructor Destroy(); override;
      class function GetInstance(): TMediaCore_FFMpeg;

      function GetErrorString(ErrorNum: integer): string;
      function FindStreamIDs(FormatCtx: PAVFormatContext; out FirstVideoStream, FirstAudioStream: integer ): boolean;
      function FindAudioStreamIndex(FormatCtx: PAVFormatContext): integer;
      function ConvertFFMpegToAudioFormat(FFMpegFormat: TSampleFormat; out Format: TAudioSampleFormat): boolean;
      procedure LockAVCodec();
      procedure UnlockAVCodec();
  end;

implementation

uses
  SysUtils;

var
  Instance: TMediaCore_FFMpeg;

constructor TMediaCore_FFMpeg.Create();
begin
  inherited;
  AVCodecLock := SDL_CreateMutex();
end;

destructor TMediaCore_FFMpeg.Destroy();
begin
  SDL_DestroyMutex(AVCodecLock);
  inherited;
end;

class function TMediaCore_FFMpeg.GetInstance(): TMediaCore_FFMpeg;
begin
  if (not Assigned(Instance)) then
    Instance := TMediaCore_FFMpeg.Create();
  Result := Instance;
end;

procedure TMediaCore_FFMpeg.LockAVCodec();
begin
  SDL_mutexP(AVCodecLock);
end;

procedure TMediaCore_FFMpeg.UnlockAVCodec();
begin
  SDL_mutexV(AVCodecLock);
end;

function TMediaCore_FFMpeg.GetErrorString(ErrorNum: integer): string;
begin
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
end;

{
  @param(FormatCtx is a PAVFormatContext returned from av_open_input_file )
  @param(FirstVideoStream is an OUT value of type integer, this is the index of the video stream)
  @param(FirstAudioStream is an OUT value of type integer, this is the index of the audio stream)
  @returns(@true on success, @false otherwise)
}
function TMediaCore_FFMpeg.FindStreamIDs(FormatCtx: PAVFormatContext; out FirstVideoStream, FirstAudioStream: integer): boolean;
var
  i: integer;
  Stream: PAVStream;
begin
  // find the first video stream
  FirstAudioStream := -1;
  FirstVideoStream := -1;

  for i := 0 to FormatCtx.nb_streams-1 do
  begin
    Stream := FormatCtx.streams[i];

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

  // return true if either an audio- or video-stream was found
  Result := (FirstAudioStream > -1) or
            (FirstVideoStream > -1) ;
end;

function TMediaCore_FFMpeg.FindAudioStreamIndex(FormatCtx: PAVFormatContext): integer;
var
  i: integer;
  StreamIndex: integer;
  Stream: PAVStream;
begin
  // find the first audio stream
  StreamIndex := -1;

  for i := 0 to FormatCtx^.nb_streams-1 do
  begin
    Stream := FormatCtx^.streams[i];

    if (Stream.codec^.codec_type = CODEC_TYPE_AUDIO) then
    begin
      StreamIndex := i;
      Break;
    end;
  end;

  Result := StreamIndex;
end;

function TMediaCore_FFMpeg.ConvertFFMpegToAudioFormat(FFMpegFormat: TSampleFormat; out Format: TAudioSampleFormat): boolean;
begin
  case FFMpegFormat of
    SAMPLE_FMT_U8:  Format := asfU8;
    SAMPLE_FMT_S16: Format := asfS16;
    SAMPLE_FMT_S24: Format := asfS24;
    SAMPLE_FMT_S32: Format := asfS32;
    SAMPLE_FMT_FLT: Format := asfFloat;
    else begin
      Result := false;
      Exit;
    end;
  end;
  Result := true;
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
  TempPacket^.priv  := StatusInfo;
  // put a copy of the package into the queue
  Result := Put(TempPacket);
  // data has been copied -> delete temp. package
  av_free(TempPacket);
end;

procedure TPacketQueue.FreeStatusInfo(var Packet: TAVPacket);
begin
  if (Packet.priv <> nil) then
    FreeMem(Packet.priv);
end;

function TPacketQueue.GetStatusInfo(var Packet: TAVPacket): Pointer;
begin
  Result := Packet.priv;
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
