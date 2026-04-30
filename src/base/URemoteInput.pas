{* UltraStar Deluxe - Karaoke Game
 *
 * Remote pitch input support for browser-based USDX remotes.
 * This unit stores timestamped pitch observations. It does not receive or
 * transmit raw microphone audio.
 *}

unit URemoteInput;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Math,
  sdl2,
  SysUtils;

const
  REMOTE_INPUT_MAX_PLAYERS = 12;
  REMOTE_INPUT_BASE_TONE_FREQ = 440;
  REMOTE_INPUT_NUM_HALFTONES = 49;
  REMOTE_INPUT_DEFAULT_DELAY_US = 0;
  REMOTE_INPUT_MAX_FRAME_AGE_US = 1500000;
  REMOTE_INPUT_MAX_FRAMES = 2048;
  REMOTE_INPUT_MIN_CONFIDENCE = 0.30;

type
  TRemotePitchFrame = record
    SongSeq: integer;
    SongTimeUs: int64;
    DurUs: int64;
    F0Hz: double;
    F0Cents: integer;
    RmsDb: double;
    Confidence: double;
    Voiced: boolean;
  end;

  TRemoteToneSample = record
    ToneValid: boolean;
    Tone: integer;
    ToneAbs: integer;
    Confidence: double;
    RmsDb: double;
  end;

  TRemotePlayerBuffer = class
  private
    FFrames: array of TRemotePitchFrame;
    FLock: PSDL_Mutex;
    FPlayerId: UTF8String;
    FRole: UTF8String;
    FSlot: integer;
    FEnabled: boolean;

    procedure DropOldFrames(TargetSongTimeUs: int64);
    function FrameToTone(const Frame: TRemotePitchFrame; out Tone: TRemoteToneSample): boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AssignPlayer(const PlayerId, Role: UTF8String; Slot: integer);
    procedure Clear;
    procedure ClearFrames;
    procedure AddFrame(const Frame: TRemotePitchFrame);
    function TryGetTone(SongSeq: integer; SongTimeUs: int64; DelayUs: int64;
      out Tone: TRemoteToneSample): boolean;

    property Enabled: boolean read FEnabled write FEnabled;
    property PlayerId: UTF8String read FPlayerId;
    property Slot: integer read FSlot;
    property Role: UTF8String read FRole;
  end;

  TRemoteInputProcessor = class
  private
    FPlayers: array of TRemotePlayerBuffer;
    FCurrentSongSeq: integer;
    FDelayUs: int64;

    function GetPlayerBuffer(PlayerIndex: integer): TRemotePlayerBuffer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure SetSongSeq(SongSeq: integer);
    procedure AssignPlayerSlot(PlayerIndex: integer; const PlayerId, Role: UTF8String; Slot: integer);
    procedure ClearPlayerSlot(PlayerIndex: integer);
    procedure AddPitchFrame(PlayerIndex: integer; const Frame: TRemotePitchFrame);
    function TryGetTone(PlayerIndex: integer; SongTimeUs: int64; out Tone: TRemoteToneSample): boolean;
    function HasAssignedPlayer(PlayerIndex: integer): boolean;

    property CurrentSongSeq: integer read FCurrentSongSeq;
    property DelayUs: int64 read FDelayUs write FDelayUs;
    property PlayerBuffer[PlayerIndex: integer]: TRemotePlayerBuffer read GetPlayerBuffer;
  end;

function RemoteInputProcessor(): TRemoteInputProcessor;

implementation

var
  singleton_RemoteInputProcessor: TRemoteInputProcessor = nil;

function RemoteInputProcessor(): TRemoteInputProcessor;
begin
  if singleton_RemoteInputProcessor = nil then
    singleton_RemoteInputProcessor := TRemoteInputProcessor.Create;

  Result := singleton_RemoteInputProcessor;
end;

function CentsToToneAbs(F0Cents: integer): integer;
begin
  // Protocol cents use A4=6900. URecord uses ToneAbs 33 for A4.
  Result := Round((F0Cents - 6900) / 100) + 33;
end;

function HertzToToneAbs(F0Hz: double): integer;
begin
  if (F0Hz <= 0) then
    Result := -1
  else
    Result := Round(12 * Log2(F0Hz / REMOTE_INPUT_BASE_TONE_FREQ)) + 33;
end;

{ TRemotePlayerBuffer }

constructor TRemotePlayerBuffer.Create;
begin
  inherited;
  FLock := SDL_CreateMutex();
  FSlot := 0;
  FEnabled := false;
end;

destructor TRemotePlayerBuffer.Destroy;
begin
  Clear;
  if (FLock <> nil) then
    SDL_DestroyMutex(FLock);
  inherited;
end;

procedure TRemotePlayerBuffer.AssignPlayer(const PlayerId, Role: UTF8String; Slot: integer);
begin
  SDL_LockMutex(FLock);
  try
    FPlayerId := PlayerId;
    FRole := Role;
    FSlot := Slot;
    FEnabled := true;
    SetLength(FFrames, 0);
  finally
    SDL_UnlockMutex(FLock);
  end;
end;

procedure TRemotePlayerBuffer.Clear;
begin
  SDL_LockMutex(FLock);
  try
    SetLength(FFrames, 0);
    FPlayerId := '';
    FRole := '';
    FSlot := 0;
    FEnabled := false;
  finally
    SDL_UnlockMutex(FLock);
  end;
end;

procedure TRemotePlayerBuffer.ClearFrames;
begin
  SDL_LockMutex(FLock);
  try
    SetLength(FFrames, 0);
  finally
    SDL_UnlockMutex(FLock);
  end;
end;

procedure TRemotePlayerBuffer.AddFrame(const Frame: TRemotePitchFrame);
var
  Index: integer;
begin
  SDL_LockMutex(FLock);
  try
    if (Length(FFrames) >= REMOTE_INPUT_MAX_FRAMES) then
    begin
      Move(FFrames[1], FFrames[0], (Length(FFrames) - 1) * SizeOf(TRemotePitchFrame));
      SetLength(FFrames, Length(FFrames) - 1);
    end;

    Index := Length(FFrames);
    SetLength(FFrames, Index + 1);
    FFrames[Index] := Frame;
  finally
    SDL_UnlockMutex(FLock);
  end;
end;

procedure TRemotePlayerBuffer.DropOldFrames(TargetSongTimeUs: int64);
var
  KeepFrom: integer;
  KeepCount: integer;
begin
  KeepFrom := 0;
  while (KeepFrom < Length(FFrames)) and
    (FFrames[KeepFrom].SongTimeUs + FFrames[KeepFrom].DurUs < TargetSongTimeUs - REMOTE_INPUT_MAX_FRAME_AGE_US) do
  begin
    Inc(KeepFrom);
  end;

  if (KeepFrom > 0) then
  begin
    KeepCount := Length(FFrames) - KeepFrom;
    if (KeepCount > 0) then
      Move(FFrames[KeepFrom], FFrames[0], KeepCount * SizeOf(TRemotePitchFrame));
    SetLength(FFrames, KeepCount);
  end;
end;

function TRemotePlayerBuffer.FrameToTone(const Frame: TRemotePitchFrame; out Tone: TRemoteToneSample): boolean;
var
  ToneAbs: integer;
begin
  Result := false;
  Tone.ToneValid := false;
  Tone.Tone := -1;
  Tone.ToneAbs := -1;
  Tone.Confidence := Frame.Confidence;
  Tone.RmsDb := Frame.RmsDb;

  if (not Frame.Voiced) or (Frame.Confidence < REMOTE_INPUT_MIN_CONFIDENCE) then
    Exit;

  if (Frame.F0Cents > 0) then
    ToneAbs := CentsToToneAbs(Frame.F0Cents)
  else
    ToneAbs := HertzToToneAbs(Frame.F0Hz);

  if (ToneAbs < 0) or (ToneAbs >= REMOTE_INPUT_NUM_HALFTONES) then
    Exit;

  Tone.ToneValid := true;
  Tone.ToneAbs := ToneAbs;
  Tone.Tone := ToneAbs mod 12;
  Result := true;
end;

function TRemotePlayerBuffer.TryGetTone(SongSeq: integer; SongTimeUs: int64; DelayUs: int64;
  out Tone: TRemoteToneSample): boolean;
var
  I: integer;
  TargetSongTimeUs: int64;
  BestIndex: integer;
  BestDistance: int64;
  Distance: int64;
begin
  Result := false;
  Tone.ToneValid := false;
  Tone.Tone := -1;
  Tone.ToneAbs := -1;
  Tone.Confidence := 0;
  Tone.RmsDb := -120;

  if (not FEnabled) then
    Exit;

  TargetSongTimeUs := SongTimeUs - DelayUs;
  if (TargetSongTimeUs < 0) then
    TargetSongTimeUs := 0;

  SDL_LockMutex(FLock);
  try
    DropOldFrames(TargetSongTimeUs);
    BestIndex := -1;
    BestDistance := High(int64);

    for I := 0 to High(FFrames) do
    begin
      if (FFrames[I].SongSeq <> SongSeq) then
        Continue;

      if (TargetSongTimeUs >= FFrames[I].SongTimeUs) and
        (TargetSongTimeUs <= FFrames[I].SongTimeUs + FFrames[I].DurUs + 40000) then
      begin
        BestIndex := I;
        Break;
      end;

      Distance := Abs(FFrames[I].SongTimeUs - TargetSongTimeUs);
      if (Distance < BestDistance) and (Distance <= 60000) then
      begin
        BestDistance := Distance;
        BestIndex := I;
      end;
    end;

    if (BestIndex >= 0) then
      Result := FrameToTone(FFrames[BestIndex], Tone);
  finally
    SDL_UnlockMutex(FLock);
  end;
end;

{ TRemoteInputProcessor }

constructor TRemoteInputProcessor.Create;
var
  I: integer;
begin
  inherited;
  FDelayUs := REMOTE_INPUT_DEFAULT_DELAY_US;
  FCurrentSongSeq := 0;
  SetLength(FPlayers, REMOTE_INPUT_MAX_PLAYERS);
  for I := 0 to High(FPlayers) do
    FPlayers[I] := TRemotePlayerBuffer.Create;
end;

destructor TRemoteInputProcessor.Destroy;
var
  I: integer;
begin
  for I := 0 to High(FPlayers) do
    FreeAndNil(FPlayers[I]);
  inherited;
end;

procedure TRemoteInputProcessor.Clear;
var
  I: integer;
begin
  for I := 0 to High(FPlayers) do
    FPlayers[I].Clear;
end;

procedure TRemoteInputProcessor.SetSongSeq(SongSeq: integer);
var
  I: integer;
begin
  if (FCurrentSongSeq <> SongSeq) then
  begin
    FCurrentSongSeq := SongSeq;
    for I := 0 to High(FPlayers) do
      FPlayers[I].ClearFrames;
  end;
end;

function TRemoteInputProcessor.GetPlayerBuffer(PlayerIndex: integer): TRemotePlayerBuffer;
begin
  if (PlayerIndex < 0) or (PlayerIndex > High(FPlayers)) then
    Result := nil
  else
    Result := FPlayers[PlayerIndex];
end;

procedure TRemoteInputProcessor.AssignPlayerSlot(PlayerIndex: integer; const PlayerId, Role: UTF8String; Slot: integer);
begin
  if (GetPlayerBuffer(PlayerIndex) <> nil) then
    FPlayers[PlayerIndex].AssignPlayer(PlayerId, Role, Slot);
end;

procedure TRemoteInputProcessor.ClearPlayerSlot(PlayerIndex: integer);
begin
  if (GetPlayerBuffer(PlayerIndex) <> nil) then
    FPlayers[PlayerIndex].Clear;
end;

procedure TRemoteInputProcessor.AddPitchFrame(PlayerIndex: integer; const Frame: TRemotePitchFrame);
begin
  if (GetPlayerBuffer(PlayerIndex) <> nil) then
    FPlayers[PlayerIndex].AddFrame(Frame);
end;

function TRemoteInputProcessor.TryGetTone(PlayerIndex: integer; SongTimeUs: int64; out Tone: TRemoteToneSample): boolean;
begin
  Result := false;
  Tone.ToneValid := false;
  Tone.Tone := -1;
  Tone.ToneAbs := -1;
  Tone.Confidence := 0;
  Tone.RmsDb := -120;

  if (GetPlayerBuffer(PlayerIndex) = nil) then
    Exit;

  Result := FPlayers[PlayerIndex].TryGetTone(FCurrentSongSeq, SongTimeUs, FDelayUs, Tone);
end;

function TRemoteInputProcessor.HasAssignedPlayer(PlayerIndex: integer): boolean;
var
  Buffer: TRemotePlayerBuffer;
begin
  Result := false;
  Buffer := GetPlayerBuffer(PlayerIndex);
  if (Buffer = nil) then
    Exit;

  SDL_LockMutex(Buffer.FLock);
  try
    Result := Buffer.FEnabled and (Buffer.FPlayerId <> '') and
      ((Buffer.FRole = 'singer') or (Buffer.FRole = 'controller'));
  finally
    SDL_UnlockMutex(Buffer.FLock);
  end;
end;

end.
