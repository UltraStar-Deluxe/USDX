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
  REMOTE_TONE_VOICED_UNSTABLE = -1000;
  REMOTE_TONE_UNVOICED = -1001;
  REMOTE_INPUT_MIN_GAME_DELAY_MS = 500;
  REMOTE_INPUT_GAME_DELAY_MARGIN_MS = 250;
  REMOTE_INPUT_DEFAULT_DELAY_US = 0;
  REMOTE_INPUT_MAX_FRAME_AGE_US = 1500000;
  REMOTE_INPUT_MAX_FRAME_DISTANCE_US = 180000;
  REMOTE_INPUT_VOTE_WINDOW_US = 45000;
  REMOTE_INPUT_MAX_FRAMES = 2048;
  REMOTE_INPUT_MIN_CONFIDENCE = 0.35;
  REMOTE_INPUT_MIN_RMS_DB = -70;

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

  TRemoteBeatTone = record
    SongSeq: integer;
    Beat: integer;
    Tone: integer;
  end;

  TRemoteToneSample = record
    ToneValid: boolean;
    Tone: integer;
    ToneAbs: integer;
    Confidence: double;
    RmsDb: double;
  end;

  TRemoteConsumedBeat = record
    SongSeq: integer;
    Beat: integer;
  end;

  TRemotePlayerBuffer = class
  private
    FFrames: array of TRemotePitchFrame;
    FBeatTones: array of TRemoteBeatTone;
    FLock: PSDL_Mutex;
    FPlayerId: UTF8String;
    FRole: UTF8String;
    FSlot: integer;
    FMicDelayMs: integer;
    FEnabled: boolean;
    FConsumedBeats: array of TRemoteConsumedBeat;

    procedure DropOldFrames(TargetSongTimeUs: int64);
    function FrameToTone(const Frame: TRemotePitchFrame; out Tone: TRemoteToneSample): boolean;
    function IsBeatConsumed(SongSeq, Beat: integer): boolean;
    procedure UnmarkBeatConsumed(SongSeq, Beat: integer);
    procedure MarkBeatConsumed(SongSeq, Beat: integer);
    function FindBeatTone(SongSeq, Beat: integer; out StoredTone: integer): boolean;
    function StoredToneToToneSample(StoredTone: integer; out Tone: TRemoteToneSample): boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AssignPlayer(const PlayerId, Role: UTF8String; Slot, MicDelayMs: integer);
    procedure Clear;
    procedure ClearFrames;
    procedure SetMicDelayMs(MicDelayMs: integer);
    procedure AddFrame(const Frame: TRemotePitchFrame);
    procedure AddBeatTone(const BeatTone: TRemoteBeatTone);
    function TryGetTone(SongSeq: integer; SongTimeUs: int64; DelayUs: int64;
      out Tone: TRemoteToneSample): boolean;
    function TryConsumeToneForBeat(SongSeq, Beat: integer; SongTimeUs: int64; DelayUs: int64;
      out Tone: TRemoteToneSample): boolean;

    property Enabled: boolean read FEnabled write FEnabled;
    property PlayerId: UTF8String read FPlayerId;
    property Slot: integer read FSlot;
    property Role: UTF8String read FRole;
    property MicDelayMs: integer read FMicDelayMs;
  end;

  TRemoteInputProcessor = class
  private
    FPlayers: array of TRemotePlayerBuffer;
    FCurrentSongSeq: integer;
    FDelayUs: int64;
    FAcceptingInput: boolean;

    function GetPlayerBuffer(PlayerIndex: integer): TRemotePlayerBuffer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure SetSongSeq(SongSeq: integer);
    procedure AssignPlayerSlot(PlayerIndex: integer; const PlayerId, Role: UTF8String; Slot, MicDelayMs: integer);
    procedure ClearPlayerSlot(PlayerIndex: integer);
    procedure SetPlayerDelayMs(PlayerIndex, MicDelayMs: integer);
    procedure AddPitchFrame(PlayerIndex: integer; const Frame: TRemotePitchFrame);
    procedure AddBeatTone(PlayerIndex: integer; Beat, Tone: integer);
    function TryGetTone(PlayerIndex: integer; SongTimeUs: int64; out Tone: TRemoteToneSample): boolean;
    function TryConsumeToneForBeat(PlayerIndex, Beat: integer; SongTimeUs: int64; out Tone: TRemoteToneSample): boolean;
    function HasAssignedPlayer(PlayerIndex: integer): boolean;
    function RequiredGameDelayMs(): integer;

    property CurrentSongSeq: integer read FCurrentSongSeq;
    property DelayUs: int64 read FDelayUs write FDelayUs;
    property AcceptingInput: boolean read FAcceptingInput write FAcceptingInput;
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

procedure TRemotePlayerBuffer.AssignPlayer(const PlayerId, Role: UTF8String; Slot, MicDelayMs: integer);
var
  SameAssignment: boolean;
begin
  SDL_LockMutex(FLock);
  try
    SameAssignment := FEnabled and (FPlayerId = PlayerId) and (FRole = Role) and (FSlot = Slot);
    FPlayerId := PlayerId;
    FRole := Role;
    FSlot := Slot;
    FMicDelayMs := MicDelayMs;
    FEnabled := true;
    if not SameAssignment then
    begin
      SetLength(FFrames, 0);
      SetLength(FBeatTones, 0);
      SetLength(FConsumedBeats, 0);
    end;
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
    FMicDelayMs := 0;
    FEnabled := false;
    SetLength(FConsumedBeats, 0);
    SetLength(FBeatTones, 0);
  finally
    SDL_UnlockMutex(FLock);
  end;
end;

procedure TRemotePlayerBuffer.SetMicDelayMs(MicDelayMs: integer);
begin
  SDL_LockMutex(FLock);
  try
    FMicDelayMs := MicDelayMs;
  finally
    SDL_UnlockMutex(FLock);
  end;
end;

procedure TRemotePlayerBuffer.ClearFrames;
begin
  SDL_LockMutex(FLock);
  try
    SetLength(FFrames, 0);
    SetLength(FBeatTones, 0);
    SetLength(FConsumedBeats, 0);
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

procedure TRemotePlayerBuffer.AddBeatTone(const BeatTone: TRemoteBeatTone);
var
  I: integer;
  Index: integer;
  PreviousTone: integer;
  HadPreviousTone: boolean;
begin
  SDL_LockMutex(FLock);
  try
    HadPreviousTone := FindBeatTone(BeatTone.SongSeq, BeatTone.Beat, PreviousTone);
    for I := 0 to High(FBeatTones) do
    begin
      if (FBeatTones[I].SongSeq = BeatTone.SongSeq) and
         (FBeatTones[I].Beat = BeatTone.Beat) then
      begin
        if (FBeatTones[I].Tone > REMOTE_TONE_VOICED_UNSTABLE) and
           (BeatTone.Tone <= REMOTE_TONE_VOICED_UNSTABLE) then
          Exit;

        FBeatTones[I].Tone := BeatTone.Tone;
        if ((not HadPreviousTone) or (PreviousTone <= REMOTE_TONE_VOICED_UNSTABLE)) and
           (BeatTone.Tone > REMOTE_TONE_VOICED_UNSTABLE) then
          UnmarkBeatConsumed(BeatTone.SongSeq, BeatTone.Beat);
        Exit;
      end;
    end;

    if (Length(FBeatTones) >= REMOTE_INPUT_MAX_FRAMES) then
    begin
      Move(FBeatTones[1], FBeatTones[0], (Length(FBeatTones) - 1) * SizeOf(TRemoteBeatTone));
      SetLength(FBeatTones, Length(FBeatTones) - 1);
    end;

    Index := Length(FBeatTones);
    SetLength(FBeatTones, Index + 1);
    FBeatTones[Index] := BeatTone;
    if (BeatTone.Tone > REMOTE_TONE_VOICED_UNSTABLE) then
      UnmarkBeatConsumed(BeatTone.SongSeq, BeatTone.Beat);
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

  if (not Frame.Voiced) or
     (Frame.Confidence < REMOTE_INPUT_MIN_CONFIDENCE) or
     (Frame.RmsDb < REMOTE_INPUT_MIN_RMS_DB) then
    Exit;

  if (Frame.F0Cents > 0) then
    ToneAbs := CentsToToneAbs(Frame.F0Cents)
  else
    ToneAbs := HertzToToneAbs(Frame.F0Hz);

  if (ToneAbs < 0) then
    ToneAbs := 0
  else if (ToneAbs >= REMOTE_INPUT_NUM_HALFTONES) then
    ToneAbs := REMOTE_INPUT_NUM_HALFTONES - 1;

  Tone.ToneValid := true;
  Tone.ToneAbs := ToneAbs;
  Tone.Tone := ToneAbs mod 12;
  Result := true;
end;

function TRemotePlayerBuffer.IsBeatConsumed(SongSeq, Beat: integer): boolean;
var
  I: integer;
begin
  Result := false;
  for I := 0 to High(FConsumedBeats) do
  begin
    if (FConsumedBeats[I].SongSeq = SongSeq) and (FConsumedBeats[I].Beat = Beat) then
    begin
      Result := true;
      Exit;
    end;
  end;
end;

procedure TRemotePlayerBuffer.UnmarkBeatConsumed(SongSeq, Beat: integer);
var
  I: integer;
  J: integer;
begin
  for I := 0 to High(FConsumedBeats) do
  begin
    if (FConsumedBeats[I].SongSeq = SongSeq) and (FConsumedBeats[I].Beat = Beat) then
    begin
      for J := I to High(FConsumedBeats) - 1 do
        FConsumedBeats[J] := FConsumedBeats[J + 1];
      SetLength(FConsumedBeats, Length(FConsumedBeats) - 1);
      Exit;
    end;
  end;
end;

procedure TRemotePlayerBuffer.MarkBeatConsumed(SongSeq, Beat: integer);
var
  Index: integer;
begin
  if IsBeatConsumed(SongSeq, Beat) then
    Exit;

  if (Length(FConsumedBeats) >= REMOTE_INPUT_MAX_FRAMES) then
  begin
    Move(FConsumedBeats[1], FConsumedBeats[0],
      (Length(FConsumedBeats) - 1) * SizeOf(TRemoteConsumedBeat));
    SetLength(FConsumedBeats, Length(FConsumedBeats) - 1);
  end;

  Index := Length(FConsumedBeats);
  SetLength(FConsumedBeats, Index + 1);
  FConsumedBeats[Index].SongSeq := SongSeq;
  FConsumedBeats[Index].Beat := Beat;
end;

function TRemotePlayerBuffer.FindBeatTone(SongSeq, Beat: integer; out StoredTone: integer): boolean;
var
  I: integer;
begin
  Result := false;
  StoredTone := REMOTE_TONE_UNVOICED;
  for I := 0 to High(FBeatTones) do
  begin
    if (FBeatTones[I].SongSeq = SongSeq) and (FBeatTones[I].Beat = Beat) then
    begin
      StoredTone := FBeatTones[I].Tone;
      Result := true;
      Exit;
    end;
  end;
end;

function TRemotePlayerBuffer.StoredToneToToneSample(StoredTone: integer; out Tone: TRemoteToneSample): boolean;
begin
  Tone.ToneValid := false;
  Tone.Tone := -1;
  Tone.ToneAbs := -1;
  Tone.Confidence := 0;
  Tone.RmsDb := -120;

  Result := StoredTone > REMOTE_TONE_VOICED_UNSTABLE;
  if not Result then
    Exit;

  Tone.ToneValid := true;
  Tone.Tone := StoredTone;
  Tone.ToneAbs := StoredTone;
  Tone.Confidence := 1;
  Tone.RmsDb := 0;
end;

function TRemotePlayerBuffer.TryGetTone(SongSeq: integer; SongTimeUs: int64; DelayUs: int64;
  out Tone: TRemoteToneSample): boolean;
var
  I: integer;
  TargetSongTimeUs: int64;
  BestIndex: integer;
  BestDistance: int64;
  BestValidDistance: int64;
  Distance: int64;
  FrameEndUs: int64;
  FrameStartUs: int64;
  FrameMidUs: int64;
  WindowStartUs: int64;
  WindowEndUs: int64;
  VoteCount: integer;
  BestVoteToneAbs: integer;
  BestVoteCount: integer;
  ToneCounts: array[0..REMOTE_INPUT_NUM_HALFTONES-1] of integer;
  ToneConfidences: array[0..REMOTE_INPUT_NUM_HALFTONES-1] of double;
  ToneRmsDb: array[0..REMOTE_INPUT_NUM_HALFTONES-1] of double;
  CandidateTone: TRemoteToneSample;
  BestTone: TRemoteToneSample;
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
  WindowStartUs := TargetSongTimeUs - (REMOTE_INPUT_VOTE_WINDOW_US div 2);
  if (WindowStartUs < 0) then
    WindowStartUs := 0;
  WindowEndUs := TargetSongTimeUs + (REMOTE_INPUT_VOTE_WINDOW_US div 2);

  SDL_LockMutex(FLock);
  try
    DropOldFrames(TargetSongTimeUs);
    BestIndex := -1;
    BestDistance := High(int64);
    BestValidDistance := High(int64);
    VoteCount := 0;
    FillChar(ToneCounts, SizeOf(ToneCounts), 0);
    FillChar(ToneConfidences, SizeOf(ToneConfidences), 0);
    FillChar(ToneRmsDb, SizeOf(ToneRmsDb), 0);

    for I := 0 to High(FFrames) do
    begin
      if (FFrames[I].SongSeq <> SongSeq) then
        Continue;

      FrameStartUs := FFrames[I].SongTimeUs;
      FrameEndUs := FFrames[I].SongTimeUs + FFrames[I].DurUs;
      if (FrameEndUs < FrameStartUs) then
        FrameEndUs := FrameStartUs;
      FrameMidUs := (FrameStartUs + FrameEndUs) div 2;

      if (FrameEndUs >= WindowStartUs) and (FrameStartUs <= WindowEndUs) then
      begin
        Inc(VoteCount);
        if FrameToTone(FFrames[I], CandidateTone) then
        begin
          Inc(ToneCounts[CandidateTone.ToneAbs]);
          ToneConfidences[CandidateTone.ToneAbs] :=
            ToneConfidences[CandidateTone.ToneAbs] + CandidateTone.Confidence;
          ToneRmsDb[CandidateTone.ToneAbs] :=
            ToneRmsDb[CandidateTone.ToneAbs] + CandidateTone.RmsDb;
        end;
      end;

      if (TargetSongTimeUs >= FFrames[I].SongTimeUs) and
        (TargetSongTimeUs <= FrameEndUs + 40000) then
      begin
        Distance := Abs(FrameMidUs - TargetSongTimeUs);
      end
      else
      begin
        Distance := Abs(FFrames[I].SongTimeUs - TargetSongTimeUs);
        if (Distance > REMOTE_INPUT_MAX_FRAME_DISTANCE_US) then
          Continue;
      end;

      if (Distance < BestDistance) then
      begin
        BestDistance := Distance;
        BestIndex := I;
      end;

      if FrameToTone(FFrames[I], CandidateTone) and (Distance < BestValidDistance) then
      begin
        BestValidDistance := Distance;
        BestTone := CandidateTone;
        Result := true;
      end;
    end;

    BestVoteToneAbs := -1;
    BestVoteCount := 0;
    for I := 0 to REMOTE_INPUT_NUM_HALFTONES - 1 do
    begin
      if (ToneCounts[I] <= 0) then
        Continue;

      if (BestVoteToneAbs < 0) then
      begin
        BestVoteToneAbs := I;
        BestVoteCount := ToneCounts[I];
        Continue;
      end;

      if (ToneCounts[I] > BestVoteCount) or
        ((ToneCounts[I] = BestVoteCount) and
         (ToneConfidences[I] / ToneCounts[I] >
          ToneConfidences[BestVoteToneAbs] / ToneCounts[BestVoteToneAbs])) then
      begin
        BestVoteToneAbs := I;
        BestVoteCount := ToneCounts[I];
      end;
    end;

    if (VoteCount > 0) then
    begin
      if (BestVoteToneAbs >= 0) then
      begin
        Tone.ToneValid := true;
        Tone.ToneAbs := BestVoteToneAbs;
        Tone.Tone := BestVoteToneAbs mod 12;
        Tone.Confidence := ToneConfidences[BestVoteToneAbs] / BestVoteCount;
        Tone.RmsDb := ToneRmsDb[BestVoteToneAbs] / BestVoteCount;
        Result := true;
        Exit;
      end;

      if Result then
      begin
        Tone := BestTone;
        Exit;
      end;
    end;

    if Result then
      Tone := BestTone
    else if (BestIndex >= 0) then
      Result := FrameToTone(FFrames[BestIndex], Tone);
  finally
    SDL_UnlockMutex(FLock);
  end;
end;

function TRemotePlayerBuffer.TryConsumeToneForBeat(SongSeq, Beat: integer; SongTimeUs: int64;
  DelayUs: int64; out Tone: TRemoteToneSample): boolean;
var
  StoredTone: integer;
begin
  SDL_LockMutex(FLock);
  try
    if IsBeatConsumed(SongSeq, Beat) then
    begin
      Result := false;
      Tone.ToneValid := false;
      Tone.Tone := -1;
      Tone.ToneAbs := -1;
      Tone.Confidence := 0;
      Tone.RmsDb := -120;
      Exit;
    end;

    if FindBeatTone(SongSeq, Beat, StoredTone) then
    begin
      Result := StoredToneToToneSample(StoredTone, Tone);
      MarkBeatConsumed(SongSeq, Beat);
      Exit;
    end;
  finally
    SDL_UnlockMutex(FLock);
  end;

  Result := TryGetTone(SongSeq, SongTimeUs, DelayUs, Tone);
  if Result and Tone.ToneValid then
  begin
    SDL_LockMutex(FLock);
    try
      MarkBeatConsumed(SongSeq, Beat);
    finally
      SDL_UnlockMutex(FLock);
    end;
  end;
end;

{ TRemoteInputProcessor }

constructor TRemoteInputProcessor.Create;
var
  I: integer;
begin
  inherited;
  FAcceptingInput := false;
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
  FAcceptingInput := false;
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

procedure TRemoteInputProcessor.AssignPlayerSlot(PlayerIndex: integer; const PlayerId, Role: UTF8String; Slot, MicDelayMs: integer);
begin
  if (GetPlayerBuffer(PlayerIndex) <> nil) then
    FPlayers[PlayerIndex].AssignPlayer(PlayerId, Role, Slot, MicDelayMs);
end;

procedure TRemoteInputProcessor.ClearPlayerSlot(PlayerIndex: integer);
begin
  if (GetPlayerBuffer(PlayerIndex) <> nil) then
    FPlayers[PlayerIndex].Clear;
end;

procedure TRemoteInputProcessor.SetPlayerDelayMs(PlayerIndex, MicDelayMs: integer);
begin
  if (GetPlayerBuffer(PlayerIndex) <> nil) then
    FPlayers[PlayerIndex].SetMicDelayMs(MicDelayMs);
end;

procedure TRemoteInputProcessor.AddPitchFrame(PlayerIndex: integer; const Frame: TRemotePitchFrame);
begin
  if FAcceptingInput and (GetPlayerBuffer(PlayerIndex) <> nil) then
    FPlayers[PlayerIndex].AddFrame(Frame);
end;

procedure TRemoteInputProcessor.AddBeatTone(PlayerIndex: integer; Beat, Tone: integer);
var
  BeatTone: TRemoteBeatTone;
begin
  if (not FAcceptingInput) or (GetPlayerBuffer(PlayerIndex) = nil) then
    Exit;

  BeatTone.SongSeq := FCurrentSongSeq;
  BeatTone.Beat := Beat;
  BeatTone.Tone := Tone;
  FPlayers[PlayerIndex].AddBeatTone(BeatTone);
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

function TRemoteInputProcessor.TryConsumeToneForBeat(PlayerIndex, Beat: integer; SongTimeUs: int64; out Tone: TRemoteToneSample): boolean;
begin
  Result := false;
  Tone.ToneValid := false;
  Tone.Tone := -1;
  Tone.ToneAbs := -1;
  Tone.Confidence := 0;
  Tone.RmsDb := -120;

  if (GetPlayerBuffer(PlayerIndex) = nil) then
    Exit;

  Result := FPlayers[PlayerIndex].TryConsumeToneForBeat(FCurrentSongSeq, Beat, SongTimeUs, FDelayUs, Tone);
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

function TRemoteInputProcessor.RequiredGameDelayMs(): integer;
var
  I: integer;
  Buffer: TRemotePlayerBuffer;
  HasRemotePlayer: boolean;
  MaxMicDelayMs: integer;
begin
  Result := 0;
  HasRemotePlayer := false;
  MaxMicDelayMs := 0;

  for I := 0 to High(FPlayers) do
  begin
    Buffer := FPlayers[I];
    if (Buffer = nil) then
      Continue;

    SDL_LockMutex(Buffer.FLock);
    try
      if Buffer.FEnabled and (Buffer.FPlayerId <> '') and
         ((Buffer.FRole = 'singer') or (Buffer.FRole = 'controller')) then
      begin
        HasRemotePlayer := true;
        MaxMicDelayMs := Max(MaxMicDelayMs, Buffer.FMicDelayMs);
      end;
    finally
      SDL_UnlockMutex(Buffer.FLock);
    end;
  end;

  if HasRemotePlayer then
    Result := Max(REMOTE_INPUT_MIN_GAME_DELAY_MS,
      MaxMicDelayMs + REMOTE_INPUT_GAME_DELAY_MARGIN_MS);
end;

end.
