{* UltraStar Deluxe - MIDI Audio Source Stream
 *
 * Generates synthesized audio from MIDI events for playback via the standard audio interface.
 *}

unit MidiAudioSourceStream;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  Classes,
  SysUtils,
  UAudioPlaybackBase,
  Math,
  UMusic,
  ULog,
  SyncObjs;

type
  TSynthEventKind = (sekNoteOn, sekNoteOff, sekStopAll);

  TScheduledSynthEvent = record
    Time: Double;
    Kind: TSynthEventKind;
    Pitch: Byte;
    Velocity: Byte;
  end;

type
  TMidiAudioSourceStream = class(TAudioSourceStream)
  private
    FFormat: TAudioFormatInfo;
    FPosition: real;
    // Synthesizer state
    FActive: Boolean;
    FNote: Integer;
    FFreq: Double;
    FVel: Double;
    FPhase: Double;
    FRelease: Boolean;
    FEnvLevel: Double;
    FNoteAge: Double;
    FReleaseAge: Double;
    FReleaseLevel: Double;
    FEvents: array of TScheduledSynthEvent;
    FLock: TCriticalSection;

    procedure QueueEvent(Time: Double; Kind: TSynthEventKind; Pitch: Byte; Velocity: Byte);
    procedure ProcessEvents(Time: Double);
    procedure ApplyNoteOn(Pitch: Byte; Velocity: Byte);
    procedure ApplyNoteOff(Pitch: Byte);
    procedure ApplyStopAll;
  protected
    function IsEOF: boolean; override;
    function IsError: boolean; override;
    function GetLength: real; override;
    function GetPosition: real; override;
    procedure SetPosition(Time: real); override;
    function GetLoop(): boolean; override;
    procedure SetLoop(Enabled: boolean); override;
  public
    constructor Create(Format: TAudioFormatInfo);
    destructor Destroy; override;
    function ReadData(Buffer: PByte; BufferSize: integer): integer; override;
    function GetAudioFormatInfo: TAudioFormatInfo; override;
    procedure Close; override;
    procedure NoteOn(Pitch: Byte; Velocity: Byte = 127; Time: Double = -1.0);
    procedure NoteOff(Pitch: Byte; Time: Double = -1.0);
    procedure StopAll(Time: Double = -1.0);
  end;

implementation

const
  OrganAttackTime = 0.004;
  OrganReleaseTime = 0.050;
  OrganGain = 0.62;
  OrganSilenceLevel = 0.0004;

function OrganEnvelope(Age: Double): Double;
begin
  if Age < OrganAttackTime then
    Result := Age / OrganAttackTime
  else
    Result := 1.0;
end;

function OrganWave(Phase, Age: Double): Double;
var
  Percussion: Double;
begin
  Percussion := Exp(-Age / 0.16);

  Result :=
    0.72 * Sin(Phase) +
    0.18 * Sin(2.0 * Phase) +
    0.10 * Sin(3.0 * Phase) +
    Percussion * (
      0.28 * Sin(4.0 * Phase) +
      0.14 * Sin(5.0 * Phase) +
      0.08 * Sin(6.0 * Phase)
    );
end;

constructor TMidiAudioSourceStream.Create(Format: TAudioFormatInfo);
begin
  FFormat := Format.Copy;
  FEnvLevel := 0;
  FPhase := 0;
  FActive := False;
  FRelease := False;
  FPosition := 0;
  FNote := -1;
  FFreq := 440.0;
  FVel := 0.0;
  FNoteAge := 0.0;
  FReleaseAge := 0.0;
  FReleaseLevel := 0.0;
  System.SetLength(FEvents, 0);
  FLock := TCriticalSection.Create;
  Log.LogDebug('MidiAudioSourceStream: Created', 'MidiSynth');
end;

destructor TMidiAudioSourceStream.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TMidiAudioSourceStream.IsEOF: boolean;
begin
  Result := False;
end;

function TMidiAudioSourceStream.IsError: boolean;
begin
  Result := false;
end;

function TMidiAudioSourceStream.GetLength: real;
begin
  Result := 0; // Not implemented
end;

function TMidiAudioSourceStream.GetPosition: real;
begin
  Result := FPosition;
end;

procedure TMidiAudioSourceStream.SetPosition(Time: real);
begin
  FLock.Enter;
  try
    FPosition := Time;
    System.SetLength(FEvents, 0);
    ApplyStopAll;
  finally
    FLock.Leave;
  end;
end;

function TMidiAudioSourceStream.ReadData(Buffer: PByte; BufferSize: integer): integer;

var
  SampleRate: Integer;
  Channels: Integer;
  Frames: Integer;
  s: PSmallInt;
  i, j: Integer;
  phaseInc, sample, sampleTime, frameTime: Double;
  outSample: SmallInt;
  tempSample: Int64;
begin
  SampleRate := Trunc(FFormat.SampleRate);
  Channels := Trunc(FFormat.Channels);
  Frames := BufferSize div (Channels * 2); // 2 bytes per sample
  s := PSmallInt(Buffer);
  sampleTime := 1.0 / SampleRate;

  FLock.Enter;
  try
    phaseInc := 2 * Pi * FFreq / SampleRate;
    for i := 0 to Frames - 1 do
    begin
      frameTime := FPosition + i * sampleTime;
      ProcessEvents(frameTime);
      phaseInc := 2 * Pi * FFreq / SampleRate;

      // Envelope
      if FRelease then
      begin
        FReleaseAge := FReleaseAge + sampleTime;
        FEnvLevel := FReleaseLevel * Exp(-FReleaseAge / OrganReleaseTime);
        if FEnvLevel <= OrganSilenceLevel then
        begin
          FEnvLevel := 0;
          FActive := False;
        end;
      end
      else if FActive then
      begin
        FNoteAge := FNoteAge + sampleTime;
        FEnvLevel := OrganEnvelope(FNoteAge);
      end;

      if FActive then
      begin
        sample := OrganWave(FPhase, FNoteAge) * FVel * FEnvLevel * OrganGain;
        // FPhase handling: keep between -Pi and +Pi
        FPhase := FPhase + phaseInc;
        if FPhase > Pi then FPhase := FPhase - 2 * Pi;
      end
      else
        sample := 0;

      // Convert to SmallInt once per frame
      tempSample := Round(sample * High(SmallInt));
      if tempSample > High(SmallInt) then
        outSample := High(SmallInt)
      else if tempSample < Low(SmallInt) then
        outSample := Low(SmallInt)
      else
        outSample := SmallInt(tempSample);
      for j := 0 to Channels - 1 do
      begin
        s^ := outSample;
        Inc(s);
      end;
    end;

    FPosition := FPosition + Frames * sampleTime;
  finally
    FLock.Leave;
  end;

  Result := BufferSize;
end;

function TMidiAudioSourceStream.GetAudioFormatInfo: TAudioFormatInfo;
begin
  Result := FFormat;
end;

procedure TMidiAudioSourceStream.Close;
begin
  if Assigned(FFormat) then
    FreeAndNil(FFormat);
  if Assigned(FLock) then
    FreeAndNil(FLock);
end;

procedure TMidiAudioSourceStream.QueueEvent(Time: Double; Kind: TSynthEventKind; Pitch: Byte; Velocity: Byte);
var
  Index: Integer;
  Count: Integer;
begin
  if Time < 0 then
    Time := FPosition
  else if Time < FPosition then
    Time := FPosition;

  Count := System.Length(FEvents);
  System.SetLength(FEvents, Count + 1);
  Index := Count;
  while (Index > 0) and (FEvents[Index - 1].Time > Time) do
  begin
    FEvents[Index] := FEvents[Index - 1];
    Dec(Index);
  end;

  FEvents[Index].Time := Time;
  FEvents[Index].Kind := Kind;
  FEvents[Index].Pitch := Pitch;
  FEvents[Index].Velocity := Velocity;
end;

procedure TMidiAudioSourceStream.ProcessEvents(Time: Double);
var
  i: Integer;
  Event: TScheduledSynthEvent;
begin
  while (System.Length(FEvents) > 0) and (FEvents[0].Time <= Time) do
  begin
    Event := FEvents[0];
    for i := 1 to High(FEvents) do
      FEvents[i - 1] := FEvents[i];
    System.SetLength(FEvents, System.Length(FEvents) - 1);

    case Event.Kind of
      sekNoteOn:
        ApplyNoteOn(Event.Pitch, Event.Velocity);
      sekNoteOff:
        ApplyNoteOff(Event.Pitch);
      sekStopAll:
        ApplyStopAll;
    end;
  end;
end;

procedure TMidiAudioSourceStream.ApplyNoteOn(Pitch: Byte; Velocity: Byte);
begin
  FNote := Pitch;
  FFreq := 440.0 * Power(2.0, (FNote - 69) / 12.0);
  FVel := Sqr(Velocity / 127.0);
  FActive := True;
  FRelease := False;
  FEnvLevel := 0;
  FPhase := 0;
  FNoteAge := 0;
  FReleaseAge := 0;
  FReleaseLevel := 0;
end;

procedure TMidiAudioSourceStream.ApplyNoteOff(Pitch: Byte);
begin
  if (FNote = Pitch) and FActive then
  begin
    FReleaseAge := 0;
    FReleaseLevel := FEnvLevel;
    FRelease := True;
  end;
end;

procedure TMidiAudioSourceStream.ApplyStopAll;
begin
  FActive := False;
  FRelease := False;
  FEnvLevel := 0;
  FVel := 0;
  FNote := -1;
  FNoteAge := 0;
  FReleaseAge := 0;
  FReleaseLevel := 0;
  System.SetLength(FEvents, 0);
end;

procedure TMidiAudioSourceStream.NoteOn(Pitch: Byte; Velocity: Byte; Time: Double);
begin
  FLock.Enter;
  try
    QueueEvent(Time, sekNoteOn, Pitch, Velocity);
    ProcessEvents(FPosition);
  finally
    FLock.Leave;
  end;
end;

procedure TMidiAudioSourceStream.NoteOff(Pitch: Byte; Time: Double);
begin
  FLock.Enter;
  try
    QueueEvent(Time, sekNoteOff, Pitch, 0);
    ProcessEvents(FPosition);
  finally
    FLock.Leave;
  end;
end;

procedure TMidiAudioSourceStream.StopAll(Time: Double);
begin
  FLock.Enter;
  try
    QueueEvent(Time, sekStopAll, 0, 0);
    ProcessEvents(FPosition);
  finally
    FLock.Leave;
  end;
end;

function TMidiAudioSourceStream.GetLoop(): boolean;
begin
  Result := False;
end;

procedure TMidiAudioSourceStream.SetLoop(Enabled: boolean);
begin
  // Do nothing, MIDI synthesis doesn't loop
end;

end.
