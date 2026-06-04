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
  SyncObjs,
  MidiCons;

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
    FLock: TCriticalSection;
  protected
    function IsEOF: boolean; override;
    function IsError: boolean; override;
    function GetLength: real; override;
    function GetPosition: real; override;
    procedure SetPosition(Time: real); override;
    function GetLoop(): boolean; override;
    procedure SetLoop(Enabled: boolean); override;
  public
    constructor Create(MidiFile: Pointer; Format: TAudioFormatInfo); // MidiFile unused for now
    function ReadData(Buffer: PByte; BufferSize: integer): integer; override;
    function GetAudioFormatInfo: TAudioFormatInfo; override;
    procedure Close; override;
    procedure HandleMidiEvent(MidiMessage: byte; Data1: byte; Data2: byte);
  end;

implementation

const
  PianoAttackTime = 0.003;
  PianoDecayTime = 1.35;
  PianoReleaseTime = 0.080;
  PianoGain = 0.58;
  PianoSilenceLevel = 0.0004;

function PianoEnvelope(Age: Double): Double;
begin
  if Age < PianoAttackTime then
    Result := Age / PianoAttackTime
  else
    Result := Exp(-(Age - PianoAttackTime) / PianoDecayTime);
end;

function PianoWave(Phase, Age: Double): Double;
var
  Bright: Double;
begin
  Bright := Exp(-Age / 0.45);

  Result :=
    Sin(Phase) +
    0.22 * Bright * Sin(2.001 * Phase) +
    0.12 * Bright * Sin(3.006 * Phase) +
    0.06 * Bright * Sin(4.014 * Phase) +
    0.03 * Bright * Sin(5.025 * Phase);
end;

constructor TMidiAudioSourceStream.Create(MidiFile: Pointer; Format: TAudioFormatInfo);
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
  FLock := TCriticalSection.Create;
  Log.LogDebug('MidiAudioSourceStream: Created', 'MidiSynth');
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
  FPosition := Time;
end;

function TMidiAudioSourceStream.ReadData(Buffer: PByte; BufferSize: integer): integer;

var
  SampleRate: Integer;
  Channels: Integer;
  Frames: Integer;
  s: PSmallInt;
  i, j: Integer;
  phaseInc, sample, sampleTime: Double;
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
      // Envelope
      if FRelease then
      begin
        FReleaseAge := FReleaseAge + sampleTime;
        FEnvLevel := FReleaseLevel * Exp(-FReleaseAge / PianoReleaseTime);
        if FEnvLevel <= PianoSilenceLevel then
        begin
          FEnvLevel := 0;
          FActive := False;
        end;
      end
      else if FActive then
      begin
        FNoteAge := FNoteAge + sampleTime;
        FEnvLevel := PianoEnvelope(FNoteAge);
      end;

      if FActive then
      begin
        sample := PianoWave(FPhase, FNoteAge) * FVel * FEnvLevel * PianoGain;
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
  FreeAndNil(FFormat);
  FreeAndNil(FLock);
end;

procedure TMidiAudioSourceStream.HandleMidiEvent(MidiMessage: byte; Data1: byte; Data2: byte);
begin
  FLock.Enter;
  try
    if ((((MidiMessage and $F0) = MIDI_CONTROLCHANGE) and
         ((Data1 = MIDI_ALLNOTESOFF) or (Data1 = $78))) or
        (MidiMessage = MIDI_STOP) or
        (MidiMessage = MIDI_SYSTEMRESET)) then
    begin
      FActive := False;
      FRelease := False;
      FEnvLevel := 0;
      FVel := 0;
      FNote := -1;
      FNoteAge := 0;
      FReleaseAge := 0;
      FReleaseLevel := 0;
      Exit;
    end;

    // NOTE ON
    if ((MidiMessage and $F0) = MIDI_NOTEON) and (Data2 <> 0) then
    begin
      FNote := Data1;
      FFreq := 440.0 * Power(2.0, (FNote - 69)/12.0);
      // Velocity mapping: amplitude proportional to square of velocity
      FVel := Sqr(Data2 / 127.0);
      FActive := True;
      FRelease := False;
      FEnvLevel := 0;
      FPhase := 0;
      FNoteAge := 0;
      FReleaseAge := 0;
      FReleaseLevel := 0;
      Exit;
    end;
    // NOTE OFF
    if ((((MidiMessage and $F0) = MIDI_NOTEON) and (Data2 = 0)) or ((MidiMessage and $F0) = MIDI_NOTEOFF)) and (FNote = Data1) then
    begin
      FReleaseAge := 0;
      FReleaseLevel := FEnvLevel;
      FRelease := True;
    end;
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
