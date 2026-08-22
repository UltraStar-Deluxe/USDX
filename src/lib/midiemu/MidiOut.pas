{* UltraStar Deluxe - editor preview synth. *}

unit MidiOut;

interface

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+}
{$ENDIF}

uses
  Classes,
  SysUtils,
  ULog,
  UMusic,
  Math,
  MidiAudioSourceStream,
  UAudioPlaybackBase;

type
  TMidiOutput = class(TComponent)
  private
    FProductName: string;
    FFormat: TAudioFormatInfo;
    FOpened: Boolean;
    FPlaybackStream: TAudioPlaybackStream;
    FSourceStream: TMidiAudioSourceStream;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Open: boolean; virtual;
    function Close: boolean; virtual;

    procedure SetVolume(Volume: Single);
    procedure NoteOn(Pitch: Byte; Velocity: Byte = 127; Time: Double = -1.0);
    procedure NoteOff(Pitch: Byte; Time: Double = -1.0);
    procedure Click(Velocity: Byte = 127; Time: Double = -1.0);
    procedure QueueNoteOn(Pitch: Byte; Velocity: Byte = 127; Time: Double = -1.0);
    procedure QueueNoteOff(Pitch: Byte; Time: Double = -1.0);
    procedure QueueClick(Velocity: Byte = 127; Time: Double = -1.0);
    procedure StopAll(Time: Double = -1.0);
    procedure SetPosition(Time: Double);
    procedure Play;
    procedure Stop;

    property ProductName: string read FProductName write FProductName;
  end;

procedure Register;

implementation

constructor TMidiOutput.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProductName := 'MIDI Emulation (synth)';
  FOpened := False;
  FPlaybackStream := nil;
  FSourceStream := nil;
end;

destructor TMidiOutput.Destroy;
begin
  if Assigned(FPlaybackStream) then
    FreeAndNil(FPlaybackStream);
  if Assigned(FSourceStream) then
    FreeAndNil(FSourceStream);
  FreeAndNil(FFormat);
  inherited Destroy;
end;

function TMidiOutput.Open: boolean;
var AP: IAudioPlayback;
begin
  Result := False;
  AP := AudioPlayback;
  if AP = nil then
  begin
    Log.LogWarn('MidiEmu: AudioPlayback not initialized', 'MidiEmu');
    Exit;
  end;
  try
    FreeAndNil(FFormat);
    FFormat := TAudioFormatInfo.Create(1, 44100, asfS16);
    if Assigned(FPlaybackStream) then
    begin
      FPlaybackStream.Stop;
      FreeAndNil(FPlaybackStream);
    end;
    if Assigned(FSourceStream) then FreeAndNil(FSourceStream);
    FSourceStream := TMidiAudioSourceStream.Create(FFormat);
    FPlaybackStream := AP.CreatePlaybackStreamForSource(FSourceStream);
    if not Assigned(FPlaybackStream) then
    begin
      Log.LogError('MidiEmu: CreatePlaybackStreamForSource failed', 'MidiEmu');
      Exit;
    end;
    Result := True;
    FOpened := True;
    Log.LogDebug('MidiEmu: synthesized MIDI stream opened', 'MidiEmu');
  except
    on E: Exception do
      Log.LogError('MidiEmu: Open failed: ' + E.Message, 'MidiEmu');
  end;
end;

function TMidiOutput.Close: boolean;
begin
  Result := True;
  FOpened := False;
  if Assigned(FPlaybackStream) then
  begin
    FPlaybackStream.Stop;
    FreeAndNil(FPlaybackStream);
  end;
  if Assigned(FSourceStream) then
    FreeAndNil(FSourceStream);
end;

procedure TMidiOutput.SetVolume(Volume: Single);
begin
  if Volume < 0 then
    Volume := 0
  else if Volume > 1 then
    Volume := 1;

  if Assigned(FPlaybackStream) then
    FPlaybackStream.Volume := Sqr(Volume);
end;

procedure TMidiOutput.NoteOn(Pitch: Byte; Velocity: Byte; Time: Double);
begin
  QueueNoteOn(Pitch, Velocity, Time);
  Play;
end;

procedure TMidiOutput.NoteOff(Pitch: Byte; Time: Double);
begin
  QueueNoteOff(Pitch, Time);
end;

procedure TMidiOutput.Click(Velocity: Byte; Time: Double);
begin
  QueueClick(Velocity, Time);
  Play;
end;

procedure TMidiOutput.QueueNoteOn(Pitch: Byte; Velocity: Byte; Time: Double);
begin
  if Assigned(FSourceStream) then
    FSourceStream.NoteOn(Pitch, Velocity, Time);
end;

procedure TMidiOutput.QueueNoteOff(Pitch: Byte; Time: Double);
begin
  if Assigned(FSourceStream) then
    FSourceStream.NoteOff(Pitch, Time);
end;

procedure TMidiOutput.QueueClick(Velocity: Byte; Time: Double);
begin
  if Assigned(FSourceStream) then
    FSourceStream.Click(Velocity, Time);
end;

procedure TMidiOutput.StopAll(Time: Double);
begin
  if Assigned(FSourceStream) then
    FSourceStream.StopAll(Time);
end;

procedure TMidiOutput.SetPosition(Time: Double);
begin
  if Assigned(FPlaybackStream) then
    FPlaybackStream.Position := Time
  else if Assigned(FSourceStream) then
    FSourceStream.Position := Time;
end;

procedure TMidiOutput.Play;
begin
  if Assigned(FPlaybackStream) then
    FPlaybackStream.Play;
end;

procedure TMidiOutput.Stop;
begin
  if Assigned(FPlaybackStream) then
    FPlaybackStream.Stop;
end;

procedure Register;
begin
end;

end.
