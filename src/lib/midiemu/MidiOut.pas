{* UltraStar Deluxe - MIDI Emulation (synth)
   Drop-in replacement for MidiOut unit. *}

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
  UPathUtils,
  UMusic,
  Math,
  SyncObjs,
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
    FLock: TCriticalSection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Open: boolean; virtual;
    function Close: boolean; virtual;

    procedure PutShort(MidiMessage: byte; Data1: byte; Data2: byte); virtual;
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
  FLock := TCriticalSection.Create;
end;

destructor TMidiOutput.Destroy;
begin
  if Assigned(FPlaybackStream) then
    FreeAndNil(FPlaybackStream);
  if Assigned(FSourceStream) then
    FreeAndNil(FSourceStream);
  FreeAndNil(FFormat);
  FreeAndNil(FLock);
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
    if Assigned(FSourceStream) then FreeAndNil(FSourceStream);
    FSourceStream := TMidiAudioSourceStream.Create(nil, FFormat); // nil for now, can pass TMidiFile if needed
    if Assigned(FPlaybackStream) then FreeAndNil(FPlaybackStream);
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

procedure TMidiOutput.PutShort(MidiMessage: byte; Data1: byte; Data2: byte);
begin
  if ((MidiMessage and $F0) = $90) and (Data2 <> 0) then
  begin
    Play;
  end;
  
  if Assigned(FSourceStream) then
    FSourceStream.HandleMidiEvent(MidiMessage, Data1, Data2);
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
