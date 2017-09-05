unit MidiOut;

interface

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+} // use long strings
{$ENDIF}

uses
  portmidi;

type
  TMidiOutput = class
  protected
    FProductName: string; { product name }
    stream: PortMidiStream;
    id: PmDeviceID;

  public
    procedure Open;
    procedure Close;
    procedure PutShort(MidiMessage: byte; Data1: byte; Data2: byte);
    constructor Create(AOwner: pointer);
    destructor Destroy; override;

  published
    property ProductName: string read FProductName;
  end;

implementation

constructor Tmidioutput.Create(AOwner: pointer);
var
  info: PPmDeviceInfo;
begin
  inherited Create;
  id := Pm_GetDefaultOutputDeviceID;
  if id = -1 then
    id := 0;
  info :=Pm_GetDeviceInfo(id);
  if info = nil then
    id := -1
  else
    FProductName := info^.name;
  stream := nil;
end;

destructor Tmidioutput.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure Tmidioutput.Open;
begin
  if id <> -1 then
    Pm_OpenOutput(@stream, id, nil, 16, nil, nil, 0);
end;

procedure TMidiOutput.PutShort(MidiMessage: byte; Data1: byte; Data2: byte);
begin
  if stream <> nil then
    Pm_WriteShort(stream, 0, Pm_Message(MidiMessage, Data1, Data2));
end;

procedure Tmidioutput.Close;
begin
  if stream <> nil then
  begin
    Pm_Close(stream);
    stream := nil;
  end;
end;

begin
  Pm_Initialize;
end.

