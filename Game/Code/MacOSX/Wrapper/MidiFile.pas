unit MidiFile;

{$I switches.inc}

interface

type

  TMidiEvent = record
    event: byte;
    data1: byte;
    data2: byte;
    str: string;
    dticks: integer;
    time: integer;
    mtime: integer;
    len: integer;
  end;
  PMidiEvent = ^TMidiEvent;

  TOnMidiEvent = procedure(event: PMidiEvent) of object;


  TMidiTrack = class
  private
  public
    OnMidiEvent: TOnMidiEvent;
    function getEventCount: integer;
    function getEvent(index: integer): PMidiEvent;
  end;

  TMidiFile = class
  private
    FOnMidiEvent : TOnMidiEvent;
  public
    TicksPerQuarter,
    NumberOfTracks,
    BPM : Integer;
    Filename : String;
    Constructor Create(AParent : TObject);
    procedure StartPlaying;
    procedure StopPlaying;
    procedure ReadFile;
    function GetTrack(index: integer): TMidiTrack;
    function getCurrentTime: integer;
    function getTrackLength: integer;
    property OnMidiEvent: TOnMidiEvent read FOnMidiEvent write FOnMidiEvent;
  end;


implementation

{ TMidiFile }

constructor TMidiFile.Create(AParent: TObject);
begin

end;

function TMidiFile.getCurrentTime: integer;
begin

end;

function TMidiFile.GetTrack(index: integer): TMidiTrack;
begin
    Result := TMidiTrack.Create;
end;

function TMidiFile.getTrackLength: integer;
begin

end;

procedure TMidiFile.ReadFile;
begin

end;

procedure TMidiFile.StartPlaying;
begin

end;

procedure TMidiFile.StopPlaying;
begin

end;

{ TMidiTrack }

function TMidiTrack.getEvent(index: integer): PMidiEvent;
begin

end;

function TMidiTrack.getEventCount: integer;
begin

end;

end.
