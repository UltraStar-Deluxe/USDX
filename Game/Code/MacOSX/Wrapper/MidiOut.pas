unit MidiOut;

{$I switches.inc}

interface

type

  TMidiOutput = class
  public
    ProductName : String;
    Constructor Create(AParent : TObject);
    procedure PutShort(MidiMessage: Byte; Data1: Byte; Data2: Byte); virtual;
    function Open: Boolean; virtual;
    function Close: Boolean; virtual;
    {property MIDIHandle: Hmidiout read FMIDIHandle;
    property DriverVersion: Version read FDriverVersion;
    property Technology: OutPortTech read FTechnology write SetTechnology default opt_Synth;
    property Voices: Word read FVoices;
    property Notes: Word read FNotes;
    property ChannelMask: Word read FChannelMask;
    property Support: DWORD read FSupport;
    property Error: Word read FError;
    property Numdevs: Word read FNumdevs;

    procedure PutMidiEvent(theEvent: TMyMidiEvent); virtual;
    procedure PutLong(TheSysex: Pointer; msgLength: Word); virtual;
    procedure SetVolume(Left: Word; Right: Word);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ProductName: string read FProductName write SetProductName;

    property DeviceID: Integer read FDeviceID write SetDeviceID default 0;
    property Onmidioutput: TNotifyEvent read FOnmidioutput write FOnmidioutput;}
  end;

implementation

{ TMidiOutput }

function TMidiOutput.Close: Boolean;
begin

end;

constructor TMidiOutput.Create(AParent: TObject);
begin
    ProductName := 'UltraStar MidiOut Wrapper';
end;

function TMidiOutput.Open: Boolean;
begin

end;

procedure TMidiOutput.PutShort(MidiMessage, Data1, Data2: Byte);
begin

end;

end.
