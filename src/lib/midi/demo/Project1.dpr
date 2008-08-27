program Project1;

uses
  Forms,
  MidiTest in 'MidiTest.pas' {MidiPlayer};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMidiPlayer, MidiPlayer);
  Application.Run;
end.
