// Test application for TMidiFile

unit MidiTest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, MidiFile,  ExtCtrls, MidiOut, MidiType, MidiScope, Grids;
type
  TMidiPlayer = class(TForm)
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    Button3: TButton;
    Button4: TButton;
    MidiOutput1: TMidiOutput;
    cmbInput: TComboBox;
    MidiFile1: TMidiFile;
    MidiScope1: TMidiScope;
    Label3: TLabel;
    edtBpm: TEdit;
    Memo2: TMemo;
    edtTime: TEdit;
    Button2: TButton;
    TrackGrid: TStringGrid;
    TracksGrid: TStringGrid;
    edtLength: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure MidiFile1MidiEvent(event: PMidiEvent);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmbInputChange(Sender: TObject);
    procedure MidiFile1UpdateEvent(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure edtBpmKeyPress(Sender: TObject; var Key: Char);
    procedure TracksGridSelectCell(Sender: TObject; Col, Row: Integer;
      var CanSelect: Boolean);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    MidiOpened : boolean;
    procedure SentAllNotesOff;

    procedure MidiOpen;
    procedure MidiClose;

  public
    { Public declarations }
  end;

var
  MidiPlayer: TMidiPlayer;

implementation

{$R *.DFM}

procedure TMidiPlayer.Button1Click(Sender: TObject);
var
  i,j: integer;
  track : TMidiTrack;
  event : PMidiEvent;
begin
  if opendialog1.execute then
  begin
    midifile1.filename := opendialog1.filename;
    midifile1.readfile;
//    label1.caption := IntToStr(midifile1.NumberOfTracks);
    edtBpm.text := IntToStr(midifile1.Bpm);
//    TracksGrid.cells.clear;
    for i := 0 to midifile1.NumberOfTracks-1 do
    begin
      track := midifile1.getTrack(i);
      TracksGrid.cells[0,i] := 'Tr: '+ track.getName + ' '+ track.getInstrument ;
    end;
    edtLength.Text := MyTimeToStr(MidiFile1.GetTrackLength);
  end;
end;

procedure TMidiPlayer.MidiFile1MidiEvent(event: PMidiEvent);
var mEvent : TMyMidiEvent;
begin
  mEvent := TMyMidiEvent.Create;
  if not (event.event = $FF) then
  begin
    mEvent.MidiMessage := event.event;
    mEvent.data1 := event.data1;
    mEvent.data2 := event.data2;
    midioutput1.PutMidiEvent(mEvent);
  end
  else
  begin
    if (event.data1 >= 1) and (event.data1 < 15) then
    begin
      memo2.Lines.add(IntToStr(event.data1) + ' '+ event.str);
    end
  end;
  midiScope1.MidiEvent(event.event,event.data1,event.data2);
  mEvent.Destroy;
end;

procedure TMidiPlayer.SentAllNotesOff;
var mEvent : TMyMidiEvent;
channel : integer;
begin
  mEvent := TMyMidiEvent.Create;
  for channel:= 0 to 15 do
  begin
    mEvent.MidiMessage := $B0 + channel;
    mEvent.data1 := $78;
    mEvent.data2 := 0;
    if MidiOpened then
      midioutput1.PutMidiEvent(mEvent);
    midiScope1.MidiEvent(mEvent.MidiMessage,mEvent.data1,mEvent.data2);
  end;
  mEvent.Destroy;
end;

procedure TMidiPlayer.Button3Click(Sender: TObject);
begin
  midifile1.StartPlaying;
end;

procedure TMidiPlayer.Button4Click(Sender: TObject);
begin
  midifile1.StopPlaying;
  SentAllNotesOff;
end;

procedure TMidiPlayer.MidiOpen;
begin
  if not (cmbInput.Text = '') then
  begin
    MidiOutput1.ProductName := cmbInput.Text;
    MidiOutput1.OPEN;
    MidiOpened := true;
  end;
end;

procedure TMidiPlayer.MidiClose;
begin
  if MidiOpened then
  begin
    MidiOutput1.Close;
    MidiOpened := false;
  end;
end;


procedure TMidiPlayer.FormCreate(Sender: TObject);
var thisDevice : integer;
begin
  for thisDevice := 0 to MidiOutput1.NumDevs - 1 do
  begin
    MidiOutput1.DeviceID := thisDevice;
    cmbInput.Items.Add(MidiOutput1.ProductName);
  end;
  cmbInput.ItemIndex := 0;
  MidiOpened := false;
  MidiOpen;
end;

procedure TMidiPlayer.cmbInputChange(Sender: TObject);
begin
  MidiClose;
  MidiOPen;
end;

procedure TMidiPlayer.MidiFile1UpdateEvent(Sender: TObject);
begin
  edtTime.Text := MyTimeToStr(MidiFile1.GetCurrentTime);
  edtTime.update;
  if MidiFile1.ready then
  begin
    midifile1.StopPlaying;
    SentAllNotesOff;
  end;
end;

procedure TMidiPlayer.Button2Click(Sender: TObject);
begin
  MidiFile1.ContinuePlaying;
end;

procedure TMidiPlayer.edtBpmKeyPress(Sender: TObject; var Key: Char);
begin
 if Key = char(13) then
 begin
   MidiFile1.Bpm := StrToInt(edtBpm.Text);
   edtBpm.text := IntToStr(midifile1.Bpm);
   abort;
 end;

end;

procedure TMidiPlayer.TracksGridSelectCell(Sender: TObject; Col,
  Row: Integer; var CanSelect: Boolean);
var
  MidiTrack : TMidiTrack;
  i         : integer;
  j         : integer;
  event     : PMidiEvent;
begin
  CanSelect := false;
  if Row < MidiFile1.NumberOfTracks then
  begin
    CanSelect := true;
    MidiTrack := MidiFile1.GetTrack(Row);
    TrackGrid.RowCount := 2;
    TrackGrid.RowCount := MidiTrack.getEventCount;
    j := 1;
    for i := 0 to MidiTrack.GetEventCount-1 do
    begin
      event := MidiTrack.getEvent(i);
      if not (event.len = -1) then
      begin // do not print when
        TrackGrid.cells[0,j] := IntToStr(i);
        TrackGrid.cells[1,j] := MyTimeToStr(event.time);
        TrackGrid.cells[2,j] := IntToHex(event.event,2);
        if not (event.event = $FF) then
        begin
          TrackGrid.cells[3,j] := IntToStr(event.len);
          TrackGrid.cells[4,j] := KeyToStr(event.data1);
          TrackGrid.cells[5,j] := IntToStr(event.data2);
        end
        else
        begin
          TrackGrid.cells[3,j] := IntToStr(event.data1);
          TrackGrid.cells[4,j] := '';
          TrackGrid.cells[5,j] := event.str;
        end;
        inc(j);
      end;
    end;
    TrackGrid.RowCount := j;
  end;
end;

procedure TMidiPlayer.FormShow(Sender: TObject);
begin
  TrackGrid.ColWidths[0] := 30;
  TrackGrid.ColWidths[2] := 30;
  TrackGrid.ColWidths[3] := 30;
  TrackGrid.ColWidths[4] := 30;
  TrackGrid.ColWidths[5] := 100;
end;

end.
