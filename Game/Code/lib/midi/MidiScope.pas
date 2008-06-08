{
  Shows a large black area where midi note/controller events are shown
  just to monitor midi activity (for the MidiPlayer)

  version 1.0 first release

  for comments/bugs
  F.Bouwmans
  fbouwmans@spiditel.nl

  if you think this component is nice and you use it, sent me a short email.
  I've seen that other of my components have been downloaded a lot, but I've
  got no clue wether they are actually used.
  Don't worry because you are free to use these components
}

unit MidiScope;

interface

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+} // use AnsiString
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TMidiScope = class(TGraphicControl)
  private
    { Private declarations }
  protected
    { Protected declarations }
    notes       : array[0..15,0..127] of integer;
    controllers : array[0..15,0..17]  of integer;
    aftertouch  : array[0..15,0..127] of integer;

    selectedChannel : integer;

    procedure PaintSlide(ch,pos,val: integer);

    procedure NoteOn(channel, note, speed : integer);
    procedure Controller(channel,number,value : integer);
    procedure AfterTch(channel, note, value : integer);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure MidiEvent(event,data1,data2 : integer);
    procedure Paint; override;
  published
    { Published declarations }
  end;


procedure Register;

const
  BarHeight = 16;
  BarHeightInc  = BarHeight+2;
  BarWidth      = 3;
  BarWidthInc   = BarWidth+1;
  HeightDiv     = 128 div BarHeight;

implementation

uses Midicons;

procedure Register;
begin
  RegisterComponents('Synth', [TMidiScope]);
end;

constructor TMidiScope.Create(AOwner: TComponent);
var
 i,j : integer;
begin
  inherited Create(AOwner);
  Height := BarHeightinc * 16 + 4;
  Width :=  147*BarWidthInc + 4 + 20;  // for channel number
  for i := 0 to 15 do
  begin
    for j := 0 to 127 do
    begin
      notes[i,j] := 0;
      aftertouch[i,j] := 0;
    end;
  end;
  for i := 0 to 17 do
  begin
    for j := 0 to 15 do
      controllers[i,j] := 0;
  end;
end;

procedure TMidiScope.PaintSlide(ch,pos,val: integer);
var x,y:integer;
begin
  Canvas.Brush.Color := clBlack;
  Canvas.Pen.color := clBlack;
  x := pos * BarWidthInc + 2;
  y := 2 + ch * BarHeightInc;
  Canvas.Rectangle(x, y, x+BarWidthInc, y+BarHeightInc);
  Canvas.Brush.Color := clGreen;
  Canvas.Pen.Color := clGreen;
  Canvas.Rectangle(x, y + (BarHeight - (val div HeightDiv )), x + BarWidth, y + BarHeight)
end;

procedure TMidiScope.Paint;
var i,j : integer;
x : integer;
begin
  Canvas.Brush.color := clBlack;
  Canvas.Rectangle(0,0,Width,Height);
  Canvas.Pen.Color := clGreen;
  x := 128*BarWidthInc+2;
  Canvas.MoveTo(x,0);
  Canvas.LineTo(x,Height);
  x :=  148*BarWIdthInc+2;
  canvas.Font.Color := clGreen;
  for i := 0 to 15 do
    Canvas.TextOut(x,((i+1)*BarHeightInc) - Canvas.font.size-3,IntToStr(i+1));
  canvas.Pen.color := clBlack;
  begin
    for j := 0 to 127 do
    begin
      PaintSlide(i,j,notes[i,j]);
    end;
    for j := 0 to 17 do
    begin
      PaintSlide(i,j+129,controllers[i,j]);
    end;
  end;
end;
procedure TMidiScope.NoteOn(channel, note, speed : integer);
begin
  notes[channel,note] := speed;
  PaintSlide(channel,note,notes[channel,note]);
end;
procedure TMidiScope.AfterTch(channel, note, value : integer);
begin
  aftertouch[channel,note] := value;
end;

procedure TMidiScope.Controller(channel,number,value : integer);
var i : integer;
begin
  if number < 18 then
  begin
  controllers[channel,number] := value;
  PaintSlide(channel,number+129,value);
  end
  else if number >= $7B then
  begin
    // all notes of for channel
    for i := 0 to 127 do
    begin
      if notes[channel,i] > 0 then
      begin
        notes[channel,i] := 0;
        PaintSlide(channel,i,0);
      end;
    end;
  end;
end;

procedure TMidiScope.MidiEvent(event,data1,data2 : integer);
begin
  case (event AND $F0) of
  MIDI_NOTEON :
  begin
    NoteOn((event AND $F),data1,data2);
  end;
  MIDI_NOTEOFF:
  begin
    NoteOn((event AND $F),data1,0);
  end;
  MIDI_CONTROLCHANGE :
  begin
    Controller((event AND $F),data1,data2);
  end;
  MIDI_CHANAFTERTOUCH:
  begin
    Controller((Event AND $F),16,Data1);
  end;
  MIDI_PITCHBEND:
  begin
  begin
    Controller((Event AND $F),17,data2);
  end;
  end;
  MIDI_KEYAFTERTOUCH:
  begin
  end;
  end;
end;
end.
