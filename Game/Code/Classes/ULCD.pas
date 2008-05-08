unit ULCD;

interface

{$I switches.inc}

type
  TLCD = class
    private
      Enabled:      boolean;
      Text:         array[1..6] of string;
      StartPos:     integer;
      LineBR:       integer;
      Position:     integer;
      procedure WriteCommand(B: byte);
      procedure WriteData(B: byte);
      procedure WriteString(S: string);
    public
      HalfInterface:    boolean;
      constructor Create;
      procedure Enable;
      procedure Clear;
      procedure WriteText(Line: integer; S: string);
      procedure MoveCursor(Line, Pos: integer);
      procedure ShowCursor;
      procedure HideCursor;

      // for 2x16
      procedure AddTextBR(S: string);
      procedure MoveCursorBR(Pos: integer);
      procedure ScrollUpBR;
      procedure AddTextArray(Line:integer; S: string);
  end;

var
  LCD:      TLCD;

const
  Data    = $378; // domyœlny adres portu
  Status  = Data + 1;
  Control = Data + 2;

implementation

uses
  SysUtils,
  {$IFDEF UseSerialPort}
  zlportio,
  {$ENDIF}
  SDL,
  UTime;

procedure TLCD.WriteCommand(B: Byte);
// Wysylanie komend sterujacych
begin
{$IFDEF UseSerialPort}
  if not HalfInterface then
  begin
    zlioportwrite(Control, 0, $02);
    zlioportwrite(Data, 0, B);
    zlioportwrite(Control, 0, $03);
  end
  else
  begin
    zlioportwrite(Control, 0, $02);
    zlioportwrite(Data, 0, B and $F0);
    zlioportwrite(Control, 0, $03);

    SDL_Delay( 100 );

    zlioportwrite(Control, 0, $02);
    zlioportwrite(Data, 0, (B * 16) and $F0);
    zlioportwrite(Control, 0, $03);
  end;

  if (B=1) or (B=2) then
    Sleep(2)
  else
    SDL_Delay( 100 );
{$ENDIF}
end;

procedure TLCD.WriteData(B: Byte);
// Wysylanie danych
begin
{$IFDEF UseSerialPort} 
  if not HalfInterface then
  begin
    zlioportwrite(Control, 0, $06);
    zlioportwrite(Data, 0, B);
    zlioportwrite(Control, 0, $07);
  end
  else
  begin
    zlioportwrite(Control, 0, $06);
    zlioportwrite(Data, 0, B and $F0);
    zlioportwrite(Control, 0, $07);

    SDL_Delay( 100 );

    zlioportwrite(Control, 0, $06);
    zlioportwrite(Data, 0, (B * 16) and $F0);
    zlioportwrite(Control, 0, $07);
  end;

  SDL_Delay( 100 );
  Inc(Position);
{$ENDIF}
end;

procedure TLCD.WriteString(S: string);
// Wysylanie slow
var
  I:    integer;
begin
  for I := 1 to Length(S) do
    WriteData(Ord(S[I]));
end;

constructor TLCD.Create;
begin
  inherited;
end;

procedure TLCD.Enable;
{var
  A:  byte;
  B:  byte;}
begin
  Enabled := true;
  if not HalfInterface then
    WriteCommand($38)
  else begin
    WriteCommand($33);
    WriteCommand($32);
    WriteCommand($28);
  end;

//  WriteCommand($06);
//  WriteCommand($0C);
//  sleep(10);
end;

procedure TLCD.Clear;
begin
  if Enabled then begin
  WriteCommand(1);
  WriteCommand(2);
  Text[1] := '';
  Text[2] := '';
  Text[3] := '';
  Text[4] := '';
  Text[5] := '';
  Text[6] := '';
  StartPos := 1;
  LineBR := 1;
  end;
end;

procedure TLCD.WriteText(Line: integer; S: string);
begin
  if Enabled then begin
  if Line <= 2 then begin
    MoveCursor(Line, 1);
    WriteString(S);
  end;

  Text[Line] := '';
  AddTextArray(Line, S);
  end;
end;

procedure TLCD.MoveCursor(Line, Pos: integer);
var
  I:    integer;
begin
  if Enabled then begin
  Pos := Pos + (Line-1) * 40;

  if Position > Pos then begin
    WriteCommand(2);
    for I := 1 to Pos-1 do
      WriteCommand(20);
  end;

  if Position < Pos then
    for I := 1 to Pos - Position do
      WriteCommand(20);

  Position := Pos;
  end;
end;

procedure TLCD.ShowCursor;
begin
  if Enabled then begin
  WriteCommand(14);
  end;
end;

procedure TLCD.HideCursor;
begin
  if Enabled then begin
  WriteCommand(12);
  end;
end;

procedure TLCD.AddTextBR(S: string);
var
  Word:     string;
//  W:        integer;
  P:        integer;
  L:        integer;
begin
  if Enabled then begin
  if LineBR <= 6 then begin
  L := LineBR;
  P := Pos(' ', S);

  if L <= 2 then
    MoveCursor(L, 1);

  while (L <= 6) and (P > 0) do begin
    Word := Copy(S, 1, P);
    if (Length(Text[L]) + Length(Word)-1) > 16 then begin
      L := L + 1;
      if L <= 2 then
        MoveCursor(L, 1);
    end;

    if L <= 6 then begin
      if L <= 2 then
        WriteString(Word);
      AddTextArray(L, Word);
    end;

    Delete(S, 1, P);
    P := Pos(' ', S)
  end;

  LineBR := L + 1;
  end;
  end;
end;

procedure TLCD.MoveCursorBR(Pos: integer);
{var
  I:    integer;
  L:    integer;}
begin
  if Enabled then begin
  Pos := Pos - (StartPos-1);
  if Pos <= Length(Text[1]) then
    MoveCursor(1, Pos);

  if Pos > Length(Text[1]) then begin
    // bez zawijania
//    Pos := Pos - Length(Text[1]);
//    MoveCursor(2, Pos);

    // z zawijaniem
    Pos := Pos - Length(Text[1]);
    ScrollUpBR;
    MoveCursor(1, Pos);
  end;
  end;
end;

procedure TLCD.ScrollUpBR;
var
  T:      array[1..5] of string;
  SP:     integer;
  LBR:    integer;
begin
  if Enabled then begin
  T[1] := Text[2];
  T[2] := Text[3];
  T[3] := Text[4];
  T[4] := Text[5];
  T[5] := Text[6];
  SP := StartPos + Length(Text[1]);
  LBR := LineBR;

  Clear;

  StartPos := SP;
  WriteText(1, T[1]);
  WriteText(2, T[2]);
  WriteText(3, T[3]);
  WriteText(4, T[4]);
  WriteText(5, T[5]);
  LineBR := LBR-1;
  end;
end;

procedure TLCD.AddTextArray(Line: integer; S: string);
begin
  if Enabled then begin
  Text[Line] := Text[Line] + S;
  end;
end;

end.

