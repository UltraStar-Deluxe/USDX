unit UMenuText;

interface
uses TextGL, UTexture, OpenGL12, SysUtils;

type
  TText = class
    private
      SelectBool:   boolean;
      TextString:   String;
      TextTiles:    Array of String;

      STicks:       Cardinal;
      SelectBlink:  Boolean;
    public
      X:      real;
      Y:      real;
      W:      real;       // if text is wider than W then it is breaked
//      H:      real;
      Size:   real;
      ColR:   real;
      ColG:   real;
      ColB:   real;
      Int:    real;
      Style:  integer;
      Visible:  boolean;
      Align:    integer; // 0 = left, 1 = center, 2 = right

      procedure SetSelect(Value: Boolean);
      property Selected: Boolean read SelectBool write SetSelect;

      procedure SetText(Value: String);
      property  Text: String read TextString write SetText;

      procedure DeleteLastL; //Procedure to Delete Last Letter

      procedure Draw;
      constructor Create; overload;
      constructor Create(X, Y: real; Tekst: string); overload;
      constructor Create(ParX, ParY, ParW: real; ParStyle: integer; ParSize, ParColR, ParColG, ParColB: real; ParAlign: integer; ParTekst: string); overload;
  end;

implementation
uses UGraphic, StrUtils, Windows;

procedure TText.SetSelect(Value: Boolean);
begin
  SelectBool := Value;
  
  //Set Cursor Visible
  SelectBlink := True;
  STicks := GettickCount div 550;
end;

procedure TText.SetText(Value: String);
var
  I: Integer;
  L: Integer;
  LastPos: Integer;
  LastBreak: Integer;
begin
  TextString := Value;

  if (W > 0) then
  begin
    //Set Font Propertys
    SetFontStyle(Style);
    SetFontSize(Size);

    //Create New TextTiles Array
    SetLength (TextTiles, 0);
    L := 0;

    LastPos := 1;
    LastBreak := 0;
    I := Pos (' ', Value);
    While (I <> 0) do
    begin
      if (glTextWidth(PChar(Copy (Value,LastBreak + 1,I))) > W) AND (LastPos <> 1) then
      begin
        //new Break
        SetLength (TextTiles, L+1);
        TextTiles[L] := Copy (Value, LastBreak + 1, LastPos - LastBreak);

        Inc(L);
        LastBreak := LastPos;
      end;
      
      LastPos := I;
      I := PosEx (' ', Value, I+1);
    end;

    //Last Break
    if (glTextWidth(PChar(Copy (Value,LastBreak + 1,Length(Value) - LastBreak))) > W) AND (LastPos <> 1) then
      begin
        //new Break
        SetLength (TextTiles, L+1);
        TextTiles[L] := Copy (Value, LastBreak + 1, LastPos - LastBreak);

        Inc(L);
        LastBreak := LastPos;
      end;

      //last Part
      SetLength (TextTiles, L+1);
      TextTiles[L] := Copy (Value, LastBreak + 1, Length(Value) - LastBreak);

  end;

  //Set Cursor Visible
  SelectBlink := True;
  STicks := GettickCount div 550;
end;

Procedure TText.DeleteLastL;
var
  S: String;
  L: Integer;
begin
  S := TextString;
  L := Length(S);
  if (L > 0) then
    SetLength(S, L-1);

  SetText(S);
end;

procedure TText.Draw;
var
  X2, Y2:     real;
  Text2:  string;
  I:      Integer;
begin
  if Visible then begin
    SetFontStyle(Style);
    SetFontSize(Size);
    SetFontItalic(False);
    glColor3f(ColR*Int, ColG*Int, ColB*Int);

    //If Selected Set Blink...
    if SelectBool then
    begin
      I := Gettickcount div 550;
      if I <> STicks then
      begin //Change Visability
        STicks := I;
        SelectBlink := Not SelectBlink;
      end;
    end;

    if (W <= 0) then //No Width set Draw as one Long String
    begin
      if not (SelectBool AND SelectBlink) then
        Text2 := Text
      else
        Text2 := Text + '|';

      case Align of
        0: X2 := X;
        1: X2 := X - glTextWidth(pchar(Text2))/2;
        2: X2 := X - glTextWidth(pchar(Text2));
      end;

      SetFontPos(X2, Y);
      glPrint(PChar(Text2));
      SetFontStyle(0); // reset to default
    end
    else
    begin //Draw Text as Many Strings
      Y2 := Y;
      for I := 0 to high(TextTiles) do
      begin
        if (not (SelectBool AND SelectBlink)) OR (I <> high(TextTiles)) then
          Text2 := TextTiles[I]
        else
          Text2 := TextTiles[I] + '|';

        case Align of
          0: X2 := X;
          1: X2 := X - glTextWidth(pchar(Text2))/2;
          2: X2 := X - glTextWidth(pchar(Text2));
        end;

        SetFontPos(X2, Y2);
        glPrint(PChar(Text2));

        Y2 := Y2 + Size * 1.7;
      end;
      SetFontStyle(0); // reset to default

    end;
  end;
end;

constructor TText.Create;
begin
  Create(0, 0, '');
end;

constructor TText.Create(X, Y: real; Tekst: string);
begin
  Create(X, Y, 0, 0, 10, 0, 0, 0, 0, Tekst);
end;

constructor TText.Create(ParX, ParY, ParW: real; ParStyle: integer; ParSize, ParColR, ParColG, ParColB: real; ParAlign: integer; ParTekst: string);
begin
  inherited Create;
  X := ParX;
  Y := ParY;
  W := ParW;
  Style := ParStyle;
  Size := ParSize;
  Text := ParTekst;
  ColR := ParColR;
  ColG := ParColG;
  ColB := ParColB;
  Int := 1;
  Align := ParAlign;
  SelectBool := false;
  Visible := true;
end;


end.
