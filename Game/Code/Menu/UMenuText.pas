unit UMenuText;

interface
uses TextGL, UTexture, OpenGL12, SysUtils;

type
  TText = class
    private
      SelectBool:   boolean;
    public
      X:      real;
      Y:      real;
//      W:      real;       // if text is wider than W then it is streched (not yet implemented)
//      H:      real;
      Size:   real;
      Text:   string;
      ColR:   real;
      ColG:   real;
      ColB:   real;
      Int:    real;
      Style:  integer;
      Visible:  boolean;
      Align:    integer; // 0 = left, 1 = center, 2 = right

      procedure SetSelect(Value: Boolean);
      property Selected: Boolean read SelectBool write SetSelect;

      procedure Draw;
      constructor Create; overload;
      constructor Create(X, Y: real; Tekst: string); overload;
      constructor Create(ParX, ParY: real; ParStyle: integer; ParSize, ParColR, ParColG, ParColB: real; ParAlign: integer; ParTekst: string); overload;
  end;

implementation
uses UGraphic;

procedure TText.SetSelect(Value: Boolean);
begin
  SelectBool := Value;
end;

procedure TText.Draw;
var
  X2:     real;
  Text2:  string;
begin
  if Visible then begin
    SetFontStyle(Style);
    SetFontSize(Size);
    glColor3f(ColR*Int, ColG*Int, ColB*Int);
    if not SelectBool then
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
  end;
end;

constructor TText.Create;
begin
  Create(0, 0, '');
end;

constructor TText.Create(X, Y: real; Tekst: string);
begin
  Create(X, Y, 0, 10, 0, 0, 0, 0, Tekst);
end;

constructor TText.Create(ParX, ParY: real; ParStyle: integer; ParSize, ParColR, ParColG, ParColB: real; ParAlign: integer; ParTekst: string);
begin
  inherited Create;
  X := ParX;
  Y := ParY;
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
