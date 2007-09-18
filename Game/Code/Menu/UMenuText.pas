unit UMenuText;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

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
      MoveX:  real;       //Some Modifier for X - Position that don't Affect the Real Y
      MoveY:  real;       //Some Modifier for Y - Position that don't Affect the Real Y
      W:      real;       // if text is wider than W then it is breaked
//      H:      real;
      Size:   real;
      ColR:   real;
      ColG:   real;
      ColB:   real;
      Alpha:  real;
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
  NextPos: Cardinal;    //NextPos of a Space etc.
  LastPos: Cardinal;    //LastPos "
  LastBreak: Cardinal;  //Last Break
  isBreak: Boolean;     //True if the Break is not Caused because the Text is out of the area
  FirstWord: Word;      //Is First Word after Break?
  Len:       Word;      //Length of the Tiles Array
  Function Smallest(const A, B: Cardinal):Cardinal;
  begin
    if (A < B) then
      Result := A
    else
      Result := B;
  end;

  Function GetNextPos: Boolean;
  var
    T1, T2, T3: Cardinal;
  begin
    LastPos := NextPos;

    //Next Space (If Width is given)
    if (W > 0) then
      T1 := PosEx(' ', Value, LastPos + 1)
    else T1 := Length(Value);

    {//Next -
    T2 := PosEx('-', Value, LastPos + 1);}

    //Next Break
    T3 := PosEx('\n', Value, LastPos + 1);

    if T1 = 0 then
      T1 := Length(Value);
    {if T2 = 0 then
      T2 := Length(Value); }
    if T3 = 0 then
      T3 := Length(Value);

    //Get Nearest Pos
    NextPos := Smallest(T1, T3{Smallest(T2, T3)});

    if (LastPos = Length(Value)) then
      NextPos := 0;

    isBreak := (NextPos = T3) AND (NextPos <> Length(Value));
    Result := (NextPos <> 0);
  end;
  procedure AddBreak(const From, bTo: Cardinal);
  begin
    if (isBreak) OR (bTo - From >= 1) then
    begin
      Inc(Len);
      SetLength (TextTiles, Len);
      TextTiles[Len-1] := Trim(Copy(Value, From, bTo - From));

      if isBreak then
        LastBreak := bTo + 2
      else
        LastBreak := bTo + 1;
      FirstWord := 0;
    end;
  end;
begin
  //Set TExtstring
  TextString := Value;

  //Set Cursor Visible
  SelectBlink := True;
  STicks := GettickCount div 550;

  //Exit if there is no Need to Create Tiles
  If (W <= 0) and (Pos('\n', Value) = 0) then
  begin
    SetLength (TextTiles, 1);
    TextTiles[0] := Value;
    Exit;
  end;

  //Create Tiles
  //Reset Text Array
  SetLength (TextTiles, 0);
  Len := 0;

  //Reset Counter Vars
  LastPos := 1;
  NextPos := 1;
  LastBreak := 1;
  FirstWord := 1;


  if (W > 0) then
  begin
    //Set Font Propertys
    SetFontStyle(Style);
    SetFontSize(Size);
  end;

  //go Through Text
  While (GetNextPos) do
  begin
      //Break in Text
      if isBreak then
      begin
        //Look for Break before the Break
        if (glTextWidth(PChar(Copy(Value, LastBreak, NextPos - LastBreak + 1))) > W) AND (NextPos-LastPos > 1) then
        begin
          isBreak := False;
          //Not the First word after Break, so we don't have to break within a word
          if (FirstWord > 1) then
          begin
            //Add Break before actual Position, because there the  Text fits the Area
            AddBreak(LastBreak, LastPos);
          end
          else //First Word after Break Break within the Word
          begin
            //ToDo
            //AddBreak(LastBreak, LastBreak + 155);
          end;
        end;

        isBreak := True;
        //Add Break from Text
        AddBreak(LastBreak, NextPos);
      end
      //Text comes out of the Text Area -> CreateBreak
      else if (glTextWidth(PChar(Copy(Value, LastBreak, NextPos - LastBreak + 1))) > W) then
      begin
        //Not the First word after Break, so we don't have to break within a word
        if (FirstWord > 1) then
        begin
          //Add Break before actual Position, because there the  Text fits the Area
          AddBreak(LastBreak, LastPos);
        end
        else //First Word after Break -> Break within the Word
        begin
          //ToDo
          //AddBreak(LastBreak, LastBreak + 155);
        end;
      end;
    //end;
    Inc(FirstWord)
  end;
  //Add Ending
  AddBreak(LastBreak, Length(Value)+1);
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
    glColor4f(ColR*Int, ColG*Int, ColB*Int, Alpha);

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

    {if (False) then //No Width set Draw as one Long String
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
    begin}
    //Now Use allways:
    //Draw Text as Many Strings
      Y2 := Y + MoveY;
      for I := 0 to high(TextTiles) do
      begin
        if (not (SelectBool AND SelectBlink)) OR (I <> high(TextTiles)) then
          Text2 := TextTiles[I]
        else
          Text2 := TextTiles[I] + '|';

        case Align of
          0: X2 := X + MoveX;
          1: X2 := X + MoveX - glTextWidth(pchar(Text2))/2;
          2: X2 := X + MoveX - glTextWidth(pchar(Text2));
        end;

        SetFontPos(X2, Y2);
        glPrint(PChar(Text2));

        {if Size >= 10 then
          Y2 := Y2 + Size * 2.8
        else}
        if (Style = 1) then
          Y2 := Y2 + Size * 2.8
        else
          Y2 := Y2 + Size * 2.15;
      end;
      SetFontStyle(0); // reset to default

    //end;
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
  Alpha := 1;
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
