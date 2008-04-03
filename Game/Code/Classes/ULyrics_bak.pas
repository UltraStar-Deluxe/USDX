unit ULyrics_bak;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses SysUtils,
     OpenGL12,
     UMusic,
     UTexture;

type
  TWord = record
      X:      real;
      Y:      real;
      Size:   real;
      Width:  real;
      Text:   string;
      ColR:   real;
      ColG:   real;
      ColB:   real;
      Scale:  real;
      Done:   real;
      FontStyle:  integer;
      Italic:     boolean;
      Selected:   boolean;
  end;

  TLyric = class
    private
      AlignI:         integer;
      XR:             real;
      YR:             real;
      SizeR:          real;
      SelectedI:      integer;
      ScaleR:         real;
      StyleI:         integer;          // 0 - one selection, 1 - long selection, 2 - one selection with fade to normal text, 3 - long selection with fade with color from left
      FontStyleI:     integer;          // font number
      Word:           array of TWord;

      //Textures for PlayerIcon Index: Playernum; Index2: Enabled/Disabled
      PlayerIconTex:  array[0..5] of array [0..1] of TTexture;

      procedure SetX(Value: real);
      procedure SetY(Value: real);
      function GetClientX: real;
      procedure SetAlign(Value: integer);
      function GetSize: real;
      procedure SetSize(Value: real);
      procedure SetSelected(Value: integer);
      procedure SetDone(Value: real);
      procedure SetScale(Value: real);
      procedure SetStyle(Value: integer);
      procedure SetFStyle(Value: integer);
      procedure Refresh;

      procedure DrawNormal(W: integer);
      procedure DrawPlain(W: integer);
      procedure DrawScaled(W: integer);
      procedure DrawSlide(W: integer);

      procedure DrawPlayerIcons;
    public
      //Array containing Players Singing the Next Sentence
      // 1: Player 1 Active
      // 2: Player 2 Active
      // 3: Player 3 Active
      PlayersActive: Byte;

      //Dark or Light Colors
      Enabled: Boolean;

      ColR:     real;
      ColG:     real;
      ColB:     real;
      ColSR:    real;
      ColSG:    real;
      ColSB:    real;
      Italic:   boolean;
      Text:     string;   // LCD

      constructor Create;
      
      procedure AddWord(Text: string);
      procedure AddCzesc(NrCzesci: integer);

      function SelectedLetter: integer;  // LCD
      function SelectedLength: integer;  // LCD

      procedure Clear;
      procedure Draw;
    published
      property X: real write SetX;
      property Y: real write SetY;
      property ClientX: real read GetClientX;
      property Align: integer write SetAlign;
      property Size: real read GetSize write SetSize;
      property Selected: integer read SelectedI write SetSelected;
      property Done: real write SetDone;
      property Scale: real write SetScale;
      property Style: integer write SetStyle;
      property FontStyle: integer write SetFStyle;
  end;

var
  Lyric:    TLyric;

implementation
uses TextGL, UGraphic, UDrawTexture, Math, USkins;

Constructor TLyric.Create;
var
  I: Integer;
begin
  //Only 2 Players for now
  For I := 0 to 1 do
  begin
    PlayerIconTex[I][0] := Texture.LoadTexture(Skin.GetTextureFileName('LyricIcon_P' + InttoStr(I+1)), TEXTURE_TYPE_TRANSPARENT, 0);
    PlayerIconTex[I][1] := Texture.LoadTexture(Skin.GetTextureFileName('LyricIconD_P' + InttoStr(I+1)), TEXTURE_TYPE_TRANSPARENT, 0);
  end;
  PlayersActive := Trunc(Power(2, 1)) + 1;
end;

procedure TLyric.SetX(Value: real);
begin
  XR := Value;
end;

procedure TLyric.SetY(Value: real);
begin
  YR := Value;
end;

function TLyric.GetClientX: real;
begin
  Result := Word[0].X;
end;

procedure TLyric.SetAlign(Value: integer);
begin
  AlignI := Value;
//  if AlignInt = 0 then Log.LogStatus('AlignInt = 0', 'TLyric.SetAlign');
end;

function TLyric.GetSize: real;
begin
  Result := SizeR;
end;

procedure TLyric.SetSize(Value: real);
begin
  SizeR := Value;
end;

procedure TLyric.SetSelected(Value: integer);
var
  W:  integer;
begin
  if (StyleI = 0) or (StyleI = 2) or (StyleI = 4) then begin
    if (SelectedI > -1) and (SelectedI <= High(Word)) then begin
      Word[SelectedI].Selected := false;
      Word[SelectedI].ColR := ColR;
      Word[SelectedI].ColG := ColG;
      Word[SelectedI].ColB := ColB;
      Word[SelectedI].Done := 0;
    end;

    SelectedI := Value;
    if (Value > -1) and (Value <= High(Word)) then begin
      Word[Value].Selected := true;
      Word[Value].ColR := ColSR;
      Word[Value].ColG := ColSG;
      Word[Value].ColB := ColSB;
      Word[Value].Scale := ScaleR;
    end;
  end;

  if (StyleI = 1) or (StyleI = 3) then begin
    if (SelectedI > -1) and (SelectedI <= High(Word)) then begin
      for W := SelectedI to High(Word) do begin
        Word[W].Selected := false;
        Word[W].ColR := ColR;
        Word[W].ColG := ColG;
        Word[W].ColB := ColB;
        Word[W].Done := 0;
      end;
    end;

    SelectedI := Value;
    if (Value > -1) and (Value <= High(Word)) then begin
      for W := 0 to Value do begin
        Word[W].Selected := true;
        Word[W].ColR := ColSR;
        Word[W].ColG := ColSG;
        Word[W].ColB := ColSB;
        Word[W].Scale := ScaleR;
        Word[W].Done := 1;
      end;
    end;
  end;

  Refresh;
end;

procedure TLyric.SetDone(Value: real);
var
  W:    integer;
begin
  W := SelectedI;
  if W > -1 then
    Word[W].Done := Value;
end;

procedure TLyric.SetScale(Value: real);
begin
  ScaleR := Value;
end;

procedure TLyric.SetStyle(Value: integer);
begin
  StyleI := Value;
end;

procedure TLyric.SetFStyle(Value: integer);
begin
  FontStyleI := Value;
end;

procedure TLyric.AddWord(Text: string);
var
  WordNum:    integer;
begin
  WordNum := Length(Word);
  SetLength(Word, WordNum + 1);
  if WordNum = 0 then begin
    Word[WordNum].X := XR;
  end else begin
    Word[WordNum].X := Word[WordNum - 1].X + Word[WordNum - 1].Width;
  end;

  Word[WordNum].Y := YR;
  Word[WordNum].Size := SizeR;
  Word[WordNum].FontStyle := FontStyleI; // new
  SetFontStyle(FontStyleI);
  SetFontSize(SizeR);
  Word[WordNum].Width := glTextWidth(pchar(Text));
  Word[WordNum].Text := Text;
  Word[WordNum].ColR := ColR;
  Word[WordNum].ColG := ColG;
  Word[WordNum].ColB := ColB;
  Word[WordNum].Scale := 1;
  Word[WordNum].Done := 0;
  Word[WordNum].Italic := Italic;

  Refresh;
end;

procedure TLyric.AddCzesc(NrCzesci: integer);
var
  N:    integer;
begin
  Clear;
  for N := 0 to Lines[0].Line[NrCzesci].HighNote do begin
    Italic := Lines[0].Line[NrCzesci].Note[N].NoteType = ntFreestyle;
    AddWord(Lines[0].Line[NrCzesci].Note[N].Text);
    Text := Text + Lines[0].Line[NrCzesci].Note[N].Text;
  end;
  Selected := -1;
end;

procedure TLyric.Clear;
begin
{  ColR := Skin_FontR;
  ColG := Skin_FontG;
  ColB := Skin_FontB;}
  SetLength(Word, 0);
  Text := '';
  SelectedI := -1;
end;

procedure TLyric.Refresh;
var
  W:          integer;
  TotWidth:   real;
begin
  if AlignI = 1 then begin
    TotWidth := 0;
    for W := 0 to High(Word) do
      TotWidth := TotWidth + Word[W].Width;

    Word[0].X := XR - TotWidth / 2;
    for W := 1 to High(Word) do
      Word[W].X := Word[W - 1].X + Word[W - 1].Width;
  end;
end;

procedure TLyric.DrawPlayerIcons;
begin

end;

procedure TLyric.Draw;
var
  W:    integer;
begin
  case StyleI of
    0:
      begin
        for W := 0 to High(Word) do
          DrawNormal(W);
      end;
    1:
      begin
        for W := 0 to High(Word) do
          DrawPlain(W);
      end;
    2: // zoom
      begin
        for W := 0 to High(Word) do
          if not Word[W].Selected then
            DrawNormal(W);

        for W := 0 to High(Word) do
          if Word[W].Selected then
            DrawScaled(W);
      end;
    3: // slide
      begin
        for W := 0 to High(Word) do begin
          if not Word[W].Selected then
            DrawNormal(W)
          else
            DrawSlide(W);
        end;
      end;
    4: // ball
      begin
        for W := 0 to High(Word) do
          DrawNormal(W);

        for W := 0 to High(Word) do
          if Word[W].Selected then begin
            Tex_Ball.X := (Word[W].X - 10) + Word[W].Done * Word[W].Width;
            Tex_Ball.Y := 480 - 10*sin(Word[W].Done * pi);
            Tex_Ball.W := 20;
            Tex_Ball.H := 20;
            DrawTexture(Tex_Ball);
          end;
      end;
  end; // case
end;

procedure TLyric.DrawNormal(W: integer);
begin
  SetFontStyle(Word[W].FontStyle);
  SetFontPos(Word[W].X+ 10*ScreenX, Word[W].Y);
  SetFontSize(Word[W].Size);
  SetFontItalic(Word[W].Italic);
  glColor3f(Word[W].ColR, Word[W].ColG, Word[W].ColB);
  glPrint(pchar(Word[W].Text));
end;

procedure TLyric.DrawPlain(W: integer);
var
  D:    real;
begin
  D := Word[W].Done; // przyrost

  SetFontStyle(Word[W].FontStyle);
  SetFontPos(Word[W].X, Word[W].Y);
  SetFontSize(Word[W].Size);
  SetFontItalic(Word[W].Italic);

  if D = 0 then
    glColor3f(ColR, ColG, ColB)
  else
    glColor3f(ColSR, ColSG, ColSB);

  glPrint(pchar(Word[W].Text));
end;

procedure TLyric.DrawScaled(W: integer);
var
  D:    real;
begin
  // previous plus dynamic scaling effect
  D := 1-Word[W].Done; // przyrost
  SetFontStyle(Word[W].FontStyle);
  SetFontPos(Word[W].X - D * Word[W].Width * (Word[W].Scale - 1) / 2 + (D+1)*10*ScreenX, Word[W].Y - D * 1.5 * Word[W].Size *(Word[W].Scale - 1));
  SetFontSize(Word[W].Size + D * (Word[W].Size * Word[W].Scale - Word[W].Size));
  SetFontItalic(Word[W].Italic);
  glColor3f(Word[W].ColR, Word[W].ColG, Word[W].ColB);
  glPrint(pchar(Word[W].Text))
end;

procedure TLyric.DrawSlide(W: integer);
var
  D:    real;
begin
  D := Word[W].Done; // przyrost
  SetFontStyle(Word[W].FontStyle);
  SetFontPos(Word[W].X, Word[W].Y);
  SetFontSize(Word[W].Size);
  SetFontItalic(Word[W].Italic);
  glColor3f(Word[W].ColR, Word[W].ColG, Word[W].ColB);
  glPrintDone(pchar(Word[W].Text), D, ColR, ColG, ColB);
end;

function TLyric.SelectedLetter;  // LCD
var
  W:    integer;
begin
  Result := 1;

  for W := 0 to SelectedI-1 do
    Result := Result + Length(Word[W].Text);
end;

function TLyric.SelectedLength: integer;  // LCD
begin
  Result := Length(Word[SelectedI].Text);
end;

end.
