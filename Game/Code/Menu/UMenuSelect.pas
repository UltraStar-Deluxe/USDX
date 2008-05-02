unit UMenuSelect;

interface

{$I switches.inc}

uses TextGL, UTexture, gl, UMenuText;

type
  PSelect = ^TSelect;
  TSelect = class
    private
      SelectBool:       boolean;
    public
      // objects
      Text:             TText; // Main Text
      TextOpt:          array of TText; // Options Text
      Texture:          TTexture; // Select Texture
      TextureSBG:       TTexture; // Background Selections Texture
      TextureS:         array of TTexture; // Selections Texture
      SelectOptInt:     integer;
      PData:            ^integer;

      // for selection and deselection
      // main static
      ColR:     real;
      ColG:     real;
      ColB:     real;
      Int:      real;
      DColR:    real;
      DColG:    real;
      DColB:    real;
      DInt:     real;

      // main text
      TColR:    real;
      TColG:    real;
      TColB:    real;
      TInt:     real;
      TDColR:   real;
      TDColG:   real;
      TDColB:   real;
      TDInt:    real;

      // selection background static
      SBGColR:    real;
      SBGColG:    real;
      SBGColB:    real;
      SBGInt:     real;
      SBGDColR:   real;
      SBGDColG:   real;
      SBGDColB:   real;
      SBGDInt:    real;

      // selection statics
      SColR:    real;
      SColG:    real;
      SColB:    real;
      SInt:     real;
      SDColR:   real;
      SDColG:   real;
      SDColB:   real;
      SDInt:    real;

      // selection text
      STColR:     real;
      STColG:     real;
      STColB:     real;
      STInt:      real;
      STDColR:    real;
      STDColG:    real;
      STDColB:    real;
      STDInt:     real;
      
      // position and size
      property X: real read Texture.x write Texture.x;
      property Y: real read Texture.y write Texture.y;
      property W: real read Texture.w write Texture.w;
      property H: real read Texture.h write Texture.h;
//      property X2: real read Texture2.x write Texture2.x;
//      property Y2: real read Texture2.y write Texture2.y;
//      property W2: real read Texture2.w write Texture2.w;
//      property H2: real read Texture2.h write Texture2.h;

      // procedures
      procedure SetSelect(Value: boolean);
      property Selected: Boolean read SelectBool write SetSelect;
      procedure SetSelectOpt(Value: integer);
      property SelectedOption: integer read SelectOptInt write SetSelectOpt;
      procedure Draw(ButtonAlpha: real);
      constructor Create;
  end;

implementation
uses UDrawTexture;

// ------------ Select
constructor TSelect.Create;
begin
  inherited Create;
  Text := TText.Create;
end;

procedure TSelect.SetSelect(Value: boolean);
{var
  SO:     integer;}
begin // default 1, 0.4
  SelectBool := Value;
  if Value then begin
    Texture.ColR := ColR;
    Texture.ColG := ColG;
    Texture.ColB := ColB;
    Texture.Int := Int;

    Text.ColR := TColR;
    Text.ColG := TColG;
    Text.ColB := TColB;
    Text.Int := TInt;

    TextureSBG.ColR := SBGColR;
    TextureSBG.ColG := SBGColG;
    TextureSBG.ColB := SBGColB;
    TextureSBG.Int := SBGInt;

{    for SO := 0 to High(TextOpt) do begin
      if SelectOptInt = SO then begin
        TextOpt[SO].ColR := STColR;
        TextOpt[SO].ColG := STColG;
        TextOpt[SO].ColB := STColB;
        TextOpt[SO].Int := STInt;
      end else begin
        TextOpt[SO].ColR := STDColR;
        TextOpt[SO].ColG := STDColG;
        TextOpt[SO].ColB := STDColB;
        TextOpt[SO].Int := STDInt;
      end;
    end;}

  end else begin
    Texture.ColR := DColR;
    Texture.ColG := DColG;
    Texture.ColB := DColB;
    Texture.Int := DInt;

    Text.ColR := TDColR;
    Text.ColG := TDColG;
    Text.ColB := TDColB;
    Text.Int := TDInt;

    TextureSBG.ColR := SBGDColR;
    TextureSBG.ColG := SBGDColG;
    TextureSBG.ColB := SBGDColB;
    TextureSBG.Int := SBGDInt;

{    for SO := 0 to High(TextOpt) do begin
      TextOpt[SO].ColR := STDColR;
      TextOpt[SO].ColG := STDColG;
      TextOpt[SO].ColB := STDColB;
      TextOpt[SO].Int := STDInt;
    end;}
  end;
end;

procedure TSelect.SetSelectOpt(Value: integer);
var
  SO:     integer;
begin
  SelectOptInt := Value;
  PData^ := Value;
//  SetSelect(true); // reset all colors

  for SO := 0 to High(TextOpt) do begin
    if SelectOptInt = SO then begin
      TextOpt[SO].ColR := STColR;
      TextOpt[SO].ColG := STColG;
      TextOpt[SO].ColB := STColB;
      TextOpt[SO].Int := STInt;
    end else begin
      TextOpt[SO].ColR := STDColR;
      TextOpt[SO].ColG := STDColG;
      TextOpt[SO].ColB := STDColB;
      TextOpt[SO].Int := STDInt;
    end;
  end;
end;

procedure TSelect.Draw(ButtonAlpha: real);
var
  SO:     integer;
begin
  DrawTexture(Texture);
  DrawTexture(TextureSBG);

  Text.Draw;

  for SO := 0 to High(TextOpt) do begin
    TextOpt[SO].Draw;
  end;
end;

end.
