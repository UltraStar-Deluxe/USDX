{* UltraStar Deluxe - Karaoke Game
 *
 * UltraStar Deluxe is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *}

unit UMenuSelectSlide;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  TextGL,
  UTexture,
  gl,
  UMenuText;

type
  PSelectSlide = ^TSelectSlide;
  TSelectSlide = class
    private
      SelectBool:       boolean;
    public
      // objects
      Text:             TText; // Main text describing option
      TextOpt:          array of TText; // 3 texts in the position of possible options
      TextOptT:         array of string; // array of names for possible options

      Texture:          TTexture; // Select Texture
      TextureSBG:       TTexture; // Background Selections Texture
//      TextureS:         array of TTexture; // Selections Texture (not used)

//      TextureArrowL:    TTexture; // Texture for left arrow (not used yet)
//      TextureArrowR:    TTexture; // Texture for right arrow (not used yet)

      SelectOptInt:     integer;
      PData:            ^integer;

      //For automatically Setting LineCount
      Lines: Byte;

      //Visibility
      Visible: Boolean;

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

      property SBGW: real read TextureSBG.w write TextureSBG.w;

      // procedures
      procedure SetSelect(Value: boolean);
      property Selected: Boolean read SelectBool write SetSelect;
      procedure SetSelectOpt(Value: integer);
      property SelectedOption: integer read SelectOptInt write SetSelectOpt;
      procedure Draw;
      constructor Create;

      //Automatically Generate Lines (Texts)
      procedure genLines;
  end;

implementation
uses UDrawTexture, math, ULog, SysUtils;

// ------------ Select
constructor TSelectSlide.Create;
begin
  inherited Create;
  Text := TText.Create;
  SetLength(TextOpt, 1);
  TextOpt[0] := TText.Create;

  //Set Standard Width for Selections Background
  SBGW := 450;

  Visible := True;
  {SetLength(TextOpt, 3);
  TextOpt[0] := TText.Create;
  TextOpt[1] := TText.Create;
  TextOpt[2] := TText.Create;}
end;

procedure TSelectSlide.SetSelect(Value: boolean);
{var
  SO:     integer;
  I:      integer;}
begin
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

{    for I := 0 to High(TextOpt) do begin
      TextOpt[I].ColR := STColR;
      TextOpt[I].ColG := STColG;
      TextOpt[I].ColB := STColB;
      TextOpt[I].Int := STInt;
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

{    for I := 0 to High(TextOpt) do begin
      TextOpt[I].ColR := STDColR;
      TextOpt[I].ColG := STDColG;
      TextOpt[I].ColB := STDColB;
      TextOpt[I].Int := STDInt;
    end;}
  end;
end;

procedure TSelectSlide.SetSelectOpt(Value: integer);
var
  SO:     integer;
  HalfL:  integer;
  HalfR:  integer;

procedure DoSelection(Sel: Cardinal);
    var I: Integer;
  begin
    for I := low(TextOpt) to high(TextOpt) do
    begin
      TextOpt[I].ColR := STDColR;
      TextOpt[I].ColG := STDColG;
      TextOpt[I].ColB := STDColB;
      TextOpt[I].Int := STDInt;
    end;
    if (integer(Sel) <= high(TextOpt)) then
    begin
      TextOpt[Sel].ColR := STColR;
      TextOpt[Sel].ColG := STColG;
      TextOpt[Sel].ColB := STColB;
      TextOpt[Sel].Int := STInt;
    end;
  end;
begin
  SelectOptInt := Value;
  PData^ := Value;
//  SetSelect(true); // reset all colors

  if (Length(TextOpt)>0) AND (Length(TextOptT)>0) then
  begin

    if (Value <= 0) then
    begin //First Option Selected
      Value := 0;

      for SO := low (TextOpt) to high(TextOpt) do
      begin
          TextOpt[SO].Text := TextOptT[SO];
      end;

      DoSelection(0);
    end
    else if (Value >= high(TextOptT)) then
    begin //Last Option Selected
      Value := high(TextOptT);

      for SO := high(TextOpt) downto low (TextOpt) do
      begin
          TextOpt[SO].Text := TextOptT[high(TextOptT)-(Lines-SO-1)];
      end;
      DoSelection(Lines-1);
    end
    else
    begin
    HalfL := Ceil((Lines-1)/2);
    HalfR := Lines-1-HalfL;

    if (Value <= HalfL) then
    begin //Selected Option is near to the left side
      {HalfL := Value;
      HalfR := Lines-1-HalfL;}
      //Change Texts
      for SO := low (TextOpt) to high(TextOpt) do
      begin
        TextOpt[SO].Text := TextOptT[SO];
      end;

      DoSelection(Value);
    end
    else if (Value > High(TextOptT)-HalfR) then
    begin //Selected is too near to the right border
      HalfR := high(TextOptT) - Value;
      HalfL := Lines-1-HalfR;
      //Change Texts
      for SO := high(TextOpt) downto low (TextOpt) do
      begin
          TextOpt[SO].Text := TextOptT[high(TextOptT)-(Lines-SO-1)];
      end;

      DoSelection (HalfL);
    end
    else
    begin
      //Change Texts
      for SO := low (TextOpt) to high(TextOpt) do
      begin
        TextOpt[SO].Text := TextOptT[Value - HalfL + SO];
      end;

      DoSelection(HalfL);
    end;

    end;

  end;

end;

procedure TSelectSlide.Draw;
var
  SO:     integer;
begin
  if Visible then
  begin
    DrawTexture(Texture);
    DrawTexture(TextureSBG);

    Text.Draw;

    for SO := low(TextOpt) to high(TextOpt) do
      TextOpt[SO].Draw;
  end;
end;

procedure TSelectSlide.GenLines;
var
maxlength: Real;
I: Integer;
begin
  SetFontStyle(0{Text.Style});
  SetFontSize(Text.Size);
  maxlength := 0;

  for I := low(TextOptT) to high (TextOptT) do
  begin
    if (glTextWidth(TextOptT[I]) > maxlength) then
      maxlength := glTextWidth(TextOptT[I]);
  end;

  Lines := floor((TextureSBG.W-40) / (maxlength+7));
  if (Lines > Length(TextOptT)) then
    Lines := Length(TextOptT);

  if (Lines <= 0) then
    Lines := 1;

  //Free old Space used by Texts
  For I := low(TextOpt) to high(TextOpt) do
    TextOpt[I].Free;
    
  setLength (TextOpt, Lines);

    for I := low(TextOpt) to high(TextOpt) do
    begin
      TextOpt[I] := TText.Create;
      TextOpt[I].Size := Text.Size;
      //TextOpt[I].Align := 1;
      TextOpt[I].Align := 0;
      TextOpt[I].Visible := True;

      TextOpt[I].ColR := STDColR;
      TextOpt[I].ColG := STDColG;
      TextOpt[I].ColB := STDColB;
      TextOpt[I].Int := STDInt;

      //Generate Positions
      //TextOpt[I].X := TextureSBG.X + 20 + (TextureSBG.W  / Lines) * (I + 0.5);
      if (I <> High(TextOpt)) OR (High(TextOpt) = 0) OR (Length(TextOptT) = Lines) then
        TextOpt[I].X := TextureSBG.X + 20 + (TextureSBG.W  / Lines) * I
      else
        TextOpt[I].X := TextureSBG.X + TextureSBG.W - maxlength;

      TextOpt[I].Y := TextureSBG.Y + (TextureSBG.H - Text.Size) / 2;

      //Better Look with 2 Options
      if (Lines=2) AND (Length(TextOptT)= 2) then
        TextOpt[I].X := TextureSBG.X + 20 + (TextureSBG.W -40 - glTextWidth(TextOptT[1])) * I;
    end;
end;

end.
