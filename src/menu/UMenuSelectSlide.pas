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
  gl,
  TextGL,
  UMenuText,
  UTexture;

type
  PSelectSlide = ^TSelectSlide;
  TSelectSlide = class
    private
      SelectBool:       boolean;
    public
      // objects
      Text:             TText; // Main text describing option
      TextOpt:          array of TText; // 3 texts in the position of possible options
      TextOptT:         array of UTF8String; // array of names for possible options

      Texture:          TTexture; // Select Texture
      TextureSBG:       TTexture; // Background Selections Texture
//      TextureS:         array of TTexture; // Selections Texture (not used)

      Tex_SelectS_ArrowL:    TTexture; // Texture for left arrow
      Tex_SelectS_ArrowR:    TTexture; // Texture for right arrow

      SelectOptInt:     integer;
      PData:            ^integer;

      //For automatically Setting LineCount
      Lines: byte;

      //Arrows on/off
      showArrows: boolean;      //default is false

      //whether to show one item or all that fit into the select
      oneItemOnly: boolean;      //default is false

      //Visibility
      Visible: boolean;

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
      property Selected: boolean read SelectBool write SetSelect;
      procedure SetSelectOpt(Value: integer);
      property SelectedOption: integer read SelectOptInt write SetSelectOpt;
      procedure Draw;
      constructor Create;

      //Automatically Generate Lines (Texts)
      procedure genLines;
  end;

implementation

uses
  math,
  SysUtils,
  UDrawTexture,
  ULog;

// ------------ Select
constructor TSelectSlide.Create;
begin
  inherited Create;
  Text := TText.Create;
  SetLength(TextOpt, 1);
  TextOpt[0] := TText.Create;
  Visible := true;
end;

procedure TSelectSlide.SetSelect(Value: boolean);
{var
  SO: integer;
  I:  integer;}
begin
  SelectBool := Value;
  if Value then
  begin
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
  end
  else
  begin
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
  end;
end;

procedure TSelectSlide.SetSelectOpt(Value: integer);
var
  SO:    integer;
  HalfL: integer;
  HalfR: integer;

  procedure DoSelection(Sel: cardinal);
  var
    I: integer;
  begin
    for I := Low(TextOpt) to High(TextOpt) do
    begin
      TextOpt[I].ColR := STDColR;
      TextOpt[I].ColG := STDColG;
      TextOpt[I].ColB := STDColB;
      TextOpt[I].Int := STDInt;
    end;

    if (integer(Sel) <= High(TextOpt)) then
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

  if (Length(TextOpt) > 0) and (Length(TextOptT) > 0) then
  begin

    //First option selected
    if (Value <= 0) then
    begin
      Value := 0;

      Tex_SelectS_ArrowL.alpha := 0;
      Tex_SelectS_ArrowR.alpha := 1;

      for SO := Low(TextOpt) to High(TextOpt) do
      begin
        TextOpt[SO].Text := TextOptT[SO];
      end;

      DoSelection(0);
    end

    //Last option selected
    else if (Value >= High(TextOptT)) then
    begin
      Value := High(TextOptT);

      Tex_SelectS_ArrowL.alpha := 1;
      Tex_SelectS_ArrowR.alpha := 0;

      for SO := High(TextOpt) downto Low(TextOpt) do
      begin
        TextOpt[SO].Text := TextOptT[High(TextOptT) - (Lines - SO - 1)];
      end;
      DoSelection(Lines - 1);
    end

    //in between first and last
    else
    begin
      Tex_SelectS_ArrowL.alpha := 1;
      Tex_SelectS_ArrowR.alpha := 1;

      HalfL := Ceil((Lines - 1) / 2);
      HalfR := Lines - 1 - HalfL;

      //Selected option is near to the left side
      if (Value <= HalfL) then
      begin
        //Change texts
      	for SO := Low(TextOpt) to High(TextOpt) do
      	begin
      	  TextOpt[SO].Text := TextOptT[SO];
      	end;

      	DoSelection(Value);
      end

      //Selected option is near to the right side
      else if (Value > High(TextOptT) - HalfR) then
      begin
      	HalfR := High(TextOptT) - Value;
      	HalfL := Lines - 1 - HalfR;
      	//Change texts
       	for SO := High(TextOpt) downto Low(TextOpt) do
       	begin
	  TextOpt[SO].Text := TextOptT[High(TextOptT) - (Lines - SO - 1)];
      	end;

    	DoSelection (HalfL);
      end

      else
      begin
      	//Change Texts
       	for SO := Low(TextOpt) to High(TextOpt) do
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
  SO: integer;
begin
  if Visible then
  begin
    DrawTexture(Texture);
    DrawTexture(TextureSBG);

    if showArrows then
    begin
      DrawTexture(Tex_SelectS_ArrowL);
      DrawTexture(Tex_SelectS_ArrowR);
    end;

    Text.Draw;

    for SO := Low(TextOpt) to High(TextOpt) do
      TextOpt[SO].Draw;
  end;
end;

procedure TSelectSlide.GenLines;
var
  maxlength: real;
  I:         integer;
begin
  SetFontStyle(0{Text.Style});
  SetFontSize(Text.Size);
  maxlength := 0;

  for I := Low(TextOptT) to High(TextOptT) do
  begin
    if (glTextWidth(TextOptT[I]) > maxlength) then
      maxlength := glTextWidth(TextOptT[I]);
  end;


  if (oneItemOnly = false) then
  begin
    //show all items
    Lines := floor((TextureSBG.W-40) / (maxlength+7));
    if (Lines > Length(TextOptT)) then
      Lines := Length(TextOptT);

    if (Lines <= 0) then
      Lines := 1;
  end
  else
  begin
    //show one item only
    Lines := 1;
  end;

  //Free old Space used by Texts
  for I := Low(TextOpt) to High(TextOpt) do
    TextOpt[I].Free;
    
  setLength (TextOpt, Lines);

  for I := Low(TextOpt) to High(TextOpt) do
  begin
    TextOpt[I] := TText.Create;
    TextOpt[I].Size := Text.Size;
    //TextOpt[I].Align := 1;
    TextOpt[I].Align := 0;
    TextOpt[I].Visible := true;

    TextOpt[I].ColR := STDColR;
    TextOpt[I].ColG := STDColG;
    TextOpt[I].ColB := STDColB;
    TextOpt[I].Int := STDInt;

    //Generate Positions
    //TextOpt[I].X := TextureSBG.X + 20 + (TextureSBG.W  / Lines) * (I + 0.5);
    if (I <> High(TextOpt)) or (High(TextOpt) = 0) or (Length(TextOptT) = Lines) then
      TextOpt[I].X := TextureSBG.X + 20 + (TextureSBG.W  / Lines) * I
    else
      TextOpt[I].X := TextureSBG.X + TextureSBG.W - maxlength;

    TextOpt[I].Y := TextureSBG.Y + (TextureSBG.H - Text.Size) / 2;

    //Better Look with 2 Options
    if (Lines = 2) and (Length(TextOptT) = 2) then
      TextOpt[I].X := TextureSBG.X + 20 + (TextureSBG.W -40 - glTextWidth(TextOptT[1])) * I;

    if (Lines = 1) then
    begin
      TextOpt[I].Align := 1; //center text
      TextOpt[I].X := TextureSBG.X + (TextureSBG.W / 2);
    end;
  end;
end;

end.
