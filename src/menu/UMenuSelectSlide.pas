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
  UTexture,
  UMenuInteract;

type
  PSelectSlide = ^TSelectSlide;
  TSelectSlide = class
    private
      SelectBool:       boolean;

      function AdjustOptionTextToFit(OptText: UTF8String): UTF8String;
    public
      // objects
      Text:             TText; // Main text describing option
      TextOpt:          array of TText; // 3 texts in the position of possible options
      TextOptT:         array of UTF8String; // array of names for possible options

      Texture:          TTexture; // Select Texture
      TextureSBG:       TTexture; // Background Selections Texture

      Colorized:          boolean;
      DeSelectTexture:    TTexture; // texture for colorized hack
      ColorizedSBG:       boolean;
      DeSelectTextureSBG: TTexture; // texture for colorized hack Select BG

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

      function GetMouseOverArea: TMouseOverRect;
      function OnClick(X, Y: Real): TMouseClickAction;
  end;

const
  ArrowAlphaOptionsLeft = 1;
  ArrowAlphaNoOptionsLeft = 0;
  MinItemSpacing = 5;
  MinSideSpacing = 24;

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

  Colorized := false;
  ColorizedSBG := false;
  ColR := 1;
  ColG := 1;
  ColB := 1;
  Int := 1;
  DColR := 1;
  DColG := 1;
  DColB := 1;
  DInt := 1;

  SBGColR := 1;
  SBGColG := 1;
  SBGColB := 1;
  SBGInt := 1;
  SBGDColR := 1;
  SBGDColG := 1;
  SBGDColB := 1;
  SBGDInt := 1;
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
    if Colorized then
      DeSelectTexture.Int := DInt
    else
    begin
      Texture.ColR := DColR;
      Texture.ColG := DColG;
      Texture.ColB := DColB;
      Texture.Int := DInt;
    end;

    Text.ColR := TDColR;
    Text.ColG := TDColG;
    Text.ColB := TDColB;
    Text.Int := TDInt;

    if (ColorizedSBG) then
      DeselectTextureSBG.Int := SBGDInt
    else
    begin
      TextureSBG.ColR := SBGDColR;
      TextureSBG.ColG := SBGDColG;
      TextureSBG.ColB := SBGDColB;
      TextureSBG.Int := SBGDInt;
    end;
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

      Tex_SelectS_ArrowL.alpha := ArrowAlphaNoOptionsLeft;
      if (Length(TextOptT) > 1) then
        Tex_SelectS_ArrowR.alpha := ArrowAlphaOptionsLeft
      else
        Tex_SelectS_ArrowR.alpha := ArrowAlphaNoOptionsLeft;

      for SO := Low(TextOpt) to High(TextOpt) do
      begin
        TextOpt[SO].Text := AdjustOptionTextToFit(TextOptT[SO]);
      end;

      DoSelection(0);
    end

    //Last option selected
    else if (Value >= High(TextOptT)) then
    begin
      Value := High(TextOptT);

      Tex_SelectS_ArrowL.alpha := ArrowAlphaOptionsLeft;
      Tex_SelectS_ArrowR.alpha := ArrowAlphaNoOptionsLeft;

      for SO := High(TextOpt) downto Low(TextOpt) do
      begin
        TextOpt[SO].Text := AdjustOptionTextToFit(TextOptT[High(TextOptT) - (Lines - SO - 1)]);
      end;
      DoSelection(Lines - 1);
    end

    //in between first and last
    else
    begin
      Tex_SelectS_ArrowL.alpha := ArrowAlphaOptionsLeft;
      Tex_SelectS_ArrowR.alpha := ArrowAlphaOptionsLeft;

      HalfL := Ceil((Lines - 1) / 2);
      HalfR := Lines - 1 - HalfL;

      //Selected option is near to the left side
      if (Value <= HalfL) then
      begin
        //Change texts
      	for SO := Low(TextOpt) to High(TextOpt) do
      	begin
      	  TextOpt[SO].Text := AdjustOptionTextToFit(TextOptT[SO]);
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
	        TextOpt[SO].Text := AdjustOptionTextToFit(TextOptT[High(TextOptT) - (Lines - SO - 1)]);
      	end;

      	DoSelection (HalfL);
      end

      else
      begin
      	//Change Texts
       	for SO := Low(TextOpt) to High(TextOpt) do
      	begin
	        TextOpt[SO].Text := AdjustOptionTextToFit(TextOptT[Value - HalfL + SO]);
      	end;

        DoSelection(HalfL);
      end;
    end;
  end;
end;

{ cuts the text if it is too long to fit on the selectbg }
function TSelectSlide.AdjustOptionTextToFit(OptText: UTF8String): UTF8String;
  var
    MaxLen: real;
    Len: integer;
begin
  Result := OptText;
  
  if (TextureSBG.W > 0) then
  begin
    MaxLen := TextureSBG.W - MinSideSpacing * 2;

    SetFontStyle(ftNormal);
    SetFontSize(Text.Size);

    // we will remove min. 2 letters by default and replace them w/ points
    // if the whole text don't fit
    Len := Length(OptText) - 1;

    while (glTextWidth(Result) > MaxLen) and (Len > 0) do
    begin
      { ensure that we only cut at full letters }
      { this code may be a problem if there is a text that
        consists of many multi byte characters and only few
        one byte characters }
      repeat
        Dec(Len);
      until (byte(OptText[Len]) and 128) = 0;
      
      Result := copy(OptText, 1, Len) + '..';
    end;
  end;
end;

procedure TSelectSlide.Draw;
var
  SO: integer;
begin
  if Visible then
  begin
    if SelectBool or not Colorized then
    begin
      DrawTexture(Texture);
    end
    else
    begin
      DeselectTexture.X := Texture.X;
      DeselectTexture.Y := Texture.Y;
      DeselectTexture.W := Texture.W;
      DeselectTexture.H := Texture.H;
      DrawTexture(DeselectTexture);
    end;

    if SelectBool or not ColorizedSBG then
    begin
      DrawTexture(TextureSBG);
    end
    else
    begin
      DeselectTextureSBG.X := TextureSBG.X;
      DeselectTextureSBG.Y := TextureSBG.Y;
      DeselectTextureSBG.W := TextureSBG.W;
      DeselectTextureSBG.H := TextureSBG.H;
      DrawTexture(DeselectTextureSBG);
    end;

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
  SetFontStyle(ftNormal{Text.Style});
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
    Lines := floor((TextureSBG.W - MinSideSpacing * 2) / (maxlength + MinItemSpacing));
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
    
  SetLength (TextOpt, Lines);

  for I := Low(TextOpt) to High(TextOpt) do
  begin
    TextOpt[I] := TText.Create;
    TextOpt[I].Size := Text.Size;
    TextOpt[I].Visible := true;
    TextOpt[I].Style := 0;

    TextOpt[I].ColR := STDColR;
    TextOpt[I].ColG := STDColG;
    TextOpt[I].ColB := STDColB;
    TextOpt[I].Int := STDInt;

    // generate positions
    TextOpt[I].Y := TextureSBG.Y + (TextureSBG.H - Text.Size) / 2;

    // better look with 2 options and a single option
    if (Lines = 2) then
    begin
      TextOpt[I].X := TextureSBG.X + 20 + (TextureSBG.W -40 - glTextWidth(TextOptT[1])) * I;
      TextOpt[I].Align := 0;
    end
    else if (Lines = 1) then
    begin
      TextOpt[I].X := TextureSBG.X + (TextureSBG.W / 2);
      TextOpt[I].Align := 1; //center text
    end
    else
    begin
      TextOpt[I].X := TextureSBG.X + TextureSBG.W / 2 + (TextureSBG.W - MinSideSpacing*2) * (I / Lines - 0.5);
      TextOpt[I].Align := 0;
    end;
  end;
end;

function TSelectSlide.GetMouseOverArea: TMouseOverRect;
begin
  Result.X := Texture.X;
  Result.Y := Texture.Y;
  Result.W := (TextureSBG.X + TextureSBG.W) - Result.X;
  Result.H := Max(Texture.H, TextureSBG.H);
end;

function TSelectSlide.OnClick(X, Y: Real): TMouseClickAction;
  var
    AreaW: Real;
begin
  // default: press return on click 
  Result := maReturn;

  // use left sides to inc or dec selection by click
  AreaW := TextureSbg.W / 20;

  if (Y >= TextureSBG.Y) and (Y <= TextureSBG.Y + TextureSBG.H) then
  begin
    if (X >= TextureSBG.X) and (X <= TextureSBG.X + AreaW) then
      Result := maLeft   // hit left area
    else if (X >= TextureSBG.X + TextureSBG.W - AreaW) and (X <= TextureSBG.X + TextureSBG.W) then
      Result := maRight; // hit right area
  end;
end;

end.
