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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/menu/UMenuSelectSlide.pas $
 * $Id: UMenuSelectSlide.pas 2293 2010-04-23 22:39:26Z tobigun $
 *}

unit UMenuSelectSlide;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  dglOpenGL,
  TextGL,
  UMenuText,
  UTexture,
  UMenuInteract,
  UPath;

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

      Scrollable: boolean;

      //Visibility
      Visible: boolean;

      // Size and position
      PosX, PosY, PosYInit, W, H, SkipX: real;

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
      procedure SetX(PosX: real);
      function GetX(): real;
      procedure SetY(PosY: real);
      function GetY(): real;
      procedure SetScrollOffset(ScrollOffset: real);
      function GetScrollOffset(): real;

      property X: real read GetX write SetX;
      property Y: real read GetY write SetY;
      property ScrollOffset: real read GetScrollOffset write SetScrollOffset;
      //property W: real read Texture.w write Texture.w;
      //property H: real read Texture.h write Texture.h;
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
      constructor Create(PosX, PosY, W, H, SkipX, SBGW, ColR, ColG, ColB, Int, DColR, DColG, DColB, DInt,
  TColR, TColG, TColB, TInt, TDColR, TDColG, TDColB, TDInt,
  SBGColR, SBGColG, SBGColB, SBGInt, SBGDColR, SBGDColG, SBGDColB, SBGDInt,
  STColR, STColG, STColB, STInt, STDColR, STDColG, STDColB, STDInt: real;
  const TexName: IPath; Typ: TTextureType; const SBGName: IPath; SBGTyp: TTextureType;
  const Caption: UTF8String; var Data: integer; Scrollable: boolean);

      //Automatically Generate Lines (Texts)
      procedure genLines;
      procedure genLinesPositionY(TextOption: TText);

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
  UGraphic,
  ULog,
  UMenu;

// ------------ Select
constructor TSelectSlide.Create(PosX, PosY, W, H, SkipX, SBGW, ColR, ColG, ColB, Int, DColR, DColG, DColB, DInt,
  TColR, TColG, TColB, TInt, TDColR, TDColG, TDColB, TDInt,
  SBGColR, SBGColG, SBGColB, SBGInt, SBGDColR, SBGDColG, SBGDColB, SBGDInt,
  STColR, STColG, STColB, STInt, STDColR, STDColG, STDColB, STDInt: real;
  const TexName: IPath; Typ: TTextureType; const SBGName: IPath; SBGTyp: TTextureType;
  const Caption: UTF8String; var Data: integer; Scrollable: boolean);
begin
  inherited Create;
  Text := TText.Create;
  SetLength(TextOpt, 1);
  TextOpt[0] := TText.Create;
  Visible := true;
  self.W := W;
  self.H := H;
  self.SkipX := SkipX;
  self.Scrollable := Scrollable;

  if (Typ = TEXTURE_TYPE_COLORIZED) then
  begin
    Colorized := true;
    Texture := UTexture.Texture.GetTexture(TexName, Typ, RGBFloatToInt(ColR, ColG, ColB));
    DeselectTexture := UTexture.Texture.GetTexture(TexName, Typ, RGBFloatToInt(DColR, DColG, DColB));
  end
  else
  begin
    Colorized := false;
    Texture := UTexture.Texture.GetTexture(TexName, Typ);

    self.ColR := ColR;
    self.ColG := ColG;
    self.ColB := ColB;

    self.DColR := DColR;
    self.DColG := DColG;
    self.DColB := DColB;
  end;
  Texture.W := W;
  Texture.H := H;

  self.Int := Int;
  self.DInt := DInt;

  //SelectsS[S].X := X;
  //SelectsS[S].Y := Y;
  //SelectsS[S].W := W;
  //SelectsS[S].H := H;

  if (SBGTyp = TEXTURE_TYPE_COLORIZED) then
  begin
    ColorizedSBG := true;
    TextureSBG := UTexture.Texture.GetTexture(SBGName, SBGTyp, RGBFloatToInt(SBGColR, SBGColG, SBGColB));
    DeselectTextureSBG := UTexture.Texture.GetTexture(SBGName, SBGTyp, RGBFloatToInt(SBGDColR, SBGDColG, SBGDColB));
  end
  else
  begin
    ColorizedSBG := false;
    TextureSBG := UTexture.Texture.GetTexture(SBGName, SBGTyp);

    self.SBGColR := SBGColR;
    self.SBGColG := SBGColG;
    self.SBGColB := SBGColB;

    self.SBGDColR := SBGDColR;
    self.SBGDColG := SBGDColG;
    self.SBGDColB := SBGDColB;
  end;


  self.SBGInt := SBGInt;
  self.SBGDInt := SBGDInt;

  self.Tex_SelectS_ArrowL   := UGraphic.Tex_SelectS_ArrowL;
  //self.Tex_SelectS_ArrowL.W := Tex_SelectS_ArrowL.W;
  //self.Tex_SelectS_ArrowL.H := Tex_SelectS_ArrowL.H;


  self.Tex_SelectS_ArrowR   := UGraphic.Tex_SelectS_ArrowR;
  //self.Tex_SelectS_ArrowR.W := Tex_SelectS_ArrowR.W;
  //self.Tex_SelectS_ArrowR.H := Tex_SelectS_ArrowR.H;


  self.SBGW := SBGW;
  self.TextureSBG.H := H;

  self.Text.Text := Caption;
  self.Text.Size := 30;
  self.Text.Visible := true;
  self.TColR := TColR;
  self.TColG := TColG;
  self.TColB := TColB;
  self.TInt := TInt;
  self.TDColR := TDColR;
  self.TDColG := TDColG;
  self.TDColB := TDColB;
  self.TDInt := TDInt;

  self.STColR := STColR;
  self.STColG := STColG;
  self.STColB := STColB;
  self.STInt := STInt;
  self.STDColR := STDColR;
  self.STDColG := STDColG;
  self.STDColB := STDColB;
  self.STDInt := STDInt;

  // new
  Texture.TexX1 := 0;
  Texture.TexY1 := 0;
  Texture.TexX2 := 1;
  Texture.TexY2 := 1;
  TextureSBG.TexX1 := 0;
  TextureSBG.TexY1 := 0;
  TextureSBG.TexX2 := 1;
  TextureSBG.TexY2 := 1;

  // Sets Data to copy the value of selectops to global value;
  PData := @Data;

  TextOpt[0].Visible := true;

  // Sets default value of selectopt from Data;
  SelectedOption := Data;

  // Disables default selection
  SetSelect(false);

  //ColorizedSBG := false;
  //ColR := 1;
  //ColG := 1;
  //ColB := 1;
  //Int := 1;

  //DInt := 1;

  //SBGColR := 1;
  //SBGColG := 1;
  //SBGColB := 1;
  //SBGInt := 1;
  //SBGDColR := 1;
  //SBGDColG := 1;
  //SBGDColB := 1;
  //SBGDInt := 1;

  X := PosX;
  Y := PosY;
  PosYInit := PosY;
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

      self.Tex_SelectS_ArrowL.alpha := ArrowAlphaNoOptionsLeft;
      if (Length(TextOptT) > 1) then
        self.Tex_SelectS_ArrowR.alpha := ArrowAlphaOptionsLeft
      else
        self.Tex_SelectS_ArrowR.alpha := ArrowAlphaNoOptionsLeft;

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

      self.Tex_SelectS_ArrowL.alpha := ArrowAlphaOptionsLeft;
      self.Tex_SelectS_ArrowR.alpha := ArrowAlphaNoOptionsLeft;

      for SO := High(TextOpt) downto Low(TextOpt) do
      begin
        TextOpt[SO].Text := AdjustOptionTextToFit(TextOptT[High(TextOptT) - (Lines - SO - 1)]);
      end;
      DoSelection(Lines - 1);
    end

    //in between first and last
    else
    begin
      self.Tex_SelectS_ArrowL.alpha := ArrowAlphaOptionsLeft;
      self.Tex_SelectS_ArrowR.alpha := ArrowAlphaOptionsLeft;

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

procedure TSelectSlide.SetX(PosX: real);
begin
  Texture.X := PosX;
  self.Tex_SelectS_ArrowL.X := PosX + Texture.W + SkipX;
  self.Tex_SelectS_ArrowR.X := PosX + Texture.W + SkipX + SBGW - self.Tex_SelectS_ArrowR.W;

  TextureSBG.X := PosX + Texture.W + SkipX;
  Text.X := PosX + 20;
  self.PosX := PosX;
end;

function TSelectSlide.GetX(): real;
begin
  Result := PosX;
end;

procedure TSelectSlide.SetY(PosY: real);
var
  I: integer;
begin
  Texture.Y := PosY;
  self.Tex_SelectS_ArrowL.Y := PosY + (TextureSBG.H - self.Tex_SelectS_ArrowL.H) / 2;
  self.Tex_SelectS_ArrowR.Y := PosY + (TextureSBG.H - self.Tex_SelectS_ArrowR.H) / 2;

  TextureSBG.Y := PosY;
  self.Text.Y := PosY + (TextureSBG.H / 2) - 15;
  for I := Low(TextOpt) to High(TextOpt) do
    genLinesPositionY(TextOpt[I]);
  self.PosY := PosY;
end;

function TSelectSlide.GetY(): real;
begin
  Result := PosY;
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
    if (showArrows) then
      MaxLen := TextureSBG.W - MinSideSpacing * 2
    else
      MaxLen := TextureSBG.W;

    SetFontFamily(0);
    SetFontStyle(ftRegular);
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
      DrawTexture(self.Tex_SelectS_ArrowL);
      DrawTexture(self.Tex_SelectS_ArrowR);
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
  SetFontFamily(0);
  SetFontStyle(ftRegular{Text.Style});
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
    genLinesPositionY(TextOpt[I]);
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

procedure TSelectSlide.genLinesPositionY(TextOption: TText);
begin
  TextOption.Y := TextureSBG.Y + (TextureSBG.H - Text.Size) / 2;
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
  AreaW := self.Tex_SelectS_ArrowL.W;

  if (Y >= TextureSBG.Y) and (Y <= TextureSBG.Y + TextureSBG.H) then
  begin
    if (X >= TextureSBG.X) and (X <= TextureSBG.X + AreaW) then
      Result := maLeft   // hit left area
    else if (X >= TextureSBG.X + TextureSBG.W - AreaW) and (X <= TextureSBG.X + TextureSBG.W) then
      Result := maRight; // hit right area
  end;
end;

function TSelectSlide.GetScrollOffset(): real;
begin
  Result := Y - PosYInit;
end;

procedure TSelectSlide.SetScrollOffset(ScrollOffset: real);
begin
  if (ScrollOffset >= 0.0) then
    Y := PosYInit + ScrollOffset;
end;

end.
