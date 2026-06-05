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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/menu/UMenuButton.pas $
 * $Id: UMenuButton.pas 3123 2015-08-23 03:15:31Z basisbit $
 *}

unit UMenuButton;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UText,
  UMenuText,
  sdl2,
  UMenuInteract,
  URenderer,
  UMenuWidget;

type
  CButton = class of TButton;

  TTextOffset = record
    X, Y: single;
  end;

  TButton = class(IMenuWidget)
    protected
      SelectBool:   boolean;

      FadeProgress: single;
      FadeLastTick: cardinal;

      DeSelectW,
      DeSelectH,
      PosX,
      PosY:         single;


    public
      Text:                      array of TText;
      TextOffset:                array of TTextOffset;
      Texture:                   TTexture; // Button Screen position and size

      Colorized:                 boolean;
      DeSelectTexture:           TTexture; // texture for colorized hack

      FadeTex:                   TTexture; //Texture for beautiful fading
      FadeTexPos:                byte;     //Pos of the FadeTexture (0: Top, 1: Left, 2: Bottom, 3: Right)

      DeselectType:              integer;  // not used yet
      Visible:                   boolean;

      Reflection:                boolean;
      Reflectionspacing,
      DeSelectReflectionspacing: single;

      Fade,
      FadeText:                  boolean;

      Selectable:                boolean;

      //Number of the Parent Collection, 0 if in no Collection
      Parent:  byte;

      SelectColR,
      SelectColG,
      SelectColB,
      SelectInt,
      SelectTInt:   single;
      //Fade Mod
      SelectW:      single;
      SelectH:      single;

      DeselectColR,
      DeselectColG,
      DeselectColB,
      DeselectInt,
      DeselectTInt: single;

      function GetX(): single; override;
      procedure SetX(Value: single) override;
      function GetY(): single; override;
      procedure SetY(Value: single) override;
      function GetZ(): single;
      procedure SetZ(Value: single);
      function GetW(): single; override;
      function GetH(): single; override;
      procedure SetW(Value: single);
      procedure SetH(Value: single);

      procedure SetSelect(Value: boolean); virtual;
      property Z: single read GetZ write SetZ;
      property W: single read GetW write SetW;
      property H: single read GetH write SetH;
      property Selected: boolean read SelectBool write SetSelect;

      procedure   Draw; override;

      constructor Create(); overload;
      constructor Create(Textura: TTexture); overload;
      constructor Create(Textura, DSTexture: TTexture); overload;
      destructor  Destroy; override;

      function GetMouseOverArea: TMouseOverRect;
  end;

implementation

uses
  SysUtils,
  UDisplay;

function TButton.GetX(): single;
begin
  Result := PosX;
end;

procedure TButton.SetX(Value: single);
var
  I: integer;
begin
  PosX := Value;
  if (not Fade) then
  begin
    Texture.X := Value;
    DeSelectTexture.X := Value;
    for I:= Low(Text) to High(Text) do
      Text[I].X := PosX + TextOffset[I].X;
  end;

end;

function TButton.GetZ(): single;
begin
  Result := Texture.Z;
end;

procedure TButton.SetZ(Value: single);
begin
  Texture.Z := Value;
end;

function TButton.GetY(): single;
begin
  Result := PosY;
end;

procedure TButton.SetY(Value: single);
var
  I: integer;
begin
  PosY := Value;
  if (not Fade) then
  begin
    Texture.y := Value;
    DeSelectTexture.Y := Value;
    for I:= Low(Text) to High(Text) do
      Text[I].Y := PosY + TextOffset[I].Y;
  end;
end;

function TButton.GetW(): single;
begin
  Result := DeSelectW;
end;

procedure TButton.SetW(Value: single);
begin
  if SelectW = DeSelectW then
    SelectW := Value;

  DeSelectW := Value;

  if not Fade then
  begin
    if SelectBool then
      Texture.W := SelectW
    else
      Texture.W := DeSelectW;
  end;
end;

function TButton.GetH(): single;
begin
  Result := DeSelectH;
end;

procedure TButton.SetH(Value: single);
begin
  if SelectH = DeSelectH then
    SelectH := Value;

  DeSelectH := Value;

  if not Fade then
  begin
    if SelectBool then
      Texture.H := SelectH
    else
      Texture.H := DeSelectH;
  end;
end;

procedure TButton.SetSelect(Value : boolean);
var
  T: integer;
begin
  SelectBool := Value;

  if (Value) then
  begin
    Texture.ColR  := SelectColR;
    Texture.ColG  := SelectColG;
    Texture.ColB  := SelectColB;
    Texture.Int   := SelectInt;

    for T := 0 to High(Text) do
    begin
      Text[T].SetSelect(SelectBool);
      Text[T].Int := SelectTInt;
    end;

    //Fade Mod
    if Fade then
    begin
      if (FadeProgress <= 0) then
        FadeProgress := 0.125;
    end
    else
    begin
      Texture.W := SelectW;
      Texture.H := SelectH;
    end;
  end
  else
  begin
    Texture.ColR  := DeselectColR;
    Texture.ColG  := DeselectColG;
    Texture.ColB  := DeselectColB;
    Texture.Int   := DeselectInt;

    for T := 0 to High(Text) do
    begin
      Text[T].SetSelect(SelectBool);
      Text[T].Int := DeselectTInt;
    end;
    
    //Fade Mod
    if Fade then
    begin
      if (FadeProgress >= 1) then
        FadeProgress := 0.875;
    end
    else
    begin
      Texture.W := DeSelectW;
      Texture.H := DeSelectH;
    end;
  end;
end;

// ***** Public methods ****** //

procedure TButton.Draw;
var
  T:       integer;
  Tick:    cardinal;
  Spacing: single;
begin
  if Visible then
  begin
    //Fade Mod
    T:=0;
    if Fade then
    begin
      if (FadeProgress < 1) and (FadeProgress > 0) then
      begin
        Tick := SDL_GetTicks() div 16;
        if (Tick <> FadeLastTick) then
        begin
          FadeLastTick := Tick;

          if SelectBool then
            FadeProgress := FadeProgress + 0.125
          else
            FadeProgress := FadeProgress - 0.125;

          if (FadeText) then
          begin
            for T := 0 to high(Text) do
            begin
              Text[T].MoveX := (SelectW - DeSelectW) * FadeProgress;
              Text[T].MoveY := (SelectH - DeSelectH) * FadeProgress;
            end;
          end;
        end;
      end;

      //Method without Fade Texture
      if (FadeTex = nil) then
      begin
        Texture.W         := DeSelectW + (SelectW - DeSelectW) * FadeProgress;
        Texture.H         := DeSelectH + (SelectH - DeSelectH) * FadeProgress;
        DeSelectTexture.W := Texture.W;
        DeSelectTexture.H := Texture.H;
      end
      else //method with Fade Texture
      begin
        Texture.W         := DeSelectW;
        Texture.H         := DeSelectH;
        DeSelectTexture.W := Texture.W;
        DeSelectTexture.H := Texture.H;

        FadeTex.ColR      := Texture.ColR;
        FadeTex.ColG      := Texture.ColG;
        FadeTex.ColB      := Texture.ColB;
        FadeTex.Int       := Texture.Int;

        FadeTex.Z         := Texture.Z;

        FadeTex.Alpha     := Texture.Alpha;
        FadeTex.TexX1     := 0;
        FadeTex.TexX2     := 1;
        FadeTex.TexY1     := 0;
        FadeTex.TexY2     := 1;

        case FadeTexPos of
          0: //FadeTex on Top
            begin
              //Standard Texture
              Texture.X := PosX;
              Texture.Y := PosY + (SelectH - DeSelectH) * FadeProgress;
              DeSelectTexture.X := Texture.X;
              DeSelectTexture.Y := Texture.Y;
              //Fade Tex
              FadeTex.X := PosX;
              FadeTex.Y := PosY;
              FadeTex.W := Texture.W;
              FadeTex.H := (SelectH - DeSelectH) * FadeProgress;
              //Some Hack that Fixes a little Space between both Textures
              FadeTex.TexY2 := 0.9;
            end;
          1: //FadeTex on Left
            begin
              //Standard Texture
              Texture.X := PosX + (SelectW - DeSelectW) * FadeProgress;
              Texture.Y := PosY;
              DeSelectTexture.X := Texture.X;
              DeSelectTexture.Y := Texture.Y;
              //Fade Tex
              FadeTex.X := PosX;
              FadeTex.Y := PosY;
              FadeTex.H := Texture.H;
              FadeTex.W := (SelectW - DeSelectW) * FadeProgress;
              //Some Hack that Fixes a little Space between both Textures
              FadeTex.TexX2 := 0.9;
            end;
          2: //FadeTex on Bottom
            begin
              //Standard Texture
              Texture.X := PosX;
              Texture.Y := PosY;
              DeSelectTexture.X := Texture.X;
              DeSelectTexture.Y := Texture.Y;
              //Fade Tex
              FadeTex.X := PosX;
              FadeTex.Y := PosY  + (SelectH - DeSelectH) * FadeProgress;;
              FadeTex.W := Texture.W;
              FadeTex.H := (SelectH - DeSelectH) * FadeProgress;
              //Some Hack that Fixes a little Space between both Textures
              FadeTex.TexY1 := 0.1;
            end;
          3: //FadeTex on Right
            begin
              //Standard Texture
              Texture.X := PosX;
              Texture.Y := PosY;
              DeSelectTexture.X := Texture.X;
              DeSelectTexture.Y := Texture.Y;
              //Fade Tex
              FadeTex.X := PosX + (SelectW - DeSelectW) * FadeProgress;
              FadeTex.Y := PosY;
              FadeTex.H := Texture.H;
              FadeTex.W := (SelectW - DeSelectW) * FadeProgress;
              //Some Hack that Fixes a little Space between both Textures
              FadeTex.TexX1 := 0.1;
            end;
        end;
      end;
    end
    else if (FadeText) then
    begin
      Text[T].MoveX := (SelectW - DeSelectW);
      Text[T].MoveY := (SelectH - DeSelectH);
    end;

    if (Reflection) then // Draw Reflections
    begin
      if (FadeProgress <> 0) and (FadeProgress <> 1) then
        Spacing := DeSelectReflectionspacing - (DeSelectReflectionspacing - Reflectionspacing) * FadeProgress
      else if SelectBool then
        Spacing := Reflectionspacing
      else
        Spacing := DeSelectReflectionspacing;
    end;

    if SelectBool or (FadeProgress > 0) or not Colorized then
    begin
      Texture.Reflection := Reflection;
      Texture.ReflectionSpacing := Spacing;
      Renderer.DrawTexture(Texture);
    end
    else
    begin
      DeSelectTexture.X := Texture.X;
      DeSelectTexture.Y := Texture.Y;
      DeSelectTexture.H := Texture.H;
      DeSelectTexture.W := Texture.W;
      DeSelectTexture.Reflection := Reflection;
      DeSelectTexture.ReflectionSpacing := Spacing;
      Renderer.DrawTexture(DeSelectTexture);
    end;

    //Draw FadeTex
    if (FadeTex <> nil) then
      Renderer.DrawTexture(FadeTex);

    for T := 0 to High(Text) do
    begin
      Text[T].Draw;
    end;
  end;
end;

function TButton.GetMouseOverArea: TMouseOverRect;
begin
  if not(Display.Cursor_HiddenByScreen) then
  begin
    if (not Fade) then
    begin
      Result.X := Texture.X;
      Result.Y := Texture.Y;
      Result.W := Texture.W;
      Result.H := Texture.H;
    end
    else
    begin
      case FadeTexPos of
        0: begin // fade tex on top
          Result.X := Texture.X;
          Result.Y := FadeTex.Y;
          Result.W := Texture.W;
          Result.H := FadeTex.H + Texture.H;
        end;

        1: begin // fade tex on left side
          Result.X := FadeTex.X;
          Result.Y := Texture.Y;
          Result.W := FadeTex.W + Texture.W;
          Result.H := Texture.H;
        end;

        2: begin // fade tex on bottom
          Result.X := Texture.X;
          Result.Y := Texture.Y;
          Result.W := Texture.W;
          Result.H := FadeTex.H + Texture.H;
        end;

        3: begin // fade tex on right side
          Result.X := Texture.X;
          Result.Y := Texture.Y;
          Result.W := FadeTex.W + Texture.W;
          Result.H := Texture.H;
        end;
      end;
    end;
  end;
end;


destructor TButton.Destroy;
begin
  Texture.Free;
  DeSelectTexture.Free;
  FadeTex.Free;
  inherited;
end;

constructor TButton.Create();
begin
  inherited Create;
  // We initialize all to 0, nil or false
  Visible        := true;
  SelectBool     := false;
  DeselectType   := 0;
  Selectable     := true;
  Reflection     := false;
  Colorized      := false;

  SelectColR     := 1;
  SelectColG     := 1;
  SelectColB     := 1;
  SelectInt      := 1;
  SelectTInt     := 1;

  DeselectColR   := 1;
  DeselectColG   := 1;
  DeselectColB   := 1;
  DeselectInt    := 0.5;
  DeselectTInt   := 1;

  Fade           := false;
  FadeTex        := nil;
  FadeProgress   := 0;
  FadeText       := false;
  SelectW        := DeSelectW;
  SelectH        := DeSelectH;

  PosX           := 0;
  PosY           := 0;

  Parent         := 0;
end;

constructor TButton.Create(Textura: TTexture);
begin
  Create();
  Texture         := Textura;
  DeSelectTexture := Textura.Clone();
  Texture.ColR    := 0;
  Texture.ColG    := 0.5;
  Texture.ColB    := 0;
  Texture.Int     := 1;
  Colorized       := false;
end;

// Button has the texture-type "colorized"
// Two textures are generated, one with Col the other one with DCol
// Check UMenu.pas line 680 to see the call ( AddButton() )
constructor TButton.Create(Textura, DSTexture: TTexture);
begin
  Create();
  Texture         := Textura;
  DeSelectTexture := DSTexture;
  Texture.ColR    := 1;
  Texture.ColG    := 1;
  Texture.ColB    := 1;
  Texture.Int     := 1;
  Colorized       := true;
end;

end.
