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

unit UMenuText;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  math,
  SysUtils,
  gl, 
  SDL,
  TextGL,
  UTexture;

type
  TText = class
    private
      SelectBool:  boolean;
      TextString:  string;
      TextTiles:   array of string;

      STicks:      cardinal;
      SelectBlink: boolean;
    public
      X:      real;
      Y:      real;
      Z:      real;
      MoveX:  real;       // some modifier for x - position that don't affect the real Y
      MoveY:  real;       // some modifier for y - position that don't affect the real Y
      W:      real;       // text wider than W is broken
//      H:      real;
      Size:   real;
      ColR:   real;
      ColG:   real;
      ColB:   real;
      Alpha:  real;
      Int:    real;
      Style:   integer;
      Visible: boolean;
      Align:   integer; // 0 = left, 1 = center, 2 = right

      // reflection
      Reflection:        boolean;
      ReflectionSpacing: real;

      procedure SetSelect(Value: boolean);
      property Selected: boolean read SelectBool write SetSelect;

      procedure SetText(Value: string);
      property  Text: string read TextString write SetText;

      procedure DeleteLastL; // procedure to delete last letter

      procedure Draw;
      constructor Create; overload;
      constructor Create(X, Y: real; Text: string); overload;
      constructor Create(ParX, ParY, ParW: real; ParStyle: integer; ParSize, ParColR, ParColG, ParColB: real; ParAlign: integer; ParText: string; ParReflection: boolean; ParReflectionSpacing: real; ParZ: real); overload;
  end;

implementation

uses
  StrUtils,
  UGraphic;

procedure TText.SetSelect(Value: boolean);
begin
  SelectBool := Value;
  
  // set cursor visible
  SelectBlink := true;
  STicks := SDL_GetTicks() div 550;
end;

procedure TText.SetText(Value: string);
var
  NextPos:   cardinal;  // next pos of a space etc.
  LastPos:   cardinal;  // last pos "
  LastBreak: cardinal;  // last break
  isBreak:   boolean;   // true if the break is not caused because the text is out of the area
  FirstWord: word;      // is first word after break?
  Len:       word;      // length of the tiles array

  function GetNextPos: boolean;
  var
    T1, {T2,} T3: cardinal;
  begin
    LastPos := NextPos;

    // next space (if width is given)
    if (W > 0) then
      T1 := PosEx(' ', Value, LastPos + 1)
    else
      T1 := Length(Value);

    {// next -
    T2 := PosEx('-', Value, LastPos + 1);}

    // next break
    T3 := PosEx('\n', Value, LastPos + 1);

    if T1 = 0 then
      T1 := Length(Value);
    {if T2 = 0 then
      T2 := Length(Value); }
    if T3 = 0 then
      T3 := Length(Value);

    // get nearest pos
    NextPos := min(T1, T3{min(T2, T3)});

    if (LastPos = cardinal(Length(Value))) then
      NextPos := 0;

    isBreak := (NextPos = T3) and (NextPos <> cardinal(Length(Value)));
    Result := (NextPos <> 0);
  end;

  procedure AddBreak(const From, bTo: cardinal);
  begin
    if (isBreak) or (bTo - From >= 1) then
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
  // set TextString
  TextString := Value;

  // set cursor visible
  SelectBlink := true;
  STicks := SDL_GetTicks() div 550;

  // exit if there is no need to create tiles
  if (W <= 0) and (Pos('\n', Value) = 0) then
  begin
    SetLength (TextTiles, 1);
    TextTiles[0] := Value;
    Exit;
  end;

  // create tiles
  // reset text array
  SetLength (TextTiles, 0);
  Len := 0;

  // reset counter vars
  LastPos := 1;
  NextPos := 1;
  LastBreak := 1;
  FirstWord := 1;

  if (W > 0) then
  begin
    // set font properties
    SetFontStyle(Style);
    SetFontSize(Size);
  end;

  // go through text
  while (GetNextPos) do
  begin
      // break in text
      if isBreak then
      begin
        // look for break before the break
        if (glTextWidth(Copy(Value, LastBreak, NextPos - LastBreak + 1)) > W) AND (NextPos-LastPos > 1) then
        begin
          isBreak := false;
          // not the first word after break, so we don't have to break within a word
          if (FirstWord > 1) then
          begin
            // add break before actual position, because there the text fits the area
            AddBreak(LastBreak, LastPos);
          end
          else // first word after break break within the word
          begin
            // to do
            // AddBreak(LastBreak, LastBreak + 155);
          end;
        end;

        isBreak := true;
        // add break from text
        AddBreak(LastBreak, NextPos);
      end
      // text comes out of the text area -> createbreak
      else if (glTextWidth(Copy(Value, LastBreak, NextPos - LastBreak + 1)) > W) then
      begin
        // not the first word after break, so we don't have to break within a word
        if (FirstWord > 1) then
        begin
          // add break before actual position, because there the  text fits the area
          AddBreak(LastBreak, LastPos);
        end
        else // first word after break -> break within the word
        begin
          // to do
          // AddBreak(LastBreak, LastBreak + 155);
        end;
      end;
    //end;
    Inc(FirstWord)
  end;
  // add ending
  AddBreak(LastBreak, Length(Value)+1);
end;

procedure TText.DeleteLastL;
var
  S: string;
  L: integer;
begin
  S := TextString;
  L := Length(S);
  if (L > 0) then
    SetLength(S, L-1);

  SetText(S);
end;

procedure TText.Draw;
var
  X2, Y2: real;
  Text2:  string;
  I:      integer;
  Ticks:  cardinal;
begin
  if Visible then
  begin
    SetFontStyle(Style);
    SetFontSize(Size);
    SetFontItalic(false);

    glColor4f(ColR*Int, ColG*Int, ColB*Int, Alpha);

    // reflection
    if Reflection then
      SetFontReflection(true, ReflectionSpacing)
    else
      SetFontReflection(false,0);

    // if selected set blink...
    if SelectBool then
    begin
      Ticks := SDL_GetTicks() div 550;
      if Ticks <> STicks then
      begin // change visability
        STicks := Ticks;
        SelectBlink := Not SelectBlink;
      end;
    end;

    {if (false) then // no width set draw as one long string
    begin
      if not (SelectBool AND SelectBlink) then
        Text2 := Text
      else
        Text2 := Text + '|';

      case Align of
        0: X2 := X;
        1: X2 := X - glTextWidth(Text2)/2;
        2: X2 := X - glTextWidth(Text2);
      end;

      SetFontPos(X2, Y);
      glPrint(Text2);
      SetFontStyle(0); // reset to default
    end
    else
    begin}
    // now use always:
    // draw text as many strings
      Y2 := Y + MoveY;
      for I := 0 to High(TextTiles) do
      begin
        if (not (SelectBool and SelectBlink)) or (I <> High(TextTiles)) then
          Text2 := TextTiles[I]
        else
          Text2 := TextTiles[I] + '|';

        case Align of
          1: X2 := X + MoveX - glTextWidth(Text2)/2; { centered }
          2: X2 := X + MoveX - glTextWidth(Text2); { right aligned }
          else X2 := X + MoveX; { left aligned (default) }
        end;

        SetFontPos(X2, Y2);

        SetFontZ(Z);

        glPrint(Text2);

        {if Size >= 10 then
          Y2 := Y2 + Size * 0.93
        else}
        if (Style = 1) then
          Y2 := Y2 + Size * 0.93
        else
          Y2 := Y2 + Size * 0.72;
      end;
      SetFontStyle(0); // reset to default

    //end;
  end;
end;

constructor TText.Create;
begin
  Create(0, 0, '');
end;

constructor TText.Create(X, Y: real; Text: string);
begin
  Create(X, Y, 0, 0, 30, 0, 0, 0, 0, Text, false, 0, 0);
end;

constructor TText.Create(ParX, ParY, ParW: real;
                         ParStyle: integer;
			 ParSize, ParColR, ParColG, ParColB: real;
			 ParAlign: integer;
			 ParText: string;
			 ParReflection: boolean;
			 ParReflectionSpacing: real;
			 ParZ: real);
begin
  inherited Create;
  Alpha := 1;
  X := ParX;
  Y := ParY;
  W := ParW;
  Z := ParZ;
  Style := ParStyle;
  Size := ParSize;
  Text := ParText;
  ColR := ParColR;
  ColG := ParColG;
  ColB := ParColB;
  Int := 1;
  Align := ParAlign;
  SelectBool := false;
  Visible := true;
  Reflection := ParReflection;
  ReflectionSpacing := ParReflectionSpacing;
end;

end.
