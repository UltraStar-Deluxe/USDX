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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/menu/UMenuText.pas $
 * $Id: UMenuText.pas 2293 2010-04-23 22:39:26Z tobigun $
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
  dglOpenGL,
  sdl2,
  TextGL,
  UMenuInteract,
  UTexture;

type
  TText = class
    private
      SelectBool:  boolean;
      TextString:  UTF8String;
      TextTiles:   array of UTF8String;

      STicks:      cardinal;
      SelectBlink: boolean;
    public
      X:       real;
      Y:       real;
      Z:       real;
      MoveX:   real;       // some modifier for x - position that don't affect the real Y
      MoveY:   real;       // some modifier for y - position that don't affect the real Y
      W:       real;       // text wider than W is wrapped
      H:       real;       // text higher than H is decreased in font size
      Size:    real;
      ColR:    real;
      ColG:    real;
      ColB:    real;

      Alpha:   real;
      Int:     real;
      Font:    integer;
      Style:   integer;
      Visible: boolean;
      Align:   integer; // 0 = left, 1 = center, 2 = right

      // reflection
      Reflection:        boolean;
      ReflectionSpacing: real;

      Writable: boolean;

      procedure SetSelect(Value: boolean);
      property Selected: boolean read SelectBool write SetSelect;

      procedure SetText(Value: UTF8String);
      property  Text: UTF8String read TextString write SetText;

      procedure DeleteLastLetter; //< Deletes the rightmost letter

      procedure Draw;
      constructor Create; overload;
      constructor Create(X, Y: real; const Text: UTF8String); overload;
      constructor Create(ParX, ParY, ParW, ParH: real; ParFont, ParStyle: integer; ParSize, ParColR, ParColG, ParColB: real; ParAlign: integer; const ParText: UTF8String; ParReflection: boolean; ParReflectionSpacing: real; ParZ: real; Writable: boolean); overload;

      function GetMouseOverArea: TMouseOverRect;
  end;

implementation

uses
  UGraphic,
  UDisplay,
  UUnicodeStringHelper,
  UUnicodeUtils,
  StrUtils;

procedure TText.SetSelect(Value: boolean);
begin
  SelectBool := Value;

  // set cursor visible
  SelectBlink := true;
  STicks := SDL_GetTicks() div 550;
end;

procedure TText.SetText(Value: UTF8String);
var
  NextPos:     cardinal;  // next pos of a space etc.
  LastPos:     cardinal;  // last pos "
  LastBreak:   cardinal;  // last break
  isBreak:     boolean;   // true if the break is not caused because the text is out of the area
  FirstWord:   word;      // is first word after break?
  Len:         word;      // length of the tiles array
  DecFontSize: boolean = true; // how to handle strings that are too long to fit a given height, either decrease font size or cut off with ellipsis

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
  isBreak := false;
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
    SetFontFamily(Font);
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
          // add break before actual position, because there the text fits the area
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

  // if a height is given, there are only two ugly solutions (but prettier than overlapping text, e.g. artist/title in song menu)
  // a) decrease font size
  // b) cut off the last lines
  // until the text fits within the given height
  if (H > 0) and (Len * Size > H) then
  begin
    if (DecFontSize) then
    begin
      Size := Size - 1; // round(Size * 0.95 * 10) / 10;
      SetText(Value);
    end
    else
    begin
      Len := trunc(H / Size);
      SetLength(TextTiles, Len);
      TextTiles[Len-1] := TextTiles[Len-1] + '...';
    end;
  end;
end;

procedure TText.DeleteLastLetter;
begin
  SetText(UTF8Copy(TextString, 1, LengthUTF8(TextString)-1));
end;

procedure TText.Draw;
var
  X2, Y2: real;
  tmpText2, Text2:  UTF8String;
  I:      integer;
  Ticks:  cardinal;
begin
  if Visible then
  begin
    SetFontFamily(Font);
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
      SetFontStyle(ftNormal); // reset to default
    end
    else
    begin}
    // now use always:
    // draw text as many strings
      Y2 := Y + MoveY;
      for I := 0 to High(TextTiles) do
      begin
        tmpText2 := TextTiles[I];

        if (not (SelectBool and SelectBlink)) or (I <> High(TextTiles)) then
        begin
          Text2 := TextTiles[I];
        end
        else
        begin
          if (Writable) then
            Text2 := TextTiles[I] + '|'
          else
            Text2 := TextTiles[I];
        end;

        case Align of
          1: X2 := X + MoveX - glTextWidth(tmpText2)/2; { centered }
          2: X2 := X + MoveX - glTextWidth(tmpText2); { right aligned }
          else X2 := X + MoveX; { left aligned (default) }
        end;

        SetFontPos(X2, Y2);

        SetFontZ(Z);

        glPrint(Text2);

        {if Size >= 10 then
          Y2 := Y2 + Size * 0.93
        else}
        if (Style = ftBold) then
          Y2 := Y2 + Size * 0.93
        else
          Y2 := Y2 + Size * 0.85;
      end;

      // reset to default
      SetFontFamily(0);
      SetFontStyle(ftRegular);

    //end;
  end;
end;

constructor TText.Create;
begin
  Create(0, 0, '');
end;

constructor TText.Create(X, Y: real; const Text: UTF8String);
begin
  Create(X, Y, 0, 0, 0, ftRegular, 30, 0, 0, 0, 0, Text, false, 0, 0, false);
end;

constructor TText.Create(ParX, ParY, ParW, ParH: real;
                         ParFont, ParStyle: integer;
                         ParSize, ParColR, ParColG, ParColB: real;
                         ParAlign: integer;
                         const ParText: UTF8String;
                         ParReflection: boolean;
                         ParReflectionSpacing: real;
                         ParZ: real;
                         Writable: boolean);
begin
  inherited Create;
  Alpha := 1;
  X := ParX;
  Y := ParY;
  W := ParW;
  H := ParH;
  Z := ParZ;
  Font := ParFont;
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
  Writable := Writable;
end;

function TText.GetMouseOverArea: TMouseOverRect;
var
  W1: real;
begin
  if not(Display.Cursor_HiddenByScreen) then
  begin
    if (Align = 0) then
    begin
      Result.X := X;
      Result.Y := Y;
      Result.W := glTextWidth(Text);
      Result.H := Size;
    end;

    if (Align = 1) then
    begin
      Result.X := X -glTextWidth(Text)/2;
      Result.Y := Y;
      Result.W := glTextWidth(Text);
      Result.H := Size;
    end;

    if (Align = 2) then
    begin
      if (W <> 0) then
        Result.X := X - W
      else
        Result.X := X - glTextWidth(Text);

      Result.Y := Y;

      if (W <> 0) then
        Result.W := W
      else
        Result.W := glTextWidth(Text);

      Result.H := Size;
    end;
  end;
end;

end.
