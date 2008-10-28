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
  TextGL,
  UTexture,
  gl, 
  math,
  SysUtils,
  SDL;

type
  TText = class
    private
      SelectBool:   boolean;
      TextString:   string;
      TextTiles:    array of string;

      STicks:       Cardinal;
      SelectBlink:  boolean;
    public
      X:      real;
      Y:      real;
      Z:      real;
      MoveX:  real;       //Some Modifier for X - Position that don't affect the real Y
      MoveY:  real;       //Some Modifier for Y - Position that don't affect the real Y
      W:      real;       //text wider than W is broken
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

      //Reflection
      Reflection:           boolean;
      ReflectionSpacing:    real;

      procedure SetSelect(Value: boolean);
      property Selected: boolean read SelectBool write SetSelect;

      procedure SetText(Value: string);
      property  Text: string read TextString write SetText;

      procedure DeleteLastL; //Procedure to Delete Last Letter

      procedure Draw;
      constructor Create; overload;
      constructor Create(X, Y: real; Tekst: string); overload;
      constructor Create(ParX, ParY, ParW: real; ParStyle: integer; ParSize, ParColR, ParColG, ParColB: real; ParAlign: integer; ParTekst: string; ParReflection: boolean; ParReflectionSpacing: real; ParZ: real); overload;
  end;

implementation

uses UGraphic,
     StrUtils;

procedure TText.SetSelect(Value: boolean);
begin
  SelectBool := Value;
  
  //Set Cursor Visible
  SelectBlink := True;
  STicks := SDL_GetTicks() div 550;
end;

procedure TText.SetText(Value: string);
var
  NextPos:   Cardinal;  //NextPos of a Space etc.
  LastPos:   Cardinal;  //LastPos "
  LastBreak: Cardinal;  //Last Break
  isBreak:   boolean;   //True if the Break is not Caused because the Text is out of the area
  FirstWord: Word;      //Is First Word after Break?
  Len:       Word;      //Length of the Tiles Array

  function GetNextPos: boolean;
  var
    T1, {T2,} T3: Cardinal;
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
    NextPos := min(T1, T3{min(T2, T3)});

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
  STicks := SDL_GetTicks() div 550;

  //Exit if there is no Need to Create Tiles
  if (W <= 0) and (Pos('\n', Value) = 0) then
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
    //Set Font Properties
    SetFontStyle(Style);
    SetFontSize(Size);
  end;

  //go Through Text
  while (GetNextPos) do
  begin
      //Break in Text
      if isBreak then
      begin
        //Look for Break before the Break
        if (glTextWidth(Copy(Value, LastBreak, NextPos - LastBreak + 1)) > W) AND (NextPos-LastPos > 1) then
        begin
          isBreak := False;
          //Not the First word after Break, so we don't have to break within a word
          if (FirstWord > 1) then
          begin
            //Add Break before actual Position, because there the Text fits the Area
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
      else if (glTextWidth(Copy(Value, LastBreak, NextPos - LastBreak + 1)) > W) then
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
begin
  if Visible then
  begin
    SetFontStyle(Style);
    SetFontSize(Size);
    SetFontItalic(False);

    glColor4f(ColR*Int, ColG*Int, ColB*Int, Alpha);

    //Reflection
    if Reflection = true then
      SetFontReflection(true, ReflectionSpacing)
    else
      SetFontReflection(false,0);

    //if selected set blink...
    if SelectBool then
    begin
      I := SDL_GetTicks() div 550;
      if I <> STicks then
      begin //Change Visability
        STicks := I;
        SelectBlink := Not SelectBlink;
      end;
    end;

    {if (False) then //no width set draw as one long string
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
    //now use allways:
    //draw text as many strings
      Y2 := Y + MoveY;
      for I := 0 to high(TextTiles) do
      begin
        if (not (SelectBool and SelectBlink)) or (I <> high(TextTiles)) then
          Text2 := TextTiles[I]
        else
          Text2 := TextTiles[I] + '|';

        case Align of
          0: X2 := X + MoveX;
          1: X2 := X + MoveX - glTextWidth(Text2)/2;
          2: X2 := X + MoveX - glTextWidth(Text2);
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

constructor TText.Create(X, Y: real; Tekst: string);
begin
  Create(X, Y, 0, 0, 30, 0, 0, 0, 0, Tekst, false, 0, 0);
end;

constructor TText.Create(ParX, ParY, ParW: real; ParStyle: integer; ParSize, ParColR, ParColG, ParColB: real; ParAlign: integer; ParTekst: string; ParReflection: boolean; ParReflectionSpacing: real; ParZ:real);
begin
  inherited Create;
  Alpha := 1;
  X := ParX;
  Y := ParY;
  W := ParW;
  Z := ParZ;
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
  Reflection:= ParReflection;
  ReflectionSpacing:= ParReflectionSpacing;
end;

end.
