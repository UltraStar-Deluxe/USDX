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

unit TextGL;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

// as long as the transition to freetype is not finished
// use the old implementation
{$IFDEF UseFreetype}
  {$INCLUDE TextGLFreetype.pas}
{$ELSE}
uses
  gl,
  SDL,
  UTexture,
  ULog;

procedure BuildFont;                          // build our bitmap font
procedure KillFont;                           // delete the font
function  glTextWidth(const text: string): real; // returns text width
procedure glPrint(const text: string);        // custom GL "Print" routine
procedure ResetFont();                        // reset font settings of active font
procedure SetFontPos(X, Y: real);             // sets X and Y
procedure SetFontZ(Z: real);                  // sets Z
procedure SetFontSize(Size: real);
procedure SetFontStyle(Style: integer);       // sets active font style (normal, bold, etc)
procedure SetFontItalic(Enable: boolean);     // sets italic type letter (works for all fonts)
procedure SetFontAspectW(Aspect: real);
procedure SetFontReflection(Enable:boolean;Spacing: real); // enables/disables text reflection

//function NextPowerOfTwo(Value: integer): integer;
// Checks if the ttf exists, if yes then a SDL_ttf is returned
//function LoadFont(FileName: PAnsiChar; PointSize: integer):PTTF_Font;
// Does the renderstuff, color is in $ffeecc style
//function RenderText(font: PTTF_Font; Text:PAnsiChar; Color: Cardinal):PSDL_Surface;

type
  TTextGL = record
    X:        real;
    Y:        real;
    Z:        real;
    Text:     string;
    Size:     real;
    ColR:     real;
    ColG:     real;
    ColB:     real;
  end;

  PFont = ^TFont;
  TFont = record
    Tex:      TTexture;
    Width:    array[0..255] of byte;
    AspectW:  real;
    Centered: boolean;
    Outline:  real;
    Italic:   boolean;
    Reflection: boolean;
    ReflectionSpacing: real;
  end;


var
  Fonts:      array of TFont;
  ActFont:    integer;


implementation

uses
  UMain,
  UCommon,
  SysUtils,
  IniFiles,
  Classes,
  UGraphic;

var
  // Colours for the reflection
  TempColor:    array[0..3] of GLfloat;

{**
 * Load font info.
 * FontFile is the name of the image (.png) not the data (.dat) file
 *}
procedure LoadFontInfo(FontID: integer; const FontFile: string);
var
  Stream:  TFileStream;
  DatFile: string;
begin
  DatFile := ChangeFileExt(FontFile, '.dat');
  FillChar(Fonts[FontID].Width[0], Length(Fonts[FontID].Width), 0);

  Stream := nil;
  try
    Stream := TFileStream.Create(DatFile, fmOpenRead);
    Stream.Read(Fonts[FontID].Width, 256);
  except
    Log.LogError('Error while reading font['+ inttostr(FontID) +']', 'LoadFontInfo');
  end;
  Stream.Free;
end;

// Builds bitmap fonts
procedure BuildFont;
var
  Count: integer;
  FontIni: TMemIniFile;
  FontFile: string;     // filename of the image (with .png/... ending)
begin
  ActFont := 0;

  SetLength(Fonts, 4);
  FontIni := TMemIniFile.Create(FontPath + 'fonts.ini');

  // Normal

  FontFile := FontPath + FontIni.ReadString('Normal', 'File', '');

  Fonts[0].Tex := Texture.LoadTexture(true, FontFile, TEXTURE_TYPE_TRANSPARENT, 0);
  Fonts[0].Tex.H := 30;
  Fonts[0].AspectW := 0.9;
  Fonts[0].Outline := 0;

  LoadFontInfo(0, FontFile);

  // Bold

  FontFile := FontPath + FontIni.ReadString('Bold', 'File', '');

  Fonts[1].Tex := Texture.LoadTexture(true, FontFile, TEXTURE_TYPE_TRANSPARENT, 0);
  Fonts[1].Tex.H := 30;
  Fonts[1].AspectW := 1;
  Fonts[1].Outline := 0;

  LoadFontInfo(1, FontFile);
  for Count := 0 to 255 do
    Fonts[1].Width[Count] := Fonts[1].Width[Count] div 2;

  // Outline1

  FontFile := FontPath + FontIni.ReadString('Outline1', 'File', '');

  Fonts[2].Tex := Texture.LoadTexture(true, FontFile, TEXTURE_TYPE_TRANSPARENT, 0);
  Fonts[2].Tex.H := 30;
  Fonts[2].AspectW := 0.95;
  Fonts[2].Outline := 5;

  LoadFontInfo(2, FontFile);
  for Count := 0 to 255 do
    Fonts[2].Width[Count] := Fonts[2].Width[Count] div 2 + 2;

  // Outline2

  FontFile := FontPath + FontIni.ReadString('Outline2', 'File', '');

  Fonts[3].Tex := Texture.LoadTexture(true, FontFile, TEXTURE_TYPE_TRANSPARENT, 0);
  Fonts[3].Tex.H := 30;
  Fonts[3].AspectW := 0.95;
  Fonts[3].Outline := 4;

  LoadFontInfo(3, FontFile);
  for Count := 0 to 255 do
    Fonts[3].Width[Count] := Fonts[3].Width[Count] + 1;


  // close ini-file
  FontIni.Free;
end;

// Deletes the font
procedure KillFont;
begin
  // delete all characters
  //glDeleteLists(..., 256);
end;

function glTextWidth(const text: string): real;
var
  Letter: char;
  i: integer;
  Font: PFont;
begin
  Result := 0;
  Font := @Fonts[ActFont];

  for i := 0 to Length(text) -1 do
  begin
    Letter := Text[i];
    Result := Result + Font.Width[Ord(Letter)] * Font.Tex.H / 30 * Font.AspectW;
  end;

  if ((Result > 0) and Font.Italic) then
    Result := Result + 12 * Font.Tex.H / 60 * Font.AspectW;
end;

procedure glPrintLetter(Letter: char);
var
  TexX, TexY:        real;
  TexR, TexB:        real;
  TexHeight:         real;
  FWidth:            real;
  PL, PT:            real;
  PR, PB:            real;
  XItal:             real; // X shift for italic type letter
  ReflectionSpacing: real; // Distance of the reflection
  Font:              PFont;
  Tex:               PTexture;
begin
  Font := @Fonts[ActFont];
  Tex := @Font.Tex;

  FWidth := Font.Width[Ord(Letter)];

  Tex.W := FWidth * (Tex.H/30) * Font.AspectW;

  // set texture positions
  TexX := (ord(Letter) mod 16) * 1/16 + 1/32 - FWidth/1024 - Font.Outline/1024;
  TexY := (ord(Letter) div 16) * 1/16 + 2/1024;
  TexR := (ord(Letter) mod 16) * 1/16 + 1/32 + FWidth/1024 + Font.Outline/1024;
  TexB := (1 + ord(Letter) div 16) * 1/16 - 2/1024;

  TexHeight := TexB - TexY;

  // set vector positions
  PL := Tex.X - Font.Outline * (Tex.H/30) * Font.AspectW /2;
  PT := Tex.Y;
  PR := PL + Tex.W + Font.Outline * (Tex.H/30) * Font.AspectW;
  PB := PT + Tex.H;

  if (not Font.Italic) then
    XItal := 0
  else
    XItal := 12;

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, Tex.TexNum);
  
  glBegin(GL_QUADS);
    glTexCoord2f(TexX, TexY); glVertex2f(PL+XItal,  PT);
    glTexCoord2f(TexX, TexB); glVertex2f(PL,        PB);
    glTexCoord2f(TexR, TexB); glVertex2f(PR,        PB);
    glTexCoord2f(TexR, TexY); glVertex2f(PR+XItal,  PT);
  glEnd;

  // <mog> Reflection
  // Yes it would make sense to put this in an extra procedure,
  // but this works, doesn't take much lines, and is almost lightweight
  if Font.Reflection then
  begin
    ReflectionSpacing := Font.ReflectionSpacing + Tex.H/2;

    glDepthRange(0, 10);
    glDepthFunc(GL_LEQUAL);
    glEnable(GL_DEPTH_TEST);

    glBegin(GL_QUADS);
      glColor4f(TempColor[0], TempColor[1], TempColor[2], 0);
      glTexCoord2f(TexX, TexY + TexHeight/2);
      glVertex3f(PL, PB + ReflectionSpacing - Tex.H/2, Tex.z);

      glColor4f(TempColor[0], TempColor[1], TempColor[2], Tex.Alpha-0.3);
      glTexCoord2f(TexX, TexB );
      glVertex3f(PL + XItal, PT + ReflectionSpacing, Tex.z);

      glTexCoord2f(TexR, TexB );
      glVertex3f(PR + XItal, PT + ReflectionSpacing, Tex.z);

      glColor4f(TempColor[0], TempColor[1], TempColor[2], 0);
      glTexCoord2f(TexR, TexY + TexHeight/2);
      glVertex3f(PR, PB + ReflectionSpacing - Tex.H/2, Tex.z);
    glEnd;

    glDisable(GL_DEPTH_TEST);
  end; // reflection

  glDisable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);

  Tex.X := Tex.X + Tex.W;

  //write the colour back
  glColor4fv(@TempColor);
end;

// Custom GL "Print" Routine
procedure glPrint(const Text: string);
var
  Pos: integer;
begin
  // if there is no text do nothing
  if (Text = '') then
    Exit;

  //Save the actual color and alpha (for reflection)
  glGetFloatv(GL_CURRENT_COLOR, @TempColor);

  for Pos := 1 to Length(Text) do
  begin
    glPrintLetter(Text[Pos]);
  end;
end;

procedure ResetFont();
begin
  SetFontPos(0, 0);
  SetFontZ(0);
  SetFontItalic(False);
  SetFontReflection(False, 0);
end;

procedure SetFontPos(X, Y: real);
begin
  Fonts[ActFont].Tex.X := X;
  Fonts[ActFont].Tex.Y := Y;
end;

procedure SetFontZ(Z: real);
begin
  Fonts[ActFont].Tex.Z := Z;
end;

procedure SetFontSize(Size: real);
begin
  Fonts[ActFont].Tex.H := Size;
end;

procedure SetFontStyle(Style: integer);
begin
  ActFont := Style;
end;

procedure SetFontItalic(Enable: boolean);
begin
  Fonts[ActFont].Italic := Enable;
end;

procedure SetFontAspectW(Aspect: real);
begin
  Fonts[ActFont].AspectW := Aspect;
end;

procedure SetFontReflection(Enable: boolean; Spacing: real);
begin
  Fonts[ActFont].Reflection        := Enable;
  Fonts[ActFont].ReflectionSpacing := Spacing;
end;

end.

{$ENDIF}

