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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/base/TextGL.pas $
 * $Id: TextGL.pas 1483 2008-10-28 19:01:20Z tobigun $
 *}

(*
unit TextGL;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}
*)

uses
  gl,
  glext,
  SDL,
  UTexture,
  UFont,
  Classes,
  ULog;

type
  PGLFont = ^TGLFont;
  TGLFont = record
    Font:     TScalableFont;
    X, Y, Z:  real;
  end;

var
  Fonts:   array of TGLFont;
  ActFont: integer;

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
procedure SetFontReflection(Enable:boolean;Spacing: real); // enables/disables text reflection

implementation

uses
  UMain,
  UCommon,
  UTextEncoding,
  SysUtils,
  IniFiles;

function FindFontFile(FontIni: TCustomIniFile; Font: string): string;
var
  Filename: string;
begin
  Filename := FontIni.ReadString(Font, 'File', '');
  Result := FontPath + Filename;
  // if path does not exist, try as an absolute path
  if (not FileExists(Result)) then
    Result := Filename;
end;

procedure BuildFont;
var
  FontIni: TMemIniFile;
  //BitmapFont: TBitmapFont;
  FontFile: string;
begin
  ActFont := 0;

  SetLength(Fonts, 4);
  FontIni := TMemIniFile.Create(FontPath + 'fontsTTF.ini');
  //FontIni := TMemIniFile.Create(FontPath + 'fonts.ini');

  try

    // Normal
    FontFile := FindFontFile(FontIni, 'Normal');
    Fonts[0].Font := TFTScalableFont.Create(FontFile, 64);
    //Fonts[0].Font.GlyphSpacing := 1.4;
    //Fonts[0].Font.Aspect := 1.2;

    {
    BitmapFont := TBitmapFont.Create(FontFile, 0, 19, 35, -10);
    BitmapFont.CorrectWidths(2, 0);
    Fonts[0].Font := TScalableFont.Create(BitmapFont, false);
    }

    //Fonts[0].Font.Aspect := 0.9;

    // Bold
    FontFile := FindFontFile(FontIni, 'Bold');
    Fonts[1].Font := TFTScalableFont.Create(FontFile, 64);

    // Outline1
    FontFile := FindFontFile(FontIni, 'Outline1');
    Fonts[2].Font := TFTScalableOutlineFont.Create(FontFile, 64, 0.06);
    //TFTScalableOutlineFont(Fonts[2].Font).SetOutlineColor(0.3, 0.3, 0.3);

    // Outline2
    FontFile := FindFontFile(FontIni, 'Outline2');
    Fonts[3].Font := TFTScalableOutlineFont.Create(FontFile, 64, 0.08);

  except on E: Exception do
    Log.LogCritical(E.Message, 'BuildFont');
  end;

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
  Bounds: TBoundsDbl;
begin
  // FIXME: remove conversion
  Bounds := Fonts[ActFont].Font.BBox(RecodeString(Text, encCP1252), true);
  Result := Bounds.Right - Bounds.Left;
end;

// Custom GL "Print" Routine
procedure glPrint(const Text: string);
var
  GLFont: PGLFont;
begin
  // if there is no text do nothing
  if (Text = '') then
    Exit;

  GLFont := @Fonts[ActFont];

  glPushMatrix();
    // set font position
    glTranslatef(GLFont.X, GLFont.Y + GLFont.Font.Ascender, GLFont.Z);
    // draw string
    // FIXME: remove conversion
    GLFont.Font.Print(RecodeString(Text, encCP1252));
  glPopMatrix();
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
  Fonts[ActFont].X := X;
  Fonts[ActFont].Y := Y;
end;

procedure SetFontZ(Z: real);
begin
  Fonts[ActFont].Z := Z;
end;

procedure SetFontSize(Size: real);
begin
  Fonts[ActFont].Font.Height := Size;
end;

procedure SetFontStyle(Style: integer);
begin
  ActFont := Style;
end;

procedure SetFontItalic(Enable: boolean);
begin
  if (Enable) then
    Fonts[ActFont].Font.Style := Fonts[ActFont].Font.Style + [Italic]
  else
    Fonts[ActFont].Font.Style := Fonts[ActFont].Font.Style - [Italic]
end;

procedure SetFontReflection(Enable: boolean; Spacing: real);
begin
  if (Enable) then
    Fonts[ActFont].Font.Style := Fonts[ActFont].Font.Style + [Reflect]
  else
    Fonts[ActFont].Font.Style := Fonts[ActFont].Font.Style - [Reflect];
  Fonts[ActFont].Font.ReflectionSpacing := Spacing - Fonts[ActFont].Font.Descender;
end;

end.
