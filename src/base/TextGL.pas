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

uses
  gl,
  glext,
  SDL,
  Classes,
  UTexture,
  UFont,
  UPath,
  ULog;

type
  PGLFont = ^TGLFont;
  TGLFont = record
    Font:     TScalableFont;
    Outlined: boolean;
    X, Y, Z:  real;
  end;

const
  ftNormal   = 0;
  ftBold     = 1;
  ftOutline1 = 2;
  ftOutline2 = 3;

var
  Fonts:   array of TGLFont;
  ActFont: integer;

procedure BuildFonts;                         // builds all fonts
procedure KillFonts;                          // deletes all font
function  glTextWidth(const text: UTF8String): real; // returns text width
procedure glPrint(const text: UTF8String);    // custom GL "Print" routine
procedure ResetFont();                        // reset font settings of active font
procedure SetFontPos(X, Y: real);             // sets X and Y
procedure SetFontZ(Z: real);                  // sets Z
procedure SetFontSize(Size: real);
procedure SetFontStyle(Style: integer);       // sets active font style (normal, bold, etc)
procedure SetFontItalic(Enable: boolean);     // sets italic type letter (works for all fonts)
procedure SetFontReflection(Enable:boolean;Spacing: real); // enables/disables text reflection

implementation

uses
  UTextEncoding,
  SysUtils,
  IniFiles,
  UCommon,
  UMain,
  UPathUtils;

{**
 * Returns either Filename if it is absolute or a path relative to FontPath.
 *}
function FindFontFile(const Filename: string): IPath;
begin
  Result := FontPath.Append(Filename);
  // if path does not exist, try as an absolute path
  if (not Result.IsFile) then
    Result := Path(Filename);
end;

procedure AddFontFallbacks(FontIni: TMemIniFile; Font: TFont);
var
  FallbackFont: IPath;
  IdentName: string;
  I: Integer;
begin
  // evaluate the ini-file's 'Fallbacks' section
  for I := 1 to 10 do
  begin
    IdentName := 'File' + IntToStr(I);
    FallbackFont := FindFontFile(FontIni.ReadString('Fallbacks', IdentName, ''));
    if (FallbackFont.Equals(PATH_NONE)) then
      Continue;
    try
      Font.AddFallback(FallbackFont);
    except
      on E: EFontError do
        Log.LogError('Setting font fallback ''' + FallbackFont.ToNative() + ''' failed: ' + E.Message);
    end;
  end;
end;

const
  FONT_NAMES: array [0..3] of string = (
    'Normal', 'Bold', 'Outline1', 'Outline2'
  );

procedure BuildFonts;
var
  I: integer;
  FontIni: TMemIniFile;
  FontFile: IPath;
  Outline: single;
  Embolden: single;
  OutlineFont: TFTScalableOutlineFont;
  SectionName: string;
begin
  ActFont := 0;

  SetLength(Fonts, Length(FONT_NAMES));

  FontIni := TMemIniFile.Create(FontPath.Append('fonts.ini').ToNative);

  try
    for I := 0 to High(FONT_NAMES) do
    begin
      SectionName := 'Font_'+FONT_NAMES[I];

      FontFile := FindFontFile(FontIni.ReadString(SectionName , 'File', ''));

      // create either outlined or normal font
      Outline := FontIni.ReadFloat(SectionName, 'Outline', 0.0);
      if (Outline > 0.0) then
      begin
        // outlined font
        OutlineFont := TFTScalableOutlineFont.Create(FontFile, 64, Outline);
        OutlineFont.SetOutlineColor(
          FontIni.ReadFloat(SectionName, 'OutlineColorR',  0.0),
          FontIni.ReadFloat(SectionName, 'OutlineColorG',  0.0),
          FontIni.ReadFloat(SectionName, 'OutlineColorB',  0.0),
          FontIni.ReadFloat(SectionName, 'OutlineColorA', -1.0)
        );
        Fonts[I].Font := OutlineFont;
        Fonts[I].Outlined := true;
      end
      else
      begin
        // normal font
        Embolden := FontIni.ReadFloat(SectionName, 'Embolden', 0.0);
        Fonts[I].Font := TFTScalableFont.Create(FontFile, 64, Embolden);
        Fonts[I].Outlined := false;
      end;

      Fonts[I].Font.GlyphSpacing := FontIni.ReadFloat(SectionName, 'GlyphSpacing', 0.0);
      Fonts[I].Font.Stretch := FontIni.ReadFloat(SectionName, 'Stretch', 1.0);

      AddFontFallbacks(FontIni, Fonts[I].Font);
    end;
  except
    on E: EFontError do
      Log.LogCritical(E.Message, 'BuildFont');
  end;

  // close ini-file
  FontIni.Free;
end;


// Deletes the font
procedure KillFonts;
var
  I: integer;
begin
  for I := 0 to High(Fonts) do
    Fonts[I].Font.Free;
end;

function glTextWidth(const text: UTF8String): real;
var
  Bounds: TBoundsDbl;
begin
  Bounds := Fonts[ActFont].Font.BBox(Text, true);
  Result := Bounds.Right - Bounds.Left;
end;

// Custom GL "Print" Routine
procedure glPrint(const Text: UTF8String);
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
    GLFont.Font.Print(Text);
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
