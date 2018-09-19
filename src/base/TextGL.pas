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
 * $Id: TextGL.pas 2675 2010-10-17 17:00:23Z tobigun $
 *}

unit TextGL;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  dglOpenGL,
  sdl2,
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
  TFont = record
    FontFamily: integer;
    FontStyle:  integer;
  end;

const
  ftRegular     = 0;
  ftBold        = 1;
  ftOutline     = 2;
  ftBoldHighRes = 3;
  {ftNormal   = 0;
  ftBold     = 1;
  ftOutline1 = 2;
  ftOutline2 = 3;
  ftBoldHighRes = 4;}

var
  Fonts:   array of array of TGLFont; // 1. dimension: font family, 2. dimesion: font style (regular, bold, outline, boldhighres)
  CurrentFont: TFont;
  FontFamilyNames: array of UTF8String;

procedure BuildFonts;                         // builds all fonts
procedure KillFonts;                          // deletes all font
function  glTextWidth(const text: UTF8String): real; // returns text width
procedure glPrint(const text: UTF8String);    // custom GL "Print" routine
procedure ResetFont();                        // reset font settings of active font
procedure SetFontPos(X, Y: real);             // sets X and Y
procedure SetFontZ(Z: real);                  // sets Z
procedure SetFontSize(Size: real);
procedure SetFontFamily(FontFamily: integer); // sets active font family
procedure SetFontStyle(FontStyle: integer);   // sets active font style (regular, bold, outline, boldhighres)
procedure SetFont(Family, Style: integer); overload;   // sets active font (family + style)
procedure SetFont(Font: TFont); overload;              // sets active font (family + style)
procedure SetFontItalic(Enable: boolean);     // sets italic type letter (works for all fonts)
procedure SetFontReflection(Enable:boolean;Spacing: real); // enables/disables text reflection
procedure SetOutlineColor(R, G, B, A: GLFloat); // set outline color

implementation

uses
  UTextEncoding,
  SysUtils,
  IniFiles,
  UCommon,
  UMain,
  UPathUtils;

const
  FONT_STYLES: array [0..3] of string = (
    'Regular', 'Bold', 'Outline', 'BoldHighRes'
  );

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

procedure BuildFonts;
var
  FontNameIndex, FontStyleIndex, FallbackIndex: integer;
  FontIni: TMemIniFile;
  FontFile: IPath;
  FontMaxResolution: Integer;
  FontPreCache: Integer;
  Outline: single;
  Embolden: single;
  OutlineFont: TFTScalableOutlineFont;
  SectionName: string;
  Sections: TStringList;
begin
  CurrentFont.FontFamily := 0;
  CurrentFont.FontStyle := 0;

  FontIni := TMemIniFile.Create(FontPath.Append('fonts.ini').ToNative);

  // each section describes one font family
  Sections := TStringList.Create;
  FontIni.ReadSections(Sections);

  // read font family names into FontFamilyNamesArray (to be used in ScreenOptionsLyrics)
  SetLength(FontFamilyNames, 0);
  for FontNameIndex := 0 to Sections.Count-1 do
  begin
    //if (Sections[FontNameIndex].StartsWith('Font_')) then // .StartsWith() does not compile on Travis-CI
    if (LeftStr(Sections[FontNameIndex], 5) = 'Font_') then
      begin
        SetLength(FontFamilyNames, Length(FontFamilyNames) + 1);
        //FontFamilyNames[FontNameIndex] := FontIni.ReadString(Sections[FontNameIndex], 'Name', Sections[FontNameIndex].Remove(0, 5)); // .Remove() does not compile on Travis-CI
        FontFamilyNames[FontNameIndex] := FontIni.ReadString(Sections[FontNameIndex], 'Name', Copy(Sections[FontNameIndex], 5));
      end;
  end;

  // set font array size: Fonts[available font families (defined in fonts.ini)][possible font styles (fixed, see FONT_STYLES)]
  SetLength(Fonts, Length(FontFamilyNames), Length(FONT_STYLES));

  try
    for FontNameIndex := 0 to Sections.Count-1 do
    begin
      for FontStyleIndex := 0 to High(FONT_STYLES) do
      begin
        SectionName := Sections[FontNameIndex];
        FontFile := FindFontFile(FontIni.ReadString(SectionName, FONT_STYLES[FontStyleIndex] + 'File', ''));
        if (FontFile.Equals(PATH_NONE)) then
          Continue;

        FontMaxResolution := FontIni.ReadInteger(SectionName, FONT_STYLES[FontStyleIndex] + 'MaxResolution', 64);
        Embolden := FontIni.ReadFloat(SectionName, FONT_STYLES[FontStyleIndex] + 'Embolden', 0.0);
        Outline := FontIni.ReadFloat(SectionName, FONT_STYLES[FontStyleIndex] + 'Outline', 0.0);
        FontPreCache := FontIni.ReadInteger(SectionName, FONT_STYLES[FontStyleIndex] + 'PreCache', 1);

        if (FONT_STYLES[FontStyleIndex] <> 'Outline') then
        begin
          // normal (non-outlined) font
          Fonts[FontNameIndex][FontStyleIndex].Font := TFTScalableFont.Create(
            FontFile,
            FontMaxResolution,
            Embolden,
            True,
            (FontPreCache<>0)
          );
          Fonts[FontNameIndex][FontStyleIndex].Outlined := false;
        end
        else
        begin
          // outlined font
          OutlineFont := TFTScalableOutlineFont.Create(
            FontFile,
            FontMaxResolution,
            Outline,
            True,
            (FontPreCache<>0)
          );
          OutlineFont.SetOutlineColor(
            FontIni.ReadFloat(SectionName, 'OutlineColorR',  0.0),
            FontIni.ReadFloat(SectionName, 'OutlineColorG',  0.0),
            FontIni.ReadFloat(SectionName, 'OutlineColorB',  0.0),
            FontIni.ReadFloat(SectionName, 'OutlineColorA', -1.0)
          );
          Fonts[FontNameIndex][FontStyleIndex].Font := OutlineFont;
          Fonts[FontNameIndex][FontStyleIndex].Outlined := true;
        end;

        Fonts[FontNameIndex][FontStyleIndex].Font.GlyphSpacing := FontIni.ReadFloat(SectionName, FONT_STYLES[FontStyleIndex] + 'GlyphSpacing', 0.0);
        Fonts[FontNameIndex][FontStyleIndex].Font.Stretch := FontIni.ReadFloat(SectionName, FONT_STYLES[FontStyleIndex] + 'Stretch', 1.0);

        for FallbackIndex := 1 to 25 do
        begin
          FontFile := FindFontFile(FontIni.ReadString(SectionName , FONT_STYLES[FontStyleIndex] + 'FallbackFile' + IntToStr(FallbackIndex), ''));
          if (FontFile.Equals(PATH_NONE)) then
            Continue;
          try
            Fonts[FontNameIndex][FontStyleIndex].Font.AddFallback(FontFile);
          except
            on E: EFontError do
              Log.LogError('Setting font fallback ''' + FontFile.ToNative() + ''' failed: ' + E.Message);
          end;
        end;
      end;
    end;
  except
    on E: EFontError do
      Log.LogCritical(E.Message, 'BuildFont');
  end;

  Sections.Free;
  // close ini-file
  FontIni.Free;
end;


// Deletes the font
procedure KillFonts;
var
  I, J: integer;
begin
  for I := 0 to High(Fonts) do
    for J := 0 to High(Fonts[I]) do
      Fonts[I][J].Font.Free;
end;

function glTextWidth(const text: UTF8String): real;
var
  Bounds: TBoundsDbl;
begin
  Bounds := Fonts[CurrentFont.FontFamily][CurrentFont.FontStyle].Font.BBox(Text, true);
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

  GLFont := @Fonts[CurrentFont.FontFamily][CurrentFont.FontStyle];

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
  SetOutlineColor(0,0,0,1);
end;

procedure SetFontPos(X, Y: real);
begin
  Fonts[CurrentFont.FontFamily][CurrentFont.FontStyle].X := X;
  Fonts[CurrentFont.FontFamily][CurrentFont.FontStyle].Y := Y;
end;

procedure SetFontZ(Z: real);
begin
  Fonts[CurrentFont.FontFamily][CurrentFont.FontStyle].Z := Z;
end;

procedure SetFontSize(Size: real);
begin
  Fonts[CurrentFont.FontFamily][CurrentFont.FontStyle].Font.Height := Size;
end;

procedure SetFontFamily(FontFamily: integer);
begin
  CurrentFont.FontFamily := FontFamily;
end;

procedure SetFontStyle(FontStyle: integer);
begin
  CurrentFont.FontStyle := FontStyle;
end;

procedure SetFont(Family, Style: integer);
begin
  SetFontFamily(Family);
  SetFontStyle(Style);
end;

procedure SetFont(Font: TFont);
begin
  SetFontFamily(Font.FontFamily);
  SetFontStyle(Font.FontStyle);
end;

procedure SetFontItalic(Enable: boolean);
begin
  if (Enable) then
    Fonts[CurrentFont.FontFamily][CurrentFont.FontStyle].Font.Style := Fonts[CurrentFont.FontFamily][CurrentFont.FontStyle].Font.Style + [Italic]
  else
    Fonts[CurrentFont.FontFamily][CurrentFont.FontStyle].Font.Style := Fonts[CurrentFont.FontFamily][CurrentFont.FontStyle].Font.Style - [Italic]
end;

procedure SetFontReflection(Enable: boolean; Spacing: real);
begin
  if (Enable) then
    Fonts[CurrentFont.FontFamily][CurrentFont.FontStyle].Font.Style := Fonts[CurrentFont.FontFamily][CurrentFont.FontStyle].Font.Style + [Reflect]
  else
    Fonts[CurrentFont.FontFamily][CurrentFont.FontStyle].Font.Style := Fonts[CurrentFont.FontFamily][CurrentFont.FontStyle].Font.Style - [Reflect];
  Fonts[CurrentFont.FontFamily][CurrentFont.FontStyle].Font.ReflectionSpacing := Spacing - Fonts[CurrentFont.FontFamily][CurrentFont.FontStyle].Font.Descender;
end;

procedure SetOutlineColor(R, G, B, A: GLFloat);
begin
  if (Fonts[CurrentFont.FontFamily][CurrentFont.FontStyle].Outlined) then
    TFTScalableOutlineFont(Fonts[CurrentFont.FontFamily][CurrentFont.FontStyle].Font).SetOutlineColor(R, G, B, A);
end;

end.
