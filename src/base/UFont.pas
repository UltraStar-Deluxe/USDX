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

unit UFont;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

interface

{$IFNDEF FREETYPE_DEMO}
  // Flip direction of y-axis.
  // Default is a cartesian coordinate system with y-axis in upper direction
  // but with USDX the y-axis is in lower direction.
  {$DEFINE FLIP_YAXIS}
  {$DEFINE BITMAP_FONT}
{$ENDIF}

uses
  FreeType,
  gl,
  glext,
  glu,
  sdl,
  Math,
  Classes,
  SysUtils,
  UUnicodeUtils,
  {$IFDEF BITMAP_FONT}
  UTexture,
  {$ENDIF}
  UPath;

type

  PGLubyteArray = ^TGLubyteArray;
  TGLubyteArray = array[0 .. (MaxInt div SizeOf(GLubyte))-1] of GLubyte;
  TGLubyteDynArray = array of GLubyte;

  TUCS4StringArray = array of UCS4String;

  TGLColor = packed record
    case byte of
      0: ( vals: array[0..3] of GLfloat; );
      1: ( r, g, b, a: GLfloat; );
  end;

  TBoundsDbl = record
    Left, Right: double;
    Bottom, Top: double;
  end;

  TPositionDbl = record
    X, Y: double;
  end;

  TTextureSize = record
    Width, Height: integer;
  end;

  TBitmapCoords = record
    Left, Top: double;
    Width, Height: integer;
  end;

  {**
   * Abstract base class representing a glyph.
   *}
  TGlyph = class
    protected
      function GetAdvance(): TPositionDbl; virtual; abstract;
      function GetBounds(): TBoundsDbl; virtual; abstract;
    public
      procedure Render(UseDisplayLists: boolean); virtual; abstract;
      procedure RenderReflection(); virtual; abstract;

      {** Distance to next glyph (in pixels) *}
      property Advance: TPositionDbl read GetAdvance;
      {** Glyph bounding box (in pixels) *}
      property Bounds: TBoundsDbl read GetBounds;
  end;

  {**
   * Font styles used by TFont.Style
   *}
  TFontStyle = set of (Italic, Underline, Reflect);

  {**
   * Base font class.
   *}
  TFont = class
    private
      {** Non-virtual reset-method used in Create() and Reset() }
      procedure ResetIntern();

    protected
      fStyle: TFontStyle;
      fUseKerning: boolean;       
      fLineSpacing: single;       // must be inited by subclass
      fReflectionSpacing: single; // must be inited by subclass to -2*Descender
      fGlyphSpacing: single;
      fReflectionPass: boolean;

      {**
       * Splits lines in Text seperated by newline (char-code #13).
       * @param Text   UCS-4 encoded string
       * @param Lines  splitted UCS4String lines
       *}
      procedure SplitLines(const Text: UCS4String; var Lines: TUCS4StringArray);

      {**
       * Print an array of UCS4Strings. Each array-item is a line of text.
       * Lines of text are seperated by the line-spacing.
       * This is the base function for all text drawing.
       *}
      procedure Print(const Text: TUCS4StringArray); overload; virtual;

      {**
       * Draws an underline.
       *}
      procedure DrawUnderline(const Text: UCS4String); virtual;

      {**
       * Renders (one) line of text.
       *}
      procedure Render(const Text: UCS4String); virtual; abstract;

      {**
       * Returns the bounds of text-lines contained in Text.
       * @param(Advance  if true the right bound is set to the advance instead
       *   of the minimal right bound.)
       *}
      function BBox(const Text: TUCS4StringArray; Advance: boolean): TBoundsDbl; overload; virtual; abstract;

      {**
       * Resets all user settings to default values.
       * Override methods should always call the inherited version.
       *}
      procedure Reset(); virtual;

      function GetHeight(): single; virtual; abstract;
      function GetAscender(): single; virtual; abstract;
      function GetDescender(): single; virtual; abstract;
      procedure SetLineSpacing(Spacing: single); virtual;
      function GetLineSpacing(): single; virtual;
      procedure SetGlyphSpacing(Spacing: single); virtual;
      function GetGlyphSpacing(): single; virtual;
      procedure SetReflectionSpacing(Spacing: single); virtual;
      function GetReflectionSpacing(): single; virtual;
      procedure SetStyle(Style: TFontStyle); virtual;
      function GetStyle(): TFontStyle; virtual;
      function GetUnderlinePosition(): single; virtual; abstract;
      function GetUnderlineThickness(): single; virtual; abstract;
      procedure SetUseKerning(Enable: boolean); virtual;
      function GetUseKerning(): boolean; virtual;
      procedure SetReflectionPass(Enable: boolean); virtual;

      {** Returns true if the current render-pass is used to draw the reflection }
      property ReflectionPass: boolean read fReflectionPass write SetReflectionPass;

    public
      constructor Create();
      destructor Destroy(); override;

      {**
       * Prints a text.
       *}
      procedure Print(const Text: UCS4String); overload;
      {** UTF-16 version of @link(Print) }
      procedure Print(const Text: WideString); overload;
      {** UTF-8 version of @link(Print) }
      procedure Print(const Text: UTF8String); overload;

      {**
       * Calculates the bounding box (width and height) around Text.
       * Works with Italic and Underline styles but reflections created
       * with the Reflect style are not considered.
       * Note that the width might differ due to kerning with appended text,
       * e.g. Width('VA') <= Width('V') + Width('A').
       * @param Advance  if set to true, Result.Right is set to the advance of
       * the given text rather than the min. right border. The advance width is
       * bigger than the text's width as it additionally contains the advance
       * and glyph-spacing of the last character.
       *}
      function BBox(const Text: UCS4String; Advance: boolean = true): TBoundsDbl; overload;
      {** UTF-16 version of @link(BBox) }
      function BBox(const Text: WideString; Advance: boolean = true): TBoundsDbl; overload;
      {** UTF-8 version of @link(BBox) }
      function BBox(const Text: UTF8String; Advance: boolean = true): TBoundsDbl; overload;

      {** Font height }
      property Height: single read GetHeight;
      {** Vertical distance from baseline to top of glyph }
      property Ascender: single read GetAscender;
      {** Vertical distance from baseline to bottom of glyph }
      property Descender: single read GetDescender;
      {** Vertical distance between two baselines }
      property LineSpacing: single read GetLineSpacing write SetLineSpacing;
      {** Space between end and start of next glyph added to the advance width }
      property GlyphSpacing: single read GetGlyphSpacing write SetGlyphSpacing;
      {** Distance between normal baseline and baseline of the reflection }
      property ReflectionSpacing: single read GetReflectionSpacing write SetReflectionSpacing;
      {** Font style (italic/underline/...) }
      property Style: TFontStyle read GetStyle write SetStyle;
      {** If set to true (default) kerning will be used if available }
      property UseKerning: boolean read GetUseKerning write SetUseKerning;
  end;

const
  //** Max. number of mipmap levels that a TScalableFont can contain
  cMaxMipmapLevel = 5;

type
  {**
   * Wrapper around TFont to allow font size changes.
   * The font is scaled to the requested size by a modelview matrix
   * transformation (glScale) and not by rescaling the internal bitmap
   * representation. This way changing the size is really fast but the result
   * may lack quality on large or small scale factors.
   *}
  TScalableFont = class(TFont)
    private
      procedure ResetIntern();

    protected
      fScale: single;        //**< current height to base-font height ratio 
      fAspect: single;       //**< width to height aspect
      fBaseFont: TFont;      //**< shortcut for fMipmapFonts[0]
      fUseMipmaps: boolean;  //**< true if mipmap fonts are generated
      /// Mipmap fonts (size[level+1] = size[level]/2)
      fMipmapFonts: array[0..cMaxMipmapLevel] of TFont;

      procedure Render(const Text: UCS4String); override;
      procedure Print(const Text: TUCS4StringArray); override;
      function BBox(const Text: TUCS4StringArray; Advance: boolean): TBoundsDbl; override;

      {**
       * Callback called for creation of each mipmap font.
       * Must be defined by the subclass.
       * Mipmaps created by this method are managed and freed by TScalableFont.
       *}
      function CreateMipmap(Level: integer; Scale: single): TFont; virtual; abstract;

      {**
       * Returns the mipmap level considering the current scale and projection
       * matrix.
       *}
      function GetMipmapLevel(): integer;

      {**
       * Returns the scale applied to the given mipmap font.
       * fScale * fBaseFont.Height / fMipmapFont[Level].Height
       *}
      function GetMipmapScale(Level: integer): single;

      {**
       * Chooses the mipmap that looks nicest with current scale and projection
       * matrix.
       *}
      function ChooseMipmapFont(): TFont;

      procedure SetHeight(Height: single); virtual;
      function GetHeight(): single; override;
      procedure SetAspect(Aspect: single); virtual;
      function GetAspect(): single; virtual;
      function GetAscender(): single; override;
      function GetDescender(): single; override;
      procedure SetLineSpacing(Spacing: single); override;
      function GetLineSpacing(): single; override;
      procedure SetGlyphSpacing(Spacing: single); override;
      function GetGlyphSpacing(): single; override;
      procedure SetReflectionSpacing(Spacing: single); override;
      function GetReflectionSpacing(): single; override;
      procedure SetStyle(Style: TFontStyle); override;
      function GetStyle(): TFontStyle; override;
      function GetUnderlinePosition(): single; override;
      function GetUnderlineThickness(): single; override;
      procedure SetUseKerning(Enable: boolean); override;

    public
      {**
       * Creates a wrapper to make the base-font Font scalable.
       * If UseMipmaps is set to true smaller fonts are created so that a
       * resized (Height property changed) font looks nicer.
       * The font passed is managed and freed by TScalableFont.
       *}
      constructor Create(Font: TFont; UseMipmaps: boolean); overload;

      {**
       * Frees memory. The fonts passed on Create() and mipmap creation
       * are freed too. 
       *}
      destructor Destroy(); override;

      {** @seealso TFont.Reset }
      procedure Reset(); override;

      {** Font height }
      property Height: single read GetHeight write SetHeight;
      {** Factor for font stretching (NewWidth = Width*Aspect), 1.0 by default }
      property Aspect: single read GetAspect write SetAspect;
  end;

  {**
   * Table for storage of max. 256 glyphs.
   * Used for the second cache level. Indexed by the LSB of the UCS4Char
   * char-code.
   *}
  PGlyphTable = ^TGlyphTable;
  TGlyphTable = array[0..255] of TGlyph;

  {**
   * Cache for glyphs of a single font.
   * The cached glyphs are stored inside a hash-list.
   * Hashing is performed in two steps:
   * 1. the least significant byte (LSB) of the UCS4Char character code
   * is removed (shr 8) and the result (we call it BaseCode here) looked up in
   * the hash-list.
   * 2. Each entry of the hash-list contains a table with max. 256 entries.
   * The LSB of the char-code of a glyph is the table-offset of that glyph.
   *}
  TGlyphCache = class
    private
      fHash: TList;

      {**
       * Finds a glyph-table storing cached glyphs with base-code BaseCode
       * (= upper char-code bytes) in the hash-list and returns the table and
       * its index.
       * @param(InsertPos  the position of the tyble in the list if it was found,
       *                   otherwise the position the table should be inserted)
       *}
      function FindGlyphTable(BaseCode: cardinal; out InsertPos: integer): PGlyphTable;

    public
      constructor Create();
      destructor Destroy(); override;

      {**
       * Add glyph Glyph with char-code ch to the cache.
       * @returns @true on success, @false otherwise
       *}
      function AddGlyph(ch: UCS4Char; const Glyph: TGlyph): boolean;

      {**
       * Removes the glyph with char-code ch from the cache.
       *}
      procedure DeleteGlyph(ch: UCS4Char);

      {**
       * Removes the glyph with char-code ch from the cache.
       *}
      function GetGlyph(ch: UCS4Char): TGlyph;

      {**
       * Checks if a glyph with char-code ch is cached.
       *}
      function HasGlyph(ch: UCS4Char): boolean;

      {**
       * Remove and free all cached glyphs. If KeepBaseSet is set to
       * true, cached characters in the range 0..255 will not be flushed.
       *}
      procedure FlushCache(KeepBaseSet: boolean);
  end;

  {**
   * Entry of a glyph-cache's (TGlyphCache) hash.
   * Stores a BaseCode (upper-bytes of a glyph's char-code) and a table
   * with all glyphs cached at the moment with that BaseCode.
   *}
  TGlyphCacheHashEntry = class
    private
      fBaseCode: cardinal;
    public
      GlyphTable: TGlyphTable;

      constructor Create(BaseCode: cardinal);

      {** Base-code (upper-bytes) of the glyphs stored in this entry's table }
      property BaseCode: cardinal read fBaseCode;
  end;

  TCachedFont = class(TFont)
    protected
      fCache: TGlyphCache;

      {**
       * Retrieves a cached glyph with char-code ch from cache.
       * If the glyph is not already cached, it is loaded with LoadGlyph().
       *}
      function GetGlyph(ch: UCS4Char): TGlyph;

      {**
       * Callback to create (load) a glyph with char-code ch.
       * Implemented by subclasses.
       *}
      function LoadGlyph(ch: UCS4Char): TGlyph; virtual; abstract;

    public
      constructor Create();
      destructor Destroy(); override;

      {**
       * Remove and free all cached glyphs. If KeepBaseSet is set to
       * true, the base glyphs are not be flushed.
       * @seealso TGlyphCache.FlushCache
       *}
      procedure FlushCache(KeepBaseSet: boolean);
  end;

  TFTFont = class;

  {**
   * Freetype glyph.
   * Each glyph stores a texture with the glyph's image.
   *}
  TFTGlyph = class(TGlyph)
    private
      fCharCode:  UCS4Char;     //**< Char code
      fCharIndex: FT_UInt;      //**< Freetype specific char-index (<> char-code)
      fDisplayList: GLuint;     //**< Display-list ID
      fTexture: GLuint;         //**< Texture ID
      fBitmapCoords: TBitmapCoords; //**< Left/Top offset and Width/Height of the bitmap (in pixels)
      fTexOffset: TPositionDbl; //**< Right and bottom texture offset for removal of power-of-2 padding
      fTexSize: TTextureSize;   //**< Texture size in pixels

      fFont: TFTFont;           //**< Font associated with this glyph
      fAdvance: TPositionDbl;   //**< Advance width of this glyph
      fBounds: TBoundsDbl;      //**< Glyph bounds
      fOutset: single;          //**< Extrusion outset

      {**
       * Extrudes the outline of a glyph's bitmap stored in TexBuffer with size
       * fTexSize by Outset pixels.
       * This is useful to create bold or outlined fonts.
       * TexBuffer must be 2*Ceil(Outset) pixels higher and wider than the
       * original glyph bitmap, otherwise the glyph borders cannot be extruded
       * correctly.
       * The bitmap must be 2* pixels wider and higher than the
       * original glyph's bitmap with the latter centered in it.
       *}
      procedure StrokeBorder(var Glyph: FT_Glyph);

      {**
       * Creates an OpenGL texture (and display list) for the glyph.
       * The glyph's and bitmap's metrics are set correspondingly.
       * @param  LoadFlags  flags passed to FT_Load_Glyph()
       * @raises Exception  if the glyph could not be initialized
       *}
      procedure CreateTexture(LoadFlags: FT_Int32);

    protected
      function GetAdvance(): TPositionDbl; override;
      function GetBounds(): TBoundsDbl; override;

    public
      {**
       * Creates a glyph with char-code ch from font Font.
       * @param LoadFlags  flags passed to FT_Load_Glyph()
       *}
      constructor Create(Font: TFTFont; ch: UCS4Char; Outset: single;
                         LoadFlags: FT_Int32);
      destructor Destroy(); override;

      {** Renders the glyph (normal render pass) }
      procedure Render(UseDisplayLists: boolean); override;
      {** Renders the glyph's reflection }
      procedure RenderReflection(); override;

      {** Freetype specific char-index (<> char-code) }
      property CharIndex: FT_UInt read fCharIndex;
  end;

  TFontPart = ( fpNone, fpInner, fpOutline );

  {**
   * Freetype font class.
   *}
  TFTFont = class(TCachedFont)
    private
      procedure ResetIntern();

    protected
      fFilename: IPath;             //**< filename of the font-file
      fSize: integer;               //**< Font base size (in pixels)
      fOutset: single;              //**< size of outset extrusion (in pixels)
      fFace: FT_Face;               //**< Holds the height of the font
      fLoadFlags: FT_Int32;         //**< FT glpyh load-flags
      fFontUnitScale: TPositionDbl; //**< FT font-units to pixel ratio
      fUseDisplayLists: boolean;    //**< true: use display-lists, false: direct drawing
      fPart: TFontPart;             //**< indicates the part of an outline font

      {** @seealso TCachedFont.LoadGlyph }
      function LoadGlyph(ch: UCS4Char): TGlyph; override;

      procedure Render(const Text: UCS4String); override;
      function BBox(const Text: TUCS4StringArray; Advance: boolean): TBoundsDbl; override;

      function GetHeight(): single; override;
      function GetAscender(): single; override;
      function GetDescender(): single; override;
      function GetUnderlinePosition(): single; override;
      function GetUnderlineThickness(): single; override;

      property Face: FT_Face read fFace;

    public
      {**
       * Creates a font of size Size (in pixels) from the file Filename.
       * If Outset (in pixels) is set to a value > 0 the glyphs will be extruded
       * at their borders. Use it for e.g. a bold effect.
       * @param  LoadFlags  flags passed to FT_Load_Glyph()
       * @raises Exception  if the font-file could not be loaded
       *}
      constructor Create(const Filename: IPath;
                         Size: integer; Outset: single = 0.0;
                         LoadFlags: FT_Int32 = FT_LOAD_DEFAULT);

      {**
       * Frees all resources associated with the font.
       *}
      destructor Destroy(); override;

      {** @seealso TFont.Reset }
      procedure Reset(); override;
      
      {** Size of the base font }
      property Size: integer read fSize;
      {** Outset size }
      property Outset: single read fOutset;
  end;

  TFTScalableFont = class(TScalableFont)
    protected
      function GetOutset(): single; virtual;
      function CreateMipmap(Level: integer; Scale: single): TFont; override;

    public
      {**
       * Creates a scalable font of size Size (in pixels) from the file Filename.
       * OutsetAmount is the ratio of the glyph extrusion.
       * The extrusion in pixels is Size*OutsetAmount
       * (0.0 -> no extrusion, 0.1 -> 10%).
       *}
      constructor Create(const Filename: IPath;
                         Size: integer; OutsetAmount: single = 0.0;
                         UseMipmaps: boolean = true);

      {** @seealso TGlyphCache.FlushCache }
      procedure FlushCache(KeepBaseSet: boolean);

      {** Outset size (in pixels) of the scaled font }
      property Outset: single read GetOutset;
  end;

  
  {**
   * Represents a freetype font with an additional outline around its glyphs.
   * The outline size is passed on creation and cannot be changed later.
   *}
  TFTOutlineFont = class(TFont)
    private
      fFilename: IPath;
      fSize: integer;
      fOutset: single;
      fInnerFont, fOutlineFont: TFTFont;
      fOutlineColor: TGLColor;

      procedure ResetIntern();
      
  protected
      procedure DrawUnderline(const Text: UCS4String); override;
      procedure Render(const Text: UCS4String); override;
      function BBox(const Text: TUCS4StringArray; Advance: boolean): TBoundsDbl; override;

      function GetHeight(): single; override;
      function GetAscender(): single; override;
      function GetDescender(): single; override;
      procedure SetLineSpacing(Spacing: single); override;
      procedure SetGlyphSpacing(Spacing: single); override;
      procedure SetReflectionSpacing(Spacing: single); override;
      procedure SetStyle(Style: TFontStyle); override;
      function GetStyle(): TFontStyle; override;
      function GetUnderlinePosition(): single; override;
      function GetUnderlineThickness(): single; override;
      procedure SetUseKerning(Enable: boolean); override;
      procedure SetReflectionPass(Enable: boolean); override;

    public
      constructor Create(const Filename: IPath;
                         Size: integer; Outset: single;
                         LoadFlags: FT_Int32 = FT_LOAD_DEFAULT);
      destructor Destroy; override;

      {**
       * Sets the color of the outline.
       * If the alpha component is < 0, OpenGL's current alpha value will be
       * used.
       *}
      procedure SetOutlineColor(r, g, b: GLfloat; a: GLfloat = -1.0);

      {** @seealso TGlyphCache.FlushCache }
      procedure FlushCache(KeepBaseSet: boolean);

      {** @seealso TFont.Reset }
      procedure Reset(); override;

      {** Size of the base font }
      property Size: integer read fSize;
      {** Outset size }
      property Outset: single read fOutset;
  end;

  {**
   * Wrapper around TOutlineFont to allow font resizing.
   * @seealso TScalableFont
   *}
  TFTScalableOutlineFont = class(TScalableFont)
    protected
      function GetOutset(): single; virtual;
      function CreateMipmap(Level: integer; Scale: single): TFont; override;

    public
      constructor Create(const Filename: IPath;
                         Size: integer; OutsetAmount: single;
                         UseMipmaps: boolean = true);

      {** @seealso TFTOutlineFont.SetOutlineColor }
      procedure SetOutlineColor(r, g, b: GLfloat; a: GLfloat = -1.0);

      {** @seealso TGlyphCache.FlushCache }
      procedure FlushCache(KeepBaseSet: boolean);

      {** Outset size }
      property Outset: single read GetOutset;
  end;

{$IFDEF BITMAP_FONT}

  {**
   * A bitmapped font loads it's glyphs from a bitmap and stores them in a
   * texture. Unicode characters are not supported (but could be by supporting
   * multiple textures each storing a subset of unicode glyphs).
   * For backward compatibility only.
   *}
  TBitmapFont = class(TFont)
    private
      fTex:       TTexture;
      fTexSize:   integer;
      fBaseline:  integer;
      fAscender:  integer;
      fDescender: integer;
      fWidths:    array[0..255] of byte; //**< half widths
      fOutline:   integer;
      fTempColor: TGLColor; //**< colours for the reflection

      procedure ResetIntern();

      procedure RenderChar(ch: UCS4Char; var AdvanceX: real);

      {**
       * Load font widths from an info file.
       * @param  InfoFile  the name of the info (.dat) file
       * @raises Exception if the file is corrupted
       *}
      procedure LoadFontInfo(const InfoFile: IPath);

    protected
      procedure Render(const Text: UCS4String); override;
      function BBox(const Text: TUCS4StringArray; Advance: boolean): TBoundsDbl; override;

      function GetHeight(): single; override;
      function GetAscender(): single; override;
      function GetDescender(): single; override;
      function GetUnderlinePosition(): single; override;
      function GetUnderlineThickness(): single; override;

    public
      {**
       * Creates a bitmapped font from image Filename and font width info
       * loaded from the corresponding file with ending .dat.
       * @param(Baseline  y-coord of the baseline given in cartesian coords
       *        (y-axis up) and from the lower edge of the glyphs bounding box)
       * @param(Ascender  pixels from baseline to top of highest glyph)
       *}
      constructor Create(const Filename: IPath; Outline: integer;
                         Baseline, Ascender, Descender: integer);
      destructor Destroy(); override;

      {**
       * Corrects font widths provided by the info file.
       * NewWidth := Width * WidthMult + WidthAdd
       *}
      procedure CorrectWidths(WidthMult: real; WidthAdd: integer);

      {** @seealso TFont.Reset }
      procedure Reset(); override;
  end;

{$ENDIF BITMAP_FONT}

  TFreeType = class
    public
      {**
       * Returns a pointer to the freetype library singleton.
       * If non exists, freetype will be initialized.
       * @raises Exception if initialization failed
       *}
      class function GetLibrary(): FT_Library;
      class procedure FreeLibrary();
  end;


implementation

uses Types;

const
  //** shear factor used for the italic effect (bigger value -> more bending)
  cShearFactor = 0.25;
  cShearMatrix: array[0..15] of GLfloat = (
      1,            0, 0, 0,
      cShearFactor, 1, 0, 0,
      0,            0, 1, 0,
      0,            0, 0, 1
  );
  cShearMatrixInv: array[0..15] of GLfloat = (
      1,             0, 0, 0,
      -cShearFactor, 1, 0, 0,
      0,             0, 1, 0,
      0,             0, 0, 1
  );

var
  LibraryInst: FT_Library;

function NewGLColor(r, g, b, a: GLfloat): TGLColor;
begin
  Result.r := r;
  Result.g := g;
  Result.b := b;
  Result.a := a;
end;

{**
 * Returns the first power of 2 >= Value.
 *}
function NextPowerOf2(Value: integer): integer; {$IFDEF HasInline}inline;{$ENDIF}
begin
  Result := 1;
  while (Result < Value) do
    Result := Result shl 1;
end;


{*
 * TFont
 *}

constructor TFont.Create();
begin
  inherited;
  ResetIntern();
end;

destructor TFont.Destroy();
begin
  inherited;
end;

procedure TFont.ResetIntern();
begin
  fStyle := [];
  fUseKerning := true;
  fGlyphSpacing := 0.0;
  fReflectionPass := false;

  // must be set by subclasses
  fLineSpacing := 0.0;
  fReflectionSpacing := 0.0;
end;

procedure TFont.Reset();
begin
  ResetIntern();
end;

procedure TFont.SplitLines(const Text: UCS4String; var Lines: TUCS4StringArray);
var
  CharIndex: integer;
  LineStart: integer;
  LineLength: integer;
  EOT: boolean; // End-Of-Text
begin
  // split lines on newline
  SetLength(Lines, 0);
  EOT := false;
  LineStart := 0;

  for CharIndex := 0 to High(Text) do
  begin
    // check for end of text (UCS4Strings are zero-terminated)
    if (CharIndex = High(Text)) then
      EOT := true;

    // check for newline (carriage return (#13)) or end of text
    if (Text[CharIndex] = 13) or EOT then
    begin
      LineLength := CharIndex - LineStart;
      // check if last character was a newline
      if (EOT and (LineLength = 0)) then
        Break;      

      // copy line (even if LineLength is 0)
      SetLength(Lines, Length(Lines)+1);
      Lines[High(Lines)] := UCS4Copy(Text, LineStart, LineLength);

      LineStart := CharIndex+1;
    end;
  end;
end;

function TFont.BBox(const Text: UCS4String; Advance: boolean): TBoundsDbl;
var
  LineArray: TUCS4StringArray;
begin
  SplitLines(Text, LineArray);
  Result := BBox(LineArray, Advance);
  SetLength(LineArray, 0);
end;

function TFont.BBox(const Text: UTF8String; Advance: boolean): TBoundsDbl;
begin
  Result := BBox(UTF8Decode(Text), Advance);
end;

function TFont.BBox(const Text: WideString; Advance: boolean): TBoundsDbl;
begin
  Result := BBox(WideStringToUCS4String(Text), Advance);
end;

procedure TFont.Print(const Text: TUCS4StringArray);
var
  LineIndex: integer;
begin
  // recursively call this function to draw reflected text
  if ((Reflect in Style) and not ReflectionPass) then
  begin
    ReflectionPass := true;
    Print(Text);
    ReflectionPass := false;
  end;

  // store current color, enable-flags, matrix-mode
  glPushAttrib(GL_CURRENT_BIT or GL_ENABLE_BIT or GL_TRANSFORM_BIT);

  // set OpenGL state
  glMatrixMode(GL_MODELVIEW);
  glDisable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glEnable(GL_TEXTURE_2D);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  {
  // TODO: just draw texels with alpha > 0 to avoid setting z-buffer for them?
  glAlphaFunc(GL_GREATER, 0);
  glEnable(GL_ALPHA_TEST);

  //TODO: Do we need depth-testing?
  if (ReflectionPass) then
  begin
    glDepthMask(0);
    glEnable(GL_DEPTH_TEST);
  end;
  }

  {$IFDEF FLIP_YAXIS}
  glPushMatrix();
  glScalef(1, -1, 1);
  {$ENDIF}

  // display text
  for LineIndex := 0 to High(Text) do
  begin
    glPushMatrix();

    // move to baseline
    glTranslatef(0, -LineSpacing*LineIndex, 0);

    if ((Underline in Style) and not ReflectionPass) then
    begin
      glDisable(GL_TEXTURE_2D);
      DrawUnderline(Text[LineIndex]);
      glEnable(GL_TEXTURE_2D);
    end;

    // draw reflection
    if (ReflectionPass) then
    begin
      // set reflection spacing
      glTranslatef(0, -ReflectionSpacing, 0);
      // flip y-axis
      glScalef(1, -1, 1);
    end;

    // shear for italic effect
    if (Italic in Style) then
      glMultMatrixf(@cShearMatrix);

    // render text line
    Render(Text[LineIndex]);

    glPopMatrix();
  end;

  // restore settings
  {$IFDEF FLIP_YAXIS}
  glPopMatrix();
  {$ENDIF}
  glPopAttrib();
end;

procedure TFont.Print(const Text: UCS4String);
var
  LineArray: TUCS4StringArray;
begin
  SplitLines(Text, LineArray);
  Print(LineArray);
  SetLength(LineArray, 0);
end;

procedure TFont.Print(const Text: UTF8String);
begin
  Print(UTF8Decode(Text));
end;

procedure TFont.Print(const Text: WideString);
begin
  Print(WideStringToUCS4String(Text));
end;

procedure TFont.DrawUnderline(const Text: UCS4String);
var
  UnderlineY1, UnderlineY2: single;
  Bounds: TBoundsDbl;
begin
  UnderlineY1 := GetUnderlinePosition();
  UnderlineY2 := UnderlineY1 + GetUnderlineThickness();
  Bounds := BBox(Text, false);
  glRectf(Bounds.Left, UnderlineY1, Bounds.Right, UnderlineY2);
end;

procedure TFont.SetStyle(Style: TFontStyle);
begin
  fStyle := Style;
end;

function TFont.GetStyle(): TFontStyle;
begin
  Result := fStyle;
end;

procedure TFont.SetLineSpacing(Spacing: single);
begin
  fLineSpacing := Spacing;
end;

function TFont.GetLineSpacing(): single;
begin
  Result := fLineSpacing;
end;

procedure TFont.SetGlyphSpacing(Spacing: single);
begin
  fGlyphSpacing := Spacing;
end;

function TFont.GetGlyphSpacing(): single;
begin
  Result := fGlyphSpacing;
end;

procedure TFont.SetReflectionSpacing(Spacing: single);
begin
  fReflectionSpacing := Spacing;
end;

function TFont.GetReflectionSpacing(): single;
begin
  Result := fReflectionSpacing;
end;

procedure TFont.SetUseKerning(Enable: boolean);
begin
  fUseKerning := Enable;
end;

function TFont.GetUseKerning(): boolean;
begin
  Result := fUseKerning;
end;

procedure TFont.SetReflectionPass(Enable: boolean);
begin
  fReflectionPass := Enable;
end;


{*
 * TScalableFont
 *}

constructor TScalableFont.Create(Font: TFont; UseMipmaps: boolean);
var
  MipmapLevel: integer;
begin
  inherited Create();
  
  fBaseFont := Font;
  fMipmapFonts[0] := Font;
  fUseMipmaps := UseMipmaps;
  ResetIntern();

  // create mipmap fonts if requested
  if (UseMipmaps) then
  begin
    for MipmapLevel := 1 to cMaxMipmapLevel do
    begin
      fMipmapFonts[MipmapLevel] := CreateMipmap(MipmapLevel, 1/(1 shl MipmapLevel));
      // stop if no smaller mipmap font is returned
      if (fMipmapFonts[MipmapLevel] = nil) then
        Break;
    end;
  end;
end;

destructor TScalableFont.Destroy();
var
  Level: integer;
begin
  for Level := 0 to High(fMipmapFonts) do
    fMipmapFonts[Level].Free;
  inherited;
end;

procedure TScalableFont.ResetIntern();
begin
  fScale := 1.0;
  fAspect := 1.0;
end;

procedure TScalableFont.Reset();
var
  Level: integer;
begin
  inherited;
  ResetIntern();
  for Level := 0 to High(fMipmapFonts) do
    if (fMipmapFonts[Level] <> nil) then
      fMipmapFonts[Level].Reset();
end;

{**
 * Returns the mipmap level to use with regard to the current projection
 * and modelview matrix, font scale and aspect.
 *
 * Note:
 * - for Freetype fonts, hinting and grid-fitting must be disabled, otherwise
 *   the glyph widths/heights ratios and advance widths of the mipmap fonts
 *   do not match as they are adjusted sligthly (e.g. an 'a' at size 12px has
 *   width 12px, but at size 6px width 8px).
 * - returned mipmap-level is used for all glyphs of the current text to print.
 *   This is faster, much easier to handle, since we just need to create
 *   multiple sized fonts and select the one we need for the mipmap-level and
 *   it avoids that neighbored glyphs use different mipmap-level which might
 *   look odd because one glyph might look blurry and the other sharp.
 *
 * Motivation:
 *   We do not use OpenGL for mipmapping as the results are very bad. At least
 *   with automatic mipmap generation (gluBuildMipmaps) the fonts look rather
 *   blurry.
 *   Defining our own mipmaps by creating multiple textures with
 *   for different mimap levels is a pain, as the font size passed to freetype
 *   is not the size of the bitmaps created and it does not guarantee that a
 *   glyph bitmap of a font with font-size s/2 is half the size of the font with
 *   font-size s. If the bitmap size is just a single pixel bigger than the half
 *   we might need a texture of the next power-of-2 and the texture would not be
 *   half of the size of the next bigger mipmap. In addition we use a fixed one
 *   pixel sized border to smooth the texture (see cTexSmoothBorder) and maybe
 *   an outset that is added to the font, so creating a glyph mipmap that is
 *   exactly half the size of the next bigger one is a very difficult task.
 *
 * Solution:
 *   Use mipmap textures that are not exactly half the size of the next mipmap
 *   level. OpenGL does not support this (at least not without extensions).
 *   The trickiest task is to determine the mipmap to use by calculating the
 *   amount of minification that is performed in this function.
 *}
function TScalableFont.GetMipmapLevel(): integer;
var
  ModelMatrix, ProjMatrix: T16dArray;
  WinCoords: array[0..2, 0..2] of GLdouble;
  ViewPortArray: TViewPortArray;
  Dist, Dist2: double;
  WidthScale, HeightScale: double;
const
  // width/height of square used for determining the scale
  cTestSize = 10.0;
  // an offset to the mipmap-level to adjust the change-over of two consecutive
  // mipmap levels. If for example the bias is 0.1 and unbiased level is 1.9
  // the result level will be 2. A bias of 0.5 is equal to rounding.
  // With bias=0.1 we prefer larger mipmaps over smaller ones.
  cBias = 0.2;
begin
  // 1. retrieve current transformation matrices for gluProject
  glGetDoublev(GL_MODELVIEW_MATRIX, @ModelMatrix);
  glGetDoublev(GL_PROJECTION_MATRIX, @ProjMatrix);
  glGetIntegerv(GL_VIEWPORT, @ViewPortArray);

  // 2. project three of the corner points of a square with size cTestSize
  // to window coordinates (the square is just a dummy for a glyph)

  // project point (x1, y1) to window corrdinates
  gluProject(0, 0, 0,
             ModelMatrix, ProjMatrix, ViewPortArray,
             @WinCoords[0][0], @WinCoords[0][1], @WinCoords[0][2]);
  // project point (x2, y1) to window corrdinates
  gluProject(cTestSize, 0, 0,
             ModelMatrix, ProjMatrix, ViewPortArray,
             @WinCoords[1][0], @WinCoords[1][1], @WinCoords[1][2]);
  // project point (x1, y2) to window corrdinates
  gluProject(0, cTestSize, 0,
             ModelMatrix, ProjMatrix, ViewPortArray,
             @WinCoords[2][0], @WinCoords[2][1], @WinCoords[2][2]);

  // 3. Lets see how much the width and height of the square changed.
  // Calculate the width and height as displayed on the screen in window
  // coordinates and calculate the ratio to the original coordinates in
  // modelview space so the ratio gives us the scale (minification here).

  // projected width ||(x1, y1) - (x2, y1)||
  Dist  := (WinCoords[0][0] - WinCoords[1][0]);
  Dist2 := (WinCoords[0][1] - WinCoords[1][1]);
  WidthScale := cTestSize / Sqrt(Dist*Dist + Dist2*Dist2);

  // projected height ||(x1, y1) - (x1, y2)||
  Dist  := (WinCoords[0][0] - WinCoords[2][0]);
  Dist2 := (WinCoords[0][1] - WinCoords[2][1]);
  HeightScale := cTestSize / Sqrt(Dist*Dist + Dist2*Dist2);

  //writeln(Format('Scale %f, %f', [WidthScale, HeightScale]));

  // 4. Now that we have got the scale, take the bigger minification scale
  // and get it to a logarithmic scale as each mipmap is 1/2 the size of its
  // predecessor (Mipmap_size[i] = Mipmap_size[i-1]/2).
  // The result is our mipmap-level = the index of the mipmap to use.

  // Level > 0: Minification; < 0: Magnification
  Result := Trunc(Log2(Max(WidthScale, HeightScale)) + cBias);

  // clamp to valid range
  if (Result < 0) then
    Result := 0;
  if (Result > High(fMipmapFonts)) then
    Result := High(fMipmapFonts);
end;

function TScalableFont.GetMipmapScale(Level: integer): single;
begin
  if (fMipmapFonts[Level] = nil) then
  begin
    Result := -1;
    Exit;
  end;

  Result := fScale * fMipmapFonts[0].Height / fMipmapFonts[Level].Height;
end;

{**
 * Returns the correct mipmap font for the current scale and projection
 * matrix. The modelview scale is adjusted to the mipmap level, so
 * Result.Print() will display the font in the correct size.
 *}
function TScalableFont.ChooseMipmapFont(): TFont;
var
  DesiredLevel: integer;
  Level: integer;
  MipmapScale: single;
begin
  Result := nil;
  DesiredLevel := GetMipmapLevel();

  // get the smallest mipmap available for the desired level
  // as not all levels must be assigned to a font.
  for Level := DesiredLevel downto 0 do
  begin
    if (fMipmapFonts[Level] <> nil) then
    begin
      Result := fMipmapFonts[Level];
      Break;
    end;
  end;

  // since the mipmap font (if level > 0) is smaller than the base-font
  // we have to scale to get its size right.
  MipmapScale := fMipmapFonts[0].Height/Result.Height;
  glScalef(MipmapScale, MipmapScale, 0);
end;

procedure TScalableFont.Print(const Text: TUCS4StringArray);
begin
  glPushMatrix();

  // set scale and stretching
  glScalef(fScale * fAspect, fScale, 0);

  // print text
  if (fUseMipmaps) then
    ChooseMipmapFont().Print(Text)
  else
    fBaseFont.Print(Text);

  glPopMatrix();
end;

procedure TScalableFont.Render(const Text: UCS4String);
begin
  Assert(false, 'Unused TScalableFont.Render() was called');
end;

function TScalableFont.BBox(const Text: TUCS4StringArray; Advance: boolean): TBoundsDbl;
begin
  Result := fBaseFont.BBox(Text, Advance);
  Result.Left   := Result.Left * fScale * fAspect;
  Result.Right  := Result.Right * fScale * fAspect;
  Result.Top    := Result.Top * fScale;
  Result.Bottom := Result.Bottom * fScale;
end;

procedure TScalableFont.SetHeight(Height: single);
begin
  fScale := Height / fBaseFont.GetHeight();
end;

function TScalableFont.GetHeight(): single;
begin
  Result := fBaseFont.GetHeight() * fScale;
end;

procedure TScalableFont.SetAspect(Aspect: single);
begin
  fAspect := Aspect;
end;

function TScalableFont.GetAspect(): single;
begin
  Result := fAspect;
end;

function TScalableFont.GetAscender(): single;
begin
  Result := fBaseFont.GetAscender() * fScale;
end;

function TScalableFont.GetDescender(): single;
begin
  Result := fBaseFont.GetDescender() * fScale;
end;

procedure TScalableFont.SetLineSpacing(Spacing: single);
var
  Level: integer;
begin
  for Level := 0 to High(fMipmapFonts) do
    if (fMipmapFonts[Level] <> nil) then
      fMipmapFonts[Level].SetLineSpacing(Spacing / GetMipmapScale(Level));
end;

function TScalableFont.GetLineSpacing(): single;
begin
  Result := fBaseFont.GetLineSpacing() * fScale;
end;

procedure TScalableFont.SetGlyphSpacing(Spacing: single);
var
  Level: integer;
begin
  for Level := 0 to High(fMipmapFonts) do
    if (fMipmapFonts[Level] <> nil) then
      fMipmapFonts[Level].SetGlyphSpacing(Spacing / GetMipmapScale(Level));
end;

function TScalableFont.GetGlyphSpacing(): single;
begin
  Result := fBaseFont.GetGlyphSpacing() * fScale;
end;

procedure TScalableFont.SetReflectionSpacing(Spacing: single);
var
  Level: integer;
begin
  for Level := 0 to High(fMipmapFonts) do
    if (fMipmapFonts[Level] <> nil) then
      fMipmapFonts[Level].SetReflectionSpacing(Spacing / GetMipmapScale(Level));
end;

function TScalableFont.GetReflectionSpacing(): single;
begin
  Result := fBaseFont.GetLineSpacing() * fScale;
end;

procedure TScalableFont.SetStyle(Style: TFontStyle);
var
  Level: integer;
begin
  for Level := 0 to High(fMipmapFonts) do
    if (fMipmapFonts[Level] <> nil) then
      fMipmapFonts[Level].SetStyle(Style);
end;

function TScalableFont.GetStyle(): TFontStyle;
begin
  Result := fBaseFont.GetStyle();
end;

function TScalableFont.GetUnderlinePosition(): single;
begin
  Result := fBaseFont.GetUnderlinePosition();
end;

function TScalableFont.GetUnderlineThickness(): single;
begin
  Result := fBaseFont.GetUnderlinePosition();
end;

procedure TScalableFont.SetUseKerning(Enable: boolean);
var
  Level: integer;
begin
  for Level := 0 to High(fMipmapFonts) do
    if (fMipmapFonts[Level] <> nil) then
      fMipmapFonts[Level].SetUseKerning(Enable);
end;


{*
 * TCachedFont
 *}

constructor TCachedFont.Create();
begin
  inherited;
  fCache := TGlyphCache.Create();
end;

destructor TCachedFont.Destroy();
begin
  fCache.Free;
  inherited;
end;

function TCachedFont.GetGlyph(ch: UCS4Char): TGlyph;
begin
  Result := fCache.GetGlyph(ch);
  if (Result = nil) then
  begin
    Result := LoadGlyph(ch);
    if (not fCache.AddGlyph(ch, Result)) then
      Result.Free;
  end;
end;

procedure TCachedFont.FlushCache(KeepBaseSet: boolean);
begin
  fCache.FlushCache(KeepBaseSet);
end;


{*
 * TFTFont
 *}

constructor TFTFont.Create(
    const Filename: IPath;
    Size: integer; Outset: single;
    LoadFlags: FT_Int32);
var
  ch: UCS4Char;
begin
  inherited Create();

  fFilename := Filename;
  fSize := Size;
  fOutset := Outset;
  fLoadFlags := LoadFlags;
  fUseDisplayLists := true;
  fPart := fpNone;

  // load font information
  if (FT_New_Face(TFreeType.GetLibrary(), PChar(Filename.ToNative), 0, fFace) <> 0) then
    raise Exception.Create('FT_New_Face: Could not load font '''  + Filename.ToNative + '''');

  // support scalable fonts only
  if (not FT_IS_SCALABLE(fFace)) then
    raise Exception.Create('Font is not scalable');

  if (FT_Set_Pixel_Sizes(fFace, 0, Size) <> 0) then
    raise Exception.Create('FT_Set_Pixel_Sizes failes');

  // get scale factor for font-unit to pixel-size transformation
  fFontUnitScale.X := fFace.size.metrics.x_ppem / fFace.units_per_EM;
  fFontUnitScale.Y := fFace.size.metrics.y_ppem / fFace.units_per_EM;

  ResetIntern();

  // pre-cache some commonly used glyphs (' ' - '~')
  for ch := 32 to 126 do
    fCache.AddGlyph(ch, TFTGlyph.Create(Self, ch, Outset, LoadFlags));
end;

destructor TFTFont.Destroy();
begin
  // free face
  FT_Done_Face(fFace);
  inherited;
end;

procedure TFTFont.ResetIntern();
begin
  // Note: outset and non outset fonts use same spacing
  fLineSpacing := fFace.height * fFontUnitScale.Y;
  fReflectionSpacing := -2*fFace.descender * fFontUnitScale.Y;
end;

procedure TFTFont.Reset();
begin
  inherited;
  ResetIntern();
end;

function TFTFont.LoadGlyph(ch: UCS4Char): TGlyph;
begin
  Result := TFTGlyph.Create(Self, ch, Outset, fLoadFlags);
end;

function TFTFont.BBox(const Text: TUCS4StringArray; Advance: boolean): TBoundsDbl;
var
  Glyph, PrevGlyph: TFTGlyph;
  TextLine: UCS4String;
  LineYOffset: single;
  LineIndex, CharIndex: integer;
  LineBounds: TBoundsDbl;
  KernDelta: FT_Vector;
  UnderlinePos: double;
begin
  // Reset global bounds
  Result.Left   := Infinity;
  Result.Right  := 0;
  Result.Bottom := Infinity;
  Result.Top    := 0;

  // reset last glyph
  PrevGlyph := nil;

  // display text
  for LineIndex := 0 to High(Text) do
  begin
    // get next text line
    TextLine := Text[LineIndex];
    LineYOffset := -LineSpacing * LineIndex;

    // reset line bounds
    LineBounds.Left   := Infinity;
    LineBounds.Right  := 0;
    LineBounds.Bottom := Infinity;
    LineBounds.Top    := 0;

    // for each glyph image, compute its bounding box
    for CharIndex := 0 to LengthUCS4(TextLine)-1 do
    begin
      Glyph := TFTGlyph(GetGlyph(TextLine[CharIndex]));
      if (Glyph <> nil) then
      begin
        // get kerning
        if (fUseKerning and FT_HAS_KERNING(fFace) and (PrevGlyph <> nil)) then
        begin
          FT_Get_Kerning(fFace, PrevGlyph.CharIndex, Glyph.CharIndex,
                         FT_KERNING_UNSCALED, KernDelta);
          LineBounds.Right := LineBounds.Right + KernDelta.x * fFontUnitScale.X;
        end;

        // update left bound (must be done before right bound is updated)
        if (LineBounds.Right + Glyph.Bounds.Left < LineBounds.Left) then
          LineBounds.Left := LineBounds.Right + Glyph.Bounds.Left;

        // update right bound
        if (CharIndex < LengthUCS4(TextLine)-1) or  // not the last character
           (TextLine[CharIndex] = Ord(' ')) or      // on space char (Bounds.Right = 0)
           Advance then                             // or in advance mode
        begin
          // add advance and glyph spacing
          LineBounds.Right := LineBounds.Right + Glyph.Advance.x + GlyphSpacing
        end
        else
        begin
          // add glyph's right bound
          LineBounds.Right := LineBounds.Right + Glyph.Bounds.Right;
        end;

        // update bottom and top bounds
        if (Glyph.Bounds.Bottom < LineBounds.Bottom) then
          LineBounds.Bottom := Glyph.Bounds.Bottom;
        if (Glyph.Bounds.Top > LineBounds.Top) then
          LineBounds.Top := Glyph.Bounds.Top;
      end;

      PrevGlyph := Glyph;
    end;

    // handle italic font style
    if (Italic in Style) then
    begin
      LineBounds.Left := LineBounds.Left + LineBounds.Bottom * cShearFactor;
      LineBounds.Right := LineBounds.Right + LineBounds.Top * cShearFactor;
    end;

    // handle underlined font style
    if (Underline in Style) then
    begin
      UnderlinePos := GetUnderlinePosition();
      if (UnderlinePos < LineBounds.Bottom) then
        LineBounds.Bottom := UnderlinePos;
    end;

    // add line offset
    LineBounds.Bottom := LineBounds.Bottom + LineYOffset;
    LineBounds.Top := LineBounds.Top + LineYOffset;

    // adjust global bounds
    if (Result.Left > LineBounds.Left) then
      Result.Left := LineBounds.Left;
    if (Result.Right < LineBounds.Right) then
      Result.Right := LineBounds.Right;
    if (Result.Bottom > LineBounds.Bottom) then
      Result.Bottom := LineBounds.Bottom;
    if (Result.Top < LineBounds.Top) then
      Result.Top := LineBounds.Top;
  end;

  // if left or bottom bound was not set, set them to 0
  if (IsInfinite(Result.Left)) then
    Result.Left := 0.0;
  if (IsInfinite(Result.Bottom)) then
    Result.Bottom := 0.0;
end;

procedure TFTFont.Render(const Text: UCS4String);
var
  CharIndex: integer;
  Glyph, PrevGlyph: TFTGlyph;
  KernDelta: FT_Vector;
begin
  // reset last glyph
  PrevGlyph := nil;

  // draw current line
  for CharIndex := 0 to LengthUCS4(Text)-1 do
  begin
    Glyph := TFTGlyph(GetGlyph(Text[CharIndex]));
    if (Assigned(Glyph)) then
    begin
      // get kerning
      if (fUseKerning and FT_HAS_KERNING(fFace) and (PrevGlyph <> nil)) then
      begin
        FT_Get_Kerning(fFace, PrevGlyph.CharIndex, Glyph.CharIndex,
                       FT_KERNING_UNSCALED, KernDelta);
        glTranslatef(KernDelta.x * fFontUnitScale.X, 0, 0);
      end;

      if (ReflectionPass) then
        Glyph.RenderReflection()
      else
        Glyph.Render(fUseDisplayLists);

      glTranslatef(Glyph.Advance.x + fGlyphSpacing, 0, 0);
    end;

    PrevGlyph := Glyph;
  end;
end;

function TFTFont.GetHeight(): single;
begin
  Result := Ascender - Descender;
end;

function TFTFont.GetAscender(): single;
begin
  Result := fFace.ascender * fFontUnitScale.Y + Outset*2;
end;

function TFTFont.GetDescender(): single;
begin
  // Note: outset is not part of the descender as the baseline is lifted
  Result := fFace.descender * fFontUnitScale.Y;
end;

function TFTFont.GetUnderlinePosition(): single;
begin
  Result := fFace.underline_position * fFontUnitScale.Y - Outset;
end;

function TFTFont.GetUnderlineThickness(): single;
begin
  Result := fFace.underline_thickness * fFontUnitScale.Y + Outset*2;
end;


{*
 * TFTScalableFont
 *}

constructor TFTScalableFont.Create(const Filename: IPath;
                   Size: integer; OutsetAmount: single;
                   UseMipmaps: boolean);
var
  LoadFlags: FT_Int32;
begin
  LoadFlags := FT_LOAD_DEFAULT;
  // Disable hinting and grid-fitting to preserve font outlines at each font
  // size, otherwise the font widths/heights do not match resulting in ugly
  // text size changes during zooming.
  // A drawback is a reduced quality with smaller font sizes but it is not that
  // bad with gray-scaled rendering (at least it looks better than OpenGL's
  // linear downscaling on minification).
  if (UseMipmaps) then
    LoadFlags := LoadFlags or FT_LOAD_NO_HINTING;
  inherited Create(
      TFTFont.Create(Filename, Size, Size * OutsetAmount, LoadFlags),
      UseMipmaps);
end;

function TFTScalableFont.CreateMipmap(Level: integer; Scale: single): TFont;
var
  ScaledSize: integer;
  BaseFont: TFTFont;
begin
  Result := nil;
  BaseFont := TFTFont(fBaseFont);
  ScaledSize := Round(BaseFont.Size * Scale);
  // do not create mipmap fonts < 8 pixels
  if (ScaledSize < 8) then
    Exit;
  Result := TFTFont.Create(BaseFont.fFilename,
      ScaledSize, BaseFont.fOutset * Scale,
      FT_LOAD_DEFAULT or FT_LOAD_NO_HINTING);
end;

function TFTScalableFont.GetOutset(): single;
begin
  Result := TFTFont(fBaseFont).Outset * fScale;
end;

procedure TFTScalableFont.FlushCache(KeepBaseSet: boolean);
var
  Level: integer;
begin
  for Level := 0 to High(fMipmapFonts) do
    if (fMipmapFonts[Level] <> nil) then
      TFTFont(fMipmapFonts[Level]).FlushCache(KeepBaseSet);
end;


{*
 * TOutlineFont
 *}

constructor TFTOutlineFont.Create(
    const Filename: IPath;
    Size: integer; Outset: single;
    LoadFlags: FT_Int32);
begin
  inherited Create();

  fFilename := Filename;
  fSize := Size;
  fOutset := Outset;

  fInnerFont := TFTFont.Create(Filename, Size, 0.0, LoadFlags);
  fInnerFont.fPart := fpInner;
  fOutlineFont := TFTFont.Create(Filename, Size, Outset, LoadFlags);
  fOutlineFont.fPart := fpOutline;

  ResetIntern();
end;

destructor TFTOutlineFont.Destroy;
begin
  fOutlineFont.Free;
  fInnerFont.Free;
  inherited;
end;

procedure TFTOutlineFont.ResetIntern();
begin
  // TODO: maybe swap fInnerFont/fOutlineFont.GlyphSpacing to use the spacing
  // of the outline font?
  //fInnerFont.GlyphSpacing := fOutset*2;
  fOutlineFont.GlyphSpacing := -fOutset*2;

  fLineSpacing := fOutlineFont.LineSpacing;
  fReflectionSpacing := fOutlineFont.ReflectionSpacing;
  fOutlineColor := NewGLColor(0, 0, 0, -1);
end;

procedure TFTOutlineFont.Reset();
begin
  inherited;
  fInnerFont.Reset();
  fOutlineFont.Reset();
  ResetIntern();
end;

procedure TFTOutlineFont.DrawUnderline(const Text: UCS4String);
var
  CurrentColor: TGLColor;
  OutlineColor: TGLColor;
begin
  // save current color
  glGetFloatv(GL_CURRENT_COLOR, @CurrentColor.vals);

  // if the outline's alpha component is < 0 use the current alpha
  OutlineColor := fOutlineColor;
  if (OutlineColor.a < 0) then
    OutlineColor.a := CurrentColor.a;

  // draw underline outline (in outline color)
  glColor4fv(@OutlineColor.vals);
  fOutlineFont.DrawUnderline(Text);
  glColor4fv(@CurrentColor.vals);

  // draw underline inner part (in current color)
  glPushMatrix();
  glTranslatef(fOutset, 0, 0);
  fInnerFont.DrawUnderline(Text);
  glPopMatrix();
end;

procedure TFTOutlineFont.Render(const Text: UCS4String);
var
  CurrentColor: TGLColor;
  OutlineColor: TGLColor;
begin
  // save current color
  glGetFloatv(GL_CURRENT_COLOR, @CurrentColor.vals);

  // if the outline's alpha component is < 0 use the current alpha
  OutlineColor := fOutlineColor;
  if (OutlineColor.a < 0) then
    OutlineColor.a := CurrentColor.a;

  { setup and render outline font }

  glColor4fv(@OutlineColor.vals);
  glPushMatrix();
  fOutlineFont.Render(Text);
  glPopMatrix();
  glColor4fv(@CurrentColor.vals);

  { setup and render inner font }

  glPushMatrix();
  glTranslatef(fOutset, fOutset, 0);
  fInnerFont.Render(Text);
  glPopMatrix();
end;

procedure TFTOutlineFont.SetOutlineColor(r, g, b: GLfloat; a: GLfloat);
begin
  fOutlineColor := NewGLColor(r, g, b, a);
end;

procedure TFTOutlineFont.FlushCache(KeepBaseSet: boolean);
begin
  fOutlineFont.FlushCache(KeepBaseSet);
  fInnerFont.FlushCache(KeepBaseSet);
end;

function TFTOutlineFont.BBox(const Text: TUCS4StringArray; Advance: boolean): TBoundsDbl;
begin
  Result := fOutlineFont.BBox(Text, Advance);
end;

function TFTOutlineFont.GetHeight(): single;
begin
  Result := fOutlineFont.Height;
end;

function TFTOutlineFont.GetAscender(): single;
begin
  Result := fOutlineFont.Ascender;
end;

function TFTOutlineFont.GetDescender(): single;
begin
  Result := fOutlineFont.Descender;
end;

procedure TFTOutlineFont.SetLineSpacing(Spacing: single);
begin
  inherited SetLineSpacing(Spacing);
  fInnerFont.LineSpacing := Spacing;
  fOutlineFont.LineSpacing := Spacing;
end;

procedure TFTOutlineFont.SetGlyphSpacing(Spacing: single);
begin
  inherited SetGlyphSpacing(Spacing);
  fInnerFont.GlyphSpacing := Spacing;
  fOutlineFont.GlyphSpacing := Spacing - Outset*2;
end;

procedure TFTOutlineFont.SetReflectionSpacing(Spacing: single);
begin
  inherited SetReflectionSpacing(Spacing);
  fInnerFont.ReflectionSpacing := Spacing;
  fOutlineFont.ReflectionSpacing := Spacing;
end;

procedure TFTOutlineFont.SetStyle(Style: TFontStyle);
begin
  inherited SetStyle(Style);
  fInnerFont.Style := Style;
  fOutlineFont.Style := Style;
end;

function TFTOutlineFont.GetStyle(): TFontStyle;
begin
  Result := inherited GetStyle();
end;

function TFTOutlineFont.GetUnderlinePosition(): single;
begin
  Result := fOutlineFont.GetUnderlinePosition();
end;

function TFTOutlineFont.GetUnderlineThickness(): single;
begin
  Result := fOutlineFont.GetUnderlinePosition();
end;

procedure TFTOutlineFont.SetUseKerning(Enable: boolean);
begin
  inherited SetUseKerning(Enable);
  fInnerFont.fUseKerning := Enable;
  fOutlineFont.fUseKerning := Enable;
end;

procedure TFTOutlineFont.SetReflectionPass(Enable: boolean);
begin
  inherited SetReflectionPass(Enable);
  fInnerFont.fReflectionPass := Enable;
  fOutlineFont.fReflectionPass := Enable;
end;

{**
 * TScalableOutlineFont
 *}

constructor TFTScalableOutlineFont.Create(
    const Filename: IPath;
    Size: integer; OutsetAmount: single;
    UseMipmaps: boolean);
var
  LoadFlags: FT_Int32;
begin
  LoadFlags := FT_LOAD_DEFAULT;
  // Disable hinting and grid-fitting (see TFTScalableFont.Create)
  if (UseMipmaps) then
    LoadFlags := LoadFlags or FT_LOAD_NO_HINTING;
  inherited Create(
      TFTOutlineFont.Create(Filename, Size, Size*OutsetAmount, LoadFlags),
      UseMipmaps);
end;

function TFTScalableOutlineFont.CreateMipmap(Level: integer; Scale: single): TFont;
var
  ScaledSize: integer;
  BaseFont: TFTOutlineFont;
begin
  Result := nil;
  BaseFont := TFTOutlineFont(fBaseFont);
  ScaledSize := Round(BaseFont.Size*Scale);
  // do not create mipmap fonts < 8 pixels
  if (ScaledSize < 8) then
    Exit;
  Result := TFTOutlineFont.Create(BaseFont.fFilename,
      ScaledSize, BaseFont.fOutset*Scale,
      FT_LOAD_DEFAULT or FT_LOAD_NO_HINTING);
end;

function TFTScalableOutlineFont.GetOutset(): single;
begin
  Result := TFTOutlineFont(fBaseFont).Outset * fScale;
end;

procedure TFTScalableOutlineFont.SetOutlineColor(r, g, b: GLfloat; a: GLfloat);
var
  Level: integer;
begin
  for Level := 0 to High(fMipmapFonts) do
    if (fMipmapFonts[Level] <> nil) then
      TFTOutlineFont(fMipmapFonts[Level]).SetOutlineColor(r, g, b, a);
end;

procedure TFTScalableOutlineFont.FlushCache(KeepBaseSet: boolean);
var
  Level: integer;
begin
  for Level := 0 to High(fMipmapFonts) do
    if (fMipmapFonts[Level] <> nil) then
      TFTOutlineFont(fMipmapFonts[Level]).FlushCache(KeepBaseSet);
end;


{*
 * TFTGlyph
 *}

const
  {**
   * Size of the transparent border surrounding the glyph image in the texture.
   * The border is necessary because OpenGL does not smooth texels at the
   * border of a texture with the GL_CLAMP or GL_CLAMP_TO_EDGE styles.
   * Without the border, magnified glyph textures look very ugly at their edges.
   * It looks edgy, as if some pixels are missing especially on the left edge
   * (just set cTexSmoothBorder to 0 to see what is meant by this).
   * With the border even the glyphs edges are blended to the border (transparent)
   * color and everything looks nice.
   *
   * Note:
   * OpenGL already supports texture border by setting the border parameter
   * of glTexImage*D() to 1 and using a texture size of 2^m+2b and setting the
   * border pixels to the border color. In some forums it is discouraged to use
   * the border parameter as only a few of the more modern graphics cards support
   * this feature. On an ATI Radeon 9700 card, the slowed down to 0.5 fps and
   * the glyph's background got black.  So instead of using this feature we
   * handle it on our own. The only drawback is that textures might get bigger
   * because the border might require a higher power of 2 size instead of just
   * two additional pixels.
   *}
  cTexSmoothBorder = 1;

procedure TFTGlyph.StrokeBorder(var Glyph: FT_Glyph);
var
  Outline: PFT_Outline;
  OuterStroker, InnerStroker:  FT_Stroker;
  OuterNumPoints, InnerNumPoints, GlyphNumPoints: FT_UInt;
  OuterNumContours, InnerNumContours, GlyphNumContours: FT_UInt;
  OuterBorder, InnerBorder: FT_StrokerBorder;
  OutlineFlags: FT_Int;
  UseStencil: boolean;
begin
  // It is possible to extrude the borders of a glyph with FT_Glyph_Stroke
  // but it will extrude the border to the outside and the inside of a glyph
  // although we just want to extrude to the outside.
  // FT_Glyph_StrokeBorder extrudes to the outside but also fills the interior
  // (this is what we need for bold fonts).
  // In both cases the inner font and outline font (border) will overlap.
  // Normally this does not matter but it does if alpha blending is active.
  // In this case if e.g. the inner color is set to white, the outline to red
  // and alpha to 0.5 the inner part will not be white it will be pink.

  InnerStroker := nil;
  OuterStroker := nil;

  // If we are to create the interior of an outlined font (fInner = true)
  // we have to create two borders:
  // - one extruded to the outside by fOutset pixels and
  // - one extruded to the inside by almost 0 zero pixels.
  // The second one is used as a stencil for the first one, clearing the
  // interiour of the glyph.
  // The stencil is not needed to create bold fonts.
  UseStencil := (fFont.fPart = fpInner);

  Outline := @FT_OutlineGlyph(Glyph).outline;

  OuterBorder := FT_Outline_GetOutsideBorder(Outline);
  if (OuterBorder = FT_STROKER_BORDER_LEFT) then
    InnerBorder := FT_STROKER_BORDER_RIGHT
  else
    InnerBorder := FT_STROKER_BORDER_LEFT;

  { extrude outer border }

  if (FT_Stroker_New(Glyph.library_, OuterStroker) <> 0) then
    raise Exception.Create('FT_Stroker_New failed!');
  FT_Stroker_Set(
      OuterStroker,
      Round(fOutset * 64),
      FT_STROKER_LINECAP_ROUND,
      FT_STROKER_LINEJOIN_BEVEL,
      0);

  // similar to FT_Glyph_StrokeBorder(inner = FT_FALSE) but it is possible to
  // use FT_Stroker_ExportBorder() afterwards to combine inner and outer borders
  if (FT_Stroker_ParseOutline(OuterStroker, Outline, FT_FALSE) <> 0) then
    raise Exception.Create('FT_Stroker_ParseOutline failed!');

  FT_Stroker_GetBorderCounts(OuterStroker, OuterBorder, OuterNumPoints, OuterNumContours);

  { extrude inner border (= stencil) }

  if (UseStencil) then
  begin
    if (FT_Stroker_New(Glyph.library_, InnerStroker) <> 0) then
      raise Exception.Create('FT_Stroker_New failed!');
    FT_Stroker_Set(
        InnerStroker,
        63, // extrude at most one pixel to avoid a black border
        FT_STROKER_LINECAP_ROUND,
        FT_STROKER_LINEJOIN_BEVEL,
        0);

    if (FT_Stroker_ParseOutline(InnerStroker, Outline, FT_FALSE) <> 0) then
      raise Exception.Create('FT_Stroker_ParseOutline failed!');

    FT_Stroker_GetBorderCounts(InnerStroker, InnerBorder, InnerNumPoints, InnerNumContours);
  end else begin
    InnerNumPoints := 0;
    InnerNumContours := 0;
  end;

  { combine borders (subtract: OuterBorder - InnerBorder) }

  GlyphNumPoints := InnerNumPoints + OuterNumPoints;
  GlyphNumContours := InnerNumContours + OuterNumContours;

  // save flags before deletion (TODO: set them on the resulting outline)
  OutlineFlags := Outline.flags;

  // resize glyph outline to hold inner and outer border
  FT_Outline_Done(Glyph.Library_, Outline);
  if (FT_Outline_New(Glyph.Library_, GlyphNumPoints, GlyphNumContours, Outline) <> 0) then
    raise Exception.Create('FT_Outline_New failed!');

  Outline.n_points := 0;
  Outline.n_contours := 0;

  // add points to outline. The inner-border is used as a stencil.
  FT_Stroker_ExportBorder(OuterStroker, OuterBorder, Outline);
  if (UseStencil) then
    FT_Stroker_ExportBorder(InnerStroker, InnerBorder, Outline);
  if (FT_Outline_Check(outline) <> 0) then
    raise Exception.Create('FT_Stroker_ExportBorder failed!');

  if (InnerStroker <> nil) then
    FT_Stroker_Done(InnerStroker);
  if (OuterStroker <> nil) then
    FT_Stroker_Done(OuterStroker);
end;

procedure TFTGlyph.CreateTexture(LoadFlags: FT_Int32);
var
  X, Y:          integer;
  Glyph:         FT_Glyph;
  BitmapGlyph:   FT_BitmapGlyph;
  Bitmap:        PFT_Bitmap;
  BitmapLine:    PByteArray;
  BitmapBuffer:  PByteArray;
  TexBuffer:     TGLubyteDynArray;
  TexLine:       PGLubyteArray;
  CBox:          FT_BBox;
begin
  // load the Glyph for our character
  if (FT_Load_Glyph(fFont.Face, fCharIndex, LoadFlags) <> 0) then
    raise Exception.Create('FT_Load_Glyph failed');

  // move the face's glyph into a Glyph object
  if (FT_Get_Glyph(fFont.Face^.glyph, Glyph) <> 0) then
    raise Exception.Create('FT_Get_Glyph failed');

  if (fOutset > 0) then
    StrokeBorder(Glyph);

  // store scaled advance width/height in glyph-object
  fAdvance.X := fFont.Face^.glyph^.advance.x / 64 + fOutset*2;
  fAdvance.Y := fFont.Face^.glyph^.advance.y / 64 + fOutset*2;

  // get the contour's bounding box (in 1/64th pixels, not font-units)
  FT_Glyph_Get_CBox(Glyph, FT_GLYPH_BBOX_UNSCALED, CBox);
  // convert 1/64th values to double values
  fBounds.Left   := CBox.xMin / 64;
  fBounds.Right  := CBox.xMax / 64 + fOutset*2;
  fBounds.Bottom := CBox.yMin / 64;
  fBounds.Top    := CBox.yMax / 64 + fOutset*2;

  // convert the glyph to a bitmap (and destroy original glyph image).
  // Request 8 bit gray level pixel mode. 
  FT_Glyph_To_Bitmap(Glyph, FT_RENDER_MODE_NORMAL, nil, 1);
  BitmapGlyph := FT_BitmapGlyph(Glyph);

  // get bitmap offsets
  fBitmapCoords.Left := BitmapGlyph^.left - cTexSmoothBorder;
  // Note: add 1*fOutset for lifting the baseline so outset fonts to not intersect
  // with the baseline; Ceil(fOutset) for the outset pixels added to the bitmap.
  fBitmapCoords.Top  := BitmapGlyph^.top + fOutset+Ceil(fOutset) + cTexSmoothBorder;

  // make accessing the bitmap easier
  Bitmap := @BitmapGlyph^.bitmap;
  // get bitmap dimensions
  fBitmapCoords.Width  := Bitmap.width + (Ceil(fOutset) + cTexSmoothBorder)*2;
  fBitmapCoords.Height := Bitmap.rows + (Ceil(fOutset) + cTexSmoothBorder)*2;

  // get power-of-2 bitmap widths
  fTexSize.Width  :=
      NextPowerOf2(Bitmap.width + (Ceil(fOutset) + cTexSmoothBorder)*2);
  fTexSize.Height :=
      NextPowerOf2(Bitmap.rows + (Ceil(fOutset) + cTexSmoothBorder)*2);

  // texture-widths ignoring empty (power-of-2) padding space
  fTexOffset.X := fBitmapCoords.Width / fTexSize.Width;
  fTexOffset.Y := fBitmapCoords.Height / fTexSize.Height;

  // allocate memory for texture data
  SetLength(TexBuffer, fTexSize.Width * fTexSize.Height);
  FillChar(TexBuffer[0], Length(TexBuffer), 0);

  // Freetype stores the bitmap with either upper (pitch is > 0) or lower
  // (pitch < 0) glyphs line first. Set the buffer to the upper line.
  // See http://freetype.sourceforge.net/freetype2/docs/glyphs/glyphs-7.html
  if (Bitmap.pitch > 0) then
    BitmapBuffer := @Bitmap.buffer[0]
  else
    BitmapBuffer := @Bitmap.buffer[(Bitmap.rows-1) * Abs(Bitmap.pitch)];

  // copy data to texture bitmap (upper line first).
  for Y := 0 to Bitmap.rows-1 do
  begin
    // set pointer to first pixel in line that holds bitmap data.
    // Each line starts with a cTexSmoothBorder pixel and multiple outset pixels
    // that are added by Extrude() later.
    TexLine := @TexBuffer[(Y + cTexSmoothBorder + Ceil(fOutset)) * fTexSize.Width +
                          cTexSmoothBorder + Ceil(fOutset)];
    // get next lower line offset, use pitch instead of width as it tells
    // us the storage direction of the lines. In addition a line might be padded.
    BitmapLine := @BitmapBuffer[Y * Bitmap.pitch];

    // check for pixel mode and copy pixels
    // Should be 8 bit gray, but even with FT_RENDER_MODE_NORMAL, freetype
    // sometimes (e.g. 16px sized japanese fonts) fallbacks to 1 bit pixels.
    case (Bitmap.pixel_mode) of
      FT_PIXEL_MODE_GRAY: begin  // 8 bit gray
        for X := 0 to Bitmap.width-1 do
          TexLine[X] := BitmapLine[X];
      end;
      FT_PIXEL_MODE_MONO: begin  // 1 bit mono
        for X := 0 to Bitmap.width-1 do
          TexLine[X] := High(GLubyte) * ((BitmapLine[X div 8] shr (7-(X mod 8))) and $1);
      end;
      else begin
        // unhandled pixel format
      end;
    end;
  end;

  // allocate resources for textures and display lists
  glGenTextures(1, @fTexture);

  // setup texture parameters
  glBindTexture(GL_TEXTURE_2D, fTexture);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  // create alpha-map (GL_ALPHA component only).
  // TexCoord (0,0) corresponds to the top left pixel of the glyph,
  // (1,1) to the bottom right pixel. So the glyph is flipped as OpenGL uses
  // a cartesian (y-axis up) coordinate system for textures.   
  // See the cTexSmoothBorder comment for info on texture borders.
  glTexImage2D(GL_TEXTURE_2D, 0, GL_ALPHA, fTexSize.Width, fTexSize.Height,
      0, GL_ALPHA, GL_UNSIGNED_BYTE, @TexBuffer[0]);

  // free expanded data
  SetLength(TexBuffer, 0);

  // create the display list
  fDisplayList := glGenLists(1);

  // render to display-list
  glNewList(fDisplayList, GL_COMPILE);
    Render(false);
  glEndList();

  // free glyph data (bitmap, etc.)
  FT_Done_Glyph(Glyph);
end;

constructor TFTGlyph.Create(Font: TFTFont; ch: UCS4Char; Outset: single;
    LoadFlags: FT_Int32);
begin
  inherited Create();

  fFont := Font;
  fOutset := Outset;
  fCharCode := ch;

  // get the Freetype char-index (use default UNICODE charmap)
  fCharIndex := FT_Get_Char_Index(Font.fFace, FT_ULONG(ch));

  CreateTexture(LoadFlags);
end;

destructor TFTGlyph.Destroy;
begin
  if (fDisplayList <> 0) then
    glDeleteLists(fDisplayList, 1);
  if (fTexture <> 0) then
    glDeleteTextures(1, @fTexture);
  inherited;
end;

procedure TFTGlyph.Render(UseDisplayLists: boolean);
begin
  // use display-lists if enabled and exit
  if (UseDisplayLists) then
  begin
    glCallList(fDisplayList);
    Exit;
  end;

  glBindTexture(GL_TEXTURE_2D, fTexture);
  glPushMatrix();

  // move to top left glyph position
  glTranslatef(fBitmapCoords.Left, fBitmapCoords.Top, 0);

  // draw glyph texture
  glBegin(GL_QUADS);
    // top right
    glTexCoord2f(fTexOffset.X, 0);
    glVertex2f(fBitmapCoords.Width, 0);

    // top left
    glTexCoord2f(0, 0);
    glVertex2f(0, 0);

    // bottom left
    glTexCoord2f(0, fTexOffset.Y);
    glVertex2f(0, -fBitmapCoords.Height);

    // bottom right
    glTexCoord2f(fTexOffset.X, fTexOffset.Y);
    glVertex2f(fBitmapCoords.Width, -fBitmapCoords.Height);
  glEnd();

  glPopMatrix();
end;

procedure TFTGlyph.RenderReflection();
var
  Color: TGLColor;
  TexUpperPos: single;
  TexLowerPos: single;
  UpperPos: single;
const
  CutOff = 0.6;
begin
  glPushMatrix();
  glBindTexture(GL_TEXTURE_2D, fTexture);
  glGetFloatv(GL_CURRENT_COLOR, @Color.vals);

  // add extra space to the left of the glyph
  glTranslatef(fBitmapCoords.Left, 0, 0);

  // The upper position of the glyph, if CutOff is 1.0, it is fFont.Ascender.
  // If CutOff is set to 0.5 only half of the glyph height is displayed.
  UpperPos := fFont.Descender + fFont.Height * CutOff;

  // the glyph texture's height is just the height of the glyph but not the font
  // height. Setting a color for the upper and lower bounds of the glyph results
  // in different color gradients. So we have to set the color values for the
  // descender and ascender (as we have a cutoff, for the upper-pos here) as
  // these positions are font but not glyph specific.

  // To get the texture positions we have to enhance the texture at the top and
  // bottom by the amount from the top to ascender (rather upper-pos here) and
  // from the bottom (Height-Top) to descender. Then we have to convert those
  // heights to texture coordinates by dividing by the bitmap Height and
  // removing the power-of-2 padding space by multiplying with fTexOffset.Y
  // (as fBitmapCoords.Height corresponds to fTexOffset.Y and not 1.0).
  TexUpperPos  := -(UpperPos - fBitmapCoords.Top) / fBitmapCoords.Height * fTexOffset.Y;
  TexLowerPos := (-(fFont.Descender + fBitmapCoords.Height - fBitmapCoords.Top) /
                    fBitmapCoords.Height + 1) * fTexOffset.Y;

  // draw glyph texture
  glBegin(GL_QUADS);
    // top right
    glColor4f(Color.r, Color.g, Color.b, 0);
    glTexCoord2f(fTexOffset.X, TexUpperPos);
    glVertex2f(fBitmapCoords.Width, UpperPos);

    // top left
    glTexCoord2f(0, TexUpperPos);
    glVertex2f(0, UpperPos);

    // bottom left
    glColor4f(Color.r, Color.g, Color.b, Color.a-0.3);
    glTexCoord2f(0, TexLowerPos);
    glVertex2f(0, fFont.Descender);

    // bottom right
    glTexCoord2f(fTexOffset.X, TexLowerPos);
    glVertex2f(fBitmapCoords.Width, fFont.Descender);
  glEnd();

  glPopMatrix();

  // restore old color
  // Note: glPopAttrib(GL_CURRENT_BIT)/glPopAttrib() is much slower then
  // glGetFloatv(GL_CURRENT_COLOR, ...)/glColor4fv(...)
  glColor4fv(@Color.vals);
end;

function TFTGlyph.GetAdvance(): TPositionDbl;
begin
  Result := fAdvance;
end;

function TFTGlyph.GetBounds(): TBoundsDbl;
begin
  Result := fBounds;
end;


{*
 * TGlyphCache
 *}

constructor TGlyphCache.Create();
begin
  inherited;
  fHash := TList.Create();
end;

destructor TGlyphCache.Destroy();
begin
  // free cached glyphs
  FlushCache(false);

  // destroy TList
  fHash.Free;
  
  inherited;
end;

function TGlyphCache.FindGlyphTable(BaseCode: cardinal; out InsertPos: integer): PGlyphTable;
var
  I: integer;
  Entry: TGlyphCacheHashEntry;
begin
  Result := nil;

  for I := 0 to fHash.Count-1 do
  begin
    Entry := TGlyphCacheHashEntry(fHash[I]);

    if (Entry.BaseCode > BaseCode) then
    begin
      InsertPos := I;
      Exit;
    end;

    if (Entry.BaseCode = BaseCode) then
    begin
      InsertPos := I;
      Result := @Entry.GlyphTable;
      Exit;
    end;
  end;

  InsertPos := fHash.Count;
end;

function TGlyphCache.AddGlyph(ch: UCS4Char; const Glyph: TGlyph): boolean;
var
  BaseCode:  cardinal;
  GlyphCode: integer;
  InsertPos: integer;
  GlyphTable: PGlyphTable;
  Entry: TGlyphCacheHashEntry;
begin
  Result := false;

  BaseCode := Ord(ch) shr 8;
  GlyphTable := FindGlyphTable(BaseCode, InsertPos);
  if (GlyphTable = nil) then
  begin
    Entry := TGlyphCacheHashEntry.Create(BaseCode);
    GlyphTable := @Entry.GlyphTable;
    fHash.Insert(InsertPos, Entry);
  end;

  // get glyph table offset
  GlyphCode := Ord(ch) and $FF;
  // insert glyph into table if not present
  if (GlyphTable[GlyphCode] = nil) then
  begin
    GlyphTable[GlyphCode] := Glyph;
    Result := true;
  end;
end;

procedure TGlyphCache.DeleteGlyph(ch: UCS4Char);
var
  Table: PGlyphTable;
  TableIndex, GlyphIndex: integer;
  TableEmpty: boolean;
begin
  // find table
  Table := FindGlyphTable(Ord(ch) shr 8, TableIndex);
  if (Table = nil) then
    Exit;

  // find glyph    
  GlyphIndex := Ord(ch) and $FF;
  if (Table[GlyphIndex] <> nil) then
  begin
    // destroy glyph
    FreeAndNil(Table[GlyphIndex]);

    // check if table is empty
    TableEmpty := true;
    for GlyphIndex := 0 to High(Table^) do
    begin
      if (Table[GlyphIndex] <> nil) then
      begin
        TableEmpty := false;
        Break;
      end;
    end;

    // free empty table
    if (TableEmpty) then
    begin
      fHash.Delete(TableIndex);
    end;
  end;
end;

function TGlyphCache.GetGlyph(ch: UCS4Char): TGlyph;
var
  InsertPos: integer;
  Table: PGlyphTable;
begin
  Table := FindGlyphTable(Ord(ch) shr 8, InsertPos);
  if (Table = nil) then
    Result := nil
  else
    Result := Table[Ord(ch) and $FF];
end;

function TGlyphCache.HasGlyph(ch: UCS4Char): boolean;
begin
  Result := (GetGlyph(ch) <> nil);
end;

procedure TGlyphCache.FlushCache(KeepBaseSet: boolean);
var
  EntryIndex, TableIndex: integer;
  Entry: TGlyphCacheHashEntry;
begin
  // destroy cached glyphs
  for EntryIndex := 0 to fHash.Count-1 do
  begin
    Entry := TGlyphCacheHashEntry(fHash[EntryIndex]);

    // the base set (0-255) has BaseCode 0 as the upper bytes are 0.
    if KeepBaseSet and (Entry.fBaseCode = 0) then
      Continue;

    for TableIndex := 0 to High(Entry.GlyphTable) do
    begin
      if (Entry.GlyphTable[TableIndex] <> nil) then
        FreeAndNil(Entry.GlyphTable[TableIndex]);
    end;
    FreeAndNil(Entry);
  end;
end;


{*
 * TGlyphCacheEntry
 *}

constructor TGlyphCacheHashEntry.Create(BaseCode: cardinal);
begin
  inherited Create();
  fBaseCode := BaseCode;
end;


{*
 * TFreeType
 *}

class function TFreeType.GetLibrary(): FT_Library;
begin
  if (LibraryInst = nil) then
  begin
    // initialize freetype
    if (FT_Init_FreeType(LibraryInst) <> 0) then
      raise Exception.Create('FT_Init_FreeType failed');
  end;
  Result := LibraryInst;
end;

class procedure TFreeType.FreeLibrary();
begin
  if (LibraryInst <> nil) then
    FT_Done_FreeType(LibraryInst);
  LibraryInst := nil;
end;


{$IFDEF BITMAP_FONT}
{*
 * TBitmapFont
 *}

constructor TBitmapFont.Create(const Filename: IPath; Outline: integer;
    Baseline, Ascender, Descender: integer);
begin
  inherited Create();

  fTex := Texture.LoadTexture(true, Filename, TEXTURE_TYPE_TRANSPARENT, 0);
  fTexSize := 1024;
  fOutline := Outline;
  fBaseline  := Baseline;
  fAscender  := Ascender;
  fDescender := Descender;

  LoadFontInfo(Filename.SetExtension('.dat'));

  ResetIntern();
end;

destructor TBitmapFont.Destroy();
begin
  glDeleteTextures(1, @fTex.TexNum);
  inherited;
end;

procedure TBitmapFont.ResetIntern();
begin
  fLineSpacing := Height;
end;

procedure TBitmapFont.Reset();
begin
  inherited;
  ResetIntern();
end;

procedure TBitmapFont.CorrectWidths(WidthMult: real; WidthAdd: integer);
var
  Count: integer;
begin
  for Count := 0 to 255 do
    fWidths[Count] := Round(fWidths[Count] * WidthMult) + WidthAdd;
end;

procedure TBitmapFont.LoadFontInfo(const InfoFile: IPath);
var
  Stream: TStream;
begin
  FillChar(fWidths[0], Length(fWidths), 0);

  Stream := nil;
  try
    Stream := TBinaryFileStream.Create(InfoFile, fmOpenRead);
    Stream.Read(fWidths, 256);
  except
    raise Exception.Create('Could not read font info file ''' +  InfoFile.ToNative + '''');
  end;
  Stream.Free;
end;

function TBitmapFont.BBox(const Text: TUCS4StringArray; Advance: boolean): TBoundsDbl;
var
  LineIndex, CharIndex: integer;
  CharCode: cardinal;
  Line: UCS4String;
  LineWidth: double;
begin
  Result.Left := 0;
  Result.Right := 0;
  Result.Top := Height;
  Result.Bottom := 0;

  for LineIndex := 0 to High(Text) do
  begin
    Line := Text[LineIndex];
    LineWidth := 0;
    for CharIndex := 0 to LengthUCS4(Line)-1 do
    begin
      CharCode := Ord(Line[CharIndex]);
      if (CharCode < Length(fWidths)) then
        LineWidth := LineWidth + fWidths[CharCode];
    end;
    if (LineWidth > Result.Right) then
      Result.Right := LineWidth;
  end;
end;

procedure TBitmapFont.RenderChar(ch: UCS4Char; var AdvanceX: real);
var
  TexX, TexY:        real;
  TexR, TexB:        real;
  GlyphWidth:        real;
  PL, PT:            real;
  PR, PB:            real;
  CharCode:          cardinal;
begin
  CharCode := Ord(ch);
  if (CharCode > High(fWidths)) then
    CharCode := 0;

  GlyphWidth := fWidths[CharCode];

  // set texture positions
  TexX := (CharCode mod 16) * 1/16 + 1/32 - (GlyphWidth/2 - fOutline)/fTexSize;
  TexY := (CharCode div 16) * 1/16 + {2 texels} 2/fTexSize;
  TexR := (CharCode mod 16) * 1/16 + 1/32 + (GlyphWidth/2 + fOutline)/fTexSize;
  TexB := (1 + CharCode div 16) * 1/16 - {2 texels} 2/fTexSize;

  // set vector positions
  PL := AdvanceX - fOutline;
  PR := PL + GlyphWidth + fOutline*2;
  PB := -fBaseline;
  PT := PB + fTexSize div 16;

  (*
  if (Font.Blend) then
  begin
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  end;
  *)

  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, fTex.TexNum);
  
  if (not ReflectionPass) then
  begin
    glBegin(GL_QUADS);
      glTexCoord2f(TexX, TexY); glVertex2f(PL, PT);
      glTexCoord2f(TexX, TexB); glVertex2f(PL, PB);
      glTexCoord2f(TexR, TexB); glVertex2f(PR, PB);
      glTexCoord2f(TexR, TexY); glVertex2f(PR, PT);
    glEnd;
  end
  else
  begin
    glDepthRange(0, 10);
    glDepthFunc(GL_LEQUAL);
    glEnable(GL_DEPTH_TEST);

    glBegin(GL_QUADS);
      glTexCoord2f(TexX, TexY); glVertex2f(PL, PT);
      glTexCoord2f(TexX, TexB); glVertex2f(PL, PB);
      glTexCoord2f(TexR, TexB); glVertex2f(PR, PB);
      glTexCoord2f(TexR, TexY); glVertex2f(PR, PT);
    glEnd;

    glBegin(GL_QUADS);
      glTexCoord2f(TexX, TexY); glVertex2f(PL, PT);
      glTexCoord2f(TexX, TexB); glVertex2f(PL, PB);
      glTexCoord2f(TexR, TexB); glVertex2f(PR, PB);
      glTexCoord2f(TexR, TexY); glVertex2f(PR, PT);

(*
      glColor4f(fTempColor.r, fTempColor.g, fTempColor.b, 0.7);
      glTexCoord2f(TexX, TexB); glVertex3f(PL, PB, 0);
      glTexCoord2f(TexR, TexB); glVertex3f(PR, PB, 0);

      glColor4f(fTempColor.r, fTempColor.g, fTempColor.b, 0);
      glTexCoord2f(TexR, (TexY + TexB)/2); glVertex3f(PR, (PT + PB)/2, 0);
      glTexCoord2f(TexX, (TexY + TexB)/2); glVertex3f(PL, (PT + PB)/2, 0);
*)
    glEnd;

    //write the colour back
    glColor4fv(@fTempColor);

    glDisable(GL_DEPTH_TEST);
  end; // reflection

  glDisable(GL_TEXTURE_2D);
  (*
  if (Font.Blend) then
    glDisable(GL_BLEND);
  *)

  AdvanceX := AdvanceX + GlyphWidth;
end;

procedure TBitmapFont.Render(const Text: UCS4String);
var
  CharIndex: integer;
  AdvanceX: real;
begin
  // if there is no text do nothing
  if (Text = nil) or (Text[0] = 0) then
    Exit;

  //Save the current color and alpha (for reflection)
  glGetFloatv(GL_CURRENT_COLOR, @fTempColor);

  AdvanceX := 0;
  for CharIndex := 0 to LengthUCS4(Text)-1 do
  begin
    RenderChar(Text[CharIndex], AdvanceX);
  end;
end;

function TBitmapFont.GetHeight(): single;
begin
  Result := fAscender - fDescender;
end;

function TBitmapFont.GetAscender(): single;
begin
  Result := fAscender;
end;

function TBitmapFont.GetDescender(): single;
begin
  Result := fDescender;
end;

function TBitmapFont.GetUnderlinePosition(): single;
begin
  Result := -2.0;
end;

function TBitmapFont.GetUnderlineThickness(): single;
begin
  Result := 1.0;
end;

{$ENDIF BITMAP_FONT}


initialization

finalization
  TFreeType.FreeLibrary();

end.
