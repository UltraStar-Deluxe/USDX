//----------------------------------------------------------------------------
// FreeType2 pascal header
//----------------------------------------------------------------------------
// Anti-Grain Geometry - Version 2.4 (Public License)
// Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)
//
// Anti-Grain Geometry - Version 2.4 Release Milano 3 (AggPas 2.4 RM3)
// Pascal Port By: Milan Marusinec alias Milano
//                 milan@marusinec.sk
//                 http://www.aggpas.org
// Copyright (c) 2005-2007
//
// Permission to copy, use, modify, sell and distribute this software
// is granted provided this copyright notice appears in all copies.
// This software is provided "as is" without express or implied
// warranty, and with no claim as to its suitability for any purpose.
//
//----------------------------------------------------------------------------
// Adapted by the UltraStar Deluxe Team
//----------------------------------------------------------------------------

unit freetype;

interface

{$IFDEF FPC}
  {$MODE DELPHI }
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

uses
  ctypes;

const
{$IF Defined(MSWINDOWS)}
  ft_lib = 'libfreetype-6.dll';
{$ELSEIF Defined(DARWIN)}
  ft_lib = 'libfreetype.dylib';
  {$LINKLIB libfreetype}
{$ELSEIF Defined(UNIX)}
  ft_lib = 'freetype.so';
{$IFEND}

type
  FT_Byte    = cuchar;
  FT_Short   = csshort;
  FT_UShort  = cushort;
  FT_Int     = csint;
  FT_UInt    = cuint;
  FT_Int16   = cint16;
  FT_UInt16  = cuint16;
  FT_Int32   = cint32;
  FT_UInt32  = cuint32;
  FT_Long    = cslong;
  FT_ULong   = culong;

  FT_Fixed   = cslong;
  FT_Pos     = cslong;
  FT_Error   = cint;
  FT_F26Dot6 = cslong;
  FT_String  = cchar;
  FT_Bool    = cuchar;

  PFT_Byte   = ^FT_Byte;
  PFT_Short  = ^FT_Short;
  PFT_String = ^FT_String;


  TByteArray = array [0 .. (MaxInt div SizeOf(byte))-1] of byte;
  PByteArray = ^TByteArray;


  (*************************************************************************)
  (*                                                                       *)
  (* <Enum>                                                                *)
  (*    FT_FACE_FLAG_XXX                                                   *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A list of bit flags used in the `face_flags' field of the          *)
  (*    @FT_FaceRec structure.  They inform client applications of         *)
  (*    properties of the corresponding face.                              *)
  (*                                                                       *)
  (* <Values>                                                              *)
  (*    FT_FACE_FLAG_SCALABLE ::                                           *)
  (*      Indicates that the face provides vectorial outlines.  This       *)
  (*      doesn't prevent embedded bitmaps, i.e., a face can have both     *)
  (*      this bit and @FT_FACE_FLAG_FIXED_SIZES set.                      *)
  (*                                                                       *)
  (*    FT_FACE_FLAG_FIXED_SIZES ::                                        *)
  (*      Indicates that the face contains `fixed sizes', i.e., bitmap     *)
  (*      strikes for some given pixel sizes.  See the `num_fixed_sizes'   *)
  (*      and `available_sizes' fields of @FT_FaceRec.                     *)
  (*                                                                       *)
  (*    FT_FACE_FLAG_FIXED_WIDTH ::                                        *)
  (*      Indicates that the face contains fixed-width characters (like    *)
  (*      Courier, Lucido, MonoType, etc.).                                *)
  (*                                                                       *)
  (*    FT_FACE_FLAG_SFNT ::                                               *)
  (*      Indicates that the face uses the `sfnt' storage scheme.  For     *)
  (*      now, this means TrueType and OpenType.                           *)
  (*                                                                       *)
  (*    FT_FACE_FLAG_HORIZONTAL ::                                         *)
  (*      Indicates that the face contains horizontal glyph metrics.  This *)
  (*      should be set for all common formats.                            *)
  (*                                                                       *)
  (*    FT_FACE_FLAG_VERTICAL ::                                           *)
  (*      Indicates that the face contains vertical glyph metrics.  This   *)
  (*      is only available in some formats, not all of them.              *)
  (*                                                                       *)
  (*    FT_FACE_FLAG_KERNING ::                                            *)
  (*      Indicates that the face contains kerning information.  If set,   *)
  (*      the kerning distance can be retrieved through the function       *)
  (*      @FT_Get_Kerning.  Note that if unset, this function will always  *)
  (*      return the vector (0,0).                                         *)
  (*                                                                       *)
  (*    FT_FACE_FLAG_FAST_GLYPHS ::                                        *)
  (*      THIS FLAG IS DEPRECATED.  DO NOT USE OR TEST IT.                 *)
  (*                                                                       *)
  (*    FT_FACE_FLAG_MULTIPLE_MASTERS ::                                   *)
  (*      Indicates that the font contains multiple masters and is capable *)
  (*      of interpolating between them.  See the multiple-masters         *)
  (*      specific API for details.                                        *)
  (*                                                                       *)
  (*    FT_FACE_FLAG_GLYPH_NAMES ::                                        *)
  (*      Indicates that the font contains glyph names that can be         *)
  (*      retrieved through @FT_Get_Glyph_Name.  Note that some TrueType   *)
  (*      fonts contain broken glyph name tables.  Use the function        *)
  (*      @FT_Has_PS_Glyph_Names when needed.                              *)
  (*                                                                       *)
  (*    FT_FACE_FLAG_EXTERNAL_STREAM ::                                    *)
  (*      Used internally by FreeType to indicate that a face's stream was *)
  (*      provided by the client application and should not be destroyed   *)
  (*      when @FT_Done_Face is called.  Don't read or test this flag.     *)
  (*                                                                       *)
const
  FT_FACE_FLAG_SCALABLE = 1 shl 0;
  FT_FACE_FLAG_KERNING  = 1 shl 6;

  (*************************************************************************)
  (*                                                                       *)
  (* <Enum>                                                                *)
  (*    FT_Encoding                                                        *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    An enumeration used to specify encodings supported by charmaps.    *)
  (*    Used in the @FT_Select_Charmap API function.                       *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    Because of 32-bit charcodes defined in Unicode (i.e., surrogates), *)
  (*    all character codes must be expressed as FT_Longs.                 *)
  (*                                                                       *)
  (*    The values of this type correspond to specific character           *)
  (*    repertories (i.e. charsets), and not to text encoding methods      *)
  (*    (like UTF-8, UTF-16, GB2312_EUC, etc.).                            *)
  (*                                                                       *)
  (*    Other encodings might be defined in the future.                    *)
  (*                                                                       *)
  (* <Values>                                                              *)
  (*   FT_ENCODING_NONE ::                                                 *)
  (*     The encoding value 0 is reserved.                                 *)
  (*                                                                       *)
  (*   FT_ENCODING_UNICODE ::                                              *)
  (*     Corresponds to the Unicode character set.  This value covers      *)
  (*     all versions of the Unicode repertoire, including ASCII and       *)
  (*     Latin-1.  Most fonts include a Unicode charmap, but not all       *)
  (*     of them.                                                          *)
  (*                                                                       *)
  (*   FT_ENCODING_MS_SYMBOL ::                                            *)
  (*     Corresponds to the Microsoft Symbol encoding, used to encode      *)
  (*     mathematical symbols in the 32..255 character code range.  For    *)
  (*     more information, see `http://www.ceviz.net/symbol.htm'.          *)
  (*                                                                       *)
  (*   FT_ENCODING_SJIS ::                                                 *)
  (*     Corresponds to Japanese SJIS encoding.  More info at              *)
  (*     at `http://langsupport.japanreference.com/encoding.shtml'.        *)
  (*     See note on multi-byte encodings below.                           *)
  (*                                                                       *)
  (*   FT_ENCODING_GB2312 ::                                               *)
  (*     Corresponds to an encoding system for Simplified Chinese as used  *)
  (*     used in mainland China.                                           *)
  (*                                                                       *)
  (*   FT_ENCODING_BIG5 ::                                                 *)
  (*     Corresponds to an encoding system for Traditional Chinese as used *)
  (*     in Taiwan and Hong Kong.                                          *)
  (*                                                                       *)
  (*   FT_ENCODING_WANSUNG ::                                              *)
  (*     Corresponds to the Korean encoding system known as Wansung.       *)
  (*     For more information see                                          *)
  (*     `http://www.microsoft.com/typography/unicode/949.txt'.            *)
  (*                                                                       *)
  (*   FT_ENCODING_JOHAB ::                                                *)
  (*     The Korean standard character set (KS C-5601-1992), which         *)
  (*     corresponds to MS Windows code page 1361.  This character set     *)
  (*     includes all possible Hangeul character combinations.             *)
  (*                                                                       *)
  (*   FT_ENCODING_ADOBE_LATIN_1 ::                                        *)
  (*     Corresponds to a Latin-1 encoding as defined in a Type 1          *)
  (*     Postscript font.  It is limited to 256 character codes.           *)
  (*                                                                       *)
  (*   FT_ENCODING_ADOBE_STANDARD ::                                       *)
  (*     Corresponds to the Adobe Standard encoding, as found in Type 1,   *)
  (*     CFF, and OpenType/CFF fonts.  It is limited to 256 character      *)
  (*     codes.                                                            *)
  (*                                                                       *)
  (*   FT_ENCODING_ADOBE_EXPERT ::                                         *)
  (*     Corresponds to the Adobe Expert encoding, as found in Type 1,     *)
  (*     CFF, and OpenType/CFF fonts.  It is limited to 256 character      *)
  (*     codes.                                                            *)
  (*                                                                       *)
  (*   FT_ENCODING_ADOBE_CUSTOM ::                                         *)
  (*     Corresponds to a custom encoding, as found in Type 1, CFF, and    *)
  (*     OpenType/CFF fonts.  It is limited to 256 character codes.        *)
  (*                                                                       *)
  (*   FT_ENCODING_APPLE_ROMAN ::                                          *)
  (*     Corresponds to the 8-bit Apple roman encoding.  Many TrueType and *)
  (*     OpenType fonts contain a charmap for this encoding, since older   *)
  (*     versions of Mac OS are able to use it.                            *)
  (*                                                                       *)
  (*   FT_ENCODING_OLD_LATIN_2 ::                                          *)
  (*     This value is deprecated and was never used nor reported by       *)
  (*     FreeType.  Don't use or test for it.                              *)
  (*                                                                       *)
  (*   FT_ENCODING_MS_SJIS ::                                              *)
  (*     Same as FT_ENCODING_SJIS.  Deprecated.                            *)
  (*                                                                       *)
  (*   FT_ENCODING_MS_GB2312 ::                                            *)
  (*     Same as FT_ENCODING_GB2312.  Deprecated.                          *)
  (*                                                                       *)
  (*   FT_ENCODING_MS_BIG5 ::                                              *)
  (*     Same as FT_ENCODING_BIG5.  Deprecated.                            *)
  (*                                                                       *)
  (*   FT_ENCODING_MS_WANSUNG ::                                           *)
  (*     Same as FT_ENCODING_WANSUNG.  Deprecated.                         *)
  (*                                                                       *)
  (*   FT_ENCODING_MS_JOHAB ::                                             *)
  (*     Same as FT_ENCODING_JOHAB.  Deprecated.                           *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*   By default, FreeType automatically synthetizes a Unicode charmap    *)
  (*   for Postscript fonts, using their glyph names dictionaries.         *)
  (*   However, it will also report the encodings defined explicitly in    *)
  (*   the font file, for the cases when they are needed, with the Adobe   *)
  (*   values as well.                                                     *)
  (*                                                                       *)
  (*   FT_ENCODING_NONE is set by the BDF and PCF drivers if the charmap   *)
  (*   is neither Unicode nor ISO-8859-1 (otherwise it is set to           *)
  (*   FT_ENCODING_UNICODE).  Use `FT_Get_BDF_Charset_ID' to find out      *)
  (*   which encoding is really present.  If, for example, the             *)
  (*   `cs_registry' field is `KOI8' and the `cs_encoding' field is `R',   *)
  (*   the font is encoded in KOI8-R.                                      *)
  (*                                                                       *)
  (*   FT_ENCODING_NONE is always set (with a single exception) by the     *)
  (*   winfonts driver.  Use `FT_Get_WinFNT_Header' and examine the        *)
  (*   `charset' field of the `FT_WinFNT_HeaderRec' structure to find out  *)
  (*   which encoding is really present.  For example, FT_WinFNT_ID_CP1251 *)
  (*   (204) means Windows code page 1251 (for Russian).                   *)
  (*                                                                       *)
  (*   FT_ENCODING_NONE is set if `platform_id' is `TT_PLATFORM_MACINTOSH' *)
  (*   and `encoding_id' is not `TT_MAC_ID_ROMAN' (otherwise it is set to  *)
  (*   FT_ENCODING_APPLE_ROMAN).                                           *)
  (*                                                                       *)
  (*   If `platform_id' is `TT_PLATFORM_MACINTOSH', use the function       *)
  (*   `FT_Get_CMap_Language_ID' to query the Mac language ID which may be *)
  (*   needed to be able to distinguish Apple encoding variants.  See      *)
  (*                                                                       *)
  (*     http://www.unicode.org/Public/MAPPINGS/VENDORS/APPLE/README.TXT   *)
  (*                                                                       *)
  (*   to get an idea how to do that.  Basically, if the language ID is 0, *)
  (*   dont use it, otherwise subtract 1 from the language ID.  Then       *)
  (*   examine `encoding_id'.  If, for example, `encoding_id' is           *)
  (*   `TT_MAC_ID_ROMAN' and the language ID (minus 1) is                  *)
  (*   `TT_MAC_LANGID_GREEK', it is the Greek encoding, not Roman.         *)
  (*   `TT_MAC_ID_ARABIC' with `TT_MAC_LANGID_FARSI' means the Farsi       *)
  (*   variant the Arabic encoding.                                        *)
  (*                                                                       *)
type
  PFT_Encoding = ^FT_Encoding;
  FT_Encoding = array[0..3] of char;
const
  FT_ENCODING_NONE:      FT_Encoding = (#0 ,#0 ,#0 ,#0 );
  FT_ENCODING_MS_SYMBOL: FT_Encoding = ('s', 'y', 'm', 'b' );
  FT_ENCODING_UNICODE:   FT_Encoding = ('u', 'n', 'i', 'c' );

  FT_ENCODING_SJIS:      FT_Encoding = ('s', 'j', 'i', 's');
  FT_ENCODING_GB2312:    FT_Encoding = ('g', 'b', ' ', ' ');
  FT_ENCODING_BIG5:      FT_Encoding = ('b', 'i', 'g', '5');
  FT_ENCODING_WANSUNG:   FT_Encoding = ('w', 'a', 'n', 's');
  FT_ENCODING_JOHAB:     FT_Encoding = ('j', 'o', 'h', 'a');

  (*************************************************************************)
  (*                                                                       *)
  (* <Enum>                                                                *)
  (*    FT_Glyph_Format                                                    *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    An enumeration type used to describe the format of a given glyph   *)
  (*    image.  Note that this version of FreeType only supports two image *)
  (*    formats, even though future font drivers will be able to register  *)
  (*    their own format.                                                  *)
  (*                                                                       *)
  (* <Values>                                                              *)
  (*    FT_GLYPH_FORMAT_NONE ::                                            *)
  (*      The value 0 is reserved and does describe a glyph format.        *)
  (*                                                                       *)
  (*    FT_GLYPH_FORMAT_COMPOSITE ::                                       *)
  (*      The glyph image is a composite of several other images.  This    *)
  (*      format is _only_ used with @FT_LOAD_NO_RECURSE, and is used to   *)
  (*      report compound glyphs (like accented characters).               *)
  (*                                                                       *)
  (*    FT_GLYPH_FORMAT_BITMAP ::                                          *)
  (*      The glyph image is a bitmap, and can be described as an          *)
  (*      @FT_Bitmap.  You generally need to access the `bitmap' field of  *)
  (*      the @FT_GlyphSlotRec structure to read it.                       *)
  (*                                                                       *)
  (*    FT_GLYPH_FORMAT_OUTLINE ::                                         *)
  (*      The glyph image is a vertorial outline made of line segments     *)
  (*      and Bezier arcs; it can be described as an @FT_Outline; you      *)
  (*      generally want to access the `outline' field of the              *)
  (*      @FT_GlyphSlotRec structure to read it.                           *)
  (*                                                                       *)
  (*    FT_GLYPH_FORMAT_PLOTTER ::                                         *)
  (*      The glyph image is a vectorial path with no inside/outside       *)
  (*      contours.  Some Type 1 fonts, like those in the Hershey family,  *)
  (*      contain glyphs in this format.  These are described as           *)
  (*      @FT_Outline, but FreeType isn't currently capable of rendering   *)
  (*      them correctly.                                                  *)
  (*                                                                       *)
type
  FT_Glyph_Format = array[0..3] of char;
const
  FT_GLYPH_FORMAT_NONE:      FT_Glyph_Format = (#0, #0, #0, #0 );

  FT_GLYPH_FORMAT_COMPOSITE: FT_Glyph_Format = ('c', 'o', 'm', 'p' );
  FT_GLYPH_FORMAT_BITMAP:    FT_Glyph_Format = ('b', 'i', 't', 's' );
  FT_GLYPH_FORMAT_OUTLINE:   FT_Glyph_Format = ('o', 'u', 't', 'l' );
  FT_GLYPH_FORMAT_PLOTTER:   FT_Glyph_Format = ('p', 'l', 'o', 't' );


  (*************************************************************************)
  (*                                                                       *)
  (* <Constant>                                                            *)
  (*    FT_STYLE_FLAG_XXX                                                  *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A list of bit-flags used to indicate the style of a given face.    *)
  (*    These are used in the `style_flags' field of @FT_FaceRec.          *)
  (*                                                                       *)
  (* <Values>                                                              *)
  (*    FT_STYLE_FLAG_ITALIC ::                                            *)
  (*      Indicates that a given face is italicized.                       *)
  (*                                                                       *)
  (*    FT_STYLE_FLAG_BOLD ::                                              *)
  (*      Indicates that a given face is bold.                             *)
  (*                                                                       *)
const
  FT_STYLE_FLAG_ITALIC = 1 shl 0;
  FT_STYLE_FLAG_BOLD   = 1 shl 1;
  

 (***************************************************************************
  *
  * @enum:
  *   FT_LOAD_XXX
  *
  * @description:
  *   A list of bit-field constants, used with @FT_Load_Glyph to indicate
  *   what kind of operations to perform during glyph loading.
  *
  * @values:
  *   FT_LOAD_DEFAULT ::
  *     Corresponding to 0, this value is used a default glyph load.  In this
  *     case, the following will happen:
  *
  *     1. FreeType looks for a bitmap for the glyph corresponding to the
  *        face's current size.  If one is found, the function returns.  The
  *        bitmap data can be accessed from the glyph slot (see note below).
  *
  *     2. If no embedded bitmap is searched or found, FreeType looks for a
  *        scalable outline.  If one is found, it is loaded from the font
  *        file, scaled to device pixels, then "hinted" to the pixel grid in
  *        order to optimize it.  The outline data can be accessed from the
  *        glyph slot (see note below).
  *
  *     Note that by default, the glyph loader doesn't render outlines into
  *     bitmaps.  The following flags are used to modify this default
  *     behaviour to more specific and useful cases.
  *
  *   FT_LOAD_NO_SCALE ::
  *     Don't scale the vector outline being loaded to 26.6 fractional
  *     pixels, but kept in font units.  Note that this also disables
  *     hinting and the loading of embedded bitmaps.  You should only use it
  *     when you want to retrieve the original glyph outlines in font units.
  *
  *   FT_LOAD_NO_HINTING ::
  *     Don't hint glyph outlines after their scaling to device pixels.
  *     This generally generates "blurrier" glyphs in anti-aliased modes.
  *
  *     This flag is ignored if @FT_LOAD_NO_SCALE is set.
  *
  *   FT_LOAD_RENDER ::
  *     Render the glyph outline immediately into a bitmap before the glyph
  *     loader returns.  By default, the glyph is rendered for the
  *     @FT_RENDER_MODE_NORMAL mode, which corresponds to 8-bit anti-aliased
  *     bitmaps using 256 opacity levels.  You can use either
  *     @FT_LOAD_TARGET_MONO or @FT_LOAD_MONOCHROME to render 1-bit
  *     monochrome bitmaps.
  *
  *     This flag is ignored if @FT_LOAD_NO_SCALE is set.
  *
  *   FT_LOAD_NO_BITMAP ::
  *     Don't look for bitmaps when loading the glyph.  Only scalable
  *     outlines will be loaded when available, and scaled, hinted, or
  *     rendered depending on other bit flags.
  *
  *     This does not prevent you from rendering outlines to bitmaps
  *     with @FT_LOAD_RENDER, however.
  *
  *   FT_LOAD_VERTICAL_LAYOUT ::
  *     Prepare the glyph image for vertical text layout.  This basically
  *     means that `face.glyph.advance' will correspond to the vertical
  *     advance height (instead of the default horizontal advance width),
  *     and that the glyph image will be translated to match the vertical
  *     bearings positions.
  *
  *   FT_LOAD_FORCE_AUTOHINT ::
  *     Force the use of the FreeType auto-hinter when a glyph outline is
  *     loaded.  You shouldn't need this in a typical application, since it
  *     is mostly used to experiment with its algorithm.
  *
  *   FT_LOAD_CROP_BITMAP ::
  *     Indicates that the glyph loader should try to crop the bitmap (i.e.,
  *     remove all space around its black bits) when loading it.  This is
  *     only useful when loading embedded bitmaps in certain fonts, since
  *     bitmaps rendered with @FT_LOAD_RENDER are always cropped by default.
  *
  *   FT_LOAD_PEDANTIC ::
  *     Indicates that the glyph loader should perform pedantic
  *     verifications during glyph loading, rejecting invalid fonts.  This
  *     is mostly used to detect broken glyphs in fonts.  By default,
  *     FreeType tries to handle broken fonts also.
  *
  *   FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH ::
  *     Indicates that the glyph loader should ignore the global advance
  *     width defined in the font.  As far as we know, this is only used by
  *     the X-TrueType font server, in order to deal correctly with the
  *     incorrect metrics contained in DynaLab's TrueType CJK fonts.
  *
  *   FT_LOAD_NO_RECURSE ::
  *     This flag is only used internally.  It merely indicates that the
  *     glyph loader should not load composite glyphs recursively.  Instead,
  *     it should set the `num_subglyph' and `subglyphs' values of the glyph
  *     slot accordingly, and set "glyph->format" to
  *     @FT_GLYPH_FORMAT_COMPOSITE.
  *
  *     The description of sub-glyphs is not available to client
  *     applications for now.
  *
  *   FT_LOAD_IGNORE_TRANSFORM ::
  *     Indicates that the glyph loader should not try to transform the
  *     loaded glyph image.  This doesn't prevent scaling, hinting, or
  *     rendering.
  *
  *   FT_LOAD_MONOCHROME ::
  *     This flag is used with @FT_LOAD_RENDER to indicate that you want
  *     to render a 1-bit monochrome glyph bitmap from a vectorial outline.
  *
  *     Note that this has no effect on the hinting algorithm used by the
  *     glyph loader.  You should better use @FT_LOAD_TARGET_MONO if you
  *     want to render monochrome-optimized glyph images instead.
  *
  *   FT_LOAD_LINEAR_DESIGN ::
  *     Return the linearly scaled metrics expressed in original font units
  *     instead of the default 16.16 pixel values.
  *
  *   FT_LOAD_NO_AUTOHINT ::
  *     Indicates that the auto-hinter should never be used to hint glyph
  *     outlines.  This doesn't prevent native format-specific hinters from
  *     being used.  This can be important for certain fonts where unhinted
  *     output is better than auto-hinted one.
  *
  *   FT_LOAD_TARGET_NORMAL ::
  *     Use hinting for @FT_RENDER_MODE_NORMAL.
  *
  *   FT_LOAD_TARGET_LIGHT ::
  *     Use hinting for @FT_RENDER_MODE_LIGHT.
  *
  *   FT_LOAD_TARGET_MONO ::
  *     Use hinting for @FT_RENDER_MODE_MONO.
  *
  *   FT_LOAD_TARGET_LCD ::
  *     Use hinting for @FT_RENDER_MODE_LCD.
  *
  *   FT_LOAD_TARGET_LCD_V ::
  *     Use hinting for @FT_RENDER_MODE_LCD_V.
  *)
const
  FT_LOAD_DEFAULT          = $0000;
  FT_LOAD_NO_SCALE         = $0001;
  FT_LOAD_NO_HINTING       = $0002;
  FT_LOAD_RENDER           = $0004;
  FT_LOAD_NO_BITMAP        = $0008;
  FT_LOAD_VERTICAL_LAYOUT  = $0010;
  FT_LOAD_FORCE_AUTOHINT   = $0020;
  FT_LOAD_CROP_BITMAP      = $0040;
  FT_LOAD_PEDANTIC         = $0080;
  FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH = $0200;
  FT_LOAD_NO_RECURSE       = $0400;
  FT_LOAD_IGNORE_TRANSFORM = $0800;
  FT_LOAD_MONOCHROME       = $1000;
  FT_LOAD_LINEAR_DESIGN    = $2000;

  (* temporary hack! *)
  FT_LOAD_SBITS_ONLY       = $4000;
  FT_LOAD_NO_AUTOHINT      = $8000;

  (*************************************************************************)
  (*                                                                       *)
  (* <Enum>                                                                *)
  (*    FT_Render_Mode                                                     *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    An enumeration type that lists the render modes supported by       *)
  (*    FreeType 2.  Each mode corresponds to a specific type of scanline  *)
  (*    conversion performed on the outline, as well as specific           *)
  (*    hinting optimizations.                                             *)
  (*                                                                       *)
  (*    For bitmap fonts the `bitmap->pixel_mode' field in the             *)
  (*    @FT_GlyphSlotRec structure gives the format of the returned        *)
  (*    bitmap.                                                            *)
  (*                                                                       *)
  (* <Values>                                                              *)
  (*    FT_RENDER_MODE_NORMAL ::                                           *)
  (*      This is the default render mode; it corresponds to 8-bit         *)
  (*      anti-aliased bitmaps, using 256 levels of opacity.               *)
  (*                                                                       *)
  (*    FT_RENDER_MODE_LIGHT ::                                            *)
  (*      This is similar to @FT_RENDER_MODE_NORMAL -- you have to use     *)
  (*      @FT_LOAD_TARGET_LIGHT in calls to @FT_Load_Glyph to get any      *)
  (*      effect since the rendering process no longer influences the      *)
  (*      positioning of glyph outlines.                                   *)
  (*                                                                       *)
  (*      The resulting glyph shapes are more similar to the original,     *)
  (*      while being a bit more fuzzy (`better shapes' instead of `better *)
  (*      contrast', so to say.                                            *)
  (*                                                                       *)
  (*    FT_RENDER_MODE_MONO ::                                             *)
  (*      This mode corresponds to 1-bit bitmaps.                          *)
  (*                                                                       *)
  (*    FT_RENDER_MODE_LCD ::                                              *)
  (*      This mode corresponds to horizontal RGB/BGR sub-pixel displays,  *)
  (*      like LCD-screens.  It produces 8-bit bitmaps that are 3 times    *)
  (*      the width of the original glyph outline in pixels, and which use *)
  (*      the @FT_PIXEL_MODE_LCD mode.                                     *)
  (*                                                                       *)
  (*    FT_RENDER_MODE_LCD_V ::                                            *)
  (*      This mode corresponds to vertical RGB/BGR sub-pixel displays     *)
  (*      (like PDA screens, rotated LCD displays, etc.).  It produces     *)
  (*      8-bit bitmaps that are 3 times the height of the original        *)
  (*      glyph outline in pixels and use the @FT_PIXEL_MODE_LCD_V mode.   *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*   The LCD-optimized glyph bitmaps produced by FT_Render_Glyph are     *)
  (*   _not filtered_ to reduce color-fringes.  It is up to the caller to  *)
  (*   perform this pass.                                                  *)
  (*                                                                       *)
type
  FT_Render_Mode = FT_Int;
const
  FT_RENDER_MODE_NORMAL = 0;
  FT_RENDER_MODE_LIGHT  = FT_RENDER_MODE_NORMAL + 1;
  FT_RENDER_MODE_MONO   = FT_RENDER_MODE_LIGHT  + 1;
  FT_RENDER_MODE_LCD    = FT_RENDER_MODE_MONO   + 1;
  FT_RENDER_MODE_LCD_V  = FT_RENDER_MODE_LCD    + 1;
  FT_RENDER_MODE_MAX    = FT_RENDER_MODE_LCD_V  + 1;


  (*************************************************************************)
  (*                                                                       *)
  (* <Enum>                                                                *)
  (*    FT_Pixel_Mode                                                      *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    An enumeration type used to describe the format of pixels in a     *)
  (*    given bitmap.  Note that additional formats may be added in the    *)
  (*    future.                                                            *)
  (*                                                                       *)
  (* <Values>                                                              *)
  (*    FT_PIXEL_MODE_NONE ::                                              *)
  (*      Value 0 is reserved.                                             *)
  (*                                                                       *)
  (*    FT_PIXEL_MODE_MONO ::                                              *)
  (*      A monochrome bitmap, using 1 bit per pixel.  Note that pixels    *)
  (*      are stored in most-significant order (MSB), which means that     *)
  (*      the left-most pixel in a byte has value 128.                     *)
  (*                                                                       *)
  (*    FT_PIXEL_MODE_GRAY ::                                              *)
  (*      An 8-bit bitmap, generally used to represent anti-aliased glyph  *)
  (*      images.  Each pixel is stored in one byte.  Note that the number *)
  (*      of value `gray' levels is stored in the `num_bytes' field of     *)
  (*      the @FT_Bitmap structure (it generally is 256).                  *)
  (*                                                                       *)
  (*    FT_PIXEL_MODE_GRAY2 ::                                             *)
  (*      A 2-bit/pixel bitmap, used to represent embedded anti-aliased    *)
  (*      bitmaps in font files according to the OpenType specification.   *)
  (*      We haven't found a single font using this format, however.       *)
  (*                                                                       *)
  (*    FT_PIXEL_MODE_GRAY4 ::                                             *)
  (*      A 4-bit/pixel bitmap, used to represent embedded anti-aliased    *)
  (*      bitmaps in font files according to the OpenType specification.   *)
  (*      We haven't found a single font using this format, however.       *)
  (*                                                                       *)
  (*    FT_PIXEL_MODE_LCD ::                                               *)
  (*      An 8-bit bitmap, used to represent RGB or BGR decimated glyph    *)
  (*      images used for display on LCD displays; the bitmap is three     *)
  (*      times wider than the original glyph image.  See also             *)
  (*      @FT_RENDER_MODE_LCD.                                             *)
  (*                                                                       *)
  (*    FT_PIXEL_MODE_LCD_V ::                                             *)
  (*      An 8-bit bitmap, used to represent RGB or BGR decimated glyph    *)
  (*      images used for display on rotated LCD displays; the bitmap      *)
  (*      is three times taller than the original glyph image.  See also   *)
  (*      @FT_RENDER_MODE_LCD_V.                                           *)
  (*                                                                       *)
type
  FT_Pixel_Mode = byte;
const
  FT_PIXEL_MODE_NONE    = 0;
  FT_PIXEL_MODE_MONO    = FT_PIXEL_MODE_NONE  + 1;
  FT_PIXEL_MODE_GRAY    = FT_PIXEL_MODE_MONO  + 1;
  FT_PIXEL_MODE_GRAY2   = FT_PIXEL_MODE_GRAY  + 1;
  FT_PIXEL_MODE_GRAY4   = FT_PIXEL_MODE_GRAY2 + 1;
  FT_PIXEL_MODE_LCD     = FT_PIXEL_MODE_GRAY4 + 1;
  FT_PIXEL_MODE_LCD_V   = FT_PIXEL_MODE_LCD   + 1;

  FT_PIXEL_MODE_MAX     = FT_PIXEL_MODE_LCD_V + 1; (* do not remove *)


  (*************************************************************************)
  (*                                                                       *)
  (* <Struct>                                                              *)
  (*    FT_Glyph_Metrics                                                   *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A structure used to model the metrics of a single glyph.  The      *)
  (*    values are expressed in 26.6 fractional pixel format; if the flag  *)
  (*    FT_LOAD_NO_SCALE is used, values are returned in font units        *)
  (*    instead.                                                           *)
  (*                                                                       *)
  (* <Fields>                                                              *)
  (*    width ::                                                           *)
  (*      The glyph's width.                                               *)
  (*                                                                       *)
  (*    height ::                                                          *)
  (*      The glyph's height.                                              *)
  (*                                                                       *)
  (*    horiBearingX ::                                                    *)
  (*      Left side bearing for horizontal layout.                         *)
  (*                                                                       *)
  (*    horiBearingY ::                                                    *)
  (*      Top side bearing for horizontal layout.                          *)
  (*                                                                       *)
  (*    horiAdvance ::                                                     *)
  (*      Advance width for horizontal layout.                             *)
  (*                                                                       *)
  (*    vertBearingX ::                                                    *)
  (*      Left side bearing for vertical layout.                           *)
  (*                                                                       *)
  (*    vertBearingY ::                                                    *)
  (*      Top side bearing for vertical layout.                            *)
  (*                                                                       *)
  (*    vertAdvance ::                                                     *)
  (*      Advance height for vertical layout.                              *)
  (*                                                                       *)
type
  FT_Glyph_Metrics = record
    width  ,
    height       : FT_Pos;

    horiBearingX ,
    horiBearingY ,
    horiAdvance  : FT_Pos;

    vertBearingX ,
    vertBearingY ,
    vertAdvance  : FT_Pos;
  end;


  (*************************************************************************)
  (*                                                                       *)
  (* <Struct>                                                              *)
  (*    FT_Bitmap_Size                                                     *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    This structure models the size of a bitmap strike (i.e., a bitmap  *)
  (*    instance of the font for a given resolution) in a fixed-size font  *)
  (*    face.  It is used for the `available_sizes' field of the           *)
  (*    @FT_FaceRec structure.                                             *)
  (*                                                                       *)
  (* <Fields>                                                              *)
  (*    height :: The (vertical) baseline-to-baseline distance in pixels.  *)
  (*              It makes most sense to define the height of a bitmap     *)
  (*              font in this way.                                        *)
  (*                                                                       *)
  (*    width  :: The average width of the font (in pixels).  Since the    *)
  (*              algorithms to compute this value are different for the   *)
  (*              various bitmap formats, it can only give an additional   *)
  (*              hint if the `height' value isn't sufficient to select    *)
  (*              the proper font.  For monospaced fonts the average width *)
  (*              is the same as the maximum width.                        *)
  (*                                                                       *)
  (*    size   :: The point size in 26.6 fractional format this font shall *)
  (*              represent (for a given vertical resolution).             *)
  (*                                                                       *)
  (*    x_ppem :: The horizontal ppem value (in 26.6 fractional format).   *)
  (*                                                                       *)
  (*    y_ppem :: The vertical ppem value (in 26.6 fractional format).     *)
  (*              Usually, this is the `nominal' pixel height of the font. *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    The values in this structure are taken from the bitmap font.  If   *)
  (*    the font doesn't provide a parameter it is set to zero to indicate *)
  (*    that the information is not available.                             *)
  (*                                                                       *)
  (*    The following formula converts from dpi to ppem:                   *)
  (*                                                                       *)
  (*      ppem = size * dpi / 72                                           *)
  (*                                                                       *)
  (*    where `size' is in points.                                         *)
  (*                                                                       *)
  (*    Windows FNT:                                                       *)
  (*      The `size' parameter is not reliable: There exist fonts (e.g.,   *)
  (*      app850.fon) which have a wrong size for some subfonts; x_ppem    *)
  (*      and y_ppem are thus set equal to pixel width and height given in *)
  (*      in the Windows FNT header.                                       *)
  (*                                                                       *)
  (*    TrueType embedded bitmaps:                                         *)
  (*      `size', `width', and `height' values are not contained in the    *)
  (*      bitmap strike itself.  They are computed from the global font    *)
  (*      parameters.                                                      *)
  (*                                                                       *)
  PFT_Bitmap_Size = ^FT_Bitmap_Size;
  FT_Bitmap_Size = record
    height,
    width  : FT_Short;
    
    size:    FT_Pos;

    x_ppem:  FT_Pos;
    y_ppem:  FT_Pos;
  end;

  PAFT_Bitmap_Size = ^AFT_Bitmap_Size;
  AFT_Bitmap_Size = array[0..High(Word)] of FT_Bitmap_Size;


  (*************************************************************************)
  (*                                                                       *)
  (* <Struct>                                                              *)
  (*    FT_Vector                                                          *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A simple structure used to store a 2D vector; coordinates are of   *)
  (*    the FT_Pos type.                                                   *)
  (*                                                                       *)
  (* <Fields>                                                              *)
  (*    x :: The horizontal coordinate.                                    *)
  (*    y :: The vertical coordinate.                                      *)
  (*                                                                       *)
  PFT_Vector = ^FT_Vector;
  FT_Vector = record
    x ,
    y : FT_Pos;
  end;


  (*************************************************************************)
  (*                                                                       *)
  (* <Struct>                                                              *)
  (*    FT_Outline                                                         *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    This structure is used to describe an outline to the scan-line     *)
  (*    converter.                                                         *)
  (*                                                                       *)
  (* <Fields>                                                              *)
  (*    n_contours :: The number of contours in the outline.               *)
  (*                                                                       *)
  (*    n_points   :: The number of points in the outline.                 *)
  (*                                                                       *)
  (*    points     :: A pointer to an array of `n_points' FT_Vector        *)
  (*                  elements, giving the outline's point coordinates.    *)
  (*                                                                       *)
  (*    tags       :: A pointer to an array of `n_points' chars, giving    *)
  (*                  each outline point's type.  If bit 0 is unset, the   *)
  (*                  point is `off' the curve, i.e. a Bezier control      *)
  (*                  point, while it is `on' when set.                    *)
  (*                                                                       *)
  (*                  Bit 1 is meaningful for `off' points only.  If set,  *)
  (*                  it indicates a third-order Bezier arc control point; *)
  (*                  and a second-order control point if unset.           *)
  (*                                                                       *)
  (*    contours   :: An array of `n_contours' shorts, giving the end      *)
  (*                  point of each contour within the outline.  For       *)
  (*                  example, the first contour is defined by the points  *)
  (*                  `0' to `contours[0]', the second one is defined by   *)
  (*                  the points `contours[0]+1' to `contours[1]', etc.    *)
  (*                                                                       *)
  (*    flags      :: A set of bit flags used to characterize the outline  *)
  (*                  and give hints to the scan-converter and hinter on   *)
  (*                  how to convert/grid-fit it.  See FT_Outline_Flags.   *)
  (*                                                                       *)
  PFT_Outline = ^FT_Outline;
  FT_Outline = record
    n_contours : FT_Short;
    n_points   : FT_Short;

    points : PFT_Vector;
    tags   : PChar;
    contours : PFT_Short;

    flags    : FT_Int;
  end;


  (*************************************************************************)
  (*                                                                       *)
  (* <Struct>                                                              *)
  (*    FT_Bitmap                                                          *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A structure used to describe a bitmap or pixmap to the raster.     *)
  (*    Note that we now manage pixmaps of various depths through the      *)
  (*    `pixel_mode' field.                                                *)
  (*                                                                       *)
  (* <Fields>                                                              *)
  (*    rows         :: The number of bitmap rows.                         *)
  (*                                                                       *)
  (*    width        :: The number of pixels in bitmap row.                *)
  (*                                                                       *)
  (*    pitch        :: The pitch's absolute value is the number of bytes  *)
  (*                    taken by one bitmap row, including padding.        *)
  (*                    However, the pitch is positive when the bitmap has *)
  (*                    a `down' flow, and negative when it has an `up'    *)
  (*                    flow.  In all cases, the pitch is an offset to add *)
  (*                    to a bitmap pointer in order to go down one row.   *)
  (*                                                                       *)
  (*    buffer       :: A typeless pointer to the bitmap buffer.  This     *)
  (*                    value should be aligned on 32-bit boundaries in    *)
  (*                    most cases.                                        *)
  (*                                                                       *)
  (*    num_grays    :: This field is only used with                       *)
  (*                    `FT_PIXEL_MODE_GRAY'; it gives the number of gray  *)
  (*                    levels used in the bitmap.                         *)
  (*                                                                       *)
  (*    pixel_mode   :: The pixel mode, i.e., how pixel bits are stored.   *)
  (*                    See @FT_Pixel_Mode for possible values.            *)
  (*                                                                       *)
  (*    palette_mode :: This field is only used with paletted pixel modes; *)
  (*                    it indicates how the palette is stored.            *)
  (*                                                                       *)
  (*    palette      :: A typeless pointer to the bitmap palette; only     *)
  (*                    used for paletted pixel modes.                     *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*   For now, the only pixel mode supported by FreeType are mono and     *)
  (*   grays.  However, drivers might be added in the future to support    *)
  (*   more `colorful' options.                                            *)
  (*                                                                       *)
  (*   When using pixel modes pal2, pal4 and pal8 with a void `palette'    *)
  (*   field, a gray pixmap with respectively 4, 16, and 256 levels of     *)
  (*   gray is assumed.  This, in order to be compatible with some         *)
  (*   embedded bitmap formats defined in the TrueType specification.      *)
  (*                                                                       *)
  (*   Note that no font was found presenting such embedded bitmaps, so    *)
  (*   this is currently completely unhandled by the library.              *)
  (*                                                                       *)
  PFT_Bitmap = ^FT_Bitmap;
  FT_Bitmap = record
    rows   ,
    width  : FT_Int;
    pitch  : FT_Int;
    buffer : PByteArray;
    num_grays    : FT_Short;
    pixel_mode   ,
    palette_mode : byte;
    palette : pointer;
  end;


  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_Face                                                            *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A handle to a given typographic face object.  A face object models *)
  (*    a given typeface, in a given style.                                *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    Each face object also owns a single @FT_GlyphSlot object, as well  *)
  (*    as one or more @FT_Size objects.                                   *)
  (*                                                                       *)
  (*    Use @FT_New_Face or @FT_Open_Face to create a new face object from *)
  (*    a given filepathname or a custom input stream.                     *)
  (*                                                                       *)
  (*    Use @FT_Done_Face to destroy it (along with its slot and sizes).   *)
  (*                                                                       *)
  (* <Also>                                                                *)
  (*    The @FT_FaceRec details the publicly accessible fields of a given  *)
  (*    face object.                                                       *)
  (*                                                                       *)
  FT_Face = ^FT_FaceRec;

  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_CharMap                                                         *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A handle to a given character map.  A charmap is used to translate *)
  (*    character codes in a given encoding into glyph indexes for its     *)
  (*    parent's face.  Some font formats may provide several charmaps per *)
  (*    font.                                                              *)
  (*                                                                       *)
  (*    Each face object owns zero or more charmaps, but only one of them  *)
  (*    can be "active" and used by @FT_Get_Char_Index or @FT_Load_Char.   *)
  (*                                                                       *)
  (*    The list of available charmaps in a face is available through the  *)
  (*    "face->num_charmaps" and "face->charmaps" fields of @FT_FaceRec.   *)
  (*                                                                       *)
  (*    The currently active charmap is available as "face->charmap".      *)
  (*    You should call @FT_Set_Charmap to change it.                      *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    When a new face is created (either through @FT_New_Face or         *)
  (*    @FT_Open_Face), the library looks for a Unicode charmap within     *)
  (*    the list and automatically activates it.                           *)
  (*                                                                       *)
  (* <Also>                                                                *)
  (*    The @FT_CharMapRec details the publicly accessible fields of a     *)
  (*    given character map.                                               *)
  (*                                                                       *)
  PFT_CharMap = ^FT_CharMap;
  FT_CharMap = ^FT_CharMapRec;

  PAFT_CharMap = ^FT_CharMap;
  AFT_CharMap = array[0..High(Word)] of FT_CharMap;

  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_Library                                                         *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A handle to a FreeType library instance.  Each `library' is        *)
  (*    completely independent from the others; it is the `root' of a set  *)
  (*    of objects like fonts, faces, sizes, etc.                          *)
  (*                                                                       *)
  (*    It also embeds a memory manager (see @FT_Memory), as well as a     *)
  (*    scan-line converter object (see @FT_Raster).                       *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    Library objects are normally created by @FT_Init_FreeType, and     *)
  (*    destroyed with @FT_Done_FreeType.                                  *)
  (*                                                                       *)
  FT_Library = ^FT_LibraryRec;
  FT_LibraryRec = record // internal
  end;

  (*************************************************************************)
  (*                                                                       *)
  (* <Section>                                                             *)
  (*    glyph_management                                                   *)
  (*                                                                       *)
  (* <Title>                                                               *)
  (*    Glyph Management                                                   *)
  (*                                                                       *)
  (* <Abstract>                                                            *)
  (*    Generic interface to manage individual glyph data.                 *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    This section contains definitions used to manage glyph data        *)
  (*    through generic FT_Glyph objects.  Each of them can contain a      *)
  (*    bitmap, a vector outline, or even images in other formats.         *)
  (*                                                                       *)
  (*************************************************************************)

  (* forward declaration to a private type *)
  PFT_Glyph_Class = ^FT_Glyph_Class;
  FT_Glyph_Class = record // internal
  end;

  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_Glyph                                                           *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    Handle to an object used to model generic glyph images.  It is a   *)
  (*    pointer to the @FT_GlyphRec structure and can contain a glyph      *)
  (*    bitmap or pointer.                                                 *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    Glyph objects are not owned by the library.  You must thus release *)
  (*    them manually (through @FT_Done_Glyph) _before_ calling            *)
  (*    @FT_Done_FreeType.                                                 *)
  (*                                                                       *)
  FT_Glyph = ^FT_GlyphRec;

  (*************************************************************************)
  (*                                                                       *)
  (* <Struct>                                                              *)
  (*    FT_GlyphRec                                                        *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    The root glyph structure contains a given glyph image plus its     *)
  (*    advance width in 16.16 fixed float format.                         *)
  (*                                                                       *)
  (* <Fields>                                                              *)
  (*    library :: A handle to the FreeType library object.                *)
  (*                                                                       *)
  (*    clazz   :: A pointer to the glyph's class.  Private.               *)
  (*                                                                       *)
  (*    format  :: The format of the glyph's image.                        *)
  (*                                                                       *)
  (*    advance :: A 16.16 vector that gives the glyph's advance width.    *)
  (*                                                                       *)
  FT_GlyphRec = record
    library_: FT_Library;
    clazz:    PFT_Glyph_Class;
    format:   FT_Glyph_Format;
    advance:  FT_Vector;
  end;


  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_BitmapGlyph                                                     *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A handle to an object used to model a bitmap glyph image.  This is *)
  (*    a sub-class of @FT_Glyph, and a pointer to @FT_BitmapGlyphRec.     *)
  (*                                                                       *)
  FT_BitmapGlyph = ^FT_BitmapGlyphRec;

  (*************************************************************************)
  (*                                                                       *)
  (* <Struct>                                                              *)
  (*    FT_BitmapGlyphRec                                                  *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A structure used for bitmap glyph images.  This really is a        *)
  (*    `sub-class' of `FT_GlyphRec'.                                      *)
  (*                                                                       *)
  (* <Fields>                                                              *)
  (*    root   :: The root FT_Glyph fields.                                *)
  (*                                                                       *)
  (*    left   :: The left-side bearing, i.e., the horizontal distance     *)
  (*              from the current pen position to the left border of the  *)
  (*              glyph bitmap.                                            *)
  (*                                                                       *)
  (*    top    :: The top-side bearing, i.e., the vertical distance from   *)
  (*              the current pen position to the top border of the glyph  *)
  (*              bitmap.  This distance is positive for upwards-y!        *)
  (*                                                                       *)
  (*    bitmap :: A descriptor for the bitmap.                             *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    You can typecast FT_Glyph to FT_BitmapGlyph if you have            *)
  (*    glyph->format == FT_GLYPH_FORMAT_BITMAP.  This lets you access     *)
  (*    the bitmap's contents easily.                                      *)
  (*                                                                       *)
  (*    The corresponding pixel buffer is always owned by the BitmapGlyph  *)
  (*    and is thus created and destroyed with it.                         *)
  (*                                                                       *)
  FT_BitmapGlyphRec = record
    root:   FT_GlyphRec;
    left:   FT_Int;
    top:    FT_Int;
    bitmap: FT_Bitmap;
  end;


  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_OutlineGlyph                                                    *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A handle to an object used to model an outline glyph image.  This  *)
  (*    is a sub-class of @FT_Glyph, and a pointer to @FT_OutlineGlyphRec. *)
  (*                                                                       *)
  FT_OutlineGlyph = ^FT_OutlineGlyphRec;

  (*************************************************************************)
  (*                                                                       *)
  (* <Struct>                                                              *)
  (*    FT_OutlineGlyphRec                                                 *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A structure used for outline (vectorial) glyph images.  This       *)
  (*    really is a `sub-class' of `FT_GlyphRec'.                          *)
  (*                                                                       *)
  (* <Fields>                                                              *)
  (*    root    :: The root FT_Glyph fields.                               *)
  (*                                                                       *)
  (*    outline :: A descriptor for the outline.                           *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    You can typecast FT_Glyph to FT_OutlineGlyph if you have           *)
  (*    glyph->format == FT_GLYPH_FORMAT_OUTLINE.  This lets you access    *)
  (*    the outline's content easily.                                      *)
  (*                                                                       *)
  (*    As the outline is extracted from a glyph slot, its coordinates are *)
  (*    expressed normally in 26.6 pixels, unless the flag                 *)
  (*    FT_LOAD_NO_SCALE was used in FT_Load_Glyph() or FT_Load_Char().    *)
  (*                                                                       *)
  (*    The outline's tables are always owned by the object and are        *)
  (*    destroyed with it.                                                 *)
  (*                                                                       *)
  FT_OutlineGlyphRec = record
    root:    FT_GlyphRec;
    outline: FT_Outline;
  end;


  (*************************************************************************)
  (*                                                                       *)
  (* <Struct>                                                              *)
  (*    FT_SubGlyph                                                        *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    The subglyph structure is an internal object used to describe      *)
  (*    subglyphs (for example, in the case of composites).                *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    The subglyph implementation is not part of the high-level API,     *)
  (*    hence the forward structure declaration.                           *)
  (*                                                                       *)
  FT_SubGlyph = ^FT_SubGlyphRec;
  FT_SubGlyphRec = record // internal
  end;


  (*************************************************************************)
  (*                                                                       *)
  (* <FuncType>                                                            *)
  (*    FT_Generic_Finalizer                                               *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    Describes a function used to destroy the `client' data of any      *)
  (*    FreeType object.  See the description of the FT_Generic type for   *)
  (*    details of usage.                                                  *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    The address of the FreeType object which is under finalization.    *)
  (*    Its client data is accessed through its `generic' field.           *)
  (*                                                                       *)
  FT_Generic_Finalizer = procedure(AnObject : pointer ); cdecl;

  (*************************************************************************)
  (*                                                                       *)
  (* <Struct>                                                              *)
  (*    FT_Generic                                                         *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    Client applications often need to associate their own data to a    *)
  (*    variety of FreeType core objects.  For example, a text layout API  *)
  (*    might want to associate a glyph cache to a given size object.      *)
  (*                                                                       *)
  (*    Most FreeType object contains a `generic' field, of type           *)
  (*    FT_Generic, which usage is left to client applications and font    *)
  (*    servers.                                                           *)
  (*                                                                       *)
  (*    It can be used to store a pointer to client-specific data, as well *)
  (*    as the address of a `finalizer' function, which will be called by  *)
  (*    FreeType when the object is destroyed (for example, the previous   *)
  (*    client example would put the address of the glyph cache destructor *)
  (*    in the `finalizer' field).                                         *)
  (*                                                                       *)
  (* <Fields>                                                              *)
  (*    data      :: A typeless pointer to any client-specified data. This *)
  (*                 field is completely ignored by the FreeType library.  *)
  (*                                                                       *)
  (*    finalizer :: A pointer to a `generic finalizer' function, which    *)
  (*                 will be called when the object is destroyed.  If this *)
  (*                 field is set to NULL, no code will be called.         *)
  (*                                                                       *)
  FT_Generic = record
    data      : pointer;
    finalizer : FT_Generic_Finalizer;
  end;

  (*************************************************************************)
  (*                                                                       *)
  (* <Struct>                                                              *)
  (*    FT_BBox                                                            *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A structure used to hold an outline's bounding box, i.e., the      *)
  (*    coordinates of its extrema in the horizontal and vertical          *)
  (*    directions.                                                        *)
  (*                                                                       *)
  (* <Fields>                                                              *)
  (*    xMin :: The horizontal minimum (left-most).                        *)
  (*                                                                       *)
  (*    yMin :: The vertical minimum (bottom-most).                        *)
  (*                                                                       *)
  (*    xMax :: The horizontal maximum (right-most).                       *)
  (*                                                                       *)
  (*    yMax :: The vertical maximum (top-most).                           *)
  (*                                                                       *)
  PFT_BBox = ^FT_BBox;
  FT_BBox = record
    xMin, yMin : FT_Pos;
    xMax, yMax : FT_Pos;
  end;


  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_GlyphSlot                                                       *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A handle to a given `glyph slot'.  A slot is a container where it  *)
  (*    is possible to load any one of the glyphs contained in its parent  *)
  (*    face.                                                              *)
  (*                                                                       *)
  (*    In other words, each time you call @FT_Load_Glyph or               *)
  (*    @FT_Load_Char, the slot's content is erased by the new glyph data, *)
  (*    i.e. the glyph's metrics, its image (bitmap or outline), and       *)
  (*    other control information.                                         *)
  (*                                                                       *)
  (* <Also>                                                                *)
  (*    @FT_GlyphSlotRec details the publicly accessible glyph fields.     *)
  (*                                                                       *)
  FT_GlyphSlot = ^FT_GlyphSlotRec;

  (*************************************************************************)
  (*                                                                       *)
  (* <Struct>                                                              *)
  (*    FT_GlyphSlotRec                                                    *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    FreeType root glyph slot class structure.  A glyph slot is a       *)
  (*    container where individual glyphs can be loaded, be they           *)
  (*    vectorial or bitmap/graymaps.                                      *)
  (*                                                                       *)
  (* <Fields>                                                              *)
  (*    library           :: A handle to the FreeType library instance     *)
  (*                         this slot belongs to.                         *)
  (*                                                                       *)
  (*    face              :: A handle to the parent face object.           *)
  (*                                                                       *)
  (*    next              :: In some cases (like some font tools), several *)
  (*                         glyph slots per face object can be a good     *)
  (*                         thing.  As this is rare, the glyph slots are  *)
  (*                         listed through a direct, single-linked list   *)
  (*                         using its `next' field.                       *)
  (*                                                                       *)
  (*    generic           :: A typeless pointer which is unused by the     *)
  (*                         FreeType library or any of its drivers.  It   *)
  (*                         can be used by client applications to link    *)
  (*                         their own data to each glyph slot object.     *)
  (*                                                                       *)
  (*    metrics           :: The metrics of the last loaded glyph in the   *)
  (*                         slot.  The returned values depend on the last *)
  (*                         load flags (see the @FT_Load_Glyph API        *)
  (*                         function) and can be expressed either in 26.6 *)
  (*                         fractional pixels or font units.              *)
  (*                                                                       *)
  (*                         Note that even when the glyph image is        *)
  (*                         transformed, the metrics are not.             *)
  (*                                                                       *)
  (*    linearHoriAdvance :: For scalable formats only, this field holds   *)
  (*                         the linearly scaled horizontal advance width  *)
  (*                         for the glyph (i.e. the scaled and unhinted   *)
  (*                         value of the hori advance).  This can be      *)
  (*                         important to perform correct WYSIWYG layout.  *)
  (*                                                                       *)
  (*                         Note that this value is expressed by default  *)
  (*                         in 16.16 pixels. However, when the glyph is   *)
  (*                         loaded with the FT_LOAD_LINEAR_DESIGN flag,   *)
  (*                         this field contains simply the value of the   *)
  (*                         advance in original font units.               *)
  (*                                                                       *)
  (*    linearVertAdvance :: For scalable formats only, this field holds   *)
  (*                         the linearly scaled vertical advance height   *)
  (*                         for the glyph.  See linearHoriAdvance for     *)
  (*                         comments.                                     *)
  (*                                                                       *)
  (*    advance           :: This is the transformed advance width for the *)
  (*                         glyph.                                        *)
  (*                                                                       *)
  (*    format            :: This field indicates the format of the image  *)
  (*                         contained in the glyph slot.  Typically       *)
  (*                         FT_GLYPH_FORMAT_BITMAP,                       *)
  (*                         FT_GLYPH_FORMAT_OUTLINE, and                  *)
  (*                         FT_GLYPH_FORMAT_COMPOSITE, but others are     *)
  (*                         possible.                                     *)
  (*                                                                       *)
  (*    bitmap            :: This field is used as a bitmap descriptor     *)
  (*                         when the slot format is                       *)
  (*                         FT_GLYPH_FORMAT_BITMAP.  Note that the        *)
  (*                         address and content of the bitmap buffer can  *)
  (*                         change between calls of @FT_Load_Glyph and a  *)
  (*                         few other functions.                          *)
  (*                                                                       *)
  (*    bitmap_left       :: This is the bitmap's left bearing expressed   *)
  (*                         in integer pixels.  Of course, this is only   *)
  (*                         valid if the format is                        *)
  (*                         FT_GLYPH_FORMAT_BITMAP.                       *)
  (*                                                                       *)
  (*    bitmap_top        :: This is the bitmap's top bearing expressed in *)
  (*                         integer pixels.  Remember that this is the    *)
  (*                         distance from the baseline to the top-most    *)
  (*                         glyph scanline, upwards y-coordinates being   *)
  (*                         *positive*.                                   *)
  (*                                                                       *)
  (*    outline           :: The outline descriptor for the current glyph  *)
  (*                         image if its format is                        *)
  (*                         FT_GLYPH_FORMAT_OUTLINE.                      *)
  (*                                                                       *)
  (*    num_subglyphs     :: The number of subglyphs in a composite glyph. *)
  (*                         This field is only valid for the composite    *)
  (*                         glyph format that should normally only be     *)
  (*                         loaded with the @FT_LOAD_NO_RECURSE flag.     *)
  (*                         For now this is internal to FreeType.         *)
  (*                                                                       *)
  (*    subglyphs         :: An array of subglyph descriptors for          *)
  (*                         composite glyphs.  There are `num_subglyphs'  *)
  (*                         elements in there.  Currently internal to     *)
  (*                         FreeType.                                     *)
  (*                                                                       *)
  (*    control_data      :: Certain font drivers can also return the      *)
  (*                         control data for a given glyph image (e.g.    *)
  (*                         TrueType bytecode, Type 1 charstrings, etc.). *)
  (*                         This field is a pointer to such data.         *)
  (*                                                                       *)
  (*    control_len       :: This is the length in bytes of the control    *)
  (*                         data.                                         *)
  (*                                                                       *)
  (*    other             :: Really wicked formats can use this pointer to *)
  (*                         present their own glyph image to client apps. *)
  (*                         Note that the app will need to know about the *)
  (*                         image format.                                 *)
  (*                                                                       *)
  (*    lsb_delta         :: The difference between hinted and unhinted    *)
  (*                         left side bearing while autohinting is        *)
  (*                         active.  Zero otherwise.                      *)
  (*                                                                       *)
  (*    rsb_delta         :: The difference between hinted and unhinted    *)
  (*                         right side bearing while autohinting is       *)
  (*                         active.  Zero otherwise.                      *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    If @FT_Load_Glyph is called with default flags (see                *)
  (*    @FT_LOAD_DEFAULT) the glyph image is loaded in the glyph slot in   *)
  (*    its native format (e.g. a vectorial outline for TrueType and       *)
  (*    Type 1 formats).                                                   *)
  (*                                                                       *)
  (*    This image can later be converted into a bitmap by calling         *)
  (*    @FT_Render_Glyph.  This function finds the current renderer for    *)
  (*    the native image's format then invokes it.                         *)
  (*                                                                       *)
  (*    The renderer is in charge of transforming the native image through *)
  (*    the slot's face transformation fields, then convert it into a      *)
  (*    bitmap that is returned in `slot->bitmap'.                         *)
  (*                                                                       *)
  (*    Note that `slot->bitmap_left' and `slot->bitmap_top' are also used *)
  (*    to specify the position of the bitmap relative to the current pen  *)
  (*    position (e.g. coordinates [0,0] on the baseline).  Of course,     *)
  (*    `slot->format' is also changed to `FT_GLYPH_FORMAT_BITMAP' .       *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    Here a small pseudo code fragment which shows how to use           *)
  (*    `lsb_delta' and `rsb_delta':                                       *)
  (*                                                                       *)
  (*    {                                                                  *)
  (*      FT_Pos  origin_x       = 0;                                      *)
  (*      FT_Pos  prev_rsb_delta = 0;                                      *)
  (*                                                                       *)
  (*                                                                       *)
  (*      for all glyphs do                                                *)
  (*        <compute kern between current and previous glyph and add it to *)
  (*         `origin_x'>                                                   *)
  (*                                                                       *)
  (*        <load glyph with `FT_Load_Glyph'>                              *)
  (*                                                                       *)
  (*        if ( prev_rsb_delta - face->glyph->lsb_delta >= 32 )           *)
  (*          origin_x -= 64;                                              *)
  (*        else if ( prev_rsb_delta - face->glyph->lsb_delta < -32 )      *)
  (*          origin_x += 64;                                              *)
  (*                                                                       *)
  (*        prev_rsb_delta = face->glyph->rsb_delta;                       *)
  (*                                                                       *)
  (*        <save glyph image, or render glyph, or ...>                    *)
  (*                                                                       *)
  (*        origin_x += face->glyph->advance.x;                            *)
  (*      endfor                                                           *)
  (*    }                                                                  *)
  (*                                                                       *)
  FT_GlyphSlotRec = record
    alibrary : FT_Library;

    face  : FT_Face;
    next  : FT_GlyphSlot;
    flags : FT_UInt;

    generic : FT_Generic;
    metrics : FT_Glyph_Metrics;

    linearHoriAdvance ,
    linearVertAdvance : FT_Fixed;

    advance : FT_Vector;
    format  : FT_Glyph_Format;
    bitmap  : FT_Bitmap;

    bitmap_left ,
    bitmap_top  : FT_Int;

    outline : FT_Outline;

    num_subglyphs : FT_UInt;
    subglyphs     : FT_SubGlyph;
    
    control_data  : pointer;
    control_len   : longint;

    lsb_delta: FT_Pos;
    rsb_delta: FT_Pos;

    other : pointer;
    
    //internal: FT_Slot_Internal;
  end;

  (*************************************************************************)
  (*                                                                       *)
  (* <Struct>                                                              *)
  (*    FT_Size_Metrics                                                    *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    The size metrics structure returned scaled important distances for *)
  (*    a given size object.                                               *)
  (*                                                                       *)
  (* <Fields>                                                              *)
  (*    x_ppem       :: The character width, expressed in integer pixels.  *)
  (*                    This is the width of the EM square expressed in    *)
  (*                    pixels, hence the term `ppem' (pixels per EM).     *)
  (*                                                                       *)
  (*    y_ppem       :: The character height, expressed in integer pixels. *)
  (*                    This is the height of the EM square expressed in   *)
  (*                    pixels, hence the term `ppem' (pixels per EM).     *)
  (*                                                                       *)
  (*    x_scale      :: A simple 16.16 fixed point format coefficient used *)
  (*                    to scale horizontal distances expressed in font    *)
  (*                    units to fractional (26.6) pixel coordinates.      *)
  (*                                                                       *)
  (*    y_scale      :: A simple 16.16 fixed point format coefficient used *)
  (*                    to scale vertical distances expressed in font      *)
  (*                    units to fractional (26.6) pixel coordinates.      *)
  (*                                                                       *)
  (*    ascender     :: The ascender, expressed in 26.6 fixed point        *)
  (*                    pixels.  Positive for ascenders above the          *)
  (*                    baseline.                                          *)
  (*                                                                       *)
  (*    descender    :: The descender, expressed in 26.6 fixed point       *)
  (*                    pixels.  Negative for descenders below the         *)
  (*                    baseline.                                          *)
  (*                                                                       *)
  (*    height       :: The text height, expressed in 26.6 fixed point     *)
  (*                    pixels.  Always positive.                          *)
  (*                                                                       *)
  (*    max_advance  :: Maximum horizontal advance, expressed in 26.6      *)
  (*                    fixed point pixels.  Always positive.              *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    For scalable fonts, the values of `ascender', `descender', and     *)
  (*    `height' are scaled versions of `face->ascender',                  *)
  (*    `face->descender', and `face->height', respectively.               *)
  (*                                                                       *)
  (*    Unfortunately, due to glyph hinting, these values might not be     *)
  (*    exact for certain fonts.  They thus must be treated as unreliable  *)
  (*    with an error margin of at least one pixel!                        *)
  (*                                                                       *)
  (*    Indeed, the only way to get the exact pixel ascender and descender *)
  (*    is to render _all_ glyphs.  As this would be a definite            *)
  (*    performance hit, it is up to client applications to perform such   *)
  (*    computations.                                                      *)
  (*                                                                       *)
  FT_Size_Metrics = record
    x_ppem  ,
    y_ppem  : FT_UShort;
    x_scale ,
    y_scale : FT_Fixed;

    ascender    ,
    descender   : FT_Pos;
    height      : FT_Pos;
    max_advance : FT_Pos;
  end;

  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_Size                                                            *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A handle to a given size object.  Such an object models the data   *)
  (*    that depends on the current _resolution_ and _character size_ in a *)
  (*    given @FT_Face.                                                    *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    Each face object owns one or more sizes.  There is however a       *)
  (*    single _active_ size for the face at any time that will be used by *)
  (*    functions like @FT_Load_Glyph, @FT_Get_Kerning, etc.               *)
  (*                                                                       *)
  (*    You can use the @FT_Activate_Size API to change the current        *)
  (*    active size of any given face.                                     *)
  (*                                                                       *)
  (* <Also>                                                                *)
  (*    The @FT_SizeRec structure details the publicly accessible fields   *)
  (*    of a given face object.                                            *)
  (*                                                                       *)
  FT_Size = ^FT_SizeRec;

  (*************************************************************************)
  (*                                                                       *)
  (* <Struct>                                                              *)
  (*    FT_SizeRec                                                         *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    FreeType root size class structure.  A size object models the      *)
  (*    resolution and pointsize dependent data of a given face.           *)
  (*                                                                       *)
  (* <Fields>                                                              *)
  (*    face    :: Handle to the parent face object.                       *)
  (*                                                                       *)
  (*    generic :: A typeless pointer, which is unused by the FreeType     *)
  (*               library or any of its drivers.  It can be used by       *)
  (*               client applications to link their own data to each size *)
  (*               object.                                                 *)
  (*                                                                       *)
  (*    metrics :: Metrics for this size object.  This field is read-only. *)
  (*                                                                       *)
  FT_SizeRec = record
    face    : FT_Face;
    generic : FT_Generic;
    metrics : FT_Size_Metrics;
    //internal : FT_Size_Internal;
  end;


  (*************************************************************************)
  (*                                                                       *)
  (* <Struct>                                                              *)
  (*    FT_FaceRec                                                         *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    FreeType root face class structure.  A face object models the      *)
  (*    resolution and point-size independent data found in a font file.   *)
  (*                                                                       *)
  (* <Fields>                                                              *)
  (*    num_faces           :: In the case where the face is located in a  *)
  (*                           collection (i.e., a file which embeds       *)
  (*                           several faces), this is the total number of *)
  (*                           faces found in the resource.  1 by default. *)
  (*                           Accessing non-existent face indices causes  *)
  (*                           an error.                                   *)
  (*                                                                       *)
  (*    face_index          :: The index of the face in its font file.     *)
  (*                           Usually, this is 0 for all normal font      *)
  (*                           formats.  It can be > 0 in the case of      *)
  (*                           collections (which embed several fonts in a *)
  (*                           single resource/file).                      *)
  (*                                                                       *)
  (*    face_flags          :: A set of bit flags that give important      *)
  (*                           information about the face; see the         *)
  (*                           @FT_FACE_FLAG_XXX constants for details.    *)
  (*                                                                       *)
  (*    style_flags         :: A set of bit flags indicating the style of  *)
  (*                           the face (i.e., italic, bold, underline,    *)
  (*                           etc).  See the @FT_STYLE_FLAG_XXX           *)
  (*                           constants.                                  *)
  (*                                                                       *)
  (*    num_glyphs          :: The total number of glyphs in the face.     *)
  (*                                                                       *)
  (*    family_name         :: The face's family name.  This is an ASCII   *)
  (*                           string, usually in English, which describes *)
  (*                           the typeface's family (like `Times New      *)
  (*                           Roman', `Bodoni', `Garamond', etc).  This   *)
  (*                           is a least common denominator used to list  *)
  (*                           fonts.  Some formats (TrueType & OpenType)  *)
  (*                           provide localized and Unicode versions of   *)
  (*                           this string.  Applications should use the   *)
  (*                           format specific interface to access them.   *)
  (*                                                                       *)
  (*    style_name          :: The face's style name.  This is an ASCII    *)
  (*                           string, usually in English, which describes *)
  (*                           the typeface's style (like `Italic',        *)
  (*                           `Bold', `Condensed', etc).  Not all font    *)
  (*                           formats provide a style name, so this field *)
  (*                           is optional, and can be set to NULL.  As    *)
  (*                           for `family_name', some formats provide     *)
  (*                           localized/Unicode versions of this string.  *)
  (*                           Applications should use the format specific *)
  (*                           interface to access them.                   *)
  (*                                                                       *)
  (*    num_fixed_sizes     :: The number of fixed sizes available in this *)
  (*                           face.  This should be set to 0 for scalable *)
  (*                           fonts, unless its face includes a set of    *)
  (*                           glyphs (called a `strike') for the          *)
  (*                           specified sizes.                            *)
  (*                                                                       *)
  (*    available_sizes     :: An array of sizes specifying the available  *)
  (*                           bitmap/graymap sizes that are contained in  *)
  (*                           in the font face.  Should be set to NULL if *)
  (*                           the field `num_fixed_sizes' is set to 0.    *)
  (*                                                                       *)
  (*    num_charmaps        :: The total number of character maps in the   *)
  (*                           face.                                       *)
  (*                                                                       *)
  (*    charmaps            :: A table of pointers to the face's charmaps. *)
  (*                           Used to scan the list of available charmaps *)
  (*                           -- this table might change after a call to  *)
  (*                           @FT_Attach_File or @FT_Attach_Stream (e.g.  *)
  (*                           if used to hook an additional encoding or   *)
  (*                           CMap to the face object).                   *)
  (*                                                                       *)
  (*    generic             :: A field reserved for client uses.  See the  *)
  (*                           @FT_Generic type description.               *)
  (*                                                                       *)
  (*    bbox                :: The font bounding box.  Coordinates are     *)
  (*                           expressed in font units (see units_per_EM). *)
  (*                           The box is large enough to contain any      *)
  (*                           glyph from the font.  Thus, bbox.yMax can   *)
  (*                           be seen as the `maximal ascender',          *)
  (*                           bbox.yMin as the `minimal descender', and   *)
  (*                           the maximal glyph width is given by         *)
  (*                           `bbox.xMax-bbox.xMin' (not to be confused   *)
  (*                           with the maximal _advance_width_).  Only    *)
  (*                           relevant for scalable formats.              *)
  (*                                                                       *)
  (*    units_per_EM        :: The number of font units per EM square for  *)
  (*                           this face.  This is typically 2048 for      *)
  (*                           TrueType fonts, 1000 for Type1 fonts, and   *)
  (*                           should be set to the (unrealistic) value 1  *)
  (*                           for fixed-sizes fonts.  Only relevant for   *)
  (*                           scalable formats.                           *)
  (*                                                                       *)
  (*    ascender            :: The face's ascender is the vertical         *)
  (*                           distance from the baseline to the topmost   *)
  (*                           point of any glyph in the face.  This       *)
  (*                           field's value is positive, expressed in     *)
  (*                           font units.  Some font designs use a value  *)
  (*                           different from `bbox.yMax'.  Only relevant  *)
  (*                           for scalable formats.                       *)
  (*                                                                       *)
  (*    descender           :: The face's descender is the vertical        *)
  (*                           distance from the baseline to the           *)
  (*                           bottommost point of any glyph in the face.  *)
  (*                           This field's value is *negative* for values *)
  (*                           below the baseline.  It is expressed in     *)
  (*                           font units.  Some font designs use a value  *)
  (*                           different from `bbox.yMin'.  Only relevant  *)
  (*                           for scalable formats.                       *)
  (*                                                                       *)
  (*    height              :: The face's height is the vertical distance  *)
  (*                           from one baseline to the next when writing  *)
  (*                           several lines of text.  Its value is always *)
  (*                           positive, expressed in font units.  The     *)
  (*                           value can be computed as                    *)
  (*                           `ascender+descender+line_gap' where the     *)
  (*                           value of `line_gap' is also called          *)
  (*                           `external leading'.  Only relevant for      *)
  (*                           scalable formats.                           *)
  (*                                                                       *)
  (*    max_advance_width   :: The maximal advance width, in font units,   *)
  (*                           for all glyphs in this face.  This can be   *)
  (*                           used to make word wrapping computations     *)
  (*                           faster.  Only relevant for scalable         *)
  (*                           formats.                                    *)
  (*                                                                       *)
  (*    max_advance_height  :: The maximal advance height, in font units,  *)
  (*                           for all glyphs in this face.  This is only  *)
  (*                           relevant for vertical layouts, and should   *)
  (*                           be set to the `height' for fonts that do    *)
  (*                           not provide vertical metrics.  Only         *)
  (*                           relevant for scalable formats.              *)
  (*                                                                       *)
  (*    underline_position  :: The position, in font units, of the         *)
  (*                           underline line for this face.  It's the     *)
  (*                           center of the underlining stem.  Only       *)
  (*                           relevant for scalable formats.              *)
  (*                                                                       *)
  (*    underline_thickness :: The thickness, in font units, of the        *)
  (*                           underline for this face.  Only relevant for *)
  (*                           scalable formats.                           *)
  (*                                                                       *)
  (*    glyph               :: The face's associated glyph slot(s).  This  *)
  (*                           object is created automatically with a new  *)
  (*                           face object.  However, certain kinds of     *)
  (*                           applications (mainly tools like converters) *)
  (*                           can need more than one slot to ease their   *)
  (*                           task.                                       *)
  (*                                                                       *)
  (*    size                :: The current active size for this face.      *)
  (*                                                                       *)
  (*    charmap             :: The current active charmap for this face.   *)
  (*                                                                       *)
  FT_FaceRec = record
    num_faces       : FT_Long;
    face_index      : FT_Long;

    face_flags      : FT_Long;
    style_flags     : FT_Long;

    num_glyphs      : FT_Long;

    family_name     : PFT_String;
    style_name      : PFT_String;

    num_fixed_sizes : FT_Int;
    available_sizes : PAFT_Bitmap_Size; // is array

    num_charmaps    : FT_Int;
    charmaps        : PAFT_CharMap;    // is array

    generic         : FT_Generic;

    (*# the following are only relevant to scalable outlines *)
    bbox            : FT_BBox;

    units_per_EM    : FT_UShort;
    ascender        : FT_Short;
    descender       : FT_Short;
    height          : FT_Short;

    max_advance_width   : FT_Short;
    max_advance_height  : FT_Short;
    
    underline_position  : FT_Short;
    underline_thickness : FT_Short;

    glyph           : FT_GlyphSlot;
    size            : FT_Size;
    charmap         : FT_CharMap;
  end;


  (*************************************************************************)
  (*                                                                       *)
  (* <Struct>                                                              *)
  (*    FT_CharMapRec                                                      *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    The base charmap structure.                                        *)
  (*                                                                       *)
  (* <Fields>                                                              *)
  (*    face        :: A handle to the parent face object.                 *)
  (*                                                                       *)
  (*    encoding    :: An @FT_Encoding tag identifying the charmap.  Use   *)
  (*                   this with @FT_Select_Charmap.                       *)
  (*                                                                       *)
  (*    platform_id :: An ID number describing the platform for the        *)
  (*                   following encoding ID.  This comes directly from    *)
  (*                   the TrueType specification and should be emulated   *)
  (*                   for other formats.                                  *)
  (*                                                                       *)
  (*    encoding_id :: A platform specific encoding number.  This also     *)
  (*                   comes from the TrueType specification and should be *)
  (*                   emulated similarly.                                 *)
  (*                                                                       *)
  FT_CharMapRec = record
    face        : FT_Face;
    encoding    : FT_Encoding;
    platform_id : FT_UShort;
    encoding_id : FT_UShort;
  end;

{ GLOBAL PROCEDURES }

  (*************************************************************************)
  (*                                                                       *)
  (* @macro:                                                               *)
  (*    FT_CURVE_TAG  ( flag )                                             *)
  (*                                                                       *)
  function  FT_CURVE_TAG(flag: byte): byte;

const
  FT_CURVE_TAG_ON    = 1;
  FT_CURVE_TAG_CONIC = 0;
  FT_CURVE_TAG_CUBIC = 2;


  (*************************************************************************)
  (*                                                                       *)
  (* @macro:                                                               *)
  (*    FT_IS_SCALABLE( face )                                             *)
  (*                                                                       *)
  (* @description:                                                         *)
  (*    A macro that returns true whenever a face object contains a        *)
  (*    scalable font face (true for TrueType, Type 1, CID, and            *)
  (*    OpenType/CFF font formats.                                         *)
  (*                                                                       *)
  function  FT_IS_SCALABLE(face : FT_Face ) : cbool;

  (*************************************************************************)
  (*                                                                       *)
  (* @macro:                                                               *)
  (*    FT_HAS_KERNING( face )                                             *)
  (*                                                                       *)
  (* @description:                                                         *)
  (*    A macro that returns true whenever a face object contains kerning  *)
  (*    data that can be accessed with @FT_Get_Kerning.                    *)
  (*                                                                       *)
  function  FT_HAS_KERNING(face : FT_Face ) : cbool;

  (*************************************************************************)
  (*                                                                       *)
  (* <Function>                                                            *)
  (*    FT_Init_FreeType                                                   *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    Initializes a new FreeType library object.  The set of modules     *)
  (*    that are registered by this function is determined at build time.  *)
  (*                                                                       *)
  (* <Output>                                                              *)
  (*    alibrary :: A handle to a new library object.                      *)
  (*                                                                       *)
  (* <Return>                                                              *)
  (*    FreeType error code.  0 means success.                             *)
  (*                                                                       *)
  function  FT_Init_FreeType(out alibrary : FT_Library ) : FT_Error;
    cdecl; external ft_lib name 'FT_Init_FreeType';

 (*************************************************************************)
  (*                                                                       *)
  (* <Function>                                                            *)
  (*    FT_Done_FreeType                                                   *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    Destroys a given FreeType library object and all of its childs,    *)
  (*    including resources, drivers, faces, sizes, etc.                   *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    library :: A handle to the target library object.                  *)
  (*                                                                       *)
  (* <Return>                                                              *)
  (*    FreeType error code.  0 means success.                             *)
  (*                                                                       *)
  function  FT_Done_FreeType(alibrary : FT_Library ) : FT_Error;
    cdecl; external ft_lib name 'FT_Done_FreeType';

  (*************************************************************************)
  (*                                                                       *)
  (* <Function>                                                            *)
  (*    FT_Attach_File                                                     *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    `Attaches' a given font file to an existing face.  This is usually *)
  (*    to read additional information for a single face object.  For      *)
  (*    example, it is used to read the AFM files that come with Type 1    *)
  (*    fonts in order to add kerning data and other metrics.              *)
  (*                                                                       *)
  (* <InOut>                                                               *)
  (*    face         :: The target face object.                            *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    filepathname :: An 8-bit pathname naming the `metrics' file.       *)
  (*                                                                       *)
  (* <Return>                                                              *)
  (*    FreeType error code.  0 means success.                             *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    If your font file is in memory, or if you want to provide your     *)
  (*    own input stream object, use @FT_Attach_Stream.                    *)
  (*                                                                       *)
  (*    The meaning of the `attach' action (i.e., what really happens when *)
  (*    the new file is read) is not fixed by FreeType itself.  It really  *)
  (*    depends on the font format (and thus the font driver).             *)
  (*                                                                       *)
  (*    Client applications are expected to know what they are doing       *)
  (*    when invoking this function.  Most drivers simply do not implement *)
  (*    file attachments.                                                  *)
  (*                                                                       *)
  function  FT_Attach_File(face : FT_Face; filepathname : PChar ) : FT_Error;
    cdecl; external ft_lib name 'FT_Attach_File';

  (*************************************************************************)
  (*                                                                       *)
  (* <Function>                                                            *)
  (*    FT_New_Memory_Face                                                 *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    Creates a new face object from a given resource and typeface index *)
  (*    using a font file already loaded into memory.                      *)
  (*                                                                       *)
  (* <InOut>                                                               *)
  (*    library    :: A handle to the library resource.                    *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    file_base  :: A pointer to the beginning of the font data.         *)
  (*                                                                       *)
  (*    file_size  :: The size of the memory chunk used by the font data.  *)
  (*                                                                       *)
  (*    face_index :: The index of the face within the resource.  The      *)
  (*                  first face has index 0.                              *)
  (*                                                                       *)
  (* <Output>                                                              *)
  (*    aface      :: A handle to a new face object.                       *)
  (*                                                                       *)
  (* <Return>                                                              *)
  (*    FreeType error code.  0 means success.                             *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    The font data bytes are used _directly_ by the @FT_Face object.    *)
  (*    This means that they are not copied, and that the client is        *)
  (*    responsible for releasing/destroying them _after_ the              *)
  (*    corresponding call to @FT_Done_Face .                              *)
  (*                                                                       *)
  (*    Unlike FreeType 1.x, this function automatically creates a glyph   *)
  (*    slot for the face object which can be accessed directly through    *)
  (*    `face->glyph'.                                                     *)
  (*                                                                       *)
  (*    @FT_New_Memory_Face can be used to determine and/or check the font *)
  (*    format of a given font resource.  If the `face_index' field is     *)
  (*    negative, the function will _not_ return any face handle in        *)
  (*    `aface'; the return value is 0 if the font format is recognized,   *)
  (*    or non-zero otherwise.                                             *)
  (*                                                                       *)
  function  FT_New_Memory_Face(
            library_ : FT_Library;
            file_base : PFT_Byte;
            file_size ,
            face_index : FT_Long;
            out aface :  FT_Face ) : FT_Error;
    cdecl; external ft_lib name 'FT_New_Memory_Face';

  (*************************************************************************)
  (*                                                                       *)
  (* <Function>                                                            *)
  (*    FT_New_Face                                                        *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    Creates a new face object from a given resource and typeface index *)
  (*    using a pathname to the font file.                                 *)
  (*                                                                       *)
  (* <InOut>                                                               *)
  (*    library    :: A handle to the library resource.                    *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    pathname   :: A path to the font file.                             *)
  (*                                                                       *)
  (*    face_index :: The index of the face within the resource.  The      *)
  (*                  first face has index 0.                              *)
  (*                                                                       *)
  (* <Output>                                                              *)
  (*    aface      :: A handle to a new face object.                       *)
  (*                                                                       *)
  (* <Return>                                                              *)
  (*    FreeType error code.  0 means success.                             *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    Unlike FreeType 1.x, this function automatically creates a glyph   *)
  (*    slot for the face object which can be accessed directly through    *)
  (*    `face->glyph'.                                                     *)
  (*                                                                       *)
  (*    @FT_New_Face can be used to determine and/or check the font format *)
  (*    of a given font resource.  If the `face_index' field is negative,  *)
  (*    the function will _not_ return any face handle in `aface';  the    *)
  (*    return value is 0 if the font format is recognized, or non-zero    *)
  (*    otherwise.                                                         *)
  (*                                                                       *)
  (*    Each new face object created with this function also owns a        *)
  (*    default @FT_Size object, accessible as `face->size'.               *)
  (*                                                                       *)
  function  FT_New_Face(
            library_ : FT_Library;
            filepathname : PChar;
            face_index : FT_Long;
            out aface : FT_Face ) : FT_Error;
    cdecl; external ft_lib name 'FT_New_Face';

  (*************************************************************************)
  (*                                                                       *)
  (* <Function>                                                            *)
  (*    FT_Done_Face                                                       *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    Discards a given face object, as well as all of its child slots    *)
  (*    and sizes.                                                         *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    face :: A handle to a target face object.                          *)
  (*                                                                       *)
  (* <Return>                                                              *)
  (*    FreeType error code.  0 means success.                             *)
  (*                                                                       *)
  function  FT_Done_Face(face : FT_Face ) : FT_Error;
    cdecl; external ft_lib name 'FT_Done_Face';

  (*************************************************************************)
  (*                                                                       *)
  (* <Function>                                                            *)
  (*    FT_Select_Charmap                                                  *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    Selects a given charmap by its encoding tag (as listed in          *)
  (*    `freetype.h').                                                     *)
  (*                                                                       *)
  (* <InOut>                                                               *)
  (*    face     :: A handle to the source face object.                    *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    encoding :: A handle to the selected charmap.                      *)
  (*                                                                       *)
  (* <Return>                                                              *)
  (*    FreeType error code.  0 means success.                             *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    This function will return an error if no charmap in the face       *)
  (*    corresponds to the encoding queried here.                          *)
  (*                                                                       *)
  function  FT_Select_Charmap(face : FT_Face; encoding : FT_Encoding ) : FT_Error;
    cdecl; external ft_lib name 'FT_Select_Charmap';

  (*************************************************************************)
  (*                                                                       *)
  (* <Function>                                                            *)
  (*    FT_Get_Char_Index                                                  *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    Returns the glyph index of a given character code.  This function  *)
  (*    uses a charmap object to do the translation.                       *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    face     :: A handle to the source face object.                    *)
  (*                                                                       *)
  (*    charcode :: The character code.                                    *)
  (*                                                                       *)
  (* <Return>                                                              *)
  (*    The glyph index.  0 means `undefined character code'.              *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    FreeType computes its own glyph indices which are not necessarily  *)
  (*    the same as used in the font in case the font is based on glyph    *)
  (*    indices.  Reason for this behaviour is to assure that index 0 is   *)
  (*    never used, representing the missing glyph.                        *)
  (*                                                                       *)
  function  FT_Get_Char_Index(face : FT_Face; charcode : FT_ULong ) : FT_UInt;
    cdecl; external ft_lib name 'FT_Get_Char_Index';

  (*************************************************************************)
  (*                                                                       *)
  (* <Function>                                                            *)
  (*    FT_Load_Glyph                                                      *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A function used to load a single glyph within a given glyph slot,  *)
  (*    for a given size.                                                  *)
  (*                                                                       *)
  (* <InOut>                                                               *)
  (*    face        :: A handle to the target face object where the glyph  *)
  (*                   will be loaded.                                     *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    glyph_index :: The index of the glyph in the font file.  For       *)
  (*                   CID-keyed fonts (either in PS or in CFF format)     *)
  (*                   this argument specifies the CID value.              *)
  (*                                                                       *)
  (*    load_flags  :: A flag indicating what to load for this glyph.  The *)
  (*                   @FT_LOAD_XXX constants can be used to control the   *)
  (*                   glyph loading process (e.g., whether the outline    *)
  (*                   should be scaled, whether to load bitmaps or not,   *)
  (*                   whether to hint the outline, etc).                  *)
  (*                                                                       *)
  (* <Return>                                                              *)
  (*    FreeType error code.  0 means success.                             *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    If the glyph image is not a bitmap, and if the bit flag            *)
  (*    FT_LOAD_IGNORE_TRANSFORM is unset, the glyph image will be         *)
  (*    transformed with the information passed to a previous call to      *)
  (*    @FT_Set_Transform.                                                 *)
  (*                                                                       *)
  (*    Note that this also transforms the `face.glyph.advance' field, but *)
  (*    *not* the values in `face.glyph.metrics'.                          *)
  (*                                                                       *)
  function  FT_Load_Glyph(
            face : FT_Face;
            glyph_index : FT_UInt ;
            load_flags : FT_Int32 ) : FT_Error;
    cdecl; external ft_lib name 'FT_Load_Glyph';


  (*************************************************************************)
  (*                                                                       *)
  (* <Function>                                                            *)
  (*    FT_Render_Glyph                                                    *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    Converts a given glyph image to a bitmap.  It does so by           *)
  (*    inspecting the glyph image format, find the relevant renderer, and *)
  (*    invoke it.                                                         *)
  (*                                                                       *)
  (* <InOut>                                                               *)
  (*    slot        :: A handle to the glyph slot containing the image to  *)
  (*                   convert.                                            *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    render_mode :: This is the render mode used to render the glyph    *)
  (*                   image into a bitmap.  See FT_Render_Mode for a list *)
  (*                   of possible values.                                 *)
  (*                                                                       *)
  (* <Return>                                                              *)
  (*    FreeType error code.  0 means success.                             *)
  (*                                                                       *)
  function  FT_Render_Glyph(slot : FT_GlyphSlot; render_mode : FT_Render_Mode ) : FT_Error;
    cdecl; external ft_lib name 'FT_Render_Glyph';

  (*************************************************************************)
  (*                                                                       *)
  (* <Enum>                                                                *)
  (*    FT_Kerning_Mode                                                    *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    An enumeration used to specify which kerning values to return in   *)
  (*    @FT_Get_Kerning.                                                   *)
  (*                                                                       *)
  (* <Values>                                                              *)
  (*    FT_KERNING_DEFAULT  :: Return scaled and grid-fitted kerning       *)
  (*                           distances (value is 0).                     *)
  (*                                                                       *)
  (*    FT_KERNING_UNFITTED :: Return scaled but un-grid-fitted kerning    *)
  (*                           distances.                                  *)
  (*                                                                       *)
  (*    FT_KERNING_UNSCALED :: Return the kerning vector in original font  *)
  (*                           units.                                      *)
  (*                                                                       *)
const
  FT_KERNING_DEFAULT  = 0;
  FT_KERNING_UNFITTED = 1;
  FT_KERNING_UNSCALED = 2;


  (*************************************************************************)
  (*                                                                       *)
  (* <Function>                                                            *)
  (*    FT_Get_Kerning                                                     *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    Returns the kerning vector between two glyphs of a same face.      *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    face        :: A handle to a source face object.                   *)
  (*                                                                       *)
  (*    left_glyph  :: The index of the left glyph in the kern pair.       *)
  (*                                                                       *)
  (*    right_glyph :: The index of the right glyph in the kern pair.      *)
  (*                                                                       *)
  (*    kern_mode   :: See @FT_Kerning_Mode for more information.          *)
  (*                   Determines the scale/dimension of the returned      *)
  (*                   kerning vector.                                     *)
  (*                                                                       *)
  (* <Output>                                                              *)
  (*    akerning    :: The kerning vector.  This is in font units for      *)
  (*                   scalable formats, and in pixels for fixed-sizes     *)
  (*                   formats.                                            *)
  (*                                                                       *)
  (* <Return>                                                              *)
  (*    FreeType error code.  0 means success.                             *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    Only horizontal layouts (left-to-right & right-to-left) are        *)
  (*    supported by this method.  Other layouts, or more sophisticated    *)
  (*    kernings, are out of the scope of this API function -- they can be *)
  (*    implemented through format-specific interfaces.                    *)
  (*                                                                       *)
  function  FT_Get_Kerning(
            face : FT_Face;
            left_glyph ,
            right_glyph ,
            kern_mode : FT_UInt;
            out akerning : FT_Vector ) : FT_Error;
    cdecl; external ft_lib name 'FT_Get_Kerning';

  (*************************************************************************)
  (*                                                                       *)
  (* <Function>                                                            *)
  (*    FT_Set_Char_Size                                                   *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    Sets the character dimensions of a given face object.  The         *)
  (*    `char_width' and `char_height' values are used for the width and   *)
  (*    height, respectively, expressed in 26.6 fractional points.         *)
  (*                                                                       *)
  (*    If the horizontal or vertical resolution values are zero, a        *)
  (*    default value of 72dpi is used.  Similarly, if one of the          *)
  (*    character dimensions is zero, its value is set equal to the other. *)
  (*                                                                       *)
  (* <InOut>                                                               *)
  (*    face            :: A handle to a target face object.               *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    char_width      :: The character width, in 26.6 fractional points. *)
  (*                                                                       *)
  (*    char_height     :: The character height, in 26.6 fractional        *)
  (*                       points.                                         *)
  (*                                                                       *)
  (*    horz_resolution :: The horizontal resolution.                      *)
  (*                                                                       *)
  (*    vert_resolution :: The vertical resolution.                        *)
  (*                                                                       *)
  (* <Return>                                                              *)
  (*    FreeType error code.  0 means success.                             *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    When dealing with fixed-size faces (i.e., non-scalable formats),   *)
  (*    @FT_Set_Pixel_Sizes provides a more convenient interface.          *)
  (*                                                                       *)
  function  FT_Set_Char_Size(
            face : FT_Face;
            char_width ,
            char_height : FT_F26dot6;
            horz_res ,
            vert_res : FT_UInt) : FT_Error;
    cdecl; external ft_lib name 'FT_Set_Char_Size';

  (*************************************************************************)
  (*                                                                       *)
  (* <Function>                                                            *)
  (*    FT_Set_Pixel_Sizes                                                 *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    Sets the character dimensions of a given face object.  The width   *)
  (*    and height are expressed in integer pixels.                        *)
  (*                                                                       *)
  (*    If one of the character dimensions is zero, its value is set equal *)
  (*    to the other.                                                      *)
  (*                                                                       *)
  (* <InOut>                                                               *)
  (*    face         :: A handle to the target face object.                *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    pixel_width  :: The character width, in integer pixels.            *)
  (*                                                                       *)
  (*    pixel_height :: The character height, in integer pixels.           *)
  (*                                                                       *)
  (* <Return>                                                              *)
  (*    FreeType error code.  0 means success.                             *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    The values of `pixel_width' and `pixel_height' correspond to the   *)
  (*    pixel values of the _typographic_ character size, which are NOT    *)
  (*    necessarily the same as the dimensions of the glyph `bitmap        *)
  (*    cells'.                                                            *)
  (*                                                                       *)
  (*    The `character size' is really the size of an abstract square      *)
  (*    called the `EM', used to design the font.  However, depending      *)
  (*    on the font design, glyphs will be smaller or greater than the     *)
  (*    EM.                                                                *)
  (*                                                                       *)
  (*    This means that setting the pixel size to, say, 8x8 doesn't        *)
  (*    guarantee in any way that you will get glyph bitmaps that all fit  *)
  (*    within an 8x8 cell (sometimes even far from it).                   *)
  (*                                                                       *)
  (*    For bitmap fonts, `pixel_height' usually is a reliable value for   *)
  (*    the height of the bitmap cell.  Drivers for bitmap font formats    *)
  (*    which contain a single bitmap strike only (BDF, PCF, FNT) ignore   *)
  (*    `pixel_width'.                                                     *)
  (*                                                                       *)
  function  FT_Set_Pixel_Sizes(
            face : FT_Face;
            pixel_width ,
            pixel_height : FT_UInt ) : FT_Error;
    cdecl; external ft_lib name 'FT_Set_Pixel_Sizes';

  (*************************************************************************)
  (*                                                                       *)
  (* <Function>                                                            *)
  (*    FT_Get_Glyph                                                       *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A function used to extract a glyph image from a slot.              *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    slot   :: A handle to the source glyph slot.                       *)
  (*                                                                       *)
  (* <Output>                                                              *)
  (*    aglyph :: A handle to the glyph object.                            *)
  (*                                                                       *)
  (* <Return>                                                              *)
  (*    FreeType error code.  0 means success.                             *)
  (*                                                                       *)
  function FT_Get_Glyph(
             slot:   FT_GlyphSlot;
             out aglyph: FT_Glyph ): FT_Error;
    cdecl; external ft_lib name 'FT_Get_Glyph';

  (*************************************************************************)
  (*                                                                       *)
  (* <Enum>                                                                *)
  (*    FT_Glyph_BBox_Mode                                                 *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    The mode how the values of @FT_Glyph_Get_CBox are returned.        *)
  (*                                                                       *)
  (* <Values>                                                              *)
  (*    FT_GLYPH_BBOX_UNSCALED ::                                          *)
  (*      Return unscaled font units.                                      *)
  (*                                                                       *)
  (*    FT_GLYPH_BBOX_SUBPIXELS ::                                         *)
  (*      Return unfitted 26.6 coordinates.                                *)
  (*                                                                       *)
  (*    FT_GLYPH_BBOX_GRIDFIT ::                                           *)
  (*      Return grid-fitted 26.6 coordinates.                             *)
  (*                                                                       *)
  (*    FT_GLYPH_BBOX_TRUNCATE ::                                          *)
  (*      Return coordinates in integer pixels.                            *)
  (*                                                                       *)
  (*    FT_GLYPH_BBOX_PIXELS ::                                            *)
  (*      Return grid-fitted pixel coordinates.                            *)
  (*                                                                       *)
type
  FT_Glyph_BBox_Mode = FT_UInt;
const
  FT_GLYPH_BBOX_UNSCALED  = 0;
  FT_GLYPH_BBOX_SUBPIXELS = 0;
  FT_GLYPH_BBOX_GRIDFIT   = 1;
  FT_GLYPH_BBOX_TRUNCATE  = 2;
  FT_GLYPH_BBOX_PIXELS    = 3;

  (*************************************************************************)
  (*                                                                       *)
  (* <Function>                                                            *)
  (*    FT_Glyph_Get_CBox                                                  *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    Return a glyph's `control box'.  The control box encloses all the  *)
  (*    outline's points, including Bézier control points.  Though it      *)
  (*    coincides with the exact bounding box for most glyphs, it can be   *)
  (*    slightly larger in some situations (like when rotating an outline  *)
  (*    which contains Bézier outside arcs).                               *)
  (*                                                                       *)
  (*    Computing the control box is very fast, while getting the bounding *)
  (*    box can take much more time as it needs to walk over all segments  *)
  (*    and arcs in the outline.  To get the latter, you can use the       *)
  (*    `ftbbox' component which is dedicated to this single task.         *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    glyph :: A handle to the source glyph object.                      *)
  (*                                                                       *)
  (*    mode  :: The mode which indicates how to interpret the returned    *)
  (*             bounding box values.                                      *)
  (*                                                                       *)
  (* <Output>                                                              *)
  (*    acbox :: The glyph coordinate bounding box.  Coordinates are       *)
  (*             expressed in 1/64th of pixels if it is grid-fitted.       *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    Coordinates are relative to the glyph origin, using the Y-upwards  *)
  (*    convention.                                                        *)
  (*                                                                       *)
  (*    If the glyph has been loaded with @FT_LOAD_NO_SCALE, `bbox_mode'   *)
  (*    must be set to @FT_GLYPH_BBOX_UNSCALED to get unscaled font        *)
  (*    units in 26.6 pixel format.  The value @FT_GLYPH_BBOX_SUBPIXELS    *)
  (*    is another name for this constant.                                 *)
  (*                                                                       *)
  (*    Note that the maximum coordinates are exclusive, which means that  *)
  (*    one can compute the width and height of the glyph image (be it in  *)
  (*    integer or 26.6 pixels) as:                                        *)
  (*                                                                       *)
  (*    {                                                                  *)
  (*      width  = bbox.xMax - bbox.xMin;                                  *)
  (*      height = bbox.yMax - bbox.yMin;                                  *)
  (*    }                                                                  *)
  (*                                                                       *)
  (*    Note also that for 26.6 coordinates, if `bbox_mode' is set to      *)
  (*    @FT_GLYPH_BBOX_GRIDFIT, the coordinates will also be grid-fitted,  *)
  (*    which corresponds to:                                              *)
  (*                                                                       *)
  (*    {                                                                  *)
  (*      bbox.xMin = FLOOR(bbox.xMin);                                    *)
  (*      bbox.yMin = FLOOR(bbox.yMin);                                    *)
  (*      bbox.xMax = CEILING(bbox.xMax);                                  *)
  (*      bbox.yMax = CEILING(bbox.yMax);                                  *)
  (*    }                                                                  *)
  (*                                                                       *)
  (*    To get the bbox in pixel coordinates, set `bbox_mode' to           *)
  (*    @FT_GLYPH_BBOX_TRUNCATE.                                           *)
  (*                                                                       *)
  (*    To get the bbox in grid-fitted pixel coordinates, set `bbox_mode'  *)
  (*    to @FT_GLYPH_BBOX_PIXELS.                                          *)
  (*                                                                       *)
  procedure FT_Glyph_Get_CBox( glyph: FT_Glyph;
                     bbox_mode: FT_UInt;
                     out acbox: FT_BBox );
    cdecl; external ft_lib name 'FT_Glyph_Get_CBox';

  (*************************************************************************)
  (*                                                                       *)
  (* <Function>                                                            *)
  (*    FT_Glyph_To_Bitmap                                                 *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    Converts a given glyph object to a bitmap glyph object.            *)
  (*                                                                       *)
  (* <InOut>                                                               *)
  (*    the_glyph   :: A pointer to a handle to the target glyph.          *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    render_mode :: An enumeration that describe how the data is        *)
  (*                   rendered.                                           *)
  (*                                                                       *)
  (*    origin      :: A pointer to a vector used to translate the glyph   *)
  (*                   image before rendering.  Can be 0 (if no            *)
  (*                   translation).  The origin is expressed in           *)
  (*                   26.6 pixels.                                        *)
  (*                                                                       *)
  (*    destroy     :: A boolean that indicates that the original glyph    *)
  (*                   image should be destroyed by this function.  It is  *)
  (*                   never destroyed in case of error.                   *)
  (*                                                                       *)
  (* <Return>                                                              *)
  (*    FreeType error code.  0 means success.                             *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    The glyph image is translated with the `origin' vector before      *)
  (*    rendering.                                                         *)
  (*                                                                       *)
  (*    The first parameter is a pointer to a FT_Glyph handle, that will   *)
  (*    be replaced by this function.  Typically, you would use (omitting  *)
  (*    error handling):                                                   *)
  (*                                                                       *)
  (*                                                                       *)
  (*      {                                                                *)
  (*        FT_Glyph        glyph;                                         *)
  (*        FT_BitmapGlyph  glyph_bitmap;                                  *)
  (*                                                                       *)
  (*                                                                       *)
  (*        // load glyph                                                  *)
  (*        error = FT_Load_Char( face, glyph_index, FT_LOAD_DEFAUT );     *)
  (*                                                                       *)
  (*        // extract glyph image                                         *)
  (*        error = FT_Get_Glyph( face->glyph, &glyph );                   *)
  (*                                                                       *)
  (*        // convert to a bitmap (default render mode + destroy old)     *)
  (*        if ( glyph->format != FT_GLYPH_FORMAT_BITMAP )                 *)
  (*        {                                                              *)
  (*          error = FT_Glyph_To_Bitmap( &glyph, FT_RENDER_MODE_DEFAULT,  *)
  (*                                      0, 1 );                          *)
  (*          if ( error ) // glyph unchanged                              *)
  (*            ...                                                        *)
  (*        }                                                              *)
  (*                                                                       *)
  (*        // access bitmap content by typecasting                        *)
  (*        glyph_bitmap = (FT_BitmapGlyph)glyph;                          *)
  (*                                                                       *)
  (*        // do funny stuff with it, like blitting/drawing               *)
  (*        ...                                                            *)
  (*                                                                       *)
  (*        // discard glyph image (bitmap or not)                         *)
  (*        FT_Done_Glyph( glyph );                                        *)
  (*      }                                                                *)
  (*                                                                       *)
  (*                                                                       *)
  (*    This function does nothing if the glyph format isn't scalable.     *)
  (*                                                                       *)
  function FT_Glyph_To_Bitmap(var the_glyph: FT_Glyph;
                      render_mode: FT_Render_Mode;
                      origin:      PFT_Vector;
                      destroy:     FT_Bool ): FT_Error;
    cdecl; external ft_lib name 'FT_Glyph_To_Bitmap';

  (*************************************************************************)
  (*                                                                       *)
  (* <Function>                                                            *)
  (*    FT_Done_Glyph                                                      *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    Destroys a given glyph.                                            *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    glyph :: A handle to the target glyph object.                      *)
  (*                                                                       *)
  procedure FT_Done_Glyph( glyph: FT_Glyph );
    cdecl; external ft_lib name 'FT_Done_Glyph';

implementation


{ FT_CURVE_TAG }
function FT_CURVE_TAG(flag: byte): byte;
begin
  result := flag and 3;
end;

{ FT_IS_SCALABLE }
function FT_IS_SCALABLE(face : FT_Face ) : cbool;
begin
  result := cbool(face.face_flags and FT_FACE_FLAG_SCALABLE );
end;

{ FT_HAS_KERNING }
function FT_HAS_KERNING(face : FT_Face ) : cbool;
begin
  result := cbool(face.face_flags and FT_FACE_FLAG_KERNING );
end;

end.

