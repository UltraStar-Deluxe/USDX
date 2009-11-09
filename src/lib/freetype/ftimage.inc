(***************************************************************************)
(*                                                                         *)
(*  ftimage.h                                                              *)
(*                                                                         *)
(*    FreeType glyph image formats and default raster interface            *)
(*    (specification).                                                     *)
(*                                                                         *)
(*  Copyright 1996-2001, 2002, 2003, 2004, 2005, 2006, 2007 by             *)
(*  David Turner, Robert Wilhelm, and Werner Lemberg.                      *)
(*                                                                         *)
(*  This file is part of the FreeType project, and may only be used,       *)
(*  modified, and distributed under the terms of the FreeType project      *)
(*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     *)
(*  this file you indicate that you have read the license and              *)
(*  understand and accept it fully.                                        *)
(*                                                                         *)
(***************************************************************************)
(***************************************************************************)
(* Pascal port by the UltraStar Deluxe Team                                *)
(***************************************************************************)

  (*************************************************************************)
  (*                                                                       *)
  (* Note: A `raster' is simply a scan-line converter, used to render      *)
  (*       FT_Outlines into FT_Bitmaps.                                    *)
  (*                                                                       *)
  (*************************************************************************)

{$IFDEF TYPE_DECL}
  
  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_Pos                                                             *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    The type FT_Pos is a 32-bit integer used to store vectorial        *)
  (*    coordinates.  Depending on the context, these can represent        *)
  (*    distances in integer font units, or 16,16, or 26.6 fixed float     *)
  (*    pixel coordinates.                                                 *)
  (*                                                                       *)
  FT_Pos     = cslong;


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

  PFT_VectorArray = ^FT_VectorArray;
  FT_VectorArray = array[0 .. (MaxInt div SizeOf(FT_Vector))-1] of FT_Vector;

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
  FT_Pixel_Mode = cint;
{$ELSE TYPE_DECL}
const
  FT_PIXEL_MODE_NONE    = 0;
  FT_PIXEL_MODE_MONO    = FT_PIXEL_MODE_NONE  + 1;
  FT_PIXEL_MODE_GRAY    = FT_PIXEL_MODE_MONO  + 1;
  FT_PIXEL_MODE_GRAY2   = FT_PIXEL_MODE_GRAY  + 1;
  FT_PIXEL_MODE_GRAY4   = FT_PIXEL_MODE_GRAY2 + 1;
  FT_PIXEL_MODE_LCD     = FT_PIXEL_MODE_GRAY4 + 1;
  FT_PIXEL_MODE_LCD_V   = FT_PIXEL_MODE_LCD   + 1;

  FT_PIXEL_MODE_MAX     = FT_PIXEL_MODE_LCD_V + 1; (* do not remove *)
{$ENDIF TYPE_DECL}
{$IFDEF TYPE_DECL}

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
    rows:         FT_Int;
    width:        FT_Int;
    pitch:        FT_Int;
    buffer:       PByteArray;
    num_grays:    FT_Short;
    pixel_mode:   byte;
    palette_mode: byte;
    palette:      pointer;
  end;


  (*************************************************************************)
  (*                                                                       *)
  (* <Section>                                                             *)
  (*    outline_processing                                                 *)
  (*                                                                       *)
  (*************************************************************************)


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
    n_contours: FT_Short;        (* number of contours in glyph        *)
    n_points:   FT_Short;        (* number of points in the glyph      *)

    points:     PFT_VectorArray; (* the outline's points               *)
    tags:       PByteArray;      (* the points flags                   *)
    contours:   PFT_ShortArray;  (* the contour end points             *)

    flags:      FT_Int;          (* outline masks                      *)
  end;

{$ELSE TYPE_DECL}

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

  FT_CURVE_TAG_TOUCH_X   =   8;  // reserved for the TrueType hinter
  FT_CURVE_TAG_TOUCH_Y   =  16;  // reserved for the TrueType hinter

  FT_CURVE_TAG_TOUCH_BOTH  = ( FT_CURVE_TAG_TOUCH_X or
                               FT_CURVE_TAG_TOUCH_Y );
{$ENDIF TYPE_DECL}
{$IFDEF TYPE_DECL}

  (*************************************************************************)
  (*                                                                       *)
  (* <FuncType>                                                            *)
  (*    FT_Outline_MoveToFunc                                              *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A function pointer type used to describe the signature of a `move  *)
  (*    to' function during outline walking/decomposition.                 *)
  (*                                                                       *)
  (*    A `move to' is emitted to start a new contour in an outline.       *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    to   :: A pointer to the target point of the `move to'.            *)
  (*                                                                       *)
  (*    user :: A typeless pointer which is passed from the caller of the  *)
  (*            decomposition function.                                    *)
  (*                                                                       *)
  (* <Return>                                                              *)
  (*    Error code.  0 means success.                                      *)
  (*                                                                       *)
  FT_Outline_MoveToFunc = function(to_: {const} PFT_Vector;
                                   user: Pointer): cint; cdecl;


  (*************************************************************************)
  (*                                                                       *)
  (* <FuncType>                                                            *)
  (*    FT_Outline_LineToFunc                                              *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A function pointer type used to describe the signature of a `line  *)
  (*    to' function during outline walking/decomposition.                 *)
  (*                                                                       *)
  (*    A `line to' is emitted to indicate a segment in the outline.       *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    to   :: A pointer to the target point of the `line to'.            *)
  (*                                                                       *)
  (*    user :: A typeless pointer which is passed from the caller of the  *)
  (*            decomposition function.                                    *)
  (*                                                                       *)
  (* <Return>                                                              *)
  (*    Error code.  0 means success.                                      *)
  (*                                                                       *)
  FT_Outline_LineToFunc = function(to_: {const} PFT_Vector;
                                   user: Pointer): cint; cdecl;


  (*************************************************************************)
  (*                                                                       *)
  (* <FuncType>                                                            *)
  (*    FT_Outline_ConicToFunc                                             *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A function pointer type use to describe the signature of a `conic  *)
  (*    to' function during outline walking/decomposition.                 *)
  (*                                                                       *)
  (*    A `conic to' is emitted to indicate a second-order Bézier arc in   *)
  (*    the outline.                                                       *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    control :: An intermediate control point between the last position *)
  (*               and the new target in `to'.                             *)
  (*                                                                       *)
  (*    to      :: A pointer to the target end point of the conic arc.     *)
  (*                                                                       *)
  (*    user    :: A typeless pointer which is passed from the caller of   *)
  (*               the decomposition function.                             *)
  (*                                                                       *)
  (* <Return>                                                              *)
  (*    Error code.  0 means success.                                      *)
  (*                                                                       *)
  FT_Outline_ConicToFunc = function(control: {const} PFT_Vector;
                             to_:  {const} PFT_Vector;
                             user: Pointer): cint; cdecl;


  (*************************************************************************)
  (*                                                                       *)
  (* <FuncType>                                                            *)
  (*    FT_Outline_CubicToFunc                                             *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A function pointer type used to describe the signature of a `cubic *)
  (*    to' function during outline walking/decomposition.                 *)
  (*                                                                       *)
  (*    A `cubic to' is emitted to indicate a third-order Bézier arc.      *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    control1 :: A pointer to the first Bézier control point.           *)
  (*                                                                       *)
  (*    control2 :: A pointer to the second Bézier control point.          *)
  (*                                                                       *)
  (*    to       :: A pointer to the target end point.                     *)
  (*                                                                       *)
  (*    user     :: A typeless pointer which is passed from the caller of  *)
  (*                the decomposition function.                            *)
  (*                                                                       *)
  (* <Return>                                                              *)
  (*    Error code.  0 means success.                                      *)
  (*                                                                       *)
  FT_Outline_CubicToFunc = function( control1: {const} PFT_Vector;
                             control2: {const} PFT_Vector;
                             to_:      {const} PFT_Vector;
                             user:     Pointer ): cint; cdecl;


  (*************************************************************************)
  (*                                                                       *)
  (* <Struct>                                                              *)
  (*    FT_Outline_Funcs                                                   *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A structure to hold various function pointers used during outline  *)
  (*    decomposition in order to emit segments, conic, and cubic Béziers, *)
  (*    as well as `move to' and `close to' operations.                    *)
  (*                                                                       *)
  (* <Fields>                                                              *)
  (*    move_to  :: The `move to' emitter.                                 *)
  (*                                                                       *)
  (*    line_to  :: The segment emitter.                                   *)
  (*                                                                       *)
  (*    conic_to :: The second-order Bézier arc emitter.                   *)
  (*                                                                       *)
  (*    cubic_to :: The third-order Bézier arc emitter.                    *)
  (*                                                                       *)
  (*    shift    :: The shift that is applied to coordinates before they   *)
  (*                are sent to the emitter.                               *)
  (*                                                                       *)
  (*    delta    :: The delta that is applied to coordinates before they   *)
  (*                are sent to the emitter, but after the shift.          *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    The point coordinates sent to the emitters are the transformed     *)
  (*    version of the original coordinates (this is important for high    *)
  (*    accuracy during scan-conversion).  The transformation is simple:   *)
  (*                                                                       *)
  (*    {                                                                  *)
  (*      x' = (x << shift) - delta                                        *)
  (*      y' = (x << shift) - delta                                        *)
  (*    }                                                                  *)
  (*                                                                       *)
  (*    Set the value of `shift' and `delta' to 0 to get the original      *)
  (*    point coordinates.                                                 *)
  (*                                                                       *)
  PFT_Outline_Funcs = ^FT_Outline_Funcs;
  FT_Outline_Funcs = record
    move_to:  FT_Outline_MoveToFunc;
    line_to:  FT_Outline_LineToFunc;
    conic_to: FT_Outline_ConicToFunc;
    cubic_to: FT_Outline_CubicToFunc;

    shift:    cint;
    delta:    FT_Pos;
  end;


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
  FT_Glyph_Format = array[0..3] of char;
{$ELSE TYPE_DECL}
const
  FT_GLYPH_FORMAT_NONE:      FT_Glyph_Format = (#0, #0, #0, #0 );

  FT_GLYPH_FORMAT_COMPOSITE: FT_Glyph_Format = ('c', 'o', 'm', 'p' );
  FT_GLYPH_FORMAT_BITMAP:    FT_Glyph_Format = ('b', 'i', 't', 's' );
  FT_GLYPH_FORMAT_OUTLINE:   FT_Glyph_Format = ('o', 'u', 't', 'l' );
  FT_GLYPH_FORMAT_PLOTTER:   FT_Glyph_Format = ('p', 'l', 'o', 't' );

{$ENDIF TYPE_DECL}

  (*************************************************************************)
  (*************************************************************************)
  (*************************************************************************)
  (*****                                                               *****)
  (*****            R A S T E R   D E F I N I T I O N S                *****)
  (*****                                                               *****)
  (*************************************************************************)
  (*************************************************************************)
  (*************************************************************************)


  (*************************************************************************)
  (*                                                                       *)
  (* A raster is a scan converter, in charge of rendering an outline into  *)
  (* a a bitmap.  This section contains the public API for rasters.        *)
  (*                                                                       *)
  (* Note that in FreeType 2, all rasters are now encapsulated within      *)
  (* specific modules called `renderers'.  See `freetype/ftrender.h' for   *)
  (* more details on renderers.                                            *)
  (*                                                                       *)
  (*************************************************************************)


  (*************************************************************************)
  (*                                                                       *)
  (* <Section>                                                             *)
  (*    raster                                                             *)
  (*                                                                       *)
  (* <Title>                                                               *)
  (*    Scanline Converter                                                 *)
  (*                                                                       *)
  (* <Abstract>                                                            *)
  (*    How vectorial outlines are converted into bitmaps and pixmaps.     *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    This section contains technical definitions.                       *)
  (*                                                                       *)
  (*************************************************************************)

{$IFDEF TYPE_DECL}

  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_Raster                                                          *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A handle (pointer) to a raster object.  Each object can be used    *)
  (*    independently to convert an outline into a bitmap or pixmap.       *)
  (*                                                                       *)
  FT_Raster = Pointer;


  (*************************************************************************)
  (*                                                                       *)
  (* <Struct>                                                              *)
  (*    FT_Span                                                            *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A structure used to model a single span of gray (or black) pixels  *)
  (*    when rendering a monochrome or anti-aliased bitmap.                *)
  (*                                                                       *)
  (* <Fields>                                                              *)
  (*    x        :: The span's horizontal start position.                  *)
  (*                                                                       *)
  (*    len      :: The span's length in pixels.                           *)
  (*                                                                       *)
  (*    coverage :: The span color/coverage, ranging from 0 (background)   *)
  (*                to 255 (foreground).  Only used for anti-aliased       *)
  (*                rendering.                                             *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    This structure is used by the span drawing callback type named     *)
  (*    @FT_SpanFunc which takes the y-coordinate of the span as a         *)
  (*    a parameter.                                                       *)
  (*                                                                       *)
  (*    The coverage value is always between 0 and 255.                    *)
  (*                                                                       *)
  PFT_Span = ^FT_Span;
  FT_Span = record
    x:         cshort;
    len:       cushort;
    coverage:  cuchar;
  end;


  (*************************************************************************)
  (*                                                                       *)
  (* <FuncType>                                                            *)
  (*    FT_SpanFunc                                                        *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A function used as a call-back by the anti-aliased renderer in     *)
  (*    order to let client applications draw themselves the gray pixel    *)
  (*    spans on each scan line.                                           *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    y     :: The scanline's y-coordinate.                              *)
  (*                                                                       *)
  (*    count :: The number of spans to draw on this scanline.             *)
  (*                                                                       *)
  (*    spans :: A table of `count' spans to draw on the scanline.         *)
  (*                                                                       *)
  (*    user  :: User-supplied data that is passed to the callback.        *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    This callback allows client applications to directly render the    *)
  (*    gray spans of the anti-aliased bitmap to any kind of surfaces.     *)
  (*                                                                       *)
  (*    This can be used to write anti-aliased outlines directly to a      *)
  (*    given background bitmap, and even perform translucency.            *)
  (*                                                                       *)
  (*    Note that the `count' field cannot be greater than a fixed value   *)
  (*    defined by the `FT_MAX_GRAY_SPANS' configuration macro in          *)
  (*    `ftoption.h'.  By default, this value is set to 32, which means    *)
  (*    that if there are more than 32 spans on a given scanline, the      *)
  (*    callback is called several times with the same `y' parameter in    *)
  (*    order to draw all callbacks.                                       *)
  (*                                                                       *)
  (*    Otherwise, the callback is only called once per scan-line, and     *)
  (*    only for those scanlines that do have `gray' pixels on them.       *)
  (*                                                                       *)
  FT_SpanFunc = procedure(y:      cint;
                  count:          cint;
                  spans:          {const} PFT_Span;
                  user:           Pointer ); cdecl;


  (*************************************************************************)
  (*                                                                       *)
  (* <FuncType>                                                            *)
  (*    FT_Raster_BitTest_Func                                             *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    THIS TYPE IS DEPRECATED.  DO NOT USE IT.                           *)
  (*                                                                       *)
  (*    A function used as a call-back by the monochrome scan-converter    *)
  (*    to test whether a given target pixel is already set to the drawing *)
  (*    `color'.  These tests are crucial to implement drop-out control    *)
  (*    per-se the TrueType spec.                                          *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    y     :: The pixel's y-coordinate.                                 *)
  (*                                                                       *)
  (*    x     :: The pixel's x-coordinate.                                 *)
  (*                                                                       *)
  (*    user  :: User-supplied data that is passed to the callback.        *)
  (*                                                                       *)
  (* <Return>                                                              *)
  (*   1 if the pixel is `set', 0 otherwise.                               *)
  (*                                                                       *)
  FT_Raster_BitTest_Func = function(y: cint;
                             x:    cint;
                             user: Pointer): cint; cdecl;


  (*************************************************************************)
  (*                                                                       *)
  (* <FuncType>                                                            *)
  (*    FT_Raster_BitSet_Func                                              *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    THIS TYPE IS DEPRECATED.  DO NOT USE IT.                           *)
  (*                                                                       *)
  (*    A function used as a call-back by the monochrome scan-converter    *)
  (*    to set an individual target pixel.  This is crucial to implement   *)
  (*    drop-out control according to the TrueType specification.          *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    y     :: The pixel's y-coordinate.                                 *)
  (*                                                                       *)
  (*    x     :: The pixel's x-coordinate.                                 *)
  (*                                                                       *)
  (*    user  :: User-supplied data that is passed to the callback.        *)
  (*                                                                       *)
  (* <Return>                                                              *)
  (*    1 if the pixel is `set', 0 otherwise.                              *)
  (*                                                                       *)
  FT_Raster_BitSet_Func = procedure(y: cint;
                            x:    cint;
                            user: Pointer ); cdecl;


  (*************************************************************************)
  (*                                                                       *)
  (* <Enum>                                                                *)
  (*    FT_RASTER_FLAG_XXX                                                 *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A list of bit flag constants as used in the `flags' field of a     *)
  (*    @FT_Raster_Params structure.                                       *)
  (*                                                                       *)
  (* <Values>                                                              *)
  (*    FT_RASTER_FLAG_DEFAULT :: This value is 0.                         *)
  (*                                                                       *)
  (*    FT_RASTER_FLAG_AA      :: This flag is set to indicate that an     *)
  (*                              anti-aliased glyph image should be       *)
  (*                              generated.  Otherwise, it will be        *)
  (*                              monochrome (1-bit).                      *)
  (*                                                                       *)
  (*    FT_RASTER_FLAG_DIRECT  :: This flag is set to indicate direct      *)
  (*                              rendering.  In this mode, client         *)
  (*                              applications must provide their own span *)
  (*                              callback.  This lets them directly       *)
  (*                              draw or compose over an existing bitmap. *)
  (*                              If this bit is not set, the target       *)
  (*                              pixmap's buffer _must_ be zeroed before  *)
  (*                              rendering.                               *)
  (*                                                                       *)
  (*                              Note that for now, direct rendering is   *)
  (*                              only possible with anti-aliased glyphs.  *)
  (*                                                                       *)
  (*    FT_RASTER_FLAG_CLIP    :: This flag is only used in direct         *)
  (*                              rendering mode.  If set, the output will *)
  (*                              be clipped to a box specified in the     *)
  (*                              `clip_box' field of the                  *)
  (*                              @FT_Raster_Params structure.             *)
  (*                                                                       *)
  (*                              Note that by default, the glyph bitmap   *)
  (*                              is clipped to the target pixmap, except  *)
  (*                              in direct rendering mode where all spans *)
  (*                              are generated if no clipping box is set. *)
  (*                                                                       *)
{$ELSE TYPE_DECL}
const
  FT_RASTER_FLAG_DEFAULT  = $0;
  FT_RASTER_FLAG_AA       = $1;
  FT_RASTER_FLAG_DIRECT   = $2;
  FT_RASTER_FLAG_CLIP     = $4;

{$ENDIF TYPE_DECL}
{$IFDEF TYPE_DECL}

  (*************************************************************************)
  (*                                                                       *)
  (* <Struct>                                                              *)
  (*    FT_Raster_Params                                                   *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A structure to hold the arguments used by a raster's render        *)
  (*    function.                                                          *)
  (*                                                                       *)
  (* <Fields>                                                              *)
  (*    target      :: The target bitmap.                                  *)
  (*                                                                       *)
  (*    source      :: A pointer to the source glyph image (e.g., an       *)
  (*                   @FT_Outline).                                       *)
  (*                                                                       *)
  (*    flags       :: The rendering flags.                                *)
  (*                                                                       *)
  (*    gray_spans  :: The gray span drawing callback.                     *)
  (*                                                                       *)
  (*    black_spans :: The black span drawing callback.                    *)
  (*                                                                       *)
  (*    bit_test    :: The bit test callback.  UNIMPLEMENTED!              *)
  (*                                                                       *)
  (*    bit_set     :: The bit set callback.  UNIMPLEMENTED!               *)
  (*                                                                       *)
  (*    user        :: User-supplied data that is passed to each drawing   *)
  (*                   callback.                                           *)
  (*                                                                       *)
  (*    clip_box    :: An optional clipping box.  It is only used in       *)
  (*                   direct rendering mode.  Note that coordinates here  *)
  (*                   should be expressed in _integer_ pixels (and not in *)
  (*                   26.6 fixed-point units).                            *)
  (*                                                                       *)
  (* <Note>                                                                *)
  (*    An anti-aliased glyph bitmap is drawn if the @FT_RASTER_FLAG_AA    *)
  (*    bit flag is set in the `flags' field, otherwise a monochrome       *)
  (*    bitmap is generated.                                               *)
  (*                                                                       *)
  (*    If the @FT_RASTER_FLAG_DIRECT bit flag is set in `flags', the      *)
  (*    raster will call the `gray_spans' callback to draw gray pixel      *)
  (*    spans, in the case of an aa glyph bitmap, it will call             *)
  (*    `black_spans', and `bit_test' and `bit_set' in the case of a       *)
  (*    monochrome bitmap.  This allows direct composition over a          *)
  (*    pre-existing bitmap through user-provided callbacks to perform the *)
  (*    span drawing/composition.                                          *)
  (*                                                                       *)
  (*    Note that the `bit_test' and `bit_set' callbacks are required when *)
  (*    rendering a monochrome bitmap, as they are crucial to implement    *)
  (*    correct drop-out control as defined in the TrueType specification. *)
  (*                                                                       *)
  PFT_Raster_Params = ^FT_Raster_Params;
  FT_Raster_Params = record
    target:        {const} PFT_Bitmap;
    source:        {const} Pointer;
    flags:         cint;
    gray_spans:    FT_SpanFunc;
    black_spans:   FT_SpanFunc;
    bit_test:      FT_Raster_BitTest_Func;     (* doesn't work! *)
    bit_set:       FT_Raster_BitSet_Func;      (* doesn't work! *)
    user:          Pointer;
    clip_box:      FT_BBox;
  end;

{$ENDIF TYPE_DECL}


