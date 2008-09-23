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

unit UTextClasses;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  gl,
  SDL,
  UTexture,
  Classes,
//  SDL_ttf,
  ULog;

{
// okay i just outline what should be here, so we can create a nice and clean implementation of sdl_ttf
// based up on this uml: http://jnr.sourceforge.net/fusion_images/www_FRS.png
// thanks to Bob Pendelton and Koshmaar!
// (1) let's start with a glyph, this represents one character in a word

type
  TGlyph = record
    character        : Char;         // unsigned char, uchar is something else in delphi
    glyphsSolid[8]   : GlyphTexture; // fast, but not that
    glyphsBlended[8] : GlyphTexture; // slower than solid, but it look's more pretty

//this class has a method, which should be a deconstructor (mog is on his way to understand the principles of oop :P)
  deconstructor  procedure ReleaseTextures();
end;

// (2) okay, we now need the stuff that's even beneath this glyph - we're right at the birth of text in here :P

  GlyphTexture = record
    textureID         : GLuint;      // we need this for caching the letters, if the texture wasn't created before create it, should be very fast because of this one
    width,
    height            : Cardinal;
    charWidth,
    charHeight        : Integer;
    advance           : Integer;     // don't know yet for what this one is
}

{
// after the glyph is done, we now start to build whole words - this one is pretty important, and does most of the work we need
  TGlyphsContainer = record
    glyphs   array of TGlyph;
    FontName array of string;
    refCount          : uChar;       // unsigned char, uchar is something else in delphi
    font              : PTTF_font;
    size,
    lineSkip          : Cardinal;    // vertical distance between multi line text output
    descent           : Integer;



}


implementation

end.
