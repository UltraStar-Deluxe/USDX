unit sdl_ttf;
{
  $Id: sdl_ttf.pas,v 1.19 2007/12/05 22:54:20 savage Exp $

}
{******************************************************************************}
{                                                                              }
{          JEDI-SDL : Pascal units for SDL - Simple DirectMedia Layer          }
{       Conversion of the Simple DirectMedia Layer Headers                     }
{                                                                              }
{ Portions created by Sam Lantinga <slouken@devolution.com> are                }
{ Copyright (C) 1997, 1998, 1999, 2000, 2001  Sam Lantinga                     }
{ 5635-34 Springhouse Dr.                                                      }
{ Pleasanton, CA 94588 (USA)                                                   }
{                                                                              }
{ All Rights Reserved.                                                         }
{                                                                              }
{ The original files are : SDL_ttf.h                                           }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Dominqiue Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{ Portions created by Dominqiue Louis are                                      }
{ Copyright (C) 2000 - 2001 Dominqiue Louis.                                   }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Tom Jones <tigertomjones@gmx.de>  His Project inspired this conversion       }
{                                                                              }
{ Obtained through:                                                            }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )                        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   The SDL Runtime libraris on Win32  : SDL.dll on Linux : libSDL.so          }
{   They are available from...                                                 }
{   http://www.libsdl.org .                                                    }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   December 08 2002 - DL : Fixed definition of TTF_RenderUnicode_Solid        }
{                                                                              }
{   April   03 2003 - DL : Added jedi-sdl.inc include file to support more     }
{                          Pascal compilers. Initial support is now included   }
{                          for GnuPascal, VirtualPascal, TMT and obviously     }
{                          continue support for Delphi Kylix and FreePascal.   }
{                                                                              }
{   April   24 2003 - DL : under instruction from Alexey Barkovoy, I have added}
{                          better TMT Pascal support and under instruction     }
{                          from Prof. Abimbola Olowofoyeku (The African Chief),}
{                          I have added better Gnu Pascal support              }
{                                                                              }
{   April   30 2003 - DL : under instruction from David Mears AKA              }
{                          Jason Siletto, I have added FPC Linux support.      }
{                          This was compiled with fpc 1.1, so remember to set  }
{                          include file path. ie. -Fi/usr/share/fpcsrc/rtl/*   }
{                                                                              }
{
  $Log: sdl_ttf.pas,v $
  Revision 1.19  2007/12/05 22:54:20  savage
  Better Mac OS X support for Frameworks.

  Revision 1.18  2007/06/01 11:16:33  savage
  Added IFDEF UNIX for Workaround.

  Revision 1.17  2007/06/01 08:38:21  savage
  Added TTF_RenderText_Solid workaround as suggested by Michalis Kamburelis

  Revision 1.16  2007/05/29 21:32:14  savage
  Changes as suggested by Almindor for 64bit compatibility.

  Revision 1.15  2007/05/20 20:32:45  savage
  Initial Changes to Handle 64 Bits

  Revision 1.14  2006/12/02 00:19:01  savage
  Updated to latest version

  Revision 1.13  2005/04/10 11:48:33  savage
  Changes as suggested by Michalis, thanks.

  Revision 1.12  2005/01/05 01:47:14  savage
  Changed LibName to reflect what MacOS X should have. ie libSDL*-1.2.0.dylib respectively.

  Revision 1.11  2005/01/04 23:14:57  savage
  Changed LibName to reflect what most Linux distros will have. ie libSDL*-1.2.so.0 respectively.

  Revision 1.10  2005/01/02 19:07:32  savage
  Slight bug fix to use LongInt instead of Long ( Thanks Michalis Kamburelis )

  Revision 1.9  2005/01/01 02:15:20  savage
  Updated to v2.0.7

  Revision 1.8  2004/10/07 21:02:32  savage
  Fix for FPC

  Revision 1.7  2004/09/30 22:39:50  savage
  Added a true type font class which contains a wrap text function.
  Changed the sdl_ttf.pas header to reflect the future of jedi-sdl.

  Revision 1.6  2004/08/14 22:54:30  savage
  Updated so that Library name defines are correctly defined for MacOS X.

  Revision 1.5  2004/05/10 14:10:04  savage
  Initial MacOS X support. Fixed defines for MACOS ( Classic ) and DARWIN ( MacOS X ).

  Revision 1.4  2004/04/13 09:32:08  savage
  Changed Shared object names back to just the .so extension to avoid conflicts on various Linux/Unix distros. Therefore developers will need to create Symbolic links to the actual Share Objects if necessary.

  Revision 1.3  2004/04/01 20:53:24  savage
  Changed Linux Shared Object names so they reflect the Symbolic Links that are created when installing the RPMs from the SDL site.

  Revision 1.2  2004/03/30 20:23:28  savage
  Tidied up use of UNIX compiler directive.

  Revision 1.1  2004/02/16 22:16:40  savage
  v1.0 changes

  
}
{******************************************************************************}

{$I jedi-sdl.inc}

{
  Define this to workaround a known bug in some freetype versions.
  The error manifests as TTF_RenderGlyph_Solid returning nil (error)
  and error message (in SDL_Error) is
  "Failed loading DPMSDisable: /usr/lib/libX11.so.6: undefined symbol: DPMSDisable"
  See [http://lists.libsdl.org/pipermail/sdl-libsdl.org/2007-March/060459.html]
}
{$IFDEF UNIX}
{$DEFINE Workaround_TTF_RenderText_Solid}
{$ENDIF}


interface

uses
{$IFDEF __GPC__}
  gpc,
{$ENDIF}

{$IFDEF WINDOWS}
  {$IFNDEF __GPC__}
  Windows,
  {$ENDIF}
{$ENDIF}
  sdl;

const
{$IFDEF WINDOWS}
  SDLttfLibName = 'SDL_ttf.dll';
{$ENDIF}

{$IFDEF UNIX}
{$IFDEF DARWIN}
  SDLttfLibName = 'libSDL_ttf-2.0.0.dylib';
{$ELSE}
  {$IFDEF FPC}
    SDLttfLibName = 'libSDL_ttf.so';
  {$ELSE}
    SDLttfLibName = 'libSDL_ttf-2.0.so.0';
  {$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFDEF MACOS}
  SDLttfLibName = 'SDL_ttf';
  {$linklib libSDL_ttf}
{$ENDIF}

  {* Printable format: "%d.%d.%d", MAJOR, MINOR, PATCHLEVEL *}
  SDL_TTF_MAJOR_VERSION = 2;
{$EXTERNALSYM SDL_TTF_MAJOR_VERSION}
  SDL_TTF_MINOR_VERSION = 0;
{$EXTERNALSYM SDL_TTF_MINOR_VERSION}
  SDL_TTF_PATCHLEVEL = 9;
{$EXTERNALSYM SDL_TTF_PATCHLEVEL}

  // Backwards compatibility
  TTF_MAJOR_VERSION = SDL_TTF_MAJOR_VERSION;
  TTF_MINOR_VERSION = SDL_TTF_MINOR_VERSION;
  TTF_PATCHLEVEL    = SDL_TTF_PATCHLEVEL;

{*
   Set and retrieve the font style
   This font style is implemented by modifying the font glyphs, and
   doesn't reflect any inherent properties of the truetype font file.
*}
  TTF_STYLE_NORMAL	  = $00;
  TTF_STYLE_BOLD      = $01;
  TTF_STYLE_ITALIC	  = $02;
  TTF_STYLE_UNDERLINE	= $04;

// ZERO WIDTH NO-BREAKSPACE (Unicode byte order mark)
  UNICODE_BOM_NATIVE  = $FEFF;
  UNICODE_BOM_SWAPPED = $FFFE;

type
  PTTF_Font = ^TTTF_font;
  TTTF_Font = record
  end;

{ This macro can be used to fill a version structure with the compile-time
  version of the SDL_ttf library. }
procedure SDL_TTF_VERSION( var X : TSDL_version );
{$EXTERNALSYM SDL_TTF_VERSION}

{ This function gets the version of the dynamically linked SDL_ttf library.
     It should NOT be used to fill a version structure, instead you should use the
     SDL_TTF_VERSION() macro. }
function TTF_Linked_Version : PSDL_version;
cdecl; external {$IFDEF __GPC__}name 'TTF_Linked_Version'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_Linked_Version}

{ This function tells the library whether UNICODE text is generally
   byteswapped.  A UNICODE BOM character in a string will override
   this setting for the remainder of that string.
}
procedure TTF_ByteSwappedUNICODE( swapped : integer );
cdecl; external {$IFDEF __GPC__}name 'TTF_ByteSwappedUNICODE'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_ByteSwappedUNICODE}

//returns 0 on succes, -1 if error occurs
function TTF_Init : integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_Init'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_Init}

{
 Open a font file and create a font of the specified point size.
 Some .fon fonts will have several sizes embedded in the file, so the
 point size becomes the index of choosing which size.  If the value
 is too high, the last indexed size will be the default.
}
function TTF_OpenFont( const filename : Pchar; ptsize : integer ) : PTTF_Font;
cdecl; external {$IFDEF __GPC__}name 'TTF_OpenFont'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_OpenFont}

function TTF_OpenFontIndex( const filename : Pchar; ptsize : integer; index : Longint ): PTTF_Font;
cdecl; external {$IFDEF __GPC__}name 'TTF_OpenFontIndex'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_OpenFontIndex}

function TTF_OpenFontRW( src : PSDL_RWops; freesrc : integer; ptsize : integer ): PTTF_Font;
cdecl; external {$IFDEF __GPC__}name 'TTF_OpenFontRW'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_OpenFontRW}

function TTF_OpenFontIndexRW( src : PSDL_RWops; freesrc : integer; ptsize : integer; index : Longint ): PTTF_Font;
cdecl; external {$IFDEF __GPC__}name 'TTF_OpenFontIndexRW'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_OpenFontIndexRW}

function TTF_GetFontStyle( font : PTTF_Font) : integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_GetFontStyle'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_GetFontStyle}

procedure TTF_SetFontStyle( font : PTTF_Font; style : integer );
cdecl; external {$IFDEF __GPC__}name 'TTF_SetFontStyle'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_SetFontStyle}

{ Get the total height of the font - usually equal to point size }
function TTF_FontHeight( font : PTTF_Font ) : Integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_FontHeight'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_FontHeight}

{ Get the offset from the baseline to the top of the font
   This is a positive value, relative to the baseline.
}
function TTF_FontAscent( font : PTTF_Font ) : Integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_FontAscent'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_FontAscent}
{ Get the offset from the baseline to the bottom of the font
   This is a negative value, relative to the baseline.
}
function TTF_FontDescent( font : PTTF_Font ) : Integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_FontDescent'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_FontDescent}

{ Get the recommended spacing between lines of text for this font }
function TTF_FontLineSkip( font : PTTF_Font ): Integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_FontLineSkip'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_FontLineSkip}

{ Get the number of faces of the font }
function TTF_FontFaces( font : PTTF_Font ) : Longint;
cdecl; external {$IFDEF __GPC__}name 'TTF_FontFaces'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_FontFaces}

{ Get the font face attributes, if any }
function TTF_FontFaceIsFixedWidth( font : PTTF_Font ): Integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_FontFaceIsFixedWidth'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_FontFaceIsFixedWidth}

function TTF_FontFaceFamilyName( font : PTTF_Font ): PChar;
cdecl; external {$IFDEF __GPC__}name 'TTF_FontFaceFamilyName'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_FontFaceFamilyName}

function TTF_FontFaceStyleName( font : PTTF_Font ): PChar;
cdecl; external {$IFDEF __GPC__}name 'TTF_FontFaceStyleName'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_FontFaceStyleName}

{ Get the metrics (dimensions) of a glyph }
function TTF_GlyphMetrics( font : PTTF_Font; ch : Uint16;
                            var minx : integer; var maxx : integer;
                            var miny : integer; var maxy : integer;
                            var advance : integer ): Integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_GlyphMetrics'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_GlyphMetrics}

{ Get the dimensions of a rendered string of text }
function TTF_SizeText( font : PTTF_Font; const text : PChar; var w : integer; var y : integer ): Integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_SizeText'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_SizeText}

function TTF_SizeUTF8( font : PTTF_Font; const text : PChar; var w : integer; var y : integer): Integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_SizeUTF8'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_SizeUTF8}

function TTF_SizeUNICODE( font : PTTF_Font; const text : PUint16; var w : integer; var y : integer): Integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_SizeUNICODE'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_SizeUNICODE}

{ Create an 8-bit palettized surface and render the given text at
   fast quality with the given font and color.  The 0 pixel is the
   colorkey, giving a transparent background, and the 1 pixel is set
   to the text color.
   This function returns the new surface, or NULL if there was an error.
}
function TTF_RenderText_Solid( font : PTTF_Font;
				const text : PChar; fg : TSDL_Color ): PSDL_Surface;
{$IFNDEF Workaround_TTF_RenderText_Solid}
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderText_Solid'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderText_Solid}
{$ENDIF}

function TTF_RenderUTF8_Solid( font : PTTF_Font;
				const text : PChar; fg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderUTF8_Solid'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderUTF8_Solid}

function TTF_RenderUNICODE_Solid( font : PTTF_Font;
				const text :PUint16; fg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderUNICODE_Solid'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderUNICODE_Solid}

{
Create an 8-bit palettized surface and render the given glyph at
   fast quality with the given font and color.  The 0 pixel is the
   colorkey, giving a transparent background, and the 1 pixel is set
   to the text color.  The glyph is rendered without any padding or
   centering in the X direction, and aligned normally in the Y direction.
   This function returns the new surface, or NULL if there was an error.
}
function  TTF_RenderGlyph_Solid( font : PTTF_Font;
					ch : Uint16; fg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderGlyph_Solid'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderGlyph_Solid}

{ Create an 8-bit palettized surface and render the given text at
   high quality with the given font and colors.  The 0 pixel is background,
   while other pixels have varying degrees of the foreground color.
   This function returns the new surface, or NULL if there was an error.
}
function TTF_RenderText_Shaded( font : PTTF_Font;
				const text : PChar; fg : TSDL_Color; bg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderText_Shaded'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderText_Shaded}
function TTF_RenderUTF8_Shaded( font : PTTF_Font;
				const text : PChar; fg : TSDL_Color; bg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderUTF8_Shaded'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderUTF8_Shaded}
function TTF_RenderUNICODE_Shaded( font : PTTF_Font;
				const text : PUint16; fg : TSDL_Color; bg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderUNICODE_Shaded'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderUNICODE_Shaded}

{ Create an 8-bit palettized surface and render the given glyph at
   high quality with the given font and colors.  The 0 pixel is background,
   while other pixels have varying degrees of the foreground color.
   The glyph is rendered without any padding or centering in the X
   direction, and aligned normally in the Y direction.
   This function returns the new surface, or NULL if there was an error.
}
function  TTF_RenderGlyph_Shaded( font : PTTF_Font; ch : Uint16; fg : TSDL_Color;
                                  bg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderGlyph_Shaded'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderGlyph_Shaded}

{ Create a 32-bit ARGB surface and render the given text at high quality,
   using alpha blending to dither the font with the given color.
   This function returns the new surface, or NULL if there was an error.
}
function TTF_RenderText_Blended( font : PTTF_Font;
				const text : PChar; fg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderText_Blended'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderText_Blended}
function TTF_RenderUTF8_Blended( font : PTTF_Font;
				const text : PChar; fg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderUTF8_Blended'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderUTF8_Blended}
function TTF_RenderUNICODE_Blended( font : PTTF_Font;
				const text: PUint16; fg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderUNICODE_Blended'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderUNICODE_Blended}

{ Create a 32-bit ARGB surface and render the given glyph at high quality,
   using alpha blending to dither the font with the given color.
   The glyph is rendered without any padding or centering in the X
   direction, and aligned normally in the Y direction.
   This function returns the new surface, or NULL if there was an error.
}
function  TTF_RenderGlyph_Blended( font : PTTF_Font; ch : Uint16; fg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderGlyph_Blended'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderGlyph_Blended}

{ For compatibility with previous versions, here are the old functions }
{#define TTF_RenderText(font, text, fg, bg)
	TTF_RenderText_Shaded(font, text, fg, bg)
#define TTF_RenderUTF8(font, text, fg, bg)	
	TTF_RenderUTF8_Shaded(font, text, fg, bg)
#define TTF_RenderUNICODE(font, text, fg, bg)	
	TTF_RenderUNICODE_Shaded(font, text, fg, bg)}

{ Close an opened font file }
procedure TTF_CloseFont( font : PTTF_Font );
cdecl; external {$IFDEF __GPC__}name 'TTF_CloseFont'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_CloseFont}

//De-initialize TTF engine
procedure TTF_Quit;
cdecl; external {$IFDEF __GPC__}name 'TTF_Quit'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_Quit}

// Check if the TTF engine is initialized
function TTF_WasInit : integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_WasInit'{$ELSE} SDLttfLibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_WasInit}

// We'll use SDL for reporting errors
procedure TTF_SetError( fmt : PChar );

function TTF_GetError : PChar;

implementation

{$IFDEF __GPC__}
  {$L 'sdl_ttf'}  { link sdl_ttf.dll.a or libsdl_ttf.so or libsdl_ttf.a }
{$ENDIF}

procedure SDL_TTF_VERSION( var X : TSDL_version );
begin
  X.major := SDL_TTF_MAJOR_VERSION;
  X.minor := SDL_TTF_MINOR_VERSION;
  X.patch := SDL_TTF_PATCHLEVEL;
end;

procedure TTF_SetError( fmt : PChar );
begin
  SDL_SetError( fmt );
end;

function TTF_GetError : PChar;
begin
  result := SDL_GetError;
end;

{$IFDEF Workaround_TTF_RenderText_Solid}
function TTF_RenderText_Solid( font : PTTF_Font;
  const text : PChar; fg : TSDL_Color ): PSDL_Surface;
const
  Black: TSDL_Color = (r: 0; g: 0; b: 0; unused: 0);
begin
  Result := TTF_RenderText_Shaded(font, text, fg, Black);
end;
{$ENDIF Workaround_TTF_RenderText_Solid}

end.
