unit sdli386utils;
{
  $Id: sdli386utils.pas,v 1.5 2004/06/02 19:38:53 savage Exp $
  
}
{******************************************************************************}
{                                                                              }
{       Borland Delphi SDL - Simple DirectMedia Layer                          }
{                SDL Utility functions                                         }
{                                                                              }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Tom Jones <tigertomjones@gmx.de>                                             }
{                                                                              }
{ Portions created by Tom Jones are                                            }
{ Copyright (C) 2000 - 2001 Tom Jones.                                         }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
{ R�bert Kisn�meth <mikrobi@freemail.hu>                                       }
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
{   Helper functions...                                                        }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   SDL.dll on Windows platforms                                               }
{   libSDL-1.1.so.0 on Linux platform                                          }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{               2000 - TJ : Initial creation                                   }
{                                                                              }
{   July   13   2001 - DL : Added PutPixel and GetPixel routines.              }
{                                                                              }
{   Sept   14   2001 - RK : Added flipping routines.                           }
{                                                                              }
{   Sept   19   2001 - RK : Added PutPixel & line drawing & blitting with ADD  }
{                           effect. Fixed a bug in SDL_PutPixel & SDL_GetPixel }
{                           Added PSDLRect()                                   }
{   Sept   22   2001 - DL : Removed need for Windows.pas by defining types here}
{                           Also removed by poor attempt or a dialog box       }
{                                                                              }
{   Sept   25   2001 - RK : Added PixelTest, NewPutPixel, SubPixel, SubLine,   }
{                           SubSurface, MonoSurface & TexturedSurface          }
{                                                                              }
{   Sept   26   2001 - DL : Made change so that it refers to native Pascal     }
{                           types rather that Windows types. This makes it more}
{                           portable to Linix.                                 }
{                                                                              }
{   Sept   27   2001 - RK : SDLUtils now can be compiled with FreePascal       }
{                                                                              }
{   Oct    27   2001 - JF : Added ScrollY function                             }
{                                                                              }
{   Jan    21   2002 - RK : Added SDL_ZoomSurface and SDL_WarpSurface          }
{                                                                              }
{   Mar    28   2002 - JF : Added SDL_RotateSurface                            }
{                                                                              }
{   May    13   2002 - RK : Improved SDL_FillRectAdd & SDL_FillRectSub         }
{                                                                              }
{   May    27   2002 - YS : GradientFillRect function                          }
{                                                                              }
{   May    30   2002 - RK : Added SDL_2xBlit, SDL_Scanline2xBlit               }
{                           & SDL_50Scanline2xBlit                             }
{                                                                              }
{  June    12   2002 - RK : Added SDL_PixelTestSurfaceVsRect                   }
{                                                                              }
{  June    12   2002 - JF : Updated SDL_PixelTestSurfaceVsRect                 }
{                                                                              }
{ November  9   2002 - JF : Added Jason's boolean Surface functions            }
{                                                                              }
{ December 10   2002 - DE : Added Dean's SDL_ClipLine function                 }
{                                                                              }
{******************************************************************************}
{
  $Log: sdli386utils.pas,v $
  Revision 1.5  2004/06/02 19:38:53  savage
  Changes to SDL_GradientFillRect as suggested by
  �ngel Eduardo Garc�a Hern�ndez.  Many thanks.

  Revision 1.4  2004/05/29 23:11:53  savage
  Changes to SDL_ScaleSurfaceRect as suggested by
  �ngel Eduardo Garc�a Hern�ndez to fix a colour issue with the function. Many thanks.

  Revision 1.3  2004/02/20 22:04:11  savage
  Added Changes as mentioned by Rodrigo "Rui" R. (1/2 RRC2Soft) to facilitate FPC compilation and it also works in Delphi. Also syncronized the funcitons so that they are identical to sdlutils.pas, when no assembly version is available.

  Revision 1.2  2004/02/14 00:23:39  savage
  As UNIX is defined in jedi-sdl.inc this will be used to check linux compatability as well. Units have been changed to reflect this change.

  Revision 1.1  2004/02/05 00:08:20  savage
  Module 1.0 release

  
}

interface

{$i jedi-sdl.inc}

uses
{$IFDEF UNIX}
  Types,
  Xlib,
{$ENDIF}
  SysUtils,
  sdl;

type
  TGradientStyle = ( gsHorizontal, gsVertical );

  // Pixel procedures
function SDL_PixelTest( SrcSurface1 : PSDL_Surface; SrcRect1 : PSDL_Rect; SrcSurface2 :
  PSDL_Surface; SrcRect2 : PSDL_Rect; Left1, Top1, Left2, Top2 : integer ) : Boolean;
  
function SDL_GetPixel( SrcSurface : PSDL_Surface; x : cardinal; y : cardinal ) : Uint32;

procedure SDL_PutPixel( SrcSurface : PSDL_Surface; x : integer; y : integer; Color :
  cardinal );

procedure SDL_AddPixel( SrcSurface : PSDL_Surface; x : integer; y : integer; Color :
  cardinal );

procedure SDL_SubPixel( SrcSurface : PSDL_Surface; x : integer; y : integer; Color :
  cardinal );

// Line procedures
procedure SDL_DrawLine( DstSurface : PSDL_Surface; x1, y1, x2, y2 : integer; Color :
  cardinal );overload;

procedure SDL_DrawLine( DstSurface : PSDL_Surface; x1, y1, x2, y2 : integer; Color :
  cardinal ; DashLength, DashSpace : byte );  overload;  

procedure SDL_AddLine( DstSurface : PSDL_Surface; x1, y1, x2, y2 : integer; Color :
  cardinal );

procedure SDL_SubLine( DstSurface : PSDL_Surface; x1, y1, x2, y2 : integer; Color :
  cardinal );

// Surface procedures
procedure SDL_AddSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DstSurface : PSDL_Surface; DestRect : PSDL_Rect );

procedure SDL_SubSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DstSurface : PSDL_Surface; DestRect : PSDL_Rect );

procedure SDL_MonoSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DstSurface : PSDL_Surface; DestRect : PSDL_Rect; Color : cardinal );

procedure SDL_TexturedSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DstSurface : PSDL_Surface; DestRect : PSDL_Rect; Texture : PSDL_Surface;
  TextureRect : PSDL_Rect );

procedure SDL_ZoomSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect; DstSurface : PSDL_Surface; DstRect : PSDL_Rect );

procedure SDL_WarpSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect; DstSurface : PSDL_Surface; UL, UR, LR, LL : PPoint );

// Flip procedures
procedure SDL_FlipRectH( DstSurface : PSDL_Surface; Rect : PSDL_Rect );

procedure SDL_FlipRectV( DstSurface : PSDL_Surface; Rect : PSDL_Rect );

function PSDLRect( aLeft, aTop, aWidth, aHeight : integer ) : PSDL_Rect;

function SDLRect( aLeft, aTop, aWidth, aHeight : integer ) : TSDL_Rect; overload;

function SDLRect( aRect : TRect ) : TSDL_Rect; overload;

function SDL_ScaleSurfaceRect( SrcSurface : PSDL_Surface; SrcX1, SrcY1, SrcW, SrcH,
  Width, Height : integer ) : PSDL_Surface;

procedure SDL_ScrollY( DstSurface : PSDL_Surface; DifY : integer );

procedure SDL_ScrollX( DstSurface : PSDL_Surface; DifX : integer );

procedure SDL_RotateDeg( DstSurface, SrcSurface : PSDL_Surface; SrcRect :
  PSDL_Rect; DestX, DestY, OffsetX, OffsetY : Integer; Angle : Integer );

procedure SDL_RotateRad( DstSurface, SrcSurface : PSDL_Surface; SrcRect :
  PSDL_Rect; DestX, DestY, OffsetX, OffsetY : Integer; Angle : Single );

function ValidateSurfaceRect( DstSurface : PSDL_Surface; dstrect : PSDL_Rect ) : TSDL_Rect;

// Fill Rect routine
procedure SDL_FillRectAdd( DstSurface : PSDL_Surface; dstrect : PSDL_Rect; color : UInt32 );

procedure SDL_FillRectSub( DstSurface : PSDL_Surface; dstrect : PSDL_Rect; color : UInt32 );

procedure SDL_GradientFillRect( DstSurface : PSDL_Surface; const Rect : PSDL_Rect; const StartColor, EndColor : TSDL_Color; const Style : TGradientStyle );

// NOTE for All SDL_2xblit... function : the dest surface must be 2x of the source surface!
procedure SDL_2xBlit( Src, Dest : PSDL_Surface );

procedure SDL_Scanline2xBlit( Src, Dest : PSDL_Surface );

procedure SDL_50Scanline2xBlit( Src, Dest : PSDL_Surface );

function SDL_PixelTestSurfaceVsRect( SrcSurface1 : PSDL_Surface; SrcRect1 :
PSDL_Rect; SrcRect2 : PSDL_Rect; Left1, Top1, Left2, Top2 : integer ) :
boolean;

// Jason's boolean Surface functions
procedure SDL_ORSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DestSurface : PSDL_Surface; DestRect : PSDL_Rect );

procedure SDL_ANDSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DestSurface : PSDL_Surface; DestRect : PSDL_Rect );

procedure SDL_GTSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DestSurface : PSDL_Surface; DestRect : PSDL_Rect );

procedure SDL_LTSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DestSurface : PSDL_Surface; DestRect : PSDL_Rect );

function SDL_ClipLine(var x1,y1,x2,y2: Integer; ClipRect: PSDL_Rect) : boolean;

implementation

uses
  Math;

function SDL_PixelTest( SrcSurface1 : PSDL_Surface; SrcRect1 : PSDL_Rect; SrcSurface2 :
  PSDL_Surface; SrcRect2 : PSDL_Rect; Left1, Top1, Left2, Top2 : integer ) : boolean;
var
  Src_Rect1, Src_Rect2 : TSDL_Rect;
  right1, bottom1 : integer;
  right2, bottom2 : integer;
  Scan1Start, Scan2Start, ScanWidth, ScanHeight : cardinal;
  Mod1, Mod2 : cardinal;
  Addr1, Addr2 : cardinal;
  BPP : cardinal;
  Pitch1, Pitch2 : cardinal;
  TransparentColor1, TransparentColor2 : cardinal;
  tx, ty : cardinal;
  StartTick : cardinal;
  Color1, Color2 : cardinal;
begin
  Result := false;
  if SrcRect1 = nil then
  begin
    with Src_Rect1 do
    begin
      x := 0;
      y := 0;
      w := SrcSurface1.w;
      h := SrcSurface1.h;
    end;
  end
  else
    Src_Rect1 := SrcRect1^;
  if SrcRect2 = nil then
  begin
    with Src_Rect2 do
    begin
      x := 0;
      y := 0;
      w := SrcSurface2.w;
      h := SrcSurface2.h;
    end;
  end
  else
    Src_Rect2 := SrcRect2^;
  with Src_Rect1 do
  begin
    Right1 := Left1 + w;
    Bottom1 := Top1 + h;
  end;
  with Src_Rect2 do
  begin
    Right2 := Left2 + w;
    Bottom2 := Top2 + h;
  end;
  if ( Left1 >= Right2 ) or ( Right1 <= Left2 ) or ( Top1 >= Bottom2 ) or ( Bottom1 <=
    Top2 ) then
    exit;
  if Left1 <= Left2 then
  begin
    // 1. left, 2. right
    Scan1Start := Src_Rect1.x + Left2 - Left1;
    Scan2Start := Src_Rect2.x;
    ScanWidth := Right1 - Left2;
    with Src_Rect2 do
      if ScanWidth > w then
        ScanWidth := w;
  end
  else
  begin
    // 1. right, 2. left
    Scan1Start := Src_Rect1.x;
    Scan2Start := Src_Rect2.x + Left1 - Left2;
    ScanWidth := Right2 - Left1;
    with Src_Rect1 do
      if ScanWidth > w then
        ScanWidth := w;
  end;
  with SrcSurface1^ do
  begin
    Pitch1 := Pitch;
    Addr1 := cardinal( Pixels );
    inc( Addr1, Pitch1 * UInt32( Src_Rect1.y ) );
    with format^ do
    begin
      BPP := BytesPerPixel;
      TransparentColor1 := colorkey;
    end;
  end;
  with SrcSurface2^ do
  begin
    TransparentColor2 := format.colorkey;
    Pitch2 := Pitch;
    Addr2 := cardinal( Pixels );
    inc( Addr2, Pitch2 * UInt32( Src_Rect2.y ) );
  end;
  Mod1 := Pitch1 - ( ScanWidth * BPP );
  Mod2 := Pitch2 - ( ScanWidth * BPP );
  inc( Addr1, BPP * Scan1Start );
  inc( Addr2, BPP * Scan2Start );
  if Top1 <= Top2 then
  begin
    // 1. up, 2. down
    ScanHeight := Bottom1 - Top2;
    if ScanHeight > Src_Rect2.h then
      ScanHeight := Src_Rect2.h;
    inc( Addr1, Pitch1 * UInt32( Top2 - Top1 ) );
  end
  else
  begin
    // 1. down, 2. up
    ScanHeight := Bottom2 - Top1;
    if ScanHeight > Src_Rect1.h then
      ScanHeight := Src_Rect1.h;
    inc( Addr2, Pitch2 * UInt32( Top1 - Top2 ) );
  end;
  case BPP of
    1 :
      for ty := 1 to ScanHeight do
      begin
        for tx := 1 to ScanWidth do
        begin
          if ( PByte( Addr1 )^ <> TransparentColor1 ) and ( PByte( Addr2 )^ <>
            TransparentColor2 ) then
          begin
            Result := true;
            exit;
          end;
          inc( Addr1 );
          inc( Addr2 );
        end;
        inc( Addr1, Mod1 );
        inc( Addr2, Mod2 );
      end;
    2 :
      for ty := 1 to ScanHeight do
      begin
        for tx := 1 to ScanWidth do
        begin
          if ( PWord( Addr1 )^ <> TransparentColor1 ) and ( PWord( Addr2 )^ <>
            TransparentColor2 ) then
          begin
            Result := true;
            exit;
          end;
          inc( Addr1, 2 );
          inc( Addr2, 2 );
        end;
        inc( Addr1, Mod1 );
        inc( Addr2, Mod2 );
      end;
    3 :
      for ty := 1 to ScanHeight do
      begin
        for tx := 1 to ScanWidth do
        begin
          Color1 := PLongWord( Addr1 )^ and $00FFFFFF;
          Color2 := PLongWord( Addr2 )^ and $00FFFFFF;
          if ( Color1 <> TransparentColor1 ) and ( Color2 <> TransparentColor2 )
            then
          begin
            Result := true;
            exit;
          end;
          inc( Addr1, 3 );
          inc( Addr2, 3 );
        end;
        inc( Addr1, Mod1 );
        inc( Addr2, Mod2 );
      end;
    4 :
      for ty := 1 to ScanHeight do
      begin
        for tx := 1 to ScanWidth do
        begin
          if ( PLongWord( Addr1 )^ <> TransparentColor1 ) and ( PLongWord( Addr2 )^ <>
            TransparentColor2 ) then
          begin
            Result := true;
            exit;
          end;
          inc( Addr1, 4 );
          inc( Addr2, 4 );
        end;
        inc( Addr1, Mod1 );
        inc( Addr2, Mod2 );
      end;
  end;
end;

function SDL_GetPixel( SrcSurface : PSDL_Surface; x : cardinal; y : cardinal ) : Uint32;
var
  bpp : UInt32;
  p : PInteger;
begin
  bpp := SrcSurface.format.BytesPerPixel;
  // Here p is the address to the pixel we want to retrieve
  p := Pointer( Uint32( SrcSurface.pixels ) + UInt32( y ) * SrcSurface.pitch + UInt32( x ) *
    bpp );
  case bpp of
    1 : result := PUint8( p )^;
    2 : result := PUint16( p )^;
    3 :
      if ( SDL_BYTEORDER = SDL_BIG_ENDIAN ) then
        result := PUInt8Array( p )[ 0 ] shl 16 or PUInt8Array( p )[ 1 ] shl 8 or
          PUInt8Array( p )[ 2 ]
      else
        result := PUInt8Array( p )[ 0 ] or PUInt8Array( p )[ 1 ] shl 8 or
          PUInt8Array( p )[ 2 ] shl 16;
    4 : result := PUint32( p )^;
  else
    result := 0; // shouldn't happen, but avoids warnings
  end;
end;

procedure SDL_PutPixel( SrcSurface : PSDL_Surface; x : integer; y : integer; Color :
  cardinal );
var
  Addr, Pitch, BPP : cardinal;
begin
  Addr := cardinal( SrcSurface.Pixels );
  Pitch := SrcSurface.Pitch;
  BPP := SrcSurface.format.BytesPerPixel;
  asm
    mov eax, y
    mul Pitch      // EAX := y * Pitch
    add Addr, eax  // Addr:= Addr + (y * Pitch)
    mov eax, x
    mov ecx, Color
    cmp BPP, 1
    jne @Not1BPP
    add eax, Addr  // Now:   EAX:= Addr + (y * Pitch) + x
    mov [eax], cl
    jmp @Quit
   @Not1BPP:
    cmp BPP, 2
    jne @Not2BPP
    mul BPP   // EAX := x * BPP
    add eax, Addr  // Now:   EAX:= Addr + (y * Pitch) + x * BPP
    mov [eax], cx
    jmp @Quit
   @Not2BPP:
    cmp BPP, 3
    jne @Not3BPP
    mul BPP   // EAX := x * BPP
    add eax, Addr  // Now:   EAX:= Addr + (y * Pitch) + x * BPP
    mov edx, [eax]
    and edx, $ff000000
    or edx, ecx
    mov [eax], edx
    jmp @Quit
   @Not3BPP:
    mul BPP   // EAX := x * BPP
    add eax, Addr  // Now:   EAX:= Addr + (y * Pitch) + x * BPP
    mov [eax], ecx
   @Quit:
  end;
end;

procedure SDL_AddPixel( SrcSurface : PSDL_Surface; x : integer; y : integer; Color :
  cardinal );
var
  SrcColor, FinalColor : cardinal;
  Addr, Pitch, Bits : cardinal;
begin
  if Color = 0 then
    exit;
  Addr := cardinal( SrcSurface.Pixels );
  Pitch := SrcSurface.Pitch;
  Bits := SrcSurface.format.BitsPerPixel;
  asm
    mov eax, y
    mul Pitch      // EAX := y * Pitch
    add Addr, eax  // Addr:= Addr + (y * Pitch)
    mov eax, x
    cmp Bits, 8
    jne @Not8bit
    add eax, Addr  // Now:   EAX:= Addr + (y * Pitch) + x
    mov cl, [eax]
    movzx ecx, cl
    mov SrcColor, ecx
    mov edx, Color
    and ecx, 3
    and edx, 3
    add ecx, edx
    cmp ecx, 3
    jbe @Skip1_8bit
    mov ecx, 3
   @Skip1_8bit:
    mov FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $1c
    and edx, $1c
    add ecx, edx
    cmp ecx, $1c
    jbe @Skip2_8bit
    mov ecx, $1c
   @Skip2_8bit:
    or FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $e0
    and edx, $e0
    add ecx, edx
    cmp ecx, $e0
    jbe @Skip3_8bit
    mov ecx, $e0
   @Skip3_8bit:
    or ecx, FinalColor
    mov [eax], cl
    jmp @Quit
   @Not8bit:
    cmp Bits, 15
    jne @Not15bit
    shl eax, 1
    add eax, Addr  // Now:   EAX:= Addr + (y * Pitch) + x * 2
    mov ecx, [eax]
    and ecx, $00007fff
    mov SrcColor, ecx
    mov edx, Color
    and ecx, $1f
    and edx, $1f
    add ecx, edx
    cmp ecx, $1f
    jbe @Skip1_15bit
    mov ecx, $1f
   @Skip1_15bit:
    mov FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $03e0
    and edx, $03e0
    add ecx, edx
    cmp ecx, $03e0
    jbe @Skip2_15bit
    mov ecx, $03e0
   @Skip2_15bit:
    or FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $7c00
    and edx, $7c00
    add ecx, edx
    cmp ecx, $7c00
    jbe @Skip3_15bit
    mov ecx, $7c00
   @Skip3_15bit:
    or ecx, FinalColor
    mov [eax], cx
    jmp @Quit
   @Not15Bit:
    cmp Bits, 16
    jne @Not16bit
    shl eax, 1
    add eax, Addr  // Now:   EAX:= Addr + (y * Pitch) + x * 2
    mov ecx, [eax]
    and ecx, $0000ffff
    mov SrcColor, ecx
    mov edx, Color
    and ecx, $1f
    and edx, $1f
    add ecx, edx
    cmp ecx, $1f
    jbe @Skip1_16bit
    mov ecx, $1f
   @Skip1_16bit:
    mov FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $07e0
    and edx, $07e0
    add ecx, edx
    cmp ecx, $07e0
    jbe @Skip2_16bit
    mov ecx, $07e0
   @Skip2_16bit:
    or FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $f800
    and edx, $f800
    add ecx, edx
    cmp ecx, $f800
    jbe @Skip3_16bit
    mov ecx, $f800
   @Skip3_16bit:
    or ecx, FinalColor
    mov [eax], cx
    jmp @Quit
   @Not16Bit:
    cmp Bits, 24
    jne @Not24bit
    mov ecx, 0
    add ecx, eax
    shl ecx, 1
    add ecx, eax
    mov eax, ecx
    jmp @32bit
   @Not24bit:
    shl eax, 2
   @32bit:
    add eax, Addr  // Now:   EAX:= Addr + (y * Pitch) + x * 2
    mov ecx, [eax]
    mov FinalColor, ecx
    and FinalColor, $ff000000
    and ecx, $00ffffff
    mov SrcColor, ecx
    mov edx, Color
    and ecx, $000000ff
    and edx, $000000ff
    add ecx, edx
    cmp ecx, $000000ff
    jbe @Skip1_32bit
    mov ecx, $000000ff
   @Skip1_32bit:
    or FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $0000ff00
    and edx, $0000ff00
    add ecx, edx
    cmp ecx, $0000ff00
    jbe @Skip2_32bit
    mov ecx, $0000ff00
   @Skip2_32bit:
    or FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $00ff0000
    and edx, $00ff0000
    add ecx, edx
    cmp ecx, $00ff0000
    jbe @Skip3_32bit
    mov ecx, $00ff0000
   @Skip3_32bit:
    or ecx, FinalColor
    mov [eax], ecx
   @Quit:
  end;
end;

procedure SDL_SubPixel( SrcSurface : PSDL_Surface; x : integer; y : integer; Color :
  cardinal );
var
  SrcColor, FinalColor : cardinal;
  Addr, Pitch, Bits : cardinal;
begin
  if Color = 0 then
    exit;
  Addr := cardinal( SrcSurface.Pixels );
  Pitch := SrcSurface.Pitch;
  Bits := SrcSurface.format.BitsPerPixel;
  asm
    mov eax, y
    mul Pitch      // EAX := y * Pitch
    add Addr, eax  // Addr:= Addr + (y * Pitch)
    mov eax, x
    cmp Bits, 8
    jne @Not8bit
    add eax, Addr  // Now:   EAX:= Addr + (y * Pitch) + x
    mov cl, [eax]
    movzx ecx, cl
    mov SrcColor, ecx
    mov edx, Color
    and ecx, 3
    and edx, 3
    sub ecx, edx
    jns @Skip1_8bit
    mov ecx, 0
   @Skip1_8bit:
    mov FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $1c
    and edx, $1c
    sub ecx, edx
    jns @Skip2_8bit
    mov ecx, 0
   @Skip2_8bit:
    or FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $e0
    and edx, $e0
    sub ecx, edx
    jns @Skip3_8bit
    mov ecx, 0
   @Skip3_8bit:
    or ecx, FinalColor
    mov [eax], cl
    jmp @Quit
   @Not8bit:
    cmp Bits, 15
    jne @Not15bit
    shl eax, 1
    add eax, Addr
    mov ecx, [eax]
    and ecx, $00007fff
    mov SrcColor, ecx
    mov edx, Color
    and ecx, $1f
    and edx, $1f
    sub ecx, edx
    jns @Skip1_15bit
    mov ecx, 0
   @Skip1_15bit:
    mov FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $03e0
    and edx, $03e0
    sub ecx, edx
    jns @Skip2_15bit
    mov ecx, 0
   @Skip2_15bit:
    or FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $7c00
    and edx, $7c00
    sub ecx, edx
    jns @Skip3_15bit
    mov ecx, 0
   @Skip3_15bit:
    or ecx, FinalColor
    mov [eax], cx
    jmp @Quit
   @Not15Bit:
    cmp Bits, 16
    jne @Not16bit
    shl eax, 1
    add eax, Addr
    mov ecx, [eax]
    and ecx, $0000ffff
    mov SrcColor, ecx
    mov edx, Color
    and ecx, $1f
    and edx, $1f
    sub ecx, edx
    jns @Skip1_16bit
    mov ecx, 0
   @Skip1_16bit:
    mov FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $07e0
    and edx, $07e0
    sub ecx, edx
    jns @Skip2_16bit
    mov ecx, 0
   @Skip2_16bit:
    or FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $f800
    and edx, $f800
    sub ecx, edx
    jns @Skip3_16bit
    mov ecx, 0
   @Skip3_16bit:
    or ecx, FinalColor
    mov [eax], cx
    jmp @Quit
   @Not16Bit:
    cmp Bits, 24
    jne @Not24bit
    mov ecx, 0
    add ecx, eax
    shl ecx, 1
    add ecx, eax
    mov eax, ecx
    jmp @32bit
   @Not24bit:
    shl eax, 2
   @32bit:
    add eax, Addr
    mov ecx, [eax]
    mov FinalColor, ecx
    and FinalColor, $ff000000
    and ecx, $00ffffff
    mov SrcColor, ecx
    mov edx, Color
    and ecx, $000000ff
    and edx, $000000ff
    sub ecx, edx
    jns @Skip1_32bit
    mov ecx, 0
   @Skip1_32bit:
    or FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $0000ff00
    and edx, $0000ff00
    sub ecx, edx
    jns @Skip2_32bit
    mov ecx, 0
   @Skip2_32bit:
    or FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $00ff0000
    and edx, $00ff0000
    sub ecx, edx
    jns @Skip3_32bit
    mov ecx, 0
   @Skip3_32bit:
    or ecx, FinalColor
    mov [eax], ecx
   @Quit:
  end;
end;

// Draw a line between x1,y1 and x2,y2 to the given surface
// NOTE: The surface must be locked before calling this!
procedure SDL_DrawLine( DstSurface : PSDL_Surface; x1, y1, x2, y2 : integer; Color :
  cardinal );
var
  dx, dy, sdx, sdy, x, y, px, py : integer;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  if dx < 0 then
    sdx := -1
  else
    sdx := 1;
  if dy < 0 then
    sdy := -1
  else
    sdy := 1;
  dx := sdx * dx + 1;
  dy := sdy * dy + 1;
  x := 0;
  y := 0;
  px := x1;
  py := y1;
  if dx >= dy then
  begin
    for x := 0 to dx - 1 do
    begin
      SDL_PutPixel( DstSurface, px, py, Color );
      y := y + dy;
      if y >= dx then
      begin
        y := y - dx;
        py := py + sdy;
      end;
      px := px + sdx;
    end;
  end
  else
  begin
    for y := 0 to dy - 1 do
    begin
      SDL_PutPixel( DstSurface, px, py, Color );
      x := x + dx;
      if x >= dy then
      begin
        x := x - dy;
        px := px + sdx;
      end;
      py := py + sdy;
    end;
  end;
end;

// Draw a dashed line between x1,y1 and x2,y2 to the given surface
// NOTE: The surface must be locked before calling this!
procedure SDL_DrawLine( DstSurface : PSDL_Surface; x1, y1, x2, y2 : integer; Color :
  cardinal ; DashLength, DashSpace : byte );  overload;
var
  dx, dy, sdx, sdy, x, y, px, py, counter : integer; drawdash : boolean;
begin
  counter := 0;
  drawdash := true; //begin line drawing with dash

  //Avoid invalid user-passed dash parameters
  if (DashLength < 1)
  then DashLength := 1;
  if (DashSpace < 1)
  then DashSpace := 0;

  dx := x2 - x1;
  dy := y2 - y1;
  if dx < 0 then
    sdx := -1
  else
    sdx := 1;
  if dy < 0 then
    sdy := -1
  else
    sdy := 1;
  dx := sdx * dx + 1;
  dy := sdy * dy + 1;
  x := 0;
  y := 0;
  px := x1;
  py := y1;
  if dx >= dy then
  begin
    for x := 0 to dx - 1 do
      begin

      //Alternate drawing dashes, or leaving spaces
      if drawdash then
        begin
          SDL_PutPixel( DstSurface, px, py, Color );
          inc(counter);
          if (counter > DashLength-1) and (DashSpace > 0) then
            begin
              drawdash := false;
              counter := 0;
            end;
        end
      else //space
        begin
          inc(counter);
          if counter > DashSpace-1 then
            begin
              drawdash := true;
              counter := 0;
            end;
        end;

      y := y + dy;
      if y >= dx then
      begin
        y := y - dx;
        py := py + sdy;
      end;
      px := px + sdx;
    end;
  end
  else
  begin
    for y := 0 to dy - 1 do
    begin

      //Alternate drawing dashes, or leaving spaces
      if drawdash then
        begin
          SDL_PutPixel( DstSurface, px, py, Color );
          inc(counter);
          if (counter > DashLength-1) and (DashSpace > 0) then
            begin
              drawdash := false;
              counter := 0;
            end;
        end
      else //space
        begin
          inc(counter);
          if counter > DashSpace-1 then
            begin
              drawdash := true;
              counter := 0;
            end;
        end;

      x := x + dx;
      if x >= dy then
      begin
        x := x - dy;
        px := px + sdx;
      end;
      py := py + sdy;
    end;
  end;
end;

procedure SDL_AddLine( DstSurface : PSDL_Surface; x1, y1, x2, y2 : integer; Color :
  cardinal );
var
  dx, dy, sdx, sdy, x, y, px, py : integer;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  if dx < 0 then
    sdx := -1
  else
    sdx := 1;
  if dy < 0 then
    sdy := -1
  else
    sdy := 1;
  dx := sdx * dx + 1;
  dy := sdy * dy + 1;
  x := 0;
  y := 0;
  px := x1;
  py := y1;
  if dx >= dy then
  begin
    for x := 0 to dx - 1 do
    begin
      SDL_AddPixel( DstSurface, px, py, Color );
      y := y + dy;
      if y >= dx then
      begin
        y := y - dx;
        py := py + sdy;
      end;
      px := px + sdx;
    end;
  end
  else
  begin
    for y := 0 to dy - 1 do
    begin
      SDL_AddPixel( DstSurface, px, py, Color );
      x := x + dx;
      if x >= dy then
      begin
        x := x - dy;
        px := px + sdx;
      end;
      py := py + sdy;
    end;
  end;
end;

procedure SDL_SubLine( DstSurface : PSDL_Surface; x1, y1, x2, y2 : integer; Color :
  cardinal );
var
  dx, dy, sdx, sdy, x, y, px, py : integer;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  if dx < 0 then
    sdx := -1
  else
    sdx := 1;
  if dy < 0 then
    sdy := -1
  else
    sdy := 1;
  dx := sdx * dx + 1;
  dy := sdy * dy + 1;
  x := 0;
  y := 0;
  px := x1;
  py := y1;
  if dx >= dy then
  begin
    for x := 0 to dx - 1 do
    begin
      SDL_SubPixel( DstSurface, px, py, Color );
      y := y + dy;
      if y >= dx then
      begin
        y := y - dx;
        py := py + sdy;
      end;
      px := px + sdx;
    end;
  end
  else
  begin
    for y := 0 to dy - 1 do
    begin
      SDL_SubPixel( DstSurface, px, py, Color );
      x := x + dx;
      if x >= dy then
      begin
        x := x - dy;
        px := px + sdx;
      end;
      py := py + sdy;
    end;
  end;
end;

// This procedure works on 8, 15, 16, 24 and 32 bits color depth surfaces.
// In 8 bit color depth mode the procedure works with the default packed
//  palette (RRRGGGBB). It handles all clipping.
procedure SDL_AddSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DstSurface : PSDL_Surface; DestRect : PSDL_Rect );
var
  Src, Dest : TSDL_Rect;
  Diff : integer;
  SrcAddr, DestAddr : cardinal;
  // TransparentColor: cardinal;
  _ebx, _esi, _edi, _esp : cardinal;
  WorkX, WorkY : word;
  SrcMod, DestMod : cardinal;
  Bits : cardinal;
begin
  if ( SrcSurface = nil ) or ( DstSurface = nil ) then
    exit; // Remove this to make it faster
  if ( SrcSurface.Format.BitsPerPixel <> DstSurface.Format.BitsPerPixel ) then
    exit; // Remove this to make it faster
  if SrcRect = nil then
  begin
    with Src do
    begin
      x := 0;
      y := 0;
      w := SrcSurface.w;
      h := SrcSurface.h;
    end;
  end
  else
    Src := SrcRect^;
  if DestRect = nil then
  begin
    Dest.x := 0;
    Dest.y := 0;
  end
  else
    Dest := DestRect^;
  Dest.w := Src.w;
  Dest.h := Src.h;
  with DstSurface.Clip_Rect do
  begin
    // Source's right side is greater than the dest.cliprect
    if Dest.x + Src.w > x + w then
    begin
      smallint( Src.w ) := x + w - Dest.x;
      smallint( Dest.w ) := x + w - Dest.x;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's bottom side is greater than the dest.clip
    if Dest.y + Src.h > y + h then
    begin
      smallint( Src.h ) := y + h - Dest.y;
      smallint( Dest.h ) := y + h - Dest.y;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
    // Source's left side is less than the dest.clip
    if Dest.x < x then
    begin
      Diff := x - Dest.x;
      Src.x := Src.x + Diff;
      smallint( Src.w ) := smallint( Src.w ) - Diff;
      Dest.x := x;
      smallint( Dest.w ) := smallint( Dest.w ) - Diff;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's Top side is less than the dest.clip
    if Dest.y < y then
    begin
      Diff := y - Dest.y;
      Src.y := Src.y + Diff;
      smallint( Src.h ) := smallint( Src.h ) - Diff;
      Dest.y := y;
      smallint( Dest.h ) := smallint( Dest.h ) - Diff;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
  end;
  with SrcSurface^ do
  begin
    SrcAddr := cardinal( Pixels ) + UInt32( Src.y ) * Pitch + UInt32( Src.x ) *
      Format.BytesPerPixel;
    SrcMod := Pitch - Src.w * Format.BytesPerPixel;
    //    TransparentColor := format.ColorKey;
  end;
  with DstSurface^ do
  begin
    DestAddr := cardinal( Pixels ) + UInt32( Dest.y ) * Pitch + UInt32( Dest.x ) *
      Format.BytesPerPixel;
    DestMod := Pitch - Dest.w * Format.BytesPerPixel;
    Bits := Format.BitsPerPixel;
  end;
  SDL_LockSurface( SrcSurface );
  SDL_LockSurface( DstSurface );
  case bits of
    8 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov al, [esi]         // AL := source color
        cmp al, 0
        je @SkipColor         // if AL=0 or AL=transparent color then skip everything
        mov esp, eax          // ESP - source color
        mov bl, [edi]         // BL := destination color
        mov dl, bl            // DL := destination color
        and ax, $03           // Adding BLUE
        and bl, $03
        add al, bl
        cmp al, $03
        jbe @Skip1
        mov al, $03
       @Skip1:
        mov cl, al
        mov eax, esp          // Adding GREEN
        mov bl, dl
        and al, $1c
        and bl, $1c
        add al, bl
        cmp al, $1c
        jbe @Skip2
        mov al, $1c
       @Skip2:
        or cl, al
        mov eax, esp          // Adding RED
        mov bl, dl
        and ax, $e0
        and bx, $e0
        add ax, bx
        cmp ax, $e0
        jbe @Skip3
        mov al, $e0
       @Skip3:
        or cl, al
        mov [edi], cl
       @SkipColor:
        inc esi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    15 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax          // WorkX := Src.w
       @Loopx:
        mov ax, [esi]         // AX := source color
        cmp ax, 0
        je @SkipColor         // if AX=0 then skip everything
        mov esp, eax          // ESP - source color
        mov bx, [edi]         // BX := destination color
        mov dx, bx            // DX := destination color
        and ax, $001F         // Adding BLUE
        and bx, $001F
        add ax, bx
        cmp ax, $001F
        jbe @Skip1
        mov ax, $001F
       @Skip1:
        mov cx, ax
        mov eax, esp          // Adding GREEN
        mov bx, dx
        and ax, $3E0
        and bx, $3E0
        add ax, bx
        cmp ax, $3E0
        jbe @Skip2
        mov ax, $3E0
       @Skip2:
        or cx, ax
        mov eax, esp          // Adding RED
        mov bx, dx
        and ax, $7C00
        and bx, $7C00
        add ax, bx
        cmp ax, $7C00
        jbe @Skip3
        mov ax, $7C00
       @Skip3:
        or cx, ax
        mov [edi], cx
       @SkipColor:
        add esi, 2
        add edi, 2
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    16 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov ax, [esi]         // AX := source color
        cmp ax, 0
        je @SkipColor         // if AX=0 then skip everything
        mov esp, eax          // ESP - source color
        mov bx, [edi]         // BX := destination color
        mov dx, bx            // DX := destination color
        and ax, $1F           // Adding BLUE
        and bx, $1F
        add ax, bx
        cmp ax, $1F
        jbe @Skip1
        mov ax, $1F
       @Skip1:
        mov cx, ax
        mov eax, esp          // Adding GREEN
        mov bx, dx
        and ax, $7E0
        and bx, $7E0
        add ax, bx
        cmp ax, $7E0
        jbe @Skip2
        mov ax, $7E0
       @Skip2:
        or cx, ax
        mov eax, esp          // Adding RED
        mov bx, dx
        and eax, $F800
        and ebx, $F800
        add eax, ebx
        cmp eax, $F800
        jbe @Skip3
        mov ax, $F800
       @Skip3:
        or cx, ax
        mov [edi], cx
       @SkipColor:
        add esi, 2
        add edi, 2
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    24 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       add WorkX, ax         // WorkX := Src.w * 2
       add WorkX, ax         // WorkX := Src.w * 3
       @Loopx:
        mov bl, [edi]         // BX := destination color
        mov al, [esi]         // AX := source color
        cmp al, 0
        je @Skip              // if AL=0 then skip COMPONENT
        mov ah, 0             // AX := COLOR COMPONENT
        mov bh, 0
        add bx, ax
        cmp bx, $00ff
        jb @Skip
        mov bl, $ff
       @Skip:
        mov [edi], bl
        inc esi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    32 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       shl ax, 2
       mov WorkX, ax         // WorkX := Src.w * 4
       @Loopx:
        mov bl, [edi]         // BX := destination color
        mov al, [esi]         // AX := source color
        cmp al, 0
        je @Skip              // if AL=0 then skip COMPONENT
        mov ah, 0             // AX := COLOR COMPONENT
        mov bh, 0
        add bx, ax
        cmp bx, $00ff
        jb @Skip
        mov bl, $ff
       @Skip:
        mov [edi], bl
        inc esi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
  end;
  SDL_UnlockSurface( SrcSurface );
  SDL_UnlockSurface( DstSurface );
end;

procedure SDL_SubSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DstSurface : PSDL_Surface; DestRect : PSDL_Rect );
var
  Src, Dest : TSDL_Rect;
  Diff : integer;
  SrcAddr, DestAddr : cardinal;
  _ebx, _esi, _edi, _esp : cardinal;
  WorkX, WorkY : word;
  SrcMod, DestMod : cardinal;
  Bits : cardinal;
begin
  if ( SrcSurface = nil ) or ( DstSurface = nil ) then
    exit; // Remove this to make it faster
  if ( SrcSurface.Format.BitsPerPixel <> DstSurface.Format.BitsPerPixel ) then
    exit; // Remove this to make it faster
  if SrcRect = nil then
  begin
    with Src do
    begin
      x := 0;
      y := 0;
      w := SrcSurface.w;
      h := SrcSurface.h;
    end;
  end
  else
    Src := SrcRect^;
  if DestRect = nil then
  begin
    Dest.x := 0;
    Dest.y := 0;
  end
  else
    Dest := DestRect^;
  Dest.w := Src.w;
  Dest.h := Src.h;
  with DstSurface.Clip_Rect do
  begin
    // Source's right side is greater than the dest.cliprect
    if Dest.x + Src.w > x + w then
    begin
      smallint( Src.w ) := x + w - Dest.x;
      smallint( Dest.w ) := x + w - Dest.x;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's bottom side is greater than the dest.clip
    if Dest.y + Src.h > y + h then
    begin
      smallint( Src.h ) := y + h - Dest.y;
      smallint( Dest.h ) := y + h - Dest.y;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
    // Source's left side is less than the dest.clip
    if Dest.x < x then
    begin
      Diff := x - Dest.x;
      Src.x := Src.x + Diff;
      smallint( Src.w ) := smallint( Src.w ) - Diff;
      Dest.x := x;
      smallint( Dest.w ) := smallint( Dest.w ) - Diff;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's Top side is less than the dest.clip
    if Dest.y < y then
    begin
      Diff := y - Dest.y;
      Src.y := Src.y + Diff;
      smallint( Src.h ) := smallint( Src.h ) - Diff;
      Dest.y := y;
      smallint( Dest.h ) := smallint( Dest.h ) - Diff;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
  end;
  with SrcSurface^ do
  begin
    SrcAddr := cardinal( Pixels ) + UInt32( Src.y ) * Pitch + UInt32( Src.x ) *
      Format.BytesPerPixel;
    SrcMod := Pitch - Src.w * Format.BytesPerPixel;
  end;
  with DstSurface^ do
  begin
    DestAddr := cardinal( Pixels ) + UInt32( Dest.y ) * Pitch + UInt32( Dest.x ) *
      Format.BytesPerPixel;
    DestMod := Pitch - Dest.w * Format.BytesPerPixel;
    Bits := DstSurface.Format.BitsPerPixel;
  end;
  SDL_LockSurface( SrcSurface );
  SDL_LockSurface( DstSurface );
  case bits of
    8 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov al, [esi]         // AL := source color
        cmp al, 0
        je @SkipColor         // if AL=0 then skip everything
        mov esp, eax          // ESP - source color
        mov bl, [edi]         // BL := destination color
        mov dl, bl            // DL := destination color
        and al, $03           // Subtract BLUE
        and bl, $03
        sub bl, al
        jns @Skip1
        mov bl, 0
       @Skip1:
        mov cl, bl
        mov eax, esp          // Subtract GREEN
        mov bl, dl
        and al, $1c
        and bl, $1c
        sub bl, al
        jns @Skip2
        mov bl, 0
       @Skip2:
        or cl, bl
        mov eax, esp          // Subtract RED
        mov bl, dl
        and ax, $e0
        and bx, $e0
        sub bx, ax
        jns @Skip3
        mov bl, 0
       @Skip3:
        or cl, bl
        mov [edi], cl
       @SkipColor:
        inc esi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    15 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax          // WorkX := Src.w
       @Loopx:
        mov ax, [esi]         // AX := source color
        cmp ax, 0
        je @SkipColor         // if AX=0 then skip everything
        mov esp, eax          // ESP - source color
        mov bx, [edi]         // BX := destination color
        mov dx, bx            // DX := destination color
        and ax, $001F         // Subtract BLUE
        and bx, $001F
        sub bx, ax
        jns @Skip1
        mov bx, 0
       @Skip1:
        mov cx, bx
        mov eax, esp          // Subtract GREEN
        mov bx, dx
        and ax, $3E0
        and bx, $3E0
        sub bx, ax
        jns @Skip2
        mov bx, 0
       @Skip2:
        or cx, bx
        mov eax, esp          // Subtract RED
        mov bx, dx
        and ax, $7C00
        and bx, $7C00
        sub bx, ax
        jns @Skip3
        mov bx, 0
       @Skip3:
        or cx, bx
        mov [edi], cx
       @SkipColor:
        add esi, 2
        add edi, 2
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    16 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov ax, [esi]         // AX := source color
        cmp ax, 0
        je @SkipColor         // if AX=0 then skip everything
        mov esp, eax          // ESP - source color
        mov bx, [edi]         // BX := destination color
        mov dx, bx            // DX := destination color
        and ax, $1F           // Subtracting BLUE
        and bx, $1F
        sub bx, ax
        jns @Skip1
        mov bx, 0
       @Skip1:
        mov cx, bx
        mov eax, esp          // Adding GREEN
        mov bx, dx
        and ax, $7E0
        and bx, $7E0
        sub bx, ax
        jns @Skip2
        mov bx, 0
       @Skip2:
        or cx, bx
        mov eax, esp          // Adding RED
        mov bx, dx
        and eax, $F800
        and ebx, $F800
        sub ebx, eax
        jns @Skip3
        mov bx, 0
       @Skip3:
        or cx, bx
        mov [edi], cx
       @SkipColor:
        add esi, 2
        add edi, 2
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    24 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       add WorkX, ax         // WorkX := Src.w * 2
       add WorkX, ax         // WorkX := Src.w * 3
       @Loopx:
        mov bl, [edi]         // BX := destination color
        mov al, [esi]         // AX := source color
        cmp al, 0
        je @Skip              // if AL=0 then skip COMPONENT
        mov ah, 0             // AX := COLOR COMPONENT
        mov bh, 0
        sub bx, ax
        jns @Skip
        mov bl, 0
       @Skip:
        mov [edi], bl
        inc esi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    32 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       shl ax, 2
       mov WorkX, ax         // WorkX := Src.w * 4
       @Loopx:
        mov bl, [edi]         // BX := destination color
        mov al, [esi]         // AX := source color
        cmp al, 0
        je @Skip              // if AL=0 then skip COMPONENT
        mov ah, 0             // AX := COLOR COMPONENT
        mov bh, 0
        sub bx, ax
        jns @Skip
        mov bl, 0
       @Skip:
        mov [edi], bl
        inc esi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
  end;
  SDL_UnlockSurface( SrcSurface );
  SDL_UnlockSurface( DstSurface );
end;

procedure SDL_MonoSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DstSurface : PSDL_Surface; DestRect : PSDL_Rect; Color : cardinal );
var
  Src, Dest : TSDL_Rect;
  Diff : integer;
  SrcAddr, DestAddr : cardinal;
  _ebx, _esi, _edi, _esp : cardinal;
  WorkX, WorkY : word;
  SrcMod, DestMod : cardinal;
  SrcTransparentColor : cardinal;
  Bits : cardinal;
begin
  if ( SrcSurface = nil ) or ( DstSurface = nil ) then
    exit; // Remove this to make it faster
  if ( SrcSurface.Format.BitsPerPixel <> DstSurface.Format.BitsPerPixel ) then
    exit; // Remove this to make it faster
  if SrcRect = nil then
  begin
    with Src do
    begin
      x := 0;
      y := 0;
      w := SrcSurface.w;
      h := SrcSurface.h;
    end;
  end
  else
    Src := SrcRect^;
  if DestRect = nil then
  begin
    Dest.x := 0;
    Dest.y := 0;
  end
  else
    Dest := DestRect^;
  Dest.w := Src.w;
  Dest.h := Src.h;
  with DstSurface.Clip_Rect do
  begin
    // Source's right side is greater than the dest.cliprect
    if Dest.x + Src.w > x + w then
    begin
      smallint( Src.w ) := x + w - Dest.x;
      smallint( Dest.w ) := x + w - Dest.x;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's bottom side is greater than the dest.clip
    if Dest.y + Src.h > y + h then
    begin
      smallint( Src.h ) := y + h - Dest.y;
      smallint( Dest.h ) := y + h - Dest.y;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
    // Source's left side is less than the dest.clip
    if Dest.x < x then
    begin
      Diff := x - Dest.x;
      Src.x := Src.x + Diff;
      smallint( Src.w ) := smallint( Src.w ) - Diff;
      Dest.x := x;
      smallint( Dest.w ) := smallint( Dest.w ) - Diff;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's Top side is less than the dest.clip
    if Dest.y < y then
    begin
      Diff := y - Dest.y;
      Src.y := Src.y + Diff;
      smallint( Src.h ) := smallint( Src.h ) - Diff;
      Dest.y := y;
      smallint( Dest.h ) := smallint( Dest.h ) - Diff;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
  end;
  with SrcSurface^ do
  begin
    SrcAddr := cardinal( Pixels ) + UInt32( Src.y ) * Pitch + UInt32( Src.x ) *
      Format.BytesPerPixel;
    SrcMod := Pitch - Src.w * Format.BytesPerPixel;
    SrcTransparentColor := format.colorkey;
  end;
  with DstSurface^ do
  begin
    DestAddr := cardinal( Pixels ) + UInt32( Dest.y ) * Pitch + UInt32( Dest.x ) *
      Format.BytesPerPixel;
    DestMod := Pitch - Dest.w * Format.BytesPerPixel;
    Bits := DstSurface.Format.BitsPerPixel;
  end;
  SDL_LockSurface( SrcSurface );
  SDL_LockSurface( DstSurface );
  case bits of
    8 :
      asm
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      mov ecx, Color
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov al, [esi]         // AL := source color
        movzx eax, al
        cmp eax, SrcTransparentColor
        je @SkipColor         // if AL=Transparent color then skip everything
        mov [edi], cl
       @SkipColor:
        inc esi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      end;
    15, 16 :
      asm
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      mov ecx, Color
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov ax, [esi]         // AX := source color
        movzx eax, ax
        cmp eax, SrcTransparentColor
        je @SkipColor         // if AX=Transparent color then skip everything
        mov [edi], cx
       @SkipColor:
        inc esi
        inc esi
        inc edi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      end;
    24 :
      asm
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov _ebx, ebx
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      mov ecx, Color
      and ecx, $00ffffff
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov eax, [esi]         // EAX := source color
        and eax, $00ffffff
        cmp eax, SrcTransparentColor
        je @SkipColor         // if EAX=Transparent color then skip everything
        mov ebx, [edi]
        and ebx, $ff000000
        or ebx, ecx
        mov [edi], ecx
       @SkipColor:
        add esi, 3
        add edi, 3
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp, _esp
      mov edi, _edi
      mov esi, _esi
      mov ebx, _ebx
      end;
    32 :
      asm
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      mov ecx, Color
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov eax, [esi]         // EAX := source color
        cmp eax, SrcTransparentColor
        je @SkipColor         // if EAX=Transparent color then skip everything
        mov [edi], ecx
       @SkipColor:
        add esi, 4
        add edi, 4
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp, _esp
      mov edi, _edi
      mov esi, _esi
      end;
  end;
  SDL_UnlockSurface( SrcSurface );
  SDL_UnlockSurface( DstSurface );
end;
// TextureRect.w and TextureRect.h are not used.
// The TextureSurface's size MUST larger than the drawing rectangle!!!

procedure SDL_TexturedSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DstSurface : PSDL_Surface; DestRect : PSDL_Rect; Texture : PSDL_Surface;
  TextureRect : PSDL_Rect );
var
  Src, Dest : TSDL_Rect;
  Diff : integer;
  SrcAddr, DestAddr, TextAddr : cardinal;
  _ebx, _esi, _edi, _esp : cardinal;
  WorkX, WorkY : word;
  SrcMod, DestMod, TextMod : cardinal;
  SrcTransparentColor : cardinal;
  Bits : cardinal;
begin
  if ( SrcSurface = nil ) or ( DstSurface = nil ) then
    exit; // Remove this to make it faster
  if ( SrcSurface.Format.BitsPerPixel <> DstSurface.Format.BitsPerPixel ) then
    exit; // Remove this to make it faster
  if SrcRect = nil then
  begin
    with Src do
    begin
      x := 0;
      y := 0;
      w := SrcSurface.w;
      h := SrcSurface.h;
    end;
  end
  else
    Src := SrcRect^;
  if DestRect = nil then
  begin
    Dest.x := 0;
    Dest.y := 0;
  end
  else
    Dest := DestRect^;
  Dest.w := Src.w;
  Dest.h := Src.h;
  with DstSurface.Clip_Rect do
  begin
    // Source's right side is greater than the dest.cliprect
    if Dest.x + Src.w > x + w then
    begin
      smallint( Src.w ) := x + w - Dest.x;
      smallint( Dest.w ) := x + w - Dest.x;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's bottom side is greater than the dest.clip
    if Dest.y + Src.h > y + h then
    begin
      smallint( Src.h ) := y + h - Dest.y;
      smallint( Dest.h ) := y + h - Dest.y;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
    // Source's left side is less than the dest.clip
    if Dest.x < x then
    begin
      Diff := x - Dest.x;
      Src.x := Src.x + Diff;
      smallint( Src.w ) := smallint( Src.w ) - Diff;
      Dest.x := x;
      smallint( Dest.w ) := smallint( Dest.w ) - Diff;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's Top side is less than the dest.clip
    if Dest.y < y then
    begin
      Diff := y - Dest.y;
      Src.y := Src.y + Diff;
      smallint( Src.h ) := smallint( Src.h ) - Diff;
      Dest.y := y;
      smallint( Dest.h ) := smallint( Dest.h ) - Diff;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
  end;
  with SrcSurface^ do
  begin
    SrcAddr := cardinal( Pixels ) + UInt32( Src.y ) * Pitch + UInt32( Src.x ) *
      Format.BytesPerPixel;
    SrcMod := Pitch - Src.w * Format.BytesPerPixel;
    SrcTransparentColor := format.colorkey;
  end;
  with DstSurface^ do
  begin
    DestAddr := cardinal( Pixels ) + UInt32( Dest.y ) * Pitch + UInt32( Dest.x ) *
      Format.BytesPerPixel;
    DestMod := Pitch - Dest.w * Format.BytesPerPixel;
    Bits := DstSurface.Format.BitsPerPixel;
  end;
  with Texture^ do
  begin
    TextAddr := cardinal( Pixels ) + UInt32( TextureRect.y ) * Pitch +
      UInt32( TextureRect.x ) * Format.BytesPerPixel;
    TextMod := Pitch - Src.w * Format.BytesPerPixel;
  end;
  SDL_LockSurface( SrcSurface );
  SDL_LockSurface( DstSurface );
  SDL_LockSurface( Texture );
  case bits of
    8 :
      asm
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov _ebx, ebx
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ebx, TextAddr
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov al, [esi]         // AL := source color
        movzx eax, al
        cmp eax, SrcTransparentColor
        je @SkipColor         // if AL=Transparent color then skip everything
        mov al, [ebx]
        mov [edi], al
       @SkipColor:
        inc esi
        inc edi
        inc ebx
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       add ebx, TextMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx, _ebx
      end;
    15, 16 :
      asm
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ecx, TextAddr
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov ax, [esi]         // AL := source color
        movzx eax, ax
        cmp eax, SrcTransparentColor
        je @SkipColor         // if AL=Transparent color then skip everything
        mov ax, [ecx]
        mov [edi], ax
       @SkipColor:
        inc esi
        inc esi
        inc edi
        inc edi
        inc ecx
        inc ecx
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       add ecx, TextMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      end;
    24 :
      asm
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov _ebx, ebx
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ebx, TextAddr
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov eax, [esi]         // AL := source color
        and eax, $00ffffff
        cmp eax, SrcTransparentColor
        je @SkipColor         // if AL=Transparent color then skip everything
        mov eax, [ebx]
        and eax, $00ffffff
        mov ecx, [edi]
        and ecx, $ff000000
        or ecx, eax
        mov [edi], eax
       @SkipColor:
        add esi, 3
        add edi, 3
        add ebx, 3
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       add ebx, TextMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx, _ebx
      end;
    32 :
      asm
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ecx, TextAddr
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov eax, [esi]         // AL := source color
        cmp eax, SrcTransparentColor
        je @SkipColor         // if AL=Transparent color then skip everything
        mov eax, [ecx]
        mov [edi], eax
       @SkipColor:
        add esi, 4
        add edi, 4
        add ecx, 4
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       add ecx, TextMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      end;
  end;
  SDL_UnlockSurface( SrcSurface );
  SDL_UnlockSurface( DstSurface );
  SDL_UnlockSurface( Texture );
end;

procedure SDL_ZoomSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect; DstSurface : PSDL_Surface; DstRect : PSDL_Rect );
var
  xc, yc : cardinal;
  rx, wx, ry, wy, ry16 : cardinal;
  color : cardinal;
  modx, mody : cardinal;
begin
  // Warning! No checks for surface pointers!!!
  if srcrect = nil then
    srcrect := @SrcSurface.clip_rect;
  if dstrect = nil then
    dstrect := @DstSurface.clip_rect;
  if SDL_MustLock( SrcSurface ) then
    SDL_LockSurface( SrcSurface );
  if SDL_MustLock( DstSurface ) then
    SDL_LockSurface( DstSurface );
  modx := trunc( ( srcrect.w / dstrect.w ) * 65536 );
  mody := trunc( ( srcrect.h / dstrect.h ) * 65536 );
  //rx := srcrect.x * 65536;
  ry := srcrect.y * 65536;
  wy := dstrect.y;
  for yc := 0 to dstrect.h - 1 do
  begin
    rx := srcrect.x * 65536;
    wx := dstrect.x;
    ry16 := ry shr 16;
    for xc := 0 to dstrect.w - 1 do
    begin
      color := SDL_GetPixel( SrcSurface, rx shr 16, ry16 );
      SDL_PutPixel( DstSurface, wx, wy, color );
      rx := rx + modx;
      inc( wx );
    end;
    ry := ry + mody;
    inc( wy );
  end;
  if SDL_MustLock( SrcSurface ) then
    SDL_UnlockSurface( SrcSurface );
  if SDL_MustLock( DstSurface ) then
    SDL_UnlockSurface( DstSurface );
end;
// Re-map a rectangular area into an area defined by four vertices
// Converted from C to Pascal by KiCHY

procedure SDL_WarpSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect; DstSurface : PSDL_Surface; UL, UR, LR, LL : PPoint );
const
  SHIFTS = 15; // Extend ints to limit round-off error (try 2 - 20)
  THRESH = 1 shl SHIFTS; // Threshold for pixel size value
  procedure CopySourceToDest( UL, UR, LR, LL : TPoint; x1, y1, x2, y2 : cardinal );
  var
    tm, lm, rm, bm, m : TPoint;
    mx, my : cardinal;
    cr : cardinal;
  begin
    // Does the destination area specify a single pixel?
    if ( ( abs( ul.x - ur.x ) < THRESH ) and
      ( abs( ul.x - lr.x ) < THRESH ) and
      ( abs( ul.x - ll.x ) < THRESH ) and
      ( abs( ul.y - ur.y ) < THRESH ) and
      ( abs( ul.y - lr.y ) < THRESH ) and
      ( abs( ul.y - ll.y ) < THRESH ) ) then
    begin // Yes
      cr := SDL_GetPixel( SrcSurface, ( x1 shr SHIFTS ), ( y1 shr SHIFTS ) );
      SDL_PutPixel( DstSurface, ( ul.x shr SHIFTS ), ( ul.y shr SHIFTS ), cr );
    end
    else
    begin // No
      // Quarter the source and the destination, and then recurse
      tm.x := ( ul.x + ur.x ) shr 1;
      tm.y := ( ul.y + ur.y ) shr 1;
      bm.x := ( ll.x + lr.x ) shr 1;
      bm.y := ( ll.y + lr.y ) shr 1;
      lm.x := ( ul.x + ll.x ) shr 1;
      lm.y := ( ul.y + ll.y ) shr 1;
      rm.x := ( ur.x + lr.x ) shr 1;
      rm.y := ( ur.y + lr.y ) shr 1;
      m.x := ( tm.x + bm.x ) shr 1;
      m.y := ( tm.y + bm.y ) shr 1;
      mx := ( x1 + x2 ) shr 1;
      my := ( y1 + y2 ) shr 1;
      CopySourceToDest( ul, tm, m, lm, x1, y1, mx, my );
      CopySourceToDest( tm, ur, rm, m, mx, y1, x2, my );
      CopySourceToDest( m, rm, lr, bm, mx, my, x2, y2 );
      CopySourceToDest( lm, m, bm, ll, x1, my, mx, y2 );
    end;
  end;
var
  _UL, _UR, _LR, _LL : TPoint;
  Rect_x, Rect_y, Rect_w, Rect_h : integer;
begin
  if SDL_MustLock( SrcSurface ) then
    SDL_LockSurface( SrcSurface );
  if SDL_MustLock( DstSurface ) then
    SDL_LockSurface( DstSurface );
  if SrcRect = nil then
  begin
    Rect_x := 0;
    Rect_y := 0;
    Rect_w := ( SrcSurface.w - 1 ) shl SHIFTS;
    Rect_h := ( SrcSurface.h - 1 ) shl SHIFTS;
  end
  else
  begin
    Rect_x := SrcRect.x;
    Rect_y := SrcRect.y;
    Rect_w := ( SrcRect.w - 1 ) shl SHIFTS;
    Rect_h := ( SrcRect.h - 1 ) shl SHIFTS;
  end;
  // Shift all values to help reduce round-off error.
  _ul.x := ul.x shl SHIFTS;
  _ul.y := ul.y shl SHIFTS;
  _ur.x := ur.x shl SHIFTS;
  _ur.y := ur.y shl SHIFTS;
  _lr.x := lr.x shl SHIFTS;
  _lr.y := lr.y shl SHIFTS;
  _ll.x := ll.x shl SHIFTS;
  _ll.y := ll.y shl SHIFTS;
  CopySourceToDest( _ul, _ur, _lr, _ll, Rect_x, Rect_y, Rect_w, Rect_h );
  if SDL_MustLock( SrcSurface ) then
    SDL_UnlockSurface( SrcSurface );
  if SDL_MustLock( DstSurface ) then
    SDL_UnlockSurface( DstSurface );
end;

// flips a rectangle vertically on given surface
procedure SDL_FlipRectV( DstSurface : PSDL_Surface; Rect : PSDL_Rect );
var
  TmpRect : TSDL_Rect;
  Locked : boolean;
  y, FlipLength, RowLength : integer;
  Row1, Row2 : Pointer;
  OneRow : TByteArray; // Optimize it if you wish
begin
  if DstSurface <> nil then
  begin
    if Rect = nil then
    begin // if Rect=nil then we flip the whole surface
      TmpRect := SDLRect( 0, 0, DstSurface.w, DstSurface.h );
      Rect := @TmpRect;
    end;
    FlipLength := Rect^.h shr 1 - 1;
    RowLength := Rect^.w * DstSurface^.format.BytesPerPixel;
    if SDL_MustLock( DstSurface ) then
    begin
      Locked := true;
      SDL_LockSurface( DstSurface );
    end
    else
      Locked := false;
    Row1 := pointer( cardinal( DstSurface^.Pixels ) + UInt32( Rect^.y ) *
      DstSurface^.Pitch );
    Row2 := pointer( cardinal( DstSurface^.Pixels ) + ( UInt32( Rect^.y ) + Rect^.h - 1 )
      * DstSurface^.Pitch );
    for y := 0 to FlipLength do
    begin
      Move( Row1^, OneRow, RowLength );
      Move( Row2^, Row1^, RowLength );
      Move( OneRow, Row2^, RowLength );
      inc( cardinal( Row1 ), DstSurface^.Pitch );
      dec( cardinal( Row2 ), DstSurface^.Pitch );
    end;
    if Locked then
      SDL_UnlockSurface( DstSurface );
  end;
end;

// flips a rectangle horizontally on given surface
procedure SDL_FlipRectH( DstSurface : PSDL_Surface; Rect : PSDL_Rect );
type
  T24bit = packed array[ 0..2 ] of byte;
  T24bitArray = packed array[ 0..8191 ] of T24bit;
  P24bitArray = ^T24bitArray;
  TLongWordArray = array[ 0..8191 ] of LongWord;
  PLongWordArray = ^TLongWordArray;
var
  TmpRect : TSDL_Rect;
  Row8bit : PByteArray;
  Row16bit : PWordArray;
  Row24bit : P24bitArray;
  Row32bit : PLongWordArray;
  y, x, RightSide, FlipLength : integer;
  Pixel : cardinal;
  Pixel24 : T24bit;
  Locked : boolean;
begin
  if DstSurface <> nil then
  begin
    if Rect = nil then
    begin
      TmpRect := SDLRect( 0, 0, DstSurface.w, DstSurface.h );
      Rect := @TmpRect;
    end;
    FlipLength := Rect^.w shr 1 - 1;
    if SDL_MustLock( DstSurface ) then
    begin
      Locked := true;
      SDL_LockSurface( DstSurface );
    end
    else
      Locked := false;
    case DstSurface^.format.BytesPerPixel of
      1 :
        begin
          Row8Bit := pointer( cardinal( DstSurface^.pixels ) + UInt32( Rect^.y ) *
            DstSurface^.pitch );
          for y := 1 to Rect^.h do
          begin
            RightSide := Rect^.w - 1;
            for x := 0 to FlipLength do
            begin
              Pixel := Row8Bit^[ x ];
              Row8Bit^[ x ] := Row8Bit^[ RightSide ];
              Row8Bit^[ RightSide ] := Pixel;
              dec( RightSide );
            end;
            inc( cardinal( Row8Bit ), DstSurface^.pitch );
          end;
        end;
      2 :
        begin
          Row16Bit := pointer( cardinal( DstSurface^.pixels ) + UInt32( Rect^.y ) *
            DstSurface^.pitch );
          for y := 1 to Rect^.h do
          begin
            RightSide := Rect^.w - 1;
            for x := 0 to FlipLength do
            begin
              Pixel := Row16Bit^[ x ];
              Row16Bit^[ x ] := Row16Bit^[ RightSide ];
              Row16Bit^[ RightSide ] := Pixel;
              dec( RightSide );
            end;
            inc( cardinal( Row16Bit ), DstSurface^.pitch );
          end;
        end;
      3 :
        begin
          Row24Bit := pointer( cardinal( DstSurface^.pixels ) + UInt32( Rect^.y ) *
            DstSurface^.pitch );
          for y := 1 to Rect^.h do
          begin
            RightSide := Rect^.w - 1;
            for x := 0 to FlipLength do
            begin
              Pixel24 := Row24Bit^[ x ];
              Row24Bit^[ x ] := Row24Bit^[ RightSide ];
              Row24Bit^[ RightSide ] := Pixel24;
              dec( RightSide );
            end;
            inc( cardinal( Row24Bit ), DstSurface^.pitch );
          end;
        end;
      4 :
        begin
          Row32Bit := pointer( cardinal( DstSurface^.pixels ) + UInt32( Rect^.y ) *
            DstSurface^.pitch );
          for y := 1 to Rect^.h do
          begin
            RightSide := Rect^.w - 1;
            for x := 0 to FlipLength do
            begin
              Pixel := Row32Bit^[ x ];
              Row32Bit^[ x ] := Row32Bit^[ RightSide ];
              Row32Bit^[ RightSide ] := Pixel;
              dec( RightSide );
            end;
            inc( cardinal( Row32Bit ), DstSurface^.pitch );
          end;
        end;
    end;
    if Locked then
      SDL_UnlockSurface( DstSurface );
  end;
end;

// Use with caution! The procedure allocates memory for TSDL_Rect and return with its pointer.
// But you MUST free it after you don't need it anymore!!!
function PSDLRect( aLeft, aTop, aWidth, aHeight : integer ) : PSDL_Rect;
var
  Rect : PSDL_Rect;
begin
  New( Rect );
  with Rect^ do
  begin
    x := aLeft;
    y := aTop;
    w := aWidth;
    h := aHeight;
  end;
  Result := Rect;
end;

function SDLRect( aLeft, aTop, aWidth, aHeight : integer ) : TSDL_Rect;
begin
  with result do
  begin
    x := aLeft;
    y := aTop;
    w := aWidth;
    h := aHeight;
  end;
end;

function SDLRect( aRect : TRect ) : TSDL_Rect;
begin
  with aRect do
    result := SDLRect( Left, Top, Right - Left, Bottom - Top );
end;

procedure SDL_Stretch8( Surface, Dst_Surface : PSDL_Surface; x1, x2, y1, y2, yr, yw,
  depth : integer );
var
  dx, dy, e, d, dx2 : integer;
  src_pitch, dst_pitch : uint16;
  src_pixels, dst_pixels : PUint8;
begin
  if ( yw >= dst_surface^.h ) then
    exit;
  dx := ( x2 - x1 );
  dy := ( y2 - y1 );
  dy := dy shl 1;
  e := dy - dx;
  dx2 := dx shl 1;
  src_pitch := Surface^.pitch;
  dst_pitch := dst_surface^.pitch;
  src_pixels := PUint8( integer( Surface^.pixels ) + yr * src_pitch + y1 * depth );
  dst_pixels := PUint8( integer( dst_surface^.pixels ) + yw * dst_pitch + x1 *
    depth );
  for d := 0 to dx - 1 do
  begin
    move( src_pixels^, dst_pixels^, depth );
    while ( e >= 0 ) do
    begin
      inc( src_pixels, depth );
      e := e - dx2;
    end;
    inc( dst_pixels, depth );
    e := e + dy;
  end;
end;

function sign( x : integer ) : integer;
begin
  if x > 0 then
    result := 1
  else
    result := -1;
end;

// Stretches a part of a surface
function SDL_ScaleSurfaceRect( SrcSurface : PSDL_Surface; SrcX1, SrcY1, SrcW, SrcH,
  Width, Height : integer ) : PSDL_Surface;
var
  dst_surface : PSDL_Surface;
  dx, dy, e, d, dx2, srcx2, srcy2 : integer;
  destx1, desty1 : integer;
begin
  srcx2 := srcx1 + SrcW;
  srcy2 := srcy1 + SrcH;
  result := nil;
  destx1 := 0;
  desty1 := 0;
  dx := abs( integer( Height - desty1 ) );
  dy := abs( integer( SrcY2 - SrcY1 ) );
  e := ( dy shl 1 ) - dx;
  dx2 := dx shl 1;
  dy := dy shl 1;
  dst_surface := SDL_CreateRGBSurface( SDL_HWPALETTE, width - destx1, Height -
      desty1,
      SrcSurface^.Format^.BitsPerPixel,
      SrcSurface^.Format^.RMask,
      SrcSurface^.Format^.GMask,
      SrcSurface^.Format^.BMask,
      SrcSurface^.Format^.AMask );
  if ( dst_surface^.format^.BytesPerPixel = 1 ) then
    SDL_SetColors( dst_surface, @SrcSurface^.format^.palette^.colors^[ 0 ], 0, 256 );
  SDL_SetColorKey( dst_surface, sdl_srccolorkey, SrcSurface^.format^.colorkey );
  if ( SDL_MustLock( dst_surface ) ) then
    if ( SDL_LockSurface( dst_surface ) < 0 ) then
      exit;
  for d := 0 to dx - 1 do
  begin
    SDL_Stretch8( SrcSurface, dst_surface, destx1, Width, SrcX1, SrcX2, SrcY1, desty1,
      SrcSurface^.format^.BytesPerPixel );
    while e >= 0 do
    begin
      inc( SrcY1 );
      e := e - dx2;
    end;
    inc( desty1 );
    e := e + dy;
  end;
  if SDL_MUSTLOCK( dst_surface ) then
    SDL_UnlockSurface( dst_surface );
  result := dst_surface;
end;

procedure SDL_ScrollY( DstSurface : PSDL_Surface; DifY : integer );
var
  r1, r2 : TSDL_Rect;
  //buffer: PSDL_Surface;
  YPos : Integer;
begin
  if ( DstSurface <> nil ) and ( DifY <> 0 ) then
  begin
    //if DifY > 0 then // going up
    //begin
    ypos := 0;
    r1.x := 0;
    r2.x := 0;
    r1.w := DstSurface.w;
    r2.w := DstSurface.w;
    r1.h := DifY;
    r2.h := DifY;
    while ypos < DstSurface.h do
    begin
      r1.y := ypos;
      r2.y := ypos + DifY;
      SDL_BlitSurface( DstSurface, @r2, DstSurface, @r1 );
      ypos := ypos + DifY;
    end;
    //end
    //else
    //begin // Going Down
    //end;
  end;
end;

procedure SDL_ScrollX( DstSurface : PSDL_Surface; DifX : integer );
var
  r1, r2 : TSDL_Rect;
  buffer : PSDL_Surface;
begin
  if ( DstSurface <> nil ) and ( DifX <> 0 ) then
  begin
    buffer := SDL_CreateRGBSurface( SDL_HWSURFACE, ( DstSurface^.w - DifX ) * 2,
      DstSurface^.h * 2,
      DstSurface^.Format^.BitsPerPixel,
      DstSurface^.Format^.RMask,
      DstSurface^.Format^.GMask,
      DstSurface^.Format^.BMask,
      DstSurface^.Format^.AMask );
    if buffer <> nil then
    begin
      if ( buffer^.format^.BytesPerPixel = 1 ) then
        SDL_SetColors( buffer, @DstSurface^.format^.palette^.colors^[ 0 ], 0, 256 );
      r1 := SDLRect( DifX, 0, buffer^.w, buffer^.h );
      r2 := SDLRect( 0, 0, buffer^.w, buffer^.h );
      SDL_BlitSurface( DstSurface, @r1, buffer, @r2 );
      SDL_BlitSurface( buffer, @r2, DstSurface, @r2 );
      SDL_FreeSurface( buffer );
    end;
  end;
end;

procedure SDL_RotateRad( DstSurface, SrcSurface : PSDL_Surface; SrcRect :
  PSDL_Rect; DestX, DestY, OffsetX, OffsetY : Integer; Angle : Single );
var
  aSin, aCos : Single;
  MX, MY, DX, DY, NX, NY, SX, SY, OX, OY, Width, Height, TX, TY, RX, RY, ROX, ROY : Integer;
  Colour, TempTransparentColour : UInt32;
  MAXX, MAXY : Integer;
begin
  // Rotate the surface to the target surface.
  TempTransparentColour := SrcSurface.format.colorkey;
  if srcRect.w > srcRect.h then
  begin
    Width := srcRect.w;
    Height := srcRect.w;
  end
  else
  begin
    Width := srcRect.h;
    Height := srcRect.h;
  end;

  maxx := DstSurface.w;
  maxy := DstSurface.h;
  aCos := cos( Angle );
  aSin := sin( Angle );

  Width := round( abs( srcrect.h * acos ) + abs( srcrect.w * asin ) );
  Height := round( abs( srcrect.h * asin ) + abs( srcrect.w * acos ) );

  OX := Width div 2;
  OY := Height div 2; ;
  MX := ( srcRect.x + ( srcRect.x + srcRect.w ) ) div 2;
  MY := ( srcRect.y + ( srcRect.y + srcRect.h ) ) div 2;
  ROX := ( -( srcRect.w div 2 ) ) + Offsetx;
  ROY := ( -( srcRect.h div 2 ) ) + OffsetY;
  Tx := ox + round( ROX * aSin - ROY * aCos );
  Ty := oy + round( ROY * aSin + ROX * aCos );
  SX := 0;
  for DX := DestX - TX to DestX - TX + ( width ) do
  begin
    Inc( SX );
    SY := 0;
    for DY := DestY - TY to DestY - TY + ( Height ) do
    begin
      RX := SX - OX;
      RY := SY - OY;
      NX := round( mx + RX * aSin + RY * aCos ); //
      NY := round( my + RY * aSin - RX * aCos ); //
      // Used for testing only
     //SDL_PutPixel(DstSurface.SDLSurfacePointer,DX,DY,0);
      if ( ( DX > 0 ) and ( DX < MAXX ) ) and ( ( DY > 0 ) and ( DY < MAXY ) ) then
      begin
        if ( NX >= srcRect.x ) and ( NX <= srcRect.x + srcRect.w ) then
        begin
          if ( NY >= srcRect.y ) and ( NY <= srcRect.y + srcRect.h ) then
          begin
            Colour := SDL_GetPixel( SrcSurface, NX, NY );
            if Colour <> TempTransparentColour then
            begin
              SDL_PutPixel( DstSurface, DX, DY, Colour );
            end;
          end;
        end;
      end;
      inc( SY );
    end;
  end;
end;

procedure SDL_RotateDeg( DstSurface, SrcSurface : PSDL_Surface; SrcRect :
  PSDL_Rect; DestX, DestY, OffsetX, OffsetY : Integer; Angle : Integer );
begin
  SDL_RotateRad( DstSurface, SrcSurface, SrcRect, DestX, DestY, OffsetX, OffsetY, DegToRad( Angle ) );
end;

function ValidateSurfaceRect( DstSurface : PSDL_Surface; dstrect : PSDL_Rect ) : TSDL_Rect;
var
  RealRect : TSDL_Rect;
  OutOfRange : Boolean;
begin
  OutOfRange := false;
  if dstrect = nil then
  begin
    RealRect.x := 0;
    RealRect.y := 0;
    RealRect.w := DstSurface.w;
    RealRect.h := DstSurface.h;
  end
  else
  begin
    if dstrect.x < DstSurface.w then
    begin
      RealRect.x := dstrect.x;
    end
    else if dstrect.x < 0 then
    begin
      realrect.x := 0;
    end
    else
    begin
      OutOfRange := True;
    end;
    if dstrect.y < DstSurface.h then
    begin
      RealRect.y := dstrect.y;
    end
    else if dstrect.y < 0 then
    begin
      realrect.y := 0;
    end
    else
    begin
      OutOfRange := True;
    end;
    if OutOfRange = False then
    begin
      if realrect.x + dstrect.w <= DstSurface.w then
      begin
        RealRect.w := dstrect.w;
      end
      else
      begin
        RealRect.w := dstrect.w - realrect.x;
      end;
      if realrect.y + dstrect.h <= DstSurface.h then
      begin
        RealRect.h := dstrect.h;
      end
      else
      begin
        RealRect.h := dstrect.h - realrect.y;
      end;
    end;
  end;
  if OutOfRange = False then
  begin
    result := realrect;
  end
  else
  begin
    realrect.w := 0;
    realrect.h := 0;
    realrect.x := 0;
    realrect.y := 0;
    result := realrect;
  end;
end;

procedure SDL_FillRectAdd( DstSurface : PSDL_Surface; dstrect : PSDL_Rect; color : UInt32 );
var
  RealRect : TSDL_Rect;
  Addr : pointer;
  ModX, BPP : cardinal;
  x, y, R, G, B, SrcColor : cardinal;
begin
  RealRect := ValidateSurfaceRect( DstSurface, DstRect );
  if ( RealRect.w > 0 ) and ( RealRect.h > 0 ) then
  begin
    SDL_LockSurface( DstSurface );
    BPP := DstSurface.format.BytesPerPixel;
    with DstSurface^ do
    begin
      Addr := pointer( UInt32( pixels ) + UInt32( RealRect.y ) * pitch + UInt32( RealRect.x ) * BPP );
      ModX := Pitch - UInt32( RealRect.w ) * BPP;
    end;
    case DstSurface.format.BitsPerPixel of
      8 :
        begin
          for y := 0 to RealRect.h - 1 do
          begin
            for x := 0 to RealRect.w - 1 do
            begin
              SrcColor := PUInt32( Addr )^;
              R := SrcColor and $E0 + Color and $E0;
              G := SrcColor and $1C + Color and $1C;
              B := SrcColor and $03 + Color and $03;
              if R > $E0 then
                R := $E0;
              if G > $1C then
                G := $1C;
              if B > $03 then
                B := $03;
              PUInt8( Addr )^ := R or G or B;
              inc( UInt32( Addr ), BPP );
            end;
            inc( UInt32( Addr ), ModX );
          end;
        end;
      15 :
        begin
          for y := 0 to RealRect.h - 1 do
          begin
            for x := 0 to RealRect.w - 1 do
            begin
              SrcColor := PUInt32( Addr )^;
              R := SrcColor and $7C00 + Color and $7C00;
              G := SrcColor and $03E0 + Color and $03E0;
              B := SrcColor and $001F + Color and $001F;
              if R > $7C00 then
                R := $7C00;
              if G > $03E0 then
                G := $03E0;
              if B > $001F then
                B := $001F;
              PUInt16( Addr )^ := R or G or B;
              inc( UInt32( Addr ), BPP );
            end;
            inc( UInt32( Addr ), ModX );
          end;
        end;
      16 :
        begin
          for y := 0 to RealRect.h - 1 do
          begin
            for x := 0 to RealRect.w - 1 do
            begin
              SrcColor := PUInt32( Addr )^;
              R := SrcColor and $F800 + Color and $F800;
              G := SrcColor and $07C0 + Color and $07C0;
              B := SrcColor and $001F + Color and $001F;
              if R > $F800 then
                R := $F800;
              if G > $07C0 then
                G := $07C0;
              if B > $001F then
                B := $001F;
              PUInt16( Addr )^ := R or G or B;
              inc( UInt32( Addr ), BPP );
            end;
            inc( UInt32( Addr ), ModX );
          end;
        end;
      24 :
        begin
          for y := 0 to RealRect.h - 1 do
          begin
            for x := 0 to RealRect.w - 1 do
            begin
              SrcColor := PUInt32( Addr )^;
              R := SrcColor and $00FF0000 + Color and $00FF0000;
              G := SrcColor and $0000FF00 + Color and $0000FF00;
              B := SrcColor and $000000FF + Color and $000000FF;
              if R > $FF0000 then
                R := $FF0000;
              if G > $00FF00 then
                G := $00FF00;
              if B > $0000FF then
                B := $0000FF;
              PUInt32( Addr )^ := SrcColor and $FF000000 or R or G or B;
              inc( UInt32( Addr ), BPP );
            end;
            inc( UInt32( Addr ), ModX );
          end;
        end;
      32 :
        begin
          for y := 0 to RealRect.h - 1 do
          begin
            for x := 0 to RealRect.w - 1 do
            begin
              SrcColor := PUInt32( Addr )^;
              R := SrcColor and $00FF0000 + Color and $00FF0000;
              G := SrcColor and $0000FF00 + Color and $0000FF00;
              B := SrcColor and $000000FF + Color and $000000FF;
              if R > $FF0000 then
                R := $FF0000;
              if G > $00FF00 then
                G := $00FF00;
              if B > $0000FF then
                B := $0000FF;
              PUInt32( Addr )^ := R or G or B;
              inc( UInt32( Addr ), BPP );
            end;
            inc( UInt32( Addr ), ModX );
          end;
        end;
    end;
    SDL_UnlockSurface( DstSurface );
  end;
end;

procedure SDL_FillRectSub( DstSurface : PSDL_Surface; dstrect : PSDL_Rect; color : UInt32 );
var
  RealRect : TSDL_Rect;
  Addr : pointer;
  ModX, BPP : cardinal;
  x, y, R, G, B, SrcColor : cardinal;
begin
  RealRect := ValidateSurfaceRect( DstSurface, DstRect );
  if ( RealRect.w > 0 ) and ( RealRect.h > 0 ) then
  begin
    SDL_LockSurface( DstSurface );
    BPP := DstSurface.format.BytesPerPixel;
    with DstSurface^ do
    begin
      Addr := pointer( UInt32( pixels ) + UInt32( RealRect.y ) * pitch + UInt32( RealRect.x ) * BPP );
      ModX := Pitch - UInt32( RealRect.w ) * BPP;
    end;
    case DstSurface.format.BitsPerPixel of
      8 :
        begin
          for y := 0 to RealRect.h - 1 do
          begin
            for x := 0 to RealRect.w - 1 do
            begin
              SrcColor := PUInt32( Addr )^;
              R := SrcColor and $E0 - Color and $E0;
              G := SrcColor and $1C - Color and $1C;
              B := SrcColor and $03 - Color and $03;
              if R > $E0 then
                R := 0;
              if G > $1C then
                G := 0;
              if B > $03 then
                B := 0;
              PUInt8( Addr )^ := R or G or B;
              inc( UInt32( Addr ), BPP );
            end;
            inc( UInt32( Addr ), ModX );
          end;
        end;
      15 :
        begin
          for y := 0 to RealRect.h - 1 do
          begin
            for x := 0 to RealRect.w - 1 do
            begin
              SrcColor := PUInt32( Addr )^;
              R := SrcColor and $7C00 - Color and $7C00;
              G := SrcColor and $03E0 - Color and $03E0;
              B := SrcColor and $001F - Color and $001F;
              if R > $7C00 then
                R := 0;
              if G > $03E0 then
                G := 0;
              if B > $001F then
                B := 0;
              PUInt16( Addr )^ := R or G or B;
              inc( UInt32( Addr ), BPP );
            end;
            inc( UInt32( Addr ), ModX );
          end;
        end;
      16 :
        begin
          for y := 0 to RealRect.h - 1 do
          begin
            for x := 0 to RealRect.w - 1 do
            begin
              SrcColor := PUInt32( Addr )^;
              R := SrcColor and $F800 - Color and $F800;
              G := SrcColor and $07C0 - Color and $07C0;
              B := SrcColor and $001F - Color and $001F;
              if R > $F800 then
                R := 0;
              if G > $07C0 then
                G := 0;
              if B > $001F then
                B := 0;
              PUInt16( Addr )^ := R or G or B;
              inc( UInt32( Addr ), BPP );
            end;
            inc( UInt32( Addr ), ModX );
          end;
        end;
      24 :
        begin
          for y := 0 to RealRect.h - 1 do
          begin
            for x := 0 to RealRect.w - 1 do
            begin
              SrcColor := PUInt32( Addr )^;
              R := SrcColor and $00FF0000 - Color and $00FF0000;
              G := SrcColor and $0000FF00 - Color and $0000FF00;
              B := SrcColor and $000000FF - Color and $000000FF;
              if R > $FF0000 then
                R := 0;
              if G > $00FF00 then
                G := 0;
              if B > $0000FF then
                B := 0;
              PUInt32( Addr )^ := SrcColor and $FF000000 or R or G or B;
              inc( UInt32( Addr ), BPP );
            end;
            inc( UInt32( Addr ), ModX );
          end;
        end;
      32 :
        begin
          for y := 0 to RealRect.h - 1 do
          begin
            for x := 0 to RealRect.w - 1 do
            begin
              SrcColor := PUInt32( Addr )^;
              R := SrcColor and $00FF0000 - Color and $00FF0000;
              G := SrcColor and $0000FF00 - Color and $0000FF00;
              B := SrcColor and $000000FF - Color and $000000FF;
              if R > $FF0000 then
                R := 0;
              if G > $00FF00 then
                G := 0;
              if B > $0000FF then
                B := 0;
              PUInt32( Addr )^ := R or G or B;
              inc( UInt32( Addr ), BPP );
            end;
            inc( UInt32( Addr ), ModX );
          end;
        end;
    end;
    SDL_UnlockSurface( DstSurface );
  end;
end;

procedure SDL_GradientFillRect( DstSurface : PSDL_Surface; const Rect : PSDL_Rect; const StartColor, EndColor : TSDL_Color; const Style : TGradientStyle );
var
  FBC : array[ 0..255 ] of Cardinal;
  // temp vars
  i, YR, YG, YB, SR, SG, SB, DR, DG, DB : Integer;

  TempStepV, TempStepH : Single;
  TempLeft, TempTop, TempHeight, TempWidth : integer;
  TempRect : TSDL_Rect;

begin
  // calc FBC
  YR := StartColor.r;
  YG := StartColor.g;
  YB := StartColor.b;
  SR := YR;
  SG := YG;
  SB := YB;
  DR := EndColor.r - SR;
  DG := EndColor.g - SG;
  DB := EndColor.b - SB;

  for i := 0 to 255 do
  begin
    FBC[ i ] := SDL_MapRGB( DstSurface.format, YR, YG, YB );
    YR := SR + round( DR / 255 * i );
    YG := SG + round( DG / 255 * i );
    YB := SB + round( DB / 255 * i );
  end;

  //  if aStyle = 1 then begin
  TempStepH := Rect.w / 255;
  TempStepV := Rect.h / 255;
  TempHeight := Trunc( TempStepV + 1 );
  TempWidth := Trunc( TempStepH + 1 );
  TempTop := 0;
  TempLeft := 0;
  TempRect.x := Rect.x;
  TempRect.y := Rect.y;
  TempRect.h := Rect.h;
  TempRect.w := Rect.w;

  case Style of
    gsHorizontal :
      begin
        TempRect.h := TempHeight;
        for i := 0 to 255 do
        begin
          TempRect.y := Rect.y + TempTop;
          SDL_FillRect( DstSurface, @TempRect, FBC[ i ] );
          TempTop := Trunc( TempStepV * i );
        end;
      end;
    gsVertical :
      begin
        TempRect.w := TempWidth;
        for i := 0 to 255 do
        begin
          TempRect.x := Rect.x + TempLeft;
          SDL_FillRect( DstSurface, @TempRect, FBC[ i ] );
          TempLeft := Trunc( TempStepH * i );
        end;
      end;
  end;
end;

procedure SDL_2xBlit( Src, Dest : PSDL_Surface );
var
  ReadAddr, WriteAddr, ReadRow, WriteRow : UInt32;
  SrcPitch, DestPitch, x, y, w, h : UInt32;
begin
  if ( Src = nil ) or ( Dest = nil ) then
    exit;
  if ( Src.w shl 1 ) < Dest.w then
    exit;
  if ( Src.h shl 1 ) < Dest.h then
    exit;

  if SDL_MustLock( Src ) then
    SDL_LockSurface( Src );
  if SDL_MustLock( Dest ) then
    SDL_LockSurface( Dest );

  ReadRow := UInt32( Src.Pixels );
  WriteRow := UInt32( Dest.Pixels );

  SrcPitch := Src.pitch;
  DestPitch := Dest.pitch;

  w := Src.w;
  h := Src.h;

  case Src.format.BytesPerPixel of
    1 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr
           mov ebx, DestPitch

           @LoopX:
             mov al, [ecx] // PUInt8(WriteAddr)^ := PUInt8(ReadAddr)^;
             mov [edx], al
             mov [edx + 1], al // PUInt8(WriteAddr + 1)^ := PUInt8(ReadAddr)^;
             mov [edx + ebx], al // PUInt8(WriteAddr + DestPitch)^ := PUInt8(ReadAddr)^;
             mov [edx + ebx + 1], al // PUInt8(WriteAddr + DestPitch + 1)^ := PUInt8(ReadAddr)^;

             inc ecx // inc(ReadAddr);
             add edx, 2 // inc(WriteAddr, 2);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
    2 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr
           mov ebx, DestPitch

           @LoopX:
             mov ax, [ecx] // PUInt16(WriteAddr)^ := PUInt16(ReadAddr)^;
             mov [edx], ax
             mov [edx + 2], ax // PUInt16(WriteAddr + 2)^ := PUInt16(ReadAddr)^;
             mov [edx + ebx], ax // PUInt16(WriteAddr + DestPitch)^ := PUInt16(ReadAddr)^;
             mov [edx + ebx + 2], ax // PUInt16(WriteAddr + DestPitch + 2)^ := PUInt16(ReadAddr)^;

             add ecx, 2 // inc(ReadAddr, 2);
             add edx, 4 // inc(WriteAddr, 4);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
    3 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr
           mov ebx, DestPitch

           @LoopX:
             mov eax, [ecx] // (PUInt32(WriteAddr)^ and $ff000000) or (PUInt32(ReadAddr)^ and $00ffffff);
             and eax, $00ffffff
             and dword ptr [edx], $ff000000
             or [edx], eax
             and dword ptr [edx + 3], $00ffffff // (PUInt32(WriteAddr + 3)^ and $ff000000) or (PUInt32(ReadAddr)^ and $00ffffff);
             or [edx + 3], eax
             and dword ptr [edx + ebx], $00ffffff // (PUInt32(WriteAddr + DestPitch)^ and $ff000000) or (PUInt32(ReadAddr)^ and $00ffffff);
             or [edx + ebx], eax
             and dword ptr [edx + ebx + 3], $00ffffff // (PUInt32(WriteAddr + DestPitch + 3)^ and $ff000000) or (PUInt32(ReadAddr)^ and $00ffffff);
             or [edx + ebx + 3], eax

             add ecx, 3 // inc(ReadAddr, 3);
             add edx, 6 // inc(WriteAddr, 6);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
    4 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr
           mov ebx, DestPitch

           @LoopX:
             mov eax, [ecx] // PUInt32(WriteAddr)^ := PUInt32(ReadAddr)^;
             mov [edx], eax
             mov [edx + 4], eax // PUInt32(WriteAddr + 4)^ := PUInt32(ReadAddr)^;
             mov [edx + ebx], eax // PUInt32(WriteAddr + DestPitch)^ := PUInt32(ReadAddr)^;
             mov [edx + ebx + 4], eax // PUInt32(WriteAddr + DestPitch + 4)^ := PUInt32(ReadAddr)^;

             add ecx, 4 // inc(ReadAddr, 4);
             add edx, 8 // inc(WriteAddr, 8);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
  end;

  if SDL_MustLock( Src ) then
    SDL_UnlockSurface( Src );
  if SDL_MustLock( Dest ) then
    SDL_UnlockSurface( Dest );
end;

procedure SDL_Scanline2xBlit( Src, Dest : PSDL_Surface );
var
  ReadAddr, WriteAddr, ReadRow, WriteRow : UInt32;
  SrcPitch, DestPitch, x, y, w, h : UInt32;
begin
  if ( Src = nil ) or ( Dest = nil ) then
    exit;
  if ( Src.w shl 1 ) < Dest.w then
    exit;
  if ( Src.h shl 1 ) < Dest.h then
    exit;

  if SDL_MustLock( Src ) then
    SDL_LockSurface( Src );
  if SDL_MustLock( Dest ) then
    SDL_LockSurface( Dest );

  ReadRow := UInt32( Src.Pixels );
  WriteRow := UInt32( Dest.Pixels );

  SrcPitch := Src.pitch;
  DestPitch := Dest.pitch;

  w := Src.w;
  h := Src.h;

  case Src.format.BytesPerPixel of
    1 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr

           @LoopX:
             mov al, [ecx] // PUInt8(WriteAddr)^ := PUInt8(ReadAddr)^;
             mov [edx], al
             mov [edx + 1], al // PUInt8(WriteAddr + 1)^ := PUInt8(ReadAddr)^;

             inc ecx // inc(ReadAddr);
             add edx, 2 // inc(WriteAddr, 2);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
    2 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr

           @LoopX:
             mov ax, [ecx] // PUInt16(WriteAddr)^ := PUInt16(ReadAddr)^;
             mov [edx], ax
             mov [edx + 2], eax // PUInt16(WriteAddr + 2)^ := PUInt16(ReadAddr)^;

             add ecx, 2 // inc(ReadAddr, 2);
             add edx, 4 // inc(WriteAddr, 4);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
    3 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr

           @LoopX:
             mov eax, [ecx] // (PUInt32(WriteAddr)^ and $ff000000) or (PUInt32(ReadAddr)^ and $00ffffff);
             and eax, $00ffffff
             and dword ptr [edx], $ff000000
             or [edx], eax
             and dword ptr [edx + 3], $00ffffff // (PUInt32(WriteAddr + 3)^ and $ff000000) or (PUInt32(ReadAddr)^ and $00ffffff);
             or [edx + 3], eax

             add ecx, 3 // inc(ReadAddr, 3);
             add edx, 6 // inc(WriteAddr, 6);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
    4 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr

           @LoopX:
             mov eax, [ecx] // PUInt32(WriteAddr)^ := PUInt32(ReadAddr)^;
             mov [edx], eax
             mov [edx + 4], eax // PUInt32(WriteAddr + 4)^ := PUInt32(ReadAddr)^;

             add ecx, 4 // inc(ReadAddr, 4);
             add edx, 8 // inc(WriteAddr, 8);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
  end;

  if SDL_MustLock( Src ) then
    SDL_UnlockSurface( Src );
  if SDL_MustLock( Dest ) then
    SDL_UnlockSurface( Dest );
end;

procedure SDL_50Scanline2xBlit( Src, Dest : PSDL_Surface );
var
  ReadAddr, WriteAddr, ReadRow, WriteRow : UInt32;
  SrcPitch, DestPitch, x, y, w, h : UInt32;
begin
  if ( Src = nil ) or ( Dest = nil ) then
    exit;
  if ( Src.w shl 1 ) < Dest.w then
    exit;
  if ( Src.h shl 1 ) < Dest.h then
    exit;

  if SDL_MustLock( Src ) then
    SDL_LockSurface( Src );
  if SDL_MustLock( Dest ) then
    SDL_LockSurface( Dest );

  ReadRow := UInt32( Src.Pixels );
  WriteRow := UInt32( Dest.Pixels );

  SrcPitch := Src.pitch;
  DestPitch := Dest.pitch;

  w := Src.w;
  h := Src.h;

  case Src.format.BitsPerPixel of
    8 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr
           mov ebx, DestPitch

           @LoopX:
             mov al, [ecx] // PUInt8(WriteAddr)^ := PUInt8(ReadAddr)^;
             mov [edx], al
             mov [edx + 1], al // PUInt8(WriteAddr + 1)^ := PUInt8(ReadAddr)^;
             shr al, 1
             and al, $6d
             mov [edx + ebx], al // PUInt8(WriteAddr + DestPitch)^ := PUInt8(ReadAddr)^;
             mov [edx + ebx + 1], al // PUInt8(WriteAddr + DestPitch + 1)^ := PUInt8(ReadAddr)^;

             inc ecx // inc(ReadAddr);
             add edx, 2 // inc(WriteAddr, 2);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
    15 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr
           mov ebx, DestPitch

           @LoopX:
             mov ax, [ecx] // PUInt16(WriteAddr)^ := PUInt16(ReadAddr)^;
             mov [edx], ax
             mov [edx + 2], ax // PUInt16(WriteAddr + 2)^ := PUInt16(ReadAddr)^;
             shr ax, 1
             and ax, $3def
             mov [edx + ebx], ax // PUInt16(WriteAddr + DestPitch)^ := PUInt16(ReadAddr)^;
             mov [edx + ebx + 2], ax // PUInt16(WriteAddr + DestPitch + 2)^ := PUInt16(ReadAddr)^;

             add ecx, 2 // inc(ReadAddr, 2);
             add edx, 4 // inc(WriteAddr, 4);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
    16 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr
           mov ebx, DestPitch

           @LoopX:
             mov ax, [ecx] // PUInt16(WriteAddr)^ := PUInt16(ReadAddr)^;
             mov [edx], ax
             mov [edx + 2], ax // PUInt16(WriteAddr + 2)^ := PUInt16(ReadAddr)^;
             shr ax, 1
             and ax, $7bef
             mov [edx + ebx], ax // PUInt16(WriteAddr + DestPitch)^ := PUInt16(ReadAddr)^;
             mov [edx + ebx + 2], ax // PUInt16(WriteAddr + DestPitch + 2)^ := PUInt16(ReadAddr)^;

             add ecx, 2 // inc(ReadAddr, 2);
             add edx, 4 // inc(WriteAddr, 4);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
    24 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr
           mov ebx, DestPitch

           @LoopX:
             mov eax, [ecx] // (PUInt32(WriteAddr)^ and $ff000000) or (PUInt32(ReadAddr)^ and $00ffffff);
             and eax, $00ffffff
             and dword ptr [edx], $ff000000
             or [edx], eax
             and dword ptr [edx + 3], $00ffffff // (PUInt32(WriteAddr + 3)^ and $ff000000) or (PUInt32(ReadAddr)^ and $00ffffff);
             or [edx + 3], eax
             shr eax, 1
             and eax, $007f7f7f
             and dword ptr [edx + ebx], $00ffffff // (PUInt32(WriteAddr + DestPitch)^ and $ff000000) or (PUInt32(ReadAddr)^ and $00ffffff);
             or [edx + ebx], eax
             and dword ptr [edx + ebx + 3], $00ffffff // (PUInt32(WriteAddr + DestPitch + 3)^ and $ff000000) or (PUInt32(ReadAddr)^ and $00ffffff);
             or [edx + ebx + 3], eax

             add ecx, 3 // inc(ReadAddr, 3);
             add edx, 6 // inc(WriteAddr, 6);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
    32 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr
           mov ebx, DestPitch

           @LoopX:
             mov eax, [ecx] // PUInt32(WriteAddr)^ := PUInt32(ReadAddr)^;
             mov [edx], eax
             mov [edx + 4], eax // PUInt32(WriteAddr + 4)^ := PUInt32(ReadAddr)^;
             shr eax, 1
             and eax, $7f7f7f7f
             mov [edx + ebx], eax // PUInt32(WriteAddr + DestPitch)^ := PUInt32(ReadAddr)^;
             mov [edx + ebx + 4], eax // PUInt32(WriteAddr + DestPitch + 4)^ := PUInt32(ReadAddr)^;

             add ecx, 4 // inc(ReadAddr, 4);
             add edx, 8 // inc(WriteAddr, 8);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
  end;

  if SDL_MustLock( Src ) then
    SDL_UnlockSurface( Src );
  if SDL_MustLock( Dest ) then
    SDL_UnlockSurface( Dest );
end;

function SDL_PixelTestSurfaceVsRect( SrcSurface1 : PSDL_Surface; SrcRect1 : PSDL_Rect; SrcRect2 : PSDL_Rect; Left1, Top1, Left2, Top2 : integer ) :
boolean;
var
  Src_Rect1, Src_Rect2 : TSDL_Rect;
  right1, bottom1 : integer;
  right2, bottom2 : integer;
  Scan1Start, Scan2Start, ScanWidth, ScanHeight : cardinal;
  Mod1: cardinal;
  Addr1 : cardinal;
  BPP : cardinal;
  Pitch1 : cardinal;
  TransparentColor1 : cardinal;
  tx, ty : cardinal;
  StartTick : cardinal;
  Color1 : cardinal;
begin
  Result := false;
  if SrcRect1 = nil then
  begin
    with Src_Rect1 do
    begin
      x := 0;
      y := 0;
      w := SrcSurface1.w;
      h := SrcSurface1.h;
    end;
  end
  else
    Src_Rect1 := SrcRect1^;

    Src_Rect2 := SrcRect2^;
  with Src_Rect1 do
  begin
    Right1 := Left1 + w;
    Bottom1 := Top1 + h;
  end;
  with Src_Rect2 do
  begin
    Right2 := Left2 + w;
    Bottom2 := Top2 + h;
  end;
  if ( Left1 >= Right2 ) or ( Right1 <= Left2 ) or ( Top1 >= Bottom2 ) or ( 
Bottom1 <=
    Top2 ) then
    exit;
  if Left1 <= Left2 then
  begin
    // 1. left, 2. right
    Scan1Start := Src_Rect1.x + Left2 - Left1;
    Scan2Start := Src_Rect2.x;
    ScanWidth := Right1 - Left2;
    with Src_Rect2 do
      if ScanWidth > w then
        ScanWidth := w;
  end
  else
  begin
    // 1. right, 2. left
    Scan1Start := Src_Rect1.x;
    Scan2Start := Src_Rect2.x + Left1 - Left2;
    ScanWidth := Right2 - Left1;
    with Src_Rect1 do
      if ScanWidth > w then
        ScanWidth := w;
  end;
  with SrcSurface1^ do
  begin
    Pitch1 := Pitch;
    Addr1 := cardinal( Pixels );
    inc( Addr1, Pitch1 * UInt32( Src_Rect1.y ) );
    with format^ do
    begin
      BPP := BytesPerPixel;
      TransparentColor1 := colorkey;
    end;
  end;

  Mod1 := Pitch1 - ( ScanWidth * BPP );

  inc( Addr1, BPP * Scan1Start );

  if Top1 <= Top2 then
  begin
    // 1. up, 2. down
    ScanHeight := Bottom1 - Top2;
    if ScanHeight > Src_Rect2.h then
      ScanHeight := Src_Rect2.h;
    inc( Addr1, Pitch1 * UInt32( Top2 - Top1 ) );
  end
  else
  begin
    // 1. down, 2. up
    ScanHeight := Bottom2 - Top1;
    if ScanHeight > Src_Rect1.h then
      ScanHeight := Src_Rect1.h;

  end;
  case BPP of
    1 :
      for ty := 1 to ScanHeight do
      begin
        for tx := 1 to ScanWidth do
        begin
          if ( PByte( Addr1 )^ <> TransparentColor1 ) then
          begin
            Result := true;
            exit;
          end;
          inc( Addr1 );

        end;
        inc( Addr1, Mod1 );

      end;
    2 :
      for ty := 1 to ScanHeight do
      begin
        for tx := 1 to ScanWidth do
        begin
          if ( PWord( Addr1 )^ <> TransparentColor1 )  then
          begin
            Result := true;
            exit;
          end;
          inc( Addr1, 2 );

        end;
        inc( Addr1, Mod1 );

      end;
    3 :
      for ty := 1 to ScanHeight do
      begin
        for tx := 1 to ScanWidth do
        begin
          Color1 := PLongWord( Addr1 )^ and $00FFFFFF;

          if ( Color1 <> TransparentColor1 )
            then
          begin
            Result := true;
            exit;
          end;
          inc( Addr1, 3 );

        end;
        inc( Addr1, Mod1 );

      end;
    4 :
      for ty := 1 to ScanHeight do
      begin
        for tx := 1 to ScanWidth do
        begin
          if ( PLongWord( Addr1 )^ <> TransparentColor1 )  then
          begin
            Result := true;
            exit;
          end;
          inc( Addr1, 4 );

        end;
        inc( Addr1, Mod1 );

      end;
  end;
end;

procedure SDL_ORSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DestSurface : PSDL_Surface; DestRect : PSDL_Rect );
var
  Src, Dest : TSDL_Rect;
  Diff : integer;
  SrcAddr, DestAddr, TransparentColor : cardinal;
  // TransparentColor: cardinal;
  _ebx, _esi, _edi, _esp : cardinal;
  WorkX, WorkY : word;
  SrcMod, DestMod : cardinal;
  Bits : cardinal;
begin
  if ( SrcSurface = nil ) or ( DestSurface = nil ) then
    exit; // Remove this to make it faster
  if ( SrcSurface.Format.BitsPerPixel <> DestSurface.Format.BitsPerPixel ) then
    exit; // Remove this to make it faster
  if SrcRect = nil then
  begin
    with Src do
    begin
      x := 0;
      y := 0;
      w := SrcSurface.w;
      h := SrcSurface.h;
    end;
  end
  else
    Src := SrcRect^;
  if DestRect = nil then
  begin
    Dest.x := 0;
    Dest.y := 0;
  end
  else
    Dest := DestRect^;
  Dest.w := Src.w;
  Dest.h := Src.h;
  with DestSurface.Clip_Rect do
  begin
    // Source's right side is greater than the dest.cliprect
    if Dest.x + Src.w > x + w then
    begin
      smallint( Src.w ) := x + w - Dest.x;
      smallint( Dest.w ) := x + w - Dest.x;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's bottom side is greater than the dest.clip
    if Dest.y + Src.h > y + h then
    begin
      smallint( Src.h ) := y + h - Dest.y;
      smallint( Dest.h ) := y + h - Dest.y;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
    // Source's left side is less than the dest.clip
    if Dest.x < x then
    begin
      Diff := x - Dest.x;
      Src.x := Src.x + Diff;
      smallint( Src.w ) := smallint( Src.w ) - Diff;
      Dest.x := x;
      smallint( Dest.w ) := smallint( Dest.w ) - Diff;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's Top side is less than the dest.clip
    if Dest.y < y then
    begin
      Diff := y - Dest.y;
      Src.y := Src.y + Diff;
      smallint( Src.h ) := smallint( Src.h ) - Diff;
      Dest.y := y;
      smallint( Dest.h ) := smallint( Dest.h ) - Diff;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
  end;
  with SrcSurface^ do
  begin
    SrcAddr := cardinal( Pixels ) + UInt32( Src.y ) * Pitch + UInt32( Src.x ) *
      Format.BytesPerPixel;
    SrcMod := Pitch - Src.w * Format.BytesPerPixel;
    TransparentColor := Format.colorkey;
  end;
  with DestSurface^ do
  begin
    DestAddr := cardinal( Pixels ) + UInt32( Dest.y ) * Pitch + UInt32( Dest.x ) *
      Format.BytesPerPixel;
    DestMod := Pitch - Dest.w * Format.BytesPerPixel;
    Bits := Format.BitsPerPixel;
  end;
  SDL_LockSurface( SrcSurface );
  SDL_LockSurface( DestSurface );
  WorkY := Src.h;
  case bits of
    8 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov al, [esi]         // AL := source color
        cmp al, 0
        je @SkipColor         // if AL=0 or AL=transparent color then skip everything
        cmp al, byte ptr TransparentColor
        je @SkipColor
        or al, [edi]
        mov [edi], al
       @SkipColor:
        inc esi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    15 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax          // WorkX := Src.w
       @Loopx:
        mov ax, [esi]         // AX := source color
        cmp ax, 0
        je @SkipColor         // if AX=0 then skip everything
        cmp ax, word ptr TransparentColor
        je @SkipColor
        or ax, [edi]
        mov [edi], ax
       @SkipColor:
        add esi, 2
        add edi, 2
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    16 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov ax, [esi]         // AX := source color
        cmp ax, 0
        je @SkipColor         // if AX=0 then skip everything
        cmp ax, word ptr TransparentColor
        je @SkipColor
        or ax, [edi]
        mov [edi], ax
       @SkipColor:
        add esi, 2
        add edi, 2
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    24 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       add WorkX, ax         // WorkX := Src.w * 2
       add WorkX, ax         // WorkX := Src.w * 3
       @Loopx:
        mov al, [esi]         // AL := source color
        or al, [edi]
        mov [edi], al
        inc esi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    32 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       shl ax, 2
       mov WorkX, ax         // WorkX := Src.w * 4
       @Loopx:
        mov al, [esi]         // AL := source color
        or al, [edi]
        mov [edi], al
        inc esi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
  end;
  SDL_UnlockSurface( SrcSurface );
  SDL_UnlockSurface( DestSurface );
end;

procedure SDL_ANDSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DestSurface : PSDL_Surface; DestRect : PSDL_Rect );
var
  Src, Dest : TSDL_Rect;
  Diff : integer;
  SrcAddr, DestAddr, TransparentColor : cardinal;
  // TransparentColor: cardinal;
  _ebx, _esi, _edi, _esp : cardinal;
  WorkX, WorkY : word;
  SrcMod, DestMod : cardinal;
  Bits : cardinal;
begin
  if ( SrcSurface = nil ) or ( DestSurface = nil ) then
    exit; // Remove this to make it faster
  if ( SrcSurface.Format.BitsPerPixel <> DestSurface.Format.BitsPerPixel ) then
    exit; // Remove this to make it faster
  if SrcRect = nil then
  begin
    with Src do
    begin
      x := 0;
      y := 0;
      w := SrcSurface.w;
      h := SrcSurface.h;
    end;
  end
  else
    Src := SrcRect^;
  if DestRect = nil then
  begin
    Dest.x := 0;
    Dest.y := 0;
  end
  else
    Dest := DestRect^;
  Dest.w := Src.w;
  Dest.h := Src.h;
  with DestSurface.Clip_Rect do
  begin
    // Source's right side is greater than the dest.cliprect
    if Dest.x + Src.w > x + w then
    begin
      smallint( Src.w ) := x + w - Dest.x;
      smallint( Dest.w ) := x + w - Dest.x;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's bottom side is greater than the dest.clip
    if Dest.y + Src.h > y + h then
    begin
      smallint( Src.h ) := y + h - Dest.y;
      smallint( Dest.h ) := y + h - Dest.y;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
    // Source's left side is less than the dest.clip
    if Dest.x < x then
    begin
      Diff := x - Dest.x;
      Src.x := Src.x + Diff;
      smallint( Src.w ) := smallint( Src.w ) - Diff;
      Dest.x := x;
      smallint( Dest.w ) := smallint( Dest.w ) - Diff;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's Top side is less than the dest.clip
    if Dest.y < y then
    begin
      Diff := y - Dest.y;
      Src.y := Src.y + Diff;
      smallint( Src.h ) := smallint( Src.h ) - Diff;
      Dest.y := y;
      smallint( Dest.h ) := smallint( Dest.h ) - Diff;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
  end;
  with SrcSurface^ do
  begin
    SrcAddr := cardinal( Pixels ) + UInt32( Src.y ) * Pitch + UInt32( Src.x ) *
      Format.BytesPerPixel;
    SrcMod := Pitch - Src.w * Format.BytesPerPixel;
    TransparentColor := Format.colorkey;
  end;
  with DestSurface^ do
  begin
    DestAddr := cardinal( Pixels ) + UInt32( Dest.y ) * Pitch + UInt32( Dest.x ) *
      Format.BytesPerPixel;
    DestMod := Pitch - Dest.w * Format.BytesPerPixel;
    Bits := Format.BitsPerPixel;
  end;
  SDL_LockSurface( SrcSurface );
  SDL_LockSurface( DestSurface );
  WorkY := Src.h;
  case bits of
    8 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov al, [esi]         // AL := source color
        cmp al, 0
        je @SkipColor         // if AL=0 or AL=transparent color then skip everything
        cmp al, byte ptr TransparentColor
        je @SkipColor
        and al, [edi]
        mov [edi], al
       @SkipColor:
        inc esi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    15 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax          // WorkX := Src.w
       @Loopx:
        mov ax, [esi]         // AX := source color
        cmp ax, 0
        je @SkipColor         // if AX=0 then skip everything
        cmp ax, word ptr TransparentColor
        je @SkipColor
        and ax, [edi]
        mov [edi], ax
       @SkipColor:
        add esi, 2
        add edi, 2
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    16 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov ax, [esi]         // AX := source color
        cmp ax, 0
        je @SkipColor         // if AX=0 then skip everything
        cmp ax, word ptr TransparentColor
        je @SkipColor
        and ax, [edi]
        mov [edi], ax
       @SkipColor:
        add esi, 2
        add edi, 2
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    24 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       add WorkX, ax         // WorkX := Src.w * 2
       add WorkX, ax         // WorkX := Src.w * 3
       @Loopx:
        mov al, [esi]         // AL := source color
        and al, [edi]
        mov [edi], al
        inc esi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    32 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       shl ax, 2
       mov WorkX, ax         // WorkX := Src.w * 4
       @Loopx:
        mov al, [esi]         // AL := source color
        and al, [edi]
        mov [edi], al
        inc esi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
  end;
  SDL_UnlockSurface( SrcSurface );
  SDL_UnlockSurface( DestSurface );
end;


procedure SDL_GTSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DestSurface : PSDL_Surface; DestRect : PSDL_Rect );
var
  R, G, B, Pixel1, Pixel2, TransparentColor : cardinal;
  Src, Dest : TSDL_Rect;
  Diff : integer;
  SrcAddr, DestAddr : cardinal;
  WorkX, WorkY : word;
  SrcMod, DestMod : cardinal;
  Bits : cardinal;
begin
  if ( SrcSurface = nil ) or ( DestSurface = nil ) then
    exit; // Remove this to make it faster
  if ( SrcSurface.Format.BitsPerPixel <> DestSurface.Format.BitsPerPixel ) then
    exit; // Remove this to make it faster
  if SrcRect = nil then
  begin
    with Src do
    begin
      x := 0;
      y := 0;
      w := SrcSurface.w;
      h := SrcSurface.h;
    end;
  end
  else
    Src := SrcRect^;
  if DestRect = nil then
  begin
    Dest.x := 0;
    Dest.y := 0;
  end
  else
    Dest := DestRect^;
  Dest.w := Src.w;
  Dest.h := Src.h;
  with DestSurface.Clip_Rect do
  begin
    // Source's right side is greater than the dest.cliprect
    if Dest.x + Src.w > x + w then
    begin
      smallint( Src.w ) := x + w - Dest.x;
      smallint( Dest.w ) := x + w - Dest.x;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's bottom side is greater than the dest.clip
    if Dest.y + Src.h > y + h then
    begin
      smallint( Src.h ) := y + h - Dest.y;
      smallint( Dest.h ) := y + h - Dest.y;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
    // Source's left side is less than the dest.clip
    if Dest.x < x then
    begin
      Diff := x - Dest.x;
      Src.x := Src.x + Diff;
      smallint( Src.w ) := smallint( Src.w ) - Diff;
      Dest.x := x;
      smallint( Dest.w ) := smallint( Dest.w ) - Diff;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's Top side is less than the dest.clip
    if Dest.y < y then
    begin
      Diff := y - Dest.y;
      Src.y := Src.y + Diff;
      smallint( Src.h ) := smallint( Src.h ) - Diff;
      Dest.y := y;
      smallint( Dest.h ) := smallint( Dest.h ) - Diff;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
  end;
  with SrcSurface^ do
  begin
    SrcAddr := cardinal( Pixels ) + UInt32( Src.y ) * Pitch + UInt32( Src.x ) *
      Format.BytesPerPixel;
    SrcMod := Pitch - Src.w * Format.BytesPerPixel;
    TransparentColor := Format.colorkey;
  end;
  with DestSurface^ do
  begin
    DestAddr := cardinal( Pixels ) + UInt32( Dest.y ) * Pitch + UInt32( Dest.x ) *
      Format.BytesPerPixel;
    DestMod := Pitch - Dest.w * Format.BytesPerPixel;
    Bits := Format.BitsPerPixel;
  end;
  SDL_LockSurface( SrcSurface );
  SDL_LockSurface( DestSurface );
  WorkY := Src.h;
  case bits of
    8 :
      begin
        repeat
          WorkX := Src.w;
          repeat
            Pixel1 := PUInt8( SrcAddr )^;
            if ( Pixel1 <> TransparentColor ) and ( Pixel1 <> 0 ) then
            begin
              Pixel2 := PUInt8( DestAddr )^;
              if Pixel2 > 0 then
              begin
                if Pixel2 and $E0 > Pixel1 and $E0 then R := Pixel2 and $E0 else R := Pixel1 and $E0;
                if Pixel2 and $1C > Pixel1 and $1C then G := Pixel2 and $1C else G := Pixel1 and $1C;
                if Pixel2 and $03 > Pixel1 and $03 then B := Pixel2 and $03 else B := Pixel1 and $03;

                if R > $E0 then
                  R := $E0;
                if G > $1C then
                  G := $1C;
                if B > $03 then
                  B := $03;
                PUInt8( DestAddr )^ := R or G or B;
              end
              else
                PUInt8( DestAddr )^ := Pixel1;
            end;
            inc( SrcAddr );
            inc( DestAddr );
            dec( WorkX );
          until WorkX = 0;
          inc( SrcAddr, SrcMod );
          inc( DestAddr, DestMod );
          dec( WorkY );
        until WorkY = 0;
      end;
    15 :
      begin
        repeat
          WorkX := Src.w;
          repeat
            Pixel1 := PUInt16( SrcAddr )^;
            if ( Pixel1 <> TransparentColor ) and ( Pixel1 <> 0 ) then
            begin
              Pixel2 := PUInt16( DestAddr )^;
              if Pixel2 > 0 then
              begin

              if Pixel2 and $7C00 > Pixel1 and $7C00 then R := Pixel2 and $7C00 else R := Pixel1 and $7C00;
              if Pixel2 and $03E0 > Pixel1 and $03E0 then G := Pixel2 and $03E0 else G := Pixel1 and $03E0;
              if Pixel2 and $001F > Pixel1 and $001F then B := Pixel2 and $001F else B := Pixel1 and $001F;

                PUInt16( DestAddr )^ := R or G or B;
              end
              else
                PUInt16( DestAddr )^ := Pixel1;
            end;
            inc( SrcAddr, 2 );
            inc( DestAddr, 2 );
            dec( WorkX );
          until WorkX = 0;
          inc( SrcAddr, SrcMod );
          inc( DestAddr, DestMod );
          dec( WorkY );
        until WorkY = 0;
      end;
    16 :
      begin
        repeat
          WorkX := Src.w;
          repeat
            Pixel1 := PUInt16( SrcAddr )^;
            if ( Pixel1 <> TransparentColor ) and ( Pixel1 <> 0 ) then
            begin
              Pixel2 := PUInt16( DestAddr )^;
              if Pixel2 > 0 then
              begin

              if Pixel2 and $F800 > Pixel1 and $F800 then R := Pixel2 and $F800 else R := Pixel1 and $F800;
              if Pixel2 and $07E0 > Pixel1 and $07E0 then G := Pixel2 and $07E0 else G := Pixel1 and $07E0;
              if Pixel2 and $001F > Pixel1 and $001F then B := Pixel2 and $001F else B := Pixel1 and $001F;

                PUInt16( DestAddr )^ := R or G or B;
              end
              else
                PUInt16( DestAddr )^ := Pixel1;
            end;
            inc( SrcAddr, 2 );
            inc( DestAddr, 2 );
            dec( WorkX );
          until WorkX = 0;
          inc( SrcAddr, SrcMod );
          inc( DestAddr, DestMod );
          dec( WorkY );
        until WorkY = 0;
      end;
    24 :
      begin
        repeat
          WorkX := Src.w;
          repeat
            Pixel1 := PUInt32( SrcAddr )^ and $00FFFFFF;
            if ( Pixel1 <> TransparentColor ) and ( Pixel1 <> 0 ) then
            begin
              Pixel2 := PUInt32( DestAddr )^ and $00FFFFFF;
              if Pixel2 > 0 then
              begin

              if Pixel2 and $FF0000 > Pixel1 and $FF0000 then R := Pixel2 and $FF0000 else R := Pixel1 and $FF0000;
              if Pixel2 and $00FF00 > Pixel1 and $00FF00 then G := Pixel2 and $00FF00 else G := Pixel1 and $00FF00;
              if Pixel2 and $0000FF > Pixel1 and $0000FF then B := Pixel2 and $0000FF else B := Pixel1 and $0000FF;

                PUInt32( DestAddr )^ := PUInt32( DestAddr )^ and $FF000000 or ( R or G or B );
              end
              else
                PUInt32( DestAddr )^ := PUInt32( DestAddr )^ and $FF000000 or Pixel1;
            end;
            inc( SrcAddr, 3 );
            inc( DestAddr, 3 );
            dec( WorkX );
          until WorkX = 0;
          inc( SrcAddr, SrcMod );
          inc( DestAddr, DestMod );
          dec( WorkY );
        until WorkY = 0;
      end;
    32 :
      begin
        repeat
          WorkX := Src.w;
          repeat
            Pixel1 := PUInt32( SrcAddr )^;
            if ( Pixel1 <> TransparentColor ) and ( Pixel1 <> 0 ) then
            begin
              Pixel2 := PUInt32( DestAddr )^;
              if Pixel2 > 0 then
              begin

              if Pixel2 and $FF0000 > Pixel1 and $FF0000 then R := Pixel2 and $FF0000 else R := Pixel1 and $FF0000;
              if Pixel2 and $00FF00 > Pixel1 and $00FF00 then G := Pixel2 and $00FF00 else G := Pixel1 and $00FF00;
              if Pixel2 and $0000FF > Pixel1 and $0000FF then B := Pixel2 and $0000FF else B := Pixel1 and $0000FF;

                PUInt32( DestAddr )^ := R or G or B;
              end
              else
                PUInt32( DestAddr )^ := Pixel1;
            end;
            inc( SrcAddr, 4 );
            inc( DestAddr, 4 );
            dec( WorkX );
          until WorkX = 0;
          inc( SrcAddr, SrcMod );
          inc( DestAddr, DestMod );
          dec( WorkY );
        until WorkY = 0;
      end;
  end;
  SDL_UnlockSurface( SrcSurface );
  SDL_UnlockSurface( DestSurface );
end;


procedure SDL_LTSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DestSurface : PSDL_Surface; DestRect : PSDL_Rect );
var
  R, G, B, Pixel1, Pixel2, TransparentColor : cardinal;
  Src, Dest : TSDL_Rect;
  Diff : integer;
  SrcAddr, DestAddr : cardinal;
  WorkX, WorkY : word;
  SrcMod, DestMod : cardinal;
  Bits : cardinal;
begin
  if ( SrcSurface = nil ) or ( DestSurface = nil ) then
    exit; // Remove this to make it faster
  if ( SrcSurface.Format.BitsPerPixel <> DestSurface.Format.BitsPerPixel ) then
    exit; // Remove this to make it faster
  if SrcRect = nil then
  begin
    with Src do
    begin
      x := 0;
      y := 0;
      w := SrcSurface.w;
      h := SrcSurface.h;
    end;
  end
  else
    Src := SrcRect^;
  if DestRect = nil then
  begin
    Dest.x := 0;
    Dest.y := 0;
  end
  else
    Dest := DestRect^;
  Dest.w := Src.w;
  Dest.h := Src.h;
  with DestSurface.Clip_Rect do
  begin
    // Source's right side is greater than the dest.cliprect
    if Dest.x + Src.w > x + w then
    begin
      smallint( Src.w ) := x + w - Dest.x;
      smallint( Dest.w ) := x + w - Dest.x;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's bottom side is greater than the dest.clip
    if Dest.y + Src.h > y + h then
    begin
      smallint( Src.h ) := y + h - Dest.y;
      smallint( Dest.h ) := y + h - Dest.y;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
    // Source's left side is less than the dest.clip
    if Dest.x < x then
    begin
      Diff := x - Dest.x;
      Src.x := Src.x + Diff;
      smallint( Src.w ) := smallint( Src.w ) - Diff;
      Dest.x := x;
      smallint( Dest.w ) := smallint( Dest.w ) - Diff;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's Top side is less than the dest.clip
    if Dest.y < y then
    begin
      Diff := y - Dest.y;
      Src.y := Src.y + Diff;
      smallint( Src.h ) := smallint( Src.h ) - Diff;
      Dest.y := y;
      smallint( Dest.h ) := smallint( Dest.h ) - Diff;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
  end;
  with SrcSurface^ do
  begin
    SrcAddr := cardinal( Pixels ) + UInt32( Src.y ) * Pitch + UInt32( Src.x ) *
      Format.BytesPerPixel;
    SrcMod := Pitch - Src.w * Format.BytesPerPixel;
    TransparentColor := Format.colorkey;
  end;
  with DestSurface^ do
  begin
    DestAddr := cardinal( Pixels ) + UInt32( Dest.y ) * Pitch + UInt32( Dest.x ) *
      Format.BytesPerPixel;
    DestMod := Pitch - Dest.w * Format.BytesPerPixel;
    Bits := Format.BitsPerPixel;
  end;
  SDL_LockSurface( SrcSurface );
  SDL_LockSurface( DestSurface );
  WorkY := Src.h;
  case bits of
    8 :
      begin
        repeat
          WorkX := Src.w;
          repeat
            Pixel1 := PUInt8( SrcAddr )^;
            if ( Pixel1 <> TransparentColor ) and ( Pixel1 <> 0 ) then
            begin
              Pixel2 := PUInt8( DestAddr )^;
              if Pixel2 > 0 then
              begin
                if Pixel2 and $E0 < Pixel1 and $E0 then R := Pixel2 and $E0 else R := Pixel1 and $E0;
                if Pixel2 and $1C < Pixel1 and $1C then G := Pixel2 and $1C else G := Pixel1 and $1C;
                if Pixel2 and $03 < Pixel1 and $03 then B := Pixel2 and $03 else B := Pixel1 and $03;

                if R > $E0 then
                  R := $E0;
                if G > $1C then
                  G := $1C;
                if B > $03 then
                  B := $03;
                PUInt8( DestAddr )^ := R or G or B;
              end
              else
                PUInt8( DestAddr )^ := Pixel1;
            end;
            inc( SrcAddr );
            inc( DestAddr );
            dec( WorkX );
          until WorkX = 0;
          inc( SrcAddr, SrcMod );
          inc( DestAddr, DestMod );
          dec( WorkY );
        until WorkY = 0;
      end;
    15 :
      begin
        repeat
          WorkX := Src.w;
          repeat
            Pixel1 := PUInt16( SrcAddr )^;
            if ( Pixel1 <> TransparentColor ) and ( Pixel1 <> 0 ) then
            begin
              Pixel2 := PUInt16( DestAddr )^;
              if Pixel2 > 0 then
              begin

              if Pixel2 and $7C00 < Pixel1 and $7C00 then R := Pixel2 and $7C00 else R := Pixel1 and $7C00;
              if Pixel2 and $03E0 < Pixel1 and $03E0 then G := Pixel2 and $03E0 else G := Pixel1 and $03E0;
              if Pixel2 and $001F < Pixel1 and $001F then B := Pixel2 and $001F else B := Pixel1 and $001F;

                PUInt16( DestAddr )^ := R or G or B;
              end
              else
                PUInt16( DestAddr )^ := Pixel1;
            end;
            inc( SrcAddr, 2 );
            inc( DestAddr, 2 );
            dec( WorkX );
          until WorkX = 0;
          inc( SrcAddr, SrcMod );
          inc( DestAddr, DestMod );
          dec( WorkY );
        until WorkY = 0;
      end;
    16 :
      begin
        repeat
          WorkX := Src.w;
          repeat
            Pixel1 := PUInt16( SrcAddr )^;
            if ( Pixel1 <> TransparentColor ) and ( Pixel1 <> 0 ) then
            begin
              Pixel2 := PUInt16( DestAddr )^;
              if Pixel2 > 0 then
              begin

              if Pixel2 and $F800 < Pixel1 and $F800 then R := Pixel2 and $F800 else R := Pixel1 and $F800;
              if Pixel2 and $07E0 < Pixel1 and $07E0 then G := Pixel2 and $07E0 else G := Pixel1 and $07E0;
              if Pixel2 and $001F < Pixel1 and $001F then B := Pixel2 and $001F else B := Pixel1 and $001F;

                PUInt16( DestAddr )^ := R or G or B;
              end
              else
                PUInt16( DestAddr )^ := Pixel1;
            end;
            inc( SrcAddr, 2 );
            inc( DestAddr, 2 );
            dec( WorkX );
          until WorkX = 0;
          inc( SrcAddr, SrcMod );
          inc( DestAddr, DestMod );
          dec( WorkY );
        until WorkY = 0;
      end;
    24 :
      begin
        repeat
          WorkX := Src.w;
          repeat
            Pixel1 := PUInt32( SrcAddr )^ and $00FFFFFF;
            if ( Pixel1 <> TransparentColor ) and ( Pixel1 <> 0 ) then
            begin
              Pixel2 := PUInt32( DestAddr )^ and $00FFFFFF;
              if Pixel2 > 0 then
              begin

              if Pixel2 and $FF0000 < Pixel1 and $FF0000 then R := Pixel2 and $FF0000 else R := Pixel1 and $FF0000;
              if Pixel2 and $00FF00 < Pixel1 and $00FF00 then G := Pixel2 and $00FF00 else G := Pixel1 and $00FF00;
              if Pixel2 and $0000FF < Pixel1 and $0000FF then B := Pixel2 and $0000FF else B := Pixel1 and $0000FF;

                PUInt32( DestAddr )^ := PUInt32( DestAddr )^ and $FF000000 or ( R or G or B );
              end
              else
                PUInt32( DestAddr )^ := PUInt32( DestAddr )^ and $FF000000 or Pixel1;
            end;
            inc( SrcAddr, 3 );
            inc( DestAddr, 3 );
            dec( WorkX );
          until WorkX = 0;
          inc( SrcAddr, SrcMod );
          inc( DestAddr, DestMod );
          dec( WorkY );
        until WorkY = 0;
      end;
    32 :
      begin
        repeat
          WorkX := Src.w;
          repeat
            Pixel1 := PUInt32( SrcAddr )^;
            if ( Pixel1 <> TransparentColor ) and ( Pixel1 <> 0 ) then
            begin
              Pixel2 := PUInt32( DestAddr )^;
              if Pixel2 > 0 then
              begin

              if Pixel2 and $FF0000 < Pixel1 and $FF0000 then R := Pixel2 and $FF0000 else R := Pixel1 and $FF0000;
              if Pixel2 and $00FF00 < Pixel1 and $00FF00 then G := Pixel2 and $00FF00 else G := Pixel1 and $00FF00;
              if Pixel2 and $0000FF < Pixel1 and $0000FF then B := Pixel2 and $0000FF else B := Pixel1 and $0000FF;

                PUInt32( DestAddr )^ := R or G or B;
              end
              else
                PUInt32( DestAddr )^ := Pixel1;
            end;
            inc( SrcAddr, 4 );
            inc( DestAddr, 4 );
            dec( WorkX );
          until WorkX = 0;
          inc( SrcAddr, SrcMod );
          inc( DestAddr, DestMod );
          dec( WorkY );
        until WorkY = 0;
      end;
  end;
  SDL_UnlockSurface( SrcSurface );
  SDL_UnlockSurface( DestSurface );
end;

function SDL_ClipLine(var x1,y1,x2,y2: Integer; ClipRect: PSDL_Rect) : boolean;
var tflag, flag1, flag2: word;
    txy, xedge, yedge: Integer;
    slope: single;

  function ClipCode(x,y: Integer): word;
  begin
    Result := 0;
    if x < ClipRect.x then Result := 1;
    if x >= ClipRect.w + ClipRect.x then Result := Result or 2;
    if y < ClipRect.y then Result := Result or 4;
    if y >= ClipRect.h + ClipRect.y then Result := Result or 8;
  end;

begin
  flag1 := ClipCode(x1,y1);
  flag2 := ClipCode(x2,y2);
  result := true;

  while true do
  begin
    if (flag1 or flag2) = 0 then Exit; // all in

    if (flag1 and flag2) <> 0 then
      begin
        result := false;
        Exit; // all out
      end;

    if flag2 = 0 then
      begin
        txy := x1; x1 := x2; x2 := txy;
        txy := y1; y1 := y2; y2 := txy;
        tflag := flag1; flag1 := flag2; flag2 := tflag;
      end;

    if (flag2 and 3) <> 0 then
      begin
        if (flag2 and 1) <> 0 then
          xedge := ClipRect.x
        else
          xedge := ClipRect.w + ClipRect.x -1; // back 1 pixel otherwise we end up in a loop

        slope := (y2 - y1) / (x2 - x1);
        y2 := y1 + Round(slope * (xedge - x1));
        x2 := xedge;
      end
    else
      begin
        if (flag2 and 4) <> 0 then
          yedge := ClipRect.y
        else
          yedge := ClipRect.h + ClipRect.y -1; // up 1 pixel otherwise we end up in a loop

        slope := (x2 - x1) / (y2 - y1);
        x2 := x1 + Round(slope * (yedge - y1));
        y2 := yedge;
      end;

    flag2 := ClipCode(x2, y2);
  end;
end;

end.


