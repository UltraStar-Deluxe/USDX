unit sdltruetypefont;
{
  $Id: sdltruetypefont.pas,v 1.1 2004/09/30 22:39:50 savage Exp $

}
{******************************************************************************}
{                                                                              }
{          JEDI-SDL : Pascal units for SDL - Simple DirectMedia Layer          }
{                Wrapper class for SDL_ttf                                     }
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
{                                                                              }
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
{   September   23 2004 - DL : Initial Creation                                }
{
  $Log: sdltruetypefont.pas,v $
  Revision 1.1  2004/09/30 22:39:50  savage
  Added a true type font class which contains a wrap text function.
  Changed the sdl_ttf.pas header to reflect the future of jedi-sdl.


}
{******************************************************************************}

interface

uses
  sdl,
  sdl_ttf;

type
  TRenderType = ( rtLatin1, rtUTF8, rtUnicode );
  TSDLFontStyle = ( fsBold, fsItalic, fsUnderline, fsStrikeOut );

  TSDLFontStyles = set of TSDLFontStyle;

  TTrueTypeFont = class( TObject )
  private
    FFont : PTTF_Font;
    FSolid : Boolean;
    FBackGroundColour : TSDL_Color;
    FForeGroundColour : TSDL_Color;
    FRenderType : TRenderType;
    FStyle : TSDLFontStyles;
    FFontFile : string;
    FFontSize : integer;
    procedure PrepareFont;
  protected

  public
    constructor Create( aFontFile : string; aRenderStyle : TSDLFontStyles = [ ]; aFontSize : integer = 14 );
    destructor Destroy; override;
    function DrawText( aText : WideString ) : PSDL_Surface; overload;
    function DrawText( aText : WideString; aWidth, aHeight : Integer ) : PSDL_Surface; overload;
    property BackGroundColour : TSDL_Color read FBackGroundColour write FBackGroundColour;
    property ForeGroundColour : TSDL_Color read FForeGroundColour write FForeGroundColour;
    property FontFile : string read FFontFile write FFontFile;
    property RenderType : TRenderType read FRenderType write FRenderType;
    property Solid : Boolean read FSolid write FSolid;
    property Style : TSDLFontStyles read FStyle write FStyle;
    property FontSize : integer read FFontSize write FFontSize;
  end;


implementation

uses
  SysUtils;

{ TTrueTypeFont }

constructor TTrueTypeFont.Create( aFontFile : string; aRenderStyle : TSDLFontStyles; aFontSize : integer );
begin
  inherited Create;
  if FileExists( aFontFile ) then
  begin
    FStyle := aRenderStyle;
    FFontSize := aFontSize;
    FSolid := false;
    FBackGroundColour.r := 255;
    FBackGroundColour.g := 255;
    FBackGroundColour.b := 255;
    FForeGroundColour.r := 0;
    FForeGroundColour.g := 0;
    FForeGroundColour.b := 0;
    FRenderType := rtUTF8;
    if ( TTF_Init >= 0 ) then
    begin
      FFontFile := aFontFile;
    end
    else
      raise Exception.Create( 'Failed to Initialiase SDL_TTF' );
  end
  else
    raise Exception.Create( 'Font File does not exist' );
end;

destructor TTrueTypeFont.Destroy;
begin
  if FFont <> nil then
    TTF_CloseFont( FFont );
  TTF_Quit;
  inherited;
end;

function TTrueTypeFont.DrawText( aText : WideString ) : PSDL_Surface;
begin
  PrepareFont;

  result := nil;

  case FRenderType of
    rtLatin1 :
      begin
        if ( FSolid ) then
        begin
          result := TTF_RenderText_Solid( FFont, PChar( string( aText ) ), FForeGroundColour );
        end
        else
        begin
          result := TTF_RenderText_Shaded( FFont, PChar( string( aText ) ), FForeGroundColour, FBackGroundColour );
        end;
      end;

    rtUTF8 :
      begin
        if ( FSolid ) then
        begin
          result := TTF_RenderUTF8_Solid( FFont, PChar( string( aText ) ), FForeGroundColour );
        end
        else
        begin
          result := TTF_RenderUTF8_Shaded( FFont, PChar( string( aText ) ), FForeGroundColour, FBackGroundColour );
        end;
      end;

    rtUnicode :
      begin
        if ( FSolid ) then
        begin
          result := TTF_RenderUNICODE_Solid( FFont, PUInt16( aText ), FForeGroundColour );
        end
        else
        begin
          result := TTF_RenderUNICODE_Shaded( FFont, PUInt16( aText ), FForeGroundColour, FBackGroundColour );
        end;
      end;
  end;
end;

function TTrueTypeFont.DrawText( aText : WideString; aWidth, aHeight : Integer ) : PSDL_Surface;
var
  textw, texth, i, yPos : integer;
  strChopped : WideString;
  SurfaceList : array of PSDL_Surface;
  strlist : array of WideString;
  ReturnedSurface : PSDL_Surface;
  BltRect : TSDL_Rect;
begin
  PrepareFont;

  // Do an initial check to see if it already fits
  case FRenderType of
    rtLatin1 :
    begin
      if TTF_SizeText( FFont, PChar( string( aText ) ), textw, texth ) = 0 then
      begin
        if ( textw < aWidth )
          and ( texth < aHeight ) then
        begin
          result := DrawText( aText );
          exit;
        end
      end;
    end;

    rtUTF8 :
    begin
      if TTF_SizeUTF8( FFont, PChar( string( aText ) ), textw, texth ) = 0 then
      begin
        if ( textw < aWidth )
          and ( texth < aHeight ) then
        begin
          result := DrawText( aText );
          exit;
        end
      end;
    end;

    rtUnicode :
    begin
      if TTF_SizeUNICODE( FFont, PUInt16( aText ), textw, texth ) = 0 then
      begin
        if ( textw < aWidth )
          and ( texth < aHeight ) then
        begin
          result := DrawText( aText );
          exit;
        end
      end;
    end;
  end;

  // Create the Surface we will be returning
  ReturnedSurface := SDL_DisplayFormat( SDL_CreateRGBSurface( SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL, aWidth, aHeight, 16, 0, 0, 0, 0 ) );

  // If we are still here there is some serious parsing to do
  case FRenderType of
    rtLatin1 :
    begin
      strChopped := aText;
      i := Length( strChopped );
      while ( i <> 0 ) do
      begin
        if ( string( strChopped[ i ] ) <> ' ' ) and ( Integer( string( strChopped[ i ] ) ) <> 13 ) then
          dec( i )
        else
        begin
          dec( i );
          strChopped := Copy( strChopped, 0, i );
          if TTF_SizeText( FFont, PChar( string( strChopped ) ), textw, texth ) = 0 then
          begin
            if ( textw < aWidth )
              and ( texth < aHeight ) then
            begin
              SetLength( strlist, Length( strlist ) + 1 );
              strlist[ Length( strlist ) - 1 ] := strChopped;
              strChopped := Copy( aText, i + 2, Length( aText ) - ( i - 1 ) );
              i := Length( strChopped );
              if TTF_SizeText( FFont, PChar( string( strChopped ) ), textw, texth ) = 0 then
              begin
                SetLength( strlist, Length( strlist ) + 1 );
                strlist[ Length( strlist ) - 1 ] := strChopped;
                break;
              end;
            end;
          end;
        end;
      end;
      SetLength( SurfaceList, Length( strlist ) );
      for i := Low( strlist ) to High( strlist ) do
      begin
        if ( FSolid ) then
        begin
          SurfaceList[ i ] := TTF_RenderText_Solid( FFont, PChar( string( strlist[ i ] ) ), FForeGroundColour );
        end
        else
        begin
          SurfaceList[ i ] := TTF_RenderText_Shaded( FFont, PChar( string( strlist[ i ] ) ), FForeGroundColour, FBackGroundColour );
        end;
      end;
    end;

    rtUTF8 :
    begin
      strChopped := aText;
      i := Length( strChopped );
      while ( i <> 0 ) do
      begin
        if ( string( strChopped[ i ] ) <> ' ' ) and ( Integer( string( strChopped[ i ] ) ) <> 13 ) then
          dec( i )
        else
        begin
          dec( i );
          strChopped := Copy( strChopped, 0, i );
          if TTF_SizeUTF8( FFont, PChar( string( strChopped ) ), textw, texth ) = 0 then
          begin
            if ( textw < aWidth )
              and ( texth < aHeight ) then
            begin
              SetLength( strlist, Length( strlist ) + 1 );
              strlist[ Length( strlist ) - 1 ] := strChopped;
              strChopped := Copy( aText, i + 2, Length( aText ) - ( i - 1 ) );
              i := Length( strChopped );
              if TTF_SizeUTF8( FFont, PChar( string( strChopped ) ), textw, texth ) = 0 then
              begin
                SetLength( strlist, Length( strlist ) + 1 );
                strlist[ Length( strlist ) - 1 ] := strChopped;
                break;
              end;
            end;
          end;
        end;
      end;
      SetLength( SurfaceList, Length( strlist ) );
      for i := Low( strlist ) to High( strlist ) do
      begin
        if ( FSolid ) then
        begin
          SurfaceList[ i ] := TTF_RenderUTF8_Solid( FFont, PChar( string( strlist[ i ] ) ), FForeGroundColour );
        end
        else
        begin
          SurfaceList[ i ] := TTF_RenderUTF8_Shaded( FFont, PChar( string( strlist[ i ] ) ), FForeGroundColour, FBackGroundColour );
        end;
      end;
    end;

    rtUnicode :
    begin
      strChopped := aText;
      i := Length( strChopped );
      while ( i <> 0 ) do
      begin
        if ( string( strChopped[ i ] ) <> ' ' ) and ( Integer( string( strChopped[ i ] ) ) <> 13 ) then
          dec( i )
        else
        begin
          dec( i );
          strChopped := Copy( strChopped, 0, i );
          if TTF_SizeUNICODE( FFont, PUInt16( strChopped ), textw, texth ) = 0 then
          begin
            if ( textw < aWidth )
              and ( texth < aHeight ) then
            begin
              SetLength( strlist, Length( strlist ) + 1 );
              strlist[ Length( strlist ) - 1 ] := strChopped;
              strChopped := Copy( aText, i + 2, Length( aText ) - ( i - 1 ) );
              i := Length( strChopped );
              if TTF_SizeUNICODE( FFont, PUInt16( strChopped ), textw, texth ) = 0 then
              begin
                SetLength( strlist, Length( strlist ) + 1 );
                strlist[ Length( strlist ) - 1 ] := strChopped;
                break;
              end;
            end;
          end;
        end;
      end;
      SetLength( SurfaceList, Length( strlist ) );
      for i := Low( strlist ) to High( strlist ) do
      begin
        if ( FSolid ) then
        begin
          SurfaceList[ i ] := TTF_RenderUNICODE_Solid( FFont, PUInt16( strlist[ i ] ), FForeGroundColour );
        end
        else
        begin
          SurfaceList[ i ] := TTF_RenderUNICODE_Shaded( FFont, PUInt16( strlist[ i ] ), FForeGroundColour, FBackGroundColour );
        end;
      end;
    end;
  end;

  // Now Draw the SurfaceList onto the resulting Surface
  yPos := 6;
  for i := Low( SurfaceList ) to High( SurfaceList ) do
  begin
    BltRect.x := 6;
    BltRect.y := yPos;
    BltRect.w := SurfaceList[ i ].w;
    BltRect.h := SurfaceList[ i ].h;
    SDL_BlitSurface( SurfaceList[ i ], nil, ReturnedSurface, @BltRect );
    yPos := yPos + TTF_FontHeight( FFont );
  end;
  result :=  ReturnedSurface;

  for i := Low( SurfaceList ) to High( SurfaceList ) do
  begin
    SDL_FreeSurface( SurfaceList[ i ] );
  end;
  SetLength( SurfaceList, 0 );
  SetLength( strlist, 0 );
end;

procedure TTrueTypeFont.PrepareFont;
var
  renderstyle : integer;
begin
  if FFont <> nil then
    TTF_CloseFont( FFont );

  FFont := TTF_OpenFont( PChar( FFontFile ), FFontSize );

  renderstyle := TTF_STYLE_NORMAL;
  if ( fsBold in FStyle ) then
    renderstyle := renderstyle or TTF_STYLE_BOLD;

  if ( fsItalic in FStyle ) then
    renderstyle := renderstyle or TTF_STYLE_ITALIC;

  if ( fsUnderline in FStyle ) then
    renderstyle := renderstyle or TTF_STYLE_UNDERLINE;

  TTF_SetFontStyle( FFont, renderstyle );
end;

end.

