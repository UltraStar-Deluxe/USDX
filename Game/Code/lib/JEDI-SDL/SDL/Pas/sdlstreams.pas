unit sdlstreams;
{
  $Id: sdlstreams.pas,v 1.1 2004/02/05 00:08:20 savage Exp $
  
}
{******************************************************************}
{                                                                  }
{       SDL - Simple DirectMedia Layer                             }
{    Copyright (C) 1997, 1998, 1999, 2000, 2001  Sam Lantinga      }
{                                                                  }
{ Portions created by Chris Bruner are                             }
{ Copyright (C) 2002 Chris Bruner.                                 }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{                                                                  }
{                                                                  }
{ Obtained through:                                                }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )            }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/NPL/NPL-1_1Final.html                     }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{ Description                                                      }
{ -----------                                                      }
{  Shows how to use OpenGL to do 2D and 3D with the SDL libraries  }
{                                                                  }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary somewhere in your path                      }
{   The Latest SDL runtime can be found on http://www.libsdl.org   }
{                                                                  }
{ Programming Notes                                                }
{ -----------------                                                }
{                                                                  }
{                                                                  }
{                                                                  }
{                                                                  }
{                                                                  }
{ Revision History                                                 }
{ ----------------                                                 }
{   January  11 2002 - CB : Software embraced and extended by      }
{                           Chris Bruner of Crystal Software       }
{                           (Canada) Inc.                          }
{                                                                  }
{  February  11 2002 - DL : Added FreePascal support as suggested  }
{                        by "QuePasha Pepe" <mrkroket@hotmail.com> }
{                                                                  }
{******************************************************************}
{
  $Log: sdlstreams.pas,v $
  Revision 1.1  2004/02/05 00:08:20  savage
  Module 1.0 release

  
}

{$i jedi-sdl.inc}

interface

uses
  Classes,
  SysUtils,
  sdl,
  sdlutils;

{$IFDEF FPC}
type
  EinvalidContainer=class(Exception); 
 {$ENDIF}
  
function LoadSDLBMPFromStream( Stream : TStream ) : PSDL_Surface;
procedure SaveSDLBMPToStream( SDL_Surface : PSDL_Surface; stream : TStream );
function SDL_Swap16( D : UInt16 ) : Uint16;
function SDL_Swap32( D : UInt32 ) : Uint32;
function SDLStreamSetup( stream : TStream ) : PSDL_RWops;
// this only closes the SDL_RWops part of the stream, not the stream itself
procedure SDLStreamCloseRWops( SDL_RWops : PSDL_RWops );

implementation

function SDL_Swap16( D : UInt16 ) : Uint16;
begin
  Result := ( D shl 8 ) or ( D shr 8 );
end;

function SDL_Swap32( D : UInt32 ) : Uint32;
begin
  Result := ( ( D shl 24 ) or ( ( D shl 8 ) and $00FF0000 ) or ( ( D shr 8 ) and $0000FF00 ) or ( D shr 24 ) );
end;

(*function SDL_Swap64(D : UInt64) : Uint64;
var hi,lo : Uint32;
begin
        // Separate into high and low 32-bit resultues and swap them
        lo := Uint32(D and $0FFFFFFFF); // bloody pascal is too tight in it's type checking!
        D := D shr 32;
        hi = Uint32((D and $FFFFFFFF));
        result = SDL_Swap32(lo);
        result := result shl 32;
        result := result or SDL_Swap32(hi);
end;
*)

function SdlStreamSeek( context : PSDL_RWops; offset : Integer; whence : Integer ) : integer; cdecl;
var
  stream : TStream;
  origin : Word;
begin
  stream := TStream( context.unknown );
  if ( stream = nil ) then
    raise EInvalidContainer.Create( 'SDLStreamSeek on nil' );
  case whence of
    0 : origin := soFromBeginning; //	Offset is from the beginning of the resource. Seek moves to the position Offset. Offset must be >= 0.
    1 : origin := soFromCurrent; //	Offset is from the current position in the resource. Seek moves to Position + Offset.
    2 : origin := soFromEnd;
  else
    origin := soFromBeginning; // just in case
  end;
  Result := stream.Seek( offset, origin );
end;

function SDLStreamWrite( context : PSDL_RWops; Ptr : Pointer;
  size : Integer; num : Integer ) : Integer; cdecl;
var
  stream : TStream;
begin
  stream := TStream( context.unknown );
  if ( stream = nil ) then
    raise EInvalidContainer.Create( 'SDLStreamWrite on nil' );
  try
    Result := stream.Write( Ptr^, Size * num ) div size;
  except
    Result := -1;
  end;
end;

function SdlStreamRead( context : PSDL_RWops; Ptr : Pointer; size : Integer; maxnum
  : Integer ) : Integer; cdecl;
var
  stream : TStream;
begin
  stream := TStream( context.unknown );
  if ( stream = nil ) then
    raise EInvalidContainer.Create( 'SDLStreamRead on nil' );
  try
    Result := stream.read( Ptr^, Size * maxnum ) div size;
  except
    Result := -1;
  end;
end;

function SDLStreamClose( context : PSDL_RWops ) : Integer; cdecl;
var
  stream : TStream;
begin
  stream := TStream( context.unknown );
  if ( stream = nil ) then
    raise EInvalidContainer.Create( 'SDLStreamClose on nil' );
  stream.Free;
  Result := 1;
end;

function SDLStreamSetup( stream : TStream ) : PSDL_RWops;
begin
  result := SDL_AllocRW;
  if ( result = nil ) then
    raise EInvalidContainer.Create( 'could not create SDLStream on nil' );
  result.unknown := TUnknown( stream );
  result.seek := SDLStreamSeek;
  result.read := SDLStreamRead;
  result.write := SDLStreamWrite;
  result.close := SDLStreamClose;
  Result.type_ := 2; // TUnknown
end;

// this only closes the SDL part of the stream, not the context

procedure SDLStreamCloseRWops( SDL_RWops : PSDL_RWops );
begin
  SDL_FreeRW( SDL_RWops );
end;

function LoadSDLBMPFromStream( stream : TStream ) : PSDL_Surface;
var
  SDL_RWops : PSDL_RWops;
begin
  SDL_RWops := SDLStreamSetup( stream );
  result := SDL_LoadBMP_RW( SDL_RWops, 0 );
  SDLStreamCloseRWops( SDL_RWops );
end;

procedure SaveSDLBMPToStream( SDL_Surface : PSDL_Surface; stream : TStream );
var
  SDL_RWops : PSDL_RWops;
begin
  SDL_RWops := SDLStreamSetup( stream );
  SDL_SaveBMP_RW( SDL_Surface, SDL_RWops, 0 );
  SDLStreamCloseRWops( SDL_RWops );
end;

end.

