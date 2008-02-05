unit sdl_image;
{
  $Id: sdl_image.pas,v 1.15 2007/12/05 22:52:23 savage Exp $
  
}
{******************************************************************************}
{                                                                              }
{       Borland Delphi SDL_Image - An example image loading library for use    }
{                                  with SDL                                    }
{       Conversion of the Simple DirectMedia Layer Image Headers               }
{                                                                              }
{ Portions created by Sam Lantinga <slouken@devolution.com> are                }
{ Copyright (C) 1997, 1998, 1999, 2000, 2001  Sam Lantinga                     }
{ 5635-34 Springhouse Dr.                                                      }
{ Pleasanton, CA 94588 (USA)                                                   }
{                                                                              }
{ All Rights Reserved.                                                         }
{                                                                              }
{ The original files are : SDL_image.h                                         }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Matthias Thoma <ma.thoma@gmx.de>                                             }
{                                                                              }
{ Portions created by Matthias Thoma are                                       }
{ Copyright (C) 2000 - 2001 Matthias Thoma.                                    }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
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
{   A simple library to load images of various formats as SDL surfaces         }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   SDL.pas in your search path.                                               }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{   See the Aliens Demo on how to make use of this libaray                     }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   April    02 2001 - MT : Initial Translation                                }
{                                                                              }
{   May      08 2001 - DL : Added ExternalSym derectives and copyright header  }
{                                                                              }
{   April   03 2003 - DL : Added jedi-sdl.inc include file to support more     }
{                          Pascal compilers. Initial support is now included   }
{                          for GnuPascal, VirtualPascal, TMT and obviously     }
{                          continue support for Delphi Kylix and FreePascal.   }
{                                                                              }
{   April   08 2003 - MK : Aka Mr Kroket - Added Better FPC support            }
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
  $Log: sdl_image.pas,v $
  Revision 1.15  2007/12/05 22:52:23  savage
  Better Mac OS X support for Frameworks.

  Revision 1.14  2007/05/29 21:31:13  savage
  Changes as suggested by Almindor for 64bit compatibility.

  Revision 1.13  2007/05/20 20:30:54  savage
  Initial Changes to Handle 64 Bits

  Revision 1.12  2006/12/02 00:14:40  savage
  Updated to latest version

  Revision 1.11  2005/04/10 18:22:59  savage
  Changes as suggested by Michalis, thanks.

  Revision 1.10  2005/04/10 11:48:33  savage
  Changes as suggested by Michalis, thanks.

  Revision 1.9  2005/01/05 01:47:07  savage
  Changed LibName to reflect what MacOS X should have. ie libSDL*-1.2.0.dylib respectively.

  Revision 1.8  2005/01/04 23:14:44  savage
  Changed LibName to reflect what most Linux distros will have. ie libSDL*-1.2.so.0 respectively.

  Revision 1.7  2005/01/01 02:03:12  savage
  Updated to v1.2.4

  Revision 1.6  2004/08/14 22:54:30  savage
  Updated so that Library name defines are correctly defined for MacOS X.

  Revision 1.5  2004/05/10 14:10:04  savage
  Initial MacOS X support. Fixed defines for MACOS ( Classic ) and DARWIN ( MacOS X ).

  Revision 1.4  2004/04/13 09:32:08  savage
  Changed Shared object names back to just the .so extension to avoid conflicts on various Linux/Unix distros. Therefore developers will need to create Symbolic links to the actual Share Objects if necessary.

  Revision 1.3  2004/04/01 20:53:23  savage
  Changed Linux Shared Object names so they reflect the Symbolic Links that are created when installing the RPMs from the SDL site.

  Revision 1.2  2004/03/30 20:23:28  savage
  Tidied up use of UNIX compiler directive.

  Revision 1.1  2004/02/14 23:35:42  savage
  version 1 of sdl_image, sdl_mixer and smpeg.


}  
{******************************************************************************}

{$I jedi-sdl.inc}

interface

uses
{$IFDEF __GPC__}
  gpc,
{$ENDIF}
  sdl;

const
{$IFDEF WINDOWS}
  SDL_ImageLibName =  'SDL_Image.dll';
{$ENDIF}

{$IFDEF UNIX}
{$IFDEF DARWIN}
  SDL_ImageLibName = 'libSDL_image-1.2.0.dylib';
{$ELSE}
  {$IFDEF FPC}
    SDL_ImageLibName = 'libSDL_image.so';
  {$ELSE}
    SDL_ImageLibName = 'libSDL_image-1.2.so.0';
  {$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFDEF MACOS}
  SDL_ImageLibName = 'SDL_image';
  {$linklib libSDL_image}
{$ENDIF}

  // Printable format: "%d.%d.%d", MAJOR, MINOR, PATCHLEVEL
  SDL_IMAGE_MAJOR_VERSION = 1;
{$EXTERNALSYM SDL_IMAGE_MAJOR_VERSION}
  SDL_IMAGE_MINOR_VERSION = 2;
{$EXTERNALSYM SDL_IMAGE_MINOR_VERSION}
  SDL_IMAGE_PATCHLEVEL    = 6;
{$EXTERNALSYM SDL_IMAGE_PATCHLEVEL}

{ This macro can be used to fill a version structure with the compile-time
  version of the SDL_image library. }
procedure SDL_IMAGE_VERSION( var X : TSDL_Version );
{$EXTERNALSYM SDL_IMAGE_VERSION}

{ This function gets the version of the dynamically linked SDL_image library.
   it should NOT be used to fill a version structure, instead you should
   use the SDL_IMAGE_VERSION() macro.
 }
function IMG_Linked_Version : PSDL_version;
external {$IFDEF __GPC__}name 'IMG_Linked_Version'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_Linked_Version}

{ Load an image from an SDL data source.
   The 'type' may be one of: "BMP", "GIF", "PNG", etc.

   If the image format supports a transparent pixel, SDL will set the
   colorkey for the surface.  You can enable RLE acceleration on the
   surface afterwards by calling:
        SDL_SetColorKey(image, SDL_RLEACCEL, image.format.colorkey);
}
function IMG_LoadTyped_RW(src: PSDL_RWops; freesrc: Integer; _type: PChar): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadTyped_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadTyped_RW}
{ Convenience functions }
function IMG_Load(const _file: PChar): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_Load'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_Load}
function IMG_Load_RW(src: PSDL_RWops; freesrc: Integer): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_Load_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_Load_RW}

{ Invert the alpha of a surface for use with OpenGL
  This function is now a no-op, and only provided for backwards compatibility. }
function IMG_InvertAlpha(_on: Integer): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_InvertAlpha'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_InvertAlpha}

{ Functions to detect a file type, given a seekable source }
function IMG_isBMP(src: PSDL_RWops): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_isBMP'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_isBMP}

function IMG_isGIF(src: PSDL_RWops): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_isGIF'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_isGIF}

function IMG_isJPG(src: PSDL_RWops): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_isJPG'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_isJPG}

function IMG_isLBM(src: PSDL_RWops): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_isLBM'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_isLBM}

function IMG_isPCX(src: PSDL_RWops): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_isPCX'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_isPCX}

function IMG_isPNG(src: PSDL_RWops): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_isPNG'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_isPNG}

function IMG_isPNM(src: PSDL_RWops): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_isPNM'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_isPNM}

function IMG_isTIF(src: PSDL_RWops): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_isTIF'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_isTIF}

function IMG_isXCF(src: PSDL_RWops): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_isXCF'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_isXCF}

function IMG_isXPM(src: PSDL_RWops): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_isXPM'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_isXPM}

function IMG_isXV(src: PSDL_RWops): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_isXV'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_isXV}


{ Individual loading functions }
function IMG_LoadBMP_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadBMP_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadBMP_RW}

function IMG_LoadGIF_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadGIF_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadGIF_RW}

function IMG_LoadJPG_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadJPG_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadJPG_RW}

function IMG_LoadLBM_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadLBM_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadLBM_RW}

function IMG_LoadPCX_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadPCX_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadPCX_RW}

function IMG_LoadPNM_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadPNM_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadPNM_RW}

function IMG_LoadPNG_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadPNG_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadPNG_RW}

function IMG_LoadTGA_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadTGA_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadTGA_RW}

function IMG_LoadTIF_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadTIF_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadTIF_RW}

function IMG_LoadXCF_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadXCF_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadXCF_RW}

function IMG_LoadXPM_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadXPM_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadXPM_RW}

function IMG_LoadXV_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadXV_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadXV_RW}

function IMG_ReadXPMFromArray( xpm : PPChar ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_ReadXPMFromArray'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_ReadXPMFromArray}




{ used internally, NOT an exported function }
//function IMG_string_equals( const str1 : PChar; const str2 : PChar ) : integer;
//cdecl; external {$IFDEF __GPC__}name 'IMG_string_equals'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
//{ $ EXTERNALSYM IMG_string_equals}

{ Error Macros }
{ We'll use SDL for reporting errors }
procedure IMG_SetError( fmt : PChar );

function IMG_GetError : PChar;

implementation

{$IFDEF __GPC__}
  {$L 'sdl_image'}  { link sdl_image.dll.a or libsdl_image.so or libsdl_image.a }
{$ENDIF}

procedure SDL_IMAGE_VERSION( var X : TSDL_Version );
begin
  X.major := SDL_IMAGE_MAJOR_VERSION;
  X.minor := SDL_IMAGE_MINOR_VERSION;
  X.patch := SDL_IMAGE_PATCHLEVEL;
end;

procedure IMG_SetError( fmt : PChar );
begin
  SDL_SetError( fmt );
end;

function IMG_GetError : PChar;
begin
  result := SDL_GetError;
end;

end.
