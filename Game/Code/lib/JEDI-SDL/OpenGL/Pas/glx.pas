unit glx;
{
  $Id: glx.pas,v 1.3 2006/11/20 21:20:59 savage Exp $

  Translation of the Mesa GLX headers for FreePascal
  Copyright (C) 1999 Sebastian Guenther


  Mesa 3-D graphics library
  Version:  3.0
  Copyright (C) 1995-1998  Brian Paul

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the Free
  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

// {$MODE delphi}  // objfpc would not work because of direct proc var assignments

{You have to enable Macros (compiler switch "-Sm") for compiling this unit!
 This is necessary for supporting different platforms with different calling
 conventions via a single unit.}

{
  $Log: glx.pas,v $
  Revision 1.3  2006/11/20 21:20:59  savage
  Updated to work in MacOS X

  Revision 1.2  2006/04/18 18:38:33  savage
  fixed boolean test - thanks grudzio

  Revision 1.1  2004/03/30 21:53:55  savage
  Moved to it's own folder.

  Revision 1.5  2004/02/15 22:48:35  savage
  More FPC and FreeBSD support changes.

  Revision 1.4  2004/02/14 22:36:29  savage
  Fixed inconsistencies of using LoadLibrary and LoadModule.
  Now all units make use of LoadModule rather than LoadLibrary and other dynamic proc procedures.

  Revision 1.3  2004/02/14 00:23:39  savage
  As UNIX is defined in jedi-sdl.inc this will be used to check linux compatability as well. Units have been changed to reflect this change.

  Revision 1.2  2004/02/14 00:09:19  savage
  Changed uses to now make use of moduleloader.pas rather than dllfuncs.pas

  Revision 1.1  2004/02/05 00:08:19  savage
  Module 1.0 release

  Revision 1.1  2003/05/11 13:18:03  savage
  Newest OpenGL Headers For Delphi, Kylix and FPC

  Revision 1.1  2002/10/13 13:57:31  sg
  * Finally, the new units are available: Match the C headers more closely;
    support for OpenGL extensions, and much more. Based on the Delphi units
    by Tom Nuydens of delphi3d.net

}

interface

{$I jedi-sdl.inc}

{$IFDEF UNIX}
  uses
    {$IFDEF FPC}
    x,
    xlib,
    xutil;
    {$ELSE}
    xlib;
    {$ENDIF}
  {$DEFINE HasGLX}  // Activate GLX stuff
{$ELSE}
  {$MESSAGE Unsupported platform.}
{$ENDIF}

{$IFNDEF HasGLX}
  {$MESSAGE GLX not present on this platform.}
{$ENDIF}


// =======================================================
//   Unit specific extensions
// =======================================================

// Note: Requires that the GL library has already been initialized
function InitGLX: Boolean;

var
  GLXDumpUnresolvedFunctions,
  GLXInitialized: Boolean;


// =======================================================
//   GLX consts, types and functions
// =======================================================

// Tokens for glXChooseVisual and glXGetConfig:
const
  GLX_USE_GL                            = 1;
  GLX_BUFFER_SIZE                       = 2;
  GLX_LEVEL                             = 3;
  GLX_RGBA                              = 4;
  GLX_DOUBLEBUFFER                      = 5;
  GLX_STEREO                            = 6;
  GLX_AUX_BUFFERS                       = 7;
  GLX_RED_SIZE                          = 8;
  GLX_GREEN_SIZE                        = 9;
  GLX_BLUE_SIZE                         = 10;
  GLX_ALPHA_SIZE                        = 11;
  GLX_DEPTH_SIZE                        = 12;
  GLX_STENCIL_SIZE                      = 13;
  GLX_ACCUM_RED_SIZE                    = 14;
  GLX_ACCUM_GREEN_SIZE                  = 15;
  GLX_ACCUM_BLUE_SIZE                   = 16;
  GLX_ACCUM_ALPHA_SIZE                  = 17;

  // GLX_EXT_visual_info extension
  GLX_X_VISUAL_TYPE_EXT                 = $22;
  GLX_TRANSPARENT_TYPE_EXT              = $23;
  GLX_TRANSPARENT_INDEX_VALUE_EXT       = $24;
  GLX_TRANSPARENT_RED_VALUE_EXT         = $25;
  GLX_TRANSPARENT_GREEN_VALUE_EXT       = $26;
  GLX_TRANSPARENT_BLUE_VALUE_EXT        = $27;
  GLX_TRANSPARENT_ALPHA_VALUE_EXT       = $28;


  // Error codes returned by glXGetConfig:
  GLX_BAD_SCREEN                        = 1;
  GLX_BAD_ATTRIBUTE                     = 2;
  GLX_NO_EXTENSION                      = 3;
  GLX_BAD_VISUAL                        = 4;
  GLX_BAD_CONTEXT                       = 5;
  GLX_BAD_VALUE                         = 6;
  GLX_BAD_ENUM                          = 7;

  // GLX 1.1 and later:
  GLX_VENDOR                            = 1;
  GLX_VERSION                           = 2;
  GLX_EXTENSIONS                        = 3;

  // GLX_visual_info extension
  GLX_TRUE_COLOR_EXT                    = $8002;
  GLX_DIRECT_COLOR_EXT                  = $8003;
  GLX_PSEUDO_COLOR_EXT                  = $8004;
  GLX_STATIC_COLOR_EXT                  = $8005;
  GLX_GRAY_SCALE_EXT                    = $8006;
  GLX_STATIC_GRAY_EXT                   = $8007;
  GLX_NONE_EXT                          = $8000;
  GLX_TRANSPARENT_RGB_EXT               = $8008;
  GLX_TRANSPARENT_INDEX_EXT             = $8009;

type
  // From XLib:
  {$IFNDEF FPC}
  TXID = XID;
  {$ENDIF}
  XPixmap = TXID;
  XFont = TXID;
  XColormap = TXID;

  GLXContext = Pointer;
  GLXPixmap = TXID;
  GLXDrawable = TXID;
  GLXContextID = TXID;

var
  glXChooseVisual: function(dpy: PDisplay; screen: Integer; var attribList: Integer): PXVisualInfo; cdecl;
  glXCreateContext: function(dpy: PDisplay; vis: PXVisualInfo; shareList: GLXContext; direct: Boolean): GLXContext; cdecl;
  glXDestroyContext: procedure(dpy: PDisplay; ctx: GLXContext); cdecl;
  glXMakeCurrent: function(dpy: PDisplay; drawable: GLXDrawable; ctx: GLXContext): Boolean; cdecl;
  glXCopyContext: procedure(dpy: PDisplay; src, dst: GLXContext; mask: LongWord); cdecl;
  glXSwapBuffers: procedure(dpy: PDisplay; drawable: GLXDrawable); cdecl;
  glXCreateGLXPixmap: function(dpy: PDisplay; visual: PXVisualInfo; pixmap: XPixmap): GLXPixmap; cdecl;
  glXDestroyGLXPixmap: procedure(dpy: PDisplay; pixmap: GLXPixmap); cdecl;
  glXQueryExtension: function(dpy: PDisplay; var errorb, event: Integer): Boolean; cdecl;
  glXQueryVersion: function(dpy: PDisplay; var maj, min: Integer): Boolean; cdecl;
  glXIsDirect: function(dpy: PDisplay; ctx: GLXContext): Boolean; cdecl;
  glXGetConfig: function(dpy: PDisplay; visual: PXVisualInfo; attrib: Integer; var value: Integer): Integer; cdecl;
  glXGetCurrentContext: function: GLXContext; cdecl;
  glXGetCurrentDrawable: function: GLXDrawable; cdecl;
  glXWaitGL: procedure; cdecl;
  glXWaitX: procedure; cdecl;
  glXUseXFont: procedure(font: XFont; first, count, list: Integer); cdecl;

  // GLX 1.1 and later
  glXQueryExtensionsString: function(dpy: PDisplay; screen: Integer): PChar; cdecl;
  glXQueryServerString: function(dpy: PDisplay; screen, name: Integer): PChar; cdecl;
  glXGetClientString: function(dpy: PDisplay; name: Integer): PChar; cdecl;

  // Mesa GLX Extensions
  glXCreateGLXPixmapMESA: function(dpy: PDisplay; visual: PXVisualInfo; pixmap: XPixmap; cmap: XColormap): GLXPixmap; cdecl;
  glXReleaseBufferMESA: function(dpy: PDisplay; d: GLXDrawable): Boolean; cdecl;
  glXCopySubBufferMESA: procedure(dpy: PDisplay; drawbale: GLXDrawable; x, y, width, height: Integer); cdecl;
  glXGetVideoSyncSGI: function(var counter: LongWord): Integer; cdecl;
  glXWaitVideoSyncSGI: function(divisor, remainder: Integer; var count: LongWord): Integer; cdecl;


// =======================================================
//
// =======================================================

implementation

uses
  {$IFNDEF __GPC__}
  SysUtils,
  {$ENDIF}
  moduleloader;

(* {$LINKLIB m}  *)

var
  libGLX: TModuleHandle;

function InitGLXFromLibrary( dll : PChar ): Boolean;
begin
  Result := False;

  if not LoadModule( libGLX, dll ) then
    exit;

  glXChooseVisual := GetModuleSymbol(libglx, 'glXChooseVisual');
  glXCreateContext := GetModuleSymbol(libglx, 'glXCreateContext');
  glXDestroyContext := GetModuleSymbol(libglx, 'glXDestroyContext');
  glXMakeCurrent := GetModuleSymbol(libglx, 'glXMakeCurrent');
  glXCopyContext := GetModuleSymbol(libglx, 'glXCopyContext');
  glXSwapBuffers := GetModuleSymbol(libglx, 'glXSwapBuffers');
  glXCreateGLXPixmap := GetModuleSymbol(libglx, 'glXCreateGLXPixmap');
  glXDestroyGLXPixmap := GetModuleSymbol(libglx, 'glXDestroyGLXPixmap');
  glXQueryExtension := GetModuleSymbol(libglx, 'glXQueryExtension');
  glXQueryVersion := GetModuleSymbol(libglx, 'glXQueryVersion');
  glXIsDirect := GetModuleSymbol(libglx, 'glXIsDirect');
  glXGetConfig := GetModuleSymbol(libglx, 'glXGetConfig');
  glXGetCurrentContext := GetModuleSymbol(libglx, 'glXGetCurrentContext');
  glXGetCurrentDrawable := GetModuleSymbol(libglx, 'glXGetCurrentDrawable');
  glXWaitGL := GetModuleSymbol(libglx, 'glXWaitGL');
  glXWaitX := GetModuleSymbol(libglx, 'glXWaitX');
  glXUseXFont := GetModuleSymbol(libglx, 'glXUseXFont');
  // GLX 1.1 and later
  glXQueryExtensionsString := GetModuleSymbol(libglx, 'glXQueryExtensionsString');
  glXQueryServerString := GetModuleSymbol(libglx, 'glXQueryServerString');
  glXGetClientString := GetModuleSymbol(libglx, 'glXGetClientString');
  // Mesa GLX Extensions
  glXCreateGLXPixmapMESA := GetModuleSymbol(libglx, 'glXCreateGLXPixmapMESA');
  glXReleaseBufferMESA := GetModuleSymbol(libglx, 'glXReleaseBufferMESA');
  glXCopySubBufferMESA := GetModuleSymbol(libglx, 'glXCopySubBufferMESA');
  glXGetVideoSyncSGI := GetModuleSymbol(libglx, 'glXGetVideoSyncSGI');
  glXWaitVideoSyncSGI := GetModuleSymbol(libglx, 'glXWaitVideoSyncSGI');

  GLXInitialized := True;
  Result := True;
end;

function InitGLX: Boolean;
begin
  Result := InitGLXFromLibrary('libGL.so') or
            InitGLXFromLibrary('libGL.so.1') or
            InitGLXFromLibrary('libMesaGL.so') or
            InitGLXFromLibrary('libMesaGL.so.3');
end;


initialization
  InitGLX;
finalization
  UnloadModule(libGLX);
end.
