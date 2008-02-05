unit glut;
{
  $Id: glut.pas,v 1.4 2007/05/20 20:28:31 savage Exp $

  Adaption of the delphi3d.net OpenGL units to FreePascal
  Sebastian Guenther (sg@freepascal.org) in 2002
  These units are free to use
}

// Copyright (c) Mark J. Kilgard, 1994, 1995, 1996. */

(* This program is freely distributable without licensing fees  and is
   provided without guarantee or warrantee expressed or  implied. This
   program is -not- in the public domain. *)

{******************************************************************************}
{                                                                              }
{ Converted to Delphi by Tom Nuydens (tom@delphi3d.net)                        }
{ For the latest updates, visit Delphi3D: http://www.delphi3d.net              }
{                                                                              }
{ Modified for Delphi/Kylix and FreePascal                                     }
{    by Dominique Louis ( Dominique@Savagesoftware.com.au)                     }
{ For the latest updates, visit JEDI-SDL : http://www.sf.net/projects/jedi-sdl }
{                                                                              }
{******************************************************************************}

{
  $Log: glut.pas,v $
  Revision 1.4  2007/05/20 20:28:31  savage
  Initial Changes to Handle 64 Bits

  Revision 1.3  2006/11/20 21:20:59  savage
  Updated to work in MacOS X

  Revision 1.2  2004/08/14 22:54:30  savage
  Updated so that Library name defines are correctly defined for MacOS X.

  Revision 1.1  2004/03/30 21:53:54  savage
  Moved to it's own folder.

  Revision 1.5  2004/02/20 17:09:55  savage
  Code tidied up in gl, glu and glut, while extensions in glext.pas are now loaded using SDL_GL_GetProcAddress, thus making it more cross-platform compatible, but now more tied to SDL.

  Revision 1.4  2004/02/14 22:36:29  savage
  Fixed inconsistencies of using LoadLibrary and LoadModule.
  Now all units make use of LoadModule rather than LoadLibrary and other dynamic proc procedures.

  Revision 1.3  2004/02/14 00:23:39  savage
  As UNIX is defined in jedi-sdl.inc this will be used to check linux compatability as well. Units have been changed to reflect this change.

  Revision 1.2  2004/02/14 00:09:19  savage
  Changed uses to now make use of moduleloader.pas rather than dllfuncs.pas

  Revision 1.1  2004/02/05 00:08:19  savage
  Module 1.0 release

  Revision 1.4  2003/06/02 12:32:13  savage
  Modified Sources to avoid warnings with Delphi by moving CVS Logging to the top of the header files. Hopefully CVS Logging still works.

}

interface

{$I jedi-sdl.inc}

uses
{$IFDEF __GPC__}
  system,
  gpc,
{$ENDIF}

{$IFDEF WINDOWS}
  Windows,
{$ENDIF}
  moduleloader,
  gl;

type
  {$IFNDEF __GPC__}
  PInteger = ^Integer;
  PPChar = ^PChar;
  {$ENDIF}
  TGlutVoidCallback = procedure; {$IFNDEF __GPC__}{$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}{$ENDIF}
  TGlut1IntCallback = procedure(value: Integer); {$IFNDEF __GPC__}{$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}{$ENDIF}
  TGlut2IntCallback = procedure(v1, v2: Integer); {$IFNDEF __GPC__}{$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}{$ENDIF}
  TGlut3IntCallback = procedure(v1, v2, v3: Integer); {$IFNDEF __GPC__}{$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}{$ENDIF}
  TGlut4IntCallback = procedure(v1, v2, v3, v4: Integer); {$IFNDEF __GPC__}{$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}{$ENDIF}
  TGlut1Char2IntCallback = procedure(c: Byte; v1, v2: Integer); {$IFNDEF __GPC__}{$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}{$ENDIF}

const
{$IFDEF WINDOWS}
  GlutLibName = 'glut32.dll';
{$ENDIF}

{$IFDEF UNIX}
{$IFDEF DARWIN}
  GlutLibName = '/System/Library/Frameworks/GLUT.framework/Libraries/libglut.dylib';
{$ELSE}
  GlutLibName = 'libglut.so';
{$ENDIF}
{$ENDIF}

  GLUT_API_VERSION                = 3;
  GLUT_XLIB_IMPLEMENTATION        = 12;
  // Display mode bit masks.
  GLUT_RGB                        = 0;
  GLUT_RGBA                       = GLUT_RGB;
  GLUT_INDEX                      = 1;
  GLUT_SINGLE                     = 0;
  GLUT_DOUBLE                     = 2;
  GLUT_ACCUM                      = 4;
  GLUT_ALPHA                      = 8;
  GLUT_DEPTH                      = 16;
  GLUT_STENCIL                    = 32;
  GLUT_MULTISAMPLE                = 128;
  GLUT_STEREO                     = 256;
  GLUT_LUMINANCE                  = 512;

  // Mouse buttons.
  GLUT_LEFT_BUTTON                = 0;
  GLUT_MIDDLE_BUTTON              = 1;
  GLUT_RIGHT_BUTTON               = 2;

  // Mouse button state.
  GLUT_DOWN                       = 0;
  GLUT_UP                         = 1;

  // function keys
  GLUT_KEY_F1                     = 1;
  GLUT_KEY_F2                     = 2;
  GLUT_KEY_F3                     = 3;
  GLUT_KEY_F4                     = 4;
  GLUT_KEY_F5                     = 5;
  GLUT_KEY_F6                     = 6;
  GLUT_KEY_F7                     = 7;
  GLUT_KEY_F8                     = 8;
  GLUT_KEY_F9                     = 9;
  GLUT_KEY_F10                    = 10;
  GLUT_KEY_F11                    = 11;
  GLUT_KEY_F12                    = 12;
  // directional keys
  GLUT_KEY_LEFT                   = 100;
  GLUT_KEY_UP                     = 101;
  GLUT_KEY_RIGHT                  = 102;
  GLUT_KEY_DOWN                   = 103;
  GLUT_KEY_PAGE_UP                = 104;
  GLUT_KEY_PAGE_DOWN              = 105;
  GLUT_KEY_HOME                   = 106;
  GLUT_KEY_END                    = 107;
  GLUT_KEY_INSERT                 = 108;

  // Entry/exit  state.
  GLUT_LEFT                       = 0;
  GLUT_ENTERED                    = 1;

  // Menu usage state.
  GLUT_MENU_NOT_IN_USE            = 0;
  GLUT_MENU_IN_USE                = 1;

  // Visibility  state.
  GLUT_NOT_VISIBLE                = 0;
  GLUT_VISIBLE                    = 1;

  // Window status  state.
  GLUT_HIDDEN                     = 0;
  GLUT_FULLY_RETAINED             = 1;
  GLUT_PARTIALLY_RETAINED         = 2;
  GLUT_FULLY_COVERED              = 3;

  // Color index component selection values.
  GLUT_RED                        = 0;
  GLUT_GREEN                      = 1;
  GLUT_BLUE                       = 2;

  // Layers for use.
  GLUT_NORMAL                     = 0;
  GLUT_OVERLAY                    = 1;

  // Stroke font constants (use these in GLUT program).
  GLUT_STROKE_ROMAN		  = Pointer(0);
  GLUT_STROKE_MONO_ROMAN	  = Pointer(1);

  // Bitmap font constants (use these in GLUT program).
  GLUT_BITMAP_9_BY_15		  = Pointer(2);
  GLUT_BITMAP_8_BY_13		  = Pointer(3);
  GLUT_BITMAP_TIMES_ROMAN_10	  = Pointer(4);
  GLUT_BITMAP_TIMES_ROMAN_24	  = Pointer(5);
  GLUT_BITMAP_HELVETICA_10	  = Pointer(6);
  GLUT_BITMAP_HELVETICA_12	  = Pointer(7);
  GLUT_BITMAP_HELVETICA_18	  = Pointer(8);

  // glutGet parameters.
  GLUT_WINDOW_X                   = 100;
  GLUT_WINDOW_Y                   = 101;
  GLUT_WINDOW_WIDTH               = 102;
  GLUT_WINDOW_HEIGHT              = 103;
  GLUT_WINDOW_BUFFER_SIZE         = 104;
  GLUT_WINDOW_STENCIL_SIZE        = 105;
  GLUT_WINDOW_DEPTH_SIZE          = 106;
  GLUT_WINDOW_RED_SIZE            = 107;
  GLUT_WINDOW_GREEN_SIZE          = 108;
  GLUT_WINDOW_BLUE_SIZE           = 109;
  GLUT_WINDOW_ALPHA_SIZE          = 110;
  GLUT_WINDOW_ACCUM_RED_SIZE      = 111;
  GLUT_WINDOW_ACCUM_GREEN_SIZE    = 112;
  GLUT_WINDOW_ACCUM_BLUE_SIZE     = 113;
  GLUT_WINDOW_ACCUM_ALPHA_SIZE    = 114;
  GLUT_WINDOW_DOUBLEBUFFER        = 115;
  GLUT_WINDOW_RGBA                = 116;
  GLUT_WINDOW_PARENT              = 117;
  GLUT_WINDOW_NUM_CHILDREN        = 118;
  GLUT_WINDOW_COLORMAP_SIZE       = 119;
  GLUT_WINDOW_NUM_SAMPLES         = 120;
  GLUT_WINDOW_STEREO              = 121;
  GLUT_WINDOW_CURSOR              = 122;
  GLUT_SCREEN_WIDTH               = 200;
  GLUT_SCREEN_HEIGHT              = 201;
  GLUT_SCREEN_WIDTH_MM            = 202;
  GLUT_SCREEN_HEIGHT_MM           = 203;
  GLUT_MENU_NUM_ITEMS             = 300;
  GLUT_DISPLAY_MODE_POSSIBLE      = 400;
  GLUT_INIT_WINDOW_X              = 500;
  GLUT_INIT_WINDOW_Y              = 501;
  GLUT_INIT_WINDOW_WIDTH          = 502;
  GLUT_INIT_WINDOW_HEIGHT         = 503;
  GLUT_INIT_DISPLAY_MODE          = 504;
  GLUT_ELAPSED_TIME               = 700;

  // glutDeviceGet parameters.
  GLUT_HAS_KEYBOARD               = 600;
  GLUT_HAS_MOUSE                  = 601;
  GLUT_HAS_SPACEBALL              = 602;
  GLUT_HAS_DIAL_AND_BUTTON_BOX    = 603;
  GLUT_HAS_TABLET                 = 604;
  GLUT_NUM_MOUSE_BUTTONS          = 605;
  GLUT_NUM_SPACEBALL_BUTTONS      = 606;
  GLUT_NUM_BUTTON_BOX_BUTTONS     = 607;
  GLUT_NUM_DIALS                  = 608;
  GLUT_NUM_TABLET_BUTTONS         = 609;

  // glutLayerGet parameters.
  GLUT_OVERLAY_POSSIBLE           = 800;
  GLUT_LAYER_IN_USE               = 801;
  GLUT_HAS_OVERLAY                = 802;
  GLUT_TRANSPARENT_INDEX          = 803;
  GLUT_NORMAL_DAMAGED             = 804;
  GLUT_OVERLAY_DAMAGED            = 805;

  // glutVideoResizeGet parameters.
  GLUT_VIDEO_RESIZE_POSSIBLE       = 900;
  GLUT_VIDEO_RESIZE_IN_USE         = 901;
  GLUT_VIDEO_RESIZE_X_DELTA        = 902;
  GLUT_VIDEO_RESIZE_Y_DELTA        = 903;
  GLUT_VIDEO_RESIZE_WIDTH_DELTA    = 904;
  GLUT_VIDEO_RESIZE_HEIGHT_DELTA   = 905;
  GLUT_VIDEO_RESIZE_X              = 906;
  GLUT_VIDEO_RESIZE_Y              = 907;
  GLUT_VIDEO_RESIZE_WIDTH          = 908;
  GLUT_VIDEO_RESIZE_HEIGHT         = 909;

  // glutGetModifiers return mask.
  GLUT_ACTIVE_SHIFT                = 1;
  GLUT_ACTIVE_CTRL                 = 2;
  GLUT_ACTIVE_ALT                  = 4;

  // glutSetCursor parameters.
  // Basic arrows.
  GLUT_CURSOR_RIGHT_ARROW          = 0;
  GLUT_CURSOR_LEFT_ARROW           = 1;
  // Symbolic cursor shapes.
  GLUT_CURSOR_INFO                 = 2;
  GLUT_CURSOR_DESTROY              = 3;
  GLUT_CURSOR_HELP                 = 4;
  GLUT_CURSOR_CYCLE                = 5;
  GLUT_CURSOR_SPRAY                = 6;
  GLUT_CURSOR_WAIT                 = 7;
  GLUT_CURSOR_TEXT                 = 8;
  GLUT_CURSOR_CROSSHAIR            = 9;
  // Directional cursors.
  GLUT_CURSOR_UP_DOWN              = 10;
  GLUT_CURSOR_LEFT_RIGHT           = 11;
  // Sizing cursors.
  GLUT_CURSOR_TOP_SIDE             = 12;
  GLUT_CURSOR_BOTTOM_SIDE          = 13;
  GLUT_CURSOR_LEFT_SIDE            = 14;
  GLUT_CURSOR_RIGHT_SIDE           = 15;
  GLUT_CURSOR_TOP_LEFT_CORNER      = 16;
  GLUT_CURSOR_TOP_RIGHT_CORNER     = 17;
  GLUT_CURSOR_BOTTOM_RIGHT_CORNER  = 18;
  GLUT_CURSOR_BOTTOM_LEFT_CORNER   = 19;
  // Inherit from parent window.
  GLUT_CURSOR_INHERIT              = 100;
  // Blank cursor.
  GLUT_CURSOR_NONE                 = 101;
  // Fullscreen crosshair (if available).
  GLUT_CURSOR_FULL_CROSSHAIR       = 102;

  // GLUT game mode sub-API.
  // glutGameModeGet.
  GLUT_GAME_MODE_ACTIVE           = 0;
  GLUT_GAME_MODE_POSSIBLE         = 1;
  GLUT_GAME_MODE_WIDTH            = 2;
  GLUT_GAME_MODE_HEIGHT           = 3;
  GLUT_GAME_MODE_PIXEL_DEPTH      = 4;
  GLUT_GAME_MODE_REFRESH_RATE     = 5;
  GLUT_GAME_MODE_DISPLAY_CHANGED  = 6;

var
// GLUT initialization sub-API.
  glutInit: procedure(argcp: PInteger; argv: PPChar); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutInitDisplayMode: procedure(mode: Word); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutInitDisplayString: procedure(const str: PChar); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutInitWindowPosition: procedure(x, y: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutInitWindowSize: procedure(width, height: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutMainLoop: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

// GLUT window sub-API.
  glutCreateWindow: function(const title: PChar): Integer; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutCreateSubWindow: function(win, x, y, width, height: Integer): Integer; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutDestroyWindow: procedure(win: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutPostRedisplay: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutPostWindowRedisplay: procedure(win: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutSwapBuffers: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutGetWindow: function: Integer; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutSetWindow: procedure(win: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutSetWindowTitle: procedure(const title: PChar); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutSetIconTitle: procedure(const title: PChar); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutPositionWindow: procedure(x, y: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutReshapeWindow: procedure(width, height: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutPopWindow: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutPushWindow: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutIconifyWindow: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutShowWindow: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutHideWindow: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutFullScreen: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutSetCursor: procedure(cursor: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutWarpPointer: procedure(x, y: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

// GLUT overlay sub-API.
  glutEstablishOverlay: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutRemoveOverlay: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutUseLayer: procedure(layer: GLenum); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutPostOverlayRedisplay: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutPostWindowOverlayRedisplay: procedure(win: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutShowOverlay: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutHideOverlay: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

// GLUT menu sub-API.
  glutCreateMenu: function(callback: TGlut1IntCallback): Integer; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutDestroyMenu: procedure(menu: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutGetMenu: function: Integer; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutSetMenu: procedure(menu: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutAddMenuEntry: procedure(const caption: PChar; value: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutAddSubMenu: procedure(const caption: PChar; submenu: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutChangeToMenuEntry: procedure(item: Integer; const caption: PChar; value: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutChangeToSubMenu: procedure(item: Integer; const caption: PChar; submenu: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutRemoveMenuItem: procedure(item: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutAttachMenu: procedure(button: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutDetachMenu: procedure(button: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

// GLUTsub-API.
  glutDisplayFunc: procedure(f: TGlutVoidCallback); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutReshapeFunc: procedure(f: TGlut2IntCallback); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutKeyboardFunc: procedure(f: TGlut1Char2IntCallback); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutMouseFunc: procedure(f: TGlut4IntCallback); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutMotionFunc: procedure(f: TGlut2IntCallback); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutPassiveMotionFunc: procedure(f: TGlut2IntCallback); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutEntryFunc: procedure(f: TGlut1IntCallback); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutVisibilityFunc: procedure(f: TGlut1IntCallback); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutIdleFunc: procedure(f: TGlutVoidCallback); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutTimerFunc: procedure(millis: Word; f: TGlut1IntCallback; value: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutMenuStateFunc: procedure(f: TGlut1IntCallback); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutSpecialFunc: procedure(f: TGlut3IntCallback); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutSpaceballMotionFunc: procedure(f: TGlut3IntCallback); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutSpaceballRotateFunc: procedure(f: TGlut3IntCallback); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutSpaceballButtonFunc: procedure(f: TGlut2IntCallback); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutButtonBoxFunc: procedure(f: TGlut2IntCallback); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutDialsFunc: procedure(f: TGlut2IntCallback); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutTabletMotionFunc: procedure(f: TGlut2IntCallback); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutTabletButtonFunc: procedure(f: TGlut4IntCallback); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutMenuStatusFunc: procedure(f: TGlut3IntCallback); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutOverlayDisplayFunc: procedure(f:TGlutVoidCallback); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutWindowStatusFunc: procedure(f: TGlut1IntCallback); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

// GLUT color index sub-API.
  glutSetColor: procedure(cell: Integer; red, green, blue: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutGetColor: function(ndx, component: Integer): GLfloat; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutCopyColormap: procedure(win: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

// GLUT state retrieval sub-API.
  glutGet: function(t: GLenum): Integer; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutDeviceGet: function(t: GLenum): Integer; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

// GLUT extension support sub-API
  glutExtensionSupported: function(const name: PChar): Integer; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutGetModifiers: function: Integer; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutLayerGet: function(t: GLenum): Integer; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

// GLUT font sub-API
  glutBitmapCharacter: procedure(font : pointer; character: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutBitmapWidth: function(font : pointer; character: Integer): Integer; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutStrokeCharacter: procedure(font : pointer; character: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutStrokeWidth: function(font : pointer; character: Integer): Integer; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutBitmapLength: function(font: pointer; const str: PChar): Integer; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutStrokeLength: function(font: pointer; const str: PChar): Integer; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

// GLUT pre-built models sub-API
  glutWireSphere: procedure(radius: GLdouble; slices, stacks: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutSolidSphere: procedure(radius: GLdouble; slices, stacks: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutWireCone: procedure(base, height: GLdouble; slices, stacks: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutSolidCone: procedure(base, height: GLdouble; slices, stacks: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutWireCube: procedure(size: GLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutSolidCube: procedure(size: GLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutWireTorus: procedure(innerRadius, outerRadius: GLdouble; sides, rings: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutSolidTorus: procedure(innerRadius, outerRadius: GLdouble; sides, rings: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutWireDodecahedron: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutSolidDodecahedron: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutWireTeapot: procedure(size: GLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutSolidTeapot: procedure(size: GLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutWireOctahedron: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutSolidOctahedron: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutWireTetrahedron: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutSolidTetrahedron: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutWireIcosahedron: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutSolidIcosahedron: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

// GLUT video resize sub-API.
  glutVideoResizeGet: function(param: GLenum): Integer; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutSetupVideoResizing: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutStopVideoResizing: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutVideoResize: procedure(x, y, width, height: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutVideoPan: procedure(x, y, width, height: Integer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

// GLUT debugging sub-API.
  glutReportErrors: procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

var
  //example glutGameModeString('1280x1024:32@75');
  glutGameModeString : procedure (const AString : PChar); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutEnterGameMode : function : integer; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutLeaveGameMode : procedure; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glutGameModeGet : function (mode : GLenum) : integer; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF} 

procedure LoadGlut(const dll: PChar);
procedure FreeGlut;

implementation

var
  LibGLUT : TModuleHandle;

procedure FreeGlut;
begin

  UnLoadModule( LibGLUT );

  @glutInit := nil;
  @glutInitDisplayMode := nil;
  @glutInitDisplayString := nil;
  @glutInitWindowPosition := nil;
  @glutInitWindowSize := nil;
  @glutMainLoop := nil;
  @glutCreateWindow := nil;
  @glutCreateSubWindow := nil;
  @glutDestroyWindow := nil;
  @glutPostRedisplay := nil;
  @glutPostWindowRedisplay := nil;
  @glutSwapBuffers := nil;
  @glutGetWindow := nil;
  @glutSetWindow := nil;
  @glutSetWindowTitle := nil;
  @glutSetIconTitle := nil;
  @glutPositionWindow := nil;
  @glutReshapeWindow := nil;
  @glutPopWindow := nil;
  @glutPushWindow := nil;
  @glutIconifyWindow := nil;
  @glutShowWindow := nil;
  @glutHideWindow := nil;
  @glutFullScreen := nil;
  @glutSetCursor := nil;
  @glutWarpPointer := nil;
  @glutEstablishOverlay := nil;
  @glutRemoveOverlay := nil;
  @glutUseLayer := nil;
  @glutPostOverlayRedisplay := nil;
  @glutPostWindowOverlayRedisplay := nil;
  @glutShowOverlay := nil;
  @glutHideOverlay := nil;
  @glutCreateMenu := nil;
  @glutDestroyMenu := nil;
  @glutGetMenu := nil;
  @glutSetMenu := nil;
  @glutAddMenuEntry := nil;
  @glutAddSubMenu := nil;
  @glutChangeToMenuEntry := nil;
  @glutChangeToSubMenu := nil;
  @glutRemoveMenuItem := nil;
  @glutAttachMenu := nil;
  @glutDetachMenu := nil;
  @glutDisplayFunc := nil;
  @glutReshapeFunc := nil;
  @glutKeyboardFunc := nil;
  @glutMouseFunc := nil;
  @glutMotionFunc := nil;
  @glutPassiveMotionFunc := nil;
  @glutEntryFunc := nil;
  @glutVisibilityFunc := nil;
  @glutIdleFunc := nil;
  @glutTimerFunc := nil;
  @glutMenuStateFunc := nil;
  @glutSpecialFunc := nil;
  @glutSpaceballMotionFunc := nil;
  @glutSpaceballRotateFunc := nil;
  @glutSpaceballButtonFunc := nil;
  @glutButtonBoxFunc := nil;
  @glutDialsFunc := nil;
  @glutTabletMotionFunc := nil;
  @glutTabletButtonFunc := nil;
  @glutMenuStatusFunc := nil;
  @glutOverlayDisplayFunc := nil;
  @glutWindowStatusFunc := nil;
  @glutSetColor := nil;
  @glutGetColor := nil;
  @glutCopyColormap := nil;
  @glutGet := nil;
  @glutDeviceGet := nil;
  @glutExtensionSupported := nil;
  @glutGetModifiers := nil;
  @glutLayerGet := nil;
  @glutBitmapCharacter := nil;
  @glutBitmapWidth := nil;
  @glutStrokeCharacter := nil;
  @glutStrokeWidth := nil;
  @glutBitmapLength := nil;
  @glutStrokeLength := nil;
  @glutWireSphere := nil;
  @glutSolidSphere := nil;
  @glutWireCone := nil;
  @glutSolidCone := nil;
  @glutWireCube := nil;
  @glutSolidCube := nil;
  @glutWireTorus := nil;
  @glutSolidTorus := nil;
  @glutWireDodecahedron := nil;
  @glutSolidDodecahedron := nil;
  @glutWireTeapot := nil;
  @glutSolidTeapot := nil;
  @glutWireOctahedron := nil;
  @glutSolidOctahedron := nil;
  @glutWireTetrahedron := nil;
  @glutSolidTetrahedron := nil;
  @glutWireIcosahedron := nil;
  @glutSolidIcosahedron := nil;
  @glutVideoResizeGet := nil;
  @glutSetupVideoResizing := nil;
  @glutStopVideoResizing := nil;
  @glutVideoResize := nil;
  @glutVideoPan := nil;
  @glutReportErrors := nil;

end;

procedure LoadGlut(const dll: PChar);
begin

  FreeGlut;

  if LoadModule( LibGLUT, dll ) then
  begin
    @glutInit := GetModuleSymbol(LibGLUT, 'glutInit');
    @glutInitDisplayMode := GetModuleSymbol(LibGLUT, 'glutInitDisplayMode');
    @glutInitDisplayString := GetModuleSymbol(LibGLUT, 'glutInitDisplayString');
    @glutInitWindowPosition := GetModuleSymbol(LibGLUT, 'glutInitWindowPosition');
    @glutInitWindowSize := GetModuleSymbol(LibGLUT, 'glutInitWindowSize');
    @glutMainLoop := GetModuleSymbol(LibGLUT, 'glutMainLoop');
    @glutCreateWindow := GetModuleSymbol(LibGLUT, 'glutCreateWindow');
    @glutCreateSubWindow := GetModuleSymbol(LibGLUT, 'glutCreateSubWindow');
    @glutDestroyWindow := GetModuleSymbol(LibGLUT, 'glutDestroyWindow');
    @glutPostRedisplay := GetModuleSymbol(LibGLUT, 'glutPostRedisplay');
    @glutPostWindowRedisplay := GetModuleSymbol(LibGLUT, 'glutPostWindowRedisplay');
    @glutSwapBuffers := GetModuleSymbol(LibGLUT, 'glutSwapBuffers');
    @glutGetWindow := GetModuleSymbol(LibGLUT, 'glutGetWindow');
    @glutSetWindow := GetModuleSymbol(LibGLUT, 'glutSetWindow');
    @glutSetWindowTitle := GetModuleSymbol(LibGLUT, 'glutSetWindowTitle');
    @glutSetIconTitle := GetModuleSymbol(LibGLUT, 'glutSetIconTitle');
    @glutPositionWindow := GetModuleSymbol(LibGLUT, 'glutPositionWindow');
    @glutReshapeWindow := GetModuleSymbol(LibGLUT, 'glutReshapeWindow');
    @glutPopWindow := GetModuleSymbol(LibGLUT, 'glutPopWindow');
    @glutPushWindow := GetModuleSymbol(LibGLUT, 'glutPushWindow');
    @glutIconifyWindow := GetModuleSymbol(LibGLUT, 'glutIconifyWindow');
    @glutShowWindow := GetModuleSymbol(LibGLUT, 'glutShowWindow');
    @glutHideWindow := GetModuleSymbol(LibGLUT, 'glutHideWindow');
    @glutFullScreen := GetModuleSymbol(LibGLUT, 'glutFullScreen');
    @glutSetCursor := GetModuleSymbol(LibGLUT, 'glutSetCursor');
    @glutWarpPointer := GetModuleSymbol(LibGLUT, 'glutWarpPointer');
    @glutEstablishOverlay := GetModuleSymbol(LibGLUT, 'glutEstablishOverlay');
    @glutRemoveOverlay := GetModuleSymbol(LibGLUT, 'glutRemoveOverlay');
    @glutUseLayer := GetModuleSymbol(LibGLUT, 'glutUseLayer');
    @glutPostOverlayRedisplay := GetModuleSymbol(LibGLUT, 'glutPostOverlayRedisplay');
    @glutPostWindowOverlayRedisplay := GetModuleSymbol(LibGLUT, 'glutPostWindowOverlayRedisplay');
    @glutShowOverlay := GetModuleSymbol(LibGLUT, 'glutShowOverlay');
    @glutHideOverlay := GetModuleSymbol(LibGLUT, 'glutHideOverlay');
    @glutCreateMenu := GetModuleSymbol(LibGLUT, 'glutCreateMenu');
    @glutDestroyMenu := GetModuleSymbol(LibGLUT, 'glutDestroyMenu');
    @glutGetMenu := GetModuleSymbol(LibGLUT, 'glutGetMenu');
    @glutSetMenu := GetModuleSymbol(LibGLUT, 'glutSetMenu');
    @glutAddMenuEntry := GetModuleSymbol(LibGLUT, 'glutAddMenuEntry');
    @glutAddSubMenu := GetModuleSymbol(LibGLUT, 'glutAddSubMenu');
    @glutChangeToMenuEntry := GetModuleSymbol(LibGLUT, 'glutChangeToMenuEntry');
    @glutChangeToSubMenu := GetModuleSymbol(LibGLUT, 'glutChangeToSubMenu');
    @glutRemoveMenuItem := GetModuleSymbol(LibGLUT, 'glutRemoveMenuItem');
    @glutAttachMenu := GetModuleSymbol(LibGLUT, 'glutAttachMenu');
    @glutDetachMenu := GetModuleSymbol(LibGLUT, 'glutDetachMenu');
    @glutDisplayFunc := GetModuleSymbol(LibGLUT, 'glutDisplayFunc');
    @glutReshapeFunc := GetModuleSymbol(LibGLUT, 'glutReshapeFunc');
    @glutKeyboardFunc := GetModuleSymbol(LibGLUT, 'glutKeyboardFunc');
    @glutMouseFunc := GetModuleSymbol(LibGLUT, 'glutMouseFunc');
    @glutMotionFunc := GetModuleSymbol(LibGLUT, 'glutMotionFunc');
    @glutPassiveMotionFunc := GetModuleSymbol(LibGLUT, 'glutPassiveMotionFunc');
    @glutEntryFunc := GetModuleSymbol(LibGLUT, 'glutEntryFunc');
    @glutVisibilityFunc := GetModuleSymbol(LibGLUT, 'glutVisibilityFunc');
    @glutIdleFunc := GetModuleSymbol(LibGLUT, 'glutIdleFunc');
    @glutTimerFunc := GetModuleSymbol(LibGLUT, 'glutTimerFunc');
    @glutMenuStateFunc := GetModuleSymbol(LibGLUT, 'glutMenuStateFunc');
    @glutSpecialFunc := GetModuleSymbol(LibGLUT, 'glutSpecialFunc');
    @glutSpaceballMotionFunc := GetModuleSymbol(LibGLUT, 'glutSpaceballMotionFunc');
    @glutSpaceballRotateFunc := GetModuleSymbol(LibGLUT, 'glutSpaceballRotateFunc');
    @glutSpaceballButtonFunc := GetModuleSymbol(LibGLUT, 'glutSpaceballButtonFunc');
    @glutButtonBoxFunc := GetModuleSymbol(LibGLUT, 'glutButtonBoxFunc');
    @glutDialsFunc := GetModuleSymbol(LibGLUT, 'glutDialsFunc');
    @glutTabletMotionFunc := GetModuleSymbol(LibGLUT, 'glutTabletMotionFunc');
    @glutTabletButtonFunc := GetModuleSymbol(LibGLUT, 'glutTabletButtonFunc');
    @glutMenuStatusFunc := GetModuleSymbol(LibGLUT, 'glutMenuStatusFunc');
    @glutOverlayDisplayFunc := GetModuleSymbol(LibGLUT, 'glutOverlayDisplayFunc');
    @glutWindowStatusFunc := GetModuleSymbol(LibGLUT, 'glutWindowStatusFunc');
    @glutSetColor := GetModuleSymbol(LibGLUT, 'glutSetColor');
    @glutGetColor := GetModuleSymbol(LibGLUT, 'glutGetColor');
    @glutCopyColormap := GetModuleSymbol(LibGLUT, 'glutCopyColormap');
    @glutGet := GetModuleSymbol(LibGLUT, 'glutGet');
    @glutDeviceGet := GetModuleSymbol(LibGLUT, 'glutDeviceGet');
    @glutExtensionSupported := GetModuleSymbol(LibGLUT, 'glutExtensionSupported');
    @glutGetModifiers := GetModuleSymbol(LibGLUT, 'glutGetModifiers');
    @glutLayerGet := GetModuleSymbol(LibGLUT, 'glutLayerGet');
    @glutBitmapCharacter := GetModuleSymbol(LibGLUT, 'glutBitmapCharacter');
    @glutBitmapWidth := GetModuleSymbol(LibGLUT, 'glutBitmapWidth');
    @glutStrokeCharacter := GetModuleSymbol(LibGLUT, 'glutStrokeCharacter');
    @glutStrokeWidth := GetModuleSymbol(LibGLUT, 'glutStrokeWidth');
    @glutBitmapLength := GetModuleSymbol(LibGLUT, 'glutBitmapLength');
    @glutStrokeLength := GetModuleSymbol(LibGLUT, 'glutStrokeLength');
    @glutWireSphere := GetModuleSymbol(LibGLUT, 'glutWireSphere');
    @glutSolidSphere := GetModuleSymbol(LibGLUT, 'glutSolidSphere');
    @glutWireCone := GetModuleSymbol(LibGLUT, 'glutWireCone');
    @glutSolidCone := GetModuleSymbol(LibGLUT, 'glutSolidCone');
    @glutWireCube := GetModuleSymbol(LibGLUT, 'glutWireCube');
    @glutSolidCube := GetModuleSymbol(LibGLUT, 'glutSolidCube');
    @glutWireTorus := GetModuleSymbol(LibGLUT, 'glutWireTorus');
    @glutSolidTorus := GetModuleSymbol(LibGLUT, 'glutSolidTorus');
    @glutWireDodecahedron := GetModuleSymbol(LibGLUT, 'glutWireDodecahedron');
    @glutSolidDodecahedron := GetModuleSymbol(LibGLUT, 'glutSolidDodecahedron');
    @glutWireTeapot := GetModuleSymbol(LibGLUT, 'glutWireTeapot');
    @glutSolidTeapot := GetModuleSymbol(LibGLUT, 'glutSolidTeapot');
    @glutWireOctahedron := GetModuleSymbol(LibGLUT, 'glutWireOctahedron');
    @glutSolidOctahedron := GetModuleSymbol(LibGLUT, 'glutSolidOctahedron');
    @glutWireTetrahedron := GetModuleSymbol(LibGLUT, 'glutWireTetrahedron');
    @glutSolidTetrahedron := GetModuleSymbol(LibGLUT, 'glutSolidTetrahedron');
    @glutWireIcosahedron := GetModuleSymbol(LibGLUT, 'glutWireIcosahedron');
    @glutSolidIcosahedron := GetModuleSymbol(LibGLUT, 'glutSolidIcosahedron');
    @glutVideoResizeGet := GetModuleSymbol(LibGLUT, 'glutVideoResizeGet');
    @glutSetupVideoResizing := GetModuleSymbol(LibGLUT, 'glutSetupVideoResizing');
    @glutStopVideoResizing := GetModuleSymbol(LibGLUT, 'glutStopVideoResizing');
    @glutVideoResize := GetModuleSymbol(LibGLUT, 'glutVideoResize');
    @glutVideoPan := GetModuleSymbol(LibGLUT, 'glutVideoPan');
    @glutReportErrors := GetModuleSymbol(LibGLUT, 'glutReportErrors');
    @glutGameModeString := GetModuleSymbol(LibGLUT, 'glutGameModeString');
    @glutEnterGameMode  := GetModuleSymbol(LibGLUT, 'glutEnterGameMode');
    @glutLeaveGameMode  := GetModuleSymbol(LibGLUT, 'glutLeaveGameMode');
    @glutGameModeGet    := GetModuleSymbol(LibGLUT, 'glutGameModeGet');
  end;
end;

initialization
  LoadGlut( GlutLibName );

finalization
  FreeGlut;

end.
