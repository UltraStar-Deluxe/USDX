unit glu;
{
  $Id: glu.pas,v 1.8 2007/05/20 20:28:31 savage Exp $

  Adaption of the delphi3d.net OpenGL units to FreePascal
  Sebastian Guenther (sg@freepascal.org) in 2002
  These units are free to use
}

(*++ BUILD Version: 0004    // Increment this if a change has global effects

Copyright (c) 1985-95, Microsoft Corporation

Module Name:

    glu.h

Abstract:

    Procedure declarations, constant definitions and macros for the OpenGL
    Utility Library.

--*)

(*
** Copyright 1991-1993, Silicon Graphics, Inc.
** All Rights Reserved.
**
** This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;
** the contents of this file may not be disclosed to third parties, copied or
** duplicated in any form, in whole or in part, without the prior written
** permission of Silicon Graphics, Inc.
**
** RESTRICTED RIGHTS LEGEND:
** Use, duplication or disclosure by the Government is subject to restrictions
** as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data
** and Computer Software clause at DFARS 252.227-7013, and/or in similar or
** successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -
** rights reserved under the Copyright Laws of the United States.
*)

(*
** Return the error string associated with a particular error code.
** This will return 0 for an invalid error code.
**
** The generic function prototype that can be compiled for ANSI or Unicode
** is defined as follows:
**
** LPCTSTR APIENTRY gluErrorStringWIN (GLenum errCode);
*)

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
  $Log: glu.pas,v $
  Revision 1.8  2007/05/20 20:28:31  savage
  Initial Changes to Handle 64 Bits

  Revision 1.7  2006/11/26 16:35:49  savage
  Messed up the last change to GLUtessCombineDataProc, had to reapply it. Thanks Michalis.

  Revision 1.6  2006/11/25 23:38:02  savage
  Changes as proposed by Michalis Kamburelis for better FPC support

  Revision 1.5  2006/11/20 21:20:59  savage
  Updated to work in MacOS X

  Revision 1.4  2005/05/22 18:52:09  savage
  Changes as suggested by Michalis Kamburelis. Thanks again.

  Revision 1.3  2004/10/07 21:01:29  savage
  Fix for FPC

  Revision 1.2  2004/08/14 22:54:30  savage
  Updated so that Library name defines are correctly defined for MacOS X.

  Revision 1.1  2004/03/30 21:53:54  savage
  Moved to it's own folder.

  Revision 1.4  2004/02/20 17:09:55  savage
  Code tidied up in gl, glu and glut, while extensions in glext.pas are now loaded using SDL_GL_GetProcAddress, thus making it more cross-platform compatible, but now more tied to SDL.

  Revision 1.3  2004/02/14 00:23:39  savage
  As UNIX is defined in jedi-sdl.inc this will be used to check linux compatability as well. Units have been changed to reflect this change.

  Revision 1.2  2004/02/14 00:09:19  savage
  Changed uses to now make use of moduleloader.pas rather than dllfuncs.pas

  Revision 1.1  2004/02/05 00:08:19  savage
  Module 1.0 release

  Revision 1.4  2003/06/02 12:32:13  savage
  Modified Sources to avoid warnings with Delphi by moving CVS Logging to the top of the header files. Hopefully CVS Logging still works.

  Revision 1.3  2003/05/29 22:55:00  savage
  Make use of new DLLFunctions

  Revision 1.2  2003/05/27 09:39:53  savage
  Added better Gnu Pascal support.

  Revision 1.1  2003/05/11 13:18:03  savage
  Newest OpenGL Headers For Delphi, Kylix and FPC

  Revision 1.2  2002/10/13 14:36:47  sg
  * Win32 fix: The OS symbol is called "Win32", not "Windows"

  Revision 1.1  2002/10/13 13:57:31  sg
  * Finally, the new units are available: Match the C headers more closely;
    support for OpenGL extensions, and much more. Based on the Delphi units
    by Tom Nuydens of delphi3d.net

}

interface

{$I jedi-sdl.inc}

uses
{$IFDEF __GPC__}
  gpc,
{$ENDIF}
  moduleloader,
  gl;

const
{$IFDEF WINDOWS}
  GLuLibName   = 'glu32.dll';
{$ENDIF}

{$IFDEF UNIX}
{$IFDEF DARWIN}
  GLuLibName   = '/System/Library/Frameworks/OpenGL.framework/Libraries/libGLU.dylib';
{$ELSE}
  GLuLibName   = 'libGLU.so';
{$ENDIF}
{$ENDIF}

type
  TViewPortArray = array[ 0..3 ] of GLint;
  T16dArray = array[ 0..15 ] of GLdouble;
  TCallBack = procedure;
  T3dArray = array[ 0..2 ] of GLdouble;
  T4pArray = array[ 0..3 ] of Pointer;
  T4fArray = array[ 0..3 ] of GLfloat;
{$IFNDEF __GPC__}
  PPointer = ^Pointer;
{$ENDIF}

var
  gluErrorString : function( errCode : GLenum ) : PChar; {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluErrorUnicodeStringEXT : function( errCode : GLenum ) : PWideChar; {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluGetString : function( name : GLenum ) : PChar; {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluOrtho2D   : procedure( left, right, bottom, top : GLdouble ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluPerspective : procedure( fovy, aspect, zNear, zFar : GLdouble ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluPickMatrix : procedure( x, y, width, height : GLdouble; var viewport : TViewPortArray ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluLookAt    : procedure( eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz : GLdouble ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluProject   : function( objx, objy, objz : GLdouble; var modelMatrix, projMatrix : T16dArray; var viewport : TViewPortArray; winx, winy, winz : PGLdouble ) : Integer; {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluUnProject : function( winx, winy, winz : GLdouble; var modelMatrix, projMatrix : T16dArray; var viewport : TViewPortArray; objx, objy, objz : PGLdouble ) : Integer; {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluScaleImage : function( format : GLenum; widthin, heightin : GLint; typein : GLenum; const datain : Pointer; widthout, heightout : GLint; typeout : GLenum; dataout : Pointer ) : Integer; {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluBuild1DMipmaps : function( target : GLenum; components, width : GLint; format, atype : GLenum; const data : Pointer ) : Integer; {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluBuild2DMipmaps : function( target : GLenum; components, width, height : GLint; format, atype : GLenum; const data : Pointer ) : Integer; {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}

type
  GLUnurbs = record
  end; PGLUnurbs = ^GLUnurbs;
  GLUquadric = record
  end; PGLUquadric = ^GLUquadric;
  GLUtesselator = record
  end; PGLUtesselator = ^GLUtesselator;

  // backwards compatibility:
  GLUnurbsObj = GLUnurbs; PGLUnurbsObj = PGLUnurbs;
  GLUquadricObj = GLUquadric; PGLUquadricObj = PGLUquadric;
  GLUtesselatorObj = GLUtesselator; PGLUtesselatorObj = PGLUtesselator;
  GLUtriangulatorObj = GLUtesselator; PGLUtriangulatorObj = PGLUtesselator;

var
  gluNewQuadric : function : PGLUquadric; {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluDeleteQuadric : procedure( state : PGLUquadric ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluQuadricNormals : procedure( quadObject : PGLUquadric; normals : GLenum ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluQuadricTexture : procedure( quadObject : PGLUquadric; textureCoords : GLboolean ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluQuadricOrientation : procedure( quadObject : PGLUquadric; orientation : GLenum ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluQuadricDrawStyle : procedure( quadObject : PGLUquadric; drawStyle : GLenum ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluCylinder  : procedure( qobj : PGLUquadric; baseRadius, topRadius, height : GLdouble; slices, stacks : GLint ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluDisk      : procedure( qobj : PGLUquadric; innerRadius, outerRadius : GLdouble; slices, loops : GLint ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluPartialDisk : procedure( qobj : PGLUquadric; innerRadius, outerRadius : GLdouble; slices, loops : GLint; startAngle, sweepAngle : GLdouble ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluSphere    : procedure( qobj : PGLuquadric; radius : GLdouble; slices, stacks : GLint ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluQuadricCallback : procedure( qobj : PGLUquadric; which : GLenum; fn : TCallBack ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluNewTess   : function : PGLUtesselator; {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluDeleteTess : procedure( tess : PGLUtesselator ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluTessBeginPolygon : procedure( tess : PGLUtesselator; polygon_data : Pointer ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluTessBeginContour : procedure( tess : PGLUtesselator ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluTessVertex : procedure( tess : PGLUtesselator; var coords : T3dArray; data : Pointer ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluTessEndContour : procedure( tess : PGLUtesselator ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluTessEndPolygon : procedure( tess : PGLUtesselator ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluTessProperty : procedure( tess : PGLUtesselator; which : GLenum; value : GLdouble ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluTessNormal : procedure( tess : PGLUtesselator; x, y, z : GLdouble ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluTessCallback : procedure( tess : PGLUtesselator; which : GLenum; fn : TCallBack ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluGetTessProperty : procedure( tess : PGLUtesselator; which : GLenum; value : PGLdouble ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluNewNurbsRenderer : function : PGLUnurbs; {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluDeleteNurbsRenderer : procedure( nobj : PGLUnurbs ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluBeginSurface : procedure( nobj : PGLUnurbs ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluBeginCurve : procedure( nobj : PGLUnurbs ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluEndCurve  : procedure( nobj : PGLUnurbs ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluEndSurface : procedure( nobj : PGLUnurbs ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluBeginTrim : procedure( nobj : PGLUnurbs ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluEndTrim   : procedure( nobj : PGLUnurbs ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluPwlCurve  : procedure( nobj : PGLUnurbs; count : GLint; aarray : PGLfloat; stride : GLint; atype : GLenum ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluNurbsCurve : procedure( nobj : PGLUnurbs; nknots : GLint; knot : PGLfloat; stride : GLint; ctlarray : PGLfloat; order : GLint; atype : GLenum ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluNurbsSurface : procedure( nobj : PGLUnurbs; sknot_count : GLint; sknot : PGLfloat; tknot_count : GLint; tknot : PGLfloat; s_stride, t_stride : GLint; ctlarray : PGLfloat; sorder, torder : GLint; atype : GLenum ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluLoadSamplingMatrices : procedure( nobj : PGLUnurbs; var modelMatrix, projMatrix : T16dArray; var viewport : TViewPortArray ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluNurbsProperty : procedure( nobj : PGLUnurbs; aproperty : GLenum; value : GLfloat ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluGetNurbsProperty : procedure( nobj : PGLUnurbs; aproperty : GLenum; value : PGLfloat ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluNurbsCallback : procedure( nobj : PGLUnurbs; which : GLenum; fn : TCallBack ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}

(**** Callback function prototypes ****)

type
  // gluQuadricCallback
  GLUquadricErrorProc = procedure( p : GLenum ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}

  // gluTessCallback
  GLUtessBeginProc = procedure( p : GLenum ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  GLUtessEdgeFlagProc = procedure( p : GLboolean ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  GLUtessVertexProc = procedure( p : Pointer ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  GLUtessEndProc = procedure; {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  GLUtessErrorProc = procedure( p : GLenum ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  GLUtessCombineProc = procedure( var p1 : T3dArray; p2 : T4pArray; p3 : T4fArray; p4 : PPointer ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  GLUtessBeginDataProc = procedure( p1 : GLenum; p2 : Pointer ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  GLUtessEdgeFlagDataProc = procedure( p1 : GLboolean; p2 : Pointer ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  GLUtessVertexDataProc = procedure( p1, p2 : Pointer ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  GLUtessEndDataProc = procedure( p : Pointer ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  GLUtessErrorDataProc = procedure( p1 : GLenum; p2 : Pointer ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  GLUtessCombineDataProc = procedure( var p1 : T3dArray; var p2 : T4pArray; var p3 : T4fArray;
    p4 : PPointer; p5 : Pointer ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}

  // gluNurbsCallback
  GLUnurbsErrorProc = procedure( p : GLenum ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}


//***           Generic constants               ****/

const
  // Version
  GLU_VERSION_1_1 = 1;
  GLU_VERSION_1_2 = 1;

  // Errors: (return value 0 = no error)
  GLU_INVALID_ENUM = 100900;
  GLU_INVALID_VALUE = 100901;
  GLU_OUT_OF_MEMORY = 100902;
  GLU_INCOMPATIBLE_GL_VERSION = 100903;

  // StringName
  GLU_VERSION  = 100800;
  GLU_EXTENSIONS = 100801;

  // Boolean
  GLU_TRUE     = GL_TRUE;
  GLU_FALSE    = GL_FALSE;


  //***           Quadric constants               ****/

  // QuadricNormal
  GLU_SMOOTH   = 100000;
  GLU_FLAT     = 100001;
  GLU_NONE     = 100002;

  // QuadricDrawStyle
  GLU_POINT    = 100010;
  GLU_LINE     = 100011;
  GLU_FILL     = 100012;
  GLU_SILHOUETTE = 100013;

  // QuadricOrientation
  GLU_OUTSIDE  = 100020;
  GLU_INSIDE   = 100021;

  // Callback types:
  //      GLU_ERROR       = 100103;


  //***           Tesselation constants           ****/

  GLU_TESS_MAX_COORD = 1.0E150;

  // TessProperty
  GLU_TESS_WINDING_RULE = 100140;
  GLU_TESS_BOUNDARY_ONLY = 100141;
  GLU_TESS_TOLERANCE = 100142;

  // TessWinding
  GLU_TESS_WINDING_ODD = 100130;
  GLU_TESS_WINDING_NONZERO = 100131;
  GLU_TESS_WINDING_POSITIVE = 100132;
  GLU_TESS_WINDING_NEGATIVE = 100133;
  GLU_TESS_WINDING_ABS_GEQ_TWO = 100134;

  // TessCallback
  GLU_TESS_BEGIN = 100100; // void (CALLBACK*)(GLenum    type)
  GLU_TESS_VERTEX = 100101; // void (CALLBACK*)(void      *data)
  GLU_TESS_END = 100102; // void (CALLBACK*)(void)
  GLU_TESS_ERROR = 100103; // void (CALLBACK*)(GLenum    errno)
  GLU_TESS_EDGE_FLAG = 100104; // void (CALLBACK*)(GLboolean boundaryEdge)
  GLU_TESS_COMBINE = 100105; { void (CALLBACK*)(GLdouble  coords[3],
                                                            void      *data[4],
                                                            GLfloat   weight[4],
                                                            void      **dataOut) }
  GLU_TESS_BEGIN_DATA = 100106; { void (CALLBACK*)(GLenum    type,
                                                            void      *polygon_data) }
  GLU_TESS_VERTEX_DATA = 100107; { void (CALLBACK*)(void      *data,
                                                            void      *polygon_data) }
  GLU_TESS_END_DATA = 100108; // void (CALLBACK*)(void      *polygon_data)
  GLU_TESS_ERROR_DATA = 100109; { void (CALLBACK*)(GLenum    errno,
                                                            void      *polygon_data) }
  GLU_TESS_EDGE_FLAG_DATA = 100110; { void (CALLBACK*)(GLboolean boundaryEdge,
                                                            void      *polygon_data) }
  GLU_TESS_COMBINE_DATA = 100111; { void (CALLBACK*)(GLdouble  coords[3],
                                                            void      *data[4],
                                                            GLfloat   weight[4],
                                                            void      **dataOut,
                                                            void      *polygon_data) }

  // TessError
  GLU_TESS_ERROR1 = 100151;
  GLU_TESS_ERROR2 = 100152;
  GLU_TESS_ERROR3 = 100153;
  GLU_TESS_ERROR4 = 100154;
  GLU_TESS_ERROR5 = 100155;
  GLU_TESS_ERROR6 = 100156;
  GLU_TESS_ERROR7 = 100157;
  GLU_TESS_ERROR8 = 100158;

  GLU_TESS_MISSING_BEGIN_POLYGON = GLU_TESS_ERROR1;
  GLU_TESS_MISSING_BEGIN_CONTOUR = GLU_TESS_ERROR2;
  GLU_TESS_MISSING_END_POLYGON = GLU_TESS_ERROR3;
  GLU_TESS_MISSING_END_CONTOUR = GLU_TESS_ERROR4;
  GLU_TESS_COORD_TOO_LARGE = GLU_TESS_ERROR5;
  GLU_TESS_NEED_COMBINE_CALLBACK = GLU_TESS_ERROR6;

  //***           NURBS constants                 ****/

  // NurbsProperty
  GLU_AUTO_LOAD_MATRIX = 100200;
  GLU_CULLING  = 100201;
  GLU_SAMPLING_TOLERANCE = 100203;
  GLU_DISPLAY_MODE = 100204;
  GLU_PARAMETRIC_TOLERANCE = 100202;
  GLU_SAMPLING_METHOD = 100205;
  GLU_U_STEP   = 100206;
  GLU_V_STEP   = 100207;

  // NurbsSampling
  GLU_PATH_LENGTH = 100215;
  GLU_PARAMETRIC_ERROR = 100216;
  GLU_DOMAIN_DISTANCE = 100217;


  // NurbsTrim
  GLU_MAP1_TRIM_2 = 100210;
  GLU_MAP1_TRIM_3 = 100211;

  // NurbsDisplay
  //      GLU_FILL                = 100012;
  GLU_OUTLINE_POLYGON = 100240;
  GLU_OUTLINE_PATCH = 100241;

  // NurbsCallback
  //      GLU_ERROR               = 100103;

  // NurbsErrors
  GLU_NURBS_ERROR1 = 100251;
  GLU_NURBS_ERROR2 = 100252;
  GLU_NURBS_ERROR3 = 100253;
  GLU_NURBS_ERROR4 = 100254;
  GLU_NURBS_ERROR5 = 100255;
  GLU_NURBS_ERROR6 = 100256;
  GLU_NURBS_ERROR7 = 100257;
  GLU_NURBS_ERROR8 = 100258;
  GLU_NURBS_ERROR9 = 100259;
  GLU_NURBS_ERROR10 = 100260;
  GLU_NURBS_ERROR11 = 100261;
  GLU_NURBS_ERROR12 = 100262;
  GLU_NURBS_ERROR13 = 100263;
  GLU_NURBS_ERROR14 = 100264;
  GLU_NURBS_ERROR15 = 100265;
  GLU_NURBS_ERROR16 = 100266;
  GLU_NURBS_ERROR17 = 100267;
  GLU_NURBS_ERROR18 = 100268;
  GLU_NURBS_ERROR19 = 100269;
  GLU_NURBS_ERROR20 = 100270;
  GLU_NURBS_ERROR21 = 100271;
  GLU_NURBS_ERROR22 = 100272;
  GLU_NURBS_ERROR23 = 100273;
  GLU_NURBS_ERROR24 = 100274;
  GLU_NURBS_ERROR25 = 100275;
  GLU_NURBS_ERROR26 = 100276;
  GLU_NURBS_ERROR27 = 100277;
  GLU_NURBS_ERROR28 = 100278;
  GLU_NURBS_ERROR29 = 100279;
  GLU_NURBS_ERROR30 = 100280;
  GLU_NURBS_ERROR31 = 100281;
  GLU_NURBS_ERROR32 = 100282;
  GLU_NURBS_ERROR33 = 100283;
  GLU_NURBS_ERROR34 = 100284;
  GLU_NURBS_ERROR35 = 100285;
  GLU_NURBS_ERROR36 = 100286;
  GLU_NURBS_ERROR37 = 100287;

//***           Backwards compatibility for old tesselator           ****/

var
  gluBeginPolygon : procedure( tess : PGLUtesselator ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluNextContour : procedure( tess : PGLUtesselator; atype : GLenum ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}
  gluEndPolygon : procedure( tess : PGLUtesselator ); {$IFDEF WINDOWS}stdcall; {$ELSE}cdecl; {$ENDIF}

const
  // Contours types -- obsolete!
  GLU_CW       = 100120;
  GLU_CCW      = 100121;
  GLU_INTERIOR = 100122;
  GLU_EXTERIOR = 100123;
  GLU_UNKNOWN  = 100124;

  // Names without "TESS_" prefix
  GLU_BEGIN    = GLU_TESS_BEGIN;
  GLU_VERTEX   = GLU_TESS_VERTEX;
  GLU_END      = GLU_TESS_END;
  GLU_ERROR    = GLU_TESS_ERROR;
  GLU_EDGE_FLAG = GLU_TESS_EDGE_FLAG;

procedure LoadGLu( const dll : PChar );
procedure FreeGLu;

implementation

var
  LibGlu       : TModuleHandle;

procedure FreeGLu;
begin

  @gluErrorString := nil;
  @gluErrorUnicodeStringEXT := nil;
  @gluGetString := nil;
  @gluOrtho2D := nil;
  @gluPerspective := nil;
  @gluPickMatrix := nil;
  @gluLookAt := nil;
  @gluProject := nil;
  @gluUnProject := nil;
  @gluScaleImage := nil;
  @gluBuild1DMipmaps := nil;
  @gluBuild2DMipmaps := nil;
  @gluNewQuadric := nil;
  @gluDeleteQuadric := nil;
  @gluQuadricNormals := nil;
  @gluQuadricTexture := nil;
  @gluQuadricOrientation := nil;
  @gluQuadricDrawStyle := nil;
  @gluCylinder := nil;
  @gluDisk := nil;
  @gluPartialDisk := nil;
  @gluSphere := nil;
  @gluQuadricCallback := nil;
  @gluNewTess := nil;
  @gluDeleteTess := nil;
  @gluTessBeginPolygon := nil;
  @gluTessBeginContour := nil;
  @gluTessVertex := nil;
  @gluTessEndContour := nil;
  @gluTessEndPolygon := nil;
  @gluTessProperty := nil;
  @gluTessNormal := nil;
  @gluTessCallback := nil;
  @gluGetTessProperty := nil;
  @gluNewNurbsRenderer := nil;
  @gluDeleteNurbsRenderer := nil;
  @gluBeginSurface := nil;
  @gluBeginCurve := nil;
  @gluEndCurve := nil;
  @gluEndSurface := nil;
  @gluBeginTrim := nil;
  @gluEndTrim := nil;
  @gluPwlCurve := nil;
  @gluNurbsCurve := nil;
  @gluNurbsSurface := nil;
  @gluLoadSamplingMatrices := nil;
  @gluNurbsProperty := nil;
  @gluGetNurbsProperty := nil;
  @gluNurbsCallback := nil;
  @gluBeginPolygon := nil;
  @gluNextContour := nil;
  @gluEndPolygon := nil;

  UnLoadModule( LibGlu );

end;

procedure LoadGLu( const dll : PChar );
begin

  FreeGLu;

  if LoadModule( LibGlu, dll ) then
  begin
    @gluErrorString := GetModuleSymbol( LibGlu, 'gluErrorString' );
    @gluErrorUnicodeStringEXT := GetModuleSymbol( LibGlu, 'gluErrorUnicodeStringEXT' );
    @gluGetString := GetModuleSymbol( LibGlu, 'gluGetString' );
    @gluOrtho2D := GetModuleSymbol( LibGlu, 'gluOrtho2D' );
    @gluPerspective := GetModuleSymbol( LibGlu, 'gluPerspective' );
    @gluPickMatrix := GetModuleSymbol( LibGlu, 'gluPickMatrix' );
    @gluLookAt := GetModuleSymbol( LibGlu, 'gluLookAt' );
    @gluProject := GetModuleSymbol( LibGlu, 'gluProject' );
    @gluUnProject := GetModuleSymbol( LibGlu, 'gluUnProject' );
    @gluScaleImage := GetModuleSymbol( LibGlu, 'gluScaleImage' );
    @gluBuild1DMipmaps := GetModuleSymbol( LibGlu, 'gluBuild1DMipmaps' );
    @gluBuild2DMipmaps := GetModuleSymbol( LibGlu, 'gluBuild2DMipmaps' );
    @gluNewQuadric := GetModuleSymbol( LibGlu, 'gluNewQuadric' );
    @gluDeleteQuadric := GetModuleSymbol( LibGlu, 'gluDeleteQuadric' );
    @gluQuadricNormals := GetModuleSymbol( LibGlu, 'gluQuadricNormals' );
    @gluQuadricTexture := GetModuleSymbol( LibGlu, 'gluQuadricTexture' );
    @gluQuadricOrientation := GetModuleSymbol( LibGlu, 'gluQuadricOrientation' );
    @gluQuadricDrawStyle := GetModuleSymbol( LibGlu, 'gluQuadricDrawStyle' );
    @gluCylinder := GetModuleSymbol( LibGlu, 'gluCylinder' );
    @gluDisk := GetModuleSymbol( LibGlu, 'gluDisk' );
    @gluPartialDisk := GetModuleSymbol( LibGlu, 'gluPartialDisk' );
    @gluSphere := GetModuleSymbol( LibGlu, 'gluSphere' );
    @gluQuadricCallback := GetModuleSymbol( LibGlu, 'gluQuadricCallback' );
    @gluNewTess := GetModuleSymbol( LibGlu, 'gluNewTess' );
    @gluDeleteTess := GetModuleSymbol( LibGlu, 'gluDeleteTess' );
    @gluTessBeginPolygon := GetModuleSymbol( LibGlu, 'gluTessBeginPolygon' );
    @gluTessBeginContour := GetModuleSymbol( LibGlu, 'gluTessBeginContour' );
    @gluTessVertex := GetModuleSymbol( LibGlu, 'gluTessVertex' );
    @gluTessEndContour := GetModuleSymbol( LibGlu, 'gluTessEndContour' );
    @gluTessEndPolygon := GetModuleSymbol( LibGlu, 'gluTessEndPolygon' );
    @gluTessProperty := GetModuleSymbol( LibGlu, 'gluTessProperty' );
    @gluTessNormal := GetModuleSymbol( LibGlu, 'gluTessNormal' );
    @gluTessCallback := GetModuleSymbol( LibGlu, 'gluTessCallback' );
    @gluGetTessProperty := GetModuleSymbol( LibGlu, 'gluGetTessProperty' );
    @gluNewNurbsRenderer := GetModuleSymbol( LibGlu, 'gluNewNurbsRenderer' );
    @gluDeleteNurbsRenderer := GetModuleSymbol( LibGlu, 'gluDeleteNurbsRenderer' );
    @gluBeginSurface := GetModuleSymbol( LibGlu, 'gluBeginSurface' );
    @gluBeginCurve := GetModuleSymbol( LibGlu, 'gluBeginCurve' );
    @gluEndCurve := GetModuleSymbol( LibGlu, 'gluEndCurve' );
    @gluEndSurface := GetModuleSymbol( LibGlu, 'gluEndSurface' );
    @gluBeginTrim := GetModuleSymbol( LibGlu, 'gluBeginTrim' );
    @gluEndTrim := GetModuleSymbol( LibGlu, 'gluEndTrim' );
    @gluPwlCurve := GetModuleSymbol( LibGlu, 'gluPwlCurve' );
    @gluNurbsCurve := GetModuleSymbol( LibGlu, 'gluNurbsCurve' );
    @gluNurbsSurface := GetModuleSymbol( LibGlu, 'gluNurbsSurface' );
    @gluLoadSamplingMatrices := GetModuleSymbol( LibGlu, 'gluLoadSamplingMatrices' );
    @gluNurbsProperty := GetModuleSymbol( LibGlu, 'gluNurbsProperty' );
    @gluGetNurbsProperty := GetModuleSymbol( LibGlu, 'gluGetNurbsProperty' );
    @gluNurbsCallback := GetModuleSymbol( LibGlu, 'gluNurbsCallback' );

    @gluBeginPolygon := GetModuleSymbol( LibGlu, 'gluBeginPolygon' );
    @gluNextContour := GetModuleSymbol( LibGlu, 'gluNextContour' );
    @gluEndPolygon := GetModuleSymbol( LibGlu, 'gluEndPolygon' );
  end;
end;

initialization

  LoadGLu( GLuLibName );

finalization

  FreeGLu;

end.

