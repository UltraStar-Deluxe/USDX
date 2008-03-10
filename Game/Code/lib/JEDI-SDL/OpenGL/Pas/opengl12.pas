unit opengl12;
{
  $Id: opengl12.pas,v 1.2 2004/04/05 09:59:46 savage Exp $
  
}
{******************************************************************************}
{                                                       	                     }
{       Borland Delphi Runtime Library                  		                   }
{       OpenGL interface unit                                                  }
{                                                       	                     }
{                                                       	                     }
{ This is an interface unit for the use of OpenGL from within Delphi and Kylix.}
{ It contains the translations of gl.h, glu.h, glx.h as well as context        }
{ and extension management functions.                                          }
{                                                                              }
{ The original Pascal code is: OpenGL12.pas 	                                 }
{ The initial developer of the Pascal code is Mike Lischke                     }
{                                                                              }
{ 									                                                           }
{ Portions created by Microsoft are 					                                 }
{ Copyright (C) 1995-2001 Microsoft Corporation. 			                         }
{ All Rights Reserved. 							                                           }
{ 									                                                           }
{ Portions created by Silicon Graphics Incorporated are 		                   }
{ Copyright (C) 1995-2001 Silicon Graphics Incorporated 		                   }
{ All Rights Reserved. 							                                           }
{ 									                                                           }
{ Portions created by NVidia are                        		                   }
{ Copyright (C) 1995-2001 NVidia 		                                           }
{ All Rights Reserved. 							                                           }
{ 									                                                           }
{ Portions created by Brian Paul                       		                     }
{ Copyright (C) 1995-2001 Brian Paul 		                                       }
{ All Rights Reserved. 							                                           }
{ 									                                                           }
{ 									                                                           }
{ The original file is: gl.h                                                   }
{ The original file is: glut.h                                                 }
{ The original file is: glx.h                                                  }
{ The original file is: glx.h                                                  }
{ 									                                                           }
{ Portions created by Mike Lischke are    				                             }
{ Copyright (C) 2001 Mike Lischke. 					                                   }
{ 									                                                           }
{ Portions created by John O'Harrow are    				                             }
{ Copyright (C) 2001 John O'Harrow. 					                                 } 
{                                                                              }
{ Portions created by Eric Grange are    				                               }
{ Copyright (C) 2001 Eric Grange. 					                                   }
{                                                                              }
{ Portions created by Olivier Chatelain    				                             }
{ Copyright (C) 2001 Olivier Chatelain. 				                               }
{                                                                              }
{ Portions created by Tom Nuydens            				                           }
{ Copyright (C) 2001 Tom Nuydens.       				                               }
{                                                                              }
{ Portions created by Matthias Thoma are    				                           }
{ Copyright (C) 2001 Matthias Thoma. 					                                 }
{                                                                              }
{ Portions created by Sven Bobrowski are    				                           }
{ Copyright (C) 2001 Sven Bobrowski 					                                 }
{                                                                              }
{                                                                              }
{       Obtained through:                               		                   }
{ 									                                                           }
{       Joint Endeavour of Delphi Innovators (Project JEDI)                    }
{									                                                             }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{									                                                             }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html 	                                   }
{ 									                                                           }
{ Software distributed under the License is distributed on an 	               }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License. 				                           }
{ 									                                                           }
{******************************************************************************}

//----------------------------------------------------------------------------------------------------------------------
//
//  This is an interface unit for the use of OpenGL from within Delphi and contains
//  the translations of gl.h, glu.h as well as some support functions.
//  OpenGL12.pas contains bug fixes and enhancements of Delphi's and other translations
//  as well as support for extensions.
//
//  NOTE: In order to fully support multi thread rendering it is necessary to hold all
//        extension address in threadvars. For single threaded applications this would be an
//        unnecessary time penalty, however. Hence there is a compiler switch to
//        allow single threaded OpenGL application to use vars while multi threaded applications
//        will use threadvars. By default the switch MULTITHREADOPENGL (see compiler switch under der interface keyword)
//        is not active and must explicitly enabled to take effect.
//
//----------------------------------------------------------------------------------------------------------------------
//
// function InitOpenGL: Boolean; 
//   Needed to load the OpenGL DLLs and all addresses of the standard functions.
//   In case OpenGL is already initialized this function does nothing. No error
//   is raised, if something goes wrong, but you need to inspect the result in order
//   to know if all went okay.
//   Result: True if successful or already loaded, False otherwise.
//
// function InitOpenGLFromLibrary(GL_Name, GLU_Name: String): Boolean; 
//   Same as InitOpenGL, but you can specify specific DLLs. Useful if you want to
//   use different DLLs than the default ones. This function closes previously
//   loaded DLLs before it tries to open the new libraries.
//   Result: True if successful, False otherwise.
//
// procedure CloseOpenGL; 
//   Unloads the OpenGL DLLs and sets all function addresses to nil, including
//   extensions. You can load and unload the DLLs as often as you like.
//
// procedure ClearExtensions; 
//   Sets all extension routines to nil. This is needed when you change the Pixelformat
//   of your OpenGL window, since the availability of these routines changes from
//   PixelFormat to Pixelformat (and also between various vendors).
//
// function CreateRenderingContext(DC: HDC; Options: TRCOptions; ColorBits, StencilBits, AccumBits, AuxBuffers: Integer; 
//   Layer: Integer; var Palette: HPALETTE): HGLRC; 
//   Sets up a pixel format and creates a new rendering context depending of the
//   given parameters:
//     DC          - the device context for which the rc is to be created
//     Options     - options for the context, which the application would like to have
//                   (it is not guaranteed they will be available)
//     ColorBits   - the color depth of the device context (Note: Because of the internal DC handling of the VCL you
//                   should avoid using GetDeviceCaps for memory DCs which are members of a TBitmap class.
//                   Translate the Pixelformat member instead!)
//     StencilBits - requested size of the stencil buffer
//     AccumBits   - requested size of the accumulation buffer
//     AuxBuffers  - requested number of auxiliary buffers
//     Layer       - ID for the layer for which the RC will be created (-1..-15 for underlay planes, 0 for main plane,
//                   1..15 for overlay planes)
//                   Note: The layer handling is not yet complete as there is very few information
//                   available and (until now) no OpenGL implementation with layer support on the low budget market.
//                   Hence use 0 (for the main plane) as layer ID.
//     Palette     - Palette Handle created within function (need to use DeleteObject(Palette) to free this if <> 0)
//   Result: the newly created context or 0 if setup failed
//
// procedure ActivateRenderingContext(DC: HDC; RC: HGLRC); 
//   Makes RC in DC 'current' (wglMakeCurrent(..)) and loads all extension addresses
//   and flags if necessary.
//
// procedure DeactivateRenderingContext; 
//   Counterpart to ActivateRenderingContext.
//
// procedure DestroyRenderingContext(RC: HGLRC); 
//   RC will be destroyed and must be recreated if you want to use it again.
//
// procedure ReadExtensions; 
//   Determines which extensions for the current rendering context are available and
//   loads their addresses. This procedure is called from ActivateRenderingContext
//   if a new pixel format is used, but you can safely call it from where you want
//   to actualize those values (under the condition that a rendering context MUST be
//   active).
//
// procedure ReadImplementationProperties; 
//   Determines other properties of the OpenGL DLL (version, availability of extensions).
//   Again, a valid rendering context must be active.
//
// function HasActiveContext: Boolean; 
//   Determines whether the calling thread has currently an active rendering context.
//----------------------------------------------------------------------------------------------------------------------
//
// This translation is based on different sources:
//
// - first translation from Artemis Alliance Inc.
// - previous versions from Mike Lischke
// - Alexander Staubo
// - Borland OpenGL.pas (from Delphi 3)
// - Microsoft and SGI OpenGL header files
// - www.opengl.org, www.sgi.com/OpenGL
// - nVidia extension reference as of December 1999
// - nVidia extension reference as of January 2001
// - vertex_array_range sample by Tom Nuydens at Delphi3D
// - minor bug fixes and greatly extended by John O'Harrow (john@elmcrest.demon.co.uk)
// - initial context activation balancing by Eric Grange (egrange@infonie.fr)
// - additional nVidia extensions by Olivier Chatelain (Olivier.Chatelain@xitact.com)
//
//  Contact: public@lischke-online.de, www.lischke-online.de
//
//  Version: 1.2.11
//----------------------------------------------------------------------------------------------------------------------
//
// 12-Feb-2002 dml :
//   - Further modifications to allow unit to compile under Free Pascal
//     as suggested by "QuePasha Pepe" <mrkroket@hotmail.com>
//
// 25-OCT-2001 dre :
//   - Made modifications to allow unit to compile under Free Pascal
//   - added tMaxLogPalette declaration to Free Pascal
//   - included fix to ReadExtensions
//   - Added Set8088CW procedure
//
// 13-SEP-2001 ml:
//   - added PWGLSwap etc. declarations for Delphi 3
// 18-AUG-2001 ml:
//   - multi thread support for function addresses (extensions)
// 28-JUL-2001 ml:
//   - included original type names (+ $EXTERNALSYM directives)
// 10-JUL-2001 ml:
//   - TGLubyte changed to UCHAR
// 05-JUL-2001 ml:
//   - own exception type for OpenGL
//   - TGLboolean is now of type BYTEBOOL
// 05-MAY-2001 ml:
//   - correct tracking of RC creation and release as well as multithreaded RC activation
//   - compatibility routines for users of other OpenGL unit variants
//   - improved rendering context creation
//   - bug fixes
// 01-MAY-2001 ml:
//   - added more nVidia extensions
//----------------------------------------------------------------------------------------------------------------------
{   April   03 2003 - DL : Added jedi-sdl.inc include file to support more     }
{                          Pascal compilers. Initial support is now included   }
{                          for GnuPascal, VirtualPascal, TMT and obviously     }
{                          continue support for Delphi Kylix and FreePascal.   }
{                                                                              }
{   May     03 2003 - DL : under instruction from David Mears AKA              }
{                          Jason Siletto, I have added FPC Linux support.      }
{                                                                              }
{******************************************************************************}
{
  $Log: opengl12.pas,v $
  Revision 1.2  2004/04/05 09:59:46  savage
  Changes for FreePacal as suggested by Marco

  Revision 1.1  2004/03/30 21:53:55  savage
  Moved to it's own folder.

  Revision 1.6  2004/02/20 17:26:19  savage
  Extensions are now loaded using SDL_GL_GetProcAddress, thus making it more cross-platform compatible, but now more tied to SDL.

  Revision 1.5  2004/02/15 22:48:36  savage
  More FPC and FreeBSD support changes.

  Revision 1.4  2004/02/14 22:36:29  savage
  Fixed inconsistencies of using LoadLibrary and LoadModule.
  Now all units make use of LoadModule rather than LoadLibrary and other dynamic proc procedures.

  Revision 1.3  2004/02/14 00:23:39  savage
  As UNIX is defined in jedi-sdl.inc this will be used to check linux compatability as well. Units have been changed to reflect this change.

  Revision 1.2  2004/02/14 00:09:19  savage
  Changed uses to now make use of moduleloader.pas rather than dllfuncs.pas

  Revision 1.1  2004/02/05 00:08:20  savage
  Module 1.0 release

  
}

{$I jedi-sdl.inc}

interface

{.$define MULTITHREADOPENGL}

uses
{$IFDEF __GPC__}
  gpc;
{$ENDIF}

{$IFDEF WIN32}
  {$IFNDEF __GPC__}
  Windows;
  {$ENDIF}
{$ENDIF}

{$IFDEF Unix}
  {$IFDEF FPC}
    {$IFDEF Ver1_0}
    linux,
    {$ELSE}
    baseunix,
    unix,
    {$ENDIF}
    x,
    xlib,
    xutil,
    dl;
  {$ELSE}
  Types,
  Libc,
  Xlib;
  {$ENDIF}
{$ENDIF}

type
  TRCOptions = set of ( opDoubleBuffered, opGDI, opStereo );

  {$EXTERNALSYM GLenum}
  GLenum      = Cardinal;
  TGLenum     = Cardinal; 
  PGLenum     = ^TGLenum;

  {$EXTERNALSYM GLboolean}
  GLboolean   = BYTEBOOL;
  TGLboolean  = BYTEBOOL;
  PGLboolean  = ^TGLboolean;

  {$EXTERNALSYM GLbitfield}
  GLbitfield  = Cardinal;
  TGLbitfield = Cardinal;
  PGLbitfield = ^TGLbitfield;

  {$EXTERNALSYM GLbyte}
  GLbyte      = ShortInt;
  TGLbyte     = ShortInt;
  PGLbyte     = ^TGLbyte;

  {$EXTERNALSYM GLshort}
  GLshort     = SmallInt;
  TGLshort    = SmallInt;
  PGLshort    = ^TGLshort;

  {$EXTERNALSYM GLint}
  GLint       = Integer;
  TGLint      = Integer;
  PGLint      = ^TGLint;

  {$EXTERNALSYM GLsizei}
  GLsizei     = Integer;
  TGLsizei    = Integer;
  PGLsizei    = ^TGLsizei;

  {$EXTERNALSYM GLubyte}
  UCHAR       = Byte;
  GLubyte     = UCHAR;
  TGLubyte    = UCHAR;
  PGLubyte    = ^TGLubyte;

  {$EXTERNALSYM GLushort}
  GLushort    = Word;
  TGLushort   = Word;
  PGLushort   = ^TGLushort;

  {$EXTERNALSYM GLuint}
  GLuint      = Cardinal;
  TGLuint     = Cardinal;
  PGLuint     = ^TGLuint;

  {$EXTERNALSYM GLfloat}
  GLfloat     = Single;
  TGLfloat    = Single;
  PGLfloat    = ^TGLfloat;

  {$EXTERNALSYM GLclampf}
  GLclampf    = Single;
  TGLclampf   = Single;
  PGLclampf   = ^TGLclampf;

  {$EXTERNALSYM GLdouble}
  GLdouble    = Double;
  TGLdouble   = Double;
  PGLdouble   = ^TGLdouble;

  {$EXTERNALSYM GLclampd}
  GLclampd    = Double;
  TGLclampd   = Double;
  PGLclampd   = ^TGLclampd; 

  TVector3d = array[0..2] of GLdouble;
  
  TVector4i = array[0..3] of GLint;
  TVector4f = array[0..3] of GLfloat;
  TVector4p = array[0..3] of Pointer;

  TMatrix4f = array[0..3, 0..3] of GLfloat;
  TMatrix4d = array[0..3, 0..3] of GLdouble;
  
  PPointer = ^Pointer;

{$ifndef FPC} 
  {$ifdef MULTITHREADOPENGL}
  threadvar
  {$else}
  var
  {$endif} 
{$else} 
var 
{$endif} 
  GL_VERSION_1_0,
  GL_VERSION_1_1,
  GL_VERSION_1_2,
  GLU_VERSION_1_1,
  GLU_VERSION_1_2,
  GLU_VERSION_1_3: Boolean; 

  // Extensions (gl)
  GL_3DFX_multisample,
  GL_3DFX_tbuffer,
  GL_3DFX_texture_compression_FXT1,

  GL_APPLE_specular_vector,
  GL_APPLE_transform_hint,

  GL_ARB_imaging,
  GL_ARB_multisample,
  GL_ARB_multitexture,
  GL_ARB_texture_compression,
  GL_ARB_texture_cube_map,
  GL_ARB_transpose_matrix,
  GL_ARB_vertex_blend,

  GL_EXT_422_pixels,
  GL_EXT_abgr,
  GL_EXT_bgra,
  GL_EXT_blend_color,
  GL_EXT_blend_func_separate,
  GL_EXT_blend_logic_op,
  GL_EXT_blend_minmax,
  GL_EXT_blend_subtract,
  GL_EXT_clip_volume_hint,
  GL_EXT_cmyka,
  GL_EXT_color_subtable,
  GL_EXT_compiled_vertex_array,
  GL_EXT_convolution,
  GL_EXT_coordinate_frame,
  GL_EXT_copy_texture,
  GL_EXT_cull_vertex,
  GL_EXT_draw_range_elements,
  GL_EXT_fog_coord,
  GL_EXT_histogram,
  GL_EXT_index_array_formats,
  GL_EXT_index_func,
  GL_EXT_index_material,
  GL_EXT_index_texture,
  GL_EXT_light_max_exponent,
  GL_EXT_light_texture,
  GL_EXT_misc_attribute,
  GL_EXT_multi_draw_arrays,
  GL_EXT_multisample,
  GL_EXT_packed_pixels,
  GL_EXT_paletted_texture,
  GL_EXT_pixel_transform,
  GL_EXT_point_parameters,
  GL_EXT_polygon_offset,
  GL_EXT_rescale_normal,
  GL_EXT_scene_marker,
  GL_EXT_secondary_color,
  GL_EXT_separate_specular_color,
  GL_EXT_shared_texture_palette,
  GL_EXT_stencil_wrap,
  GL_EXT_subtexture,
  GL_EXT_texture_color_table,
  GL_EXT_texture_compression_s3tc,
  GL_EXT_texture_cube_map,
  GL_EXT_texture_edge_clamp,
  GL_EXT_texture_env_add,
  GL_EXT_texture_env_combine,
  GL_EXT_texture_filter_anisotropic,
  GL_EXT_texture_lod_bias,
  GL_EXT_texture_object,
  GL_EXT_texture_perturb_normal,
  GL_EXT_texture3D,
  GL_EXT_vertex_array,
  GL_EXT_vertex_weighting,

  GL_FfdMaskSGIX,
  GL_HP_convolution_border_modes,
  GL_HP_image_transform,
  GL_HP_occlusion_test,
  GL_HP_texture_lighting,

  GL_IBM_cull_vertex,
  GL_IBM_multimode_draw_arrays,
  GL_IBM_rasterpos_clip,
  GL_IBM_vertex_array_lists,

  GL_INGR_color_clamp,
  GL_INGR_interlace_read,

  GL_INTEL_parallel_arrays,

  GL_KTX_buffer_region,

  GL_MESA_resize_buffers,
  GL_MESA_window_pos,

  GL_NV_blend_square,
  GL_NV_fog_distance,
  GL_NV_light_max_exponent,
  GL_NV_register_combiners,
  GL_NV_texgen_emboss,
  GL_NV_texgen_reflection,
  GL_NV_texture_env_combine4,
  GL_NV_vertex_array_range,
  GL_NV_vertex_program,

  GL_PGI_misc_hints,
  GL_PGI_vertex_hints,

  GL_REND_screen_coordinates,

  GL_SGI_color_matrix,
  GL_SGI_color_table,
  GL_SGI_depth_pass_instrument,

  GL_SGIS_detail_texture,
  GL_SGIS_fog_function,
  GL_SGIS_generate_mipmap,
  GL_SGIS_multisample,
  GL_SGIS_multitexture,
  GL_SGIS_pixel_texture,
  GL_SGIS_point_line_texgen,
  GL_SGIS_point_parameters,
  GL_SGIS_sharpen_texture,
  GL_SGIS_texture_border_clamp,
  GL_SGIS_texture_color_mask,
  GL_SGIS_texture_edge_clamp,
  GL_SGIS_texture_filter4,
  GL_SGIS_texture_lod,
  GL_SGIS_texture_select,
  GL_SGIS_texture4D,

  GL_SGIX_async,
  GL_SGIX_async_histogram,
  GL_SGIX_async_pixel,
  GL_SGIX_blend_alpha_minmax,
  GL_SGIX_calligraphic_fragment,
  GL_SGIX_clipmap,
  GL_SGIX_convolution_accuracy,
  GL_SGIX_depth_texture,
  GL_SGIX_flush_raster,
  GL_SGIX_fog_offset,
  GL_SGIX_fog_scale,
  GL_SGIX_fragment_lighting,
  GL_SGIX_framezoom,
  GL_SGIX_igloo_interface,
  GL_SGIX_instruments,
  GL_SGIX_interlace,
  GL_SGIX_ir_instrument1,
  GL_SGIX_list_priority,
  GL_SGIX_pixel_texture,
  GL_SGIX_pixel_tiles,
  GL_SGIX_polynomial_ffd,
  GL_SGIX_reference_plane,
  GL_SGIX_resample,
  GL_SGIX_shadow,
  GL_SGIX_shadow_ambient,
  GL_SGIX_sprite,
  GL_SGIX_subsample,
  GL_SGIX_tag_sample_buffer,
  GL_SGIX_texture_add_env,
  GL_SGIX_texture_lod_bias,
  GL_SGIX_texture_multi_buffer,
  GL_SGIX_texture_scale_bias,
  GL_SGIX_vertex_preclip,
  GL_SGIX_ycrcb,
  GL_SGIX_ycrcba,

  GL_SUN_convolution_border_modes,
  GL_SUN_global_alpha,
  GL_SUN_triangle_list,
  GL_SUN_vertex,

  GL_SUNX_constant_data,

  GL_WIN_phong_shading,
  GL_WIN_specular_fog,
  GL_WIN_swap_hint,

  WGL_EXT_swap_control,
  WGL_ARB_extensions_string,
  WGL_ARB_pixel_format,

  // Extensions (glu)
  GLU_EXT_Texture,
  GLU_EXT_object_space_tess,
  GLU_EXT_nurbs_tessellator: Boolean; 

const
  // ********** GL generic constants **********

  // errors
  GL_NO_ERROR                                       = 0;
  {$EXTERNALSYM GL_NO_ERROR}
  GL_INVALID_ENUM                                   = $0500;
  {$EXTERNALSYM GL_INVALID_ENUM}
  GL_INVALID_VALUE                                  = $0501;
  {$EXTERNALSYM GL_INVALID_VALUE}
  GL_INVALID_OPERATION                              = $0502;
  {$EXTERNALSYM GL_INVALID_OPERATION}
  GL_STACK_OVERFLOW                                 = $0503;
  {$EXTERNALSYM GL_STACK_OVERFLOW}
  GL_STACK_UNDERFLOW                                = $0504;
  {$EXTERNALSYM GL_STACK_UNDERFLOW}
  GL_OUT_OF_MEMORY                                  = $0505;
  {$EXTERNALSYM GL_STACK_UNDERFLOW}

  // attribute bits
  GL_CURRENT_BIT                                    = $00000001;
  {$EXTERNALSYM GL_CURRENT_BIT}
  GL_POINT_BIT                                      = $00000002;
  {$EXTERNALSYM GL_POINT_BIT}
  GL_LINE_BIT                                       = $00000004;
  {$EXTERNALSYM GL_LINE_BIT}
  GL_POLYGON_BIT                                    = $00000008;
  {$EXTERNALSYM GL_POLYGON_BIT}
  GL_POLYGON_STIPPLE_BIT                            = $00000010;
  {$EXTERNALSYM GL_POLYGON_STIPPLE_BIT}
  GL_PIXEL_MODE_BIT                                 = $00000020;
  {$EXTERNALSYM GL_PIXEL_MODE_BIT}
  GL_LIGHTING_BIT                                   = $00000040;
  {$EXTERNALSYM GL_LIGHTING_BIT}
  GL_FOG_BIT                                        = $00000080;
  {$EXTERNALSYM GL_FOG_BIT}
  GL_DEPTH_BUFFER_BIT                               = $00000100;
  {$EXTERNALSYM GL_DEPTH_BUFFER_BIT}
  GL_ACCUM_BUFFER_BIT                               = $00000200;
  {$EXTERNALSYM GL_ACCUM_BUFFER_BIT}
  GL_STENCIL_BUFFER_BIT                             = $00000400;
  {$EXTERNALSYM GL_STENCIL_BUFFER_BIT}
  GL_VIEWPORT_BIT                                   = $00000800;
  {$EXTERNALSYM GL_VIEWPORT_BIT}
  GL_TRANSFORM_BIT                                  = $00001000;
  {$EXTERNALSYM GL_TRANSFORM_BIT}
  GL_ENABLE_BIT                                     = $00002000;
  {$EXTERNALSYM GL_ENABLE_BIT}
  GL_COLOR_BUFFER_BIT                               = $00004000;
  {$EXTERNALSYM GL_COLOR_BUFFER_BIT}
  GL_HINT_BIT                                       = $00008000;
  {$EXTERNALSYM GL_HINT_BIT}
  GL_EVAL_BIT                                       = $00010000;
  {$EXTERNALSYM GL_EVAL_BIT}
  GL_LIST_BIT                                       = $00020000;
  {$EXTERNALSYM GL_LIST_BIT}
  GL_TEXTURE_BIT                                    = $00040000;
  {$EXTERNALSYM GL_TEXTURE_BIT}
  GL_SCISSOR_BIT                                    = $00080000;
  {$EXTERNALSYM GL_SCISSOR_BIT}
  GL_ALL_ATTRIB_BITS                                = $000FFFFF;
  {$EXTERNALSYM GL_ALL_ATTRIB_BITS}

  // client attribute bits
  GL_CLIENT_PIXEL_STORE_BIT                         = $00000001;
  {$EXTERNALSYM GL_CLIENT_PIXEL_STORE_BIT}
  GL_CLIENT_VERTEX_ARRAY_BIT                        = $00000002;
  {$EXTERNALSYM GL_CLIENT_VERTEX_ARRAY_BIT}
  GL_CLIENT_ALL_ATTRIB_BITS                         = $FFFFFFFF;
  {$EXTERNALSYM GL_CLIENT_ALL_ATTRIB_BITS}

  // boolean values
  GL_FALSE                                          = Boolean( 0 );
  {$EXTERNALSYM GL_FALSE}
  GL_TRUE                                           = Boolean( 1 );
  {$EXTERNALSYM GL_TRUE}

  // primitives
  GL_POINTS                                         = $0000;
  {$EXTERNALSYM GL_POINTS}
  GL_LINES                                          = $0001;
  {$EXTERNALSYM GL_LINES}
  GL_LINE_LOOP                                      = $0002;
  {$EXTERNALSYM GL_LINE_LOOP}
  GL_LINE_STRIP                                     = $0003;
  {$EXTERNALSYM GL_LINE_STRIP}
  GL_TRIANGLES                                      = $0004;
  {$EXTERNALSYM GL_TRIANGLES}
  GL_TRIANGLE_STRIP                                 = $0005;
  {$EXTERNALSYM GL_TRIANGLE_STRIP}
  GL_TRIANGLE_FAN                                   = $0006;
  {$EXTERNALSYM GL_TRIANGLE_FAN}
  GL_QUADS                                          = $0007;
  {$EXTERNALSYM GL_QUADS}
  GL_QUAD_STRIP                                     = $0008;
  {$EXTERNALSYM GL_QUAD_STRIP}
  GL_POLYGON                                        = $0009;
  {$EXTERNALSYM GL_POLYGON}

  // blending
  GL_ZERO                                           = 0;
  {$EXTERNALSYM GL_ZERO}
  GL_ONE                                            = 1;
  {$EXTERNALSYM GL_ONE}
  GL_SRC_COLOR                                      = $0300;
  {$EXTERNALSYM GL_SRC_COLOR}
  GL_ONE_MINUS_SRC_COLOR                            = $0301;
  {$EXTERNALSYM GL_ONE_MINUS_SRC_COLOR}
  GL_SRC_ALPHA                                      = $0302;
  {$EXTERNALSYM GL_SRC_ALPHA}
  GL_ONE_MINUS_SRC_ALPHA                            = $0303;
  {$EXTERNALSYM GL_ONE_MINUS_SRC_ALPHA}
  GL_DST_ALPHA                                      = $0304;
  {$EXTERNALSYM GL_DST_ALPHA}
  GL_ONE_MINUS_DST_ALPHA                            = $0305;
  {$EXTERNALSYM GL_ONE_MINUS_DST_ALPHA}
  GL_DST_COLOR                                      = $0306;
  {$EXTERNALSYM GL_DST_COLOR}
  GL_ONE_MINUS_DST_COLOR                            = $0307;
  {$EXTERNALSYM GL_ONE_MINUS_DST_COLOR}
  GL_SRC_ALPHA_SATURATE                             = $0308;
  {$EXTERNALSYM GL_SRC_ALPHA_SATURATE}
  GL_BLEND_DST                                      = $0BE0;
  {$EXTERNALSYM GL_BLEND_DST}
  GL_BLEND_SRC                                      = $0BE1;
  {$EXTERNALSYM GL_BLEND_SRC}
  GL_BLEND                                          = $0BE2;
  {$EXTERNALSYM GL_BLEND}

  // blending (GL 1.2 ARB imaging)
  GL_BLEND_COLOR                                    = $8005;
  {$EXTERNALSYM GL_BLEND_COLOR}
  GL_CONSTANT_COLOR                                 = $8001;
  {$EXTERNALSYM GL_CONSTANT_COLOR}
  GL_ONE_MINUS_CONSTANT_COLOR                       = $8002;
  {$EXTERNALSYM GL_ONE_MINUS_CONSTANT_COLOR}
  GL_CONSTANT_ALPHA                                 = $8003;
  {$EXTERNALSYM GL_CONSTANT_ALPHA}
  GL_ONE_MINUS_CONSTANT_ALPHA                       = $8004;
  {$EXTERNALSYM GL_ONE_MINUS_CONSTANT_ALPHA}
  GL_FUNC_ADD                                       = $8006;
  {$EXTERNALSYM GL_FUNC_ADD}
  GL_MIN                                            = $8007;
  {$EXTERNALSYM GL_MIN}
  GL_MAX                                            = $8008;
  {$EXTERNALSYM GL_MAX}
  GL_FUNC_SUBTRACT                                  = $800A;
  {$EXTERNALSYM GL_FUNC_SUBTRACT}
  GL_FUNC_REVERSE_SUBTRACT                          = $800B;
  {$EXTERNALSYM GL_FUNC_REVERSE_SUBTRACT}

  // color table GL 1.2 ARB imaging
  GL_COLOR_TABLE                                    = $80D0;
  {$EXTERNALSYM GL_COLOR_TABLE}
  GL_POST_CONVOLUTION_COLOR_TABLE                   = $80D1;
  {$EXTERNALSYM GL_POST_CONVOLUTION_COLOR_TABLE}
  GL_POST_COLOR_MATRIX_COLOR_TABLE                  = $80D2;
  {$EXTERNALSYM GL_POST_COLOR_MATRIX_COLOR_TABLE}
  GL_PROXY_COLOR_TABLE                              = $80D3;
  {$EXTERNALSYM GL_PROXY_COLOR_TABLE}
  GL_PROXY_POST_CONVOLUTION_COLOR_TABLE             = $80D4;
  {$EXTERNALSYM GL_PROXY_POST_CONVOLUTION_COLOR_TABLE}
  GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE            = $80D5;
  {$EXTERNALSYM GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE}
  GL_COLOR_TABLE_SCALE                              = $80D6;
  {$EXTERNALSYM GL_COLOR_TABLE_SCALE}
  GL_COLOR_TABLE_BIAS                               = $80D7;
  {$EXTERNALSYM GL_COLOR_TABLE_BIAS}
  GL_COLOR_TABLE_FORMAT                             = $80D8;
  {$EXTERNALSYM GL_COLOR_TABLE_FORMAT}
  GL_COLOR_TABLE_WIDTH                              = $80D9;
  {$EXTERNALSYM GL_COLOR_TABLE_WIDTH}
  GL_COLOR_TABLE_RED_SIZE                           = $80DA;
  {$EXTERNALSYM GL_COLOR_TABLE_RED_SIZE}
  GL_COLOR_TABLE_GREEN_SIZE                         = $80DB;
  {$EXTERNALSYM GL_COLOR_TABLE_GREEN_SIZE}
  GL_COLOR_TABLE_BLUE_SIZE                          = $80DC;
  {$EXTERNALSYM GL_COLOR_TABLE_BLUE_SIZE}
  GL_COLOR_TABLE_ALPHA_SIZE                         = $80DD;
  {$EXTERNALSYM GL_COLOR_TABLE_ALPHA_SIZE}
  GL_COLOR_TABLE_LUMINANCE_SIZE                     = $80DE;
  {$EXTERNALSYM GL_COLOR_TABLE_LUMINANCE_SIZE}
  GL_COLOR_TABLE_INTENSITY_SIZE                     = $80DF;
  {$EXTERNALSYM GL_COLOR_TABLE_INTENSITY_SIZE}

  // convolutions GL 1.2 ARB imaging
  GL_CONVOLUTION_1D                                 = $8010;
  {$EXTERNALSYM GL_CONVOLUTION_1D}
  GL_CONVOLUTION_2D                                 = $8011;
  {$EXTERNALSYM GL_CONVOLUTION_2D}
  GL_SEPARABLE_2D                                   = $8012;
  {$EXTERNALSYM GL_SEPARABLE_2D}
  GL_CONVOLUTION_BORDER_MODE                        = $8013;
  {$EXTERNALSYM GL_CONVOLUTION_BORDER_MODE}
  GL_CONVOLUTION_FILTER_SCALE                       = $8014;
  {$EXTERNALSYM GL_CONVOLUTION_FILTER_SCALE}
  GL_CONVOLUTION_FILTER_BIAS                        = $8015;
  {$EXTERNALSYM GL_CONVOLUTION_FILTER_BIAS}
  GL_REDUCE                                         = $8016;
  {$EXTERNALSYM GL_REDUCE}
  GL_CONVOLUTION_FORMAT                             = $8017;
  {$EXTERNALSYM GL_CONVOLUTION_FORMAT}
  GL_CONVOLUTION_WIDTH                              = $8018;
  {$EXTERNALSYM GL_CONVOLUTION_WIDTH}
  GL_CONVOLUTION_HEIGHT                             = $8019;
  {$EXTERNALSYM GL_CONVOLUTION_HEIGHT}
  GL_MAX_CONVOLUTION_WIDTH                          = $801A;
  {$EXTERNALSYM GL_MAX_CONVOLUTION_WIDTH}
  GL_MAX_CONVOLUTION_HEIGHT                         = $801B;
  {$EXTERNALSYM GL_MAX_CONVOLUTION_HEIGHT}
  GL_POST_CONVOLUTION_RED_SCALE                     = $801C;
  {$EXTERNALSYM GL_POST_CONVOLUTION_RED_SCALE}
  GL_POST_CONVOLUTION_GREEN_SCALE                   = $801D;
  {$EXTERNALSYM GL_POST_CONVOLUTION_GREEN_SCALE}
  GL_POST_CONVOLUTION_BLUE_SCALE                    = $801E;
  {$EXTERNALSYM GL_POST_CONVOLUTION_BLUE_SCALE}
  GL_POST_CONVOLUTION_ALPHA_SCALE                   = $801F;
  {$EXTERNALSYM GL_POST_CONVOLUTION_ALPHA_SCALE}
  GL_POST_CONVOLUTION_RED_BIAS                      = $8020;
  {$EXTERNALSYM GL_POST_CONVOLUTION_RED_BIAS}
  GL_POST_CONVOLUTION_GREEN_BIAS                    = $8021;
  {$EXTERNALSYM GL_POST_CONVOLUTION_GREEN_BIAS}
  GL_POST_CONVOLUTION_BLUE_BIAS                     = $8022;
  {$EXTERNALSYM GL_POST_CONVOLUTION_BLUE_BIAS}
  GL_POST_CONVOLUTION_ALPHA_BIAS                    = $8023;
  {$EXTERNALSYM GL_POST_CONVOLUTION_ALPHA_BIAS}

  // histogram GL 1.2 ARB imaging
  GL_HISTOGRAM                                      = $8024;
  {$EXTERNALSYM GL_HISTOGRAM}
  GL_PROXY_HISTOGRAM                                = $8025;
  {$EXTERNALSYM GL_PROXY_HISTOGRAM}
  GL_HISTOGRAM_WIDTH                                = $8026;
  {$EXTERNALSYM GL_HISTOGRAM_WIDTH}
  GL_HISTOGRAM_FORMAT                               = $8027;
  {$EXTERNALSYM GL_HISTOGRAM_FORMAT}
  GL_HISTOGRAM_RED_SIZE                             = $8028;
  {$EXTERNALSYM GL_HISTOGRAM_RED_SIZE}
  GL_HISTOGRAM_GREEN_SIZE                           = $8029;
  {$EXTERNALSYM GL_HISTOGRAM_GREEN_SIZE}
  GL_HISTOGRAM_BLUE_SIZE                            = $802A;
  {$EXTERNALSYM GL_HISTOGRAM_BLUE_SIZE}
  GL_HISTOGRAM_ALPHA_SIZE                           = $802B;
  {$EXTERNALSYM GL_HISTOGRAM_ALPHA_SIZE}
  GL_HISTOGRAM_LUMINANCE_SIZE                       = $802C;
  {$EXTERNALSYM GL_HISTOGRAM_LUMINANCE_SIZE}
  GL_HISTOGRAM_SINK                                 = $802D;
  {$EXTERNALSYM GL_HISTOGRAM_SINK}
  GL_MINMAX                                         = $802E;
  {$EXTERNALSYM GL_MINMAX}
  GL_MINMAX_FORMAT                                  = $802F;
  {$EXTERNALSYM GL_MINMAX_FORMAT}
  GL_MINMAX_SINK                                    = $8030;
  {$EXTERNALSYM GL_MINMAX_SINK}

  // buffers
  GL_NONE                                           = 0;
  {$EXTERNALSYM GL_NONE}
  GL_FRONT_LEFT                                     = $0400;
  {$EXTERNALSYM GL_FRONT_LEFT}
  GL_FRONT_RIGHT                                    = $0401;
  {$EXTERNALSYM GL_FRONT_RIGHT}
  GL_BACK_LEFT                                      = $0402;
  {$EXTERNALSYM GL_BACK_LEFT}
  GL_BACK_RIGHT                                     = $0403;
  {$EXTERNALSYM GL_BACK_RIGHT}
  GL_FRONT                                          = $0404;
  {$EXTERNALSYM GL_FRONT}
  GL_BACK                                           = $0405;
  {$EXTERNALSYM GL_BACK}
  GL_LEFT                                           = $0406;
  {$EXTERNALSYM GL_LEFT}
  GL_RIGHT                                          = $0407;
  {$EXTERNALSYM GL_RIGHT}
  GL_FRONT_AND_BACK                                 = $0408;
  {$EXTERNALSYM GL_FRONT_AND_BACK}
  GL_AUX0                                           = $0409;
  {$EXTERNALSYM GL_AUX0}
  GL_AUX1                                           = $040A;
  {$EXTERNALSYM GL_AUX1}
  GL_AUX2                                           = $040B;
  {$EXTERNALSYM GL_AUX2}
  GL_AUX3                                           = $040C;
  {$EXTERNALSYM GL_AUX3}
  GL_AUX_BUFFERS                                    = $0C00;
  {$EXTERNALSYM GL_AUX_BUFFERS}
  GL_DRAW_BUFFER                                    = $0C01;
  {$EXTERNALSYM GL_DRAW_BUFFER}
  GL_READ_BUFFER                                    = $0C02;
  {$EXTERNALSYM GL_READ_BUFFER}
  GL_DOUBLEBUFFER                                   = $0C32;
  {$EXTERNALSYM GL_DOUBLEBUFFER}
  GL_STEREO                                         = $0C33;
  {$EXTERNALSYM GL_STEREO}

  // depth buffer
  GL_DEPTH_RANGE                                    = $0B70;
  {$EXTERNALSYM GL_DEPTH_RANGE}
  GL_DEPTH_TEST                                     = $0B71;
  {$EXTERNALSYM GL_DEPTH_TEST}
  GL_DEPTH_WRITEMASK                                = $0B72;
  {$EXTERNALSYM GL_DEPTH_WRITEMASK}
  GL_DEPTH_CLEAR_VALUE                              = $0B73;
  {$EXTERNALSYM GL_DEPTH_CLEAR_VALUE}
  GL_DEPTH_FUNC                                     = $0B74;
  {$EXTERNALSYM GL_DEPTH_FUNC}
  GL_NEVER                                          = $0200;
  {$EXTERNALSYM GL_NEVER}
  GL_LESS                                           = $0201;
  {$EXTERNALSYM GL_LESS}
  GL_EQUAL                                          = $0202;
  {$EXTERNALSYM GL_EQUAL}
  GL_LEQUAL                                         = $0203;
  {$EXTERNALSYM GL_LEQUAL}
  GL_GREATER                                        = $0204;
  {$EXTERNALSYM GL_GREATER}
  GL_NOTEQUAL                                       = $0205;
  {$EXTERNALSYM GL_NOTEQUAL}
  GL_GEQUAL                                         = $0206;
  {$EXTERNALSYM GL_GEQUAL}
  GL_ALWAYS                                         = $0207;
  {$EXTERNALSYM GL_ALWAYS}

  // accumulation buffer
  GL_ACCUM                                          = $0100;
  {$EXTERNALSYM GL_ACCUM}
  GL_LOAD                                           = $0101;
  {$EXTERNALSYM GL_LOAD}
  GL_RETURN                                         = $0102;
  {$EXTERNALSYM GL_RETURN}
  GL_MULT                                           = $0103;
  {$EXTERNALSYM GL_MULT}
  GL_ADD                                            = $0104;
  {$EXTERNALSYM GL_ADD}
  GL_ACCUM_CLEAR_VALUE                              = $0B80;
  {$EXTERNALSYM GL_ACCUM_CLEAR_VALUE}

  // feedback buffer
  GL_FEEDBACK_BUFFER_POINTER                        = $0DF0;
  {$EXTERNALSYM GL_FEEDBACK_BUFFER_POINTER}
  GL_FEEDBACK_BUFFER_SIZE                           = $0DF1;
  {$EXTERNALSYM GL_FEEDBACK_BUFFER_SIZE}
  GL_FEEDBACK_BUFFER_TYPE                           = $0DF2;
  {$EXTERNALSYM GL_FEEDBACK_BUFFER_TYPE}

  // feedback types
  GL_2D                                             = $0600;
  {$EXTERNALSYM GL_2D}
  GL_3D                                             = $0601;
  {$EXTERNALSYM GL_3D}
  GL_3D_COLOR                                       = $0602;
  {$EXTERNALSYM GL_3D_COLOR}
  GL_3D_COLOR_TEXTURE                               = $0603;
  {$EXTERNALSYM GL_3D_COLOR_TEXTURE}
  GL_4D_COLOR_TEXTURE                               = $0604;
  {$EXTERNALSYM GL_4D_COLOR_TEXTURE}

  // feedback tokens
  GL_PASS_THROUGH_TOKEN                             = $0700;
  {$EXTERNALSYM GL_PASS_THROUGH_TOKEN}
  GL_POINT_TOKEN                                    = $0701;
  {$EXTERNALSYM GL_POINT_TOKEN}
  GL_LINE_TOKEN                                     = $0702;
  {$EXTERNALSYM GL_LINE_TOKEN}
  GL_POLYGON_TOKEN                                  = $0703;
  {$EXTERNALSYM GL_POLYGON_TOKEN}
  GL_BITMAP_TOKEN                                   = $0704;
  {$EXTERNALSYM GL_BITMAP_TOKEN}
  GL_DRAW_PIXEL_TOKEN                               = $0705;
  {$EXTERNALSYM GL_DRAW_PIXEL_TOKEN}
  GL_COPY_PIXEL_TOKEN                               = $0706;
  {$EXTERNALSYM GL_COPY_PIXEL_TOKEN}
  GL_LINE_RESET_TOKEN                               = $0707;
  {$EXTERNALSYM GL_LINE_RESET_TOKEN}

  // fog
  GL_EXP                                            = $0800;
  {$EXTERNALSYM GL_EXP}
  GL_EXP2                                           = $0801;
  {$EXTERNALSYM GL_EXP2}
  GL_FOG                                            = $0B60;
  {$EXTERNALSYM GL_FOG}
  GL_FOG_INDEX                                      = $0B61;
  {$EXTERNALSYM GL_FOG_INDEX}
  GL_FOG_DENSITY                                    = $0B62;
  {$EXTERNALSYM GL_FOG_DENSITY}
  GL_FOG_START                                      = $0B63;
  {$EXTERNALSYM GL_FOG_START}
  GL_FOG_END                                        = $0B64;
  {$EXTERNALSYM GL_FOG_END}
  GL_FOG_MODE                                       = $0B65;
  {$EXTERNALSYM GL_FOG_MODE}
  GL_FOG_COLOR                                      = $0B66;
  {$EXTERNALSYM GL_FOG_COLOR}

  // pixel mode, transfer
  GL_PIXEL_MAP_I_TO_I                               = $0C70;
  {$EXTERNALSYM GL_PIXEL_MAP_I_TO_I}
  GL_PIXEL_MAP_S_TO_S                               = $0C71;
  {$EXTERNALSYM GL_PIXEL_MAP_S_TO_S}
  GL_PIXEL_MAP_I_TO_R                               = $0C72;
  {$EXTERNALSYM GL_PIXEL_MAP_I_TO_R}
  GL_PIXEL_MAP_I_TO_G                               = $0C73;
  {$EXTERNALSYM GL_PIXEL_MAP_I_TO_G}
  GL_PIXEL_MAP_I_TO_B                               = $0C74;
  {$EXTERNALSYM GL_PIXEL_MAP_I_TO_B}
  GL_PIXEL_MAP_I_TO_A                               = $0C75;
  {$EXTERNALSYM GL_PIXEL_MAP_I_TO_A}
  GL_PIXEL_MAP_R_TO_R                               = $0C76;
  {$EXTERNALSYM GL_PIXEL_MAP_R_TO_R}
  GL_PIXEL_MAP_G_TO_G                               = $0C77;
  {$EXTERNALSYM GL_PIXEL_MAP_G_TO_G}
  GL_PIXEL_MAP_B_TO_B                               = $0C78;
  {$EXTERNALSYM GL_PIXEL_MAP_B_TO_B}
  GL_PIXEL_MAP_A_TO_A                               = $0C79;
  {$EXTERNALSYM GL_PIXEL_MAP_A_TO_A}

  // vertex arrays
  GL_VERTEX_ARRAY_POINTER                           = $808E;
  {$EXTERNALSYM GL_VERTEX_ARRAY_POINTER}
  GL_NORMAL_ARRAY_POINTER                           = $808F;
  {$EXTERNALSYM GL_NORMAL_ARRAY_POINTER}
  GL_COLOR_ARRAY_POINTER                            = $8090;
  {$EXTERNALSYM GL_COLOR_ARRAY_POINTER}
  GL_INDEX_ARRAY_POINTER                            = $8091;
  {$EXTERNALSYM GL_INDEX_ARRAY_POINTER}
  GL_TEXTURE_COORD_ARRAY_POINTER                    = $8092;
  {$EXTERNALSYM GL_TEXTURE_COORD_ARRAY_POINTER}
  GL_EDGE_FLAG_ARRAY_POINTER                        = $8093;
  {$EXTERNALSYM GL_EDGE_FLAG_ARRAY_POINTER}

  // stenciling
  GL_STENCIL_TEST                                   = $0B90;
  {$EXTERNALSYM GL_STENCIL_TEST}
  GL_STENCIL_CLEAR_VALUE                            = $0B91;
  {$EXTERNALSYM GL_STENCIL_CLEAR_VALUE}
  GL_STENCIL_FUNC                                   = $0B92;
  {$EXTERNALSYM GL_STENCIL_FUNC}
  GL_STENCIL_VALUE_MASK                             = $0B93;
  {$EXTERNALSYM GL_STENCIL_VALUE_MASK}
  GL_STENCIL_FAIL                                   = $0B94;
  {$EXTERNALSYM GL_STENCIL_FAIL}
  GL_STENCIL_PASS_DEPTH_FAIL                        = $0B95;
  {$EXTERNALSYM GL_STENCIL_PASS_DEPTH_FAIL}
  GL_STENCIL_PASS_DEPTH_PASS                        = $0B96;
  {$EXTERNALSYM GL_STENCIL_PASS_DEPTH_PASS}
  GL_STENCIL_REF                                    = $0B97;
  {$EXTERNALSYM GL_STENCIL_REF}
  GL_STENCIL_WRITEMASK                              = $0B98;
  {$EXTERNALSYM GL_STENCIL_WRITEMASK}
  GL_KEEP                                           = $1E00;
  {$EXTERNALSYM GL_KEEP}
  GL_REPLACE                                        = $1E01;
  {$EXTERNALSYM GL_REPLACE}
  GL_INCR                                           = $1E02;
  {$EXTERNALSYM GL_INCR}
  GL_DECR                                           = $1E03;
  {$EXTERNALSYM GL_DECR}

  // color material
  GL_COLOR_MATERIAL_FACE                            = $0B55;
  {$EXTERNALSYM GL_COLOR_MATERIAL_FACE}
  GL_COLOR_MATERIAL_PARAMETER                       = $0B56;
  {$EXTERNALSYM GL_COLOR_MATERIAL_PARAMETER}
  GL_COLOR_MATERIAL                                 = $0B57;
  {$EXTERNALSYM GL_COLOR_MATERIAL}

  // points
  GL_POINT_SMOOTH                                   = $0B10;
  {$EXTERNALSYM GL_POINT_SMOOTH}
  GL_POINT_SIZE                                     = $0B11;
  {$EXTERNALSYM GL_POINT_SIZE}
  GL_POINT_SIZE_RANGE                               = $0B12;
  {$EXTERNALSYM GL_POINT_SIZE_RANGE}
  GL_POINT_SIZE_GRANULARITY                         = $0B13;
  {$EXTERNALSYM GL_POINT_SIZE_GRANULARITY}

  // lines
  GL_LINE_SMOOTH                                    = $0B20;
  {$EXTERNALSYM GL_LINE_SMOOTH}
  GL_LINE_WIDTH                                     = $0B21;
  {$EXTERNALSYM GL_LINE_WIDTH}
  GL_LINE_WIDTH_RANGE                               = $0B22;
  {$EXTERNALSYM GL_LINE_WIDTH_RANGE}
  GL_LINE_WIDTH_GRANULARITY                         = $0B23;
  {$EXTERNALSYM GL_LINE_WIDTH_GRANULARITY}
  GL_LINE_STIPPLE                                   = $0B24;
  {$EXTERNALSYM GL_LINE_STIPPLE}
  GL_LINE_STIPPLE_PATTERN                           = $0B25;
  {$EXTERNALSYM GL_LINE_STIPPLE_PATTERN}
  GL_LINE_STIPPLE_REPEAT                            = $0B26;
  {$EXTERNALSYM GL_LINE_STIPPLE_REPEAT}

  // polygons
  GL_POLYGON_MODE                                   = $0B40;
  {$EXTERNALSYM GL_POLYGON_MODE}
  GL_POLYGON_SMOOTH                                 = $0B41;
  {$EXTERNALSYM GL_POLYGON_SMOOTH}
  GL_POLYGON_STIPPLE                                = $0B42;
  {$EXTERNALSYM GL_POLYGON_STIPPLE}
  GL_EDGE_FLAG                                      = $0B43;
  {$EXTERNALSYM GL_EDGE_FLAG}
  GL_CULL_FACE                                      = $0B44;
  {$EXTERNALSYM GL_CULL_FACE}
  GL_CULL_FACE_MODE                                 = $0B45;
  {$EXTERNALSYM GL_CULL_FACE_MODE}
  GL_FRONT_FACE                                     = $0B46;
  {$EXTERNALSYM GL_FRONT_FACE}
  GL_CW                                             = $0900;
  {$EXTERNALSYM GL_CW}
  GL_CCW                                            = $0901;
  {$EXTERNALSYM GL_CCW}
  GL_POINT                                          = $1B00;
  {$EXTERNALSYM GL_POINT}
  GL_LINE                                           = $1B01;
  {$EXTERNALSYM GL_LINE}
  GL_FILL                                           = $1B02;
  {$EXTERNALSYM GL_FILL}

  // display lists
  GL_LIST_MODE                                      = $0B30;
  {$EXTERNALSYM GL_LIST_MODE}
  GL_LIST_BASE                                      = $0B32;
  {$EXTERNALSYM GL_LIST_BASE}
  GL_LIST_INDEX                                     = $0B33;
  {$EXTERNALSYM GL_LIST_INDEX}
  GL_COMPILE                                        = $1300;
  {$EXTERNALSYM GL_COMPILE}
  GL_COMPILE_AND_EXECUTE                            = $1301;
  {$EXTERNALSYM GL_COMPILE_AND_EXECUTE}

  // lighting
  GL_LIGHTING                                       = $0B50;
  {$EXTERNALSYM GL_LIGHTING}
  GL_LIGHT_MODEL_LOCAL_VIEWER                       = $0B51;
  {$EXTERNALSYM GL_LIGHT_MODEL_LOCAL_VIEWER}
  GL_LIGHT_MODEL_TWO_SIDE                           = $0B52;
  {$EXTERNALSYM GL_LIGHT_MODEL_TWO_SIDE}
  GL_LIGHT_MODEL_AMBIENT                            = $0B53;
  {$EXTERNALSYM GL_LIGHT_MODEL_AMBIENT}
  GL_LIGHT_MODEL_COLOR_CONTROL                      = $81F8; // GL 1.2
  {$EXTERNALSYM GL_LIGHT_MODEL_COLOR_CONTROL}
  GL_SHADE_MODEL                                    = $0B54;
  {$EXTERNALSYM GL_SHADE_MODEL}
  GL_NORMALIZE                                      = $0BA1;
  {$EXTERNALSYM GL_NORMALIZE}
  GL_AMBIENT                                        = $1200;
  {$EXTERNALSYM GL_AMBIENT}
  GL_DIFFUSE                                        = $1201;
  {$EXTERNALSYM GL_DIFFUSE}
  GL_SPECULAR                                       = $1202;
  {$EXTERNALSYM GL_SPECULAR}
  GL_POSITION                                       = $1203;
  {$EXTERNALSYM GL_POSITION}
  GL_SPOT_DIRECTION                                 = $1204;
  {$EXTERNALSYM GL_SPOT_DIRECTION}
  GL_SPOT_EXPONENT                                  = $1205;
  {$EXTERNALSYM GL_SPOT_EXPONENT}
  GL_SPOT_CUTOFF                                    = $1206;
  {$EXTERNALSYM GL_SPOT_CUTOFF}
  GL_CONSTANT_ATTENUATION                           = $1207;
  {$EXTERNALSYM GL_CONSTANT_ATTENUATION}
  GL_LINEAR_ATTENUATION                             = $1208;
  {$EXTERNALSYM GL_LINEAR_ATTENUATION}
  GL_QUADRATIC_ATTENUATION                          = $1209;
  {$EXTERNALSYM GL_QUADRATIC_ATTENUATION}
  GL_EMISSION                                       = $1600;
  {$EXTERNALSYM GL_EMISSION}
  GL_SHININESS                                      = $1601;
  {$EXTERNALSYM GL_SHININESS}
  GL_AMBIENT_AND_DIFFUSE                            = $1602;
  {$EXTERNALSYM GL_AMBIENT_AND_DIFFUSE}
  GL_COLOR_INDEXES                                  = $1603;
  {$EXTERNALSYM GL_COLOR_INDEXES}
  GL_FLAT                                           = $1D00;
  {$EXTERNALSYM GL_FLAT}
  GL_SMOOTH                                         = $1D01;
  {$EXTERNALSYM GL_SMOOTH}
  GL_LIGHT0                                         = $4000;
  {$EXTERNALSYM GL_LIGHT0}
  GL_LIGHT1                                         = $4001;
  {$EXTERNALSYM GL_LIGHT1}
  GL_LIGHT2                                         = $4002;
  {$EXTERNALSYM GL_LIGHT2}
  GL_LIGHT3                                         = $4003;
  {$EXTERNALSYM GL_LIGHT3}
  GL_LIGHT4                                         = $4004;
  {$EXTERNALSYM GL_LIGHT4}
  GL_LIGHT5                                         = $4005;
  {$EXTERNALSYM GL_LIGHT5}
  GL_LIGHT6                                         = $4006;
  {$EXTERNALSYM GL_LIGHT6}
  GL_LIGHT7                                         = $4007;
  {$EXTERNALSYM GL_LIGHT7}

  // matrix modes
  GL_MATRIX_MODE                                    = $0BA0;
  {$EXTERNALSYM GL_MATRIX_MODE}
  GL_MODELVIEW                                      = $1700;
  {$EXTERNALSYM GL_MODELVIEW}
  GL_PROJECTION                                     = $1701;
  {$EXTERNALSYM GL_PROJECTION}
  GL_TEXTURE                                        = $1702;
  {$EXTERNALSYM GL_TEXTURE}

  // gets
  GL_CURRENT_COLOR                                  = $0B00;
  {$EXTERNALSYM GL_CURRENT_COLOR}
  GL_CURRENT_INDEX                                  = $0B01;
  {$EXTERNALSYM GL_CURRENT_INDEX}
  GL_CURRENT_NORMAL                                 = $0B02;
  {$EXTERNALSYM GL_CURRENT_NORMAL}
  GL_CURRENT_TEXTURE_COORDS                         = $0B03;
  {$EXTERNALSYM GL_CURRENT_TEXTURE_COORDS}
  GL_CURRENT_RASTER_COLOR                           = $0B04;
  {$EXTERNALSYM GL_CURRENT_RASTER_COLOR}
  GL_CURRENT_RASTER_INDEX                           = $0B05;
  {$EXTERNALSYM GL_CURRENT_RASTER_INDEX}
  GL_CURRENT_RASTER_TEXTURE_COORDS                  = $0B06;
  {$EXTERNALSYM GL_CURRENT_RASTER_TEXTURE_COORDS}
  GL_CURRENT_RASTER_POSITION                        = $0B07;
  {$EXTERNALSYM GL_CURRENT_RASTER_POSITION}
  GL_CURRENT_RASTER_POSITION_VALID                  = $0B08;
  {$EXTERNALSYM GL_CURRENT_RASTER_POSITION_VALID}
  GL_CURRENT_RASTER_DISTANCE                        = $0B09;
  {$EXTERNALSYM GL_CURRENT_RASTER_DISTANCE}
  GL_MAX_LIST_NESTING                               = $0B31;
  {$EXTERNALSYM GL_MAX_LIST_NESTING}
  GL_VIEWPORT                                       = $0BA2;
  {$EXTERNALSYM GL_VIEWPORT}
  GL_MODELVIEW_STACK_DEPTH                          = $0BA3;
  {$EXTERNALSYM GL_MODELVIEW_STACK_DEPTH}
  GL_PROJECTION_STACK_DEPTH                         = $0BA4;
  {$EXTERNALSYM GL_PROJECTION_STACK_DEPTH}
  GL_TEXTURE_STACK_DEPTH                            = $0BA5;
  {$EXTERNALSYM GL_TEXTURE_STACK_DEPTH}
  GL_MODELVIEW_MATRIX                               = $0BA6;
  {$EXTERNALSYM GL_MODELVIEW_MATRIX}
  GL_PROJECTION_MATRIX                              = $0BA7;
  {$EXTERNALSYM GL_PROJECTION_MATRIX}
  GL_TEXTURE_MATRIX                                 = $0BA8;
  {$EXTERNALSYM GL_TEXTURE_MATRIX}
  GL_ATTRIB_STACK_DEPTH                             = $0BB0;
  {$EXTERNALSYM GL_ATTRIB_STACK_DEPTH}
  GL_CLIENT_ATTRIB_STACK_DEPTH                      = $0BB1;
  {$EXTERNALSYM GL_CLIENT_ATTRIB_STACK_DEPTH}

  GL_SINGLE_COLOR                                   = $81F9; // GL 1.2
  {$EXTERNALSYM GL_SINGLE_COLOR}
  GL_SEPARATE_SPECULAR_COLOR                        = $81FA; // GL 1.2
  {$EXTERNALSYM GL_SEPARATE_SPECULAR_COLOR}

  // alpha testing
  GL_ALPHA_TEST                                     = $0BC0;
  {$EXTERNALSYM GL_ALPHA_TEST}
  GL_ALPHA_TEST_FUNC                                = $0BC1;
  {$EXTERNALSYM GL_ALPHA_TEST_FUNC}
  GL_ALPHA_TEST_REF                                 = $0BC2;
  {$EXTERNALSYM GL_ALPHA_TEST_REF}

  GL_LOGIC_OP_MODE                                  = $0BF0;
  {$EXTERNALSYM GL_LOGIC_OP_MODE}
  GL_INDEX_LOGIC_OP                                 = $0BF1;
  {$EXTERNALSYM GL_INDEX_LOGIC_OP}
  GL_LOGIC_OP                                       = $0BF1;
  {$EXTERNALSYM GL_LOGIC_OP}
  GL_COLOR_LOGIC_OP                                 = $0BF2;
  {$EXTERNALSYM GL_COLOR_LOGIC_OP}
  GL_SCISSOR_BOX                                    = $0C10;
  {$EXTERNALSYM GL_SCISSOR_BOX}
  GL_SCISSOR_TEST                                   = $0C11;
  {$EXTERNALSYM GL_SCISSOR_TEST}
  GL_INDEX_CLEAR_VALUE                              = $0C20;
  {$EXTERNALSYM GL_INDEX_CLEAR_VALUE}
  GL_INDEX_WRITEMASK                                = $0C21;
  {$EXTERNALSYM GL_INDEX_WRITEMASK}
  GL_COLOR_CLEAR_VALUE                              = $0C22;
  {$EXTERNALSYM GL_COLOR_CLEAR_VALUE}
  GL_COLOR_WRITEMASK                                = $0C23;
  {$EXTERNALSYM GL_COLOR_WRITEMASK}
  GL_INDEX_MODE                                     = $0C30;
  {$EXTERNALSYM GL_INDEX_MODE}
  GL_RGBA_MODE                                      = $0C31;
  {$EXTERNALSYM GL_RGBA_MODE}
  GL_RENDER_MODE                                    = $0C40;
  {$EXTERNALSYM GL_RENDER_MODE}
  GL_PERSPECTIVE_CORRECTION_HINT                    = $0C50;
  {$EXTERNALSYM GL_PERSPECTIVE_CORRECTION_HINT}
  GL_POINT_SMOOTH_HINT                              = $0C51;
  {$EXTERNALSYM GL_POINT_SMOOTH_HINT}
  GL_LINE_SMOOTH_HINT                               = $0C52;
  {$EXTERNALSYM GL_LINE_SMOOTH_HINT}
  GL_POLYGON_SMOOTH_HINT                            = $0C53;
  {$EXTERNALSYM GL_POLYGON_SMOOTH_HINT}
  GL_FOG_HINT                                       = $0C54;
  {$EXTERNALSYM GL_FOG_HINT}
  GL_TEXTURE_GEN_S                                  = $0C60;
  {$EXTERNALSYM GL_TEXTURE_GEN_S}
  GL_TEXTURE_GEN_T                                  = $0C61;
  {$EXTERNALSYM GL_TEXTURE_GEN_T}
  GL_TEXTURE_GEN_R                                  = $0C62;
  {$EXTERNALSYM GL_TEXTURE_GEN_R}
  GL_TEXTURE_GEN_Q                                  = $0C63;
  {$EXTERNALSYM GL_TEXTURE_GEN_Q}
  GL_PIXEL_MAP_I_TO_I_SIZE                          = $0CB0;
  {$EXTERNALSYM GL_PIXEL_MAP_I_TO_I_SIZE}
  GL_PIXEL_MAP_S_TO_S_SIZE                          = $0CB1;
  {$EXTERNALSYM GL_PIXEL_MAP_S_TO_S_SIZE}
  GL_PIXEL_MAP_I_TO_R_SIZE                          = $0CB2;
  {$EXTERNALSYM GL_PIXEL_MAP_I_TO_R_SIZE}
  GL_PIXEL_MAP_I_TO_G_SIZE                          = $0CB3;
  {$EXTERNALSYM GL_PIXEL_MAP_I_TO_G_SIZE}
  GL_PIXEL_MAP_I_TO_B_SIZE                          = $0CB4;
  {$EXTERNALSYM GL_PIXEL_MAP_I_TO_B_SIZE}
  GL_PIXEL_MAP_I_TO_A_SIZE                          = $0CB5;
  {$EXTERNALSYM GL_PIXEL_MAP_I_TO_A_SIZE}
  GL_PIXEL_MAP_R_TO_R_SIZE                          = $0CB6;
  {$EXTERNALSYM GL_PIXEL_MAP_R_TO_R_SIZE}
  GL_PIXEL_MAP_G_TO_G_SIZE                          = $0CB7;
  {$EXTERNALSYM GL_PIXEL_MAP_G_TO_G_SIZE}
  GL_PIXEL_MAP_B_TO_B_SIZE                          = $0CB8;
  {$EXTERNALSYM GL_PIXEL_MAP_B_TO_B_SIZE}
  GL_PIXEL_MAP_A_TO_A_SIZE                          = $0CB9;
  {$EXTERNALSYM GL_PIXEL_MAP_A_TO_A_SIZE}
  GL_UNPACK_SWAP_BYTES                              = $0CF0;
  {$EXTERNALSYM GL_UNPACK_SWAP_BYTES}
  GL_UNPACK_LSB_FIRST                               = $0CF1;
  {$EXTERNALSYM GL_UNPACK_LSB_FIRST}
  GL_UNPACK_ROW_LENGTH                              = $0CF2;
  {$EXTERNALSYM GL_UNPACK_ROW_LENGTH}
  GL_UNPACK_SKIP_ROWS                               = $0CF3;
  {$EXTERNALSYM GL_UNPACK_SKIP_ROWS}
  GL_UNPACK_SKIP_PIXELS                             = $0CF4;
  {$EXTERNALSYM GL_UNPACK_SKIP_PIXELS}
  GL_UNPACK_ALIGNMENT                               = $0CF5;
  {$EXTERNALSYM GL_UNPACK_ALIGNMENT}
  GL_PACK_SWAP_BYTES                                = $0D00;
  {$EXTERNALSYM GL_PACK_SWAP_BYTES}
  GL_PACK_LSB_FIRST                                 = $0D01;
  {$EXTERNALSYM GL_PACK_LSB_FIRST}
  GL_PACK_ROW_LENGTH                                = $0D02;
  {$EXTERNALSYM GL_PACK_ROW_LENGTH}
  GL_PACK_SKIP_ROWS                                 = $0D03;
  {$EXTERNALSYM GL_PACK_SKIP_ROWS}
  GL_PACK_SKIP_PIXELS                               = $0D04;
  {$EXTERNALSYM GL_PACK_SKIP_PIXELS}
  GL_PACK_ALIGNMENT                                 = $0D05;
  {$EXTERNALSYM GL_PACK_ALIGNMENT}
  GL_PACK_SKIP_IMAGES                               = $806B; // GL 1.2
  {$EXTERNALSYM GL_PACK_SKIP_IMAGES}
  GL_PACK_IMAGE_HEIGHT                              = $806C; // GL 1.2
  {$EXTERNALSYM GL_PACK_IMAGE_HEIGHT}
  GL_UNPACK_SKIP_IMAGES                             = $806D; // GL 1.2
  {$EXTERNALSYM GL_UNPACK_SKIP_IMAGES}
  GL_UNPACK_IMAGE_HEIGHT                            = $806E; // GL 1.2
  {$EXTERNALSYM GL_UNPACK_IMAGE_HEIGHT}
  GL_MAP_COLOR                                      = $0D10;
  {$EXTERNALSYM GL_MAP_COLOR}
  GL_MAP_STENCIL                                    = $0D11;
  {$EXTERNALSYM GL_MAP_STENCIL}
  GL_INDEX_SHIFT                                    = $0D12;
  {$EXTERNALSYM GL_INDEX_SHIFT}
  GL_INDEX_OFFSET                                   = $0D13;
  {$EXTERNALSYM GL_INDEX_OFFSET}
  GL_RED_SCALE                                      = $0D14;
  {$EXTERNALSYM GL_RED_SCALE}
  GL_RED_BIAS                                       = $0D15;
  {$EXTERNALSYM GL_RED_BIAS}
  GL_ZOOM_X                                         = $0D16;
  {$EXTERNALSYM GL_ZOOM_X}
  GL_ZOOM_Y                                         = $0D17;
  {$EXTERNALSYM GL_ZOOM_Y}
  GL_GREEN_SCALE                                    = $0D18;
  {$EXTERNALSYM GL_GREEN_SCALE}
  GL_GREEN_BIAS                                     = $0D19;
  {$EXTERNALSYM GL_GREEN_BIAS}
  GL_BLUE_SCALE                                     = $0D1A;
  {$EXTERNALSYM GL_BLUE_SCALE}
  GL_BLUE_BIAS                                      = $0D1B;
  {$EXTERNALSYM GL_BLUE_BIAS}
  GL_ALPHA_SCALE                                    = $0D1C;
  {$EXTERNALSYM GL_ALPHA_SCALE}
  GL_ALPHA_BIAS                                     = $0D1D;
  {$EXTERNALSYM GL_ALPHA_BIAS}
  GL_DEPTH_SCALE                                    = $0D1E;
  {$EXTERNALSYM GL_DEPTH_SCALE}
  GL_DEPTH_BIAS                                     = $0D1F;
  {$EXTERNALSYM GL_DEPTH_BIAS}
  GL_MAX_EVAL_ORDER                                 = $0D30;
  {$EXTERNALSYM GL_MAX_EVAL_ORDER}
  GL_MAX_LIGHTS                                     = $0D31;
  {$EXTERNALSYM GL_MAX_LIGHTS}
  GL_MAX_CLIP_PLANES                                = $0D32;
  {$EXTERNALSYM GL_MAX_CLIP_PLANES}
  GL_MAX_TEXTURE_SIZE                               = $0D33;
  {$EXTERNALSYM GL_MAX_TEXTURE_SIZE}
  GL_MAX_3D_TEXTURE_SIZE                            = $8073; // GL 1.2
  {$EXTERNALSYM GL_MAX_3D_TEXTURE_SIZE}
  GL_MAX_PIXEL_MAP_TABLE                            = $0D34;
  {$EXTERNALSYM GL_MAX_PIXEL_MAP_TABLE}
  GL_MAX_ATTRIB_STACK_DEPTH                         = $0D35;
  {$EXTERNALSYM GL_MAX_ATTRIB_STACK_DEPTH}
  GL_MAX_MODELVIEW_STACK_DEPTH                      = $0D36;
  {$EXTERNALSYM GL_MAX_MODELVIEW_STACK_DEPTH}
  GL_MAX_NAME_STACK_DEPTH                           = $0D37;
  {$EXTERNALSYM GL_MAX_NAME_STACK_DEPTH}
  GL_MAX_PROJECTION_STACK_DEPTH                     = $0D38;
  {$EXTERNALSYM GL_MAX_PROJECTION_STACK_DEPTH}
  GL_MAX_TEXTURE_STACK_DEPTH                        = $0D39;
  {$EXTERNALSYM GL_MAX_TEXTURE_STACK_DEPTH}
  GL_MAX_VIEWPORT_DIMS                              = $0D3A;
  {$EXTERNALSYM GL_MAX_VIEWPORT_DIMS}
  GL_MAX_CLIENT_ATTRIB_STACK_DEPTH                  = $0D3B;
  {$EXTERNALSYM GL_MAX_CLIENT_ATTRIB_STACK_DEPTH}
  GL_MAX_ELEMENTS_VERTICES                          = $80E8; // GL 1.2
  {$EXTERNALSYM GL_MAX_ELEMENTS_VERTICES}
  GL_MAX_ELEMENTS_INDICES                           = $80E9; // GL 1.2
  {$EXTERNALSYM GL_MAX_ELEMENTS_INDICES}
  GL_RESCALE_NORMAL                                 = $803A; // GL 1.2
  {$EXTERNALSYM GL_RESCALE_NORMAL}
  GL_SUBPIXEL_BITS                                  = $0D50;
  {$EXTERNALSYM GL_SUBPIXEL_BITS}
  GL_INDEX_BITS                                     = $0D51;
  {$EXTERNALSYM GL_INDEX_BITS}
  GL_RED_BITS                                       = $0D52;
  {$EXTERNALSYM GL_RED_BITS}
  GL_GREEN_BITS                                     = $0D53;
  {$EXTERNALSYM GL_GREEN_BITS}
  GL_BLUE_BITS                                      = $0D54;
  {$EXTERNALSYM GL_BLUE_BITS}
  GL_ALPHA_BITS                                     = $0D55;
  {$EXTERNALSYM GL_ALPHA_BITS}
  GL_DEPTH_BITS                                     = $0D56;
  {$EXTERNALSYM GL_DEPTH_BITS}
  GL_STENCIL_BITS                                   = $0D57;
  {$EXTERNALSYM GL_STENCIL_BITS}
  GL_ACCUM_RED_BITS                                 = $0D58;
  {$EXTERNALSYM GL_ACCUM_RED_BITS}
  GL_ACCUM_GREEN_BITS                               = $0D59;
  {$EXTERNALSYM GL_ACCUM_GREEN_BITS}
  GL_ACCUM_BLUE_BITS                                = $0D5A;
  {$EXTERNALSYM GL_ACCUM_BLUE_BITS}
  GL_ACCUM_ALPHA_BITS                               = $0D5B;
  {$EXTERNALSYM GL_ACCUM_ALPHA_BITS}
  GL_NAME_STACK_DEPTH                               = $0D70;
  {$EXTERNALSYM GL_NAME_STACK_DEPTH}
  GL_AUTO_NORMAL                                    = $0D80;
  {$EXTERNALSYM GL_AUTO_NORMAL}
  GL_MAP1_COLOR_4                                   = $0D90;
  {$EXTERNALSYM GL_MAP1_COLOR_4}
  GL_MAP1_INDEX                                     = $0D91;
  {$EXTERNALSYM GL_MAP1_INDEX}
  GL_MAP1_NORMAL                                    = $0D92;
  {$EXTERNALSYM GL_MAP1_NORMAL}
  GL_MAP1_TEXTURE_COORD_1                           = $0D93;
  {$EXTERNALSYM GL_MAP1_TEXTURE_COORD_1}
  GL_MAP1_TEXTURE_COORD_2                           = $0D94;
  {$EXTERNALSYM GL_MAP1_TEXTURE_COORD_2}
  GL_MAP1_TEXTURE_COORD_3                           = $0D95;
  {$EXTERNALSYM GL_MAP1_TEXTURE_COORD_3}
  GL_MAP1_TEXTURE_COORD_4                           = $0D96;
  {$EXTERNALSYM GL_MAP1_TEXTURE_COORD_4}
  GL_MAP1_VERTEX_3                                  = $0D97;
  {$EXTERNALSYM GL_MAP1_VERTEX_3}
  GL_MAP1_VERTEX_4                                  = $0D98;
  {$EXTERNALSYM GL_MAP1_VERTEX_4}
  GL_MAP2_COLOR_4                                   = $0DB0;
  {$EXTERNALSYM GL_MAP2_COLOR_4}
  GL_MAP2_INDEX                                     = $0DB1;
  {$EXTERNALSYM GL_MAP2_INDEX}
  GL_MAP2_NORMAL                                    = $0DB2;
  {$EXTERNALSYM GL_MAP2_NORMAL}
  GL_MAP2_TEXTURE_COORD_1                           = $0DB3;
  {$EXTERNALSYM GL_MAP2_TEXTURE_COORD_1}
  GL_MAP2_TEXTURE_COORD_2                           = $0DB4;
  {$EXTERNALSYM GL_MAP2_TEXTURE_COORD_2}
  GL_MAP2_TEXTURE_COORD_3                           = $0DB5;
  {$EXTERNALSYM GL_MAP2_TEXTURE_COORD_3}
  GL_MAP2_TEXTURE_COORD_4                           = $0DB6;
  {$EXTERNALSYM GL_MAP2_TEXTURE_COORD_4}
  GL_MAP2_VERTEX_3                                  = $0DB7;
  {$EXTERNALSYM GL_MAP2_VERTEX_3}
  GL_MAP2_VERTEX_4                                  = $0DB8;
  {$EXTERNALSYM GL_MAP2_VERTEX_4}
  GL_MAP1_GRID_DOMAIN                               = $0DD0;
  {$EXTERNALSYM GL_MAP1_GRID_DOMAIN}
  GL_MAP1_GRID_SEGMENTS                             = $0DD1;
  {$EXTERNALSYM GL_MAP1_GRID_SEGMENTS}
  GL_MAP2_GRID_DOMAIN                               = $0DD2;
  {$EXTERNALSYM GL_MAP2_GRID_DOMAIN}
  GL_MAP2_GRID_SEGMENTS                             = $0DD3;
  {$EXTERNALSYM GL_MAP2_GRID_SEGMENTS}
  GL_TEXTURE_1D                                     = $0DE0;
  {$EXTERNALSYM GL_TEXTURE_1D}
  GL_TEXTURE_2D                                     = $0DE1;
  {$EXTERNALSYM GL_TEXTURE_2D}
  GL_TEXTURE_3D                                     = $806F; // GL 1.2
  {$EXTERNALSYM GL_TEXTURE_3D}
  GL_SELECTION_BUFFER_POINTER                       = $0DF3;
  {$EXTERNALSYM GL_SELECTION_BUFFER_POINTER}
  GL_SELECTION_BUFFER_SIZE                          = $0DF4;
  {$EXTERNALSYM GL_SELECTION_BUFFER_SIZE}
  GL_POLYGON_OFFSET_UNITS                           = $2A00;
  {$EXTERNALSYM GL_POLYGON_OFFSET_UNITS}
  GL_POLYGON_OFFSET_POINT                           = $2A01;
  {$EXTERNALSYM GL_POLYGON_OFFSET_POINT}
  GL_POLYGON_OFFSET_LINE                            = $2A02;
  {$EXTERNALSYM GL_POLYGON_OFFSET_LINE}
  GL_POLYGON_OFFSET_FILL                            = $8037;
  {$EXTERNALSYM GL_POLYGON_OFFSET_FILL}
  GL_POLYGON_OFFSET_FACTOR                          = $8038;
  {$EXTERNALSYM GL_POLYGON_OFFSET_FACTOR}
  GL_TEXTURE_BINDING_1D                             = $8068;
  {$EXTERNALSYM GL_TEXTURE_BINDING_1D}
  GL_TEXTURE_BINDING_2D                             = $8069;
  {$EXTERNALSYM GL_TEXTURE_BINDING_2D}
  GL_VERTEX_ARRAY                                   = $8074;
  {$EXTERNALSYM GL_VERTEX_ARRAY}
  GL_NORMAL_ARRAY                                   = $8075;
  {$EXTERNALSYM GL_NORMAL_ARRAY}
  GL_COLOR_ARRAY                                    = $8076;
  {$EXTERNALSYM GL_COLOR_ARRAY}
  GL_INDEX_ARRAY                                    = $8077;
  {$EXTERNALSYM GL_INDEX_ARRAY}
  GL_TEXTURE_COORD_ARRAY                            = $8078;
  {$EXTERNALSYM GL_TEXTURE_COORD_ARRAY}
  GL_EDGE_FLAG_ARRAY                                = $8079;
  {$EXTERNALSYM GL_EDGE_FLAG_ARRAY}
  GL_VERTEX_ARRAY_SIZE                              = $807A;
  {$EXTERNALSYM GL_VERTEX_ARRAY_SIZE}
  GL_VERTEX_ARRAY_TYPE                              = $807B;
  {$EXTERNALSYM GL_VERTEX_ARRAY_TYPE}
  GL_VERTEX_ARRAY_STRIDE                            = $807C;
  {$EXTERNALSYM GL_VERTEX_ARRAY_STRIDE}
  GL_NORMAL_ARRAY_TYPE                              = $807E;
  {$EXTERNALSYM GL_NORMAL_ARRAY_TYPE}
  GL_NORMAL_ARRAY_STRIDE                            = $807F;
  {$EXTERNALSYM GL_NORMAL_ARRAY_STRIDE}
  GL_COLOR_ARRAY_SIZE                               = $8081;
  {$EXTERNALSYM GL_COLOR_ARRAY_SIZE}
  GL_COLOR_ARRAY_TYPE                               = $8082;
  {$EXTERNALSYM GL_COLOR_ARRAY_TYPE}
  GL_COLOR_ARRAY_STRIDE                             = $8083;
  {$EXTERNALSYM GL_COLOR_ARRAY_STRIDE}
  GL_INDEX_ARRAY_TYPE                               = $8085;
  {$EXTERNALSYM GL_INDEX_ARRAY_TYPE}
  GL_INDEX_ARRAY_STRIDE                             = $8086;
  {$EXTERNALSYM GL_INDEX_ARRAY_STRIDE}
  GL_TEXTURE_COORD_ARRAY_SIZE                       = $8088;
  {$EXTERNALSYM GL_TEXTURE_COORD_ARRAY_SIZE}
  GL_TEXTURE_COORD_ARRAY_TYPE                       = $8089;
  {$EXTERNALSYM GL_TEXTURE_COORD_ARRAY_TYPE}
  GL_TEXTURE_COORD_ARRAY_STRIDE                     = $808A;
  {$EXTERNALSYM GL_TEXTURE_COORD_ARRAY_STRIDE}
  GL_EDGE_FLAG_ARRAY_STRIDE                         = $808C;
  {$EXTERNALSYM GL_EDGE_FLAG_ARRAY_STRIDE}
  GL_COLOR_MATRIX                                   = $80B1; // GL 1.2 ARB imaging
  {$EXTERNALSYM GL_COLOR_MATRIX}
  GL_COLOR_MATRIX_STACK_DEPTH                       = $80B2; // GL 1.2 ARB imaging
  {$EXTERNALSYM GL_COLOR_MATRIX_STACK_DEPTH}
  GL_MAX_COLOR_MATRIX_STACK_DEPTH                   = $80B3; // GL 1.2 ARB imaging
  {$EXTERNALSYM GL_MAX_COLOR_MATRIX_STACK_DEPTH}
  GL_POST_COLOR_MATRIX_RED_SCALE                    = $80B4; // GL 1.2 ARB imaging
  {$EXTERNALSYM GL_POST_COLOR_MATRIX_RED_SCALE}
  GL_POST_COLOR_MATRIX_GREEN_SCALE                  = $80B5; // GL 1.2 ARB imaging
  {$EXTERNALSYM GL_POST_COLOR_MATRIX_GREEN_SCALE}
  GL_POST_COLOR_MATRIX_BLUE_SCALE                   = $80B6; // GL 1.2 ARB imaging
  {$EXTERNALSYM GL_POST_COLOR_MATRIX_BLUE_SCALE}
  GL_POST_COLOR_MATRIX_ALPHA_SCALE                  = $80B7; // GL 1.2 ARB imaging
  {$EXTERNALSYM GL_POST_COLOR_MATRIX_ALPHA_SCALE}
  GL_POST_COLOR_MATRIX_RED_BIAS                     = $80B8; // GL 1.2 ARB imaging
  {$EXTERNALSYM GL_POST_COLOR_MATRIX_RED_BIAS}
  GL_POST_COLOR_MATRIX_GREEN_BIAS                   = $80B9; // GL 1.2 ARB imaging
  {$EXTERNALSYM GL_POST_COLOR_MATRIX_GREEN_BIAS}
  GL_POST_COLOR_MATRIX_BLUE_BIAS                    = $80BA; // GL 1.2 ARB imaging
  {$EXTERNALSYM GL_POST_COLOR_MATRIX_BLUE_BIAS}
  GL_POST_COLOR_MATRIX_ALPHA_BIAS                   = $80BB; // GL 1.2 ARB imaging
  {$EXTERNALSYM GL_POST_COLOR_MATRIX_ALPHA_BIAS}

  // evaluators
  GL_COEFF                                          = $0A00;
  {$EXTERNALSYM GL_COEFF}
  GL_ORDER                                          = $0A01;
  {$EXTERNALSYM GL_ORDER}
  GL_DOMAIN                                         = $0A02;
  {$EXTERNALSYM GL_DOMAIN}

  // texture mapping
  GL_TEXTURE_WIDTH                                  = $1000;
  {$EXTERNALSYM GL_TEXTURE_WIDTH}
  GL_TEXTURE_HEIGHT                                 = $1001;
  {$EXTERNALSYM GL_TEXTURE_HEIGHT}
  GL_TEXTURE_INTERNAL_FORMAT                        = $1003;
  {$EXTERNALSYM GL_TEXTURE_INTERNAL_FORMAT}
  GL_TEXTURE_COMPONENTS                             = $1003;
  {$EXTERNALSYM GL_TEXTURE_COMPONENTS}
  GL_TEXTURE_BORDER_COLOR                           = $1004;
  {$EXTERNALSYM GL_TEXTURE_BORDER_COLOR}
  GL_TEXTURE_BORDER                                 = $1005;
  {$EXTERNALSYM GL_TEXTURE_BORDER}
  GL_TEXTURE_RED_SIZE                               = $805C;
  {$EXTERNALSYM GL_TEXTURE_RED_SIZE}
  GL_TEXTURE_GREEN_SIZE                             = $805D;
  {$EXTERNALSYM GL_TEXTURE_GREEN_SIZE}
  GL_TEXTURE_BLUE_SIZE                              = $805E;
  {$EXTERNALSYM GL_TEXTURE_BLUE_SIZE}
  GL_TEXTURE_ALPHA_SIZE                             = $805F;
  {$EXTERNALSYM GL_TEXTURE_ALPHA_SIZE}
  GL_TEXTURE_LUMINANCE_SIZE                         = $8060;
  {$EXTERNALSYM GL_TEXTURE_LUMINANCE_SIZE}
  GL_TEXTURE_INTENSITY_SIZE                         = $8061;
  {$EXTERNALSYM GL_TEXTURE_INTENSITY_SIZE}
  GL_TEXTURE_PRIORITY                               = $8066;
  {$EXTERNALSYM GL_TEXTURE_PRIORITY}
  GL_TEXTURE_RESIDENT                               = $8067;
  {$EXTERNALSYM GL_TEXTURE_RESIDENT}
  GL_BGR                                            = $80E0; // v 1.2
  {$EXTERNALSYM GL_BGR}
  GL_BGRA                                           = $80E1; // v 1.2
  {$EXTERNALSYM GL_BGRA}
  GL_S                                              = $2000;
  {$EXTERNALSYM GL_S}
  GL_T                                              = $2001;
  {$EXTERNALSYM GL_T}
  GL_R                                              = $2002;
  {$EXTERNALSYM GL_R}
  GL_Q                                              = $2003;
  {$EXTERNALSYM GL_Q}
  GL_MODULATE                                       = $2100;
  {$EXTERNALSYM GL_MODULATE}
  GL_DECAL                                          = $2101;
  {$EXTERNALSYM GL_DECAL}
  GL_TEXTURE_ENV_MODE                               = $2200;
  {$EXTERNALSYM GL_TEXTURE_ENV_MODE}
  GL_TEXTURE_ENV_COLOR                              = $2201;
  {$EXTERNALSYM GL_TEXTURE_ENV_COLOR}
  GL_TEXTURE_ENV                                    = $2300;
  {$EXTERNALSYM GL_TEXTURE_ENV}
  GL_EYE_LINEAR                                     = $2400;
  {$EXTERNALSYM GL_EYE_LINEAR}
  GL_OBJECT_LINEAR                                  = $2401;
  {$EXTERNALSYM GL_OBJECT_LINEAR}
  GL_SPHERE_MAP                                     = $2402;
  {$EXTERNALSYM GL_SPHERE_MAP}
  GL_TEXTURE_GEN_MODE                               = $2500;
  {$EXTERNALSYM GL_TEXTURE_GEN_MODE}
  GL_OBJECT_PLANE                                   = $2501;
  {$EXTERNALSYM GL_OBJECT_PLANE}
  GL_EYE_PLANE                                      = $2502;
  {$EXTERNALSYM GL_EYE_PLANE}
  GL_NEAREST                                        = $2600;
  {$EXTERNALSYM GL_NEAREST}
  GL_LINEAR                                         = $2601;
  {$EXTERNALSYM GL_LINEAR}
  GL_NEAREST_MIPMAP_NEAREST                         = $2700;
  {$EXTERNALSYM GL_NEAREST_MIPMAP_NEAREST}
  GL_LINEAR_MIPMAP_NEAREST                          = $2701;
  {$EXTERNALSYM GL_LINEAR_MIPMAP_NEAREST}
  GL_NEAREST_MIPMAP_LINEAR                          = $2702;
  {$EXTERNALSYM GL_NEAREST_MIPMAP_LINEAR}
  GL_LINEAR_MIPMAP_LINEAR                           = $2703;
  {$EXTERNALSYM GL_LINEAR_MIPMAP_LINEAR}
  GL_TEXTURE_MAG_FILTER                             = $2800;
  {$EXTERNALSYM GL_TEXTURE_MAG_FILTER}
  GL_TEXTURE_MIN_FILTER                             = $2801;
  {$EXTERNALSYM GL_TEXTURE_MIN_FILTER}
  GL_TEXTURE_WRAP_R                                 = $8072; // GL 1.2
  {$EXTERNALSYM GL_TEXTURE_WRAP_R}
  GL_TEXTURE_WRAP_S                                 = $2802;
  {$EXTERNALSYM GL_TEXTURE_WRAP_S}
  GL_TEXTURE_WRAP_T                                 = $2803;
  {$EXTERNALSYM GL_TEXTURE_WRAP_T}
  GL_CLAMP_TO_EDGE                                  = $812F; // GL 1.2
  {$EXTERNALSYM GL_CLAMP_TO_EDGE}
  GL_TEXTURE_MIN_LOD                                = $813A; // GL 1.2
  {$EXTERNALSYM GL_TEXTURE_MIN_LOD}
  GL_TEXTURE_MAX_LOD                                = $813B; // GL 1.2
  {$EXTERNALSYM GL_TEXTURE_MAX_LOD}
  GL_TEXTURE_BASE_LEVEL                             = $813C; // GL 1.2
  {$EXTERNALSYM GL_TEXTURE_BASE_LEVEL}
  GL_TEXTURE_MAX_LEVEL                              = $813D; // GL 1.2
  {$EXTERNALSYM GL_TEXTURE_MAX_LEVEL}
  GL_TEXTURE_DEPTH                                  = $8071; // GL 1.2
  {$EXTERNALSYM GL_TEXTURE_DEPTH}
  GL_PROXY_TEXTURE_1D                               = $8063;
  {$EXTERNALSYM GL_PROXY_TEXTURE_1D}
  GL_PROXY_TEXTURE_2D                               = $8064;
  {$EXTERNALSYM GL_PROXY_TEXTURE_2D}
  GL_PROXY_TEXTURE_3D                               = $8070; // GL 1.2
  {$EXTERNALSYM GL_PROXY_TEXTURE_3D}
  GL_CLAMP                                          = $2900;
  {$EXTERNALSYM GL_CLAMP}
  GL_REPEAT                                         = $2901;
  {$EXTERNALSYM GL_REPEAT}

  // hints
  GL_DONT_CARE                                      = $1100;
  {$EXTERNALSYM GL_DONT_CARE}
  GL_FASTEST                                        = $1101;
  {$EXTERNALSYM GL_FASTEST}
  GL_NICEST                                         = $1102;
  {$EXTERNALSYM GL_NICEST}

  // data types
  GL_BYTE                                           = $1400;
  {$EXTERNALSYM GL_BYTE}
  GL_UNSIGNED_BYTE                                  = $1401;
  {$EXTERNALSYM GL_UNSIGNED_BYTE}
  GL_SHORT                                          = $1402;
  {$EXTERNALSYM GL_SHORT}
  GL_UNSIGNED_SHORT                                 = $1403;
  {$EXTERNALSYM GL_UNSIGNED_SHORT}
  GL_INT                                            = $1404;
  {$EXTERNALSYM GL_INT}
  GL_UNSIGNED_INT                                   = $1405;
  {$EXTERNALSYM GL_UNSIGNED_INT}
  GL_FLOAT                                          = $1406;
  {$EXTERNALSYM GL_FLOAT}
  GL_2_BYTES                                        = $1407;
  {$EXTERNALSYM GL_2_BYTES}
  GL_3_BYTES                                        = $1408;
  {$EXTERNALSYM GL_3_BYTES}
  GL_4_BYTES                                        = $1409;
  {$EXTERNALSYM GL_4_BYTES}
  GL_DOUBLE                                         = $140A;
  {$EXTERNALSYM GL_DOUBLE}
  GL_DOUBLE_EXT                                     = $140A;
  {$EXTERNALSYM GL_DOUBLE_EXT}

  // logic operations
  GL_CLEAR                                          = $1500;
  {$EXTERNALSYM GL_CLEAR}
  GL_AND                                            = $1501;
  {$EXTERNALSYM GL_AND}
  GL_AND_REVERSE                                    = $1502;
  {$EXTERNALSYM GL_AND_REVERSE}
  GL_COPY                                           = $1503;
  {$EXTERNALSYM GL_COPY}
  GL_AND_INVERTED                                   = $1504;
  {$EXTERNALSYM GL_AND_INVERTED}
  GL_NOOP                                           = $1505;
  {$EXTERNALSYM GL_NOOP}
  GL_XOR                                            = $1506;
  {$EXTERNALSYM GL_XOR}
  GL_OR                                             = $1507;
  {$EXTERNALSYM GL_OR}
  GL_NOR                                            = $1508;
  {$EXTERNALSYM GL_NOR}
  GL_EQUIV                                          = $1509;
  {$EXTERNALSYM GL_EQUIV}
  GL_INVERT                                         = $150A;
  {$EXTERNALSYM GL_INVERT}
  GL_OR_REVERSE                                     = $150B;
  {$EXTERNALSYM GL_OR_REVERSE}
  GL_COPY_INVERTED                                  = $150C;
  {$EXTERNALSYM GL_COPY_INVERTED}
  GL_OR_INVERTED                                    = $150D;
  {$EXTERNALSYM GL_OR_INVERTED}
  GL_NAND                                           = $150E;
  {$EXTERNALSYM GL_NAND}
  GL_SET                                            = $150F;
  {$EXTERNALSYM GL_SET}

  // PixelCopyType
  GL_COLOR                                          = $1800;
  {$EXTERNALSYM GL_COLOR}
  GL_DEPTH                                          = $1801;
  {$EXTERNALSYM GL_DEPTH}
  GL_STENCIL                                        = $1802;
  {$EXTERNALSYM GL_STENCIL}

  // pixel formats
  GL_COLOR_INDEX                                    = $1900;
  {$EXTERNALSYM GL_COLOR_INDEX}
  GL_STENCIL_INDEX                                  = $1901;
  {$EXTERNALSYM GL_STENCIL_INDEX}
  GL_DEPTH_COMPONENT                                = $1902;
  {$EXTERNALSYM GL_DEPTH_COMPONENT}
  GL_RED                                            = $1903;
  {$EXTERNALSYM GL_RED}
  GL_GREEN                                          = $1904;
  {$EXTERNALSYM GL_GREEN}
  GL_BLUE                                           = $1905;
  {$EXTERNALSYM GL_BLUE}
  GL_ALPHA                                          = $1906;
  {$EXTERNALSYM GL_ALPHA}
  GL_RGB                                            = $1907;
  {$EXTERNALSYM GL_RGB}
  GL_RGBA                                           = $1908;
  {$EXTERNALSYM GL_RGBA}
  GL_LUMINANCE                                      = $1909;
  {$EXTERNALSYM GL_LUMINANCE}
  GL_LUMINANCE_ALPHA                                = $190A;
  {$EXTERNALSYM GL_LUMINANCE_ALPHA}

  // pixel type
  GL_BITMAP                                         = $1A00;
  {$EXTERNALSYM GL_BITMAP}

  // rendering modes
  GL_RENDER                                         = $1C00;
  {$EXTERNALSYM GL_RENDER}
  GL_FEEDBACK                                       = $1C01;
  {$EXTERNALSYM GL_FEEDBACK}
  GL_SELECT                                         = $1C02;
  {$EXTERNALSYM GL_SELECT}

  // implementation strings
  GL_VENDOR                                         = $1F00;
  {$EXTERNALSYM GL_VENDOR}
  GL_RENDERER                                       = $1F01;
  {$EXTERNALSYM GL_RENDERER}
  GL_VERSION                                        = $1F02;
  {$EXTERNALSYM GL_VERSION}
  GL_EXTENSIONS                                     = $1F03;
  {$EXTERNALSYM GL_EXTENSIONS}

  // pixel formats
  GL_R3_G3_B2                                       = $2A10;
  {$EXTERNALSYM GL_R3_G3_B2}
  GL_ALPHA4                                         = $803B;
  {$EXTERNALSYM GL_ALPHA4}
  GL_ALPHA8                                         = $803C;
  {$EXTERNALSYM GL_ALPHA8}
  GL_ALPHA12                                        = $803D;
  {$EXTERNALSYM GL_ALPHA12}
  GL_ALPHA16                                        = $803E;
  {$EXTERNALSYM GL_ALPHA16}
  GL_LUMINANCE4                                     = $803F;
  {$EXTERNALSYM GL_LUMINANCE4}
  GL_LUMINANCE8                                     = $8040;
  {$EXTERNALSYM GL_LUMINANCE8}
  GL_LUMINANCE12                                    = $8041;
  {$EXTERNALSYM GL_LUMINANCE12}
  GL_LUMINANCE16                                    = $8042;
  {$EXTERNALSYM GL_LUMINANCE16}
  GL_LUMINANCE4_ALPHA4                              = $8043;
  {$EXTERNALSYM GL_LUMINANCE4_ALPHA4}
  GL_LUMINANCE6_ALPHA2                              = $8044;
  {$EXTERNALSYM GL_LUMINANCE6_ALPHA2}
  GL_LUMINANCE8_ALPHA8                              = $8045;
  {$EXTERNALSYM GL_LUMINANCE8_ALPHA8}
  GL_LUMINANCE12_ALPHA4                             = $8046;
  {$EXTERNALSYM GL_LUMINANCE12_ALPHA4}
  GL_LUMINANCE12_ALPHA12                            = $8047;
  {$EXTERNALSYM GL_LUMINANCE12_ALPHA12}
  GL_LUMINANCE16_ALPHA16                            = $8048;
  {$EXTERNALSYM GL_LUMINANCE16_ALPHA16}
  GL_INTENSITY                                      = $8049;
  {$EXTERNALSYM GL_INTENSITY}
  GL_INTENSITY4                                     = $804A;
  {$EXTERNALSYM GL_INTENSITY4}
  GL_INTENSITY8                                     = $804B;
  {$EXTERNALSYM GL_INTENSITY8}
  GL_INTENSITY12                                    = $804C;
  {$EXTERNALSYM GL_INTENSITY12}
  GL_INTENSITY16                                    = $804D;
  {$EXTERNALSYM GL_INTENSITY16}
  GL_RGB4                                           = $804F;
  {$EXTERNALSYM GL_RGB4}
  GL_RGB5                                           = $8050;
  {$EXTERNALSYM GL_RGB5}
  GL_RGB8                                           = $8051;
  {$EXTERNALSYM GL_RGB8}
  GL_RGB10                                          = $8052;
  {$EXTERNALSYM GL_RGB10}
  GL_RGB12                                          = $8053;
  {$EXTERNALSYM GL_RGB12}
  GL_RGB16                                          = $8054;
  {$EXTERNALSYM GL_RGB16}
  GL_RGBA2                                          = $8055;
  {$EXTERNALSYM GL_RGBA2}
  GL_RGBA4                                          = $8056;
  {$EXTERNALSYM GL_RGBA4}
  GL_RGB5_A1                                        = $8057;
  {$EXTERNALSYM GL_RGB5_A1}
  GL_RGBA8                                          = $8058;
  {$EXTERNALSYM GL_RGBA8}
  GL_RGB10_A2                                       = $8059;
  {$EXTERNALSYM GL_RGB10_A2}
  GL_RGBA12                                         = $805A;
  {$EXTERNALSYM GL_RGBA12}
  GL_RGBA16                                         = $805B;
  {$EXTERNALSYM GL_RGBA16}
  UNSIGNED_BYTE_3_3_2                               = $8032; // GL 1.2
  {$EXTERNALSYM UNSIGNED_BYTE_3_3_2}
  UNSIGNED_BYTE_2_3_3_REV                           = $8362; // GL 1.2
  {$EXTERNALSYM UNSIGNED_BYTE_2_3_3_REV}
  UNSIGNED_SHORT_5_6_5                              = $8363; // GL 1.2
  {$EXTERNALSYM UNSIGNED_SHORT_5_6_5}
  UNSIGNED_SHORT_5_6_5_REV                          = $8364; // GL 1.2
  {$EXTERNALSYM UNSIGNED_SHORT_5_6_5_REV}
  UNSIGNED_SHORT_4_4_4_4                            = $8033; // GL 1.2
  {$EXTERNALSYM UNSIGNED_SHORT_4_4_4_4}
  UNSIGNED_SHORT_4_4_4_4_REV                        = $8365; // GL 1.2
  {$EXTERNALSYM UNSIGNED_SHORT_4_4_4_4_REV}
  UNSIGNED_SHORT_5_5_5_1                            = $8034; // GL 1.2
  {$EXTERNALSYM UNSIGNED_SHORT_5_5_5_1}
  UNSIGNED_SHORT_1_5_5_5_REV                        = $8366; // GL 1.2
  {$EXTERNALSYM UNSIGNED_SHORT_1_5_5_5_REV}
  UNSIGNED_INT_8_8_8_8                              = $8035; // GL 1.2
  {$EXTERNALSYM UNSIGNED_INT_8_8_8_8}
  UNSIGNED_INT_8_8_8_8_REV                          = $8367; // GL 1.2
  {$EXTERNALSYM UNSIGNED_INT_8_8_8_8_REV}
  UNSIGNED_INT_10_10_10_2                           = $8036; // GL 1.2
  {$EXTERNALSYM UNSIGNED_INT_10_10_10_2}
  UNSIGNED_INT_2_10_10_10_REV                       = $8368; // GL 1.2
  {$EXTERNALSYM UNSIGNED_INT_2_10_10_10_REV}

  // interleaved arrays formats
  GL_V2F                                            = $2A20;
  {$EXTERNALSYM GL_V2F}
  GL_V3F                                            = $2A21;
  {$EXTERNALSYM GL_V3F}
  GL_C4UB_V2F                                       = $2A22;
  {$EXTERNALSYM GL_C4UB_V2F}
  GL_C4UB_V3F                                       = $2A23;
  {$EXTERNALSYM GL_C4UB_V3F}
  GL_C3F_V3F                                        = $2A24;
  {$EXTERNALSYM GL_C3F_V3F}
  GL_N3F_V3F                                        = $2A25;
  {$EXTERNALSYM GL_N3F_V3F}
  GL_C4F_N3F_V3F                                    = $2A26;
  {$EXTERNALSYM GL_C4F_N3F_V3F}
  GL_T2F_V3F                                        = $2A27;
  {$EXTERNALSYM GL_T2F_V3F}
  GL_T4F_V4F                                        = $2A28;
  {$EXTERNALSYM GL_T4F_V4F}
  GL_T2F_C4UB_V3F                                   = $2A29;
  {$EXTERNALSYM GL_T2F_C4UB_V3F}
  GL_T2F_C3F_V3F                                    = $2A2A;
  {$EXTERNALSYM GL_T2F_C3F_V3F}
  GL_T2F_N3F_V3F                                    = $2A2B;
  {$EXTERNALSYM GL_T2F_N3F_V3F}
  GL_T2F_C4F_N3F_V3F                                = $2A2C;
  {$EXTERNALSYM GL_T2F_C4F_N3F_V3F}
  GL_T4F_C4F_N3F_V4F                                = $2A2D;
  {$EXTERNALSYM GL_T4F_C4F_N3F_V4F}

  // clip planes
  GL_CLIP_PLANE0                                    = $3000;
  {$EXTERNALSYM GL_CLIP_PLANE0}
  GL_CLIP_PLANE1                                    = $3001;
  {$EXTERNALSYM GL_CLIP_PLANE1}
  GL_CLIP_PLANE2                                    = $3002;
  {$EXTERNALSYM GL_CLIP_PLANE2}
  GL_CLIP_PLANE3                                    = $3003;
  {$EXTERNALSYM GL_CLIP_PLANE3}
  GL_CLIP_PLANE4                                    = $3004;
  {$EXTERNALSYM GL_CLIP_PLANE4}
  GL_CLIP_PLANE5                                    = $3005;
  {$EXTERNALSYM GL_CLIP_PLANE5}

  // miscellaneous
  GL_DITHER                                         = $0BD0;
  {$EXTERNALSYM GL_DITHER}

  // ----- extensions enumerants -----
  // EXT_abgr
  GL_ABGR_EXT                                       = $8000;
  {$EXTERNALSYM GL_ABGR_EXT}

  // EXT_packed_pixels
  GL_UNSIGNED_BYTE_3_3_2_EXT                        = $8032;
  {$EXTERNALSYM GL_UNSIGNED_BYTE_3_3_2_EXT}
  GL_UNSIGNED_SHORT_4_4_4_4_EXT                     = $8033;
  {$EXTERNALSYM GL_UNSIGNED_SHORT_4_4_4_4_EXT}
  GL_UNSIGNED_SHORT_5_5_5_1_EXT                     = $8034;
  {$EXTERNALSYM GL_UNSIGNED_SHORT_5_5_5_1_EXT}
  GL_UNSIGNED_INT_8_8_8_8_EXT                       = $8035;
  {$EXTERNALSYM GL_UNSIGNED_INT_8_8_8_8_EXT}
  GL_UNSIGNED_INT_10_10_10_2_EXT                    = $8036;
  {$EXTERNALSYM GL_UNSIGNED_INT_10_10_10_2_EXT}

  // EXT_vertex_array
  GL_VERTEX_ARRAY_EXT                               = $8074;
  {$EXTERNALSYM GL_VERTEX_ARRAY_EXT}
  GL_NORMAL_ARRAY_EXT                               = $8075;
  {$EXTERNALSYM GL_NORMAL_ARRAY_EXT}
  GL_COLOR_ARRAY_EXT                                = $8076;
  {$EXTERNALSYM GL_COLOR_ARRAY_EXT}
  GL_INDEX_ARRAY_EXT                                = $8077;
  {$EXTERNALSYM GL_INDEX_ARRAY_EXT}
  GL_TEXTURE_COORD_ARRAY_EXT                        = $8078;
  {$EXTERNALSYM GL_TEXTURE_COORD_ARRAY_EXT}
  GL_EDGE_FLAG_ARRAY_EXT                            = $8079;
  {$EXTERNALSYM GL_EDGE_FLAG_ARRAY_EXT}
  GL_VERTEX_ARRAY_SIZE_EXT                          = $807A;
  {$EXTERNALSYM GL_VERTEX_ARRAY_SIZE_EXT}
  GL_VERTEX_ARRAY_TYPE_EXT                          = $807B;
  {$EXTERNALSYM GL_VERTEX_ARRAY_TYPE_EXT}
  GL_VERTEX_ARRAY_STRIDE_EXT                        = $807C;
  {$EXTERNALSYM GL_VERTEX_ARRAY_STRIDE_EXT}
  GL_VERTEX_ARRAY_COUNT_EXT                         = $807D;
  {$EXTERNALSYM GL_VERTEX_ARRAY_COUNT_EXT}
  GL_NORMAL_ARRAY_TYPE_EXT                          = $807E;
  {$EXTERNALSYM GL_NORMAL_ARRAY_TYPE_EXT}
  GL_NORMAL_ARRAY_STRIDE_EXT                        = $807F;
  {$EXTERNALSYM GL_NORMAL_ARRAY_STRIDE_EXT}
  GL_NORMAL_ARRAY_COUNT_EXT                         = $8080;
  {$EXTERNALSYM GL_NORMAL_ARRAY_COUNT_EXT}
  GL_COLOR_ARRAY_SIZE_EXT                           = $8081;
  {$EXTERNALSYM GL_COLOR_ARRAY_SIZE_EXT}
  GL_COLOR_ARRAY_TYPE_EXT                           = $8082;
  {$EXTERNALSYM GL_COLOR_ARRAY_TYPE_EXT}
  GL_COLOR_ARRAY_STRIDE_EXT                         = $8083;
  {$EXTERNALSYM GL_COLOR_ARRAY_STRIDE_EXT}
  GL_COLOR_ARRAY_COUNT_EXT                          = $8084;
  {$EXTERNALSYM GL_COLOR_ARRAY_COUNT_EXT}
  GL_INDEX_ARRAY_TYPE_EXT                           = $8085;
  {$EXTERNALSYM GL_INDEX_ARRAY_TYPE_EXT}
  GL_INDEX_ARRAY_STRIDE_EXT                         = $8086;
  {$EXTERNALSYM GL_INDEX_ARRAY_STRIDE_EXT}
  GL_INDEX_ARRAY_COUNT_EXT                          = $8087;
  {$EXTERNALSYM GL_INDEX_ARRAY_COUNT_EXT}
  GL_TEXTURE_COORD_ARRAY_SIZE_EXT                   = $8088;
  {$EXTERNALSYM GL_TEXTURE_COORD_ARRAY_SIZE_EXT}
  GL_TEXTURE_COORD_ARRAY_TYPE_EXT                   = $8089;
  {$EXTERNALSYM GL_TEXTURE_COORD_ARRAY_TYPE_EXT}
  GL_TEXTURE_COORD_ARRAY_STRIDE_EXT                 = $808A;
  {$EXTERNALSYM GL_TEXTURE_COORD_ARRAY_STRIDE_EXT}
  GL_TEXTURE_COORD_ARRAY_COUNT_EXT                  = $808B;
  {$EXTERNALSYM GL_TEXTURE_COORD_ARRAY_COUNT_EXT}
  GL_EDGE_FLAG_ARRAY_STRIDE_EXT                     = $808C;
  {$EXTERNALSYM GL_EDGE_FLAG_ARRAY_STRIDE_EXT}
  GL_EDGE_FLAG_ARRAY_COUNT_EXT                      = $808D;
  {$EXTERNALSYM GL_EDGE_FLAG_ARRAY_COUNT_EXT}
  GL_VERTEX_ARRAY_POINTER_EXT                       = $808E;
  {$EXTERNALSYM GL_VERTEX_ARRAY_POINTER_EXT}
  GL_NORMAL_ARRAY_POINTER_EXT                       = $808F;
  {$EXTERNALSYM GL_NORMAL_ARRAY_POINTER_EXT}
  GL_COLOR_ARRAY_POINTER_EXT                        = $8090;
  {$EXTERNALSYM GL_COLOR_ARRAY_POINTER_EXT}
  GL_INDEX_ARRAY_POINTER_EXT                        = $8091;
  {$EXTERNALSYM GL_INDEX_ARRAY_POINTER_EXT}
  GL_TEXTURE_COORD_ARRAY_POINTER_EXT                = $8092;
  {$EXTERNALSYM GL_TEXTURE_COORD_ARRAY_POINTER_EXT}
  GL_EDGE_FLAG_ARRAY_POINTER_EXT                    = $8093;
  {$EXTERNALSYM GL_EDGE_FLAG_ARRAY_POINTER_EXT}

  // EXT_color_table
  GL_TABLE_TOO_LARGE_EXT                            = $8031;
  {$EXTERNALSYM GL_TABLE_TOO_LARGE_EXT}
  GL_COLOR_TABLE_EXT                                = $80D0;
  {$EXTERNALSYM GL_COLOR_TABLE_EXT}
  GL_POST_CONVOLUTION_COLOR_TABLE_EXT               = $80D1;
  {$EXTERNALSYM GL_POST_CONVOLUTION_COLOR_TABLE_EXT}
  GL_POST_COLOR_MATRIX_COLOR_TABLE_EXT              = $80D2;
  {$EXTERNALSYM GL_POST_COLOR_MATRIX_COLOR_TABLE_EXT}
  GL_PROXY_COLOR_TABLE_EXT                          = $80D3;
  {$EXTERNALSYM GL_PROXY_COLOR_TABLE_EXT}
  GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_EXT         = $80D4;
  {$EXTERNALSYM GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_EXT}
  GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_EXT        = $80D5;
  {$EXTERNALSYM GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_EXT}
  GL_COLOR_TABLE_SCALE_EXT                          = $80D6;
  {$EXTERNALSYM GL_COLOR_TABLE_SCALE_EXT}
  GL_COLOR_TABLE_BIAS_EXT                           = $80D7;
  {$EXTERNALSYM GL_COLOR_TABLE_BIAS_EXT}
  GL_COLOR_TABLE_FORMAT_EXT                         = $80D8;
  {$EXTERNALSYM GL_COLOR_TABLE_FORMAT_EXT}
  GL_COLOR_TABLE_WIDTH_EXT                          = $80D9;
  {$EXTERNALSYM GL_COLOR_TABLE_WIDTH_EXT}
  GL_COLOR_TABLE_RED_SIZE_EXT                       = $80DA;
  {$EXTERNALSYM GL_COLOR_TABLE_RED_SIZE_EXT}
  GL_COLOR_TABLE_GREEN_SIZE_EXT                     = $80DB;
  {$EXTERNALSYM GL_COLOR_TABLE_GREEN_SIZE_EXT}
  GL_COLOR_TABLE_BLUE_SIZE_EXT                      = $80DC;
  {$EXTERNALSYM GL_COLOR_TABLE_BLUE_SIZE_EXT}
  GL_COLOR_TABLE_ALPHA_SIZE_EXT                     = $80DD;
  {$EXTERNALSYM GL_COLOR_TABLE_ALPHA_SIZE_EXT}
  GL_COLOR_TABLE_LUMINANCE_SIZE_EXT                 = $80DE;
  {$EXTERNALSYM GL_COLOR_TABLE_LUMINANCE_SIZE_EXT}
  GL_COLOR_TABLE_INTENSITY_SIZE_EXT                 = $80DF;
  {$EXTERNALSYM GL_COLOR_TABLE_INTENSITY_SIZE_EXT}

  // EXT_bgra
  GL_BGR_EXT                                        = $80E0;
  {$EXTERNALSYM GL_BGR_EXT}
  GL_BGRA_EXT                                       = $80E1;
  {$EXTERNALSYM GL_BGRA_EXT}

  // EXT_paletted_texture
  GL_COLOR_INDEX1_EXT                               = $80E2;
  {$EXTERNALSYM GL_COLOR_INDEX1_EXT}
  GL_COLOR_INDEX2_EXT                               = $80E3;
  {$EXTERNALSYM GL_COLOR_INDEX2_EXT}
  GL_COLOR_INDEX4_EXT                               = $80E4;
  {$EXTERNALSYM GL_COLOR_INDEX4_EXT}
  GL_COLOR_INDEX8_EXT                               = $80E5;
  {$EXTERNALSYM GL_COLOR_INDEX8_EXT}
  GL_COLOR_INDEX12_EXT                              = $80E6;
  {$EXTERNALSYM GL_COLOR_INDEX12_EXT}
  GL_COLOR_INDEX16_EXT                              = $80E7;
  {$EXTERNALSYM GL_COLOR_INDEX16_EXT}

  // EXT_blend_color
  GL_CONSTANT_COLOR_EXT                             = $8001;
  {$EXTERNALSYM GL_CONSTANT_COLOR_EXT}
  GL_ONE_MINUS_CONSTANT_COLOR_EXT                   = $8002;
  {$EXTERNALSYM GL_ONE_MINUS_CONSTANT_COLOR_EXT}
  GL_CONSTANT_ALPHA_EXT                             = $8003;
  {$EXTERNALSYM GL_CONSTANT_ALPHA_EXT}
  GL_ONE_MINUS_CONSTANT_ALPHA_EXT                   = $8004;
  {$EXTERNALSYM GL_ONE_MINUS_CONSTANT_ALPHA_EXT}
  GL_BLEND_COLOR_EXT                                = $8005;
  {$EXTERNALSYM GL_BLEND_COLOR_EXT}

  // EXT_blend_minmax
  GL_FUNC_ADD_EXT                                   = $8006;
  {$EXTERNALSYM GL_FUNC_ADD_EXT}
  GL_MIN_EXT                                        = $8007;
  {$EXTERNALSYM GL_MIN_EXT}
  GL_MAX_EXT                                        = $8008;
  {$EXTERNALSYM GL_MAX_EXT}
  GL_BLEND_EQUATION_EXT                             = $8009;
  {$EXTERNALSYM GL_BLEND_EQUATION_EXT}

  // EXT_blend_subtract
  GL_FUNC_SUBTRACT_EXT                              = $800A;
  {$EXTERNALSYM GL_FUNC_SUBTRACT_EXT}
  GL_FUNC_REVERSE_SUBTRACT_EXT                      = $800B;
  {$EXTERNALSYM GL_FUNC_REVERSE_SUBTRACT_EXT}

  // EXT_convolution
  GL_CONVOLUTION_1D_EXT                             = $8010;
  {$EXTERNALSYM GL_CONVOLUTION_1D_EXT}
  GL_CONVOLUTION_2D_EXT                             = $8011;
  {$EXTERNALSYM GL_CONVOLUTION_2D_EXT}
  GL_SEPARABLE_2D_EXT                               = $8012;
  {$EXTERNALSYM GL_SEPARABLE_2D_EXT}
  GL_CONVOLUTION_BORDER_MODE_EXT                    = $8013;
  {$EXTERNALSYM GL_CONVOLUTION_BORDER_MODE_EXT}
  GL_CONVOLUTION_FILTER_SCALE_EXT                   = $8014;
  {$EXTERNALSYM GL_CONVOLUTION_FILTER_SCALE_EXT}
  GL_CONVOLUTION_FILTER_BIAS_EXT                    = $8015;
  {$EXTERNALSYM GL_CONVOLUTION_FILTER_BIAS_EXT}
  GL_REDUCE_EXT                                     = $8016;
  {$EXTERNALSYM GL_REDUCE_EXT}
  GL_CONVOLUTION_FORMAT_EXT                         = $8017;
  {$EXTERNALSYM GL_CONVOLUTION_FORMAT_EXT}
  GL_CONVOLUTION_WIDTH_EXT                          = $8018;
  {$EXTERNALSYM GL_CONVOLUTION_WIDTH_EXT}
  GL_CONVOLUTION_HEIGHT_EXT                         = $8019;
  {$EXTERNALSYM GL_CONVOLUTION_HEIGHT_EXT}
  GL_MAX_CONVOLUTION_WIDTH_EXT                      = $801A;
  {$EXTERNALSYM GL_MAX_CONVOLUTION_WIDTH_EXT}
  GL_MAX_CONVOLUTION_HEIGHT_EXT                     = $801B;
  {$EXTERNALSYM GL_MAX_CONVOLUTION_HEIGHT_EXT}
  GL_POST_CONVOLUTION_RED_SCALE_EXT                 = $801C;
  {$EXTERNALSYM GL_POST_CONVOLUTION_RED_SCALE_EXT}
  GL_POST_CONVOLUTION_GREEN_SCALE_EXT               = $801D;
  {$EXTERNALSYM GL_POST_CONVOLUTION_GREEN_SCALE_EXT}
  GL_POST_CONVOLUTION_BLUE_SCALE_EXT                = $801E;
  {$EXTERNALSYM GL_POST_CONVOLUTION_BLUE_SCALE_EXT}
  GL_POST_CONVOLUTION_ALPHA_SCALE_EXT               = $801F;
  {$EXTERNALSYM GL_POST_CONVOLUTION_ALPHA_SCALE_EXT}
  GL_POST_CONVOLUTION_RED_BIAS_EXT                  = $8020;
  {$EXTERNALSYM GL_POST_CONVOLUTION_RED_BIAS_EXT}
  GL_POST_CONVOLUTION_GREEN_BIAS_EXT                = $8021;
  {$EXTERNALSYM GL_POST_CONVOLUTION_GREEN_BIAS_EXT}
  GL_POST_CONVOLUTION_BLUE_BIAS_EXT                 = $8022;
  {$EXTERNALSYM GL_POST_CONVOLUTION_BLUE_BIAS_EXT}
  GL_POST_CONVOLUTION_ALPHA_BIAS_EXT                = $8023;
  {$EXTERNALSYM GL_POST_CONVOLUTION_ALPHA_BIAS_EXT}

  // EXT_histogram
  GL_HISTOGRAM_EXT                                  = $8024;
  {$EXTERNALSYM GL_HISTOGRAM_EXT}
  GL_PROXY_HISTOGRAM_EXT                            = $8025;
  {$EXTERNALSYM GL_PROXY_HISTOGRAM_EXT}
  GL_HISTOGRAM_WIDTH_EXT                            = $8026;
  {$EXTERNALSYM GL_HISTOGRAM_WIDTH_EXT}
  GL_HISTOGRAM_FORMAT_EXT                           = $8027;
  {$EXTERNALSYM GL_HISTOGRAM_FORMAT_EXT}
  GL_HISTOGRAM_RED_SIZE_EXT                         = $8028;
  {$EXTERNALSYM GL_HISTOGRAM_RED_SIZE_EXT}
  GL_HISTOGRAM_GREEN_SIZE_EXT                       = $8029;
  {$EXTERNALSYM GL_HISTOGRAM_GREEN_SIZE_EXT}
  GL_HISTOGRAM_BLUE_SIZE_EXT                        = $802A;
  {$EXTERNALSYM GL_HISTOGRAM_BLUE_SIZE_EXT}
  GL_HISTOGRAM_ALPHA_SIZE_EXT                       = $802B;
  {$EXTERNALSYM GL_HISTOGRAM_ALPHA_SIZE_EXT}
  GL_HISTOGRAM_LUMINANCE_SIZE_EXT                   = $802C;
  {$EXTERNALSYM GL_HISTOGRAM_LUMINANCE_SIZE_EXT}
  GL_HISTOGRAM_SINK_EXT                             = $802D;
  {$EXTERNALSYM GL_HISTOGRAM_SINK_EXT}
  GL_MINMAX_EXT                                     = $802E;
  {$EXTERNALSYM GL_MINMAX_EXT}
  GL_MINMAX_FORMAT_EXT                              = $802F;
  {$EXTERNALSYM GL_MINMAX_FORMAT_EXT}
  GL_MINMAX_SINK_EXT                                = $8030;
  {$EXTERNALSYM GL_MINMAX_SINK_EXT}

  // EXT_polygon_offset
  GL_POLYGON_OFFSET_EXT                             = $8037;
  {$EXTERNALSYM GL_POLYGON_OFFSET_EXT}
  GL_POLYGON_OFFSET_FACTOR_EXT                      = $8038;
  {$EXTERNALSYM GL_POLYGON_OFFSET_FACTOR_EXT}
  GL_POLYGON_OFFSET_BIAS_EXT                        = $8039;
  {$EXTERNALSYM GL_POLYGON_OFFSET_BIAS_EXT}

  // EXT_texture
  GL_ALPHA4_EXT                                     = $803B;
  {$EXTERNALSYM GL_ALPHA4_EXT}
  GL_ALPHA8_EXT                                     = $803C;
  {$EXTERNALSYM GL_ALPHA8_EXT}
  GL_ALPHA12_EXT                                    = $803D;
  {$EXTERNALSYM GL_ALPHA12_EXT}
  GL_ALPHA16_EXT                                    = $803E;
  {$EXTERNALSYM GL_ALPHA16_EXT}
  GL_LUMINANCE4_EXT                                 = $803F;
  {$EXTERNALSYM GL_LUMINANCE4_EXT}
  GL_LUMINANCE8_EXT                                 = $8040;
  {$EXTERNALSYM GL_LUMINANCE8_EXT}
  GL_LUMINANCE12_EXT                                = $8041;
  {$EXTERNALSYM GL_LUMINANCE12_EXT}
  GL_LUMINANCE16_EXT                                = $8042;
  {$EXTERNALSYM GL_LUMINANCE16_EXT}
  GL_LUMINANCE4_ALPHA4_EXT                          = $8043;
  {$EXTERNALSYM GL_LUMINANCE4_ALPHA4_EXT}
  GL_LUMINANCE6_ALPHA2_EXT                          = $8044;
  {$EXTERNALSYM GL_LUMINANCE6_ALPHA2_EXT}
  GL_LUMINANCE8_ALPHA8_EXT                          = $8045;
  {$EXTERNALSYM GL_LUMINANCE8_ALPHA8_EXT}
  GL_LUMINANCE12_ALPHA4_EXT                         = $8046;
  {$EXTERNALSYM GL_LUMINANCE12_ALPHA4_EXT}
  GL_LUMINANCE12_ALPHA12_EXT                        = $8047;
  {$EXTERNALSYM GL_LUMINANCE12_ALPHA12_EXT}
  GL_LUMINANCE16_ALPHA16_EXT                        = $8048;
  {$EXTERNALSYM GL_LUMINANCE16_ALPHA16_EXT}
  GL_INTENSITY_EXT                                  = $8049;
  {$EXTERNALSYM GL_INTENSITY_EXT}
  GL_INTENSITY4_EXT                                 = $804A;
  {$EXTERNALSYM GL_INTENSITY4_EXT}
  GL_INTENSITY8_EXT                                 = $804B;
  {$EXTERNALSYM GL_INTENSITY8_EXT}
  GL_INTENSITY12_EXT                                = $804C;
  {$EXTERNALSYM GL_INTENSITY12_EXT}
  GL_INTENSITY16_EXT                                = $804D;
  {$EXTERNALSYM GL_INTENSITY16_EXT}
  GL_RGB2_EXT                                       = $804E;
  {$EXTERNALSYM GL_RGB2_EXT}
  GL_RGB4_EXT                                       = $804F;
  {$EXTERNALSYM GL_RGB4_EXT}
  GL_RGB5_EXT                                       = $8050;
  {$EXTERNALSYM GL_RGB5_EXT}
  GL_RGB8_EXT                                       = $8051;
  {$EXTERNALSYM GL_RGB8_EXT}
  GL_RGB10_EXT                                      = $8052;
  {$EXTERNALSYM GL_RGB10_EXT}
  GL_RGB12_EXT                                      = $8053;
  {$EXTERNALSYM GL_RGB12_EXT}
  GL_RGB16_EXT                                      = $8054;
  {$EXTERNALSYM GL_RGB16_EXT}
  GL_RGBA2_EXT                                      = $8055;
  {$EXTERNALSYM GL_RGBA2_EXT}
  GL_RGBA4_EXT                                      = $8056;
  {$EXTERNALSYM GL_RGBA4_EXT}
  GL_RGB5_A1_EXT                                    = $8057;
  {$EXTERNALSYM GL_RGB5_A1_EXT}
  GL_RGBA8_EXT                                      = $8058;
  {$EXTERNALSYM GL_RGBA8_EXT}
  GL_RGB10_A2_EXT                                   = $8059;
  {$EXTERNALSYM GL_RGB10_A2_EXT}
  GL_RGBA12_EXT                                     = $805A;
  {$EXTERNALSYM GL_RGBA12_EXT}
  GL_RGBA16_EXT                                     = $805B;
  {$EXTERNALSYM GL_RGBA16_EXT}
  GL_TEXTURE_RED_SIZE_EXT                           = $805C;
  {$EXTERNALSYM GL_TEXTURE_RED_SIZE_EXT}
  GL_TEXTURE_GREEN_SIZE_EXT                         = $805D;
  {$EXTERNALSYM GL_TEXTURE_GREEN_SIZE_EXT}
  GL_TEXTURE_BLUE_SIZE_EXT                          = $805E;
  {$EXTERNALSYM GL_TEXTURE_BLUE_SIZE_EXT}
  GL_TEXTURE_ALPHA_SIZE_EXT                         = $805F;
  {$EXTERNALSYM GL_TEXTURE_ALPHA_SIZE_EXT}
  GL_TEXTURE_LUMINANCE_SIZE_EXT                     = $8060;
  {$EXTERNALSYM GL_TEXTURE_LUMINANCE_SIZE_EXT}
  GL_TEXTURE_INTENSITY_SIZE_EXT                     = $8061;
  {$EXTERNALSYM GL_TEXTURE_INTENSITY_SIZE_EXT}
  GL_REPLACE_EXT                                    = $8062;
  {$EXTERNALSYM GL_REPLACE_EXT}
  GL_PROXY_TEXTURE_1D_EXT                           = $8063;
  {$EXTERNALSYM GL_PROXY_TEXTURE_1D_EXT}
  GL_PROXY_TEXTURE_2D_EXT                           = $8064;
  {$EXTERNALSYM GL_PROXY_TEXTURE_2D_EXT}
  GL_TEXTURE_TOO_LARGE_EXT                          = $8065;
  {$EXTERNALSYM GL_TEXTURE_TOO_LARGE_EXT}

  // EXT_texture_object
  GL_TEXTURE_PRIORITY_EXT                           = $8066;
  {$EXTERNALSYM GL_TEXTURE_PRIORITY_EXT}
  GL_TEXTURE_RESIDENT_EXT                           = $8067;
  {$EXTERNALSYM GL_TEXTURE_RESIDENT_EXT}
  GL_TEXTURE_1D_BINDING_EXT                         = $8068;
  {$EXTERNALSYM GL_TEXTURE_1D_BINDING_EXT}
  GL_TEXTURE_2D_BINDING_EXT                         = $8069;
  {$EXTERNALSYM GL_TEXTURE_2D_BINDING_EXT}
  GL_TEXTURE_3D_BINDING_EXT                         = $806A;
  {$EXTERNALSYM GL_TEXTURE_3D_BINDING_EXT}

  // EXT_texture3D
  GL_PACK_SKIP_IMAGES_EXT                           = $806B;
  {$EXTERNALSYM GL_PACK_SKIP_IMAGES_EXT}
  GL_PACK_IMAGE_HEIGHT_EXT                          = $806C;
  {$EXTERNALSYM GL_PACK_IMAGE_HEIGHT_EXT}
  GL_UNPACK_SKIP_IMAGES_EXT                         = $806D;
  {$EXTERNALSYM GL_UNPACK_SKIP_IMAGES_EXT}
  GL_UNPACK_IMAGE_HEIGHT_EXT                        = $806E;
  {$EXTERNALSYM GL_UNPACK_IMAGE_HEIGHT_EXT}
  GL_TEXTURE_3D_EXT                                 = $806F;
  {$EXTERNALSYM GL_TEXTURE_3D_EXT}
  GL_PROXY_TEXTURE_3D_EXT                           = $8070;
  {$EXTERNALSYM GL_PROXY_TEXTURE_3D_EXT}
  GL_TEXTURE_DEPTH_EXT                              = $8071;
  {$EXTERNALSYM GL_TEXTURE_DEPTH_EXT}
  GL_TEXTURE_WRAP_R_EXT                             = $8072;
  {$EXTERNALSYM GL_TEXTURE_WRAP_R_EXT}
  GL_MAX_3D_TEXTURE_SIZE_EXT                        = $8073;
  {$EXTERNALSYM GL_MAX_3D_TEXTURE_SIZE_EXT}

  // SGI_color_matrix
  GL_COLOR_MATRIX_SGI                               = $80B1;
  {$EXTERNALSYM GL_COLOR_MATRIX_SGI}
  GL_COLOR_MATRIX_STACK_DEPTH_SGI                   = $80B2;
  {$EXTERNALSYM GL_COLOR_MATRIX_STACK_DEPTH_SGI}
  GL_MAX_COLOR_MATRIX_STACK_DEPTH_SGI               = $80B3;
  {$EXTERNALSYM GL_MAX_COLOR_MATRIX_STACK_DEPTH_SGI}
  GL_POST_COLOR_MATRIX_RED_SCALE_SGI                = $80B4;
  {$EXTERNALSYM GL_POST_COLOR_MATRIX_RED_SCALE_SGI}
  GL_POST_COLOR_MATRIX_GREEN_SCALE_SGI              = $80B5;
  {$EXTERNALSYM GL_POST_COLOR_MATRIX_GREEN_SCALE_SGI}
  GL_POST_COLOR_MATRIX_BLUE_SCALE_SGI               = $80B6;
  {$EXTERNALSYM GL_POST_COLOR_MATRIX_BLUE_SCALE_SGI}
  GL_POST_COLOR_MATRIX_ALPHA_SCALE_SGI              = $80B7;
  {$EXTERNALSYM GL_POST_COLOR_MATRIX_ALPHA_SCALE_SGI}
  GL_POST_COLOR_MATRIX_RED_BIAS_SGI                 = $80B8;
  {$EXTERNALSYM GL_POST_COLOR_MATRIX_RED_BIAS_SGI}
  GL_POST_COLOR_MATRIX_GREEN_BIAS_SGI               = $80B9;
  {$EXTERNALSYM GL_POST_COLOR_MATRIX_GREEN_BIAS_SGI}
  GL_POST_COLOR_MATRIX_BLUE_BIAS_SGI                = $80BA;
  {$EXTERNALSYM GL_POST_COLOR_MATRIX_BLUE_BIAS_SGI}
  GL_POST_COLOR_MATRIX_ALPHA_BIAS_SGI               = $80BB;
  {$EXTERNALSYM GL_POST_COLOR_MATRIX_ALPHA_BIAS_SGI}

  // SGI_texture_color_table
  GL_TEXTURE_COLOR_TABLE_SGI                        = $80BC;
  {$EXTERNALSYM GL_TEXTURE_COLOR_TABLE_SGI}
  GL_PROXY_TEXTURE_COLOR_TABLE_SGI                  = $80BD;
  {$EXTERNALSYM GL_PROXY_TEXTURE_COLOR_TABLE_SGI}
  GL_TEXTURE_COLOR_TABLE_BIAS_SGI                   = $80BE;
  {$EXTERNALSYM GL_TEXTURE_COLOR_TABLE_BIAS_SGI}
  GL_TEXTURE_COLOR_TABLE_SCALE_SGI                  = $80BF;
  {$EXTERNALSYM GL_TEXTURE_COLOR_TABLE_SCALE_SGI}

  // SGI_color_table
  GL_COLOR_TABLE_SGI                                = $80D0;
  {$EXTERNALSYM GL_COLOR_TABLE_SGI}
  GL_POST_CONVOLUTION_COLOR_TABLE_SGI               = $80D1;
  {$EXTERNALSYM GL_POST_CONVOLUTION_COLOR_TABLE_SGI}
  GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI              = $80D2;
  {$EXTERNALSYM GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI}
  GL_PROXY_COLOR_TABLE_SGI                          = $80D3;
  {$EXTERNALSYM GL_PROXY_COLOR_TABLE_SGI}
  GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI         = $80D4;
  {$EXTERNALSYM GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI}
  GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI        = $80D5;
  {$EXTERNALSYM GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI}
  GL_COLOR_TABLE_SCALE_SGI                          = $80D6;
  {$EXTERNALSYM GL_COLOR_TABLE_SCALE_SGI}
  GL_COLOR_TABLE_BIAS_SGI                           = $80D7;
  {$EXTERNALSYM GL_COLOR_TABLE_BIAS_SGI}
  GL_COLOR_TABLE_FORMAT_SGI                         = $80D8;
  {$EXTERNALSYM GL_COLOR_TABLE_FORMAT_SGI}
  GL_COLOR_TABLE_WIDTH_SGI                          = $80D9;
  {$EXTERNALSYM GL_COLOR_TABLE_WIDTH_SGI}
  GL_COLOR_TABLE_RED_SIZE_SGI                       = $80DA;
  {$EXTERNALSYM GL_COLOR_TABLE_RED_SIZE_SGI}
  GL_COLOR_TABLE_GREEN_SIZE_SGI                     = $80DB;
  {$EXTERNALSYM GL_COLOR_TABLE_GREEN_SIZE_SGI}
  GL_COLOR_TABLE_BLUE_SIZE_SGI                      = $80DC;
  {$EXTERNALSYM GL_COLOR_TABLE_BLUE_SIZE_SGI}
  GL_COLOR_TABLE_ALPHA_SIZE_SGI                     = $80DD;
  {$EXTERNALSYM GL_COLOR_TABLE_ALPHA_SIZE_SGI}
  GL_COLOR_TABLE_LUMINANCE_SIZE_SGI                 = $80DE;
  {$EXTERNALSYM GL_COLOR_TABLE_LUMINANCE_SIZE_SGI}
  GL_COLOR_TABLE_INTENSITY_SIZE_SGI                 = $80DF;
  {$EXTERNALSYM GL_COLOR_TABLE_INTENSITY_SIZE_SGI}

  // EXT_cmyka
  GL_CMYK_EXT                                       = $800C;
  {$EXTERNALSYM GL_CMYK_EXT}
  GL_CMYKA_EXT                                      = $800D;
  {$EXTERNALSYM GL_CMYKA_EXT}
  GL_PACK_CMYK_HINT_EXT                             = $800E;
  {$EXTERNALSYM GL_PACK_CMYK_HINT_EXT}
  GL_UNPACK_CMYK_HINT_EXT                           = $800F;
  {$EXTERNALSYM GL_UNPACK_CMYK_HINT_EXT}

  // EXT_rescale_normal
  GL_RESCALE_NORMAL_EXT                             = $803A;
  {$EXTERNALSYM GL_RESCALE_NORMAL_EXT}

  // EXT_clip_volume_hint
  GL_CLIP_VOLUME_CLIPPING_HINT_EXT	                = $80F0;
  {$EXTERNALSYM GL_CLIP_VOLUME_CLIPPING_HINT_EXT}

  // EXT_cull_vertex
  GL_CULL_VERTEX_EXT                                = $81AA;
  {$EXTERNALSYM GL_CULL_VERTEX_EXT}
  GL_CULL_VERTEX_EYE_POSITION_EXT                   = $81AB;
  {$EXTERNALSYM GL_CULL_VERTEX_EYE_POSITION_EXT}
  GL_CULL_VERTEX_OBJECT_POSITION_EXT                = $81AC;
  {$EXTERNALSYM GL_CULL_VERTEX_OBJECT_POSITION_EXT}

  // EXT_index_array_formats
  GL_IUI_V2F_EXT                                    = $81AD;
  {$EXTERNALSYM GL_IUI_V2F_EXT}
  GL_IUI_V3F_EXT                                    = $81AE;
  {$EXTERNALSYM GL_IUI_V3F_EXT}
  GL_IUI_N3F_V2F_EXT                                = $81AF;
  {$EXTERNALSYM GL_IUI_N3F_V2F_EXT}
  GL_IUI_N3F_V3F_EXT                                = $81B0;
  {$EXTERNALSYM GL_IUI_N3F_V3F_EXT}
  GL_T2F_IUI_V2F_EXT                                = $81B1;
  {$EXTERNALSYM GL_T2F_IUI_V2F_EXT}
  GL_T2F_IUI_V3F_EXT                                = $81B2;
  {$EXTERNALSYM GL_T2F_IUI_V3F_EXT}
  GL_T2F_IUI_N3F_V2F_EXT                            = $81B3;
  {$EXTERNALSYM GL_T2F_IUI_N3F_V2F_EXT}
  GL_T2F_IUI_N3F_V3F_EXT                            = $81B4;
  {$EXTERNALSYM GL_T2F_IUI_N3F_V3F_EXT}

  // EXT_index_func
  GL_INDEX_TEST_EXT                                 = $81B5;
  {$EXTERNALSYM GL_INDEX_TEST_EXT}
  GL_INDEX_TEST_FUNC_EXT                            = $81B6;
  {$EXTERNALSYM GL_INDEX_TEST_FUNC_EXT}
  GL_INDEX_TEST_REF_EXT                             = $81B7;
  {$EXTERNALSYM GL_INDEX_TEST_REF_EXT}

  // EXT_index_material
  GL_INDEX_MATERIAL_EXT                             = $81B8;
  {$EXTERNALSYM GL_INDEX_MATERIAL_EXT}
  GL_INDEX_MATERIAL_PARAMETER_EXT                   = $81B9;
  {$EXTERNALSYM GL_INDEX_MATERIAL_PARAMETER_EXT}
  GL_INDEX_MATERIAL_FACE_EXT                        = $81BA;
  {$EXTERNALSYM GL_INDEX_MATERIAL_FACE_EXT}

  // EXT_misc_attribute
  GL_MISC_BIT_EXT                                   = 0; // not yet defined
  {$EXTERNALSYM GL_MISC_BIT_EXT}

  // EXT_scene_marker
  GL_SCENE_REQUIRED_EXT                             = 0; // not yet defined
  {$EXTERNALSYM GL_SCENE_REQUIRED_EXT}

  // EXT_shared_texture_palette
  GL_SHARED_TEXTURE_PALETTE_EXT                     = $81FB;
  {$EXTERNALSYM GL_SHARED_TEXTURE_PALETTE_EXT}

  // EXT_nurbs_tessellator
  GLU_NURBS_MODE_EXT                                = 100160;
  {$EXTERNALSYM GLU_NURBS_MODE_EXT}
  GLU_NURBS_TESSELLATOR_EXT                         = 100161;
  {$EXTERNALSYM GLU_NURBS_TESSELLATOR_EXT}
  GLU_NURBS_RENDERER_EXT                            = 100162;
  {$EXTERNALSYM GLU_NURBS_RENDERER_EXT}
  GLU_NURBS_BEGIN_EXT                               = 100164;
  {$EXTERNALSYM GLU_NURBS_BEGIN_EXT}
  GLU_NURBS_VERTEX_EXT                              = 100165;
  {$EXTERNALSYM GLU_NURBS_VERTEX_EXT}
  GLU_NURBS_NORMAL_EXT                              = 100166;
  {$EXTERNALSYM GLU_NURBS_NORMAL_EXT}
  GLU_NURBS_COLOR_EXT                               = 100167;
  {$EXTERNALSYM GLU_NURBS_COLOR_EXT}
  GLU_NURBS_TEX_COORD_EXT                           = 100168;
  {$EXTERNALSYM GLU_NURBS_TEX_COORD_EXT}
  GLU_NURBS_END_EXT                                 = 100169;
  {$EXTERNALSYM GLU_NURBS_END_EXT}
  GLU_NURBS_BEGIN_DATA_EXT                          = 100170;
  {$EXTERNALSYM GLU_NURBS_BEGIN_DATA_EXT}
  GLU_NURBS_VERTEX_DATA_EXT                         = 100171;
  {$EXTERNALSYM GLU_NURBS_VERTEX_DATA_EXT}
  GLU_NURBS_NORMAL_DATA_EXT                         = 100172;
  {$EXTERNALSYM GLU_NURBS_NORMAL_DATA_EXT}
  GLU_NURBS_COLOR_DATA_EXT                          = 100173;
  {$EXTERNALSYM GLU_NURBS_COLOR_DATA_EXT}
  GLU_NURBS_TEX_COORD_DATA_EXT                      = 100174;
  {$EXTERNALSYM GLU_NURBS_TEX_COORD_DATA_EXT}
  GLU_NURBS_END_DATA_EXT                            = 100175;
  {$EXTERNALSYM GLU_NURBS_END_DATA_EXT}

  // EXT_object_space_tess
  GLU_OBJECT_PARAMETRIC_ERROR_EXT                   = 100208;
  {$EXTERNALSYM GLU_OBJECT_PARAMETRIC_ERROR_EXT}
  GLU_OBJECT_PATH_LENGTH_EXT                        = 100209;
  {$EXTERNALSYM GLU_OBJECT_PATH_LENGTH_EXT}

  // EXT_point_parameters
  GL_POINT_SIZE_MIN_EXT                             = $8126;
  {$EXTERNALSYM GL_POINT_SIZE_MIN_EXT}
  GL_POINT_SIZE_MAX_EXT                             = $8127;
  {$EXTERNALSYM GL_POINT_SIZE_MAX_EXT}
  GL_POINT_FADE_THRESHOLD_SIZE_EXT                  = $8128;
  {$EXTERNALSYM GL_POINT_FADE_THRESHOLD_SIZE_EXT}
  GL_DISTANCE_ATTENUATION_EXT                       = $8129;
  {$EXTERNALSYM GL_DISTANCE_ATTENUATION_EXT}

  // EXT_compiled_vertex_array
  GL_ARRAY_ELEMENT_LOCK_FIRST_EXT                   = $81A8;
  {$EXTERNALSYM GL_ARRAY_ELEMENT_LOCK_FIRST_EXT}
  GL_ARRAY_ELEMENT_LOCK_COUNT_EXT                   = $81A9;
  {$EXTERNALSYM GL_ARRAY_ELEMENT_LOCK_COUNT_EXT}

  // ARB_multitexture
  GL_ACTIVE_TEXTURE_ARB                             = $84E0;
  {$EXTERNALSYM GL_ACTIVE_TEXTURE_ARB}
  GL_CLIENT_ACTIVE_TEXTURE_ARB                      = $84E1;
  {$EXTERNALSYM GL_CLIENT_ACTIVE_TEXTURE_ARB}
  GL_MAX_TEXTURE_UNITS_ARB                          = $84E2;
  {$EXTERNALSYM GL_MAX_TEXTURE_UNITS_ARB}
  GL_TEXTURE0_ARB                                   = $84C0;
  {$EXTERNALSYM GL_TEXTURE0_ARB}
  GL_TEXTURE1_ARB                                   = $84C1;
  {$EXTERNALSYM GL_TEXTURE1_ARB}
  GL_TEXTURE2_ARB                                   = $84C2;
  {$EXTERNALSYM GL_TEXTURE2_ARB}
  GL_TEXTURE3_ARB                                   = $84C3;
  {$EXTERNALSYM GL_TEXTURE3_ARB}
  GL_TEXTURE4_ARB                                   = $84C4;
  {$EXTERNALSYM GL_TEXTURE4_ARB}
  GL_TEXTURE5_ARB                                   = $84C5;
  {$EXTERNALSYM GL_TEXTURE5_ARB}
  GL_TEXTURE6_ARB                                   = $84C6;
  {$EXTERNALSYM GL_TEXTURE6_ARB}
  GL_TEXTURE7_ARB                                   = $84C7;
  {$EXTERNALSYM GL_TEXTURE7_ARB}
  GL_TEXTURE8_ARB                                   = $84C8;
  {$EXTERNALSYM GL_TEXTURE8_ARB}
  GL_TEXTURE9_ARB                                   = $84C9;
  {$EXTERNALSYM GL_TEXTURE9_ARB}
  GL_TEXTURE10_ARB                                  = $84CA;
  {$EXTERNALSYM GL_TEXTURE10_ARB}
  GL_TEXTURE11_ARB                                  = $84CB;
  {$EXTERNALSYM GL_TEXTURE11_ARB}
  GL_TEXTURE12_ARB                                  = $84CC;
  {$EXTERNALSYM GL_TEXTURE12_ARB}
  GL_TEXTURE13_ARB                                  = $84CD;
  {$EXTERNALSYM GL_TEXTURE13_ARB}
  GL_TEXTURE14_ARB                                  = $84CE;
  {$EXTERNALSYM GL_TEXTURE14_ARB}
  GL_TEXTURE15_ARB                                  = $84CF;
  {$EXTERNALSYM GL_TEXTURE15_ARB}
  GL_TEXTURE16_ARB                                  = $84D0;
  {$EXTERNALSYM GL_TEXTURE16_ARB}
  GL_TEXTURE17_ARB                                  = $84D1;
  {$EXTERNALSYM GL_TEXTURE17_ARB}
  GL_TEXTURE18_ARB                                  = $84D2;
  {$EXTERNALSYM GL_TEXTURE18_ARB}
  GL_TEXTURE19_ARB                                  = $84D3;
  {$EXTERNALSYM GL_TEXTURE19_ARB}
  GL_TEXTURE20_ARB                                  = $84D4;
  {$EXTERNALSYM GL_TEXTURE20_ARB}
  GL_TEXTURE21_ARB                                  = $84D5;
  {$EXTERNALSYM GL_TEXTURE21_ARB}
  GL_TEXTURE22_ARB                                  = $84D6;
  {$EXTERNALSYM GL_TEXTURE22_ARB}
  GL_TEXTURE23_ARB                                  = $84D7;
  {$EXTERNALSYM GL_TEXTURE23_ARB}
  GL_TEXTURE24_ARB                                  = $84D8;
  {$EXTERNALSYM GL_TEXTURE24_ARB}
  GL_TEXTURE25_ARB                                  = $84D9;
  {$EXTERNALSYM GL_TEXTURE25_ARB}
  GL_TEXTURE26_ARB                                  = $84DA;
  {$EXTERNALSYM GL_TEXTURE26_ARB}
  GL_TEXTURE27_ARB                                  = $84DB;
  {$EXTERNALSYM GL_TEXTURE27_ARB}
  GL_TEXTURE28_ARB                                  = $84DC;
  {$EXTERNALSYM GL_TEXTURE28_ARB}
  GL_TEXTURE29_ARB                                  = $84DD;
  {$EXTERNALSYM GL_TEXTURE29_ARB}
  GL_TEXTURE30_ARB                                  = $84DE;
  {$EXTERNALSYM GL_TEXTURE30_ARB}
  GL_TEXTURE31_ARB                                  = $84DF;
  {$EXTERNALSYM GL_TEXTURE31_ARB}

  // EXT_stencil_wrap
  GL_INCR_WRAP_EXT                                  = $8507;
  {$EXTERNALSYM GL_INCR_WRAP_EXT}
  GL_DECR_WRAP_EXT                                  = $8508;
  {$EXTERNALSYM GL_DECR_WRAP_EXT}

  // NV_texgen_reflection
  GL_NORMAL_MAP_NV                                  = $8511;
  {$EXTERNALSYM GL_NORMAL_MAP_NV}
  GL_REFLECTION_MAP_NV                              = $8512;
  {$EXTERNALSYM GL_REFLECTION_MAP_NV}

  // EXT_texture_env_combine
  GL_COMBINE_EXT                                    = $8570;
  {$EXTERNALSYM GL_COMBINE_EXT}
  GL_COMBINE_RGB_EXT                                = $8571;
  {$EXTERNALSYM GL_COMBINE_RGB_EXT}
  GL_COMBINE_ALPHA_EXT                              = $8572;
  {$EXTERNALSYM GL_COMBINE_ALPHA_EXT}
  GL_RGB_SCALE_EXT                                  = $8573;
  {$EXTERNALSYM GL_RGB_SCALE_EXT}
  GL_ADD_SIGNED_EXT                                 = $8574;
  {$EXTERNALSYM GL_ADD_SIGNED_EXT}
  GL_INTERPOLATE_EXT                                = $8575;
  {$EXTERNALSYM GL_INTERPOLATE_EXT}
  GL_CONSTANT_EXT                                   = $8576;
  {$EXTERNALSYM GL_CONSTANT_EXT}
  GL_PRIMARY_COLOR_EXT                              = $8577;
  {$EXTERNALSYM GL_PRIMARY_COLOR_EXT}
  GL_PREVIOUS_EXT                                   = $8578;
  {$EXTERNALSYM GL_PREVIOUS_EXT}
  GL_SOURCE0_RGB_EXT                                = $8580;
  {$EXTERNALSYM GL_SOURCE0_RGB_EXT}
  GL_SOURCE1_RGB_EXT                                = $8581;
  {$EXTERNALSYM GL_SOURCE1_RGB_EXT}
  GL_SOURCE2_RGB_EXT                                = $8582;
  {$EXTERNALSYM GL_SOURCE2_RGB_EXT}
  GL_SOURCE0_ALPHA_EXT                              = $8588;
  {$EXTERNALSYM GL_SOURCE0_ALPHA_EXT}
  GL_SOURCE1_ALPHA_EXT                              = $8589;
  {$EXTERNALSYM GL_SOURCE1_ALPHA_EXT}
  GL_SOURCE2_ALPHA_EXT                              = $858A;
  {$EXTERNALSYM GL_SOURCE2_ALPHA_EXT}
  GL_OPERAND0_RGB_EXT                               = $8590;
  {$EXTERNALSYM GL_OPERAND0_RGB_EXT}
  GL_OPERAND1_RGB_EXT                               = $8591;
  {$EXTERNALSYM GL_OPERAND1_RGB_EXT}
  GL_OPERAND2_RGB_EXT                               = $8592;
  {$EXTERNALSYM GL_OPERAND2_RGB_EXT}
  GL_OPERAND0_ALPHA_EXT                             = $8598;
  {$EXTERNALSYM GL_OPERAND0_ALPHA_EXT}
  GL_OPERAND1_ALPHA_EXT                             = $8599;
  {$EXTERNALSYM GL_OPERAND1_ALPHA_EXT}
  GL_OPERAND2_ALPHA_EXT                             = $859A;
  {$EXTERNALSYM GL_OPERAND2_ALPHA_EXT}

  // NV_texture_env_combine4
  GL_COMBINE4_NV                                    = $8503;
  {$EXTERNALSYM GL_COMBINE4_NV}
  GL_SOURCE3_RGB_NV                                 = $8583;
  {$EXTERNALSYM GL_SOURCE3_RGB_NV}
  GL_SOURCE3_ALPHA_NV                               = $858B;
  {$EXTERNALSYM GL_SOURCE3_ALPHA_NV}
  GL_OPERAND3_RGB_NV                                = $8593;
  {$EXTERNALSYM GL_OPERAND3_RGB_NV}
  GL_OPERAND3_ALPHA_NV                              = $859B;
  {$EXTERNALSYM GL_OPERAND3_ALPHA_NV}

  GL_BLEND_EQUATION                                 = $8009;
  {$EXTERNALSYM GL_BLEND_EQUATION}
  GL_TABLE_TOO_LARGE                                = $8031;
  {$EXTERNALSYM GL_TABLE_TOO_LARGE}
  GL_UNSIGNED_BYTE_3_3_2                            = $8032;
  {$EXTERNALSYM GL_UNSIGNED_BYTE_3_3_2}
  GL_UNSIGNED_SHORT_4_4_4_4                         = $8033;
  {$EXTERNALSYM GL_UNSIGNED_SHORT_4_4_4_4}
  GL_UNSIGNED_SHORT_5_5_5_1                         = $8034;
  {$EXTERNALSYM GL_UNSIGNED_SHORT_5_5_5_1}
  GL_UNSIGNED_INT_8_8_8_8                           = $8035;
  {$EXTERNALSYM GL_UNSIGNED_INT_8_8_8_8}
  GL_UNSIGNED_INT_10_10_10_2                        = $8036;
  {$EXTERNALSYM GL_UNSIGNED_INT_10_10_10_2}
  GL_UNSIGNED_BYTE_2_3_3_REV                        = $8362;
  {$EXTERNALSYM GL_UNSIGNED_BYTE_2_3_3_REV}
  GL_UNSIGNED_SHORT_5_6_5                           = $8363;
  {$EXTERNALSYM GL_UNSIGNED_SHORT_5_6_5}
  GL_UNSIGNED_SHORT_5_6_5_REV                       = $8364;
  {$EXTERNALSYM GL_UNSIGNED_SHORT_5_6_5_REV}
  GL_UNSIGNED_SHORT_4_4_4_4_REV                     = $8365;
  {$EXTERNALSYM GL_UNSIGNED_SHORT_4_4_4_4_REV}
  GL_UNSIGNED_SHORT_1_5_5_5_REV                     = $8366;
  {$EXTERNALSYM GL_UNSIGNED_SHORT_1_5_5_5_REV}
  GL_UNSIGNED_INT_8_8_8_8_REV                       = $8367;
  {$EXTERNALSYM GL_UNSIGNED_INT_8_8_8_8_REV}
  GL_UNSIGNED_INT_2_10_10_10_REV                    = $8368;
  {$EXTERNALSYM GL_UNSIGNED_INT_2_10_10_10_REV}

  // GL_ARB_transpose_matrix
  GL_TRANSPOSE_MODELVIEW_MATRIX_ARB                 = $84E3;
  {$EXTERNALSYM GL_TRANSPOSE_MODELVIEW_MATRIX_ARB}
  GL_TRANSPOSE_PROJECTION_MATRIX_ARB                = $84E4;
  {$EXTERNALSYM GL_TRANSPOSE_PROJECTION_MATRIX_ARB}
  GL_TRANSPOSE_TEXTURE_MATRIX_ARB                   = $84E5;
  {$EXTERNALSYM GL_TRANSPOSE_TEXTURE_MATRIX_ARB}
  GL_TRANSPOSE_COLOR_MATRIX_ARB                     = $84E6;
  {$EXTERNALSYM GL_TRANSPOSE_COLOR_MATRIX_ARB}

  // GL_ARB_multisample
  GL_MULTISAMPLE_ARB                                = $809D;
  {$EXTERNALSYM GL_MULTISAMPLE_ARB}
  GL_SAMPLE_ALPHA_TO_COVERAGE_ARB                   = $809E;
  {$EXTERNALSYM GL_SAMPLE_ALPHA_TO_COVERAGE_ARB}
  GL_SAMPLE_ALPHA_TO_ONE_ARB                        = $809F;
  {$EXTERNALSYM GL_SAMPLE_ALPHA_TO_ONE_ARB}
  GL_SAMPLE_COVERAGE_ARB                            = $80A0;
  {$EXTERNALSYM GL_SAMPLE_COVERAGE_ARB}
  GL_SAMPLE_BUFFERS_ARB                             = $80A8;
  {$EXTERNALSYM GL_SAMPLE_BUFFERS_ARB}
  GL_SAMPLES_ARB                                    = $80A9;
  {$EXTERNALSYM GL_SAMPLES_ARB}
  GL_SAMPLE_COVERAGE_VALUE_ARB                      = $80AA;
  {$EXTERNALSYM GL_SAMPLE_COVERAGE_VALUE_ARB}
  GL_SAMPLE_COVERAGE_INVERT_ARB                     = $80AB;
  {$EXTERNALSYM GL_SAMPLE_COVERAGE_INVERT_ARB}
  GL_MULTISAMPLE_BIT_ARB                            = $20000000;
  {$EXTERNALSYM GL_MULTISAMPLE_BIT_ARB}
  GLX_SAMPLE_BUFFERS_ARB                            = 100000;
  {$EXTERNALSYM GLX_SAMPLE_BUFFERS_ARB}
  GLX_SAMPLES_ARB                                   = 100001;
  {$EXTERNALSYM GLX_SAMPLES_ARB}
  WGL_SAMPLE_BUFFERS_ARB                            = $2041;
  {$EXTERNALSYM WGL_SAMPLE_BUFFERS_ARB}
  WGL_SAMPLES_ARB                                   = $2042;
  {$EXTERNALSYM WGL_SAMPLES_ARB}

  // GL_ARB_texture_cube_map
  GL_NORMAL_MAP_ARB                                 = $8511;
  {$EXTERNALSYM GL_NORMAL_MAP_ARB}
  GL_REFLECTION_MAP_ARB                             = $8512;
  {$EXTERNALSYM GL_REFLECTION_MAP_ARB}
  GL_TEXTURE_CUBE_MAP_ARB                           = $8513;
  {$EXTERNALSYM GL_TEXTURE_CUBE_MAP_ARB}
  GL_TEXTURE_BINDING_CUBE_MAP_ARB                   = $8514;
  {$EXTERNALSYM GL_TEXTURE_BINDING_CUBE_MAP_ARB}
  GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB                = $8515;
  {$EXTERNALSYM GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB}
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB                = $8516;
  {$EXTERNALSYM GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB}
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB                = $8517;
  {$EXTERNALSYM GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB}
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB                = $8518;
  {$EXTERNALSYM GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB}
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB                = $8519;
  {$EXTERNALSYM GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB}
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB                = $851A;
  {$EXTERNALSYM GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB}
  GL_PROXY_TEXTURE_CUBE_MAP_ARB                     = $851B;
  {$EXTERNALSYM GL_PROXY_TEXTURE_CUBE_MAP_ARB}
  GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB                  = $851C;
  {$EXTERNALSYM GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB}

  // GL_ARB_texture_compression
  GL_COMPRESSED_ALPHA_ARB                           = $84E9;
  {$EXTERNALSYM GL_COMPRESSED_ALPHA_ARB}
  GL_COMPRESSED_LUMINANCE_ARB                       = $84EA;
  {$EXTERNALSYM GL_COMPRESSED_LUMINANCE_ARB}
  GL_COMPRESSED_LUMINANCE_ALPHA_ARB                 = $84EB;
  {$EXTERNALSYM GL_COMPRESSED_LUMINANCE_ALPHA_ARB}
  GL_COMPRESSED_INTENSITY_ARB                       = $84EC;
  {$EXTERNALSYM GL_COMPRESSED_INTENSITY_ARB}
  GL_COMPRESSED_RGB_ARB                             = $84ED;
  {$EXTERNALSYM GL_COMPRESSED_RGB_ARB}
  GL_COMPRESSED_RGBA_ARB                            = $84EE;
  {$EXTERNALSYM GL_COMPRESSED_RGBA_ARB}
  GL_TEXTURE_COMPRESSION_HINT_ARB                   = $84EF;
  {$EXTERNALSYM GL_TEXTURE_COMPRESSION_HINT_ARB}
  GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB              = $86A0;
  {$EXTERNALSYM GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB}
  GL_TEXTURE_COMPRESSED_ARB                         = $86A1;
  {$EXTERNALSYM GL_TEXTURE_COMPRESSED_ARB}
  GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB             = $86A2;
  {$EXTERNALSYM GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB}
  GL_COMPRESSED_TEXTURE_FORMATS_ARB                 = $86A3;
  {$EXTERNALSYM GL_COMPRESSED_TEXTURE_FORMATS_ARB}

  // GL_ARB_vertex_blend
  GL_MAX_VERTEX_UNITS_ARB                           = $86A4;
  {$EXTERNALSYM GL_MAX_VERTEX_UNITS_ARB}
  GL_ACTIVE_VERTEX_UNITS_ARB                        = $86A5;
  {$EXTERNALSYM GL_ACTIVE_VERTEX_UNITS_ARB}
  GL_WEIGHT_SUM_UNITY_ARB                           = $86A6;
  {$EXTERNALSYM GL_WEIGHT_SUM_UNITY_ARB}
  GL_VERTEX_BLEND_ARB                               = $86A7;
  {$EXTERNALSYM GL_VERTEX_BLEND_ARB}
  GL_CURRENT_WEIGHT_ARB                             = $86A8;
  {$EXTERNALSYM GL_CURRENT_WEIGHT_ARB}
  GL_WEIGHT_ARRAY_TYPE_ARB                          = $86A9;
  {$EXTERNALSYM GL_WEIGHT_ARRAY_TYPE_ARB}
  GL_WEIGHT_ARRAY_STRIDE_ARB                        = $86AA;
  {$EXTERNALSYM GL_WEIGHT_ARRAY_STRIDE_ARB}
  GL_WEIGHT_ARRAY_SIZE_ARB                          = $86AB;
  {$EXTERNALSYM GL_WEIGHT_ARRAY_SIZE_ARB}
  GL_WEIGHT_ARRAY_POINTER_ARB                       = $86AC;
  {$EXTERNALSYM GL_WEIGHT_ARRAY_POINTER_ARB}
  GL_WEIGHT_ARRAY_ARB                               = $86AD;
  {$EXTERNALSYM GL_WEIGHT_ARRAY_ARB}
  GL_MODELVIEW0_ARB                                 = $1700;
  {$EXTERNALSYM GL_MODELVIEW0_ARB}
  GL_MODELVIEW1_ARB                                 = $850A;
  {$EXTERNALSYM GL_MODELVIEW1_ARB}
  GL_MODELVIEW2_ARB                                 = $8722;
  {$EXTERNALSYM GL_MODELVIEW2_ARB}
  GL_MODELVIEW3_ARB                                 = $8723;
  {$EXTERNALSYM GL_MODELVIEW3_ARB}
  GL_MODELVIEW4_ARB                                 = $8724;
  {$EXTERNALSYM GL_MODELVIEW4_ARB}
  GL_MODELVIEW5_ARB                                 = $8725;
  {$EXTERNALSYM GL_MODELVIEW5_ARB}
  GL_MODELVIEW6_ARB                                 = $8726;
  {$EXTERNALSYM GL_MODELVIEW6_ARB}
  GL_MODELVIEW7_ARB                                 = $8727;
  {$EXTERNALSYM GL_MODELVIEW7_ARB}
  GL_MODELVIEW8_ARB                                 = $8728;
  {$EXTERNALSYM GL_MODELVIEW8_ARB}
  GL_MODELVIEW9_ARB                                 = $8729;
  {$EXTERNALSYM GL_MODELVIEW9_ARB}
  GL_MODELVIEW10_ARB                                = $872A;
  {$EXTERNALSYM GL_MODELVIEW10_ARB}
  GL_MODELVIEW11_ARB                                = $872B;
  {$EXTERNALSYM GL_MODELVIEW11_ARB}
  GL_MODELVIEW12_ARB                                = $872C;
  {$EXTERNALSYM GL_MODELVIEW12_ARB}
  GL_MODELVIEW13_ARB                                = $872D;
  {$EXTERNALSYM GL_MODELVIEW13_ARB}
  GL_MODELVIEW14_ARB                                = $872E;
  {$EXTERNALSYM GL_MODELVIEW14_ARB}
  GL_MODELVIEW15_ARB                                = $872F;
  {$EXTERNALSYM GL_MODELVIEW15_ARB}
  GL_MODELVIEW16_ARB                                = $8730;
  {$EXTERNALSYM GL_MODELVIEW16_ARB}
  GL_MODELVIEW17_ARB                                = $8731;
  {$EXTERNALSYM GL_MODELVIEW17_ARB}
  GL_MODELVIEW18_ARB                                = $8732;
  {$EXTERNALSYM GL_MODELVIEW18_ARB}
  GL_MODELVIEW19_ARB                                = $8733;
  {$EXTERNALSYM GL_MODELVIEW19_ARB}
  GL_MODELVIEW20_ARB                                = $8734;
  {$EXTERNALSYM GL_MODELVIEW20_ARB}
  GL_MODELVIEW21_ARB                                = $8735;
  {$EXTERNALSYM GL_MODELVIEW21_ARB}
  GL_MODELVIEW22_ARB                                = $8736;
  {$EXTERNALSYM GL_MODELVIEW22_ARB}
  GL_MODELVIEW23_ARB                                = $8737;
  {$EXTERNALSYM GL_MODELVIEW23_ARB}
  GL_MODELVIEW24_ARB                                = $8738;
  {$EXTERNALSYM GL_MODELVIEW24_ARB}
  GL_MODELVIEW25_ARB                                = $8739;
  {$EXTERNALSYM GL_MODELVIEW25_ARB}
  GL_MODELVIEW26_ARB                                = $873A;
  {$EXTERNALSYM GL_MODELVIEW26_ARB}
  GL_MODELVIEW27_ARB                                = $873B;
  {$EXTERNALSYM GL_MODELVIEW27_ARB}
  GL_MODELVIEW28_ARB                                = $873C;
  {$EXTERNALSYM GL_MODELVIEW28_ARB}
  GL_MODELVIEW29_ARB                                = $873D;
  {$EXTERNALSYM GL_MODELVIEW29_ARB}
  GL_MODELVIEW30_ARB                                = $873E;
  {$EXTERNALSYM GL_MODELVIEW30_ARB}
  GL_MODELVIEW31_ARB                                = $873F;
  {$EXTERNALSYM GL_MODELVIEW31_ARB}

  // GL_SGIS_texture_filter4
  GL_FILTER4_SGIS                                   = $8146;
  {$EXTERNALSYM GL_FILTER4_SGIS}
  GL_TEXTURE_FILTER4_SIZE_SGIS                      = $8147;
  {$EXTERNALSYM GL_TEXTURE_FILTER4_SIZE_SGIS}

  // GL_SGIS_pixel_texture
  GL_PIXEL_TEXTURE_SGIS                             = $8353;
  {$EXTERNALSYM GL_PIXEL_TEXTURE_SGIS}
  GL_PIXEL_FRAGMENT_RGB_SOURCE_SGIS                 = $8354;
  {$EXTERNALSYM GL_PIXEL_FRAGMENT_RGB_SOURCE_SGIS}
  GL_PIXEL_FRAGMENT_ALPHA_SOURCE_SGIS               = $8355;
  {$EXTERNALSYM GL_PIXEL_FRAGMENT_ALPHA_SOURCE_SGIS}
  GL_PIXEL_GROUP_COLOR_SGIS                         = $8356;
  {$EXTERNALSYM GL_PIXEL_GROUP_COLOR_SGIS}

  // GL_SGIX_pixel_texture
  GL_PIXEL_TEX_GEN_SGIX                             = $8139;
  {$EXTERNALSYM GL_PIXEL_TEX_GEN_SGIX}
  GL_PIXEL_TEX_GEN_MODE_SGIX                        = $832B;
  {$EXTERNALSYM GL_PIXEL_TEX_GEN_MODE_SGIX}

  // GL_SGIS_texture4D
  GL_PACK_SKIP_VOLUMES_SGIS                         = $8130;
  {$EXTERNALSYM GL_PACK_SKIP_VOLUMES_SGIS}
  GL_PACK_IMAGE_DEPTH_SGIS                          = $8131;
  {$EXTERNALSYM GL_PACK_IMAGE_DEPTH_SGIS}
  GL_UNPACK_SKIP_VOLUMES_SGIS                       = $8132;
  {$EXTERNALSYM GL_UNPACK_SKIP_VOLUMES_SGIS}
  GL_UNPACK_IMAGE_DEPTH_SGIS                        = $8133;
  {$EXTERNALSYM GL_UNPACK_IMAGE_DEPTH_SGIS}
  GL_TEXTURE_4D_SGIS                                = $8134;
  {$EXTERNALSYM GL_TEXTURE_4D_SGIS}
  GL_PROXY_TEXTURE_4D_SGIS                          = $8135;
  {$EXTERNALSYM GL_PROXY_TEXTURE_4D_SGIS}
  GL_TEXTURE_4DSIZE_SGIS                            = $8136;
  {$EXTERNALSYM GL_TEXTURE_4DSIZE_SGIS}
  GL_TEXTURE_WRAP_Q_SGIS                            = $8137;
  {$EXTERNALSYM GL_TEXTURE_WRAP_Q_SGIS}
  GL_MAX_4D_TEXTURE_SIZE_SGIS                       = $8138;
  {$EXTERNALSYM GL_MAX_4D_TEXTURE_SIZE_SGIS}
  GL_TEXTURE_4D_BINDING_SGIS                        = $814F;
  {$EXTERNALSYM GL_TEXTURE_4D_BINDING_SGIS}

  // GL_SGIS_detail_texture
  GL_DETAIL_TEXTURE_2D_SGIS                         = $8095;
  {$EXTERNALSYM GL_DETAIL_TEXTURE_2D_SGIS}
  GL_DETAIL_TEXTURE_2D_BINDING_SGIS                 = $8096;
  {$EXTERNALSYM GL_DETAIL_TEXTURE_2D_BINDING_SGIS}
  GL_LINEAR_DETAIL_SGIS                             = $8097;
  {$EXTERNALSYM GL_LINEAR_DETAIL_SGIS}
  GL_LINEAR_DETAIL_ALPHA_SGIS                       = $8098;
  {$EXTERNALSYM GL_LINEAR_DETAIL_ALPHA_SGIS}
  GL_LINEAR_DETAIL_COLOR_SGIS                       = $8099;
  {$EXTERNALSYM GL_LINEAR_DETAIL_COLOR_SGIS}
  GL_DETAIL_TEXTURE_LEVEL_SGIS                      = $809A;
  {$EXTERNALSYM GL_DETAIL_TEXTURE_LEVEL_SGIS}
  GL_DETAIL_TEXTURE_MODE_SGIS                       = $809B;
  {$EXTERNALSYM GL_DETAIL_TEXTURE_MODE_SGIS}
  GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS                = $809C;
  {$EXTERNALSYM GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS}

  // GL_SGIS_sharpen_texture
  GL_LINEAR_SHARPEN_SGIS                            = $80AD;
  {$EXTERNALSYM GL_LINEAR_SHARPEN_SGIS}
  GL_LINEAR_SHARPEN_ALPHA_SGIS                      = $80AE;
  {$EXTERNALSYM GL_LINEAR_SHARPEN_ALPHA_SGIS}
  GL_LINEAR_SHARPEN_COLOR_SGIS                      = $80AF;
  {$EXTERNALSYM GL_LINEAR_SHARPEN_COLOR_SGIS}
  GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS               = $80B0;
  {$EXTERNALSYM GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS}

  // GL_SGIS_texture_lod
  GL_TEXTURE_MIN_LOD_SGIS                           = $813A;
  {$EXTERNALSYM GL_TEXTURE_MIN_LOD_SGIS}
  GL_TEXTURE_MAX_LOD_SGIS                           = $813B;
  {$EXTERNALSYM GL_TEXTURE_MAX_LOD_SGIS}
  GL_TEXTURE_BASE_LEVEL_SGIS                        = $813C;
  {$EXTERNALSYM GL_TEXTURE_BASE_LEVEL_SGIS}
  GL_TEXTURE_MAX_LEVEL_SGIS                         = $813D;
  {$EXTERNALSYM GL_TEXTURE_MAX_LEVEL_SGIS}

  // GL_SGIS_multisample
  GL_MULTISAMPLE_SGIS                               = $809D;
  {$EXTERNALSYM GL_MULTISAMPLE_SGIS}
  GL_SAMPLE_ALPHA_TO_MASK_SGIS                      = $809E;
  {$EXTERNALSYM GL_SAMPLE_ALPHA_TO_MASK_SGIS}
  GL_SAMPLE_ALPHA_TO_ONE_SGIS                       = $809F;
  {$EXTERNALSYM GL_SAMPLE_ALPHA_TO_ONE_SGIS}
  GL_SAMPLE_MASK_SGIS                               = $80A0;
  {$EXTERNALSYM GL_SAMPLE_MASK_SGIS}
  GL_1PASS_SGIS                                     = $80A1;
  {$EXTERNALSYM GL_1PASS_SGIS}
  GL_2PASS_0_SGIS                                   = $80A2;
  {$EXTERNALSYM GL_2PASS_0_SGIS}
  GL_2PASS_1_SGIS                                   = $80A3;
  {$EXTERNALSYM GL_2PASS_1_SGIS}
  GL_4PASS_0_SGIS                                   = $80A4;
  {$EXTERNALSYM GL_4PASS_0_SGIS}
  GL_4PASS_1_SGIS                                   = $80A5;
  {$EXTERNALSYM GL_4PASS_1_SGIS}
  GL_4PASS_2_SGIS                                   = $80A6;
  {$EXTERNALSYM GL_4PASS_2_SGIS}
  GL_4PASS_3_SGIS                                   = $80A7;
  {$EXTERNALSYM GL_4PASS_3_SGIS}
  GL_SAMPLE_BUFFERS_SGIS                            = $80A8;
  {$EXTERNALSYM GL_SAMPLE_BUFFERS_SGIS}
  GL_SAMPLES_SGIS                                   = $80A9;
  {$EXTERNALSYM GL_SAMPLES_SGIS}
  GL_SAMPLE_MASK_VALUE_SGIS                         = $80AA;
  {$EXTERNALSYM GL_SAMPLE_MASK_VALUE_SGIS}
  GL_SAMPLE_MASK_INVERT_SGIS                        = $80AB;
  {$EXTERNALSYM GL_SAMPLE_MASK_INVERT_SGIS}
  GL_SAMPLE_PATTERN_SGIS                            = $80AC;
  {$EXTERNALSYM GL_SAMPLE_PATTERN_SGIS}

  // GL_SGIS_generate_mipmap
  GL_GENERATE_MIPMAP_SGIS                           = $8191;
  {$EXTERNALSYM GL_GENERATE_MIPMAP_SGIS}
  GL_GENERATE_MIPMAP_HINT_SGIS                      = $8192;
  {$EXTERNALSYM GL_GENERATE_MIPMAP_HINT_SGIS}

  // GL_SGIX_clipmap
  GL_LINEAR_CLIPMAP_LINEAR_SGIX                     = $8170;
  {$EXTERNALSYM GL_LINEAR_CLIPMAP_LINEAR_SGIX}
  GL_TEXTURE_CLIPMAP_CENTER_SGIX                    = $8171;
  {$EXTERNALSYM GL_TEXTURE_CLIPMAP_CENTER_SGIX}
  GL_TEXTURE_CLIPMAP_FRAME_SGIX                     = $8172;
  {$EXTERNALSYM GL_TEXTURE_CLIPMAP_FRAME_SGIX}
  GL_TEXTURE_CLIPMAP_OFFSET_SGIX                    = $8173;
  {$EXTERNALSYM GL_TEXTURE_CLIPMAP_OFFSET_SGIX}
  GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX             = $8174;
  {$EXTERNALSYM GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX}
  GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX                = $8175;
  {$EXTERNALSYM GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX}
  GL_TEXTURE_CLIPMAP_DEPTH_SGIX                     = $8176;
  {$EXTERNALSYM GL_TEXTURE_CLIPMAP_DEPTH_SGIX}
  GL_MAX_CLIPMAP_DEPTH_SGIX                         = $8177;
  {$EXTERNALSYM GL_MAX_CLIPMAP_DEPTH_SGIX}
  GL_MAX_CLIPMAP_VIRTUAL_DEPTH_SGIX                 = $8178;
  {$EXTERNALSYM GL_MAX_CLIPMAP_VIRTUAL_DEPTH_SGIX}
  GL_NEAREST_CLIPMAP_NEAREST_SGIX                   = $844D;
  {$EXTERNALSYM GL_NEAREST_CLIPMAP_NEAREST_SGIX}
  GL_NEAREST_CLIPMAP_LINEAR_SGIX                    = $844E;
  {$EXTERNALSYM GL_NEAREST_CLIPMAP_LINEAR_SGIX}
  GL_LINEAR_CLIPMAP_NEAREST_SGIX                    = $844F;
  {$EXTERNALSYM GL_LINEAR_CLIPMAP_NEAREST_SGIX}

  // GL_SGIX_shadow
  GL_TEXTURE_COMPARE_SGIX                           = $819A;
  {$EXTERNALSYM GL_TEXTURE_COMPARE_SGIX}
  GL_TEXTURE_COMPARE_OPERATOR_SGIX                  = $819B;
  {$EXTERNALSYM GL_TEXTURE_COMPARE_OPERATOR_SGIX}
  GL_TEXTURE_LEQUAL_R_SGIX                          = $819C;
  {$EXTERNALSYM GL_TEXTURE_LEQUAL_R_SGIX}
  GL_TEXTURE_GEQUAL_R_SGIX                          = $819D;
  {$EXTERNALSYM GL_TEXTURE_GEQUAL_R_SGIX}

  // GL_SGIS_texture_edge_clamp
  GL_CLAMP_TO_EDGE_SGIS                             = $812F;
  {$EXTERNALSYM GL_CLAMP_TO_EDGE_SGIS}

  // GL_SGIS_texture_border_clamp
  GL_CLAMP_TO_BORDER_SGIS                           = $812D;
  {$EXTERNALSYM GL_CLAMP_TO_BORDER_SGIS}

  // GL_SGIX_interlace
  GL_INTERLACE_SGIX                                 = $8094;
  {$EXTERNALSYM GL_INTERLACE_SGIX}

  // GL_SGIX_pixel_tiles
  GL_PIXEL_TILE_BEST_ALIGNMENT_SGIX                 = $813E;
  {$EXTERNALSYM GL_PIXEL_TILE_BEST_ALIGNMENT_SGIX}
  GL_PIXEL_TILE_CACHE_INCREMENT_SGIX                = $813F;
  {$EXTERNALSYM GL_PIXEL_TILE_CACHE_INCREMENT_SGIX}
  GL_PIXEL_TILE_WIDTH_SGIX                          = $8140;
  {$EXTERNALSYM GL_PIXEL_TILE_WIDTH_SGIX}
  GL_PIXEL_TILE_HEIGHT_SGIX                         = $8141;
  {$EXTERNALSYM GL_PIXEL_TILE_HEIGHT_SGIX}
  GL_PIXEL_TILE_GRID_WIDTH_SGIX                     = $8142;
  {$EXTERNALSYM GL_PIXEL_TILE_GRID_WIDTH_SGIX}
  GL_PIXEL_TILE_GRID_HEIGHT_SGIX                    = $8143;
  {$EXTERNALSYM GL_PIXEL_TILE_GRID_HEIGHT_SGIX}
  GL_PIXEL_TILE_GRID_DEPTH_SGIX                     = $8144;
  {$EXTERNALSYM GL_PIXEL_TILE_GRID_DEPTH_SGIX}
  GL_PIXEL_TILE_CACHE_SIZE_SGIX                     = $8145;
  {$EXTERNALSYM GL_PIXEL_TILE_CACHE_SIZE_SGIX}

  // GL_SGIS_texture_select
  GL_DUAL_ALPHA4_SGIS                               = $8110;
  {$EXTERNALSYM GL_DUAL_ALPHA4_SGIS}
  GL_DUAL_ALPHA8_SGIS                               = $8111;
  {$EXTERNALSYM GL_DUAL_ALPHA8_SGIS}
  GL_DUAL_ALPHA12_SGIS                              = $8112;
  {$EXTERNALSYM GL_DUAL_ALPHA12_SGIS}
  GL_DUAL_ALPHA16_SGIS                              = $8113;
  {$EXTERNALSYM GL_DUAL_ALPHA16_SGIS}
  GL_DUAL_LUMINANCE4_SGIS                           = $8114;
  {$EXTERNALSYM GL_DUAL_LUMINANCE4_SGIS}
  GL_DUAL_LUMINANCE8_SGIS                           = $8115;
  {$EXTERNALSYM GL_DUAL_LUMINANCE8_SGIS}
  GL_DUAL_LUMINANCE12_SGIS                          = $8116;
  {$EXTERNALSYM GL_DUAL_LUMINANCE12_SGIS}
  GL_DUAL_LUMINANCE16_SGIS                          = $8117;
  {$EXTERNALSYM GL_DUAL_LUMINANCE16_SGIS}
  GL_DUAL_INTENSITY4_SGIS                           = $8118;
  {$EXTERNALSYM GL_DUAL_INTENSITY4_SGIS}
  GL_DUAL_INTENSITY8_SGIS                           = $8119;
  {$EXTERNALSYM GL_DUAL_INTENSITY8_SGIS}
  GL_DUAL_INTENSITY12_SGIS                          = $811A;
  {$EXTERNALSYM GL_DUAL_INTENSITY12_SGIS}
  GL_DUAL_INTENSITY16_SGIS                          = $811B;
  {$EXTERNALSYM GL_DUAL_INTENSITY16_SGIS}
  GL_DUAL_LUMINANCE_ALPHA4_SGIS                     = $811C;
  {$EXTERNALSYM GL_DUAL_LUMINANCE_ALPHA4_SGIS}
  GL_DUAL_LUMINANCE_ALPHA8_SGIS                     = $811D;
  {$EXTERNALSYM GL_DUAL_LUMINANCE_ALPHA8_SGIS}
  GL_QUAD_ALPHA4_SGIS                               = $811E;
  {$EXTERNALSYM GL_QUAD_ALPHA4_SGIS}
  GL_QUAD_ALPHA8_SGIS                               = $811F;
  {$EXTERNALSYM GL_QUAD_ALPHA8_SGIS}
  GL_QUAD_LUMINANCE4_SGIS                           = $8120;
  {$EXTERNALSYM GL_QUAD_LUMINANCE4_SGIS}
  GL_QUAD_LUMINANCE8_SGIS                           = $8121;
  {$EXTERNALSYM GL_QUAD_LUMINANCE8_SGIS}
  GL_QUAD_INTENSITY4_SGIS                           = $8122;
  {$EXTERNALSYM GL_QUAD_INTENSITY4_SGIS}
  GL_QUAD_INTENSITY8_SGIS                           = $8123;
  {$EXTERNALSYM GL_QUAD_INTENSITY8_SGIS}
  GL_DUAL_TEXTURE_SELECT_SGIS                       = $8124;
  {$EXTERNALSYM GL_DUAL_TEXTURE_SELECT_SGIS}
  GL_QUAD_TEXTURE_SELECT_SGIS                       = $8125;
  {$EXTERNALSYM GL_QUAD_TEXTURE_SELECT_SGIS}

  // GL_SGIX_sprite
  GL_SPRITE_SGIX                                    = $8148;
  {$EXTERNALSYM GL_SPRITE_SGIX}
  GL_SPRITE_MODE_SGIX                               = $8149;
  {$EXTERNALSYM GL_SPRITE_MODE_SGIX}
  GL_SPRITE_AXIS_SGIX                               = $814A;
  {$EXTERNALSYM GL_SPRITE_AXIS_SGIX}
  GL_SPRITE_TRANSLATION_SGIX                        = $814B;
  {$EXTERNALSYM GL_SPRITE_TRANSLATION_SGIX}
  GL_SPRITE_AXIAL_SGIX                              = $814C;
  {$EXTERNALSYM GL_SPRITE_AXIAL_SGIX}
  GL_SPRITE_OBJECT_ALIGNED_SGIX                     = $814D;
  {$EXTERNALSYM GL_SPRITE_OBJECT_ALIGNED_SGIX}
  GL_SPRITE_EYE_ALIGNED_SGIX                        = $814E;
  {$EXTERNALSYM GL_SPRITE_EYE_ALIGNED_SGIX}

  // GL_SGIX_texture_multi_buffer
  GL_TEXTURE_MULTI_BUFFER_HINT_SGIX                 = $812E;
  {$EXTERNALSYM GL_TEXTURE_MULTI_BUFFER_HINT_SGIX}

  // GL_SGIS_point_parameters
  GL_POINT_SIZE_MIN_SGIS                            = $8126;
  {$EXTERNALSYM GL_POINT_SIZE_MIN_SGIS}
  GL_POINT_SIZE_MAX_SGIS                            = $8127;
  {$EXTERNALSYM GL_POINT_SIZE_MAX_SGIS}
  GL_POINT_FADE_THRESHOLD_SIZE_SGIS                 = $8128;
  {$EXTERNALSYM GL_POINT_FADE_THRESHOLD_SIZE_SGIS}
  GL_DISTANCE_ATTENUATION_SGIS                      = $8129;
  {$EXTERNALSYM GL_DISTANCE_ATTENUATION_SGIS}

  // GL_SGIX_instruments
  GL_INSTRUMENT_BUFFER_POINTER_SGIX                 = $8180;
  {$EXTERNALSYM GL_INSTRUMENT_BUFFER_POINTER_SGIX}
  GL_INSTRUMENT_MEASUREMENTS_SGIX                   = $8181;
  {$EXTERNALSYM GL_INSTRUMENT_MEASUREMENTS_SGIX}

  // GL_SGIX_texture_scale_bias
  GL_POST_TEXTURE_FILTER_BIAS_SGIX                  = $8179;
  {$EXTERNALSYM GL_POST_TEXTURE_FILTER_BIAS_SGIX}
  GL_POST_TEXTURE_FILTER_SCALE_SGIX                 = $817A;
  {$EXTERNALSYM GL_POST_TEXTURE_FILTER_SCALE_SGIX}
  GL_POST_TEXTURE_FILTER_BIAS_RANGE_SGIX            = $817B;
  {$EXTERNALSYM GL_POST_TEXTURE_FILTER_BIAS_RANGE_SGIX}
  GL_POST_TEXTURE_FILTER_SCALE_RANGE_SGIX           = $817C;
  {$EXTERNALSYM GL_POST_TEXTURE_FILTER_SCALE_RANGE_SGIX}

  // GL_SGIX_framezoom
  GL_FRAMEZOOM_SGIX                                 = $818B;
  {$EXTERNALSYM GL_FRAMEZOOM_SGIX}
  GL_FRAMEZOOM_FACTOR_SGIX                          = $818C;
  {$EXTERNALSYM GL_FRAMEZOOM_FACTOR_SGIX}
  GL_MAX_FRAMEZOOM_FACTOR_SGIX                      = $818D;
  {$EXTERNALSYM GL_MAX_FRAMEZOOM_FACTOR_SGIX}

  // GL_FfdMaskSGIX
  GL_TEXTURE_DEFORMATION_BIT_SGIX                   = $00000001;
  {$EXTERNALSYM GL_TEXTURE_DEFORMATION_BIT_SGIX}
  GL_GEOMETRY_DEFORMATION_BIT_SGIX                  = $00000002;
  {$EXTERNALSYM GL_GEOMETRY_DEFORMATION_BIT_SGIX}

  // GL_SGIX_polynomial_ffd
  GL_GEOMETRY_DEFORMATION_SGIX                      = $8194;
  {$EXTERNALSYM GL_GEOMETRY_DEFORMATION_SGIX}
  GL_TEXTURE_DEFORMATION_SGIX                       = $8195;
  {$EXTERNALSYM GL_TEXTURE_DEFORMATION_SGIX}
  GL_DEFORMATIONS_MASK_SGIX                         = $8196;
  {$EXTERNALSYM GL_DEFORMATIONS_MASK_SGIX}
  GL_MAX_DEFORMATION_ORDER_SGIX                     = $8197;
  {$EXTERNALSYM GL_MAX_DEFORMATION_ORDER_SGIX}

  // GL_SGIX_reference_plane
  GL_REFERENCE_PLANE_SGIX                           = $817D;
  {$EXTERNALSYM GL_REFERENCE_PLANE_SGIX}
  GL_REFERENCE_PLANE_EQUATION_SGIX                  = $817E;
  {$EXTERNALSYM GL_REFERENCE_PLANE_EQUATION_SGIX}

  // GL_SGIX_depth_texture
  GL_DEPTH_COMPONENT16_SGIX                         = $81A5;
  {$EXTERNALSYM GL_DEPTH_COMPONENT16_SGIX}
  GL_DEPTH_COMPONENT24_SGIX                         = $81A6;
  {$EXTERNALSYM GL_DEPTH_COMPONENT24_SGIX}
  GL_DEPTH_COMPONENT32_SGIX                         = $81A7;
  {$EXTERNALSYM GL_DEPTH_COMPONENT32_SGIX}

  // GL_SGIS_fog_function
  GL_FOG_FUNC_SGIS                                  = $812A;
  {$EXTERNALSYM GL_FOG_FUNC_SGIS}
  GL_FOG_FUNC_POINTS_SGIS                           = $812B;
  {$EXTERNALSYM GL_FOG_FUNC_POINTS_SGIS}
  GL_MAX_FOG_FUNC_POINTS_SGIS                       = $812C;
  {$EXTERNALSYM GL_MAX_FOG_FUNC_POINTS_SGIS}

  // GL_SGIX_fog_offset
  GL_FOG_OFFSET_SGIX                                = $8198;
  {$EXTERNALSYM GL_FOG_OFFSET_SGIX}
  GL_FOG_OFFSET_VALUE_SGIX                          = $8199;
  {$EXTERNALSYM GL_FOG_OFFSET_VALUE_SGIX}

  // GL_HP_image_transform
  GL_IMAGE_SCALE_X_HP                               = $8155;
  {$EXTERNALSYM GL_IMAGE_SCALE_X_HP}
  GL_IMAGE_SCALE_Y_HP                               = $8156;
  {$EXTERNALSYM GL_IMAGE_SCALE_Y_HP}
  GL_IMAGE_TRANSLATE_X_HP                           = $8157;
  {$EXTERNALSYM GL_IMAGE_TRANSLATE_X_HP}
  GL_IMAGE_TRANSLATE_Y_HP                           = $8158;
  {$EXTERNALSYM GL_IMAGE_TRANSLATE_Y_HP}
  GL_IMAGE_ROTATE_ANGLE_HP                          = $8159;
  {$EXTERNALSYM GL_IMAGE_ROTATE_ANGLE_HP}
  GL_IMAGE_ROTATE_ORIGIN_X_HP                       = $815A;
  {$EXTERNALSYM GL_IMAGE_ROTATE_ORIGIN_X_HP}
  GL_IMAGE_ROTATE_ORIGIN_Y_HP                       = $815B;
  {$EXTERNALSYM GL_IMAGE_ROTATE_ORIGIN_Y_HP}
  GL_IMAGE_MAG_FILTER_HP                            = $815C;
  {$EXTERNALSYM GL_IMAGE_MAG_FILTER_HP}
  GL_IMAGE_MIN_FILTER_HP                            = $815D;
  {$EXTERNALSYM GL_IMAGE_MIN_FILTER_HP}
  GL_IMAGE_CUBIC_WEIGHT_HP                          = $815E;
  {$EXTERNALSYM GL_IMAGE_CUBIC_WEIGHT_HP}
  GL_CUBIC_HP                                       = $815F;
  {$EXTERNALSYM GL_CUBIC_HP}
  GL_AVERAGE_HP                                     = $8160;
  {$EXTERNALSYM GL_AVERAGE_HP}
  GL_IMAGE_TRANSFORM_2D_HP                          = $8161;
  {$EXTERNALSYM GL_IMAGE_TRANSFORM_2D_HP}
  GL_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP            = $8162;
  {$EXTERNALSYM GL_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP}
  GL_PROXY_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP      = $8163;
  {$EXTERNALSYM GL_PROXY_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP}

  // GL_HP_convolution_border_modes
  GL_IGNORE_BORDER_HP                               = $8150;
  {$EXTERNALSYM GL_IGNORE_BORDER_HP}
  GL_CONSTANT_BORDER_HP                             = $8151;
  {$EXTERNALSYM GL_CONSTANT_BORDER_HP}
  GL_REPLICATE_BORDER_HP                            = $8153;
  {$EXTERNALSYM GL_REPLICATE_BORDER_HP}
  GL_CONVOLUTION_BORDER_COLOR_HP                    = $8154;
  {$EXTERNALSYM GL_CONVOLUTION_BORDER_COLOR_HP}

  // GL_SGIX_texture_add_env
  GL_TEXTURE_ENV_BIAS_SGIX                          = $80BE;
  {$EXTERNALSYM GL_TEXTURE_ENV_BIAS_SGIX}

  // GL_PGI_vertex_hints
  GL_VERTEX_DATA_HINT_PGI                           = $1A22A;
  {$EXTERNALSYM GL_VERTEX_DATA_HINT_PGI}
  GL_VERTEX_CONSISTENT_HINT_PGI                     = $1A22B;
  {$EXTERNALSYM GL_VERTEX_CONSISTENT_HINT_PGI}
  GL_MATERIAL_SIDE_HINT_PGI                         = $1A22C;
  {$EXTERNALSYM GL_MATERIAL_SIDE_HINT_PGI}
  GL_MAX_VERTEX_HINT_PGI                            = $1A22D;
  {$EXTERNALSYM GL_MAX_VERTEX_HINT_PGI}
  GL_COLOR3_BIT_PGI                                 = $00010000;
  {$EXTERNALSYM GL_COLOR3_BIT_PGI}
  GL_COLOR4_BIT_PGI                                 = $00020000;
  {$EXTERNALSYM GL_COLOR4_BIT_PGI}
  GL_EDGEFLAG_BIT_PGI                               = $00040000;
  {$EXTERNALSYM GL_EDGEFLAG_BIT_PGI}
  GL_INDEX_BIT_PGI                                  = $00080000;
  {$EXTERNALSYM GL_INDEX_BIT_PGI}
  GL_MAT_AMBIENT_BIT_PGI                            = $00100000;
  {$EXTERNALSYM GL_MAT_AMBIENT_BIT_PGI}
  GL_MAT_AMBIENT_AND_DIFFUSE_BIT_PGI                = $00200000;
  {$EXTERNALSYM GL_MAT_AMBIENT_AND_DIFFUSE_BIT_PGI}
  GL_MAT_DIFFUSE_BIT_PGI                            = $00400000;
  {$EXTERNALSYM GL_MAT_DIFFUSE_BIT_PGI}
  GL_MAT_EMISSION_BIT_PGI                           = $00800000;
  {$EXTERNALSYM GL_MAT_EMISSION_BIT_PGI}
  GL_MAT_COLOR_INDEXES_BIT_PGI                      = $01000000;
  {$EXTERNALSYM GL_MAT_COLOR_INDEXES_BIT_PGI}
  GL_MAT_SHININESS_BIT_PGI                          = $02000000;
  {$EXTERNALSYM GL_MAT_SHININESS_BIT_PGI}
  GL_MAT_SPECULAR_BIT_PGI                           = $04000000;
  {$EXTERNALSYM GL_MAT_SPECULAR_BIT_PGI}
  GL_NORMAL_BIT_PGI                                 = $08000000;
  {$EXTERNALSYM GL_NORMAL_BIT_PGI}
  GL_TEXCOORD1_BIT_PGI                              = $10000000;
  {$EXTERNALSYM GL_TEXCOORD1_BIT_PGI}
  GL_TEXCOORD2_BIT_PGI                              = $20000000;
  {$EXTERNALSYM GL_TEXCOORD2_BIT_PGI}
  GL_TEXCOORD3_BIT_PGI                              = $40000000;
  {$EXTERNALSYM GL_TEXCOORD3_BIT_PGI}
  GL_TEXCOORD4_BIT_PGI                              = $80000000;
  {$EXTERNALSYM GL_TEXCOORD4_BIT_PGI}
  GL_VERTEX23_BIT_PGI                               = $00000004;
  {$EXTERNALSYM GL_VERTEX23_BIT_PGI}
  GL_VERTEX4_BIT_PGI                                = $00000008;
  {$EXTERNALSYM GL_VERTEX4_BIT_PGI}

  // GL_PGI_misc_hints
  GL_PREFER_DOUBLEBUFFER_HINT_PGI                   = $1A1F8;
  {$EXTERNALSYM GL_PREFER_DOUBLEBUFFER_HINT_PGI}
  GL_CONSERVE_MEMORY_HINT_PGI                       = $1A1FD;
  {$EXTERNALSYM GL_CONSERVE_MEMORY_HINT_PGI}
  GL_RECLAIM_MEMORY_HINT_PGI                        = $1A1FE;
  {$EXTERNALSYM GL_RECLAIM_MEMORY_HINT_PGI}
  GL_NATIVE_GRAPHICS_HANDLE_PGI                     = $1A202;
  {$EXTERNALSYM GL_NATIVE_GRAPHICS_HANDLE_PGI}
  GL_NATIVE_GRAPHICS_BEGIN_HINT_PGI                 = $1A203;
  {$EXTERNALSYM GL_NATIVE_GRAPHICS_BEGIN_HINT_PGI}
  GL_NATIVE_GRAPHICS_END_HINT_PGI                   = $1A204;
  {$EXTERNALSYM GL_NATIVE_GRAPHICS_END_HINT_PGI}
  GL_ALWAYS_FAST_HINT_PGI                           = $1A20C;
  {$EXTERNALSYM GL_ALWAYS_FAST_HINT_PGI}
  GL_ALWAYS_SOFT_HINT_PGI                           = $1A20D;
  {$EXTERNALSYM GL_ALWAYS_SOFT_HINT_PGI}
  GL_ALLOW_DRAW_OBJ_HINT_PGI                        = $1A20E;
  {$EXTERNALSYM GL_ALLOW_DRAW_OBJ_HINT_PGI}
  GL_ALLOW_DRAW_WIN_HINT_PGI                        = $1A20F;
  {$EXTERNALSYM GL_ALLOW_DRAW_WIN_HINT_PGI}
  GL_ALLOW_DRAW_FRG_HINT_PGI                        = $1A210;
  {$EXTERNALSYM GL_ALLOW_DRAW_FRG_HINT_PGI}
  GL_ALLOW_DRAW_MEM_HINT_PGI                        = $1A211;
  {$EXTERNALSYM GL_ALLOW_DRAW_MEM_HINT_PGI}
  GL_STRICT_DEPTHFUNC_HINT_PGI                      = $1A216;
  {$EXTERNALSYM GL_STRICT_DEPTHFUNC_HINT_PGI}
  GL_STRICT_LIGHTING_HINT_PGI                       = $1A217;
  {$EXTERNALSYM GL_STRICT_LIGHTING_HINT_PGI}
  GL_STRICT_SCISSOR_HINT_PGI                        = $1A218;
  {$EXTERNALSYM GL_STRICT_SCISSOR_HINT_PGI}
  GL_FULL_STIPPLE_HINT_PGI                          = $1A219;
  {$EXTERNALSYM GL_FULL_STIPPLE_HINT_PGI}
  GL_CLIP_NEAR_HINT_PGI                             = $1A220;
  {$EXTERNALSYM GL_CLIP_NEAR_HINT_PGI}
  GL_CLIP_FAR_HINT_PGI                              = $1A221;
  {$EXTERNALSYM GL_CLIP_FAR_HINT_PGI}
  GL_WIDE_LINE_HINT_PGI                             = $1A222;
  {$EXTERNALSYM GL_WIDE_LINE_HINT_PGI}
  GL_BACK_NORMALS_HINT_PGI                          = $1A223;
  {$EXTERNALSYM GL_BACK_NORMALS_HINT_PGI}

  // GL_EXT_paletted_texture
  GL_TEXTURE_INDEX_SIZE_EXT                         = $80ED;
  {$EXTERNALSYM GL_TEXTURE_INDEX_SIZE_EXT}

  // GL_SGIX_list_priority
  GL_LIST_PRIORITY_SGIX                             = $8182;
  {$EXTERNALSYM GL_LIST_PRIORITY_SGIX}

  // GL_SGIX_ir_instrument1
  GL_IR_INSTRUMENT1_SGIX                            = $817F;
  {$EXTERNALSYM GL_IR_INSTRUMENT1_SGIX}

  // GL_SGIX_calligraphic_fragment
  GL_CALLIGRAPHIC_FRAGMENT_SGIX                     = $8183;
  {$EXTERNALSYM GL_CALLIGRAPHIC_FRAGMENT_SGIX}

  // GL_SGIX_texture_lod_bias
  GL_TEXTURE_LOD_BIAS_S_SGIX                        = $818E;
  {$EXTERNALSYM GL_TEXTURE_LOD_BIAS_S_SGIX}
  GL_TEXTURE_LOD_BIAS_T_SGIX                        = $818F;
  {$EXTERNALSYM GL_TEXTURE_LOD_BIAS_T_SGIX}
  GL_TEXTURE_LOD_BIAS_R_SGIX                        = $8190;
  {$EXTERNALSYM GL_TEXTURE_LOD_BIAS_R_SGIX}

  // GL_SGIX_shadow_ambient
  GL_SHADOW_AMBIENT_SGIX                            = $80BF;
  {$EXTERNALSYM GL_SHADOW_AMBIENT_SGIX}

  // GL_SGIX_ycrcb
  GL_YCRCB_422_SGIX                                 = $81BB;
  {$EXTERNALSYM GL_YCRCB_422_SGIX}
  GL_YCRCB_444_SGIX                                 = $81BC;
  {$EXTERNALSYM GL_YCRCB_444_SGIX}

  // GL_SGIX_fragment_lighting
  GL_FRAGMENT_LIGHTING_SGIX                         = $8400;
  {$EXTERNALSYM GL_FRAGMENT_LIGHTING_SGIX}
  GL_FRAGMENT_COLOR_MATERIAL_SGIX                   = $8401;
  {$EXTERNALSYM GL_FRAGMENT_COLOR_MATERIAL_SGIX}
  GL_FRAGMENT_COLOR_MATERIAL_FACE_SGIX              = $8402;
  {$EXTERNALSYM GL_FRAGMENT_COLOR_MATERIAL_FACE_SGIX}
  GL_FRAGMENT_COLOR_MATERIAL_PARAMETER_SGIX         = $8403;
  {$EXTERNALSYM GL_FRAGMENT_COLOR_MATERIAL_PARAMETER_SGIX}
  GL_MAX_FRAGMENT_LIGHTS_SGIX                       = $8404;
  {$EXTERNALSYM GL_MAX_FRAGMENT_LIGHTS_SGIX}
  GL_MAX_ACTIVE_LIGHTS_SGIX                         = $8405;
  {$EXTERNALSYM GL_MAX_ACTIVE_LIGHTS_SGIX}
  GL_CURRENT_RASTER_NORMAL_SGIX                     = $8406;
  {$EXTERNALSYM GL_CURRENT_RASTER_NORMAL_SGIX}
  GL_LIGHT_ENV_MODE_SGIX                            = $8407;
  {$EXTERNALSYM GL_LIGHT_ENV_MODE_SGIX}
  GL_FRAGMENT_LIGHT_MODEL_LOCAL_VIEWER_SGIX         = $8408;
  {$EXTERNALSYM GL_FRAGMENT_LIGHT_MODEL_LOCAL_VIEWER_SGIX}
  GL_FRAGMENT_LIGHT_MODEL_TWO_SIDE_SGIX             = $8409;
  {$EXTERNALSYM GL_FRAGMENT_LIGHT_MODEL_TWO_SIDE_SGIX}
  GL_FRAGMENT_LIGHT_MODEL_AMBIENT_SGIX              = $840A;
  {$EXTERNALSYM GL_FRAGMENT_LIGHT_MODEL_AMBIENT_SGIX}
  GL_FRAGMENT_LIGHT_MODEL_NORMAL_INTERPOLATION_SGIX = $840B;
  {$EXTERNALSYM GL_FRAGMENT_LIGHT_MODEL_NORMAL_INTERPOLATION_SGIX}
  GL_FRAGMENT_LIGHT0_SGIX                           = $840C;
  {$EXTERNALSYM GL_FRAGMENT_LIGHT0_SGIX}
  GL_FRAGMENT_LIGHT1_SGIX                           = $840D;
  {$EXTERNALSYM GL_FRAGMENT_LIGHT1_SGIX}
  GL_FRAGMENT_LIGHT2_SGIX                           = $840E;
  {$EXTERNALSYM GL_FRAGMENT_LIGHT2_SGIX}
  GL_FRAGMENT_LIGHT3_SGIX                           = $840F;
  {$EXTERNALSYM GL_FRAGMENT_LIGHT3_SGIX}
  GL_FRAGMENT_LIGHT4_SGIX                           = $8410;
  {$EXTERNALSYM GL_FRAGMENT_LIGHT4_SGIX}
  GL_FRAGMENT_LIGHT5_SGIX                           = $8411;
  {$EXTERNALSYM GL_FRAGMENT_LIGHT5_SGIX}
  GL_FRAGMENT_LIGHT6_SGIX                           = $8412;
  {$EXTERNALSYM GL_FRAGMENT_LIGHT6_SGIX}
  GL_FRAGMENT_LIGHT7_SGIX                           = $8413;
  {$EXTERNALSYM GL_FRAGMENT_LIGHT7_SGIX}

  // GL_IBM_rasterpos_clip
  GL_RASTER_POSITION_UNCLIPPED_IBM                  = $19262;
  {$EXTERNALSYM GL_RASTER_POSITION_UNCLIPPED_IBM}

  // GL_HP_texture_lighting
  GL_TEXTURE_LIGHTING_MODE_HP                       = $8167;
  {$EXTERNALSYM GL_TEXTURE_LIGHTING_MODE_HP}
  GL_TEXTURE_POST_SPECULAR_HP                       = $8168;
  {$EXTERNALSYM GL_TEXTURE_POST_SPECULAR_HP}
  GL_TEXTURE_PRE_SPECULAR_HP                        = $8169;
  {$EXTERNALSYM GL_TEXTURE_PRE_SPECULAR_HP}

  // GL_EXT_draw_range_elements
  GL_MAX_ELEMENTS_VERTICES_EXT                      = $80E8;
  {$EXTERNALSYM GL_MAX_ELEMENTS_VERTICES_EXT}
  GL_MAX_ELEMENTS_INDICES_EXT                       = $80E9;
  {$EXTERNALSYM GL_MAX_ELEMENTS_INDICES_EXT}

  // GL_WIN_phong_shading
  GL_PHONG_WIN                                      = $80EA;
  {$EXTERNALSYM GL_PHONG_WIN}
  GL_PHONG_HINT_WIN                                 = $80EB;
  {$EXTERNALSYM GL_PHONG_HINT_WIN}

  // GL_WIN_specular_fog
  GL_FOG_SPECULAR_TEXTURE_WIN                       = $80EC;
  {$EXTERNALSYM GL_FOG_SPECULAR_TEXTURE_WIN}

  // GL_EXT_light_texture
  GL_FRAGMENT_MATERIAL_EXT                          = $8349;
  {$EXTERNALSYM GL_FRAGMENT_MATERIAL_EXT}
  GL_FRAGMENT_NORMAL_EXT                            = $834A;
  {$EXTERNALSYM GL_FRAGMENT_NORMAL_EXT}
  GL_FRAGMENT_COLOR_EXT                             = $834C;
  {$EXTERNALSYM GL_FRAGMENT_COLOR_EXT}
  GL_ATTENUATION_EXT                                = $834D;
  {$EXTERNALSYM GL_ATTENUATION_EXT}
  GL_SHADOW_ATTENUATION_EXT                         = $834E;
  {$EXTERNALSYM GL_SHADOW_ATTENUATION_EXT}
  GL_TEXTURE_APPLICATION_MODE_EXT                   = $834F;
  {$EXTERNALSYM GL_TEXTURE_APPLICATION_MODE_EXT}
  GL_TEXTURE_LIGHT_EXT                              = $8350;
  {$EXTERNALSYM GL_TEXTURE_LIGHT_EXT}
  GL_TEXTURE_MATERIAL_FACE_EXT                      = $8351;
  {$EXTERNALSYM GL_TEXTURE_MATERIAL_FACE_EXT}
  GL_TEXTURE_MATERIAL_PARAMETER_EXT                 = $8352;
  {$EXTERNALSYM GL_TEXTURE_MATERIAL_PARAMETER_EXT}

  // GL_SGIX_blend_alpha_minmax
  GL_ALPHA_MIN_SGIX                                 = $8320;
  {$EXTERNALSYM GL_ALPHA_MIN_SGIX}
  GL_ALPHA_MAX_SGIX                                 = $8321;
  {$EXTERNALSYM GL_ALPHA_MAX_SGIX}

  // GL_SGIX_async
  GL_ASYNC_MARKER_SGIX                              = $8329;
  {$EXTERNALSYM GL_ASYNC_MARKER_SGIX}

  // GL_SGIX_async_pixel
  GL_ASYNC_TEX_IMAGE_SGIX                           = $835C;
  {$EXTERNALSYM GL_ASYNC_TEX_IMAGE_SGIX}
  GL_ASYNC_DRAW_PIXELS_SGIX                         = $835D;
  {$EXTERNALSYM GL_ASYNC_DRAW_PIXELS_SGIX}
  GL_ASYNC_READ_PIXELS_SGIX                         = $835E;
  {$EXTERNALSYM GL_ASYNC_READ_PIXELS_SGIX}
  GL_MAX_ASYNC_TEX_IMAGE_SGIX                       = $835F;
  {$EXTERNALSYM GL_MAX_ASYNC_TEX_IMAGE_SGIX}
  GL_MAX_ASYNC_DRAW_PIXELS_SGIX                     = $8360;
  {$EXTERNALSYM GL_MAX_ASYNC_DRAW_PIXELS_SGIX}
  GL_MAX_ASYNC_READ_PIXELS_SGIX                     = $8361;
  {$EXTERNALSYM GL_MAX_ASYNC_READ_PIXELS_SGIX}

  // GL_SGIX_async_histogram
  GL_ASYNC_HISTOGRAM_SGIX                           = $832C;
  {$EXTERNALSYM GL_ASYNC_HISTOGRAM_SGIX}
  GL_MAX_ASYNC_HISTOGRAM_SGIX                       = $832D;
  {$EXTERNALSYM GL_MAX_ASYNC_HISTOGRAM_SGIX}

  // GL_INTEL_parallel_arrays
  GL_PARALLEL_ARRAYS_INTEL                          = $83F4;
  {$EXTERNALSYM GL_PARALLEL_ARRAYS_INTEL}
  GL_VERTEX_ARRAY_PARALLEL_POINTERS_INTEL           = $83F5;
  {$EXTERNALSYM GL_VERTEX_ARRAY_PARALLEL_POINTERS_INTEL}
  GL_NORMAL_ARRAY_PARALLEL_POINTERS_INTEL           = $83F6;
  {$EXTERNALSYM GL_NORMAL_ARRAY_PARALLEL_POINTERS_INTEL}
  GL_COLOR_ARRAY_PARALLEL_POINTERS_INTEL            = $83F7;
  {$EXTERNALSYM GL_COLOR_ARRAY_PARALLEL_POINTERS_INTEL}
  GL_TEXTURE_COORD_ARRAY_PARALLEL_POINTERS_INTEL    = $83F8;
  {$EXTERNALSYM GL_TEXTURE_COORD_ARRAY_PARALLEL_POINTERS_INTEL}

  // GL_HP_occlusion_test
  GL_OCCLUSION_TEST_HP                              = $8165;
  {$EXTERNALSYM GL_OCCLUSION_TEST_HP}
  GL_OCCLUSION_TEST_RESULT_HP                       = $8166;
  {$EXTERNALSYM GL_OCCLUSION_TEST_RESULT_HP}

  // GL_EXT_pixel_transform
  GL_PIXEL_TRANSFORM_2D_EXT                         = $8330;
  {$EXTERNALSYM GL_PIXEL_TRANSFORM_2D_EXT}
  GL_PIXEL_MAG_FILTER_EXT                           = $8331;
  {$EXTERNALSYM GL_PIXEL_MAG_FILTER_EXT}
  GL_PIXEL_MIN_FILTER_EXT                           = $8332;
  {$EXTERNALSYM GL_PIXEL_MIN_FILTER_EXT}
  GL_PIXEL_CUBIC_WEIGHT_EXT                         = $8333;
  {$EXTERNALSYM GL_PIXEL_CUBIC_WEIGHT_EXT}
  GL_CUBIC_EXT                                      = $8334;
  {$EXTERNALSYM GL_CUBIC_EXT}
  GL_AVERAGE_EXT                                    = $8335;
  {$EXTERNALSYM GL_AVERAGE_EXT}
  GL_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT             = $8336;
  {$EXTERNALSYM GL_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT}
  GL_MAX_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT         = $8337;
  {$EXTERNALSYM GL_MAX_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT}
  GL_PIXEL_TRANSFORM_2D_MATRIX_EXT                  = $8338;
  {$EXTERNALSYM GL_PIXEL_TRANSFORM_2D_MATRIX_EXT}

  // GL_EXT_separate_specular_color
  GL_LIGHT_MODEL_COLOR_CONTROL_EXT                  = $81F8;
  {$EXTERNALSYM GL_LIGHT_MODEL_COLOR_CONTROL_EXT}
  GL_SINGLE_COLOR_EXT                               = $81F9;
  {$EXTERNALSYM GL_SINGLE_COLOR_EXT}
  GL_SEPARATE_SPECULAR_COLOR_EXT                    = $81FA;
  {$EXTERNALSYM GL_SEPARATE_SPECULAR_COLOR_EXT}

  // GL_EXT_secondary_color
  GL_COLOR_SUM_EXT                                  = $8458;
  {$EXTERNALSYM GL_COLOR_SUM_EXT}
  GL_CURRENT_SECONDARY_COLOR_EXT                    = $8459;
  {$EXTERNALSYM GL_CURRENT_SECONDARY_COLOR_EXT}
  GL_SECONDARY_COLOR_ARRAY_SIZE_EXT                 = $845A;
  {$EXTERNALSYM GL_SECONDARY_COLOR_ARRAY_SIZE_EXT}
  GL_SECONDARY_COLOR_ARRAY_TYPE_EXT                 = $845B;
  {$EXTERNALSYM GL_SECONDARY_COLOR_ARRAY_TYPE_EXT}
  GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT               = $845C;
  {$EXTERNALSYM GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT}
  GL_SECONDARY_COLOR_ARRAY_POINTER_EXT              = $845D;
  {$EXTERNALSYM GL_SECONDARY_COLOR_ARRAY_POINTER_EXT}
  GL_SECONDARY_COLOR_ARRAY_EXT                      = $845E;
  {$EXTERNALSYM GL_SECONDARY_COLOR_ARRAY_EXT}

  // GL_EXT_texture_perturb_normal
  GL_PERTURB_EXT                                    = $85AE;
  {$EXTERNALSYM GL_PERTURB_EXT}
  GL_TEXTURE_NORMAL_EXT                             = $85AF;
  {$EXTERNALSYM GL_TEXTURE_NORMAL_EXT}

  // GL_EXT_fog_coord
  GL_FOG_COORDINATE_SOURCE_EXT                      = $8450;
  {$EXTERNALSYM GL_FOG_COORDINATE_SOURCE_EXT}
  GL_FOG_COORDINATE_EXT                             = $8451;
  {$EXTERNALSYM GL_FOG_COORDINATE_EXT}
  GL_FRAGMENT_DEPTH_EXT                             = $8452;
  {$EXTERNALSYM GL_FRAGMENT_DEPTH_EXT}
  GL_CURRENT_FOG_COORDINATE_EXT                     = $8453;
  {$EXTERNALSYM GL_CURRENT_FOG_COORDINATE_EXT}
  GL_FOG_COORDINATE_ARRAY_TYPE_EXT                  = $8454;
  {$EXTERNALSYM GL_FOG_COORDINATE_ARRAY_TYPE_EXT}
  GL_FOG_COORDINATE_ARRAY_STRIDE_EXT                = $8455;
  {$EXTERNALSYM GL_FOG_COORDINATE_ARRAY_STRIDE_EXT}
  GL_FOG_COORDINATE_ARRAY_POINTER_EXT               = $8456;
  {$EXTERNALSYM GL_FOG_COORDINATE_ARRAY_POINTER_EXT}
  GL_FOG_COORDINATE_ARRAY_EXT                       = $8457;
  {$EXTERNALSYM GL_FOG_COORDINATE_ARRAY_EXT}

  // GL_REND_screen_coordinates
  GL_SCREEN_COORDINATES_REND                        = $8490;
  {$EXTERNALSYM GL_SCREEN_COORDINATES_REND}
  GL_INVERTED_SCREEN_W_REND                         = $8491;
  {$EXTERNALSYM GL_INVERTED_SCREEN_W_REND}

  // GL_EXT_coordinate_frame
  GL_TANGENT_ARRAY_EXT                              = $8439;
  {$EXTERNALSYM GL_TANGENT_ARRAY_EXT}
  GL_BINORMAL_ARRAY_EXT                             = $843A;
  {$EXTERNALSYM GL_BINORMAL_ARRAY_EXT}
  GL_CURRENT_TANGENT_EXT                            = $843B;
  {$EXTERNALSYM GL_CURRENT_TANGENT_EXT}
  GL_CURRENT_BINORMAL_EXT                           = $843C;
  {$EXTERNALSYM GL_CURRENT_BINORMAL_EXT}
  GL_TANGENT_ARRAY_TYPE_EXT                         = $843E;
  {$EXTERNALSYM GL_TANGENT_ARRAY_TYPE_EXT}
  GL_TANGENT_ARRAY_STRIDE_EXT                       = $843F;
  {$EXTERNALSYM GL_TANGENT_ARRAY_STRIDE_EXT}
  GL_BINORMAL_ARRAY_TYPE_EXT                        = $8440;
  {$EXTERNALSYM GL_BINORMAL_ARRAY_TYPE_EXT}
  GL_BINORMAL_ARRAY_STRIDE_EXT                      = $8441;
  {$EXTERNALSYM GL_BINORMAL_ARRAY_STRIDE_EXT}
  GL_TANGENT_ARRAY_POINTER_EXT                      = $8442;
  {$EXTERNALSYM GL_TANGENT_ARRAY_POINTER_EXT}
  GL_BINORMAL_ARRAY_POINTER_EXT                     = $8443;
  {$EXTERNALSYM GL_BINORMAL_ARRAY_POINTER_EXT}
  GL_MAP1_TANGENT_EXT                               = $8444;
  {$EXTERNALSYM GL_MAP1_TANGENT_EXT}
  GL_MAP2_TANGENT_EXT                               = $8445;
  {$EXTERNALSYM GL_MAP2_TANGENT_EXT}
  GL_MAP1_BINORMAL_EXT                              = $8446;
  {$EXTERNALSYM GL_MAP1_BINORMAL_EXT}
  GL_MAP2_BINORMAL_EXT                              = $8447;
  {$EXTERNALSYM GL_MAP2_BINORMAL_EXT}

  // GL_EXT_texture_env_combine
  GL_SOURCE3_RGB_EXT                                = $8583;
  {$EXTERNALSYM GL_SOURCE3_RGB_EXT}
  GL_SOURCE4_RGB_EXT                                = $8584;
  {$EXTERNALSYM GL_SOURCE4_RGB_EXT}
  GL_SOURCE5_RGB_EXT                                = $8585;
  {$EXTERNALSYM GL_SOURCE5_RGB_EXT}
  GL_SOURCE6_RGB_EXT                                = $8586;
  {$EXTERNALSYM GL_SOURCE6_RGB_EXT}
  GL_SOURCE7_RGB_EXT                                = $8587;
  {$EXTERNALSYM GL_SOURCE7_RGB_EXT}
  GL_SOURCE3_ALPHA_EXT                              = $858B;
  {$EXTERNALSYM GL_SOURCE3_ALPHA_EXT}
  GL_SOURCE4_ALPHA_EXT                              = $858C;
  {$EXTERNALSYM GL_SOURCE4_ALPHA_EXT}
  GL_SOURCE5_ALPHA_EXT                              = $858D;
  {$EXTERNALSYM GL_SOURCE5_ALPHA_EXT}
  GL_SOURCE6_ALPHA_EXT                              = $858E;
  {$EXTERNALSYM GL_SOURCE6_ALPHA_EXT}
  GL_SOURCE7_ALPHA_EXT                              = $858F;
  {$EXTERNALSYM GL_SOURCE7_ALPHA_EXT}
  GL_OPERAND3_RGB_EXT                               = $8593;
  {$EXTERNALSYM GL_OPERAND3_RGB_EXT}
  GL_OPERAND4_RGB_EXT                               = $8594;
  {$EXTERNALSYM GL_OPERAND4_RGB_EXT}
  GL_OPERAND5_RGB_EXT                               = $8595;
  {$EXTERNALSYM GL_OPERAND5_RGB_EXT}
  GL_OPERAND6_RGB_EXT                               = $8596;
  {$EXTERNALSYM GL_OPERAND6_RGB_EXT}
  GL_OPERAND7_RGB_EXT                               = $8597;
  {$EXTERNALSYM GL_OPERAND7_RGB_EXT}
  GL_OPERAND3_ALPHA_EXT                             = $859B;
  {$EXTERNALSYM GL_OPERAND3_ALPHA_EXT}
  GL_OPERAND4_ALPHA_EXT                             = $859C;
  {$EXTERNALSYM GL_OPERAND4_ALPHA_EXT}
  GL_OPERAND5_ALPHA_EXT                             = $859D;
  {$EXTERNALSYM GL_OPERAND5_ALPHA_EXT}
  GL_OPERAND6_ALPHA_EXT                             = $859E;
  {$EXTERNALSYM GL_OPERAND6_ALPHA_EXT}
  GL_OPERAND7_ALPHA_EXT                             = $859F;
  {$EXTERNALSYM GL_OPERAND7_ALPHA_EXT}

  // GL_APPLE_specular_vector
  GL_LIGHT_MODEL_SPECULAR_VECTOR_APPLE              = $85B0;
  {$EXTERNALSYM GL_LIGHT_MODEL_SPECULAR_VECTOR_APPLE}

  // GL_APPLE_transform_hint
  GL_TRANSFORM_HINT_APPLE                           = $85B1;
  {$EXTERNALSYM GL_TRANSFORM_HINT_APPLE}

  // GL_SGIX_fog_scale
  GL_FOG_SCALE_SGIX                                 = $81FC;
  {$EXTERNALSYM GL_FOG_SCALE_SGIX}
  GL_FOG_SCALE_VALUE_SGIX                           = $81FD;
  {$EXTERNALSYM GL_FOG_SCALE_VALUE_SGIX}

  // GL_SUNX_constant_data
  GL_UNPACK_CONSTANT_DATA_SUNX                      = $81D5;
  {$EXTERNALSYM GL_UNPACK_CONSTANT_DATA_SUNX}
  GL_TEXTURE_CONSTANT_DATA_SUNX                     = $81D6;
  {$EXTERNALSYM GL_TEXTURE_CONSTANT_DATA_SUNX}

  // GL_SUN_global_alpha
  GL_GLOBAL_ALPHA_SUN                               = $81D9;
  {$EXTERNALSYM GL_GLOBAL_ALPHA_SUN}
  GL_GLOBAL_ALPHA_FACTOR_SUN                        = $81DA;
  {$EXTERNALSYM GL_GLOBAL_ALPHA_FACTOR_SUN}

  // GL_SUN_triangle_list
  GL_RESTART_SUN                                    = $01;
  {$EXTERNALSYM GL_RESTART_SUN}
  GL_REPLACE_MIDDLE_SUN                             = $02;
  {$EXTERNALSYM GL_REPLACE_MIDDLE_SUN}
  GL_REPLACE_OLDEST_SUN                             = $03;
  {$EXTERNALSYM GL_REPLACE_OLDEST_SUN}
  GL_TRIANGLE_LIST_SUN                              = $81D7;
  {$EXTERNALSYM GL_TRIANGLE_LIST_SUN}
  GL_REPLACEMENT_CODE_SUN                           = $81D8;
  {$EXTERNALSYM GL_REPLACEMENT_CODE_SUN}
  GL_REPLACEMENT_CODE_ARRAY_SUN                     = $85C0;
  {$EXTERNALSYM GL_REPLACEMENT_CODE_ARRAY_SUN}
  GL_REPLACEMENT_CODE_ARRAY_TYPE_SUN                = $85C1;
  {$EXTERNALSYM GL_REPLACEMENT_CODE_ARRAY_TYPE_SUN}
  GL_REPLACEMENT_CODE_ARRAY_STRIDE_SUN              = $85C2;
  {$EXTERNALSYM GL_REPLACEMENT_CODE_ARRAY_STRIDE_SUN}
  GL_REPLACEMENT_CODE_ARRAY_POINTER_SUN             = $85C3;
  {$EXTERNALSYM GL_REPLACEMENT_CODE_ARRAY_POINTER_SUN}
  GL_R1UI_V3F_SUN                                   = $85C4;
  {$EXTERNALSYM GL_R1UI_V3F_SUN}
  GL_R1UI_C4UB_V3F_SUN                              = $85C5;
  {$EXTERNALSYM GL_R1UI_C4UB_V3F_SUN}
  GL_R1UI_C3F_V3F_SUN                               = $85C6;
  {$EXTERNALSYM GL_R1UI_C3F_V3F_SUN}
  GL_R1UI_N3F_V3F_SUN                               = $85C7;
  {$EXTERNALSYM GL_R1UI_N3F_V3F_SUN}
  GL_R1UI_C4F_N3F_V3F_SUN                           = $85C8;
  {$EXTERNALSYM GL_R1UI_C4F_N3F_V3F_SUN}
  GL_R1UI_T2F_V3F_SUN                               = $85C9;
  {$EXTERNALSYM GL_R1UI_T2F_V3F_SUN}
  GL_R1UI_T2F_N3F_V3F_SUN                           = $85CA;
  {$EXTERNALSYM GL_R1UI_T2F_N3F_V3F_SUN}
  GL_R1UI_T2F_C4F_N3F_V3F_SUN                       = $85CB;
  {$EXTERNALSYM GL_R1UI_T2F_C4F_N3F_V3F_SUN}

  // GL_EXT_blend_func_separate
  GL_BLEND_DST_RGB_EXT                              = $80C8;
  {$EXTERNALSYM GL_BLEND_DST_RGB_EXT}
  GL_BLEND_SRC_RGB_EXT                              = $80C9;
  {$EXTERNALSYM GL_BLEND_SRC_RGB_EXT}
  GL_BLEND_DST_ALPHA_EXT                            = $80CA;
  {$EXTERNALSYM GL_BLEND_DST_ALPHA_EXT}
  GL_BLEND_SRC_ALPHA_EXT                            = $80CB;
  {$EXTERNALSYM GL_BLEND_SRC_ALPHA_EXT}

  // GL_INGR_color_clamp
  GL_RED_MIN_CLAMP_INGR                             = $8560;
  {$EXTERNALSYM GL_RED_MIN_CLAMP_INGR}
  GL_GREEN_MIN_CLAMP_INGR                           = $8561;
  {$EXTERNALSYM GL_GREEN_MIN_CLAMP_INGR}
  GL_BLUE_MIN_CLAMP_INGR                            = $8562;
  {$EXTERNALSYM GL_BLUE_MIN_CLAMP_INGR}
  GL_ALPHA_MIN_CLAMP_INGR                           = $8563;
  {$EXTERNALSYM GL_ALPHA_MIN_CLAMP_INGR}
  GL_RED_MAX_CLAMP_INGR                             = $8564;
  {$EXTERNALSYM GL_RED_MAX_CLAMP_INGR}
  GL_GREEN_MAX_CLAMP_INGR                           = $8565;
  {$EXTERNALSYM GL_GREEN_MAX_CLAMP_INGR}
  GL_BLUE_MAX_CLAMP_INGR                            = $8566;
  {$EXTERNALSYM GL_BLUE_MAX_CLAMP_INGR}
  GL_ALPHA_MAX_CLAMP_INGR                           = $8567;
  {$EXTERNALSYM GL_ALPHA_MAX_CLAMP_INGR}

  // GL_INGR_interlace_read
  GL_INTERLACE_READ_INGR                            = $8568;
  {$EXTERNALSYM GL_INTERLACE_READ_INGR}

  // GL_EXT_422_pixels
  GL_422_EXT                                        = $80CC;
  {$EXTERNALSYM GL_422_EXT}
  GL_422_REV_EXT                                    = $80CD;
  {$EXTERNALSYM GL_422_REV_EXT}
  GL_422_AVERAGE_EXT                                = $80CE;
  {$EXTERNALSYM GL_422_AVERAGE_EXT}
  GL_422_REV_AVERAGE_EXT                            = $80CF;
  {$EXTERNALSYM GL_422_REV_AVERAGE_EXT}

  // GL_EXT_texture_cube_map
  GL_NORMAL_MAP_EXT                                 = $8511;
  {$EXTERNALSYM GL_NORMAL_MAP_EXT}
  GL_REFLECTION_MAP_EXT                             = $8512;
  {$EXTERNALSYM GL_REFLECTION_MAP_EXT}
  GL_TEXTURE_CUBE_MAP_EXT                           = $8513;
  {$EXTERNALSYM GL_TEXTURE_CUBE_MAP_EXT}
  GL_TEXTURE_BINDING_CUBE_MAP_EXT                   = $8514;
  {$EXTERNALSYM GL_TEXTURE_BINDING_CUBE_MAP_EXT}
  GL_TEXTURE_CUBE_MAP_POSITIVE_X_EXT                = $8515;
  {$EXTERNALSYM GL_TEXTURE_CUBE_MAP_POSITIVE_X_EXT}
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X_EXT                = $8516;
  {$EXTERNALSYM GL_TEXTURE_CUBE_MAP_NEGATIVE_X_EXT}
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y_EXT                = $8517;
  {$EXTERNALSYM GL_TEXTURE_CUBE_MAP_POSITIVE_Y_EXT}
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_EXT                = $8518;
  {$EXTERNALSYM GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_EXT}
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z_EXT                = $8519;
  {$EXTERNALSYM GL_TEXTURE_CUBE_MAP_POSITIVE_Z_EXT}
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_EXT                = $851A;
  {$EXTERNALSYM GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_EXT}
  GL_PROXY_TEXTURE_CUBE_MAP_EXT                     = $851B;
  {$EXTERNALSYM GL_PROXY_TEXTURE_CUBE_MAP_EXT}
  GL_MAX_CUBE_MAP_TEXTURE_SIZE_EXT                  = $851C;
  {$EXTERNALSYM GL_MAX_CUBE_MAP_TEXTURE_SIZE_EXT}

  // GL_SUN_convolution_border_modes
  GL_WRAP_BORDER_SUN                                = $81D4;
  {$EXTERNALSYM GL_WRAP_BORDER_SUN}

  // GL_EXT_texture_lod_bias
  GL_MAX_TEXTURE_LOD_BIAS_EXT                       = $84FD;
  {$EXTERNALSYM GL_MAX_TEXTURE_LOD_BIAS_EXT}
  GL_TEXTURE_FILTER_CONTROL_EXT                     = $8500;
  {$EXTERNALSYM GL_TEXTURE_FILTER_CONTROL_EXT}
  GL_TEXTURE_LOD_BIAS_EXT                           = $8501;
  {$EXTERNALSYM GL_TEXTURE_LOD_BIAS_EXT}

  // GL_EXT_texture_filter_anisotropic
  GL_TEXTURE_MAX_ANISOTROPY_EXT                     = $84FE;
  {$EXTERNALSYM GL_TEXTURE_MAX_ANISOTROPY_EXT}
  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT                 = $84FF;
  {$EXTERNALSYM GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT}

  // GL_EXT_vertex_weighting
  GL_MODELVIEW0_STACK_DEPTH_EXT                     = GL_MODELVIEW_STACK_DEPTH;
  {$EXTERNALSYM GL_MODELVIEW0_STACK_DEPTH_EXT}
  GL_MODELVIEW1_STACK_DEPTH_EXT                     = $8502;
  {$EXTERNALSYM GL_MODELVIEW1_STACK_DEPTH_EXT}
  GL_MODELVIEW0_MATRIX_EXT                          = GL_MODELVIEW_MATRIX;
  {$EXTERNALSYM GL_MODELVIEW0_MATRIX_EXT}
  GL_MODELVIEW_MATRIX1_EXT                          = $8506;
  {$EXTERNALSYM GL_MODELVIEW_MATRIX1_EXT}
  GL_VERTEX_WEIGHTING_EXT                           = $8509;
  {$EXTERNALSYM GL_VERTEX_WEIGHTING_EXT}
  GL_MODELVIEW0_EXT                                 = GL_MODELVIEW;
  {$EXTERNALSYM GL_MODELVIEW0_EXT}
  GL_MODELVIEW1_EXT                                 = $850A;
  {$EXTERNALSYM GL_MODELVIEW1_EXT}
  GL_CURRENT_VERTEX_WEIGHT_EXT                      = $850B;
  {$EXTERNALSYM GL_CURRENT_VERTEX_WEIGHT_EXT}
  GL_VERTEX_WEIGHT_ARRAY_EXT                        = $850C;
  {$EXTERNALSYM GL_VERTEX_WEIGHT_ARRAY_EXT}
  GL_VERTEX_WEIGHT_ARRAY_SIZE_EXT                   = $850D;
  {$EXTERNALSYM GL_VERTEX_WEIGHT_ARRAY_SIZE_EXT}
  GL_VERTEX_WEIGHT_ARRAY_TYPE_EXT                   = $850E;
  {$EXTERNALSYM GL_VERTEX_WEIGHT_ARRAY_TYPE_EXT}
  GL_VERTEX_WEIGHT_ARRAY_STRIDE_EXT                 = $850F;
  {$EXTERNALSYM GL_VERTEX_WEIGHT_ARRAY_STRIDE_EXT}
  GL_VERTEX_WEIGHT_ARRAY_POINTER_EXT                = $8510;
  {$EXTERNALSYM GL_VERTEX_WEIGHT_ARRAY_POINTER_EXT}

  // GL_NV_light_max_exponent
  GL_MAX_SHININESS_NV                               = $8504;
  {$EXTERNALSYM GL_MAX_SHININESS_NV}
  GL_MAX_SPOT_EXPONENT_NV                           = $8505;
  {$EXTERNALSYM GL_MAX_SPOT_EXPONENT_NV}

  // GL_NV_vertex_array_range
  GL_VERTEX_ARRAY_RANGE_NV                          = $851D;
  {$EXTERNALSYM GL_VERTEX_ARRAY_RANGE_NV}
  GL_VERTEX_ARRAY_RANGE_LENGTH_NV                   = $851E;
  {$EXTERNALSYM GL_VERTEX_ARRAY_RANGE_LENGTH_NV}
  GL_VERTEX_ARRAY_RANGE_VALID_NV                    = $851F;
  {$EXTERNALSYM GL_VERTEX_ARRAY_RANGE_VALID_NV}
  GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV              = $8520;
  {$EXTERNALSYM GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV}
  GL_VERTEX_ARRAY_RANGE_POINTER_NV                  = $8521;
  {$EXTERNALSYM GL_VERTEX_ARRAY_RANGE_POINTER_NV}

  // GL_NV_register_combiners
  GL_REGISTER_COMBINERS_NV                          = $8522;
  {$EXTERNALSYM GL_REGISTER_COMBINERS_NV}
  GL_VARIABLE_A_NV                                  = $8523;
  {$EXTERNALSYM GL_VARIABLE_A_NV}
  GL_VARIABLE_B_NV                                  = $8524;
  {$EXTERNALSYM GL_VARIABLE_B_NV}
  GL_VARIABLE_C_NV                                  = $8525;
  {$EXTERNALSYM GL_VARIABLE_C_NV}
  GL_VARIABLE_D_NV                                  = $8526;
  {$EXTERNALSYM GL_VARIABLE_D_NV}
  GL_VARIABLE_E_NV                                  = $8527;
  {$EXTERNALSYM GL_VARIABLE_E_NV}
  GL_VARIABLE_F_NV                                  = $8528;
  {$EXTERNALSYM GL_VARIABLE_F_NV}
  GL_VARIABLE_G_NV                                  = $8529;
  {$EXTERNALSYM GL_VARIABLE_G_NV}
  GL_CONSTANT_COLOR0_NV                             = $852A;
  {$EXTERNALSYM GL_CONSTANT_COLOR0_NV}
  GL_CONSTANT_COLOR1_NV                             = $852B;
  {$EXTERNALSYM GL_CONSTANT_COLOR1_NV}
  GL_PRIMARY_COLOR_NV                               = $852C;
  {$EXTERNALSYM GL_PRIMARY_COLOR_NV}
  GL_SECONDARY_COLOR_NV                             = $852D;
  {$EXTERNALSYM GL_SECONDARY_COLOR_NV}
  GL_SPARE0_NV                                      = $852E;
  {$EXTERNALSYM GL_SPARE0_NV}
  GL_SPARE1_NV                                      = $852F;
  {$EXTERNALSYM GL_SPARE1_NV}
  GL_DISCARD_NV                                     = $8530;
  {$EXTERNALSYM GL_DISCARD_NV}
  GL_E_TIMES_F_NV                                   = $8531;
  {$EXTERNALSYM GL_E_TIMES_F_NV}
  GL_SPARE0_PLUS_SECONDARY_COLOR_NV                 = $8532;
  {$EXTERNALSYM GL_SPARE0_PLUS_SECONDARY_COLOR_NV}
  GL_UNSIGNED_IDENTITY_NV                           = $8536;
  {$EXTERNALSYM GL_UNSIGNED_IDENTITY_NV}
  GL_UNSIGNED_INVERT_NV                             = $8537;
  {$EXTERNALSYM GL_UNSIGNED_INVERT_NV}
  GL_EXPAND_NORMAL_NV                               = $8538;
  {$EXTERNALSYM GL_EXPAND_NORMAL_NV}
  GL_EXPAND_NEGATE_NV                               = $8539;
  {$EXTERNALSYM GL_EXPAND_NEGATE_NV}
  GL_HALF_BIAS_NORMAL_NV                            = $853A;
  {$EXTERNALSYM GL_HALF_BIAS_NORMAL_NV}
  GL_HALF_BIAS_NEGATE_NV                            = $853B;
  {$EXTERNALSYM GL_HALF_BIAS_NEGATE_NV}
  GL_SIGNED_IDENTITY_NV                             = $853C;
  {$EXTERNALSYM GL_SIGNED_IDENTITY_NV}
  GL_SIGNED_NEGATE_NV                               = $853D;
  {$EXTERNALSYM GL_SIGNED_NEGATE_NV}
  GL_SCALE_BY_TWO_NV                                = $853E;
  {$EXTERNALSYM GL_SCALE_BY_TWO_NV}
  GL_SCALE_BY_FOUR_NV                               = $853F;
  {$EXTERNALSYM GL_SCALE_BY_FOUR_NV}
  GL_SCALE_BY_ONE_HALF_NV                           = $8540;
  {$EXTERNALSYM GL_SCALE_BY_ONE_HALF_NV}
  GL_BIAS_BY_NEGATIVE_ONE_HALF_NV                   = $8541;
  {$EXTERNALSYM GL_BIAS_BY_NEGATIVE_ONE_HALF_NV}
  GL_COMBINER_INPUT_NV                              = $8542;
  {$EXTERNALSYM GL_COMBINER_INPUT_NV}
  GL_COMBINER_MAPPING_NV                            = $8543;
  {$EXTERNALSYM GL_COMBINER_MAPPING_NV}
  GL_COMBINER_COMPONENT_USAGE_NV                    = $8544;
  {$EXTERNALSYM GL_COMBINER_COMPONENT_USAGE_NV}
  GL_COMBINER_AB_DOT_PRODUCT_NV                     = $8545;
  {$EXTERNALSYM GL_COMBINER_AB_DOT_PRODUCT_NV}
  GL_COMBINER_CD_DOT_PRODUCT_NV                     = $8546;
  {$EXTERNALSYM GL_COMBINER_CD_DOT_PRODUCT_NV}
  GL_COMBINER_MUX_SUM_NV                            = $8547;
  {$EXTERNALSYM GL_COMBINER_MUX_SUM_NV}
  GL_COMBINER_SCALE_NV                              = $8548;
  {$EXTERNALSYM GL_COMBINER_SCALE_NV}
  GL_COMBINER_BIAS_NV                               = $8549;
  {$EXTERNALSYM GL_COMBINER_BIAS_NV}
  GL_COMBINER_AB_OUTPUT_NV                          = $854A;
  {$EXTERNALSYM GL_COMBINER_AB_OUTPUT_NV}
  GL_COMBINER_CD_OUTPUT_NV                          = $854B;
  {$EXTERNALSYM GL_COMBINER_CD_OUTPUT_NV}
  GL_COMBINER_SUM_OUTPUT_NV                         = $854C;
  {$EXTERNALSYM GL_COMBINER_SUM_OUTPUT_NV}
  GL_MAX_GENERAL_COMBINERS_NV                       = $854D;
  {$EXTERNALSYM GL_MAX_GENERAL_COMBINERS_NV}
  GL_NUM_GENERAL_COMBINERS_NV                       = $854E;
  {$EXTERNALSYM GL_NUM_GENERAL_COMBINERS_NV}
  GL_COLOR_SUM_CLAMP_NV                             = $854F;
  {$EXTERNALSYM GL_COLOR_SUM_CLAMP_NV}
  GL_COMBINER0_NV                                   = $8550;
  {$EXTERNALSYM GL_COMBINER0_NV}
  GL_COMBINER1_NV                                   = $8551;
  {$EXTERNALSYM GL_COMBINER1_NV}
  GL_COMBINER2_NV                                   = $8552;
  {$EXTERNALSYM GL_COMBINER2_NV}
  GL_COMBINER3_NV                                   = $8553;
  {$EXTERNALSYM GL_COMBINER3_NV}
  GL_COMBINER4_NV                                   = $8554;
  {$EXTERNALSYM GL_COMBINER4_NV}
  GL_COMBINER5_NV                                   = $8555;
  {$EXTERNALSYM GL_COMBINER5_NV}
  GL_COMBINER6_NV                                   = $8556;
  {$EXTERNALSYM GL_COMBINER6_NV}
  GL_COMBINER7_NV                                   = $8557;
  {$EXTERNALSYM GL_COMBINER7_NV}

  // GL_NV_fog_distance
  GL_FOG_DISTANCE_MODE_NV                           = $855A;
  {$EXTERNALSYM GL_FOG_DISTANCE_MODE_NV}
  GL_EYE_RADIAL_NV                                  = $855B;
  {$EXTERNALSYM GL_EYE_RADIAL_NV}
  GL_EYE_PLANE_ABSOLUTE_NV                          = $855C;
  {$EXTERNALSYM GL_EYE_PLANE_ABSOLUTE_NV}

  // GL_NV_texgen_emboss
  GL_EMBOSS_LIGHT_NV                                = $855D;
  {$EXTERNALSYM GL_EMBOSS_LIGHT_NV}
  GL_EMBOSS_CONSTANT_NV                             = $855E;
  {$EXTERNALSYM GL_EMBOSS_CONSTANT_NV}
  GL_EMBOSS_MAP_NV                                  = $855F;
  {$EXTERNALSYM GL_EMBOSS_MAP_NV}

  // GL_EXT_texture_compression_s3tc
  GL_COMPRESSED_RGB_S3TC_DXT1_EXT                   = $83F0;
  {$EXTERNALSYM GL_COMPRESSED_RGB_S3TC_DXT1_EXT}
  GL_COMPRESSED_RGBA_S3TC_DXT1_EXT                  = $83F1;
  {$EXTERNALSYM GL_COMPRESSED_RGBA_S3TC_DXT1_EXT}
  GL_COMPRESSED_RGBA_S3TC_DXT3_EXT                  = $83F2;
  {$EXTERNALSYM GL_COMPRESSED_RGBA_S3TC_DXT3_EXT}
  GL_COMPRESSED_RGBA_S3TC_DXT5_EXT                  = $83F3;
  {$EXTERNALSYM GL_COMPRESSED_RGBA_S3TC_DXT5_EXT}

  // GL_IBM_cull_vertex
  GL_CULL_VERTEX_IBM                                = 103050;
  {$EXTERNALSYM GL_CULL_VERTEX_IBM}

  // GL_IBM_vertex_array_lists
  GL_VERTEX_ARRAY_LIST_IBM                          = 103070;
  {$EXTERNALSYM GL_VERTEX_ARRAY_LIST_IBM}
  GL_NORMAL_ARRAY_LIST_IBM                          = 103071;
  {$EXTERNALSYM GL_NORMAL_ARRAY_LIST_IBM}
  GL_COLOR_ARRAY_LIST_IBM                           = 103072;
  {$EXTERNALSYM GL_COLOR_ARRAY_LIST_IBM}
  GL_INDEX_ARRAY_LIST_IBM                           = 103073;
  {$EXTERNALSYM GL_INDEX_ARRAY_LIST_IBM}
  GL_TEXTURE_COORD_ARRAY_LIST_IBM                   = 103074;
  {$EXTERNALSYM GL_TEXTURE_COORD_ARRAY_LIST_IBM}
  GL_EDGE_FLAG_ARRAY_LIST_IBM                       = 103075;
  {$EXTERNALSYM GL_EDGE_FLAG_ARRAY_LIST_IBM}
  GL_FOG_COORDINATE_ARRAY_LIST_IBM                  = 103076;
  {$EXTERNALSYM GL_FOG_COORDINATE_ARRAY_LIST_IBM}
  GL_SECONDARY_COLOR_ARRAY_LIST_IBM                 = 103077;
  {$EXTERNALSYM GL_SECONDARY_COLOR_ARRAY_LIST_IBM}
  GL_VERTEX_ARRAY_LIST_STRIDE_IBM                   = 103080;
  {$EXTERNALSYM GL_VERTEX_ARRAY_LIST_STRIDE_IBM}
  GL_NORMAL_ARRAY_LIST_STRIDE_IBM                   = 103081;
  {$EXTERNALSYM GL_NORMAL_ARRAY_LIST_STRIDE_IBM}
  GL_COLOR_ARRAY_LIST_STRIDE_IBM                    = 103082;
  {$EXTERNALSYM GL_COLOR_ARRAY_LIST_STRIDE_IBM}
  GL_INDEX_ARRAY_LIST_STRIDE_IBM                    = 103083;
  {$EXTERNALSYM GL_INDEX_ARRAY_LIST_STRIDE_IBM}
  GL_TEXTURE_COORD_ARRAY_LIST_STRIDE_IBM            = 103084;
  {$EXTERNALSYM GL_TEXTURE_COORD_ARRAY_LIST_STRIDE_IBM}
  GL_EDGE_FLAG_ARRAY_LIST_STRIDE_IBM                = 103085;
  {$EXTERNALSYM GL_EDGE_FLAG_ARRAY_LIST_STRIDE_IBM}
  GL_FOG_COORDINATE_ARRAY_LIST_STRIDE_IBM           = 103086;
  {$EXTERNALSYM GL_FOG_COORDINATE_ARRAY_LIST_STRIDE_IBM}
  GL_SECONDARY_COLOR_ARRAY_LIST_STRIDE_IBM          = 103087;
  {$EXTERNALSYM GL_SECONDARY_COLOR_ARRAY_LIST_STRIDE_IBM}

  // GL_SGIX_subsample
  GL_PACK_SUBSAMPLE_RATE_SGIX                       = $85A0;
  {$EXTERNALSYM GL_PACK_SUBSAMPLE_RATE_SGIX}
  GL_UNPACK_SUBSAMPLE_RATE_SGIX                     = $85A1;
  {$EXTERNALSYM GL_UNPACK_SUBSAMPLE_RATE_SGIX}
  GL_PIXEL_SUBSAMPLE_4444_SGIX                      = $85A2;
  {$EXTERNALSYM GL_PIXEL_SUBSAMPLE_4444_SGIX}
  GL_PIXEL_SUBSAMPLE_2424_SGIX                      = $85A3;
  {$EXTERNALSYM GL_PIXEL_SUBSAMPLE_2424_SGIX}
  GL_PIXEL_SUBSAMPLE_4242_SGIX                      = $85A4;
  {$EXTERNALSYM GL_PIXEL_SUBSAMPLE_4242_SGIX}

  // GL_SGIX_ycrcba
  GL_YCRCB_SGIX                                     = $8318;
  {$EXTERNALSYM GL_YCRCB_SGIX}
  GL_YCRCBA_SGIX                                    = $8319;
  {$EXTERNALSYM GL_YCRCBA_SGIX}

  // GL_SGI_depth_pass_instrument
  GL_DEPTH_PASS_INSTRUMENT_SGIX                     = $8310;
  {$EXTERNALSYM GL_DEPTH_PASS_INSTRUMENT_SGIX}
  GL_DEPTH_PASS_INSTRUMENT_COUNTERS_SGIX            = $8311;
  {$EXTERNALSYM GL_DEPTH_PASS_INSTRUMENT_COUNTERS_SGIX}
  GL_DEPTH_PASS_INSTRUMENT_MAX_SGIX                 = $8312;
  {$EXTERNALSYM GL_DEPTH_PASS_INSTRUMENT_MAX_SGIX}

  // GL_3DFX_texture_compression_FXT1
  GL_COMPRESSED_RGB_FXT1_3DFX                       = $86B0;
  {$EXTERNALSYM GL_COMPRESSED_RGB_FXT1_3DFX}
  GL_COMPRESSED_RGBA_FXT1_3DFX                      = $86B1;
  {$EXTERNALSYM GL_COMPRESSED_RGBA_FXT1_3DFX}

  // GL_3DFX_multisample
  GL_MULTISAMPLE_3DFX                               = $86B2;
  {$EXTERNALSYM GL_MULTISAMPLE_3DFX}
  GL_SAMPLE_BUFFERS_3DFX                            = $86B3;
  {$EXTERNALSYM GL_SAMPLE_BUFFERS_3DFX}
  GL_SAMPLES_3DFX                                   = $86B4;
  {$EXTERNALSYM GL_SAMPLES_3DFX}
  GL_MULTISAMPLE_BIT_3DFX                           = $20000000;
  {$EXTERNALSYM GL_MULTISAMPLE_BIT_3DFX}

  // GL_EXT_multisample
  GL_MULTISAMPLE_EXT                                = $809D;
  {$EXTERNALSYM GL_MULTISAMPLE_EXT}
  GL_SAMPLE_ALPHA_TO_MASK_EXT                       = $809E;
  {$EXTERNALSYM GL_SAMPLE_ALPHA_TO_MASK_EXT}
  GL_SAMPLE_ALPHA_TO_ONE_EXT                        = $809F;
  {$EXTERNALSYM GL_SAMPLE_ALPHA_TO_ONE_EXT}
  GL_SAMPLE_MASK_EXT                                = $80A0;
  {$EXTERNALSYM GL_SAMPLE_MASK_EXT}
  GL_1PASS_EXT                                      = $80A1;
  {$EXTERNALSYM GL_1PASS_EXT}
  GL_2PASS_0_EXT                                    = $80A2;
  {$EXTERNALSYM GL_2PASS_0_EXT}
  GL_2PASS_1_EXT                                    = $80A3;
  {$EXTERNALSYM GL_2PASS_1_EXT}
  GL_4PASS_0_EXT                                    = $80A4;
  {$EXTERNALSYM GL_4PASS_0_EXT}
  GL_4PASS_1_EXT                                    = $80A5;
  {$EXTERNALSYM GL_4PASS_1_EXT}
  GL_4PASS_2_EXT                                    = $80A6;
  {$EXTERNALSYM GL_4PASS_2_EXT}
  GL_4PASS_3_EXT                                    = $80A7;
  {$EXTERNALSYM GL_4PASS_3_EXT}
  GL_SAMPLE_BUFFERS_EXT                             = $80A8;
  {$EXTERNALSYM GL_SAMPLE_BUFFERS_EXT}
  GL_SAMPLES_EXT                                    = $80A9;
  {$EXTERNALSYM GL_SAMPLES_EXT}
  GL_SAMPLE_MASK_VALUE_EXT                          = $80AA;
  {$EXTERNALSYM GL_SAMPLE_MASK_VALUE_EXT}
  GL_SAMPLE_MASK_INVERT_EXT                         = $80AB;
  {$EXTERNALSYM GL_SAMPLE_MASK_INVERT_EXT}
  GL_SAMPLE_PATTERN_EXT                             = $80AC;
  {$EXTERNALSYM GL_SAMPLE_PATTERN_EXT}

  // GL_SGIX_vertex_preclip
  GL_VERTEX_PRECLIP_SGIX                            = $83EE;
  {$EXTERNALSYM GL_VERTEX_PRECLIP_SGIX}
  GL_VERTEX_PRECLIP_HINT_SGIX                       = $83EF;
  {$EXTERNALSYM GL_VERTEX_PRECLIP_HINT_SGIX}

  // GL_SGIX_convolution_accuracy
  GL_CONVOLUTION_HINT_SGIX                          = $8316;
  {$EXTERNALSYM GL_CONVOLUTION_HINT_SGIX}

  // GL_SGIX_resample
  GL_PACK_RESAMPLE_SGIX                             = $842C;
  {$EXTERNALSYM GL_PACK_RESAMPLE_SGIX}
  GL_UNPACK_RESAMPLE_SGIX                           = $842D;
  {$EXTERNALSYM GL_UNPACK_RESAMPLE_SGIX}
  GL_RESAMPLE_REPLICATE_SGIX                        = $842E;
  {$EXTERNALSYM GL_RESAMPLE_REPLICATE_SGIX}
  GL_RESAMPLE_ZERO_FILL_SGIX                        = $842F;
  {$EXTERNALSYM GL_RESAMPLE_ZERO_FILL_SGIX}
  GL_RESAMPLE_DECIMATE_SGIX                         = $8430;
  {$EXTERNALSYM GL_RESAMPLE_DECIMATE_SGIX}

  // GL_SGIS_point_line_texgen
  GL_EYE_DISTANCE_TO_POINT_SGIS                     = $81F0;
  {$EXTERNALSYM GL_EYE_DISTANCE_TO_POINT_SGIS}
  GL_OBJECT_DISTANCE_TO_POINT_SGIS                  = $81F1;
  {$EXTERNALSYM GL_OBJECT_DISTANCE_TO_POINT_SGIS}
  GL_EYE_DISTANCE_TO_LINE_SGIS                      = $81F2;
  {$EXTERNALSYM GL_EYE_DISTANCE_TO_LINE_SGIS}
  GL_OBJECT_DISTANCE_TO_LINE_SGIS                   = $81F3;
  {$EXTERNALSYM GL_OBJECT_DISTANCE_TO_LINE_SGIS}
  GL_EYE_POINT_SGIS                                 = $81F4;
  {$EXTERNALSYM GL_EYE_POINT_SGIS}
  GL_OBJECT_POINT_SGIS                              = $81F5;
  {$EXTERNALSYM GL_OBJECT_POINT_SGIS}
  GL_EYE_LINE_SGIS                                  = $81F6;
  {$EXTERNALSYM GL_EYE_LINE_SGIS}
  GL_OBJECT_LINE_SGIS                               = $81F7;
  {$EXTERNALSYM GL_OBJECT_LINE_SGIS}

  // GL_SGIS_texture_color_mask
  GL_TEXTURE_COLOR_WRITEMASK_SGIS                   = $81EF;
  {$EXTERNALSYM GL_TEXTURE_COLOR_WRITEMASK_SGIS}

  // GL_NV_vertex_program
  GL_VERTEX_PROGRAM_NV                              = $8620;
  {$EXTERNALSYM GL_VERTEX_PROGRAM_NV}
  GL_VERTEX_STATE_PROGRAM_NV                        = $8621;
  {$EXTERNALSYM GL_VERTEX_STATE_PROGRAM_NV}
  GL_ATTRIB_ARRAY_SIZE_NV                           = $8623;
  {$EXTERNALSYM GL_ATTRIB_ARRAY_SIZE_NV}
  GL_ATTRIB_ARRAY_STRIDE_NV                         = $8624;
  {$EXTERNALSYM GL_ATTRIB_ARRAY_STRIDE_NV}
  GL_ATTRIB_ARRAY_TYPE_NV                           = $8625;
  {$EXTERNALSYM GL_ATTRIB_ARRAY_TYPE_NV}
  GL_CURRENT_ATTRIB_NV                              = $8626;
  {$EXTERNALSYM GL_CURRENT_ATTRIB_NV}
  GL_PROGRAM_LENGTH_NV                              = $8627;
  {$EXTERNALSYM GL_PROGRAM_LENGTH_NV}
  GL_PROGRAM_STRING_NV                              = $8628;
  {$EXTERNALSYM GL_PROGRAM_STRING_NV}
  GL_MODELVIEW_PROJECTION_NV                        = $8629;
  {$EXTERNALSYM GL_MODELVIEW_PROJECTION_NV}
  GL_IDENTITY_NV                                    = $862A;
  {$EXTERNALSYM GL_IDENTITY_NV}
  GL_INVERSE_NV                                     = $862B;
  {$EXTERNALSYM GL_INVERSE_NV}
  GL_TRANSPOSE_NV                                   = $862C;
  {$EXTERNALSYM GL_TRANSPOSE_NV}
  GL_INVERSE_TRANSPOSE_NV                           = $862D;
  {$EXTERNALSYM GL_INVERSE_TRANSPOSE_NV}
  GL_MAX_TRACK_MATRIX_STACK_DEPTH_NV                = $862E;
  {$EXTERNALSYM GL_MAX_TRACK_MATRIX_STACK_DEPTH_NV}
  GL_MAX_TRACK_MATRICES_NV                          = $862F;
  {$EXTERNALSYM GL_MAX_TRACK_MATRICES_NV}
  GL_MATRIX0_NV                                     = $8630;
  {$EXTERNALSYM GL_MATRIX0_NV}
  GL_MATRIX1_NV                                     = $8631;
  {$EXTERNALSYM GL_MATRIX1_NV}
  GL_MATRIX2_NV                                     = $8632;
  {$EXTERNALSYM GL_MATRIX2_NV}
  GL_MATRIX3_NV                                     = $8633;
  {$EXTERNALSYM GL_MATRIX3_NV}
  GL_MATRIX4_NV                                     = $8634;
  {$EXTERNALSYM GL_MATRIX4_NV}
  GL_MATRIX5_NV                                     = $8635;
  {$EXTERNALSYM GL_MATRIX5_NV}
  GL_MATRIX6_NV                                     = $8636;
  {$EXTERNALSYM GL_MATRIX6_NV}
  GL_MATRIX7_NV                                     = $8637;
  {$EXTERNALSYM GL_MATRIX7_NV}
  GL_CURRENT_MATRIX_STACK_DEPTH_NV                  = $8640;
  {$EXTERNALSYM GL_CURRENT_MATRIX_STACK_DEPTH_NV}
  GL_CURRENT_MATRIX_NV                              = $8641;
  {$EXTERNALSYM GL_CURRENT_MATRIX_NV}
  GL_VERTEX_PROGRAM_POINT_SIZE_NV                   = $8642;
  {$EXTERNALSYM GL_VERTEX_PROGRAM_POINT_SIZE_NV}
  GL_VERTEX_PROGRAM_TWO_SIDE_NV                     = $8643;
  {$EXTERNALSYM GL_VERTEX_PROGRAM_TWO_SIDE_NV}
  GL_PROGRAM_PARAMETER_NV                           = $8644;
  {$EXTERNALSYM GL_PROGRAM_PARAMETER_NV}
  GL_ATTRIB_ARRAY_POINTER_NV                        = $8645;
  {$EXTERNALSYM GL_ATTRIB_ARRAY_POINTER_NV}
  GL_PROGRAM_TARGET_NV                              = $8646;
  {$EXTERNALSYM GL_PROGRAM_TARGET_NV}
  GL_PROGRAM_RESIDENT_NV                            = $8647;
  {$EXTERNALSYM GL_PROGRAM_RESIDENT_NV}
  GL_TRACK_MATRIX_NV                                = $8648;
  {$EXTERNALSYM GL_TRACK_MATRIX_NV}
  GL_TRACK_MATRIX_TRANSFORM_NV                      = $8649;
  {$EXTERNALSYM GL_TRACK_MATRIX_TRANSFORM_NV}
  GL_VERTEX_PROGRAM_BINDING_NV                      = $864A;
  {$EXTERNALSYM GL_VERTEX_PROGRAM_BINDING_NV}
  GL_PROGRAM_ERROR_POSITION_NV                      = $864B;
  {$EXTERNALSYM GL_PROGRAM_ERROR_POSITION_NV}
  GL_VERTEX_ATTRIB_ARRAY0_NV                        = $8650;
  {$EXTERNALSYM GL_VERTEX_ATTRIB_ARRAY0_NV}
  GL_VERTEX_ATTRIB_ARRAY1_NV                        = $8651;
  {$EXTERNALSYM GL_VERTEX_ATTRIB_ARRAY1_NV}
  GL_VERTEX_ATTRIB_ARRAY2_NV                        = $8652;
  {$EXTERNALSYM GL_VERTEX_ATTRIB_ARRAY2_NV}
  GL_VERTEX_ATTRIB_ARRAY3_NV                        = $8653;
  {$EXTERNALSYM GL_VERTEX_ATTRIB_ARRAY3_NV}
  GL_VERTEX_ATTRIB_ARRAY4_NV                        = $8654;
  {$EXTERNALSYM GL_VERTEX_ATTRIB_ARRAY4_NV}
  GL_VERTEX_ATTRIB_ARRAY5_NV                        = $8655;
  {$EXTERNALSYM GL_VERTEX_ATTRIB_ARRAY5_NV}
  GL_VERTEX_ATTRIB_ARRAY6_NV                        = $8656;
  {$EXTERNALSYM GL_VERTEX_ATTRIB_ARRAY6_NV}
  GL_VERTEX_ATTRIB_ARRAY7_NV                        = $8657;
  {$EXTERNALSYM GL_VERTEX_ATTRIB_ARRAY7_NV}
  GL_VERTEX_ATTRIB_ARRAY8_NV                        = $8658;
  {$EXTERNALSYM GL_VERTEX_ATTRIB_ARRAY8_NV}
  GL_VERTEX_ATTRIB_ARRAY9_NV                        = $8659;
  {$EXTERNALSYM GL_VERTEX_ATTRIB_ARRAY9_NV}
  GL_VERTEX_ATTRIB_ARRAY10_NV                       = $865A;
  {$EXTERNALSYM GL_VERTEX_ATTRIB_ARRAY10_NV}
  GL_VERTEX_ATTRIB_ARRAY11_NV                       = $865B;
  {$EXTERNALSYM GL_VERTEX_ATTRIB_ARRAY11_NV}
  GL_VERTEX_ATTRIB_ARRAY12_NV                       = $865C;
  {$EXTERNALSYM GL_VERTEX_ATTRIB_ARRAY12_NV}
  GL_VERTEX_ATTRIB_ARRAY13_NV                       = $865D;
  {$EXTERNALSYM GL_VERTEX_ATTRIB_ARRAY13_NV}
  GL_VERTEX_ATTRIB_ARRAY14_NV                       = $865E;
  {$EXTERNALSYM GL_VERTEX_ATTRIB_ARRAY14_NV}
  GL_VERTEX_ATTRIB_ARRAY15_NV                       = $865F;
  {$EXTERNALSYM GL_VERTEX_ATTRIB_ARRAY15_NV}
  GL_MAP1_VERTEX_ATTRIB0_4_NV                       = $8660;
  {$EXTERNALSYM GL_MAP1_VERTEX_ATTRIB0_4_NV}
  GL_MAP1_VERTEX_ATTRIB1_4_NV                       = $8661;
  {$EXTERNALSYM GL_MAP1_VERTEX_ATTRIB1_4_NV}
  GL_MAP1_VERTEX_ATTRIB2_4_NV                       = $8662;
  {$EXTERNALSYM GL_MAP1_VERTEX_ATTRIB2_4_NV}
  GL_MAP1_VERTEX_ATTRIB3_4_NV                       = $8663;
  {$EXTERNALSYM GL_MAP1_VERTEX_ATTRIB3_4_NV}
  GL_MAP1_VERTEX_ATTRIB4_4_NV                       = $8664;
  {$EXTERNALSYM GL_MAP1_VERTEX_ATTRIB4_4_NV}
  GL_MAP1_VERTEX_ATTRIB5_4_NV                       = $8665;
  {$EXTERNALSYM GL_MAP1_VERTEX_ATTRIB5_4_NV}
  GL_MAP1_VERTEX_ATTRIB6_4_NV                       = $8666;
  {$EXTERNALSYM GL_MAP1_VERTEX_ATTRIB6_4_NV}
  GL_MAP1_VERTEX_ATTRIB7_4_NV                       = $8667;
  {$EXTERNALSYM GL_MAP1_VERTEX_ATTRIB7_4_NV}
  GL_MAP1_VERTEX_ATTRIB8_4_NV                       = $8668;
  {$EXTERNALSYM GL_MAP1_VERTEX_ATTRIB8_4_NV}
  GL_MAP1_VERTEX_ATTRIB9_4_NV                       = $8669;
  {$EXTERNALSYM GL_MAP1_VERTEX_ATTRIB9_4_NV}
  GL_MAP1_VERTEX_ATTRIB10_4_NV                      = $866A;
  {$EXTERNALSYM GL_MAP1_VERTEX_ATTRIB10_4_NV}
  GL_MAP1_VERTEX_ATTRIB11_4_NV                      = $866B;
  {$EXTERNALSYM GL_MAP1_VERTEX_ATTRIB11_4_NV}
  GL_MAP1_VERTEX_ATTRIB12_4_NV                      = $866C;
  {$EXTERNALSYM GL_MAP1_VERTEX_ATTRIB12_4_NV}
  GL_MAP1_VERTEX_ATTRIB13_4_NV                      = $866D;
  {$EXTERNALSYM GL_MAP1_VERTEX_ATTRIB13_4_NV}
  GL_MAP1_VERTEX_ATTRIB14_4_NV                      = $866E;
  {$EXTERNALSYM GL_MAP1_VERTEX_ATTRIB14_4_NV}
  GL_MAP1_VERTEX_ATTRIB15_4_NV                      = $866F;
  {$EXTERNALSYM GL_MAP1_VERTEX_ATTRIB15_4_NV}
  GL_MAP2_VERTEX_ATTRIB0_4_NV                       = $8670;
  {$EXTERNALSYM GL_MAP2_VERTEX_ATTRIB0_4_NV}
  GL_MAP2_VERTEX_ATTRIB1_4_NV                       = $8671;
  {$EXTERNALSYM GL_MAP2_VERTEX_ATTRIB1_4_NV}
  GL_MAP2_VERTEX_ATTRIB2_4_NV                       = $8672;
  {$EXTERNALSYM GL_MAP2_VERTEX_ATTRIB2_4_NV}
  GL_MAP2_VERTEX_ATTRIB3_4_NV                       = $8673;
  {$EXTERNALSYM GL_MAP2_VERTEX_ATTRIB3_4_NV}
  GL_MAP2_VERTEX_ATTRIB4_4_NV                       = $8674;
  {$EXTERNALSYM GL_MAP2_VERTEX_ATTRIB4_4_NV}
  GL_MAP2_VERTEX_ATTRIB5_4_NV                       = $8675;
  {$EXTERNALSYM GL_MAP2_VERTEX_ATTRIB5_4_NV}
  GL_MAP2_VERTEX_ATTRIB6_4_NV                       = $8676;
  {$EXTERNALSYM GL_MAP2_VERTEX_ATTRIB6_4_NV}
  GL_MAP2_VERTEX_ATTRIB7_4_NV                       = $8677;
  {$EXTERNALSYM GL_MAP2_VERTEX_ATTRIB7_4_NV}
  GL_MAP2_VERTEX_ATTRIB8_4_NV                       = $8678;
  {$EXTERNALSYM GL_MAP2_VERTEX_ATTRIB8_4_NV}
  GL_MAP2_VERTEX_ATTRIB9_4_NV                       = $8679;
  {$EXTERNALSYM GL_MAP2_VERTEX_ATTRIB9_4_NV}
  GL_MAP2_VERTEX_ATTRIB10_4_NV                      = $867A;
  {$EXTERNALSYM GL_MAP2_VERTEX_ATTRIB10_4_NV}
  GL_MAP2_VERTEX_ATTRIB11_4_NV                      = $867B;
  {$EXTERNALSYM GL_MAP2_VERTEX_ATTRIB11_4_NV}
  GL_MAP2_VERTEX_ATTRIB12_4_NV                      = $867C;
  {$EXTERNALSYM GL_MAP2_VERTEX_ATTRIB12_4_NV}
  GL_MAP2_VERTEX_ATTRIB13_4_NV                      = $867D;
  {$EXTERNALSYM GL_MAP2_VERTEX_ATTRIB13_4_NV}
  GL_MAP2_VERTEX_ATTRIB14_4_NV                      = $867E;
  {$EXTERNALSYM GL_MAP2_VERTEX_ATTRIB14_4_NV}
  GL_MAP2_VERTEX_ATTRIB15_4_NV                      = $867F;
  {$EXTERNALSYM GL_MAP2_VERTEX_ATTRIB15_4_NV}

  // WGL_ARB_pixel_format
  WGL_NUMBER_PIXEL_FORMATS_ARB                      = $2000;
  {$EXTERNALSYM WGL_NUMBER_PIXEL_FORMATS_ARB}
  WGL_DRAW_TO_WINDOW_ARB                            = $2001;
  {$EXTERNALSYM WGL_DRAW_TO_WINDOW_ARB}
  WGL_DRAW_TO_BITMAP_ARB                            = $2002;
  {$EXTERNALSYM WGL_DRAW_TO_BITMAP_ARB}
  WGL_ACCELERATION_ARB                              = $2003;
  {$EXTERNALSYM WGL_ACCELERATION_ARB}
  WGL_NEED_PALETTE_ARB                              = $2004;
  {$EXTERNALSYM WGL_NEED_PALETTE_ARB}
  WGL_NEED_SYSTEM_PALETTE_ARB                       = $2005;
  {$EXTERNALSYM WGL_NEED_SYSTEM_PALETTE_ARB}
  WGL_SWAP_LAYER_BUFFERS_ARB                        = $2006;
  {$EXTERNALSYM WGL_SWAP_LAYER_BUFFERS_ARB}
  WGL_SWAP_METHOD_ARB                               = $2007;
  {$EXTERNALSYM WGL_SWAP_METHOD_ARB}
  WGL_NUMBER_OVERLAYS_ARB                           = $2008;
  {$EXTERNALSYM WGL_NUMBER_OVERLAYS_ARB}
  WGL_NUMBER_UNDERLAYS_ARB                          = $2009;
  {$EXTERNALSYM WGL_NUMBER_UNDERLAYS_ARB}
  WGL_TRANSPARENT_ARB                               = $200A;
  {$EXTERNALSYM WGL_TRANSPARENT_ARB}
  WGL_TRANSPARENT_RED_VALUE_ARB                     = $2037;
  {$EXTERNALSYM WGL_TRANSPARENT_RED_VALUE_ARB}
  WGL_TRANSPARENT_GREEN_VALUE_ARB                   = $2038;
  {$EXTERNALSYM WGL_TRANSPARENT_GREEN_VALUE_ARB}
  WGL_TRANSPARENT_BLUE_VALUE_ARB                    = $2039;
  {$EXTERNALSYM WGL_TRANSPARENT_BLUE_VALUE_ARB}
  WGL_TRANSPARENT_ALPHA_VALUE_ARB                   = $203A;
  {$EXTERNALSYM WGL_TRANSPARENT_ALPHA_VALUE_ARB}
  WGL_TRANSPARENT_INDEX_VALUE_ARB                   = $203B;
  {$EXTERNALSYM WGL_TRANSPARENT_INDEX_VALUE_ARB}
  WGL_SHARE_DEPTH_ARB                               = $200C;
  {$EXTERNALSYM WGL_SHARE_DEPTH_ARB}
  WGL_SHARE_STENCIL_ARB                             = $200D;
  {$EXTERNALSYM WGL_SHARE_STENCIL_ARB}
  WGL_SHARE_ACCUM_ARB                               = $200E;
  {$EXTERNALSYM WGL_SHARE_ACCUM_ARB}
  WGL_SUPPORT_GDI_ARB                               = $200F;
  {$EXTERNALSYM WGL_SUPPORT_GDI_ARB}
  WGL_SUPPORT_OPENGL_ARB                            = $2010;
  {$EXTERNALSYM WGL_SUPPORT_OPENGL_ARB}
  WGL_DOUBLE_BUFFER_ARB                             = $2011;
  {$EXTERNALSYM WGL_DOUBLE_BUFFER_ARB}
  WGL_STEREO_ARB                                    = $2012;
  {$EXTERNALSYM WGL_STEREO_ARB}
  WGL_PIXEL_TYPE_ARB                                = $2013;
  {$EXTERNALSYM WGL_PIXEL_TYPE_ARB}
  WGL_COLOR_BITS_ARB                                = $2014;
  {$EXTERNALSYM WGL_COLOR_BITS_ARB}
  WGL_RED_BITS_ARB                                  = $2015;
  {$EXTERNALSYM WGL_RED_BITS_ARB}
  WGL_RED_SHIFT_ARB                                 = $2016;
  {$EXTERNALSYM WGL_RED_SHIFT_ARB}
  WGL_GREEN_BITS_ARB                                = $2017;
  {$EXTERNALSYM WGL_GREEN_BITS_ARB}
  WGL_GREEN_SHIFT_ARB                               = $2018;
  {$EXTERNALSYM WGL_GREEN_SHIFT_ARB}
  WGL_BLUE_BITS_ARB                                 = $2019;
  {$EXTERNALSYM WGL_BLUE_BITS_ARB}
  WGL_BLUE_SHIFT_ARB                                = $201A;
  {$EXTERNALSYM WGL_BLUE_SHIFT_ARB}
  WGL_ALPHA_BITS_ARB                                = $201B;
  {$EXTERNALSYM WGL_ALPHA_BITS_ARB}
  WGL_ALPHA_SHIFT_ARB                               = $201C;
  {$EXTERNALSYM WGL_ALPHA_SHIFT_ARB}
  WGL_ACCUM_BITS_ARB                                = $201D;
  {$EXTERNALSYM WGL_ACCUM_BITS_ARB}
  WGL_ACCUM_RED_BITS_ARB                            = $201E;
  {$EXTERNALSYM WGL_ACCUM_RED_BITS_ARB}
  WGL_ACCUM_GREEN_BITS_ARB                          = $201F;
  {$EXTERNALSYM WGL_ACCUM_GREEN_BITS_ARB}
  WGL_ACCUM_BLUE_BITS_ARB                           = $2020;
  {$EXTERNALSYM WGL_ACCUM_BLUE_BITS_ARB}
  WGL_ACCUM_ALPHA_BITS_ARB                          = $2021;
  {$EXTERNALSYM WGL_ACCUM_ALPHA_BITS_ARB}
  WGL_DEPTH_BITS_ARB                                = $2022;
  {$EXTERNALSYM WGL_DEPTH_BITS_ARB}
  WGL_STENCIL_BITS_ARB                              = $2023;
  {$EXTERNALSYM WGL_STENCIL_BITS_ARB}
  WGL_AUX_BUFFERS_ARB                               = $2024;
  {$EXTERNALSYM WGL_AUX_BUFFERS_ARB}
  WGL_NO_ACCELERATION_ARB                           = $2025;
  {$EXTERNALSYM WGL_NO_ACCELERATION_ARB}
  WGL_GENERIC_ACCELERATION_ARB                      = $2026;
  {$EXTERNALSYM WGL_GENERIC_ACCELERATION_ARB}
  WGL_FULL_ACCELERATION_ARB                         = $2027;
  {$EXTERNALSYM WGL_FULL_ACCELERATION_ARB}
  WGL_SWAP_EXCHANGE_ARB                             = $2028;
  {$EXTERNALSYM WGL_SWAP_EXCHANGE_ARB}
  WGL_SWAP_COPY_ARB                                 = $2029;
  {$EXTERNALSYM WGL_SWAP_COPY_ARB}
  WGL_SWAP_UNDEFINED_ARB                            = $202A;
  {$EXTERNALSYM WGL_SWAP_UNDEFINED_ARB}
  WGL_TYPE_RGBA_ARB                                 = $202B;
  {$EXTERNALSYM WGL_TYPE_RGBA_ARB}
  WGL_TYPE_COLORINDEX_ARB                           = $202C;
  {$EXTERNALSYM WGL_TYPE_COLORINDEX_ARB}

  
  // ********** GLU generic constants **********

  // Errors: (return value 0 = no error)
  GLU_INVALID_ENUM                                  = 100900;
  {$EXTERNALSYM GLU_INVALID_ENUM}
  GLU_INVALID_VALUE                                 = 100901;
  {$EXTERNALSYM GLU_INVALID_VALUE}
  GLU_OUT_OF_MEMORY                                 = 100902;
  {$EXTERNALSYM GLU_OUT_OF_MEMORY}
  GLU_INCOMPATIBLE_GL_VERSION                       = 100903;
  {$EXTERNALSYM GLU_INCOMPATIBLE_GL_VERSION}

  // StringName
  GLU_VERSION                                       = 100800;
  {$EXTERNALSYM GLU_VERSION}
  GLU_EXTENSIONS                                    = 100801;
  {$EXTERNALSYM GLU_EXTENSIONS}

  // Boolean
  GLU_TRUE                                          = GL_TRUE;
  {$EXTERNALSYM GLU_TRUE}
  GLU_FALSE                                         = GL_FALSE;
  {$EXTERNALSYM GLU_FALSE}

  // Quadric constants
  // QuadricNormal
  GLU_SMOOTH                                        = 100000;
  {$EXTERNALSYM GLU_SMOOTH}
  GLU_FLAT                                          = 100001;
  {$EXTERNALSYM GLU_FLAT}
  GLU_NONE                                          = 100002;
  {$EXTERNALSYM GLU_NONE}

  // QuadricDrawStyle
  GLU_POINT                                         = 100010;
  {$EXTERNALSYM GLU_POINT}
  GLU_LINE                                          = 100011;
  {$EXTERNALSYM GLU_LINE}
  GLU_FILL                                          = 100012;
  {$EXTERNALSYM GLU_FILL}
  GLU_SILHOUETTE                                    = 100013;
  {$EXTERNALSYM GLU_SILHOUETTE}

  // QuadricOrientation
  GLU_OUTSIDE                                       = 100020;
  {$EXTERNALSYM GLU_OUTSIDE}
  GLU_INSIDE                                        = 100021;
  {$EXTERNALSYM GLU_INSIDE}

  // Tesselation constants
  GLU_TESS_MAX_COORD                                = 1.0e150;
  {$EXTERNALSYM GLU_TESS_MAX_COORD}

  // TessProperty
  GLU_TESS_WINDING_RULE                             = 100140;
  {$EXTERNALSYM GLU_TESS_WINDING_RULE}
  GLU_TESS_BOUNDARY_ONLY                            = 100141;
  {$EXTERNALSYM GLU_TESS_BOUNDARY_ONLY}
  GLU_TESS_TOLERANCE                                = 100142;
  {$EXTERNALSYM GLU_TESS_TOLERANCE}

  // TessWinding
  GLU_TESS_WINDING_ODD                              = 100130;
  {$EXTERNALSYM GLU_TESS_WINDING_ODD}
  GLU_TESS_WINDING_NONZERO                          = 100131;
  {$EXTERNALSYM GLU_TESS_WINDING_NONZERO}
  GLU_TESS_WINDING_POSITIVE                         = 100132;
  {$EXTERNALSYM GLU_TESS_WINDING_POSITIVE}
  GLU_TESS_WINDING_NEGATIVE                         = 100133;
  {$EXTERNALSYM GLU_TESS_WINDING_NEGATIVE}
  GLU_TESS_WINDING_ABS_GEQ_TWO                      = 100134;
  {$EXTERNALSYM GLU_TESS_WINDING_ABS_GEQ_TWO}

  // TessCallback
  GLU_TESS_BEGIN                                    = 100100; // TGLUTessBeginProc
  {$EXTERNALSYM GLU_TESS_BEGIN}
  GLU_TESS_VERTEX                                   = 100101; // TGLUTessVertexProc
  {$EXTERNALSYM GLU_TESS_VERTEX}
  GLU_TESS_END                                      = 100102; // TGLUTessEndProc
  {$EXTERNALSYM GLU_TESS_END}
  GLU_TESS_ERROR                                    = 100103; // TGLUTessErrorProc
  {$EXTERNALSYM GLU_TESS_ERROR}
  GLU_TESS_EDGE_FLAG                                = 100104; // TGLUTessEdgeFlagProc
  {$EXTERNALSYM GLU_TESS_EDGE_FLAG}
  GLU_TESS_COMBINE                                  = 100105; // TGLUTessCombineProc
  {$EXTERNALSYM GLU_TESS_COMBINE}
  GLU_TESS_BEGIN_DATA                               = 100106; // TGLUTessBeginDataProc
  {$EXTERNALSYM GLU_TESS_BEGIN_DATA}
  GLU_TESS_VERTEX_DATA                              = 100107; // TGLUTessVertexDataProc
  {$EXTERNALSYM GLU_TESS_VERTEX_DATA}
  GLU_TESS_END_DATA                                 = 100108; // TGLUTessEndDataProc
  {$EXTERNALSYM GLU_TESS_END_DATA}
  GLU_TESS_ERROR_DATA                               = 100109; // TGLUTessErrorDataProc
  {$EXTERNALSYM GLU_TESS_ERROR_DATA}
  GLU_TESS_EDGE_FLAG_DATA                           = 100110; // TGLUTessEdgeFlagDataProc
  {$EXTERNALSYM GLU_TESS_EDGE_FLAG_DATA}
  GLU_TESS_COMBINE_DATA                             = 100111; // TGLUTessCombineDataProc
  {$EXTERNALSYM GLU_TESS_COMBINE_DATA}

  // TessError
  GLU_TESS_ERROR1                                   = 100151;
  {$EXTERNALSYM GLU_TESS_ERROR1}
  GLU_TESS_ERROR2                                   = 100152;
  {$EXTERNALSYM GLU_TESS_ERROR2}
  GLU_TESS_ERROR3                                   = 100153;
  {$EXTERNALSYM GLU_TESS_ERROR3}
  GLU_TESS_ERROR4                                   = 100154;
  {$EXTERNALSYM GLU_TESS_ERROR4}
  GLU_TESS_ERROR5                                   = 100155;
  {$EXTERNALSYM GLU_TESS_ERROR5}
  GLU_TESS_ERROR6                                   = 100156;
  {$EXTERNALSYM GLU_TESS_ERROR6}
  GLU_TESS_ERROR7                                   = 100157;
  {$EXTERNALSYM GLU_TESS_ERROR7}
  GLU_TESS_ERROR8                                   = 100158;
  {$EXTERNALSYM GLU_TESS_ERROR8}

  GLU_TESS_MISSING_BEGIN_POLYGON                    = GLU_TESS_ERROR1;
  {$EXTERNALSYM GLU_TESS_MISSING_BEGIN_POLYGON}
  GLU_TESS_MISSING_BEGIN_CONTOUR                    = GLU_TESS_ERROR2;
  {$EXTERNALSYM GLU_TESS_MISSING_BEGIN_CONTOUR}
  GLU_TESS_MISSING_END_POLYGON                      = GLU_TESS_ERROR3;
  {$EXTERNALSYM GLU_TESS_MISSING_END_POLYGON}
  GLU_TESS_MISSING_END_CONTOUR                      = GLU_TESS_ERROR4;
  {$EXTERNALSYM GLU_TESS_MISSING_END_CONTOUR}
  GLU_TESS_COORD_TOO_LARGE                          = GLU_TESS_ERROR5;
  {$EXTERNALSYM GLU_TESS_COORD_TOO_LARGE}
  GLU_TESS_NEED_COMBINE_CALLBACK                    = GLU_TESS_ERROR6;
  {$EXTERNALSYM GLU_TESS_NEED_COMBINE_CALLBACK}

  // NURBS constants

  // NurbsProperty
  GLU_AUTO_LOAD_MATRIX                              = 100200;
  {$EXTERNALSYM GLU_AUTO_LOAD_MATRIX}
  GLU_CULLING                                       = 100201;
  {$EXTERNALSYM GLU_CULLING}
  GLU_SAMPLING_TOLERANCE                            = 100203;
  {$EXTERNALSYM GLU_SAMPLING_TOLERANCE}
  GLU_DISPLAY_MODE                                  = 100204;
  {$EXTERNALSYM GLU_DISPLAY_MODE}
  GLU_PARAMETRIC_TOLERANCE                          = 100202;
  {$EXTERNALSYM GLU_PARAMETRIC_TOLERANCE}
  GLU_SAMPLING_METHOD                               = 100205;
  {$EXTERNALSYM GLU_SAMPLING_METHOD}
  GLU_U_STEP                                        = 100206;
  {$EXTERNALSYM GLU_U_STEP}
  GLU_V_STEP                                        = 100207;
  {$EXTERNALSYM GLU_V_STEP}

  // NurbsSampling
  GLU_PATH_LENGTH                                   = 100215;
  {$EXTERNALSYM GLU_PATH_LENGTH}
  GLU_PARAMETRIC_ERROR                              = 100216;
  {$EXTERNALSYM GLU_PARAMETRIC_ERROR}
  GLU_DOMAIN_DISTANCE                               = 100217;
  {$EXTERNALSYM GLU_DOMAIN_DISTANCE}

  // NurbsTrim
  GLU_MAP1_TRIM_2                                   = 100210;
  {$EXTERNALSYM GLU_MAP1_TRIM_2}
  GLU_MAP1_TRIM_3                                   = 100211;
  {$EXTERNALSYM GLU_MAP1_TRIM_3}

  // NurbsDisplay
  GLU_OUTLINE_POLYGON                               = 100240;
  {$EXTERNALSYM GLU_OUTLINE_POLYGON}
  GLU_OUTLINE_PATCH                                 = 100241;
  {$EXTERNALSYM GLU_OUTLINE_PATCH}

  // NurbsErrors
  GLU_NURBS_ERROR1                                  = 100251;
  {$EXTERNALSYM GLU_NURBS_ERROR1}
  GLU_NURBS_ERROR2                                  = 100252;
  {$EXTERNALSYM GLU_NURBS_ERROR2}
  GLU_NURBS_ERROR3                                  = 100253;
  {$EXTERNALSYM GLU_NURBS_ERROR3}
  GLU_NURBS_ERROR4                                  = 100254;
  {$EXTERNALSYM GLU_NURBS_ERROR4}
  GLU_NURBS_ERROR5                                  = 100255;
  {$EXTERNALSYM GLU_NURBS_ERROR5}
  GLU_NURBS_ERROR6                                  = 100256;
  {$EXTERNALSYM GLU_NURBS_ERROR6}
  GLU_NURBS_ERROR7                                  = 100257;
  {$EXTERNALSYM GLU_NURBS_ERROR7}
  GLU_NURBS_ERROR8                                  = 100258;
  {$EXTERNALSYM GLU_NURBS_ERROR8}
  GLU_NURBS_ERROR9                                  = 100259;
  {$EXTERNALSYM GLU_NURBS_ERROR9}
  GLU_NURBS_ERROR10                                 = 100260;
  {$EXTERNALSYM GLU_NURBS_ERROR10}
  GLU_NURBS_ERROR11                                 = 100261;
  {$EXTERNALSYM GLU_NURBS_ERROR11}
  GLU_NURBS_ERROR12                                 = 100262;
  {$EXTERNALSYM GLU_NURBS_ERROR12}
  GLU_NURBS_ERROR13                                 = 100263;
  {$EXTERNALSYM GLU_NURBS_ERROR13}
  GLU_NURBS_ERROR14                                 = 100264;
  {$EXTERNALSYM GLU_NURBS_ERROR14}
  GLU_NURBS_ERROR15                                 = 100265;
  {$EXTERNALSYM GLU_NURBS_ERROR15}
  GLU_NURBS_ERROR16                                 = 100266;
  {$EXTERNALSYM GLU_NURBS_ERROR16}
  GLU_NURBS_ERROR17                                 = 100267;
  {$EXTERNALSYM GLU_NURBS_ERROR17}
  GLU_NURBS_ERROR18                                 = 100268;
  {$EXTERNALSYM GLU_NURBS_ERROR18}
  GLU_NURBS_ERROR19                                 = 100269;
  {$EXTERNALSYM GLU_NURBS_ERROR19}
  GLU_NURBS_ERROR20                                 = 100270;
  {$EXTERNALSYM GLU_NURBS_ERROR20}
  GLU_NURBS_ERROR21                                 = 100271;
  {$EXTERNALSYM GLU_NURBS_ERROR21}
  GLU_NURBS_ERROR22                                 = 100272;
  {$EXTERNALSYM GLU_NURBS_ERROR22}
  GLU_NURBS_ERROR23                                 = 100273;
  {$EXTERNALSYM GLU_NURBS_ERROR23}
  GLU_NURBS_ERROR24                                 = 100274;
  {$EXTERNALSYM GLU_NURBS_ERROR24}
  GLU_NURBS_ERROR25                                 = 100275;
  {$EXTERNALSYM GLU_NURBS_ERROR25}
  GLU_NURBS_ERROR26                                 = 100276;
  {$EXTERNALSYM GLU_NURBS_ERROR26}
  GLU_NURBS_ERROR27                                 = 100277;
  {$EXTERNALSYM GLU_NURBS_ERROR27}
  GLU_NURBS_ERROR28                                 = 100278;
  {$EXTERNALSYM GLU_NURBS_ERROR28}
  GLU_NURBS_ERROR29                                 = 100279;
  {$EXTERNALSYM GLU_NURBS_ERROR29}
  GLU_NURBS_ERROR30                                 = 100280;
  {$EXTERNALSYM GLU_NURBS_ERROR30}
  GLU_NURBS_ERROR31                                 = 100281;
  {$EXTERNALSYM GLU_NURBS_ERROR31}
  GLU_NURBS_ERROR32                                 = 100282;
  {$EXTERNALSYM GLU_NURBS_ERROR32}
  GLU_NURBS_ERROR33                                 = 100283;
  {$EXTERNALSYM GLU_NURBS_ERROR33}
  GLU_NURBS_ERROR34                                 = 100284;
  {$EXTERNALSYM GLU_NURBS_ERROR34}
  GLU_NURBS_ERROR35                                 = 100285;
  {$EXTERNALSYM GLU_NURBS_ERROR35}
  GLU_NURBS_ERROR36                                 = 100286;
  {$EXTERNALSYM GLU_NURBS_ERROR36}
  GLU_NURBS_ERROR37                                 = 100287;
  {$EXTERNALSYM GLU_NURBS_ERROR37}

  // Contours types -- obsolete!
  GLU_CW                                            = 100120;
  {$EXTERNALSYM GLU_CW}
  GLU_CCW                                           = 100121;
  {$EXTERNALSYM GLU_CCW}
  GLU_INTERIOR                                      = 100122;
  {$EXTERNALSYM GLU_INTERIOR}
  GLU_EXTERIOR                                      = 100123;
  {$EXTERNALSYM GLU_EXTERIOR}
  GLU_UNKNOWN                                       = 100124;
  {$EXTERNALSYM GLU_UNKNOWN}

  // Names without "TESS_" prefix
  GLU_BEGIN                                         = GLU_TESS_BEGIN;
  {$EXTERNALSYM GLU_BEGIN}
  GLU_VERTEX                                        = GLU_TESS_VERTEX;
  {$EXTERNALSYM GLU_VERTEX}
  GLU_END                                           = GLU_TESS_END;
  {$EXTERNALSYM GLU_END}
  GLU_ERROR                                         = GLU_TESS_ERROR;
  {$EXTERNALSYM GLU_ERROR}
  GLU_EDGE_FLAG                                     = GLU_TESS_EDGE_FLAG;
  {$EXTERNALSYM GLU_EDGE_FLAG}

  GLX_VERSION_1_1                                   = 1;
  GLX_VERSION_1_2                                   = 1;
  GLX_VERSION_1_3                                   = 1;
  GLX_EXTENSION_NAME                                = 'GLX';
  {$EXTERNALSYM GLX_EXTENSION_NAME}
  GLX_USE_GL                                        = 1;
  {$EXTERNALSYM GLX_USE_GL}
  GLX_BUFFER_SIZE                                   = 2;
  {$EXTERNALSYM GLX_BUFFER_SIZE}
  GLX_LEVEL                                         = 3;
  {$EXTERNALSYM GLX_LEVEL}
  GLX_RGBA                                          = 4;
  {$EXTERNALSYM GLX_RGBA}
  GLX_DOUBLEBUFFER                                  = 5;
  {$EXTERNALSYM GLX_DOUBLEBUFFER}
  GLX_STEREO                                        = 6;
  {$EXTERNALSYM GLX_STEREO}
  GLX_AUX_BUFFERS                                   = 7;
  {$EXTERNALSYM GLX_AUX_BUFFERS}
  GLX_RED_SIZE                                      = 8;
  {$EXTERNALSYM GLX_RED_SIZE}
  GLX_GREEN_SIZE                                    = 9;
  {$EXTERNALSYM GLX_GREEN_SIZE}
  GLX_BLUE_SIZE                                     = 10;
  {$EXTERNALSYM GLX_BLUE_SIZE}
  GLX_ALPHA_SIZE                                    = 11;
  {$EXTERNALSYM GLX_ALPHA_SIZE}
  GLX_DEPTH_SIZE                                    = 12;
  {$EXTERNALSYM GLX_DEPTH_SIZE}
  GLX_STENCIL_SIZE                                  = 13;
  {$EXTERNALSYM GLX_STENCIL_SIZE}
  GLX_ACCUM_RED_SIZE                                = 14;
  {$EXTERNALSYM GLX_ACCUM_RED_SIZE}
  GLX_ACCUM_GREEN_SIZE                              = 15;
  {$EXTERNALSYM GLX_ACCUM_GREEN_SIZE}
  GLX_ACCUM_BLUE_SIZE                               = 16;
  {$EXTERNALSYM GLX_ACCUM_BLUE_SIZE}
  GLX_ACCUM_ALPHA_SIZE                              = 17;
  {$EXTERNALSYM GLX_ACCUM_ALPHA_SIZE}

  // Error codes returned by glXGetConfig:
  GLX_BAD_SCREEN                                    = 1;
  {$EXTERNALSYM GLX_BAD_SCREEN}
  GLX_BAD_ATTRIBUTE                                 = 2;
  {$EXTERNALSYM GLX_BAD_ATTRIBUTE}
  GLX_NO_EXTENSION                                  = 3;
  {$EXTERNALSYM GLX_NO_EXTENSION}
  GLX_BAD_VISUAL                                    = 4;
  {$EXTERNALSYM GLX_BAD_VISUAL}
  GLX_BAD_CONTEXT                                   = 5;
  {$EXTERNALSYM GLX_BAD_CONTEXT}
  GLX_BAD_VALUE                                     = 6;
  {$EXTERNALSYM GLX_BAD_VALUE}
  GLX_BAD_ENUM                                      = 7;
  {$EXTERNALSYM GLX_BAD_ENUM}

  // GLX 1.1 and later:
  GLX_VENDOR                                        = 1;
  {$EXTERNALSYM GLX_VENDOR}
  GLX_VERSION                                       = 2;
  {$EXTERNALSYM GLX_VERSION}
  GLX_EXTENSIONS                                    = 3;
  {$EXTERNALSYM GLX_EXTENSIONS}

  // GLX 1.3 and later:
  GLX_CONFIG_CAVEAT                                 = $20;
  {$EXTERNALSYM GLX_CONFIG_CAVEAT}
  GLX_DONT_CARE                                     = $FFFFFFFF;
  {$EXTERNALSYM GLX_DONT_CARE}
  GLX_SLOW_CONFIG                                   = $8001;
  {$EXTERNALSYM GLX_SLOW_CONFIG}
  GLX_NON_CONFORMANT_CONFIG                         = $800D;
  {$EXTERNALSYM GLX_NON_CONFORMANT_CONFIG}
  GLX_X_VISUAL_TYPE                                 = $22;
  {$EXTERNALSYM GLX_X_VISUAL_TYPE}
  GLX_TRANSPARENT_TYPE                              = $23;
  {$EXTERNALSYM GLX_TRANSPARENT_TYPE}
  GLX_TRANSPARENT_INDEX_VALUE                       = $24;
  {$EXTERNALSYM GLX_TRANSPARENT_INDEX_VALUE}
  GLX_TRANSPARENT_RED_VALUE                         = $25;
  {$EXTERNALSYM GLX_TRANSPARENT_RED_VALUE}
  GLX_TRANSPARENT_GREEN_VALUE                       = $26;
  {$EXTERNALSYM GLX_TRANSPARENT_GREEN_VALUE}
  GLX_TRANSPARENT_BLUE_VALUE                        = $27;
  {$EXTERNALSYM GLX_TRANSPARENT_BLUE_VALUE}
  GLX_TRANSPARENT_ALPHA_VALUE                       = $28;
  {$EXTERNALSYM GLX_TRANSPARENT_ALPHA_VALUE}
  GLX_MAX_PBUFFER_WIDTH                             = $8016;
  {$EXTERNALSYM GLX_MAX_PBUFFER_WIDTH}
  GLX_MAX_PBUFFER_HEIGHT                            = $8017;
  {$EXTERNALSYM GLX_MAX_PBUFFER_HEIGHT}
  GLX_MAX_PBUFFER_PIXELS                            = $8018;
  {$EXTERNALSYM GLX_MAX_PBUFFER_PIXELS}
  GLX_PRESERVED_CONTENTS                            = $801B;
  {$EXTERNALSYM GLX_PRESERVED_CONTENTS}
  GLX_LARGEST_BUFFER                                = $801C;
  {$EXTERNALSYM GLX_LARGEST_BUFFER}
  GLX_DRAWABLE_TYPE                                 = $8010;
  {$EXTERNALSYM GLX_DRAWABLE_TYPE}
  GLX_FBCONFIG_ID                                   = $8013;
  {$EXTERNALSYM GLX_FBCONFIG_ID}
  GLX_VISUAL_ID                                     = $800B;
  {$EXTERNALSYM GLX_VISUAL_ID}
  GLX_WINDOW_BIT                                    = $00000001;
  {$EXTERNALSYM GLX_WINDOW_BIT}
  GLX_PIXMAP_BIT                                    = $00000002;
  {$EXTERNALSYM GLX_PIXMAP_BIT}
  GLX_PBUFFER_BIT                                   = $00000004;
  {$EXTERNALSYM GLX_PBUFFER_BIT}
  GLX_AUX_BUFFERS_BIT                               = $00000010;
  {$EXTERNALSYM GLX_AUX_BUFFERS_BIT}
  GLX_FRONT_LEFT_BUFFER_BIT                         = $00000001;
  {$EXTERNALSYM GLX_FRONT_LEFT_BUFFER_BIT}
  GLX_FRONT_RIGHT_BUFFER_BIT                        = $00000002;
  {$EXTERNALSYM GLX_FRONT_RIGHT_BUFFER_BIT}
  GLX_BACK_LEFT_BUFFER_BIT                          = $00000004;
  {$EXTERNALSYM GLX_BACK_LEFT_BUFFER_BIT}
  GLX_BACK_RIGHT_BUFFER_BIT                         = $00000008;
  {$EXTERNALSYM GLX_BACK_RIGHT_BUFFER_BIT}
  GLX_DEPTH_BUFFER_BIT                              = $00000020;
  {$EXTERNALSYM GLX_DEPTH_BUFFER_BIT}
  GLX_STENCIL_BUFFER_BIT                            = $00000040;
  {$EXTERNALSYM GLX_STENCIL_BUFFER_BIT}
  GLX_ACCUM_BUFFER_BIT                              = $00000080;
  {$EXTERNALSYM GLX_ACCUM_BUFFER_BIT}
  GLX_RENDER_TYPE                                   = $8011;
  {$EXTERNALSYM GLX_RENDER_TYPE}
  GLX_X_RENDERABLE                                  = $8012;
  {$EXTERNALSYM GLX_X_RENDERABLE}
  GLX_NONE                                          = $8000;
  {$EXTERNALSYM GLX_NONE}
  GLX_TRUE_COLOR                                    = $8002;
  {$EXTERNALSYM GLX_TRUE_COLOR}
  GLX_DIRECT_COLOR                                  = $8003;
  {$EXTERNALSYM GLX_DIRECT_COLOR}
  GLX_PSEUDO_COLOR                                  = $8004;
  {$EXTERNALSYM GLX_PSEUDO_COLOR}
  GLX_STATIC_COLOR                                  = $8005;
  {$EXTERNALSYM GLX_STATIC_COLOR}
  GLX_GRAY_SCALE                                    = $8006;
  {$EXTERNALSYM GLX_GRAY_SCALE}
  GLX_STATIC_GRAY                                   = $8007;
  {$EXTERNALSYM GLX_STATIC_GRAY}
  GLX_TRANSPARENT_INDEX                             = $8009;
  {$EXTERNALSYM GLX_TRANSPARENT_INDEX}
  GLX_COLOR_INDEX_TYPE                              = $8015;
  {$EXTERNALSYM GLX_COLOR_INDEX_TYPE}
  GLX_COLOR_INDEX_BIT                               = $00000002;
  {$EXTERNALSYM GLX_COLOR_INDEX_BIT}
  GLX_SCREEN                                        = $800C;
  {$EXTERNALSYM GLX_SCREEN}
  GLX_PBUFFER_CLOBBER_MASK                          = $08000000;
  {$EXTERNALSYM GLX_PBUFFER_CLOBBER_MASK}
  GLX_DAMAGED                                       = $8020;
  {$EXTERNALSYM GLX_DAMAGED}
  GLX_SAVED                                         = $8021;
  {$EXTERNALSYM GLX_SAVED}
  GLX_WINDOW                                        = $8022;
  {$EXTERNALSYM GLX_WINDOW}
  GLX_PBUFFER                                       = $8023;
  {$EXTERNALSYM GLX_PBUFFER}
  GLX_EXT_visual_info                               = 1;
  {$EXTERNALSYM GLX_EXT_visual_info}
  GLX_X_VISUAL_TYPE_EXT                             = $22;
  {$EXTERNALSYM GLX_X_VISUAL_TYPE_EXT}
  GLX_TRANSPARENT_TYPE_EXT                          = $23;
  {$EXTERNALSYM GLX_TRANSPARENT_TYPE_EXT}
  GLX_TRANSPARENT_INDEX_VALUE_EXT                   = $24;
  {$EXTERNALSYM GLX_TRANSPARENT_INDEX_VALUE_EXT}
  GLX_TRANSPARENT_RED_VALUE_EXT                     = $25;
  {$EXTERNALSYM GLX_TRANSPARENT_RED_VALUE_EXT}
  GLX_TRANSPARENT_GREEN_VALUE_EXT                   = $26;
  {$EXTERNALSYM GLX_TRANSPARENT_GREEN_VALUE_EXT}
  GLX_TRANSPARENT_BLUE_VALUE_EXT                    = $27;
  {$EXTERNALSYM GLX_TRANSPARENT_BLUE_VALUE_EXT}
  GLX_TRANSPARENT_ALPHA_VALUE_EXT                   = $28;
  {$EXTERNALSYM GLX_TRANSPARENT_ALPHA_VALUE_EXT}
  GLX_TRUE_COLOR_EXT                                = $8002;
  {$EXTERNALSYM GLX_TRUE_COLOR_EXT}
  GLX_DIRECT_COLOR_EXT                              = $8003;
  {$EXTERNALSYM GLX_DIRECT_COLOR_EXT}
  GLX_PSEUDO_COLOR_EXT                              = $8004;
  {$EXTERNALSYM GLX_PSEUDO_COLOR_EXT}
  GLX_STATIC_COLOR_EXT                              = $8005;
  {$EXTERNALSYM GLX_STATIC_COLOR_EXT}
  GLX_GRAY_SCALE_EXT                                = $8006;
  {$EXTERNALSYM GLX_GRAY_SCALE_EXT}
  GLX_STATIC_GRAY_EXT                               = $8007;
  {$EXTERNALSYM GLX_STATIC_GRAY_EXT}
  GLX_NONE_EXT                                      = $8000;
  {$EXTERNALSYM GLX_NONE_EXT}
  GLX_TRANSPARENT_RGB_EXT                           = $8008;
  {$EXTERNALSYM GLX_TRANSPARENT_RGB_EXT}
  GLX_TRANSPARENT_INDEX_EXT                         = $8009;
  {$EXTERNALSYM GLX_TRANSPARENT_INDEX_EXT}
  GLX_VISUAL_CAVEAT_EXT                             = $20;
  {$EXTERNALSYM GLX_VISUAL_CAVEAT_EXT}
  GLX_SLOW_VISUAL_EXT                               = $8001;
  {$EXTERNALSYM GLX_SLOW_VISUAL_EXT}
  GLX_NON_CONFORMANT_VISUAL_EXT                     = $800D;
  {$EXTERNALSYM GLX_NON_CONFORMANT_VISUAL_EXT}
  GLX_SHARE_CONTEXT_EXT                             = $800A;
  {$EXTERNALSYM GLX_SHARE_CONTEXT_EXT}
  GLX_VISUAL_ID_EXT                                 = $800B;
  {$EXTERNALSYM GLX_VISUAL_ID_EXT}
  GLX_SCREEN_EXT                                    = $800C;
  {$EXTERNALSYM GLX_SCREEN_EXT}
  GLX_3DFX_WINDOW_MODE_MESA                         = $1;
  {$EXTERNALSYM GLX_3DFX_WINDOW_MODE_MESA}
  GLX_3DFX_FULLSCREEN_MODE_MESA                     = $2;
  {$EXTERNALSYM GLX_3DFX_FULLSCREEN_MODE_MESA}


type
  // GLU types
  TGLUNurbs = record end; 
  TGLUQuadric = record end; 
  TGLUTesselator = record end; 

  PGLUNurbs = ^TGLUNurbs; 
  PGLUQuadric = ^TGLUQuadric; 
  PGLUTesselator = ^TGLUTesselator; 

  // backwards compatibility
  TGLUNurbsObj = TGLUNurbs; 
  TGLUQuadricObj = TGLUQuadric; 
  TGLUTesselatorObj = TGLUTesselator;
  TGLUTriangulatorObj = TGLUTesselator;

  PGLUNurbsObj = PGLUNurbs;
  PGLUQuadricObj = PGLUQuadric;
  PGLUTesselatorObj = PGLUTesselator;
  PGLUTriangulatorObj = PGLUTesselator;

  {$IFDEF FPC}
    {$IFDEF UNIX}
    PALETTEENTRY = record
       peRed : BYTE;
       peGreen : BYTE;
       peBlue : BYTE;
       peFlags : BYTE;
    end;
    LPPALETTEENTRY = ^PALETTEENTRY;
    tagPALETTEENTRY = PALETTEENTRY;
    TPaletteEntry = PALETTEENTRY;
    PPALETTEENTRY = ^PALETTEENTRY;
    xid = txid;
    pixmap = tpixmap;
    font = tfont;
    window = twindow;
    colormap = tcolormap;
  {$ENDIF}

  PMaxLogPalette = ^TMaxLogPalette;
  TMaxLogPalette = packed record
    palversion : word;
    palnumentries : word;
    palpalentry : array[byte] of TPaletteEntry;
  end;

  {$ifdef WIN32} // If Windows
  PWGLSwap = ^TWGLSwap;
  {$EXTERNALSYM _WGLSWAP}
  _WGLSWAP = packed record
    hdc: HDC;
    uiFlags: Cardinal;
  end;
  TWGLSwap = _WGLSWAP;
  {$EXTERNALSYM WGLSWAP}
  WGLSWAP = _WGLSWAP;
  {$endif WIN32}

  {$ENDIF}

  {$ifdef VER100} // Delphi 3
  PWGLSwap = ^TWGLSwap;
  {$EXTERNALSYM _WGLSWAP}
  _WGLSWAP = packed record
    hdc: HDC;
    uiFlags: Cardinal;
  end;
  TWGLSwap = _WGLSWAP;
  {$EXTERNALSYM WGLSWAP}
  WGLSWAP = _WGLSWAP;
  {$endif VER100}

  // Callback function prototypes
  // GLUQuadricCallback
  TGLUQuadricErrorProc = procedure(errorCode: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}

  // GLUTessCallback
  TGLUTessBeginProc = procedure(AType: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  TGLUTessEdgeFlagProc = procedure(Flag: TGLboolean); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  TGLUTessVertexProc = procedure(VertexData: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  TGLUTessEndProc = procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  TGLUTessErrorProc = procedure(ErrNo: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  TGLUTessCombineProc = procedure(Coords: TVector3d; VertexData: TVector4p; Weight: TVector4f; OutData: PPointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  TGLUTessBeginDataProc = procedure(AType: TGLEnum; UserData: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  TGLUTessEdgeFlagDataProc = procedure(Flag: TGLboolean; UserData: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  TGLUTessVertexDataProc = procedure(VertexData: Pointer; UserData: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  TGLUTessEndDataProc = procedure(UserData: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  TGLUTessErrorDataProc = procedure(ErrNo: TGLEnum; UserData: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  TGLUTessCombineDataProc = procedure(Coords: TVector3d; VertexData: TVector4p; Weight: TVector4f; OutData: PPointer; UserData: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}

  // GLUNurbsCallback
  TGLUNurbsErrorProc = procedure(ErrorCode: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}

var
  // GL functions and procedures
  glAccum: procedure(op: TGLuint; value: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glAccum}
  glAlphaFunc: procedure(func: TGLEnum; ref: TGLclampf); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glAlphaFunc}
  glAreTexturesResident: function(n: TGLsizei; Textures: PGLuint; residences: PGLboolean): TGLboolean; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glAreTexturesResident}
  glArrayElement: procedure(i: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glArrayElement}
  glBegin: procedure(mode: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBegin}
  glBindTexture: procedure(target: TGLEnum; texture: TGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBindTexture}
  glBitmap: procedure(width: TGLsizei; height: TGLsizei; xorig, yorig: TGLfloat; xmove: TGLfloat; ymove: TGLfloat; bitmap: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBitmap}
  glBlendFunc: procedure(sfactor: TGLEnum; dfactor: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBlendFunc}
  glCallList: procedure(list: TGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCallList}
  glCallLists: procedure(n: TGLsizei; atype: TGLEnum; lists: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCallLists}
  glClear: procedure(mask: TGLbitfield); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glClear}
  glClearAccum: procedure(red, green, blue, alpha: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glClearAccum}
  glClearColor: procedure(red, green, blue, alpha: TGLclampf); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glClearColor}
  glClearDepth: procedure(depth: TGLclampd); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glClearDepth}
  glClearIndex: procedure(c: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glClearIndex}
  glClearStencil: procedure(s: TGLint ); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glClearStencil}
  glClipPlane: procedure(plane: TGLEnum; equation: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glClipPlane}
  glColor3b: procedure(red, green, blue: TGLbyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor3b}
  glColor3bv: procedure(v: PGLbyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor3bv}
  glColor3d: procedure(red, green, blue: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor3d}
  glColor3dv: procedure(v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor3dv}
  glColor3f: procedure(red, green, blue: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor3f}
  glColor3fv: procedure(v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor3fv}
  glColor3i: procedure(red, green, blue: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor3i}
  glColor3iv: procedure(v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor3iv}
  glColor3s: procedure(red, green, blue: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor3s}
  glColor3sv: procedure(v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor3sv}
  glColor3ub: procedure(red, green, blue: TGLubyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor3ub}
  glColor3ubv: procedure(v: PGLubyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor3ubv}
  glColor3ui: procedure(red, green, blue: TGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor3ui}
  glColor3uiv: procedure(v: PGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor3uiv}
  glColor3us: procedure(red, green, blue: TGLushort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor3us}
  glColor3usv: procedure(v: PGLushort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor3usv}
  glColor4b: procedure(red, green, blue, alpha: TGLbyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4b}
  glColor4bv: procedure(v: PGLbyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4bv}
  glColor4d: procedure(red, green, blue, alpha: TGLdouble ); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4d}
  glColor4dv: procedure(v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4dv}
  glColor4f: procedure(red, green, blue, alpha: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4f}
  glColor4fv: procedure(v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4fv}
  glColor4i: procedure(red, green, blue, alpha: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4i}
  glColor4iv: procedure(v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4iv}
  glColor4s: procedure(red, green, blue, alpha: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4s}
  glColor4sv: procedure(v: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4sv}
  glColor4ub: procedure(red, green, blue, alpha: TGLubyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4ub}
  glColor4ubv: procedure(v: PGLubyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4ubv}
  glColor4ui: procedure(red, green, blue, alpha: TGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4ui}
  glColor4uiv: procedure(v: PGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4uiv}
  glColor4us: procedure(red, green, blue, alpha: TGLushort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4us}
  glColor4usv: procedure(v: PGLushort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4usv}
  glColorMask: procedure(red, green, blue, alpha: TGLboolean); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColorMask}
  glColorMaterial: procedure(face: TGLEnum; mode: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColorMaterial}
  glColorPointer: procedure(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColorPointer}
  glCopyPixels: procedure(x, y: TGLint; width, height: TGLsizei; atype: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCopyPixels}
  glCopyTexImage1D: procedure(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width: TGLsizei; border: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCopyTexImage1D}
  glCopyTexImage2D: procedure(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width, height: TGLsizei; border: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCopyTexImage2D}
  glCopyTexSubImage1D: procedure(target: TGLEnum; level, xoffset, x, y: TGLint; width: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCopyTexSubImage1D}
  glCopyTexSubImage2D: procedure(target: TGLEnum; level, xoffset, yoffset, x, y: TGLint; width, height: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCopyTexSubImage2D}
  glCullFace: procedure(mode: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCullFace}
  glDeleteLists: procedure(list: TGLuint; range: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glDeleteLists}
  glDeleteTextures: procedure(n: TGLsizei; textures: PGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glDeleteTextures}
  glDepthFunc: procedure(func: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glDepthFunc}
  glDepthMask: procedure(flag: TGLboolean); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glDepthMask}
  glDepthRange: procedure(zNear, zFar: TGLclampd); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glDepthRange}
  glDisable: procedure(cap: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glDisable}
  glDisableClientState: procedure(aarray: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glDisableClientState}
  glDrawArrays: procedure(mode: TGLEnum; first: TGLint; count: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glDrawArrays}
  glDrawBuffer: procedure(mode: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glDrawBuffer}
  glDrawElements: procedure(mode: TGLEnum; count: TGLsizei; atype: TGLEnum; indices: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glDrawElements}
  glDrawPixels: procedure(width, height: TGLsizei; format, atype: TGLEnum; pixels: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glDrawPixels}
  glEdgeFlag: procedure(flag: TGLboolean); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEdgeFlag}
  glEdgeFlagPointer: procedure(stride: TGLsizei; data: pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEdgeFlagPointer}
  glEdgeFlagv: procedure(flag: PGLboolean); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEdgeFlagv}
  glEnable: procedure(cap: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEnable}
  glEnableClientState: procedure(aarray: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEnableClientState}
  glEnd: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEnd}
  glEndList: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEndList}
  glEvalCoord1d: procedure(u: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEvalCoord1d}
  glEvalCoord1dv: procedure(u: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEvalCoord1dv}
  glEvalCoord1f: procedure(u: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEvalCoord1f}
  glEvalCoord1fv: procedure(u: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEvalCoord1fv}
  glEvalCoord2d: procedure(u: TGLdouble; v: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEvalCoord2d}
  glEvalCoord2dv: procedure(u: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEvalCoord2dv}
  glEvalCoord2f: procedure(u, v: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEvalCoord2f}
  glEvalCoord2fv: procedure(u: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEvalCoord2fv}
  glEvalMesh1: procedure(mode: TGLEnum; i1, i2: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEvalMesh1}
  glEvalMesh2: procedure(mode: TGLEnum; i1, i2, j1, j2: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEvalMesh2}
  glEvalPoint1: procedure(i: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEvalPoint1}
  glEvalPoint2: procedure(i, j: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEvalPoint2}
  glFeedbackBuffer: procedure(size: TGLsizei; atype: TGLEnum; buffer: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFeedbackBuffer}
  glFinish: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFinish}
  glFlush: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFlush}
  glFogf: procedure(pname: TGLEnum; param: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFogf}
  glFogfv: procedure(pname: TGLEnum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFogfv}
  glFogi: procedure(pname: TGLEnum; param: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFogi}
  glFogiv: procedure(pname: TGLEnum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFogiv}
  glFrontFace: procedure(mode: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFrontFace}
  glFrustum: procedure(left, right, bottom, top, zNear, zFar: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFrustum}
  glGenLists: function(range: TGLsizei): TGLuint; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGenLists}
  glGenTextures: procedure(n: TGLsizei; textures: PGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGenTextures}
  glGetBooleanv: procedure(pname: TGLEnum; params: PGLboolean); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetBooleanv}
  glGetClipPlane: procedure(plane: TGLEnum; equation: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetClipPlane}
  glGetDoublev: procedure(pname: TGLEnum; params: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetDoublev}
  glGetError: function: TGLuint; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetError}
  glGetFloatv: procedure(pname: TGLEnum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetFloatv}
  glGetIntegerv: procedure(pname: TGLEnum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetIntegerv}
  glGetLightfv: procedure(light, pname: TGLEnum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetLightfv}
  glGetLightiv: procedure(light, pname: TGLEnum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetLightiv}
  glGetMapdv: procedure(target, query: TGLEnum; v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetMapdv}
  glGetMapfv: procedure(target, query: TGLEnum; v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetMapfv}
  glGetMapiv: procedure(target, query: TGLEnum; v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetMapiv}
  glGetMaterialfv: procedure(face, pname: TGLEnum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetMaterialfv}
  glGetMaterialiv: procedure(face, pname: TGLEnum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetMaterialiv}
  glGetPixelMapfv: procedure(map: TGLEnum; values: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetPixelMapfv}
  glGetPixelMapuiv: procedure(map: TGLEnum; values: PGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetPixelMapuiv}
  glGetPixelMapusv: procedure(map: TGLEnum; values: PGLushort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetPixelMapusv}
  glGetPointerv: procedure(pname: TGLEnum; var params); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetPointerv}
  glGetPolygonStipple: procedure(mask: PGLubyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetPolygonStipple}
  glGetString: function(name: TGLEnum): PChar; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetString}
  glGetTexEnvfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetTexEnvfv}
  glGetTexEnviv: procedure(target, pname: TGLEnum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetTexEnviv}
  glGetTexGendv: procedure(coord, pname: TGLEnum; params: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetTexGendv}
  glGetTexGenfv: procedure(coord, pname: TGLEnum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetTexGenfv}
  glGetTexGeniv: procedure(coord, pname: TGLEnum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetTexGeniv}
  glGetTexImage: procedure(target: TGLEnum; level: TGLint; format, atype: TGLEnum; pixels: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetTexImage}
  glGetTexLevelParameterfv: procedure(target: TGLEnum; level: TGLint; pname: TGLEnum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetTexLevelParameterfv}
  glGetTexLevelParameteriv: procedure(target: TGLEnum; level: TGLint; pname: TGLEnum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetTexLevelParameteriv}
  glGetTexParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetTexParameterfv}
  glGetTexParameteriv: procedure(target, pname: TGLEnum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetTexParameteriv}
  glHint: procedure(target, mode: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glHint}
  glIndexMask: procedure(mask: TGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIndexMask}
  glIndexPointer: procedure(atype: TGLEnum; stride: TGLsizei; data: pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIndexPointer}
  glIndexd: procedure(c: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIndexd}
  glIndexdv: procedure(c: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIndexdv}
  glIndexf: procedure(c: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIndexf}
  glIndexfv: procedure(c: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIndexfv}
  glIndexi: procedure(c: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIndexi}
  glIndexiv: procedure(c: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIndexiv}
  glIndexs: procedure(c: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIndexs}
  glIndexsv: procedure(c: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIndexsv}
  glIndexub: procedure(c: TGLubyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIndexub}
  glIndexubv: procedure(c: PGLubyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIndexubv}
  glInitNames: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glInitNames}
  glInterleavedArrays: procedure(format: TGLEnum; stride: TGLsizei; data: pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glInterleavedArrays}
  glIsEnabled: function(cap: TGLEnum): TGLboolean; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIsEnabled}
  glIsList: function(list: TGLuint): TGLboolean; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIsList}
  glIsTexture: function(texture: TGLuint): TGLboolean; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIsTexture}
  glLightModelf: procedure(pname: TGLEnum; param: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLightModelf}
  glLightModelfv: procedure(pname: TGLEnum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLightModelfv}
  glLightModeli: procedure(pname: TGLEnum; param: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLightModeli}
  glLightModeliv: procedure(pname: TGLEnum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLightModeliv}
  glLightf: procedure(light, pname: TGLEnum; param: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLightf}
  glLightfv: procedure(light, pname: TGLEnum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLightfv}
  glLighti: procedure(light, pname: TGLEnum; param: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLighti}
  glLightiv: procedure(light, pname: TGLEnum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLightiv}
  glLineStipple: procedure(factor: TGLint; pattern: TGLushort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLineStipple}
  glLineWidth: procedure(width: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLineWidth}
  glListBase: procedure(base: TGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glListBase}
  glLoadIdentity: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLoadIdentity}
  glLoadMatrixd: procedure(m: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLoadMatrixd}
  glLoadMatrixf: procedure(m: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLoadMatrixf}
  glLoadName: procedure(name: TGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLoadName}
  glLogicOp: procedure(opcode: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLogicOp}
  glMap1d: procedure(target: TGLEnum; u1, u2: TGLdouble; stride, order: TGLint; points: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMap1d}
  glMap1f: procedure(target: TGLEnum; u1, u2: TGLfloat; stride, order: TGLint; points: PGLfloat);   {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMap1f}
  glMap2d: procedure(target: TGLEnum; u1, u2: TGLdouble; ustride, uorder: TGLint; v1, v2: TGLdouble; vstride,
    vorder: TGLint; points: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMap2d}
  glMap2f: procedure(target: TGLEnum; u1, u2: TGLfloat; ustride, uorder: TGLint; v1, v2: TGLfloat; vstride,
    vorder: TGLint; points: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMap2f}
  glMapGrid1d: procedure(un: TGLint; u1, u2: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMapGrid1d}
  glMapGrid1f: procedure(un: TGLint; u1, u2: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMapGrid1f}
  glMapGrid2d: procedure(un: TGLint; u1, u2: TGLdouble; vn: TGLint; v1, v2: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMapGrid2d}
  glMapGrid2f: procedure(un: TGLint; u1, u2: TGLfloat; vn: TGLint; v1, v2: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMapGrid2f}
  glMaterialf: procedure(face, pname: TGLEnum; param: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMaterialf}
  glMaterialfv: procedure(face, pname: TGLEnum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMaterialfv}
  glMateriali: procedure(face, pname: TGLEnum; param: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMateriali}
  glMaterialiv: procedure(face, pname: TGLEnum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMaterialiv}
  glMatrixMode: procedure(mode: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMatrixMode}
  glMultMatrixd: procedure(m: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultMatrixd}
  glMultMatrixf: procedure(m: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultMatrixf}
  glNewList: procedure(list: TGLuint; mode: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glNewList}
  glNormal3b: procedure(nx, ny, nz: TGLbyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glNormal3b}
  glNormal3bv: procedure(v: PGLbyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glNormal3bv}
  glNormal3d: procedure(nx, ny, nz: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glNormal3d}
  glNormal3dv: procedure(v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glNormal3dv}
  glNormal3f: procedure(nx, ny, nz: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glNormal3f}
  glNormal3fv: procedure(v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glNormal3fv}
  glNormal3i: procedure(nx, ny, nz: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glNormal3i}
  glNormal3iv: procedure(v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glNormal3iv}
  glNormal3s: procedure(nx, ny, nz: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glNormal3s}
  glNormal3sv: procedure(v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glNormal3sv}
  glNormalPointer: procedure(atype: TGLEnum; stride: TGLsizei; data: pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glNormalPointer}
  glOrtho: procedure(left, right, bottom, top, zNear, zFar: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glOrtho}
  glPassThrough: procedure(token: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPassThrough}
  glPixelMapfv: procedure(map: TGLEnum; mapsize: TGLsizei; values: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPixelMapfv}
  glPixelMapuiv: procedure(map: TGLEnum; mapsize: TGLsizei; values: PGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPixelMapuiv}
  glPixelMapusv: procedure(map: TGLEnum; mapsize: TGLsizei; values: PGLushort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPixelMapusv}
  glPixelStoref: procedure(pname: TGLEnum; param: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPixelStoref}
  glPixelStorei: procedure(pname: TGLEnum; param: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPixelStorei}
  glPixelTransferf: procedure(pname: TGLEnum; param: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPixelTransferf}
  glPixelTransferi: procedure(pname: TGLEnum; param: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPixelTransferi}
  glPixelZoom: procedure(xfactor, yfactor: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPixelZoom}
  glPointSize: procedure(size: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPointSize}
  glPolygonMode: procedure(face, mode: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPolygonMode}
  glPolygonOffset: procedure(factor, units: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPolygonOffset}
  glPolygonStipple: procedure(mask: PGLubyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPolygonStipple}
  glPopAttrib: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPopAttrib}
  glPopClientAttrib: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPopClientAttrib}
  glPopMatrix: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPopMatrix}
  glPopName: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPopName}
  glPrioritizeTextures: procedure(n: TGLsizei; textures: PGLuint; priorities: PGLclampf); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPrioritizeTextures}
  glPushAttrib: procedure(mask: TGLbitfield); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPushAttrib}
  glPushClientAttrib: procedure(mask: TGLbitfield); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPushClientAttrib}
  glPushMatrix: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPushMatrix}
  glPushName: procedure(name: TGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPushName}
  glRasterPos2d: procedure(x, y: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos2d}
  glRasterPos2dv: procedure(v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos2dv}
  glRasterPos2f: procedure(x, y: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos2f}
  glRasterPos2fv: procedure(v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos2fv}
  glRasterPos2i: procedure(x, y: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos2i}
  glRasterPos2iv: procedure(v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos2iv}
  glRasterPos2s: procedure(x, y: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos2s}
  glRasterPos2sv: procedure(v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos2sv}
  glRasterPos3d: procedure(x, y, z: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos3d}
  glRasterPos3dv: procedure(v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos3dv}
  glRasterPos3f: procedure(x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos3f}
  glRasterPos3fv: procedure(v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos3fv}
  glRasterPos3i: procedure(x, y, z: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos3i}
  glRasterPos3iv: procedure(v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos3iv}
  glRasterPos3s: procedure(x, y, z: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos3s}
  glRasterPos3sv: procedure(v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos3sv}
  glRasterPos4d: procedure(x, y, z, w: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos4d}
  glRasterPos4dv: procedure(v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos4dv}
  glRasterPos4f: procedure(x, y, z, w: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos4f}
  glRasterPos4fv: procedure(v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos4fv}
  glRasterPos4i: procedure(x, y, z, w: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos4i}
  glRasterPos4iv: procedure(v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos4iv}
  glRasterPos4s: procedure(x, y, z, w: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos4s}
  glRasterPos4sv: procedure(v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRasterPos4sv}
  glReadBuffer: procedure(mode: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReadBuffer}
  glReadPixels: procedure(x, y: TGLint; width, height: TGLsizei; format, atype: TGLEnum; pixels: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReadPixels}
  glRectd: procedure(x1, y1, x2, y2: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRectd}
  glRectdv: procedure(v1, v2: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRectdv}
  glRectf: procedure(x1, y1, x2, y2: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRectf}
  glRectfv: procedure(v1, v2: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRectfv}
  glRecti: procedure(x1, y1, x2, y2: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRecti}
  glRectiv: procedure(v1, v2: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRectiv}
  glRects: procedure(x1, y1, x2, y2: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRects}
  glRectsv: procedure(v1, v2: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRectsv}
  glRenderMode: function(mode: TGLEnum): TGLint; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRenderMode}
  glRotated: procedure(angle, x, y, z: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRotated}
  glRotatef: procedure(angle, x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRotatef}
  glScaled: procedure(x, y, z: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glScaled}
  glScalef: procedure(x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glScalef}
  glScissor: procedure(x, y: TGLint; width, height: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glScissor}
  glSelectBuffer: procedure(size: TGLsizei; buffer: PGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSelectBuffer}
  glShadeModel: procedure(mode: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glShadeModel}
  glStencilFunc: procedure(func: TGLEnum; ref: TGLint; mask: TGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glStencilFunc}
  glStencilMask: procedure(mask: TGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glStencilMask}
  glStencilOp: procedure(fail, zfail, zpass: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glStencilOp}
  glTexCoord1d: procedure(s: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord1d}
  glTexCoord1dv: procedure(v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord1dv}
  glTexCoord1f: procedure(s: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord1f}
  glTexCoord1fv: procedure(v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord1fv}
  glTexCoord1i: procedure(s: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord1i}
  glTexCoord1iv: procedure(v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord1iv}
  glTexCoord1s: procedure(s: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord1s}
  glTexCoord1sv: procedure(v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord1sv}
  glTexCoord2d: procedure(s, t: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord2d}
  glTexCoord2dv: procedure(v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord2dv}
  glTexCoord2f: procedure(s, t: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord2f}
  glTexCoord2fv: procedure(v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord2fv}
  glTexCoord2i: procedure(s, t: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord2i}
  glTexCoord2iv: procedure(v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord2iv}
  glTexCoord2s: procedure(s, t: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord2s}
  glTexCoord2sv: procedure(v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord2sv}
  glTexCoord3d: procedure(s, t, r: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord3d}
  glTexCoord3dv: procedure(v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord3dv}
  glTexCoord3f: procedure(s, t, r: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord3f}
  glTexCoord3fv: procedure(v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord3fv}
  glTexCoord3i: procedure(s, t, r: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord3i}
  glTexCoord3iv: procedure(v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord3iv}
  glTexCoord3s: procedure(s, t, r: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord3s}
  glTexCoord3sv: procedure(v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord3sv}
  glTexCoord4d: procedure(s, t, r, q: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord4d}
  glTexCoord4dv: procedure(v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord4dv}
  glTexCoord4f: procedure(s, t, r, q: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord4f}
  glTexCoord4fv: procedure(v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord4fv}
  glTexCoord4i: procedure(s, t, r, q: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord4i}
  glTexCoord4iv: procedure(v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord4iv}
  glTexCoord4s: procedure(s, t, r, q: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord4s}
  glTexCoord4sv: procedure(v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord4sv}
  glTexCoordPointer: procedure(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoordPointer}
  glTexEnvf: procedure(target, pname: TGLEnum; param: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexEnvf}
  glTexEnvfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexEnvfv}
  glTexEnvi: procedure(target, pname: TGLEnum; param: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexEnvi}
  glTexEnviv: procedure(target, pname: TGLEnum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexEnviv}
  glTexGend: procedure(coord, pname: TGLEnum; param: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexGend}
  glTexGendv: procedure(coord, pname: TGLEnum; params: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexGendv}
  glTexGenf: procedure(coord, pname: TGLEnum; param: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexGenf}
  glTexGenfv: procedure(coord, pname: TGLEnum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexGenfv}
  glTexGeni: procedure(coord, pname: TGLEnum; param: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexGeni}
  glTexGeniv: procedure(coord, pname: TGLEnum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexGeniv}
  glTexImage1D: procedure(target: TGLEnum; level, internalformat: TGLint; width: TGLsizei; border: TGLint; format,
    atype: TGLEnum; pixels: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexImage1D}
  glTexImage2D: procedure(target: TGLEnum; level, internalformat: TGLint; width, height: TGLsizei; border: TGLint;
    format, atype: TGLEnum; Pixels:Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexImage2D}
  glTexParameterf: procedure(target, pname: TGLEnum; param: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexParameterf}
  glTexParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexParameterfv}
  glTexParameteri: procedure(target, pname: TGLEnum; param: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexParameteri}
  glTexParameteriv: procedure(target, pname: TGLEnum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexParameteriv}
  glTexSubImage1D: procedure(target: TGLEnum; level, xoffset: TGLint; width: TGLsizei; format, atype: TGLEnum;
    pixels: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexSubImage1D}
  glTexSubImage2D: procedure(target: TGLEnum; level, xoffset, yoffset: TGLint; width, height: TGLsizei; format,
    atype: TGLEnum; pixels: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexSubImage2D}
  glTranslated: procedure(x, y, z: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTranslated}
  glTranslatef: procedure(x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTranslatef}
  glVertex2d: procedure(x, y: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex2d}
  glVertex2dv: procedure(v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex2dv}
  glVertex2f: procedure(x, y: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex2f}
  glVertex2fv: procedure(v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex2fv}
  glVertex2i: procedure(x, y: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex2i}
  glVertex2iv: procedure(v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex2iv}
  glVertex2s: procedure(x, y: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex2s}
  glVertex2sv: procedure(v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex2sv}
  glVertex3d: procedure(x, y, z: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex3d}
  glVertex3dv: procedure(v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex3dv}
  glVertex3f: procedure(x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex3f}
  glVertex3fv: procedure(v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex3fv}
  glVertex3i: procedure(x, y, z: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex3i}
  glVertex3iv: procedure(v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex3iv}
  glVertex3s: procedure(x, y, z: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex3s}
  glVertex3sv: procedure(v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex3sv}
  glVertex4d: procedure(x, y, z, w: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex4d}
  glVertex4dv: procedure(v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex4dv}
  glVertex4f: procedure(x, y, z, w: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex4f}
  glVertex4fv: procedure(v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex4fv}
  glVertex4i: procedure(x, y, z, w: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex4i}
  glVertex4iv: procedure(v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex4iv}
  glVertex4s: procedure(x, y, z, w: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex4s}
  glVertex4sv: procedure(v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertex4sv}
  glVertexPointer: procedure(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexPointer}
  glViewport: procedure(x, y: TGLint; width, height: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glViewport}

  // GL 1.2
  glDrawRangeElements: procedure(mode: TGLEnum; Astart, Aend: TGLuint; count: TGLsizei; Atype: TGLEnum;
    indices: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glDrawRangeElements}
  glTexImage3D: procedure(target: TGLEnum; level: TGLint; internalformat: TGLEnum; width, height, depth: TGLsizei;
    border: TGLint; format: TGLEnum; Atype: TGLEnum; pixels: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexImage3D}

  // GL 1.2 ARB imaging
  glBlendColor: procedure(red, green, blue, alpha: TGLclampf); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBlendColor}
  glBlendEquation: procedure(mode: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBlendEquation}
  glColorSubTable: procedure(target: TGLEnum; start, count: TGLsizei; format, Atype: TGLEnum; data: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColorSubTable}
  glCopyColorSubTable: procedure(target: TGLEnum; start: TGLsizei; x, y: TGLint; width: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCopyColorSubTable}
  glColorTable: procedure(target, internalformat: TGLEnum; width: TGLsizei; format, Atype: TGLEnum;
    table: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColorTable}
  glCopyColorTable: procedure(target, internalformat: TGLEnum; x, y: TGLint; width: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCopyColorTable}
  glColorTableParameteriv: procedure(target, pname: TGLEnum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColorTableParameteriv}
  glColorTableParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColorTableParameterfv}
  glGetColorTable: procedure(target, format, Atype: TGLEnum; table: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetColorTable}
  glGetColorTableParameteriv: procedure(target, pname: TGLEnum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetColorTableParameteriv}
  glGetColorTableParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetColorTableParameterfv}
  glConvolutionFilter1D: procedure(target, internalformat: TGLEnum; width: TGLsizei; format, Atype: TGLEnum;
    image: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glConvolutionFilter1D}
  glConvolutionFilter2D: procedure(target, internalformat: TGLEnum; width, height: TGLsizei; format, Atype: TGLEnum;
    image: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glConvolutionFilter2D}
  glCopyConvolutionFilter1D: procedure(target, internalformat: TGLEnum; x, y: TGLint; width: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCopyConvolutionFilter1D}
  glCopyConvolutionFilter2D: procedure(target, internalformat: TGLEnum; x, y: TGLint; width, height: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCopyConvolutionFilter2D}
  glGetConvolutionFilter: procedure(target, internalformat, Atype: TGLEnum; image: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetConvolutionFilter}
  glSeparableFilter2D: procedure(target, internalformat: TGLEnum; width, height: TGLsizei; format, Atype: TGLEnum; row,
    column: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSeparableFilter2D}
  glGetSeparableFilter: procedure(target, format, Atype: TGLEnum; row, column, span: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetSeparableFilter}
  glConvolutionParameteri: procedure(target, pname: TGLEnum; param: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glConvolutionParameteri}
  glConvolutionParameteriv: procedure(target, pname: TGLEnum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glConvolutionParameteriv}
  glConvolutionParameterf: procedure(target, pname: TGLEnum; param: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glConvolutionParameterf}
  glConvolutionParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glConvolutionParameterfv}
  glGetConvolutionParameteriv: procedure(target, pname: TGLEnum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetConvolutionParameteriv}
  glGetConvolutionParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetConvolutionParameterfv}
  glHistogram: procedure(target: TGLEnum; width: TGLsizei; internalformat: TGLEnum; sink: TGLboolean); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glHistogram}
  glResetHistogram: procedure(target: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glResetHistogram}
  glGetHistogram: procedure(target: TGLEnum; reset: TGLboolean; format, Atype: TGLEnum; values: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetHistogram}
  glGetHistogramParameteriv: procedure(target, pname: TGLEnum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetHistogramParameteriv}
  glGetHistogramParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetHistogramParameterfv}
  glMinmax: procedure(target, internalformat: TGLEnum; sink: TGLboolean); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMinmax}
  glResetMinmax: procedure(target: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glResetMinmax}
  glGetMinmax: procedure(target: TGLEnum; reset: TGLboolean; format, Atype: TGLEnum; values: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetMinmax}
  glGetMinmaxParameteriv: procedure(target, pname: TGLEnum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetMinmaxParameteriv}
  glGetMinmaxParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetMinmaxParameterfv}

  // GL utility functions and procedures
  gluErrorString: function(errCode: TGLEnum): PChar; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluErrorString}
  gluGetString: function(name: TGLEnum): PChar; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluGetString}
  gluOrtho2D: procedure(left, right, bottom, top: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluOrtho2D}
  gluPerspective: procedure(fovy, aspect, zNear, zFar: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluPerspective}
  gluPickMatrix: procedure(x, y, width, height: TGLdouble; viewport: TVector4i); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluPickMatrix}
  gluLookAt: procedure(eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluLookAt}
  gluProject: function(objx, objy, objz: TGLdouble; modelMatrix: TMatrix4d; projMatrix: TMatrix4d; viewport: TVector4i;
    winx, winy, winz: PGLdouble): TGLint; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluProject}
  gluUnProject: function(winx, winy, winz: TGLdouble; modelMatrix: TMatrix4d; projMatrix: TMatrix4d; viewport: TVector4i;
    objx, objy, objz: PGLdouble): TGLint; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluUnProject}
  gluScaleImage: function(format: TGLEnum; widthin, heightin: TGLint; typein: TGLEnum; datain: Pointer; widthout,
    heightout: TGLint; typeout: TGLEnum; dataout: Pointer): TGLint; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluScaleImage}
  gluBuild1DMipmaps: function(target: TGLEnum; components, width: TGLint; format, atype: TGLEnum;
    data: Pointer): TGLint; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluBuild1DMipmaps}
  gluBuild2DMipmaps: function(target: TGLEnum; components, width, height: TGLint; format, atype: TGLEnum;
    Data: Pointer): TGLint; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluBuild2DMipmaps}
  gluNewQuadric: function: PGLUquadric; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluNewQuadric}
  gluDeleteQuadric: procedure(state: PGLUquadric); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluDeleteQuadric}
  gluQuadricNormals: procedure(quadObject: PGLUquadric; normals: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluQuadricNormals}
  gluQuadricTexture: procedure(quadObject: PGLUquadric; textureCoords: TGLboolean); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluQuadricTexture}
  gluQuadricOrientation: procedure(quadObject: PGLUquadric; orientation: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluQuadricOrientation}
  gluQuadricDrawStyle: procedure(quadObject: PGLUquadric; drawStyle: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluQuadricDrawStyle}
  gluCylinder: procedure(quadObject: PGLUquadric; baseRadius, topRadius, height: TGLdouble; slices,
    stacks: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluCylinder}
  gluDisk: procedure(quadObject: PGLUquadric; innerRadius, outerRadius: TGLdouble; slices, loops: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluDisk}
  gluPartialDisk: procedure(quadObject: PGLUquadric; innerRadius, outerRadius: TGLdouble; slices, loops: TGLint;
    startAngle, sweepAngle: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluPartialDisk}
  gluSphere: procedure(quadObject: PGLUquadric; radius: TGLdouble; slices, stacks: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluSphere}
  gluQuadricCallback: procedure(quadObject: PGLUquadric; which: TGLEnum; fn: TGLUQuadricErrorProc); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluQuadricCallback}
  gluNewTess: function: PGLUtesselator; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluNewTess}
  gluDeleteTess: procedure(tess: PGLUtesselator); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluDeleteTess}
  gluTessBeginPolygon: procedure(tess: PGLUtesselator; polygon_data: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluTessBeginPolygon}
  gluTessBeginContour: procedure(tess: PGLUtesselator); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluTessBeginContour}
  gluTessVertex: procedure(tess: PGLUtesselator; coords: TVector3d; data: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluTessVertex}
  gluTessEndContour: procedure(tess: PGLUtesselator); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluTessEndContour}
  gluTessEndPolygon: procedure(tess: PGLUtesselator); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluTessEndPolygon}
  gluTessProperty: procedure(tess: PGLUtesselator; which: TGLEnum; value: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluTessProperty}
  gluTessNormal: procedure(tess: PGLUtesselator; x, y, z: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluTessNormal}
  gluTessCallback: procedure(tess: PGLUtesselator; which: TGLEnum; fn: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluTessCallback}
  gluGetTessProperty: procedure(tess: PGLUtesselator; which: TGLEnum; value: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluGetTessProperty}
  gluNewNurbsRenderer: function: PGLUnurbs; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluNewNurbsRenderer}
  gluDeleteNurbsRenderer: procedure(nobj: PGLUnurbs); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluDeleteNurbsRenderer}
  gluBeginSurface: procedure(nobj: PGLUnurbs); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluBeginSurface}
  gluBeginCurve: procedure(nobj: PGLUnurbs); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluBeginCurve}
  gluEndCurve: procedure(nobj: PGLUnurbs); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluEndCurve}
  gluEndSurface: procedure(nobj: PGLUnurbs); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluEndSurface}
  gluBeginTrim: procedure(nobj: PGLUnurbs); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluBeginTrim}
  gluEndTrim: procedure(nobj: PGLUnurbs); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluEndTrim}
  gluPwlCurve: procedure(nobj: PGLUnurbs; count: TGLint; points: PGLfloat; stride: TGLint; atype: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluPwlCurve}
  gluNurbsCurve: procedure(nobj: PGLUnurbs; nknots: TGLint; knot: PGLfloat; stride: TGLint; ctlarray: PGLfloat; order: TGLint; atype: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluNurbsCurve}
  gluNurbsSurface: procedure(nobj: PGLUnurbs; sknot_count: TGLint; sknot: PGLfloat; tknot_count: TGLint; tknot: PGLfloat; s_stride, t_stride: TGLint; ctlarray: PGLfloat; sorder, torder: TGLint; atype: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluNurbsSurface}
  gluLoadSamplingMatrices: procedure(nobj: PGLUnurbs; modelMatrix, projMatrix: TMatrix4f; viewport: TVector4i); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluLoadSamplingMatrices}
  gluNurbsProperty: procedure(nobj: PGLUnurbs; aproperty: TGLEnum; value: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluNurbsProperty}
  gluGetNurbsProperty: procedure(nobj: PGLUnurbs; aproperty: TGLEnum; value: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluGetNurbsProperty}
  gluNurbsCallback: procedure(nobj: PGLUnurbs; which: TGLEnum; fn: TGLUNurbsErrorProc); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluNurbsCallback}
  gluBeginPolygon: procedure(tess: PGLUtesselator); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluBeginPolygon}
  gluNextContour: procedure(tess: PGLUtesselator; atype: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluNextContour}
  gluEndPolygon: procedure(tess: PGLUtesselator); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluEndPolygon}

  // window support functions
  {$ifdef Win32}
  wglGetProcAddress: function(ProcName: PChar): Pointer; stdcall;
  {$EXTERNALSYM wglGetProcAddress}
  wglCopyContext: function(p1: HGLRC; p2: HGLRC; p3: Cardinal): BOOL; stdcall;
  {$EXTERNALSYM wglCopyContext}
  wglCreateContext: function(DC: HDC): HGLRC; stdcall;
  {$EXTERNALSYM wglCreateContext}
  wglCreateLayerContext: function(p1: HDC; p2: Integer): HGLRC; stdcall;
  {$EXTERNALSYM wglCreateLayerContext}
  wglDeleteContext: function(p1: HGLRC): BOOL; stdcall;
  {$EXTERNALSYM wglDeleteContext}
  wglDescribeLayerPlane:function(p1: HDC; p2, p3: Integer; p4: Cardinal; var p5: TLayerPlaneDescriptor): BOOL; stdcall;
  {$EXTERNALSYM wglDescribeLayerPlane}
  wglGetCurrentContext: function: HGLRC; stdcall;
  {$EXTERNALSYM wglGetCurrentContext}
  wglGetCurrentDC: function: HDC; stdcall;
  {$EXTERNALSYM wglGetCurrentDC}
  wglGetLayerPaletteEntries: function(p1: HDC; p2, p3, p4: Integer; var pcr): Integer; stdcall;
  {$EXTERNALSYM wglGetLayerPaletteEntries}
  wglMakeCurrent: function(DC: HDC; p2: HGLRC): BOOL; stdcall;
  {$EXTERNALSYM wglMakeCurrent}
  wglRealizeLayerPalette: function(p1: HDC; p2: Integer; p3: BOOL): BOOL; stdcall;
  {$EXTERNALSYM wglRealizeLayerPalette}
  wglSetLayerPaletteEntries: function(p1: HDC; p2, p3, p4: Integer; var pcr): Integer; stdcall;
  {$EXTERNALSYM wglSetLayerPaletteEntries}
  wglShareLists: function(p1, p2: HGLRC): BOOL; stdcall;
  {$EXTERNALSYM wglShareLists}
  wglSwapLayerBuffers: function(p1: HDC; p2: Cardinal): BOOL; stdcall;
  {$EXTERNALSYM wglSwapLayerBuffers}
  wglSwapMultipleBuffers: function(p1: Cardinal; const p2: PWGLSwap): DWORD; stdcall;
  {$EXTERNALSYM wglSwapMultipleBuffers}
  wglUseFontBitmapsA: function(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall;
  {$EXTERNALSYM wglUseFontBitmapsA}
  wglUseFontOutlinesA: function (p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall;
  {$EXTERNALSYM wglUseFontOutlinesA}
  wglUseFontBitmapsW: function(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall;
  {$EXTERNALSYM wglUseFontBitmapsW}
  wglUseFontOutlinesW: function (p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall;
  {$EXTERNALSYM wglUseFontOutlinesW}
  wglUseFontBitmaps: function(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall;
  {$EXTERNALSYM wglUseFontBitmaps}
  wglUseFontOutlines: function(p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall;
  {$EXTERNALSYM wglUseFontOutlines}

  // ARB wgl extensions
  wglGetExtensionsStringARB: function(DC: HDC): PChar; stdcall;
  {$EXTERNALSYM wglGetExtensionsStringARB}
  wglGetPixelFormatAttribivARB: function(DC: HDC; iPixelFormat, iLayerPlane: Integer; nAttributes: Cardinal;
    const piAttributes: PInteger; piValues : PInteger) : BOOL; stdcall;
  {$EXTERNALSYM wglGetPixelFormatAttribivARB}
  wglGetPixelFormatAttribfvARB: function(DC: HDC; iPixelFormat, iLayerPlane: Integer; nAttributes: Cardinal;
    const piAttributes: PInteger; piValues: PGLFloat) : BOOL; stdcall;
  {$EXTERNALSYM wglGetPixelFormatAttribfvARB}
  wglChoosePixelFormatARB: function(DC: HDC; const piAttribIList: PInteger; const pfAttribFList: PGLFloat;
    nMaxFormats: Cardinal; piFormats: PInteger; nNumFormats: PCardinal) : BOOL; stdcall;
  {$EXTERNALSYM wglChoosePixelFormatARB}
  {$endif}

  // ARB_multitexture
  glMultiTexCoord1dARB: procedure(target: TGLenum; s: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord1dARB}
  glMultiTexCoord1dVARB: procedure(target: TGLenum; v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord1dVARB}
  glMultiTexCoord1fARBP: procedure(target: TGLenum; s: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord1fARBP}
  glMultiTexCoord1fVARB: procedure(target: TGLenum; v: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord1fVARB}
  glMultiTexCoord1iARB: procedure(target: TGLenum; s: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord1iARB}
  glMultiTexCoord1iVARB: procedure(target: TGLenum; v: PGLInt); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord1iVARB}
  glMultiTexCoord1sARBP: procedure(target: TGLenum; s: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord1sARBP}
  glMultiTexCoord1sVARB: procedure(target: TGLenum; v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord1sVARB}
  glMultiTexCoord2dARB: procedure(target: TGLenum; s, t: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord2dARB}
  glMultiTexCoord2dvARB: procedure(target: TGLenum; v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord2dvARB}
  glMultiTexCoord2fARB: procedure(target: TGLenum; s, t: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord2fARB}
  glMultiTexCoord2fvARB: procedure(target: TGLenum; v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord2fvARB}
  glMultiTexCoord2iARB: procedure(target: TGLenum; s, t: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord2iARB}
  glMultiTexCoord2ivARB: procedure(target: TGLenum; v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord2ivARB}
  glMultiTexCoord2sARB: procedure(target: TGLenum; s, t: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord2sARB}
  glMultiTexCoord2svARB: procedure(target: TGLenum; v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord2svARB}
  glMultiTexCoord3dARB: procedure(target: TGLenum; s, t, r: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord3dARB}
  glMultiTexCoord3dvARB: procedure(target: TGLenum; v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord3dvARB}
  glMultiTexCoord3fARB: procedure(target: TGLenum; s, t, r: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord3fARB}
  glMultiTexCoord3fvARB: procedure(target: TGLenum; v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord3fvARB}
  glMultiTexCoord3iARB: procedure(target: TGLenum; s, t, r: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord3iARB}
  glMultiTexCoord3ivARB: procedure(target: TGLenum; v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord3ivARB}
  glMultiTexCoord3sARB: procedure(target: TGLenum; s, t, r: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord3sARB}
  glMultiTexCoord3svARB: procedure(target: TGLenum; v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord3svARB}
  glMultiTexCoord4dARB: procedure(target: TGLenum; s, t, r, q: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord4dARB}
  glMultiTexCoord4dvARB: procedure(target: TGLenum; v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord4dvARB}
  glMultiTexCoord4fARB: procedure(target: TGLenum; s, t, r, q: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord4fARB}
  glMultiTexCoord4fvARB: procedure(target: TGLenum; v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord4fvARB}
  glMultiTexCoord4iARB: procedure(target: TGLenum; s, t, r, q: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord4iARB}
  glMultiTexCoord4ivARB: procedure(target: TGLenum; v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord4ivARB}
  glMultiTexCoord4sARB: procedure(target: TGLenum; s, t, r, q: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord4sARB}
  glMultiTexCoord4svARB: procedure(target: TGLenum; v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiTexCoord4svARB}
  glActiveTextureARB: procedure(target: TGLenum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glActiveTextureARB}
  glClientActiveTextureARB: procedure(target: TGLenum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glClientActiveTextureARB}

  // GLU extensions
  gluNurbsCallbackDataEXT: procedure(nurb: PGLUnurbs; userData: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluNurbsCallbackDataEXT}
  gluNewNurbsTessellatorEXT: function: PGLUnurbs; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluNewNurbsTessellatorEXT}
  gluDeleteNurbsTessellatorEXT: procedure(nurb: PGLUnurbs); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM gluDeleteNurbsTessellatorEXT}

{$ifndef FPC} 
  {$ifdef MULTITHREADOPENGL}
  threadvar
  {$else}
  var
  {$endif}
{$else}
var 
{$endif} 
  // Extension functions
  glAreTexturesResidentEXT: function(n: TGLsizei; textures: PGLuint; residences: PGLBoolean): TGLboolean; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glAreTexturesResidentEXT}
  glArrayElementArrayEXT: procedure(mode: TGLEnum; count: TGLsizei; pi: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glArrayElementArrayEXT}
  glBeginSceneEXT: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBeginSceneEXT}
  glBindTextureEXT: procedure(target: TGLEnum; texture: TGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBindTextureEXT}
  glColorTableEXT: procedure(target, internalFormat: TGLEnum; width: TGLsizei; format, atype: TGLEnum; data: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColorTableEXT}
  glColorSubTableExt: procedure(target: TGLEnum; start, count: TGLsizei; format, atype: TGLEnum; data: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColorSubTableExt}
  glCopyTexImage1DEXT: procedure(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width: TGLsizei; border: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCopyTexImage1DEXT}
  glCopyTexSubImage1DEXT: procedure(target: TGLEnum; level, xoffset, x, y: TGLint; width: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCopyTexSubImage1DEXT}
  glCopyTexImage2DEXT: procedure(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width, height: TGLsizei; border: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCopyTexImage2DEXT}
  glCopyTexSubImage2DEXT: procedure(target: TGLEnum; level, xoffset, yoffset, x, y: TGLint; width, height: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCopyTexSubImage2DEXT}
  glCopyTexSubImage3DEXT: procedure(target: TGLEnum; level, xoffset, yoffset, zoffset, x, y: TGLint; width, height: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCopyTexSubImage3DEXT}
  glDeleteTexturesEXT: procedure(n: TGLsizei; textures: PGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glDeleteTexturesEXT}
  glEndSceneEXT: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEndSceneEXT}
  glGenTexturesEXT: procedure(n: TGLsizei; textures: PGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGenTexturesEXT}
  glGetColorTableEXT: procedure(target, format, atype: TGLEnum; data: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetColorTableEXT}
  glGetColorTablePameterfvEXT: procedure(target, pname: TGLEnum; params: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetColorTablePameterfvEXT}
  glGetColorTablePameterivEXT: procedure(target, pname: TGLEnum; params: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetColorTablePameterivEXT}
  glIndexFuncEXT: procedure(func: TGLEnum; ref: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIndexFuncEXT}
  glIndexMaterialEXT: procedure(face: TGLEnum; mode: TGLEnum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIndexMaterialEXT}
  glIsTextureEXT: function(texture: TGLuint): TGLboolean; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIsTextureEXT}
  glLockArraysEXT: procedure(first: TGLint; count: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLockArraysEXT}
  glPolygonOffsetEXT: procedure(factor, bias: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPolygonOffsetEXT}
  glPrioritizeTexturesEXT: procedure(n: TGLsizei; textures: PGLuint; priorities: PGLclampf); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPrioritizeTexturesEXT}
  glTexSubImage1DEXT: procedure(target: TGLEnum; level, xoffset: TGLint; width: TGLsizei; format, Atype: TGLEnum; pixels: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexSubImage1DEXT}
  glTexSubImage2DEXT: procedure(target: TGLEnum; level, xoffset, yoffset: TGLint; width, height: TGLsizei; format, Atype: TGLEnum; pixels: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexSubImage2DEXT}
  glTexSubImage3DEXT: procedure(target: TGLEnum; level, xoffset, yoffset, zoffset: TGLint; width, height, depth: TGLsizei; format, Atype: TGLEnum; pixels: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexSubImage3DEXT}
  glUnlockArraysEXT: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glUnlockArraysEXT}

  // EXT_vertex_array
  glArrayElementEXT: procedure(I: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glArrayElementEXT}
  glColorPointerEXT: procedure(size: TGLInt; atype: TGLenum; stride, count: TGLsizei; data: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColorPointerEXT}
  glDrawArraysEXT: procedure(mode: TGLenum; first: TGLInt; count: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glDrawArraysEXT}
  glEdgeFlagPointerEXT: procedure(stride, count: TGLsizei; data: PGLboolean); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEdgeFlagPointerEXT}
  glGetPointervEXT: procedure(pname: TGLEnum; var params); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetPointervEXT}
  glIndexPointerEXT: procedure(AType: TGLEnum; stride, count: TGLsizei; P: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIndexPointerEXT}
  glNormalPointerEXT: procedure(AType: TGLsizei; stride, count: TGLsizei; P: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glNormalPointerEXT}
  glTexCoordPointerEXT: procedure(size: TGLint; AType: TGLenum;  stride, count: TGLsizei; P: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoordPointerEXT}
  glVertexPointerEXT: procedure(size: TGLint; AType: TGLenum; stride, count: TGLsizei; P: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexPointerEXT}

  // EXT_compiled_vertex_array
  glLockArrayEXT: procedure(first: TGLint; count: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLockArrayEXT}
  glUnlockArrayEXT: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glUnlockArrayEXT}

  // EXT_cull_vertex
  glCullParameterdvEXT: procedure(pname: TGLenum; params: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCullParameterdvEXT}
  glCullParameterfvEXT: procedure(pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCullParameterfvEXT}

  // WIN_swap_hint
  glAddSwapHintRectWIN: procedure(x, y: TGLint; width, height: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glAddSwapHintRectWIN}

  // EXT_point_parameter
  glPointParameterfEXT: procedure(pname: TGLenum; param: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPointParameterfEXT}
  glPointParameterfvEXT: procedure(pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPointParameterfvEXT}

  // GL_ARB_transpose_matrix
  glLoadTransposeMatrixfARB: procedure(m: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLoadTransposeMatrixfARB}
  glLoadTransposeMatrixdARB: procedure(m: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLoadTransposeMatrixdARB}
  glMultTransposeMatrixfARB: procedure(m: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultTransposeMatrixfARB}
  glMultTransposeMatrixdARB: procedure(m: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultTransposeMatrixdARB}

  // GL_ARB_multisample
  glSampleCoverageARB: procedure(Value: TGLclampf; invert: TGLboolean); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSampleCoverageARB}
  glSamplePassARB: procedure(pass: TGLenum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSamplePassARB}

  // GL_ARB_texture_compression
  glCompressedTexImage3DARB: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width, Height, depth: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCompressedTexImage3DARB}
  glCompressedTexImage2DARB: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width, Height: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCompressedTexImage2DARB}
  glCompressedTexImage1DARB: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCompressedTexImage1DARB}
  glCompressedTexSubImage3DARB: procedure(target: TGLenum; level: TGLint; xoffset, yoffset, zoffset: TGLint; width, height, depth: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCompressedTexSubImage3DARB}
  glCompressedTexSubImage2DARB: procedure(target: TGLenum; level: TGLint; xoffset, yoffset: TGLint; width, height: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCompressedTexSubImage2DARB}
  glCompressedTexSubImage1DARB: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; width: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCompressedTexSubImage1DARB}
  glGetCompressedTexImageARB: procedure(target: TGLenum; level: TGLint; img: pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetCompressedTexImageARB}

  // GL_EXT_blend_color
  glBlendColorEXT: procedure(red, green, blue: TGLclampf; alpha: TGLclampf); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBlendColorEXT}

  // GL_EXT_texture3D
  glTexImage3DEXT: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; width, height, depth: TGLsizei; border: TGLint; Format, AType: TGLenum; pixels: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexImage3DEXT}

  // GL_SGIS_texture_filter4
  glGetTexFilterFuncSGIS: procedure(target, Filter: TGLenum; weights: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetTexFilterFuncSGIS}
  glTexFilterFuncSGIS: procedure(target, Filter: TGLenum; n: TGLsizei; weights: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexFilterFuncSGIS}

  // GL_EXT_histogram
  glGetHistogramEXT: procedure(target: TGLenum; reset: TGLboolean; Format, AType: TGLenum; values: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetHistogramEXT}
  glGetHistogramParameterfvEXT: procedure(target, pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetHistogramParameterfvEXT}
  glGetHistogramParameterivEXT: procedure(target, pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetHistogramParameterivEXT}
  glGetMinmaxEXT: procedure(target: TGLenum; reset: TGLboolean; Format, AType: TGLenum; values: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetMinmaxEXT}
  glGetMinmaxParameterfvEXT: procedure(target, pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetMinmaxParameterfvEXT}
  glGetMinmaxParameterivEXT: procedure(target, pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetMinmaxParameterivEXT}
  glHistogramEXT: procedure(target: TGLenum; Width: TGLsizei; internalformat: TGLenum; sink: TGLboolean); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glHistogramEXT}
  glMinmaxEXT: procedure(target, internalformat: TGLenum; sink: TGLboolean); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMinmaxEXT}
  glResetHistogramEXT: procedure(target: TGLenum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glResetHistogramEXT}
  glResetMinmaxEXT: procedure(target: TGLenum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glResetMinmaxEXT}

  // GL_EXT_convolution
  glConvolutionFilter1DEXT: procedure(target, internalformat: TGLenum; Width: TGLsizei; Format, AType: TGLenum; image: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glConvolutionFilter1DEXT}
  glConvolutionFilter2DEXT: procedure(target, internalformat: TGLenum; Width, Height: TGLsizei; Format, AType: TGLenum; image: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glConvolutionFilter2DEXT}
  glConvolutionParameterfEXT: procedure(target, pname: TGLenum; params: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glConvolutionParameterfEXT}
  glConvolutionParameterfvEXT: procedure(target, pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glConvolutionParameterfvEXT}
  glConvolutionParameteriEXT: procedure(target, pname: TGLenum; params: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glConvolutionParameteriEXT}
  glConvolutionParameterivEXT: procedure(target, pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glConvolutionParameterivEXT}
  glCopyConvolutionFilter1DEXT: procedure(target, internalformat: TGLenum; x, y: TGLint; Width: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCopyConvolutionFilter1DEXT}
  glCopyConvolutionFilter2DEXT: procedure(target, internalformat: TGLenum; x, y: TGLint; Width, Height: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCopyConvolutionFilter2DEXT}
  glGetConvolutionFilterEXT: procedure(target, Format, AType: TGLenum; image: pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetConvolutionFilterEXT}
  glGetConvolutionParameterfvEXT: procedure(target, pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetConvolutionParameterfvEXT}
  glGetConvolutionParameterivEXT: procedure(target, pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetConvolutionParameterivEXT}
  glGetSeparableFilterEXT: procedure(target, Format, AType: TGLenum; row, column, span: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetSeparableFilterEXT}
  glSeparableFilter2DEXT: procedure(target, internalformat: TGLenum; Width, Height: TGLsizei; Format, AType: TGLenum; row, column: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSeparableFilter2DEXT}

  // GL_SGI_color_table
  glColorTableSGI: procedure(target, internalformat: TGLenum; Width: TGLsizei; Format, AType: TGLenum; Table: pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColorTableSGI}
  glColorTableParameterfvSGI: procedure(target, pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColorTableParameterfvSGI}
  glColorTableParameterivSGI: procedure(target, pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColorTableParameterivSGI}
  glCopyColorTableSGI: procedure(target, internalformat: TGLenum; x, y: TGLint; Width: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCopyColorTableSGI}
  glGetColorTableSGI: procedure(target, Format, AType: TGLenum; Table: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetColorTableSGI}
  glGetColorTableParameterfvSGI: procedure(target, pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetColorTableParameterfvSGI}
  glGetColorTableParameterivSGI: procedure(target, pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetColorTableParameterivSGI}

  // GL_SGIX_pixel_texture
  glPixelTexGenSGIX: procedure(mode: TGLenum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPixelTexGenSGIX}

  // GL_SGIS_pixel_texture
  glPixelTexGenParameteriSGIS: procedure(pname: TGLenum; param: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPixelTexGenParameteriSGIS}
  glPixelTexGenParameterivSGIS: procedure(pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPixelTexGenParameterivSGIS}
  glPixelTexGenParameterfSGIS: procedure(pname: TGLenum; param: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPixelTexGenParameterfSGIS}
  glPixelTexGenParameterfvSGIS: procedure(pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPixelTexGenParameterfvSGIS}
  glGetPixelTexGenParameterivSGIS: procedure(pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetPixelTexGenParameterivSGIS}
  glGetPixelTexGenParameterfvSGIS: procedure(pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetPixelTexGenParameterfvSGIS}

  // GL_SGIS_texture4D
  glTexImage4DSGIS: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width, Height, depth, size4d: TGLsizei; border: TGLint; Format, AType: TGLenum; pixels: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexImage4DSGIS}
  glTexSubImage4DSGIS: procedure(target: TGLenum; level, xoffset, yoffset, zoffset, woffset: TGLint; Width, Height, depth, size4d: TGLsizei; Format, AType: TGLenum; pixels: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexSubImage4DSGIS}

  // GL_SGIS_detail_texture
  glDetailTexFuncSGIS: procedure(target: TGLenum; n: TGLsizei; points: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glDetailTexFuncSGIS}
  glGetDetailTexFuncSGIS: procedure(target: TGLenum; points: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetDetailTexFuncSGIS}

  // GL_SGIS_sharpen_texture
  glSharpenTexFuncSGIS: procedure(target: TGLenum; n: TGLsizei; points: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSharpenTexFuncSGIS}
  glGetSharpenTexFuncSGIS: procedure(target: TGLenum; points: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetSharpenTexFuncSGIS}

  // GL_SGIS_multisample
  glSampleMaskSGIS: procedure(Value: TGLclampf; invert: TGLboolean); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSampleMaskSGIS}
  glSamplePatternSGIS: procedure(pattern: TGLenum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSamplePatternSGIS}

  // GL_EXT_blend_minmax
  glBlendEquationEXT: procedure(mode: TGLenum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBlendEquationEXT}

  // GL_SGIX_sprite
  glSpriteParameterfSGIX: procedure(pname: TGLenum; param: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSpriteParameterfSGIX}
  glSpriteParameterfvSGIX: procedure(pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSpriteParameterfvSGIX}
  glSpriteParameteriSGIX: procedure(pname: TGLenum; param: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSpriteParameteriSGIX}
  glSpriteParameterivSGIX: procedure(pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSpriteParameterivSGIX}

  // GL_EXT_point_parameters
  glPointParameterfSGIS: procedure(pname: TGLenum; param: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPointParameterfSGIS}
  glPointParameterfvSGIS: procedure(pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPointParameterfvSGIS}

  // GL_SGIX_instruments
  glGetInstrumentsSGIX: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetInstrumentsSGIX}
  glInstrumentsBufferSGIX: procedure(Size: TGLsizei; buffer: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glInstrumentsBufferSGIX}
  glPollInstrumentsSGIX: procedure(marker_p: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPollInstrumentsSGIX}
  glReadInstrumentsSGIX: procedure(marker: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReadInstrumentsSGIX}
  glStartInstrumentsSGIX: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glStartInstrumentsSGIX}
  glStopInstrumentsSGIX: procedure(marker: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glStopInstrumentsSGIX}

  // GL_SGIX_framezoom
  glFrameZoomSGIX: procedure(factor: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFrameZoomSGIX}

  // GL_SGIX_tag_sample_buffer
  glTagSampleBufferSGIX: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTagSampleBufferSGIX}

  // GL_SGIX_polynomial_ffd
  glDeformationMap3dSGIX: procedure(target: TGLenum; u1, u2: TGLdouble; ustride, uorder: TGLint; v1, v2: TGLdouble; vstride, vorder: TGLint; w1, w2: TGLdouble; wstride, worder: TGLint; points: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glDeformationMap3dSGIX}
  glDeformationMap3fSGIX: procedure(target: TGLenum; u1, u2: TGLfloat; ustride, uorder: TGLint; v1, v2: TGLfloat; vstride, vorder: TGLint; w1, w2: TGLfloat; wstride, worder: TGLint; points: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glDeformationMap3fSGIX}
  glDeformSGIX: procedure(mask: TGLbitfield); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glDeformSGIX}
  glLoadIdentityDeformationMapSGIX: procedure(mask: TGLbitfield); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLoadIdentityDeformationMapSGIX}

  // GL_SGIX_reference_plane
  glReferencePlaneSGIX: procedure(equation: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReferencePlaneSGIX}

  // GL_SGIX_flush_raster
  glFlushRasterSGIX: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFlushRasterSGIX}

  // GL_SGIS_fog_function
  glFogFuncSGIS: procedure(n: TGLsizei; points: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFogFuncSGIS}
  glGetFogFuncSGIS: procedure(points: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetFogFuncSGIS}

  // GL_HP_image_transform
  glImageTransformParameteriHP: procedure(target, pname: TGLenum; param: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glImageTransformParameteriHP}
  glImageTransformParameterfHP: procedure(target, pname: TGLenum; param: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glImageTransformParameterfHP}
  glImageTransformParameterivHP: procedure(target, pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glImageTransformParameterivHP}
  glImageTransformParameterfvHP: procedure(target, pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glImageTransformParameterfvHP}
  glGetImageTransformParameterivHP: procedure(target, pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetImageTransformParameterivHP}
  glGetImageTransformParameterfvHP: procedure(target, pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetImageTransformParameterfvHP}

  // GL_EXT_color_subtable
  glCopyColorSubTableEXT: procedure(target: TGLenum; start: TGLsizei; x, y: TGLint; Width: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCopyColorSubTableEXT}

  // GL_PGI_misc_hints
  glHintPGI: procedure(target: TGLenum; mode: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glHintPGI}

  // GL_EXT_paletted_texture
  glGetColorTableParameterivEXT: procedure(target, pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetColorTableParameterivEXT}
  glGetColorTableParameterfvEXT: procedure(target, pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetColorTableParameterfvEXT}

  // GL_SGIX_list_priority
  glGetListParameterfvSGIX: procedure(list: TGLuint; pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetListParameterfvSGIX}
  glGetListParameterivSGIX: procedure(list: TGLuint; pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetListParameterivSGIX}
  glListParameterfSGIX: procedure(list: TGLuint; pname: TGLenum; param: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glListParameterfSGIX}
  glListParameterfvSGIX: procedure(list: TGLuint; pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glListParameterfvSGIX}
  glListParameteriSGIX: procedure(list: TGLuint; pname: TGLenum; param: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glListParameteriSGIX}
  glListParameterivSGIX: procedure(list: TGLuint; pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glListParameterivSGIX}

  // GL_SGIX_fragment_lighting
  glFragmentColorMaterialSGIX: procedure(face, mode: TGLenum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFragmentColorMaterialSGIX}
  glFragmentLightfSGIX: procedure(light, pname: TGLenum; param: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFragmentLightfSGIX}
  glFragmentLightfvSGIX: procedure(light, pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFragmentLightfvSGIX}
  glFragmentLightiSGIX: procedure(light, pname: TGLenum; param: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFragmentLightiSGIX}
  glFragmentLightivSGIX: procedure(light, pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFragmentLightivSGIX}
  glFragmentLightModelfSGIX: procedure(pname: TGLenum; param: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFragmentLightModelfSGIX}
  glFragmentLightModelfvSGIX: procedure(pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFragmentLightModelfvSGIX}
  glFragmentLightModeliSGIX: procedure(pname: TGLenum; param: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFragmentLightModeliSGIX}
  glFragmentLightModelivSGIX: procedure(pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFragmentLightModelivSGIX}
  glFragmentMaterialfSGIX: procedure(face, pname: TGLenum; param: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFragmentMaterialfSGIX}
  glFragmentMaterialfvSGIX: procedure(face, pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFragmentMaterialfvSGIX}
  glFragmentMaterialiSGIX: procedure(face, pname: TGLenum; param: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFragmentMaterialiSGIX}
  glFragmentMaterialivSGIX: procedure(face, pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFragmentMaterialivSGIX}
  glGetFragmentLightfvSGIX: procedure(light, pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetFragmentLightfvSGIX}
  glGetFragmentLightivSGIX: procedure(light, pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetFragmentLightivSGIX}
  glGetFragmentMaterialfvSGIX: procedure(face, pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetFragmentMaterialfvSGIX}
  glGetFragmentMaterialivSGIX: procedure(face, pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetFragmentMaterialivSGIX}
  glLightEnviSGIX: procedure(pname: TGLenum; param: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLightEnviSGIX}

  // GL_EXT_draw_range_elements
  glDrawRangeElementsEXT: procedure(mode: TGLenum; start, Aend: TGLuint; Count: TGLsizei; Atype: TGLenum; indices: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glDrawRangeElementsEXT}

  // GL_EXT_light_texture
  glApplyTextureEXT: procedure(mode: TGLenum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glApplyTextureEXT}
  glTextureLightEXT: procedure(pname: TGLenum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTextureLightEXT}
  glTextureMaterialEXT: procedure(face, mode: TGLenum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTextureMaterialEXT}

  // GL_SGIX_async
  glAsyncMarkerSGIX: procedure(marker: TGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glAsyncMarkerSGIX}
  glFinishAsyncSGIX: procedure(markerp: PGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFinishAsyncSGIX}
  glPollAsyncSGIX: procedure(markerp: PGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPollAsyncSGIX}
  glGenAsyncMarkersSGIX: procedure(range: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGenAsyncMarkersSGIX}
  glDeleteAsyncMarkersSGIX: procedure(marker: TGLuint; range: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glDeleteAsyncMarkersSGIX}
  glIsAsyncMarkerSGIX: procedure(marker: TGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIsAsyncMarkerSGIX}

  // GL_INTEL_parallel_arrays
  glVertexPointervINTEL: procedure(size: TGLint; Atype: TGLenum; var P); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexPointervINTEL}
  glNormalPointervINTEL: procedure(Atype: TGLenum; var P); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glNormalPointervINTEL}
  glColorPointervINTEL: procedure(size: TGLint; Atype: TGLenum; var P); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColorPointervINTEL}
  glTexCoordPointervINTEL: procedure(size: TGLint; Atype: TGLenum; var P); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoordPointervINTEL}

  // GL_EXT_pixel_transform
  glPixelTransformParameteriEXT: procedure(target, pname: TGLenum; param: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPixelTransformParameteriEXT}
  glPixelTransformParameterfEXT: procedure(target, pname: TGLenum; param: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPixelTransformParameterfEXT}
  glPixelTransformParameterivEXT: procedure(target, pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPixelTransformParameterivEXT}
  glPixelTransformParameterfvEXT: procedure(target, pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glPixelTransformParameterfvEXT}

  // GL_EXT_secondary_color
  glSecondaryColor3bEXT: procedure(red, green, blue: TGLbyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSecondaryColor3bEXT}
  glSecondaryColor3bvEXT: procedure(v: PGLbyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSecondaryColor3bvEXT}
  glSecondaryColor3dEXT: procedure(red, green, blue: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSecondaryColor3dEXT}
  glSecondaryColor3dvEXT: procedure(v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSecondaryColor3dvEXT}
  glSecondaryColor3fEXT: procedure(red, green, blue: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSecondaryColor3fEXT}
  glSecondaryColor3fvEXT: procedure(v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSecondaryColor3fvEXT}
  glSecondaryColor3iEXT: procedure(red, green, blue: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSecondaryColor3iEXT}
  glSecondaryColor3ivEXT: procedure(v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSecondaryColor3ivEXT}

  glSecondaryColor3sEXT: procedure(red, green, blue: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSecondaryColor3sEXT}
  glSecondaryColor3svEXT: procedure(v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSecondaryColor3svEXT}
  glSecondaryColor3ubEXT: procedure(red, green, blue: TGLubyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSecondaryColor3ubEXT}
  glSecondaryColor3ubvEXT: procedure(v: PGLubyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSecondaryColor3ubvEXT}
  glSecondaryColor3uiEXT: procedure(red, green, blue: TGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSecondaryColor3uiEXT}
  glSecondaryColor3uivEXT: procedure(v: PGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSecondaryColor3uivEXT}
  glSecondaryColor3usEXT: procedure(red, green, blue: TGLushort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSecondaryColor3usEXT}
  glSecondaryColor3usvEXT: procedure(v: PGLushort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSecondaryColor3usvEXT}
  glSecondaryColorPointerEXT: procedure(Size: TGLint; Atype: TGLenum; stride: TGLsizei; p: pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSecondaryColorPointerEXT}

  // GL_EXT_texture_perturb_normal
  glTextureNormalEXT: procedure(mode: TGLenum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTextureNormalEXT}

  // GL_EXT_multi_draw_arrays
  glMultiDrawArraysEXT: procedure(mode: TGLenum; First: PGLint; Count: PGLsizei; primcount: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiDrawArraysEXT}
  glMultiDrawElementsEXT: procedure(mode: TGLenum; Count: PGLsizei; AType: TGLenum; var indices; primcount: TGLsizei); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiDrawElementsEXT}

  // GL_EXT_fog_coord
  glFogCoordfEXT: procedure(coord: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFogCoordfEXT}
  glFogCoordfvEXT: procedure(coord: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFogCoordfvEXT}
  glFogCoorddEXT: procedure(coord: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFogCoorddEXT}
  glFogCoorddvEXT: procedure(coord: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFogCoorddvEXT}
  glFogCoordPointerEXT: procedure(AType: TGLenum; stride: TGLsizei; p: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFogCoordPointerEXT}

  // GL_EXT_coordinate_frame
  glTangent3bEXT: procedure(tx, ty, tz: TGLbyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTangent3bEXT}
  glTangent3bvEXT: procedure(v: PGLbyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTangent3bvEXT}
  glTangent3dEXT: procedure(tx, ty, tz: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTangent3dEXT}
  glTangent3dvEXT: procedure(v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTangent3dvEXT}
  glTangent3fEXT: procedure(tx, ty, tz: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTangent3fEXT}
  glTangent3fvEXT: procedure(v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTangent3fvEXT}
  glTangent3iEXT: procedure(tx, ty, tz: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTangent3iEXT}
  glTangent3ivEXT: procedure(v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTangent3ivEXT}
  glTangent3sEXT: procedure(tx, ty, tz: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTangent3sEXT}
  glTangent3svEXT: procedure(v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTangent3svEXT}

  glBinormal3bEXT: procedure(bx, by, bz: TGLbyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBinormal3bEXT}
  glBinormal3bvEXT: procedure(v: PGLbyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBinormal3bvEXT}
  glBinormal3dEXT: procedure(bx, by, bz: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBinormal3dEXT}
  glBinormal3dvEXT: procedure(v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBinormal3dvEXT}
  glBinormal3fEXT: procedure(bx, by, bz: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBinormal3fEXT}
  glBinormal3fvEXT: procedure(v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBinormal3fvEXT}
  glBinormal3iEXT: procedure(bx, by, bz: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBinormal3iEXT}
  glBinormal3ivEXT: procedure(v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBinormal3ivEXT}
  glBinormal3sEXT: procedure(bx, by, bz: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBinormal3sEXT}
  glBinormal3svEXT: procedure(v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBinormal3svEXT}
  glTangentPointerEXT: procedure(Atype: TGLenum; stride: TGLsizei; p: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTangentPointerEXT}
  glBinormalPointerEXT: procedure(Atype: TGLenum; stride: TGLsizei; p: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBinormalPointerEXT}

  // GL_SUNX_constant_data
  glFinishTextureSUNX: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFinishTextureSUNX}

  // GL_SUN_global_alpha
  glGlobalAlphaFactorbSUN: procedure(factor: TGLbyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGlobalAlphaFactorbSUN}
  glGlobalAlphaFactorsSUN: procedure(factor: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGlobalAlphaFactorsSUN}
  glGlobalAlphaFactoriSUN: procedure(factor: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGlobalAlphaFactoriSUN}
  glGlobalAlphaFactorfSUN: procedure(factor: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGlobalAlphaFactorfSUN}
  glGlobalAlphaFactordSUN: procedure(factor: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGlobalAlphaFactordSUN}
  glGlobalAlphaFactorubSUN: procedure(factor: TGLubyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGlobalAlphaFactorubSUN}
  glGlobalAlphaFactorusSUN: procedure(factor: TGLushort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGlobalAlphaFactorusSUN}
  glGlobalAlphaFactoruiSUN: procedure(factor: TGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGlobalAlphaFactoruiSUN}

  // GL_SUN_triangle_list
  glReplacementCodeuiSUN: procedure(code: TGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeuiSUN}
  glReplacementCodeusSUN: procedure(code: TGLushort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeusSUN}
  glReplacementCodeubSUN: procedure(code: TGLubyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeubSUN}
  glReplacementCodeuivSUN: procedure(code: PGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeuivSUN}
  glReplacementCodeusvSUN: procedure(code: PGLushort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeusvSUN}
  glReplacementCodeubvSUN: procedure(code: PGLubyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeubvSUN}
  glReplacementCodePointerSUN: procedure(Atype: TGLenum; stride: TGLsizei; var p); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodePointerSUN}

  // GL_SUN_vertex
  glColor4ubVertex2fSUN: procedure(r, g, b, a: TGLubyte; x, y: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4ubVertex2fSUN}
  glColor4ubVertex2fvSUN: procedure(c: PGLubyte; v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4ubVertex2fvSUN}
  glColor4ubVertex3fSUN: procedure(r, g, b, a: TGLubyte; x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4ubVertex3fSUN}
  glColor4ubVertex3fvSUN: procedure(c: PGLubyte; v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4ubVertex3fvSUN}
  glColor3fVertex3fSUN: procedure(r, g, b, x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor3fVertex3fSUN}
  glColor3fVertex3fvSUN: procedure(c, v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor3fVertex3fvSUN}
  glNormal3fVertex3fSUN: procedure(nx, ny, nz: TGLfloat; x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glNormal3fVertex3fSUN}
  glNormal3fVertex3fvSUN: procedure(n, v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glNormal3fVertex3fvSUN}
  glColor4fNormal3fVertex3fSUN: procedure(r, g, b, a, nx, ny, nz, x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4fNormal3fVertex3fSUN}
  glColor4fNormal3fVertex3fvSUN: procedure(c, n, v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColor4fNormal3fVertex3fvSUN}
  glTexCoord2fVertex3fSUN: procedure(s, t, x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord2fVertex3fSUN}
  glTexCoord2fVertex3fvSUN: procedure(tc, v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord2fVertex3fvSUN}
  glTexCoord4fVertex4fSUN: procedure(s, t, p, q, x, y, z, w: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord4fVertex4fSUN}
  glTexCoord4fVertex4fvSUN: procedure(tc, v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord4fVertex4fvSUN}
  glTexCoord2fColor4ubVertex3fSUN: procedure(s, t, r, g, b, a, x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord2fColor4ubVertex3fSUN}
  glTexCoord2fColor4ubVertex3fvSUN: procedure(tc: PGLfloat; c: PGLubyte; v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord2fColor4ubVertex3fvSUN}
  glTexCoord2fColor3fVertex3fSUN: procedure(s, t, r, g, b, x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord2fColor3fVertex3fSUN}
  glTexCoord2fColor3fVertex3fvSUN: procedure(tc, c, v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord2fColor3fVertex3fvSUN}
  glTexCoord2fNormal3fVertex3fSUN: procedure(s, t, nx, ny, nz, x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord2fNormal3fVertex3fSUN}
  glTexCoord2fNormal3fVertex3fvSUN: procedure(tc, n, v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord2fNormal3fVertex3fvSUN}
  glTexCoord2fColor4fNormal3fVertex3fSUN: procedure(s, t, r, g, b, a, nx, ny, nz, x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord2fColor4fNormal3fVertex3fSUN}
  glTexCoord2fColor4fNormal3fVertex3fvSUN: procedure(tc, c, n, v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord2fColor4fNormal3fVertex3fvSUN}
  glTexCoord4fColor4fNormal3fVertex4fSUN: procedure(s, t, p, q, r, g, b, a, nx, ny, nz, x, y, z, w: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord4fColor4fNormal3fVertex4fSUN}
  glTexCoord4fColor4fNormal3fVertex4fvSUN: procedure(tc, c, n, v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoord4fColor4fNormal3fVertex4fvSUN}
  glReplacementCodeuiVertex3fSUN: procedure(rc: TGLenum; x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeuiVertex3fSUN}
  glReplacementCodeuiVertex3fvSUN: procedure(rc: PGLenum; v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeuiVertex3fvSUN}
  glReplacementCodeuiColor4ubVertex3fSUN: procedure(rc: TGLenum; r, g, b, a: TGLubyte; x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeuiColor4ubVertex3fSUN}
  glReplacementCodeuiColor4ubVertex3fvSUN: procedure(rc: PGLenum; c: PGLubyte; v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeuiColor4ubVertex3fvSUN}
  glReplacementCodeuiColor3fVertex3fSUN: procedure(rc: TGLenum; r, g, b, x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeuiColor3fVertex3fSUN}
  glReplacementCodeuiColor3fVertex3fvSUN: procedure(rc: PGLenum; c, v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeuiColor3fVertex3fvSUN}
  glReplacementCodeuiNormal3fVertex3fSUN: procedure(rc: TGLenum; nx, ny, nz, x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeuiNormal3fVertex3fSUN}
  glReplacementCodeuiNormal3fVertex3fvSUN: procedure(rc: PGLenum; n, v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeuiNormal3fVertex3fvSUN}
  glReplacementCodeuiColor4fNormal3fVertex3fSUN: procedure(rc: TGLenum; r, g, b, a, nx, ny, nz, x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeuiColor4fNormal3fVertex3fSUN}
  glReplacementCodeuiColor4fNormal3fVertex3fvSUN: procedure(rc: PGLenum; c, n, v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeuiColor4fNormal3fVertex3fvSUN}
  glReplacementCodeuiTexCoord2fVertex3fSUN: procedure(rc: TGLenum; s, t, x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeuiTexCoord2fVertex3fSUN}
  glReplacementCodeuiTexCoord2fVertex3fvSUN: procedure(rc: PGLenum; tc, v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeuiTexCoord2fVertex3fvSUN}
  glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN: procedure(rc: TGLenum; s, t, nx, ny, nz, x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN}
  glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN: procedure(rc: PGLenum; tc, n, v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN}
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN: procedure(rc: TGLenum; s, t, r, g, b, a, nx, ny, nz, x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN}
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN: procedure(rc: PGLenum; tc, c, n, v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN}

  // GL_EXT_blend_func_separate
  glBlendFuncSeparateEXT: procedure(sfactorRGB, dfactorRGB, sfactorAlpha, dfactorAlpha: TGLenum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBlendFuncSeparateEXT}

  // GL_EXT_vertex_weighting
  glVertexWeightfEXT: procedure(weight: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexWeightfEXT}
  glVertexWeightfvEXT: procedure(weight: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexWeightfvEXT}
  glVertexWeightPointerEXT: procedure(Size: TGLsizei; Atype: TGLenum; stride: TGLsizei; p: pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexWeightPointerEXT}

  // GL_NV_vertex_array_range
  glFlushVertexArrayRangeNV: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFlushVertexArrayRangeNV}
  glVertexArrayRangeNV: procedure(Size: TGLsizei; p: pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexArrayRangeNV}
  wglAllocateMemoryNV: function(size: TGLsizei; readFrequency, writeFrequency, priority: Single): Pointer; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM wglAllocateMemoryNV}
  wglFreeMemoryNV: procedure(ptr: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM wglFreeMemoryNV}

  // GL_NV_register_combiners
  glCombinerParameterfvNV: procedure(pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCombinerParameterfvNV}
  glCombinerParameterfNV: procedure(pname: TGLenum; param: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCombinerParameterfNV}
  glCombinerParameterivNV: procedure(pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCombinerParameterivNV}
  glCombinerParameteriNV: procedure(pname: TGLenum; param: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCombinerParameteriNV}
  glCombinerInputNV: procedure(stage, portion, variable, input, mapping, componentUsage: TGLenum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCombinerInputNV}
  glCombinerOutputNV: procedure(stage, portion, abOutput, cdOutput, sumOutput, scale, bias: TGLenum; abDotProduct, cdDotProduct, muxSum: TGLboolean); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glCombinerOutputNV}
  glFinalCombinerInputNV: procedure(variable, input, mapping, componentUsage: TGLenum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFinalCombinerInputNV}
  glGetCombinerInputParameterfvNV: procedure(stage, portion, variable, pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetCombinerInputParameterfvNV}
  glGetCombinerInputParameterivNV: procedure(stage, portion, variable, pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetCombinerInputParameterivNV}
  glGetCombinerOutputParameterfvNV: procedure(stage, portion, pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetCombinerOutputParameterfvNV}
  glGetCombinerOutputParameterivNV: procedure(stage, portion, pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetCombinerOutputParameterivNV}
  glGetFinalCombinerInputParameterfvNV: procedure(variable, pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetFinalCombinerInputParameterfvNV}
  glGetFinalCombinerInputParameterivNV: procedure(variable, pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetFinalCombinerInputParameterivNV}

  // GL_MESA_resize_buffers
  glResizeBuffersMESA: procedure; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glResizeBuffersMESA}

  // GL_MESA_window_pos
  glWindowPos2dMESA: procedure(x, y: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos2dMESA}
  glWindowPos2dvMESA: procedure(v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos2dvMESA}
  glWindowPos2fMESA: procedure(x, y: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos2fMESA}
  glWindowPos2fvMESA: procedure(v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos2fvMESA}
  glWindowPos2iMESA: procedure(x, y: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos2iMESA}
  glWindowPos2ivMESA: procedure(v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos2ivMESA}
  glWindowPos2sMESA: procedure(x, y: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos2sMESA}
  glWindowPos2svMESA: procedure(v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos2svMESA}
  glWindowPos3dMESA: procedure(x, y, z: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos3dMESA}
  glWindowPos3dvMESA: procedure(v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos3dvMESA}
  glWindowPos3fMESA: procedure(x, y, z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos3fMESA}
  glWindowPos3fvMESA: procedure(v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos3fvMESA}
  glWindowPos3iMESA: procedure(x, y, z: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos3iMESA}
  glWindowPos3ivMESA: procedure(v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos3ivMESA}
  glWindowPos3sMESA: procedure(x, y, z: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos3sMESA}
  glWindowPos3svMESA: procedure(v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos3svMESA}
  glWindowPos4dMESA: procedure(x, y, z, w: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos4dMESA}
  glWindowPos4dvMESA: procedure(v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos4dvMESA}
  glWindowPos4fMESA: procedure(x, y, z, w: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos4fMESA}
  glWindowPos4fvMESA: procedure(v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos4fvMESA}
  glWindowPos4iMESA: procedure(x, y, z, w: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos4iMESA}
  glWindowPos4ivMESA: procedure(v: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos4ivMESA}
  glWindowPos4sMESA: procedure(x, y, z, w: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos4sMESA}
  glWindowPos4svMESA: procedure(v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glWindowPos4svMESA}

  // GL_IBM_multimode_draw_arrays
  glMultiModeDrawArraysIBM: procedure(mode: TGLenum; First: PGLint; Count: PGLsizei; primcount: TGLsizei; modestride: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiModeDrawArraysIBM}
  glMultiModeDrawElementsIBM: procedure(mode: PGLenum; Count: PGLsizei; Atype: TGLenum; var indices; primcount: TGLsizei; modestride: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glMultiModeDrawElementsIBM}

  // GL_IBM_vertex_array_lists
  glColorPointerListIBM: procedure(Size: TGLint; Atype: TGLenum; stride: TGLint; var p; ptrstride: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glColorPointerListIBM}
  glSecondaryColorPointerListIBM: procedure(Size: TGLint; Atype: TGLenum; stride: TGLint; var p; ptrstride: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSecondaryColorPointerListIBM}
  glEdgeFlagPointerListIBM: procedure(stride: TGLint; var p: PGLboolean; ptrstride: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glEdgeFlagPointerListIBM}
  glFogCoordPointerListIBM: procedure(Atype: TGLenum; stride: TGLint; var p; ptrstride: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glFogCoordPointerListIBM}
  glIndexPointerListIBM: procedure(Atype: TGLenum; stride: TGLint; var p; ptrstride: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIndexPointerListIBM}
  glNormalPointerListIBM: procedure(Atype: TGLenum; stride: TGLint; var p; ptrstride: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glNormalPointerListIBM}
  glTexCoordPointerListIBM: procedure(Size: TGLint; Atype: TGLenum; stride: TGLint; var p; ptrstride: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTexCoordPointerListIBM}
  glVertexPointerListIBM: procedure(Size: TGLint; Atype: TGLenum; stride: TGLint; var p; ptrstride: TGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexPointerListIBM}

  // GL_3DFX_tbuffer
  glTbufferMask3DFX: procedure(mask: TGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTbufferMask3DFX}

  // GL_EXT_multisample
  glSampleMaskEXT: procedure(Value: TGLclampf; invert: TGLboolean); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSampleMaskEXT}
  glSamplePatternEXT: procedure(pattern: TGLenum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glSamplePatternEXT}

  // GL_SGIS_texture_color_mask
  glTextureColorMaskSGIS: procedure(red, green, blue, alpha: TGLboolean); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTextureColorMaskSGIS}

  // GL_SGIX_igloo_interface
  glIglooInterfaceSGIX: procedure(pname: TGLenum; params: pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIglooInterfaceSGIX}

  // GL_NV_vertex_program
  glAreProgramsResidentNV: procedure(n: TGLSizei; programs: PGLuint; residences: PGLboolean); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glAreProgramsResidentNV}
  glBindProgramNV: procedure(target: TGLenum; id: TGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glBindProgramNV}
  glDeleteProgramsNV: procedure(n: TGLSizei; programs: PGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glDeleteProgramsNV}
  glExecuteProgramNV: procedure(target: TGLenum; id: TGLuint; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glExecuteProgramNV}
  glGenProgramsNV: procedure(n: TGLSizei; programs: PGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGenProgramsNV}
  glGetProgramParameterdvNV: procedure (target: TGLenum; index: TGLuint; pname: TGLenum; params: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetProgramParameterdvNV}
  glGetProgramParameterfvNV: procedure (target: TGLenum; index: TGLuint; pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetProgramParameterfvNV}
  glGetProgramivNV: procedure (id: TGLuint; pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetProgramivNV}
  glGetProgramStringNV: procedure (id: TGLuint; pname: TGLenum; programIdx: PGLubyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetProgramStringNV}
  glGetTrackMatrixivNV: procedure (target: TGLenum; address: TGLuint; pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetTrackMatrixivNV}
  glGetVertexAttribdvNV: procedure (index: TGLuint; pname: TGLenum; params: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetVertexAttribdvNV}
  glGetVertexAttribfvNV: procedure (index: TGLuint; pname: TGLenum; params: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetVertexAttribfvNV}
  glGetVertexAttribivNV: procedure (index: TGLuint; pname: TGLenum; params: PGLint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetVertexAttribivNV}
  glGetVertexAttribPointervNV: procedure (index: TGLuint; pname: TGLenum; pointer: PPointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glGetVertexAttribPointervNV}
  glIsProgramNV: function (id: TGLuint): TGLboolean; {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glIsProgramNV}
  glLoadProgramNV: procedure (target: TGLenum; id: TGLuint; len: TGLSizei; programIdx: PGLubyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glLoadProgramNV}
  glProgramParameter4dNV: procedure (target: TGLenum; index: TGLuint; x, y, z, w: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glProgramParameter4dNV}
  glProgramParameter4dvNV: procedure (target: TGLenum; index: TGLuint; v: PGLdouble ); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glProgramParameter4dvNV}
  glProgramParameter4fNV: procedure (target: TGLenum; index: TGLuint; x, y, z, w: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glProgramParameter4fNV}
  glProgramParameter4fvNV: procedure (target: TGLenum; index: TGLuint; v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glProgramParameter4fvNV}
  glProgramParameters4dvNV: procedure (target: TGLenum; index: TGLuint; count: TGLSizei; v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glProgramParameters4dvNV}
  glProgramParameters4fvNV: procedure (target: TGLenum; index: TGLuint; count: TGLSizei; v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glProgramParameters4fvNV}
  glRequestResidentProgramsNV: procedure (n: TGLSizei; programs: PGLuint); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glRequestResidentProgramsNV}
  glTrackMatrixNV: procedure (target: TGLenum; address: TGLuint; matrix: TGLenum; transform: TGLenum); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glTrackMatrixNV}
  glVertexAttribPointerNV: procedure (index: TGLuint; fsize: TGLint; vertextype: TGLenum; stride: TGLSizei; pointer: Pointer); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttribPointerNV}
  glVertexAttrib1dNV: procedure (index: TGLuint; x: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib1dNV}
  glVertexAttrib1dvNV: procedure (index: TGLuint; v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib1dvNV}
  glVertexAttrib1fNV: procedure (index: TGLuint; x: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib1fNV}
  glVertexAttrib1fvNV: procedure (index: TGLuint; v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib1fvNV}
  glVertexAttrib1sNV: procedure (index: TGLuint; x: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib1sNV}
  glVertexAttrib1svNV: procedure (index: TGLuint; v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib1svNV}
  glVertexAttrib2dNV: procedure (index: TGLuint; x: TGLdouble; y: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib2dNV}
  glVertexAttrib2dvNV: procedure (index: TGLuint; v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib2dvNV}
  glVertexAttrib2fNV: procedure (index: TGLuint; x: TGLfloat; y: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib2fNV}
  glVertexAttrib2fvNV: procedure (index: TGLuint; v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib2fvNV}
  glVertexAttrib2sNV: procedure (index: TGLuint; x: TGLshort; y: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib2sNV}
  glVertexAttrib2svNV: procedure (index: TGLuint; v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib2svNV}
  glVertexAttrib3dNV: procedure (index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib3dNV}
  glVertexAttrib3dvNV: procedure (index: TGLuint; v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib3dvNV}
  glVertexAttrib3fNV: procedure (index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib3fNV}
  glVertexAttrib3fvNV: procedure (index: TGLuint; v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib3fvNV}
  glVertexAttrib3sNV: procedure (index: TGLuint; x: TGLshort; y: TGLshort; z: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib3sNV}
  glVertexAttrib3svNV: procedure (index: TGLuint; v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib3svNV}
  glVertexAttrib4dNV: procedure (index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib4dNV}
  glVertexAttrib4dvNV: procedure (index: TGLuint; v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib4dvNV}
  glVertexAttrib4fNV: procedure(index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib4fNV}
  glVertexAttrib4fvNV: procedure(index: TGLuint; v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib4fvNV}
  glVertexAttrib4sNV: procedure (index: TGLuint; x: TGLshort; y: TGLshort; z: TGLdouble; w: TGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib4sNV}
  glVertexAttrib4svNV: procedure (index: TGLuint; v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib4svNV}
  glVertexAttrib4ubvNV: procedure (index: TGLuint; v: PGLubyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttrib4ubvNV}
  glVertexAttribs1dvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttribs1dvNV}
  glVertexAttribs1fvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttribs1fvNV}
  glVertexAttribs1svNV: procedure (index: TGLuint; count: TGLSizei; v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttribs1svNV}
  glVertexAttribs2dvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttribs2dvNV}
  glVertexAttribs2fvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttribs2fvNV}
  glVertexAttribs2svNV: procedure (index: TGLuint; count: TGLSizei; v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttribs2svNV}
  glVertexAttribs3dvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttribs3dvNV}
  glVertexAttribs3fvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttribs3fvNV}
  glVertexAttribs3svNV: procedure (index: TGLuint; count: TGLSizei; v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttribs3svNV}
  glVertexAttribs4dvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLdouble); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttribs4dvNV}
  glVertexAttribs4fvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLfloat); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttribs4fvNV}
  glVertexAttribs4svNV: procedure (index: TGLuint; count: TGLSizei; v: PGLshort); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttribs4svNV}
  glVertexAttribs4ubvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLubyte); {$ifdef Win32} stdcall; {$endif} {$ifdef UNIX} cdecl; {$endif}
  {$EXTERNALSYM glVertexAttribs4ubvNV}

{$ifdef UNIX}
type
  GLXContext     = Pointer; 
  GLXPixmap      = XID; 
  GLXDrawable    = XID; 

  // GLX 1.3 and later
  GLXFBConfig    = Pointer; 
  GLXFBConfigID  = XID; 
  GLXContextID   = XID; 
  GLXWindow      = XID; 
  GLXPbuffer     = XID;

var
  glXChooseVisual: function(dpy: PDisplay; screen: TGLint; attribList: PGLint): PXVisualInfo; cdecl;
  {$EXTERNALSYM glXChooseVisual}
  glXCreateContext: function(dpy: PDisplay; vis: PXVisualInfo; shareList: GLXContext; direct: TGLboolean): GLXContext; cdecl;
  {$EXTERNALSYM glXCreateContext}
  glXDestroyContext: procedure(dpy: PDisplay; ctx: GLXContext); cdecl;
  {$EXTERNALSYM glXDestroyContext}
  glXMakeCurrent: function(dpy: PDisplay; drawable: GLXDrawable; ctx: GLXContext): TGLboolean; cdecl;
  {$EXTERNALSYM glXMakeCurrent}
  glXCopyContext: procedure(dpy: PDisplay; src: GLXContext; dst: GLXContext; mask: TGLuint); cdecl;
  {$EXTERNALSYM glXCopyContext}
  glXSwapBuffers: procedure(dpy: PDisplay; drawable: GLXDrawable); cdecl;
  {$EXTERNALSYM glXSwapBuffers}
  glXCreateGLXPixmap: function(dpy: PDisplay; visual: PXVisualInfo; pixmap: Pixmap): GLXPixmap; cdecl;
  {$EXTERNALSYM glXCreateGLXPixmap}
  glXDestroyGLXPixmap: procedure(dpy: PDisplay; pixmap: GLXPixmap); cdecl;
  {$EXTERNALSYM glXDestroyGLXPixmap}
  glXQueryExtension: function(dpy: PDisplay; errorb: PGLInt; event: PGLInt): TGLboolean; cdecl;
  {$EXTERNALSYM glXQueryExtension}
  glXQueryVersion: function(dpy: PDisplay; maj: PGLInt; min: PGLINT): TGLboolean; cdecl;
  {$EXTERNALSYM glXQueryVersion}
  glXIsDirect: function(dpy: PDisplay; ctx: GLXContext): TGLboolean; cdecl;
  {$EXTERNALSYM glXIsDirect}
  glXGetConfig: function(dpy: PDisplay; visual: PXVisualInfo; attrib: TGLInt; value: PGLInt): TGLInt; cdecl;
  {$EXTERNALSYM glXGetConfig}
  glXGetCurrentContext: function: GLXContext; cdecl;
  {$EXTERNALSYM glXGetCurrentContext}
  glXGetCurrentDrawable: function: GLXDrawable; cdecl;
  {$EXTERNALSYM glXGetCurrentDrawable}
  glXWaitGL: procedure; cdecl;
  {$EXTERNALSYM glXWaitGL}
  glXWaitX: procedure; cdecl;
  {$EXTERNALSYM glXWaitX}
  glXUseXFont: procedure(font: Font; first: TGLInt; count: TGLInt; list: TGLint); cdecl;
  {$EXTERNALSYM glXUseXFont}

  // GLX 1.1 and later
  glXQueryExtensionsString: function(dpy: PDisplay; screen: TGLInt): PChar; cdecl;
  {$EXTERNALSYM glXQueryExtensionsString}
  glXQueryServerString: function(dpy: PDisplay; screen: TGLInt; name: TGLInt): PChar; cdecl;
  {$EXTERNALSYM glXQueryServerString}
  glXGetClientString: function(dpy: PDisplay; name: TGLInt): PChar; cdecl;
  {$EXTERNALSYM glXGetClientString}

  // GLX 1.2 and later
  glXGetCurrentDisplay: function: PDisplay; cdecl;
  {$EXTERNALSYM glXGetCurrentDisplay}

  // GLX 1.3 and later
  glXChooseFBConfig: function(dpy: PDisplay; screen: TGLInt; attribList: PGLInt; nitems: PGLInt): GLXFBConfig; cdecl;
  {$EXTERNALSYM glXChooseFBConfig}
  glXGetFBConfigAttrib: function(dpy: PDisplay; config: GLXFBConfig; attribute: TGLInt; value: PGLInt): TGLInt; cdecl;
  {$EXTERNALSYM glXGetFBConfigAttrib}
  glXGetFBConfigs: function(dpy: PDisplay; screen: TGLInt; nelements: PGLInt): GLXFBConfig; cdecl;
  {$EXTERNALSYM glXGetFBConfigs}
  glXGetVisualFromFBConfig: function(dpy: PDisplay; config: GLXFBConfig): PXVisualInfo; cdecl;
  {$EXTERNALSYM glXGetVisualFromFBConfig}
  glXCreateWindow: function(dpy: PDisplay; config: GLXFBConfig; win: Window; const attribList: PGLInt): GLXWindow; cdecl;
  {$EXTERNALSYM glXCreateWindow}
  glXDestroyWindow: procedure(dpy: PDisplay; window: GLXWindow); cdecl;
  {$EXTERNALSYM glXDestroyWindow}
  glXCreatePixmap: function(dpy: PDisplay; config: GLXFBConfig; pixmap: Pixmap; attribList: PGLInt): GLXPixmap; cdecl;
  {$EXTERNALSYM glXCreatePixmap}
  glXDestroyPixmap: procedure(dpy: PDisplay; pixmap: GLXPixmap); cdecl;
  {$EXTERNALSYM glXDestroyPixmap}
  glXCreatePbuffer: function(dpy: PDisplay; config: GLXFBConfig; attribList: PGLInt): GLXPBuffer; cdecl;
  {$EXTERNALSYM glXCreatePbuffer}
  glXDestroyPbuffer: procedure(dpy: PDisplay; pbuf: GLXPBuffer); cdecl;
  {$EXTERNALSYM glXDestroyPbuffer}
  glXQueryDrawable: procedure(dpy: PDisplay; draw: GLXDrawable; attribute: TGLInt; value: PGLuint); cdecl;
  {$EXTERNALSYM glXQueryDrawable}
  glXCreateNewContext: function(dpy: PDisplay; config: GLXFBConfig; renderType: TGLInt; shareList: GLXContext; direct: TGLboolean): GLXContext; cdecl;
  {$EXTERNALSYM glXCreateNewContext}
  glXMakeContextCurrent: function(dpy: PDisplay; draw: GLXDrawable; read: GLXDrawable; ctx: GLXContext): TGLboolean; cdecl;
  {$EXTERNALSYM glXMakeContextCurrent}
  glXGetCurrentReadDrawable: function: GLXDrawable; cdecl;
  {$EXTERNALSYM glXGetCurrentReadDrawable}
  glXQueryContext: function(dpy: PDisplay; ctx: GLXContext; attribute: TGLInt; value: PGLInt): TGLInt; cdecl;
  {$EXTERNALSYM glXQueryContext}
  glXSelectEvent: procedure(dpy: PDisplay; drawable: GLXDrawable; mask: TGLsizei); cdecl;
  {$EXTERNALSYM glXSelectEvent}
  glXGetSelectedEvent: procedure(dpy: PDisplay; drawable: GLXDrawable; mask: TGLsizei); cdecl;
  {$EXTERNALSYM glXGetSelectedEvent}
  glXGetVideoSyncSGI: function(count: PGLuint): TGLInt; cdecl;
  {$EXTERNALSYM glXGetVideoSyncSGI}
  glXWaitVideoSyncSGI: function(divisor: TGLInt; remainder: TGLInt; count: PGLuint): TGLInt; cdecl;
  {$EXTERNALSYM glXWaitVideoSyncSGI}
  glXFreeContextEXT: procedure(dpy: PDisplay; context: GLXContext); cdecl;
  {$EXTERNALSYM glXFreeContextEXT}
  glXGetContextIDEXT: function(const context: GLXContext): GLXContextID; cdecl;
  {$EXTERNALSYM glXGetContextIDEXT}
  glXGetCurrentDisplayEXT: function: PDisplay; cdecl;
  {$EXTERNALSYM glXGetCurrentDisplayEXT}
  glXImportContextEXT: function(dpy: PDisplay; contextID: GLXContextID): GLXContext; cdecl;
  {$EXTERNALSYM glXImportContextEXT}
  glXQueryContextInfoEXT: function(dpy: PDisplay; context: GLXContext; attribute: TGLInt; value: PGLInt): TGLInt; cdecl;
  {$EXTERNALSYM glXQueryContextInfoEXT}
  glXCopySubBufferMESA: procedure(dpy: PDisplay; drawable: GLXDrawable; x: TGLInt; y: TGLInt; width: TGLInt; height: TGLInt); cdecl;
  {$EXTERNALSYM glXCopySubBufferMESA}
  glXCreateGLXPixmapMESA: function(dpy: PDisplay; visual: PXVisualInfo; pixmap: Pixmap; cmap: Colormap): GLXPixmap; cdecl;
  {$EXTERNALSYM glXCreateGLXPixmapMESA}
  glXReleaseBuffersMESA: function(dpy: PDisplay; d: GLXDrawable): TGLboolean; cdecl;
  {$EXTERNALSYM glXReleaseBuffersMESA}
  glXSet3DfxModeMESA: function(mode: TGLint): TGLboolean; cdecl;
  {$EXTERNALSYM glXSet3DfxModeMESA}
{$endif}


//----------------------------------------------------------------------------------------------------------------------

procedure CloseOpenGL;
function InitOpenGL: Boolean;
function InitOpenGLFromLibrary(GLName, GLUName: PChar): Boolean;
function IsOpenGLInitialized: Boolean;

// Compatibility routines
procedure UnloadOpenGL;
function LoadOpenGL: Boolean;
function LoadOpenGLFromLibrary(GLName, GLUName: PChar): Boolean;
function IsOpenGLLoaded: Boolean;

{$ifdef Win32}
procedure ActivateRenderingContext(DC: HDC; RC: HGLRC); 
function CreateRenderingContext(DC: HDC; Options: TRCOptions; ColorBits, StencilBits, AccumBits, AuxBuffers: Integer; Layer: Integer; var Palette: HPALETTE): HGLRC; 
function CurrentDC: HDC;
procedure DeactivateRenderingContext;
procedure DestroyRenderingContext(RC: HGLRC); 
procedure ClearExtensions; 
function HasActiveContext: Boolean; 
procedure ReadExtensions;
procedure ReadImplementationProperties;
{$endif}

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  SysUtils,
  Classes,
  sdl,
  moduleloader;

type
  EOpenGLException = class(Exception);

{$ifndef FPC} 
threadvar
{$else}
var 
{$endif}
  LastPixelFormat: Integer; 
// ActivationRefCount: Integer; // Auto Removed, Unused Variable

{$ifdef Win32}
const
  INVALID_MODULEHANDLE = 0; 

var
  GLHandle: TModuleHandle;
  GLUHandle: TModuleHandle; 
{$endif}

{$ifdef UNIX}
const
  INVALID_MODULEHANDLE = nil; 

var
  GLHandle: TModuleHandle;
  GLUHandle: TModuleHandle; 
{$endif}

  // The context list is used to determine if a context is active already in any thread.
  ContextList: TThreadList; 

resourcestring
  SRCAlreadyActive = 'Rendering context already active in another thread.'; 
  SMakeCurrentFailed = 'wglMakeCurrent failed'; 
  SDeleteContextFailed = 'wglDeleteContext failed'; 
  SContextInUse = 'Cannot delete rendering context. It is still in use by another thread.'; 

{$ifdef Win32}
  SDefaultGLLibrary = 'OpenGL32.dll'; 
  SDefaultGLULibrary = 'GLU32.dll'; 
{$endif}

{$ifdef UNIX}
  SDefaultGLLibrary = 'libGL.so'; 
  SDefaultGLULibrary = 'libGLU.so'; 
{$endif}

//----------------------------------------------------------------------------------------------------------------------

procedure ShowError(const Message: string);

begin
  raise EOpenGLException.Create(Message);
end;

//----------------------------------------------------------------------------------------------------------------------

{$ifndef VER140}

procedure RaiseLastOSError;
begin
  {$ifndef FPC}
  RaiseLastWin32Error;
  {$endif}
end;

{$endif VER140}

//----------------------------------------------------------------------------------------------------------------------

procedure ClearProcAddresses;

begin
  glAccum := nil; 
  glAlphaFunc := nil; 
  glAreTexturesResident := nil; 
  glArrayElement := nil; 
  glBegin := nil; 
  glBindTexture := nil; 
  glBitmap := nil; 
  glBlendFunc := nil; 
  glCallList := nil; 
  glCallLists := nil; 
  glClear := nil; 
  glClearAccum := nil; 
  glClearColor := nil; 
  glClearDepth := nil; 
  glClearIndex := nil; 
  glClearStencil := nil; 
  glClipPlane := nil; 
  glColor3b := nil; 
  glColor3bv := nil; 
  glColor3d := nil; 
  glColor3dv := nil;
  glColor3f := nil; 
  glColor3fv := nil; 
  glColor3i := nil; 
  glColor3iv := nil; 
  glColor3s := nil; 
  glColor3sv := nil; 
  glColor3ub := nil; 
  glColor3ubv := nil; 
  glColor3ui := nil; 
  glColor3uiv := nil; 
  glColor3us := nil; 
  glColor3usv := nil; 
  glColor4b := nil; 
  glColor4bv := nil; 
  glColor4d := nil; 
  glColor4dv := nil; 
  glColor4f := nil; 
  glColor4fv := nil; 
  glColor4i := nil; 
  glColor4iv := nil; 
  glColor4s := nil; 
  glColor4sv := nil; 
  glColor4ub := nil; 
  glColor4ubv := nil; 
  glColor4ui := nil;
  glColor4uiv := nil; 
  glColor4us := nil; 
  glColor4usv := nil; 
  glColorMask := nil; 
  glColorMaterial := nil; 
  glColorPointer := nil; 
  glCopyPixels := nil; 
  glCopyTexImage1D := nil; 
  glCopyTexImage2D := nil; 
  glCopyTexSubImage1D := nil; 
  glCopyTexSubImage2D := nil; 
  glCullFace := nil; 
  glDeleteLists := nil; 
  glDeleteTextures := nil; 
  glDepthFunc := nil; 
  glDepthMask := nil; 
  glDepthRange := nil; 
  glDisable := nil; 
  glDisableClientState := nil; 
  glDrawArrays := nil; 
  glDrawBuffer := nil; 
  glDrawElements := nil; 
  glDrawPixels := nil; 
  glEdgeFlag := nil; 
  glEdgeFlagPointer := nil;
  glEdgeFlagv := nil; 
  glEnable := nil; 
  glEnableClientState := nil; 
  glEnd := nil; 
  glEndList := nil; 
  glEvalCoord1d := nil; 
  glEvalCoord1dv := nil; 
  glEvalCoord1f := nil; 
  glEvalCoord1fv := nil; 
  glEvalCoord2d := nil; 
  glEvalCoord2dv := nil; 
  glEvalCoord2f := nil; 
  glEvalCoord2fv := nil; 
  glEvalMesh1 := nil; 
  glEvalMesh2 := nil; 
  glEvalPoint1 := nil; 
  glEvalPoint2 := nil; 
  glFeedbackBuffer := nil; 
  glFinish := nil; 
  glFlush := nil; 
  glFogf := nil; 
  glFogfv := nil; 
  glFogi := nil; 
  glFogiv := nil; 
  glFrontFace := nil;
  glFrustum := nil; 
  glGenLists := nil; 
  glGenTextures := nil; 
  glGetBooleanv := nil; 
  glGetClipPlane := nil; 
  glGetDoublev := nil; 
  glGetError := nil; 
  glGetFloatv := nil; 
  glGetIntegerv := nil; 
  glGetLightfv := nil; 
  glGetLightiv := nil; 
  glGetMapdv := nil; 
  glGetMapfv := nil; 
  glGetMapiv := nil; 
  glGetMaterialfv := nil; 
  glGetMaterialiv := nil; 
  glGetPixelMapfv := nil; 
  glGetPixelMapuiv := nil; 
  glGetPixelMapusv := nil; 
  glGetPointerv := nil; 
  glGetPolygonStipple := nil; 
  glGetString := nil; 
  glGetTexEnvfv := nil; 
  glGetTexEnviv := nil; 
  glGetTexGendv := nil;
  glGetTexGenfv := nil; 
  glGetTexGeniv := nil; 
  glGetTexImage := nil; 
  glGetTexLevelParameterfv := nil; 
  glGetTexLevelParameteriv := nil; 
  glGetTexParameterfv := nil; 
  glGetTexParameteriv := nil; 
  glHint := nil; 
  glIndexMask := nil; 
  glIndexPointer := nil; 
  glIndexd := nil; 
  glIndexdv := nil; 
  glIndexf := nil; 
  glIndexfv := nil; 
  glIndexi := nil; 
  glIndexiv := nil; 
  glIndexs := nil; 
  glIndexsv := nil; 
  glIndexub := nil; 
  glIndexubv := nil; 
  glInitNames := nil; 
  glInterleavedArrays := nil; 
  glIsEnabled := nil; 
  glIsList := nil; 
  glIsTexture := nil;
  glLightModelf := nil; 
  glLightModelfv := nil; 
  glLightModeli := nil; 
  glLightModeliv := nil; 
  glLightf := nil; 
  glLightfv := nil; 
  glLighti := nil; 
  glLightiv := nil; 
  glLineStipple := nil; 
  glLineWidth := nil; 
  glListBase := nil; 
  glLoadIdentity := nil; 
  glLoadMatrixd := nil; 
  glLoadMatrixf := nil; 
  glLoadName := nil; 
  glLogicOp := nil; 
  glMap1d := nil; 
  glMap1f := nil; 
  glMap2d := nil; 
  glMap2f := nil; 
  glMapGrid1d := nil; 
  glMapGrid1f := nil; 
  glMapGrid2d := nil; 
  glMapGrid2f := nil; 
  glMaterialf := nil;
  glMaterialfv := nil; 
  glMateriali := nil; 
  glMaterialiv := nil; 
  glMatrixMode := nil; 
  glMultMatrixd := nil; 
  glMultMatrixf := nil; 
  glNewList := nil; 
  glNormal3b := nil; 
  glNormal3bv := nil; 
  glNormal3d := nil; 
  glNormal3dv := nil; 
  glNormal3f := nil; 
  glNormal3fv := nil; 
  glNormal3i := nil; 
  glNormal3iv := nil; 
  glNormal3s := nil; 
  glNormal3sv := nil; 
  glNormalPointer := nil; 
  glOrtho := nil; 
  glPassThrough := nil; 
  glPixelMapfv := nil; 
  glPixelMapuiv := nil; 
  glPixelMapusv := nil; 
  glPixelStoref := nil; 
  glPixelStorei := nil;
  glPixelTransferf := nil; 
  glPixelTransferi := nil; 
  glPixelZoom := nil; 
  glPointSize := nil; 
  glPolygonMode := nil; 
  glPolygonOffset := nil; 
  glPolygonStipple := nil; 
  glPopAttrib := nil; 
  glPopClientAttrib := nil; 
  glPopMatrix := nil; 
  glPopName := nil; 
  glPrioritizeTextures := nil; 
  glPushAttrib := nil; 
  glPushClientAttrib := nil; 
  glPushMatrix := nil; 
  glPushName := nil; 
  glRasterPos2d := nil; 
  glRasterPos2dv := nil; 
  glRasterPos2f := nil; 
  glRasterPos2fv := nil; 
  glRasterPos2i := nil; 
  glRasterPos2iv := nil; 
  glRasterPos2s := nil; 
  glRasterPos2sv := nil; 
  glRasterPos3d := nil;
  glRasterPos3dv := nil; 
  glRasterPos3f := nil; 
  glRasterPos3fv := nil; 
  glRasterPos3i := nil; 
  glRasterPos3iv := nil; 
  glRasterPos3s := nil; 
  glRasterPos3sv := nil; 
  glRasterPos4d := nil; 
  glRasterPos4dv := nil; 
  glRasterPos4f := nil; 
  glRasterPos4fv := nil; 
  glRasterPos4i := nil; 
  glRasterPos4iv := nil; 
  glRasterPos4s := nil; 
  glRasterPos4sv := nil; 
  glReadBuffer := nil; 
  glReadPixels := nil; 
  glRectd := nil; 
  glRectdv := nil; 
  glRectf := nil; 
  glRectfv := nil; 
  glRecti := nil; 
  glRectiv := nil; 
  glRects := nil; 
  glRectsv := nil;
  glRenderMode := nil; 
  glRotated := nil; 
  glRotatef := nil; 
  glScaled := nil; 
  glScalef := nil; 
  glScissor := nil; 
  glSelectBuffer := nil; 
  glShadeModel := nil; 
  glStencilFunc := nil; 
  glStencilMask := nil; 
  glStencilOp := nil; 
  glTexCoord1d := nil; 
  glTexCoord1dv := nil; 
  glTexCoord1f := nil; 
  glTexCoord1fv := nil; 
  glTexCoord1i := nil; 
  glTexCoord1iv := nil; 
  glTexCoord1s := nil; 
  glTexCoord1sv := nil; 
  glTexCoord2d := nil; 
  glTexCoord2dv := nil; 
  glTexCoord2f := nil; 
  glTexCoord2fv := nil; 
  glTexCoord2i := nil; 
  glTexCoord2iv := nil;
  glTexCoord2s := nil; 
  glTexCoord2sv := nil; 
  glTexCoord3d := nil; 
  glTexCoord3dv := nil; 
  glTexCoord3f := nil; 
  glTexCoord3fv := nil; 
  glTexCoord3i := nil; 
  glTexCoord3iv := nil; 
  glTexCoord3s := nil; 
  glTexCoord3sv := nil; 
  glTexCoord4d := nil; 
  glTexCoord4dv := nil; 
  glTexCoord4f := nil; 
  glTexCoord4fv := nil; 
  glTexCoord4i := nil; 
  glTexCoord4iv := nil; 
  glTexCoord4s := nil; 
  glTexCoord4sv := nil; 
  glTexCoordPointer := nil; 
  glTexEnvf := nil; 
  glTexEnvfv := nil; 
  glTexEnvi := nil; 
  glTexEnviv := nil; 
  glTexGend := nil; 
  glTexGendv := nil;
  glTexGenf := nil; 
  glTexGenfv := nil; 
  glTexGeni := nil; 
  glTexGeniv := nil; 
  glTexImage1D := nil; 
  glTexImage2D := nil; 
  glTexParameterf := nil; 
  glTexParameterfv := nil; 
  glTexParameteri := nil; 
  glTexParameteriv := nil; 
  glTexSubImage1D := nil; 
  glTexSubImage2D := nil; 
  glTranslated := nil; 
  glTranslatef := nil; 
  glVertex2d := nil; 
  glVertex2dv := nil; 
  glVertex2f := nil; 
  glVertex2fv := nil; 
  glVertex2i := nil; 
  glVertex2iv := nil; 
  glVertex2s := nil; 
  glVertex2sv := nil; 
  glVertex3d := nil; 
  glVertex3dv := nil; 
  glVertex3f := nil;
  glVertex3fv := nil; 
  glVertex3i := nil; 
  glVertex3iv := nil; 
  glVertex3s := nil; 
  glVertex3sv := nil; 
  glVertex4d := nil; 
  glVertex4dv := nil; 
  glVertex4f := nil; 
  glVertex4fv := nil; 
  glVertex4i := nil; 
  glVertex4iv := nil; 
  glVertex4s := nil; 
  glVertex4sv := nil; 
  glVertexPointer := nil; 
  glViewport := nil; 

  {$ifdef Win32}
  wglGetProcAddress := nil; 
  wglCopyContext := nil; 
  wglCreateContext := nil; 
  wglCreateLayerContext := nil; 
  wglDeleteContext := nil; 
  wglDescribeLayerPlane := nil; 
  wglGetCurrentContext := nil; 
  wglGetCurrentDC := nil;
  wglGetLayerPaletteEntries := nil; 
  wglMakeCurrent := nil; 
  wglRealizeLayerPalette := nil; 
  wglSetLayerPaletteEntries := nil; 
  wglShareLists := nil; 
  wglSwapLayerBuffers := nil; 
  wglSwapMultipleBuffers := nil; 
  wglUseFontBitmapsA := nil; 
  wglUseFontOutlinesA := nil; 
  wglUseFontBitmapsW := nil; 
  wglUseFontOutlinesW := nil; 
  wglUseFontBitmaps := nil; 
  wglUseFontOutlines := nil; 
  {$endif}

  // GL 1.2
  glDrawRangeElements := nil; 
  glTexImage3D := nil; 

  // GL 1.2 ARB imaging
  glBlendColor := nil; 
  glBlendEquation := nil; 
  glColorSubTable := nil; 
  glCopyColorSubTable := nil; 
  glColorTable := nil;
  glCopyColorTable := nil; 
  glColorTableParameteriv := nil; 
  glColorTableParameterfv := nil; 
  glGetColorTable := nil; 
  glGetColorTableParameteriv := nil; 
  glGetColorTableParameterfv := nil; 
  glConvolutionFilter1D := nil; 
  glConvolutionFilter2D := nil; 
  glCopyConvolutionFilter1D := nil; 
  glCopyConvolutionFilter2D := nil; 
  glGetConvolutionFilter := nil; 
  glSeparableFilter2D := nil; 
  glGetSeparableFilter := nil; 
  glConvolutionParameteri := nil; 
  glConvolutionParameteriv := nil; 
  glConvolutionParameterf := nil; 
  glConvolutionParameterfv := nil; 
  glGetConvolutionParameteriv := nil; 
  glGetConvolutionParameterfv := nil; 
  glHistogram := nil; 
  glResetHistogram := nil; 
  glGetHistogram := nil; 
  glGetHistogramParameteriv := nil; 
  glGetHistogramParameterfv := nil; 
  glMinmax := nil;
  glResetMinmax := nil; 
  glGetMinmax := nil; 
  glGetMinmaxParameteriv := nil; 
  glGetMinmaxParameterfv := nil; 

  // GLX
  {$ifdef UNIX}
  glXChooseVisual := nil; 
  glXCreateContext := nil; 
  glXDestroyContext := nil; 
  glXMakeCurrent := nil; 
  glXCopyContext := nil; 
  glXSwapBuffers := nil; 
  glXCreateGLXPixmap := nil; 
  glXDestroyGLXPixmap := nil; 
  glXQueryExtension := nil; 
  glXQueryVersion := nil; 
  glXIsDirect := nil; 
  glXGetConfig := nil; 
  glXGetCurrentContext := nil; 
  glXGetCurrentDrawable := nil; 
  glXWaitGL := nil; 
  glXWaitX := nil; 
  glXUseXFont := nil; 

  // GLX 1.1 and later
  glXQueryExtensionsString := nil;
  glXQueryServerString := nil;
  glXGetClientString := nil;

  // GLX 1.2 and later
  glXGetCurrentDisplay := nil;

  // GLX 1.3 and later
  glXChooseFBConfig := nil;
  glXGetFBConfigAttrib := nil;
  glXGetFBConfigs := nil;
  glXGetVisualFromFBConfig := nil;
  glXCreateWindow := nil;
  glXDestroyWindow := nil;
  glXCreatePixmap := nil;
  glXDestroyPixmap := nil;
  glXCreatePbuffer := nil;
  glXDestroyPbuffer := nil;
  glXQueryDrawable := nil;
  glXCreateNewContext := nil;
  glXMakeContextCurrent := nil;
  glXGetCurrentReadDrawable := nil;
  glXQueryContext := nil;
  glXSelectEvent := nil;
  glXGetSelectedEvent := nil;
  glXGetVideoSyncSGI := nil;
  glXWaitVideoSyncSGI := nil;
  glXFreeContextEXT := nil;
  glXGetContextIDEXT := nil;
  glXGetCurrentDisplayEXT := nil;
  glXImportContextEXT := nil;
  glXQueryContextInfoEXT := nil; 
  glXCopySubBufferMESA := nil; 
  glXCreateGLXPixmapMESA := nil; 
  glXReleaseBuffersMESA := nil; 
  glXSet3DfxModeMESA := nil; 
  {$endif}
end; 

//----------------------------------------------------------------------------------------------------------------------

procedure LoadProcAddresses;
begin
  if GLHandle <> INVALID_MODULEHANDLE then
  begin
    glAccum := GetModuleSymbol( GLHandle, 'glAccum');
    glAlphaFunc := GetModuleSymbol( GLHandle, 'glAlphaFunc'); 
    glAreTexturesResident := GetModuleSymbol( GLHandle, 'glAreTexturesResident'); 
    glArrayElement := GetModuleSymbol( GLHandle, 'glArrayElement'); 
    glBegin := GetModuleSymbol( GLHandle, 'glBegin'); 
    glBindTexture := GetModuleSymbol( GLHandle, 'glBindTexture'); 
    glBitmap := GetModuleSymbol( GLHandle, 'glBitmap'); 
    glBlendFunc := GetModuleSymbol( GLHandle, 'glBlendFunc'); 
    glCallList := GetModuleSymbol( GLHandle, 'glCallList'); 
    glCallLists := GetModuleSymbol( GLHandle, 'glCallLists'); 
    glClear := GetModuleSymbol( GLHandle, 'glClear'); 
    glClearAccum := GetModuleSymbol( GLHandle, 'glClearAccum'); 
    glClearColor := GetModuleSymbol( GLHandle, 'glClearColor'); 
    glClearDepth := GetModuleSymbol( GLHandle, 'glClearDepth'); 
    glClearIndex := GetModuleSymbol( GLHandle, 'glClearIndex'); 
    glClearStencil := GetModuleSymbol( GLHandle, 'glClearStencil'); 
    glClipPlane := GetModuleSymbol( GLHandle, 'glClipPlane'); 
    glColor3b := GetModuleSymbol( GLHandle, 'glColor3b'); 
    glColor3bv := GetModuleSymbol( GLHandle, 'glColor3bv'); 
    glColor3d := GetModuleSymbol( GLHandle, 'glColor3d'); 
    glColor3dv := GetModuleSymbol( GLHandle, 'glColor3dv'); 
    glColor3f := GetModuleSymbol( GLHandle, 'glColor3f'); 
    glColor3fv := GetModuleSymbol( GLHandle, 'glColor3fv'); 
    glColor3i := GetModuleSymbol( GLHandle, 'glColor3i'); 
    glColor3iv := GetModuleSymbol( GLHandle, 'glColor3iv');
    glColor3s := GetModuleSymbol( GLHandle, 'glColor3s'); 
    glColor3sv := GetModuleSymbol( GLHandle, 'glColor3sv'); 
    glColor3ub := GetModuleSymbol( GLHandle, 'glColor3ub');
    glColor3ubv := GetModuleSymbol( GLHandle, 'glColor3ubv'); 
    glColor3ui := GetModuleSymbol( GLHandle, 'glColor3ui'); 
    glColor3uiv := GetModuleSymbol( GLHandle, 'glColor3uiv'); 
    glColor3us := GetModuleSymbol( GLHandle, 'glColor3us'); 
    glColor3usv := GetModuleSymbol( GLHandle, 'glColor3usv'); 
    glColor4b := GetModuleSymbol( GLHandle, 'glColor4b'); 
    glColor4bv := GetModuleSymbol( GLHandle, 'glColor4bv'); 
    glColor4d := GetModuleSymbol( GLHandle, 'glColor4d'); 
    glColor4dv := GetModuleSymbol( GLHandle, 'glColor4dv'); 
    glColor4f := GetModuleSymbol( GLHandle, 'glColor4f'); 
    glColor4fv := GetModuleSymbol( GLHandle, 'glColor4fv'); 
    glColor4i := GetModuleSymbol( GLHandle, 'glColor4i'); 
    glColor4iv := GetModuleSymbol( GLHandle, 'glColor4iv'); 
    glColor4s := GetModuleSymbol( GLHandle, 'glColor4s'); 
    glColor4sv := GetModuleSymbol( GLHandle, 'glColor4sv'); 
    glColor4ub := GetModuleSymbol( GLHandle, 'glColor4ub'); 
    glColor4ubv := GetModuleSymbol( GLHandle, 'glColor4ubv'); 
    glColor4ui := GetModuleSymbol( GLHandle, 'glColor4ui'); 
    glColor4uiv := GetModuleSymbol( GLHandle, 'glColor4uiv'); 
    glColor4us := GetModuleSymbol( GLHandle, 'glColor4us'); 
    glColor4usv := GetModuleSymbol( GLHandle, 'glColor4usv'); 
    glColorMask := GetModuleSymbol( GLHandle, 'glColorMask');
    glColorMaterial := GetModuleSymbol( GLHandle, 'glColorMaterial'); 
    glColorPointer := GetModuleSymbol( GLHandle, 'glColorPointer'); 
    glCopyPixels := GetModuleSymbol( GLHandle, 'glCopyPixels'); 
    glCopyTexImage1D := GetModuleSymbol( GLHandle, 'glCopyTexImage1D'); 
    glCopyTexImage2D := GetModuleSymbol( GLHandle, 'glCopyTexImage2D'); 
    glCopyTexSubImage1D := GetModuleSymbol( GLHandle, 'glCopyTexSubImage1D'); 
    glCopyTexSubImage2D := GetModuleSymbol( GLHandle, 'glCopyTexSubImage2D'); 
    glCullFace := GetModuleSymbol( GLHandle, 'glCullFace'); 
    glDeleteLists := GetModuleSymbol( GLHandle, 'glDeleteLists'); 
    glDeleteTextures := GetModuleSymbol( GLHandle, 'glDeleteTextures'); 
    glDepthFunc := GetModuleSymbol( GLHandle, 'glDepthFunc');
    glDepthMask := GetModuleSymbol( GLHandle, 'glDepthMask'); 
    glDepthRange := GetModuleSymbol( GLHandle, 'glDepthRange'); 
    glDisable := GetModuleSymbol( GLHandle, 'glDisable'); 
    glDisableClientState := GetModuleSymbol( GLHandle, 'glDisableClientState'); 
    glDrawArrays := GetModuleSymbol( GLHandle, 'glDrawArrays'); 
    glDrawBuffer := GetModuleSymbol( GLHandle, 'glDrawBuffer'); 
    glDrawElements := GetModuleSymbol( GLHandle, 'glDrawElements'); 
    glDrawPixels := GetModuleSymbol( GLHandle, 'glDrawPixels'); 
    glEdgeFlag := GetModuleSymbol( GLHandle, 'glEdgeFlag'); 
    glEdgeFlagPointer := GetModuleSymbol( GLHandle, 'glEdgeFlagPointer'); 
    glEdgeFlagv := GetModuleSymbol( GLHandle, 'glEdgeFlagv'); 
    glEnable := GetModuleSymbol( GLHandle, 'glEnable'); 
    glEnableClientState := GetModuleSymbol( GLHandle, 'glEnableClientState'); 
    glEnd := GetModuleSymbol( GLHandle, 'glEnd');
    glEndList := GetModuleSymbol( GLHandle, 'glEndList'); 
    glEvalCoord1d := GetModuleSymbol( GLHandle, 'glEvalCoord1d'); 
    glEvalCoord1dv := GetModuleSymbol( GLHandle, 'glEvalCoord1dv'); 
    glEvalCoord1f := GetModuleSymbol( GLHandle, 'glEvalCoord1f'); 
    glEvalCoord1fv := GetModuleSymbol( GLHandle, 'glEvalCoord1fv'); 
    glEvalCoord2d := GetModuleSymbol( GLHandle, 'glEvalCoord2d'); 
    glEvalCoord2dv := GetModuleSymbol( GLHandle, 'glEvalCoord2dv'); 
    glEvalCoord2f := GetModuleSymbol( GLHandle, 'glEvalCoord2f'); 
    glEvalCoord2fv := GetModuleSymbol( GLHandle, 'glEvalCoord2fv'); 
    glEvalMesh1 := GetModuleSymbol( GLHandle, 'glEvalMesh1'); 
    glEvalMesh2 := GetModuleSymbol( GLHandle, 'glEvalMesh2'); 
    glEvalPoint1 := GetModuleSymbol( GLHandle, 'glEvalPoint1'); 
    glEvalPoint2 := GetModuleSymbol( GLHandle, 'glEvalPoint2'); 
    glFeedbackBuffer := GetModuleSymbol( GLHandle, 'glFeedbackBuffer'); 
    glFinish := GetModuleSymbol( GLHandle, 'glFinish'); 
    glFlush := GetModuleSymbol( GLHandle, 'glFlush'); 
    glFogf := GetModuleSymbol( GLHandle, 'glFogf'); 
    glFogfv := GetModuleSymbol( GLHandle, 'glFogfv'); 
    glFogi := GetModuleSymbol( GLHandle, 'glFogi');
    glFogiv := GetModuleSymbol( GLHandle, 'glFogiv'); 
    glFrontFace := GetModuleSymbol( GLHandle, 'glFrontFace'); 
    glFrustum := GetModuleSymbol( GLHandle, 'glFrustum'); 
    glGenLists := GetModuleSymbol( GLHandle, 'glGenLists'); 
    glGenTextures := GetModuleSymbol( GLHandle, 'glGenTextures'); 
    glGetBooleanv := GetModuleSymbol( GLHandle, 'glGetBooleanv');
    glGetClipPlane := GetModuleSymbol( GLHandle, 'glGetClipPlane'); 
    glGetDoublev := GetModuleSymbol( GLHandle, 'glGetDoublev'); 
    glGetError := GetModuleSymbol( GLHandle, 'glGetError'); 
    glGetFloatv := GetModuleSymbol( GLHandle, 'glGetFloatv'); 
    glGetIntegerv := GetModuleSymbol( GLHandle, 'glGetIntegerv'); 
    glGetLightfv := GetModuleSymbol( GLHandle, 'glGetLightfv'); 
    glGetLightiv := GetModuleSymbol( GLHandle, 'glGetLightiv'); 
    glGetMapdv := GetModuleSymbol( GLHandle, 'glGetMapdv'); 
    glGetMapfv := GetModuleSymbol( GLHandle, 'glGetMapfv'); 
    glGetMapiv := GetModuleSymbol( GLHandle, 'glGetMapiv'); 
    glGetMaterialfv := GetModuleSymbol( GLHandle, 'glGetMaterialfv'); 
    glGetMaterialiv := GetModuleSymbol( GLHandle, 'glGetMaterialiv'); 
    glGetPixelMapfv := GetModuleSymbol( GLHandle, 'glGetPixelMapfv'); 
    glGetPixelMapuiv := GetModuleSymbol( GLHandle, 'glGetPixelMapuiv'); 
    glGetPixelMapusv := GetModuleSymbol( GLHandle, 'glGetPixelMapusv'); 
    glGetPointerv := GetModuleSymbol( GLHandle, 'glGetPointerv'); 
    glGetPolygonStipple := GetModuleSymbol( GLHandle, 'glGetPolygonStipple'); 
    glGetString := GetModuleSymbol( GLHandle, 'glGetString'); 
    glGetTexEnvfv := GetModuleSymbol( GLHandle, 'glGetTexEnvfv'); 
    glGetTexEnviv := GetModuleSymbol( GLHandle, 'glGetTexEnviv'); 
    glGetTexGendv := GetModuleSymbol( GLHandle, 'glGetTexGendv'); 
    glGetTexGenfv := GetModuleSymbol( GLHandle, 'glGetTexGenfv'); 
    glGetTexGeniv := GetModuleSymbol( GLHandle, 'glGetTexGeniv'); 
    glGetTexImage := GetModuleSymbol( GLHandle, 'glGetTexImage'); 
    glGetTexLevelParameterfv := GetModuleSymbol( GLHandle, 'glGetTexLevelParameterfv');
    glGetTexLevelParameteriv := GetModuleSymbol( GLHandle, 'glGetTexLevelParameteriv'); 
    glGetTexParameterfv := GetModuleSymbol( GLHandle, 'glGetTexParameterfv');
    glGetTexParameteriv := GetModuleSymbol( GLHandle, 'glGetTexParameteriv'); 
    glHint := GetModuleSymbol( GLHandle, 'glHint'); 
    glIndexMask := GetModuleSymbol( GLHandle, 'glIndexMask'); 
    glIndexPointer := GetModuleSymbol( GLHandle, 'glIndexPointer'); 
    glIndexd := GetModuleSymbol( GLHandle, 'glIndexd'); 
    glIndexdv := GetModuleSymbol( GLHandle, 'glIndexdv'); 
    glIndexf := GetModuleSymbol( GLHandle, 'glIndexf'); 
    glIndexfv := GetModuleSymbol( GLHandle, 'glIndexfv'); 
    glIndexi := GetModuleSymbol( GLHandle, 'glIndexi'); 
    glIndexiv := GetModuleSymbol( GLHandle, 'glIndexiv'); 
    glIndexs := GetModuleSymbol( GLHandle, 'glIndexs'); 
    glIndexsv := GetModuleSymbol( GLHandle, 'glIndexsv'); 
    glIndexub := GetModuleSymbol( GLHandle, 'glIndexub'); 
    glIndexubv := GetModuleSymbol( GLHandle, 'glIndexubv'); 
    glInitNames := GetModuleSymbol( GLHandle, 'glInitNames'); 
    glInterleavedArrays := GetModuleSymbol( GLHandle, 'glInterleavedArrays'); 
    glIsEnabled := GetModuleSymbol( GLHandle, 'glIsEnabled'); 
    glIsList := GetModuleSymbol( GLHandle, 'glIsList'); 
    glIsTexture := GetModuleSymbol( GLHandle, 'glIsTexture'); 
    glLightModelf := GetModuleSymbol( GLHandle, 'glLightModelf'); 
    glLightModelfv := GetModuleSymbol( GLHandle, 'glLightModelfv'); 
    glLightModeli := GetModuleSymbol( GLHandle, 'glLightModeli'); 
    glLightModeliv := GetModuleSymbol( GLHandle, 'glLightModeliv');
    glLightf := GetModuleSymbol( GLHandle, 'glLightf'); 
    glLightfv := GetModuleSymbol( GLHandle, 'glLightfv'); 
    glLighti := GetModuleSymbol( GLHandle, 'glLighti'); 
    glLightiv := GetModuleSymbol( GLHandle, 'glLightiv'); 
    glLineStipple := GetModuleSymbol( GLHandle, 'glLineStipple'); 
    glLineWidth := GetModuleSymbol( GLHandle, 'glLineWidth'); 
    glListBase := GetModuleSymbol( GLHandle, 'glListBase'); 
    glLoadIdentity := GetModuleSymbol( GLHandle, 'glLoadIdentity'); 
    glLoadMatrixd := GetModuleSymbol( GLHandle, 'glLoadMatrixd'); 
    glLoadMatrixf := GetModuleSymbol( GLHandle, 'glLoadMatrixf');
    glLoadName := GetModuleSymbol( GLHandle, 'glLoadName'); 
    glLogicOp := GetModuleSymbol( GLHandle, 'glLogicOp'); 
    glMap1d := GetModuleSymbol( GLHandle, 'glMap1d'); 
    glMap1f := GetModuleSymbol( GLHandle, 'glMap1f'); 
    glMap2d := GetModuleSymbol( GLHandle, 'glMap2d'); 
    glMap2f := GetModuleSymbol( GLHandle, 'glMap2f'); 
    glMapGrid1d := GetModuleSymbol( GLHandle, 'glMapGrid1d'); 
    glMapGrid1f := GetModuleSymbol( GLHandle, 'glMapGrid1f'); 
    glMapGrid2d := GetModuleSymbol( GLHandle, 'glMapGrid2d'); 
    glMapGrid2f := GetModuleSymbol( GLHandle, 'glMapGrid2f'); 
    glMaterialf := GetModuleSymbol( GLHandle, 'glMaterialf'); 
    glMaterialfv := GetModuleSymbol( GLHandle, 'glMaterialfv'); 
    glMateriali := GetModuleSymbol( GLHandle, 'glMateriali'); 
    glMaterialiv := GetModuleSymbol( GLHandle, 'glMaterialiv'); 
    glMatrixMode := GetModuleSymbol( GLHandle, 'glMatrixMode');
    glMultMatrixd := GetModuleSymbol( GLHandle, 'glMultMatrixd'); 
    glMultMatrixf := GetModuleSymbol( GLHandle, 'glMultMatrixf'); 
    glNewList := GetModuleSymbol( GLHandle, 'glNewList'); 
    glNormal3b := GetModuleSymbol( GLHandle, 'glNormal3b'); 
    glNormal3bv := GetModuleSymbol( GLHandle, 'glNormal3bv'); 
    glNormal3d := GetModuleSymbol( GLHandle, 'glNormal3d'); 
    glNormal3dv := GetModuleSymbol( GLHandle, 'glNormal3dv'); 
    glNormal3f := GetModuleSymbol( GLHandle, 'glNormal3f'); 
    glNormal3fv := GetModuleSymbol( GLHandle, 'glNormal3fv'); 
    glNormal3i := GetModuleSymbol( GLHandle, 'glNormal3i'); 
    glNormal3iv := GetModuleSymbol( GLHandle, 'glNormal3iv'); 
    glNormal3s := GetModuleSymbol( GLHandle, 'glNormal3s'); 
    glNormal3sv := GetModuleSymbol( GLHandle, 'glNormal3sv'); 
    glNormalPointer := GetModuleSymbol( GLHandle, 'glNormalPointer'); 
    glOrtho := GetModuleSymbol( GLHandle, 'glOrtho'); 
    glPassThrough := GetModuleSymbol( GLHandle, 'glPassThrough'); 
    glPixelMapfv := GetModuleSymbol( GLHandle, 'glPixelMapfv'); 
    glPixelMapuiv := GetModuleSymbol( GLHandle, 'glPixelMapuiv');
    glPixelMapusv := GetModuleSymbol( GLHandle, 'glPixelMapusv'); 
    glPixelStoref := GetModuleSymbol( GLHandle, 'glPixelStoref'); 
    glPixelStorei := GetModuleSymbol( GLHandle, 'glPixelStorei'); 
    glPixelTransferf := GetModuleSymbol( GLHandle, 'glPixelTransferf'); 
    glPixelTransferi := GetModuleSymbol( GLHandle, 'glPixelTransferi'); 
    glPixelZoom := GetModuleSymbol( GLHandle, 'glPixelZoom'); 
    glPointSize := GetModuleSymbol( GLHandle, 'glPointSize');
    glPolygonMode := GetModuleSymbol( GLHandle, 'glPolygonMode'); 
    glPolygonOffset := GetModuleSymbol( GLHandle, 'glPolygonOffset'); 
    glPolygonStipple := GetModuleSymbol( GLHandle, 'glPolygonStipple'); 
    glPopAttrib := GetModuleSymbol( GLHandle, 'glPopAttrib'); 
    glPopClientAttrib := GetModuleSymbol( GLHandle, 'glPopClientAttrib'); 
    glPopMatrix := GetModuleSymbol( GLHandle, 'glPopMatrix'); 
    glPopName := GetModuleSymbol( GLHandle, 'glPopName'); 
    glPrioritizeTextures := GetModuleSymbol( GLHandle, 'glPrioritizeTextures'); 
    glPushAttrib := GetModuleSymbol( GLHandle, 'glPushAttrib'); 
    glPushClientAttrib := GetModuleSymbol( GLHandle, 'glPushClientAttrib'); 
    glPushMatrix := GetModuleSymbol( GLHandle, 'glPushMatrix'); 
    glPushName := GetModuleSymbol( GLHandle, 'glPushName'); 
    glRasterPos2d := GetModuleSymbol( GLHandle, 'glRasterPos2d'); 
    glRasterPos2dv := GetModuleSymbol( GLHandle, 'glRasterPos2dv'); 
    glRasterPos2f := GetModuleSymbol( GLHandle, 'glRasterPos2f'); 
    glRasterPos2fv := GetModuleSymbol( GLHandle, 'glRasterPos2fv'); 
    glRasterPos2i := GetModuleSymbol( GLHandle, 'glRasterPos2i'); 
    glRasterPos2iv := GetModuleSymbol( GLHandle, 'glRasterPos2iv'); 
    glRasterPos2s := GetModuleSymbol( GLHandle, 'glRasterPos2s'); 
    glRasterPos2sv := GetModuleSymbol( GLHandle, 'glRasterPos2sv'); 
    glRasterPos3d := GetModuleSymbol( GLHandle, 'glRasterPos3d'); 
    glRasterPos3dv := GetModuleSymbol( GLHandle, 'glRasterPos3dv'); 
    glRasterPos3f := GetModuleSymbol( GLHandle, 'glRasterPos3f'); 
    glRasterPos3fv := GetModuleSymbol( GLHandle, 'glRasterPos3fv'); 
    glRasterPos3i := GetModuleSymbol( GLHandle, 'glRasterPos3i');
    glRasterPos3iv := GetModuleSymbol( GLHandle, 'glRasterPos3iv');
    glRasterPos3s := GetModuleSymbol( GLHandle, 'glRasterPos3s'); 
    glRasterPos3sv := GetModuleSymbol( GLHandle, 'glRasterPos3sv'); 
    glRasterPos4d := GetModuleSymbol( GLHandle, 'glRasterPos4d'); 
    glRasterPos4dv := GetModuleSymbol( GLHandle, 'glRasterPos4dv'); 
    glRasterPos4f := GetModuleSymbol( GLHandle, 'glRasterPos4f'); 
    glRasterPos4fv := GetModuleSymbol( GLHandle, 'glRasterPos4fv'); 
    glRasterPos4i := GetModuleSymbol( GLHandle, 'glRasterPos4i'); 
    glRasterPos4iv := GetModuleSymbol( GLHandle, 'glRasterPos4iv'); 
    glRasterPos4s := GetModuleSymbol( GLHandle, 'glRasterPos4s'); 
    glRasterPos4sv := GetModuleSymbol( GLHandle, 'glRasterPos4sv'); 
    glReadBuffer := GetModuleSymbol( GLHandle, 'glReadBuffer'); 
    glReadPixels := GetModuleSymbol( GLHandle, 'glReadPixels'); 
    glRectd := GetModuleSymbol( GLHandle, 'glRectd'); 
    glRectdv := GetModuleSymbol( GLHandle, 'glRectdv'); 
    glRectf := GetModuleSymbol( GLHandle, 'glRectf'); 
    glRectfv := GetModuleSymbol( GLHandle, 'glRectfv'); 
    glRecti := GetModuleSymbol( GLHandle, 'glRecti'); 
    glRectiv := GetModuleSymbol( GLHandle, 'glRectiv'); 
    glRects := GetModuleSymbol( GLHandle, 'glRects'); 
    glRectsv := GetModuleSymbol( GLHandle, 'glRectsv'); 
    glRenderMode := GetModuleSymbol( GLHandle, 'glRenderMode'); 
    glRotated := GetModuleSymbol( GLHandle, 'glRotated'); 
    glRotatef := GetModuleSymbol( GLHandle, 'glRotatef'); 
    glScaled := GetModuleSymbol( GLHandle, 'glScaled');
    glScalef := GetModuleSymbol( GLHandle, 'glScalef'); 
    glScissor := GetModuleSymbol( GLHandle, 'glScissor'); 
    glSelectBuffer := GetModuleSymbol( GLHandle, 'glSelectBuffer'); 
    glShadeModel := GetModuleSymbol( GLHandle, 'glShadeModel'); 
    glStencilFunc := GetModuleSymbol( GLHandle, 'glStencilFunc'); 
    glStencilMask := GetModuleSymbol( GLHandle, 'glStencilMask'); 
    glStencilOp := GetModuleSymbol( GLHandle, 'glStencilOp'); 
    glTexCoord1d := GetModuleSymbol( GLHandle, 'glTexCoord1d'); 
    glTexCoord1dv := GetModuleSymbol( GLHandle, 'glTexCoord1dv');
    glTexCoord1f := GetModuleSymbol( GLHandle, 'glTexCoord1f'); 
    glTexCoord1fv := GetModuleSymbol( GLHandle, 'glTexCoord1fv'); 
    glTexCoord1i := GetModuleSymbol( GLHandle, 'glTexCoord1i'); 
    glTexCoord1iv := GetModuleSymbol( GLHandle, 'glTexCoord1iv'); 
    glTexCoord1s := GetModuleSymbol( GLHandle, 'glTexCoord1s'); 
    glTexCoord1sv := GetModuleSymbol( GLHandle, 'glTexCoord1sv'); 
    glTexCoord2d := GetModuleSymbol( GLHandle, 'glTexCoord2d'); 
    glTexCoord2dv := GetModuleSymbol( GLHandle, 'glTexCoord2dv'); 
    glTexCoord2f := GetModuleSymbol( GLHandle, 'glTexCoord2f'); 
    glTexCoord2fv := GetModuleSymbol( GLHandle, 'glTexCoord2fv'); 
    glTexCoord2i := GetModuleSymbol( GLHandle, 'glTexCoord2i'); 
    glTexCoord2iv := GetModuleSymbol( GLHandle, 'glTexCoord2iv'); 
    glTexCoord2s := GetModuleSymbol( GLHandle, 'glTexCoord2s'); 
    glTexCoord2sv := GetModuleSymbol( GLHandle, 'glTexCoord2sv'); 
    glTexCoord3d := GetModuleSymbol( GLHandle, 'glTexCoord3d'); 
    glTexCoord3dv := GetModuleSymbol( GLHandle, 'glTexCoord3dv');
    glTexCoord3f := GetModuleSymbol( GLHandle, 'glTexCoord3f'); 
    glTexCoord3fv := GetModuleSymbol( GLHandle, 'glTexCoord3fv'); 
    glTexCoord3i := GetModuleSymbol( GLHandle, 'glTexCoord3i'); 
    glTexCoord3iv := GetModuleSymbol( GLHandle, 'glTexCoord3iv'); 
    glTexCoord3s := GetModuleSymbol( GLHandle, 'glTexCoord3s'); 
    glTexCoord3sv := GetModuleSymbol( GLHandle, 'glTexCoord3sv'); 
    glTexCoord4d := GetModuleSymbol( GLHandle, 'glTexCoord4d'); 
    glTexCoord4dv := GetModuleSymbol( GLHandle, 'glTexCoord4dv'); 
    glTexCoord4f := GetModuleSymbol( GLHandle, 'glTexCoord4f'); 
    glTexCoord4fv := GetModuleSymbol( GLHandle, 'glTexCoord4fv'); 
    glTexCoord4i := GetModuleSymbol( GLHandle, 'glTexCoord4i'); 
    glTexCoord4iv := GetModuleSymbol( GLHandle, 'glTexCoord4iv'); 
    glTexCoord4s := GetModuleSymbol( GLHandle, 'glTexCoord4s'); 
    glTexCoord4sv := GetModuleSymbol( GLHandle, 'glTexCoord4sv'); 
    glTexCoordPointer := GetModuleSymbol( GLHandle, 'glTexCoordPointer'); 
    glTexEnvf := GetModuleSymbol( GLHandle, 'glTexEnvf'); 
    glTexEnvfv := GetModuleSymbol( GLHandle, 'glTexEnvfv');
    glTexEnvi := GetModuleSymbol( GLHandle, 'glTexEnvi'); 
    glTexEnviv := GetModuleSymbol( GLHandle, 'glTexEnviv'); 
    glTexGend := GetModuleSymbol( GLHandle, 'glTexGend'); 
    glTexGendv := GetModuleSymbol( GLHandle, 'glTexGendv'); 
    glTexGenf := GetModuleSymbol( GLHandle, 'glTexGenf'); 
    glTexGenfv := GetModuleSymbol( GLHandle, 'glTexGenfv'); 
    glTexGeni := GetModuleSymbol( GLHandle, 'glTexGeni'); 
    glTexGeniv := GetModuleSymbol( GLHandle, 'glTexGeniv');
    glTexImage1D := GetModuleSymbol( GLHandle, 'glTexImage1D'); 
    glTexImage2D := GetModuleSymbol( GLHandle, 'glTexImage2D'); 
    glTexParameterf := GetModuleSymbol( GLHandle, 'glTexParameterf'); 
    glTexParameterfv := GetModuleSymbol( GLHandle, 'glTexParameterfv'); 
    glTexParameteri := GetModuleSymbol( GLHandle, 'glTexParameteri'); 
    glTexParameteriv := GetModuleSymbol( GLHandle, 'glTexParameteriv'); 
    glTexSubImage1D := GetModuleSymbol( GLHandle, 'glTexSubImage1D'); 
    glTexSubImage2D := GetModuleSymbol( GLHandle, 'glTexSubImage2D'); 
    glTranslated := GetModuleSymbol( GLHandle, 'glTranslated'); 
    glTranslatef := GetModuleSymbol( GLHandle, 'glTranslatef'); 
    glVertex2d := GetModuleSymbol( GLHandle, 'glVertex2d'); 
    glVertex2dv := GetModuleSymbol( GLHandle, 'glVertex2dv'); 
    glVertex2f := GetModuleSymbol( GLHandle, 'glVertex2f'); 
    glVertex2fv := GetModuleSymbol( GLHandle, 'glVertex2fv'); 
    glVertex2i := GetModuleSymbol( GLHandle, 'glVertex2i'); 
    glVertex2iv := GetModuleSymbol( GLHandle, 'glVertex2iv'); 
    glVertex2s := GetModuleSymbol( GLHandle, 'glVertex2s'); 
    glVertex2sv := GetModuleSymbol( GLHandle, 'glVertex2sv'); 
    glVertex3d := GetModuleSymbol( GLHandle, 'glVertex3d'); 
    glVertex3dv := GetModuleSymbol( GLHandle, 'glVertex3dv'); 
    glVertex3f := GetModuleSymbol( GLHandle, 'glVertex3f'); 
    glVertex3fv := GetModuleSymbol( GLHandle, 'glVertex3fv'); 
    glVertex3i := GetModuleSymbol( GLHandle, 'glVertex3i'); 
    glVertex3iv := GetModuleSymbol( GLHandle, 'glVertex3iv'); 
    glVertex3s := GetModuleSymbol( GLHandle, 'glVertex3s');
    glVertex3sv := GetModuleSymbol( GLHandle, 'glVertex3sv'); 
    glVertex4d := GetModuleSymbol( GLHandle, 'glVertex4d'); 
    glVertex4dv := GetModuleSymbol( GLHandle, 'glVertex4dv'); 
    glVertex4f := GetModuleSymbol( GLHandle, 'glVertex4f'); 
    glVertex4fv := GetModuleSymbol( GLHandle, 'glVertex4fv'); 
    glVertex4i := GetModuleSymbol( GLHandle, 'glVertex4i'); 
    glVertex4iv := GetModuleSymbol( GLHandle, 'glVertex4iv'); 
    glVertex4s := GetModuleSymbol( GLHandle, 'glVertex4s'); 
    glVertex4sv := GetModuleSymbol( GLHandle, 'glVertex4sv'); 
    glVertexPointer := GetModuleSymbol( GLHandle, 'glVertexPointer'); 
    glViewport := GetModuleSymbol( GLHandle, 'glViewport'); 

    // window support routines
    {$ifdef Win32}
    wglGetProcAddress := GetModuleSymbol( GLHandle, 'wglGetProcAddress'); 
    wglCopyContext := GetModuleSymbol( GLHandle, 'wglCopyContext'); 
    wglCreateContext := GetModuleSymbol( GLHandle, 'wglCreateContext'); 
    wglCreateLayerContext := GetModuleSymbol( GLHandle, 'wglCreateLayerContext'); 
    wglDeleteContext := GetModuleSymbol( GLHandle, 'wglDeleteContext'); 
    wglDescribeLayerPlane := GetModuleSymbol( GLHandle, 'wglDescribeLayerPlane'); 
    wglGetCurrentContext := GetModuleSymbol( GLHandle, 'wglGetCurrentContext'); 
    wglGetCurrentDC := GetModuleSymbol( GLHandle, 'wglGetCurrentDC'); 
    wglGetLayerPaletteEntries := GetModuleSymbol( GLHandle, 'wglGetLayerPaletteEntries'); 
    wglMakeCurrent := GetModuleSymbol( GLHandle, 'wglMakeCurrent'); 
    wglRealizeLayerPalette := GetModuleSymbol( GLHandle, 'wglRealizeLayerPalette');
    wglSetLayerPaletteEntries := GetModuleSymbol( GLHandle, 'wglSetLayerPaletteEntries'); 
    wglShareLists := GetModuleSymbol( GLHandle, 'wglShareLists'); 
    wglSwapLayerBuffers := GetModuleSymbol( GLHandle, 'wglSwapLayerBuffers'); 
    wglSwapMultipleBuffers := GetModuleSymbol( GLHandle, 'wglSwapMultipleBuffers'); 
    wglUseFontBitmapsA := GetModuleSymbol( GLHandle, 'wglUseFontBitmapsA'); 
    wglUseFontOutlinesA := GetModuleSymbol( GLHandle, 'wglUseFontOutlinesA'); 
    wglUseFontBitmapsW := GetModuleSymbol( GLHandle, 'wglUseFontBitmapsW'); 
    wglUseFontOutlinesW := GetModuleSymbol( GLHandle, 'wglUseFontOutlinesW');
    wglUseFontBitmaps := GetModuleSymbol( GLHandle, 'wglUseFontBitmapsA'); 
    wglUseFontOutlines := GetModuleSymbol( GLHandle, 'wglUseFontOutlinesA');
    {$endif}

    // GL 1.2
    glDrawRangeElements := GetModuleSymbol( GLHandle, 'glDrawRangeElements'); 
    glTexImage3D := GetModuleSymbol( GLHandle, 'glTexImage3D'); 

    // GL 1.2 ARB imaging
    glBlendColor := GetModuleSymbol( GLHandle, 'glBlendColor'); 
    glBlendEquation := GetModuleSymbol( GLHandle, 'glBlendEquation'); 
    glColorSubTable := GetModuleSymbol( GLHandle, 'glColorSubTable'); 
    glCopyColorSubTable := GetModuleSymbol( GLHandle, 'glCopyColorSubTable'); 
    glColorTable := GetModuleSymbol( GLHandle, 'glCopyColorSubTable'); 
    glCopyColorTable := GetModuleSymbol( GLHandle, 'glCopyColorTable'); 
    glColorTableParameteriv := GetModuleSymbol( GLHandle, 'glColorTableParameteriv'); 
    glColorTableParameterfv := GetModuleSymbol( GLHandle, 'glColorTableParameterfv');
    glGetColorTable := GetModuleSymbol( GLHandle, 'glGetColorTable'); 
    glGetColorTableParameteriv := GetModuleSymbol( GLHandle, 'glGetColorTableParameteriv'); 
    glGetColorTableParameterfv := GetModuleSymbol( GLHandle, 'glGetColorTableParameterfv'); 
    glConvolutionFilter1D := GetModuleSymbol( GLHandle, 'glConvolutionFilter1D'); 
    glConvolutionFilter2D := GetModuleSymbol( GLHandle, 'glConvolutionFilter2D'); 
    glCopyConvolutionFilter1D := GetModuleSymbol( GLHandle, 'glCopyConvolutionFilter1D'); 
    glCopyConvolutionFilter2D := GetModuleSymbol( GLHandle, 'glCopyConvolutionFilter2D'); 
    glGetConvolutionFilter := GetModuleSymbol( GLHandle, 'glGetConvolutionFilter'); 
    glSeparableFilter2D := GetModuleSymbol( GLHandle, 'glSeparableFilter2D'); 
    glGetSeparableFilter := GetModuleSymbol( GLHandle, 'glGetSeparableFilter'); 
    glConvolutionParameteri := GetModuleSymbol( GLHandle, 'glConvolutionParameteri'); 
    glConvolutionParameteriv := GetModuleSymbol( GLHandle, 'glConvolutionParameteriv'); 
    glConvolutionParameterf := GetModuleSymbol( GLHandle, 'glConvolutionParameterf'); 
    glConvolutionParameterfv := GetModuleSymbol( GLHandle, 'glConvolutionParameterfv'); 
    glGetConvolutionParameteriv := GetModuleSymbol( GLHandle, 'glGetConvolutionParameteriv'); 
    glGetConvolutionParameterfv := GetModuleSymbol( GLHandle, 'glGetConvolutionParameterfv');
    glHistogram := GetModuleSymbol( GLHandle, 'glHistogram'); 
    glResetHistogram := GetModuleSymbol( GLHandle, 'glResetHistogram');
    glGetHistogram := GetModuleSymbol( GLHandle, 'glGetHistogram');
    glGetHistogramParameteriv := GetModuleSymbol( GLHandle, 'glGetHistogramParameteriv'); 
    glGetHistogramParameterfv := GetModuleSymbol( GLHandle, 'glGetHistogramParameterfv'); 
    glMinmax := GetModuleSymbol( GLHandle, 'glMinmax'); 
    glResetMinmax := GetModuleSymbol( GLHandle, 'glResetMinmax'); 
    glGetMinmax := GetModuleSymbol( GLHandle, 'glGetMinmax'); 
    glGetMinmaxParameteriv := GetModuleSymbol( GLHandle, 'glGetMinmaxParameteriv');
    glGetMinmaxParameterfv := GetModuleSymbol( GLHandle, 'glGetMinmaxParameterfv'); 

    {$ifdef UNIX}
    glXChooseVisual := GetModuleSymbol( GLHandle, 'glXChooseVisual'); 
    glXCreateContext := GetModuleSymbol( GLHandle, 'glXCreateContext'); 
    glXDestroyContext := GetModuleSymbol( GLHandle, 'glXDestroyContext'); 
    glXMakeCurrent := GetModuleSymbol( GLHandle, 'glXMakeCurrent'); 
    glXCopyContext := GetModuleSymbol( GLHandle, 'glXCopyContext'); 
    glXSwapBuffers := GetModuleSymbol( GLHandle, 'glXSwapBuffers'); 
    glXCreateGLXPixmap := GetModuleSymbol( GLHandle, 'glXCreateGLXPixmap'); 
    glXDestroyGLXPixmap := GetModuleSymbol( GLHandle, 'glXDestroyGLXPixmap'); 
    glXQueryExtension := GetModuleSymbol( GLHandle, 'glXQueryExtension'); 
    glXQueryVersion := GetModuleSymbol( GLHandle, 'glXQueryVersion'); 
    glXIsDirect := GetModuleSymbol( GLHandle, 'glXIsDirect'); 
    glXGetConfig := GetModuleSymbol( GLHandle, 'glXGetConfig'); 
    glXGetCurrentContext := GetModuleSymbol( GLHandle, 'glXGetCurrentContext'); 
    glXGetCurrentDrawable := GetModuleSymbol( GLHandle, 'glXGetCurrentDrawable'); 
    glXWaitGL := GetModuleSymbol( GLHandle, 'glXWaitGL'); 
    glXWaitX := GetModuleSymbol( GLHandle, 'glXWaitX'); 
    glXUseXFont := GetModuleSymbol( GLHandle, 'glXUseXFont'); 
    glXQueryExtensionsString := GetModuleSymbol( GLHandle, 'glXQueryExtensionsString'); 
    glXQueryServerString := GetModuleSymbol( GLHandle, 'glXQueryServerString'); 
    glXGetClientString := GetModuleSymbol( GLHandle, 'glXGetClientString'); 
    glXGetCurrentDisplay := GetModuleSymbol( GLHandle, 'glXGetCurrentDisplay');
    glXChooseFBConfig := GetModuleSymbol( GLHandle, 'glXChooseFBConfig');
    glXGetFBConfigAttrib := GetModuleSymbol( GLHandle, 'glXGetFBConfigAttrib'); 
    glXGetFBConfigs := GetModuleSymbol( GLHandle, 'glXGetFBConfigs'); 
    glXGetVisualFromFBConfig := GetModuleSymbol( GLHandle, 'glXGetVisualFromFBConfig'); 
    glXCreateWindow := GetModuleSymbol( GLHandle, 'glXCreateWindow'); 
    glXDestroyWindow := GetModuleSymbol( GLHandle, 'glXDestroyWindow'); 
    glXCreatePixmap := GetModuleSymbol( GLHandle, 'glXCreatePixmap'); 
    glXDestroyPixmap := GetModuleSymbol( GLHandle, 'glXDestroyPixmap'); 
    glXCreatePbuffer := GetModuleSymbol( GLHandle, 'glXCreatePbuffer'); 
    glXDestroyPbuffer := GetModuleSymbol( GLHandle, 'glXDestroyPbuffer'); 
    glXQueryDrawable := GetModuleSymbol( GLHandle, 'glXQueryDrawable'); 
    glXCreateNewContext := GetModuleSymbol( GLHandle, 'glXCreateNewContext'); 
    glXMakeContextCurrent := GetModuleSymbol( GLHandle, 'glXMakeContextCurrent'); 
    glXGetCurrentReadDrawable := GetModuleSymbol( GLHandle, 'glXGetCurrentReadDrawable'); 
    glXQueryContext := GetModuleSymbol( GLHandle, 'glXQueryContext'); 
    glXSelectEvent := GetModuleSymbol( GLHandle, 'glXSelectEvent'); 
    glXGetSelectedEvent := GetModuleSymbol( GLHandle, 'glXGetSelectedEvent'); 
    glXGetVideoSyncSGI := GetModuleSymbol( GLHandle, 'glXGetVideoSyncSGI'); 
    glXWaitVideoSyncSGI := GetModuleSymbol( GLHandle, 'glXWaitVideoSyncSGI'); 
    glXFreeContextEXT := GetModuleSymbol( GLHandle, 'glXFreeContextEXT'); 
    glXGetContextIDEXT := GetModuleSymbol( GLHandle, 'glXGetContextIDEXT'); 
    glXGetCurrentDisplayEXT := GetModuleSymbol( GLHandle, 'glXGetCurrentDisplayEXT'); 
    glXImportContextEXT := GetModuleSymbol( GLHandle, 'glXImportContextEXT'); 
    glXQueryContextInfoEXT := GetModuleSymbol( GLHandle, 'glXQueryContextInfoEXT'); 
    glXCopySubBufferMESA := GetModuleSymbol( GLHandle, 'glXCopySubBufferMESA'); 
    glXCreateGLXPixmapMESA := GetModuleSymbol( GLHandle, 'glXCreateGLXPixmapMESA');
    glXReleaseBuffersMESA := GetModuleSymbol( GLHandle, 'glXReleaseBuffersMESA'); 
    glXSet3DfxModeMESA := GetModuleSymbol( GLHandle, 'glXSet3DfxModeMESA'); 
    {$endif}
  end; 

  if GLUHandle <> INVALID_MODULEHANDLE then
  begin
    GLHandle := TModuleHandle(GLUHandle); // Kylix compatiblilty trick

    gluBeginCurve := GetModuleSymbol( GLUHandle, 'gluBeginCurve');
    gluBeginPolygon := GetModuleSymbol( GLUHandle, 'gluBeginPolygon');
    gluBeginSurface := GetModuleSymbol( GLUHandle, 'gluBeginSurface');
    gluBeginTrim := GetModuleSymbol( GLUHandle, 'gluBeginTrim');
    gluBuild1DMipmaps := GetModuleSymbol( GLUHandle, 'gluBuild1DMipmaps');
    gluBuild2DMipmaps := GetModuleSymbol( GLUHandle, 'gluBuild2DMipmaps');
    gluCylinder := GetModuleSymbol( GLUHandle, 'gluCylinder');
    gluDeleteNurbsRenderer := GetModuleSymbol( GLUHandle, 'gluDeleteNurbsRenderer');
    gluDeleteQuadric := GetModuleSymbol( GLUHandle, 'gluDeleteQuadric');
    gluDeleteTess := GetModuleSymbol( GLUHandle, 'gluDeleteTess');
    gluDisk := GetModuleSymbol( GLUHandle, 'gluDisk');
    gluEndCurve := GetModuleSymbol( GLUHandle, 'gluEndCurve');
    gluEndPolygon := GetModuleSymbol( GLUHandle, 'gluEndPolygon');
    gluEndSurface := GetModuleSymbol( GLUHandle, 'gluEndSurface');
    gluEndTrim := GetModuleSymbol( GLUHandle, 'gluEndTrim');
    gluErrorString := GetModuleSymbol( GLUHandle, 'gluErrorString');
    gluGetNurbsProperty := GetModuleSymbol( GLUHandle, 'gluGetNurbsProperty');
    gluGetString := GetModuleSymbol( GLUHandle, 'gluGetString');
    gluGetTessProperty := GetModuleSymbol( GLUHandle, 'gluGetTessProperty');
    gluLoadSamplingMatrices := GetModuleSymbol( GLUHandle, 'gluLoadSamplingMatrices');
    gluLookAt := GetModuleSymbol( GLUHandle, 'gluLookAt');
    gluNewNurbsRenderer := GetModuleSymbol( GLUHandle, 'gluNewNurbsRenderer');
    gluNewQuadric := GetModuleSymbol( GLUHandle, 'gluNewQuadric');
    gluNewTess := GetModuleSymbol( GLUHandle, 'gluNewTess');
    gluNextContour := GetModuleSymbol( GLUHandle, 'gluNextContour');
    gluNurbsCallback := GetModuleSymbol( GLUHandle, 'gluNurbsCallback');
    gluNurbsCurve := GetModuleSymbol( GLUHandle, 'gluNurbsCurve');
    gluNurbsProperty := GetModuleSymbol( GLUHandle, 'gluNurbsProperty');
    gluNurbsSurface := GetModuleSymbol( GLUHandle, 'gluNurbsSurface');
    gluOrtho2D := GetModuleSymbol( GLUHandle, 'gluOrtho2D');
    gluPartialDisk := GetModuleSymbol( GLUHandle, 'gluPartialDisk');
    gluPerspective := GetModuleSymbol( GLUHandle, 'gluPerspective');
    gluPickMatrix := GetModuleSymbol( GLUHandle, 'gluPickMatrix');
    gluProject := GetModuleSymbol( GLUHandle, 'gluProject');
    gluPwlCurve := GetModuleSymbol( GLUHandle, 'gluPwlCurve');
    gluQuadricCallback := GetModuleSymbol( GLUHandle, 'gluQuadricCallback');
    gluQuadricDrawStyle := GetModuleSymbol( GLUHandle, 'gluQuadricDrawStyle');
    gluQuadricNormals := GetModuleSymbol( GLUHandle, 'gluQuadricNormals');
    gluQuadricOrientation := GetModuleSymbol( GLUHandle, 'gluQuadricOrientation');
    gluQuadricTexture := GetModuleSymbol( GLUHandle, 'gluQuadricTexture');
    gluScaleImage := GetModuleSymbol( GLUHandle, 'gluScaleImage');
    gluSphere := GetModuleSymbol( GLUHandle, 'gluSphere');
    gluTessBeginContour := GetModuleSymbol( GLUHandle, 'gluTessBeginContour');
    gluTessBeginPolygon := GetModuleSymbol( GLUHandle, 'gluTessBeginPolygon');
    gluTessCallback := GetModuleSymbol( GLUHandle, 'gluTessCallback');
    gluTessEndContour := GetModuleSymbol( GLUHandle, 'gluTessEndContour');
    gluTessEndPolygon := GetModuleSymbol( GLUHandle, 'gluTessEndPolygon');
    gluTessNormal := GetModuleSymbol( GLUHandle, 'gluTessNormal');
    gluTessProperty := GetModuleSymbol( GLUHandle, 'gluTessProperty');
    gluTessVertex := GetModuleSymbol( GLUHandle, 'gluTessVertex');
    gluUnProject := GetModuleSymbol( GLUHandle, 'gluUnProject'); 
  end; 
end; 

//----------------------------------------------------------------------------------------------------------------------

procedure ClearExtensions; 

begin
  glArrayElementEXT := nil;
  glDrawArraysEXT := nil;
  glVertexPointerEXT := nil;
  glNormalPointerEXT := nil;
  glColorPointerEXT := nil;
  glIndexPointerEXT := nil;
  glTexCoordPointerEXT := nil; 
  glEdgeFlagPointerEXT := nil;
  glGetPointervEXT := nil; 
  glArrayElementArrayEXT := nil; 
  glAddSwapHintRectWIN := nil; 
  glColorTableEXT := nil; 
  glColorSubTableEXT := nil; 
  glGetColorTableEXT := nil; 
  glGetColorTablePameterivEXT := nil; 
  glGetColorTablePameterfvEXT := nil; 
  gluNurbsCallbackDataEXT := nil; 
  gluNewNurbsTessellatorEXT := nil; 
  gluDeleteNurbsTessellatorEXT := nil; 
  glLockArraysEXT := nil; 
  glUnlockArraysEXT := nil; 
  glCopyTexImage1DEXT := nil; 
  glCopyTexSubImage1DEXT := nil; 
  glCopyTexImage2DEXT := nil; 
  glCopyTexSubImage2DEXT := nil; 
  glCopyTexSubImage3DEXT := nil; 
  glCullParameterfvEXT := nil; 
  glCullParameterdvEXT := nil; 
  glIndexFuncEXT := nil; 
  glIndexMaterialEXT := nil; 
  glPolygonOffsetEXT := nil; 
  glTexSubImage1DEXT := nil; 
  glTexSubImage2DEXT := nil;
  glTexSubImage3DEXT := nil; 
  glGenTexturesEXT := nil; 
  glDeleteTexturesEXT := nil; 
  glBindTextureEXT := nil; 
  glPrioritizeTexturesEXT := nil; 
  glAreTexturesResidentEXT := nil;
  glIsTextureEXT := nil; 

  glMultiTexCoord1dARB := nil; 
  glMultiTexCoord1dVARB := nil; 
  glMultiTexCoord1fARBP := nil; 
  glMultiTexCoord1fVARB := nil; 
  glMultiTexCoord1iARB := nil; 
  glMultiTexCoord1iVARB := nil; 
  glMultiTexCoord1sARBP := nil; 
  glMultiTexCoord1sVARB := nil; 
  glMultiTexCoord2dARB := nil; 
  glMultiTexCoord2dvARB := nil; 
  glMultiTexCoord2fARB := nil; 
  glMultiTexCoord2fvARB := nil; 
  glMultiTexCoord2iARB := nil; 
  glMultiTexCoord2ivARB := nil; 
  glMultiTexCoord2sARB := nil; 
  glMultiTexCoord2svARB := nil; 
  glMultiTexCoord3dARB := nil;
  glMultiTexCoord3dvARB := nil; 
  glMultiTexCoord3fARB := nil; 
  glMultiTexCoord3fvARB := nil; 
  glMultiTexCoord3iARB := nil; 
  glMultiTexCoord3ivARB := nil; 
  glMultiTexCoord3sARB := nil; 
  glMultiTexCoord3svARB := nil; 
  glMultiTexCoord4dARB := nil;
  glMultiTexCoord4dvARB := nil; 
  glMultiTexCoord4fARB := nil; 
  glMultiTexCoord4fvARB := nil; 
  glMultiTexCoord4iARB := nil; 
  glMultiTexCoord4ivARB := nil; 
  glMultiTexCoord4sARB := nil; 
  glMultiTexCoord4svARB := nil; 
  glActiveTextureARB := nil; 
  glClientActiveTextureARB := nil; 

  // EXT_compiled_vertex_array
  glLockArrayEXT := nil; 
  glUnlockArrayEXT := nil; 

  // EXT_cull_vertex
  glCullParameterdvEXT := nil; 
  glCullParameterfvEXT := nil;

  // WIN_swap_hint
  glAddSwapHintRectWIN := nil; 

  // EXT_point_parameter
  glPointParameterfEXT := nil; 
  glPointParameterfvEXT := nil; 

  // GL_ARB_transpose_matrix
  glLoadTransposeMatrixfARB := nil; 
  glLoadTransposeMatrixdARB := nil; 
  glMultTransposeMatrixfARB := nil; 
  glMultTransposeMatrixdARB := nil; 

  glSampleCoverageARB := nil; 
  glSamplePassARB := nil;

  // GL_ARB_multisample
  glCompressedTexImage3DARB := nil; 
  glCompressedTexImage2DARB := nil; 
  glCompressedTexImage1DARB := nil; 
  glCompressedTexSubImage3DARB := nil; 
  glCompressedTexSubImage2DARB := nil; 
  glCompressedTexSubImage1DARB := nil; 
  glGetCompressedTexImageARB := nil;

  // GL_EXT_blend_color
  glBlendColorEXT := nil; 

  // GL_EXT_texture3D
  glTexImage3DEXT := nil; 

  // GL_SGIS_texture_filter4
  glGetTexFilterFuncSGIS := nil; 
  glTexFilterFuncSGIS := nil; 

  // GL_EXT_histogram
  glGetHistogramEXT := nil; 
  glGetHistogramParameterfvEXT := nil; 
  glGetHistogramParameterivEXT := nil; 
  glGetMinmaxEXT := nil; 
  glGetMinmaxParameterfvEXT := nil; 
  glGetMinmaxParameterivEXT := nil; 
  glHistogramEXT := nil; 
  glMinmaxEXT := nil; 
  glResetHistogramEXT := nil; 
  glResetMinmaxEXT := nil; 

  // GL_EXT_convolution
  glConvolutionFilter1DEXT := nil;
  glConvolutionFilter2DEXT := nil; 
  glConvolutionParameterfEXT := nil; 
  glConvolutionParameterfvEXT := nil; 
  glConvolutionParameteriEXT := nil; 
  glConvolutionParameterivEXT := nil; 
  glCopyConvolutionFilter1DEXT := nil; 
  glCopyConvolutionFilter2DEXT := nil; 
  glGetConvolutionFilterEXT := nil; 
  glGetConvolutionParameterfvEXT := nil; 
  glGetConvolutionParameterivEXT := nil; 
  glGetSeparableFilterEXT := nil; 
  glSeparableFilter2DEXT := nil; 

  // GL_SGI_color_table
  glColorTableSGI := nil; 
  glColorTableParameterfvSGI := nil; 
  glColorTableParameterivSGI := nil; 
  glCopyColorTableSGI := nil; 
  glGetColorTableSGI := nil; 
  glGetColorTableParameterfvSGI := nil; 
  glGetColorTableParameterivSGI := nil; 

  // GL_SGIX_pixel_texture
  glPixelTexGenSGIX := nil; 

  // GL_SGIS_pixel_texture
  glPixelTexGenParameteriSGIS := nil; 
  glPixelTexGenParameterivSGIS := nil; 
  glPixelTexGenParameterfSGIS := nil; 
  glPixelTexGenParameterfvSGIS := nil; 
  glGetPixelTexGenParameterivSGIS := nil; 
  glGetPixelTexGenParameterfvSGIS := nil;

  // GL_SGIS_texture4D
  glTexImage4DSGIS := nil; 
  glTexSubImage4DSGIS := nil; 

  // GL_SGIS_detail_texture
  glDetailTexFuncSGIS := nil; 
  glGetDetailTexFuncSGIS := nil; 

  // GL_SGIS_sharpen_texture
  glSharpenTexFuncSGIS := nil; 
  glGetSharpenTexFuncSGIS := nil; 

  // GL_SGIS_multisample
  glSampleMaskSGIS := nil; 
  glSamplePatternSGIS := nil; 

  // GL_EXT_blend_minmax
  glBlendEquationEXT := nil; 

  // GL_SGIX_sprite
  glSpriteParameterfSGIX := nil; 
  glSpriteParameterfvSGIX := nil; 
  glSpriteParameteriSGIX := nil; 
  glSpriteParameterivSGIX := nil; 

  // GL_EXT_point_parameters
  glPointParameterfSGIS := nil; 
  glPointParameterfvSGIS := nil; 

  // GL_SGIX_instruments
  glGetInstrumentsSGIX := nil; 
  glInstrumentsBufferSGIX := nil;
  glPollInstrumentsSGIX := nil; 
  glReadInstrumentsSGIX := nil; 
  glStartInstrumentsSGIX := nil; 
  glStopInstrumentsSGIX := nil; 

  // GL_SGIX_framezoom
  glFrameZoomSGIX := nil; 

  // GL_SGIX_tag_sample_buffer
  glTagSampleBufferSGIX := nil;

  // GL_SGIX_polynomial_ffd
  glDeformationMap3dSGIX := nil; 
  glDeformationMap3fSGIX := nil; 
  glDeformSGIX := nil; 
  glLoadIdentityDeformationMapSGIX := nil; 

  // GL_SGIX_reference_plane
  glReferencePlaneSGIX := nil; 

  // GL_SGIX_flush_raster
  glFlushRasterSGIX := nil; 

  // GL_SGIS_fog_function
  glFogFuncSGIS := nil; 
  glGetFogFuncSGIS := nil; 

  // GL_HP_image_transform
  glImageTransformParameteriHP := nil; 
  glImageTransformParameterfHP := nil; 
  glImageTransformParameterivHP := nil; 
  glImageTransformParameterfvHP := nil; 
  glGetImageTransformParameterivHP := nil;
  glGetImageTransformParameterfvHP := nil; 

  // GL_EXT_color_subtable
  glCopyColorSubTableEXT := nil; 

  // GL_PGI_misc_hints
  glHintPGI := nil; 

  // GL_EXT_paletted_texture
  glGetColorTableParameterivEXT := nil; 
  glGetColorTableParameterfvEXT := nil; 

  // GL_SGIX_list_priority
  glGetListParameterfvSGIX := nil; 
  glGetListParameterivSGIX := nil; 
  glListParameterfSGIX := nil; 
  glListParameterfvSGIX := nil; 
  glListParameteriSGIX := nil; 
  glListParameterivSGIX := nil; 

  // GL_SGIX_fragment_lighting
  glFragmentColorMaterialSGIX := nil; 
  glFragmentLightfSGIX := nil; 
  glFragmentLightfvSGIX := nil; 
  glFragmentLightiSGIX := nil; 
  glFragmentLightivSGIX := nil; 
  glFragmentLightModelfSGIX := nil;
  glFragmentLightModelfvSGIX := nil; 
  glFragmentLightModeliSGIX := nil; 
  glFragmentLightModelivSGIX := nil; 
  glFragmentMaterialfSGIX := nil; 
  glFragmentMaterialfvSGIX := nil; 
  glFragmentMaterialiSGIX := nil;
  glFragmentMaterialivSGIX := nil; 
  glGetFragmentLightfvSGIX := nil; 
  glGetFragmentLightivSGIX := nil; 
  glGetFragmentMaterialfvSGIX := nil; 
  glGetFragmentMaterialivSGIX := nil; 
  glLightEnviSGIX := nil; 

  // GL_EXT_draw_range_elements
  glDrawRangeElementsEXT := nil; 

  // GL_EXT_light_texture
  glApplyTextureEXT := nil; 
  glTextureLightEXT := nil; 
  glTextureMaterialEXT := nil; 

  // GL_SGIX_async
  glAsyncMarkerSGIX := nil; 
  glFinishAsyncSGIX := nil; 
  glPollAsyncSGIX := nil;
  glGenAsyncMarkersSGIX := nil; 
  glDeleteAsyncMarkersSGIX := nil; 
  glIsAsyncMarkerSGIX := nil; 

  // GL_INTEL_parallel_arrays
  glVertexPointervINTEL := nil; 
  glNormalPointervINTEL := nil; 
  glColorPointervINTEL := nil; 
  glTexCoordPointervINTEL := nil; 

  // GL_EXT_pixel_transform
  glPixelTransformParameteriEXT := nil; 
  glPixelTransformParameterfEXT := nil; 
  glPixelTransformParameterivEXT := nil;
  glPixelTransformParameterfvEXT := nil; 

  // GL_EXT_secondary_color
  glSecondaryColor3bEXT := nil; 
  glSecondaryColor3bvEXT := nil; 
  glSecondaryColor3dEXT := nil; 
  glSecondaryColor3dvEXT := nil; 
  glSecondaryColor3fEXT := nil; 
  glSecondaryColor3fvEXT := nil; 
  glSecondaryColor3iEXT := nil; 
  glSecondaryColor3ivEXT := nil;
  glSecondaryColor3sEXT := nil; 
  glSecondaryColor3svEXT := nil; 
  glSecondaryColor3ubEXT := nil; 
  glSecondaryColor3ubvEXT := nil; 
  glSecondaryColor3uiEXT := nil; 
  glSecondaryColor3uivEXT := nil; 
  glSecondaryColor3usEXT := nil; 
  glSecondaryColor3usvEXT := nil; 
  glSecondaryColorPointerEXT := nil; 

  // GL_EXT_texture_perturb_normal
  glTextureNormalEXT := nil; 

  // GL_EXT_multi_draw_arrays
  glMultiDrawArraysEXT := nil; 
  glMultiDrawElementsEXT := nil; 

  // GL_EXT_fog_coord
  glFogCoordfEXT := nil; 
  glFogCoordfvEXT := nil; 
  glFogCoorddEXT := nil; 
  glFogCoorddvEXT := nil;
  glFogCoordPointerEXT := nil; 

  // GL_EXT_coordinate_frame
  glTangent3bEXT := nil; 
  glTangent3bvEXT := nil; 
  glTangent3dEXT := nil; 
  glTangent3dvEXT := nil; 
  glTangent3fEXT := nil; 
  glTangent3fvEXT := nil; 
  glTangent3iEXT := nil; 
  glTangent3ivEXT := nil; 
  glTangent3sEXT := nil; 
  glTangent3svEXT := nil; 
  glBinormal3bEXT := nil; 
  glBinormal3bvEXT := nil; 
  glBinormal3dEXT := nil; 
  glBinormal3dvEXT := nil; 
  glBinormal3fEXT := nil; 
  glBinormal3fvEXT := nil; 
  glBinormal3iEXT := nil; 
  glBinormal3ivEXT := nil; 
  glBinormal3sEXT := nil; 
  glBinormal3svEXT := nil; 
  glTangentPointerEXT := nil; 
  glBinormalPointerEXT := nil; 

  // GL_SUNX_constant_data
  glFinishTextureSUNX := nil;
  
  // GL_SUN_global_alpha
  glGlobalAlphaFactorbSUN := nil; 
  glGlobalAlphaFactorsSUN := nil; 
  glGlobalAlphaFactoriSUN := nil;
  glGlobalAlphaFactorfSUN := nil; 
  glGlobalAlphaFactordSUN := nil; 
  glGlobalAlphaFactorubSUN := nil; 
  glGlobalAlphaFactorusSUN := nil; 
  glGlobalAlphaFactoruiSUN := nil; 

  // GL_SUN_triangle_list
  glReplacementCodeuiSUN := nil; 
  glReplacementCodeusSUN := nil; 
  glReplacementCodeubSUN := nil; 
  glReplacementCodeuivSUN := nil; 
  glReplacementCodeusvSUN := nil; 
  glReplacementCodeubvSUN := nil; 
  glReplacementCodePointerSUN := nil; 

  // GL_SUN_vertex
  glColor4ubVertex2fSUN := nil; 
  glColor4ubVertex2fvSUN := nil; 
  glColor4ubVertex3fSUN := nil; 
  glColor4ubVertex3fvSUN := nil;
  glColor3fVertex3fSUN := nil; 
  glColor3fVertex3fvSUN := nil; 
  glNormal3fVertex3fSUN := nil; 
  glNormal3fVertex3fvSUN := nil; 
  glColor4fNormal3fVertex3fSUN := nil; 
  glColor4fNormal3fVertex3fvSUN := nil; 
  glTexCoord2fVertex3fSUN := nil; 
  glTexCoord2fVertex3fvSUN := nil; 
  glTexCoord4fVertex4fSUN := nil; 
  glTexCoord4fVertex4fvSUN := nil; 
  glTexCoord2fColor4ubVertex3fSUN := nil; 
  glTexCoord2fColor4ubVertex3fvSUN := nil; 
  glTexCoord2fColor3fVertex3fSUN := nil;
  glTexCoord2fColor3fVertex3fvSUN := nil; 
  glTexCoord2fNormal3fVertex3fSUN := nil; 
  glTexCoord2fNormal3fVertex3fvSUN := nil; 
  glTexCoord2fColor4fNormal3fVertex3fSUN := nil; 
  glTexCoord2fColor4fNormal3fVertex3fvSUN := nil; 
  glTexCoord4fColor4fNormal3fVertex4fSUN := nil; 
  glTexCoord4fColor4fNormal3fVertex4fvSUN := nil; 
  glReplacementCodeuiVertex3fSUN := nil; 
  glReplacementCodeuiVertex3fvSUN := nil; 
  glReplacementCodeuiColor4ubVertex3fSUN := nil; 
  glReplacementCodeuiColor4ubVertex3fvSUN := nil; 
  glReplacementCodeuiColor3fVertex3fSUN := nil;
  glReplacementCodeuiColor3fVertex3fvSUN := nil; 
  glReplacementCodeuiNormal3fVertex3fSUN := nil; 
  glReplacementCodeuiNormal3fVertex3fvSUN := nil; 
  glReplacementCodeuiColor4fNormal3fVertex3fSUN := nil; 
  glReplacementCodeuiColor4fNormal3fVertex3fvSUN := nil; 
  glReplacementCodeuiTexCoord2fVertex3fSUN := nil; 
  glReplacementCodeuiTexCoord2fVertex3fvSUN := nil; 
  glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN := nil; 
  glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN := nil; 
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN := nil; 
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN := nil; 

  // GL_EXT_blend_func_separate
  glBlendFuncSeparateEXT := nil; 

  // GL_EXT_vertex_weighting
  glVertexWeightfEXT := nil; 
  glVertexWeightfvEXT := nil; 
  glVertexWeightPointerEXT := nil; 

  // GL_NV_vertex_array_range
  glFlushVertexArrayRangeNV := nil; 
  glVertexArrayRangeNV := nil; 
  wglAllocateMemoryNV := nil; 
  wglFreeMemoryNV := nil;

  // GL_NV_register_combiners
  glCombinerParameterfvNV := nil; 
  glCombinerParameterfNV := nil; 
  glCombinerParameterivNV := nil; 
  glCombinerParameteriNV := nil; 
  glCombinerInputNV := nil; 
  glCombinerOutputNV := nil; 
  glFinalCombinerInputNV := nil; 
  glGetCombinerInputParameterfvNV := nil; 
  glGetCombinerInputParameterivNV := nil; 
  glGetCombinerOutputParameterfvNV := nil; 
  glGetCombinerOutputParameterivNV := nil; 
  glGetFinalCombinerInputParameterfvNV := nil; 
  glGetFinalCombinerInputParameterivNV := nil; 

  // GL_MESA_resize_buffers
  glResizeBuffersMESA := nil; 

  // GL_MESA_window_pos
  glWindowPos2dMESA := nil; 
  glWindowPos2dvMESA := nil; 
  glWindowPos2fMESA := nil; 
  glWindowPos2fvMESA := nil; 
  glWindowPos2iMESA := nil;
  glWindowPos2ivMESA := nil; 
  glWindowPos2sMESA := nil; 
  glWindowPos2svMESA := nil; 
  glWindowPos3dMESA := nil;
  glWindowPos3dvMESA := nil; 
  glWindowPos3fMESA := nil; 
  glWindowPos3fvMESA := nil; 
  glWindowPos3iMESA := nil; 
  glWindowPos3ivMESA := nil; 
  glWindowPos3sMESA := nil; 
  glWindowPos3svMESA := nil; 
  glWindowPos4dMESA := nil; 
  glWindowPos4dvMESA := nil; 
  glWindowPos4fMESA := nil; 
  glWindowPos4fvMESA := nil; 
  glWindowPos4iMESA := nil; 
  glWindowPos4ivMESA := nil; 
  glWindowPos4sMESA := nil; 
  glWindowPos4svMESA := nil; 

  // GL_IBM_multimode_draw_arrays
  glMultiModeDrawArraysIBM := nil; 
  glMultiModeDrawElementsIBM := nil; 

  // GL_IBM_vertex_array_lists
  glColorPointerListIBM := nil; 
  glSecondaryColorPointerListIBM := nil; 
  glEdgeFlagPointerListIBM := nil; 
  glFogCoordPointerListIBM := nil; 
  glIndexPointerListIBM := nil; 
  glNormalPointerListIBM := nil; 
  glTexCoordPointerListIBM := nil; 
  glVertexPointerListIBM := nil; 

  // GL_3DFX_tbuffer
  glTbufferMask3DFX := nil; 

  // GL_EXT_multisample
  glSampleMaskEXT := nil; 
  glSamplePatternEXT := nil; 

  // GL_SGIS_texture_color_mask
  glTextureColorMaskSGIS := nil; 

  // GL_SGIX_igloo_interface
  glIglooInterfaceSGIX := nil; 

  // GLU extensions
  gluNurbsCallbackDataEXT := nil; 
  gluNewNurbsTessellatorEXT := nil;
  gluDeleteNurbsTessellatorEXT := nil; 

  // GL_NV_vertex_program
  glAreProgramsResidentNV := nil; 
  glBindProgramNV := nil; 
  glDeleteProgramsNV := nil; 
  glExecuteProgramNV := nil; 
  glGenProgramsNV := nil; 
  glGetProgramParameterdvNV := nil; 
  glGetProgramParameterfvNV := nil; 
  glGetProgramivNV := nil; 
  glGetProgramStringNV := nil; 
  glGetTrackMatrixivNV := nil; 
  glGetVertexAttribdvNV:= nil; 
  glGetVertexAttribfvNV:= nil; 
  glGetVertexAttribivNV:= nil; 
  glGetVertexAttribPointervNV := nil; 
  glIsProgramNV := nil; 
  glLoadProgramNV := nil; 
  glProgramParameter4dNV := nil;
  glProgramParameter4dvNV := nil; 
  glProgramParameter4fNV := nil; 
  glProgramParameter4fvNV := nil; 
  glProgramParameters4dvNV := nil; 
  glProgramParameters4fvNV := nil;
  glRequestResidentProgramsNV := nil; 
  glTrackMatrixNV := nil; 
  glVertexAttribPointerNV := nil; 
  glVertexAttrib1dNV := nil; 
  glVertexAttrib1dvNV := nil; 
  glVertexAttrib1fNV := nil; 
  glVertexAttrib1fvNV := nil; 
  glVertexAttrib1sNV := nil; 
  glVertexAttrib1svNV := nil; 
  glVertexAttrib2dNV := nil; 
  glVertexAttrib2dvNV := nil; 
  glVertexAttrib2fNV := nil; 
  glVertexAttrib2fvNV := nil; 
  glVertexAttrib2sNV := nil; 
  glVertexAttrib2svNV := nil; 
  glVertexAttrib3dNV := nil; 
  glVertexAttrib3dvNV := nil; 
  glVertexAttrib3fNV := nil; 
  glVertexAttrib3fvNV := nil; 
  glVertexAttrib3sNV := nil; 
  glVertexAttrib3svNV := nil; 
  glVertexAttrib4dNV := nil; 
  glVertexAttrib4dvNV := nil; 
  glVertexAttrib4fNV := nil; 
  glVertexAttrib4fvNV := nil;
  glVertexAttrib4sNV := nil; 
  glVertexAttrib4svNV := nil; 
  glVertexAttrib4ubvNV := nil;
  glVertexAttribs1dvNV := nil;
  glVertexAttribs1fvNV := nil;
  glVertexAttribs1svNV := nil;
  glVertexAttribs2dvNV := nil;
  glVertexAttribs2fvNV := nil;
  glVertexAttribs2svNV := nil;
  glVertexAttribs3dvNV := nil;
  glVertexAttribs3fvNV := nil;
  glVertexAttribs3svNV := nil;
  glVertexAttribs4dvNV := nil;
  glVertexAttribs4fvNV := nil;
  glVertexAttribs4svNV := nil;
  glVertexAttribs4ubvNV := nil;

  LastPixelFormat := 0; // to get synchronized again, if this proc was called from outside
end;

//----------------------------------------------------------------------------------------------------------------------

{$ifdef Win32}

function HasActiveContext: Boolean;

// Returns True if the caller thread has an active (current) rendering context.

begin
  Result := ActivationRefCount > 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ReadExtensions;
begin
  if GLHandle <> INVALID_MODULEHANDLE then
  begin
    // GL extensions
    glArrayElementArrayEXT := SDL_GL_GetProcAddress( 'glArrayElementArrayEXT');
    glColorTableEXT := SDL_GL_GetProcAddress( 'glColorTableEXT');
    glColorSubTableEXT := SDL_GL_GetProcAddress( 'glColorSubTableEXT');
    glGetColorTableEXT := SDL_GL_GetProcAddress( 'glGetColorTableEXT');
    glGetColorTablePameterivEXT := SDL_GL_GetProcAddress( 'glGetColorTablePameterivEXT');
    glGetColorTablePameterfvEXT := SDL_GL_GetProcAddress( 'glGetColorTablePameterfvEXT');
    glLockArraysEXT := SDL_GL_GetProcAddress( 'glLockArraysEXT');
    glUnlockArraysEXT := SDL_GL_GetProcAddress( 'glUnlockArraysEXT');
    glCopyTexImage1DEXT := SDL_GL_GetProcAddress( 'glCopyTexImage1DEXT');
    glCopyTexSubImage1DEXT := SDL_GL_GetProcAddress( 'glCopyTexSubImage1DEXT');
    glCopyTexImage2DEXT := SDL_GL_GetProcAddress( 'glCopyTexImage2DEXT');
    glCopyTexSubImage2DEXT := SDL_GL_GetProcAddress( 'glCopyTexSubImage2DEXT');
    glCopyTexSubImage3DEXT := SDL_GL_GetProcAddress( 'glCopyTexSubImage3DEXT');
    glIndexFuncEXT := GetModuleSymbol( GLHandle, 'glIndexFuncEXT');
    glIndexMaterialEXT := SDL_GL_GetProcAddress( 'glIndexMaterialEXT');
    glPolygonOffsetEXT := SDL_GL_GetProcAddress( 'glPolygonOffsetEXT');
    glTexSubImage1dEXT := SDL_GL_GetProcAddress( 'glTexSubImage1DEXT');
    glTexSubImage2dEXT := SDL_GL_GetProcAddress( 'glTexSubImage2DEXT');
    glTexSubImage3dEXT := SDL_GL_GetProcAddress( 'glTexSubImage3DEXT');
    glGenTexturesEXT := SDL_GL_GetProcAddress( 'glGenTexturesEXT');
    glDeleteTexturesEXT := SDL_GL_GetProcAddress( 'glDeleteTexturesEXT');
    glBindTextureEXT := SDL_GL_GetProcAddress( 'glBindTextureEXT');
    glPrioritizeTexturesEXT := SDL_GL_GetProcAddress( 'glPrioritizeTexturesEXT');
    glAreTexturesResidentEXT := SDL_GL_GetProcAddress( 'glAreTexturesResidentEXT');
    glIsTextureEXT := GetModuleSymbol( GLHandle, 'glIsTextureEXT');

    // EXT_vertex_array
    glArrayElementEXT := SDL_GL_GetProcAddress( 'glArrayElementEXT');
    glColorPointerEXT := SDL_GL_GetProcAddress( 'glColorPointerEXT');
    glDrawArraysEXT := SDL_GL_GetProcAddress( 'glDrawArraysEXT');
    glEdgeFlagPointerEXT := SDL_GL_GetProcAddress( 'glEdgeFlagPointerEXT');
    glGetPointervEXT := SDL_GL_GetProcAddress( 'glGetPointervEXT');
    glIndexPointerEXT := SDL_GL_GetProcAddress( 'glIndexPointerEXT');
    glNormalPointerEXT := SDL_GL_GetProcAddress( 'glNormalPointerEXT');
    glTexCoordPointerEXT := SDL_GL_GetProcAddress( 'glTexCoordPointerEXT');
    glVertexPointerEXT := SDL_GL_GetProcAddress( 'glVertexPointerEXT');

    // ARB_multitexture
    glMultiTexCoord1dARB := SDL_GL_GetProcAddress( 'glMultiTexCoord1dARB');
    glMultiTexCoord1dVARB := SDL_GL_GetProcAddress( 'glMultiTexCoord1dVARB');
    glMultiTexCoord1fARBP := SDL_GL_GetProcAddress( 'glMultiTexCoord1fARBP');
    glMultiTexCoord1fVARB := SDL_GL_GetProcAddress( 'glMultiTexCoord1fVARB');
    glMultiTexCoord1iARB := SDL_GL_GetProcAddress( 'glMultiTexCoord1iARB');
    glMultiTexCoord1iVARB := SDL_GL_GetProcAddress( 'glMultiTexCoord1iVARB');
    glMultiTexCoord1sARBP := SDL_GL_GetProcAddress( 'glMultiTexCoord1sARBP');
    glMultiTexCoord1sVARB := SDL_GL_GetProcAddress( 'glMultiTexCoord1sVARB');
    glMultiTexCoord2dARB := SDL_GL_GetProcAddress( 'glMultiTexCoord2dARB');
    glMultiTexCoord2dvARB := SDL_GL_GetProcAddress( 'glMultiTexCoord2dvARB');
    glMultiTexCoord2fARB := SDL_GL_GetProcAddress( 'glMultiTexCoord2fARB');
    glMultiTexCoord2fvARB := SDL_GL_GetProcAddress( 'glMultiTexCoord2fvARB');
    glMultiTexCoord2iARB := SDL_GL_GetProcAddress( 'glMultiTexCoord2iARB');
    glMultiTexCoord2ivARB := SDL_GL_GetProcAddress( 'glMultiTexCoord2ivARB');
    glMultiTexCoord2sARB := SDL_GL_GetProcAddress( 'glMultiTexCoord2sARB');
    glMultiTexCoord2svARB := SDL_GL_GetProcAddress( 'glMultiTexCoord2svARB');
    glMultiTexCoord3dARB := SDL_GL_GetProcAddress( 'glMultiTexCoord3dARB');
    glMultiTexCoord3dvARB := SDL_GL_GetProcAddress( 'glMultiTexCoord3dvARB');
    glMultiTexCoord3fARB := SDL_GL_GetProcAddress( 'glMultiTexCoord3fARB');
    glMultiTexCoord3fvARB := SDL_GL_GetProcAddress( 'glMultiTexCoord3fvARB');
    glMultiTexCoord3iARB := SDL_GL_GetProcAddress( 'glMultiTexCoord3iARB');
    glMultiTexCoord3ivARB := SDL_GL_GetProcAddress( 'glMultiTexCoord3ivARB');
    glMultiTexCoord3sARB := SDL_GL_GetProcAddress( 'glMultiTexCoord3sARB');
    glMultiTexCoord3svARB := SDL_GL_GetProcAddress( 'glMultiTexCoord3svARB');
    glMultiTexCoord4dARB := SDL_GL_GetProcAddress( 'glMultiTexCoord4dARB');
    glMultiTexCoord4dvARB := SDL_GL_GetProcAddress( 'glMultiTexCoord4dvARB');
    glMultiTexCoord4fARB := SDL_GL_GetProcAddress( 'glMultiTexCoord4fARB');
    glMultiTexCoord4fvARB := SDL_GL_GetProcAddress( 'glMultiTexCoord4fvARB');
    glMultiTexCoord4iARB := SDL_GL_GetProcAddress( 'glMultiTexCoord4iARB');
    glMultiTexCoord4ivARB := SDL_GL_GetProcAddress( 'glMultiTexCoord4ivARB');
    glMultiTexCoord4sARB := SDL_GL_GetProcAddress( 'glMultiTexCoord4sARB');
    glMultiTexCoord4svARB := SDL_GL_GetProcAddress( 'glMultiTexCoord4svARB');
    glActiveTextureARB := SDL_GL_GetProcAddress( 'glActiveTextureARB');
    glClientActiveTextureARB := SDL_GL_GetProcAddress( 'glClientActiveTextureARB');

    // EXT_compiled_vertex_array
    glLockArrayEXT := SDL_GL_GetProcAddress( 'glLockArrayEXT');
    glUnlockArrayEXT := SDL_GL_GetProcAddress( 'glUnlockArrayEXT');

    // EXT_cull_vertex
    glCullParameterdvEXT := SDL_GL_GetProcAddress( 'glCullParameterdvEXT');
    glCullParameterfvEXT := SDL_GL_GetProcAddress( 'glCullParameterfvEXT');

    // WIN_swap_hint
    glAddSwapHintRectWIN := SDL_GL_GetProcAddress( 'glAddSwapHintRectWIN');

    // EXT_point_parameter
    glPointParameterfEXT := SDL_GL_GetProcAddress( 'glPointParameterfEXT');
    glPointParameterfvEXT := SDL_GL_GetProcAddress( 'glPointParameterfvEXT');

    // GL_ARB_transpose_matrix
    glLoadTransposeMatrixfARB := SDL_GL_GetProcAddress( 'glLoadTransposeMatrixfARB');
    glLoadTransposeMatrixdARB := SDL_GL_GetProcAddress( 'glLoadTransposeMatrixdARB');
    glMultTransposeMatrixfARB := SDL_GL_GetProcAddress( 'glMultTransposeMatrixfARB');
    glMultTransposeMatrixdARB := SDL_GL_GetProcAddress( 'glMultTransposeMatrixdARB');

    glSampleCoverageARB := SDL_GL_GetProcAddress( 'glSampleCoverageARB');
    glSamplePassARB := SDL_GL_GetProcAddress( 'glSamplePassARB');

    // GL_ARB_multisample
    glCompressedTexImage3DARB := SDL_GL_GetProcAddress( 'glCompressedTexImage3DARB');
    glCompressedTexImage2DARB := SDL_GL_GetProcAddress( 'glCompressedTexImage2DARB');
    glCompressedTexImage1DARB := SDL_GL_GetProcAddress( 'glCompressedTexImage1DARB');
    glCompressedTexSubImage3DARB := SDL_GL_GetProcAddress( 'glCompressedTexSubImage3DARB');
    glCompressedTexSubImage2DARB := SDL_GL_GetProcAddress( 'glCompressedTexSubImage2DARB');
    glCompressedTexSubImage1DARB := SDL_GL_GetProcAddress( 'glCompressedTexSubImage1DARB');
    glGetCompressedTexImageARB := SDL_GL_GetProcAddress( 'glGetCompressedTexImageARB');

    // GL_EXT_blend_color
    glBlendColorEXT := SDL_GL_GetProcAddress( 'glBlendColorEXT');

    // GL_EXT_texture3D
    glTexImage3DEXT := SDL_GL_GetProcAddress( 'glTexImage3DEXT');

    // GL_SGIS_texture_filter4
    glGetTexFilterFuncSGIS := SDL_GL_GetProcAddress( 'glGetTexFilterFuncSGIS'); 
    glTexFilterFuncSGIS := SDL_GL_GetProcAddress( 'glTexFilterFuncSGIS');

    // GL_EXT_histogram
    glGetHistogramEXT := SDL_GL_GetProcAddress( 'glGetHistogramEXT'); 
    glGetHistogramParameterfvEXT := SDL_GL_GetProcAddress( 'glGetHistogramParameterfvEXT'); 
    glGetHistogramParameterivEXT := SDL_GL_GetProcAddress( 'glGetHistogramParameterivEXT'); 
    glGetMinmaxEXT := SDL_GL_GetProcAddress( 'glGetMinmaxEXT'); 
    glGetMinmaxParameterfvEXT := SDL_GL_GetProcAddress( 'glGetMinmaxParameterfvEXT'); 
    glGetMinmaxParameterivEXT := SDL_GL_GetProcAddress( 'glGetMinmaxParameterivEXT'); 
    glHistogramEXT := SDL_GL_GetProcAddress( 'glHistogramEXT'); 
    glMinmaxEXT := SDL_GL_GetProcAddress( 'glMinmaxEXT'); 
    glResetHistogramEXT := SDL_GL_GetProcAddress( 'glResetHistogramEXT'); 
    glResetMinmaxEXT := SDL_GL_GetProcAddress( 'glResetMinmaxEXT'); 

    // GL_EXT_convolution
    glConvolutionFilter1DEXT := SDL_GL_GetProcAddress( 'glConvolutionFilter1DEXT'); 
    glConvolutionFilter2DEXT := SDL_GL_GetProcAddress( 'glConvolutionFilter2DEXT'); 
    glConvolutionParameterfEXT := SDL_GL_GetProcAddress( 'glConvolutionParameterfEXT');
    glConvolutionParameterfvEXT := SDL_GL_GetProcAddress( 'glConvolutionParameterfvEXT'); 
    glConvolutionParameteriEXT := SDL_GL_GetProcAddress( 'glConvolutionParameteriEXT'); 
    glConvolutionParameterivEXT := SDL_GL_GetProcAddress( 'glConvolutionParameterivEXT'); 
    glCopyConvolutionFilter1DEXT := SDL_GL_GetProcAddress( 'glCopyConvolutionFilter1DEXT'); 
    glCopyConvolutionFilter2DEXT := SDL_GL_GetProcAddress( 'glCopyConvolutionFilter2DEXT'); 
    glGetConvolutionFilterEXT := SDL_GL_GetProcAddress( 'glGetConvolutionFilterEXT'); 
    glGetConvolutionParameterfvEXT := SDL_GL_GetProcAddress( 'glGetConvolutionParameterfvEXT'); 
    glGetConvolutionParameterivEXT := SDL_GL_GetProcAddress( 'glGetConvolutionParameterivEXT');
    glGetSeparableFilterEXT := SDL_GL_GetProcAddress( 'glGetSeparableFilterEXT'); 
    glSeparableFilter2DEXT := SDL_GL_GetProcAddress( 'glSeparableFilter2DEXT'); 

    // GL_SGI_color_table
    glColorTableSGI := SDL_GL_GetProcAddress( 'glColorTableSGI'); 
    glColorTableParameterfvSGI := SDL_GL_GetProcAddress( 'glColorTableParameterfvSGI'); 
    glColorTableParameterivSGI := SDL_GL_GetProcAddress( 'glColorTableParameterivSGI'); 
    glCopyColorTableSGI := SDL_GL_GetProcAddress( 'glCopyColorTableSGI');
    glGetColorTableSGI := SDL_GL_GetProcAddress( 'glGetColorTableSGI'); 
    glGetColorTableParameterfvSGI := SDL_GL_GetProcAddress( 'glGetColorTableParameterfvSGI'); 
    glGetColorTableParameterivSGI := SDL_GL_GetProcAddress( 'glGetColorTableParameterivSGI'); 

    // GL_SGIX_pixel_texture
    glPixelTexGenSGIX := SDL_GL_GetProcAddress( 'glPixelTexGenSGIX'); 

    // GL_SGIS_pixel_texture
    glPixelTexGenParameteriSGIS := SDL_GL_GetProcAddress( 'glPixelTexGenParameteriSGIS'); 
    glPixelTexGenParameterivSGIS := SDL_GL_GetProcAddress( 'glPixelTexGenParameterivSGIS'); 
    glPixelTexGenParameterfSGIS := SDL_GL_GetProcAddress( 'glPixelTexGenParameterfSGIS'); 
    glPixelTexGenParameterfvSGIS := SDL_GL_GetProcAddress( 'glPixelTexGenParameterfvSGIS'); 
    glGetPixelTexGenParameterivSGIS := SDL_GL_GetProcAddress( 'glGetPixelTexGenParameterivSGIS'); 
    glGetPixelTexGenParameterfvSGIS := SDL_GL_GetProcAddress( 'glGetPixelTexGenParameterfvSGIS'); 

    // GL_SGIS_texture4D
    glTexImage4DSGIS := SDL_GL_GetProcAddress( 'glTexImage4DSGIS');
    glTexSubImage4DSGIS := SDL_GL_GetProcAddress( 'glTexSubImage4DSGIS'); 

    // GL_SGIS_detail_texture
    glDetailTexFuncSGIS := SDL_GL_GetProcAddress( 'glDetailTexFuncSGIS'); 
    glGetDetailTexFuncSGIS := SDL_GL_GetProcAddress( 'glGetDetailTexFuncSGIS'); 

    // GL_SGIS_sharpen_texture
    glSharpenTexFuncSGIS := SDL_GL_GetProcAddress( 'glSharpenTexFuncSGIS'); 
    glGetSharpenTexFuncSGIS := SDL_GL_GetProcAddress( 'glGetSharpenTexFuncSGIS'); 

    // GL_SGIS_multisample
    glSampleMaskSGIS := SDL_GL_GetProcAddress( 'glSampleMaskSGIS'); 
    glSamplePatternSGIS := SDL_GL_GetProcAddress( 'glSamplePatternSGIS'); 

    // GL_EXT_blend_minmax
    glBlendEquationEXT := SDL_GL_GetProcAddress( 'glBlendEquationEXT'); 

    // GL_SGIX_sprite
    glSpriteParameterfSGIX := SDL_GL_GetProcAddress( 'glSpriteParameterfSGIX'); 
    glSpriteParameterfvSGIX := SDL_GL_GetProcAddress( 'glSpriteParameterfvSGIX'); 
    glSpriteParameteriSGIX := SDL_GL_GetProcAddress( 'glSpriteParameteriSGIX'); 
    glSpriteParameterivSGIX := SDL_GL_GetProcAddress( 'glSpriteParameterivSGIX'); 

    // GL_EXT_point_parameters
    glPointParameterfSGIS := SDL_GL_GetProcAddress( 'glPointParameterfSGIS');
    glPointParameterfvSGIS := SDL_GL_GetProcAddress( 'glPointParameterfvSGIS'); 

    // GL_SGIX_instruments
    glGetInstrumentsSGIX := SDL_GL_GetProcAddress( 'glGetInstrumentsSGIX'); 
    glInstrumentsBufferSGIX := SDL_GL_GetProcAddress( 'glInstrumentsBufferSGIX'); 
    glPollInstrumentsSGIX := SDL_GL_GetProcAddress( 'glPollInstrumentsSGIX');
    glReadInstrumentsSGIX := SDL_GL_GetProcAddress( 'glReadInstrumentsSGIX'); 
    glStartInstrumentsSGIX := SDL_GL_GetProcAddress( 'glStartInstrumentsSGIX'); 
    glStopInstrumentsSGIX := SDL_GL_GetProcAddress( 'glStopInstrumentsSGIX'); 

    // GL_SGIX_framezoom
    glFrameZoomSGIX := SDL_GL_GetProcAddress( 'glFrameZoomSGIX'); 

    // GL_SGIX_tag_sample_buffer
    glTagSampleBufferSGIX := SDL_GL_GetProcAddress( 'glTagSampleBufferSGIX'); 

    // GL_SGIX_polynomial_ffd
    glDeformationMap3dSGIX := SDL_GL_GetProcAddress( 'glDeformationMap3dSGIX'); 
    glDeformationMap3fSGIX := SDL_GL_GetProcAddress( 'glDeformationMap3fSGIX'); 
    glDeformSGIX := SDL_GL_GetProcAddress( 'glDeformSGIX'); 
    glLoadIdentityDeformationMapSGIX := SDL_GL_GetProcAddress( 'glLoadIdentityDeformationMapSGIX'); 

    // GL_SGIX_reference_plane
    glReferencePlaneSGIX := SDL_GL_GetProcAddress( 'glReferencePlaneSGIX'); 

    // GL_SGIX_flush_raster
    glFlushRasterSGIX := SDL_GL_GetProcAddress( 'glFlushRasterSGIX'); 

    // GL_SGIS_fog_function
    glFogFuncSGIS := SDL_GL_GetProcAddress( 'glFogFuncSGIS'); 
    glGetFogFuncSGIS := SDL_GL_GetProcAddress( 'glGetFogFuncSGIS'); 

    // GL_HP_image_transform
    glImageTransformParameteriHP := SDL_GL_GetProcAddress( 'glImageTransformParameteriHP'); 
    glImageTransformParameterfHP := SDL_GL_GetProcAddress( 'glImageTransformParameterfHP'); 
    glImageTransformParameterivHP := SDL_GL_GetProcAddress( 'glImageTransformParameterivHP'); 
    glImageTransformParameterfvHP := SDL_GL_GetProcAddress( 'glImageTransformParameterfvHP'); 
    glGetImageTransformParameterivHP := SDL_GL_GetProcAddress( 'glGetImageTransformParameterivHP');
    glGetImageTransformParameterfvHP := SDL_GL_GetProcAddress( 'glGetImageTransformParameterfvHP'); 

    // GL_EXT_color_subtable
    glCopyColorSubTableEXT := SDL_GL_GetProcAddress( 'glCopyColorSubTableEXT'); 

    // GL_PGI_misc_hints
    glHintPGI := SDL_GL_GetProcAddress( 'glHintPGI'); 

    // GL_EXT_paletted_texture
    glGetColorTableParameterivEXT := SDL_GL_GetProcAddress( 'glGetColorTableParameterivEXT'); 
    glGetColorTableParameterfvEXT := SDL_GL_GetProcAddress( 'glGetColorTableParameterfvEXT'); 

    // GL_SGIX_list_priority
    glGetListParameterfvSGIX := SDL_GL_GetProcAddress( 'glGetListParameterfvSGIX'); 
    glGetListParameterivSGIX := SDL_GL_GetProcAddress( 'glGetListParameterivSGIX'); 
    glListParameterfSGIX := SDL_GL_GetProcAddress( 'glListParameterfSGIX');
    glListParameterfvSGIX := SDL_GL_GetProcAddress( 'glListParameterfvSGIX'); 
    glListParameteriSGIX := SDL_GL_GetProcAddress( 'glListParameteriSGIX'); 
    glListParameterivSGIX := SDL_GL_GetProcAddress( 'glListParameterivSGIX'); 

    // GL_SGIX_fragment_lighting
    glFragmentColorMaterialSGIX := SDL_GL_GetProcAddress( 'glFragmentColorMaterialSGIX'); 
    glFragmentLightfSGIX := SDL_GL_GetProcAddress( 'glFragmentLightfSGIX'); 
    glFragmentLightfvSGIX := SDL_GL_GetProcAddress( 'glFragmentLightfvSGIX'); 
    glFragmentLightiSGIX := SDL_GL_GetProcAddress( 'glFragmentLightiSGIX'); 
    glFragmentLightivSGIX := SDL_GL_GetProcAddress( 'glFragmentLightivSGIX'); 
    glFragmentLightModelfSGIX := SDL_GL_GetProcAddress( 'glFragmentLightModelfSGIX'); 
    glFragmentLightModelfvSGIX := SDL_GL_GetProcAddress( 'glFragmentLightModelfvSGIX'); 
    glFragmentLightModeliSGIX := SDL_GL_GetProcAddress( 'glFragmentLightModeliSGIX'); 
    glFragmentLightModelivSGIX := SDL_GL_GetProcAddress( 'glFragmentLightModelivSGIX'); 
    glFragmentMaterialfSGIX := SDL_GL_GetProcAddress( 'glFragmentMaterialfSGIX'); 
    glFragmentMaterialfvSGIX := SDL_GL_GetProcAddress( 'glFragmentMaterialfvSGIX');
    glFragmentMaterialiSGIX := SDL_GL_GetProcAddress( 'glFragmentMaterialiSGIX'); 
    glFragmentMaterialivSGIX := SDL_GL_GetProcAddress( 'glFragmentMaterialivSGIX'); 
    glGetFragmentLightfvSGIX := SDL_GL_GetProcAddress( 'glGetFragmentLightfvSGIX'); 
    glGetFragmentLightivSGIX := SDL_GL_GetProcAddress( 'glGetFragmentLightivSGIX'); 
    glGetFragmentMaterialfvSGIX := SDL_GL_GetProcAddress( 'glGetFragmentMaterialfvSGIX');
    glGetFragmentMaterialivSGIX := SDL_GL_GetProcAddress( 'glGetFragmentMaterialivSGIX'); 
    glLightEnviSGIX := SDL_GL_GetProcAddress( 'glLightEnviSGIX'); 

    // GL_EXT_draw_range_elements
    glDrawRangeElementsEXT := SDL_GL_GetProcAddress( 'glDrawRangeElementsEXT'); 

    // GL_EXT_light_texture
    glApplyTextureEXT := SDL_GL_GetProcAddress( 'glApplyTextureEXT'); 
    glTextureLightEXT := SDL_GL_GetProcAddress( 'glTextureLightEXT'); 
    glTextureMaterialEXT := SDL_GL_GetProcAddress( 'glTextureMaterialEXT'); 

    // GL_SGIX_async
    glAsyncMarkerSGIX := SDL_GL_GetProcAddress( 'glAsyncMarkerSGIX'); 
    glFinishAsyncSGIX := SDL_GL_GetProcAddress( 'glFinishAsyncSGIX'); 
    glPollAsyncSGIX := SDL_GL_GetProcAddress( 'glPollAsyncSGIX'); 
    glGenAsyncMarkersSGIX := SDL_GL_GetProcAddress( 'glGenAsyncMarkersSGIX'); 
    glDeleteAsyncMarkersSGIX := SDL_GL_GetProcAddress( 'glDeleteAsyncMarkersSGIX'); 
    glIsAsyncMarkerSGIX := SDL_GL_GetProcAddress( 'glIsAsyncMarkerSGIX'); 

    // GL_INTEL_parallel_arrays
    glVertexPointervINTEL := SDL_GL_GetProcAddress( 'glVertexPointervINTEL'); 
    glNormalPointervINTEL := SDL_GL_GetProcAddress( 'glNormalPointervINTEL'); 
    glColorPointervINTEL := SDL_GL_GetProcAddress( 'glColorPointervINTEL'); 
    glTexCoordPointervINTEL := SDL_GL_GetProcAddress( 'glTexCoordPointervINTEL'); 

    // GL_EXT_pixel_transform
    glPixelTransformParameteriEXT := SDL_GL_GetProcAddress( 'glPixelTransformParameteriEXT');
    glPixelTransformParameterfEXT := SDL_GL_GetProcAddress( 'glPixelTransformParameterfEXT'); 
    glPixelTransformParameterivEXT := SDL_GL_GetProcAddress( 'glPixelTransformParameterivEXT'); 
    glPixelTransformParameterfvEXT := SDL_GL_GetProcAddress( 'glPixelTransformParameterfvEXT'); 

    // GL_EXT_secondary_color
    glSecondaryColor3bEXT := SDL_GL_GetProcAddress( 'glSecondaryColor3bEXT'); 
    glSecondaryColor3bvEXT := SDL_GL_GetProcAddress( 'glSecondaryColor3bvEXT'); 
    glSecondaryColor3dEXT := SDL_GL_GetProcAddress( 'glSecondaryColor3dEXT'); 
    glSecondaryColor3dvEXT := SDL_GL_GetProcAddress( 'glSecondaryColor3dvEXT'); 
    glSecondaryColor3fEXT := SDL_GL_GetProcAddress( 'glSecondaryColor3fEXT'); 
    glSecondaryColor3fvEXT := SDL_GL_GetProcAddress( 'glSecondaryColor3fvEXT'); 
    glSecondaryColor3iEXT := SDL_GL_GetProcAddress( 'glSecondaryColor3iEXT'); 
    glSecondaryColor3ivEXT := SDL_GL_GetProcAddress( 'glSecondaryColor3ivEXT'); 
    glSecondaryColor3sEXT := SDL_GL_GetProcAddress( 'glSecondaryColor3sEXT'); 
    glSecondaryColor3svEXT := SDL_GL_GetProcAddress( 'glSecondaryColor3svEXT'); 
    glSecondaryColor3ubEXT := SDL_GL_GetProcAddress( 'glSecondaryColor3ubEXT');
    glSecondaryColor3ubvEXT := SDL_GL_GetProcAddress( 'glSecondaryColor3ubvEXT'); 
    glSecondaryColor3uiEXT := SDL_GL_GetProcAddress( 'glSecondaryColor3uiEXT'); 
    glSecondaryColor3uivEXT := SDL_GL_GetProcAddress( 'glSecondaryColor3uivEXT'); 
    glSecondaryColor3usEXT := SDL_GL_GetProcAddress( 'glSecondaryColor3usEXT'); 
    glSecondaryColor3usvEXT := SDL_GL_GetProcAddress( 'glSecondaryColor3usvEXT'); 
    glSecondaryColorPointerEXT := SDL_GL_GetProcAddress( 'glSecondaryColorPointerEXT'); 

    // GL_EXT_texture_perturb_normal
    glTextureNormalEXT := SDL_GL_GetProcAddress( 'glTextureNormalEXT'); 

    // GL_EXT_multi_draw_arrays
    glMultiDrawArraysEXT := SDL_GL_GetProcAddress( 'glMultiDrawArraysEXT'); 
    glMultiDrawElementsEXT := SDL_GL_GetProcAddress( 'glMultiDrawElementsEXT'); 

    // GL_EXT_fog_coord
    glFogCoordfEXT := SDL_GL_GetProcAddress( 'glFogCoordfEXT');
    glFogCoordfvEXT := SDL_GL_GetProcAddress( 'glFogCoordfvEXT'); 
    glFogCoorddEXT := SDL_GL_GetProcAddress( 'glFogCoorddEXT'); 
    glFogCoorddvEXT := SDL_GL_GetProcAddress( 'glFogCoorddvEXT'); 
    glFogCoordPointerEXT := SDL_GL_GetProcAddress( 'glFogCoordPointerEXT'); 

    // GL_EXT_coordinate_frame
    glTangent3bEXT := SDL_GL_GetProcAddress( 'glTangent3bEXT'); 
    glTangent3bvEXT := SDL_GL_GetProcAddress( 'glTangent3bvEXT'); 
    glTangent3dEXT := SDL_GL_GetProcAddress( 'glTangent3dEXT'); 
    glTangent3dvEXT := SDL_GL_GetProcAddress( 'glTangent3dvEXT'); 
    glTangent3fEXT := SDL_GL_GetProcAddress( 'glTangent3fEXT'); 
    glTangent3fvEXT := SDL_GL_GetProcAddress( 'glTangent3fvEXT'); 
    glTangent3iEXT := SDL_GL_GetProcAddress( 'glTangent3iEXT'); 
    glTangent3ivEXT := SDL_GL_GetProcAddress( 'glTangent3ivEXT'); 
    glTangent3sEXT := SDL_GL_GetProcAddress( 'glTangent3sEXT'); 
    glTangent3svEXT := SDL_GL_GetProcAddress( 'glTangent3svEXT');
    glBinormal3bEXT := SDL_GL_GetProcAddress( 'glBinormal3bEXT'); 
    glBinormal3bvEXT := SDL_GL_GetProcAddress( 'glBinormal3bvEXT'); 
    glBinormal3dEXT := SDL_GL_GetProcAddress( 'glBinormal3dEXT'); 
    glBinormal3dvEXT := SDL_GL_GetProcAddress( 'glBinormal3dvEXT'); 
    glBinormal3fEXT := SDL_GL_GetProcAddress( 'glBinormal3fEXT'); 
    glBinormal3fvEXT := SDL_GL_GetProcAddress( 'glBinormal3fvEXT'); 
    glBinormal3iEXT := SDL_GL_GetProcAddress( 'glBinormal3iEXT'); 
    glBinormal3ivEXT := SDL_GL_GetProcAddress( 'glBinormal3ivEXT'); 
    glBinormal3sEXT := SDL_GL_GetProcAddress( 'glBinormal3sEXT'); 
    glBinormal3svEXT := SDL_GL_GetProcAddress( 'glBinormal3svEXT'); 
    glTangentPointerEXT := SDL_GL_GetProcAddress( 'glTangentPointerEXT'); 
    glBinormalPointerEXT := SDL_GL_GetProcAddress( 'glBinormalPointerEXT'); 

    // GL_SUNX_constant_data
    glFinishTextureSUNX := SDL_GL_GetProcAddress( 'glFinishTextureSUNX'); 

    // GL_SUN_global_alpha
    glGlobalAlphaFactorbSUN := SDL_GL_GetProcAddress( 'glGlobalAlphaFactorbSUN'); 
    glGlobalAlphaFactorsSUN := SDL_GL_GetProcAddress( 'glGlobalAlphaFactorsSUN'); 
    glGlobalAlphaFactoriSUN := SDL_GL_GetProcAddress( 'glGlobalAlphaFactoriSUN'); 
    glGlobalAlphaFactorfSUN := SDL_GL_GetProcAddress( 'glGlobalAlphaFactorfSUN'); 
    glGlobalAlphaFactordSUN := SDL_GL_GetProcAddress( 'glGlobalAlphaFactordSUN'); 
    glGlobalAlphaFactorubSUN := SDL_GL_GetProcAddress( 'glGlobalAlphaFactorubSUN'); 
    glGlobalAlphaFactorusSUN := SDL_GL_GetProcAddress( 'glGlobalAlphaFactorusSUN'); 
    glGlobalAlphaFactoruiSUN := SDL_GL_GetProcAddress( 'glGlobalAlphaFactoruiSUN');

    // GL_SUN_triangle_list
    glReplacementCodeuiSUN := SDL_GL_GetProcAddress( 'glReplacementCodeuiSUN'); 
    glReplacementCodeusSUN := SDL_GL_GetProcAddress( 'glReplacementCodeusSUN'); 
    glReplacementCodeubSUN := SDL_GL_GetProcAddress( 'glReplacementCodeubSUN'); 
    glReplacementCodeuivSUN := SDL_GL_GetProcAddress( 'glReplacementCodeuivSUN'); 
    glReplacementCodeusvSUN := SDL_GL_GetProcAddress( 'glReplacementCodeusvSUN');
    glReplacementCodeubvSUN := SDL_GL_GetProcAddress( 'glReplacementCodeubvSUN'); 
    glReplacementCodePointerSUN := SDL_GL_GetProcAddress( 'glReplacementCodePointerSUN'); 

    // GL_SUN_vertex
    glColor4ubVertex2fSUN := SDL_GL_GetProcAddress( 'glColor4ubVertex2fSUN'); 
    glColor4ubVertex2fvSUN := SDL_GL_GetProcAddress( 'glColor4ubVertex2fvSUN'); 
    glColor4ubVertex3fSUN := SDL_GL_GetProcAddress( 'glColor4ubVertex3fSUN'); 
    glColor4ubVertex3fvSUN := SDL_GL_GetProcAddress( 'glColor4ubVertex3fvSUN'); 
    glColor3fVertex3fSUN := SDL_GL_GetProcAddress( 'glColor3fVertex3fSUN'); 
    glColor3fVertex3fvSUN := SDL_GL_GetProcAddress( 'glColor3fVertex3fvSUN'); 
    glNormal3fVertex3fSUN := SDL_GL_GetProcAddress( 'glNormal3fVertex3fSUN'); 
    glNormal3fVertex3fvSUN := SDL_GL_GetProcAddress( 'glNormal3fVertex3fvSUN'); 
    glColor4fNormal3fVertex3fSUN := SDL_GL_GetProcAddress( 'glColor4fNormal3fVertex3fSUN'); 
    glColor4fNormal3fVertex3fvSUN := SDL_GL_GetProcAddress( 'glColor4fNormal3fVertex3fvSUN'); 
    glTexCoord2fVertex3fSUN := SDL_GL_GetProcAddress( 'glTexCoord2fVertex3fSUN'); 
    glTexCoord2fVertex3fvSUN := SDL_GL_GetProcAddress( 'glTexCoord2fVertex3fvSUN');
    glTexCoord4fVertex4fSUN := SDL_GL_GetProcAddress( 'glTexCoord4fVertex4fSUN'); 
    glTexCoord4fVertex4fvSUN := SDL_GL_GetProcAddress( 'glTexCoord4fVertex4fvSUN');
    glTexCoord2fColor4ubVertex3fSUN := SDL_GL_GetProcAddress( 'glTexCoord2fColor4ubVertex3fSUN'); 
    glTexCoord2fColor4ubVertex3fvSUN := SDL_GL_GetProcAddress( 'glTexCoord2fColor4ubVertex3fvSUN'); 
    glTexCoord2fColor3fVertex3fSUN := SDL_GL_GetProcAddress( 'glTexCoord2fColor3fVertex3fSUN'); 
    glTexCoord2fColor3fVertex3fvSUN := SDL_GL_GetProcAddress( 'glTexCoord2fColor3fVertex3fvSUN'); 
    glTexCoord2fNormal3fVertex3fSUN := SDL_GL_GetProcAddress( 'glTexCoord2fNormal3fVertex3fSUN'); 
    glTexCoord2fNormal3fVertex3fvSUN := SDL_GL_GetProcAddress( 'glTexCoord2fNormal3fVertex3fvSUN'); 
    glTexCoord2fColor4fNormal3fVertex3fSUN := SDL_GL_GetProcAddress( 'glTexCoord2fColor4fNormal3fVertex3fSUN'); 
    glTexCoord2fColor4fNormal3fVertex3fvSUN := SDL_GL_GetProcAddress( 'glTexCoord2fColor4fNormal3fVertex3fvSUN'); 
    glTexCoord4fColor4fNormal3fVertex4fSUN := SDL_GL_GetProcAddress( 'glTexCoord4fColor4fNormal3fVertex4fSUN'); 
    glTexCoord4fColor4fNormal3fVertex4fvSUN := SDL_GL_GetProcAddress( 'glTexCoord4fColor4fNormal3fVertex4fvSUN'); 
    glReplacementCodeuiVertex3fSUN := SDL_GL_GetProcAddress( 'glReplacementCodeuiVertex3fSUN'); 
    glReplacementCodeuiVertex3fvSUN := SDL_GL_GetProcAddress( 'glReplacementCodeuiVertex3fvSUN'); 
    glReplacementCodeuiColor4ubVertex3fSUN := SDL_GL_GetProcAddress( 'glReplacementCodeuiColor4ubVertex3fSUN'); 
    glReplacementCodeuiColor4ubVertex3fvSUN := SDL_GL_GetProcAddress( 'glReplacementCodeuiColor4ubVertex3fvSUN');
    glReplacementCodeuiColor3fVertex3fSUN := SDL_GL_GetProcAddress( 'glReplacementCodeuiColor3fVertex3fSUN'); 
    glReplacementCodeuiColor3fVertex3fvSUN := SDL_GL_GetProcAddress( 'glReplacementCodeuiColor3fVertex3fvSUN'); 
    glReplacementCodeuiNormal3fVertex3fSUN := SDL_GL_GetProcAddress( 'glReplacementCodeuiNormal3fVertex3fSUN'); 
    glReplacementCodeuiNormal3fVertex3fvSUN := SDL_GL_GetProcAddress( 'glReplacementCodeuiNormal3fVertex3fvSUN'); 
    glReplacementCodeuiColor4fNormal3fVertex3fSUN := SDL_GL_GetProcAddress( 'glReplacementCodeuiColor4fNormal3fVertex3fSUN'); 
    glReplacementCodeuiColor4fNormal3fVertex3fvSUN := SDL_GL_GetProcAddress( 'glReplacementCodeuiColor4fNormal3fVertex3fvSUN'); 
    glReplacementCodeuiTexCoord2fVertex3fSUN := SDL_GL_GetProcAddress( 'glReplacementCodeuiTexCoord2fVertex3fSUN'); 
    glReplacementCodeuiTexCoord2fVertex3fvSUN := SDL_GL_GetProcAddress( 'glReplacementCodeuiTexCoord2fVertex3fvSUN'); 
    glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN := SDL_GL_GetProcAddress( 'glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN'); 
    glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN := SDL_GL_GetProcAddress( 'glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN'); 
    glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN := SDL_GL_GetProcAddress( 'glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN');
    glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN := SDL_GL_GetProcAddress( 'glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN'); 

    // GL_EXT_blend_func_separate
    glBlendFuncSeparateEXT := SDL_GL_GetProcAddress( 'glBlendFuncSeparateEXT'); 

    // GL_EXT_vertex_weighting
    glVertexWeightfEXT := SDL_GL_GetProcAddress( 'glVertexWeightfEXT'); 
    glVertexWeightfvEXT := SDL_GL_GetProcAddress( 'glVertexWeightfvEXT'); 
    glVertexWeightPointerEXT := SDL_GL_GetProcAddress( 'glVertexWeightPointerEXT'); 

    // GL_NV_vertex_array_range
    glFlushVertexArrayRangeNV := SDL_GL_GetProcAddress( 'glFlushVertexArrayRangeNV'); 
    glVertexArrayRangeNV := SDL_GL_GetProcAddress( 'glVertexArrayRangeNV'); 
    wglAllocateMemoryNV := SDL_GL_GetProcAddress( 'wglAllocateMemoryNV'); 
    wglFreeMemoryNV := SDL_GL_GetProcAddress( 'wglFreeMemoryNV'); 

    // GL_NV_register_combiners
    glCombinerParameterfvNV := SDL_GL_GetProcAddress( 'glCombinerParameterfvNV'); 
    glCombinerParameterfNV := SDL_GL_GetProcAddress( 'glCombinerParameterfNV'); 
    glCombinerParameterivNV := SDL_GL_GetProcAddress( 'glCombinerParameterivNV'); 
    glCombinerParameteriNV := SDL_GL_GetProcAddress( 'glCombinerParameteriNV');
    glCombinerInputNV := SDL_GL_GetProcAddress( 'glCombinerInputNV'); 
    glCombinerOutputNV := SDL_GL_GetProcAddress( 'glCombinerOutputNV'); 
    glFinalCombinerInputNV := SDL_GL_GetProcAddress( 'glFinalCombinerInputNV'); 
    glGetCombinerInputParameterfvNV := SDL_GL_GetProcAddress( 'glGetCombinerInputParameterfvNV');
    glGetCombinerInputParameterivNV := SDL_GL_GetProcAddress( 'glGetCombinerInputParameterivNV'); 
    glGetCombinerOutputParameterfvNV := SDL_GL_GetProcAddress( 'glGetCombinerOutputParameterfvNV'); 
    glGetCombinerOutputParameterivNV := SDL_GL_GetProcAddress( 'glGetCombinerOutputParameterivNV'); 
    glGetFinalCombinerInputParameterfvNV := SDL_GL_GetProcAddress( 'glGetFinalCombinerInputParameterfvNV'); 
    glGetFinalCombinerInputParameterivNV := SDL_GL_GetProcAddress( 'glGetFinalCombinerInputParameterivNV'); 

    // GL_MESA_resize_buffers
    glResizeBuffersMESA := SDL_GL_GetProcAddress( 'glResizeBuffersMESA'); 

    // GL_MESA_window_pos
    glWindowPos2dMESA := SDL_GL_GetProcAddress( 'glWindowPos2dMESA'); 
    glWindowPos2dvMESA := SDL_GL_GetProcAddress( 'glWindowPos2dvMESA');
    glWindowPos2fMESA := SDL_GL_GetProcAddress( 'glWindowPos2fMESA'); 
    glWindowPos2fvMESA := SDL_GL_GetProcAddress( 'glWindowPos2fvMESA'); 
    glWindowPos2iMESA := SDL_GL_GetProcAddress( 'glWindowPos2iMESA'); 
    glWindowPos2ivMESA := SDL_GL_GetProcAddress( 'glWindowPos2ivMESA'); 
    glWindowPos2sMESA := SDL_GL_GetProcAddress( 'glWindowPos2sMESA'); 
    glWindowPos2svMESA := SDL_GL_GetProcAddress( 'glWindowPos2svMESA'); 
    glWindowPos3dMESA := SDL_GL_GetProcAddress( 'glWindowPos3dMESA'); 
    glWindowPos3dvMESA := SDL_GL_GetProcAddress( 'glWindowPos3dvMESA'); 
    glWindowPos3fMESA := SDL_GL_GetProcAddress( 'glWindowPos3fMESA'); 
    glWindowPos3fvMESA := SDL_GL_GetProcAddress( 'glWindowPos3fvMESA'); 
    glWindowPos3iMESA := SDL_GL_GetProcAddress( 'glWindowPos3iMESA'); 
    glWindowPos3ivMESA := SDL_GL_GetProcAddress( 'glWindowPos3ivMESA'); 
    glWindowPos3sMESA := SDL_GL_GetProcAddress( 'glWindowPos3sMESA');
    glWindowPos3svMESA := SDL_GL_GetProcAddress( 'glWindowPos3svMESA'); 
    glWindowPos4dMESA := SDL_GL_GetProcAddress( 'glWindowPos4dMESA'); 
    glWindowPos4dvMESA := SDL_GL_GetProcAddress( 'glWindowPos4dvMESA');
    glWindowPos4fMESA := SDL_GL_GetProcAddress( 'glWindowPos4fMESA'); 
    glWindowPos4fvMESA := SDL_GL_GetProcAddress( 'glWindowPos4fvMESA'); 
    glWindowPos4iMESA := SDL_GL_GetProcAddress( 'glWindowPos4iMESA'); 
    glWindowPos4ivMESA := SDL_GL_GetProcAddress( 'glWindowPos4ivMESA'); 
    glWindowPos4sMESA := SDL_GL_GetProcAddress( 'glWindowPos4sMESA'); 
    glWindowPos4svMESA := SDL_GL_GetProcAddress( 'glWindowPos4svMESA'); 

    // GL_IBM_multimode_draw_arrays
    glMultiModeDrawArraysIBM := SDL_GL_GetProcAddress( 'glMultiModeDrawArraysIBM'); 
    glMultiModeDrawElementsIBM := SDL_GL_GetProcAddress( 'glMultiModeDrawElementsIBM'); 

    // GL_IBM_vertex_array_lists
    glColorPointerListIBM := SDL_GL_GetProcAddress( 'glColorPointerListIBM'); 
    glSecondaryColorPointerListIBM := SDL_GL_GetProcAddress( 'glSecondaryColorPointerListIBM'); 
    glEdgeFlagPointerListIBM := SDL_GL_GetProcAddress( 'glEdgeFlagPointerListIBM'); 
    glFogCoordPointerListIBM := SDL_GL_GetProcAddress( 'glFogCoordPointerListIBM');
    glIndexPointerListIBM := SDL_GL_GetProcAddress( 'glIndexPointerListIBM'); 
    glNormalPointerListIBM := SDL_GL_GetProcAddress( 'glNormalPointerListIBM'); 
    glTexCoordPointerListIBM := SDL_GL_GetProcAddress( 'glTexCoordPointerListIBM'); 
    glVertexPointerListIBM := SDL_GL_GetProcAddress( 'glVertexPointerListIBM'); 

    // GL_3DFX_tbuffer
    glTbufferMask3DFX := SDL_GL_GetProcAddress( 'glTbufferMask3DFX'); 

    // GL_EXT_multisample
    glSampleMaskEXT := SDL_GL_GetProcAddress( 'glSampleMaskEXT'); 
    glSamplePatternEXT := SDL_GL_GetProcAddress( 'glSamplePatternEXT'); 

    // GL_SGIS_texture_color_mask
    glTextureColorMaskSGIS := SDL_GL_GetProcAddress( 'glTextureColorMaskSGIS'); 

    // GL_SGIX_igloo_interface
    glIglooInterfaceSGIX := SDL_GL_GetProcAddress( 'glIglooInterfaceSGIX'); 

    // GLU extensions
    gluNurbsCallbackDataEXT := SDL_GL_GetProcAddress( 'gluNurbsCallbackDataEXT'); 
    gluNewNurbsTessellatorEXT := SDL_GL_GetProcAddress( 'gluNewNurbsTessellatorEXT'); 
    gluDeleteNurbsTessellatorEXT := SDL_GL_GetProcAddress( 'gluDeleteNurbsTessellatorEXT'); 

    // GL_NV_vertex_program
    glAreProgramsResidentNV := SDL_GL_GetProcAddress( 'glAreProgramsResidentNV'); 
    glBindProgramNV := SDL_GL_GetProcAddress( 'glBindProgramNV'); 
    glDeleteProgramsNV := SDL_GL_GetProcAddress( 'glDeleteProgramsNV'); 
    glExecuteProgramNV := SDL_GL_GetProcAddress( 'glExecuteProgramNV'); 
    glGenProgramsNV := SDL_GL_GetProcAddress( 'glGenProgramsNV'); 
    glGetProgramParameterdvNV := SDL_GL_GetProcAddress( 'glGetProgramParameterdvNV'); 
    glGetProgramParameterfvNV := SDL_GL_GetProcAddress( 'glGetProgramParameterfvNV');
    glGetProgramivNV := SDL_GL_GetProcAddress( 'glGetProgramivNV');
    glGetProgramStringNV := SDL_GL_GetProcAddress( 'glGetProgramStringNV'); 
    glGetTrackMatrixivNV := SDL_GL_GetProcAddress( 'glGetTrackMatrixivNV'); 
    glGetVertexAttribdvNV:= SDL_GL_GetProcAddress( 'glGetVertexAttribdvNV'); 
    glGetVertexAttribfvNV:= SDL_GL_GetProcAddress( 'glGetVertexAttribfvNV'); 
    glGetVertexAttribivNV:= SDL_GL_GetProcAddress( 'glGetVertexAttribivNV'); 
    glGetVertexAttribPointervNV := wglGetProcAddress ('glGetVertexAttribPointervNV'); 
    glIsProgramNV := SDL_GL_GetProcAddress( 'glIsProgramNV'); 
    glLoadProgramNV := SDL_GL_GetProcAddress( 'glLoadProgramNV'); 
    glProgramParameter4dNV := SDL_GL_GetProcAddress( 'glProgramParameter4dNV'); 
    glProgramParameter4dvNV := SDL_GL_GetProcAddress( 'glProgramParameter4dvNV'); 
    glProgramParameter4fNV := SDL_GL_GetProcAddress( 'glProgramParameter4fNV'); 
    glProgramParameter4fvNV := SDL_GL_GetProcAddress( 'glProgramParameter4fvNV'); 
    glProgramParameters4dvNV := wglGetProcAddress ('glProgramParameters4dvNV'); 
    glProgramParameters4fvNV := wglGetProcAddress ('glProgramParameters4fvNV'); 
    glRequestResidentProgramsNV := wglGetProcAddress ('glRequestResidentProgramsNV'); 
    glTrackMatrixNV := SDL_GL_GetProcAddress( 'glTrackMatrixNV');
    glVertexAttribPointerNV := SDL_GL_GetProcAddress( 'glVertexAttribPointerNV'); 
    glVertexAttrib1dNV := SDL_GL_GetProcAddress( 'glVertexAttrib1dNV'); 
    glVertexAttrib1dvNV := SDL_GL_GetProcAddress( 'glVertexAttrib1dvNV'); 
    glVertexAttrib1fNV := SDL_GL_GetProcAddress( 'glVertexAttrib1fNV'); 
    glVertexAttrib1fvNV := SDL_GL_GetProcAddress( 'glVertexAttrib1fvNV'); 
    glVertexAttrib1sNV := SDL_GL_GetProcAddress( 'glVertexAttrib1sNV'); 
    glVertexAttrib1svNV := SDL_GL_GetProcAddress( 'glVertexAttrib1svNV'); 
    glVertexAttrib2dNV := SDL_GL_GetProcAddress( 'glVertexAttrib2dNV');
    glVertexAttrib2dvNV := SDL_GL_GetProcAddress( 'glVertexAttrib2dvNV'); 
    glVertexAttrib2fNV := SDL_GL_GetProcAddress( 'glVertexAttrib2fNV'); 
    glVertexAttrib2fvNV := SDL_GL_GetProcAddress( 'glVertexAttrib2fvNV'); 
    glVertexAttrib2sNV := SDL_GL_GetProcAddress( 'glVertexAttrib2sNV'); 
    glVertexAttrib2svNV := SDL_GL_GetProcAddress( 'glVertexAttrib2svNV'); 
    glVertexAttrib3dNV := SDL_GL_GetProcAddress( 'glVertexAttrib3dNV'); 
    glVertexAttrib3dvNV := SDL_GL_GetProcAddress( 'glVertexAttrib3dvNV'); 
    glVertexAttrib3fNV := SDL_GL_GetProcAddress( 'glVertexAttrib3fNV');
    glVertexAttrib3fvNV := SDL_GL_GetProcAddress( 'glVertexAttrib3fvNV'); 
    glVertexAttrib3sNV := SDL_GL_GetProcAddress( 'glVertexAttrib3sNV'); 
    glVertexAttrib3svNV := SDL_GL_GetProcAddress( 'glVertexAttrib3svNV'); 
    glVertexAttrib4dNV := SDL_GL_GetProcAddress( 'glVertexAttrib4dNV'); 
    glVertexAttrib4dvNV := SDL_GL_GetProcAddress( 'glVertexAttrib4dvNV'); 
    glVertexAttrib4fNV := SDL_GL_GetProcAddress( 'glVertexAttrib4fNV'); 
    glVertexAttrib4fvNV := SDL_GL_GetProcAddress( 'glVertexAttrib4fvNV'); 
    glVertexAttrib4sNV := SDL_GL_GetProcAddress( 'glVertexAttrib4sNV'); 
    glVertexAttrib4svNV := SDL_GL_GetProcAddress( 'glVertexAttrib4svNV'); 
    glVertexAttrib4ubvNV := SDL_GL_GetProcAddress( 'glVertexAttrib4ubvNV'); 
    glVertexAttribs1dvNV := SDL_GL_GetProcAddress( 'glVertexAttribs1dvNV'); 
    glVertexAttribs1fvNV := SDL_GL_GetProcAddress( 'glVertexAttribs1fvNV'); 
    glVertexAttribs1svNV := SDL_GL_GetProcAddress( 'glVertexAttribs1svNV'); 
    glVertexAttribs2dvNV := SDL_GL_GetProcAddress( 'glVertexAttribs2dvNV'); 
    glVertexAttribs2fvNV := SDL_GL_GetProcAddress( 'glVertexAttribs2fvNV'); 
    glVertexAttribs2svNV := SDL_GL_GetProcAddress( 'glVertexAttribs2svNV');
    glVertexAttribs3dvNV := SDL_GL_GetProcAddress( 'glVertexAttribs3dvNV');
    glVertexAttribs3fvNV := SDL_GL_GetProcAddress( 'glVertexAttribs3fvNV'); 
    glVertexAttribs3svNV := SDL_GL_GetProcAddress( 'glVertexAttribs3svNV'); 
    glVertexAttribs4dvNV := SDL_GL_GetProcAddress( 'glVertexAttribs4dvNV'); 
    glVertexAttribs4fvNV := SDL_GL_GetProcAddress( 'glVertexAttribs4fvNV'); 
    glVertexAttribs4svNV := SDL_GL_GetProcAddress( 'glVertexAttribs4svNV'); 
    glVertexAttribs4ubvNV := SDL_GL_GetProcAddress( 'glVertexAttribs4ubvN'); 

    // ARB wgl extensions
    wglGetExtensionsStringARB := SDL_GL_GetProcAddress( 'wglGetExtensionsStringARB');
    wglGetPixelFormatAttribivARB := SDL_GL_GetProcAddress( 'wglGetPixelFormatAttribivARB');
    wglGetPixelFormatAttribfvARB := SDL_GL_GetProcAddress( 'wglGetPixelFormatAttribfvARB');
    wglChoosePixelFormatARB := SDL_GL_GetProcAddress( 'wglChoosePixelFormatARB');

    // To get synchronized again, if this proc was called externally.
    LastPixelFormat := 0;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TrimAndSplitVersionString(Buffer: String; var Max, Min: Integer);

// Peels out the X.Y form from the given Buffer which must contain a version string like "text Minor.Major.Build text"
// at least however "Major.Minor".

var
  Separator: Integer;
   
begin
  try
    // There must be at least one dot to separate major and minor version number.
    Separator := Pos('.', Buffer);
    // At least one number must be before and one after the dot.
    if (Separator > 1) and (Separator < Length(Buffer)) and (Buffer[Separator - 1] in ['0'..'9']) and
      (Buffer[Separator + 1] in ['0'..'9']) then
    begin
      // OK, it's a valid version string. Now remove unnecessary parts.
      Dec(Separator); 
      // Find last non-numeric character before version number.
      while (Separator > 0) and (Buffer[Separator] in ['0'..'9']) do
        Dec(Separator); 
      // Delete leading characters which do not belong to the version string.
      Delete(Buffer, 1, Separator);
      Separator := Pos('.', Buffer) + 1;
      // Find first non-numeric character after version number
      while (Separator <= Length(Buffer)) and (Buffer[Separator] in ['0'..'9']) do
        Inc(Separator); 
      // delete trailing characters not belonging to the version string
      Delete(Buffer, Separator, 255); 
      // Now translate the numbers.
      Separator := Pos('.', Buffer); // This is necessary because the buffer length might have changed.
      Max := StrToInt(Copy(Buffer, 1, Separator - 1)); 
      Min := StrToInt(Copy(Buffer, Separator + 1, 255)); 
    end
    else
      Abort; 
  except
    Min := 0; 
    Max := 0; 
  end; 
end; 

//----------------------------------------------------------------------------------------------------------------------

procedure ReadImplementationProperties;
 
var
  Buffer: string; 
  MajorVersion,
  MinorVersion: Integer;

  //--------------- local function --------------------------------------------

   function CheckExtension(const Extension: string): Boolean;

   // Checks if the given Extension string is in Buffer.

   var
     ExtPos: Integer;

   begin
     // First find the position of the extension string as substring in Buffer.
     ExtPos := Pos(Extension, Buffer);
     Result := ExtPos > 0;
     // Now check that it isn't only a substring of another extension.
     if Result then
       Result := ((ExtPos + Length(Extension) - 1) = Length(Buffer)) or
         not (Buffer[ExtPos + Length(Extension)] in ['_', 'A'..'Z', 'a'..'z']);
   end; 

  //--------------- end local function ----------------------------------------

begin
  // determine version of implementation
  // GL
  Buffer := glGetString(GL_VERSION); 
  TrimAndSplitVersionString(Buffer, Majorversion, MinorVersion); 
  GL_VERSION_1_0 := True; 
  GL_VERSION_1_1 := False; 
  GL_VERSION_1_2 := False; 
  if MajorVersion > 0 then
  begin
    if MinorVersion > 0 then
    begin
      GL_VERSION_1_1 := True; 
      if MinorVersion > 1 then
        GL_VERSION_1_2 := True;
    end; 
  end; 

  // GLU
  GLU_VERSION_1_1 := False; 
  GLU_VERSION_1_2 := False; 
  GLU_VERSION_1_3 := False; 
  // gluGetString is valid for version 1.1 or later
  if Assigned(gluGetString) then
  begin
    Buffer := gluGetString(GLU_VERSION); 
    TrimAndSplitVersionString(Buffer, Majorversion, MinorVersion); 
    GLU_VERSION_1_1 := True; 
    if MinorVersion > 1 then
    begin
      GLU_VERSION_1_2 := True; 
      if MinorVersion > 2 then
        GLU_VERSION_1_3 := True; 
    end; 
  end; 

  // check supported extensions
  // GL
  Buffer := glGetString(GL_EXTENSIONS); 
  GL_3DFX_multisample :=CheckExtension('GL_3DFX_multisample');
  GL_3DFX_tbuffer := CheckExtension('GL_3DFX_tbuffer'); 
  GL_3DFX_texture_compression_FXT1 := CheckExtension('GL_3DFX_texture_compression_FXT1'); 

  GL_APPLE_specular_vector := CheckExtension('GL_APPLE_specular_vector'); 
  GL_APPLE_transform_hint := CheckExtension('GL_APPLE_transform_hint'); 

  GL_ARB_imaging := CheckExtension('GL_ARB_imaging'); 
  GL_ARB_multisample := CheckExtension('GL_ARB_multisample'); 
  GL_ARB_multitexture := CheckExtension('GL_ARB_multitexture'); 
  GL_ARB_texture_compression := CheckExtension('GL_ARB_texture_compression'); 
  GL_ARB_texture_cube_map := CheckExtension('GL_ARB_texture_cube_map'); 
  GL_ARB_transpose_matrix := CheckExtension('GL_ARB_transpose_matrix'); 
  GL_ARB_vertex_blend := CheckExtension('GL_ARB_vertex_blend'); 

  GL_EXT_422_pixels := CheckExtension('GL_EXT_422_pixels'); 
  GL_EXT_abgr := CheckExtension('GL_EXT_abgr'); 
  GL_EXT_bgra := CheckExtension('GL_EXT_bgra'); 
  GL_EXT_blend_color := CheckExtension('GL_EXT_blend_color'); 
  GL_EXT_blend_func_separate := CheckExtension('GL_EXT_blend_func_separate'); 
  GL_EXT_blend_logic_op := CheckExtension('GL_EXT_blend_logic_op'); 
  GL_EXT_blend_minmax := CheckExtension('GL_EXT_blend_minmax'); 
  GL_EXT_blend_subtract := CheckExtension('GL_EXT_blend_subtract'); 
  GL_EXT_clip_volume_hint := CheckExtension('GL_EXT_clip_volume_hint'); 
  GL_EXT_cmyka := CheckExtension('GL_EXT_cmyka'); 
  GL_EXT_color_subtable := CheckExtension('GL_EXT_color_subtable');
  GL_EXT_compiled_vertex_array := CheckExtension('GL_EXT_compiled_vertex_array'); 
  GL_EXT_convolution := CheckExtension('GL_EXT_convolution'); 
  GL_EXT_coordinate_frame := CheckExtension('GL_EXT_coordinate_frame'); 
  GL_EXT_copy_texture := CheckExtension('GL_EXT_copy_texture'); 
  GL_EXT_cull_vertex := CheckExtension('GL_EXT_cull_vertex'); 
  GL_EXT_draw_range_elements := CheckExtension('GL_EXT_draw_range_elements'); 
  GL_EXT_fog_coord := CheckExtension('GL_EXT_fog_coord'); 
  GL_EXT_histogram := CheckExtension('GL_EXT_histogram'); 
  GL_EXT_index_array_formats := CheckExtension('GL_EXT_index_array_formats'); 
  GL_EXT_index_func := CheckExtension('GL_EXT_index_func'); 
  GL_EXT_index_material := CheckExtension('GL_EXT_index_material'); 
  GL_EXT_index_texture := CheckExtension('GL_EXT_index_texture'); 
  GL_EXT_light_max_exponent := CheckExtension('GL_EXT_light_max_exponent'); 
  GL_EXT_light_texture := CheckExtension('GL_EXT_light_texture'); 
  GL_EXT_misc_attribute := CheckExtension('GL_EXT_misc_attribute'); 
  GL_EXT_multi_draw_arrays := CheckExtension('GL_EXT_multi_draw_arrays'); 
  GL_EXT_multisample := CheckExtension('GL_EXT_multisample'); 
  GL_EXT_packed_pixels := CheckExtension('GL_EXT_packed_pixels'); 
  GL_EXT_paletted_texture := CheckExtension('GL_EXT_paletted_texture'); 
  GL_EXT_pixel_transform := CheckExtension('GL_EXT_pixel_transform'); 
  GL_EXT_point_parameters := CheckExtension('GL_EXT_point_parameters'); 
  GL_EXT_polygon_offset := CheckExtension('GL_EXT_polygon_offset'); 
  GL_EXT_rescale_normal := CheckExtension('GL_EXT_rescale_normal'); 
  GL_EXT_scene_marker := CheckExtension('GL_EXT_scene_marker'); 
  GL_EXT_secondary_color := CheckExtension('GL_EXT_secondary_color');
  GL_EXT_separate_specular_color := CheckExtension('GL_EXT_separate_specular_color'); 
  GL_EXT_shared_texture_palette := CheckExtension('GL_EXT_shared_texture_palette'); 
  GL_EXT_stencil_wrap := CheckExtension('GL_EXT_stencil_wrap'); 
  GL_EXT_subtexture := CheckExtension('GL_EXT_subtexture'); 
  GL_EXT_texture_color_table := CheckExtension('GL_EXT_texture_color_table'); 
  GL_EXT_texture_compression_s3tc := CheckExtension('GL_EXT_texture_compression_s3tc'); 
  GL_EXT_texture_cube_map := CheckExtension('GL_EXT_texture_cube_map'); 
  GL_EXT_texture_edge_clamp := CheckExtension('GL_EXT_texture_edge_clamp'); 
  GL_EXT_texture_env_add := CheckExtension('GL_EXT_texture_env_add'); 
  GL_EXT_texture_env_combine := CheckExtension('GL_EXT_texture_env_combine'); 
  GL_EXT_texture_filter_anisotropic := CheckExtension('GL_EXT_texture_filter_anisotropic'); 
  GL_EXT_texture_lod_bias := CheckExtension('GL_EXT_texture_lod_bias'); 
  GL_EXT_texture_object := CheckExtension('GL_EXT_texture_object'); 
  GL_EXT_texture_perturb_normal := CheckExtension('GL_EXT_texture_perturb_normal'); 
  GL_EXT_texture3D := CheckExtension('GL_EXT_texture3D'); 
  GL_EXT_vertex_array := CheckExtension('GL_EXT_vertex_array'); 
  GL_EXT_vertex_weighting := CheckExtension('GL_EXT_vertex_weighting'); 

  GL_FfdMaskSGIX := CheckExtension('GL_FfdMaskSGIX'); 
  GL_HP_convolution_border_modes := CheckExtension('GL_HP_convolution_border_modes'); 
  GL_HP_image_transform := CheckExtension('GL_HP_image_transform'); 
  GL_HP_occlusion_test := CheckExtension('GL_HP_occlusion_test'); 
  GL_HP_texture_lighting := CheckExtension('GL_HP_texture_lighting'); 

  GL_IBM_cull_vertex := CheckExtension('GL_IBM_cull_vertex');
  GL_IBM_multimode_draw_arrays := CheckExtension('GL_IBM_multimode_draw_arrays'); 
  GL_IBM_rasterpos_clip := CheckExtension('GL_IBM_rasterpos_clip'); 
  GL_IBM_vertex_array_lists := CheckExtension('GL_IBM_vertex_array_lists'); 

  GL_INGR_color_clamp := CheckExtension('GL_INGR_color_clamp'); 
  GL_INGR_interlace_read := CheckExtension('GL_INGR_interlace_read'); 

  GL_INTEL_parallel_arrays := CheckExtension('GL_INTEL_parallel_arrays'); 

  GL_KTX_buffer_region := CheckExtension('GL_KTX_buffer_region'); 

  GL_MESA_resize_buffers := CheckExtension('GL_MESA_resize_buffers'); 
  GL_MESA_window_pos := CheckExtension('GL_MESA_window_pos'); 

  GL_NV_blend_square := CheckExtension('GL_NV_blend_square'); 
  GL_NV_fog_distance := CheckExtension('GL_NV_fog_distance'); 
  GL_NV_light_max_exponent := CheckExtension('GL_NV_light_max_exponent'); 
  GL_NV_register_combiners := CheckExtension('GL_NV_register_combiners'); 
  GL_NV_texgen_emboss := CheckExtension('GL_NV_texgen_emboss'); 
  GL_NV_texgen_reflection := CheckExtension('GL_NV_texgen_reflection'); 
  GL_NV_texture_env_combine4 := CheckExtension('GL_NV_texture_env_combine4'); 
  GL_NV_vertex_array_range := CheckExtension('GL_NV_vertex_array_range'); 
  GL_NV_vertex_program := CheckExtension('GL_NV_vertex_program'); 

  GL_PGI_misc_hints := CheckExtension('GL_PGI_misc_hints');
  GL_PGI_vertex_hints := CheckExtension('GL_PGI_vertex_hints'); 

  GL_REND_screen_coordinates := CheckExtension('GL_REND_screen_coordinates'); 

  GL_SGI_color_matrix := CheckExtension('GL_SGI_color_matrix'); 
  GL_SGI_color_table := CheckExtension('GL_SGI_color_table'); 
  GL_SGI_depth_pass_instrument := CheckExtension('GL_SGI_depth_pass_instrument'); 

  GL_SGIS_detail_texture := CheckExtension('GL_SGIS_detail_texture'); 
  GL_SGIS_fog_function := CheckExtension('GL_SGIS_fog_function'); 
  GL_SGIS_generate_mipmap := CheckExtension('GL_SGIS_generate_mipmap'); 
  GL_SGIS_multisample := CheckExtension('GL_SGIS_multisample'); 
  GL_SGIS_multitexture := CheckExtension('GL_SGIS_multitexture'); 
  GL_SGIS_pixel_texture := CheckExtension('GL_SGIS_pixel_texture'); 
  GL_SGIS_point_line_texgen := CheckExtension('GL_SGIS_point_line_texgen'); 
  GL_SGIS_point_parameters := CheckExtension('GL_SGIS_point_parameters'); 
  GL_SGIS_sharpen_texture := CheckExtension('GL_SGIS_sharpen_texture'); 
  GL_SGIS_texture_border_clamp := CheckExtension('GL_SGIS_texture_border_clamp'); 
  GL_SGIS_texture_color_mask := CheckExtension('GL_SGIS_texture_color_mask'); 
  GL_SGIS_texture_edge_clamp := CheckExtension('GL_SGIS_texture_edge_clamp'); 
  GL_SGIS_texture_filter4 := CheckExtension('GL_SGIS_texture_filter4'); 
  GL_SGIS_texture_lod := CheckExtension('GL_SGIS_texture_lod'); 
  GL_SGIS_texture_select := CheckExtension('GL_SGIS_texture_select'); 
  GL_SGIS_texture4D := CheckExtension('GL_SGIS_texture4D'); 

  GL_SGIX_async := CheckExtension('GL_SGIX_async'); 
  GL_SGIX_async_histogram := CheckExtension('GL_SGIX_async_histogram'); 
  GL_SGIX_async_pixel := CheckExtension('GL_SGIX_async_pixel'); 
  GL_SGIX_blend_alpha_minmax := CheckExtension('GL_SGIX_blend_alpha_minmax'); 
  GL_SGIX_calligraphic_fragment := CheckExtension('GL_SGIX_calligraphic_fragment'); 
  GL_SGIX_clipmap := CheckExtension('GL_SGIX_clipmap'); 
  GL_SGIX_convolution_accuracy := CheckExtension('GL_SGIX_convolution_accuracy'); 
  GL_SGIX_depth_texture := CheckExtension('GL_SGIX_depth_texture'); 
  GL_SGIX_flush_raster := CheckExtension('GL_SGIX_flush_raster'); 
  GL_SGIX_fog_offset := CheckExtension('GL_SGIX_fog_offset'); 
  GL_SGIX_fog_scale := CheckExtension('GL_SGIX_fog_scale'); 
  GL_SGIX_fragment_lighting := CheckExtension('GL_SGIX_fragment_lighting'); 
  GL_SGIX_framezoom := CheckExtension('GL_SGIX_framezoom'); 
  GL_SGIX_igloo_interface := CheckExtension('GL_SGIX_igloo_interface'); 
  GL_SGIX_instruments := CheckExtension('GL_SGIX_instruments'); 
  GL_SGIX_interlace := CheckExtension('GL_SGIX_interlace'); 
  GL_SGIX_ir_instrument1 := CheckExtension('GL_SGIX_ir_instrument1'); 
  GL_SGIX_list_priority := CheckExtension('GL_SGIX_list_priority'); 
  GL_SGIX_pixel_texture := CheckExtension('GL_SGIX_pixel_texture'); 
  GL_SGIX_pixel_tiles := CheckExtension('GL_SGIX_pixel_tiles'); 
  GL_SGIX_polynomial_ffd := CheckExtension('GL_SGIX_polynomial_ffd'); 
  GL_SGIX_reference_plane := CheckExtension('GL_SGIX_reference_plane'); 
  GL_SGIX_resample := CheckExtension('GL_SGIX_resample'); 
  GL_SGIX_shadow := CheckExtension('GL_SGIX_shadow'); 
  GL_SGIX_shadow_ambient := CheckExtension('GL_SGIX_shadow_ambient');
  GL_SGIX_sprite := CheckExtension('GL_SGIX_sprite'); 
  GL_SGIX_subsample := CheckExtension('GL_SGIX_subsample'); 
  GL_SGIX_tag_sample_buffer := CheckExtension('GL_SGIX_tag_sample_buffer'); 
  GL_SGIX_texture_add_env := CheckExtension('GL_SGIX_texture_add_env'); 
  GL_SGIX_texture_lod_bias := CheckExtension('GL_SGIX_texture_lod_bias'); 
  GL_SGIX_texture_multi_buffer := CheckExtension('GL_SGIX_texture_multi_buffer'); 
  GL_SGIX_texture_scale_bias := CheckExtension('GL_SGIX_texture_scale_bias'); 
  GL_SGIX_vertex_preclip := CheckExtension('GL_SGIX_vertex_preclip'); 
  GL_SGIX_ycrcb := CheckExtension('GL_SGIX_ycrcb'); 
  GL_SGIX_ycrcba := CheckExtension('GL_SGIX_ycrcba'); 

  GL_SUN_convolution_border_modes := CheckExtension('GL_SUN_convolution_border_modes'); 
  GL_SUN_global_alpha := CheckExtension('GL_SUN_global_alpha'); 
  GL_SUN_triangle_list := CheckExtension('GL_SUN_triangle_list'); 
  GL_SUN_vertex := CheckExtension('GL_SUN_vertex'); 

  GL_SUNX_constant_data := CheckExtension('GL_SUNX_constant_data'); 

  GL_WIN_phong_shading := CheckExtension('GL_WIN_phong_shading'); 
  GL_WIN_specular_fog := CheckExtension('GL_WIN_specular_fog'); 
  GL_WIN_swap_hint := CheckExtension('GL_WIN_swap_hint'); 

  WGL_EXT_swap_control := CheckExtension('WGL_EXT_swap_control');
  WGL_ARB_extensions_string := CheckExtension('WGL_ARB_extensions_string');
  WGL_ARB_pixel_format := CheckExtension('WGL_ARB_pixel_format');

  // GLU
  Buffer := gluGetString(GLU_EXTENSIONS); 
  GLU_EXT_TEXTURE := CheckExtension('GLU_EXT_TEXTURE'); 
  GLU_EXT_object_space_tess := CheckExtension('GLU_EXT_object_space_tess'); 
  GLU_EXT_nurbs_tessellator := CheckExtension('GLU_EXT_nurbs_tessellator');

  // ARB wgl extensions
  if Assigned(wglGetExtensionsStringARB) then
  begin
    Buffer := wglGetExtensionsStringARB(wglGetCurrentDC);
    WGL_ARB_extensions_string := CheckExtension('WGL_ARB_extensions_string');
    WGL_ARB_pixel_format := CheckExtension('WGL_ARB_pixel_format');
  end
  else
  begin
    WGL_ARB_extensions_string := False;
    WGL_ARB_pixel_format := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function SetupPalette(DC: HDC; PFD: TPixelFormatDescriptor): HPalette;

var
  nColors,
  I: Integer;
  LogPalette: TMaxLogPalette;
  RedMask,                                   
  GreenMask,
  BlueMask: Byte;
   
begin
  nColors := 1 shl Pfd.cColorBits; 
  LogPalette.palVersion := $300; 
  LogPalette.palNumEntries := nColors; 
  RedMask := (1 shl Pfd.cRedBits  ) - 1; 
  GreenMask := (1 shl Pfd.cGreenBits) - 1; 
  BlueMask := (1 shl Pfd.cBlueBits ) - 1;
  with LogPalette, PFD do
    for I := 0 to nColors - 1 do
    begin
      palPalEntry[I].peRed := (((I shr cRedShift  ) and RedMask  ) * 255) div RedMask; 
      palPalEntry[I].peGreen := (((I shr cGreenShift) and GreenMask) * 255) div GreenMask; 
      palPalEntry[I].peBlue := (((I shr cBlueShift ) and BlueMask ) * 255) div BlueMask; 
      palPalEntry[I].peFlags := 0; 
    end; 

  Result := CreatePalette(PLogPalette(@LogPalette)^); 
  if Result <> 0 then
  begin
    SelectPalette(DC, Result, False); 
    RealizePalette(DC); 
  end
  else
    RaiseLastOSError; 
end; 

//----------------------------------------------------------------------------------------------------------------------

function CreateRenderingContext(DC: HDC; Options: TRCOptions; ColorBits, StencilBits, AccumBits, AuxBuffers: Integer; 
  Layer: Integer; var Palette: HPALETTE): HGLRC; 

// Set the OpenGL properties required to draw to the given canvas and create a rendering context for it.

const
  MemoryDCs = [OBJ_MEMDC, OBJ_METADC, OBJ_ENHMETADC]; 

var
  PFDescriptor: TPixelFormatDescriptor; 
  PixelFormat: Integer; 
  AType: DWORD; 

begin
  FillChar(PFDescriptor, SizeOf(PFDescriptor), 0); 
  with PFDescriptor do
  begin
    nSize := SizeOf(PFDescriptor); 
    nVersion := 1; 
    dwFlags := PFD_SUPPORT_OPENGL; 
    AType := GetObjectType(DC); 
    if AType = 0 then
      RaiseLastOSError; 
      
    if AType in MemoryDCs then
      dwFlags := dwFlags or PFD_DRAW_TO_BITMAP
    else
      dwFlags := dwFlags or PFD_DRAW_TO_WINDOW; 
    if opDoubleBuffered in Options then
      dwFlags := dwFlags or PFD_DOUBLEBUFFER; 
    if opGDI in Options then
      dwFlags := dwFlags or PFD_SUPPORT_GDI; 
    if opStereo in Options then
      dwFlags := dwFlags or PFD_STEREO; 
    iPixelType := PFD_TYPE_RGBA; 
    cColorBits := ColorBits; 
    cDepthBits := 32; 
    cStencilBits := StencilBits; 
    cAccumBits := AccumBits; 
    cAuxBuffers := AuxBuffers; 
    if Layer = 0 then
      iLayerType := PFD_MAIN_PLANE
    else
      if Layer > 0 then
        iLayerType := PFD_OVERLAY_PLANE
      else
        iLayerType := Byte(PFD_UNDERLAY_PLANE); 
  end; 

  // Just in case it didn't happen already.
  if not InitOpenGL then
    RaiseLastOSError; 
  PixelFormat := ChoosePixelFormat(DC, {$ifndef FPC}@{$endif}PFDescriptor); 
  if PixelFormat = 0 then
    RaiseLastOSError; 

  // NOTE: It is not allowed to change a pixel format of a device context once it has been set.
  //       Hence you may create more than one rendering context for one single device only if it
  //       uses the same pixel format as the first created RC.
  if GetPixelFormat(DC) <> PixelFormat then
  begin
    if not SetPixelFormat(DC, PixelFormat, @PFDescriptor) then
      RaiseLastOSError; 
  end; 

  // Check the properties we just set.
  DescribePixelFormat(DC, PixelFormat, SizeOf(PFDescriptor), PFDescriptor); 
  with PFDescriptor do
    if (dwFlags and PFD_NEED_PALETTE) <> 0 then
      Palette := SetupPalette(DC, PFDescriptor)
    else
      Palette := 0; 
    
  Result := wglCreateLayerContext(DC, Layer);
  if Result = 0 then
    RaiseLastOSError
  else
    LastPixelFormat := 0; 
end; 

//----------------------------------------------------------------------------------------------------------------------

procedure ActivateRenderingContext(DC: HDC; RC: HGLRC); 

var
  PixelFormat: Integer; 

begin
  Assert((DC <> 0), 'DC must not be 0'); 
  Assert((RC <> 0), 'RC must not be 0'); 

  if ActivationRefCount = 0 then
  begin
    // Before activating the rendering context check if it is not already used by another thread.
    with ContextList.LockList do
    try
      if IndexOf(Pointer(RC)) = -1 then
      begin
        if wglMakeCurrent(DC, RC) then
          Add(Pointer(RC))
        else
          ShowError(SMakeCurrentFailed); 
      end
      else
        ShowError(SRCAlreadyActive)
    finally
      ContextList.UnlockList; 
    end; 

    Inc(ActivationRefCount); 

    // The extension function addresses are unique for each pixel format. All rendering
    // contexts of a given pixel format share the same extension function addresses.
    PixelFormat := GetPixelFormat(DC); 
    if PixelFormat <> LastPixelFormat then
    begin
      ReadExtensions; 
      ReadImplementationProperties; 
      LastPixelFormat := PixelFormat;
    end; 
  end
  else
  begin
    Assert((wglGetCurrentDC = DC) and (wglGetCurrentContext = RC), 'Incoherent DC/RC pair.'); 
    Inc(ActivationRefCount); 
  end; 
end; 

//----------------------------------------------------------------------------------------------------------------------

procedure DeactivateRenderingContext; 

begin
  Assert(ActivationRefCount > 0, 'Unbalanced deactivation.'); 
  if ActivationRefCount > 0 then
  begin
    Dec(ActivationRefCount); 

    if ActivationRefCount = 0 then
    begin
      // If the rendering context is no longer used then remove it from the context list to indicate
      // it can now be used in any thread.
      with ContextList.LockList do
      try
        Remove(Pointer(wglGetCurrentContext));
        if not wglMakeCurrent(0, 0) then
          ShowError(SMakeCurrentFailed);
      finally
        ContextList.UnlockList;
      end;
    end;
  end; 
end; 

//----------------------------------------------------------------------------------------------------------------------

procedure DestroyRenderingContext(RC: HGLRC); 

// Used to destroy the given rendering context. Only contexts which are no longer in use by any thread can be deleted.

begin
  Assert((ActivationRefCount = 0), 'Active contexts cannot be deleted.'); 
  
  with ContextList.LockList do
  try
    if not wglDeleteContext(RC) then
      ShowError(SDeleteContextFailed);
    if IndexOf(Pointer(RC)) > -1 then
      ShowError(SContextInUse);
  finally
    ContextList.UnlockList;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function CurrentDC: HDC; 

// Returns the device context which is used for the current rendering context of the caller thread.

begin
  Result := wglGetCurrentDC; 
end;

{$endif}

//----------------------------------------------------------------------------------------------------------------------

procedure CloseOpenGL; 
begin
  if GLHandle <> INVALID_MODULEHANDLE then
  begin
    UnloadModule( GLHandle );
    GLHandle := INVALID_MODULEHANDLE;
  end;

  if GLUHandle <> INVALID_MODULEHANDLE then
  begin
    UnloadModule( GLUHandle );
    GLUHandle := INVALID_MODULEHANDLE;
  end;

  ClearProcAddresses;
  ClearExtensions;
end;

//----------------------------------------------------------------------------------------------------------------------

function InitOpenGL: Boolean;

begin
  if (GLHandle = INVALID_MODULEHANDLE) or (GLUHandle = INVALID_MODULEHANDLE) then
    Result := InitOpenGLFromLibrary(PChar( SDefaultGLLibrary ), PChar( SDefaultGLULibrary ) )
  else
    Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function InitOpenGLFromLibrary(GLName, GLUName: PChar): Boolean;

begin
  Result := False; 
  CloseOpenGL;

  LoadModule( GLHandle, GLName );
  LoadModule( GLUHandle, GLUName );

  if (GLHandle <> INVALID_MODULEHANDLE) and (GLUHandle <> INVALID_MODULEHANDLE) then
  begin
    LoadProcAddresses;
    Result := True; 
  end
  else
  begin
    if GLHandle <>  INVALID_MODULEHANDLE then
      UnloadModule( GLHandle );

    if GLUHandle <>  INVALID_MODULEHANDLE then
      UnloadModule( GLUHandle );
  end; 
end; 

//----------------------------------------------------------------------------------------------------------------------

function IsOpenGLInitialized: Boolean;

begin
  Result := GLHandle <> INVALID_MODULEHANDLE; 
end; 

//----------------------------------------------------------------------------------------------------------------------

procedure UnloadOpenGL;

// compatibility routine

begin
  CloseOpenGL; 
end; 

//----------------------------------------------------------------------------------------------------------------------

function LoadOpenGL: Boolean;

// compatibility routine

begin
  Result := InitOpenGL; 
end; 

//----------------------------------------------------------------------------------------------------------------------

function LoadOpenGLFromLibrary(GLName, GLUName: PChar): Boolean; 

// compatibility routine

begin
  Result := InitOpenGLFromLibrary(GLName, GLUName);
end;

//----------------------------------------------------------------------------------------------------------------------

function IsOpenGLLoaded: Boolean;

// compatibility routine

begin
  Result := GLHandle <> INVALID_MODULEHANDLE; 
end; 

//----------------------------------------------------------------------------------------------------------------------
{$ifdef FPC}
const Default8087CW: Word = $1332;

{$ASMMODE INTEL}
procedure Set8087CW(NewCW: Word); Assembler;
asm
  MOV Default8087CW, AX
end;
{$endif}

//----------------------------------------------------------------------------------------------------------------------

initialization
  ContextList := TThreadList.Create; 
  Set8087CW($133F); 
finalization
  CloseOpenGL; 
  ContextList.Free; 
  // We don't need to reset the FPU control word as the previous set call is process specific.
end.
