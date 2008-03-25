unit OpenGL12;

{$I switches.inc}

interface

uses Windows;

const
  glu32 = 'OpenGL';

type

  TRCOptions = set of (
    opDoubleBuffered,
    opGDI,
    opStereo
  );

  UINT = Cardinal;

  {$EXTERNALSYM GLenum}
  GLenum      = UINT;
  TGLenum     = UINT;
  PGLenum     = ^TGLenum;

  {$EXTERNALSYM GLboolean}
  GLboolean   = BYTEBOOL;
  TGLboolean  = BYTEBOOL;
  PGLboolean  = ^TGLboolean;

  {$EXTERNALSYM GLbitfield}
  GLbitfield  = UINT;
  TGLbitfield = UINT;
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
  GLuint      = UINT;
  TGLuint     = UINT;
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

{$ifdef FPC}
  var
{$else}
  {$ifdef MULTITHREADOPENGL}
    threadvar
  {$else}
    var
  {$endif}
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
  GL_FALSE                                          = Boolean(0);
  {$EXTERNALSYM GL_FALSE}
  GL_TRUE                                           = Boolean(1);
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
  GL_CLIP_VOLUME_CLIPPING_HINT_EXT                  = $80F0;
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

  {$ifdef FPC}
  PMaxLogPalette = ^TMaxLogPalette;
  TMaxLogPalette = packed record
    palversion : word;
    palnumentries : word;
    palpalentry : array[byte] of TPaletteEntry;
  end;
  {$endif}

  {$ifdef VER100, Delphi 3 compatibility}
  PWGLSwap = ^TWGLSwap;
  {$EXTERNALSYM _WGLSWAP}
  _WGLSWAP = packed record
    hdc: HDC;
    uiFlags: UINT;
  end;
  TWGLSwap = _WGLSWAP;
  {$EXTERNALSYM WGLSWAP}
  WGLSWAP = _WGLSWAP;
  {$endif VER100}

  // Callback function prototypes
  // GLUQuadricCallback
  TGLUQuadricErrorProc = procedure(errorCode: TGLEnum); {$ifdef MSWINDOWS} stdcall; {$endif} {$ifdef LINUX} cdecl; {$endif}

  // GLUTessCallback
  TGLUTessBeginProc = procedure(AType: TGLEnum); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF}
  TGLUTessEdgeFlagProc = procedure(Flag: TGLboolean); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF}
  TGLUTessVertexProc = procedure(VertexData: Pointer); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF}
  TGLUTessEndProc = procedure; {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF}
  TGLUTessErrorProc = procedure(ErrNo: TGLEnum); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF}
  TGLUTessCombineProc = procedure(Coords: TVector3d; VertexData: TVector4p; Weight: TVector4f; OutData: PPointer); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF}
  TGLUTessBeginDataProc = procedure(AType: TGLEnum; UserData: Pointer); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF}
  TGLUTessEdgeFlagDataProc = procedure(Flag: TGLboolean; UserData: Pointer); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF}
  TGLUTessVertexDataProc = procedure(VertexData: Pointer; UserData: Pointer); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF}
  TGLUTessEndDataProc = procedure(UserData: Pointer); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF}
  TGLUTessErrorDataProc = procedure(ErrNo: TGLEnum; UserData: Pointer); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF}
  TGLUTessCombineDataProc = procedure(Coords: TVector3d; VertexData: TVector4p; Weight: TVector4f; OutData: PPointer; UserData: Pointer); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF}

  // GLUNurbsCallback
  TGLUNurbsErrorProc = procedure(ErrorCode: TGLEnum); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF}

  // GL functions
  procedure glBindTexture(target: TGLEnum; texture: TGLuint); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glTexParameteri(target, pname: TGLEnum; param: TGLint); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glTexImage2D(target: TGLEnum; level, internalformat: TGLint; width, height: TGLsizei; border: TGLint; format, atype: TGLEnum; Pixels:Pointer); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glDeleteTextures(n: TGLsizei; textures: PGLuint); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glGenTextures(n: GLsizei; textures: PGLuint); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glColor3f(red, green, blue: TGLfloat); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glBegin(mode: TGLEnum); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glEnd; {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glVertex2f(x, y: TGLfloat); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glColor4f(red, green, blue, alpha: TGLfloat); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glEnable(cap: TGLEnum); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glDisable(cap: TGLEnum); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glDepthRange(zNear, zFar: TGLclampd); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glDepthFunc(func: TGLEnum); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glBlendFunc(sfactor: TGLEnum; dfactor: TGLEnum); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glTexCoord2f(s, t: TGLfloat); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glVertex3f(x, y, z: TGLfloat); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glClearColor(red, green, blue, alpha: TGLclampf); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glClear(mask: TGLbitfield); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glMatrixMode(mode: TGLEnum); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glLoadIdentity; {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glOrtho(left, right, bottom, top, zNear, zFar: TGLdouble); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glViewport(x, y: TGLint; width, height: TGLsizei); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glReadPixels(x, y: TGLint; width, height: TGLsizei; format, atype: TGLEnum; pixels: Pointer); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glPixelStorei(pname: TGLEnum; param: TGLint); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  function  glGetError:TGLuint; {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glTexSubImage2D(target: TGLEnum; level, xoffset, yoffset: TGLint; width, height: TGLsizei; format, atype: TGLEnum; pixels: Pointer); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure gluOrtho2D(left, right, bottom, top: TGLdouble); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glPushMatrix; {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glPopMatrix; {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glFrustum(left, right, bottom, top, zNear, zFar: TGLdouble); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glTranslatef(x, y, z: TGLfloat); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glRotatef(angle, x, y, z: TGLfloat);  {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glScalef(x, y, z: TGLfloat); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glTexParameterf(target, pname: TGLEnum; param: TGLfloat); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glCopyTexImage2D(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width, height: TGLsizei; border: TGLint); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glGetIntegerv(pname: TGLEnum; params: PGLint); {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;
  procedure glFinish; {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external opengl32;

  // GLU functions
  function gluBuild2DMipmaps(target: TGLEnum; components, width, height: TGLint; format, atype: TGLEnum; Data: Pointer): TGLint; {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external glu32;
  function gluScaleImage(format: TGLEnum; widthin, heightin: TGLint; typein: TGLEnum; datain: Pointer; widthout, heightout: TGLint; typeout: TGLEnum; dataout: Pointer): TGLint; {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external glu32;
  function gluErrorString(errCode : GLenum) : PChar;  {$IFDEF DLL_STDCALL} stdcall; {$ENDIF} {$IFDEF DLL_CDECL} cdecl; {$ENDIF} external glu32;

  Procedure LoadOpenGL;
  Procedure UnloadOpenGL;

implementation

Procedure LoadOpenGL;
begin
end;

Procedure UnloadOpenGL;
begin
end;

end.
