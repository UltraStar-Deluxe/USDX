{* UltraStar Deluxe - Karaoke Game
 *
 * UltraStar Deluxe is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *}

unit URenderer_OpenGL;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  dglOpenGL,
  matrix,
  sdl2,
  UPath,
  URenderer;

type
  TViewPortArray = array[0..3] of integer;

  // Abstract TRenderer base for OpenGL
  TRenderer_OpenGLBase = class(TRenderer)
    protected
      glcontext: TSDL_GLContext;
      MajorVersion, MinorVersion: integer;

      // Shader programs
      MainProgram: GLuint;
      TextProgram: GLuint;
      LineStripProgram: GLuint;

      // Vertex information
      MainVAO: GLuint;
      LineStripVAO: GLuint;
      VBO: GLuint;
      EBO: GLuint;
      VBOCursor: GLuint;
      EBOCursor: GLuint;

      // Transformation information
      ProjectionMatrix: Tmatrix4_single;
      OldProjectionMatrix: Tmatrix4_single;
      ViewPortArray: TViewPortArray;

      // Shader Uniform locations
      TransformLocationMain: GLint;
      TransformLocationText: GLint;
      TransformLocationLineStrip: GLint;
      ColorLocationLineStrip: GLint;
      UpdateTransformMain: boolean;
      UpdateTransformText: boolean;

      WhiteTexture: GLuint;
      ErrorCode: GLenum;
      SupportsVAO: boolean;
      SupportsFBO: boolean;

      procedure InitShaderPrograms();
      procedure InitBuffers();
      procedure BindMainVertexAttrib();
      procedure BindLineStripVertexAttrib();
      function LoadTexture(Data: PByte; W, H: integer; const Identifier: IPath; Typ: TTextureType): TTexture; overload; override;
      procedure DrawTexture(Texture: TTexture; Prog: GLuint; var UpdateTransform: boolean; TransformLocation: GLint); overload;
      function CreateEmptyTexture(const Identifier: IPath): TTexture; override;
      function GetArrayBuffer(var Bytes: GLuint): PGLfloat;
      procedure UpdateTransformationMatrix();
      procedure CheckVersion(); virtual; abstract;
      procedure GetShaderSource(out MainVertex, MainFragment, TextFragment, LineStripVertex, LineStripFragment: string); virtual; abstract;

      {$IFDEF DEBUG_MODE}
      procedure RaiseExceptionIfError();
      {$ENDIF}

    public
      constructor Create(glcontext: TSDL_GLContext; MajorVersion, MinorVersion: integer);
      destructor Destroy; override;
      function LoadGlyph(Data: PByte; W, H: integer): TTexture; overload; override;
      procedure DrawTexture(Texture: TTexture); override;
      procedure DrawGlyph(Texture: TTexture); override;
      procedure DrawQuads(QuadList: TQuadList); override;
      procedure DrawTriangles(TriangleList: TTriangleList); override;
      procedure DrawLines(LineList: TLineList); override;
      procedure DrawParticles(Texture: TTexture; ParticleList: TParticleList); override;
      procedure DrawLineStrip(PointList: TPointList; ScaleX, ScaleY, TranslateX, TranslateY, ColR, ColG, ColB, Alpha: single); override;
      procedure SetBlend(Enabled: boolean); override;
      function GetBlend(): boolean; override;
      procedure SetDepthTest(Enabled: boolean); override;
      function GetDepthTest(): boolean; override;
      procedure SetScissorRect(X, Y: integer; W, H: cardinal); override;
      procedure SetScissorTest(Scissor: boolean); override;
      function GetScissorTest(): boolean; override;
      procedure SetOrthographicProjection(Left, Right, Bottom, Top, NearVal, FarVal: single); override;
      procedure SetViewPort(x, y: integer; width, height: cardinal); override;
      procedure SetVSync(Enabled: boolean); override;
      function GetVSync(): boolean; override;
      procedure SwapBuffers(); override;
      function GetError(): boolean; override;
      function GetErrorCode(): integer; override;
      procedure SetClearColor(R, G, B, A: single); override;
      procedure ClearFrameBuffer(Buffers: cardinal); override;
      procedure SetTextClipBoundary(X: single; Direction: ClippingDirection); override;
      procedure SetClipText(Enabled: boolean); override;
      procedure ResetState(); override;
      function GetFrameBufferData(out RowSize: integer): PByte; override;

  end;

  // Renderer subclass for modern OpenGL (3.0 and later)
  TRenderer_OpenGL3 = class(TRenderer_OpenGLBase)
    public
      constructor Create(glcontext: TSDL_GLContext; MajorVersion, MinorVersion: integer);
      procedure CheckVersion() override;
      procedure GetShaderSource(out MainVertex, MainFragment, TextFragment, LineStripVertex, LineStripFragment: string); override;
    end;

  // Renderer subclass for legacy OpengGL 2.x
  TRenderer_OpenGL2 = class(TRenderer_OpenGLBase)
    public
      constructor Create(glcontext: TSDL_GLContext; MajorVersion, MinorVersion: integer);
      procedure CheckVersion() override;
      procedure GetShaderSource(out MainVertex, MainFragment, TextFragment, LineStripVertex, LineStripFragment: string); override;
    end;

  // Renderer subclass for OpenGL ES 2.0 and later
  TRenderer_OpenGLES = class(TRenderer_OpenGLBase)
    public
      constructor Create(glcontext: TSDL_GLContext; MajorVersion, MinorVersion: integer);
      procedure CheckVersion() override;
      procedure GetShaderSource(out MainVertex, MainFragment, TextFragment, LineStripVertex, LineStripFragment: string); override;
    end;


implementation

{$IFDEF FPC}
  {$POINTERMATH ON}
{$ENDIF}

uses
  Math,
  UGraphic,
  ULog,
  SysUtils;

const
  // Offsets for vertex attributes (indices, not bytes)
  X_OFFSET = 0;
  Y_OFFSET = 1;
  Z_OFFSET = 2;
  R_OFFSET = 3;
  G_OFFSET = 4;
  B_OFFSET = 5;
  A_OFFSET = 6;
  TEXX_OFFSET = 7;
  TEXY_OFFSET = 8;
  VERTEX_STRIDE = (TEXY_OFFSET - X_OFFSET + 1);

  VERTEX_STRIDE_BYTES = VERTEX_STRIDE * SizeOf(GLfloat);

  // Offsets for VBO (indices, not bytes)
  VERTEX_TOPRIGHT_ORDER = 0;
  VERTEX_BOTTOMRIGHT_ORDER = 1;
  VERTEX_BOTTOMLEFT_ORDER = 2;
  VERTEX_TOPLEFT_ORDER = 3;
  VERTEX_TOPRIGHT_OFFSET = VERTEX_TOPRIGHT_ORDER * VERTEX_STRIDE;
  VERTEX_BOTTOMRIGHT_OFFSET = VERTEX_BOTTOMRIGHT_ORDER * VERTEX_STRIDE;
  VERTEX_BOTTOMLEFT_OFFSET = VERTEX_BOTTOMLEFT_ORDER * VERTEX_STRIDE;
  VERTEX_TOPLEFT_OFFSET = VERTEX_TOPLEFT_ORDER * VERTEX_STRIDE;
  QUAD_STRIDE = VERTEX_STRIDE * 4;
  QUAD_STRIDE_BYTES = QUAD_STRIDE * SizeOf(GLfloat);
  TRIANGLE_STRIDE = VERTEX_STRIDE * 3;

  // Shader constants
  GLSL_CORE_HEADER = '#version 150 core' + #10; // GLSL Header for OpenGL 3.2+ core profile
  GLSL_COMPAT_HEADER = '#version 130' + #10;  // GLSL header for OpenGL 3.0-3.1
  GLSL_LEGACY_HEADER = '#version 110' + #10; // GLSL header for OpenGL 2.0
  ES_PRECISION_SPECIFIER = 'precision mediump float;' + #10; // GLSL ES shaders require a precision specifier

{
 * Different versions of OpenGL have different syntax for their shaders. To handle this,
 * we introduce abstract syntax in square brackets. When compiling the shaders, the subclass
 * will resolve the abstraction by substituting the syntax required by the specfic version of GLSL being used:
 *
 * [VTX_IN] resolves to 'in' for modern GL, and 'attribute' for legacy GL and GLES
 * [VTX_OUT] resolves to 'out' for modern GL, and 'varying' for legacy GL and GLES
 * [FRAG_IN] resoves to 'in' for modern GL, and 'varying' for legacy GL and GLES
 * [FRAG_OUT_DECL] declares the fragment shader output variable for modern GL, and is unused for legacy GL and GLES
 * [TEXTURE_FUNC] resolves to 'texture' for modern GL, and 'texture2D' for legacy GL and GLES
 * [FRAG_OUT] resolves to 'out_color' for modern GL, and the built-in variable 'gl_FragColor' for legacy GL and GLES
}

  // Main vertex shader source, use for drawing textures, quads, lines, triangles, glyphs
  MAIN_VERTEX_SHADER_SOURCE =
    '[VTX_IN] vec3 pos;                          ' + #10 +
    '[VTX_IN] vec4 color;                        ' + #10 +
    '[VTX_IN] vec2 tex_coords;                   ' + #10 +
    '[VTX_OUT] vec4 col;                         ' + #10 +
    '[VTX_OUT] vec2 tc;                          ' + #10 +
    'uniform mat4 transform;                     ' + #10 +
    'void main()                                 ' + #10 +
    '{                                           ' + #10 +
	  '  gl_Position = transform * vec4(pos, 1.0); ' + #10 +
	  '  col = color;                              ' + #10 +
	  '  tc = tex_coords;                          ' + #10 +
    '}';

  // Main fragment shader source, use for drawing textures, quads, lines, triangles
  MAIN_FRAGMENT_SHADER_SOURCE =
    '[FRAG_IN] vec4 col;                            ' + #10 +
    '[FRAG_IN] vec2 tc;                             ' + #10 +
    '[FRAG_OUT_DECL]                                ' + #10 +
    'uniform sampler2D tex;                         ' + #10 +
    'void main()                                    ' + #10 +
    '{                                              ' + #10 +
    '  [FRAG_OUT] = [TEXTURE_FUNC](tex, tc) * col;  ' + #10 +
    '}';

  // The text rendering requires a special fragment shader because the texture provided by FreeType
  // is a 1 byte per pixel alpha map stored in the red channel. So we set the red value from the texture
  // to the alpha, and use the vertex color for RGB
  TEXT_FRAGMENT_SHADER_SOURCE =
    '[FRAG_IN] vec4 col;                                             ' + #10 +
    '[FRAG_IN] vec2 tc;                                              ' + #10 +
    '[FRAG_OUT_DECL]                                                 ' + #10 +
    'uniform sampler2D tex;                                          ' + #10 +
    'void main()                                                     ' + #10 +
    '{                                                               ' + #10 +
    '  [FRAG_OUT] = vec4(col.rgb, [TEXTURE_FUNC](tex, tc).r * col.a);' + #10 +
    '}';

  // Special vertex shader used to render line strips for the oscilloscope
  LINE_STRIP_VERTEX_SHADER_SOURCE =
    '[VTX_IN] vec2 pos;                               ' + #10 +
    'uniform mat4 transform;                          ' + #10 +
    'void main()                                      ' + #10 +
    '{                                                ' + #10 +
	  '  gl_Position = transform * vec4(pos, 0.0, 1.0); ' + #10 +
    '}';

  // Special fragment shader used to render line strips for the oscilloscope
  LINE_STRIP_FRAGMENT_SHADER_SOURCE =
    '[FRAG_OUT_DECL]        ' + #10 +
    'uniform vec4 color;    ' + #10 +
    'void main()            ' + #10 +
    '{                      ' + #10 +
    '  [FRAG_OUT] = color;  ' + #10 +
    '}';

  VBO_SIZE = (2 * 2 shl 20); // 2 MB
  MAX_QUADS = (VBO_SIZE div QUAD_STRIDE_BYTES); // Number of quads that can be stored in the VBO
  EBO_INDICES = MAX_QUADS * 6; // 2 triangles, 6 total vertices per quad
  EBO_SIZE = EBO_INDICES * SizeOf(GLuint);

type
  TTexture_OpenGL = class(TTexture)
    protected
      TexID: GLuint;

    public
      constructor Create(Data: PByte; W, H: integer; const Identifier: IPath; Format: GLint; Alignment: GLint; WrapMode: GLint); overload;
      constructor Create(const Identifier: IPath); overload;
      destructor Destroy; override;
      procedure UpdateData(Data: PByte; Width, Height: word; PixelsPerRow: integer; Typ: TTextureType); override;
      procedure CopyFrameBuffer(X, Y: integer; Width, Height: cardinal); override;
      procedure Release(); override;
      function Clone(): TTexture; overload; override;
  end;


constructor TTexture_OpenGL.Create(Data: PByte; W, H: integer; const Identifier: IPath; Format: GLint; Alignment: GLint; WrapMode: GLint);
begin
  inherited Create(Identifier);
  self.W := W;
  self.H := H;

  // prepare OpenGL texture
  glGenTextures(1, @TexID);
  glBindTexture(GL_TEXTURE_2D, TexID);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, WrapMode);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, WrapMode);
  glPixelStorei(GL_UNPACK_ALIGNMENT, Alignment);

  // load data into gl texture
  glTexImage2D(GL_TEXTURE_2D, 0, Format, W, H, 0, Format, GL_UNSIGNED_BYTE, Data);
  fIsEmpty := false;
  {$IFDEF DEBUG_MODE}
  TRenderer_OpenGLBase(Renderer).RaiseExceptionIfError;
  {$ENDIF}
end;

// Empty texture constructor
constructor TTexture_OpenGL.Create(const Identifier: IPath);
begin
  inherited Create(Identifier);
  W := 0;
  H := 0;
  TexID := 0;
end;

destructor TTexture_OpenGL.Destroy();
begin
  if (OwnsTex) then
    glDeleteTextures(1, @TexID);
  inherited;
end;

procedure TTexture_OpenGL.UpdateData(Data: PByte; Width, Height: word; PixelsPerRow: integer; Typ: TTextureType);
begin
  glBindTexture(GL_TEXTURE_2D, TexID);
  glPixelStorei(GL_UNPACK_ROW_LENGTH, PixelsPerRow);
  if (Typ = TEXTURE_TYPE_PLAIN) then
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, Width, Height, GL_RGB, GL_UNSIGNED_BYTE, Data)
  else
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, Data);
  glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
  fIsEmpty := false;

  {$IFDEF DEBUG_MODE}
  TRenderer_OpenGLBase(Renderer).RaiseExceptionIfError;
  {$ENDIF}
end;

// Copy the current framebuffer contents to the texture (used for screen fades)
procedure TTexture_OpenGL.CopyFrameBuffer(X, Y: integer; Width, Height: cardinal);
begin
  glBindTexture(GL_TEXTURE_2D, TexID);
  glCopyTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, X, Y, Width, Height);
  {$IFDEF DEBUG_MODE}
  TRenderer_OpenGLBase(Renderer).RaiseExceptionIfError;
  {$ENDIF}
end;

// Delete the corresponding GPU texture
procedure TTexture_OpenGL.Release();
begin
  if (OwnsTex) then
  begin
    glDeleteTextures(1, @TexID);
    TexID := 0;
  end;
  fIsEmpty := true;
  {$IFDEF DEBUG_MODE}
  TRenderer_OpenGLBase(Renderer).RaiseExceptionIfError;
  {$ENDIF}
end;

function TTexture_OpenGL.Clone(): TTexture;
var
  T: TTexture_OpenGL;
begin
  T := TTexture_OpenGL.Create(Name);
  T.TexID := TexID;
  Clone(T);
  Result := T;
end;

constructor TRenderer_OpenGLBase.Create(glcontext: TSDL_GLContext; MajorVersion, MinorVersion: integer);
var
  WhitePixel: array[0..3] of GLubyte = (255, 255, 255, 255);
  S: string;
begin
  inherited Create();
  self.glcontext := glcontext;
  self.MajorVersion := MajorVersion;
  self.MinorVersion := MinorVersion;
  VBOCursor := 0;
  EBOCursor := 0;
  ProjectionMatrix.init_zero;
  InitOpenGL;
  ReadOpenGLCore;
  ReadCoreVersion;

  S := glGetString(GL_RENDERER);
  Log.LogInfo('OpenGL vendor ' + glGetString(GL_VENDOR), 'TRenderer_OpenGLBase.Create');
  Log.LogInfo('OpenGL renderer ' + S, 'TRenderer_OpenGLBase.Create');
  Log.LogInfo('OpenGL version ' + glGetString(GL_VERSION), 'TRenderer_OpenGLBase.Create');

  if (Pos('GDI Generic', S) > 0) or // Microsoft
     (Pos('Software Renderer', S) > 0) or // Apple
     (Pos('Software Rasterizer', S) > 0) or // Mesa (-Ddri-drivers=swrast)
     (Pos('softpipe', S) > 0) or // Mesa (-Dgallium-drivers=swrast -Dllvm=false)
     (Pos('llvmpipe', S) > 0) or // Mesa (-Dgallium-drivers=swrast -Dllvm=true)
     (Pos('SWR', S) > 0) or // Mesa (-Dgallium-drivers=swr)
     (Pos('Mesa X11', S) > 0) or // Mesa (-Dglx=xlib)
     (Pos('SwiftShader', S) > 0) then // Google; OpenGL ES, D3D9 & Vulkan only so far, but who knows...
    SWRendering := true
  else
    SWRendering := false;

  // Basic setup
  CheckVersion;
  InitShaderPrograms;
  InitBuffers;

  // Initialize state
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glDepthFunc(GL_LEQUAL);
  glDepthRangef(0, 10);
  glUseProgram(MainProgram);
  glActiveTexture(GL_TEXTURE0);

  // Get uniform locations in shader programs
  glUniform1i(glGetUniformLocation(MainProgram, 'tex'), 0);
  TransformLocationMain := glGetUniformLocation(MainProgram, 'transform');
  glUniform1i(glGetUniformLocation(TextProgram, 'tex'), 0);
  TransformLocationText := glGetUniformLocation(TextProgram, 'transform');
  TransformLocationText := glGetUniformLocation(LineStripProgram, 'transform');
  ColorLocationLineStrip := glGetUniformLocation(LineStripProgram, 'color');

  // Changing the active shader program is expensive. For drawing basic shapes (quads, triangles, lines), we define
  // a 1x1 white texture to sample color from, which lets us use the same fragment shader for quads that we use
  // for drawing regular textures, thereby avoiding the expensive state change
  glGenTextures(1, @WhiteTexture);
  glBindTexture(GL_TEXTURE_2D, WhiteTexture);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 1, 1, 0, GL_RGBA, GL_UNSIGNED_BYTE, PGLvoid(@WhitePixel[0]));

  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF}
end;

destructor TRenderer_OpenGLBase.Destroy();
begin
  glDeleteProgram(MainProgram);
  glDeleteProgram(TextProgram);
  glDeleteProgram(LineStripProgram);
  glDeleteVertexArrays(1, @MainVAO);
  glDeleteVertexArrays(1, @LineStripVAO);
  glDeleteBuffers(1, @VBO);
  glDeleteBuffers(1, @EBO);
  if (WhiteTexture <> 0) then
    glDeleteTextures(1, @WhiteTexture);
  SDL_GL_DeleteContext(glcontext);
  inherited;
end;

// Compile all shaders and link programs. The shader source is provided by the subclass, derived from the abstracted version
procedure TRenderer_OpenGLBase.InitShaderPrograms();
var
  VertexShader, MainFragmentShader, TextFragmentShader, LineStripVertexShader, LineStripFragmentShader: GLuint;
  MainVertexSource, MainFragmentSource, TextFragmentSource, LineStripVertexSource, LineStripFragmentSource: string;
  CompileResult: GLint;
  ErrorBuf: array[0..511] of GLcharARB;

  function CompileShader(ShaderType: GLenum; Source: string; Name: string): GLuint;
  begin
    Result := glCreateShader(ShaderType);
    glShaderSource(Result, 1, PPGLcharARB(@Source), nil);
    glCompileShader(Result);
    glGetShaderiv(Result, GL_COMPILE_STATUS, @CompileResult);
    if (ByteBool(CompileResult) = GL_FALSE) then
    begin
      glGetShaderInfoLog(Result, SizeOf(ErrorBuf), nil, @ErrorBuf[0]);
      raise Exception.Create('Failed to compile ' + Name + ' shader. OpenGL Error: ' + ErrorBuf);
    end;
  end;

begin
  // Get shader sources from subclass
  GetShaderSource(MainVertexSource, MainFragmentSource, TextFragmentSource, LineStripVertexSource, LineStripFragmentSource);

  // Compile vertex and fragment shaders
  VertexShader := CompileShader(GL_VERTEX_SHADER, MainVertexSource, 'main vertex');
  MainFragmentShader := CompileShader(GL_FRAGMENT_SHADER, MainFragmentSource, 'main fragment');
  TextFragmentShader := CompileShader(GL_FRAGMENT_SHADER, TextFragmentSource, 'text fragment');
  LineStripVertexShader := CompileShader(GL_VERTEX_SHADER, LineStripVertexSource, 'line strip vertex');
  LineStripFragmentShader := CompileShader(GL_FRAGMENT_SHADER, LineStripFragmentSource, 'line strip fragment');

  // Link main shader program
  MainProgram := glCreateProgram();
  glAttachShader(MainProgram, VertexShader);
  glAttachShader(MainProgram, MainFragmentShader);
  glBindAttribLocation(MainProgram, 0, 'pos');
  glBindAttribLocation(MainProgram, 1, 'color');
  glBindAttribLocation(MainProgram, 2, 'tex_coords');
  glLinkProgram(MainProgram);
  glGetProgramiv(MainProgram, GL_LINK_STATUS, @CompileResult);
  if (ByteBool(CompileResult) = GL_FALSE) then
  begin
    glGetProgramInfoLog(MainProgram, SizeOf(ErrorBuf), nil, @ErrorBuf[0]);
    raise Exception.Create('Failed to link main shader program. OpenGL Error: ' + ErrorBuf);
  end;

  // Link text shader program
  TextProgram := glCreateProgram();
  glAttachShader(TextProgram, VertexShader);
  glAttachShader(TextProgram, TextFragmentShader);
  glBindAttribLocation(TextProgram, 0, 'pos');
  glBindAttribLocation(TextProgram, 1, 'color');
  glBindAttribLocation(TextProgram, 2, 'tex_coords');
  glLinkProgram(TextProgram);
  glGetProgramiv(TextProgram, GL_LINK_STATUS, @CompileResult);
  if (ByteBool(CompileResult) = GL_FALSE) then
  begin
    glGetProgramInfoLog(TextProgram, SizeOf(ErrorBuf), nil, @ErrorBuf[0]);
    raise Exception.Create('Failed to link text shader program. OpenGL Error: ' + ErrorBuf);
  end;

  // Link line strip shader program
  LineStripProgram := glCreateProgram();
  glAttachShader(LineStripProgram, LineStripVertexShader);
  glAttachShader(LineStripProgram, LineStripFragmentShader);
  glBindAttribLocation(LineStripProgram, 0, 'pos');
  glLinkProgram(LineStripProgram);
  glGetProgramiv(LineStripProgram, GL_LINK_STATUS, @CompileResult);
  if (ByteBool(CompileResult) = GL_FALSE) then
  begin
    glGetProgramInfoLog(LineStripProgram, SizeOf(ErrorBuf), nil, @ErrorBuf[0]);
    raise Exception.Create('Failed to link line strip shader program. OpenGL Error: ' + ErrorBuf);
  end;

  glDeleteShader(VertexShader);
  glDeleteShader(MainFragmentShader);
  glDeleteShader(TextFragmentShader);
  glDeleteShader(LineStripVertexShader);
  glDeleteShader(LineStripFragmentShader);

  {$IFDEF DEBUG_MODE}
  TRenderer_OpenGLBase(Renderer).RaiseExceptionIfError;
  {$ENDIF}
end;

procedure TRenderer_OpenGLBase.InitBuffers();
var
  I, Quad: Cardinal;
  EBOData: array of GLuint;
begin
  if (SupportsVAO) then
  begin
    glGenVertexArrays(1, @MainVAO);
    glGenVertexArrays(1, @LineStripVAO);
  end;

  glGenBuffers(1, @VBO);
  glGenBuffers(1, @EBO);
  if (SupportsVAO) then
    glBindVertexArray(MainVAO);

  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glBufferData(GL_ARRAY_BUFFER, VBO_SIZE, nil, GL_STREAM_DRAW);

  // Modern OpenGL requires us to decompose quads into two triangles. The purpose of the EBO is that
  // it serves as a pointer into the VBO to indicate which vertices to draw. It allows us to specify only 4 vertices for
  // each quad instead of 6, which saves memory and bandwidth. So the EBO will point to VBO indices (0,1,3) and (1,2,3)
  // for the first quad, and so on. We fill up the EBO to point to every possible vertex location in the VBO for drawing quads
  SetLength(EBOData, EBO_INDICES);
  I := 0;
  Quad := 0;
  while (I < EBO_INDICES) do
  begin
    // First triangle
    EBOData[I + 0] := Quad + 0;
    EBOData[I + 1] := Quad + 1;
    EBOData[I + 2] := Quad + 3;

    // Second triangle
    EBOData[I + 3] := Quad + 1;
    EBOData[I + 4] := Quad + 2;
    EBOData[I + 5] := Quad + 3;

    I := I + 6;
    Quad := Quad + 4;
  end;
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, EBO_SIZE, PGLvoid(EBOData), GL_STATIC_DRAW);

  if (SupportsVAO) then
  begin
    BindMainVertexAttrib;
    glBindVertexArray(LineStripVAO);
    BindLineStripVertexAttrib;
    glBindVertexArray(0);
  end;

  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

// Setup vertex attributes for main vertex shader
procedure TRenderer_OpenGLBase.BindMainVertexAttrib();
begin
  // Position
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, VERTEX_STRIDE_BYTES, PGLvoid(0));
  glEnableVertexAttribArray(0);

  // RGBA
  glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, VERTEX_STRIDE_BYTES, PGLvoid(R_OFFSET * SizeOf(GLfloat)));
  glEnableVertexAttribArray(1);

  // Texture Coords
  glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, VERTEX_STRIDE_BYTES, PGLvoid(TEXX_OFFSET * SizeOf(GLfloat)));
  glEnableVertexAttribArray(2);

  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

// Setup vertex attributes for line strip vertex shader used for oscilloscope
procedure TRenderer_OpenGLBase.BindLineStripVertexAttrib();
begin
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, PGLvoid(0));
  glEnableVertexAttribArray(0);

  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

{*
 * We use a single VBO for all drawing, in a ringbuffer-like streaming setup. A cursor keeps track of where the last
 * write operation ended. When we reach the end of the VBO, the buffer is orphaned and the cursor is reset to the beginning.
 * This strategy ensures that the CPU will never write where the GPU is currently reading from, which prevents pipeline stalls.
}
function TRenderer_OpenGLBase.GetArrayBuffer(var Bytes: GLuint): PGLfloat;
begin
  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  Bytes := Bytes + Bytes mod QUAD_STRIDE_BYTES;
  if (GLuint(VBOCursor) * SizeOf(GLfloat) + Bytes >= VBO_SIZE) then
  begin
    glBufferData(GL_ARRAY_BUFFER, VBO_SIZE, nil, GL_STREAM_DRAW);
    VBOCursor := 0;
    EBOCursor := 0;
  end;
  Result := PGLfloat(glMapBufferRange(GL_ARRAY_BUFFER, VBOCursor * SizeOf(GLfloat), Bytes, GL_MAP_WRITE_BIT or GL_MAP_UNSYNCHRONIZED_BIT));
  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

procedure TRenderer_OpenGLBase.UpdateTransformationMatrix();
begin
  UpdateTransformMain := true;
  UpdateTransformText := true;
end;

procedure TRenderer_OpenGLBase.DrawTexture(Texture: TTexture; Prog: GLuint; var UpdateTransform: boolean; TransformLocation: GLint);
var
  Tex: TTexture_OpenGL;
  NumQuads: GLuint;
  Buffer: PGLfloat;
  Bytes: GLuint;
  ColorR, ColorG, ColorB: single;
  X2, Y2: single;
begin
  Tex := TTexture_OpenGL(Texture);
  if (Tex.TexID = 0) then
    Exit;

  // Setup shader
  if (SupportsVAO) then
    glBindVertexArray(MainVAO)
  else
    BindMainVertexAttrib;
  glUseProgram(Prog);
  if (UpdateTransform) then
  begin
    glUniformMatrix4fv(TransformLocation, 1, GL_TRUE, PGLfloat(@ProjectionMatrix));
    UpdateTransform := false;
  end;
  glBindTexture(GL_TEXTURE_2D, Tex.TexID);
  if (Tex.Reflection) then
    NumQuads := 2
  else
    NumQuads := 1;

  // Map VBO to system memory
  Bytes := QUAD_STRIDE_BYTES * NumQuads;
  Buffer := GetArrayBuffer(Bytes);
  if (Buffer = nil) then
    Exit;

  // Fill in VBO mapped memory with vertex information from our texture
  glBindTexture(GL_TEXTURE_2D, Tex.TexID);
  with Tex do
  begin
    ColorR := Int * ColR;
    ColorG := Int * ColG;
    ColorB := Int * ColB;
    X2 := X + W;
    Y2 := Y + H;

    // Top right vertex
    Buffer[VERTEX_TOPRIGHT_OFFSET + X_OFFSET] := X2;
    Buffer[VERTEX_TOPRIGHT_OFFSET + Y_OFFSET] := Y;
    Buffer[VERTEX_TOPRIGHT_OFFSET + Z_OFFSET] := Z;
    Buffer[VERTEX_TOPRIGHT_OFFSET + R_OFFSET] := ColorR;
    Buffer[VERTEX_TOPRIGHT_OFFSET + G_OFFSET] := ColorG;
    Buffer[VERTEX_TOPRIGHT_OFFSET + B_OFFSET] := ColorB;
    Buffer[VERTEX_TOPRIGHT_OFFSET + TEXX_OFFSET] := TexX2;
    Buffer[VERTEX_TOPRIGHT_OFFSET + TEXY_OFFSET] := TexY1;

    // Bottom right vertex
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + X_OFFSET] := X2;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + Y_OFFSET] := Y2;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + Z_OFFSET] := Z;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + R_OFFSET] := ColorR;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + G_OFFSET] := ColorG;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + B_OFFSET] := ColorB;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + TEXX_OFFSET] := TexX2;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + TEXY_OFFSET] := TexY2;

    // Bottom left vertex
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + X_OFFSET] := X;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + Y_OFFSET] := Y2;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + Z_OFFSET] := Z;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + R_OFFSET] := ColorR;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + G_OFFSET] := ColorG;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + B_OFFSET] := ColorB;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + TEXX_OFFSET] := TexX1;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + TEXY_OFFSET] := TexY2;

    // Top left vertex
    Buffer[VERTEX_TOPLEFT_OFFSET + X_OFFSET] := X;
    Buffer[VERTEX_TOPLEFT_OFFSET + Y_OFFSET] := Y;
    Buffer[VERTEX_TOPLEFT_OFFSET + Z_OFFSET] := Z;
    Buffer[VERTEX_TOPLEFT_OFFSET + R_OFFSET] := ColorR;
    Buffer[VERTEX_TOPLEFT_OFFSET + G_OFFSET] := ColorG;
    Buffer[VERTEX_TOPLEFT_OFFSET + B_OFFSET] := ColorB;
    Buffer[VERTEX_TOPLEFT_OFFSET + TEXX_OFFSET] := TexX1;
    Buffer[VERTEX_TOPLEFT_OFFSET + TEXY_OFFSET] := TexY1;

    // Alpha (may have a gradient)
    if (AlphaGradient = gdNone) then
    begin
      Buffer[VERTEX_TOPRIGHT_OFFSET + A_OFFSET] := Alpha;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + A_OFFSET] := Alpha;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + A_OFFSET] := Alpha;
      Buffer[VERTEX_TOPLEFT_OFFSET + A_OFFSET] := Alpha;
    end
    else if (AlphaGradient = gdVertical) then
    begin
      Buffer[VERTEX_TOPRIGHT_OFFSET + A_OFFSET] := Alpha;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + A_OFFSET] := Alpha2;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + A_OFFSET] := Alpha2;
      Buffer[VERTEX_TOPLEFT_OFFSET + A_OFFSET] := Alpha;
    end
    else if (AlphaGradient = gdHorizontal) then
    begin
      Buffer[VERTEX_TOPRIGHT_OFFSET + A_OFFSET] := Alpha2;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + A_OFFSET] := Alpha2;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + A_OFFSET] := Alpha;
      Buffer[VERTEX_TOPLEFT_OFFSET + A_OFFSET] := Alpha;
    end;

    // Shear effect for italic text
    if (Shear <> 0) then
    begin
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + X_OFFSET] := Buffer[VERTEX_BOTTOMRIGHT_OFFSET + X_OFFSET] + Shear;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + X_OFFSET] := Buffer[VERTEX_BOTTOMLEFT_OFFSET + X_OFFSET] + Shear;
    end;

    // Now do the reflection
    if (Reflection) then
    begin
      Buffer := Buffer + QUAD_STRIDE;

      // Top right vertex
      Buffer[VERTEX_TOPRIGHT_OFFSET + X_OFFSET] := X2;
      Buffer[VERTEX_TOPRIGHT_OFFSET + Y_OFFSET] := Y + H + ReflectionSpacing;
      Buffer[VERTEX_TOPRIGHT_OFFSET + Z_OFFSET] := Z;
      Buffer[VERTEX_TOPRIGHT_OFFSET + R_OFFSET] := ColorR;
      Buffer[VERTEX_TOPRIGHT_OFFSET + G_OFFSET] := ColorG;
      Buffer[VERTEX_TOPRIGHT_OFFSET + B_OFFSET] := ColorB;
      Buffer[VERTEX_TOPRIGHT_OFFSET + A_OFFSET] := Alpha - ReflectionAlphaDiff;
      Buffer[VERTEX_TOPRIGHT_OFFSET + TEXX_OFFSET] := TexX2;
      Buffer[VERTEX_TOPRIGHT_OFFSET + TEXY_OFFSET] := ReflectionTexY1;

      // Bottom right vertex
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + X_OFFSET] := X2;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + Y_OFFSET] := Y + H + ReflectionSpacing + (H * ReflectionHeight) ;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + Z_OFFSET] := Z;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + R_OFFSET] := ColorR;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + G_OFFSET] := ColorG;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + B_OFFSET] := ColorB;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + A_OFFSET] := 0;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + TEXX_OFFSET] := TexX2;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + TEXY_OFFSET] := ReflectionTexY2;

      // Bottom left vertex
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + X_OFFSET] := X;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + Y_OFFSET] := Y + H + ReflectionSpacing + (H * ReflectionHeight);
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + Z_OFFSET] := Z;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + R_OFFSET] := ColorR;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + G_OFFSET] := ColorG;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + B_OFFSET] := ColorB;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + A_OFFSET] := 0;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + TEXX_OFFSET] := TexX1;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + TEXY_OFFSET] := ReflectionTexY2;

      // Top left vertex
      Buffer[VERTEX_TOPLEFT_OFFSET + X_OFFSET] := X;
      Buffer[VERTEX_TOPLEFT_OFFSET + Y_OFFSET] := Y + H + ReflectionSpacing;
      Buffer[VERTEX_TOPLEFT_OFFSET + Z_OFFSET] := Z;
      Buffer[VERTEX_TOPLEFT_OFFSET + R_OFFSET] := ColorR;
      Buffer[VERTEX_TOPLEFT_OFFSET + G_OFFSET] := ColorG;
      Buffer[VERTEX_TOPLEFT_OFFSET + B_OFFSET] := ColorB;
      Buffer[VERTEX_TOPLEFT_OFFSET + A_OFFSET] := Alpha - ReflectionAlphaDiff;
      Buffer[VERTEX_TOPLEFT_OFFSET + TEXX_OFFSET] := TexX1;
      Buffer[VERTEX_TOPLEFT_OFFSET + TEXY_OFFSET] := ReflectionTexY1;
    end;
  end;

  // Upload our vertex data to the GPU
  glUnmapBuffer(GL_ARRAY_BUFFER);

  // Draw elements and update VBO and EBO positions
  glDrawElements(GL_TRIANGLES, NumQuads * 6, GL_UNSIGNED_INT, PGLvoid(EBOCursor * SizeOf(GLuint)));
  VBOCursor := VBOCursor + (NumQuads * 4 * VERTEX_STRIDE);
  EBOCursor := EBOCursor + (NumQuads * 6);
  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

procedure TRenderer_OpenGLBase.DrawTexture(Texture: TTexture);
begin
  DrawTexture(Texture, MainProgram, UpdateTransformMain, TransformLocationMain);
end;

procedure TRenderer_OpenGLBase.DrawGlyph(Texture: TTexture);
begin
  DrawTexture(Texture, TextProgram, UpdateTransformText, TransformLocationText);
end;

// Procedure to draw colored quads
procedure TRenderer_OpenGLBase.DrawQuads(QuadList: TQuadList);
var
  NumQuads: GLuint;
  Buffer: PGLfloat;
  Bytes: GLuint;
  X2, Y2: single;
  I: integer;
begin
  NumQuads := Length(QuadList);
  if (NumQuads = 0) then
    Exit;

  // Setup shader
  if (SupportsVAO) then
    glBindVertexArray(MainVAO)
  else
    BindMainVertexAttrib;
  glUseProgram(MainProgram);
  if (UpdateTransformMain) then
  begin
    glUniformMatrix4fv(TransformLocationMain, 1, GL_TRUE, PGLfloat(@ProjectionMatrix));
    UpdateTransformMain := false;
  end;

  // Map VBO to system memory
  Bytes := QUAD_STRIDE_BYTES * NumQuads;
  Buffer := GetArrayBuffer(Bytes);

  // Fill in VBO mapped memory with vertex information
  glBindTexture(GL_TEXTURE_2D, WhiteTexture);
  for I := Low(QuadList) to High(QuadList) do
  begin
    X2 := QuadList[I].X + QuadList[I].W;
    Y2 := QuadList[I].Y + QuadList[I].H;

    // Top right vertex
    Buffer[VERTEX_TOPRIGHT_OFFSET + X_OFFSET] := X2;
    Buffer[VERTEX_TOPRIGHT_OFFSET + Y_OFFSET] := QuadList[I].Y;
    Buffer[VERTEX_TOPRIGHT_OFFSET + Z_OFFSET] := QuadList[I].Z;
    Buffer[VERTEX_TOPRIGHT_OFFSET + TEXX_OFFSET] := 1;
    Buffer[VERTEX_TOPRIGHT_OFFSET + TEXY_OFFSET] := 0;

    // Bottom right vertex
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + X_OFFSET] := X2;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + Y_OFFSET] := Y2;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + Z_OFFSET] := QuadList[I].Z;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + TEXX_OFFSET] := 1;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + TEXY_OFFSET] := 1;

    // Bottom left vertex
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + X_OFFSET] := QuadList[I].X;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + Y_OFFSET] := Y2;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + Z_OFFSET] := QuadList[I].Z;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + TEXX_OFFSET] := 0;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + TEXY_OFFSET] := 1;

    // Top left vertex
    Buffer[VERTEX_TOPLEFT_OFFSET + X_OFFSET] := QuadList[I].X;
    Buffer[VERTEX_TOPLEFT_OFFSET + Y_OFFSET] := QuadList[I].Y;
    Buffer[VERTEX_TOPLEFT_OFFSET + Z_OFFSET] := QuadList[I].Z;
    Buffer[VERTEX_TOPLEFT_OFFSET + TEXX_OFFSET] := 0;
    Buffer[VERTEX_TOPLEFT_OFFSET + TEXY_OFFSET] := 0;

    // RGBA may have a gradient
    if (QuadList[I].Gradient = gdNone) then
    begin

      // Top right vertex
      Buffer[VERTEX_TOPRIGHT_OFFSET + R_OFFSET] := QuadList[I].ColR;
      Buffer[VERTEX_TOPRIGHT_OFFSET + G_OFFSET] := QuadList[I].ColG;
      Buffer[VERTEX_TOPRIGHT_OFFSET + B_OFFSET] := QuadList[I].ColB;
      Buffer[VERTEX_TOPRIGHT_OFFSET + A_OFFSET] := QuadList[I].Alpha;

      // Bottom Right vertex
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + R_OFFSET] := QuadList[I].ColR;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + G_OFFSET] := QuadList[I].ColG;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + B_OFFSET] := QuadList[I].ColB;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + A_OFFSET] := QuadList[I].Alpha;

      // Bottom Left vertex
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + R_OFFSET] := QuadList[I].ColR;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + G_OFFSET] := QuadList[I].ColG;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + B_OFFSET] := QuadList[I].ColB;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + A_OFFSET] := QuadList[I].Alpha;

      // Top Left vertex
      Buffer[VERTEX_TOPLEFT_OFFSET + R_OFFSET] := QuadList[I].ColR;
      Buffer[VERTEX_TOPLEFT_OFFSET + G_OFFSET] := QuadList[I].ColG;
      Buffer[VERTEX_TOPLEFT_OFFSET + B_OFFSET] := QuadList[I].ColB;
      Buffer[VERTEX_TOPLEFT_OFFSET + A_OFFSET] := QuadList[I].Alpha;
    end
    else if (QuadList[I].Gradient = gdVertical) then
    begin

      // Top right vertex
      Buffer[VERTEX_TOPRIGHT_OFFSET + R_OFFSET] := QuadList[I].ColR;
      Buffer[VERTEX_TOPRIGHT_OFFSET + G_OFFSET] := QuadList[I].ColG;
      Buffer[VERTEX_TOPRIGHT_OFFSET + B_OFFSET] := QuadList[I].ColB;
      Buffer[VERTEX_TOPRIGHT_OFFSET + A_OFFSET] := QuadList[I].Alpha;

      // Bottom right vertex
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + R_OFFSET] := QuadList[I].ColR2;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + G_OFFSET] := QuadList[I].ColG2;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + B_OFFSET] := QuadList[I].ColB2;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + A_OFFSET] := QuadList[I].Alpha2;

      // Bottom left vertex
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + R_OFFSET] := QuadList[I].ColR2;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + G_OFFSET] := QuadList[I].ColG2;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + B_OFFSET] := QuadList[I].ColB2;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + A_OFFSET] := QuadList[I].Alpha2;

      // Top left vertex
      Buffer[VERTEX_TOPLEFT_OFFSET + R_OFFSET] := QuadList[I].ColR;
      Buffer[VERTEX_TOPLEFT_OFFSET + G_OFFSET] := QuadList[I].ColG;
      Buffer[VERTEX_TOPLEFT_OFFSET + B_OFFSET] := QuadList[I].ColB;
      Buffer[VERTEX_TOPLEFT_OFFSET + A_OFFSET] := QuadList[I].Alpha;
    end
    else if (QuadList[I].Gradient = gdHorizontal) then
    begin

      // Top right vertex
      Buffer[VERTEX_TOPRIGHT_OFFSET + R_OFFSET] := QuadList[I].ColR2;
      Buffer[VERTEX_TOPRIGHT_OFFSET + G_OFFSET] := QuadList[I].ColG2;
      Buffer[VERTEX_TOPRIGHT_OFFSET + B_OFFSET] := QuadList[I].ColB2;
      Buffer[VERTEX_TOPRIGHT_OFFSET + A_OFFSET] := QuadList[I].Alpha2;

      // Bottom right vertex
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + R_OFFSET] := QuadList[I].ColR2;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + G_OFFSET] := QuadList[I].ColG2;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + B_OFFSET] := QuadList[I].ColB2;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + A_OFFSET] := QuadList[I].Alpha2;

      // Bottom left vertex
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + R_OFFSET] := QuadList[I].ColR;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + G_OFFSET] := QuadList[I].ColG;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + B_OFFSET] := QuadList[I].ColB;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + A_OFFSET] := QuadList[I].Alpha;

      // Top left vertex
      Buffer[VERTEX_TOPLEFT_OFFSET + R_OFFSET] := QuadList[I].ColR;
      Buffer[VERTEX_TOPLEFT_OFFSET + G_OFFSET] := QuadList[I].ColG;
      Buffer[VERTEX_TOPLEFT_OFFSET + B_OFFSET] := QuadList[I].ColB;
      Buffer[VERTEX_TOPLEFT_OFFSET + A_OFFSET] := QuadList[I].Alpha;
    end;

    Buffer := Buffer + QUAD_STRIDE;
  end;

  // Upload our vertex data to the GPU
  glUnmapBuffer(GL_ARRAY_BUFFER);

  // Draw elements and update VBO and EBO positions
  glDrawElements(GL_TRIANGLES, NumQuads * 6, GL_UNSIGNED_INT, PGLvoid(EBOCursor * SizeOf(GLuint)));
  VBOCursor := VBOCursor + (NumQuads * QUAD_STRIDE);
  EBOCursor := EBOCursor + (NumQuads * 6);
  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

procedure TRenderer_OpenGLBase.DrawTriangles(TriangleList: TTriangleList);
var
  NumTriangles: GLuint;
  EquivalentQuads: GLuint;
  Buffer: PGLfloat;
  Bytes: GLuint;
  I: integer;
begin
  NumTriangles := Length(TriangleList);
  if (NumTriangles = 0) then
    Exit;

  // Setup shader
  if (SupportsVAO) then
    glBindVertexArray(MainVAO)
  else
    BindMainVertexAttrib;
  glUseProgram(MainProgram);
  if (UpdateTransformMain) then
  begin
    glUniformMatrix4fv(TransformLocationMain, 1, GL_TRUE, PGLfloat(@ProjectionMatrix));
    UpdateTransformMain := false;
  end;

  // Map VBO to system memory
  Bytes := VERTEX_STRIDE_BYTES * 3;
  Buffer := GetArrayBuffer(Bytes);

  // Fill in VBO mapped memory with vertex information
  glBindTexture(GL_TEXTURE_2D, WhiteTexture);
  for I := Low(TriangleList) to High(TriangleList) do
  begin

    // First Vertex
    Buffer[VERTEX_TOPRIGHT_OFFSET + X_OFFSET] := TriangleList[I].X1;
    Buffer[VERTEX_TOPRIGHT_OFFSET + Y_OFFSET] := TriangleList[I].Y1;
    Buffer[VERTEX_TOPRIGHT_OFFSET + Z_OFFSET] := TriangleList[I].Z;
    Buffer[VERTEX_TOPRIGHT_OFFSET + R_OFFSET] := TriangleList[I].ColR;
    Buffer[VERTEX_TOPRIGHT_OFFSET + G_OFFSET] := TriangleList[I].ColG;
    Buffer[VERTEX_TOPRIGHT_OFFSET + B_OFFSET] := TriangleList[I].ColB;
    Buffer[VERTEX_TOPRIGHT_OFFSET + A_OFFSET] := TriangleList[I].Alpha;
    Buffer[VERTEX_TOPRIGHT_OFFSET + TEXX_OFFSET] := 1;
    Buffer[VERTEX_TOPRIGHT_OFFSET + TEXY_OFFSET] := 0;

    // Second Vertex
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + X_OFFSET] := TriangleList[I].X2;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + Y_OFFSET] := TriangleList[I].Y2;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + Z_OFFSET] := TriangleList[I].Z;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + R_OFFSET] := TriangleList[I].ColR;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + G_OFFSET] := TriangleList[I].ColG;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + B_OFFSET] := TriangleList[I].ColB;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + A_OFFSET] := TriangleList[I].Alpha;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + TEXX_OFFSET] := 1;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + TEXY_OFFSET] := 1;

    // Third Vertex
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + X_OFFSET] := TriangleList[I].X3;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + Y_OFFSET] := TriangleList[I].Y3;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + Z_OFFSET] := TriangleList[I].Z;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + R_OFFSET] := TriangleList[I].ColR;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + G_OFFSET] := TriangleList[I].ColG;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + B_OFFSET] := TriangleList[I].ColB;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + A_OFFSET] := TriangleList[I].Alpha;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + TEXX_OFFSET] := 0;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + TEXY_OFFSET] := 1;

    Buffer := Buffer + TRIANGLE_STRIDE;
  end;

  // Upload our vertex data to the GPU
  glUnmapBuffer(GL_ARRAY_BUFFER);

  // Draw elements
  glDrawArrays(GL_TRIANGLES, VBOCursor div VERTEX_STRIDE, NumTriangles * 3);

  // We didn't draw quads, but the VBO is optimized for drawing quads. So we
  // calculate the equivalent quad sizes for the amount of bytes we used
  // and update the VBO and EBO positions accordingly
  EquivalentQuads := Bytes div QUAD_STRIDE_BYTES;
  VBOCursor := VBOCursor + (EquivalentQuads * QUAD_STRIDE);
  EBOCursor := EBOCursor + (EquivalentQuads * 6);
  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

// Procedure to draw lines. Because modern OpenGL no longer supports thick lines,
// we have to decompose the lines into quads. We could use a geometry shader for this,
// but to keep support for older OpenGL versions, we will do it on the CPU instead
procedure TRenderer_OpenGLBase.DrawLines(LineList: TLineList);
var
  VecX, VecY, VecPerpX, VecPerpY, Dist, LineThicknessW, LineThicknessH, HalfLineThicknessW, HalfLineThicknessH, X1, X2, Y1, Y2: single;
  I, NumQuads: integer;
  Buffer: PGLfloat;
  Bytes: GLuint;
begin
  NumQuads := Length(LineList);
  if (NumQuads = 0) then
    Exit;

  // Setup shader
  if (SupportsVAO) then
    glBindVertexArray(MainVAO)
  else
    BindMainVertexAttrib;
  glUseProgram(MainProgram);
  if (UpdateTransformMain) then
  begin
    glUniformMatrix4fv(TransformLocationMain, 1, GL_TRUE, PGLfloat(@ProjectionMatrix));
    UpdateTransformMain := false;
  end;

  // Map VBO to system memory
  Bytes := QUAD_STRIDE_BYTES * NumQuads;
  Buffer := GetArrayBuffer(Bytes);

  // Fill in VBO mapped memory with vertex information
  glBindTexture(GL_TEXTURE_2D, WhiteTexture);
  for I := Low(LineList) to High(LineList) do
  begin

    // Calculate line thickness in 800x600 vertex space
    LineThicknessW := LineList[I].Thickness * PixelW;
    LineThicknessH := LineList[I].Thickness * PixelH;
    HalfLineThicknessW := LineThicknessW * 0.5;
    HalfLineThicknessH := LineThicknessH * 0.5;

    // Start with specialized cases for horizontal and vertical lines, because those are simpler. For
    // diagnoal lines, it gets more complicated

    // Horizontal line case
    if (SameValue(LineList[I].Y1, LineList[I].Y2, 0.001)) then
    begin
      // Snap vertex to nearest window pixel to prevent quantization artifacts
      Y1 := Round((LineList[I].Y1 - HalfLineThicknessH) * VertexH) * PixelH;
      Y2 := Y1 + LineThicknessH;

      // Top right
      Buffer[VERTEX_TOPRIGHT_OFFSET + X_OFFSET] := LineList[I].X2;
      Buffer[VERTEX_TOPRIGHT_OFFSET + Y_OFFSET] := Y1;

      // Bottom right
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + X_OFFSET] := LineList[I].X2;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + Y_OFFSET] := Y2;

      // Bottom left
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + X_OFFSET] := LineList[I].X1;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + Y_OFFSET] := Y2;

      // Top left
      Buffer[VERTEX_TOPLEFT_OFFSET + X_OFFSET] := LineList[I].X1;
      Buffer[VERTEX_TOPLEFT_OFFSET + Y_OFFSET] := Y1;

    end

    // Vertical line case
    else if (SameValue(LineList[I].X1, LineList[I].X2, 0.001)) then
    begin
      // Snap vertex to nearest window pixel to prevent quantization artifacts
      X1 := Round((LineList[I].X1 - HalfLineThicknessW) * VertexW) * PixelW;
      X2 := X1 + LineThicknessW;

      // Top right
      Buffer[VERTEX_TOPRIGHT_OFFSET + X_OFFSET] := X2;
      Buffer[VERTEX_TOPRIGHT_OFFSET + Y_OFFSET] := LineList[I].Y1;

      // Bottom right
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + X_OFFSET] := X2;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + Y_OFFSET] := LineList[I].Y2;

      // Bottom left
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + X_OFFSET] := X1;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + Y_OFFSET] := LineList[I].Y2;

      // Top left
      Buffer[VERTEX_TOPLEFT_OFFSET + X_OFFSET] := X1;
      Buffer[VERTEX_TOPLEFT_OFFSET + Y_OFFSET] := LineList[I].Y1;
    end

    // Diaganol line (general case)
    else
    begin
      // Snap vertices to nearest window pixel to prevent quantization artifacts
      X1 := Round(LineList[I].X1 * VertexW) * PixelW;
      X2 := Round(LineList[I].X2 * VertexW) * PixelW;
      Y1 := Round(LineList[I].Y1 * VertexH) * PixelH;
      Y2 := Round(LineList[I].Y2 * VertexH) * PixelH;

      // Calculate perpendicular unit vector
      VecX := X2 - X1;
      VecY := Y2 - Y1;
      Dist := Sqrt(VecX * VecX + VecY * VecY);
      VecX := VecX / Dist;
      VecY := VecY / Dist;
      VecPerpX := -VecY;
      VecPerpY := VecX;

      // Scale vector by desired thickness
      VecPerpX := VecPerpX * HalfLineThicknessW;
      VecPerpY := VecPerpY * HalfLineThicknessH;

      // Top right
      Buffer[VERTEX_TOPRIGHT_OFFSET + X_OFFSET] := X2 + VecPerpX;
      Buffer[VERTEX_TOPRIGHT_OFFSET + Y_OFFSET] := Y2 + VecPerpY;

      // Bottom right
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + X_OFFSET] := X2 - VecPerpX;
      Buffer[VERTEX_BOTTOMRIGHT_OFFSET + Y_OFFSET] := Y2 - VecPerpY;

      // Bottom left
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + X_OFFSET] := X1 - VecPerpX;
      Buffer[VERTEX_BOTTOMLEFT_OFFSET + Y_OFFSET] := Y1 - VecPerpY;

      // Top left
      Buffer[VERTEX_TOPLEFT_OFFSET + X_OFFSET] := X1 + VecPerpX;
      Buffer[VERTEX_TOPLEFT_OFFSET + Y_OFFSET] := Y1 + VecPerpY;
    end;

    // Now fill in Z position and colors...
    // Top right
    Buffer[VERTEX_TOPRIGHT_OFFSET + Z_OFFSET] := LineList[I].Z;
    Buffer[VERTEX_TOPRIGHT_OFFSET + R_OFFSET] := LineList[I].ColR;
    Buffer[VERTEX_TOPRIGHT_OFFSET + G_OFFSET] := LineList[I].ColG;
    Buffer[VERTEX_TOPRIGHT_OFFSET + B_OFFSET] := LineList[I].ColB;
    Buffer[VERTEX_TOPRIGHT_OFFSET + A_OFFSET] := LineList[I].Alpha;
    Buffer[VERTEX_TOPRIGHT_OFFSET + TEXX_OFFSET] := 1;
    Buffer[VERTEX_TOPRIGHT_OFFSET + TEXY_OFFSET] := 0;

    // Bottom right
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + Z_OFFSET] := LineList[I].Z;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + R_OFFSET] := LineList[I].ColR;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + G_OFFSET] := LineList[I].ColG;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + B_OFFSET] := LineList[I].ColB;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + A_OFFSET] := LineList[I].Alpha;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + TEXX_OFFSET] := 1;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + TEXY_OFFSET] := 1;

    // Bottom left
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + Z_OFFSET] := LineList[I].Z;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + R_OFFSET] := LineList[I].ColR;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + G_OFFSET] := LineList[I].ColG;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + B_OFFSET] := LineList[I].ColB;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + A_OFFSET] := LineList[I].Alpha;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + TEXX_OFFSET] := 0;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + TEXY_OFFSET] := 1;

    // Top left
    Buffer[VERTEX_TOPLEFT_OFFSET + Z_OFFSET] := LineList[I].Z;
    Buffer[VERTEX_TOPLEFT_OFFSET + R_OFFSET] := LineList[I].ColR;
    Buffer[VERTEX_TOPLEFT_OFFSET + G_OFFSET] := LineList[I].ColG;
    Buffer[VERTEX_TOPLEFT_OFFSET + B_OFFSET] := LineList[I].ColB;
    Buffer[VERTEX_TOPLEFT_OFFSET + A_OFFSET] := LineList[I].Alpha;
    Buffer[VERTEX_TOPLEFT_OFFSET + TEXX_OFFSET] := 0;
    Buffer[VERTEX_TOPLEFT_OFFSET + TEXY_OFFSET] := 0;
    Buffer := Buffer + QUAD_STRIDE;
  end;

  // Upload our vertex data to the GPU
  glUnmapBuffer(GL_ARRAY_BUFFER);

  // Draw elements and update VBO and EBO positions
  glDrawElements(GL_TRIANGLES, NumQuads * 6, GL_UNSIGNED_INT, PGLvoid(EBOCursor * SizeOf(GLuint)));
  VBOCursor := VBOCursor + (NumQuads * QUAD_STRIDE);
  EBOCursor := EBOCursor + (NumQuads * 6);
  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

procedure TRenderer_OpenGLBase.DrawParticles(Texture: TTexture; ParticleList: TParticleList);
var
  Tex: TTexture_OpenGL;
  NumQuads: GLuint;
  Buffer: PGLfloat;
  Bytes: GLuint;
  X2, Y2: single;
  I: integer;
begin
  Tex := TTexture_OpenGL(Texture);
  if (Tex.TexID = 0) then
    Exit;
  NumQuads := Length(ParticleList);
  if (NumQuads = 0) then
    Exit;

  // Setup shader
  if (SupportsVAO) then
    glBindVertexArray(MainVAO)
  else
    BindMainVertexAttrib;
  glUseProgram(MainProgram);
  if (UpdateTransformMain) then
  begin
    glUniformMatrix4fv(TransformLocationMain, 1, GL_TRUE, PGLfloat(@ProjectionMatrix));
    UpdateTransformMain := false;
  end;

  // Map VBO to system memory
  Bytes := QUAD_STRIDE_BYTES * NumQuads;
  Buffer := GetArrayBuffer(Bytes);

  // Fill in VBO mapped memory with vertex information
  glBindTexture(GL_TEXTURE_2D, Tex.TexID);
  for I := Low(ParticleList) to High(ParticleList) do
  begin

    X2 := ParticleList[I].X + ParticleList[I].W;
    Y2 := ParticleList[I].Y + ParticleList[I].H;

    // Top right
    Buffer[VERTEX_TOPRIGHT_OFFSET + X_OFFSET] := X2;
    Buffer[VERTEX_TOPRIGHT_OFFSET + Y_OFFSET] := ParticleList[I].Y;
    Buffer[VERTEX_TOPRIGHT_OFFSET + Z_OFFSET] := 0;
    Buffer[VERTEX_TOPRIGHT_OFFSET + R_OFFSET] := ParticleList[I].ColR;
    Buffer[VERTEX_TOPRIGHT_OFFSET + G_OFFSET] := ParticleList[I].ColG;
    Buffer[VERTEX_TOPRIGHT_OFFSET + B_OFFSET] := ParticleList[I].ColB;
    Buffer[VERTEX_TOPRIGHT_OFFSET + A_OFFSET] := ParticleList[I].Alpha;
    Buffer[VERTEX_TOPRIGHT_OFFSET + TEXX_OFFSET] := ParticleList[I].TexX1;
    Buffer[VERTEX_TOPRIGHT_OFFSET + TEXY_OFFSET] := ParticleList[I].TexY1;

    // Bottom right
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + X_OFFSET] := X2;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + Y_OFFSET] := Y2;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + Z_OFFSET] := 0;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + R_OFFSET] := ParticleList[I].ColR;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + G_OFFSET] := ParticleList[I].ColG;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + B_OFFSET] := ParticleList[I].ColB;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + A_OFFSET] := ParticleList[I].Alpha;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + TEXX_OFFSET] := ParticleList[I].TexX2;
    Buffer[VERTEX_BOTTOMRIGHT_OFFSET + TEXY_OFFSET] := ParticleList[I].TexY1;

    // Bottom left
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + X_OFFSET] := ParticleList[I].X;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + Y_OFFSET] := Y2;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + Z_OFFSET] := 0;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + R_OFFSET] := ParticleList[I].ColR;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + G_OFFSET] := ParticleList[I].ColG;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + B_OFFSET] := ParticleList[I].ColB;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + A_OFFSET] := ParticleList[I].Alpha;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + TEXX_OFFSET] := ParticleList[I].TexX2;
    Buffer[VERTEX_BOTTOMLEFT_OFFSET + TEXY_OFFSET] := ParticleList[I].TexY2;

    // Top left
    Buffer[VERTEX_TOPLEFT_OFFSET + X_OFFSET] := ParticleList[I].X;
    Buffer[VERTEX_TOPLEFT_OFFSET + Y_OFFSET] := ParticleList[I].Y;
    Buffer[VERTEX_TOPLEFT_OFFSET + Z_OFFSET] := 0;
    Buffer[VERTEX_TOPLEFT_OFFSET + R_OFFSET] := ParticleList[I].ColR;
    Buffer[VERTEX_TOPLEFT_OFFSET + G_OFFSET] := ParticleList[I].ColG;
    Buffer[VERTEX_TOPLEFT_OFFSET + B_OFFSET] := ParticleList[I].ColB;
    Buffer[VERTEX_TOPLEFT_OFFSET + A_OFFSET] := ParticleList[I].Alpha;
    Buffer[VERTEX_TOPLEFT_OFFSET + TEXX_OFFSET] := ParticleList[I].TexX1;
    Buffer[VERTEX_TOPLEFT_OFFSET + TEXY_OFFSET] := ParticleList[I].TexY2;
    Buffer := Buffer + QUAD_STRIDE;
  end;

  // Upload our vertex data to the GPU
  glUnmapBuffer(GL_ARRAY_BUFFER);

  // Draw elements and update VBO and EBO positions
  glDrawElements(GL_TRIANGLES, NumQuads * 6, GL_UNSIGNED_INT, PGLvoid(EBOCursor * SizeOf(GLuint)));
  VBOCursor := VBOCursor + (NumQuads * 4 * VERTEX_STRIDE);
  EBOCursor := EBOCursor + (NumQuads * 6);
  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

// Line strip procedure for oscilloscope
procedure TRenderer_OpenGLBase.DrawLineStrip(PointList: TPointList; ScaleX, ScaleY, TranslateX, TranslateY, ColR, ColG, ColB, Alpha: single);
var
  Transform: Tmatrix4_single;
  NumPoints, EquivalentQuads: integer;
  Buffer: PGLfloat;
  Bytes: GLuint;
begin
  NumPoints := Length(PointList);
  if (NumPoints = 0) then
    Exit;

  // Setup shader
  if (SupportsVAO) then
    glBindVertexArray(LineStripVAO)
  else
    BindLineStripVertexAttrib;
  glUseProgram(LineStripProgram);

  // Use combination scale/translation matrix for model matrix (scale first, then translate)
  Transform.init_identity;
  Transform.data[0,0] := ScaleX;
  Transform.data[0,3] := TranslateX;
  Transform.data[1,1] := ScaleY;
  Transform.data[1,3] := TranslateY;
  Transform := ProjectionMatrix * Transform;
  glUniformMatrix4fv(TransformLocationLineStrip, 1, GL_TRUE, PGLfloat(@Transform));

  // Set colors
  glUniform4f(ColorLocationLineStrip, ColR, ColG, ColB, Alpha);

  // Map VBO to system memory
  Bytes := 2 * SizeOf(GLfloat) * NumPoints;
  Buffer := GetArrayBuffer(Bytes);

  // Because our list of points is already in the correct data format, we can just directly copy
  // it into the mapped VBO without further modification
  Move(PointList[0], Buffer[0], NumPoints * SizeOf(TPoint));

  // Upload our vertex data to the GPU
  glUnmapBuffer(GL_ARRAY_BUFFER);

  // Draw elements
  glDrawArrays(GL_LINE_STRIP, VBOCursor div 2, NumPoints);

  // We didn't draw quads, but the VBO is optimized for drawing quads. So we
  // calculate the equivalent quad sizes for the amount of bytes we used
  // and update the VBO and EBO positions accordingly
  EquivalentQuads := Bytes div QUAD_STRIDE_BYTES;
  VBOCursor := VBOCursor + (EquivalentQuads * QUAD_STRIDE);
  EBOCursor := EBOCursor + (EquivalentQuads * 6);
  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

procedure TRenderer_OpenGLBase.SetBlend(Enabled: boolean);
begin
  if (Enabled) then
    glEnable(GL_BLEND)
  else
    glDisable(GL_BLEND);
  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

function TRenderer_OpenGLBase.GetBlend(): boolean;
begin
  Result := glIsEnabled(GL_BLEND) = GL_TRUE;
end;

function TRenderer_OpenGLBase.LoadTexture(Data: PByte; W, H: integer; const Identifier: IPath; Typ: TTextureType): TTexture;
begin
  if (Typ = TEXTURE_TYPE_TRANSPARENT) or (Typ = TEXTURE_TYPE_COLORIZED) then
    Result := TTexture_OpenGL.Create(Data, W, H, Identifier, GL_RGBA, 4, GL_CLAMP_TO_EDGE)
  else // TEXTURE_TYPE_PLAIN
    Result := TTexture_OpenGL.Create(Data, W, H, Identifier, GL_RGB, 4, GL_CLAMP_TO_EDGE);
  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

// Special loading function for glyphs. The source data is a 1 byte per pixel alpha map. We
// store this in the red channel, and later assign it to alpha in the fragment shader
function TRenderer_OpenGLBase.LoadGlyph(Data: PByte; W, H: integer): TTexture;
begin
  Result := TTexture_OpenGL.Create(Data, W, H, PATH_NONE, GL_RED, 1, GL_CLAMP_TO_EDGE);
  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

function TRenderer_OpenGLBase.CreateEmptyTexture(const Identifier: IPath): TTexture;
begin
  Result := TTexture_OpenGL.Create(Identifier);
  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

procedure TRenderer_OpenGLBase.SetDepthTest(Enabled: boolean);
begin
  if (Enabled) then
    glEnable(GL_DEPTH_TEST)
  else
    glDisable(GL_DEPTH_TEST);
  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

function TRenderer_OpenGLBase.GetDepthTest(): boolean;
begin
  Result := glIsEnabled(GL_DEPTH_TEST) = GL_TRUE;
end;

procedure TRenderer_OpenGLBase.SetScissorRect(X, Y: integer; W, H: cardinal);
begin
  glScissor(X, Y, W, H);
  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

procedure TRenderer_OpenGLBase.SetScissorTest(Scissor: boolean);
begin
  if (Scissor) then
    glEnable(GL_SCISSOR_TEST)
  else
    glDisable(GL_SCISSOR_TEST);
  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

function TRenderer_OpenGLBase.GetScissorTest(): boolean;
begin
  Result := glIsEnabled(GL_SCISSOR_TEST) = GL_TRUE;
end;

// The equations for the orthographic projection matrix can be easily found online, e.g. in the Khronos documentation
// for the glOrtho() function
procedure TRenderer_OpenGLBase.SetOrthographicProjection(Left, Right, Bottom, Top, NearVal, FarVal: single);
begin
  ProjectionMatrix.init_zero;
  ProjectionMatrix.data[0,0] := 2 / (Right - Left);
  ProjectionMatrix.data[0,3] := -1 * ((Right + Left) / (Right - Left));
  ProjectionMatrix.data[1,1] := 2 / (Top - Bottom);
  ProjectionMatrix.data[1,3] := -1 * ((Top + Bottom) / (Top - Bottom));
  ProjectionMatrix.data[2,2] := -2 / (FarVal - NearVal);
  ProjectionMatrix.data[2,3] := -1 * ((FarVal + NearVal) / (FarVal - NearVal));
  ProjectionMatrix.data[3,3] := 1;
  UpdateTransformationMatrix;
end;

procedure TRenderer_OpenGLBase.SetViewPort(x, y: integer; width, height: cardinal);
begin
  glViewPort(x, y, width, height);
  ViewPortArray[0] := x;
  ViewPortArray[1] := y;
  ViewPortArray[2] := width;
  ViewPortArray[3] := height;
  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

procedure TRenderer_OpenGLBase.SetVSync(Enabled: boolean);
begin
  if (Enabled) then
    SDL_GL_SetSwapInterval(1)
  else
    SDL_GL_SetSwapInterval(0);
  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

function TRenderer_OpenGLBase.GetVSync(): boolean;
begin
  Result := SDL_GL_GetSwapInterval() = 1;
end;

procedure TRenderer_OpenGLBase.SwapBuffers();
begin
  SDL_GL_SwapWindow(Screen);
  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

function TRenderer_OpenGLBase.GetError(): boolean;
begin
  ErrorCode := glGetError();
  Result := ErrorCode <> GL_NO_ERROR;
end;

function TRenderer_OpenGLBase.GetErrorCode(): integer;
begin
  Result := Integer(ErrorCode);
  ErrorCode := GL_NO_ERROR;
end;

procedure TRenderer_OpenGLBase.SetClearColor(R, G, B, A: single);
begin
  glClearColor(R, G, B, A);
  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

procedure TRenderer_OpenGLBase.ClearFrameBuffer(Buffers: cardinal);
var
  GLBuffers: cardinal;
begin
  GLBuffers := 0;
  if ((Buffers and CLEAR_COLOR) <> 0) then
    GLBuffers := GLBuffers or GL_COLOR_BUFFER_BIT;
  if ((Buffers and CLEAR_DEPTH) <> 0) then
    GLBuffers := GLBuffers or GL_DEPTH_BUFFER_BIT;
  glClear(GLBuffers);
  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

{
 * The text cliping works by setting the edge of the orthographic projection matrix to the X boundary, and
 * adjusting the viewport so that it aligns to the correct pixel boundary. To prevent quantization artifacts,
 * we calculate the window (pixel) coordinate of X, then recalculate X in vertex space based on that window coordinate
}
procedure TRenderer_OpenGLBase.SetTextClipBoundary(X: single; Direction: ClippingDirection);
var
  WindowBoundary: integer; // X converted to window coordinate
  NewX: single; // Recaculated vertex from window coordinate
begin
  WindowBoundary := Round(X * VertexW);
  NewX := (WindowBoundary * PixelW);
  if (ScreenAct = 2) then
    WindowBoundary := WindowBoundary + ScreenWPerScreen;

  // Partial update of the projection matrix based on the values that are actually changing
  // see SetOrthographicProjection() function for equations
  if (Direction = cdLeft) then
  begin
    ProjectionMatrix.data[0,0] := 2 / (RenderW - NewX);
    ProjectionMatrix.data[0,3] := -1 * ((RenderW + NewX) / (RenderW - NewX));
    glViewport(WindowBoundary, ViewPortArray[1], (ScreenAct * ScreenWPerScreen) - WindowBoundary, ViewPortArray[3]);
  end
  else
  begin
    ProjectionMatrix.data[0,0] := 2 / NewX;
    ProjectionMatrix.data[0,3] := -1;
    glViewPort((ScreenAct - 1) * ScreenWPerScreen, ViewPortArray[1], WindowBoundary - (ScreenAct - 1) * ScreenWPerScreen, ViewPortArray[3]);
  end;
  UpdateTransformText := true;
  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

procedure TRenderer_OpenGLBase.SetClipText(Enabled: boolean);
begin
  if (Enabled) then
    OldProjectionMatrix := ProjectionMatrix
  else
  begin
    ProjectionMatrix := OldProjectionMatrix;
    SetViewPort(ViewPortArray[0], ViewPortArray[1], ViewPortArray[2], ViewPortArray[3]);
    UpdateTransformText := true;
  end;
  {$IFDEF DEBUG_MODE}
  RaiseExceptionIfError;
  {$ENDIF};
end;

// Reset various OpenGL state variables after ProjectM renders a frame, otherwise we get very odd glitches
procedure TRenderer_OpenGLBase.ResetState();
begin
  inherited;
  SetViewPort(ViewPortArray[0], ViewPortArray[1], ViewPortArray[2], ViewPortArray[3]);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glDepthFunc(GL_LEQUAL);
  glDepthRangef(0, 10);
  if (SupportsFBO) then
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
end;

// Return framebuffer data, used for screenshot function. Caller is responsible for freeing return value
function TRenderer_OpenGLBase.GetFrameBufferData(out RowSize: integer): PByte;
var
  Align: integer;
begin
  // we must take the row-alignment (4byte by default) into account
  glGetIntegerv(GL_PACK_ALIGNMENT, @Align);
  // calc aligned row-size
  RowSize := ((ScreenW*3 + (Align-1)) div Align) * Align;

  GetMem(Result, RowSize * ScreenH);
  glReadPixels(0, 0, ScreenW, ScreenH, GL_RGB, GL_UNSIGNED_BYTE, Result);
end;

{$IFDEF DEBUG_MODE}
procedure TRenderer_OpenGLBase.RaiseExceptionIfError();
var
  ErrCode: GLenum;
begin
  ErrCode := glGetError();
  if (ErrCode <> GL_NO_ERROR) then
    raise Exception.Create('OpenGL error: ' + IntToStr(Integer(ErrCode)));
end;
{$ENDIF}

constructor TRenderer_OpenGL3.Create(glcontext: TSDL_GLContext; MajorVersion, MinorVersion: integer);
begin
  inherited Create(glcontext, MajorVersion, MinorVersion);
end;

procedure TRenderer_OpenGL3.CheckVersion();
begin
  if (MajorVersion < 3) then
    raise Exception.Create('Could not initialize OpenGL 3.0 or later');
  Log.LogInfo('Using Modern OpenGL renderer', 'TRenderer_OpenGL3.CheckVersion');
  SupportsVAO := true;
  SupportsFBO := true;
  if ((MajorVersion > 3) or ((MajorVersion = 3) and (MinorVersion >= 3))) then
    fSupportsProjectM := true;
end;

procedure TRenderer_OpenGL3.GetShaderSource(out MainVertex, MainFragment, TextFragment, LineStripVertex, LineStripFragment: string);
var
  Header: string;
  function GetVertexShader(const ShaderSource: string): string;
  begin
    Result := Header + ShaderSource;
    Result := StringReplace(Result, '[VTX_IN]', 'in', [rfReplaceAll]);
    Result := StringReplace(Result, '[VTX_OUT]', 'out', [rfReplaceAll]);
  end;
  function GetFragmentShader(const ShaderSource: string): string;
  begin
    Result := Header + ShaderSource;
    Result := StringReplace(Result, '[FRAG_IN]', 'in', [rfReplaceAll]);
    Result := StringReplace(Result, '[FRAG_OUT_DECL]', 'out vec4 [FRAG_OUT];', [rfReplaceAll]);
    Result := StringReplace(Result, '[FRAG_OUT]', 'out_col', [rfReplaceAll]);
    Result := StringReplace(Result, '[TEXTURE_FUNC]', 'texture', [rfReplaceAll]);
  end;
begin
  if ((MajorVersion > 3) or ((MajorVersion = 3) and (MinorVersion >= 2))) then
    Header := GLSL_CORE_HEADER
  else
    Header := GLSL_COMPAT_HEADER;

  MainVertex := GetVertexShader(MAIN_VERTEX_SHADER_SOURCE);
  MainFragment := GetFragmentShader(MAIN_FRAGMENT_SHADER_SOURCE);
  TextFragment := GetFragmentShader(TEXT_FRAGMENT_SHADER_SOURCE);
  LineStripVertex := GetVertexShader(LINE_STRIP_VERTEX_SHADER_SOURCE);
  LineStripFragment := GetFragmentShader(LINE_STRIP_FRAGMENT_SHADER_SOURCE);
end;

constructor TRenderer_OpenGLES.Create(glcontext: TSDL_GLContext; MajorVersion, MinorVersion: integer);
begin
  inherited Create(glcontext, MajorVersion, MinorVersion);
end;

procedure TRenderer_OpenGLES.CheckVersion();
var
  Extensions: string;
  GL_OES_vertex_array_object: boolean;
begin
  if (MajorVersion < 2) then
    raise Exception.Create('Could not initialize OpenGL ES 2.0 or later');
  Log.LogInfo('Using OpenGL ES renderer', 'TRenderer_OpenGLES.CheckVersion');
  if (MajorVersion >= 3) then
  begin
    SupportsVAO := true;
    fSupportsProjectM := true;
  end
  else
  begin
    Extensions := Int_GetExtensionString;
    GL_OES_vertex_array_object := Int_CheckExtension(Extensions, 'GL_OES_vertex_array_object');
    if (GL_OES_vertex_array_object) then
      SupportsVAO := true;
  end;
  SupportsFBO := true;
end;

procedure TRenderer_OpenGLES.GetShaderSource(out MainVertex, MainFragment, TextFragment, LineStripVertex, LineStripFragment: string);
  function GetVertexShader(const ShaderSource: string): string;
  begin
    Result := StringReplace(ShaderSource, '[VTX_IN]', 'attribute', [rfReplaceAll]);
    Result := StringReplace(Result, '[VTX_OUT]', 'varying', [rfReplaceAll]);
  end;
  function GetFragmentShader(const ShaderSource: string): string;
  begin
    Result := ES_PRECISION_SPECIFIER + ShaderSource;
    Result := StringReplace(Result, '[FRAG_IN]', 'varying', [rfReplaceAll]);
    Result := StringReplace(Result, '[FRAG_OUT_DECL]', '', [rfReplaceAll]);
    Result := StringReplace(Result, '[FRAG_OUT]', 'gl_FragColor', [rfReplaceAll]);
    Result := StringReplace(Result, '[TEXTURE_FUNC]', 'texture2D', [rfReplaceAll]);
  end;
begin
  MainVertex := GetVertexShader(MAIN_VERTEX_SHADER_SOURCE);
  MainFragment := GetFragmentShader(MAIN_FRAGMENT_SHADER_SOURCE);
  TextFragment := GetFragmentShader(TEXT_FRAGMENT_SHADER_SOURCE);
  LineStripVertex := GetVertexShader(LINE_STRIP_VERTEX_SHADER_SOURCE);
  LineStripFragment := GetFragmentShader(LINE_STRIP_FRAGMENT_SHADER_SOURCE);
end;

constructor TRenderer_OpenGL2.Create(glcontext: TSDL_GLContext; MajorVersion, MinorVersion: integer);
begin
  inherited Create(glcontext, MajorVersion, MinorVersion);
end;

procedure TRenderer_OpenGL2.CheckVersion();
var
  Extensions: string;
begin
  if (MajorVersion < 2) then
    raise Exception.Create('Could not initialize OpenGL 2.0 or later');
  Log.LogInfo('Using Legacy OpenGL renderer', 'TRenderer_OpenGL2.CheckVersion');
  if (MajorVersion >= 3) then
    SupportsVAO := true
  else
  begin
    Extensions := Int_GetExtensionString;
    GL_ARB_vertex_array_object := Int_CheckExtension(Extensions, 'GL_ARB_vertex_array_object');
    if (GL_ARB_vertex_array_object) then
      SupportsVAO := true;
  end;
  fSupportsProjectM := false;
  SupportsFBO := false;
end;

procedure TRenderer_OpenGL2.GetShaderSource(out MainVertex, MainFragment, TextFragment, LineStripVertex, LineStripFragment: string);
  function GetVertexShader(const ShaderSource: string): string;
  begin
    Result := GLSL_LEGACY_HEADER + ShaderSource;
    Result := StringReplace(ShaderSource, '[VTX_IN]', 'attribute', [rfReplaceAll]);
    Result := StringReplace(Result, '[VTX_OUT]', 'varying', [rfReplaceAll]);
  end;
  function GetFragmentShader(const ShaderSource: string): string;
  begin
    Result := GLSL_LEGACY_HEADER + ShaderSource;
    Result := StringReplace(Result, '[FRAG_IN]', 'varying', [rfReplaceAll]);
    Result := StringReplace(Result, '[FRAG_OUT_DECL]', '', [rfReplaceAll]);
    Result := StringReplace(Result, '[FRAG_OUT]', 'gl_FragColor', [rfReplaceAll]);
    Result := StringReplace(Result, '[TEXTURE_FUNC]', 'texture2D', [rfReplaceAll]);
  end;
begin
  MainVertex := GetVertexShader(MAIN_VERTEX_SHADER_SOURCE);
  MainFragment := GetFragmentShader(MAIN_FRAGMENT_SHADER_SOURCE);
  TextFragment := GetFragmentShader(TEXT_FRAGMENT_SHADER_SOURCE);
  LineStripVertex := GetVertexShader(LINE_STRIP_VERTEX_SHADER_SOURCE);
  LineStripFragment := GetFragmentShader(LINE_STRIP_FRAGMENT_SHADER_SOURCE);
end;

end.
