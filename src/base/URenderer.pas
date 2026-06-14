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

unit URenderer;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  sdl2,
  UPath;

const
  {$IF Defined(UNIX) and (not Defined(DARWIN)) and (Defined(CPUARM) or Defined(CPUAARCH64))}
  DEFAULT_RENDERRER = 'gles';
  {$ELSE}
  DEFAULT_RENDERRER = 'gl';
  {$ENDIF}
  REFLECTION_HEIGHT = 0.5; // Height of the reflection relative to the texture height
  REFLECTION_ALPHA_DIFF = 0.3; // Starting alpha of the top of reflection (subtracted from texture's alpha)

type
  GradientDirection = (
    gdNone,
    gdVertical,
    gdHorizontal
  );
  ClippingDirection = (
    cdLeft,
    cdRight
  );
  TTextureType = (
    TEXTURE_TYPE_PLAIN,        // Plain (alpha = 1)
    TEXTURE_TYPE_TRANSPARENT,  // Alpha is used
    TEXTURE_TYPE_COLORIZED     // Alpha is used; Hue of the HSV color-model will be replaced by a new value
  );
  TextureID = cardinal;

  TQuad = record
    X: single;
    Y: single;
    W: single;
    H: single;
    Z: single;

    // Some quads can have a color and/or alpha gradient. If the quad does not have a color/alpha gradient,
    // then the color/alpha values are stored in ColR,G,B,Alpha and ColR2,G2,B2 and Alpha2 are unused.
    // If there is a color/alpha gradient, top/left value is stored in ColR,G,B,Alpha, and bottom/right
    // value is stored in ColR2,B2,G2,Alpha2
    Gradient: GradientDirection;
    ColR: single;
    ColG: single;
    ColB: single;
    Alpha: single;
    ColR2: single;
    ColG2: single;
    ColB2: single;
    Alpha2: single;
  end;
  TQuadList = array of TQuad;

  TTriangle = record
    X1: single;
    Y1: single;
    X2: single;
    Y2: single;
    X3: single;
    Y3: single;
    Z: single;
    ColR: single;
    ColG: single;
    ColB: single;
    Alpha: single;
  end;
  TTriangleList = array of TTriangle;

  TLineInfo = record
    X1: single;
    Y1: single;
    X2: single;
    Y2: single;
    Z: single;
    Thickness: single; // Specified in Window (pixel) coordinates, NOT the virtual 800x600 vertices
    ColR: single;
    ColG: single;
    ColB: single;
    Alpha: single;
  end;
  TLineList = array of TLineInfo;

  TParticleInfo = record
    X: single;
    Y: single;
    W: single;
    H: single;
    ColR: single;
    ColG: single;
    ColB: single;
    Alpha: single;
    TexX1: single;
    TexY1: single;
    TexX2: single;
    TexY2: single;
  end;
  TParticleList = array of TParticleInfo;

  TPoint = packed record
    X: single;
    Y: single;
  end;
  TPointList = array of TPoint;

  TTexture = class
    protected
      OwnsTex: boolean;
      fIsEmpty: boolean;

      constructor Create(const Identifier: IPath);
      procedure Clone(T: TTexture); overload;

    public
      X: single;
      Y: single;
      Z: single;
      W: single;
      H: single;
      Shear: single;
      Int: single; // intensity
      ColR, ColG, ColB: single;
      TexX1: single;
      TexY1: single;
      TexX2: single;
      TexY2: single;

      // Some textures can have an alpha gradient. If the texture does NOT have an alpha gradient,
      // then the alpha value is stored in Alpha and Alpha2 is unused. If there is an alpha gradient,
      //  top/left value is storedin alpha, and bottom/right value is stored in Alpha2
      AlphaGradient: GradientDirection;
      Alpha: single;
      Alpha2: single;

      Name: IPath;
      Reflection: boolean;
      ReflectionSpacing: single;
      ReflectionHeight: single;
      ReflectionAlphaDiff: single;
      ReflectionTexY1: single;
      ReflectionTexY2: single;

      // Update the pixel data
      procedure UpdateData(Data: PByte; Width, Height: word; PixelsPerRow: integer; Typ: TTextureType); virtual; abstract;

      // Copy the framebuffer to the texture. Used for the screen transitions
      procedure CopyFrameBuffer(X, Y: integer; Width, Height: cardinal); virtual; abstract;

      // Free the GPU memory, but not the TTexture object itself.
      procedure Release(); virtual; abstract;

      // Copies the TTexture object, but does NOT make a copy of the GPU data. Original TTexture
      // retains ownership of the GPU data, which will be freed when that TTexture is freed
      function Clone(): TTexture; overload; virtual; abstract;
      destructor Destroy; override;

      // Returns true if no GPU data is assigned
      property IsEmpty: boolean read fIsEmpty;

  end;

const
  TextureTypeStr: array[TTextureType] of string = (
    'Plain',
    'Transparent',
    'Colorized'
  );

  CLEAR_COLOR = 1;
  CLEAR_DEPTH = 2;

function TextureTypeToStr(TexType: TTextureType): string;
function ParseTextureType(const TypeStr: string; Default: TTextureType): TTextureType;
procedure AdjustPixelFormat(var TexSurface: PSDL_Surface; Typ: TTextureType);
type
  PTextureEntry = ^TTextureEntry;
  TTextureEntry = record
    Name:         IPath;
    Typ:          TTextureType;
    Color:        cardinal;
    Texture:      TTexture; // Full-size texture
  end;

  TTextureDatabase = class
    private
      Texture: array of TTextureEntry;
    public
      function FindTexture(const Name: IPath; Typ: TTextureType; Color: cardinal): integer;

      destructor Destroy; override;
  end;

  TRenderer = class
    protected
      TextureDatabase: TTextureDatabase;
      SWRendering: boolean;
      fSupportsProjectM: boolean;

      function LoadTexture(Data: PByte; W, H: integer; const Identifier: IPath; Typ: TTextureType): TTexture; overload; virtual; abstract;
      function CreateEmptyTexture(const Identifier: IPath): TTexture; virtual; abstract;

    public

      // Drawing functions
      procedure DrawTexture(Texture: TTexture); virtual; abstract;
      procedure DrawGlyph(Texture: TTexture); virtual; abstract;
      procedure DrawQuads(QuadList: TQuadList); virtual; abstract;
      procedure DrawQuad(X, Y, Z, W, H, ColR, ColG, ColB, Alpha: single);
      procedure DrawTriangles(TriangleList: TTriangleList); virtual; abstract;
      procedure DrawTriangle(X1, Y1, X2, Y2, X3, Y3, Z, ColR, ColG, ColB, Alpha: single);
      procedure DrawLines(LineList: TLineList); virtual; abstract;
      procedure DrawLine(X1, Y1, X2, Y2, Z, LineThickness, ColR, ColG, ColB, Alpha: single);
      procedure DrawParticles(Texture: TTexture; ParticleList: TParticleList); virtual; abstract;
      procedure DrawBoundedBox(X1, Y1, X2, Y2, Z, LineThickness, ColR, ColG, ColB, Alpha: single); virtual;
      procedure DrawLineStrip(PointList: TPointList; ScaleX, ScaleY, TranslateX, TranslateY, ColR, ColG, ColB, Alpha: single); virtual; abstract;

      // State changes
      procedure SetOrthographicProjection(Left, Right, Bottom, Top, NearVal, FarVal: single); virtual; abstract;
      procedure SetViewPort(x, y: integer; width, height: cardinal); virtual; abstract;
      procedure SetBlend(Enabled: boolean); virtual; abstract;
      function GetBlend(): boolean; virtual; abstract;
      procedure SetDepthTest(Enabled: boolean); virtual; abstract;
      function GetDepthTest(): boolean; virtual; abstract;
      procedure SetScissorRect(X, Y: integer; W, H: cardinal); virtual; abstract;
      procedure SetScissorTest(Scissor: boolean); virtual; abstract;
      function GetScissorTest(): boolean; virtual; abstract;
      procedure SetVSync(Enabled: boolean); virtual; abstract;
      function GetVSync(): boolean; virtual; abstract;
      procedure SwapBuffers(); virtual; abstract;
      function GetError(): boolean; virtual; abstract;
      function GetErrorCode(): integer; virtual; abstract;
      procedure SetClearColor(R, G, B, A: single); virtual; abstract;
      procedure ClearFrameBuffer(Buffers: cardinal); virtual; abstract;

      // Clip all vertices to the left/right of X. Used for the lyrics 'slide' effect. X is specified in the
      // virtual 800x600 grid and the implementation should take into account the actual screen being drawn to
      procedure SetTextClipBoundary(X: single; Direction: ClippingDirection); virtual; abstract;
      procedure SetClipText(Enabled: boolean); virtual; abstract;

      procedure ResetState(); virtual;

      // Texture loading functions
      function GetTexture(const Name: IPath; Typ: TTextureType): TTexture; overload;
      function GetTexture(const Name: IPath; Typ: TTextureType; Col: LongWord): TTexture; overload;
      function LoadTexture(const Identifier: IPath; Typ: TTextureType; Col: LongWord): TTexture; overload;
      function LoadTexture(const Identifier: IPath): TTexture; overload;
      function LoadGlyph(Data: PByte; W, H: integer): TTexture; overload; virtual; abstract;

      // Data should be in RGB format (no alpha)
      function CreateTexture(Data: PChar; const Name: IPath; Width, Height: word; Typ: TTextureType): TTexture;
      procedure UnloadTexture(const Name: IPath; Typ: TTextureType); overload;
      procedure UnloadTexture(const Name: IPath; Typ: TTextureType; Col: cardinal); overload;

      function GetFrameBufferData(out RowSize: integer): PByte; virtual; abstract;

      constructor Create;
      destructor Destroy; override;

      property Blend: boolean read GetBlend write SetBlend;
      property DepthTest: boolean read GetDepthTest write SetDepthTest;
      property ScissorTest: boolean read GetScissorTest write SetScissorTest;
      property ClipText: boolean write SetClipText;
      property SoftwareRendering: boolean read SWRendering;
      property VSync: boolean read GetVSync write SetVSync;
      property SupportsProjectM: boolean read fSupportsProjectM;
      property Error: boolean read GetError;
      property ErrorCode: integer read GetErrorCode;
  end;

procedure PreInitRenderer();
procedure InitRenderer();

var
  Renderer: TRenderer;

implementation
uses
  math,
  sysutils,
  StrUtils,
  UCommandLine,
  UGraphic,
  UImage,
  ULog,
  URenderer_OpenGL;

const
  Limit = 1920;

constructor TTexture.Create(const Identifier: IPath);
begin
  inherited Create;
  OwnsTex := true;
  X := 0;
  Y := 0;
  Z := 0;
  W := 0;
  H := 0;
  ColR := 1;
  ColG := 1;
  ColB := 1;
  Int := 1;
  AlphaGradient := gdNone;
  Alpha := 1;
  Alpha2 := 1;
  TexX1 := 0;
  TexY1 := 0;
  TexX2 := 1;
  TexY2 := 1;
  Name := Identifier;
  Reflection := false;
  ReflectionSpacing := 0;
  ReflectionHeight := 0.5;
  ReflectionAlphaDiff := REFLECTION_ALPHA_DIFF;
  ReflectionTexY1 := TexY2;
  ReflectionTexY2 := TexY1 + ReflectionHeight;
  fIsEmpty := true;
end;

procedure TTexture.Clone(T: TTexture);
begin
  T.OwnsTex := false;
  T.fIsEmpty := fIsEmpty;
  T.X := X;
  T.Y := Y;
  T.Z := Z;
  T.W := W;
  T.H := H;
  T.Int := Int;
  T.ColR := ColR;
  T.ColG := ColG;
  T.ColB := ColB;
  T.TexX1 := TexX1;
  T.TexY1 := TexY1;
  T.TexX2 := TexX2;
  T.TexY2 := TexY2;
  T.AlphaGradient := AlphaGradient;
  T.Alpha := Alpha;
  T.Alpha2 := Alpha2;
  T.Reflection := Reflection;
  T.ReflectionSpacing := ReflectionSpacing;
  T.ReflectionHeight := ReflectionHeight;
  T.ReflectionAlphaDiff := ReflectionAlphaDiff;
  T.ReflectionTexY1 := ReflectionTexY1;
  T.ReflectionTexY2 := ReflectionTexY2;
end;

destructor TTexture.Destroy();
begin
  inherited;
end;

constructor TRenderer.Create();
begin
  inherited;
  SWRendering := false;
  TextureDatabase := TTextureDatabase.Create;
end;

destructor TRenderer.Destroy();
begin
  TextureDatabase.Free;
  inherited;
end;

function TRenderer.LoadTexture(const Identifier: IPath): TTexture;
begin
  Result := LoadTexture(Identifier, TEXTURE_TYPE_PLAIN, 0);
end;

function TRenderer.LoadTexture(const Identifier: IPath; Typ: TTextureType; Col: LongWord): TTexture;
var
  TexSurface: PSDL_Surface;
  newWidth, newHeight: integer;
  ScaleBy: real;
begin
  // load texture data into memory
  if (Identifier = nil) or (Identifier.IsUnset) then
  begin
    Result := CreateEmptyTexture(Identifier);
    Exit;
  end;
  TexSurface := LoadImage(Identifier);
  if not assigned(TexSurface) then
  begin
    Log.LogError('Could not load texture: "' + Identifier.ToNative +'" with type "'+ TextureTypeToStr(Typ) +'"',
                 'TRenderer.LoadTexture');
    Result := CreateEmptyTexture(Identifier);
    Exit;
  end;

  // convert pixel format as needed
  AdjustPixelFormat(TexSurface, Typ);
  if not assigned(TexSurface) then
  begin
    Log.LogError('Could not convert surface', 'TRenderer.LoadTexture');
    Result := CreateEmptyTexture(Identifier);
    SDL_FreeSurface(TexSurface);
    Exit;
  end;

  // adjust texture size (scale down, if necessary) while keeping aspect ratio
  ScaleBy := 1;
  if (Max(TexSurface.W, TexSurface.H) > Limit) then
    ScaleBy := Max(TexSurface.W, TexSurface.H) / Limit;

  newWidth   := Floor(TexSurface.W / ScaleBy);
  newHeight  := Floor(TexSurface.H / ScaleBy);

  if (TexSurface.W > newWidth) or (TexSurface.H > newHeight) then
    ScaleImage(TexSurface, newWidth, newHeight);

  // now we might colorize the whole thing
  if (Typ = TEXTURE_TYPE_COLORIZED) then
    ColorizeImage(TexSurface, Col);

  SDL_LockSurface(TexSurface); // unlocked by SDL_FreeSurface
  Result := LoadTexture(TexSurface^.pixels, TexSurface^.w, TexSurface^.h, Identifier, Typ);
  SDL_FreeSurface(TexSurface);
end;

function TRenderer.GetTexture(const Name: IPath; Typ: TTextureType): TTexture;
begin
  Result := GetTexture(Name, Typ, 0);
end;

function TRenderer.GetTexture(const Name: IPath; Typ: TTextureType; Col: LongWord): TTexture;
var
  TextureIndex: integer;
begin
  if (Name = nil) or (Name.IsUnset) then
  begin
    Result := CreateEmptyTexture(Name);
    Exit;
  end;

  // find texture entry in database
  TextureIndex := TextureDatabase.FindTexture(Name, Typ, Col);
  if (TextureIndex = -1) then
  begin
    // create texture entry in database
    TextureIndex := Length(TextureDatabase.Texture);
    SetLength(TextureDatabase.Texture, TextureIndex+1);

    TextureDatabase.Texture[TextureIndex].Name  := Name;
    TextureDatabase.Texture[TextureIndex].Typ   := Typ;
    TextureDatabase.Texture[TextureIndex].Color := Col;

    // inform database that no textures have been loaded into memory
    TextureDatabase.Texture[TextureIndex].Texture      := nil;
  end;

  // load full texture
  if (TextureDatabase.Texture[TextureIndex].Texture = nil) then
    TextureDatabase.Texture[TextureIndex].Texture := LoadTexture(Name, Typ, Col);

  // use texture
  Result := TextureDatabase.Texture[TextureIndex].Texture.Clone;
end;

function TRenderer.CreateTexture(Data: PChar; const Name: IPath; Width, Height: word; Typ: TTextureType): TTexture;
begin
  Result := LoadTexture(PByte(Data), Width, Height, Name, Typ);
end;

procedure TRenderer.UnloadTexture(const Name: IPath; Typ: TTextureType);
begin
  UnloadTexture(Name, Typ, 0);
end;

procedure TRenderer.UnloadTexture(const Name: IPath; Typ: TTextureType; Col: cardinal);
var
  T:      integer;
begin
  if name = nil then
    Exit;
  T := TextureDatabase.FindTexture(Name, Typ, Col);
  if T < 0 then
    Exit;
  FreeAndNil(TextureDatabase.Texture[T].Texture);
end;

// Convenience function to draw a single quad
procedure TRenderer.DrawQuad(X, Y, Z, W, H, ColR, ColG, ColB, Alpha: single);
var
  QuadList: TQuadList;
begin
  SetLength(QuadList, 1);
  QuadList[0].X := X;
  QuadList[0].Y := Y;
  QuadList[0].Z := Z;
  QuadList[0].W := W;
  QuadList[0].H := H;
  QuadList[0].ColR := ColR;
  QuadList[0].ColG := ColG;
  QuadList[0].ColB := ColB;
  QuadList[0].Gradient := gdNone;
  QuadList[0].Alpha := Alpha;
  DrawQuads(QuadList);
end;

// Convenience function to draw a single triangle
procedure TRenderer.DrawTriangle(X1, Y1, X2, Y2, X3, Y3, Z, ColR, ColG, ColB, Alpha: single);
var
  TriangleList: TTriangleList;
begin
  SetLength(TriangleList, 1);
  TriangleList[0].X1 := X1;
  TriangleList[0].Y1 := Y1;
  TriangleList[0].X2 := X2;
  TriangleList[0].Y2 := Y2;
  TriangleList[0].X3 := X3;
  TriangleList[0].Y3 := Y3;
  TriangleList[0].Z := Z;
  TriangleList[0].ColR := ColR;
  TriangleList[0].ColG := ColG;
  TriangleList[0].ColB := ColB;
  TriangleList[0].Alpha := Alpha;
  DrawTriangles(TriangleList);
end;

// Convenience function to draw a single line
procedure TRenderer.DrawLine(X1, Y1, X2, Y2, Z, LineThickness, ColR, ColG, ColB, Alpha: single);
var
  LineList: TLineList;
begin
  SetLength(LineList, 1);
  LineList[0].X1 := X1;
  LineList[0].Y1 := Y1;
  LineList[0].X2 := X2;
  LineList[0].Y2 := Y2;
  LineList[0].Z := Z;
  LineList[0].Thickness := LineThickness;
  LineList[0].ColR := ColR;
  LineList[0].ColG := ColG;
  LineList[0].ColB := ColB;
  LineList[0].Alpha := Alpha;
  DrawLines(LineList);
end;

// To draw a bounded box, we break down the 4 segments into quads based on the requested line thickness (in pixels)
procedure TRenderer.DrawBoundedBox(X1, Y1, X2, Y2, Z, LineThickness, ColR, ColG, ColB, Alpha: single);
var
  QuadList: TQuadList;
  LineThicknessPixelW, LineThicknessPixelH: single;
  HalfLineThicknessPixelW, HalfLineThicknessPixelH : single;
begin
  SetLength(QuadList, 4);

  // Convert line thickness from window pixels to units of 800x600 vertices
  LineThicknessPixelW := LineThickness * PixelW;
  LineThicknessPixelH := LineThickness * PixelH;
  HalfLineThicknessPixelW := 0.5 * LineThicknessPixelW;
  HalfLineThicknessPixelH := 0.5 * LineThicknessPixelH;

  // Snap vectices to nearest window (pixel) coordinates to prevent quantization artifacts
  X1 := Round((X1 - HalfLineThicknessPixelW) * VertexW) * PixelW;
  X2 := Round((X2 - HalfLineThicknessPixelW) * VertexW) * PixelW;
  Y1 := Round((Y1 - HalfLineThicknessPixelH) * VertexH) * PixelH;
  Y2 := Round((Y2 - HalfLineThicknessPixelH) * VertexH) * PixelH;

  // Top Segment
  QuadList[0].X := X1;
  QuadList[0].Y := Y1;
  QuadList[0].Z := Z;
  QuadList[0].W := X2 + LineThicknessPixelW - X1;
  QuadList[0].H := LineThicknessPixelH;
  QuadList[0].Gradient := gdNone;
  QuadList[0].ColR := ColR;
  QuadList[0].ColG := ColG;
  QuadList[0].ColB := ColB;
  QuadList[0].Alpha := Alpha;

  // Left segment
  QuadList[1].X := X1;
  QuadList[1].Y := Y1 + LineThicknessPixelH;
  QuadList[1].Z := Z;
  QuadList[1].W := LineThicknessPixelW;
  QuadList[1].H := Y2 - (Y1 + LineThicknessPixelH);
  QuadList[1].Gradient := gdNone;
  QuadList[1].ColR := ColR;
  QuadList[1].ColG := ColG;
  QuadList[1].ColB := ColB;
  QuadList[1].Alpha := Alpha;

  // Bottom segment
  QuadList[2].X := X1;
  QuadList[2].Y := Y2;
  QuadList[2].Z := Z;
  QuadList[2].W := X2 - X1 + LineThicknessPixelW;
  QuadList[2].H := LineThicknessPixelH;
  QuadList[2].Gradient := gdNone;
  QuadList[2].ColR := ColR;
  QuadList[2].ColG := ColG;
  QuadList[2].ColB := ColB;
  QuadList[2].Alpha := Alpha;

  // Right segment
  QuadList[3].X := X2;
  QuadList[3].Y := Y1 + LineThicknessPixelH;
  QuadList[3].Z := Z;
  QuadList[3].W := LineThicknessPixelW;
  QuadList[3].H := Y2 - (Y1 + LineThicknessPixelH);
  QuadList[3].Gradient := gdNone;
  QuadList[3].ColR := ColR;
  QuadList[3].ColG := ColG;
  QuadList[3].ColB := ColB;
  QuadList[3].Alpha := Alpha;

  Renderer.DrawQuads(QuadList);
end;

procedure TRenderer.ResetState();
begin
  Blend := true;
  DepthTest := true;
end;

{
 * SDL requires that some OpenGL-related attributes are set before creating the Window. So we
 * set these based on the intended rendering backend
}
procedure PreInitRenderer();
var
  ValidRenderers: array of string;
begin
  ValidRenderers := ['gl', 'gles' {$IFDEF DEBUG_MODE},'gl2'{$ENDIF}];
  if Params.Renderer.IsEmpty() then
    Params.Renderer := DEFAULT_RENDERRER
  else if (IndexStr(Params.Renderer, ValidRenderers) = -1) then
  begin
    Log.LogError('Requested renderer  unavailable, defaulting to ' + DEFAULT_RENDERRER);
    Params.Renderer := DEFAULT_RENDERRER;
  end;
  if (Params.Renderer = 'gles') then
  begin
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 0);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_ES);
  end
  else if (Params.Renderer = 'gl') then
  begin
    // Profile Mask
    {$IF Defined(DARWIN) or Defined(UseProjectM)}
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
    {$ELSE}
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_FLAGS, SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG);
    {$ENDIF}

    // Major Version
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);

    // Minor Version
    {$IF Defined(UseProjectM)}
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
    {$ELSEIF Defined(DARWIN)}
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 2);
    {$ELSE}
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 0);
    {$ENDIF}
  end;
end;

{
 * Initialize the renderer based on the type of SDL window that was created
}
procedure InitRenderer();
var
  Profile: integer;
  MajorVersion: integer;
  MinorVersion: integer;
  glcontext: TSDL_GLContext;
begin
  glcontext := SDL_GL_CreateContext(Screen);
  if (glcontext = nil) then
    raise Exception.Create('Failed to obtain OpenGL context');
  SDL_GL_GetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, @MajorVersion);
  SDL_GL_GetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, @MinorVersion);
  SDL_GL_GetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, @Profile);
  if (Profile = SDL_GL_CONTEXT_PROFILE_ES) then
    Renderer := TRenderer_OpenGLES.Create(glcontext, MajorVersion, MinorVersion)
  else if (MajorVersion >= 3) then
    Renderer := TRenderer_OpenGL3.Create(glcontext, MajorVersion, MinorVersion)
  else
    Renderer := TRenderer_OpenGL2.Create(glcontext, MajorVersion, MinorVersion);
end;

destructor TTextureDatabase.Destroy();
var
  I: integer;
begin
  for I := Low(Texture) to High(Texture) do
    Texture[I].Texture.Free;
  inherited;
end;

function TTextureDatabase.FindTexture(const Name: IPath; Typ: TTextureType; Color: cardinal): integer;
var
  TextureIndex: integer;
  CurrentTexture: PTextureEntry;
begin
  Result := -1;
  for TextureIndex := 0 to High(Texture) do
  begin
    CurrentTexture := @Texture[TextureIndex];
    if (CurrentTexture.Name.Equals(Name)) and
       (CurrentTexture.Typ = Typ) then
    begin
      // colorized textures must match in their color too
      if (CurrentTexture.Typ <> TEXTURE_TYPE_COLORIZED) or
         (CurrentTexture.Color = Color) then
      begin
        Result := TextureIndex;
        Break;
      end;
    end;
  end;
end;

function TextureTypeToStr(TexType: TTextureType): string;
begin
  Result := TextureTypeStr[TexType];
end;

function ParseTextureType(const TypeStr: string; Default: TTextureType): TTextureType;
var
  TextureType:   TTextureType;
  UpCaseStr: string;
begin
  UpCaseStr := UpperCase(TypeStr);
  for TextureType := Low(TextureTypeStr) to High(TextureTypeStr) do
  begin
    if (UpCaseStr = UpperCase(TextureTypeStr[TextureType])) then
    begin
      Result := TextureType;
      Exit;
    end;
  end;
  //Log.LogInfo('Unknown texture type: "' + TypeStr + '". Using default texture type "'
  //    + TextureTypeToStr(Default) + '"', 'UTexture.ParseTextureType');
  Result := Default;
end;

procedure AdjustPixelFormat(var TexSurface: PSDL_Surface; Typ: TTextureType);
var
  TempSurface: PSDL_Surface;
  NeededPixFmt: UInt32;
begin
  if      (Typ = TEXTURE_TYPE_PLAIN) then
    NeededPixFmt := SDL_PIXELFORMAT_RGB24
  else if (Typ = TEXTURE_TYPE_TRANSPARENT) or
          (Typ = TEXTURE_TYPE_COLORIZED) then
    NeededPixFmt := SDL_PIXELFORMAT_RGBA32
  else
    NeededPixFmt := SDL_PIXELFORMAT_RGB24;

  if not (TexSurface^.format.format = NeededPixFmt) then
  begin
    TempSurface := TexSurface;
    TexSurface := SDL_ConvertSurfaceFormat(TempSurface, NeededPixFmt, 0);
    SDL_FreeSurface(TempSurface);
  end;
end;

end.
