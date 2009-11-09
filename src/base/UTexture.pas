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
 *
 * $URL$
 * $Id$
 *}

unit UTexture;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  gl,
  glu,
  glext,
  Classes,
  SysUtils,
  UCommon,
  UPath,
  SDL,
  SDL_Image;

type
  PTexture = ^TTexture;
  TTexture = record
    TexNum:   GLuint;
    X:        real;
    Y:        real;
    Z:        real;
    W:        real;
    H:        real;
    ScaleW:   real; // for dynamic scalling while leaving width constant
    ScaleH:   real; // for dynamic scalling while leaving height constant
    Rot:      real; // 0 - 2*pi
    Int:      real; // intensity
    ColR:     real;
    ColG:     real;
    ColB:     real;
    TexW:     real; // percentage of width to use [0..1]
    TexH:     real; // percentage of height to use [0..1]
    TexX1:    real;
    TexY1:    real;
    TexX2:    real;
    TexY2:    real;
    Alpha:    real;
    Name:     IPath; // experimental for handling cache images. maybe it's useful for dynamic skins
  end;

type
  TTextureType = (
    TEXTURE_TYPE_PLAIN,        // Plain (alpha = 1)
    TEXTURE_TYPE_TRANSPARENT,  // Alpha is used
    TEXTURE_TYPE_COLORIZED     // Alpha is used; Hue of the HSV color-model will be replaced by a new value
  );

const
  TextureTypeStr: array[TTextureType] of string = (
    'Plain',
    'Transparent',
    'Colorized'
  );

function TextureTypeToStr(TexType: TTextureType): string;
function ParseTextureType(const TypeStr: string; Default: TTextureType): TTextureType;

procedure AdjustPixelFormat(var TexSurface: PSDL_Surface; Typ: TTextureType);

type
  PTextureEntry = ^TTextureEntry;
  TTextureEntry = record
    Name:         IPath;
    Typ:          TTextureType;
    Color:        cardinal;

    // we use normal TTexture, it's easier to implement and if needed - we copy ready data
    Texture:      TTexture; // Full-size texture
    TextureCache: TTexture; // Thumbnail texture
  end;

  TTextureDatabase = class
    private
      Texture: array of TTextureEntry;
    public
      procedure AddTexture(var Tex: TTexture; Typ: TTextureType; Color: cardinal; Cache: boolean);
      function FindTexture(const Name: IPath; Typ: TTextureType; Color: cardinal): integer;
  end;

  TTextureUnit = class
    private
      TextureDatabase: TTextureDatabase;
    public
      Limit: integer;

      procedure AddTexture(var Tex: TTexture; Typ: TTextureType; Cache: boolean = false); overload;
      procedure AddTexture(var Tex: TTexture; Typ: TTextureType; Color: cardinal; Cache: boolean = false); overload;
      function GetTexture(const Name: IPath; Typ: TTextureType; FromCache: boolean = false): TTexture; overload;
      function GetTexture(const Name: IPath; Typ: TTextureType; Col: LongWord; FromCache: boolean = false): TTexture; overload;
      function LoadTexture(FromRegistry: boolean; const Identifier: IPath; Typ: TTextureType; Col: LongWord): TTexture; overload;
      function LoadTexture(const Identifier: IPath; Typ: TTextureType; Col: LongWord): TTexture; overload;
      function LoadTexture(const Identifier: IPath): TTexture; overload;
      function CreateTexture(Data: PChar; const Name: IPath; Width, Height: word; BitsPerPixel: byte): TTexture;
      procedure UnloadTexture(const Name: IPath; Typ: TTextureType; FromCache: boolean); overload;
      procedure UnloadTexture(const Name: IPath; Typ: TTextureType; Col: cardinal; FromCache: boolean); overload;
      //procedure FlushTextureDatabase();

      constructor Create;
      destructor Destroy; override;
  end;

var
  Texture: TTextureUnit;

implementation

uses
  DateUtils,
  StrUtils,
  Math,
  ULog,
  UCovers,
  UThemes,
  UImage;

procedure AdjustPixelFormat(var TexSurface: PSDL_Surface; Typ: TTextureType);
var
  TempSurface: PSDL_Surface;
  NeededPixFmt: PSDL_Pixelformat;
begin
  if      (Typ = TEXTURE_TYPE_PLAIN) then
    NeededPixFmt := @PixelFmt_RGB
  else if (Typ = TEXTURE_TYPE_TRANSPARENT) or
          (Typ = TEXTURE_TYPE_COLORIZED) then
    NeededPixFmt := @PixelFmt_RGBA
  else
    NeededPixFmt := @PixelFmt_RGB;

  if not PixelformatEquals(TexSurface^.format, NeededPixFmt) then
  begin
    TempSurface := TexSurface;
    TexSurface := SDL_ConvertSurface(TempSurface, NeededPixFmt, SDL_SWSURFACE);
    SDL_FreeSurface(TempSurface);
  end;
end;

{ TTextureDatabase }

procedure TTextureDatabase.AddTexture(var Tex: TTexture; Typ: TTextureType; Color: cardinal; Cache: boolean);
var
  TextureIndex: integer;
begin
  TextureIndex := FindTexture(Tex.Name, Typ, Color);
  if (TextureIndex = -1) then
  begin
    TextureIndex := Length(Texture);
    SetLength(Texture, TextureIndex+1);

    Texture[TextureIndex].Name  := Tex.Name;
    Texture[TextureIndex].Typ   := Typ;
    Texture[TextureIndex].Color := Color;
  end;

  if (Cache) then
    Texture[TextureIndex].TextureCache := Tex
  else
    Texture[TextureIndex].Texture      := Tex;
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

{ TTextureUnit }

constructor TTextureUnit.Create;
begin
  inherited Create;
  TextureDatabase := TTextureDatabase.Create;
end;

destructor TTextureUnit.Destroy;
begin
  TextureDatabase.Free;
  inherited Destroy;
end;

procedure TTextureUnit.AddTexture(var Tex: TTexture; Typ: TTextureType; Cache: boolean);
begin
  TextureDatabase.AddTexture(Tex, Typ, 0, Cache);
end;

procedure TTextureUnit.AddTexture(var Tex: TTexture; Typ: TTextureType; Color: cardinal; Cache: boolean);
begin
  TextureDatabase.AddTexture(Tex, Typ, Color, Cache);
end;

function TTextureUnit.LoadTexture(FromRegistry: boolean; const Identifier: IPath; Typ: TTextureType; Col: LongWord): TTexture;
begin
  // FIXME: what is the FromRegistry parameter supposed to do?
  Result := LoadTexture(Identifier, Typ, Col);
end;

function TTextureUnit.LoadTexture(const Identifier: IPath): TTexture;
begin
  Result := LoadTexture(Identifier, TEXTURE_TYPE_PLAIN, 0);
end;

function TTextureUnit.LoadTexture(const Identifier: IPath; Typ: TTextureType; Col: LongWord): TTexture;
var
  TexSurface: PSDL_Surface;
  newWidth, newHeight: integer;
  oldWidth, oldHeight: integer;
  ActTex: GLuint;
begin
  // zero texture data
  FillChar(Result, SizeOf(Result), 0);

  // load texture data into memory
  TexSurface := LoadImage(Identifier);
  if not assigned(TexSurface) then
  begin
    Log.LogError('Could not load texture: "' + Identifier.ToNative +'" with type "'+ TextureTypeToStr(Typ) +'"',
                 'TTextureUnit.LoadTexture');
    Exit;
  end;

  // convert pixel format as needed
  AdjustPixelFormat(TexSurface, Typ);

  // adjust texture size (scale down, if necessary)
  newWidth   := TexSurface.W;
  newHeight  := TexSurface.H;

  if (newWidth > Limit) then
    newWidth := Limit;

  if (newHeight > Limit) then
    newHeight := Limit;

  if (TexSurface.W > newWidth) or (TexSurface.H > newHeight) then
    ScaleImage(TexSurface, newWidth, newHeight);

  // now we might colorize the whole thing
  if (Typ = TEXTURE_TYPE_COLORIZED) then
    ColorizeImage(TexSurface, Col);

  // save actual dimensions of our texture
  oldWidth  := newWidth;
  oldHeight := newHeight;

  // make texture dimensions be powers of 2
  newWidth  := Round(Power(2, Ceil(Log2(newWidth))));
  newHeight := Round(Power(2, Ceil(Log2(newHeight))));
  if (newHeight <> oldHeight) or (newWidth <> oldWidth) then
    FitImage(TexSurface, newWidth, newHeight);

  // at this point we have the image in memory...
  // scaled so that dimensions are powers of 2
  // and converted to either RGB or RGBA

  // if we got a Texture of Type Plain, Transparent or Colorized,
  // then we're done manipulating it
  // and could now create our openGL texture from it

  // prepare OpenGL texture
  glGenTextures(1, @ActTex);

  glBindTexture(GL_TEXTURE_2D, ActTex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  // load data into gl texture
  if (Typ = TEXTURE_TYPE_TRANSPARENT) or
     (Typ = TEXTURE_TYPE_COLORIZED) then
  begin
    {$IFDEF FPC_BIG_ENDIAN}
    glTexImage2D(GL_TEXTURE_2D, 0, 4, newWidth, newHeight, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV, TexSurface.pixels);
    {$ELSE}
    glTexImage2D(GL_TEXTURE_2D, 0, 4, newWidth, newHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, TexSurface.pixels);
    {$ENDIF}
  end
  else //if Typ = TEXTURE_TYPE_PLAIN then
  begin
    {$IFDEF FPC_BIG_ENDIAN}
    glTexImage2D(GL_TEXTURE_2D, 0, 3, newWidth, newHeight, 0, GL_BGR, GL_UNSIGNED_BYTE, TexSurface.pixels);
    {$ELSE}
    glTexImage2D(GL_TEXTURE_2D, 0, 3, newWidth, newHeight, 0, GL_RGB, GL_UNSIGNED_BYTE, TexSurface.pixels);
    {$ENDIF}
  end;

  // setup texture struct
  with Result do
  begin
    X := 0;
    Y := 0;
    Z := 0;
    W := oldWidth;
    H := oldHeight;
    ScaleW := 1;
    ScaleH := 1;
    Rot := 0;
    TexNum := ActTex;
    TexW := oldWidth / newWidth;
    TexH := oldHeight / newHeight;

    Int   := 1;
    ColR  := 1;
    ColG  := 1;
    ColB  := 1;
    Alpha := 1;

    // new test - default use whole texure, taking TexW and TexH as const and changing these
    TexX1 := 0;
    TexY1 := 0;
    TexX2 := 1;
    TexY2 := 1;

    Name := Identifier;
  end;

  SDL_FreeSurface(TexSurface);
end;

function TTextureUnit.GetTexture(const Name: IPath; Typ: TTextureType; FromCache: boolean): TTexture;
begin
  Result := GetTexture(Name, Typ, 0, FromCache);
end;

function TTextureUnit.GetTexture(const Name: IPath; Typ: TTextureType; Col: LongWord; FromCache: boolean): TTexture;
var
  TextureIndex: integer;
begin
  if (Name.IsUnset) then
  begin
    // zero texture data
    FillChar(Result, SizeOf(Result), 0);
    Exit;
  end;

  if (FromCache) then
  begin
    // use texture
    TextureIndex := TextureDatabase.FindTexture(Name, Typ, Col);
    if (TextureIndex > -1) then
      Result := TextureDatabase.Texture[TextureIndex].TextureCache;
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
    TextureDatabase.Texture[TextureIndex].Texture.TexNum      := 0;
    TextureDatabase.Texture[TextureIndex].TextureCache.TexNum := 0;
  end;

  // load full texture
  if (TextureDatabase.Texture[TextureIndex].Texture.TexNum = 0) then
    TextureDatabase.Texture[TextureIndex].Texture := LoadTexture(false, Name, Typ, Col);

  // use texture
  Result := TextureDatabase.Texture[TextureIndex].Texture;
end;

function TTextureUnit.CreateTexture(Data: PChar; const Name: IPath; Width, Height: word; BitsPerPixel: byte): TTexture;
var
  //Error:     integer;
  ActTex:    GLuint;
begin
  glGenTextures(1, @ActTex); // ActText = new texture number
  glBindTexture(GL_TEXTURE_2D, ActTex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  {$IFDEF FPC_BIG_ENDIAN}
  glTexImage2D(GL_TEXTURE_2D, 0, 3, Width, Height, 0, GL_BGR, GL_UNSIGNED_BYTE, Data);
  {$ELSE}
  glTexImage2D(GL_TEXTURE_2D, 0, 3, Width, Height, 0, GL_RGB, GL_UNSIGNED_BYTE, Data);
  {$ENDIF}

{
  if Mipmapping then
  begin
    Error := gluBuild2DMipmaps(GL_TEXTURE_2D, 3, W, H, GL_RGB, GL_UNSIGNED_BYTE, @Data[0]);
// FPC_BIG_ENDIAN   Error := gluBuild2DMipmaps(GL_TEXTURE_2D, 3, W, H, GL_BGR, GL_UNSIGNED_BYTE, @Data[0]);
    if Error > 0 then
      Log.LogError('gluBuild2DMipmaps() failed', 'TTextureUnit.CreateTexture');
  end;
}

  Result.X := 0;
  Result.Y := 0;
  Result.Z := 0;
  Result.W := 0;
  Result.H := 0;
  Result.ScaleW := 1;
  Result.ScaleH := 1;
  Result.Rot := 0;
  Result.TexNum := ActTex;
  Result.TexW := 1;
  Result.TexH := 1;

  Result.Int := 1;
  Result.ColR := 1;
  Result.ColG := 1;
  Result.ColB := 1;
  Result.Alpha := 1;

  // new test - default use whole texure, taking TexW and TexH as const and changing these
  Result.TexX1 := 0;
  Result.TexY1 := 0;
  Result.TexX2 := 1;
  Result.TexY2 := 1;

  Result.Name := Name;
end;

procedure TTextureUnit.UnloadTexture(const Name: IPath; Typ: TTextureType; FromCache: boolean);
begin
  UnloadTexture(Name, Typ, 0, FromCache);
end;

procedure TTextureUnit.UnloadTexture(const Name: IPath; Typ: TTextureType; Col: cardinal; FromCache: boolean);
var
  T:      integer;
  TexNum: GLuint;
begin
  T := TextureDatabase.FindTexture(Name, Typ, Col);

  if not FromCache then
  begin
    TexNum := TextureDatabase.Texture[T].Texture.TexNum;
    if TexNum > 0 then
    begin
      glDeleteTextures(1, PGLuint(@TexNum));
      TextureDatabase.Texture[T].Texture.TexNum := 0;
      //Log.LogError('Unload texture no '+IntToStr(TexNum));
    end;
  end
  else
  begin
    TexNum := TextureDatabase.Texture[T].TextureCache.TexNum;
    if TexNum > 0 then
    begin
      glDeleteTextures(1, @TexNum);
      TextureDatabase.Texture[T].TextureCache.TexNum := 0;
      //Log.LogError('Unload texture cache no '+IntToStr(TexNum));
    end;
  end;
end;

(* This needs some work
procedure TTextureUnit.FlushTextureDatabase();
var
  i: integer;
  Tex: ^TTexture;
begin
  for i := 0 to High(TextureDatabase.Texture) do
  begin
    // only delete non-cached entries
    if (TextureDatabase.Texture[i].Texture.TexNum > 0) then
    begin
      Tex := @TextureDatabase.Texture[i].Texture;
      glDeleteTextures(1, PGLuint(Tex^.TexNum));
      Tex^.TexNum := 0;
    end;
  end;
end;
*)

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
  Log.LogWarn('Unknown texture type: "' + TypeStr + '". Using default texture type "' + TextureTypeToStr(Default) + '"', 'ParseTextureType');
  Result := Default;
end;

end.
