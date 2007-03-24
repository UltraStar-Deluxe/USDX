unit UTexture;

// Plain (alpha = 1)
// Transparent
// Transparent Range
// Font (white is drawn, black is transparent)
// Font Outline (Font with darker outline)
// Font Outline 2 (Font with darker outline)
// Font Black (black is drawn, white is transparent)
// Font Gray (gray is drawn, white is transparent)
// Arrow (for arrows, white is white, gray has color, black is transparent);

interface
uses OpenGL12, Windows, Math, Classes, SysUtils, Graphics, JPEG, UThemes, PNGImage;

procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
//procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;
//function  gluBuild2DMipmaps (target: GLenum; components, width, height: GLint;
//                             format, atype: GLenum; data: Pointer): Integer; stdcall; external glu32;
//procedure glCopyTexImage2D(target: GLenum; level: GLint; internalFormat: GLenum; x, y: GLint; width, height: GLsizei; border: GLint); stdcall; external opengl32;


type
  TTexture = record
    TexNum:   integer;
    X:        real;
    Y:        real;
    Z:        real; // new
    W:        real;
    H:        real;
    ScaleW:   real; // for dynamic scalling while leaving width constant
    ScaleH:   real; // for dynamic scalling while leaving height constant
    Rot:      real; // 0 - 2*pi
    Int:      real; // intensity
    ColR:     real;
    ColG:     real;
    ColB:     real;
    TexW:     real; // used?
    TexH:     real; // used?
    TexX1:    real;
    TexY1:    real;
    TexX2:    real;
    TexY2:    real;
    Alpha:    real;
    Name:     string; // 0.5.0: experimental for handling cache images. maybe it's useful for dynamic skins
  end;

  TTextureEntry = record
    Name:       string;
    Typ:        string;

    // we use normal TTexture, it's easier to implement and if needed - we copy ready data
    Texture:        TTexture;
    TextureCache:   TTexture; // 0.5.0
  end;

  TTextureDatabase = record
    Texture:    array of TTextureEntry;
  end;

  TTextureUnit = class
    Limit:      integer;
    CreateCacheMipmap:  boolean;

//    function GetNumberFor
    function GetTexture(Name, Typ: string): TTexture; overload;
    function GetTexture(Name, Typ: string; FromCache: boolean): TTexture; overload;
    function FindTexture(Name: string): integer;
    function LoadTexture(FromRegistry: boolean; Nazwa, Format, Typ: PChar; Col: LongWord): TTexture; overload;
    function LoadTexture(Nazwa, Format, Typ: PChar; Col: LongWord): TTexture; overload;
    function LoadTexture(Nazwa: string): TTexture; overload;
    function CreateTexture(var Data: array of byte; Name: string; W, H: word; Bits: byte): TTexture;
    procedure UnloadTexture(Name: string; FromCache: boolean);
  end;

var
  Texture:          TTextureUnit;
  TextureDatabase:  TTextureDatabase;


  // for print screens
//  PrintScreenTex:   GLuint;
//  PrintScreenData:  array[0..480-1, 0..640-1] of longword;
  PrintScreenData:  array[0..1024*768-1] of longword;

//  Tekstur:    Gluint;
  ActTex:     GLuint;//integer;

{  Tekstura:   array[1..32] of TTekstura;
  Mipmapping: boolean = true;}

  TexOrygW:   integer;
  TexOrygH:   integer;
  TexNewW:    integer;
  TexNewH:    integer;
{  RLE:        array[1..128*128] of byte;
  RLE2:       array[1..128*128] of byte;}

  TexFitW:    integer;
  TexFitH:    integer; // new for limit

  TextureD8:    array[1..1024*1024] of byte; // 1MB
  TextureD16:   array[1..1024*1024, 1..2] of byte;  // luminance/alpha tex (2MB)
  TextureD24:   array[1..1024*1024, 1..3] of byte;  // normal 24-bit tex (3MB)
  TextureD242:  array[1..512*512, 1..3] of byte;  // normal 24-bit tex (0,75MB)
  TextureD32:   array[1..1024*1024, 1..4] of byte; // transparent 32-bit tex (4MB)
  // total 40MB at 2048*2048
  // total 10MB at 1024*1024

{  Paleta:     array[0..255, 1..4] of byte;
  Len:        integer;}
  Mipmapping: Boolean;

  CacheMipmap:  array[0..256*256*3-1] of byte; // 3KB


implementation
uses ULog, DateUtils, UCovers;

function TTextureUnit.GetTexture(Name, Typ: string): TTexture;
begin
  Result := GetTexture(Name, Typ, true);
end;

function TTextureUnit.GetTexture(Name, Typ: string; FromCache: boolean): TTexture;
var
  T:    integer; // texture
  C:    integer; // cover
  Data: array of byte;
begin
  // find texture entry
  T := FindTexture(Name);

  if T = -1 then begin
    // create texture entry
    T := Length(TextureDatabase.Texture);
    SetLength(TextureDatabase.Texture, T+1);
    TextureDatabase.Texture[T].Name := Name;
    TextureDatabase.Texture[T].Typ := Typ;

    // inform database that not textures has been loaded into memory
    TextureDatabase.Texture[T].Texture.TexNum := -1;
    TextureDatabase.Texture[T].TextureCache.TexNum := -1;
  end;

  // use preloaded texture
  if (not FromCache) or (FromCache and not Covers.CoverExists(Name)) then begin
    // use full texture
    if TextureDatabase.Texture[T].Texture.TexNum = -1 then begin
      // load texture
      TextureDatabase.Texture[T].Texture := LoadTexture(false, pchar(Name), 'JPG', pchar(Typ), $0);
    end;

    // use texture
    Result := TextureDatabase.Texture[T].Texture;

  end;

  if FromCache and Covers.CoverExists(Name) then begin
    // use cache texture
    C := Covers.CoverNumber(Name);

    if TextureDatabase.Texture[T].TextureCache.TexNum = -1 then begin
      // load texture
      Covers.PrepareData(Name);
{      Covers.Data[0] := 0;
      Covers.Data[1] := 0;
      Covers.Data[2] := 0;
      Covers.Data[3] := 255;
      Covers.Data[4] := 255;
      Covers.Data[5] := 255;}
      TextureDatabase.Texture[T].TextureCache := CreateTexture(Covers.Data, Name, Covers.Cover[C].W, Covers.Cover[C].H, 24);
    end;

    // use texture
    Result := TextureDatabase.Texture[T].TextureCache;
  end;
end;

function TTextureUnit.FindTexture(Name: string): integer;
var
  T:    integer; // texture
begin
  Result := -1;
  for T := 0 to high(TextureDatabase.Texture) do
    if TextureDatabase.Texture[T].Name = Name then
      Result := T;
end;

function TTextureUnit.LoadTexture(FromRegistry: boolean; Nazwa, Format, Typ: PChar; Col: LongWord): TTexture;
var
  Res:        TResourceStream;
  TextureB:   TBitmap;
  TextureJ:   TJPEGImage;
  TexturePNG: TPNGObject;

  Pet:        integer;
  Pet2:       integer;
  Pix:        integer;
  ColInt:     real;
  PPix:       PByteArray;
  TempA:      integer;
  Error:      integer;
  SkipX:      integer;
begin
  Log.BenchmarkStart(4);
  Mipmapping := true;

  if FromRegistry then begin
    try
      Res := TResourceStream.Create(HInstance, Nazwa, Format);
    except
      beep;
      Exit;
    end;
  end;

  if FromRegistry or ((not FromRegistry) and FileExists(Nazwa)) then begin
    TextureB := TBitmap.Create;

  if Format = 'BMP' then begin
    if FromRegistry then TextureB.LoadFromStream(Res)
    else TextureB.LoadFromFile(Nazwa);
  end

  else if Format = 'JPG' then begin
    TextureJ := TJPEGImage.Create;
    if FromRegistry then TextureJ.LoadFromStream(Res)
    else begin
      if FileExists(Nazwa) then
        TextureJ.LoadFromFile(Nazwa)
      else
        Exit;
    end;
    TextureB.Assign(TextureJ);
    TextureJ.Free;
  end

  else if Format = 'PNG' then begin
    TexturePNG := TPNGObject.Create;
    if FromRegistry then TexturePNG.LoadFromStream(Res)
    else begin
      if FileExists(Nazwa) then
        TexturePNG.LoadFromFile(Nazwa)
      else
        Exit;
    end;
    TextureB.Assign(TexturePNG);
    TexturePNG.Free;
  end;

  if FromRegistry then Res.Free;

  if (TextureB.Width > 1024) or (TextureB.Height > 1024) then begin // will be fixed in 0.5.1 and dynamically extended to 8192x8192 depending on the driver
    Log.LogError('Image ' + Nazwa + ' is too big (' + IntToStr(TextureB.Width) + 'x' + IntToStr(TextureB.Height) + ')');
    Result.TexNum := -1;
  end else begin

  glGenTextures(1, ActTex);
  glBindTexture(GL_TEXTURE_2D, ActTex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);

  if Typ = 'Plain' then begin
    // wymiary
    TexOrygW := TextureB.Width;
    TexOrygH := TextureB.Height;
    TexNewW := Round(Power(2, Ceil(Log2(TexOrygW))));
    TexNewH := Round(Power(2, Ceil(Log2(TexOrygH))));

    // kopiowanie
    TextureB.PixelFormat := pf24bit;
{    if (TextureB.PixelFormat = pf8bit) then begin
      for Pet := 0 to TexOrygH-1 do begin
        for Pet2 := 0 to TexOrygW-1 do begin
          Pix := TextureB.Canvas.Pixels[Pet2, Pet];
          TextureD24[Pet*TexNewW + Pet2+1, 1] := Pix;
          TextureD24[Pet*TexNewW + Pet2+1, 2] := Pix div 256;
          TextureD24[Pet*TexNewW + Pet2+1, 3] := Pix div (256*256);
        end;
      end;
    end;}
    if (TexOrygW <= Limit) and (TexOrygW <= Limit) then begin
      if (TextureB.PixelFormat = pf24bit) then begin
        for Pet := 0 to TexOrygH-1 do begin
          PPix := TextureB.ScanLine[Pet];
          for Pet2 := 0 to TexOrygW-1 do begin
            TextureD24[Pet*TexNewW + Pet2+1, 1] := PPix[Pet2*3+2];
            TextureD24[Pet*TexNewW + Pet2+1, 2] := PPix[Pet2*3+1];
            TextureD24[Pet*TexNewW + Pet2+1, 3] := PPix[Pet2*3];
          end;
        end;
      end;
    end else begin
      // limit
      TexFitW := 4 * (TexOrygW div 4); // fix for bug in gluScaleImage
      TexFitH := TexOrygH;
      if (TextureB.PixelFormat = pf24bit) then begin
        for Pet := 0 to TexOrygH-1 do begin
          PPix := TextureB.ScanLine[Pet];
          for Pet2 := 0 to TexOrygW-1 do begin
            TextureD24[Pet*TexFitW + Pet2+1, 1] := PPix[Pet2*3+2];
            TextureD24[Pet*TexFitW + Pet2+1, 2] := PPix[Pet2*3+1];
            TextureD24[Pet*TexFitW + Pet2+1, 3] := PPix[Pet2*3];
          end;
        end;
      end;
      gluScaleImage(GL_RGB, TexFitW, TexFitH, GL_UNSIGNED_BYTE, @TextureD24,
        Limit, Limit, GL_UNSIGNED_BYTE, @TextureD24); // takes some time

      TexNewW := Limit;
      TexNewH := Limit;
      TexOrygW := Limit;
      TexOrygH := Limit;
    end;

    // creating cache mipmap
    if CreateCacheMipmap then begin
      if (TexOrygW <> TexNewW) or (TexOrygH <> TexNewH) then begin
        // texture only uses some of it's space. there's a need for resize to fit full size
        // and get best quality
        TexFitW := 4 * (TexOrygW div 4); // 0.5.0: fix for bug in gluScaleImage
        SkipX := (TexOrygW div 2) mod 2; // 0.5.0: try to center image

        TexFitH := TexOrygH;
        for Pet := 0 to TexOrygH-1 do begin
          PPix := TextureB.ScanLine[Pet];
          for Pet2 := 0 to TexOrygW-1 do begin
            TextureD242[Pet*TexFitW + Pet2+1, 1] := PPix[(Pet2+SkipX)*3+2];
            TextureD242[Pet*TexFitW + Pet2+1, 2] := PPix[(Pet2+SkipX)*3+1];
            TextureD242[Pet*TexFitW + Pet2+1, 3] := PPix[(Pet2+SkipX)*3];
          end;
        end;
        gluScaleImage(GL_RGB, TexFitW, TexFitH, GL_UNSIGNED_BYTE, @TextureD242,
          Covers.W, Covers.H, GL_UNSIGNED_BYTE, @CacheMipmap[0]); // takes some time

      end else begin
        // texture fits perfectly
        gluScaleImage(GL_RGB, TexOrygW, TexOrygH, GL_UNSIGNED_BYTE, @TextureD24,
          Covers.W, Covers.H, GL_UNSIGNED_BYTE, @CacheMipmap[0]); // takes some time
      end;
    end;

    glTexImage2D(GL_TEXTURE_2D, 0, 3, TexNewW, TexNewH, 0, GL_RGB, GL_UNSIGNED_BYTE, @TextureD24);
    if Mipmapping then begin
      Error := gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TexNewW, TexNewH, GL_RGB, GL_UNSIGNED_BYTE, @TextureD24);
      if Error > 0 then beep;
    end
  end;

  if Typ = 'Transparent' then begin
    // wymiary
    TexOrygW := TextureB.Width;
    TexOrygH := TextureB.Height;
    TexNewW := Round(Power(2, Ceil(Log2(TexOrygW))));
    TexNewH := Round(Power(2, Ceil(Log2(TexOrygH))));
    TextureB.Width := TexNewW;
    TextureB.Height := TexNewH;
    // kopiowanie
    for Pet := 0 to TexOrygH-1 do begin
      for Pet2 := 0 to TexOrygW-1 do begin
        Pix := TextureB.Canvas.Pixels[Pet2, Pet];
        if Pix = Col then begin
          TextureD32[Pet*TexNewW + Pet2 + 1, 1] := 0;
          TextureD32[Pet*TexNewW + Pet2 + 1, 2] := 0;
          TextureD32[Pet*TexNewW + Pet2 + 1, 3] := 0;
          TextureD32[Pet*TexNewW + Pet2 + 1, 4] := 0;
        end else begin
          TextureD32[Pet*TexNewW + Pet2+1, 1] := Pix;
          TextureD32[Pet*TexNewW + Pet2+1, 2] := Pix div 256;
          TextureD32[Pet*TexNewW + Pet2+1, 3] := Pix div (256*256);
          TextureD32[Pet*TexNewW + Pet2+1, 4] := 255;
        end;
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, 4, TexNewW, TexNewH, 0, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);
{    if Mipmapping then begin
      Error := gluBuild2DMipmaps(GL_TEXTURE_2D, 4, TextureB.Width, TextureB.Height, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);
      if Error > 0 then beep;
    end;}
  end;

  if Typ = 'Transparent Range' then begin
    // wymiary
    TexOrygW := TextureB.Width;
    TexOrygH := TextureB.Height;
    TexNewW := Round(Power(2, Ceil(Log2(TexOrygW))));
    TexNewH := Round(Power(2, Ceil(Log2(TexOrygH))));
    TextureB.Width := TexNewW;
    TextureB.Height := TexNewH;
    // kopiowanie
    for Pet := 0 to TexOrygH-1 do begin
      for Pet2 := 0 to TexOrygW-1 do begin
        Pix := TextureB.Canvas.Pixels[Pet2, Pet];
        TextureD32[Pet*TexNewW + Pet2+1, 1] := Pix;
        TextureD32[Pet*TexNewW + Pet2+1, 2] := Pix div 256;
        TextureD32[Pet*TexNewW + Pet2+1, 3] := Pix div (256*256);
        TextureD32[Pet*TexNewW + Pet2+1, 4] := 256 - Pix div 256;
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, 4, TexNewW, TexNewH, 0, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);
{    if Mipmapping then begin
      Error := gluBuild2DMipmaps(GL_TEXTURE_2D, 4, TextureB.Width, TextureB.Height, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);
      if Error > 0 then beep;
    end;}
  end;

  if Typ = 'Font' then begin
    TextureB.PixelFormat := pf24bit;
    for Pet := 0 to TextureB.Height-1 do begin
      PPix := TextureB.ScanLine[Pet];
      for Pet2 := 0 to TextureB.Width-1 do begin
        Pix := PPix[Pet2 * 3];
        TextureD16[Pet*TextureB.Width + Pet2 + 1, 1] := 255;
        TextureD16[Pet*TextureB.Width + Pet2 + 1, 2] := Pix;
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, 2, TextureB.Width, TextureB.Height, 0, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @TextureD16);

    if Mipmapping then glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    if Mipmapping then begin
      Error := gluBuild2DMipmaps(GL_TEXTURE_2D, 2, TextureB.Width, TextureB.Height, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @TextureD16);
      if Error > 0 then beep;
    end;
  end;

  if Typ = 'Font Outline' then begin
    TextureB.PixelFormat := pf24bit;
    for Pet := 0 to TextureB.Height-1 do begin
      PPix := TextureB.ScanLine[Pet];
      for Pet2 := 0 to TextureB.Width-1 do begin
        Pix := PPix[Pet2 * 3];

        Col := Pix;
        if Col < 127 then Col := 127;

        TempA := Pix;
        if TempA >= 95 then TempA := 255;
        if TempA >= 31 then TempA := 255;
        if Pix < 95 then TempA := (Pix * 256) div 96;


        TextureD16[Pet*TextureB.Width + Pet2 + 1, 1] := Col;
        TextureD16[Pet*TextureB.Width + Pet2 + 1, 2] := TempA;
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, 2, TextureB.Width, TextureB.Height, 0, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @TextureD16);

    if Mipmapping then glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    if Mipmapping then begin
      Error := gluBuild2DMipmaps(GL_TEXTURE_2D, 2, TextureB.Width, TextureB.Height, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @TextureD16);
      if Error > 0 then beep;
    end;
  end;

  if Typ = 'Font Outline 2' then begin
    TextureB.PixelFormat := pf24bit;
    for Pet := 0 to TextureB.Height-1 do begin
      PPix := TextureB.ScanLine[Pet];
      for Pet2 := 0 to TextureB.Width-1 do begin
        Pix := PPix[Pet2 * 3];

        Col := Pix;
        if Col < 31 then Col := 31;

        TempA := Pix;
        if TempA >= 31 then TempA := 255;
        if Pix < 31 then TempA := Pix * (256 div 32);

        TextureD16[Pet*TextureB.Width + Pet2 + 1, 1] := Col;
        TextureD16[Pet*TextureB.Width + Pet2 + 1, 2] := TempA;
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, 2, TextureB.Width, TextureB.Height, 0, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @TextureD16);

    if Mipmapping then glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    if Mipmapping then begin
      Error := gluBuild2DMipmaps(GL_TEXTURE_2D, 2, TextureB.Width, TextureB.Height, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @TextureD16);
      if Error > 0 then beep;
    end;
  end;

  if Typ = 'Font Black' then begin
    // normalnie 0,125s     bez niczego 0,015s - 0,030s    z pix 0,125s
    // wymiary
    TextureB.PixelFormat := pf24bit;
    TexOrygW := TextureB.Width;
    TexOrygH := TextureB.Height;
    TexNewW := Round(Power(2, Ceil(Log2(TexOrygW))));
    TexNewH := Round(Power(2, Ceil(Log2(TexOrygH))));
    TextureB.Width := TexNewW;
    TextureB.Height := TexNewH;
    // kopiowanie
    for Pet := 0 to TextureB.Height-1 do begin
      PPix := TextureB.ScanLine[Pet];
      for Pet2 := 0 to TextureB.Width-1 do begin
        Pix := PPix[Pet2*3];
        TextureD32[Pet*TextureB.Width + Pet2 + 1, 1] := 255;
        TextureD32[Pet*TextureB.Width + Pet2 + 1, 2] := 255;
        TextureD32[Pet*TextureB.Width + Pet2 + 1, 3] := 255;
        TextureD32[Pet*TextureB.Width + Pet2 + 1, 4] := 255 - (Pix mod 256);
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, 4, TextureB.Width, TextureB.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);
  end;

  if Typ = 'Alpha Black Colored' then begin
    TextureB.PixelFormat := pf24bit;
    TexOrygW := TextureB.Width;
    TexOrygH := TextureB.Height;
    TexNewW := Round(Power(2, Ceil(Log2(TexOrygW))));
    TexNewH := Round(Power(2, Ceil(Log2(TexOrygH))));
    TextureB.Width := TexNewW;
    TextureB.Height := TexNewH;
    // kopiowanie
    for Pet := 0 to TextureB.Height-1 do begin
      PPix := TextureB.ScanLine[Pet];
      for Pet2 := 0 to TextureB.Width-1 do begin
        Pix := PPix[Pet2*3];
        TextureD32[Pet*TextureB.Width + Pet2 + 1, 1] := (Col div $10000) and $FF;
        TextureD32[Pet*TextureB.Width + Pet2 + 1, 2] := (Col div $100) and $FF;
        TextureD32[Pet*TextureB.Width + Pet2 + 1, 3] := Col and $FF;
        TextureD32[Pet*TextureB.Width + Pet2 + 1, 4] := 255 - (Pix mod 256);
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, 4, TextureB.Width, TextureB.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);
  end;

  if Typ = 'Font Gray' then begin
    // wymiary
    TexOrygW := TextureB.Width;
    TexOrygH := TextureB.Height;
    TexNewW := Round(Power(2, Ceil(Log2(TexOrygW))));
    TexNewH := Round(Power(2, Ceil(Log2(TexOrygH))));
    TextureB.Width := TexNewW;
    TextureB.Height := TexNewH;
    // kopiowanie
    for Pet := 0 to TextureB.Height-1 do begin
      for Pet2 := 0 to TextureB.Width-1 do begin
        Pix := TextureB.Canvas.Pixels[Pet2, Pet];
        TextureD32[Pet*TextureB.Width + Pet2 + 1, 1] := 127;
        TextureD32[Pet*TextureB.Width + Pet2 + 1, 2] := 127;
        TextureD32[Pet*TextureB.Width + Pet2 + 1, 3] := 127;
        TextureD32[Pet*TextureB.Width + Pet2 + 1, 4] := 255 - (Pix mod 256);
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, 4, TextureB.Width, TextureB.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);
{    if Mipmapping then begin
      Error := gluBuild2DMipmaps(GL_TEXTURE_2D, 4, TextureB.Width, TextureB.Height, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);
      if Error > 0 then beep;
    end;}
  end;

  if Typ = 'Arrow' then begin
    TextureB.PixelFormat := pf24bit;
    for Pet := 0 to TextureB.Height-1 do begin
      PPix := TextureB.ScanLine[Pet];
      for Pet2 := 0 to TextureB.Width-1 do begin
        Pix := PPix[Pet2 * 3];

        // transparency
        if Pix >= 127 then TempA := 255;
        if Pix < 127 then TempA := Pix * 2;

        // ColInt = color intensity
        if Pix < 127 then ColInt := 1;
        if Pix >= 127 then ColInt := 2 - Pix / 128;
        //0.75, 0.6, 0.25

        TextureD32[Pet*TextureB.Width + Pet2 + 1, 1] := Round(ColInt * 0.75 * 255 + (1 - ColInt) * 255);
        TextureD32[Pet*TextureB.Width + Pet2 + 1, 2] := Round(ColInt * 0.6  * 255 + (1 - ColInt) * 255);
        TextureD32[Pet*TextureB.Width + Pet2 + 1, 3] := Round(ColInt * 0.25 * 255 + (1 - ColInt) * 255);
        TextureD32[Pet*TextureB.Width + Pet2 + 1, 4] := TempA;
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, 4, TextureB.Width, TextureB.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);

    if Mipmapping then glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    if Mipmapping then begin
      Error := gluBuild2DMipmaps(GL_TEXTURE_2D, 4, TextureB.Width, TextureB.Height, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);
      if Error > 0 then beep;
    end;
  end;

  if Typ = 'Note Plain' then begin
    for Pet := 0 to TextureB.Height-1 do begin
      PPix := TextureB.ScanLine[Pet];
      for Pet2 := 0 to TextureB.Width-1 do begin



        // Skin Patch
        // 0-191= Fade Black to Col, 192= Col, 193-254 Fade Col to White, 255= White
        case PPix[Pet2*3] of
          0..191:    Pix := $10000 * ((((Col div $10000) and $FF) * PPix[Pet2*3]) div $Bf) + $100 * ((((Col div $100) and $FF) * PPix[Pet2*3]) div $Bf) + (((Col and $FF) * PPix[Pet2*3]) div $Bf);
          192:       Pix := Col;
          193..254:  Pix := Col + ($10000 * ((($FF - ((Col div $10000) and $FF)) * ((PPix[Pet2*3] - $C0) * 4) ) div $FF) + $100 * ((($FF - ((Col div $100) and $FF)) * ((PPix[Pet2*3] - $C0) * 4)) div $FF) + ((($FF - (Col and $FF)) * ((PPix[Pet2*3] - $C0) * 4)) div $FF));
          255:       Pix := $FFFFFF;
         end;
//  0.5.0. Original
//        case PPix[Pet2*3] of
//           128:    Pix := $10000 * ((Col div $10000) div 2) + $100 * (((Col div $100) and $FF) div 2) + (Col and $FF) div 2;
//           192:    Pix := Col;
//           255:    Pix := $FFFFFF;
//        end;





        TextureD24[Pet*TextureB.Width + Pet2 + 1, 1] := Pix div $10000;
        TextureD24[Pet*TextureB.Width + Pet2 + 1, 2] := (Pix div $100) and $FF;
        TextureD24[Pet*TextureB.Width + Pet2 + 1, 3] := Pix and $FF;
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, 3, TextureB.Width, TextureB.Height, 0, GL_RGB, GL_UNSIGNED_BYTE, @TextureD24);
  end;

  if Typ = 'Note Transparent' then begin
    for Pet := 0 to TextureB.Height-1 do begin
      PPix := TextureB.ScanLine[Pet];
      for Pet2 := 0 to TextureB.Width-1 do begin
        TempA := 255;



         //Skin Patch
         // 0= Transparent, 1-191= Fade Black to Col, 192= Col, 193-254 Fade Col to White, 255= White
        case PPix[Pet2*3] of
          0:         TempA := 0;
          1..191:    Pix := $10000 * ((((Col div $10000) and $FF) * PPix[Pet2*3]) div $Bf) + $100 * ((((Col div $100) and $FF) * PPix[Pet2*3]) div $Bf) + (((Col and $FF) * PPix[Pet2*3]) div $Bf);
          192:       Pix := Col;
          193..254:  Pix := Col + ($10000 * ((($FF - ((Col div $10000) and $FF)) * ((PPix[Pet2*3] - $C0) * 4) ) div $FF) + $100 * ((($FF - ((Col div $100) and $FF)) * ((PPix[Pet2*3] - $C0) * 4)) div $FF) + ((($FF - (Col and $FF)) * ((PPix[Pet2*3] - $C0) * 4)) div $FF));
          255:       Pix := $FFFFFF;
        end;
// 0.5.0 Original
//        case PPix[Pet2*3] of
//          0:      TempA := 0;
//          128:    Pix := $10000 * ((Col div $10000) div 2) + $100 * (((Col div $100) and $FF) div 2) + (Col and $FF) div 2;
//          192:    Pix := Col;
//          255:    Pix := $FFFFFF;
//        end;




        TextureD32[Pet*TextureB.Width + Pet2 + 1, 1] := Pix div $10000;
        TextureD32[Pet*TextureB.Width + Pet2 + 1, 2] := (Pix div $100) and $FF;
        TextureD32[Pet*TextureB.Width + Pet2 + 1, 3] := Pix and $FF;
        TextureD32[Pet*TextureB.Width + Pet2 + 1, 4] := TempA;
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, 4, TextureB.Width, TextureB.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);
  end;




  TextureB.Free;
//  Inc(ActTex);
{  Tekst.Tekstura := ActTex;
  Tekst.W := TexOrygW;
  Tekst.H := TexOrygH;
  Tekst.X2 := TexOrygW/TexNewW;
  Tekst.Y2 := TexOrygH/TexNewH;}
  Result.X := 0;
  Result.Y := 0;
  Result.W := 0;
  Result.H := 0;
  Result.ScaleW := 1;
  Result.ScaleH := 1;
  Result.Rot := 0;
  Result.TexNum := ActTex;
  Result.TexW := TexOrygW / TexNewW;
  Result.TexH := TexOrygH / TexNewH;

  Result.Int := 1;
  Result.ColR := 1;
  Result.ColG := 1;
  Result.ColB := 1;
  Result.Alpha := 1;

  // 0.4.2 new test - default use whole texure, taking TexW and TexH as const and changing these
  Result.TexX1 := 0;
  Result.TexY1 := 0;
  Result.TexX2 := 1;
  Result.TexY2 := 1;

  // 0.5.0
  Result.Name := Nazwa;

  end;

  Log.BenchmarkEnd(4);
  if Log.BenchmarkTimeLength[4] >= 1 then
    Log.LogBenchmark('**********> Texture Load Time Warning - ' + Format + '/' + Nazwa + '/' + Typ, 4);

  end; // logerror
end;

{procedure ResizeTexture(s: pbytearray; d: pbytearray);
var
  Pet:    integer;
  Pet2:   integer;
begin
  for Pet := 0 to TexNewH*4-1 do
    for Pet2 := 0 to TexNewW-1 do
      d[Pet*TexNewW + Pet2] := 0;

  for Pet := 0 to TexOrygH-1 do begin
    for Pet2 := 0 to TexOrygW-1 do begin
      d[(Pet*TexNewW + Pet2)*4] := Paleta[s[Pet*TexOrygW + Pet2], 1];
      d[(Pet*TexNewW + Pet2)*4+1] := Paleta[s[Pet*TexOrygW + Pet2], 2];
      d[(Pet*TexNewW + Pet2)*4+2] := Paleta[s[Pet*TexOrygW + Pet2], 3];
      d[(Pet*TexNewW + Pet2)*4+3] := Paleta[s[Pet*TexOrygW + Pet2], 4];
    end;
  end;
end;}

{procedure SetTexture(p: pointer);
begin
  glGenTextures(1, Tekstur);
  glBindTexture(GL_TEXTURE_2D, Tekstur);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  glTexImage2D(GL_TEXTURE_2D, 0, 4, TexNewW, TexNewH, 0, GL_RGBA, GL_UNSIGNED_BYTE, p);
end;}

function TTextureUnit.LoadTexture(Nazwa, Format, Typ: PChar; Col: LongWord): TTexture;
begin
  Result := LoadTexture(false, Nazwa, Format, Typ, Col);
//  Result := LoadTexture(SkinReg, Nazwa, Format, Typ, Col); // default to SkinReg

end;

function TTextureUnit.LoadTexture(Nazwa: string): TTexture;
begin
  Result := LoadTexture(false, pchar(Nazwa), 'JPG', 'Plain', 0);
end;

function TTextureUnit.CreateTexture(var Data: array of byte; Name: string; W, H: word; Bits: byte): TTexture;
var
  Pet:        integer;
  Pet2:       integer;
  Pix:        integer;
  ColInt:     real;
  PPix:       PByteArray;
  TempA:      integer;
  Error:      integer;
begin
  Mipmapping := false;

  glGenTextures(1, ActTex); // ActText = new texture number
  glBindTexture(GL_TEXTURE_2D, ActTex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);

  glTexImage2D(GL_TEXTURE_2D, 0, 3, W, H, 0, GL_RGB, GL_UNSIGNED_BYTE, @Data[0]);
  if Mipmapping then begin
    Error := gluBuild2DMipmaps(GL_TEXTURE_2D, 3, W, H, GL_RGB, GL_UNSIGNED_BYTE, @Data[0]);
    if Error > 0 then beep;
  end;


//  Inc(ActTex);
{  Tekst.Tekstura := ActTex;
  Tekst.W := TexOrygW;
  Tekst.H := TexOrygH;
  Tekst.X2 := TexOrygW/TexNewW;
  Tekst.Y2 := TexOrygH/TexNewH;}
  Result.X := 0;
  Result.Y := 0;
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

  // 0.4.2 new test - default use whole texure, taking TexW and TexH as const and changing these
  Result.TexX1 := 0;
  Result.TexY1 := 0;
  Result.TexX2 := 1;
  Result.TexY2 := 1;

  // 0.5.0
  Result.Name := Name;
end;

procedure TTextureUnit.UnloadTexture(Name: string; FromCache: boolean);
var
  T:      integer;
  TexNum: GLuint;
begin
  T := FindTexture(Name);

  if not FromCache then begin
    TexNum := TextureDatabase.Texture[T].Texture.TexNum;
    if TexNum >= 0 then begin
      glDeleteTextures(1, @TexNum);
      TextureDatabase.Texture[T].Texture.TexNum := -1;
//      Log.LogError('Unload texture no '+IntToStr(TexNum));
    end;
  end else begin
    TexNum := TextureDatabase.Texture[T].TextureCache.TexNum;
    if TexNum >= 0 then begin
      glDeleteTextures(1, @TexNum);
      TextureDatabase.Texture[T].TextureCache.TexNum := -1;
//      Log.LogError('Unload texture cache no '+IntToStr(TexNum));
    end;
  end;
end;

end.