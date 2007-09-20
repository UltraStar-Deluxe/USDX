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

{$I switches.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses OpenGL12,
     {$IFDEF win32}
     windows,
     {$ENDIF}
     Math,
     Classes,
     SysUtils,
     {$IFDEF FPC}
     ulazjpeg,
     {$ELSE}
     JPEG,
     PNGImage,
     {$ENDIF}
     Graphics,
     UCommon,
     UThemes;


  {$IFDEF Win32}
    procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;

  {$ELSE}
    {$ifdef darwin}
    const opengl32 = '/System/Library/Frameworks/OpenGL.framework/Libraries/libGL.dylib';
    {$ELSE}
    const opengl32 = 'libGL.so' ; // YES Capital GL
    {$ENDIF}
    
    procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
  {$ENDIF}

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
  
    private
      function LoadBitmap( aSourceStream : TStream; aIMG : TBitMap ): boolean;
      function LoadJpeg( aSourceStream : TStream; aIMG : TBitMap ): boolean;
    public
    Limit:      integer;
    CreateCacheMipmap:  boolean;

//    function GetNumberFor
    function GetTexture(Name, Typ: string): TTexture; overload;
    function GetTexture(Name, Typ: string; FromCache: boolean): TTexture; overload;
    function FindTexture(Name: string): integer;
    function LoadTexture(FromRegistry: boolean; Identifier, Format, Typ: PChar; Col: LongWord): TTexture; overload;
    function LoadTexture(Identifier, Format, Typ: PChar; Col: LongWord): TTexture; overload;
    function LoadTexture(Identifier: string): TTexture; overload;
    function CreateTexture(var Data: array of byte; Name: string; W, H: word; Bits: byte): TTexture;
    procedure UnloadTexture(Name: string; FromCache: boolean);
  end;

var
  lasthue: double;
  Texture:          TTextureUnit;
  TextureDatabase:  TTextureDatabase;

  PrintScreenData:  array[0..1024*768-1] of longword;

  ActTex:     GLuint;//integer;

  TexOrigW:   integer;
  TexOrigH:   integer;
  TexNewW:    integer;
  TexNewH:    integer;

  TexFitW:    integer;
  TexFitH:    integer; // new for limit

  TextureD8:    array[1..1024*1024] of byte; // 1MB
  TextureD16:   array[1..1024*1024, 1..2] of byte;  // luminance/alpha tex (2MB)
  TextureD24:   array[1..1024*1024, 1..3] of byte;  // normal 24-bit tex (3MB)
  TextureD242:  array[1..512*512, 1..3] of byte;  // normal 24-bit tex (0,75MB)
  TextureD32:   array[1..1024*1024, 1..4] of byte; // transparent 32-bit tex (4MB)
  // total 40MB at 2048*2048
  // total 10MB at 1024*1024

  Mipmapping: Boolean;

  CacheMipmap:  array[0..256*256*3-1] of byte; // 3KB


implementation
uses ULog, DateUtils, UCovers, StrUtils;

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

    // inform database that no textures have been loaded into memory
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

// expects: src, dst: pointers to r,g,b,a
//          hue: new hue within range [0.0-6.0)
procedure ColorizeCopy(Src, Dst: PByteArray;  hue: Double); overload;
var
  i,j,k: Cardinal;
  clr, hls: array[0..2] of Double;
  delta, f, p, q, t: Double;
begin
  hls[0]:=hue;

      clr[0] := src[0]/255;
      clr[1] := src[1]/255;
      clr[2] := src[2]/255;
      
      //calculate luminance and saturation from rgb
      hls[1] := maxvalue(clr); //l:=...
      delta  := hls[1] - minvalue(clr);
      
      if hls[1] =  0.0 then
         hls[2] := 0.0
      else
         hls[2] := delta/hls[1]; //v:=...
         
      // calc new rgb from our hls (h from color, l ans s from pixel)
//      if (hls[1]<>0.0) and (hls[2]<>0.0) then // only if colorizing makes sense
      begin
        k:=trunc(hls[0]);
        f:=hls[0]-k;
        p:=hls[1]*(1.0-hls[2]);
        q:=hls[1]*(1.0-(hls[2]*f));
        t:=hls[1]*(1.0-(hls[2]*(1.0-f)));
        case k of
          0: begin clr[0]:=hls[1]; clr[1]:=t; clr[2]:=p; end;
          1: begin clr[0]:=q; clr[1]:=hls[1]; clr[2]:=p; end;
          2: begin clr[0]:=p; clr[1]:=hls[1]; clr[2]:=t; end;
          3: begin clr[0]:=p; clr[1]:=q; clr[2]:=hls[1]; end;
          4: begin clr[0]:=t; clr[1]:=p; clr[2]:=hls[1]; end;
          5: begin clr[0]:=hls[1]; clr[1]:=p; clr[2]:=q; end;
        end;
        // and store new rgb back into the image
        dst[0]:=floor(255*clr[0]);
        dst[1]:=floor(255*clr[1]);
        dst[2]:=floor(255*clr[2]);
        dst[3]:=src[3];
      end;
end;

// expects: src: $rrggbb
//          dst: pointer to r,g,b,a
//          hue: new hue within range [0.0-6.0)
procedure ColorizeCopy(Src: Cardinal; Dst: PByteArray;  hue: Double); overload;
var
  i,j,k: Cardinal;
  clr, hls: array[0..2] of Double;
  delta, f, p, q, t: Double;
begin
  hls[0]:=hue;

      clr[0]:=((src shr 16) and $ff)/255;
      clr[1]:=((src shr 8) and $ff)/255;
      clr[2]:=(src and $ff)/255;
      //calculate luminance and saturation from rgb
      hls[1]:=maxvalue(clr); //l:=...
      delta:=hls[1]-minvalue(clr);
      if hls[1]=0.0 then hls[2]:=0.0 else hls[2]:=delta/hls[1]; //v:=...
      // calc new rgb from our hls (h from color, l ans s from pixel)
//      if (hls[1]<>0.0) and (hls[2]<>0.0) then // only if colorizing makes sense
      begin
        k:=trunc(hls[0]);
        f:=hls[0]-k;
        p:=hls[1]*(1.0-hls[2]);
        q:=hls[1]*(1.0-(hls[2]*f));
        t:=hls[1]*(1.0-(hls[2]*(1.0-f)));
        case k of
          0: begin clr[0]:=hls[1]; clr[1]:=t; clr[2]:=p; end;
          1: begin clr[0]:=q; clr[1]:=hls[1]; clr[2]:=p; end;
          2: begin clr[0]:=p; clr[1]:=hls[1]; clr[2]:=t; end;
          3: begin clr[0]:=p; clr[1]:=q; clr[2]:=hls[1]; end;
          4: begin clr[0]:=t; clr[1]:=p; clr[2]:=hls[1]; end;
          5: begin clr[0]:=hls[1]; clr[1]:=p; clr[2]:=q; end;
        end;
        // and store new rgb back into the image
        dst[0]:=floor(255*clr[0]);
        dst[1]:=floor(255*clr[1]);
        dst[2]:=floor(255*clr[2]);
        dst[3]:=255;
      end;
end;
//returns hue within range [0.0-6.0)
function col2h(Color:Cardinal):double;
var
  clr,hls: array[0..2] of double;
  delta: double;
begin
  clr[0]:=((Color and $ff0000) shr 16)/255;
  clr[1]:=((Color and $ff00) shr 8)/255;
  clr[2]:=(Color and $ff)/255;
  hls[1]:=maxvalue(clr);
  delta:=hls[1]-minvalue(clr);
  if      clr[0]=hls[1] then hls[0]:=(clr[1]-clr[2])/delta
  else if clr[1]=hls[1] then hls[0]:=2.0+(clr[2]-clr[0])/delta
  else if clr[2]=hls[1] then hls[0]:=4.0+(clr[0]-clr[1])/delta;
  if hls[0]<0.0 then hls[0]:=hls[0]+6.0;
  if hls[0]=6.0 then hls[0]:=0.0;
  col2h:=hls[0];
end;


function TTextureUnit.LoadBitmap( aSourceStream : TStream; aIMG : TBitMap ): boolean;
begin
  result := false;
  try
    Log.LogStatus( '  TTextureUnit.LoadBitmap' , '' );

    aSourceStream.position := 0;
    aIMG.LoadFromStream( aSourceStream );
  finally
    result := aSourceStream.position > 0;
  end;
end;

function TTextureUnit.LoadJpeg( aSourceStream : TStream; aIMG : TBitMap ): boolean;
var
  TextureJ:   TJPEGImage;
begin
  result := false;
  try
    Log.LogStatus( '  TTextureUnit.LoadJpeg' , '');

    aSourceStream.position := 0;

    TextureJ := TJPEGImage.Create;
    try
      TextureJ.LoadFromStream( aSourceStream );
      aIMG.Assign(TextureJ);
    finally
      TextureJ.Free;
    end;
  finally
    result := aSourceStream.position > 0;
  end;
end;




function TTextureUnit.LoadTexture(FromRegistry: boolean; Identifier, Format, Typ: PChar; Col: LongWord): TTexture;
var
  Res:        TResourceStream;
  TextureB:   TBitmap;
  TextureJ:   TJPEGImage;
  {$IFNDEF FPC}
  TexturePNG: TPNGObject;
  {$ENDIF}
  
  TextureAlpha: array of byte;
  AlphaPtr:   PByte;
  TransparentColor: TColor;
  PixelColor: TColor;

  Position:        integer;
  Position2:       integer;
  Pix:        integer;
  ColInt:     real;
  PPix:       PByteArray;
  TempA:      integer;
  Error:      integer;
  SkipX:      integer;
  myAlpha:    Real;
  myRGBABitmap: array of byte;
  RGBPtr: PByte;
  myHue: Double;
  
  lTextureStream : TStream;
begin
  lTextureStream := nil;

//   Log.LogStatus( 'From Resource - ' + inttostr( integer( FromRegistry ) ) , Identifier +' '+ Format +' '+ Typ );

//  {$IFNDEF FPC}
                // TODO : JB_lazarus eeeew this is a nasty one...
                // but lazarus implementation scanlines is different :(
                // need to implement as per
                //    http://www.lazarus.freepascal.org/index.php?name=PNphpBB2&file=viewtopic&p=18512
                //    http://www.lazarus.freepascal.org/index.php?name=PNphpBB2&file=viewtopic&p=10797
                //    http://wiki.lazarus.freepascal.org/Developing_with_Graphics
  Log.BenchmarkStart(4);
  Mipmapping := true;

  if FromRegistry then
  begin
    try
    //      Res := TResourceStream.Create(HInstance, Identifier, Format);
      lTextureStream := TResourceStream.Create(HInstance, Identifier, Format);

      // TEmp, untill all code is moved to refactord way..
      Res := TResourceStream( lTextureStream );
    except
      Log.LogStatus( 'ERROR Could not load from resource' , Identifier +' '+ Format +' '+ Typ );
      beep;
      Exit;
    end;
  end
  else
  begin
    if ( FileExists(Identifier) ) then
    begin
      // Get the File Extension...
      Format := PAnsichar(UpperCase(RightStr(ExtractFileExt(Identifier),3)));
      lTextureStream := TFileStream.create( Identifier , fmOpenRead or fmShareDenyNone );
    end;
  end;

  if assigned( lTextureStream ) then
  begin
  TextureB := TBitmap.Create;

  if Format = 'BMP' then
    LoadBitmap( lTextureStream , TextureB )
  else
  if Format = 'JPG' then
    LoadJpeg( lTextureStream , TextureB )
  else if Format = 'PNG' then
  begin
    {$IFNDEF FPC}
    // TODO : JB_lazarus - fix this for lazarus..
    TexturePNG := TPNGObject.Create;
    if FromRegistry then
      TexturePNG.LoadFromStream(Res)
    else
    begin
      if FileExists(Identifier) then
        TexturePNG.LoadFromFile(Identifier)
      else
        Exit;
    end;
    
    TextureB.Assign(TexturePNG);
    // transparent png hack start (part 1 of 2)
    if ((Typ = 'Transparent') or (Typ = 'Colorized')) and (TexturePNG.TransparencyMode = ptmPartial) then
    begin
      setlength(TextureAlpha, TextureB.Width*TextureB.Height);
      setlength(MyRGBABitmap,TextureB.Width*TextureB.Height*4);
      if (TexturePNG.Header.ColorType = COLOR_GRAYSCALEALPHA) or
         (TexturePNG.Header.ColorType = COLOR_RGBALPHA) then
      begin
        // i would have preferred english variables here but i use Position because i'm lazy
        for Position := 0 to TextureB.Height - 1 do
        begin
          AlphaPtr := PByte(TexturePNG.AlphaScanline[Position]);
          RGBPtr:=PByte(TexturePNG.Scanline[Position]);
          for Position2 := 0 to TextureB.Width - 1 do
          begin
            TextureAlpha[Position*TextureB.Width+Position2]:= AlphaPtr^;
            MyRGBABitmap[(Position*TextureB.Width+Position2)*4]:= RGBPtr^;
            Inc(RGBPtr);
            MyRGBABitmap[(Position*TextureB.Width+Position2)*4+1]:= RGBPtr^;
            Inc(RGBPtr);
            MyRGBABitmap[(Position*TextureB.Width+Position2)*4+2]:= RGBPtr^;
            Inc(RGBPtr);
            MyRGBABitmap[(Position*TextureB.Width+Position2)*4+3]:= AlphaPtr^;
//            Inc(RGBPtr);
            Inc(AlphaPtr);
          end;
        end;
      end;
    end else
      setlength(TextureAlpha,0); // just no special transparency for unimplemented transparency types (ptmBit)
    // transparent png hack end
    TexturePNG.Free;
    {$ENDIF}
  end;

  if FromRegistry then Res.Free;

  if (TextureB.Width > 1024) or (TextureB.Height > 1024) then begin // will be fixed in 0.5.1 and dynamically extended to 8192x8192 depending on the driver
    Log.LogError('Image ' + Identifier + ' is too big (' + IntToStr(TextureB.Width) + 'x' + IntToStr(TextureB.Height) + ')');
    Result.TexNum := -1;
  end else begin

  glGenTextures(1, ActTex);
  glBindTexture(GL_TEXTURE_2D, ActTex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  if Typ = 'Plain' then
  begin
    {$IFNDEF FPC}
    // dimensions
    TexOrigW := TextureB.Width;
    TexOrigH := TextureB.Height;
    TexNewW := Round(Power(2, Ceil(Log2(TexOrigW))));
    TexNewH := Round(Power(2, Ceil(Log2(TexOrigH))));

    // copy and process pixeldata
    TextureB.PixelFormat := pf24bit;
{    if (TextureB.PixelFormat = pf8bit) then begin
      for Position := 0 to TexOrigH-1 do begin
        for Position2 := 0 to TexOrigW-1 do begin
          Pix := TextureB.Canvas.Pixels[Position2, Position];
          TextureD24[Position*TexNewW + Position2+1, 1] := Pix;
          TextureD24[Position*TexNewW + Position2+1, 2] := Pix div 256;
          TextureD24[Position*TexNewW + Position2+1, 3] := Pix div (256*256);
        end;
      end;
    end;}
    if (TexOrigW <= Limit) and (TexOrigW <= Limit) then begin
      if (TextureB.PixelFormat = pf24bit) then begin
        for Position := 0 to TexOrigH-1 do begin
          PPix := TextureB.ScanLine[Position];
          for Position2 := 0 to TexOrigW-1 do begin
            TextureD24[Position*TexNewW + Position2+1, 1] := PPix[Position2*3+2];
            TextureD24[Position*TexNewW + Position2+1, 2] := PPix[Position2*3+1];
            TextureD24[Position*TexNewW + Position2+1, 3] := PPix[Position2*3];
          end;
        end;
      end;
    end else begin
      // limit
      TexFitW := 4 * (TexOrigW div 4); // fix for bug in gluScaleImage
      TexFitH := TexOrigH;
      if (TextureB.PixelFormat = pf24bit) then begin
        for Position := 0 to TexOrigH-1 do begin
          PPix := TextureB.ScanLine[Position];
          for Position2 := 0 to TexOrigW-1 do begin
            TextureD24[Position*TexFitW + Position2+1, 1] := PPix[Position2*3+2];
            TextureD24[Position*TexFitW + Position2+1, 2] := PPix[Position2*3+1];
            TextureD24[Position*TexFitW + Position2+1, 3] := PPix[Position2*3];
          end;
        end;
      end;
      gluScaleImage(GL_RGB, TexFitW, TexFitH, GL_UNSIGNED_BYTE, @TextureD24,
        Limit, Limit, GL_UNSIGNED_BYTE, @TextureD24); // takes some time

      TexNewW := Limit;
      TexNewH := Limit;
      TexOrigW := Limit;
      TexOrigH := Limit;
    end;

    // creating cache mipmap
    if CreateCacheMipmap then begin
      if (TexOrigW <> TexNewW) or (TexOrigH <> TexNewH) then begin
        // texture only uses some of it's space. there's a need for resize to fit full size
        // and get best quality
        TexFitW := 4 * (TexOrigW div 4); // 0.5.0: fix for bug in gluScaleImage
        SkipX := (TexOrigW div 2) mod 2; // 0.5.0: try to center image

        TexFitH := TexOrigH;
        for Position := 0 to TexOrigH-1 do begin
          PPix := TextureB.ScanLine[Position];
          for Position2 := 0 to TexOrigW-1 do begin
            TextureD242[Position*TexFitW + Position2+1, 1] := PPix[(Position2+SkipX)*3+2];
            TextureD242[Position*TexFitW + Position2+1, 2] := PPix[(Position2+SkipX)*3+1];
            TextureD242[Position*TexFitW + Position2+1, 3] := PPix[(Position2+SkipX)*3];
          end;
        end;
        gluScaleImage(GL_RGB, TexFitW, TexFitH, GL_UNSIGNED_BYTE, @TextureD242,
          Covers.W, Covers.H, GL_UNSIGNED_BYTE, @CacheMipmap[0]); // takes some time

      end else begin
        // texture fits perfectly
        gluScaleImage(GL_RGB, TexOrigW, TexOrigH, GL_UNSIGNED_BYTE, @TextureD24,
          Covers.W, Covers.H, GL_UNSIGNED_BYTE, @CacheMipmap[0]); // takes some time
      end;
    end;

    glTexImage2D(GL_TEXTURE_2D, 0, 3, TexNewW, TexNewH, 0, GL_RGB, GL_UNSIGNED_BYTE, @TextureD24);
    if Mipmapping then begin
      Error := gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TexNewW, TexNewH, GL_RGB, GL_UNSIGNED_BYTE, @TextureD24);
      if Error > 0 then beep;
    end
    {$ENDIF}
  end;

  if Typ = 'Transparent' then begin
    // dimensions
    TexOrigW := TextureB.Width;
    TexOrigH := TextureB.Height;
    TexNewW := Round(Power(2, Ceil(Log2(TexOrigW))));
    TexNewH := Round(Power(2, Ceil(Log2(TexOrigH))));
    TextureB.Width := TexNewW;
    TextureB.Height := TexNewH;

    // copy and process pixeldata
    for Position := 0 to TexOrigH-1 do begin
      for Position2 := 0 to TexOrigW-1 do begin
        Pix := TextureB.Canvas.Pixels[Position2, Position];
                                           // ,- part of transparent png hack
        if ((Pix = $fefefe) or (Pix = Col)) and (length(TextureAlpha)=0) then begin //Small fix, that caused artefacts to be drawn (#fe == dec254)
          TextureD32[Position*TexNewW + Position2 + 1, 1] := 0;
          TextureD32[Position*TexNewW + Position2 + 1, 2] := 0;
          TextureD32[Position*TexNewW + Position2 + 1, 3] := 0;
          TextureD32[Position*TexNewW + Position2 + 1, 4] := 0;
        end else if (Format = 'PNG') and (length(TextureAlpha) <> 0) then begin
          myAlpha:=TextureAlpha[Position*TexOrigW+Position2];
          TextureD32[Position*TexNewW + Position2+1, 1] := MyRGBABitmap[(Position*TexOrigW+Position2)*4+2];
          TextureD32[Position*TexNewW + Position2+1, 2] := MyRGBABitmap[(Position*TexOrigW+Position2)*4+1];
          TextureD32[Position*TexNewW + Position2+1, 3] := MyRGBABitmap[(Position*TexOrigW+Position2)*4];
          TextureD32[Position*TexNewW+Position2+1,4]:=MyRGBABitmap[(Position*TexOrigW+Position2)*4+3];
        end else begin
          TextureD32[Position*TexNewW + Position2+1, 1] := (Pix and $ff);
          TextureD32[Position*TexNewW + Position2+1, 2] := ((Pix shr 8) and $ff);
          TextureD32[Position*TexNewW + Position2+1, 3] := ((Pix shr 16) and $ff);
          TextureD32[Position*TexNewW + Position2+1, 4] := 255;
        end;
      end;
    end;
    setlength(TextureAlpha,0);
    setlength(MyRGBABitmap,0);
    glTexImage2D(GL_TEXTURE_2D, 0, 4, TexNewW, TexNewH, 0, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);
{    if Mipmapping then begin
      Error := gluBuild2DMipmaps(GL_TEXTURE_2D, 4, TextureB.Width, TextureB.Height, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);
      if Error > 0 then beep;
    end;}
  end;

// The new awesomeness of colorized pngs starts here
// We're the first who had this feature, so give credit when you copy+paste :P
    if Typ = 'Colorized' then begin
    // dimensions
    TexOrigW := TextureB.Width;
    TexOrigH := TextureB.Height;
    TexNewW := Round(Power(2, Ceil(Log2(TexOrigW))));
    TexNewH := Round(Power(2, Ceil(Log2(TexOrigH))));
    TextureB.Width := TexNewW;
    TextureB.Height := TexNewH;

    myHue:=col2h(Col);
    // copy and process pixeldata
    for Position := 0 to TexOrigH-1 do begin
      for Position2 := 0 to TexOrigW-1 do begin
        Pix := TextureB.Canvas.Pixels[Position2, Position];
        if (Format = 'PNG') and (length(MyRGBABitmap) <> 0) then
         ColorizeCopy(@MyRGBABitmap[(Position*TexOrigW+Position2)*4],
                      @TextureD32[Position*TexNewW + Position2+1, 1],
                      myHue)
        else
          ColorizeCopy(Pix,
                       @TextureD32[Position*TexNewW + Position2+1, 1],
                       myHue);
      end;
    end;

    setlength(TextureAlpha,0);
    setlength(MyRGBABitmap,0);
    glTexImage2D(GL_TEXTURE_2D, 0, 4, TexNewW, TexNewH, 0, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);
  end;
// eoa COLORIZE

  if Typ = 'Transparent Range' then begin
    // dimensions
    TexOrigW := TextureB.Width;
    TexOrigH := TextureB.Height;
    TexNewW := Round(Power(2, Ceil(Log2(TexOrigW))));
    TexNewH := Round(Power(2, Ceil(Log2(TexOrigH))));
    TextureB.Width := TexNewW;
    TextureB.Height := TexNewH;
    // copy and process pixeldata
    for Position := 0 to TexOrigH-1 do begin
      for Position2 := 0 to TexOrigW-1 do begin
        Pix := TextureB.Canvas.Pixels[Position2, Position];
        TextureD32[Position*TexNewW + Position2+1, 1] := Pix;
        TextureD32[Position*TexNewW + Position2+1, 2] := Pix div 256;
        TextureD32[Position*TexNewW + Position2+1, 3] := Pix div (256*256);
        TextureD32[Position*TexNewW + Position2+1, 4] := 256 - Pix div 256;
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, 4, TexNewW, TexNewH, 0, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);
{    if Mipmapping then begin
      Error := gluBuild2DMipmaps(GL_TEXTURE_2D, 4, TextureB.Width, TextureB.Height, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);
      if Error > 0 then beep;
    end;}
  end;

  if Typ = 'Font' then
  begin
    {$IFNDEF FPC}
    TextureB.PixelFormat := pf24bit;
    for Position := 0 to TextureB.Height-1 do begin
      PPix := TextureB.ScanLine[Position];
      for Position2 := 0 to TextureB.Width-1 do begin
        Pix := PPix[Position2 * 3];
        TextureD16[Position*TextureB.Width + Position2 + 1, 1] := 255;
        TextureD16[Position*TextureB.Width + Position2 + 1, 2] := Pix;
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, 2, TextureB.Width, TextureB.Height, 0, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @TextureD16);

    if Mipmapping then glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    if Mipmapping then begin
      Error := gluBuild2DMipmaps(GL_TEXTURE_2D, 2, TextureB.Width, TextureB.Height, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @TextureD16);
      if Error > 0 then beep;
    end;
    {$ENDIF}
  end;

  if Typ = 'Font Outline' then
  begin
    {$IFNDEF FPC}
    TextureB.PixelFormat := pf24bit;
    for Position := 0 to TextureB.Height-1 do begin
      PPix := TextureB.ScanLine[Position];
      for Position2 := 0 to TextureB.Width-1 do begin
        Pix := PPix[Position2 * 3];

        Col := Pix;
        if Col < 127 then Col := 127;

        TempA := Pix;
        if TempA >= 95 then TempA := 255;
        if TempA >= 31 then TempA := 255;
        if Pix < 95 then TempA := (Pix * 256) div 96;


        TextureD16[Position*TextureB.Width + Position2 + 1, 1] := Col;
        TextureD16[Position*TextureB.Width + Position2 + 1, 2] := TempA;
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, 2, TextureB.Width, TextureB.Height, 0, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @TextureD16);

    if Mipmapping then glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    if Mipmapping then begin
      Error := gluBuild2DMipmaps(GL_TEXTURE_2D, 2, TextureB.Width, TextureB.Height, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @TextureD16);
      if Error > 0 then beep;
    end;
    {$ENDIF}
  end;

  if Typ = 'Font Outline 2' then
  begin
    {$IFNDEF FPC}
    TextureB.PixelFormat := pf24bit;
    for Position := 0 to TextureB.Height-1 do begin
      PPix := TextureB.ScanLine[Position];
      for Position2 := 0 to TextureB.Width-1 do begin
        Pix := PPix[Position2 * 3];

        Col := Pix;
        if Col < 31 then Col := 31;

        TempA := Pix;
        if TempA >= 31 then TempA := 255;
        if Pix < 31 then TempA := Pix * (256 div 32);

        TextureD16[Position*TextureB.Width + Position2 + 1, 1] := Col;
        TextureD16[Position*TextureB.Width + Position2 + 1, 2] := TempA;
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, 2, TextureB.Width, TextureB.Height, 0, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @TextureD16);

    if Mipmapping then glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    if Mipmapping then begin
      Error := gluBuild2DMipmaps(GL_TEXTURE_2D, 2, TextureB.Width, TextureB.Height, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @TextureD16);
      if Error > 0 then beep;
    end;
    {$ENDIF}
  end;

  if Typ = 'Font Black' then
  begin
    {$IFNDEF FPC}
    // normalnie 0,125s     bez niczego 0,015s - 0,030s    z pix 0,125s  <-- ???
    // dimensions
    TextureB.PixelFormat := pf24bit;
    TexOrigW := TextureB.Width;
    TexOrigH := TextureB.Height;
    TexNewW := Round(Power(2, Ceil(Log2(TexOrigW))));
    TexNewH := Round(Power(2, Ceil(Log2(TexOrigH))));
    TextureB.Width := TexNewW;
    TextureB.Height := TexNewH;
    // copy and process pixeldata
    for Position := 0 to TextureB.Height-1 do begin
      PPix := TextureB.ScanLine[Position];
      for Position2 := 0 to TextureB.Width-1 do begin
        Pix := PPix[Position2*3];
        TextureD32[Position*TextureB.Width + Position2 + 1, 1] := 255;
        TextureD32[Position*TextureB.Width + Position2 + 1, 2] := 255;
        TextureD32[Position*TextureB.Width + Position2 + 1, 3] := 255;
        TextureD32[Position*TextureB.Width + Position2 + 1, 4] := 255 - (Pix mod 256);
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, 4, TextureB.Width, TextureB.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);
    {$ENDIF}
  end;

  if Typ = 'Alpha Black Colored' then
  begin
    {$IFNDEF FPC}
    TextureB.PixelFormat := pf24bit;
    TexOrigW := TextureB.Width;
    TexOrigH := TextureB.Height;
    TexNewW := Round(Power(2, Ceil(Log2(TexOrigW))));
    TexNewH := Round(Power(2, Ceil(Log2(TexOrigH))));
    TextureB.Width := TexNewW;
    TextureB.Height := TexNewH;
    // copy and process pixeldata
    for Position := 0 to TextureB.Height-1 do begin
      PPix := TextureB.ScanLine[Position];
      for Position2 := 0 to TextureB.Width-1 do begin
        Pix := PPix[Position2*3];
        TextureD32[Position*TextureB.Width + Position2 + 1, 1] := (Col div $10000) and $FF;
        TextureD32[Position*TextureB.Width + Position2 + 1, 2] := (Col div $100) and $FF;
        TextureD32[Position*TextureB.Width + Position2 + 1, 3] := Col and $FF;
        TextureD32[Position*TextureB.Width + Position2 + 1, 4] := 255 - (Pix mod 256);
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, 4, TextureB.Width, TextureB.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);
    {$ENDIF}
  end;

  if Typ = 'Font Gray' then
  begin
    {$IFNDEF FPC}
    // dimensions
    TexOrigW := TextureB.Width;
    TexOrigH := TextureB.Height;
    TexNewW := Round(Power(2, Ceil(Log2(TexOrigW))));
    TexNewH := Round(Power(2, Ceil(Log2(TexOrigH))));
    TextureB.Width := TexNewW;
    TextureB.Height := TexNewH;
    // copy and process pixeldata
    for Position := 0 to TextureB.Height-1 do begin
      for Position2 := 0 to TextureB.Width-1 do begin
        Pix := TextureB.Canvas.Pixels[Position2, Position];
        TextureD32[Position*TextureB.Width + Position2 + 1, 1] := 127;
        TextureD32[Position*TextureB.Width + Position2 + 1, 2] := 127;
        TextureD32[Position*TextureB.Width + Position2 + 1, 3] := 127;
        TextureD32[Position*TextureB.Width + Position2 + 1, 4] := 255 - (Pix mod 256);
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, 4, TextureB.Width, TextureB.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);
{    if Mipmapping then begin
      Error := gluBuild2DMipmaps(GL_TEXTURE_2D, 4, TextureB.Width, TextureB.Height, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);
      if Error > 0 then beep;
    end;}
    {$ENDIF}
  end;

  if Typ = 'Arrow' then
  begin
    {$IFNDEF FPC}
    TextureB.PixelFormat := pf24bit;
    for Position := 0 to TextureB.Height-1 do begin
      PPix := TextureB.ScanLine[Position];
      for Position2 := 0 to TextureB.Width-1 do begin
        Pix := PPix[Position2 * 3];

        // transparency
        if Pix >= 127 then TempA := 255;
        if Pix < 127 then TempA := Pix * 2;

        // ColInt = color intensity
        if Pix < 127 then ColInt := 1;
        if Pix >= 127 then ColInt := 2 - Pix / 128;
        //0.75, 0.6, 0.25

        TextureD32[Position*TextureB.Width + Position2 + 1, 1] := Round(ColInt * 0.75 * 255 + (1 - ColInt) * 255);
        TextureD32[Position*TextureB.Width + Position2 + 1, 2] := Round(ColInt * 0.6  * 255 + (1 - ColInt) * 255);
        TextureD32[Position*TextureB.Width + Position2 + 1, 3] := Round(ColInt * 0.25 * 255 + (1 - ColInt) * 255);
        TextureD32[Position*TextureB.Width + Position2 + 1, 4] := TempA;
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, 4, TextureB.Width, TextureB.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);

    if Mipmapping then glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    if Mipmapping then begin
      Error := gluBuild2DMipmaps(GL_TEXTURE_2D, 4, TextureB.Width, TextureB.Height, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);
      if Error > 0 then beep;
    end;
    {$ENDIF}
  end;

  if Typ = 'Note Plain' then
  begin
    {$IFNDEF FPC}
    for Position := 0 to TextureB.Height-1 do
    begin
      PPix := TextureB.ScanLine[Position];
      for Position2 := 0 to TextureB.Width-1 do
      begin



        // Skin Patch
        // 0-191= Fade Black to Col, 192= Col, 193-254 Fade Col to White, 255= White
        case PPix[Position2*3] of
          0..191:    Pix := $10000 * ((((Col div $10000) and $FF) * PPix[Position2*3]) div $Bf) + $100 * ((((Col div $100) and $FF) * PPix[Position2*3]) div $Bf) + (((Col and $FF) * PPix[Position2*3]) div $Bf);
          192:       Pix := Col;
          193..254:  Pix := Col + ($10000 * ((($FF - ((Col div $10000) and $FF)) * ((PPix[Position2*3] - $C0) * 4) ) div $FF) + $100 * ((($FF - ((Col div $100) and $FF)) * ((PPix[Position2*3] - $C0) * 4)) div $FF) + ((($FF - (Col and $FF)) * ((PPix[Position2*3] - $C0) * 4)) div $FF));
          255:       Pix := $FFFFFF;
         end;
//  0.5.0. Original
//        case PPix[Position2*3] of
//           128:    Pix := $10000 * ((Col div $10000) div 2) + $100 * (((Col div $100) and $FF) div 2) + (Col and $FF) div 2;
//           192:    Pix := Col;
//           255:    Pix := $FFFFFF;
//        end;





        TextureD24[Position*TextureB.Width + Position2 + 1, 1] := Pix div $10000;
        TextureD24[Position*TextureB.Width + Position2 + 1, 2] := (Pix div $100) and $FF;
        TextureD24[Position*TextureB.Width + Position2 + 1, 3] := Pix and $FF;
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, 3, TextureB.Width, TextureB.Height, 0, GL_RGB, GL_UNSIGNED_BYTE, @TextureD24);
    {$ENDIF}
  end;

  if Typ = 'Note Transparent' then
  begin
    {$IFNDEF FPC}
    for Position := 0 to TextureB.Height-1 do begin
      PPix := TextureB.ScanLine[Position];
      for Position2 := 0 to TextureB.Width-1 do begin
        TempA := 255;



         //Skin Patch
         // 0= Transparent, 1-191= Fade Black to Col, 192= Col, 193-254 Fade Col to White, 255= White
        case PPix[Position2*3] of
          0:         TempA := 0;
          1..191:    Pix := $10000 * ((((Col div $10000) and $FF) * PPix[Position2*3]) div $Bf) + $100 * ((((Col div $100) and $FF) * PPix[Position2*3]) div $Bf) + (((Col and $FF) * PPix[Position2*3]) div $Bf);
          192:       Pix := Col;
          193..254:  Pix := Col + ($10000 * ((($FF - ((Col div $10000) and $FF)) * ((PPix[Position2*3] - $C0) * 4) ) div $FF) + $100 * ((($FF - ((Col div $100) and $FF)) * ((PPix[Position2*3] - $C0) * 4)) div $FF) + ((($FF - (Col and $FF)) * ((PPix[Position2*3] - $C0) * 4)) div $FF));
          255:       Pix := $FFFFFF;
        end;
// 0.5.0 Original
//        case PPix[Position2*3] of
//          0:      TempA := 0;
//          128:    Pix := $10000 * ((Col div $10000) div 2) + $100 * (((Col div $100) and $FF) div 2) + (Col and $FF) div 2;
//          192:    Pix := Col;
//          255:    Pix := $FFFFFF;
//        end;




        TextureD32[Position*TextureB.Width + Position2 + 1, 1] := Pix div $10000;
        TextureD32[Position*TextureB.Width + Position2 + 1, 2] := (Pix div $100) and $FF;
        TextureD32[Position*TextureB.Width + Position2 + 1, 3] := Pix and $FF;
        TextureD32[Position*TextureB.Width + Position2 + 1, 4] := TempA;
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, 4, TextureB.Width, TextureB.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, @TextureD32);
    {$ENDIF}
  end;

  TextureB.Free;
  Result.X := 0;
  Result.Y := 0;
  Result.W := 0;
  Result.H := 0;
  Result.ScaleW := 1;
  Result.ScaleH := 1;
  Result.Rot := 0;
  Result.TexNum := ActTex;
  Result.TexW := TexOrigW / TexNewW;
  Result.TexH := TexOrigH / TexNewH;

  Result.Int   := 1;
  Result.ColR  := 1;
  Result.ColG  := 1;
  Result.ColB  := 1;
  Result.Alpha := 1;

  // 0.4.2 new test - default use whole texure, taking TexW and TexH as const and changing these
  Result.TexX1 := 0;
  Result.TexY1 := 0;
  Result.TexX2 := 1;
  Result.TexY2 := 1;

  // 0.5.0
  Result.Name := Identifier;

  end;

  Log.BenchmarkEnd(4);
  if Log.BenchmarkTimeLength[4] >= 1 then
    Log.LogBenchmark('**********> Texture Load Time Warning - ' + Format + '/' + Identifier + '/' + Typ, 4);

  end; // logerror    
//  {$ENDIF}
end;

{procedure ResizeTexture(s: pbytearray; d: pbytearray);
var
  Position:    integer;
  Position2:   integer;
begin
  for Position := 0 to TexNewH*4-1 do
    for Position2 := 0 to TexNewW-1 do
      d[Position*TexNewW + Position2] := 0;

  for Position := 0 to TexOrigH-1 do begin
    for Position2 := 0 to TexOrigW-1 do begin
      d[(Position*TexNewW + Position2)*4] := Paleta[s[Position*TexOrigW + Position2], 1];
      d[(Position*TexNewW + Position2)*4+1] := Paleta[s[Position*TexOrigW + Position2], 2];
      d[(Position*TexNewW + Position2)*4+2] := Paleta[s[Position*TexOrigW + Position2], 3];
      d[(Position*TexNewW + Position2)*4+3] := Paleta[s[Position*TexOrigW + Position2], 4];
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

function TTextureUnit.LoadTexture(Identifier, Format, Typ: PChar; Col: LongWord): TTexture;
begin
  Result := LoadTexture(false, Identifier, Format, Typ, Col);
//  Result := LoadTexture(SkinReg, Identifier, Format, Typ, Col); // default to SkinReg

end;

function TTextureUnit.LoadTexture(Identifier: string): TTexture;
begin
  Result := LoadTexture(false, pchar(Identifier), 'JPG', 'Plain', 0);
end;

function TTextureUnit.CreateTexture(var Data: array of byte; Name: string; W, H: word; Bits: byte): TTexture;
var
  Position:        integer;
  Position2:       integer;
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
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  glTexImage2D(GL_TEXTURE_2D, 0, 3, W, H, 0, GL_RGB, GL_UNSIGNED_BYTE, @Data[0]);
  if Mipmapping then begin
    Error := gluBuild2DMipmaps(GL_TEXTURE_2D, 3, W, H, GL_RGB, GL_UNSIGNED_BYTE, @Data[0]);
    if Error > 0 then beep;
  end;

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
