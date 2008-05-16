unit UTexture;
// added for easier debug disabling
{$undef blindydebug}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  gl,
  glu,
  glext,
  Math,
  Classes,
  SysUtils,
  UCommon,
  UImage,
  SDL,
  sdlutils,
  SDL_Image;

type
  TTexture = record
    TexNum:   GLuint;
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

type
  TTextureEntry = record
    Name:       string;
    Typ:        TTextureType;
    Color:      Cardinal;

    // we use normal TTexture, it's easier to implement and if needed - we copy ready data
    Texture:        TTexture;
    TextureCache:   TTexture;
  end;

  TTextureDatabase = record
    Texture:    array of TTextureEntry;
  end;

  TTextureUnit = class
    private
      TnWidth, TnHeight: Cardinal; //Width and Height of the Cover Thumbnails

      TnBuffer:  array of byte;
      TnSurface: PSDL_Surface;

      function pixfmt_eq(fmt1,fmt2: PSDL_Pixelformat): boolean;
      procedure AdjustPixelFormat(var TexSurface: PSDL_Surface; Typ: TTextureType);
      function GetScaledTexture(TexSurface: PSDL_Surface; W,H: Cardinal): PSDL_Surface;
      procedure ScaleTexture(var TexSurface: PSDL_Surface; W,H: Cardinal);
      procedure FitTexture(var TexSurface: PSDL_Surface; W,H: Cardinal);
      procedure ColorizeTexture(TexSurface: PSDL_Surface; Col: Cardinal);
    public
      Limit:      integer;
      CreateCacheMipmap:  boolean;

      //function GetNumberFor
      function GetTexture(const Name: string; Typ: TTextureType; FromCache: boolean = true): TTexture; overload;
      function GetTexture(const Name: string; Typ: TTextureType; Col: LongWord; FromCache: boolean = true): TTexture; overload;
      function FindTexture(const Name: string; Typ: TTextureType; Col: Cardinal): integer;
      function LoadTexture(FromRegistry: boolean; const Identifier: string; Typ: TTextureType; Col: LongWord): TTexture; overload;
      function LoadTexture(const Identifier: string; Typ: TTextureType; Col: LongWord): TTexture; overload;
      function LoadTexture(const Identifier: string): TTexture; overload;
      function CreateTexture(var Data: array of byte; const Name: string; W, H: word; Bits: byte): TTexture;
      procedure UnloadTexture(const Name: string; Typ: TTextureType; FromCache: boolean); overload;
      procedure UnloadTexture(const Name: string; Typ: TTextureType; Col: Cardinal; FromCache: boolean); overload;
      //procedure FlushTextureDatabase();

      Function GetCoverThumbnail(const Name: string): Pointer;
      Procedure SetCoverSize(W, H: Integer);

      Constructor Create;
      Destructor Destroy; override;
  end;

var
  Texture:          TTextureUnit;
  TextureDatabase:  TTextureDatabase;

  Mipmapping: Boolean;

  CacheMipmap:  array[0..256*256*3-1] of byte; // 3KB
  CacheMipmapSurface: PSDL_Surface;


implementation

uses ULog,
     DateUtils,
     UCovers,
     UThemes,
     {$IFDEF DARWIN}
 //    MacResources,
     {$ENDIF}
     StrUtils;

Constructor TTextureUnit.Create;
begin
  inherited Create;
end;

Destructor TTextureUnit.Destroy;
begin
  inherited Destroy;
end;

function TTextureUnit.pixfmt_eq(fmt1,fmt2: PSDL_Pixelformat): boolean;
begin
  if (fmt1^.BitsPerPixel = fmt2^.BitsPerPixel) and
     (fmt1^.BytesPerPixel = fmt2^.BytesPerPixel) and
     (fmt1^.Rloss = fmt2^.Rloss) and (fmt1^.Gloss = fmt2^.Gloss) and
     (fmt1^.Bloss = fmt2^.Bloss) and (fmt1^.Rmask = fmt2^.Rmask) and
     (fmt1^.Gmask = fmt2^.Gmask) and (fmt1^.Bmask = fmt2^.Bmask) and
     (fmt1^.Rshift = fmt2^.Rshift) and (fmt1^.Gshift = fmt2^.Gshift) and
     (fmt1^.Bshift = fmt2^.Bshift)
   then
    Result:=True
  else
    Result:=False;
end;

procedure TTextureUnit.AdjustPixelFormat(var TexSurface: PSDL_Surface; Typ: TTextureType);
var
  TempSurface: PSDL_Surface;
  NeededPixFmt: PSDL_Pixelformat;
begin
  NeededPixFmt:=@PixelFmt_RGBA;
  if      (Typ = TEXTURE_TYPE_PLAIN) then
    NeededPixFmt:=@PixelFmt_RGB
  else if (Typ = TEXTURE_TYPE_TRANSPARENT) or
          (Typ = TEXTURE_TYPE_COLORIZED) then
    NeededPixFmt:=@PixelFmt_RGBA
  else
    NeededPixFmt:=@PixelFmt_RGB;


  if not pixfmt_eq(TexSurface^.format, NeededPixFmt) then
  begin
    TempSurface:=TexSurface;
    TexSurface:=SDL_ConvertSurface(TempSurface,NeededPixFmt,SDL_SWSURFACE);
    SDL_FreeSurface(TempSurface);
  end;
end;

function TTextureUnit.GetScaledTexture(TexSurface: PSDL_Surface; W,H: Cardinal): PSDL_Surface;
var
  TempSurface: PSDL_Surface;
begin
  TempSurface:=TexSurface;
  Result:=SDL_ScaleSurfaceRect(TempSurface,
                  0,0,TempSurface^.W,TempSurface^.H,
                  W,H);
  SDL_FreeSurface(TempSurface);
end;

procedure TTextureUnit.ScaleTexture(var TexSurface: PSDL_Surface; W,H: Cardinal);
var
  TempSurface: PSDL_Surface;
begin
  TempSurface:=TexSurface;
  TexSurface:=SDL_ScaleSurfaceRect(TempSurface,
                  0,0,TempSurface^.W,TempSurface^.H,
                  W,H);
  SDL_FreeSurface(TempSurface);
end;

procedure TTextureUnit.FitTexture(var TexSurface: PSDL_Surface; W,H: Cardinal);
var
  TempSurface: PSDL_Surface;
begin
  TempSurface:=TexSurface;
  with TempSurface^.format^ do
    TexSurface:=SDL_CreateRGBSurface(SDL_SWSURFACE,W,H,BitsPerPixel,RMask, GMask, BMask, AMask);
  SDL_SetAlpha(TexSurface, 0, 255);
  SDL_SetAlpha(TempSurface, 0, 255);
  SDL_BlitSurface(TempSurface,nil,TexSurface,nil);
  SDL_FreeSurface(TempSurface);
end;

procedure TTextureUnit.ColorizeTexture(TexSurface: PSDL_Surface; Col: Cardinal);
  //returns hue within range [0.0-6.0)
  function col2hue(Color:Cardinal): double;
  var
    clr: array[0..2] of double;
    hue, max, delta: double;
  begin
    clr[0] := ((Color and $ff0000) shr 16)/255; // R
    clr[1] := ((Color and   $ff00) shr  8)/255; // G
    clr[2] :=  (Color and     $ff)        /255; // B
    max := maxvalue(clr);
    delta := max - minvalue(clr);
    // calc hue
    if (delta = 0.0) then       hue := 0
    else if (clr[0] = max) then hue :=     (clr[1]-clr[2])/delta
    else if (clr[1] = max) then hue := 2.0+(clr[2]-clr[0])/delta
    else if (clr[2] = max) then hue := 4.0+(clr[0]-clr[1])/delta;
    if (hue < 0.0) then
      hue := hue + 6.0;
    Result := hue;
  end;

var
  DestinationHue: Double;
  PixelIndex: Cardinal;
  Pixel: PByte;
    PixelColors: PByteArray;
//    clr: array[0..2] of Double; // [0: R, 1: G, 2: B]
    clr2: array[0..2] of Uint32;
//    hsv: array[0..2] of Double; // [0: H(ue), 1: S(aturation), 2: V(alue)]
    hsv2: array[0..2] of UInt32;//LongInt;
    dhue: UInt32;//LongInt;
    h_int: Cardinal;
//    delta, f, p, q, t: Double;
    delta2,f2,p2,q2,t2: Longint;//LongInt;
//    max: Double;
    max2: Uint32;
begin
  DestinationHue := col2hue(Col);

  dhue:=Trunc(DestinationHue*1024);

  Pixel := TexSurface^.Pixels;

  for PixelIndex := 0 to (TexSurface^.W * TexSurface^.H)-1 do
  begin
    PixelColors:=PByteArray(Pixel);
  // inlined colorize per pixel

  // uses fixed point math
    // get color values
    clr2[0]:=PixelColors[0] shl 10;
    clr2[1]:=PixelColors[1] shl 10;
    clr2[2]:=PixelColors[2] shl 10;
    //calculate luminance and saturation from rgb

    max2:=clr2[0];
    if clr2[1]>max2 then max2:=clr2[1];
    if clr2[2]>max2 then max2:=clr2[2];
    delta2:=clr2[0];
    if clr2[1]<delta2 then delta2:=clr2[1];
    if clr2[2]<delta2 then delta2:=clr2[2];
    delta2:=max2-delta2;
    hsv2[0]:=dhue;  // shl 8
    hsv2[2]:=max2;  // shl 8
    if (max2=0) then hsv2[1] := 0
    else             hsv2[1] := (delta2 shl 10) div max2; // shl 8
      h_int:= hsv2[0] and $fffffC00;
      f2:= hsv2[0]-h_int; //shl 10
      p2:= (hsv2[2]*(1024-hsv2[1])) shr 10;
      q2:= (hsv2[2]*(1024-(hsv2[1]*f2) shr 10)) shr 10;
      t2:= (hsv2[2]*(1024-(hsv2[1]*(1024-f2)) shr 10)) shr 10;
      h_int:=h_int shr 10;
      case h_int of
        0: begin clr2[0]:=hsv2[2]; clr2[1]:=t2;      clr2[2]:=p2;      end; // (v,t,p)
        1: begin clr2[0]:=q2;      clr2[1]:=hsv2[2]; clr2[2]:=p2;      end; // (q,v,p)
        2: begin clr2[0]:=p2;      clr2[1]:=hsv2[2]; clr2[2]:=t2;      end; // (p,v,t)
        3: begin clr2[0]:=p2;      clr2[1]:=q2;      clr2[2]:=hsv2[2]; end; // (p,q,v)
        4: begin clr2[0]:=t2;      clr2[1]:=p2;      clr2[2]:=hsv2[2]; end; // (t,p,v)
        5: begin clr2[0]:=hsv2[2]; clr2[1]:=p2;      clr2[2]:=q2;      end; // (v,p,q)
      end;

    PixelColors[0]:=clr2[0] shr 10;
    PixelColors[1]:=clr2[1] shr 10;
    PixelColors[2]:=clr2[2] shr 10;

    // old floating point version
(*    clr[0] := PixelColors[0]/255;
    clr[1] := PixelColors[1]/255;
    clr[2] := PixelColors[2]/255;
    max := maxvalue(clr);
    delta := max - minvalue(clr);

    hsv[0] := DestinationHue; // set H(ue)
    hsv[2] := max; // set V(alue)
    // calc S(aturation)
    if (max = 0.0) then hsv[1] := 0.0
    else                hsv[1] := delta/max;

//    ColorizePixel(PByteArray(Pixel), DestinationHue);
      h_int := trunc(hsv[0]);             // h_int = |_h_|
      f := hsv[0]-h_int;                  // f = h-h_int
      p := hsv[2]*(1.0-hsv[1]);           // p = v*(1-s)
      q := hsv[2]*(1.0-(hsv[1]*f));       // q = v*(1-s*f)
      t := hsv[2]*(1.0-(hsv[1]*(1.0-f))); // t = v*(1-s*(1-f))
      case h_int of
        0: begin clr[0]:=hsv[2]; clr[1]:=t;      clr[2]:=p;      end; // (v,t,p)
        1: begin clr[0]:=q;      clr[1]:=hsv[2]; clr[2]:=p;      end; // (q,v,p)
        2: begin clr[0]:=p;      clr[1]:=hsv[2]; clr[2]:=t;      end; // (p,v,t)
        3: begin clr[0]:=p;      clr[1]:=q;      clr[2]:=hsv[2]; end; // (p,q,v)
        4: begin clr[0]:=t;      clr[1]:=p;      clr[2]:=hsv[2]; end; // (t,p,v)
        5: begin clr[0]:=hsv[2]; clr[1]:=p;      clr[2]:=q;      end; // (v,p,q)
      end;

    // and store new rgb back into the image
    PixelColors[0] := trunc(255*clr[0]);
    PixelColors[1] := trunc(255*clr[1]);
    PixelColors[2] := trunc(255*clr[2]);
*)
    Inc(Pixel, TexSurface^.format.BytesPerPixel);
  end;
end;

function TTextureUnit.LoadTexture(FromRegistry: boolean; const Identifier: string; Typ: TTextureType; Col: LongWord): TTexture;
var
  TexSurface: PSDL_Surface;
  MipmapSurface: PSDL_Surface;
  newWidth, newHeight: Cardinal;
  oldWidth, oldHeight: Cardinal;
  ActTex: GLuint;
begin
  Log.BenchmarkStart(4);
  Mipmapping := true;

  // zero texture data
  FillChar(Result, SizeOf(Result), 0);

  // load texture data into memory
  {$ifdef blindydebug}
  Log.LogStatus('',' ----------------------------------------------------');
  Log.LogStatus('',' LoadImage('''+Identifier+''') (called by '+Format+')');
  {$endif}
  TexSurface := LoadImage(Identifier);
  {$ifdef blindydebug}
  Log.LogStatus('',' ok');
  {$endif}
  if not assigned(TexSurface) then
  begin
    Log.LogError('Could not load texture: "' + Identifier +' '+ TextureTypeToStr(Typ) +'"',
                 'TTextureUnit.LoadTexture');
    Exit;
  end;

  // convert pixel format as needed
  {$ifdef blindydebug}
  Log.LogStatus('',' AdjustPixelFormat');
  {$endif}
  AdjustPixelFormat(TexSurface, Typ);
  {$ifdef blindydebug}
  Log.LogStatus('',' ok');
  {$endif}
  // adjust texture size (scale down, if necessary)
  newWidth   := TexSurface.W;
  newHeight  := TexSurface.H;

  if (newWidth > Limit) then
    newWidth := Limit;

  if (newHeight > Limit) then
    newHeight := Limit;

  if (TexSurface.W > newWidth) or (TexSurface.H > newHeight) then
  begin
    {$ifdef blindydebug}
    Log.LogStatus('',' ScaleTexture');
    {$endif}
    ScaleTexture(TexSurface,newWidth,newHeight);
    {$ifdef blindydebug}
    Log.LogStatus('',' ok');
    {$endif}
  end;

  {$ifdef blindydebug}
  Log.LogStatus('',' JB-1 : typ='+Typ);
  {$endif}


  (*

  // don't actually understand, if this is needed...
  // this should definately be changed... together with all this
  // cover cache stuff
  if (CreateCacheMipmap) and (Typ = TEXTURE_TYPE_PLAIN) then
  begin
    {$ifdef blindydebug}
    Log.LogStatus('',' JB-1 : Minimap');
    {$endif}

    if (TnWidth <= 256) and (TnHeight <= 256) then
    begin
      {$ifdef blindydebug}
      Log.LogStatus('',' GetScaledTexture('''+inttostr(Covers.W)+''','''+inttostr(Covers.H)+''') (for CacheMipmap)');
      {$endif}
      MipmapSurface:=GetScaledTexture(TexSurface, TnWidth, TnHeight);
      if assigned(MipmapSurface) then
      begin
        {$ifdef blindydebug}
        Log.LogStatus('',' ok');
        Log.LogStatus('',' BlitSurface Stuff');
        {$endif}
        // creating and freeing the surface could be done once, if Cover.W and Cover.H don't change
        TnSurface:=SDL_CreateRGBSurfaceFrom(@TnBuffer[0], TnWidth, TnHeight, 24, TnWidth*3, $000000ff, $0000ff00, $00ff0000, 0);
        SDL_BlitSurface(TnSurface, nil, TnSurface, nil);
        SDL_FreeSurface(TnSurface);
        {$ifdef blindydebug}
        Log.LogStatus('',' ok');
        Log.LogStatus('',' SDL_FreeSurface (CacheMipmap)');
        {$endif}
        SDL_FreeSurface(TnSurface);
        {$ifdef blindydebug}
        Log.LogStatus('',' ok');
        {$endif}
      end
      else
      begin
        Log.LogStatus(' Error creating CacheMipmap',' LoadTexture('''+Identifier+''')');
      end;
    end;
    // should i create a cache texture, if Covers.W/H are larger?
  end;

  *)

  {$ifdef blindydebug}
  Log.LogStatus('',' JB-2');
  {$endif}


 // now we might colorize the whole thing
  if (Typ = TEXTURE_TYPE_COLORIZED) then
    ColorizeTexture(TexSurface, Col);

 // save actual dimensions of our texture
  oldWidth  := newWidth;
  oldHeight := newHeight;
 // make texture dimensions be powers of 2
  newWidth  := Round(Power(2, Ceil(Log2(newWidth))));
  newHeight := Round(Power(2, Ceil(Log2(newHeight))));
  if (newHeight <> oldHeight) or (newWidth <> oldWidth) then
    FitTexture(TexSurface, newWidth, newHeight);

  // at this point we have the image in memory...
  // scaled to be at most 1024x1024 pixels large
  // scaled so that dimensions are powers of 2
  // and converted to either RGB or RGBA

  {$ifdef blindydebug}
  Log.LogStatus('',' JB-3');
  {$endif}


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
    glTexImage2D(GL_TEXTURE_2D, 0, 4, newWidth, newHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, TexSurface.pixels);
  end
  else //if Typ = TEXTURE_TYPE_PLAIN then
  begin
    glTexImage2D(GL_TEXTURE_2D, 0, 3, newWidth, newHeight, 0, GL_RGB, GL_UNSIGNED_BYTE, TexSurface.pixels);
  end;

  {$ifdef blindydebug}
  Log.LogStatus('',' JB-5');
  {$endif}


  // setup texture struct
  with Result do
  begin
    X := 0;
    Y := 0;
    Z := 0;
    W := 0;
    H := 0;
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

  {$ifdef blindydebug}
  Log.LogStatus('',' JB-6');
  {$endif}

  SDL_FreeSurface(TexSurface);

  {$ifdef blindydebug}
  Log.LogStatus('',' JB-7');
  {$endif}


  Log.BenchmarkEnd(4);
  if Log.BenchmarkTimeLength[4] >= 1 then
    Log.LogBenchmark('**********> Texture Load Time Warning - ' + Identifier + '/' + TextureTypeToStr(Typ), 4)
  else Log.LogBenchmark('**********> Texture Load Time ' + ExtractFileName(Identifier) + '/' + TextureTypeToStr(Typ), 4);
  {$ifdef blindydebug}
  Log.LogStatus('',' JB-8');
  {$endif}

end;


function TTextureUnit.GetTexture(const Name: string; Typ: TTextureType; FromCache: boolean): TTexture;
begin
  Result := GetTexture(Name, Typ, 0, FromCache);
end;

function TTextureUnit.GetTexture(const Name: string; Typ: TTextureType; Col: LongWord; FromCache: boolean): TTexture;
var
  T:    integer; // texture
  C:    integer; // cover
  Data: array of byte;
begin

  if Name = '' then
    exit;

  // find texture entry
  T := FindTexture(Name, Typ, Col);

  if T = -1 then
  begin
    // create texture entry
    T := Length(TextureDatabase.Texture);
    SetLength(TextureDatabase.Texture, T+1);

    TextureDatabase.Texture[T].Name  := Name;
    TextureDatabase.Texture[T].Typ   := Typ;
    TextureDatabase.Texture[T].Color := Col;

    // inform database that no textures have been loaded into memory
    TextureDatabase.Texture[T].Texture.TexNum      := 0;
    TextureDatabase.Texture[T].TextureCache.TexNum := 0;
  end;

  // use preloaded texture
  if (not FromCache) or (FromCache{ and (Covers.CoverExists(Name) < 0)}) then
  begin
    // use full texture
    if TextureDatabase.Texture[T].Texture.TexNum = 0 then
    begin
      // load texture
      {$ifdef blindydebug}
      Log.LogStatus('...', 'GetTexture('''+Name+''','''+Typ+''')');
      {$endif}
      TextureDatabase.Texture[T].Texture := LoadTexture(false, Name, Typ, Col);
      {$ifdef blindydebug}
      Log.LogStatus('done',' ');
      {$endif}
    end;

    // use texture
    Result := TextureDatabase.Texture[T].Texture;
  end;

  if FromCache and Covers.CoverExists(Name) then
  begin
    // use cache texture
    C := Covers.CoverNumber(Name);

    if TextureDatabase.Texture[T].TextureCache.TexNum = 0 then
    begin
      // load texture
      Covers.PrepareData(Name);
      TextureDatabase.Texture[T].TextureCache := CreateTexture(Covers.Data, Name, Covers.Cover[C].W, Covers.Cover[C].H, 24);
    end;

    // use texture
    Result := TextureDatabase.Texture[T].TextureCache;
  end;
end;

//--------
// Returns Pointer to an Array of Byte containing the Texture Data in the
// requested Size
//--------
Function TTextureUnit.GetCoverThumbnail(const Name: string): Pointer;
var
  TexSurface: PSDL_Surface;
  newHeight, newWidth: Cardinal;
const
  Typ = TEXTURE_TYPE_PLAIN;
begin
  Result := nil;
  If (FileExists(Name)) then
  begin
    {$ifdef blindydebug}
    Log.LogStatus('',' ----------------------------------------------------');
    Log.LogStatus('',' GetCoverThumbnail('''+Name+''')');
    {$endif}
    TexSurface := LoadImage(Name);
    {$ifdef blindydebug}
    Log.LogStatus('',' ok');
    {$endif}
    if assigned(TexSurface) then
    begin
      // convert pixel format as needed
      {$ifdef blindydebug}
      Log.LogStatus('',' AdjustPixelFormat');
      {$endif}
      AdjustPixelFormat(TexSurface, Typ);

      {$ifdef blindydebug}
      Log.LogStatus('',' ok');
      {$endif}

      // Scale Texture to Covers Dimensions
      {$ifdef blindydebug}
      Log.LogStatus('',' ScaleTexture('''+inttostr(tnWidth)+''','''+inttostr(TnHeight)+''') (for CacheMipmap)');
      {$endif}
      ScaleTexture(TexSurface, TnWidth, TnHeight);

      if assigned(TexSurface) AND assigned(TnSurface) then
      begin
        {$ifdef blindydebug}
        Log.LogStatus('',' ok');
        Log.LogStatus('',' BlitSurface Stuff');
        {$endif}

        SDL_BlitSurface(TexSurface, nil, TnSurface, nil);

        Result := @TnBuffer[0];

        {$ifdef blindydebug}
        Log.LogStatus('',' ok');
        {$endif}
      end
      else
        Log.LogStatus(' Error creating Cover Thumbnail',' LoadTexture('''+Name+''')');
    end
    else
      Log.LogError('Could not load texture for Cover Thumbnail: "' + name+' '+ TextureTypeToStr(Typ) +'"',
                   'TTextureUnit.GetCoverThumbnail');

    SDL_FreeSurface(TexSurface);
  end;
end;

//--------
// Sets Textures Thumbnail Size Vars and Sets LEngth of DataBuffer and Create CoverSurface
//--------
Procedure TTextureUnit.SetCoverSize(W, H: Integer);
begin
  If (H > 0) AND (W > 0) then
  begin
    TnWidth := W;
    TnHeight := H;

    SetLength(TnBuffer, TnWidth * TnHeight * 3);

    //Free if necesary and Create new Surface at Data
    If (Assigned(TnSurface)) then
      SDL_FreeSurface(TnSurface);
      
    TnSurface := SDL_CreateRGBSurfaceFrom(@TnBuffer[0], TnWidth, TnHeight, 24, TnWidth*3, $000000ff, $0000ff00, $00ff0000, 0);
  end;
end;

function TTextureUnit.FindTexture(const Name: string; Typ: TTextureType; Col: Cardinal): integer;
var
  T:    integer; // texture
begin
  Result := -1;
  for T := 0 to high(TextureDatabase.Texture) do
    if (TextureDatabase.Texture[T].Name = Name) and
       (TextureDatabase.Texture[T].Typ = Typ) then
    begin
      // colorized textures must match in their color too
      if (TextureDatabase.Texture[T].Typ <> TEXTURE_TYPE_COLORIZED) or
         (TextureDatabase.Texture[T].Color = Col) then
      begin
        Result := T;
        break;
      end;
    end;
end;

function TTextureUnit.LoadTexture(const Identifier: string; Typ: TTextureType; Col: LongWord): TTexture;
begin
  Result := LoadTexture(false, Identifier, Typ, Col);
end;

function TTextureUnit.LoadTexture(const Identifier: string): TTexture;
begin
  Result := LoadTexture(false, Identifier, TEXTURE_TYPE_PLAIN, 0);
end;

function TTextureUnit.CreateTexture(var Data: array of byte; const Name: string; W, H: word; Bits: byte): TTexture;
var
  Position:        integer;
  Position2:       integer;
  Pix:        integer;
  ColInt:     real;
  PPix:       PByteArray;
  TempA:      integer;
  Error:      integer;
  ActTex:     GLuint;
begin
  Mipmapping := false;

  glGenTextures(1, @ActTex); // ActText = new texture number
  glBindTexture(GL_TEXTURE_2D, ActTex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  glTexImage2D(GL_TEXTURE_2D, 0, 3, W, H, 0, GL_RGB, GL_UNSIGNED_BYTE, @Data[0]);
  if Mipmapping then begin
    Error := gluBuild2DMipmaps(GL_TEXTURE_2D, 3, W, H, GL_RGB, GL_UNSIGNED_BYTE, @Data[0]);
    if Error > 0 then
      Log.LogError('gluBuild2DMipmaps() failed', 'TTextureUnit.CreateTexture');
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

procedure TTextureUnit.UnloadTexture(const Name: string; Typ: TTextureType; FromCache: boolean);
begin
  UnloadTexture(Name, Typ, 0, FromCache);
end;

procedure TTextureUnit.UnloadTexture(const Name: string; Typ: TTextureType; Col: Cardinal; FromCache: boolean);
var
  T:      integer;
  TexNum: GLuint;
begin
  T := FindTexture(Name, Typ, Col);

  if not FromCache then begin
    TexNum := TextureDatabase.Texture[T].Texture.TexNum;
    if TexNum > 0 then begin
      glDeleteTextures(1, PGLuint(@TexNum));
      TextureDatabase.Texture[T].Texture.TexNum := 0;
//      Log.LogError('Unload texture no '+IntToStr(TexNum));
    end;
  end else begin
    TexNum := TextureDatabase.Texture[T].TextureCache.TexNum;
    if TexNum > 0 then begin
      glDeleteTextures(1, @TexNum);
      TextureDatabase.Texture[T].TextureCache.TexNum := 0;
//      Log.LogError('Unload texture cache no '+IntToStr(TexNum));
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
  TexType: TTextureType;
  UpCaseStr: string;
begin
  UpCaseStr := UpperCase(TypeStr);
  for TexType := Low(TextureTypeStr) to High(TextureTypeStr) do
  begin
    if (UpCaseStr = UpperCase(TextureTypeStr[TexType])) then
    begin
      Result := TexType;
      Exit;
    end;
  end;
  Log.LogWarn('Unknown texture-type: "' + TypeStr + '"', 'ParseTextureType');
  Result := TEXTURE_TYPE_PLAIN;
end;

end.
