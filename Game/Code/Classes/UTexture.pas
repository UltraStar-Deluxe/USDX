unit UTexture;
// added for easier debug disabling
{$undef blindydebug}

// Plain (alpha = 1)
// Transparent
// Colorized

// obsolete?
// Transparent Range
// Font (white is drawn, black is transparent)
// Font Outline (Font with darker outline)
// Font Outline 2 (Font with darker outline)
// Font Black (black is drawn, white is transparent)
// Font Gray (gray is drawn, white is transparent)
// Arrow (for arrows, white is white, gray has color, black is transparent);

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses OpenGL12,
     {$IFDEF win32}
     windows,
     {$ENDIF}
     Math,
     Classes,
     SysUtils,
     Graphics,
     UCommon,
     UThemes,
     SDL,
     sdlutils,
     SDL_Image;

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
      function LoadImage(Identifier: PChar): PSDL_Surface;
      function pixfmt_eq(fmt1,fmt2: PSDL_Pixelformat): boolean;
      procedure AdjustPixelFormat(var TexSurface: PSDL_Surface; Typ: PChar);
      function GetScaledTexture(TexSurface: PSDL_Surface; W,H: Cardinal): PSDL_Surface;
      procedure ScaleTexture(var TexSurface: PSDL_Surface; W,H: Cardinal);
      procedure FitTexture(var TexSurface: PSDL_Surface; W,H: Cardinal);
      procedure ColorizeTexture(TexSurface: PSDL_Surface; Col: Cardinal);

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
    Constructor Create;
    Destructor Destroy; override;
  end;

var
  Texture:          TTextureUnit;
  TextureDatabase:  TTextureDatabase;

  // this should be in UDisplay?!
  PrintScreenData:  array[0..1024*768-1] of longword;

  ActTex:     GLuint;//integer;

//  TextureD8:    array[1..1024*1024] of byte; // 1MB
  TextureD16:   array[1..1024*1024, 1..2] of byte;  // luminance/alpha tex (2MB)
//  TextureD24:   array[1..1024*1024, 1..3] of byte;  // normal 24-bit tex (3MB)
//  TextureD242:  array[1..512*512, 1..3] of byte;  // normal 24-bit tex (0,75MB)
//  TextureD32:   array[1..1024*1024, 1..4] of byte; // transparent 32-bit tex (4MB)
  // total 40MB at 2048*2048
  // total 10MB at 1024*1024

  Mipmapping: Boolean;

  CacheMipmap:  array[0..256*256*3-1] of byte; // 3KB
  CacheMipmapSurface: PSDL_Surface;


implementation

uses ULog,
     DateUtils,
     UCovers,
     {$ifdef LINUX}
       fileutil,
     {$endif}
     {$IFDEF LAZARUS}
     LResources,
     {$ENDIF}
     {$IFDEF DARWIN}
     MacResources,
     {$ENDIF}
     StrUtils, dialogs;

const
  fmt_rgba: TSDL_Pixelformat=(palette:      nil;
                              BitsPerPixel:  32;
                              BytesPerPixel:  4;
                              Rloss:          0;
                              Gloss:          0;
                              Bloss:          0;
                              Aloss:          0;
                              Rshift:         0;
                              Gshift:         8;
                              Bshift:        16;
                              Ashift:        24;
                              Rmask:  $000000ff;
                              Gmask:  $0000ff00;
                              Bmask:  $00ff0000;
                              Amask:  $ff000000;
                              ColorKey:       0;
                              Alpha:        255);
  fmt_rgb: TSDL_Pixelformat=( palette:      nil;
                              BitsPerPixel:  24;
                              BytesPerPixel:  3;
                              Rloss:          0;
                              Gloss:          0;
                              Bloss:          0;
                              Aloss:          0;
                              Rshift:         0;
                              Gshift:         8;
                              Bshift:        16;
                              Ashift:         0;
                              Rmask:  $000000ff;
                              Gmask:  $0000ff00;
                              Bmask:  $00ff0000;
                              Amask:  $00000000;
                              ColorKey:       0;
                              Alpha:        255);


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

// +++++++++++++++++++++ helpers for loadimage +++++++++++++++
  function SdlStreamSeek( context : PSDL_RWops; offset : Integer; whence : Integer ) : integer; cdecl;
  var
    stream : TStream;
    origin : Word;
  begin
    stream := TStream( context.unknown );
    if ( stream = nil ) then
      raise EInvalidContainer.Create( 'SDLStreamSeek on nil' );
    case whence of
      0 : origin := soFromBeginning; //	Offset is from the beginning of the resource. Seek moves to the position Offset. Offset must be >= 0.
      1 : origin := soFromCurrent; //	Offset is from the current position in the resource. Seek moves to Position + Offset.
      2 : origin := soFromEnd;
    else
      origin := soFromBeginning; // just in case
    end;
    Result := stream.Seek( offset, origin );
  end;
  function SdlStreamRead( context : PSDL_RWops; Ptr : Pointer; size : Integer; maxnum: Integer ) : Integer; cdecl;
  var
    stream : TStream;
  begin
    stream := TStream( context.unknown );
    if ( stream = nil ) then
      raise EInvalidContainer.Create( 'SDLStreamRead on nil' );
    try
      Result := stream.read( Ptr^, Size * maxnum ) div size;
    except
      Result := -1;
    end;
  end;
  function SDLStreamClose( context : PSDL_RWops ) : Integer; cdecl;
  var
    stream : TStream;
  begin
    stream := TStream( context.unknown );
    if ( stream = nil ) then
      raise EInvalidContainer.Create( 'SDLStreamClose on nil' );
    stream.Free;
    Result := 1;
  end;
// -----------------------------------------------

function TTextureUnit.LoadImage(Identifier: PChar): PSDL_Surface;

  function FileExistsInsensative( var aFileName : PChar ): boolean;
  begin
{$IFDEF LINUX} // eddie: Changed FPC to LINUX: Windows and Mac OS X dont have case sensitive file systems
    result := true;

    if FileExists( aFileName ) then
      exit;

    aFileName := pchar( FindDiskFileCaseInsensitive( aFileName ) );
    result    := FileExists( aFileName );
{$ELSE}
    result := FileExists( aFileName );
{$ENDIF}
  end;

var

  TexRWops:  PSDL_RWops;
  dHandle: THandle;

  {$IFDEF LAZARUS}
  lLazRes  : TLResource;
  lResData : TStringStream;
  {$ELSE}
  TexStream: TStream;
  {$ENDIF}
  
  lFileName : pchar;

begin
  Result   := nil;
  TexRWops := nil;

  if Identifier = '' then
    exit;
    
  lFileName := Identifier;

//  Log.LogStatus( Identifier, 'LoadImage' );

//    Log.LogStatus( 'Looking for File ( Loading : '+Identifier+' - '+ FindDiskFileCaseInsensitive(Identifier) +')', '  LoadImage' );

  if ( FileExistsInsensative(lFileName) ) then
  begin
    // load from file
    Log.LogStatus( 'Is File ( Loading : '+lFileName+')', '  LoadImage' );
    try
      Result:=IMG_Load(lFileName);
      Log.LogStatus( '       '+inttostr( integer( Result ) ), '  LoadImage' );
    except
      Log.LogStatus( 'ERROR Could not load from file' , Identifier);
      beep;
      Exit;
    end;
  end
  else
  begin
    Log.LogStatus( 'IS Resource, because file does not exist.('+Identifier+')', '  LoadImage' );

    // load from resource stream
    {$IFDEF LAZARUS}
      lLazRes := LazFindResource( Identifier, 'TEX' );
      if lLazRes <> nil then
      begin
        lResData := TStringStream.create( lLazRes.value );
        try
          lResData.position := 0;
          try
            TexRWops         := SDL_AllocRW;
            TexRWops.unknown := TUnknown( lResData );
            TexRWops.seek    := SDLStreamSeek;
            TexRWops.read    := SDLStreamRead;
            TexRWops.write   := nil;
            TexRWops.close   := SDLStreamClose;
            TexRWops.type_   := 2;
          except
            Log.LogStatus( 'ERROR Could not assign resource ('+Identifier+')' , Identifier);
            beep;
            Exit;
          end;
        
          Result := IMG_Load_RW(TexRWops,0);
          SDL_FreeRW(TexRWops);
        finally
          freeandnil( lResData );
        end;
      end
      else
      begin
        Log.LogStatus( 'NOT found in Resource ('+Identifier+')', '  LoadImage' );
      end;
    {$ELSE}
      dHandle := FindResource(hInstance, Identifier, 'TEX');
      if dHandle=0 then
      begin
        Log.LogStatus( 'ERROR Could not find resource' , '  '+ Identifier);
        beep;
        Exit;
      end;


      TexStream := nil;
      try
        TexStream        := TResourceStream.Create(HInstance, Identifier, 'TEX');
      except
        Log.LogStatus( 'ERROR Could not load from resource' , Identifier);
        beep;
        Exit;
      end;

      try
        TexStream.position := 0;
        try
          TexRWops         := SDL_AllocRW;
          TexRWops.unknown := TUnknown(TexStream);
          TexRWops.seek    := SDLStreamSeek;
          TexRWops.read    := SDLStreamRead;
          TexRWops.write   := nil;
          TexRWops.close   := SDLStreamClose;
          TexRWops.type_   := 2;
        except
          Log.LogStatus( 'ERROR Could not assign resource' , Identifier);
          beep;
          Exit;
        end;

        Log.LogStatus( 'resource Assigned....' , Identifier);        
        Result:=IMG_Load_RW(TexRWops,0);
        SDL_FreeRW(TexRWops);
        
      finally
        if assigned( TexStream ) then
          freeandnil( TexStream );
      end;
    {$ENDIF}
  end;
end;

procedure TTextureUnit.AdjustPixelFormat(var TexSurface: PSDL_Surface; Typ: PChar);
var
  TempSurface: PSDL_Surface;
  NeededPixFmt: PSDL_Pixelformat;
begin
  NeededPixFmt:=@fmt_rgba;
  if Typ= 'Plain' then NeededPixFmt:=@fmt_rgb
  else
  if (Typ='Transparent') or
     (Typ='Colorized')
     then NeededPixFmt:=@fmt_rgba
  else
    NeededPixFmt:=@fmt_rgb;


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
    // this is for safety reasons
    if delta = 0.0 then delta:=0.000000000001;
    if      clr[0]=hls[1] then hls[0]:=(clr[1]-clr[2])/delta
    else if clr[1]=hls[1] then hls[0]:=2.0+(clr[2]-clr[0])/delta
    else if clr[2]=hls[1] then hls[0]:=4.0+(clr[0]-clr[1])/delta;
    if hls[0]<0.0 then hls[0]:=hls[0]+6.0;
    if hls[0]=6.0 then hls[0]:=0.0;
    col2h:=hls[0];
  end;
  procedure ColorizePixel(Pix: PByteArray;  hue: Double);
  var
    i,j,k: Cardinal;
    clr, hls: array[0..2] of Double;
    delta, f, p, q, t: Double;
  begin
    hls[0]:=hue;

      clr[0] := Pix[0]/255;
      clr[1] := Pix[1]/255;
      clr[2] := Pix[2]/255;

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
        Pix[0]:=floor(255*clr[0]);
        Pix[1]:=floor(255*clr[1]);
        Pix[2]:=floor(255*clr[2]);
      end;
  end;

var
  DestinationHue: Double;
  PixelIndex: Cardinal;
begin
  DestinationHue:=col2h(Col);
  for PixelIndex:=0 to (TexSurface^.W*TexSurface^.H -1) do
    ColorizePixel(@(PByteArray(TexSurface^.Pixels)[PixelIndex*TexSurface^.format.BytesPerPixel]),DestinationHue);
end;

function TTextureUnit.LoadTexture(FromRegistry: boolean; Identifier, Format, Typ: PChar; Col: LongWord): TTexture;
var
  TexSurface: PSDL_Surface;
  MipmapSurface: PSDL_Surface;
  newWidth, newHeight: Cardinal;
  oldWidth, oldHeight: Cardinal;
  kopierindex: Cardinal;
begin
  Log.BenchmarkStart(4);
  Mipmapping := true;
(*
  Log.LogStatus( '', '' );

  if Identifier = nil then
    Log.LogStatus(' ERROR unknown Identifier', 'Id:'''+Identifier+''' Fmt:'''+Format+''' Typ:'''+Typ+'''')
  else
    Log.LogStatus(' should be ok - trying to load', 'Id:'''+Identifier+''' Fmt:'''+Format+''' Typ:'''+Typ+'''');
*)

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
    Log.LogStatus( 'ERROR Could not load texture' , Identifier +' '+ Format +' '+ Typ );
    beep;
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



  // don't actually understand, if this is needed...
  // this should definately be changed... together with all this
  // cover cache stuff
  if (CreateCacheMipmap) and (Typ='Plain') then
  begin
    {$ifdef blindydebug}
    Log.LogStatus('',' JB-1 : Minimap');
    {$endif}

    if (Covers.W <= 256) and (Covers.H <= 256) then
    begin
      {$ifdef blindydebug}
      Log.LogStatus('',' GetScaledTexture('''+inttostr(Covers.W)+''','''+inttostr(Covers.H)+''') (for CacheMipmap)');
      {$endif}
      MipmapSurface:=GetScaledTexture(TexSurface,Covers.W, Covers.H);
      if assigned(MipmapSurface) then
      begin
        {$ifdef blindydebug}
        Log.LogStatus('',' ok');
        Log.LogStatus('',' BlitSurface Stuff');
        {$endif}
        // creating and freeing the surface could be done once, if Cover.W and Cover.H don't change
        CacheMipmapSurface:=SDL_CreateRGBSurfaceFrom(@CacheMipmap[0], Covers.W, Covers.H, 24, Covers.W*3, $000000ff, $0000ff00, $00ff0000, 0);
        SDL_BlitSurface(MipMapSurface,nil,CacheMipmapSurface,nil);
        SDL_FreeSurface(CacheMipmapSurface);
        {$ifdef blindydebug}
        Log.LogStatus('',' ok');
        Log.LogStatus('',' SDL_FreeSurface (CacheMipmap)');
        {$endif}
        SDL_FreeSurface(MipmapSurface);
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
  
  {$ifdef blindydebug}
  Log.LogStatus('',' JB-2');
  {$endif}


 // now we might colorize the whole thing
  if Typ='Colorized' then
    ColorizeTexture(TexSurface,Col);
    
 // save actual dimensions of our texture
  oldWidth:=newWidth;
  oldHeight:=newHeight;
 // make texture dimensions be powers of 2
  newWidth:=Round(Power(2, Ceil(Log2(newWidth))));
  newHeight:=Round(Power(2, Ceil(Log2(newHeight))));
  if (newHeight <> oldHeight) or (newWidth <> oldWidth) then
    FitTexture(TexSurface,newWidth,newHeight);

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
 
  // JB_linux : this is causing AV's on linux... ActText seems to be nil !
//  {$IFnDEF win32}
//  if pointer(ActTex) = nil then
//    exit;
//  {$endif}
    
  glGenTextures(1, @ActTex);

  glBindTexture(GL_TEXTURE_2D, ActTex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

 // load data into gl texture
  if (Typ = 'Transparent') or
     (Typ='Colorized') then
  begin
    glTexImage2D(GL_TEXTURE_2D, 0, 4, newWidth, newHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, TexSurface.pixels);
  end
  {if Typ = 'Plain' then} else
  begin
    glTexImage2D(GL_TEXTURE_2D, 0, 3, newWidth, newHeight, 0, GL_RGB, GL_UNSIGNED_BYTE, TexSurface.pixels);
  end;
  
  {$ifdef blindydebug}
  Log.LogStatus('',' JB-4');
  {$endif}

{
  if Typ = 'Transparent Range' then
    // set alpha to 256-green-component (not sure)
        Pix := TextureB.Canvas.Pixels[Position2, Position];
        TextureD32[Position*TexNewW + Position2+1, 1] := Pix;
        TextureD32[Position*TexNewW + Position2+1, 2] := Pix div 256;
        TextureD32[Position*TexNewW + Position2+1, 3] := Pix div (256*256);
        TextureD32[Position*TexNewW + Position2+1, 4] := 256 - Pix div 256;
}
{
  if Typ = 'Font' then
    // either create luminance-alpha texture
    // or use transparency from differently saved file
    // or do something totally different (text engine with ttf)
        Pix := PPix[Position2 * 3];
        TextureD16[Position*TextureB.Width + Position2 + 1, 1] := 255;
        TextureD16[Position*TextureB.Width + Position2 + 1, 2] := Pix;
    glTexImage2D(GL_TEXTURE_2D, 0, 2, TextureB.Width, TextureB.Height, 0, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @TextureD16);
}
{
  if Typ = 'Font Outline' then
    // no idea...
  begin
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
  end;
}
{
  if Typ = 'Font Outline 2' then
    // same as above
  begin
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
  end;

  if Typ = 'Font Black' then
    // and so on
  begin
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
  end;

  if Typ = 'Alpha Black Colored' then
    // ... hope, noone needs this
  begin
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
  end;

  if Typ = 'Font Gray' then
  begin
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
  end;

  if Typ = 'Arrow' then
  begin
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
  end;

  if Typ = 'Note Plain' then
  begin
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
  end;

  if Typ = 'Note Transparent' then
  begin
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
  end;
}

  {$ifdef blindydebug}
  Log.LogStatus('',' JB-5');
  {$endif}


  Result.X := 0;
  Result.Y := 0;
  Result.Z := 0;
  Result.W := 0;
  Result.H := 0;
  Result.ScaleW := 1;
  Result.ScaleH := 1;
  Result.Rot := 0;
  Result.TexNum := ActTex;
  Result.TexW := oldWidth / newWidth;
  Result.TexH := oldHeight / newHeight;

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
  
  {$ifdef blindydebug}
  Log.LogStatus('',' JB-6');
  {$endif}


  // 0.5.0
  Result.Name := Identifier;

  SDL_FreeSurface(TexSurface);

  {$ifdef blindydebug}
  Log.LogStatus('',' JB-7');
  {$endif}


  Log.BenchmarkEnd(4);
  if Log.BenchmarkTimeLength[4] >= 1 then
    Log.LogBenchmark('**********> Texture Load Time Warning - ' + Format + '/' + Identifier + '/' + Typ, 4);
    
  {$ifdef blindydebug}
  Log.LogStatus('',' JB-8');
  {$endif}

end;


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

  if Name = '' then
    exit;

  // find texture entry
  T := FindTexture(Name);

  if T = -1 then
  begin
    // create texture entry
    T := Length(TextureDatabase.Texture);
    SetLength(TextureDatabase.Texture, T+1);

    TextureDatabase.Texture[T].Name := Name;
    TextureDatabase.Texture[T].Typ  := Typ;

    // inform database that no textures have been loaded into memory
    TextureDatabase.Texture[T].Texture.TexNum      := -1;
    TextureDatabase.Texture[T].TextureCache.TexNum := -1;
  end;

  // use preloaded texture
  if (not FromCache) or (FromCache and not Covers.CoverExists(Name)) then
  begin
    // use full texture
    if TextureDatabase.Texture[T].Texture.TexNum = -1 then
    begin
      // load texture
      {$ifdef blindydebug}
      Log.LogStatus('...', 'GetTexture('''+Name+''','''+Typ+''')');
      {$endif}
      TextureDatabase.Texture[T].Texture := LoadTexture(false, pchar(Name), 'JPG', pchar(Typ), $0);
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

    if TextureDatabase.Texture[T].TextureCache.TexNum = -1 then
    begin
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

function TTextureUnit.LoadTexture(Identifier, Format, Typ: PChar; Col: LongWord): TTexture;
begin
  Result := LoadTexture(false, Identifier, Format, Typ, Col);
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

  glGenTextures(1, @ActTex); // ActText = new texture number
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
  TexNum: integer;
begin
  T := FindTexture(Name);

  if not FromCache then begin
    TexNum := TextureDatabase.Texture[T].Texture.TexNum;
    if TexNum >= 0 then begin
      glDeleteTextures(1, PGLuint(@TexNum));
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

{$IFDEF LAZARUS}
initialization
  {$I UltraStar.lrs}
{$ENDIF}


end.
