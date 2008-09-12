unit TextGL;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  gl,
  SDL,
  UTexture,
//  SDL_ttf,
  ULog;

procedure BuildFont;                          // build our bitmap font
procedure KillFont;                           // delete the font
function  glTextWidth(text: PChar): real;     // returns text width
procedure glPrintLetter(letter: char);
procedure glPrint(text: pchar);               // custom GL "Print" routine
procedure SetFontPos(X, Y: real);             // sets X and Y
procedure SetFontZ(Z: real);                  // sets Z
procedure SetFontSize(Size: real);
procedure SetFontStyle(Style: integer);       // sets active font style (normal, bold, etc)
procedure SetFontItalic(Enable: boolean);     // sets italic type letter (works for all fonts)
procedure SetFontAspectW(Aspect: real);
procedure SetFontReflection(Enable:boolean;Spacing: real); // enables/disables text reflection
procedure SetFontBlend(Enable: boolean);      // enables/disables blending

//function NextPowerOfTwo(Value: integer): integer;
// Checks if the ttf exists, if yes then a SDL_ttf is returned
//function LoadFont(FileName: PAnsiChar; PointSize: integer):PTTF_Font;
// Does the renderstuff, color is in $ffeecc style
//function RenderText(font: PTTF_Font; Text:PAnsiChar; Color: Cardinal):PSDL_Surface;

type
  TTextGL = record
    X:        real;
    Y:        real;
    Z:        real;
    Text:     string;
    Size:     real;
    ColR:     real;
    ColG:     real;
    ColB:     real;
  end;

  PFont = ^TFont;
  TFont = record
    Tex:      TTexture;
    Width:    array[0..255] of byte;
    AspectW:  real;
    Centered: boolean;
    Outline:  real;
    Italic:   boolean;
    Reflection: boolean;
    ReflectionSpacing: real;
    Blend: boolean;
  end;


var
  Fonts:      array of TFont;
  ActFont:    integer;


implementation

uses
  UMain,
  UCommon,
  SysUtils,
  IniFiles,
  Classes,
  UGraphic;

var
  // Colours for the reflection
  TempColor:    array[0..3] of GLfloat;

{**
 * Load font info.
 * FontFile is the name of the image (.png) not the data (.dat) file
 *}
procedure LoadFontInfo(FontID: integer; const FontFile: string);
var
  Stream:  TFileStream;
  DatFile: string;
begin
  DatFile := ChangeFileExt(FontFile, '.dat');
  FillChar(Fonts[FontID].Width[0], Length(Fonts[FontID].Width), 0);

  Stream := nil;
  try
    Stream := TFileStream.Create(DatFile, fmOpenRead);
    Stream.Read(Fonts[FontID].Width, 256);
  except
    Log.LogError('Error while reading font['+ inttostr(FontID) +']', 'LoadFontInfo');
  end;
  Stream.Free;
end;

// Builds bitmap fonts
procedure BuildFont;
var
  Count: integer;
  FontIni: TMemIniFile;
  FontFile: string;     // filename of the image (with .png/... ending)
begin
  ActFont := 0;

  SetLength(Fonts, 4);
  FontIni := TMemIniFile.Create(FontPath + 'fonts.ini');

  // Normal

  FontFile := FontPath + FontIni.ReadString('Normal', 'File', '');

  Fonts[0].Tex := Texture.LoadTexture(true, FontFile, TEXTURE_TYPE_TRANSPARENT, 0);
  Fonts[0].Tex.H := 30;
  Fonts[0].AspectW := 0.9;
  Fonts[0].Outline := 0;

  LoadFontInfo(0, FontFile);

  // Bold

  FontFile := FontPath + FontIni.ReadString('Bold', 'File', '');

  Fonts[1].Tex := Texture.LoadTexture(true, FontFile, TEXTURE_TYPE_TRANSPARENT, 0);
  Fonts[1].Tex.H := 30;
  Fonts[1].AspectW := 1;
  Fonts[1].Outline := 0;

  LoadFontInfo(1, FontFile);
  for Count := 0 to 255 do
    Fonts[1].Width[Count] := Fonts[1].Width[Count] div 2;

  // Outline1

  FontFile := FontPath + FontIni.ReadString('Outline1', 'File', '');

  Fonts[2].Tex := Texture.LoadTexture(true, FontFile, TEXTURE_TYPE_TRANSPARENT, 0);
  Fonts[2].Tex.H := 30;
  Fonts[2].AspectW := 0.95;
  Fonts[2].Outline := 5;

  LoadFontInfo(2, FontFile);
  for Count := 0 to 255 do
    Fonts[2].Width[Count] := Fonts[2].Width[Count] div 2 + 2;

  // Outline2

  FontFile := FontPath + FontIni.ReadString('Outline2', 'File', '');

  Fonts[3].Tex := Texture.LoadTexture(true, FontFile, TEXTURE_TYPE_TRANSPARENT, 0);
  Fonts[3].Tex.H := 30;
  Fonts[3].AspectW := 0.95;
  Fonts[3].Outline := 4;

  LoadFontInfo(3, FontFile);
  for Count := 0 to 255 do
    Fonts[3].Width[Count] := Fonts[3].Width[Count] + 1;


  // close ini-file
  FontIni.Free;

  // enable blending by default
  for Count := 0 to High(Fonts) do
    Fonts[Count].Blend := true;
end;

// Deletes the font
procedure KillFont;
begin
  // delete all characters
  //glDeleteLists(..., 256);
end;

function glTextWidth(text: pchar): real;
var
  Letter: char;
  i: integer;
begin
  Result := 0;
  for i := 0 to Length(text) -1 do
  begin
    Letter := Text[i];
    Result := Result + Fonts[ActFont].Width[Ord(Letter)] * Fonts[ActFont].Tex.H / 30 * Fonts[ActFont].AspectW;
  end;
end;

procedure glPrintLetter(Letter: char);
var
  TexX, TexY:        real;
  TexR, TexB:        real;
  TexHeight:         real;
  FWidth:            real;
  PL, PT:            real;
  PR, PB:            real;
  XItal:             real; // X shift for italic type letter
  ReflectionSpacing: real; // Distance of the reflection
  Font:              PFont;
  Tex:               PTexture;
begin
  Font := @Fonts[ActFont];
  Tex := @Font.Tex;

  FWidth := Font.Width[Ord(Letter)];

  Tex.W := FWidth * (Tex.H/30) * Font.AspectW;

  // set texture positions
  TexX := (ord(Letter) mod 16) * 1/16 + 1/32 - FWidth/1024 - Font.Outline/1024;
  TexY := (ord(Letter) div 16) * 1/16 + 2/1024;
  TexR := (ord(Letter) mod 16) * 1/16 + 1/32 + FWidth/1024 + Font.Outline/1024;
  TexB := (1 + ord(Letter) div 16) * 1/16 - 2/1024;

  TexHeight := TexB - TexY;

  // set vector positions
  PL := Tex.X - Font.Outline * (Tex.H/30) * Font.AspectW /2;
  PT := Tex.Y;
  PR := PL + Tex.W + Font.Outline * (Tex.H/30) * Font.AspectW;
  PB := PT + Tex.H;

  if (not Font.Italic) then
    XItal := 0
  else
    XItal := 12;

  if (Font.Blend) then
  begin
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  end;

  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, Tex.TexNum);
  
  glBegin(GL_QUADS);
    glTexCoord2f(TexX, TexY); glVertex2f(PL+XItal,  PT);
    glTexCoord2f(TexX, TexB); glVertex2f(PL,        PB);
    glTexCoord2f(TexR, TexB); glVertex2f(PR,        PB);
    glTexCoord2f(TexR, TexY); glVertex2f(PR+XItal,  PT);
  glEnd;

  // <mog> Reflection
  // Yes it would make sense to put this in an extra procedure,
  // but this works, doesn't take much lines, and is almost lightweight
  if Font.Reflection then
  begin
    ReflectionSpacing := Font.ReflectionSpacing + Tex.H/2;

    glDepthRange(0, 10);
    glDepthFunc(GL_LEQUAL);
    glEnable(GL_DEPTH_TEST);

    glBegin(GL_QUADS);
      glColor4f(TempColor[0], TempColor[1], TempColor[2], 0);
      glTexCoord2f(TexX, TexY + TexHeight/2);
      glVertex3f(PL, PB + ReflectionSpacing - Tex.H/2, Tex.z);

      glColor4f(TempColor[0], TempColor[1], TempColor[2], Tex.Alpha-0.3);
      glTexCoord2f(TexX, TexB );
      glVertex3f(PL + XItal, PT + ReflectionSpacing, Tex.z);

      glTexCoord2f(TexR, TexB );
      glVertex3f(PR + XItal, PT + ReflectionSpacing, Tex.z);

      glColor4f(TempColor[0], TempColor[1], TempColor[2], 0);
      glTexCoord2f(TexR, TexY + TexHeight/2);
      glVertex3f(PR, PB + ReflectionSpacing - Tex.H/2, Tex.z);
    glEnd;

    glDisable(GL_DEPTH_TEST);
  end; // reflection

  glDisable(GL_TEXTURE_2D);
  if (Font.Blend) then
    glDisable(GL_BLEND);

  Tex.X := Tex.X + Tex.W;

  //write the colour back
  glColor4fv(@TempColor);
end;

// Custom GL "Print" Routine
procedure glPrint(Text: PChar);
var
  Pos: integer;
begin
  // if there is no text do nothing
  if ((Text = nil) or (Text = '')) then
    Exit;

  //Save the actual color and alpha (for reflection)
  glGetFloatv(GL_CURRENT_COLOR, @TempColor);

  for Pos := 0 to Length(Text) - 1 do
  begin
    glPrintLetter(Text[Pos]);
  end;
end;

procedure SetFontPos(X, Y: real);
begin
  Fonts[ActFont].Tex.X := X;
  Fonts[ActFont].Tex.Y := Y;
end;

procedure SetFontZ(Z: real);
begin
  Fonts[ActFont].Tex.Z := Z;
end;

procedure SetFontSize(Size: real);
begin
  Fonts[ActFont].Tex.H := 30 * (Size/10);
end;

procedure SetFontStyle(Style: integer);
begin
  ActFont := Style;
end;

procedure SetFontItalic(Enable: boolean);
begin
  Fonts[ActFont].Italic := Enable;
end;

procedure SetFontAspectW(Aspect: real);
begin
  Fonts[ActFont].AspectW := Aspect;
end;

procedure SetFontReflection(Enable: boolean; Spacing: real);
begin
  Fonts[ActFont].Reflection        := Enable;
  Fonts[ActFont].ReflectionSpacing := Spacing;
end;

procedure SetFontBlend(Enable: boolean);
begin
  Fonts[ActFont].Blend := Enable;
end;




(*
<mog> I uncommented this, because it was some kind of after hour hack together with blindy
it's actually just a prove of concept, as it's having some flaws
- instead nice and clean ttf code should be placed here :)

{$IFDEF FPC}
  {$ASMMODE Intel}
{$ENDIF}

function NextPowerOfTwo(Value: integer): integer;
begin
  Result:= 1;
{$IF Defined(CPUX86_64)}
  asm
    mov rcx, -1
    bsr rcx, Value
    inc rcx
    shl Result, cl
  end;
{$ELSEIF Defined(CPU386) or Defined(CPUI386)}
  asm
    mov ecx, -1
    bsr ecx, Value
    inc ecx
    shl Result, cl
  end;
{$ELSE}
  while (Result <= Value) do
    Result := 2 * Result;
{$IFEND}
end;

function LoadFont(FileName: PAnsiChar; PointSize: integer):PTTF_Font;
begin
 if (FileExists(FileName)) then
   begin
     Result := TTF_OpenFont( FileName, PointSize );
   end
 else
   begin
     Log.LogStatus('ERROR Could not find font in ' + FileName , '');
     ShowMessage(  'ERROR Could not find font in ' + FileName );
     Result := nil;
   end;
end;

function RenderText(font: PTTF_Font; Text:PAnsiChar; Color: Cardinal): PSDL_Surface;
var
  clr : TSDL_color;
begin
  clr.r  := ((Color and $ff0000) shr 16  ) div 255;
  clr.g  := ((Color and $ff00  ) shr 8   ) div 255;
  clr.b  := ( Color and $ff    ) div 255 ;

  result := TTF_RenderText_Blended( font, text, cLr);
end;

procedure printrandomtext();
var
  stext,intermediary : PSDL_surface;
  clrFg, clrBG       : TSDL_color;
  texture            : Gluint;
  font               : PTTF_Font;
  w,h                : integer;
begin

  font := LoadFont('fonts\comicbd.ttf', 42);

  clrFg.r := 255;
  clrFg.g := 255;
  clrFg.b := 255;
  clrFg.unused := 255;

  clrBg.r := 255;
  clrbg.g := 0;
  clrbg.b := 255;
  clrbg.unused := 0;

  sText := RenderText(font, 'katzeeeeeee', $fe198e);
  //sText :=  TTF_RenderText_Blended( font, 'huuuuuuuuuund', clrFG);

  // Convert the rendered text to a known format
  w := nextpoweroftwo(sText.w);
  h := nextpoweroftwo(sText.h);

  intermediary := SDL_CreateRGBSurface(0, w, h, 32,
      $000000ff, $0000ff00, $00ff0000, $ff000000);

  SDL_SetAlpha(intermediary, 0, 255);
  SDL_SetAlpha(sText,        0, 255);
  SDL_BlitSurface(sText, nil, intermediary, nil);

  glGenTextures(1, @texture);

  glBindTexture(GL_TEXTURE_2D, texture);

  glTexImage2D(GL_TEXTURE_2D, 0, 4, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, intermediary.pixels);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  glEnable(GL_TEXTURE_2D);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);
  glBindTexture(GL_TEXTURE_2D, texture);
  glColor4f(1, 0, 1, 1);

  glbegin(gl_quads);
  glTexCoord2f(0, 0);                 glVertex2f(200          , 300          );
  glTexCoord2f(0, sText.h/h);         glVertex2f(200          , 300 + sText.h);
  glTexCoord2f(sText.w/w, sText.h/h); glVertex2f(200 + sText.w, 300 + sText.h);
  glTexCoord2f(sText.w/w, 0);         glVertex2f(200 + sText.w, 300          );
  glEnd;
  glfinish();
  glDisable(GL_BLEND);
  gldisable(gl_texture_2d);

  SDL_FreeSurface(sText);
  SDL_FreeSurface(intermediary);
  glDeleteTextures(1, @texture);
  TTF_CloseFont(font);

end;
*)


end.
