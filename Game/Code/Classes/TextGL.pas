unit TextGL;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}


uses
  OpenGL12,
  SDL,
  UTexture,
  Classes,
  SDL_ttf,
  ULog;

procedure BuildFont;			                // Build Our Bitmap Font
procedure KillFont;     		                // Delete The Font
function  glTextWidth(text: pchar): real;     // Returns Text Width
procedure glPrintDone(text: pchar; Done: real; ColR, ColG, ColB: real);
procedure glPrintLetter(letter: char);
procedure glPrintLetterCut(letter: char; Start, Finish: real);
procedure glPrint(text: pchar);	                  // Custom GL "Print" Routine
procedure glPrintCut(text: pchar);
procedure SetFontPos(X, Y: real);                     // Sets X And Y
procedure SetFontSize(Size: real);
procedure SetFontStyle(Style: integer); // sets active font style (normal, bold, etc)
procedure SetFontItalic(Enable: boolean); // sets italic type letter (works for all fonts)
procedure SetFontAspectW(Aspect: real);

// Start of SDL_ttf
function NextPowerOfTwo(Value: Integer): Integer;
//Checks if the ttf exists, if yes then a SDL_ttf is returned
function LoadFont(FileName: PAnsiChar; PointSize: integer):PTTF_Font;

// Does the renderstuff, color is in $ffeecc style
function RenderText(font: PTTF_Font; Text:PAnsiChar; Color: Cardinal):PSDL_Surface;
procedure printrandomtext();
// End of SDL_ttf

type
  TTextGL = record
    X:        real;
    Y:        real;
    Text:     string;
    Size:     real;
    ColR:     real;
    ColG:     real;
    ColB:     real;
  end;

  TFont = record
    Tex:      TTexture;
    Width:    array[0..255] of byte;
    AspectW:  real;
    Centered: boolean;
    Done:     real;
    Outline:  real;
    Italic:   boolean;
  end;


var
  base:       GLuint;			                // Base Display List For The Font Set
  Fonts:      array of TFont;
  ActFont:    integer;
  PColR:      real;  // temps for glPrintDone
  PColG:      real;
  PColB:      real;

implementation

uses
  UMain,
  UCommon,
  SysUtils,
  {$IFDEF LCL}
  LResources,
  {$ENDIF}
  {$IFDEF DARWIN}
  MacResources,
  {$ENDIF}
  UGraphic;

procedure BuildFont;			                // Build Our Bitmap Font

  procedure loadfont( aID : integer; aType, aResourceName : String);
  {$IFDEF LCL}
  var
    lLazRes  : TLResource;
    lResData : TStringStream;
  begin
      try
        lLazRes := LazFindResource( aResourceName, aType );
        if lLazRes <> nil then
        begin
          lResData := TStringStream.create( lLazRes.value );
          try
            lResData.position := 0;
            lResData.Read(Fonts[ aID ].Width, 256);
          finally
            freeandnil( lResData );
          end;
        end;
  {$ELSE}
  var
    Reg:  TResourceStream;
  begin
    try
        Reg := TResourceStream.Create(HInstance, aResourceName , pchar( aType ) );
        try
          Reg.Read(Fonts[ aID ].Width, 256);
        finally
          Reg.Free;
        end;
  {$ENDIF}
      
    except
      Log.LogStatus( 'Could not load font : loadfont( '+ inttostr( aID ) +' , '+aType+' )' , 'ERROR');
    end;
  end;

var
  Count:      integer;
begin
  ActFont := 0;

  //Log.LogStatus( '' , '---------------------------');

  //Log.LogStatus( 'Font' , '---------------------------');
  SetLength(Fonts, 5);
  Fonts[0].Tex := Texture.LoadTexture(true, 'Font', TEXTURE_TYPE_TRANSPARENT, 0);
  Fonts[0].Tex.H := 30;
  Fonts[0].AspectW := 0.9;
  Fonts[0].Done := -1;
  Fonts[0].Outline := 0;

  //Log.LogStatus( 'FontB' , '---------------------------');

  Fonts[1].Tex := Texture.LoadTexture(true, 'FontB', TEXTURE_TYPE_TRANSPARENT, 0);
  Fonts[1].Tex.H := 30;
  Fonts[1].AspectW := 1;
  Fonts[1].Done := -1;
  Fonts[1].Outline := 0;

  //Log.LogStatus( 'FontO' , '---------------------------');
  Fonts[2].Tex := Texture.LoadTexture(true, 'FontO', TEXTURE_TYPE_TRANSPARENT, 0);
  Fonts[2].Tex.H := 30;
  Fonts[2].AspectW := 0.95;
  Fonts[2].Done := -1;
  Fonts[2].Outline := 5;

  //Log.LogStatus( 'FontO2' , '---------------------------');
  Fonts[3].Tex := Texture.LoadTexture(true, 'FontO2', TEXTURE_TYPE_TRANSPARENT, 0);
  Fonts[3].Tex.H := 30;
  Fonts[3].AspectW := 0.95;
  Fonts[3].Done := -1;
  Fonts[3].Outline := 4;

{  Fonts[4].Tex := Texture.LoadTexture('FontO', TEXTURE_TYPE_TRANSPARENT, 0); // for score screen
  Fonts[4].Tex.H := 30;
  Fonts[4].AspectW := 0.95;
  Fonts[4].Done := -1;
  Fonts[4].Outline := 5;}



  loadfont( 0, 'FNT', 'Font'   );
  loadfont( 1, 'FNT', 'FontB'  );
  loadfont( 2, 'FNT', 'FontO'  );
  loadfont( 3, 'FNT', 'FontO2' );

{  Reg := TResourceStream.Create(HInstance, 'FontO', 'FNT');
  Reg.Read(Fonts[4].Width, 256);
  Reg.Free;}

  for Count := 0 to 255 do
    Fonts[1].Width[Count] := Fonts[1].Width[Count] div 2;

  for Count := 0 to 255 do
    Fonts[2].Width[Count] := Fonts[2].Width[Count] div 2 + 2;

  for Count := 0 to 255 do
    Fonts[3].Width[Count] := Fonts[3].Width[Count] + 1;

{  for Count := 0 to 255 do
    Fonts[4].Width[Count] := Fonts[4].Width[Count] div 2 + 2;}

end;

procedure KillFont;     		                // Delete The Font
begin
//  glDeleteLists(base, 256); 		                // Delete All 96 Characters
end;

function glTextWidth(text: pchar): real;
var
  Letter: char;
  i: integer;
begin
//  Log.LogStatus(Text, 'glTextWidth');
  Result := 0;
  for i := 0 to Length(text) -1 do  //  Patched by AlexanderS : bug with wrong sliced text lines
  begin
    Letter := Text[i];
    // Bugfix: does not work with FPC, probably because a part of text is assigned to itself
    //text := pchar(Copy(text, 2, Length(text)-1));
    Result := Result + Fonts[ActFont].Width[Ord(Letter)] * Fonts[ActFont].Tex.H / 30 * Fonts[ActFont].AspectW;
  end;
end;

procedure glPrintDone(text: pchar; Done: real; ColR, ColG, ColB: real);
begin
  Fonts[ActFont].Done := Done;
  PColR := ColR;
  PColG := ColG;
  PColB := ColB;
  glPrintCut(text);
  Fonts[ActFont].Done := -1;
end;

procedure glPrintLetter(Letter: char);
var
  TexX, TexY:   real;
  TexR, TexB:   real;
  FWidth:       real;
  PL, PT:       real;
  PR, PB:       real;
  XItal:        real; // X shift for italic type letter
begin
  with Fonts[ActFont].Tex do
  begin
    FWidth := Fonts[ActFont].Width[Ord(Letter)];

    W := FWidth * (H/30) * Fonts[ActFont].AspectW;
  //  H := 30;

    // set texture positions
    TexX := (ord(Letter) mod 16) * 1/16 + 1/32 - FWidth/1024 - Fonts[ActFont].Outline/1024;
    TexY := (ord(Letter) div 16) * 1/16 + 2/1024; // 2/1024
    TexR := (ord(Letter) mod 16) * 1/16 + 1/32 + FWidth/1024 + Fonts[ActFont].Outline/1024;
    TexB := (1 + ord(Letter) div 16) * 1/16 - 2/1024;

    // set vector positions
    PL := X - Fonts[ActFont].Outline * (H/30) * Fonts[ActFont].AspectW /2;
    PT := Y;
    PR := PL + W + Fonts[ActFont].Outline * (H/30) * Fonts[ActFont].AspectW;
    PB := PT + H;

    if Fonts[ActFont].Italic = false then
      XItal := 0
    else
      XItal := 12;

    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glBindTexture(GL_TEXTURE_2D, TexNum);

    glBegin(GL_QUADS);
    try
      glTexCoord2f(TexX, TexY); glVertex2f(PL+XItal,  PT);
      glTexCoord2f(TexX, TexB); glVertex2f(PL,        PB);
      glTexCoord2f(TexR, TexB); glVertex2f(PR,        PB);
      glTexCoord2f(TexR, TexY); glVertex2f(PR+XItal,  PT);
    finally
      glEnd;
    end;

    X := X + W;
    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
  end; // with
end;

procedure glPrintLetterCut(letter: char; Start, Finish: real);
var
  TexX, TexY:   real;
  TexR, TexB:   real;
  TexTemp:      real;
  FWidth:       real;
  PL, PT:       real;
  PR, PB:       real;
  OutTemp:      real;
  XItal:        real;
begin
  with Fonts[ActFont].Tex do begin
  FWidth := Fonts[ActFont].Width[Ord(Letter)];

  W := FWidth * (H/30) * Fonts[ActFont].AspectW;
//  H := 30;
  OutTemp := Fonts[ActFont].Outline * (H/30) * Fonts[ActFont].AspectW;

  // set texture positions
  TexX := (ord(Letter) mod 16) * 1/16 + 1/32 - FWidth/1024 - Fonts[ActFont].Outline/1024;
  TexY := (ord(Letter) div 16) * 1/16 + 2/1024; // 2/1024
  TexR := (ord(Letter) mod 16) * 1/16 + 1/32 + FWidth/1024 + Fonts[ActFont].Outline/1024;
  TexB := (1 + ord(Letter) div 16) * 1/16 - 2/1024;

  TexTemp := TexX + Start * (TexR - TexX);
  TexR := TexX + Finish * (TexR - TexX);
  TexX := TexTemp;

  // set vector positions
  PL := X - OutTemp / 2 + OutTemp * Start;
  PT := Y;
  PR := PL + (W + OutTemp) * (Finish - Start);
  PB := PT + H;
  if Fonts[ActFont].Italic = false then
    XItal := 0
  else
    XItal := 12;

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindTexture(GL_TEXTURE_2D, TexNum);
  glBegin(GL_QUADS);
    glTexCoord2f(TexX, TexY); glVertex2f(PL+XItal,  PT);
    glTexCoord2f(TexX, TexB); glVertex2f(PL,        PB);
    glTexCoord2f(TexR, TexB); glVertex2f(PR,        PB);
    glTexCoord2f(TexR, TexY); glVertex2f(PR+XItal,  PT); // not tested with XItal
  glEnd;
  X := X + W * (Finish - Start);
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
  end; // with

end;

procedure glPrint(text: pchar);	                // Custom GL "Print" Routine
var
//  Letter :       char;
  iPos   : Integer;

begin
  if (Text = '') then     // If There's No Text
    Exit;					        // Do Nothing

(*
  while (length(text) > 0) do
  begin
    // cut
    Letter := Text[0];
    Text   := pchar(Copy(Text, 2, Length(Text)-1));

    // print
    glPrintLetter(Letter);
  end; // while
*)

  // This code is better, because doing a Copy of for every
  // letter in a string is a waste of CPU & Memory resources.
  // Copy operations are quite memory intensive, and this simple
  // code achieves the same result.
  for iPos := 0 to length( text ) - 1 do
  begin
    glPrintLetter( Text[iPos] );
  end;

end;

function NextPowerOfTwo(Value: Integer): Integer;
// tyty to Asphyre
begin
 Result:= 1;
 asm
  xor ecx, ecx
  bsr ecx, Value
  inc ecx
  shl Result, cl
 end;
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
  w :=  nextpoweroftwo(sText.w);
  h :=  nextpoweroftwo(sText.h);

intermediary := SDL_CreateRGBSurface(0, w, h, 32,
			$000000ff, $0000ff00, $00ff0000, $ff000000);

 SDL_SetAlpha(intermediary, 0, 255);
 SDL_SetAlpha(sText, 0, 255);
 SDL_BlitSurface(sText, nil, intermediary, nil);

 glGenTextures(1, @texture);

  glBindTexture(GL_TEXTURE_2D, texture);

  glTexImage2D(GL_TEXTURE_2D, 0, 4, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, intermediary.pixels);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);




      glEnable(GL_TEXTURE_2D);
      glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
      glEnable(GL_BLEND);
        glBindTexture(GL_TEXTURE_2D, texture);
        glColor4f(1, 0, 1, 1);

      glbegin(gl_quads);
        glTexCoord2f(0,0); glVertex2f(200,     300);
        glTexCoord2f(0,sText.h/h); glVertex2f(200    , 300 + sText.h);
        glTexCoord2f(sText.w/w,sText.h/h); glVertex2f(200 + sText.w, 300 + sText.h);
        glTexCoord2f(sText.w/w,0); glVertex2f(200 + sText.w, 300);
      glEnd;
      glfinish();
      glDisable(GL_BLEND);
      gldisable(gl_texture_2d);




SDL_FreeSurface( sText );
SDL_FreeSurface( intermediary );
glDeleteTextures(1, @texture);
TTF_CloseFont( font );

end;

procedure glPrintCut(text: pchar);
var
  Letter:       char;
  PToDo:        real;
  PTotWidth:    real;
  PDoingNow:    real;
  S:            string;
begin
  if (Text = '') then   			        // If There's No Text
                Exit;					        // Do Nothing

  PTotWidth := glTextWidth(Text);
  PToDo := Fonts[ActFont].Done;

  while (length(text) > 0) do begin
    // cut
    Letter := Text[0];
    Text := pchar(Copy(Text, 2, Length(Text)-1));

    // analyze
    S := Letter;
    PDoingNow := glTextWidth(pchar(S)) / PTotWidth;

    // drawing
    if (PToDo > 0) and (PDoingNow <= PToDo) then
      glPrintLetter(Letter);

    if (PToDo > 0) and (PDoingNow > PToDo) then begin
      glPrintLetterCut(Letter, 0, PToDo / PDoingNow);
      glColor3f(PColR, PColG,  PColB);
      glPrintLetterCut(Letter, PToDo / PDoingNow, 1);
    end;

    if (PToDo <= 0) then
      glPrintLetter(Letter);

    PToDo := PToDo - PDoingNow;

  end; // while
end;


procedure SetFontPos(X, Y: real);
begin
  Fonts[ActFont].Tex.X := X;
  Fonts[ActFont].Tex.Y := Y;
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

end.


