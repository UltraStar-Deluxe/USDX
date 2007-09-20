unit TextGL;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}


uses  OpenGL12,
      SDL,
      UTexture,
      Classes,
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

uses UMain,
     {$IFDEF win32}
     windows,
     {$ELSE}
     lclintf,
     lcltype,
     {$ENDIF}
     SysUtils,
     {$IFDEF FPC}
     LResources,
     {$ENDIF}
     UGraphic;

procedure BuildFont;			                // Build Our Bitmap Font

  procedure loadfont( aID : integer; aType, aResourceName : String);
  var
    Rejestr:  TResourceStream;
  begin
    {$IFNDEF FPC}
    Log.LogStatus( 'TextGL - BUILDFONT - load Font Resource - ' + inttostr( integer( aID ) ) , aType +' '+ aResourceName  );
    
    Rejestr := TResourceStream.Create(HInstance, aResourceName , pchar( aType ) );
    try
      Rejestr.Read(Fonts[ aID ].Width, 256);
    finally
      Rejestr.Free;
    end;
    {$ENDIF}
  end;

var
  font:     HFONT;              	          // Windows Font ID
  h_dc:     hdc;
  Pet:      integer;
begin
  ActFont := 0;

  SetLength(Fonts, 5);
  Fonts[0].Tex := Texture.LoadTexture(true, 'Font', 'PNG', 'Font', 0);
  Fonts[0].Tex.H := 30;
  Fonts[0].AspectW := 0.9;
  Fonts[0].Done := -1;
  Fonts[0].Outline := 0;

  Fonts[1].Tex := Texture.LoadTexture(true, 'FontB', 'PNG', 'Font', 0);
  Fonts[1].Tex.H := 30;
  Fonts[1].AspectW := 1;
  Fonts[1].Done := -1;
  Fonts[1].Outline := 0;

  Fonts[2].Tex := Texture.LoadTexture(true, 'FontO', 'PNG', 'Font Outline', 0);
  Fonts[2].Tex.H := 30;
  Fonts[2].AspectW := 0.95;
  Fonts[2].Done := -1;
  Fonts[2].Outline := 5;

  Fonts[3].Tex := Texture.LoadTexture(true, 'FontO2', 'PNG', 'Font Outline 2', 0);
  Fonts[3].Tex.H := 30;
  Fonts[3].AspectW := 0.95;
  Fonts[3].Done := -1;
  Fonts[3].Outline := 4;

{  Fonts[4].Tex := Texture.LoadTexture('FontO', 'BMP', 'Arrow', 0); // for score screen
  Fonts[4].Tex.H := 30;
  Fonts[4].AspectW := 0.95;
  Fonts[4].Done := -1;
  Fonts[4].Outline := 5;}



  {$IFDEF FPC}
    loadfont( 0, 'DAT', 'eurostar_regular'   );
    loadfont( 1, 'DAT', 'eurostar_regular_bold'  );
    loadfont( 2, 'DAT', 'Outline 1'  );
    loadfont( 3, 'DAT', 'Outline 2' );
  {$ELSE}
    loadfont( 0, 'FNT', 'Font'   );
    loadfont( 1, 'FNT', 'FontB'  );
    loadfont( 2, 'FNT', 'FontO'  );
    loadfont( 3, 'FNT', 'FontO2' );
  {$ENDIF}




{  Rejestr := TResourceStream.Create(HInstance, 'FontO', 'FNT');
  Rejestr.Read(Fonts[4].Width, 256);
  Rejestr.Free;}

  for Pet := 0 to 255 do
    Fonts[1].Width[Pet] := Fonts[1].Width[Pet] div 2;

  for Pet := 0 to 255 do
    Fonts[2].Width[Pet] := Fonts[2].Width[Pet] div 2 + 2;

  for Pet := 0 to 255 do
    Fonts[3].Width[Pet] := Fonts[3].Width[Pet] + 1;

{  for Pet := 0 to 255 do
    Fonts[4].Width[Pet] := Fonts[4].Width[Pet] div 2 + 2;}

end;

procedure KillFont;     		                // Delete The Font
begin
//  glDeleteLists(base, 256); 		                // Delete All 96 Characters
end;

function glTextWidth(text: pchar): real;
var
  Letter:       char;
begin
//  Log.LogStatus(Text, 'glTextWidth');
  Result := 0;
  while (length(text) > 0) do begin
    Letter := Text[0];
    text := pchar(Copy(text, 2, Length(text)-1));
    Result := Result + Fonts[ActFont].Width[Ord(Letter)] * Fonts[ActFont].Tex.H / 30 * Fonts[ActFont].AspectW;
  end; // while
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
  with Fonts[ActFont].Tex do begin
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
    glTexCoord2f(TexX, TexY); glVertex2f(PL+XItal,  PT);
    glTexCoord2f(TexX, TexB); glVertex2f(PL,        PB);
    glTexCoord2f(TexR, TexB); glVertex2f(PR,        PB);
    glTexCoord2f(TexR, TexY); glVertex2f(PR+XItal,  PT);
  glEnd;
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
  Letter:       char;
begin
  if (Text = '') then   			        // If There's No Text
                Exit;					        // Do Nothing

  while (length(text) > 0) do begin
    // cut
    Letter := Text[0];
    Text := pchar(Copy(Text, 2, Length(Text)-1));

    // print
    glPrintLetter(Letter);
  end; // while
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

{$IFDEF FPC}
{$IFDEF win32}
initialization
  {$I UltraStar.lrs}
{$ENDIF}
{$ENDIF}

end.


