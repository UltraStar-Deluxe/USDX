unit UDraw;

interface
uses UThemes, ModiSDK, UGraphicClasses;

procedure SingDraw;
procedure SingModiDraw (PlayerInfo: TPlayerInfo);
procedure SingDrawBackground;
procedure SingDrawOscilloscope(X, Y, W, H: real; NrSound: integer);
procedure SingDrawNoteLines(Left, Top, Right: real; Space: integer);
procedure SingDrawBeatDelimeters(Left, Top, Right: real; NrCzesci: integer);
procedure SingDrawCzesc(Left, Top, Right: real; NrCzesci: integer; Space: integer);
procedure SingDrawPlayerCzesc(X, Y, W: real; NrGracza: integer; Space: integer);
procedure SingDrawPlayerBGCzesc(Left, Top, Right: real; NrCzesci, NrGracza: integer; Space: integer);
{ for no use since we have UGraphicClasses
procedure SingDrawStar(X, Y, A: real);
procedure SingGoldenStar(X, Y, A: real);
}
// The Singbar
procedure SingDrawSingbar(X, Y, W, H: real; Percent: integer);

//Phrasen Bonus - Line Bonus
procedure SingDrawLineBonus( const X, Y: integer; Color: TRGB; Alpha: Single; Text: string);

//Draw Editor NoteLines
procedure EditDrawCzesc(Left, Top, Right: real; NrCzesci: integer; Space: integer);


type
  TRecR = record
    Top:    real;
    Left:   real;
    Right:  real;
    Bottom: real;

    Width:  real;
    WMid:   real;
    Height: real;
    HMid:   real;

    Mid:    real;
  end;

var
  NotesW:   real;
  NotesH:   real;
  Starfr:   integer;
  StarfrG:   integer;


  
  //SingBar Mod
  TickOld: cardinal;
  TickOld2:cardinal;
  //end Singbar Mod




const
  Przedz = 32;

implementation

uses Windows, OpenGL12, UGraphic, SysUtils, UMusic, URecord, ULog, UScreenSing, UScreenSingModi, ULyrics, UMain, TextGL, UTexture, UDrawTexture, UIni, Math, UDLLManager;

procedure SingDrawBackground;
var
  Rec:      TRecR;
  TexRec:   TRecR;
begin
  if ScreenSing.Tex_Background.TexNum >= 1 then begin

  glClearColor (1, 1, 1, 1);
  glColor4f (1, 1, 1, 1);

    if (Ini.MovieSize = 0) then  //HalfSize BG
    begin
      (* half screen + gradient *)
      Rec.Top := 110; // 80
      Rec.Bottom := Rec.Top + 20;
      Rec.Left := 0;
      Rec.Right := 800;

      TexRec.Top := (Rec.Top / 600) * ScreenSing.Tex_Background.TexH;
      TexRec.Bottom := (Rec.Bottom / 600) * ScreenSing.Tex_Background.TexH;
      TexRec.Left := 0;
      TexRec.Right := ScreenSing.Tex_Background.TexW;

      glEnable(GL_TEXTURE_2D);
      glBindTexture(GL_TEXTURE_2D, ScreenSing.Tex_Background.TexNum);
      glEnable(GL_BLEND);
      glBegin(GL_QUADS);
        (* gradient draw *)
        (* top *)
        glColor4f(1, 1, 1, 0);
        glTexCoord2f(TexRec.Right, TexRec.Top);    glVertex2f(Rec.Right, Rec.Top);
        glTexCoord2f(TexRec.Left,  TexRec.Top);    glVertex2f(Rec.Left,  Rec.Top);
        glColor4f(1, 1, 1, 1);
        glTexCoord2f(TexRec.Left,  TexRec.Bottom); glVertex2f(Rec.Left,  Rec.Bottom);
        glTexCoord2f(TexRec.Right, TexRec.Bottom); glVertex2f(Rec.Right, Rec.Bottom);
        (* mid *)
        Rec.Top := Rec.Bottom;
        Rec.Bottom := 490 - 20; // 490 - 20
        TexRec.Top := TexRec.Bottom;
        TexRec.Bottom := (Rec.Bottom / 600) * ScreenSing.Tex_Background.TexH;
        glTexCoord2f(TexRec.Left,  TexRec.Top);    glVertex2f(Rec.Left,  Rec.Top);
        glTexCoord2f(TexRec.Left,  TexRec.Bottom); glVertex2f(Rec.Left,  Rec.Bottom);
        glTexCoord2f(TexRec.Right, TexRec.Bottom); glVertex2f(Rec.Right, Rec.Bottom);
        glTexCoord2f(TexRec.Right, TexRec.Top);    glVertex2f(Rec.Right, Rec.Top);
        (* bottom *)
        Rec.Top := Rec.Bottom;
        Rec.Bottom := 490; // 490
        TexRec.Top := TexRec.Bottom;
        TexRec.Bottom := (Rec.Bottom / 600) * ScreenSing.Tex_Background.TexH;
        glTexCoord2f(TexRec.Right, TexRec.Top);    glVertex2f(Rec.Right, Rec.Top);
        glTexCoord2f(TexRec.Left,  TexRec.Top);    glVertex2f(Rec.Left,  Rec.Top);
        glColor4f(1, 1, 1, 0);
        glTexCoord2f(TexRec.Left,  TexRec.Bottom); glVertex2f(Rec.Left,  Rec.Bottom);
        glTexCoord2f(TexRec.Right, TexRec.Bottom); glVertex2f(Rec.Right, Rec.Bottom);

      glEnd;
      glDisable(GL_TEXTURE_2D);
      glDisable(GL_BLEND);
    end
    else //Full Size BG
    begin
      glEnable(GL_TEXTURE_2D);
      glBindTexture(GL_TEXTURE_2D, ScreenSing.Tex_Background.TexNum);
      //glEnable(GL_BLEND);
      glBegin(GL_QUADS);

        glTexCoord2f(0, 0);   glVertex2f(0,  0);
        glTexCoord2f(0,  ScreenSing.Tex_Background.TexH);   glVertex2f(0,  600);
        glTexCoord2f( ScreenSing.Tex_Background.TexW,  ScreenSing.Tex_Background.TexH);   glVertex2f(800, 600);
        glTexCoord2f( ScreenSing.Tex_Background.TexW, 0);   glVertex2f(800, 0);

      glEnd;
      glDisable(GL_TEXTURE_2D);
      //glDisable(GL_BLEND);
    end;
  end;
end;

procedure SingDrawOscilloscope(X, Y, W, H: real; NrSound: integer);
var
  Pet:    integer;
begin;
//  Log.LogStatus('Oscilloscope', 'SingDraw');
  glColor3f(Skin_OscR, Skin_OscG, Skin_OscB);
  if (ParamStr(1) = '-black') or (ParamStr(1) = '-fsblack') then
    glColor3f(1, 1, 1);

  glBegin(GL_LINE_STRIP);
    glVertex2f(X, -Sound[NrSound].BufferArray[1] / $10000 * H + Y + H/2);
    for Pet := 2 to Sound[NrSound].n div 1 do begin
      glVertex2f(X + (Pet-1) * W / (Sound[NrSound].n - 1),
      -Sound[NrSound].BufferArray[Pet] / $10000 * H + Y + H/2);
    end;
  glEnd;
end;

procedure SingDrawNoteLines(Left, Top, Right: real; Space: integer);
var
  Pet:    integer;
begin
  glEnable(GL_BLEND);
  glColor4f(Skin_P1_LinesR, Skin_P1_LinesG, Skin_P1_LinesB, 0.4);
  glBegin(GL_LINES);
  for Pet := 0 to 9 do begin
    glVertex2f(Left,  Top + Pet * Space);
    glVertex2f(Right, Top + Pet * Space);
  end;
  glEnd;
  glDisable(GL_BLEND);
end;

procedure SingDrawBeatDelimeters(Left, Top, Right: real; NrCzesci: integer);
var
  Pet:    integer;
  TempR:  real;
begin
  TempR := (Right-Left) / (Czesci[NrCzesci].Czesc[Czesci[NrCzesci].Akt].Koniec - Czesci[NrCzesci].Czesc[Czesci[NrCzesci].Akt].StartNote);
  glEnable(GL_BLEND);
  glBegin(GL_LINES);
  for Pet := Czesci[NrCzesci].Czesc[Czesci[NrCzesci].Akt].StartNote to Czesci[NrCzesci].Czesc[Czesci[NrCzesci].Akt].Koniec do begin
    if (Pet mod Czesci[NrCzesci].Resolution) = Czesci[NrCzesci].NotesGAP then
      glColor4f(0, 0, 0, 1)
    else
      glColor4f(0, 0, 0, 0.3);
    glVertex2f(Left + TempR * (Pet - Czesci[NrCzesci].Czesc[Czesci[NrCzesci].Akt].StartNote), Top);
    glVertex2f(Left + TempR * (Pet - Czesci[NrCzesci].Czesc[Czesci[NrCzesci].Akt].StartNote), Top + 135);
  end;
  glEnd;
  glDisable(GL_BLEND);
end;

// draw blank Notebars
procedure SingDrawCzesc(Left, Top, Right: real; NrCzesci: integer; Space: integer);
var
  Rec:      TRecR;
  Pet:      integer;
  TempR:    real;
  R,G,B:    real;

  GoldenStarPos : real;
begin
  glColor3f(1, 1, 1);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  TempR := (Right-Left) / (Czesci[NrCzesci].Czesc[Czesci[NrCzesci].Akt].Koniec - Czesci[NrCzesci].Czesc[Czesci[NrCzesci].Akt].StartNote);
  with Czesci[NrCzesci].Czesc[Czesci[NrCzesci].Akt] do begin
    for Pet := 0 to HighNut do begin
      with Nuta[Pet] do begin
        if not FreeStyle then begin


          if Ini.EffectGolden = 0 then
          // If Golden note Effect of then Change not Color
          begin
            case Wartosc of
              1: glColor4f(1, 1, 1, 0.85);
              2: glColor4f(1, 1, 0.3, 0.85); // no stars, paint yellow -> glColor4f(1, 1, 0.3, 0.85);
            end; // case
          end //Else all Notes same Color
          else
            glColor4f(1, 1, 1, 0.85);

          // lewa czesc  -  left part
          Rec.Left := (Start-Czesci[NrCzesci].Czesc[Czesci[NrCzesci].Akt].StartNote) * TempR + Left + 0.5 + 10*ScreenX;
          Rec.Right := Rec.Left + NotesW;
          Rec.Top := Top - (Ton-BaseNote)*Space/2 - NotesH;
          Rec.Bottom := Rec.Top + 2 * NotesH;
          glBindTexture(GL_TEXTURE_2D, Tex_Left[Color].TexNum);
          glBegin(GL_QUADS);
            glTexCoord2f(0,   0); glVertex2f(Rec.Left,  Rec.Top);
            glTexCoord2f(0,   1); glVertex2f(Rec.Left,  Rec.Bottom);
            glTexCoord2f(7/8, 1); glVertex2f(Rec.Right, Rec.Bottom);
            glTexCoord2f(7/8, 0); glVertex2f(Rec.Right, Rec.Top);
          glEnd;

          //We keep the postion of the top left corner b4 it's overwritten
            GoldenStarPos := Rec.Left;
          //done


         // srodkowa czesc  -  middle part
        Rec.Left := Rec.Right;
        Rec.Right := (Start+Dlugosc-Czesci[NrCzesci].Czesc[Czesci[NrCzesci].Akt].StartNote) * TempR + Left - NotesW - 0.5 + 10*ScreenX;

        glBindTexture(GL_TEXTURE_2D, Tex_Mid[Color].TexNum);
        glBegin(GL_QUADS);
          glTexCoord2f(1/32,  0); glVertex2f(Rec.Left,  Rec.Top);
          glTexCoord2f(1/32,  1); glVertex2f(Rec.Left,  Rec.Bottom);
          glTexCoord2f(31/32, 1); glVertex2f(Rec.Right, Rec.Bottom);
         glTexCoord2f(31/32, 0); glVertex2f(Rec.Right, Rec.Top);
        glEnd;

        // prawa czesc  -  right part
        Rec.Left := Rec.Right;
        Rec.Right := Rec.Right + NotesW;

        glBindTexture(GL_TEXTURE_2D, Tex_Right[Color].TexNum);
        glBegin(GL_QUADS);
          glTexCoord2f(1/8, 0); glVertex2f(Rec.Left,  Rec.Top);
          glTexCoord2f(1/8, 1); glVertex2f(Rec.Left,  Rec.Bottom);
          glTexCoord2f(1,   1); glVertex2f(Rec.Right, Rec.Bottom);
          glTexCoord2f(1,   0); glVertex2f(Rec.Right, Rec.Top);
        glEnd;

          // Golden Star Patch
          if (Wartosc = 2) AND (Ini.EffectGolden=1) then
          begin
            GoldenRec.SaveGoldenStarsRec(GoldenStarPos, Rec.Top, Rec.Right, Rec.Bottom);
          end;

        end; // if not FreeStyle
      end; // with
    end; // for
  end; // with

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;


// draw sung notes
procedure SingDrawPlayerCzesc(X, Y, W: real; NrGracza: integer; Space: integer);
var
  TempR:    real;
  Rec:      TRecR;
  N:        integer;
  R:        real;
  G:        real;
  B:        real;
  A:        real;
  NotesH2:  real;
  begin
//  Log.LogStatus('Player notes', 'SingDraw');

//  if NrGracza = 0 then LoadColor(R, G, B, 'P1Light')
//  else LoadColor(R, G, B, 'P2Light');

//  R := 71/255;
//  G := 175/255;
//  B := 247/255;


  glColor3f(1, 1, 1);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  if Player[NrGracza].IlNut > 0 then begin
    TempR := W / (Czesci[0].Czesc[Czesci[0].Akt].Koniec - Czesci[0].Czesc[Czesci[0].Akt].StartNote);
    for N := 0 to Player[NrGracza].HighNut do begin
      with Player[NrGracza].Nuta[N] do begin
        // lewa czesc
        Rec.Left := X + (Start-Czesci[0].Czesc[Czesci[0].Akt].StartNote) * TempR + 0.5 + 10*ScreenX;
        Rec.Right := Rec.Left + NotesW;


       // Half size Notes Patch
       if Hit then begin
       NotesH2 := NotesH
       end else begin
       NotesH2 := int(NotesH * 0.65);
       end; //if



        //        if True then
        Rec.Top    := Y - (Ton-Czesci[0].Czesc[Czesci[0].Akt].BaseNote)*Space/2 - NotesH2;
        Rec.Bottom := Rec.Top + 2 *NotesH2;

        glColor3f(1, 1, 1);
        glBindTexture(GL_TEXTURE_2D, Tex_Left[NrGracza+1].TexNum);
        glBegin(GL_QUADS);
          glTexCoord2f(1/8, 0); glVertex2f(Rec.Left,  Rec.Top);
          glTexCoord2f(1/8, 1); glVertex2f(Rec.Left,  Rec.Bottom);
          glTexCoord2f(7/8, 1); glVertex2f(Rec.Right, Rec.Bottom);
          glTexCoord2f(7/8, 0); glVertex2f(Rec.Right, Rec.Top);
        glEnd;

        // srodkowa czesc
        Rec.Left := Rec.Right;
        Rec.Right := X + (Start+Dlugosc-Czesci[0].Czesc[Czesci[0].Akt].StartNote) * TempR - NotesW - 0.5  + 10*ScreenX;
        // (nowe)
        if (Start+Dlugosc-1 = Czas.AktBeatD) then
          Rec.Right := Rec.Right - (1-Frac(Czas.MidBeatD)) * TempR;

        if Rec.Right <= Rec.Left then Rec.Right := Rec.Left;


//        glColor3f(R, G, B);
//        glBindTexture(GL_TEXTURE_2D, Tex_MidGray.TexNum);
        glBindTexture(GL_TEXTURE_2D, Tex_Mid[NrGracza+1].TexNum);
        glBegin(GL_QUADS);
          glTexCoord2f(1/8,  0); glVertex2f(Rec.Left,  Rec.Top);
          glTexCoord2f(1/8,  1); glVertex2f(Rec.Left,  Rec.Bottom);
          glTexCoord2f(7/8, 1); glVertex2f(Rec.Right, Rec.Bottom);
          glTexCoord2f(7/8, 0); glVertex2f(Rec.Right, Rec.Top);
        glEnd;
        glColor3f(1, 1, 1);

        // prawa czesc
        Rec.Left := Rec.Right;
        Rec.Right := Rec.Right + NotesW;

        glBindTexture(GL_TEXTURE_2D, Tex_Right[NrGracza+1].TexNum);
        glBegin(GL_QUADS);
          glTexCoord2f(1/8, 0); glVertex2f(Rec.Left,  Rec.Top);
          glTexCoord2f(1/8, 1); glVertex2f(Rec.Left,  Rec.Bottom);
          glTexCoord2f(1,   1); glVertex2f(Rec.Right, Rec.Bottom);
          glTexCoord2f(1,   0); glVertex2f(Rec.Right, Rec.Top);
        glEnd;



        //Rec.Right := X + (Start+Dlugosc-Czesci[0].Czesc[Czesci[0].Akt].StartNote) * TempR - NotesW - 0.5  + 10*ScreenX;
        //if (Start+Dlugosc-1 = Czas.AktBeatD) then
        if Perfect and (Ini.EffectPerfect=1) then begin
//          A := sqrt((1+sin(Music.Position * 3))/2);
          A := 1 - 2*(Czas.Teraz - GetTimeFromBeat(Start+Dlugosc));
          if not (Start+Dlugosc-1 = Czas.AktBeatD) then

            //Star animation counter
            //inc(Starfr);
            //Starfr := Starfr mod 128;

            GoldenRec.SavePerfectNotePos(Rec.Left, Rec.Top);
          {  SingDrawStar(Rec.Left+2, Rec.Top+4, A);}
        end;

        // detekt
{        Rec.Left := Round((Detekt-Czesci.Czesc[Czesci.Akt].Start) * TempR) + 130;
        glColor3f(1, 0.2, 0.2);
        glDisable(GL_BLEND);
        glDisable(GL_TEXTURE_2D);
        glBegin(GL_QUADS);
          glVertex(Rec.Left,   Rec.Top-5);
          glVertex(Rec.Left,   Rec.Bottom+5);
          glVertex(Rec.Left+1, Rec.Bottom+5);
          glVertex(Rec.Left+1, Rec.Top-5);
        glEnd;
        glColor3f(1, 1, 1);
        glEnable(GL_BLEND);
        glEnable(GL_TEXTURE_2D);}

        // detekt + FFT length
{        Rec.Right := (Detekt-Czesci.Czesc[Czesci.Akt].Start) * TempR + 130;
        // TempR = dlugosc 1 kostki
        // 60 * 4 / BPM  = czas w sekundach na 1 kostke, np. 0,4s
        // 4096 / 44100 = czas jednego sampla FFT, np. 0,1s
        // ile to ma kostek? np. 0.25
        // (4096 / 44100) / (60 * 4 / BPM), np. 0,1s / 0,4s = 0.25
        // * TempR = dlugosc sampla FFT
        Rec.Left := Rec.Right - (Sound.n / 44100) / (60 * 4 / Muzyka.BPM) * TempR;

        glColor3f(1, 0.2, 0.2);
        glVertex(Rec.Left,   Rec.Top-4);
        glVertex(Rec.Left,   Rec.Bottom+4);
        glVertex(Rec.Right, Rec.Bottom+4);
        glVertex(Rec.Right, Rec.Top-4);}

      end; // with
    end; // for
    // eigentlich brauchen wir hier einen vergleich, um festzustellen, ob wir mit
    // singen schon weiter wären, als bei Rec.Right, _auch, wenn nicht gesungen wird_

    // passing on NrGracza... hope this is really something like the player-number, not only
    // some kind of weird index into a colour-table

    if (Ini.EffectGolden=1) then
    GoldenRec.GoldenNoteTwinkle(Rec.Top,Rec.Bottom,Rec.Right, NrGracza);
  end; // if
end;

//draw Note glow
procedure SingDrawPlayerBGCzesc(Left, Top, Right: real; NrCzesci, NrGracza: integer; Space: integer);
var
  Rec:      TRecR;
  Pet:      integer;
  TempR:    real;
  R,G,B:    real;
  X1, X2, X3, X4: real;
  W, H:     real;
begin
  if (Player[NrGracza].ScoreTotalI >= 0) then begin
  glColor4f(1, 1, 1, sqrt((1+sin(Music.Position * 3))/4)/ 2 + 0.5 );
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  TempR := (Right-Left) / (Czesci[NrCzesci].Czesc[Czesci[NrCzesci].Akt].Koniec - Czesci[NrCzesci].Czesc[Czesci[NrCzesci].Akt].StartNote);
  with Czesci[NrCzesci].Czesc[Czesci[NrCzesci].Akt] do begin
    for Pet := 0 to HighNut do begin
      with Nuta[Pet] do begin
        if not FreeStyle then begin
          // begin: 14, 20
          // easy: 6, 11
          W := NotesW * 2 + 2;
          H := NotesH * 1.5 + 3.5;

          X2 := (Start-Czesci[NrCzesci].Czesc[Czesci[NrCzesci].Akt].StartNote) * TempR + Left + 0.5 + 10*ScreenX + 4; // wciecie
          X1 := X2-W;

          X3 := (Start+Dlugosc-Czesci[NrCzesci].Czesc[Czesci[NrCzesci].Akt].StartNote) * TempR + Left - 0.5 + 10*ScreenX - 4; // wciecie
          X4 := X3+W;

          // left
          Rec.Left := X1;
          Rec.Right := X2;
          Rec.Top := Top - (Ton-BaseNote)*Space/2 - H;
          Rec.Bottom := Rec.Top + 2 * H;

          glBindTexture(GL_TEXTURE_2D, Tex_BG_Left[NrGracza+1].TexNum);
          glBegin(GL_QUADS);
            glTexCoord2f(0,   0); glVertex2f(Rec.Left,  Rec.Top);
            glTexCoord2f(0,   1); glVertex2f(Rec.Left,  Rec.Bottom);
            glTexCoord2f(7/8, 1); glVertex2f(Rec.Right, Rec.Bottom);
            glTexCoord2f(7/8, 0); glVertex2f(Rec.Right, Rec.Top);
          glEnd;


        // srodkowa czesc
        Rec.Left := X2;
        Rec.Right := X3;

        glBindTexture(GL_TEXTURE_2D, Tex_BG_Mid[NrGracza+1].TexNum);
        glBegin(GL_QUADS);
          glTexCoord2f(1/32,  0); glVertex2f(Rec.Left,  Rec.Top);
          glTexCoord2f(1/32,  1); glVertex2f(Rec.Left,  Rec.Bottom);
          glTexCoord2f(31/32, 1); glVertex2f(Rec.Right, Rec.Bottom);
          glTexCoord2f(31/32, 0); glVertex2f(Rec.Right, Rec.Top);
        glEnd;

        // prawa czesc
        Rec.Left := X3;
        Rec.Right := X4;

        glBindTexture(GL_TEXTURE_2D, Tex_BG_Right[NrGracza+1].TexNum);
        glBegin(GL_QUADS);
          glTexCoord2f(1/8, 0); glVertex2f(Rec.Left,  Rec.Top);
          glTexCoord2f(1/8, 1); glVertex2f(Rec.Left,  Rec.Bottom);
          glTexCoord2f(1,   1); glVertex2f(Rec.Right, Rec.Bottom);
          glTexCoord2f(1,   0); glVertex2f(Rec.Right, Rec.Top);
        glEnd;
        end; // if not FreeStyle
      end; // with
    end; // for
  end; // with

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
  end;
end;

{not used anymore tough we have UGraphicClasses
procedure SingDrawStar(X, Y, A: real);
var
  TempR:    real;
  W, H:     real;
  Starframe: real;
  begin
  W := 32;
  H := 32;

// Golden Star Patch
//  case Z of
//    1: glColor4f(1, 1, 1, A);
//    2: glColor4f(1, 1, 0.3, A);
//  end; // case

  glColor4f(1, 1, 1, A);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindTexture(GL_TEXTURE_2D, Tex_Note_Star.TexNum);

  Starframe := 15 - ((GetTickCount div 33) mod 16);

  glBegin(GL_QUADS);
    glTexCoord2f((1/16) * Starframe, 0); glVertex2f(X-W, Y-H);
    glTexCoord2f((1/16) * Starframe + (1/16), 0); glVertex2f(X-W, Y+H);
    glTexCoord2f((1/16) * Starframe + (1/16), 1); glVertex2f(X+W, Y+H);
    glTexCoord2f((1/16) * Starframe, 1); glVertex2f(X+W, Y-H);
  glEnd;
end;
}

{not used anymore tough we have UGraphicClasses
procedure SingGoldenStar(X, Y, A: real);
var
  TempR:    real;
  W, H:     real;
  StarfrG2: real;
  begin
  W := 16;
  H := 16;
  glColor4f(1, 1, 0.3, A);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindTexture(GL_TEXTURE_2D, Tex_Note_Star.TexNum);
  StarfrG2 := 15 - ((GetTickCount div 67) mod 16);
  glBegin(GL_QUADS);
    //x1
    glTexCoord2f((1/16) * StarfrG2, 0); glVertex2f(X-W, Y-H);
    glTexCoord2f((1/16) * StarfrG2 + (1/16), 0); glVertex2f(X-W, Y+H);
    glTexCoord2f((1/16) * StarfrG2 + (1/16), 1); glVertex2f(X+W, Y+H);
    glTexCoord2f((1/16) * StarfrG2, 1); glVertex2f(X+W, Y-H);
  glEnd;
end;
}

procedure SingDraw;
var
  Pet:      integer;
  Pet2:     integer;
  TempR:    real;
  Rec:      TRecR;
  TexRec:   TRecR;
  NR:       TRecR;
  FS:       real;
  BarFrom:  integer;
  BarAlpha: real;
  BarWspol: real;
  TempCol:  real;
  Tekst:    string;
  LyricTemp:  string;
  PetCz:    integer;



  //SingBar Mod
  A: Integer;
  E: Integer;
  I: Integer;
  //end Singbar Mod



begin
  // positions
  if Ini.SingWindow = 0 then begin
    NR.Left := 120;
  end else begin
    NR.Left := 20;
  end;
  NR.Right := 780;

  NR.Width := NR.Right - NR.Left;
  NR.WMid := NR.Width / 2;
  NR.Mid := NR.Left + NR.WMid;

  // background  //BG Fullsize Mod
  //SingDrawBackground;

  // time bar
//  Log.LogStatus('Time Bar', 'SingDraw');
  glBegin(GL_QUADS);
    glColor3f(0.9, 0.9, 0.9);
    glVertex2f(140 + 10*ScreenX, 21);
    glVertex2f(140 + 10*ScreenX, 29);
    glVertex2f(330 + 10*ScreenX, 29);
    glVertex2f(330 + 10*ScreenX, 21);
    glColor3f(Skin_TimeR, Skin_TimeG, Skin_TimeB);
    glVertex2f(140 + 10*ScreenX,                                   21);
    glVertex2f(140 + 10*ScreenX,                                   29);
    glVertex2f(140 + 10*ScreenX+(330-140)*(Czas.Teraz/Czas.Razem), 29);
    glVertex2f(140 + 10*ScreenX+(330-140)*(Czas.Teraz/Czas.Razem), 21);
  glEnd;

  // rysuje paski pod nutami
  if PlayersPlay = 1 then
    SingDrawNoteLines(Nr.Left + 10*ScreenX, Skin_P2_NotesB - 105, Nr.Right + 10*ScreenX, 15);
  if (PlayersPlay = 2) or (PlayersPlay = 4) then begin
    SingDrawNoteLines(Nr.Left + 10*ScreenX, Skin_P1_NotesB - 105, Nr.Right + 10*ScreenX, 15);
    SingDrawNoteLines(Nr.Left + 10*ScreenX, Skin_P2_NotesB - 105, Nr.Right + 10*ScreenX, 15);
  end;

  if (PlayersPlay = 3) or (PlayersPlay = 6) then begin
    SingDrawNoteLines(Nr.Left + 10*ScreenX, 120, Nr.Right + 10*ScreenX, 12);
    SingDrawNoteLines(Nr.Left + 10*ScreenX, 245, Nr.Right + 10*ScreenX, 12);
    SingDrawNoteLines(Nr.Left + 10*ScreenX, 370, Nr.Right + 10*ScreenX, 12);
  end;

  // rysuje tekst - new Lyric engine
  ScreenSing.LyricMain.Draw;
  ScreenSing.LyricSub.Draw;

  // rysuje pasek, podpowiadajacy poczatek spiwania w scenie
  FS := 1.3;
  BarFrom := Czesci[0].Czesc[Czesci[0].Akt].StartNote - Czesci[0].Czesc[Czesci[0].Akt].Start;
  if BarFrom > 40 then BarFrom := 40;
  if (Czesci[0].Czesc[Czesci[0].Akt].StartNote - Czesci[0].Czesc[Czesci[0].Akt].Start > 8) and  // dluga przerwa //16->12 for more help bars and then 12->8 for even more
    (Czesci[0].Czesc[Czesci[0].Akt].StartNote - Czas.MidBeat > 0) and                     // przed tekstem
    (Czesci[0].Czesc[Czesci[0].Akt].StartNote - Czas.MidBeat < 40) then begin            // ale nie za wczesnie
    BarWspol := (Czas.MidBeat - (Czesci[0].Czesc[Czesci[0].Akt].StartNote - BarFrom)) / BarFrom;
    Rec.Left := NR.Left + BarWspol *
//      (NR.WMid - Czesci[0].Czesc[Czesci[0].Akt].LyricWidth / 2 * FS - 50);
      (ScreenSing.LyricMain.ClientX - NR.Left - 50) + 10*ScreenX;
    Rec.Right := Rec.Left + 50;
    Rec.Top := Skin_LyricsT + 3;
    Rec.Bottom := Rec.Top + 33;//SingScreen.LyricMain.Size * 3;

    // zapalanie
    BarAlpha := (BarWspol*10) * 0.5;
    if BarAlpha > 0.5 then BarAlpha := 0.5;

    // gaszenie
    if BarWspol > 0.95 then BarAlpha := 0.5 * (1 - (BarWspol - 0.95) * 20);
    //Change fuer Crazy Joker
    glEnable(GL_BLEND);
    glBegin(GL_QUADS);
      glColor4f(26/255, 165/255, 220/255, 0);
      glVertex2f(Rec.Left, Rec.Top);
      glVertex2f(Rec.Left, Rec.Bottom);
      glColor4f(26/255, 165/255, 220/255, BarAlpha);
      glVertex2f(Rec.Right, Rec.Bottom);
      glVertex2f(Rec.Right, Rec.Top);
    glEnd;
    glDisable(GL_BLEND);
  end;

  // oscilloscope
  if Ini.Oscilloscope = 1 then begin
    if PlayersPlay = 1 then
      SingDrawOscilloscope(190 + 10*ScreenX, 55, 180, 40, 0);

    if PlayersPlay = 2 then begin
      SingDrawOscilloscope(190 + 10*ScreenX, 55, 180, 40, 0);
      SingDrawOscilloscope(425 + 10*ScreenX, 55, 180, 40, 1);
    end;

    if PlayersPlay = 4 then begin
      if ScreenAct = 1 then begin
        SingDrawOscilloscope(190 + 10*ScreenX, 55, 180, 40, 0);
        SingDrawOscilloscope(425 + 10*ScreenX, 55, 180, 40, 1);
      end;
      if ScreenAct = 2 then begin
        SingDrawOscilloscope(190 + 10*ScreenX, 55, 180, 40, 2);
        SingDrawOscilloscope(425 + 10*ScreenX, 55, 180, 40, 3);
      end;
    end;

    if PlayersPlay = 3 then begin
      SingDrawOscilloscope(75 + 10*ScreenX, 95, 100, 20, 0);
      SingDrawOscilloscope(370 + 10*ScreenX, 95, 100, 20, 1);
      SingDrawOscilloscope(670 + 10*ScreenX, 95, 100, 20, 2);
    end;

    if PlayersPlay = 6 then begin
      if ScreenAct = 1 then begin
        SingDrawOscilloscope( 75 + 10*ScreenX, 95, 100, 20, 0);
        SingDrawOscilloscope(370 + 10*ScreenX, 95, 100, 20, 1);
        SingDrawOscilloscope(670 + 10*ScreenX, 95, 100, 20, 2);
      end;
      if ScreenAct = 2 then begin
        SingDrawOscilloscope( 75 + 10*ScreenX, 95, 100, 20, 3);
        SingDrawOscilloscope(370 + 10*ScreenX, 95, 100, 20, 4);
        SingDrawOscilloscope(670 + 10*ScreenX, 95, 100, 20, 5);
      end;
    end;

  end

  //SingBar Mod
  else if Ini.Oscilloscope = 2 then begin
    A := GetTickCount div 33;
    if A <> Tickold then begin
      Tickold := A;
      for E := 0 to (PlayersPlay - 1) do begin //Set new Pos + Alpha
        I := Player[E].ScorePercentTarget - Player[E].ScorePercent;
        if I > 0 then Inc(Player[E].ScorePercent)
        else if I < 0 then Dec(Player[E].ScorePercent);
      end; //for
    end; //if
    if PlayersPlay = 1 then begin
      SingDrawSingbar( 75 + 10*ScreenX, 95, 100, 8, Player[0].ScorePercent);
   end;
    if PlayersPlay = 2 then begin
      SingDrawSingbar( 75 + 10*ScreenX, 95, 100, 8, Player[0].ScorePercent);
      SingDrawSingbar(620 + 10*ScreenX, 95, 100, 8, Player[1].ScorePercent);
   end;
    if PlayersPlay = 3 then begin
      SingDrawSingbar( 75 + 10*ScreenX, 95, 100, 8, Player[0].ScorePercent);
      SingDrawSingbar(370 + 10*ScreenX, 95, 100, 8, Player[1].ScorePercent);
      SingDrawSingbar(670 + 10*ScreenX, 95, 100, 8, Player[2].ScorePercent);
    end;
    if PlayersPlay = 4 then begin
      if ScreenAct = 1 then begin
      SingDrawSingbar( 75 + 10*ScreenX, 95, 100, 8, Player[0].ScorePercent);
      SingDrawSingbar(620 + 10*ScreenX, 95, 100, 8, Player[1].ScorePercent);
      end;
      if ScreenAct = 2 then begin
      SingDrawSingbar( 75 + 10*ScreenX, 95, 100, 8, Player[2].ScorePercent);
      SingDrawSingbar(620 + 10*ScreenX, 95, 100, 8, Player[3].ScorePercent);
      end;
    end;
    if PlayersPlay = 6 then begin
      if ScreenAct = 1 then begin
        SingDrawSingbar( 75 + 10*ScreenX, 95, 100, 8, Player[0].ScorePercent);
        SingDrawSingbar(370 + 10*ScreenX, 95, 100, 8, Player[1].ScorePercent);
        SingDrawSingbar(670 + 10*ScreenX, 95, 100, 8, Player[2].ScorePercent);
      end;
      if ScreenAct = 2 then begin
        SingDrawSingbar( 75 + 10*ScreenX, 95, 100, 8, Player[3].ScorePercent);
        SingDrawSingbar(370 + 10*ScreenX, 95, 100, 8, Player[4].ScorePercent);
        SingDrawSingbar(670 + 10*ScreenX, 95, 100, 8, Player[5].ScorePercent);
      end;
    end;
  end;
  //end Singbar Mod

  //PhrasenBonus - Line Bonus Mod
  if Ini.LineBonus > 0 then begin
    A := GetTickCount div 33;
    if (A <> Tickold2) AND (Player[0].LineBonus_Visible) then begin
      Tickold2 := A;
      for E := 0 to (PlayersPlay - 1) do begin
          //Change Alpha
          Player[E].LineBonus_Alpha := Player[E].LineBonus_Alpha - 0.02;

          if Player[E].LineBonus_Alpha <= 0 then
            Player[E].LineBonus_Visible := False
          else
          begin
          //Change Position
          if (Player[E].LineBonus_PosX < Player[E].LineBonus_TargetX) then
            Inc(Player[E].LineBonus_PosX,1)
           else if (Player[E].LineBonus_PosX > Player[E].LineBonus_TargetX) then
            Dec(Player[E].LineBonus_PosX,1);

          if (Player[E].LineBonus_PosY < Player[E].LineBonus_TargetY) then
            Inc(Player[E].LineBonus_PosY,1)
          else if (Player[E].LineBonus_PosY > Player[E].LineBonus_TargetY) then
            Dec(Player[E].LineBonus_PosY,1);

          end;
      end;
    end; //if

    if PlayersPlay = 1 then begin
      SingDrawLineBonus( Player[0].LineBonus_PosX, Player[0].LineBonus_PosY, Player[0].LineBonus_Color, Player[0].LineBonus_Alpha, Player[0].LineBonus_Text);
   end
   else if PlayersPlay = 2 then begin
      SingDrawLineBonus( Player[0].LineBonus_PosX, Player[0].LineBonus_PosY, Player[0].LineBonus_Color, Player[0].LineBonus_Alpha, Player[0].LineBonus_Text);
      SingDrawLineBonus( Player[1].LineBonus_PosX, Player[1].LineBonus_PosY, Player[1].LineBonus_Color, Player[1].LineBonus_Alpha, Player[1].LineBonus_Text);
   end
   else if PlayersPlay = 3 then begin
      SingDrawLineBonus( Player[0].LineBonus_PosX, Player[0].LineBonus_PosY, Player[0].LineBonus_Color, Player[0].LineBonus_Alpha, Player[0].LineBonus_Text);
      SingDrawLineBonus( Player[1].LineBonus_PosX, Player[1].LineBonus_PosY, Player[1].LineBonus_Color, Player[1].LineBonus_Alpha, Player[1].LineBonus_Text);
      SingDrawLineBonus( Player[2].LineBonus_PosX, Player[2].LineBonus_PosY, Player[2].LineBonus_Color, Player[2].LineBonus_Alpha, Player[2].LineBonus_Text);
    end
   else if PlayersPlay = 4 then begin
      if ScreenAct = 1 then begin
      SingDrawLineBonus( Player[0].LineBonus_PosX, Player[0].LineBonus_PosY, Player[0].LineBonus_Color, Player[0].LineBonus_Alpha, Player[0].LineBonus_Text);
      SingDrawLineBonus( Player[1].LineBonus_PosX, Player[1].LineBonus_PosY, Player[1].LineBonus_Color, Player[1].LineBonus_Alpha, Player[1].LineBonus_Text);
      end;
      if ScreenAct = 2 then begin
      SingDrawLineBonus( Player[2].LineBonus_PosX, Player[2].LineBonus_PosY, Player[2].LineBonus_Color, Player[2].LineBonus_Alpha, Player[2].LineBonus_Text);
      SingDrawLineBonus( Player[3].LineBonus_PosX, Player[3].LineBonus_PosY, Player[3].LineBonus_Color, Player[3].LineBonus_Alpha, Player[3].LineBonus_Text);
      end;
    end;
    if PlayersPlay = 6 then begin
      if ScreenAct = 1 then begin
        SingDrawLineBonus( Player[0].LineBonus_PosX, Player[0].LineBonus_PosY, Player[0].LineBonus_Color, Player[0].LineBonus_Alpha, Player[0].LineBonus_Text);
        SingDrawLineBonus( Player[1].LineBonus_PosX, Player[1].LineBonus_PosY, Player[1].LineBonus_Color, Player[1].LineBonus_Alpha, Player[1].LineBonus_Text);
        SingDrawLineBonus( Player[2].LineBonus_PosX, Player[2].LineBonus_PosY, Player[2].LineBonus_Color, Player[2].LineBonus_Alpha, Player[2].LineBonus_Text);
      end;
      if ScreenAct = 2 then begin
        SingDrawLineBonus( Player[3].LineBonus_PosX, Player[3].LineBonus_PosY, Player[3].LineBonus_Color, Player[3].LineBonus_Alpha, Player[3].LineBonus_Text);
        SingDrawLineBonus( Player[4].LineBonus_PosX, Player[4].LineBonus_PosY, Player[4].LineBonus_Color, Player[4].LineBonus_Alpha, Player[4].LineBonus_Text);
        SingDrawLineBonus( Player[5].LineBonus_PosX, Player[5].LineBonus_PosY, Player[5].LineBonus_Color, Player[5].LineBonus_Alpha, Player[5].LineBonus_Text);
      end;
    end;
  end;
  //PhrasenBonus - Line Bonus Mod End


  // rysuje paski
//  Log.LogStatus('Original notes', 'SingDraw');
  case Ini.Difficulty of
    0:
      begin
        NotesH := 11; // 9
        NotesW := 6; // 5
      end;
    1:
      begin
        NotesH := 8; // 7
        NotesW := 4; // 4
      end;
    2:
      begin
        NotesH := 5;
        NotesW := 3;
      end;
  end;

  if PlayersPlay = 1 then begin
    SingDrawPlayerBGCzesc(NR.Left + 20, Skin_P2_NotesB, NR.Right - 20, 0, 0, 15);
    SingDrawCzesc(NR.Left + 20, Skin_P2_NotesB, NR.Right - 20, 0, 15);
    SingDrawPlayerCzesc(Nr.Left + 20, Skin_P2_NotesB, Nr.Width - 40, 0, 15);
  end;

  if (PlayersPlay = 2)  then begin
    SingDrawPlayerBGCzesc(Nr.Left + 20, Skin_P1_NotesB, Nr.Right - 20, 0, 0, 15);
    SingDrawPlayerBGCzesc(Nr.Left + 20, Skin_P2_NotesB, Nr.Right - 20, 0, 1, 15);

    SingDrawCzesc(NR.Left + 20, Skin_P1_NotesB, NR.Right - 20, 0, 15);
    SingDrawCzesc(NR.Left + 20, Skin_P2_NotesB, NR.Right - 20, 0, 15);

    SingDrawPlayerCzesc(Nr.Left + 20, Skin_P1_NotesB, Nr.Width - 40, 0, 15);
    SingDrawPlayerCzesc(Nr.Left + 20, Skin_P2_NotesB, Nr.Width - 40, 1, 15);
  end;

  if PlayersPlay = 3 then begin
    NotesW := NotesW * 0.8;
    NotesH := NotesH * 0.8;

    SingDrawPlayerBGCzesc(Nr.Left + 20, 120+95, Nr.Right - 20, 0, 0, 12);
    SingDrawPlayerBGCzesc(Nr.Left + 20, 245+95, Nr.Right - 20, 0, 1, 12);
    SingDrawPlayerBGCzesc(Nr.Left + 20, 370+95, Nr.Right - 20, 0, 2, 12);

    SingDrawCzesc(NR.Left + 20, 120+95, NR.Right - 20, 0, 12);
    SingDrawCzesc(NR.Left + 20, 245+95, NR.Right - 20, 0, 12);
    SingDrawCzesc(NR.Left + 20, 370+95, NR.Right - 20, 0, 12);

    SingDrawPlayerCzesc(Nr.Left + 20, 120+95, Nr.Width - 40, 0, 12);
    SingDrawPlayerCzesc(Nr.Left + 20, 245+95, Nr.Width - 40, 1, 12);
    SingDrawPlayerCzesc(Nr.Left + 20, 370+95, Nr.Width - 40, 2, 12);
  end;

  if PlayersPlay = 4 then begin
    if ScreenAct = 1 then begin
      SingDrawPlayerBGCzesc(Nr.Left + 20, Skin_P1_NotesB, Nr.Right - 20, 0, 0, 15);
      SingDrawPlayerBGCzesc(Nr.Left + 20, Skin_P2_NotesB, Nr.Right - 20, 0, 1, 15);
    end;
    if ScreenAct = 2 then begin
      SingDrawPlayerBGCzesc(Nr.Left + 20, Skin_P1_NotesB, Nr.Right - 20, 0, 2, 15);
      SingDrawPlayerBGCzesc(Nr.Left + 20, Skin_P2_NotesB, Nr.Right - 20, 0, 3, 15);
    end;

    SingDrawCzesc(NR.Left + 20, Skin_P1_NotesB, NR.Right - 20, 0, 15);
    SingDrawCzesc(NR.Left + 20, Skin_P2_NotesB, NR.Right - 20, 0, 15);

    if ScreenAct = 1 then begin
      SingDrawPlayerCzesc(Nr.Left + 20, Skin_P1_NotesB, Nr.Width - 40, 0, 15);
      SingDrawPlayerCzesc(Nr.Left + 20, Skin_P2_NotesB, Nr.Width - 40, 1, 15);
    end;
    if ScreenAct = 2 then begin
      SingDrawPlayerCzesc(Nr.Left + 20, Skin_P1_NotesB, Nr.Width - 40, 2, 15);
      SingDrawPlayerCzesc(Nr.Left + 20, Skin_P2_NotesB, Nr.Width - 40, 3, 15);
    end;
  end;

  if PlayersPlay = 6 then begin
    NotesW := NotesW * 0.8;
    NotesH := NotesH * 0.8;

    if ScreenAct = 1 then begin
      SingDrawPlayerBGCzesc(Nr.Left + 20, 120+95, Nr.Right - 20, 0, 0, 12);
      SingDrawPlayerBGCzesc(Nr.Left + 20, 245+95, Nr.Right - 20, 0, 1, 12);
      SingDrawPlayerBGCzesc(Nr.Left + 20, 370+95, Nr.Right - 20, 0, 2, 12);
    end;
    if ScreenAct = 2 then begin
      SingDrawPlayerBGCzesc(Nr.Left + 20, 120+95, Nr.Right - 20, 0, 3, 12);
      SingDrawPlayerBGCzesc(Nr.Left + 20, 245+95, Nr.Right - 20, 0, 4, 12);
      SingDrawPlayerBGCzesc(Nr.Left + 20, 370+95, Nr.Right - 20, 0, 5, 12);
    end;

    SingDrawCzesc(NR.Left + 20, 120+95, NR.Right - 20, 0, 12);
    SingDrawCzesc(NR.Left + 20, 245+95, NR.Right - 20, 0, 12);
    SingDrawCzesc(NR.Left + 20, 370+95, NR.Right - 20, 0, 12);

    if ScreenAct = 1 then begin
      SingDrawPlayerCzesc(Nr.Left + 20, 120+95, Nr.Width - 40, 0, 12);
      SingDrawPlayerCzesc(Nr.Left + 20, 245+95, Nr.Width - 40, 1, 12);
      SingDrawPlayerCzesc(Nr.Left + 20, 370+95, Nr.Width - 40, 2, 12);
    end;
    if ScreenAct = 2 then begin
      SingDrawPlayerCzesc(Nr.Left + 20, 120+95, Nr.Width - 40, 3, 12);
      SingDrawPlayerCzesc(Nr.Left + 20, 245+95, Nr.Width - 40, 4, 12);
      SingDrawPlayerCzesc(Nr.Left + 20, 370+95, Nr.Width - 40, 5, 12);
    end;
  end;

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure SingModiDraw (PlayerInfo: TPlayerInfo);
var
  Pet:      integer;
  Pet2:     integer;
  TempR:    real;
  Rec:      TRecR;
  TexRec:   TRecR;
  NR:       TRecR;
  FS:       real;
  BarFrom:  integer;
  BarAlpha: real;
  BarWspol: real;
  TempCol:  real;
  Tekst:    string;
  LyricTemp:  string;
  PetCz:    integer;



  //SingBar Mod
  A: Integer;
  E: Integer;
  I: Integer;
  //end Singbar Mod



begin
  // positions
  if Ini.SingWindow = 0 then begin
    NR.Left := 120;
  end else begin
    NR.Left := 20;
  end;
  NR.Right := 780;

  NR.Width := NR.Right - NR.Left;
  NR.WMid := NR.Width / 2;
  NR.Mid := NR.Left + NR.WMid;

  // background  //BG Fullsize Mod
  //SingDrawBackground;

  // time bar
//  Log.LogStatus('Time Bar', 'SingDraw');
  glBegin(GL_QUADS);
    glColor3f(0.9, 0.9, 0.9);
    glVertex2f(140 + 10*ScreenX, 21);
    glVertex2f(140 + 10*ScreenX, 29);
    glVertex2f(330 + 10*ScreenX, 29);
    glVertex2f(330 + 10*ScreenX, 21);
    glColor3f(Skin_TimeR, Skin_TimeG, Skin_TimeB);
    glVertex2f(140 + 10*ScreenX,                                   21);
    glVertex2f(140 + 10*ScreenX,                                   29);
    glVertex2f(140 + 10*ScreenX+(330-140)*(Czas.Teraz/Czas.Razem), 29);
    glVertex2f(140 + 10*ScreenX+(330-140)*(Czas.Teraz/Czas.Razem), 21);
  glEnd;

  if DLLMan.Selected.ShowNotes then
  begin
    // rysuje paski pod nutami
    if PlayersPlay = 1 then
      SingDrawNoteLines(Nr.Left + 10*ScreenX, Skin_P2_NotesB - 105, Nr.Right + 10*ScreenX, 15);
    if (PlayersPlay = 2) or (PlayersPlay = 4) then begin
      SingDrawNoteLines(Nr.Left + 10*ScreenX, Skin_P1_NotesB - 105, Nr.Right + 10*ScreenX, 15);
      SingDrawNoteLines(Nr.Left + 10*ScreenX, Skin_P2_NotesB - 105, Nr.Right + 10*ScreenX, 15);
    end;

    if (PlayersPlay = 3) or (PlayersPlay = 6) then begin
      SingDrawNoteLines(Nr.Left + 10*ScreenX, 120, Nr.Right + 10*ScreenX, 12);
      SingDrawNoteLines(Nr.Left + 10*ScreenX, 245, Nr.Right + 10*ScreenX, 12);
      SingDrawNoteLines(Nr.Left + 10*ScreenX, 370, Nr.Right + 10*ScreenX, 12);
    end;
  end;

    // rysuje tekst - new Lyric engine
    ScreenSingModi.LyricMain.Draw;
    ScreenSingModi.LyricSub.Draw;

    // rysuje pasek, podpowiadajacy poczatek spiwania w scenie
    FS := 1.3;
    BarFrom := Czesci[0].Czesc[Czesci[0].Akt].StartNote - Czesci[0].Czesc[Czesci[0].Akt].Start;
    if BarFrom > 40 then BarFrom := 40;
    if (Czesci[0].Czesc[Czesci[0].Akt].StartNote - Czesci[0].Czesc[Czesci[0].Akt].Start > 8) and  // dluga przerwa //16->12 for more help bars and then 12->8 for even more
      (Czesci[0].Czesc[Czesci[0].Akt].StartNote - Czas.MidBeat > 0) and                     // przed tekstem
      (Czesci[0].Czesc[Czesci[0].Akt].StartNote - Czas.MidBeat < 40) then begin            // ale nie za wczesnie
      BarWspol := (Czas.MidBeat - (Czesci[0].Czesc[Czesci[0].Akt].StartNote - BarFrom)) / BarFrom;
      Rec.Left := NR.Left + BarWspol *
  //      (NR.WMid - Czesci[0].Czesc[Czesci[0].Akt].LyricWidth / 2 * FS - 50);
        (ScreenSingModi.LyricMain.ClientX - NR.Left - 50) + 10*ScreenX;
      Rec.Right := Rec.Left + 50;
      Rec.Top := Skin_LyricsT + 3;
      Rec.Bottom := Rec.Top + 33;//SingScreen.LyricMain.Size * 3;

      // zapalanie
      BarAlpha := (BarWspol*10) * 0.5;
      if BarAlpha > 0.5 then BarAlpha := 0.5;

      // gaszenie
      if BarWspol > 0.95 then BarAlpha := 0.5 * (1 - (BarWspol - 0.95) * 20);
      //Change fuer Crazy Joker
      glEnable(GL_BLEND);
      glBegin(GL_QUADS);
        glColor4f(26/255, 165/255, 220/255, 0);
        glVertex2f(Rec.Left, Rec.Top);
        glVertex2f(Rec.Left, Rec.Bottom);
        glColor4f(26/255, 165/255, 220/255, BarAlpha);
        glVertex2f(Rec.Right, Rec.Bottom);
        glVertex2f(Rec.Right, Rec.Top);
      glEnd;
      glDisable(GL_BLEND);
    end;

  // oscilloscope
  if (((Ini.Oscilloscope = 1) AND (DLLMan.Selected.ShowRateBar_O)) AND (NOT DLLMan.Selected.ShowRateBar)) then begin
    if PlayersPlay = 1 then
      if PlayerInfo.Playerinfo[0].Enabled then
        SingDrawOscilloscope(190 + 10*ScreenX, 55, 180, 40, 0);

    if PlayersPlay = 2 then begin
      if PlayerInfo.Playerinfo[0].Enabled then
        SingDrawOscilloscope(190 + 10*ScreenX, 55, 180, 40, 0);
      if PlayerInfo.Playerinfo[1].Enabled then
        SingDrawOscilloscope(425 + 10*ScreenX, 55, 180, 40, 1);
    end;

    if PlayersPlay = 4 then begin
      if ScreenAct = 1 then begin
        if PlayerInfo.Playerinfo[0].Enabled then
          SingDrawOscilloscope(190 + 10*ScreenX, 55, 180, 40, 0);
        if PlayerInfo.Playerinfo[1].Enabled then
          SingDrawOscilloscope(425 + 10*ScreenX, 55, 180, 40, 1);
      end;
      if ScreenAct = 2 then begin
        if PlayerInfo.Playerinfo[2].Enabled then
          SingDrawOscilloscope(190 + 10*ScreenX, 55, 180, 40, 2);
        if PlayerInfo.Playerinfo[3].Enabled then
          SingDrawOscilloscope(425 + 10*ScreenX, 55, 180, 40, 3);
      end;
    end;

    if PlayersPlay = 3 then begin
      if PlayerInfo.Playerinfo[0].Enabled then
        SingDrawOscilloscope(75 + 10*ScreenX, 95, 100, 20, 0);
      if PlayerInfo.Playerinfo[1].Enabled then
        SingDrawOscilloscope(370 + 10*ScreenX, 95, 100, 20, 1);
      if PlayerInfo.Playerinfo[2].Enabled then
        SingDrawOscilloscope(670 + 10*ScreenX, 95, 100, 20, 2);
    end;

    if PlayersPlay = 6 then begin
      if ScreenAct = 1 then begin
        if PlayerInfo.Playerinfo[0].Enabled then
          SingDrawOscilloscope( 75 + 10*ScreenX, 95, 100, 20, 0);
        if PlayerInfo.Playerinfo[1].Enabled then
          SingDrawOscilloscope(370 + 10*ScreenX, 95, 100, 20, 1);
        if PlayerInfo.Playerinfo[2].Enabled then
          SingDrawOscilloscope(670 + 10*ScreenX, 95, 100, 20, 2);
      end;
      if ScreenAct = 2 then begin
        if PlayerInfo.Playerinfo[3].Enabled then
          SingDrawOscilloscope( 75 + 10*ScreenX, 95, 100, 20, 3);
        if PlayerInfo.Playerinfo[4].Enabled then
          SingDrawOscilloscope(370 + 10*ScreenX, 95, 100, 20, 4);
        if PlayerInfo.Playerinfo[5].Enabled then
          SingDrawOscilloscope(670 + 10*ScreenX, 95, 100, 20, 5);
      end;
    end;

  end

  //SingBar Mod
  else if ((Ini.Oscilloscope = 2) AND (DLLMan.Selected.ShowRateBar_O)) OR (DLLMan.Selected.ShowRateBar) then begin
    A := GetTickCount div 33;
    if A <> Tickold then begin
      Tickold := A;
      for E := 0 to (PlayersPlay - 1) do begin //Set new Pos + Alpha
        I := Player[E].ScorePercentTarget - Player[E].ScorePercent;
        if I > 0 then Inc(Player[E].ScorePercent)
        else if I < 0 then Dec(Player[E].ScorePercent);
      end; //for
    end; //if
    if PlayersPlay = 1 then begin
      if PlayerInfo.Playerinfo[0].Enabled then
        SingDrawSingbar( 75 + 10*ScreenX, 95, 100, 8, Player[0].ScorePercent);
   end;
    if PlayersPlay = 2 then begin
      if PlayerInfo.Playerinfo[0].Enabled then
        SingDrawSingbar( 75 + 10*ScreenX, 95, 100, 8, Player[0].ScorePercent);
      if PlayerInfo.Playerinfo[1].Enabled then
        SingDrawSingbar(620 + 10*ScreenX, 95, 100, 8, Player[1].ScorePercent);
   end;
    if PlayersPlay = 3 then begin
      if PlayerInfo.Playerinfo[0].Enabled then
        SingDrawSingbar( 75 + 10*ScreenX, 95, 100, 8, Player[0].ScorePercent);
      if PlayerInfo.Playerinfo[1].Enabled then
        SingDrawSingbar(370 + 10*ScreenX, 95, 100, 8, Player[1].ScorePercent);
      if PlayerInfo.Playerinfo[2].Enabled then
        SingDrawSingbar(670 + 10*ScreenX, 95, 100, 8, Player[2].ScorePercent);
    end;
    if PlayersPlay = 4 then begin
      if ScreenAct = 1 then begin
        if PlayerInfo.Playerinfo[0].Enabled then
          SingDrawSingbar( 75 + 10*ScreenX, 95, 100, 8, Player[0].ScorePercent);
        if PlayerInfo.Playerinfo[1].Enabled then
          SingDrawSingbar(620 + 10*ScreenX, 95, 100, 8, Player[1].ScorePercent);
      end;
      if ScreenAct = 2 then begin
        if PlayerInfo.Playerinfo[2].Enabled then
          SingDrawSingbar( 75 + 10*ScreenX, 95, 100, 8, Player[2].ScorePercent);
        if PlayerInfo.Playerinfo[3].Enabled then
          SingDrawSingbar(620 + 10*ScreenX, 95, 100, 8, Player[3].ScorePercent);
      end;
    end;
    if PlayersPlay = 6 then begin
      if ScreenAct = 1 then begin
        if PlayerInfo.Playerinfo[0].Enabled then
          SingDrawSingbar( 75 + 10*ScreenX, 95, 100, 8, Player[0].ScorePercent);
        if PlayerInfo.Playerinfo[1].Enabled then
          SingDrawSingbar(370 + 10*ScreenX, 95, 100, 8, Player[1].ScorePercent);
        if PlayerInfo.Playerinfo[2].Enabled then
          SingDrawSingbar(670 + 10*ScreenX, 95, 100, 8, Player[2].ScorePercent);
      end;
      if ScreenAct = 2 then begin
        if PlayerInfo.Playerinfo[3].Enabled then
          SingDrawSingbar( 75 + 10*ScreenX, 95, 100, 8, Player[3].ScorePercent);
        if PlayerInfo.Playerinfo[4].Enabled then
          SingDrawSingbar(370 + 10*ScreenX, 95, 100, 8, Player[4].ScorePercent);
        if PlayerInfo.Playerinfo[5].Enabled then
          SingDrawSingbar(670 + 10*ScreenX, 95, 100, 8, Player[5].ScorePercent);
      end;
    end;
  end;
  //end Singbar Mod

  //PhrasenBonus - Line Bonus Mod
  if ((Ini.LineBonus > 0) AND (DLLMan.Selected.EnLineBonus_O)) OR (DLLMan.Selected.EnLineBonus) then begin
    A := GetTickCount div 33;
    if (A <> Tickold2) AND (Player[0].LineBonus_Visible) then begin
      Tickold2 := A;
      for E := 0 to (PlayersPlay - 1) do begin
          //Change Alpha
          Player[E].LineBonus_Alpha := Player[E].LineBonus_Alpha - 0.02;

          if Player[E].LineBonus_Alpha <= 0 then
            Player[E].LineBonus_Visible := False
          else
          begin
          //Change Position
          if (Player[E].LineBonus_PosX < Player[E].LineBonus_TargetX) then
            Inc(Player[E].LineBonus_PosX,1)
           else if (Player[E].LineBonus_PosX > Player[E].LineBonus_TargetX) then
            Dec(Player[E].LineBonus_PosX,1);

          if (Player[E].LineBonus_PosY < Player[E].LineBonus_TargetY) then
            Inc(Player[E].LineBonus_PosY,1)
          else if (Player[E].LineBonus_PosY > Player[E].LineBonus_TargetY) then
            Dec(Player[E].LineBonus_PosY,1);

          end;
      end;
    end; //if

    if PlayersPlay = 1 then begin
      if PlayerInfo.Playerinfo[0].Enabled then
        SingDrawLineBonus( Player[0].LineBonus_PosX, Player[0].LineBonus_PosY, Player[0].LineBonus_Color, Player[0].LineBonus_Alpha, Player[0].LineBonus_Text);
   end
   else if PlayersPlay = 2 then begin
      if PlayerInfo.Playerinfo[0].Enabled then
        SingDrawLineBonus( Player[0].LineBonus_PosX, Player[0].LineBonus_PosY, Player[0].LineBonus_Color, Player[0].LineBonus_Alpha, Player[0].LineBonus_Text);
      if PlayerInfo.Playerinfo[1].Enabled then
        SingDrawLineBonus( Player[1].LineBonus_PosX, Player[1].LineBonus_PosY, Player[1].LineBonus_Color, Player[1].LineBonus_Alpha, Player[1].LineBonus_Text);
   end
   else if PlayersPlay = 3 then begin
      if PlayerInfo.Playerinfo[0].Enabled then
        SingDrawLineBonus( Player[0].LineBonus_PosX, Player[0].LineBonus_PosY, Player[0].LineBonus_Color, Player[0].LineBonus_Alpha, Player[0].LineBonus_Text);
      if PlayerInfo.Playerinfo[1].Enabled then
        SingDrawLineBonus( Player[1].LineBonus_PosX, Player[1].LineBonus_PosY, Player[1].LineBonus_Color, Player[1].LineBonus_Alpha, Player[1].LineBonus_Text);
      if PlayerInfo.Playerinfo[2].Enabled then
        SingDrawLineBonus( Player[2].LineBonus_PosX, Player[2].LineBonus_PosY, Player[2].LineBonus_Color, Player[2].LineBonus_Alpha, Player[2].LineBonus_Text);
    end
   else if PlayersPlay = 4 then begin
      if ScreenAct = 1 then begin
        if PlayerInfo.Playerinfo[0].Enabled then
          SingDrawLineBonus( Player[0].LineBonus_PosX, Player[0].LineBonus_PosY, Player[0].LineBonus_Color, Player[0].LineBonus_Alpha, Player[0].LineBonus_Text);
        if PlayerInfo.Playerinfo[1].Enabled then
          SingDrawLineBonus( Player[1].LineBonus_PosX, Player[1].LineBonus_PosY, Player[1].LineBonus_Color, Player[1].LineBonus_Alpha, Player[1].LineBonus_Text);
      end;
      if ScreenAct = 2 then begin
        if PlayerInfo.Playerinfo[2].Enabled then
          SingDrawLineBonus( Player[2].LineBonus_PosX, Player[2].LineBonus_PosY, Player[2].LineBonus_Color, Player[2].LineBonus_Alpha, Player[2].LineBonus_Text);
        if PlayerInfo.Playerinfo[3].Enabled then
          SingDrawLineBonus( Player[3].LineBonus_PosX, Player[3].LineBonus_PosY, Player[3].LineBonus_Color, Player[3].LineBonus_Alpha, Player[3].LineBonus_Text);
      end;
    end;
    if PlayersPlay = 6 then begin
      if ScreenAct = 1 then begin
        if PlayerInfo.Playerinfo[0].Enabled then
          SingDrawLineBonus( Player[0].LineBonus_PosX, Player[0].LineBonus_PosY, Player[0].LineBonus_Color, Player[0].LineBonus_Alpha, Player[0].LineBonus_Text);
        if PlayerInfo.Playerinfo[1].Enabled then
          SingDrawLineBonus( Player[1].LineBonus_PosX, Player[1].LineBonus_PosY, Player[1].LineBonus_Color, Player[1].LineBonus_Alpha, Player[1].LineBonus_Text);
        if PlayerInfo.Playerinfo[2].Enabled then
          SingDrawLineBonus( Player[2].LineBonus_PosX, Player[2].LineBonus_PosY, Player[2].LineBonus_Color, Player[2].LineBonus_Alpha, Player[2].LineBonus_Text);
      end;
      if ScreenAct = 2 then begin
        if PlayerInfo.Playerinfo[3].Enabled then
          SingDrawLineBonus( Player[3].LineBonus_PosX, Player[3].LineBonus_PosY, Player[3].LineBonus_Color, Player[3].LineBonus_Alpha, Player[3].LineBonus_Text);
        if PlayerInfo.Playerinfo[4].Enabled then
          SingDrawLineBonus( Player[4].LineBonus_PosX, Player[4].LineBonus_PosY, Player[4].LineBonus_Color, Player[4].LineBonus_Alpha, Player[4].LineBonus_Text);
        if PlayerInfo.Playerinfo[5].Enabled then
          SingDrawLineBonus( Player[5].LineBonus_PosX, Player[5].LineBonus_PosY, Player[5].LineBonus_Color, Player[5].LineBonus_Alpha, Player[5].LineBonus_Text);
      end;
    end;
  end;
  //PhrasenBonus - Line Bonus Mod End


  // rysuje paski
//  Log.LogStatus('Original notes', 'SingDraw');
  case Ini.Difficulty of
    0:
      begin
        NotesH := 11; // 9
        NotesW := 6; // 5
      end;
    1:
      begin
        NotesH := 8; // 7
        NotesW := 4; // 4
      end;
    2:
      begin
        NotesH := 5;
        NotesW := 3;
      end;
  end;

  if (DLLMAn.Selected.ShowNotes And DLLMan.Selected.LoadSong) then
  begin
    if (PlayersPlay = 1) And PlayerInfo.Playerinfo[0].Enabled then begin
      SingDrawPlayerBGCzesc(NR.Left + 20, Skin_P2_NotesB, NR.Right - 20, 0, 0, 15);
      SingDrawCzesc(NR.Left + 20, Skin_P2_NotesB, NR.Right - 20, 0, 15);
      SingDrawPlayerCzesc(Nr.Left + 20, Skin_P2_NotesB, Nr.Width - 40, 0, 15);
    end;

    if (PlayersPlay = 2)  then begin
      if PlayerInfo.Playerinfo[0].Enabled then
      begin
        SingDrawPlayerBGCzesc(Nr.Left + 20, Skin_P1_NotesB, Nr.Right - 20, 0, 0, 15);
        SingDrawCzesc(NR.Left + 20, Skin_P1_NotesB, NR.Right - 20, 0, 15);
        SingDrawPlayerCzesc(Nr.Left + 20, Skin_P1_NotesB, Nr.Width - 40, 0, 15);
      end;
      if PlayerInfo.Playerinfo[1].Enabled then
      begin
        SingDrawPlayerBGCzesc(Nr.Left + 20, Skin_P2_NotesB, Nr.Right - 20, 0, 1, 15);
        SingDrawCzesc(NR.Left + 20, Skin_P2_NotesB, NR.Right - 20, 0, 15);
        SingDrawPlayerCzesc(Nr.Left + 20, Skin_P2_NotesB, Nr.Width - 40, 1, 15);
      end;

    end;

    if PlayersPlay = 3 then begin
      NotesW := NotesW * 0.8;
      NotesH := NotesH * 0.8;

      if PlayerInfo.Playerinfo[0].Enabled then
      begin
        SingDrawPlayerBGCzesc(Nr.Left + 20, 120+95, Nr.Right - 20, 0, 0, 12);
        SingDrawCzesc(NR.Left + 20, 120+95, NR.Right - 20, 0, 12);
        SingDrawPlayerCzesc(Nr.Left + 20, 120+95, Nr.Width - 40, 0, 12);
      end;

      if PlayerInfo.Playerinfo[1].Enabled then
      begin
        SingDrawPlayerBGCzesc(Nr.Left + 20, 245+95, Nr.Right - 20, 0, 1, 12);
        SingDrawCzesc(NR.Left + 20, 245+95, NR.Right - 20, 0, 12);
        SingDrawPlayerCzesc(Nr.Left + 20, 245+95, Nr.Width - 40, 1, 12);
      end;

      if PlayerInfo.Playerinfo[2].Enabled then
      begin
        SingDrawPlayerBGCzesc(Nr.Left + 20, 370+95, Nr.Right - 20, 0, 2, 12);
        SingDrawCzesc(NR.Left + 20, 370+95, NR.Right - 20, 0, 12);
        SingDrawPlayerCzesc(Nr.Left + 20, 370+95, Nr.Width - 40, 2, 12);
      end;
    end;

    if PlayersPlay = 4 then begin
      if ScreenAct = 1 then begin
        SingDrawPlayerBGCzesc(Nr.Left + 20, Skin_P1_NotesB, Nr.Right - 20, 0, 0, 15);
        SingDrawPlayerBGCzesc(Nr.Left + 20, Skin_P2_NotesB, Nr.Right - 20, 0, 1, 15);
      end;
      if ScreenAct = 2 then begin
        SingDrawPlayerBGCzesc(Nr.Left + 20, Skin_P1_NotesB, Nr.Right - 20, 0, 2, 15);
        SingDrawPlayerBGCzesc(Nr.Left + 20, Skin_P2_NotesB, Nr.Right - 20, 0, 3, 15);
      end;

      SingDrawCzesc(NR.Left + 20, Skin_P1_NotesB, NR.Right - 20, 0, 15);
      SingDrawCzesc(NR.Left + 20, Skin_P2_NotesB, NR.Right - 20, 0, 15);

      if ScreenAct = 1 then begin
        SingDrawPlayerCzesc(Nr.Left + 20, Skin_P1_NotesB, Nr.Width - 40, 0, 15);
        SingDrawPlayerCzesc(Nr.Left + 20, Skin_P2_NotesB, Nr.Width - 40, 1, 15);
      end;
      if ScreenAct = 2 then begin
        SingDrawPlayerCzesc(Nr.Left + 20, Skin_P1_NotesB, Nr.Width - 40, 2, 15);
        SingDrawPlayerCzesc(Nr.Left + 20, Skin_P2_NotesB, Nr.Width - 40, 3, 15);
      end;
    end;

    if PlayersPlay = 6 then begin
      NotesW := NotesW * 0.8;
      NotesH := NotesH * 0.8;

      if ScreenAct = 1 then begin
        SingDrawPlayerBGCzesc(Nr.Left + 20, 120+95, Nr.Right - 20, 0, 0, 12);
        SingDrawPlayerBGCzesc(Nr.Left + 20, 245+95, Nr.Right - 20, 0, 1, 12);
        SingDrawPlayerBGCzesc(Nr.Left + 20, 370+95, Nr.Right - 20, 0, 2, 12);
      end;
      if ScreenAct = 2 then begin
        SingDrawPlayerBGCzesc(Nr.Left + 20, 120+95, Nr.Right - 20, 0, 3, 12);
        SingDrawPlayerBGCzesc(Nr.Left + 20, 245+95, Nr.Right - 20, 0, 4, 12);
        SingDrawPlayerBGCzesc(Nr.Left + 20, 370+95, Nr.Right - 20, 0, 5, 12);
      end;

      SingDrawCzesc(NR.Left + 20, 120+95, NR.Right - 20, 0, 12);
      SingDrawCzesc(NR.Left + 20, 245+95, NR.Right - 20, 0, 12);
      SingDrawCzesc(NR.Left + 20, 370+95, NR.Right - 20, 0, 12);

      if ScreenAct = 1 then begin
        SingDrawPlayerCzesc(Nr.Left + 20, 120+95, Nr.Width - 40, 0, 12);
        SingDrawPlayerCzesc(Nr.Left + 20, 245+95, Nr.Width - 40, 1, 12);
        SingDrawPlayerCzesc(Nr.Left + 20, 370+95, Nr.Width - 40, 2, 12);
      end;
      if ScreenAct = 2 then begin
        SingDrawPlayerCzesc(Nr.Left + 20, 120+95, Nr.Width - 40, 3, 12);
        SingDrawPlayerCzesc(Nr.Left + 20, 245+95, Nr.Width - 40, 4, 12);
        SingDrawPlayerCzesc(Nr.Left + 20, 370+95, Nr.Width - 40, 5, 12);
      end;
    end;
  end;

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;


//SingBar Mod
procedure SingDrawSingbar(X, Y, W, H: real; Percent: integer);
var
  R:   Real;
  G:   Real;
  B:   Real;
  A:   cardinal;
  I:   Integer;

begin;

   //SingBar Background
  glColor4f(1, 1, 1, 0.8);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindTexture(GL_TEXTURE_2D, Tex_SingBar_Back.TexNum);
  glBegin(GL_QUADS);
    glTexCoord2f(1/16, 0); glVertex2f(X, Y);
    glTexCoord2f(1/16, 1); glVertex2f(X, Y+H);
    glTexCoord2f(1 - 1/16, 1); glVertex2f(X+W, Y+H);
    glTexCoord2f(1 - 1/16, 0); glVertex2f(X+W, Y);
  glEnd;

  //SingBar coloured Bar
  Case Percent of
    0..22: begin
          R := 1;
          G := 0;
          B := 0;
        end;
    23..42: begin
          R := 1;
          G := ((Percent-23)/100)*5;
          B := 0;
        end;
    43..57: begin
          R := 1;
          G := 1;
          B := 0;
        end;
    58..77: begin
          R := 1-(Percent - 58)/100*5;
          G := 1;
          B := 0;
        end;
    78..99: begin
          R := 0;
          G := 1;
          B := 0;
        end;
    End; //Case

  glColor4f(R, G, B, 1);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindTexture(GL_TEXTURE_2D, Tex_SingBar_Bar.TexNum);
  //Size= Player[PlayerNum].ScorePercent of W
    glBegin(GL_QUADS);
    glTexCoord2f(1/16, 0); glVertex2f(X, Y);
    glTexCoord2f(1/16, 1); glVertex2f(X, Y+H);
    glTexCoord2f(1 - 1/16, 1); glVertex2f(X+(W/100 * (Percent +1)), Y+H);
    glTexCoord2f(1 - 1/16, 0); glVertex2f(X+(W/100 * (Percent +1)), Y);
  glEnd;

  //SingBar Front
  glColor4f(1, 1, 1, 0.6);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindTexture(GL_TEXTURE_2D, Tex_SingBar_Front.TexNum);
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex2f(X, Y);
    glTexCoord2f(0, 1); glVertex2f(X, Y+H);
    glTexCoord2f(1, 1); glVertex2f(X+W, Y+H);
    glTexCoord2f(1, 0); glVertex2f(X+W, Y);
  glEnd;
end;
//end Singbar Mod

//PhrasenBonus - Line Bonus Mod
procedure SingDrawLineBonus( const X, Y: integer; Color: TRGB; Alpha: Single; Text: string);
var
Length, X2: Real; //Length of Text
begin
if Alpha <> 0 then
begin

//Set Font Propertys
SetFontStyle(2); //Font: Outlined1
SetFontSize(6);
SetFontItalic(False);

//Check Font Size
Length := glTextWidth ( PChar(Text)) + 3; //Little Space for a Better Look ^^

//Text
SetFontPos (X + 50 - (Length / 2), Y + 12); //Position

  //Draw  Background
  glColor4f(Color.R, Color.G, Color.B, Alpha); //Set Color
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  //glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  //Old Method with Variable Background
  {//Draw Left Side
  glBindTexture(GL_TEXTURE_2D, Tex_SingLineBonusL.TexNum);

  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex2f(X, Y);
    glTexCoord2f(0, 1); glVertex2f(X, Y + Tex_SingLineBonusL.H * Tex_SingLineBonusL.ScaleH);
    glTexCoord2f(1, 1); glVertex2f(X + Tex_SingLineBonusL.W * Tex_SingLineBonusL.scaleW, Y + Tex_SingLineBonusL.H * Tex_SingLineBonusL.scaleH);
    glTexCoord2f(1, 0); glVertex2f(X + Tex_SingLineBonusL.W * Tex_SingLineBonusL.scaleW, Y);
  glEnd;

  //Set X of Next Tile
  X2 := X + Tex_SingLineBonusL.W * Tex_SingLineBonusL.scaleW;

  //Draw Middle
  glBindTexture(GL_TEXTURE_2D, Tex_SingLineBonusM.TexNum);

  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex2f(X2, Y);
    glTexCoord2f(0, 1); glVertex2f(X2, Y + Tex_SingLineBonusM.H * Tex_SingLineBonusM.ScaleH);
    glTexCoord2f(1, 1); glVertex2f(X2 + Length, Y + Tex_SingLineBonusM.H * Tex_SingLineBonusM.scaleH);
    glTexCoord2f(1, 0); glVertex2f(X2 + Length, Y);
  glEnd;

  //Set X of Next Tile
  X2 := X2 + Length;

  //Draw Rigth Side
  glBindTexture(GL_TEXTURE_2D, Tex_SingLineBonusR.TexNum);

  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex2f(X2, Y);
    glTexCoord2f(0, 1); glVertex2f(X2, Y + Tex_SingLineBonusR.H * Tex_SingLineBonusR.ScaleH);
    glTexCoord2f(1, 1); glVertex2f(X2 + Tex_SingLineBonusR.W * Tex_SingLineBonusR.scaleW, Y + Tex_SingLineBonusR.H * Tex_SingLineBonusR.scaleH);
    glTexCoord2f(1, 0); glVertex2f(X2 + Tex_SingLineBonusR.W * Tex_SingLineBonusR.scaleW, Y);
  glEnd;}
  //New Method, Not Variable
  glBindTexture(GL_TEXTURE_2D, Tex_SingLineBonusBack.TexNum);

  glBegin(GL_QUADS);
    glTexCoord2f(1/32, 0); glVertex2f(X, Y);
    glTexCoord2f(1/32, 1); glVertex2f(X, Y + 50);
    glTexCoord2f(31/32, 1); glVertex2f(X + 100, Y + 50);
    glTexCoord2f(31/32, 0); glVertex2f(X + 100, Y);
  glEnd;

  glColor4f(1, 1, 1, Alpha); //Set Color
  //Draw Text
  glPrint (PChar(Text));
end;
end;
//PhrasenBonus - Line Bonus Mod

// Draw Note Bars for Editor
//There are 11 Resons for a new Procdedure:
// 1. It don't look good when you Draw the Golden Note Star Effect in the Editor
// 2. You can see the Freestyle Notes in the Editor SemiTransparent
// 3. Its easier and Faster then changing the old Procedure
procedure EditDrawCzesc(Left, Top, Right: real; NrCzesci: integer; Space: integer);
var
  Rec:      TRecR;
  Pet:      integer;
  TempR:    real;
begin
  glColor3f(1, 1, 1);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  TempR := (Right-Left) / (Czesci[NrCzesci].Czesc[Czesci[NrCzesci].Akt].Koniec - Czesci[NrCzesci].Czesc[Czesci[NrCzesci].Akt].StartNote);
  with Czesci[NrCzesci].Czesc[Czesci[NrCzesci].Akt] do begin
    for Pet := 0 to HighNut do begin
      with Nuta[Pet] do begin

          // Golden Note Patch
          case Wartosc of
            0: glColor4f(1, 1, 1, 0.35);
            1: glColor4f(1, 1, 1, 0.85);
            2: glColor4f(1, 1, 0.3, 0.85);
          end; // case



          // lewa czesc  -  left part
          Rec.Left := (Start-Czesci[NrCzesci].Czesc[Czesci[NrCzesci].Akt].StartNote) * TempR + Left + 0.5 + 10*ScreenX;
          Rec.Right := Rec.Left + NotesW;
          Rec.Top := Top - (Ton-BaseNote)*Space/2 - NotesH;
          Rec.Bottom := Rec.Top + 2 * NotesH;
          glBindTexture(GL_TEXTURE_2D, Tex_Left[Color].TexNum);
          glBegin(GL_QUADS);
            glTexCoord2f(0,   0); glVertex2f(Rec.Left,  Rec.Top);
            glTexCoord2f(0,   1); glVertex2f(Rec.Left,  Rec.Bottom);
            glTexCoord2f(7/8, 1); glVertex2f(Rec.Right, Rec.Bottom);
            glTexCoord2f(7/8, 0); glVertex2f(Rec.Right, Rec.Top);
          glEnd;

         // srodkowa czesc  -  middle part
        Rec.Left := Rec.Right;
        Rec.Right := (Start+Dlugosc-Czesci[NrCzesci].Czesc[Czesci[NrCzesci].Akt].StartNote) * TempR + Left - NotesW - 0.5 + 10*ScreenX;

        glBindTexture(GL_TEXTURE_2D, Tex_Mid[Color].TexNum);
        glBegin(GL_QUADS);
          glTexCoord2f(1/32,  0); glVertex2f(Rec.Left,  Rec.Top);
          glTexCoord2f(1/32,  1); glVertex2f(Rec.Left,  Rec.Bottom);
          glTexCoord2f(31/32, 1); glVertex2f(Rec.Right, Rec.Bottom);
         glTexCoord2f(31/32, 0); glVertex2f(Rec.Right, Rec.Top);
        glEnd;

        // prawa czesc  -  right part
        Rec.Left := Rec.Right;
        Rec.Right := Rec.Right + NotesW;

        glBindTexture(GL_TEXTURE_2D, Tex_Right[Color].TexNum);
        glBegin(GL_QUADS);
          glTexCoord2f(1/8, 0); glVertex2f(Rec.Left,  Rec.Top);
          glTexCoord2f(1/8, 1); glVertex2f(Rec.Left,  Rec.Bottom);
          glTexCoord2f(1,   1); glVertex2f(Rec.Right, Rec.Bottom);
          glTexCoord2f(1,   0); glVertex2f(Rec.Right, Rec.Top);
        glEnd;

      end; // with
    end; // for
  end; // with

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

end.

