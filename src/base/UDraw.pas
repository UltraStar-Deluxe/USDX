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

unit UDraw;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UThemes,
  UGraphicClasses;

procedure SingDraw;
procedure SingDrawBackground;
procedure SingDrawOscilloscope(X, Y, W, H: real; NrSound: integer);
procedure SingDrawNoteLines(Left, Top, Right: real; Space: integer);
procedure SingDrawLyricHelper(Left, LyricsMid: real);
procedure SingDrawBeatDelimeters(Left, Top, Right: real; NrLines: integer);
procedure SingDrawLine(Left, Top, Right: real; NrLines: integer; Space: integer);
procedure SingDrawPlayerLine(X, Y, W: real; PlayerIndex: integer; Space: integer);
procedure SingDrawPlayerBGLine(Left, Top, Right: real; NrLines, PlayerIndex: integer; Space: integer);

// TimeBar
procedure SingDrawTimeBar();

//Draw Editor NoteLines
procedure EditDrawLine(Left, Top, Right: real; NrLines: integer; Space: integer);

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
  StarfrG:  integer;

  //SingBar
  TickOld:  cardinal;
  TickOld2: cardinal;

implementation

uses
  SysUtils,
  Math,
  gl,
  TextGL,
  UDrawTexture,
  UGraphic,
  UIni,
  ULog,
  ULyrics,
  UNote,
  UMusic,
  URecord,
  UScreenSing,
  UTexture;

procedure SingDrawBackground;
var
  Rec:    TRecR;
  TexRec: TRecR;
begin
  if (ScreenSing.Tex_Background.TexNum > 0) then
  begin
    if (Ini.MovieSize <= 1) then  //HalfSize BG
    begin
      (* half screen + gradient *)
      Rec.Top := 110; // 80
      Rec.Bottom := Rec.Top + 20;
      Rec.Left  := 0;
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
  SampleIndex: integer;
  Sound:       TCaptureBuffer;
  MaxX, MaxY:  real;
begin;
  Sound := AudioInputProcessor.Sound[NrSound];

  //  Log.LogStatus('Oscilloscope', 'SingDraw');
  glColor3f(Skin_OscR, Skin_OscG, Skin_OscB);
{
  if (ParamStr(1) = '-black') or (ParamStr(1) = '-fsblack') then
    glColor3f(1, 1, 1);
}
  MaxX := W-1;
  MaxY := (H-1) / 2;

  Sound.LockAnalysisBuffer();

  glBegin(GL_LINE_STRIP);
    for SampleIndex := 0 to High(Sound.AnalysisBuffer) do
    begin
      glVertex2f(X + MaxX * SampleIndex/High(Sound.AnalysisBuffer),
                 Y + MaxY * (1 - Sound.AnalysisBuffer[SampleIndex]/-Low(Smallint)));
    end;
  glEnd;

  Sound.UnlockAnalysisBuffer();
end;

procedure SingDrawNoteLines(Left, Top, Right: real; Space: integer);
var
  Count: integer;
begin
  glEnable(GL_BLEND);
  glColor4f(Skin_P1_LinesR, Skin_P1_LinesG, Skin_P1_LinesB, 0.4);
  glBegin(GL_LINES);
  for Count := 0 to 9 do
  begin
    glVertex2f(Left,  Top + Count * Space);
    glVertex2f(Right, Top + Count * Space);
  end;
  glEnd;
  glDisable(GL_BLEND);
end;

procedure SingDrawBeatDelimeters(Left, Top, Right: real; NrLines: integer);
var
  Count: integer;
  TempR: real;
begin
  TempR := (Right-Left) / (Lines[NrLines].Line[Lines[NrLines].Current].End_ - Lines[NrLines].Line[Lines[NrLines].Current].Note[0].Start);
  glEnable(GL_BLEND);
  glBegin(GL_LINES);
  for Count := Lines[NrLines].Line[Lines[NrLines].Current].Note[0].Start to Lines[NrLines].Line[Lines[NrLines].Current].End_ do
  begin
    if (Count mod Lines[NrLines].Resolution) = Lines[NrLines].NotesGAP then
      glColor4f(0, 0, 0, 1)
    else
      glColor4f(0, 0, 0, 0.3);
    glVertex2f(Left + TempR * (Count - Lines[NrLines].Line[Lines[NrLines].Current].Note[0].Start), Top);
    glVertex2f(Left + TempR * (Count - Lines[NrLines].Line[Lines[NrLines].Current].Note[0].Start), Top + 135);
  end;
  glEnd;
  glDisable(GL_BLEND);
end;

// draw blank Notebars
procedure SingDrawLine(Left, Top, Right: real; NrLines: integer; Space: integer);
var
  Rec:   TRecR;
  Count: integer;
  TempR: real;

  PlayerNumber:  integer;

  GoldenStarPos: real;

  lTmpA, lTmpB : real;
begin
// We actually don't have a playernumber in this procedure, it should reside in NrLines - but it is always set to zero
// So we exploit this behavior a bit - we give NrLines the playernumber, keep it in playernumber - and then we set NrLines to zero
// This could also come quite in handy when we do the duet mode, cause just the notes for the player that has to sing should be drawn then
// BUT this is not implemented yet, all notes are drawn! :D
  if (ScreenSing.settings.NotesVisible and (1 shl NrLines) <> 0) then
  begin

    PlayerNumber := NrLines + 1; // Player 1 is 0
    NrLines     := 0;

  // exploit done

    glColor3f(1, 1, 1);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    lTmpA := (Right-Left);
    lTmpB := (Lines[NrLines].Line[Lines[NrLines].Current].End_ - Lines[NrLines].Line[Lines[NrLines].Current].Note[0].Start);

  if ( lTmpA > 0 ) and ( lTmpB > 0 ) then
    TempR := lTmpA / lTmpB
  else
    TempR := 0;

  with Lines[NrLines].Line[Lines[NrLines].Current] do
  begin
    for Count := 0 to HighNote do
    begin
      with Note[Count] do
      begin
        if NoteType <> ntFreestyle then
	begin

            if Ini.EffectSing = 0 then
              // If Golden note Effect of then Change not Color
            begin
              case NoteType of
                ntNormal: glColor4f(1, 1, 1, 1);   // We set alpha to 1, cause we can control the transparency through the png itself
                ntGolden: glColor4f(1, 1, 0.3, 1); // no stars, paint yellow -> glColor4f(1, 1, 0.3, 0.85); - we could
            end; // case
            end //Else all Notes same Color
            else
              glColor4f(1, 1, 1, 1);        // We set alpha to 1, cause we can control the transparency through the png itself

          // left part
          Rec.Left  := (Start-Lines[NrLines].Line[Lines[NrLines].Current].Note[0].Start) * TempR + Left + 0.5 + 10*ScreenX;
          Rec.Right := Rec.Left + NotesW;
          Rec.Top := Top - (Tone-BaseNote)*Space/2 - NotesH;
          Rec.Bottom := Rec.Top + 2 * NotesH;
          glBindTexture(GL_TEXTURE_2D, Tex_plain_Left[PlayerNumber].TexNum);
          glBegin(GL_QUADS);
            glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
            glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
            glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
            glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
          glEnd;

            //We keep the postion of the top left corner b4 it's overwritten
            GoldenStarPos := Rec.Left;
            //done

            // middle part
            Rec.Left := Rec.Right;
            Rec.Right := (Start+Length-Lines[NrLines].Line[Lines[NrLines].Current].Note[0].Start) * TempR + Left - NotesW - 0.5 + 10*ScreenX;

            glBindTexture(GL_TEXTURE_2D, Tex_plain_Mid[PlayerNumber].TexNum);
            glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
            glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );
            glBegin(GL_QUADS);
              glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
              glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
              glTexCoord2f(round((Rec.Right-Rec.Left)/32), 1); glVertex2f(Rec.Right, Rec.Bottom);
              glTexCoord2f(round((Rec.Right-Rec.Left)/32), 0); glVertex2f(Rec.Right, Rec.Top);
            glEnd;

        // right part
        Rec.Left  := Rec.Right;
        Rec.Right := Rec.Right + NotesW;

            glBindTexture(GL_TEXTURE_2D, Tex_plain_Right[PlayerNumber].TexNum);
            glBegin(GL_QUADS);
              glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
              glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
              glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
              glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
            glEnd;

          // Golden Star Patch
          if (NoteType = ntGolden) and (Ini.EffectSing=1) then
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
end;

// draw sung notes
procedure SingDrawPlayerLine(X, Y, W: real; PlayerIndex: integer; Space: integer);
var
  TempR:      real;
  Rec:        TRecR;
  N:          integer;
//  R, G, B, A: real;
  NotesH2:    real;
begin
  //Log.LogStatus('Player notes', 'SingDraw');
{
  if NrGracza = 0 then
    LoadColor(R, G, B, 'P1Light')
  else
    LoadColor(R, G, B, 'P2Light');
}
  //R :=  71/255;
  //G := 175/255;
  //B := 247/255;

  glColor3f(1, 1, 1);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  //if Player[NrGracza].LengthNote > 0 then
  begin
    TempR := W / (Lines[0].Line[Lines[0].Current].End_ - Lines[0].Line[Lines[0].Current].Note[0].Start);
    for N := 0 to Player[PlayerIndex].HighNote do
    begin
      with Player[PlayerIndex].Note[N] do
      begin
        // Left part of note
        Rec.Left  := X + (Start-Lines[0].Line[Lines[0].Current].Note[0].Start) * TempR + 0.5 + 10*ScreenX;
        Rec.Right := Rec.Left + NotesW;

        // Draw it in half size, if not hit
        if Hit then
        begin
          NotesH2 := NotesH
        end
        else
        begin
          NotesH2 := int(NotesH * 0.65);
        end;

        Rec.Top    := Y - (Tone-Lines[0].Line[Lines[0].Current].BaseNote)*Space/2 - NotesH2;
        Rec.Bottom := Rec.Top + 2 * NotesH2;

        // draw the left part
        glColor3f(1, 1, 1);
        glBindTexture(GL_TEXTURE_2D, Tex_Left[PlayerIndex+1].TexNum);
        glBegin(GL_QUADS);
          glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
          glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
          glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
          glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
        glEnd;

        // Middle part of the note
        Rec.Left  := Rec.Right;
        Rec.Right := X + (Start+Length-Lines[0].Line[Lines[0].Current].Note[0].Start) * TempR - NotesW - 0.5  + 10*ScreenX;

        // new
        if (Start+Length-1 = LyricsState.CurrentBeatD) then
          Rec.Right := Rec.Right - (1-Frac(LyricsState.MidBeatD)) * TempR;
        // the left note is more right than the right note itself, sounds weird - so we fix that xD
        if Rec.Right <= Rec.Left then
          Rec.Right := Rec.Left;

        // draw the middle part
        glBindTexture(GL_TEXTURE_2D, Tex_Mid[PlayerIndex+1].TexNum);
        glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
        glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );
        glBegin(GL_QUADS);
          glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
          glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
          glTexCoord2f(round((Rec.Right-Rec.Left)/32), 1); glVertex2f(Rec.Right, Rec.Bottom);
          glTexCoord2f(round((Rec.Right-Rec.Left)/32), 0); glVertex2f(Rec.Right, Rec.Top);
        glEnd;
        glColor3f(1, 1, 1);

        // the right part of the note
        Rec.Left  := Rec.Right;
        Rec.Right := Rec.Right + NotesW;

        glBindTexture(GL_TEXTURE_2D, Tex_Right[PlayerIndex+1].TexNum);
        glBegin(GL_QUADS);
          glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
          glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
          glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
          glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
        glEnd;

        // Perfect note is stored
        if Perfect and (Ini.EffectSing=1) then
        begin
          //A := 1 - 2*(LyricsState.GetCurrentTime() - GetTimeFromBeat(Start+Length));
          if not (Start+Length-1 = LyricsState.CurrentBeatD) then
          begin
            //Star animation counter
            //inc(Starfr);
            //Starfr := Starfr mod 128;
            GoldenRec.SavePerfectNotePos(Rec.Left, Rec.Top);
          end;
        end;
      end; // with
    end; // for

    // actually we need a comparison here, to determine if the singing process
    // is ahead Rec.Right even if there is no singing

    if (Ini.EffectSing = 1) then
      GoldenRec.GoldenNoteTwinkle(Rec.Top,Rec.Bottom,Rec.Right, PlayerIndex);
  end; // if
end;

//draw Note glow
procedure SingDrawPlayerBGLine(Left, Top, Right: real; NrLines, PlayerIndex: integer; Space: integer);
var
  Rec:            TRecR;
  Count:          integer;
  TempR:          real;
  X1, X2, X3, X4: real;
  W, H:           real;
  lTmpA, lTmpB:   real;
begin
  if (ScreenSing.settings.NotesVisible and (1 shl PlayerIndex) <> 0) then
  begin
    glColor4f(1, 1, 1, sqrt((1+sin( AudioPlayback.Position * 3))/4)/ 2 + 0.5 );
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    lTmpA := (Right-Left);
    lTmpB := (Lines[NrLines].Line[Lines[NrLines].Current].End_ - Lines[NrLines].Line[Lines[NrLines].Current].Note[0].Start);

    if ( lTmpA > 0 ) and ( lTmpB > 0 ) then
      TempR := lTmpA / lTmpB
    else
      TempR := 0;

    with Lines[NrLines].Line[Lines[NrLines].Current] do
    begin
      for Count := 0 to HighNote do
      begin
        with Note[Count] do
        begin
          if NoteType <> ntFreestyle then
          begin
            // begin: 14, 20
            // easy: 6, 11
            W := NotesW * 2 + 2;
            H := NotesH * 1.5 + 3.5;

            X2 := (Start-Lines[NrLines].Line[Lines[NrLines].Current].Note[0].Start) * TempR + Left + 0.5 + 10*ScreenX + 4;
            X1 := X2-W;

            X3 := (Start+Length-Lines[NrLines].Line[Lines[NrLines].Current].Note[0].Start) * TempR + Left - 0.5 + 10*ScreenX - 4;
            X4 := X3+W;

            // left
            Rec.Left  := X1;
            Rec.Right := X2;
            Rec.Top := Top - (Tone-BaseNote)*Space/2 - H;
            Rec.Bottom := Rec.Top + 2 * H;

            glBindTexture(GL_TEXTURE_2D, Tex_BG_Left[PlayerIndex+1].TexNum);
            glBegin(GL_QUADS);
              glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
              glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
              glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
              glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
            glEnd;

            // middle part
            Rec.Left  := X2;
            Rec.Right := X3;

            glBindTexture(GL_TEXTURE_2D, Tex_BG_Mid[PlayerIndex+1].TexNum);
            glBegin(GL_QUADS);
              glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
              glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
              glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
              glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
            glEnd;

            // right part
            Rec.Left  := X3;
            Rec.Right := X4;

            glBindTexture(GL_TEXTURE_2D, Tex_BG_Right[PlayerIndex+1].TexNum);
            glBegin(GL_QUADS);
              glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
              glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
              glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
              glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
            glEnd;
          end; // if not FreeStyle
        end; // with
      end; // for
    end; // with

    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
  end;
end;

(**
 * Draws the lyrics helper bar.
 * Left: position the bar starts at
 * LyricsMid: the middle of the lyrics relative to the position Left 
 *)
procedure SingDrawLyricHelper(Left, LyricsMid: real);
var
  Bounds: TRecR;           // bounds of the lyric help bar
  BarProgress: real;       // progress of the lyrics helper
  BarMoveDelta: real;      // current beat relative to the beat the bar starts to move at
  BarAlpha: real;          // transparency
  CurLine:  PLine;         // current lyric line (beat specific)
  LineWidth: real;         // lyric line width
  FirstNoteBeat: integer;  // beat of the first note in the current line
  FirstNoteDelta: integer; // time in beats between the start of the current line and its first note
  MoveStartX: real;        // x-pos. the bar starts to move from
  MoveDist: real;          // number of pixels the bar will move
  LyricEngine: TLyricEngine;
const
  BarWidth  = 50; // width  of the lyric helper bar
  BarHeight = 30; // height of the lyric helper bar
  BarMoveLimit = 40; // max. number of beats remaining before the bar starts to move
begin
  // get current lyrics line and the time in beats of its first note
  CurLine := @Lines[0].Line[Lines[0].Current];

  // FIXME: accessing ScreenSing is not that generic
  LyricEngine := ScreenSing.Lyrics;

  // do not draw the lyrics helper if the current line does not contain any note
  if (Length(CurLine.Note) > 0) then
  begin
    // start beat of the first note of this line
    FirstNoteBeat := CurLine.Note[0].Start;
    // time in beats between the start of the current line and its first note
    FirstNoteDelta := FirstNoteBeat - CurLine.Start;

    // beats from current beat to the first note of the line
    BarMoveDelta := FirstNoteBeat - LyricsState.MidBeat;

    if (FirstNoteDelta > 8) and  // if the wait-time is large enough
       (BarMoveDelta > 0) then   // and the first note of the line is not reached
    begin
      // let the bar blink to the beat
      BarAlpha := 0.75 + cos(BarMoveDelta/2) * 0.25;

      // if the number of beats to the first note is too big,
      // the bar stays on the left side.
      if (BarMoveDelta > BarMoveLimit) then
        BarMoveDelta := BarMoveLimit;

      // limit number of beats the bar moves
      if (FirstNoteDelta > BarMoveLimit) then
        FirstNoteDelta := BarMoveLimit;

      // calc bar progress
      BarProgress := 1 - BarMoveDelta / FirstNoteDelta;

      // retrieve the width of the upper lyrics line on the display
      if (LyricEngine.GetUpperLine() <> nil) then
        LineWidth := LyricEngine.GetUpperLine().Width
      else
        LineWidth := 0;

      // distance the bar will move (LyricRec.Left to beginning of text)
      MoveDist := LyricsMid - LineWidth / 2 - BarWidth;
      // if the line is too long the helper might move from right to left
      // so we have to assure the start position is left of the text.
      if (MoveDist >= 0) then
        MoveStartX := Left
      else
        MoveStartX := Left + MoveDist;

      // determine lyric help bar position and size
      Bounds.Left := MoveStartX + BarProgress * MoveDist;
      Bounds.Right := Bounds.Left + BarWidth;
      Bounds.Top := Theme.LyricBar.IndicatorYOffset + Theme.LyricBar.UpperY ;
      Bounds.Bottom := Bounds.Top + BarHeight + 3;

      // draw lyric help bar
      glEnable(GL_TEXTURE_2D);
      glEnable(GL_BLEND);
      glColor4f(1, 1, 1, BarAlpha);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glBindTexture(GL_TEXTURE_2D, Tex_Lyric_Help_Bar.TexNum);
      glBegin(GL_QUADS);
        glTexCoord2f(0, 0); glVertex2f(Bounds.Left,  Bounds.Top);
        glTexCoord2f(0, 1); glVertex2f(Bounds.Left,  Bounds.Bottom);
        glTexCoord2f(1, 1); glVertex2f(Bounds.Right, Bounds.Bottom);
        glTexCoord2f(1, 0); glVertex2f(Bounds.Right, Bounds.Top);
      glEnd;
      glDisable(GL_BLEND);
    end;
  end;
end;

procedure SingDraw;
var
  NR: TRecR;         // lyrics area bounds (NR = NoteRec?)
  LyricEngine: TLyricEngine;
begin
  // positions
  if Ini.SingWindow = 0 then
    NR.Left := 120
  else
    NR.Left := 20;

  NR.Right := 780;

  NR.Width := NR.Right - NR.Left;
  NR.WMid  := NR.Width / 2;
  NR.Mid   := NR.Left + NR.WMid;

  // FIXME: accessing ScreenSing is not that generic
  LyricEngine := ScreenSing.Lyrics;

  // draw time-bar
  SingDrawTimeBar();

  // draw note-lines

  // to-do : needs fix when party mode works w/ 2 screens 
  if (PlayersPlay = 1) and (Ini.NoteLines = 1) and (ScreenSing.settings.NotesVisible and (1) <> 0) then
    SingDrawNoteLines(NR.Left + 10*ScreenX, Skin_P2_NotesB - 105, NR.Right + 10*ScreenX, 15);

  if ((PlayersPlay = 2) or (PlayersPlay = 4)) and (Ini.NoteLines = 1) then
  begin
    if (ScreenSing.settings.NotesVisible and (1 shl 0) <> 0) then
      SingDrawNoteLines(Nr.Left + 10*ScreenX, Skin_P1_NotesB - 105, Nr.Right + 10*ScreenX, 15);
    if (ScreenSing.settings.NotesVisible and (1 shl 1) <> 0) then
      SingDrawNoteLines(Nr.Left + 10*ScreenX, Skin_P2_NotesB - 105, Nr.Right + 10*ScreenX, 15);
  end;

  if ((PlayersPlay = 3) or (PlayersPlay = 6)) and (Ini.NoteLines = 1) then begin
    if (ScreenSing.settings.NotesVisible and (1 shl 0) <> 0) then
      SingDrawNoteLines(Nr.Left + 10*ScreenX, 120, Nr.Right + 10*ScreenX, 12);
    if (ScreenSing.settings.NotesVisible and (1 shl 1) <> 0) then
      SingDrawNoteLines(Nr.Left + 10*ScreenX, 245, Nr.Right + 10*ScreenX, 12);
    if (ScreenSing.settings.NotesVisible and (1 shl 2) <> 0) then
      SingDrawNoteLines(Nr.Left + 10*ScreenX, 370, Nr.Right + 10*ScreenX, 12);
  end;

  // draw Lyrics
  LyricEngine.Draw(LyricsState.MidBeat);
  SingDrawLyricHelper(NR.Left, NR.WMid);

  // oscilloscope
  if Ini.Oscilloscope = 1 then
  begin
    if PlayersPlay = 1 then
      SingDrawOscilloscope(190 + 10*ScreenX, 55, 180, 40, 0);

    if PlayersPlay = 2 then
    begin
      SingDrawOscilloscope(190 + 10*ScreenX, 55, 180, 40, 0);
      SingDrawOscilloscope(425 + 10*ScreenX, 55, 180, 40, 1);
    end;

    if PlayersPlay = 4 then
    begin
      if ScreenAct = 1 then
      begin
        SingDrawOscilloscope(190 + 10*ScreenX, 55, 180, 40, 0);
        SingDrawOscilloscope(425 + 10*ScreenX, 55, 180, 40, 1);
      end;
      if ScreenAct = 2 then
      begin
        SingDrawOscilloscope(190 + 10*ScreenX, 55, 180, 40, 2);
        SingDrawOscilloscope(425 + 10*ScreenX, 55, 180, 40, 3);
      end;
    end;

    if PlayersPlay = 3 then
    begin
      SingDrawOscilloscope( 75 + 10*ScreenX, 95, 100, 20, 0);
      SingDrawOscilloscope(370 + 10*ScreenX, 95, 100, 20, 1);
      SingDrawOscilloscope(670 + 10*ScreenX, 95, 100, 20, 2);
    end;

    if PlayersPlay = 6 then
    begin
      if ScreenAct = 1 then
      begin
        SingDrawOscilloscope( 75 + 10*ScreenX, 95, 100, 20, 0);
        SingDrawOscilloscope(370 + 10*ScreenX, 95, 100, 20, 1);
        SingDrawOscilloscope(670 + 10*ScreenX, 95, 100, 20, 2);
      end;
      if ScreenAct = 2 then
      begin
        SingDrawOscilloscope( 75 + 10*ScreenX, 95, 100, 20, 3);
        SingDrawOscilloscope(370 + 10*ScreenX, 95, 100, 20, 4);
        SingDrawOscilloscope(670 + 10*ScreenX, 95, 100, 20, 5);
      end;
    end;

  end;

  // Set the note heights according to the difficulty level
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

  // Draw the Notes
  if PlayersPlay = 1 then
  begin
    SingDrawPlayerBGLine(NR.Left + 20, Skin_P2_NotesB, NR.Right - 20, 0, 0, 15);  // Background glow    - colorized in playercolor
    SingDrawLine(NR.Left + 20, Skin_P2_NotesB, NR.Right - 20, 0, 15);             // Plain unsung notes - colorized in playercolor
    SingDrawPlayerLine(NR.Left + 20, Skin_P2_NotesB, NR.Width - 40, 0, 15);       // imho the sung notes
  end;

  if PlayersPlay = 2 then
  begin
    SingDrawPlayerBGLine(NR.Left + 20, Skin_P1_NotesB, NR.Right - 20, 0, 0, 15);
    SingDrawPlayerBGLine(NR.Left + 20, Skin_P2_NotesB, NR.Right - 20, 0, 1, 15);

    SingDrawLine(NR.Left + 20, Skin_P1_NotesB, NR.Right - 20, 0, 15);
    SingDrawLine(NR.Left + 20, Skin_P2_NotesB, NR.Right - 20, 1, 15);

    SingDrawPlayerLine(NR.Left + 20, Skin_P1_NotesB, NR.Width - 40, 0, 15);
    SingDrawPlayerLine(NR.Left + 20, Skin_P2_NotesB, NR.Width - 40, 1, 15);
  end;

  if PlayersPlay = 3 then
  begin
    NotesW := NotesW * 0.8;
    NotesH := NotesH * 0.8;

    SingDrawPlayerBGLine(NR.Left + 20, 120+95, NR.Right - 20, 0, 0, 12);
    SingDrawPlayerBGLine(NR.Left + 20, 245+95, NR.Right - 20, 0, 1, 12);
    SingDrawPlayerBGLine(NR.Left + 20, 370+95, NR.Right - 20, 0, 2, 12);

    SingDrawLine(NR.Left + 20, 120+95, NR.Right - 20, 0, 12);
    SingDrawLine(NR.Left + 20, 245+95, NR.Right - 20, 1, 12);
    SingDrawLine(NR.Left + 20, 370+95, NR.Right - 20, 2, 12);

    SingDrawPlayerLine(NR.Left + 20, 120+95, NR.Width - 40, 0, 12);
    SingDrawPlayerLine(NR.Left + 20, 245+95, NR.Width - 40, 1, 12);
    SingDrawPlayerLine(NR.Left + 20, 370+95, NR.Width - 40, 2, 12);
  end;

  if PlayersPlay = 4 then
  begin
    if ScreenAct = 1 then
    begin
      SingDrawPlayerBGLine(NR.Left + 20, Skin_P1_NotesB, NR.Right - 20, 0, 0, 15);
      SingDrawPlayerBGLine(NR.Left + 20, Skin_P2_NotesB, NR.Right - 20, 0, 1, 15);
    end;
    if ScreenAct = 2 then
    begin
      SingDrawPlayerBGLine(NR.Left + 20, Skin_P1_NotesB, NR.Right - 20, 0, 2, 15);
      SingDrawPlayerBGLine(NR.Left + 20, Skin_P2_NotesB, NR.Right - 20, 0, 3, 15);
    end;

    if ScreenAct = 1 then
    begin
      SingDrawLine(NR.Left + 20, Skin_P1_NotesB, NR.Right - 20, 0, 15);
      SingDrawLine(NR.Left + 20, Skin_P2_NotesB, NR.Right - 20, 1, 15);
    end;
    if ScreenAct = 2 then
    begin
      SingDrawLine(NR.Left + 20, Skin_P1_NotesB, NR.Right - 20, 2, 15);
      SingDrawLine(NR.Left + 20, Skin_P2_NotesB, NR.Right - 20, 3, 15);
    end;

    if ScreenAct = 1 then
    begin
      SingDrawPlayerLine(NR.Left + 20, Skin_P1_NotesB, NR.Width - 40, 0, 15);
      SingDrawPlayerLine(NR.Left + 20, Skin_P2_NotesB, NR.Width - 40, 1, 15);
    end;
    if ScreenAct = 2 then
    begin
      SingDrawPlayerLine(NR.Left + 20, Skin_P1_NotesB, NR.Width - 40, 2, 15);
      SingDrawPlayerLine(NR.Left + 20, Skin_P2_NotesB, NR.Width - 40, 3, 15);
    end;
  end;

  if PlayersPlay = 6 then
  begin
    NotesW := NotesW * 0.8;
    NotesH := NotesH * 0.8;

    if ScreenAct = 1 then
    begin
      SingDrawPlayerBGLine(NR.Left + 20, 120+95, NR.Right - 20, 0, 0, 12);
      SingDrawPlayerBGLine(NR.Left + 20, 245+95, NR.Right - 20, 0, 1, 12);
      SingDrawPlayerBGLine(NR.Left + 20, 370+95, NR.Right - 20, 0, 2, 12);
    end;
    if ScreenAct = 2 then
    begin
      SingDrawPlayerBGLine(NR.Left + 20, 120+95, NR.Right - 20, 0, 3, 12);
      SingDrawPlayerBGLine(NR.Left + 20, 245+95, NR.Right - 20, 0, 4, 12);
      SingDrawPlayerBGLine(NR.Left + 20, 370+95, NR.Right - 20, 0, 5, 12);
    end;

    if ScreenAct = 1 then
    begin
      SingDrawLine(NR.Left + 20, 120+95, NR.Right - 20, 0, 12);
      SingDrawLine(NR.Left + 20, 245+95, NR.Right - 20, 1, 12);
      SingDrawLine(NR.Left + 20, 370+95, NR.Right - 20, 2, 12);
    end;
    if ScreenAct = 2 then
    begin
      SingDrawLine(NR.Left + 20, 120+95, NR.Right - 20, 3, 12);
      SingDrawLine(NR.Left + 20, 245+95, NR.Right - 20, 4, 12);
      SingDrawLine(NR.Left + 20, 370+95, NR.Right - 20, 5, 12);
    end;

    if ScreenAct = 1 then
    begin
      SingDrawPlayerLine(NR.Left + 20, 120+95, NR.Width - 40, 0, 12);
      SingDrawPlayerLine(NR.Left + 20, 245+95, NR.Width - 40, 1, 12);
      SingDrawPlayerLine(NR.Left + 20, 370+95, NR.Width - 40, 2, 12);
    end;
    if ScreenAct = 2 then
    begin
      SingDrawPlayerLine(NR.Left + 20, 120+95, NR.Width - 40, 3, 12);
      SingDrawPlayerLine(NR.Left + 20, 245+95, NR.Width - 40, 4, 12);
      SingDrawPlayerLine(NR.Left + 20, 370+95, NR.Width - 40, 5, 12);
    end;
  end;
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

{//SingBar Mod
procedure SingDrawSingbar(X, Y, W, H: real; Percent: integer);
var
  R: real;
  G: real;
  B: real;
  A: cardinal;
  I: integer;

begin;

   //SingBar Background
  glColor4f(1, 1, 1, 0.8);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindTexture(GL_TEXTURE_2D, Tex_SingBar_Back.TexNum);
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex2f(X, Y);
    glTexCoord2f(0, 1); glVertex2f(X, Y+H);
    glTexCoord2f(1, 1); glVertex2f(X+W, Y+H);
    glTexCoord2f(1, 0); glVertex2f(X+W, Y);
  glEnd;

  //SingBar coloured Bar
  case Percent of
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
    end; //case

  glColor4f(R, G, B, 1);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindTexture(GL_TEXTURE_2D, Tex_SingBar_Bar.TexNum);
  //Size= Player[PlayerNum].ScorePercent of W
    glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex2f(X, Y);
    glTexCoord2f(0, 1); glVertex2f(X, Y+H);
    glTexCoord2f(1, 1); glVertex2f(X+(W/100 * (Percent +1)), Y+H);
    glTexCoord2f(1, 0); glVertex2f(X+(W/100 * (Percent +1)), Y);
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

//PhrasenBonus - Line Bonus Pop Up
procedure SingDrawLineBonus(const X, Y: Single; Color: TRGB; Alpha: Single; Text: string; Age: integer);
var
  Length, X2: real; //Length of Text
  Size: integer;    //Size of Popup
begin
  if Alpha <> 0 then
  begin

//Set Font Propertys
    SetFontStyle(2); //Font: Outlined1
    if Age < 5 then
      SetFontSize((Age + 1) * 3)
    else
      SetFontSize(18);
    SetFontItalic(False);

//Check Font Size
    Length := glTextWidth (Text) + 3; //Little Space for a Better Look ^^

//Text
    SetFontPos (X + 50 - (Length / 2), Y + 12); //Position

    if Age < 5 then
      Size := Age * 10
    else
      Size := 50;

//Draw  Background
//    glColor4f(Color.R, Color.G, Color.B, Alpha); //Set Color
    glColor4f(1, 1, 1, Alpha);

    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
//    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

//New Method, Not Variable
    glBindTexture(GL_TEXTURE_2D, Tex_SingLineBonusBack[2].TexNum);

    glBegin(GL_QUADS);
      glTexCoord2f(0, 0); glVertex2f(X + 50 - Size, Y + 25 - (Size/2));
      glTexCoord2f(0, 1); glVertex2f(X + 50 - Size, Y + 25 + (Size/2));
      glTexCoord2f(1, 1); glVertex2f(X + 50 + Size, Y + 25 + (Size/2));
      glTexCoord2f(1, 0); glVertex2f(X + 50 + Size, Y + 25 - (Size/2));
    glEnd;

    glColor4f(1, 1, 1, Alpha); //Set Color
//Draw Text
    glPrint (Text);
  end;
end;
//PhrasenBonus - Line Bonus Mod}

// Draw Note Bars for Editor
// There are 11 reasons for a new procedure:   (nice binary :D )
// 1. It does not look good when you draw the golden note star effect in the editor
// 2. You can see the freestyle notes in the editor semitransparent
// 3. It is easier and faster then changing the old procedure
procedure EditDrawLine(Left, Top, Right: real; NrLines: integer; Space: integer);
var
  Rec:   TRecR;
  Count: integer;
  TempR: real;
begin
  glColor3f(1, 1, 1);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  TempR := (Right-Left) / (Lines[NrLines].Line[Lines[NrLines].Current].End_ - Lines[NrLines].Line[Lines[NrLines].Current].Note[0].Start);
  with Lines[NrLines].Line[Lines[NrLines].Current] do
  begin
    for Count := 0 to HighNote do
    begin
      with Note[Count] do
      begin

        // Golden Note Patch
        case NoteType of
          ntFreestyle: glColor4f(1, 1, 1, 0.35);
          ntNormal: glColor4f(1, 1, 1, 0.85);
          ntGolden: Glcolor4f(1, 1, 0.3, 0.85);
        end; // case

        // left part
        Rec.Left  := (Start-Lines[NrLines].Line[Lines[NrLines].Current].Note[0].Start) * TempR + Left + 0.5 + 10*ScreenX;
        Rec.Right := Rec.Left + NotesW;
        Rec.Top := Top - (Tone-BaseNote)*Space/2 - NotesH;
        Rec.Bottom := Rec.Top + 2 * NotesH;
        glBindTexture(GL_TEXTURE_2D, Tex_Left[Color].TexNum);
        glBegin(GL_QUADS);
          glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
          glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
          glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
          glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
        glEnd;

        // middle part
        Rec.Left  := Rec.Right;
        Rec.Right := (Start+Length-Lines[NrLines].Line[Lines[NrLines].Current].Note[0].Start) * TempR + Left - NotesW - 0.5 + 10*ScreenX;

        glBindTexture(GL_TEXTURE_2D, Tex_Mid[Color].TexNum);
        glBegin(GL_QUADS);
          glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
          glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
          glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
          glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
        glEnd;

        // right part
        Rec.Left  := Rec.Right;
        Rec.Right := Rec.Right + NotesW;

        glBindTexture(GL_TEXTURE_2D, Tex_Right[Color].TexNum);
        glBegin(GL_QUADS);
          glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
          glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
          glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
          glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
        glEnd;

      end; // with
    end; // for
  end; // with

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure SingDrawTimeBar();
var
  x, y:           real;
  width, height:  real;
  LyricsProgress: real;
  CurLyricsTime:  real;
begin
  x := Theme.Sing.StaticTimeProgress.x;
  y := Theme.Sing.StaticTimeProgress.y;

  width  := Theme.Sing.StaticTimeProgress.w;
  height := Theme.Sing.StaticTimeProgress.h;

  glColor4f(Theme.Sing.StaticTimeProgress.ColR,
            Theme.Sing.StaticTimeProgress.ColG,
            Theme.Sing.StaticTimeProgress.ColB, 1); //Set Color

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);

  glBindTexture(GL_TEXTURE_2D, Tex_TimeProgress.TexNum);

  glBegin(GL_QUADS);
    glTexCoord2f(0, 0);
    glVertex2f(x, y);

    CurLyricsTime := LyricsState.GetCurrentTime();
    if (CurLyricsTime > 0) and
       (LyricsState.TotalTime > 0) then
    begin
      LyricsProgress := CurLyricsTime / LyricsState.TotalTime;
      glTexCoord2f((width * LyricsProgress) / 8, 0);
      glVertex2f(x + width * LyricsProgress, y);

      glTexCoord2f((width * LyricsProgress) / 8, 1);
      glVertex2f(x + width * LyricsProgress, y + height);
    end;

    glTexCoord2f(0, 1);
    glVertex2f(x, y + height);
  glEnd;

 glDisable(GL_TEXTURE_2D);
 glDisable(GL_BLEND);
 glcolor4f(1, 1, 1, 1);
end;

end.

