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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/base/UDraw.pas $
 * $Id: UDraw.pas 2514 2010-06-13 10:57:33Z tobigun $
 *}

unit UDraw;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UCommon,
  UThemes,
  sdl2,
  UGraphicClasses,
  UIni;

procedure SingDraw;
procedure SingDrawLines;
procedure SingDrawBackground;
procedure SingDrawOscilloscopes;
procedure SingDrawOscilloscope(Position: TThemePosition; NrSound: integer);
procedure SingDrawNoteLines(Left, Top, Right: real; LineSpacing: integer = 15);
procedure SingDrawLyricHelper(CP: integer; Left, LyricsMid: real);
procedure SingDrawLine(Left, Top, Right: real; Track, PlayerNumber: integer; LineSpacing: integer = 15);
procedure SingDrawPlayerLine(X, Y, W: real; Track, PlayerIndex: integer; LineSpacing: integer = 15);
procedure SingDrawPlayerBGLine(Left, Top, Right: real; Track, PlayerIndex: integer; LineSpacing: integer = 15);

//Draw Editor NoteLines
procedure EditDrawLine(X, YBaseNote, W, H: real; Track: integer; NumLines: integer = 10);
procedure EditDrawBorderedBox(X, Y, W, H: integer; FillR: real = 0.9; FillG: real = 0.9; FillB: real = 0.9; FillAlpha: real = 0.5);
procedure EditDrawBeatDelimiters(X, Y, W, H: real; Track: integer);

// Draw Jukebox
procedure SingDrawJukebox;
procedure SingDrawJukeboxBackground;
procedure SingDrawJukeboxBlackBackground;
procedure SingDrawJukeboxTimeBar();
procedure SingDrawLyricHelperJukebox(Left, LyricsMid: real);

// Draw Webcam
procedure SingDrawWebCamFrame;

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
  NotesW:   array [0..UIni.IMaxPlayerCount-1] of real;
  NotesH:   array [0..UIni.IMaxPlayerCount-1] of real;
  Starfr:   integer;
  StarfrG:  integer;

  //SingBar
  TickOld:  cardinal;
  TickOld2: cardinal;

  FrameThread:  PSDL_Thread;
  Mutex:        PSDL_Mutex;

implementation

uses
  SysUtils,
  Math,
  dglOpenGL,
  opencv_core,
  TextGL,
  UDrawTexture,
  UGraphic,
  ULog,
  ULyrics,
  UNote,
  UParty,
  UMusic,
  URecord,
  UScreenSingController,
  UScreenJukebox,
  USong,
  UTexture,
  UWebcam;


procedure SingDrawWebCamFrame;
var
  status: integer;
  TextureCam2: PTexture;
begin

  Webcam.GetWebcamFrame;

  if (Webcam.TextureCam.TexNum > 0) then
  begin
    glColor4f(1, 1, 1, 1);
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
    glEnable(GL_TEXTURE_2D);

    glBindTexture(GL_TEXTURE_2D, Webcam.TextureCam.TexNum);
    glEnable(GL_BLEND);
    glBegin(GL_QUADS);

      glTexCoord2f(0, 0);
      glVertex2f(800,  0);
      glTexCoord2f(0, Webcam.TextureCam.TexH);
      glVertex2f(800,  600);
      glTexCoord2f( Webcam.TextureCam.TexW, Webcam.TextureCam.TexH);
      glVertex2f(0, 600);
      glTexCoord2f( Webcam.TextureCam.TexW, 0);
      glVertex2f(0, 0);

    glEnd;
    glDisable(GL_TEXTURE_2D);
    glDisable(GL_BLEND);

    // reset to default
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

  end;

  {
  // Save Frame to AVI
  if (ScreenSing.WebcamSave) then
  begin
    cvCvtColor(WebcamFrame, WebcamFrame, CV_RGB2BGR);
    //cvInvert(WebcamFrame, WebcamFrame, 0);
    cvWriteFrame(ScreenSing.WebCamVideoWriter, WebcamFrame);
  end;
  }

end;

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

procedure SingDrawJukeboxBackground;
var
  Rec:    TRecR;
  TexRec: TRecR;
begin
  if (ScreenJukebox.Tex_Background.TexNum > 0) then
  begin
    if (Ini.MovieSize <= 1) then  //HalfSize BG
    begin
      (* half screen + gradient *)
      Rec.Top := 110; // 80
      Rec.Bottom := Rec.Top + 20;
      Rec.Left  := 0;
      Rec.Right := 800;

      TexRec.Top := (Rec.Top / 600) * ScreenJukebox.Tex_Background.TexH;
      TexRec.Bottom := (Rec.Bottom / 600) * ScreenJukebox.Tex_Background.TexH;
      TexRec.Left := 0;
      TexRec.Right := ScreenJukebox.Tex_Background.TexW;

      glEnable(GL_TEXTURE_2D);
      glBindTexture(GL_TEXTURE_2D, ScreenJukebox.Tex_Background.TexNum);
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
        TexRec.Bottom := (Rec.Bottom / 600) * ScreenJukebox.Tex_Background.TexH;
        glTexCoord2f(TexRec.Left,  TexRec.Top);    glVertex2f(Rec.Left,  Rec.Top);
        glTexCoord2f(TexRec.Left,  TexRec.Bottom); glVertex2f(Rec.Left,  Rec.Bottom);
        glTexCoord2f(TexRec.Right, TexRec.Bottom); glVertex2f(Rec.Right, Rec.Bottom);

        glTexCoord2f(TexRec.Right, TexRec.Top);    glVertex2f(Rec.Right, Rec.Top);
        (* bottom *)
        Rec.Top := Rec.Bottom;
        Rec.Bottom := 490; // 490
        TexRec.Top := TexRec.Bottom;
        TexRec.Bottom := (Rec.Bottom / 600) * ScreenJukebox.Tex_Background.TexH;
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
      glBindTexture(GL_TEXTURE_2D, ScreenJukebox.Tex_Background.TexNum);
      glEnable(GL_BLEND);
      glBegin(GL_QUADS);
        glColor4f(1, 1, 1, 1);

        glTexCoord2f(0, 0);   glVertex2f(0,  0);
        glTexCoord2f(0,  ScreenJukebox.Tex_Background.TexH);   glVertex2f(0,  600);
        glTexCoord2f( ScreenJukebox.Tex_Background.TexW,  ScreenJukebox.Tex_Background.TexH);   glVertex2f(800, 600);
        glTexCoord2f( ScreenJukebox.Tex_Background.TexW, 0);   glVertex2f(800, 0);

      glEnd;
      glDisable(GL_TEXTURE_2D);
      glDisable(GL_BLEND);
    end;
  end
  else
  begin
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, ScreenJukebox.Tex_Background.TexNum);
    //glEnable(GL_BLEND);
    glBegin(GL_QUADS);
      glColor4f(0, 0, 0, 1);

      glTexCoord2f(0, 0);   glVertex2f(0,  0);
      glTexCoord2f(0,  ScreenJukebox.Tex_Background.TexH);   glVertex2f(0,  600);
      glTexCoord2f( ScreenJukebox.Tex_Background.TexW,  ScreenJukebox.Tex_Background.TexH);   glVertex2f(800, 600);
      glTexCoord2f( ScreenJukebox.Tex_Background.TexW, 0);   glVertex2f(800, 0);

    glEnd;
    glDisable(GL_TEXTURE_2D);
    //glDisable(GL_BLEND);
  end;
end;

procedure SingDrawJukeboxBlackBackground;
var
  Rec:    TRecR;
  TexRec: TRecR;
begin
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, ScreenJukebox.Tex_Background.TexNum);
  //glEnable(GL_BLEND);
  glBegin(GL_QUADS);
    glColor4f(0, 0, 0, 1);

    glTexCoord2f(0, 0);   glVertex2f(0,  0);
    glTexCoord2f(0,  ScreenJukebox.Tex_Background.TexH);   glVertex2f(0,  600);
    glTexCoord2f( ScreenJukebox.Tex_Background.TexW,  ScreenJukebox.Tex_Background.TexH);   glVertex2f(800, 600);
    glTexCoord2f( ScreenJukebox.Tex_Background.TexW, 0);   glVertex2f(800, 0);

  glEnd;
  glDisable(GL_TEXTURE_2D);
  //glDisable(GL_BLEND);
end;

procedure SingDrawOscilloscopes;
begin;
  if PlayersPlay = 1 then
    SingDrawOscilloscope(Theme.Sing.SingP1Oscilloscope, 0);

  if PlayersPlay = 2 then
  begin
    SingDrawOscilloscope(Theme.Sing.SingP1TwoPOscilloscope, 0);
    SingDrawOscilloscope(Theme.Sing.SingP2ROscilloscope, 1);
  end;

  if PlayersPlay = 3 then
  begin
    if (CurrentSong.isDuet) then
    begin
      SingDrawOscilloscope(Theme.Sing.SingDuetP1ThreePOscilloscope, 0);
      SingDrawOscilloscope(Theme.Sing.SingDuetP2MOscilloscope, 1);
      SingDrawOscilloscope(Theme.Sing.SingDuetP3ROscilloscope, 2);
    end
    else
    begin
      SingDrawOscilloscope(Theme.Sing.SingP1ThreePOscilloscope, 0);
      SingDrawOscilloscope(Theme.Sing.SingP2MOscilloscope, 1);
      SingDrawOscilloscope(Theme.Sing.SingP3ROscilloscope, 2);
    end;
  end;

  if PlayersPlay = 4 then
  begin
    if (Ini.Screens = 1) then
    begin
      if ScreenAct = 1 then
      begin
        SingDrawOscilloscope(Theme.Sing.SingP1TwoPOscilloscope, 0);
        SingDrawOscilloscope(Theme.Sing.SingP2ROscilloscope, 1);
      end;
      if ScreenAct = 2 then
      begin
        SingDrawOscilloscope(Theme.Sing.SingP1TwoPOscilloscope, 2);
        SingDrawOscilloscope(Theme.Sing.SingP2ROscilloscope, 3);
      end;
    end
    else
    begin
      if (CurrentSong.isDuet) then
      begin
        SingDrawOscilloscope(Theme.Sing.SingP1DuetFourPOscilloscope, 0);
        SingDrawOscilloscope(Theme.Sing.SingP2DuetFourPOscilloscope, 1);
        SingDrawOscilloscope(Theme.Sing.SingP3DuetFourPOscilloscope, 2);
        SingDrawOscilloscope(Theme.Sing.SingP4DuetFourPOscilloscope, 3);
      end
      else
      begin
        SingDrawOscilloscope(Theme.Sing.SingP1FourPOscilloscope, 0);
        SingDrawOscilloscope(Theme.Sing.SingP2FourPOscilloscope, 1);
        SingDrawOscilloscope(Theme.Sing.SingP3FourPOscilloscope, 2);
        SingDrawOscilloscope(Theme.Sing.SingP4FourPOscilloscope, 3);
      end;
    end;
  end;

  if PlayersPlay = 6 then
  begin
    if (Ini.Screens = 1) then
    begin
      if (CurrentSong.isDuet) then
      begin
        if ScreenAct = 1 then
        begin
          SingDrawOscilloscope(Theme.Sing.SingDuetP1ThreePOscilloscope, 0);
          SingDrawOscilloscope(Theme.Sing.SingDuetP2MOscilloscope, 1);
          SingDrawOscilloscope(Theme.Sing.SingDuetP3ROscilloscope, 2);
        end;
        if ScreenAct = 2 then
        begin
          SingDrawOscilloscope(Theme.Sing.SingDuetP1ThreePOscilloscope, 3);
          SingDrawOscilloscope(Theme.Sing.SingDuetP2MOscilloscope, 4);
          SingDrawOscilloscope(Theme.Sing.SingDuetP3ROscilloscope, 5);
        end;
      end
      else
      begin
        if ScreenAct = 1 then
        begin
          SingDrawOscilloscope(Theme.Sing.SingP1ThreePOscilloscope, 0);
          SingDrawOscilloscope(Theme.Sing.SingP2MOscilloscope, 1);
          SingDrawOscilloscope(Theme.Sing.SingP3ROscilloscope, 2);
        end;

        if ScreenAct = 2 then
        begin
          SingDrawOscilloscope(Theme.Sing.SingP1ThreePOscilloscope, 3);
          SingDrawOscilloscope(Theme.Sing.SingP2MOscilloscope, 4);
          SingDrawOscilloscope(Theme.Sing.SingP3ROscilloscope, 5);
        end;
      end;
    end
    else
    begin
      if (CurrentSong.isDuet) then
      begin
        SingDrawOscilloscope(Theme.Sing.SingP1DuetSixPOscilloscope, 0);
        SingDrawOscilloscope(Theme.Sing.SingP2DuetSixPOscilloscope, 1);
        SingDrawOscilloscope(Theme.Sing.SingP3DuetSixPOscilloscope, 2);
        SingDrawOscilloscope(Theme.Sing.SingP4DuetSixPOscilloscope, 3);
        SingDrawOscilloscope(Theme.Sing.SingP5DuetSixPOscilloscope, 4);
        SingDrawOscilloscope(Theme.Sing.SingP6DuetSixPOscilloscope, 5);
      end
      else
      begin
        SingDrawOscilloscope(Theme.Sing.SingP1SixPOscilloscope, 0);
        SingDrawOscilloscope(Theme.Sing.SingP2SixPOscilloscope, 1);
        SingDrawOscilloscope(Theme.Sing.SingP3SixPOscilloscope, 2);
        SingDrawOscilloscope(Theme.Sing.SingP4SixPOscilloscope, 3);
        SingDrawOscilloscope(Theme.Sing.SingP5SixPOscilloscope, 4);
        SingDrawOscilloscope(Theme.Sing.SingP6SixPOscilloscope, 5);
      end;
    end;
  end;
end;

procedure SingDrawOscilloscope(Position: TThemePosition; NrSound: integer);
var
  SampleIndex: integer;
  Sound:       TCaptureBuffer;
  MaxX, MaxY:  real;
  Col: TRGB;
begin;
  Sound := AudioInputProcessor.Sound[NrSound];

  //  Log.LogStatus('Oscilloscope', 'SingDraw');
  //glColor3f(Skin_OscR, Skin_OscG, Skin_OscB);

  if (Party.bPartyGame) then
    Col := GetPlayerColor(Ini.TeamColor[NrSound])
  else
    Col := GetPlayerColor(Ini.PlayerColor[NrSound]);

  glColor3f(Col.R, Col.G, Col.B);
{
  if (ParamStr(1) = '-black') or (ParamStr(1) = '-fsblack') then
    glColor3f(1, 1, 1);
}
  MaxX := Position.W-1;
  MaxY := (Position.H-1) / 2;

  Sound.LockAnalysisBuffer();

  glBegin(GL_LINE_STRIP);
    for SampleIndex := 0 to High(Sound.AnalysisBuffer) do
    begin
      glVertex2f(Position.X + MaxX * SampleIndex/High(Sound.AnalysisBuffer),
                 Position.Y + MaxY * (1 - Sound.AnalysisBuffer[SampleIndex]/-Low(Smallint)));
    end;
  glEnd;

  Sound.UnlockAnalysisBuffer();
end;

procedure SingDrawNoteLines(Left, Top, Right: real; LineSpacing: integer);
var
  Count: integer;
begin
  glEnable(GL_BLEND);
  glColor4f(Skin_P1_LinesR, Skin_P1_LinesG, Skin_P1_LinesB, 0.4);
  glBegin(GL_LINES);
  for Count := 0 to 9 do
  begin
    glVertex2f(Left,  Top + Count * LineSpacing);
    glVertex2f(Right, Top + Count * LineSpacing);
  end;
  glEnd;
  glDisable(GL_BLEND);
end;

// draw blank Notebars
procedure SingDrawLine(Left, Top, Right: real; Track, PlayerNumber: integer; LineSpacing: integer);
var
  Rec:   TRecR;
  Count: integer;
  TempR: real;

  GoldenStarPos: real;
begin
// We actually don't have a playernumber in this procedure, it should reside in Track - but it is always set to zero
// So we exploit this behavior a bit - we give Track the playernumber, keep it in playernumber - and then we set Track to zero
// This could also come quite in handy when we do the duet mode, cause just the notes for the player that has to sing should be drawn then
// BUT this is not implemented yet, all notes are drawn! :D
  if (ScreenSing.settings.NotesVisible and (1 shl Track) <> 0) then
  begin
    //PlayerNumber := Track + 1; // Player 1 is 0

    // exploit done

    glColor3f(1, 1, 1);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    if not Tracks[Track].Lines[Tracks[Track].CurrentLine].HasLength(TempR) then TempR := 0
    else TempR := (Right-Left) / TempR;


    with Tracks[Track].Lines[Tracks[Track].CurrentLine] do
    begin
      for Count := 0 to HighNote do
      begin
        with Notes[Count] do
        begin
          if NoteType <> ntFreestyle then
          begin
            if Ini.EffectSing = 0 then
              // If Golden note Effect of then Change not Color
            begin
              case NoteType of
                ntNormal: glColor4f(1, 1, 1, 1);   // We set alpha to 1, cause we can control the transparency through the png itself
                ntGolden: glColor4f(1, 1, 0.3, 1); // no stars, paint yellow -> glColor4f(1, 1, 0.3, 0.85); - we could
                ntRap:    glColor4f(1, 1, 1, 1);
                ntRapGolden: glColor4f(1, 1, 0.3, 1);
            end; // case
            end //Else all Notes same Color
            else
              glColor4f(1, 1, 1, 1);        // We set alpha to 1, cause we can control the transparency through the png itself

            // left part
            Rec.Left  := (StartBeat - Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + Left + 0.5 + 10*ScreenX;
            Rec.Right := Rec.Left + NotesW[PlayerNumber - 1];
            Rec.Top := Top - (Tone-BaseNote)*LineSpacing/2 - NotesH[PlayerNumber - 1];
            Rec.Bottom := Rec.Top + 2 * NotesH[PlayerNumber - 1];
            If (NoteType = ntRap) or (NoteType = ntRapGolden) then
            begin
              glBindTexture(GL_TEXTURE_2D, Tex_plain_Left_Rap[PlayerNumber].TexNum);
            end
            else
            begin
              glBindTexture(GL_TEXTURE_2D, Tex_plain_Left[PlayerNumber].TexNum);
            end;
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
            Rec.Right := (StartBeat + Duration - Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + Left - NotesW[PlayerNumber - 1] - 0.5 + 10*ScreenX;

            // the left note is more right than the right note itself, sounds weird - so we fix that xD
            if Rec.Right <= Rec.Left then
              Rec.Right := Rec.Left;

            If (NoteType = ntRap) or (NoteType = ntRapGolden) then
            begin
              glBindTexture(GL_TEXTURE_2D, Tex_plain_Mid_Rap[PlayerNumber].TexNum);
            end
            else
            begin
              glBindTexture(GL_TEXTURE_2D, Tex_plain_Mid[PlayerNumber].TexNum);
            end;
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
            Rec.Right := Rec.Right + NotesW[PlayerNumber - 1];


            if (NoteType = ntRap) or (NoteType = ntRapGolden) then
            begin
              glBindTexture(GL_TEXTURE_2D, Tex_plain_Right_Rap[PlayerNumber].TexNum);
            end
            else
            begin
              glBindTexture(GL_TEXTURE_2D, Tex_plain_Right[PlayerNumber].TexNum);
            end;
            glBegin(GL_QUADS);
              glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
              glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
              glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
              glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
            glEnd;

            // Golden Star Patch
            if ((NoteType = ntGolden) or (NoteType = ntRapGolden)) and (Ini.EffectSing=1) then
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
procedure SingDrawPlayerLine(X, Y, W: real; Track, PlayerIndex: integer; LineSpacing: integer);
var
  TempR:      real;
  Rec:        TRecR;
  N: integer;
//  R, G, B, A: real;
  NotesH2:    real;
begin
  if (ScreenSing.Settings.InputVisible) then
  begin
    //Log.LogStatus('Player notes', 'SingDraw');

    glColor3f(1, 1, 1);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    //if Player[NrGracza].LengthNote > 0 then
    begin
      if not Tracks[Track].Lines[Tracks[Track].CurrentLine].HasLength(TempR) then TempR := 0
      else TempR := W / TempR;

      for N := 0 to Player[PlayerIndex].HighNote do
      begin
        with Player[PlayerIndex].Note[N] do
        begin
          // Left part of note
          Rec.Left  := X + (Start - Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + 0.5 + 10*ScreenX;
          Rec.Right := Rec.Left + NotesW[PlayerIndex];

          // Draw it in half size, if not hit
          if Hit then
          begin
            NotesH2 := NotesH[PlayerIndex]
          end
          else
          begin
            NotesH2 := int(NotesH[PlayerIndex] * 0.65);
          end;

          Rec.Top    := Y - (Tone-Tracks[Track].Lines[Tracks[Track].CurrentLine].BaseNote)*LineSpacing/2 - NotesH2;
          Rec.Bottom := Rec.Top + 2 * NotesH2;

          // draw the left part
          glColor3f(1, 1, 1);
          If (NoteType = ntRap) or (NoteType = ntRapGolden) then
          begin
            glBindTexture(GL_TEXTURE_2D, Tex_Left_Rap[PlayerIndex+1].TexNum);
          end
          else
          begin
            glBindTexture(GL_TEXTURE_2D, Tex_Left[PlayerIndex+1].TexNum);
          end;
          glBegin(GL_QUADS);
            glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
            glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
            glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
            glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
          glEnd;

          // Middle part of the note
          Rec.Left  := Rec.Right;
          Rec.Right := X + (Start + Duration - Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR - NotesW[PlayerIndex] - 0.5  + 10*ScreenX;

          // new
          if (Start + Duration - 1 = LyricsState.CurrentBeatD) then
            Rec.Right := Rec.Right - (1-Frac(LyricsState.MidBeatD)) * TempR;

          // the left note is more right than the right note itself, sounds weird - so we fix that xD
          if Rec.Right <= Rec.Left then
            Rec.Right := Rec.Left;

          // draw the middle part
          If (NoteType = ntRap) or (NoteType = ntRapGolden) then
          begin
            glBindTexture(GL_TEXTURE_2D, Tex_Mid_Rap[PlayerIndex+1].TexNum);
          end
          else
          begin
            glBindTexture(GL_TEXTURE_2D, Tex_Mid[PlayerIndex+1].TexNum);
          end;
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
          Rec.Right := Rec.Right + NotesW[PlayerIndex];

          If (NoteType = ntRap) or (NoteType = ntRapGolden) then
          begin
            glBindTexture(GL_TEXTURE_2D, Tex_Right_Rap[PlayerIndex+1].TexNum);
          end
          else
          begin
          glBindTexture(GL_TEXTURE_2D, Tex_Right[PlayerIndex+1].TexNum);
          end;
          glBegin(GL_QUADS);
            glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
            glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
            glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
            glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
          glEnd;

          // Perfect note is stored
          if Perfect and (Ini.EffectSing=1) then
          begin
            //A := 1 - 2*(LyricsState.GetCurrentTime() - GetTimeFromBeat(Start + Duration));
            if not (Start + Duration - 1 = LyricsState.CurrentBeatD) then
            begin
              //Star animation counter
              //inc(Starfr);
              //Starfr := Starfr mod 128;
             // if not(CurrentSong.isDuet) or (PlayerIndex mod 2 = Track) then
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
  end; // if
end;

//draw Note glow
procedure SingDrawPlayerBGLine(Left, Top, Right: real; Track, PlayerIndex: integer; LineSpacing: integer);
var
  Rec:            TRecR;
  Count:          integer;
  TempR:          real;
  W, H:           real;
begin
  if (ScreenSing.settings.NotesVisible and (1 shl PlayerIndex) <> 0) then
  begin
    //glColor4f(1, 1, 1, sqrt((1+sin( AudioPlayback.Position * 3))/4)/ 2 + 0.5 );
    glColor4f(1, 1, 1, sqrt((1 + sin(AudioPlayback.Position * 3)))/2 + 0.05);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    if not Tracks[Track].Lines[Tracks[Track].CurrentLine].HasLength(TempR) then TempR := 0
    else TempR := (Right-Left) / TempR;

    with Tracks[Track].Lines[Tracks[Track].CurrentLine] do
    begin
      for Count := 0 to HighNote do
      begin
        with Notes[Count] do
        begin
          if NoteType <> ntFreestyle then
          begin
            // begin: 14, 20
            // easy: 6, 11
            W := NotesW[PlayerIndex] * 2 + 2;
            H := NotesH[PlayerIndex] * 1.5 + 3.5;

            {
            X2 := (Start-Tracks[Track].Lines[Tracks[Track].Current].Notes[0].Start) * TempR + Left + 0.5 + 10*ScreenX + 4;
            X1 := X2-W;

            X3 := (Start+Length-Tracks[Track].Lines[Tracks[Track].Current].Notes[0].Start) * TempR + Left - 0.5 + 10*ScreenX - 4;
            X4 := X3+W;
            }

            // left
            Rec.Right := (StartBeat - Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + Left + 0.5 + 10*ScreenX + 4;
            Rec.Left  := Rec.Right - W;
            Rec.Top := Top - (Tone-BaseNote)*LineSpacing/2 - H;
            Rec.Bottom := Rec.Top + 2 * H;

            If (NoteType = ntRap) or (NoteType = ntRapGolden) then
            begin
              glBindTexture(GL_TEXTURE_2D, Tex_BG_Left_Rap[PlayerIndex+1].TexNum);
            end
            else
            begin
              glBindTexture(GL_TEXTURE_2D, Tex_BG_Left[PlayerIndex+1].TexNum);
            end;
            glBegin(GL_QUADS);
              glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
              glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
              glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
              glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
            glEnd;

            // middle part
            Rec.Left  := Rec.Right;
            Rec.Right := (StartBeat + Duration - Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + Left - 0.5 + 10*ScreenX - 4;

            // the left note is more right than the right note itself, sounds weird - so we fix that xD
            if Rec.Right <= Rec.Left then
              Rec.Right := Rec.Left;

            If (NoteType = ntRap) or (NoteType = ntRapGolden) then
            begin
              glBindTexture(GL_TEXTURE_2D, Tex_BG_Mid_Rap[PlayerIndex+1].TexNum);
            end
            else
            begin
              glBindTexture(GL_TEXTURE_2D, Tex_BG_Mid[PlayerIndex+1].TexNum);
            end;
            glBegin(GL_QUADS);
              glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
              glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
              glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
              glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
            glEnd;

            // right part
            Rec.Left  := Rec.Right;
            Rec.Right := Rec.Left + W;

            If (NoteType = ntRap) or (NoteType = ntRapGolden) then
            begin
              glBindTexture(GL_TEXTURE_2D, Tex_BG_Right_Rap[PlayerIndex+1].TexNum);
            end
            else
            begin
              glBindTexture(GL_TEXTURE_2D, Tex_BG_Right[PlayerIndex+1].TexNum);
            end;
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
procedure SingDrawLyricHelper(CP: integer; Left, LyricsMid: real);
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
  Col: TRGB;
const
  BarWidth  = 50; // width  of the lyric helper bar
  BarHeight = 30; // height of the lyric helper bar
  BarMoveLimit = 40; // max. number of beats remaining before the bar starts to move
begin
  // get current lyrics line and the time in beats of its first note
  CurLine := @Tracks[CP].Lines[Tracks[CP].CurrentLine];

  // FIXME: accessing ScreenSing is not that generic
  if (CurrentSong.isDuet) and (PlayersPlay <> 1) then
  begin
    if (CP = 1) then
      LyricEngine := ScreenSing.LyricsDuetP2
    else
      LyricEngine := ScreenSing.LyricsDuetP1;
  end
  else
    LyricEngine := ScreenSing.Lyrics;

  LyricEngine.FontFamily := Ini.LyricsFont;
  LyricEngine.FontStyle  := Ini.LyricsStyle;

  // do not draw the lyrics helper if the current line does not contain any note
  if (Length(CurLine.Notes) > 0) then
  begin
    // start beat of the first note of this line
    FirstNoteBeat := CurLine.Notes[0].StartBeat;
    // time in beats between the start of the current line and its first note
    FirstNoteDelta := FirstNoteBeat - CurLine.StartBeat;

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
      if (CurrentSong.isDuet) and (PlayersPlay <> 1) then
      begin
        if (CP = 0) then
          Bounds.Top := Theme.LyricBarDuetP1.IndicatorYOffset + Theme.LyricBarDuetP1.UpperY
        else
          Bounds.Top := Theme.LyricBarDuetP2.IndicatorYOffset + Theme.LyricBarDuetP2.UpperY ;
      end
      else
        Bounds.Top := Theme.LyricBar.IndicatorYOffset + Theme.LyricBar.UpperY ;

      Bounds.Bottom := Bounds.Top + BarHeight + 3;

      // draw lyric help bar
      glEnable(GL_TEXTURE_2D);
      glEnable(GL_BLEND);

      if (CurrentSong.isDuet) then
      begin
        if (PlayersPlay = 1) or (PlayersPlay = 2) then
          Col := GetLyricBarColor(Ini.SingColor[CP])
        else
        begin
          if (PlayersPlay = 3) or (PlayersPlay = 6) then
          begin
            //if (PlayersPlay = 3) then
              Col := GetLyricBarColor(Ini.SingColor[CP]);

            //if (PlayersPlay = 6) then
            //  Col := GetLyricBarColor(CP + 1);
          end
          else
          begin
            if ScreenAct = 1 then
              Col := GetLyricBarColor(Ini.SingColor[CP])
            else
              Col := GetLyricBarColor(Ini.SingColor[CP + 2]);
          end;
        end;
      end
      else
        Col := GetLyricBarColor(1);

      glColor4f(Col.R, Col.G, Col.B, BarAlpha);

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

(**
 * Draws the lyrics helper bar jukebox.
 * Left: position the bar starts at
 * LyricsMid: the middle of the lyrics relative to the position Left
 *)
procedure SingDrawLyricHelperJukebox(Left, LyricsMid: real);
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
  CurLine := @Tracks[0].Lines[Tracks[0].CurrentLine];

  // FIXME: accessing ScreenSing is not that generic
  LyricEngine := ScreenJukebox.Lyrics;

  // do not draw the lyrics helper if the current line does not contain any note
  if (Length(CurLine.Notes) > 0) then
  begin
    // start beat of the first note of this line
    FirstNoteBeat := CurLine.Notes[0].StartBeat;
    // time in beats between the start of the current line and its first note
    FirstNoteDelta := FirstNoteBeat - CurLine.StartBeat;

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
      MoveDist := LyricsMid - LineWidth/2 - BarWidth;

      if (LineWidth/2 + (BarWidth - Left) * 2 >= 400) then
        Bounds.Left := - BarProgress * MoveDist + Left - BarWidth
      else
      begin
        // if the line is too long the helper might move from right to left
        // so we have to assure the start position is left of the text.
        if (MoveDist >= 0) then
          MoveStartX := Left
        else
          MoveStartX := Left + MoveDist;

        // determine lyric help bar position and size
        Bounds.Left := MoveStartX + BarProgress * MoveDist;
      end;

      Bounds.Right := Bounds.Left + BarWidth;
      Bounds.Top := Theme.LyricBarJukebox.IndicatorYOffset + ScreenJukeBox.Lyrics.UpperLineY;
      Bounds.Bottom := Bounds.Top + BarHeight + 3;

      // draw lyric help bar
      glEnable(GL_TEXTURE_2D);
      glEnable(GL_BLEND);

      //glColor4f(1, 0.75, 0, BarAlpha);
      glColor4f(ScreenJukebox.LyricHelper.R, ScreenJukebox.LyricHelper.G, ScreenJukebox.LyricHelper.B, BarAlpha);

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

procedure SingDrawLines;
var
  NR: TRecR;         // lyrics area bounds (NR = NoteRec?)
  LyricEngine: TLyricEngine;
  LyricEngineDuet: TLyricEngine;
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

  // draw note-lines

  // to-do : needs fix when party mode works w/ 2 screens
  if (PlayersPlay = 1) and (Ini.NoteLines = 1) and (ScreenSing.settings.NotesVisible and (1) <> 0) then
    SingDrawNoteLines(NR.Left + 10*ScreenX, Skin_P2_NotesB - 105, NR.Right + 10*ScreenX, 15);

  if (PlayersPlay = 2) and (Ini.NoteLines = 1) then
  begin
    if (ScreenSing.settings.NotesVisible and (1 shl 0) <> 0) then
      SingDrawNoteLines(Nr.Left + 10*ScreenX, Skin_P1_NotesB - 105, Nr.Right + 10*ScreenX, 15);
    if (ScreenSing.settings.NotesVisible and (1 shl 1) <> 0) then
      SingDrawNoteLines(Nr.Left + 10*ScreenX, Skin_P2_NotesB - 105, Nr.Right + 10*ScreenX, 15);
  end;

  if (PlayersPlay = 4) and (Ini.NoteLines = 1) then
  begin
    if (ScreenSing.settings.NotesVisible and (1 shl 0) <> 0) then
    begin
      if (Ini.Screens = 1) then
        SingDrawNoteLines(Nr.Left + 10*ScreenX, Skin_P1_NotesB - 105, Nr.Right + 10*ScreenX, 15)
      else
      begin
        SingDrawNoteLines(Nr.Left + 10*ScreenX, Skin_P1_NotesB - 105, Nr.Right/2 - 5 + 10*ScreenX, 15);
        SingDrawNoteLines(Nr.Right/2 - 20 + 10*ScreenX + Nr.Left + 10*ScreenX, Skin_P1_NotesB - 105, Nr.Right + 10*ScreenX, 15)
      end;
    end;

    if (ScreenSing.settings.NotesVisible and (1 shl 1) <> 0) then
    begin
      if (Ini.Screens = 1) then
        SingDrawNoteLines(Nr.Left + 10*ScreenX, Skin_P2_NotesB - 105, Nr.Right + 10*ScreenX, 15)
      else
      begin
        SingDrawNoteLines(Nr.Left + 10*ScreenX, Skin_P2_NotesB - 105, Nr.Right/2 - 5 + 10*ScreenX, 15);
        SingDrawNoteLines(Nr.Right/2 - 20 + 10*ScreenX + Nr.Left + 10*ScreenX, Skin_P2_NotesB - 105, Nr.Right + 10*ScreenX, 15)
      end;
    end;
  end;

  if (PlayersPlay = 3) and (Ini.NoteLines = 1) then begin
    if (ScreenSing.settings.NotesVisible and (1 shl 0) <> 0) then
      SingDrawNoteLines(Nr.Left + 10*ScreenX, 120, Nr.Right + 10*ScreenX, 12);
    if (ScreenSing.settings.NotesVisible and (1 shl 1) <> 0) then
      SingDrawNoteLines(Nr.Left + 10*ScreenX, 245, Nr.Right + 10*ScreenX, 12);
    if (ScreenSing.settings.NotesVisible and (1 shl 2) <> 0) then
      SingDrawNoteLines(Nr.Left + 10*ScreenX, 370, Nr.Right + 10*ScreenX, 12);
  end;

  if (PlayersPlay = 6) and (Ini.NoteLines = 1) then begin
    if (ScreenSing.settings.NotesVisible and (1 shl 0) <> 0) then
    begin
      if (Ini.Screens = 1) then
        SingDrawNoteLines(Nr.Left + 10*ScreenX, 120, Nr.Right + 10*ScreenX, 12)
      else
      begin
        SingDrawNoteLines(Nr.Left + 10*ScreenX, 120, Nr.Right/2 - 5 + 10*ScreenX, 12);
        SingDrawNoteLines(Nr.Right/2 - 20 + 10*ScreenX + Nr.Left + 10*ScreenX, 120, Nr.Right + 10*ScreenX, 12);
      end;
    end;

    if (ScreenSing.settings.NotesVisible and (1 shl 1) <> 0) then
    begin
      if (Ini.Screens = 1) then
        SingDrawNoteLines(Nr.Left + 10*ScreenX, 245, Nr.Right + 10*ScreenX, 12)
      else
      begin
        SingDrawNoteLines(Nr.Left + 10*ScreenX, 245, Nr.Right/2 - 5 + 10*ScreenX, 12);
        SingDrawNoteLines(Nr.Right/2 - 20 + 10*ScreenX + Nr.Left + 10*ScreenX, 245, Nr.Right + 10*ScreenX, 12);
      end;
    end;

    if (ScreenSing.settings.NotesVisible and (1 shl 2) <> 0) then
    begin
      if (Ini.Screens = 1) then
        SingDrawNoteLines(Nr.Left + 10*ScreenX, 370, Nr.Right + 10*ScreenX, 12)
      else
      begin
        SingDrawNoteLines(Nr.Left + 10*ScreenX, 370, Nr.Right/2 - 5 + 10*ScreenX, 12);
        SingDrawNoteLines(Nr.Right/2 - 20 + 10*ScreenX + Nr.Left + 10*ScreenX, 370, Nr.Right + 10*ScreenX, 12);
      end;
    end;
  end;
end;

procedure SingDraw;
var
  NR: TRecR;         // lyrics area bounds (NR = NoteRec?)
  LyricEngine: TLyricEngine;
  LyricEngineDuetP1: TLyricEngine;
  LyricEngineDuetP2: TLyricEngine;
  I: integer;
  Difficulty: integer;
  TrackP1, TrackP2, TrackP3, TrackP4, TrackP5, TrackP6: integer;
const
  LineSpacingOneRow = 15;
  LineSpacingTwoRows = 15;
  LineSpacingThreeRows = 12;
  TopOneRow1 = Skin_P2_NotesB;
  TopTwoRows1 = Skin_P1_NotesB;
  TopTwoRows2 = Skin_P2_NotesB;
  TopThreeRows1 = 120+95;
  TopThreeRows2 = 245+95;
  TopThreeRows3 = 370+95;
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

  TrackP1 := 0;
  TrackP2 := 0;
  TrackP3 := 0;
  TrackP4 := 0;
  TrackP5 := 0;
  TrackP6 := 0;
  // FIXME: accessing ScreenSing is not that generic
  if (CurrentSong.isDuet) and (PlayersPlay <> 1) then
  begin
    LyricEngineDuetP1 := ScreenSing.LyricsDuetP1;
    LyricEngineDuetP2 := ScreenSing.LyricsDuetP2;
    TrackP2 := 1;
    TrackP4 := 1;
    TrackP6 := 1;
  end
  else
    LyricEngine := ScreenSing.Lyrics;

  // draw lyrics
  if (ScreenSing.Settings.LyricsVisible) then
  begin
    if (CurrentSong.isDuet) and (PlayersPlay <> 1) then
    begin
      LyricEngineDuetP1.Draw(LyricsState.MidBeat);
      SingDrawLyricHelper(0, NR.Left, NR.WMid);

      LyricEngineDuetP2.Draw(LyricsState.MidBeat);
      SingDrawLyricHelper(1, NR.Left, NR.WMid);
    end
    else
    begin
      LyricEngine.Draw(LyricsState.MidBeat);
      SingDrawLyricHelper(0, NR.Left, NR.WMid);
    end;
  end;

  // oscilloscope
  if (ScreenSing.Settings.OscilloscopeVisible) then
  begin
    SingDrawOscilloscopes;
  end;

  for I := 1 to PlayersPlay do
  begin

    if (ScreenSong.Mode = smNormal) then
      Difficulty := Ini.PlayerLevel[I - 1]
    else
      Difficulty := Ini.Difficulty;

    case Difficulty of
      0:
        begin
          NotesH[I - 1] := 11; // 9
          NotesW[I - 1] := 6; // 5
        end;
      1:
        begin
          NotesH[I - 1] := 8; // 7
          NotesW[I - 1] := 4; // 4
        end;
      2:
        begin
          NotesH[I - 1] := 5;
          NotesW[I - 1] := 3;
        end;
    end;

    if PlayersPlay = 3 then
    begin
      NotesW[I - 1] := NotesW[I - 1] * 0.8;
      NotesH[I - 1] := NotesH[I - 1] * 0.8;
    end;

    if PlayersPlay = 4 then
    begin
      if (Ini.Screens = 0) then
      begin
        NotesW[I - 1] := NotesW[I - 1] * 0.9;
      end;
    end;

    if PlayersPlay = 6 then
    begin
      NotesW[I - 1] := NotesW[I - 1] * 0.8;
      NotesH[I - 1] := NotesH[I - 1] * 0.8;
    end;

  end;

  // Draw the Notes
  if PlayersPlay = 1 then
  begin
    // SINGLESCREEN
    SingDrawPlayerBGLine(NR.Left + 20, TopOneRow1, NR.Right - 20, TrackP1, 0, LineSpacingOneRow);  // Background glow    - colorized in playercolor
    SingDrawLine(NR.Left + 20, TopOneRow1, NR.Right - 20, TrackP1, 1, LineSpacingOneRow);             // Plain unsung notes - colorized in playercolor
    SingDrawPlayerLine(NR.Left + 20, TopOneRow1, NR.Width - 40, TrackP1, 0, LineSpacingOneRow);       // imho the sung notes
  end;

  if PlayersPlay = 2 then
  begin
    // SINGLESCREEN
    SingDrawPlayerBGLine(NR.Left + 20, TopTwoRows1, NR.Right - 20, TrackP1, 0, LineSpacingTwoRows);
    SingDrawPlayerBGLine(NR.Left + 20, TopTwoRows2, NR.Right - 20, TrackP2, 1, LineSpacingTwoRows);

    SingDrawLine(NR.Left + 20, TopTwoRows1, NR.Right - 20, TrackP1, 1, LineSpacingTwoRows);
    SingDrawLine(NR.Left + 20, TopTwoRows2, NR.Right - 20, TrackP2, 2, LineSpacingTwoRows);

    SingDrawPlayerLine(NR.Left + 20, TopTwoRows1, NR.Width - 40, TrackP1, 0, LineSpacingTwoRows);
    SingDrawPlayerLine(NR.Left + 20, TopTwoRows2, NR.Width - 40, TrackP2, 1, LineSpacingTwoRows);
  end;

  if PlayersPlay = 3 then
  begin
    // SINGLESCREEN
    SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows1, NR.Right - 20, TrackP1, 0, LineSpacingThreeRows);
    SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows2, NR.Right - 20, TrackP2, 1, LineSpacingThreeRows);
    SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows3, NR.Right - 20, TrackP3, 2, LineSpacingThreeRows);

    SingDrawLine(NR.Left + 20, TopThreeRows1, NR.Right - 20, TrackP1, 1, LineSpacingThreeRows);
    SingDrawLine(NR.Left + 20, TopThreeRows2, NR.Right - 20, TrackP2, 2, LineSpacingThreeRows);
    SingDrawLine(NR.Left + 20, TopThreeRows3, NR.Right - 20, TrackP3, 3, LineSpacingThreeRows);

    SingDrawPlayerLine(NR.Left + 20, TopThreeRows1, NR.Width - 40, TrackP1, 0, LineSpacingThreeRows);
    SingDrawPlayerLine(NR.Left + 20, TopThreeRows2, NR.Width - 40, TrackP2, 1, LineSpacingThreeRows);
    SingDrawPlayerLine(NR.Left + 20, TopThreeRows3, NR.Width - 40, TrackP3, 2, LineSpacingThreeRows);
  end;

  if PlayersPlay = 4 then
  begin
    if (Ini.Screens = 1) then
    begin
      // MULTISCREEN
      if ScreenAct = 1 then
      begin
        // MULTISCREEN 1
        SingDrawPlayerBGLine(NR.Left + 20, TopTwoRows1, NR.Right - 20, TrackP1, 0, LineSpacingTwoRows);
        SingDrawPlayerBGLine(NR.Left + 20, TopTwoRows2, NR.Right - 20, TrackP2, 1, LineSpacingTwoRows);

        SingDrawLine(NR.Left + 20, TopTwoRows1, NR.Right - 20, TrackP1, 1, LineSpacingTwoRows);
        SingDrawLine(NR.Left + 20, TopTwoRows2, NR.Right - 20, TrackP2, 2, LineSpacingTwoRows);

        SingDrawPlayerLine(NR.Left + 20, TopTwoRows1, NR.Width - 40, TrackP1, 0, LineSpacingTwoRows);
        SingDrawPlayerLine(NR.Left + 20, TopTwoRows2, NR.Width - 40, TrackP2, 1, LineSpacingTwoRows);
      end;
      if ScreenAct = 2 then
      begin
        // MULTISCREEN 2
        SingDrawPlayerBGLine(NR.Left + 20, TopTwoRows1, NR.Right - 20, TrackP3, 2, LineSpacingTwoRows);
        SingDrawPlayerBGLine(NR.Left + 20, TopTwoRows2, NR.Right - 20, TrackP4, 3, LineSpacingTwoRows);

        SingDrawLine(NR.Left + 20, TopTwoRows1, NR.Right - 20, TrackP3, 3, LineSpacingTwoRows);
        SingDrawLine(NR.Left + 20, TopTwoRows2, NR.Right - 20, TrackP4, 4, LineSpacingTwoRows);

        SingDrawPlayerLine(NR.Left + 20, TopTwoRows1, NR.Width - 40, TrackP3, 2, LineSpacingTwoRows);
        SingDrawPlayerLine(NR.Left + 20, TopTwoRows2, NR.Width - 40, TrackP4, 3, LineSpacingTwoRows);
      end;
    end
    else
    begin
      // SINGLESCREEN
      SingDrawPlayerBGLine(NR.Left + 20, TopTwoRows1, NR.Right/2 - 20, TrackP1, 0, LineSpacingTwoRows);
      SingDrawPlayerBGLine(NR.Left + 20, TopTwoRows2, NR.Right/2 - 20, TrackP2, 1, LineSpacingTwoRows);
      SingDrawPlayerBGLine(NR.Right/2 - 20 + NR.Left + 20, TopTwoRows1, NR.Right - 20, TrackP3, 2, LineSpacingTwoRows);
      SingDrawPlayerBGLine(NR.Right/2 - 20 + NR.Left + 20, TopTwoRows2, NR.Right - 20, TrackP4, 3, LineSpacingTwoRows);

      SingDrawLine(NR.Left + 20, TopTwoRows1, NR.Right/2 - 20, TrackP1, 1, LineSpacingTwoRows);
      SingDrawLine(NR.Left + 20, TopTwoRows2, NR.Right/2 - 20, TrackP2, 2, LineSpacingTwoRows);
      SingDrawLine(NR.Right/2 - 20 + NR.Left + 20, TopTwoRows1, NR.Right - 20, TrackP3, 3, LineSpacingTwoRows);
      SingDrawLine(NR.Right/2 - 20 + NR.Left + 20, TopTwoRows2, NR.Right - 20, TrackP4, 4, LineSpacingTwoRows);

      SingDrawPlayerLine(NR.Left + 20, TopTwoRows1, NR.Width/2 - 50, TrackP1, 0, LineSpacingTwoRows);
      SingDrawPlayerLine(NR.Left + 20, TopTwoRows2, NR.Width/2 - 50, TrackP2, 1, LineSpacingTwoRows);
      SingDrawPlayerLine(NR.Width/2 - 10 + NR.Left + 20, TopTwoRows1, NR.Width/2 - 30, TrackP3, 2, LineSpacingTwoRows);
      SingDrawPlayerLine(NR.Width/2 - 10 + NR.Left + 20, TopTwoRows2, NR.Width/2 - 30, TrackP4, 3, LineSpacingTwoRows);
    end;
  end;

  if PlayersPlay = 6 then
  begin
    if (Ini.Screens = 1) then
    begin
      // MULTISCREEN
      if ScreenAct = 1 then
      begin
        // MULTISCREEN 1
        SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows1, NR.Right - 20, TrackP1, 0, LineSpacingThreeRows);
        SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows2, NR.Right - 20, TrackP2, 1, LineSpacingThreeRows);
        SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows3, NR.Right - 20, TrackP3, 2, LineSpacingThreeRows);

        SingDrawLine(NR.Left + 20, TopThreeRows1, NR.Right - 20, TrackP1, 1, LineSpacingThreeRows);
        SingDrawLine(NR.Left + 20, TopThreeRows2, NR.Right - 20, TrackP2, 2, LineSpacingThreeRows);
        SingDrawLine(NR.Left + 20, TopThreeRows3, NR.Right - 20, TrackP3, 3, LineSpacingThreeRows);

        SingDrawPlayerLine(NR.Left + 20, TopThreeRows1, NR.Width - 40, TrackP1, 0, LineSpacingThreeRows);
        SingDrawPlayerLine(NR.Left + 20, TopThreeRows2, NR.Width - 40, TrackP2, 1, LineSpacingThreeRows);
        SingDrawPlayerLine(NR.Left + 20, TopThreeRows3, NR.Width - 40, TrackP3, 2, LineSpacingThreeRows);
      end;
      if ScreenAct = 2 then
      begin
        // MULTISCREEN 2
        SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows1, NR.Right - 20, TrackP4, 3, LineSpacingThreeRows);
        SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows2, NR.Right - 20, TrackP5, 4, LineSpacingThreeRows);
        SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows3, NR.Right - 20, TrackP6, 5, LineSpacingThreeRows);

        SingDrawLine(NR.Left + 20, TopThreeRows1, NR.Right - 20, TrackP4, 4, LineSpacingThreeRows);
        SingDrawLine(NR.Left + 20, TopThreeRows2, NR.Right - 20, TrackP5, 5, LineSpacingThreeRows);
        SingDrawLine(NR.Left + 20, TopThreeRows3, NR.Right - 20, TrackP6, 6, LineSpacingThreeRows);

        SingDrawPlayerLine(NR.Left + 20, TopThreeRows1, NR.Width - 40, TrackP4, 3, LineSpacingThreeRows);
        SingDrawPlayerLine(NR.Left + 20, TopThreeRows2, NR.Width - 40, TrackP5, 4, LineSpacingThreeRows);
        SingDrawPlayerLine(NR.Left + 20, TopThreeRows3, NR.Width - 40, TrackP6, 5, LineSpacingThreeRows);
      end;
    end
    else
    begin
      // SINGLESCREEN
      SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows1, NR.Right/2 - 20, TrackP1, 0, LineSpacingThreeRows);
      SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows2, NR.Right/2 - 20, TrackP2, 1, LineSpacingThreeRows);
      SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows3, NR.Right/2 - 20, TrackP3, 2, LineSpacingThreeRows);
      SingDrawPlayerBGLine(NR.Right/2 - 20 + NR.Left + 20, TopThreeRows1, NR.Right - 20, TrackP4, 3, LineSpacingThreeRows);
      SingDrawPlayerBGLine(NR.Right/2 - 20 + NR.Left + 20, TopThreeRows2, NR.Right - 20, TrackP5, 4, LineSpacingThreeRows);
      SingDrawPlayerBGLine(NR.Right/2 - 20 + NR.Left + 20, TopThreeRows3, NR.Right - 20, TrackP6, 5, LineSpacingThreeRows);

      SingDrawLine(NR.Left + 20, TopThreeRows1, NR.Right/2 - 20, TrackP1, 1, LineSpacingThreeRows);
      SingDrawLine(NR.Left + 20, TopThreeRows2, NR.Right/2 - 20, TrackP2, 2, LineSpacingThreeRows);
      SingDrawLine(NR.Left + 20, TopThreeRows3, NR.Right/2 - 20, TrackP3, 3, LineSpacingThreeRows);
      SingDrawLine(NR.Right/2 - 20 + NR.Left + 20, TopThreeRows1, NR.Right - 20, TrackP4, 4, LineSpacingThreeRows);
      SingDrawLine(NR.Right/2 - 20 + NR.Left + 20, TopThreeRows2, NR.Right - 20, TrackP5, 5, LineSpacingThreeRows);
      SingDrawLine(NR.Right/2 - 20 + NR.Left + 20, TopThreeRows3, NR.Right - 20, TrackP6, 6, LineSpacingThreeRows);

      SingDrawPlayerLine(NR.Left + 20, TopThreeRows1, NR.Width/2 - 50, TrackP1, 0, LineSpacingThreeRows);
      SingDrawPlayerLine(NR.Left + 20, TopThreeRows2, NR.Width/2 - 50, TrackP2, 1, LineSpacingThreeRows);
      SingDrawPlayerLine(NR.Left + 20, TopThreeRows3, NR.Width/2 - 50, TrackP3, 2, LineSpacingThreeRows);
      SingDrawPlayerLine(NR.Width/2 - 10 + NR.Left + 20, TopThreeRows1, NR.Width/2 - 30, TrackP4, 3, LineSpacingThreeRows);
      SingDrawPlayerLine(NR.Width/2 - 10 + NR.Left + 20, TopThreeRows2, NR.Width/2 - 30, TrackP5, 4, LineSpacingThreeRows);
      SingDrawPlayerLine(NR.Width/2 - 10 + NR.Left + 20, TopThreeRows3, NR.Width/2 - 30, TrackP6, 5, LineSpacingThreeRows);
    end;
  end;
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure SingDrawJukebox;
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

  // FIXME: accessing ScreenJukebox is not that generic
  LyricEngine := ScreenJukebox.Lyrics;

  // draw Lyrics
  if (ScreenJukebox.ShowLyrics) then
  begin
    if (ScreenJukebox.LyricsStart) or ((not(ScreenJukebox.LyricsStart) and (LyricsState.GetCurrentTime() * 1000 >= LyricsState.StartTime - 3000))) then
    begin
        LyricEngine.Draw(LyricsState.MidBeat);
        SingDrawLyricHelperJukebox(NR.Left, NR.WMid);
        ScreenJukebox.LyricsStart := true;
    end;
  end;

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

// Draw Note Bars for Editor
// There are 11 reasons for a new procedure:   (nice binary :D )
// 1. It does not look good when you draw the golden note star effect in the editor
// 2. You can see the freestyle notes in the editor semitransparent
// 3. It is easier and faster then changing the old procedure
procedure EditDrawLine(X, YBaseNote, W, H: real; Track: integer; NumLines: integer);
var
  Rec:   TRecR;
  Count: integer;
  TempR: real;
  GoldenStarPos: real;
  Space: real;
begin
  Space := H / (NumLines - 1);
  glColor3f(1, 1, 1);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  if not Tracks[Track].Lines[Tracks[Track].CurrentLine].HasLength(TempR) then TempR := 0
  else TempR := W / TempR;

  with Tracks[Track].Lines[Tracks[Track].CurrentLine] do
  begin
    for Count := 0 to HighNote do
    begin
      with Notes[Count] do
      begin

        // Golden Note Patch
        case NoteType of
          ntFreestyle: glColor4f(1, 1, 1, 0.35);
          ntNormal: glColor4f(1, 1, 1, 0.85);
          ntGolden: Glcolor4f(1, 1, 0.3, 0.85);
          ntRap: glColor4f(1, 1, 1, 0.85);
          ntRapGolden: Glcolor4f(1, 1, 0.3, 0.85);
        end; // case

        // left part
        Rec.Left  := (StartBeat - Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + X + 0.5 + 10*ScreenX;
        Rec.Right := Rec.Left + NotesW[0];
        Rec.Top := YBaseNote - (Tone-BaseNote)*Space/2 - NotesH[0];
        Rec.Bottom := Rec.Top + 2 * NotesH[0];
        If (NoteType = ntRap) or (NoteType = ntRapGolden) then
        begin
          glBindTexture(GL_TEXTURE_2D, Tex_Left_Rap[Color].TexNum);
        end
        else
        begin
          glBindTexture(GL_TEXTURE_2D, Tex_Left[Color].TexNum);
        end;
        glBegin(GL_QUADS);
          glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
          glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
          glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
          glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
        glEnd;
        GoldenStarPos := Rec.Left;

        // middle part
        Rec.Left  := Rec.Right;
        Rec.Right := (StartBeat + Duration - Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + X - NotesW[0] - 0.5 + 10*ScreenX;

        If (NoteType = ntRap) or (NoteType = ntRapGolden) then
        begin
          glBindTexture(GL_TEXTURE_2D, Tex_Mid_Rap[Color].TexNum);
        end
        else
        begin
          glBindTexture(GL_TEXTURE_2D, Tex_Mid[Color].TexNum);
        end;
        glBegin(GL_QUADS);
          glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
          glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
          glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
          glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
        glEnd;

        // right part
        Rec.Left  := Rec.Right;
        Rec.Right := Rec.Right + NotesW[0];

        If (NoteType = ntRap) or (NoteType = ntRapGolden) then
        begin
          glBindTexture(GL_TEXTURE_2D, Tex_Right_Rap[Color].TexNum);
        end
        else
        begin
          glBindTexture(GL_TEXTURE_2D, Tex_Right[Color].TexNum);
        end;
        glBegin(GL_QUADS);
          glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
          glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
          glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
          glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
        glEnd;

        if ((NoteType = ntGolden) or (NoteType = ntRapGolden)) and (Ini.EffectSing = 1) then
        begin
          GoldenRec.SaveGoldenStarsRec(GoldenStarPos, Rec.Top, Rec.Right, Rec.Bottom);
        end;
        
      end; // with
    end; // for
  end; // with

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure EditDrawBorderedBox(X, Y, W, H: integer; FillR, FillG, FillB, FillAlpha: real);
begin
  glDisable(GL_BLEND);

  // box
  glColor4f(FillR,
            FillG,
            FillB,
            FillAlpha);
  glbegin(gl_quads);
    glVertex2f(X, Y);
    glVertex2f(X, Y+H);
    glVertex2f(X+W, Y+H);
    glVertex2f(X+W, Y);
  glEnd;

  // black border
  glColor4f(0, 0, 0, 1);
  glLineWidth(2);
  glBegin(GL_LINE_LOOP);
    glVertex2f(X-1, Y-1);
    glVertex2f(X+W+1, Y-1);
    glVertex2f(X+W+1, Y+H+1);
    glVertex2f(X-1, Y+H+1);
  glEnd;
end;

procedure EditDrawBeatDelimiters(X, Y, W, H: real; Track: integer);
var
  Count: integer;
  TempR: real;
begin

  if not Tracks[Track].Lines[Tracks[Track].CurrentLine].HasLength(TempR) then TempR := 0
  else TempR := W / TempR;

  if (Tracks[Track].Lines[Tracks[Track].CurrentLine].ScoreValue > 0) and ( W > 0 ) and ( (Tracks[Track].Lines[Tracks[Track].CurrentLine].EndBeat - Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[0].StartBeat) > 0 ) then
      TempR := W / (Tracks[Track].Lines[Tracks[Track].CurrentLine].EndBeat - Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[0].StartBeat)
    else
      TempR := 0;
  glEnable(GL_BLEND);
  glBegin(GL_LINES);
  for Count := Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[0].StartBeat to Tracks[Track].Lines[Tracks[Track].CurrentLine].EndBeat do
  begin
    if (Count mod Tracks[Track].Resolution) = Tracks[Track].NotesGAP then
      glColor4f(0, 0, 0, 1)
    else
      glColor4f(0, 0, 0, 0.3);
    glVertex2f(X + TempR * (Count - Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[0].StartBeat), Y);
    glVertex2f(X + TempR * (Count - Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[0].StartBeat), Y + H);
  end;
  glEnd;
  glDisable(GL_BLEND);
end;

procedure SingDrawJukeboxTimeBar();
var
  x, y:           real;
  width, height:  real;
  LyricsProgress: real;
  CurLyricsTime:  real;
begin

  if (ScreenJukebox.SongListVisible) then
  begin
    x := Theme.Jukebox.StaticTimeProgress.x;
    y := Theme.Jukebox.StaticTimeProgress.y;

    width  := Theme.Jukebox.StaticTimeProgress.w;
    height := Theme.Jukebox.StaticTimeProgress.h;

    glColor4f(Theme.Jukebox.StaticTimeProgress.ColR,
              Theme.Jukebox.StaticTimeProgress.ColG,
              Theme.Jukebox.StaticTimeProgress.ColB, 1); //Set Color
  end;

  if (ScreenJukebox.SongMenuVisible) then
  begin
    x := Theme.Jukebox.StaticSongMenuTimeProgress.x;
    y := Theme.Jukebox.StaticSongMenuTimeProgress.y;

    width  := Theme.Jukebox.StaticSongMenuTimeProgress.w;
    height := Theme.Jukebox.StaticSongMenuTimeProgress.h;

    glColor4f(Theme.Jukebox.StaticSongMenuTimeProgress.ColR,
              Theme.Jukebox.StaticSongMenuTimeProgress.ColG,
              Theme.Jukebox.StaticSongMenuTimeProgress.ColB, 1); //Set Color
  end;

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);

  glBindTexture(GL_TEXTURE_2D, Tex_JukeboxTimeProgress.TexNum);

  glBegin(GL_QUADS);
    glTexCoord2f(0, 0);
    glVertex2f(x, y);

    CurLyricsTime := LyricsState.GetCurrentTime();
    if (CurLyricsTime > 0) and
       (LyricsState.TotalTime > 0) then
    begin
      LyricsProgress := CurLyricsTime / LyricsState.TotalTime;
      // avoid that the bar "overflows" for inaccurate song lengths
      if (LyricsProgress > 1.0) then
        LyricsProgress := 1.0;
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

