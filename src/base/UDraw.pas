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
procedure SingDrawNoteLines(Left, Top, Right: real; LineSpacing: integer = 15; LineThickness: single = 1);
procedure SingDrawLyricHelper(CP: integer; Left, LyricsMid: real);
procedure SingDrawLine(Left, Top, Right: real; Track, PlayerIndex: integer; LineSpacing: integer = 15);
procedure SingDrawPlayerLine(Left, Top, W: real; Track, PlayerIndex: integer; LineSpacing: integer = 15);
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

const
P1_INVERTED = 99;

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
  UText,
  UGraphic,
  ULog,
  ULyrics,
  UNote,
  UParty,
  UMusic,
  URecord,
  URenderer,
  UScreenSingController,
  UScreenJukebox,
  USong,
  UWebcam;


procedure SingDrawWebCamFrame;
begin

  Webcam.GetWebcamFrame;

  if (Webcam.TextureCam <> nil) then
  begin
    with Webcam.TextureCam do
    begin
      X := 0;
      Y := 0;
      Z := 0;
      W := RenderW;
      H := renderH;
    end;
    Renderer.DrawTexture(Webcam.TextureCam);

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
  // TODO: these (especially the aspects) should just be precomputed
  ScreenAspect, TexAspect: double;  // aspect of screen resolution and image
  ScaledTexWidth, ScaledTexHeight: double;
begin
  // TODO: this is also called if a video is playing
  if (ScreenSing.Tex_Background <> nil) then
  begin
    if (Ini.MovieSize <= 1) then  //HalfSize BG
    begin
      (* half screen + gradient *)
      Rec.Top := 110; // 80
      Rec.Bottom := Rec.Top + 20;
      Rec.Left  := 0;
      Rec.Right := 800;

      TexRec.Top := (Rec.Top / 600);
      TexRec.Bottom := (Rec.Bottom / 600);
      TexRec.Left := 0;
      TexRec.Right := 1;

      (* gradient draw *)
      (* top *)
      with ScreenSing.Tex_Background do
      begin
        X := Rec.Left;
        Y := Rec.Top;
        W := Rec.Right - Rec.Left;
        H := Rec.Bottom - Rec.Top;
        TexX1 := TexRec.Left;
        TexX2 := TexRec.Right;
        TexY1 := TexRec.Top;
        TexY2 := TexRec.Bottom;
        AlphaGradient := gdVertical;
        Alpha := 0; // Top alpha
        Alpha2 := 1; // Bottom alpha
      end;
      Renderer.DrawTexture(ScreenSing.Tex_Background);

      (* mid *)
      Rec.Top := Rec.Bottom;
      Rec.Bottom := 490 - 20; // 490 - 20
      TexRec.Top := TexRec.Bottom;
      TexRec.Bottom := (Rec.Bottom / 600);
      with ScreenSing.Tex_Background do
      begin
        Y := Rec.Top;
        H := Rec.Bottom - Rec.Top;
        TexY1 := TexRec.Top;
        TexY2 := TexRec.Bottom;
        AlphaGradient := gdNone;
        Alpha := 1;
      end;
      Renderer.DrawTexture(ScreenSing.Tex_Background);

      (* bottom *)
      Rec.Top := Rec.Bottom;
      Rec.Bottom := 490; // 490
      TexRec.Top := TexRec.Bottom;
      TexRec.Bottom := (Rec.Bottom / 600);
      with ScreenSing.Tex_Background do
      begin
        Y := Rec.Top;
        H := Rec.Bottom - Rec.Top;
        TexY1 := TexRec.Top;
        TexY2 := TexRec.Bottom;
        AlphaGradient := gdVertical;
        Alpha := 1; // Top alpha
        Alpha2 := 0; // Bottom alpha
      end;
      Renderer.DrawTexture(ScreenSing.Tex_Background);
    end
    else //Full Size BG
    begin
      // Three aspects to take into account:
      //  1. Screen/display resolution (e.g. 1920x1080 -> 16:9)
      //  2. Render aspect (fWidth x fHeight -> variable)
      //  3. Movie aspect (video frame aspect stored in fAspect)
      ScreenAspect := (ScreenW/Screens)/ScreenH;
      TexAspect := ScreenSing.Tex_Background.W/ScreenSing.Tex_Background.H;

      case ScreenSing.BackgroundAspectCorrection of
        acoCrop: begin
          if (ScreenAspect >= TexAspect) then
          begin
            ScaledTexWidth  := RenderW;
            ScaledTexHeight := RenderH * ScreenAspect/TexAspect;
          end else
          begin
            ScaledTexHeight := RenderH;
            ScaledTexWidth  := RenderW * TexAspect/ScreenAspect;
          end;
        end;

        acoHalfway: begin
          ScaledTexWidth  := (RenderW + RenderW * TexAspect/ScreenAspect)/2;
          ScaledTexHeight := (RenderH + RenderH * ScreenAspect/TexAspect)/2;
        end;

        acoLetterBox: begin
          if (ScreenAspect <= TexAspect) then
          begin
            ScaledTexWidth  := RenderW;
            ScaledTexHeight := RenderH * ScreenAspect/TexAspect;
          end else
          begin
            ScaledTexHeight := RenderH;
            ScaledTexWidth  := RenderW * TexAspect/ScreenAspect;
          end;
        end else
          raise Exception.Create('Unhandled aspect correction!');
      end;

      //center video
      Rec.Left  := (RenderW - ScaledTexWidth) / 2;
      Rec.Right := Rec.Left + ScaledTexWidth;
      Rec.Top := (RenderH - ScaledTexHeight) / 2;
      Rec.Bottom := Rec.Top + ScaledTexHeight;
      with ScreenSing.Tex_Background do
      begin
        X := Rec.Left;
        Y := Rec.Top;
        W := Rec.Right - Rec.Left;
        H := Rec.Bottom - Rec.Top;
        TexX1 := 0;
        TexX2 := 1;
        TexY1 := 0;
        TexY2 := 1;
        AlphaGradient := gdNone;
        Alpha := 1;
      end;
      Renderer.DrawTexture(ScreenSing.Tex_Background);
    end;
  end;
end;

procedure SingDrawJukeboxBackground;
var
  Rec:    TRecR;
  TexRec: TRecR;
begin
  if (ScreenJukebox.Tex_Background <> nil) then
  begin
    if (Ini.MovieSize <= 1) then  //HalfSize BG
    begin
      (* half screen + gradient *)
      Rec.Top := 110; // 80
      Rec.Bottom := Rec.Top + 20;
      Rec.Left  := 0;
      Rec.Right := 800;

      TexRec.Top := (Rec.Top / 600);
      TexRec.Bottom := (Rec.Bottom / 600);
      TexRec.Left := 0;
      TexRec.Right := 1;

      (* gradient draw *)
      (* top *)
      with ScreenSing.Tex_Background do
      begin
        X := Rec.Left;
        Y := Rec.Top;
        W := Rec.Right - Rec.Left;
        H := Rec.Bottom - Rec.Top;
        TexX1 := TexRec.Left;
        TexX2 := TexRec.Right;
        TexY1 := TexRec.Top;
        TexY2 := TexRec.Bottom;
        AlphaGradient := gdVertical;
        Alpha := 0; // Top alpha
        Alpha2 := 1; // Bottom alpha
      end;
      Renderer.DrawTexture(ScreenSing.Tex_Background);

      (* mid *)
      Rec.Top := Rec.Bottom;
      Rec.Bottom := 490 - 20; // 490 - 20
      TexRec.Top := TexRec.Bottom;
      TexRec.Bottom := (Rec.Bottom / 600);
      with ScreenSing.Tex_Background do
      begin
        Y := Rec.Top;
        H := Rec.Bottom - Rec.Top;
        TexY1 := TexRec.Top;
        TexY2 := TexRec.Bottom;
        AlphaGradient := gdNone;
        Alpha := 1;
      end;
      Renderer.DrawTexture(ScreenSing.Tex_Background);

      (* bottom *)
      Rec.Top := Rec.Bottom;
      Rec.Bottom := 490; // 490
      TexRec.Top := TexRec.Bottom;
      TexRec.Bottom := (Rec.Bottom / 600);
      with ScreenSing.Tex_Background do
      begin
        Y := Rec.Top;
        H := Rec.Bottom - Rec.Top;
        TexY1 := TexRec.Top;
        TexY2 := TexRec.Bottom;
        AlphaGradient := gdVertical;
        Alpha := 1; // Top alpha
        Alpha2 := 0; // Bottom alpha
      end;
      Renderer.DrawTexture(ScreenSing.Tex_Background);
    end
    else //Full Size BG
    begin
      with ScreenSing.Tex_Background do
      begin
        X := 0;
        Y := 0;
        W := RenderW;
        H := RenderH;
        TexX1 := 0;
        TexX2 := 1;
        TexY1 := 0;
        TexY2 := 1;
        AlphaGradient := gdNone;
        Alpha := 1;
      end;
      Renderer.DrawTexture(ScreenSing.Tex_Background);
    end;
  end
  else
    SingDrawJukeboxBlackBackground;
end;

procedure SingDrawJukeboxBlackBackground;
begin
  Renderer.DrawQuad(0, 0, 0, RenderW, RenderH, 0, 0, 0, 1);
end;

procedure SingDrawOscilloscopes;
begin;
  if PlayersPlay = 1 then
    SingDrawOscilloscope(Theme.Sing.Solo1PP1.Oscilloscope, 0);

  if PlayersPlay = 2 then
  begin
    SingDrawOscilloscope(Theme.Sing.Solo2PP1.Oscilloscope, 0);
    SingDrawOscilloscope(Theme.Sing.Solo2PP2.Oscilloscope, 1);
  end;

  if PlayersPlay = 3 then
  begin
    if (CurrentSong.isDuet) then
    begin
      SingDrawOscilloscope(Theme.Sing.Duet3PP1.Oscilloscope, 0);
      SingDrawOscilloscope(Theme.Sing.Duet3PP2.Oscilloscope, 1);
      SingDrawOscilloscope(Theme.Sing.Duet3PP3.Oscilloscope, 2);
    end
    else
    begin
      SingDrawOscilloscope(Theme.Sing.Solo3PP1.Oscilloscope, 0);
      SingDrawOscilloscope(Theme.Sing.Solo3PP2.Oscilloscope, 1);
      SingDrawOscilloscope(Theme.Sing.Solo3PP3.Oscilloscope, 2);
    end;
  end;

  if PlayersPlay = 4 then
  begin
    if (Ini.Screens = 1) then
    begin
      if ScreenAct = 1 then
      begin
        SingDrawOscilloscope(Theme.Sing.Solo2PP1.Oscilloscope, 0);
        SingDrawOscilloscope(Theme.Sing.Solo2PP2.Oscilloscope, 1);
      end;
      if ScreenAct = 2 then
      begin
        SingDrawOscilloscope(Theme.Sing.Solo2PP1.Oscilloscope, 2);
        SingDrawOscilloscope(Theme.Sing.Solo2PP2.Oscilloscope, 3);
      end;
    end
    else
    begin
      if (CurrentSong.isDuet) then
      begin
        SingDrawOscilloscope(Theme.Sing.Duet4PP1.Oscilloscope, 0);
        SingDrawOscilloscope(Theme.Sing.Duet4PP2.Oscilloscope, 1);
        SingDrawOscilloscope(Theme.Sing.Duet4PP3.Oscilloscope, 2);
        SingDrawOscilloscope(Theme.Sing.Duet4PP4.Oscilloscope, 3);
      end
      else
      begin
        SingDrawOscilloscope(Theme.Sing.Solo4PP1.Oscilloscope, 0);
        SingDrawOscilloscope(Theme.Sing.Solo4PP2.Oscilloscope, 1);
        SingDrawOscilloscope(Theme.Sing.Solo4PP3.Oscilloscope, 2);
        SingDrawOscilloscope(Theme.Sing.Solo4PP4.Oscilloscope, 3);
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
          SingDrawOscilloscope(Theme.Sing.Duet3PP1.Oscilloscope, 0);
          SingDrawOscilloscope(Theme.Sing.Duet3PP2.Oscilloscope, 1);
          SingDrawOscilloscope(Theme.Sing.Duet3PP3.Oscilloscope, 2);
        end;
        if ScreenAct = 2 then
        begin
          SingDrawOscilloscope(Theme.Sing.Duet3PP1.Oscilloscope, 3);
          SingDrawOscilloscope(Theme.Sing.Duet3PP2.Oscilloscope, 4);
          SingDrawOscilloscope(Theme.Sing.Duet3PP3.Oscilloscope, 5);
        end;
      end
      else
      begin
        if ScreenAct = 1 then
        begin
          SingDrawOscilloscope(Theme.Sing.Solo3PP1.Oscilloscope, 0);
          SingDrawOscilloscope(Theme.Sing.Solo3PP2.Oscilloscope, 1);
          SingDrawOscilloscope(Theme.Sing.Solo3PP3.Oscilloscope, 2);
        end;

        if ScreenAct = 2 then
        begin
          SingDrawOscilloscope(Theme.Sing.Solo3PP1.Oscilloscope, 3);
          SingDrawOscilloscope(Theme.Sing.Solo3PP2.Oscilloscope, 4);
          SingDrawOscilloscope(Theme.Sing.Solo3PP3.Oscilloscope, 5);
        end;
      end;
    end
    else
    begin
      if (CurrentSong.isDuet) then
      begin
        SingDrawOscilloscope(Theme.Sing.Duet6PP1.Oscilloscope, 0);
        SingDrawOscilloscope(Theme.Sing.Duet6PP2.Oscilloscope, 1);
        SingDrawOscilloscope(Theme.Sing.Duet6PP3.Oscilloscope, 2);
        SingDrawOscilloscope(Theme.Sing.Duet6PP4.Oscilloscope, 3);
        SingDrawOscilloscope(Theme.Sing.Duet6PP5.Oscilloscope, 4);
        SingDrawOscilloscope(Theme.Sing.Duet6PP6.Oscilloscope, 5);
      end
      else
      begin
        SingDrawOscilloscope(Theme.Sing.Solo6PP1.Oscilloscope, 0);
        SingDrawOscilloscope(Theme.Sing.Solo6PP2.Oscilloscope, 1);
        SingDrawOscilloscope(Theme.Sing.Solo6PP3.Oscilloscope, 2);
        SingDrawOscilloscope(Theme.Sing.Solo6PP4.Oscilloscope, 3);
        SingDrawOscilloscope(Theme.Sing.Solo6PP5.Oscilloscope, 4);
        SingDrawOscilloscope(Theme.Sing.Solo6PP6.Oscilloscope, 5);
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
  PointList: TPointList;
begin;
  Sound := AudioInputProcessor.Sound[NrSound];

  if (Party.bPartyGame) then
    Col := GetPlayerColor(Ini.TeamColor[NrSound])
  else
    Col := GetPlayerColor(Ini.PlayerColor[NrSound]);

  MaxX := Position.W-1;
  MaxY := (Position.H-1) / 2;
  Sound.LockAnalysisBuffer();
  SetLength(PointList, Length(Sound.AnalysisBuffer));

  for SampleIndex := 0 to High(Sound.AnalysisBuffer) do
  begin
    PointList[SampleIndex].X := SampleIndex;
    PointList[SampleIndex].Y := Sound.AnalysisBuffer[SampleIndex];
  end;

  Sound.UnlockAnalysisBuffer();
  Renderer.DrawLineStrip(PointList, MaxX/High(Sound.AnalysisBuffer), MaxY/Low(Smallint), Position.X, Position.Y + MaxY, Col.R, Col.G, Col.B, 1);
end;

procedure SingDrawNoteLines(Left, Top, Right: real; LineSpacing: integer; LineThickness: single);
var
  Count: integer;
  Y: single;
begin
  for Count := 0 to 9 do
  begin
    Y := Top + Count * LineSpacing;
    Renderer.DrawLine(Left, Y, Right, Y, 0, LineThickness, Skin_P1_LinesR, Skin_P1_LinesG, Skin_P1_LinesB, 0.4);
  end;
end;

// draw blank Notebars
procedure SingDrawLine(Left, Top, Right: real; Track, PlayerIndex: integer; LineSpacing: integer);
var
  Rec:   TRecR;
  Count: integer;
  TempR: real;
  PlayerNumber: integer;
  Texture: TTexture;
  ColorR, ColorG, ColorB, A: single;
  GoldenStarPos: real;
begin
  if (ScreenSing.settings.NotesVisible[Track]) then
  begin
    // the textures start counting at 1, but everything else just starts at 0
    PlayerNumber := PlayerIndex + 1;

    if not CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].HasLength(TempR) then TempR := 0
    else TempR := (Right-Left) / TempR;

    with CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine] do
    begin
      for Count := 0 to HighNote do
      begin
        with Notes[Count] do
        begin
          if NoteType <> ntFreestyle then
          begin
            If (NoteType = ntRap) or (NoteType = ntRapGolden) then
              Texture := Tex_plain_Left_Rap[PlayerNumber]
            else
              Texture := Tex_plain_Left[PlayerNumber];
            if Ini.EffectSing = 0 then
              // If Golden note Effect of then Change not Color
            begin
              case NoteType of
                ntNormal:
                begin
                  ColorR := 1;
                  ColorG := 1;
                  ColorB := 1;
                  A := 1; // We set alpha to 1, cause we can control the transparency through the png itself
                end;
                ntGolden:
                begin
                  ColorR := 1;
                  ColorG := 1;
                  ColorB := 0.3;
                  A := 1; // no stars, paint yellow -> glColor4f(1, 1, 0.3, 0.85); - we could
                end;
                ntRap:
                begin
                  ColorR := 1;
                  ColorG := 1;
                  ColorB := 1;
                  A := 1;
                end;
                ntRapGolden:
                begin
                  ColorR := 1;
                  ColorG := 1;
                  ColorB := 0.3;
                  A := 1;
                end;
              end; // case
            end //Else all Notes same Color
            else
            begin
              ColorR := 1;
              ColorG := 1;
              ColorB := 1;
              A := 1;
            end;

            // left part
            Rec.Left  := (StartBeat - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + Left + 0.5;
            Rec.Right := Rec.Left + NotesW[PlayerIndex];
            Rec.Top := Top - (Tone-BaseNote)*LineSpacing/2 - NotesH[PlayerIndex];
            Rec.Bottom := Rec.Top + 2 * NotesH[PlayerIndex];
            with Texture do
            begin
              X := Rec.Left;
              Y := Rec.Top;
              W := Rec.Right - Rec.Left;
              H := Rec.Bottom - Rec.Top;
              ColR := ColorR;
              ColG := ColorG;
              ColB := ColorB;
              Alpha := A;
            end;
            Renderer.DrawTexture(Texture);

            //We keep the postion of the top left corner b4 it's overwritten
            GoldenStarPos := Rec.Left;
            //done

            // middle part
            Rec.Left := Rec.Right;
            Rec.Right := (StartBeat + Duration - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + Left - NotesW[PlayerIndex] - 0.5;

            // the left note is more right than the right note itself, sounds weird - so we fix that xD
            if Rec.Right <= Rec.Left then
              Rec.Right := Rec.Left;

            If (NoteType = ntRap) or (NoteType = ntRapGolden) then
              Texture := Tex_plain_Mid_Rap[PlayerNumber]
            else
              Texture := Tex_plain_Mid[PlayerNumber];

            with Texture do
            begin
              X := Rec.Left;
              Y := Rec.Top;
              W := Rec.Right - Rec.Left;
              H := Rec.Bottom - Rec.Top;
              ColR := ColorR;
              ColG := ColorG;
              ColB := ColorB;
              Alpha := A;
              TexX1 := 0;
              TexX2 := round((Rec.Right-Rec.Left)/32);
              TexY1 := 0;
              TexY2 := 1;
            end;
            Renderer.DrawTexture(Texture);

            // right part
            Rec.Left  := Rec.Right;
            Rec.Right := Rec.Right + NotesW[PlayerIndex];


            if (NoteType = ntRap) or (NoteType = ntRapGolden) then
              Texture := Tex_plain_Right_Rap[PlayerNumber]
            else
              Texture := Tex_plain_Right[PlayerNumber];
            with Texture do
            begin
              X := Rec.Left;
              Y := Rec.Top;
              W := Rec.Right - Rec.Left;
              H := Rec.Bottom - Rec.Top;
              ColR := ColorR;
              ColG := ColorG;
              ColB := ColorB;
              Alpha := A;
            end;
            Renderer.DrawTexture(Texture);

            // Golden Star Patch
            if ((NoteType = ntGolden) or (NoteType = ntRapGolden)) and (Ini.EffectSing=1) then
            begin
              GoldenRec.SaveGoldenStarsRec(GoldenStarPos, Rec.Top, Rec.Right, Rec.Bottom);
            end;
          end; // if not FreeStyle
        end; // with
      end; // for
    end; // with
  end;
end;

// draw sung notes
procedure SingDrawPlayerLine(Left, Top, W: real; Track, PlayerIndex: integer; LineSpacing: integer);
var
  TempR:      real;
  Rec:        TRecR;
  N: integer;
  Texture: TTexture;
//  R, G, B, A: real;
  NotesH2:    real;
begin
  if (ScreenSing.Settings.InputVisible) then
  begin
    //Log.LogStatus('Player notes', 'SingDraw');

    //if Player[NrGracza].LengthNote > 0 then
    begin
      if not CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].HasLength(TempR) then TempR := 0
      else TempR := W / TempR;

      for N := 0 to Player[PlayerIndex].HighNote do
      begin
        with Player[PlayerIndex].Note[N] do
        begin
          // Left part of note
          Rec.Left  := Left + (Start - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + 0.5;
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

          Rec.Top    := Top - (Tone-CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].BaseNote)*LineSpacing/2 - NotesH2;
          Rec.Bottom := Rec.Top + 2 * NotesH2;

          // draw the left part
          If (NoteType = ntRap) or (NoteType = ntRapGolden) then
            Texture := Tex_Left_Rap[PlayerIndex+1]
          else
            Texture := Tex_Left[PlayerIndex+1];
          with Texture do
          begin
            X := Rec.Left;
            Y := Rec.Top;
            W := Rec.Right - Rec.Left;
            H := Rec.Bottom - Rec.Top;
          end;
          Renderer.DrawTexture(Texture);

          // Middle part of the note
          Rec.Left  := Rec.Right;
          Rec.Right := Left + (Start + Duration - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR - NotesW[PlayerIndex] - 0.5;

          // new
          if (Start + Duration - 1 = LyricsState.CurrentBeatD) then
            Rec.Right := Rec.Right - (1-Frac(LyricsState.MidBeatD)) * TempR;

          // the left note is more right than the right note itself, sounds weird - so we fix that xD
          if Rec.Right <= Rec.Left then
            Rec.Right := Rec.Left;

          // draw the middle part
          If (NoteType = ntRap) or (NoteType = ntRapGolden) then
            Texture := Tex_Mid_Rap[PlayerIndex+1]
          else
            Texture := Tex_Mid[PlayerIndex+1];

          with Texture do
          begin
            X := Rec.Left;
            Y := Rec.Top;
            W := Rec.Right - Rec.Left;
            H := Rec.Bottom - Rec.Top;
            TexX1 := 0;
            TexY1 := 0;
            TexX2 := round((Rec.Right-Rec.Left)/32);
            TexY2 := 1;
          end;
          Renderer.DrawTexture(Texture);

          // the right part of the note
          Rec.Left  := Rec.Right;
          Rec.Right := Rec.Right + NotesW[PlayerIndex];

          If (NoteType = ntRap) or (NoteType = ntRapGolden) then
            Texture := Tex_Right_Rap[PlayerIndex+1]
          else
            Texture := Tex_Right[PlayerIndex+1];
          with Texture do
          begin
            X := Rec.Left;
            Y := Rec.Top;
            W := Rec.Right - Rec.Left;
            H := Rec.Bottom - Rec.Top;
          end;
          Renderer.DrawTexture(Texture);

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
  A:          single;
  Texture:        TTexture;
begin
  if (ScreenSing.settings.NotesVisible[PlayerIndex]) then
  begin
    A := sqrt((1 + sin(AudioPlayback.Position * 3)))/2 + 0.05;

    if not CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].HasLength(TempR) then TempR := 0
    else TempR := (Right-Left) / TempR;

    with CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine] do
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

            // left
            Rec.Right := (StartBeat - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + Left + 0.5 + 4;
            Rec.Left  := Rec.Right - W;
            Rec.Top := Top - (Tone-BaseNote)*LineSpacing/2 - H;
            Rec.Bottom := Rec.Top + 2 * H;

            If (NoteType = ntRap) or (NoteType = ntRapGolden) then
              Texture := Tex_BG_Left_Rap[PlayerIndex+1]
            else
              Texture := Tex_BG_Left[PlayerIndex+1];
            with Texture do
            begin
              X := Rec.Left;
              Y := Rec.Top;
              W := Rec.Right - Rec.Left;
              H := Rec.Bottom - Rec.Top;
              Alpha := A;
            end;
            Renderer.DrawTexture(Texture);

            // middle part
            Rec.Left  := Rec.Right;
            Rec.Right := (StartBeat + Duration - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + Left - 0.5 - 4;

            // the left note is more right than the right note itself, sounds weird - so we fix that xD
            if Rec.Right <= Rec.Left then
              Rec.Right := Rec.Left;

            If (NoteType = ntRap) or (NoteType = ntRapGolden) then
              Texture := Tex_BG_Mid_Rap[PlayerIndex+1]
            else
              Texture := Tex_BG_Mid[PlayerIndex+1];
            with Texture do
            begin
              X := Rec.Left;
              Y := Rec.Top;
              W := Rec.Right - Rec.Left;
              H := Rec.Bottom - Rec.Top;
              Alpha := A;
            end;
            Renderer.DrawTexture(Texture);

            // right part
            Rec.Left  := Rec.Right;
            Rec.Right := Rec.Left + W;

            If (NoteType = ntRap) or (NoteType = ntRapGolden) then
              Texture := Tex_BG_Right_Rap[PlayerIndex+1]
            else
              Texture := Tex_BG_Right[PlayerIndex+1];
            with Texture do
            begin
              X := Rec.Left;
              Y := Rec.Top;
              W := Rec.Right - Rec.Left;
              H := Rec.Bottom - Rec.Top;
              Alpha := A;
            end;
            Renderer.DrawTexture(Texture);

          end; // if not FreeStyle
        end; // with
      end; // for
    end; // with

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
  CurLine := @CurrentSong.Tracks[CP].Lines[CurrentSong.Tracks[CP].CurrentLine];

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

      with Tex_Lyric_Help_Bar do
      begin
        X := Bounds.Left;
        Y := Bounds.Top;
        W := Bounds.Right - Bounds.Left;
        H := Bounds.Bottom - Bounds.Top;
        ColR := Col.R;
        ColG := Col.G;
        ColB := Col.B;
        Alpha := BarAlpha;
      end;
      Renderer.DrawTexture(Tex_Lyric_Help_Bar);
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
  CurLine := @CurrentSong.Tracks[0].Lines[CurrentSong.Tracks[0].CurrentLine];

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
      with Tex_Lyric_Help_Bar do
      begin
        X := Bounds.Left;
        Y := Bounds.Top;
        W := Bounds.Right - Bounds.Left;
        H := Bounds.Bottom - Bounds.Top;
        ColR := ScreenJukebox.LyricHelper.R;
        ColG := ScreenJukebox.LyricHelper.G;
        ColB := ScreenJukebox.LyricHelper.B;
        Alpha := BarAlpha;
      end;
      Renderer.DrawTexture(Tex_Lyric_Help_Bar);
    end;
  end;
end;

procedure SingDrawLines;
var
  NR: TRecR;         // lyrics area bounds (NR = NoteRec?)
begin
  // positions
  NR.Left := 20;
  NR.Right := 780;
  NR.Width := 760; //NR.Right - NR.Left;
  NR.WMid  := 380;//NR.Width / 2;
  NR.Mid   := 400;//NR.Left + NR.WMid;

  // draw note-lines

  // to-do : needs fix when party mode works w/ 2 screens
  if (PlayersPlay = 1) and (Ini.NoteLines = 1) and (ScreenSing.settings.NotesVisible[0]) then
    SingDrawNoteLines(NR.Left, Skin_P2_NotesB - 105, NR.Right, 15);

  if (PlayersPlay = 2) and (Ini.NoteLines = 1) then
  begin
    if (ScreenSing.settings.NotesVisible[0]) then
      SingDrawNoteLines(Nr.Left, Skin_P1_NotesB - 105, Nr.Right, 15);
    if (ScreenSing.settings.NotesVisible[1]) then
      SingDrawNoteLines(Nr.Left, Skin_P2_NotesB - 105, Nr.Right, 15);
  end;

  if (PlayersPlay = 3) and (Ini.NoteLines = 1) then begin
    if (ScreenSing.settings.NotesVisible[0]) then
      SingDrawNoteLines(Nr.Left, 120, Nr.Right, 12);
    if (ScreenSing.settings.NotesVisible[1]) then
      SingDrawNoteLines(Nr.Left, 245, Nr.Right, 12);
    if (ScreenSing.settings.NotesVisible[2]) then
      SingDrawNoteLines(Nr.Left, 370, Nr.Right, 12);
  end;

  if (PlayersPlay = 4) and (Ini.NoteLines = 1) then
  begin
    if (ScreenSing.settings.NotesVisible[0]) then
    begin
      if (Ini.Screens = 1) then
        SingDrawNoteLines(Nr.Left, Skin_P1_NotesB - 105, Nr.Right, 15)
      else
      begin
        SingDrawNoteLines(Nr.Left, Skin_P1_NotesB - 105, Nr.Right/2 - 5, 15);
        SingDrawNoteLines(Nr.Right/2 - 20 + Nr.Left, Skin_P1_NotesB - 105, Nr.Right, 15)
      end;
    end;

    if (ScreenSing.settings.NotesVisible[1]) then
    begin
      if (Ini.Screens = 1) then
        SingDrawNoteLines(Nr.Left, Skin_P2_NotesB - 105, Nr.Right, 15)
      else
      begin
        SingDrawNoteLines(Nr.Left, Skin_P2_NotesB - 105, Nr.Right/2 - 5, 15);
        SingDrawNoteLines(Nr.Right/2 - 20 + Nr.Left, Skin_P2_NotesB - 105, Nr.Right, 15)
      end;
    end;
  end;

  if (PlayersPlay = 6) and (Ini.NoteLines = 1) then begin
    if (ScreenSing.settings.NotesVisible[0]) then
    begin
      if (Ini.Screens = 1) then
        SingDrawNoteLines(Nr.Left, 120, Nr.Right, 12)
      else
      begin
        SingDrawNoteLines(Nr.Left, 120, Nr.Right/2 - 5, 12);
        SingDrawNoteLines(Nr.Right/2 - 20 + Nr.Left, 120, Nr.Right, 12);
      end;
    end;

    if (ScreenSing.settings.NotesVisible[1]) then
    begin
      if (Ini.Screens = 1) then
        SingDrawNoteLines(Nr.Left, 245, Nr.Right, 12)
      else
      begin
        SingDrawNoteLines(Nr.Left, 245, Nr.Right/2 - 5, 12);
        SingDrawNoteLines(Nr.Right/2 - 20 + Nr.Left, 245, Nr.Right, 12);
      end;
    end;

    if (ScreenSing.settings.NotesVisible[2]) then
    begin
      if (Ini.Screens = 1) then
        SingDrawNoteLines(Nr.Left, 370, Nr.Right, 12)
      else
      begin
        SingDrawNoteLines(Nr.Left, 370, Nr.Right/2 - 5, 12);
        SingDrawNoteLines(Nr.Right/2 - 20 + Nr.Left, 370, Nr.Right, 12);
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
  // TODO: it looks like all these TopXRowsY constants are actually referring to the bottom. But all the functions they call have historically called it Top.
  TopOneRow1 = Skin_P2_NotesB;
  TopTwoRows1 = Skin_P1_NotesB;
  TopTwoRows2 = Skin_P2_NotesB;
  TopThreeRows1 = 120+95;
  TopThreeRows2 = 245+95;
  TopThreeRows3 = 370+95;
begin
  // positions
  NR.Left := 20;
  NR.Right := 780;
  NR.Width := 760; //NR.Right - NR.Left;
  NR.WMid  := 380; //NR.Width / 2;
  NR.Mid   := 400; //NR.Left + NR.WMid;

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

    if (ScreenSong.Mode = smNormal) or (ScreenSong.Mode = smMedley) then
      Difficulty := Player[I - 1].Level
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

  // draw notes lines
  if (ScreenSing.Settings.InputVisible) then
    SingDrawLines;
  // Draw the Notes
  if (PlayersPlay = 1) then
  begin
    // SINGLESCREEN
    SingDrawPlayerBGLine(NR.Left + 20, TopOneRow1, NR.Right - 20, TrackP1, 0, LineSpacingOneRow);  // Background glow    - colorized in playercolor
    SingDrawLine(NR.Left + 20, TopOneRow1, NR.Right - 20, TrackP1, 0, LineSpacingOneRow);             // Plain unsung notes - colorized in playercolor
    SingDrawPlayerLine(NR.Left + 20, TopOneRow1, NR.Width - 40, TrackP1, 0, LineSpacingOneRow);       // imho the sung notes
  end;

  if (PlayersPlay = 2) then
  begin
    // SINGLESCREEN
    SingDrawPlayerBGLine(NR.Left + 20, TopTwoRows1, NR.Right - 20, TrackP1, 0, LineSpacingTwoRows);
    SingDrawLine(NR.Left + 20, TopTwoRows1, NR.Right - 20, TrackP1, 0, LineSpacingTwoRows);
    SingDrawPlayerLine(NR.Left + 20, TopTwoRows1, NR.Width - 40, TrackP1, 0, LineSpacingTwoRows);

    SingDrawPlayerBGLine(NR.Left + 20, TopTwoRows2, NR.Right - 20, TrackP2, 1, LineSpacingTwoRows);
    SingDrawLine(NR.Left + 20, TopTwoRows2, NR.Right - 20, TrackP2, 1, LineSpacingTwoRows);
    SingDrawPlayerLine(NR.Left + 20, TopTwoRows2, NR.Width - 40, TrackP2, 1, LineSpacingTwoRows);
  end;

  if (PlayersPlay = 3) then
  begin
    // SINGLESCREEN
    SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows1, NR.Right - 20, TrackP1, 0, LineSpacingThreeRows);
    SingDrawLine(NR.Left + 20, TopThreeRows1, NR.Right - 20, TrackP1, 0, LineSpacingThreeRows);
    SingDrawPlayerLine(NR.Left + 20, TopThreeRows1, NR.Width - 40, TrackP1, 0, LineSpacingThreeRows);

    SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows2, NR.Right - 20, TrackP2, 1, LineSpacingThreeRows);
    SingDrawLine(NR.Left + 20, TopThreeRows2, NR.Right - 20, TrackP2, 1, LineSpacingThreeRows);
    SingDrawPlayerLine(NR.Left + 20, TopThreeRows2, NR.Width - 40, TrackP2, 1, LineSpacingThreeRows);

    SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows3, NR.Right - 20, TrackP3, 2, LineSpacingThreeRows);
    SingDrawLine(NR.Left + 20, TopThreeRows3, NR.Right - 20, TrackP3, 2, LineSpacingThreeRows);
    SingDrawPlayerLine(NR.Left + 20, TopThreeRows3, NR.Width - 40, TrackP3, 2, LineSpacingThreeRows);
  end;

  if (PlayersPlay = 4) then
  begin
    if (Ini.Screens = 1) then
    begin
      // MULTISCREEN
      if (ScreenAct = 1) then
      begin
        // MULTISCREEN - SCREEN 1
        SingDrawPlayerBGLine(NR.Left + 20, TopTwoRows1, NR.Right - 20, TrackP1, 0, LineSpacingTwoRows);
        SingDrawLine(NR.Left + 20, TopTwoRows1, NR.Right - 20, TrackP1, 0, LineSpacingTwoRows);
        SingDrawPlayerLine(NR.Left + 20, TopTwoRows1, NR.Width - 40, TrackP1, 0, LineSpacingTwoRows);

        SingDrawPlayerBGLine(NR.Left + 20, TopTwoRows2, NR.Right - 20, TrackP2, 1, LineSpacingTwoRows);
        SingDrawLine(NR.Left + 20, TopTwoRows2, NR.Right - 20, TrackP2, 1, LineSpacingTwoRows);
        SingDrawPlayerLine(NR.Left + 20, TopTwoRows2, NR.Width - 40, TrackP2, 1, LineSpacingTwoRows);
      end;
      if (ScreenAct = 2) then
      begin
        // MULTISCREEN - SCREEN 2
        SingDrawPlayerBGLine(NR.Left + 20, TopTwoRows1, NR.Right - 20, TrackP3, 2, LineSpacingTwoRows);
        SingDrawLine(NR.Left + 20, TopTwoRows1, NR.Right - 20, TrackP3, 2, LineSpacingTwoRows);
        SingDrawPlayerLine(NR.Left + 20, TopTwoRows1, NR.Width - 40, TrackP3, 2, LineSpacingTwoRows);

        SingDrawPlayerBGLine(NR.Left + 20, TopTwoRows2, NR.Right - 20, TrackP4, 3, LineSpacingTwoRows);
        SingDrawLine(NR.Left + 20, TopTwoRows2, NR.Right - 20, TrackP4, 3, LineSpacingTwoRows);
        SingDrawPlayerLine(NR.Left + 20, TopTwoRows2, NR.Width - 40, TrackP4, 3, LineSpacingTwoRows);
      end;
    end
    else
    begin
      // SINGLESCREEN
      SingDrawPlayerBGLine(NR.Left + 20, TopTwoRows1, NR.Right/2 - 20, TrackP1, 0, LineSpacingTwoRows);
      SingDrawLine(NR.Left + 20, TopTwoRows1, NR.Right/2 - 20, TrackP1, 0, LineSpacingTwoRows);
      SingDrawPlayerLine(NR.Left + 20, TopTwoRows1, NR.Width/2 - 50, TrackP1, 0, LineSpacingTwoRows);

      SingDrawPlayerBGLine(NR.Left + 20, TopTwoRows2, NR.Right/2 - 20, TrackP2, 1, LineSpacingTwoRows);
      SingDrawLine(NR.Left + 20, TopTwoRows2, NR.Right/2 - 20, TrackP2, 1, LineSpacingTwoRows);
      SingDrawPlayerLine(NR.Left + 20, TopTwoRows2, NR.Width/2 - 50, TrackP2, 1, LineSpacingTwoRows);

      SingDrawPlayerBGLine(NR.Right/2 - 20 + NR.Left + 20, TopTwoRows1, NR.Right - 20, TrackP3, 2, LineSpacingTwoRows);
      SingDrawLine(NR.Right/2 - 20 + NR.Left + 20, TopTwoRows1, NR.Right - 20, TrackP3, 2, LineSpacingTwoRows);
      SingDrawPlayerLine(NR.Width/2 - 10 + NR.Left + 20, TopTwoRows1, NR.Width/2 - 30, TrackP3, 2, LineSpacingTwoRows);

      SingDrawPlayerBGLine(NR.Right/2 - 20 + NR.Left + 20, TopTwoRows2, NR.Right - 20, TrackP4, 3, LineSpacingTwoRows);
      SingDrawLine(NR.Right/2 - 20 + NR.Left + 20, TopTwoRows2, NR.Right - 20, TrackP4, 3, LineSpacingTwoRows);
      SingDrawPlayerLine(NR.Width/2 - 10 + NR.Left + 20, TopTwoRows2, NR.Width/2 - 30, TrackP4, 3, LineSpacingTwoRows);
    end;
  end;

  if (PlayersPlay = 6) then
  begin
    if (Ini.Screens = 1) then
    begin
      // MULTISCREEN
      if (ScreenAct = 1) then
      begin
        // MULTISCREEN - SCREEN 1
        SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows1, NR.Right - 20, TrackP1, 0, LineSpacingThreeRows);
        SingDrawLine(NR.Left + 20, TopThreeRows1, NR.Right - 20, TrackP1, 0, LineSpacingThreeRows);
        SingDrawPlayerLine(NR.Left + 20, TopThreeRows1, NR.Width - 40, TrackP1, 0, LineSpacingThreeRows);

        SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows2, NR.Right - 20, TrackP2, 1, LineSpacingThreeRows);
        SingDrawLine(NR.Left + 20, TopThreeRows2, NR.Right - 20, TrackP2, 1, LineSpacingThreeRows);
        SingDrawPlayerLine(NR.Left + 20, TopThreeRows2, NR.Width - 40, TrackP2, 1, LineSpacingThreeRows);

        SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows3, NR.Right - 20, TrackP3, 2, LineSpacingThreeRows);
        SingDrawLine(NR.Left + 20, TopThreeRows3, NR.Right - 20, TrackP3, 2, LineSpacingThreeRows);
        SingDrawPlayerLine(NR.Left + 20, TopThreeRows3, NR.Width - 40, TrackP3, 2, LineSpacingThreeRows);
      end;
      if (ScreenAct = 2) then
      begin
        // MULTISCREEN - SCREEN 2
        SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows1, NR.Right - 20, TrackP4, 3, LineSpacingThreeRows);
        SingDrawLine(NR.Left + 20, TopThreeRows1, NR.Right - 20, TrackP4, 3, LineSpacingThreeRows);
        SingDrawPlayerLine(NR.Left + 20, TopThreeRows1, NR.Width - 40, TrackP4, 3, LineSpacingThreeRows);

        SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows2, NR.Right - 20, TrackP5, 4, LineSpacingThreeRows);
        SingDrawLine(NR.Left + 20, TopThreeRows2, NR.Right - 20, TrackP5, 4, LineSpacingThreeRows);
        SingDrawPlayerLine(NR.Left + 20, TopThreeRows2, NR.Width - 40, TrackP5, 4, LineSpacingThreeRows);

        SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows3, NR.Right - 20, TrackP6, 5, LineSpacingThreeRows);
        SingDrawLine(NR.Left + 20, TopThreeRows3, NR.Right - 20, TrackP6, 5, LineSpacingThreeRows);
        SingDrawPlayerLine(NR.Left + 20, TopThreeRows3, NR.Width - 40, TrackP6, 5, LineSpacingThreeRows);
      end;
    end
    else
    begin
      // SINGLESCREEN
      SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows1, NR.Right/2 - 20, TrackP1, 0, LineSpacingThreeRows);
      SingDrawLine(NR.Left + 20, TopThreeRows1, NR.Right/2 - 20, TrackP1, 0, LineSpacingThreeRows);
      SingDrawPlayerLine(NR.Left + 20, TopThreeRows1, NR.Width/2 - 50, TrackP1, 0, LineSpacingThreeRows);

      SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows2, NR.Right/2 - 20, TrackP2, 1, LineSpacingThreeRows);
      SingDrawLine(NR.Left + 20, TopThreeRows2, NR.Right/2 - 20, TrackP2, 1, LineSpacingThreeRows);
      SingDrawPlayerLine(NR.Left + 20, TopThreeRows2, NR.Width/2 - 50, TrackP2, 1, LineSpacingThreeRows);

      SingDrawPlayerBGLine(NR.Left + 20, TopThreeRows3, NR.Right/2 - 20, TrackP3, 2, LineSpacingThreeRows);
      SingDrawLine(NR.Left + 20, TopThreeRows3, NR.Right/2 - 20, TrackP3, 2, LineSpacingThreeRows);
      SingDrawPlayerLine(NR.Left + 20, TopThreeRows3, NR.Width/2 - 50, TrackP3, 2, LineSpacingThreeRows);

      SingDrawPlayerBGLine(NR.Right/2 - 20 + NR.Left + 20, TopThreeRows1, NR.Right - 20, TrackP4, 3, LineSpacingThreeRows);
      SingDrawLine(NR.Right/2 - 20 + NR.Left + 20, TopThreeRows1, NR.Right - 20, TrackP4, 3, LineSpacingThreeRows);
      SingDrawPlayerLine(NR.Width/2 - 10 + NR.Left + 20, TopThreeRows1, NR.Width/2 - 30, TrackP4, 3, LineSpacingThreeRows);

      SingDrawPlayerBGLine(NR.Right/2 - 20 + NR.Left + 20, TopThreeRows2, NR.Right - 20, TrackP5, 4, LineSpacingThreeRows);
      SingDrawLine(NR.Right/2 - 20 + NR.Left + 20, TopThreeRows2, NR.Right - 20, TrackP5, 4, LineSpacingThreeRows);
      SingDrawPlayerLine(NR.Width/2 - 10 + NR.Left + 20, TopThreeRows2, NR.Width/2 - 30, TrackP5, 4, LineSpacingThreeRows);

      SingDrawPlayerBGLine(NR.Right/2 - 20 + NR.Left + 20, TopThreeRows3, NR.Right - 20, TrackP6, 5, LineSpacingThreeRows);
      SingDrawLine(NR.Right/2 - 20 + NR.Left + 20, TopThreeRows3, NR.Right - 20, TrackP6, 5, LineSpacingThreeRows);
      SingDrawPlayerLine(NR.Width/2 - 10 + NR.Left + 20, TopThreeRows3, NR.Width/2 - 30, TrackP6, 5, LineSpacingThreeRows);
    end;
  end;
end;

procedure SingDrawJukebox;
var
  NR: TRecR;         // lyrics area bounds (NR = NoteRec?)
  LyricEngine: TLyricEngine;
begin
  // positions
  NR.Left := 20;
  NR.Right := 780;
  NR.Width := 760; //NR.Right - NR.Left;
  NR.WMid  := 380; //NR.Width / 2;
  NR.Mid   := 400; //NR.Left + NR.WMid;

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
  Texture: TTexture;
  ColorR, ColorG, ColorB, A: single;
begin
  Space := H / (NumLines - 1);

  if not CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].HasLength(TempR) then TempR := 0
  else TempR := W / TempR;

  with CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine] do
  begin
    for Count := 0 to HighNote do
    begin
      with Notes[Count] do
      begin

        // Golden Note Patch
        case NoteType of
          ntFreestyle:
          begin
            ColorR := 1;
            ColorG := 1;
            ColorB := 1;
            A := 0.35;
          end;
          ntNormal:
          begin
            ColorR := 1;
            ColorG := 1;
            ColorB := 1;
            A := 0.85;
          end;
          ntGolden:
          begin
            ColorR := 1;
            ColorG := 1;
            ColorB := 0.3;
            A := 0.85;
          end;
          ntRap:
          begin
            if (Color = P1_INVERTED) then
            begin
              ColorR := 1;
              ColorG := 1;
              ColorB := 1;
              A := 0.75;
            end
            else
            begin
              ColorR := 1;
              ColorG := 1;
              ColorB := 1;
              A := 0.5;
            end;
          end;
          ntRapGolden:
          begin
            if (Color = P1_INVERTED) then
            begin
              ColorR := 1;
              ColorG := 1;
              ColorB := 0.3;
              A := 0.75;
            end
            else
            begin
              ColorR := 1;
              ColorG := 1;
              ColorB := 0.3;
              A := 0.5;
            end;
          end;
        end; // case

        // left part
        Rec.Left  := (StartBeat - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + X + 0.5;
        Rec.Right := Rec.Left + NotesW[0];
        Rec.Top := YBaseNote - (Tone-BaseNote)*Space/2 - NotesH[0];
        Rec.Bottom := Rec.Top + 2 * NotesH[0];
        If (NoteType = ntRap) or (NoteType = ntRapGolden) then
        begin
          If Color = P1_INVERTED then
            Texture := Tex_Left_Rap_Inv
          else
            Texture := Tex_Left_Rap[Color];
        end
        else
        begin
          If Color = P1_INVERTED then
            Texture := Tex_Left_Inv
          else
            Texture := Tex_Left[Color];
        end;
        with Texture do
        begin
          X := Rec.Left;
          Y := Rec.Top;
          W := Rec.Right - Rec.Left;
          H := Rec.Bottom - Rec.Top;
          ColR := ColorR;
          ColG := ColorG;
          ColB := ColorB;
          Alpha := A;
          TexX1 := 0;
          TexX2 := 1;
          TexY1 := 0;
          TexY2 := 1;
        end;
        Renderer.DrawTexture(Texture);
        GoldenStarPos := Rec.Left;

        // middle part
        Rec.Left  := Rec.Right;
        Rec.Right := (StartBeat + Duration - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + X - NotesW[0] - 0.5;

        If (NoteType = ntRap) or (NoteType = ntRapGolden) then
        begin
          If Color = P1_INVERTED then
            Texture := Tex_Mid_Rap_Inv
          else
            Texture := Tex_Mid_Rap[Color];
        end
        else
        begin
          If Color = P1_INVERTED then
            Texture := Tex_Mid_Inv
          else
            Texture := Tex_Mid[Color];
        end;
        with Texture do
        begin
          X := Rec.Left;
          Y := Rec.Top;
          W := Rec.Right - Rec.Left;
          H := Rec.Bottom - Rec.Top;
          ColR := ColorR;
          ColG := ColorG;
          ColB := ColorB;
          Alpha := A;
          TexX1 := 0;
          TexX2 := 1;
          TexY1 := 0;
          TexY2 := 1;
        end;
        Renderer.DrawTexture(Texture);

        // right part
        Rec.Left  := Rec.Right;
        Rec.Right := Rec.Right + NotesW[0];

        If (NoteType = ntRap) or (NoteType = ntRapGolden) then
        begin
          If Color = P1_INVERTED then
            Texture := Tex_Right_Rap_Inv
          else
            Texture := Tex_Right_Rap[Color]
        end
        else
        begin
          If Color = P1_INVERTED then
            Texture := Tex_Right_Inv
          else
            Texture := Tex_Right[Color]
        end;
        with Texture do
        begin
          X := Rec.Left;
          Y := Rec.Top;
          W := Rec.Right - Rec.Left;
          H := Rec.Bottom - Rec.Top;
          ColR := ColorR;
          ColG := ColorG;
          ColB := ColorB;
          Alpha := A;
          TexX1 := 0;
          TexX2 := 1;
          TexY1 := 0;
          TexY2 := 1;
        end;
        Renderer.DrawTexture(Texture);

        if ((NoteType = ntGolden) or (NoteType = ntRapGolden)) and (Ini.EffectSing = 1) then
        begin
          GoldenRec.SaveGoldenStarsRec(GoldenStarPos, Rec.Top, Rec.Right, Rec.Bottom);
        end;
        
      end; // with
    end; // for
  end; // with
end;

procedure EditDrawBorderedBox(X, Y, W, H: integer; FillR, FillG, FillB, FillAlpha: real);
begin
  Renderer.Blend := false;
  Renderer.DrawQuad(X, Y, 0, W, H, FillR, FillG, FillB, FillAlpha);
  Renderer.DrawBoundedBox(X-1, Y-1, X+W+1, Y+H+1, 0, 2, 0, 0, 0, 1);
  Renderer.Blend := true;
end;

procedure EditDrawBeatDelimiters(X, Y, W, H: real; Track: integer);
var
  Count, I: integer;
  TempR: real;
  LineList: TLineList;
begin

  if not CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].HasLength(TempR) then TempR := 0
  else TempR := W / TempR;

  if (CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].ScoreValue > 0) and ( W > 0 ) and ( (CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].EndBeat - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat) > 0 ) then
      TempR := W / (CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].EndBeat - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat)
    else
      TempR := 0;
  SetLength(LineList, CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].EndBeat - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat + 1);
  I := 0;
  for Count := CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat to CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].EndBeat do
  begin
    with LineList[I] do
    begin
      X1 := X + TempR * (Count - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat);
      Y1 := Y;
      X2 := X + TempR * (Count - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat);
      Y2 := Y + H;
      Z := 0;
      Thickness := 2;
      ColR := 0;
      ColG := 0;
      ColB := 0;
    end;
    if (Count mod USong.DEFAULT_RESOLUTION) = CurrentSong.Tracks[Track].NotesGAP then
      LineList[I].Alpha := 1
    else
      LineList[I].Alpha := 0.3;
    I := I + 1;
  end;
  Renderer.DrawLines(LineList);
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
  end;

  if (ScreenJukebox.SongMenuVisible) then
  begin
    x := Theme.Jukebox.StaticSongMenuTimeProgress.x;
    y := Theme.Jukebox.StaticSongMenuTimeProgress.y;

    width  := Theme.Jukebox.StaticSongMenuTimeProgress.w;
    height := Theme.Jukebox.StaticSongMenuTimeProgress.h;
  end;

  CurLyricsTime := LyricsState.GetCurrentTime();
  if (CurLyricsTime > 0) and
      (LyricsState.TotalTime > 0) then
  begin
    LyricsProgress := CurLyricsTime / LyricsState.TotalTime;
    // avoid that the bar "overflows" for inaccurate song lengths
    if (LyricsProgress > 1.0) then
      LyricsProgress := 1.0;
    Tex_JukeboxTimeProgress.X := x;
    Tex_JukeboxTimeProgress.Y := y;
    Tex_JukeboxTimeProgress.W := width;
    Tex_JukeboxTimeProgress.H := height;
    Tex_JukeboxTimeProgress.ColR := Theme.Jukebox.StaticSongMenuTimeProgress.ColR;
    Tex_JukeboxTimeProgress.ColG := Theme.Jukebox.StaticSongMenuTimeProgress.ColG;
    Tex_JukeboxTimeProgress.ColB := Theme.Jukebox.StaticSongMenuTimeProgress.ColB;
    Tex_JukeboxTimeProgress.Alpha := 1;
    Tex_JukeboxTimeProgress.TexX1 := 0;
    Tex_JukeboxTimeProgress.TexY1 := 0;
    Tex_JukeboxTimeProgress.TexX2 := (width * LyricsProgress) / 8;
    Tex_JukeboxTimeProgress.TexY2 := 1;
    Renderer.DrawTexture(Tex_JukeboxTimeProgress);
  end;
end;

end.
