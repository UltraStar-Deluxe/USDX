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
  UIni,
  UPlayerLayout;

procedure SingDraw;
procedure SingDrawLines;
procedure SingDrawBackground;
procedure SingDrawOscilloscopes;
procedure SingDrawOscilloscope(Position: TThemePosition; NrSound: integer);
procedure SingDrawNoteLines(Left, Top, Right: real; LineSpacing: integer = 15);
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
      glVertex2f(RenderW,  0);
      glTexCoord2f(0, Webcam.TextureCam.TexH);
      glVertex2f(RenderW,  RenderH);
      glTexCoord2f( Webcam.TextureCam.TexW, Webcam.TextureCam.TexH);
      glVertex2f(0, RenderH);
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
  // TODO: these (especially the aspects) should just be precomputed
  ScreenAspect, TexAspect: double;  // aspect of screen resolution and image
  ScaledTexWidth, ScaledTexHeight: double;
begin
  // TODO: this is also called if a video is playing
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

      glEnable(GL_TEXTURE_2D);
      glBindTexture(GL_TEXTURE_2D, ScreenSing.Tex_Background.TexNum);
      // Ensure color and blending state is set so the texture is drawn correctly
      glColor4f(1, 1, 1, 1);
      glEnable(GL_BLEND);
      glBegin(GL_QUADS);
      // top left
      glTexCoord2f(0, 0);
      glVertex2f(Rec.Left, Rec.Top);
      // bottom left
      glTexCoord2f(0, ScreenSing.Tex_Background.TexH);
      glVertex2f(Rec.Left, Rec.Bottom);
      // bottom right
      glTexCoord2f(ScreenSing.Tex_Background.TexW, ScreenSing.Tex_Background.TexH);
      glVertex2f(Rec.Right, Rec.Bottom);
      // top right
      glTexCoord2f(ScreenSing.Tex_Background.TexW, 0);
      glVertex2f(Rec.Right, Rec.Top);
      glEnd;
      glDisable(GL_TEXTURE_2D);
      glDisable(GL_BLEND);
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
      Rec.Right := RenderW;

      TexRec.Top := (Rec.Top / RenderH) * ScreenJukebox.Tex_Background.TexH;
      TexRec.Bottom := (Rec.Bottom / RenderH) * ScreenJukebox.Tex_Background.TexH;
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
        TexRec.Bottom := (Rec.Bottom / RenderH) * ScreenJukebox.Tex_Background.TexH;
        glTexCoord2f(TexRec.Left,  TexRec.Top);    glVertex2f(Rec.Left,  Rec.Top);
        glTexCoord2f(TexRec.Left,  TexRec.Bottom); glVertex2f(Rec.Left,  Rec.Bottom);
        glTexCoord2f(TexRec.Right, TexRec.Bottom); glVertex2f(Rec.Right, Rec.Bottom);

        glTexCoord2f(TexRec.Right, TexRec.Top);    glVertex2f(Rec.Right, Rec.Top);
        (* bottom *)
        Rec.Top := Rec.Bottom;
        Rec.Bottom := 490; // 490
        TexRec.Top := TexRec.Bottom;
        TexRec.Bottom := (Rec.Bottom / RenderH) * ScreenJukebox.Tex_Background.TexH;
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
        glTexCoord2f(0,  ScreenJukebox.Tex_Background.TexH);   glVertex2f(0,  RenderH);
        glTexCoord2f( ScreenJukebox.Tex_Background.TexW,  ScreenJukebox.Tex_Background.TexH);   glVertex2f(RenderW, RenderH);
        glTexCoord2f( ScreenJukebox.Tex_Background.TexW, 0);   glVertex2f(RenderW, 0);

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
      glTexCoord2f(0,  ScreenJukebox.Tex_Background.TexH);   glVertex2f(0,  RenderH);
      glTexCoord2f( ScreenJukebox.Tex_Background.TexW,  ScreenJukebox.Tex_Background.TexH);   glVertex2f(RenderW, RenderH);
      glTexCoord2f( ScreenJukebox.Tex_Background.TexW, 0);   glVertex2f(RenderW, 0);

    glEnd;
    glDisable(GL_TEXTURE_2D);
    //glDisable(GL_BLEND);
  end;
end;

procedure SingDrawJukeboxBlackBackground;
begin
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, ScreenJukebox.Tex_Background.TexNum);
  //glEnable(GL_BLEND);
  glBegin(GL_QUADS);
    glColor4f(0, 0, 0, 1);

    glTexCoord2f(0, 0);   glVertex2f(0,  0);
    glTexCoord2f(0,  ScreenJukebox.Tex_Background.TexH);   glVertex2f(0,  RenderH);
    glTexCoord2f( ScreenJukebox.Tex_Background.TexW,  ScreenJukebox.Tex_Background.TexH);   glVertex2f(RenderW, RenderH);
    glTexCoord2f( ScreenJukebox.Tex_Background.TexW, 0);   glVertex2f(RenderW, 0);

  glEnd;
  glDisable(GL_TEXTURE_2D);
  //glDisable(GL_BLEND);
end;

procedure SingDrawOscilloscopes;
  function GetBaseOscilloscopePosition: TThemePosition;
  begin
    Result := Theme.Sing.PlayerTemplate.Oscilloscope;
  end;
  function GetBaseSingPlayerTemplate: TThemeSingPlayer;
  begin
    Result := Theme.Sing.PlayerTemplate;
  end;
  procedure GetLaneLayout(const PlayerCountOnScreen, PlayerIndexOnScreen: integer;
    out LaneLeft, LaneRight, LaneTop, LaneWidth: integer);
  var
    Layout: TSingLaneLayout;
  begin
    Layout := GetSingLaneLayout(PlayerCountOnScreen, PlayerIndexOnScreen, Theme.Sing.PlayerLayout,
      CurrentSong.isDuet and (PlayersPlay <> 1));
    LaneLeft := Layout.ColumnLeft;
    LaneRight := Layout.ColumnRight;
    LaneTop := Layout.RowAnchorY;
    LaneWidth := Layout.ColumnWidth;
  end;
  function GetOscilloscopePosition(PlayerIndex: integer): TThemePosition;
  var
    BaseTemplate: TThemeSingPlayer;
    BasePosition: TThemePosition;
    LocalPlayerCount: integer;
    LocalIndex: integer;
    LaneLeft: integer;
    LaneRight: integer;
    LaneTop: integer;
    LaneWidth: integer;
    Scale: real;
    FrameW: integer;
    FrameH: integer;
    ScoreW: integer;
    ScoreH: integer;
    NameX: integer;
    NameY: integer;
    NameW: integer;
    GroupTop: integer;
    HeaderOffsetLeft: integer;
    Layout: TSingLaneLayout;
  begin
    if Screens > 1 then
    begin
      LocalPlayerCount := GetScreenPlayerCount(PlayersPlay, Screens, ScreenAct);
      LocalIndex := GetPlayerIndexOnScreen(PlayerIndex, PlayersPlay, Screens);
    end
    else
    begin
      LocalPlayerCount := PlayersPlay;
      LocalIndex := PlayerIndex;
    end;

    BaseTemplate := GetBaseSingPlayerTemplate;
    BasePosition := GetBaseOscilloscopePosition;
    Layout := GetSingLaneLayout(LocalPlayerCount, LocalIndex, Theme.Sing.PlayerLayout,
      CurrentSong.isDuet and (PlayersPlay <> 1));
    GetLaneLayout(LocalPlayerCount, LocalIndex, LaneLeft, LaneRight, LaneTop, LaneWidth);
    Scale := Layout.WidgetScale;

    FrameW := Max(Theme.Sing.PlayerWidgetLayout.MinFrameW, Round(BaseTemplate.AvatarFrame.W * Scale));
    FrameH := Max(Theme.Sing.PlayerWidgetLayout.MinFrameH, Round(BaseTemplate.AvatarFrame.H * Scale));
    ScoreW := Max(Theme.Sing.PlayerWidgetLayout.MinScoreW, Round(BaseTemplate.ScoreBackground.W * Scale));
    ScoreH := Max(Theme.Sing.PlayerWidgetLayout.MinScoreH, Round(BaseTemplate.ScoreBackground.H * Scale));
    HeaderOffsetLeft := Round(Theme.Sing.PlayerWidgetLayout.HeaderOffsetLeft * Scale);
    GroupTop := Max(10, LaneTop -
      GetSingHeaderTopOffset(Theme.Sing.PlayerWidgetLayout, Layout.GridRows, Scale));
    NameX := Max(0, LaneLeft - HeaderOffsetLeft) + FrameW +
      Max(Theme.Sing.PlayerWidgetLayout.NameGapMinX, Round(Theme.Sing.PlayerWidgetLayout.NameGapBaseX * Scale));
    NameW := Max(Theme.Sing.PlayerWidgetLayout.NameMinW,
      (LaneRight - ScoreW - Max(Theme.Sing.PlayerWidgetLayout.NameGapMinX,
      Round(Theme.Sing.PlayerWidgetLayout.NameGapBaseX * Scale))) - NameX);
    NameY := GroupTop + Max(0, (FrameH - Max(12, Round(BaseTemplate.Name.Size * Scale))) div 2);
    NameX := Max(0, NameX - Max(Theme.Sing.PlayerWidgetLayout.NamePaddingMinX,
      Round(Theme.Sing.PlayerWidgetLayout.NamePaddingBaseX * Scale)));
    NameY := Max(0, NameY - Max(Theme.Sing.PlayerWidgetLayout.NamePaddingMinY,
      Round(Theme.Sing.PlayerWidgetLayout.NamePaddingBaseY * Scale)));

    Result := BasePosition;
    Result.X := NameX;
    Result.Y := NameY + Max(12, Round(BaseTemplate.Name.H * Scale)) +
      Max(Theme.Sing.PlayerWidgetLayout.OscilloscopeGapMinY,
      Round(Theme.Sing.PlayerWidgetLayout.OscilloscopeGapBaseY * Scale));
    Result.W := Min(NameW, Max(Theme.Sing.PlayerWidgetLayout.OscilloscopeMinW, Round(BasePosition.W * Scale)));
    Result.H := Max(Theme.Sing.PlayerWidgetLayout.OscilloscopeMinH, Round(BasePosition.H * Scale));
  end;
var
  PlayerIndex: integer;
begin;
  for PlayerIndex := 0 to PlayersPlay - 1 do
    if (Screens <= 1) or (GetPlayerScreen(PlayerIndex, PlayersPlay, Screens) = ScreenAct) then
      SingDrawOscilloscope(GetOscilloscopePosition(PlayerIndex), PlayerIndex);
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

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glTranslatef(Position.X, Position.Y + MaxY, 0);
  glScalef(MaxX/High(Sound.AnalysisBuffer), MaxY/Low(Smallint), 1);

  Sound.LockAnalysisBuffer();

  glBegin(GL_LINE_STRIP);
    for SampleIndex := 0 to High(Sound.AnalysisBuffer) do
    begin
      glVertex2s(SampleIndex, Sound.AnalysisBuffer[SampleIndex]);
    end;
  glEnd;

  Sound.UnlockAnalysisBuffer();

  glPopMatrix();
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
procedure SingDrawLine(Left, Top, Right: real; Track, PlayerIndex: integer; LineSpacing: integer);
var
  Rec:   TRecR;
  Count: integer;
  TempR: real;
  PlayerNumber: integer;

  GoldenStarPos: real;
begin
  if (ScreenSing.settings.NotesVisible[Track]) then
  begin
    // the textures start counting at 1, but everything else just starts at 0
    PlayerNumber := PlayerIndex + 1;
    glColor3f(1, 1, 1);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

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
            Rec.Left  := (StartBeat - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + Left + 0.5;
            Rec.Right := Rec.Left + NotesW[PlayerIndex];
            Rec.Top := Top - (Tone-BaseNote)*LineSpacing/2 - NotesH[PlayerIndex];
            Rec.Bottom := Rec.Top + 2 * NotesH[PlayerIndex];
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
            Rec.Right := (StartBeat + Duration - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + Left - NotesW[PlayerIndex] - 0.5;

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
            Rec.Right := Rec.Right + NotesW[PlayerIndex];


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
procedure SingDrawPlayerLine(Left, Top, W: real; Track, PlayerIndex: integer; LineSpacing: integer);
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
          Rec.Right := Left + (Start + Duration - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR - NotesW[PlayerIndex] - 0.5;

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
  GlowPadX:       real;
  GlowExtraW:     real;
  GlowExtraH:     real;
begin
  if (ScreenSing.settings.NotesVisible[PlayerIndex]) then
  begin
    //glColor4f(1, 1, 1, sqrt((1+sin( AudioPlayback.Position * 3))/4)/ 2 + 0.5 );
    glColor4f(1, 1, 1, sqrt((1 + sin(AudioPlayback.Position * 3)))/2 + 0.05);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

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
            GlowPadX := Max(1.0, NotesW[PlayerIndex] * 0.65);
            GlowExtraW := Max(1.0, NotesW[PlayerIndex] * 0.35);
            GlowExtraH := Max(1.5, NotesH[PlayerIndex] * 0.3);
            W := NotesW[PlayerIndex] * 2 + GlowExtraW;
            H := NotesH[PlayerIndex] * 1.5 + GlowExtraH;

            {
            X2 := (Start-CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].Current].Notes[0].Start) * TempR + Left + 0.5 + 4;
            X1 := X2-W;

            X3 := (Start+Length-CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].Current].Notes[0].Start) * TempR + Left - 0.5 - 4;
            X4 := X3+W;
            }

            // left
            Rec.Right := (StartBeat - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + Left + 0.5 + GlowPadX;
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
            Rec.Right := (StartBeat + Duration - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + Left - 0.5 - GlowPadX;

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
      glEnable(GL_TEXTURE_2D);
      glEnable(GL_BLEND);

      if (CurrentSong.isDuet) then
      begin
        if (Screens > 1) and (GetScreenPlayerCount(PlayersPlay, Screens, ScreenAct) = 2) then
          Col := GetLyricBarColor(Ini.SingColor[GetFirstPlayerIndexForScreen(PlayersPlay, Screens, ScreenAct) + CP])
        else
          Col := GetLyricBarColor(Ini.SingColor[CP]);
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
      glDisable(GL_TEXTURE_2D);
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
  PlayerIndex: integer;
  LocalIndex: integer;
  LineTop: real;
  LineSpacing: integer;
  PlayerCountOnScreen: integer;
  LaneLeft: real;
  LaneRight: real;
  procedure GetLaneLayout(const CurrentPlayerIndex: integer; out Left, Right, Top: real; out Spacing: integer);
  var
    Layout: TSingLaneLayout;
  begin
    if Screens > 1 then
    begin
      PlayerCountOnScreen := GetScreenPlayerCount(PlayersPlay, Screens, ScreenAct);
      LocalIndex := GetPlayerIndexOnScreen(CurrentPlayerIndex, PlayersPlay, Screens);
    end
    else
    begin
      PlayerCountOnScreen := PlayersPlay;
      LocalIndex := CurrentPlayerIndex;
    end;

    Layout := GetSingLaneLayout(PlayerCountOnScreen, LocalIndex, Theme.Sing.PlayerLayout,
      CurrentSong.isDuet and (PlayersPlay <> 1));
    Left := Layout.GridLeft;
    Right := Layout.GridRight;
    Top := Layout.GuideTopY;
    Spacing := Layout.NoteLineSpacing;
  end;
begin
  // positions
  NR.Left := 20;
  NR.Right := 780;
  NR.Width := 760; //NR.Right - NR.Left;
  NR.WMid  := 380;//NR.Width / 2;
  NR.Mid   := 400;//NR.Left + NR.WMid;

  // draw note-lines

  if Ini.NoteLines <> 1 then
    Exit;

  for PlayerIndex := 0 to PlayersPlay - 1 do
  begin
    if (Screens > 1) and (GetPlayerScreen(PlayerIndex, PlayersPlay, Screens) <> ScreenAct) then
      Continue;
    if not ScreenSing.Settings.NotesVisible[PlayerIndex] then
      Continue;

    GetLaneLayout(PlayerIndex, LaneLeft, LaneRight, LineTop, LineSpacing);
    SingDrawNoteLines(LaneLeft, LineTop, LaneRight, LineSpacing);
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
  PlayerCountOnScreen: integer;
  PlayerIndex: integer;
  LocalIndex: integer;
  LineTop: real;
  LineSpacing: integer;
  TrackIndex: integer;
  LaneLeft: real;
  LaneRight: real;
  LaneWidth: real;
  Layout: TSingLaneLayout;
  ContentScale: real;
  BaseNoteH: real;
  BaseNoteW: real;
  procedure GetLaneLayout(const CurrentPlayerIndex: integer; out Left, Right, Width, Top: real; out Spacing: integer);
  begin
    if Screens > 1 then
    begin
      PlayerCountOnScreen := GetScreenPlayerCount(PlayersPlay, Screens, ScreenAct);
      LocalIndex := GetPlayerIndexOnScreen(CurrentPlayerIndex, PlayersPlay, Screens);
    end
    else
    begin
      PlayerCountOnScreen := PlayersPlay;
      LocalIndex := CurrentPlayerIndex;
    end;

    Layout := GetSingLaneLayout(PlayerCountOnScreen, LocalIndex, Theme.Sing.PlayerLayout,
      CurrentSong.isDuet and (PlayersPlay <> 1));
    Left := Layout.GridLeft;
    Right := Layout.GridRight;
    Width := Right - Left;
    Top := Layout.RowAnchorY;
    Spacing := Layout.NoteLineSpacing;
  end;
begin
  // positions
  NR.Left := 20;
  NR.Right := 780;
  NR.Width := 760; //NR.Right - NR.Left;
  NR.WMid  := 380; //NR.Width / 2;
  NR.Mid   := 400; //NR.Left + NR.WMid;
  // FIXME: accessing ScreenSing is not that generic
  if (CurrentSong.isDuet) and (PlayersPlay <> 1) then
  begin
    LyricEngineDuetP1 := ScreenSing.LyricsDuetP1;
    LyricEngineDuetP2 := ScreenSing.LyricsDuetP2;
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
          BaseNoteH := 11;
          BaseNoteW := 6;
        end;
      1:
        begin
          BaseNoteH := 8;
          BaseNoteW := 4;
        end;
      2:
        begin
          BaseNoteH := 5;
          BaseNoteW := 3;
        end;
    else
      begin
        BaseNoteH := 8;
        BaseNoteW := 4;
      end;
    end;

    if Screens > 1 then
    begin
      PlayerCountOnScreen := GetScreenPlayerCount(PlayersPlay, Screens, ScreenAct);
      LocalIndex := GetPlayerIndexOnScreen(I - 1, PlayersPlay, Screens);
    end
    else
    begin
      PlayerCountOnScreen := PlayersPlay;
      LocalIndex := I - 1;
    end;

    Layout := GetSingLaneLayout(PlayerCountOnScreen, LocalIndex, Theme.Sing.PlayerLayout,
      CurrentSong.isDuet and (PlayersPlay <> 1));
    ContentScale := Layout.ContentScale;
    NotesH[I - 1] := Max(2.0, BaseNoteH * ContentScale);
    NotesW[I - 1] := Max(1.0, BaseNoteW * ContentScale);
  end;

  // draw notes lines
  if (ScreenSing.Settings.InputVisible) then
    SingDrawLines;
  // Draw the Notes
  for PlayerIndex := 0 to PlayersPlay - 1 do
  begin
    if (Screens > 1) and (GetPlayerScreen(PlayerIndex, PlayersPlay, Screens) <> ScreenAct) then
      Continue;

    GetLaneLayout(PlayerIndex, LaneLeft, LaneRight, LaneWidth, LineTop, LineSpacing);

    TrackIndex := 0;
    if (CurrentSong.isDuet) and (PlayersPlay <> 1) and Odd(PlayerIndex) then
      TrackIndex := 1;

    SingDrawPlayerBGLine(LaneLeft, LineTop, LaneRight, TrackIndex, PlayerIndex, LineSpacing);
    SingDrawLine(LaneLeft, LineTop, LaneRight, TrackIndex, PlayerIndex, LineSpacing);
    SingDrawPlayerLine(LaneLeft, LineTop, LaneWidth, TrackIndex, PlayerIndex, LineSpacing);
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
          ntFreestyle: glColor4f(1, 1, 1, 0.35);
          ntNormal: glColor4f(1, 1, 1, 0.85);
          ntGolden: Glcolor4f(1, 1, 0.3, 0.85);
          ntRap: glColor4f(1, 1, 1, 0.85);
          ntRapGolden: Glcolor4f(1, 1, 0.3, 0.85);
        end; // case

        // left part
        Rec.Left  := (StartBeat - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + X + 0.5;
        Rec.Right := Rec.Left + NotesW[0];
        Rec.Top := YBaseNote - (Tone-BaseNote)*Space/2 - NotesH[0];
        Rec.Bottom := Rec.Top + 2 * NotesH[0];
        If (NoteType = ntRap) or (NoteType = ntRapGolden) then
        begin
          If Color = P1_INVERTED then
            glBindTexture(GL_TEXTURE_2D, Tex_Left_Rap_Inv.TexNum)
          else
            glBindTexture(GL_TEXTURE_2D, Tex_Left_Rap[Color].TexNum)
        end
        else
        begin
          If Color = P1_INVERTED then
            glBindTexture(GL_TEXTURE_2D, Tex_Left_Inv.TexNum)
          else
            glBindTexture(GL_TEXTURE_2D, Tex_Left[Color].TexNum)
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
        Rec.Right := (StartBeat + Duration - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + X - NotesW[0] - 0.5;

        If (NoteType = ntRap) or (NoteType = ntRapGolden) then
        begin
          If Color = P1_INVERTED then
            glBindTexture(GL_TEXTURE_2D, Tex_Mid_Rap_Inv.TexNum)
          else
            glBindTexture(GL_TEXTURE_2D, Tex_Mid_Rap[Color].TexNum)
        end
        else
        begin
          If Color = P1_INVERTED then
            glBindTexture(GL_TEXTURE_2D, Tex_Mid_Inv.TexNum)
          else
            glBindTexture(GL_TEXTURE_2D, Tex_Mid[Color].TexNum)
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
          If Color = P1_INVERTED then
            glBindTexture(GL_TEXTURE_2D, Tex_Right_Rap_Inv.TexNum)
          else
            glBindTexture(GL_TEXTURE_2D, Tex_Right_Rap[Color].TexNum)
        end
        else
        begin
          If Color = P1_INVERTED then
          begin
            glBindTexture(GL_TEXTURE_2D, Tex_Right_Inv.TexNum);
          end
          else
            glBindTexture(GL_TEXTURE_2D, Tex_Right[Color].TexNum)
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

  if not CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].HasLength(TempR) then TempR := 0
  else TempR := W / TempR;

  if (CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].ScoreValue > 0) and ( W > 0 ) and ( (CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].EndBeat - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat) > 0 ) then
      TempR := W / (CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].EndBeat - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat)
    else
      TempR := 0;
  glEnable(GL_BLEND);
  glBegin(GL_LINES);
  for Count := CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat to CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].EndBeat do
  begin
    if (Count mod CurrentSong.Tracks[Track].Resolution) = CurrentSong.Tracks[Track].NotesGAP then
      glColor4f(0, 0, 0, 1)
    else
      glColor4f(0, 0, 0, 0.3);
    glVertex2f(X + TempR * (Count - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat), Y);
    glVertex2f(X + TempR * (Count - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat), Y + H);
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
