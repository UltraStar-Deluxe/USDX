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

unit ULyrics;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  gl,
  glext,
  UTexture,
  UThemes,
  UMusic;

type
  // stores two textures for enabled/disabled states
  TPlayerIconTex = array [0..1] of TTexture;

  TLyricsEffect = (lfxSimple, lfxZoom, lfxSlide, lfxBall, lfxShift);
  
  PLyricWord = ^TLyricWord;
  TLyricWord = record
    X:          real;     // left corner
    Width:      real;     // width
    Start:      cardinal; // start of the word in quarters (beats)
    Length:     cardinal; // length of the word in quarters
    Text:       string;   // text
    Freestyle:  boolean;  // is freestyle?
  end;
  TLyricWordArray = array of TLyricWord;

  TLyricLine = class
    public
      Text:           string;       // text
      Width:          real;         // width
      Height:         real;         // height
      Words:          TLyricWordArray;   // words in this line
      CurWord:        integer;      // current active word idx (only valid if line is active)
      Start:          integer;      // start of this line in quarters (Note: negative start values are possible due to gap)
      StartNote:      integer;      // start of the first note of this line in quarters
      Length:         integer;      // length in quarters (from start of first to the end of the last note)
      Players:        byte;         // players that should sing that line (bitset, Player1: 1, Player2: 2, Player3: 4)
      LastLine:       boolean;      // is this the last line of the song?

      constructor Create();
      destructor Destroy(); override;
      procedure Reset();
  end;

  TLyricEngine = class
    private
      LastDrawBeat:   real;
      UpperLine:      TLyricLine;    // first line displayed (top)
      LowerLine:      TLyricLine;    // second lind displayed (bottom)
      QueueLine:      TLyricLine;    // third line (will be displayed when lower line is finished)

      IndicatorTex:   TTexture;      // texture for lyric indikator
      BallTex:        TTexture;      // texture of the ball for the lyric effect

      QueueFull:      boolean;       // set to true if the queue is full and a line will be replaced with the next AddLine
      LCounter:       integer;       // line counter

      // duet mode - textures for player icons
      // FIXME: do not use a fixed player count, use MAX_PLAYERS instead
      PlayerIconTex:  array[0..5] of TPlayerIconTex;

      // Some helper procedures for lyric drawing
      procedure DrawLyrics (Beat: real);
      procedure UpdateLineMetrics(LyricLine: TLyricLine);
      procedure DrawLyricsWords(LyricLine: TLyricLine; X, Y: real; StartWord, EndWord: integer);
      procedure DrawLyricsLine(X, W, Y, H: real; Line: TLyricLine; Beat: real);
      procedure DrawPlayerIcon(Player: byte; Enabled: boolean; X, Y: real; Size, Alpha: real);
      procedure DrawBall(XBall, YBall, Alpha: real);

    public
      // positions, line specific settings
      UpperLineX:     real;       // X start-pos of UpperLine
      UpperLineW:     real;       // Width of UpperLine with icon(s) and text
      UpperLineY:     real;       // Y start-pos of UpperLine
      UpperLineH:     real;       // Max. font-size of lyrics text in UpperLine

      LowerLineX:     real;       // X start-pos of LowerLine
      LowerLineW:     real;       // Width of LowerLine with icon(s) and text
      LowerLineY:     real;       // Y start-pos of LowerLine
      LowerLineH:     real;       // Max. font-size of lyrics text in LowerLine

      // display propertys
      LineColor_en:   TRGBA;      // Color of words in an enabled line
      LineColor_dis:  TRGBA;      // Color of words in a disabled line
      LineColor_act:  TRGBA;      // Color of the active word
      FontStyle:      byte;       // Font for the lyric text
      
      { // currently not used
       FadeInEffect:   byte;       // Effect for line fading in: 0: No Effect; 1: Fade Effect; 2: Move Upwards from Bottom to Pos
       FadeOutEffect:  byte;       // Effect for line fading out: 0: No Effect; 1: Fade Effect; 2: Move Upwards
      }

      // song specific settings
      BPM:            real;
      Resolution:     integer;

      // properties to easily read options of this class
      property IsQueueFull: boolean read QueueFull;  // line in queue?
      property LineCounter: integer read LCounter;   // lines that were progressed so far (after last clear)

      procedure AddLine(Line: PLine);              // adds a line to the queue, if there is space
      procedure Draw (Beat: real);                 // draw the current (active at beat) lyrics

      // clears all cached song specific information
      procedure Clear(cBPM: real = 0; cResolution: integer = 0);

      function GetUpperLine(): TLyricLine;
      function GetLowerLine(): TLyricLine;

      function GetUpperLineIndex(): integer;

      constructor Create(ULX, ULY, ULW, ULH, LLX, LLY, LLW, LLH: real);
      procedure   LoadTextures;
      destructor  Destroy; override;
  end;

implementation

uses
  SysUtils,
  USkins,
  TextGL,
  UGraphic,
  UDisplay,
  ULog,
  math,
  UIni;

{ TLyricLine }

constructor TLyricLine.Create();
begin
  inherited;
  Reset();
end;

destructor TLyricLine.Destroy();
begin
  SetLength(Words, 0);
  inherited;
end;

procedure TLyricLine.Reset();
begin
  Start     := 0;
  StartNote := 0;
  Length    := 0;
  LastLine  := False;

  Text      := '';
  Width     := 0;

  // duet mode: players of that line (default: all)
  Players   := $FF;

  SetLength(Words, 0);
  CurWord   := -1;
end;


{ TLyricEngine }

{**
 * Initializes the engine.
 *}
constructor TLyricEngine.Create(ULX, ULY, ULW, ULH, LLX, LLY, LLW, LLH: real);
begin
  inherited Create();

  BPM := 0;
  Resolution := 0;
  LCounter := 0;
  QueueFull := False;

  UpperLine := TLyricLine.Create;
  LowerLine := TLyricLine.Create;
  QueueLine := TLyricLine.Create;

  LastDrawBeat := 0;

  UpperLineX := ULX;
  UpperLineW := ULW;
  UpperLineY := ULY;
  UpperLineH := ULH;

  LowerLineX := LLX;
  LowerLineW := LLW;
  LowerLineY := LLY;
  LowerLineH := LLH;

  LoadTextures;
end;


{**
 * Frees memory.
 *}
destructor TLyricEngine.Destroy;
begin
  UpperLine.Free;
  LowerLine.Free;
  QueueLine.Free;
  inherited;
end;

{**
 * Clears all cached Song specific Information.
 *}
procedure TLyricEngine.Clear(cBPM: real; cResolution: integer);
begin
  BPM := cBPM;
  Resolution := cResolution;
  LCounter := 0;
  QueueFull := False;

  LastDrawBeat:=0;
end;


{**
 * Loads textures needed for the drawing the lyrics,
 * player icons, a ball for the ball effect and the lyric indicator.
 *}
procedure TLyricEngine.LoadTextures;
var
  I: Integer;
begin
  // lyric indicator (bar that indicates when the line start)
  IndicatorTex := Texture.LoadTexture(Skin.GetTextureFileName('LyricHelpBar'), TEXTURE_TYPE_TRANSPARENT, $FF00FF);

  // ball for current word hover in ball effect
  BallTex := Texture.LoadTexture(Skin.GetTextureFileName('Ball'), TEXTURE_TYPE_TRANSPARENT, 0);

  // duet mode: load player icon
  for I := 0 to 5 do
  begin
    PlayerIconTex[I][0] := Texture.LoadTexture(Skin.GetTextureFileName('LyricIcon_P' + InttoStr(I+1)), TEXTURE_TYPE_TRANSPARENT, 0);
    PlayerIconTex[I][1] := Texture.LoadTexture(Skin.GetTextureFileName('LyricIconD_P' + InttoStr(I+1)), TEXTURE_TYPE_TRANSPARENT, 0);
  end;
end;

{**
 * Adds LyricLine to queue.
 * The LyricEngine stores three lines in its queue:
 *   UpperLine: the upper line displayed in the lyrics
 *   LowerLine: the lower line displayed in the lyrics
 *   QueueLine: an offscreen line that precedes LowerLine
 * If the queue is full the next call to AddLine will replace UpperLine with
 * LowerLine, LowerLine with QueueLine and QueueLine with the Line parameter.
 *}
procedure TLyricEngine.AddLine(Line: PLine);
var
  LyricLine: TLyricLine;
  I: integer;
begin
  // only add lines, if there is space
  if not IsQueueFull then
  begin
    // set LyricLine to line to write to
    if (LineCounter = 0) then
      LyricLine := UpperLine
    else if (LineCounter = 1) then
      LyricLine := LowerLine
    else
    begin
      // now the queue is full
      LyricLine := QueueLine;
      QueueFull := True;
    end;
  end
  else
  begin // rotate lines (round-robin-like)
    LyricLine := UpperLine;
    UpperLine := LowerLine;
    LowerLine := QueueLine;
    QueueLine := LyricLine;
  end;

  // reset line state
  LyricLine.Reset();

  // check if sentence has notes
  if (Line <> nil) and (Length(Line.Note) > 0) then
  begin
    // copy values from SongLine to LyricLine
    LyricLine.Start     := Line.Start;
    LyricLine.StartNote := Line.Note[0].Start;
    LyricLine.Length    := Line.Note[High(Line.Note)].Start +
                           Line.Note[High(Line.Note)].Length -
                           Line.Note[0].Start;
    LyricLine.LastLine  := Line.LastLine;

    // copy words
    SetLength(LyricLine.Words, Length(Line.Note));
    for I := 0 to High(Line.Note) do
    begin
      LyricLine.Words[I].Start     := Line.Note[I].Start;
      LyricLine.Words[I].Length    := Line.Note[I].Length;
      LyricLine.Words[I].Text      := Line.Note[I].Text;
      LyricLine.Words[I].Freestyle := Line.Note[I].NoteType = ntFreestyle;

      LyricLine.Text := LyricLine.Text + LyricLine.Words[I].Text;
    end;

    UpdateLineMetrics(LyricLine);
  end;

  // increase the counter
  Inc(LCounter);
end;

{**
 * Draws Lyrics.
 * Draw just manages the Lyrics, drawing is done by a call of DrawLyrics.
 * @param Beat: current Beat in Quarters
 *}
procedure TLyricEngine.Draw(Beat: real);
begin
  DrawLyrics(Beat);
  LastDrawBeat := Beat;
end;

{**
 * Main Drawing procedure.
 *}
procedure TLyricEngine.DrawLyrics(Beat: real);
begin
  DrawLyricsLine(UpperLineX, UpperLineW, UpperLineY, UpperLineH, UpperLine, Beat);
  DrawLyricsLine(LowerLineX, LowerLineW, LowerLineY, LowerLineH, LowerLine, Beat);
end;

{**
 * Draws a Player's icon.
 *}
procedure TLyricEngine.DrawPlayerIcon(Player: byte; Enabled: boolean; X, Y: real; Size, Alpha: real);
var
  IEnabled: byte;
begin
  if Enabled then
    IEnabled := 0
  else
    IEnabled := 1;

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindTexture(GL_TEXTURE_2D, PlayerIconTex[Player][IEnabled].TexNum);

  glColor4f(1, 1, 1, Alpha);
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex2f(X, Y);
    glTexCoord2f(0, 1); glVertex2f(X, Y + Size);
    glTexCoord2f(1, 1); glVertex2f(X + Size, Y + Size);
    glTexCoord2f(1, 0); glVertex2f(X + Size, Y);
  glEnd;

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

{**
 * Draws the Ball over the LyricLine if needed.
 *}
procedure TLyricEngine.DrawBall(XBall, YBall, Alpha: real);
begin
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindTexture(GL_TEXTURE_2D, BallTex.TexNum);

  glColor4f(1, 1, 1, Alpha);
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex2f(XBall - 10, YBall);
    glTexCoord2f(0, 1); glVertex2f(XBall - 10, YBall + 20);
    glTexCoord2f(1, 1); glVertex2f(XBall + 10, YBall + 20);
    glTexCoord2f(1, 0); glVertex2f(XBall + 10, YBall);
  glEnd;

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure TLyricEngine.DrawLyricsWords(LyricLine: TLyricLine;
    X, Y: real; StartWord, EndWord: integer);
var
  I: integer;
  PosX: real;
  CurWord: PLyricWord;
begin
  PosX := X;

  // set word positions and line size and draw the line
  for I := StartWord to EndWord do
  begin
    CurWord := @LyricLine.Words[I];
    SetFontItalic(CurWord.Freestyle);
    SetFontPos(PosX, Y);
    glPrint(CurWord.Text);
    PosX := PosX + CurWord.Width;
  end;
end;

procedure TLyricEngine.UpdateLineMetrics(LyricLine: TLyricLine);
var
  I: integer;
  PosX: real;
  CurWord: PLyricWord;
  RequestWidth, RequestHeight: real;
begin
  PosX := 0;

  // setup font
  SetFontStyle(FontStyle);
  ResetFont();

  // check if line is lower or upper line and set sizes accordingly
  // Note: at the moment upper and lower lines have same width/height
  // and this function is just called by AddLine() but this may change
  // so that it is called by DrawLyricsLine().
  //if (LyricLine = LowerLine) then
  //begin
  //  RequestWidth  := LowerLineW;
  //  RequestHeight := LowerLineH;
  //end
  //else
  //begin
    RequestWidth  := UpperLineW;
    RequestHeight := UpperLineH;
  //end;

  // set font size to a reasonable value
  LyricLine.Height := RequestHeight * 0.9;
  SetFontSize(LyricLine.Height);
  LyricLine.Width := glTextWidth(LyricLine.Text);

  // change font-size to fit into the lyric bar
  if (LyricLine.Width > RequestWidth) then
  begin
    LyricLine.Height := Trunc(LyricLine.Height * (RequestWidth / LyricLine.Width));
    // the line is very loooong, set font to at least 1px
    if (LyricLine.Height < 1) then
      LyricLine.Height := 1;

    SetFontSize(LyricLine.Height);
    LyricLine.Width := glTextWidth(LyricLine.Text);
  end;

  // calc word positions and widths
  for I := 0 to High(LyricLine.Words) do
  begin
    CurWord := @LyricLine.Words[I];

    // - if current word is italic but not the next word get the width of the
    // italic font to avoid overlapping.
    // - if two italic words follow each other use the normal style's
    // width otherwise the spacing between the words will be too big.
    // - if it is the line's last word use normal width  
    if CurWord.Freestyle and
       (I+1 < Length(LyricLine.Words)) and
       (not LyricLine.Words[I+1].Freestyle) then
    begin
      SetFontItalic(true);
    end;

    CurWord.X := PosX;
    CurWord.Width := glTextWidth(CurWord.Text);
    PosX := PosX + CurWord.Width;
    SetFontItalic(false);
  end;
end;


{**
 * Draws one LyricLine
 *}
procedure TLyricEngine.DrawLyricsLine(X, W, Y, H: real; Line: TLyricLine; Beat: real);
var
  CurWord:        PLyricWord;     // current word
  LastWord:       PLyricWord;     // last word in line
  NextWord:       PLyricWord;     // word following current word
  Progress:       real;           // progress of singing the current word
  LyricX, LyricY: real;           // left/top lyric position
  WordY: real;                    // word y-position
  LyricsEffect: TLyricsEffect;
  Alpha: real;                    // alphalevel to fade out at end
  ClipPlaneEq: array[0..3] of GLdouble; // clipping plane for slide effect 
  {// duet mode
  IconSize: real;                 // size of player icons
  IconAlpha: real;                // alpha level of player icons
  }
begin
  // do not draw empty lines
  if (Length(Line.Words) = 0) then
    Exit;

  {
  // duet mode
  IconSize := (2 * Height);
  IconAlpha := Frac(Beat/(Resolution*4));

  DrawPlayerIcon (0, True, X, Y + (42 - IconSize) / 2 , IconSize, IconAlpha);
  DrawPlayerIcon (1, True, X + IconSize + 1,  Y + (42 - IconSize) / 2, IconSize, IconAlpha);
  DrawPlayerIcon (2, True, X + (IconSize + 1)*2, Y + (42 - IconSize) / 2, IconSize, IconAlpha);
  }

  // set font size and style
  SetFontStyle(FontStyle);
  ResetFont();
  SetFontSize(Line.Height);

  // center lyrics
  LyricX := X + (W - Line.Width) / 2;
  LyricY := Y + (H - Line.Height) / 2;
  // get lyrics effect
  LyricsEffect := TLyricsEffect(Ini.LyricsEffect);

  // TODO: what about alpha in freetype outline fonts?
  Alpha := 1;

  // check if this line is active (at least its first note must be active)
  if (Beat >= Line.StartNote) then
  begin
    // if this line just got active, CurWord is -1,
    // this means we should try to make the first word active
    if (Line.CurWord = -1) then
      Line.CurWord := 0;

    // check if the current active word is still active.
    // Otherwise proceed to the next word if there is one in this line.
    // Note: the max. value of Line.CurWord is High(Line.Words)
    if (Line.CurWord < High(Line.Words)) and
       (Beat >= Line.Words[Line.CurWord + 1].Start) then
    begin
      Inc(Line.CurWord);
    end;

    // determine current and last word in this line.
    // If the end of the line is reached use the last word as current word.
    LastWord := @Line.Words[High(Line.Words)];
    CurWord := @Line.Words[Line.CurWord];
    if (Line.CurWord+1 < Length(Line.Words)) then
      NextWord := @Line.Words[Line.CurWord+1]
    else
      NextWord := nil;

    // calc the progress of the lyrics effect
    Progress := (Beat - CurWord.Start) / CurWord.Length;
    if (Progress >= 1) then
      Progress := 1;
    if (Progress <= 0) then
      Progress := 0;

    // last word of this line finished, but this line did not hide -> fade out
    if Line.LastLine and
       (Beat > LastWord.Start + LastWord.Length) then
    begin
      Alpha := 1 - (Beat - (LastWord.Start + LastWord.Length)) / 15;
      if (Alpha < 0) then
        Alpha := 0;
    end;

    // draw sentence before current word
    if (LyricsEffect in [lfxSimple, lfxBall, lfxShift]) then
      // only highlight current word and not that ones before in this line
      glColorRGB(LineColor_en, Alpha)
    else
      glColorRGB(LineColor_act, Alpha);
    DrawLyricsWords(Line, LyricX, LyricY, 0, Line.CurWord-1);

    // draw rest of sentence (without current word)
    glColorRGB(LineColor_en, Alpha);
    if (NextWord <> nil) then
    begin
      DrawLyricsWords(Line, LyricX + NextWord.X, LyricY,
                      Line.CurWord+1, High(Line.Words));
    end;

    // draw current word
    if (LyricsEffect in [lfxSimple, lfxBall, lfxShift]) then
    begin
      if (LyricsEffect = lfxShift) then
        WordY := LyricY - 8 * (1-Progress)
      else
        WordY := LyricY;

      // change the color of the current word
      glColor4f(LineColor_act.r, LineColor_act.g, LineColor_act.b, Alpha);
      DrawLyricsWords(Line, LyricX + CurWord.X, WordY, Line.CurWord, Line.CurWord);
    end
    // change color and zoom current word
    else if (LyricsEffect = lfxZoom) then
    begin
      glPushMatrix;

      // zoom at word center
      glTranslatef(LyricX + CurWord.X + CurWord.Width/2,
                   LyricY + Line.Height/2, 0);
      glScalef(1.0 + (1-Progress) * 0.5, 1.0 + (1-Progress) * 0.5, 1.0);

      glColor4f(LineColor_act.r, LineColor_act.g, LineColor_act.b, Alpha);
      DrawLyricsWords(Line, -CurWord.Width/2, -Line.Height/2, Line.CurWord, Line.CurWord);

      glPopMatrix;
    end
    // split current word into active and non-active part
    else if (LyricsEffect = lfxSlide) then
    begin
      // enable clipping and set clip equation coefficients to zeros
      glEnable(GL_CLIP_PLANE0);
      FillChar(ClipPlaneEq[0], SizeOf(ClipPlaneEq), 0);

      glPushMatrix;
      glTranslatef(LyricX + CurWord.X, LyricY, 0);

      // clip non-active right part of the current word
      ClipPlaneEq[0] := -1;
      ClipPlaneEq[3] := CurWord.Width * Progress;
      glClipPlane(GL_CLIP_PLANE0, @ClipPlaneEq);
      // and draw active left part
      glColor4f(LineColor_act.r, LineColor_act.g, LineColor_act.b, Alpha);
      DrawLyricsWords(Line, 0, 0, Line.CurWord, Line.CurWord);

      // clip active left part of the current word
      ClipPlaneEq[0] := -ClipPlaneEq[0];
      ClipPlaneEq[3] := -ClipPlaneEq[3];
      glClipPlane(GL_CLIP_PLANE0, @ClipPlaneEq);
      // and draw non-active right part
      glColor4f(LineColor_en.r, LineColor_en.g, LineColor_en.b, Alpha);
      DrawLyricsWords(Line, 0, 0, Line.CurWord, Line.CurWord);

      glPopMatrix;

      glDisable(GL_CLIP_PLANE0);
    end;

    // draw the ball onto the current word
    if (LyricsEffect = lfxBall) then
    begin
      DrawBall(LyricX + CurWord.X + CurWord.Width * Progress,
               LyricY - 15 - 15*sin(Progress * Pi), Alpha);
    end;
  end
  else
  begin
    // this section is called if the whole line can be drawn at once and no
    // word is highlighted.

    // enable the upper, disable the lower line
    if (Line = UpperLine) then
      glColorRGB(LineColor_en)
    else
      glColorRGB(LineColor_dis);

    DrawLyricsWords(Line, LyricX, LyricY, 0, High(Line.Words));
  end;
end;

{**
 * @returns a reference to the upper line
 *}
function TLyricEngine.GetUpperLine(): TLyricLine;
begin
  Result := UpperLine;
end;

{**
 * @returns a reference to the lower line
 *}
function TLyricEngine.GetLowerLine(): TLyricLine;
begin
  Result := LowerLine;
end;

{**
 * @returns the index of the upper line
 *}
function TLyricEngine.GetUpperLineIndex(): integer;
const
  QUEUE_SIZE = 3;
begin
  // no line in queue
  if (LineCounter <= 0) then
    Result := -1
  // no line has been removed from queue yet
  else if (LineCounter <= QUEUE_SIZE) then
    Result := 0
  // lines have been removed from queue already
  else
    Result := LineCounter - QUEUE_SIZE;
end;

end.

