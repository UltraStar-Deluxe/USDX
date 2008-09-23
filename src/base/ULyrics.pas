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

  PLyricWord = ^TLyricWord;
  TLyricWord = record
    X:          Real;     // left corner
    Width:      Real;     // width
    Start:      Cardinal; // start of the word in quarters (beats)
    Length:     Cardinal; // length of the word in quarters
    Text:       String;   // text
    Freestyle:  Boolean;  // is freestyle?
  end;
  ALyricWord = array of TLyricWord;

  TLyricLine = class
    public
      Text:           String;       // text
      Tex:            glUInt;       // texture of the text
      Width:          Real;         // width
      Size:           Byte;         // fontsize
      Words:          ALyricWord;   // words in this line
      CurWord:        Integer;      // current active word idx (only valid if line is active)
      Start:          Integer;      // start of this line in quarters (Note: negative start values are possible due to gap)
      StartNote:      Integer;      // start of the first note of this line in quarters
      Length:         Integer;      // length in quarters (from start of first to the end of the last note)
      HasFreestyle:   Boolean;      // one or more word are freestyle?
      CountFreestyle: Integer;      // how often there is a change from freestyle to non freestyle in this line
      Players:        Byte;         // players that should sing that line (bitset, Player1: 1, Player2: 2, Player3: 4)
      LastLine:       Boolean;      // is this the last line of the song?

      constructor Create();
      destructor Destroy(); override;
      procedure Reset();
  end;

  TLyricEngine = class
    private
      LastDrawBeat:   Real;
      UpperLine:      TLyricLine;    // first line displayed (top)
      LowerLine:      TLyricLine;    // second lind displayed (bottom)
      QueueLine:      TLyricLine;    // third line (will be displayed when lower line is finished)

      IndicatorTex:   TTexture;      // texture for lyric indikator
      BallTex:        TTexture;      // texture of the ball for the lyric effect

      QueueFull:      Boolean;       // set to true if the queue is full and a line will be replaced with the next AddLine
      LCounter:       Word;          // line counter

      // duet mode - textures for player icons
      // FIXME: do not use a fixed player count, use MAX_PLAYERS instead
      PlayerIconTex:  array[0..5] of TPlayerIconTex;

      //Some helper Procedures for Lyric Drawing
      procedure DrawLyrics (Beat: Real);
      procedure DrawLyricsLine(X, W, Y: Real; Size: Byte; Line: TLyricLine; Beat: Real);
      procedure DrawPlayerIcon(Player: Byte; Enabled: Boolean; X, Y, Size, Alpha: Real);
      procedure DrawBall(XBall, YBall, Alpha:Real);

    public
      // positions, line specific settings
      UpperLineX:     Real;       //X Start Pos of UpperLine
      UpperLineW:     Real;       //Width of UpperLine with Icon(s) and Text
      UpperLineY:     Real;       //Y Start Pos of UpperLine
      UpperLineSize:  Byte;       //Max Size of Lyrics Text in UpperLine

      LowerLineX:     Real;       //X Start Pos of LowerLine
      LowerLineW:     Real;       //Width of LowerLine with Icon(s) and Text
      LowerLineY:     Real;       //Y Start Pos of LowerLine
      LowerLineSize:  Byte;       //Max Size of Lyrics Text in LowerLine

      // display propertys
      LineColor_en:   TRGBA;      //Color of Words in an Enabled Line
      LineColor_dis:  TRGBA;      //Color of Words in a Disabled Line
      LineColor_act:  TRGBA;      //Color of teh active Word
      FontStyle:      Byte;       //Font for the Lyric Text
      FontReSize:     Boolean;    //ReSize Lyrics if they don't fit Screen
      
      { // currently not used
       FadeInEffect:   Byte;       //Effect for Line Fading in: 0: No Effect; 1: Fade Effect; 2: Move Upwards from Bottom to Pos
       FadeOutEffect:  Byte;       //Effect for Line Fading out: 0: No Effect; 1: Fade Effect; 2: Move Upwards
      }

      UseLinearFilter: Boolean;    //Should Linear Tex Filter be used

      // song specific settings
      BPM:            Real;
      Resolution:     Integer;

      // properties to easily read options of this class
      property IsQueueFull: Boolean read QueueFull;    // line in queue?
      property LineCounter: Word    read LCounter;   // lines that were progressed so far (after last clear)

      procedure AddLine(Line: PLine);              // adds a line to the queue, if there is space
      procedure Draw (Beat: Real);                 // draw the current (active at beat) lyrics

      // clears all cached song specific information
      procedure Clear(cBPM: Real = 0; cResolution: Integer = 0);

      function GetUpperLine(): TLyricLine;
      function GetLowerLine(): TLyricLine;

      function GetUpperLineIndex(): Integer;

      constructor Create; overload;
      constructor Create(ULX,ULY,ULW,ULS,LLX,LLY,LLW,LLS: Real); overload;
      procedure   LoadTextures;
      destructor  Destroy; override;
  end;

implementation

uses SysUtils,
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

  HasFreestyle := False;
  CountFreestyle := 0;
end;


{ TLyricEngine }

//---------------
// Create - Constructor, just get Memory
//---------------
constructor TLyricEngine.Create;
begin
  inherited;

  BPM := 0;
  Resolution := 0;
  LCounter := 0;
  QueueFull := False;

  UpperLine := TLyricLine.Create;
  LowerLine := TLyricLine.Create;
  QueueLine := TLyricLine.Create;

  UseLinearFilter := True;
  LastDrawBeat := 0;
end;

constructor TLyricEngine.Create(ULX,ULY,ULW,ULS,LLX,LLY,LLW,LLS:Real);
begin
  Create;
  
  UpperLineX := ULX;
  UpperLineW := ULW;
  UpperLineY := ULY;
  UpperLineSize := Trunc(ULS);

  LowerLineX := LLX;
  LowerLineW := LLW;
  LowerLineY := LLY;
  LowerLineSize := Trunc(LLS);

  LoadTextures;
end;


//---------------
// Destroy - Frees Memory
//---------------
destructor TLyricEngine.Destroy;
begin
  UpperLine.Free;
  LowerLine.Free;
  QueueLine.Free;
  inherited;
end;

//---------------
// Clear - Clears all cached Song specific Information
//---------------
procedure TLyricEngine.Clear(cBPM: Real; cResolution: Integer);
begin
  BPM := cBPM;
  Resolution := cResolution;
  LCounter := 0;
  QueueFull := False;

  LastDrawBeat:=0;
end;


//---------------
// LoadTextures - Load Player Textures and Create Lyric Textures
//---------------
procedure TLyricEngine.LoadTextures;
var
  I: Integer;

  function CreateLineTex: glUint;
  begin
    // generate and bind Texture
    glGenTextures(1, @Result);
    glBindTexture(GL_TEXTURE_2D, Result);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    if UseLinearFilter then
    begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    end;
  end;

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

  // create line textures
  UpperLine.Tex := CreateLineTex;
  LowerLine.Tex := CreateLineTex;
  QueueLine.Tex := CreateLineTex;
end;


//---------------
// AddLine - Adds LyricLine to queue
// The LyricEngine stores three lines in its queue:
//   UpperLine: the upper line displayed in the lyrics
//   LowerLine: the lower line displayed in the lyrics
//   QueueLine: an offscreen line that precedes LowerLine
// If the queue is full the next call to AddLine will replace UpperLine with
// LowerLine, LowerLine with QueueLine and QueueLine with the Line parameter.
//---------------
procedure TLyricEngine.AddLine(Line: PLine);
var
  LyricLine: TLyricLine;
  PosX: Real;
  I: Integer;
  CurWord: PLyricWord;
  RenderPass: Integer;

  function CalcWidth(LyricLine: TLyricLine): Real;
  begin
    Result := glTextWidth(PChar(LyricLine.Text));

    Result := Result + (LyricLine.CountFreestyle * 10);

    // if the line ends with a freestyle not, then leave the place to finish to draw the text italic
    if (LyricLine.Words[High(LyricLine.Words)].Freestyle) then
      Result := Result + 12;
  end;

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
    LyricLine  := UpperLine;
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

      LyricLine.HasFreestyle       := LyricLine.HasFreestyle or LyricLine.Words[I].Freestyle;
      LyricLine.Text               := LyricLine.Text + LyricLine.Words[I].Text;

      if (I > 0) and
         LyricLine.Words[I-1].Freestyle and
         not LyricLine.Words[I].Freestyle then
      begin
        Inc(LyricLine.CountFreestyle);
      end;
    end;

    // set font params
    SetFontStyle(FontStyle);
    SetFontPos(0, 0);
    LyricLine.Size := UpperLineSize;
    SetFontSize(LyricLine.Size);
    SetFontItalic(False);
    SetFontReflection(False, 0);
    glColor4f(1, 1, 1, 1);

    // change fontsize to fit the screen
    LyricLine.Width := CalcWidth(LyricLine);
    while (LyricLine.Width > UpperLineW) do
    begin
      Dec(LyricLine.Size);

      if (LyricLine.Size <=1) then
        Break;

      SetFontSize(LyricLine.Size);
      LyricLine.Width := CalcWidth(LyricLine);
    end;

    // Offscreen rendering of LyricTexture:
    // First we will create a white transparent background to draw on.
    // If the text was simply drawn to the screen with blending, the translucent
    // parts of the text would be merged with the current color of the background.
    // This would result in a texture that partly contains the screen we are
    // drawing on. This will be visible in the fonts outline when the LyricTexture
    // is drawn to the screen later.
    // So we have to draw the text in TWO passes.
    // At the first pass we copy the characters to the back-buffer in such a way
    // that preceding characters are not repainted by following chars (otherwise
    // some characters would be blended twice in the 2nd pass).
    // To achieve this we disable blending and enable the depth-test. The z-buffer
    // is used as a mask or replacement for the missing stencil-buffer. A z-value
    // of 1 means the pixel was not assigned yet, whereas 0 stands for a pixel
    // that was already drawn on. In addition we set the depth-func in such a way
    // that assigned pixels (0) will not be drawn a second time.
    // At the second pass we draw the text with blending and without depth-test.
    // This will blend overlapping characters but not the background as it was
    // repainted in the first pass.

    glPushAttrib(GL_VIEWPORT_BIT or GL_DEPTH_BUFFER_BIT);
    glViewPort(0, 0, 800, 600);
    glClearColor(0, 0, 0, 0);
    glDepthRange(0, 1);
    glClearDepth(1);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    SetFontZ(0);
    
    // assure blending is off and the correct depth-func is enabled
    glDisable(GL_BLEND);
    glDepthFunc(GL_LESS);

    // we need two passes to draw the font onto the screen.
    for RenderPass := 0 to 1 do
    begin
      if (RenderPass = 0) then
      begin
        // first pass: simply copy each character to the screen without overlapping.
        SetFontBlend(false);
        glEnable(GL_DEPTH_TEST);
      end
      else
      begin
        // second pass: now we will blend overlapping characters.
        SetFontBlend(true);
        glDisable(GL_DEPTH_TEST);
      end;

      PosX := 0;

      // set word positions and line size and draw the line to the back-buffer
      for I := 0 to High(LyricLine.Words) do
      begin
        CurWord := @LyricLine.Words[I];

        SetFontItalic(CurWord.Freestyle);

        CurWord.X := PosX;

        // Draw Lyrics
        SetFontPos(PosX, 0);
        glPrint(PChar(CurWord.Text));

        CurWord.Width := glTextWidth(PChar(CurWord.Text));
        if CurWord.Freestyle then
        begin
          if (I < High(LyricLine.Words)) and not LyricLine.Words[I+1].Freestyle then
            CurWord.Width := CurWord.Width + 10
          else if (I = High(LyricLine.Words)) then
            CurWord.Width := CurWord.Width + 12;
        end;
        PosX := PosX + CurWord.Width;
      end;
    end;
    
    // copy back-buffer to texture
    glBindTexture(GL_TEXTURE_2D, LyricLine.Tex);
    // FIXME: this does not work this way
    glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 0, 600-64, 1024, 64, 0);
    if (glGetError() <> GL_NO_ERROR) then
      Log.LogError('Creation of Lyrics-texture failed', 'TLyricEngine.AddLine');

    // restore OpenGL state
    glPopAttrib();

    // clear buffer (use white to avoid flimmering if no cover/video is available)
    glClearColor(1, 1, 1, 1);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  end; // if (Line <> nil) and (Length(Line.Note) > 0)

  // increase the counter
  Inc(LCounter);
end;


//---------------
// Draw - Procedure Draws Lyrics; Beat is curent Beat in Quarters
//        Draw just manage the Lyrics, drawing is done by a call of DrawLyrics
//---------------
procedure TLyricEngine.Draw(Beat: Real);
begin
  DrawLyrics(Beat);
  LastDrawBeat := Beat;
end;

//---------------
// DrawLyrics(private) - Helper for Draw; main Drawing procedure
//---------------
procedure TLyricEngine.DrawLyrics(Beat: Real);
begin
  DrawLyricsLine(UpperLineX, UpperLineW, UpperlineY, 15, Upperline, Beat);
  DrawLyricsLine(LowerLineX, LowerLineW, LowerlineY, 15, Lowerline, Beat);
end;

//---------------
// DrawPlayerIcon(private) - Helper for Draw; Draws a Playericon
//---------------
procedure TLyricEngine.DrawPlayerIcon(Player: Byte; Enabled: Boolean; X, Y, Size, Alpha: Real);
var
  IEnabled: Byte;
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

//---------------
// DrawBall(private) - Helper for Draw; Draws the Ball over the LyricLine if needed
//---------------
procedure TLyricEngine.DrawBall(XBall, YBall, Alpha: Real);
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

//---------------
// DrawLyricsLine(private) - Helper for Draw; Draws one LyricLine
//---------------
procedure TLyricEngine.DrawLyricsLine(X, W, Y: Real; Size: Byte; Line: TLyricLine; Beat: Real);
var
  CurWordStart, CurWordEnd: Real; // screen coordinates of current word and the rest of the sentence
  FreestyleDiff: Integer;         // difference between top and bottom coordiantes for freestyle lyrics
  Progress: Real;                 // progress of singing the current word
  LyricX: Real;                   // left
  LyricX2: Real;                  // right
  LyricY: Real;                   // top
  LyricsHeight: Real;             // height the lyrics are displayed
  Alpha: Real;                    // alphalevel to fade out at end
  CurWord, LastWord: PLyricWord;  // current word

  {// duet mode
  IconSize: Real;                 // size of player icons
  IconAlpha: Real;                // alpha level of player icons
  }
begin
  // do not draw empty lines
  // Note: lines with no words in it do not have a valid texture
  if (Length(Line.Words) = 0) or
     (Line.Width <= 0) then
  begin
    Exit;
  end;

  // this is actually a bit more than the real font size
  // it helps adjusting the "zoom-center"
  LyricsHeight := 30.5 * (Line.Size/10);

  {
  // duet mode
  IconSize := (2 * Size);
  IconAlpha := Frac(Beat/(Resolution*4));

  DrawPlayerIcon (0, True, X, Y + (42 - IconSize) / 2 , IconSize, IconAlpha);
  DrawPlayerIcon (1, True, X + IconSize + 1,  Y + (42 - IconSize) / 2, IconSize, IconAlpha);
  DrawPlayerIcon (2, True, X + (IconSize + 1)*2, Y + (42 - IconSize) / 2, IconSize, IconAlpha);
  }

  LyricX := X + W/2 - Line.Width/2;
  LyricX2 := LyricX + Line.Width;

  // maybe center smaller lines
  //LyricY := Y;
  LyricY := Y + ((Size / Line.Size - 1) * LyricsHeight) / 2;

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

    FreestyleDiff := 0;

    // determine current and last word in this line.
    // If the end of the line is reached use the last word as current word.
    LastWord := @Line.Words[High(Line.Words)];
    CurWord := @Line.Words[Line.CurWord];

    // calc the progress of the lyrics effect
    Progress := (Beat - CurWord.Start) / CurWord.Length;
    if Progress >= 1 then
      Progress := 1;
    if Progress <= 0 then
      Progress := 0;

    // last word of this line finished, but this line did not hide -> fade out
    if Line.LastLine and
       (Beat > LastWord.Start + LastWord.Length) then
    begin
      Alpha := 1 - (Beat - (LastWord.Start + LastWord.Length)) / 15;
      if (Alpha < 0) then
        Alpha := 0;
    end;

    // determine the start-/end positions of the fragment of the current word
    CurWordStart := CurWord.X;
    CurWordEnd := CurWord.X + CurWord.Width;

    // Slide Effect
    // simply paint the active texture to the current position
    if Ini.LyricsEffect = 2 then
    begin
      CurWordStart := CurWordStart + CurWord.Width * Progress;
      CurWordEnd := CurWordStart;
    end;

    if CurWord.Freestyle then
    begin
      if (Line.CurWord < High(Line.Words)) and
         (not Line.Words[Line.CurWord + 1].Freestyle) then
      begin
        FreestyleDiff := 2;
      end
      else
      begin
        FreestyleDiff := 12;
        CurWordStart := CurWordStart - 1;
        CurWordEnd := CurWordEnd - 2;
      end;
    end;

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, Line.Tex);

    // draw sentence up to current word
    // type 0: simple lyric effect
    // type 3: ball lyric effect
    // type 4: shift lyric effect
    if (Ini.LyricsEffect in [0, 3, 4]) then
      // ball lyric effect - only highlight current word and not that ones before in this line
      glColorRGB(LineColor_en, Alpha)
    else
      glColorRGB(LineColor_act, Alpha);

    glBegin(GL_QUADS);
      glTexCoord2f(0, 1);
      glVertex2f(LyricX, LyricY);

      glTexCoord2f(0, 1-LyricsHeight/64);
      glVertex2f(LyricX, LyricY + LyricsHeight);

      glTexCoord2f(CurWordStart/1024, 1-LyricsHeight/64);
      glVertex2f(LyricX + CurWordStart, LyricY + LyricsHeight);

      glTexCoord2f((CurWordStart + FreestyleDiff)/1024, 1);
      glVertex2f(LyricX + CurWordStart + FreestyleDiff, LyricY);
    glEnd;

    // draw rest of sentence
    glColorRGB(LineColor_en, Alpha);
    glBegin(GL_QUADS);
      glTexCoord2f((CurWordEnd + FreestyleDiff)/1024, 1);
      glVertex2f(LyricX + CurWordEnd + FreestyleDiff, LyricY);

      glTexCoord2f(CurWordEnd/1024, 1-LyricsHeight/64);
      glVertex2f(LyricX + CurWordEnd, LyricY + LyricsHeight);

      glTexCoord2f(Line.Width/1024, 1-LyricsHeight/64);
      glVertex2f(LyricX2, LyricY + LyricsHeight);

      glTexCoord2f(Line.Width/1024, 1);
      glVertex2f(LyricX2, LyricY);
    glEnd;

    // draw active word:
    // type 0: simple lyric effect
    // type 3: ball lyric effect
    // type 4: shift lyric effect
    // only change the color of the current word
    if (Ini.LyricsEffect in [0, 3, 4]) then
    begin
      if (Ini.LyricsEffect = 4) then
        LyricY := LyricY - 8 * (1-Progress);

      glColor4f(LineColor_act.r, LineColor_act.g, LineColor_act.b, Alpha);
      glBegin(GL_QUADS);
        glTexCoord2f((CurWordStart + FreestyleDiff)/1024, 1);
        glVertex2f(LyricX + CurWordStart + FreestyleDiff, LyricY);

        glTexCoord2f(CurWordStart/1024, 0);
        glVertex2f(LyricX + CurWordStart, LyricY + 64);

        glTexCoord2f(CurWordEnd/1024, 0);
        glVertex2f(LyricX + CurWordEnd, LyricY + 64);

        glTexCoord2f((CurWordEnd + FreestyleDiff)/1024, 1);
        glVertex2f(LyricX + CurWordEnd + FreestyleDiff, LyricY);
      glEnd;

      if (Ini.LyricsEffect = 4) then
        LyricY := LyricY + 8 * (1-Progress);
    end

    // draw active word:
    // type 1: zoom lyric effect
    // change color and zoom current word
    else if Ini.LyricsEffect = 1 then
    begin
      glPushMatrix;

      glTranslatef(LyricX + CurWordStart + (CurWordEnd-CurWordStart)/2,
                   LyricY + LyricsHeight/2, 0);

      // set current zoom factor
      glScalef(1.0 + (1-Progress) * 0.5, 1.0 + (1-Progress) * 0.5, 1.0);

      glColor4f(LineColor_act.r, LineColor_act.g, LineColor_act.b, Alpha);
      glBegin(GL_QUADS);
        glTexCoord2f((CurWordStart + FreestyleDiff)/1024, 1);
        glVertex2f(-(CurWordEnd-CurWordStart)/2 + FreestyleDiff, -LyricsHeight/2);

        glTexCoord2f(CurWordStart/1024, 1-LyricsHeight/64);
        glVertex2f(-(CurWordEnd-CurWordStart)/2, LyricsHeight/2);

        glTexCoord2f(CurWordEnd/1024, 1-LyricsHeight/64);
        glVertex2f((CurWordEnd-CurWordStart)/2,  LyricsHeight/2);

        glTexCoord2f((CurWordEnd + FreestyleDiff)/1024, 1);
        glVertex2f((CurWordEnd-CurWordStart)/2 + FreestyleDiff, -LyricsHeight/2);
      glEnd;

      glPopMatrix;
    end;

    glDisable(GL_TEXTURE_2D);
    glDisable(GL_BLEND);

    // type 3: ball lyric effect
    if Ini.LyricsEffect = 3 then
    begin
      DrawBall(LyricX + CurWordStart + (CurWordEnd-CurWordStart) * Progress,
               LyricY - 15 - 15*sin(Progress * Pi), Alpha);
    end;
  end
  else
  begin
    // this section is called if the whole line can be drawn at once and no
    // word has to be emphasized.

    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glBindTexture(GL_TEXTURE_2D, Line.Tex);

    // enable the upper, disable the lower line
    if (Line = UpperLine) then
      glColorRGB(LineColor_en)
    else
      glColorRGB(LineColor_dis);

    glBegin(GL_QUADS);
      glTexCoord2f(0, 1);
      glVertex2f(LyricX, LyricY);

      glTexCoord2f(0, 1-LyricsHeight/64);
      glVertex2f(LyricX, LyricY + LyricsHeight);

      glTexCoord2f(Line.Width/1024, 1-LyricsHeight/64);
      glVertex2f(LyricX2, LyricY + LyricsHeight);

      glTexCoord2f(Line.Width/1024, 1);
      glVertex2f(LyricX2, LyricY);
    glEnd;

    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
  end;
end;

//---------------
// GetUpperLine() - Returns a reference to the upper line
//---------------
function TLyricEngine.GetUpperLine(): TLyricLine;
begin
  Result := UpperLine;
end;

//---------------
// GetLowerLine() - Returns a reference to the lower line
//---------------
function TLyricEngine.GetLowerLine(): TLyricLine;
begin
  Result := LowerLine;
end;

//---------------
// GetUpperLineIndex() - Returns the index of the upper line
//---------------
function TLyricEngine.GetUpperLineIndex(): Integer;
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

