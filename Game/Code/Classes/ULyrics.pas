unit ULyrics;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses OpenGL12,
     UTexture,
     UThemes,
     UMusic;

type
  TLyricWord = record
    X:          Real;     // left corner
    Width:      Real;     // width
    TexPos:     Real;     // left corner in texture (0 to 1)
    TexWidth:   Real;     // width (0 to 1)
    Start:      Cardinal; // start of the word in quarters (beats)
    Length:     Cardinal; // length of the word in quarters
    Text:       String;   // text
    Freestyle:  Boolean;  // freestyle?
  end;
  ALyricWord = array of TLyricWord;

  PLyricLine = ^TLyricLine;
  TLyricLine = record
    Text:       String;       // text
    Tex:        glUInt;       // texture of the text
    Width:      Real;         // width
    Size:       Byte;         // fontsize
    Words:      ALyricWord;   // words in this line 
	CurWord:    Integer;      // current active word idx (only valid if line is active)
    Start:      Cardinal;     // start of this line in quarters
    Length:     Cardinal;     // length in quarters
    Freestyle:  Boolean;      // complete line is freestyle?
    Players:    Byte;         // players that should sing that line (bitset, Player1: 1, Player2: 2, Player3: 4)
    Done:       Boolean;      // is sentence already sung?
  end;

  TLyricEngine = class
    private
      EoLastSentence: Real;          // end of the previous sentence (in beats)
      LastDrawBeat: Real;
      UpperLine:      TLyricLine;    // first line displayed (top)
      LowerLine:      TLyricLine;    // second lind displayed (bottom)
      QueueLine:      TLyricLine;    // third line (queue and will be displayed when next line is finished)
      PUpperLine, PLowerLine, PQueueLine: PLyricLine;

      IndicatorTex:   TTexture;       // texture for lyric indikator
      BallTex:        TTexture;       // texture of the ball for the lyric effekt
      
      inQueue:        Boolean;		  // is line in queue
      LCounter:       Word;           // line counter
      
      // duet mode - textures for player icons
      PlayerIconTex:  array[0..5] of  // player idx
                      array [0..1] of // enabled disabled
                      TTexture;


      //Some helper Procedures for Lyric Drawing
      procedure DrawLyrics (Beat: Real);
      procedure DrawLyricsLine(const X, W, Y: Real; Size: Byte; const Line: PLyricLine; Beat: Real);
      procedure DrawPlayerIcon(const Player: Byte; const Enabled: Boolean; const X, Y, Size, Alpha: Real);
      procedure DrawBall(const XBall, YBall:Real);
      
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
      
      
      HoverEffekt:    Byte;       //effekt of hovering active words: 
                                  // 1 - simple (only current word is highlighted)
                                  // 2 - zoom (current word is highlighted and zoomed bigger)
                                  // 3 - slide (highlight the line up to the current position - like a progress bar)
                                  // 4 - ball (current word is highlighted and ball is jumpung over that word)
      
      { // currently not used
       FadeInEffekt:   Byte;       //Effekt for Line Fading in: 0: No Effekt; 1: Fade Effekt; 2: Move Upwards from Bottom to Pos
       FadeOutEffekt:  Byte;       //Effekt for Line Fading out: 0: No Effekt; 1: Fade Effekt; 2: Move Upwards
      }

      UseLinearFilter:Boolean;    //Should Linear Tex Filter be used

      // song specific settings
      BPM:            Real;
      Resolution:     Integer;


      // properties to easily read options of this class
      property LineinQueue: Boolean read inQueue;    // line in queue?
      property LineCounter: Word    read LCounter;   // lines that were progressed so far (after last clear)
      
      Procedure   AddLine(Line: PLine);              // adds a line to the queue, if there is space
      Procedure   Draw (Beat: Real);                 // draw the current (active at beat) lyrics
      
      Procedure   Clear (const cBPM: Real = 0;       // clears all cached song specific information
                         const cResolution: Integer = 0);
      
      Constructor Create; overload; 
      Constructor Create(ULX,ULY,ULW,ULS,LLX,LLY,LLW,LLS:Real); overload;
      Procedure   LoadTextures;
      Destructor  Free;
  end;

implementation

uses SysUtils,
     USkins,
     TextGL,
     UGraphic,
     UDisplay,
     dialogs,
     math;

//-----------
//Helper procs to use TRGB in Opengl ...maybe this should be somewhere else
//-----------
procedure glColorRGB(Color: TRGB);  overload;
begin
  glColor3f(Color.R, Color.G, Color.B);
end;

procedure glColorRGB(Color: TRGBA); overload;
begin
  glColor4f(Color.R, Color.G, Color.B, Color.A);
end;



//---------------
// Create - Constructor, just get Memory
//---------------
Constructor TLyricEngine.Create;
begin
  BPM := 0;
  Resolution := 0;
  LCounter := 0;
  inQueue := False;

  UpperLine.Done := True;
  LowerLine.Done := True;
  QueueLine.Done := True;
  PUpperline:=@UpperLine;
  PLowerLine:=@LowerLine;
  PQueueLine:=@QueueLine;

  UseLinearFilter := True;
  {$IFDEF DARWIN}
    // eddie: Getting range check error with NAN on OS X:
    LastDrawBeat:=0;
  {$ELSE}
    LastDrawBeat:=NAN;
  {$ENDIF}
end;

Constructor TLyricEngine.Create(ULX,ULY,ULW,ULS,LLX,LLY,LLW,LLS:Real);
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
// Free - Frees Memory
//---------------
Destructor  TLyricEngine.Free;
begin

end;

//---------------
// Clear - Clears all cached Song specific Information
//---------------
Procedure   TLyricEngine.Clear (const cBPM: Real; const cResolution: Integer);
begin
  BPM := cBPM;
  Resolution := cResolution;
  LCounter := 0;
  inQueue := False;

  UpperLine.Done := True;
  LowerLine.Done := True;
  QueueLine.Done := True;

  PUpperline:=@UpperLine;
  PLowerLine:=@LowerLine;
  PQueueLine:=@QueueLine;
  
  {$IFDEF DARWIN}
    // eddie: Getting range check error with NAN on OS X:
    LastDrawBeat:=0;
  {$ELSE}
    LastDrawBeat:=NAN;
  {$ENDIF}
end;


//---------------
// LoadTextures - Load Player Textures and Create Lyric Textures
//---------------
Procedure   TLyricEngine.LoadTextures;
var
  I: Integer;
  
  function CreateLineTex: glUint;
  var
    PTexData: Pointer;
  begin
    // get memory
    GetMem(pTexData, 1024*64*4); 

    // generate and bind Texture
    glGenTextures(1, @Result);
    glBindTexture(GL_TEXTURE_2D, Result);

    // get texture memeory
    glTexImage2D(GL_TEXTURE_2D, 0, 4, 1024, 64, 0, GL_RGBA, GL_UNSIGNED_BYTE, pTexData);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    if UseLinearFilter then
    begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    end;

    // free unused memory
    FreeMem(pTexData);
  end;
begin
  
  // lyric indikator (bar that indicates when the line start)
  IndicatorTex := Texture.LoadTexture(pchar(Skin.GetTextureFileName('LyricHelpBar')), 'BMP', 'Transparent', $FF00FF);

  // ball for current word hover in ball effekt
  BallTex := Texture.LoadTexture(pchar(Skin.GetTextureFileName('Ball')), 'PNG', 'Transparent', 0);

  // duet mode: load player icon
  For I := 0 to 5 do
  begin
    PlayerIconTex[I][0] := Texture.LoadTexture(pchar(Skin.GetTextureFileName('LyricIcon_P' + InttoStr(I+1))),   'PNG', 'Transparent', 0);
    PlayerIconTex[I][1] := Texture.LoadTexture(pchar(Skin.GetTextureFileName('LyricIconD_P' + InttoStr(I+1))),   'PNG', 'Transparent', 0);
  end;
  
  // create line textures
  UpperLine.Tex := CreateLineTex;
  LowerLine.Tex := CreateLineTex;
  QueueLine.Tex := CreateLineTex;
end;


//---------------
// AddLine - Adds LyricLine to queue
//---------------
Procedure   TLyricEngine.AddLine(Line: PLine);
var
  LyricLine: PLyricLine;
  countNotes: Cardinal;
  Viewport: Array[0..3] of Integer;
  
  PosX: Real;
  I:  Integer;
begin
  // only add lines, if there is space
  If not LineinQueue then
  begin
    // set pointer to line to write
    
    If (LineCounter = 0) then
      LyricLine := PUpperLine 
    else if (LineCounter = 1) then
      LyricLine := PLowerLine
    else
    begin  
      LyricLine := PQueueLine;
      
      //now there is a queued line
      inQueue   := True;       
    end;
  end
  else
  begin // rotate lines (round-robin-like)
    LyricLine  := PUpperLine;
    PUpperLine := PLowerLine;
    PLowerLine := PQueueLine;
    PQueueLine := LyricLine;
  end;

  // sentence has notes?
  If (Length(Line.Note) > 0) then
  begin
    // copy values from SongLine to LyricLine
    CountNotes          := High(Line.Note);
    LyricLine.Start     := Line.Note[0].Start;
    LyricLine.Length    := Line.Note[CountNotes].Start + Line.Note[CountNotes].Lenght - LyricLine.Start;
    LyricLine.CurWord   := -1;
    
    // default values - set later
    LyricLine.Freestyle := True;
    LyricLine.Text      := '';
    
    // duet mode: players of that line
    LyricLine.Players   := 127;

    //copy words
    SetLength(LyricLine.Words, CountNotes + 1);
    For I := 0 to CountNotes do
    begin
      LyricLine.Words[I].Start     := Line.Note[I].Start;
      LyricLine.Words[I].Length    := Line.Note[I].Lenght;
      LyricLine.Words[I].Text      := Line.Note[I].Text;
      LyricLine.Words[I].Freestyle := Line.Note[I].FreeStyle;
      
      LyricLine.Freestyle          := LyricLine.Freestyle AND Line.Note[I].FreeStyle;
      LyricLine.Text               := LyricLine.Text + LyricLine.Words[I].Text
    end;

    // set font params
    SetFontStyle(FontStyle);
    SetFontPos(0, 0);
    LyricLine.Size := UpperLineSize;
    SetFontSize(LyricLine.Size);
    SetFontItalic(False);
    glColor4f(1, 1, 1, 1);

    // change fontsize to fit the screen
    LyricLine.Width := glTextWidth(PChar(LyricLine.Text));
    while (LyricLine.Width > UpperLineW) do
    begin
      Dec(LyricLine.Size);

      if (LyricLine.Size <=1) then
        Break;

      SetFontSize(LyricLine.Size);
      LyricLine.Width := glTextWidth(PChar(LyricLine.Text));
    end;
    
    // set word positions and line size
    PosX := 0;
    for I := 0 to High(LyricLine.Words) do
    begin
      LyricLine.Words[I].X        := PosX;
      LyricLine.Words[I].Width    := glTextWidth(PChar(LyricLine.Words[I].Text));
      LyricLine.Words[I].TexPos   := (PosX+1) / 1024;
      LyricLine.Words[I].TexWidth := (LyricLine.Words[I].Width-1) / 1024;
      
      PosX := PosX + LyricLine.Words[I].Width;
    end;

    // create LyricTexture - prepare OpenGL
    glGetIntegerv(GL_VIEWPORT, @ViewPort);
    glClearColor(0.0,0.0,0.0,0.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    glViewPort(0,0,800,600);

    //Draw Lyrics
    SetFontPos(0, 0);
    glPrint(PChar(LyricLine.Text));
    
    Display.ScreenShot;
    
    //Copy to Texture
    glEnable(GL_ALPHA);
    glBindTexture(GL_TEXTURE_2D, LyricLine.Tex);
    glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 0, 600-64, 1024, 64, 0);
    glDisable(GL_ALPHA);
    
    //Clear Buffer
    glClearColor(0,0,0,0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    glViewPort(ViewPort[0], ViewPort[1], ViewPort[2], ViewPort[3]);

  end;

  //Increase the Counter
  Inc(LCounter);
end;


//---------------
// Draw - Procedure Draws Lyrics; Beat is curent Beat in Quarters
//        Draw just manage the Lyrics, drawing is done by a call of DrawLyrics
//---------------
Procedure TLyricEngine.Draw (Beat: Real);
begin
  DrawLyrics(Beat);
  LastDrawBeat := Beat;
end;

//---------------
// DrawLyrics(private) - Helper for Draw; main Drawing procedure
//---------------
procedure TLyricEngine.DrawLyrics (Beat: Real);
begin
  DrawLyricsLine(UpperLineX, UpperLineW, UpperlineY, 15, PUpperline, Beat);
  DrawLyricsLine(LowerLineX, LowerLineW, LowerlineY, 15, PLowerline, Beat);
end;

//---------------
// DrawPlayerIcon(private) - Helper for Draw; Draws a Playericon
//---------------
procedure TLyricEngine.DrawPlayerIcon(const Player: Byte; const Enabled: Boolean; const X, Y, Size, Alpha: Real);
var IEnabled: Byte;
begin
  Case Enabled of
    True: 	IEnabled := 0;
    False:  IEnabled := 1;
  end;

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindTexture(GL_TEXTURE_2D, PlayerIconTex[Player][IEnabled].TexNum);

  glColor4f(1,1,1,Alpha);
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
procedure TLyricEngine.DrawBall(const XBall, YBall:Real);
begin
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindTexture(GL_TEXTURE_2D, BallTex.TexNum);
      
  glColor3f(1,1,1);
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
procedure TLyricEngine.DrawLyricsLine(const X, W, Y: Real; Size: Byte; const Line: PLyricLine; Beat: Real);
var
  CurWordStartTx, CurWordEndTx: Real; 	// texture-coordinates of start and end of current word
  CurWordStart, CurWordEnd: Real; 		// screen coordinates of current word and the rest of the sentence
  Progress: Real;						// progress of singing the current word
  LyricX: Real; 						// left
  LyricX2: Real;						// right
  LyricY: Real;							// top
  LyricsHeight: Real;					// height the lyrics are displayed
  
  {// duet mode
  IconSize: Real;						// size of player icons
  IconAlpha: Real;						// alpha level of player icons
  }
begin
  // this is actually a bit more than the real font size
  // it helps adjusting the "zoom-center"
  LyricsHeight:=30 * (Line^.Size/10)+16;
  
  {
  // duet mode
  IconSize := (2 * Size);
  IconAlpha := Frac(Beat/(Resolution*4));
  
  DrawPlayerIcon (0, True, X, Y + (42 - IconSize) / 2 , IconSize, IconAlpha);
  DrawPlayerIcon (1, True, X + IconSize + 1,  Y + (42 - IconSize) / 2, IconSize, IconAlpha);
  DrawPlayerIcon (2, True, X + (IconSize + 1)*2, Y + (42 - IconSize) / 2, IconSize, IconAlpha);
  }
  
  LyricX := X+W/2;
  LyricX2 := LyricX + Line^.Width/2;
  LyricX := LyricX - Line^.Width/2;
  
  // maybe center smaller lines
  LyricY := Y;
  //LyricY := Y + ((Size / Line.Size - 1) * LyricsHeight) / 2; 
    
  // word in the sentence is active?
  if (Line^.Start < Beat) then
  begin
    // if this line just got active, then CurWord is still -1
    // this means, we should try to make the first word active
    // then we check if the current active word is still meant to be active
    // if not, we proceed to the next word
    if Line^.CurWord = -1 then
      Line^.CurWord:=0;
    
    if not ((Beat < (Line^.Words[Line^.CurWord].Start+Line^.Words[Line^.CurWord].Length))) then
    Line^.CurWord:=Line^.CurWord+1;
    
    // last word of this line finished, but this line did not hide
    if (Line^.CurWord > High(Line^.Words)) then
    begin
      with Line^.Words[High(Line^.Words)] do
      begin
        CurWordStartTx := TexPos + TexWidth;
        CurWordEndTx := CurWordStartTx;
        CurWordStart := X + Width;
        CurWordEnd := CurWordStart;
      end;
    end
    else
    begin
      with Line^.Words[Line^.CurWord] do
      begin
        Progress:=(Beat-Start)/Length;
        if Progress >= 1 then
          Progress := 1;
      
        if Progress <= 0 then
          Progress := 0;
      
        CurWordStartTx:=TexPos;
        CurWordEndTx:=TexPos+TexWidth;
        CurWordStart:=X;
        CurWordEnd:=X+Width;
      
        // Slide Effect
        // simply paint the active texture to the current position 
        if HoverEffekt = 3 then
        begin
          CurWordStartTx := CurWordStartTx + TexWidth * progress;
          CurWordEndTx := CurWordStartTx;
          CurWordStart := CurWordStart + Width * progress;
          CurWordEnd := CurWordStart;
        end;
      end;
    end;
    
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, Line^.Tex);

    // draw sentence up to current word
    if HoverEffekt = 4 then
      // ball lyric effect - only highlight current word and not that ones before in this line
      glColorRGB(LineColor_en)
    else
      glColorRGB(LineColor_act);
    
    glBegin(GL_QUADS);
      glTexCoord2f(0, 1); glVertex2f(LyricX, LyricY);
      glTexCoord2f(0, 1-LyricsHeight/64); glVertex2f(LyricX, LyricY + LyricsHeight);
      glTexCoord2f(CurWordStartTx, 1-LyricsHeight/64); glVertex2f(LyricX+CurWordStart, LyricY + LyricsHeight);
      glTexCoord2f(CurWordStartTx, 1); glVertex2f(LyricX+CurWordStart, LyricY);
    glEnd;

    // draw active word:
    // type 1: simple lyric effect
    // type 4: ball lyric effect
    // only change the color of the current word
    if (HoverEffekt = 1) or (HoverEffekt = 4) then
    begin
      { // maybe fade in?
      glColor4f(LineColor_en.r,LineColor_en.g,LineColor_en.b,1-progress);
      glBegin(GL_QUADS);
        glTexCoord2f(CurWordStartTx, 1); glVertex2f(LyricX+CurWordStart, Y);
        glTexCoord2f(CurWordStartTx, 0); glVertex2f(LyricX+CurWordStart, Y + 64);
        glTexCoord2f(CurWordEndTx, 0); glVertex2f(LyricX+CurWordEnd, Y + 64);
        glTexCoord2f(CurWordEndTx, 1); glVertex2f(LyricX+CurWordEnd, Y);
      glEnd;
      }
      
      glColor3f(LineColor_act.r,LineColor_act.g,LineColor_act.b);
      glBegin(GL_QUADS);
        glTexCoord2f(CurWordStartTx, 1); glVertex2f(LyricX+CurWordStart, LyricY);
        glTexCoord2f(CurWordStartTx, 0); glVertex2f(LyricX+CurWordStart, LyricY + 64);
        glTexCoord2f(CurWordEndTx, 0); glVertex2f(LyricX+CurWordEnd, LyricY + 64);
        glTexCoord2f(CurWordEndTx, 1); glVertex2f(LyricX+CurWordEnd, LyricY);
      glEnd;
    end
      
    // draw active word:
    // type 2: zoom lyric effect
    // change color and zoom current word
    else if HoverEffekt = 2 then
    begin
      glPushMatrix;
      glTranslatef(LyricX+CurWordStart+(CurWordEnd-CurWordStart)/2,LyricY+LyricsHeight/2,0);
      glScalef(1.0+(1-progress)/2,1.0+(1-progress)/2,1.0);
      glColor4f(LineColor_en.r,LineColor_en.g,LineColor_en.b,1-progress);
      glBegin(GL_QUADS);
        glTexCoord2f(CurWordStartTx+0.0001, 1); glVertex2f(-(CurWordEnd-CurWordStart)/2, -LyricsHeight/2);
        glTexCoord2f(CurWordStartTx+0.0001, 1-LyricsHeight/64); glVertex2f(-(CurWordEnd-CurWordStart)/2,  + LyricsHeight/2);
        glTexCoord2f(CurWordEndTx-0.0001, 1-LyricsHeight/64); glVertex2f((CurWordEnd-CurWordStart)/2,  + LyricsHeight/2);
        glTexCoord2f(CurWordEndTx-0.0001, 1); glVertex2f((CurWordEnd-CurWordStart)/2, -LyricsHeight/2);
      glEnd;
      glColor4f(LineColor_act.r,LineColor_act.g,LineColor_act.b,1);
      glBegin(GL_QUADS);
        glTexCoord2f(CurWordStartTx+0.0001, 1); glVertex2f(-(CurWordEnd-CurWordStart)/2, -LyricsHeight/2);
        glTexCoord2f(CurWordStartTx+0.0001, 1-LyricsHeight/64); glVertex2f(-(CurWordEnd-CurWordStart)/2,  + LyricsHeight/2);
        glTexCoord2f(CurWordEndTx-0.0001, 1-LyricsHeight/64); glVertex2f((CurWordEnd-CurWordStart)/2,  + LyricsHeight/2);
        glTexCoord2f(CurWordEndTx-0.0001, 1); glVertex2f((CurWordEnd-CurWordStart)/2, -LyricsHeight/2);
      glEnd;
      glPopMatrix;
    end;

    // draw rest of sentence
    glColorRGB(LineColor_en);
    glBegin(GL_QUADS);
      glTexCoord2f(CurWordEndTx, 1); glVertex2f(LyricX+CurWordEnd, LyricY);
      glTexCoord2f(CurWordEndTx, 1-LyricsHeight/64); glVertex2f(LyricX+CurWordEnd, LyricY + LyricsHeight);
      glTexCoord2f(Line^.Width/1024, 1-LyricsHeight/64); glVertex2f(LyricX2, LyricY + LyricsHeight);
      glTexCoord2f(Line^.Width/1024, 1); glVertex2f(LyricX2, LyricY);
    glEnd;


    glDisable(GL_TEXTURE_2D);
    glDisable(GL_BLEND);
    
    if HoverEffekt = 4 then
      DrawBall(LyricX + CurWordStart + (CurWordEnd - CurWordStart) * progress, LyricY - 15 - 15*sin(progress * pi));
  end
  else
  begin
    // draw complete inactive sentence if line hasn't started but is already shown
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glBindTexture(GL_TEXTURE_2D, Line^.Tex);

    glColorRGB(LineColor_dis);
    glBegin(GL_QUADS);
      glTexCoord2f(0, 1); glVertex2f(LyricX, LyricY);
      glTexCoord2f(0, 1-LyricsHeight/64); glVertex2f(LyricX, LyricY + LyricsHeight);
      glTexCoord2f(Line^.Width/1024, 1-LyricsHeight/64); glVertex2f(LyricX2, LyricY + LyricsHeight);
      glTexCoord2f(Line^.Width/1024, 1); glVertex2f(LyricX2, LyricY);
    glEnd;

    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
  end;
end;


end.

