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
  TLyricWord = record
    X:          Real;     // left corner
    Width:      Real;     // width
    Start:      Cardinal; // start of the word in quarters (beats)
    Length:     Cardinal; // length of the word in quarters
    Text:       String;   // text
    Freestyle:  Boolean;  // is freestyle?
  end;
  ALyricWord = array of TLyricWord;

  PLyricLine = ^TLyricLine;
  TLyricLine = record
    Text:           String;       // text
    Tex:            glUInt;       // texture of the text
    Width:          Real;         // width
    Size:           Byte;         // fontsize
    Words:          ALyricWord;   // words in this line 
	CurWord:        Integer;      // current active word idx (only valid if line is active)
    Start:          Cardinal;     // start of this line in quarters
    Length:         Cardinal;     // length in quarters
    HasFreestyle:   Boolean;      // one or more word are freestyle?
    CountFreestyle: Integer;      // how often there is a change from freestyle to non freestyle in this line
    Players:        Byte;         // players that should sing that line (bitset, Player1: 1, Player2: 2, Player3: 4)
    Done:           Boolean;      // is sentence already sung?
    LastLine:		Boolean;      // is this the last line ob the song?
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
      BallTex:        TTexture;       // texture of the ball for the lyric effect
      
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
      procedure DrawBall(const XBall, YBall, Alpha:Real);
      
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
      Destructor  Destroy; override;
  end;

implementation

uses SysUtils,
     USkins,
     TextGL,
     UGraphic,
     UDisplay,
     math,
     UIni;

//-----------
//Helper procs to use TRGB in Opengl ...maybe this should be somewhere else
//-----------
procedure glColorRGB(Color: TRGB);  overload;
begin
  glColor3f(Color.R, Color.G, Color.B);
end;

procedure glColorRGB(Color: TRGB; Alpha: Real);  overload;
begin
  glColor4f(Color.R, Color.G, Color.B, Alpha);
end;

procedure glColorRGB(Color: TRGBA); overload;
begin
  glColor4f(Color.R, Color.G, Color.B, Color.A);
end;

procedure glColorRGB(Color: TRGBA; Alpha: Real); overload;
begin
  glColor4f(Color.R, Color.G, Color.B, Min(Color.A, Alpha));
end;



//---------------
// Create - Constructor, just get Memory
//---------------
Constructor TLyricEngine.Create;
begin
  inherited;

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
  LastDrawBeat:=0;
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
// Destroy - Frees Memory
//---------------
Destructor  TLyricEngine.Destroy;
begin
  inherited;
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
  
  LastDrawBeat:=0;
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
    try
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

    finally  
      // free unused memory
      FreeMem(pTexData);
    end;
  end;
begin
  
  // lyric indicator (bar that indicates when the line start)
  IndicatorTex := Texture.LoadTexture(Skin.GetTextureFileName('LyricHelpBar'), TEXTURE_TYPE_TRANSPARENT, $FF00FF);

  // ball for current word hover in ball effect
  BallTex := Texture.LoadTexture(Skin.GetTextureFileName('Ball'), TEXTURE_TYPE_TRANSPARENT, 0);

  // duet mode: load player icon
  For I := 0 to 5 do
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
//---------------
Procedure   TLyricEngine.AddLine(Line: PLine);
var
  LyricLine: PLyricLine;
  countNotes: Cardinal;
  Viewport: Array[0..3] of Integer;
  
  PosX: Real;
  I:  Integer;
  
  function CalcWidth(LyricLine: PLyricLine): Real;
  begin
    Result := glTextWidth(PChar(LyricLine.Text));
    
    Result := Result + (LyricLine.CountFreestyle * 10);
    
    // if the line ends with a freestyle not, then leave the place to finish to draw the text italic
    if (LyricLine.Words[High(LyricLine.Words)].Freestyle) then
      Result := Result + 12;
  end;
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
  If  Line = nil then
  begin
    // reset all values, if the new line is nil (lines after the last line)
    LyricLine.Start     := 0;
    LyricLine.Length    := 0;
    LyricLine.CurWord   := -1;
    LyricLine.LastLine  := False;
    LyricLine.Width     := 0;
    SetLength(LyricLine.Words, 0);
  end
  else if Length(Line.Note) > 0 then
  begin
    // copy values from SongLine to LyricLine
    CountNotes          := High(Line.Note);
    LyricLine.Start     := Line.Note[0].Start;
    LyricLine.Length    := Line.Note[CountNotes].Start + Line.Note[CountNotes].Length - LyricLine.Start;
    LyricLine.CurWord   := -1;
    LyricLine.LastLine  := Line.LastLine;
    
    // default values - set later
    LyricLine.HasFreestyle := False;
    LyricLine.CountFreestyle := 0;
    LyricLine.Text      := '';
    
    // duet mode: players of that line
    LyricLine.Players   := 127;

    //copy words
    SetLength(LyricLine.Words, CountNotes + 1);
    For I := 0 to CountNotes do
    begin
      LyricLine.Words[I].Start     := Line.Note[I].Start;
      LyricLine.Words[I].Length    := Line.Note[I].Length;
      LyricLine.Words[I].Text      := Line.Note[I].Text;
      LyricLine.Words[I].Freestyle := Line.Note[I].NoteType = ntFreestyle;
      
      LyricLine.HasFreestyle       := LyricLine.HasFreestyle OR LyricLine.Words[I].Freestyle;
      LyricLine.Text               := LyricLine.Text + LyricLine.Words[I].Text;
        
      if (I > 0) AND LyricLine.Words[I-1].Freestyle AND not LyricLine.Words[I].Freestyle then
        Inc(LyricLine.CountFreestyle);
    end;
    
    // set font params
    SetFontStyle(FontStyle);
    SetFontPos(0, 0);
    LyricLine.Size := UpperLineSize;
    SetFontSize(LyricLine.Size);
    SetFontItalic(False);
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
    
    // create LyricTexture - prepare OpenGL
    glGetIntegerv(GL_VIEWPORT, @ViewPort);
    glClearColor(0.0,0.0,0.0,0.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    glViewPort(0,0,800,600);

    // set word positions and line size
    PosX := 0;
    for I := 0 to High(LyricLine.Words) do
    begin
      with LyricLine.Words[I] do
      begin
        SetFontItalic(Freestyle);

        X := PosX;
            
        //Draw Lyrics
        SetFontPos(PosX, 0);
        glPrint(PChar(Text));
      
        Width := glTextWidth(PChar(Text));
        if (I < High(LyricLine.Words)) AND Freestyle AND not LyricLine.Words[I+1].Freestyle then
          Width := Width + 10
        else
          if (I = High(LyricLine.Words)) AND Freestyle then
            Width := Width + 12;
        PosX := PosX + Width;
      end;
    end;
  end
  else
  begin
    // create LyricTexture - prepare OpenGL
    glGetIntegerv(GL_VIEWPORT, @ViewPort);
    glClearColor(0.0,0.0,0.0,0.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    glViewPort(0,0,800,600);
  end;

  //for debugging, is this used anymore?
  //Display.ScreenShot;
  
  //Copy to Texture
  glEnable(GL_ALPHA);
  glBindTexture(GL_TEXTURE_2D, LyricLine.Tex);
  glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 0, 600-64, 1024, 64, 0);
  glDisable(GL_ALPHA);
  
  //Clear Buffer
  glClearColor(0,0,0,0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glViewPort(ViewPort[0], ViewPort[1], ViewPort[2], ViewPort[3]);

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
  
  try
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

  finally
    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
  end;
end;

//---------------
// DrawBall(private) - Helper for Draw; Draws the Ball over the LyricLine if needed
//---------------
procedure TLyricEngine.DrawBall(const XBall, YBall, Alpha:Real);
begin
  try
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glBindTexture(GL_TEXTURE_2D, BallTex.TexNum);
      
    glColor4f(1,1,1, Alpha);
    glBegin(GL_QUADS);
      glTexCoord2f(0, 0); glVertex2f(XBall - 10, YBall);
      glTexCoord2f(0, 1); glVertex2f(XBall - 10, YBall + 20);
      glTexCoord2f(1, 1); glVertex2f(XBall + 10, YBall + 20);
      glTexCoord2f(1, 0); glVertex2f(XBall + 10, YBall);
    glEnd;
    
  finally
    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
  end;
end;

//---------------
// DrawLyricsLine(private) - Helper for Draw; Draws one LyricLine
//---------------
procedure TLyricEngine.DrawLyricsLine(const X, W, Y: Real; Size: Byte; const Line: PLyricLine; Beat: Real);
var
  CurWordStart, CurWordEnd: Real; 		// screen coordinates of current word and the rest of the sentence
  FreestyleDiff: Integer;               // difference between top and bottom coordiantes for freestyle lyrics
  Progress: Real;						// progress of singing the current word
  LyricX: Real; 						// left
  LyricX2: Real;						// right
  LyricY: Real;							// top
  LyricsHeight: Real;					// height the lyrics are displayed
  Alpha: Real;                          // alphalevel to fade out at end
  
  {// duet mode
  IconSize: Real;						// size of player icons
  IconAlpha: Real;						// alpha level of player icons
  }
begin
  // lines with a width lower than 0, have not to be draw
  if Line^.Width <= 0 then
    exit;
  
  // this is actually a bit more than the real font size
  // it helps adjusting the "zoom-center"
  LyricsHeight:=30.5 * (Line^.Size/10);
  
  {
  // duet mode
  IconSize := (2 * Size);
  IconAlpha := Frac(Beat/(Resolution*4));
  
  DrawPlayerIcon (0, True, X, Y + (42 - IconSize) / 2 , IconSize, IconAlpha);
  DrawPlayerIcon (1, True, X + IconSize + 1,  Y + (42 - IconSize) / 2, IconSize, IconAlpha);
  DrawPlayerIcon (2, True, X + (IconSize + 1)*2, Y + (42 - IconSize) / 2, IconSize, IconAlpha);
  }
  
  LyricX := X+W/2 - Line^.Width/2;
  LyricX2 := LyricX + Line^.Width;
   
  // maybe center smaller lines
  //LyricY := Y;
  LyricY := Y + ((Size / Line.Size - 1) * LyricsHeight) / 2; 
  
  Alpha := 1;
  
  // word in the sentence is active?
  if (Line^.Start < Beat) then
  begin
    // if this line just got active, then CurWord is still -1
    // this means, we should try to make the first word active
    // then we check if the current active word is still meant to be active
    // if not, we proceed to the next word
    if Line^.CurWord = -1 then
      Line^.CurWord:=0;
   
    if (Line^.CurWord < High(Line^.Words)) AND (Beat >= (Line^.Words[Line^.CurWord + 1].Start)) then
      Line^.CurWord:=Line^.CurWord+1;
    
    FreestyleDiff := 0;
    
    // last word of this line finished, but this line did not hide
    if (Line^.CurWord > High(Line^.Words)) then
    begin
      CurWordStart := Line^.Words[High(Line^.Words)].X + Line^.Words[High(Line^.Words)].Width;
      CurWordEnd := CurWordStart;
      
      // fade out last line
      if Line^.LastLine then
      begin
        Alpha := 1 - (Beat - (Line^.Words[High(Line^.Words)].Start + Line^.Words[High(Line^.Words)].Length)) / 15;
        if (Alpha < 0) then
          Alpha := 0;
      end;
    end
    else
    begin
      with Line^.Words[Line^.CurWord] do
      begin
        Progress := (Beat - Start) / Length;
        if Progress >= 1 then
          Progress := 1;
      
        if Progress <= 0 then
          Progress := 0;
      
        CurWordStart:=X;
        CurWordEnd:=X+Width;
      
        // Slide Effect
        // simply paint the active texture to the current position 
        if Ini.LyricsEffect = 2 then
        begin
          CurWordStart := CurWordStart + Width * progress;
          CurWordEnd := CurWordStart;
        end;
        
        if (Line^.CurWord < High(Line^.Words)) AND Freestyle AND not Line^.Words[Line^.CurWord + 1].Freestyle then
        begin
          FreestyleDiff := 2;          
        end
        else
          if Freestyle then
          begin
            FreestyleDiff := 12;
            CurWordStart := CurWordStart - 1;
            CurWordEnd := CurWordEnd - 2;
          end;
      end;
    end;
        
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, Line^.Tex);

    // draw sentence up to current word
    if (Ini.LyricsEffect = 3) or (Ini.LyricsEffect = 4) then
      // ball lyric effect - only highlight current word and not that ones before in this line
      glColorRGB(LineColor_en, Alpha)
    else
      glColorRGB(LineColor_act, Alpha);
    
    glBegin(GL_QUADS);
      glTexCoord2f(0, 1); glVertex2f(LyricX, LyricY);
      glTexCoord2f(0, 1-LyricsHeight/64); glVertex2f(LyricX, LyricY + LyricsHeight);
      glTexCoord2f(CurWordStart/1024, 1-LyricsHeight/64); glVertex2f(LyricX+CurWordStart, LyricY + LyricsHeight);
      glTexCoord2f((CurWordStart+FreestyleDiff)/1024, 1); glVertex2f(LyricX+CurWordStart+FreestyleDiff, LyricY);
    glEnd;

    // draw rest of sentence
    glColorRGB(LineColor_en);
    glBegin(GL_QUADS);
      glTexCoord2f((CurWordEnd+FreestyleDiff)/1024, 1); glVertex2f(LyricX+CurWordEnd+FreestyleDiff, LyricY);
      glTexCoord2f(CurWordEnd/1024, 1-LyricsHeight/64); glVertex2f(LyricX+CurWordEnd, LyricY + LyricsHeight);
      glTexCoord2f(Line^.Width/1024, 1-LyricsHeight/64); glVertex2f(LyricX2, LyricY + LyricsHeight);
      glTexCoord2f(Line^.Width/1024, 1); glVertex2f(LyricX2, LyricY);
    glEnd;
    
    // draw active word:
    // type 0: simple lyric effect
    // type 3: ball lyric effect
    // type 4: shift lyric effect
    // only change the color of the current word
    if (Ini.LyricsEffect = 0) or (Ini.LyricsEffect = 3) or (Ini.LyricsEffect = 4) then
    begin
      { // maybe fade in?
      glColor4f(LineColor_en.r,LineColor_en.g,LineColor_en.b,1-progress);
      glBegin(GL_QUADS);
        glTexCoord2f(CurWordStart/1024, 1); glVertex2f(LyricX+CurWordStart, Y);
        glTexCoord2f(CurWordStart/1024, 0); glVertex2f(LyricX+CurWordStart, Y + 64);
        glTexCoord2f(CurWordEnd/1024, 0); glVertex2f(LyricX+CurWordEnd, Y + 64);
        glTexCoord2f(CurWordEnd/1024, 1); glVertex2f(LyricX+CurWordEnd, Y);
      glEnd;
      }
      
      if (Ini.LyricsEffect = 4) then
        LyricY := LyricY - 8 * (1-progress);
      
      glColor3f(LineColor_act.r,LineColor_act.g,LineColor_act.b);
      glBegin(GL_QUADS);
        glTexCoord2f((CurWordStart+FreestyleDiff)/1024, 1); glVertex2f(LyricX+CurWordStart+FreestyleDiff, LyricY);
        glTexCoord2f(CurWordStart/1024, 0); glVertex2f(LyricX+CurWordStart, LyricY + 64);
        glTexCoord2f(CurWordEnd/1024, 0); glVertex2f(LyricX+CurWordEnd, LyricY + 64);
        glTexCoord2f((CurWordEnd+FreestyleDiff)/1024, 1); glVertex2f(LyricX+CurWordEnd+FreestyleDiff, LyricY);
      glEnd;
      
      if (Ini.LyricsEffect = 4) then
        LyricY := LyricY + 8 * (1-progress);
    end
      
    // draw active word:
    // type 1: zoom lyric effect
    // change color and zoom current word
    else if Ini.LyricsEffect = 1 then
    begin
      glPushMatrix;
      glTranslatef(LyricX+CurWordStart+(CurWordEnd-CurWordStart)/2,LyricY+LyricsHeight/2,0);
      glScalef(1.0+(1-progress)/2,1.0+(1-progress)/2,1.0);
      glColor4f(LineColor_en.r,LineColor_en.g,LineColor_en.b,1-progress);
      glBegin(GL_QUADS);
        glTexCoord2f((CurWordStart+FreestyleDiff)/1024, 1); glVertex2f(-(CurWordEnd-CurWordStart)/2+FreestyleDiff, -LyricsHeight/2);
        glTexCoord2f(CurWordStart/1024, 1-LyricsHeight/64); glVertex2f(-(CurWordEnd-CurWordStart)/2, + LyricsHeight/2);
        glTexCoord2f(CurWordEnd/1024, 1-LyricsHeight/64); glVertex2f((CurWordEnd-CurWordStart)/2, + LyricsHeight/2);
        glTexCoord2f((CurWordEnd+FreestyleDiff)/1024, 1); glVertex2f((CurWordEnd-CurWordStart)/2+FreestyleDiff, -LyricsHeight/2);
      glEnd;
      glColor4f(LineColor_act.r,LineColor_act.g,LineColor_act.b,1);
      glBegin(GL_QUADS);
        glTexCoord2f((CurWordStart+FreestyleDiff)/1024, 1); glVertex2f(-(CurWordEnd-CurWordStart)/2+FreestyleDiff, -LyricsHeight/2);
        glTexCoord2f(CurWordStart/1024, 1-LyricsHeight/64); glVertex2f(-(CurWordEnd-CurWordStart)/2,  + LyricsHeight/2);
        glTexCoord2f(CurWordEnd/1024, 1-LyricsHeight/64); glVertex2f((CurWordEnd-CurWordStart)/2,  + LyricsHeight/2);
        glTexCoord2f((CurWordEnd+FreestyleDiff)/1024, 1); glVertex2f((CurWordEnd-CurWordStart)/2+FreestyleDiff, -LyricsHeight/2);
      glEnd;
      glPopMatrix;
    end;

    glDisable(GL_TEXTURE_2D);
    glDisable(GL_BLEND);
    
    if Ini.LyricsEffect = 3 then
      DrawBall(LyricX + CurWordStart + (CurWordEnd - CurWordStart) * progress, LyricY - 15 - 15*sin(progress * pi), Alpha);
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

