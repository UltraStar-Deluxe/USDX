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
    X:          Real;     //X Pos of the Word
    Width:      Real;     //Width of the Text
    TexPos:     Real;     //Pos of the Word (0 to 1) in the Sentence Texture
    TexWidth:   Real;     //width of the Word in Sentence Texture (0 to 1)
    Start:      Cardinal; //Start of the Words in Quarters (Beats)
    Length:     Cardinal; //Length of the Word in Quarters
    Text:       String;   //Text of this Word
    Freestyle:  Boolean;  //Is this Word Freestyle
  end;
  ALyricWord = array of TLyricWord;

  PLyricLine = ^TLyricLine;
  TLyricLine = record
    Text:       String;       //Text of the Line
    Tex:        glUInt;       //Texture of the Text from this Line
    Width:      Real;         //Width of the Lyricline in Tex
    Size:       Byte;         //Size of the Font in the Texture
    Words:      ALyricWord;   //Words from this Line
	CurWord:    Integer;      //current active word (only valid if line is active)
    Start:      Cardinal;     //Start in Quarters of teh Line
    Length:     Cardinal;     //Length in Quarters (From Start of First Note to the End of Last Note)
    Freestyle:  Boolean;      //Complete Line is Freestyle ?
    Players:    Byte;         //Which Players Sing this Line (1: Player1; 2: Player2; 4: Player3; [..])
    Done:       Boolean;      //Is Sentence Sung
  end;

  TLyricEngine = class
    private
      EoLastSentence: Real;       //When did the Last Sentence End (in Beats)
      LastDrawBeat: Real;
      UpperLine:      TLyricLine; //Line in the Upper Part of the Lyric Display
      LowerLine:      TLyricLine; //Line in the Lower Part of teh Lyric Display
      QueueLine:      TLyricLine; //Line that is in Queue and will be added when next Line is Finished
      PUpperLine, PLowerLine, PQueueLine: PLyricLine;

      IndicatorTex:   TTexture;       //Texture for Lyric Indikator(Bar that indicates when the Line start)
      BallTex:        TTexture;       //Texture of the Ball for cur. Word hover in Ballmode
      PlayerIconTex:  array[0..5] of  //Textures for PlayerIcon Index: Playernum; Index2: Enabled/Disabled
                      array [0..1] of
                      TTexture;

      inQueue:        Boolean;
      LCounter:       Word;

      //Some helper Procedures for Lyric Drawing
      procedure DrawLyrics (Beat: Real);
      procedure DrawLyricsLine(const X, W, Y: Real; Size: Byte; const Line: PLyricLine; Beat: Real);
      procedure DrawPlayerIcon(const Player: Byte; const Enabled: Boolean; const X, Y, Size, Alpha: Real);
    public
      //Positions, Line specific Settings
      UpperLineX:     Real;       //X Start Pos of UpperLine
      UpperLineW:     Real;       //Width of UpperLine with Icon(s) and Text
      UpperLineY:     Real;       //Y Start Pos of UpperLine
      UpperLineSize:  Byte;       //Max Size of Lyrics Text in UpperLine

      LowerLineX:     Real;       //X Start Pos of LowerLine
      LowerLineW:     Real;       //Width of LowerLine with Icon(s) and Text
      LowerLineY:     Real;       //Y Start Pos of LowerLine
      LowerLineSize:  Byte;       //Max Size of Lyrics Text in LowerLine

      //Display Propertys
      LineColor_en:   TRGBA;      //Color of Words in an Enabled Line
      LineColor_dis:  TRGBA;      //Color of Words in a Disabled Line
      LineColor_act:  TRGBA;      //Color of teh active Word
      FontStyle:      Byte;       //Font for the Lyric Text
      FontReSize:     Boolean;    //ReSize Lyrics if they don't fit Screen

      HoverEffekt:    Byte;       //Effekt of Hovering active Word: 0 - one selection, 1 - long selection, 2 - one selection with fade to normal text, 3 - long selection with fade with color from left
      FadeInEffekt:   Byte;       //Effekt for Line Fading in: 0: No Effekt; 1: Fade Effekt; 2: Move Upwards from Bottom to Pos
      FadeOutEffekt:  Byte;       //Effekt for Line Fading out: 0: No Effekt; 1: Fade Effekt; 2: Move Upwards

      UseLinearFilter:Boolean;    //Should Linear Tex Filter be used

      //Song specific Settings
      BPM:            Real;
      Resolution:     Integer;


      //properties to easily update this Class within other Parts of Code
      property LineinQueue: Boolean read inQueue;    //True when there is a Line in Queue
      property LineCounter: Word    read LCounter;   //Lines that was Progressed so far (after last Clear)

      Constructor Create; overload;     //Constructor, just get Memory
      Constructor Create(ULX,ULY,ULW,ULS,LLX,LLY,LLW,LLS:Real); overload;
      Procedure   LoadTextures;         //Load Player Textures and Create

      Procedure   AddLine(Line: PLine); //Adds a Line to the Queue if there is Space
      Procedure   Draw (Beat: Real);    //Procedure Draws Lyrics; Beat is curent Beat in Quarters
      Procedure   Clear (const cBPM: Real = 0; const cResolution: Integer = 0); //Clears all cached Song specific Information

      Destructor  Free;                 //Frees Memory
  end;

const LyricTexStart = 2/512;

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
  PTexData: Pointer;

  function CreateLineTex: glUint;
  begin
    GetMem(pTexData, 1024*64*4); //get Memory to save Tex in

    //generate and bind Texture
    glGenTextures(1, @Result);
    glBindTexture(GL_TEXTURE_2D, Result);

    //Get Memory
    glTexImage2D(GL_TEXTURE_2D, 0, 4, 1024, 64, 0, GL_RGBA, GL_UNSIGNED_BYTE, pTexData);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    if UseLinearFilter then
    begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    end;

    //Free now unused Memory
    FreeMem(pTexData);
  end;
begin
  //Load Texture for Lyric Indikator(Bar that indicates when the Line start)
  IndicatorTex := Texture.LoadTexture(pchar(Skin.GetTextureFileName('LyricHelpBar')), 'BMP', 'Transparent', $FF00FF);

  //Load Texture of the Ball for cur. Word hover in Ballmode
  BallTex := Texture.LoadTexture(pchar(Skin.GetTextureFileName('Ball')), 'BMP', 'Transparent', $FF00FF);

  //Load PlayerTexs
  For I := 0 to 1 do
  begin
    PlayerIconTex[I][0] := Texture.LoadTexture(pchar(Skin.GetTextureFileName('LyricIcon_P' + InttoStr(I+1))),   'PNG', 'Transparent', 0);
    PlayerIconTex[I][1] := Texture.LoadTexture(pchar(Skin.GetTextureFileName('LyricIconD_P' + InttoStr(I+1))),   'PNG', 'Transparent', 0);
  end;

  //atm just unset other texs
  For I := 2 to 5 do
  begin
    PlayerIconTex[I][0].TexNum := high(Cardinal); //Set to C's -1
    PlayerIconTex[I][1].TexNum := high(Cardinal);
  end;

  //Create LineTexs
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
  I:  Integer;
  countNotes: Cardinal;
  PosX: Real;
  Viewport: Array[0..3] of Integer;
begin
  //Only Add Lines if there is enough space
  If not LineinQueue then
  begin
    //Set Pointer to Line to Write
    If (LineCounter = 0) then
      LyricLine := PUpperLine //Set Upper Line
    else if (LineCounter = 1) then
      LyricLine := PLowerLine //Set Lower Line
    else
    begin
      LyricLine := PQueueLine; //Set Queue Line
      inQueue   := True;       //now there is a Queued Line
    end;
  end
  else
  begin // rotate lines (round-robin-like)
    LyricLine:=PUpperLine;
    PUpperLine:=PLowerLine;
    PLowerLine:=PQueueLine;
    PQueueLine:=LyricLine;
  end;

  //Check if Sentence has Notes
  If (Length(Line.Note) > 0) then
  begin
    //Copy Values from SongLine to LyricLine
    CountNotes := high(Line.Note);
    LyricLine.Start := Line.Note[0].Start;
    LyricLine.Length := Line.Note[CountNotes].Start + Line.Note[CountNotes].Dlugosc - LyricLine.Start;
    LyricLine.Freestyle := True; //is set by And Notes Freestyle while copying Notes
    LyricLine.Text    := '';      //Also Set while copying Notes
    LyricLine.Players := 127; //All Players for now, no Duett Mode available
    LyricLine.CurWord:=-1; // inactive line - so no word active atm
    //Copy Words
    SetLength(LyricLine.Words, CountNotes + 1);
    For I := 0 to CountNotes do
    begin
      LyricLine.Freestyle := LyricLine.Freestyle AND Line.Note[I].FreeStyle;
      LyricLine.Words[I].Start      := Line.Note[I].Start;
      LyricLine.Words[I].Length     := Line.Note[I].Dlugosc;
      LyricLine.Words[I].Text       := Line.Note[I].Tekst;
      LyricLine.Words[I].Freestyle  := Line.Note[I].FreeStyle;
      LyricLine.Text := LyricLine.Text + LyricLine.Words[I].Text
    end;

    //Set Font Params
    SetFontStyle(FontStyle);
    SetFontPos(0, 0);
    LyricLine.Size := UpperLineSize;
    SetFontSize(LyricLine.Size);
    SetFontItalic(False);
    glColor4f(1, 1, 1, 1);

    //Change Fontsize to Fit the Screen
    LyricLine.Width := glTextWidth(PChar(LyricLine.Text));
    While (LyricLine.Width > UpperLineW) do
    begin
      Dec(LyricLine.Size);

      if (LyricLine.Size <=1) then
        Break;

      SetFontSize(LyricLine.Size);
      LyricLine.Width := glTextWidth(PChar(LyricLine.Text));
    end;

    //Set Word Positions and Line Size
    PosX := 0 {LowerLineX + LowerLineW/2 + 80 - LyricLine.Width/2};
    For I := 0 to High(LyricLine.Words) do
    begin
      LyricLine.Words[I].X := PosX;
      LyricLine.Words[I].Width := glTextWidth(PChar(LyricLine.Words[I].Text));
      LyricLine.Words[I].TexPos := (PosX+1) / 1024;
      LyricLine.Words[I].TexWidth := (LyricLine.Words[I].Width-1) / 1024;

      PosX := PosX + LyricLine.Words[I].Width;
    end;


    //Create LyricTexture
    //Prepare Ogl
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
Procedure   TLyricEngine.Draw (Beat: Real);
begin
  DrawLyrics(Beat);
  LastDrawBeat:=Beat;
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
    True: IEnabled := 0;
    False: IEnabled:= 1;
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
// DrawLyricsLine(private) - Helper for Draw; Draws one LyricLine
//---------------
procedure TLyricEngine.DrawLyricsLine(const X, W, Y: Real; Size: Byte; const Line: PLyricLine; Beat: Real);
var
  I: Integer;
//  CurWord: Integer;
  CurWordStartTx,
  CurWordEndTx: Real; // texture-coordinates of start and end of current word
  CurWordStart,
  CurWordEnd: Real; // screen coordinates of current word and the rest of the sentence
  Progress: Real;
  LyricX: Real; //Left Corner on X Axis
  LyricX2: Real;//Right Corner " "
  LyricScale: Real; //Up or Downscale the Lyrics need  <- ???
  IconSize: Real;
  IconAlpha: Real;

  mybeat:string;
  mywidth:real;
  realfontsize:real;
begin
{    SetFontStyle(FontStyle);
    SetFontSize(Size);
    glColor4f(1, 1, 1, 1);

    // line start beat
    SetFontPos(50, Y-500);
    mybeat:=inttostr(trunc(line^.start*100));
    glPrint(addr(mybeat[1]));

    // current beat
    SetFontPos(250, Y-500);
    mybeat:=inttostr(trunc(beat*100));
    glPrint(addr(mybeat[1]));

    // current beat
    SetFontPos(450, Y-500);
    mybeat:=inttostr(trunc((line^.start+line^.length)*100));
    glPrint(addr(mybeat[1]));
}

  // what is this for?
  LyricScale := Size / Line.Size;

  //Draw Icons
  IconSize := (2 * Size);
  //IconAlpha := 1;
  IconAlpha := Frac(Beat/(Resolution*4));

  {DrawPlayerIcon (0, True, X, Y, IconSize, IconAlpha);
  DrawPlayerIcon (1, True, X + IconSize + 1, Y, IconSize, IconAlpha);
  DrawPlayerIcon (2, True, X + (IconSize + 1)*2, Y, IconSize, IconAlpha);}

  //Check if a Word in the Sentence is active
  if ((Line^.Start < Beat) and (Beat < Line^.Start + Line^.Length)) then
  begin
    // if this line just got active, then CurWord is still -1
    // this means, we should try to make the first word active
    // then we check if the current active word is still meant to be active
    // if not, we proceed to the next word
    if Line^.CurWord = -1 then
      Line^.CurWord:=0;
    if not ((Beat < (Line^.Words[Line^.CurWord].Start+Line^.Words[Line^.CurWord].Length))) then
    Line^.CurWord:=Line^.CurWord+1;

// !!TODO: make sure, it works if the sentence is still enabled, after last word was active
//    if Line^.CurWord > high(Line^.Words) then Line^.CurWord:=-2;

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

    //Get Start Position:
    {  Start of Line - Width of all Icons + LineWidth/2 (Center}
//    LyricX  := X + {(W - ((IconSize + 1) * 6))/2 + ((IconSize + 1) * 3) +} (W/2);
    LyricX:=X+W/2;
    LyricX2 := LyricX + Line^.Width/2;
    LyricX:=LyricX - Line^.Width/2;

    //Draw complete Sentence
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

{    glColor4f(0,1,0.1,0.1);
    glBegin(GL_QUADS);
      glVertex2f(X+W/2, Y);
      glVertex2f(X+W/2, Y + line^.size*3.5);
      glVertex2f(X+W/2+line^.width/2, Y + line^.size*3.5);
      glVertex2f(X+W/2+line^.width/2, Y);
    glEnd;
    glColor4f(0,1,0,0.1);
    glBegin(GL_QUADS);
      glVertex2f(X+W/2-line^.width/2, Y);
      glVertex2f(X+W/2-line^.width/2, Y + line^.size*3.5);
      glVertex2f(X+W/2, Y + line^.size*3.5);
      glVertex2f(X+W/2, Y);
    glEnd;

    // draw whole sentence
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, Line^.Tex);

    glColorRGB(LineColor_en);
    glBegin(GL_QUADS);
      glTexCoord2f(0, 1); glVertex2f(LyricX, Y);
      glTexCoord2f(0, 0); glVertex2f(LyricX, Y + 64);
      glTexCoord2f(Line^.Width/512, 0); glVertex2f(LyricX2, Y + 64);
      glTexCoord2f(Line^.Width/512, 1); glVertex2f(LyricX2, Y);
    glEnd;
}

    // this is actually a bit more than the real font size
    // it helps adjusting the "zoom-center"
    realfontsize:=30 * (Line^.Size/10)+16;
    // draw sentence up to current word
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, Line^.Tex);

    if HoverEffekt = 4 then
      // ball lyric effect - only highlight current word and not that ones before in this line
      glColorRGB(LineColor_en)
    else
      glColorRGB(LineColor_act);
    
    glBegin(GL_QUADS);
      glTexCoord2f(0, 1); glVertex2f(LyricX, Y);
      glTexCoord2f(0, 1-realfontsize/64); glVertex2f(LyricX, Y + realfontsize);
      glTexCoord2f(CurWordStartTx, 1-realfontsize/64); glVertex2f(LyricX+CurWordStart, Y + realfontsize);
      glTexCoord2f(CurWordStartTx, 1); glVertex2f(LyricX+CurWordStart, Y);
    glEnd;

    // draw active word:
    // type 1: simple lyric effect
    // type 4: ball lyric effect
    // only change the color of the current word
    if (HoverEffekt = 1) or (HoverEffekt = 4) then
    begin
      {
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
        glTexCoord2f(CurWordStartTx, 1); glVertex2f(LyricX+CurWordStart, Y);
        glTexCoord2f(CurWordStartTx, 0); glVertex2f(LyricX+CurWordStart, Y + 64);
        glTexCoord2f(CurWordEndTx, 0); glVertex2f(LyricX+CurWordEnd, Y + 64);
        glTexCoord2f(CurWordEndTx, 1); glVertex2f(LyricX+CurWordEnd, Y);
      glEnd;
    end
      
    // draw active word:
    // type 1: zoom lyric effect
    // change color and zoom current word
    else if HoverEffekt = 2 then
    begin
      glPushMatrix;
      glTranslatef(LyricX+CurWordStart+(CurWordEnd-CurWordStart)/2,Y+realfontsize/2,0);
      glScalef(1.0+(1-progress)/2,1.0+(1-progress)/2,1.0);
      glColor4f(LineColor_en.r,LineColor_en.g,LineColor_en.b,1-progress);
      glBegin(GL_QUADS);
        glTexCoord2f(CurWordStartTx+0.0001, 1); glVertex2f(-(CurWordEnd-CurWordStart)/2, -realfontsize/2);
        glTexCoord2f(CurWordStartTx+0.0001, 1-realfontsize/64); glVertex2f(-(CurWordEnd-CurWordStart)/2,  + realfontsize/2);
        glTexCoord2f(CurWordEndTx-0.0001, 1-realfontsize/64); glVertex2f((CurWordEnd-CurWordStart)/2,  + realfontsize/2);
        glTexCoord2f(CurWordEndTx-0.0001, 1); glVertex2f((CurWordEnd-CurWordStart)/2, -realfontsize/2);
      glEnd;
      glColor4f(LineColor_act.r,LineColor_act.g,LineColor_act.b,1);
      glBegin(GL_QUADS);
        glTexCoord2f(CurWordStartTx+0.0001, 1); glVertex2f(-(CurWordEnd-CurWordStart)/2, -realfontsize/2);
        glTexCoord2f(CurWordStartTx+0.0001, 1-realfontsize/64); glVertex2f(-(CurWordEnd-CurWordStart)/2,  + realfontsize/2);
        glTexCoord2f(CurWordEndTx-0.0001, 1-realfontsize/64); glVertex2f((CurWordEnd-CurWordStart)/2,  + realfontsize/2);
        glTexCoord2f(CurWordEndTx-0.0001, 1); glVertex2f((CurWordEnd-CurWordStart)/2, -realfontsize/2);
      glEnd;
      glPopMatrix;
    end;

    // draw rest of sentence
    glColorRGB(LineColor_en);
    glBegin(GL_QUADS);
      glTexCoord2f(CurWordEndTx, 1); glVertex2f(LyricX+CurWordEnd, Y);
      glTexCoord2f(CurWordEndTx, 1-realfontsize/64); glVertex2f(LyricX+CurWordEnd, Y + realfontsize);
      glTexCoord2f(Line^.Width/1024, 1-realfontsize/64); glVertex2f(LyricX2, Y + realfontsize);
      glTexCoord2f(Line^.Width/1024, 1); glVertex2f(LyricX2, Y);
    glEnd;


    glDisable(GL_TEXTURE_2D);
    glDisable(GL_BLEND);

{    SetFontPos(50, Y);
    SetFontSize(9);
    mybeat:=line^.words[line^.CurWord].text;
    mybeat:=inttostr(trunc(Fonts[actfont].Tex.H));
    glPrint(addr(mybeat[1]));
}
  end
  else
  begin
    //Get Start Position:
    {  Start of Line - Width of all Icons + LineWidth/2 (Center}
//    LyricX  := X + {(W - ((IconSize + 1) * 6))/2 + ((IconSize + 1) * 3) +} (W/2);
    LyricX:=X+W/2;
    LyricX2 := LyricX + Line^.Width/2;
    LyricX:=LyricX - Line^.Width/2;

    //Draw complete Sentence
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glBindTexture(GL_TEXTURE_2D, Line^.Tex);

    realfontsize:=30 * (Line^.Size/10)+16;

    glColorRGB(LineColor_dis);
    glBegin(GL_QUADS);
      glTexCoord2f(0, 1); glVertex2f(LyricX, Y);
      glTexCoord2f(0, 1-realfontsize/64); glVertex2f(LyricX, Y + realfontsize);
      glTexCoord2f(Line^.Width/1024, 1-realfontsize/64); glVertex2f(LyricX2, Y + realfontsize);
      glTexCoord2f(Line^.Width/1024, 1); glVertex2f(LyricX2, Y);
    glEnd;

    glDisable(GL_TEXTURE_2D);
{    glColor4f(0,0,0,0.1);
    glBegin(GL_QUADS);
      glTexCoord2f(0, 1); glVertex2f(LyricX, Y);
      glTexCoord2f(0, 0); glVertex2f(LyricX, Y + line^.size*3.5);
      glTexCoord2f(Line^.Width/512, 0); glVertex2f(LyricX2, Y + line^.size*3.5);
      glTexCoord2f(Line^.Width/512, 1); glVertex2f(LyricX2, Y);
    glEnd;
}

    glDisable(GL_BLEND);
//    glDisable(GL_TEXTURE_2D);
{    SetFontPos(0, Y);
    SetFontSize(9);
    glColor4f(1,1,0,1);
    mybeat:=inttostr(line^.size);
    glPrint(addr(mybeat[1]));
{    mywidth:=gltextwidth(addr(mybeat[1]));
    glEnable(GL_BLEND);
    glColor4f(0,0,1,0.1);
    glBegin(GL_QUADS);
      glVertex2f(0,y);
      glVertex2f(0,y+64);
      glVertex2f(0+mywidth,y+64);
      glVertex2f(0+mywidth,y);
    glEnd;
    glDisable(GL_BLEND);
}

  end;

  {//Search for active Word
  For I := 0 to High(Line.Words) do
    if (Line.Words[I].Start < Beat) then
    begin
      CurWord := I - 1;
    end;

  if (CurWord < 0) then Exit;

  //Draw Part until cur Word
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_COLOR {GL_ONE_MINUS_SRC_COLOR}{, GL_ONE_MINUS_SRC_COLOR);
  glBindTexture(GL_TEXTURE_2D, Line.Tex);

  glColorRGB(LineColor_en);
  glBegin(GL_QUADS);
    glTexCoord2f(0, 1); glVertex2f(X, Y);
    glTexCoord2f(0, 0); glVertex2f(X, Y + 64 * W / 512);
    glTexCoord2f(Line.Words[CurWord].TexPos, 0); glVertex2f(X + W, Y + 64 * W / 512);
    glTexCoord2f(Line.Words[CurWord].TexPos, 1); glVertex2f(X + W, Y);
  glEnd;


  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);}
end;


end.

