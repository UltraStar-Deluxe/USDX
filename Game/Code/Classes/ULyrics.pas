unit ULyrics;

interface

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
    Start:      Cardinal;     //Start in Quarters of teh Line
    Length:     Cardinal;     //Length in Quarters (From Start of First Note to the End of Last Note)
    Freestyle:  Boolean;      //Complete Line is Freestyle ?
    Players:    Byte;         //Which Players Sing this Line (1: Player1; 2: Player2; 4: Player3; [..])
    Done:       Boolean;      //Is Sentence Sung
  end;

  TLyricEngine = class
    private
      EoLastSentence: Real;       //When did the Last Sentence End (in Beats)
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
      procedure DrawLyricsLine(const X, W, Y: Real; Size: Byte; const Line: TLyricLine; Beat: Real);
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
      LineColor_akt:  TRGBA;      //Color of teh active Word
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
     dialogs;

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
    GetMem(pTexData, 1024*128*4); //get Memory to save Tex in

    //generate and bind Texture
    glGenTextures(1, @Result);
    glBindTexture(GL_TEXTURE_2D, Result);

    //Get Memory
    glTexImage2D(GL_TEXTURE_2D, 0, 4, 1024, 64, 0, GL_RGBA, GL_UNSIGNED_BYTE, pTexData);

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
  begin
    LyricLine:=PUpperLine;
    PUpperLine:=PLowerLine;
    PLowerLine:=PQueueLine;
    PQueueLine:=LyricLine;
  end;

  //Check if Sentence has Notes
  If (Length(Line.Nuta) > 0) then
  begin
    //Copy Values from SongLine to LyricLine
    CountNotes := high(Line.Nuta);
    LyricLine.Start := Line.Nuta[0].Start;
    LyricLine.Length := Line.Nuta[CountNotes].Start + Line.Nuta[CountNotes].Dlugosc - LyricLine.Start;
    LyricLine.Freestyle := True; //is set by And Notes Freestyle while copying Notes
    LyricLine.Text    := '';      //Also Set while copying Notes
    LyricLine.Players := 127; //All Players for now, no Duett Mode available
    //Copy Words
    SetLength(LyricLine.Words, CountNotes + 1);
    For I := 0 to CountNotes do
    begin
      LyricLine.Freestyle := LyricLine.Freestyle AND Line.Nuta[I].FreeStyle;
      LyricLine.Words[I].Start      := Line.Nuta[I].Start;
      LyricLine.Words[I].Length     := Line.Nuta[I].Dlugosc;
      LyricLine.Words[I].Text       := Line.Nuta[I].Tekst;
      LyricLine.Words[I].Freestyle  := Line.Nuta[I].FreeStyle;
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
    While (LyricLine.Width > 508) do
    begin
      Dec(LyricLine.Size);

      if (LyricLine.Size <=1) then
        Break;

      SetFontSize(LyricLine.Size);
      LyricLine.Width := glTextWidth(PChar(LyricLine.Text));
    end;

    //Set Word Positions and Line Size
    PosX := 2 {LowerLineX + LowerLineW/2 + 80 - LyricLine.Width/2};
    For I := 0 to High(LyricLine.Words) do
    begin
      LyricLine.Words[I].X := PosX;
      LyricLine.Words[I].Width := glTextWidth(PChar(LyricLine.Words[I].Text));
      LyricLine.Words[I].TexPos := PosX / 512;
      LyricLine.Words[I].TexWidth := LyricLine.Words[I].TexWidth / 512;

      PosX := PosX + LyricLine.Words[I].Width;
    end;

    //Create LyricTexture
    //Prepare Ogl
    glGetIntegerv(GL_VIEWPORT, @ViewPort);
    glClearColor(0.0,0.0,0.0,0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    {glMatrixMode(GL_PROJECTION);
      glLoadIdentity;
      glOrtho(0, 1024, 64, 0, -1, 100);
    glMatrixMode(GL_MODELVIEW);}
    glViewport(0, 0, 512, 512);

    //Draw Lyrics
    SetFontPos(0, 0);
    glPrint(PChar(LyricLine.Text));

    Display.ScreenShot;
    //Copy to Texture
    glBindTexture(GL_TEXTURE_2D, LyricLine.Tex);
    glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 0, 448, 512, 64, 0);

    //Clear Buffer
    glClearColor(0,0,0,0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    glViewPort(ViewPort[0], ViewPort[1], ViewPort[2], ViewPort[3]);
    {glMatrixMode(GL_PROJECTION);
      glLoadIdentity;
      glOrtho(0, RenderW, RenderH, 0, -1, 100);
    glMatrixMode(GL_MODELVIEW); }
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

    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_Alpha {GL_ONE_MINUS_SRC_COLOR}, GL_ONE_MINUS_SRC_Alpha);
    glBindTexture(GL_TEXTURE_2D, PUpperLine^.Tex);

    glColor4f(1,1,0,1);
    glBegin(GL_QUADS);
      glTexCoord2f(0, 1); glVertex2f(100, 100);
      glTexCoord2f(0, 0); glVertex2f(100, 200);
      glTexCoord2f(1, 0); glVertex2f(612, 200);
      glTexCoord2f(1, 1); glVertex2f(612, 100);
    glEnd;

    glBindTexture(GL_TEXTURE_2D, PLowerLine^.Tex);

    glColor4f(1,0,1,1);
    glBegin(GL_QUADS);
      glTexCoord2f(0, 1); glVertex2f(100, 200);
      glTexCoord2f(0, 0); glVertex2f(100, 300);
      glTexCoord2f(1, 0); glVertex2f(612, 300);
      glTexCoord2f(1, 1); glVertex2f(612, 200);
    glEnd;

    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);

end;

//---------------
// DrawLyrics(private) - Helper for Draw; main Drawing procedure
//---------------
procedure TLyricEngine.DrawLyrics (Beat: Real);
begin
  DrawLyricsLine(UpperLineX, UpperLineW, UpperlineY, 15, Upperline, Beat);
  DrawLyricsLine(LowerLineX, LowerLineW, LowerlineY, 15, Lowerline, Beat);
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
procedure TLyricEngine.DrawLyricsLine(const X, W, Y: Real; Size: Byte; const Line: TLyricLine; Beat: Real);
var
  I: Integer;
  CurWord: Integer;
  Progress: Real;
  LyricX: Real; //Left Corner on X Axis
  LyricX2: Real;//Right Corner " "
  LyricScale: Real; //Up or Downscale the Lyrics need
  IconSize: Real;
  IconAlpha: Real;
begin

{  For I := 0 to High(Line.Words) do
  begin
    //Set Font Params
    SetFontStyle(FontStyle);
    SetFontSize(Size);
    SetFontItalic(Line.Words[I].Freestyle);
    glColor4f(1, 1, 1, 1);

    SetFontPos(Line.Words[I].X, Y);

    glPrint(PChar(Line.Words[I].Text));
  end; }

  LyricScale := Size / Line.Size;

  //Draw Icons
  IconSize := (2 * Size);
  //IconAlpha := 1;
  IconAlpha := Frac(Beat/(Resolution*4));

  {DrawPlayerIcon (0, True, X, Y, IconSize, IconAlpha);
  DrawPlayerIcon (1, True, X + IconSize + 1, Y, IconSize, IconAlpha);
  DrawPlayerIcon (2, True, X + (IconSize + 1)*2, Y, IconSize, IconAlpha);}

  //Check if a Word in the Sentence is active
  if ((Line.Start > Beat) AND (Line.Start + Line.Length < Beat)) then
  begin
    //Get Start Position:
    {  Start of Line - Width of all Icons + LineWidth/2 (Center}
    LyricX  := X + (W - ((IconSize + 1) * 6))/2 + ((IconSize + 1) * 3);

    LyricX2 := LyricX + Line.Width;

    //Draw complete Sentence
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_COLOR {GL_ONE_MINUS_SRC_COLOR}, GL_ONE_MINUS_SRC_COLOR);
    glBindTexture(GL_TEXTURE_2D, Line.Tex);

    glColorRGB(LineColor_en);
    glBegin(GL_QUADS);
      glTexCoord2f(0, 1); glVertex2f(LyricX, Y);
      glTexCoord2f(0, 0); glVertex2f(LyricX, Y + 64 * W / 512);
      glTexCoord2f(1, 0); glVertex2f(LyricX + LyricX2, Y + 64 * W / 512);
      glTexCoord2f(1, 1); glVertex2f(LyricX + LyricX2, Y);
    glEnd;


    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
  end
  else
  begin

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

