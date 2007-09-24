unit USingScores;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses UThemes,
     OpenGl12,
     UTexture;

//Some Constances containing Options that could change by time
const
  MaxPlayers = 6;   //Maximum of Players that could be added
  MaxPositions = 6; //Maximum of Score Positions that could be added

type
  //-----------
  // TScorePlayer - Record Containing Information about a Players Score
  //-----------
  TScorePlayer = record
    Position: Byte;    //Index of the Position where the Player should be Drawn
    Enabled:  Boolean; //Is the Score Display Enabled
    Visible:  Boolean; //Is the Score Display Visible
    Score:    Word;    //Current Score of the Player
    ScoreDisplayed: Word; //Score cur. Displayed(for counting up)
    ScoreBG:  TTexture;//Texture of the Players Scores BG
    Color:    TRGB;    //Teh Players Color
  end;
  aScorePlayer = array[0..MaxPlayers-1] of TScorePlayer;

  //-----------
  // TScorePosition - Record Containing Information about a Score Position, that can be used
  //-----------
  PScorePosition = ^TScorePosition;
  TScorePosition = record
    //The Position is Used for Which Playercount
    PlayerCount: Byte;
    // 1 - One Player per Screen
    // 2 - 2 Players per Screen
    // 4 - 3 Players per Screen
    // 6 would be 2 and 3 Players per Screen

    BGX: Real;     //X Position of the Score BG
    BGY: Real;     //Y Position of the Score BG
    BGW: Real;     //Width of the Score BG
    BGH: Real;     //Height of the Score BG

    RBX: Real;     //X Position of the Rating Bar
    RBY: Real;     //Y Position of the Rating Bar
    RBW: Real;     //Width of the Rating Bar
    RBH: Real;     //Height of the Rating Bar

    TextX: Real;        //X Position of the Score Text
    TextY: Real;        //Y Position of the Score Text
    TextFont: Byte;     //Font of the Score Text
    TextSize: Byte;     //Size of the Score Text

    PUW: Real;          //Width of the LineBonus Popup
    PUH: Real;          //Height of the LineBonus Popup
    PUFont: Byte;       //Font for the PopUps
    PUFontSize: Byte;   //FontSize for the PopUps
    PUStartX: Real;     //X Start Position of the LineBonus Popup
    PUStartY: Real;     //Y Start Position of the LineBonus Popup
    PUTargetX: Real;    //X Target Position of the LineBonus Popup
    PUTargetY: Real;    //Y Target Position of the LineBonus Popup
  end;
  aScorePosition = array[0..MaxPositions-1] of TScorePosition;

  //-----------
  // TScorePopUp - Record Containing Information about a LineBonus Popup
  // List, Next Item is Saved in Next attribute
  //-----------
  PScorePopUp = ^TScorePopUp;
  TScorePopUp = record
    Player:  Byte;          //Index of the PopUps Player
    TimeStamp: Cardinal;    //Timestamp of Popups Spawn
    Rating:    Byte;        //0 to 8, Type of Rating (Cool, bad, etc.)
    ScoreGiven:Word;        //Score that has already been given to the Player
    ScoreDiff: Word;        //Difference Between Cur Score at Spawn and Old Score
    Next:      PScorePopUp; //Next Item in List
  end;
  aScorePopUp = array of TScorePopUp;

  //-----------
  // TSingScores - Class containing Scores Positions and Drawing Scores, Rating Bar + Popups
  //-----------
  TSingScores = class
    private
      Positions: aScorePosition;
      aPlayers: aScorePlayer;
      oPositionCount: Byte;
      oPlayerCount: Byte;

      //Saves the First and Last Popup of the List
      FirstPopUp: PScorePopUp;
      LastPopUp:  PScorePopUp;

      //Procedure Draws a Popup by Pointer
      Procedure DrawPopUp(const PopUp: PScorePopUp);

      //Procedure Draws a Score by Playerindex
      Procedure DrawScore(const Index: Integer);

      //Procedure Removes a PopUp w/o destroying the List
      Procedure KillPopUp(const last, cur: PScorePopUp);
    public
      Settings: record //Record containing some Displaying Options
        Phase1Time: Real;     //time for Phase 1 to complete (in msecs)
                              //The Plop Up of the PopUp
        Phase2Time: Real;     //time for Phase 2 to complete (in msecs)
                              //The Moving (mainly Upwards) of the Popup
        Phase3Time: Real;     //time for Phase 3 to complete (in msecs)
                              //The Fade out and Score adding

        PopUpTex:   Array [0..8] of TTexture; //Textures for every Popup Rating

        RatingBar_BG_Tex:   TTexture; //Rating Bar Texs
        RatingBar_FG_Tex:   TTexture;
        RatingBar_Bar_Tex:  TTexture;

      end;

      //Propertys for Reading Position and Playercount
      Property PositionCount: Byte read oPositionCount;
      Property PlayerCount: Byte read oPlayerCount;
      Property Players: aScorePlayer read aPlayers;

      //Constructor just sets some standard Settings
      Constructor Create;

      //Procedure Adds a Position to Array and Increases Position Count
      Procedure AddPosition(const pPosition: PScorePosition);

      //Procedure Adds a Player to Array and Increases Player Count
      Procedure AddPlayer(const ScoreBG: TTexture; const Color: TRGB; const Score: Word = 0; const Enabled: Boolean = True; const Visible: Boolean = True);

      //Change a Players Visibility, Enable
      Procedure ChangePlayerVisibility(const Index: Byte; const pVisible: Boolean);
      Procedure ChangePlayerEnabled(const Index: Byte; const pEnabled: Boolean);

      //Procedure Deletes all Player Information
      Procedure ClearPlayers;

      //Procedure Deletes Positions and Playerinformation
      Procedure Clear;

      //Procedure Loads some Settings and the Positions from Theme
      Procedure LoadfromTheme;

      //Procedure has to be called after Positions and Players have been added, before first call of Draw
      //It gives every Player a Score Position
      Procedure Init;

      //Spawns a new Line Bonus PopUp for the Player
      Procedure SpawnPopUp(const PlayerIndex: Byte; const Rating: Byte; const Score: Word);

      //Removes all PopUps from Mem
      Procedure KillAllPopUps;

      //Procedure Draws Scores and Linebonus PopUps
      Procedure Draw;
  end;


implementation

uses SDL,
     SysUtils,
     ULog,
     UGraphic,
     TextGL;

//-----------
//Constructor just sets some standard Settings
//-----------
Constructor TSingScores.Create;
begin
  //Clear PopupList Pointers
  FirstPopUp := nil;
  LastPopUp  := nil;
  
  //Clear Position Index
  oPositionCount  := 0;
  oPlayerCount    := 0;

  Settings.Phase1Time := 1000;
  Settings.Phase2Time := 2000;
  Settings.Phase3Time := 2000;

  Settings.PopUpTex[0].TexNum := High(gluInt);
  Settings.PopUpTex[1].TexNum := High(gluInt);
  Settings.PopUpTex[2].TexNum := High(gluInt);
  Settings.PopUpTex[3].TexNum := High(gluInt);
  Settings.PopUpTex[4].TexNum := High(gluInt);
  Settings.PopUpTex[5].TexNum := High(gluInt);
  Settings.PopUpTex[6].TexNum := High(gluInt);
  Settings.PopUpTex[7].TexNum := High(gluInt);
  Settings.PopUpTex[8].TexNum := High(gluInt);

  Settings.RatingBar_BG_Tex.TexNum   := High(gluInt);
  Settings.RatingBar_FG_Tex.TexNum   := High(gluInt);
  Settings.RatingBar_Bar_Tex.TexNum  := High(gluInt);
end;

//-----------
//Procedure Adds a Position to Array and Increases Position Count
//-----------
Procedure TSingScores.AddPosition(const pPosition: PScorePosition);
begin
  if (PositionCount < MaxPositions) then
  begin
    Positions[PositionCount] := pPosition^;

    Inc(oPositionCount);
  end;
end;

//-----------
//Procedure Adds a Player to Array and Increases Player Count
//-----------
Procedure TSingScores.AddPlayer(const ScoreBG: TTexture; const Color: TRGB; const Score: Word; const Enabled: Boolean; const Visible: Boolean);
begin
  if (PlayerCount < MaxPlayers) then
  begin
    aPlayers[PlayerCount].Position  := High(byte);
    aPlayers[PlayerCount].Enabled   := Enabled;
    aPlayers[PlayerCount].Visible   := Visible;
    aPlayers[PlayerCount].Score     := Score;
    aPlayers[PlayerCount].ScoreDisplayed     := Score;
    aPlayers[PlayerCount].ScoreBG   := ScoreBG;
    aPlayers[PlayerCount].Color     := Color;

    Inc(oPlayerCount);
  end;
end;

//-----------
//Change a Players Visibility
//-----------
Procedure TSingScores.ChangePlayerVisibility(const Index: Byte; const pVisible: Boolean);
begin
  if (Index < MaxPlayers) then
    aPlayers[Index].Visible := pVisible;
end;

//-----------
//Change Player Enabled
//-----------
Procedure TSingScores.ChangePlayerEnabled(const Index: Byte; const pEnabled: Boolean);
begin
  if (Index < MaxPlayers) then
    aPlayers[Index].Enabled := pEnabled;
end;

//-----------
//Procedure Deletes all Player Information
//-----------
Procedure TSingScores.ClearPlayers;    
begin
  KillAllPopUps;
  oPlayerCount := 0;
end;

//-----------
//Procedure Deletes Positions and Playerinformation
//-----------
Procedure TSingScores.Clear; 
begin
  KillAllPopUps;
  oPlayerCount    := 0;
  oPositionCount  := 0;
end;

//-----------
//Procedure Loads some Settings and the Positions from Theme
//-----------
Procedure TSingScores.LoadfromTheme;
var I: Integer;
  Procedure AddbyStatics(const PC: Byte; const ScoreStatic, SingBarStatic: TThemeStatic; ScoreText: TThemeText);
    var nPosition: TScorePosition;
  begin
    nPosition.PlayerCount := PC; //Only for one Player Playing

    nPosition.BGX := ScoreStatic.X;
    nPosition.BGY := ScoreStatic.Y;
    nPosition.BGW := ScoreStatic.W;
    nPosition.BGH := ScoreStatic.H;

    nPosition.TextX     := ScoreText.X;
    nPosition.TextY     := ScoreText.Y;
    nPosition.TextFont  := ScoreText.Font;
    nPosition.TextSize  := ScoreText.Size;

    nPosition.RBX := SingBarStatic.X;
    nPosition.RBY := SingBarStatic.Y;
    nPosition.RBW := SingBarStatic.W;
    nPosition.RBH := SingBarStatic.H;

    nPosition.PUW := nPosition.BGW;
    nPosition.PUH := nPosition.BGH;

    nPosition.PUFont     := 2;
    nPosition.PUFontSize := 6;

    nPosition.PUStartX := nPosition.BGX;
    nPosition.PUStartY := nPosition.TextY + 65;

    nPosition.PUTargetX := nPosition.BGX;
    nPosition.PUTargetY := nPosition.TextY;

    AddPosition(@nPosition);
  end;
begin
  Clear;

  //Set Textures
  //Popup Tex
  For I := 0 to 8 do
    Settings.PopUpTex[I] := Tex_SingLineBonusBack[I];

  //Rating Bar Tex  
  Settings.RatingBar_BG_Tex   :=  Tex_SingBar_Back;
  Settings.RatingBar_FG_Tex   :=  Tex_SingBar_Front;
  Settings.RatingBar_Bar_Tex  :=  Tex_SingBar_Bar;

  //Load Positions from Theme

  // Player1:
  AddByStatics(1, Theme.Sing.StaticP1ScoreBG, Theme.Sing.StaticP1SingBar, Theme.Sing.TextP1Score);
  AddByStatics(2, Theme.Sing.StaticP1TwoPScoreBG, Theme.Sing.StaticP1TwoPSingBar, Theme.Sing.TextP1TwoPScore);
  AddByStatics(4, Theme.Sing.StaticP1ThreePScoreBG, Theme.Sing.StaticP1ThreePSingBar, Theme.Sing.TextP1ThreePScore);

  // Player2:
  AddByStatics(2, Theme.Sing.StaticP2RScoreBG, Theme.Sing.StaticP2RSingBar, Theme.Sing.TextP2RScore);
  AddByStatics(4, Theme.Sing.StaticP2MScoreBG, Theme.Sing.StaticP2MSingBar, Theme.Sing.TextP2MScore);

  // Player3:
  AddByStatics(4, Theme.Sing.StaticP3RScoreBG, Theme.Sing.StaticP3RScoreBG, Theme.Sing.TextP3RScore);
end;

//-----------
//Spawns a new Line Bonus PopUp for the Player
//-----------
Procedure TSingScores.SpawnPopUp(const PlayerIndex: Byte; const Rating: Byte; const Score: Word);
var Cur: PScorePopUp;
begin
  Log.LogError('Spawn PopUp: Score: ' + InttoStr(Score));
  if (PlayerIndex < PlayerCount) then
  begin
    //Get Memory and Add Data
    GetMem(Cur, SizeOf(TScorePopUp));

    Cur.Player  := PlayerIndex;
    Cur.TimeStamp := SDL_GetTicks;
    Cur.Rating    := Rating;
    Cur.ScoreGiven:= 0;
    If (Players[PlayerIndex].Score < Score) then
    begin
      Cur.ScoreDiff := Score - Players[PlayerIndex].Score;
      aPlayers[PlayerIndex].Score := Score;
    end
    else
      Cur.ScoreDiff := 0;
    Cur.Next := nil;

    //Log.LogError('TSingScores.SpawnPopUp| Player: ' + InttoStr(PlayerIndex) + ', Score: ' + InttoStr(Score) + ', ScoreDiff: ' + InttoStr(Cur.ScoreDiff));

    //Add it to the Chain
    if (FirstPopUp = nil) then
      //the first PopUp in the List
      FirstPopUp := Cur
    else
    //second or earlier popup
      LastPopUp.Next := Cur;

    //Set new Popup to Last PopUp in the List
    LastPopUp := Cur;
  end
  else
    Log.LogError('TSingScores: Try to add PopUp for not existing player');
end;

//-----------
// Removes a PopUp w/o destroying the List
//-----------
Procedure TSingScores.KillPopUp(const last, cur: PScorePopUp);
begin
  //Give Player the Last Points that missing till now
  aPlayers[Cur.Player].ScoreDisplayed := aPlayers[Cur.Player].ScoreDisplayed + Cur.ScoreDiff - Cur.ScoreGiven;

  //If this is the First PopUp => Make Next PopUp the First
  If (Cur = FirstPopUp) then
    FirstPopUp := Cur.Next
  //Else => Remove Curent Popup from Chain
  else
    Last.Next := Cur.Next;

  //If this is the Last PopUp, Make PopUp before the Last
  If (Cur = LastPopUp) then
    LastPopUp := Last;

  //Free the Memory
  FreeMem(Cur, SizeOf(TScorePopUp));
end;

//-----------
//Removes all PopUps from Mem
//-----------
Procedure TSingScores.KillAllPopUps;
var
  Cur:  PScorePopUp;
  Last: PScorePopUp;
begin
  Cur := FirstPopUp;

  //Remove all PopUps:
  While (Cur <> nil) do
  begin
    Last := Cur;
    Cur  := Cur.Next;
    FreeMem(Last, SizeOf(TScorePopUp));
  end;

  FirstPopUp := nil;
  LastPopUp := nil;
end;

//-----------
//Init - has to be called after Positions and Players have been added, before first call of Draw
//It gives every Player a Score Position
//-----------
Procedure TSingScores.Init;
var
  PlC: Array [0..1] of Byte; //Playercount First Screen and Second Screen
  I, J: Integer;
  MaxPlayersperScreen: Byte;
  CurPlayer:           Byte;

  Function GetPositionCountbyPlayerCount(bPlayerCount: Byte): Byte;
  var I: Integer;
  begin
    Result := 0;
    bPlayerCount := 1 shl (bPlayerCount - 1);

    For I := 0 to PositionCount-1 do
    begin
      If ((Positions[I].PlayerCount AND bPlayerCount) <> 0) then
        Inc(Result);
    end;
  end;

  Function GetPositionbyPlayernum(bPlayerCount, bPlayer: Byte): Byte;
  var I: Integer;
  begin
    bPlayerCount := 1 shl (bPlayerCount - 1);
    Result := High(Byte);

    For I := 0 to PositionCount-1 do
    begin
      If ((Positions[I].PlayerCount AND bPlayerCount) <> 0) then
      begin
        If (bPlayer = 0) then
        begin
          Result := I;
          Break;
        end
        else
          Dec(bPlayer);
      end;
    end;
  end;

begin

  For I := 1 to 6 do
  begin
    //If there are enough Positions -> Write to MaxPlayers
    If (GetPositionCountbyPlayerCount(I) = I) then
      MaxPlayersperScreen := I
    else
      Break;
  end;


  //Split Players to both Screen or Display on One Screen
  if (Screens = 2) and (MaxPlayersperScreen < PlayerCount) then
  begin
    PlC[0] := PlayerCount div 2 + PlayerCount mod 2;
    PlC[1] := PlayerCount div 2;
  end
  else
  begin
    PlC[0] := PlayerCount;
    PlC[1] := 0;
  end;


  //Check if there are enough Positions for all Players
  For I := 0 to Screens - 1 do
  begin
    if (PlC[I] > MaxPlayersperScreen) then
    begin
      PlC[I] := MaxPlayersperScreen;
      Log.LogError('More Players than available Positions, TSingScores');
    end;
  end;
  
  CurPlayer := 0;
  //Give every Player a Position
  For I := 0 to Screens - 1 do
    For J := 0 to PlC[I]-1 do
    begin
      aPlayers[CurPlayer].Position := GetPositionbyPlayernum(PlC[I], J) OR (I shl 7);
      //Log.LogError('Player ' + InttoStr(CurPlayer) + ' gets Position: ' + InttoStr(aPlayers[CurPlayer].Position));
      Inc(CurPlayer);
    end;
end;

//-----------
//Procedure Draws Scores and Linebonus PopUps
//-----------
Procedure TSingScores.Draw;
var
  I: Integer;
  CurTime: Cardinal;
  CurPopUp, LastPopUp: PScorePopUp;
begin
  CurTime := SDL_GetTicks;
  
  //Draw Popups
  LastPopUp := nil;
  CurPopUp  := FirstPopUp;
  
  While (CurPopUp <> nil) do
  begin
    if (CurTime - CurPopUp.TimeStamp > Settings.Phase1Time + Settings.Phase2Time + Settings.Phase3Time) then
    begin
      KillPopUp(LastPopUp, CurPopUp);
      if (LastPopUp = nil) then
        CurPopUp := FirstPopUp
      else
        CurPopUp  := LastPopUp.Next;
    end
    else
    begin
      DrawPopUp(CurPopUp);
      LastPopUp := CurPopUp;
      CurPopUp  := LastPopUp.Next;
    end;
  end;


  //Draw Players
  For I := 0 to PlayerCount-1 do
  begin
    DrawScore(I);
  end;
end;

//-----------
//Procedure Draws a Popup by Pointer
//-----------
Procedure TSingScores.DrawPopUp(const PopUp: PScorePopUp);
var
  Progress: Real;
  CurTime:  Cardinal;
  X, Y, W, H, Alpha: Real;
  FontSize: Byte;
  TimeDiff: Cardinal;
  PIndex:   Byte;
  TextLen:  Real;
  ScoretoAdd: Word;
  PosDiff:  Real;
begin
  if (PopUp <> nil) then
  begin
    //Only Draw if Player has a Position
    PIndex := Players[PopUp.Player].Position;
    If PIndex <> high(byte) then
    begin
      //Only Draw if Player is on Cur Screen
      If ((Players[PopUp.Player].Position AND 128) = 0) = (ScreenAct = 1) then
      begin
        CurTime := SDL_GetTicks;
        TimeDiff := CurTime - PopUp.TimeStamp;

        //Get Position of PopUp
        PIndex := PIndex AND 127;

    
        //Check for Phase ...
        If (TimeDiff <= Settings.Phase1Time) then
        begin
          //Phase 1 - The Ploping up
          Progress := TimeDiff / Settings.Phase1Time;


          W := Positions[PIndex].PUW * Sin(Progress/2*Pi);
          H := Positions[PIndex].PUH * Sin(Progress/2*Pi);

          X := Positions[PIndex].PUStartX + (Positions[PIndex].PUW - W)/2;
          Y := Positions[PIndex].PUStartY + (Positions[PIndex].PUH - H)/2;

          FontSize := Round(Progress * Positions[PIndex].PUFontSize);
          Alpha := 1;
        end

        Else If (TimeDiff <= Settings.Phase2Time + Settings.Phase1Time) then
        begin
          //Phase 2 - The Moving
          Progress := (TimeDiff - Settings.Phase1Time) / Settings.Phase2Time;

          W := Positions[PIndex].PUW;
          H := Positions[PIndex].PUH;

          PosDiff := Positions[PIndex].PUTargetX - Positions[PIndex].PUStartX;
          If PosDiff > 0 then
            PosDiff := PosDiff + W;
          X := Positions[PIndex].PUStartX + PosDiff * sqr(Progress);

          PosDiff := Positions[PIndex].PUTargetY - Positions[PIndex].PUStartY;
          If PosDiff < 0 then
            PosDiff := PosDiff + Positions[PIndex].BGH;
          Y := Positions[PIndex].PUStartY + PosDiff * sqr(Progress);

          FontSize := Positions[PIndex].PUFontSize;
          Alpha := 1 - 0.3 * Progress;
        end

        else
        begin
          //Phase 3 - The Fading out + Score adding
          Progress := (TimeDiff - Settings.Phase1Time - Settings.Phase2Time) / Settings.Phase3Time;

          If (PopUp.Rating > 0) then
          begin
            //Add Scores
            ScoreToAdd := Round(PopUp.ScoreDiff * Progress) - PopUp.ScoreGiven;
            Inc(PopUp.ScoreGiven, ScoreToAdd);
            aPlayers[PopUp.Player].ScoreDisplayed := Players[PopUp.Player].ScoreDisplayed + ScoreToAdd;

            //Set Positions etc.
            Alpha    := 0.7 - 0.7 * Progress;

            W := Positions[PIndex].PUW;
            H := Positions[PIndex].PUH;

            PosDiff := Positions[PIndex].PUTargetX - Positions[PIndex].PUStartX;
            If (PosDiff > 0) then
              PosDiff := W
            else
              PosDiff := 0;
            X := Positions[PIndex].PUTargetX + PosDiff * Progress;

            PosDiff := Positions[PIndex].PUTargetY - Positions[PIndex].PUStartY;
            If (PosDiff < 0) then
              PosDiff := -Positions[PIndex].BGH
            else
              PosDiff := 0;
            Y := Positions[PIndex].PUTargetY - PosDiff * (1-Progress);

            FontSize := Positions[PIndex].PUFontSize;
          end
          else
          begin
            //Here the Effect that Should be shown if a PopUp without Score is Drawn
            //And or Spawn with the GraphicObjects etc.
            //Some Work for Blindy to do :P

            //ATM: Just Let it Slide in the Scores just like the Normal PopUp
            Alpha := 0;
          end;
        end;

        //Draw PopUp

        if (Alpha > 0) then
        begin
          //Draw BG:
          glEnable(GL_TEXTURE_2D);
          glEnable(GL_BLEND);
          glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

          glColor4f(1,1,1, Alpha);
          glBindTexture(GL_TEXTURE_2D, Settings.PopUpTex[PopUp.Rating].TexNum);

          glBegin(GL_QUADS);
            glTexCoord2f(0, 0); glVertex2f(X, Y);
            glTexCoord2f(0, Settings.PopUpTex[PopUp.Rating].TexH); glVertex2f(X, Y + H);
            glTexCoord2f(Settings.PopUpTex[PopUp.Rating].TexW, Settings.PopUpTex[PopUp.Rating].TexH); glVertex2f(X + W, Y + H);
            glTexCoord2f(Settings.PopUpTex[PopUp.Rating].TexW, 0); glVertex2f(X + W, Y);
          glEnd;

          glDisable(GL_TEXTURE_2D);
          glDisable(GL_BLEND);

          //Set FontStyle and Size
          SetFontStyle(Positions[PIndex].PUFont);
          SetFontItalic(False);
          SetFontSize(FontSize);

          //Draw Text
          TextLen := glTextWidth(PChar(Theme.Sing.LineBonusText[PopUp.Rating]));

          //Color and Pos
          SetFontPos (X + (W - TextLen) / 2, Y + 12);
          glColor4f(1, 1, 1, Alpha);

          //Draw
          glPrint(PChar(Theme.Sing.LineBonusText[PopUp.Rating]));
        end; //eo Alpha check
      end; //eo Right Screen
    end; //eo Player has Position
  end
  else
    Log.LogError('TSingScores: Try to Draw a not existing PopUp');
end;

//-----------
//Procedure Draws a Score by Playerindex
//-----------
Procedure TSingScores.DrawScore(const Index: Integer);
var
  Position: PScorePosition;
  ScoreStr: String;
begin
  //Only Draw if Player has a Position
  If Players[Index].Position <> high(byte) then
  begin
    //Only Draw if Player is on Cur Screen
    If ((Players[Index].Position AND 128) = 0) = (ScreenAct = 1) then
    begin
      Position := @Positions[Players[Index].Position and 127];

      //Draw ScoreBG
      glEnable(GL_TEXTURE_2D);
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

      glColor4f(1,1,1, 1);
      glBindTexture(GL_TEXTURE_2D, Players[Index].ScoreBG.TexNum);

      glBegin(GL_QUADS);
        glTexCoord2f(0, 0); glVertex2f(Position.BGX, Position.BGY);
        glTexCoord2f(0, Players[Index].ScoreBG.TexH); glVertex2f(Position.BGX, Position.BGY + Position.BGH);
        glTexCoord2f(Players[Index].ScoreBG.TexW, Players[Index].ScoreBG.TexH); glVertex2f(Position.BGX + Position.BGW, Position.BGY + Position.BGH);
        glTexCoord2f(Players[Index].ScoreBG.TexW, 0); glVertex2f(Position.BGX + Position.BGW, Position.BGY);
      glEnd;

      glDisable(GL_TEXTURE_2D);
      glDisable(GL_BLEND);

      //Draw Score Text
      SetFontStyle(Position.TextFont);
      SetFontItalic(False);
      SetFontSize(Position.TextSize);
      SetFontPos(Position.TextX, Position.TextY);

      ScoreStr := InttoStr(Players[Index].ScoreDisplayed div 10) + '0';
      While (Length(ScoreStr) < 5) do
        ScoreStr := '0' + ScoreStr;

      glPrint(PChar(ScoreStr));

    end; //eo Right Screen
  end; //eo Player has Position
end;

end.
