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

unit USingScores;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UThemes,
  gl,
  UTexture;

//////////////////////////////////////////////////////////////
//                        ATTENTION:                        //
// Enabled Flag does not Work atm. This should cause Popups //
// Not to Move and Scores to stay until Renenabling.        //
// To use e.g. in Pause Mode                                //
// Also InVisible Flag causes Attributes not to change.     //
// This should be fixed after next Draw when Visible = True,//
// but not testet yet                                       //
//////////////////////////////////////////////////////////////

//Some constants containing options that could change by time
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
    RBPos:    Real;    //Cur. Percentille of the Rating Bar
    RBTarget: Real;    //Target Position of Rating Bar
    RBVisible:Boolean; //Is Rating bar Drawn
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

    TextX: Real;         //X Position of the Score Text
    TextY: Real;         //Y Position of the Score Text
    TextFont: Byte;      //Font of the Score Text
    TextSize: integer;   //Size of the Score Text

    PUW: Real;           //Width of the LineBonus Popup
    PUH: Real;           //Height of the LineBonus Popup
    PUFont: Byte;        //Font for the PopUps
    PUFontSize: integer; //FontSize for the PopUps
    PUStartX: Real;      //X Start Position of the LineBonus Popup
    PUStartY: Real;      //Y Start Position of the LineBonus Popup
    PUTargetX: Real;     //X Target Position of the LineBonus Popup
    PUTargetY: Real;     //Y Target Position of the LineBonus Popup
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

      // Draws a Popup by Pointer
      procedure DrawPopUp(const PopUp: PScorePopUp);

      // Draws a Score by Playerindex
      procedure DrawScore(const Index: Integer);

      // Draws the RatingBar by Playerindex
      procedure DrawRatingBar(const Index: Integer);

      // Removes a PopUp w/o destroying the List
      procedure KillPopUp(const last, cur: PScorePopUp);
    public
      Settings: record //Record containing some Displaying Options
        Phase1Time: Real;     //time for Phase 1 to complete (in msecs)
                              //The Plop Up of the PopUp
        Phase2Time: Real;     //time for Phase 2 to complete (in msecs)
                              //The Moving (mainly Upwards) of the Popup
        Phase3Time: Real;     //time for Phase 3 to complete (in msecs)
                              //The Fade out and Score adding

        PopUpTex:  array [0..8] of TTexture; //Textures for every Popup Rating

        RatingBar_BG_Tex:   TTexture; //Rating Bar Texs
        RatingBar_FG_Tex:   TTexture;
        RatingBar_Bar_Tex:  TTexture;

      end;

      Visible: Boolean;    //Visibility of all Scores
      Enabled: Boolean;    //Scores are changed, PopUps are Moved etc.
      RBVisible: Boolean;  //Visibility of all Rating Bars

      //Propertys for Reading Position and Playercount
      property PositionCount: Byte read oPositionCount;
      property PlayerCount: Byte read oPlayerCount;
      property Players: aScorePlayer read aPlayers;

      //Constructor just sets some standard Settings
      constructor Create;

      // Adds a Position to Array and Increases Position Count
      procedure AddPosition(const pPosition: PScorePosition);

      // Adds a Player to Array and Increases Player Count
      procedure AddPlayer(const ScoreBG: TTexture; const Color: TRGB; const Score: Word = 0; const Enabled: Boolean = True; const Visible: Boolean = True);

      //Change a Players Visibility, Enable
      procedure ChangePlayerVisibility(const Index: Byte; const pVisible: Boolean);
      procedure ChangePlayerEnabled(const Index: Byte; const pEnabled: Boolean);

      // Deletes all Player Information
      procedure ClearPlayers;

      // Deletes Positions and Playerinformation
      procedure Clear;

      // Loads some Settings and the Positions from Theme
      procedure LoadfromTheme;

      // has to be called after Positions and Players have been added, before first call of Draw
      //It gives every Player a Score Position
      procedure Init;

      //Spawns a new Line Bonus PopUp for the Player
      procedure SpawnPopUp(const PlayerIndex: Byte; const Rating: Byte; const Score: Word);

      //Removes all PopUps from Mem
      procedure KillAllPopUps;

      // Draws Scores and Linebonus PopUps
      procedure Draw;
  end;


implementation

uses SDL,
     SysUtils,
     ULog,
     UGraphic,
     TextGL;

{**
 * Sets some standard Settings
 *}
Constructor TSingScores.Create;
begin
  inherited;

  //Clear PopupList Pointers
  FirstPopUp := nil;
  LastPopUp  := nil;

  //Clear Variables
  Visible := True;
  Enabled := True;
  RBVisible := True;
  
  //Clear Position Index
  oPositionCount  := 0;
  oPlayerCount    := 0;

  Settings.Phase1Time := 350;  // plop it up     . -> [   ]
  Settings.Phase2Time := 550;  // shift it up        ^[   ]^
  Settings.Phase3Time := 200;  // increase score      [s++]

  Settings.PopUpTex[0].TexNum := 0;
  Settings.PopUpTex[1].TexNum := 0;
  Settings.PopUpTex[2].TexNum := 0;
  Settings.PopUpTex[3].TexNum := 0;
  Settings.PopUpTex[4].TexNum := 0;
  Settings.PopUpTex[5].TexNum := 0;
  Settings.PopUpTex[6].TexNum := 0;
  Settings.PopUpTex[7].TexNum := 0;
  Settings.PopUpTex[8].TexNum := 0;

  Settings.RatingBar_BG_Tex.TexNum   := 0;
  Settings.RatingBar_FG_Tex.TexNum   := 0;
  Settings.RatingBar_Bar_Tex.TexNum  := 0;
end;

{**
 * Adds a Position to Array and Increases Position Count
 *}
Procedure TSingScores.AddPosition(const pPosition: PScorePosition);
begin
  if (PositionCount < MaxPositions) then
  begin
    Positions[PositionCount] := pPosition^;

    Inc(oPositionCount);
  end;
end;

{**
 * Adds a Player to Array and Increases Player Count
 *}
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
    aPlayers[PlayerCount].RBPos     := 0.5;
    aPlayers[PlayerCount].RBTarget  := 0.5;
    aPlayers[PlayerCount].RBVisible := True;

    Inc(oPlayerCount);
  end;
end;

{**
 * Change a Players Visibility
 *}
Procedure TSingScores.ChangePlayerVisibility(const Index: Byte; const pVisible: Boolean);
begin
  if (Index < MaxPlayers) then
    aPlayers[Index].Visible := pVisible;
end;

{**
 * Change Player Enabled
 *}
Procedure TSingScores.ChangePlayerEnabled(const Index: Byte; const pEnabled: Boolean);
begin
  if (Index < MaxPlayers) then
    aPlayers[Index].Enabled := pEnabled;
end;

{**
 * Procedure Deletes all Player Information
 *}
Procedure TSingScores.ClearPlayers;
begin
  KillAllPopUps;
  oPlayerCount := 0;
end;

{**
 * Procedure Deletes Positions and Playerinformation
 *}
Procedure TSingScores.Clear;
begin
  KillAllPopUps;
  oPlayerCount    := 0;
  oPositionCount  := 0;
end;

{**
 * Procedure Loads some Settings and the Positions from Theme
 *}
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
    nPosition.PUFontSize := 18;

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
  AddByStatics(4, Theme.Sing.StaticP3RScoreBG, Theme.Sing.StaticP3SingBar, Theme.Sing.TextP3RScore);
end;

{**
 * Spawns a new Line Bonus PopUp for the Player
 *}
Procedure TSingScores.SpawnPopUp(const PlayerIndex: Byte; const Rating: Byte; const Score: Word);
var Cur: PScorePopUp;
begin
  if (PlayerIndex < PlayerCount) then
  begin
    //Get Memory and Add Data
    GetMem(Cur, SizeOf(TScorePopUp));

    Cur.Player  := PlayerIndex;
    Cur.TimeStamp := SDL_GetTicks;

    //limit rating value to 8
    //a higher value would cause a crash when selecting the bg textur
    if (Rating > 8) then
      Cur.Rating := 8
    else
      Cur.Rating := Rating;

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

{**
 * Removes a PopUp w/o destroying the List
 *}
Procedure TSingScores.KillPopUp(const last, cur: PScorePopUp);
begin
  //Give Player the Last Points that missing till now
  aPlayers[Cur.Player].ScoreDisplayed := aPlayers[Cur.Player].ScoreDisplayed + Cur.ScoreDiff - Cur.ScoreGiven;

  //Change Bars Position
  if (Cur.ScoreDiff > 0) THEN
  begin //Popup w/ scorechange -> give missing Percentille
    aPlayers[Cur.Player].RBTarget := aPlayers[Cur.Player].RBTarget +
                                     (Cur.ScoreDiff - Cur.ScoreGiven) / Cur.ScoreDiff
                                     * (Cur.Rating / 20 - 0.26);
  end
  else
  begin //Popup w/o scorechange -> give complete Percentille
    aPlayers[Cur.Player].RBTarget := aPlayers[Cur.Player].RBTarget +
                                     (Cur.Rating / 20 - 0.26);
  end;

  If (aPlayers[Cur.Player].RBTarget > 1) then
    aPlayers[Cur.Player].RBTarget := 1
  else
  If (aPlayers[Cur.Player].RBTarget < 0) then
    aPlayers[Cur.Player].RBTarget := 0;

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

{**
 * Removes all PopUps from Mem
 *}
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

{**
 * Has to be called after Positions and Players have been added, before first call of Draw
 * It gives every Player a Score Position
 *}
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
  MaxPlayersPerScreen := 0;

  For I := 1 to 6 do
  begin
    //If there are enough Positions -> Write to MaxPlayers
    If (GetPositionCountbyPlayerCount(I) = I) then
      MaxPlayersPerScreen := I
    else
      Break;
  end;


  //Split Players to both Screen or Display on One Screen
  if (Screens = 2) and (MaxPlayersPerScreen < PlayerCount) then
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

{**
 * Draws Scores and Linebonus PopUps
 *}
Procedure TSingScores.Draw;
var
  I: Integer;
  CurTime: Cardinal;
  CurPopUp, LastPopUp: PScorePopUp;
begin
  CurTime := SDL_GetTicks;

  If Visible then
  begin
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


    IF (RBVisible) then
      //Draw Players w/ Rating Bar
      For I := 0 to PlayerCount-1 do
      begin
        DrawScore(I);
        DrawRatingBar(I);
      end
    else
      //Draw Players w/o Rating Bar
      For I := 0 to PlayerCount-1 do
      begin
        DrawScore(I);
      end;

  end; //eo Visible
end;

{**
 * Draws a Popup by Pointer
 *}
Procedure TSingScores.DrawPopUp(const PopUp: PScorePopUp);
var
  Progress: Real;
  CurTime:  Cardinal;
  X, Y, W, H, Alpha: Real;
  FontSize: integer;
  FontOffset: Real;
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
        If Not (Enabled AND Players[PopUp.Player].Enabled) then
        //Increase Timestamp with TIem where there is no Movement ...
        begin
          //Inc(PopUp.TimeStamp, LastRender);
        end;
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

          FontSize   := Round(Progress * Positions[PIndex].PUFontSize);
          FontOffset := (H  - FontSize) / 2;
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

          FontSize   := Positions[PIndex].PUFontSize;
          FontOffset := (H - FontSize) / 2;
          Alpha := 1 - 0.3 * Progress;
        end

        else
        begin
          //Phase 3 - The Fading out + Score adding
          Progress := (TimeDiff - Settings.Phase1Time - Settings.Phase2Time) / Settings.Phase3Time;

          If (PopUp.Rating > 0) then
          begin
            //Add Scores if Player Enabled
            If (Enabled AND Players[PopUp.Player].Enabled) then
            begin
              ScoreToAdd := Round(PopUp.ScoreDiff * Progress) - PopUp.ScoreGiven;
              Inc(PopUp.ScoreGiven, ScoreToAdd);
              aPlayers[PopUp.Player].ScoreDisplayed := Players[PopUp.Player].ScoreDisplayed + ScoreToAdd;

              //Change Bars Position
              aPlayers[PopUp.Player].RBTarget := aPlayers[PopUp.Player].RBTarget + ScoreToAdd/PopUp.ScoreDiff * (PopUp.Rating / 20 - 0.26);
              If (aPlayers[PopUp.Player].RBTarget > 1) then
                aPlayers[PopUp.Player].RBTarget := 1
              else If (aPlayers[PopUp.Player].RBTarget < 0) then
                aPlayers[PopUp.Player].RBTarget := 0;
            end;

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

            FontSize   := Positions[PIndex].PUFontSize;
            FontOffset := (H - FontSize) / 2;
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

        if (Alpha > 0) AND (Players[PopUp.Player].Visible) then
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
          TextLen := glTextWidth(Theme.Sing.LineBonusText[PopUp.Rating]);

          //Color and Pos
          SetFontPos (X + (W - TextLen) / 2, Y + FontOffset);
          glColor4f(1, 1, 1, Alpha);

          //Draw
          glPrint(Theme.Sing.LineBonusText[PopUp.Rating]);
        end; //eo Alpha check
      end; //eo Right Screen
    end; //eo Player has Position
  end
  else
    Log.LogError('TSingScores: Try to Draw a not existing PopUp');
end;

{**
 * Draws a Score by Playerindex
 *}
Procedure TSingScores.DrawScore(const Index: Integer);
var
  Position: PScorePosition;
  ScoreStr: String;
begin
  //Only Draw if Player has a Position
  If Players[Index].Position <> high(byte) then
  begin
    //Only Draw if Player is on Cur Screen
    If (((Players[Index].Position AND 128) = 0) = (ScreenAct = 1)) AND Players[Index].Visible then
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

      glPrint(ScoreStr);

    end; //eo Right Screen
  end; //eo Player has Position
end;


Procedure TSingScores.DrawRatingBar(const Index: Integer);
var
  Position: PScorePosition;
  R,G,B, Size: Real;
  Diff: Real;
begin
  //Only Draw if Player has a Position
  if Players[Index].Position <> high(byte) then
  begin
    //Only Draw if Player is on Cur Screen
    if (((Players[Index].Position and 128) = 0) = (ScreenAct = 1) and
        Players[index].RBVisible and
        Players[index].Visible) then
    begin
      Position := @Positions[Players[Index].Position and 127];

      if (Enabled AND Players[Index].Enabled) then
      begin
        //Move Position if Enabled
        Diff := Players[Index].RBTarget - Players[Index].RBPos;
        If(Abs(Diff) < 0.02) then
          aPlayers[Index].RBPos := aPlayers[Index].RBTarget
        else
          aPlayers[Index].RBPos := aPlayers[Index].RBPos + Diff*0.1;
      end;

      //Get Colors for RatingBar
      if (Players[index].RBPos <= 0.22) then
      begin
        R := 1;
        G := 0;
        B := 0;
      end
      else if (Players[index].RBPos <= 0.42) then
      begin
        R := 1;
        G := Players[index].RBPos*5;
        B := 0;
      end
      else if (Players[index].RBPos <= 0.57) then
      begin
        R := 1;
        G := 1;
        B := 0;
      end
      else if (Players[index].RBPos <= 0.77) then
      begin
        R := 1-(Players[index].RBPos-0.57)*5;
        G := 1;
        B := 0;
      end
      else
      begin
        R := 0;
        G := 1;
        B := 0;
      end;

      //Enable all glFuncs Needed
      glEnable(GL_TEXTURE_2D);
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

      //Draw RatingBar BG
      glColor4f(1, 1, 1, 0.8);
      glBindTexture(GL_TEXTURE_2D, Settings.RatingBar_BG_Tex.TexNum);

      glBegin(GL_QUADS);
        glTexCoord2f(0, 0);
        glVertex2f(Position.RBX, Position.RBY);

        glTexCoord2f(0, Settings.RatingBar_BG_Tex.TexH);
        glVertex2f(Position.RBX, Position.RBY+Position.RBH);

        glTexCoord2f(Settings.RatingBar_BG_Tex.TexW, Settings.RatingBar_BG_Tex.TexH);
        glVertex2f(Position.RBX+Position.RBW, Position.RBY+Position.RBH);

        glTexCoord2f(Settings.RatingBar_BG_Tex.TexW, 0);
        glVertex2f(Position.RBX+Position.RBW, Position.RBY);
      glEnd;

      //Draw Rating bar itself
      Size := Position.RBX + Position.RBW * Players[Index].RBPos;
      glColor4f(R, G, B, 1);
      glBindTexture(GL_TEXTURE_2D, Settings.RatingBar_Bar_Tex.TexNum);
      glBegin(GL_QUADS);
        glTexCoord2f(0, 0);
        glVertex2f(Position.RBX, Position.RBY);

        glTexCoord2f(0, Settings.RatingBar_Bar_Tex.TexH);
        glVertex2f(Position.RBX, Position.RBY + Position.RBH);

        glTexCoord2f(Settings.RatingBar_Bar_Tex.TexW, Settings.RatingBar_Bar_Tex.TexH);
        glVertex2f(Size, Position.RBY + Position.RBH);

        glTexCoord2f(Settings.RatingBar_Bar_Tex.TexW, 0);
        glVertex2f(Size, Position.RBY);
      glEnd;

      //Draw Ratingbar FG (Teh thing with the 3 lines to get better readability)
      glColor4f(1, 1, 1, 0.6);
      glBindTexture(GL_TEXTURE_2D, Settings.RatingBar_FG_Tex.TexNum);
      glBegin(GL_QUADS);
        glTexCoord2f(0, 0);
        glVertex2f(Position.RBX, Position.RBY);

        glTexCoord2f(0, Settings.RatingBar_FG_Tex.TexH);
        glVertex2f(Position.RBX, Position.RBY + Position.RBH);

        glTexCoord2f(Settings.RatingBar_FG_Tex.TexW, Settings.RatingBar_FG_Tex.TexH);
        glVertex2f(Position.RBX + Position.RBW, Position.RBY + Position.RBH);

        glTexCoord2f(Settings.RatingBar_FG_Tex.TexW, 0);
        glVertex2f(Position.RBX + Position.RBW, Position.RBY);
      glEnd;

      //Disable all Enabled glFuncs
      glDisable(GL_TEXTURE_2D);
      glDisable(GL_BLEND);
    end; //eo Right Screen
  end; //eo Player has Position
end;

end.
