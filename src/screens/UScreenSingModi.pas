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

unit UScreenSingModi;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}


uses UMenu,
     UMusic,
     SDL,
     SysUtils,
     UFiles,
     UTime,
     USongs,
     UIni,
     ULog,
     UTexture,
     ULyrics,
     TextGL,
     gl,

     UThemes,
     UScreenSing,
     ModiSDK;

type
  TScreenSingModi = class(TScreenSing)
    protected
    //paused: boolean; //Pause Mod
    //PauseTime: Real;
    //NumEmptySentences: integer;
    public
      //TextTime:           integer;

      //StaticP1:           integer;
      //StaticP1ScoreBG:    integer;
      //TextP1:             integer;
      //TextP1Score:        integer;

      //StaticP2R:          integer;
      //StaticP2RScoreBG:   integer;
      //TextP2R:            integer;
      //TextP2RScore:       integer;

      //StaticP2M:          integer;
      //StaticP2MScoreBG:   integer;
      //TextP2M:            integer;
      //TextP2MScore:       integer;

      //StaticP3R:          integer;
      //StaticP3RScoreBG:   integer;
      //TextP3R:            integer;
      //TextP3RScore:       integer;

      //Tex_Background:     TTexture;
      //FadeOut:            boolean;
      //LyricMain:          TLyric;
      //LyricSub:           TLyric;
      Winner: Byte; //Who Wins
      PlayerInfo: TPlayerInfo;
      TeamInfo:   TTeamInfo;

      constructor Create; override;
      procedure onShow; override;
      //procedure onShowFinish; override;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      function Draw: boolean; override;
      procedure Finish; override;
      //procedure Pause; //Pause Mod(Toggles Pause)
  end;

type
  TCustomSoundEntry = record
    Filename : String;
    Stream   : TAudioPlaybackStream;
  end;

var
  //Custom Sounds
  CustomSounds: array of TCustomSoundEntry;

//Procedured for Plugin
function LoadTex   (const Name: PChar; Typ: TTextureType): TsmallTexture; stdcall;
//function Translate (const Name: PChar): PChar; stdcall;
procedure Print (const Style, Size: Byte; const X, Y: Real; const Text: PChar); stdcall;       //Procedure to Print Text
function LoadSound  (const Name: PChar): Cardinal; stdcall;       //Procedure that loads a Custom Sound
procedure PlaySound (const Index: Cardinal); stdcall;       //Plays a Custom Sound

//Utilys
function ToSentences(Const Lines: TLines): TSentences;

implementation
uses UGraphic, UDraw, UMain, Classes, URecord, ULanguage, math, UDLLManager, USkins, UGraphicClasses;

// Method for input parsing. If False is returned, GetNextWindow
// should be checked to know the next window to load;
function TScreenSingModi.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    case PressedKey of

      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          Finish;
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenPartyScore);
        end;

      else
        Result := inherited ParseInput(PressedKey, CharCode, PressedDown);
    end;
  end;
end;

constructor TScreenSingModi.Create;
begin
  inherited Create;

end;

function ToSentences(Const Lines: TLines): TSentences;
var
  I, J: Integer;
begin
  Result.Current     := Lines.Current;
  Result.High        := Lines.High;
  Result.Number      := Lines.Number;
  Result.Resolution  := Lines.Resolution;
  Result.NotesGAP    := Lines.NotesGAP;
  Result.TotalLength := Lines.ScoreValue;

  SetLength(Result.Sentence, Length(Lines.Line));
  for I := low(Result.Sentence) to high(Result.Sentence) do
  begin
    Result.Sentence[I].Start      := Lines.Line[I].Start;
    Result.Sentence[I].StartNote  := Lines.Line[I].Note[0].Start;
    Result.Sentence[I].Lyric      := Lines.Line[I].Lyric;
    Result.Sentence[I].LyricWidth := Lines.Line[I].LyricWidth;
    Result.Sentence[I].End_       := Lines.Line[I].End_;
    Result.Sentence[I].BaseNote   := Lines.Line[I].BaseNote;
    Result.Sentence[I].HighNote   := Lines.Line[I].HighNote;
    Result.Sentence[I].TotalNotes := Lines.Line[I].TotalNotes;

    SetLength(Result.Sentence[I].Note, Length(Lines.Line[I].Note));
    for J := low(Result.Sentence[I].Note) to high(Result.Sentence[I].Note) do
    begin
      Result.Sentence[I].Note[J].Color     := Lines.Line[I].Note[J].Color;
      Result.Sentence[I].Note[J].Start     := Lines.Line[I].Note[J].Start;
      Result.Sentence[I].Note[J].Length    := Lines.Line[I].Note[J].Length;
      Result.Sentence[I].Note[J].Tone      := Lines.Line[I].Note[J].Tone;
      //Result.Sentence[I].Note[J].Text      := Lines.Line[I].Note[J].Tekst;
      Result.Sentence[I].Note[J].FreeStyle := (Lines.Line[I].Note[J].NoteType = ntFreestyle);
    end;
  end;
end;

procedure TScreenSingModi.onShow;
var
  I: Integer;
begin
  inherited;

  PlayersPlay := TeamInfo.NumTeams;

  if DLLMan.Selected.LoadSong then //Start with Song
  begin
    inherited;
  end
  else //Start Without Song
  begin
    AudioInput.CaptureStart;
  end;

//Set Playerinfo
  PlayerInfo.NumPlayers := PlayersPlay;
  for I := 0 to PlayerInfo.NumPlayers-1 do
  begin
    PlayerInfo.Playerinfo[I].Name    := PChar(Ini.Name[I]);
    PlayerInfo.Playerinfo[I].Score   := 0;
    PlayerInfo.Playerinfo[I].Bar     := 50;
    PlayerInfo.Playerinfo[I].Enabled := True;
  end;

  for I := PlayerInfo.NumPlayers to high(PlayerInfo.Playerinfo) do
  begin
    PlayerInfo.Playerinfo[I].Score:=  0;
    PlayerInfo.Playerinfo[I].Bar :=  0;
    PlayerInfo.Playerinfo[I].Enabled := False;
  end;

  {Case PlayersPlay of
    1: begin
      PlayerInfo.Playerinfo[0].PosX := Static[StaticP1ScoreBG].Texture.X;
      PlayerInfo.Playerinfo[0].PosY := Static[StaticP1ScoreBG].Texture.Y + Static[StaticP1ScoreBG].Texture.H;
    end;
    2,4: begin
      PlayerInfo.Playerinfo[0].PosX := Static[StaticP1TwoPScoreBG].Texture.X;
      PlayerInfo.Playerinfo[0].PosY := Static[StaticP1TwoPScoreBG].Texture.Y + Static[StaticP1TwoPScoreBG].Texture.H;
      PlayerInfo.Playerinfo[2].PosX := Static[StaticP1TwoPScoreBG].Texture.X;
      PlayerInfo.Playerinfo[2].PosY := Static[StaticP1TwoPScoreBG].Texture.Y + Static[StaticP1TwoPScoreBG].Texture.H;
      PlayerInfo.Playerinfo[1].PosX := Static[StaticP2RScoreBG].Texture.X;
      PlayerInfo.Playerinfo[1].PosY := Static[StaticP2RScoreBG].Texture.Y + Static[StaticP2RScoreBG].Texture.H;
      PlayerInfo.Playerinfo[3].PosX := Static[StaticP2RScoreBG].Texture.X;
      PlayerInfo.Playerinfo[3].PosY := Static[StaticP2RScoreBG].Texture.Y + Static[StaticP2RScoreBG].Texture.H;
    end;
    3,6: begin
      PlayerInfo.Playerinfo[0].PosX := Static[StaticP1ThreePScoreBG].Texture.X;
      PlayerInfo.Playerinfo[0].PosY := Static[StaticP1ThreePScoreBG].Texture.Y + Static[StaticP1ThreePScoreBG].Texture.H;
      PlayerInfo.Playerinfo[3].PosX := Static[StaticP1ThreePScoreBG].Texture.X;
      PlayerInfo.Playerinfo[3].PosY := Static[StaticP1ThreePScoreBG].Texture.Y + Static[StaticP1ThreePScoreBG].Texture.H;
      PlayerInfo.Playerinfo[1].PosX := Static[StaticP2MScoreBG].Texture.X;
      PlayerInfo.Playerinfo[1].PosY := Static[StaticP2MScoreBG].Texture.Y + Static[StaticP2MScoreBG].Texture.H;
      PlayerInfo.Playerinfo[4].PosX := Static[StaticP2MScoreBG].Texture.X;
      PlayerInfo.Playerinfo[4].PosY := Static[StaticP2MScoreBG].Texture.Y + Static[StaticP2MScoreBG].Texture.H;
      PlayerInfo.Playerinfo[2].PosX := Static[StaticP3RScoreBG].Texture.X;
      PlayerInfo.Playerinfo[2].PosY := Static[StaticP3RScoreBG].Texture.Y + Static[StaticP3RScoreBG].Texture.H;
      PlayerInfo.Playerinfo[5].PosX := Static[StaticP3RScoreBG].Texture.X;
      PlayerInfo.Playerinfo[5].PosY := Static[StaticP3RScoreBG].Texture.Y + Static[StaticP3RScoreBG].Texture.H;
    end;
  end;       }

  // play music (I)
  //Music.CaptureStart;
  //Music.MoveTo(AktSong.Start);

  //Init Plugin
  if not DLLMan.PluginInit(TeamInfo, PlayerInfo, ToSentences(Lines[0]), LoadTex, Print, LoadSound, PlaySound) then
  begin
    //Fehler
    Log.LogError('Could not Init Plugin');
    Halt;
  end;

  // Set Background (Little Workaround, maybe change sometime)
  if (DLLMan.Selected.LoadBack) AND (DLLMan.Selected.LoadSong) then
    ScreenSing.Tex_Background := Tex_Background;

  Winner := 0;

  //Set Score Visibility
  {if PlayersPlay = 1 then begin
    Text[TextP1Score].Visible := DLLMan.Selected.ShowScore;
    Static[StaticP1ScoreBG].Visible := DLLMan.Selected.ShowScore;
  end;

  if (PlayersPlay = 2) OR (PlayersPlay = 4) then begin
    Text[TextP1TwoPScore].Visible := DLLMan.Selected.ShowScore;
    Static[StaticP1TwoPScoreBG].Visible := DLLMan.Selected.ShowScore;

    Text[TextP2RScore].Visible := DLLMan.Selected.ShowScore;
    Static[StaticP2RScoreBG].Visible := DLLMan.Selected.ShowScore;
  end;

  if (PlayersPlay = 3) OR (PlayersPlay = 6) then begin
    Text[TextP1ThreePScore].Visible := DLLMan.Selected.ShowScore;
    Static[StaticP1ThreePScoreBG].Visible := DLLMan.Selected.ShowScore;

    Text[TextP2MScore].Visible := DLLMan.Selected.ShowScore;
    Static[StaticP2MScoreBG].Visible := DLLMan.Selected.ShowScore;

    Text[TextP3RScore].Visible := DLLMan.Selected.ShowScore;
    Static[StaticP3RScoreBG].Visible := DLLMan.Selected.ShowScore;
  end; }
end;

function TScreenSingModi.Draw: boolean;
var
  Min:    integer;
  Sec:    integer;
  Tekst:  string;
  S, I:   integer;
  T:      integer;
  CurLyricsTime: real;
begin
  Result := false;

  //Set Playerinfo
  PlayerInfo.NumPlayers := PlayersPlay;
  for I := 0 to PlayerInfo.NumPlayers-1 do
  begin
    PlayerInfo.Playerinfo[I].Name := PChar(Player[I].Name);
    if PlayerInfo.Playerinfo[I].Enabled then
    begin
      if (Player[I].ScoreTotalInt <= MAX_SONG_SCORE) then
        PlayerInfo.Playerinfo[I].Score:=  Player[I].ScoreTotalInt;
      PlayerInfo.Playerinfo[I].Bar :=  Round(Scores.Players[I].RBPos * 100);
    end;
  end;

  //Show Score
  if DLLMan.Selected.ShowScore then
  begin
    {//ScoreBG Mod
    // set player colors
    if PlayersPlay = 4 then begin
      if ScreenAct = 1 then begin
        LoadColor(Static[StaticP1TwoP].Texture.ColR, Static[StaticP1TwoP].Texture.ColG,
        Static[StaticP1TwoP].Texture.ColB, 'P1Dark');
        LoadColor(Static[StaticP2R].Texture.ColR, Static[StaticP2R].Texture.ColG,
        Static[StaticP2R].Texture.ColB, 'P2Dark');



        LoadColor(Static[StaticP1TwoPScoreBG].Texture.ColR, Static[StaticP1TwoPScoreBG].Texture.ColG,
        Static[StaticP1TwoPScoreBG].Texture.ColB, 'P1Dark');
        LoadColor(Static[StaticP2RScoreBG].Texture.ColR, Static[StaticP2RScoreBG].Texture.ColG,
        Static[StaticP2RScoreBG].Texture.ColB, 'P2Dark');



      end;
      if ScreenAct = 2 then begin
        LoadColor(Static[StaticP1TwoP].Texture.ColR, Static[StaticP1TwoP].Texture.ColG,
          Static[StaticP1TwoP].Texture.ColB, 'P3Dark');
        LoadColor(Static[StaticP2R].Texture.ColR, Static[StaticP2R].Texture.ColG,
          Static[StaticP2R].Texture.ColB, 'P4Dark');



        LoadColor(Static[StaticP1TwoPScoreBG].Texture.ColR, Static[StaticP1TwoPScoreBG].Texture.ColG,
          Static[StaticP1TwoPScoreBG].Texture.ColB, 'P3Dark');
        LoadColor(Static[StaticP2RScoreBG].Texture.ColR, Static[StaticP2RScoreBG].Texture.ColG,
          Static[StaticP2RScoreBG].Texture.ColB, 'P4Dark');



       end;
    end;

    if PlayersPlay = 6 then begin
      if ScreenAct = 1 then begin
        LoadColor(Static[StaticP1ThreeP].Texture.ColR, Static[StaticP1ThreeP].Texture.ColG,
          Static[StaticP1ThreeP].Texture.ColB, 'P1Dark');
        LoadColor(Static[StaticP2M].Texture.ColR, Static[StaticP2M].Texture.ColG,
          Static[StaticP2R].Texture.ColB, 'P2Dark');
        LoadColor(Static[StaticP3R].Texture.ColR, Static[StaticP3R].Texture.ColG,
          Static[StaticP3R].Texture.ColB, 'P3Dark');



        LoadColor(Static[StaticP1ThreePScoreBG].Texture.ColR, Static[StaticP1ThreePScoreBG].Texture.ColG,
          Static[StaticP1ThreePScoreBG].Texture.ColB, 'P1Dark');
        LoadColor(Static[StaticP2MScoreBG].Texture.ColR, Static[StaticP2MScoreBG].Texture.ColG,
          Static[StaticP2RScoreBG].Texture.ColB, 'P2Dark');
        LoadColor(Static[StaticP3RScoreBG].Texture.ColR, Static[StaticP3RScoreBG].Texture.ColG,
          Static[StaticP3RScoreBG].Texture.ColB, 'P3Dark');



      end;
      if ScreenAct = 2 then begin
        LoadColor(Static[StaticP1ThreeP].Texture.ColR, Static[StaticP1ThreeP].Texture.ColG,
          Static[StaticP1ThreeP].Texture.ColB, 'P4Dark');
        LoadColor(Static[StaticP2M].Texture.ColR, Static[StaticP2M].Texture.ColG,
          Static[StaticP2R].Texture.ColB, 'P5Dark');
        LoadColor(Static[StaticP3R].Texture.ColR, Static[StaticP3R].Texture.ColG,
          Static[StaticP3R].Texture.ColB, 'P6Dark');




        LoadColor(Static[StaticP1ThreePScoreBG].Texture.ColR, Static[StaticP1ThreePScoreBG].Texture.ColG,
          Static[StaticP1ThreePScoreBG].Texture.ColB, 'P4Dark');
        LoadColor(Static[StaticP2MScoreBG].Texture.ColR, Static[StaticP2MScoreBG].Texture.ColG,
          Static[StaticP2RScoreBG].Texture.ColB, 'P5Dark');
        LoadColor(Static[StaticP3RScoreBG].Texture.ColR, Static[StaticP3RScoreBG].Texture.ColG,
          Static[StaticP3RScoreBG].Texture.ColB, 'P6Dark');




      end;
    end;
    //end ScoreBG Mod }

    // set player names (for 2 screens and only Singstar skin)
    if ScreenAct = 1 then begin
      Text[TextP1].Text       := 'P1';
      Text[TextP1TwoP].Text   := 'P1'; // added for ps3 skin
      Text[TextP1ThreeP].Text := 'P1'; // added for ps3 skin
      Text[TextP2R].Text      := 'P2';
      Text[TextP2M].Text      := 'P2';
      Text[TextP3R].Text      := 'P3';
    end;

    if ScreenAct = 2 then begin
      case PlayersPlay of
        4:  begin
              Text[TextP1TwoP].Text := 'P3';
              Text[TextP2R].Text := 'P4';
            end;
        6:  begin
              Text[TextP1ThreeP].Text := 'P4';
              Text[TextP2M].Text := 'P5';
              Text[TextP3R].Text := 'P6';
            end;
      end; // case
    end; // if


    // stereo   <- and where iss P2M? or P3?
    Static[StaticP1].Texture.X := Static[StaticP1].Texture.X + 10*ScreenX;
    Text[TextP1].X := Text[TextP1].X + 10*ScreenX;

    {Static[StaticP1ScoreBG].Texture.X := Static[StaticP1ScoreBG].Texture.X + 10*ScreenX;
    Text[TextP1Score].X := Text[TextP1Score].X + 10*ScreenX;}

    Static[StaticP2R].Texture.X := Static[StaticP2R].Texture.X + 10*ScreenX;
    Text[TextP2R].X := Text[TextP2R].X + 10*ScreenX;

    {Static[StaticP2RScoreBG].Texture.X := Static[StaticP2RScoreBG].Texture.X + 10*ScreenX;
    Text[TextP2RScore].X := Text[TextP2RScore].X + 10*ScreenX;}

    // .. and scores
    {if PlayersPlay = 1 then begin
      Tekst := IntToStr(Player[0].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP1Score].Text := Tekst;
    end;

    if PlayersPlay = 2 then begin
      Tekst := IntToStr(Player[0].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP1TwoPScore].Text := Tekst;

      Tekst := IntToStr(Player[1].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP2RScore].Text := Tekst;
    end;

    if PlayersPlay = 3 then begin
      Tekst := IntToStr(Player[0].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP1ThreePScore].Text := Tekst;

      Tekst := IntToStr(Player[1].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP2MScore].Text := Tekst;

      Tekst := IntToStr(Player[2].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP3RScore].Text := Tekst;
    end;

    if PlayersPlay = 4 then begin
      if ScreenAct = 1 then begin
        Tekst := IntToStr(Player[0].ScoreTotalI);
        while Length(Tekst) < 5 do Tekst := '0' + Tekst;
        Text[TextP1TwoPScore].Text := Tekst;

        Tekst := IntToStr(Player[1].ScoreTotalI);
        while Length(Tekst) < 5 do Tekst := '0' + Tekst;
        Text[TextP2RScore].Text := Tekst;
      end;
      if ScreenAct = 2 then begin
        Tekst := IntToStr(Player[2].ScoreTotalI);
        while Length(Tekst) < 5 do Tekst := '0' + Tekst;
        Text[TextP1TwoPScore].Text := Tekst;

        Tekst := IntToStr(Player[3].ScoreTotalI);
        while Length(Tekst) < 5 do Tekst := '0' + Tekst;
        Text[TextP2RScore].Text := Tekst;
      end;
    end;

    if PlayersPlay = 6 then begin
      if ScreenAct = 1 then begin
        Tekst := IntToStr(Player[0].ScoreTotalI);
        while Length(Tekst) < 5 do Tekst := '0' + Tekst;
        Text[TextP1ThreePScore].Text := Tekst;

        Tekst := IntToStr(Player[1].ScoreTotalI);
        while Length(Tekst) < 5 do Tekst := '0' + Tekst;
        Text[TextP2MScore].Text := Tekst;

        Tekst := IntToStr(Player[2].ScoreTotalI);
        while Length(Tekst) < 5 do Tekst := '0' + Tekst;
        Text[TextP3RScore].Text := Tekst;
      end;
      if ScreenAct = 2 then begin
        Tekst := IntToStr(Player[3].ScoreTotalI);
        while Length(Tekst) < 5 do Tekst := '0' + Tekst;
        Text[TextP1ThreePScore].Text := Tekst;

        Tekst := IntToStr(Player[4].ScoreTotalI);
        while Length(Tekst) < 5 do Tekst := '0' + Tekst;
        Text[TextP2MScore].Text := Tekst;

        Tekst := IntToStr(Player[5].ScoreTotalI);
        while Length(Tekst) < 5 do Tekst := '0' + Tekst;
        Text[TextP3RScore].Text := Tekst;
      end;
    end;   }

  end; //ShowScore

  for S := 1 to 1 do
    Static[S].Texture.X := Static[S].Texture.X + 10*ScreenX;

  for T := 0 to 1 do
    Text[T].X := Text[T].X + 10*ScreenX;

  if DLLMan.Selected.LoadSong then
  begin
    // update static menu with time ...
    CurLyricsTime := LyricsState.GetCurrentTime();
    Min := Round(CurLyricsTime) div 60;
    Sec := Round(CurLyricsTime) mod 60;

    Text[TextTimeText].Text := '';
    if Min < 10 then Text[TextTimeText].Text := '0';
    Text[TextTimeText].Text := Text[TextTimeText].Text + IntToStr(Min) + ':';
    if Sec < 10 then Text[TextTimeText].Text := Text[TextTimeText].Text + '0';
    Text[TextTimeText].Text := Text[TextTimeText].Text + IntToStr(Sec);
  end;

  // draw static menu (BG)
  DrawBG;

  //Draw Background
  if (DllMan.Selected.LoadSong) AND (DllMan.Selected.LoadBack) then
    SingDrawBackground;

  // comment by blindy: wo zum henker wird denn in diesem screen ein video abgespielt?
  // update and draw movie
  // <mog> wie wo wadd? also in der selben funktion in der uscreensing kommt des video in der zeile 995, oder was wollteste wissen? :X
{  if ShowFinish and CurrentSong.VideoLoaded AND DllMan.Selected.LoadVideo then begin
    UpdateSmpeg; // this only draws
  end;}

  // draw static menu (FG)
  DrawFG;

  if ShowFinish then begin
    if DllMan.Selected.LoadSong then
    begin
      if (not AudioPlayback.Finished) and ((CurrentSong.Finish = 0) or (LyricsState.GetCurrentTime*1000 <= CurrentSong.Finish)) then begin
        //Pause Mod:
        if not Paused then
          Sing(Self);       // analyze song
      end else begin
        if not FadeOut then begin
          Finish;
          FadeOut := true;
          FadeTo(@ScreenPartyScore);
        end;
      end;
    end;
  end;

  // draw custom items
  SingModiDraw(PlayerInfo);  // always draw

  //GoldenNoteStarsTwinkle Mod
    GoldenRec.SpawnRec;
  //GoldenNoteStarsTwinkle Mod

  //Update PlayerInfo
  for I := 0 to PlayerInfo.NumPlayers-1 do
  begin
    if PlayerInfo.Playerinfo[I].Enabled then
    begin
      //PlayerInfo.Playerinfo[I].Bar :=  Player[I].ScorePercent;
      PlayerInfo.Playerinfo[I].Score := Player[I].ScoreTotalInt;
    end;
  end;

  if ((ShowFinish) AND (NOT Paused)) then
  begin
    if not DLLMan.PluginDraw(Playerinfo, Lines[0].Current) then
    begin
      if not FadeOut then begin
          Finish;
          FadeOut := true;
          FadeTo(@ScreenPartyScore);
      end;
    end;   
  end;

  //Change PlayerInfo/Changeables
  for I := 0 to PlayerInfo.NumPlayers-1 do
  begin
    if (Player[I].ScoreTotalInt <> PlayerInfo.Playerinfo[I].Score) then
    begin
      //Player[I].ScoreTotal   := Player[I].ScoreTotal + (PlayerInfo.Playerinfo[I].Score - Player[I].ScoreTotalI);
      Player[I].ScoreTotalInt := PlayerInfo.Playerinfo[I].Score;
    end;
    {if (PlayerInfo.Playerinfo[I].Bar <> Player[I].ScorePercent) then
      Player[I].ScorePercentTarget := PlayerInfo.Playerinfo[I].Bar; }
  end;

  // back stereo
  Static[StaticP1].Texture.X := Static[StaticP1].Texture.X - 10*ScreenX;
  Text[TextP1].X := Text[TextP1].X - 10*ScreenX;

  {Static[StaticP1ScoreBG].Texture.X := Static[StaticP1ScoreBG].Texture.X - 10*ScreenX;
  Text[TextP1Score].X := Text[TextP1Score].X - 10*ScreenX;}


  Static[StaticP2R].Texture.X := Static[StaticP2R].Texture.X - 10*ScreenX;
  Text[TextP2R].X := Text[TextP2R].X - 10*ScreenX;

  {Static[StaticP2RScoreBG].Texture.X := Static[StaticP2RScoreBG].Texture.X - 10*ScreenX;
  Text[TextP2RScore].X := Text[TextP2RScore].X - 10*ScreenX;}


  for S := 1 to 1 do
    Static[S].Texture.X := Static[S].Texture.X - 10*ScreenX;

  for T := 0 to 1 do
    Text[T].X := Text[T].X - 10*ScreenX;

  Result := true;
end;

procedure TScreenSingModi.Finish;
begin
inherited Finish;

Winner := DllMan.PluginFinish(PlayerInfo);

//Log.LogError('Winner: ' + InttoStr(Winner));

//DLLMan.UnLoadPlugin;
end;

function LoadTex (const Name: PChar; Typ: TTextureType): TsmallTexture; stdcall;
var
  Texname, EXT: String;
  Tex: TTexture;
begin
  //Get texture Name
  TexName := Skin.GetTextureFileName(String(Name));
  //Get File Typ
  Ext := ExtractFileExt(TexName);
  if (uppercase(Ext) = '.JPG') then
    Ext := 'JPG'
  else
    Ext := 'BMP';

  Tex := Texture.LoadTexture(false, PChar(TexName), UTEXTURE.TTextureType(Typ), 0);

  Result.TexNum := Tex.TexNum;
  Result.W := Tex.W;
  Result.H := Tex.H;
end;
{
function Translate (const Name: PChar): PChar; stdcall;
begin
  Result := PChar(Language.Translate(String(Name)));
end; }

procedure Print(const Style, Size: Byte; const X, Y: Real; const Text: PChar); stdcall;       //Procedure to Print Text
begin
  SetFontItalic ((Style and 128) = 128);
  SetFontStyle(Style and 7);
  // FIXME: FONTSIZE
  // used by Hold_The_Line / TeamDuell
  SetFontSize(Size * 3);
  SetFontPos (X, Y);
  glPrint (Language.Translate(String(Text)));
end;

function LoadSound(const Name: PChar): Cardinal; stdcall;       //Procedure that loads a Custom Sound
var
  Stream: TAudioPlaybackStream;
  i: Integer;
  Filename: String;
begin
  //Search for Sound in already loaded Sounds
  Filename := UpperCase(SoundPath + Name);
  for i := 0 to High(CustomSounds) do
  begin
    if (UpperCase(CustomSounds[i].Filename) = Filename) then
    begin
      Result := i;
      Exit;
    end;
  end;

  Stream := AudioPlayback.OpenSound(SoundPath + String(Name));
  if (Stream = nil) then
  begin
    Result := 0;
    Exit;
  end;

  SetLength(CustomSounds, Length(CustomSounds)+1);
  CustomSounds[High(CustomSounds)].Stream := Stream;
  Result := High(CustomSounds);
end;

procedure PlaySound(const Index: Cardinal); stdcall;       //Plays a Custom Sound
begin
  if (Index <= High(CustomSounds)) then
    AudioPlayback.PlaySound(CustomSounds[Index].Stream);
end;

end.

