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

uses
  UMenu,
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
  UPath,
  UThemes,
  UScreenSing,
  ModiSDK;

type
  TScreenSingModi = class(TScreenSing)
    protected
    
    public
      Winner: byte; //Who Wins
      PlayerInfo: TPlayerInfo;
      TeamInfo:   TTeamInfo;

      constructor Create; override;
      procedure OnShow; override;
      //procedure onShowFinish; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      function Draw: boolean; override;
      procedure Finish; override;
  end;

type
  TCustomSoundEntry = record
    Filename : IPath;
    Stream   : TAudioPlaybackStream;
  end;

var
  //Custom Sounds
  CustomSounds: array of TCustomSoundEntry;

//Procedured for Plugin
function LoadTex(const Name: PChar; Typ: TTextureType): TsmallTexture;
  {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
//function Translate (const Name: PChar): PChar;
//  {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
//Procedure to Print Text
procedure Print(const Style, Size: byte; const X, Y: real; const Text: PChar);
  {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
//Procedure that loads a Custom Sound
function LoadSound(const Name: PChar): cardinal;
  {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
//Plays a Custom Sound
procedure PlaySound(const Index: cardinal);
  {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

//Utilys
function ToSentences(Const Lines: TLines): TSentences;

implementation

uses
  Classes, 
  Math,
  UDLLManager,
  UDraw,
  UGraphic,
  UGraphicClasses,
  ULanguage,
  UNote,
  UPathUtils,
  URecord, 
  USkins;

// Method for input parsing. If false is returned, GetNextWindow
// should be checked to know the next window to load;
function TScreenSingModi.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
  if (PressedDown) then
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
  I, J: integer;
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
      //Result.Sentence[I].Note[J].Text      := Lines.Line[I].Note[J].Text;
      Result.Sentence[I].Note[J].FreeStyle := (Lines.Line[I].Note[J].NoteType = ntFreestyle);
    end;
  end;
end;

procedure TScreenSingModi.OnShow;
var
  I: integer;
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
    PlayerInfo.Playerinfo[I].Enabled := true;
  end;

  for I := PlayerInfo.NumPlayers to high(PlayerInfo.Playerinfo) do
  begin
    PlayerInfo.Playerinfo[I].Score:=  0;
    PlayerInfo.Playerinfo[I].Bar :=  0;
    PlayerInfo.Playerinfo[I].Enabled := false;
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
  if (DLLMan.Selected.LoadBack) and (DLLMan.Selected.LoadSong) then
    ScreenSing.Tex_Background := Tex_Background;

  Winner := 0;

  //Set Score Visibility
  Scores.Visible := DLLMan.Selected.ShowScore;

  {if PlayersPlay = 1 then
  begin
    Text[TextP1Score].Visible := DLLMan.Selected.ShowScore;
    Static[StaticP1ScoreBG].Visible := DLLMan.Selected.ShowScore;
  end;

  if (PlayersPlay = 2) or (PlayersPlay = 4) then
  begin
    Text[TextP1TwoPScore].Visible := DLLMan.Selected.ShowScore;
    Static[StaticP1TwoPScoreBG].Visible := DLLMan.Selected.ShowScore;

    Text[TextP2RScore].Visible := DLLMan.Selected.ShowScore;
    Static[StaticP2RScoreBG].Visible := DLLMan.Selected.ShowScore;
  end;

  if (PlayersPlay = 3) or (PlayersPlay = 6) then
  begin
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
  TextStr:  string;
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

  Background.Draw;

  // draw background picture (if any, and if no visualizations)
  // when we don't check for visualizations the visualizations would
  // be overdrawn by the picture when {UNDEFINED UseTexture} in UVisualizer
  if (DllMan.Selected.LoadSong) and (DllMan.Selected.LoadBack) and (not fShowVisualization) then
    SingDrawBackground;

  // set player names (for 2 screens and only Singstar skin)
  if ScreenAct = 1 then
  begin
    Text[TextP1].Text       := 'P1';
    Text[TextP1TwoP].Text   := 'P1'; // added for ps3 skin
    Text[TextP1ThreeP].Text := 'P1'; // added for ps3 skin
    Text[TextP2R].Text      := 'P2';
    Text[TextP2M].Text      := 'P2';
    Text[TextP3R].Text      := 'P3';
  end

  Else if ScreenAct = 2 then
  begin
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

  // update and draw movie
{  if ShowFinish and CurrentSong.VideoLoaded and DllMan.Selected.LoadVideo then
  begin
    UpdateSmpeg; // this only draws
  end;}

  // update and draw movie
  if (ShowFinish and (VideoLoaded or fShowVisualization) and DllMan.Selected.LoadVideo) then
  begin
    if assigned(fCurrentVideoPlaybackEngine) then
    begin
      // Just call this once
      // when Screens = 2
      if (ScreenAct = 1) then
        fCurrentVideoPlaybackEngine.GetFrame(CurrentSong.VideoGAP + LyricsState.GetCurrentTime());

      fCurrentVideoPlaybackEngine.DrawGL(ScreenAct);
    end;
  end;

  // draw static menu (FG)
  DrawFG;

  if ShowFinish then
  begin
    if DllMan.Selected.LoadSong then
    begin
      if (not AudioPlayback.Finished) and ((CurrentSong.Finish = 0) or (LyricsState.GetCurrentTime*1000 <= CurrentSong.Finish)) then
      begin
        //Pause Mod:
        if not Paused then
          Sing(Self);       // analyze song
      end
      else
      begin
        if not FadeOut then
        begin
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

  //Draw Score
  Scores.Draw;

  //Update PlayerInfo
  for I := 0 to PlayerInfo.NumPlayers-1 do
  begin
    if PlayerInfo.Playerinfo[I].Enabled then
    begin
      //PlayerInfo.Playerinfo[I].Bar :=  Player[I].ScorePercent;
      PlayerInfo.Playerinfo[I].Score := Player[I].ScoreTotalInt;
    end;
  end;

  if ((ShowFinish) and (not Paused)) then
  begin
    if not DLLMan.PluginDraw(Playerinfo, Lines[0].Current) then
    begin
      if not FadeOut then
      begin
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

function LoadTex(const Name: PChar; Typ: TTextureType): TsmallTexture;
var
  TexName: IPath;
  Ext: UTF8String;
  Tex: TTexture;
begin
  //Get texture Name
  TexName := Skin.GetTextureFileName(string(Name));
  //Get File Typ
  Ext := TexName.GetExtension().ToUTF8;
  if (UpperCase(Ext) = '.JPG') then
    Ext := 'JPG'
  else
    Ext := 'BMP';

  Tex := Texture.LoadTexture(false, TexName, UTexture.TTextureType(Typ), 0);

  Result.TexNum := Tex.TexNum;
  Result.W := Tex.W;
  Result.H := Tex.H;
end;
{
function Translate (const Name: PChar): PChar; stdcall;
begin
  Result := PChar(Language.Translate(string(Name)));
end; }

//Procedure to Print Text
procedure Print(const Style, Size: byte; const X, Y: real; const Text: PChar);
begin
  SetFontItalic ((Style and 128) = 128);
  SetFontStyle(Style and 7);
  // FIXME: FONTSIZE
  // used by Hold_The_Line / TeamDuell
  SetFontSize(Size);
  SetFontPos (X, Y);
  glPrint (Language.Translate(string(Text)));
end;

//Procedure that loads a Custom Sound
function LoadSound(const Name: PChar): cardinal;
var
  Stream: TAudioPlaybackStream;
  i: integer;
  Filename: IPath;
  SoundFile: IPath;
begin
  //Search for Sound in already loaded Sounds
  SoundFile := SoundPath.Append(Name);
  for i := 0 to High(CustomSounds) do
  begin
    if (SoundFile.Equals(CustomSounds[i].Filename, true)) then
    begin
      Result := i;
      Exit;
    end;
  end;

  Stream := AudioPlayback.OpenSound(SoundFile);
  if (Stream = nil) then
  begin
    Result := 0;
    Exit;
  end;

  SetLength(CustomSounds, Length(CustomSounds)+1);
  CustomSounds[High(CustomSounds)].Stream := Stream;
  Result := High(CustomSounds);
end;

//Plays a Custom Sound
procedure PlaySound(const Index: cardinal);
begin
  if (Index <= High(CustomSounds)) then
    AudioPlayback.PlaySound(CustomSounds[Index].Stream);
end;

end.

