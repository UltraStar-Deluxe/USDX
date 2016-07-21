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
 *}



unit UScreenSingController;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  sdl2,
  dglOpenGL,
  TextGL,
  UAvatars,
  UCommon,
  UFiles,
  UGraphicClasses,
  UHookableEvent,
  UScreenSingView,
  UIni,
  ULog,
  ULyrics,
  UMenu,
  UMusic,
  UPath,
  USingScores,
  USongs,
  UTexture,
  UThemes,
  UTime,
  USkins;

type
  TPos = record // Lines[part].Line[line].Note[note]
    part: integer;
    line: integer;
    note: integer;
    CP: integer;
  end;

  TLyricsSyncSource = class(TSyncSource)
    function GetClock(): real; override;
  end;

  TMusicSyncSource = class(TSyncSource)
    function GetClock(): real; override;
  end;

  TTimebarMode = (
    tbmCurrent,   // current song position
    tbmRemaining, // remaining time
    tbmTotal      // total time
  );

type
  TScreenSingController = class(TMenu)
  private

    StartNote, EndNote:     TPos;

    procedure LoadNextSong();

    procedure SongError();
  public
    eSongLoaded: THookableEvent; //< event is called after lyrics of a song are loaded on OnShow
    Paused:     boolean; //pause Mod
    NumEmptySentences: array [0..1] of integer;
    // views
    fShowVisualization: boolean;
    fShowBackground: boolean;

    fCurrentVideo: IVideo;
    fVideoClip:    IVideo;
    fLyricsSync: TLyricsSyncSource;
    fMusicSync: TMusicSyncSource;
    fTimebarMode: TTimebarMode;

    PlayMidi: boolean;

    removeVoice: boolean;
    fShowWebcam: boolean;

    Act_Level: integer;
    Act_MD5Song: string;

    MedleyStart, MedleyEnd: real;

    Lyrics: TLyricEngine;
    LyricsDuetP1: TLyricEngine;
    LyricsDuetP2: TLyricEngine;

    // score manager:
    Scores: TSingScores;

    //the song was sung to the end
    SungToEnd: boolean;

    //use pause
    SungPaused: boolean;

    // some settings to be set by plugins
    Settings: record
      Finish: Boolean; //< if true, screen will finish on next draw

      LyricsVisible: Boolean; //< shows or hides lyrics
      NotesVisible: Integer; //< if bit[playernum] is set the notes for the specified player are visible. By default all players notes are visible

      PlayerEnabled: Integer; //< defines whether a player can score atm

      SoundEnabled: Boolean; //< mute or unmute sound
    end;

    // MIDI
    ChannelOff  : integer;

    MidiFadeIn: boolean;
    MidiFadeOut: boolean;
    FadeTime: cardinal;

    InfoMessageBG: cardinal;
    InfoMessageText: cardinal;

    MessageTime: cardinal;
    MessageTimeFade: cardinal;

    TextMedleyFadeOut: boolean;
    TextMedleyFadeTime: cardinal;

    // names
    PlayerNames: array [1..IMaxPlayerCount] of UTF8String;
    PlayerDuetNames:array [1..IMaxPlayerCount] of UTF8String;

    Tex_Background: TTexture;
    FadeOut: boolean;

    procedure ClearSettings;
    procedure ApplySettings; //< applies changes of settings record
    procedure EndSong;

    constructor Create; override;
    procedure OnShow; override;
    procedure OnShowFinish; override;
    procedure OnHide; override;
    function Draw: boolean; override;

    function ParseInput(PressedKey: cardinal; CharCode: UCS4Char;
      PressedDown: boolean): boolean; override;

    function FinishedMusic: boolean;

    procedure AutoSendScore;
    procedure AutoSaveScore;

    procedure Finish; virtual;
    procedure Pause; // toggle pause
    procedure UpdateMedleyStats(medley_end: boolean);
    procedure OnSentenceEnd(CP: integer; SentenceIndex: cardinal);     // for linebonus + singbar
    procedure OnSentenceChange(CP: integer; SentenceIndex: cardinal);  // for golden notes
  end;

var screenSingViewRef: TScreenSingView;
    TotalTime:              real;

implementation

uses
  Classes,
  Math,
  UDatabase,
  UDllManager,
  UDraw,
  UGraphic,
  ULanguage,
  UNote,
  URecord,
  USong,
  UDisplay,
  UParty,
  UPathUtils,
  UUnicodeUtils,
  UWebcam,
  UWebSDK;

const
  MAX_MESSAGE = 3;

// method for input parsing. if false is returned, getnextwindow
// should be checked to know the next window to load;

function TScreenSingController.ParseInput(PressedKey: Cardinal; CharCode: UCS4Char;
  PressedDown: boolean): boolean;
var
  SDL_ModState:  word;
  i1: integer;
  Color:      TRGB;
begin
  Result := true;
  if (PressedDown) then
  begin // key down

    SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);


    // check normal keys
    case UCS4UpperCase(CharCode) of
      Ord('Q'):
      begin
        // when not ask before exit then finish now
        if (Ini.AskbeforeDel <> 1) then
          Finish
        // else just pause and let the popup make the work
        else if not Paused then
          Pause;

        Result := false;
        Exit;
      end;

      //Restart and pause song
      Ord('R'):
      begin
        if ScreenSong.Mode = smMedley then Exit;
        for i1 := 0 to High(Player) do
        with Player[i1] do
        begin
          Score          := 0;
          ScoreLine      := 0;
          ScoreGolden    := 0;

          ScoreInt       := 0;
          ScoreLineInt   := 0;
          ScoreGoldenInt := 0;
          ScoreTotalInt  := 0;

          ScoreLast      := 0;

          LastSentencePerfect := false;
        end;
        AudioPlayback.SetPosition(CurrentSong.Start);
        if (Assigned(fCurrentVideo)) then
           fCurrentVideo.Position := CurrentSong.VideoGAP + CurrentSong.Start;// + (CurrentSong.gap / 1000.0 - 5.0);
        Scores.KillAllPopUps;
        // setup score manager
        Scores.ClearPlayers; // clear old player values
        Color.R := 0;
        Color.G := 0;
        Color.B := 0;

        // add new players
        for i1 := 0 to PlayersPlay - 1 do
        begin
          Scores.AddPlayer(Tex_ScoreBG[i1], Color);
        end;
        LyricsState.SetCurrentTime(CurrentSong.Start);
        Scores.Init;
        Exit;
      end;

      // show visualization
      Ord('V'):
      begin
        if fShowWebcam then
        begin
          Webcam.Release;
          fShowWebCam:=false;
        end;
        if ((fShowBackground = true) and (Ini.VideoEnabled = 1) and CurrentSong.Video.IsSet())
                             or (fShowVisualization and not CurrentSong.Background.IsSet()) then //switch to video
        begin
          Log.LogStatus('decided to switch to video', 'UScreenSing.ParseInput');
          fShowBackground := false;
          fCurrentVideo := nil;
          fShowVisualization := false;
          fCurrentVideo := fVideoClip;
          if (Assigned(fCurrentVideo)) then
               fCurrentVideo.Position := CurrentSong.VideoGAP + CurrentSong.Start + AudioPlayback.Position;
          Log.LogStatus('finished switching to video', 'UScreenSing.ParseInput');
        end
        else
        begin
          if fShowVisualization and CurrentSong.Background.IsSet() then
          begin //switch to Background only
            Log.LogStatus('decided to switch to background', 'UScreenSing.ParseInput');
            fShowBackground := true;
            fCurrentVideo := nil;
            fShowVisualization := false;
            Log.LogStatus('finished switching to background', 'UScreenSing.ParseInput');
          end
          else
          begin //Video is currently visible, change to visualization
            Log.LogStatus('decided to switch to visualization', 'UScreenSing.ParseInput');
            fShowVisualization := true;
            fCurrentVideo := Visualization.Open(PATH_NONE);
            fCurrentVideo.play;
            Log.LogStatus('finished switching to visualization', 'UScreenSing.ParseInput');
          end;
        end;
        Exit;
      end;

      // show Webcam
      Ord('W'):
      begin
        if (fShowWebCam = false) then
        begin
          fCurrentVideo := nil;
          fShowVisualization := false;
          fShowBackground := false;
          Webcam.Restart;
          if (Webcam.Capture = nil) then
          begin
            fShowWebCam := false;
            fShowBackground := true;
            ScreenPopupError.ShowPopup(Language.Translate('SING_OPTIONS_WEBCAM_NO_WEBCAM'))
          end
          else
            fShowWebCam := true;
        //  ChangeEffectLastTick := SDL_GetTicks;
        //  SelectsS[WebcamParamsSlide].Visible := true;
        //  LastTickFrame := SDL_GetTicks;
        end
        else
        begin
          Webcam.Release;
          fShowWebCam:=false;
        end;

        Exit;
      end;

      // pause
      Ord('P'):
      begin
        Pause;
        Exit;
      end;

      // toggle time display
      Ord('T'):
      begin
        if (fTimebarMode = High(TTimebarMode)) then
          fTimebarMode := Low(TTimebarMode)
        else
          Inc(fTimebarMode);

        Ini.SingTimebarMode := Ord(fTimebarMode);
        Ini.SaveSingTimebarMode;
        Exit;
      end;

      // skip intro
      Ord('S'):
      begin
        if (AudioPlayback.Position < CurrentSong.gap / 1000 - 6) then
        begin
          AudioPlayback.SetPosition(CurrentSong.gap / 1000.0 - 5.0);
            if (Assigned(fCurrentVideo)) then
               fCurrentVideo.Position := CurrentSong.VideoGAP + CurrentSong.Start + (CurrentSong.gap / 1000.0 - 5.0);
        end;
        Exit;
      end;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE:
      begin
        // record sound hack:
        //Sound[0].BufferLong
        if ScreenSong.Mode = smMedley then
          PlaylistMedley.NumMedleySongs := PlaylistMedley.CurrentMedleySong;

        Finish;
        FadeOut := true;
        AudioPlayback.PlaySound(SoundLib.Back);
      end;

      SDLK_SPACE:
      begin
        Pause;
      end;

      SDLK_TAB: // change visualization preset
      begin
        if fShowVisualization then
          fCurrentVideo.Position := now; // move to a random position

        if (fShowWebcam) then
        begin
          if (Ini.WebCamEffect < 10) then
            Ini.WebCamEffect := Ini.WebCamEffect + 1
          else
            Ini.WebCamEffect := 0;
        end;
      end;
    end;
  end;
end;

procedure TScreenSingController.Pause;
begin
  if (not Paused) then  // enable pause
  begin
    // pause time
    Paused := true;

    LyricsState.Pause();

    // pause music
      AudioPlayback.Pause;

    // pause video
    if (fCurrentVideo <> nil) then
      fCurrentVideo.Pause;

  end
  else              // disable pause
  begin
    LyricsState.Start();

    // play music
      AudioPlayback.Play;

    // video
    if (fCurrentVideo <> nil) then
      fCurrentVideo.Pause;

    Paused := false;
  end;
end;

constructor TScreenSingController.Create;
var
  Col: array [1..6] of TRGB;
  I: integer;
  Color: cardinal;
begin
  inherited Create;
  ScreenSing := self;
  screenSingViewRef := TScreenSingView.Create();

  ClearSettings;
end;

procedure TScreenSingController.OnShow;
var
  V1:     boolean;
  V1TwoP: boolean;   // position of score box in two player mode
  V1ThreeP: boolean; // position of score box in three player mode
  V2R:    boolean;
  V2M:    boolean;
  V3R:    boolean;
  VDuet1ThreeP: boolean;
  VDuet2M:    boolean;
  VDuet3R:    boolean;
  V1FourP: boolean;
  V2FourP: boolean;
  V3FourP: boolean;
  V4FourP: boolean;
  V1SixP: boolean;
  V2SixP: boolean;
  V3SixP: boolean;
  V4SixP: boolean;
  V5SixP: boolean;
  V6SixP: boolean;
  V1DuetFourP: boolean;
  V2DuetFourP: boolean;
  V3DuetFourP: boolean;
  V4DuetFourP: boolean;
  V1DuetSixP: boolean;
  V2DuetSixP: boolean;
  V3DuetSixP: boolean;
  V4DuetSixP: boolean;
  V5DuetSixP: boolean;
  V6DuetSixP: boolean;
  BadPlayer: integer;
  Col, ColP1, ColP2: TRGB;
  I: integer;
begin
  inherited;

  Log.LogStatus('Begin', 'OnShow');

  FadeOut := false;

  screenSingViewRef.CloseMessage();

  //the song was sung to the end
  SungToEnd := false;
  SungPaused := false;

  ClearSettings;
  Party.CallBeforeSing;

  // prepare players
  SetLength(Player, PlayersPlay);

  //Reset Player Medley stats
  if (ScreenSong.Mode = smMedley) then
  begin
    PlaylistMedley.CurrentMedleySong:=1;

    PlaylistMedley.NumPlayer := PlayersPlay;
    SetLength(PlaylistMedley.Stats, 0);
  end;

  fTimebarMode := TTimebarMode(Ini.SingTimebarMode);

  if (ScreenSong.Mode = smMedley) then
    CatSongs.Selected := PlaylistMedley.Song[PlaylistMedley.CurrentMedleySong-1];

  CurrentSong := CatSongs.Song[CatSongs.Selected];

  for I := 0 to High(screenSingViewRef.StaticDuet) do
    Statics[screenSingViewRef.StaticDuet[I]].Visible := CurrentSong.isDuet and (PlayersPlay > 1);

  Statics[screenSingViewRef.SongNameStatic].Visible := false;
  Text[screenSingViewRef.SongNameText].Visible := false;

  V1     := false;
  V1TwoP := false;
  V1ThreeP := false;
  V2R    := false;
  V2M    := false;
  V3R    := false;

  VDuet1ThreeP := false;
  VDuet2M := false;
  VDuet3R := false;

  V1FourP := false;
  V2FourP := false;
  V3FourP := false;
  V4FourP := false;

  V1SixP := false;
  V2SixP := false;
  V3SixP := false;
  V4SixP := false;
  V5SixP := false;
  V6SixP := false;

  V1DuetFourP := false;
  V2DuetFourP := false;
  V3DuetFourP := false;
  V4DuetFourP := false;

  V1DuetSixP := false;
  V2DuetSixP := false;
  V3DuetSixP := false;
  V4DuetSixP := false;
  V5DuetSixP := false;
  V6DuetSixP := false;

  case PlayersPlay of
    1:
    begin
      V1     := true;
    end;
    2:
    begin
      V1TwoP := true;
      V2R    := true;
    end;
    3:
    begin
      if (CurrentSong.isDuet) then
      begin
        VDuet1ThreeP := true;
        VDuet2M := true;
        VDuet3R := true;
      end
      else
      begin
        V1ThreeP := true;
        V2M    := true;
        V3R    := true;
      end;
    end;
    4:
    begin // double screen
      if (Ini.Screens = 1) then
      begin
        V1TwoP := true;
        V2R    := true;
      end
      else
      begin
        if (CurrentSong.isDuet) then
        begin
          V1DuetFourP := true;
          V2DuetFourP := true;
          V3DuetFourP := true;
          V4DuetFourP := true;
        end
        else
        begin
          V1FourP := true;
          V2FourP := true;
          V3FourP := true;
          V4FourP := true;
        end;
      end;
    end;
    6:
    begin // double screen
      if (Ini.Screens = 1) then
      begin
        if (CurrentSong.isDuet) then
        begin
          VDuet1ThreeP := true;
          VDuet2M := true;
          VDuet3R := true;
        end
        else
        begin
          V1ThreeP := true;
          V2M    := true;
          V3R    := true;
        end;
      end
      else
      begin
       if (CurrentSong.isDuet) then
        begin
          V1DuetSixP := true;
          V2DuetSixP := true;
          V3DuetSixP := true;
          V4DuetSixP := true;
          V5DuetSixP := true;
          V6DuetSixP := true;
        end
        else
        begin
          V1SixP := true;
          V2SixP := true;
          V3SixP := true;
          V4SixP := true;
          V5SixP := true;
          V6SixP := true;
        end;
      end;
    end;
  end;

  Text[screenSingViewRef.TextP1].Visible       := V1;
  Text[screenSingViewRef.TextP1TwoP].Visible   := V1TwoP;
  Text[screenSingViewRef.TextP2R].Visible      := V2R;
  Text[screenSingViewRef.TextP1ThreeP].Visible := V1ThreeP;
  Text[screenSingViewRef.TextP2M].Visible      := V2M;
  Text[screenSingViewRef.TextP3R].Visible      := V3R;
  Text[screenSingViewRef.TextDuetP1ThreeP].Visible := VDuet1ThreeP;
  Text[screenSingViewRef.TextDuetP2M].Visible      := VDuet2M;
  Text[screenSingViewRef.TextDuetP3R].Visible      := VDuet3R;
  Text[screenSingViewRef.TextP1FourP].Visible   := V1FourP;
  Text[screenSingViewRef.TextP2FourP].Visible   := V2FourP;
  Text[screenSingViewRef.TextP3FourP].Visible   := V3FourP;
  Text[screenSingViewRef.TextP4FourP].Visible   := V4FourP;
  Text[screenSingViewRef.TextP1SixP].Visible    := V1SixP;
  Text[screenSingViewRef.TextP2SixP].Visible    := V2SixP;
  Text[screenSingViewRef.TextP3SixP].Visible    := V3SixP;
  Text[screenSingViewRef.TextP4SixP].Visible    := V4SixP;
  Text[screenSingViewRef.TextP5SixP].Visible    := V5SixP;
  Text[screenSingViewRef.TextP6SixP].Visible    := V6SixP;
  Text[screenSingViewRef.TextP1DuetFourP].Visible   := V1DuetFourP;
  Text[screenSingViewRef.TextP2DuetFourP].Visible   := V2DuetFourP;
  Text[screenSingViewRef.TextP3DuetFourP].Visible   := V3DuetFourP;
  Text[screenSingViewRef.TextP4DuetFourP].Visible   := V4DuetFourP;
  Text[screenSingViewRef.TextP1DuetSixP].Visible    := V1DuetSixP;
  Text[screenSingViewRef.TextP2DuetSixP].Visible    := V2DuetSixP;
  Text[screenSingViewRef.TextP3DuetSixP].Visible    := V3DuetSixP;
  Text[screenSingViewRef.TextP4DuetSixP].Visible    := V4DuetSixP;
  Text[screenSingViewRef.TextP5DuetSixP].Visible    := V5DuetSixP;
  Text[screenSingViewRef.TextP6DuetSixP].Visible    := V6DuetSixP;

  BadPlayer := AudioInputProcessor.CheckPlayersConfig(PlayersPlay);
  if (BadPlayer <> 0) then
  begin
    ScreenPopupError.ShowPopup(
        Format(Language.Translate('ERROR_PLAYER_NO_DEVICE_ASSIGNMENT'),
        [BadPlayer]));
  end;

  if (CurrentSong.isDuet) then
  begin
    Col := GetLyricColor(Ini.SingColor[0]);

//    if (PlayersPlay = 3) or (PlayersPlay = 6) then
//      Col := GetLyricColor(1);

    if (PlayersPlay = 4) then
    begin
      screenSingViewRef.ColPlayer[0] := GetLyricColor(Ini.SingColor[0]);
      screenSingViewRef.ColPlayer[1] := GetLyricColor(Ini.SingColor[1]);
      screenSingViewRef.ColPlayer[2] := GetLyricColor(Ini.SingColor[2]);
      screenSingViewRef.ColPlayer[3] := GetLyricColor(Ini.SingColor[3]);
    end;

  end
  else
    Col := GetLyricColor(1);;

  // set custom options
  if (CurrentSong.isDuet) and (PlayersPlay <> 1) then
  begin
    ColP1 := GetLyricColor(Ini.SingColor[0]);
    ColP2 := GetLyricColor(Ini.SingColor[1]);

    //if (PlayersPlay = 6) then
    //  Col := GetLyricColor(2);

    // set custom options
    case Ini.LyricsFont of
      0: // normal fonts
      begin
        LyricsDuetP1.FontStyle := ftNormal;
        LyricsDuetP2.FontStyle := ftNormal;

        LyricsDuetP1.LineColor_en.R := Skin_FontR;
        LyricsDuetP1.LineColor_en.G := Skin_FontG;
        LyricsDuetP1.LineColor_en.B := Skin_FontB;
        LyricsDuetP1.LineColor_en.A := 1;

        LyricsDuetP2.LineColor_en.R := Skin_FontR;
        LyricsDuetP2.LineColor_en.G := Skin_FontG;
        LyricsDuetP2.LineColor_en.B := Skin_FontB;
        LyricsDuetP2.LineColor_en.A := 1;

        LyricsDuetP1.LineColor_dis.R := 0.4;
        LyricsDuetP1.LineColor_dis.G := 0.4;
        LyricsDuetP1.LineColor_dis.B := 0.4;
        LyricsDuetP1.LineColor_dis.A := 1;

        LyricsDuetP2.LineColor_dis.R := 0.4;
        LyricsDuetP2.LineColor_dis.G := 0.4;
        LyricsDuetP2.LineColor_dis.B := 0.4;
        LyricsDuetP2.LineColor_dis.A := 1;

        LyricsDuetP1.LineColor_act.R := ColP1.R; //0.02;
        LyricsDuetP1.LineColor_act.G := ColP1.G; //0.6;
        LyricsDuetP1.LineColor_act.B := ColP1.B; //0.8;
        LyricsDuetP1.LineColor_act.A := 1;

        LyricsDuetP2.LineColor_act.R := ColP2.R; //0.02;
        LyricsDuetP2.LineColor_act.G := ColP2.G; //0.6;
        LyricsDuetP2.LineColor_act.B := ColP2.B; //0.8;
        LyricsDuetP2.LineColor_act.A := 1;

      end;
      1, 2: // outline fonts
      begin
        if (Ini.LyricsFont = 1) then
        begin
          LyricsDuetP1.FontStyle := ftOutline1;
          LyricsDuetP2.FontStyle := ftOutline1;
        end
        else
        begin
          LyricsDuetP1.FontStyle := ftOutline2;
          LyricsDuetP2.FontStyle := ftOutline2;
        end;

        LyricsDuetP1.LineColor_en.R := 0.7;
        LyricsDuetP1.LineColor_en.G := 0.7;
        LyricsDuetP1.LineColor_en.B := 0.7;
        LyricsDuetP1.LineColor_en.A := 1;

        LyricsDuetP2.LineColor_en.R := 0.7;
        LyricsDuetP2.LineColor_en.G := 0.7;
        LyricsDuetP2.LineColor_en.B := 0.7;
        LyricsDuetP2.LineColor_en.A := 1;

        LyricsDuetP1.LineColor_dis.R := 0.8;
        LyricsDuetP1.LineColor_dis.G := 0.8;
        LyricsDuetP1.LineColor_dis.B := 0.8;
        LyricsDuetP1.LineColor_dis.A := 1;

        LyricsDuetP2.LineColor_dis.R := 0.8;
        LyricsDuetP2.LineColor_dis.G := 0.8;
        LyricsDuetP2.LineColor_dis.B := 0.8;
        LyricsDuetP2.LineColor_dis.A := 1;

        LyricsDuetP1.LineColor_act.R := ColP1.R; //0.5;
        LyricsDuetP1.LineColor_act.G := ColP1.G; //0.5;
        LyricsDuetP1.LineColor_act.B := ColP1.B; //1;
        LyricsDuetP1.LineColor_act.A := 1;

        LyricsDuetP2.LineColor_act.R := ColP2.R; //0.5;
        LyricsDuetP2.LineColor_act.G := ColP2.G; //0.5;
        LyricsDuetP2.LineColor_act.B := ColP2.B; //1;
        LyricsDuetP2.LineColor_act.A := 1;
      end;
    end; // case

  end
  else
  begin

    case Ini.LyricsFont of
      0: // normal fonts
      begin
        Lyrics.FontStyle := ftNormal;

        Lyrics.LineColor_en.R := Skin_FontR;
        Lyrics.LineColor_en.G := Skin_FontG;
        Lyrics.LineColor_en.B := Skin_FontB;
        Lyrics.LineColor_en.A := 1;

        Lyrics.LineColor_dis.R := 0.4;
        Lyrics.LineColor_dis.G := 0.4;
        Lyrics.LineColor_dis.B := 0.4;
        Lyrics.LineColor_dis.A := 1;

        Lyrics.LineColor_act.R := Col.R; //0.02;
        Lyrics.LineColor_act.G := Col.G; //0.6;
        Lyrics.LineColor_act.B := Col.B; //0.8;
        Lyrics.LineColor_act.A := 1;
      end;
      1, 2: // outline fonts
      begin
        if (Ini.LyricsFont = 1) then
          Lyrics.FontStyle := ftOutline1
        else
          Lyrics.FontStyle := ftOutline2;

        if (Ini.JukeboxSingLineColor = High(UIni.ISingLineColor)) then
          Col := GetJukeboxLyricOtherColor(0)
        else
          Col := GetLyricColor(Ini.JukeboxSingLineColor);
        Lyrics.LineColor_act.R := Col.R;
        Lyrics.LineColor_act.G := Col.G;
        Lyrics.LineColor_act.B := Col.B;
        Lyrics.LineColor_act.A := 1;

        if (Ini.JukeboxActualLineColor = High(UIni.IActualLineColor)) then
          Col := GetJukeboxLyricOtherColor(1)
        else
          Col := GetLyricGrayColor(Ini.JukeboxActualLineColor);
        Lyrics.LineColor_en.R := Col.R;
        Lyrics.LineColor_en.G := Col.G;
        Lyrics.LineColor_en.B := Col.B;
        Lyrics.LineColor_en.A := 1;

        if (Ini.JukeboxNextLineColor = High(UIni.INextLineColor)) then
          Col := GetJukeboxLyricOtherColor(2)
        else
          Col := GetLyricGrayColor(Ini.JukeboxNextLineColor);
        Lyrics.LineColor_dis.R := Col.R;
        Lyrics.LineColor_dis.G := Col.G;
        Lyrics.LineColor_dis.B := Col.B;
        Lyrics.LineColor_dis.A := 1;
      end;
    end; // case
  end;

  // deactivate pause
  Paused := false;

  LoadNextSong();

  Log.LogStatus('End', 'OnShow');
end;

procedure TScreenSingController.onShowFinish;
var
  I, Index: integer;
begin
  // hide cursor on singscreen show
  Display.SetCursor;

  // clear the scores of all players
  for Index := 0 to High(Player) do
    with Player[Index] do
    begin
      Score          := 0;
      ScoreLine      := 0;
      ScoreGolden    := 0;

      ScoreInt       := 0;
      ScoreLineInt   := 0;
      ScoreGoldenInt := 0;
      ScoreTotalInt  := 0;

      ScoreLast      := 0;

      LastSentencePerfect := false;
    end;

  // prepare music
  // Important: AudioPlayback must not be initialized in onShow() as TScreenSong
  // uses stops AudioPlayback in onHide() which interferes with TScreenSings onShow.
  PlayMidi := false;
  MidiFadeIn := false;

  AudioPlayback.Open(CurrentSong.Path.Append(CurrentSong.Mp3));
  if ScreenSong.Mode = smMedley then
    AudioPlayback.SetVolume(0.1)
  else
    AudioPlayback.SetVolume(1.0);
  //AudioPlayback.Position := CurrentSong.Start;
  AudioPlayback.Position := LyricsState.GetCurrentTime();


  // set time
  if (CurrentSong.Finish > 0) then
    LyricsState.TotalTime := CurrentSong.Finish / 1000
  else
  begin
    LyricsState.TotalTime := AudioPlayback.Length;
  end;

  LyricsState.UpdateBeats();

  // synchronize music
  if Ini.SyncTo = Ord(stLyrics) then
    AudioPlayback.SetSyncSource(fLyricsSync)
  else
    AudioPlayback.SetSyncSource(nil);

  // synchronize lyrics (do not set this before AudioPlayback is initialized)
  if Ini.SyncTo = Ord(stMusic) then
    LyricsState.SetSyncSource(fMusicSync)
  else
    LyricsState.SetSyncSource(nil);

  // start lyrics
  LyricsState.Start(true);

  // start music
  if ScreenSong.Mode = smMedley then
    AudioPlayback.FadeIn(CurrentSong.Medley.FadeIn_time, 1.0)
  else
    AudioPlayback.Play();

  // Send Score
  Act_MD5Song := CurrentSong.MD5;
  Act_Level := Ini.PlayerLevel[0];

  // start timer
  CountSkipTimeSet;

end;

procedure TScreenSingController.SongError();
var
  I, len:  integer;

begin
  if ScreenSong.Mode <> smMedley then
  begin
    // error loading song -> go back to previous screen and show some error message
    Display.AbortScreenChange;

    // select new song in party mode
    if ScreenSong.Mode = smPartyClassic then
      ScreenSong.SelectRandomSong();

    if (Length(CurrentSong.LastError) > 0) then
      ScreenPopupError.ShowPopup(Format(Language.Translate(CurrentSong.LastError), [CurrentSong.ErrorLineNo]))
    else
      ScreenPopupError.ShowPopup(Language.Translate('ERROR_CORRUPT_SONG'));
    // FIXME: do we need this?
    CurrentSong.Path := CatSongs.Song[CatSongs.Selected].Path;
    Exit;
  end
  else
  begin
    if (PlaylistMedley.CurrentMedleySong<PlaylistMedley.NumMedleySongs) then
    begin
      //Error Loading Song in Medley Mode -> skip actual Medley Song an go on if possible
      len := Length(PlaylistMedley.Song);
      for I := PlaylistMedley.CurrentMedleySong-1 to len - 1 do
        PlaylistMedley.Song[I] := PlaylistMedley.Song[I+1];

      SetLength(PlaylistMedley.Song, Len-1);
      Dec(PlaylistMedley.NumMedleySongs);
      LoadNextSong;
      Exit;
    end
    else
    begin
      if (PlaylistMedley.NumMedleySongs=1) then
      begin
        //Error Loading Song in Medley Mode -> Go back to Song Screen and Show some Error Message
        Display.AbortScreenChange;

        // select new song in party mode
        if ScreenSong.Mode = smPartyClassic then
          ScreenSong.SelectRandomSong();

        if (Length(CurrentSong.LastError) > 0) then
          ScreenPopupError.ShowPopup(Format(Language.Translate(CurrentSong.LastError), [CurrentSong.ErrorLineNo]))
        else
          ScreenPopupError.ShowPopup(Language.Translate('ERROR_CORRUPT_SONG'));

        // FIXME: do we need this?
        CurrentSong.Path := CatSongs.Song[CatSongs.Selected].Path;
        Exit;
      end
      else
      begin
        //Error Loading Song in Medley Mode -> Finish actual round
        len := Length(PlaylistMedley.Song);
        SetLength(PlaylistMedley.Song, len-1);
        Dec(PlaylistMedley.NumMedleySongs);
        Finish;
        Exit;
      end;
    end;
  end;
end;

procedure TScreenSingController.LoadNextSong();
var
  Color:      TRGB;
  Index:      integer;
  VideoFile:  IPath;
  BgFile:     IPath;
  success:    boolean;

  function FindNote(beat: integer): TPos;
  var
    line:   integer;
    note:   integer;
    found:  boolean;
    min:    integer;
    diff:   integer;

  begin
    found := false;

    for line := 0 to length(Lines[0].Line) - 1 do
    begin
      for note := 0 to length(Lines[0].Line[line].Note) - 1 do
      begin
        if (beat >= Lines[0].Line[line].Note[line].Start) and
           (beat <= Lines[0].Line[line].Note[line].Start + Lines[0].Line[line].Note[note].Length) then
        begin
          Result.part := 0;
          Result.line := line;
          Result.note := note;
          Result.CP := 0;
          found:=true;
          break;
        end;
      end;
    end;

    if found then //found exactly
      exit;

    if CurrentSong.isDuet and (PlayersPlay <> 1) then
    begin
      for Line := 0 to length(Lines[1].Line) - 1 do
      begin
        for Note := 0 to length(Lines[1].Line[Line].Note) - 1 do
        begin
          if (beat>=Lines[1].Line[Line].Note[Note].Start) and
            (beat<=Lines[1].Line[Line].Note[Note].Start + Lines[1].Line[Line].Note[Note].Length) then
          begin
            Result.CP := 1;
            Result.line := Line;
            Result.note := Note;
            found:=true;
            break;
          end;
        end;
      end;
    end;

    if found then //found exactly
      exit;

    min := high(integer);
    //second try (approximating)
    for line := 0 to length(Lines[0].Line) - 1 do
    begin
      for note := 0 to length(Lines[0].Line[line].Note) - 1 do
      begin
        diff := abs(Lines[0].Line[line].Note[note].Start - beat);
        if diff < min then
        begin
          Result.part := 0;
          Result.line := line;
          Result.note := note;
          Result.CP := 0;
          min := diff;
        end;
      end;
    end;

    if CurrentSong.isDuet and (PlayersPlay <> 1) then
    begin
      for Line := 0 to length(Lines[1].Line) - 1 do
      begin
        for Note := 0 to length(Lines[1].Line[Line].Note) - 1 do
        begin
          diff := abs(Lines[1].Line[Line].Note[Note].Start - beat);
          if diff<min then
          begin
            Result.CP := 1;
            Result.line := Line;
            Result.note := Note;
            min := diff;
          end;
        end;
      end;
    end;

  end;

begin
  // background texture (garbage disposal)
  if (Tex_Background.TexNum > 0) then
  begin
    glDeleteTextures(1, PGLuint(@Tex_Background.TexNum));
    Tex_Background.TexNum := 0;
  end;

  // reset video playback engine
  fCurrentVideo := nil;

  // setup score manager
  Scores.ClearPlayers; // clear old player values
  Color.R := 0;
  Color.G := 0;
  Color.B := 0;

  // add new players
  for Index := 0 to PlayersPlay - 1 do
  begin
    Scores.AddPlayer(Tex_ScoreBG[Index], Color);
  end;

  Scores.Init; // get positions for players

  // FIXME: sets path and filename to ''
  ResetSingTemp;

  PlaylistMedley.ApplausePlayed := false;

  if ScreenSong.Mode = smMedley then
  begin
    if length(PlaylistMedley.Song) >= PlaylistMedley.CurrentMedleySong then
    begin
      CatSongs.Selected := PlaylistMedley.Song[PlaylistMedley.CurrentMedleySong-1];
      //Music.Open(CatSongs.Song[CatSongs.Selected].Path + CatSongs.Song[CatSongs.Selected].Mp3);
    end
    else
    begin
      SongError;
      Exit;
    end;
  end;

  CurrentSong := CatSongs.Song[CatSongs.Selected];
  success := false;
  // FIXME: bad style, put the try-except into loadsong() and not here
  try
    // check if file is xml
    if CurrentSong.FileName.GetExtension.ToUTF8 = '.xml' then
      success := CurrentSong.AnalyseXML and CurrentSong.LoadXMLSong()
    else
      success := CurrentSong.Analyse(false, ScreenSong.DuetChange); // and CurrentSong.LoadSong();
  except
    on E: EInOutError do Log.LogWarn(E.Message, 'TScreenSing.LoadNextSong');
  end;

  if (not success) then
  begin
    SongError();
    Exit;
  end;

  // Set up Medley timings
  if ScreenSong.Mode = smMedley then
  begin
    CurrentSong.SetMedleyMode();

    if (PlaylistMedley.NumMedleySongs > 1) then
      Text[screenSingViewRef.SongNameText].Text := IntToStr(PlaylistMedley.CurrentMedleySong) +
        '/' + IntToStr(PlaylistMedley.NumMedleySongs) + ': ' +
        CurrentSong.Artist + ' - ' + CurrentSong.Title
    else
      Text[screenSingViewRef.SongNameText].Text := CurrentSong.Artist + ' - ' + CurrentSong.Title;

    //medley start and end timestamps
    StartNote := FindNote(CurrentSong.Medley.StartBeat - round(CurrentSong.BPM[0].BPM*CurrentSong.Medley.FadeIn_time/60));
    MedleyStart := GetTimeFromBeat(Lines[0].Line[StartNote.line].Note[0].Start);

    //check Medley-Start
    if (MedleyStart+CurrentSong.Medley.FadeIn_time*0.5>GetTimeFromBeat(CurrentSong.Medley.StartBeat)) then
      MedleyStart := GetTimeFromBeat(CurrentSong.Medley.StartBeat) - CurrentSong.Medley.FadeIn_time;
    if MedleyStart<0 then
      MedleyStart := 0;

    MedleyEnd := GetTimeFromBeat(CurrentSong.Medley.EndBeat) + CurrentSong.Medley.FadeOut_time;
  end;

  {*
   * == Background ==
   * We have five types of backgrounds:
   *   + Blank        : Nothing has been set, this is our fallback
   *   + Picture      : Picture has been set, and exists - otherwise we fallback
   *   + Video        : Video has been set, and exists - otherwise we fallback
   *   + Visualization: + Off                 : No visualization
   *                    + WhenNoVideo         : Overwrites blank and picture
   *                    + WhenNoVideoAndImage : Overwrites blank
   *                    + On                  : Overwrites blank, picture and video
   *}

  {*
   * set background to: video
   * Note: ffmpeg / this is also used for many background formats"
   *}
  fShowVisualization := false;
  VideoFile := CurrentSong.Path.Append(CurrentSong.Video);
  if (Ini.VideoEnabled = 1) and CurrentSong.Video.IsSet() and VideoFile.IsFile then
  begin
    fVideoClip := VideoPlayback.Open(VideoFile);
    fCurrentVideo := fVideoClip;
    if (fVideoClip <> nil) then
    begin
      fShowVisualization := false;
      if ScreenSong.Mode = smMedley then
        fCurrentVideo.Position := CurrentSong.VideoGAP + MedleyStart
      else
        fCurrentVideo.Position := CurrentSong.VideoGAP + CurrentSong.Start;
      fCurrentVideo.Play;
    end;
  end;

  {*
   * set background to: picture
   *}
  if (CurrentSong.Background.IsSet) then
  begin
    BgFile := CurrentSong.Path.Append(CurrentSong.Background);
    try
      Tex_Background := Texture.LoadTexture(BgFile);
    except
      Log.LogError('Background could not be loaded: ' + BgFile.ToNative);
      Tex_Background.TexNum := 0;
    end
  end
  else
  begin
    Tex_Background.TexNum := 0;
  end;

  {*
   * set background to: visualization (Overwrites all)
   *}
  if (TVisualizerOption(Ini.VisualizerOption) in [voOn]) then
  begin
    fShowVisualization := true;
    fCurrentVideo := Visualization.Open(PATH_NONE);
    if (fCurrentVideo <> nil) then
      fCurrentVideo.Play;
  end

  {*
   * set background to: visualization (if video and image is not set)
   *}
  else if (TVisualizerOption(Ini.VisualizerOption) in [voWhenNoVideoAndImage]) and
     (not CurrentSong.Background.IsSet) and (fVideoClip = nil) then
  begin
    fShowVisualization := true;
    fCurrentVideo := Visualization.Open(PATH_NONE);
    if fCurrentVideo <> nil then
      fCurrentVideo.Play;
  end

  {*
   * set background to: visualization (Videos are still shown)
   *}
  else if ((TVisualizerOption(Ini.VisualizerOption) in [voWhenNoVideo]) and
     (fVideoClip = nil)) then
  begin
    fShowVisualization := true;
    fCurrentVideo := Visualization.Open(PATH_NONE);
    if fCurrentVideo <> nil then
      fCurrentVideo.Play;
  end;

  // prepare lyrics timer
  LyricsState.Reset();

  if ScreenSong.Mode = smMedley then
  begin
    LyricsState.SetCurrentTime(MedleyStart);
    LyricsState.StartTime := CurrentSong.Gap;
    LyricsState.TotalTime := MedleyEnd;
  end
  else
  begin
    LyricsState.SetCurrentTime(CurrentSong.Start);
    LyricsState.StartTime := CurrentSong.Gap;
    if CurrentSong.Finish > 0 then
      LyricsState.TotalTime := CurrentSong.Finish / 1000
    else
      LyricsState.TotalTime := AudioPlayback.Length;
  end;

  LyricsState.UpdateBeats();

  // prepare and start voice-capture
  AudioInput.CaptureStart;

  // main text
  Lyrics.Clear(CurrentSong.BPM[0].BPM, CurrentSong.Resolution);
  LyricsDuetP1.Clear(CurrentSong.BPM[0].BPM, CurrentSong.Resolution);
  LyricsDuetP2.Clear(CurrentSong.BPM[0].BPM, CurrentSong.Resolution);

  if (CurrentSong.isDuet) and (PlayersPlay <> 1) then
  begin
    // initialize lyrics by filling its queue
    while (not LyricsDuetP1.IsQueueFull) and
          (LyricsDuetP1.LineCounter <= High(Lines[0].Line)) do
    begin
      LyricsDuetP1.AddLine(@Lines[0].Line[LyricsDuetP1.LineCounter]);
    end;

    // initialize lyrics by filling its queue
    while (not LyricsDuetP2.IsQueueFull) and
          (LyricsDuetP2.LineCounter <= High(Lines[1].Line)) do
    begin
      LyricsDuetP2.AddLine(@Lines[1].Line[LyricsDuetP2.LineCounter]);
    end;
  end
  else
  begin
    // initialize lyrics by filling its queue
    while (not Lyrics.IsQueueFull) and
          (Lyrics.LineCounter <= High(Lines[0].Line)) do
    begin
      Lyrics.AddLine(@Lines[0].Line[Lyrics.LineCounter]);
    end;
  end;

  // kill all stars not killed yet (goldenstarstwinkle mod)
  GoldenRec.SentenceChange(0);
  if (CurrentSong.isDuet) and (PlayersPlay <> 1) then
    GoldenRec.SentenceChange(1);

  // set position of line bonus - line bonus end
  // set number of empty sentences for line bonus
  NumEmptySentences[0] := 0;
  NumEmptySentences[1] := 0;

  if (CurrentSong.isDuet) and (PlayersPlay <> 1) then
  begin
    for Index := Low(Lines[1].Line) to High(Lines[1].Line) do
    if Lines[1].Line[Index].TotalNotes = 0 then
      Inc(NumEmptySentences[1]);

    for Index := Low(Lines[0].Line) to High(Lines[0].Line) do
    if Lines[0].Line[Index].TotalNotes = 0 then
      Inc(NumEmptySentences[0]);
  end
  else
  begin
    for Index := Low(Lines[0].Line) to High(Lines[0].Line) do
      if Lines[0].Line[Index].TotalNotes = 0 then
        Inc(NumEmptySentences[0]);
  end;

  eSongLoaded.CallHookChain(False);

  if (ScreenSong.Mode = smMedley) and (PlaylistMedley.CurrentMedleySong>1) then
    onShowFinish;
end;

procedure TScreenSingController.ClearSettings;
begin
  Settings.Finish := False;
  Settings.LyricsVisible := True;
  Settings.NotesVisible := high(Integer);
  Settings.PlayerEnabled := high(Integer);
  Settings.SoundEnabled := True;
end;

{ applies changes of settings record }
procedure TScreenSingController.ApplySettings;
begin
  //
end;

procedure TScreenSingController.EndSong;
begin
  Settings.Finish := True;
end;

procedure TScreenSingController.OnHide;
begin
  // background texture
  if Tex_Background.TexNum > 0 then
  begin
    glDeleteTextures(1, PGLuint(@Tex_Background.TexNum));
    Tex_Background.TexNum := 0;
  end;
  if fShowWebcam then
        begin
          Webcam.Release;
          fShowWebCam:=false;
        end;
  Background.OnFinish;
  Display.SetCursor;
end;

function TScreenSingController.Draw: boolean;
begin
  Result := screenSingViewRef.Draw();
end;

function TScreenSingController.FinishedMusic: boolean;
begin
  Result := AudioPlayback.Finished;
end;

procedure TScreenSingController.Finish;
var
  I, J:     integer;
  len, num: integer;

begin
  AudioInput.CaptureStop;
  AudioPlayback.Stop;
  AudioPlayback.SetSyncSource(nil);

  if (ScreenSong.Mode = smNormal) and (SungPaused = false) and (SungToEnd) and (Length(DllMan.Websites) > 0) then
  begin
    AutoSendScore;
    AutoSaveScore;
  end;

  LyricsState.Stop();
  LyricsState.SetSyncSource(nil);

  // close video files
  fVideoClip := nil;
  fCurrentVideo := nil;

  // kill all stars and effects
  GoldenRec.KillAll;

  if (Ini.SavePlayback = 1) then
  begin
    Log.BenchmarkStart(0);
    Log.LogVoice(0);
    if (PlayersPlay > 1) then
      Log.LogVoice(1);
    if (PlayersPlay > 2) then
      Log.LogVoice(2);
    Log.BenchmarkEnd(0);
    Log.LogBenchmark('Creating files', 0);
  end;

  SetFontItalic(false);

  if ScreenSong.Mode = smMedley then
  begin
    if not FadeOut then
    begin
      for I := 0 to PlayersPlay - 1 do
        PlaylistMedley.Stats[Length(PlaylistMedley.Stats) - 1].Player[I] := Player[I];

      Inc(PlaylistMedley.CurrentMedleySong);
      if PlaylistMedley.CurrentMedleySong <= PlaylistMedley.NumMedleySongs then
      begin
        LoadNextSong;
      end
      else
      begin
        //build sums
        len := Length(PlaylistMedley.Stats);
        num := PlaylistMedley.NumPlayer;

        SetLength(PlaylistMedley.Stats, len + 1);
        SetLength(PlaylistMedley.Stats[len].Player, num);

        for J := 0 to len - 1 do
        begin
          for I := 0 to num - 1 do
          begin
            PlaylistMedley.Stats[len].Player[I].Score :=
              PlaylistMedley.Stats[len].Player[I].Score +
              PlaylistMedley.Stats[J].Player[I].Score;

            PlaylistMedley.Stats[len].Player[I].ScoreLine :=
              PlaylistMedley.Stats[len].Player[I].ScoreLine +
              PlaylistMedley.Stats[J].Player[I].ScoreLine;

            PlaylistMedley.Stats[len].Player[I].ScoreGolden :=
              PlaylistMedley.Stats[len].Player[I].ScoreGolden +
              PlaylistMedley.Stats[J].Player[I].ScoreGolden;

            PlaylistMedley.Stats[len].Player[I].ScoreInt :=
              PlaylistMedley.Stats[len].Player[I].ScoreInt +
              PlaylistMedley.Stats[J].Player[I].ScoreInt;

            PlaylistMedley.Stats[len].Player[I].ScoreLineInt :=
              PlaylistMedley.Stats[len].Player[I].ScoreLineInt +
              PlaylistMedley.Stats[J].Player[I].ScoreLineInt;

            PlaylistMedley.Stats[len].Player[I].ScoreGoldenInt :=
              PlaylistMedley.Stats[len].Player[I].ScoreGoldenInt +
              PlaylistMedley.Stats[J].Player[I].ScoreGoldenInt;

            PlaylistMedley.Stats[len].Player[I].ScoreTotalInt :=
              PlaylistMedley.Stats[len].Player[I].ScoreTotalInt +
              PlaylistMedley.Stats[J].Player[I].ScoreTotalInt;
          end; //of for I
        end; //of for J

        //build mean on sum
        for I := 0 to num - 1 do
        begin
          PlaylistMedley.Stats[len].Player[I].Score := round(
            PlaylistMedley.Stats[len].Player[I].Score / len);

          PlaylistMedley.Stats[len].Player[I].ScoreLine := round(
            PlaylistMedley.Stats[len].Player[I].ScoreLine / len);

          PlaylistMedley.Stats[len].Player[I].ScoreGolden := round(
            PlaylistMedley.Stats[len].Player[I].ScoreGolden / len);

          PlaylistMedley.Stats[len].Player[I].ScoreInt := round(
            PlaylistMedley.Stats[len].Player[I].ScoreInt / len);

          PlaylistMedley.Stats[len].Player[I].ScoreLineInt := round(
            PlaylistMedley.Stats[len].Player[I].ScoreLineInt / len);

          PlaylistMedley.Stats[len].Player[I].ScoreGoldenInt := round(
            PlaylistMedley.Stats[len].Player[I].ScoreGoldenInt / len);

          PlaylistMedley.Stats[len].Player[I].ScoreTotalInt := round(
            PlaylistMedley.Stats[len].Player[I].ScoreTotalInt / len);
        end;

        Party.CallAfterSing;
        FadeOut:=true;
      end;
    end;
  end
  else
  begin
    SetLength(PlaylistMedley.Stats, 1);
    SetLength(PlaylistMedley.Stats[0].Player, PlayersPlay);
    for I := 0 to PlayersPlay - 1 do
      PlaylistMedley.Stats[0].Player[I] := Player[I];

    PlaylistMedley.Stats[0].SongArtist := CurrentSong.Artist;
    PlaylistMedley.Stats[0].SongTitle := CurrentSong.Title;

    if not FadeOut then
      Party.CallAfterSing;

    FadeOut := true;
  end;

end;

procedure TScreenSingController.OnSentenceEnd(CP: integer; SentenceIndex: cardinal); //ToDo: split and redo
var
  PlayerIndex: byte;
  CurrentPlayer: PPLayer;
  CurrentScore: real;
  Line:      PLine;
  LinePerfection: real;  // perfection of singing performance on the current line
  Rating:    integer;
  LineScore: real;
  LineBonus: real;
  MaxSongScore: integer; // max. points for the song (without line bonus)
  MaxLineScore: real;    // max. points for the current line
  Index: integer;
const
  // TODO: move this to a better place
  MAX_LINE_RATING = 8;        // max. rating for singing performance
begin
  Line := @Lines[CP].Line[SentenceIndex];

  // check for empty sentence
  if Line.TotalNotes <= 0 then
    Exit;

  // set max song score
  if Ini.LineBonus = 0 then
    MaxSongScore := MAX_SONG_SCORE
  else
    MaxSongScore := MAX_SONG_SCORE - MAX_SONG_LINE_BONUS;

  // Note: ScoreValue is the sum of all note values of the song
  MaxLineScore := MaxSongScore * (Line.TotalNotes / Lines[CP].ScoreValue);

  for PlayerIndex := 0 to High(Player) do
  begin
    //PlayerIndex := Index;

    if (not CurrentSong.isDuet) or (PlayerIndex mod 2 = CP) or (PlayersPlay = 1)then
    begin
      CurrentPlayer := @Player[PlayerIndex];
      CurrentScore  := CurrentPlayer.Score + CurrentPlayer.ScoreGolden;

      // line bonus

      // points for this line
      LineScore := CurrentScore - CurrentPlayer.ScoreLast;

      // check for lines with low points
      if MaxLineScore <= 2 then
        LinePerfection := 1
      else
        // determine LinePerfection
        // Note: the "+2" extra points are a little bonus so the player does not
        // have to be that perfect to reach the bonus steps.
        LinePerfection := LineScore / (MaxLineScore - 2);

      // clamp LinePerfection to range [0..1]
      if LinePerfection < 0 then
        LinePerfection := 0
      else if LinePerfection > 1 then
        LinePerfection := 1;

      // add line-bonus if enabled
      if Ini.LineBonus > 0 then
      begin
        // line-bonus points (same for each line, no matter how long the line is)
        LineBonus := MAX_SONG_LINE_BONUS / (Length(Lines[CP].Line) -
          NumEmptySentences[CP]);
        // apply line-bonus
        CurrentPlayer.ScoreLine :=
          CurrentPlayer.ScoreLine + LineBonus * LinePerfection;
        CurrentPlayer.ScoreLineInt := Floor(CurrentPlayer.ScoreLine / 10) * 10;
        // update total score
        CurrentPlayer.ScoreTotalInt :=
          CurrentPlayer.ScoreInt +
          CurrentPlayer.ScoreGoldenInt
          + CurrentPlayer.ScoreLineInt;

        // spawn rating pop-up
        Rating := Round(LinePerfection * MAX_LINE_RATING);
        Scores.SpawnPopUp(PlayerIndex, Rating, CurrentPlayer.ScoreTotalInt);
      end
      else
        Scores.RaiseScore(PlayerIndex, CurrentPlayer.ScoreTotalInt);

      // PerfectLineTwinkle (effect), part 1
      if Ini.EffectSing = 1 then
        CurrentPlayer.LastSentencePerfect := (LinePerfection >= 1);

      // refresh last score
      CurrentPlayer.ScoreLast := CurrentScore;
    end;
  end;

  // PerfectLineTwinkle (effect), part 2
  if Ini.EffectSing = 1 then
  begin
    GoldenRec.SpawnPerfectLineTwinkle;

    for PlayerIndex := 0 to High(Player) do
    begin
      CurrentPlayer := @Player[PlayerIndex];
      CurrentPlayer.LastSentencePerfect := false;
    end;
  end;

end;

 // Called on sentence change
 // SentenceIndex: index of the new active sentence
procedure TScreenSingController.OnSentenceChange(CP: integer; SentenceIndex: cardinal);  //ToDo: split and redo
var
  tmp_Lyric: TLyricEngine;
begin
  // goldenstarstwinkle
  GoldenRec.SentenceChange(CP);

  if (CurrentSong.isDuet) and (PlayersPlay <> 1) then
  begin
    if (CP = 1) then
      tmp_Lyric := LyricsDuetP2
    else
      tmp_Lyric := LyricsDuetP1;
  end
  else
    tmp_Lyric := Lyrics;

  // fill lyrics queue and set upper line to the current sentence
  while (tmp_Lyric.GetUpperLineIndex() < SentenceIndex) or
    (not tmp_Lyric.IsQueueFull) do
  begin
    // add the next line to the queue or a dummy if no more lines are available
    if (tmp_Lyric.LineCounter <= High(Lines[CP].Line)) then
    begin
      tmp_Lyric.AddLine(@Lines[CP].Line[tmp_Lyric.LineCounter]);
    end
    else
      tmp_Lyric.AddLine(nil);
  end;

end;

function TLyricsSyncSource.GetClock(): real;
begin
  Result := LyricsState.GetCurrentTime();
end;

function TMusicSyncSource.GetClock(): real;
begin
  Result := AudioPlayback.Position;
end;

procedure TScreenSingController.UpdateMedleyStats(medley_end: boolean);   //TODO: view or controller? unsure
var
  len, num, I : integer;

begin
  len := Length(PlaylistMedley.Stats);
  num := PlaylistMedley.NumPlayer;

  if (PlaylistMedley.CurrentMedleySong > len) and
    (PlaylistMedley.CurrentMedleySong <= PlaylistMedley.NumMedleySongs) then
  begin
    inc(len);
    SetLength(PlaylistMedley.Stats, len);
    SetLength(PlaylistMedley.Stats[len - 1].Player, num);
    PlaylistMedley.Stats[len-1].SongArtist := CurrentSong.Artist;
    PlaylistMedley.Stats[len-1].SongTitle := CurrentSong.Title;
  end;

  if PlaylistMedley.CurrentMedleySong <= PlaylistMedley.NumMedleySongs then
    for I := 0 to num - 1 do
      PlaylistMedley.Stats[len - 1].Player[I] := Player[I];

  if medley_end and not PlaylistMedley.ApplausePlayed and
    (PlaylistMedley.CurrentMedleySong<=PlaylistMedley.NumMedleySongs) then
  begin
    PlaylistMedley.ApplausePlayed:=true;

    AudioPlayback.Fade(CurrentSong.Medley.FadeOut_time, 0.1);
    AudioPlayback.PlaySound(SoundLib.Applause);
  end;
end;


procedure TScreenSingController.AutoSendScore;
var
  SendInfo: TSendInfo;
  SendStatus: byte;
  Send: boolean;
  TotalScore: integer;
  PlayerIndex, IndexWeb, IndexUser: integer;
begin
  for PlayerIndex := 1 to PlayersPlay do
  begin
    for IndexWeb := 0 to High(DataBase.NetworkUser) do
    begin
      for IndexUser := 0 to High(DataBase.NetworkUser[IndexWeb].Userlist) do
      begin
        Send := false;
        TotalScore := player[PlayerIndex - 1].ScoreInt + player[PlayerIndex - 1].ScoreLineInt + player[PlayerIndex - 1].ScoreGoldenInt;

        case (Act_Level) of
          0: if (TotalScore >= DataBase.NetworkUser[IndexWeb].UserList[IndexUser].AutoScoreEasy)
              and (DataBase.NetworkUser[IndexWeb].UserList[IndexUser].AutoMode = 1)
              and (DataBase.NetworkUser[IndexWeb].UserList[IndexUser].AutoPlayer = PlayerIndex - 1) then
                Send := true;

          1: if (TotalScore >= DataBase.NetworkUser[IndexWeb].UserList[IndexUser].AutoScoreMedium)
              and (DataBase.NetworkUser[IndexWeb].UserList[IndexUser].AutoMode = 1)
              and (DataBase.NetworkUser[IndexWeb].UserList[IndexUser].AutoPlayer = PlayerIndex - 1) then
                Send := true;

          2: if (TotalScore >= DataBase.NetworkUser[IndexWeb].UserList[IndexUser].AutoScoreHard)
              and (DataBase.NetworkUser[IndexWeb].UserList[IndexUser].AutoMode = 1)
              and (DataBase.NetworkUser[IndexWeb].UserList[IndexUser].AutoPlayer = PlayerIndex - 1) then
                Send := true;
        end;

        if (Send) then
        begin

          DllMan.LoadWebsite(IndexWeb);

          SendInfo.Username := DataBase.NetworkUser[IndexWeb].UserList[IndexUser].Username;
          SendInfo.Password := DataBase.NetworkUser[IndexWeb].UserList[IndexUser].Password;

          if (DataBase.NetworkUser[IndexWeb].UserList[IndexUser].SendSavePlayer = 1) then
            SendInfo.Name := Ini.Name[PlayerIndex - 1]
          else
            SendInfo.Name := '';

          SendInfo.ScoreInt := player[PlayerIndex - 1].ScoreInt;
          SendInfo.ScoreLineInt := player[PlayerIndex - 1].ScoreLineInt;
          SendInfo.ScoreGoldenInt := player[PlayerIndex - 1].ScoreGoldenInt;
          SendInfo.MD5Song := Act_MD5Song;
          SendInfo.Level := Act_Level;

          SendStatus := DllMan.WebsiteSendScore(SendInfo);

          case SendStatus of
            0: ScreenPopupError.ShowPopup(Language.Translate('WEBSITE_NO_CONNECTION'));
            2: ScreenPopupError.ShowPopup(Language.Translate('WEBSITE_LOGIN_ERROR'));
            3: ScreenPopupInfo.ShowPopup(Language.Translate('WEBSITE_OK_SEND'));
            4: ScreenPopupError.ShowPopup(Language.Translate('WEBSITE_ERROR_SCORE'));
            5: ScreenPopupError.ShowPopup(Language.Translate('WEBSITE_ERROR_SCORE_DUPLICATED'));
            7: ScreenPopupError.ShowPopup(Language.Translate('WEBSITE_ERROR_SONG'));
          end;

       end;
      end;
    end;
  end;
end;

procedure TScreenSingController.AutoSaveScore;
var
  SendInfo: TSendInfo;
  ScoreFile: TextFile;
  EncryptText: string;
  WebName: UTF8String;
  Save: boolean;
  TotalScore: integer;
  PlayerIndex, IndexWeb, IndexUser: integer;
begin
  for PlayerIndex := 1 to PlayersPlay do
  begin
    for IndexWeb := 0 to High(DataBase.NetworkUser) do
    begin
      for IndexUser := 0 to High(DataBase.NetworkUser[IndexWeb].Userlist) do
      begin
        Save := false;
        TotalScore := player[PlayerIndex - 1].ScoreInt + player[PlayerIndex - 1].ScoreLineInt + player[PlayerIndex - 1].ScoreGoldenInt;

        case (Act_Level) of
          0: if (TotalScore >= DataBase.NetworkUser[IndexWeb].UserList[IndexUser].AutoScoreEasy)
              and (DataBase.NetworkUser[IndexWeb].UserList[IndexUser].AutoMode = 2)
              and (DataBase.NetworkUser[IndexWeb].UserList[IndexUser].AutoPlayer = PlayerIndex - 1) then
                Save := true;

          1: if (TotalScore >= DataBase.NetworkUser[IndexWeb].UserList[IndexUser].AutoScoreMedium)
              and (DataBase.NetworkUser[IndexWeb].UserList[IndexUser].AutoMode = 2)
              and (DataBase.NetworkUser[IndexWeb].UserList[IndexUser].AutoPlayer = PlayerIndex - 1) then
                Save := true;

          2: if (TotalScore >= DataBase.NetworkUser[IndexWeb].UserList[IndexUser].AutoScoreHard)
              and (DataBase.NetworkUser[IndexWeb].UserList[IndexUser].AutoMode = 2)
              and (DataBase.NetworkUser[IndexWeb].UserList[IndexUser].AutoPlayer = PlayerIndex - 1) then
                Save := true;
        end;

        if (Save) then
        begin

          DllMan.LoadWebsite(IndexWeb);

          SendInfo.Username := DataBase.NetworkUser[IndexWeb].UserList[IndexUser].Username;
          SendInfo.Password := DataBase.NetworkUser[IndexWeb].UserList[IndexUser].Password;

          if (DataBase.NetworkUser[IndexWeb].UserList[IndexUser].SendSavePlayer = 1) then
            SendInfo.Name := Ini.Name[PlayerIndex - 1]
          else
            SendInfo.Name := '';

          SendInfo.ScoreInt := player[PlayerIndex - 1].ScoreInt;
          SendInfo.ScoreLineInt := player[PlayerIndex - 1].ScoreLineInt;
          SendInfo.ScoreGoldenInt := player[PlayerIndex - 1].ScoreGoldenInt;
          SendInfo.MD5Song := Act_MD5Song;
          SendInfo.Level := Act_Level;

          WebName := DataBase.NetworkUser[IndexWeb].Website;
          EncryptText := DllMan.WebsiteEncryptScore(SendInfo);

          AssignFile(ScoreFile, WebScoresPath.Append(WebName + '.usc').ToNative);

          if FileExists(WebScoresPath.Append(WebName + '.usc').ToNative) then
            Append(ScoreFile)
          else
            Rewrite(ScoreFile);

          WriteLn(ScoreFile, DatetoStr(Now) + '|' + TimetoStr(Now) + '|' + EncryptText);

          Flush(ScoreFile);
          Close(ScoreFile);

          ScreenPopupInfo.ShowPopup(Language.Translate('WEBSITE_SAVE_SCORE'));

       end;
      end;
    end;
  end;
end;

end.

