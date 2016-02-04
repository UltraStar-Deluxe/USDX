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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenSing.pas $
 * $Id: UScreenSing.pas 3150 2015-10-20 00:07:57Z basisbit $
 *}

unit UScreenSing;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  sdl2,
  gl,
  TextGL,
  UAvatars,
  UCommon,
  UFiles,
  UGraphicClasses,
  UHookableEvent,
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
  TScreenSing = class(TMenu)
  private
    // views
    fShowVisualization: boolean;
    fShowBackground: boolean;

    fCurrentVideo: IVideo;
    fVideoClip:    IVideo;
    fLyricsSync: TLyricsSyncSource;
    fMusicSync: TMusicSyncSource;
    fTimebarMode: TTimebarMode;

    PlayMidi: boolean;

    StartNote, EndNote:     TPos;

    procedure LoadNextSong();
    procedure UpdateMedleyStats(medley_end: boolean);
    procedure DrawMedleyCountdown();
    procedure SongError();
  protected
    eSongLoaded: THookableEvent; //< event is called after lyrics of a song are loaded on OnShow
    Paused:     boolean; //pause Mod
    NumEmptySentences: array [0..1] of integer;
  public
    removeVoice: boolean;
    fShowWebcam: boolean;

    Act_Level: integer;
    Act_MD5Song: string;

    StaticDuet: array of cardinal;
    ColPlayer:  array[0..3] of TRGB;

    MedleyStart, MedleyEnd: real;

    // timebar fields
    StaticTimeProgress: integer;
    TextTimeText: integer;

    StaticP1: array [0..1] of integer;
    TextP1:   integer;
    StaticP1Avatar: array [0..1] of integer;

    // shown when game is in 2/4 player modus
    StaticP1TwoP: array [0..1] of integer;
    TextP1TwoP:   integer;
    StaticP1TwoPAvatar: array [0..1] of integer;

    // shown when game is in 3/6 player modus
    StaticP1ThreeP: array [0..1] of integer;
    TextP1ThreeP:   integer;
    StaticP1ThreePAvatar: array [0..1] of integer;

    StaticP2R: array [0..1] of integer;
    TextP2R:   integer;
    StaticP2RAvatar: array [0..1] of integer;

    StaticP2M: array [0..1] of integer;
    TextP2M:   integer;
    StaticP2MAvatar: array [0..1] of integer;

    StaticP3R: array [0..1] of integer;
    TextP3R:   integer;
    StaticP3RAvatar: array [0..1] of integer;

    // 4/6 players in one screen
    StaticP1FourP:  integer;
    StaticP2FourP:  integer;
    StaticP3FourP:  integer;
    StaticP4FourP:  integer;

    StaticP1FourPAvatar:  integer;
    StaticP2FourPAvatar:  integer;
    StaticP3FourPAvatar:  integer;
    StaticP4FourPAvatar:  integer;

    TextP1FourP:   integer;
    TextP2FourP:   integer;
    TextP3FourP:   integer;
    TextP4FourP:   integer;

    StaticP1SixP:  integer;
    StaticP2SixP:  integer;
    StaticP3SixP:  integer;
    StaticP4SixP:  integer;
    StaticP5SixP:  integer;
    StaticP6SixP:  integer;

    StaticP1SixPAvatar:  integer;
    StaticP2SixPAvatar:  integer;
    StaticP3SixPAvatar:  integer;
    StaticP4SixPAvatar:  integer;
    StaticP5SixPAvatar:  integer;
    StaticP6SixPAvatar:  integer;

    TextP1SixP:   integer;
    TextP2SixP:   integer;
    TextP3SixP:   integer;
    TextP4SixP:   integer;
    TextP5SixP:   integer;
    TextP6SixP:   integer;

    // 3/6 players duet
    StaticDuetP1ThreeP: array [0..1] of integer;
    TextDuetP1ThreeP:   integer;
    StaticDuetP1ThreePAvatar: array [0..1] of integer;

    StaticDuetP2M: array [0..1] of integer;
    TextDuetP2M:   integer;
    StaticDuetP2MAvatar: array [0..1] of integer;

    StaticDuetP3R: array [0..1] of integer;
    TextDuetP3R:   integer;
    StaticDuetP3RAvatar: array [0..1] of integer;

    // 4/6 players duet one screen
    StaticP1DuetFourP:  integer;
    StaticP2DuetFourP:  integer;
    StaticP3DuetFourP:  integer;
    StaticP4DuetFourP:  integer;

    StaticP1DuetFourPAvatar:  integer;
    StaticP2DuetFourPAvatar:  integer;
    StaticP3DuetFourPAvatar:  integer;
    StaticP4DuetFourPAvatar:  integer;

    TextP1DuetFourP:   integer;
    TextP2DuetFourP:   integer;
    TextP3DuetFourP:   integer;
    TextP4DuetFourP:   integer;

    StaticP1DuetSixP:  integer;
    StaticP2DuetSixP:  integer;
    StaticP3DuetSixP:  integer;
    StaticP4DuetSixP:  integer;
    StaticP5DuetSixP:  integer;
    StaticP6DuetSixP:  integer;

    StaticP1DuetSixPAvatar:  integer;
    StaticP2DuetSixPAvatar:  integer;
    StaticP3DuetSixPAvatar:  integer;
    StaticP4DuetSixPAvatar:  integer;
    StaticP5DuetSixPAvatar:  integer;
    StaticP6DuetSixPAvatar:  integer;

    TextP1DuetSixP:   integer;
    TextP2DuetSixP:   integer;
    TextP3DuetSixP:   integer;
    TextP4DuetSixP:   integer;
    TextP5DuetSixP:   integer;
    TextP6DuetSixP:   integer;


    StaticPausePopup: integer;

    SongNameStatic:   integer;
    SongNameText:     integer;

    Tex_Background: TTexture;
    FadeOut: boolean;

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
    P1Name, P2Name, P3Name, P4Name, P5Name, P6Name: UTF8String;
    P1DuetName, P2DuetName, P3DuetName, P4DuetName, P5DuetName, P6DuetName: UTF8String;
    
    procedure ClearSettings;
    procedure ApplySettings; //< applies changes of settings record
    procedure EndSong;

    constructor Create; override;
    procedure OnShow; override;
    procedure OnShowFinish; override;
    procedure OnHide; override;

    function ParseInput(PressedKey: cardinal; CharCode: UCS4Char;
      PressedDown: boolean): boolean; override;
    function Draw: boolean; override;

    function FinishedMusic: boolean;

    procedure AutoSendScore;
    procedure AutoSaveScore;

    procedure Finish; virtual;
    procedure Pause; // toggle pause

    procedure OnSentenceEnd(CP: integer; SentenceIndex: cardinal);     // for linebonus + singbar
    procedure OnSentenceChange(CP: integer; SentenceIndex: cardinal);  // for golden notes

    procedure SwapToScreen(Screen: integer);

    procedure WriteMessage(msg: UTF8String);
    procedure FadeMessage();
    procedure CloseMessage();

    procedure MedleyTitleFadeOut();

    function GetLyricColor(Color: integer): TRGB;

    procedure DrawInfoLyricBar();
  end;
var
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

function TScreenSing.ParseInput(PressedKey: Cardinal; CharCode: UCS4Char;
  PressedDown: boolean): boolean;
var
  SDL_ModState:  word;
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

// pause mod
procedure TScreenSing.Pause;
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
// pause mod end

//ToDo basisbit: check this again
// Dirty HacK
procedure TScreenSing.SwapToScreen(Screen: integer);
var
  P, I: integer;
begin
  { if screens = 2 and playerplay <= 3 the 2nd screen shows the
    textures of screen 1 }
  if (PlayersPlay <= 3) and (Screen = 2) then
    Screen := 1;

  Statics[StaticP1[0]].Visible := false;
  Statics[StaticP1TwoP[0]].Visible := false;
  Statics[StaticP2R[0]].Visible := false;
  Statics[StaticP1ThreeP[0]].Visible := false;
  Statics[StaticP2M[0]].Visible := false;
  Statics[StaticP3R[0]].Visible := false;
  Statics[StaticP1[1]].Visible := false;
  Statics[StaticP1TwoP[1]].Visible := false;
  Statics[StaticP2R[1]].Visible := false;
  Statics[StaticP1ThreeP[1]].Visible := false;
  Statics[StaticP2M[1]].Visible := false;
  Statics[StaticP3R[1]].Visible := false;

  Statics[StaticP1Avatar[0]].Visible := false;
  Statics[StaticP1TwoPAvatar[0]].Visible := false;
  Statics[StaticP2RAvatar[0]].Visible := false;
  Statics[StaticP1ThreePAvatar[0]].Visible := false;
  Statics[StaticP2MAvatar[0]].Visible := false;
  Statics[StaticP3RAvatar[0]].Visible := false;
  Statics[StaticP1Avatar[1]].Visible := false;
  Statics[StaticP1TwoPAvatar[1]].Visible := false;
  Statics[StaticP2RAvatar[1]].Visible := false;
  Statics[StaticP1ThreePAvatar[1]].Visible := false;
  Statics[StaticP2MAvatar[1]].Visible := false;
  Statics[StaticP3RAvatar[1]].Visible := false;

  Statics[StaticDuetP1ThreeP[0]].Visible := false;
  Statics[StaticDuetP1ThreeP[1]].Visible := false;
  Statics[StaticDuetP2M[0]].Visible := false;
  Statics[StaticDuetP2M[1]].Visible := false;
  Statics[StaticDuetP3R[1]].Visible := false;
  Statics[StaticDuetP3R[0]].Visible := false;

  Statics[StaticDuetP1ThreePAvatar[0]].Visible := false;
  Statics[StaticDuetP1ThreePAvatar[1]].Visible := false;
  Statics[StaticDuetP2MAvatar[0]].Visible := false;
  Statics[StaticDuetP2MAvatar[1]].Visible := false;
  Statics[StaticDuetP3RAvatar[1]].Visible := false;
  Statics[StaticDuetP3RAvatar[0]].Visible := false;

  // 4/6 players in one screen
  Statics[StaticP1FourP].Visible := false;
  Statics[StaticP2FourP].Visible := false;
  Statics[StaticP3FourP].Visible := false;
  Statics[StaticP4FourP].Visible := false;

  Statics[StaticP1FourPAvatar].Visible := false;
  Statics[StaticP2FourPAvatar].Visible := false;
  Statics[StaticP3FourPAvatar].Visible := false;
  Statics[StaticP4FourPAvatar].Visible := false;

  Statics[StaticP1DuetFourP].Visible := false;
  Statics[StaticP2DuetFourP].Visible := false;
  Statics[StaticP3DuetFourP].Visible := false;
  Statics[StaticP4DuetFourP].Visible := false;

  Statics[StaticP1DuetFourPAvatar].Visible := false;
  Statics[StaticP2DuetFourPAvatar].Visible := false;
  Statics[StaticP3DuetFourPAvatar].Visible := false;
  Statics[StaticP4DuetFourPAvatar].Visible := false;

  Statics[StaticP1SixP].Visible := false;
  Statics[StaticP2SixP].Visible := false;
  Statics[StaticP3SixP].Visible := false;
  Statics[StaticP4SixP].Visible := false;
  Statics[StaticP5SixP].Visible := false;
  Statics[StaticP6SixP].Visible := false;

  Statics[StaticP1SixPAvatar].Visible := false;
  Statics[StaticP2SixPAvatar].Visible := false;
  Statics[StaticP3SixPAvatar].Visible := false;
  Statics[StaticP4SixPAvatar].Visible := false;
  Statics[StaticP5SixPAvatar].Visible := false;
  Statics[StaticP6SixPAvatar].Visible := false;

  Statics[StaticP1DuetSixP].Visible := false;
  Statics[StaticP2DuetSixP].Visible := false;
  Statics[StaticP3DuetSixP].Visible := false;
  Statics[StaticP4DuetSixP].Visible := false;
  Statics[StaticP5DuetSixP].Visible := false;
  Statics[StaticP6DuetSixP].Visible := false;

  Statics[StaticP1DuetSixPAvatar].Visible := false;
  Statics[StaticP2DuetSixPAvatar].Visible := false;
  Statics[StaticP3DuetSixPAvatar].Visible := false;
  Statics[StaticP4DuetSixPAvatar].Visible := false;
  Statics[StaticP5DuetSixPAvatar].Visible := false;
  Statics[StaticP6DuetSixPAvatar].Visible := false;

  if (PlayersPlay = 1) then
  begin
    if (Screen = 2) then
    begin
      Statics[StaticP1[0]].Visible := true;

      Statics[StaticP1Avatar[0]].Visible := true;
    end;

    if (Screen = 1) then
    begin
      Statics[StaticP1[0]].Visible := true;

      Statics[StaticP1Avatar[0]].Visible := true;
    end;
  end;

  if (PlayersPlay = 2) or ((PlayersPlay = 4) and (Ini.Screens = 1)) then
  begin
    if (Screen = 2) then
    begin
      Statics[StaticP1TwoP[1]].Visible := true;
      Statics[StaticP2R[1]].Visible := true;

      Statics[StaticP1TwoPAvatar[1]].Visible := true;
      Statics[StaticP2RAvatar[1]].Visible := true;
    end;

    if (Screen = 1) then
    begin
      Statics[StaticP1TwoP[0]].Visible := true;
      Statics[StaticP2R[0]].Visible := true;

      Statics[StaticP1TwoPAvatar[0]].Visible := true;
      Statics[StaticP2RAvatar[0]].Visible := true;
    end;
  end;

  if (PlayersPlay = 3) or ((PlayersPlay = 6) and (Ini.Screens = 1)) then
  begin
    if (CurrentSong.isDuet) then
    begin
      if (Screen = 2) then
      begin
        Statics[StaticDuetP1ThreeP[1]].Visible := true;
        Statics[StaticDuetP2M[1]].Visible := true;
        Statics[StaticDuetP3R[1]].Visible := true;

        Statics[StaticDuetP1ThreePAvatar[1]].Visible := true;
        Statics[StaticDuetP2MAvatar[1]].Visible := true;
        Statics[StaticDuetP3RAvatar[1]].Visible := true;
      end;

      if (Screen = 1) then
      begin
        Statics[StaticDuetP1ThreeP[0]].Visible := true;
        Statics[StaticDuetP2M[0]].Visible := true;
        Statics[StaticDuetP3R[0]].Visible := true;

        Statics[StaticDuetP1ThreePAvatar[0]].Visible := true;
        Statics[StaticDuetP2MAvatar[0]].Visible := true;
        Statics[StaticDuetP3RAvatar[0]].Visible := true;
      end;
    end
    else
    begin
      if (Screen = 2) then
      begin
        Statics[StaticP1ThreeP[1]].Visible := true;
        Statics[StaticP2M[1]].Visible := true;
        Statics[StaticP3R[1]].Visible := true;

        Statics[StaticP1ThreePAvatar[1]].Visible := true;
        Statics[StaticP2MAvatar[1]].Visible := true;
        Statics[StaticP3RAvatar[1]].Visible := true;
      end;

      if (Screen = 1) then
      begin
        Statics[StaticP1ThreeP[0]].Visible := true;
        Statics[StaticP2M[0]].Visible := true;
        Statics[StaticP3R[0]].Visible := true;

        Statics[StaticP1ThreePAvatar[0]].Visible := true;
        Statics[StaticP2MAvatar[0]].Visible := true;
        Statics[StaticP3RAvatar[0]].Visible := true;
      end;
    end;
  end;

  // 4 Players in 1 Screen
  if (PlayersPlay = 4) and (Ini.Screens = 0) then
  begin
    if (CurrentSong.isDuet) then
    begin
      Statics[StaticP1DuetFourP].Visible := true;
      Statics[StaticP2DuetFourP].Visible := true;
      Statics[StaticP3DuetFourP].Visible := true;
      Statics[StaticP4DuetFourP].Visible := true;

      Statics[StaticP1DuetFourPAvatar].Visible := true;
      Statics[StaticP2DuetFourPAvatar].Visible := true;
      Statics[StaticP3DuetFourPAvatar].Visible := true;
      Statics[StaticP4DuetFourPAvatar].Visible := true;
    end
    else
    begin
      Statics[StaticP1FourP].Visible := true;
      Statics[StaticP2FourP].Visible := true;
      Statics[StaticP3FourP].Visible := true;
      Statics[StaticP4FourP].Visible := true;

      Statics[StaticP1FourPAvatar].Visible := true;
      Statics[StaticP2FourPAvatar].Visible := true;
      Statics[StaticP3FourPAvatar].Visible := true;
      Statics[StaticP4FourPAvatar].Visible := true;
    end;
  end;

  // 6 Players in 1 Screen
  if (PlayersPlay = 6) and (Ini.Screens = 0) then
  begin
    if (CurrentSong.isDuet) then
    begin
      Statics[StaticP1DuetSixP].Visible := true;
      Statics[StaticP2DuetSixP].Visible := true;
      Statics[StaticP3DuetSixP].Visible := true;
      Statics[StaticP4DuetSixP].Visible := true;
      Statics[StaticP5DuetSixP].Visible := true;
      Statics[StaticP6DuetSixP].Visible := true;

      Statics[StaticP1DuetSixPAvatar].Visible := true;
      Statics[StaticP2DuetSixPAvatar].Visible := true;
      Statics[StaticP3DuetSixPAvatar].Visible := true;
      Statics[StaticP4DuetSixPAvatar].Visible := true;
      Statics[StaticP5DuetSixPAvatar].Visible := true;
      Statics[StaticP6DuetSixPAvatar].Visible := true;
    end
    else
    begin
      Statics[StaticP1SixP].Visible := true;
      Statics[StaticP2SixP].Visible := true;
      Statics[StaticP3SixP].Visible := true;
      Statics[StaticP4SixP].Visible := true;
      Statics[StaticP5SixP].Visible := true;
      Statics[StaticP6SixP].Visible := true;

      Statics[StaticP1SixPAvatar].Visible := true;
      Statics[StaticP2SixPAvatar].Visible := true;
      Statics[StaticP3SixPAvatar].Visible := true;
      Statics[StaticP4SixPAvatar].Visible := true;
      Statics[StaticP5SixPAvatar].Visible := true;
      Statics[StaticP6SixPAvatar].Visible := true;
    end;
  end;

end;

constructor TScreenSing.Create;
var
  Col: array [1..6] of TRGB;
  I: integer;
  Color: cardinal;
begin
  inherited Create;

  //too dangerous, a mouse button is quickly pressed by accident
  RightMbESC := false;

  fShowVisualization := false;
  fShowWebcam := false;
  fShowBackground := false;

  fCurrentVideo := nil;

  // create score class
  Scores := TSingScores.Create;
  Scores.LoadfromTheme;

  LoadFromTheme(Theme.Sing);

  SetLength(StaticDuet, Length(Theme.Sing.StaticDuet));
  for i := 0 to High(StaticDuet) do
    StaticDuet[i] := AddStatic(Theme.Sing.StaticDuet[i]);

  // timebar
  StaticTimeProgress := AddStatic(Theme.Sing.StaticTimeProgress);
  TextTimeText := AddText(Theme.Sing.TextTimeText);

  for I := 1 to 6 do
    Col[I] := GetPlayerColor(Ini.SingColor[I - 1]);

  // SCREEN 1
  // 1 player       | P1
  Theme.Sing.StaticP1.ColR := Col[1].R;
  Theme.Sing.StaticP1.ColG := Col[1].G;
  Theme.Sing.StaticP1.ColB := Col[1].B;

  // 2 or 4 players | P1
  Theme.Sing.StaticP1TwoP.ColR := Col[1].R;
  Theme.Sing.StaticP1TwoP.ColG := Col[1].G;
  Theme.Sing.StaticP1TwoP.ColB := Col[1].B;

  //                | P2
  Theme.Sing.StaticP2R.ColR := Col[2].R;
  Theme.Sing.StaticP2R.ColG := Col[2].G;
  Theme.Sing.StaticP2R.ColB := Col[2].B;

  // 3 or 6 players | P1
  Theme.Sing.StaticP1ThreeP.ColR := Col[1].R;
  Theme.Sing.StaticP1ThreeP.ColG := Col[1].G;
  Theme.Sing.StaticP1ThreeP.ColB := Col[1].B;

  //                | P2
  Theme.Sing.StaticP2M.ColR := Col[2].R;
  Theme.Sing.StaticP2M.ColG := Col[2].G;
  Theme.Sing.StaticP2M.ColB := Col[2].B;

  //                | P3

  Theme.Sing.StaticP3R.ColR := Col[3].R;
  Theme.Sing.StaticP3R.ColG := Col[3].G;
  Theme.Sing.StaticP3R.ColB := Col[3].B;

  // 3 or 6 players | P1 DUET
  Theme.Sing.StaticDuetP1ThreeP.ColR := Col[1].R;
  Theme.Sing.StaticDuetP1ThreeP.ColG := Col[1].G;
  Theme.Sing.StaticDuetP1ThreeP.ColB := Col[1].B;

  //                | P2 DUET
  Theme.Sing.StaticDuetP2M.ColR := Col[2].R;
  Theme.Sing.StaticDuetP2M.ColG := Col[2].G;
  Theme.Sing.StaticDuetP2M.ColB := Col[2].B;

  //                | P3 DUET

  Theme.Sing.StaticDuetP3R.ColR := Col[3].R;
  Theme.Sing.StaticDuetP3R.ColG := Col[3].G;
  Theme.Sing.StaticDuetP3R.ColB := Col[3].B;

  StaticP1[0]       := AddStatic(Theme.Sing.StaticP1);
  StaticP1TwoP[0]   := AddStatic(Theme.Sing.StaticP1TwoP);
  StaticP2R[0]      := AddStatic(Theme.Sing.StaticP2R);
  StaticP1ThreeP[0] := AddStatic(Theme.Sing.StaticP1ThreeP);
  StaticP2M[0]      := AddStatic(Theme.Sing.StaticP2M);
  StaticP3R[0]      := AddStatic(Theme.Sing.StaticP3R);
  StaticDuetP1ThreeP[0] := AddStatic(Theme.Sing.StaticDuetP1ThreeP);
  StaticDuetP2M[0]      := AddStatic(Theme.Sing.StaticDuetP2M);
  StaticDuetP3R[0]      := AddStatic(Theme.Sing.StaticDuetP3R);

  // SCREEN 2
  // 1 player       | P1
  Theme.Sing.StaticP1.ColR := Col[1].R;
  Theme.Sing.StaticP1.ColG := Col[1].G;
  Theme.Sing.StaticP1.ColB := Col[1].B;

  // 2 or 4 players | P1
  Theme.Sing.StaticP1TwoP.ColR := Col[3].R;
  Theme.Sing.StaticP1TwoP.ColG := Col[3].G;
  Theme.Sing.StaticP1TwoP.ColB := Col[3].B;

  //                | P2
  Theme.Sing.StaticP2R.ColR := Col[4].R;
  Theme.Sing.StaticP2R.ColG := Col[4].G;
  Theme.Sing.StaticP2R.ColB := Col[4].B;

  // 3 or 6 players | P1
  Theme.Sing.StaticP1ThreeP.ColR := Col[4].R;
  Theme.Sing.StaticP1ThreeP.ColG := Col[4].G;
  Theme.Sing.StaticP1ThreeP.ColB := Col[4].B;

  //                | P2
  Theme.Sing.StaticP2M.ColR := Col[5].R;
  Theme.Sing.StaticP2M.ColG := Col[5].G;
  Theme.Sing.StaticP2M.ColB := Col[5].B;

  //                | P3
  Theme.Sing.StaticP3R.ColR := Col[6].R;
  Theme.Sing.StaticP3R.ColG := Col[6].G;
  Theme.Sing.StaticP3R.ColB := Col[6].B;

    // 3 or 6 players | P1 DUET
  Theme.Sing.StaticDuetP1ThreeP.ColR := Col[4].R;
  Theme.Sing.StaticDuetP1ThreeP.ColG := Col[4].G;
  Theme.Sing.StaticDuetP1ThreeP.ColB := Col[4].B;

  //                | P2 DUET
  Theme.Sing.StaticDuetP2M.ColR := Col[5].R;
  Theme.Sing.StaticDuetP2M.ColG := Col[5].G;
  Theme.Sing.StaticDuetP2M.ColB := Col[5].B;

  //                | P3 DUET
  Theme.Sing.StaticDuetP3R.ColR := Col[6].R;
  Theme.Sing.StaticDuetP3R.ColG := Col[6].G;
  Theme.Sing.StaticDuetP3R.ColB := Col[6].B;

  StaticP1[1]       := AddStatic(Theme.Sing.StaticP1);
  StaticP1TwoP[1]   := AddStatic(Theme.Sing.StaticP1TwoP);
  StaticP2R[1]      := AddStatic(Theme.Sing.StaticP2R);
  StaticP1ThreeP[1] := AddStatic(Theme.Sing.StaticP1ThreeP);
  StaticP2M[1]      := AddStatic(Theme.Sing.StaticP2M);
  StaticP3R[1]      := AddStatic(Theme.Sing.StaticP3R);
  StaticDuetP1ThreeP[1] := AddStatic(Theme.Sing.StaticDuetP1ThreeP);
  StaticDuetP2M[1]      := AddStatic(Theme.Sing.StaticDuetP2M);
  StaticDuetP3R[1]      := AddStatic(Theme.Sing.StaticDuetP3R);

  TextP1   := AddText(Theme.Sing.TextP1);
  TextP1TwoP   := AddText(Theme.Sing.TextP1TwoP);
  TextP2R   := AddText(Theme.Sing.TextP2R);
  TextP1ThreeP   := AddText(Theme.Sing.TextP1ThreeP);
  TextP2M   := AddText(Theme.Sing.TextP2M);
  TextP3R   := AddText(Theme.Sing.TextP3R);
  TextDuetP1ThreeP   := AddText(Theme.Sing.TextDuetP1ThreeP);
  TextDuetP2M   := AddText(Theme.Sing.TextDuetP2M);
  TextDuetP3R   := AddText(Theme.Sing.TextDuetP3R);

  if (PlayersPlay = 1) then
  begin
    if (Party.bPartyGame) then
    begin
      if (Text[TextP1].Text = 'PLAYERNAME') then
        P1Name := Ini.NameTeam[0]
      else
        P1Name := 'P1';
    end
    else
    begin
      if (Text[TextP1].Text = 'PLAYERNAME') then
        P1Name := Ini.Name[0]
      else
        P1Name := 'P1';
    end;
  end;

  if (PlayersPlay = 2) then
  begin
    if (Party.bPartyGame) then
    begin
      if (Text[TextP1TwoP].Text = 'PLAYERNAME') then
        P1Name := Ini.NameTeam[0]
      else
        P1Name := 'P1';

      if (Text[TextP2R].Text = 'PLAYERNAME') then
        P2Name := Ini.NameTeam[1]
      else
        P2Name := 'P2';
    end
    else
    begin
      if (Text[TextP1TwoP].Text = 'PLAYERNAME') then
        P1Name := Ini.Name[0]
      else
        P1Name := 'P1';

      if (Text[TextP2R].Text = 'PLAYERNAME') then
        P2Name := Ini.Name[1]
      else
        P2Name := 'P2';
    end;
  end;

  if (PlayersPlay = 3) then
  begin
    if (Party.bPartyGame) then
    begin

      if (Text[TextDuetP1ThreeP].Text = 'PLAYERNAME') then
        P1DuetName := Ini.NameTeam[0]
      else
        P1DuetName := 'P1';

      if (Text[TextDuetP2M].Text = 'PLAYERNAME') then
        P2DuetName := Ini.NameTeam[1]
      else
        P2DuetName := 'P2';

      if (Text[TextDuetP3R].Text = 'PLAYERNAME') then
        P3DuetName := Ini.NameTeam[2]
      else
        P3DuetName := 'P3';

      if (Text[TextP1ThreeP].Text = 'PLAYERNAME') then
        P1Name := Ini.NameTeam[0]
      else
        P1Name := 'P1';

      if (Text[TextP2M].Text = 'PLAYERNAME') then
        P2Name := Ini.NameTeam[1]
      else
        P2Name := 'P2';

      if (Text[TextP3R].Text = 'PLAYERNAME') then
        P3Name := Ini.NameTeam[2]
      else
        P3Name := 'P3';
    end
    else
    begin
      if (Text[TextDuetP1ThreeP].Text = 'PLAYERNAME') then
        P1DuetName := Ini.Name[0]
      else
        P1DuetName := 'P1';

      if (Text[TextDuetP2M].Text = 'PLAYERNAME') then
        P2DuetName := Ini.Name[1]
      else
        P2DuetName := 'P2';

      if (Text[TextDuetP3R].Text = 'PLAYERNAME') then
        P3DuetName := Ini.Name[2]
      else
        P3DuetName := 'P3';

      if (Text[TextP1ThreeP].Text = 'PLAYERNAME') then
        P1Name := Ini.Name[0]
      else
        P1Name := 'P1';

      if (Text[TextP2M].Text = 'PLAYERNAME') then
        P2Name := Ini.Name[1]
      else
        P2Name := 'P2';

      if (Text[TextP3R].Text = 'PLAYERNAME') then
        P3Name := Ini.Name[2]
      else
        P3Name := 'P3';
    end;
  end;

  // 4/6 players in 1 screen
  // P1
  Theme.Sing.StaticP1FourP.ColR := Col[1].R;
  Theme.Sing.StaticP1FourP.ColG := Col[1].G;
  Theme.Sing.StaticP1FourP.ColB := Col[1].B;

  // P2
  Theme.Sing.StaticP2FourP.ColR := Col[2].R;
  Theme.Sing.StaticP2FourP.ColG := Col[2].G;
  Theme.Sing.StaticP2FourP.ColB := Col[2].B;

  // P3
  Theme.Sing.StaticP3FourP.ColR := Col[3].R;
  Theme.Sing.StaticP3FourP.ColG := Col[3].G;
  Theme.Sing.StaticP3FourP.ColB := Col[3].B;

  // P4
  Theme.Sing.StaticP4FourP.ColR := Col[4].R;
  Theme.Sing.StaticP4FourP.ColG := Col[4].G;
  Theme.Sing.StaticP4FourP.ColB := Col[4].B;

  StaticP1FourP   := AddStatic(Theme.Sing.StaticP1FourP);
  StaticP2FourP   := AddStatic(Theme.Sing.StaticP2FourP);
  StaticP3FourP   := AddStatic(Theme.Sing.StaticP3FourP);
  StaticP4FourP   := AddStatic(Theme.Sing.StaticP4FourP);

  TextP1FourP   := AddText(Theme.Sing.TextP1FourP);
  TextP2FourP   := AddText(Theme.Sing.TextP2FourP);
  TextP3FourP   := AddText(Theme.Sing.TextP3FourP);
  TextP4FourP   := AddText(Theme.Sing.TextP4FourP);

  // P1
  Theme.Sing.StaticP1SixP.ColR := Col[1].R;
  Theme.Sing.StaticP1SixP.ColG := Col[1].G;
  Theme.Sing.StaticP1SixP.ColB := Col[1].B;

  // P2
  Theme.Sing.StaticP2SixP.ColR := Col[2].R;
  Theme.Sing.StaticP2SixP.ColG := Col[2].G;
  Theme.Sing.StaticP2SixP.ColB := Col[2].B;

  // P3
  Theme.Sing.StaticP3SixP.ColR := Col[3].R;
  Theme.Sing.StaticP3SixP.ColG := Col[3].G;
  Theme.Sing.StaticP3SixP.ColB := Col[3].B;

  // P4
  Theme.Sing.StaticP4SixP.ColR := Col[4].R;
  Theme.Sing.StaticP4SixP.ColG := Col[4].G;
  Theme.Sing.StaticP4SixP.ColB := Col[4].B;

  // P5
  Theme.Sing.StaticP5SixP.ColR := Col[5].R;
  Theme.Sing.StaticP5SixP.ColG := Col[5].G;
  Theme.Sing.StaticP5SixP.ColB := Col[5].B;

  // P6
  Theme.Sing.StaticP6SixP.ColR := Col[6].R;
  Theme.Sing.StaticP6SixP.ColG := Col[6].G;
  Theme.Sing.StaticP6SixP.ColB := Col[6].B;

  StaticP1SixP  := AddStatic(Theme.Sing.StaticP1SixP);
  StaticP2SixP  := AddStatic(Theme.Sing.StaticP2SixP);
  StaticP3SixP  := AddStatic(Theme.Sing.StaticP3SixP);
  StaticP4SixP  := AddStatic(Theme.Sing.StaticP4SixP);
  StaticP5SixP  := AddStatic(Theme.Sing.StaticP5SixP);
  StaticP6SixP  := AddStatic(Theme.Sing.StaticP6SixP);

  TextP1SixP   := AddText(Theme.Sing.TextP1SixP);
  TextP2SixP   := AddText(Theme.Sing.TextP2SixP);
  TextP3SixP   := AddText(Theme.Sing.TextP3SixP);
  TextP4SixP   := AddText(Theme.Sing.TextP4SixP);
  TextP5SixP   := AddText(Theme.Sing.TextP5SixP);
  TextP6SixP   := AddText(Theme.Sing.TextP6SixP);


  // 4/6 players duet in 1 screen
  // P1
  Theme.Sing.StaticP1DuetFourP.ColR := Col[1].R;
  Theme.Sing.StaticP1DuetFourP.ColG := Col[1].G;
  Theme.Sing.StaticP1DuetFourP.ColB := Col[1].B;

  // P2
  Theme.Sing.StaticP2DuetFourP.ColR := Col[2].R;
  Theme.Sing.StaticP2DuetFourP.ColG := Col[2].G;
  Theme.Sing.StaticP2DuetFourP.ColB := Col[2].B;

  // P3
  Theme.Sing.StaticP3DuetFourP.ColR := Col[3].R;
  Theme.Sing.StaticP3DuetFourP.ColG := Col[3].G;
  Theme.Sing.StaticP3DuetFourP.ColB := Col[3].B;

  // P4
  Theme.Sing.StaticP4DuetFourP.ColR := Col[4].R;
  Theme.Sing.StaticP4DuetFourP.ColG := Col[4].G;
  Theme.Sing.StaticP4DuetFourP.ColB := Col[4].B;

  StaticP1DuetFourP   := AddStatic(Theme.Sing.StaticP1DuetFourP);
  StaticP2DuetFourP   := AddStatic(Theme.Sing.StaticP2DuetFourP);
  StaticP3DuetFourP   := AddStatic(Theme.Sing.StaticP3DuetFourP);
  StaticP4DuetFourP   := AddStatic(Theme.Sing.StaticP4DuetFourP);

  TextP1DuetFourP   := AddText(Theme.Sing.TextP1DuetFourP);
  TextP2DuetFourP   := AddText(Theme.Sing.TextP2DuetFourP);
  TextP3DuetFourP   := AddText(Theme.Sing.TextP3DuetFourP);
  TextP4DuetFourP   := AddText(Theme.Sing.TextP4DuetFourP);

  // P1
  Theme.Sing.StaticP1DuetSixP.ColR := Col[1].R;
  Theme.Sing.StaticP1DuetSixP.ColG := Col[1].G;
  Theme.Sing.StaticP1DuetSixP.ColB := Col[1].B;

  // P2
  Theme.Sing.StaticP2DuetSixP.ColR := Col[2].R;
  Theme.Sing.StaticP2DuetSixP.ColG := Col[2].G;
  Theme.Sing.StaticP2DuetSixP.ColB := Col[2].B;

  // P3
  Theme.Sing.StaticP3DuetSixP.ColR := Col[3].R;
  Theme.Sing.StaticP3DuetSixP.ColG := Col[3].G;
  Theme.Sing.StaticP3DuetSixP.ColB := Col[3].B;

  // P4
  Theme.Sing.StaticP4DuetSixP.ColR := Col[4].R;
  Theme.Sing.StaticP4DuetSixP.ColG := Col[4].G;
  Theme.Sing.StaticP4DuetSixP.ColB := Col[4].B;

  // P5
  Theme.Sing.StaticP5DuetSixP.ColR := Col[5].R;
  Theme.Sing.StaticP5DuetSixP.ColG := Col[5].G;
  Theme.Sing.StaticP5DuetSixP.ColB := Col[5].B;

  // P6
  Theme.Sing.StaticP6DuetSixP.ColR := Col[6].R;
  Theme.Sing.StaticP6DuetSixP.ColG := Col[6].G;
  Theme.Sing.StaticP6DuetSixP.ColB := Col[6].B;

  StaticP1DuetSixP  := AddStatic(Theme.Sing.StaticP1DuetSixP);
  StaticP2DuetSixP  := AddStatic(Theme.Sing.StaticP2DuetSixP);
  StaticP3DuetSixP  := AddStatic(Theme.Sing.StaticP3DuetSixP);
  StaticP4DuetSixP  := AddStatic(Theme.Sing.StaticP4DuetSixP);
  StaticP5DuetSixP  := AddStatic(Theme.Sing.StaticP5DuetSixP);
  StaticP6DuetSixP  := AddStatic(Theme.Sing.StaticP6DuetSixP);

  TextP1DuetSixP   := AddText(Theme.Sing.TextP1DuetSixP);
  TextP2DuetSixP   := AddText(Theme.Sing.TextP2DuetSixP);
  TextP3DuetSixP   := AddText(Theme.Sing.TextP3DuetSixP);
  TextP4DuetSixP   := AddText(Theme.Sing.TextP4DuetSixP);
  TextP5DuetSixP   := AddText(Theme.Sing.TextP5DuetSixP);
  TextP6DuetSixP   := AddText(Theme.Sing.TextP6DuetSixP);

  if (PlayersPlay = 4) then
  begin
    if (Text[TextP1DuetFourP].Text = 'PLAYERNAME') then
      P1DuetName := Ini.Name[0]
    else
      P1DuetName := 'P1';

    if (Text[TextP2DuetFourP].Text = 'PLAYERNAME') then
      P2DuetName := Ini.Name[1]
    else
      P2DuetName := 'P2';

    if (Text[TextP3DuetFourP].Text = 'PLAYERNAME') then
      P3DuetName := Ini.Name[2]
    else
      P3DuetName := 'P3';

    if (Text[TextP4DuetFourP].Text = 'PLAYERNAME') then
      P4DuetName := Ini.Name[3]
    else
      P4DuetName := 'P4';

    if (Text[TextP1FourP].Text = 'PLAYERNAME') then
      P1Name := Ini.Name[0]
    else
      P1Name := 'P1';

    if (Text[TextP2FourP].Text = 'PLAYERNAME') then
      P2Name := Ini.Name[1]
    else
      P2Name := 'P2';

    if (Text[TextP3FourP].Text = 'PLAYERNAME') then
      P3Name := Ini.Name[2]
    else
      P3Name := 'P3';

    if (Text[TextP4FourP].Text = 'PLAYERNAME') then
      P4Name := Ini.Name[3]
    else
      P4Name := 'P4';
  end;

  if (PlayersPlay = 6) then
  begin
    if (Text[TextP1DuetSixP].Text = 'PLAYERNAME') then
      P1DuetName := Ini.Name[0]
    else
      P1DuetName := 'P1';

    if (Text[TextP2DuetSixP].Text = 'PLAYERNAME') then
      P2DuetName := Ini.Name[1]
    else
      P2DuetName := 'P2';

    if (Text[TextP3DuetSixP].Text = 'PLAYERNAME') then
      P3DuetName := Ini.Name[2]
    else
      P3DuetName := 'P3';

    if (Text[TextP4DuetSixP].Text = 'PLAYERNAME') then
      P4DuetName := Ini.Name[3]
    else
      P4DuetName := 'P4';

    if (Text[TextP5DuetSixP].Text = 'PLAYERNAME') then
      P5DuetName := Ini.Name[4]
    else
      P5DuetName := 'P5';

    if (Text[TextP6DuetSixP].Text = 'PLAYERNAME') then
      P6DuetName := Ini.Name[5]
    else
      P6DuetName := 'P6';

    if (Text[TextP1SixP].Text = 'PLAYERNAME') then
      P1Name := Ini.Name[0]
    else
      P1Name := 'P1';

    if (Text[TextP2SixP].Text = 'PLAYERNAME') then
      P2Name := Ini.Name[1]
    else
      P2Name := 'P2';

    if (Text[TextP3SixP].Text = 'PLAYERNAME') then
      P3Name := Ini.Name[2]
    else
      P3Name := 'P3';

    if (Text[TextP4SixP].Text = 'PLAYERNAME') then
      P4Name := Ini.Name[3]
    else
      P4Name := 'P4';

    if (Text[TextP5SixP].Text = 'PLAYERNAME') then
      P5Name := Ini.Name[4]
    else
      P5Name := 'P5';

    if (Text[TextP6SixP].Text = 'PLAYERNAME') then
      P6Name := Ini.Name[5]
    else
      P6Name := 'P6';
  end;

  // Sing Bars
  // P1-6
  for I := 1 to 6 do
  begin
    Color := RGBFloatToInt(Col[I].R, Col[I].G, Col[I].B);

	// Color := $002222; //light blue
  // Color := $10000 * Round(0.22*255) + $100 * Round(0.39*255) + Round(0.64*255); //dark blue

    Tex_Left[I]         := Texture.LoadTexture(Skin.GetTextureFileName('GrayLeft'),  TEXTURE_TYPE_COLORIZED, Color);
    Tex_Mid[I]          := Texture.LoadTexture(Skin.GetTextureFileName('GrayMid'),   TEXTURE_TYPE_COLORIZED, Color);
    Tex_Right[I]        := Texture.LoadTexture(Skin.GetTextureFileName('GrayRight'), TEXTURE_TYPE_COLORIZED, Color);

    Tex_plain_Left[I]   := Texture.LoadTexture(Skin.GetTextureFileName('NotePlainLeft'),  TEXTURE_TYPE_COLORIZED, Color);
    Tex_plain_Mid[I]    := Texture.LoadTexture(Skin.GetTextureFileName('NotePlainMid'),   TEXTURE_TYPE_COLORIZED, Color);
    Tex_plain_Right[I]  := Texture.LoadTexture(Skin.GetTextureFileName('NotePlainRight'), TEXTURE_TYPE_COLORIZED, Color);

    Tex_BG_Left[I]      := Texture.LoadTexture(Skin.GetTextureFileName('NoteBGLeft'),  TEXTURE_TYPE_COLORIZED, Color);
    Tex_BG_Mid[I]       := Texture.LoadTexture(Skin.GetTextureFileName('NoteBGMid'),   TEXTURE_TYPE_COLORIZED, Color);
    Tex_BG_Right[I]     := Texture.LoadTexture(Skin.GetTextureFileName('NoteBGRight'), TEXTURE_TYPE_COLORIZED, Color);

    //## backgrounds for the scores ##
    Tex_ScoreBG[I - 1] := Texture.LoadTexture(Skin.GetTextureFileName('ScoreBG'), TEXTURE_TYPE_COLORIZED, Color);
  end;

  StaticPausePopup := AddStatic(Theme.Sing.PausePopUp);

  // <note> pausepopup is not visible at the beginning </note>
  Statics[StaticPausePopup].Visible := false;

  Lyrics := TLyricEngine.Create(
      Theme.LyricBar.UpperX, Theme.LyricBar.UpperY, Theme.LyricBar.UpperW, Theme.LyricBar.UpperH,
      Theme.LyricBar.LowerX, Theme.LyricBar.LowerY, Theme.LyricBar.LowerW, Theme.LyricBar.LowerH);

  LyricsDuetP1 := TLyricEngine.Create(
      Theme.LyricBarDuetP1.UpperX, Theme.LyricBarDuetP1.UpperY, Theme.LyricBarDuetP1.UpperW, Theme.LyricBarDuetP1.UpperH,
      Theme.LyricBarDuetP1.LowerX, Theme.LyricBarDuetP1.LowerY, Theme.LyricBarDuetP1.LowerW, Theme.LyricBarDuetP1.LowerH);

  LyricsDuetP2 := TLyricEngine.Create(
      Theme.LyricBarDuetP2.UpperX, Theme.LyricBarDuetP2.UpperY, Theme.LyricBarDuetP2.UpperW, Theme.LyricBarDuetP2.UpperH,
      Theme.LyricBarDuetP2.LowerX, Theme.LyricBarDuetP2.LowerY, Theme.LyricBarDuetP2.LowerW, Theme.LyricBarDuetP2.LowerH);

  fLyricsSync := TLyricsSyncSource.Create();
  fMusicSync := TMusicSyncSource.Create();

  SongNameStatic := AddStatic(Theme.Sing.StaticSongName);;
  SongNameText := AddText(Theme.Sing.TextSongName);

  eSongLoaded := THookableEvent.Create('ScreenSing.SongLoaded');

  // Info Message
  InfoMessageBG := AddStatic(Theme.Sing.InfoMessageBG);
  InfoMessageText := AddText(Theme.Sing.InfoMessageText);

  // avatars
  StaticP1Avatar[0] := AddStatic(Theme.Sing.StaticP1Avatar);
  Statics[StaticP1Avatar[0]].Texture := AvatarPlayerTextures[1];
  Statics[StaticP1Avatar[0]].Texture.X  := Theme.Sing.StaticP1Avatar.X;
  Statics[StaticP1Avatar[0]].Texture.Y  := Theme.Sing.StaticP1Avatar.Y;
  Statics[StaticP1Avatar[0]].Texture.H  := Theme.Sing.StaticP1Avatar.H;
  Statics[StaticP1Avatar[0]].Texture.W  := Theme.Sing.StaticP1Avatar.W;
  Statics[StaticP1Avatar[0]].Texture.Z := Theme.Sing.StaticP1Avatar.Z;
  Statics[StaticP1Avatar[0]].Texture.Alpha := Theme.Sing.StaticP1Avatar.Alpha;

  StaticP1Avatar[1] := AddStatic(Theme.Sing.StaticP1Avatar);
  Statics[StaticP1Avatar[1]].Texture := AvatarPlayerTextures[1];
  Statics[StaticP1Avatar[1]].Texture.X  := Theme.Sing.StaticP1Avatar.X;
  Statics[StaticP1Avatar[1]].Texture.Y  := Theme.Sing.StaticP1Avatar.Y;
  Statics[StaticP1Avatar[1]].Texture.H  := Theme.Sing.StaticP1Avatar.H;
  Statics[StaticP1Avatar[1]].Texture.W  := Theme.Sing.StaticP1Avatar.W;
  Statics[StaticP1Avatar[1]].Texture.Z := Theme.Sing.StaticP1Avatar.Z;
  Statics[StaticP1Avatar[1]].Texture.Alpha := Theme.Sing.StaticP1Avatar.Alpha;

  StaticP1TwoPAvatar[0] := AddStatic(Theme.Sing.StaticP1TwoPAvatar);
  Statics[StaticP1TwoPAvatar[0]].Texture := AvatarPlayerTextures[1];
  Statics[StaticP1TwoPAvatar[0]].Texture.X  := Theme.Sing.StaticP1TwoPAvatar.X;
  Statics[StaticP1TwoPAvatar[0]].Texture.Y  := Theme.Sing.StaticP1TwoPAvatar.Y;
  Statics[StaticP1TwoPAvatar[0]].Texture.H  := Theme.Sing.StaticP1TwoPAvatar.H;
  Statics[StaticP1TwoPAvatar[0]].Texture.W  := Theme.Sing.StaticP1TwoPAvatar.W;
  Statics[StaticP1TwoPAvatar[0]].Texture.Z := Theme.Sing.StaticP1TwoPAvatar.Z;
  Statics[StaticP1TwoPAvatar[0]].Texture.Alpha := Theme.Sing.StaticP1TwoPAvatar.Alpha;

  StaticP1TwoPAvatar[1] := AddStatic(Theme.Sing.StaticP1TwoPAvatar);
  Statics[StaticP1TwoPAvatar[1]].Texture := AvatarPlayerTextures[1];
  Statics[StaticP1TwoPAvatar[1]].Texture.X  := Theme.Sing.StaticP1TwoPAvatar.X;
  Statics[StaticP1TwoPAvatar[1]].Texture.Y  := Theme.Sing.StaticP1TwoPAvatar.Y;
  Statics[StaticP1TwoPAvatar[1]].Texture.H  := Theme.Sing.StaticP1TwoPAvatar.H;
  Statics[StaticP1TwoPAvatar[1]].Texture.W  := Theme.Sing.StaticP1TwoPAvatar.W;
  Statics[StaticP1TwoPAvatar[1]].Texture.Z := Theme.Sing.StaticP1TwoPAvatar.Z;
  Statics[StaticP1TwoPAvatar[1]].Texture.Alpha := Theme.Sing.StaticP1TwoPAvatar.Alpha;

  StaticP2RAvatar[0] := AddStatic(Theme.Sing.StaticP2RAvatar);
  Statics[StaticP2RAvatar[0]].Texture := AvatarPlayerTextures[2];
  Statics[StaticP2RAvatar[0]].Texture.X  := Theme.Sing.StaticP2RAvatar.X;
  Statics[StaticP2RAvatar[0]].Texture.Y  := Theme.Sing.StaticP2RAvatar.Y;
  Statics[StaticP2RAvatar[0]].Texture.H  := Theme.Sing.StaticP2RAvatar.H;
  Statics[StaticP2RAvatar[0]].Texture.W  := Theme.Sing.StaticP2RAvatar.W;
  Statics[StaticP2RAvatar[0]].Texture.Z := Theme.Sing.StaticP2RAvatar.Z;
  Statics[StaticP2RAvatar[0]].Texture.Alpha := Theme.Sing.StaticP2RAvatar.Alpha;

  StaticP2RAvatar[1] := AddStatic(Theme.Sing.StaticP2RAvatar);
  Statics[StaticP2RAvatar[1]].Texture := AvatarPlayerTextures[2];
  Statics[StaticP2RAvatar[1]].Texture.X  := Theme.Sing.StaticP2RAvatar.X;
  Statics[StaticP2RAvatar[1]].Texture.Y  := Theme.Sing.StaticP2RAvatar.Y;
  Statics[StaticP2RAvatar[1]].Texture.H  := Theme.Sing.StaticP2RAvatar.H;
  Statics[StaticP2RAvatar[1]].Texture.W  := Theme.Sing.StaticP2RAvatar.W;
  Statics[StaticP2RAvatar[1]].Texture.Z := Theme.Sing.StaticP2RAvatar.Z;
  Statics[StaticP2RAvatar[1]].Texture.Alpha := Theme.Sing.StaticP2RAvatar.Alpha;

  StaticP1ThreePAvatar[0] := AddStatic(Theme.Sing.StaticP1ThreePAvatar);
  Statics[StaticP1ThreePAvatar[0]].Texture := AvatarPlayerTextures[1];
  Statics[StaticP1ThreePAvatar[0]].Texture.X  := Theme.Sing.StaticP1ThreePAvatar.X;
  Statics[StaticP1ThreePAvatar[0]].Texture.Y  := Theme.Sing.StaticP1ThreePAvatar.Y;
  Statics[StaticP1ThreePAvatar[0]].Texture.H  := Theme.Sing.StaticP1ThreePAvatar.H;
  Statics[StaticP1ThreePAvatar[0]].Texture.W  := Theme.Sing.StaticP1ThreePAvatar.W;
  Statics[StaticP1ThreePAvatar[0]].Texture.Z := Theme.Sing.StaticP1ThreePAvatar.Z;
  Statics[StaticP1ThreePAvatar[0]].Texture.Alpha := Theme.Sing.StaticP1ThreePAvatar.Alpha;

  StaticP1ThreePAvatar[1] := AddStatic(Theme.Sing.StaticP1ThreePAvatar);
  Statics[StaticP1ThreePAvatar[1]].Texture := AvatarPlayerTextures[1];
  Statics[StaticP1ThreePAvatar[1]].Texture.X  := Theme.Sing.StaticP1ThreePAvatar.X;
  Statics[StaticP1ThreePAvatar[1]].Texture.Y  := Theme.Sing.StaticP1ThreePAvatar.Y;
  Statics[StaticP1ThreePAvatar[1]].Texture.H  := Theme.Sing.StaticP1ThreePAvatar.H;
  Statics[StaticP1ThreePAvatar[1]].Texture.W  := Theme.Sing.StaticP1ThreePAvatar.W;
  Statics[StaticP1ThreePAvatar[1]].Texture.Z := Theme.Sing.StaticP1ThreePAvatar.Z;
  Statics[StaticP1ThreePAvatar[1]].Texture.Alpha := Theme.Sing.StaticP1ThreePAvatar.Alpha;

  StaticP2MAvatar[0] := AddStatic(Theme.Sing.StaticP2MAvatar);
  Statics[StaticP2MAvatar[0]].Texture := AvatarPlayerTextures[2];
  Statics[StaticP2MAvatar[0]].Texture.X  := Theme.Sing.StaticP2MAvatar.X;
  Statics[StaticP2MAvatar[0]].Texture.Y  := Theme.Sing.StaticP2MAvatar.Y;
  Statics[StaticP2MAvatar[0]].Texture.H  := Theme.Sing.StaticP2MAvatar.H;
  Statics[StaticP2MAvatar[0]].Texture.W  := Theme.Sing.StaticP2MAvatar.W;
  Statics[StaticP2MAvatar[0]].Texture.Z := Theme.Sing.StaticP2MAvatar.Z;
  Statics[StaticP2MAvatar[0]].Texture.Alpha := Theme.Sing.StaticP2MAvatar.Alpha;

  StaticP2MAvatar[1] := AddStatic(Theme.Sing.StaticP2MAvatar);
  Statics[StaticP2MAvatar[1]].Texture := AvatarPlayerTextures[2];
  Statics[StaticP2MAvatar[1]].Texture.X  := Theme.Sing.StaticP2MAvatar.X;
  Statics[StaticP2MAvatar[1]].Texture.Y  := Theme.Sing.StaticP2MAvatar.Y;
  Statics[StaticP2MAvatar[1]].Texture.H  := Theme.Sing.StaticP2MAvatar.H;
  Statics[StaticP2MAvatar[1]].Texture.W  := Theme.Sing.StaticP2MAvatar.W;
  Statics[StaticP2MAvatar[1]].Texture.Z := Theme.Sing.StaticP2MAvatar.Z;
  Statics[StaticP2MAvatar[1]].Texture.Alpha := Theme.Sing.StaticP2MAvatar.Alpha;

  StaticP3RAvatar[0] := AddStatic(Theme.Sing.StaticP3RAvatar);
  Statics[StaticP3RAvatar[0]].Texture := AvatarPlayerTextures[3];
  Statics[StaticP3RAvatar[0]].Texture.X  := Theme.Sing.StaticP3RAvatar.X;
  Statics[StaticP3RAvatar[0]].Texture.Y  := Theme.Sing.StaticP3RAvatar.Y;
  Statics[StaticP3RAvatar[0]].Texture.H  := Theme.Sing.StaticP3RAvatar.H;
  Statics[StaticP3RAvatar[0]].Texture.W  := Theme.Sing.StaticP3RAvatar.W;
  Statics[StaticP3RAvatar[0]].Texture.Z := Theme.Sing.StaticP3RAvatar.Z;
  Statics[StaticP3RAvatar[0]].Texture.Alpha := Theme.Sing.StaticP3RAvatar.Alpha;

  StaticP3RAvatar[1] := AddStatic(Theme.Sing.StaticP3RAvatar);
  Statics[StaticP3RAvatar[1]].Texture := AvatarPlayerTextures[3];
  Statics[StaticP3RAvatar[1]].Texture.X  := Theme.Sing.StaticP3RAvatar.X;
  Statics[StaticP3RAvatar[1]].Texture.Y  := Theme.Sing.StaticP3RAvatar.Y;
  Statics[StaticP3RAvatar[1]].Texture.H  := Theme.Sing.StaticP3RAvatar.H;
  Statics[StaticP3RAvatar[1]].Texture.W  := Theme.Sing.StaticP3RAvatar.W;
  Statics[StaticP3RAvatar[1]].Texture.Z := Theme.Sing.StaticP3RAvatar.Z;
  Statics[StaticP3RAvatar[1]].Texture.Alpha := Theme.Sing.StaticP3RAvatar.Alpha;

  StaticDuetP1ThreePAvatar[0] := AddStatic(Theme.Sing.StaticDuetP1ThreePAvatar);
  Statics[StaticDuetP1ThreePAvatar[0]].Texture := AvatarPlayerTextures[1];
  Statics[StaticDuetP1ThreePAvatar[0]].Texture.X  := Theme.Sing.StaticDuetP1ThreePAvatar.X;
  Statics[StaticDuetP1ThreePAvatar[0]].Texture.Y  := Theme.Sing.StaticDuetP1ThreePAvatar.Y;
  Statics[StaticDuetP1ThreePAvatar[0]].Texture.H  := Theme.Sing.StaticDuetP1ThreePAvatar.H;
  Statics[StaticDuetP1ThreePAvatar[0]].Texture.W  := Theme.Sing.StaticDuetP1ThreePAvatar.W;
  Statics[StaticDuetP1ThreePAvatar[0]].Texture.Z := Theme.Sing.StaticDuetP1ThreePAvatar.Z;
  Statics[StaticDuetP1ThreePAvatar[0]].Texture.Alpha := Theme.Sing.StaticDuetP1ThreePAvatar.Alpha;

  StaticDuetP1ThreePAvatar[1] := AddStatic(Theme.Sing.StaticDuetP1ThreePAvatar);
  Statics[StaticDuetP1ThreePAvatar[1]].Texture := AvatarPlayerTextures[1];
  Statics[StaticDuetP1ThreePAvatar[1]].Texture.X  := Theme.Sing.StaticDuetP1ThreePAvatar.X;
  Statics[StaticDuetP1ThreePAvatar[1]].Texture.Y  := Theme.Sing.StaticDuetP1ThreePAvatar.Y;
  Statics[StaticDuetP1ThreePAvatar[1]].Texture.H  := Theme.Sing.StaticDuetP1ThreePAvatar.H;
  Statics[StaticDuetP1ThreePAvatar[1]].Texture.W  := Theme.Sing.StaticDuetP1ThreePAvatar.W;
  Statics[StaticDuetP1ThreePAvatar[1]].Texture.Z := Theme.Sing.StaticDuetP1ThreePAvatar.Z;
  Statics[StaticDuetP1ThreePAvatar[1]].Texture.Alpha := Theme.Sing.StaticDuetP1ThreePAvatar.Alpha;

  StaticDuetP2MAvatar[0] := AddStatic(Theme.Sing.StaticDuetP2MAvatar);
  Statics[StaticDuetP2MAvatar[0]].Texture := AvatarPlayerTextures[2];
  Statics[StaticDuetP2MAvatar[0]].Texture.X  := Theme.Sing.StaticDuetP2MAvatar.X;
  Statics[StaticDuetP2MAvatar[0]].Texture.Y  := Theme.Sing.StaticDuetP2MAvatar.Y;
  Statics[StaticDuetP2MAvatar[0]].Texture.H  := Theme.Sing.StaticDuetP2MAvatar.H;
  Statics[StaticDuetP2MAvatar[0]].Texture.W  := Theme.Sing.StaticDuetP2MAvatar.W;
  Statics[StaticDuetP2MAvatar[0]].Texture.Z := Theme.Sing.StaticDuetP2MAvatar.Z;
  Statics[StaticDuetP2MAvatar[0]].Texture.Alpha := Theme.Sing.StaticDuetP2MAvatar.Alpha;

  StaticDuetP2MAvatar[1] := AddStatic(Theme.Sing.StaticDuetP2MAvatar);
  Statics[StaticDuetP2MAvatar[1]].Texture := AvatarPlayerTextures[2];
  Statics[StaticDuetP2MAvatar[1]].Texture.X  := Theme.Sing.StaticDuetP2MAvatar.X;
  Statics[StaticDuetP2MAvatar[1]].Texture.Y  := Theme.Sing.StaticDuetP2MAvatar.Y;
  Statics[StaticDuetP2MAvatar[1]].Texture.H  := Theme.Sing.StaticDuetP2MAvatar.H;
  Statics[StaticDuetP2MAvatar[1]].Texture.W  := Theme.Sing.StaticDuetP2MAvatar.W;
  Statics[StaticDuetP2MAvatar[1]].Texture.Z := Theme.Sing.StaticDuetP2MAvatar.Z;
  Statics[StaticDuetP2MAvatar[1]].Texture.Alpha := Theme.Sing.StaticDuetP2MAvatar.Alpha;

  StaticDuetP3RAvatar[0] := AddStatic(Theme.Sing.StaticDuetP3RAvatar);
  Statics[StaticDuetP3RAvatar[0]].Texture := AvatarPlayerTextures[3];
  Statics[StaticDuetP3RAvatar[0]].Texture.X  := Theme.Sing.StaticDuetP3RAvatar.X;
  Statics[StaticDuetP3RAvatar[0]].Texture.Y  := Theme.Sing.StaticDuetP3RAvatar.Y;
  Statics[StaticDuetP3RAvatar[0]].Texture.H  := Theme.Sing.StaticDuetP3RAvatar.H;
  Statics[StaticDuetP3RAvatar[0]].Texture.W  := Theme.Sing.StaticDuetP3RAvatar.W;
  Statics[StaticDuetP3RAvatar[0]].Texture.Z := Theme.Sing.StaticDuetP3RAvatar.Z;
  Statics[StaticDuetP3RAvatar[0]].Texture.Alpha := Theme.Sing.StaticDuetP3RAvatar.Alpha;

  StaticDuetP3RAvatar[1] := AddStatic(Theme.Sing.StaticDuetP3RAvatar);
  Statics[StaticDuetP3RAvatar[1]].Texture := AvatarPlayerTextures[3];
  Statics[StaticDuetP3RAvatar[1]].Texture.X  := Theme.Sing.StaticDuetP3RAvatar.X;
  Statics[StaticDuetP3RAvatar[1]].Texture.Y  := Theme.Sing.StaticDuetP3RAvatar.Y;
  Statics[StaticDuetP3RAvatar[1]].Texture.H  := Theme.Sing.StaticDuetP3RAvatar.H;
  Statics[StaticDuetP3RAvatar[1]].Texture.W  := Theme.Sing.StaticDuetP3RAvatar.W;
  Statics[StaticDuetP3RAvatar[1]].Texture.Z := Theme.Sing.StaticDuetP3RAvatar.Z;
  Statics[StaticDuetP3RAvatar[1]].Texture.Alpha := Theme.Sing.StaticDuetP3RAvatar.Alpha;

  StaticP1FourPAvatar := AddStatic(Theme.Sing.StaticP1FourPAvatar);
  Statics[StaticP1FourPAvatar].Texture := AvatarPlayerTextures[1];
  Statics[StaticP1FourPAvatar].Texture.X  := Theme.Sing.StaticP1FourPAvatar.X;
  Statics[StaticP1FourPAvatar].Texture.Y  := Theme.Sing.StaticP1FourPAvatar.Y;
  Statics[StaticP1FourPAvatar].Texture.H  := Theme.Sing.StaticP1FourPAvatar.H;
  Statics[StaticP1FourPAvatar].Texture.W  := Theme.Sing.StaticP1FourPAvatar.W;
  Statics[StaticP1FourPAvatar].Texture.Z := Theme.Sing.StaticP1FourPAvatar.Z;
  Statics[StaticP1FourPAvatar].Texture.Alpha := Theme.Sing.StaticP1FourPAvatar.Alpha;

  StaticP2FourPAvatar := AddStatic(Theme.Sing.StaticP2FourPAvatar);
  Statics[StaticP2FourPAvatar].Texture := AvatarPlayerTextures[2];
  Statics[StaticP2FourPAvatar].Texture.X  := Theme.Sing.StaticP2FourPAvatar.X;
  Statics[StaticP2FourPAvatar].Texture.Y  := Theme.Sing.StaticP2FourPAvatar.Y;
  Statics[StaticP2FourPAvatar].Texture.H  := Theme.Sing.StaticP2FourPAvatar.H;
  Statics[StaticP2FourPAvatar].Texture.W  := Theme.Sing.StaticP2FourPAvatar.W;
  Statics[StaticP2FourPAvatar].Texture.Z := Theme.Sing.StaticP2FourPAvatar.Z;
  Statics[StaticP2FourPAvatar].Texture.Alpha := Theme.Sing.StaticP2FourPAvatar.Alpha;

  StaticP3FourPAvatar := AddStatic(Theme.Sing.StaticP3FourPAvatar);
  Statics[StaticP3FourPAvatar].Texture := AvatarPlayerTextures[3];
  Statics[StaticP3FourPAvatar].Texture.X  := Theme.Sing.StaticP3FourPAvatar.X;
  Statics[StaticP3FourPAvatar].Texture.Y  := Theme.Sing.StaticP3FourPAvatar.Y;
  Statics[StaticP3FourPAvatar].Texture.H  := Theme.Sing.StaticP3FourPAvatar.H;
  Statics[StaticP3FourPAvatar].Texture.W  := Theme.Sing.StaticP3FourPAvatar.W;
  Statics[StaticP3FourPAvatar].Texture.Z := Theme.Sing.StaticP3FourPAvatar.Z;
  Statics[StaticP3FourPAvatar].Texture.Alpha := Theme.Sing.StaticP3FourPAvatar.Alpha;

  StaticP4FourPAvatar := AddStatic(Theme.Sing.StaticP4FourPAvatar);
  Statics[StaticP4FourPAvatar].Texture := AvatarPlayerTextures[4];
  Statics[StaticP4FourPAvatar].Texture.X  := Theme.Sing.StaticP4FourPAvatar.X;
  Statics[StaticP4FourPAvatar].Texture.Y  := Theme.Sing.StaticP4FourPAvatar.Y;
  Statics[StaticP4FourPAvatar].Texture.H  := Theme.Sing.StaticP4FourPAvatar.H;
  Statics[StaticP4FourPAvatar].Texture.W  := Theme.Sing.StaticP4FourPAvatar.W;
  Statics[StaticP4FourPAvatar].Texture.Z := Theme.Sing.StaticP4FourPAvatar.Z;
  Statics[StaticP4FourPAvatar].Texture.Alpha := Theme.Sing.StaticP4FourPAvatar.Alpha;

  StaticP1DuetFourPAvatar := AddStatic(Theme.Sing.StaticP1DuetFourPAvatar);
  Statics[StaticP1DuetFourPAvatar].Texture := AvatarPlayerTextures[1];
  Statics[StaticP1DuetFourPAvatar].Texture.X  := Theme.Sing.StaticP1DuetFourPAvatar.X;
  Statics[StaticP1DuetFourPAvatar].Texture.Y  := Theme.Sing.StaticP1DuetFourPAvatar.Y;
  Statics[StaticP1DuetFourPAvatar].Texture.H  := Theme.Sing.StaticP1DuetFourPAvatar.H;
  Statics[StaticP1DuetFourPAvatar].Texture.W  := Theme.Sing.StaticP1DuetFourPAvatar.W;
  Statics[StaticP1DuetFourPAvatar].Texture.Z := Theme.Sing.StaticP1DuetFourPAvatar.Z;
  Statics[StaticP1DuetFourPAvatar].Texture.Alpha := Theme.Sing.StaticP1DuetFourPAvatar.Alpha;

  StaticP2DuetFourPAvatar := AddStatic(Theme.Sing.StaticP2DuetFourPAvatar);
  Statics[StaticP2DuetFourPAvatar].Texture := AvatarPlayerTextures[2];
  Statics[StaticP2DuetFourPAvatar].Texture.X  := Theme.Sing.StaticP2DuetFourPAvatar.X;
  Statics[StaticP2DuetFourPAvatar].Texture.Y  := Theme.Sing.StaticP2DuetFourPAvatar.Y;
  Statics[StaticP2DuetFourPAvatar].Texture.H  := Theme.Sing.StaticP2DuetFourPAvatar.H;
  Statics[StaticP2DuetFourPAvatar].Texture.W  := Theme.Sing.StaticP2DuetFourPAvatar.W;
  Statics[StaticP2DuetFourPAvatar].Texture.Z := Theme.Sing.StaticP2DuetFourPAvatar.Z;
  Statics[StaticP2DuetFourPAvatar].Texture.Alpha := Theme.Sing.StaticP2DuetFourPAvatar.Alpha;

  StaticP3DuetFourPAvatar := AddStatic(Theme.Sing.StaticP3DuetFourPAvatar);
  Statics[StaticP3DuetFourPAvatar].Texture := AvatarPlayerTextures[3];
  Statics[StaticP3DuetFourPAvatar].Texture.X  := Theme.Sing.StaticP3DuetFourPAvatar.X;
  Statics[StaticP3DuetFourPAvatar].Texture.Y  := Theme.Sing.StaticP3DuetFourPAvatar.Y;
  Statics[StaticP3DuetFourPAvatar].Texture.H  := Theme.Sing.StaticP3DuetFourPAvatar.H;
  Statics[StaticP3DuetFourPAvatar].Texture.W  := Theme.Sing.StaticP3DuetFourPAvatar.W;
  Statics[StaticP3DuetFourPAvatar].Texture.Z := Theme.Sing.StaticP3DuetFourPAvatar.Z;
  Statics[StaticP3DuetFourPAvatar].Texture.Alpha := Theme.Sing.StaticP3DuetFourPAvatar.Alpha;

  StaticP4DuetFourPAvatar := AddStatic(Theme.Sing.StaticP4DuetFourPAvatar);
  Statics[StaticP4DuetFourPAvatar].Texture := AvatarPlayerTextures[4];
  Statics[StaticP4DuetFourPAvatar].Texture.X  := Theme.Sing.StaticP4DuetFourPAvatar.X;
  Statics[StaticP4DuetFourPAvatar].Texture.Y  := Theme.Sing.StaticP4DuetFourPAvatar.Y;
  Statics[StaticP4DuetFourPAvatar].Texture.H  := Theme.Sing.StaticP4DuetFourPAvatar.H;
  Statics[StaticP4DuetFourPAvatar].Texture.W  := Theme.Sing.StaticP4DuetFourPAvatar.W;
  Statics[StaticP4DuetFourPAvatar].Texture.Z := Theme.Sing.StaticP4DuetFourPAvatar.Z;
  Statics[StaticP4DuetFourPAvatar].Texture.Alpha := Theme.Sing.StaticP4DuetFourPAvatar.Alpha;

  StaticP1SixPAvatar := AddStatic(Theme.Sing.StaticP1SixPAvatar);
  Statics[StaticP1SixPAvatar].Texture := AvatarPlayerTextures[1];
  Statics[StaticP1SixPAvatar].Texture.X  := Theme.Sing.StaticP1SixPAvatar.X;
  Statics[StaticP1SixPAvatar].Texture.Y  := Theme.Sing.StaticP1SixPAvatar.Y;
  Statics[StaticP1SixPAvatar].Texture.H  := Theme.Sing.StaticP1SixPAvatar.H;
  Statics[StaticP1SixPAvatar].Texture.W  := Theme.Sing.StaticP1SixPAvatar.W;
  Statics[StaticP1SixPAvatar].Texture.Z := Theme.Sing.StaticP1SixPAvatar.Z;
  Statics[StaticP1SixPAvatar].Texture.Alpha := Theme.Sing.StaticP1SixPAvatar.Alpha;

  StaticP2SixPAvatar := AddStatic(Theme.Sing.StaticP2SixPAvatar);
  Statics[StaticP2SixPAvatar].Texture := AvatarPlayerTextures[2];
  Statics[StaticP2SixPAvatar].Texture.X  := Theme.Sing.StaticP2SixPAvatar.X;
  Statics[StaticP2SixPAvatar].Texture.Y  := Theme.Sing.StaticP2SixPAvatar.Y;
  Statics[StaticP2SixPAvatar].Texture.H  := Theme.Sing.StaticP2SixPAvatar.H;
  Statics[StaticP2SixPAvatar].Texture.W  := Theme.Sing.StaticP2SixPAvatar.W;
  Statics[StaticP2SixPAvatar].Texture.Z := Theme.Sing.StaticP2SixPAvatar.Z;
  Statics[StaticP2SixPAvatar].Texture.Alpha := Theme.Sing.StaticP2SixPAvatar.Alpha;

  StaticP3SixPAvatar := AddStatic(Theme.Sing.StaticP3SixPAvatar);
  Statics[StaticP3SixPAvatar].Texture := AvatarPlayerTextures[3];
  Statics[StaticP3SixPAvatar].Texture.X  := Theme.Sing.StaticP3SixPAvatar.X;
  Statics[StaticP3SixPAvatar].Texture.Y  := Theme.Sing.StaticP3SixPAvatar.Y;
  Statics[StaticP3SixPAvatar].Texture.H  := Theme.Sing.StaticP3SixPAvatar.H;
  Statics[StaticP3SixPAvatar].Texture.W  := Theme.Sing.StaticP3SixPAvatar.W;
  Statics[StaticP3SixPAvatar].Texture.Z := Theme.Sing.StaticP3SixPAvatar.Z;
  Statics[StaticP3SixPAvatar].Texture.Alpha := Theme.Sing.StaticP3SixPAvatar.Alpha;

  StaticP4SixPAvatar := AddStatic(Theme.Sing.StaticP4SixPAvatar);
  Statics[StaticP4SixPAvatar].Texture := AvatarPlayerTextures[4];
  Statics[StaticP4SixPAvatar].Texture.X  := Theme.Sing.StaticP4SixPAvatar.X;
  Statics[StaticP4SixPAvatar].Texture.Y  := Theme.Sing.StaticP4SixPAvatar.Y;
  Statics[StaticP4SixPAvatar].Texture.H  := Theme.Sing.StaticP4SixPAvatar.H;
  Statics[StaticP4SixPAvatar].Texture.W  := Theme.Sing.StaticP4SixPAvatar.W;
  Statics[StaticP4SixPAvatar].Texture.Z := Theme.Sing.StaticP4SixPAvatar.Z;
  Statics[StaticP4SixPAvatar].Texture.Alpha := Theme.Sing.StaticP4SixPAvatar.Alpha;

  StaticP5SixPAvatar := AddStatic(Theme.Sing.StaticP5SixPAvatar);
  Statics[StaticP5SixPAvatar].Texture := AvatarPlayerTextures[5];
  Statics[StaticP5SixPAvatar].Texture.X  := Theme.Sing.StaticP5SixPAvatar.X;
  Statics[StaticP5SixPAvatar].Texture.Y  := Theme.Sing.StaticP5SixPAvatar.Y;
  Statics[StaticP5SixPAvatar].Texture.H  := Theme.Sing.StaticP5SixPAvatar.H;
  Statics[StaticP5SixPAvatar].Texture.W  := Theme.Sing.StaticP5SixPAvatar.W;
  Statics[StaticP5SixPAvatar].Texture.Z := Theme.Sing.StaticP5SixPAvatar.Z;
  Statics[StaticP5SixPAvatar].Texture.Alpha := Theme.Sing.StaticP5SixPAvatar.Alpha;

  StaticP6SixPAvatar := AddStatic(Theme.Sing.StaticP6SixPAvatar);
  Statics[StaticP6SixPAvatar].Texture := AvatarPlayerTextures[6];
  Statics[StaticP6SixPAvatar].Texture.X  := Theme.Sing.StaticP6SixPAvatar.X;
  Statics[StaticP6SixPAvatar].Texture.Y  := Theme.Sing.StaticP6SixPAvatar.Y;
  Statics[StaticP6SixPAvatar].Texture.H  := Theme.Sing.StaticP6SixPAvatar.H;
  Statics[StaticP6SixPAvatar].Texture.W  := Theme.Sing.StaticP6SixPAvatar.W;
  Statics[StaticP6SixPAvatar].Texture.Z := Theme.Sing.StaticP6SixPAvatar.Z;
  Statics[StaticP6SixPAvatar].Texture.Alpha := Theme.Sing.StaticP6SixPAvatar.Alpha;

  StaticP1DuetSixPAvatar := AddStatic(Theme.Sing.StaticP1DuetSixPAvatar);
  Statics[StaticP1DuetSixPAvatar].Texture := AvatarPlayerTextures[1];
  Statics[StaticP1DuetSixPAvatar].Texture.X  := Theme.Sing.StaticP1DuetSixPAvatar.X;
  Statics[StaticP1DuetSixPAvatar].Texture.Y  := Theme.Sing.StaticP1DuetSixPAvatar.Y;
  Statics[StaticP1DuetSixPAvatar].Texture.H  := Theme.Sing.StaticP1DuetSixPAvatar.H;
  Statics[StaticP1DuetSixPAvatar].Texture.W  := Theme.Sing.StaticP1DuetSixPAvatar.W;
  Statics[StaticP1DuetSixPAvatar].Texture.Z := Theme.Sing.StaticP1DuetSixPAvatar.Z;
  Statics[StaticP1DuetSixPAvatar].Texture.Alpha := Theme.Sing.StaticP1DuetSixPAvatar.Alpha;

  StaticP2DuetSixPAvatar := AddStatic(Theme.Sing.StaticP2DuetSixPAvatar);
  Statics[StaticP2DuetSixPAvatar].Texture := AvatarPlayerTextures[2];
  Statics[StaticP2DuetSixPAvatar].Texture.X  := Theme.Sing.StaticP2DuetSixPAvatar.X;
  Statics[StaticP2DuetSixPAvatar].Texture.Y  := Theme.Sing.StaticP2DuetSixPAvatar.Y;
  Statics[StaticP2DuetSixPAvatar].Texture.H  := Theme.Sing.StaticP2DuetSixPAvatar.H;
  Statics[StaticP2DuetSixPAvatar].Texture.W  := Theme.Sing.StaticP2DuetSixPAvatar.W;
  Statics[StaticP2DuetSixPAvatar].Texture.Z := Theme.Sing.StaticP2DuetSixPAvatar.Z;
  Statics[StaticP2DuetSixPAvatar].Texture.Alpha := Theme.Sing.StaticP2DuetSixPAvatar.Alpha;

  StaticP3DuetSixPAvatar := AddStatic(Theme.Sing.StaticP3DuetSixPAvatar);
  Statics[StaticP3DuetSixPAvatar].Texture := AvatarPlayerTextures[3];
  Statics[StaticP3DuetSixPAvatar].Texture.X  := Theme.Sing.StaticP3DuetSixPAvatar.X;
  Statics[StaticP3DuetSixPAvatar].Texture.Y  := Theme.Sing.StaticP3DuetSixPAvatar.Y;
  Statics[StaticP3DuetSixPAvatar].Texture.H  := Theme.Sing.StaticP3DuetSixPAvatar.H;
  Statics[StaticP3DuetSixPAvatar].Texture.W  := Theme.Sing.StaticP3DuetSixPAvatar.W;
  Statics[StaticP3DuetSixPAvatar].Texture.Z := Theme.Sing.StaticP3DuetSixPAvatar.Z;
  Statics[StaticP3DuetSixPAvatar].Texture.Alpha := Theme.Sing.StaticP3DuetSixPAvatar.Alpha;

  StaticP4DuetSixPAvatar := AddStatic(Theme.Sing.StaticP4DuetSixPAvatar);
  Statics[StaticP4DuetSixPAvatar].Texture := AvatarPlayerTextures[4];
  Statics[StaticP4DuetSixPAvatar].Texture.X  := Theme.Sing.StaticP4DuetSixPAvatar.X;
  Statics[StaticP4DuetSixPAvatar].Texture.Y  := Theme.Sing.StaticP4DuetSixPAvatar.Y;
  Statics[StaticP4DuetSixPAvatar].Texture.H  := Theme.Sing.StaticP4DuetSixPAvatar.H;
  Statics[StaticP4DuetSixPAvatar].Texture.W  := Theme.Sing.StaticP4DuetSixPAvatar.W;
  Statics[StaticP4DuetSixPAvatar].Texture.Z := Theme.Sing.StaticP4DuetSixPAvatar.Z;
  Statics[StaticP4DuetSixPAvatar].Texture.Alpha := Theme.Sing.StaticP4DuetSixPAvatar.Alpha;

  StaticP5DuetSixPAvatar := AddStatic(Theme.Sing.StaticP5DuetSixPAvatar);
  Statics[StaticP5DuetSixPAvatar].Texture := AvatarPlayerTextures[5];
  Statics[StaticP5DuetSixPAvatar].Texture.X  := Theme.Sing.StaticP5DuetSixPAvatar.X;
  Statics[StaticP5DuetSixPAvatar].Texture.Y  := Theme.Sing.StaticP5DuetSixPAvatar.Y;
  Statics[StaticP5DuetSixPAvatar].Texture.H  := Theme.Sing.StaticP5DuetSixPAvatar.H;
  Statics[StaticP5DuetSixPAvatar].Texture.W  := Theme.Sing.StaticP5DuetSixPAvatar.W;
  Statics[StaticP5DuetSixPAvatar].Texture.Z := Theme.Sing.StaticP5DuetSixPAvatar.Z;
  Statics[StaticP5DuetSixPAvatar].Texture.Alpha := Theme.Sing.StaticP5DuetSixPAvatar.Alpha;

  StaticP6DuetSixPAvatar := AddStatic(Theme.Sing.StaticP6DuetSixPAvatar);
  Statics[StaticP6DuetSixPAvatar].Texture := AvatarPlayerTextures[6];
  Statics[StaticP6DuetSixPAvatar].Texture.X  := Theme.Sing.StaticP6DuetSixPAvatar.X;
  Statics[StaticP6DuetSixPAvatar].Texture.Y  := Theme.Sing.StaticP6DuetSixPAvatar.Y;
  Statics[StaticP6DuetSixPAvatar].Texture.H  := Theme.Sing.StaticP6DuetSixPAvatar.H;
  Statics[StaticP6DuetSixPAvatar].Texture.W  := Theme.Sing.StaticP6DuetSixPAvatar.W;
  Statics[StaticP6DuetSixPAvatar].Texture.Z := Theme.Sing.StaticP6DuetSixPAvatar.Z;
  Statics[StaticP6DuetSixPAvatar].Texture.Alpha := Theme.Sing.StaticP6DuetSixPAvatar.Alpha;

  ClearSettings;
end;

procedure TScreenSing.OnShow;
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

  CloseMessage;

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

  for I := 0 to High(StaticDuet) do
    Statics[StaticDuet[I]].Visible := CurrentSong.isDuet and (PlayersPlay > 1);

  Statics[SongNameStatic].Visible := false;
  Text[SongNameText].Visible := false;

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

  Text[TextP1].Visible       := V1;
  Text[TextP1TwoP].Visible   := V1TwoP;
  Text[TextP2R].Visible      := V2R;
  Text[TextP1ThreeP].Visible := V1ThreeP;
  Text[TextP2M].Visible      := V2M;
  Text[TextP3R].Visible      := V3R;
  Text[TextDuetP1ThreeP].Visible := VDuet1ThreeP;
  Text[TextDuetP2M].Visible      := VDuet2M;
  Text[TextDuetP3R].Visible      := VDuet3R;
  Text[TextP1FourP].Visible   := V1FourP;
  Text[TextP2FourP].Visible   := V2FourP;
  Text[TextP3FourP].Visible   := V3FourP;
  Text[TextP4FourP].Visible   := V4FourP;
  Text[TextP1SixP].Visible    := V1SixP;
  Text[TextP2SixP].Visible    := V2SixP;
  Text[TextP3SixP].Visible    := V3SixP;
  Text[TextP4SixP].Visible    := V4SixP;
  Text[TextP5SixP].Visible    := V5SixP;
  Text[TextP6SixP].Visible    := V6SixP;
  Text[TextP1DuetFourP].Visible   := V1DuetFourP;
  Text[TextP2DuetFourP].Visible   := V2DuetFourP;
  Text[TextP3DuetFourP].Visible   := V3DuetFourP;
  Text[TextP4DuetFourP].Visible   := V4DuetFourP;
  Text[TextP1DuetSixP].Visible    := V1DuetSixP;
  Text[TextP2DuetSixP].Visible    := V2DuetSixP;
  Text[TextP3DuetSixP].Visible    := V3DuetSixP;
  Text[TextP4DuetSixP].Visible    := V4DuetSixP;
  Text[TextP5DuetSixP].Visible    := V5DuetSixP;
  Text[TextP6DuetSixP].Visible    := V6DuetSixP;

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
      ColPlayer[0] := GetLyricColor(Ini.SingColor[0]);
      ColPlayer[1] := GetLyricColor(Ini.SingColor[1]);
      ColPlayer[2] := GetLyricColor(Ini.SingColor[2]);
      ColPlayer[3] := GetLyricColor(Ini.SingColor[3]);
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

        if (CurrentSong.isDuet) then
        begin
          Lyrics.LineColor_en.R := 0.7;
          Lyrics.LineColor_en.G := 0.7;
          Lyrics.LineColor_en.B := 0.7;
          Lyrics.LineColor_en.A := 1;
        end
        else
        begin
          Lyrics.LineColor_en.R := 0.75;
          Lyrics.LineColor_en.G := 0.75;
          Lyrics.LineColor_en.B := 1;
          Lyrics.LineColor_en.A := 1;
        end;

        Lyrics.LineColor_dis.R := 0.8;
        Lyrics.LineColor_dis.G := 0.8;
        Lyrics.LineColor_dis.B := 0.8;
        Lyrics.LineColor_dis.A := 1;

        Lyrics.LineColor_act.R := Col.R; //0.5;
        Lyrics.LineColor_act.G := Col.G; //0.5;
        Lyrics.LineColor_act.B := Col.B; //1;
        Lyrics.LineColor_act.A := 1;
      end;
    end; // case
  end;

  // deactivate pause
  Paused := false;

  LoadNextSong();

  Log.LogStatus('End', 'OnShow');
end;

procedure TScreenSing.onShowFinish;
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

procedure TScreenSing.SongError();
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

procedure TScreenSing.LoadNextSong();
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
      Text[SongNameText].Text := IntToStr(PlaylistMedley.CurrentMedleySong) +
        '/' + IntToStr(PlaylistMedley.NumMedleySongs) + ': ' +
        CurrentSong.Artist + ' - ' + CurrentSong.Title
    else
      Text[SongNameText].Text := CurrentSong.Artist + ' - ' + CurrentSong.Title;

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
   * We have four types of backgrounds:
   *   + Blank        : Nothing has been set, this is our fallback
   *   + Picture      : Picture has been set, and exists - otherwise we fallback
   *   + Video        : Video has been set, and exists - otherwise we fallback
   *   + Visualization: + Off        : No visualization
   *                    + WhenNoVideo: Overwrites blank and picture
   *                    + On         : Overwrites blank, picture and video
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
  end;

  {*
   * set background to: visualization (Videos are still shown)
   *}
  if ((TVisualizerOption(Ini.VisualizerOption) in [voWhenNoVideo]) and
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

procedure TScreenSing.ClearSettings;
begin
  Settings.Finish := False;
  Settings.LyricsVisible := True;
  Settings.NotesVisible := high(Integer);
  Settings.PlayerEnabled := high(Integer);
  Settings.SoundEnabled := True;
end;

{ applies changes of settings record }
procedure TScreenSing.ApplySettings;
begin
  //
end;

procedure TScreenSing.EndSong;
begin
  Settings.Finish := True;
end;

procedure TScreenSing.OnHide;
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

function TScreenSing.FinishedMusic: boolean;
begin
  Result := AudioPlayback.Finished;
end;

function TScreenSing.Draw: boolean;
var
  DisplayTime:            real;
  DisplayPrefix:          string;
  DisplayMin:             integer;
  DisplaySec:             integer;
  T:                      integer;
  CurLyricsTime:          real;
  VideoFrameTime:         Extended;
  Line:                   TLyricLine;
  LastWord:               TLyricWord;
  LineDuet:                   TLyricLine;
  LastWordDuet:               TLyricWord;
  medley_end:             boolean;
  medley_start_applause:  boolean;
begin
  Background.Draw;

  // sound enabled/disabled (Party plugins)
  if not(ScreenSing.settings.SoundEnabled) then
    AudioPlayback.SetVolume(0)
  else
    AudioPlayback.SetVolume(1);

  // swap static textures to current screen ones
  SwapToScreen(ScreenAct);

  // draw background picture (if any, and if no visualizations)
  // when we don't check for visualizations the visualizations would
  // be overdrawn by the picture when {UNDEFINED UseTexture} in UVisualizer
  //if (not fShowVisualization) then
  if (not fShowVisualization) or (fShowBackground) then
    SingDrawBackground;

  if (fShowWebCam) then
    SingDrawWebCamFrame;

  // set player names (for 2 screens and only singstar skin)
  if ScreenAct = 1 then
  begin
    Text[TextP1].Text     := P1Name;
    Text[TextP1TwoP].Text := P1Name;
    Text[TextP1ThreeP].Text := P1Name;
    Text[TextP2R].Text    := P2Name;
    Text[TextP2M].Text    := P2Name;
    Text[TextP3R].Text    := P3Name;
    Text[TextDuetP1ThreeP].Text := P1DuetName;
    Text[TextDuetP2M].Text      := P2DuetName;
    Text[TextDuetP3R].Text      := P3DuetName;
    Text[TextP1FourP].Text     := P1Name;
    Text[TextP2FourP].Text     := P2Name;
    Text[TextP3FourP].Text     := P3Name;
    Text[TextP4FourP].Text     := P4Name;
    Text[TextP1DuetFourP].Text     := P1DuetName;
    Text[TextP2DuetFourP].Text     := P2DuetName;
    Text[TextP3DuetFourP].Text     := P3DuetName;
    Text[TextP4DuetFourP].Text     := P4DuetName;
    Text[TextP1SixP].Text     := P1Name;
    Text[TextP2SixP].Text     := P2Name;
    Text[TextP3SixP].Text     := P3Name;
    Text[TextP4SixP].Text     := P4Name;
    Text[TextP5SixP].Text     := P5Name;
    Text[TextP6SixP].Text     := P6Name;
    Text[TextP1DuetSixP].Text     := P1DuetName;
    Text[TextP2DuetSixP].Text     := P2DuetName;
    Text[TextP3DuetSixP].Text     := P3DuetName;
    Text[TextP4DuetSixP].Text     := P4DuetName;
    Text[TextP5DuetSixP].Text     := P5DuetName;
    Text[TextP6DuetSixP].Text     := P6DuetName;

    if (CurrentSong.isDuet) then
    begin
      if (PlayersPlay = 4) then
      begin
        LyricsDuetP1.LineColor_act.R := ColPlayer[0].R;
        LyricsDuetP1.LineColor_act.G := ColPlayer[0].G;
        LyricsDuetP1.LineColor_act.B := ColPlayer[0].B;

        LyricsDuetP2.LineColor_act.R := ColPlayer[1].R;
        LyricsDuetP2.LineColor_act.G := ColPlayer[1].G;
        LyricsDuetP2.LineColor_act.B := ColPlayer[1].B;
      end;
    end;
  end;

  if ScreenAct = 2 then
  begin
    case PlayersPlay of
      4:
      begin
        Text[TextP1TwoP].Text := P3Name;
        Text[TextP2R].Text    := P4Name;

        if (CurrentSong.isDuet) and (PlayersPlay = 4) then
        begin
          LyricsDuetP1.LineColor_act.R := ColPlayer[2].R;
          LyricsDuetP1.LineColor_act.G := ColPlayer[2].G;
          LyricsDuetP1.LineColor_act.B := ColPlayer[2].B;

          LyricsDuetP2.LineColor_act.R := ColPlayer[3].R;
          LyricsDuetP2.LineColor_act.G := ColPlayer[3].G;
          LyricsDuetP2.LineColor_act.B := ColPlayer[3].B;
        end;

      end;
      6:
      begin
        if (CurrentSong.isDuet) then
        begin
          Text[TextDuetP1ThreeP].Text := P4DuetName;
          Text[TextDuetP2M].Text      := P5DuetName;
          Text[TextDuetP3R].Text      := P6DuetName;
        end
        else
        begin
          Text[TextP1ThreeP].Text := P4Name;
          Text[TextP2M].Text      := P5Name;
          Text[TextP3R].Text      := P6Name;
        end;
      end;
    end; // case
  end; // if

  // retrieve current lyrics time, we have to store the value to avoid
  // that min- and sec-values do not match
  if ScreenSong.Mode = smMedley then
  begin
    CurLyricsTime := LyricsState.GetCurrentTime() - MedleyStart;
    TotalTime := MedleyEnd - MedleyStart;
  end
  else
  begin
    CurLyricsTime := LyricsState.GetCurrentTime();
    TotalTime :=  LyricsState.TotalTime;
  end;

  // retrieve time for timebar text
  case (fTimebarMode) of
    tbmRemaining: begin
      DisplayTime := TotalTime - CurLyricsTime;
      DisplayPrefix := '-';
    end;
    tbmTotal: begin
      DisplayTime := TotalTime;
      DisplayPrefix := '#';
    end;
    else begin       // current time
      DisplayTime := CurLyricsTime;
      DisplayPrefix := '';
    end;
  end;
  DisplayMin := Round(DisplayTime) div 60;
  DisplaySec := Round(DisplayTime) mod 60;

  // update static menu with time ...
  Text[TextTimeText].Text := Format('%s%.2d:%.2d',
      [DisplayPrefix, DisplayMin, DisplaySec]);

  //the song was sung to the end?
  if not(CurrentSong.isDuet) then
  begin
    Line := Lyrics.GetUpperLine();
    if Line.LastLine then
    begin
      LastWord := Line.Words[Length(Line.Words)-1];
      if CurLyricsTime >= GetTimeFromBeat(LastWord.Start + LastWord.Length) then
        SungToEnd := true;
    end;
  end
  else
  begin
  {  Line := Lyrics.GetUpperLine();
    LineDuet := LyricsDuet.GetUpperLine();
    if Line.LastLine and (LineDuet.LastLine) then
    begin
      LastWord := Line.Words[Length(Line.Words)-1];
      LastWordDuet := LineDuet.Words[Length(Line.Words)-1];
      if (CurLyricsTime >= GetTimeFromBeat(LastWord.Start+LastWord.Length)) and (CurLyricsTime >= GetTimeFromBeat(LastWordDuet.Start+LastWordDuet.Length)) then
        // TODO SAVE DUET SCORES
        SungToEnd := false;
        //SungToEnd := true;
    end;
    }
  end;

  // for medley-mode:
  CurLyricsTime := LyricsState.GetCurrentTime();
  if (ScreenSong.Mode = smMedley) and (CurLyricsTime > MedleyEnd) then
    medley_end := true
  else
    medley_end := false;

  if (ScreenSong.Mode = smMedley) and (CurLyricsTime >
    GetTimeFromBeat(CurrentSong.Medley.EndBeat)) then
    medley_start_applause := true
  else
    medley_start_applause := false;

  // update and draw movie
  // USE FFMPEG
  if Assigned(fCurrentVideo) and (not fShowWebcam) then
  begin
    // Just call this once
    // when Screens = 2
    if (ScreenAct = 1) then
    begin
      if (ShowFinish) then
      begin
        // everything is setup, determine the current position
        VideoFrameTime := CurrentSong.VideoGAP + LyricsState.GetCurrentTime();
      end
      else
      begin
        // Important: do not yet start the triggered timer by a call to
        // LyricsState.GetCurrentTime()
        VideoFrameTime := CurrentSong.VideoGAP;
      end;

      fCurrentVideo.GetFrame(VideoFrameTime);
    end;

    fCurrentVideo.SetScreen(ScreenAct);
    fCurrentVideo.Draw;
  end;

  // draw notes lines
  SingDrawLines;

  // draw static menu (FG)
  DrawFG;

  //Medley Countdown
  if ScreenSong.Mode = smMedley then
    DrawMedleyCountdown;

  // check for music finish
  //Log.LogError('Check for music finish: ' + BoolToStr(Music.Finished) + ' ' + FloatToStr(LyricsState.CurrentTime*1000) + ' ' + IntToStr(CurrentSong.Finish));
  if ShowFinish then
  begin
    if (not FinishedMusic) and (not medley_end or (ScreenSong.Mode <> smMedley)) and
       ((CurrentSong.Finish = 0) or
        (LyricsState.GetCurrentTime() * 1000 <= CurrentSong.Finish)) and
       (not Settings.Finish) then
    begin
      // analyze song if not paused
      if (not Paused) then
      begin
        Sing(Self);

        //Update Medley Stats
        if (ScreenSong.Mode = smMedley) and not FadeOut then
          UpdateMedleyStats(medley_start_applause);

        Party.CallOnSing;
      end;
    end
    else
    begin
      if (not FadeOut) and (Screens=1) or (ScreenAct=2) then
      begin
        Finish;
      end;
    end;
  end;

  // draw info lyric bar
  DrawInfoLyricBar;

  // always draw custom items
  SingDraw;

  // goldennotestarstwinkle
  GoldenRec.SpawnRec;

  // draw scores
  if (Ini.SingScores = 1) or (Party.bPartyGame) then
    Scores.Draw;

  FadeMessage();

  // draw pausepopup
  // FIXME: this is a workaround that the static is drawn over the lyrics, lines, scores and effects
  // maybe someone could find a better solution
  if Paused then
  begin
    Statics[StaticPausePopup].Texture.Z := 1;
    Statics[StaticPausePopup].Visible := true;
    Statics[StaticPausePopup].Draw;
    Statics[StaticPausePopup].Visible := false;
  end;

  Result := true;
end;

procedure TScreenSing.Finish;
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

procedure TScreenSing.OnSentenceEnd(CP: integer; SentenceIndex: cardinal);
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
procedure TScreenSing.OnSentenceChange(CP: integer; SentenceIndex: cardinal);
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

procedure TScreenSing.UpdateMedleyStats(medley_end: boolean);
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

procedure TScreenSing.DrawMedleyCountdown();
var
  w, h:           real;
  timeDiff:       real;
  t:              real;
  CountDownText:  UTF8String;
  Position:       real;
begin
  if AudioPlayback.Position < GetTimeFromBeat(CurrentSong.Medley.StartBeat) then
  begin
    TextMedleyFadeOut := false;

    Statics[SongNameStatic].Texture.Alpha := 1;
    Text[SongNameText].Alpha := 1;

    Statics[SongNameStatic].Visible := true;
    Text[SongNameText].Visible := true;

    timeDiff := GetTimeFromBeat(CurrentSong.Medley.StartBeat) - AudioPlayback.Position + 1;
    t := frac(timeDiff);

    glColor4f(0.15, 0.30, 0.6, t);

    h := 300*t*ScreenH/RenderH;
    SetFontStyle(ftBoldHighRes);
    SetFontItalic(false);
    SetFontSize(h);
    CountDownText := IntToStr(round(timeDiff-t));
    w := glTextWidth(PChar(CountDownText));

    SetFontPos (RenderW/2-w/2, RenderH/2-h/2);
    glPrint(PChar(CountDownText));
  end else
  begin
    if (TextMedleyFadeOut = false) then
    begin
      TextMedleyFadeOut := true;
      TextMedleyFadeTime := SDL_GetTicks();
    end;

    MedleyTitleFadeOut;
  end;
end;

procedure TScreenSing.AutoSendScore;
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

procedure TScreenSing.AutoSaveScore;
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

procedure TScreenSing.WriteMessage(msg: UTF8String);
begin
  MessageTime := SDL_GetTicks();

  Statics[InfoMessageBG].Texture.Alpha := 1;
  Text[InfoMessageText].Alpha := 1;

  Statics[InfoMessageBG].Visible := true;
  Text[InfoMessageText].Visible := true;
  Text[InfoMessageText].Text := msg;
end;

procedure TScreenSing.FadeMessage();
var
  factor: real;
begin
  if ((SDL_GetTicks - MessageTime)/1000 > MAX_MESSAGE) then
  begin
    if (MessageTimeFade = 0) then
      MessageTimeFade := SDL_GetTicks();

    factor := (SDL_GetTicks - MessageTimeFade)/1000/2;

    Statics[InfoMessageBG].Texture.Alpha := 1 - factor;
    Text[InfoMessageText].Alpha := 1 - factor;
  end
  else
    MessageTimeFade := 0;

  Statics[InfoMessageBG].Draw;
  Text[InfoMessageText].Draw;
end;

procedure TScreenSing.CloseMessage();
begin
  Statics[InfoMessageBG].Visible := false;
  Text[InfoMessageText].Visible := false;
end;

procedure TScreenSing.MedleyTitleFadeOut();
var
  I: integer;
  Alpha: real;
  CTime: cardinal;
begin

  CTime := SDL_GetTicks() - TextMedleyFadeTime;
  Alpha := CTime/3000;

  if (Alpha >= 1) then
  begin
    Statics[SongNameStatic].Visible := false;
    Text[SongNameText].Visible := false;
  end
  else
  begin
    Text[SongNameText].Alpha := 1 - Alpha;
    Statics[SongNameStatic].Texture.Alpha := 1 - Alpha;
  end;
end;

function TScreenSing.GetLyricColor(Color: integer): TRGB;
begin
  case (Color) of
    1://blue
    begin
      Result.R := 5/255;
      Result.G := 153/255;
      Result.B := 204/255;
    end;
    2: //red
    begin
      Result.R := 230/255;
      Result.G := 0;
      Result.B := 0;
    end;
    3: //green
    begin
      Result.R := 0;
      Result.G := 170/255;
      Result.B := 0;
    end;
    4: //yellow
    begin
      Result.R := 255/255;
      Result.G := 225/255;
      Result.B := 0;
    end;
    5: //orange
    begin
      Result.R := 227/255;
      Result.G := 127/255;
      Result.B := 0;
    end;
    6: //pink
    begin
      Result.R := 255/255;
      Result.G := 0/255;
      Result.B := 130/255;
    end;
    7: //purple
    begin
      Result.R := 180/255;
      Result.G := 0;
      Result.B := 220/255;
    end;
    8: //gold
    begin
      Result.R := 255/255;
      Result.G := 190/255;
      Result.B := 35/255;
    end;
    9: //gray
    begin
      Result.R := 80/255;
      Result.G := 80/255;
      Result.B := 80/255;
    end;
    10: //dark blue
    begin
      Result.R := 90/255;
      Result.G := 90/255;
      Result.B := 240/255;
    end;
    11: //sky
    begin
      Result.R := 0;
      Result.G := 110/255;
      Result.B := 210/255;
    end;
    12: //cyan
    begin
      Result.R := 0/255;
      Result.G := 215/255;
      Result.B := 215/255;
    end;
    13: //flame
    begin
      Result.R := 210/255;
      Result.G := 70/255;
      Result.B := 0/255;
    end;
    14: //orchid
    begin
      Result.R := 210/255;
      Result.G := 0;
      Result.B := 210/255;
    end;
    15: //harlequin
    begin
      Result.R := 110/255;
      Result.G := 210/255;
      Result.B := 0;
    end;
    16: //lime
    begin
      Result.R := 160/255;
      Result.G := 210/255;
      Result.B := 0;
    end;
  end;
end;

procedure TScreenSing.DrawInfoLyricBar();
var
  SongStart, SongEnd: real;
  ww:                 real;

  pos:                real;
  br:                 real;

  line:               integer;
  numLines:           integer;

  x, y, w, h: real;
  CurrentLine: integer;
  GAPxStart, GAPw: real;
begin
  x := Theme.Sing.StaticTimeProgress.x;
  y := Theme.Sing.StaticTimeProgress.y;

  w := Theme.Sing.StaticTimeProgress.w;
  h := Theme.Sing.StaticTimeProgress.h;

  //calculate x (position of song start on the time-bar)
  GAPxStart := w*((CurrentSong.GAP/1000)/LyricsState.TotalTime);
  x := x + GAPxStart; //move x to the right by Song-Gap-Seconds

  //width
  //LastLine := Lines[0].Line[Length(Lines[0].Line) - 1];
  w := w - GAPxStart;

  //calculate total singing seconds of song
  SongStart := 99999999999999;
  SongEnd := CurrentSong.BPM[0].BPM*TotalTime/60;
  for CurrentLine := 0 to High(Lines) do //P1 of Duett or standard, P2 of Duett,..
  begin
    numLines := Length(Lines[CurrentLine].Line); //Lyric lines
    if (numLines < 2) then //catch cases which could cause endless loop
      Exit;
    if SongStart > (Lines[CurrentLine].Line[0].Note[0].Start+(CurrentSong.BPM[0].BPM*CurrentSong.GAP*(1/60/1000))) then
           SongStart := Lines[CurrentLine].Line[0].Note[0].Start + (CurrentSong.BPM[0].BPM*CurrentSong.GAP*(1/60/1000));
  end;
  ww := SongEnd - SongStart;

  for CurrentLine := 0 to High(Lines) do //for P1 of Duett-lyrics or standard-lyrics, P2 of Duett,..
  begin
    numLines := Length(Lines[CurrentLine].Line); //Lyric lines
    if (numLines < 2) then //catch cases which could cause endless loop
      Exit;

    for line := 0 to numLines - 1 do
    begin
      //set color to player.color
      if (CurrentLine = 0) then
        glColor4f(GetLyricColor(Ini.SingColor[0]).R, GetLyricColor(Ini.SingColor[0]).G, GetLyricColor(Ini.SingColor[0]).B, 0.8)
      else
        glColor4f(GetLyricColor(Ini.SingColor[CurrentLine]).R, GetLyricColor(Ini.SingColor[CurrentLine]).G, GetLyricColor(Ini.SingColor[CurrentLine]).B, 0.4);
      if Lines[CurrentLine].Line[line].Note = nil then Continue;
      pos := (Lines[CurrentLine].Line[line].Note[0].Start)/ww*w;
      br := (Lines[CurrentLine].Line[line].Note[Lines[CurrentLine].Line[line].HighNote].Start +
                Lines[CurrentLine].Line[line].Note[Lines[CurrentLine].Line[line].HighNote].Length -
                Lines[CurrentLine].Line[line].Note[0].Start ) / ww*w; //br = last note of sentence position + its length - first note of sentence position

      glbegin(gl_quads); //draw a square
        glVertex2f(x+pos, y); //left top
        glVertex2f(x+pos, y+h); //left bottom
        glVertex2f(x+pos+br, y+h); //right bottom
        glVertex2f(x+pos+br, y); //right top
      glEnd;
    end;
  end;
end;

end.

