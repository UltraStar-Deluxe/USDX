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

unit UScreenSing;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  SDL,
  gl,
  TextGL,
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
  UTime;

type
  TPos = record // Lines[part].Line[line].Note[note]
    part: integer;
    line: integer;
    note: integer;
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
    fShowVisualization: boolean;
    fCurrentVideo:      IVideo;
    fVideoClip:         IVideo;
    fLyricsSync:        TLyricsSyncSource;
    fMusicSync:         TMusicSyncSource;
    fTimebarMode:       TTimebarMode;

    StartNote, EndNote: TPos;

    procedure LoadNextSong();
    procedure UpdateMedleyStats(medley_end: boolean);
    procedure DrawMedleyCountdown();
    procedure SongError();
  protected
    eSongLoaded: THookableEvent; //< event is called after lyrics of a song are loaded on OnShow
    Paused:     boolean; //pause Mod
    NumEmptySentences: integer;
  public
    // timebar fields
    StaticTimeProgress: integer;
    TextTimeText: integer;

    StaticP1: integer;
    TextP1:   integer;

    // shown when game is in 2/4 player modus
    StaticP1TwoP: integer;
    TextP1TwoP:   integer;

    // shown when game is in 3/6 player modus
    StaticP1ThreeP: integer;
    TextP1ThreeP:   integer;

    MedleyStart, MedleyEnd: real;

    StaticP2R: integer;
    TextP2R:   integer;

    StaticP2M: integer;
    TextP2M:   integer;

    StaticP3R: integer;
    TextP3R:   integer;

    StaticPausePopup: integer;

    SongNameStatic:   integer;
    SongNameText:     integer;

    Tex_Background: TTexture;
    FadeOut: boolean;
    Lyrics:  TLyricEngine;

    // score manager:
    Scores: TSingScores;

    //the song was sung to the end
    SungToEnd: boolean;

    // some settings to be set by plugins
    Settings: record
      Finish: boolean; //< if true, screen will finish on next draw

      LyricsVisible: boolean; //< shows or hides lyrics
      NotesVisible: integer; //< if bit[playernum] is set the notes for the specified player are visible. By default all players notes are visible

      PlayerEnabled: integer; //< defines whether a player can score atm
    end;

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

    procedure Finish; virtual;
    procedure Pause; // toggle pause

    procedure OnSentenceEnd(SentenceIndex: cardinal);     // for linebonus + singbar
    procedure OnSentenceChange(SentenceIndex: cardinal);  // for golden notes
  end;

implementation

uses
  Classes,
  Math,
  UDisplay,
  UDraw,
  UGraphic,
  ULanguage,
  UNote,
  UParty,
  URecord,
  USong,
  UUnicodeUtils;

// method for input parsing. if false is returned, getnextwindow
// should be checked to know the next window to load;

function TScreenSing.ParseInput(PressedKey: cardinal; CharCode: UCS4Char;
  PressedDown: boolean): boolean;
begin
  Result := true;
  if PressedDown then
  begin // key down
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
        fShowVisualization := not fShowVisualization;

        if fShowVisualization then
        begin
          fCurrentVideo := Visualization.Open(PATH_NONE);
          fCurrentVideo.play;
        end
        else
        begin
          fCurrentVideo := fVideoClip;
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
        if fTimebarMode = High(TTimebarMode) then
          fTimebarMode := Low(TTimebarMode)
        else
          Inc(fTimebarMode);
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
      end;

      SDLK_RETURN:
      begin
      end;

      // up and down could be done at the same time,
      // but i don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:
      begin
      end;
      SDLK_UP:
      begin
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

constructor TScreenSing.Create;
begin
  inherited Create;

  //too dangerous, a mouse button is quickly pressed by accident
  RightMbESC := false;

  fShowVisualization := false;

  fCurrentVideo := nil;

  // create score class
  Scores := TSingScores.Create;
  Scores.LoadfromTheme;

  LoadFromTheme(Theme.Sing);

  // timebar
  StaticTimeProgress := AddStatic(Theme.Sing.StaticTimeProgress);
  TextTimeText := AddText(Theme.Sing.TextTimeText);

  // 1 player       | P1
  StaticP1 := AddStatic(Theme.Sing.StaticP1);
  TextP1   := AddText(Theme.Sing.TextP1);

  // 2 or 4 players | P1
  StaticP1TwoP := AddStatic(Theme.Sing.StaticP1TwoP);
  TextP1TwoP   := AddText(Theme.Sing.TextP1TwoP);

  //                | P2
  StaticP2R := AddStatic(Theme.Sing.StaticP2R);
  TextP2R   := AddText(Theme.Sing.TextP2R);

  // 3 or 6 players | P1
  StaticP1ThreeP := AddStatic(Theme.Sing.StaticP1ThreeP);
  TextP1ThreeP   := AddText(Theme.Sing.TextP1ThreeP);

  //                | P2
  StaticP2M := AddStatic(Theme.Sing.StaticP2M);
  TextP2M   := AddText(Theme.Sing.TextP2M);

  //                | P3
  StaticP3R := AddStatic(Theme.Sing.StaticP3R);
  TextP3R   := AddText(Theme.Sing.TextP3R);

  StaticPausePopup := AddStatic(Theme.Sing.PausePopUp);

  // <note> pausepopup is not visible at the beginning </note>
  Statics[StaticPausePopup].Visible := false;

  Lyrics := TLyricEngine.Create(
      Theme.LyricBar.UpperX, Theme.LyricBar.UpperY, Theme.LyricBar.UpperW, Theme.LyricBar.UpperH,
      Theme.LyricBar.LowerX, Theme.LyricBar.LowerY, Theme.LyricBar.LowerW, Theme.LyricBar.LowerH);

  fLyricsSync := TLyricsSyncSource.Create();
  fMusicSync := TMusicSyncSource.Create();

  SongNameStatic := AddStatic(Theme.Sing.StaticSongName);;
  SongNameText := AddText(Theme.Sing.TextSongName);

  eSongLoaded := THookableEvent.Create('ScreenSing.SongLoaded');

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
  BadPlayer: integer;

begin
  inherited;

  Log.LogStatus('Begin', 'OnShow');
  FadeOut := false;

  //the song was sung to the end
  SungToEnd := false;
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

    fTimebarMode := tbmRemaining;
  end
  else
    fTimebarMode := tbmCurrent;

  Statics[SongNameStatic].Visible := false;
  Text[SongNameText].Visible := false;

  case PlayersPlay of
    1:
    begin
      V1     := true;
      V1TwoP := false;
      V1ThreeP := false;
      V2R    := false;
      V2M    := false;
      V3R    := false;
    end;
    2:
    begin
      V1     := false;
      V1TwoP := true;
      V1ThreeP := false;
      V2R    := true;
      V2M    := false;
      V3R    := false;
    end;
    3:
    begin
      V1     := false;
      V1TwoP := false;
      V1ThreeP := true;
      V2R    := false;
      V2M    := true;
      V3R    := true;
    end;
    4:
    begin // double screen
      V1     := false;
      V1TwoP := true;
      V1ThreeP := false;
      V2R    := true;
      V2M    := false;
      V3R    := false;
    end;
    6:
    begin // double screen
      V1     := false;
      V1TwoP := false;
      V1ThreeP := true;
      V2R    := false;
      V2M    := true;
      V3R    := true;
    end;

  end;

  // this one is shown in 1P mode
  Statics[StaticP1].Visible := V1;
  Text[TextP1].Visible     := V1;

  // this one is shown in 2/4P mode
  Statics[StaticP1TwoP].Visible := V1TwoP;
  Text[TextP1TwoP].Visible     := V1TwoP;

  Statics[StaticP2R].Visible := V2R;
  Text[TextP2R].Visible     := V2R;

  // this one is shown in 3/6P mode
  Statics[StaticP1ThreeP].Visible := V1ThreeP;
  Text[TextP1ThreeP].Visible     := V1ThreeP;

  Statics[StaticP2M].Visible := V2M;
  Text[TextP2M].Visible     := V2M;

  Statics[StaticP3R].Visible := V3R;
  Text[TextP3R].Visible     := V3R;

  BadPlayer := AudioInputProcessor.CheckPlayersConfig(PlayersPlay);
  if BadPlayer <> 0 then
  begin
    ScreenPopupError.ShowPopup(
        Format(Language.Translate('ERROR_PLAYER_NO_DEVICE_ASSIGNMENT'),
        [BadPlayer]));
  end;

  // set custom options
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

      Lyrics.LineColor_act.R := 0.02;
      Lyrics.LineColor_act.G := 0.6;
      Lyrics.LineColor_act.B := 0.8;
      Lyrics.LineColor_act.A := 1;
    end;
    1, 2: // outline fonts
    begin
      if (Ini.LyricsFont = 1) then
        Lyrics.FontStyle := ftOutline1
      else
        Lyrics.FontStyle := ftOutline2;

      Lyrics.LineColor_en.R := 0.75;
      Lyrics.LineColor_en.G := 0.75;
      Lyrics.LineColor_en.B := 1;
      Lyrics.LineColor_en.A := 1;

      Lyrics.LineColor_dis.R := 0.8;
      Lyrics.LineColor_dis.G := 0.8;
      Lyrics.LineColor_dis.B := 0.8;
      Lyrics.LineColor_dis.A := 1;

      Lyrics.LineColor_act.R := 0.5;
      Lyrics.LineColor_act.G := 0.5;
      Lyrics.LineColor_act.B := 1;
      Lyrics.LineColor_act.A := 1;
    end;
  end; // case

  // deactivate pause
  Paused := false;

  LoadNextSong();

  Log.LogStatus('End', 'OnShow');
end;

procedure TScreenSing.onShowFinish;
begin
  // hide cursor on singscreen show    
  Display.SetCursor;

  // prepare music
  // Important: AudioPlayback must not be initialized in onShow() as TScreenSong
  // uses stops AudioPlayback in onHide() which interferes with TScreenSings onShow.
  AudioPlayback.Open(CurrentSong.Path.Append(CurrentSong.Mp3));
  if ScreenSong.Mode = smMedley then
    AudioPlayback.SetVolume(0.1)
  else
    AudioPlayback.SetVolume(1.0);
  AudioPlayback.Position := LyricsState.GetCurrentTime();

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


  // start timer
  CountSkipTimeSet;
end;

procedure TScreenSing.SongError();
var
  I, len: integer;

begin
  if ScreenSong.Mode <> smMedley then
  begin
    // error loading song -> go back to previous screen and show some error message
    Display.AbortScreenChange;
    // select new song in party mode
    if ScreenSong.Mode = smPartyMode then
      ScreenSong.SelectRandomSong();
    if Length(CurrentSong.LastError) > 0 then
      ScreenPopupError.ShowPopup(Format(Language.Translate(CurrentSong.LastError), [CurrentSong.ErrorLineNo]))
    else
      ScreenPopupError.ShowPopup(Language.Translate('ERROR_CORRUPT_SONG'));
    // FIXME: do we need this?
    CurrentSong.Path := CatSongs.Song[CatSongs.Selected].Path;
    Exit;
  end
  else
  begin
    if PlaylistMedley.CurrentMedleySong < PlaylistMedley.NumMedleySongs then
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
      if PlaylistMedley.NumMedleySongs = 1 then
      begin
        //Error Loading Song in Medley Mode -> Go back to Song Screen and Show some Error Message
        Display.AbortScreenChange;
        // select new song in party mode
        if ScreenSong.Mode = smPartyMode then
          ScreenSong.SelectRandomSong();
        if Length(CurrentSong.LastError) > 0 then
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
  Color:     TRGB;
  Index:     integer;
  VideoFile: IPath;
  BgFile:    IPath;
  success:   boolean;

  function FindNote(beat: integer): TPos;
  var
    line:  integer;
    note:  integer;
    found: boolean;
    min:   integer;
    diff:  integer;

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
          found:=true;
          break;
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
          min := diff;
        end;
      end;
    end;
  end;

begin
  // reset video playback engine
  fCurrentVideo := nil;

  // setup score manager
  Scores.ClearPlayers; // clear old player values
  Color.R := 0;
  Color.G := 0;
  Color.B := 0; // dummy atm  <- \(O.o)/? B like bummy?

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

  // FIXME: bad style, put the try-except into loadsong() and not here
  try
    // check if file is xml
    if CurrentSong.FileName.GetExtension.ToUTF8 = '.xml' then
      success := CurrentSong.AnalyseXML and CurrentSong.LoadXMLSong()
    else
      success := CurrentSong.Analyse; // and CurrentSong.LoadSong();
  except
    success := false;
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

    Text[SongNameText].Text := IntToStr(PlaylistMedley.CurrentMedleySong) +
      '/' + IntToStr(PlaylistMedley.NumMedleySongs) + ': ' +
      CurrentSong.Artist + ' - ' + CurrentSong.Title;

    //medley start and end timestamps
    StartNote := FindNote(CurrentSong.Medley.StartBeat - round(CurrentSong.BPM[0].BPM*CurrentSong.Medley.FadeIn_time / 60));
    MedleyStart := GetTimeFromBeat(Lines[0].Line[StartNote.line].Note[0].Start);

    //check Medley-Start
    if MedleyStart+CurrentSong.Medley.FadeIn_time * 0.5 > GetTimeFromBeat(CurrentSong.Medley.StartBeat) then
      MedleyStart := GetTimeFromBeat(CurrentSong.Medley.StartBeat) - CurrentSong.Medley.FadeIn_time;
    if MedleyStart < 0 then
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
  if (CurrentSong.Background.IsSet) and (fVideoClip = nil)
    and (TVisualizerOption(Ini.VisualizerOption) = voOff)  then
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

  // main text
  Lyrics.Clear(CurrentSong.BPM[0].BPM, CurrentSong.Resolution);

  // initialize lyrics by filling its queue
  while (not Lyrics.IsQueueFull) and
        (Lyrics.LineCounter <= High(Lines[0].Line)) do
  begin
    Lyrics.AddLine(@Lines[0].Line[Lyrics.LineCounter]);
  end;

  // kill all stars not killed yet (goldenstarstwinkle mod)
  GoldenRec.SentenceChange;

  // set position of line bonus - line bonus end
  // set number of empty sentences for line bonus
  NumEmptySentences := 0;
  for Index := Low(Lines[0].Line) to High(Lines[0].Line) do
    if Lines[0].Line[Index].TotalNotes = 0 then
      Inc(NumEmptySentences);

  eSongLoaded.CallHookChain(false);

  if (ScreenSong.Mode = smMedley) and (PlaylistMedley.CurrentMedleySong > 1) then
    onShowFinish;
end;

procedure TScreenSing.ClearSettings;
begin
  Settings.Finish := false;
  Settings.LyricsVisible := true;
  Settings.NotesVisible := high(integer);
  Settings.PlayerEnabled := high(integer);
end;

{ applies changes of settings record }
procedure TScreenSing.ApplySettings;
begin
  //
end;

procedure TScreenSing.EndSong;
begin
  Settings.Finish := true;
end;

procedure TScreenSing.OnHide;
begin
  // background texture
  if Tex_Background.TexNum > 0 then
  begin
    glDeleteTextures(1, PGLuint(@Tex_Background.TexNum));
    Tex_Background.TexNum := 0;
  end;

  Background.OnFinish;
  Display.SetCursor;
end;

function TScreenSing.Draw: boolean;
var
  DisplayTime:           real;
  DisplayPrefix:         string;
  DisplayMin:            integer;
  DisplaySec:            integer;
  T:                     integer;
  CurLyricsTime:         real;
  TotalTime:             real;
  VideoFrameTime:        extended;
  Line:                  TLyricLine;
  LastWord:              TLyricWord;
  medley_end:            boolean;
  medley_start_applause: boolean;

begin
  Background.Draw;

  // draw background picture (if any, and if no visualizations)
  // when we don't check for visualizations the visualizations would
  // be overdrawn by the picture when {UNDEFINED UseTexture} in UVisualizer
  if (not fShowVisualization) then
    SingDrawBackground;

  // set player names (for 2 screens and only singstar skin)
  if ScreenAct = 1 then
  begin
    Text[TextP1].Text     := 'P1';
    Text[TextP1TwoP].Text := 'P1';
    Text[TextP1ThreeP].Text := 'P1';
    Text[TextP2R].Text    := 'P2';
    Text[TextP2M].Text    := 'P2';
    Text[TextP3R].Text    := 'P3';
  end;

  if ScreenAct = 2 then
  begin
    case PlayersPlay of
      4:
      begin
        Text[TextP1TwoP].Text := 'P3';
        Text[TextP2R].Text    := 'P4';
      end;
      6:
      begin
        Text[TextP1ThreeP].Text := 'P4';
        Text[TextP2M].Text      := 'P5';
        Text[TextP3R].Text      := 'P6';
      end;
    end; // case
  end; // if

  // retrieve current lyrics time, we have to store the value to avoid
  // that min- and sec-values do not match
  if ScreenSong.Mode = smMedley then
  begin
    CurLyricsTime := LyricsState.GetCurrentTime() - ScreenSing.MedleyStart;
    TotalTime := ScreenSing.MedleyEnd - ScreenSing.MedleyStart;
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
  Line := Lyrics.GetUpperLine();
  if Line.LastLine then
  begin
    LastWord := Line.Words[Length(Line.Words)-1];
    if CurLyricsTime >= GetTimeFromBeat(LastWord.Start+LastWord.Length) then
      SungToEnd := true;
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
  if Assigned(fCurrentVideo) then
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

  // draw static menu (FG)
  DrawFG;

  //Medley Countdown
  if ScreenSong.Mode = smMedley then
    DrawMedleyCountdown;

  // check for music finish
  //Log.LogError('Check for music finish: ' + BoolToStr(Music.Finished) + ' ' + FloatToStr(LyricsState.CurrentTime*1000) + ' ' + IntToStr(CurrentSong.Finish));
  if ShowFinish then
  begin
    if (not AudioPlayback.Finished) and (not medley_end or (ScreenSong.Mode <> smMedley)) and
      ((CurrentSong.Finish = 0) or (LyricsState.GetCurrentTime()*1000 <= CurrentSong.Finish)) and
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
      if (not FadeOut) and (Screens = 1) or (ScreenAct = 2) then
      begin
        Finish;
      end;
    end;
  end;

  // always draw custom items
  SingDraw;

  // goldennotestarstwinkle
  GoldenRec.SpawnRec;

  // draw scores
  Scores.Draw;

  // draw pausepopup
  // FIXME: this is a workaround that the static is drawn over the lyrics, lines, scores and effects
  // maybe someone could find a better solution
  if Paused then
  begin
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

procedure TScreenSing.OnSentenceEnd(SentenceIndex: cardinal);
var
  PlayerIndex:    byte;
  CurrentPlayer:  PPLayer;
  CurrentScore:   real;
  Line:           PLine;
  LinePerfection: real;  // perfection of singing performance on the current line
  Rating:         integer;
  LineScore:      real;
  LineBonus:      real;
  MaxSongScore:   integer; // max. points for the song (without line bonus)
  MaxLineScore:   real;    // max. points for the current line

const
  // TODO: move this to a better place
  MAX_LINE_RATING = 8;        // max. rating for singing performance

begin
  Line := @Lines[0].Line[SentenceIndex];

  // check for empty sentence
  if Line.TotalNotes <= 0 then
    Exit;

  // set max song score
  if Ini.LineBonus = 0 then
    MaxSongScore := MAX_SONG_SCORE
  else
    MaxSongScore := MAX_SONG_SCORE - MAX_SONG_LINE_BONUS;

  // Note: ScoreValue is the sum of all note values of the song
  MaxLineScore := MaxSongScore * (Line.TotalNotes / Lines[0].ScoreValue);

  for PlayerIndex := 0 to High(Player) do
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
      LineBonus := MAX_SONG_LINE_BONUS / (Length(Lines[0].Line) -
        NumEmptySentences);
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

  // PerfectLineTwinkle (effect), part 2
  if Ini.EffectSing = 1 then
    GoldenRec.SpawnPerfectLineTwinkle;
end;

 // Called on sentence change
 // SentenceIndex: index of the new active sentence
procedure TScreenSing.OnSentenceChange(SentenceIndex: cardinal);
begin
  // goldenstarstwinkle
  GoldenRec.SentenceChange;

  // fill lyrics queue and set upper line to the current sentence
  while (Lyrics.GetUpperLineIndex() < SentenceIndex) or
    (not Lyrics.IsQueueFull) do
  begin
    // add the next line to the queue or a dummy if no more lines are available
    if Lyrics.LineCounter <= High(Lines[0].Line) then
      Lyrics.AddLine(@Lines[0].Line[Lyrics.LineCounter])
    else
      Lyrics.AddLine(nil);
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
  len, num, I: integer;

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
    (PlaylistMedley.CurrentMedleySong <= PlaylistMedley.NumMedleySongs) then
  begin
    PlaylistMedley.ApplausePlayed := true;
    AudioPlayback.Fade(CurrentSong.Medley.FadeOut_time, 0.1);
    AudioPlayback.PlaySound(SoundLib.Applause);
  end;
end;

procedure TScreenSing.DrawMedleyCountdown();
var
  w, h:          real;
  timeDiff:      real;
  t:             real;
  CountDownText: UTF8String;

begin
  if AudioPlayback.Position < GetTimeFromBeat(CurrentSong.Medley.StartBeat) then
  begin
    Statics[SongNameStatic].Visible := true;
    Text[SongNameText].Visible := true;

    timeDiff := GetTimeFromBeat(CurrentSong.Medley.StartBeat) - AudioPlayback.Position + 1;
    t := frac(timeDiff);

    glColor4f(0.15, 0.30, 0.6, t);

    h := 300 * t * ScreenH / RenderH;
    SetFontStyle(ftBoldHighRes);
    SetFontItalic(false);
    SetFontSize(h);
    CountDownText := IntToStr(round(timeDiff - t));
    w := glTextWidth(PChar(CountDownText));

    SetFontPos (RenderW / 2 - w / 2, RenderH / 2 - h / 2);
    glPrint(PChar(CountDownText));
  end
  else
  begin
    Statics[SongNameStatic].Visible := false;
    Text[SongNameText].Visible := false;
  end;
end;
end;

end.
