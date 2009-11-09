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

unit UNote;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  Classes,
  SDL,
  UMusic,
  URecord,
  UTime,
  UDisplay,
  UIni,
  ULog,
  ULyrics,
  UScreenSing,
  USong,
  gl;

type
  PPLayerNote = ^TPlayerNote;
  TPlayerNote = record
    Start:   integer;
    Length:  integer;
    Detect:  real;    // accurate place, detected in the note
    Tone:    real;
    Perfect: boolean; // true if the note matches the original one, light the star
    Hit:     boolean; // true if the note hits the line
  end;

  PPLayer = ^TPlayer;
  TPlayer = record
    Name:           UTF8String;

    // Index in Teaminfo record
    TeamID:         byte;
    PlayerID:       byte;

    // Scores
    Score:          real;
    ScoreLine:      real;
    ScoreGolden:    real;

    ScoreInt:       integer;
    ScoreLineInt:   integer;
    ScoreGoldenInt: integer;
    ScoreTotalInt:  integer;

    // LineBonus
    ScoreLast:      real;    // Last Line Score

    // PerfectLineTwinkle (effect)
    LastSentencePerfect: boolean;

    HighNote:       integer; // index of last note (= High(Note)?)
    LengthNote:     integer; // number of notes (= Length(Note)?).
    Note:           array of TPlayerNote;
  end;

var

  // player and music info
  Player:      array of TPlayer;
  PlayersPlay: integer;

  CurrentSong: TSong;

const
  MAX_SONG_SCORE = 10000;     // max. achievable points per song
  MAX_SONG_LINE_BONUS = 1000; // max. achievable line bonus per song

procedure Sing(Screen: TScreenSing);
procedure NewSentence(Screen: TScreenSing);
procedure NewBeatClick(Screen: TScreenSing);  // executed when on then new beat for click
procedure NewBeatDetect(Screen: TScreenSing); // executed when on then new beat for detection
procedure NewNote(Screen: TScreenSing);       // detect note
function  GetMidBeat(Time: real): real;
function  GetTimeFromBeat(Beat: integer): real;

implementation

uses
  Math,
  StrUtils,
  USongs,
  UJoystick,
  UCommandLine,
  ULanguage,
  //SDL_ttf,
  USkins,
  UCovers,
  UCatCovers,
  UDataBase,
  UPlaylist,
  UDLLManager,
  UParty,
  UConfig,
  UCommon,
  UGraphic,
  UGraphicClasses,
  UPathUtils,
  UPlatform,
  UThemes;

function GetTimeForBeats(BPM, Beats: real): real;
begin
  Result := 60 / BPM * Beats;
end;

function GetBeats(BPM, msTime: real): real;
begin
  Result := BPM * msTime / 60;
end;

procedure GetMidBeatSub(BPMNum: integer; var Time: real; var CurBeat: real);
var
  NewTime: real;
begin
  if High(CurrentSong.BPM) = BPMNum then
  begin
    // last BPM
    CurBeat := CurrentSong.BPM[BPMNum].StartBeat + GetBeats(CurrentSong.BPM[BPMNum].BPM, Time);
    Time := 0;
  end
  else
  begin
    // not last BPM
    // count how much time is it for start of the new BPM and store it in NewTime
    NewTime := GetTimeForBeats(CurrentSong.BPM[BPMNum].BPM, CurrentSong.BPM[BPMNum+1].StartBeat - CurrentSong.BPM[BPMNum].StartBeat);

    // compare it to remaining time
    if (Time - NewTime) > 0 then
    begin
      // there is still remaining time
      CurBeat := CurrentSong.BPM[BPMNum].StartBeat;
      Time := Time - NewTime;
    end
    else
    begin
      // there is no remaining time
      CurBeat := CurrentSong.BPM[BPMNum].StartBeat + GetBeats(CurrentSong.BPM[BPMNum].BPM, Time);
      Time := 0;
    end; // if
  end; // if
end;

function GetMidBeat(Time: real): real;
var
  CurBeat: real;
  CurBPM:  integer;
begin
  // static BPM
  if Length(CurrentSong.BPM) = 1 then
  begin
    Result := Time * CurrentSong.BPM[0].BPM / 60;
  end
  // variable BPM
  else if Length(CurrentSong.BPM) > 1 then
  begin
    CurBeat := 0;
    CurBPM := 0;
    while (Time > 0) do
    begin
      GetMidBeatSub(CurBPM, Time, CurBeat);
      Inc(CurBPM);
    end;

    Result := CurBeat;
  end
  // invalid BPM
  else
  begin
    Result := 0;
  end;
end;

function GetTimeFromBeat(Beat: integer): real;
var
  CurBPM: integer;
begin
  // static BPM
  if Length(CurrentSong.BPM) = 1 then
  begin
    Result := CurrentSong.GAP / 1000 + Beat * 60 / CurrentSong.BPM[0].BPM;
  end
  // variable BPM
  else if Length(CurrentSong.BPM) > 1 then
  begin
    Result := CurrentSong.GAP / 1000;
    CurBPM := 0;
    while (CurBPM <= High(CurrentSong.BPM)) and
          (Beat > CurrentSong.BPM[CurBPM].StartBeat) do
    begin
      if (CurBPM < High(CurrentSong.BPM)) and
         (Beat >= CurrentSong.BPM[CurBPM+1].StartBeat) then
      begin
        // full range
        Result := Result + (60 / CurrentSong.BPM[CurBPM].BPM) *
                           (CurrentSong.BPM[CurBPM+1].StartBeat - CurrentSong.BPM[CurBPM].StartBeat);
      end;

      if (CurBPM = High(CurrentSong.BPM)) or
         (Beat < CurrentSong.BPM[CurBPM+1].StartBeat) then
      begin
        // in the middle
        Result := Result + (60 / CurrentSong.BPM[CurBPM].BPM) *
                           (Beat - CurrentSong.BPM[CurBPM].StartBeat);
      end;
      Inc(CurBPM);
    end;

    {
    while (Time > 0) do
    begin
      GetMidBeatSub(CurBPM, Time, CurBeat);
      Inc(CurBPM);
    end;
    }
  end
  // invalid BPM
  else
  begin
    Result := 0;
  end;
end;

procedure Sing(Screen: TScreenSing);
var
  Count:   integer;
  CountGr: integer;
  CP:      integer;
begin
  LyricsState.UpdateBeats();

  // sentences routines
  for CountGr := 0 to 0 do //High(Lines)
  begin;
    CP := CountGr;
    // old parts
    LyricsState.OldLine := Lines[CP].Current;

    // choose current parts
    for Count := 0 to Lines[CP].High do
    begin
      if LyricsState.CurrentBeat >= Lines[CP].Line[Count].Start then
        Lines[CP].Current := Count;
    end;

    // clean player note if there is a new line
    // (optimization on halfbeat time)
    if Lines[CP].Current <> LyricsState.OldLine then
      NewSentence(Screen);

  end; // for CountGr

  // make some operations on clicks
  if {(LyricsState.CurrentBeatC >= 0) and }(LyricsState.OldBeatC <> LyricsState.CurrentBeatC) then
    NewBeatClick(Screen);

  // make some operations when detecting new voice pitch
  if (LyricsState.CurrentBeatD >= 0) and (LyricsState.OldBeatD <> LyricsState.CurrentBeatD) then
    NewBeatDetect(Screen);
end;

procedure NewSentence(Screen: TScreenSing);
var
  i: integer;
begin
  // clean note of player
  for i := 0 to High(Player) do
  begin
    Player[i].LengthNote := 0;
    Player[i].HighNote := -1;
    SetLength(Player[i].Note, 0);
  end;

  // on sentence change...
  Screen.onSentenceChange(Lines[0].Current);
end;

procedure NewBeatClick;
var
  Count: integer;
begin
  // beat click
  if ((Ini.BeatClick = 1) and
      ((LyricsState.CurrentBeatC + Lines[0].Resolution + Lines[0].NotesGAP) mod Lines[0].Resolution = 0)) then
  begin
    AudioPlayback.PlaySound(SoundLib.Click);
  end;

  for Count := 0 to Lines[0].Line[Lines[0].Current].HighNote do
  begin
    if (Lines[0].Line[Lines[0].Current].Note[Count].Start = LyricsState.CurrentBeatC) then
    begin
      // click assist
      if Ini.ClickAssist = 1 then
        AudioPlayback.PlaySound(SoundLib.Click);

      // drum machine
      (*
      TempBeat := LyricsState.CurrentBeat; // + 2;
      if (TempBeat mod 8 = 0) then Music.PlayDrum;
      if (TempBeat mod 8 = 4) then Music.PlayClap;
      //if (TempBeat mod 4 = 2) then Music.PlayHihat;
      if (TempBeat mod 4 <> 0) then Music.PlayHihat;
      *)
    end;
  end;
end;

procedure NewBeatDetect(Screen: TScreenSing);
begin
  NewNote(Screen);
end;

procedure NewNote(Screen: TScreenSing);
var
  LineFragmentIndex:   integer;
  CurrentLineFragment: PLineFragment;
  PlayerIndex:         integer;
  CurrentSound:        TCaptureBuffer;
  CurrentPlayer:       PPlayer;
  LastPlayerNote:      PPlayerNote;
  Line: 	       PLine;
  SentenceIndex:       integer;
  SentenceMin:         integer;
  SentenceMax:         integer;
  SentenceDetected:    integer; // sentence of detected note
  NoteAvailable:       boolean;
  NewNote:             boolean;
  Range:               integer;
  NoteHit:             boolean;
  MaxSongPoints:       integer; // max. points for the song (without line bonus)
  CurNotePoints:       real;    // Points for the cur. Note (PointsperNote * ScoreFactor[CurNote])
begin
  // TODO: add duet mode support
  // use Lines[LineSetIndex] with LineSetIndex depending on the current player

  // count min and max sentence range for checking 
  // (detection is delayed to the notes we see on the screen)
  SentenceMin := Lines[0].Current-1;
  if (SentenceMin < 0) then
    SentenceMin := 0;
  SentenceMax := Lines[0].Current;

  // check for an active note at the current time defined in the lyrics
  NoteAvailable := false;
  SentenceDetected := SentenceMin;
  for SentenceIndex := SentenceMin to SentenceMax do
  begin
    Line := @Lines[0].Line[SentenceIndex];
    for LineFragmentIndex := 0 to Line.HighNote do
    begin
      CurrentLineFragment := @Line.Note[LineFragmentIndex];
      // check if line is active
      if ((CurrentLineFragment.Start <= LyricsState.CurrentBeatD) and
          (CurrentLineFragment.Start + CurrentLineFragment.Length-1 >= LyricsState.CurrentBeatD)) and
         (CurrentLineFragment.NoteType <> ntFreestyle) and       // but ignore FreeStyle notes
         (CurrentLineFragment.Length > 0) then                   // and make sure the note length is at least 1
      begin
        SentenceDetected := SentenceIndex;
        NoteAvailable := true;
        Break;
      end;
    end;
    // TODO: break here, if NoteAvailable is true? We would then use the first instead
    // of the last note matching the current beat if notes overlap. But notes
    // should not overlap at all.
    // if (NoteAvailable) then
    //  Break;
  end;

  // analyze player signals
  for PlayerIndex := 0 to PlayersPlay-1 do
  begin
    CurrentPlayer := @Player[PlayerIndex];
    CurrentSound := AudioInputProcessor.Sound[PlayerIndex];

    // at the beginning of the song there is no previous note
    if (Length(CurrentPlayer.Note) > 0) then
      LastPlayerNote := @CurrentPlayer.Note[CurrentPlayer.HighNote]
    else
      LastPlayerNote := nil;

    // analyze buffer
    CurrentSound.AnalyzeBuffer;

    // add some noise
    // TODO: do we need this?
    //LyricsState.Tone := LyricsState.Tone + Round(Random(3)) - 1;

    // add note if possible
    if (CurrentSound.ToneValid and NoteAvailable) then
    begin
      Line := @Lines[0].Line[SentenceDetected];

      // process until last note
      for LineFragmentIndex := 0 to Line.HighNote do
      begin
        CurrentLineFragment := @Line.Note[LineFragmentIndex];
        if (CurrentLineFragment.Start <= LyricsState.OldBeatD+1) and
           (CurrentLineFragment.Start + CurrentLineFragment.Length > LyricsState.OldBeatD+1) then
        begin
          // compare notes (from song-file and from player)

          // move players tone to proper octave
          while (CurrentSound.Tone - CurrentLineFragment.Tone > 6) do
            CurrentSound.Tone := CurrentSound.Tone - 12;

          while (CurrentSound.Tone - CurrentLineFragment.Tone < -6) do
            CurrentSound.Tone := CurrentSound.Tone + 12;

          // half size notes patch
          NoteHit := false;

          // if Ini.Difficulty = 0 then Range := 2;
          // if Ini.Difficulty = 1 then Range := 1;
          // if Ini.Difficulty = 2 then Range := 0;
          Range := 2 - Ini.Difficulty;

          // check if the player hit the correct tone within the tolerated range
          if (Abs(CurrentLineFragment.Tone - CurrentSound.Tone) <= Range) then
          begin
            // adjust the players tone to the correct one
            // TODO: do we need to do this?
            // Philipp: I think we do, at least when we draw the notes.
            //          Otherwise the notehit thing would be shifted to the
            //          correct unhit note. I think this will look kind of strange.
            CurrentSound.Tone := CurrentLineFragment.Tone;

            // half size notes patch
            NoteHit := true;

            if (Ini.LineBonus > 0) then
              MaxSongPoints := MAX_SONG_SCORE - MAX_SONG_LINE_BONUS
            else
              MaxSongPoints := MAX_SONG_SCORE;

            // Note: ScoreValue is the sum of all note values of the song
            // (MaxSongPoints / ScoreValue) is the points that a player
            // gets for a hit of one beat of a normal note
            // CurNotePoints is the amount of points that is meassured
            // for a hit of the note per full beat
            CurNotePoints := (MaxSongPoints / Lines[0].ScoreValue) * ScoreFactor[CurrentLineFragment.NoteType];
            
            case CurrentLineFragment.NoteType of
              ntNormal: CurrentPlayer.Score       := CurrentPlayer.Score       + CurNotePoints;
              ntGolden: CurrentPlayer.ScoreGolden := CurrentPlayer.ScoreGolden + CurNotePoints;
            end;

            // a problem if we use floor instead of round is that a score of
            // 10000 points is only possible if the last digit of the total points
            // for golden and normal notes is 0.
            // if we use round, the max score is 10000 for most songs
            // but a score of 10010 is possible if the last digit of the total
            // points for golden and normal notes is 5
            // the best solution is to use round for one of these scores
            // and round the other score in the opposite direction
            // so we assure that the highest possible score is 10000 in every case.
            CurrentPlayer.ScoreInt := round(CurrentPlayer.Score / 10) * 10;

            if (CurrentPlayer.ScoreInt < CurrentPlayer.Score) then
              //normal score is floored so we have to ceil golden notes score
              CurrentPlayer.ScoreGoldenInt := ceil(CurrentPlayer.ScoreGolden / 10) * 10
            else
              //normal score is ceiled so we have to floor golden notes score
              CurrentPlayer.ScoreGoldenInt := floor(CurrentPlayer.ScoreGolden / 10) * 10;


            CurrentPlayer.ScoreTotalInt := CurrentPlayer.ScoreInt +
                                           CurrentPlayer.ScoreGoldenInt +
                                           CurrentPlayer.ScoreLineInt;
          end;

        end; // operation
      end; // for

      // check if we have to add a new note or extend the note's length
      if (SentenceDetected = SentenceMax) then
      begin
        // we will add a new note
        NewNote := true;

        // if previous note (if any) was the same, extend previous note
        if ((CurrentPlayer.LengthNote > 0) and
            (LastPlayerNote <> nil) and
            (LastPlayerNote.Tone = CurrentSound.Tone) and
            ((LastPlayerNote.Start + LastPlayerNote.Length) = LyricsState.CurrentBeatD)) then
        begin
          NewNote := false;
        end;

        // if is not as new note to control
        for LineFragmentIndex := 0 to Line.HighNote do
        begin
          if (Line.Note[LineFragmentIndex].Start = LyricsState.CurrentBeatD) then
            NewNote := true;
        end;

        // add new note
        if NewNote then
        begin
          // new note
          Inc(CurrentPlayer.LengthNote);
          Inc(CurrentPlayer.HighNote);
          SetLength(CurrentPlayer.Note, CurrentPlayer.LengthNote);

          // update player's last note
          LastPlayerNote := @CurrentPlayer.Note[CurrentPlayer.HighNote];
          with LastPlayerNote^ do
          begin
            Start  := LyricsState.CurrentBeatD;
            Length := 1;
            Tone   := CurrentSound.Tone; // Tone || ToneAbs
            Detect := LyricsState.MidBeat;
            Hit    := NoteHit; // half note patch
          end;
        end
        else
        begin
          // extend note length
          if (LastPlayerNote <> nil) then
            Inc(LastPlayerNote.Length);
        end;

        // check for perfect note and then light the star (on Draw)
        for LineFragmentIndex := 0 to Line.HighNote do
        begin
          CurrentLineFragment := @Line.Note[LineFragmentIndex];
          if (CurrentLineFragment.Start  = LastPlayerNote.Start) and
             (CurrentLineFragment.Length = LastPlayerNote.Length) and
             (CurrentLineFragment.Tone   = LastPlayerNote.Tone) then
          begin
            LastPlayerNote.Perfect := true;
          end;
        end;
      end; // if SentenceDetected = SentenceMax

    end; // if Detected
  end; // for PlayerIndex

  //Log.LogStatus('EndBeat', 'NewBeat');

  // on sentence end -> for LineBonus and display of SingBar (rating pop-up)
  if (SentenceDetected >= Low(Lines[0].Line)) and
     (SentenceDetected <= High(Lines[0].Line)) then
  begin
    Line := @Lines[0].Line[SentenceDetected];
    CurrentLineFragment := @Line.Note[Line.HighNote];
    if ((CurrentLineFragment.Start + CurrentLineFragment.Length - 1) = LyricsState.CurrentBeatD) then
    begin
      if assigned(Screen) then
        Screen.OnSentenceEnd(SentenceDetected);
    end;
  end;

end;

end.
