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

unit UBeatTimer;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UTime;

type
  (**
   * TLyricsState contains all information concerning the
   * state of the lyrics, e.g. the current beat or duration of the lyrics.
   *)
  TLyricsState = class
    private
      fTimer:        TRelativeTimer; // keeps track of the current time
      fSyncSource:   TSyncSource;
      fAvgSyncDiff:  real;
      fLastClock:    real;       // last master clock value
      // Note: do not use Timer.GetState() to check if lyrics are paused as
      // Timer.Pause() is used for synching.
      fPaused:       boolean;

      function Synchronize(LyricTime: real): real;
    public
      OldBeat:      integer;    // previous discovered beat
      CurrentBeat:  integer;    // current beat (rounded)
      MidBeat:      real;       // current beat (float)

      // now we use this for super synchronization!
      // only used when analyzing voice
      // TODO: change ...D to ...Detect(ed)
      OldBeatD:     integer;    // previous discovered beat
      CurrentBeatD: integer;    // current discovered beat (rounded)
      MidBeatD:     real;       // current discovered beat (float)

      // we use this for audible clicks
      // TODO: Change ...C to ...Click
      OldBeatC:     integer;    // previous discovered beat
      CurrentBeatC: integer;
      MidBeatC:     real;       // like CurrentBeatC

      OldLine:      integer;    // previous displayed sentence

      StartTime:    real;       // time till start of lyrics (= Gap)
      TotalTime:    real;       // total song time

      constructor Create();

      {**
       * Resets the LyricsState state.
       *}
      procedure Reset();

      procedure UpdateBeats();

      {**
       * Sets a master clock for this LyricsState. If no sync-source is set
       * or SyncSource is nil the internal timer is used.
       *}
      procedure SetSyncSource(SyncSource: TSyncSource);

      {**
       * Starts the timer. This is either done
       * - immediately if WaitForTrigger is false or
       * - after the first call to GetCurrentTime()/SetCurrentTime() or Start(false)
       *}
      procedure Start(WaitForTrigger: boolean = false);

      {**
       * Pauses the timer.
       * The counter is preserved and can be resumed by a call to Start().
       *}
      procedure Pause();

      {**
       * Stops the timer.
       * The counter is reset to 0.
       *}
      procedure Stop();

      (**
       * Returns/Sets the current song time (in seconds) used as base-timer for lyrics etc.
       * If GetCurrentTime()/SetCurrentTime() if Start() was called
       *)
      function GetCurrentTime(): real;
      procedure SetCurrentTime(Time: real);
  end;

implementation

uses
  UNote,
  ULog,
  SysUtils,
  Math;


constructor TLyricsState.Create();
begin
  // create a triggered timer, so we can Pause() it, set the time
  // and Resume() it afterwards for better synching.
  fTimer := TRelativeTimer.Create();

  // reset state
  Reset();
end;

procedure TLyricsState.Pause();
begin
  fTimer.Pause();
  fPaused := true;
end;

procedure TLyricsState.Start(WaitForTrigger: boolean);
begin
  fTimer.Start(WaitForTrigger);
  fPaused := false;
  fLastClock := -1;
  fAvgSyncDiff := -1;
end;

procedure TLyricsState.Stop();
begin
  fTimer.Stop();
  fPaused := false;
end;

procedure TLyricsState.SetCurrentTime(Time: real);
begin
  fTimer.SetTime(Time);
  fLastClock := -1;
  fAvgSyncDiff := -1;
end;

{.$DEFINE LOG_SYNC}

function TLyricsState.Synchronize(LyricTime: real): real;
var
  MasterClock: real;
  TimeDiff: real;
const
  AVG_HISTORY_FACTOR = 0.7;
  PAUSE_THRESHOLD = 0.010; // 10ms
  FORWARD_THRESHOLD = 0.010; // 10ms
begin
  MasterClock := fSyncSource.GetClock();
  Result := LyricTime;

  // do not sync if lyrics are paused externally or if the timestamp is old
  if (fPaused or (MasterClock = fLastClock)) then
    Exit;

  // calculate average time difference (some sort of weighted mean).
  // The bigger AVG_HISTORY_FACTOR is, the smoother is the average diff.
  // This is done as some timestamps might be wrong or even lower
  // than their predecessor.
  TimeDiff := MasterClock - LyricTime;
  if (fAvgSyncDiff = -1) then
    fAvgSyncDiff := TimeDiff
  else
    fAvgSyncDiff := TimeDiff * (1-AVG_HISTORY_FACTOR) +
                    fAvgSyncDiff * AVG_HISTORY_FACTOR;

  {$IFDEF LOG_SYNC}
  //Log.LogError(Format('TimeDiff: %.3f', [TimeDiff]));
  {$ENDIF}

  // do not go backwards in time as this could mess up the score
  if (fAvgSyncDiff > FORWARD_THRESHOLD) then
  begin
    {$IFDEF LOG_SYNC}
    Log.LogError('Sync: ' + floatToStr(MasterClock) + ' > ' + floatToStr(LyricTime));
    {$ENDIF}

    Result := LyricTime + fAvgSyncDiff;
    fTimer.SetTime(Result);
    fTimer.Start();
    fAvgSyncDiff := -1;
  end
  else if (fAvgSyncDiff < -PAUSE_THRESHOLD) then
  begin
    // wait until timer and master clock are in sync (> 10ms)
    fTimer.Pause();

    {$IFDEF LOG_SYNC}
    Log.LogError('Pause: ' + floatToStr(MasterClock) + ' < ' + floatToStr(LyricTime));
    {$ENDIF}
  end
  else if (fTimer.GetState = rtsPaused) and (fAvgSyncDiff >= 0) then
  begin
    fTimer.Start();

    {$IFDEF LOG_SYNC}
    Log.LogError('Unpause: ' + floatToStr(LyricTime));
    {$ENDIF}
  end;
  fLastClock := MasterClock;
end;

function TLyricsState.GetCurrentTime(): real;
var
  LyricTime: real;
begin
  LyricTime := fTimer.GetTime();
  if Assigned(fSyncSource) then
    Result := Synchronize(LyricTime)
  else
    Result := LyricTime;
end;

procedure TLyricsState.SetSyncSource(SyncSource: TSyncSource);
begin
  fSyncSource := SyncSource;
end;

(**
 * Resets the timer and state of the lyrics.
 * The timer will be stopped afterwards so you have to call Resume()
 * to start the lyrics timer. 
 *)
procedure TLyricsState.Reset();
begin
  Stop();
  fPaused := false;

  fSyncSource := nil;

  StartTime := 0;
  TotalTime := 0;

  OldBeat      := -1;
  MidBeat      := -1;
  CurrentBeat  := -1;

  OldBeatC     := -1;
  MidBeatC     := -1;
  CurrentBeatC := -1;

  OldBeatD     := -1;
  MidBeatD     := -1;
  CurrentBeatD := -1;
end;

(**
 * Updates the beat information (CurrentBeat/MidBeat/...) according to the
 * current lyric time.
 *)
procedure TLyricsState.UpdateBeats();
var
  CurLyricsTime: real;
begin
  CurLyricsTime := GetCurrentTime();

  OldBeat := CurrentBeat;
  MidBeat := GetMidBeat(CurLyricsTime - StartTime / 1000);
  CurrentBeat := Floor(MidBeat);

  OldBeatC := CurrentBeatC;
  MidBeatC := GetMidBeat(CurLyricsTime - StartTime / 1000);
  CurrentBeatC := Floor(MidBeatC);

  OldBeatD := CurrentBeatD;
  // MidBeatD = MidBeat with additional GAP
  MidBeatD := -0.5 + GetMidBeat(CurLyricsTime - (StartTime + 120 + 20) / 1000);
  CurrentBeatD := Floor(MidBeatD);
end;

end.