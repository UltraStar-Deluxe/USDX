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
      Timer:        TRelativeTimer; // keeps track of the current time
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
      procedure Pause();
      procedure Resume();

      procedure Reset();
      procedure UpdateBeats();

      (**
       * current song time (in seconds) used as base-timer for lyrics etc.
       *)
      function GetCurrentTime(): real;
      procedure SetCurrentTime(Time: real);
  end;

implementation
uses UNote, Math;


constructor TLyricsState.Create();
begin
  // create a triggered timer, so we can Pause() it, set the time
  // and Resume() it afterwards for better synching.
  Timer := TRelativeTimer.Create(true);

  // reset state
  Reset();
end;

procedure TLyricsState.Pause();
begin
  Timer.Pause();
end;

procedure TLyricsState.Resume();
begin
  Timer.Resume();
end;

procedure TLyricsState.SetCurrentTime(Time: real);
begin
  // do not start the timer (if not started already),
  // after setting the current time
  Timer.SetTime(Time, false);
end;

function TLyricsState.GetCurrentTime(): real;
begin
  Result := Timer.GetTime();
end;

(**
 * Resets the timer and state of the lyrics.
 * The timer will be stopped afterwards so you have to call Resume()
 * to start the lyrics timer. 
 *)
procedure TLyricsState.Reset();
begin
  Pause();
  SetCurrentTime(0);

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