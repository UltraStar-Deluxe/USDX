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

unit UBeatNoteTimer;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMusic, // Generically, the song structure, notes
  UIni, // For the number of players //
  UCommon,
  UScreenSingController;

type
  // Elementary storage, for a given player, of the currently playing
  // beat or beat silence note (or nil for the LineFragment field
  // if none of these notes is playing)
  TPlayerBeatNoteState = record
    CurrentBeat:  integer;    // current beat (rounded)
    LineFragment: PLineFragment; // Points to current line fragment (note) for each player or nil if none available
    NoteHit: boolean;
    NoteHitExactBeatTime: real;
  end;




  type
   // This class acts to indicate the state of the advancing song in terms of beat
  // notes / silences playing at a given time. For each player
  // this information is stored as a record of type TPlayerBeatNoteState
  TBeatNoteTimerState = class // This class handles indication of beat/beat silence notes playing (or not)
    public
    playerBeatNoteState: array of TPlayerBeatNoteState; // Array indicating where each player is at present
    playerLastHitBeatTime: array of real; // Array keeping track of players last hit in terms of lyrics time
    constructor Create;
    procedure analyzeBeatNoteTiming(Screen: TScreenSingController); // Analyze present timing for all player to see whether ther are currently
                     // Beat note or beat breaks playing
    procedure analyzeBeatNoteTimingForPlayer(CP: integer); // Analysis for a given player
    procedure analyzeBeatNoteTimingForPlayerOld(CP: integer); // Analysis for a given player
    function doBeatNoteDetection(CP: integer; ActualBeat: real):Boolean; // Function that indicates whether for a given player, beat note/break detection should be done
     // at present. This is the case if the current beat note/break has not yet been hit on the current beat (breaks can have a longer duration)
    procedure notifyNoteHit(CP: integer; ActualBeat: real); // The current player has hit the current note, update the lcoal variables correspondingly
  end;

procedure createTBeatNoteTimerState();

// global singleton for the TBeatNoteTimerState class
var BeatNoteTimerState : TBeatNoteTimerState;

implementation

uses
  ULyrics,
  UNote,
  SysUtils,
  UBeatNote,
  Math;

// Instantiate the singleton if necessary
procedure createTBeatNoteTimerState();
begin
   if BeatNoteTimerState = nil then
      BeatNoteTimerState := TBeatNoteTimerState.Create();
end;

// Constructor with default values, with none of the players having a note playing
constructor TBeatNoteTimerState.Create;
var
  count: integer;
begin
   setLength(playerBeatNoteState, high(IKeyPlayLetters)+1);
   for count:= 0 to high(IKeyPlayLetters) do
   begin
       playerBeatNoteState[count].CurrentBeat:=-100;  // usually current beat is positve or a few beats negative
       playerBeatNoteState[count].LineFragment:=nil;
       playerBeatNoteState[count].NoteHit:=false;
       playerBeatNoteState[count].NoteHitExactBeatTime:=-100.0; // same as current beat, just this is float
   end;

end;

// As a function of the present time in the song, determine the beat/beat silence notes playing
// for the different players (if any)
procedure TBeatNoteTimerState.analyzeBeatNoteTiming(Screen: TScreenSingController);
var
  MaxCP, PlayerIndex: integer;
  CP: integer;
  J: cardinal;
begin

  MaxCP := 0;
  if (CurrentSong.isDuet) and (PlayersPlay <> 1) then
    MaxCP := 1;

  if (assigned(Screen)) then
  begin
    for J := 0 to MaxCP do
    begin
      CP := J;

  for PlayerIndex := 0 to PlayersPlay-1 do
    begin
      if (not CurrentSong.isDuet) or (PlayerIndex mod 2 = CP) then
      begin
         analyzeBeatNoteTimingForPlayer(CP);
      end
    end
     end;

    end;

  end;

// The idea here is to go through the song and see whether based on the current player and time,
// a beat not is currently open (also, not yet hit)
// The algorithm is as follows: First the beat notes are analyzed. If, within the
// present level of tolerance (set by the players difficulty level), a beat note is hit
// it is considered the note to be analyzed, regardless of potential overlap with a break. If
// several beat notes overlap with their tolerance bands, then, we choose the closest
// one to the actual timing.
// If none of the beat notes is presently playing (hit or not), the analysis proceeds with analyzing the breaks.
// Beat breaks are also expanded to the left (earlier times) such that no
// whole is created between a beat note and an adjacent break (in that order).

procedure TBeatNoteTimerState.analyzeBeatNoteTimingForPlayer(CP: integer);
var
  ActualBeat:      real;
  SentenceMin,SentenceMax,SentenceIndex, LineFragmentIndex: integer;
  Line: 	       PLine;
  tolerance:           real; // Time tolerance for beat detection
  toleranceBreak:      real; // This goes the opposite, the smaller the beats, the larger the tolerance on the breaks

  CurrentLineFragment: PLineFragment;
  noteFound:           Boolean;
  BestDeltaBeat:           real; // The best timing delta between the current time (Actual Beat) and theoretical beat time
                                 // This is to find out the best beat if several are overlapping with their tolerances
begin
   // Work with the current line and adjacent ones
  SentenceMin := Tracks[CP].CurrentLine-1;
  if (SentenceMin < 0) then
    SentenceMin := 0;
  SentenceMax := Tracks[CP].CurrentLine+1;
  if SentenceMax > High(Tracks[CP].Lines) then
     SentenceMax := High(Tracks[CP].Lines);

  // Directly look for the beat where we are
  ActualBeat:=getActualBeatUsingBeatDetectionDelay();

  tolerance:=BeatTolerance(CP);

  // First round: beat notes only
  noteFound:=false;
  BestDeltaBeat:=0;
  for SentenceIndex := SentenceMin to SentenceMax do
  begin  // The detection is done on the tracks (not the players detected notes)
    // since we want to know which note should be hit by a beat
    Line := @Tracks[CP].Lines[SentenceIndex];
    for LineFragmentIndex := 0 to Line.HighNote do
      begin
        CurrentLineFragment := @Line.Notes[LineFragmentIndex];
        if CurrentLineFragment.NoteType= ntBeat then
        begin
        if spansCurrentBeat(ActualBeat, CurrentLineFragment.StartBeat,
            CurrentLineFragment.NoteType, CurrentLineFragment.Duration, tolerance) then
            begin

               if not noteFound then // First hit, take into account anyways
                  begin
                    playerBeatNoteState[CP].LineFragment:=CurrentLineFragment;
                    playerBeatNoteState[CP].CurrentBeat:=Min(Max(Round(ActualBeat),CurrentLineFragment.StartBeat),
                         CurrentLineFragment.StartBeat+CurrentLineFragment.Duration-1); // Stay within the line fragment
                    BestDeltaBeat:=Abs(ActualBeat-CurrentLineFragment.StartBeat);
                    noteFound:=true;
                  end
               else
                  begin // We've already found beat notes that are technically hit, compare which is
                    // closer to the current timing
                    if Abs(ActualBeat-CurrentLineFragment.StartBeat)<BestDeltaBeat then
                    begin
                       playerBeatNoteState[CP].LineFragment:=CurrentLineFragment;
                       playerBeatNoteState[CP].CurrentBeat:=Min(Max(Round(ActualBeat),CurrentLineFragment.StartBeat),
                         CurrentLineFragment.StartBeat+CurrentLineFragment.Duration-1); // Stay within the line fragment
                       BestDeltaBeat:=Abs(ActualBeat-CurrentLineFragment.StartBeat);
                    end;
                  end;


            end; // End beat hit

        end; // End its a beat note

      end; // End for loop over the line fragments

  end; // End for loop for the sentences

  // Now we've gone through all the beat note, and found nothing, so let's look at the breaks
  if (not noteFound)  then
  begin
  for SentenceIndex := SentenceMin to SentenceMax do
  begin  // This detection is done on the tracks
    Line := @Tracks[CP].Lines[SentenceIndex];
    for LineFragmentIndex := 0 to Line.HighNote do
      begin
        CurrentLineFragment := @Line.Notes[LineFragmentIndex];
        if CurrentLineFragment.NoteType = ntBeatSilence then
        begin
        if spansCurrentBeat(ActualBeat, CurrentLineFragment.StartBeat,
            CurrentLineFragment.NoteType, CurrentLineFragment.Duration, tolerance) then
               begin
                 if(playerBeatNoteState[CP].LineFragment = nil) then // no fragment detected yet
                 begin
                    playerBeatNoteState[CP].NoteHit:= false;
                 end else
                 begin
                   if not (CurrentLineFragment = playerBeatNoteState[CP].LineFragment) then // otherwise, detected a new fragment
                     begin
                       playerBeatNoteState[CP].NoteHit:= false;
                     end
                    else
                    begin
                      if not (Min(Max(Round(ActualBeat),CurrentLineFragment.StartBeat),
                         CurrentLineFragment.StartBeat+CurrentLineFragment.Duration-1)=playerBeatNoteState[CP].CurrentBeat) then
                         begin
                           playerBeatNoteState[CP].NoteHit:= false;
                         end;
                    end;
                 end;
                 playerBeatNoteState[CP].LineFragment:=CurrentLineFragment;
                 playerBeatNoteState[CP].CurrentBeat:=Min(Max(Round(ActualBeat),CurrentLineFragment.StartBeat),
                         CurrentLineFragment.StartBeat+CurrentLineFragment.Duration-1); // Stay within the line fragment
                noteFound:=true;
                Break;

               end;

        end;

      end; // End going through the fragments of the current line
      if noteFound then Break;
  end; // for sentence


  end;
  // We looped through all the sentences but could not found any presently matching note
  if not noteFound then begin
     playerBeatNoteState[CP].LineFragment:=nil;
     playerBeatNoteState[CP].CurrentBeat:=Round(ActualBeat);
  end;







end;



procedure TBeatNoteTimerState.analyzeBeatNoteTimingForPlayerOld(CP: integer);
var
  ActualBeat:      real;
  SentenceMin,SentenceMax,SentenceIndex, LineFragmentIndex: integer;
  Line: 	       PLine;
  tolerance:           real; // Time tolerance for beat detection
  toleranceBreak:      real; // This goes the opposite, the smaller the beats, the larger the tolerance on the breaks

  CurrentLineFragment: PLineFragment;

  noteFound:           Boolean;

begin






  // Directly look for the beat where we are
  ActualBeat:=getActualBeatUsingBeatDetectionDelay();

  tolerance:=BeatTolerance(CP);
  toleranceBreak:=1.0-tolerance;

  noteFound:=false;

  for SentenceIndex := SentenceMin to SentenceMax do
  begin  // This detection is done on the tracks
    Line := @Tracks[CP].Lines[SentenceIndex];
    for LineFragmentIndex := 0 to Line.HighNote do
      begin
        CurrentLineFragment := @Line.Notes[LineFragmentIndex];
        if spansCurrentBeat(ActualBeat, CurrentLineFragment.StartBeat,
            CurrentLineFragment.NoteType, CurrentLineFragment.Duration, tolerance) then
               begin
                 if(playerBeatNoteState[CP].LineFragment = nil) then // no fragment detected yet
                 begin
                    playerBeatNoteState[CP].NoteHit:= false;
                 end else
                 begin
                   if not (CurrentLineFragment = playerBeatNoteState[CP].LineFragment) then // otherwise, detected a new fragment
                     begin
                       playerBeatNoteState[CP].NoteHit:= false;
                     end
                    else
                    begin
                      if not (Min(Max(Round(ActualBeat),CurrentLineFragment.StartBeat),
                         CurrentLineFragment.StartBeat+CurrentLineFragment.Duration-1)=playerBeatNoteState[CP].CurrentBeat) then
                         begin
                           playerBeatNoteState[CP].NoteHit:= false;
                         end;
                    end;
                 end;
                 playerBeatNoteState[CP].LineFragment:=CurrentLineFragment;
                 playerBeatNoteState[CP].CurrentBeat:=Min(Max(Round(ActualBeat),CurrentLineFragment.StartBeat),
                         CurrentLineFragment.StartBeat+CurrentLineFragment.Duration-1); // Stay within the line fragment
                noteFound:=true;
                Break;

               end;

      end; // End going through the fragments of the current line
      if noteFound then Break;
  end; // for sentence

  // We looped through all the sentences but could not found any presently matching note
  if not noteFound then begin
     playerBeatNoteState[CP].LineFragment:=nil;
     playerBeatNoteState[CP].CurrentBeat:=Round(ActualBeat);
  end;







end;

// Analysis is needed only if a beat/beat silence not is playing and not yet hit
function TBeatNoteTimerState.doBeatNoteDetection(CP: integer; ActualBeat: real):Boolean;
begin
  Result := false;
  if(Abs(ActualBeat - playerBeatNoteState[CP].NoteHitExactBeatTime)>2) then
      playerBeatNoteState[CP].NoteHitExactBeatTime:=ActualBeat-100; // Safety, if it is too far away, don't consider blockage
  // Acoustic or keyboard note detection is necessary when a note is actually playing and not yet hit, plus we respect a blocakge
  // after a note is hit to avoid double detection (typicall on a beat and immediately behind)
  if not (playerBeatNoteState[CP].LineFragment = nil) and not (playerBeatNoteState[CP].NoteHit) and
  (ActualBeat > playerBeatNoteState[CP].NoteHitExactBeatTime+0.75) then
    Result := true;
end;

// update notes hit during analysis
procedure TBeatNoteTimerState.notifyNoteHit(CP: integer; ActualBeat: real);
begin
   playerBeatNoteState[CP].NoteHit:=true;
   playerBeatNoteState[CP].NoteHitExactBeatTime:=ActualBeat;
end;

end.

