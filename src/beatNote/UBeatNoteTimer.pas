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
    NoteHit: boolean; // Records whether at all the present note has been hit
    NoteHitExactBeatTime: real; // For drawing, exact timing of last hit
    NoteLastHitOnBeat: integer; // Records the last hit of this note (there can be as many hits as there are beats in the note)
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

    function doBeatNoteDetection(CP: integer; ActualBeat: real):Boolean; // Function that indicates whether for a given player, beat note/break detection should be done
     // at present. This is the case if the current beat note/break has not yet been hit on the current beat (breaks can have a longer duration)

    function BeatNoteHitAtPresent(CP: integer; ActualBeat: real):Boolean; // Indicates whether the currently playing beat note (if any) is hit on its
    // first beat. This function needs to invoked after doBeatNoteDetection, in the same cycle (i.e. the ActualBeat timing value being the same)

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
   setLength(playerBeatNoteState, 12); // Maximum 12 players in any configuration
   for count:= 0 to 11 do
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
  previousPlayerBeatNoteState:  TPlayerBeatNoteState; // To see whether things have changed from last time

  CurrentLineFragment: PLineFragment;
  noteFound:           Boolean;
  BestDeltaBeat:           real; // The best timing delta between the current time (Actual Beat) and theoretical beat time
                                 // This is to find out the best beat if several are overlapping with their tolerances
begin
  noteFound:=false;
  if CurrentSong.RapBeat then // This is only meant to work in the beat mode
  begin


  // Search through the song (for now)
  SentenceMin := 0;
  SentenceMax := High(Tracks[CP].Lines);

  // Directly look for the beat where we are
  ActualBeat:=getActualBeatUsingBeatDetectionDelay();

  tolerance:=BeatTolerance(CP);

  // Save the current state for comparison
  previousPlayerBeatNoteState.CurrentBeat := playerBeatNoteState[CP].CurrentBeat;
  previousPlayerBeatNoteState.LineFragment := playerBeatNoteState[CP].LineFragment;
  previousPlayerBeatNoteState.NoteHit := playerBeatNoteState[CP].NoteHit;
  previousPlayerBeatNoteState.NoteHitExactBeatTime := playerBeatNoteState[CP].NoteHitExactBeatTime;
  previousPlayerBeatNoteState.NoteLastHitOnBeat := playerBeatNoteState[CP].NoteLastHitOnBeat;



  BestDeltaBeat:=0;
  for SentenceIndex := SentenceMin to SentenceMax do
  begin  // The detection is done on the tracks (not the players detected notes)
    // since we want to know which note should be hit by a beat
    Line := @Tracks[CP].Lines[SentenceIndex];
    for LineFragmentIndex := 0 to Line.HighNote do
      begin
        CurrentLineFragment := @Line.Notes[LineFragmentIndex];
        if (CurrentLineFragment.NoteType= ntRap) then
        begin
        if spansCurrentBeat(ActualBeat, CurrentLineFragment.StartBeat,
            CurrentLineFragment.NoteType, CurrentLineFragment.Duration, tolerance) then
            begin

               if not noteFound then // First hit, take into account anyways
                  begin
                    noteFound:=true;
                    playerBeatNoteState[CP].LineFragment:=CurrentLineFragment;
                    playerBeatNoteState[CP].CurrentBeat:=beatInNote(ActualBeat, CurrentLineFragment.StartBeat, CurrentLineFragment.Duration); // Stay within the line fragment
                    BestDeltaBeat:=beatDistanceToNote(ActualBeat, CurrentLineFragment.StartBeat,CurrentLineFragment.Duration);

                    if hitsCurrentBeat(ActualBeat, CurrentLineFragment.StartBeat,
            CurrentLineFragment.NoteType, CurrentLineFragment.Duration, tolerance) then // Full-on hit, we fix this as the current note
                    begin
                       playerBeatNoteState[CP].CurrentBeat:=CurrentLineFragment.StartBeat; // Again, this is a full-on hit
                       break; // With a full hit, we are done, no more searching even if there would be overlaps
                    end;
                    // Note spanned, but not hit with the desired tolerance around the relevant (first) beat of the note


                  end
               else
                  begin // We've already found beat notes that are technically hit, compare which is
                    // closer to the current timing
                    if beatDistanceToNote(ActualBeat, CurrentLineFragment.StartBeat,CurrentLineFragment.Duration)<BestDeltaBeat then
                    begin
                       playerBeatNoteState[CP].LineFragment:=CurrentLineFragment;
                       // Stay within the line fragment
                       playerBeatNoteState[CP].CurrentBeat:=beatInNote(ActualBeat, CurrentLineFragment.StartBeat, CurrentLineFragment.Duration);

                       BestDeltaBeat:=beatDistanceToNote(ActualBeat, CurrentLineFragment.StartBeat,CurrentLineFragment.Duration);
                    end;
                  end;


            end; // End beat hit

        end; // End its a beat note

      end; // End for loop over the line fragments

  end; // End for loop for the sentences
   end; // End Beat rap mode

  // We looped through all the sentences but could not found any presently matching note
  if not noteFound then begin
     playerBeatNoteState[CP].LineFragment:=nil;
     playerBeatNoteState[CP].CurrentBeat:=Round(ActualBeat);
  end;

  if noteFound then
  begin

    if not (previousPlayerBeatNoteState.LineFragment = playerBeatNoteState[CP].LineFragment) then
    begin  // We have a change of fragment compared to the last time
       playerBeatNoteState[CP].NoteHit:=false;
       playerBeatNoteState[CP].NoteHitExactBeatTime:=0;
       playerBeatNoteState[CP].NoteLastHitOnBeat:=0;



    end
    else
    begin // It's the same fragment, we need to see whether it has already been hit on the same beat
       if playerBeatNoteState[CP].NoteHit then
       begin
          if not (playerBeatNoteState[CP].CurrentBeat = playerBeatNoteState[CP].NoteLastHitOnBeat) then
          begin // Same fragment, but we've advanced a beat
             playerBeatNoteState[CP].NoteHit:=false;
             playerBeatNoteState[CP].NoteHitExactBeatTime:=0;
             playerBeatNoteState[CP].NoteLastHitOnBeat:=0;
          end;

       end;
    end;
  end;








end;


function TBeatNoteTimerState.BeatNoteHitAtPresent(CP: integer; ActualBeat: real):Boolean;

begin
  Result := false;
  // An actual fragment needs to be playing
  if not (playerBeatNoteState[CP].LineFragment = nil) then
  begin

      Result := hitsCurrentBeat(ActualBeat, playerBeatNoteState[CP].LineFragment.StartBeat,
         playerBeatNoteState[CP].LineFragment.NoteType, playerBeatNoteState[CP].LineFragment.Duration,
         BeatTolerance(CP));
  end;
end;

// Analysis is needed only if a beat/beat silence not is playing and not yet hit
function TBeatNoteTimerState.doBeatNoteDetection(CP: integer; ActualBeat: real):Boolean;
begin
  Result := false;
  // BeatNote detection should be done if there is a note available, and if the present beat has not been hit
  Result := not (playerBeatNoteState[CP].LineFragment = nil) and not (playerBeatNoteState[CP].NoteHit);
end;

// update notes hit during analysis
procedure TBeatNoteTimerState.notifyNoteHit(CP: integer; ActualBeat: real);
begin
   playerBeatNoteState[CP].NoteHit:=true;
   playerBeatNoteState[CP].NoteHitExactBeatTime:=ActualBeat;
   playerBeatNoteState[CP].NoteLastHitOnBeat:=playerBeatNoteState[CP].CurrentBeat; // Record the currently assigned beat as the relavant hit beat
end;

end.

