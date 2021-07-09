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

unit UBeatNoteEdit;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMusic,
  UNote;


type arrayTLine = array of TLineFragment; // This is just formal because in Pascal, a function apparently cannot be written with an array return type


procedure copyLineFragmentValues(source: PLineFragment; target: PLineFragment); // Utility function to copy the values from one note to another

procedure ConvertTrackToBeats(theTrackLines: PLines); // In a full track (type TLines, here with pointer to change the variable value)
  // change normal notes to beat notes and add beat breaks for the length of the notes

procedure ConvertTrackToNormal(theTrackLines: PLines); // In a full track (type TLines, here with pointer to change the variable value)
  // change beat notes and immediately following beat break notes to reconstitute normal notes (opposite of ConvertTrackToBeats)

procedure ConvertLineToBeats(theLine: PLine); // For sentence (line of notes), change normal notes (ntNormal) to beat notes (ntBeat) and
  // place beat breaks to account for the length of the notes

procedure ConvertLineToNormal(theLine: PLine); // For sentence (line of notes), change beat notes (ntBeat) to normal notes (ntNormal) and
  // integrate beat breaks to recover the duration of the notes where possible

function getLineNotesWithBeats(theLine: TLine): arrayTLine;
  // For sentence (line of notes), change normal notes (ntNormal) to beat notes (ntBeat) and
  // place beat breaks to account for the length of the notes

function getLineNotesWithNormalNotes(theLine: TLine): arrayTLine;
  // For sentence (line of notes), change beats notes (ntBeat) to normal notes (ntNormal) and
  // remove beat breaks, integrating them into the duration of the notes

implementation

procedure copyLineFragmentValues(source: PLineFragment; target: PLineFragment);
begin
  target.Color := source.Color;
  target.StartBeat := source.StartBeat;
  target.Duration := source.Duration;
  target.Tone := source.Tone;
  target.Text := source.Text;
  target.NoteType := source.NoteType;
  target.IsMedley := source.IsMedley;
  target.IsStartPreview := source.IsStartPreview;
end;


function getLineNotesWithBeats(theLine: TLine): arrayTLine;
var
  NewNotes:          arrayTLine;  // Construct new notes here.
  theNotes:          arrayTLine;  // That's the old notes
  NewNotesLength:    integer;
  NewNotesIndex:     Integer;
  NoteIndex:         Integer;
begin

    NewNotesLength:=0;
    theNotes := theLine.Notes;

      for NoteIndex := 0 to High(theNotes) do
      begin

        // Normal note, so in principle we need to change it and add beat break
        if  theNotes[NoteIndex].NoteType = ntNormal then
        begin
          // But there are still different cases
          // First, if it is the last note, a beat break should only be inserted if the end of the
          // Lyrics line does not coincide with the beginning of the note itself:
          if NoteIndex = High(theNotes) then
          begin
            if theNotes[NoteIndex].StartBeat<theLine.EndBeat-1 then
            begin
              NewNotesLength:=NewNotesLength+1;
            end;
          end else // It's not the last note, so it depends on the distance to the next note. Do break only if the next note is also
                            // a normal note
          begin
            if (theNotes[NoteIndex+1].StartBeat-
               theNotes[NoteIndex].StartBeat > 1) and
               (theNotes[NoteIndex+1].NoteType = ntNormal) then
            begin
                NewNotesLength:=NewNotesLength+1;
            end;



          end;


          end;
          NewNotesLength:=NewNotesLength+1; // In any case, we need to copy over the notes that we already have

      end; // End going through notes of the line for a first analysis

      // Constitute a new note array with the correct length, and copy over values / insert breaks as we go
      NewNotesIndex:=0;
      setLength(NewNotes,NewNotesLength);

      for NoteIndex := 0 to High(theNotes) do
      begin
        copyLineFragmentValues(@theNotes[NoteIndex],@NewNotes[NewNotesIndex]);
        if theNotes[NoteIndex].NoteType = ntNormal then
        begin
          NewNotes[NewNotesIndex].NoteType:=ntBeat;
          NewNotes[NewNotesIndex].Duration:=1;
        end;
        NewNotesIndex:=NewNotesIndex+1;
        if  theNotes[NoteIndex].NoteType = ntNormal then
        begin // Same cases as above for the counting
          if NoteIndex = High(theNotes) then
          begin
            if theNotes[NoteIndex].StartBeat<theLine.EndBeat-1 then
            begin
              copyLineFragmentValues(@theNotes[NoteIndex],@NewNotes[NewNotesIndex]);
              NewNotes[NewNotesIndex].NoteType:=ntBeatSilence; // Convert this to the silence note
              NewNotes[NewNotesIndex].Duration:=theLine.EndBeat-
                theNotes[NoteIndex].StartBeat-1; // Duration: from beat+1 to end of sentence
              NewNotes[NewNotesIndex].StartBeat:=theNotes[NoteIndex].StartBeat+1;
              NewNotesIndex:=NewNotesIndex+1;

            end;
            end else // It's not the last note, so it depends on the distance to the next note. Do break only if the next note is also
                            // a normal note
              begin
                if (theNotes[NoteIndex+1].StartBeat-
                              theNotes[NoteIndex].StartBeat > 1) and
                               (theNotes[NoteIndex+1].NoteType = ntNormal) then
                begin
                   copyLineFragmentValues(@theNotes[NoteIndex],@NewNotes[NewNotesIndex]);
                   NewNotes[NewNotesIndex].NoteType:=ntBeatSilence; // Convert this to the silence note
                   NewNotes[NewNotesIndex].Duration:=theNotes[NoteIndex+1].StartBeat-
                   theNotes[NoteIndex].StartBeat-1; // Duration: from beat+1 to the beginning of the next note
                   NewNotes[NewNotesIndex].StartBeat:=theNotes[NoteIndex].StartBeat+1;
                   NewNotesIndex:=NewNotesIndex+1;
                end;



              end; // end not last note


            end; // end normal note


      end; // end going through the notes and making a copy / adding the breaks


      Result:=NewNotes;  // return the assembled array




end;


procedure ConvertLineToBeats(theLine: PLine);
var
  NoteIndex:         Integer;
  NewNotes:          arrayTLine; // Construct new notes here. This goes line per line
begin
      NewNotes := getLineNotesWithBeats(theLine^);

      setLength(theLine.Notes,Length(NewNotes));

      for NoteIndex := 0 to High(theLine.Notes) do
      begin
                        copyLineFragmentValues(@NewNotes[NoteIndex],@theLine.Notes[NoteIndex]);
      end;
      // The length of the current line is also stored separately
      theLine.HighNote:=High(theLine.Notes);


end;

procedure ConvertTrackToBeats(theTrackLines: PLines);
var
LineIndex:         Integer;
begin
             for LineIndex := 0 to High(theTrackLines.Lines) do
             begin

                 ConvertLineToBeats(@theTrackLines.Lines[LineIndex]);

             end; // end for over the lines



end;


procedure ConvertTrackToNormal(theTrackLines: PLines);
var
LineIndex:         Integer;
begin
             for LineIndex := 0 to High(theTrackLines.Lines) do
             begin

                 ConvertLineToNormal(@theTrackLines.Lines[LineIndex]);

             end; // end for over the lines



end;


procedure ConvertLineToNormal(theLine: PLine);
var
  NoteIndex:         Integer;
  NewNotes:          arrayTLine; // Construct new notes here. This goes line per line
begin
      NewNotes := getLineNotesWithNormalNotes(theLine^);

      setLength(theLine.Notes,Length(NewNotes));

      for NoteIndex := 0 to High(theLine.Notes) do
      begin
                        copyLineFragmentValues(@NewNotes[NoteIndex],@theLine.Notes[NoteIndex]);
      end;
      // The length of the current line is also stored separately
      theLine.HighNote:=High(theLine.Notes);


end;


function getLineNotesWithNormalNotes(theLine: TLine): arrayTLine;
var
  NewNotes:          arrayTLine;  // Construct new notes here.
  theNotes:          arrayTLine;  // That's the old notes
  NewNotesLength:    integer;
  NewNotesIndex:     Integer;
  NoteIndex:         Integer;
begin

    NewNotesLength:=0;
    theNotes := theLine.Notes;

      for NoteIndex := 0 to High(theNotes) do
      begin

        // Beat break, check whether it will be integrated into the preceding beat note
        if  theNotes[NoteIndex].NoteType <> ntBeatSilence then
        begin
          NewNotesLength:=NewNotesLength+1; // We will remove all ntBeatSilence notes
          end;
      end; // End going through notes of the line for a first analysis

      // Constitute a new note array with the correct length, and copy over values / insert breaks as we go
      NewNotesIndex:=0;
      setLength(NewNotes,NewNotesLength);

      for NoteIndex := 0 to High(theNotes) do
      begin
        // Beat break, check whether it will be integrated into the preceding beat note
        if  theNotes[NoteIndex].NoteType = ntBeatSilence then
        begin
          if NoteIndex > 0 then
          begin
             if theNotes[NoteIndex-1].NoteType=ntBeat then
             begin
                NewNotes[NewNotesIndex-1].Duration:=NewNotes[NewNotesIndex-1].Duration+theNotes[NoteIndex].Duration;
             end;
          end;


        end else begin
          copyLineFragmentValues(@theNotes[NoteIndex],@NewNotes[NewNotesIndex]);
          if theNotes[NoteIndex].NoteType = ntBeat then begin
             NewNotes[NewNotesIndex].NoteType:=ntNormal;
          end;
          NewNotesIndex:=NewNotesIndex+1;

        end;



      end; // end going through the notes and making a copy / adding the breaks


      Result:=NewNotes;  // return the assembled array




end;
end.

