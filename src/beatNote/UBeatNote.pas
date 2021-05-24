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

unit UBeatNote;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UBeatNoteTimer, // Provides the detection of beat notes along the time
  UKeyboardRecording,
  UCommon,
  UScreenSingController,
  UMusic,
  USong,
  SysUtils,
  UIni,
  URecord,
  UGraphicClasses; // Provides ScreenSong

type
TBeatDetectionParameters = record // single structure to hold the beat detection parameters
    Threshold:             integer;  // Threshold, in percent relative to full range (-low(smallint))
    RiseRate:              integer; // Rise rate at the beginning of the pulse, in percent full range per ms. Provide 0 to skip check
    MinPeakDuration:       integer; // Minimal peak duration before intensity falls definitely below Threshold. Provide 0 to skip check
    DropAfterPeak:         integer; // The relative drop that we expect after the initial peak
    TestTimeAfterPeak:     integer; // How many milliseconds after the peak we should check for the drop
  end;

procedure handleBeatNotes(Screen: TScreenSingController); // General handler called at every cycle. This is NewBeatDetect from
// UNote with adaptation

procedure checkBeatNote(CP: integer; Screen: TScreenSingController); // Specifically: detect current beat note (if any). This is
// This is NewNote from UNote with adaptation

function TimeTolerance(PlayerIndex: integer): real; // As a function of the difficulty, get the tolerance in time length

function BeatTolerance(PlayerIndex: integer): real; // As a function of the difficulty, get the tolerance in musc beat length

procedure SingDrawLineBeats(Left, Top, Right: real; Track, PlayerNumber: integer; LineSpacing: integer);

procedure SingDrawPlayerBGLineBeats(Left, Top, Right: real; Track, PlayerIndex: integer; LineSpacing: integer);

procedure SingDrawPlayerLineBeats(X, Y, W: real; Track, PlayerIndex: integer; LineSpacing: integer);

function spansCurrentBeat(ActualBeat: real; lineFragmentStartBeat: integer;
         lineFragmentNoteType:  TNoteType; lineFragmentDuration: integer;
         tolerance: real  ): boolean;

function BeatDetectionParametersFromIni (PlayerIndex: integer): TBeatDetectionParameters;
function BeatDetectionParametersForDeviceAndChannel(DeviceIndex: integer; ChannelIndex: integer): TBeatDetectionParameters;


function GetTimeFromBeatReal(Beat: real; SelfSong: TSong = nil): real; // Timing function for AudioPlayback, with non-integer beat values
// Basically a copy from UNote.pas (GetTimeFromBeat)

function getActualBeatUsingBeatDetectionDelay():real;    // For the beat detection,
// we make use of the "midBeatD" variable in LyricsState, as usually.
// The sampling is different though: while for sing notes, new notes are triggered
// when midBeatD changes integral (floor) value, see the definition of CurrentBeatD
// in TLyricsState.UpdateBeats in UBeatTimer.pas. Here, we still need this information,
// but whether a given time-points falls within the beat or beat silence note with the
// desired precision is determined separately, by the procedure spansCurrentBeat.
// Also, the sampling is different. For regular sing note sampling, generally 4096 samples
// are used, which at a typical 44.1kHz spans some 100ms. This is a bit long for precise beat detection, so
// we cut down the sample here to the part that actually falls in the correct interval. Together with
// roughly 25Hz screen renewal and thus audio sampling, that makes some 40ms delay - not 100+40ms as for
// typical microphone sampling. Hence, we use here the delay specifically set for beat detection, not the
// genreal microphone delay as elsewhere.




implementation

uses
  UNote,
  Math,
  UGraphic,
  dglOpenGL,
  UDraw,
  ULyrics; // Drawing routines;

function getActualBeatUsingBeatDetectionDelay():real;

begin
   // Directly look for the beat where we are, using the beat detection delay set by the user,
  // instead of the microphone delay set for singing.
     Result:=LyricsState.MidBeatD - GetMidBeat((Ini.KeyboardDelay-Ini.MicDelay)/1000.0);;



end;

procedure handleBeatNotes(Screen: TScreenSingController);
var

  SentenceMin:         integer;
  SentenceMax:         integer;
  MaxCP, PlayerIndex: integer;
  CP: integer;
  J: cardinal;
begin



  MaxCP := 0;
  if (CurrentSong.isDuet) and (PlayersPlay <> 1) then
    MaxCP := 1;

  if (assigned(Screen)) then
  begin
    BeatNoteTimerState.analyzeBeatNoteTiming(Screen);
    for J := 0 to MaxCP do
    begin
      CP := J;

  for PlayerIndex := 0 to PlayersPlay-1 do
    begin
      if (not CurrentSong.isDuet) or (PlayerIndex mod 2 = CP) then
      begin
         checkBeatNote(CP, Screen);
      end
    end
     end;

    end;

  end;

function TimeTolerance(PlayerIndex: integer): real;
var
  tolerance: real;
  beatTimeS: real;
begin
  tolerance := 0.1;

  if (ScreenSong.Mode = smNormal) then
  begin
    case Ini.PlayerLevel[PlayerIndex] of
      0: tolerance:=0.15;
      1: tolerance:=0.1;
      2: tolerance:=0.06;
      end;

    end
      else
      case Ini.Difficulty of
      0: tolerance:=0.15;
      1: tolerance:=0.1;
      2: tolerance:=0.06;
      end;



   Result:=tolerance;

end;

function BeatTolerance(PlayerIndex: integer): real;
begin
   Result:=TimeTolerance(PlayerIndex)/60.0*currentSong.BPM[0].BPM;
end;

function BeatDetectionParametersFromIni(PlayerIndex: integer): TBeatDetectionParameters;
var
  DeviceIndex: integer;
  DeviceIndexToUse: integer;
  Device:       TAudioInputDevice;
  DeviceCfg:    PInputDeviceConfig;
  ChannelIndex: integer;
  ChannelIndexToUse: integer;
  thePlayer: integer;
begin
  DeviceIndexToUse:=-1;
  // Look for the device used by the player
  for DeviceIndex := 0 to High(AudioInputProcessor.DeviceList) do
  begin
    Device := AudioInputProcessor.DeviceList[DeviceIndex];
    if not assigned(Device) then
      continue;
    DeviceCfg := @Ini.InputDeviceConfig[Device.CfgIndex];

    // check if device is used
    for ChannelIndex := 0 to High(DeviceCfg.ChannelToPlayerMap) do
    begin
      thePlayer := DeviceCfg.ChannelToPlayerMap[ChannelIndex] - 1;
      if thePlayer = PlayerIndex then
      begin
        DeviceIndexToUse:=DeviceIndex;
        ChannelIndexToUse:=ChannelIndex;
        break;
      end;

    end;

  end;



  Result:=BeatDetectionParametersForDeviceAndChannel(DeviceIndexToUse,ChannelIndexToUse);


end;


function BeatDetectionParametersForDeviceAndChannel(DeviceIndex: integer; ChannelIndex: integer): TBeatDetectionParameters;
var
  returnVal: TBeatDetectionParameters;
  getIndex: integer;
begin
  returnVal.Threshold:=50;
  returnVal.RiseRate:=20;
  returnVal.MinPeakDuration:=2;
  returnVal.DropAfterPeak:=40;
  returnVal.TestTimeAfterPeak:=20;
  if DeviceIndex < 0 then  // No device found, return default values
  begin
         // nothing to do specifically
  end
  else
  begin // This is a bit complicated since the ini settings contain the index to the correct value
        // rather than the values themselves. So we need to get them from indexing of the IBeatDetect... arrays.

     if DeviceIndex > High(Ini.InputDeviceBeatDetectionConfig) then
        DeviceIndex := High(Ini.InputDeviceBeatDetectionConfig);

    // No channels configured yet in the ini file, use default values
    if Length(Ini.InputDeviceBeatDetectionConfig[DeviceIndex].
          ChannelBeatDectectionSettings)= 0 then begin

           // nothing to specifically

          end
    else
    begin

    if ChannelIndex<0 then
      ChannelIndex := 0;
    if ChannelIndex > High(Ini.InputDeviceBeatDetectionConfig[DeviceIndex].
          ChannelBeatDectectionSettings) then
          ChannelIndex:=High(Ini.InputDeviceBeatDetectionConfig[DeviceIndex].
          ChannelBeatDectectionSettings);


    getIndex:=Ini.InputDeviceBeatDetectionConfig[DeviceIndex].
          ChannelBeatDectectionSettings[ChannelIndex].IntensityThreshold;
    if getIndex <0 then getIndex:=0;
    if getIndex >High(IBeatDetectIntensityThresholdValues) then getIndex:= High(IBeatDetectIntensityThresholdValues);

    returnVal.Threshold:=IBeatDetectIntensityThresholdValues[getIndex];

    // next, the riserate factor
    getIndex:=Ini.InputDeviceBeatDetectionConfig[DeviceIndex].
          ChannelBeatDectectionSettings[ChannelIndex].RiseRateFactor;
    if getIndex <0 then getIndex:=0;
    if getIndex >High(IBeatDetectRiseRateFactorValues) then getIndex:= High(IBeatDetectRiseRateFactorValues);

    returnVal.RiseRate:=Round(returnVal.Threshold/IBeatDetectRiseRateFactorValues[getIndex]);
     // This is done relatively to threshold to allow for main configuration through the threshold value

    // The other values are integers read directly from the ini system (for now without GUI access)
    returnVal.MinPeakDuration:=Ini.InputDeviceBeatDetectionConfig[DeviceIndex].
          ChannelBeatDectectionSettings[ChannelIndex].MinPeakMillisecond;
    returnVal.DropAfterPeak:=Ini.InputDeviceBeatDetectionConfig[DeviceIndex].
          ChannelBeatDectectionSettings[ChannelIndex].DropAfterPeakPercent;
    returnVal.TestTimeAfterPeak:=Ini.InputDeviceBeatDetectionConfig[DeviceIndex].
          ChannelBeatDectectionSettings[ChannelIndex].TestTimeAfterPeak;






  end;

  end;

  Result := returnVal;

end;




// The idea here is to have adjacent beat/break junctions tight, without a hole
// yet allow for A) usual notation in the lyrics file in the sence that
// a continuous stream of notes is associated with a complete coverage of the real
// time axis, e.g. start_note_2 = start_note_1 + duration_1
// The strategy is to allow arbitrary tolerance (as set by the players difficulty level)
// for the beats, but extend, if necessary, adjacent breaks such to earlier time such
// as not to leave a hole.
function spansCurrentBeat(ActualBeat: real; lineFragmentStartBeat: integer;
         lineFragmentNoteType:  TNoteType; lineFragmentDuration: integer;
         tolerance: real  ): boolean;
var toleranceBreak:real;
begin
if (lineFragmentNoteType = ntBeat) then
Result:= ((ActualBeat >= lineFragmentStartBeat-tolerance/2.0) and
              (ActualBeat < lineFragmentStartBeat + lineFragmentDuration-1 + tolerance/2.0)) and
              (lineFragmentDuration > 0)
else
if (lineFragmentNoteType = ntBeatSilence) then
  begin
    toleranceBreak:=1.0; // In principle, beat breaks extend up to 1 beat before,
                          // but they are second priority behind beats
    if toleranceBreak < 0 then
        toleranceBreak:=0;
    Result:=  ((ActualBeat >= lineFragmentStartBeat-toleranceBreak) and
              (ActualBeat < lineFragmentStartBeat + lineFragmentDuration)) and
              (lineFragmentDuration > 0)

  end
else
  Result := false;

end;

procedure checkBeatNote(CP: integer; Screen: TScreenSingController); // Specifically: detect current beat note (if any). This is
// inspired by NewNote from UNote with adaptation to the beat detection system (i.e. UBeateNoteTimer and specifically BeatNoteTimerState)
// The idea is that BeatNoteTimerState provides the information on whether there is a beat note (NoteType ntBeat) or beat silence note
// NoteType ntBeatSilence playing at present. If so, sound analysis is carried out (via the specific procedure AnalyzeBufferBeatOnly
// implemented in URecord. If a clap (loud sound compared to background and at absolute level) is detected by AnalyzeBufferBeatOnly
// in the expected time-frame (usually shorter than the 4096 available samples=ca. 0.1s in the sound buffer), then the beat or beat silence
// note is considere hit for the current rythmic beat. The player history is adaptated correspondingly, along with scoring, and BeatNoteTimerState
// is notified of the hit even (this avoids double hitting)
var
  CurrentLineFragment: PLineFragment;
  CurrentSound:        TCaptureBuffer;
  CurrentPlayer:       PPlayer;
  ActualBeat:          real;
  validNoteFound:      Boolean;
  MaxSongPoints:       integer;
  CurNotePoints:       real;
  TimeElapsed:         real; // This is the time the present note has already been running
  BeatDetectionParams: TBeatDetectionParameters;


begin
   ActualBeat := getActualBeatUsingBeatDetectionDelay();
   if BeatNoteTimerState.doBeatNoteDetection(CP,ActualBeat) then
   begin

     // Get the actual beat, with keyboard delay if necessary


     CurrentLineFragment := BeatNoteTimerState.playerBeatNoteState[CP].LineFragment;
     CurrentPlayer := @Player[CP];
     CurrentSound := AudioInputProcessor.Sound[CP];


     validNoteFound:=false;

     TimeElapsed :=  (ActualBeat-BeatNoteTimerState.playerBeatNoteState[CP].CurrentBeat)/currentSong.BPM[0].BPM*60.0;

     if Ini.KeyPlayOn=0 then
     begin
       // The beat detection settings are dependent on the input source
       // This in turn can be evaluated from the current player
       // The beat detection settings are dependent on the input source
       // This in turn can be evaluated from the current player

       BeatDetectionParams:=BeatDetectionParametersFromIni(CP);
       // Add additional time for gathering the necessary samples
       TimeElapsed := TimeElapsed + BeatDetectionParams.MinPeakDuration/1000.0+0.005; // Time for baseline before
       if BeatDetectionParams.DropAfterPeak > 0 then
       begin
         TimeElapsed := TimeElapsed + BeatDetectionParams.TestTimeAfterPeak/1000.0;
       end;
     end;


     if BeatNoteTimerState.playerBeatNoteState[CP].CurrentBeat = CurrentLineFragment.StartBeat then
     begin
        if CurrentLineFragment.notetype = ntBeat then
        begin
           TimeElapsed := TimeElapsed+TimeTolerance(CP)/2.0;

        end;
        if CurrentLineFragment.notetype = ntBeatSilence then
        begin

           TimeElapsed := TimeElapsed+60.0/currentSong.BPM[0].BPM; // Beat notes in principle extend to -1 beat

        end;
     end
     else
     begin
       TimeElapsed := TimeElapsed+0.5*60.0/currentSong.BPM[0].BPM;
     end;





     // Do sound or keyboard analysis
     if Ini.KeyPlayOn=0 then
     begin


       CurrentSound.AnalyzeBufferBeatOnly(TimeElapsed,
                                             BeatDetectionParams.Threshold,
                                             BeatDetectionParams.RiseRate,
                                             BeatDetectionParams.MinPeakDuration,
                                             BeatDetectionParams.DropAfterPeak,
                                             BeatDetectionParams.TestTimeAfterPeak);

       validNoteFound:=CurrentSound.ToneValid;
     end
     else
     begin
       validNoteFound:=KeyBoardRecorder.keyboardPressedForPlayer(CP,true);
               // Cleanup to avoid carry over to the next beat
     end;


     if validNoteFound then  // In the beat analysis, we only take into account new notes, multiple detection of the same note is irrelevant
                begin
                    Inc(CurrentPlayer.LengthNote);
                    Inc(CurrentPlayer.HighNote);
                    SetLength(CurrentPlayer.Note, CurrentPlayer.LengthNote);
                    CurrentPlayer.Note[CurrentPlayer.HighNote].Start:=floor(ActualBeat);
                    CurrentPlayer.Note[CurrentPlayer.HighNote].Duration:=1;
                    CurrentPlayer.Note[CurrentPlayer.HighNote].Detect:=ActualBeat;
                    CurrentPlayer.Note[CurrentPlayer.HighNote].Tone := CurrentLineFragment.Tone;
                    CurrentPlayer.Note[CurrentPlayer.HighNote].Hit := true;
                    CurrentPlayer.Note[CurrentPlayer.HighNote].NoteType := CurrentLineFragment.NoteType;
                    if(CurrentPlayer.Note[CurrentPlayer.HighNote].NoteType=ntBeat) then
                      CurrentPlayer.Note[CurrentPlayer.HighNote].Perfect:=true;

                    if (Ini.LineBonus > 0) then
                       MaxSongPoints := MAX_SONG_SCORE - MAX_SONG_LINE_BONUS
                    else
                        MaxSongPoints := MAX_SONG_SCORE;

                        // Note: ScoreValue is the sum of all note values of the song
                        // (MaxSongPoints / ScoreValue) is the points that a player
                        // gets for a hit of one beat of a normal note
                        // CurNotePoints is the amount of points that is meassured
                        // for a hit of the note per full beat
                    CurNotePoints := (MaxSongPoints / Tracks[CP].ScoreValue) * ScoreFactor[CurrentLineFragment.NoteType];
                    if (CurrentLineFragment.NoteType = ntBeatSilence) then
                       CurNotePoints := (MaxSongPoints / Tracks[CP].ScoreValue);

                    // We subtract here points if a clap is heard
                    case CurrentLineFragment.NoteType of
                         ntBeat:      CurrentPlayer.Score       := CurrentPlayer.Score       + CurNotePoints;
                         ntBeatSilence: CurrentPlayer.Score     := CurrentPlayer.Score       - CurNotePoints/5;
                    end;

                    // To indicate better smileys along
                    if CurrentLineFragment.NoteType =ntBeat then
                       GoldenRec.IncrementBeatLevel();


                    if CurrentLineFragment.NoteType = ntBeatSilence then
                       GoldenRec.ResetBeatLevel();

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


                SingDraw; // Update the screen for the new note
                BeatNoteTimerState.notifyNoteHit(CP,ActualBeat);

                end; // We have to add a new note



   end; // Beat note detection necessary


end;







// Basically SingDrawLine from UDraw, but specifically for the beat notes
procedure SingDrawLineBeats(Left, Top, Right: real; Track, PlayerNumber: integer; LineSpacing: integer);
var
  Rec:   TRecR;
  Count: integer;
  TempR: real;
  W, H:  real;
  GoldenStarPos: real;
begin
// We actually don't have a playernumber in this procedure, it should reside in Track - but it is always set to zero
// So we exploit this behavior a bit - we give Track the playernumber, keep it in playernumber - and then we set Track to zero
// This could also come quite in handy when we do the duet mode, cause just the notes for the player that has to sing should be drawn then
// BUT this is not implemented yet, all notes are drawn! :D
  if (ScreenSing.settings.NotesVisible and (1 shl Track) <> 0) then
  begin
    //PlayerNumber := Track + 1; // Player 1 is 0

    // exploit done




    glColor3f(1, 1, 1);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    if not Tracks[Track].Lines[Tracks[Track].CurrentLine].HasLength(TempR) then TempR := 0
    else TempR := (Right-Left) / TempR;



    with Tracks[Track].Lines[Tracks[Track].CurrentLine] do
    begin
      for Count := 0 to HighNote do
      begin
        with Notes[Count] do
        begin

           if NoteType = ntBeat then
           begin
                glColor4f(1, 1, 1, 1);

            W := NotesW[PlayerNumber - 1] * 2 + 1;
            H := NotesH[PlayerNumber - 1] * 1.5 + 3.5;



            // Technically, center on the beat (which is shifted by 0.5 compared to the standard notes
            Rec.Right := (StartBeat - Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + Left + 0.5 + 10*ScreenX + 2;
            Rec.Left  := Rec.Right - W;
            Rec.Top := Top - (Tone-BaseNote)*LineSpacing/2 - W/2.0;
            Rec.Bottom := Rec.Top + W;



            //ConsoleWriteLn(FloatToStr(Tex_Note_Beat[PlayerIndex+1].W));

            //Syntax for test Tex_BG_Left[PlayerIndex+1].TexNum
            glBindTexture(GL_TEXTURE_2D, Tex_Note_Beat[PlayerNumber].TexNum);

            glBegin(GL_QUADS);
              glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
              glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
              glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
              glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
            glEnd;

            // Optional clapping hands to indicate the nature of the beat notes
            if Ini.KeyPlayClapSignOn=1 then
            begin
               glBindTexture(GL_TEXTURE_2D, Tex_Note_Clap.TexNum);

              glBegin(GL_QUADS);
              glTexCoord2f(0, 0); glVertex2f((Rec.Left+Rec.Right)/2-8,  Rec.Top-25);
              glTexCoord2f(0, 1); glVertex2f((Rec.Left+Rec.Right)/2-8,  Rec.Top-8);
              glTexCoord2f(1, 1); glVertex2f((Rec.Left+Rec.Right)/2+11, Rec.Top-8);
              glTexCoord2f(1, 0); glVertex2f((Rec.Left+Rec.Right)/2+11, Rec.Top-25);
              glEnd;

            end;


           end; // Note type is a beat

        end; // with
      end; // for
    end; // with

    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);

end;

end;


//draw Note glow
procedure SingDrawPlayerBGLineBeats(Left, Top, Right: real; Track, PlayerIndex: integer; LineSpacing: integer);
var
  Rec:            TRecR;
  Count:          integer;
  TempR:          real;
  W, H:           real;
begin
  if (ScreenSing.settings.NotesVisible and (1 shl PlayerIndex) <> 0) then
  begin
    //glColor4f(1, 1, 1, sqrt((1+sin( AudioPlayback.Position * 3))/4)/ 2 + 0.5 );
    glColor4f(1, 1, 1, sqrt((1 + sin(AudioPlayback.Position * 3)))/2 + 0.05);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    if not Tracks[Track].Lines[Tracks[Track].CurrentLine].HasLength(TempR) then TempR := 0
    else TempR := (Right-Left) / TempR;

    with Tracks[Track].Lines[Tracks[Track].CurrentLine] do
    begin
      for Count := 0 to HighNote do
      begin
        with Notes[Count] do
        begin
          if NoteType = ntBeat then
          begin
            W := NotesW[PlayerIndex] * 3 + 1.5;
            H := NotesH[PlayerIndex] * 1.5 + 3.5;



            // The timing of the beats is differnt, centered on rhythmic beat, not mid interval
            Rec.Right := (StartBeat - Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + Left + 0.5 + 10*ScreenX + 4;
            Rec.Left  := Rec.Right - W;
            Rec.Top := Top - (Tone-BaseNote)*LineSpacing/2 - W/2.0;
            Rec.Bottom := Rec.Top + W;

            //ConsoleWriteLn(FloatToStr(Tex_Note_Beat[PlayerIndex+1].W));

            //Syntax for test Tex_BG_Left[PlayerIndex+1].TexNum
            glBindTexture(GL_TEXTURE_2D, Tex_Note_Beat_BG[PlayerIndex+1].TexNum);

            glBegin(GL_QUADS);
              glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
              glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
              glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
              glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
            glEnd;





          end

        end; // with
      end; // for
    end; // with

    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
  end;
end;

// draw sung notes
procedure SingDrawPlayerLineBeats(X, Y, W: real; Track, PlayerIndex: integer; LineSpacing: integer);
var
  TempR:      real;
  Rec:        TRecR;
  N: integer;
//  R, G, B, A: real;
  NotesH2:    real;
begin
  if (ScreenSing.Settings.InputVisible) then
  begin
    //Log.LogStatus('Player notes', 'SingDraw');

    glColor3f(1, 1, 1);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    //if Player[NrGracza].LengthNote > 0 then
    begin
      if not Tracks[Track].Lines[Tracks[Track].CurrentLine].HasLength(TempR) then TempR := 0
      else TempR := W / TempR;

      for N := 0 to Player[PlayerIndex].HighNote do
      begin
        with Player[PlayerIndex].Note[N] do
        begin
          if (NoteType = ntBeatSilence) or (NoteType = ntBeat) then
          begin


          // Left part of note

          Rec.Right := X+(Detect - Duration/4.0- Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR   + 10*ScreenX;
          Rec.Left  := Rec.Right - NotesW[PlayerIndex];

          //Rec.Left  := X + (Start - Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + 0.5 + 10*ScreenX;
          //Rec.Right := Rec.Left + NotesW[PlayerIndex];

          // Draw it in half size, if not hit
          if Hit then
          begin
            NotesH2 := NotesH[PlayerIndex]
          end
          else
          begin
            NotesH2 := int(NotesH[PlayerIndex] * 0.65);
          end;

          Rec.Top    := Y - NotesH2+ 3*LineSpacing;



          if(NoteType = ntBeat) then
          begin
             Rec.Top    := Y - (Tone-Tracks[Track].Lines[Tracks[Track].CurrentLine].BaseNote)*LineSpacing/2 - NotesH2;
          end;

          Rec.Bottom := Rec.Top + 2 * NotesH2;

          // draw the left part
          glColor3f(1, 1, 1);
          If (NoteType = ntRap) or (NoteType = ntRapGolden) then
          begin
            glBindTexture(GL_TEXTURE_2D, Tex_Left_Rap[PlayerIndex+1].TexNum);
          end
          else
          begin
            glBindTexture(GL_TEXTURE_2D, Tex_Left[PlayerIndex+1].TexNum);
          end;
          glBegin(GL_QUADS);
            glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
            glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
            glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
            glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
          glEnd;

          // Middle part of the note
          Rec.Left  := Rec.Right;
          Rec.Right := X + (Detect + Duration/4.0 - Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[0].StartBeat) *
                    TempR - NotesW[PlayerIndex]  + 10*ScreenX;

          // new
          //if (Start + Duration - 1 = LyricsState.CurrentBeatD) then
          //  Rec.Right := Rec.Right - (1-Frac(LyricsState.MidBeatD)) * TempR;

          // the left note is more right than the right note itself, sounds weird - so we fix that xD
          if Rec.Right <= Rec.Left then
            Rec.Right := Rec.Left;

          // draw the middle part

          glBindTexture(GL_TEXTURE_2D, Tex_Mid[PlayerIndex+1].TexNum);
          glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
          glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );
          glBegin(GL_QUADS);
            glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
            glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
            glTexCoord2f(round((Rec.Right-Rec.Left)/32), 1); glVertex2f(Rec.Right, Rec.Bottom);
            glTexCoord2f(round((Rec.Right-Rec.Left)/32), 0); glVertex2f(Rec.Right, Rec.Top);
          glEnd;
          glColor3f(1, 1, 1);

          // the right part of the note
          Rec.Left  := Rec.Right;
          Rec.Right := Rec.Right + NotesW[PlayerIndex];

          glBindTexture(GL_TEXTURE_2D, Tex_Right[PlayerIndex+1].TexNum);

          glBegin(GL_QUADS);
            glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
            glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
            glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
            glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
          glEnd;

          // Perfect note is stored
          if Perfect and (Ini.EffectSing=1) then
          begin
            //A := 1 - 2*(LyricsState.GetCurrentTime() - GetTimeFromBeat(Start + Duration));
            //if not (Start + Duration - 1 = LyricsState.CurrentBeatD) then
            begin
              //Star animation counter
              //inc(Starfr);
              //Starfr := Starfr mod 128;
             // if not(CurrentSong.isDuet) or (PlayerIndex mod 2 = Track) then
                if  (NoteType = ntBeat) then
                   GoldenRec.SavePerfectBeatPos((Rec.Left+Rec.Right)/2.0, (Rec.Top+Rec.Bottom)/2.0);
                if  (NoteType = ntBeatSilence) then
                   GoldenRec.SaveMissedBeatPos((Rec.Left+Rec.Right)/2.0, (Rec.Top+Rec.Bottom)/2.0);
            end;
          end;
          end; // it's a beat note
        end; // with
      end; // for


    end; // if
  end; // if
end;

function GetTimeFromBeatReal(Beat: real; SelfSong: TSong = nil): real;
var
  CurBPM: integer;
  Song: TSong;
begin

  if (SelfSong <> nil) then
    Song := SelfSong
  else
    Song := CurrentSong;

  Result := 0;

  // static BPM
  if Length(Song.BPM) = 1 then
  begin
    Result := Song.GAP / 1000 + Beat * 60 / Song.BPM[0].BPM;
  end
  // variable BPM
  else if Length(Song.BPM) > 1 then
  begin
    Result := Song.GAP / 1000;
    CurBPM := 0;
    while (CurBPM <= High(Song.BPM)) and
          (Beat > Song.BPM[CurBPM].StartBeat) do
    begin
      if (CurBPM < High(Song.BPM)) and
         (Beat >= Song.BPM[CurBPM+1].StartBeat) then
      begin
        // full range
        Result := Result + (60 / Song.BPM[CurBPM].BPM) *
                           (Song.BPM[CurBPM+1].StartBeat - Song.BPM[CurBPM].StartBeat);
      end;

      if (CurBPM = High(Song.BPM)) or
         (Beat < Song.BPM[CurBPM+1].StartBeat) then
      begin
        // in the middle
        Result := Result + (60 / Song.BPM[CurBPM].BPM) *
                           (Beat - Song.BPM[CurBPM].StartBeat);
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





end.

