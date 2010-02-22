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

unit UScreenEditConvert;

{*
 * See
 * MIDI Recommended Practice (RP-017): SMF Lyric Meta Event Definition
 *   http://www.midi.org/techspecs/rp17.php
 * MIDI Recommended Practice (RP-026): SMF Language and Display Extensions
 *   http://www.midi.org/techspecs/rp26.php
 * MIDI File Format
 *   http://www.sonicspot.com/guide/midifiles.html
 * KMIDI File Format
 *   http://gnese.free.fr/Projects/KaraokeTime/Fichiers/karfaq.html
 *   http://journals.rpungin.fotki.com/karaoke/category/midi
 *
 * There are two widely spread karaoke formats:
 * - KMIDI (.kar), an inofficial midi extension by Tune 1000
 * - Standard Midi files with lyric meta-tags (SMF with lyrics, .mid).
 *
 * KMIDI uses two tracks, the first just contains a header (mostly track 2) and
 * the second the lyrics (track 3). It uses text meta tags for the lyrics.
 * SMF uses just one track (normally track 1) and uses lyric meta tags for storage.
 *
 * Most files are in the KMIDI format. Some Midi files contain both lyric types.
 *}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  math,
  UMenu,
  SDL,
  {$IFDEF UseMIDIPort}
  MidiFile,
  MidiOut,
  {$ENDIF}
  ULog,
  USongs,
  USong,
  UMusic,
  UThemes,
  UPath;

type
  TMidiNote = record
    Event:     integer;
    EventType: integer;
    Channel:   integer;
    Start:     real;
    Len:       real;
    Data1:     integer;
    Data2:     integer;
    Str:       UTF8String; // normally ASCII
  end;

  TLyricType = (ltKMIDI, ltSMFLyric);

  TTrack = record
    Note:   array of TMidiNote;
    Name:   UTF8String; // normally ASCII
    Status: set of (tsNotes, tsLyrics); //< track contains notes, lyrics or both
    LyricType: set of TLyricType;
    NoteType:  (ntNone, ntAvail);
  end;

  TNote = record
    Start:    integer;
    Len:      integer;
    Tone:     integer;
    Lyric:    UTF8String;
    NewSentence:  boolean;
  end;

  TArrayTrack = array of TTrack;

  TScreenEditConvert = class(TMenu)
    private
      Tracks:    TArrayTrack; // current track
      ColR:      array[0..100] of real;
      ColG:      array[0..100] of real;
      ColB:      array[0..100] of real;
      Len:       real;
      SelTrack:  integer;     // index of selected track
      fFileName: IPath;

      {$IFDEF UseMIDIPort}
      MidiFile:  TMidiFile;
      MidiOut:   TMidiOutput;
      {$ENDIF}

      BPM:       real;
      Ticks:     real;
      Note:      array of TNote;

      procedure AddLyric(Start: integer; LyricType: TLyricType; Text: UTF8String);
      procedure Extract(out Song: TSong; out Lines: TLines);

      {$IFDEF UseMIDIPort}
      procedure MidiFile1MidiEvent(event: PMidiEvent);
      {$ENDIF}

      function CountSelectedTracks: integer;

    public
      constructor Create; override;
      procedure OnShow; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      function Draw: boolean; override;
      procedure OnHide; override;
  end;

implementation

uses
  SysUtils,
  TextGL,
  gl,
  UDrawTexture,
  UFiles,
  UGraphic,
  UIni,
  UMain,
  UPathUtils,
  USkins,
  ULanguage,
  UTextEncoding,
  UUnicodeUtils;

const
  // MIDI/KAR lyrics are specified to be ASCII only.
  // Assume backward compatible CP1252 encoding.
  DEFAULT_ENCODING = encCP1252;

const
  MIDI_EVENTTYPE_NOTEOFF    = $8;
  MIDI_EVENTTYPE_NOTEON     = $9;
  MIDI_EVENTTYPE_META_SYSEX = $F;

  MIDI_EVENT_META = $FF;
  MIDI_META_TEXT   = $1;
  MIDI_META_LYRICS = $5;

function TScreenEditConvert.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
{$IFDEF UseMIDIPort}
var
  SResult: TSaveSongResult;
  Playing: boolean;
  MidiTrack: TMidiTrack;
  Song:  TSong;
  Lines: TLines;
{$ENDIF}
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check normal keys
    case UCS4UpperCase(CharCode) of
      Ord('Q'):
        begin
          Result := false;
          Exit;
        end;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          {$IFDEF UseMIDIPort}
          if (MidiFile <> nil) then
            MidiFile.StopPlaying;
          {$ENDIF}
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenEdit);
        end;

      SDLK_RETURN:
        begin
          if Interaction = 0 then
          begin
            AudioPlayback.PlaySound(SoundLib.Start);
            ScreenOpen.Filename := GamePath.Append('file.mid');
            ScreenOpen.BackScreen := @ScreenEditConvert;
            FadeTo(@ScreenOpen);
          end
          else if Interaction = 1 then
          begin
            {$IFDEF UseMIDIPort}
            if (MidiFile <> nil) then
            begin
              MidiFile.OnMidiEvent := MidiFile1MidiEvent;
              //MidiFile.GoToTime(MidiFile.GetTrackLength div 2);
              MidiFile.StartPlaying;
            end;
            {$ENDIF}
          end
          else if Interaction = 2 then
          begin
            {$IFDEF UseMIDIPort}
            if (MidiFile <> nil) then
            begin
              MidiFile.OnMidiEvent := nil;
              MidiFile.StartPlaying;
            end;
            {$ENDIF}
          end
          else if Interaction = 3 then
          begin
            {$IFDEF UseMIDIPort}
            if CountSelectedTracks > 0 then
            begin
              Extract(Song, Lines);
              SResult := SaveSong(Song, Lines, fFileName.SetExtension('.txt'),
                       false);
              FreeAndNil(Song);
              if (SResult = ssrOK) then
                ScreenPopupInfo.ShowPopup(Language.Translate('INFO_FILE_SAVED'))
              else
                ScreenPopupError.ShowPopup(Language.Translate('ERROR_SAVE_FILE_FAILED'));
            end
            else
            begin
              ScreenPopupError.ShowPopup(Language.Translate('EDITOR_ERROR_NO_TRACK_SELECTED'));
            end;
            {$ENDIF}
          end;

        end;

      SDLK_SPACE:
        begin
          {$IFDEF UseMIDIPort}
          if (MidiFile <> nil) then
          begin
            if (Tracks[SelTrack].NoteType = ntAvail) and
               (Tracks[SelTrack].LyricType <> []) then
            begin
              if (Tracks[SelTrack].Status = []) then
                Tracks[SelTrack].Status := [tsNotes]
              else if (Tracks[SelTrack].Status = [tsNotes]) then
                Tracks[SelTrack].Status := [tsLyrics]
              else if (Tracks[SelTrack].Status = [tsLyrics]) then
                Tracks[SelTrack].Status := [tsNotes, tsLyrics]
              else if (Tracks[SelTrack].Status = [tsNotes, tsLyrics]) then
                Tracks[SelTrack].Status := [];
            end
            else if (Tracks[SelTrack].NoteType = ntAvail) then
            begin
              if (Tracks[SelTrack].Status = []) then
                Tracks[SelTrack].Status := [tsNotes]
              else
                Tracks[SelTrack].Status := [];
            end
            else if (Tracks[SelTrack].LyricType <> []) then
            begin
              if (Tracks[SelTrack].Status = []) then
                Tracks[SelTrack].Status := [tsLyrics]
              else
                Tracks[SelTrack].Status := [];
            end;

            Playing := (MidiFile.GetCurrentTime > 0);
            MidiFile.StopPlaying();
            MidiTrack := MidiFile.GetTrack(SelTrack);
            if tsNotes in Tracks[SelTrack].Status then
              MidiTrack.OnMidiEvent := MidiFile1MidiEvent
            else
              MidiTrack.OnMidiEvent := nil;
            if (Playing) then
              MidiFile.ContinuePlaying();
          end;
          {$ENDIF}
        end;

      SDLK_RIGHT:
        begin
          InteractNext;
        end;

      SDLK_LEFT:
        begin
          InteractPrev;
        end;

      SDLK_DOWN:
        begin
          Inc(SelTrack);
          if SelTrack > High(Tracks) then
            SelTrack := 0;
        end;
      SDLK_UP:
        begin
          Dec(SelTrack);
          if SelTrack < 0 then
            SelTrack := High(Tracks);
        end;
    end;
  end;
end;

procedure TScreenEditConvert.AddLyric(Start: integer; LyricType: TLyricType; Text: UTF8String);
var
  N:    integer;
begin
  // find corresponding note
  N := 0;
  while (N <= High(Note)) do
  begin
    if Note[N].Start = Start then
      Break;
    Inc(N);
  end;

  // check if note was found
  if (N > High(Note)) then
    Exit;
  
  // set text
  if (LyricType = ltKMIDI) then
  begin
    // end of paragraph
    if Copy(Text, 1, 1) = '\' then
    begin
      Delete(Text, 1, 1);
    end
    // end of line
    else if Copy(Text, 1, 1) = '/' then
    begin
      Delete(Text, 1, 1);
      Note[N].NewSentence := true;
    end;
  end
  else // SMFLyric
  begin
    // Line Feed -> end of paragraph
    if Copy(Text, 1, 1) = #$0A then
    begin
      Delete(Text, 1, 1);
    end
    // Carriage Return -> end of line
    else if Copy(Text, 1, 1) = #$0D then
    begin
      Delete(Text, 1, 1);
      Note[N].NewSentence := true;
    end;
  end;

  // overwrite lyric or append
  if Note[N].Lyric = '-' then
    Note[N].Lyric := Text
  else
    Note[N].Lyric := Note[N].Lyric + Text;
end;

procedure TScreenEditConvert.Extract(out Song: TSong; out Lines: TLines);

var
  T:    integer;
  C:    integer;
  N:    integer;
  Nu:   integer;
  NoteTemp: TNote;
  Move: integer;
  Max, Min: integer;
  LyricType: TLyricType;
  Text: UTF8String;
begin
  // song info
  Song := TSong.Create();
  Song.Clear();
  Song.Resolution := 4;
  SetLength(Song.BPM, 1);
  Song.BPM[0].BPM := BPM*4;
  SetLength(Note, 0);

  // extract notes
  for T := 0 to High(Tracks) do
  begin
    if tsNotes in Tracks[T].Status then
    begin
      for N := 0 to High(Tracks[T].Note) do
      begin
        if (Tracks[T].Note[N].EventType = MIDI_EVENTTYPE_NOTEON) and
           (Tracks[T].Note[N].Data2 > 0) then
        begin
          Nu := Length(Note);
          SetLength(Note, Nu + 1);
          Note[Nu].Start := Round(Tracks[T].Note[N].Start / Ticks);
          Note[Nu].Len := Round(Tracks[T].Note[N].Len / Ticks);
          Note[Nu].Tone := Tracks[T].Note[N].Data1 - 12*5;
          Note[Nu].Lyric := '-';
        end;
      end;
    end;
  end;

  // extract lyrics (and artist + title info)
  for T := 0 to High(Tracks) do
  begin
    if not (tsLyrics in Tracks[T].Status) then
      Continue;

    for N := 0 to High(Tracks[T].Note) do
    begin
      if (Tracks[T].Note[N].Event = MIDI_EVENT_META) then
      begin
        // determine and validate lyric meta tag
        if (ltKMIDI in Tracks[T].LyricType) and
           (Tracks[T].Note[N].Data1 = MIDI_META_TEXT) then
        begin
          Text := Tracks[T].Note[N].Str;
          
          // check for meta info
          if (Length(Text) > 2) and (Text[1] = '@') then
          begin
            case Text[2] of
              'L': Song.Language := Copy(Text, 3, Length(Text)); // language
              'T': begin // title info
                if (Song.Artist = '') then
                  Song.Artist := Copy(Text, 3, Length(Text))
                else if (Song.Title = '') then
                  Song.Title := Copy(Text, 3, Length(Text));
              end;
            end;
            Continue;
          end;

          LyricType := ltKMIDI;
        end
        else if (ltSMFLyric in Tracks[T].LyricType) and
                (Tracks[T].Note[N].Data1 = MIDI_META_LYRICS) then
        begin
          LyricType := ltSMFLyric;
        end
        else
        begin
          // unknown meta event
          Continue;
        end;

        AddLyric(Round(Tracks[T].Note[N].Start / Ticks), LyricType, Tracks[T].Note[N].Str);
      end;
    end;
  end;

  // sort notes
  for N := 0 to High(Note) do
    for Nu := 0 to High(Note)-1 do
      if Note[Nu].Start > Note[Nu+1].Start then
      begin
        NoteTemp := Note[Nu];
        Note[Nu] := Note[Nu+1];
        Note[Nu+1] := NoteTemp;
      end;

  // move to 0 at beginning
  Move := Note[0].Start;
  for N := 0 to High(Note) do
    Note[N].Start := Note[N].Start - Move;

  // copy notes
  SetLength(Lines.Line, 1);
  Lines.Number     := 1;
  Lines.High       := 0;
  Lines.Current    := 0;
  Lines.Resolution := 0;
  Lines.NotesGAP   := 0;
  Lines.ScoreValue := 0;

  C := 0;
  N := 0;
  Lines.Line[C].HighNote := -1;

  for Nu := 0 to High(Note) do
  begin
    if Note[Nu].NewSentence then // new line
    begin
      SetLength(Lines.Line, Length(Lines.Line)+1);
      Lines.Number := Lines.Number + 1;
      Lines.High := Lines.High + 1;
      C := C + 1;
      N := 0;
      SetLength(Lines.Line[C].Note, 0);
      Lines.Line[C].HighNote := -1;

      //Calculate Start of the Last Sentence
      if (C > 0) and (Nu > 0) then
      begin
        Max := Note[Nu].Start;
        Min := Note[Nu-1].Start + Note[Nu-1].Len;
        
        case (Max - Min) of
          0:    Lines.Line[C].Start := Max;
          1:    Lines.Line[C].Start := Max;
          2:    Lines.Line[C].Start := Max - 1;
          3:    Lines.Line[C].Start := Max - 2;
          else
            if ((Max - Min) > 4) then
              Lines.Line[C].Start := Min + 2
            else
              Lines.Line[C].Start := Max;

        end; // case

      end;
    end;

    // create space for new note
    SetLength(Lines.Line[C].Note, Length(Lines.Line[C].Note)+1);
    Inc(Lines.Line[C].HighNote);

    // initialize note
    Lines.Line[C].Note[N].Start := Note[Nu].Start;
    Lines.Line[C].Note[N].Length := Note[Nu].Len;
    Lines.Line[C].Note[N].Tone := Note[Nu].Tone;
    Lines.Line[C].Note[N].Text := DecodeStringUTF8(Note[Nu].Lyric, DEFAULT_ENCODING);
    Lines.Line[C].Note[N].NoteType := ntNormal;
    Inc(N);
  end;
end;

function TScreenEditConvert.CountSelectedTracks: integer;
var
  T:    integer; // track
begin
  Result := 0;
  for T := 0 to High(Tracks) do
    if tsNotes in Tracks[T].Status then
      Inc(Result);
end;

{$IFDEF UseMIDIPort}
procedure TScreenEditConvert.MidiFile1MidiEvent(event: PMidiEvent);
begin
  //Log.LogStatus(IntToStr(event.event), 'MIDI');
  try
    MidiOut.PutShort(event.event, event.data1, event.data2);
  except
    MidiFile.StopPlaying();
  end;
end;
{$ENDIF}

constructor TScreenEditConvert.Create;
var
  P:  integer;
begin
  inherited Create;
  AddButton(40, 20, 100, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(15, 5, 0, 0, 0, 'Open');
  //Button[High(Button)].Text[0].Size := 11;

  AddButton(160, 20, 100, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(25, 5, 0, 0, 0, 'Play');

  AddButton(280, 20, 200, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(25, 5, 0, 0, 0, 'Play Selected');

  AddButton(500, 20, 100, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(20, 5, 0, 0, 0, 'Save');

  fFileName := PATH_NONE;

  for P := 0 to 100 do
  begin
    ColR[P] := Random(10)/10;
    ColG[P] := Random(10)/10;
    ColB[P] := Random(10)/10;
  end;

end;

procedure TScreenEditConvert.OnShow;
{$IFDEF UseMIDIPort}
var
  T:    integer; // track
  N:    integer; // note
  MidiTrack: TMidiTrack;
  MidiEvent: PMidiEvent;
  FileOpened: boolean;
  KMIDITrackIndex, SMFTrackIndex: integer;
{$ENDIF}
begin
  inherited;

  Interaction := 0;

{$IFDEF UseMIDIPort}
  MidiOut := TMidiOutput.Create(nil);
  Log.LogInfo(MidiOut.ProductName, 'MIDI');
  MidiOut.Open;
  MidiFile := nil;
  SetLength(Tracks, 0);

  // Filename is only <> PATH_NONE if we called the OpenScreen before
  fFilename := ScreenOpen.Filename;
  if (fFilename = PATH_NONE) then
    Exit;
  ScreenOpen.Filename := PATH_NONE;

  FileOpened := false;
  if fFileName.Exists then
  begin
    MidiFile := TMidiFile.Create(nil);
    MidiFile.Filename := fFileName;
    try
      MidiFile.ReadFile;
      FileOpened := true;
    except
      MidiFile.Free;
    end;
  end;

  if (not FileOpened) then
  begin
    ScreenPopupError.ShowPopup(Language.Translate('ERROR_FILE_NOT_FOUND'));
    Exit;
  end;

  Len := 0;
  SelTrack := 0;
  BPM := MidiFile.Bpm;
  Ticks := MidiFile.TicksPerQuarter / 4;

  KMIDITrackIndex := -1;
  SMFTrackIndex := -1;

  SetLength(Tracks, MidiFile.NumberOfTracks);
  for T := 0 to MidiFile.NumberOfTracks-1 do
    Tracks[T].LyricType := [];

  for T := 0 to MidiFile.NumberOfTracks-1 do
  begin
    MidiTrack := MidiFile.GetTrack(T);
    MidiTrack.OnMidiEvent := nil;
    Tracks[T].Name := DecodeStringUTF8(MidiTrack.getName, DEFAULT_ENCODING);
    Tracks[T].NoteType := ntNone;
    Tracks[T].Status := [];

    SetLength(Tracks[T].Note, MidiTrack.getEventCount());
    for N := 0 to MidiTrack.getEventCount-1 do
    begin
      MidiEvent := MidiTrack.GetEvent(N);

      Tracks[T].Note[N].Start     := MidiEvent.time;
      Tracks[T].Note[N].Len       := MidiEvent.len;
      Tracks[T].Note[N].Event     := MidiEvent.event;
      Tracks[T].Note[N].EventType := MidiEvent.event shr 4;
      Tracks[T].Note[N].Channel   := MidiEvent.event and $0F;
      Tracks[T].Note[N].Data1     := MidiEvent.data1;
      Tracks[T].Note[N].Data2     := MidiEvent.data2;
      Tracks[T].Note[N].Str       := DecodeStringUTF8(MidiEvent.str, DEFAULT_ENCODING);

      if (Tracks[T].Note[N].Event = MIDI_EVENT_META) then
      begin
        case (Tracks[T].Note[N].Data1) of
          MIDI_META_TEXT: begin
            // KMIDI lyrics (uses MIDI_META_TEXT events)
            if (StrLComp(PAnsiChar(Tracks[T].Note[N].Str), '@KMIDI KARAOKE FILE', 19) = 0) and
               (High(Tracks) >= T+1) then
            begin
              // The '@KMIDI ...' mark is in the first track (mostly named 'Soft Karaoke')
              // but the lyrics are in the second track (named 'Words')
              Tracks[T+1].LyricType := Tracks[T+1].LyricType + [ltKMIDI];
              KMIDITrackIndex := T+1;
            end;
          end;
          MIDI_META_LYRICS: begin
            // lyrics in Standard Midi File format found (uses MIDI_META_LYRICS events)
            Tracks[T].LyricType := Tracks[T].LyricType + [ltSMFLyric];
            SMFTrackIndex := T;
          end;
        end;
      end
      else if (Tracks[T].Note[N].EventType = MIDI_EVENTTYPE_NOTEON) then
      begin
        // notes available
        Tracks[T].NoteType := ntAvail;
      end;

      if Tracks[T].Note[N].Start + Tracks[T].Note[N].Len > Len then
        Len := Tracks[T].Note[N].Start + Tracks[T].Note[N].Len;
    end;
  end;

  // set default lyric track. Prefer KMIDI.
  if (KMIDITrackIndex > -1) then
    Tracks[KMIDITrackIndex].Status := Tracks[KMIDITrackIndex].Status + [tsLyrics]
  else if (SMFTrackIndex > -1) then
    Tracks[SMFTrackIndex].Status := Tracks[SMFTrackIndex].Status + [tsLyrics];
{$ENDIF}
end;

function TScreenEditConvert.Draw: boolean;
var
  Count:  integer;
  Count2: integer;
  Bottom: real;
  X:      real;
  Y:      real;
  Height: real;
  YSkip:  real;
begin
  // draw static menu
  inherited Draw;

  Y := 100;

  Height := min(480, 40 * Length(Tracks));
  Bottom := Y + Height;

  YSkip := Height / Length(Tracks);

  // highlight selected track
  DrawQuad(10, Y+SelTrack*YSkip, 780, YSkip, 0.8, 0.8, 0.8);

  // track-selection info
  for Count := 0 to High(Tracks) do
    if Tracks[Count].Status <> [] then
      DrawQuad(10, Y + Count*YSkip, 50, YSkip, 0.8, 0.3, 0.3);
  glColor3f(0, 0, 0);
  for Count := 0 to High(Tracks) do
  begin
    if Tracks[Count].NoteType = ntAvail then
    begin
      if tsNotes in Tracks[Count].Status then
        glColor3f(0, 0, 0)
      else
        glColor3f(0.7, 0.7, 0.7);
      SetFontPos(25, Y + Count*YSkip + 10);
      SetFontSize(15);
      glPrint('N');
    end;
    if Tracks[Count].LyricType <> [] then
    begin
      if tsLyrics in Tracks[Count].Status then
        glColor3f(0, 0, 0)
      else
        glColor3f(0.7, 0.7, 0.7);
      SetFontPos(40, Y + Count*YSkip + 10);
      SetFontSize(15);
      glPrint('L');
    end;
  end;

  DrawLine( 10, Y,  10, Bottom, 0, 0, 0);
  DrawLine( 60, Y,  60, Bottom, 0, 0, 0);
  DrawLine(790, Y, 790, Bottom, 0, 0, 0);

  for Count := 0 to Length(Tracks) do
    DrawLine(10, Y + Count*YSkip, 790, Y + Count*YSkip, 0, 0, 0);

  for Count := 0 to High(Tracks) do
  begin
    SetFontPos(65, Y + Count*YSkip);
    SetFontSize(15);
    glPrint(Tracks[Count].Name);
  end;

  for Count := 0 to High(Tracks) do
  begin
    for Count2 := 0 to High(Tracks[Count].Note) do
    begin
      if Tracks[Count].Note[Count2].EventType = MIDI_EVENTTYPE_NOTEON then
        DrawQuad(60 + Tracks[Count].Note[Count2].Start/Len * 725,
                 Y + (Count+1)*YSkip - Tracks[Count].Note[Count2].Data1*35/127,
                 3, 3,
                 ColR[Count], ColG[Count], ColB[Count]);
      if Tracks[Count].Note[Count2].EventType = 15 then
        DrawLine(60 + Tracks[Count].Note[Count2].Start/Len * 725, Y + 0.75 * YSkip + Count*YSkip,
                 60 + Tracks[Count].Note[Count2].Start/Len * 725, Y + YSkip + Count*YSkip,
                 ColR[Count], ColG[Count], ColB[Count]);
    end;
  end;

  // playing line
  {$IFDEF UseMIDIPort}
  if (MidiFile <> nil) then
    X := 60 + MidiFile.GetCurrentTime/MidiFile.GetTrackLength*730;
  {$ENDIF}
  DrawLine(X, Y, X, Bottom, 0.3, 0.3, 0.3);

  Result := true;
end;

procedure TScreenEditConvert.OnHide;
begin
{$IFDEF UseMIDIPort}
  FreeAndNil(MidiFile);
  MidiOut.Close;
  FreeAndNil(MidiOut);
{$ENDIF}
end;

end.
