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
 * $URL: svn+ssh://svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenEditConvert.pas $
 * $Id: UScreenEditConvert.pas 2146 2010-02-22 18:27:15Z k-m_schindler $
 *}

unit UScreenEditConvert;

// TODO: Add tracks and channel paging for UI
// TODO: Use a midi lib (or extend it) in order to read channels from midi files

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
  UMenu,
  UMenuInteract,
  UMusic,
  UPath,
  USong,
  USongs,
  UThemes,
  math,
  {$IFDEF UseMIDIPort}
  MidiFile,
  MidiOut,
  MidiCons,
  {$ENDIF}
  SDL2;

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

  TMTrack = record
    Selected:  boolean;
    Note:      array of TMidiNote;
    Name:      UTF8String; // normally ASCII
    Status:    set of (tsNotes, tsLyrics); //< track contains notes, lyrics or both
    LyricType: set of TLyricType;
    NoteType:  (ntNone, ntAvail);
    HighNote, LowNote: byte; // highest and lowest note of this track
  end;

  TChannel = record
    Filter: boolean;
    Name:   UTF8String; // normally ASCII
  end;

  TNote = record
    StartBeat:   integer;
    Len:         integer;
    Tone:        integer;
    Lyric:       UTF8String;
    NewSentence: boolean;
  end;

  TArrayMTrack = array of TMTrack;
  TArrayChannel = array of TChannel;

  TScreenEditConvert = class(TMenu)
    private
      MTracks:   TArrayMTrack;      // current tracks
      Channels:  TArrayChannel;    // current channels

      ColR:      array[0..100] of real;
      ColG:      array[0..100] of real;
      ColB:      array[0..100] of real;

      Len:                  real;
      SelTrack:             integer;    // index of selected track
      SelChannel:           integer;    // index of selected channel
      SelMaxHeight:         boolean;    // set when the selected tack should be maxed to possible height
      HighNote, LowNote:    byte;       // highest and lowest note in all tracks
      LowStart, HighEnd:  integer;      // tracks lowest start and highest end of all tracks

      OffsetHighNote, OffsetLowNote: byte; // offset for zooming

      fFileName: IPath;
      IsFileOpen: boolean;
      IsPlaying: boolean;
      IsPlayingSelective: boolean;

      ShowChannels: boolean;
      TracksArea: TMouseOverRect;

      {$IFDEF UseMIDIPort}
      MidiFile:  TMidiFile;
      MidiOut:   TMidiOutput;
      {$ENDIF}

      BPM:       real;
      Ticks:     real;
      Notes:     array of TNote;

      procedure AddLyric(StartBeat: integer; LyricType: TLyricType; Text: UTF8String);
      procedure Extract(out Song: TSong; out Track: TLines);

      {$IFDEF UseMIDIPort}
      procedure MidiFile1MidiEvent(event: PMidiEvent);
      {$ENDIF}

      function CountSelectedTracks: integer;
      procedure ClearMidi;

      procedure DrawTracks(InWidth: real; Offset: real = 0.0);
      procedure DrawChannels(InWidth: real; Offset: real = 0.5);

      procedure StopPlayback();

    public
      constructor Create; override;
      procedure OnShow; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      function ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean; override;
      function Draw: boolean; override;
      procedure OnHide; override;
  end;

const
  ID='ID_061';   //for help system

implementation

uses
  UDrawTexture,
  UFiles,
  UGraphic,
  UHelp,
  UIni,
  ULanguage,
  ULog,
  UMain,
  UPathUtils,
  USkins,
  UTextEncoding,
  UUnicodeUtils,
  dglOpenGL,
  SysUtils,
  TextGL;

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

  TRACK_SCROLL_ZOOM_AMOUNT = 2;


function TScreenEditConvert.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  SDL_ModState: word;
{$IFDEF UseMIDIPort}
  SResult: TSaveSongResult;
  MidiTrack: TMidiTrack;
  Song:  TSong;
  Lines: TLines;
  i: integer;
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

    SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);

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
          if Interaction = 0 then // open
          begin
            IsFileOpen := true;
            AudioPlayback.PlaySound(SoundLib.Start);
            ScreenOpen.Filename := GamePath.Append('file.mid');
            ScreenOpen.BackScreen := @ScreenEditConvert;
            FadeTo(@ScreenOpen);
          end
          else if Interaction = 1 then // play
          begin
            {$IFDEF UseMIDIPort}
            if (MidiFile <> nil) then
            begin
              if IsPlaying then
              begin
                MidiOut.PutShort(MIDI_STOP, 0, 0);
                MidiOut.PutShort(MIDI_NOTEOFF or 1, 0, 127);
                MidiFile.OnMidiEvent := nil;
                MidiFile.StopPlaying;
                Button[Interaction].Text[0].Text := Language.Translate('SING_EDIT_CONVERT_BUTTON_PLAY');
                IsPlaying := false;
                Button[2].Text[0].Text := Language.Translate('SING_EDIT_CONVERT_BUTTON_PLAYSELECTED');
              end else
              begin
                MidiFile.OnMidiEvent := MidiFile1MidiEvent;
                //MidiFile.GoToTime(MidiFile.GetTrackLength div 2);
                MidiFile.ContinuePlaying;
                Button[Interaction].Text[0].Text := Language.Translate('SING_EDIT_CONVERT_BUTTON_PAUSE');
                IsPlaying := true;
                IsPlayingSelective := false;
              end;
            end;
            {$ENDIF}
          end
          else if Interaction = 2 then // play selected
          begin
            {$IFDEF UseMIDIPort}
            if (MidiFile <> nil) then
            begin
              if CountSelectedTracks > 0 then
              begin
                if IsPlayingSelective then
                begin
                  MidiFile.OnMidiEvent := nil;
                  MidiFile.StopPlaying;
                  Button[Interaction].Text[0].Text := Language.Translate('SING_EDIT_CONVERT_BUTTON_PLAYSELECTED');
                  IsPlayingSelective := false;
                end
                else
                begin
                  if IsPlaying then MidiFile.StopPlaying;
                  MidiFile.OnMidiEvent := nil;

                  // play only selected
                  for i := 0 to High(MTracks) do
                  begin
                    MidiTrack := MidiFile.GetTrack(i);
                    if not assigned(MidiTrack) then continue;
                    if MTracks[i].Selected then MidiTrack.OnMidiEvent := MidiFile1MidiEvent
                    else MidiTrack.OnMidiEvent := nil;
                  end;

                  MidiFile.ContinuePlaying;
                  Button[Interaction].Text[0].Text := Language.Translate('SING_EDIT_CONVERT_BUTTON_PAUSE');
                  Button[1].Text[0].Text := Language.Translate('SING_EDIT_CONVERT_BUTTON_PLAY');
                  IsPlaying := false;
                  IsPlayingSelective := true;
                end;
              end
              else
              begin
                ScreenPopupError.ShowPopup(Language.Translate('EDITOR_ERROR_NO_TRACK_SELECTED'));
              end;
            end;
            {$ENDIF}
          end
          else if Interaction = 3 then // stop
          begin
            StopPlayback();
          end
          else if Interaction = 4 then // save
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
            if not ShowChannels then
            begin
              if (MTracks[SelTrack].NoteType = ntAvail) and
                 (MTracks[SelTrack].LyricType <> []) then
              begin
                if (MTracks[SelTrack].Status = []) then
                  MTracks[SelTrack].Status := [tsNotes]
                else if (MTracks[SelTrack].Status = [tsNotes]) then
                  MTracks[SelTrack].Status := [tsLyrics]
                else if (MTracks[SelTrack].Status = [tsLyrics]) then
                  MTracks[SelTrack].Status := [tsNotes, tsLyrics]
                else if (MTracks[SelTrack].Status = [tsNotes, tsLyrics]) then
                  MTracks[SelTrack].Status := [];
              end
              else if (MTracks[SelTrack].NoteType = ntAvail) then
              begin
                if (MTracks[SelTrack].Status = []) then
                  MTracks[SelTrack].Status := [tsNotes]
                else
                  MTracks[SelTrack].Status := [];
              end
              else if (MTracks[SelTrack].LyricType <> []) then
              begin
                if (MTracks[SelTrack].Status = []) then
                  MTracks[SelTrack].Status := [tsLyrics]
                else
                  MTracks[SelTrack].Status := [];
              end;

              MidiTrack := MidiFile.GetTrack(SelTrack);
              if tsNotes in MTracks[SelTrack].Status then
                MidiTrack.OnMidiEvent := MidiFile1MidiEvent
              else
                MidiTrack.OnMidiEvent := nil;
              MTracks[SelTrack].Selected := assigned(MidiTrack.OnMidiEvent);
            end
            else
            begin
              Channels[SelChannel].Filter := not Channels[SelChannel].Filter;
            end;
          end

          {$ENDIF}
        end;

      SDLK_TAB:
        begin
          if (SDL_ModState = KMOD_LCTRL) then // toggle channels
            begin
              if Length(Channels) > 0 then
              begin
                ShowChannels := not ShowChannels;
                if ShowChannels then SelChannel := 0;
              end;
            end
          else // show help popup
            ScreenPopupHelp.ShowPopup();
        end;

      // zooming controls

      SDLK_KP_MULTIPLY: SelMaxHeight := not SelMaxHeight;

      SDLK_KP_7: OffsetHighNote := Min(Max(0, OffsetHighNote-TRACK_SCROLL_ZOOM_AMOUNT), MTracks[SelTrack].HighNote);
      SDLK_KP_9: OffsetHighNote := Min(OffsetHighNote+TRACK_SCROLL_ZOOM_AMOUNT, HighNote - MTracks[SelTrack].HighNote);
      SDLK_KP_8: OffsetHighNote := 0;

      SDLK_KP_1: OffsetLowNote := Max(0, OffsetLowNote-TRACK_SCROLL_ZOOM_AMOUNT);
      SDLK_KP_3: OffsetLowNote := Min( OffsetLowNote+TRACK_SCROLL_ZOOM_AMOUNT, MTracks[SelTrack].LowNote - LowNote - 1);
      SDLK_KP_2: OffsetLowNote := 0;

      SDLK_RIGHT:
        begin
          InteractNext;
        end;

      SDLK_LEFT:
        begin
          InteractPrev;
        end;

      SDLK_DOWN, SDLK_UP:
        begin
          if ShowChannels then
          begin
            inc(SelChannel, ifthen(PressedKey = SDLK_DOWN, 1, -1));
            if SelChannel < 0 then SelChannel := High(Channels)
            else if SelChannel > High(Channels) then SelChannel := 0;
          end
          else
          begin
            inc(SelTrack, ifthen(PressedKey = SDLK_DOWN, 1, -1));
            if SelTrack < 0 then SelTrack := High(MTracks)
            else if SelTrack > High(MTracks) then SelTrack := 0;

            SelMaxHeight := false;
            OffsetHighNote := 0;
            OffsetLowNote := 0;

          end;
        end;
    end;
  end;
end;

function TScreenEditConvert.ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean;
var
  seektime: real;
  i: integer;
  {$IFDEF UseMIDIPort}
  MidiTrack: TMidiTrack;
  {$ENDIF}
begin
  Result := inherited ParseMouse(MouseButton, BtnDown, X, Y);

  // transfer mousecords to the 800x600 raster we use to draw
  X := Min(RenderW, Round((X / (ScreenW / Screens)) * RenderW));
  Y := Round((Y / ScreenH) * RenderH);

  if BtnDown and (MouseButton = SDL_BUTTON_LEFT) then
  begin
    if InRegion(X, Y, TracksArea) then
    begin
      {$IFDEF UseMIDIPort}
      MidiFile.OnMidiEvent := nil;

      seektime := ((X - TracksArea.X) / TracksArea.W);
      seektime := seektime * MidiFile.GetTrackLength;
      if seektime < MidiFile.GetCurrentTime then
        MidiFile.GoToTime(trunc(seektime));

      for i := 0 to High(MTracks) do
      begin
        MidiTrack := MidiFile.GetTrack(i);
        if not assigned(MidiTrack) then continue;
        if not IsPlayingSelective or MTracks[i].Selected then
          MidiTrack.OnMidiEvent := MidiFile1MidiEvent;
      end;

      MidiFile.PlayToTime(trunc(seektime));
      MidiFile.ContinuePlaying;
      {$ENDIF}
    end;
  end;
end;

procedure TScreenEditConvert.AddLyric(StartBeat: integer; LyricType: TLyricType; Text: UTF8String);
var
  N:    integer;
begin
  // find corresponding note
  N := 0;
  while (N <= High(Notes)) do
  begin
    if Notes[N].StartBeat = StartBeat then
      Break;
    Inc(N);
  end;

  // check if note was found
  if (N > High(Notes)) then
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
      Notes[N].NewSentence := true;
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
      Notes[N].NewSentence := true;
    end;
  end;

  // overwrite lyric or append
  if Notes[N].Lyric = '-' then
    Notes[N].Lyric := Text
  else
    Notes[N].Lyric := Notes[N].Lyric + Text;
end;

procedure TScreenEditConvert.Extract(out Song: TSong; out Track: TLines);

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
  SetLength(Notes, 0);

  // extract notes
  for T := 0 to High(MTracks) do
  begin
    if tsNotes in MTracks[T].Status then
    begin
      for N := 0 to High(MTracks[T].Note) do
      begin
        if (MTracks[T].Note[N].EventType = MIDI_EVENTTYPE_NOTEON) and
           (MTracks[T].Note[N].Data2 > 0) then
        begin
          Nu := Length(Notes);
          SetLength(Notes, Nu + 1);
          Notes[Nu].StartBeat := Round(MTracks[T].Note[N].Start / Ticks);
          Notes[Nu].Len := Round(MTracks[T].Note[N].Len / Ticks);
          Notes[Nu].Tone := MTracks[T].Note[N].Data1 - 12*5;
          Notes[Nu].Lyric := '-';
        end;
      end;
    end;
  end;

  // extract lyrics (and artist + title info)
  for T := 0 to High(MTracks) do
  begin
    if not (tsLyrics in MTracks[T].Status) then
      Continue;

    for N := 0 to High(MTracks[T].Note) do
    begin
      if (MTracks[T].Note[N].Event = MIDI_EVENT_META) then
      begin
        // determine and validate lyric meta tag
        if (ltKMIDI in MTracks[T].LyricType) and
           (MTracks[T].Note[N].Data1 = MIDI_META_TEXT) then
        begin
          Text := MTracks[T].Note[N].Str;
          
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
        else if (ltSMFLyric in MTracks[T].LyricType) and
                (MTracks[T].Note[N].Data1 = MIDI_META_LYRICS) then
        begin
          LyricType := ltSMFLyric;
        end
        else
        begin
          // unknown meta event
          Continue;
        end;

        AddLyric(Round(MTracks[T].Note[N].Start / Ticks), LyricType, MTracks[T].Note[N].Str);
      end;
    end;
  end;

  // sort notes
  for N := 0 to High(Notes) do
    for Nu := 0 to High(Notes)-1 do
      if Notes[Nu].StartBeat > Notes[Nu+1].StartBeat then
      begin
        NoteTemp := Notes[Nu];
        Notes[Nu] := Notes[Nu+1];
        Notes[Nu+1] := NoteTemp;
      end;

  // move to 0 at beginning
  Move := Notes[0].StartBeat;
  for N := 0 to High(Notes) do
    Notes[N].StartBeat := Notes[N].StartBeat - Move;

  // copy notes
  SetLength(Track.Lines, 1);
  Track.Number      := 1;
  Track.High        := 0;
  Track.CurrentLine := 0;
  Track.Resolution  := 0;
  Track.NotesGAP    := 0;
  Track.ScoreValue  := 0;

  C := 0;
  N := 0;
  Track.Lines[C].HighNote := -1;

  for Nu := 0 to High(Notes) do
  begin
    if Notes[Nu].NewSentence then // new line
    begin
      SetLength(Track.Lines, Length(Track.Lines)+1);
      Track.Number := Track.Number + 1;
      Track.High := Track.High + 1;
      C := C + 1;
      N := 0;
      SetLength(Track.Lines[C].Notes, 0);
      Track.Lines[C].HighNote := -1;

      //Calculate Start of the Last Sentence
      if (C > 0) and (Nu > 0) then
      begin
        Max := Notes[Nu].StartBeat;
        Min := Notes[Nu-1].StartBeat + Notes[Nu-1].Len;
        
        case (Max - Min) of
          0:    Track.Lines[C].StartBeat := Max;
          1:    Track.Lines[C].StartBeat := Max;
          2:    Track.Lines[C].StartBeat := Max - 1;
          3:    Track.Lines[C].StartBeat := Max - 2;
          else
            if ((Max - Min) > 4) then
              Track.Lines[C].StartBeat := Min + 2
            else
              Track.Lines[C].StartBeat := Max;

        end; // case

      end;
    end;

    // create space for new note
    SetLength(Track.Lines[C].Notes, Length(Track.Lines[C].Notes)+1);
    Inc(Track.Lines[C].HighNote);

    // initialize note
    Track.Lines[C].Notes[N].StartBeat := Notes[Nu].StartBeat;
    Track.Lines[C].Notes[N].Duration := Notes[Nu].Len;
    Track.Lines[C].Notes[N].Tone := Notes[Nu].Tone;
    Track.Lines[C].Notes[N].Text := DecodeStringUTF8(Notes[Nu].Lyric, DEFAULT_ENCODING);
    Track.Lines[C].Notes[N].NoteType := ntNormal;
    Inc(N);
  end;
end;

function TScreenEditConvert.CountSelectedTracks: integer;
var
  T:    integer; // track
begin
  Result := 0;
  for T := 0 to High(MTracks) do
    if tsNotes in MTracks[T].Status then
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
  {$IFDEF UseMIDIPort}
  MidiFile := nil;
  MidiOut := nil;
  {$ENDIF}

  // TODO: Add midi channel filtering. It should allow to filer channels of instruments in single-track midi files
  //       The following lines can be uncommented to test the current UI functionality. With
  //       the TAB button, you can toggle the visible channels and select them with SPACE to filter them.
  //       The notes of these channel should be filter in the extraction method
  //SetLength(Channels, 8);
  //Channels[0].Name := 'Flute';
  //Channels[1].Name := 'Bass';
  //Channels[2].Name := 'Drum';
  //Channels[3].Name := 'Piano';
  //Channels[4].Name := 'Trumpet';
  //Channels[5].Name := 'Violin';
  //Channels[6].Name := 'Oboe';
  //Channels[7].Name := 'Gramophone';

  LoadFromTheme(Theme.EditConvert);

  AddButton(Theme.EditConvert.ButtonOpen);
  AddButton(Theme.EditConvert.ButtonPlay);
  AddButton(Theme.EditConvert.ButtonPlaySelected);
  AddButton(Theme.EditConvert.ButtonStop);
  AddButton(Theme.EditConvert.ButtonSave);

  fFileName := PATH_NONE;

  // generate color array, randomize color
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

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenEditConvert');

  Interaction := 0;

  // BgMusic distracts too much, pause it
  SoundLib.PauseBgMusic;

  // abort if File-open dialog wasn't shown previously, don't need to do anything without a file
  if not IsFileOpen then Exit;
  IsFileOpen := false;

{$IFDEF UseMIDIPort}
  // Filename is only <> PATH_NONE if we called the OpenScreen before
  fFilename := ScreenOpen.Filename;
  if (fFilename = PATH_NONE) then
    Exit;
  ScreenOpen.Filename := PATH_NONE;

  // clear old midi runtime
  ClearMidi;

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
      MidiFile := nil;
    end;
  end;

  if (not FileOpened) then
  begin
    ScreenPopupError.ShowPopup(Language.Translate('ERROR_FILE_NOT_FOUND'));
    Exit;
  end;

  MidiOut := TMidiOutput.Create(nil);
  Log.LogInfo(MidiOut.ProductName, 'MIDI');
  MidiOut.Open;
  SetLength(MTracks, 0);

  Len := 0;
  SelTrack := 0;
  SelMaxHeight := false;
  OffsetHighNote := 0;
  OffsetLowNote := 0;
  BPM := MidiFile.Bpm;
  Ticks := MidiFile.TicksPerQuarter / 4;

  KMIDITrackIndex := -1;
  SMFTrackIndex := -1;

  HighNote := 0;
  LowNote := 255;

  HighEnd := 0;
  LowStart := MaxInt;

  SetLength(MTracks, MidiFile.NumberOfTracks);
  for T := 0 to MidiFile.NumberOfTracks-1 do
  begin
    MTracks[T].HighNote := 0;
    MTracks[T].LowNote := 255;

    MidiTrack := MidiFile.GetTrack(T);
    MidiTrack.OnMidiEvent := nil;
    MTracks[T].Name := DecodeStringUTF8(MidiTrack.getName, DEFAULT_ENCODING);
    MTracks[T].NoteType := ntNone;

    SetLength(MTracks[T].Note, MidiTrack.getEventCount());
    for N := 0 to MidiTrack.getEventCount-1 do
    begin
      MidiEvent := MidiTrack.GetEvent(N);

      MTracks[T].Note[N].Start     := MidiEvent.time;
      MTracks[T].Note[N].Len       := MidiEvent.len;
      MTracks[T].Note[N].Event     := MidiEvent.event;
      MTracks[T].Note[N].EventType := MidiEvent.event shr 4;
      MTracks[T].Note[N].Channel   := MidiEvent.event and $0F;
      MTracks[T].Note[N].Data1     := MidiEvent.data1;
      MTracks[T].Note[N].Data2     := MidiEvent.data2;
      MTracks[T].Note[N].Str       := DecodeStringUTF8(MidiEvent.str, DEFAULT_ENCODING);

      // store lowest and highest note for the track and file
      if (MTracks[T].Note[N].Event <> MIDI_EVENT_META) then
      begin
        if MidiEvent.len > 0 then // ignore invalid notes
        begin
          HighEnd := Max(HighEnd, MidiEvent.time);
          LowStart := Min(LowStart, MidiEvent.time+MidiEvent.len);
          HighNote := Max(HighNote, MidiEvent.data1);
          LowNote := Min(LowNote, MidiEvent.data1);

          MTracks[T].HighNote := Max(MTracks[T].HighNote, MidiEvent.data1);
          MTracks[T].LowNote := Min(MTracks[T].LowNote, MidiEvent.data1);
        end;
      end;

      if (MTracks[T].Note[N].Event = MIDI_EVENT_META) then
      begin
        case (MTracks[T].Note[N].Data1) of
          MIDI_META_TEXT: begin
            // KMIDI lyrics (uses MIDI_META_TEXT events)
            if (StrLComp(PAnsiChar(MTracks[T].Note[N].Str), '@KMIDI KARAOKE FILE', 19) = 0) and
               (High(MTracks) >= T+1) then
            begin
              // The '@KMIDI ...' mark is in the first track (mostly named 'Soft Karaoke')
              // but the lyrics are in the second track (named 'Words')
              MTracks[T+1].LyricType := MTracks[T+1].LyricType + [ltKMIDI];
              KMIDITrackIndex := T+1;
            end;
          end;
          MIDI_META_LYRICS: begin
            // lyrics in Standard Midi File format found (uses MIDI_META_LYRICS events)
            MTracks[T].LyricType := MTracks[T].LyricType + [ltSMFLyric];
            SMFTrackIndex := T;
          end;
        end;
      end
      else if (MTracks[T].Note[N].EventType = MIDI_EVENTTYPE_NOTEON) then
      begin
        // notes available
        MTracks[T].NoteType := ntAvail;
      end;

      if MTracks[T].Note[N].Start + MTracks[T].Note[N].Len > Len then
        Len := MTracks[T].Note[N].Start + MTracks[T].Note[N].Len;
    end;
  end;

  // set default lyric track. Prefer KMIDI.
  if (KMIDITrackIndex > -1) then
    MTracks[KMIDITrackIndex].Status := MTracks[KMIDITrackIndex].Status + [tsLyrics]
  else if (SMFTrackIndex > -1) then
    MTracks[SMFTrackIndex].Status := MTracks[SMFTrackIndex].Status + [tsLyrics];
{$ENDIF}
end;

function TScreenEditConvert.Draw: boolean;
begin
  // draw static menu
  Result := inherited Draw;

  // abort if no tracks loaded
  if Length(MTracks) < 1 then Exit;

  if ShowChannels then
  begin
    DrawTracks(RenderW*0.7);
    DrawChannels(RenderW*0.3, 0.7);
  end
  else DrawTracks(RenderW);

end;

procedure TScreenEditConvert.DrawTracks(InWidth: real; Offset: real = 0.0);
  procedure MidiTimeToSeconds(InTime: integer; out Minutes, Seconds: integer);
  var t: real;
  begin
    t := (((BPM * Ticks * 4) / 60000) * InTime) / 1000;
    Minutes := trunc(t) div 60;
    Seconds := trunc(t) mod 60;
  end;

var
  Count:    integer;
  Count2:   integer;
  Bottom:   real;
  Padding:  real;
  Right:    real;
  Top:      real;
  Footer:   real;
  FontSize: real;
  Y:        real;
  Height:   real;
  YSkip:    real;
  TrackPos: real;
  TrackPadding:  real;
  TrackWidth: real;
  NoteDiff, TrackDiff, TrackDiffSel, TrackDiffOther: integer;
  YSelected: real;
  YHeight, YNote:    real;

  XTrack: real;
  TimeWidth: real;
  tm, ts: integer;

begin

  // define
  Top := 200;
  XTrack := 60;
  Padding := 10;
  FontSize := 12;
  Footer := 70;

  // calc
  Height := RenderH - Padding - Top - Footer;
  Bottom := Top + Height;
  Right := InWidth - Padding;
  Y := Top;

  if SelMaxHeight then
  begin
    YSkip := FontSize + 2;
    YSelected := Height;
  end
  else
  begin
    YSkip := EnsureRange(Height / (Length(MTracks)+20), FontSize + 2, FontSize * 2);
    YSelected := Min(Max(YSkip, Height - Length(MTracks)*YSkip), 6*YSkip);
  end;

  TrackPadding := 0.15 * YSkip;
  TrackDiffSel := (HighNote-OffsetHighNote) - (LowNote+OffsetLowNote);
  TrackDiffOther := HighNote - LowNote;
  TrackWidth := InWidth - XTrack - 5 - Padding;


  // Draw time bar
  DrawLine(XTrack, Y-YSkip, XTrack, Y, 0, 0, 0); // start
  DrawLine(Right, Y-YSkip, Right, Y, 0, 0, 0); // end
  // draw time
  SetFontSize(FontSize);
  TimeWidth := glTextWidth('00:00');
  SetFontPos(XTrack-TimeWidth-5, Y-YSkip);
  glPrint(Format('%.2d:%.2d', [0,0]));
  SetFontPos(Right-TimeWidth-5, Y-YSkip);
  {$IFDEF UseMIDIPort}
  MidiTimeToSeconds(MidiFile.GetTrackLength, tm, ts);
  {$ENDIF}
  glPrint(Format('%.2d:%.2d', [tm,ts]));


  // highlight selected track
  DrawQuad(Padding, Y+ ifthen(SelMaxHeight, 0, SelTrack*YSkip), Right-Padding, YSelected, 0.8, 0.8, 0.8);

  // vertical grid
  DrawLine(Padding,   Y,  Padding,   Bottom, 0, 0, 0);
  DrawLine(XTrack,    Y,  XTrack,    Bottom, 0, 0, 0);
  DrawLine(Right,     Y,  Right,     Bottom, 0, 0, 0);

  glColor3f(0, 0, 0);
  DrawLine(Padding, Y, Right, Y, 0, 0, 0);
  for Count := 0 to High(MTracks) do
  begin
    YHeight := ifthen(Count = SelTrack, YSelected, YSkip);
    TrackDiff := ifthen(SelTrack = Count, TrackDiffSel, TrackDiffOther);

    // if maxed mode, skip all non-selected tracks
    if SelMaxHeight and (Count <> SelTrack) then continue;

    // draw track-selection
    if MTracks[Count].Status <> [] then
    begin
      DrawQuad(Padding, Y, XTrack-Padding, YHeight, 0.8, 0.3, 0.3);
    end;

    // draw track info
    if MTracks[Count].NoteType = ntAvail then
    begin
      if tsNotes in MTracks[Count].Status then
        glColor3f(0, 0, 0)
      else
        glColor3f(0.7, 0.7, 0.7);
      SetFontPos(Padding+FontSize, Y);
      SetFontSize(FontSize);
      glPrint('N');
    end;
    if MTracks[Count].LyricType <> [] then
    begin
      if tsLyrics in MTracks[Count].Status then
        glColor3f(0, 0, 0)
      else
        glColor3f(0.7, 0.7, 0.7);
      SetFontPos(Padding+30, Y);
      SetFontSize(FontSize);
      glPrint('L');
    end;

    // Draw track lines
    DrawLine(Padding, Y+YHeight, Right, Y+YHeight, 0, 0, 0);

    // Draw track names
    SetFontPos(XTrack+5, Y);
    SetFontSize(FontSize);
    glPrint(MTracks[Count].Name);

    // draw track notes
    //if Count = SelTrack then
    for Count2 := 0 to High(MTracks[Count].Note) do
    begin
      NoteDiff := MTracks[Count].Note[Count2].Data1 - (LowNote+ ifthen(Count = SelTrack, OffsetLowNote, 0));
      YNote := NoteDiff / TrackDiff;

      if MTracks[Count].Note[Count2].EventType = MIDI_EVENTTYPE_NOTEON then
        DrawQuad(XTrack + MTracks[Count].Note[Count2].Start/Len * TrackWidth,
                 Y+YHeight - (YHeight - 2*Padding)*YNote - Padding,
                 Max(1.0, (MTracks[Count].Note[Count2].Len/Len) * TrackWidth), Max(1.0, YHeight / TrackDiff),
                 ColR[Count], ColG[Count], ColB[Count]);
      if MTracks[Count].Note[Count2].EventType = MIDI_EVENTTYPE_META_SYSEX then
        DrawLine(XTrack + MTracks[Count].Note[Count2].Start/Len * TrackWidth, Y+YHeight - 0.25*YSkip,
                 XTrack + MTracks[Count].Note[Count2].Start/Len * TrackWidth, Y+YHeight,
                 ColR[Count], ColG[Count], ColB[Count]);
    end;

    Y := Y + YHeight;
  end;

  // last horizontal track line
  DrawLine(Padding, Y+YHeight, Right, Y+YHeight, 0, 0, 0);


  // update tracks area for mouse interaction (e.g. seeking bar)
  TracksArea.X := XTrack;
  TracksArea.Y := Top - YSkip;
  TracksArea.W := Right - TracksArea.X;
  TracksArea.H := Y - YHeight;

  // playing line
  {$IFDEF UseMIDIPort}
  if (MidiFile <> nil) then
    // TODO: use proper event to stop midi playback
    TrackPos := XTrack + (MidiFile.GetCurrentTime/MidiFile.GetTrackLength) * TrackWidth;
    if MidiFile.GetCurrentTime > MidiFile.GetTrackLength then StopPlayback;
  {$ENDIF}
  DrawLine(TrackPos, Top, TrackPos, Bottom, 0.3, 0.3, 0.3);

  // TODO: time stamp (in seconds) seems to run slower than actual seconds. IIRC the calculation is correct. Could be related to the Mouse lag while playing
  SetFontSize(FontSize);
  SetFontPos(Max(XTrack + 5, Min(Right - 5 - 2*TimeWidth - 5, TrackPos-(0.5*TimeWidth))), Top-YSkip);
  {$IFDEF UseMIDIPort}
  MidiTimeToSeconds(MidiFile.GetCurrentTime, tm, ts);
  {$ENDIF}
  glPrint(Format('%.2d:%.2d', [tm, ts]));
end;

procedure TScreenEditConvert.DrawChannels(InWidth: real; Offset: real = 0.5);
var
  Count:    integer;
  Count2:   integer;

  Top:      real;
  Padding:  real;
  FontSize: real;

  Bottom:   real;
  Left:     real;
  Right:    real;
  Y, X:     real;
  Height:   real;

  YHeight:    real;
  XPadding: real;
  XChannel: real;

begin

  // define
  Top := 100;
  Padding := 10;
  FontSize := 12;

  // calc
  Left := RenderW * Offset;
  Height := RenderH - Padding - Top;

  Y := Top;
  XPadding := Left + Padding;
  YHeight := EnsureRange(Height / (Length(Channels)+20), FontSize + 2, FontSize * 2);

  Bottom := Top + Min(YHeight * Length(Channels), Height);
  Right := Left + InWidth - Padding;

  // highlight selected channel
  DrawQuad(XPadding, Y+SelChannel*YHeight, InWidth - 2*Padding, YHeight, 0.8, 0.8, 0.8);

  SetFontSize(FontSize);
  X := glTextWidth('XXX');
  XChannel := 0;

  for Count := 0 to High(Channels) do
  begin
    glColor3f(0, 0, 0);

    // draw channel-filter state
    if Channels[Count].Filter then
    begin
      SetFontPos(XPadding, Y);
      SetFontSize(FontSize);

      glPrint(' X ');
    end;

    // draw channel name
    SetFontPos(XPadding+X, Y);
    SetFontSize(FontSize);
    XChannel := Max(XChannel, glTextWidth(Channels[Count].Name));
    glPrint(Channels[Count].Name);

    // Draw track lines
    DrawLine(XPadding, Y, Right, Y, 0, 0, 0);

    Y := Y + YHeight;
  end;

  XChannel := XPadding + X + XChannel + 10;

  // grid
  DrawLine(XPadding, Y, Right, Y, 0, 0, 0); // horz
  DrawLine(XPadding, Top, XPadding, Bottom, 0, 0, 0);
  DrawLine(XChannel, Top, XChannel, Bottom, 0, 0, 0);
  DrawLine(Right,    Top, Right,    Bottom, 0, 0, 0);
end;

procedure TScreenEditConvert.OnHide;
begin
{$IFDEF UseMIDIPort}
  if not IsFileOpen then ClearMidi; // do not clear midi when opening file
{$ENDIF}
end;

procedure TScreenEditConvert.ClearMidi;
begin
{$IFDEF UseMIDIPort}
  // clear data
  MTracks := nil;

  FreeAndNil(MidiFile);

  If assigned(MidiOut) then
  begin
    MidiOut.Close;
    FreeAndNil(MidiOut);
  end;
{$ENDIF}
end;

procedure TScreenEditConvert.StopPlayback;
begin
{$IFDEF UseMIDIPort}
  if (MidiFile <> nil) then
  begin
    MidiFile.OnMidiEvent := nil;

    // seek to start first, to update time bar
    MidiFile.PlayToTime(0);
    MidiFile.GoToTime(0);

    MidiFile.StopPlaying;

    MidiOut.Close;
    MidiOut.Open;

    Button[1].Text[0].Text := Language.Translate('SING_EDIT_CONVERT_BUTTON_PLAY');
    Button[2].Text[0].Text := Language.Translate('SING_EDIT_CONVERT_BUTTON_PLAYSELECTED');
    IsPlaying := false;
    IsPlayingSelective := false;
  end;
{$ENDIF}
end;

end.
