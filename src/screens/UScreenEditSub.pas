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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenEditSub.pas $
 * $Id: UScreenEditSub.pas 3103 2014-11-22 23:21:19Z k-m_schindler $
 *}

unit UScreenEditSub;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
{$I switches.inc}

uses
  UCommon,
  UEditorLyrics,
  UFiles,
  UFilesystem,
  UGraphicClasses,
  UIni,
  UMenu,
  UMenuText,
  UMusic,
  UPath,
  URecord,
  USong,
  USongs,
  UTexture,
  UThemes,
  UTime,
  dglOpenGL,
  Math,
  {$IFDEF UseMIDIPort}
  MidiOut,
  MidiCons,
  {$ENDIF}
  sdl2,
  strutils,
  SysUtils;

type

  TMedleyNotes = record
    start: TPos;
    end_: TPos;
    Preview: TPos;
    isStart: boolean;   //start beat is declared
    isEnd: boolean;     //end beat is declared
    isCustom: boolean;
  end;

  TVisibleHeaders = record
    Title:           UTF8String;
    Artist:          UTF8String;
    Language:        UTF8String;
    Edition:         UTF8String;
    Genre:           UTF8String;
    Year:            Integer;
    Creator:         UTF8String;
    Mp3:             IPath;
    Mp3Id:           Integer;
    Cover:           IPath;
    CoverId:         Integer;
    Background:      IPath;
    BackgroundId:    Integer;
    Video:           IPath;
    VideoId:         Integer;
    VideoGAP:        Real;
    BPM:             array of TBPM;
    GAP:             Real;
    StartTag:        Real;
    EndTag:          longint;
    MedleyStartBeat: longint;
    MedleyEndBeat:   longint;
    PreviewStart:    Real;
    Relative:        boolean;
  end;

  TScreenEditSub = class(TMenu)
    private

      CurrentBeat:             integer;
      //Variable is True if no Song is loaded
      Error:                   boolean;

      TextNote:                integer;
      TextSentence:            integer;
      {
      TextTitle:               integer;
      TextArtist:              integer;
      TextMp3:                 integer;
      TextBPM:                 integer;
      TextGAP:                 integer;
      }
      TextDebug:               integer;
      {
      TextNStart:              integer;
      TextNLength:             integer;
      TextNTon:                integer;
      TextNText:               integer;
      }
      CurrentNote:             array[0..1] of integer;
      PlaySentence:            boolean;
      PlayOne:                 boolean;
      PlayOneMidi:             boolean;
      PlaySentenceMidi:        boolean;
      midinotefound:           boolean; // if one note was found
      PlayVideo:               boolean;
      PlayStopTime:            real;
      LastClick:               integer;
      Click:                   boolean;
      CopySrc:                 integer;
      {$IFDEF UseMIDIPort}
      MidiOut:                 TMidiOutput;
      MidiStart:               real;
      MidiStop:                real;
      MidiTime:                real;
      MidiPos:                 real;
      MidiLastNote:            integer;
      {$ENDIF}

      //for mouse move
      LastPressedMouseButton:  boolean;
      LastPressedMouseType:    integer;
      LastPressedNote:         integer;
      PressedNoteId:           integer;

      TextPosition:            integer;
      TextEditMode:            boolean;
      TitleEditMode:           boolean;
      ArtistEditMode:          boolean;
      LanguageEditMode:        boolean;
      EditionEditMode:         boolean;
      GenreEditMode:           boolean;
      YearEditMode:            boolean;
      CreatorEditMode:         boolean;
      // to interactive divide note
      LastClickTime:           integer;

      BackupEditText:          UTF8String; //backup of current text in text-edit-mode
      CurrentEditText:         UTF8String; // current edit text
      editLengthText:          integer;
      CurrentSlideId:          integer;
      //title header
      TitleSlideId:            integer;
      TitleData:               integer;
      TitleVal:                array of UTF8String;
      SlideTitleIndex:         integer;
      // artist header
      ArtistSlideId:           integer;
      ArtistData:              integer;
      ArtistVal:               array of UTF8String;
      SlideArtistIndex:        integer;
      // language header
      LanguageSlideId:         integer;
      LanguageData:            integer;
      LanguageVal:             array of UTF8String;
      SlideLanguageIndex:      integer;
      // edition header
      EditionSlideId:          integer;
      EditionData:             integer;
      EditionVal:              array of UTF8String;
      SlideEditionIndex:       integer;
      // genre header
      GenreSlideId:            integer;
      GenreData:               integer;
      GenreVal:                array of UTF8String;
      SlideGenreIndex:         integer;
      // year header
      YearSlideId:             integer;
      YearData:                integer;
      YearVal:                 array of UTF8String;
      SlideYearIndex:          integer;
      // creator header
      CreatorSlideId:          integer;
      CreatorData:             integer;
      CreatorVal:              array of UTF8String;
      SlideCreatorIndex:       integer;
      // mp3 header
      MP3SlideId:              integer;
      MP3Data:                 integer;
      MP3Val:                  array of UTF8String;
      SlideMP3Index:           integer;
      // Cover header
      CoverSlideId:            integer;
      CoverData:               integer;
      CoverVal:                array of UTF8String;
      SlideCoverIndex:         integer;
      // Background header
      BackgroundSlideId:       integer;
      BackgroundData:          integer;
      BackgroundVal:           array of UTF8String;
      SlideBackgroundIndex:    integer;
      // Video header
      VideoSlideId:            integer;
      VideoData:               integer;
      VideoVal:                array of UTF8String;
      SlideVideoIndex:         integer;
      // VideoGap header
      VideoGapSlideId:         integer;
      VideoGapData:            integer;
      VideoGapVal:             array of UTF8String;
      SlideVideoGapIndex:      integer;
      // BPM header
      BPMSlideId:              integer;
      BPMData:                 integer;
      BPMVal:                  array of UTF8String;
      SlideBPMIndex:           integer;
      // GAP header
      GAPSlideId:              integer;
      GAPData:                 integer;
      GAPVal:                  array of UTF8String;
      SlideGAPIndex:           integer;
      // StartTag header
      StartTagSlideId:         integer;
      StartTagData:            integer;
      StartTagVal:             array of UTF8String;
      SlideStartTagIndex:      integer;
      // EndTag header
      EndTagSlideId:           integer;
      EndTagData:              integer;
      EndTagVal:               array of UTF8String;
      SlideEndTagIndex:        integer;
      // MedleyStart header
      MedleyStartSlideId:      integer;
      MedleyStartData:         integer;
      MedleyStartVal:          array of UTF8String;
      SlideMedleyStartIndex:   integer;
      // MedleyEnd header
      MedleyEndSlideId:        integer;
      MedleyEndData:           integer;
      MedleyEndVal:            array of UTF8String;
      SlideMedleyEndIndex:     integer;
      // PreviewStart header
      PreviewStartSlideId:     integer;
      PreviewStartData:        integer;
      PreviewStartVal:         array of UTF8String;
      SlidePreviewStartIndex:  integer;
      // Relative header
      RelativeSlideId:         integer;
      RelativeData:            integer;
      RelativeVal:             array of UTF8String;
      SlideRelativeIndex:      integer;
      // Start header
      StartSlideId:            integer;
      StartData:               integer;
      StartVal:                array of UTF8String;
      SlideStartIndex:         integer;
      // Duration header
      DurationSlideId:         integer;
      DurationData:            integer;
      DurationVal:             array of UTF8String;
      SlideDurationIndex:      integer;
      // Tone header
      ToneSlideId:             integer;
      ToneData:                integer;
      ToneVal:                 array of UTF8String;
      SlideToneIndex:          integer;
      // Text header
      LyricSlideId:            integer;
      LyricData:               integer;
      LyricVal:                array of UTF8String;
      SlideLyricIndex:         integer;
      // Volume Slide
      VolumeAudioSlideId:      integer;
      VolumeMidiSlideId:       integer;
      VolumeClickSlideId:      integer;
      VolumeAudioIndex:        integer;
      VolumeMidiIndex:         integer;
      VolumeClickIndex:        integer; //for update slide

      VolumeAudio:             array of UTF8String;
      VolumeMidi:              array of UTF8String;
      VolumeClick:             array of UTF8String;
      // control buttons
      UndoButtonId:            integer;
      PreviousSeqButtonID:     integer;
      NextSeqButtonID:         integer;
      FreestyleButtonID:       integer;
      GoldButtonID:            integer;
      PlayOnlyButtonID:        integer;
      PlayWithNoteButtonID:    integer;
      PlayNoteButtonID:        integer;
      // background image & video preview
      BackgroundImageId:       integer;
      Empty:                   array of UTF8String;   //temporary variable to initialize slide - todo change
      // background for notes to posibility move note
      //NotesBackgroundId:       integer;
      // player static picture
      playerIconId:            array[1..2] of integer;
      //  currentX, CurrentY
      CurrentX:                integer;
      CurrentY:                integer;
      LastX:                   integer;
      LastY:                   integer;
      Xmouse:                  integer;

      resize_note_left:        boolean;
      resize_note_right:       boolean;
      move_note:               boolean;

      CurrentTrack:            integer; // 0 or 1 (for duets)
      EditorLyrics:            array[0..1] of TEditorLyrics;

      //undo declaration
      UndoLines:               array of TLines;
      UndoStateNote:           array of integer; //UNDO: note's position
      CurrentUndoLines:        integer;
      UndoHeader:              array of TVisibleHeaders;

      //video view
      fCurrentVideo: IVideo;

      //singtrack
      CurrentSound:            TCaptureBuffer;
      // Interactive note
      InteractiveNoteId:       array of integer;
      TransparentNoteButtonId: array of integer;
      // Interactive Line bar
      InteractiveLineId:       array of integer;
      TransparentLineButtonId: array of integer;

      {$IFDEF UseMIDIPort}
      PlayMidi:                boolean;
	  {$ENDIF}// Midi

      // medley
      MedleyNotes:             TMedleyNotes;

      procedure DivideBPM;
      procedure MultiplyBPM;
      procedure LyricsCapitalize;
      procedure LyricsCorrectSpaces;
      procedure FixTimings;
      procedure DivideSentence;
      procedure JoinSentence;
      procedure NextSentence;
      procedure PreviousSentence;
      procedure DivideNote(doubleclick: boolean);
      procedure DeleteNote;
      procedure DeleteSentence;
      procedure TransposeNote(Transpose: integer);
      procedure ChangeWholeTone(Tone: integer);
      procedure MoveAllToEnd(Move: integer);
      procedure MoveTextToRight;
      procedure MarkSrc;
      procedure PasteText;
      procedure CopySentence(Src, Dst: integer);
      procedure CopySentences(Src, Dst, Num: integer);
      procedure MakeSolo;
      procedure MakeDuet;
      function  DuetCopyLine: boolean;
      function DuetMoveLine: boolean;
      procedure CopyLine(SrcTrack, SrcLine, DstTrack, DstLine: integer);
      procedure Refresh;
      procedure CopyToUndo; //copy current lines, mouse position and headers
      procedure CopyFromUndo; //undo last lines, mouse position and headers
      procedure DrawPlayerTrack(X, Y, W: real; Space: integer; CurrentTone: integer; Count: integer; CurrentNote: array of integer);
      procedure DrawInfoBar(X, Y, W, H: integer; ColR, ColG, ColB, Alpha: real; Track: integer);
      procedure DrawText(X, Y, W, H: real; Track: integer; NumLines: integer = 10);
      //video view
      procedure StartVideoPreview();
      procedure StopVideoPreview();
      procedure UpdateVideoPosition(NewPosition: real);
      //Note Name Mod
      function GetNoteName(Note: integer): string;
      // show transparent background note for interactions
      procedure ShowInteractiveBackground;

      procedure UpdateMedleyInfo;
      function GetMedleyLength: real; //if available returns the length of the medley in seconds, otherwise 0

    public
      Tex_PrevBackground:      TTexture;
      FadeOut:                 boolean;

      constructor Create; override;
      procedure OnShow; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      function ParseInputEditText(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
      function ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean; override;
      function Draw: boolean; override;
      procedure OnHide; override;
  end;

const
  ID='ID_064';   //for help system

implementation

uses
  UDisplay,
  UDraw,
  UGraphic,
  UHelp,
  ULanguage,
  ULog,
  UMenuInteract,
  UNote,
  USkins,
  UTextEncoding,
  UUnicodeUtils,
  TextGL;

const
  DEFAULT_FADE_IN_TIME = 8;    //TODO in INI
  DEFAULT_FADE_OUT_TIME = 2;
  NOT_SET = '-';

procedure OnSaveEncodingError(Value: boolean; Data: Pointer);
var
  SResult: TSaveSongResult;
  FilePath: IPath;
  Success: boolean;
begin
  Success := false;
  if (Value) then
  begin
    CurrentSong.Encoding := encUTF8;
    FilePath := CurrentSong.Path.Append(CurrentSong.FileName);
    // create backup file
    FilePath.CopyFile(Path(FilePath.ToUTF8 + '.ansi.bak'), false);
    // store in UTF-8 encoding
    SResult := SaveSong(CurrentSong, Lines, FilePath, boolean(Data));
    Success := (SResult = ssrOK);
  end;

  if (Success) then
    ScreenPopupInfo.ShowPopup(Language.Translate('INFO_FILE_SAVED'))
  else
    ScreenPopupError.ShowPopup(Language.Translate('ERROR_SAVE_FILE_FAILED'));
end;

procedure OnExit(Value: boolean; Data: Pointer);
begin
  Display.CheckOK := Value;
  if (Value) then
  begin
    Display.CheckOK := false;
    AudioPlayback.Stop;
    Display.FadeTo(@ScreenSong);
  end;
end;

// Method for input parsing. If false is returned, GetNextWindow
// should be checked to know the next window to load;
function TScreenEditSub.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  SDL_ModState:  word;
  R:    real;
  SResult: TSaveSongResult;
  i:  integer;
  HasPreview: Boolean;
begin
  Result := true;

  if TextEditMode or TitleEditMode or ArtistEditMode or LanguageEditMode or EditionEditMode or GenreEditMode or YearEditMode or CreatorEditMode then
  begin
    Result := ParseInputEditText(PressedKey, CharCode, PressedDown);
  end
  else
  begin

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT + KMOD_RALT {+ KMOD_CAPS});

  if (PressedDown) then  // Key Down
  begin
    // check normal keys
    case PressedKey of
      SDLK_Q:
        begin
          Result := false;
          Exit;
        end;
      {
      SDLK_S:
        begin
          if SDL_ModState = KMOD_LSHIFT then
          begin

            // Save Song
            SResult := SaveSong(CurrentSong, Lines[CurrentTrack], CurrentSong.Path.Append(CurrentSong.FileName),
                     (SDL_ModState = KMOD_LSHIFT));
            if (SResult = ssrOK) then
            begin
              //ScreenPopupInfo.ShowPopup(Language.Translate('INFO_FILE_SAVED'));
              Text[TextDebug].Text := Language.Translate('INFO_FILE_SAVED');
              SetLength(UndoLines, 0); //clear undo lines
              SetLength(UndoStateNote, 0); //clear undo currentnote state
              SetLength(Undoheader, 0); //clear undo headrers
              CurrentUndoLines := 0;
            end
            else if (SResult = ssrEncodingError) then
            begin
              ScreenPopupCheck.ShowPopup(Language.Translate('ENCODING_ERROR_ASK_FOR_UTF8'), OnSaveEncodingError,
                  Pointer(SDL_ModState = KMOD_LSHIFT), true);
            end;
          end
          else
          begin
            ScreenPopupError.ShowPopup(Language.Translate('ERROR_SAVE_FILE_FAILED'));
          end;
          Exit;
          end;

        end;
      }
      SDLK_S:
        begin
          // handle medley tags first
          if CurrentSong.isDuet then
          begin
            CurrentSong.Medley.Source := msNone;
          end
          else if (MedleyNotes.isStart and MedleyNotes.isEnd) and
             MedleyNotes.isCustom and
            (MedleyNotes.start.line < MedleyNotes.end_.line) and
            (Length(Lines[CurrentTrack].Line)> MedleyNotes.end_.line) and
            (Length(Lines[CurrentTrack].Line[MedleyNotes.end_.line].Note) > MedleyNotes.end_.note) and
            (Length(Lines[CurrentTrack].Line[MedleyNotes.start.line].Note) > MedleyNotes.start.note) then
          begin
            CurrentSong.Medley.Source := msTag;
            CurrentSong.Medley.StartBeat := Lines[CurrentTrack].Line[MedleyNotes.start.line].Note[MedleyNotes.start.note].StartBeat;
            CurrentSong.Medley.EndBeat := Lines[CurrentTrack].Line[MedleyNotes.end_.line].Note[MedleyNotes.end_.note].StartBeat +
              Lines[CurrentTrack].Line[MedleyNotes.end_.line].Note[MedleyNotes.end_.note].Duration;
            CurrentSong.Medley.FadeIn_time := DEFAULT_FADE_IN_TIME;
            CurrentSong.Medley.FadeOut_time := DEFAULT_FADE_OUT_TIME;
          end
          else if MedleyNotes.isCustom then
          begin
            CurrentSong.Medley.Source := msNone;
            CurrentSong.Medley.StartBeat := 0;
            CurrentSong.Medley.EndBeat := 0;
          end;

          // Save Song (SHIFT = relative)
          if SDL_ModState = KMOD_LSHIFT then
          begin
            if (CurrentSong.isDuet) then
            begin
              ScreenPopupError.ShowPopup(Language.Translate('EDIT_POPUP_DUET_RELATIVE_UNSUPPORTED'));
              Exit;
            end;

            if (CurrentSong.Medley.Source = msTag) then
            begin
              ScreenPopupError.ShowPopup(Language.Translate('EDIT_POPUP_MEDLEY_RELATIVE_UNSUPPORTED') + ' ' + Language.Translate('EDIT_POPUP_MEDLEY_DELETED'));
            end;

            CurrentSong.Medley.Source := msNone;
            CurrentSong.Relative := true;
            SResult := SaveSong(CurrentSong, Lines, CurrentSong.Path.Append(CurrentSong.FileName), CurrentSong.Relative); //save with relative timings
          end else
          begin
            CurrentSong.Relative := false;
            SResult := SaveSong(CurrentSong, Lines, CurrentSong.Path.Append(CurrentSong.FileName), CurrentSong.Relative); // save with absolute timings
          end;

          if (SResult = ssrOK) then // saving was successful
          begin
            Text[TextDebug].Text := Language.Translate('INFO_FILE_SAVED');
            SetLength(UndoLines, 0); //clear undo lines
            SetLength(UndoStateNote, 0); //clear undo CurrentNote[CurrentTrack] state
            SetLength(Undoheader, 0); //clear undo headers
            CurrentUndoLines := 0;
            //if not CheckSong then
            //  ScreenPopupError.ShowPopup(Language.Translate(''));

            //CatSongs.Song[SongIndex] := CurrentSong;
          end
          else // saving was unsuccessful
          begin
            ScreenPopupError.ShowPopup(Language.Translate('ERROR_SAVE_FILE_FAILED'));
          end;

          Exit;
        end;

      // set PreviewStart tag
      SDLK_I:
        begin
          CopyToUndo;
          if SDL_ModState and KMOD_SHIFT <> 0 then
          begin
            // set preview start
            R := round(GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].EndBeat) * 1000) / 1000;
            CurrentSong.PreviewStart := ifthen(CurrentSong.PreviewStart <> R, R, -1);
            CurrentSong.HasPreview := CurrentSong.PreviewStart >= 0.0;
            Text[TextDebug].Text := ifthen(CurrentSong.HasPreview, Format(Language.Translate('EDIT_INFO_PREVIEW_SET'), [CurrentSong.PreviewStart]), Language.Translate('EDIT_INFO_PREVIEW_CLEARED'));
          end
          else if InRange(CurrentSong.PreviewStart, 0.0, AudioPlayback.Length) then
          begin
            if SDL_ModState = KMOD_LALT then
            begin // jump and play
              // simulate sentence switch to clear props
              PreviousSentence;

              Lines[CurrentTrack].Current := 0; // update lyric

              Text[TextDebug].Text := Language.Translate('EDIT_INFO_JUMPTO_PREVIEW_AND_PLAY');
              PlayStopTime := AudioPlayback.Length;
              PlaySentence := true;
              Click := false;
              AudioPlayback.Position := CurrentSong.PreviewStart;
              AudioPlayback.Play;

              // play video in sync if visible
              if (fCurrentVideo <> nil) then UpdateVideoPosition(AudioPlayback.Position);
            end
            else if SDL_ModState = 0 then
            begin // jump to preview start
              // simulate sentence switch to clear props
              PreviousSentence;

              CurrentBeat := Floor(GetMidBeat(CurrentSong.PreviewStart - (CurrentSong.GAP) / 1000));
              i := 0; while (i <= Lines[CurrentTrack].High) and (CurrentBeat > Lines[CurrentTrack].Line[i].EndBeat) do Inc(i);
              if i <= High(Lines[CurrentTrack].Line) then
              begin
                Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 1;
                Lines[CurrentTrack].Current := i;

                // finding the right note
                CurrentNote[CurrentTrack] := 0;
                while (CurrentNote[CurrentTrack] <= Lines[CurrentTrack].Line[i].HighNote) and (CurrentBeat > Lines[CurrentTrack].Line[i].Note[CurrentNote[CurrentTrack]].EndBeat) do Inc(CurrentNote[CurrentTrack]);

                Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 2;
                EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
                EditorLyrics[CurrentTrack].Selected := 0;

                Text[TextDebug].Text := Format(Language.Translate('EDIT_INFO_PREVIEW_INFO'), [CurrentSong.PreviewStart]);
              end;
            end;
          end
          else Text[TextDebug].Text := 'No preview start';
          Exit;
        end;

      // set Medley tags
      SDLK_A:
        begin
          CopyToUndo;
          if CurrentSong.Relative then
          begin
            ScreenPopupError.ShowPopup(Language.Translate('EDIT_POPUP_MEDLEY_RELATIVE_UNSUPPORTED'));
            Exit;
          end;

          if CurrentSong.isDuet then
          begin
            ScreenPopupError.ShowPopup(Language.Translate('EDIT_POPUP_MEDLEY_DUET_UNSUPPORTED'));
            Exit;
          end;

          MedleyNotes.isCustom := true;
          if SDL_ModState = KMOD_LSHIFT then //Medley End Note
          begin
            if MedleyNotes.isEnd then
            begin
              if (Lines[CurrentTrack].Current = MedleyNotes.end_.line) and (CurrentNote[CurrentTrack] = MedleyNotes.end_.note) then
              begin
                MedleyNotes.isEnd := false;
                Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].IsMedley := false;
                Text[TextDebug].Text := Language.Translate('EDIT_INFO_MEDLEY_END_CLEARED');
              end else
              begin
                Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].IsMedley := true;
                if (Length(Lines[CurrentTrack].Line) > MedleyNotes.end_.line) and
                  (Length(Lines[CurrentTrack].Line[MedleyNotes.end_.line].Note) > MedleyNotes.end_.note) then
                  Lines[CurrentTrack].Line[MedleyNotes.end_.line].Note[MedleyNotes.end_.note].IsMedley := false;
                MedleyNotes.end_.line := Lines[CurrentTrack].Current;
                MedleyNotes.end_.note := CurrentNote[CurrentTrack];
                Text[TextDebug].Text := Language.Translate('EDIT_INFO_MEDLEY_END_SET');
              end;
            end else
            begin
              MedleyNotes.isEnd := true;
              Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].IsMedley := true;
              MedleyNotes.end_.line := Lines[CurrentTrack].Current;
              MedleyNotes.end_.note := CurrentNote[CurrentTrack];
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_MEDLEY_END_SET');
            end;
          end else
          begin        //Medley Start Note
            if MedleyNotes.isStart then
            begin
              if (Lines[CurrentTrack].Current = MedleyNotes.start.line) and (CurrentNote[CurrentTrack] = MedleyNotes.start.note) then
              begin
                MedleyNotes.isStart := false;
                Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].IsMedley := false;
                Text[TextDebug].Text := Language.Translate('EDIT_INFO_MEDLEY_START_CLEARED');
              end else
              begin
                Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].IsMedley := true;
                if (Length(Lines[CurrentTrack].Line) > MedleyNotes.start.line) and
                  (Length(Lines[CurrentTrack].Line[MedleyNotes.start.line].Note) > MedleyNotes.start.note) then
                  Lines[CurrentTrack].Line[MedleyNotes.start.line].Note[MedleyNotes.start.note].IsMedley := false;
                MedleyNotes.start.line := Lines[CurrentTrack].Current;
                MedleyNotes.start.note := CurrentNote[CurrentTrack];
                Text[TextDebug].Text := Language.Translate('EDIT_INFO_MEDLEY_START_SET');
              end;
            end else
            begin
              Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].IsMedley := true;
              MedleyNotes.isStart := true;
              MedleyNotes.start.line := Lines[CurrentTrack].Current;
              MedleyNotes.start.note := CurrentNote[CurrentTrack];
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_MEDLEY_START_SET');
            end;
          end;

          UpdateMedleyInfo;
          Exit;
        end;

      // jump to Medley tags
      SDLK_J:
        begin
          if CurrentSong.Relative then
          begin
            ScreenPopupError.ShowPopup(Language.Translate('EDIT_POPUP_MEDLEY_RELATIVE_UNSUPPORTED'));
            Exit;
          end;

          if CurrentSong.isDuet then
          begin
            ScreenPopupError.ShowPopup(Language.Translate('EDIT_POPUP_MEDLEY_DUET_UNSUPPORTED'));
            Exit;
          end;

          if not MedleyNotes.IsEnd and not MedleyNotes.IsStart then
          begin
            ScreenPopupError.ShowPopup(Language.Translate('EDIT_POPUP_NO_MEDLEY_SECTION'));
            Exit;
          end;

          if (SDL_ModState = KMOD_LSHIFT) and MedleyNotes.IsEnd then //Medley End Note
          begin
            // simulate sentence switch to clear props
            PreviousSentence;

            if (Length(Lines[CurrentTrack].Line) > MedleyNotes.end_.line) and
               (Length(Lines[CurrentTrack].Line[MedleyNotes.end_.line].Note) > MedleyNotes.end_.note) then
            begin
              Lines[CurrentTrack].Current := MedleyNotes.end_.line;
              CurrentNote[CurrentTrack] := MedleyNotes.end_.note;
              Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 2;

              EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
              EditorLyrics[CurrentTrack].Selected := 0;
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_JUMPTO_MEDLEY_END');
            end;
          end else if MedleyNotes.IsStart then
          begin
            // simulate sentence switch to clear props
            PreviousSentence;

            if (Length(Lines[CurrentTrack].Line)> MedleyNotes.start.line) and
               (Length(Lines[CurrentTrack].Line[MedleyNotes.start.line].Note) > MedleyNotes.start.note) then
            begin
              Lines[CurrentTrack].Current := MedleyNotes.start.line;
              CurrentNote[CurrentTrack] := MedleyNotes.start.note;
              Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 2;

              EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
              EditorLyrics[CurrentTrack].Selected := 0;
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_JUMPTO_MEDLEY_START');
            end;
          end;

          if (SDL_ModState = KMOD_LALT) then
          begin
            // simulate sentence switch to clear props
            PreviousSentence;

            if (MedleyNotes.isStart and MedleyNotes.isEnd) and
              (MedleyNotes.start.line < MedleyNotes.end_.line) and
              (Length(Lines[CurrentTrack].Line)> MedleyNotes.end_.line) and
              (Length(Lines[CurrentTrack].Line[MedleyNotes.end_.line].Note)>MedleyNotes.end_.note) and
              (Length(Lines[CurrentTrack].Line[MedleyNotes.start.line].Note)>MedleyNotes.start.note) then
            begin
              R := GetTimeFromBeat(Lines[CurrentTrack].Line[MedleyNotes.start.line].Note[MedleyNotes.start.note].StartBeat);
              if InRange(R, 0.0, AudioPlayback.Length) then
              begin
                AudioPlayback.Position:= R;
                PlayStopTime := GetTimeFromBeat(
                  Lines[CurrentTrack].Line[MedleyNotes.end_.line].Note[MedleyNotes.end_.note].StartBeat +
                  Lines[CurrentTrack].Line[MedleyNotes.end_.line].Note[MedleyNotes.end_.note].Duration);
                PlaySentence := true;
                Click := false;
                AudioPlayback.Play;

                // play video in sync if visible
                if (fCurrentVideo <> nil) then UpdateVideoPosition(AudioPlayback.Position);
                Text[TextDebug].Text := Language.Translate('EDIT_INFO_JUMPTO_MEDLEY_AND_PLAY');
              end;
            end;
          end;

          UpdateMedleyInfo;
          Exit;
        end;

      SDLK_R:   //reload
        begin
          AudioPlayback.Stop;
          {$IFDEF UseMIDIPort}
          MidiOut.Close;
          MidiOut.Free;
          {$ENDIF}

          OnShow;
          Text[TextDebug].Text := Language.Translate('EDIT_INFO_SONG_RELOADED');
        end;

      SDLK_D:
        begin
          // Divide lengths by 2
          if (SDL_ModState = KMOD_LSHIFT) then
          begin
            CopyToUndo;
            DivideBPM;
            ShowInteractiveBackground;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_DIVIDED_BPM');
            Exit;
          end;

          // create duet or convert duet to normal song
          if (SDL_ModState = KMOD_LCTRL or KMOD_LSHIFT) then
          begin
            if (CurrentSong.isDuet) then
            begin
              MakeSolo;
              FixTimings;
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_CONVERTED_TO_SOLO');
            end
            else
            begin
              MakeDuet;
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_CONVERTED_TO_DUET');
            end;
          end;
        end;

      SDLK_M:
        begin
          // Multiply lengths by 2
          if (SDL_ModState = KMOD_LSHIFT) then
          begin
            CopyToUndo;
            MultiplyBPM;
            ShowInteractiveBackground;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_MULTIPLIED_BPM');
            Exit;
          end;
        end;

      SDLK_C:
        begin
          // Capitalize letter at the beginning of line
          if SDL_ModState = 0 then
            begin
            CopyToUndo;
            LyricsCapitalize;
            EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
            EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_CAPITALIZATION_CORRECTED');
            end;

          // Correct spaces
          if SDL_ModState = KMOD_LSHIFT then
            begin
              CopyToUndo;
              LyricsCorrectSpaces;
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_SPACES_CORRECTED');
            end;

          // Copy sentence
          if SDL_ModState = KMOD_LCTRL then
          begin
            MarkSrc;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_SENTENCE_COPIED');
          end;

          Exit;
        end;

      SDLK_V:
        begin
          if (SDL_ModState = 0) or (SDL_ModState = KMOD_LALT) then
          begin
            StopVideoPreview;
            AudioPlayback.Stop;
            PlayVideo := true;
            PlaySentenceMidi := false;
            StopVideoPreview();
            Click := true;
            with Lines[CurrentTrack].Line[Lines[CurrentTrack].Current] do
            begin
              Note[CurrentNote[CurrentTrack]].Color := 1;
              CurrentNote[CurrentTrack] := 0;
              AudioPlayback.Position := GetTimeFromBeat(Note[0].StartBeat);
              PlayStopTime := ifthen(SDL_ModState = KMOD_LALT,
                                   GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].High].EndBeat),
                                   GetTimeFromBeat(Note[High(Note)].EndBeat));
            end;
            if (SDL_ModState = KMOD_LALT) then
            begin
              PlaySentenceMidi := true;
              {$IFDEF UseMIDIPort} MidiTime  := USTime.GetTime;
              MidiStart := AudioPlayback.Position;
              MidiStop  := PlayStopTime; {$ENDIF}
            end;
            PlaySentence := true;
            AudioPlayback.Play;
            LastClick := -100;
            StartVideoPreview();
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_PLAY_SONG');
          end;
          // Paste text
          if SDL_ModState = KMOD_LCTRL then
          begin
            if Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].HighNote >= Lines[CurrentTrack].Line[CopySrc].HighNote then
            begin
              PasteText;
              EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_PASTE_TEXT');
            end
            else
              Log.LogStatus('PasteText: invalid range', 'TScreenEditSub.ParseInput');
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then
          begin
            CopyToUndo;
            CopySentence(CopySrc, Lines[CurrentTrack].Current);
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_PASTE_SENTENCE');
          end;
          GoldenRec.KillAll;
          ShowInteractiveBackground;
        end;

      SDLK_T:
        begin
          // Fixes timings between sentences
          CopyToUndo;
          FixTimings;
          Text[TextDebug].Text := Language.Translate('EDIT_INFO_FIX_TIMINGS');
          Exit;
        end;

      SDLK_P:
        begin
          if SDL_ModState = 0 then
          begin
            // Play Sentence
            Click := true;
            AudioPlayback.Stop;
            PlayVideo := false;
            StopVideoPreview;
            Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 1;
            CurrentNote[CurrentTrack] := 0;
            R := GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[0].StartBeat);
            if R <= AudioPlayback.Length then
            begin
              AudioPlayback.Position := R;
              PlayStopTime := GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat);
              PlaySentence := true;
              AudioPlayback.Play;
              LastClick := -100;
            end;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE_AUDIO');
          end
          else if SDL_ModState = KMOD_LSHIFT then
          begin
            Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 1;
            CurrentNote[CurrentTrack] := 0;
            PlaySentenceMidi := true;
            PlayVideo := false;
            StopVideoPreview;
            {$IFDEF UseMIDIPort} MidiTime := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[0].StartBeat);
            MidiStop := GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat); {$ENDIF}

            LastClick := -100;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE_MIDI');
          end
          else if SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL then
          begin
            Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 1;
            CurrentNote[CurrentTrack] := 0;
            PlaySentenceMidi := true;
            PlayVideo := false;
            StopVideoPreview;
            {$IFDEF UseMIDIPort} MidiTime  := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[0].StartBeat);
            MidiStop  := GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat); {$ENDIF}
            
            LastClick := -100;

            PlaySentence := true;
            Click := true;
            AudioPlayback.Stop;
            AudioPlayback.Position := GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[0].StartBeat)+0{-0.10};
            PlayStopTime := GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat)+0;
            AudioPlayback.Play;
            LastClick := -100;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE_AUDIO_AND_MIDI');
          end;
          Exit;
        end;

      // Golden Note
      SDLK_G:
        begin
          CopyToUndo;
          if (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].NoteType = ntGolden) then
          begin
            Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].NoteType := ntRapGolden;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_RAPGOLDEN');
          end
          else if (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].NoteType = ntRapGolden) then
          begin
            Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].NoteType := ntNormal;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_NORMAL');
          end
          else
          begin
            Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].NoteType := ntGolden;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_GOLDEN');
          end;
          GoldenRec.KillAll;
          Exit;
        end;

      // Freestyle Note
      SDLK_F:
        begin
          CopyToUndo;
          if (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].NoteType = ntFreestyle) then
          begin
            Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].NoteType := ntRap;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_RAP');
          end
          else if (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].NoteType = ntRap) then
          begin
            Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].NoteType := ntNormal;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_NORMAL');
          end
          else
          begin
            Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].NoteType := ntFreestyle;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_FREESTYLE');
          end;
          GoldenRec.KillAll;

          // update lyrics
          EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
          EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
          Exit;
        end;

      // undo
      SDLK_Z:
        begin
          if SDL_ModState = KMOD_LCTRL then
          begin
              CopyFromUndo;
              GoldenRec.KillAll;
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_UNDO');
          end;
          ShowInteractiveBackground;
        end;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE :
//      SDLK_BACKSPACE : // disable to leave editor by backspace key
        begin
          if length(UndoLines) > 0 then
            ScreenPopupcheck.ShowPopup(Language.Translate('EDIT_INFO_EXIT'), OnExit, nil, false)
          else
          begin
            FadeTo(@ScreenSong);
          end;
        end;

      SDLK_TAB:
      begin
        ScreenPopupHelp.ShowPopup();
      end;

      SDLK_BACKQUOTE:
        begin
          // Increase Note Length (same as Alt + Right)
          CopyToUndo;
          Inc(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration);
          if CurrentNote[CurrentTrack] = Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].HighNote then
          begin
            Inc(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat);
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_LENGTH_INCREASED');
          end;
          GoldenRec.KillAll;
          ShowInteractiveBackground;
        end;

      SDLK_EQUALS, // for keyboards which produce the EQUALS symbol without SHIFT modifier
      SDLK_PLUS: // for keyboards which produce the PLUS symbol without SHIFT modifier
        begin
          // Increase BPM
          CopyToUndo;
          if SDL_ModState = 0 then
          begin
            CurrentSong.BPM[0].BPM := Round((CurrentSong.BPM[0].BPM * 5) + 1) / 5; // (1/20)
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_BPM_INCREASED_BY') + ' 0.05';
          end;
          if SDL_ModState = KMOD_LSHIFT then
          begin
            CurrentSong.BPM[0].BPM := CurrentSong.BPM[0].BPM + 4; // (1/1)
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_BPM_INCREASED_BY') + ' 1.0';
          end;
          if SDL_ModState = KMOD_LCTRL then
          begin
            CurrentSong.BPM[0].BPM := Round((CurrentSong.BPM[0].BPM * 25) + 1) / 25; // (1/100)
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_BPM_INCREASED_BY') + ' 0.01';
          end;
        end;

      SDLK_MINUS:
        begin
          // Decrease BPM
          CopyToUndo;
          if SDL_ModState = 0 then
          begin
            CurrentSong.BPM[0].BPM := Round((CurrentSong.BPM[0].BPM * 5) - 1) / 5;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_BPM_DECREASED_BY') + ' 0.05';
          end;
          if SDL_ModState = KMOD_LSHIFT then
          begin
            CurrentSong.BPM[0].BPM := CurrentSong.BPM[0].BPM - 4;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_BPM_DECREASED_BY') + ' 1.0';
          end;
          if SDL_ModState = KMOD_LCTRL then
          begin
            CurrentSong.BPM[0].BPM := Round((CurrentSong.BPM[0].BPM * 25) - 1) / 25;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_BPM_DECREASED_BY') + ' 0.01';
          end;
        end;

      SDLK_4:
        begin
          if (CurrentSong.isDuet) then
            Exit; // FIXME: implement for duets

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then
          begin
            CopyToUndo;
            CopySentence(CopySrc, Lines[CurrentTrack].Current);
            CopySentence(CopySrc+1, Lines[CurrentTrack].Current+1);
            CopySentence(CopySrc+2, Lines[CurrentTrack].Current+2);
            CopySentence(CopySrc+3, Lines[CurrentTrack].Current+3);
            GoldenRec.KillAll;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_PASTE_4_SENTENCES');
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then
          begin
            CopyToUndo;
            CopySentences(CopySrc, Lines[CurrentTrack].Current, 4);
            GoldenRec.KillAll;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_COPY_4_SENTENCES');
          end;
        ShowInteractiveBackground;
        end;

      SDLK_5:
        begin
          if (CurrentSong.isDuet) then
            Exit; // FIXME: implement for duets

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then
          begin
            CopyToUndo;
            CopySentence(CopySrc, Lines[CurrentTrack].Current);
            CopySentence(CopySrc+1, Lines[CurrentTrack].Current+1);
            CopySentence(CopySrc+2, Lines[CurrentTrack].Current+2);
            CopySentence(CopySrc+3, Lines[CurrentTrack].Current+3);
            CopySentence(CopySrc+4, Lines[CurrentTrack].Current+4);
            GoldenRec.KillAll;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_PASTE_5_SENTENCES');
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then
          begin
            CopyToUndo;
            CopySentences(CopySrc, Lines[CurrentTrack].Current, 5);
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_COPY_5_SENTENCES');
            GoldenRec.KillAll;
          end;
        ShowInteractiveBackground;
        end;

      SDLK_7:
        begin
          // Decrease VideoGap
          CopyToUndo;
          if SDL_ModState = 0 then
          begin
            CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) - 1) / 100;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_VIDEOGAP_DECREASED_BY') + ' 0.01';
          end;
          if SDL_ModState = KMOD_LSHIFT then
          begin
            CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) - 10) / 100;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_VIDEOGAP_DECREASED_BY') + ' 0.1';
          end;
          if SDL_ModState = KMOD_LCTRL then
          begin
            CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) - 100) / 100;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_VIDEOGAP_DECREASED_BY') + ' 1.0';
          end;
        end;

      SDLK_8:
        begin
          // Increase VideoGap
          CopyToUndo;
          if SDL_ModState = 0 then
          begin
            CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) + 1) / 100;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_VIDEOGAP_INCREASED_BY') + ' 0.01 s';
          end;
          if SDL_ModState = KMOD_LSHIFT then
          begin
            CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) + 10) / 100;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_VIDEOGAP_INCREASED_BY') + ' 0.1 s';
          end;
          if SDL_ModState = KMOD_LCTRL then
          begin
            CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) + 100) / 100;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_VIDEOGAP_INCREASED_BY') + ' 1.0 s';
          end;
        end;

      SDLK_9:
        begin
          // Decrease GAP
          CopyToUndo;
          if SDL_ModState = 0 then
          begin
            CurrentSong.GAP := CurrentSong.GAP - 10;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_GAP_DECREASED_BY') + ' 10 ms';
          end;
          if SDL_ModState = KMOD_LSHIFT then
          begin
            CurrentSong.GAP := CurrentSong.GAP - 1000;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_GAP_DECREASED_BY') + ' 1000 ms';
          end;
        end;
      SDLK_0:
        begin
          // Increase GAP
          CopyToUndo;
          if SDL_ModState = 0 then
          begin
            CurrentSong.GAP := CurrentSong.GAP + 10;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_GAP_INCREASED_BY') + ' 10 ms';
          end;
          if SDL_ModState = KMOD_LSHIFT then
          begin
            CurrentSong.GAP := CurrentSong.GAP + 1000;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_GAP_INCREASED_BY') + ' 1000 ms';
          end;
        end;

      SDLK_KP_PLUS:
        begin
          // Increase tone of all notes
          CopyToUndo;
          if SDL_ModState = 0 then
          begin
            ChangeWholeTone(1);
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_ALL_TONES_INCREASED_BY_SEMITONE');
          end;
          if SDL_ModState = KMOD_LSHIFT then
          begin
            ChangeWholeTone(12);
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_ALL_TONES_INCREASED_BY_OCTAVE');
          end;
          GoldenRec.KillAll;
          ShowInteractiveBackground;
        end;

      SDLK_KP_MINUS:
        begin
          // Decrease tone of all notes
          CopyToUndo;
          if SDL_ModState = 0 then
          begin
            ChangeWholeTone(-1);
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_ALL_TONES_DECREASED_BY_SEMITONE');
          end;
          if SDL_ModState = KMOD_LSHIFT then
          begin
            ChangeWholeTone(-12);
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_ALL_TONES_DECREASED_BY_OCTAVE');
          end;
          GoldenRec.KillAll;
          ShowInteractiveBackground;
        end;

      SDLK_SLASH, SDLK_HASH:
        begin
          CopyToUndo;
          if SDL_ModState = 0 then
          begin
            // Start a new sentence with currently selected note
            if CurrentNote[CurrentTrack] > 0 then
            begin
              DivideSentence;
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_LINE_DIVIDED');
            end;
            GoldenRec.KillAll;
          end;

          if SDL_ModState = KMOD_LSHIFT then
          begin
            // Join current with subsequent sentence
            if Lines[CurrentTrack].Current < Lines[CurrentTrack].High then
            begin
              JoinSentence;
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_LINES_JOINED');
            end;
            GoldenRec.KillAll;
          end;

          if SDL_ModState = KMOD_LCTRL then
          begin
            // divide note
            DivideNote(false);
            EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
            EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_DIVIDED');
            GoldenRec.KillAll;
          end;
        ShowInteractiveBackground;
        end;

      SDLK_F4:
        begin
          // Enter Text Edit Mode
          BackupEditText := Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Text;
          CurrentEditText := BackupEditText;
          CurrentSlideId := LyricSlideId;
          TextPosition := LengthUTF8(BackupEditText);
          editLengthText := LengthUTF8(BackupEditText);
          TextEditMode := true;
        end;

      SDLK_SPACE:
        begin
          if (SDL_ModState = 0) or (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) then
          begin
            // Play current note
            PlaySentenceMidi := false; // stop midi
            PlaySentence := false;
            midinotefound := false;
            PlayOne := true;
            PlayOneMidi := false;
            PlayVideo := false;
            StopVideoPreview;
            Click := false;
            AudioPlayback.Stop;
            AudioPlayback.Position := GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat);
            PlayStopTime := (GetTimeFromBeat(
              Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat +
              Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration));
            AudioPlayback.Play;
            LastClick := -100;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_PLAY_NOTE_AUDIO');
          end;

          if (SDL_ModState = KMOD_LSHIFT) or (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) then
          begin
            // Play Midi
            PlaySentenceMidi := false;
            PlayVideo := false;
            midinotefound := false;
            PlayOne := true;
            PlayOneMidi := true;
            StopVideoPreview();
            {$IFDEF UseMIDIPort} MidiTime := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat);
            MidiStop := GetTimeFromBeat(
              Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat +
              Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration); {$ENDIF}
            LastClick := -100;
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_PLAY_NOTE_MIDI');
          end;
        end;

      SDLK_RETURN:
        begin
           if Interaction =  TitleSlideId then
           begin
             BackupEditText := CurrentSong.Title;
             CurrentEditText := BackupEditText;
             editLengthText := LengthUTF8(BackupEditText);
             CurrentSlideId := TitleSlideId;
             TextPosition := LengthUTF8(BackupEditText);
             TitleEditMode := true;
           end;

           if Interaction = ArtistSlideId then
           begin
             BackupEditText := CurrentSong.Artist;
             CurrentEditText := BackupEditText;
             editLengthText := LengthUTF8(BackupEditText);
             CurrentSlideId := ArtistSlideId;
             TextPosition := LengthUTF8(BackupEditText);
             ArtistEditMode := true;
           end;

           if Interaction = LanguageSlideId then
           begin
             BackupEditText := ifthen(CurrentSong.Language <> 'Unknown', CurrentSong.Language, NOT_SET);
             CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
             editLengthText := LengthUTF8(BackupEditText);
             CurrentSlideId := LanguageSlideId;
             TextPosition := LengthUTF8(BackupEditText);
             LanguageEditMode := true;
           end;

           if Interaction = EditionSlideId then
           begin
             BackupEditText := ifthen(CurrentSong.Edition <> 'Unknown', CurrentSong.Edition, NOT_SET);
             CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
             editLengthText := LengthUTF8(BackupEditText);
             CurrentSlideId := EditionSlideId;
             TextPosition := LengthUTF8(BackupEditText);
             EditionEditMode := true;
           end;

           if Interaction = GenreSlideId then
           begin
             BackupEditText := ifthen(CurrentSong.Genre <> 'Unknown', CurrentSong.Genre, NOT_SET);
             CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
             editLengthText := LengthUTF8(BackupEditText);
             CurrentSlideId := GenreSlideId;
             TextPosition := LengthUTF8(BackupEditText);
             GenreEditMode := true;
           end;

           if Interaction = YearSlideId then
           begin
             BackupEditText := ifthen(CurrentSong.Year <> 0, IntToStr(CurrentSong.Year), NOT_SET);
             CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
             editLengthText := LengthUTF8(BackupEditText);
             CurrentSlideId := YearSlideId;
             TextPosition := LengthUTF8(BackupEditText);
             YearEditMode := true;
           end;

           if Interaction = CreatorSlideId then
           begin
             BackupEditText := ifthen(CurrentSong.Creator <> '', CurrentSong.Creator, NOT_SET);
             CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
             editLengthText := LengthUTF8(BackupEditText);
             CurrentSlideId := CreatorSlideId;
             TextPosition := LengthUTF8(BackupEditText);
             CreatorEditMode := true;
           end;

           // Interaction = 7 // Mp3SlideId
           // Interaction = 8 // CoverSlideId
           // Interaction = 9 // BackgroundSlideId
           // Interaction = 10 // VideoSlideId
           // Interaction = 11 // VideoGapSlideId
           // Interaction = 12 // BPMSlideId
           // Interaction = 13 // GAPSlideId
           // Interaction = 14 // StartTagSlideId
           // Interaction = 15 // EndTagSlideId
           // Interaction = 16 // MedleyStartSlideId
           // Interaction = 17 // MedleyEndSlideId
           // Interaction = 18 // PreviewSlideId
           // Interaction = 19 // RelativeSlideId
           // Interaction = 20 // StartSlideId
           // Interaction = 21 // DurationSlideId
           // Interaction = 22 // ToneSlideId

           if Interaction = LyricSlideId then
           begin
             BackupEditText := Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Text;
             CurrentEditText := BackupEditText;
             editLengthText := LengthUTF8(BackupEditText);
             CurrentSlideId := LyricSlideId;
             TextPosition := LengthUTF8(BackupEditText);
             TextEditMode := true;
           end;

           if Interaction = 24 then // UndoButtonId
           begin
             CopyFromUndo;
             GoldenRec.KillAll;
             Text[TextDebug].Text := Language.Translate('EDIT_INFO_UNDO');
             ShowInteractiveBackground;
           end;

           if Interaction = 25 then // PreviousSeqButtonID
           begin
             PreviousSentence;
           end;

           if Interaction = 26 then // NextSeqButtonID
           begin
             NextSentence;
           end;

           if Interaction = 27 then // FreestyleButtonID
           begin
             CopyToUndo;
             if (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].NoteType = ntFreestyle) then
             begin
               Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].NoteType := ntRap;
             end
             else if (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].NoteType = ntRap) then
             begin
               Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].NoteType := ntNormal;
             end
             else
             begin
               Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].NoteType := ntFreestyle;
             end;
             GoldenRec.KillAll;

             // update lyrics
             EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
             EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
             Exit;
           end;

           if Interaction = 28 then // GoldButtonID
           begin
             CopyToUndo;
             if (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].NoteType = ntGolden) then
             begin
               Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].NoteType := ntRapGolden;
             end
             else if (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].NoteType = ntRapGolden) then
             begin
               Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].NoteType := ntNormal;
             end
             else
             begin
               Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].NoteType := ntGolden;
             end;
             GoldenRec.KillAll;
             Exit;
           end;

           if Interaction = 29 then // PlayOnlyButtonID
           begin
             // Play Sentence
             Click := true;
             AudioPlayback.Stop;
             PlayVideo := false;
             StopVideoPreview;
             Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 1;
             CurrentNote[CurrentTrack] := 0;
             R := GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[0].StartBeat);
             if R <= AudioPlayback.Length then
             begin
               AudioPlayback.Position := R;
               PlayStopTime := GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat);
               PlaySentence := true;
               AudioPlayback.Play;
               LastClick := -100;
             end;
             Text[TextDebug].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE');
           end;

           if Interaction = 30 then // PlayWithNoteButtonID
           begin
             Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 1;
             CurrentNote[CurrentTrack] := 0;
             PlaySentenceMidi := true;
             PlayVideo := false;
             StopVideoPreview;
             {$IFDEF UseMIDIPort} MidiTime  := USTime.GetTime;
             MidiStart := GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[0].StartBeat);
             MidiStop  := GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat); {$ENDIF}
             LastClick := -100;

             PlaySentence := true;
             Click := true;
             AudioPlayback.Stop;
             AudioPlayback.Position := GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[0].StartBeat)+0{-0.10};
             PlayStopTime := GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat)+0;
             AudioPlayback.Play;
             LastClick := -100;
             Text[TextDebug].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE');
           end;

           if Interaction = 31 then // PlayNoteButtonID
           begin
             Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 1;
             CurrentNote[CurrentTrack] := 0;
             PlaySentenceMidi := true;
             PlayVideo := false;
             StopVideoPreview;
             {$IFDEF UseMIDIPort} MidiTime := USTime.GetTime;
             MidiStart := GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[0].StartBeat);
             MidiStop := GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat); {$ENDIF}

             LastClick := -100;
             Text[TextDebug].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE');
           end;

           for i := 0 to Lines[CurrentTrack].High do
           begin
              if Interaction = InteractiveLineId[i] then
              begin
                CopyToUndo;
                GoldenRec.KillAll;
                Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 1;
                Lines[CurrentTrack].Current := i;
                ShowInteractiveBackground;
                CurrentNote[CurrentTrack] := 0;
                Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 2;
                EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
                EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
              end;
           end;

           if high(InteractiveNoteId) >= Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].HighNote then
           for i := 0 to Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].HighNote do
           begin
              if Interaction = InteractiveNoteId[i] then
              begin
                if (SDL_GetTicks() - LastClickTime < 250) and (SDL_ModState = 0) then
                begin
                   CopyToUndo;
                   GoldenRec.KillAll;
                   DivideNote(true);
                   ShowInteractiveBackground;
                end;

                // to check last click for divide note
                LastClickTime := SDL_GetTicks();

                Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 1;
                CurrentNote[CurrentTrack] := i;
                Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 2;
                EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
                //play current note playonewithmidi
                PlaySentenceMidi := false;
                midinotefound := false;
                PlayOne := true;
                PlayOneMidi := true;
                {$IFDEF UseMIDIPort} MidiTime := USTime.GetTime;
                MidiStart := GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat);
                MidiStop := GetTimeFromBeat(
                  Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat +
                  Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration); {$ENDIF}

                // playone
                PlayVideo := false;
                StopVideoPreview;
                Click := false;
                AudioPlayback.Stop;
                AudioPlayback.Position := GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat);
                PlayStopTime := (GetTimeFromBeat(
                Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat +
                Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration));
                AudioPlayback.Play;

                LastClick := -100;
              end;
           end;
        end;

      SDLK_DELETE:
        begin
          if SDL_ModState = KMOD_LCTRL then
          begin
            // deletes current note
            CopyToUndo;
            DeleteNote;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_DELETE_NOTE');
            GoldenRec.KillAll;
            ShowInteractiveBackground;
          end;
        end;

      SDLK_PERIOD:
        begin
          // moves text to right in current sentence
          CopyToUndo;
          MoveTextToRight;
          Text[TextDebug].Text := Language.Translate('EDIT_INFO_MOVE_TEXT_RIGHT');
        end;

      SDLK_RIGHT:
        begin
          // right
          CopyToUndo;
          if SDL_ModState = 0 then
          begin
            // clear debug text
            Text[TextDebug].Text := '';
            AudioPlayback.Stop;
            PlaySentence := false;
            PlayOne := false;
            PlayVideo := false;
            {$IFDEF UseMIDIPort}
            //MidiOut.PutShort($B1, $7, floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
            //MidiOut.PutShort($81, Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[MidiLastNote].Tone + 60, 127);
            PlaySentenceMidi := false;
            {$endif}
            Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 1;
            Inc(CurrentNote[CurrentTrack]);
            if CurrentNote[CurrentTrack] > Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].HighNote then
              CurrentNote[CurrentTrack] := 0;
            Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 2;
            EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
          end;

          // ctrl + right
          if SDL_ModState = KMOD_LCTRL then
          begin
            if Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration > 1 then
            begin
              Dec(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration);
              Inc(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat);
              if CurrentNote[CurrentTrack] = 0 then
              begin
                Inc(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].StartBeat);
              end;
            end;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_SHORTENED_AT_START');
            GoldenRec.KillAll;
          end;

          // shift + right
          if SDL_ModState = KMOD_LSHIFT then
          begin
            Inc(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat);
            if CurrentNote[CurrentTrack] = 0 then
            begin
              Inc(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].StartBeat);
            end;
            if CurrentNote[CurrentTrack] = Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].HighNote then
              Inc(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat);
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_SHIFTED_RIGHT');
            GoldenRec.KillAll;
          end;

          // alt + right
          if SDL_ModState = KMOD_LALT then
          begin
            Inc(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration);
            if CurrentNote[CurrentTrack] = Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].HighNote then
            begin
              Inc(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat);
            end;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_LENGTHENED_AT_END');
            GoldenRec.KillAll;
          end;

          // alt + ctrl + shift + right = move all from cursor to right
          if SDL_ModState = KMOD_LALT + KMOD_LCTRL + KMOD_LSHIFT then
          begin
            MoveAllToEnd(1);
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTES_SHIFTED_RIGHT');
            GoldenRec.KillAll;
          end;
        ShowInteractiveBackground;
        end;

      SDLK_LEFT:
        begin
          // left
          CopyToUndo;
          if SDL_ModState = 0 then
          begin
            // clear debug text
            Text[TextDebug].Text := '';
            AudioPlayback.Stop();
            PlaySentence := false;
            PlayOne := false;
            PlayVideo := false;
            {$IFDEF UseMIDIPort}
            //MidiOut.PutShort($B1, $7, floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
            //MidiOut.PutShort($81, Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[MidiLastNote].Tone + 60, 127);
            PlaySentenceMidi := false;
            {$endif}

            Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 1;
            Dec(CurrentNote[CurrentTrack]);
            if CurrentNote[CurrentTrack] = -1 then
              CurrentNote[CurrentTrack] := Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].HighNote;
            Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 2;
            EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
          end;

          // ctrl + left
          if SDL_ModState = KMOD_LCTRL then
          begin
            Dec(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat);
            Inc(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration);
            if CurrentNote[CurrentTrack] = 0 then
              Dec(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].StartBeat);
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_LENGTHENED_AT_START');
            GoldenRec.KillAll;
          end;

          // shift + left
          if SDL_ModState = KMOD_LSHIFT then
          begin
            Dec(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat);

            // resizing sentences
            if CurrentNote[CurrentTrack] = 0 then
            begin
              Dec(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].StartBeat);
            end;

            if CurrentNote[CurrentTrack] = Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].HighNote then
              Dec(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat);
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_SHIFTED_LEFT');
            GoldenRec.KillAll;
          end;

          // alt + left
          if SDL_ModState = KMOD_LALT then
          begin
            if Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration > 1 then
            begin
              Dec(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration);
              if CurrentNote[CurrentTrack] = Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].HighNote then
              begin
                Dec(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat);
              end;
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_SHORTENED_AT_END');
            end;
            GoldenRec.KillAll;
          end;

          // alt + ctrl + shift + right = move all from cursor to left
          if SDL_ModState = KMOD_LALT + KMOD_LCTRL + KMOD_LSHIFT then
          begin
            MoveAllToEnd(-1);
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTES_SHIFTED_LEFT');
            GoldenRec.KillAll;
          end;
        ShowInteractiveBackground;
        end;

      SDLK_DOWN:
        begin
          // skip to next sentence
          if SDL_ModState = 0 then
          begin
            // clear debug text
            Text[TextDebug].Text := '';
            NextSentence;
          end;

          // decrease tone
          if SDL_ModState = KMOD_LSHIFT then
          begin
            CopyToUndo;
            TransposeNote(-1);
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_PITCH_DECREASED');
            GoldenRec.KillAll;
          end;

          // switch to second track, if possible
          if (SDL_ModState = KMOD_LCTRL or KMOD_LALT) and (CurrentSong.isDuet) then
          begin
            if (Length(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note) > 0) then
            begin
              Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 0;
              CurrentTrack := 1;
              Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 2;
              EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
              EditorLyrics[CurrentTrack].Selected := 0;
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_SWITCHED_TO_TRACK') + ' 2';
            end;
          end;

          // copy line from first to second track
          if (CurrentSong.isDuet) and (CurrentTrack = 0) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) then
          begin
            //CopyToUndo; // FIXME: implement correct undo functionality also for duets
            if (DuetCopyLine) then
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_LINE_COPIED_TO_TRACK') + ' 2'
            else
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_LINE_NOT_COPIED');
          end;

          // move line from first to second track
          if (CurrentSong.isDuet) and (CurrentTrack = 0) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL or KMOD_LALT) then
          begin
            //CopyToUndo; FIXME: implement correct undo functionality also for duets
            if (DuetMoveLine) then
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_LINE_MOVED_TO_TRACK') + ' 2'
            else
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_LINE_NOT_MOVED');
          end;

        ShowInteractiveBackground;
        end;

      SDLK_UP:
        begin
          // skip to previous sentence
          if SDL_ModState = 0 then
          begin
            // clear debug text
            Text[TextDebug].Text := '';
            PreviousSentence;
          end;

          // increase tone
          if SDL_ModState = KMOD_LSHIFT then
          begin
            CopyToUndo;
            TransposeNote(1);
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_PITCH_INCREASED');
            GoldenRec.KillAll;
          end;

          // switch to first track, if possible
          if (SDL_ModState = KMOD_LCTRL or KMOD_LALT) and (CurrentSong.isDuet) then
          begin
            if (Length(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note) > 0) then
            begin
              Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 0;
              CurrentTrack := 0;
              Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 2;
              EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
              EditorLyrics[CurrentTrack].Selected := 0;
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_SWITCHED_TO_TRACK') + ' 1';
            end;
          end;

          // copy line from second to first track
          if (CurrentSong.isDuet) and (CurrentTrack = 1) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) then
          begin
            //CopyToUndo; //FIXME: implement correct undo functionality also for duets
            if (DuetCopyLine) then
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_LINE_COPIED_TO_TRACK') + ' 1'
            else
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_LINE_NOT_COPIED');
          end;

          // move line from second to first track
          if (CurrentSong.isDuet) and (CurrentTrack = 1) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL or KMOD_LALT) then
          begin
            //CopyToUndo; //FIXME: implement correct undo functionality also for duets
            if (DuetMoveLine) then
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_LINE_MOVED_TO_TRACK') + ' 1'
            else
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_LINE_NOT_MOVED');
          end;

        ShowInteractiveBackground;
        end;

      end; // case
    end;
  end; // if
end;

function TScreenEditSub.ParseInputEditText(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  SDL_ModState:  word;
begin
  // used when in Text Edit Mode
  Result := true;

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT {+ KMOD_CAPS});

  if (PressedDown) then
  begin
    // check normal keys
    if (IsPrintableChar(CharCode)) then
    begin
      CurrentEditText :=
      UTF8Copy(CurrentEditText, 1,TextPosition) + UCS4ToUTF8String(CharCode) +
      UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText));
      inc(editLengthText);
      inc(TextPosition);

      if TextEditMode then
        begin
        Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Text := CurrentEditText;
        EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
        EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
        end;
      Exit;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE:
        begin
          if TextEditMode then Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Text := BackupEditText;
          if TitleEditMode then
          begin
            CurrentSong.Title := BackupEditText;
            SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
          end;
          if ArtistEditMode then
          begin
            CurrentSong.Artist := BackupEditText;
            SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
          end;
          if LanguageEditMode then
          begin
            CurrentSong.Language := ifthen(BackupEditText <> NOT_SET, BackupEditText, 'Unknown');
            SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
          end;
          if EditionEditMode then
          begin
            CurrentSong.Edition := ifthen(BackupEditText <> NOT_SET, BackupEditText, 'Unknown');
            SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
          end;
          if GenreEditMode then
          begin
            CurrentSong.Genre := ifthen(BackupEditText <> NOT_SET, BackupEditText, 'Unknown');
            SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
          end;
          if YearEditMode then
          begin
            if (TryStrToInt(BackupEditText, CurrentSong.Year)) then
              SelectsS[CurrentSlideId].TextOpt[0].Text := IntToStr(CurrentSong.Year)
            else
              SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
          end;
          if CreatorEditMode then
          begin
            CurrentSong.Creator := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
            SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
          end;
          EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
          EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
          TextEditMode := false;
          TitleEditMode := false;
          ArtistEditMode := false;
          LanguageEditMode := false;
          EditionEditMode := false;
          GenreEditMode := false;
          YearEditMode := false;
          CreatorEditMode := false;
          editLengthText := 0;
          TextPosition := -1;
        end;
      SDLK_F4, SDLK_RETURN:
        begin
          // Exit Text Edit Mode
          CopyToUndo;
          if TitleEditMode then
          begin
            CurrentSong.Title := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            TitleVal[0] := CurrentSong.Title;
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
            UpdateSelectSlideOptions(Theme.EditSub.SlideTitle,TitleSlideId,TitleVal,SlideTitleIndex);
            SelectsS[TitleSlideId].TextOpt[0].Align := 0;
            SelectsS[TitleSlideId].TextOpt[0].X := SelectsS[TitleSlideId].TextureSBG.X + 5;
          end;
          if ArtistEditMode then
          begin
            CurrentSong.Artist := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            ArtistVal[0] := CurrentSong.Artist;
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
            UpdateSelectSlideOptions(Theme.EditSub.SlideArtist,ArtistSlideId,ArtistVal,SlideArtistIndex);
            SelectsS[ArtistSlideId].TextOpt[0].Align := 0;
            SelectsS[ArtistSlideId].TextOpt[0].X := SelectsS[ArtistSlideId].TextureSBG.X + 5;
          end;
          if LanguageEditMode then
          begin
            CurrentSong.Language := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            CurrentSong.Language := ifthen(CurrentSong.Language <> '', CurrentSong.Language, 'Unknown');
            LanguageVal[0] := ifthen(CurrentSong.Language <> 'Unknown', CurrentSong.Language, NOT_SET);
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
            UpdateSelectSlideOptions(Theme.EditSub.SlideLanguage,LanguageSlideId,LanguageVal,SlideLanguageIndex);
            SelectsS[LanguageSlideId].TextOpt[0].Align := 0;
            SelectsS[LanguageSlideId].TextOpt[0].X := SelectsS[LanguageSlideId].TextureSBG.X + 5;
          end;
          if EditionEditMode then
          begin
            CurrentSong.Edition := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            CurrentSong.Edition := ifthen(CurrentSong.Edition <> '', CurrentSong.Edition, 'Unknown');
            EditionVal[0] := ifthen(CurrentSong.Edition <> 'Unknown', CurrentSong.Edition, NOT_SET);
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
            UpdateSelectSlideOptions(Theme.EditSub.SlideEdition,EditionSlideId,EditionVal,SlideEditionIndex);
            SelectsS[EditionSlideId].TextOpt[0].Align := 0;
            SelectsS[EditionSlideId].TextOpt[0].X := SelectsS[EditionSlideId].TextureSBG.X + 5;
          end;
          if GenreEditMode then
          begin
            CurrentSong.Genre := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            CurrentSong.Genre := ifthen(CurrentSong.Genre <> '', CurrentSong.Genre, 'Unknown');
            GenreVal[0] := ifthen(CurrentSong.Genre <> 'Unknown', CurrentSong.Genre, NOT_SET);
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
            UpdateSelectSlideOptions(Theme.EditSub.SlideGenre,GenreSlideId,GenreVal,SlideGenreIndex);
            SelectsS[GenreSlideId].TextOpt[0].Align := 0;
            SelectsS[GenreSlideId].TextOpt[0].X := SelectsS[GenreSlideId].TextureSBG.X + 5;
          end;
          if YearEditMode then
          begin
            if (TryStrToInt(UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition), CurrentSong.Year)) and (CurrentSong.Year <= 2100) and (CurrentSong.Year >= 1900) then
            begin
              YearVal[0] := IntToStr(CurrentSong.Year);
              SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
              UpdateSelectSlideOptions(Theme.EditSub.SlideYear,YearSlideId,YearVal,SlideYearIndex);
              SelectsS[YearSlideId].TextOpt[0].Align := 0;
              SelectsS[YearSlideId].TextOpt[0].X := SelectsS[YearSlideId].TextureSBG.X + 5;
            end
            else
            begin
              CurrentSong.Year := 0;
              SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
            end;
          end;
          if CreatorEditMode then
          begin
            CurrentSong.Creator := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            CreatorVal[0] := ifthen(CurrentSong.Creator <> '', CurrentSong.Creator, NOT_SET);
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
            UpdateSelectSlideOptions(Theme.EditSub.SlideCreator,CreatorSlideId,CreatorVal,SlideCreatorIndex);
            SelectsS[CreatorSlideId].TextOpt[0].Align := 0;
            SelectsS[CreatorSlideId].TextOpt[0].X := SelectsS[CreatorSlideId].TextureSBG.X + 5;
          end;
          if TextEditMode then
          begin
            Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Text := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            LyricVal[0] := Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Text;
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
            UpdateSelectSlideOptions(Theme.EditSub.SlideLyric,LyricSlideId,LyricVal,SlideLyricIndex);
            SelectsS[LyricSlideId].TextOpt[0].Align := 0;
            SelectsS[LyricSlideId].TextOpt[0].X := SelectsS[LyricSlideId].TextureSBG.X + 5;
            EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
          end;
          EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
          TitleEditMode := false;
          ArtistEditMode := false;
          LanguageEditMode := false;
          EditionEditMode := false;
          GenreEditMode := false;
          YearEditMode := false;
          CreatorEditMode := false;
          TextEditMode := false;
          editLengthText := 0;
          TextPosition := -1;
          CurrentSlideId := -1;
        end;
      SDLK_BACKSPACE:
        begin
          if (TextPosition > 0) then
          begin
            UTF8Delete(CurrentEditText, TextPosition, 1);
            dec(TextPosition);
            if TextEditMode then
            begin
              Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Text := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
              EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
              EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
            end;
          end;
        end;
      SDLK_DELETE:
        begin
            UTF8Delete(CurrentEditText, TextPosition+1, 1);
        end;

      SDLK_RIGHT:
        begin
          // right
          if SDL_ModState = 0 then
          begin
            if (TextPosition >= 0) and (TextPosition < editLengthText-1) then
                TextPosition := TextPosition + 1
            else
              begin
              // todo change to next note
              TextPosition := 0;
              end;
          end;
        end;
      SDLK_LEFT:
        begin
          // left
          if SDL_ModState = 0 then
          begin
            if TextPosition > 0 then
                TextPosition := TextPosition - 1
            else
              begin
                // todo change to next note
                TextPosition := editLengthText-1;
              end;
          end;
        end;
      SDLK_SLASH:
        begin
          CopyToUndo;
          if SDL_ModState = KMOD_LCTRL then
          begin
            // divide note
            DivideNote(false);
            TextEditMode := false;
            EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
            EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
            GoldenRec.KillAll;
          end;
          ShowInteractiveBackground;
        end;

    end; //case
  end; //if (PressedDown)
end;

function TScreenEditSub.ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean;
var
  nBut: integer;
  Action: TMouseClickAction;
  tempR: real;
  i: integer;
begin
  // transfer mousecords to the 800x600 raster we use to draw
  X := Round((X / (ScreenW / Screens)) * RenderW);
  if (X > RenderW) then
    X := X - RenderW;
  Y := Round((Y / ScreenH) * RenderH);

  CurrentX := X;
  CurrentY := Y;

  Result := true;
  nBut := InteractAt(X, Y);
  Action := maNone;

  if nBut >= 0 then
  begin
    //select on mouse-over
    if nBut <> Interaction then
      SetInteraction(nBut);
  end;
  if Not BtnDown then
  begin
    PressedNoteId := -1;
    Xmouse := 0;
  end;

  if (nBut > -1) then
  begin
  if (BtnDown) then
  begin
    if (MouseButton = SDL_BUTTON_RIGHT) or (MouseButton = SDL_BUTTON_LEFT) then
    begin
      LastPressedMouseType := MouseButton;
      LastX := CurrentX;
      LastY := CurrentY;

      move_note := true;
      resize_note_left := false;
      resize_note_right := false;
      // check current mouse position to resize note - 20% of left or right note to resize
      if (Interactions[nBut].Typ = iButton) then
      begin
        if CurrentX < Button[Interactions[nBut].Num].X + Button[Interactions[nBut].Num].W*0.2 then
        begin
          // selected left side note - 20%
          resize_note_left := true;
          resize_note_right := false;
          move_note := false;
        end;

        if CurrentX > Button[Interactions[nBut].Num].X + Button[Interactions[nBut].Num].W - Button[Interactions[nBut].Num].W*0.2 then
        begin
          // selected right side note - 20%
          resize_note_left := false;
          resize_note_right := true;
          move_note := false;
        end;
        PressedNoteId :=  Interactions[nBut].Num;
      end;
    end;

    if (MouseButton = SDL_BUTTON_LEFT) then
    begin
      //click button or SelectS
      if (Interactions[nBut].Typ = iSelectS) then
      begin
        Action := SelectsS[Interactions[nBut].Num].OnClick(X, Y);
      end
      else
        Action := maReturn;
    end;

    // move notes by mouse move (left-right)
    tempR := 720 / (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat - Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[0].StartBeat);
    if (MouseButton = 0) and (LastPressedMouseType = SDL_BUTTON_RIGHT) and (PressedNoteId >=0) then
    begin
      // left & right
      if (Floor((CurrentX-40)/tempr) > Floor((LastX-40)/tempr)) or  (Floor((CurrentX-40)/tempr) < Floor((LastX-40)/tempr)) then
      begin
        CopyToUndo;
        i := floor((currentx-40) / floor(tempr)) - floor((lastx-40) / floor(tempr));
        if move_note then
          MoveAllToEnd(i);
        if (resize_note_right) and (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration + i > 0) then
        begin
          MoveAllToEnd(i);
          Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat := Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat - i;
          Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration := Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration + i;
        end;
        if (resize_note_left) and (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration - i > 0) then
        begin
          Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat := Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat + i;
          Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration := Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration - i;
          if CurrentNote[CurrentTrack] = 0 then
            Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].StartBeat := Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].StartBeat - i;
        end;
        LastX := CurrentX;
        GoldenRec.KillAll;
        ShowInteractiveBackground;
      end;
      // up & down
      if (CurrentY - LastY < -6) or (CurrentY - LastY > 6) then
      begin
        CopyToUndo;
        TransposeNote(-1* floor((CurrentY-LastY) div (6)));
        LastY := CurrentY;
        GoldenRec.KillAll;
        ShowInteractiveBackground;
      end;
    end;

    //move one note by mouse move
    if (MouseButton = 0) and (LastPressedMouseType = SDL_BUTTON_LEFT) and (PressedNoteId >=0) then
    begin
      if (Floor((CurrentX-40)/tempr) > Floor((LastX-40)/tempr)) or (Floor((CurrentX-40)/tempr) < Floor((LastX-40)/tempr)) then
      begin
        CopyToUndo;
        // move left & right
        i := floor((currentx-40) / floor(tempr)) - floor((lastx-40) / floor(tempr));
        if move_note then
        begin
            Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat := Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat +i;
            if CurrentNote[CurrentTrack] = 0 then
              Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].StartBeat := Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].StartBeat - i;
            if CurrentNote[CurrentTrack] = Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].HighNote then
              Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat := Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat + i;
        end;
        // resize note
        if (resize_note_right) and (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration + i > 0) then
        begin
          Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration := Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration + i;
          if CurrentNote[CurrentTrack] = Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].HighNote then
              Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat := Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat + i;
        end;
        if (resize_note_left) and (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration - i > 0) then
        begin
          Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat := Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat + i;
          Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration := Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration - i;
          if CurrentNote[CurrentTrack] = 0 then
              Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].StartBeat := Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].StartBeat + i;
        end;

        LastX := CurrentX;
        GoldenRec.KillAll;
        ShowInteractiveBackground;
      end;

      // up & down
      if (CurrentY - LastY < -6) or (CurrentY - LastY > 6) then
      begin
          CopyToUndo;
          TransposeNote(-1* floor((CurrentY-LastY) div (6)));
          LastY := CurrentY;
          GoldenRec.KillAll;
          ShowInteractiveBackground;
      end;
    end;

    // change to next sequence
    if (MouseButton = 0) and (LastPressedMouseType = SDL_BUTTON_LEFT) and (Interactions[nBut].Typ <> iButton) and (Interactions[nBut].Typ <> iText) and (Interactions[nBut].Typ <>  iSelectS) and (Interactions[nBut].Typ <> iBCollectionChild) and (PressedNoteId = -1) then
    begin
        if CurrentX - LastX > 120 then
        begin
            PreviousSentence;
            LastX := CurrentX;
            showInteractiveBackground;
        end;
        if CurrentX - LastX < -120 then
        begin
            NextSentence;
            LastX := CurrentX;
            showInteractiveBackground;
        end;
        if (CurrentX - LastX < 120) and (CurrentX - LastX > -120) then
            Xmouse := CurrentX - LastX;
    //          log.LogError('beside notes');
    end;
  end;

  // changed cover
  if ((CoverSlideId = Interactions[nBut].Num) and (Action = maLeft) and (SelectsS[Interactions[nBut].Num].SelectedOption > 0)) then
  begin
    CopyToUndo;
    SelectsS[Interactions[nBut].Num].SelectedOption := SelectsS[Interactions[nBut].Num].SelectedOption -1;
    CurrentSong.Cover := Path(SelectsS[Interactions[nBut].Num].TextOptT[SelectsS[Interactions[nBut].Num].SelectedOption]);
  end;

  if ((CoverSlideId = Interactions[nBut].Num) and (Action = maRight) and (SelectsS[Interactions[nBut].Num].SelectedOption < Length(SelectsS[Interactions[nBut].Num].TextOptT)-1)) then
  begin
    CopyToUndo;
    SelectsS[Interactions[nBut].Num].SelectedOption := SelectsS[Interactions[nBut].Num].SelectedOption +1;
    CurrentSong.Cover := Path(SelectsS[Interactions[nBut].Num].TextOptT[SelectsS[Interactions[nBut].Num].SelectedOption]);
  end;

  // changed background picture
  if ((BackgroundSlideId = Interactions[nBut].Num) and (Action = maLeft) and (SelectsS[Interactions[nBut].Num].SelectedOption > 0)) then
  begin
    CopyToUndo;
    SelectsS[Interactions[nBut].Num].SelectedOption := SelectsS[Interactions[nBut].Num].SelectedOption -1;
    CurrentSong.Background := Path(SelectsS[Interactions[nBut].Num].TextOptT[SelectsS[Interactions[nBut].Num].SelectedOption]);
    Tex_PrevBackground := Texture.LoadTexture(CurrentSong.Path.Append(CurrentSong.Background));
    Texture.AddTexture(Tex_PrevBackground, TEXTURE_TYPE_PLAIN, false);
    Statics[BackgroundImageId].Texture := Tex_PrevBackground;
    Statics[BackgroundImageId].Texture.X := theme.EditSub.BackgroundImage.X;
    Statics[BackgroundImageId].Texture.Y := theme.EditSub.BackgroundImage.Y;
    Statics[BackgroundImageId].Texture.W := theme.EditSub.BackgroundImage.W;
    Statics[BackgroundImageId].Texture.H := theme.EditSub.BackgroundImage.H;
  end;

  if ((BackgroundSlideId = Interactions[nBut].Num) and (Action = maRight) and (SelectsS[Interactions[nBut].Num].SelectedOption < Length(SelectsS[Interactions[nBut].Num].TextOptT)-1)) then
  begin
    CopyToUndo;
    SelectsS[Interactions[nBut].Num].SelectedOption := SelectsS[Interactions[nBut].Num].SelectedOption +1;
    CurrentSong.Background := Path(SelectsS[Interactions[nBut].Num].TextOptT[SelectsS[Interactions[nBut].Num].SelectedOption]);
    Tex_PrevBackground := Texture.LoadTexture(CurrentSong.Path.Append(CurrentSong.Background));
    Texture.AddTexture(Tex_PrevBackground, TEXTURE_TYPE_PLAIN, false);
    Statics[BackgroundImageId].Texture := Tex_PrevBackground;
    Statics[BackgroundImageId].Texture.X := theme.EditSub.BackgroundImage.X;
    Statics[BackgroundImageId].Texture.Y := theme.EditSub.BackgroundImage.Y;
    Statics[BackgroundImageId].Texture.W := theme.EditSub.BackgroundImage.W;
    Statics[BackgroundImageId].Texture.H := theme.EditSub.BackgroundImage.H;
  end;

  // changed video
  if ((VideoSlideId = Interactions[nBut].Num) and (Action = maLeft) and (SelectsS[Interactions[nBut].Num].SelectedOption > 0)) then
  begin
    CopyToUndo;
    SelectsS[Interactions[nBut].Num].SelectedOption := SelectsS[Interactions[nBut].Num].SelectedOption -1;
    CurrentSong.Video := Path(SelectsS[Interactions[nBut].Num].TextOptT[SelectsS[Interactions[nBut].Num].SelectedOption]);
  end;

  if ((VideoSlideId = Interactions[nBut].Num) and (Action = maRight) and (SelectsS[Interactions[nBut].Num].SelectedOption < Length(SelectsS[Interactions[nBut].Num].TextOptT)-1)) then
  begin
    CopyToUndo;
    SelectsS[Interactions[nBut].Num].SelectedOption := SelectsS[Interactions[nBut].Num].SelectedOption +1;
    CurrentSong.Video := Path(SelectsS[Interactions[nBut].Num].TextOptT[SelectsS[Interactions[nBut].Num].SelectedOption]);
  end;

  if ((Mp3SlideId = Interactions[nBut].Num) and (Action = maLeft) and (SelectsS[Interactions[nBut].Num].SelectedOption > 0)) then
  begin
    CopyToUndo;
    SelectsS[Interactions[nBut].Num].SelectedOption := SelectsS[Interactions[nBut].Num].SelectedOption -1;
    CurrentSong.Mp3 := Path(SelectsS[Interactions[nBut].Num].TextOptT[SelectsS[Interactions[nBut].Num].SelectedOption]);
    AudioPlayback.Close;
    AudioPlayback.Open(CurrentSong.Path.Append(CurrentSong.Mp3));
  end;

  if ((Mp3SlideId = Interactions[nBut].Num) and (Action = maRight) and (SelectsS[Interactions[nBut].Num].SelectedOption < Length(SelectsS[Interactions[nBut].Num].TextOptT)-1)) then
  begin
    CopyToUndo;
    SelectsS[Interactions[nBut].Num].SelectedOption := SelectsS[Interactions[nBut].Num].SelectedOption +1;
    CurrentSong.Mp3 := Path(SelectsS[Interactions[nBut].Num].TextOptT[SelectsS[Interactions[nBut].Num].SelectedOption]);
    AudioPlayback.Close();
    AudioPlayback.Open(CurrentSong.Path.Append(CurrentSong.Mp3));
  end;

  if (((VolumeAudioSlideId = Interactions[nBut].Num) or (VolumeMidiSlideId = Interactions[nBut].Num) or (VolumeClickSlideId = Interactions[nBut].Num))
    and (Action = maLeft) and (SelectsS[Interactions[nBut].Num].SelectedOption > 0)) then
  begin
    SelectsS[Interactions[nBut].Num].SelectedOption := SelectsS[Interactions[nBut].Num].SelectedOption -1;
  end;

  if (((VolumeAudioSlideId = Interactions[nBut].Num) or (VolumeMidiSlideId = Interactions[nBut].Num) or (VolumeClickSlideId = Interactions[nBut].Num))
    and (Action = maRight) and (SelectsS[Interactions[nBut].Num].SelectedOption < Length(SelectsS[Interactions[nBut].Num].TextOptT)-1)) then
  begin
    SelectsS[Interactions[nBut].Num].SelectedOption := SelectsS[Interactions[nBut].Num].SelectedOption +1;
  end;
  end
  else if (MouseButton = SDL_BUTTON_RIGHT) then
  begin
    if length(UndoLines) > 0 then
    begin
      ScreenPopupcheck.ShowPopup(Language.Translate('EDIT_INFO_EXIT'), OnExit, nil, false);
    end
    else
    begin
      FadeTo(@ScreenSong);
    end;
  end;

  case Action of
    maReturn: Result := ParseInput(SDLK_RETURN, 0, true);
    //maLeft:   Result := ParseInput(SDLK_LEFT, 0, true);
    //maRight:  Result := ParseInput(SDLK_RIGHT, 0, true);
  end;
end;

{
procedure TScreenEditSub.NewBeat;
begin
  // click
  for Pet := 0 to Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].HighNut do
    if (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[Pet].Start = Czas.CurrentBeat) then
      Music.PlayClick;
end;
}

procedure TScreenEditSub.DivideBPM;
var
  TrackIndex: Integer;
  LineIndex:  Integer;
  NoteIndex:  Integer;

begin
  CurrentSong.BPM[0].BPM := CurrentSong.BPM[0].BPM / 2;
  for TrackIndex := 0 to High(Lines) do
  begin
    for LineIndex := 0 to Lines[TrackIndex].High do
    begin
      Lines[TrackIndex].Line[LineIndex].StartBeat := Lines[TrackIndex].Line[LineIndex].StartBeat div 2;
      Lines[TrackIndex].Line[LineIndex].EndBeat  := Lines[TrackIndex].Line[LineIndex].EndBeat div 2;
      for NoteIndex := 0 to Lines[TrackIndex].Line[LineIndex].HighNote do
      begin
        Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].StartBeat  := Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].StartBeat div 2;
        Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Duration := Round(Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Duration / 2);
      end; // NoteIndex
    end; // LineIndex
  end; // TrackIndex
end;

procedure TScreenEditSub.MultiplyBPM;
var
  TrackIndex: Integer;
  LineIndex:  Integer;
  NoteIndex:  Integer;
begin
  CurrentSong.BPM[0].BPM := CurrentSong.BPM[0].BPM * 2;
  for TrackIndex := 0 to High(Lines) do
  begin
    for LineIndex := 0 to Lines[TrackIndex].High do
    begin
      Lines[TrackIndex].Line[LineIndex].StartBeat := Lines[TrackIndex].Line[LineIndex].StartBeat * 2;
      Lines[TrackIndex].Line[LineIndex].EndBeat  := Lines[TrackIndex].Line[LineIndex].EndBeat * 2;
      for NoteIndex := 0 to Lines[TrackIndex].Line[LineIndex].HighNote do
      begin
        Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].StartBeat  := Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].StartBeat * 2;
        Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Duration := Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Duration * 2;
      end; // NoteIndex
    end; // LineIndex
  end; // TrackIndex
end;

procedure TScreenEditSub.LyricsCapitalize;
var
  TrackIndex:   Integer;
  LineIndex:    Integer;
  //NoteIndex:    Integer; // temporary
  Str:          UTF8String;
begin
  // temporary
  {
  for LineIndex := 0 to Lines[TrackIndex].High do
    for NoteIndex := 0 to Lines[TrackIndex].Line[LineIndex].HighNote do
      Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Text := UTF8LowerCase(Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Text);
  }

  for TrackIndex := 0 to High(Lines) do
  begin
    for LineIndex := 0 to Lines[TrackIndex].High do
    begin
      Str := UTF8UpperCase(UTF8Copy(TrimLeft(Lines[TrackIndex].Line[LineIndex].Note[0].Text), 1, 1));
      Str := Str + UTF8Copy(TrimLeft(Lines[TrackIndex].Line[LineIndex].Note[0].Text), 2, Length(Lines[TrackIndex].Line[LineIndex].Note[0].Text)-1);
      Lines[TrackIndex].Line[LineIndex].Note[0].Text := Str;
    end; // LineIndex
  end; // TrackIndex
end;

procedure TScreenEditSub.LyricsCorrectSpaces;
var
  TrackIndex:   Integer;
  LineIndex:    Integer;
  NoteIndex:    Integer;
begin
  for TrackIndex := 0 to High(Lines) do
  begin
    for LineIndex := 0 to Lines[TrackIndex].High do
    begin
      // correct starting spaces in the first word
      while Copy(Lines[TrackIndex].Line[LineIndex].Note[0].Text, 1, 1) = ' ' do
        Lines[TrackIndex].Line[LineIndex].Note[0].Text := Copy(Lines[TrackIndex].Line[LineIndex].Note[0].Text, 2, 100);

      // move spaces on the start to the end of the previous note
      for NoteIndex := 1 to Lines[TrackIndex].Line[LineIndex].HighNote do
      begin
        while (Copy(Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Text, 1, 1) = ' ') do
        begin
          Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Text := Copy(Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Text, 2, 100);
          Lines[TrackIndex].Line[LineIndex].Note[NoteIndex-1].Text := Lines[TrackIndex].Line[LineIndex].Note[NoteIndex-1].Text + ' ';
        end;
      end; // NoteIndex

      // correct '-'  to '- '
      for NoteIndex := 0 to Lines[TrackIndex].Line[LineIndex].HighNote do
      begin
        if Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Text = '-' then
          Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Text := '- ';
      end; // NoteIndex

      // add space to the previous note when the current word is '- '
      for NoteIndex := 1 to Lines[TrackIndex].Line[LineIndex].HighNote do
      begin
        if Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Text  = '- ' then
          Lines[TrackIndex].Line[LineIndex].Note[NoteIndex-1].Text := Lines[TrackIndex].Line[LineIndex].Note[NoteIndex-1].Text + ' ';
      end; // NoteIndex

      // correct too many spaces at the end of note
      for NoteIndex := 0 to Lines[TrackIndex].Line[LineIndex].HighNote do
      begin
        while Copy(Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Text, Length(Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Text)-1, 2) = '  ' do
          Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Text := Copy(Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Text, 1, Length(Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Text)-1);
      end; // NoteIndex

      // and correct if there is no space at the end of sentence
      NoteIndex := Lines[TrackIndex].Line[LineIndex].HighNote;
      if Copy(Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Text, Length(Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Text), 1) <> ' ' then
        Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Text := Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Text + ' ';
    end; // LineIndex
  end; // TrackIndex
end;

procedure TScreenEditSub.FixTimings;
var
  TrackIndex: integer;
  LineIndex:  integer;
  NoteIndex:  integer;
  LineStart:  integer;
  Min:        integer;
  Max:        integer;
  FirstBeat:  integer;
begin
  FirstBeat := Lines[0].Line[0].Note[0].StartBeat;
  if (CurrentSong.isDuet) then
    for TrackIndex := 1 to High(Lines) do
      if (Lines[TrackIndex].Line[0].Note[0].StartBeat < FirstBeat) then
        FirstBeat := Lines[TrackIndex].Line[0].Note[0].StartBeat;

  // set first note to start at beat 0 (common practice)
  if (FirstBeat <> 0) then
  begin
    for TrackIndex := 0 to High(Lines) do
      for LineIndex := 0 to Lines[TrackIndex].High do
        for NoteIndex := 0 to Lines[TrackIndex].Line[LineIndex].HighNote do
          Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].StartBeat := Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].StartBeat - FirstBeat;

    // adjust GAP accordingly
    CurrentSong.GAP := round((CurrentSong.GAP + (FirstBeat * 15000) / CurrentSong.BPM[0].BPM) * 100) / 100;

    // adjust medley tags accordingly
    if (MedleyNotes.isStart) then
      CurrentSong.Medley.StartBeat := CurrentSong.Medley.StartBeat - FirstBeat;
    if (MedleyNotes.isEnd) then
      CurrentSong.Medley.EndBeat := CurrentSong.Medley.EndBeat - FirstBeat;
  end;

  // adjust line break timings
  for TrackIndex := 0 to High(Lines) do
  begin
    for LineIndex := 1 to Lines[TrackIndex].High do
    begin
      with Lines[TrackIndex].Line[LineIndex-1] do
      begin
        Min := Note[HighNote].StartBeat + Note[HighNote].Duration;
        Max := Lines[TrackIndex].Line[LineIndex].Note[0].StartBeat;
        case (Max - Min) of
          0:    LineStart := Max;
          1:    LineStart := Max;
          2:    LineStart := Max - 1;
          3:    LineStart := Max - 2;
          else
            if ((Max - Min) > 4) then
              LineStart := Min + 2
            else
              LineStart := Max;
        end; // case

        Lines[TrackIndex].Line[LineIndex].StartBeat := LineStart;
      end; // with
    end; // LineIndex
  end; // TrackIndex
end;

procedure TScreenEditSub.DivideSentence;
var
  LineIndex:     integer;
  LineStart:     integer;
  LineNew:       integer;
  LineLength:    integer;
  NoteIndex:     integer;
  NoteStart:     integer;
  NoteHigh:      integer;
begin
  // increase sentence length by 1
  LineLength := Length(Lines[CurrentTrack].Line);
  SetLength(Lines[CurrentTrack].Line, LineLength + 1);
  Inc(Lines[CurrentTrack].Number);
  Inc(Lines[CurrentTrack].High);

  // move needed sentences to one forward. newly has the copy of divided sentence
  LineStart := Lines[CurrentTrack].Current;
  for LineIndex := LineLength-1 downto LineStart do
    CopyLine(CurrentTrack, LineIndex, CurrentTrack, LineIndex+1);
    //Lines[CurrentTrack].Line[LineIndex+1] := Lines[CurrentTrack].Line[LineIndex];

  // clear and set new sentence
  LineNew := LineStart + 1;
  NoteStart := CurrentNote[CurrentTrack];
  Lines[CurrentTrack].Line[LineNew].StartBeat := Lines[CurrentTrack].Line[LineStart].Note[NoteStart].StartBeat;
  Lines[CurrentTrack].Line[LineNew].Lyric := '';
  Lines[CurrentTrack].Line[LineNew].EndBeat := 0;
  Lines[CurrentTrack].Line[LineNew].BaseNote := 0;//High(integer); // TODO: High (integer) will causes a memory exception later in this procedure. Weird!
  Lines[CurrentTrack].Line[LineNew].HighNote := -1;
  SetLength(Lines[CurrentTrack].Line[LineNew].Note, 0);

  // move right notes to new sentences
  NoteHigh := Lines[CurrentTrack].Line[LineStart].HighNote;
  for NoteIndex := NoteStart to NoteHigh do
  begin
    // increase sentence counters
    with Lines[CurrentTrack].Line[LineNew] do
    begin
      Inc(HighNote);
      SetLength(Note, HighNote + 1);
      Note[HighNote] := Lines[CurrentTrack].Line[LineStart].Note[NoteIndex];
      EndBeat := Note[HighNote].StartBeat + Note[HighNote].Duration;
      
      if Note[HighNote].Tone < BaseNote then
        BaseNote := Note[HighNote].Tone;
    end;
  end;

  // clear old notes and set sentence counters
  Lines[CurrentTrack].Line[LineStart].HighNote := NoteStart - 1;
  Lines[CurrentTrack].Line[LineStart].EndBeat := Lines[CurrentTrack].Line[LineStart].Note[NoteStart-1].StartBeat +
    Lines[CurrentTrack].Line[LineStart].Note[NoteStart-1].Duration;
  SetLength(Lines[CurrentTrack].Line[LineStart].Note, Lines[CurrentTrack].Line[LineStart].HighNote + 1);

  //recalculate BaseNote of the divided sentence
  with Lines[CurrentTrack].Line[LineStart] do
  begin
    BaseNote := High(integer);

    for NoteIndex := 0 to HighNote do
      if Note[NoteIndex].Tone < BaseNote then
        BaseNote := Note[NoteIndex].Tone;
  end;

  //cleanup of first note of new sentence: trim leading white space and capitalize
  Lines[CurrentTrack].Line[LineNew].Note[0].Text := TrimLeft(Lines[CurrentTrack].Line[LineNew].Note[0].Text);
  Lines[CurrentTrack].Line[LineNew].Note[0].Text := UTF8UpperCase(UTF8Copy(Lines[CurrentTrack].Line[LineNew].Note[0].Text, 1, 1)) + UTF8Copy(Lines[CurrentTrack].Line[LineNew].Note[0].Text, 2, Length(Lines[CurrentTrack].Line[LineNew].Note[0].Text) - 1);

  Lines[CurrentTrack].Current := Lines[CurrentTrack].Current + 1;
  CurrentNote[CurrentTrack] := 0;

  Refresh;
  Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 2;
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
  EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
end;

procedure TScreenEditSub.JoinSentence;
var
  LineIndex:    integer;
  NoteIndex:    integer;
  NStart:       integer;
  NDst:         integer;
begin
  LineIndex := Lines[CurrentTrack].Current;

  // add space to last note's syllable
  Lines[CurrentTrack].Line[LineIndex].Note[Lines[CurrentTrack].Line[LineIndex].HighNote].Text := Lines[CurrentTrack].Line[LineIndex].Note[Lines[CurrentTrack].Line[LineIndex].HighNote].Text + ' ';

  // set new sentence
  NStart := Lines[CurrentTrack].Line[LineIndex].HighNote + 1;
  Lines[CurrentTrack].Line[LineIndex].HighNote := Lines[CurrentTrack].Line[LineIndex].HighNote + Lines[CurrentTrack].Line[LineIndex+1].HighNote + 1;
  SetLength(Lines[CurrentTrack].Line[LineIndex].Note, Lines[CurrentTrack].Line[LineIndex].HighNote + 1);

  // move right notes to new sentences
  for NoteIndex := 0 to Lines[CurrentTrack].Line[LineIndex+1].HighNote do
  begin
    NDst := NStart + NoteIndex;
    Lines[CurrentTrack].Line[LineIndex].Note[NDst] := Lines[CurrentTrack].Line[LineIndex+1].Note[NoteIndex];
  end;

  // increase sentence counters
  NDst := Lines[CurrentTrack].Line[LineIndex].HighNote;
  Lines[CurrentTrack].Line[LineIndex].EndBeat := Lines[CurrentTrack].Line[LineIndex].Note[NDst].StartBeat +
    Lines[CurrentTrack].Line[LineIndex].Note[NDst].Duration;

  // move needed sentences to one backward.
  for LineIndex := Lines[CurrentTrack].Current + 1 to Lines[CurrentTrack].High - 1 do
    CopyLine(CurrentTrack, LineIndex+1, CurrentTrack, LineIndex);
    //Lines[CurrentTrack].Line[LineIndex] := Lines[CurrentTrack].Line[LineIndex+1];

  // increase sentence length by 1
  SetLength(Lines[CurrentTrack].Line, Length(Lines[CurrentTrack].Line) - 1);
  Dec(Lines[CurrentTrack].Number);
  Dec(Lines[CurrentTrack].High);

  Refresh;
  Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 2;
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
  EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
end;

procedure TScreenEditSub.NextSentence;
begin
  {$IFDEF UseMIDIPort}
  //MidiOut.PutShort($B1, $7, floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
  //MidiOut.PutShort(MIDI_NOTEOFF or 1, Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[MidiLastNote].Tone + 60, 127);
  PlaySentenceMidi := false;
  PlayOne := false;
  {$ENDIF}
  Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 1;
  Inc(Lines[CurrentTrack].Current);
  CurrentNote[CurrentTrack] := 0;
  if Lines[CurrentTrack].Current > Lines[CurrentTrack].High then
    Lines[CurrentTrack].Current := 0;
  Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 2;

  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
  EditorLyrics[CurrentTrack].Selected := 0;
  AudioPlayback.Stop();
  PlaySentence := false;
  PlayVideo := false;
  GoldenRec.KillAll;
end;

procedure TScreenEditSub.PreviousSentence;
begin
  AudioPlayback.Stop();
  PlayVideo := false;
  PlaySentence := false;
  PlayOne := false;
  {$IFDEF UseMIDIPort}
  //MidiOut.PutShort(MIDI_NOTEOFF or 1, $7, floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
  //MidiOut.PutShort($81, Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[MidiLastNote].Tone + 60, 127);
  PlaySentenceMidi := false;
  {$endif}

  Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 1;
  Dec(Lines[CurrentTrack].Current);
  CurrentNote[CurrentTrack] := 0;
  if Lines[CurrentTrack].Current = -1 then
    Lines[CurrentTrack].Current := Lines[CurrentTrack].High;
  Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 2;

  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
  EditorLyrics[CurrentTrack].Selected := 0;
  GoldenRec.KillAll;
end;

procedure TScreenEditSub.DivideNote(doubleclick: boolean);
var
  C:    integer;
  N:    integer;
  wherecutting, spacepos: integer;
  tempR:  real;
  tempstr : UCS4String;
begin
  C := Lines[CurrentTrack].Current;
  tempR := 720 / (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat - Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[0].StartBeat);

  if (doubleclick) and (InteractAt(currentX, CurrentY) > 0) then
      wherecutting := Round((currentX - button[Interactions[InteractAt(currentX, CurrentY)].Num].X) / tempR)
  else
      wherecutting := 1;

  with Lines[CurrentTrack].Line[C] do
  begin
    Inc(HighNote);
    SetLength(Note, HighNote + 1);

    // we copy all notes including selected one
    for N := HighNote downto CurrentNote[CurrentTrack]+1 do
    begin
      Note[N] := Note[N-1];
    end;

    // Note[Cur] and Note[Cur + 1] is identical at this point
    // modify first note
    Note[CurrentNote[CurrentTrack]].Duration := wherecutting;

    // 2nd note
    Note[CurrentNote[CurrentTrack]+1].StartBeat := Note[CurrentNote[CurrentTrack]].StartBeat + Note[CurrentNote[CurrentTrack]].Duration;
    Note[CurrentNote[CurrentTrack]+1].Duration := Note[CurrentNote[CurrentTrack]+1].Duration - Note[CurrentNote[CurrentTrack]].Duration;

    // find space in text
    spacepos := -1;
    for  N:=0 to LengthUTF8(Note[CurrentNote[CurrentTrack]].Text) do
    begin

      tempstr := UTF8ToUCS4String(Note[CurrentNote[CurrentTrack]].Text);
      if ((UCS4ToUTF8String(tempstr[N]) = ' ') and (spacepos < 0)) then
         spacepos := N;

    end;
    if ((TextPosition < 0) and (ansipos(' ', Note[CurrentNote[CurrentTrack]].Text) > 1) and (ansipos(' ', Note[CurrentNote[CurrentTrack]].Text) < Length(Note[CurrentNote[CurrentTrack]].Text)  )) then
    begin
        Note[CurrentNote[CurrentTrack]+1].Text := UTF8Copy(Note[CurrentNote[CurrentTrack]].Text,spacepos + 2,LengthUTF8(Note[CurrentNote[CurrentTrack]].Text));
        Note[CurrentNote[CurrentTrack]].Text := UTF8Copy(Note[CurrentNote[CurrentTrack]].Text, 1,spacepos+1)
    end
    else
    if ((TextPosition >= 0) and (TextPosition < Length(Note[CurrentNote[CurrentTrack]].Text))) then
    begin
        Note[CurrentNote[CurrentTrack]+1].Text := UTF8Copy(SelectsS[LyricSlideId].TextOpt[0].Text, TextPosition+2, LengthUTF8(SelectsS[LyricSlideId].TextOpt[0].Text));
        Note[CurrentNote[CurrentTrack]].Text := UTF8Copy(SelectsS[LyricSlideId].TextOpt[0].Text, 1, TextPosition);
        SelectsS[LyricSlideId].TextOpt[0].Text := Note[CurrentNote[CurrentTrack]].Text;
        TextPosition := -1;
    end
    else
        Note[CurrentNote[CurrentTrack]+1].Text := '~';
    Note[CurrentNote[CurrentTrack]+1].Color := 1;
  end;

  // update lyric display
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
  EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
end;

procedure TScreenEditSub.DeleteNote;
var
  C:    integer;
  N:    integer;
begin
  C := Lines[CurrentTrack].Current;

  //Do Not delete Last Note
  if (Lines[CurrentTrack].Line[C].HighNote > 0) then
  begin
    // we copy all notes from the next to the selected one
    for N := CurrentNote[CurrentTrack]+1 to Lines[CurrentTrack].Line[C].HighNote do
    begin
      Lines[CurrentTrack].Line[C].Note[N-1] := Lines[CurrentTrack].Line[C].Note[N];
    end;
    
    Dec(Lines[CurrentTrack].Line[C].HighNote);

    SetLength(Lines[CurrentTrack].Line[C].Note, Lines[CurrentTrack].Line[C].HighNote + 1);

    // last note was deleted
    if (CurrentNote[CurrentTrack] > Lines[CurrentTrack].Line[C].HighNote) then
    begin
      // select new last note
      CurrentNote[CurrentTrack] := Lines[CurrentTrack].Line[C].HighNote;

      // correct Line ending
      with Lines[CurrentTrack].Line[C] do
        EndBeat := Note[HighNote].StartBeat + Note[HighNote].Duration;
    end;

    Lines[CurrentTrack].Line[C].Note[CurrentNote[CurrentTrack]].Color := 2;
  end
  // Last Note of current Sentence Deleted - > Delete Sentence
  // if there are more than two left
  else if (Lines[CurrentTrack].High > 1) then
  begin
    //Move all Sentences after the current to the Left
    for N := C+1 to Lines[CurrentTrack].High do
      Lines[CurrentTrack].Line[N-1] := Lines[CurrentTrack].Line[N];

    //Delete Last Sentence
    SetLength(Lines[CurrentTrack].Line, Lines[CurrentTrack].High);
    Lines[CurrentTrack].High := High(Lines[CurrentTrack].Line);
    Lines[CurrentTrack].Number := Length(Lines[CurrentTrack].Line);

    CurrentNote[CurrentTrack] := 0;
    if (C > 0) then
      Lines[CurrentTrack].Current := C - 1
    else
      Lines[CurrentTrack].Current := 0;

    Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 2;
  end;

  // update lyric display
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
  EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
end;

procedure TScreenEditSub.DeleteSentence;
var
  Pv, Pt:         integer;
  TrackIndex:     integer;
  CurrentLine:    integer;
  LineIndex:      integer;

begin
  Pv := CurrentTrack;
  Pt := CurrentTrack;

  // FIXME
  {if CurrentSong.isDuet then
  begin
    if (Length(Lines[(CP+1) mod 2].Line[Lines[CP].Current].Note)=0) then
    begin
      Pv := 0;
      Pt := 1;
    end;
  end;  }

  for TrackIndex := Pv to Pt do
  begin
    CurrentLine := Lines[CurrentTrack].Current;
    {if (Pv <> Pt) or not CurrentSong.isDuet then
    begin}
      //Move all Sentences after the current to the Left
      for LineIndex := CurrentLine+1 to Lines[TrackIndex].High do
        CopyLine(TrackIndex, LineIndex, TrackIndex, LineIndex-1);

      //Delete Last Sentence
      SetLength(Lines[TrackIndex].Line, Lines[TrackIndex].High);
      Lines[TrackIndex].High := High(Lines[TrackIndex].Line);
      Lines[TrackIndex].Number := Length(Lines[TrackIndex].Line);

      CurrentNote[TrackIndex] := 0;
      if (CurrentLine > 0) then
        Lines[TrackIndex].Current := CurrentLine - 1
      else
        Lines[TrackIndex].Current := 0;
    {end else
    begin
      //delete all notes in that line
      SetLength(Lines[TrackIndex].Line[CurrentLine].Note, 0);

      //switch to the other line
      CP := (CP+1) mod 2;
      CurrentNote[CP] := 0;
      CurrentNote[(CP+1) mod 2] := 0;
    end;}
  end;

  Refresh;
  //SelectPrevNote();
  //SelectNextNote();
  Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 2;
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
end;

procedure TScreenEditSub.TransposeNote(Transpose: integer);
begin
  Inc(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Tone, Transpose);
end;

procedure TScreenEditSub.ChangeWholeTone(Tone: integer);
var
  C:  integer;
  N:  integer;
begin
  for C := 0 to Lines[CurrentTrack].High do
  begin
    Lines[CurrentTrack].Line[C].BaseNote := Lines[CurrentTrack].Line[C].BaseNote + Tone;
    for N := 0 to Lines[CurrentTrack].Line[C].HighNote do
      Lines[CurrentTrack].Line[C].Note[N].Tone := Lines[CurrentTrack].Line[C].Note[N].Tone + Tone;
  end;
end;

procedure TScreenEditSub.MoveAllToEnd(Move: integer);
var
  C:    integer;
  N:    integer;
  NStart: integer;
begin
  for C := Lines[CurrentTrack].Current to Lines[CurrentTrack].High do
  begin
    NStart := 0;
    if C = Lines[CurrentTrack].Current then
      NStart := CurrentNote[CurrentTrack];
    for N := NStart to Lines[CurrentTrack].Line[C].HighNote do
    begin
      Inc(Lines[CurrentTrack].Line[C].Note[N].StartBeat, Move); // move note start

      if N = 0 then
      begin // fix beginning
        Inc(Lines[CurrentTrack].Line[C].StartBeat, Move);
      end;

      if N = Lines[CurrentTrack].Line[C].HighNote then // fix ending
        Inc(Lines[CurrentTrack].Line[C].EndBeat, Move);

    end; // for
  end; // for
end;

procedure TScreenEditSub.MoveTextToRight;
var
  C:      integer;
  N:      integer;
  NHigh:  integer;
begin
  {
  C := Lines[CurrentTrack].Current;

  for N := Lines[CurrentTrack].Line[C].HighNut downto 1 do
  begin
    Lines[CurrentTrack].Line[C].Note[N].Text := Lines[CurrentTrack].Line[C].Note[N-1].Text;
  end; // for

  Lines[CurrentTrack].Line[C].Note[0].Text := '- ';
  }

  C := Lines[CurrentTrack].Current;
  NHigh := Lines[CurrentTrack].Line[C].HighNote;

  // last word
  Lines[CurrentTrack].Line[C].Note[NHigh].Text := Lines[CurrentTrack].Line[C].Note[NHigh-1].Text + Lines[CurrentTrack].Line[C].Note[NHigh].Text;

  // other words
  for N := NHigh - 1 downto CurrentNote[CurrentTrack] + 1 do
  begin
    Lines[CurrentTrack].Line[C].Note[N].Text := Lines[CurrentTrack].Line[C].Note[N-1].Text;
  end; // for
  Lines[CurrentTrack].Line[C].Note[CurrentNote[CurrentTrack]].Text := '- ';
end;

procedure TScreenEditSub.MarkSrc;
begin
  CopySrc := Lines[CurrentTrack].Current;
end;

procedure TScreenEditSub.PasteText;
var
  C:    integer;
  N:    integer;
begin
  C := Lines[CurrentTrack].Current;

  for N := 0 to Lines[CurrentTrack].Line[CopySrc].HighNote do
    Lines[CurrentTrack].Line[C].Note[N].Text := Lines[CurrentTrack].Line[CopySrc].Note[N].Text;
end;

procedure TScreenEditSub.CopySentence(Src, Dst: integer);
var
  N:     integer;
  Time1: integer;
  Time2: integer;
  TD:    integer;
begin
  Time1 := Lines[CurrentTrack].Line[Src].Note[0].StartBeat;
  Time2 := Lines[CurrentTrack].Line[Dst].Note[0].StartBeat;
  TD := Time2-Time1;

  SetLength(Lines[CurrentTrack].Line[Dst].Note, Lines[CurrentTrack].Line[Src].HighNote + 1);
  Lines[CurrentTrack].Line[Dst].HighNote := Lines[CurrentTrack].Line[Src].HighNote;
  for N := 0 to Lines[CurrentTrack].Line[Src].HighNote do
  begin
    Lines[CurrentTrack].Line[Dst].Note[N].Text := Lines[CurrentTrack].Line[Src].Note[N].Text;
    Lines[CurrentTrack].Line[Dst].Note[N].Duration := Lines[CurrentTrack].Line[Src].Note[N].Duration;
    Lines[CurrentTrack].Line[Dst].Note[N].Tone := Lines[CurrentTrack].Line[Src].Note[N].Tone;
    Lines[CurrentTrack].Line[Dst].Note[N].StartBeat := Lines[CurrentTrack].Line[Src].Note[N].StartBeat + TD;
  end;
  N := Lines[CurrentTrack].Line[Src].HighNote;
  Lines[CurrentTrack].Line[Dst].EndBeat := Lines[CurrentTrack].Line[Dst].Note[N].StartBeat + Lines[CurrentTrack].Line[Dst].Note[N].Duration;
end;

procedure TScreenEditSub.CopySentences(Src, Dst, Num: integer);
var
  C:      integer;
begin
  // create place for new sentences
  SetLength(Lines[CurrentTrack].Line, Lines[CurrentTrack].Number + Num - 1);

  // moves sentences next to the destination
  for C := Lines[CurrentTrack].High downto Dst + 1 do
  begin
    Lines[CurrentTrack].Line[C + Num - 1] := Lines[CurrentTrack].Line[C];
  end;

  // prepares new sentences: sets sentence start and create first note
  for C := 1 to Num-1 do
  begin
    Lines[CurrentTrack].Line[Dst + C].StartBeat := Lines[CurrentTrack].Line[Dst + C - 1].Note[0].StartBeat +
      (Lines[CurrentTrack].Line[Src + C].Note[0].StartBeat - Lines[CurrentTrack].Line[Src + C - 1].Note[0].StartBeat);
    SetLength(Lines[CurrentTrack].Line[Dst + C].Note, 1);
    Lines[CurrentTrack].Line[Dst + C].HighNote := 0;
    Lines[CurrentTrack].Line[Dst + C].Note[0].StartBeat := Lines[CurrentTrack].Line[Dst + C].StartBeat;
    Lines[CurrentTrack].Line[Dst + C].Note[0].Duration := 1;
    Lines[CurrentTrack].Line[Dst + C].EndBeat := Lines[CurrentTrack].Line[Dst + C].StartBeat + 1;
  end;

  // increase counters
  Lines[CurrentTrack].Number := Lines[CurrentTrack].Number + Num - 1;
  Lines[CurrentTrack].High := Lines[CurrentTrack].High + Num - 1;

  for C := 0 to Num-1 do
    CopySentence(Src + C, Dst + C);
end;

procedure TScreenEditSub.MakeSolo;
begin
  // use current track to make solo
  if (CurrentTrack <> 0) then
    Lines[0] := Lines[CurrentTrack];

  SetLength(Lines, 1);
  CurrentSong.isDuet := false;

  CurrentTrack := 0;
  Refresh;
  CurrentNote[CurrentTrack] := 0;
  Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 2;

  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
end;

procedure TScreenEditSub.MakeDuet;
var
  TrackIndex: integer;
  LineIndex:  integer;

begin
  SetLength(Lines, 2);

  Lines[1].Current := Lines[0].Current;
  Lines[1].High := Lines[0].High;
  Lines[1].Number := Lines[0].Number;
  Lines[1].Resolution := Lines[0].Resolution;
  Lines[1].NotesGAP := Lines[0].NotesGAP;
  Lines[1].ScoreValue := 0;
  SetLength(Lines[1].Line, Length(Lines[0].Line));

  for LineIndex := 0 to High(Lines[0].Line) do
    CopyLine(0, LineIndex, 1, LineIndex);

  CurrentSong.isDuet := true;

  CurrentNote[1] := 0;
  Lines[1].Current := 0;

  EditorLyrics[1] := EditorLyrics[0];

  //delete medley
  MedleyNotes.isStart := false;
  MedleyNotes.isEnd := false;
  CurrentSong.Medley.Source := msNone;
end;

function TScreenEditSub.DuetCopyLine: boolean;
var
  SrcLine:    integer;
  DstLine:    integer;
  SrcTrack:   integer;
  DstTrack:   integer;

  SrcStart:   integer;
  SrcEnd:     integer;

  DstStart:   integer;
  DstEnd:     integer;

  SrcNumN:    integer;
  DstNumN:    integer;

  LineIndex1: integer;
  LineIndex2: integer;

  LineLength: integer;
begin
  Result := false;

  SrcTrack := CurrentTrack;
  DstTrack := (CurrentTrack+1) mod 2;
  SrcLine := Lines[SrcTrack].Current;
  DstLine := -1;

  SrcStart := Lines[SrcTrack].Line[SrcLine].Note[0].StartBeat;
  SrcNumN := Length(Lines[SrcTrack].Line[SrcLine].Note);
  SrcEnd := Lines[SrcTrack].Line[SrcLine].Note[SrcNumN-1].StartBeat + Lines[SrcTrack].Line[SrcLine].Note[SrcNumN-1].Duration;

  for LineIndex1 := 0 to High(Lines[DstTrack].Line) do
  begin
    DstStart := Lines[DstTrack].Line[LineIndex1].Note[0].StartBeat;
    DstNumN := Length(Lines[DstTrack].Line[LineIndex1].Note);
    DstEnd := Lines[DstTrack].Line[LineIndex1].Note[DstNumN-1].StartBeat + Lines[DstTrack].Line[LineIndex1].Note[DstNumN-1].Duration;
    if (DstStart<=SrcStart) and (SrcEnd<=DstEnd) then
    begin
      DstLine := LineIndex1;
      break;
    end;

    if (DstLine = -1) and (LineIndex1 < Length(Lines[DstTrack].Line)-1) then
    begin
      DstStart := DstEnd;
      DstEnd := Lines[DstTrack].Line[LineIndex1+1].Note[0].StartBeat;
      if (DstStart<SrcStart) and (SrcEnd<DstEnd) then
      begin
        LineLength := Length(Lines[DstTrack].Line);
        SetLength(Lines[DstTrack].Line, LineLength + 1);
        Inc(Lines[DstTrack].Number);
        Inc(Lines[DstTrack].High);

        for LineIndex2 := LineLength-1 downto LineIndex1 do
          CopyLine(DstTrack, LineIndex2, DstTrack, LineIndex2+1);

        SetLength(Lines[DstTrack].Line[LineIndex1+1].Note, 0);
        DstLine := LineIndex1+1;
        break;
      end;
    end;
  end;

  if (DstLine = -1) then
    Exit;

  CopyLine(SrcTrack, SrcLine, DstTrack, DstLine);

  Refresh;
  EditorLyrics[DstTrack].AddLine(DstTrack, Lines[DstTrack].Current);
  EditorLyrics[DstTrack].Selected := 0;
  CurrentNote[DstTrack] := 0;
  Lines[SrcTrack].Line[SrcLine].Note[CurrentNote[SrcTrack]].Color := 2;
  Result := true;
end;

procedure TScreenEditSub.CopyLine(SrcTrack, SrcLine, DstTrack, DstLine: integer);
var
  N:  integer;
begin
  Lines[DstTrack].Line[DstLine] := Lines[SrcTrack].Line[SrcLine];
  Lines[DstTrack].Line[DstLine].Note := Copy(Lines[SrcTrack].Line[SrcLine].Note);
end;

function TScreenEditSub.DuetMoveLine: boolean;
begin
  Result := DuetCopyLine;
  if (Result) then
    DeleteSentence;
end;

procedure TScreenEditSub.CopyToUndo;
var
  BPMIndex:      integer;
  TrackIndex:    integer;
  LineIndex:     integer;
  NoteIndex:     integer;
begin
  SetLength(UndoLines, high(UndoLines)+2);
  CurrentUndoLines := high(UndoLines);
  SetLength(UndoStateNote, CurrentUndoLines+1);
  SetLength(UndoHeader, CurrentUndoLines+1);

  Undoheader[CurrentUndoLines].Title := CurrentSong.Title;
  Undoheader[CurrentUndoLines].Artist := CurrentSong.Artist;
  Undoheader[CurrentUndoLines].Language := CurrentSong.Language;
  Undoheader[CurrentUndoLines].Edition := CurrentSong.Edition;
  Undoheader[CurrentUndoLines].Genre := CurrentSong.Genre;
  Undoheader[CurrentUndoLines].Year := CurrentSong.Year;
  Undoheader[CurrentUndoLines].Creator := CurrentSong.Creator;
  Undoheader[CurrentUndoLines].Mp3 := CurrentSong.Mp3;
  Undoheader[CurrentUndoLines].Mp3Id := SelectsS[Mp3SlideId].SelectedOption;
  Undoheader[CurrentUndoLines].Cover := CurrentSong.Cover;
  Undoheader[CurrentUndoLines].CoverId := SelectsS[CoverSlideId].SelectedOption;
  Undoheader[CurrentUndoLines].Background := CurrentSong.Background;
  Undoheader[CurrentUndoLines].BackgroundId := SelectsS[BackgroundSlideId].SelectedOption;
  Undoheader[CurrentUndoLines].Video := CurrentSong.Video;
  Undoheader[CurrentUndoLines].VideoId := SelectsS[VideoSlideId].SelectedOption;
  Undoheader[CurrentUndoLines].VideoGAP := CurrentSong.VideoGAP;
  SetLength(Undoheader[CurrentUndoLines].BPM, length(CurrentSong.BPM));
  for BPMIndex := 0 to High(CurrentSong.BPM) do
  begin
    Undoheader[CurrentUndoLines].BPM[BPMIndex].BPM := CurrentSong.BPM[BPMIndex].BPM;
    Undoheader[CurrentUndoLines].BPM[BPMIndex].StartBeat := CurrentSong.BPM[BPMIndex].StartBeat;
  end;
  Undoheader[CurrentUndoLines].GAP  := CurrentSong.GAP;
  Undoheader[CurrentUndoLines].StartTag := CurrentSong.Start;
  Undoheader[CurrentUndoLines].EndTag := CurrentSong.Finish;
  if not (CurrentSong.isDuet) then
  begin
    Undoheader[CurrentUndoLines].MedleyStartBeat := Lines[CurrentTrack].Line[MedleyNotes.start.line].Note[MedleyNotes.start.note].StartBeat;
    Undoheader[CurrentUndoLines].MedleyEndBeat := Lines[CurrentTrack].Line[MedleyNotes.end_.line].Note[MedleyNotes.end_.note].StartBeat + Lines[CurrentTrack].Line[MedleyNotes.end_.line].Note[MedleyNotes.end_.note].Duration;
  end;
  Undoheader[CurrentUndoLines].PreviewStart := CurrentSong.PreviewStart;
  Undoheader[CurrentUndoLines].Relative := CurrentSong.Relative;

  for TrackIndex := 0 to 0 do // High(Lines) do
  begin
    UndoStateNote[CurrentUndoLines] := CurrentNote[TrackIndex];

    UndoLines[CurrentUndoLines].Current := Lines[TrackIndex].Current;
    UndoLines[CurrentUndoLines].High := Lines[TrackIndex].High;
    UndoLines[CurrentUndoLines].Number := Lines[TrackIndex].Number;
    UndoLines[CurrentUndoLines].Resolution := Lines[TrackIndex].Resolution;
    UndoLines[CurrentUndoLines].NotesGAP := Lines[TrackIndex].NotesGAP;
    UndoLines[CurrentUndoLines].ScoreValue := Lines[TrackIndex].ScoreValue;
    SetLength(UndoLines[CurrentUndoLines].Line, length(Lines[TrackIndex].Line));

    for LineIndex := 0 to High(Lines[TrackIndex].Line) do
    begin
      UndoLines[CurrentUndoLines].Line[LineIndex].StartBeat  := Lines[TrackIndex].Line[LineIndex].StartBeat;
      UndoLines[CurrentUndoLines].Line[LineIndex].Lyric      := Lines[TrackIndex].Line[LineIndex].Lyric;
      UndoLines[CurrentUndoLines].Line[LineIndex].EndBeat    := Lines[TrackIndex].Line[LineIndex].EndBeat;
      UndoLines[CurrentUndoLines].Line[LineIndex].BaseNote   := Lines[TrackIndex].Line[LineIndex].BaseNote;
      UndoLines[CurrentUndoLines].Line[LineIndex].HighNote   := Lines[TrackIndex].Line[LineIndex].HighNote;
      UndoLines[CurrentUndoLines].Line[LineIndex].TotalNotes := Lines[TrackIndex].Line[LineIndex].TotalNotes;
      UndoLines[CurrentUndoLines].Line[LineIndex].LastLine   := Lines[TrackIndex].Line[LineIndex].LastLine;

      SetLength(UndoLines[CurrentUndoLines].Line[LineIndex].Note, length(Lines[TrackIndex].Line[LineIndex].Note));
      for NoteIndex := 0 to High(Lines[TrackIndex].Line[LineIndex].Note) do
      begin
        UndoLines[CurrentUndoLines].Line[LineIndex].Note[NoteIndex].Color     := Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Color;
        UndoLines[CurrentUndoLines].Line[LineIndex].Note[NoteIndex].StartBeat := Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].StartBeat;
        UndoLines[CurrentUndoLines].Line[LineIndex].Note[NoteIndex].Duration    := Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Duration;
        UndoLines[CurrentUndoLines].Line[LineIndex].Note[NoteIndex].Tone      := Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Tone;
        UndoLines[CurrentUndoLines].Line[LineIndex].Note[NoteIndex].Text      := Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Text;
        UndoLines[CurrentUndoLines].Line[LineIndex].Note[NoteIndex].NoteType  := Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].NoteType;
      end; //for NoteIndex
    end; //for LineIndex
  end; // TrackIndex
end;

procedure TScreenEditSub.Refresh;
var
  TrackIndex:  integer;
  LineIndex:  integer;
  NoteIndex:  integer;

begin
  FixTimings;
  LyricsCorrectSpaces;

  if MedleyNotes.isStart and
    ((High(Lines[0].Line) < MedleyNotes.start.line) or
     (High(Lines[0].Line[MedleyNotes.start.line].Note) < MedleyNotes.start.note)) then
    MedleyNotes.isStart := false;

  if MedleyNotes.isEnd and
    ((High(Lines[0].Line) < MedleyNotes.end_.line) or
     (High(Lines[0].Line[MedleyNotes.end_.line].Note) < MedleyNotes.end_.note)) then
    MedleyNotes.isEnd := false;

  {
  for TrackIndex := 0 to High(Lines) do
  begin
    Lines[TrackIndex].Number := Length(Lines[TrackIndex].Line);
    Lines[TrackIndex].High := Lines[TrackIndex].Number-1;
    Lines[TrackIndex].ScoreValue := 0;

    for LineIndex := 0 to Lines[TrackIndex].High do
    begin
      with Lines[TrackIndex].Line[LineIndex] do
      begin
        HighNote := Length(Note) - 1;
        TotalNotes := 0;
        BaseNote := 120;

        if (Length(Note)>0) then
        begin
          StartBeat := Note[0].StartBeat;
          for NoteIndex := 0 to High(Lines[TrackIndex].Line[LineIndex].Note) do
          begin
            Note[NoteIndex].Color := 0;
            if (MedleyNotes.isStart and (MedleyNotes.start.CP = TrackIndex) and (MedleyNotes.start.line = LineIndex) and
              (MedleyNotes.start.note = NoteIndex)) or
              (MedleyNotes.isEnd and (MedleyNotes.end_.CP = TrackIndex) and (MedleyNotes.end_.line = LineIndex) and
              (MedleyNotes.end_.note = NoteIndex)) then
              Note[NoteIndex].IsMedley := true
            else
              Note[NoteIndex].IsMedley := false;

            Note[NoteIndex].IsStartPreview := false;

            Lines[TrackIndex].ScoreValue := Lines[TrackIndex].ScoreValue + Note[NoteIndex].Duration * ScoreFactor[Note[NoteIndex].NoteType];
            TotalNotes := TotalNotes + Note[NoteIndex].Duration * ScoreFactor[Note[NoteIndex].NoteType];

            if (Note[NoteIndex].Tone < BaseNote) then
              BaseNote := Note[NoteIndex].Tone;
          end;
        end else
          BaseNote := 0;
      end;
    end;
  end;

  // set Preview Start
  //MedleyNotes.Preview := FindNote(round(GetMidBeat(CurrentSong.PreviewStart-CurrentSong.Gap/1000)));
  //Lines[MedleyNotes.Preview.CP].Line[MedleyNotes.Preview.line].Note[MedleyNotes.Preview.note].IsStartPreview := true;
  //CurrentSong.PreviewStart := GetTimeFromBeat(Lines[MedleyNotes.Preview.CP].Line[MedleyNotes.Preview.line].Note[MedleyNotes.Preview.note].start);
  }
end;

procedure TScreenEditSub.CopyFromUndo;
var
 I,J: integer;

begin

CurrentUndoLines := high(UndoLines);

if CurrentUndoLines >= 0 then
begin
  CurrentSong.Title := Undoheader[CurrentUndoLines].Title;
  CurrentSong.Artist := Undoheader[CurrentUndoLines].Artist;
  CurrentSong.Language := Undoheader[CurrentUndoLines].Language;
  CurrentSong.Edition := Undoheader[CurrentUndoLines].Edition;
  CurrentSong.Genre := Undoheader[CurrentUndoLines].Genre;
  CurrentSong.Year := Undoheader[CurrentUndoLines].Year;
  CurrentSong.Creator := Undoheader[CurrentUndoLines].Creator;
  CurrentSong.Mp3 := Undoheader[CurrentUndoLines].Mp3;
  SelectsS[Mp3SlideId].SelectedOption := Undoheader[CurrentUndoLines].Mp3Id;
  CurrentSong.Cover := Undoheader[CurrentUndoLines].Cover;
  SelectsS[CoverSlideId].SelectedOption := Undoheader[CurrentUndoLines].CoverId;
  CurrentSong.Background := Undoheader[CurrentUndoLines].Background;
  SelectsS[BackgroundSlideId].SelectedOption := Undoheader[CurrentUndoLines].BackgroundId;
  CurrentSong.Video := Undoheader[CurrentUndoLines].Video;
  SelectsS[VideoSlideId].SelectedOption := Undoheader[CurrentUndoLines].VideoId;
  CurrentSong.VideoGAP := Undoheader[CurrentUndoLines].VideoGAP;
  SetLength(CurrentSong.BPM, length(Undoheader[CurrentUndoLines].BPM));
  for I := 0 to High(Undoheader[CurrentUndoLines].BPM) do
  begin
    CurrentSong.BPM[I].BPM := Undoheader[CurrentUndoLines].BPM[I].BPM;
    CurrentSong.BPM[I].StartBeat := Undoheader[CurrentUndoLines].BPM[I].StartBeat;
  end;
  CurrentSong.GAP := Undoheader[CurrentUndoLines].GAP;
  CurrentSong.Start := Undoheader[CurrentUndoLines].StartTag;
  CurrentSong.Finish := Undoheader[CurrentUndoLines].EndTag;
  CurrentSong.Medley.StartBeat := Undoheader[CurrentUndoLines].MedleyStartBeat;
  CurrentSong.Medley.EndBeat := Undoheader[CurrentUndoLines].MedleyEndBeat;
  CurrentSong.PreviewStart := Undoheader[CurrentUndoLines].PreviewStart;
  CurrentSong.Relative := Undoheader[CurrentUndoLines].Relative;

  CurrentNote[CurrentTrack] := UndoStateNote[high(UndoStateNote)];

  Lines[CurrentTrack].Current := UndoLines[CurrentUndoLines].Current;
  Lines[CurrentTrack].High := UndoLines[CurrentUndoLines].High;
  Lines[CurrentTrack].Number := UndoLines[CurrentUndoLines].Number;
  Lines[CurrentTrack].Resolution := UndoLines[CurrentUndoLines].Resolution;
  Lines[CurrentTrack].NotesGAP := UndoLines[CurrentUndoLines].NotesGAP;
  Lines[CurrentTrack].ScoreValue := UndoLines[CurrentUndoLines].ScoreValue;
  SetLength(Lines[CurrentTrack].Line, length(UndoLines[CurrentUndoLines].Line));
  for I := 0 to High(UndoLines[CurrentUndoLines].Line) do
  begin
      Lines[CurrentTrack].Line[I].StartBeat := UndoLines[CurrentUndoLines].Line[I].StartBeat;
      Lines[CurrentTrack].Line[I].Lyric := UndoLines[CurrentUndoLines].Line[I].Lyric;
      Lines[CurrentTrack].Line[I].EndBeat := UndoLines[CurrentUndoLines].Line[I].EndBeat;
      Lines[CurrentTrack].Line[I].BaseNote := UndoLines[CurrentUndoLines].Line[I].BaseNote;
      Lines[CurrentTrack].Line[I].HighNote := UndoLines[CurrentUndoLines].Line[I].HighNote;
      Lines[CurrentTrack].Line[I].TotalNotes := UndoLines[CurrentUndoLines].Line[I].TotalNotes;
      Lines[CurrentTrack].Line[I].LastLine := UndoLines[CurrentUndoLines].Line[I].LastLine;

      SetLength(Lines[CurrentTrack].Line[I].Note, length(UndoLines[CurrentUndoLines].Line[I].Note));
      for  J:= 0 to High(UndoLines[CurrentUndoLines].Line[I].Note) do
      begin
        Lines[CurrentTrack].Line[I].Note[J].Color := UndoLines[CurrentUndoLines].Line[I].Note[J].Color;
        Lines[CurrentTrack].Line[I].Note[J].StartBeat := UndoLines[CurrentUndoLines].Line[I].Note[J].StartBeat;
        Lines[CurrentTrack].Line[I].Note[J].Duration := UndoLines[CurrentUndoLines].Line[I].Note[J].Duration;
        Lines[CurrentTrack].Line[I].Note[J].Tone := UndoLines[CurrentUndoLines].Line[I].Note[J].Tone;
        Lines[CurrentTrack].Line[I].Note[J].Text := UndoLines[CurrentUndoLines].Line[I].Note[J].Text;
        Lines[CurrentTrack].Line[I].Note[J].NoteType := UndoLines[CurrentUndoLines].Line[I].Note[J].NoteType;
      end; //for J
  end; //for I
  SetLength(UndoStateNote, high(UndoStateNote));
  SetLength(UndoHeader, high(UndoLines));
  SetLength(UndoLines, high(UndoLines));
  Text[TextDebug].Text := Language.Translate('EDIT_INFO_UNDO');

  // to refresh all headers
  SelectsS[TitleSlideId].TextOpt[0].Text := CurrentSong.Title;
  SelectsS[ArtistSlideId].TextOpt[0].Text := CurrentSong.Artist;
  SelectsS[LanguageSlideId].TextOpt[0].Text := CurrentSong.Language;
  SelectsS[EditionSlideId].TextOpt[0].Text := CurrentSong.Edition;
  SelectsS[GenreSlideId].TextOpt[0].Text := CurrentSong.Genre;
  SelectsS[YearSlideId].TextOpt[0].Text := IntToStr(CurrentSong.Year);
  SelectsS[CreatorSlideId].TextOpt[0].Text := CurrentSong.Creator;
  SelectsS[LyricSlideId].TextOpt[0].Text := Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Text;
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
  EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
end; //if CurrentUndoLines
end;

procedure TScreenEditSub.DrawPlayerTrack(X, Y, W: real; Space: integer; CurrentTone: integer; Count: integer; CurrentNote: array of integer);
var
  TempR:                      real;
  Rec:                        TRecR;
  N, scale:                   integer;
  //R, G, B, A:                 real;
  NotesH2, W1, H1, X1, X2:    real;
begin
  glColor3f(1, 1, 1);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  TempR := W / (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat - Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[0].StartBeat);

  NotesH2 := int(NotesH[0] * 0.65);
  W1 := NotesW[0] * 2 + 2;
  H1 := NotesH[0] * 1.5;// + 3.5;
  X2 := 40 + 0.5 + 10*ScreenX+Count;
  X1 := X2-W1-2;

  Rec.Left  := X1;
  Rec.Right := X2;
  scale := 0;
  repeat
    if (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Tone + 12*scale > CurrentTone) then
      dec(scale)
    else
      inc(scale);

  until (
    (((Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Tone + 12*scale) / 12) < 1) and
    (((Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Tone + 12*scale) / 12) >= 0));

  Rec.Top := 410 - (CurrentTone-12*scale-Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].BaseNote)*Space/2 - H1;
  Rec.Bottom := Rec.Top + 2 * H1;

  glColor3f(1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, Tex_Lyric_Help_Bar.TexNum);
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
    glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
    glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
    glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
  glEnd;

  // draw currently recorded tone (yellow)
  SetFontPos(480, 296);
  glColor4f(1, 1, 0, 1);
  glPrint(GetNoteName(CurrentTone));
end;

procedure TScreenEditSub.DrawInfoBar(X, Y, W, H: integer; ColR, ColG, ColB, Alpha: real; Track: integer);
var
  start, end_:        integer;
  SongStart, SongEnd: integer;
  ww:                 integer;
  i:                  integer;

  pos:                real;
  br:                 real;

  line:               integer;
  numLines:           integer;

  function FindStartBeat(): integer;
  var
    TrackIndex: integer;
    LineIndex:  integer;
  begin
    Result := High(integer);

    for TrackIndex := 0 to High(Lines) do
      for LineIndex := 0 to High(Lines[TrackIndex].Line) do
      begin
        if (Length(Lines[TrackIndex].Line[LineIndex].Note) > 0) then
        begin
          if(Result > Lines[TrackIndex].Line[LineIndex].Note[0].StartBeat) then
            Result := Lines[TrackIndex].Line[LineIndex].Note[0].StartBeat;
        end;
      end;
  end;

  function FindEndBeat(): integer;
  var
    TrackIndex: integer;
    LineIndex:  integer;
    NoteIndex:  integer;
  begin
    Result := Low(integer);

    for TrackIndex := 0 to High(Lines) do
      for LineIndex := 0 to High(Lines[TrackIndex].Line) do
      begin
        if (Length(Lines[TrackIndex].Line[LineIndex].Note)>0) then
        begin
          NoteIndex := Length(Lines[TrackIndex].Line[LineIndex].Note)-1;
          if(Result < Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].StartBeat + Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Duration) then
            Result := Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].StartBeat + Lines[TrackIndex].Line[LineIndex].Note[NoteIndex].Duration;
        end;
      end;
  end;

begin
  numLines := Length(Lines[Track].Line);

  if (Track = CurrentTrack) then
    EditDrawBorderedBox(X, Y, W, H, ColR, ColG, ColB, Alpha)
  else
    EditDrawBorderedBox(X, Y, W, H, ColR, ColG, ColB, Alpha);

  if(numLines=1) then
    Exit;

  SongStart := FindStartBeat;
  SongEnd := FindEndBeat;
  ww := SongEnd - SongStart;

  Statics[playerIconId[Track+1]].Visible := true;

  for i := 0 to High(TransparentLineButtonId) do
  begin
    Button[TransparentLineButtonId[i]].SetX(0);
    Button[TransparentLineButtonId[i]].SetY(0);
    Button[TransparentLineButtonId[i]].SetW(0);
    Button[TransparentLineButtonId[i]].SetH(0);
  end;

  while (length(TransparentLineButtonId) < numLines) do
  begin
      SetLength(InteractiveLineId, Length(InteractiveLineId)+1);
      SetLength(TransparentLineButtonId, Length(TransparentLineButtonId)+1);
      TransparentLineButtonId[Length(TransparentLineButtonId)-1] := AddButton(0, 0, 0, 0,PATH_NONE);
      InteractiveLineId[Length(InteractiveLineId)-1] := length(Interactions)-1;
  end;

  for line := 0 to numLines - 1 do
  begin
    if (line = Lines[Track].Current) and not (PlaySentence or PlaySentenceMidi or PlayOne) then
      glColor4f(0.4, 0.4, 0, 1) // currently selected line
    else
      if (CurrentSong.Medley.Source <> msNone) and (line >= MedleyNotes.start.line) and (line <= MedleyNotes.end_.line) then
        glColor4f(0.15, 0.75, 0.15, 1)
      else
      begin
        // all other lines in orange
        if (Track = CurrentTrack) then
          glColor4f(1, 0.6, 0, 1)
        else
          glColor4f(0.7, 0.7, 0.7, 1);
      end;

    start := Lines[Track].Line[line].Note[0].StartBeat;
    end_ := Lines[Track].Line[line].Note[Lines[Track].Line[line].HighNote].StartBeat +
      Lines[Track].Line[line].Note[Lines[Track].Line[line].HighNote].Duration;

    pos := (start - SongStart)/ww*w;
    br := (end_-start)/ww*w;

    // todo: add transparent active button to change current line
    Button[TransparentLineButtonId[line]].SetX(x+pos);
    Button[TransparentLineButtonId[line]].SetY(y);
    Button[TransparentLineButtonId[line]].SetW(br);
    Button[TransparentLineButtonId[line]].SetH(h);

    glbegin(gl_quads);
      glVertex2f(x+pos, y);
      glVertex2f(x+pos, y+h);
      glVertex2f(x+pos+br, y+h);
      glVertex2f(x+pos+br, y);
    glEnd;
  end;

  if(PlaySentence or PlaySentenceMidi or PlayOne) then
  begin
    glColor4f(0, 0, 0, 0.5);
    pos := 0;
    br := (CurrentBeat - SongStart)/ww*w;
    if (br>w) then
      br := w;
  end else
  begin
    glColor4f(1, 0, 0, 1);
    pos := (Lines[Track].Line[Lines[Track].Current].Note[CurrentNote[Track]].StartBeat - SongStart) / ww*w;
    br := Lines[Track].Line[Lines[Track].Current].Note[CurrentNote[Track]].Duration / ww*w;
    if (br<1) then
      br := 1;
  end;

  glEnable(GL_BLEND);
  glbegin(gl_quads);
    glVertex2f(x+pos, y);
    glVertex2f(x+pos, y+h);
    glVertex2f(x+pos+br, y+h);
    glVertex2f(x+pos+br, y);
  glEnd;
  glDisable(GL_BLEND);

  glLineWidth(1);
end;

procedure TScreenEditSub.DrawText(X, Y, W, H: real; Track: integer; NumLines: integer);
var
  Rec:   TRecR;
  Count: integer;
  TempR: real;
  Space: real;

  //PlayerNumber:  integer;
  OrgFontStyle:  integer;

  GoldenStarPos: real;
begin
  if ( (1 shl Track) <> 0) then
  begin
    Space := H / (NumLines - 1);
    //PlayerNumber := Track + 1; // Player 1 is 0
    glColor3f(1, 1, 1);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    if not Lines[Track].Line[Lines[Track].Current].HasLength(TempR) then TempR := 0
    else TempR := W / TempR;

    with Lines[Track].Line[Lines[Track].Current] do
    begin

      OrgFontStyle := ActFont;
      glColor4f(0, 0, 0, 1);
      SetFontStyle(1);
      SetFontItalic(False);
      SetFontSize(14);

      for Count := 0 to HighNote do
      begin
        with Note[Count] do
        begin
          // left part
          Rec.Left  := 0;
          Rec.Right := 0;
          BaseNote := Lines[Track].Line[Lines[Track].Current].BaseNote;
          Rec.Top := Y - (Tone-BaseNote)*Space/2 - NotesH[0];
          Rec.Bottom := Rec.Top + 2 * NotesH[0];
          // middle part
          Rec.Left := (StartBeat - Lines[Track].Line[Lines[Track].Current].Note[0].StartBeat) * TempR + X + 0.5 + 10*ScreenX + NotesW[0];
          Rec.Right := (StartBeat + Duration - Lines[Track].Line[Lines[Track].Current].Note[0].StartBeat) * TempR + X - NotesW[0] - 0.5 + 10*ScreenX;
          SetFontPos (Rec.Left, Rec.Top);
          glPrint(Text);
        end; // with
      end; // for
    end; // with

    // revert the font to prevent conflicts within drawing the editor lyric line
    SetFontStyle(OrgFontStyle);

    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
  end;
end;

// show transparent background for interactive note

procedure TScreenEditSub.ShowInteractiveBackground;
var
  TempR:      real;
  i:          integer;
begin

  for i := 0 to High(TransparentNoteButtonId) do
  begin
    Button[TransparentNoteButtonId[i]].SetX(0);
    Button[TransparentNoteButtonId[i]].SetY(0);
    Button[TransparentNoteButtonId[i]].SetW(0);
    Button[TransparentNoteButtonId[i]].SetH(0);
  end;

// adding transparent buttons
  while (Length(TransparentNoteButtonId)-1 < Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].HighNote) do
  begin
      SetLength(InteractiveNoteId, Length(InteractiveNoteId)+1);
      SetLength(TransparentNoteButtonId, Length(TransparentNoteButtonId)+1);
      TransparentNoteButtonId[Length(TransparentNoteButtonId)-1] := AddButton(0, 0, 0, 0,PATH_NONE);
      InteractiveNoteId[Length(InteractiveNoteId)-1] := length(Interactions)-1;

  end;
  TempR := 720 / (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat - Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[0].StartBeat);
  for i := 0 to Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].HighNote do
  begin
    Button[TransparentNoteButtonId[i]].SetX(40 + (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[i].StartBeat - Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[0].StartBeat) * TempR + 0.5 + 10*ScreenX);
    Button[TransparentNoteButtonId[i]].SetY(410 - (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[i].Tone - Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].BaseNote)*15/2 - 9);
    Button[TransparentNoteButtonId[i]].SetW((Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[i].Duration) * TempR - 0.5  + 10*(ScreenX));
    Button[TransparentNoteButtonId[i]].SetH(19);
  end;
end;

// from revision 2475
procedure TScreenEditSub.StartVideoPreview;
var
  VideoFile:  IPath;

begin
  if Assigned(fCurrentVideo) then
  begin
    fCurrentVideo.Stop();
    fCurrentVideo := nil;
  end;

  VideoFile := CurrentSong.Path.Append(CurrentSong.Video);
  if (CurrentSong.Video.IsSet) and VideoFile.IsFile then
  begin
    fCurrentVideo := VideoPlayback.Open(VideoFile);
    if (fCurrentVideo <> nil) then
    begin
      fCurrentVideo.Position := CurrentSong.VideoGAP + AudioPlayback.Position;
      fCurrentVideo.Play;
    end;
  end;
end;

procedure TScreenEditSub.StopVideoPreview;
begin
  // Stop video preview of previous song
  if Assigned(fCurrentVideo) then
  begin
    fCurrentVideo.Stop();
    fCurrentVideo := nil;
  end;
end;

procedure TScreenEditSub.UpdateVideoPosition(NewPosition: real);
begin
  if (fCurrentVideo <> nil) then
  begin
    fCurrentVideo.Position := CurrentSong.VideoGAP + NewPosition;
    fCurrentVideo.Play;
  end;
end;

constructor TScreenEditSub.Create;
var
i: integer;
begin
  inherited Create;
  LoadFromTheme(Theme.EditSub);

  //video
  fCurrentVideo := nil;

  EditorLyrics[0] := TEditorLyrics.Create;
  EditorLyrics[1] := TEditorLyrics.Create;

  SetLength(Player, 1);
  SetLength(TitleVal, 0);
  SetLength(ArtistVal, 0);
  SetLength(LanguageVal, 0);
  SetLength(EditionVal, 0);
  SetLength(GenreVal, 0);
  SetLength(YearVal, 0);
  SetLength(CreatorVal, 0);
  SetLength(MP3Val, 0);
  SetLength(CoverVal, 0);
  SetLength(BackgroundVal, 0);
  SetLength(VideoVal, 0);
  SetLength(VideoGapVal, 0);
  SetLength(BPMVal, 0);
  SetLength(GAPVal, 0);
  SetLength(StartTagVal, 0);
  SetLength(EndTagVal, 0);
  SetLength(MedleyStartVal, 0);
  SetLength(MedleyEndVal, 0);
  SetLength(PreviewStartVal, 0);
  SetLength(RelativeVal, 0);
  SetLength(StartVal, 0);
  SetLength(ToneVal, 0);
  SetLength(DurationVal, 0);
  SetLength(LyricVal, 0);

  //volume
  SetLength(VolumeAudio,0);
  SetLength(VolumeMidi,0);
  SetLength(VolumeClick,0);

  // interactive
  SetLength(InteractiveNoteId, 0);
  SetLength(TransparentNoteButtonId, 0);
  SetLength(InteractiveLineId, 0);
  SetLength(TransparentLineButtonId, 0);

  // file info
  // title header
  TitleSlideId := AddSelectSlide(Theme.EditSub.SlideTitle, TitleData, TitleVal);
  SelectsS[TitleSlideId].Text.Align := 0;
  SelectsS[TitleSlideId].Text.X := SelectsS[TitleSlideId].Texture.X + 3;

  // artist header
  ArtistSlideId := AddSelectSlide(Theme.EditSub.SlideArtist, ArtistData, ArtistVal);
  SelectsS[ArtistSlideId].Text.Align := 0;
  SelectsS[ArtistSlideId].Text.X := SelectsS[ArtistSlideId].Texture.X + 3;

  // language header
  LanguageSlideId := AddSelectSlide(Theme.EditSub.SlideLanguage, LanguageData, LanguageVal);
  SelectsS[LanguageSlideId].Text.Align := 0;
  SelectsS[LanguageSlideId].Text.X := SelectsS[LanguageSlideId].Texture.X + 3;

  // edition header
  EditionSlideId := AddSelectSlide(Theme.EditSub.SlideEdition, EditionData, EditionVal);
  SelectsS[EditionSlideId].Text.Align := 0;
  SelectsS[EditionSlideId].Text.X := SelectsS[EditionSlideId].Texture.X + 3;

  // genre header
  GenreSlideId := AddSelectSlide(Theme.EditSub.SlideGenre, GenreData, GenreVal);
  SelectsS[GenreSlideId].Text.Align := 0;
  SelectsS[GenreSlideId].Text.X := SelectsS[GenreSlideId].Texture.X + 3;

  // year header
  YearSlideId := AddSelectSlide(Theme.EditSub.SlideYear, YearData, YearVal);
  SelectsS[YearSlideId].Text.Align := 0;
  SelectsS[YearSlideId].Text.X := SelectsS[YearSlideId].Texture.X + 3;

  // creator header
  CreatorSlideId := AddSelectSlide(Theme.EditSub.SlideCreator, CreatorData, CreatorVal);
  SelectsS[CreatorSlideId].Text.Align := 0;
  SelectsS[CreatorSlideId].Text.X := SelectsS[CreatorSlideId].Texture.X + 3;

  // MP3 header
  MP3SlideId := AddSelectSlide(Theme.EditSub.SlideMP3, MP3Data, MP3Val);
  SelectsS[MP3SlideId].Text.Align := 0;
  SelectsS[MP3SlideId].Text.X := SelectsS[MP3SlideId].Texture.X + 3;

  // cover header
  CoverSlideId := AddSelectSlide(Theme.EditSub.SlideCover, CoverData, CoverVal);
  SelectsS[CoverSlideId].Text.Align := 0;
  SelectsS[CoverSlideId].Text.X := SelectsS[CoverSlideId].Texture.X + 3;

  // background header
  BackgroundSlideId := AddSelectSlide(Theme.EditSub.SlideBackground, BackgroundData, BackgroundVal);
  SelectsS[BackgroundSlideId].Text.Align := 0;
  SelectsS[BackgroundSlideId].Text.X := SelectsS[BackgroundSlideId].Texture.X + 3;

  // video header
  VideoSlideId := AddSelectSlide(Theme.EditSub.SlideVideo, VideoData, VideoVal);
  SelectsS[VideoSlideId].Text.Align := 0;
  SelectsS[VideoSlideId].Text.X := SelectsS[VideoSlideId].Texture.X + 3;

  // videogap header
  VideoGapSlideId := AddSelectSlide(Theme.EditSub.SlideVideoGap, VideoGapData, VideoGapVal);
  SelectsS[VideoGapSlideId].Text.Align := 0;
  SelectsS[VideoGapSlideId].Text.X := SelectsS[VideoGapSlideId].Texture.X + 3;

  // BPM header
  BPMSlideId := AddSelectSlide(Theme.EditSub.SlideBPM, BPMData, BPMVal);
  SelectsS[BPMSlideId].Text.Align := 0;
  SelectsS[BPMSlideId].Text.X := SelectsS[BPMSlideId].Texture.X + 3;

  // GAP header
  GAPSlideId := AddSelectSlide(Theme.EditSub.SlideGAP, GAPData, GAPVal);
  SelectsS[GAPSlideId].Text.Align := 0;
  SelectsS[GAPSlideId].Text.X := SelectsS[GAPSlideId].Texture.X + 3;

  // StartTag header
  StartTagSlideId := AddSelectSlide(Theme.EditSub.SlideStartTag, StartTagData, StartTagVal);
  SelectsS[StartTagSlideId].Text.Align := 0;
  SelectsS[StartTagSlideId].Text.X := SelectsS[StartTagSlideId].Texture.X + 3;

  // EndTag header
  EndTagSlideId := AddSelectSlide(Theme.EditSub.SlideEndTag, EndTagData, EndTagVal);
  SelectsS[EndTagSlideId].Text.Align := 0;
  SelectsS[EndTagSlideId].Text.X := SelectsS[EndTagSlideId].Texture.X + 3;

  // MedleyStart header
  MedleyStartSlideId := AddSelectSlide(Theme.EditSub.SlideMedleyStart, MedleyStartData, MedleyStartVal);
  SelectsS[MedleyStartSlideId].Text.Align := 0;
  SelectsS[MedleyStartSlideId].Text.X := SelectsS[MedleyStartSlideId].Texture.X + 3;

  // MedleyEnd header
  MedleyEndSlideId := AddSelectSlide(Theme.EditSub.SlideMedleyEnd, MedleyEndData, MedleyEndVal);
  SelectsS[MedleyEndSlideId].Text.Align := 0;
  SelectsS[MedleyEndSlideId].Text.X := SelectsS[MedleyEndSlideId].Texture.X + 3;

  // PreviewStart header
  PreviewStartSlideId := AddSelectSlide(Theme.EditSub.SlidePreviewStart, PreviewStartData, PreviewStartVal);
  SelectsS[PreviewStartSlideId].Text.Align := 0;
  SelectsS[PreviewStartSlideId].Text.X := SelectsS[PreviewStartSlideId].Texture.X + 3;

  // Relative header
  RelativeSlideId := AddSelectSlide(Theme.EditSub.SlideRelative, RelativeData, RelativeVal);
  SelectsS[RelativeSlideId].Text.Align := 0;
  SelectsS[RelativeSlideId].Text.X := SelectsS[RelativeSlideId].Texture.X + 3;

  // background image & preview
  BackgroundImageId := AddStatic(Theme.EditSub.BackgroundImage);

  // note info
  // start header
  StartSlideId := AddSelectSlide(Theme.EditSub.SlideStart, StartData, StartVal);
  SelectsS[StartSlideId].Text.Align := 0;
  SelectsS[StartSlideId].Text.X := SelectsS[StartSlideId].Texture.X + 3;

  // duration header
  DurationSlideId := AddSelectSlide(Theme.EditSub.SlideDuration, DurationData, DurationVal);
  SelectsS[DurationSlideId].Text.Align := 0;
  SelectsS[DurationSlideId].Text.X := SelectsS[DurationSlideId].Texture.X + 3;

  // tone header
  ToneSlideId := AddSelectSlide(Theme.EditSub.SlideTone, ToneData, ToneVal);
  SelectsS[ToneSlideId].Text.Align := 0;
  SelectsS[ToneSlideId].Text.X := SelectsS[ToneSlideId].Texture.X + 3;

  // lyrics header
  LyricSlideId := AddSelectSlide(Theme.EditSub.SlideLyric, LyricData, LyricVal);
  SelectsS[LyricSlideId].Text.Align := 0;
  SelectsS[LyricSlideId].Text.X := SelectsS[LyricSlideId].Texture.X + 3;

  // control buttons
  UndoButtonId         := AddButton(Theme.EditSub.undo);
  PreviousSeqButtonID  := AddButton(Theme.EditSub.previousseq);
  NextSeqButtonID      := AddButton(Theme.EditSub.nextseq);
  FreestyleButtonID    := AddButton(Theme.EditSub.freestyle);
  GoldButtonID         := AddButton(Theme.EditSub.gold);
  PlayOnlyButtonID     := AddButton(Theme.EditSub.PlayOnly);
  PlayWithNoteButtonID := AddButton(Theme.EditSub.PlayWithNote);
  PlayNoteButtonID     := AddButton(Theme.EditSub.PlayNote);

  // current line
  TextSentence := AddButton(Theme.EditSub.ButtonCurrentLine);

  // current note
  TextNote := AddButton(Theme.EditSub.ButtonCurrentNote);

  // Audio Volume
  VolumeAudioSlideId := AddSelectSlide(Theme.EditSub.SelectVolAudio, VolumeAudioIndex, VolumeAudio);

  // Midi Volume
  VolumeMidiSlideId := AddSelectSlide(Theme.EditSub.SelectVolMidi, VolumeMidiIndex, VolumeMidi);

  // Click Volume
  VolumeClickSlideId := AddSelectSlide(Theme.EditSub.SelectVolClick, VolumeClickIndex, VolumeClick);

  {
  //TextVideoGap :=  AddText(600, 265,  0, 24, 0, 0, 0, 'e');
    playerIconId[1] := AddStatic(Theme.Score.StaticPlayerIdBox[1]);
//    (20, 460, 760, 15);
    Statics[playerIconId[1]].Texture.X := 2;
    Statics[playerIconId[1]].Texture.Y := 460;
    Statics[playerIconId[1]].Texture.W := 14;
    Statics[playerIconId[1]].Texture.H := 15;
    Statics[playerIconId[1]].Reflection := false;
    Statics[playerIconId[1]].Visible := false;

    playerIconId[2] := AddStatic(Theme.Score.StaticPlayerIdBox[3]);
    Statics[playerIconId[2]].Texture.X := 2;
    Statics[playerIconId[2]].Texture.Y := 480;
    Statics[playerIconId[2]].Texture.W := 14;
    Statics[playerIconId[2]].Texture.H := 15;
    Statics[playerIconId[2]].Reflection := false;
    Statics[playerIconId[2]].Visible := false;
    }

  // debug
  //TextDebug := AddText(265, 551, 0, 18, 0, 0, 0, '');
  TextDebug := AddText(Theme.EditSub.TextDebug);
  // in notes place -> for move notes by mouse
//  NotesBackgroundId := AddSelectSlide(Theme.EditSub.NotesBackground, i, Empty);

end;

procedure TScreenEditSub.OnShow;
const
  SUPPORTED_EXTS_AUDIO: array[0..1] of string = ('.mp3', '.ogg');
  SUPPORTED_EXTS_IMAGE: array[0..1] of string = ('.jpg', '.png');
  SUPPORTED_EXTS_VIDEO: array[0..10] of string = ('.avi', '.mov', '.divx', '.mkv', '.mpeg', '.mpg', '.mp4', '.mpeg', '.m2v', '.ts', '.wmv');
var
  FileExt: IPath;
  Files: TPathDynArray;
  i: integer;
  TrackIndex: integer;
  Ext: string;

  function IsBeatMatchingNote(beat: integer; Note: TLineFragment): boolean;
  begin
    Result := InRange(beat, Note.StartBeat, Note.StartBeat + Note.Duration);
  end;

  // borrowed from TScreenSing.LoadNextSong
  function FindNote(beat: integer): TPos;
  var
    LineIndex: integer;
    NoteIndex: integer;
    found:     boolean;
    min:       integer;
    diff:      integer;

  begin
    found := false;

    for LineIndex := 0 to High(Lines[0].Line) do
    begin
      for NoteIndex := 0 to High(Lines[0].Line[LineIndex].Note) do
      begin
        if (beat >= Lines[0].Line[LineIndex].Note[NoteIndex].StartBeat) and
           (beat <= Lines[0].Line[LineIndex].Note[NoteIndex].StartBeat + Lines[0].Line[LineIndex].Note[NoteIndex].Duration) then
        begin
          //Result.part := 0;
          Result.line := LineIndex;
          Result.note := NoteIndex;
          Result.CP := 0;
          found := true;
          break;
        end;
      end;
    end;

    if found then //found exactly
      exit;

    if CurrentSong.isDuet and (PlayersPlay <> 1) then
    begin
      for LineIndex := 0 to High(Lines[1].Line) do
      begin
        for NoteIndex := 0 to High(Lines[1].Line[LineIndex].Note) do
        begin
          if (beat >= Lines[1].Line[LineIndex].Note[NoteIndex].StartBeat) and
            (beat <= Lines[1].Line[LineIndex].Note[NoteIndex].StartBeat + Lines[1].Line[LineIndex].Note[NoteIndex].Duration) then
          begin
            Result.CP := 1;
            Result.line := LineIndex;
            Result.note := NoteIndex;
            found := true;
            break;
          end;
        end;
      end;
    end;

    if found then //found exactly
      exit;

    min := high(integer);
    //second try (approximating)
    for LineIndex := 0 to High(Lines[0].Line) do
    begin
      for NoteIndex := 0 to High(Lines[0].Line[LineIndex].Note) do
      begin
        diff := abs(Lines[0].Line[LineIndex].Note[NoteIndex].StartBeat - beat);
        if diff < min then
        begin
          //Result.part := 0;
          Result.line := LineIndex;
          Result.note := NoteIndex;
          Result.CP := 0;
          min := diff;
        end;
      end;
    end;

    if CurrentSong.isDuet and (PlayersPlay <> 1) then
    begin
      for LineIndex := 0 to High(Lines[1].Line) do
      begin
        for NoteIndex := 0 to High(Lines[1].Line[LineIndex].Note) do
        begin
          diff := abs(Lines[1].Line[LineIndex].Note[NoteIndex].StartBeat - beat);
          if diff < min then
          begin
            Result.CP := 1;
            Result.line := LineIndex;
            Result.note := NoteIndex;
            min := diff;
          end;
        end;
      end;
    end;
  end; // function FindNote

begin
  inherited;
  // reset video playback engine
  CurrentTrack := 0;
  fCurrentVideo := nil;
  AudioPlayback.Stop;
  PlaySentence := false;
  PlayOne := false;
  PlaySentenceMidi := false;
  Text[TextDebug].Text := '';
  Log.LogStatus('Initializing', 'TEditScreen.OnShow');
  Xmouse := 0;

  ResetSingTemp;
  GoldenRec.KillAll;
//  SetLength(UndoSong, 0);
  SetLength(UndoLines, 0);
  SetLength(UndoStateNote, 0);
  SetLength(Undoheader, 0);

  try
    //Check if File is XML
    FileExt := CurrentSong.FileName.GetExtension;
    if FileExt.ToUTF8 = '.xml' then
      Error := not CurrentSong.LoadXMLSong()
    else
    begin
      // reread header with custom tags
      Error := not CurrentSong.Analyse(true, false);

      // with the duet/medley code, TSong.Analyse is already loading the song
      //if not Error then
      //  Error := not CurrentSong.LoadSong(false);
    end;
  except
    Error := true;
  end;

  if Error then
  begin
    //Error Loading Song -> Go back to Song Screen and Show some Error Message
    FadeTo(@ScreenSong);
    ScreenPopupError.ShowPopup (Language.Translate('ERROR_CORRUPT_SONG'));
    Exit;
  end
  else
  begin
  {$IFDEF UseMIDIPort}
    MidiOut := TMidiOutput.Create(nil);
    MidiOut.Open;
  {$ENDIF}

    if not Help.SetHelpID(ID) then
      Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenEditSub)');

    //    Text[TextTitle].Text :=   CurrentSong.Title;

    // Header Title
    SetLength(TitleVal, 1);
    TitleVal[0] := CurrentSong.Title;
    SlideTitleIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlideTitle,TitleSlideId,TitleVal,SlideTitleIndex);
    SelectsS[TitleSlideId].TextOpt[0].Align := 0;
    SelectsS[TitleSlideId].TextOpt[0].X := SelectsS[TitleSlideId].TextureSBG.X + 5;

    // Header Artist
    SetLength(ArtistVal, 1);
    ArtistVal[0] := CurrentSong.Artist;
    SlideArtistIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlideArtist,ArtistSlideId,ArtistVal,SlideArtistIndex);
    SelectsS[ArtistSlideId].TextOpt[0].Align := 0;
    SelectsS[ArtistSlideId].TextOpt[0].X := SelectsS[ArtistSlideId].TextureSBG.X + 5;

    // Header Language
    SetLength(LanguageVal, 1);
    LanguageVal[0] := ifthen(CurrentSong.Language <> 'Unknown', CurrentSong.Language, NOT_SET);
    SlideLanguageIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlideLanguage,LanguageSlideId,LanguageVal,SlideLanguageIndex);
    SelectsS[LanguageSlideId].TextOpt[0].Align := 0;
    SelectsS[LanguageSlideId].TextOpt[0].X := SelectsS[LanguageSlideId].TextureSBG.X + 5;

    // Header Edition
    SetLength(EditionVal, 1);
    EditionVal[0] := ifthen(CurrentSong.Edition <> 'Unknown', CurrentSong.Edition, NOT_SET);
    SlideEditionIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlideEdition,EditionSlideId,EditionVal,SlideEditionIndex);
    SelectsS[EditionSlideId].TextOpt[0].Align := 0;
    SelectsS[EditionSlideId].TextOpt[0].X := SelectsS[EditionSlideId].TextureSBG.X + 5;

    // Header Genre
    SetLength(GenreVal, 1);
    GenreVal[0] := ifthen(CurrentSong.Genre <> 'Unknown', CurrentSong.Genre, NOT_SET);
    SlideGenreIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlideGenre,GenreSlideId,GenreVal,SlideGenreIndex);
    SelectsS[GenreSlideId].TextOpt[0].Align := 0;
    SelectsS[GenreSlideId].TextOpt[0].X := SelectsS[GenreSlideId].TextureSBG.X + 5;

    // Header Year
    SetLength(YearVal, 1);
    YearVal[0] := ifthen(CurrentSong.Year <> 0, IntToStr(CurrentSong.Year), NOT_SET);
    SlideYearIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlideYear,YearSlideId,YearVal,SlideYearIndex);
    SelectsS[YearSlideId].TextOpt[0].Align := 0;
    SelectsS[YearSlideId].TextOpt[0].X := SelectsS[YearSlideId].TextureSBG.X + 5;

    // Header Creator
    SetLength(CreatorVal, 1);
    CreatorVal[0] := ifthen(CurrentSong.Creator <> '', CurrentSong.Creator, NOT_SET);
    SlideCreatorIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlideCreator,CreatorSlideId,CreatorVal,SlideCreatorIndex);
    SelectsS[CreatorSlideId].TextOpt[0].Align := 0;
    SelectsS[CreatorSlideId].TextOpt[0].X := SelectsS[CreatorSlideId].TextureSBG.X + 5;

    //Text[TextMp3].Text :=     CurrentSong.Mp3.ToUTF8;
    // Header MP3
    SetLength(MP3Val, 0);
    SetLength(Files, 0);
    SlideMP3Index := -1;

    for Ext in SUPPORTED_EXTS_AUDIO do
      Songs.FindFilesByExtension(Path(includeTrailingPathDelimiter(CurrentSong.Path.ToNative)), Path(Ext), true, Files);

    for i:=0 to high(Files) do
    begin
      SetLength(MP3Val, high(MP3Val)+2);
      MP3Val[i] := filesystem.ExtractFileName(files[i]).ToUTF8;
      if (UTF8CompareText(MP3Val[i],CurrentSong.Mp3.ToUTF8) = 0) then
            SlideMP3Index := i;
    end;
    UpdateSelectSlideOptions(Theme.EditSub.SlideMP3,MP3SlideId,MP3Val,SlideMP3Index);

    // Header Cover
    SetLength(Files, 0);
    SetLength(CoverVal, 0);
    SlideCoverIndex := -1;

    for Ext in SUPPORTED_EXTS_IMAGE do
      Songs.FindFilesByExtension(Path(includeTrailingPathDelimiter(CurrentSong.Path.ToNative)), Path(Ext), true, Files);

    for i:=0 to high(Files) do
    begin
      SetLength(CoverVal, high(CoverVal)+2);
      CoverVal[i] := ExtractFileName(files[i].ToUTF8());
      if UTF8CompareText(CoverVal[i], CurrentSong.Cover.ToUTF8) = 0 then
            SlideCoverIndex := i;
    end;
    if high(Files) < 0 then
    begin
      SetLength(CoverVal, 1);
      CoverVal[0] := CurrentSong.Cover.ToUTF8;
      SlideCoverIndex := 0;
    end;
    UpdateSelectSlideOptions(Theme.EditSub.SlideCover,CoverSlideId,CoverVal,SlideCoverIndex);

    // Header Background
    SetLength(Files, 0);
    SetLength(BackgroundVal, 0);
    SlideBackgroundIndex := -1;

    for Ext in SUPPORTED_EXTS_IMAGE do
      Songs.FindFilesByExtension(Path(includeTrailingPathDelimiter(CurrentSong.Path.ToNative)), Path(Ext), true, Files);

    for i:=0 to high(Files) do
    begin
      SetLength(BackgroundVal, high(BackgroundVal)+2);
      BackgroundVal[i] := ExtractFileName(files[i].ToUTF8());
      if UTF8CompareText(BackgroundVal[i], CurrentSong.Background.ToUTF8) = 0 then
            SlideBackgroundIndex := i;
    end;

    if high(Files) < 0 then
    begin
      SetLength(BackgroundVal, 1);
      BackgroundVal[0] := CurrentSong.Background.ToUTF8;
      SlideBackgroundIndex := 0;
    end;
    UpdateSelectSlideOptions(Theme.EditSub.SlideBackground,BackgroundSlideId,BackgroundVal,SlideBackgroundIndex);

    // Header Video
    SetLength(Files, 0);
    SetLength(VideoVal, 0);
    SlideVideoIndex := -1;
    for Ext in SUPPORTED_EXTS_VIDEO do
      Songs.FindFilesByExtension(Path(includeTrailingPathDelimiter(CurrentSong.Path.ToNative)), Path(Ext), true, Files);

    for i:=0 to high(Files) do
    begin
      SetLength(VideoVal, high(VideoVal)+2);
      VideoVal[i] := ExtractFileName(files[i].ToUTF8());
      if UTF8CompareText(VideoVal[i], CurrentSong.Video.ToUTF8) = 0 then
            SlideVideoIndex := i;
    end;

    if high(Files) < 0 then
    begin
      SetLength(VideoVal, 1);
      VideoVal[0] := CurrentSong.Video.ToUTF8;
      SlideVideoIndex := 0;
    end;
    UpdateSelectSlideOptions(Theme.EditSub.SlideVideo,VideoSlideId,VideoVal,SlideVideoIndex);

    // Header VideoGap
    SetLength(VideoGapVal, 1);
    VideoGapVal[0] := '';
    SlideVideoGapIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlideVideoGap,VideoGapSlideId,VideoGapVal,SlideVideoGapIndex);
    SelectsS[VideoGapSlideId].TextOpt[0].Align := 0;
    SelectsS[VideoGapSlideId].TextOpt[0].X := SelectsS[VideoGapSlideId].TextureSBG.X + 5;

//    SelectsS[BackgroundSlideId].TextOpt[0].Align := 0;

    // Header BPM
    SetLength(BPMVal, 1);
    BPMVal[0] := '';
    SlideBPMIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlideBPM,BPMSlideId,BPMVal,SlideBPMIndex);
    SelectsS[BPMSlideId].TextOpt[0].Align := 0;
    SelectsS[BPMSlideId].TextOpt[0].X := SelectsS[BPMSlideId].TextureSBG.X + 5;

    // Header GAP
    SetLength(GAPVal, 1);
    GAPVal[0] := '';
    SlideGAPIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlideGAP,GAPSlideId,GAPVal,SlideGAPIndex);
    SelectsS[GAPSlideId].TextOpt[0].Align := 0;
    SelectsS[GAPSlideId].TextOpt[0].X := SelectsS[GAPSlideId].TextureSBG.X + 5;

    // Header StartTag
    SetLength(StartTagVal, 1);
    StartTagVal[0] := '';
    SlideStartTagIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlideStartTag,StartTagSlideId,StartTagVal,SlideStartTagIndex);
    SelectsS[StartTagSlideId].TextOpt[0].Align := 0;
    SelectsS[StartTagSlideId].TextOpt[0].X := SelectsS[StartTagSlideId].TextureSBG.X + 5;

    // Header EndTag
    SetLength(EndTagVal, 1);
    EndTagVal[0] := '';
    SlideEndTagIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlideEndTag,EndTagSlideId,EndTagVal,SlideEndTagIndex);
    SelectsS[EndTagSlideId].TextOpt[0].Align := 0;
    SelectsS[EndTagSlideId].TextOpt[0].X := SelectsS[EndTagSlideId].TextureSBG.X + 5;

    // Header MedleyStart
    SetLength(MedleyStartVal, 1);
    MedleyStartVal[0] := '';
    SlideMedleyStartIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlideMedleyStart,MedleyStartSlideId,MedleyStartVal,SlideMedleyStartIndex);
    SelectsS[MedleyStartSlideId].TextOpt[0].Align := 0;
    SelectsS[MedleyStartSlideId].TextOpt[0].X := SelectsS[MedleyStartSlideId].TextureSBG.X + 5;

    // Header MedleyEnd
    SetLength(MedleyEndVal, 1);
    MedleyEndVal[0] := '';
    SlideMedleyEndIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlideMedleyEnd,MedleyEndSlideId,MedleyEndVal,SlideMedleyEndIndex);
    SelectsS[MedleyEndSlideId].TextOpt[0].Align := 0;
    SelectsS[MedleyEndSlideId].TextOpt[0].X := SelectsS[MedleyEndSlideId].TextureSBG.X + 5;

    // Header PreviewStart
    SetLength(PreviewStartVal, 1);
    PreviewStartVal[0] := '';
    SlidePreviewStartIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlidePreviewStart,PreviewStartSlideId,PreviewStartVal,SlidePreviewStartIndex);
    SelectsS[PreviewStartSlideId].TextOpt[0].Align := 0;
    SelectsS[PreviewStartSlideId].TextOpt[0].X := SelectsS[PreviewStartSlideId].TextureSBG.X + 5;

    // Header Relative
    SetLength(RelativeVal, 1);
    RelativeVal[0] := '';
    UpdateSelectSlideOptions(Theme.EditSub.SlideRelative,RelativeSlideId,RelativeVal,SlideRelativeIndex);
    SelectsS[RelativeSlideId].TextOpt[0].Align := 0;
    SelectsS[RelativeSlideId].TextOpt[0].X := SelectsS[RelativeSlideId].TextureSBG.X + 5;

    // Header Start
    SetLength(StartVal, 1);
    StartVal[0] := '';
    SlideStartIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlideStart,StartSlideId,StartVal,SlideStartIndex);
    SelectsS[StartSlideId].TextOpt[0].Align := 0;
    SelectsS[StartSlideId].TextOpt[0].X := SelectsS[StartSlideId].TextureSBG.X + 5;

    // Header Duration
    SetLength(DurationVal, 1);
    DurationVal[0] := '';
    SlideDurationIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlideDuration,DurationSlideId,DurationVal,SlideDurationIndex);
    SelectsS[DurationSlideId].TextOpt[0].Align := 0;
    SelectsS[DurationSlideId].TextOpt[0].X := SelectsS[DurationSlideId].TextureSBG.X + 5;

    // Header Tone
    SetLength(ToneVal, 1);
    ToneVal[0] := '';
    SlideDurationIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlideTone,ToneSlideId,ToneVal,SlideToneIndex);
    SelectsS[ToneSlideId].TextOpt[0].Align := 0;
    SelectsS[ToneSlideId].TextOpt[0].X := SelectsS[ToneSlideId].TextureSBG.X + 5;

    // Header Lyric
    SetLength(LyricVal, 1);
    LyricVal[0] := '';
    SlideLyricIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlideLyric,LyricSlideId,LyricVal,SlideLyricIndex);
    SelectsS[LyricSlideId].TextOpt[0].Align := 0;
    SelectsS[LyricSlideId].TextOpt[0].X := SelectsS[LyricSlideId].TextureSBG.X + 5;

    // volume slides
    SetLength(VolumeAudio, 0);
    SetLength(VolumeMidi, 0);
    SetLength(VolumeClick, 0);
    for i:=0 to 100 do
    begin
      SetLength(VolumeAudio, high(VolumeAudio)+2);
      SetLength(VolumeMidi, high(VolumeMidi)+2);
      SetLength(VolumeClick, high(VolumeClick)+2);
      VolumeAudio[i] := inttostr(i);
      VolumeMidi[i] := inttostr(i);
      VolumeClick[i] := inttostr(i);
    end;
    VolumeAudioIndex := 100;
    VolumeMidiIndex := 100;
    VolumeClickIndex := 100;
    UpdateSelectSlideOptions(Theme.EditSub.SelectVolAudio, VolumeAudioSlideId, VolumeAudio, VolumeAudioIndex);
    UpdateSelectSlideOptions(Theme.EditSub.SelectVolMidi, VolumeMidiSlideId, VolumeMidi, VolumeMidiIndex);
    UpdateSelectSlideOptions(Theme.EditSub.SelectVolClick, VolumeClickSlideId, VolumeClick, VolumeClickIndex);

    Lines[CurrentTrack].Current := 0;
    CurrentNote[CurrentTrack] := 0;
    Lines[CurrentTrack].Line[0].Note[0].Color := 2;
    AudioPlayBack.Open(CurrentSong.Path.Append(CurrentSong.Mp3));
    //Set Down Music Volume for Better hearability of Midi Sounds
    //Music.SetVolume(0.4);

    // finding the note for the Medley section
    MedleyNotes := Default(TMedleyNotes);
    if (CurrentSong.Medley.Source = msTag) then
    begin
      if (CurrentSong.Medley.EndBeat > 0) then MedleyNotes.end_ := FindNote(CurrentSong.Medley.EndBeat);
      if (CurrentSong.Medley.EndBeat > CurrentSong.Medley.StartBeat) then MedleyNotes.start := FindNote(CurrentSong.Medley.StartBeat);

      MedleyNotes.isEnd := (MedleyNotes.end_.line > 0) or (MedleyNotes.end_.note >= 0);
      MedleyNotes.isStart:= (MedleyNotes.start.line > 0) or (MedleyNotes.start.note >= 0);
    end;

    for TrackIndex := 0 to High(Lines) do
    begin
      EditorLyrics[TrackIndex].Clear;

      EditorLyrics[TrackIndex].X := Theme.EditSub.TextSentence.X;
      EditorLyrics[TrackIndex].Y := Theme.EditSub.TextSentence.Y;
      EditorLyrics[TrackIndex].Align := Theme.EditSub.TextSentence.Align;
      EditorLyrics[TrackIndex].Size := Theme.EditSub.TextSentence.Size;
      EditorLyrics[TrackIndex].ColR := Theme.EditSub.TextSentence.ColR;
      EditorLyrics[TrackIndex].ColG := Theme.EditSub.TextSentence.ColG;
      EditorLyrics[TrackIndex].ColB := Theme.EditSub.TextSentence.ColB;
      EditorLyrics[TrackIndex].DColR := Theme.EditSub.TextSentence.DColR;
      EditorLyrics[TrackIndex].DColG := Theme.EditSub.TextSentence.DColG;
      EditorLyrics[TrackIndex].DColB := Theme.EditSub.TextSentence.DColB;

      EditorLyrics[TrackIndex].AddLine(CurrentTrack, 0);
      EditorLyrics[TrackIndex].Selected := 0;
    end;

    NotesH[0] := 7;
    NotesW[0] := 4;
    resize_note_left := false;
    resize_note_right := false;
    move_note := false;
    //show transparent background for notes
    ShowInteractiveBackground;
    // user input tracking
    AudioInput.CaptureStart;
  end;

  // background picture
  try
    if (Not (CurrentSong.Background = PATH_NONE) and CurrentSong.Path.Append(CurrentSong.Background).Exists) then
    begin
      Tex_PrevBackground := Texture.LoadTexture(CurrentSong.Path.Append(CurrentSong.Background));
      Texture.AddTexture(Tex_PrevBackground, TEXTURE_TYPE_PLAIN, true);
      Statics[BackgroundImageId].Texture := Tex_PrevBackground;
      Statics[BackgroundImageId].Texture.X := theme.EditSub.BackgroundImage.X;
      Statics[BackgroundImageId].Texture.Y := theme.EditSub.BackgroundImage.Y;
      Statics[BackgroundImageId].Texture.W := theme.EditSub.BackgroundImage.W;
      Statics[BackgroundImageId].Texture.H := theme.EditSub.BackgroundImage.H;
    end;
  except
    Log.LogError('Background could not be loaded: ' + CurrentSong.Background.ToNative);
    Tex_PrevBackground.TexNum := 0;
  end;

  //Interaction := 0;
  TextEditMode := false;
  TitleEditMode := false;
  ArtistEditMode := false;
  LanguageEditMode := false;
  EditionEditMode := false;
  GenreEditMode := false;
  YearEditMode := false;
  CreatorEditMode := false;

  editLengthText := 0;
  TextPosition := -1;
end;

function TScreenEditSub.Draw: boolean;
const
  Padding: Integer = 20;
  NumLines: Integer = 10;
var
  Pet, i:    integer;
  lastline, note,Count: integer;
  notechange:          boolean;
begin
  //glClearColor(1,1,1,1);

  {$IFDEF UseMIDIPort} // midi music
  if PlaySentenceMidi and Not (PlayOneMidi) then
  begin
    MidiPos := USTime.GetTime - MidiTime + MidiStart;

    // stop the music
    if (MidiPos > MidiStop) then
    begin
      MidiOut.PutShort($B1, $7, floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
      MidiOut.PutShort(MIDI_NOTEOFF or 1, Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[MidiLastNote].Tone + 60, 127);
      PlaySentenceMidi := false;
    end;

    // click
    CurrentBeat := Floor(GetMidBeat(MidiPos - CurrentSong.GAP / 1000));
    Text[TextDebug].Text := Language.Translate('EDIT_INFO_CURRENT_BEAT') + ' ' + IntToStr(CurrentBeat);

    if CurrentBeat <> LastClick then
    begin
      for i := 0 to Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].HighNote do
        if (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[i].StartBeat = CurrentBeat) then
        begin

          LastClick := CurrentBeat;
          MidiOut.PutShort($B1, $7, floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
          if i > 0 then
            MidiOut.PutShort(MIDI_NOTEOFF or 1, Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[i-1].Tone + 60, 127);
          MidiOut.PutShort($91, Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[i].Tone + 60, 127);
          MidiLastNote := i;

        end;
    end;
  end; // if PlaySentenceMidi
  {$ENDIF}

  // move "cursor"
  if (PlaySentence or PlaySentenceMidi or PlayVideo) and not (PlayOne) then //and Not (PlayNote) then
  begin
    if (PlaySentence or PlayVideo) then
    begin
      CurrentBeat := Floor(GetMidBeat(AudioPlayback.Position - (CurrentSong.GAP) / 1000));
      Text[TextDebug].Text := Language.Translate('EDIT_INFO_CURRENT_BEAT') + ' ' + IntToStr(CurrentBeat);
    end;

    // store current line, find next line to given beat
    lastline := Lines[CurrentTrack].Current;
    while (Lines[CurrentTrack].Current < High(Lines[CurrentTrack].Line)) and (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat < CurrentBeat) do
      inc(Lines[CurrentTrack].Current);

    // only update lyric if line changes
    if Lines[CurrentTrack].Current <> lastline then
    begin
        Lines[CurrentTrack].Line[lastline].Note[CurrentNote[CurrentTrack]].Color := 1;
        EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Lines[CurrentTrack].Current);
        EditorLyrics[CurrentTrack].Selected := 0;
        CurrentNote[CurrentTrack] := 0;
        ShowInteractiveBackground;
        GoldenRec.KillAll;
    end;

    for note := CurrentNote[CurrentTrack] to High(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].note) do
      begin
        //note change
        if Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[note].StartBeat < CurrentBeat then
            begin
              Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 1;
              CurrentNote[CurrentTrack] := note;
              EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
              Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Color := 2;
            end; //if
      end; //for note}
  end; //end move cursor

  // mp3 music
  if (PlaySentence or PlayVideo or PlayOne) then
  begin
    // stop the music
    if (AudioPlayback.Position > PlayStopTime) then
    begin
    Log.LogError('STOP');
      AudioPlayback.Stop;
      PlaySentence := false;
      PlayOne := false;
      PlayVideo := false;
      StopVideoPreview;
    end;

    // click
    if (Click) and (PlaySentence) then
    begin
      //CurrentBeat := Floor(CurrentSong.BPM[0].BPM * (Music.Position - CurrentSong.GAP / 1000) / 60);
      CurrentBeat := Floor(GetMidBeat(AudioPlayback.Position - CurrentSong.GAP / 1000));
      Text[TextDebug].Text := Language.Translate('EDIT_INFO_CURRENT_BEAT') + ' ' + IntToStr(CurrentBeat);
      if CurrentBeat <> LastClick then
      begin
        for i := 0 to Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].HighNote do
          if (Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[i].StartBeat = CurrentBeat) then
          begin
            SoundLib.Click.Volume := SelectsS[VolumeClickSlideId].SelectedOption / 100;
            AudioPlayback.PlaySound( SoundLib.Click );
            LastClick := CurrentBeat;
          end;
      end;
    end; // click
  end; // if PlaySentence

  {$IFDEF UseMIDIPort} if PlayOneMidi then
  begin
    MidiPos := USTime.GetTime - MidiTime + MidiStart;
    // stop the music
    if ((MidiPos > MidiStop))  then // and (midinotefound)
    begin
      MidiOut.PutShort($B1, $7, floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
      MidiOut.PutShort($81, Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Tone + 60, 127);
      MidiOut.PutShort(MIDI_STOP, 0, 0);
      PlayOneMidi := false;
    end;

    // click
    CurrentBeat := Floor(GetMidBeat(MidiPos - CurrentSong.GAP / 1000));
    Text[TextDebug].Text := Language.Translate('EDIT_INFO_CURRENT_BEAT') + ' ' + IntToStr(CurrentBeat);

    if ((CurrentBeat <> LastClick) and Not (midinotefound)) then
    begin
//      for i := 0 to Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].HighNote do
//      begin
        if ((Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat <= CurrentBeat) and
        ((Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat + Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration) > CurrentBeat)) then
        begin
          LastClick := CurrentBeat;
          midinotefound := true;
          MidiOut.PutShort($B1, $7, floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
//          if i > 0 then
            MidiOut.PutShort($81, Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Tone + 60, 127);
          MidiOut.PutShort($91, Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Tone + 60, 127);

          MidiLastNote := i;
        end;
//      end;
    end;
  end; // if PlayOneNoteMidi
  {$ENDIF}

  Button[TextSentence].Text[0].Text := Language.Translate('EDIT_INFO_CURRENT_LINE') + ' ' + IntToStr(Lines[CurrentTrack].Current + 1) + ' / ' + IntToStr(Lines[CurrentTrack].Number);
  Button[TextNote].Text[0].Text :=  Language.Translate('EDIT_INFO_CURRENT_NOTE') + ' ' + IntToStr(CurrentNote[CurrentTrack] + 1) + ' / ' + IntToStr(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].HighNote + 1);

  // Song info
  // VideoGap
  VideoGapVal[0] := ifthen(CurrentSong.VideoGAP <> 0, FloatToStr(CurrentSong.VideoGAP) + ' s', NOT_SET);
  SelectsS[VideoGapSlideId].TextOpt[0].Text := VideoGapVal[0];
  // BPM
  BPMVal[0] := FloatToStr(CurrentSong.BPM[0].BPM / 4);
  SelectsS[BPMSlideId].TextOpt[0].Text := BPMVal[0];
  // GAP
  GAPVal[0] := FloatToStr(CurrentSong.GAP) + ' ms';
  SelectsS[GAPSlideId].TextOpt[0].Text := GAPVal[0];
  // StartTag
  StartTagVal[0] := ifthen(CurrentSong.Start > 0.0, FloatToStr(CurrentSong.Start) + ' s', NOT_SET);
  SelectsS[StartTagSlideId].TextOpt[0].Text := StartTagVal[0];
  // EndTag
  EndTagVal[0] := ifthen(CurrentSong.Finish > 0.0, FloatToStr(CurrentSong.Finish) + ' ms', NOT_SET);
  SelectsS[EndTagSlideId].TextOpt[0].Text := EndTagVal[0];
  // MedleyStart
  if (MedleyNotes.isStart) then
    MedleyStartVal[0] := IntToStr(Lines[CurrentTrack].Line[MedleyNotes.start.line].Note[MedleyNotes.start.note].StartBeat)
  else
    MedleyStartVal[0] := NOT_SET;
  SelectsS[MedleyStartSlideId].TextOpt[0].Text := MedleyStartVal[0];
  // MedleyEnd
  if (MedleyNotes.isEnd) then
    MedleyEndVal[0] := IntToStr(Lines[CurrentTrack].Line[MedleyNotes.end_.line].Note[MedleyNotes.end_.note].StartBeat + Lines[CurrentTrack].Line[MedleyNotes.end_.line].Note[MedleyNotes.end_.note].Duration)
  else
    MedleyEndVal[0] := NOT_SET;
  SelectsS[MedleyEndSlideId].TextOpt[0].Text := MedleyEndVal[0];
  // PreviewStart
  if (CurrentSong.HasPreview) then
    PreviewStartVal[0] := FloatToStr(CurrentSong.PreviewStart) + ' s'
  else
    PreviewStartVal[0] := NOT_SET;
  SelectsS[PreviewStartSlideId].TextOpt[0].Text := PreviewStartVal[0];
  if (CurrentSong.Relative) then
    RelativeVal[0] := Language.Translate('SONG_MENU_YES')
  else
    RelativeVal[0] := NOT_SET + ' (' + Language.Translate('SONG_MENU_NO') + ')';
  SelectsS[RelativeSlideId].TextOpt[0].Text := RelativeVal[0];

  //Error reading variables when no song is loaded
  if not (Error or TitleEditMode or TextEditMode) then
  begin
    // Note info
    //Text[TextNStart].Text :=    IntToStr(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Start);
    StartVal[0] := IntToStr(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].StartBeat);
    SelectsS[StartSlideId].TextOpt[0].Text := StartVal[0];
    //Text[TextNLength].Text :=  IntToStr(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Length);
    DurationVal[0] := IntToStr(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Duration);
    SelectsS[DurationSlideId].TextOpt[0].Text := DurationVal[0];
    //Text[TextNTon].Text :=      IntToStr(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Tone) + ' ( ' + GetNoteName(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Tone) + ' )';
    ToneVal[0] := IntToStr(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Tone) + ' (' + GetNoteName(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Tone) + ')';
    SelectsS[ToneSlideId].TextOpt[0].Text := ToneVal[0];
    //Text[TextNText].Text :=              Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Text;
    LyricVal[0] := Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[CurrentNote[CurrentTrack]].Text;
    SelectsS[LyricSlideId].TextOpt[0].Text := LyricVal[0];
  end;

  // Text Edit Mode
  if TextEditMode or TitleEditMode or ArtistEditMode or LanguageEditMode or EditionEditMode or GenreEditMode or YearEditMode or CreatorEditMode then
  begin
    if TextPosition >= 0 then
    SelectsS[CurrentSlideId].TextOpt[0].Text :=
      UTF8Copy(CurrentEditText, 1, TextPosition) + '|' + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
    editLengthText := LengthUTF8(SelectsS[CurrentSlideId].TextOpt[0].Text);
  end;

  // draw static menu
  DrawBG;

  // header background
  EditDrawBorderedBox(Theme.EditSub.HeaderBackground.X, Theme.EditSub.HeaderBackground.Y, Theme.EditSub.HeaderBackground.W, Theme.EditSub.HeaderBackground.H, Theme.EditSub.HeaderBackground.ColR, Theme.EditSub.HeaderBackground.ColG, Theme.EditSub.HeaderBackground.ColB, Theme.EditSub.HeaderBackground.Alpha);

  // currently selected note info background
  EditDrawBorderedBox(Theme.EditSub.CurrentNoteInfoBackground.X, Theme.EditSub.CurrentNoteInfoBackground.Y, Theme.EditSub.CurrentNoteInfoBackground.W, Theme.EditSub.CurrentNoteInfoBackground.H, Theme.EditSub.CurrentNoteInfoBackground.ColR, Theme.EditSub.CurrentNoteInfoBackground.ColG, Theme.EditSub.CurrentNoteInfoBackground.ColB, Theme.EditSub.CurrentNoteInfoBackground.Alpha);

  // volume slider background
  EditDrawBorderedBox(Theme.EditSub.VolumeSliderBackground.X, Theme.EditSub.VolumeSliderBackground.Y, Theme.EditSub.VolumeSliderBackground.W, Theme.EditSub.VolumeSliderBackground.H, Theme.EditSub.VolumeSliderBackground.ColR, Theme.EditSub.VolumeSliderBackground.ColG, Theme.EditSub.VolumeSliderBackground.ColB, Theme.EditSub.VolumeSliderBackground.Alpha);

  // P1 info bar
  DrawInfoBar(Theme.EditSub.P1InfoBarBackground.X, Theme.EditSub.P1InfoBarBackground.Y, Theme.EditSub.P1InfoBarBackground.W, Theme.EditSub.P1InfoBarBackground.H, Theme.EditSub.P1InfoBarBackground.ColR, Theme.EditSub.P1InfoBarBackground.ColG, Theme.EditSub.P1InfoBarBackground.ColB, Theme.EditSub.P1InfoBarBackground.Alpha, 0);

  // P2 info bar - for duet mode
  if (CurrentSong.isDuet) then
    DrawInfoBar(Theme.EditSub.P2InfoBarBackground.X, Theme.EditSub.P2InfoBarBackground.Y, Theme.EditSub.P2InfoBarBackground.W, Theme.EditSub.P2InfoBarBackground.H, Theme.EditSub.P2InfoBarBackground.ColR, Theme.EditSub.P2InfoBarBackground.ColG, Theme.EditSub.P2InfoBarBackground.ColB, Theme.EditSub.P2InfoBarBackground.Alpha, 1);

  // sentence background
  EditDrawBorderedBox(Theme.EditSub.SentenceBackground.X, Theme.EditSub.SentenceBackground.Y, Theme.EditSub.SentenceBackground.W, Theme.EditSub.SentenceBackground.H, Theme.EditSub.SentenceBackground.ColR, Theme.EditSub.SentenceBackground.ColG, Theme.EditSub.SentenceBackground.ColB, Theme.EditSub.SentenceBackground.Alpha);

  //inherited Draw;
  DrawFG;
  // draw notes

  //Error Drawing when no Song is loaded
  if not Error then
  begin
    if Xmouse < 0 then
    begin
      // notes background
      EditDrawBorderedBox(Theme.EditSub.NotesBackground.X, Theme.EditSub.NotesBackground.Y, Theme.EditSub.NotesBackground.W + Xmouse, Theme.EditSub.NotesBackground.H, Theme.EditSub.NotesBackground.ColR, Theme.EditSub.NotesBackground.ColG, Theme.EditSub.NotesBackground.ColB, Theme.EditSub.NotesBackground.Alpha);
      // horizontal lines
      SingDrawNoteLines(Theme.EditSub.NotesBackground.X, Theme.EditSub.NotesBackground.Y, Theme.EditSub.NotesBackground.X + Theme.EditSub.NotesBackground.W + Xmouse, 15);
      // vertical lines
      EditDrawBeatDelimiters(Theme.EditSub.NotesBackground.X + Padding, Theme.EditSub.NotesBackground.Y, Theme.EditSub.NotesBackground.W - 2 * Padding + Xmouse, Theme.EditSub.NotesBackground.H, CurrentTrack);
      // draw notes
      EditDrawLine(Theme.EditSub.NotesBackground.X + Padding, Theme.EditSub.NotesBackground.Y + (7/9) * Theme.EditSub.NotesBackground.H, Theme.EditSub.NotesBackground.W - 2 * Padding + Xmouse, Theme.EditSub.NotesBackground.W, CurrentTrack);
      // draw text on notes
      DrawText(Theme.EditSub.NotesBackground.X + Padding, Theme.EditSub.NotesBackground.Y + (7/9) * Theme.EditSub.NotesBackground.H, Theme.EditSub.NotesBackground.W - 2 * Padding + Xmouse, Theme.EditSub.NotesBackground.H, CurrentTrack);
    end
    else
    begin
      // notes background
      EditDrawBorderedBox(Theme.EditSub.NotesBackground.X + Xmouse, Theme.EditSub.NotesBackground.Y, Theme.EditSub.NotesBackground.W - Xmouse, Theme.EditSub.NotesBackground.H, Theme.EditSub.NotesBackground.ColR, Theme.EditSub.NotesBackground.ColG, Theme.EditSub.NotesBackground.ColB, Theme.EditSub.NotesBackground.Alpha);
      // horizontal lines
      SingDrawNoteLines(Theme.EditSub.NotesBackground.X + Xmouse, Theme.EditSub.NotesBackground.Y, Theme.EditSub.NotesBackground.X + Theme.EditSub.NotesBackground.W, 15);
      // vertical lines
      EditDrawBeatDelimiters(Theme.EditSub.NotesBackground.X + Padding + Xmouse, Theme.EditSub.NotesBackground.Y, Theme.EditSub.NotesBackground.W - 2 * Padding, Theme.EditSub.NotesBackground.H, CurrentTrack);
      // draw notes
      EditDrawLine(Theme.EditSub.NotesBackground.X + Padding + Xmouse, Theme.EditSub.NotesBackground.Y + (7/9) * Theme.EditSub.NotesBackground.H, Theme.EditSub.NotesBackground.W - 2 * Padding, Theme.EditSub.NotesBackground.H, CurrentTrack);
      // draw text on notes
      DrawText(Theme.EditSub.NotesBackground.X + Padding + Xmouse, Theme.EditSub.NotesBackground.Y + (7/9) * Theme.EditSub.NotesBackground.H, Theme.EditSub.NotesBackground.W - 2 * Padding, Theme.EditSub.NotesBackground.H, CurrentTrack);
    end;

    if Xmouse <> 0 then
       GoldenRec.KillAll;
  end;

  CurrentSound := AudioInputProcessor.Sound[0];
  CurrentSound.AnalyzeBuffer;
  if (CurrentSound.ToneString <> '-') then
  begin
    Count := trunc((720 / (GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].EndBeat) - GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[0].StartBeat)))*(AudioPlayback.Position - GetTimeFromBeat(Lines[CurrentTrack].Line[Lines[CurrentTrack].Current].Note[0].StartBeat)));
    // DrawPlayerTrack(0, 16, 32, 15, CurrentSound.Tone, Count, CurrentNote[CurrentTrack]); // FIXME: crash when switching from first to second track
  end;

  GoldenRec.SpawnRec;

  // draw text
  EditorLyrics[CurrentTrack].Draw;

  //video
  if Assigned(fCurrentVideo) then
  begin
    fCurrentVideo.GetFrame(CurrentSong.VideoGAP + AudioPlayback.Position);
    fCurrentVideo.SetScreen(1);
    fCurrentVideo.Alpha := 1;
    fCurrentVideo.SetScreenPosition(theme.EditSub.BackgroundImage.X, theme.EditSub.BackgroundImage.Y, 1);
    fCurrentVideo.Width := theme.EditSub.BackgroundImage.W;
    fCurrentVideo.Height := theme.EditSub.BackgroundImage.H;
    fCurrentVideo.ReflectionSpacing := 1;
    fCurrentVideo.AspectCorrection := acoCrop;

    fCurrentVideo.Draw;
  end;

  Result := true;
end;

procedure TScreenEditSub.OnHide;
var
i: integer;
begin
  {$IFDEF UseMIDIPort}
  MidiOut.Close;
  MidiOut.Free;
  {$ENDIF}

  //Music.SetVolume(1.0);
  AudioInput.CaptureStop;

end;

function TScreenEditSub.GetNoteName(Note: integer): string;
var
  N1, N2: integer;
begin
  if (Note > 0) then
  begin
    N1 := Note mod 12;
    N2 := Note div 12;
  end
  else
  begin
    N1 := (Note + (-Trunc(Note/12)+1)*12) mod 12;
    N2 := -1;
  end;

  case N1 of
    0: Result := 'c';
    1: Result := 'c#';
    2: Result := 'd';
    3: Result := 'd#';
    4: Result := 'e';
    5: Result := 'f';
    6: Result := 'f#';
    7: Result := 'g';
    8: Result := 'g#';
    9: Result := 'a';
    10: Result := 'a#/b';
    11: Result := 'b/h';
  end;

  case N2 of
    0: Result := UpperCase(Result); //Normal Uppercase Note, 1: Normal lowercase Note
    2: Result := Result + '''';     //One Striped
    3: Result := Result + '''''';   //Two Striped
    4: Result := Result + ''''''''; //etc.
    5: Result := Result + '''''''''';
    6: Result := Result + '''''''''''';
    7: Result := Result + '''''''''''''';
  end;
end;

function TScreenEditSub.GetMedleyLength: real;
begin
  if MedleyNotes.isStart and MedleyNotes.isEnd then
  begin
    Result := GetTimeFromBeat(
      Lines[CurrentTrack].Line[MedleyNotes.end_.line].Note[MedleyNotes.end_.note].StartBeat +
      Lines[CurrentTrack].Line[MedleyNotes.end_.line].Note[MedleyNotes.end_.note].Duration) -
      GetTimeFromBeat(Lines[CurrentTrack].Line[MedleyNotes.start.line].Note[MedleyNotes.start.note].StartBeat);
  end else
    Result := 0;
end;

procedure TScreenEditSub.UpdateMedleyInfo;
begin
  if not MedleyNotes.IsStart and not MedleyNotes.IsEnd then
    Text[TextDebug].Text := ''
  else if not MedleyNotes.IsStart then
    Text[TextDebug].Text := Format(Language.Translate('EDIT_INFO_NO_MEDLEY_START'), [ifthen(MedleyNotes.IsEnd, Format(Language.Translate('EDIT_INFO_MEDLEY_END'), [Lines[CurrentTrack].Line[MedleyNotes.end_.line].Note[MedleyNotes.end_.note].StartBeat + Lines[CurrentTrack].Line[MedleyNotes.end_.line].Note[MedleyNotes.end_.note].Duration]))])
  else if not MedleyNotes.IsEnd then
    Text[TextDebug].Text := Format(Language.Translate('EDIT_INFO_NO_MEDLEY_END'), [ifthen(MedleyNotes.IsStart, Format(Language.Translate('EDIT_INFO_MEDLEY_START'), [Lines[CurrentTrack].Line[MedleyNotes.start.line].Note[MedleyNotes.start.note].StartBeat]))])
  else
    Text[TextDebug].Text := Format(Language.Translate('EDIT_INFO_MEDLEY_LENGTH'), [GetMedleyLength, ifthen(MedleyNotes.isCustom, Language.Translate('EDIT_INFO_MEDLEY_CUSTOM'), Language.Translate('EDIT_INFO_MEDLEY_TXT'))]);

end;

end.
