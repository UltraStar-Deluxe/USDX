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
    start:    TPos;
    end_:     TPos;
    Preview:  TPos;
    isStart:  boolean;   //start beat is declared
    isEnd:    boolean;   //end beat is declared
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
      P1EditMode:              boolean;
      P2EditMode:              boolean;

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
      UndoLines:               array of array of TLines;
      UndoStateNote:           array of array of integer; //UNDO: note's position
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
      procedure CopySentence(SrcLine, DstLine: integer);
      procedure CopySentences(SrcLine, DstLine, Num: integer);
      procedure MakeSolo;
      procedure MakeDuet;
      function  DuetCopyLine: boolean;
      function  DuetMoveLine: boolean;
      procedure CopyLine(SrcTrack, SrcLine, DstTrack, DstLine: integer);
      procedure Refresh;
      procedure CopyToUndo; //copy current lines, mouse position and headers
      procedure CopyFromUndo; //undo last lines, mouse position and headers
      procedure DrawPlayerTrack(X, Y, W: real; Space: integer; CurrentTone: integer; Count: integer; CurrentNote: integer);
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
  NotesSkipX: integer = 20;
  LineSpacing: integer = 15;

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
    SResult := SaveSong(CurrentSong, Tracks, FilePath, boolean(Data));
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
  SDL_ModState: word;
  R:            real;
  SResult:      TSaveSongResult;
  i:            integer;
  HasPreview:   Boolean;
begin
  Result := true;

  if TextEditMode or
     TitleEditMode or
     ArtistEditMode or
     LanguageEditMode or
     EditionEditMode or
     GenreEditMode or
     YearEditMode or
     CreatorEditMode or
     P1EditMode or
     P2EditMode then
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
            SResult := SaveSong(CurrentSong, Tracks[CurrentTrack], CurrentSong.Path.Append(CurrentSong.FileName),
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
            (Length(Tracks[CurrentTrack].Lines)> MedleyNotes.end_.line) and
            (Length(Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes) > MedleyNotes.end_.note) and
            (Length(Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes) > MedleyNotes.start.note) then
          begin
            CurrentSong.Medley.Source := msTag;
            CurrentSong.Medley.StartBeat := Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes[MedleyNotes.start.note].StartBeat;
            CurrentSong.Medley.EndBeat := Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].StartBeat +
              Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].Duration;
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
            SResult := SaveSong(CurrentSong, Tracks, CurrentSong.Path.Append(CurrentSong.FileName), CurrentSong.Relative); //save with relative timings
          end else
          begin
            CurrentSong.Relative := false;
            SResult := SaveSong(CurrentSong, Tracks, CurrentSong.Path.Append(CurrentSong.FileName), CurrentSong.Relative); // save with absolute timings
          end;

          if (SResult = ssrOK) then // saving was successful
          begin
            Text[TextDebug].Text := Language.Translate('INFO_FILE_SAVED');
            SetLength(UndoLines, 0, High(Tracks)); //clear undo lines
            SetLength(UndoStateNote, 0, Length(Tracks)); //clear undo CurrentNote[CurrentTrack] state
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
            if (CurrentSong.HasPreview) and
               (CurrentTrack = MedleyNotes.Preview.track) and
               (Tracks[CurrentTrack].CurrentLine = MedleyNotes.Preview.line) and
               (CurrentNote[CurrentTrack] = MedleyNotes.Preview.note) and
               (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsStartPreview) then
            begin
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsStartPreview := false;
              CurrentSong.PreviewStart := 0;
              CurrentSong.HasPreview := false;
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_PREVIEW_CLEARED');
            end
            else
            begin
              // clear old IsStartPreview flag
              Tracks[MedleyNotes.Preview.track].Lines[MedleyNotes.Preview.line].Notes[MedleyNotes.Preview.note].IsStartPreview := false;

              // set preview start
              CurrentSong.PreviewStart := round(GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat) * 1000) / 1000;
              CurrentSong.HasPreview := CurrentSong.PreviewStart >= 0.0;
              if (CurrentSong.HasPreview) then
              begin
                MedleyNotes.Preview.track := CurrentTrack;
                MedleyNotes.Preview.line := Tracks[CurrentTrack].CurrentLine;
                MedleyNotes.Preview.note := CurrentNote[CurrentTrack];
                Tracks[MedleyNotes.Preview.track].Lines[MedleyNotes.Preview.line].Notes[MedleyNotes.Preview.note].IsStartPreview := true;
                Text[TextDebug].Text := Format(Language.Translate('EDIT_INFO_PREVIEW_SET'), [CurrentSong.PreviewStart]);
              end
              else
              begin
                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsStartPreview := false;

              end;
            end;
          end
          else if InRange(CurrentSong.PreviewStart, 0.0, AudioPlayback.Length) then
          begin
            if SDL_ModState = KMOD_LALT then
            begin // jump and play
              // simulate sentence switch to clear props
              PreviousSentence;

              Tracks[CurrentTrack].CurrentLine := 0; // update lyric

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
              i := 0; while (i <= Tracks[CurrentTrack].High) and (CurrentBeat > Tracks[CurrentTrack].Lines[i].EndBeat) do Inc(i);
              if i <= High(Tracks[CurrentTrack].Lines) then
              begin
                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
                Tracks[CurrentTrack].CurrentLine := i;

                // finding the right note
                CurrentNote[CurrentTrack] := 0;
                while (CurrentNote[CurrentTrack] <= Tracks[CurrentTrack].Lines[i].HighNote) and (CurrentBeat > Tracks[CurrentTrack].Lines[i].Notes[CurrentNote[CurrentTrack]].EndBeat) do Inc(CurrentNote[CurrentTrack]);

                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 2;
                EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
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
              if (Tracks[CurrentTrack].CurrentLine = MedleyNotes.end_.line) and (CurrentNote[CurrentTrack] = MedleyNotes.end_.note) then
              begin
                MedleyNotes.isEnd := false;
                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsMedley := false;
                Text[TextDebug].Text := Language.Translate('EDIT_INFO_MEDLEY_END_CLEARED');
              end else
              begin
                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsMedley := true;
                if (Length(Tracks[CurrentTrack].Lines) > MedleyNotes.end_.line) and
                  (Length(Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes) > MedleyNotes.end_.note) then
                  Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].IsMedley := false;
                MedleyNotes.end_.line := Tracks[CurrentTrack].CurrentLine;
                MedleyNotes.end_.note := CurrentNote[CurrentTrack];
                Text[TextDebug].Text := Language.Translate('EDIT_INFO_MEDLEY_END_SET');
              end;
            end else
            begin
              MedleyNotes.isEnd := true;
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsMedley := true;
              MedleyNotes.end_.line := Tracks[CurrentTrack].CurrentLine;
              MedleyNotes.end_.note := CurrentNote[CurrentTrack];
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_MEDLEY_END_SET');
            end;
          end else
          begin        //Medley Start Note
            if MedleyNotes.isStart then
            begin
              if (Tracks[CurrentTrack].CurrentLine = MedleyNotes.start.line) and (CurrentNote[CurrentTrack] = MedleyNotes.start.note) then
              begin
                MedleyNotes.isStart := false;
                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsMedley := false;
                Text[TextDebug].Text := Language.Translate('EDIT_INFO_MEDLEY_START_CLEARED');
              end else
              begin
                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsMedley := true;
                if (Length(Tracks[CurrentTrack].Lines) > MedleyNotes.start.line) and
                  (Length(Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes) > MedleyNotes.start.note) then
                  Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes[MedleyNotes.start.note].IsMedley := false;
                MedleyNotes.start.line := Tracks[CurrentTrack].CurrentLine;
                MedleyNotes.start.note := CurrentNote[CurrentTrack];
                Text[TextDebug].Text := Language.Translate('EDIT_INFO_MEDLEY_START_SET');
              end;
            end else
            begin
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsMedley := true;
              MedleyNotes.isStart := true;
              MedleyNotes.start.line := Tracks[CurrentTrack].CurrentLine;
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

            if (Length(Tracks[CurrentTrack].Lines) > MedleyNotes.end_.line) and
               (Length(Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes) > MedleyNotes.end_.note) then
            begin
              Tracks[CurrentTrack].CurrentLine := MedleyNotes.end_.line;
              CurrentNote[CurrentTrack] := MedleyNotes.end_.note;
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 2;

              EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
              EditorLyrics[CurrentTrack].Selected := 0;
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_JUMPTO_MEDLEY_END');
            end;
          end else if MedleyNotes.IsStart then
          begin
            // simulate sentence switch to clear props
            PreviousSentence;

            if (Length(Tracks[CurrentTrack].Lines)> MedleyNotes.start.line) and
               (Length(Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes) > MedleyNotes.start.note) then
            begin
              Tracks[CurrentTrack].CurrentLine := MedleyNotes.start.line;
              CurrentNote[CurrentTrack] := MedleyNotes.start.note;
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 2;

              EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
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
              (Length(Tracks[CurrentTrack].Lines)> MedleyNotes.end_.line) and
              (Length(Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes)>MedleyNotes.end_.note) and
              (Length(Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes)>MedleyNotes.start.note) then
            begin
              R := GetTimeFromBeat(Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes[MedleyNotes.start.note].StartBeat);
              if InRange(R, 0.0, AudioPlayback.Length) then
              begin
                AudioPlayback.Position:= R;
                PlayStopTime := GetTimeFromBeat(
                  Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].StartBeat +
                  Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].Duration);
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
            EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
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
            with Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine] do
            begin
              Notes[CurrentNote[CurrentTrack]].Color := 1;
              CurrentNote[CurrentTrack] := 0;
              AudioPlayback.Position := GetTimeFromBeat(Notes[0].StartBeat);
              PlayStopTime := ifthen(SDL_ModState = KMOD_LALT,
                                   GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].High].EndBeat),
                                   GetTimeFromBeat(Notes[High(Notes)].EndBeat));
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
            if Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote >= Tracks[CurrentTrack].Lines[CopySrc].HighNote then
            begin
              PasteText;
              EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_PASTE_TEXT');
            end
            else
              Log.LogStatus('PasteText: invalid range', 'TScreenEditSub.ParseInput');
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then
          begin
            CopyToUndo;
            CopySentence(CopySrc, Tracks[CurrentTrack].CurrentLine);
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
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
            CurrentNote[CurrentTrack] := 0;
            R := GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat);
            if R <= AudioPlayback.Length then
            begin
              AudioPlayback.Position := R;
              PlayStopTime := GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat);
              PlaySentence := true;
              AudioPlayback.Play;
              LastClick := -100;
            end;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE_AUDIO');
          end
          else if SDL_ModState = KMOD_LSHIFT then
          begin
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
            CurrentNote[CurrentTrack] := 0;
            PlaySentenceMidi := true;
            PlayVideo := false;
            StopVideoPreview;
            {$IFDEF UseMIDIPort} MidiTime := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat);
            MidiStop := GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat); {$ENDIF}

            LastClick := -100;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE_MIDI');
          end
          else if SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL then
          begin
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
            CurrentNote[CurrentTrack] := 0;
            PlaySentenceMidi := true;
            PlayVideo := false;
            StopVideoPreview;
            {$IFDEF UseMIDIPort} MidiTime  := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat);
            MidiStop  := GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat); {$ENDIF}
            
            LastClick := -100;

            PlaySentence := true;
            Click := true;
            AudioPlayback.Stop;
            AudioPlayback.Position := GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat)+0{-0.10};
            PlayStopTime := GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat)+0;
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
          if (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType = ntGolden) then
          begin
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntRapGolden;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_RAPGOLDEN');
          end
          else if (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType = ntRapGolden) then
          begin
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntNormal;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_NORMAL');
          end
          else
          begin
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntGolden;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_GOLDEN');
          end;
          GoldenRec.KillAll;
          Exit;
        end;

      // Freestyle Note
      SDLK_F:
        begin
          CopyToUndo;
          if (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType = ntFreestyle) then
          begin
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntRap;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_RAP');
          end
          else if (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType = ntRap) then
          begin
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntNormal;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_NORMAL');
          end
          else
          begin
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntFreestyle;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_FREESTYLE');
          end;
          GoldenRec.KillAll;

          // update lyrics
          EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
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
          Inc(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration);
          if CurrentNote[CurrentTrack] = Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote then
          begin
            Inc(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat);
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
            CopySentence(CopySrc, Tracks[CurrentTrack].CurrentLine);
            CopySentence(CopySrc+1, Tracks[CurrentTrack].CurrentLine+1);
            CopySentence(CopySrc+2, Tracks[CurrentTrack].CurrentLine+2);
            CopySentence(CopySrc+3, Tracks[CurrentTrack].CurrentLine+3);
            GoldenRec.KillAll;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_PASTE_4_SENTENCES');
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then
          begin
            CopyToUndo;
            CopySentences(CopySrc, Tracks[CurrentTrack].CurrentLine, 4);
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
            CopySentence(CopySrc, Tracks[CurrentTrack].CurrentLine);
            CopySentence(CopySrc+1, Tracks[CurrentTrack].CurrentLine+1);
            CopySentence(CopySrc+2, Tracks[CurrentTrack].CurrentLine+2);
            CopySentence(CopySrc+3, Tracks[CurrentTrack].CurrentLine+3);
            CopySentence(CopySrc+4, Tracks[CurrentTrack].CurrentLine+4);
            GoldenRec.KillAll;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_PASTE_5_SENTENCES');
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then
          begin
            CopyToUndo;
            CopySentences(CopySrc, Tracks[CurrentTrack].CurrentLine, 5);
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
            if Tracks[CurrentTrack].CurrentLine < Tracks[CurrentTrack].High then
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
            EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
            EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_DIVIDED');
            GoldenRec.KillAll;
          end;
        ShowInteractiveBackground;
        end;

      SDLK_F4:
        begin
          // Enter Text Edit Mode
          BackupEditText := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text;
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
            AudioPlayback.Position := GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);
            PlayStopTime := (GetTimeFromBeat(
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat +
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration));
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
            MidiStart := GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);
            MidiStop := GetTimeFromBeat(
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat +
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration); {$ENDIF}
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
           // Interaction = 16 // PreviewSlideId
           // Interaction = 17 // RelativeSlideId

           if Interaction = MedleyStartSlideId then
           begin
             BackupEditText := ifthen(CurrentSong.DuetNames[0] <> '', CurrentSong.DuetNames[0], NOT_SET);
             CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
             editLengthText := LengthUTF8(BackupEditText);
             CurrentSlideId := MedleyStartSlideId;
             TextPosition := LengthUTF8(BackupEditText);
             P1EditMode := true;
           end;

           if Interaction = MedleyEndSlideId then
           begin
             BackupEditText := ifthen(CurrentSong.DuetNames[1] <> '', CurrentSong.DuetNames[1], NOT_SET);
             CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
             editLengthText := LengthUTF8(BackupEditText);
             CurrentSlideId := MedleyEndSlideId;
             TextPosition := LengthUTF8(BackupEditText);
             P2EditMode := true;
           end;

           // Interaction = 20 // StartSlideId
           // Interaction = 21 // DurationSlideId
           // Interaction = 22 // ToneSlideId

           if Interaction = LyricSlideId then
           begin
             BackupEditText := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text;
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
             if (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType = ntFreestyle) then
             begin
               Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntRap;
             end
             else if (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType = ntRap) then
             begin
               Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntNormal;
             end
             else
             begin
               Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntFreestyle;
             end;
             GoldenRec.KillAll;

             // update lyrics
             EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
             EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
             Exit;
           end;

           if Interaction = 28 then // GoldButtonID
           begin
             CopyToUndo;
             if (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType = ntGolden) then
             begin
               Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntRapGolden;
             end
             else if (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType = ntRapGolden) then
             begin
               Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntNormal;
             end
             else
             begin
               Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntGolden;
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
             Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
             CurrentNote[CurrentTrack] := 0;
             R := GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat);
             if R <= AudioPlayback.Length then
             begin
               AudioPlayback.Position := R;
               PlayStopTime := GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat);
               PlaySentence := true;
               AudioPlayback.Play;
               LastClick := -100;
             end;
             Text[TextDebug].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE');
           end;

           if Interaction = 30 then // PlayWithNoteButtonID
           begin
             Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
             CurrentNote[CurrentTrack] := 0;
             PlaySentenceMidi := true;
             PlayVideo := false;
             StopVideoPreview;
             {$IFDEF UseMIDIPort} MidiTime  := USTime.GetTime;
             MidiStart := GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat);
             MidiStop  := GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat); {$ENDIF}
             LastClick := -100;

             PlaySentence := true;
             Click := true;
             AudioPlayback.Stop;
             AudioPlayback.Position := GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat)+0{-0.10};
             PlayStopTime := GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat)+0;
             AudioPlayback.Play;
             LastClick := -100;
             Text[TextDebug].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE');
           end;

           if Interaction = 31 then // PlayNoteButtonID
           begin
             Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
             CurrentNote[CurrentTrack] := 0;
             PlaySentenceMidi := true;
             PlayVideo := false;
             StopVideoPreview;
             {$IFDEF UseMIDIPort} MidiTime := USTime.GetTime;
             MidiStart := GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat);
             MidiStop := GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat); {$ENDIF}

             LastClick := -100;
             Text[TextDebug].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE');
           end;

           for i := 0 to Tracks[CurrentTrack].High do
           begin
              if Interaction = InteractiveLineId[i] then
              begin
                CopyToUndo;
                GoldenRec.KillAll;
                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
                Tracks[CurrentTrack].CurrentLine := i;
                ShowInteractiveBackground;
                CurrentNote[CurrentTrack] := 0;
                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 2;
                EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
                EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
              end;
           end;

           if high(InteractiveNoteId) >= Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote then
           for i := 0 to Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote do
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

                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
                CurrentNote[CurrentTrack] := i;
                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 2;
                EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
                //play current note playonewithmidi
                PlaySentenceMidi := false;
                midinotefound := false;
                PlayOne := true;
                PlayOneMidi := true;
                {$IFDEF UseMIDIPort} MidiTime := USTime.GetTime;
                MidiStart := GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);
                MidiStop := GetTimeFromBeat(
                  Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat +
                  Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration); {$ENDIF}

                // playone
                PlayVideo := false;
                StopVideoPreview;
                Click := false;
                AudioPlayback.Stop;
                AudioPlayback.Position := GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);
                PlayStopTime := (GetTimeFromBeat(
                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat +
                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration));
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

          if (SDL_ModState = KMOD_LCTRL or KMOD_LSHIFT) then
          begin
            // deletes current sentence
            CopyToUndo;
            DeleteSentence;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_DELETE_SENTENCE');
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
            //MidiOut.PutShort($81, Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].Current].Notes[MidiLastNote].Tone + 60, 127);
            PlaySentenceMidi := false;
            {$endif}
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
            Inc(CurrentNote[CurrentTrack]);
            if CurrentNote[CurrentTrack] > Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote then
              CurrentNote[CurrentTrack] := 0;
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 2;
            EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
          end;

          // ctrl + right
          if SDL_ModState = KMOD_LCTRL then
          begin
            if Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration > 1 then
            begin
              Dec(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration);
              Inc(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);
              if CurrentNote[CurrentTrack] = 0 then
              begin
                Inc(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].StartBeat);
              end;
            end;
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_SHORTENED_AT_START');
            GoldenRec.KillAll;
          end;

          // shift + right
          if SDL_ModState = KMOD_LSHIFT then
          begin
            Inc(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);
            if CurrentNote[CurrentTrack] = 0 then
            begin
              Inc(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].StartBeat);
            end;
            if CurrentNote[CurrentTrack] = Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote then
              Inc(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat);
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_SHIFTED_RIGHT');
            GoldenRec.KillAll;
          end;

          // alt + right
          if SDL_ModState = KMOD_LALT then
          begin
            Inc(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration);
            if CurrentNote[CurrentTrack] = Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote then
            begin
              Inc(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat);
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
            //MidiOut.PutShort($81, Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].Current].Notes[MidiLastNote].Tone + 60, 127);
            PlaySentenceMidi := false;
            {$endif}

            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
            Dec(CurrentNote[CurrentTrack]);
            if CurrentNote[CurrentTrack] = -1 then
              CurrentNote[CurrentTrack] := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote;
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 2;
            EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
          end;

          // ctrl + left
          if SDL_ModState = KMOD_LCTRL then
          begin
            Dec(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);
            Inc(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration);
            if CurrentNote[CurrentTrack] = 0 then
              Dec(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].StartBeat);
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_LENGTHENED_AT_START');
            GoldenRec.KillAll;
          end;

          // shift + left
          if SDL_ModState = KMOD_LSHIFT then
          begin
            Dec(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);

            // resizing sentences
            if CurrentNote[CurrentTrack] = 0 then
            begin
              Dec(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].StartBeat);
            end;

            if CurrentNote[CurrentTrack] = Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote then
              Dec(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat);
            Text[TextDebug].Text := Language.Translate('EDIT_INFO_NOTE_SHIFTED_LEFT');
            GoldenRec.KillAll;
          end;

          // alt + left
          if SDL_ModState = KMOD_LALT then
          begin
            if Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration > 1 then
            begin
              Dec(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration);
              if CurrentNote[CurrentTrack] = Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote then
              begin
                Dec(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat);
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
          if ((SDL_ModState = KMOD_LCTRL) or (SDL_ModState = KMOD_LALT)) and (CurrentSong.isDuet) then
          begin
            if (Length(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes) > 0) then
            begin
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 0;
              CurrentTrack := 1;
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 2;
              EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
              EditorLyrics[CurrentTrack].Selected := 0;
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_SWITCHED_TO_TRACK') + ' 2 (' + CurrentSong.DuetNames[CurrentTrack] + ')';
            end;
          end;

          // copy line from first to second track
          if (CurrentSong.isDuet) and (CurrentTrack = 0) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) then
          begin
            CopyToUndo;
            if (DuetCopyLine) then
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_LINE_COPIED_TO_TRACK') + ' 2'
            else
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_LINE_NOT_COPIED');
          end;

          // move line from first to second track
          if (CurrentSong.isDuet) and (CurrentTrack = 0) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL or KMOD_LALT) then
          begin
            CopyToUndo;
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
          if ((SDL_ModState = KMOD_LCTRL) or (SDL_ModState = KMOD_LALT)) and (CurrentSong.isDuet) then
          begin
            if (Length(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes) > 0) then
            begin
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 0;
              CurrentTrack := 0;
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 2;
              EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
              EditorLyrics[CurrentTrack].Selected := 0;
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_SWITCHED_TO_TRACK') + ' 1 (' + CurrentSong.DuetNames[CurrentTrack] + ')';
            end;
          end;

          // copy line from second to first track
          if (CurrentSong.isDuet) and (CurrentTrack = 1) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) then
          begin
            CopyToUndo;
            if (DuetCopyLine) then
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_LINE_COPIED_TO_TRACK') + ' 1'
            else
              Text[TextDebug].Text := Language.Translate('EDIT_INFO_LINE_NOT_COPIED');
          end;

          // move line from second to first track
          if (CurrentSong.isDuet) and (CurrentTrack = 1) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL or KMOD_LALT) then
          begin
            CopyToUndo;
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
        Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text := CurrentEditText;
        EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
        EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
        end;
      Exit;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE:
        begin
          if TextEditMode then Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text := BackupEditText;
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
          if P1EditMode then
          begin
            CurrentSong.DuetNames[0] := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
            SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
          end;
          if P2EditMode then
          begin
            CurrentSong.DuetNames[1] := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
            SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
          end;
          EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
          EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
          TextEditMode := false;
          TitleEditMode := false;
          ArtistEditMode := false;
          LanguageEditMode := false;
          EditionEditMode := false;
          GenreEditMode := false;
          YearEditMode := false;
          CreatorEditMode := false;
          P1EditMode := false;
          P2EditMode := false;
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
          if P1EditMode then
          begin
            CurrentSong.DuetNames[0] := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            MedleyStartVal[0] := ifthen(CurrentSong.DuetNames[0] <> '', CurrentSong.DuetNames[0], NOT_SET);
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
            UpdateSelectSlideOptions(Theme.EditSub.SlideMedleyStart,MedleyStartSlideId,MedleyStartVal,SlideMedleyStartIndex);
            SelectsS[MedleyStartSlideId].TextOpt[0].Align := 0;
            SelectsS[MedleyStartSlideId].TextOpt[0].X := SelectsS[MedleyStartSlideId].TextureSBG.X + 5;
          end;
          if P2EditMode then
          begin
            CurrentSong.DuetNames[1] := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            MedleyEndVal[0] := ifthen(CurrentSong.DuetNames[1] <> '', CurrentSong.DuetNames[1], NOT_SET);
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
            UpdateSelectSlideOptions(Theme.EditSub.SlideMedleyEnd,MedleyEndSlideId,MedleyEndVal,SlideMedleyEndIndex);
            SelectsS[MedleyEndSlideId].TextOpt[0].Align := 0;
            SelectsS[MedleyEndSlideId].TextOpt[0].X := SelectsS[MedleyEndSlideId].TextureSBG.X + 5;
          end;
          if TextEditMode then
          begin
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            LyricVal[0] := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text;
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
            UpdateSelectSlideOptions(Theme.EditSub.SlideLyric,LyricSlideId,LyricVal,SlideLyricIndex);
            SelectsS[LyricSlideId].TextOpt[0].Align := 0;
            SelectsS[LyricSlideId].TextOpt[0].X := SelectsS[LyricSlideId].TextureSBG.X + 5;
            EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
          end;

          EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
          TitleEditMode := false;
          ArtistEditMode := false;
          LanguageEditMode := false;
          EditionEditMode := false;
          GenreEditMode := false;
          YearEditMode := false;
          CreatorEditMode := false;
          P1EditMode := false;
          P2EditMode := false;
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
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
              EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
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
            EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
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
    tempR := 720 / (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat - Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat);
    if (MouseButton = 0) and (LastPressedMouseType = SDL_BUTTON_RIGHT) and (PressedNoteId >=0) then
    begin
      // left & right
      if (Floor((CurrentX-40)/tempr) > Floor((LastX-40)/tempr)) or  (Floor((CurrentX-40)/tempr) < Floor((LastX-40)/tempr)) then
      begin
        CopyToUndo;
        i := floor((currentx-40) / floor(tempr)) - floor((lastx-40) / floor(tempr));
        if move_note then
          MoveAllToEnd(i);
        if (resize_note_right) and (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration + i > 0) then
        begin
          MoveAllToEnd(i);
          Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat - i;
          Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration + i;
        end;
        if (resize_note_left) and (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration - i > 0) then
        begin
          Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat + i;
          Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration - i;
          if CurrentNote[CurrentTrack] = 0 then
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].StartBeat := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].StartBeat - i;
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
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat +i;
            if CurrentNote[CurrentTrack] = 0 then
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].StartBeat := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].StartBeat - i;
            if CurrentNote[CurrentTrack] = Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote then
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat + i;
        end;
        // resize note
        if (resize_note_right) and (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration + i > 0) then
        begin
          Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration + i;
          if CurrentNote[CurrentTrack] = Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote then
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat + i;
        end;
        if (resize_note_left) and (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration - i > 0) then
        begin
          Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat + i;
          Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration - i;
          if CurrentNote[CurrentTrack] = 0 then
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].StartBeat := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].StartBeat + i;
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
  for Pet := 0 to Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].Current].HighNut do
    if (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].Current].Notes[Pet].Start = Czas.CurrentBeat) then
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
  for TrackIndex := 0 to High(Tracks) do
  begin
    for LineIndex := 0 to Tracks[TrackIndex].High do
    begin
      Tracks[TrackIndex].Lines[LineIndex].StartBeat := Tracks[TrackIndex].Lines[LineIndex].StartBeat div 2;
      Tracks[TrackIndex].Lines[LineIndex].EndBeat  := Tracks[TrackIndex].Lines[LineIndex].EndBeat div 2;
      for NoteIndex := 0 to Tracks[TrackIndex].Lines[LineIndex].HighNote do
      begin
        Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat  := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat div 2;
        Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration := Round(Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration / 2);
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
  for TrackIndex := 0 to High(Tracks) do
  begin
    for LineIndex := 0 to Tracks[TrackIndex].High do
    begin
      Tracks[TrackIndex].Lines[LineIndex].StartBeat := Tracks[TrackIndex].Lines[LineIndex].StartBeat * 2;
      Tracks[TrackIndex].Lines[LineIndex].EndBeat := Tracks[TrackIndex].Lines[LineIndex].EndBeat * 2;
      for NoteIndex := 0 to Tracks[TrackIndex].Lines[LineIndex].HighNote do
      begin
        Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat * 2;
        Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration * 2;
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
  for LineIndex := 0 to Tracks[TrackIndex].High do
    for NoteIndex := 0 to Tracks[TrackIndex].Lines[LineIndex].HighNote do
      Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text := UTF8LowerCase(Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text);
  }

  for TrackIndex := 0 to High(Tracks) do
  begin
    for LineIndex := 0 to Tracks[TrackIndex].High do
    begin
      Str := UTF8UpperCase(UTF8Copy(TrimLeft(Tracks[TrackIndex].Lines[LineIndex].Notes[0].Text), 1, 1));
      Str := Str + UTF8Copy(TrimLeft(Tracks[TrackIndex].Lines[LineIndex].Notes[0].Text), 2, Length(Tracks[TrackIndex].Lines[LineIndex].Notes[0].Text)-1);
      Tracks[TrackIndex].Lines[LineIndex].Notes[0].Text := Str;
    end; // LineIndex
  end; // TrackIndex
end;

procedure TScreenEditSub.LyricsCorrectSpaces;
var
  TrackIndex:   Integer;
  LineIndex:    Integer;
  NoteIndex:    Integer;
begin
  for TrackIndex := 0 to High(Tracks) do
  begin
    for LineIndex := 0 to Tracks[TrackIndex].High do
    begin
      // correct starting spaces in the first word
      while Copy(Tracks[TrackIndex].Lines[LineIndex].Notes[0].Text, 1, 1) = ' ' do
        Tracks[TrackIndex].Lines[LineIndex].Notes[0].Text := Copy(Tracks[TrackIndex].Lines[LineIndex].Notes[0].Text, 2, 100);

      // move spaces on the start to the end of the previous note
      for NoteIndex := 1 to Tracks[TrackIndex].Lines[LineIndex].HighNote do
      begin
        while (Copy(Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text, 1, 1) = ' ') do
        begin
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text := Copy(Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text, 2, 100);
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex-1].Text := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex-1].Text + ' ';
        end;
      end; // NoteIndex

      // correct '-'  to '- '
      for NoteIndex := 0 to Tracks[TrackIndex].Lines[LineIndex].HighNote do
      begin
        if Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text = '-' then
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text := '- ';
      end; // NoteIndex

      // add space to the previous note when the current word is '- '
      for NoteIndex := 1 to Tracks[TrackIndex].Lines[LineIndex].HighNote do
      begin
        if Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text  = '- ' then
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex-1].Text := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex-1].Text + ' ';
      end; // NoteIndex

      // correct too many spaces at the end of note
      for NoteIndex := 0 to Tracks[TrackIndex].Lines[LineIndex].HighNote do
      begin
        while Copy(Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text, Length(Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text)-1, 2) = '  ' do
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text := Copy(Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text, 1, Length(Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text)-1);
      end; // NoteIndex

      // and correct if there is no space at the end of sentence
      NoteIndex := Tracks[TrackIndex].Lines[LineIndex].HighNote;
      if Copy(Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text, Length(Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text), 1) <> ' ' then
        Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text + ' ';
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
  FirstBeat := High(integer);

  for TrackIndex := 0 to High(Tracks) do
    if (Tracks[TrackIndex].Lines[0].Notes[0].StartBeat < FirstBeat) then
    FirstBeat := Tracks[TrackIndex].Lines[0].Notes[0].StartBeat;

  // set first note to start at beat 0 (common practice)
  if (FirstBeat <> 0) then
  begin
    for TrackIndex := 0 to High(Tracks) do
      for LineIndex := 0 to Tracks[TrackIndex].High do
        for NoteIndex := 0 to Tracks[TrackIndex].Lines[LineIndex].HighNote do
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat - FirstBeat;

    // adjust GAP accordingly
    CurrentSong.GAP := round((CurrentSong.GAP + (FirstBeat * 15000) / CurrentSong.BPM[0].BPM) * 100) / 100;

    // adjust medley tags accordingly
    if (MedleyNotes.isStart) then
      CurrentSong.Medley.StartBeat := CurrentSong.Medley.StartBeat - FirstBeat;
    if (MedleyNotes.isEnd) then
      CurrentSong.Medley.EndBeat := CurrentSong.Medley.EndBeat - FirstBeat;
  end;

  // adjust line break timings
  for TrackIndex := 0 to High(Tracks) do
  begin
    for LineIndex := 1 to Tracks[TrackIndex].High do
    begin
      with Tracks[TrackIndex].Lines[LineIndex-1] do
      begin
        Min := Notes[HighNote].StartBeat + Notes[HighNote].Duration;
        Max := Tracks[TrackIndex].Lines[LineIndex].Notes[0].StartBeat;
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

        Tracks[TrackIndex].Lines[LineIndex].StartBeat := LineStart;
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
  LineLength := Length(Tracks[CurrentTrack].Lines);
  SetLength(Tracks[CurrentTrack].Lines, LineLength + 1);
  Inc(Tracks[CurrentTrack].Number);
  Inc(Tracks[CurrentTrack].High);

  // move needed sentences to one forward. newly has the copy of divided sentence
  LineStart := Tracks[CurrentTrack].CurrentLine;
  for LineIndex := LineLength-1 downto LineStart do
    CopyLine(CurrentTrack, LineIndex, CurrentTrack, LineIndex+1);
    //Tracks[CurrentTrack].Lines[LineIndex+1] := Tracks[CurrentTrack].Lines[LineIndex];

  // clear and set new sentence
  LineNew := LineStart + 1;
  NoteStart := CurrentNote[CurrentTrack];
  Tracks[CurrentTrack].Lines[LineNew].StartBeat := Tracks[CurrentTrack].Lines[LineStart].Notes[NoteStart].StartBeat;
  Tracks[CurrentTrack].Lines[LineNew].Lyric := '';
  Tracks[CurrentTrack].Lines[LineNew].EndBeat := 0;
  Tracks[CurrentTrack].Lines[LineNew].BaseNote := 0;//High(integer); // TODO: High (integer) will causes a memory exception later in this procedure. Weird!
  Tracks[CurrentTrack].Lines[LineNew].HighNote := -1;
  SetLength(Tracks[CurrentTrack].Lines[LineNew].Notes, 0);

  // move right notes to new sentences
  NoteHigh := Tracks[CurrentTrack].Lines[LineStart].HighNote;
  for NoteIndex := NoteStart to NoteHigh do
  begin
    // increase sentence counters
    with Tracks[CurrentTrack].Lines[LineNew] do
    begin
      Inc(HighNote);
      SetLength(Notes, HighNote + 1);
      Notes[HighNote] := Tracks[CurrentTrack].Lines[LineStart].Notes[NoteIndex];
      EndBeat := Notes[HighNote].StartBeat + Notes[HighNote].Duration;
      
      if Notes[HighNote].Tone < BaseNote then
        BaseNote := Notes[HighNote].Tone;
    end;
  end;

  // clear old notes and set sentence counters
  Tracks[CurrentTrack].Lines[LineStart].HighNote := NoteStart - 1;
  Tracks[CurrentTrack].Lines[LineStart].EndBeat := Tracks[CurrentTrack].Lines[LineStart].Notes[NoteStart-1].StartBeat +
    Tracks[CurrentTrack].Lines[LineStart].Notes[NoteStart-1].Duration;
  SetLength(Tracks[CurrentTrack].Lines[LineStart].Notes, Tracks[CurrentTrack].Lines[LineStart].HighNote + 1);

  //cleanup of first note of new sentence: trim leading white space and capitalize
  Tracks[CurrentTrack].Lines[LineNew].Notes[0].Text := TrimLeft(Tracks[CurrentTrack].Lines[LineNew].Notes[0].Text);
  Tracks[CurrentTrack].Lines[LineNew].Notes[0].Text := UTF8UpperCase(UTF8Copy(Tracks[CurrentTrack].Lines[LineNew].Notes[0].Text, 1, 1)) + UTF8Copy(Tracks[CurrentTrack].Lines[LineNew].Notes[0].Text, 2, Length(Tracks[CurrentTrack].Lines[LineNew].Notes[0].Text) - 1);

  Tracks[CurrentTrack].CurrentLine := Tracks[CurrentTrack].CurrentLine + 1;
  CurrentNote[CurrentTrack] := 0;

  // adjust medley tags
  if (MedleyNotes.isStart) then
  begin
    if (MedleyNotes.start.line = LineStart) then
    begin
      if (MedleyNotes.start.note >= NoteStart) then
      begin
        Inc(MedleyNotes.start.line);
        MedleyNotes.start.note := MedleyNotes.start.note - NoteStart;
      end;
    end
    else if (MedleyNotes.start.line > LineStart) then
    begin
      Inc(MedleyNotes.start.line);
    end;
  end;

  if (MedleyNotes.isEnd) then
  begin
    if (MedleyNotes.end_.line = LineStart) then
    begin
      if (MedleyNotes.end_.note >= NoteStart) then
      begin
        Inc(MedleyNotes.end_.line);
        MedleyNotes.end_.note := MedleyNotes.end_.note - NoteStart;
      end;
    end
    else if (MedleyNotes.end_.line > LineStart) then
    begin
      Inc(MedleyNotes.end_.line);
    end;
  end;

  Refresh;
  Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 2;
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
  EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
end;

procedure TScreenEditSub.JoinSentence;
var
  LineIndex:    integer;
  NoteIndex:    integer;
  NStart:       integer;
  NDst:         integer;
begin
  LineIndex := Tracks[CurrentTrack].CurrentLine;

  // add space to last note's syllable
  Tracks[CurrentTrack].Lines[LineIndex].Notes[Tracks[CurrentTrack].Lines[LineIndex].HighNote].Text := Tracks[CurrentTrack].Lines[LineIndex].Notes[Tracks[CurrentTrack].Lines[LineIndex].HighNote].Text + ' ';

  // increase TotalNotes, HighNote and number of Notes in current sentence
  NStart := Tracks[CurrentTrack].Lines[LineIndex].HighNote + 1;
  Tracks[CurrentTrack].Lines[LineIndex].HighNote := Tracks[CurrentTrack].Lines[LineIndex].HighNote + Length(Tracks[CurrentTrack].Lines[LineIndex+1].Notes);
  SetLength(Tracks[CurrentTrack].Lines[LineIndex].Notes, Tracks[CurrentTrack].Lines[LineIndex].HighNote + 1);

  // copy notes of subsequent sentence to the end of the current sentence
  for NoteIndex := 0 to Tracks[CurrentTrack].Lines[LineIndex+1].HighNote do
  begin
    NDst := NStart + NoteIndex;
    Tracks[CurrentTrack].Lines[LineIndex].Notes[NDst] := Tracks[CurrentTrack].Lines[LineIndex+1].Notes[NoteIndex];
  end;

  // adjust end beat of
  NDst := Tracks[CurrentTrack].Lines[LineIndex].HighNote;
  Tracks[CurrentTrack].Lines[LineIndex].EndBeat := Tracks[CurrentTrack].Lines[LineIndex].Notes[NDst].StartBeat +
    Tracks[CurrentTrack].Lines[LineIndex].Notes[NDst].Duration;

  // move needed sentences to one backward.
  for LineIndex := Tracks[CurrentTrack].CurrentLine + 1 to Tracks[CurrentTrack].High - 1 do
    CopyLine(CurrentTrack, LineIndex+1, CurrentTrack, LineIndex);
    //Tracks[CurrentTrack].Lines[LineIndex] := Tracks[CurrentTrack].Lines[LineIndex+1];

  // decrease sentence length by 1
  SetLength(Tracks[CurrentTrack].Lines, Length(Tracks[CurrentTrack].Lines) - 1);
  Dec(Tracks[CurrentTrack].Number);
  Dec(Tracks[CurrentTrack].High);

  // adjust medley tags
  if (MedleyNotes.isStart) then
  begin
    if (MedleyNotes.start.line = Tracks[CurrentTrack].CurrentLine + 1) then
    begin
      Dec(MedleyNotes.start.line);
      MedleyNotes.start.note := NStart + MedleyNotes.start.note;
    end
    else if (MedleyNotes.start.line > Tracks[CurrentTrack].CurrentLine + 1) then
    begin
      Dec(MedleyNotes.start.line);
    end;
  end;

  if (MedleyNotes.isEnd) then
  begin
    if (MedleyNotes.end_.line = Tracks[CurrentTrack].CurrentLine + 1) then
    begin
      Dec(MedleyNotes.end_.line);
      MedleyNotes.end_.note := NStart + MedleyNotes.end_.note;
    end
    else if (MedleyNotes.end_.line > Tracks[CurrentTrack].CurrentLine + 1) then
    begin
      Dec(MedleyNotes.end_.line);
    end;
  end;

  Refresh;
  Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 2;
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
  EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
end;

procedure TScreenEditSub.NextSentence;
begin
  {$IFDEF UseMIDIPort}
  //MidiOut.PutShort($B1, $7, floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
  //MidiOut.PutShort(MIDI_NOTEOFF or 1, Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].Current].Notes[MidiLastNote].Tone + 60, 127);
  PlaySentenceMidi := false;
  PlayOne := false;
  {$ENDIF}
  Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
  Inc(Tracks[CurrentTrack].CurrentLine);
  CurrentNote[CurrentTrack] := 0;
  if Tracks[CurrentTrack].CurrentLine > Tracks[CurrentTrack].High then
    Tracks[CurrentTrack].CurrentLine := 0;
  Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 2;

  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
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
  //MidiOut.PutShort($81, Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].Current].Notes[MidiLastNote].Tone + 60, 127);
  PlaySentenceMidi := false;
  {$endif}

  Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
  Dec(Tracks[CurrentTrack].CurrentLine);
  CurrentNote[CurrentTrack] := 0;
  if Tracks[CurrentTrack].CurrentLine = -1 then
    Tracks[CurrentTrack].CurrentLine := Tracks[CurrentTrack].High;
  Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 2;

  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
  EditorLyrics[CurrentTrack].Selected := 0;
  GoldenRec.KillAll;
end;

procedure TScreenEditSub.DivideNote(doubleclick: boolean);
var
  LineIndex:     integer;
  NoteIndex:     integer;
  CutPosition:   integer;
  SpacePosition: integer;
  tempR:         real;
  tempstr:       UCS4String;
begin
  LineIndex := Tracks[CurrentTrack].CurrentLine;
  tempR := 720 / (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat - Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat);

  if (doubleclick) and (InteractAt(currentX, CurrentY) > 0) then
      CutPosition := Round((currentX - button[Interactions[InteractAt(currentX, CurrentY)].Num].X) / tempR)
  else
      CutPosition := 1;

  with Tracks[CurrentTrack].Lines[LineIndex] do
  begin
    Inc(HighNote);
    SetLength(Notes, HighNote + 1);

    // we copy all notes including selected one
    for NoteIndex := HighNote downto CurrentNote[CurrentTrack]+1 do
    begin
      Notes[NoteIndex] := Notes[NoteIndex-1];
    end;

    // Notes[CurrentNote[CurrentTrack]] and Notes[CurrentNote[CurrentTrack] + 1] is identical at this point
    // modify first note
    Notes[CurrentNote[CurrentTrack]].Duration := CutPosition;

    // 2nd note
    Notes[CurrentNote[CurrentTrack]+1].StartBeat := Notes[CurrentNote[CurrentTrack]].StartBeat + Notes[CurrentNote[CurrentTrack]].Duration;
    Notes[CurrentNote[CurrentTrack]+1].Duration := Notes[CurrentNote[CurrentTrack]+1].Duration - Notes[CurrentNote[CurrentTrack]].Duration;

    // find space in text
    SpacePosition := -1;
    for  NoteIndex := 0 to LengthUTF8(Notes[CurrentNote[CurrentTrack]].Text) do
    begin

      tempstr := UTF8ToUCS4String(Notes[CurrentNote[CurrentTrack]].Text);
      if ((UCS4ToUTF8String(tempstr[NoteIndex]) = ' ') and (SpacePosition < 0)) then
        SpacePosition := NoteIndex;

    end;
    if ((TextPosition < 0) and (ansipos(' ', Notes[CurrentNote[CurrentTrack]].Text) > 1) and (ansipos(' ', Notes[CurrentNote[CurrentTrack]].Text) < Length(Notes[CurrentNote[CurrentTrack]].Text)  )) then
    begin
      Notes[CurrentNote[CurrentTrack]+1].Text := UTF8Copy(Notes[CurrentNote[CurrentTrack]].Text, SpacePosition + 2, LengthUTF8(Notes[CurrentNote[CurrentTrack]].Text));
      Notes[CurrentNote[CurrentTrack]].Text := UTF8Copy(Notes[CurrentNote[CurrentTrack]].Text, 1, SpacePosition + 1)
    end
    else
    if ((TextPosition >= 0) and (TextPosition < Length(Notes[CurrentNote[CurrentTrack]].Text))) then
    begin
      Notes[CurrentNote[CurrentTrack]+1].Text := UTF8Copy(SelectsS[LyricSlideId].TextOpt[0].Text, TextPosition + 2, LengthUTF8(SelectsS[LyricSlideId].TextOpt[0].Text));
      Notes[CurrentNote[CurrentTrack]].Text := UTF8Copy(SelectsS[LyricSlideId].TextOpt[0].Text, 1, TextPosition);
      SelectsS[LyricSlideId].TextOpt[0].Text := Notes[CurrentNote[CurrentTrack]].Text;
      TextPosition := -1;
    end
    else
      Notes[CurrentNote[CurrentTrack]+1].Text := '~';
    Notes[CurrentNote[CurrentTrack]+1].Color := 1;
  end;

  // update lyric display
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
  EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
end;

procedure TScreenEditSub.DeleteNote;
var
  CurrentLine: integer;
  LineIndex:   integer;
  NoteIndex:   integer;
begin
  CurrentLine := Tracks[CurrentTrack].CurrentLine;

  //Do Not delete Last Note
  if (Tracks[CurrentTrack].Lines[CurrentLine].HighNote > 0) then
  begin
    // we copy all notes from the next to the selected one
    for NoteIndex := CurrentNote[CurrentTrack]+1 to Tracks[CurrentTrack].Lines[CurrentLine].HighNote do
    begin
      Tracks[CurrentTrack].Lines[CurrentLine].Notes[NoteIndex-1] := Tracks[CurrentTrack].Lines[CurrentLine].Notes[NoteIndex];
    end;
    
    Dec(Tracks[CurrentTrack].Lines[CurrentLine].HighNote);

    SetLength(Tracks[CurrentTrack].Lines[CurrentLine].Notes, Tracks[CurrentTrack].Lines[CurrentLine].HighNote + 1);

    // last note was deleted
    if (CurrentNote[CurrentTrack] > Tracks[CurrentTrack].Lines[CurrentLine].HighNote) then
    begin
      // select new last note
      CurrentNote[CurrentTrack] := Tracks[CurrentTrack].Lines[CurrentLine].HighNote;

      // correct Line ending
      with Tracks[CurrentTrack].Lines[CurrentLine] do
        EndBeat := Notes[HighNote].StartBeat + Notes[HighNote].Duration;
    end;

    Tracks[CurrentTrack].Lines[CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 2;
  end
  // Last Note of current Sentence Deleted - > Delete Sentence
  // if there are more than two left
  else if (Tracks[CurrentTrack].High > 1) then
  begin
    //Move all Sentences after the current to the Left
    for LineIndex := CurrentLine+1 to Tracks[CurrentTrack].High do
      CopyLine(CurrentTrack, LineIndex, CurrentTrack, LineIndex-1);
      //Tracks[CurrentTrack].Lines[LineIndex-1] := Tracks[CurrentTrack].Lines[LineIndex];

    //Delete Last Sentence
    SetLength(Tracks[CurrentTrack].Lines, Tracks[CurrentTrack].High);
    Tracks[CurrentTrack].High := High(Tracks[CurrentTrack].Lines);
    Tracks[CurrentTrack].Number := Length(Tracks[CurrentTrack].Lines);

    CurrentNote[CurrentTrack] := 0;
    if (CurrentLine > 0) then
      Tracks[CurrentTrack].CurrentLine := CurrentLine - 1
    else
      Tracks[CurrentTrack].CurrentLine := 0;

    Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 2;
  end;

  // update lyric display
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
  EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
end;

procedure TScreenEditSub.DeleteSentence;
var
  CurrentLine:    integer;
  LineIndex:      integer;

begin
  CurrentLine := Tracks[CurrentTrack].CurrentLine;

  // move all sentences after the current to the Left
  for LineIndex := CurrentLine+1 to Tracks[CurrentTrack].High do
    CopyLine(CurrentTrack, LineIndex, CurrentTrack, LineIndex-1);

  // delete last sentence
  SetLength(Tracks[CurrentTrack].Lines, Tracks[CurrentTrack].High);
  Tracks[CurrentTrack].High := High(Tracks[CurrentTrack].Lines);
  Tracks[CurrentTrack].Number := Length(Tracks[CurrentTrack].Lines);

  CurrentNote[CurrentTrack] := 0;
  if (CurrentLine > 0) then
    Tracks[CurrentTrack].CurrentLine := CurrentLine - 1
  else
    Tracks[CurrentTrack].CurrentLine := 0;

  Refresh;
  //SelectPrevNote();
  //SelectNextNote();
  Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 2;
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
end;

procedure TScreenEditSub.TransposeNote(Transpose: integer);
begin
  Inc(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Tone, Transpose);
end;

procedure TScreenEditSub.ChangeWholeTone(Tone: integer);
var
  LineIndex: integer;
  NoteIndex: integer;
begin
  for LineIndex := 0 to Tracks[CurrentTrack].High do
  begin
    Tracks[CurrentTrack].Lines[LineIndex].BaseNote := Tracks[CurrentTrack].Lines[LineIndex].BaseNote + Tone;
    for NoteIndex := 0 to Tracks[CurrentTrack].Lines[LineIndex].HighNote do
      Tracks[CurrentTrack].Lines[LineIndex].Notes[NoteIndex].Tone := Tracks[CurrentTrack].Lines[LineIndex].Notes[NoteIndex].Tone + Tone;
  end;
end;

procedure TScreenEditSub.MoveAllToEnd(Move: integer);
var
  LineIndex: integer;
  NoteIndex: integer;
  NoteStart:    integer;
begin
  for LineIndex := Tracks[CurrentTrack].CurrentLine to Tracks[CurrentTrack].High do
  begin
    NoteStart := 0;
    if LineIndex = Tracks[CurrentTrack].CurrentLine then
      NoteStart := CurrentNote[CurrentTrack];
    for NoteIndex := NoteStart to Tracks[CurrentTrack].Lines[LineIndex].HighNote do
    begin
      Inc(Tracks[CurrentTrack].Lines[LineIndex].Notes[NoteIndex].StartBeat, Move); // move note start

      if NoteIndex = 0 then
      begin // fix beginning
        Inc(Tracks[CurrentTrack].Lines[LineIndex].StartBeat, Move);
      end;

      if NoteIndex = Tracks[CurrentTrack].Lines[LineIndex].HighNote then // fix ending
        Inc(Tracks[CurrentTrack].Lines[LineIndex].EndBeat, Move);

    end; // for NoteIndex
  end; // for LineIndex
end;

procedure TScreenEditSub.MoveTextToRight;
var
  LineIndex: integer;
  NoteIndex: integer;
  NoteHigh:  integer;
begin
  LineIndex := Tracks[CurrentTrack].CurrentLine;
  NoteHigh := Tracks[CurrentTrack].Lines[LineIndex].HighNote;

  // last word
  Tracks[CurrentTrack].Lines[LineIndex].Notes[NoteHigh].Text := Tracks[CurrentTrack].Lines[LineIndex].Notes[NoteHigh-1].Text + Tracks[CurrentTrack].Lines[LineIndex].Notes[NoteHigh].Text;

  // other words
  for NoteIndex := NoteHigh - 1 downto CurrentNote[CurrentTrack] + 1 do
  begin
    Tracks[CurrentTrack].Lines[LineIndex].Notes[NoteIndex].Text := Tracks[CurrentTrack].Lines[LineIndex].Notes[NoteIndex-1].Text;
  end; // for
  Tracks[CurrentTrack].Lines[LineIndex].Notes[CurrentNote[CurrentTrack]].Text := '- ';
end;

procedure TScreenEditSub.MarkSrc;
begin
  CopySrc := Tracks[CurrentTrack].CurrentLine;
end;

procedure TScreenEditSub.PasteText;
var
  LineIndex: integer;
  NoteIndex: integer;
begin
  LineIndex := Tracks[CurrentTrack].CurrentLine;

  for NoteIndex := 0 to Tracks[CurrentTrack].Lines[CopySrc].HighNote do
    Tracks[CurrentTrack].Lines[LineIndex].Notes[NoteIndex].Text := Tracks[CurrentTrack].Lines[CopySrc].Notes[NoteIndex].Text;
end;

procedure TScreenEditSub.CopySentence(SrcLine, DstLine: integer);
var
  NoteIndex: integer;
  Time1:     integer;
  Time2:     integer;
  TimeDiff:  integer;
begin
  Time1 := Tracks[CurrentTrack].Lines[SrcLine].Notes[0].StartBeat;
  Time2 := Tracks[CurrentTrack].Lines[DstLine].Notes[0].StartBeat;
  TimeDiff := Time2-Time1;

  SetLength(Tracks[CurrentTrack].Lines[DstLine].Notes, Tracks[CurrentTrack].Lines[SrcLine].HighNote + 1);
  Tracks[CurrentTrack].Lines[DstLine].HighNote := Tracks[CurrentTrack].Lines[SrcLine].HighNote;
  for NoteIndex := 0 to Tracks[CurrentTrack].Lines[SrcLine].HighNote do
  begin
    Tracks[CurrentTrack].Lines[DstLine].Notes[NoteIndex].Text := Tracks[CurrentTrack].Lines[SrcLine].Notes[NoteIndex].Text;
    Tracks[CurrentTrack].Lines[DstLine].Notes[NoteIndex].Duration := Tracks[CurrentTrack].Lines[SrcLine].Notes[NoteIndex].Duration;
    Tracks[CurrentTrack].Lines[DstLine].Notes[NoteIndex].Tone := Tracks[CurrentTrack].Lines[SrcLine].Notes[NoteIndex].Tone;
    Tracks[CurrentTrack].Lines[DstLine].Notes[NoteIndex].StartBeat := Tracks[CurrentTrack].Lines[SrcLine].Notes[NoteIndex].StartBeat + TimeDiff;
  end;
  //NoteIndex := Tracks[CurrentTrack].Lines[Src].HighNote;
  Tracks[CurrentTrack].Lines[DstLine].EndBeat := Tracks[CurrentTrack].Lines[DstLine].Notes[NoteIndex].StartBeat + Tracks[CurrentTrack].Lines[DstLine].Notes[NoteIndex].Duration;
end;

procedure TScreenEditSub.CopySentences(SrcLine, DstLine, Num: integer);
var
  LineIndex: integer;
begin
  // create place for new sentences
  SetLength(Tracks[CurrentTrack].Lines, Tracks[CurrentTrack].Number + Num - 1);

  // moves sentences next to the destination
  for LineIndex := Tracks[CurrentTrack].High downto DstLine + 1 do
  begin
    Tracks[CurrentTrack].Lines[LineIndex + Num - 1] := Tracks[CurrentTrack].Lines[LineIndex];
  end;

  // prepares new sentences: sets sentence start and create first note
  for LineIndex := 1 to Num-1 do
  begin
    Tracks[CurrentTrack].Lines[DstLine + LineIndex].StartBeat := Tracks[CurrentTrack].Lines[DstLine + LineIndex - 1].Notes[0].StartBeat +
      (Tracks[CurrentTrack].Lines[SrcLine + LineIndex].Notes[0].StartBeat - Tracks[CurrentTrack].Lines[SrcLine + LineIndex - 1].Notes[0].StartBeat);
    SetLength(Tracks[CurrentTrack].Lines[DstLine + LineIndex].Notes, 1);
    Tracks[CurrentTrack].Lines[DstLine + LineIndex].HighNote := 0;
    Tracks[CurrentTrack].Lines[DstLine + LineIndex].Notes[0].StartBeat := Tracks[CurrentTrack].Lines[DstLine + LineIndex].StartBeat;
    Tracks[CurrentTrack].Lines[DstLine + LineIndex].Notes[0].Duration := 1;
    Tracks[CurrentTrack].Lines[DstLine + LineIndex].EndBeat := Tracks[CurrentTrack].Lines[DstLine + LineIndex].StartBeat + 1;
  end;

  // increase counters
  Tracks[CurrentTrack].Number := Tracks[CurrentTrack].Number + Num - 1;
  Tracks[CurrentTrack].High := Tracks[CurrentTrack].High + Num - 1;

  for LineIndex := 0 to Num-1 do
    CopySentence(SrcLine + LineIndex, DstLine + LineIndex);
end;

procedure TScreenEditSub.MakeSolo;
begin
  if not (CurrentSong.isDuet) then
    Exit;

  // use current track to make solo
  if (CurrentTrack <> 0) then
    Tracks[0] := Tracks[CurrentTrack];

  SetLength(Tracks, 1);
  CurrentSong.isDuet := false;

  CurrentTrack := 0;
  Refresh;
  CurrentNote[CurrentTrack] := 0;
  Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 2;

  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
end;

procedure TScreenEditSub.MakeDuet;
var
  TrackIndex: integer;
  LineIndex:  integer;

begin
  if (CurrentSong.isDuet) then
    Exit;

  SetLength(Tracks, 2);

  Tracks[CurrentTrack+1].CurrentLine := Tracks[CurrentTrack].CurrentLine;
  Tracks[CurrentTrack+1].High := Tracks[CurrentTrack].High;
  Tracks[CurrentTrack+1].Number := Tracks[CurrentTrack].Number;
  Tracks[CurrentTrack+1].Resolution := Tracks[CurrentTrack].Resolution;
  Tracks[CurrentTrack+1].NotesGAP := Tracks[CurrentTrack].NotesGAP;
  Tracks[CurrentTrack+1].ScoreValue := 0;
  SetLength(Tracks[CurrentTrack+1].Lines, Length(Tracks[CurrentTrack].Lines));

  for LineIndex := 0 to High(Tracks[CurrentTrack].Lines) do
    CopyLine(0, LineIndex, 1, LineIndex);

  CurrentSong.isDuet := true;

  CurrentNote[CurrentTrack+1] := 0;
  Tracks[CurrentTrack+1].CurrentLine := 0;

  EditorLyrics[CurrentTrack+1] := EditorLyrics[0];

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

  SrcStartBeat: integer;
  SrcEndBeat:   integer;

  DstStartBeat: integer;
  DstEndBeat:   integer;

  SrcNumN:      integer;
  DstNumN:      integer;

  LineIndex1:   integer;
  LineIndex2:   integer;

  LineLength: integer;
begin
  Result := false;

  SrcTrack := CurrentTrack;
  DstTrack := (CurrentTrack + 1) mod 2;
  SrcLine := Tracks[SrcTrack].CurrentLine;
  DstLine := -1;

  SrcStartBeat := Tracks[SrcTrack].Lines[SrcLine].Notes[0].StartBeat;
  SrcNumN := Length(Tracks[SrcTrack].Lines[SrcLine].Notes);
  SrcEndBeat := Tracks[SrcTrack].Lines[SrcLine].Notes[SrcNumN-1].StartBeat + Tracks[SrcTrack].Lines[SrcLine].Notes[SrcNumN-1].Duration;

  for LineIndex1 := 0 to High(Tracks[DstTrack].Lines) do
  begin
    DstStartBeat := Tracks[DstTrack].Lines[LineIndex1].Notes[0].StartBeat;
    DstNumN := Length(Tracks[DstTrack].Lines[LineIndex1].Notes);
    DstEndBeat := Tracks[DstTrack].Lines[LineIndex1].Notes[DstNumN-1].StartBeat + Tracks[DstTrack].Lines[LineIndex1].Notes[DstNumN-1].Duration;
    if (DstStartBeat <= SrcStartBeat) and (SrcEndBeat <= DstEndBeat) then // SrcLine fits into existing line DstLine --> replace DstLine by SrcLine
    begin
      DstLine := LineIndex1;
      break;
    end;

    if (DstLine = -1) then // SrcLine does not fit into any of the existing lines --> insert SrcLine
    begin
      if (LineIndex1 < Length(Tracks[DstTrack].Lines)-1) then // insert somewhere in the middle
      begin
        DstStartBeat := DstEndBeat;
        DstEndBeat := Tracks[DstTrack].Lines[LineIndex1+1].Notes[0].StartBeat;
        if (DstStartBeat < SrcStartBeat) and (SrcEndBeat < DstEndBeat) then
        begin
          LineLength := Length(Tracks[DstTrack].Lines);
          SetLength(Tracks[DstTrack].Lines, LineLength + 1);
          Inc(Tracks[DstTrack].Number);
          Inc(Tracks[DstTrack].High);

          // make room for new line
          for LineIndex2 := LineLength-1 downto LineIndex1 do
            CopyLine(DstTrack, LineIndex2, DstTrack, LineIndex2+1);

          DstLine := LineIndex1 + 1;
          SetLength(Tracks[DstTrack].Lines[DstLine].Notes, 0);
          break;
        end;
      end
      else
      begin
        if (SrcEndBeat <= Tracks[DstTrack].Lines[0].Notes[0].StartBeat) then // insert at beginning
        begin
          LineLength := Length(Tracks[DstTrack].Lines);
          SetLength(Tracks[DstTrack].Lines, LineLength + 1);
          Inc(Tracks[DstTrack].Number);
          Inc(Tracks[DstTrack].High);

          // make room for new line
          for LineIndex2 := LineLength-1 downto 0 do
            CopyLine(DstTrack, LineIndex2, DstTrack, LineIndex2+1);

          DstLine := 0;
          SetLength(Tracks[DstTrack].Lines[DstLine].Notes, 0);
          break;
        end
        else
        if (SrcStartBeat >= Tracks[DstTrack].Lines[High(Tracks[DstTrack].Lines)].EndBeat) then // insert at end
        begin
          LineLength := Length(Tracks[DstTrack].Lines);
          SetLength(Tracks[DstTrack].Lines, LineLength + 1);
          Inc(Tracks[DstTrack].Number);
          Inc(Tracks[DstTrack].High);

          DstLine := High(Tracks[DstTrack].Lines);
          SetLength(Tracks[DstTrack].Lines[DstLine].Notes, 0);
          break;
        end;
      end;
    end;
  end;

  if (DstLine = -1) then
    Exit;

  CopyLine(SrcTrack, SrcLine, DstTrack, DstLine);

  Refresh;
  EditorLyrics[DstTrack].AddLine(DstTrack, Tracks[DstTrack].CurrentLine);
  EditorLyrics[DstTrack].Selected := 0;
  CurrentNote[DstTrack] := 0;
  Tracks[SrcTrack].Lines[SrcLine].Notes[CurrentNote[SrcTrack]].Color := 2;
  Result := true;
end;

procedure TScreenEditSub.CopyLine(SrcTrack, SrcLine, DstTrack, DstLine: integer);
begin
  Tracks[DstTrack].Lines[DstLine] := Tracks[SrcTrack].Lines[SrcLine];
  Tracks[DstTrack].Lines[DstLine].Notes := Copy(Tracks[SrcTrack].Lines[SrcLine].Notes);
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
  SetLength(UndoLines, Length(UndoLines)+1, Length(Tracks));
  CurrentUndoLines := high(UndoLines);
  SetLength(UndoStateNote, CurrentUndoLines+1, Length(Tracks));
  SetLength(UndoHeader, CurrentUndoLines+1);

  UndoHeader[CurrentUndoLines].Title := CurrentSong.Title;
  UndoHeader[CurrentUndoLines].Artist := CurrentSong.Artist;
  UndoHeader[CurrentUndoLines].Language := CurrentSong.Language;
  UndoHeader[CurrentUndoLines].Edition := CurrentSong.Edition;
  UndoHeader[CurrentUndoLines].Genre := CurrentSong.Genre;
  UndoHeader[CurrentUndoLines].Year := CurrentSong.Year;
  UndoHeader[CurrentUndoLines].Creator := CurrentSong.Creator;
  UndoHeader[CurrentUndoLines].Mp3 := CurrentSong.Mp3;
  UndoHeader[CurrentUndoLines].Mp3Id := SelectsS[Mp3SlideId].SelectedOption;
  UndoHeader[CurrentUndoLines].Cover := CurrentSong.Cover;
  UndoHeader[CurrentUndoLines].CoverId := SelectsS[CoverSlideId].SelectedOption;
  UndoHeader[CurrentUndoLines].Background := CurrentSong.Background;
  UndoHeader[CurrentUndoLines].BackgroundId := SelectsS[BackgroundSlideId].SelectedOption;
  UndoHeader[CurrentUndoLines].Video := CurrentSong.Video;
  UndoHeader[CurrentUndoLines].VideoId := SelectsS[VideoSlideId].SelectedOption;
  UndoHeader[CurrentUndoLines].VideoGAP := CurrentSong.VideoGAP;
  SetLength(UndoHeader[CurrentUndoLines].BPM, length(CurrentSong.BPM));
  for BPMIndex := 0 to High(CurrentSong.BPM) do
  begin
    UndoHeader[CurrentUndoLines].BPM[BPMIndex].BPM := CurrentSong.BPM[BPMIndex].BPM;
    UndoHeader[CurrentUndoLines].BPM[BPMIndex].StartBeat := CurrentSong.BPM[BPMIndex].StartBeat;
  end;
  UndoHeader[CurrentUndoLines].GAP  := CurrentSong.GAP;
  UndoHeader[CurrentUndoLines].StartTag := CurrentSong.Start;
  UndoHeader[CurrentUndoLines].EndTag := CurrentSong.Finish;
  if not (CurrentSong.isDuet) then
  begin
    if (MedleyNotes.isStart) then UndoHeader[CurrentUndoLines].MedleyStartBeat := Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes[MedleyNotes.start.note].StartBeat;
    if (MedleyNotes.isEnd) then UndoHeader[CurrentUndoLines].MedleyEndBeat := Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].StartBeat + Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].Duration;
  end;
  UndoHeader[CurrentUndoLines].PreviewStart := CurrentSong.PreviewStart;
  UndoHeader[CurrentUndoLines].Relative := CurrentSong.Relative;

  for TrackIndex := 0 to High(Tracks) do
  begin
    UndoStateNote[CurrentUndoLines, TrackIndex] := CurrentNote[TrackIndex];

    UndoLines[CurrentUndoLines, TrackIndex].CurrentLine := Tracks[TrackIndex].CurrentLine;
    UndoLines[CurrentUndoLines, TrackIndex].High := Tracks[TrackIndex].High;
    UndoLines[CurrentUndoLines, TrackIndex].Number := Tracks[TrackIndex].Number;
    UndoLines[CurrentUndoLines, TrackIndex].Resolution := Tracks[TrackIndex].Resolution;
    UndoLines[CurrentUndoLines, TrackIndex].NotesGAP := Tracks[TrackIndex].NotesGAP;
    UndoLines[CurrentUndoLines, TrackIndex].ScoreValue := Tracks[TrackIndex].ScoreValue;
    SetLength(UndoLines[CurrentUndoLines, TrackIndex].Lines, Length(Tracks[TrackIndex].Lines));

    for LineIndex := 0 to High(Tracks[TrackIndex].Lines) do
    begin
      UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].StartBeat  := Tracks[TrackIndex].Lines[LineIndex].StartBeat;
      UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Lyric      := Tracks[TrackIndex].Lines[LineIndex].Lyric;
      UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].EndBeat    := Tracks[TrackIndex].Lines[LineIndex].EndBeat;
      UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].BaseNote   := Tracks[TrackIndex].Lines[LineIndex].BaseNote;
      UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].HighNote   := Tracks[TrackIndex].Lines[LineIndex].HighNote;
      UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].ScoreValue := Tracks[TrackIndex].Lines[LineIndex].ScoreValue;
      UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].LastLine   := Tracks[TrackIndex].Lines[LineIndex].LastLine;

      SetLength(UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes, length(Tracks[TrackIndex].Lines[LineIndex].Notes));
      for NoteIndex := 0 to High(Tracks[TrackIndex].Lines[LineIndex].Notes) do
      begin
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Color     := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Color;
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat;
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration  := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration;
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Tone      := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Tone;
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text      := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text;
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].NoteType  := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].NoteType;
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

  if not (CurrentSong.isDuet) then
  begin
    if MedleyNotes.isStart and
      ((High(Tracks[CurrentTrack].Lines) < MedleyNotes.start.line) or
       (High(Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes) < MedleyNotes.start.note)) then
      MedleyNotes.isStart := false;

    if MedleyNotes.isEnd and
      ((High(Tracks[CurrentTrack].Lines) < MedleyNotes.end_.line) or
       (High(Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes) < MedleyNotes.end_.note)) then
      MedleyNotes.isEnd := false;
  end;


  for TrackIndex := 0 to High(Tracks) do
  begin
    Tracks[TrackIndex].Number := Length(Tracks[TrackIndex].Lines);
    Tracks[TrackIndex].High := Tracks[TrackIndex].Number - 1;
    Tracks[TrackIndex].ScoreValue := 0;

    for LineIndex := 0 to High(Tracks[TrackIndex].Lines) do
    begin
      with Tracks[TrackIndex].Lines[LineIndex] do
      begin
        HighNote := Length(Notes) - 1;
        ScoreValue := 0;
        BaseNote := High(Integer);

        if (Length(Notes) > 0) then
        begin
          //StartBeat := Notes[0].StartBeat; // FIXME: is this really true? Isn't StartBeat of a Line corresponding to the line break time? See TLine in UMusic
          for NoteIndex := 0 to High(Tracks[TrackIndex].Lines[LineIndex].Notes) do
          begin
            //Notes[NoteIndex].Color := 0;

            if not (CurrentSong.isDuet) then
            begin
              if (MedleyNotes.isStart and
                 (MedleyNotes.start.track = TrackIndex) and
                 (MedleyNotes.start.line = LineIndex) and
                 (MedleyNotes.start.note = NoteIndex)) or
                 (MedleyNotes.isEnd and (MedleyNotes.end_.track = TrackIndex) and
                 (MedleyNotes.end_.line = LineIndex) and
                 (MedleyNotes.end_.note = NoteIndex)) then
                Notes[NoteIndex].IsMedley := true
              else
                Notes[NoteIndex].IsMedley := false;
            end;

            Notes[NoteIndex].IsStartPreview := false;

            Tracks[TrackIndex].ScoreValue := Tracks[TrackIndex].ScoreValue + Notes[NoteIndex].Duration * ScoreFactor[Notes[NoteIndex].NoteType];
            ScoreValue := ScoreValue + Notes[NoteIndex].Duration * ScoreFactor[Notes[NoteIndex].NoteType];

            if (Notes[NoteIndex].Tone < BaseNote) then
              BaseNote := Notes[NoteIndex].Tone;
          end;
        end else
          BaseNote := 0;
      end;
    end;
  end;

  // set Preview Start
  MedleyNotes.Preview := FindNote(round(GetMidBeat(CurrentSong.PreviewStart - CurrentSong.Gap / 1000)));
  Tracks[MedleyNotes.Preview.track].Lines[MedleyNotes.Preview.line].Notes[MedleyNotes.Preview.note].IsStartPreview := true;
  CurrentSong.PreviewStart := round(GetTimeFromBeat(Tracks[MedleyNotes.Preview.track].Lines[MedleyNotes.Preview.line].Notes[MedleyNotes.Preview.note].StartBeat) * 1000) / 1000;
end;

procedure TScreenEditSub.CopyFromUndo;
var
  I:             integer;
  TrackIndex:    integer;
  LineIndex:     integer;
  NoteIndex:     integer;

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

  for TrackIndex := 0 to High(Tracks) do
  begin
    CurrentNote[TrackIndex] := UndoStateNote[high(UndoStateNote), TrackIndex];

    Tracks[TrackIndex].CurrentLine := UndoLines[CurrentUndoLines, TrackIndex].CurrentLine;
    Tracks[TrackIndex].High := UndoLines[CurrentUndoLines, TrackIndex].High;
    Tracks[TrackIndex].Number := UndoLines[CurrentUndoLines, TrackIndex].Number;
    Tracks[TrackIndex].Resolution := UndoLines[CurrentUndoLines, TrackIndex].Resolution;
    Tracks[TrackIndex].NotesGAP := UndoLines[CurrentUndoLines, TrackIndex].NotesGAP;
    Tracks[TrackIndex].ScoreValue := UndoLines[CurrentUndoLines, TrackIndex].ScoreValue;
    SetLength(Tracks[TrackIndex].Lines, Length(UndoLines[CurrentUndoLines, TrackIndex].Lines));
    for LineIndex := 0 to High(UndoLines[CurrentUndoLines, TrackIndex].Lines) do
    begin
        Tracks[TrackIndex].Lines[LineIndex].StartBeat  := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].StartBeat;
        Tracks[TrackIndex].Lines[LineIndex].Lyric      := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Lyric;
        Tracks[TrackIndex].Lines[LineIndex].EndBeat    := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].EndBeat;
        Tracks[TrackIndex].Lines[LineIndex].BaseNote   := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].BaseNote;
        Tracks[TrackIndex].Lines[LineIndex].HighNote   := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].HighNote;
        Tracks[TrackIndex].Lines[LineIndex].ScoreValue := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].ScoreValue;
        Tracks[TrackIndex].Lines[LineIndex].LastLine   := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].LastLine;

        SetLength(Tracks[TrackIndex].Lines[LineIndex].Notes, Length(UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes));
        for  NoteIndex := 0 to High(UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes) do
        begin
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Color     := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Color;
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat;
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration  := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration;
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Tone      := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Tone;
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text      := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text;
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].NoteType  := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].NoteType;
        end; //for NoteIndex
    end; //for LineIndex
  end; //for TrackIndex
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
  SelectsS[LyricSlideId].TextOpt[0].Text := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text;
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
  EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
end; //if CurrentUndoLines
end;

procedure TScreenEditSub.DrawPlayerTrack(X, Y, W: real; Space: integer; CurrentTone: integer; Count: integer; CurrentNote: integer);
var
  TempR:   real;
  Rec:     TRecR;
  N:       integer;
  scale:   integer;
  NotesH2: real;
  W1:      real;
  H1:      real;
  X1:      real;
  X2:      real;
begin
  glColor3f(1, 1, 1);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  TempR := W / (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat - Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat);

  NotesH2 := int(NotesH[0] * 0.65);
  W1 := NotesW[0] * 2 + 2;
  H1 := NotesH[0] * 1.5;// + 3.5;
  X2 := 40 + 0.5 + 10*ScreenX+Count;
  X1 := X2-W1-2;

  Rec.Left  := X1;
  Rec.Right := X2;
  scale := 0;
  repeat
    if (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote].Tone + 12 * scale > CurrentTone) then
      dec(scale)
    else
      inc(scale);

  until (
    (((Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote].Tone + 12 * scale) / 12) < 1) and
    (((Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote].Tone + 12 * scale) / 12) >= 0));

  Rec.Top := 410 - (CurrentTone - 12*scale - Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].BaseNote) * Space/2 - H1;
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
  StartBeat:    integer;
  EndBeat:      integer;
  SongStart:    integer;
  SongEnd:      integer;
  SongDuration: integer;
  i:            integer;
  Color:        TRGB;

  CurrentPos: real;
  Width:      real;

  LineIndex: integer;
  numLines:  integer;

  function FindStartBeat(): integer;
  var
    TrackIndex: integer;
    LineIndex:  integer;
  begin
    Result := High(integer);

    for TrackIndex := 0 to High(Tracks) do
      for LineIndex := 0 to High(Tracks[TrackIndex].Lines) do
      begin
        if (Length(Tracks[TrackIndex].Lines[LineIndex].Notes) > 0) then
        begin
          if(Result > Tracks[TrackIndex].Lines[LineIndex].Notes[0].StartBeat) then
            Result := Tracks[TrackIndex].Lines[LineIndex].Notes[0].StartBeat;
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

    for TrackIndex := 0 to High(Tracks) do
      for LineIndex := 0 to High(Tracks[TrackIndex].Lines) do
      begin
        if (Length(Tracks[TrackIndex].Lines[LineIndex].Notes)>0) then
        begin
          NoteIndex := Length(Tracks[TrackIndex].Lines[LineIndex].Notes) - 1;
          if(Result < Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat + Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration) then
            Result := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat + Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration;
        end;
      end;
  end;

begin
  numLines := Length(Tracks[Track].Lines);

  if (Track = CurrentTrack) then
    EditDrawBorderedBox(X, Y, W, H, ColR, ColG, ColB, Alpha)
  else
    EditDrawBorderedBox(X, Y, W, H, ColR, ColG, ColB, Alpha);

  if(numLines = 1) then
    Exit;

  SongStart := FindStartBeat;
  SongEnd := FindEndBeat;
  SongDuration := SongEnd - SongStart;

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
    SetLength(InteractiveLineId, Length(InteractiveLineId) + 1);
    SetLength(TransparentLineButtonId, Length(TransparentLineButtonId) + 1);
    TransparentLineButtonId[Length(TransparentLineButtonId) - 1] := AddButton(0, 0, 0, 0,PATH_NONE);
    //TransparentLineButtonId[Length(TransparentLineButtonId) - 1] := AddButton(0, 0, 0, 0, Skin.GetTextureFileName(Theme.Main.Buttonsolo.Tex));
    InteractiveLineId[Length(InteractiveLineId) - 1] := length(Interactions) - 1;
  end;

  for LineIndex := 0 to numLines - 1 do
  begin
    if (LineIndex = Tracks[Track].CurrentLine) and not (PlaySentence or PlaySentenceMidi or PlayOne) then
      glColor4f(1, 0.6, 0, 1) // currently selected line in orange
    else
      if (CurrentSong.Medley.Source <> msNone) and (LineIndex >= MedleyNotes.start.line) and (LineIndex <= MedleyNotes.end_.line) then
        glColor4f(0.15, 0.75, 0.15, 1)
      else
      begin
        // all other lines in orange (current track) and gray (other track)
        if (Track = CurrentTrack) then
        begin
          Color := GetPlayerColor(Ini.SingColor[CurrentTrack]);
          glColor4f(Color.R, Color.G, Color.B, 1)
        end
        else
          glColor4f(0.7, 0.7, 0.7, 1);
      end;

    StartBeat := Tracks[Track].Lines[LineIndex].Notes[0].StartBeat;
    EndBeat := Tracks[Track].Lines[LineIndex].Notes[Tracks[Track].Lines[LineIndex].HighNote].StartBeat +
      Tracks[Track].Lines[LineIndex].Notes[Tracks[Track].Lines[LineIndex].HighNote].Duration;

    CurrentPos := (StartBeat - SongStart) / SongDuration * W;
    Width := (EndBeat - StartBeat) / SongDuration * W;

    // todo: add transparent active button to change current line
    Button[TransparentLineButtonId[LineIndex]].SetX(X + CurrentPos);
    Button[TransparentLineButtonId[LineIndex]].SetY(Y);
    Button[TransparentLineButtonId[LineIndex]].SetW(Width);
    Button[TransparentLineButtonId[LineIndex]].SetH(H);

    glbegin(gl_quads);
      glVertex2f(X + CurrentPos, Y);
      glVertex2f(X + CurrentPos, Y + H);
      glVertex2f(X + CurrentPos + Width, Y + H);
      glVertex2f(X + CurrentPos + Width, Y);
    glEnd;
  end;

  if(PlaySentence or PlaySentenceMidi or PlayOne) then
  begin
    glColor4f(0, 0, 0, 0.5);
    CurrentPos := 0;
    Width := (CurrentBeat - SongStart) / SongDuration * W;
    if (Width > W) then
      Width := W;
  end else
  begin
    glColor4f(1, 0, 0, 1);
    CurrentPos := (Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[CurrentNote[Track]].StartBeat - SongStart) / SongDuration * W;
    Width := Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[CurrentNote[Track]].Duration / SongDuration * W;
    if (Width < 1) then
      Width := 1;
  end;

  glEnable(GL_BLEND);
  glbegin(gl_quads);
    glVertex2f(X + CurrentPos, Y);
    glVertex2f(X + CurrentPos, Y + H);
    glVertex2f(X + CurrentPos + Width, Y + H);
    glVertex2f(X + CurrentPos + Width, Y);
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
  Str:   UTF8String;

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

    if not Tracks[Track].Lines[Tracks[Track].CurrentLine].HasLength(TempR) then TempR := 0
    else TempR := W / TempR;

    with Tracks[Track].Lines[Tracks[Track].CurrentLine] do
    begin

      OrgFontStyle := ActFont;
      glColor4f(0, 0, 0, 1);
      SetFontStyle(1);
      SetFontItalic(False);
      SetFontSize(14);

      for Count := 0 to HighNote do
      begin
        with Notes[Count] do
        begin
          // left part
          Rec.Left  := 0;
          Rec.Right := 0;
          BaseNote := Tracks[Track].Lines[Tracks[Track].CurrentLine].BaseNote;
          Rec.Top := Y - (Tone-BaseNote)*Space/2 - NotesH[0];
          Rec.Bottom := Rec.Top + 2 * NotesH[0];
          // middle part
          Rec.Left := (StartBeat - Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + X + 0.5 + 10*ScreenX + NotesW[0];
          Rec.Right := (StartBeat + Duration - Tracks[Track].Lines[Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + X - NotesW[0] - 0.5 + 10*ScreenX;
          SetFontPos(Rec.Left, Rec.Top);
          glPrint(Text);
          // add info if current note is medley start
          if (MedleyNotes.isStart) and (Tracks[Track].CurrentLine = MedleyNotes.start.line) and (Count = MedleyNotes.start.note) then
          begin
            Str := '| MedleyStart';
            SetFontPos(Rec.Left - 0.5 - NotesW[0], Rec.Top + Space);
            glColor4f(0.15, 0.75, 0.15, 1);
            glPrint(Str);
            glColor4f(0, 0, 0, 1);
          end;
          // add info if current note is medley end
          if (MedleyNotes.isEnd) and (Tracks[Track].CurrentLine = MedleyNotes.end_.line) and (Count = MedleyNotes.end_.note) then
          begin
            Str := 'MedleyEnd |';
            SetFontPos(Rec.Right + 0.5 + NotesW[0] - glTextWidth(Str), Rec.Top + Space);
            glColor4f(0.15, 0.75, 0.15, 1);
            glPrint(Str);
            glColor4f(0, 0, 0, 1);
          end;
          if (CurrentSong.HasPreview) and (IsStartPreview) then
          begin
            Str := '| PreviewStart';
            SetFontPos(Rec.Left - 0.5 - NotesW[0], Rec.Top - Space);
            //glColor4f(0.15, 0.75, 0.15, 1);
            glPrint(Str);
            //glColor4f(0, 0, 0, 1);
          end;
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
  while (Length(TransparentNoteButtonId)-1 < Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote) do
  begin
    SetLength(InteractiveNoteId, Length(InteractiveNoteId)+1);
    SetLength(TransparentNoteButtonId, Length(TransparentNoteButtonId)+1);
    TransparentNoteButtonId[Length(TransparentNoteButtonId)-1] := AddButton(0, 0, 0, 0, PATH_NONE);
    // for debug purposes: use some button texture instead of a transparent button (comment out line above, uncomment line below)
    //TransparentNoteButtonId[Length(TransparentNoteButtonId)-1] := AddButton(0, 0, 0, 0, Skin.GetTextureFileName(Theme.Main.Buttonsolo.Tex));
    InteractiveNoteId[Length(InteractiveNoteId)-1] := length(Interactions)-1;
  end;
  TempR := 720 / (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat - Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat);
  for i := 0 to Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote do
  begin
    Button[TransparentNoteButtonId[i]].SetX(Theme.EditSub.NotesBackground.X + NotesSkipX + (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[i].StartBeat - Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat) * TempR + 0.5 + 10*ScreenX);
    Button[TransparentNoteButtonId[i]].SetY(Theme.EditSub.NotesBackground.Y + 7 * LineSpacing - (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[i].Tone - Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].BaseNote) * LineSpacing / 2 - NotesH[0]);
    Button[TransparentNoteButtonId[i]].SetW((Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[i].Duration) * TempR - 0.5 + 10*(ScreenX));
    Button[TransparentNoteButtonId[i]].SetH(2 * NotesH[0]);
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

    for TrackIndex := 0 to High(Tracks) do
    begin
      Tracks[TrackIndex].CurrentLine := 0;
      CurrentNote[TrackIndex] := 0;
      Tracks[TrackIndex].Lines[0].Notes[0].Color := 2;
    end;

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

    // set preview start
    MedleyNotes.Preview := FindNote(round(GetMidBeat(CurrentSong.PreviewStart - CurrentSong.GAP/1000)));
    Tracks[MedleyNotes.Preview.track].Lines[MedleyNotes.Preview.line].Notes[MedleyNotes.Preview.note].IsStartPreview := true;
    CurrentSong.PreviewStart := round(GetTimeFromBeat(Tracks[MedleyNotes.Preview.track].Lines[MedleyNotes.Preview.line].Notes[MedleyNotes.Preview.note].StartBeat) * 1000) / 1000;

    for TrackIndex := 0 to High(Tracks) do
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

      EditorLyrics[TrackIndex].AddLine(TrackIndex, 0);
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
  P1EditMode := false;
  P2EditMode := false;

  editLengthText := 0;
  TextPosition := -1;
end;

function TScreenEditSub.Draw: boolean;
const
  NumLines:   integer = 10;
var
  Pet, i:     integer;
  LastLine:   integer;
  NoteIndex:  integer;
  Count:      integer;
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
      MidiOut.PutShort(MIDI_NOTEOFF or 1, Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[MidiLastNote].Tone + 60, 127);
      PlaySentenceMidi := false;
    end;

    // click
    CurrentBeat := Floor(GetMidBeat(MidiPos - CurrentSong.GAP / 1000));
    Text[TextDebug].Text := Language.Translate('EDIT_INFO_CURRENT_BEAT') + ' ' + IntToStr(CurrentBeat);

    if CurrentBeat <> LastClick then
    begin
      for i := 0 to Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote do
        if (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[i].StartBeat = CurrentBeat) then
        begin

          LastClick := CurrentBeat;
          MidiOut.PutShort($B1, $7, floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
          if i > 0 then
            MidiOut.PutShort(MIDI_NOTEOFF or 1, Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[i-1].Tone + 60, 127);
          MidiOut.PutShort($91, Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[i].Tone + 60, 127);
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
    LastLine := Tracks[CurrentTrack].CurrentLine;
    while (Tracks[CurrentTrack].CurrentLine < High(Tracks[CurrentTrack].Lines)) and (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat < CurrentBeat) do
      inc(Tracks[CurrentTrack].CurrentLine);

    // only update lyric if line changes
    if Tracks[CurrentTrack].CurrentLine <> LastLine then
    begin
        Tracks[CurrentTrack].Lines[LastLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
        EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
        EditorLyrics[CurrentTrack].Selected := 0;
        CurrentNote[CurrentTrack] := 0;
        ShowInteractiveBackground;
        GoldenRec.KillAll;
    end;

    for NoteIndex := CurrentNote[CurrentTrack] to High(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes) do
      begin
        //note change
        if Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[NoteIndex].StartBeat < CurrentBeat then
            begin
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
              CurrentNote[CurrentTrack] := NoteIndex;
              EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 2;
            end; //if
      end; //for NoteIndex}
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
        for i := 0 to Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote do
          if (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[i].StartBeat = CurrentBeat) then
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
      MidiOut.PutShort($81, Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Tone + 60, 127);
      MidiOut.PutShort(MIDI_STOP, 0, 0);
      PlayOneMidi := false;
    end;

    // click
    CurrentBeat := Floor(GetMidBeat(MidiPos - CurrentSong.GAP / 1000));
    Text[TextDebug].Text := Language.Translate('EDIT_INFO_CURRENT_BEAT') + ' ' + IntToStr(CurrentBeat);

    if ((CurrentBeat <> LastClick) and Not (midinotefound)) then
    begin
//      for i := 0 to Tracks[CurrentTrack].Lines[Lines[CurrentTrack].Current].HighNote do
//      begin
        if ((Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat <= CurrentBeat) and
        ((Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat + Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration) > CurrentBeat)) then
        begin
          LastClick := CurrentBeat;
          midinotefound := true;
          MidiOut.PutShort($B1, $7, floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
//          if i > 0 then
            MidiOut.PutShort($81, Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Tone + 60, 127);
          MidiOut.PutShort($91, Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Tone + 60, 127);

          MidiLastNote := i;
        end;
//      end;
    end;
  end; // if PlayOneNoteMidi
  {$ENDIF}

  Button[TextSentence].Text[0].Text := Language.Translate('EDIT_INFO_CURRENT_LINE') + ' ' + IntToStr(Tracks[CurrentTrack].CurrentLine + 1) + ' / ' + IntToStr(Tracks[CurrentTrack].Number);
  Button[TextNote].Text[0].Text :=  Language.Translate('EDIT_INFO_CURRENT_NOTE') + ' ' + IntToStr(CurrentNote[CurrentTrack] + 1) + ' / ' + IntToStr(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote + 1);

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
  StartTagVal[0] := ifthen(CurrentSong.Start <> 0.0, FloatToStr(CurrentSong.Start) + ' s', NOT_SET);
  SelectsS[StartTagSlideId].TextOpt[0].Text := StartTagVal[0];
  // EndTag
  EndTagVal[0] := ifthen(CurrentSong.Finish > 0.0, FloatToStr(CurrentSong.Finish) + ' ms', NOT_SET);
  SelectsS[EndTagSlideId].TextOpt[0].Text := EndTagVal[0];
  // MedleyStart
  if not (CurrentSong.isDuet) then
  begin
    SelectsS[MedleyStartSlideId].Text.Text := Theme.EditSub.SlideMedleyStart.Text;
    SelectsS[MedleyStartSlideId].TextOpt[0].Writable := false;
    SelectsS[MedleyEndSlideId].Text.Text := Theme.EditSub.SlideMedleyEnd.Text;
    SelectsS[MedleyEndSlideId].TextOpt[0].Writable := false;
    if (MedleyNotes.isStart) then
      MedleyStartVal[0] := IntToStr(Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes[MedleyNotes.start.note].StartBeat)
    else
      MedleyStartVal[0] := NOT_SET;
    SelectsS[MedleyStartSlideId].TextOpt[0].Text := MedleyStartVal[0];
    // MedleyEnd
    if (MedleyNotes.isEnd) then
      MedleyEndVal[0] := IntToStr(Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].StartBeat + Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].Duration)
    else
      MedleyEndVal[0] := NOT_SET;
    SelectsS[MedleyEndSlideId].TextOpt[0].Text := MedleyEndVal[0];
  end
  else // reuse for P1/P2 tag
  begin
    SelectsS[MedleyStartSlideId].Text.Text := 'P1:';
    MedleyStartVal[0] := CurrentSong.DuetNames[0];
    SelectsS[MedleyStartSlideId].TextOpt[0].Text := MedleyStartVal[0];
    SelectsS[MedleyStartSlideId].TextOpt[0].Writable := true;
    SelectsS[MedleyEndSlideId].Text.Text := 'P2:';
    MedleyEndVal[0] := CurrentSong.DuetNames[1];
    SelectsS[MedleyEndSlideId].TextOpt[0].Text := MedleyEndVal[0];
    SelectsS[MedleyEndSlideId].TextOpt[0].Writable := true;
  end;
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
    StartVal[0] := IntToStr(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);
    SelectsS[StartSlideId].TextOpt[0].Text := StartVal[0];
    DurationVal[0] := IntToStr(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration);
    SelectsS[DurationSlideId].TextOpt[0].Text := DurationVal[0];
    ToneVal[0] := IntToStr(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Tone) + ' (' + GetNoteName(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Tone) + ')';
    SelectsS[ToneSlideId].TextOpt[0].Text := ToneVal[0];
    LyricVal[0] := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text;
    SelectsS[LyricSlideId].TextOpt[0].Text := LyricVal[0];
  end;

  // Text Edit Mode
  if TextEditMode or
     TitleEditMode or
     ArtistEditMode or
     LanguageEditMode or
     EditionEditMode or
     GenreEditMode or
     YearEditMode or
     CreatorEditMode or
     P1EditMode or
     P2EditMode then
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
      SingDrawNoteLines(Theme.EditSub.NotesBackground.X, Theme.EditSub.NotesBackground.Y, Theme.EditSub.NotesBackground.X + Theme.EditSub.NotesBackground.W + Xmouse);
      // vertical lines
      EditDrawBeatDelimiters(Theme.EditSub.NotesBackground.X + NotesSkipX, Theme.EditSub.NotesBackground.Y, Theme.EditSub.NotesBackground.W - 2 * NotesSkipX + Xmouse, Theme.EditSub.NotesBackground.H, CurrentTrack);
      // draw notes
      EditDrawLine(Theme.EditSub.NotesBackground.X + NotesSkipX, Theme.EditSub.NotesBackground.Y + 7 * LineSpacing, Theme.EditSub.NotesBackground.W - 2 * NotesSkipX + Xmouse, Theme.EditSub.NotesBackground.W, CurrentTrack);
      // draw text on notes
      DrawText(Theme.EditSub.NotesBackground.X + NotesSkipX, Theme.EditSub.NotesBackground.Y + 7 * LineSpacing, Theme.EditSub.NotesBackground.W - 2 * NotesSkipX + Xmouse, Theme.EditSub.NotesBackground.H, CurrentTrack);
    end
    else
    begin
      // notes background
      EditDrawBorderedBox(Theme.EditSub.NotesBackground.X + Xmouse, Theme.EditSub.NotesBackground.Y, Theme.EditSub.NotesBackground.W - Xmouse, Theme.EditSub.NotesBackground.H, Theme.EditSub.NotesBackground.ColR, Theme.EditSub.NotesBackground.ColG, Theme.EditSub.NotesBackground.ColB, Theme.EditSub.NotesBackground.Alpha);
      // horizontal lines
      SingDrawNoteLines(Theme.EditSub.NotesBackground.X + Xmouse, Theme.EditSub.NotesBackground.Y, Theme.EditSub.NotesBackground.X + Theme.EditSub.NotesBackground.W);
      // vertical lines
      EditDrawBeatDelimiters(Theme.EditSub.NotesBackground.X + NotesSkipX + Xmouse, Theme.EditSub.NotesBackground.Y, Theme.EditSub.NotesBackground.W - 2 * NotesSkipX, Theme.EditSub.NotesBackground.H, CurrentTrack);
      // draw notes
      EditDrawLine(Theme.EditSub.NotesBackground.X + NotesSkipX + Xmouse, Theme.EditSub.NotesBackground.Y + (7/9) * Theme.EditSub.NotesBackground.H, Theme.EditSub.NotesBackground.W - 2 * NotesSkipX, Theme.EditSub.NotesBackground.H, CurrentTrack);
      // draw text on notes
      DrawText(Theme.EditSub.NotesBackground.X + NotesSkipX + Xmouse, Theme.EditSub.NotesBackground.Y + (7/9) * Theme.EditSub.NotesBackground.H, Theme.EditSub.NotesBackground.W - 2 * NotesSkipX, Theme.EditSub.NotesBackground.H, CurrentTrack);
    end;

    if Xmouse <> 0 then
       GoldenRec.KillAll;
  end;

  CurrentSound := AudioInputProcessor.Sound[0];
  CurrentSound.AnalyzeBuffer;
  if (CurrentSound.ToneString <> '-') then
  begin
    Count := trunc((720 / (GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat) - GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat)))*(AudioPlayback.Position - GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat)));
    DrawPlayerTrack(0, 16, 32, 15, CurrentSound.Tone, Count, CurrentNote[CurrentTrack]);
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
      Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].StartBeat +
      Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].Duration) -
      GetTimeFromBeat(Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes[MedleyNotes.start.note].StartBeat);
  end else
    Result := 0;
end;

procedure TScreenEditSub.UpdateMedleyInfo;
begin
  if not MedleyNotes.IsStart and not MedleyNotes.IsEnd then
    Text[TextDebug].Text := ''
  else if not MedleyNotes.IsStart then
    Text[TextDebug].Text := Format(Language.Translate('EDIT_INFO_NO_MEDLEY_START'), [ifthen(MedleyNotes.IsEnd, Format(Language.Translate('EDIT_INFO_MEDLEY_END'), [Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].StartBeat + Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].Duration]))])
  else if not MedleyNotes.IsEnd then
    Text[TextDebug].Text := Format(Language.Translate('EDIT_INFO_NO_MEDLEY_END'), [ifthen(MedleyNotes.IsStart, Format(Language.Translate('EDIT_INFO_MEDLEY_START'), [Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes[MedleyNotes.start.note].StartBeat]))])
  else
    Text[TextDebug].Text := Format(Language.Translate('EDIT_INFO_MEDLEY_LENGTH'), [GetMedleyLength, ifthen(MedleyNotes.isCustom, Language.Translate('EDIT_INFO_MEDLEY_CUSTOM'), Language.Translate('EDIT_INFO_MEDLEY_TXT'))]);

end;

end.
