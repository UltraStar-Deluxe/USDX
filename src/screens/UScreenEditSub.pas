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
  UMain,
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
    start:        TPos;
    end_:         TPos;
    Preview:      TPos;
    isStart:      boolean;   //start beat is declared
    isEnd:        boolean;   //end beat is declared
    isCustom:     boolean;
  end;

  TVisibleHeaders = record
    Title:        UTF8String;
    Artist:       UTF8String;
    Language:     UTF8String;
    Edition:      UTF8String;
    Genre:        UTF8String;
    Year:         Integer;
    Creator:      UTF8String;
    Mp3:          IPath;
    Mp3Id:        Integer;
    Cover:        IPath;
    CoverId:      Integer;
    Background:   IPath;
    BackgroundId: Integer;
    Video:        IPath;
    VideoId:      Integer;
    VideoGAP:     Real;
    BPM:          array of TBPM;
    GAP:          Real;
    StartTag:     Real;
    EndTag:       longint;
    MedleyNotes:  TMedleyNotes;
    PreviewStart: Real;
    Relative:     boolean;
  end;

  TScreenEditSub = class(TMenu)
    private

      CurrentBeat:             Integer;
      //Variable is True if no Song is loaded
      Error:                   boolean;

      TextNote:                Integer;
      TextSentence:            Integer;
      TextInfo:                Integer;

      CurrentNote:             array[0..1] of Integer;
      PlaySentence:            boolean;
      PlayOne:                 boolean;
      PlayOneMidi:             boolean;
      PlaySentenceMidi:        boolean;
      midinotefound:           boolean; // if one note was found
      PlayVideo:               boolean;
      PlayStopTime:            real;
      LastClick:               Integer;
      Click:                   boolean;
      CopySrc:                 TPos;
      {$IFDEF UseMIDIPort}
      MidiOut:                 TMidiOutput;
      MidiStart:               real;
      MidiStop:                real;
      MidiTime:                real;
      MidiPos:                 real;
      MidiLastNote:            Integer;
      {$ENDIF}

      //for mouse move
      LastPressedMouseButton:  boolean;
      LastPressedMouseType:    Integer;
      LastPressedNote:         Integer;
      PressedNoteId:           Integer;

      TextPosition:            Integer;
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
      BPMEditMode:             boolean;
      PianoEditMode:           boolean;
      
      PianoKeysLow: TPianoKeyArray;
      PianoKeysHigh: TPianoKeyArray;

      // to interactive divide note
      LastClickTime:           Integer;

      BackupEditText:          UTF8String; //backup of current text in text-edit-mode
      CurrentEditText:         UTF8String; // current edit text
      editLengthText:          Integer;
      CurrentSlideId:          Integer;
      //title header
      TitleSlideId:            Integer;
      TitleData:               Integer;
      TitleVal:                array of UTF8String;
      SlideTitleIndex:         Integer;
      // artist header
      ArtistSlideId:           Integer;
      ArtistData:              Integer;
      ArtistVal:               array of UTF8String;
      SlideArtistIndex:        Integer;
      // language header
      LanguageSlideId:         Integer;
      LanguageData:            Integer;
      LanguageVal:             array of UTF8String;
      SlideLanguageIndex:      Integer;
      // edition header
      EditionSlideId:          Integer;
      EditionData:             Integer;
      EditionVal:              array of UTF8String;
      SlideEditionIndex:       Integer;
      // genre header
      GenreSlideId:            Integer;
      GenreData:               Integer;
      GenreVal:                array of UTF8String;
      SlideGenreIndex:         Integer;
      // year header
      YearSlideId:             Integer;
      YearData:                Integer;
      YearVal:                 array of UTF8String;
      SlideYearIndex:          Integer;
      // creator header
      CreatorSlideId:          Integer;
      CreatorData:             Integer;
      CreatorVal:              array of UTF8String;
      SlideCreatorIndex:       Integer;
      // mp3 header
      MP3SlideId:              Integer;
      MP3Data:                 Integer;
      MP3Val:                  array of UTF8String;
      SlideMP3Index:           Integer;
      // Cover header
      CoverSlideId:            Integer;
      CoverData:               Integer;
      CoverVal:                array of UTF8String;
      SlideCoverIndex:         Integer;
      // Background header
      BackgroundSlideId:       Integer;
      BackgroundData:          Integer;
      BackgroundVal:           array of UTF8String;
      SlideBackgroundIndex:    Integer;
      // Video header
      VideoSlideId:            Integer;
      VideoData:               Integer;
      VideoVal:                array of UTF8String;
      SlideVideoIndex:         Integer;
      // VideoGap header
      VideoGapSlideId:         Integer;
      VideoGapData:            Integer;
      VideoGapVal:             array of UTF8String;
      SlideVideoGapIndex:      Integer;
      // BPM header
      BPMSlideId:              Integer;
      BPMData:                 Integer;
      BPMVal:                  array of UTF8String;
      SlideBPMIndex:           Integer;
      // GAP header
      GAPSlideId:              Integer;
      GAPData:                 Integer;
      GAPVal:                  array of UTF8String;
      SlideGAPIndex:           Integer;
      // StartTag header
      StartTagSlideId:         Integer;
      StartTagData:            Integer;
      StartTagVal:             array of UTF8String;
      SlideStartTagIndex:      Integer;
      // EndTag header
      EndTagSlideId:           Integer;
      EndTagData:              Integer;
      EndTagVal:               array of UTF8String;
      SlideEndTagIndex:        Integer;
      // MedleyStart header
      MedleyStartSlideId:      Integer;
      MedleyStartData:         Integer;
      MedleyStartVal:          array of UTF8String;
      SlideMedleyStartIndex:   Integer;
      // MedleyEnd header
      MedleyEndSlideId:        Integer;
      MedleyEndData:           Integer;
      MedleyEndVal:            array of UTF8String;
      SlideMedleyEndIndex:     Integer;
      // PreviewStart header
      PreviewStartSlideId:     Integer;
      PreviewStartData:        Integer;
      PreviewStartVal:         array of UTF8String;
      SlidePreviewStartIndex:  Integer;
      // Relative header
      RelativeSlideId:         Integer;
      RelativeData:            Integer;
      RelativeVal:             array of UTF8String;
      SlideRelativeIndex:      Integer;
      // Start header
      StartSlideId:            Integer;
      StartData:               Integer;
      StartVal:                array of UTF8String;
      SlideStartIndex:         Integer;
      // Duration header
      DurationSlideId:         Integer;
      DurationData:            Integer;
      DurationVal:             array of UTF8String;
      SlideDurationIndex:      Integer;
      // Tone header
      ToneSlideId:             Integer;
      ToneData:                Integer;
      ToneVal:                 array of UTF8String;
      SlideToneIndex:          Integer;
      // Text header
      LyricSlideId:            Integer;
      LyricData:               Integer;
      LyricVal:                array of UTF8String;
      SlideLyricIndex:         Integer;
      // Volume Slide
      VolumeAudioSlideId:      Integer;
      VolumeMidiSlideId:       Integer;
      VolumeClickSlideId:      Integer;
      VolumeAudioIndex:        Integer;
      VolumeMidiIndex:         Integer;
      VolumeClickIndex:        Integer; //for update slide

      VolumeAudio:             array of UTF8String;
      VolumeMidi:              array of UTF8String;
      VolumeClick:             array of UTF8String;
      // control buttons
      UndoButtonId:            Integer;
      PreviousSeqButtonID:     Integer;
      NextSeqButtonID:         Integer;
      FreestyleButtonID:       Integer;
      GoldButtonID:            Integer;
      PlayOnlyButtonID:        Integer;
      PlayWithNoteButtonID:    Integer;
      PlayNoteButtonID:        Integer;
      // background image & video preview
      BackgroundImageId:       Integer;
      Empty:                   array of UTF8String;   //temporary variable to initialize slide - todo change
      // background for notes to posibility move note
      //NotesBackgroundId:       Integer;
      // player static picture
      playerIconId:            array[1..2] of Integer;
      //  currentX, CurrentY
      CurrentX:                Integer;
      CurrentY:                Integer;
      LastX:                   Integer;
      LastY:                   Integer;
      Xmouse:                  Integer;
      ResizeNoteLeft:          boolean;
      ResizeNoteRight:         boolean;
      MoveNote:                boolean;

      CurrentTrack:            Integer; // 0 or 1 (for duets)
      EditorLyrics:            array[0..1] of TEditorLyrics;

      //undo declaration
      UndoLines:               array of array of TLines;
      UndoStateNote:           array of array of Integer; //UNDO: note's position
      CurrentUndoLines:        Integer;
      UndoHeader:              array of TVisibleHeaders;

      //video view
      fCurrentVideo: IVideo;

      //singtrack
      CurrentSound:            TCaptureBuffer;
      // Interactive note
      InteractiveNoteId:       array of Integer;
      TransparentNoteButtonId: array of Integer;
      // Interactive Line bar
      InteractiveLineId:       array of Integer;
      TransparentLineButtonId: array of Integer;

      {$IFDEF UseMIDIPort}
      PlayMidi:                boolean;
	  {$ENDIF}// Midi

      // medley
      MedleyNotes:             TMedleyNotes;

      procedure ChangeBPM(newBPM: real);
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
      procedure TransposeNote(Transpose: Integer);
      procedure ChangeWholeTone(Tone: Integer);
      procedure MoveAllToEnd(Move: Integer);
      procedure MoveTextToRight;
      procedure MarkCopySrc;
      procedure CopySentence(SrcTrack, SrcLine, DstTrack, DstLine: Integer; CopyText: boolean = true; CopyNotes: boolean = true; EnforceSrcLength: boolean = false);
      procedure CopySentences(SrcTrack, SrcLine, DstTrack, DstLine, Num: Integer);
      procedure MakeSolo;
      procedure MakeDuet;
      function  DuetCopyLine: boolean;
      function  DuetMoveLine: boolean;
      procedure CopyLine(SrcTrack, SrcLine, DstTrack, DstLine: Integer);
      procedure Refresh;
      procedure CopyToUndo; //copy current lines, mouse position and headers
      procedure CopyFromUndo; //undo last lines, mouse position and headers
      procedure DrawPlayerTrack(CurrentTone: Integer; Count: Integer; CurrentNote: Integer);
      procedure DrawInfoBar(X, Y, W, H: Integer; ColR, ColG, ColB, Alpha: real; Track: Integer);
      procedure DrawText(X, Y, W, H: real; Track: Integer; NumLines: Integer = 10);
      //video view
      procedure StartVideoPreview();
      procedure StopVideoPreview();
      procedure UpdateVideoPosition(NewPosition: real);
      //Note Name Mod
      function GetNoteName(Note: Integer): string;
      // show transparent background note for interactions
      procedure ShowInteractiveBackground;
      function  GetMedleyLength: real; //if available returns the length of the medley in seconds, otherwise 0

    public
      Tex_PrevBackground:      TTexture;
      FadeOut:                 boolean;

      constructor Create; override;
      procedure OnShow; override;
      function  ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      function  ParseInputEditText(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
      function  ParseInputEditBPM(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
      function  ParseInputEditPiano(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
      function  ParseMouse(MouseButton: Integer; BtnDown: boolean; X, Y: Integer): boolean; override;
      function  Draw: boolean; override;
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
  DEFAULT_FADE_IN_TIME  = 8;    //TODO in INI
  DEFAULT_FADE_OUT_TIME = 2;
  NOT_SET = '-';
  NotesSkipX:  Integer = 20;
  LineSpacing: Integer = 15;

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
  DstTrack:     Integer;
  DstLine:      Integer;
  NumLines:     Integer;
  LineIndex:    Integer;
  LineCount:    Integer;
  NoteIndex:    Integer;
  HasPreview:   Boolean;
begin
  Result := true;

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT + KMOD_RALT {+ KMOD_CAPS});

  if PianoEditMode then
  begin
    Result := ParseInputEditPiano(PressedKey, CharCode, PressedDown);
    if (Result = true) then
    begin
      SDL_ModState := KMOD_LSHIFT or KMOD_LCTRL;
      PressedKey := SDLK_SPACE;
    end;
    if (PressedKey = SDLK_RETURN) then
    begin
      PressedKey := SDLK_P;
    end;
      Result := true;
  end;

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
  else if BPMEditMode then
  begin
    Result := ParseInputEditBPM(PressedKey, CharCode, PressedDown)
  end
  else 
  begin

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
              Text[TextInfo].Text := Language.Translate('INFO_FILE_SAVED');
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
              ScreenPopupError.ShowPopup(Language.Translate('EDIT_INFO_DUET_RELATIVE_UNSUPPORTED'));
              Exit;
            end;

            if (CurrentSong.Medley.Source = msTag) then
            begin
              ScreenPopupError.ShowPopup(Language.Translate('EDIT_INFO_MEDLEY_RELATIVE_UNSUPPORTED') + ' ' + Language.Translate('EDIT_INFO_MEDLEY_DELETED'));
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
            Text[TextInfo].Text := Language.Translate('INFO_FILE_SAVED');
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
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_PREVIEW_CLEARED');
            end
            else
            begin
              // clear old IsStartPreview flag
              Tracks[MedleyNotes.Preview.track].Lines[MedleyNotes.Preview.line].Notes[MedleyNotes.Preview.note].IsStartPreview := false;

              // set preview start
              CurrentSong.PreviewStart := Round(GetTimeFromBeat(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat) * 100) / 100;
              CurrentSong.HasPreview := CurrentSong.PreviewStart >= 0.0;
              if (CurrentSong.HasPreview) then
              begin
                MedleyNotes.Preview.track := CurrentTrack;
                MedleyNotes.Preview.line := Tracks[CurrentTrack].CurrentLine;
                MedleyNotes.Preview.note := CurrentNote[CurrentTrack];
                Tracks[MedleyNotes.Preview.track].Lines[MedleyNotes.Preview.line].Notes[MedleyNotes.Preview.note].IsStartPreview := true;
                Text[TextInfo].Text := Format(Language.Translate('EDIT_INFO_PREVIEW_SET'), [CurrentSong.PreviewStart]);
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

              Text[TextInfo].Text := Language.Translate('EDIT_INFO_JUMPTO_PREVIEW_AND_PLAY') + ' (' + FloatToStr(CurrentSong.PreviewStart) + ' s)';
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
              LineIndex := 0;
              while (LineIndex <= Tracks[CurrentTrack].High) and (CurrentBeat > Tracks[CurrentTrack].Lines[LineIndex].EndBeat) do
                Inc(LineIndex);

              if LineIndex <= High(Tracks[CurrentTrack].Lines) then
              begin
                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
                Tracks[CurrentTrack].CurrentLine := LineIndex;

                // finding the right note
                CurrentNote[CurrentTrack] := 0;
                while (CurrentNote[CurrentTrack] <= Tracks[CurrentTrack].Lines[LineIndex].HighNote) and (CurrentBeat > Tracks[CurrentTrack].Lines[LineIndex].Notes[CurrentNote[CurrentTrack]].EndBeat) do
                  Inc(CurrentNote[CurrentTrack]);

                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
                EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
                EditorLyrics[CurrentTrack].Selected := 0;

                Text[TextInfo].Text := Language.Translate('EDIT_INFO_JUMPTO_PREVIEW') + ' (' + FloatToStr(CurrentSong.PreviewStart) + ' s)';
              end;
            end;
          end
          else Text[TextInfo].Text := Language.Translate('EDIT_INFO_NO_PREVIEW');
          Exit;
        end;

      // set Medley tags
      SDLK_A:
        begin
          CopyToUndo;
          if CurrentSong.Relative then
          begin
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_RELATIVE_UNSUPPORTED');
            Exit;
          end;

          if CurrentSong.isDuet then
          begin
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_DUET_UNSUPPORTED');
            Exit;
          end;

          MedleyNotes.isCustom := true;
          CurrentSong.Medley.Source := msTag;
          if SDL_ModState = KMOD_LSHIFT then // medley end note
          begin
            if MedleyNotes.isEnd then // if end is already set
            begin
              if (Tracks[CurrentTrack].CurrentLine = MedleyNotes.end_.line) and (CurrentNote[CurrentTrack] = MedleyNotes.end_.note) then
              begin // current note is medley end, so clear it
                MedleyNotes.isEnd := false;
                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsMedley := false;
                Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_END_CLEARED');
              end else // otherwise, clear old medley end flag and set medley end flag to current note
              begin
                if (MedleyNotes.isStart) and
                   (MedleyNotes.start.line < Tracks[CurrentTrack].CurrentLine) then // ensure that medley start is set and is earlier than medley end
                begin
                  Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsMedley := true;
                  if (Length(Tracks[CurrentTrack].Lines) > MedleyNotes.end_.line) and
                    (Length(Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes) > MedleyNotes.end_.note) then
                    Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].IsMedley := false;
                  MedleyNotes.end_.line := Tracks[CurrentTrack].CurrentLine;
                  MedleyNotes.end_.note := CurrentNote[CurrentTrack];
                  Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_END_SET') + ' (' + FloatToStr(GetMedleyLength) + ' s)';
                end
                else
                begin
                  if (MedleyNotes.isStart) then
                    Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_END_AFTER_MEDLEY_START')
                  else
                    Text[TextInfo].Text := Language.Translate('EDIT_INFO_SET_MEDLEY_START_FIRST');
                end;
              end;
            end else // end is not set yet
            begin
              if (MedleyNotes.isStart) and
                 (MedleyNotes.start.line < Tracks[CurrentTrack].CurrentLine) then // ensure that medley start is set and is earlier than medley end
              begin
                MedleyNotes.isEnd := true;
                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsMedley := true;
                MedleyNotes.end_.line := Tracks[CurrentTrack].CurrentLine;
                MedleyNotes.end_.note := CurrentNote[CurrentTrack];
                Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_END_SET') + ' (' + FloatToStr(GetMedleyLength) + ' s)';
              end
              else
              begin
                if (MedleyNotes.isStart) then
                  Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_END_AFTER_MEDLEY_START')
                else
                  Text[TextInfo].Text := Language.Translate('EDIT_INFO_SET_MEDLEY_START_FIRST');
              end;
            end;
          end else
          begin // medley start note
            if MedleyNotes.isStart then
            begin
              if (Tracks[CurrentTrack].CurrentLine = MedleyNotes.start.line) and (CurrentNote[CurrentTrack] = MedleyNotes.start.note) then
              begin
                MedleyNotes.isStart := false;
                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsMedley := false;
                Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_START_CLEARED');
              end else
              begin
                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsMedley := true;
                if (Length(Tracks[CurrentTrack].Lines) > MedleyNotes.start.line) and
                  (Length(Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes) > MedleyNotes.start.note) then
                  Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes[MedleyNotes.start.note].IsMedley := false;
                MedleyNotes.start.line := Tracks[CurrentTrack].CurrentLine;
                MedleyNotes.start.note := CurrentNote[CurrentTrack];
                Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_START_SET') + ifthen(MedleyNotes.isEnd, '(' + FloatToStr(GetMedleyLength) + ' s)', '');
              end;
            end else
            begin
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsMedley := true;
              MedleyNotes.isStart := true;
              MedleyNotes.start.line := Tracks[CurrentTrack].CurrentLine;
              MedleyNotes.start.note := CurrentNote[CurrentTrack];
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_START_SET');
            end;
          end;

          Exit;
        end;

      // jump to Medley tags
      SDLK_J:
        begin
          if CurrentSong.Relative then
          begin
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_RELATIVE_UNSUPPORTED');
            Exit;
          end;

          if CurrentSong.isDuet then
          begin
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_DUET_UNSUPPORTED');
            Exit;
          end;

          //Medley End Note
          if (SDL_ModState = KMOD_LSHIFT) then
          begin
            if (MedleyNotes.IsEnd) then
            begin
              // simulate sentence switch to clear props
              PreviousSentence;

              if (Length(Tracks[CurrentTrack].Lines) > MedleyNotes.end_.line) and
                 (Length(Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes) > MedleyNotes.end_.note) then
              begin
                Tracks[CurrentTrack].CurrentLine := MedleyNotes.end_.line;
                CurrentNote[CurrentTrack] := MedleyNotes.end_.note;
                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;

                EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
                EditorLyrics[CurrentTrack].Selected := 0;
                Text[TextInfo].Text := Language.Translate('EDIT_INFO_JUMPTO_MEDLEY_END') + ' (' + Language.Translate('EDIT_DURATION') + ' ' + FloatToStr(GetMedleyLength) + ' s)';
              end;
            end
            else
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_NO_MEDLEY_END');
          end;

          if (SDL_ModState = 0) then
          begin
            if (MedleyNotes.IsStart) then
            begin
              // simulate sentence switch to clear props
              PreviousSentence;

              if (Length(Tracks[CurrentTrack].Lines)> MedleyNotes.start.line) and
                 (Length(Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes) > MedleyNotes.start.note) then
              begin
                Tracks[CurrentTrack].CurrentLine := MedleyNotes.start.line;
                CurrentNote[CurrentTrack] := MedleyNotes.start.note;
                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;

                EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
                EditorLyrics[CurrentTrack].Selected := 0;
                Text[TextInfo].Text := Language.Translate('EDIT_INFO_JUMPTO_MEDLEY_START') + ' (' + Language.Translate('EDIT_DURATION') + ' ' + FloatToStr(GetMedleyLength) + ' s)';
              end;
            end
            else
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_NO_MEDLEY_START');
          end;

          if (SDL_ModState = KMOD_LALT) then
          begin
            Writeln('ALT+J');
            // simulate sentence switch to clear props
            PreviousSentence;

            if (MedleyNotes.isStart and MedleyNotes.isEnd) and
              (MedleyNotes.start.line < MedleyNotes.end_.line) and
              (Length(Tracks[CurrentTrack].Lines) > MedleyNotes.end_.line) and
              (Length(Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes) > MedleyNotes.end_.note) and
              (Length(Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes) > MedleyNotes.start.note) then
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
                Text[TextInfo].Text := Language.Translate('EDIT_INFO_JUMPTO_MEDLEY_AND_PLAY');
              end;
            end
            else
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_NO_MEDLEY_SECTION');
          end;

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
          Text[TextInfo].Text := Language.Translate('EDIT_INFO_SONG_RELOADED');
        end;

      SDLK_D:
        begin
          // Divide lengths by 2
          if (SDL_ModState = KMOD_LSHIFT) then
          begin
            CopyToUndo;
            DivideBPM;
            ShowInteractiveBackground;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_DIVIDED_BPM');
            Exit;
          end;

          // create duet or convert duet to normal song
          if (SDL_ModState = KMOD_LCTRL or KMOD_LSHIFT) then
          begin
            if (CurrentSong.isDuet) then
            begin
              MakeSolo;
              FixTimings;
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_CONVERTED_TO_SOLO');
            end
            else
            begin
              MakeDuet;
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_CONVERTED_TO_DUET');
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
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_MULTIPLIED_BPM');
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
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_CAPITALIZATION_CORRECTED');
            end;

          // Correct spaces
          if SDL_ModState = KMOD_LSHIFT then
            begin
              CopyToUndo;
              LyricsCorrectSpaces;
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_SPACES_CORRECTED');
            end;

          // Copy sentence
          if SDL_ModState = KMOD_LCTRL then
          begin
            MarkCopySrc;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_MARKED_FOR_COPY');
          end;

          Exit;
        end;

      SDLK_V:
        begin
          if (SDL_ModState = 0) or (SDL_ModState = KMOD_LALT) then // play current line/remainder of song with video
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
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_PLAY_SONG');
          end;

          // paste notes + text (enforce length of src line)
          if SDL_ModState = KMOD_LCTRL then
          begin
            CopyToUndo;
            CopySentence(CopySrc.track, CopySrc.line, CurrentTrack, Tracks[CurrentTrack].CurrentLine, true, true, true);
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_PASTE_SENTENCE');
          end;

          // paste text only (use minimum of src and dst length)
          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then
          begin
            CopyToUndo;
            CopySentence(CopySrc.track, CopySrc.line, CurrentTrack, Tracks[CurrentTrack].CurrentLine, true, false, false);
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_PASTE_TEXT');
          end;

          // paste notes only (use minimum of src and dst length)
          if SDL_ModState = KMOD_LCTRL + KMOD_LALT then
          begin
            CopyToUndo;
            CopySentence(CopySrc.track, CopySrc.line, CurrentTrack, Tracks[CurrentTrack].CurrentLine, false, true, false);
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_PASTE_NOTES');
          end;

          // paste notes + text (use minimum of src and dst length)
          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then
          begin
            CopyToUndo;
            CopySentence(CopySrc.track, CopySrc.line, CurrentTrack, Tracks[CurrentTrack].CurrentLine, true, true, false);
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_PASTE_TEXT_NOTES');
          end;

          GoldenRec.KillAll;
          ShowInteractiveBackground;
        end;

      SDLK_T:
        begin
          // Fixes timings between sentences
          CopyToUndo;
          FixTimings;
          Text[TextInfo].Text := Language.Translate('EDIT_INFO_FIX_TIMINGS');
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
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE_AUDIO');
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
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE_MIDI');
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
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE_AUDIO_AND_MIDI');
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
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_RAPGOLDEN');
          end
          else if (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType = ntRapGolden) then
          begin
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntNormal;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_NORMAL');
          end
          else
          begin
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntGolden;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_GOLDEN');
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
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_RAP');
          end
          else if (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType = ntRap) then
          begin
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntNormal;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_NORMAL');
          end
          else
          begin
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntFreestyle;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_FREESTYLE');
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
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_UNDO');
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
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_LENGTH_INCREASED');
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
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_BPM_INCREASED_BY') + ' 0.05';
          end;
          if SDL_ModState = KMOD_LSHIFT then
          begin
            CurrentSong.BPM[0].BPM := CurrentSong.BPM[0].BPM + 4; // (1/1)
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_BPM_INCREASED_BY') + ' 1.0';
          end;
          if SDL_ModState = KMOD_LCTRL then
          begin
            CurrentSong.BPM[0].BPM := Round((CurrentSong.BPM[0].BPM * 25) + 1) / 25; // (1/100)
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_BPM_INCREASED_BY') + ' 0.01';
          end;
        end;

      SDLK_MINUS:
        begin
          // Decrease BPM
          CopyToUndo;
          if SDL_ModState = 0 then
          begin
            CurrentSong.BPM[0].BPM := Round((CurrentSong.BPM[0].BPM * 5) - 1) / 5;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_BPM_DECREASED_BY') + ' 0.05';
          end;
          if SDL_ModState = KMOD_LSHIFT then
          begin
            CurrentSong.BPM[0].BPM := CurrentSong.BPM[0].BPM - 4;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_BPM_DECREASED_BY') + ' 1.0';
          end;
          if SDL_ModState = KMOD_LCTRL then
          begin
            CurrentSong.BPM[0].BPM := Round((CurrentSong.BPM[0].BPM * 25) - 1) / 25;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_BPM_DECREASED_BY') + ' 0.01';
          end;
        end;

      SDLK_2,
      SDLK_3,
      SDLK_4,
      SDLK_5,
      SDLK_6:
        begin
          DstTrack := CurrentTrack;
          DstLine := Tracks[CurrentTrack].CurrentLine;
          NumLines := PressedKey - SDLK_0;
          // copy less lines if there are not enough src lines available
          NumLines := Min(NumLines, High(Tracks[CopySrc.track].Lines) - CopySrc.line + 1);
          // copy less lines if there are not enough dst lines available
          NumLines := Min(NumLines, High(Tracks[CurrentTrack].Lines) - DstLine + 1);

          for LineCount := 0 to NumLines - 1 do
          begin
            if (SDL_ModState = KMOD_LCTRL) then
            begin
              CopyToUndo;
              // paste notes + text (use src length, ignore dst length)
              CopySentence(CopySrc.track, CopySrc.line + LineCount, DstTrack, DstLine + LineCount, true, true, true);
              Text[TextInfo].Text := Format(Language.Translate('EDIT_INFO_PASTE_SENTENCE_N'), [NumLines]);
            end;
            if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then
            begin
              CopyToUndo;
              // paste text only (use minimum of src and dst length)
              CopySentence(CopySrc.track, CopySrc.line + LineCount, DstTrack, DstLine + LineCount, true, false, false);
              Text[TextInfo].Text := Format(Language.Translate('EDIT_INFO_PASTE_TEXT_N'), [NumLines]);
            end;
            if SDL_ModState = KMOD_LCTRL + KMOD_LALT then
            begin
              CopyToUndo;
              // paste notes only (use minimum of src and dst length)
              CopySentence(CopySrc.track, CopySrc.line + LineCount, DstTrack, DstLine + LineCount, false, true, false);
              Text[TextInfo].Text := Format(Language.Translate('EDIT_INFO_PASTE_NOTES_N'), [NumLines]);
            end;
            if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then
            begin
              CopyToUndo;
              // paste notes + text (use minimum of src and dst length)
              CopySentence(CopySrc.track, CopySrc.line + LineCount, DstTrack, DstLine + LineCount, true, true, false);
              Text[TextInfo].Text := Format(Language.Translate('EDIT_INFO_PASTE_TEXT_NOTES_N'), [NumLines]);
            end;
          end;
          GoldenRec.KillAll;
          ShowInteractiveBackground;

          // does this insert additional 4 lines before the current line?
          {if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then
          begin
            CopyToUndo;
            CopySentences(CopySrc.track, CopySrc.line, CurrentTrack, Tracks[CurrentTrack].CurrentLine, 4);
            GoldenRec.KillAll;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_COPY_4_SENTENCES');
          end;}
        end;

      SDLK_7:
        begin
          // Decrease VideoGap
          CopyToUndo;
          if SDL_ModState = 0 then
          begin
            CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) - 1) / 100;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_VIDEOGAP_DECREASED_BY') + ' 0.01';
          end;
          if SDL_ModState = KMOD_LSHIFT then
          begin
            CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) - 10) / 100;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_VIDEOGAP_DECREASED_BY') + ' 0.1';
          end;
          if SDL_ModState = KMOD_LCTRL then
          begin
            CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) - 100) / 100;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_VIDEOGAP_DECREASED_BY') + ' 1.0';
          end;
        end;

      SDLK_8:
        begin
          // Increase VideoGap
          CopyToUndo;
          if SDL_ModState = 0 then
          begin
            CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) + 1) / 100;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_VIDEOGAP_INCREASED_BY') + ' 0.01 s';
          end;
          if SDL_ModState = KMOD_LSHIFT then
          begin
            CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) + 10) / 100;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_VIDEOGAP_INCREASED_BY') + ' 0.1 s';
          end;
          if SDL_ModState = KMOD_LCTRL then
          begin
            CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) + 100) / 100;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_VIDEOGAP_INCREASED_BY') + ' 1.0 s';
          end;
        end;

      SDLK_9:
        begin
          // Decrease GAP
          CopyToUndo;
          if SDL_ModState = 0 then
          begin
            CurrentSong.GAP := CurrentSong.GAP - 10;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_GAP_DECREASED_BY') + ' 10 ms';
          end;
          if SDL_ModState = KMOD_LSHIFT then
          begin
            CurrentSong.GAP := CurrentSong.GAP - 1000;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_GAP_DECREASED_BY') + ' 1000 ms';
          end;
        end;
      SDLK_0:
        begin
          // Increase GAP
          CopyToUndo;
          if SDL_ModState = 0 then
          begin
            CurrentSong.GAP := CurrentSong.GAP + 10;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_GAP_INCREASED_BY') + ' 10 ms';
          end;
          if SDL_ModState = KMOD_LSHIFT then
          begin
            CurrentSong.GAP := CurrentSong.GAP + 1000;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_GAP_INCREASED_BY') + ' 1000 ms';
          end;
        end;

      SDLK_KP_PLUS:
        begin
          // Increase tone of all notes
          CopyToUndo;
          if SDL_ModState = 0 then
          begin
            ChangeWholeTone(1);
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_ALL_TONES_INCREASED_BY_SEMITONE');
          end;
          if SDL_ModState = KMOD_LSHIFT then
          begin
            ChangeWholeTone(12);
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_ALL_TONES_INCREASED_BY_OCTAVE');
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
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_ALL_TONES_DECREASED_BY_SEMITONE');
          end;
          if SDL_ModState = KMOD_LSHIFT then
          begin
            ChangeWholeTone(-12);
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_ALL_TONES_DECREASED_BY_OCTAVE');
          end;
          GoldenRec.KillAll;
          ShowInteractiveBackground;
        end;

      SDLK_SLASH, SDLK_HASH, SDLK_KP_DIVIDE:
        begin
          CopyToUndo;
          if SDL_ModState = 0 then
          begin
            // Start a new sentence with currently selected note
            if CurrentNote[CurrentTrack] > 0 then
            begin
              DivideSentence;
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_LINE_DIVIDED');
            end;
            GoldenRec.KillAll;
          end;

          if SDL_ModState = KMOD_LSHIFT then
          begin
            // Join current with subsequent sentence
            if Tracks[CurrentTrack].CurrentLine < Tracks[CurrentTrack].High then
            begin
              JoinSentence;
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_LINES_JOINED');
            end;
            GoldenRec.KillAll;
          end;

          if SDL_ModState = KMOD_LCTRL then
          begin
            // divide note
            DivideNote(false);
            EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
            EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_DIVIDED');
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
          StartTextInput;
        end;

      SDLK_F5:
        begin
          // Enter BPM Edit Mode
          BackupEditText := FloatToStr(CurrentSong.BPM[0].BPM / 4);
          CurrentEditText := BackupEditText;
          CurrentSlideId := BPMSlideId;
          TextPosition := LengthUTF8(BackupEditText);
          editLengthText := LengthUTF8(BackupEditText);
          BPMEditMode := true;
          StartTextInput;
        end;

      SDLK_F6:
        begin
          // Enter Piano Edit Mode
          PianoEditMode := true;
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
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_PLAY_NOTE_AUDIO');
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
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_PLAY_NOTE_MIDI');
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
             StartTextInput;
           end;

           if Interaction = ArtistSlideId then
           begin
             BackupEditText := CurrentSong.Artist;
             CurrentEditText := BackupEditText;
             editLengthText := LengthUTF8(BackupEditText);
             CurrentSlideId := ArtistSlideId;
             TextPosition := LengthUTF8(BackupEditText);
             ArtistEditMode := true;
             StartTextInput;
           end;

           if Interaction = LanguageSlideId then
           begin
             BackupEditText := ifthen(CurrentSong.Language <> 'Unknown', CurrentSong.Language, NOT_SET);
             CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
             editLengthText := LengthUTF8(BackupEditText);
             CurrentSlideId := LanguageSlideId;
             TextPosition := LengthUTF8(BackupEditText);
             LanguageEditMode := true;
             StartTextInput;
           end;

           if Interaction = EditionSlideId then
           begin
             BackupEditText := ifthen(CurrentSong.Edition <> 'Unknown', CurrentSong.Edition, NOT_SET);
             CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
             editLengthText := LengthUTF8(BackupEditText);
             CurrentSlideId := EditionSlideId;
             TextPosition := LengthUTF8(BackupEditText);
             EditionEditMode := true;
             StartTextInput;
           end;

           if Interaction = GenreSlideId then
           begin
             BackupEditText := ifthen(CurrentSong.Genre <> 'Unknown', CurrentSong.Genre, NOT_SET);
             CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
             editLengthText := LengthUTF8(BackupEditText);
             CurrentSlideId := GenreSlideId;
             TextPosition := LengthUTF8(BackupEditText);
             GenreEditMode := true;
             StartTextInput;
           end;

           if Interaction = YearSlideId then
           begin
             BackupEditText := ifthen(CurrentSong.Year <> 0, IntToStr(CurrentSong.Year), NOT_SET);
             CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
             editLengthText := LengthUTF8(BackupEditText);
             CurrentSlideId := YearSlideId;
             TextPosition := LengthUTF8(BackupEditText);
             YearEditMode := true;
             StartTextInput;
           end;

           if Interaction = CreatorSlideId then
           begin
             BackupEditText := ifthen(CurrentSong.Creator <> '', CurrentSong.Creator, NOT_SET);
             CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
             editLengthText := LengthUTF8(BackupEditText);
             CurrentSlideId := CreatorSlideId;
             TextPosition := LengthUTF8(BackupEditText);
             CreatorEditMode := true;
             StartTextInput;
           end;

           // Interaction = 7 // Mp3SlideId
           // Interaction = 8 // CoverSlideId
           // Interaction = 9 // BackgroundSlideId
           // Interaction = 10 // VideoSlideId
           // Interaction = 11 // VideoGapSlideId

           if Interaction = BPMSlideId then
           begin
             BackupEditText := FloatToStr(CurrentSong.BPM[0].BPM / 4);
             CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
             editLengthText := LengthUTF8(BackupEditText);
             CurrentSlideId := BPMSlideId;
             TextPosition := LengthUTF8(BackupEditText);
             BPMEditMode := true;
             StartTextInput;
           end;

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
             StartTextInput;
           end;

           if Interaction = MedleyEndSlideId then
           begin
             BackupEditText := ifthen(CurrentSong.DuetNames[1] <> '', CurrentSong.DuetNames[1], NOT_SET);
             CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
             editLengthText := LengthUTF8(BackupEditText);
             CurrentSlideId := MedleyEndSlideId;
             TextPosition := LengthUTF8(BackupEditText);
             P2EditMode := true;
             StartTextInput;
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
             StartTextInput;
           end;

           if Interaction = 24 then // UndoButtonId
           begin
             CopyFromUndo;
             GoldenRec.KillAll;
             Text[TextInfo].Text := Language.Translate('EDIT_INFO_UNDO');
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
             Text[TextInfo].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE');
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
             Text[TextInfo].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE');
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
             Text[TextInfo].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE');
           end;

           for LineIndex := 0 to Tracks[CurrentTrack].High do
           begin
              if Interaction = InteractiveLineId[LineIndex] then
              begin
                CopyToUndo;
                GoldenRec.KillAll;
                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
                Tracks[CurrentTrack].CurrentLine := LineIndex;
                ShowInteractiveBackground;
                CurrentNote[CurrentTrack] := 0;
                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
                EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
                EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
              end;
           end;

           if high(InteractiveNoteId) >= Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote then
           for NoteIndex := 0 to Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote do
           begin
              if Interaction = InteractiveNoteId[NoteIndex] then
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
                CurrentNote[CurrentTrack] := NoteIndex;
                Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
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
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_DELETE_NOTE');
            GoldenRec.KillAll;
            ShowInteractiveBackground;
          end;

          if (SDL_ModState = KMOD_LCTRL or KMOD_LSHIFT) then
          begin
            // deletes current sentence
            CopyToUndo;
            DeleteSentence;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_DELETE_SENTENCE');
            GoldenRec.KillAll;
            ShowInteractiveBackground;
          end;
        end;

      SDLK_PERIOD:
        begin
          // moves text to right in current sentence
          CopyToUndo;
          MoveTextToRight;
          Text[TextInfo].Text := Language.Translate('EDIT_INFO_MOVE_TEXT_RIGHT');
        end;

      SDLK_RIGHT:
        begin
          // right
          if SDL_ModState = 0 then
          begin
            // clear debug text
            Text[TextInfo].Text := '';
            AudioPlayback.Stop;
            PlaySentence := false;
            PlayOne := false;
            PlayVideo := false;
            {$IFDEF UseMIDIPort}
            //MidiOut.PutShort($B1, $7, Floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
            //MidiOut.PutShort($81, Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].Current].Notes[MidiLastNote].Tone + 60, 127);
            PlaySentenceMidi := false;
            {$endif}
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
            Inc(CurrentNote[CurrentTrack]);
            if CurrentNote[CurrentTrack] > Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote then
              CurrentNote[CurrentTrack] := 0;
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
            EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
          end;

          // ctrl + right
          if SDL_ModState = KMOD_LCTRL then
          begin
            CopyToUndo;
            if Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration > 1 then
            begin
              Dec(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration);
              Inc(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);
              if CurrentNote[CurrentTrack] = 0 then
              begin
                Inc(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].StartBeat);
              end;
            end;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_SHORTENED_AT_START');
            GoldenRec.KillAll;
          end;

          // shift + right
          if SDL_ModState = KMOD_LSHIFT then
          begin
            CopyToUndo;
            Inc(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);
            if CurrentNote[CurrentTrack] = 0 then
            begin
              Inc(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].StartBeat);
            end;
            if CurrentNote[CurrentTrack] = Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote then
              Inc(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat);
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_SHIFTED_RIGHT');
            GoldenRec.KillAll;
          end;

          // alt + right
          if SDL_ModState = KMOD_LALT then
          begin
            CopyToUndo;
            Inc(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration);
            if CurrentNote[CurrentTrack] = Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote then
            begin
              Inc(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat);
            end;
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_LENGTHENED_AT_END');
            GoldenRec.KillAll;
          end;

          // alt + ctrl + shift + right = move all from cursor to right
          if SDL_ModState = KMOD_LALT + KMOD_LCTRL + KMOD_LSHIFT then
          begin
            CopyToUndo;
            MoveAllToEnd(1);
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTES_SHIFTED_RIGHT');
            GoldenRec.KillAll;
          end;
        ShowInteractiveBackground;
        end;

      SDLK_LEFT:
        begin
          // left
          if SDL_ModState = 0 then
          begin
            // clear debug text
            Text[TextInfo].Text := '';
            AudioPlayback.Stop();
            PlaySentence := false;
            PlayOne := false;
            PlayVideo := false;
            {$IFDEF UseMIDIPort}
            //MidiOut.PutShort($B1, $7, Floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
            //MidiOut.PutShort($81, Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].Current].Notes[MidiLastNote].Tone + 60, 127);
            PlaySentenceMidi := false;
            {$endif}

            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
            Dec(CurrentNote[CurrentTrack]);
            if CurrentNote[CurrentTrack] = -1 then
              CurrentNote[CurrentTrack] := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote;
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
            EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
          end;

          // ctrl + left
          if SDL_ModState = KMOD_LCTRL then
          begin
            CopyToUndo;
            Dec(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);
            Inc(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration);
            if CurrentNote[CurrentTrack] = 0 then
              Dec(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].StartBeat);
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_LENGTHENED_AT_START');
            GoldenRec.KillAll;
          end;

          // shift + left
          if SDL_ModState = KMOD_LSHIFT then
          begin
            CopyToUndo;
            Dec(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);

            // resizing sentences
            if CurrentNote[CurrentTrack] = 0 then
            begin
              Dec(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].StartBeat);
            end;

            if CurrentNote[CurrentTrack] = Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote then
              Dec(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat);
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_SHIFTED_LEFT');
            GoldenRec.KillAll;
          end;

          // alt + left
          if SDL_ModState = KMOD_LALT then
          begin
            CopyToUndo;
            if Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration > 1 then
            begin
              Dec(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration);
              if CurrentNote[CurrentTrack] = Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote then
              begin
                Dec(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat);
              end;
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_SHORTENED_AT_END');
            end;
            GoldenRec.KillAll;
          end;

          // alt + ctrl + shift + right = move all from cursor to left
          if SDL_ModState = KMOD_LALT + KMOD_LCTRL + KMOD_LSHIFT then
          begin
            CopyToUndo;
            MoveAllToEnd(-1);
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTES_SHIFTED_LEFT');
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
            Text[TextInfo].Text := '';
            NextSentence;
          end;

          // decrease tone
          if SDL_ModState = KMOD_LSHIFT then
          begin
            CopyToUndo;
            TransposeNote(-1);
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_PITCH_DECREASED');
            GoldenRec.KillAll;
          end;

          // switch to second track, if possible
          if ((SDL_ModState = KMOD_LCTRL) or (SDL_ModState = KMOD_LALT)) and (CurrentSong.isDuet) then
          begin
            if (Length(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes) > 0) then
            begin
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 0;
              CurrentTrack := 1;
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
              EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
              EditorLyrics[CurrentTrack].Selected := 0;
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_SWITCHED_TO_TRACK') + ' 2 (' + CurrentSong.DuetNames[CurrentTrack] + ')';
            end;
          end;

          // copy line from first to second track
          if (CurrentSong.isDuet) and (CurrentTrack = 0) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) then
          begin
            CopyToUndo;
            if (DuetCopyLine) then
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_LINE_COPIED_TO_TRACK') + ' 2'
            else
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_LINE_NOT_COPIED');
          end;

          // move line from first to second track
          if (CurrentSong.isDuet) and (CurrentTrack = 0) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL or KMOD_LALT) then
          begin
            CopyToUndo;
            if (DuetMoveLine) then
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_LINE_MOVED_TO_TRACK') + ' 2'
            else
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_LINE_NOT_MOVED');
          end;

        ShowInteractiveBackground;
        end;

      SDLK_UP:
        begin
          // skip to previous sentence
          if SDL_ModState = 0 then
          begin
            // clear debug text
            Text[TextInfo].Text := '';
            PreviousSentence;
          end;

          // increase tone
          if SDL_ModState = KMOD_LSHIFT then
          begin
            CopyToUndo;
            TransposeNote(1);
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_PITCH_INCREASED');
            GoldenRec.KillAll;
          end;

          // switch to first track, if possible
          if ((SDL_ModState = KMOD_LCTRL) or (SDL_ModState = KMOD_LALT)) and (CurrentSong.isDuet) then
          begin
            if (Length(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes) > 0) then
            begin
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 0;
              CurrentTrack := 0;
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
              EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
              EditorLyrics[CurrentTrack].Selected := 0;
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_SWITCHED_TO_TRACK') + ' 1 (' + CurrentSong.DuetNames[CurrentTrack] + ')';
            end;
          end;

          // copy line from second to first track
          if (CurrentSong.isDuet) and (CurrentTrack = 1) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) then
          begin
            CopyToUndo;
            if (DuetCopyLine) then
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_LINE_COPIED_TO_TRACK') + ' 1'
            else
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_LINE_NOT_COPIED');
          end;

          // move line from second to first track
          if (CurrentSong.isDuet) and (CurrentTrack = 1) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL or KMOD_LALT) then
          begin
            CopyToUndo;
            if (DuetMoveLine) then
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_LINE_MOVED_TO_TRACK') + ' 1'
            else
              Text[TextInfo].Text := Language.Translate('EDIT_INFO_LINE_NOT_MOVED');
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
      Inc(editLengthText);
      Inc(TextPosition);

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
          StopTextInput;
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
          StopTextInput;
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
      SDLK_SLASH, SDLK_KP_DIVIDE:
        begin
          CopyToUndo;
          if SDL_ModState = KMOD_LCTRL then
          begin
            // divide note
            DivideNote(false);
            TextEditMode := false;
            StopTextInput;
            EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
            EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
            GoldenRec.KillAll;
          end;
          ShowInteractiveBackground;
        end;

    end; //case
  end; //if (PressedDown)
end;

function TScreenEditSub.ParseInputEditBPM(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  SDL_ModState:  word;
  qBPM:          real;
begin
  // used when in Text Edit Mode
  Result := true;

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT {+ KMOD_CAPS});

  if (PressedDown) then
  begin
    // only allow digits and decimal separators
    if (CharCode in [48..57, 46]) then    // 48..57 = 0-9, 44 = decimal comma, 46 = decimal point
    begin
      CurrentEditText := UTF8Copy(CurrentEditText, 1,TextPosition) + UCS4ToUTF8String(CharCode) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText));
      Inc(editLengthText);
      Inc(TextPosition);

      Exit;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE:
        begin
          // exit BPM edit mode, restore previous BPM value
          SelectsS[CurrentSlideId].TextOpt[0].Text := FloatToStr(CurrentSong.BPM[0].BPM / 4);
          BPMEditMode := false;
          StopTextInput;
          editLengthText := 0;
          TextPosition := -1;
        end;
      SDLK_F5, SDLK_RETURN:
        begin
          // Exit BPM Edit Mode
          //CopyToUndo;
          if (TryStrToFloat(UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition), qBPM)) then
          begin
            BPMVal[0] := FloatToStr(qBPM * 4);
            ChangeBPM(qBPM * 4);
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
            UpdateSelectSlideOptions(Theme.EditSub.SlideBPM,BPMSlideId,BPMVal,SlideBPMIndex);
            SelectsS[BPMSlideId].TextOpt[0].Align := 0;
            SelectsS[BPMSlideId].TextOpt[0].X := SelectsS[BPMSlideId].TextureSBG.X + 5;
          end
          else
          begin
            CurrentSong.BPM[0].BPM := 0;
            SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
          end;

          BPMEditMode := false;
          StopTextInput;
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
                TextPosition := editLengthText-1;
              end;
          end;
        end;

    end; //case
  end; //if (PressedDown)
end;

function TScreenEditSub.ParseInputEditPiano(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  SDL_ModState:  word;
  Shift: Integer;
  NewNote: Integer;
  i: Integer;
begin
  // used when in Piano Edit Mode
  Result := true;
  NewNote := -1000;
  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT {+ KMOD_CAPS});

  Shift := 0;
  if SDL_ModState = KMOD_LSHIFT then
  begin
    Shift := 12;
  end;


  if PressedDown then
  begin
    // check special keys
    case PressedKey of
      SDLK_ESCAPE, SDLK_F6:
        begin
          PianoEditMode := false;
        end;
    end;
    if PianoEditMode = true then
    begin
      for i := Low(PianoKeysLow) to High(PianoKeysLow) do
      begin
        if PressedKey = PianoKeysLow[i] then
        begin
          NewNote := i - 7 + Shift; // Adjusted index to match existing logic
          Break;
        end;
      end;
      if NewNote = -1000 then // If not found in PianoKeysLow, check PianoKeysHigh
      begin
        for i := Low(PianoKeysHigh) to High(PianoKeysHigh) do
        begin
          if PressedKey = PianoKeysHigh[i] then
          begin
            NewNote := i + 6 + Shift; // Adjusted index to match existing logic
            Break;
          end;
        end;
      end;

      if NewNote <> -1000 then
      begin
        Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Tone := NewNote;
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
      end
      else
        Result := False;
    end; //if (PianoEditMode)
  end; //if (PressedDown)
end;

function TScreenEditSub.ParseMouse(MouseButton: Integer; BtnDown: boolean; X, Y: Integer): boolean;
var
  nBut:   Integer;
  Action: TMouseClickAction;
  TempR:  real;
  i:      Integer;
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
  if not BtnDown then
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

      MoveNote := true;
      ResizeNoteLeft := false;
      ResizeNoteRight := false;
      // check current mouse position to resize note - 20% of left or right note to resize
      if (Interactions[nBut].Typ = iButton) then
      begin
        if CurrentX < Button[Interactions[nBut].Num].X + Button[Interactions[nBut].Num].W * 0.2 then
        begin
          // selected left side note - 20%
          ResizeNoteLeft := true;
          ResizeNoteRight := false;
          MoveNote := false;
        end;

        if CurrentX > Button[Interactions[nBut].Num].X + Button[Interactions[nBut].Num].W - Button[Interactions[nBut].Num].W * 0.2 then
        begin
          // selected right side note - 20%
          ResizeNoteLeft := false;
          ResizeNoteRight := true;
          MoveNote := false;
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
    TempR := 720 / (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat - Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat);
    if (MouseButton = 0) and (LastPressedMouseType = SDL_BUTTON_RIGHT) and (PressedNoteId >=0) then
    begin
      // left & right
      if (Floor((CurrentX - 40) / TempR) > Floor((LastX - 40) / TempR)) or  (Floor((CurrentX - 40) / TempR) < Floor((LastX - 40) / TempR)) then
      begin
        CopyToUndo;
        i := Floor((currentx-40) / Floor(TempR)) - Floor((lastx - 40) / Floor(TempR));
        if MoveNote then
          MoveAllToEnd(i);
        if (ResizeNoteRight) and (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration + i > 0) then
        begin
          MoveAllToEnd(i);
          Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat - i;
          Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration + i;
        end;
        if (ResizeNoteLeft) and (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration - i > 0) then
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
        TransposeNote(-1* Floor((CurrentY-LastY) div (6)));
        LastY := CurrentY;
        GoldenRec.KillAll;
        ShowInteractiveBackground;
      end;
    end;

    //move one note by mouse move
    if (MouseButton = 0) and (LastPressedMouseType = SDL_BUTTON_LEFT) and (PressedNoteId >= 0) then
    begin
      if (Floor((CurrentX - 40) / TempR) > Floor((LastX - 40) / TempR)) or (Floor((CurrentX - 40) / TempR) < Floor((LastX - 40) / TempR)) then
      begin
        CopyToUndo;
        // move left & right
        i := Floor((currentx - 40) / Floor(TempR)) - Floor((lastx - 40) / Floor(TempR));
        if MoveNote then
        begin
            Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat +i;
            if CurrentNote[CurrentTrack] = 0 then
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].StartBeat := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].StartBeat - i;
            if CurrentNote[CurrentTrack] = Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote then
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat + i;
        end;
        // resize note
        if (ResizeNoteRight) and (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration + i > 0) then
        begin
          Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration + i;
          if CurrentNote[CurrentTrack] = Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote then
              Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat + i;
        end;
        if (ResizeNoteLeft) and (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration - i > 0) then
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
        TransposeNote(-1* Floor((CurrentY-LastY) div (6)));
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
  for NoteIndex := 0 to Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].Current].HighNut do
    if (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].Current].Notes[NoteIndex].Start = Czas.CurrentBeat) then
      Music.PlayClick;
end;
}

procedure TScreenEditSub.ChangeBPM(newBPM: real);
var
  TrackIndex: Integer;
  LineIndex:  Integer;
  NoteIndex:  Integer;
  factor:     real;

begin
  factor := newBPM / CurrentSong.BPM[0].BPM;    // e.g. new/old => 1/2 = 0.5 => * 0.5
  CurrentSong.BPM[0].BPM := newBPM;

  for TrackIndex := 0 to High(Tracks) do
  begin
    for LineIndex := 0 to Tracks[TrackIndex].High do
    begin
      Tracks[TrackIndex].Lines[LineIndex].StartBeat := ceil(Tracks[TrackIndex].Lines[LineIndex].StartBeat * factor);
      //Tracks[TrackIndex].Lines[LineIndex].EndBeat   := ceil(Tracks[TrackIndex].Lines[LineIndex].EndBeat * factor);
      if (Length(Tracks[TrackIndex].Lines[LineIndex].Notes) > 0) then
      begin
        for NoteIndex := 0 to Tracks[TrackIndex].Lines[LineIndex].HighNote do
        begin
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat := ceil(Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat * factor);
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration  := floor(Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration * factor);
          if (Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration = 0) then
            Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration := 1;
        end; // NoteIndex
        Tracks[TrackIndex].Lines[LineIndex].EndBeat := Tracks[TrackIndex].Lines[LineIndex].Notes[Tracks[TrackIndex].Lines[LineIndex].HighNote].StartBeat + Tracks[TrackIndex].Lines[LineIndex].Notes[Tracks[TrackIndex].Lines[LineIndex].HighNote].Duration;
      end else
        Tracks[TrackIndex].Lines[LineIndex].EndBeat := round(Tracks[TrackIndex].Lines[LineIndex].EndBeat * factor)
    end; // LineIndex
  end; // TrackIndex
end;

procedure TScreenEditSub.DivideBPM;
begin
  ChangeBPM(CurrentSong.BPM[0].BPM / 2);
end;

procedure TScreenEditSub.MultiplyBPM;
begin
  ChangeBPM(CurrentSong.BPM[0].BPM * 2);
end;

procedure TScreenEditSub.LyricsCapitalize;
var
  TrackIndex:   Integer;
  LineIndex:    Integer;
  Str:          UTF8String;
begin
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
  TrackIndex:   Integer;
  LineIndex:    Integer;
  NoteIndex:    Integer;
  LineStart:    Integer;
  MinLineStart: Integer;
  MaxLineStart: Integer;
  FirstBeat:    Integer;
begin
  FirstBeat := High(Integer);

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

    // adjust GAP accordingly, round to nearest integer value (fractional GAPs make no sense)
    CurrentSong.GAP := round((CurrentSong.GAP + (FirstBeat * 15000) / (CurrentSong.BPM[0].BPM / 4)));

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
        MinLineStart := Notes[HighNote].StartBeat + Notes[HighNote].Duration;
        MaxLineStart := Tracks[TrackIndex].Lines[LineIndex].Notes[0].StartBeat;
        case (MaxLineStart - MinLineStart) of
          0:    LineStart := MaxLineStart;
          1:    LineStart := MaxLineStart;
          2:    LineStart := MaxLineStart - 1;
          3:    LineStart := MaxLineStart - 2;
          else
            if ((MaxLineStart - MinLineStart) >= 4) then
              LineStart := MinLineStart + 2
            else
              LineStart := MaxLineStart;
        end; // case

        Tracks[TrackIndex].Lines[LineIndex].StartBeat := LineStart;
      end; // with
    end; // LineIndex
  end; // TrackIndex
end;

procedure TScreenEditSub.DivideSentence;
var
  LineIndex:  Integer;
  LineStart:  Integer;
  LineNew:    Integer;
  LineLength: Integer;
  NoteIndex:  Integer;
  NoteStart:  Integer;
  NoteHigh:   Integer;
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
  Tracks[CurrentTrack].Lines[LineNew].BaseNote := 0;//High(Integer); // TODO: High (Integer) will causes a memory exception later in this procedure. Weird!
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
  Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
  EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
end;

procedure TScreenEditSub.JoinSentence;
var
  LineIndex: Integer;
  NoteIndex: Integer;
  StartNote: Integer;
  DstNote:   Integer;
begin
  LineIndex := Tracks[CurrentTrack].CurrentLine;

  // add space to last note's syllable
  Tracks[CurrentTrack].Lines[LineIndex].Notes[Tracks[CurrentTrack].Lines[LineIndex].HighNote].Text := Tracks[CurrentTrack].Lines[LineIndex].Notes[Tracks[CurrentTrack].Lines[LineIndex].HighNote].Text + ' ';

  // increase TotalNotes, HighNote and number of Notes in current sentence
  StartNote := Tracks[CurrentTrack].Lines[LineIndex].HighNote + 1;
  Tracks[CurrentTrack].Lines[LineIndex].HighNote := Tracks[CurrentTrack].Lines[LineIndex].HighNote + Length(Tracks[CurrentTrack].Lines[LineIndex+1].Notes);
  SetLength(Tracks[CurrentTrack].Lines[LineIndex].Notes, Tracks[CurrentTrack].Lines[LineIndex].HighNote + 1);

  // copy notes of subsequent sentence to the end of the current sentence
  for NoteIndex := 0 to Tracks[CurrentTrack].Lines[LineIndex+1].HighNote do
  begin
    DstNote := StartNote + NoteIndex;
    Tracks[CurrentTrack].Lines[LineIndex].Notes[DstNote] := Tracks[CurrentTrack].Lines[LineIndex+1].Notes[NoteIndex];
  end;

  // adjust end beat of
  DstNote := Tracks[CurrentTrack].Lines[LineIndex].HighNote;
  Tracks[CurrentTrack].Lines[LineIndex].EndBeat := Tracks[CurrentTrack].Lines[LineIndex].Notes[DstNote].StartBeat +
    Tracks[CurrentTrack].Lines[LineIndex].Notes[DstNote].Duration;

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
      MedleyNotes.start.note := StartNote + MedleyNotes.start.note;
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
      MedleyNotes.end_.note := StartNote + MedleyNotes.end_.note;
    end
    else if (MedleyNotes.end_.line > Tracks[CurrentTrack].CurrentLine + 1) then
    begin
      Dec(MedleyNotes.end_.line);
    end;
  end;

  Refresh;
  Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
  EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
end;

procedure TScreenEditSub.NextSentence;
begin
  {$IFDEF UseMIDIPort}
  //MidiOut.PutShort($B1, $7, Floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
  //MidiOut.PutShort(MIDI_NOTEOFF or 1, Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].Current].Notes[MidiLastNote].Tone + 60, 127);
  PlaySentenceMidi := false;
  PlayOne := false;
  {$ENDIF}
  Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
  Inc(Tracks[CurrentTrack].CurrentLine);
  CurrentNote[CurrentTrack] := 0;
  if Tracks[CurrentTrack].CurrentLine > Tracks[CurrentTrack].High then
    Tracks[CurrentTrack].CurrentLine := 0;
  Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;

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
  //MidiOut.PutShort(MIDI_NOTEOFF or 1, $7, Floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
  //MidiOut.PutShort($81, Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].Current].Notes[MidiLastNote].Tone + 60, 127);
  PlaySentenceMidi := false;
  {$endif}

  Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
  Dec(Tracks[CurrentTrack].CurrentLine);
  CurrentNote[CurrentTrack] := 0;
  if Tracks[CurrentTrack].CurrentLine = -1 then
    Tracks[CurrentTrack].CurrentLine := Tracks[CurrentTrack].High;
  Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;

  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
  EditorLyrics[CurrentTrack].Selected := 0;
  GoldenRec.KillAll;
end;

procedure TScreenEditSub.DivideNote(doubleclick: boolean);
var
  LineIndex:     Integer;
  NoteIndex:     Integer;
  CutPosition:   Integer;
  SpacePosition: Integer;
  TempR:         real;
  TempStr:       UCS4String;
begin
  LineIndex := Tracks[CurrentTrack].CurrentLine;
  TempR := 720 / (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat - Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat);

  if (doubleclick) and (InteractAt(currentX, CurrentY) > 0) then
      CutPosition := Round((currentX - button[Interactions[InteractAt(currentX, CurrentY)].Num].X) / TempR)
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

      TempStr := UTF8ToUCS4String(Notes[CurrentNote[CurrentTrack]].Text);
      if ((UCS4ToUTF8String(TempStr[NoteIndex]) = ' ') and (SpacePosition < 0)) then
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
  CurrentLine: Integer;
  LineIndex:   Integer;
  NoteIndex:   Integer;
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

    Tracks[CurrentTrack].Lines[CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
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

    Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
  end;

  // update lyric display
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
  EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
end;

procedure TScreenEditSub.DeleteSentence;
var
  CurrentLine: Integer;
  LineIndex:   Integer;

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
  Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
end;

procedure TScreenEditSub.TransposeNote(Transpose: Integer);
begin
  Inc(Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Tone, Transpose);
end;

procedure TScreenEditSub.ChangeWholeTone(Tone: Integer);
var
  LineIndex: Integer;
  NoteIndex: Integer;
begin
  for LineIndex := 0 to Tracks[CurrentTrack].High do
  begin
    Tracks[CurrentTrack].Lines[LineIndex].BaseNote := Tracks[CurrentTrack].Lines[LineIndex].BaseNote + Tone;
    for NoteIndex := 0 to Tracks[CurrentTrack].Lines[LineIndex].HighNote do
      Tracks[CurrentTrack].Lines[LineIndex].Notes[NoteIndex].Tone := Tracks[CurrentTrack].Lines[LineIndex].Notes[NoteIndex].Tone + Tone;
  end;
end;

procedure TScreenEditSub.MoveAllToEnd(Move: Integer);
var
  LineIndex: Integer;
  NoteIndex: Integer;
  NoteStart: Integer;
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
  LineIndex: Integer;
  NoteIndex: Integer;
  NoteHigh:  Integer;
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

procedure TScreenEditSub.MarkCopySrc;
begin
  CopySrc.track := CurrentTrack;
  CopySrc.line  := Tracks[CurrentTrack].CurrentLine;
  CopySrc.note  := CurrentNote[CurrentTrack];
end;

procedure TScreenEditSub.CopySentence(SrcTrack, SrcLine, DstTrack, DstLine: Integer; CopyText, CopyNotes, EnforceSrcLength: boolean);
var
  SrcStartBeat: Integer;
  DstStartBeat: Integer;
  BeatDiff:     Integer;
  NoteIndex:    Integer;
begin
  SrcStartBeat := Tracks[SrcTrack].Lines[SrcLine].Notes[0].StartBeat;
  DstStartBeat := Tracks[DstTrack].Lines[DstLine].Notes[0].StartBeat;
  BeatDiff := DstStartBeat - SrcStartBeat;

  if (EnforceSrcLength) then
  begin
    // copy src line as is (even if length of src and dst lines don't match)
    SetLength(Tracks[DstTrack].Lines[DstLine].Notes, Length(Tracks[SrcTrack].Lines[SrcLine].Notes));
    Tracks[DstTrack].Lines[DstLine].HighNote := Tracks[SrcTrack].Lines[SrcLine].HighNote;
  end;

  // if length(src) and length(dst) differ, only copy as many notes as possible (ignoring the rest)
  for NoteIndex := 0 to Min(Tracks[SrcTrack].Lines[SrcLine].HighNote, Tracks[DstTrack].Lines[DstLine].HighNote) do
  begin
    if (CopyText) then // copy text
    begin
      Tracks[DstTrack].Lines[DstLine].Notes[NoteIndex].Text      := Tracks[CopySrc.track].Lines[SrcLine].Notes[NoteIndex].Text;
    end;
    if (CopyNotes) then // copy duration, tone, note type and (shifted) start beat
    begin
      Tracks[DstTrack].Lines[DstLine].Notes[NoteIndex].Duration  := Tracks[SrcTrack].Lines[SrcLine].Notes[NoteIndex].Duration;
      Tracks[DstTrack].Lines[DstLine].Notes[NoteIndex].Tone      := Tracks[SrcTrack].Lines[SrcLine].Notes[NoteIndex].Tone;
      Tracks[DstTrack].Lines[DstLine].Notes[NoteIndex].NoteType  := Tracks[SrcTrack].Lines[SrcLine].Notes[NoteIndex].NoteType;
      Tracks[DstTrack].Lines[DstLine].Notes[NoteIndex].StartBeat := Tracks[SrcTrack].Lines[SrcLine].Notes[NoteIndex].StartBeat + BeatDiff;
      Tracks[DstTrack].Lines[DstLine].Notes[NoteIndex].Color     := Tracks[SrcTrack].Lines[SrcLine].Notes[NoteIndex].Color;
    end;
  end;

  NoteIndex := Tracks[DstTrack].Lines[DstLine].HighNote;
  Tracks[DstTrack].Lines[DstLine].EndBeat := Tracks[DstTrack].Lines[DstLine].Notes[NoteIndex].StartBeat + Tracks[DstTrack].Lines[DstLine].Notes[NoteIndex].Duration;

  Refresh;
  Tracks[DstTrack].Lines[DstLine].Notes[CurrentNote[DstTrack]].Color := P1_INVERTED;
  EditorLyrics[DstTrack].AddLine(DstTrack, Tracks[DstTrack].CurrentLine);
end;

procedure TScreenEditSub.CopySentences(SrcTrack, SrcLine, DstTrack, DstLine, Num: Integer);
var
  LineIndex: Integer;
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

  for LineIndex := 0 to Num - 1 do
    CopySentence(SrcTrack, SrcLine + LineIndex, DstTrack, DstLine + LineIndex);
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
  Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;

  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
end;

procedure TScreenEditSub.MakeDuet;
var
  TrackIndex: Integer;
  LineIndex:  Integer;

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
  SrcLine:      Integer;
  DstLine:      Integer;
  SrcTrack:     Integer;
  DstTrack:     Integer;

  SrcStartBeat: Integer;
  SrcEndBeat:   Integer;

  DstStartBeat: Integer;
  DstEndBeat:   Integer;

  SrcNumN:      Integer;
  DstNumN:      Integer;

  LineIndex1:   Integer;
  LineIndex2:   Integer;

  LineLength:   Integer;
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
  Tracks[SrcTrack].Lines[SrcLine].Notes[CurrentNote[SrcTrack]].Color := P1_INVERTED;
  Result := true;
end;

procedure TScreenEditSub.CopyLine(SrcTrack, SrcLine, DstTrack, DstLine: Integer);
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
  BPMIndex:   Integer;
  TrackIndex: Integer;
  LineIndex:  Integer;
  NoteIndex:  Integer;
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
    UndoHeader[CurrentUndoLines].MedleyNotes := MedleyNotes;
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
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Color          := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Color;
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat      := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat;
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration       := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration;
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Tone           := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Tone;
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text           := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text;
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].NoteType       := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].NoteType;
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].IsMedley       := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].IsMedley;
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].IsStartPreview := Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].IsStartPreview;
      end; //for NoteIndex
    end; //for LineIndex
  end; // TrackIndex
end;

procedure TScreenEditSub.Refresh;
var
  TrackIndex: Integer;
  LineIndex:  Integer;
  NoteIndex:  Integer;

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
  CurrentSong.PreviewStart := Round(GetTimeFromBeat(Tracks[MedleyNotes.Preview.track].Lines[MedleyNotes.Preview.line].Notes[MedleyNotes.Preview.note].StartBeat) * 100) / 100;
end;

procedure TScreenEditSub.CopyFromUndo;
var
  BPMIndex:   Integer;
  TrackIndex: Integer;
  LineIndex:  Integer;
  NoteIndex:  Integer;

begin
CurrentUndoLines := high(UndoLines);

if CurrentUndoLines >= 0 then
begin
  CurrentSong.Title        := Undoheader[CurrentUndoLines].Title;
  CurrentSong.Artist       := Undoheader[CurrentUndoLines].Artist;
  CurrentSong.Language     := Undoheader[CurrentUndoLines].Language;
  CurrentSong.Edition      := Undoheader[CurrentUndoLines].Edition;
  CurrentSong.Genre        := Undoheader[CurrentUndoLines].Genre;
  CurrentSong.Year         := Undoheader[CurrentUndoLines].Year;
  CurrentSong.Creator      := Undoheader[CurrentUndoLines].Creator;
  CurrentSong.Mp3          := Undoheader[CurrentUndoLines].Mp3;
  SelectsS[Mp3SlideId].SelectedOption := Undoheader[CurrentUndoLines].Mp3Id;
  CurrentSong.Cover        := Undoheader[CurrentUndoLines].Cover;
  SelectsS[CoverSlideId].SelectedOption := Undoheader[CurrentUndoLines].CoverId;
  CurrentSong.Background   := Undoheader[CurrentUndoLines].Background;
  SelectsS[BackgroundSlideId].SelectedOption := Undoheader[CurrentUndoLines].BackgroundId;
  CurrentSong.Video        := Undoheader[CurrentUndoLines].Video;
  SelectsS[VideoSlideId].SelectedOption := Undoheader[CurrentUndoLines].VideoId;
  CurrentSong.VideoGAP     := Undoheader[CurrentUndoLines].VideoGAP;
  SetLength(CurrentSong.BPM, length(Undoheader[CurrentUndoLines].BPM));
  for BPMIndex := 0 to High(Undoheader[CurrentUndoLines].BPM) do
  begin
    CurrentSong.BPM[BPMIndex].BPM := Undoheader[CurrentUndoLines].BPM[BPMIndex].BPM;
    CurrentSong.BPM[BPMIndex].StartBeat := Undoheader[CurrentUndoLines].BPM[BPMIndex].StartBeat;
  end;
  CurrentSong.GAP          := Undoheader[CurrentUndoLines].GAP;
  CurrentSong.Start        := Undoheader[CurrentUndoLines].StartTag;
  CurrentSong.Finish       := Undoheader[CurrentUndoLines].EndTag;
  MedleyNotes := Undoheader[CurrentUndoLines].MedleyNotes;
  CurrentSong.PreviewStart := Undoheader[CurrentUndoLines].PreviewStart;
  CurrentSong.Relative     := Undoheader[CurrentUndoLines].Relative;

  for TrackIndex := 0 to High(Tracks) do
  begin
    CurrentNote[TrackIndex] := UndoStateNote[High(UndoStateNote), TrackIndex];

    Tracks[TrackIndex].CurrentLine := UndoLines[CurrentUndoLines, TrackIndex].CurrentLine;
    Tracks[TrackIndex].High        := UndoLines[CurrentUndoLines, TrackIndex].High;
    Tracks[TrackIndex].Number      := UndoLines[CurrentUndoLines, TrackIndex].Number;
    Tracks[TrackIndex].Resolution  := UndoLines[CurrentUndoLines, TrackIndex].Resolution;
    Tracks[TrackIndex].NotesGAP    := UndoLines[CurrentUndoLines, TrackIndex].NotesGAP;
    Tracks[TrackIndex].ScoreValue  := UndoLines[CurrentUndoLines, TrackIndex].ScoreValue;
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
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Color          := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Color;
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat      := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat;
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration       := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration;
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Tone           := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Tone;
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text           := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text;
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].NoteType       := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].NoteType;
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].IsMedley       := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].IsMedley;
          Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].IsStartPreview := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].IsStartPreview;
        end; //for NoteIndex
    end; //for LineIndex
  end; //for TrackIndex
  SetLength(UndoStateNote, high(UndoStateNote));
  SetLength(UndoHeader, high(UndoLines));
  SetLength(UndoLines, high(UndoLines));
  Text[TextInfo].Text := Language.Translate('EDIT_INFO_UNDO');

  // to refresh all headers
  SelectsS[TitleSlideId].TextOpt[0].Text    := CurrentSong.Title;
  SelectsS[ArtistSlideId].TextOpt[0].Text   := CurrentSong.Artist;
  SelectsS[LanguageSlideId].TextOpt[0].Text := CurrentSong.Language;
  SelectsS[EditionSlideId].TextOpt[0].Text  := CurrentSong.Edition;
  SelectsS[GenreSlideId].TextOpt[0].Text    := CurrentSong.Genre;
  SelectsS[YearSlideId].TextOpt[0].Text     := IntToStr(CurrentSong.Year);
  SelectsS[CreatorSlideId].TextOpt[0].Text  := CurrentSong.Creator;
  SelectsS[LyricSlideId].TextOpt[0].Text    := Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text;
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, Tracks[CurrentTrack].CurrentLine);
  EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
end; //if CurrentUndoLines
end;

procedure TScreenEditSub.DrawPlayerTrack(CurrentTone: Integer; Count: Integer; CurrentNote: Integer);
var
  Rec:     TRecR;
  scale:   Integer;
  HalfToneHeight: real;
  BarWidth: real;
  BarHeight: real;
begin
  glColor3f(1, 1, 1);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  BarWidth := 16;
  BarHeight := 16;
  HalfToneHeight := 7.5;
  Rec.Right := 40 + 0.5 + 10 * ScreenX + Count;
  Rec.Left  := Rec.Right - BarWidth;

  scale := Round((CurrentTone - Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote].Tone) / 12);

  Rec.Top := 429 - (CurrentTone - 12*scale - Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].BaseNote) * HalfToneHeight - 0.5 * BarHeight;
  Rec.Bottom := Rec.Top + BarHeight;

  glColor3f(1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, Tex_Lyric_Help_Bar.TexNum);
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
    glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
    glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
    glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
  glEnd;

  // draw currently recorded tone
  SetFontPos(Theme.EditSub.TextCurrentTone.X, Theme.EditSub.TextCurrentTone.Y);
  SetFontSize(Theme.EditSub.TextCurrentTone.Size);
  glColor4f(Theme.EditSub.TextCurrentTone.ColR, Theme.EditSub.TextCurrentTone.ColG, Theme.EditSub.TextCurrentTone.ColB, 1);
  glPrint(GetNoteName(CurrentTone));
end;

procedure TScreenEditSub.DrawInfoBar(X, Y, W, H: Integer; ColR, ColG, ColB, Alpha: real; Track: Integer);
var
  StartBeat:    Integer;
  EndBeat:      Integer;
  SongStart:    Integer;
  SongEnd:      Integer;
  SongDuration: Integer;
  i:            Integer;
  Color:        TRGB;

  CurrentPos: real;
  Width:      real;

  LineIndex: Integer;
  numLines:  Integer;

  function FindStartBeat(): Integer;
  var
    TrackIndex: Integer;
    LineIndex:  Integer;
  begin
    Result := High(Integer);

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

  function FindEndBeat(): Integer;
  var
    TrackIndex: Integer;
    LineIndex:  Integer;
    NoteIndex:  Integer;
  begin
    Result := Low(Integer);

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

  while (Length(TransparentLineButtonId) < numLines) do
  begin
    SetLength(InteractiveLineId, Length(InteractiveLineId) + 1);
    SetLength(TransparentLineButtonId, Length(TransparentLineButtonId) + 1);
    TransparentLineButtonId[Length(TransparentLineButtonId) - 1] := AddButton(0, 0, 0, 0,PATH_NONE);
    //TransparentLineButtonId[Length(TransparentLineButtonId) - 1] := AddButton(0, 0, 0, 0, Skin.GetTextureFileName(Theme.Main.Buttonsolo.Tex));
    InteractiveLineId[Length(InteractiveLineId) - 1] := Length(Interactions) - 1;
  end;

  for LineIndex := 0 to numLines - 1 do
  begin
    if (LineIndex = Tracks[Track].CurrentLine) and not (PlaySentence or PlaySentenceMidi or PlayOne) then
      glColor4f(1, 0.6, 0, 1) // currently selected line in orange
    else
      if (CurrentSong.Medley.Source <> msNone) and
         (MedleyNotes.isStart) and (MedleyNotes.isEnd) and
         (LineIndex >= MedleyNotes.start.line) and (LineIndex <= MedleyNotes.end_.line) then
        // medley section in green
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
    EndBeat := Tracks[Track].Lines[LineIndex].Notes[Tracks[Track].Lines[LineIndex].HighNote].StartBeat + Tracks[Track].Lines[LineIndex].Notes[Tracks[Track].Lines[LineIndex].HighNote].Duration;

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

procedure TScreenEditSub.DrawText(X, Y, W, H: real; Track: Integer; NumLines: Integer);
var
  Rec:   TRecR;
  Count: Integer;
  TempR: real;
  Space: real;
  Str:   UTF8String;

  //PlayerNumber:  Integer;
  OrgFont:  TFont;

  GoldenStarPos: real;
begin
  if ((1 shl Track) <> 0) then
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

      OrgFont := CurrentFont;
      glColor4f(0, 0, 0, 1);
      SetFontFamily(0);
      SetFontStyle(ftBold);
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
          if (CurrentSong.Medley.Source <> msNone) and (MedleyNotes.isStart) and (Tracks[Track].CurrentLine = MedleyNotes.start.line) and (Count = MedleyNotes.start.note) then
          begin
            Str := Language.Translate('EDIT_MEDLEYSTART');
            Str := '| ' + Copy(Str, 1, Length(Str) - 1) + ' (' + Language.Translate('EDIT_DURATION') + ' ' + FloatToStr(GetMedleyLength) + ' s)';
            SetFontPos(Rec.Left - 0.5 - NotesW[0], Rec.Top + Space);
            glColor4f(0.15, 0.75, 0.15, 1);
            glPrint(Str);
            glColor4f(0, 0, 0, 1);
          end;
          // add info if current note is medley end
          if (CurrentSong.Medley.Source <> msNone) and (MedleyNotes.isEnd) and (Tracks[Track].CurrentLine = MedleyNotes.end_.line) and (Count = MedleyNotes.end_.note) then
          begin
            Str := Language.Translate('EDIT_MEDLEYEND');
            Str := '(' + Language.Translate('EDIT_DURATION') + ' ' + FloatToStr(GetMedleyLength) + ' s) ' + Copy(Str, 1, Length(Str) - 1) + ' |';
            SetFontPos(Rec.Right + 0.5 + NotesW[0] - glTextWidth(Str), Rec.Top + Space);
            glColor4f(0.15, 0.75, 0.15, 1);
            glPrint(Str);
            glColor4f(0, 0, 0, 1);
          end;
          if (CurrentSong.HasPreview) and (IsStartPreview) then
          begin
            Str := Language.Translate('EDIT_PREVIEWSTART');
            Str := '| ' + Copy(Str, 1, Length(Str) - 1 ) + ' (' + FloatToStr(CurrentSong.PreviewStart) + ' s)';
            SetFontPos(Rec.Left - 0.5 - NotesW[0], Rec.Top - Space);
            //glColor4f(0.15, 0.75, 0.15, 1);
            glPrint(Str);
            //glColor4f(0, 0, 0, 1);
          end;
        end; // with
      end; // for
    end; // with

    // revert the font to prevent conflicts within drawing the editor lyric line
    SetFont(OrgFont);

    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
  end;
end;

// show transparent background for interactive note

procedure TScreenEditSub.ShowInteractiveBackground;
var
  TempR:     real;
  NoteIndex: Integer;
begin

  for NoteIndex := 0 to High(TransparentNoteButtonId) do
  begin
    Button[TransparentNoteButtonId[NoteIndex]].SetX(0);
    Button[TransparentNoteButtonId[NoteIndex]].SetY(0);
    Button[TransparentNoteButtonId[NoteIndex]].SetW(0);
    Button[TransparentNoteButtonId[NoteIndex]].SetH(0);
  end;

// adding transparent buttons
  while (Length(TransparentNoteButtonId) - 1 < Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote) do
  begin
    SetLength(InteractiveNoteId, Length(InteractiveNoteId) + 1);
    SetLength(TransparentNoteButtonId, Length(TransparentNoteButtonId) + 1);
    TransparentNoteButtonId[Length(TransparentNoteButtonId) - 1] := AddButton(0, 0, 0, 0, PATH_NONE);
    // for debug purposes: use some button texture instead of a transparent button (comment out line above, uncomment line below)
    //TransparentNoteButtonId[Length(TransparentNoteButtonId)-1] := AddButton(0, 0, 0, 0, Skin.GetTextureFileName(Theme.Main.Buttonsolo.Tex));
    InteractiveNoteId[Length(InteractiveNoteId) - 1] := Length(Interactions) - 1;
  end;
  TempR := 720 / (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].EndBeat - Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat);
  for NoteIndex := 0 to Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote do
  begin
    Button[TransparentNoteButtonId[NoteIndex]].SetX(Theme.EditSub.NotesBackground.X + NotesSkipX + (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[NoteIndex].StartBeat - Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat) * TempR + 0.5 + 10*ScreenX);
    Button[TransparentNoteButtonId[NoteIndex]].SetY(Theme.EditSub.NotesBackground.Y + 7 * LineSpacing - (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[NoteIndex].Tone - Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].BaseNote) * LineSpacing / 2 - NotesH[0]);
    Button[TransparentNoteButtonId[NoteIndex]].SetW((Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[NoteIndex].Duration) * TempR - 0.5 + 10*(ScreenX));
    Button[TransparentNoteButtonId[NoteIndex]].SetH(2 * NotesH[0]);
  end;
end;

// from revision 2475
procedure TScreenEditSub.StartVideoPreview;
var
  VideoFile: IPath;

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
  playerIconId[1] := AddStatic(Theme.Score.StaticPlayerIdBox[1]);
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

  // info text
  TextInfo := AddText(Theme.EditSub.TextInfo);

  // in notes place -> for move notes by mouse
  //NotesBackgroundId := AddSelectSlide(Theme.EditSub.NotesBackground, i, Empty);

  // Initialize Piano Keys to default values
  PianoKeysLow := Ini.PianoKeysLow;
  PianoKeysHigh := Ini.PianoKeysHigh;

end;

procedure TScreenEditSub.OnShow;
const
  SUPPORTED_EXTS_AUDIO: array[0..4]  of string = ('.mp3', '.flac', '.wav', '.ogg', '.m4a');
  SUPPORTED_EXTS_IMAGE: array[0..1]  of string = ('.jpg', '.png');
  SUPPORTED_EXTS_VIDEO: array[0..11] of string = ('.avi', '.mov', '.divx', '.mkv', '.mpeg', '.mpg', '.mp4', '.mpeg', '.m2v', '.ts', '.webm', '.wmv');
var
  FileExt:     IPath;
  Files:       TPathDynArray;
  FileIndex:   Integer;
  TrackIndex:  Integer;
  VolumeIndex: Integer;
  Ext:         string;

  function IsBeatMatchingNote(beat: Integer; Note: TLineFragment): boolean;
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
  Text[TextInfo].Text := '';
  Log.LogStatus('Initializing', 'TEditScreen.OnShow');
  Xmouse := 0;

  ResetSingTemp;
  GoldenRec.KillAll;
//  SetLength(UndoSong, 0);
  SetLength(UndoLines, 0);
  SetLength(UndoStateNote, 0);
  SetLength(Undoheader, 0);

  try
    // reread header with custom tags
    Error := not CurrentSong.Analyse(true, false);

    // with the duet/medley code, TSong.Analyse is already loading the song
    //if not Error then
    //  Error := not CurrentSong.LoadSong(false);
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
      Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenEditSub');

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

    for FileIndex := 0 to High(Files) do
    begin
      SetLength(MP3Val, High(MP3Val) + 2);
      MP3Val[FileIndex] := filesystem.ExtractFileName(Files[FileIndex]).ToUTF8;
      if (UTF8CompareText(MP3Val[FileIndex],CurrentSong.Mp3.ToUTF8) = 0) then
            SlideMP3Index := FileIndex;
    end;
    UpdateSelectSlideOptions(Theme.EditSub.SlideMP3,MP3SlideId,MP3Val,SlideMP3Index);

    // Header Cover
    SetLength(Files, 0);
    SetLength(CoverVal, 0);
    SlideCoverIndex := -1;

    for Ext in SUPPORTED_EXTS_IMAGE do
      Songs.FindFilesByExtension(Path(includeTrailingPathDelimiter(CurrentSong.Path.ToNative)), Path(Ext), true, Files);

    for FileIndex := 0 to High(Files) do
    begin
      SetLength(CoverVal, High(CoverVal) + 2);
      CoverVal[FileIndex] := ExtractFileName(Files[FileIndex].ToUTF8());
      if UTF8CompareText(CoverVal[FileIndex], CurrentSong.Cover.ToUTF8) = 0 then
            SlideCoverIndex := FileIndex;
    end;

    if High(Files) < 0 then
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

    for FileIndex := 0 to High(Files) do
    begin
      SetLength(BackgroundVal, High(BackgroundVal) + 2);
      BackgroundVal[FileIndex] := ExtractFileName(Files[FileIndex].ToUTF8());
      if UTF8CompareText(BackgroundVal[FileIndex], CurrentSong.Background.ToUTF8) = 0 then
            SlideBackgroundIndex := FileIndex;
    end;

    if High(Files) < 0 then
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

    for FileIndex := 0 to High(Files) do
    begin
      SetLength(VideoVal, High(VideoVal) + 2);
      VideoVal[FileIndex] := ExtractFileName(Files[FileIndex].ToUTF8());
      if UTF8CompareText(VideoVal[FileIndex], CurrentSong.Video.ToUTF8) = 0 then
            SlideVideoIndex := FileIndex;
    end;

    if High(Files) < 0 then
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
    for VolumeIndex := 0 to 100 do
    begin
      SetLength(VolumeAudio, High(VolumeAudio) + 2);
      SetLength(VolumeMidi,  High(VolumeMidi)  + 2);
      SetLength(VolumeClick, High(VolumeClick) + 2);
      VolumeAudio[VolumeIndex] := IntToStr(VolumeIndex);
      VolumeMidi[VolumeIndex]  := IntToStr(VolumeIndex);
      VolumeClick[VolumeIndex] := IntToStr(VolumeIndex);
    end;
    VolumeAudioIndex := 100;
    VolumeMidiIndex  := 100;
    VolumeClickIndex := 100;
    UpdateSelectSlideOptions(Theme.EditSub.SelectVolAudio, VolumeAudioSlideId, VolumeAudio, VolumeAudioIndex);
    UpdateSelectSlideOptions(Theme.EditSub.SelectVolMidi,  VolumeMidiSlideId,  VolumeMidi,  VolumeMidiIndex);
    UpdateSelectSlideOptions(Theme.EditSub.SelectVolClick, VolumeClickSlideId, VolumeClick, VolumeClickIndex);

    for TrackIndex := 0 to High(Tracks) do
    begin
      Tracks[TrackIndex].CurrentLine := 0;
      CurrentNote[TrackIndex] := 0;
      Tracks[TrackIndex].Lines[0].Notes[0].Color := P1_INVERTED;
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

      MedleyNotes.isEnd   := (MedleyNotes.end_.line > 0)  or (MedleyNotes.end_.note  >= 0);
      MedleyNotes.isStart := (MedleyNotes.start.line > 0) or (MedleyNotes.start.note >= 0);
    end;

    // set preview start
    MedleyNotes.Preview := FindNote(round(GetMidBeat(CurrentSong.PreviewStart - CurrentSong.GAP/1000)));
    Tracks[MedleyNotes.Preview.track].Lines[MedleyNotes.Preview.line].Notes[MedleyNotes.Preview.note].IsStartPreview := true;
    CurrentSong.PreviewStart := round(GetTimeFromBeat(Tracks[MedleyNotes.Preview.track].Lines[MedleyNotes.Preview.line].Notes[MedleyNotes.Preview.note].StartBeat) * 1000) / 1000;

    for TrackIndex := 0 to High(Tracks) do
    begin
      EditorLyrics[TrackIndex].Clear;

      EditorLyrics[TrackIndex].X     := Theme.EditSub.TextSentence.X;
      EditorLyrics[TrackIndex].Y     := Theme.EditSub.TextSentence.Y;
      EditorLyrics[TrackIndex].Align := Theme.EditSub.TextSentence.Align;
      EditorLyrics[TrackIndex].Size  := Theme.EditSub.TextSentence.Size;
      EditorLyrics[TrackIndex].ColR  := Theme.EditSub.TextSentence.ColR;
      EditorLyrics[TrackIndex].ColG  := Theme.EditSub.TextSentence.ColG;
      EditorLyrics[TrackIndex].ColB  := Theme.EditSub.TextSentence.ColB;
      EditorLyrics[TrackIndex].DColR := Theme.EditSub.TextSentence.DColR;
      EditorLyrics[TrackIndex].DColG := Theme.EditSub.TextSentence.DColG;
      EditorLyrics[TrackIndex].DColB := Theme.EditSub.TextSentence.DColB;

      EditorLyrics[TrackIndex].AddLine(TrackIndex, 0);
      EditorLyrics[TrackIndex].Selected := 0;
    end;

    NotesH[0] := 7;
    NotesW[0] := 4;
    ResizeNoteLeft := false;
    ResizeNoteRight := false;
    MoveNote := false;
    //show transparent background for notes
    ShowInteractiveBackground;
    // user input tracking
    AudioInput.CaptureStart;
  end;

  // background picture
  try
    if (not (CurrentSong.Background = PATH_NONE) and CurrentSong.Path.Append(CurrentSong.Background).Exists) then
    begin
      Tex_PrevBackground := Texture.LoadTexture(CurrentSong.Path.Append(CurrentSong.Background));
      Texture.AddTexture(Tex_PrevBackground, TEXTURE_TYPE_PLAIN, true);
      Statics[BackgroundImageId].Texture   := Tex_PrevBackground;
      Statics[BackgroundImageId].Texture.X := Theme.EditSub.BackgroundImage.X;
      Statics[BackgroundImageId].Texture.Y := Theme.EditSub.BackgroundImage.Y;
      Statics[BackgroundImageId].Texture.W := Theme.EditSub.BackgroundImage.W;
      Statics[BackgroundImageId].Texture.H := Theme.EditSub.BackgroundImage.H;
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
  BPMEditMode := false;
  StopTextInput;

  editLengthText := 0;
  TextPosition := -1;
end;

function TScreenEditSub.Draw: boolean;
var
  LastLine:  Integer;
  NoteIndex: Integer;
  Count:     Integer;
begin
  {$IFDEF UseMIDIPort} // midi music
  if PlaySentenceMidi and Not (PlayOneMidi) then
  begin
    MidiPos := USTime.GetTime - MidiTime + MidiStart;

    // stop the music
    if (MidiPos > MidiStop) then
    begin
      MidiOut.PutShort($B1, $7, Floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
      MidiOut.PutShort(MIDI_NOTEOFF or 1, Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[MidiLastNote].Tone + 60, 127);
      PlaySentenceMidi := false;
    end;

    // click
    CurrentBeat := Floor(GetMidBeat(MidiPos - CurrentSong.GAP / 1000));
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_CURRENT_BEAT') + ' ' + IntToStr(CurrentBeat);

    if CurrentBeat <> LastClick then
    begin
      for NoteIndex := 0 to Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote do
        if (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[NoteIndex].StartBeat = CurrentBeat) then
        begin
          LastClick := CurrentBeat;
          MidiOut.PutShort($B1, $7, Floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
          if NoteIndex > 0 then
            MidiOut.PutShort(MIDI_NOTEOFF or 1, Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[NoteIndex-1].Tone + 60, 127);
          MidiOut.PutShort($91, Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[NoteIndex].Tone + 60, 127);
          MidiLastNote := NoteIndex;
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
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_CURRENT_BEAT') + ' ' + IntToStr(CurrentBeat);
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
        Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
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
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_CURRENT_BEAT') + ' ' + IntToStr(CurrentBeat);
      if CurrentBeat <> LastClick then
      begin
        for NoteIndex := 0 to Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].HighNote do
          if (Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[NoteIndex].StartBeat = CurrentBeat) then
          begin
            SoundLib.Click.Volume := SelectsS[VolumeClickSlideId].SelectedOption / 100;
            AudioPlayback.PlaySound(SoundLib.Click);
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
      MidiOut.PutShort($B1, $7, Floor(1.27 * SelectsS[VolumeMidiSlideId].SelectedOption));
      MidiOut.PutShort($81, Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Tone + 60, 127);
      MidiOut.PutShort(MIDI_STOP, 0, 0);
      PlayOneMidi := false;
    end;

    // click
    CurrentBeat := Floor(GetMidBeat(MidiPos - CurrentSong.GAP / 1000));
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_CURRENT_BEAT') + ' ' + IntToStr(CurrentBeat);

    if ((CurrentBeat <> LastClick) and Not (midinotefound)) then
    begin
//      for NoteIndex := 0 to Tracks[CurrentTrack].Lines[Lines[CurrentTrack].Current].HighNote do
//      begin
        if ((Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat <= CurrentBeat) and
        ((Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat + Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration) > CurrentBeat)) then
        begin
          LastClick := CurrentBeat;
          midinotefound := true;
          MidiOut.PutShort($B1, $7, Floor(1.27 * SelectsS[VolumeMidiSlideId].SelectedOption));
//          if NoteIndex > 0 then
            MidiOut.PutShort($81, Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Tone + 60, 127);
          MidiOut.PutShort($91, Tracks[CurrentTrack].Lines[Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Tone + 60, 127);

          MidiLastNote := NoteIndex;
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
  if not BPMEditMode then
  begin
    BPMVal[0] := FloatToStr(CurrentSong.BPM[0].BPM / 4);
    SelectsS[BPMSlideId].TextOpt[0].Text := BPMVal[0];
  end;
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
     P2EditMode or
     BPMEditMode then
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
    DrawPlayerTrack(CurrentSound.Tone, Count, CurrentNote[CurrentTrack]);
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
    fCurrentVideo.AspectCorrection := acoLetterBox;
    fCurrentVideo.Draw;
  end;

  Result := true;
end;

procedure TScreenEditSub.OnHide;
begin
  {$IFDEF UseMIDIPort}
  MidiOut.Close;
  MidiOut.Free;
  {$ENDIF}

  //Music.SetVolume(1.0);
  AudioInput.CaptureStop;
end;

function TScreenEditSub.GetNoteName(Note: Integer): string;
var
  NormNote: Integer;
  Octave:   Integer;
const
  SUB_MINUS = #$E2#$82#$8B;
  SUB_0     = #$E2#$82#$80;
  SUB_1     = #$E2#$82#$81;
  SUB_2     = #$E2#$82#$82;
  SUB_3     = #$E2#$82#$83;
  SUB_4     = #$E2#$82#$84;
  SUB_5     = #$E2#$82#$85;
  SUB_6     = #$E2#$82#$86;
  SUB_7     = #$E2#$82#$87;
  SUB_8     = #$E2#$82#$88;
  SUB_9     = #$E2#$82#$89;
begin
  if (Note >= 0) then
  begin
    NormNote := Note mod 12;
    Octave := Note div 12;
  end
  else
  begin
    NormNote := (Note + (-Trunc(Note/12)+1)*12) mod 12;
    Octave := (Note + 1) div 12 - 1;
  end;

  case NormNote of
     0: Result := Language.Translate('EDIT_NOTENAME_C');
     1: Result := Language.Translate('EDIT_NOTENAME_C_SHARP');
     2: Result := Language.Translate('EDIT_NOTENAME_D');
     3: Result := Language.Translate('EDIT_NOTENAME_D_SHARP');
     4: Result := Language.Translate('EDIT_NOTENAME_E');
     5: Result := Language.Translate('EDIT_NOTENAME_F');
     6: Result := Language.Translate('EDIT_NOTENAME_F_SHARP');
     7: Result := Language.Translate('EDIT_NOTENAME_G');
     8: Result := Language.Translate('EDIT_NOTENAME_G_SHARP');
     9: Result := Language.Translate('EDIT_NOTENAME_A');
    10: Result := Language.Translate('EDIT_NOTENAME_A_SHARP');
    11: Result := Language.Translate('EDIT_NOTENAME_B');
  end;

  case Octave of
    -3: Result := Result + SUB_MINUS + SUB_1; // subsub-contra: '''C - '''B
    -2: Result := Result + SUB_0;             // sub-contra:     ''C - ''B
    -1: Result := Result + SUB_1;             // contra:          'C - 'B
     0: Result := Result + SUB_2;             // great:            C - B
     1: Result := Result + SUB_3;             // small:            c - b
     2: Result := Result + SUB_4;             // one-lined:       c' - b'
     3: Result := Result + SUB_5;             // two-lined:      c'' - b''
     4: Result := Result + SUB_6;             // three-lined:   c''' - b'''
     5: Result := Result + SUB_7;             // four-lined:   c'''' - b''''
     6: Result := Result + SUB_8;             // five-lined:  c''''' - b'''''
     7: Result := Result + SUB_9;             // six-lined:  c'''''' - b''''''
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
    Result := Round(Result * 100) / 100;
  end else
    Result := 0;
end;

end.
