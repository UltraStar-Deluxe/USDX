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
  UMenu,
  UMusic,
  sdl2,
  SysUtils,
  UFiles,
  UGraphicClasses,
  UTime,
  USongs,
  USong,
  UIni,
  ULog,
  UTexture,
  UMenuText,
  URecord,
  UEditorLyrics,
  UFilesystem,
  Math,
  dglOpenGL,
  {$IFDEF UseMIDIPort}
  MidiOut,
  MidiCons,
  {$ENDIF}
  UThemes,
  UPath;

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
        Title: UTF8String;
        Artist: UTF8String;
        CoverId:  Integer;
        BackgroundId: Integer;
        Mp3Id:  Integer;
        GAP:  Real;
        Video:  IPath;
        VideoGAP: real;
        BPM:  array of TBPM;
  end;

  TScreenEditSub = class(TMenu)
    private

      AktBeat:          integer;
      //Variable is True if no Song is loaded
      Error:            boolean;

      TextNote:         integer;
      TextSentence:     integer;
{      TextTitle:        integer;
      TextArtist:       integer;
      TextMp3:          integer;
      TextBPM:          integer;
      TextGAP:          integer;}
      TextDebug:        integer;
{      TextNStart:       integer;
      TextNLength:      integer;
      TextNTon:         integer;
      TextNText:        integer;}
      CurrentNote:      integer;
      PlaySentence:     boolean;
      PlayOne:          boolean;
      PlayOneMidi:      boolean;
      PlaySentenceMidi: boolean;
      midinotefound:    boolean; // if one note was found
      PlayVideo:        boolean;
      PlayStopTime:     real;
      LastClick:        integer;
      Click:            boolean;
      CopySrc:          integer;
      {$IFDEF UseMIDIPort}
      MidiOut:          TMidiOutput;
      MidiStart:        real;
      MidiStop:         real;
      MidiTime:         real;
      MidiPos:          real;
      MidiLastNote:     integer;
      {$ENDIF}

      //for mouse move
      LastPressedMouseButton:  boolean;
      LastPressedMouseType:   integer;
      LastPressedNote:        integer;
      PressedNoteId:          integer;

      TextPosition:     integer;
      TextEditMode:     boolean;
      TitleEditMode:    boolean;
      ArtistEditMode:   boolean;
      // to interactive divide note
      LastClickTime:      integer;

      BackupEditText:   UTF8String; //backup of current text in text-edit-mode
      CurrentEditText:  UTF8String; // current edit text
      editLenghtText:   integer;
      CurrentSlideId:   integer;
      //title header
      TitleSlideId:     integer;
      TitleData:        integer;
      TitleVal:         array of UTF8String;
      SlideTitleIndex:  integer;
      // artist header
      ArtistSlideId:     integer;
      ArtistData:        integer;
      ArtistVal:         array of UTF8String;
      SlideArtistIndex:  integer;
      // mp3 header
      MP3SlideId:     integer;
      MP3Data:        integer;
      MP3Val:         array of UTF8String;
      SlideMP3Index:  integer;
      // Cover header
      CoverSlideId:     integer;
      CoverData:        integer;
      CoverVal:         array of UTF8String;
      SlideCoverIndex:  integer;
      // Background header
      BackgroundSlideId:     integer;
      BackgroundData:        integer;
      BackgroundVal:         array of UTF8String;
      SlideBackgroundIndex:  integer;
      // BPM header
      BPMSlideId:     integer;
      BPMData:        integer;
      BPMVal:         array of UTF8String;
      SlideBPMIndex:  integer;
      // GAP header
      GAPSlideId:     integer;
      GAPData:        integer;
      GAPVal:         array of UTF8String;
      SlideGAPIndex:  integer;
      // Start header
      StartSlideId:     integer;
      StartData:        integer;
      StartVal:         array of UTF8String;
      SlideStartIndex:  integer;
      // Duration header
      DurationSlideId:     integer;
      DurationData:        integer;
      DurationVal:         array of UTF8String;
      SlideDurationIndex:  integer;
      // Tone header
      ToneSlideId:     integer;
      ToneData:        integer;
      ToneVal:         array of UTF8String;
      SlideToneIndex:  integer;
      // Text header
      LyricSlideId:     integer;
      LyricData:        integer;
      LyricVal:         array of UTF8String;
      SlideLyricIndex:  integer;
      // VideoGap header
      VideoGapSlideId:  integer;
      VideoGapData:        integer;
      VideoGapVal:         array of UTF8String;
      SlideVideoGapIndex:  integer;
      // Volume Slide
      VolumeAudioSlideId: integer;
      VolumeMidiSlideId:  integer;
      VolumeClickSlideId: integer;
      VolumeAudioIndex,VolumeMidiIndex,VolumeClickIndex: integer; //for update slide

      VolumeAudio:    array of UTF8String;
      VolumeMidi:    array of UTF8String;
      VolumeClick:    array of UTF8String;
      // control buttons
      UndoButtonId:       integer;
      // background image & video preview
      BackgroundImageId:  integer;
      Empty:         array of UTF8String;   //temporary variable to initialize slide - todo change
      // background for notes to posibility move note
//      NotesBackgroundId:  integer;
      // player static picture
      playerIconId:     array[1..2] of integer;
      //  currentX, CurrentY
      CurrentX:           integer;
      CurrentY:           integer;
      LastX:              integer;
      LastY:              integer;
      Xmouse:             integer;

      resize_note_left:   boolean;
      resize_note_right:  boolean;
      move_note:          boolean;


      Lyric:            TEditorLyrics;

      //undo declaration
      UndoLines:       array of TLines;
      UndoStateNote:      array of integer; //UNDO: note's position
      CurrentUndoLines: integer;
      UndoHeader: array of TVisibleHeaders;

      //video view
      fCurrentVideo: IVideo;

      //singtrack
      CurrentSound:        TCaptureBuffer;
      // Interactive note
      InteractiveNoteId:  array of integer;
      TransparentNoteButtonId: array of integer;
      // Interactive Line bar
      InteractiveLineId:  array of integer;
      TransparentLineButtonId:  array of integer;

      {$IFDEF UseMIDIPort}
      PlayMidi: boolean;
	  {$ENDIF}// Midi

      // medley
      MedleyNotes:  TMedleyNotes;

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
      procedure TransposeNote(Transpose: integer);
      procedure ChangeWholeTone(Tone: integer);
      procedure MoveAllToEnd(Move: integer);
      procedure MoveTextToRight;
      procedure MarkSrc;
      procedure PasteText;
      procedure CopySentence(Src, Dst: integer);
      procedure CopySentences(Src, Dst, Num: integer);
      procedure CopyToUndo; //copy current Lines,mouse position and headers
      procedure CopyFromUndo; //undo last Lines,mouse position and headers
      procedure DrawPlayerTrack(X, Y, W: real; Space: integer; CurrentTone: integer; Count: integer; CurrentNote: integer);
      procedure DrawStatics_UP(X, Y, W, H: integer);
      procedure DrawStatics_Sentences(X, Y, W, H: integer);
      procedure DrawStatics_Notes(X, Y, W, H: integer);
      procedure DrawInfoBar(x, y, w, h: integer; currentLines: integer);
      procedure DrawText(Left, Top, Right: real; NrLines: integer; Space: integer);
      //video view
      procedure StartVideoPreview();
      procedure StopVideoPreview();
      //Note Name Mod
      function GetNoteName(Note: integer): string;
      // show transparent background note for intaractions
      procedure ShowInteractiveBackground;

      procedure UpdateMedleyInfo;
      function GetMedleyLength: real; //if available returns the length of the medley in seconds, otherwise 0

    public
      Tex_PrevBackground:     TTexture;
      FadeOut:            boolean;
      constructor Create; override;
      procedure OnShow; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      function ParseInputEditText(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
      function ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean; override;
      function Draw: boolean; override;
      procedure OnHide; override;
  end;

implementation

uses
  UGraphic,
  UDraw,
  UDisplay,
  UMenuInteract,
  UNote,
  USkins,
  ULanguage,
  TextGL,
  UTextEncoding,
  UUnicodeUtils;

const
  DEFAULT_FADE_IN_TIME = 8;    //TODO in INI
  DEFAULT_FADE_OUT_TIME = 2;

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
    SResult := SaveSong(CurrentSong, Lines[0], FilePath,
             boolean(Data));
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
begin
  Result := true;

  if TextEditMode or TitleEditMode or ArtistEditMode then
  begin
    Result := ParseInputEditText(PressedKey, CharCode, PressedDown);
  end
  else
  begin

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT {+ KMOD_CAPS});

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
            SResult := SaveSong(CurrentSong, Lines[0], CurrentSong.Path.Append(CurrentSong.FileName),
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
          //Medley MOD:
          {if CurrentSong.isDuet then
          begin
            CurrentSong.Medley.Source := msNone;
          end
          else }
          if (MedleyNotes.isStart and MedleyNotes.isEnd) and
             MedleyNotes.isCustom and
            (MedleyNotes.start.line < MedleyNotes.end_.line) and
            (Length(Lines[0].Line)> MedleyNotes.end_.line) and
            (Length(Lines[0].Line[MedleyNotes.end_.line].Note)>MedleyNotes.end_.note) and
            (Length(Lines[0].Line[MedleyNotes.start.line].Note)>MedleyNotes.start.note) then
          begin
            CurrentSong.Medley.Source := msTag;
            CurrentSong.Medley.StartBeat:=Lines[0].Line[MedleyNotes.start.line].Note[MedleyNotes.start.note].Start;
            CurrentSong.Medley.EndBeat:=Lines[0].Line[MedleyNotes.end_.line].Note[MedleyNotes.end_.note].Start +
              Lines[0].Line[MedleyNotes.end_.line].Note[MedleyNotes.end_.note].Length;
            CurrentSong.Medley.FadeIn_time := DEFAULT_FADE_IN_TIME;
            CurrentSong.Medley.FadeOut_time := DEFAULT_FADE_OUT_TIME;
          end else if MedleyNotes.isCustom then
          begin
            CurrentSong.Medley.Source := msNone;
            CurrentSong.Medley.StartBeat:=0;
            CurrentSong.Medley.EndBeat:=0;
          end;

          // Save Song
          if SDL_ModState = KMOD_LSHIFT then
          begin
            {if (CurrentSong.isDuet) then
            begin
              ScreenPopupError.ShowPopup('Duet with Relative is not supported!');
              Exit;
            end;
             }
            if (CurrentSong.Medley.Source = msTag) then
            begin
              ScreenPopupError.ShowPopup('Medley with Relative is not supported! Medley-Tags deleted!');
            end;

            CurrentSong.Medley.Source := msNone;
            SResult := SaveSong(CurrentSong, Lines[0], CurrentSong.Path.Append(CurrentSong.FileName), true); //save with relative
          end else
            SResult := SaveSong(CurrentSong, Lines[0], CurrentSong.Path.Append(CurrentSong.FileName), false);

          if (SResult = ssrOK) then
          begin
            Text[TextDebug].Text := Language.Translate('INFO_FILE_SAVED');
            SetLength(UndoLines, 0); //clear undo lines
            SetLength(UndoStateNote, 0); //clear undo currentnote state
            SetLength(Undoheader, 0); //clear undo headrers
            CurrentUndoLines := 0;
            //if not CheckSong then
            //  ScreenPopupError.ShowPopup('This song contains some syntax errors!');

            //CatSongs.Song[SongIndex] := AktSong;
          end else
          begin
            ScreenPopupError.ShowPopup(Language.Translate('ERROR_SAVE_FILE_FAILED'));
          end;

          Exit;
        end;

      // set Medley tags
      SDLK_A:
        begin
          if CurrentSong.Relative then
          begin
            ScreenPopupError.ShowPopup('Medley with Relative is not supported!');
            Exit;
          end;

          {
          if CurrentSong.isDuet then
          begin
            ScreenPopupError.ShowPopup('Medley with Duet is not supported!');
            Exit;
          end;
          }

          MedleyNotes.isCustom := true;
          if SDL_ModState = KMOD_LSHIFT then //Medley End Note
          begin
            if MedleyNotes.isEnd then
            begin
              if (Lines[0].Current = MedleyNotes.end_.line) and (CurrentNote = MedleyNotes.end_.note) then
              begin
                MedleyNotes.isEnd := false;
                Lines[0].Line[Lines[0].Current].Note[CurrentNote].IsMedley := false;
              end else
              begin
                Lines[0].Line[Lines[0].Current].Note[CurrentNote].IsMedley := true;
                if (Length(Lines[0].Line)> MedleyNotes.end_.line) and
                  (Length(Lines[0].Line[MedleyNotes.end_.line].Note)>MedleyNotes.end_.note) then
                  Lines[0].Line[MedleyNotes.end_.line].Note[MedleyNotes.end_.note].IsMedley := false;
                MedleyNotes.end_.line := Lines[0].Current;
                MedleyNotes.end_.note := CurrentNote;
              end;
            end else
            begin
              MedleyNotes.isEnd := true;
              Lines[0].Line[Lines[0].Current].Note[CurrentNote].IsMedley := true;
              MedleyNotes.end_.line := Lines[0].Current;
              MedleyNotes.end_.note := CurrentNote;
            end;
          end else
          begin        //Medley Start Note
            if MedleyNotes.isStart then
            begin
              if (Lines[0].Current = MedleyNotes.start.line) and (CurrentNote = MedleyNotes.start.note) then
              begin
                MedleyNotes.isStart := false;
                Lines[0].Line[Lines[0].Current].Note[CurrentNote].IsMedley := false;
              end else
              begin
                Lines[0].Line[Lines[0].Current].Note[CurrentNote].IsMedley := true;
                if (Length(Lines[0].Line)> MedleyNotes.start.line) and
                  (Length(Lines[0].Line[MedleyNotes.start.line].Note)>MedleyNotes.start.note) then
                  Lines[0].Line[MedleyNotes.start.line].Note[MedleyNotes.start.note].IsMedley := false;
                MedleyNotes.start.line := Lines[0].Current;
                MedleyNotes.start.note := CurrentNote;
              end;
            end else
            begin
              Lines[0].Line[Lines[0].Current].Note[CurrentNote].IsMedley := true;
              MedleyNotes.isStart := true;
              MedleyNotes.start.line := Lines[0].Current;
              MedleyNotes.start.note := CurrentNote;
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
            ScreenPopupError.ShowPopup('Medley with Relative is not supported!');
            Exit;
          end;

          {
          if CurrentSong.isDuet then
          begin
            ScreenPopupError.ShowPopup('Medley with Duet is not supported!');
            Exit;
          end;
          }

          if not MedleyNotes.IsEnd and not MedleyNotes.IsStart then
          begin
            // TODO: localize popup message for medley without start/end beat
            ScreenPopupError.ShowPopup('No Medley section set. Check your txt file or set it with A and Shift+A.');
            Exit;
          end;

          if (SDL_ModState = KMOD_LSHIFT) and MedleyNotes.IsEnd then //Medley End Note
          begin
            {$IFDEF UseMIDIPort} MidiOut.PutShort($81, Lines[0].Line[Lines[0].Current].Note[MidiLastNote].Tone + 60, 127);
            PlaySentenceMidi := false;
            PlayOneMidi := false; {$ENDIF}
            AudioPlayback.Stop;
            PlaySentence := false;
            PlayOne := false;

            if (Length(Lines[0].Line)> MedleyNotes.end_.line) and
              (Length(Lines[0].Line[MedleyNotes.end_.line].Note)>MedleyNotes.end_.note) then
            begin
              Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 0;
              Lines[0].Current := MedleyNotes.end_.line;
              CurrentNote := MedleyNotes.end_.note;
              Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 2;

              //EditorLyric[0].AddCzesc(0, Lines[0].Current);
              //EditorLyric[0].Selected := AktNuta[0];
            end;
          end else if MedleyNotes.IsStart then
          begin
            {$IFDEF UseMIDIPort} MidiOut.PutShort($81, Lines[0].Line[Lines[0].Current].Note[MidiLastNote].Tone + 60, 127);
            PlaySentenceMidi := false;
            PlayOneMidi := false; {$ENDIF}
            AudioPlayback.Stop;
            PlaySentence := false;
            PlayOne := false;

            if (Length(Lines[0].Line)> MedleyNotes.start.line) and
              (Length(Lines[0].Line[MedleyNotes.start.line].Note)>MedleyNotes.start.note) then
            begin
              Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 0;
              Lines[0].Current := MedleyNotes.start.line;
              CurrentNote := MedleyNotes.start.note;
              Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 2;

              //EditorLyric[0].AddCzesc(0, Lines[0].Akt);
              //EditorLyric[0].Selected := AktNuta[0];
            end;
          end;

          if (SDL_ModState = KMOD_LALT) then
          begin
            PlaySentenceMidi := false;
            PlayOneMidi := false;
            PlayOne := false;
            AudioPlayback.Stop;
            //LineChanged[0]:=false;
            //LineChanged[1]:=false;

            if (MedleyNotes.isStart and MedleyNotes.isEnd) and
              (MedleyNotes.start.line < MedleyNotes.end_.line) and
              (Length(Lines[0].Line)> MedleyNotes.end_.line) and
              (Length(Lines[0].Line[MedleyNotes.end_.line].Note)>MedleyNotes.end_.note) and
              (Length(Lines[0].Line[MedleyNotes.start.line].Note)>MedleyNotes.start.note) then
            begin
              R := GetTimeFromBeat(Lines[0].Line[MedleyNotes.start.line].Note[MedleyNotes.start.note].Start);
              if R <= AudioPlayback.Length then
              begin
                AudioPlayback.Position:= R;

                //noteStart := AktNuta[0];
                //lineStart := Lines[0].Akt;
                //cpStart := 0;

                PlayStopTime := GetTimeFromBeat(
                  Lines[0].Line[MedleyNotes.end_.line].Note[MedleyNotes.end_.note].Start +
                  Lines[0].Line[MedleyNotes.end_.line].Note[MedleyNotes.end_.note].Length);
                PlaySentence := true;
                AudioPlayback.Play;
                LastClick := Lines[0].Line[MedleyNotes.start.line].Note[MedleyNotes.start.note].Start-1;
              end;
            end;
          end;

          GoldenRec.KillAll;
          ShowInteractiveBackground;
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
          Lyric.Free;

          onShow;
//          Text[TextDebug].Text := 'song reloaded'; //TODO: Language.Translate('SONG_RELOADED');
          Text[TextDebug].Text := Language.Translate('INFO_SONG_RELOADED');
        end;

      SDLK_D:
        begin
          // Divide lengths by 2
          if (SDL_ModState = KMOD_LSHIFT) then
          begin
            CopyToUndo;
            DivideBPM;
            ShowInteractiveBackground;
            Text[TextDebug].Text := Language.Translate('INFO_DIVIDED_BPM');
            Exit;
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
            Text[TextDebug].Text := Language.Translate('INFO_MULTIPLIED');
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
            Lyric.AddLine(Lines[0].Current);
            Lyric.Selected := CurrentNote;
            Text[TextDebug].Text := Language.Translate('INFO_CAPITALIZE');
            end;

          // Correct spaces
          if SDL_ModState = KMOD_LSHIFT then
            begin
              CopyToUndo;
              LyricsCorrectSpaces;
            end;

          // Copy sentence
          if SDL_ModState = KMOD_LCTRL then
            MarkSrc;

          Exit;
        end;
      SDLK_V:
        begin
          if SDL_ModState = 0 then
          begin
              AudioPlayback.Stop;
              Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
              CurrentNote := 0;
              AudioPlayback.Position := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].Note[0].Start);
              PlayStopTime := GetTimeFromBeat(Lines[0].Line[Lines[0].High].End_);
              PlaySentence := true;
              AudioPlayback.Play;
              LastClick := -100;
              PlayVideo := true;
              StartVideoPreview();
              Text[TextDebug].Text := Language.Translate('INFO_PLAY_SONG');
          end;
          // Paste text
          if SDL_ModState = KMOD_LCTRL then
          begin
            if Lines[0].Line[Lines[0].Current].HighNote >= Lines[0].Line[CopySrc].HighNote then
              PasteText
            else
              Log.LogStatus('PasteText: invalid range', 'TScreenEditSub.ParseInput');
            Lyric.AddLine(Lines[0].Current);
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then
          begin
            CopyToUndo;
            CopySentence(CopySrc, Lines[0].Current);
          end;
          GoldenRec.KillAll;
          ShowInteractiveBackground;
        end;
      SDLK_T:
        begin
          // Fixes timings between sentences
          CopyToUndo;
          FixTimings;
          Text[TextDebug].Text := Language.Translate('INFO_TIME_FIXED');
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
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
            CurrentNote := 0;
            R := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].Note[0].Start);
            if R <= AudioPlayback.Length then
            begin
              AudioPlayback.Position := R;
              PlayStopTime := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].End_);
              PlaySentence := true;
              AudioPlayback.Play;
              LastClick := -100;
            end;
            Text[TextDebug].Text := Language.Translate('INFO_PLAY_SENTENCE');
          end
          else if SDL_ModState = KMOD_LSHIFT then
          begin
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
            CurrentNote := 0;
            PlaySentenceMidi := true;
            PlayVideo := false;
            StopVideoPreview;
            {$IFDEF UseMIDIPort} MidiTime := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].Note[0].Start);
            MidiStop := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].End_); {$ENDIF}

            LastClick := -100;
            Text[TextDebug].Text := Language.Translate('INFO_PLAY_SENTENCE');
          end
          else if SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL then
          begin
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
            CurrentNote := 0;
            PlaySentenceMidi := true;
            PlayVideo := false;
            StopVideoPreview;
            {$IFDEF UseMIDIPort} MidiTime  := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].Note[0].Start);
            MidiStop  := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].End_); {$ENDIF}
            
            LastClick := -100;

            PlaySentence := true;
            Click := true;
            AudioPlayback.Stop;
            AudioPlayback.Position := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].Note[0].Start)+0{-0.10};
            PlayStopTime := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].End_)+0;
            AudioPlayback.Play;
            LastClick := -100;
            Text[TextDebug].Text := Language.Translate('INFO_PLAY_SENTENCE');
          end;
          Exit;
        end;

      // Golden Note
      SDLK_G:
        begin
          CopyToUndo;
          if (Lines[0].Line[Lines[0].Current].Note[CurrentNote].NoteType = ntGolden) then
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].NoteType := ntNormal
          else
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].NoteType := ntGolden;
          GoldenRec.KillAll;
          Exit;
        end;

      // Freestyle Note
      SDLK_F:
        begin
          CopyToUndo;
          if (Lines[0].Line[Lines[0].Current].Note[CurrentNote].NoteType = ntFreestyle) then
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].NoteType := ntNormal
          else
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].NoteType := ntFreestyle;
          	GoldenRec.KillAll;

            // update lyrics
            Lyric.AddLine(Lines[0].Current);
            Lyric.Selected := CurrentNote;
          Exit;
        end;
      SDLK_Z:
        begin
          if SDL_ModState = KMOD_LCTRL then
          begin
              CopyFromUndo;
              GoldenRec.KillAll;
              Text[TextDebug].Text := Language.Translate('INFO_UNDO');
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
            ScreenPopupcheck.ShowPopup(Language.Translate('INFO_EXIT'), OnExit, 0, false)
          else
          begin
            FadeTo(@ScreenSong);
          end;
        end;

      SDLK_BACKQUOTE:
        begin
          // Increase Note Length (same as Alt + Right)
          CopyToUndo;
          Inc(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length);
          if CurrentNote = Lines[0].Line[Lines[0].Current].HighNote then
            Inc(Lines[0].Line[Lines[0].Current].End_);
          GoldenRec.KillAll;
          ShowInteractiveBackground;
        end;

      SDLK_EQUALS:
        begin
          // Increase BPM
          CopyToUndo;
          if SDL_ModState = 0 then
            CurrentSong.BPM[0].BPM := Round((CurrentSong.BPM[0].BPM * 5) + 1) / 5; // (1/20)
          if SDL_ModState = KMOD_LSHIFT then
            CurrentSong.BPM[0].BPM := CurrentSong.BPM[0].BPM + 4; // (1/1)
          if SDL_ModState = KMOD_LCTRL then
            CurrentSong.BPM[0].BPM := Round((CurrentSong.BPM[0].BPM * 25) + 1) / 25; // (1/100)
        end;

      SDLK_MINUS:
        begin
          // Decrease BPM
          CopyToUndo;
          if SDL_ModState = 0 then
            CurrentSong.BPM[0].BPM := Round((CurrentSong.BPM[0].BPM * 5) - 1) / 5;
          if SDL_ModState = KMOD_LSHIFT then
            CurrentSong.BPM[0].BPM := CurrentSong.BPM[0].BPM - 4;
          if SDL_ModState = KMOD_LCTRL then
            CurrentSong.BPM[0].BPM := Round((CurrentSong.BPM[0].BPM * 25) - 1) / 25;
        end;

      SDLK_4:
        begin
          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then
          begin
            CopyToUndo;
            CopySentence(CopySrc, Lines[0].Current);
            CopySentence(CopySrc+1, Lines[0].Current+1);
            CopySentence(CopySrc+2, Lines[0].Current+2);
            CopySentence(CopySrc+3, Lines[0].Current+3);
            GoldenRec.KillAll;
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then
          begin
            CopyToUndo;
            CopySentences(CopySrc, Lines[0].Current, 4);
            GoldenRec.KillAll;
          end;
        ShowInteractiveBackground;
        end;
      SDLK_5:
        begin
          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then
          begin
            CopyToUndo;
            CopySentence(CopySrc, Lines[0].Current);
            CopySentence(CopySrc+1, Lines[0].Current+1);
            CopySentence(CopySrc+2, Lines[0].Current+2);
            CopySentence(CopySrc+3, Lines[0].Current+3);
            CopySentence(CopySrc+4, Lines[0].Current+4);
            GoldenRec.KillAll;
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then
          begin
            CopyToUndo;
            CopySentences(CopySrc, Lines[0].Current, 5);
            GoldenRec.KillAll;
          end;
        ShowInteractiveBackground;
        end;

      SDLK_7:
        begin
          if SDL_ModState = 0 then
            CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) - 1 )/100;
          if SDL_ModState = KMOD_LSHIFT then
            CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) - 10 )/100;
          if SDL_ModState = KMOD_LCTRL then
            CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) - 100 )/100;
        end;

      SDLK_8:
        begin
          if SDL_ModState = 0 then
            CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) + 1 )/100;
          if SDL_ModState = KMOD_LSHIFT then
            CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) + 10 )/100;
          if SDL_ModState = KMOD_LCTRL then
            CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) + 100 )/100;
        end;

      SDLK_9:
        begin
          // Decrease GAP
          CopyToUndo;
          if SDL_ModState = 0 then
            CurrentSong.GAP := CurrentSong.GAP - 10;
          if SDL_ModState = KMOD_LSHIFT then
            CurrentSong.GAP := CurrentSong.GAP - 1000;
        end;
      SDLK_0:
        begin
          // Increase GAP
          CopyToUndo;
          if SDL_ModState = 0 then
            CurrentSong.GAP := CurrentSong.GAP + 10;
          if SDL_ModState = KMOD_LSHIFT then
            CurrentSong.GAP := CurrentSong.GAP + 1000;
        end;

      SDLK_KP_PLUS:
        begin
          // Increase tone of all notes
          CopyToUndo;
          if SDL_ModState = 0 then
            ChangeWholeTone(1);
          if SDL_ModState = KMOD_LSHIFT then
            ChangeWholeTone(12);
          GoldenRec.KillAll;
          ShowInteractiveBackground;
        end;

      SDLK_KP_MINUS:
        begin
          // Decrease tone of all notes
          CopyToUndo;
          if SDL_ModState = 0 then
            ChangeWholeTone(-1);
          if SDL_ModState = KMOD_LSHIFT then
            ChangeWholeTone(-12);
            GoldenRec.KillAll;
          ShowInteractiveBackground;
        end;

      SDLK_SLASH:
        begin
          CopyToUndo;
          if SDL_ModState = 0 then
          begin
            // Insert start of sentece
            if CurrentNote > 0 then
              DivideSentence;
            GoldenRec.KillAll;
          end;

          if SDL_ModState = KMOD_LSHIFT then
          begin
            // Join next sentence with current
            if Lines[0].Current < Lines[0].High then
              JoinSentence;
            GoldenRec.KillAll;
          end;

          if SDL_ModState = KMOD_LCTRL then
          begin
            // divide note
            DivideNote(false);
            Lyric.AddLine(Lines[0].Current);
            Lyric.Selected := CurrentNote;
            GoldenRec.KillAll;
          end;
        ShowInteractiveBackground;
        end;

      SDLK_F4:
        begin
          // Enter Text Edit Mode
          BackupEditText := Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text;
          CurrentEditText := BackupEditText;
          CurrentSlideId := LyricSlideId;
          TextPosition := LengthUTF8(BackupEditText);
          editLenghtText := LengthUTF8(BackupEditText);
          TextEditMode := true;
        end;

      SDLK_SPACE:
        begin
          if (SDL_ModState = 0) or (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) then
          begin
            // Play Sentence
            PlaySentenceMidi := false; // stop midi
            PlaySentence := false;
            midinotefound := false;
            PlayOne := true;
            PlayOneMidi := false;
            PlayVideo := false;
            StopVideoPreview;
            Click := false;
            AudioPlayback.Stop;
            AudioPlayback.Position := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start);
            PlayStopTime := (GetTimeFromBeat(
              Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start +
              Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length));
            AudioPlayback.Play;
            LastClick := -100;
          end;

          if (SDL_ModState = KMOD_LSHIFT) or (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) then
          begin
            // Play Midi
            PlaySentenceMidi := false;
            midinotefound := false;
            PlayOne := true;
            PlayOneMidi := true;
            //basisbit ToDo add midi tone playback support here
            LastClick := -100;
          end;
        end;

      SDLK_RETURN:
        begin
           if Interaction =  2 then //TitleSlideId
           begin
             BackupEditText := CurrentSong.Title;
             CurrentEditText := BackupEditText;
             editLenghtText := LengthUTF8(BackupEditText);
             CurrentSlideId := TitleSlideId;
             TextPosition := LengthUTF8(BackupEditText);
             TitleEditMode := true;
           end;
           if Interaction = 3 then //ArtistSlideId
           begin
             BackupEditText := CurrentSong.Artist;
             CurrentEditText := BackupEditText;
             editLenghtText := LengthUTF8(BackupEditText);
             CurrentSlideId := ArtistSlideId;
             TextPosition := LengthUTF8(BackupEditText);
             ArtistEditMode := true;
           end;

           if Interaction = 12 then //LyricSlideId
           begin
             BackupEditText := Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text;
             CurrentEditText := BackupEditText;
             editLenghtText := LengthUTF8(BackupEditText);
             CurrentSlideId := LyricSlideId;
             TextPosition := LengthUTF8(BackupEditText);
             TextEditMode := true;
           end;

           if Interaction = 17 then //Only Play
           begin
              // Play Sentence
              Click := true;
              AudioPlayback.Stop;
              PlayVideo := false;
              StopVideoPreview;
              Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
              CurrentNote := 0;
              R := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].Note[0].Start);
              if R <= AudioPlayback.Length then
              begin
                AudioPlayback.Position := R;
                PlayStopTime := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].End_);
                PlaySentence := true;
                AudioPlayback.Play;
                LastClick := -100;
              end;
              Text[TextDebug].Text := Language.Translate('INFO_PLAY_SENTENCE');
           end;

           if Interaction = 18 then //Play with midi
           begin
              Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
              CurrentNote := 0;
              PlaySentenceMidi := true;
              PlayVideo := false;
              StopVideoPreview;
              {$IFDEF UseMIDIPort} MidiTime  := USTime.GetTime;
              MidiStart := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].Note[0].Start);
              MidiStop  := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].End_); {$ENDIF}
              LastClick := -100;

              PlaySentence := true;
              Click := true;
              AudioPlayback.Stop;
              AudioPlayback.Position := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].Note[0].Start)+0{-0.10};
              PlayStopTime := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].End_)+0;
              AudioPlayback.Play;
              LastClick := -100;
              Text[TextDebug].Text := Language.Translate('INFO_PLAY_SENTENCE');
           end;

           if Interaction = 19 then //Play midi
           begin
              Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
              CurrentNote := 0;
              PlaySentenceMidi := true;
              PlayVideo := false;
              StopVideoPreview;
              {$IFDEF UseMIDIPort} MidiTime := USTime.GetTime;
              MidiStart := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].Note[0].Start);
              MidiStop := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].End_); {$ENDIF}

              LastClick := -100;
              Text[TextDebug].Text := Language.Translate('INFO_PLAY_SENTENCE');
           end;

           if Interaction = 20 then //previous sequence
           begin
              PreviousSentence;
           end;

           if Interaction = 21 then //next sequence
           begin
              NextSentence;
           end;

           if Interaction = 22 then //undo
           begin
              CopyFromUndo;
              GoldenRec.KillAll;
              Text[TextDebug].Text := Language.Translate('INFO_UNDO');
              ShowInteractiveBackground;
           end;

           if Interaction = 23 then //golden note
           begin
              CopyToUndo;
              if (Lines[0].Line[Lines[0].Current].Note[CurrentNote].NoteType = ntGolden) then
                Lines[0].Line[Lines[0].Current].Note[CurrentNote].NoteType := ntNormal
              else
                Lines[0].Line[Lines[0].Current].Note[CurrentNote].NoteType := ntGolden;
              GoldenRec.KillAll;
              Exit;
           end;

           if Interaction = 24 then //freestyle note
           begin
              CopyToUndo;
              if (Lines[0].Line[Lines[0].Current].Note[CurrentNote].NoteType = ntFreestyle) then
                Lines[0].Line[Lines[0].Current].Note[CurrentNote].NoteType := ntNormal
              else
                Lines[0].Line[Lines[0].Current].Note[CurrentNote].NoteType := ntFreestyle;
          	  GoldenRec.KillAll;

              // update lyrics
              Lyric.AddLine(Lines[0].Current);
              Lyric.Selected := CurrentNote;
              Exit;
           end;

           for i := 0 to Lines[0].High do
           begin
              if Interaction = InteractiveLineId[i] then
              begin
                CopyToUndo;
                GoldenRec.KillAll;
                Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
                Lines[0].Current := i;
                ShowInteractiveBackground;
                currentnote := 0;
                Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 2;
                Lyric.AddLine(Lines[0].Current);
                Lyric.Selected := CurrentNote;
              end;

           end;

           if high(InteractiveNoteId) >= Lines[0].Line[Lines[0].Current].HighNote then
           for i := 0 to Lines[0].Line[Lines[0].Current].HighNote do
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

                Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
                currentnote := i;
                Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 2;
                Lyric.Selected := CurrentNote;
                //play current note playonewithmidi
                PlaySentenceMidi := false;
                midinotefound := false;
                PlayOne := true;
                PlayOneMidi := true;
                {$IFDEF UseMIDIPort} MidiTime := USTime.GetTime;
                MidiStart := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start);
                MidiStop := GetTimeFromBeat(
                  Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start +
                  Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length); {$ENDIF}

                // playone
                PlayVideo := false;
                StopVideoPreview;
                Click := false;
                AudioPlayback.Stop;
                AudioPlayback.Position := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start);
                  PlayStopTime := (GetTimeFromBeat(
                Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start +
                Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length));
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
            GoldenRec.KillAll;
            ShowInteractiveBackground;
          end;
        end;

      SDLK_PERIOD:
        begin
          // moves text to right in current sentence
          CopyToUndo;
          MoveTextToRight;
        end;

      SDLK_RIGHT:
        begin
          // right
          CopyToUndo;
          if SDL_ModState = 0 then
          begin
            AudioPlayback.Stop;
            PlaySentence := false;
            PlayOne := false;
            PlayVideo := false;
            {$IFDEF UseMIDIPort}
            MidiOut.PutShort($B1, $7, floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
            MidiOut.PutShort($81, Lines[0].Line[Lines[0].Current].Note[MidiLastNote].Tone + 60, 127);
            PlaySentenceMidi := false;
            {$endif}
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
            Inc(CurrentNote);
            if CurrentNote > Lines[0].Line[Lines[0].Current].HighNote then
              CurrentNote := 0;
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 2;
            Lyric.Selected := CurrentNote;
          end;

          // ctrl + right
          if SDL_ModState = KMOD_LCTRL then
          begin
            if Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length > 1 then
            begin
              Dec(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length);
              Inc(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start);
              if CurrentNote = 0 then
              begin
                Inc(Lines[0].Line[Lines[0].Current].Start);
              end;
            end;
            GoldenRec.KillAll;
          end;

          // shift + right
          if SDL_ModState = KMOD_LSHIFT then
          begin
            Inc(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start);
            if CurrentNote = 0 then
            begin
              Inc(Lines[0].Line[Lines[0].Current].Start);
            end;
            if CurrentNote = Lines[0].Line[Lines[0].Current].HighNote then
              Inc(Lines[0].Line[Lines[0].Current].End_);
            GoldenRec.KillAll;
          end;

          // alt + right
          if SDL_ModState = KMOD_LALT then
          begin
            Inc(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length);
            if CurrentNote = Lines[0].Line[Lines[0].Current].HighNote then
              Inc(Lines[0].Line[Lines[0].Current].End_);
            GoldenRec.KillAll;
          end;

          // alt + ctrl + shift + right = move all from cursor to right
          if SDL_ModState = KMOD_LALT + KMOD_LCTRL + KMOD_LSHIFT then
          begin
            MoveAllToEnd(1);
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
            AudioPlayback.Stop();
            PlaySentence := false;
            PlayOne := false;
            PlayVideo := false;
            {$IFDEF UseMIDIPort}
            MidiOut.PutShort($B1, $7, floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
            MidiOut.PutShort($81, Lines[0].Line[Lines[0].Current].Note[MidiLastNote].Tone + 60, 127);
            PlaySentenceMidi := false;
            {$endif}

            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
            Dec(CurrentNote);
            if CurrentNote = -1 then
              CurrentNote := Lines[0].Line[Lines[0].Current].HighNote;
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 2;
            Lyric.Selected := CurrentNote;
          end;

          // ctrl + left
          if SDL_ModState = KMOD_LCTRL then
          begin
            Dec(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start);
            Inc(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length);
            if CurrentNote = 0 then
            begin
              Dec(Lines[0].Line[Lines[0].Current].Start);
            end;
            GoldenRec.KillAll;
          end;

          // shift + left
          if SDL_ModState = KMOD_LSHIFT then
          begin
            Dec(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start);

            // resizing sentences
            if CurrentNote = 0 then
            begin
              Dec(Lines[0].Line[Lines[0].Current].Start);
            end;

            if CurrentNote = Lines[0].Line[Lines[0].Current].HighNote then
              Dec(Lines[0].Line[Lines[0].Current].End_);
            GoldenRec.KillAll;
          end;

          // alt + left
          if SDL_ModState = KMOD_LALT then
          begin
            if Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length > 1 then
            begin
              Dec(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length);
              if CurrentNote = Lines[0].Line[Lines[0].Current].HighNote then
                Dec(Lines[0].Line[Lines[0].Current].End_);
            end;
            GoldenRec.KillAll;
          end;

          // alt + ctrl + shift + right = move all from cursor to left
          if SDL_ModState = KMOD_LALT + KMOD_LCTRL + KMOD_LSHIFT then
          begin
            MoveAllToEnd(-1);
            GoldenRec.KillAll;
          end;
        ShowInteractiveBackground;
        end;

      SDLK_DOWN:
        begin
          // skip to next sentence
          if SDL_ModState = 0 then
          begin
            NextSentence;
          end;

          // decrease tone
          if SDL_ModState = KMOD_LSHIFT then
          begin
            CopyToUndo;
            TransposeNote(-1);
            GoldenRec.KillAll;
          end;
        ShowInteractiveBackground;
        end;

      SDLK_UP:
        begin
          // skip to previous sentence
          if SDL_ModState = 0 then
          begin
            PreviousSentence;
          end;

          // increase tone
          if SDL_ModState = KMOD_LSHIFT then
          begin
            CopyToUndo;
            TransposeNote(1);
            GoldenRec.KillAll;
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
      inc(editLenghtText);
      inc(TextPosition);

      if TextEditMode then
        begin
        Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text := CurrentEditText;
        Lyric.AddLine(Lines[0].Current);
        Lyric.Selected := CurrentNote;
        end;
      Exit;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE:
        begin
          if TextEditMode then Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text := BackupEditText;
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
          Lyric.AddLine(Lines[0].Current);
          Lyric.Selected := CurrentNote;
          TextEditMode := false;
          TitleEditMode := false;
          ArtistEditMode := false;
          editLenghtText := 0;
          TextPosition := -1;
        end;
      SDLK_F4, SDLK_RETURN:
        begin
          // Exit Text Edit Mode
          CopyToUndo;
          if TitleEditMode then
          begin
            CurrentSong.Title := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
          end;
          if ArtistEditMode then
          begin
            CurrentSong.Artist := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
          end;
          if TextEditMode then
          begin
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
            Lyric.AddLine(Lines[0].Current);
          end;
          Lyric.Selected := CurrentNote;
          TitleEditMode := false;
          TextEditMode := false;
          ArtistEditMode := false;
          editLenghtText := 0;
          TextPosition := -1;
          CurrentSlideId := -1;
        end;
      SDLK_BACKSPACE:
        begin
          UTF8Delete(CurrentEditText, TextPosition, 1);
          dec(TextPosition);
          if TextEditMode then
          begin
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            Lyric.AddLine(Lines[0].Current);
            Lyric.Selected := CurrentNote;
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
            if (TextPosition >= 0) and (TextPosition < editLenghtText-1) then
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
                TextPosition := editLenghtText-1;
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
            Lyric.AddLine(Lines[0].Current);
            Lyric.Selected := CurrentNote;
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
     tempR := 720 / (Lines[0].Line[Lines[0].Current].End_ - Lines[0].Line[Lines[0].Current].Note[0].Start);
     if (MouseButton = 0) and (LastPressedMouseType = SDL_BUTTON_RIGHT) and (PressedNoteId >=0) then
     begin
        // left & right
        if (Floor((CurrentX-40)/tempr) > Floor((LastX-40)/tempr)) or  (Floor((CurrentX-40)/tempr) < Floor((LastX-40)/tempr)) then
        begin
          CopyToUndo;
          i := floor((currentx-40) / floor(tempr)) - floor((lastx-40) / floor(tempr));
          if move_note then
            MoveAllToEnd(i);
          if (resize_note_right) and (Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length + i > 0) then
          begin
            MoveAllToEnd(i);
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start := Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start - i;
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length := Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length + i;
          end;
          if (resize_note_left) and (Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length - i > 0) then
          begin
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start := Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start + i;
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length := Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length - i;
            if CurrentNote = 0 then
              Lines[0].Line[Lines[0].Current].Start := Lines[0].Line[Lines[0].Current].Start - i;
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
              Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start := Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start +i;
              if CurrentNote = 0 then
                Lines[0].Line[Lines[0].Current].Start := Lines[0].Line[Lines[0].Current].Start - i;
              if CurrentNote = Lines[0].Line[Lines[0].Current].HighNote then
                Lines[0].Line[Lines[0].Current].End_ := Lines[0].Line[Lines[0].Current].End_ + i;
          end;
          // resize note
          if (resize_note_right) and (Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length + i > 0) then
          begin
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length := Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length + i;
            if CurrentNote = Lines[0].Line[Lines[0].Current].HighNote then
                Lines[0].Line[Lines[0].Current].End_ := Lines[0].Line[Lines[0].Current].End_ + i;
          end;
          if (resize_note_left) and (Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length - i > 0) then
          begin
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start := Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start + i;
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length := Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length - i;
            if CurrentNote = 0 then
                Lines[0].Line[Lines[0].Current].Start := Lines[0].Line[Lines[0].Current].Start + i;
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
     // change to next sequense
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

  if ((BackgroundSlideId = Interactions[nBut].Num) and (Action = maLeft) and (SelectsS[Interactions[nBut].Num].SelectedOption > 0)) then
  begin
    CopyToUndo;
    SelectsS[Interactions[nBut].Num].SelectedOption := SelectsS[Interactions[nBut].Num].SelectedOption -1;
    CurrentSong.Background := Path(SelectsS[Interactions[nBut].Num].TextOptT[SelectsS[Interactions[nBut].Num].SelectedOption]);
    // change background picture
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
    // change background picture
    Tex_PrevBackground := Texture.LoadTexture(CurrentSong.Path.Append(CurrentSong.Background));
    Texture.AddTexture(Tex_PrevBackground, TEXTURE_TYPE_PLAIN, false);
    Statics[BackgroundImageId].Texture := Tex_PrevBackground;
    Statics[BackgroundImageId].Texture.X := theme.EditSub.BackgroundImage.X;
    Statics[BackgroundImageId].Texture.Y := theme.EditSub.BackgroundImage.Y;
    Statics[BackgroundImageId].Texture.W := theme.EditSub.BackgroundImage.W;
    Statics[BackgroundImageId].Texture.H := theme.EditSub.BackgroundImage.H;
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

  case Action of
    maReturn: Result := ParseInput(SDLK_RETURN, 0, true);
//    maLeft:   Result := ParseInput(SDLK_LEFT, 0, true);
//    maRight:  Result := ParseInput(SDLK_RIGHT, 0, true);
    end;

end;

{
procedure TScreenEditSub.NewBeat;
begin
  // click
  for Pet := 0 to Lines[0].Line[Lines[0].Current].HighNut do
    if (Lines[0].Line[Lines[0].Current].Note[Pet].Start = Czas.AktBeat) then
      Music.PlayClick;
end;
}

procedure TScreenEditSub.DivideBPM;
var
  i,j:    integer;

begin
  CurrentSong.BPM[0].BPM := CurrentSong.BPM[0].BPM / 2;

  for i := 0 to Lines[0].High do
  begin
    Lines[0].Line[i].Start := Lines[0].Line[i].Start div 2;
    Lines[0].Line[i].End_  := Lines[0].Line[i].End_ div 2;
    for j := 0 to Lines[0].Line[i].HighNote do
    begin
      Lines[0].Line[i].Note[j].Start  := Lines[0].Line[i].Note[j].Start div 2;
      Lines[0].Line[i].Note[j].Length := Round(Lines[0].Line[i].Note[j].Length / 2);
    end; // j
  end; // i
end;

procedure TScreenEditSub.MultiplyBPM;
var
  i,j:    integer;
begin
  CurrentSong.BPM[0].BPM := CurrentSong.BPM[0].BPM * 2;
  for i := 0 to Lines[0].High do
  begin
    Lines[0].Line[i].Start := Lines[0].Line[i].Start * 2;
    Lines[0].Line[i].End_  := Lines[0].Line[i].End_ * 2;
    for j := 0 to Lines[0].Line[i].HighNote do
    begin
      Lines[0].Line[i].Note[j].Start  := Lines[0].Line[i].Note[j].Start * 2;
      Lines[0].Line[i].Note[j].Length := Lines[0].Line[i].Note[j].Length * 2;
    end; // i
  end; // j
end;

procedure TScreenEditSub.LyricsCapitalize;
var
  i:    integer;
  //N:    integer; // temporary
  S:    UTF8String;
begin
  // temporary
  {
  for C := 0 to Lines[0].High do
    for N := 0 to Lines[0].Line[C].HighNut do
      Lines[0].Line[C].Note[N].Text := UTF8LowerCase(Lines[0].Line[C].Note[N].Text);
  }

  for i := 0 to Lines[0].High do
  begin
    S := UTF8UpperCase(UTF8Copy(Lines[0].Line[i].Note[0].Text, 1, 1));
    S := S + UTF8Copy(Lines[0].Line[i].Note[0].Text, 2, Length(Lines[0].Line[i].Note[0].Text)-1);
    Lines[0].Line[i].Note[0].Text := S;
  end; // i
end;

procedure TScreenEditSub.LyricsCorrectSpaces;
var
  C:    integer;
  N:    integer;
begin
  for C := 0 to Lines[0].High do
  begin
    // correct starting spaces in the first word
    while Copy(Lines[0].Line[C].Note[0].Text, 1, 1) = ' ' do
      Lines[0].Line[C].Note[0].Text := Copy(Lines[0].Line[C].Note[0].Text, 2, 100);

    // move spaces on the start to the end of the previous note
    for N := 1 to Lines[0].Line[C].HighNote do
    begin
      while (Copy(Lines[0].Line[C].Note[N].Text, 1, 1) = ' ') do
      begin
        Lines[0].Line[C].Note[N].Text := Copy(Lines[0].Line[C].Note[N].Text, 2, 100);
        Lines[0].Line[C].Note[N-1].Text := Lines[0].Line[C].Note[N-1].Text + ' ';
      end;
    end; // N

    // correct '-'  to '- '
    for N := 0 to Lines[0].Line[C].HighNote do
    begin
      if Lines[0].Line[C].Note[N].Text = '-' then
        Lines[0].Line[C].Note[N].Text := '- ';
    end; // N

    // add space to the previous note when the current word is '- '
    for N := 1 to Lines[0].Line[C].HighNote do
    begin
      if Lines[0].Line[C].Note[N].Text  = '- ' then
        Lines[0].Line[C].Note[N-1].Text := Lines[0].Line[C].Note[N-1].Text + ' ';
    end; // N

    // correct too many spaces at the end of note
    for N := 0 to Lines[0].Line[C].HighNote do
    begin
      while Copy(Lines[0].Line[C].Note[N].Text, Length(Lines[0].Line[C].Note[N].Text)-1, 2) = '  ' do
        Lines[0].Line[C].Note[N].Text := Copy(Lines[0].Line[C].Note[N].Text, 1, Length(Lines[0].Line[C].Note[N].Text)-1);
    end; // N

    // and correct if there is no space at the end of sentence
    N := Lines[0].Line[C].HighNote;
    if Copy(Lines[0].Line[C].Note[N].Text, Length(Lines[0].Line[C].Note[N].Text), 1) <> ' ' then
      Lines[0].Line[C].Note[N].Text := Lines[0].Line[C].Note[N].Text + ' ';

  end; // C
end;

procedure TScreenEditSub.FixTimings;
var
  C:    integer;
  S:    integer;
  Min:  integer;
  Max:  integer;
begin
  for C := 1 to Lines[0].High do
  begin
    with Lines[0].Line[C-1] do
    begin
      Min := Note[HighNote].Start + Note[HighNote].Length;
      Max := Lines[0].Line[C].Note[0].Start;
      case (Max - Min) of
        0:    S := Max;
        1:    S := Max;
        2:    S := Max - 1;
        3:    S := Max - 2;
        else
          if ((Max - Min) > 4) then
            S := Min + 2
          else
            S := Max;
      end; // case

      Lines[0].Line[C].Start := S;
    end; // with
  end; // for
end;

procedure TScreenEditSub.DivideSentence;
var
  C:      integer;
  CStart: integer;
  CNew:   integer;
  CLen:   integer;
  N:      integer;
  NStart: integer;
  NHigh:  integer;
begin
  // increase sentence length by 1
  CLen := Length(Lines[0].Line);
  SetLength(Lines[0].Line, CLen + 1);
  Inc(Lines[0].Number);
  Inc(Lines[0].High);

  // move needed sentences to one forward. newly has the copy of divided sentence
  CStart := Lines[0].Current;
  for C := CLen-1 downto CStart do
    Lines[0].Line[C+1] := Lines[0].Line[C];

  // clear and set new sentence
  CNew := CStart + 1;
  NStart := CurrentNote;
  Lines[0].Line[CNew].Start := Lines[0].Line[CStart].Note[NStart].Start;
  Lines[0].Line[CNew].Lyric := '';
  Lines[0].Line[CNew].End_ := 0;
  Lines[0].Line[CNew].BaseNote := 0;//High(integer); // TODO: High (integer) will causes a memory exception later in this procedure. Weird!
  Lines[0].Line[CNew].HighNote := -1;
  SetLength(Lines[0].Line[CNew].Note, 0);

  // move right notes to new sentences
  NHigh := Lines[0].Line[CStart].HighNote;
  for N := NStart to NHigh do
  begin
    // increase sentence counters
    with Lines[0].Line[CNew] do
    begin
      Inc(HighNote);
      SetLength(Note, HighNote + 1);
      Note[HighNote] := Lines[0].Line[CStart].Note[N];
      End_ := Note[HighNote].Start + Note[HighNote].Length;
      
      if Note[HighNote].Tone < BaseNote then
        BaseNote := Note[HighNote].Tone;
    end;
  end;

  // clear old notes and set sentence counters
  Lines[0].Line[CStart].HighNote := NStart - 1;
  Lines[0].Line[CStart].End_ := Lines[0].Line[CStart].Note[NStart-1].Start +
    Lines[0].Line[CStart].Note[NStart-1].Length;
  SetLength(Lines[0].Line[CStart].Note, Lines[0].Line[CStart].HighNote + 1);

  //recalculate BaseNote of the divided Sentence
  with Lines[0].Line[CStart] do
  begin
    BaseNote := High(integer);

    for N := 0 to HighNote do
      if Note[N].Tone < BaseNote then
        BaseNote := Note[N].Tone;
  end;

  Lines[0].Current := Lines[0].Current + 1;
  CurrentNote := 0;
  Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 2;
  Lyric.AddLine(Lines[0].Current);
end;

procedure TScreenEditSub.JoinSentence;
var
  C:      integer;
  N:      integer;
  NStart: integer;
  NDst:   integer;
begin
  C := Lines[0].Current;

  // set new sentence
  NStart := Lines[0].Line[C].HighNote + 1;
  Lines[0].Line[C].HighNote := Lines[0].Line[C].HighNote + Lines[0].Line[C+1].HighNote + 1;
  SetLength(Lines[0].Line[C].Note, Lines[0].Line[C].HighNote + 1);

  // move right notes to new sentences
  for N := 0 to Lines[0].Line[C+1].HighNote do
  begin
    NDst := NStart + N;
    Lines[0].Line[C].Note[NDst] := Lines[0].Line[C+1].Note[N];
  end;

  // increase sentence counters
  NDst := Lines[0].Line[C].HighNote;
  Lines[0].Line[C].End_ := Lines[0].Line[C].Note[NDst].Start +
    Lines[0].Line[C].Note[NDst].Length;

  // move needed sentences to one backward.
  for C := Lines[0].Current + 1 to Lines[0].High - 1 do
    Lines[0].Line[C] := Lines[0].Line[C+1];

  // increase sentence length by 1
  SetLength(Lines[0].Line, Length(Lines[0].Line) - 1);
  Dec(Lines[0].Number);
  Dec(Lines[0].High);
end;

procedure TScreenEditSub.NextSentence;
begin
            {$IFDEF UseMIDIPort}
            MidiOut.PutShort($B1, $7, floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
            MidiOut.PutShort(MIDI_NOTEOFF or 1, Lines[0].Line[Lines[0].Current].Note[MidiLastNote].Tone + 60, 127);
            PlaySentenceMidi := false;
            PlayOne := false;
            {$ENDIF}
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
            Inc(Lines[0].Current);
            CurrentNote := 0;
            if Lines[0].Current > Lines[0].High then
              Lines[0].Current := 0;
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 2;

            Lyric.AddLine(Lines[0].Current);
            Lyric.Selected := 0;
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
            MidiOut.PutShort(MIDI_NOTEOFF or 1, $7, floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
            MidiOut.PutShort($81, Lines[0].Line[Lines[0].Current].Note[MidiLastNote].Tone + 60, 127);
            PlaySentenceMidi := false;
            {$endif}

            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
            Dec(Lines[0].Current);
            CurrentNote := 0;
            if Lines[0].Current = -1 then
              Lines[0].Current := Lines[0].High;
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 2;

            Lyric.AddLine(Lines[0].Current);
            Lyric.Selected := 0;
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
  C := Lines[0].Current;
  tempR := 720 / (Lines[0].Line[Lines[0].Current].End_ - Lines[0].Line[Lines[0].Current].Note[0].Start);

  if (doubleclick) and (InteractAt(currentX, CurrentY) > 0) then
      wherecutting := Round((currentX - button[Interactions[InteractAt(currentX, CurrentY)].Num].X) / tempR)
  else
      wherecutting := 1;

  with Lines[0].Line[C] do
  begin
    Inc(HighNote);
    SetLength(Note, HighNote + 1);

    // we copy all notes including selected one
    for N := HighNote downto CurrentNote+1 do
    begin
      Note[N] := Note[N-1];
    end;

    // Note[Cur] and Note[Cur + 1] is identical at this point
    // modify first note
    Note[CurrentNote].Length := wherecutting;

    // 2nd note
    Note[CurrentNote+1].Start := Note[CurrentNote].Start + Note[CurrentNote].Length;
    Note[CurrentNote+1].Length := Note[CurrentNote+1].Length - Note[CurrentNote].Length;

    // find space in text
    spacepos := -1;
    for  N:=0 to LengthUTF8(Note[CurrentNote].Text) do
    begin

      tempstr := UTF8ToUCS4String(Note[CurrentNote].Text);
      if ((UCS4ToUTF8String(tempstr[N]) = ' ') and (spacepos < 0)) then
         spacepos := N;

    end;
    if ((TextPosition < 0) and (ansipos(' ', Note[CurrentNote].Text) > 1) and (ansipos(' ', Note[CurrentNote].Text) < Length(Note[CurrentNote].Text)  )) then
    begin
        Note[CurrentNote+1].Text := UTF8Copy(Note[CurrentNote].Text,spacepos + 2,LengthUTF8(Note[CurrentNote].Text));
        Note[CurrentNote].Text := UTF8Copy(Note[CurrentNote].Text, 1,spacepos+1)
    end
    else
    if ((TextPosition >= 0) and (TextPosition < Length(Note[CurrentNote].Text))) then
    begin
        Note[CurrentNote+1].Text := UTF8Copy(SelectsS[LyricSlideId].TextOpt[0].Text, TextPosition+2, LengthUTF8(SelectsS[LyricSlideId].TextOpt[0].Text));
        Note[CurrentNote].Text := UTF8Copy(SelectsS[LyricSlideId].TextOpt[0].Text, 1, TextPosition);
        SelectsS[LyricSlideId].TextOpt[0].Text := Note[CurrentNote].Text;
        TextPosition := -1;
    end
    else
        Note[CurrentNote+1].Text := '~';
    Note[CurrentNote+1].Color := 1;
  end;

  // update lyric display
  Lyric.AddLine(Lines[0].Current);
  Lyric.Selected := CurrentNote;
end;

procedure TScreenEditSub.DeleteNote;
var
  C:    integer;
  N:    integer;
begin
  C := Lines[0].Current;

  //Do Not delete Last Note
  if (Lines[0].Line[C].HighNote > 0) then
  begin
    // we copy all notes from the next to the selected one
    for N := CurrentNote+1 to Lines[0].Line[C].HighNote do
    begin
      Lines[0].Line[C].Note[N-1] := Lines[0].Line[C].Note[N];
    end;
    
    Dec(Lines[0].Line[C].HighNote);

    SetLength(Lines[0].Line[C].Note, Lines[0].Line[C].HighNote + 1);

    // last note was deleted
    if (CurrentNote > Lines[0].Line[C].HighNote) then
    begin
      // select new last note
      CurrentNote := Lines[0].Line[C].HighNote;

      // correct Line ending
      with Lines[0].Line[C] do
        End_ := Note[HighNote].Start + Note[HighNote].Length;
    end;

    Lines[0].Line[C].Note[CurrentNote].Color := 2;
  end
  // Last Note of current Sentence Deleted - > Delete Sentence
  // if there are more than two left
  else if (Lines[0].High > 1) then
  begin
    //Move all Sentences after the current to the Left
    for N := C+1 to Lines[0].High do
      Lines[0].Line[N-1] := Lines[0].Line[N];

    //Delete Last Sentence
    SetLength(Lines[0].Line, Lines[0].High);
    Lines[0].High := High(Lines[0].Line);
    Lines[0].Number := Length(Lines[0].Line);

    CurrentNote := 0;
    if (C > 0) then
      Lines[0].Current := C - 1
    else
      Lines[0].Current := 0;

    Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 2;
  end;

  // update lyric display
  Lyric.AddLine(Lines[0].Current);
  Lyric.Selected := CurrentNote;
end;

procedure TScreenEditSub.TransposeNote(Transpose: integer);
begin
  Inc(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Tone, Transpose);
end;

procedure TScreenEditSub.ChangeWholeTone(Tone: integer);
var
  C:  integer;
  N:  integer;
begin
  for C := 0 to Lines[0].High do
  begin
    Lines[0].Line[C].BaseNote := Lines[0].Line[C].BaseNote + Tone;
    for N := 0 to Lines[0].Line[C].HighNote do
      Lines[0].Line[C].Note[N].Tone := Lines[0].Line[C].Note[N].Tone + Tone;
  end;
end;

procedure TScreenEditSub.MoveAllToEnd(Move: integer);
var
  C:    integer;
  N:    integer;
  NStart: integer;
begin
  for C := Lines[0].Current to Lines[0].High do
  begin
    NStart := 0;
    if C = Lines[0].Current then
      NStart := CurrentNote;
    for N := NStart to Lines[0].Line[C].HighNote do
    begin
      Inc(Lines[0].Line[C].Note[N].Start, Move); // move note start

      if N = 0 then
      begin // fix beginning
        Inc(Lines[0].Line[C].Start, Move);
      end;

      if N = Lines[0].Line[C].HighNote then // fix ending
        Inc(Lines[0].Line[C].End_, Move);

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
  C := Lines[0].Current;

  for N := Lines[0].Line[C].HighNut downto 1 do
  begin
    Lines[0].Line[C].Note[N].Text := Lines[0].Line[C].Note[N-1].Text;
  end; // for

  Lines[0].Line[C].Note[0].Text := '- ';
  }

  C := Lines[0].Current;
  NHigh := Lines[0].Line[C].HighNote;

  // last word
  Lines[0].Line[C].Note[NHigh].Text := Lines[0].Line[C].Note[NHigh-1].Text + Lines[0].Line[C].Note[NHigh].Text;

  // other words
  for N := NHigh - 1 downto CurrentNote + 1 do
  begin
    Lines[0].Line[C].Note[N].Text := Lines[0].Line[C].Note[N-1].Text;
  end; // for
  Lines[0].Line[C].Note[CurrentNote].Text := '- ';
end;

procedure TScreenEditSub.MarkSrc;
begin
  CopySrc := Lines[0].Current;
end;

procedure TScreenEditSub.PasteText;
var
  C:    integer;
  N:    integer;
begin
  C := Lines[0].Current;

  for N := 0 to Lines[0].Line[CopySrc].HighNote do
    Lines[0].Line[C].Note[N].Text := Lines[0].Line[CopySrc].Note[N].Text;
end;

procedure TScreenEditSub.CopySentence(Src, Dst: integer);
var
  N:     integer;
  Time1: integer;
  Time2: integer;
  TD:    integer;
begin
  Time1 := Lines[0].Line[Src].Note[0].Start;
  Time2 := Lines[0].Line[Dst].Note[0].Start;
  TD := Time2-Time1;

  SetLength(Lines[0].Line[Dst].Note, Lines[0].Line[Src].HighNote + 1);
  Lines[0].Line[Dst].HighNote := Lines[0].Line[Src].HighNote;
  for N := 0 to Lines[0].Line[Src].HighNote do
  begin
    Lines[0].Line[Dst].Note[N].Text := Lines[0].Line[Src].Note[N].Text;
    Lines[0].Line[Dst].Note[N].Length := Lines[0].Line[Src].Note[N].Length;
    Lines[0].Line[Dst].Note[N].Tone := Lines[0].Line[Src].Note[N].Tone;
    Lines[0].Line[Dst].Note[N].Start := Lines[0].Line[Src].Note[N].Start + TD;
  end;
  N := Lines[0].Line[Src].HighNote;
  Lines[0].Line[Dst].End_ := Lines[0].Line[Dst].Note[N].Start + Lines[0].Line[Dst].Note[N].Length;
end;

procedure TScreenEditSub.CopySentences(Src, Dst, Num: integer);
var
  C:      integer;
begin
  // create place for new sentences
  SetLength(Lines[0].Line, Lines[0].Number + Num - 1);

  // moves sentences next to the destination
  for C := Lines[0].High downto Dst + 1 do
  begin
    Lines[0].Line[C + Num - 1] := Lines[0].Line[C];
  end;

  // prepares new sentences: sets sentence start and create first note
  for C := 1 to Num-1 do
  begin
    Lines[0].Line[Dst + C].Start := Lines[0].Line[Dst + C - 1].Note[0].Start +
      (Lines[0].Line[Src + C].Note[0].Start - Lines[0].Line[Src + C - 1].Note[0].Start);
    SetLength(Lines[0].Line[Dst + C].Note, 1);
    Lines[0].Line[Dst + C].HighNote := 0;
    Lines[0].Line[Dst + C].Note[0].Start := Lines[0].Line[Dst + C].Start;
    Lines[0].Line[Dst + C].Note[0].Length := 1;
    Lines[0].Line[Dst + C].End_ := Lines[0].Line[Dst + C].Start + 1;
  end;

  // increase counters
  Lines[0].Number := Lines[0].Number + Num - 1;
  Lines[0].High := Lines[0].High + Num - 1;

  for C := 0 to Num-1 do
    CopySentence(Src + C, Dst + C);
end;

procedure TScreenEditSub.CopyToUndo;
var
 I,J: integer;
begin
  SetLength(UndoLines, high(UndoLines)+2);
  CurrentUndoLines := high(UndoLines);
  SetLength(UndoStateNote, CurrentUndoLines+1);
  SetLength(UndoHeader, CurrentUndoLines+1);

  Undoheader[CurrentUndoLines].Title := CurrentSong.Title;
  Undoheader[CurrentUndoLines].Artist := CurrentSong.Artist;
  Undoheader[CurrentUndoLines].CoverId := SelectsS[CoverSlideId].SelectedOption;
  Undoheader[CurrentUndoLines].BackgroundId := SelectsS[BackgroundSlideId].SelectedOption;
  Undoheader[CurrentUndoLines].Mp3Id := SelectsS[Mp3SlideId].SelectedOption;
  Undoheader[CurrentUndoLines].GAP  := CurrentSong.GAP;
  Undoheader[CurrentUndoLines].Video := CurrentSong.Video;
  Undoheader[CurrentUndoLines].VideoGAP := CurrentSong.VideoGAP;
  SetLength(Undoheader[CurrentUndoLines].BPM, length(CurrentSong.BPM));
  for I:=0 to length(CurrentSong.BPM)-1 do
  begin
    Undoheader[CurrentUndoLines].BPM[I].BPM := CurrentSong.BPM[I].BPM;
    Undoheader[CurrentUndoLines].BPM[I].StartBeat := CurrentSong.BPM[I].StartBeat;
  end;

  UndoStateNote[CurrentUndoLines] := currentnote;

  UndoLines[CurrentUndoLines].Current := Lines[0].Current;
  UndoLines[CurrentUndoLines].High := Lines[0].High;
  UndoLines[CurrentUndoLines].Number := Lines[0].Number;
  UndoLines[CurrentUndoLines].Resolution := Lines[0].Resolution;
  UndoLines[CurrentUndoLines].NotesGAP := Lines[0].NotesGAP;
  UndoLines[CurrentUndoLines].ScoreValue := Lines[0].ScoreValue;
  SetLength(UndoLines[CurrentUndoLines].Line, length(Lines[0].Line));

  for I:=0 to length(Lines[0].Line)-1 do
  begin
    UndoLines[CurrentUndoLines].Line[I].Start := Lines[0].Line[I].Start;
    UndoLines[CurrentUndoLines].Line[I].Lyric := Lines[0].Line[I].Lyric;
    UndoLines[CurrentUndoLines].Line[I].End_ := Lines[0].Line[I].End_;
    UndoLines[CurrentUndoLines].Line[I].BaseNote := Lines[0].Line[I].BaseNote;
    UndoLines[CurrentUndoLines].Line[I].HighNote := Lines[0].Line[I].HighNote;
    UndoLines[CurrentUndoLines].Line[I].TotalNotes := Lines[0].Line[I].TotalNotes;
    UndoLines[CurrentUndoLines].Line[I].LastLine := Lines[0].Line[I].LastLine;

    SetLength(UndoLines[CurrentUndoLines].Line[I].Note, length(Lines[0].Line[I].Note));
    for J:=0 to length(Lines[0].Line[I].Note)-1 do
    begin
      UndoLines[CurrentUndoLines].Line[I].Note[J].Color := Lines[0].Line[I].Note[J].Color;
      UndoLines[CurrentUndoLines].Line[I].Note[J].Start := Lines[0].Line[I].Note[J].Start;
      UndoLines[CurrentUndoLines].Line[I].Note[J].Length := Lines[0].Line[I].Note[J].Length;
      UndoLines[CurrentUndoLines].Line[I].Note[J].Tone := Lines[0].Line[I].Note[J].Tone;
      UndoLines[CurrentUndoLines].Line[I].Note[J].Text := Lines[0].Line[I].Note[J].Text;
      UndoLines[CurrentUndoLines].Line[I].Note[J].NoteType := Lines[0].Line[I].Note[J].NoteType;
    end; //for J
  end; //for I
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
  SelectsS[CoverSlideId].SelectedOption := Undoheader[CurrentUndoLines].CoverId;
  SelectsS[BackgroundSlideId].SelectedOption := Undoheader[CurrentUndoLines].BackgroundId;
  SelectsS[Mp3SlideId].SelectedOption := Undoheader[CurrentUndoLines].Mp3Id;
  CurrentSong.GAP := Undoheader[CurrentUndoLines].GAP;
  CurrentSong.Video := Undoheader[CurrentUndoLines].Video;
  CurrentSong.VideoGAP := Undoheader[CurrentUndoLines].VideoGAP;

  currentnote := UndoStateNote[high(UndoStateNote)];

  SetLength(CurrentSong.BPM, length(Undoheader[CurrentUndoLines].BPM));
  for I:=0 to length(Undoheader[CurrentUndoLines].BPM)-1 do
  begin
    CurrentSong.BPM[I].BPM := Undoheader[CurrentUndoLines].BPM[I].BPM;
    CurrentSong.BPM[I].StartBeat := Undoheader[CurrentUndoLines].BPM[I].StartBeat;
  end;
  Lines[0].Current := UndoLines[CurrentUndoLines].Current;
  Lines[0].High := UndoLines[CurrentUndoLines].High;
  Lines[0].Number := UndoLines[CurrentUndoLines].Number;
  Lines[0].Resolution := UndoLines[CurrentUndoLines].Resolution;
  Lines[0].NotesGAP := UndoLines[CurrentUndoLines].NotesGAP;
  Lines[0].ScoreValue := UndoLines[CurrentUndoLines].ScoreValue;
  SetLength(Lines[0].Line, length(UndoLines[CurrentUndoLines].Line));
  for I:=0 to length(UndoLines[CurrentUndoLines].Line)-1 do
  begin
      Lines[0].Line[I].Start := UndoLines[CurrentUndoLines].Line[I].Start;
      Lines[0].Line[I].Lyric := UndoLines[CurrentUndoLines].Line[I].Lyric;
      Lines[0].Line[I].End_ := UndoLines[CurrentUndoLines].Line[I].End_;
      Lines[0].Line[I].BaseNote := UndoLines[CurrentUndoLines].Line[I].BaseNote;
      Lines[0].Line[I].HighNote := UndoLines[CurrentUndoLines].Line[I].HighNote;
      Lines[0].Line[I].TotalNotes := UndoLines[CurrentUndoLines].Line[I].TotalNotes;
      Lines[0].Line[I].LastLine := UndoLines[CurrentUndoLines].Line[I].LastLine;

      SetLength(Lines[0].Line[I].Note, length(UndoLines[CurrentUndoLines].Line[I].Note));
      for J:=0 to length(UndoLines[CurrentUndoLines].Line[I].Note)-1 do
      begin
        Lines[0].Line[I].Note[J].Color := UndoLines[CurrentUndoLines].Line[I].Note[J].Color;
        Lines[0].Line[I].Note[J].Start := UndoLines[CurrentUndoLines].Line[I].Note[J].Start;
        Lines[0].Line[I].Note[J].Length := UndoLines[CurrentUndoLines].Line[I].Note[J].Length;
        Lines[0].Line[I].Note[J].Tone := UndoLines[CurrentUndoLines].Line[I].Note[J].Tone;
        Lines[0].Line[I].Note[J].Text := UndoLines[CurrentUndoLines].Line[I].Note[J].Text;
        Lines[0].Line[I].Note[J].NoteType := UndoLines[CurrentUndoLines].Line[I].Note[J].NoteType;
      end; //for J
  end; //for I
  SetLength(UndoStateNote, high(UndoStateNote));
  SetLength(UndoHeader, high(UndoLines));
  SetLength(UndoLines, high(UndoLines));
  Text[TextDebug].Text := Language.Translate('INFO_UNDO');
  // to refresh all headers
  SelectsS[LyricSlideId].TextOpt[0].Text := Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text;
  SelectsS[ArtistSlideId].TextOpt[0].Text := CurrentSong.Artist;
  SelectsS[TitleSlideId].TextOpt[0].Text := CurrentSong.Title;
  Lyric.AddLine(Lines[0].Current);
  Lyric.Selected := CurrentNote;
end; //if CurrentUndoLines
end;

procedure TScreenEditSub.DrawPlayerTrack(X, Y, W: real; Space: integer; CurrentTone: integer; Count: integer; CurrentNote: integer);
var
  TempR:      real;
  Rec:        TRecR;
  N, scale:          integer;
//  R, G, B, A: real;
  NotesH2,W1,H1,X1,X2:    real;
begin

  glColor3f(1, 1, 1);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    TempR := W / (Lines[0].Line[Lines[0].Current].End_ - Lines[0].Line[Lines[0].Current].Note[0].Start);

          NotesH2 := int(NotesH[0] * 0.65);
            W1 := NotesW[0] * 2 + 2;
            H1 := NotesH[0] * 1.5;// + 3.5;
            X2 := 40 + 0.5 + 10*ScreenX+Count;
            X1 := X2-W1-2;

            Rec.Left  := X1;
            Rec.Right := X2;
            scale := 0;
            repeat
            if (Lines[0].Line[Lines[0].Current].Note[CurrentNote].Tone+12*scale > CurrentTone) then
              dec(scale)
            else
              inc(scale);

            until (
                  (((Lines[0].Line[Lines[0].Current].Note[CurrentNote].Tone + 12*scale) / 12) < 1) and
                  (((Lines[0].Line[Lines[0].Current].Note[CurrentNote].Tone + 12*scale) / 12) >= 0));

            Rec.Top := 410 - (CurrentTone-12*scale-Lines[0].Line[Lines[0].Current].BaseNote)*Space/2 - H1;
            Rec.Bottom := Rec.Top + 2 * H1;

        glColor3f(1, 1, 1);
        glBindTexture(GL_TEXTURE_2D, Tex_Lyric_Help_Bar.TexNum);
        glBegin(GL_QUADS);
          glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
          glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
          glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
          glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
        glEnd;
  SetFontPos(90, 560);
  glColor4f(1, 1, 0, 1);
  glPrint (GetNoteName(CurrentTone));
end;

procedure TScreenEditSub.DrawStatics_UP(X, Y, W, H: integer);
begin
  //Theme:
  //bg
  glDisable(GL_BLEND);
{
  x := 0;
  y := 0;
  w := 800;
  h := 600;
  glColor4f(0.3, 0.5, 0.6, 1);
  glbegin(gl_quads);
   glVertex2f(x, y);
   glVertex2f(x, y+h);
   glVertex2f(x+w, y+h);
   glVertex2f(x+w, y);
  glEnd;}

  // Line
{  glColor4f(0.9, 0.9, 0.9, 1);
  x := 20;
  y := 5;
  w := 200;
  h := 40;
  glbegin(gl_quads);
   glVertex2f(x, y);
   glVertex2f(x, y+h);
   glVertex2f(x+w, y+h);
   glVertex2f(x+w, y);
  glEnd;

  // Note
  x := 260;
  y := 5;
  w := 200;
  h := 40;
  glbegin(gl_quads);
   glVertex2f(x, y);
   glVertex2f(x, y+h);
   glVertex2f(x+w, y+h);
   glVertex2f(x+w, y);
  glEnd;}

  // some borders

  glColor4f(0.9, 0.9, 0.9, 0.5);
  glbegin(gl_quads);
   glVertex2f(x, y);
   glVertex2f(x, y+h);
   glVertex2f(x+w, y+h);
   glVertex2f(x+w, y);
  glEnd;

  glColor4f(0, 0, 0, 1);
  glLineWidth(2);
  glBegin(GL_LINE_LOOP);
    glVertex2f(x-1, y-1);
    glVertex2f(x+w+1, y-1);
    glVertex2f(x+w+1, y+h+1);
    glVertex2f(x-1, y+h+1);
  glEnd;

end;
procedure TScreenEditSub.DrawStatics_Notes(X, Y, W, H: integer);
begin
{  x := 20;
  y := 305;
  w := 760;
  h := 135;}
  glColor4f(0.9, 0.9, 0.9, 1);
  glbegin(gl_quads);
   glVertex2f(x, y);
   glVertex2f(x, y+h);
   glVertex2f(x+w, y+h);
   glVertex2f(x+w, y);
  glEnd;

  glColor4f(0, 0, 0, 1);
  glLineWidth(2);
  glBegin(GL_LINE_LOOP);
    glVertex2f(x-1, y-1);
    glVertex2f(x+w+1, y-1);
    glVertex2f(x+w+1, y+h+1);
    glVertex2f(x-1, y+h+1);
  glEnd;
end;

procedure TScreenEditSub.DrawStatics_Sentences(X, Y, W, H: integer);
begin
  //Theme:
  //bg
  glDisable(GL_BLEND);
{  x := 20;
  y := 500;
  w := 760;
  h := 40;}
  glColor4f(0.9, 0.9, 0.9, 1);
  glbegin(gl_quads);
   glVertex2f(x, y);
   glVertex2f(x, y+h);
   glVertex2f(x+w, y+h);
   glVertex2f(x+w, y);
  glEnd;

  glColor4f(0, 0, 0, 1);
  glLineWidth(2);
  glBegin(GL_LINE_LOOP);
    glVertex2f(x-1, y-1);
    glVertex2f(x+w+1, y-1);
    glVertex2f(x+w+1, y+h+1);
    glVertex2f(x-1, y+h+1);
  glEnd;

  glLineWidth(1);
end;

procedure TScreenEditSub.DrawInfoBar(x, y, w, h: integer; currentLines: integer);
var
  start, end_:        integer;
  SongStart, SongEnd: integer;
  ww:                 integer;
  i:                  integer;

  pos:                real;
  br:                 real;

  line:               integer;
  numLines:           integer;

begin
  numLines := Length(Lines[currentLines].Line);

  glColor4f(0, 0, 0, 1);
  glDisable(GL_BLEND);
  glLineWidth(2);
  glBegin(GL_LINE_LOOP);
    glVertex2f(x-1, y-1);
    glVertex2f(x+w+1, y-1);
    glVertex2f(x+w+1, y+h+1);
    glVertex2f(x-1, y+h+1);
  glEnd;

  glColor4f(0.9, 0.9, 0.9, 1);
  glbegin(gl_quads);
   glVertex2f(x, y);
   glVertex2f(x, y+h);
   glVertex2f(x+w, y+h);
   glVertex2f(x+w, y);
  glEnd;

  if(numLines=1) then
    Exit;

  SongStart := Lines[currentLines].Line[0].Note[0].Start;
  SongEnd := Lines[currentLines].Line[numLines-1].End_;
  ww := SongEnd - SongStart;

  Statics[playerIconId[currentLines+1]].Visible := true;

  for i := 0 to Length(TransparentLineButtonId)-1 do
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
//      AddButton(0, 0, 0, 0,Skin.GetTextureFileName('ButtonF'));
      InteractiveLineId[Length(InteractiveLineId)-1] := length(Interactions)-1;
  end;

  for line := 0 to numLines - 1 do
  begin
    if (line = Lines[currentLines].Current) and not (PlaySentence or PlaySentenceMidi or PlayOne) then
      glColor4f(0.4, 0.4, 0, 1)
    else
      glColor4f(1, 0.6, 0, 1);


    start := Lines[currentLines].Line[line].Note[0].Start;
    end_ := Lines[currentLines].Line[line].Note[Lines[0].Line[line].HighNote].Start+
      Lines[currentLines].Line[line].Note[Lines[0].Line[line].HighNote].Length;

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
    br := (AktBeat - SongStart)/ww*w;
    if (br>w) then
      br := w;
  end else
  begin
    glColor4f(1, 0, 0, 1);
    pos := (Lines[currentLines].Line[Lines[0].Current].Note[CurrentNote].Start - SongStart)/ww*w;
    br := Lines[currentLines].Line[Lines[0].Current].Note[CurrentNote].Length/ww*w;
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

procedure TScreenEditSub.DrawText(Left, Top, Right: real; NrLines: integer; Space: integer);
var
  Rec:   TRecR;
  Count: integer;
  TempR: real;

  PlayerNumber:  integer;

  GoldenStarPos: real;
begin
  if ( (1 shl NrLines) <> 0) then
  begin
    PlayerNumber := NrLines + 1; // Player 1 is 0
    glColor3f(1, 1, 1);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    if not Lines[NrLines].Line[Lines[NrLines].Current].HasLength(TempR) then TempR := 0
    else TempR := (Right-Left) / TempR;

    with Lines[NrLines].Line[Lines[NrLines].Current] do
    begin
      for Count := 0 to HighNote do
      begin
        with Note[Count] do
        begin
          // left part
          Rec.Left  := 0;
          Rec.Right := 0;
          BaseNote := Lines[0].Line[Lines[NrLines].Current].BaseNote;
          Rec.Top := Top - (Tone-BaseNote)*Space/2 - NotesH[0];
          Rec.Bottom := Rec.Top + 2 * NotesH[0];
          // middle part
          Rec.Left := (Start-Lines[NrLines].Line[Lines[NrLines].Current].Note[0].Start) * TempR + Left + 0.5 + 10*ScreenX + NotesW[0];
          Rec.Right := (Start+Length-Lines[NrLines].Line[Lines[NrLines].Current].Note[0].Start) * TempR + Left - NotesW[0] - 0.5 + 10*ScreenX;
          glColor4f(0, 0, 0, 1);
          SetFontStyle (1);
          SetFontItalic(False);
          SetFontSize(14);
          SetFontPos (Rec.Left, Rec.Top);
          glPrint(Text);
        end; // with
      end; // for
    end; // with

    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
  end;
end;

// show transparent background for intaractive note

procedure TScreenEditSub.ShowInteractiveBackground;
var
  TempR:      real;
  i:          integer;
begin

  for i := 0 to Length(TransparentNoteButtonId)-1 do
  begin
    Button[TransparentNoteButtonId[i]].SetX(0);
    Button[TransparentNoteButtonId[i]].SetY(0);
    Button[TransparentNoteButtonId[i]].SetW(0);
    Button[TransparentNoteButtonId[i]].SetH(0);
  end;

// adding transparent buttons
  while (Length(TransparentNoteButtonId)-1 < Lines[0].Line[Lines[0].Current].HighNote) do
  begin
      SetLength(InteractiveNoteId, Length(InteractiveNoteId)+1);
      SetLength(TransparentNoteButtonId, Length(TransparentNoteButtonId)+1);
      TransparentNoteButtonId[Length(TransparentNoteButtonId)-1] := AddButton(0, 0, 0, 0,PATH_NONE);
      InteractiveNoteId[Length(InteractiveNoteId)-1] := length(Interactions)-1;

  end;
  TempR := 720 / (Lines[0].Line[Lines[0].Current].End_ - Lines[0].Line[Lines[0].Current].Note[0].Start);
  for i := 0 to Lines[0].Line[Lines[0].Current].HighNote do
  begin
    Button[TransparentNoteButtonId[i]].SetX(40 + (Lines[0].Line[Lines[0].Current].Note[i].Start - Lines[0].Line[Lines[0].Current].Note[0].Start) * TempR + 0.5 + 10*ScreenX);
    Button[TransparentNoteButtonId[i]].SetY(410 - (Lines[0].Line[Lines[0].Current].Note[i].Tone - Lines[0].Line[Lines[0].Current].BaseNote)*15/2 - 9);
    Button[TransparentNoteButtonId[i]].SetW((Lines[0].Line[Lines[0].Current].Note[i].Length) * TempR - 0.5  + 10*(ScreenX));
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

constructor TScreenEditSub.Create;
var
i: integer;
begin
  inherited Create;
  LoadFromTheme(Theme.EditSub);
  //video
  fCurrentVideo := nil;
  SetLength(Player, 1);
  SetLength(TitleVal, 0);
  SetLength(ArtistVal, 0);
  SetLength(MP3Val, 0);
  SetLength(CoverVal, 0);
  SetLength(BackgroundVal, 0);
  SetLength(BPMVal, 0);
  SetLength(GAPVal, 0);
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

  // line
 // AddText(40, 11, 1, 20, 0, 0, 0, 'Line:');
  TextSentence := AddButton(Theme.EditSub.ButtonCurrentLine);
//  TextSentence := AddText(110, 11, 1, 20, 0, 0, 0, '0 / 0');

  // Note
//  AddText(282, 11, 1, 20, 0, 0, 0, 'Note:');
  TextNote := AddButton(Theme.EditSub.ButtonCurrentNote);
//  TextNote := AddText(360, 11, 1, 20, 0, 0, 0, '0 / 0');

  // file info
  //  AddText(30, 65,  0, 24, 0, 0, 0, 'Title:');
  // Title Header
  TitleSlideId := AddSelectSlide(Theme.EditSub.SlideTitle, Titledata, TitleVal);
  SelectsS[TitleSlideId].Text.Align := 0;
  SelectsS[TitleSlideId].Text.X := SelectsS[TitleSlideId].Texture.X + 3;

  //  AddText(30, 90,  0, 24, 0, 0, 0, 'Artist:');
  // Artist Header
  ArtistSlideId := AddSelectSlide(Theme.EditSub.SlideArtist, Artistdata, ArtistVal);
  SelectsS[ArtistSlideId].Text.Align := 0;
  SelectsS[ArtistSlideId].Text.X := SelectsS[ArtistSlideId].Texture.X + 3;

  //AddText(30, 115, 0, 24, 0, 0, 0, 'Mp3:');
  // Artist Header
  MP3SlideId := AddSelectSlide(Theme.EditSub.SlideMP3, MP3data, MP3Val);
  SelectsS[MP3SlideId].Text.Align := 0;
  SelectsS[MP3SlideId].Text.X := SelectsS[MP3SlideId].Texture.X + 3;
  // Cover Header
  CoverSlideId := AddSelectSlide(Theme.EditSub.SlideCover, Coverdata, CoverVal);
  SelectsS[CoverSlideId].Text.Align := 0;
  SelectsS[CoverSlideId].Text.X := SelectsS[CoverSlideId].Texture.X + 3;
  // Background Header
  BackgroundSlideId := AddSelectSlide(Theme.EditSub.SlideBackground, Backgrounddata, BackgroundVal);
  SelectsS[BackgroundSlideId].Text.Align := 0;
  SelectsS[BackgroundSlideId].Text.X := SelectsS[BackgroundSlideId].Texture.X + 3;
  //  AddText(30, 140, 0, 24, 0, 0, 0, 'BPM:');
  // BPM Header
  BPMSlideId := AddSelectSlide(Theme.EditSub.SlideBPM, BPMdata, BPMVal);
  SelectsS[BPMSlideId].Text.Align := 0;
  SelectsS[BPMSlideId].Text.X := SelectsS[BPMSlideId].Texture.X + 3;
  //AddText(30, 165, 0, 24, 0, 0, 0, 'GAP:');
  // GAP Header
  GAPSlideId := AddSelectSlide(Theme.EditSub.SlideGAP, GAPdata, GAPVal);
  SelectsS[GAPSlideId].Text.Align := 0;
  SelectsS[GAPSlideId].Text.X := SelectsS[GAPSlideId].Texture.X + 3;
  // Start Header
  StartSlideId := AddSelectSlide(Theme.EditSub.SlideStart, Startdata, StartVal);
  SelectsS[StartSlideId].Text.Align := 0;
  SelectsS[StartSlideId].Text.X := SelectsS[StartSlideId].Texture.X + 3;
  // Duration Header
  DurationSlideId := AddSelectSlide(Theme.EditSub.SlideDuration, Durationdata, DurationVal);
  SelectsS[DurationSlideId].Text.Align := 0;
  SelectsS[DurationSlideId].Text.X := SelectsS[StartSlideId].Texture.X + 3;
  // Tone Header
  ToneSlideId := AddSelectSlide(Theme.EditSub.SlideTone, Tonedata, ToneVal);
  SelectsS[ToneSlideId].Text.Align := 0;
  SelectsS[ToneSlideId].Text.X := SelectsS[ToneSlideId].Texture.X + 3;
  // Text Header
  LyricSlideId := AddSelectSlide(Theme.EditSub.SlideLyric, Lyricdata, LyricVal);
  SelectsS[LyricSlideId].Text.Align := 0;
  SelectsS[LyricSlideId].Text.X := SelectsS[LyricSlideId].Texture.X + 3;

  VolumeAudioSlideId := AddSelectSlide(Theme.EditSub.SelectVolAudio, VolumeAudioIndex, VolumeAudio);
  VolumeMidiSlideId := AddSelectSlide(Theme.EditSub.SelectVolMidi, VolumeMidiIndex, VolumeMidi);
  VolumeClickSlideId := AddSelectSlide(Theme.EditSub.SelectVolClick, VolumeClickIndex, VolumeClick);
  // VideoGap Header
  VideoGapSlideId := AddSelectSlide(Theme.EditSub.SlideVideoGap, VideoGapdata, VideoGapVal);
  SelectsS[VideoGapSlideId].Text.Align := 0;
  SelectsS[VideoGapSlideId].Text.X := SelectsS[VideoGapSlideId].Texture.X + 3;

  // background image & preview
  BackgroundImageId := AddStatic(Theme.EditSub.BackgroundImage);

//  TextTitle :=  AddText(180, 65,  0, 24, 0, 0, 0, 'a');
//  TextArtist := AddText(180, 90,  0, 24, 0, 0, 0, 'b');
//  TextMp3 :=    AddText(180, 115, 0, 24, 0, 0, 0, 'c');
//  TextBPM :=    AddText(180, 140, 0, 24, 0, 0, 0, 'd');
//  TextGAP :=    AddText(180, 165, 0, 24, 0, 0, 0, 'e');

  // note info
//  AddText(30, 190,  0, 24, 0, 0, 0, 'Start:');
//  AddText(30, 215,  0, 24, 0, 0, 0, 'Duration:');
//  AddText(30, 240,  0, 24, 0, 0, 0, 'Tone:');
//  AddText(30, 265,  0, 24, 0, 0, 0, 'Text:');      //AddText(500, 265,  0, 8, 0, 0, 0, 'VideoGap:');

//  TextNStart :=   AddText(180, 190,  0, 24, 0, 0, 0, 'a');
//  TextNLength :=  AddText(180, 215,  0, 24, 0, 0, 0, 'b');
//  TextNTon :=     AddText(180, 240,  0, 24, 0, 0, 0, 'c');
//  TextNText :=    AddText(180, 265,  0, 24, 0, 0, 0, 'd');

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
    // control buttons
    AddButton(Theme.EditSub.PlayOnly);
    AddButton(Theme.EditSub.PlayWithNote);
    AddButton(Theme.EditSub.PlayNote);
    AddButton(Theme.EditSub.previousseq);
    AddButton(Theme.EditSub.nextseq);
    UndoButtonId := AddButton(Theme.EditSub.undo);
    AddButton(Theme.EditSub.gold);
    AddButton(Theme.EditSub.freestyle);

  // debug
  TextDebug :=  AddText(30, 550, 0, 27, 0, 0, 0, '');
  // in notes place -> for move notes by mouse
//  NotesBackgroundId := AddSelectSlide(Theme.EditSub.NotesBackground, i, Empty);

end;

procedure TScreenEditSub.OnShow;
var
  FileExt: IPath;
  Files: TPathDynArray;
  i: integer;

  function IsBeatMatchingNote(beat: integer; Note: TLineFragment): boolean;
  begin
    Result := InRange(beat, Note.Start, Note.Start+Note.Length);
  end;

  // borrowed from TScreenSing.LoadNextSong
  function FindNote(beat: integer): TPos;
  var
    line:  integer;
    note:  integer;
    diff, mindiff: integer;

  begin
    for line := 0 to length(Lines[0].Line) - 1 do
    begin
      for note := 0 to length(Lines[0].Line[line].Note) - 1 do
      begin
        if IsBeatMatchingNote(beat, Lines[0].Line[line].Note[note]) then
        begin
          Result.cp := 0;
          Result.line := line;
          Result.note := note;
          Exit;
        end;

        diff := abs(Lines[0].Line[line].Note[note].Start - beat);
        if diff < mindiff then
        begin
          mindiff := diff;
          Result.line := line;
          Result.note := note;
        end;
      end;
    end;

    //second try (approximating)
    mindiff := high(integer);
    for line := 0 to length(Lines[0].Line) - 1 do
    begin
      for note := 0 to length(Lines[0].Line[line].Note) - 1 do
      begin
        diff := abs(Lines[0].Line[line].Note[note].Start - beat);
        if diff < mindiff then
        begin
          mindiff := diff;
          Result.line := line;
          Result.note := note;
        end;
      end;
    end;

    // return approximated note
    if diff > 0 then
    begin
      Result.cp := 0;
      Exit;
    end;

    Result.cp := 0;
    Result.line := -1;
    Result.note := -1;
  end;
begin
  inherited;
  // reset video playback engine
  fCurrentVideo := nil;
  AudioPlayback.Stop;
  PlaySentence := false;
  PlayOne := false;
  PlaySentenceMidi := false;
  Text[TextDebug].Text := '';
  Log.LogStatus('Initializing', 'TEditScreen.OnShow');
  Lyric := TEditorLyrics.Create;
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
    //    Text[TextTitle].Text :=   CurrentSong.Title;
    // Header Title
    SetLength(TitleVal, 1);
    TitleVal[0] := CurrentSong.Title;
    SlideTitleIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlideTitle,TitleSlideId,TitleVal,SlideTitleIndex);
    SelectsS[TitleSlideId].TextOpt[0].Align := 0;
    SelectsS[TitleSlideId].TextOpt[0].X := SelectsS[TitleSlideId].TextureSBG.X + 5;

    //    Text[TextArtist].Text :=  CurrentSong.Artist;
    // Header Artist
    SetLength(ArtistVal, 1);
    ArtistVal[0] := CurrentSong.Artist;
    SlideArtistIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlideArtist,ArtistSlideId,ArtistVal,SlideArtistIndex);
    SelectsS[ArtistSlideId].TextOpt[0].Align := 0;
    SelectsS[ArtistSlideId].TextOpt[0].X := SelectsS[ArtistSlideId].TextureSBG.X + 5;

    //Text[TextMp3].Text :=     CurrentSong.Mp3.ToUTF8;
    // Header MP3
    SetLength(MP3Val, 0);
    SetLength(Files, 0);
    SlideMP3Index := -1;
    Songs.FindFilesByExtension(Path(includeTrailingPathDelimiter(CurrentSong.Path.ToNative)), Path('.mp3'), true, Files);
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
    Songs.FindFilesByExtension(Path(includeTrailingPathDelimiter(CurrentSong.Path.ToNative)), Path('.jpg'), true, Files);
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
    Songs.FindFilesByExtension(Path(includeTrailingPathDelimiter(CurrentSong.Path.ToNative)), Path('.jpg'), true, Files);
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

    // Header VideoGap
    SetLength(VideoGapVal, 1);
    VideoGapVal[0] := '';
    SlideVideoGapIndex := 1;
    UpdateSelectSlideOptions(Theme.EditSub.SlideVideoGap,VideoGapSlideId,VideoGapVal,SlideVideoGapIndex);
    SelectsS[VideoGapSlideId].TextOpt[0].Align := 0;
    SelectsS[VideoGapSlideId].TextOpt[0].X := SelectsS[VideoGapSlideId].TextureSBG.X + 5;

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
    UpdateSelectSlideOptions(Theme.EditSub.SelectVolAudio,VolumeAudioSlideId,VolumeAudio,VolumeAudioIndex);
    UpdateSelectSlideOptions(Theme.EditSub.SelectVolMidi,VolumeMidiSlideId,VolumeMidi,VolumeMidiIndex);
    UpdateSelectSlideOptions(Theme.EditSub.SelectVolClick,VolumeClickSlideId,VolumeClick,VolumeClickIndex);

    Lines[0].Current := 0;
    CurrentNote := 0;
    Lines[0].Line[0].Note[0].Color := 2;
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

    Lyric.Clear;
    Lyric.X := 400;
    Lyric.Y := 500;
    Lyric.Align := atCenter;
    Lyric.Size := 42;
    Lyric.ColR := 0;
    Lyric.ColG := 0;
    Lyric.ColB := 0;
    Lyric.ColSR := Skin_FontHighlightR;
    Lyric.ColSG := Skin_FontHighlightG;
    Lyric.ColSB := Skin_FontHighlightB;
    Lyric.AddLine(0);
    Lyric.Selected := 0;

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
    //BgFile := CurrentSong.Path.Append(CurrentSong.Background);
    Statics[BackgroundImageId].Texture.X := 500;
    Statics[BackgroundImageId].Texture.Y := 65;
    Statics[BackgroundImageId].Texture.W := 80;
    Statics[BackgroundImageId].Texture.H := 0;

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

  editLenghtText := 0;
  TextPosition := -1;
end;

function TScreenEditSub.Draw: boolean;
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
      MidiOut.PutShort(MIDI_NOTEOFF or 1, Lines[0].Line[Lines[0].Current].Note[MidiLastNote].Tone + 60, 127);
      PlaySentenceMidi := false;
    end;

    // click
    AktBeat := Floor(GetMidBeat(MidiPos - CurrentSong.GAP / 1000));
    Text[TextDebug].Text := IntToStr(AktBeat);

    if AktBeat <> LastClick then
    begin
      for i := 0 to Lines[0].Line[Lines[0].Current].HighNote do
        if (Lines[0].Line[Lines[0].Current].Note[i].Start = AktBeat) then
        begin

          LastClick := AktBeat;
          MidiOut.PutShort($B1, $7, floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
          if i > 0 then
            MidiOut.PutShort(MIDI_NOTEOFF or 1, Lines[0].Line[Lines[0].Current].Note[i-1].Tone + 60, 127);
          MidiOut.PutShort($91, Lines[0].Line[Lines[0].Current].Note[i].Tone + 60, 127);
          MidiLastNote := i;

        end;
    end;
  end; // if PlaySentenceMidi
  {$ENDIF}

  // move "cursor"
  if (PlaySentence or PlaySentenceMidi or PlayVideo) and not (PlayOne) then //and Not (PlayNote) then
  begin
    if (PlaySentence or PlayVideo) then
      AktBeat := Floor(GetMidBeat(AudioPlayback.Position - (CurrentSong.GAP) / 1000));

    lastline := Lines[0].Current;
    repeat              //find current line
    if Lines[0].Line[Lines[0].Current].End_ < AktBeat then
      inc(Lines[0].Current);
    until ((Length(Lines[0].Line) = Lines[0].Current) or (Lines[0].Line[Lines[0].Current].End_ >= AktBeat));
    if Lines[0].Current <> lastline then
    begin
        Lines[0].Line[lastline].Note[CurrentNote].Color := 1;
        Lyric.AddLine(Lines[0].Current);
        Lyric.Selected := 0;
        CurrentNote := 0;
        ShowInteractiveBackground;
        GoldenRec.KillAll;
    end;

    for note := CurrentNote to Length(Lines[0].Line[Lines[0].Current].note) - 1 do
      begin
        //note change
        if Lines[0].Line[Lines[0].Current].Note[note].Start < AktBeat then
            begin
              Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
              CurrentNote := note;
              Lyric.Selected := CurrentNote;
              Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 2;
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
      //AktBeat := Floor(CurrentSong.BPM[0].BPM * (Music.Position - CurrentSong.GAP / 1000) / 60);
      AktBeat := Floor(GetMidBeat(AudioPlayback.Position - CurrentSong.GAP / 1000));
      Text[TextDebug].Text := IntToStr(AktBeat);
      if AktBeat <> LastClick then
      begin
        for i := 0 to Lines[0].Line[Lines[0].Current].HighNote do
          if (Lines[0].Line[Lines[0].Current].Note[i].Start = AktBeat) then
          begin
            SoundLib.Click.Volume := SelectsS[VolumeClickSlideId].SelectedOption / 100;
            AudioPlayback.PlaySound( SoundLib.Click );
            LastClick := AktBeat;
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
      MidiOut.PutShort($81, Lines[0].Line[Lines[0].Current].Note[currentnote].Tone + 60, 127);
      MidiOut.PutShort(MIDI_STOP, 0, 0);
      PlayOneMidi := false;
    end;

    // click
    AktBeat := Floor(GetMidBeat(MidiPos - CurrentSong.GAP / 1000));
    Text[TextDebug].Text := IntToStr(AktBeat);

    if ((AktBeat <> LastClick) and Not (midinotefound)) then
    begin
//      for i := 0 to Lines[0].Line[Lines[0].Current].HighNote do
//      begin
        if ((Lines[0].Line[Lines[0].Current].Note[currentnote].Start <= AktBeat) and
        ((Lines[0].Line[Lines[0].Current].Note[currentnote].Start + Lines[0].Line[Lines[0].Current].Note[currentnote].Length) > AktBeat)) then
        begin
          LastClick := AktBeat;
          midinotefound := true;
          MidiOut.PutShort($B1, $7, floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
//          if i > 0 then
            MidiOut.PutShort($81, Lines[0].Line[Lines[0].Current].Note[currentnote-1].Tone + 60, 127);
          MidiOut.PutShort($91, Lines[0].Line[Lines[0].Current].Note[currentnote].Tone + 60, 127);

          MidiLastNote := i;
        end;
//      end;
    end;
  end; // if PlayOneNoteMidi
  {$ENDIF}

  Button[TextSentence].Text[0].Text := Language.Translate('INFO_CURRENT_LINE') + ' ' + IntToStr(Lines[0].Current + 1) + ' / ' + IntToStr(Lines[0].Number);
  Button[TextNote].Text[0].Text :=  Language.Translate('INFO_CURRENT_NOTE') + ' ' + IntToStr(CurrentNote + 1) + ' / ' + IntToStr(Lines[0].Line[Lines[0].Current].HighNote + 1);

  // Song info
  //Text[TextBPM].Text := FloatToStr(CurrentSong.BPM[0].BPM / 4);
  BPMVal[0] := FloatToStr(CurrentSong.BPM[0].BPM / 4);
  SelectsS[BPMSlideId].TextOpt[0].Text := BPMVal[0];
  //Text[TextGAP].Text := FloatToStr(CurrentSong.GAP);
  GAPVal[0] := FloatToStr(CurrentSong.GAP);
  SelectsS[GAPSlideId].TextOpt[0].Text := GAPVal[0];

  //Error reading Variables when no Song is loaded
  if not (Error or TitleEditMode or TextEditMode) then
  begin
    // Note info
    //Text[TextNStart].Text :=    IntToStr(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start);
    StartVal[0] := IntToStr(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start);
    SelectsS[StartSlideId].TextOpt[0].Text := StartVal[0];
    //Text[TextNLength].Text :=  IntToStr(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length);
    DurationVal[0] := IntToStr(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length);
    SelectsS[DurationSlideId].TextOpt[0].Text := DurationVal[0];
    //Text[TextNTon].Text :=      IntToStr(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Tone) + ' ( ' + GetNoteName(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Tone) + ' )';
    ToneVal[0] := IntToStr(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Tone) + ' ( ' + GetNoteName(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Tone) + ' )';
    SelectsS[ToneSlideId].TextOpt[0].Text := ToneVal[0];
    //Text[TextNText].Text :=              Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text;
    LyricVal[0] := Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text;
    SelectsS[LyricSlideId].TextOpt[0].Text := LyricVal[0];
    VideoGapVal[0] := floattostr(CurrentSong.VideoGAP);
    SelectsS[VideoGapSlideId].TextOpt[0].Text := VideoGapVal[0];
  end;

  // Text Edit Mode
  if TextEditMode or TitleEditMode or ArtistEditMode then
  begin
    if TextPosition >= 0 then
    SelectsS[CurrentSlideId].TextOpt[0].Text :=
      UTF8Copy(CurrentEditText, 1, TextPosition) + '|' + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
    editLenghtText := LengthUTF8(SelectsS[CurrentSlideId].TextOpt[0].Text);
  end;

  // draw static menu
  DrawBG;
  DrawStatics_UP(20, 55, 760, 236);
  DrawStatics_Sentences(20, 500, 760, 40);
  DrawInfoBar(20, 460, 760, 15, 0);
//  DrawInfoBar(20, 480, 760, 15, 1);  //for duet mode

  //inherited Draw;
  DrawFG;
  // draw notes
  //Error Drawing when no Song is loaded
  if not Error then
  begin
    if Xmouse < 0 then
    begin
      // notes table
      DrawStatics_Notes(20, 305, 760+Xmouse, 135);
      // horizontal lines
      SingDrawNoteLines(20, 305, 780+Xmouse, 15);
      // vertical lines
      SingDrawBeatDelimeters(40, 305, 760+Xmouse, 0);
      // draw notes
      EditDrawLine(40, 410, 760+Xmouse, 0, 15);
      // draw text on notes
      DrawText(40, 410, 760+Xmouse, 0, 15);
    end
    else
    begin
      // notes table
      DrawStatics_Notes(20+Xmouse, 305, 760-Xmouse, 135);
      // horizontal lines
      SingDrawNoteLines(20+Xmouse, 305, 780, 15);
      // vertical lines
      SingDrawBeatDelimeters(40+Xmouse, 305, 760, 0);
      // draw notes
      EditDrawLine(40+Xmouse, 410, 760, 0, 15);
      // draw text on notes
      DrawText(40+Xmouse, 410, 760, 0, 15);
    end;

    if Xmouse <> 0 then
       GoldenRec.KillAll;
  end;

  CurrentSound := AudioInputProcessor.Sound[0];
  CurrentSound.AnalyzeBuffer;
  if (CurrentSound.ToneString <> '-') then
  begin
    Count := trunc((720 / (GetTimeFromBeat(Lines[0].Line[Lines[0].Current].End_) - GetTimeFromBeat(Lines[0].Line[Lines[0].Current].Note[0].Start)))*(AudioPlayback.Position - GetTimeFromBeat(Lines[0].Line[Lines[0].Current].Note[0].Start)));
    DrawPlayerTrack(0, 16, 32, 15, CurrentSound.Tone, Count,CurrentNote);
  end;

  GoldenRec.SpawnRec;
  // draw text
  Lyric.Draw;
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
  Lyric.Free;
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
      Lines[0].Line[MedleyNotes.end_.line].Note[MedleyNotes.end_.note].Start +
      Lines[0].Line[MedleyNotes.end_.line].Note[MedleyNotes.end_.note].Length) -
      GetTimeFromBeat(Lines[0].Line[MedleyNotes.start.line].Note[MedleyNotes.start.note].Start);
  end else
    Result := 0;
end;

procedure TScreenEditSub.UpdateMedleyInfo;
begin
  if not MedleyNotes.IsStart and not MedleyNotes.IsEnd then
    Text[TextDebug].Text := ''
  else if not MedleyNotes.IsStart then
    Text[TextDebug].Text := Format('No Medley start beat.%s', [ifthen(MedleyNotes.IsEnd, Format(' End beat is at %d', [Lines[0].Line[MedleyNotes.end_.line].Note[MedleyNotes.end_.note].Start]))])
  else if not MedleyNotes.IsEnd then
    Text[TextDebug].Text := Format('No Medley end beat.%s', [ifthen(MedleyNotes.IsStart, Format(' Start beat is at %d', [Lines[0].Line[MedleyNotes.start.line].Note[MedleyNotes.start.note].Start]))])
  else
    Text[TextDebug].Text := Format('MedleyLength: %0.2fs %s', [GetMedleyLength, ifthen(MedleyNotes.isCustom, '(custom)', '(txt)')]);

end;

end.
