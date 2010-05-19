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

unit UScreenEditSub;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
{$I switches.inc}

uses
  UMenu,
  UMusic,
  SDL,
  SysUtils,
  UFiles,
  UTime,
  USongs,
  USong,
  UIni,
  ULog,
  UTexture,
  UMenuText,
  UEditorLyrics,
  Math,
  gl,
  {$IFDEF UseMIDIPort}
  MidiOut,
  {$ENDIF}
  UThemes;

type
  TScreenEditSub = class(TMenu)
    private
      AktBeat:          integer;
      //Variable is True if no Song is loaded
      Error:            boolean;
      
      TextNote:         integer;
      TextSentence:     integer;
      TextTitle:        integer;
      TextArtist:       integer;
      TextMp3:          integer;
      TextBPM:          integer;
      TextGAP:          integer;
      TextDebug:        integer;
      TextNStart:       integer;
      TextNLength:      integer;
      TextNTon:         integer;
      TextNText:        integer;
      CurrentNote:      integer;
      PlaySentence:     boolean;
      PlaySentenceMidi: boolean;
      PlayStopTime:     real;
      LastClick:        integer;
      Click:            boolean;
      CopySrc:          integer;

      {$IFDEF UseMIDIPort}
      MidiOut:          TMidiOutput;
      {$endif}

      MidiStart:        real;
      MidiStop:         real;
      MidiTime:         real;
      MidiPos:          real;
      MidiLastNote:     integer;

      TextEditMode:     boolean;
      editText:         UTF8String; //backup of current text in text-edit-mode

      Lyric:            TEditorLyrics;

      procedure DivideBPM;
      procedure MultiplyBPM;
      procedure LyricsCapitalize;
      procedure LyricsCorrectSpaces;
      procedure FixTimings;
      procedure DivideSentence;
      procedure JoinSentence;
      procedure DivideNote;
      procedure DeleteNote;
      procedure TransposeNote(Transpose: integer);
      procedure ChangeWholeTone(Tone: integer);
      procedure MoveAllToEnd(Move: integer);
      procedure MoveTextToRight;
      procedure MarkSrc;
      procedure PasteText;
      procedure CopySentence(Src, Dst: integer);
      procedure CopySentences(Src, Dst, Num: integer);
      procedure DrawStatics;
      procedure DrawInfoBar(x, y, w, h: integer);
      //Note Name Mod
      function GetNoteName(Note: integer): string;
    public
      Tex_Background:     TTexture;
      FadeOut:            boolean;
      constructor Create; override;
      procedure OnShow; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      function ParseInputEditText(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
      function Draw: boolean; override;
      procedure OnHide; override;
  end;

implementation

uses
  UGraphic,
  UDraw,
  UNote,
  USkins,
  ULanguage,
  UTextEncoding,
  UUnicodeUtils,
  UPath;


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

// Method for input parsing. If false is returned, GetNextWindow
// should be checked to know the next window to load;
function TScreenEditSub.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  SDL_ModState:  word;
  R:    real;
  SResult: TSaveSongResult;
begin
  Result := true;

  if TextEditMode then
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
      SDLK_S:
        begin
          // Save Song
          SResult := SaveSong(CurrentSong, Lines[0], CurrentSong.Path.Append(CurrentSong.FileName),
                   (SDL_ModState = KMOD_LSHIFT));
          if (SResult = ssrOK) then
          begin
            //ScreenPopupInfo.ShowPopup(Language.Translate('INFO_FILE_SAVED'));
            Text[TextDebug].Text := Language.Translate('INFO_FILE_SAVED');
          end
          else if (SResult = ssrEncodingError) then
          begin
            ScreenPopupCheck.ShowPopup(Language.Translate('ENCODING_ERROR_ASK_FOR_UTF8'), OnSaveEncodingError,
                Pointer(SDL_ModState = KMOD_LSHIFT), true);
          end
          else
          begin
            ScreenPopupError.ShowPopup(Language.Translate('ERROR_SAVE_FILE_FAILED'));
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
          Lyric.Free;

          onShow;
          Text[TextDebug].Text := 'song reloaded'; //TODO: Language.Translate('SONG_RELOADED'); 
        end;

      SDLK_D:
        begin
          // Divide lengths by 2
          if (SDL_ModState = KMOD_LSHIFT) then
          begin
            DivideBPM;
            Exit;
          end;
        end;
      SDLK_M:
        begin
          // Multiply lengths by 2
          if (SDL_ModState = KMOD_LSHIFT) then
          begin
            MultiplyBPM;
            Exit;
          end;
        end;
      SDLK_C:
        begin
          // Capitalize letter at the beginning of line
          if SDL_ModState = 0 then
            LyricsCapitalize;

          // Correct spaces
          if SDL_ModState = KMOD_LSHIFT then
            LyricsCorrectSpaces;

          // Copy sentence
          if SDL_ModState = KMOD_LCTRL then
            MarkSrc;

          Exit;
        end;
      SDLK_V:
        begin
          // Paste text
          if SDL_ModState = KMOD_LCTRL then
          begin
            if Lines[0].Line[Lines[0].Current].HighNote >= Lines[0].Line[CopySrc].HighNote then
              PasteText
            else
              Log.LogStatus('PasteText: invalid range', 'TScreenEditSub.ParseInput');
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then
          begin
            CopySentence(CopySrc, Lines[0].Current);
          end;
        end;
      SDLK_T:
        begin
          // Fixes timings between sentences
          FixTimings;
          Exit;
        end;
      SDLK_P:
        begin
          if SDL_ModState = 0 then
          begin
            // Play Sentence
            Click := true;
            AudioPlayback.Stop;
            R := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].Note[0].Start);
            if R <= AudioPlayback.Length then
            begin
              AudioPlayback.Position := R;
              PlayStopTime := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].End_);
              PlaySentence := true;
              AudioPlayback.Play;
              LastClick := -100;
            end;
          end
          else if SDL_ModState = KMOD_LSHIFT then
          begin
            PlaySentenceMidi := true;

            MidiTime := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].Note[0].Start);
            MidiStop := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].End_);

            LastClick := -100;
          end
          else if SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL then
          begin
            PlaySentenceMidi := true;
            MidiTime  := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].Note[0].Start);
            MidiStop  := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].End_);
            LastClick := -100;

            PlaySentence := true;
            Click := true;
            AudioPlayback.Stop;
            AudioPlayback.Position := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].Note[0].Start)+0{-0.10};
            PlayStopTime := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].End_)+0;
            AudioPlayback.Play;
            LastClick := -100;
          end;
          Exit;
        end;
      
      // Golden Note
      SDLK_G:
        begin
          if (Lines[0].Line[Lines[0].Current].Note[CurrentNote].NoteType = ntGolden) then
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].NoteType := ntNormal
          else
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].NoteType := ntGolden;
          
          Exit;
        end;
      
      // Freestyle Note
      SDLK_F:
        begin
          if (Lines[0].Line[Lines[0].Current].Note[CurrentNote].NoteType = ntFreestyle) then
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].NoteType := ntNormal
          else
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].NoteType := ntFreestyle;
          
          Exit;
        end;
    end;
    
    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          FadeTo(@ScreenSong);
        end;

      SDLK_BACKQUOTE:
        begin
          // Increase Note Length (same as Alt + Right)
          Inc(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length);
          if CurrentNote = Lines[0].Line[Lines[0].Current].HighNote then
            Inc(Lines[0].Line[Lines[0].Current].End_);
        end;

      SDLK_EQUALS:
        begin
          // Increase BPM
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
            CopySentence(CopySrc, Lines[0].Current);
            CopySentence(CopySrc+1, Lines[0].Current+1);
            CopySentence(CopySrc+2, Lines[0].Current+2);
            CopySentence(CopySrc+3, Lines[0].Current+3);
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then
          begin
            CopySentences(CopySrc, Lines[0].Current, 4);
          end;
        end;
      SDLK_5:
        begin
          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then
          begin
            CopySentence(CopySrc, Lines[0].Current);
            CopySentence(CopySrc+1, Lines[0].Current+1);
            CopySentence(CopySrc+2, Lines[0].Current+2);
            CopySentence(CopySrc+3, Lines[0].Current+3);
            CopySentence(CopySrc+4, Lines[0].Current+4);
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then
          begin
            CopySentences(CopySrc, Lines[0].Current, 5);
          end;
        end;

      SDLK_9:
        begin
          // Decrease GAP
          if SDL_ModState = 0 then
            CurrentSong.GAP := CurrentSong.GAP - 10;
          if SDL_ModState = KMOD_LSHIFT then
            CurrentSong.GAP := CurrentSong.GAP - 1000;
        end;
      SDLK_0:
        begin
          // Increase GAP
          if SDL_ModState = 0 then
            CurrentSong.GAP := CurrentSong.GAP + 10;
          if SDL_ModState = KMOD_LSHIFT then
            CurrentSong.GAP := CurrentSong.GAP + 1000;
        end;

      SDLK_KP_PLUS:
        begin
          // Increase tone of all notes
          if SDL_ModState = 0 then
            ChangeWholeTone(1);
          if SDL_ModState = KMOD_LSHIFT then
            ChangeWholeTone(12);
        end;

      SDLK_KP_MINUS:
        begin
          // Decrease tone of all notes
          if SDL_ModState = 0 then
            ChangeWholeTone(-1);
          if SDL_ModState = KMOD_LSHIFT then
            ChangeWholeTone(-12);
        end;

      SDLK_SLASH:
        begin
          if SDL_ModState = 0 then
          begin
            // Insert start of sentece
            if CurrentNote > 0 then
              DivideSentence;
          end;

          if SDL_ModState = KMOD_LSHIFT then
          begin
            // Join next sentence with current
            if Lines[0].Current < Lines[0].High then
              JoinSentence;
          end;

          if SDL_ModState = KMOD_LCTRL then
          begin
            // divide note
            DivideNote;
          end;

        end;

      SDLK_F4:
        begin
          // Enter Text Edit Mode
          editText := Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text;
          TextEditMode := true;
        end;

      SDLK_SPACE:
        begin
          if (SDL_ModState = 0) or (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) then
          begin
            // Play Sentence
            PlaySentenceMidi := false; // stop midi
            PlaySentence := true;
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
            PlaySentenceMidi := true;

            MidiTime := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start);
            MidiStop := GetTimeFromBeat(
              Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start +
              Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length);

            LastClick := -100;
          end;
        end;

      SDLK_RETURN:
        begin
        end;

      SDLK_DELETE:
        begin
          if SDL_ModState = KMOD_LCTRL then
          begin
            // moves text to right in current sentence
            DeleteNote;
          end;
        end;

      SDLK_PERIOD:
        begin
          // moves text to right in current sentence
          MoveTextToRight;
        end;

      SDLK_RIGHT:
        begin
          // right
          if SDL_ModState = 0 then
          begin
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
          end;

          // alt + right
          if SDL_ModState = KMOD_LALT then
          begin
            Inc(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length);
            if CurrentNote = Lines[0].Line[Lines[0].Current].HighNote then
              Inc(Lines[0].Line[Lines[0].Current].End_);
          end;

          // alt + ctrl + shift + right = move all from cursor to right
          if SDL_ModState = KMOD_LALT + KMOD_LCTRL + KMOD_LSHIFT then
          begin
            MoveAllToEnd(1);
          end;

        end;

      SDLK_LEFT:
        begin
          // left
          if SDL_ModState = 0 then
          begin
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
          end;

          // alt + ctrl + shift + right = move all from cursor to left
          if SDL_ModState = KMOD_LALT + KMOD_LCTRL + KMOD_LSHIFT then
          begin
            MoveAllToEnd(-1);
          end;

        end;

      SDLK_DOWN:
        begin

          // skip to next sentence
          if SDL_ModState = 0 then
          begin
            {$IFDEF UseMIDIPort}
            MidiOut.PutShort($81, Lines[0].Line[Lines[0].Current].Note[MidiLastNote].Tone + 60, 127);
            PlaySentenceMidi := false;
            {$ENDIF}

            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
            Inc(Lines[0].Current);
            CurrentNote := 0;
            if Lines[0].Current > Lines[0].High then
              Lines[0].Current := 0;
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 2;

            Lyric.AddLine(Lines[0].Current);
            Lyric.Selected := 0;
            AudioPlayback.Stop;
            PlaySentence := false;
          end;

          // decrease tone
          if SDL_ModState = KMOD_LCTRL then
          begin
            TransposeNote(-1);
          end;

        end;

      SDLK_UP:
        begin

          // skip to previous sentence
          if SDL_ModState = 0 then
          begin
            {$IFDEF UseMIDIPort}
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
            AudioPlayback.Stop;
            PlaySentence := false;
          end;

          // increase tone
          if SDL_ModState = KMOD_LCTRL then
          begin
            TransposeNote(1);
          end;
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
      Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text :=
        Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text + UCS4ToUTF8String(CharCode);

      Lyric.AddLine(Lines[0].Current);
      Lyric.Selected := CurrentNote;
      Exit;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE:
        begin
          Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text := editText;
          Lyric.AddLine(Lines[0].Current);
          Lyric.Selected := CurrentNote;
          TextEditMode := false;
        end;
      SDLK_F4, SDLK_RETURN:
        begin
          // Exit Text Edit Mode
          TextEditMode := false;
        end;
      SDLK_BACKSPACE:
        begin
          UTF8Delete(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text,
            LengthUTF8(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text), 1);
          Lyric.AddLine(Lines[0].Current);
          Lyric.Selected := CurrentNote;
        end;
      SDLK_RIGHT:
        begin
          // right
          if SDL_ModState = 0 then
          begin
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
            Inc(CurrentNote);
            if CurrentNote > Lines[0].Line[Lines[0].Current].HighNote then
              CurrentNote := 0;
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 2;
            Lyric.Selected := CurrentNote;
            editText := Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text;
          end;
        end;
      SDLK_LEFT:
        begin
          // left
          if SDL_ModState = 0 then
          begin
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
            Dec(CurrentNote);
            if CurrentNote = -1 then
              CurrentNote := Lines[0].Line[Lines[0].Current].HighNote;
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 2;
            Lyric.Selected := CurrentNote;
            editText := Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text;
          end;
      end;
    end;
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
  C:    integer;
  N:    integer;

begin
  CurrentSong.BPM[0].BPM := CurrentSong.BPM[0].BPM / 2;

  for C := 0 to Lines[0].High do
  begin
    Lines[0].Line[C].Start := Lines[0].Line[C].Start div 2;
    Lines[0].Line[C].End_  := Lines[0].Line[C].End_ div 2;
    for N := 0 to Lines[0].Line[C].HighNote do
    begin
      Lines[0].Line[C].Note[N].Start  := Lines[0].Line[C].Note[N].Start div 2;
      Lines[0].Line[C].Note[N].Length := Round(Lines[0].Line[C].Note[N].Length / 2);
    end; // N
  end; // C
end;

procedure TScreenEditSub.MultiplyBPM;
var
  C:    integer;
  N:    integer;
begin
  CurrentSong.BPM[0].BPM := CurrentSong.BPM[0].BPM * 2;
  for C := 0 to Lines[0].High do
  begin
    Lines[0].Line[C].Start := Lines[0].Line[C].Start * 2;
    Lines[0].Line[C].End_  := Lines[0].Line[C].End_ * 2;
    for N := 0 to Lines[0].Line[C].HighNote do
    begin
      Lines[0].Line[C].Note[N].Start  := Lines[0].Line[C].Note[N].Start * 2;
      Lines[0].Line[C].Note[N].Length := Lines[0].Line[C].Note[N].Length * 2;
    end; // N
  end; // C
end;

procedure TScreenEditSub.LyricsCapitalize;
var
  C:    integer;
  //N:    integer; // temporary
  S:    string;
begin
  // temporary
  {
  for C := 0 to Lines[0].High do
    for N := 0 to Lines[0].Line[C].HighNut do
      Lines[0].Line[C].Note[N].Text := UTF8LowerCase(Lines[0].Line[C].Note[N].Text);
  }

  for C := 0 to Lines[0].High do
  begin
    S := AnsiUpperCase(Copy(Lines[0].Line[C].Note[0].Text, 1, 1));
    S := S + Copy(Lines[0].Line[C].Note[0].Text, 2, Length(Lines[0].Line[C].Note[0].Text)-1);
    Lines[0].Line[C].Note[0].Text := S;
  end; // C
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

procedure TScreenEditSub.DivideNote;
var
  C:    integer;
  N:    integer;
begin
  C := Lines[0].Current;

  with Lines[0].Line[C] do
  begin
    Inc(HighNote);
    SetLength(Note, HighNote + 1);

    // we copy all notes including selected one
    for N := HighNote downto CurrentNote+1 do
    begin
      Note[N] := Note[N-1];
    end;

    // me slightly modify new note
    Note[CurrentNote].Length := 1;
    Inc(Note[CurrentNote+1].Start);
    Dec(Note[CurrentNote+1].Length);
    Note[CurrentNote+1].Text := '- ';
    Note[CurrentNote+1].Color := 1;
  end;
end;

procedure TScreenEditSub.DeleteNote;
var
  C:    integer;
  N:    integer;
begin
  C := Lines[0].Current;

  //Do Not delete Last Note
  if (Lines[0].High > 0) or (Lines[0].Line[C].HighNote > 0) then
  begin

    // we copy all notes from the next to the selected one
    for N := CurrentNote+1 to Lines[0].Line[C].HighNote do
    begin
      Lines[0].Line[C].Note[N-1] := Lines[0].Line[C].Note[N];
    end;
    
    Dec(Lines[0].Line[C].HighNote);
    if (Lines[0].Line[C].HighNote >= 0) then
    begin
      SetLength(Lines[0].Line[C].Note, Lines[0].Line[C].HighNote + 1);

      // me slightly modify new note
      if CurrentNote > Lines[0].Line[C].HighNote then
        Dec(CurrentNote);
      
      Lines[0].Line[C].Note[CurrentNote].Color := 2;
    end
    //Last Note of current Sentence Deleted - > Delete Sentence
    else
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
  end;
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

procedure TScreenEditSub.DrawStatics;
var
  x, y, w, h: Integer;
begin
  //Theme:
  //bg
  glDisable(GL_BLEND);

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
  glEnd;

  // Line
  glColor4f(0.9, 0.9, 0.9, 1);
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
  glEnd;

  // some borders
  x := 20;
  y := 55;
  w := 760;
  h := 236;
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

  x := 20;
  y := 305;
  w := 760;
  h := 135;
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

  x := 20;
  y := 500;
  w := 760;
  h := 40;
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

procedure TScreenEditSub.DrawInfoBar(x, y, w, h: integer);
var
  start, end_:        integer;
  ww:                 integer;

  pos:                real;
  br:                 real;

  line, note:         integer;
  numLines, numNotes: integer;

begin
  numLines := Length(Lines[0].Line);

  if(numLines=0) then
    Exit;

  start := Lines[0].Line[0].Start;
  end_ := Lines[0].Line[numLines-1].End_;
  ww := end_ - start;

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


  for line := 0 to numLines - 1 do
  begin
    if (line = Lines[0].Current) and not (PlaySentence or PlaySentenceMidi) then
      glColor4f(0.4, 0.4, 0, 1)
    else
      glColor4f(1, 0.6, 0, 1);


    start := Lines[0].Line[line].Note[0].Start;
    end_ := Lines[0].Line[line].Note[Lines[0].Line[line].HighNote].Start+
      Lines[0].Line[line].Note[Lines[0].Line[line].HighNote].Length;

    pos := start/ww*w;
    br := (end_-start)/ww*w;

    glbegin(gl_quads);
      glVertex2f(x+pos, y);
      glVertex2f(x+pos, y+h);
      glVertex2f(x+pos+br, y+h);
      glVertex2f(x+pos+br, y);
    glEnd;
    {
    numNotes := Length(Lines[0].Line[line].Nuta);

    for note := 0 to numNotes - 1 do
    begin

    end;  }
  end;

  if(PlaySentence or PlaySentenceMidi) then
  begin
    glColor4f(0, 0, 0, 0.5);
    pos := 0;
    br := AktBeat/ww*w;
    if (br>w) then
      br := w;
  end else
  begin
    glColor4f(1, 0, 0, 1);
    pos := Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start/ww*w;
    br := Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length/ww*w;
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

constructor TScreenEditSub.Create;
begin
  inherited Create;
  SetLength(Player, 1);

  // line
  AddText(40, 11, 1, 30, 0, 0, 0, 'Line:');
  TextSentence := AddText(110, 11, 1, 30, 0, 0, 0, '0 / 0');


  // Note
  AddText(282, 11, 1, 30, 0, 0, 0, 'Note:');
  TextNote := AddText(360, 11, 1, 30, 0, 0, 0, '0 / 0');

  // file info
  AddText(30, 65,  0, 24, 0, 0, 0, 'Title:');
  AddText(30, 90,  0, 24, 0, 0, 0, 'Artist:');
  AddText(30, 115, 0, 24, 0, 0, 0, 'Mp3:');
  AddText(30, 140, 0, 24, 0, 0, 0, 'BPM:');
  AddText(30, 165, 0, 24, 0, 0, 0, 'GAP:');

  TextTitle :=  AddText(180, 65,  0, 24, 0, 0, 0, 'a');
  TextArtist := AddText(180, 90,  0, 24, 0, 0, 0, 'b');
  TextMp3 :=    AddText(180, 115, 0, 24, 0, 0, 0, 'c');
  TextBPM :=    AddText(180, 140, 0, 24, 0, 0, 0, 'd');
  TextGAP :=    AddText(180, 165, 0, 24, 0, 0, 0, 'e');

  // note info
  AddText(30, 190,  0, 24, 0, 0, 0, 'Start:');
  AddText(30, 215,  0, 24, 0, 0, 0, 'Duration:');
  AddText(30, 240,  0, 24, 0, 0, 0, 'Tone:');
  AddText(30, 265,  0, 24, 0, 0, 0, 'Text:');      //AddText(500, 265,  0, 8, 0, 0, 0, 'VideoGap:');

  TextNStart :=   AddText(180, 190,  0, 24, 0, 0, 0, 'a');
  TextNLength :=  AddText(180, 215,  0, 24, 0, 0, 0, 'b');
  TextNTon :=     AddText(180, 240,  0, 24, 0, 0, 0, 'c');
  TextNText :=    AddText(180, 265,  0, 24, 0, 0, 0, 'd');

  //TextVideoGap :=  AddText(600, 265,  0, 24, 0, 0, 0, 'e');

  // debug
  TextDebug :=  AddText(30, 550, 0, 27, 0, 0, 0, '');

end;

procedure TScreenEditSub.OnShow;
var
  FileExt: IPath;
begin
  inherited;

  AudioPlayback.Stop;
  PlaySentence := false;
  PlaySentenceMidi := false;

  Log.LogStatus('Initializing', 'TEditScreen.OnShow');
  Lyric := TEditorLyrics.Create;

  ResetSingTemp;

  try 
    //Check if File is XML
    FileExt := CurrentSong.FileName.GetExtension;
    if FileExt.ToUTF8 = '.xml' then
      Error := not CurrentSong.LoadXMLSong()
    else
    begin
      // reread header with custom tags
      Error := not CurrentSong.Analyse(true);
      if not Error then
        Error := not CurrentSong.LoadSong;
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
    Text[TextTitle].Text :=   CurrentSong.Title;
    Text[TextArtist].Text :=  CurrentSong.Artist;
    Text[TextMp3].Text :=     CurrentSong.Mp3.ToUTF8;

    Lines[0].Current := 0;
    CurrentNote := 0;
    Lines[0].Line[0].Note[0].Color := 2;
    AudioPlayback.Open(CurrentSong.Path.Append(CurrentSong.Mp3));
    //Set Down Music Volume for Better hearability of Midi Sounds
    //Music.SetVolume(0.4);

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

    NotesH := 7;
    NotesW := 4;

  end;

//  Interaction := 0;
  TextEditMode := false;
end;

function TScreenEditSub.Draw: boolean;
var
  Pet:    integer;
begin

  glClearColor(1,1,1,1);

  // midi music
  if PlaySentenceMidi then
  begin
   {$IFDEF UseMIDIPort}
    MidiPos := USTime.GetTime - MidiTime + MidiStart;

    // stop the music
    if (MidiPos > MidiStop) then
    begin
      MidiOut.PutShort($81, Lines[0].Line[Lines[0].Current].Note[MidiLastNote].Tone + 60, 127);
      PlaySentenceMidi := false;
    end;
  {$ENDIF}

    // click
    AktBeat := Floor(GetMidBeat(MidiPos - CurrentSong.GAP / 1000));
    Text[TextDebug].Text := IntToStr(AktBeat);

    if AktBeat <> LastClick then
    begin
      for Pet := 0 to Lines[0].Line[Lines[0].Current].HighNote do
        if (Lines[0].Line[Lines[0].Current].Note[Pet].Start = AktBeat) then
        begin

          LastClick := AktBeat;
          {$IFDEF UseMIDIPort}
          if Pet > 0 then
            MidiOut.PutShort($81, Lines[0].Line[Lines[0].Current].Note[Pet-1].Tone + 60, 127);
          MidiOut.PutShort($91, Lines[0].Line[Lines[0].Current].Note[Pet].Tone + 60, 127);
          MidiLastNote := Pet;
          {$ENDIF}

        end;
    end;
  end; // if PlaySentenceMidi

  // mp3 music
  if PlaySentence then
  begin
    // stop the music
    if (AudioPlayback.Position > PlayStopTime) then
    begin
      AudioPlayback.Stop;
      PlaySentence := false;
    end;

    // click
    if (Click) and (PlaySentence) then
    begin
//      AktBeat := Floor(CurrentSong.BPM[0].BPM * (Music.Position - CurrentSong.GAP / 1000) / 60);
      AktBeat := Floor(GetMidBeat(AudioPlayback.Position - CurrentSong.GAP / 1000));
      Text[TextDebug].Text := IntToStr(AktBeat);
      if AktBeat <> LastClick then
      begin
        for Pet := 0 to Lines[0].Line[Lines[0].Current].HighNote do
          if (Lines[0].Line[Lines[0].Current].Note[Pet].Start = AktBeat) then
          begin
            AudioPlayback.PlaySound( SoundLib.Click );
            LastClick := AktBeat;
          end;
      end;
    end; // click
  end; // if PlaySentence
  

  Text[TextSentence].Text := IntToStr(Lines[0].Current + 1) + ' / ' + IntToStr(Lines[0].Number);
  Text[TextNote].Text := IntToStr(CurrentNote + 1) + ' / ' + IntToStr(Lines[0].Line[Lines[0].Current].HighNote + 1);

  // Song info
  Text[TextBPM].Text := FloatToStr(CurrentSong.BPM[0].BPM / 4);
  Text[TextGAP].Text := FloatToStr(CurrentSong.GAP);

  //Error reading Variables when no Song is loaded
  if not Error then
  begin
    // Note info
    Text[TextNStart].Text :=    IntToStr(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start);
    Text[TextNLength].Text :=  IntToStr(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length);
    Text[TextNTon].Text :=      IntToStr(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Tone) + ' ( ' + GetNoteName(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Tone) + ' )';
    Text[TextNText].Text :=              Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text;
  end;

  // Text Edit Mode
  if TextEditMode then
    Text[TextNText].Text := Text[TextNText].Text + '|'; 

  // draw static menu
  DrawStatics;
  DrawInfoBar(20, 460, 760, 20);
  //inherited Draw;
  DrawFG;
  // draw notes
  SingDrawNoteLines(20, 305, 780, 15);
  //Error Drawing when no Song is loaded
  if not Error then
  begin
    SingDrawBeatDelimeters(40, 305, 760, 0);
    EditDrawLine(40, 410, 760, 0, 15);
  end;

  // draw text
  Lyric.Draw;

  Result := true;
end;

procedure TScreenEditSub.OnHide;
begin
  {$IFDEF UseMIDIPort}
  MidiOut.Close;
  MidiOut.Free;
  {$ENDIF}
  Lyric.Free;
  //Music.SetVolume(1.0);
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
    10: Result := 'b';
    11: Result := 'h';
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

end.