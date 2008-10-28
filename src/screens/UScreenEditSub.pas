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
      //Variable is True if no Song is loaded
      Error:            Boolean;
      
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
      MidiOut:         TMidiOutput;
      {$endif}

      MidiStart:       real;
      MidiStop:        real;
      MidiTime:        real;
      MidiPos:         real;
      MidiLastNote:    integer;

      TextEditMode:    boolean;

      Lyric:    TEditorLyrics;

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
      //Note Name Mod
      function GetNoteName(Note: Integer): String;
    public
      Tex_Background:     TTexture;
      FadeOut:            boolean;
      constructor Create; override;
      procedure onShow; override;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      function ParseInputEditText(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
      function Draw: boolean; override;
      procedure onHide; override;
  end;

implementation

uses
  UGraphic,
  UDraw,
  UMain,
  USkins,
  ULanguage;

// Method for input parsing. If False is returned, GetNextWindow
// should be checked to know the next window to load;
function TScreenEditSub.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
var
  SDL_ModState:  Word;
  R:    real;
begin
  Result := true;

  if TextEditMode then begin
    Result := ParseInputEditText(PressedKey, CharCode, PressedDown);
  end else begin

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT {+ KMOD_CAPS});

  If (PressedDown) then begin // Key Down
    // check normal keys
    case WideCharUpperCase(CharCode)[1] of
      'Q':
        begin
          Result := false;
          Exit;
        end;
      'S':
        begin
          // Save Song
          if SDL_ModState = KMOD_LSHIFT then
            SaveSong(CurrentSong, Lines[0], CurrentSong.Path + CurrentSong.FileName, true)
          else
            SaveSong(CurrentSong, Lines[0], CurrentSong.Path + CurrentSong.FileName, false);

          {if SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL + KMOD_LALT then
            // Save Song
            SaveSongDebug(CurrentSong, Lines[0], 'C:\song.asm', false);}

          Exit;
        end;
      'D':
        begin
          // Divide lengths by 2
          DivideBPM;
          Exit;
        end;
      'M':
        begin
          // Multiply lengths by 2
          MultiplyBPM;
          Exit;
        end;
      'C':
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
      'V':
        begin
          // Paste text
          if SDL_ModState = KMOD_LCTRL then begin
            if Lines[0].Line[Lines[0].Current].HighNote >= Lines[0].Line[CopySrc].HighNote then
              PasteText
            else
              Log.LogStatus('PasteText: invalid range', 'TScreenEditSub.ParseInput');
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then begin
            CopySentence(CopySrc, Lines[0].Current);
          end;
        end;
      'T':
        begin
          // Fixes timings between sentences
          FixTimings;
          Exit;
        end;
      'P':
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
      
      // Golden Note Patch
      'G':
        begin
          if (Lines[0].Line[Lines[0].Current].Note[CurrentNote].NoteType = ntGolden) then
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].NoteType := ntNormal
          else
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].NoteType := ntGolden;
          
          Exit;
        end;
      
      // Freestyle Note Patch
      'F':
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
          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then begin
            CopySentence(CopySrc, Lines[0].Current);
            CopySentence(CopySrc+1, Lines[0].Current+1);
            CopySentence(CopySrc+2, Lines[0].Current+2);
            CopySentence(CopySrc+3, Lines[0].Current+3);
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then begin
            CopySentences(CopySrc, Lines[0].Current, 4);
          end;
        end;
      SDLK_5:
        begin
          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then begin
            CopySentence(CopySrc, Lines[0].Current);
            CopySentence(CopySrc+1, Lines[0].Current+1);
            CopySentence(CopySrc+2, Lines[0].Current+2);
            CopySentence(CopySrc+3, Lines[0].Current+3);
            CopySentence(CopySrc+4, Lines[0].Current+4);
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then begin
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
          if SDL_ModState = 0 then begin
            // Insert start of sentece
            if CurrentNote > 0 then
              DivideSentence;
          end;

          if SDL_ModState = KMOD_LSHIFT then begin
            // Join next sentence with current
            if Lines[0].Current < Lines[0].High  then
              JoinSentence;
          end;

          if SDL_ModState = KMOD_LCTRL then begin
            // divide note
            DivideNote;
          end;

        end;

      SDLK_F4:
        begin
          // Enter Text Edit Mode
          TextEditMode := true;
        end;

      SDLK_SPACE:
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

      SDLK_RETURN:
        begin
        end;

      SDLK_LCTRL:
        begin
        end;

      SDLK_DELETE:
        begin
          if SDL_ModState = KMOD_LCTRL then begin
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
          if SDL_ModState = 0 then begin
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 0;
            Inc(CurrentNote);
            if CurrentNote > Lines[0].Line[Lines[0].Current].HighNote then CurrentNote := 0;
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
            Lyric.Selected := CurrentNote;
          end;

          // ctrl + right
          if SDL_ModState = KMOD_LCTRL then begin
            if Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length > 1 then begin
              Dec(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length);
              Inc(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start);
              if CurrentNote = 0 then begin
                Inc(Lines[0].Line[Lines[0].Current].Start);
              end;
            end;
          end;

          // shift + right
          if SDL_ModState = KMOD_LSHIFT then begin
            Inc(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start);
            if CurrentNote = 0 then begin
              Inc(Lines[0].Line[Lines[0].Current].Start);
            end;
            if CurrentNote = Lines[0].Line[Lines[0].Current].HighNote then
              Inc(Lines[0].Line[Lines[0].Current].End_);
          end;

          // alt + right
          if SDL_ModState = KMOD_LALT then begin
            Inc(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length);
            if CurrentNote = Lines[0].Line[Lines[0].Current].HighNote then
              Inc(Lines[0].Line[Lines[0].Current].End_);
          end;

          // alt + ctrl + shift + right = move all from cursor to right
          if SDL_ModState = KMOD_LALT + KMOD_LCTRL + KMOD_LSHIFT then begin
            MoveAllToEnd(1);
          end;

        end;

      SDLK_LEFT:
        begin
          // left
          if SDL_ModState = 0 then begin
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 0;
            Dec(CurrentNote);
            if CurrentNote = -1 then CurrentNote := Lines[0].Line[Lines[0].Current].HighNote;
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
            Lyric.Selected := CurrentNote;
          end;

          // ctrl + left
          if SDL_ModState = KMOD_LCTRL then begin
            Dec(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start);
            Inc(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length);
            if CurrentNote = 0 then begin
              Dec(Lines[0].Line[Lines[0].Current].Start);
            end;
          end;

          // shift + left
          if SDL_ModState = KMOD_LSHIFT then begin
            Dec(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Start);

            // resizing sentences
            if CurrentNote = 0 then begin
              Dec(Lines[0].Line[Lines[0].Current].Start);
            end;

            if CurrentNote = Lines[0].Line[Lines[0].Current].HighNote then
              Dec(Lines[0].Line[Lines[0].Current].End_);

          end;

          // alt + left
          if SDL_ModState = KMOD_LALT then begin
            if Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length > 1 then begin
              Dec(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Length);
              if CurrentNote = Lines[0].Line[Lines[0].Current].HighNote then
                Dec(Lines[0].Line[Lines[0].Current].End_);
            end;
          end;

          // alt + ctrl + shift + right = move all from cursor to left
          if SDL_ModState = KMOD_LALT + KMOD_LCTRL + KMOD_LSHIFT then begin
            MoveAllToEnd(-1);
          end;

        end;

      SDLK_DOWN:
        begin

          // skip to next sentence
          if SDL_ModState = 0 then begin                       {$IFDEF UseMIDIPort}
            MidiOut.PutShort($81, Lines[0].Line[Lines[0].Current].Note[MidiLastNote].Tone + 60, 127);
            PlaySentenceMidi := false;
            {$endif}

            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 0;
            Inc(Lines[0].Current);
            CurrentNote := 0;
            if Lines[0].Current > Lines[0].High then Lines[0].Current := 0;
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;

            Lyric.AddLine(Lines[0].Current);
            Lyric.Selected := 0;
            AudioPlayback.Stop;
            PlaySentence := false;
          end;

          // decrease tone
          if SDL_ModState = KMOD_LCTRL then begin
            TransposeNote(-1);
          end;

        end;

      SDLK_UP:
        begin

          // skip to previous sentence
          if SDL_ModState = 0 then begin
            {$IFDEF UseMIDIPort}
            MidiOut.PutShort($81, Lines[0].Line[Lines[0].Current].Note[MidiLastNote].Tone + 60, 127);
            PlaySentenceMidi := false;
            {$endif}

            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 0;
            Dec(Lines[0].Current);
            CurrentNote := 0;
            if Lines[0].Current = -1 then Lines[0].Current := Lines[0].High;
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;

            Lyric.AddLine(Lines[0].Current);
            Lyric.Selected := 0;
            AudioPlayback.Stop;
            PlaySentence := false;
          end;

          // increase tone
          if SDL_ModState = KMOD_LCTRL then begin
            TransposeNote(1);
          end;
        end;

      end; // case
    end;
  end; // if
end;

function TScreenEditSub.ParseInputEditText(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
var
  SDL_ModState:  Word;
begin
  // used when in Text Edit Mode
  Result := true;

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT {+ KMOD_CAPS});

  If (PressedDown) Then
  begin // Key Down
    case PressedKey of

      SDLK_ESCAPE:
        begin
          FadeTo(@ScreenSong);
        end;
      SDLK_F4, SDLK_RETURN:
        begin
          // Exit Text Edit Mode
          TextEditMode := false;
        end;
      SDLK_0..SDLK_9, SDLK_A..SDLK_Z, SDLK_SPACE, SDLK_MINUS, SDLK_EXCLAIM, SDLK_COMMA, SDLK_SLASH, SDLK_ASTERISK, SDLK_QUESTION, SDLK_QUOTE, SDLK_QUOTEDBL:
        begin
          Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text :=
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text + CharCode;
        end;
      SDLK_BACKSPACE:
        begin
          Delete(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text,
            Length(Lines[0].Line[Lines[0].Current].Note[CurrentNote].Text), 1);
        end;
      SDLK_RIGHT:
        begin
          // right
          if SDL_ModState = 0 then begin
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 0;
            Inc(CurrentNote);
            if CurrentNote > Lines[0].Line[Lines[0].Current].HighNote then CurrentNote := 0;
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
            Lyric.Selected := CurrentNote;
          end;
        end;
      SDLK_LEFT:
        begin
          // left
          if SDL_ModState = 0 then begin
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 0;
            Dec(CurrentNote);
            if CurrentNote = -1 then CurrentNote := Lines[0].Line[Lines[0].Current].HighNote;
            Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
            Lyric.Selected := CurrentNote;
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
  for C := 0 to Lines[0].High do begin
    Lines[0].Line[C].Start :=     Lines[0].Line[C].Start div 2;
    Lines[0].Line[C].End_ :=    Lines[0].Line[C].End_ div 2;
    for N := 0 to Lines[0].Line[C].HighNote do begin
      Lines[0].Line[C].Note[N].Start :=   Lines[0].Line[C].Note[N].Start div 2;
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
  for C := 0 to Lines[0].High do begin
    Lines[0].Line[C].Start :=     Lines[0].Line[C].Start * 2;
    Lines[0].Line[C].End_ :=    Lines[0].Line[C].End_ * 2;
    for N := 0 to Lines[0].Line[C].HighNote do begin
      Lines[0].Line[C].Note[N].Start :=   Lines[0].Line[C].Note[N].Start * 2;
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
{  for C := 0 to Lines[0].High do
    for N := 0 to Lines[0].Line[C].HighNut do
      Lines[0].Line[C].Note[N].Text := AnsiLowerCase(Lines[0].Line[C].Note[N].Text);}

  for C := 0 to Lines[0].High do begin
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
  for C := 0 to Lines[0].High do begin
    // correct starting spaces in the first word
    while Copy(Lines[0].Line[C].Note[0].Text, 1, 1) = ' ' do
      Lines[0].Line[C].Note[0].Text := Copy(Lines[0].Line[C].Note[0].Text, 2, 100);

    // move spaces on the start to the end of the previous note
    for N := 1 to Lines[0].Line[C].HighNote do begin
      while (Copy(Lines[0].Line[C].Note[N].Text, 1, 1) = ' ') do begin
        Lines[0].Line[C].Note[N].Text := Copy(Lines[0].Line[C].Note[N].Text, 2, 100);
        Lines[0].Line[C].Note[N-1].Text := Lines[0].Line[C].Note[N-1].Text + ' ';
      end;
    end; // N

    // correct '-'  to '- '
    for N := 0 to Lines[0].Line[C].HighNote do begin
      if Lines[0].Line[C].Note[N].Text = '-' then
        Lines[0].Line[C].Note[N].Text := '- ';
    end; // N

    // add space to the previous note when the current word is '- '
    for N := 1 to Lines[0].Line[C].HighNote do begin
      if Lines[0].Line[C].Note[N].Text  = '- ' then
        Lines[0].Line[C].Note[N-1].Text := Lines[0].Line[C].Note[N-1].Text + ' ';
    end; // N

    // correct too many spaces at the end of note
    for N := 0 to Lines[0].Line[C].HighNote do begin
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
  for C := 1 to Lines[0].High do begin
    with Lines[0].Line[C-1] do begin
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
  Lines[0].Line[CNew].LyricWidth := 0;
  Lines[0].Line[CNew].End_ := 0;
  Lines[0].Line[CNew].BaseNote := 0; // 0.5.0: we modify it later in this procedure
  Lines[0].Line[CNew].HighNote := -1;
  SetLength(Lines[0].Line[CNew].Note, 0);

  // move right notes to new sentences
  NHigh := Lines[0].Line[CStart].HighNote;
  for N := NStart to NHigh do begin
    // increase sentence counters
    with Lines[0].Line[CNew] do
    begin
      Inc(HighNote);
      SetLength(Note, HighNote + 1);
      Note[HighNote] := Note[N];
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

  Lines[0].Current := Lines[0].Current + 1;
  CurrentNote := 0;
  Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
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
  for N := 0 to Lines[0].Line[C+1].HighNote do begin
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
    for N := HighNote downto CurrentNote+1 do begin
      Note[N] := Note[N-1];
    end;

    // me slightly modify new note
    Note[CurrentNote].Length := 1;
    Inc(Note[CurrentNote+1].Start);
    Dec(Note[CurrentNote+1].Length);
    Note[CurrentNote+1].Text := '- ';
    Note[CurrentNote+1].Color := 0;
  end;
end;

procedure TScreenEditSub.DeleteNote;
var
  C:    integer;
  N:    integer;
begin
  C := Lines[0].Current;

  //Do Not delete Last Note
  if (Lines[0].High > 0) OR (Lines[0].Line[C].HighNote > 0) then
  begin

    // we copy all notes from the next to the selected one
    for N := CurrentNote+1 to Lines[0].Line[C].HighNote do begin
      Lines[0].Line[C].Note[N-1] := Lines[0].Line[C].Note[N];
    end;
    
    Dec(Lines[0].Line[C].HighNote);
    if (Lines[0].Line[C].HighNote >= 0) then
    begin
      SetLength(Lines[0].Line[C].Note, Lines[0].Line[C].HighNote + 1);

      // me slightly modify new note
      if CurrentNote > Lines[0].Line[C].HighNote then
        Dec(CurrentNote);
      
      Lines[0].Line[C].Note[CurrentNote].Color := 1;
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

      Lines[0].Line[Lines[0].Current].Note[CurrentNote].Color := 1;
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
  for C := 0 to Lines[0].High do begin
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
  for C := Lines[0].Current to Lines[0].High do begin
    NStart := 0;
    if C = Lines[0].Current then NStart := CurrentNote;
    for N := NStart to Lines[0].Line[C].HighNote do begin
      Inc(Lines[0].Line[C].Note[N].Start, Move); // move note start

      if N = 0 then begin // fix beginning
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
{  C := Lines[0].Current;

  for N := Lines[0].Line[C].HighNut downto 1 do begin
    Lines[0].Line[C].Note[N].Text := Lines[0].Line[C].Note[N-1].Text;
  end; // for

  Lines[0].Line[C].Note[0].Text := '- ';}

  C := Lines[0].Current;
  NHigh := Lines[0].Line[C].HighNote;

  // last word
  Lines[0].Line[C].Note[NHigh].Text := Lines[0].Line[C].Note[NHigh-1].Text + Lines[0].Line[C].Note[NHigh].Text;

  // other words
  for N := NHigh - 1 downto CurrentNote + 1 do begin
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
  N:      integer;
  Time1:  integer;
  Time2:  integer;
  TD:  integer;
begin
  Time1 := Lines[0].Line[Src].Note[0].Start;
  Time2 := Lines[0].Line[Dst].Note[0].Start;
  TD := Time2-Time1;

  SetLength(Lines[0].Line[Dst].Note, Lines[0].Line[Src].HighNote + 1);
  Lines[0].Line[Dst].HighNote := Lines[0].Line[Src].HighNote;
  for N := 0 to Lines[0].Line[Src].HighNote do begin
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
  for C := Lines[0].High downto Dst + 1 do begin
    Lines[0].Line[C + Num - 1] := Lines[0].Line[C];
  end;

  // prepares new sentences: sets sentence start and create first note
  for C := 1 to Num-1 do begin
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


constructor TScreenEditSub.Create;
begin
  inherited Create;
  SetLength(Player, 1);

  // line
  AddStatic(20, 10, 80, 30, 0, 0, 0, Skin.GetTextureFileName('ButtonF'), TEXTURE_TYPE_COLORIZED);
  AddText(40, 17, 1, 18, 1, 1, 1, 'Line');
  TextSentence := AddText(120, 14, 1, 24, 0, 0, 0, '0 / 0');

  // Note
  AddStatic(220, 10, 80, 30, 0, 0, 0, Skin.GetTextureFileName('ButtonF'), TEXTURE_TYPE_COLORIZED);
  AddText(242, 17, 1, 18, 1, 1, 1, 'Note');
  TextNote := AddText(320, 14, 1, 24, 0, 0, 0, '0 / 0');

  // file info
  AddStatic(150, 50, 500, 150, 0, 0, 0, Skin.GetTextureFileName('MainBar'), TEXTURE_TYPE_COLORIZED);
  AddStatic(151, 52, 498, 146,  1, 1, 1, Skin.GetTextureFileName('MainBar'), TEXTURE_TYPE_COLORIZED);
  AddText(180, 65,  0, 24, 0, 0, 0, 'Title:');
  AddText(180, 90,  0, 24, 0, 0, 0, 'Artist:');
  AddText(180, 115, 0, 24, 0, 0, 0, 'Mp3:');
  AddText(180, 140, 0, 24, 0, 0, 0, 'BPM:');
  AddText(180, 165, 0, 24, 0, 0, 0, 'GAP:');

  TextTitle :=  AddText(250, 65,  0, 24, 0, 0, 0, 'a');
  TextArtist := AddText(250, 90,  0, 24, 0, 0, 0, 'b');
  TextMp3 :=    AddText(250, 115, 0, 24, 0, 0, 0, 'c');
  TextBPM :=    AddText(250, 140, 0, 24, 0, 0, 0, 'd');
  TextGAP :=    AddText(250, 165, 0, 24, 0, 0, 0, 'e');

{  AddInteraction(2, TextTitle);
  AddInteraction(2, TextArtist);
  AddInteraction(2, TextMp3);
  AddInteraction(2, TextBPM);
  AddInteraction(2, TextGAP);}

  // note info
  AddText(20, 190,  0, 24, 0, 0, 0, 'Start:');
  AddText(20, 215,  0, 24, 0, 0, 0, 'Duration:');
  AddText(20, 240,  0, 24, 0, 0, 0, 'Tone:');
  AddText(20, 265,  0, 24, 0, 0, 0, 'Text:');

  TextNStart :=   AddText(120, 190,  0, 24, 0, 0, 0, 'a');
  TextNLength :=  AddText(120, 215,  0, 24, 0, 0, 0, 'b');
  TextNTon :=     AddText(120, 240,  0, 24, 0, 0, 0, 'c');
  TextNText :=    AddText(120, 265,  0, 24, 0, 0, 0, 'd');

  // debug
  TextDebug :=  AddText(30, 550, 0, 8, 0, 0, 0, '');

end;

procedure TScreenEditSub.onShow;
begin
  inherited;

  Log.LogStatus('Initializing', 'TEditScreen.onShow');
  Lyric := TEditorLyrics.Create;

  ResetSingTemp;

  try 
  //Check if File is XML
   if copy(CurrentSong.FileName,length(CurrentSong.FileName)-3,4) = '.xml'
    then Error := not CurrentSong.LoadXMLSong()
    else Error := not CurrentSong.LoadSong();
  except
    Error := True;
  end;

  if Error then
  begin
    //Error Loading Song -> Go back to Song Screen and Show some Error Message
    FadeTo(@ScreenSong);
    ScreenPopupError.ShowPopup (Language.Translate('ERROR_CORRUPT_SONG'));
    Exit;
  end
  else begin
  {$IFDEF UseMIDIPort}
    MidiOut := TMidiOutput.Create(nil);
    if Ini.Debug = 1 then
      MidiOut.ProductName := 'Microsoft GS Wavetable SW Synth'; // for my kxproject without midi table
    MidiOut.Open;
  {$ENDIF}
    Text[TextTitle].Text :=   CurrentSong.Title;
    Text[TextArtist].Text :=  CurrentSong.Artist;
    Text[TextMp3].Text :=     CurrentSong.Mp3;

    Lines[0].Current := 0;
    CurrentNote := 0;
    Lines[0].Line[0].Note[0].Color := 1;
    AudioPlayback.Open(CurrentSong.Path + CurrentSong.Mp3);
    //Set Down Music Volume for Better hearability of Midi Sounds
    //Music.SetVolume(0.4);

    Lyric.Clear;
    Lyric.X := 400;
    Lyric.Y := 500;
    Lyric.Align := 1;
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
  AktBeat:  integer;
begin
  glClearColor(1,1,1,1);

  // midi music
  if PlaySentenceMidi then begin
   {$IFDEF UseMIDIPort}
    MidiPos := USTime.GetTime - MidiTime + MidiStart;


    // stop the music
    if (MidiPos > MidiStop) then begin
      MidiOut.PutShort($81, Lines[0].Line[Lines[0].Current].Note[MidiLastNote].Tone + 60, 127);
      PlaySentenceMidi := false;
    end;
  {$ENDIF}

    // click
    AktBeat := Floor(GetMidBeat(MidiPos - CurrentSong.GAP / 1000));
    Text[TextDebug].Text := IntToStr(AktBeat);

    if AktBeat <> LastClick then begin
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
  if PlaySentence then begin
    // stop the music
    if (AudioPlayback.Position > PlayStopTime) then
    begin
      AudioPlayback.Stop;
      PlaySentence := false;
    end;

    // click
    if (Click) and (PlaySentence) then begin
//      AktBeat := Floor(CurrentSong.BPM[0].BPM * (Music.Position - CurrentSong.GAP / 1000) / 60);
      AktBeat := Floor(GetMidBeat(AudioPlayback.Position - CurrentSong.GAP / 1000));
      Text[TextDebug].Text := IntToStr(AktBeat);
      if AktBeat <> LastClick then begin
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
  inherited Draw;

  // draw notes
  SingDrawNoteLines(20, 300, 780, 15);
  //Error Drawing when no Song is loaded
  if not Error then
  begin
    SingDrawBeatDelimeters(40, 300, 760, 0);
    EditDrawLine(40, 405, 760, 0, 15);
  end;

  // draw text
  Lyric.Draw;

  Result := true;
end;

procedure TScreenEditSub.onHide;
begin
  {$IFDEF UseMIDIPort}
  MidiOut.Close;
  MidiOut.Free;
  {$ENDIF}
  Lyric.Free;
  //Music.SetVolume(1.0);
end;

function TScreenEditSub.GetNoteName(Note: Integer): String;
var N1, N2: Integer;
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
