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
    ULyrics_bak,
    Math,
    OpenGL12,
    {$IFDEF UseMIDIPort}
    MidiOut,
    {$ENDIF}
    UThemes;

type
  TScreenEditSub = class(TMenu)
    private
      //Variable is True if no SOng is loaded 
      Error:        Boolean;
      
      TextNote:     integer;
      TextSentence: integer;
      TextTitle:    integer;
      TextArtist:   integer;
      TextMp3:      integer;
      TextBPM:      integer;
      TextGAP:      integer;
      TextDebug:    integer;
      TextNStart:   integer;
      TextNDlugosc: integer;
      TextNTon:     integer;
      TextNText:    integer;
      AktNuta:      integer;
      PlaySentence: boolean;
      PlaySentenceMidi: boolean;
      PlayStopTime: real;
      LastClick:    integer;
      Click:        boolean;
      CopySrc:      integer;

      {$IFDEF UseMIDIPort}
      MidiOut:      TMidiOutput;
      {$endif}

      MidiStart:    real;
      MidiStop:     real;
      MidiTime:     real;
      MidiPos:      real;
      MidiLastNote: integer;

      TextEditMode: boolean;

      procedure NewBeat;
      procedure CzesciDivide;
      procedure CzesciMultiply;
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
      Path:               string;
      FileName:           string;
      constructor Create; override;
      procedure onShow; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      function ParseInputEditText(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
      function Draw: boolean; override;
      procedure onHide; override;
  end;

implementation
uses UGraphic, UDraw, UMain, USkins, ULanguage;

// Method for input parsing. If False is returned, GetNextWindow
// should be checked to know the next window to load;
function TScreenEditSub.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
var
  SDL_ModState:  Word;
  R:    real;
begin
  Result := true;

  if TextEditMode then begin
    Result := ParseInputEditText(PressedKey, ScanCode, PressedDown);
  end else begin

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT {+ KMOD_CAPS});

  If (PressedDown) then begin // Key Down
    case PressedKey of

      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          FadeTo(@ScreenSong);
        end;

      SDLK_Q:
        begin
          Result := false;
        end;

      SDLK_BACKQUOTE:
        begin
          // Increase Note Length (same as Alt + Right)
          Inc(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc);
          if AktNuta = Czesci[0].Czesc[Czesci[0].Akt].HighNut then
            Inc(Czesci[0].Czesc[Czesci[0].Akt].Koniec);
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

      SDLK_0:
        begin
          // Increase GAP
          if SDL_ModState = 0 then
            CurrentSong.GAP := CurrentSong.GAP + 10;
          if SDL_ModState = KMOD_LSHIFT then
            CurrentSong.GAP := CurrentSong.GAP + 1000;
        end;

      SDLK_9:
        begin
          // Decrease GAP
          if SDL_ModState = 0 then
            CurrentSong.GAP := CurrentSong.GAP - 10;
          if SDL_ModState = KMOD_LSHIFT then
            CurrentSong.GAP := CurrentSong.GAP - 1000;
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
            if AktNuta > 0 then
              DivideSentence;
          end;

          if SDL_ModState = KMOD_LSHIFT then begin
            // Join next sentence with current
            if Czesci[0].Akt < Czesci[0].High  then
              JoinSentence;
          end;

          if SDL_ModState = KMOD_LCTRL then begin
            // divide note
            DivideNote;
          end;

        end;

      SDLK_S:
        begin
          // Save Song
          if SDL_ModState = KMOD_LSHIFT then
            SaveSong(CurrentSong, Czesci[0], Path + FileName, true)
          else
            SaveSong(CurrentSong, Czesci[0], Path + FileName, false);

          {if SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL + KMOD_LALT then
            // Save Song
            SaveSongDebug(CurrentSong, Czesci[0], 'C:\song.asm', false);}

        end;

      SDLK_D:
        begin
          // Divide lengths by 2
          CzesciDivide;
        end;

      SDLK_M:
        begin
          // Multiply lengths by 2
          CzesciMultiply;
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
        end;

      SDLK_V:
        begin
          // Paste text
          if SDL_ModState = KMOD_LCTRL then begin
            if Czesci[0].Czesc[Czesci[0].Akt].IlNut >= Czesci[0].Czesc[CopySrc].IlNut then
              PasteText
            else
              beep;
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then begin
            CopySentence(CopySrc, Czesci[0].Akt);
          end;
        end;

      SDLK_4:
        begin
          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then begin
            CopySentence(CopySrc, Czesci[0].Akt);
            CopySentence(CopySrc+1, Czesci[0].Akt+1);
            CopySentence(CopySrc+2, Czesci[0].Akt+2);
            CopySentence(CopySrc+3, Czesci[0].Akt+3);
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then begin
            CopySentences(CopySrc, Czesci[0].Akt, 4);
          end;
        end;
      SDLK_5:
        begin
          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then begin
            CopySentence(CopySrc, Czesci[0].Akt);
            CopySentence(CopySrc+1, Czesci[0].Akt+1);
            CopySentence(CopySrc+2, Czesci[0].Akt+2);
            CopySentence(CopySrc+3, Czesci[0].Akt+3);
            CopySentence(CopySrc+4, Czesci[0].Akt+4);
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then begin
            CopySentences(CopySrc, Czesci[0].Akt, 5);
          end;
        end;

      SDLK_T:
        begin
          // Fixes timings between sentences
          FixTimings;
        end;

      SDLK_F4:
        begin
          // Enter Text Edit Mode
          TextEditMode := true;
        end;

      SDLK_P:
        begin
          if SDL_ModState = 0 then begin
            // Play Sentence
            Click := true;
            AudioPlayback.Stop;
            R := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].StartNote);
            if R <= AudioPlayback.Length then begin
              AudioPlayback.Position := R;
              PlayStopTime := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].Koniec);
              PlaySentence := true;
              AudioPlayback.Play;
              LastClick := -100;
            end;
          end;

          if SDL_ModState = KMOD_LSHIFT then begin
            PlaySentenceMidi := true;

            MidiTime := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].StartNote);
            MidiStop := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].Koniec);

            LastClick := -100;
          end;
          if SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL then begin
            PlaySentenceMidi := true;
            MidiTime := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].StartNote);
            MidiStop := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].Koniec);
            LastClick := -100;

            PlaySentence := true;
            Click := true;
            AudioPlayback.Stop;
            AudioPlayback.Position := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].StartNote)+0{-0.10};
            PlayStopTime := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].Koniec)+0;
            AudioPlayback.Play;
            LastClick := -100;
          end;
        end;

      SDLK_SPACE:
        begin
          // Play Sentence
          PlaySentenceMidi := false; // stop midi
          PlaySentence := true;
          Click := false;
          AudioPlayback.Stop;
          AudioPlayback.Position := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Start);
          PlayStopTime := (GetTimeFromBeat(
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Start +
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc));
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
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
            Inc(AktNuta);
            if AktNuta = Czesci[0].Czesc[Czesci[0].Akt].IlNut then AktNuta := 0;
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 1;
            Lyric.Selected := AktNuta;
          end;

          // ctrl + right
          if SDL_ModState = KMOD_LCTRL then begin
            if Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc > 1 then begin
              Dec(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc);
              Inc(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Start);
              if AktNuta = 0 then begin
                Inc(Czesci[0].Czesc[Czesci[0].Akt].Start);
                Inc(Czesci[0].Czesc[Czesci[0].Akt].StartNote);
              end;
            end;
          end;

          // shift + right
          if SDL_ModState = KMOD_LSHIFT then begin
            Inc(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Start);
            if AktNuta = 0 then begin
              Inc(Czesci[0].Czesc[Czesci[0].Akt].Start);
              Inc(Czesci[0].Czesc[Czesci[0].Akt].StartNote);
            end;
            if AktNuta = Czesci[0].Czesc[Czesci[0].Akt].HighNut then
              Inc(Czesci[0].Czesc[Czesci[0].Akt].Koniec);
          end;

          // alt + right
          if SDL_ModState = KMOD_LALT then begin
            Inc(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc);
            if AktNuta = Czesci[0].Czesc[Czesci[0].Akt].HighNut then
              Inc(Czesci[0].Czesc[Czesci[0].Akt].Koniec);
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
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
            Dec(AktNuta);
            if AktNuta = -1 then AktNuta := Czesci[0].Czesc[Czesci[0].Akt].HighNut;
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 1;
            Lyric.Selected := AktNuta;
          end;

          // ctrl + left
          if SDL_ModState = KMOD_LCTRL then begin
            Dec(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Start);
            Inc(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc);
            if AktNuta = 0 then begin
              Dec(Czesci[0].Czesc[Czesci[0].Akt].Start);
              Dec(Czesci[0].Czesc[Czesci[0].Akt].StartNote);
            end;
          end;

          // shift + left
          if SDL_ModState = KMOD_LSHIFT then begin
            Dec(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Start);

            // resizing sentences
            if AktNuta = 0 then begin
              Dec(Czesci[0].Czesc[Czesci[0].Akt].Start);
              Dec(Czesci[0].Czesc[Czesci[0].Akt].StartNote);
            end;

            if AktNuta = Czesci[0].Czesc[Czesci[0].Akt].HighNut then
              Dec(Czesci[0].Czesc[Czesci[0].Akt].Koniec);

          end;

          // alt + left
          if SDL_ModState = KMOD_LALT then begin
            if Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc > 1 then begin
              Dec(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc);
              if AktNuta = Czesci[0].Czesc[Czesci[0].Akt].HighNut then
                Dec(Czesci[0].Czesc[Czesci[0].Akt].Koniec);
            end;
          end;

          // alt + ctrl + shift + right = move all from cursor to left
          if SDL_ModState = KMOD_LALT + KMOD_LCTRL + KMOD_LSHIFT then begin
            MoveAllToEnd(-1);
          end;

        end;

      SDLK_DOWN:
        begin
          {$IFDEF UseMIDIPort}
          // skip to next sentence
          if SDL_ModState = 0 then begin
            MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[MidiLastNote].Ton + 60, 127);
            PlaySentenceMidi := false;

            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
            Inc(Czesci[0].Akt);
            AktNuta := 0;
            if Czesci[0].Akt > Czesci[0].High then Czesci[0].Akt := 0;
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 1;

            Lyric.AddCzesc(Czesci[0].Akt);
            Lyric.Selected := 0;
            AudioPlayback.Stop;
            PlaySentence := false;
          end;

          // decrease tone
          if SDL_ModState = KMOD_LCTRL then begin
            TransposeNote(-1);
          end;
          {$endif}

        end;

      SDLK_UP:
        begin
          {$IFDEF UseMIDIPort}
          // skip to previous sentence
          if SDL_ModState = 0 then begin
            MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[MidiLastNote].Ton + 60, 127);
            PlaySentenceMidi := false;

            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
            Dec(Czesci[0].Akt);
            AktNuta := 0;
            if Czesci[0].Akt = -1 then Czesci[0].Akt := Czesci[0].High;
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 1;

            Lyric.AddCzesc(Czesci[0].Akt);
            Lyric.Selected := 0;
            AudioPlayback.Stop;
            PlaySentence := false;
          end;

          // increase tone
          if SDL_ModState = KMOD_LCTRL then begin
            TransposeNote(1);
          end;
          {$endif}
        end;

      // Golden Note Patch
      SDLK_G:
        begin
          case Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Wartosc of
            0: Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Wartosc := 2;
            1: Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Wartosc := 2;
            2: Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Wartosc := 1;
          end; // case
          Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Freestyle := False;
        end;

      // Freestyle Note Patch
      SDLK_F:
        begin
           case Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Wartosc of
            0:
            begin;
              Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Wartosc := 1;
              Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Freestyle := False;
            end;
            1,2:
            begin;
              Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Wartosc := 0;
              Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Freestyle := True;
            end;
          end; // case

        end;


      end;
    end;
  end; // if
end;

function TScreenEditSub.ParseInputEditText(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
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
          Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Tekst :=
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Tekst + chr(ScanCode);
        end;
      SDLK_BACKSPACE:
        begin
          Delete(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Tekst,
            Length(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Tekst), 1);
        end;
      SDLK_RIGHT:
        begin
          // right
          if SDL_ModState = 0 then begin
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
            Inc(AktNuta);
            if AktNuta = Czesci[0].Czesc[Czesci[0].Akt].IlNut then AktNuta := 0;
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 1;
            Lyric.Selected := AktNuta;
          end;
        end;
      SDLK_LEFT:
        begin
          // left
          if SDL_ModState = 0 then begin
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
            Dec(AktNuta);
            if AktNuta = -1 then AktNuta := Czesci[0].Czesc[Czesci[0].Akt].HighNut;
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 1;
            Lyric.Selected := AktNuta;
          end;
      end;
    end;
  end;
end;

procedure TScreenEditSub.NewBeat;
begin
    // click
{    for Pet := 0 to Czesci[0].Czesc[Czesci[0].Akt].HighNut do
  if (Czesci[0].Czesc[Czesci[0].Akt].Nuta[Pet].Start = Czas.AktBeat) then begin
      // old}
//    Music.PlayClick;
end;

procedure TScreenEditSub.CzesciDivide;
var
  C:    integer;
  N:    integer;
begin                    
  CurrentSong.BPM[0].BPM := CurrentSong.BPM[0].BPM / 2;
  for C := 0 to Czesci[0].High do begin
    Czesci[0].Czesc[C].Start :=     Czesci[0].Czesc[C].Start div 2;
    Czesci[0].Czesc[C].StartNote := Czesci[0].Czesc[C].StartNote div 2;
    Czesci[0].Czesc[C].Koniec :=    Czesci[0].Czesc[C].Koniec div 2;
    for N := 0 to Czesci[0].Czesc[C].HighNut do begin
      Czesci[0].Czesc[C].Nuta[N].Start :=   Czesci[0].Czesc[C].Nuta[N].Start div 2;
      Czesci[0].Czesc[C].Nuta[N].Dlugosc := Round(Czesci[0].Czesc[C].Nuta[N].Dlugosc / 2);
    end; // N
  end; // C
end;

procedure TScreenEditSub.CzesciMultiply;
var
  C:    integer;
  N:    integer;
begin
  CurrentSong.BPM[0].BPM := CurrentSong.BPM[0].BPM * 2;
  for C := 0 to Czesci[0].High do begin
    Czesci[0].Czesc[C].Start :=     Czesci[0].Czesc[C].Start * 2;
    Czesci[0].Czesc[C].StartNote := Czesci[0].Czesc[C].StartNote * 2;
    Czesci[0].Czesc[C].Koniec :=    Czesci[0].Czesc[C].Koniec * 2;
    for N := 0 to Czesci[0].Czesc[C].HighNut do begin
      Czesci[0].Czesc[C].Nuta[N].Start :=   Czesci[0].Czesc[C].Nuta[N].Start * 2;
      Czesci[0].Czesc[C].Nuta[N].Dlugosc := Czesci[0].Czesc[C].Nuta[N].Dlugosc * 2;
    end; // N
  end; // C
end;

procedure TScreenEditSub.LyricsCapitalize;
var
  C:    integer;
  N:    integer; // temporary
  S:    string;
begin
  // temporary
{  for C := 0 to Czesci[0].High do
    for N := 0 to Czesci[0].Czesc[C].HighNut do
      Czesci[0].Czesc[C].Nuta[N].Tekst := AnsiLowerCase(Czesci[0].Czesc[C].Nuta[N].Tekst);}

  for C := 0 to Czesci[0].High do begin
    S := AnsiUpperCase(Copy(Czesci[0].Czesc[C].Nuta[0].Tekst, 1, 1));
    S := S + Copy(Czesci[0].Czesc[C].Nuta[0].Tekst, 2, Length(Czesci[0].Czesc[C].Nuta[0].Tekst)-1);
    Czesci[0].Czesc[C].Nuta[0].Tekst := S;
  end; // C
end;

procedure TScreenEditSub.LyricsCorrectSpaces;
var
  C:    integer;
  N:    integer;
begin
  for C := 0 to Czesci[0].High do begin
    // correct starting spaces in the first word
    while Copy(Czesci[0].Czesc[C].Nuta[0].Tekst, 1, 1) = ' ' do
      Czesci[0].Czesc[C].Nuta[0].Tekst := Copy(Czesci[0].Czesc[C].Nuta[0].Tekst, 2, 100);

    // move spaces on the start to the end of the previous note
    for N := 1 to Czesci[0].Czesc[C].HighNut do begin
      while (Copy(Czesci[0].Czesc[C].Nuta[N].Tekst, 1, 1) = ' ') do begin
        Czesci[0].Czesc[C].Nuta[N].Tekst := Copy(Czesci[0].Czesc[C].Nuta[N].Tekst, 2, 100);
        Czesci[0].Czesc[C].Nuta[N-1].Tekst := Czesci[0].Czesc[C].Nuta[N-1].Tekst + ' ';
      end;
    end; // N

    // correct '-'  to '- '
    for N := 0 to Czesci[0].Czesc[C].HighNut do begin
      if Czesci[0].Czesc[C].Nuta[N].Tekst = '-' then
        Czesci[0].Czesc[C].Nuta[N].Tekst := '- ';
    end; // N

    // add space to the previous note when the current word is '- '
    for N := 1 to Czesci[0].Czesc[C].HighNut do begin
      if Czesci[0].Czesc[C].Nuta[N].Tekst  = '- ' then
        Czesci[0].Czesc[C].Nuta[N-1].Tekst := Czesci[0].Czesc[C].Nuta[N-1].Tekst + ' ';
    end; // N

    // correct too many spaces at the end of note
    for N := 0 to Czesci[0].Czesc[C].HighNut do begin
      while Copy(Czesci[0].Czesc[C].Nuta[N].Tekst, Length(Czesci[0].Czesc[C].Nuta[N].Tekst)-1, 2) = '  ' do
        Czesci[0].Czesc[C].Nuta[N].Tekst := Copy(Czesci[0].Czesc[C].Nuta[N].Tekst, 1, Length(Czesci[0].Czesc[C].Nuta[N].Tekst)-1);
    end; // N

    // and correct if there is no space at the end of sentence
    N := Czesci[0].Czesc[C].HighNut;
    if Copy(Czesci[0].Czesc[C].Nuta[N].Tekst, Length(Czesci[0].Czesc[C].Nuta[N].Tekst), 1) <> ' ' then
      Czesci[0].Czesc[C].Nuta[N].Tekst := Czesci[0].Czesc[C].Nuta[N].Tekst + ' ';

  end; // C
end;

procedure TScreenEditSub.FixTimings;
var
  C:    integer;
  S:    integer;
  Min:  integer;
  Max:  integer;
begin
  for C := 1 to Czesci[0].High do begin
    with Czesci[0].Czesc[C-1] do begin
      Min := Nuta[HighNut].Start + Nuta[HighNut].Dlugosc;
      Max := Czesci[0].Czesc[C].StartNote;
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

      Czesci[0].Czesc[C].Start := S;
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
  NNewL:  integer;
begin
  // increase sentence length by 1
  CLen := Length(Czesci[0].Czesc);
  SetLength(Czesci[0].Czesc, CLen + 1);
  Inc(Czesci[0].Ilosc);
  Inc(Czesci[0].High);

  // move needed sentences to one forward. newly has the copy of divided sentence
  CStart := Czesci[0].Akt;
  for C := CLen-1 downto CStart do
    Czesci[0].Czesc[C+1] := Czesci[0].Czesc[C];

  // clear and set new sentence
  CNew := CStart + 1;
  NStart := AktNuta;
  Czesci[0].Czesc[CNew].Start := Czesci[0].Czesc[CStart].Nuta[NStart].Start;
  Czesci[0].Czesc[CNew].StartNote := Czesci[0].Czesc[CStart].Nuta[NStart].Start;
  Czesci[0].Czesc[CNew].Lyric := '';
  Czesci[0].Czesc[CNew].LyricWidth := 0;
  Czesci[0].Czesc[CNew].Koniec := 0;
  Czesci[0].Czesc[CNew].BaseNote := 0; // 0.5.0: we modify it later in this procedure
  Czesci[0].Czesc[CNew].IlNut := 0;
  Czesci[0].Czesc[CNew].HighNut := -1;
  SetLength(Czesci[0].Czesc[CNew].Nuta, 0);

  // move right notes to new sentences
  NHigh := Czesci[0].Czesc[CStart].HighNut;
  for N := NStart to NHigh do begin
    NNewL := Czesci[0].Czesc[CNew].IlNut;
    SetLength(Czesci[0].Czesc[CNew].Nuta, NNewL + 1);
    Czesci[0].Czesc[CNew].Nuta[NNewL] := Czesci[0].Czesc[CStart].Nuta[N];

    // increase sentence counters
    Inc(Czesci[0].Czesc[CNew].IlNut);
    Inc(Czesci[0].Czesc[CNew].HighNut);
    Czesci[0].Czesc[CNew].Koniec := Czesci[0].Czesc[CNew].Nuta[NNewL].Start +
      Czesci[0].Czesc[CNew].Nuta[NNewL].Dlugosc;
  end;

  // clear old notes and set sentence counters
  Czesci[0].Czesc[CStart].HighNut := NStart - 1;
  Czesci[0].Czesc[CStart].IlNut := Czesci[0].Czesc[CStart].HighNut + 1;
  Czesci[0].Czesc[CStart].Koniec := Czesci[0].Czesc[CStart].Nuta[NStart-1].Start +
    Czesci[0].Czesc[CStart].Nuta[NStart-1].Dlugosc;
  SetLength(Czesci[0].Czesc[CStart].Nuta, Czesci[0].Czesc[CStart].IlNut);

  // 0.5.0: modify BaseNote
  Czesci[0].Czesc[CNew].BaseNote := 120;
  for N := 0 to Czesci[0].Czesc[CNew].IlNut do
    if Czesci[0].Czesc[CNew].Nuta[N].Ton < Czesci[0].Czesc[CNew].BaseNote then
      Czesci[0].Czesc[CNew].BaseNote := Czesci[0].Czesc[CNew].Nuta[N].Ton;

  Czesci[0].Akt := Czesci[0].Akt + 1;
  AktNuta := 0;
  Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 1;
  Lyric.AddCzesc(Czesci[0].Akt);

end;

procedure TScreenEditSub.JoinSentence;
var
  C:      integer;
  N:      integer;
  NStart: integer;
  NDst:   integer;
begin
  C := Czesci[0].Akt;

  // set new sentence
  NStart := Czesci[0].Czesc[C].IlNut;
  Czesci[0].Czesc[C].IlNut := Czesci[0].Czesc[C].IlNut + Czesci[0].Czesc[C+1].IlNut;
  Czesci[0].Czesc[C].HighNut := Czesci[0].Czesc[C].HighNut + Czesci[0].Czesc[C+1].IlNut;
  SetLength(Czesci[0].Czesc[C].Nuta, Czesci[0].Czesc[C].IlNut);

  // move right notes to new sentences
  for N := 0 to Czesci[0].Czesc[C+1].HighNut do begin
    NDst := NStart + N;
    Czesci[0].Czesc[C].Nuta[NDst] := Czesci[0].Czesc[C+1].Nuta[N];
  end;

  // increase sentence counters
  NDst := Czesci[0].Czesc[C].HighNut;
  Czesci[0].Czesc[C].Koniec := Czesci[0].Czesc[C].Nuta[NDst].Start +
    Czesci[0].Czesc[C].Nuta[NDst].Dlugosc;

  // move needed sentences to one backward.
  for C := Czesci[0].Akt + 1 to Czesci[0].High - 1 do
    Czesci[0].Czesc[C] := Czesci[0].Czesc[C+1];

  // increase sentence length by 1
  SetLength(Czesci[0].Czesc, Length(Czesci[0].Czesc) - 1);
  Dec(Czesci[0].Ilosc);
  Dec(Czesci[0].High);
end;

procedure TScreenEditSub.DivideNote;
var
  C:    integer;
  N:    integer;
  NLen: integer;
begin
  C := Czesci[0].Akt;

  NLen := Czesci[0].Czesc[C].IlNut + 1;
  SetLength(Czesci[0].Czesc[C].Nuta, NLen);
  Inc(Czesci[0].Czesc[C].HighNut);
  Inc(Czesci[0].Czesc[C].IlNut);

  // we copy all notes including selected one
  for N := Czesci[0].Czesc[C].HighNut downto AktNuta+1 do begin
    Czesci[0].Czesc[C].Nuta[N] := Czesci[0].Czesc[C].Nuta[N-1];
  end;

  // me slightly modify new note
  Czesci[0].Czesc[C].Nuta[AktNuta].Dlugosc := 1;
  Inc(Czesci[0].Czesc[C].Nuta[AktNuta+1].Start);
  Dec(Czesci[0].Czesc[C].Nuta[AktNuta+1].Dlugosc);
  Czesci[0].Czesc[C].Nuta[AktNuta+1].Tekst := '- ';
  Czesci[0].Czesc[C].Nuta[AktNuta+1].Color := 0;
end;

procedure TScreenEditSub.DeleteNote;
var
  C:    integer;
  N:    integer;
  NLen: integer;
begin
  C := Czesci[0].Akt;

  //Do Not delete Last Note
  if (Czesci[0].High > 0) OR (Czesci[0].Czesc[C].HighNut > 0) then
  begin

    // we copy all notes from the next to the selected one
    for N := AktNuta+1 to Czesci[0].Czesc[C].HighNut do begin
      Czesci[0].Czesc[C].Nuta[N-1] := Czesci[0].Czesc[C].Nuta[N];
    end;

    NLen := Czesci[0].Czesc[C].IlNut - 1;

    if (NLen > 0) then
    begin
      SetLength(Czesci[0].Czesc[C].Nuta, NLen);
      Dec(Czesci[0].Czesc[C].HighNut);
      Dec(Czesci[0].Czesc[C].IlNut);


      // me slightly modify new note
      if AktNuta > Czesci[0].Czesc[C].HighNut then Dec(AktNuta);
        Czesci[0].Czesc[C].Nuta[AktNuta].Color := 1;
    end
    //Last Note of current Sentence Deleted - > Delete Sentence
    else
    begin
      //Move all Sentences after the current to the Left
      for N := C+1 to Czesci[0].High do
        Czesci[0].Czesc[N-1] := Czesci[0].Czesc[N];

      //Delete Last Sentence
      SetLength(Czesci[0].Czesc, Czesci[0].High);
      Czesci[0].High := High(Czesci[0].Czesc);
      Czesci[0].Ilosc := Length(Czesci[0].Czesc);

      AktNuta := 0;
      if (C > 0) then
        Czesci[0].Akt := C - 1
      else
        Czesci[0].Akt := 0;

      Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 1;
    end;
  end;
end;

procedure TScreenEditSub.TransposeNote(Transpose: integer);
begin
  Inc(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Ton, Transpose);
end;

procedure TScreenEditSub.ChangeWholeTone(Tone: integer);
var
  C:  integer;
  N:  integer;
begin
  for C := 0 to Czesci[0].High do begin
    Czesci[0].Czesc[C].BaseNote := Czesci[0].Czesc[C].BaseNote + Tone;
    for N := 0 to Czesci[0].Czesc[C].HighNut do
      Czesci[0].Czesc[C].Nuta[N].Ton := Czesci[0].Czesc[C].Nuta[N].Ton + Tone;
  end;
end;

procedure TScreenEditSub.MoveAllToEnd(Move: integer);
var
  C:    integer;
  N:    integer;
  NStart: integer;
begin
  for C := Czesci[0].Akt to Czesci[0].High do begin
    NStart := 0;
    if C = Czesci[0].Akt then NStart := AktNuta;
    for N := NStart to Czesci[0].Czesc[C].HighNut do begin
      Inc(Czesci[0].Czesc[C].Nuta[N].Start, Move); // move note start

      if N = 0 then begin // fix beginning
        Inc(Czesci[0].Czesc[C].Start, Move);
        Inc(Czesci[0].Czesc[C].StartNote, Move);
      end;

      if N = Czesci[0].Czesc[C].HighNut then // fix ending
        Inc(Czesci[0].Czesc[C].Koniec, Move);

    end; // for
  end; // for
end;

procedure TScreenEditSub.MoveTextToRight;
var
  C:      integer;
  N:      integer;
  NHigh:  integer;
begin
{  C := Czesci[0].Akt;

  for N := Czesci[0].Czesc[C].HighNut downto 1 do begin
    Czesci[0].Czesc[C].Nuta[N].Tekst := Czesci[0].Czesc[C].Nuta[N-1].Tekst;
  end; // for

  Czesci[0].Czesc[C].Nuta[0].Tekst := '- ';}

  C := Czesci[0].Akt;
  NHigh := Czesci[0].Czesc[C].HighNut;

  // last word
  Czesci[0].Czesc[C].Nuta[NHigh].Tekst := Czesci[0].Czesc[C].Nuta[NHigh-1].Tekst + Czesci[0].Czesc[C].Nuta[NHigh].Tekst;

  // other words
  for N := NHigh - 1 downto AktNuta + 1 do begin
    Czesci[0].Czesc[C].Nuta[N].Tekst := Czesci[0].Czesc[C].Nuta[N-1].Tekst;
  end; // for
  Czesci[0].Czesc[C].Nuta[AktNuta].Tekst := '- ';
end;

procedure TScreenEditSub.MarkSrc;
begin
  CopySrc := Czesci[0].Akt;
end;

procedure TScreenEditSub.PasteText;
var
  C:    integer;
  N:    integer;
begin
  C := Czesci[0].Akt;

  for N := 0 to Czesci[0].Czesc[CopySrc].HighNut do
    Czesci[0].Czesc[C].Nuta[N].Tekst := Czesci[0].Czesc[CopySrc].Nuta[N].Tekst;
end;

procedure TScreenEditSub.CopySentence(Src, Dst: integer);
var
  N:      integer;
  Time1:  integer;
  Time2:  integer;
  TD:  integer;
begin
  Time1 := Czesci[0].Czesc[Src].Nuta[0].Start;
  Time2 := Czesci[0].Czesc[Dst].Nuta[0].Start;
  TD := Time2-Time1;

  SetLength(Czesci[0].Czesc[Dst].Nuta, Czesci[0].Czesc[Src].IlNut);
  Czesci[0].Czesc[Dst].IlNut := Czesci[0].Czesc[Src].IlNut;
  Czesci[0].Czesc[Dst].HighNut := Czesci[0].Czesc[Src].HighNut;
  for N := 0 to Czesci[0].Czesc[Src].HighNut do begin
    Czesci[0].Czesc[Dst].Nuta[N].Tekst := Czesci[0].Czesc[Src].Nuta[N].Tekst;
    Czesci[0].Czesc[Dst].Nuta[N].Dlugosc := Czesci[0].Czesc[Src].Nuta[N].Dlugosc;
    Czesci[0].Czesc[Dst].Nuta[N].Ton := Czesci[0].Czesc[Src].Nuta[N].Ton;
    Czesci[0].Czesc[Dst].Nuta[N].Start := Czesci[0].Czesc[Src].Nuta[N].Start + TD;
  end;
  N := Czesci[0].Czesc[Src].HighNut;
  Czesci[0].Czesc[Dst].Koniec := Czesci[0].Czesc[Dst].Nuta[N].Start + Czesci[0].Czesc[Dst].Nuta[N].Dlugosc;
end;

procedure TScreenEditSub.CopySentences(Src, Dst, Num: integer);
var
  C:      integer;
begin
  Lyric := TLyric.Create;
  // create place for new sentences
  SetLength(Czesci[0].Czesc, Czesci[0].Ilosc + Num - 1);

  // moves sentences next to the destination
  for C := Czesci[0].High downto Dst + 1 do begin
    Czesci[0].Czesc[C + Num - 1] := Czesci[0].Czesc[C];
  end;

  // prepares new sentences: sets sentence start and create first note
  for C := 1 to Num-1 do begin
    Czesci[0].Czesc[Dst + C].Start := Czesci[0].Czesc[Dst + C - 1].StartNote +
      (Czesci[0].Czesc[Src + C].StartNote - Czesci[0].Czesc[Src + C - 1].StartNote);
    SetLength(Czesci[0].Czesc[Dst + C].Nuta, 1);
    Czesci[0].Czesc[Dst + C].IlNut := 1;
    Czesci[0].Czesc[Dst + C].HighNut := 0;
    Czesci[0].Czesc[Dst + C].Nuta[0].Start := Czesci[0].Czesc[Dst + C].Start;
    Czesci[0].Czesc[Dst + C].Nuta[0].Dlugosc := 1;
    Czesci[0].Czesc[Dst + C].StartNote := Czesci[0].Czesc[Dst + C].Start;
    Czesci[0].Czesc[Dst + C].Koniec := Czesci[0].Czesc[Dst + C].Start + 1;
  end;

  // increase counters
  Czesci[0].Ilosc := Czesci[0].Ilosc + Num - 1;
  Czesci[0].High := Czesci[0].High + Num - 1;

  for C := 0 to Num-1 do
    CopySentence(Src + C, Dst + C);
end;


constructor TScreenEditSub.Create;
begin
  inherited Create;
  SetLength(Player, 1);

  // linijka
  AddStatic(20, 10, 80, 30, 0, 0, 0, Skin.GetTextureFileName('ButtonF'), 'JPG', 'Font Black');
  AddText(40, 17, 1, 6, 1, 1, 1, 'Line');
  TextSentence := AddText(120, 14, 1, 8, 0, 0, 0, '0 / 0');

  // nuta
  AddStatic(220, 10, 80, 30, 0, 0, 0, Skin.GetTextureFileName('ButtonF'), 'JPG', 'Font Black');
  AddText(242, 17, 1, 6, 1, 1, 1, 'Note');
  TextNote := AddText(320, 14, 1, 8, 0, 0, 0, '0 / 0');

  // file info
  AddStatic(150, 50, 500, 150, 0, 0, 0, Skin.GetTextureFileName('Bar'), 'JPG', 'Font Black');
  AddStatic(151, 52, 498, 146,  1, 1, 1, Skin.GetTextureFileName('Bar'), 'JPG', 'Font Black');
  AddText(180, 65,  0, 8, 0, 0, 0, 'Title:');
  AddText(180, 90,  0, 8, 0, 0, 0, 'Artist:');
  AddText(180, 115, 0, 8, 0, 0, 0, 'Mp3:');
  AddText(180, 140, 0, 8, 0, 0, 0, 'BPM:');
  AddText(180, 165, 0, 8, 0, 0, 0, 'GAP:');

  TextTitle :=  AddText(250, 65,  0, 8, 0, 0, 0, 'a');
  TextArtist := AddText(250, 90,  0, 8, 0, 0, 0, 'b');
  TextMp3 :=    AddText(250, 115, 0, 8, 0, 0, 0, 'c');
  TextBPM :=    AddText(250, 140, 0, 8, 0, 0, 0, 'd');
  TextGAP :=    AddText(250, 165, 0, 8, 0, 0, 0, 'e');

{  AddInteraction(2, TextTitle);
  AddInteraction(2, TextArtist);
  AddInteraction(2, TextMp3);
  AddInteraction(2, TextBPM);
  AddInteraction(2, TextGAP);}

  // note info
  AddText(20, 190,  0, 8, 0, 0, 0, 'Start:');
  AddText(20, 215,  0, 8, 0, 0, 0, 'Duration:');
  AddText(20, 240,  0, 8, 0, 0, 0, 'Tone:');
  AddText(20, 265,  0, 8, 0, 0, 0, 'Text:');

  TextNStart :=   AddText(120, 190,  0, 8, 0, 0, 0, 'a');
  TextNDlugosc := AddText(120, 215,  0, 8, 0, 0, 0, 'b');
  TextNTon :=     AddText(120, 240,  0, 8, 0, 0, 0, 'c');
  TextNText :=    AddText(120, 265,  0, 8, 0, 0, 0, 'd');

  // debug
  TextDebug :=  AddText(30, 550, 0, 8, 0, 0, 0, '');

end;

procedure TScreenEditSub.onShow;
begin
  Log.LogStatus('Initializing', 'TEditScreen.onShow');

  try
    ResetSingTemp;
//    Error := not LoadSong(Path + FileName);  // todo - JB come back to this
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

    Czesci[0].Akt := 0;
    AktNuta := 0;
    Czesci[0].Czesc[0].Nuta[0].Color := 1;

    AudioPlayback.Open(Path + CurrentSong.Mp3);
    //Set Down Music Volume for Better hearability of Midi Sounds
    //Music.SetVolume(40);
    
    Lyric.Clear;
    Lyric.X := 400;
    Lyric.Y := 500;
    Lyric.Align := 1;
    Lyric.Size := 14;
    Lyric.ColR := 0;
    Lyric.ColG := 0;
    Lyric.ColB := 0;
    Lyric.ColSR := Skin_FontHighlightR;
    Lyric.ColSG := Skin_FontHighlightG;
    Lyric.ColSB := Skin_FontHighlightB;
    Lyric.Style := 0;
    Lyric.AddCzesc(0);
    Lyric.Selected := 0;

    NotesH := 7;
    NotesW := 4;

  end;

//  Interaction := 0;
  TextEditMode := false;
end;

function TScreenEditSub.Draw: boolean;
var
  Min:    integer;
  Sec:    integer;
  Tekst:  string;
  Pet:    integer;
  AktBeat:  integer;
begin
  glClearColor(1,1,1,1);

  // midi music
  if PlaySentenceMidi then begin
    MidiPos := USTime.GetTime - MidiTime + MidiStart;

    {$IFDEF UseMIDIPort}
    // stop the music
    if (MidiPos > MidiStop) then begin
      MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[MidiLastNote].Ton + 60, 127);
      PlaySentenceMidi := false;
    end;
    {$ENDIF}

    // click
    AktBeat := Floor(GetMidBeat(MidiPos - CurrentSong.GAP / 1000));
    Text[TextDebug].Text := IntToStr(AktBeat);

    if AktBeat <> LastClick then begin
      for Pet := 0 to Czesci[0].Czesc[Czesci[0].Akt].HighNut do
        if (Czesci[0].Czesc[Czesci[0].Akt].Nuta[Pet].Start = AktBeat) then
        begin

          {$IFDEF UseMIDIPort}
          LastClick := AktBeat;
          if Pet > 0 then
            MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[Pet-1].Ton + 60, 127);
          MidiOut.PutShort($91, Czesci[0].Czesc[Czesci[0].Akt].Nuta[Pet].Ton + 60, 127);
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
        for Pet := 0 to Czesci[0].Czesc[Czesci[0].Akt].HighNut do
          if (Czesci[0].Czesc[Czesci[0].Akt].Nuta[Pet].Start = AktBeat) then
          begin
            AudioPlayback.PlayClick;
            LastClick := AktBeat;
          end;
      end;
    end; // click
  end; // if PlaySentence
  

  Text[TextSentence].Text := IntToStr(Czesci[0].Akt + 1) + ' / ' + IntToStr(Czesci[0].Ilosc);
  Text[TextNote].Text := IntToStr(AktNuta + 1) + ' / ' + IntToStr(Czesci[0].Czesc[Czesci[0].Akt].IlNut);

  // Song info
  Text[TextBPM].Text := FloatToStr(CurrentSong.BPM[0].BPM / 4);
  Text[TextGAP].Text := FloatToStr(CurrentSong.GAP);

  //Error reading Variables when no Song is loaded
  if not Error then
  begin
    // Note info
    Text[TextNStart].Text :=    IntToStr(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Start);
    Text[TextNDlugosc].Text :=  IntToStr(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc);
    Text[TextNTon].Text :=      IntToStr(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Ton) + ' ( ' + GetNoteName(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Ton) + ' )';
    Text[TextNText].Text :=              Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Tekst;
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
    EditDrawCzesc(40, 405, 760, 0, 15);
  end;

  // draw text
  Lyric.Draw;

end;

procedure TScreenEditSub.onHide;
begin
  {$IFDEF UseMIDIPort}
  MidiOut.Close;
  MidiOut.Free;
  {$ENDIF}
  //Music.SetVolume(100);
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
