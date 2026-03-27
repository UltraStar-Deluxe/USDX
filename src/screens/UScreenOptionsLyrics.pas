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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenOptionsLyrics.pas $
 * $Id: UScreenOptionsLyrics.pas 2337 2010-05-03 21:55:18Z k-m_schindler $
 *}

unit UScreenOptionsLyrics;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
{$codepage UTF8}

{$I switches.inc}

uses
  UCommon,
  UDisplay,
  UFiles,
  UIni,
  ULyrics,
  UMenu,
  UMusic,
  UThemes,
  sdl2,
  TextGL;

type
  TScreenOptionsLyrics = class(TOptionsMenu)
    private
      LyricEngine:   array [0..1] of TLyricEngine;
      GermanLine:     TLine;
      FrenchLine:     TLine;
      SpanishLine:    TLine;
      PolishLine:     TLine;
      LastFontFamily: Integer;
      LastFontStyle:  Integer;
      AnimStartTicks: UInt32;
      LastAnimBeat:   real;
      SavedAnimBeat:  real;
      procedure RebuildLines;

    public
      constructor Create; override;
      destructor Destroy; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure OnHide; override;
      function Draw: boolean; override;
      procedure LyricSample;

    protected
      procedure LoadWidgets; override;
  end;

const
  ID='ID_075';   //for help system
  PREVIEW_LOOP_BEATS = 200;

implementation

uses
  UGraphic,
  UHelp,
  ULanguage,
  ULog,
  UMenuButton,
  UUnicodeUtils,
  Math,
  SysUtils;

function TScreenOptionsLyrics.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check normal keys
    case PressedKey of
      SDLK_Q:
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
          Ini.Save;
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenOptions);
        end;
      SDLK_TAB:
      begin
        ScreenPopupHelp.ShowPopup();
      end;
      SDLK_RETURN:
        begin
          if SelInteraction = 4 then
          begin
            Ini.Save;
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(@ScreenOptions);
          end;
        end;
      SDLK_DOWN:
        InteractNext;
      SDLK_UP :
        InteractPrev;
      SDLK_RIGHT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 3) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractInc;
          end;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 3) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
          end;
        end;
    end;
  end;
end;

constructor TScreenOptionsLyrics.Create;
const
  LyricsGerman:  array [0..7]  of string = ('Max ', '(zwölf) ', 'quäkt ', 'hy', 'pend: ', '»Grüß ', 'Jobs, ', 'Vic!« ');
  LyricsFrench:  array [0..12] of string = ('Voix ', 'am', 'bi', 'gu', 'ë ', 'd’un ', 'cœur ', 'qui ', 'pré', 'fère ', 'le ', 'zé', 'phyr. ');
  LyricsSpanish: array [0..15] of string = ('Whis', 'ky ', 'bu', 'e', 'no: ', '¡Ex', 'ci', 'tad ', 'mi ', 'frá', 'gil ', 'pe', 'que', 'ña ', 've', 'jez! ');
  LyricsPolish:  array [0..9]  of string = ('“W ', 'ni', 'żach ', 'mógł ', 'zjeść ', 'tru', 'flę ', 'koń ', 'bądź ', 'psy?” ');

var
  i: Integer;
begin
  inherited Create;
  Description := Language.Translate('SING_OPTIONS_LYRICS_DESC');
  WhereAmI := Language.Translate('SING_OPTIONS_LYRICS_WHEREAMI');
  Load;

  // lyric sample engine 1
  LyricEngine[0] := TLyricEngine.Create(
      80, 350, 640, 40,
      80, 390, 640, 40);

  // build German line data
  SetLength(GermanLine.Notes, Length(LyricsGerman));
  for i := Low(GermanLine.Notes) to High(GermanLine.Notes) do
  begin
    GermanLine.Notes[i].NoteType  := ntNormal;
    GermanLine.Notes[i].StartBeat := i * Trunc(PREVIEW_LOOP_BEATS/Length(LyricsGerman));
    GermanLine.Notes[i].Duration  := Trunc(PREVIEW_LOOP_BEATS/Length(LyricsGerman));
    GermanLine.Notes[i].Text      := LyricsGerman[i];
  end;
  GermanLine.ScoreValue := 6;
  GermanLine.StartBeat  := 0;
  GermanLine.EndBeat    := GermanLine.Notes[High(GermanLine.Notes)].StartBeat + GermanLine.Notes[High(GermanLine.Notes)].Duration;

  // build French line data
  SetLength(FrenchLine.Notes, Length(LyricsFrench));
  for i := Low(FrenchLine.Notes) to High(FrenchLine.Notes) do
  begin
    FrenchLine.Notes[i].NoteType  := ntFreestyle;
    FrenchLine.Notes[i].StartBeat := i * Trunc(PREVIEW_LOOP_BEATS/Length(LyricsFrench));
    FrenchLine.Notes[i].Duration  := Trunc(PREVIEW_LOOP_BEATS/Length(LyricsFrench));
    FrenchLine.Notes[i].Text      := LyricsFrench[i];
  end;

  // lyric sample engine 2
  LyricEngine[1] := TLyricEngine.Create(
      80, 450, 640, 40,
      80, 490, 640, 40);

  // build Spanish line data
  SetLength(SpanishLine.Notes, Length(LyricsSpanish));
  for i := Low(SpanishLine.Notes) to High(SpanishLine.Notes) do
  begin
    SpanishLine.Notes[i].NoteType  := ntNormal;
    SpanishLine.Notes[i].StartBeat := i * Trunc(PREVIEW_LOOP_BEATS/Length(LyricsSpanish));
    SpanishLine.Notes[i].Duration  := Trunc(PREVIEW_LOOP_BEATS/Length(LyricsSpanish));
    SpanishLine.Notes[i].Text      := LyricsSpanish[i];
  end;

  // build Polish line data
  SetLength(PolishLine.Notes, Length(LyricsPolish));
  for i := Low(PolishLine.Notes) to High(PolishLine.Notes) do
  begin
    PolishLine.Notes[i].NoteType  := ntFreestyle;
    PolishLine.Notes[i].StartBeat := i * Trunc(PREVIEW_LOOP_BEATS/Length(LyricsPolish));
    PolishLine.Notes[i].Duration  := Trunc(PREVIEW_LOOP_BEATS/Length(LyricsPolish));
    PolishLine.Notes[i].Text      := LyricsPolish[i];
  end;

  // force RebuildLines on first draw
  LastFontFamily := -1;
  LastFontStyle  := -1;
  SavedAnimBeat  := 0;
end;

destructor TScreenOptionsLyrics.Destroy;
var
  i: Integer;
begin
  for i := Low(LyricEngine) to High(LyricEngine) do
    LyricEngine[i].Free;
  inherited Destroy;
end;

procedure TScreenOptionsLyrics.RebuildLines;
var
  i: Integer;
begin
  for i := Low(LyricEngine) to High(LyricEngine) do
  begin
    LyricEngine[i].Clear;
  end;
  LyricEngine[0].AddLine(@GermanLine);
  LyricEngine[0].AddLine(@FrenchLine);
  LyricEngine[1].AddLine(@SpanishLine);
  LyricEngine[1].AddLine(@PolishLine);
end;

procedure TScreenOptionsLyrics.LyricSample;
var
  i:        Integer;
  AnimBeat: real;
begin
  if (Ini.LyricsFont <> LastFontFamily) or (Ini.LyricsStyle <> LastFontStyle) then
  begin
    for i := Low(LyricEngine) to High(LyricEngine) do
    begin
      LyricEngine[i].FontFamily := Ini.LyricsFont;
      LyricEngine[i].FontStyle  := Ini.LyricsStyle;
    end;
    if ((LastFontFamily = -1) or (LastFontStyle = -1)) then
      RebuildLines
    else
    begin
      for i := Low(LyricEngine) to High(LyricEngine) do
        LyricEngine[i].UpdateLineMetrics;
    end;
    LastFontFamily           := Ini.LyricsFont;
    LastFontStyle            := Ini.LyricsStyle;
  end;

  AnimBeat := fmod((SDL_GetTicks - AnimStartTicks) / 100.0, PREVIEW_LOOP_BEATS);

  // when the beat wraps back to the start, rebuild lines to reset engine state
  if AnimBeat < LastAnimBeat then
    RebuildLines;
  LastAnimBeat := AnimBeat;

  for i := Low(LyricEngine) to High(LyricEngine) do
  begin
    // current lyrics
    LyricEngine[i].LineColor_act.R := 0;
    LyricEngine[i].LineColor_act.G := 0.6;
    LyricEngine[i].LineColor_act.B := 1;

    // current line
    LyricEngine[i].LineColor_en.R := 1;
    LyricEngine[i].LineColor_en.G := 1;
    LyricEngine[i].LineColor_en.B := 1;

    // next line
    LyricEngine[i].LineColor_dis.R := 1;
    LyricEngine[i].LineColor_dis.G := 1;
    LyricEngine[i].LineColor_dis.B := 1;

    LyricEngine[i].Draw(AnimBeat);
  end;
end;

procedure TScreenOptionsLyrics.OnShow;
begin
  inherited;

  Interaction := 0;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenOptionsLyrics');

  AnimStartTicks := SDL_GetTicks - Trunc(SavedAnimBeat * 100.0);
  LastAnimBeat   := -1;
end;

procedure TScreenOptionsLyrics.OnHide;
begin
  inherited;
  SavedAnimBeat := LastAnimBeat;
end;

function TScreenOptionsLyrics.Draw: boolean;
begin
  Result := inherited Draw;
  LyricSample();
end;

procedure TScreenOptionsLyrics.LoadWidgets;
begin
  // when editing this, also search for SelInteraction
  AddSelectSlide('SING_OPTIONS_LYRICS_FONT', Ini.LyricsFont, FontFamilyNames);
  AddSelectSlide('SING_OPTIONS_LYRICS_STYLE', Ini.LyricsStyle, ILyricsStyleTranslated);
  AddSelectSlide('SING_OPTIONS_LYRICS_EFFECT', Ini.LyricsEffect, ILyricsEffectTranslated);
  AddSelectSlide('SING_OPTIONS_LYRICS_NOTELINES', Ini.NoteLines, INoteLinesTranslated);
end;

end.