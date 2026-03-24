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
      Lyrics:        TLyricEngine;
      TopLine:       TLine;
      BottomLine:    TLine;
      LastFontFamily: Integer;
      LastFontStyle:  Integer;
      procedure RebuildLines;

    public
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      function Draw: boolean; override;
      procedure LyricSample;

    protected
      procedure LoadWidgets; override;
  end;

const
  ID='ID_075';   //for help system

implementation

uses
  UGraphic,
  UHelp,
  ULanguage,
  ULog,
  UMenuButton,
  UUnicodeUtils,
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
  LyricsTop: array [0..7] of string = ('Max ', '(zwölf) ', 'quäkt ', 'hy', 'pend: ', '»Grüß ', 'Jobs, ', 'Vic!« ');
  LyricsBottom: array [0..12] of string = ('Voix ', 'am', 'bi', 'gu', 'ë ', 'd''un ', 'cœur ', 'qui ', 'pré', 'fère ', 'le ', 'zé', 'phyr. ');
var
  i: Integer;
begin
  inherited Create;
  Description := Language.Translate('SING_OPTIONS_LYRICS_DESC');
  WhereAmI := Language.Translate('SING_OPTIONS_LYRICS_WHEREAMI');
  Load;

  // lyric sample engine
  Lyrics := TLyricEngine.Create(
      80, 450, 640, 40,
      80, 490, 640, 40);

  // build top line data
  SetLength(TopLine.Notes, Length(LyricsTop));
  for i := 0 to High(TopLine.Notes) do
  begin
    TopLine.Notes[i].NoteType  := ntNormal;
    TopLine.Notes[i].StartBeat := i * 10;
    TopLine.Notes[i].Duration  := 8;
    TopLine.Notes[i].Text      := LyricsTop[i];
  end;
  TopLine.ScoreValue := 6;
  TopLine.StartBeat  := 0;
  TopLine.EndBeat    := TopLine.Notes[High(TopLine.Notes)].StartBeat + TopLine.Notes[High(TopLine.Notes)].Duration;
  TopLine.LastLine   := true;

  // build bottom line data
  SetLength(BottomLine.Notes, Length(LyricsBottom));
  for i := 0 to High(BottomLine.Notes) do
  begin
    BottomLine.Notes[i].NoteType  := ntFreestyle;
    BottomLine.Notes[i].StartBeat := i * 10;
    BottomLine.Notes[i].Duration  := 8;
    BottomLine.Notes[i].Text      := LyricsBottom[i];
  end;
  BottomLine.LastLine := true;

  // force RebuildLines on first draw
  LastFontFamily := -1;
  LastFontStyle  := -1;
end;

procedure TScreenOptionsLyrics.RebuildLines;
begin
  Lyrics.Clear;
  Lyrics.AddLine(@TopLine);
  Lyrics.AddLine(@BottomLine);
  Lyrics.AddLine(@BottomLine);
end;

procedure TScreenOptionsLyrics.LyricSample;
begin
  if (Ini.LyricsFont <> LastFontFamily) or (Ini.LyricsStyle <> LastFontStyle) then
  begin
    LastFontFamily    := Ini.LyricsFont;
    LastFontStyle     := Ini.LyricsStyle;
    Lyrics.FontFamily := Ini.LyricsFont;
    Lyrics.FontStyle  := Ini.LyricsStyle;
    RebuildLines;
  end;

  // current lyrics
  Lyrics.LineColor_act.R := 0;
  Lyrics.LineColor_act.G := 0.6;
  Lyrics.LineColor_act.B := 1;

  // current line
  Lyrics.LineColor_en.R := 1;
  Lyrics.LineColor_en.G := 1;
  Lyrics.LineColor_en.B := 1;

  // next line
  Lyrics.LineColor_dis.R := 1;
  Lyrics.LineColor_dis.G := 1;
  Lyrics.LineColor_dis.B := 1;

  Lyrics.Draw(LyricsState.MidBeat);
end;

procedure TScreenOptionsLyrics.OnShow;
begin
  inherited;

  Interaction := 0;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenOptionsLyrics');

  LyricsState.StartTime := 0;
  LyricsState.UpdateBeats;
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
