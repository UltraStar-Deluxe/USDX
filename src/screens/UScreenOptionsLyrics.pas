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
  TScreenOptionsLyrics = class(TMenu)
    private
      Lyrics: TLyricEngine;
      Line: TLine;

    public
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      function Draw: boolean; override;
      procedure LyricSample;
  end;

const
  ID='ID_075';   //for help system

implementation

uses
  UGraphic,
  UHelp,
  ULog,
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
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsLyrics);

  Theme.OptionsLyrics.SelectLyricsFont.showArrows := true;
  Theme.OptionsLyrics.SelectLyricsFont.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsLyrics.SelectLyricsFont, Ini.LyricsFont, FontFamilyNames);

  Theme.OptionsLyrics.SelectLyricsStyle.showArrows := true;
  Theme.OptionsLyrics.SelectLyricsStyle.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsLyrics.SelectLyricsStyle, Ini.LyricsStyle, ILyricsStyleTranslated);

  Theme.OptionsLyrics.SelectLyricsEffect.showArrows := true;
  Theme.OptionsLyrics.SelectLyricsEffect.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsLyrics.SelectLyricsEffect, Ini.LyricsEffect, ILyricsEffectTranslated);

  Theme.OptionsLyrics.SelectNoteLines.showArrows := true;
  Theme.OptionsLyrics.SelectNoteLines.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsLyrics.SelectNoteLines, Ini.NoteLines, INoteLinesTranslated);

  AddButton(Theme.OptionsLyrics.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(20, 5, Theme.Options.Description[OPTIONS_DESC_INDEX_BACK]);

  // lyric sample
  Lyrics := TLyricEngine.Create(
      Theme.OptionsLyrics.UpperX, Theme.OptionsLyrics.UpperY, Theme.OptionsLyrics.UpperW, Theme.OptionsLyrics.UpperH,
      Theme.OptionsLyrics.LowerX, Theme.OptionsLyrics.LowerY, Theme.OptionsLyrics.LowerW, Theme.OptionsLyrics.LowerH);

  //Line.Lyric := 'Lorem ipsum dolor sit amet';
  // 1st line
  SetLength(Line.Notes, 5);
  Line.Notes[0].Text := 'Lorem';
  Line.Notes[1].Text := ' ipsum';
  Line.Notes[2].Text := ' dolor';
  Line.Notes[3].Text := ' sit';
  Line.Notes[4].Text := ' amet';

  Line.Notes[0].StartBeat := 0;
  Line.Notes[1].StartBeat := 10;
  Line.Notes[2].StartBeat := 20;
  Line.Notes[3].StartBeat := 30;
  Line.Notes[4].StartBeat := 40;

  Line.Notes[0].Duration := 10;
  Line.Notes[1].Duration := 10;
  Line.Notes[2].Duration := 10;
  Line.Notes[3].Duration := 10;
  Line.Notes[4].Duration := 10;

  Line.ScoreValue := 6;
  Line.EndBeat := 50;
  Line.StartBeat := 0;
  Line.LastLine := true;
  Lyrics.AddLine(@Line);

  // 2nd line
  //consectetur adipiscing elit
  SetLength(Line.Notes, 3);

  Line.Notes[0].Text := 'consectetur';
  Line.Notes[1].Text := ' adipiscing';
  Line.Notes[2].Text := ' elit';

  Line.Notes[0].StartBeat := 50;
  Line.Notes[1].StartBeat := 60;
  Line.Notes[2].StartBeat := 70;

  Line.Notes[0].Duration := 10;
  Line.Notes[1].Duration := 10;
  Line.Notes[2].Duration := 10;

  Line.LastLine := true;

  Lyrics.AddLine(@Line);
  Lyrics.AddLine(@Line);
end;

procedure TScreenOptionsLyrics.LyricSample;
var
  Col: TRGB;
begin
  Lyrics.FontFamily := Ini.LyricsFont;
  Lyrics.FontStyle  := Ini.LyricsStyle;

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

end.
