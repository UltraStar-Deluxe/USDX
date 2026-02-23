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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenOptionsAdvanced.pas $
 * $Id: UScreenOptionsAdvanced.pas 2338 2010-05-03 21:58:30Z k-m_schindler $
 *}

unit UScreenOptionsAdvanced;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UDisplay,
  UFiles,
  UIni,
  UMenu,
  UMusic,
  UThemes,
  sdl2;

type
  TScreenOptionsAdvanced = class(TOptionsMenu)
    public
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
    protected
      procedure LoadWidgets; override;
  end;

const
  ID='ID_078';   //for help system

implementation

uses
  UGraphic,
  UHelp,
  ULanguage,
  ULog,
  UUnicodeUtils,
  SysUtils;

function TScreenOptionsAdvanced.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
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
          if SelInteraction = 8 then
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
          if (SelInteraction >= 0) and (SelInteraction <= 7) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractInc;
          end;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 7) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
          end;
        end;
    end;
  end;
end;

constructor TScreenOptionsAdvanced.Create;
begin
  inherited Create;
  Description := Language.Translate('SING_OPTIONS_ADVANCED_DESC');
  WhereAmI := Language.Translate('SING_OPTIONS_ADVANCED_WHEREAMI');
  Load;
  Interaction := 0;
end;

procedure TScreenOptionsAdvanced.OnShow;
begin
  inherited;

  Interaction := 0;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenOptionsAdvanced');
end;

procedure TScreenOptionsAdvanced.LoadWidgets;
begin
  AddSelectSlide('SING_OPTIONS_ADVANCED_SCREENFADE', Ini.ScreenFade, IScreenFadeTranslated);
  AddSelectSlide('SING_OPTIONS_ADVANCED_EFFECTSING', Ini.EffectSing, IEffectSingTranslated);
  AddSelectSlide('SING_OPTIONS_ADVANCED_ONSONGCLICK', Ini.OnSongClick, IOnSongClickTranslated);
  AddSelectSlide('SING_OPTIONS_ADVANCED_ASKBEFOREDEL', Ini.AskBeforeDel, IAskbeforeDelTranslated);
  AddSelectSlide('SING_OPTIONS_ADVANCED_PARTYPOPUP', Ini.PartyPopup, IPartyPopupTranslated);
  AddSelectSlide('SING_OPTIONS_ADVANCED_SINGSCORES', Ini.SingScores, ISingScoresTranslated);
  AddSelectSlide('SING_OPTIONS_ADVANCED_TOPSCORES', Ini.TopScores, ITopScoresTranslated);
end;

end.
