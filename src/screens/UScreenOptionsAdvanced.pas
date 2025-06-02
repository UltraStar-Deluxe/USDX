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
  TScreenOptionsAdvanced = class(TMenu)
    public
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
  end;

const
  ID='ID_078';   //for help system

implementation

uses
  UGraphic,
  UHelp,
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

  LoadFromTheme(Theme.OptionsAdvanced);

  //SelectLoadAnimation Hidden because it is useless atm
  //AddSelect(Theme.OptionsAdvanced.SelectLoadAnimation, Ini.LoadAnimation, ILoadAnimationTranslated);
  Theme.OptionsAdvanced.SelectScreenFade.showArrows := true;
  Theme.OptionsAdvanced.SelectScreenFade.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsAdvanced.SelectScreenFade, Ini.ScreenFade, IScreenFadeTranslated);

  Theme.OptionsAdvanced.SelectEffectSing.showArrows := true;
  Theme.OptionsAdvanced.SelectEffectSing.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsAdvanced.SelectEffectSing, Ini.EffectSing, IEffectSingTranslated);

  Theme.OptionsAdvanced.SelectLineBonus.showArrows := true;
  Theme.OptionsAdvanced.SelectLineBonus.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsAdvanced.SelectLineBonus, Ini.LineBonus, ILineBonusTranslated);

  Theme.OptionsAdvanced.SelectOnSongClick.showArrows := true;
  Theme.OptionsAdvanced.SelectOnSongClick.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsAdvanced.SelectOnSongClick, Ini.OnSongClick, IOnSongClickTranslated);

  Theme.OptionsAdvanced.SelectAskbeforeDel.showArrows := true;
  Theme.OptionsAdvanced.SelectAskbeforeDel.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsAdvanced.SelectAskbeforeDel, Ini.AskBeforeDel, IAskbeforeDelTranslated);

  Theme.OptionsAdvanced.SelectPartyPopup.showArrows := true;
  Theme.OptionsAdvanced.SelectPartyPopup.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsAdvanced.SelectPartyPopup, Ini.PartyPopup, IPartyPopupTranslated);

  Theme.OptionsAdvanced.SelectSingScores.showArrows := true;
  Theme.OptionsAdvanced.SelectSingScores.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsAdvanced.SelectSingScores, Ini.SingScores, ISingScoresTranslated);

  Theme.OptionsAdvanced.SelectDuetScores.showArrows := true;
  Theme.OptionsAdvanced.SelectDuetScores.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsAdvanced.SelectDuetScores, Ini.DuetScores, IDuetScoresTranslated);

  Theme.OptionsAdvanced.SelectTopScores.showArrows := true;
  Theme.OptionsAdvanced.SelectTopScores.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsAdvanced.SelectTopScores, Ini.TopScores, ITopScoresTranslated);

  AddButton(Theme.OptionsAdvanced.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(20, 5, Theme.Options.Description[OPTIONS_DESC_INDEX_BACK]);

  Interaction := 0;
end;

procedure TScreenOptionsAdvanced.OnShow;
begin
  inherited;

  Interaction := 0;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenOptionsAdvanced');
end;

end.
