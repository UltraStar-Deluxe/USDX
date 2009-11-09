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

unit UScreenOptionsAdvanced;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SDL,
  UMenu,
  UDisplay,
  UMusic,
  UFiles,
  UIni,
  UThemes;

type
  TScreenOptionsAdvanced = class(TMenu)
    public
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
  end;

implementation

uses
  UGraphic,
  UUnicodeUtils,
  SysUtils;

function TScreenOptionsAdvanced.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check normal keys
    case UCS4UpperCase(CharCode) of
      Ord('Q'):
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
          // Escape -> save nothing - just leave this screen
          
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenOptions);
        end;
      SDLK_RETURN:
        begin
          //SelectLoadAnimation Hidden because it is useless atm
          //if SelInteraction = 7 then begin
          if SelInteraction = 6 then
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
          //SelectLoadAnimation Hidden because it is useless atm
          //if (SelInteraction >= 0) and (SelInteraction <= 6) then begin
          if (SelInteraction >= 0) and (SelInteraction <= 5) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractInc;
          end;
        end;
      SDLK_LEFT:
        begin
          //SelectLoadAnimation Hidden because it is useless atm
          //if (SelInteraction >= 0) and (SelInteraction <= 6) then begin
          if (SelInteraction >= 0) and (SelInteraction <= 5) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
          end;
        end;
    end;
  end;
end;

constructor TScreenOptionsAdvanced.Create;
//var
// I:      integer; // Auto Removed, Unused Variable
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

  AddButton(Theme.OptionsAdvanced.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);

  Interaction := 0;
end;

procedure TScreenOptionsAdvanced.OnShow;
begin
  inherited;

  Interaction := 0;
end;

end.
