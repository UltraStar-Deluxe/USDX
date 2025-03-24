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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenOptionsThemes.pas $
 * $Id: UScreenOptionsThemes.pas 3133 2015-09-07 17:10:36Z basisbit $
 *}

unit UScreenOptionsThemes;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UConfig,
  UDisplay,
  UFiles,
  UIni,
  UMenu,
  UMusic,
  UThemes,
  sdl2;

type
  TScreenOptionsThemes = class(TMenu)
    private
      procedure ReloadTheme;
      procedure ReloadScreens;

    public
      ActualTheme:  Integer;
      ActualSkin:   Integer;
      ActualColor:  Integer;

      SkinSelect: integer;

      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure InteractInc; override;
      procedure InteractDec; override;
  end;

const
  ID='ID_076';   //for help system

implementation

uses 
  UGraphic,
  UHelp,
  ULog,
  UMain,
  UPathUtils,
  URecord,
  USkins,
  UUnicodeUtils,
  SysUtils;

function TScreenOptionsThemes.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
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

          ReloadScreens;

          AudioPlayback.PlaySound(SoundLib.Back);

          // select theme button in new created options screen
          ScreenOptions.Interaction := 4;

          FadeTo(@ScreenOptions);
        end;
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;
      SDLK_RETURN:
        begin
          if SelInteraction = 3 then
          begin
            Ini.Save;

            ReloadScreens;

            AudioPlayback.PlaySound(SoundLib.Back);

            // select theme button in new created options screen
            ScreenOptions.Interaction := 4;

            FadeTo(@ScreenOptions);
          end;
        end;
      SDLK_DOWN:
        InteractNext;
      SDLK_UP :
        InteractPrev;
      SDLK_RIGHT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 2) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractInc;
          end;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 2) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
          end;
        end;
    end;
  end;
end;

procedure TScreenOptionsThemes.InteractInc;
begin
  inherited InteractInc;

  //Update Skins
  if (SelInteraction = 0) then
  begin
    Skin.OnThemeChange;
    UpdateSelectSlideOptions(Theme.OptionsThemes.SelectSkin, SkinSelect, ISkin, Ini.SkinNo);

    // set skin to themes default skin
    Ini.SkinNo := Theme.Themes[Ini.Theme].DefaultSkin;
  end;

  { set skins default color }
  if (SelInteraction = 0) or (SelInteraction = 1) then
  begin
    Ini.Color := Skin.GetDefaultColor(Ini.SkinNo);
  end;

  ReloadTheme();
end;

procedure TScreenOptionsThemes.InteractDec;
begin
  inherited InteractDec;

  //Update Skins
  if (SelInteraction = 0 ) then
  begin
    Skin.OnThemeChange;
    UpdateSelectSlideOptions (Theme.OptionsThemes.SelectSkin, SkinSelect, ISkin, Ini.SkinNo);

    // set skin to themes default skin
    Ini.SkinNo := Theme.Themes[Ini.Theme].DefaultSkin;
  end;

  { set skins default color }
  if (SelInteraction = 0) or (SelInteraction = 1) then
  begin
    Ini.Color := Skin.GetDefaultColor(Ini.SkinNo);
  end;

  ReloadTheme();
end;

constructor TScreenOptionsThemes.Create;
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsThemes);

  Theme.OptionsThemes.SelectTheme.showArrows := true;
  Theme.OptionsThemes.SelectTheme.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsThemes.SelectTheme, Ini.Theme, ITheme);

  Theme.OptionsThemes.SelectSkin.showArrows := true;
  Theme.OptionsThemes.SelectSkin.oneItemOnly := true;
  SkinSelect := AddSelectSlide(Theme.OptionsThemes.SelectSkin, Ini.SkinNo, ISkin);

  Theme.OptionsThemes.SelectColor.showArrows := true;
  Theme.OptionsThemes.SelectColor.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsThemes.SelectColor, Ini.Color, IColorTranslated);

  AddButton(Theme.OptionsThemes.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(20, 5, Theme.Options.Description[OPTIONS_DESC_INDEX_BACK]);
  if (Scrollable) then
    InitScrollBar(Theme.OptionsThemes);
end;

procedure TScreenOptionsThemes.OnShow;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenOptionsThemes');

  ActualTheme := Ini.Theme;
  ActualSkin := Ini.SkinNo;
  ActualColor := Ini.Color;

  Interaction := 0;
end;

procedure TScreenOptionsThemes.ReloadTheme;
begin
  Theme.LoadTheme(Ini.Theme, Ini.Color);

  ScreenOptionsThemes := TScreenOptionsThemes.create();
  ScreenOptionsThemes.onshow;
  Display.CurrentScreen := @ScreenOptionsThemes;

  ScreenOptionsThemes.Interaction    := self.Interaction;
  ScreenOptionsThemes.Draw;

  Display.Draw;
  SwapBuffers;

  ScreenOptionsThemes.ActualTheme := self.ActualTheme;
  ScreenOptionsThemes.ActualSkin := self.ActualSkin;
  ScreenOptionsThemes.ActualColor := self.ActualColor;

  Self.Destroy;
end;

procedure TScreenOptionsThemes.ReloadScreens;
begin
  // Reload all screens, after Theme changed
  if(ActualTheme <> Ini.Theme) or
    (ActualSkin <> Ini.SkinNo) or
    (ActualColor <> Ini.Color) then
  begin
    UGraphic.UnLoadScreens();
    UGraphic.LoadScreens(USDXVersionStr);
    Ini.Load;
    AudioInputProcessor.UpdateInputDeviceConfig();
  end;
end;

end.
