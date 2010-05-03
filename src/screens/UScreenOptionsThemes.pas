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

unit UScreenOptionsThemes;

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
  TScreenOptionsThemes = class(TMenu)
    private
      procedure ReloadTheme;
    public
      SkinSelect: integer;
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure InteractInc; override;
      procedure InteractDec; override;
  end;

implementation

uses 
  SysUtils,
  UGraphic,
  UMain,
  UPathUtils,
  UUnicodeUtils,
  USkins;

function TScreenOptionsThemes.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
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
          Ini.Save;

          // Reload all screens, after Theme changed
          // Todo : JB - Check if theme was actually changed
          UGraphic.UnLoadScreens();
          UGraphic.LoadScreens();

          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenOptions);
        end;
      SDLK_RETURN:
        begin
          if SelInteraction = 3 then
          begin
            Ini.Save;

            // Reload all screens, after Theme changed
            // Todo : JB - Check if theme was actually changed
            UGraphic.UnLoadScreens();
            UGraphic.LoadScreens();

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
    AddButtonText(20, 5, Theme.Options.Description[7]);
end;

procedure TScreenOptionsThemes.OnShow;
begin
  inherited;

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

  Self.Destroy;
end;

end.
