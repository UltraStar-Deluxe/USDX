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
      SkinSelect: Integer;
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure InteractInc; override;
      procedure InteractDec; override;
  end;

implementation

uses UMain,
     UGraphic,
     USkins,
     SysUtils;

function TScreenOptionsThemes.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    // check normal keys
    case WideCharUpperCase(CharCode)[1] of
      'Q':
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
    UpdateSelectSlideOptions (Theme.OptionsThemes.SelectSkin, SkinSelect, ISkin, Ini.SkinNo);
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
  end;

  ReloadTheme();
end;

constructor TScreenOptionsThemes.Create;
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsThemes);

  AddSelectSlide(Theme.OptionsThemes.SelectTheme, Ini.Theme, ITheme);

  SkinSelect := AddSelectSlide(Theme.OptionsThemes.SelectSkin, Ini.SkinNo, ISkin);

  AddSelectSlide(Theme.OptionsThemes.SelectColor, Ini.Color, IColor);

  AddButton(Theme.OptionsThemes.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);
end;

procedure TScreenOptionsThemes.onShow;
begin
  inherited;

  Interaction := 0;
end;

procedure TScreenOptionsThemes.ReloadTheme;
begin
  Theme.LoadTheme(ThemePath + ITheme[Ini.Theme] + '.ini', Ini.Color);

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
