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

unit UScreenOptionsGraphics;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu,
  SDL,
  UDisplay,
  UMusic,
  UFiles,
  UIni,
  UThemes;

type
  TScreenOptionsGraphics = class(TMenu)
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

function TScreenOptionsGraphics.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
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
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenOptions);
        end;
      SDLK_RETURN:
        begin
          if SelInteraction = 6 then
          begin
            Ini.Save;
            AudioPlayback.PlaySound(SoundLib.Back);
            // FIXME: changing the video mode does not work this way in windows
            // and MacOSX as all textures will be invalidated through this.
            // See the ALT+TAB code too.
            {$IF Defined(Linux) or Defined(FreeBSD)}
            Reinitialize3D();
            {$IFEND}
            FadeTo(@ScreenOptions);
          end;
        end;
      SDLK_DOWN:
        InteractNext;
      SDLK_UP :
        InteractPrev;
      SDLK_RIGHT:
        begin
          if (SelInteraction >= 0) and (SelInteraction < 6) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractInc;
          end;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction < 6) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
          end;
        end;
    end;
  end;
end;

constructor TScreenOptionsGraphics.Create;
begin
  inherited Create;
  LoadFromTheme(Theme.OptionsGraphics);

  Theme.OptionsGraphics.SelectResolution.showArrows := true;
  Theme.OptionsGraphics.SelectResolution.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsGraphics.SelectResolution,   Ini.Resolution, IResolution);

  Theme.OptionsGraphics.SelectFullscreen.showArrows := true;
  Theme.OptionsGraphics.SelectFullscreen.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsGraphics.SelectFullscreen,   Ini.Fullscreen, IFullScreenTranslated);

  Theme.OptionsGraphics.SelectDepth.showArrows := true;
  Theme.OptionsGraphics.SelectDepth.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsGraphics.SelectDepth,        Ini.Depth, IDepth);

  Theme.OptionsGraphics.SelectVisualizer.showArrows := true;
  Theme.OptionsGraphics.SelectVisualizer.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsGraphics.SelectVisualizer,   Ini.VisualizerOption, IVisualizerTranslated);

  Theme.OptionsGraphics.SelectOscilloscope.showArrows := true;
  Theme.OptionsGraphics.SelectOscilloscope.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsGraphics.SelectOscilloscope, Ini.Oscilloscope, IOscilloscopeTranslated);

  Theme.OptionsGraphics.SelectMovieSize.showArrows := true;
  Theme.OptionsGraphics.SelectMovieSize.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsGraphics.SelectMovieSize,    Ini.MovieSize, IMovieSizeTranslated);

  AddButton(Theme.OptionsGraphics.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(20, 5, Theme.Options.Description[7]);

end;

procedure TScreenOptionsGraphics.OnShow;
begin
  inherited;

  Interaction := 0;
end;

end.
