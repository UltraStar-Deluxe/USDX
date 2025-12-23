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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/screens/UScreenOptionsGraphics.pas $
 * $Id: UScreenOptionsGraphics.pas 2338 2010-05-03 21:58:30Z k-m_schindler $
 *}

unit UScreenOptionsGraphics;

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
  TScreenOptionsGraphics = class(TOptionsMenu)
    private
      SelectWindowMode:    cardinal;
      SelectResolution:    cardinal;

      IResolutionEmpty:    array of UTF8String;
      ResolutionEmpty:     integer; // not used, only to prevent changing original by-ref passed variable

      OldWindowMode:       integer;

      procedure UpdateWindowMode;
      procedure UpdateResolution;

    public
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; Repeated: boolean = false): boolean; override;
      procedure OnShow; override;
      procedure OnHide; override;
      procedure OnWindowResized; override;

    protected
      procedure LoadWidgets; override;
  end;

const
  ID='ID_072';   //for help system

implementation

uses
  UGraphic,
  UHelp,
  ULanguage,
  ULog,
  UUnicodeUtils,
  SysUtils;

function TScreenOptionsGraphics.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; Repeated: boolean = false): boolean;
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
          if SelInteraction = 6 then
          begin
            Ini.Save;
            AudioPlayback.PlaySound(SoundLib.Back);

            if OldWindowMode <> Ini.FullScreen then UGraphic.UpdateVideoMode()
            else UGraphic.UpdateResolution();

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

          if (Interaction = SelectWindowMode) then
          begin
            UpdateResolution;
          end;

        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction < 6) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
          end;

          if (Interaction = SelectWindowMode) then
          begin
            UpdateResolution;
          end;
        end;
    end;
  end;
end;

constructor TScreenOptionsGraphics.Create;
begin
  inherited Create;
  ResolutionEmpty := 0;
  SetLength(IResolutionEmpty, 1);
  IResolutionEmpty[0] := '---';
  Description := Language.Translate('SING_OPTIONS_GRAPHICS_DESC');
  WhereAmI := Language.Translate('SING_OPTIONS_GRAPHICS_WHEREAMI');
  Load;
end;

procedure TScreenOptionsGraphics.OnShow;
begin
  inherited;

  if CurrentWindowMode = Mode_Windowed then Ini.SetResolution(ScreenW, ScreenH);

  UpdateWindowMode();
  UpdateResolution();

  Interaction := 0;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenOptionsGraphics');
end;

procedure TScreenOptionsGraphics.OnHide;
begin
  inherited;
  Ini.ClearCustomResolutions();
end;

procedure TScreenOptionsGraphics.OnWindowResized;
begin
  inherited;

  UpdateWindowMode;

  if CurrentWindowMode = Mode_Windowed then Ini.SetResolution(ScreenW, ScreenH);
  UpdateResolution;

end;

procedure TScreenOptionsGraphics.UpdateWindowMode;
begin

  UpdateSelectSlideOptions(SelectWindowMode, IFullScreenTranslated, Ini.FullScreen);
  OldWindowMode := integer(Ini.FullScreen);
end;

procedure TScreenOptionsGraphics.UpdateResolution;
begin

  if Ini.Fullscreen = 2 then
    UpdateSelectSlideOptions(SelectResolution, IResolutionEmpty, ResolutionEmpty)
  else if Ini.Fullscreen = 1 then
    UpdateSelectSlideOptions(SelectResolution, IResolutionFullScreen, Ini.ResolutionFullscreen)
  else
    UpdateSelectSlideOptions(SelectResolution, IResolution, Ini.Resolution);

end;

procedure TScreenOptionsGraphics.LoadWidgets;
begin
  SelectWindowMode := AddSelectSlide('SING_OPTIONS_GRAPHICS_FULLSCREEN', Ini.Fullscreen, IFullScreenTranslated);
  SelectResolution := AddSelectSlide('SING_OPTIONS_GRAPHICS_RESOLUTION', Ini.Resolution, IResolution);
  AddSelectSlide('SING_OPTIONS_GRAPHICS_DEPTH', Ini.Depth, IDepth);
  AddSelectSlide('SING_OPTIONS_GRAPHICS_VISUALIZER', Ini.VisualizerOption, IVisualizerTranslated);
  AddSelectSlide('SING_OPTIONS_GRAPHICS_OSCILLOSCOPE', Ini.Oscilloscope, IOscilloscopeTranslated);
  AddSelectSlide('SING_OPTIONS_GRAPHICS_MOVIE_SIZE', Ini.MovieSize, IMovieSizeTranslated);
end;

end.
