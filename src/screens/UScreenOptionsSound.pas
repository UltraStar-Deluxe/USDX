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

unit UScreenOptionsSound;

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
  TScreenOptionsSound = class(TMenu)
  public
    constructor Create; override;
    function ParseInput(PressedKey: cardinal; CharCode: widechar;
      PressedDown: boolean): boolean; override;
    procedure onShow; override;
  end;

implementation

uses
  UGraphic,
  SysUtils;

function TScreenOptionsSound.ParseInput(PressedKey: cardinal;
  CharCode: widechar; PressedDown: boolean): boolean;
begin
  Result := true;
  if (PressedDown) then
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
      SDLK_BACKSPACE:
      begin
        // Escape -> save nothing - just leave this screen
        AudioPlayback.PlaySound(SoundLib.Back);
        FadeTo(@ScreenOptions);
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
      SDLK_UP:
        InteractPrev;
      SDLK_RIGHT:
      begin
        if (SelInteraction >= 0) and (SelInteraction < 8) then
        begin
          AudioPlayback.PlaySound(SoundLib.Option);
          InteractInc;
        end;
      end;
      SDLK_LEFT:
      begin
        if (SelInteraction >= 0) and (SelInteraction < 8) then
        begin
          AudioPlayback.PlaySound(SoundLib.Option);
          InteractDec;
        end;
      end;
    end;
  end;

{**
 * Actually this one isn't pretty - but it does the trick of
 * turning the background music on/off in "real time"
 * bgm = background music
 * TODO: - Fetching the SelectInteraction via something more descriptive
 *       - Obtaining the current value of a select is imho ugly
 *}
  if (SelInteraction = 1) then
  begin
    if TBackgroundMusicOption(SelectsS[1].SelectedOption) = bmoOn then
      SoundLib.StartBgMusic
    else
      SoundLib.PauseBgMusic;
  end;
  
end;

constructor TScreenOptionsSound.Create;
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsSound);

  AddSelectSlide(Theme.OptionsSound.SelectSlideVoicePassthrough,
    Ini.VoicePassthrough, IVoicePassthrough);
  AddSelectSlide(Theme.OptionsSound.SelectBackgroundMusic,
    Ini.BackgroundMusicOption, IBackgroundMusic);
  AddSelectSlide(Theme.OptionsSound.SelectMicBoost, Ini.MicBoost, IMicBoost);
  // TODO: - MicBoost needs to be moved to ScreenOptionsRecord
  AddSelectSlide(Theme.OptionsSound.SelectClickAssist, Ini.ClickAssist, IClickAssist);
  AddSelectSlide(Theme.OptionsSound.SelectBeatClick, Ini.BeatClick, IBeatClick);
  AddSelectSlide(Theme.OptionsSound.SelectThreshold, Ini.ThresholdIndex, IThreshold);
  AddSelectSlide(Theme.OptionsSound.SelectSlidePreviewVolume,
    Ini.PreviewVolume, IPreviewVolume);
  AddSelectSlide(Theme.OptionsSound.SelectSlidePreviewFading,
    Ini.PreviewFading, IPreviewFading);

  AddButton(Theme.OptionsSound.ButtonExit);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);

  Interaction := 0;
end;

procedure TScreenOptionsSound.onShow;
begin
  inherited;
  Interaction := 0;
end;

end.
