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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenOptionsSound.pas $
 * $Id: UScreenOptionsSound.pas 3124 2015-08-23 03:31:49Z basisbit $
 *}

unit UScreenOptionsSound;

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
  TScreenOptionsSound = class(TMenu)
  public
    constructor Create; override;
    function ParseInput(PressedKey: Cardinal; CharCode: UCS4Char;
      PressedDown: boolean): boolean; override;
    procedure OnShow; override;
  end;

const
  ID='ID_073';   //for help system

implementation

uses
  UGraphic,
  UHelp,
  ULog,
  UUnicodeUtils,
  SysUtils;

function TScreenOptionsSound.ParseInput(PressedKey: cardinal;
  CharCode: UCS4Char; PressedDown: boolean): boolean;
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
      SDLK_BACKSPACE:
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
        if SelInteraction = 7 then
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
  end
  else if (SelInteraction = 5) then
  begin
    // preview volume, restart the background music so it uses the new volume
    SoundLib.PauseBgMusic;
    SoundLib.StartBgMusic;
  end;
  
end;

constructor TScreenOptionsSound.Create;
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsSound);

  Theme.OptionsSound.SelectSlideVoicePassthrough.showArrows := true;
  Theme.OptionsSound.SelectSlideVoicePassthrough.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsSound.SelectSlideVoicePassthrough, Ini.VoicePassthrough, IVoicePassthroughTranslated);

  Theme.OptionsSound.SelectBackgroundMusic.showArrows := true;
  Theme.OptionsSound.SelectBackgroundMusic.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsSound.SelectBackgroundMusic, Ini.BackgroundMusicOption, IBackgroundMusicTranslated);

  Theme.OptionsSound.SelectClickAssist.showArrows := true;
  Theme.OptionsSound.SelectClickAssist.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsSound.SelectClickAssist, Ini.ClickAssist, IClickAssistTranslated);

  Theme.OptionsSound.SelectBeatClick.showArrows := true;
  Theme.OptionsSound.SelectBeatClick.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsSound.SelectBeatClick, Ini.BeatClick, IBeatClickTranslated);

  Theme.OptionsSound.SelectSlideMusicAutoGain.showArrows := true;
  Theme.OptionsSound.SelectSlideMusicAutoGain.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsSound.SelectSlideMusicAutoGain, Ini.MusicAutoGain, IMusicAutoGainTranslated);

  Theme.OptionsSound.SelectSlidePreviewVolume.showArrows := true;
  Theme.OptionsSound.SelectSlidePreviewVolume.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsSound.SelectSlidePreviewVolume, Ini.PreviewVolume, IPreviewVolumeTranslated);

  Theme.OptionsSound.SelectSlidePreviewFading.showArrows := true;
  Theme.OptionsSound.SelectSlidePreviewFading.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsSound.SelectSlidePreviewFading, Ini.PreviewFading, IPreviewFadingTranslated);

  AddButton(Theme.OptionsSound.ButtonExit);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(20, 5, Theme.Options.Description[OPTIONS_DESC_INDEX_BACK]);

  Interaction := 0;
end;

procedure TScreenOptionsSound.OnShow;
begin
  inherited;
  Interaction := 0;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenOptionsSound');
end;

end.
