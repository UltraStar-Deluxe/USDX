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
  TScreenOptionsSound = class(TOptionsMenu)
  private
    function AddVolumeSlider(const Text: UTF8String; var Data: integer): integer;
    procedure ApplyRealtimeSoundSettings;
  public
    constructor Create; override;
    function ParseInput(PressedKey: Cardinal; CharCode: UCS4Char;
      PressedDown: boolean): boolean; override;
    function ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean; override;
    procedure OnShow; override;
  protected
    procedure LoadWidgets; override;
  end;

const
  ID='ID_073';   //for help system

implementation

uses
  UGraphic,
  UHelp,
  ULanguage,
  ULog,
  UUnicodeUtils,
  SysUtils,
  Math;

var
  VolumePercentOptions: array of UTF8String;

procedure EnsureVolumePercentOptions;
var
  I: integer;
begin
  if Length(VolumePercentOptions) > 0 then
    Exit;

  SetLength(VolumePercentOptions, 101);
  for I := 0 to High(VolumePercentOptions) do
    VolumePercentOptions[I] := IntToStr(I) + '%';
end;

function TScreenOptionsSound.ParseInput(PressedKey: cardinal;
  CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    case PressedKey of
      SDLK_Q:
      begin
        Result := false;
        Exit;
      end;
    end;

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
        if (SelInteraction >= 0) and (SelInteraction <= High(Interactions)) and
           (Interactions[SelInteraction].Typ = iButton) then
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
        if (SelInteraction >= 0) and (SelInteraction <= High(Interactions)) and
            (Interactions[SelInteraction].Typ = iSelectS) then
        begin
          AudioPlayback.PlaySound(SoundLib.Option);
          InteractInc;
        end;
      end;
      SDLK_LEFT:
      begin
        if (SelInteraction >= 0) and (SelInteraction <= High(Interactions)) and
            (Interactions[SelInteraction].Typ = iSelectS) then
        begin
          AudioPlayback.PlaySound(SoundLib.Option);
          InteractDec;
        end;
      end;
    end;
  end;

  ApplyRealtimeSoundSettings;
end;

function TScreenOptionsSound.ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean;
begin
  Result := inherited ParseMouse(MouseButton, BtnDown, X, Y);
  ApplyRealtimeSoundSettings;
end;

constructor TScreenOptionsSound.Create;
begin
  inherited Create;
  Description := Language.Translate('SING_OPTIONS_SOUND_DESC');
  WhereAmI := Language.Translate('SING_OPTIONS_SOUND_WHEREAMI');
  Load;
end;

procedure TScreenOptionsSound.OnShow;
begin
  inherited;
  Interaction := 0;
  ApplyRealtimeSoundSettings;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenOptionsSound');
end;

procedure TScreenOptionsSound.LoadWidgets;
begin
  AddSelectSlide('SING_OPTIONS_SOUND_VOICEPASSTHROUGH', Ini.VoicePassthrough, IVoicePassthroughTranslated);
  AddSelectSlide('SING_OPTIONS_SOUND_BACKGROUNDMUSIC', Ini.BackgroundMusicOption, IBackgroundMusicTranslated);
  AddVolumeSlider('SING_OPTIONS_SOUND_BACKGROUNDMUSICVOLUME', Ini.BackgroundMusicVolume);
  AddSelectSlide('SING_OPTIONS_SOUND_CLICK_ASSIST', Ini.ClickAssist, IClickAssistTranslated);
  AddSelectSlide('SING_OPTIONS_SOUND_BEAT_CLICK', Ini.BeatClick, IBeatClickTranslated);
  AddSelectSlide('SING_OPTIONS_SOUND_MUSICAUTOGAIN', Ini.ReplayGain, IReplayGainTranslated);
  AddVolumeSlider('SING_OPTIONS_SOUND_AUDIOVOLUME', Ini.AudioVolume);
  AddVolumeSlider('SING_OPTIONS_SOUND_VOCALSVOLUME', Ini.VocalsVolume);
  AddVolumeSlider('SING_OPTIONS_SOUND_SFXVOLUME', Ini.SfxVolume);
  AddVolumeSlider('SING_OPTIONS_SOUND_PREVIEWVOLUME', Ini.PreviewVolume);
  AddSelectSlide('SING_OPTIONS_SOUND_PREVIEWFADING', Ini.PreviewFading, IPreviewFadingTranslated);
end;

function TScreenOptionsSound.AddVolumeSlider(const Text: UTF8String; var Data: integer): integer;
begin
  EnsureVolumePercentOptions;
  Data := EnsureRange(Data, 0, High(VolumePercentOptions));
  Result := AddSelectSlide(Text, Data, VolumePercentOptions);
end;

procedure TScreenOptionsSound.ApplyRealtimeSoundSettings;
var
  MusicOptionValue: integer;
begin
  SetAudioVolumePercent(Ini.AudioVolume);

  SetVocalsVolumePercent(Ini.VocalsVolume);

  SetSfxVolumePercent(Ini.SfxVolume);

  SetBackgroundMusicVolumePercent(Ini.BackgroundMusicVolume);

  SetPreviewVolumePercent(Ini.PreviewVolume);

  MusicOptionValue := EnsureRange(Ini.BackgroundMusicOption,
    Ord(Low(TBackgroundMusicOption)), Ord(High(TBackgroundMusicOption)));
  if TBackgroundMusicOption(MusicOptionValue) = bmoOn then
    SoundLib.StartBgMusic
  else
    SoundLib.PauseBgMusic;
end;

end.
