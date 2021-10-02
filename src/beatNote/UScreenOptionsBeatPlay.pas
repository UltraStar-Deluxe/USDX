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

unit UScreenOptionsBeatPlay;

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

// Class definition for the options screen for the tapping (accessible through
// Tools -> Options -> Beat Tapping in the english version
type
  TScreenOptionsBeatPlay = class(TMenu)
    private
      BeatDetectionDelayOptInt:integer; // Value for Keyboard delay. Private because we only store the float value which is 10x this
      BeatDetectionDelaySelectNum: integer; // This is the reference number of the graphical element
      ButtonConfigureID: integer;
    public
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure UpdateCalculatedSelectSlides(Init: boolean); // For showing suitable text to choose
  end;



implementation

uses
  UGraphic,
  UHelp,
  ULog,
  UUnicodeUtils,
  SysUtils,
  UCommon;

type
TGetTextFunc = function(var Param: integer; Offset: integer; Modify: boolean; OptText: PUtf8String): boolean;
UTF8StringArray = array of UTF8String;

function TScreenOptionsBeatPlay.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
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
      SDLK_TAB:
      begin
        ScreenPopupHelp.ShowPopup();
      end;
      SDLK_RETURN:
        begin
          if SelInteraction = 2 then
          begin
            Ini.Save;
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(@ScreenOptions);
          end;
          if SelInteraction = 3 then
          begin
            Ini.Save;
            FadeTo(@ScreenOptionsBeatPlayPeakAnalysis);
          end;
        end;
      SDLK_DOWN:
        InteractNext;
      SDLK_UP :
        InteractPrev;
      SDLK_RIGHT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 4) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractInc;
          end;
          UpdateCalculatedSelectSlides(false);
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 4) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
          end;
          UpdateCalculatedSelectSlides(false);
        end;
    end;
  end;
end;

constructor TScreenOptionsBeatPlay.Create;
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsBeatPlay);

  Theme.OptionsBeatPlay.SelectBeatDetectionDelay.oneItemOnly := true;
  Theme.OptionsBeatPlay.SelectBeatDetectionDelay.showArrows := true;


  Theme.OptionsBeatPlay.SelectBeatPlayClapSign.showArrows := true;
  Theme.OptionsBeatPlay.SelectBeatPlayClapSign.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsBeatPlay.SelectBeatPlayClapSign, Ini.BeatPlayClapSignOn, IBeatPlayClapSignOn);

  UpdateCalculatedSelectSlides(true); // Instantiate the calculated slides

  AddButton(Theme.OptionsAdvanced.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(20, 5, Theme.Options.Description[OPTIONS_DESC_INDEX_BACK]);

  AddButton(Theme.OptionsBeatPlay.ButtonAudioConfigure);
  if (Length(Button[1].Text)=0) then
    AddButtonText(20, 5, Theme.OptionsBeatPlay.Description[0]);







  Interaction := 0;



end;

procedure TScreenOptionsBeatPlay.OnShow;
begin
  inherited;

  Interaction := 0;

end;

function GetBeatDetectionDelayOptText(var Param: integer; Offset: integer; Modify: boolean; OptText: PUTF8String): boolean;
begin
  if (Param + Offset * 10 < -1000) or (Param + Offset * 10 > 1000) then
    Result := false
  else
  begin
    if OptText <> nil then
      OptText^ := Format('%d ms', [Param + Offset * 10]);
    if Modify then
      Param := Param + Offset * 10;
    Result := true;
  end;
end;

function GetBeatChoiceOptText(var Param: integer; Offset: integer; Modify: boolean; OptText: PUTF8String): boolean;
begin
  if (Param + Offset * 10 < -1000) or (Param + Offset * 10 > 1000) then
    Result := false
  else
  begin
    if OptText <> nil then
      OptText^ := Format('%d ms', [Param + Offset * 10]);
    if Modify then
      Param := Param + Offset * 10;
    Result := true;
  end;
end;


procedure CalculateSelectSlide(Init: boolean; GetText: TGetTextFunc; var Param: integer; var OptInt: integer; var Texts: UTF8StringArray);
var
  Idx: integer;
  NumOpts: integer;
begin
  if not GetText(Param, 0, true, nil) then
  begin
    SetLength(Texts, 0);
    Exit;
  end;
  if GetText(Param, -1, false, nil) then
    Idx := 1
  else
    Idx := 0;
  if not Init then
  begin
    if OptInt = Idx then
      Exit;
    GetText(Param, OptInt - Idx, true, nil);
    if GetText(Param, -1, false, nil) then
      Idx := 1
    else
      Idx := 0;
  end;
  OptInt := Idx;
  if GetText(Param, 1, false, nil) then
    NumOpts := Idx + 2
  else
    NumOpts := Idx + 1;
  SetLength(Texts, NumOpts);
  for Idx := 0 to High(Texts) do
    GetText(Param, Idx - OptInt, false, @Texts[Idx]);
end;

procedure TScreenOptionsBeatPlay.UpdateCalculatedSelectSlides(Init: boolean);
begin
  CalculateSelectSlide(Init, @GetBeatDetectionDelayOptText, Ini.BeatDetectionDelay, BeatDetectionDelayOptInt, IBeatDetectionDelay);
  if Init then
  begin
    BeatDetectionDelaySelectNum := AddSelectSlide(Theme.OptionsBeatPlay.SelectBeatDetectionDelay, BeatDetectionDelayOptInt, IBeatDetectionDelay);
  end
  else
  begin
    UpdateSelectSlideOptions(Theme.OptionsBeatPlay.SelectBeatDetectionDelay, BeatDetectionDelaySelectNum, IBeatDetectionDelay, BeatDetectionDelayOptInt);
  end;
end;




end.
