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

unit UScreenOptionsKeyPlay;

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
  TScreenOptionsKeyPlay = class(TMenu)
    private
      KeyboardDelayOptInt:integer; // Value for Keyboard delay. Private because we only store the float value which is 10x this
      KeyboardDelaySelectNum: integer; // This is the reference number of the graphical element
      SelectLetterID: integer; // the presently selected leeter
      SelectLetterIDGraphicalNum: integer;  // Again, the graphical reference number
      ButtonConfigureID: integer;
    public
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure UpdateCalculatedSelectSlides(Init: boolean); // For showing suitable text to choose
      procedure UpdateLetterSelection(); // Ensure that the letter shown corresponds to the player
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

function TScreenOptionsKeyPlay.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
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
          if SelInteraction = 5 then
          begin
            Ini.Save;
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(@ScreenOptions);
          end;
          if SelInteraction = 6 then
          begin
            Ini.Save;
            FadeTo(@ScreenOptionsKeyPlayPeakAnalysis);
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
          if SelInteraction = 1 then // Player selected, update letters from known map
             UpdateLetterSelection();
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 4) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
          end;
          UpdateCalculatedSelectSlides(false);
          if SelInteraction = 1 then // Player selected, update letters from known map
             UpdateLetterSelection();
        end;
    end;
  end;
end;

constructor TScreenOptionsKeyPlay.Create;
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsKeyPlay);

  Theme.OptionsKeyPlay.SelectKeyPlayOn.showArrows := true;
  Theme.OptionsKeyPlay.SelectKeyPlayOn.oneItemOnly := true;
  Theme.OptionsKeyPlay.SelectKeyboardDelay.oneItemOnly := true;
  Theme.OptionsKeyPlay.SelectKeyboardDelay.showArrows := true;
  AddSelectSlide(Theme.OptionsKeyPlay.SelectKeyPlayOn, Ini.KeyPlayOn, IKeyPlayOn);
  AddSelectSlide(Theme.OptionsKeyPlay.SelectPlayer, Ini.KeyPlayPlayerSelected, IKeyPlayPlayers);

  SelectLetterIDGraphicalNum:=AddSelectSlide(Theme.OptionsKeyPlay.SelectLetter,
        SelectLetterID, IKeyPlayLetters);

  Theme.OptionsKeyPlay.SelectKeyPlayClapSign.showArrows := true;
  Theme.OptionsKeyPlay.SelectKeyPlayClapSign.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsKeyPlay.SelectKeyPlayClapSign, Ini.KeyPlayClapSignOn, IKeyPlayClapSignOn);

  UpdateCalculatedSelectSlides(true); // Instantiate the calculated slides

  AddButton(Theme.OptionsAdvanced.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(20, 5, Theme.Options.Description[OPTIONS_DESC_INDEX_BACK]);

  AddButton(Theme.OptionsKeyPlay.ButtonAudioConfigure);
  if (Length(Button[1].Text)=0) then
    AddButtonText(20, 5, Theme.OptionsKeyPlay.Description[0]);







  Interaction := 0;

  UpdateLetterSelection();

end;

procedure TScreenOptionsKeyPlay.OnShow;
begin
  inherited;

  Interaction := 0;

end;

function GetKeyDelayOptText(var Param: integer; Offset: integer; Modify: boolean; OptText: PUTF8String): boolean;
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

function GetKeyChoiceOptText(var Param: integer; Offset: integer; Modify: boolean; OptText: PUTF8String): boolean;
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

procedure TScreenOptionsKeyPlay.UpdateCalculatedSelectSlides(Init: boolean);
begin
  CalculateSelectSlide(Init, @GetKeyDelayOptText, Ini.KeyboardDelay, KeyboardDelayOptInt, IKeyboardDelay);
  if Init then
  begin
    KeyboardDelaySelectNum := AddSelectSlide(Theme.OptionsKeyPlay.SelectKeyboardDelay, KeyboardDelayOptInt, IKeyboardDelay);
  end
  else
  begin
    UpdateSelectSlideOptions(Theme.OptionsKeyPlay.SelectKeyboardDelay, KeyboardDelaySelectNum, IKeyboardDelay, KeyboardDelayOptInt);
  end;
end;

procedure TScreenOptionsKeyPlay.UpdateLetterSelection();
begin
  UpdateSelectSlideOptions(Theme.OptionsKeyPlay.SelectLetter,SelectLetterIDGraphicalNum,IKeyPlayLetters,Ini.PlayerKeys[Ini.KeyPlayPlayerSelected]);

end;


end.
