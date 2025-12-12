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
 * $URL: $
 * $Id: $
 *}

unit UScreenOptionsInput;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UCommon,
  UIni,
  UJoystick,
  UMenu,
  UMusic,
  UThemes,
  sdl2;

type

  TByteSet = set of byte;

  TScreenOptionsInput = class(TOptionsMenu)
    protected
      // interaction IDs
      ButtonExitIID: integer;
      SelectMouse: integer;
      SelectJoyPad: integer;

      // values
      ActualMouse: integer;

      SoundInteractions: TByteSet;

      WasJoy: boolean;
      WasMouse: boolean;

    protected
      procedure CheckOption;
      procedure LoadWidgets; override;

    public
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; Repeated: boolean = false): boolean; override;

      procedure OnShow; override;
      procedure OnHide; override;

  end;

const
  ID='ID_074';   //for help system

implementation

uses
  UDisplay,
  UGraphic,
  UHelp,
  ULanguage,
  ULog,
  UUnicodeUtils,
  SysUtils;

function TScreenOptionsInput.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; Repeated: boolean = false): boolean;
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
          if Interaction = ButtonExitIID then
          begin
            Ini.Save;
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
          if (Interaction in SoundInteractions) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractInc;
          end;
          CheckOption;
        end;
      SDLK_LEFT:
        begin
          if (Interaction in SoundInteractions) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
          end;
          CheckOption;
        end;
    end;
  end;
end;

constructor TScreenOptionsInput.Create;
begin
  inherited Create;
  SoundInteractions := [];
  Description := Language.Translate('SING_OPTIONS_INPUT_DESC');
  WhereAmI := Language.Translate('SING_OPTIONS_INPUT_WHEREAMI');
  Load;
  ButtonExitIID := High(Interactions);

end;

procedure TScreenOptionsInput.OnShow;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenOptionsInput)');

  ActualMouse := Ini.Mouse;
  UpdateSelectSlideOptions(SelectMouse, IMouseTranslated, ActualMouse);

  WasMouse := Ini.Mouse > 0;
  WasJoy := HasJoyStick;
  Interaction := 0;
end;

procedure TScreenOptionsInput.OnHide;
begin
  inherited;

  // TODO: query confirm whether to disable mouse
  // TODO: RattleSN4K3: query confirm whether to disable Joypad (requires interaction mode, Controller/Mouse/Keyboard)

  // update actual Mouse configuration
  Ini.Mouse := ActualMouse;
  Display.SetCursor;

  // re-initialize or remove joypad support
  if WasJoy and (Ini.Joypad = 0) then begin
    FinalizeJoyStick;
  end else if not WasJoy and (Ini.Joypad = 1) then
    InitializeJoystick;

end;

procedure TScreenOptionsInput.CheckOption;
begin
  inherited;

  if (Interaction = SelectMouse) then
  begin
    if ActualMouse > 0 then Ini.Mouse := ActualMouse
    else Ini.Mouse := 1;
    Display.SetCursor;
    Display.UpdateCursor;
  end;

end;

procedure TScreenOptionsInput.LoadWidgets;
begin
  SelectMouse := AddSelectSlide('SING_OPTIONS_INPUT_MOUSE', ActualMouse, IMouseTranslated);
  Include(SoundInteractions, SelectMouse);
  SelectJoyPad := AddSelectSlide('SING_OPTIONS_INPUT_JOYPAD_SUPPORT', Ini.Joypad, IJoypad);
  Include(SoundInteractions, SelectJoyPad);
end;

end.
