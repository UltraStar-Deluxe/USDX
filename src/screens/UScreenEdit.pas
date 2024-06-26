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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenEdit.pas $
 * $Id: UScreenEdit.pas 2246 2010-04-18 13:43:36Z tobigun $
 *}

unit UScreenEdit;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu,
  UThemes,
  sdl2;

type
  TScreenEdit = class(TMenu)
    public
      TextDescription:     integer;
      TextDescriptionLong: integer;

      constructor Create; override;
      procedure OnShow; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure SetInteraction(Num: integer); override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

const
  ID='ID_060';   //for help system

implementation

uses
  UGraphic,
  UHelp,
  ULog,
  UMusic,
  USkins,
  UUnicodeUtils,
  SysUtils;

function TScreenEdit.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  SDL_ModState: word;
begin
  Result := true;

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT +
    KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT + KMOD_RALT);

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
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenMain);
        end;
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;
      SDLK_RETURN:
        begin
          if Interaction = 0 then
          begin
            AudioPlayback.PlaySound(SoundLib.Start);
            FadeTo(@ScreenEditConvert);
          end;

          if Interaction = 1 then
          begin
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(@ScreenMain);
          end;
        end;

      SDLK_DOWN:  InteractInc;
      SDLK_UP:    InteractDec;
      SDLK_RIGHT: InteractNext;
      SDLK_LEFT:  InteractPrev;
    end;
  end;
end;

constructor TScreenEdit.Create;
begin
  inherited Create;

  TextDescription := AddText(Theme.Edit.TextDescription);

  LoadFromTheme(Theme.Edit);

  AddButton(Theme.Edit.ButtonConvert);
{ Some ideas for more:
  AddButton(Theme.Edit.ButtonEditHeaders);
  AddButton(Theme.Edit.ButtonAdjustGap);
}
  AddButton(Theme.Edit.ButtonExit);

  Interaction := 0;
end;

procedure TScreenEdit.OnShow;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenEdit');

  // continue possibly stopped bg-music (stopped in midi import screen)
  SoundLib.StartBgMusic;
end;

procedure TScreenEdit.SetInteraction(Num: integer);
begin
  inherited SetInteraction(Num);
  Text[TextDescription].Text     := Theme.Edit.Description[Interaction];

  // long description not used atm // IMPROVE: Theme
  //Text[TextDescriptionLong].Text := Theme.Edit.DescriptionLong[Interaction];
end;

procedure TScreenEdit.SetAnimationProgress(Progress: real);
begin
  Statics[0].Texture.ScaleW := Progress;
  Statics[0].Texture.ScaleH := Progress;
end;

end.
