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

unit UScreenParty;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu,
  sdl2,
  UMusic,
  UThemes;

type
  TScreenParty = class(TMenu)
    public
      TextDescription:     integer;
      TextDescriptionLong: integer;

      constructor Create; override;
      procedure OnShow; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure SetInteraction(Num: integer); override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

implementation

uses
  UGraphic,
  ULanguage,
  UUnicodeUtils;

function TScreenParty.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
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
      SDLK_BACKSPACE : FadeTo(@ScreenMain, SoundLib.Back);
      SDLK_RETURN:
        begin
          if Interaction = 3 then // challenge not available yet
            ScreenPopupError.ShowPopup(Language.Translate('PARTY_MODE_NOT_AVAILABLE'))
          else
            FadeTo(@ScreenPartyOptions, SoundLib.Start);
        end;

      SDLK_DOWN:  InteractInc;
      SDLK_UP:    InteractDec;
      SDLK_RIGHT: InteractNext;
      SDLK_LEFT:  InteractPrev;
    end;
  end;
end;

constructor TScreenParty.Create;
begin
  inherited Create;

  TextDescription := AddText(Theme.Party.TextDescription);
  TextDescriptionLong := AddText(Theme.Party.TextDescriptionLong);

  LoadFromTheme(Theme.Party);

  AddButton(Theme.Party.ButtonClassic);
  AddButton(Theme.Party.ButtonClassicFree);
  AddButton(Theme.Party.ButtonChallenge);
  AddButton(Theme.Party.ButtonTournament);

  Interaction := 0;
end;

procedure TScreenParty.OnShow;
begin
  inherited;

end;

procedure TScreenParty.SetInteraction(Num: integer);
begin
  inherited SetInteraction(Num);
  Text[TextDescription].Text     := Theme.Party.Description[Interaction];
  Text[TextDescriptionLong].Text := Theme.Party.DescriptionLong[Interaction];
end;

procedure TScreenParty.SetAnimationProgress(Progress: real);
begin
  Button[0].Texture.ScaleW := Progress;
  Button[1].Texture.ScaleW := Progress;
  Button[2].Texture.ScaleW := Progress;
end;

end.
