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

unit UScreenLevel;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, SysUtils, UThemes;

type
  TScreenLevel = class(TMenu)
    public
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

implementation

uses UGraphic,
     UMain,
     UIni,
     USong,
     UTexture;

function TScreenLevel.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
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
      SDLK_BACKSPACE :
        begin
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenName);
        end;

      SDLK_RETURN:
        begin
          Ini.Difficulty := Interaction;
          Ini.SaveLevel;
          AudioPlayback.PlaySound(SoundLib.Start);
          //Set Standard Mode
          ScreenSong.Mode := smNormal;
          FadeTo(@ScreenSong);
        end;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:    InteractNext;
      SDLK_UP:      InteractPrev;
      SDLK_RIGHT:   InteractNext;
      SDLK_LEFT:    InteractPrev;
    end;
  end;
end;

constructor TScreenLevel.Create;
//var
// I:    integer; // Auto Removed, Unused Variable
begin
  inherited Create;

  LoadFromTheme(Theme.Level);

  AddButton(Theme.Level.ButtonEasy);
  AddButton(Theme.Level.ButtonMedium);
  AddButton(Theme.Level.ButtonHard);

  Interaction := 0;
end;

procedure TScreenLevel.onShow;
begin
  inherited;

  Interaction := Ini.Difficulty;
end;

procedure TScreenLevel.SetAnimationProgress(Progress: real);
begin
  Button[0].Texture.ScaleW := Progress;
  Button[1].Texture.ScaleW := Progress;
  Button[2].Texture.ScaleW := Progress;
end;

end.
