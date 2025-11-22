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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenLevel.pas $
 * $Id: UScreenLevel.pas 1975 2009-12-06 14:40:10Z s_alexander $
 *}

unit UScreenLevel;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UDisplay,
  UFiles,
  UMenu,
  UMusic,
  UThemes,
  sdl2,
  SysUtils;

type
  TScreenLevel = class(TMenu)
    public
      constructor Create; override;
      function ParseInput(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean; override;
      procedure OnShow; override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

const
  ID='ID_011';   //for help system

implementation

uses
  UGraphic,
  UHelp,
  UIni,
  ULog,
  UMain,
  USong,
  UTexture,
  UUnicodeUtils;

function TScreenLevel.ParseInput(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
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
          AudioPlayback.PlaySound(SoundLib.Back);

          if Ini.OnSongClick = sSelectPlayer then
            FadeTo(@ScreenMain)
          else
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
begin
  inherited Create;

  LoadFromTheme(Theme.Level);

  AddButton(Theme.Level.ButtonEasy);
  AddButton(Theme.Level.ButtonMedium);
  AddButton(Theme.Level.ButtonHard);

  Interaction := 0;
end;

procedure TScreenLevel.OnShow;
begin
  inherited;

  Interaction := Ini.Difficulty;
  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenLevel');
end;

procedure TScreenLevel.SetAnimationProgress(Progress: real);
begin
  Button[0].Texture.ScaleW := Progress;
  Button[1].Texture.ScaleW := Progress;
  Button[2].Texture.ScaleW := Progress;
end;

end.
