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

unit UScreenAbout;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UDisplay,
  UIni,
  UMenu,
  UMusic,
  UThemes,
  sdl2,
  SysUtils;

type
  TScreenAbout = class(TMenu)
    public
      TextOverview:    integer;
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure SetAnimationProgress(Progress: real); override;

      procedure SetOverview;
  end;

const
  ID='ID_002';   //for help system

implementation

uses
  UCommon,
  UDataBase,
  UGraphic,
  UHelp,
  ULanguage,
  ULog,
  USong,
  USongs,
  UUnicodeUtils,
  Classes;

function TScreenAbout.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check normal keys
    case UCS4UpperCase(CharCode) of
      Ord('C'):
        begin
          FadeTo(@ScreenCredits, SoundLib.Start);
          Exit;
        end;

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
          FadeTo(@ScreenMain);
        end;
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;
      SDLK_RETURN:
        begin
          //Exit Button Pressed
          if Interaction = 1 then
          begin
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(@ScreenMain);
          end;

          // ultrastar deluxe team credits
          if Interaction = 0 then
          begin
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(@ScreenCredits);
          end;
        end;
      SDLK_LEFT:
      begin
          InteractPrev;
      end;
      SDLK_RIGHT:
      begin
          InteractNext;
      end;
      SDLK_UP:
      begin
          InteractPrev;
      end;
      SDLK_DOWN:
      begin
          InteractNext;
      end;
    end;
  end;
end;

constructor TScreenAbout.Create;
begin
  inherited Create;

  TextOverview := AddText(Theme.AboutMain.TextOverview);

  LoadFromTheme(Theme.AboutMain);

  AddButton(Theme.AboutMain.ButtonCredits);
  AddButton(Theme.AboutMain.ButtonExit);

  Interaction := 0;
end;

procedure TScreenAbout.OnShow;
begin
  inherited;

  if not Help.SetHelpID(ID) then
      Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenAbout)');

  //Set Overview Text:
  SetOverview;
end;

procedure TScreenAbout.SetOverview;
var
  Overview: UTF8String;
begin
  // Format overview
  Overview := Language.Translate('ABOUT_OVERVIEW');
  Text[0].Text := Overview;
end;

procedure TScreenAbout.SetAnimationProgress(Progress: real);
var
  I: integer;
begin
  for I := 0 to high(Button) do
    Button[I].Texture.ScaleW := Progress;
end;

end.
