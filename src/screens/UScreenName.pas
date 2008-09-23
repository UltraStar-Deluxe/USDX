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

unit UScreenName;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, SysUtils, UThemes;

type
  TScreenName = class(TMenu)
    public
      Goto_SingScreen: Boolean; //If True then next Screen in SingScreen
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

implementation

uses UGraphic, UMain, UIni, UTexture, UCommon;


function TScreenName.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
var
  I:    integer;
SDL_ModState:  Word;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down

    SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);

    // check normal keys
    if (IsAlphaNumericChar(CharCode) or
        {(CharCode in [' ','-','_','!',',','<','/','*','?','''','"']))} IsPunctuationChar(CharCode)) then
    begin
      Button[Interaction].Text[0].Text := Button[Interaction].Text[0].Text + CharCode;
      Exit;
    end;

    // check special keys
    case PressedKey of
      // Templates for Names Mod
      SDLK_F1:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[0] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[0];
         end;
      SDLK_F2:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[1] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[1];
         end;
      SDLK_F3:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[2] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[2];
         end;
      SDLK_F4:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[3] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[3];
         end;
      SDLK_F5:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[4] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[4];
         end;
      SDLK_F6:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[5] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[5];
         end;
      SDLK_F7:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[6] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[6];
         end;
      SDLK_F8:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[7] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[7];
         end;
      SDLK_F9:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[8] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[8];
         end;
      SDLK_F10:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[9] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[9];
         end;
      SDLK_F11:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[10] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[10];
         end;
      SDLK_F12:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[11] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[11];
         end;


      SDLK_BACKSPACE:
        begin
          Button[Interaction].Text[0].DeleteLastL;
        end;

      SDLK_ESCAPE :
        begin
          Ini.SaveNames;
          AudioPlayback.PlaySound(SoundLib.Back);
          if GoTo_SingScreen then
            FadeTo(@ScreenSong)
          else
            FadeTo(@ScreenMain);
        end;

      SDLK_RETURN:
        begin
          for I := 1 to 6 do
            Ini.Name[I-1] := Button[I-1].Text[0].Text;
          Ini.SaveNames;
          AudioPlayback.PlaySound(SoundLib.Start);

          if GoTo_SingScreen then
            FadeTo(@ScreenSing)
          else
            FadeTo(@ScreenLevel);

          GoTo_SingScreen := False;
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

constructor TScreenName.Create;
var
  I:    integer;
begin
  inherited Create;

  LoadFromTheme(Theme.Name);


  for I := 1 to 6 do
    AddButton(Theme.Name.ButtonPlayer[I]);

  Interaction := 0;
end;

procedure TScreenName.onShow;
var
  I:    integer;
begin
  inherited;
  
  for I := 1 to 6 do
    Button[I-1].Text[0].Text := Ini.Name[I-1];

  for I := 1 to PlayersPlay do begin
    Button[I-1].Visible := true;
    Button[I-1].Selectable := true;
  end;

  for I := PlayersPlay+1 to 6 do begin
    Button[I-1].Visible := false;
    Button[I-1].Selectable := false;
  end;

end;

procedure TScreenName.SetAnimationProgress(Progress: real);
var
  I:    integer;
begin
  for I := 1 to 6 do
    Button[I-1].Texture.ScaleW := Progress;
end;

end.
