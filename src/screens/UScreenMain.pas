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

unit UScreenMain;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu,
  SDL,
  UDisplay,
  UMusic,
  UFiles,
  SysUtils,
  UThemes;

type
  TScreenMain = class(TMenu)
  public
    TextDescription:     integer;
    TextDescriptionLong: integer;

    constructor Create; override;
    function ParseInput(PressedKey: cardinal; CharCode: widechar;
      PressedDown: boolean): boolean; override;
    procedure onShow; override;
    procedure InteractNext; override;
    procedure InteractPrev; override;
    procedure InteractInc; override;
    procedure InteractDec; override;
    procedure SetAnimationProgress(Progress: real); override;
  end;

implementation

uses
  UGraphic,
  UMain,
  UIni,
  UTexture,
  USongs,
  Textgl,
  ULanguage,
  UParty,
  UDLLManager,
  UScreenCredits,
  USkins;

function TScreenMain.ParseInput(PressedKey: cardinal; CharCode: widechar;
  PressedDown: boolean): boolean;
var
  SDL_ModState: word;
begin
  Result := True;

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT +
    KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT + KMOD_RALT);

  if (PressedDown) then
  begin // Key Down
        // check normal keys
    case WideCharUpperCase(CharCode)[1] of
      'Q':
      begin
        Result := False;
        Exit;
      end;
      'C':
      begin
        if (SDL_ModState = KMOD_LALT) then
        begin
          FadeTo(@ScreenCredits, SoundLib.Start);
          Exit;
        end;
      end;
      'M':
      begin
        if (Ini.Players >= 1) and (Length(DLLMan.Plugins) >= 1) then
        begin
          FadeTo(@ScreenPartyOptions, SoundLib.Start);
          Exit;
        end;
      end;

      'S':
      begin
        FadeTo(@ScreenStatMain, SoundLib.Start);
        Exit;
      end;

      'E':
      begin
        FadeTo(@ScreenEdit, SoundLib.Start);
        Exit;
      end;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE:
      begin
        Result := False;
      end;

      SDLK_RETURN:
      begin
        //Solo
        if (Interaction = 0) then
        begin
          if (Songs.SongList.Count >= 1) then
          begin
            if (Ini.Players >= 0) and (Ini.Players <= 3) then
              PlayersPlay := Ini.Players + 1;
            if (Ini.Players = 4) then
              PlayersPlay := 6;

            ScreenName.Goto_SingScreen := False;
            FadeTo(@ScreenName, SoundLib.Start);
          end
          else //show error message
            ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_SONGS'));
        end;

        //Multi
        if Interaction = 1 then
        begin
          if (Songs.SongList.Count >= 1) then
          begin
            if (Length(DLLMan.Plugins) >= 1) then
            begin
              FadeTo(@ScreenPartyOptions, SoundLib.Start);
            end
            else //show error message, No Plugins Loaded
              ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_PLUGINS'));
          end
          else //show error message, No Songs Loaded
            ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_SONGS'));
        end;

        //Stats
        if Interaction = 2 then
        begin
          FadeTo(@ScreenStatMain, SoundLib.Start);
        end;

        //Editor
        if Interaction = 3 then
        begin
          FadeTo(@ScreenEdit, SoundLib.Start);
        end;

        //Options
        if Interaction = 4 then
        begin
          FadeTo(@ScreenOptions, SoundLib.Start);
        end;

        //Exit
        if Interaction = 5 then
        begin
          Result := False;
        end;
      end;
      {**
       * Up and Down could be done at the same time,
       * but I don't want to declare variables inside
       * functions like this one, called so many times
       *}
      SDLK_DOWN: InteractInc;
      SDLK_UP: InteractDec;
      SDLK_RIGHT: InteractNext;
      SDLK_LEFT: InteractPrev;
    end;
  end
  else // Key Up
    case PressedKey of
      SDLK_RETURN:
      begin
      end;
    end;
end;

constructor TScreenMain.Create;
begin
  inherited Create;
{**
 * Attention ^^:
 * New Creation Order needed because of LoadFromTheme
 * and Button Collections.
 * At First Custom Texts and Statics
 * Then LoadFromTheme
 * after LoadFromTheme the Buttons and Selects
 *}
  TextDescription     := AddText(Theme.Main.TextDescription);
  TextDescriptionLong := AddText(Theme.Main.TextDescriptionLong);

  LoadFromTheme(Theme.Main);

  AddButton(Theme.Main.ButtonSolo);
  AddButton(Theme.Main.ButtonMulti);
  AddButton(Theme.Main.ButtonStat);
  AddButton(Theme.Main.ButtonEditor);
  AddButton(Theme.Main.ButtonOptions);
  AddButton(Theme.Main.ButtonExit);

  Interaction := 0;
end;

procedure TScreenMain.onShow;
begin
  inherited;
{**
 * Start background music
 *}
  SoundLib.StartBgMusic;
end;

procedure TScreenMain.InteractNext;
begin
  inherited InteractNext;
  Text[TextDescription].Text     := Theme.Main.Description[Interaction];
  Text[TextDescriptionLong].Text := Theme.Main.DescriptionLong[Interaction];
end;

procedure TScreenMain.InteractPrev;
begin
  inherited InteractPrev;
  Text[TextDescription].Text     := Theme.Main.Description[Interaction];
  Text[TextDescriptionLong].Text := Theme.Main.DescriptionLong[Interaction];
end;

procedure TScreenMain.InteractDec;
begin
  inherited InteractDec;
  Text[TextDescription].Text     := Theme.Main.Description[Interaction];
  Text[TextDescriptionLong].Text := Theme.Main.DescriptionLong[Interaction];
end;

procedure TScreenMain.InteractInc;
begin
  inherited InteractInc;
  Text[TextDescription].Text     := Theme.Main.Description[Interaction];
  Text[TextDescriptionLong].Text := Theme.Main.DescriptionLong[Interaction];
end;

procedure TScreenMain.SetAnimationProgress(Progress: real);
begin
  Static[0].Texture.ScaleW := Progress;
  Static[0].Texture.ScaleH := Progress;
end;

end.
