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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenMain.pas $
 * $Id: UScreenMain.pas 3128 2015-08-28 01:45:23Z basisbit $
 *}

unit UScreenMain;

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
  UScreenSong,
  USong,
  UThemes,
  MD5,
  sdl2,
  SysUtils,
  UThemes,
  UConfig;

type

  TScreenMain = class(TMenu)
  private
    { ticks when the user interacted, used to start credits
      after a period of time w/o user interaction }
    UserInteractionTicks: cardinal;

  public
    TextDescription:     integer;
    TextDescriptionLong: integer;

    constructor Create; override;
    function ParseInput(PressedKey: Cardinal; CharCode: UCS4Char;
      PressedDown: boolean): boolean; override;
    function ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean; override;
    procedure OnShow; override;
    procedure SetInteraction(Num: integer); override;
    procedure SetAnimationProgress(Progress: real); override;
    function Draw: boolean; override;
  end;

const
  { start credits after 60 seconds w/o interaction }
  TicksUntilCredits = 5 * 60 * 1000;
  ID = 'ID_001';   //for help system

implementation

uses
  UGraphic,
  UHelp,
  UIni,
  ULanguage,
  ULog,
  UNote,
  UParty,
  UScreenCredits,
  USkins,
  USongs,
  UTexture,
  UUnicodeUtils;

function TScreenMain.ParseInput(PressedKey: Cardinal; CharCode: UCS4Char;
  PressedDown: boolean): boolean;
var
  SDL_ModState: word;
begin
  Result := true;

  { reset user interaction timer }
  UserInteractionTicks := SDL_GetTicks;

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT +
    KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT + KMOD_RALT);

  if (PressedDown) then
  begin // Key Down
        // check normal keys
    case UCS4UpperCase(CharCode) of
      Ord('S'): begin
        FadeTo(@ScreenName, SoundLib.Start);
        Exit;
      end;

      Ord('P'): begin
        if (Ini.Players >= 1) and (Party.ModesAvailable) then
        begin
          FadeTo(@ScreenPartyOptions, SoundLib.Start);
          Exit;
        end;
      end;

      Ord('J'): begin
        FadeTo(@ScreenJukeboxPlaylist, SoundLib.Start);
        Exit;
      end;

      Ord('R'): begin
        UGraphic.UnLoadScreens();
        Theme.LoadTheme(Ini.Theme, Ini.Color);
        UGraphic.LoadScreens(USDXVersionStr);
      end;

      Ord('S'): begin
        FadeTo(@ScreenStatMain, SoundLib.Start);
        Exit;
      end;

      Ord('E'): begin
        FadeTo(@ScreenEdit, SoundLib.Start);
        Exit;
      end;

      Ord('O'): begin
        FadeTo(@ScreenOptions, SoundLib.Start);
        Exit;
      end;

      Ord('A'): begin
        FadeTo(@ScreenAbout, SoundLib.Start);
        Exit;
      end;

      Ord('C'): begin
         FadeTo(@ScreenCredits, SoundLib.Start);
         Exit;
      end;

      Ord('Q'): begin
        Result := false;
        Exit;
      end;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE:
      begin
        Result := false;
      end;

      SDLK_TAB:
      begin
        ScreenPopupHelp.ShowPopup();
      end;

      SDLK_RETURN:
      begin
        // reset
        Party.bPartyGame := false;

        //Solo
        if (Interaction = 0) then
        begin
          if (Songs.SongList.Count >= 1) then
          begin
            if (Ini.Players >= 0) and (Ini.Players <= 3) then
              PlayersPlay := Ini.Players + 1;
            if (Ini.Players = 4) then
              PlayersPlay := 6;

            if Ini.OnSongClick = sSelectPlayer then
              FadeTo(@ScreenSong)
            else
            begin
              ScreenName.Goto_SingScreen := false;
              FadeTo(@ScreenName, SoundLib.Start);
            end;
          end
          else //show error message
            ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_SONGS'));
        end;

        //Party
        if Interaction = 1 then
        begin
          if (Songs.SongList.Count >= 1) then
          begin
            Party.bPartyGame := true;

            FadeTo(@ScreenPartyOptions, SoundLib.Start);
          end
          else //show error message, No Songs Loaded
            ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_SONGS'));
        end;

        //Jukebox
        if Interaction = 2 then
        begin
          if (Songs.SongList.Count >= 1) then
          begin
            FadeTo(@ScreenJukeboxPlaylist, SoundLib.Start);
          end
          else //show error message, No Songs Loaded
            ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_SONGS'));
        end;

        //Stats
        if Interaction = 3 then
        begin
          FadeTo(@ScreenStatMain, SoundLib.Start);
        end;

        //Editor
        if Interaction = 4 then
        begin
          {$IFDEF UseMIDIPort}
          FadeTo(@ScreenEdit, SoundLib.Start);
          {$ELSE}
          ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_EDITOR'));
          {$ENDIF}
        end;

        //Options
        if Interaction = 5 then
        begin
          FadeTo(@ScreenOptions, SoundLib.Start);
        end;

        //About
        if Interaction = 6 then
        begin
          FadeTo(@ScreenAbout, SoundLib.Start);
        end;

        //Exit
        if Interaction = 7 then
        begin
          Result := false;
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

function TScreenMain.ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean;
begin
  // default mouse behaviour
  Result := inherited ParseMouse(MouseButton, BtnDown, X, Y);

  { reset user interaction timer }
  UserInteractionTicks := SDL_GetTicks;
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
  AddButton(Theme.Main.ButtonJukebox);
  AddButton(Theme.Main.ButtonStat);
  AddButton(Theme.Main.ButtonEditor);
  AddButton(Theme.Main.ButtonOptions);
  AddButton(Theme.Main.ButtonAbout);
  AddButton(Theme.Main.ButtonExit);

  Interaction := 0;
end;

procedure TScreenMain.OnShow;
begin
  inherited;

  SoundLib.StartBgMusic;

  ScreenSong.Mode := smNormal;

  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenMain)');

 {**
  * Clean up TPartyGame here
  * at the moment there is no better place for this
  *}
  Party.Clear;

  { reset user interaction timer }
  UserInteractionTicks := SDL_GetTicks;
end;

function TScreenMain.Draw: boolean;
begin
  Result := inherited Draw;

  { start credits after a period w/o user interaction }
  if (UserInteractionTicks + TicksUntilCredits < SDL_GetTicks) then
  begin
    FadeTo(@ScreenCredits, SoundLib.Start);
  end;
end;

procedure TScreenMain.SetInteraction(Num: integer);
begin
  inherited SetInteraction(Num);
  Text[TextDescription].Text     := Theme.Main.Description[Interaction];
  Text[TextDescriptionLong].Text := Theme.Main.DescriptionLong[Interaction];
end;

procedure TScreenMain.SetAnimationProgress(Progress: real);
begin
  Statics[0].Texture.ScaleW := Progress;
  Statics[0].Texture.ScaleH := Progress;
end;

end.
