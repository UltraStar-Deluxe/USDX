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

unit UScreenOptionsGame;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, UIni, UThemes, USongs;

type
  TScreenOptionsGame = class(TMenu)
    public
      old_Tabs, old_Sorting: integer;
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure RefreshSongs;
  end;

implementation

uses UGraphic, SysUtils;

function TScreenOptionsGame.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
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
          RefreshSongs;

          FadeTo(@ScreenOptions);
        end;
      SDLK_RETURN:
        begin
          if SelInteraction = 6 then begin
            AudioPlayback.PlaySound(SoundLib.Back);
            RefreshSongs;
            FadeTo(@ScreenOptions);
          end;
        end;
      SDLK_DOWN:
        InteractNext;
      SDLK_UP :
        InteractPrev;
      SDLK_RIGHT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 5) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractInc;
          end;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 5) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
          end;
        end;
    end;
  end;
end;

constructor TScreenOptionsGame.Create;
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsGame);

  //Refresh Songs Patch
  old_Sorting := Ini.Sorting;
  old_Tabs    := Ini.Tabs;

  AddSelectSlide(Theme.OptionsGame.SelectPlayers,    Ini.Players,    IPlayers);
  AddSelectSlide(Theme.OptionsGame.SelectDifficulty, Ini.Difficulty, IDifficulty);
  AddSelectSlide(Theme.OptionsGame.SelectLanguage,   Ini.Language,   ILanguage);
  AddSelectSlide(Theme.OptionsGame.SelectTabs,       Ini.Tabs,       ITabs);
  AddSelectSlide(Theme.OptionsGame.SelectSorting,    Ini.Sorting,    ISorting);
  AddSelectSlide(Theme.OptionsGame.SelectDebug,      Ini.Debug,      IDebug);

  AddButton(Theme.OptionsGame.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);

end;

//Refresh Songs Patch
procedure TScreenOptionsGame.RefreshSongs;
begin
if (ini.Sorting <> old_Sorting) or (ini.Tabs <> old_Tabs) then
    ScreenSong.Refresh;
end;

procedure TScreenOptionsGame.onShow;
begin
  inherited;

//  Interaction := 0;
end;

end.
