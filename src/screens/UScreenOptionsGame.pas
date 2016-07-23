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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenOptionsGame.pas $
 * $Id: UScreenOptionsGame.pas 2203 2010-03-16 19:25:13Z brunzelchen $
 *}

unit UScreenOptionsGame;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  sdl2,
  UMenu,
  ULog,
  UDisplay,
  UMusic,
  UFiles,
  UIni,
  UThemes,
  UScreensong,
  USongs;

type
  TScreenOptionsGame = class(TMenu)
    private
      procedure ReloadScreens;

    public
      ActualLanguage:  Integer;
      ActualSongMenu: Integer;

      old_Tabs, old_Sorting: integer;
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure RefreshSongs;
  end;

implementation

uses
  UGraphic,
  ULanguage,
  UUnicodeUtils,
  SysUtils;

function TScreenOptionsGame.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
  if PressedDown then
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
      SDLK_BACKSPACE :
        begin
          ReloadScreens;
          AudioPlayback.PlaySound(SoundLib.Back);
          RefreshSongs;
          FadeTo(@ScreenOptions);
        end;
      SDLK_RETURN:
        begin
          if SelInteraction = 6 then
          begin
            ReloadScreens;
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

  Theme.OptionsGame.SelectLanguage.showArrows  := true;
  Theme.OptionsGame.SelectLanguage.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsGame.SelectLanguage,   Ini.Language,  ILanguageTranslated);

  Theme.OptionsGame.SelectSongMenu.showArrows  := true;
  Theme.OptionsGame.SelectSongMenu.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsGame.SelectSongMenu,    Ini.SongMenu, ISongMenuTranslated);

  Theme.OptionsGame.SelectTabs.showArrows  := true;
  Theme.OptionsGame.SelectTabs.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsGame.SelectTabs,       Ini.Tabs,       ITabsTranslated);

  Theme.OptionsGame.SelectSorting.showArrows  := true;
  Theme.OptionsGame.SelectSorting.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsGame.SelectSorting,    Ini.Sorting,    ISortingTranslated);

  Theme.OptionsGame.SelectShowScores.showArrows  := true;
  Theme.OptionsGame.SelectShowScores.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsGame.SelectShowScores,    Ini.ShowScores,    IShowScoresTranslated);

  Theme.OptionsGame.SelectDebug.showArrows  := true;
  Theme.OptionsGame.SelectDebug.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsGame.SelectDebug,      Ini.Debug,      IDebugTranslated);

  AddButton(Theme.OptionsGame.ButtonExit);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(20, 5, Theme.Options.Description[OPTIONS_DESC_INDEX_BACK]);

end;

//Refresh Songs Patch
procedure TScreenOptionsGame.RefreshSongs;
begin
  if (Ini.Sorting <> old_Sorting) or (Ini.Tabs <> old_Tabs) then
    ScreenSong.Refresh;
end;

procedure TScreenOptionsGame.OnShow;
begin
  inherited;

  ActualLanguage := Ini.Language;
  ActualSongMenu := Ini.SongMenu;

  Interaction := 0;
end;

procedure TScreenOptionsGame.ReloadScreens;
begin

  if(ActualSongMenu <> Ini.SongMenu) then
  begin
    Theme.ThemeSongLoad;

    ScreenSong.Free;
    ScreenSong := TScreenSong.Create;
  end;

  // Reload all screens, after Language changed
  if(ActualLanguage <> Ini.Language) then
  begin
     {
    //Language.ChangeLanguage(ILanguage[Ini.Language]);
    Ini.Save;

    Language.Free;
    Language := TLanguage.Create;

    Ini.Free;
    Ini := TIni.Create;
    //Ini.Load;

    Theme.Free;
    Theme := TTheme.Create;

    Menu.Free;
    Menu := TMenu.Create;

    //Language.ChangeLanguage('Inglês');
    UGraphic.UnLoadScreens();
    UGraphic.LoadScreens(true);
    }
  end;
end;

end.
