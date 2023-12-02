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
  UDisplay,
  UFiles,
  UIni,
  UMenu,
  UMusic,
  UScreensong,
  USongs,
  UThemes,
  sdl2;

type
  TScreenOptionsGame = class(TMenu)
    private
      old_Language:  integer;
      old_SongMenu:  integer;
      old_Sorting:   integer;
      old_Tabs:      integer;
      AVDelayOptInt: integer;
      MicDelayOptInt:integer;
      AVDelaySelectNum:  integer;
      MicDelaySelectNum: integer;

      procedure Leave;
      procedure ReloadCurrentScreen;
      procedure ReloadAllScreens;
      procedure ReloadSongMenu;
      procedure UpdateCalculatedSelectSlides(Init: boolean);

    public
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
  end;

const
  ID='ID_071';   //for help system

implementation

uses
  UConfig,
  UGraphic,
  UHelp,
  ULanguage,
  ULog,
  UUnicodeUtils,
  SysUtils;

type
  TGetTextFunc = function(var Param: integer; Offset: integer; Modify: boolean; OptText: PUtf8String): boolean;
  UTF8StringArray = array of UTF8String;

function TScreenOptionsGame.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
  if PressedDown then
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
          Leave;
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;
      SDLK_RETURN:
        begin
          if SelInteraction = 8 then
            Leave;
        end;
      SDLK_DOWN:
        InteractNext;
      SDLK_UP :
        InteractPrev;
      SDLK_RIGHT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 7) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractInc;
          end;
          UpdateCalculatedSelectSlides(false);
          if (SelInteraction = 0) then
          begin
            ReloadCurrentScreen;
          end;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 7) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
          end;
          UpdateCalculatedSelectSlides(false);
          if (SelInteraction = 0) then
          begin
            ReloadCurrentScreen;
          end;
        end;
    end;
  end;
end;

procedure CalculateSelectSlide(Init: boolean; GetText: TGetTextFunc; var Param: integer; var OptInt: integer; var Texts: UTF8StringArray);
var
  Idx: integer;
  NumOpts: integer;
begin
  if not GetText(Param, 0, true, nil) then
  begin
    SetLength(Texts, 0);
    Exit;
  end;
  if GetText(Param, -1, false, nil) then
    Idx := 1
  else
    Idx := 0;
  if not Init then
  begin
    if OptInt = Idx then
      Exit;
    GetText(Param, OptInt - Idx, true, nil);
    if GetText(Param, -1, false, nil) then
      Idx := 1
    else
      Idx := 0;
  end;
  OptInt := Idx;
  if GetText(Param, 1, false, nil) then
    NumOpts := Idx + 2
  else
    NumOpts := Idx + 1;
  SetLength(Texts, NumOpts);
  for Idx := 0 to High(Texts) do
    GetText(Param, Idx - OptInt, false, @Texts[Idx]);
end;

function GetAVDelayOptText(var Param: integer; Offset: integer; Modify: boolean; OptText: PUTF8String): boolean;
begin
  if OptText <> nil then
    OptText^ := Format('%d ms', [Param + Offset * 10]);
  if Modify then
    Param := Param + Offset * 10;
  Result := true;
end;

function GetMicDelayOptText(var Param: integer; Offset: integer; Modify: boolean; OptText: PUTF8String): boolean;
begin
  if Param + Offset * 10 < 0 then
    Result := false
  else
  begin
    if OptText <> nil then
      OptText^ := Format('%d ms', [Param + Offset * 10]);
    if Modify then
      Param := Param + Offset * 10;
    Result := true;
  end;
end;

procedure TScreenOptionsGame.UpdateCalculatedSelectSlides(Init: boolean);
begin
  CalculateSelectSlide(Init, @GetAVDelayOptText, Ini.AVDelay, AVDelayOptInt, IAVDelay);
  CalculateSelectSlide(Init, @GetMicDelayOptText, Ini.MicDelay, MicDelayOptInt, IMicDelay);
  if Init then
  begin
    AVDelaySelectNum := AddSelectSlide(Theme.OptionsGame.SelectAVDelay, AVDelayOptInt, IAVDelay);
    MicDelaySelectNum := AddSelectSlide(Theme.OptionsGame.SelectMicDelay, MicDelayOptInt, IMicDelay);
  end
  else
  begin
    UpdateSelectSlideOptions(Theme.OptionsGame.SelectAVDelay, AVDelaySelectNum, IAVDelay, AVDelayOptInt);
    UpdateSelectSlideOptions(Theme.OptionsGame.SelectMicDelay, MicDelaySelectNum, IMicDelay, MicDelayOptInt);
  end;
end;

constructor TScreenOptionsGame.Create;
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsGame);

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

  Theme.OptionsGame.SelectAVDelay.showArrows  := true;
  Theme.OptionsGame.SelectAVDelay.oneItemOnly := true;
  Theme.OptionsGame.SelectMicDelay.showArrows  := true;
  Theme.OptionsGame.SelectMicDelay.oneItemOnly := true;
  UpdateCalculatedSelectSlides(true);

  AddButton(Theme.OptionsGame.ButtonExit);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(20, 5, Theme.Options.Description[OPTIONS_DESC_INDEX_BACK]);

end;

procedure TScreenOptionsGame.OnShow;
begin
  inherited;

  // save current settings in order to determine if a refresh is required
  old_Language := Ini.Language;
  old_SongMenu := Ini.SongMenu;
  old_Sorting  := Ini.Sorting;
  old_Tabs     := Ini.Tabs;

  Interaction := 0;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenOptionsGame');
end;

procedure TScreenOptionsGame.Leave;
begin
  Ini.Save;
  ReloadAllScreens;
  ReloadSongMenu;

  AudioPlayback.PlaySound(SoundLib.Back);
  FadeTo(@ScreenOptions);
end;

procedure TScreenOptionsGame.ReloadCurrentScreen;
begin
  ScreenOptionsGame.Free;
  Language.ChangeLanguage(ILanguage[Ini.Language]);
  Ini.TranslateOptionValues;
  Theme.LoadTheme(Ini.Theme, Ini.Color);
  ScreenOptionsGame := TScreenOptionsGame.Create;
end;

procedure TScreenOptionsGame.ReloadAllScreens;
begin
  // Reload all screens after language changed
  if (old_Language <> Ini.Language) then
  begin
    UGraphic.UnloadScreens;

    Theme.LoadTheme(Ini.Theme, Ini.Color);
    UGraphic.LoadScreens(USDXVersionStr);

    old_Language := Ini.Language;
    old_SongMenu := Ini.SongMenu;
    old_Sorting  := Ini.Sorting;
    old_Tabs     := Ini.Tabs;
  end;
end;

procedure TScreenOptionsGame.ReloadSongMenu;
begin
  if (Ini.Sorting <> old_Sorting) or (Ini.Tabs <> old_Tabs) or (old_SongMenu <> Ini.SongMenu) then
  begin
    Theme.ThemeSongLoad;

    ScreenSong.Free;
    ScreenSong := TScreenSong.Create;
  end;
end;

end.
