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
  UConfig,
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
  UKeyBindings;

type

  TScreenMain = class(TMenu)
  private
    FKeyBindingsInitialized: boolean;
    procedure RegisterKeyBindings;
    function HandleShowHelp(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
    function HandleExitScreen(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
    function HandleShortcutSolo(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
    function HandleShortcutParty(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
    function HandleShortcutJukebox(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
    function HandleReloadTheme(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
    function HandleShortcutStats(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
    function HandleShortcutEditor(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
    function HandleShortcutOptions(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
    function HandleShortcutAbout(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
    function HandleShortcutCredits(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
    function HandleShortcutQuit(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
    function HandleConfirmSelection(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
    function HandleNavigateDown(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
    function HandleNavigateUp(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
    function HandleNavigateRight(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
    function HandleNavigateLeft(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
  protected
    function GetKeyBindingContext: UTF8String; override;
  public
    TextDescription:     integer;
    TextDescriptionLong: integer;

    constructor Create; override;
    function ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean; override;
    procedure OnShow; override;
    procedure SetInteraction(Num: integer); override;
    procedure SetAnimationProgress(Progress: real); override;
    function Draw: boolean; override;
  end;

const
  ID = 'ID_001';   //for help system

var
  WantSoftwareRenderingMsg: boolean;

implementation

uses
  UGraphic,
  UHelp,
  UIni,
  ULanguage,
  ULog,
  UNote,
  UParty,
  USkins,
  USongs,
  UTexture,
  UUnicodeUtils;

function TScreenMain.GetKeyBindingContext: UTF8String;
begin
  Result := ID;
end;

procedure TScreenMain.RegisterKeyBindings;
begin
  if FKeyBindingsInitialized then
    Exit;

  ClearKeyBindings;

  // General
  RegisterKeyBinding('SEC_001', 'RETURN', SDLK_RETURN, HandleConfirmSelection);
  RegisterKeyBinding('SEC_001', 'TAB', SDLK_TAB, HandleShowHelp);
  RegisterKeyBinding('SEC_001', 'PRINT', SDLK_PRINTSCREEN);
  RegisterKeyBinding('SEC_001', 'F11', SDLK_F11);
  RegisterKeyBinding('SEC_001', 'ALT_RETURN', SDLK_RETURN or MOD_LALT);
  RegisterKeyBinding('SEC_001', 'BACKSPACE', SDLK_BACKSPACE, HandleExitScreen);
  RegisterKeyBinding('SEC_001', 'ESC', SDLK_ESCAPE, HandleExitScreen);

  // Navigation
  RegisterKeyBinding('SEC_010', 'ARROWKEYS', SDLK_UP, HandleNavigateUp);
  RegisterKeyBinding('SEC_010', 'ARROWKEYS', SDLK_DOWN, HandleNavigateDown);
  RegisterKeyBinding('SEC_010', 'ARROWKEYS', SDLK_LEFT, HandleNavigateLeft);
  RegisterKeyBinding('SEC_010', 'ARROWKEYS', SDLK_RIGHT, HandleNavigateRight);

  // Shortcuts
  RegisterKeyBinding('SEC_020', 'S', SDLK_S, HandleShortcutSolo);
  RegisterKeyBinding('SEC_020', 'P', SDLK_P, HandleShortcutParty);
  RegisterKeyBinding('SEC_020', 'J', SDLK_J, HandleShortcutJukebox);
  RegisterKeyBinding('SEC_020', 'T', SDLK_T, HandleShortcutStats);
  RegisterKeyBinding('SEC_020', 'E', SDLK_E, HandleShortcutEditor);
  RegisterKeyBinding('SEC_020', 'O', SDLK_O, HandleShortcutOptions);
  RegisterKeyBinding('SEC_020', 'A', SDLK_A, HandleShortcutAbout);
  RegisterKeyBinding('SEC_020', 'C', SDLK_C, HandleShortcutCredits);
  RegisterKeyBinding('SEC_020', 'Q', SDLK_Q, HandleShortcutQuit);

  // Maintenance shortcuts not exposed via help
  RegisterKeyBinding('', '', SDLK_R, HandleReloadTheme);

  FKeyBindingsInitialized := true;
  EnsureKeyBindingsPublished;
end;

function TScreenMain.ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean;
begin
  // default mouse behaviour
  Result := inherited ParseMouse(MouseButton, BtnDown, X, Y);
end;

function TScreenMain.HandleShowHelp(PressedKey: QWord; CharCode: UCS4Char;
  PressedDown: boolean; Parameter: integer): boolean;
begin
  Result := true;
  if not PressedDown then
    Exit;
  ScreenPopupHelp.ShowPopup;
end;

function TScreenMain.HandleExitScreen(PressedKey: QWord; CharCode: UCS4Char;
  PressedDown: boolean; Parameter: integer): boolean;
begin
  if not PressedDown then
    Exit(true);
  Result := false;
end;

function TScreenMain.HandleShortcutSolo(PressedKey: QWord; CharCode: UCS4Char;
  PressedDown: boolean; Parameter: integer): boolean;
begin
  Result := true;
  if not PressedDown then
    Exit;
  FadeTo(@ScreenName, SoundLib.Start);
end;

function TScreenMain.HandleShortcutParty(PressedKey: QWord; CharCode: UCS4Char;
  PressedDown: boolean; Parameter: integer): boolean;
begin
  Result := true;
  if not PressedDown then
    Exit;
  if (Ini.Players >= 1) and (Party.ModesAvailable) then
    FadeTo(@ScreenPartyOptions, SoundLib.Start);
end;

function TScreenMain.HandleShortcutJukebox(PressedKey: QWord; CharCode: UCS4Char;
  PressedDown: boolean; Parameter: integer): boolean;
begin
  Result := true;
  if not PressedDown then
    Exit;
  FadeTo(@ScreenJukeboxPlaylist, SoundLib.Start);
end;

function TScreenMain.HandleReloadTheme(PressedKey: QWord; CharCode: UCS4Char;
  PressedDown: boolean; Parameter: integer): boolean;
begin
  Result := true;
  if not PressedDown then
    Exit;
  UGraphic.UnLoadScreens();
  Theme.LoadTheme(Ini.Theme, Ini.Color);
  UGraphic.LoadScreens(USDXVersionStr);
end;

function TScreenMain.HandleShortcutStats(PressedKey: QWord; CharCode: UCS4Char;
  PressedDown: boolean; Parameter: integer): boolean;
begin
  Result := true;
  if not PressedDown then
    Exit;
  FadeTo(@ScreenStatMain, SoundLib.Start);
end;

function TScreenMain.HandleShortcutEditor(PressedKey: QWord; CharCode: UCS4Char;
  PressedDown: boolean; Parameter: integer): boolean;
begin
  Result := true;
  if not PressedDown then
    Exit;
  FadeTo(@ScreenEdit, SoundLib.Start);
end;

function TScreenMain.HandleShortcutOptions(PressedKey: QWord; CharCode: UCS4Char;
  PressedDown: boolean; Parameter: integer): boolean;
begin
  Result := true;
  if not PressedDown then
    Exit;
  FadeTo(@ScreenOptions, SoundLib.Start);
end;

function TScreenMain.HandleShortcutAbout(PressedKey: QWord; CharCode: UCS4Char;
  PressedDown: boolean; Parameter: integer): boolean;
begin
  Result := true;
  if not PressedDown then
    Exit;
  FadeTo(@ScreenAbout, SoundLib.Start);
end;

function TScreenMain.HandleShortcutCredits(PressedKey: QWord; CharCode: UCS4Char;
  PressedDown: boolean; Parameter: integer): boolean;
begin
  Result := true;
  if not PressedDown then
    Exit;
  FadeTo(@ScreenCredits, SoundLib.Start);
end;

function TScreenMain.HandleShortcutQuit(PressedKey: QWord; CharCode: UCS4Char;
  PressedDown: boolean; Parameter: integer): boolean;
begin
  if not PressedDown then
    Exit(true);
  Result := false;
end;

function TScreenMain.HandleConfirmSelection(PressedKey: QWord;
  CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
begin
  Result := true;
  if not PressedDown then
    Exit;

  Party.bPartyGame := false;

  case Interaction of
    0: begin
         if (Songs.SongList.Count >= 1) then
         begin
           if (Ini.Players >= 0) and (Ini.Players <= 3) then
             PlayersPlay := Ini.Players + 1
           else if (Ini.Players = 4) then
             PlayersPlay := 6;

           if Ini.OnSongClick = sSelectPlayer then
             FadeTo(@ScreenSong)
           else
           begin
             ScreenName.Goto_SingScreen := false;
             FadeTo(@ScreenName, SoundLib.Start);
           end;
         end
         else
           ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_SONGS'));
       end;

    1: begin
         if (Songs.SongList.Count >= 1) then
         begin
           Party.bPartyGame := true;
           FadeTo(@ScreenPartyOptions, SoundLib.Start);
         end
         else
           ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_SONGS'));
       end;

    2: begin
         if (Songs.SongList.Count >= 1) then
           FadeTo(@ScreenJukeboxPlaylist, SoundLib.Start)
         else
           ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_SONGS'));
       end;

    3: FadeTo(@ScreenStatMain, SoundLib.Start);

    4: begin
         {$IFDEF UseMIDIPort}
         FadeTo(@ScreenEdit, SoundLib.Start);
         {$ELSE}
         ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_EDITOR'));
         {$ENDIF}
       end;

    5: FadeTo(@ScreenOptions, SoundLib.Start);
    6: FadeTo(@ScreenAbout, SoundLib.Start);
    7: begin
         Result := false;
         Exit;
       end;
  end;
end;

function TScreenMain.HandleNavigateDown(PressedKey: QWord; CharCode: UCS4Char;
  PressedDown: boolean; Parameter: integer): boolean;
begin
  Result := true;
  if not PressedDown then
    Exit;
  InteractInc;
end;

function TScreenMain.HandleNavigateUp(PressedKey: QWord; CharCode: UCS4Char;
  PressedDown: boolean; Parameter: integer): boolean;
begin
  Result := true;
  if not PressedDown then
    Exit;
  InteractDec;
end;

function TScreenMain.HandleNavigateRight(PressedKey: QWord; CharCode: UCS4Char;
  PressedDown: boolean; Parameter: integer): boolean;
begin
  Result := true;
  if not PressedDown then
    Exit;
  InteractNext;
end;

function TScreenMain.HandleNavigateLeft(PressedKey: QWord; CharCode: UCS4Char;
  PressedDown: boolean; Parameter: integer): boolean;
begin
  Result := true;
  if not PressedDown then
    Exit;
  InteractPrev;
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

  RegisterKeyBindings;
  Interaction := 0;

  WantSoftwareRenderingMsg := SoftwareRendering;
end;

procedure TScreenMain.OnShow;
begin
  RegisterKeyBindings;
  inherited;

  SoundLib.StartBgMusic;

  ScreenSong.Mode := smNormal;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenMains');

 {**
  * Clean up TPartyGame here
  * at the moment there is no better place for this
  *}
  Party.Clear;
end;

function TScreenMain.Draw: boolean;
begin
  Result := inherited Draw;

  if not ScreenPopupError.Visible then
  begin
    if WantSoftwareRenderingMsg then
    begin
      WantSoftwareRenderingMsg := false;
      ScreenPopupError.ShowPopup(Language.Translate('ERROR_SOFTWARE_RENDERING'));
    end;
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
