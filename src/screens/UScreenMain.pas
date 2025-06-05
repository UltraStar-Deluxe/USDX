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
  URecord,
  UScreenSong,
  USong,
  UThemes,
  MidiOut,
  MidiCons,
  Math,
  MD5,
  sdl2,
  SysUtils;

type

  TScreenMain = class(TMenu)
  public
    TextDescription:     integer;
    TextDescriptionLong: integer;
    PingTime:            integer;
    PingResponse:        integer;
    CurrentSound:        TCaptureBuffer;
    MidiOut:             TMidiOutput;

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

function TScreenMain.ParseInput(PressedKey: Cardinal; CharCode: UCS4Char;
  PressedDown: boolean): boolean;
var
  SDL_ModState: word;
begin
  Result := true;

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT +
    KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT + KMOD_RALT);

  if (PressedDown) then
  begin // Key Down
        // check normal keys
    case PressedKey of
      SDLK_S: begin
        FadeTo(@ScreenName, SoundLib.Start);
        Exit;
      end;

      SDLK_P: begin
        if (Ini.Players >= 1) and (Party.ModesAvailable) then
        begin
          FadeTo(@ScreenPartyOptions, SoundLib.Start);
          Exit;
        end;
      end;

      SDLK_W: begin
        SoundLib.Ping.Volume := 1.0;
        PingResponse := 0;
        PingTime := SDL_GetTicks;
        SoundLib.Ping.Play;
        Exit;
      end;

      SDLK_M: begin
        SoundLib.Ping.Volume := 1.0;
        PingResponse := 0;
        PingTime := SDL_GetTicks;
        MidiOut.PutShort($B1, $7, 127);
        MidiOut.PutShort($81, 24 + 60, 127);
        MidiOut.PutShort($91, 24 + 60, 127);
        Exit;
      end;

      SDLK_J: begin
        FadeTo(@ScreenJukeboxPlaylist, SoundLib.Start);
        Exit;
      end;

      SDLK_R: begin
        UGraphic.UnLoadScreens();
        Theme.LoadTheme(Ini.Theme, Ini.Color);
        UGraphic.LoadScreens(USDXVersionStr);
      end;

      SDLK_T: begin
        FadeTo(@ScreenStatMain, SoundLib.Start);
        Exit;
      end;

      SDLK_E: begin
        FadeTo(@ScreenEdit, SoundLib.Start);
        Exit;
      end;

      SDLK_O: begin
        FadeTo(@ScreenOptions, SoundLib.Start);
        Exit;
      end;

      SDLK_A: begin
        FadeTo(@ScreenAbout, SoundLib.Start);
        Exit;
      end;

      SDLK_C: begin
         FadeTo(@ScreenCredits, SoundLib.Start);
         Exit;
      end;

      SDLK_Q: begin
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

  WantSoftwareRenderingMsg := SoftwareRendering;

  PingTime := 0;
  PingResponse := 0;

  MidiOut := TMidiOutput.Create(nil);
  MidiOut.Open;
end;

procedure TScreenMain.OnShow;
begin
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

  AudioInput.CaptureStart;
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

  CurrentSound := AudioInputProcessor.Sound[0];
  CurrentSound.AnalyzeBuffer;

  if (CurrentSound.ToneAbs = 48) and (PingResponse = 0) then
  begin
    PingResponse := SDL_GetTicks - PingTime;
    MidiOut.PutShort($B1, $7, 127);
    MidiOut.PutShort($81, 24 + 60, 127);
    MidiOut.PutShort(MIDI_STOP, 0, 0);
  end;
  Display.DrawDelay(PingResponse);
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
