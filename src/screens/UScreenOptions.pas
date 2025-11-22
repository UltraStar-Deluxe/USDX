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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenOptions.pas $
 * $Id: UScreenOptions.pas 2649 2010-10-10 10:34:20Z tobigun $
 *}

unit UScreenOptions;

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
  USongs,
  UThemes,
  sdl2,
  SysUtils;

type
  TScreenOptions = class(TMenu)
    private
      ButtonGameIID,
      ButtonGraphicsIID,
      ButtonSoundIID,
      ButtonInputIID,
      ButtonLyricsIID,
      ButtonThemesIID,
      ButtonRecordIID,
      ButtonAdvancedIID,
      ButtonNetworkIID,
      ButtonWebcamIID,
      ButtonJukeboxIID,
      ButtonExitIID: cardinal;

      MapIIDtoDescID: array of integer;

      procedure UpdateTextDescriptionFor(IID: integer); virtual;

    public
      TextDescription:    integer;
      constructor Create; override;
      function ParseInput(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean; override;
      procedure OnShow; override;
      procedure SetInteraction(Num: integer); override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

const
  ID='ID_070';   //for help system

implementation

uses
  UDatabase,
  UGraphic,
  UHelp,
  ULanguage,
  ULog,
  UWebcam,
  UUnicodeUtils;

function TScreenOptions.ParseInput(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check normal keys
    case PressedKey of
      SDLK_G:
        begin
          FadeTo(@ScreenOptionsGame, SoundLib.Start);
          Exit;
        end;

      SDLK_H:
        begin
          FadeTo(@ScreenOptionsGraphics, SoundLib.Start);
          Exit;
        end;

      SDLK_S:
        begin
          FadeTo(@ScreenOptionsSound, SoundLib.Start);
          Exit;
        end;

      SDLK_I:
        begin
          FadeTo(@ScreenOptionsInput, SoundLib.Start);
          Exit;
        end;

      SDLK_L:
        begin
          FadeTo(@ScreenOptionsLyrics, SoundLib.Start);
          Exit;
        end;

      SDLK_T:
        begin
          FadeTo(@ScreenOptionsThemes, SoundLib.Start);
          Exit;
        end;

      SDLK_R:
        begin
          FadeTo(@ScreenOptionsRecord, SoundLib.Start);
          Exit;
        end;

      SDLK_A:
        begin
          FadeTo(@ScreenOptionsAdvanced, SoundLib.Start);
          Exit;
        end;

      SDLK_N:
        begin
          if (High(DataBase.NetworkUser) = -1) then
            ScreenPopupError.ShowPopup(Language.Translate('SING_OPTIONS_NETWORK_NO_DLL'))
          else
          begin
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(@ScreenOptionsNetwork);
          end;
          Exit;
        end;

      SDLK_W:
        begin
          FadeTo(@ScreenOptionsWebcam, SoundLib.Start);
          Exit;
        end;

      SDLK_J:
        begin
          FadeTo(@ScreenOptionsJukebox, SoundLib.Start);
          Exit;
        end;

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
          if Interaction = ButtonGameIID then
          begin
            AudioPlayback.PlaySound(SoundLib.Start);
            FadeTo(@ScreenOptionsGame);
          end;

          if Interaction = ButtonGraphicsIID then
          begin
            AudioPlayback.PlaySound(SoundLib.Start);
            FadeTo(@ScreenOptionsGraphics);
          end;

          if Interaction = ButtonSoundIID then
          begin
            AudioPlayback.PlaySound(SoundLib.Start);
            FadeTo(@ScreenOptionsSound);
          end;

          if Interaction = ButtonInputIID then
          begin
            AudioPlayback.PlaySound(SoundLib.Start);
            FadeTo(@ScreenOptionsInput);
          end;

          if Interaction = ButtonLyricsIID then
          begin
            AudioPlayback.PlaySound(SoundLib.Start);
            FadeTo(@ScreenOptionsLyrics);
          end;

          if Interaction = ButtonThemesIID then
          begin
            AudioPlayback.PlaySound(SoundLib.Start);
            FadeTo(@ScreenOptionsThemes);
          end;

          if Interaction = ButtonRecordIID then
          begin
            AudioPlayback.PlaySound(SoundLib.Start);
            FadeTo(@ScreenOptionsRecord);
          end;

          if Interaction = ButtonAdvancedIID then
          begin
            AudioPlayback.PlaySound(SoundLib.Start);
            FadeTo(@ScreenOptionsAdvanced);
          end;

          if Interaction = ButtonNetworkIID then
          begin
            if (High(DataBase.NetworkUser) = -1) then
              ScreenPopupError.ShowPopup(Language.Translate('SING_OPTIONS_NETWORK_NO_DLL'))
            else
            begin
              AudioPlayback.PlaySound(SoundLib.Back);
              FadeTo(@ScreenOptionsNetwork);
            end;
          end;

          if Interaction = ButtonWebcamIID then
          begin
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(@ScreenOptionsWebcam);
          end;

          if Interaction = ButtonJukeboxIID then
          begin
            if (Songs.SongList.Count >= 1) then
            begin
              AudioPlayback.PlaySound(SoundLib.Start);
              FadeTo(@ScreenOptionsJukebox);
            end
            else //show error message, No Songs Loaded
              ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_SONGS'));
          end;

          if Interaction = ButtonExitIID then
          begin
            Ini.Save;
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(@ScreenMain);
          end;
        end;
      SDLK_DOWN:    InteractNextRow;
      SDLK_UP:      InteractPrevRow;
      SDLK_RIGHT:   InteractNext;
      SDLK_LEFT:    InteractPrev;
    end;
  end;
end;

constructor TScreenOptions.Create;

  // TODO: Generalize method and implement it into base code (to be used by every screen/menu)
  function AddButtonChecked(Btn: TThemeButton; DescIndex: byte; out IIDvar: cardinal; AddX: real = 14; AddY: real = 20): cardinal;
  var OldPos: integer;
  begin
    OldPos := Length(Button);
    Result := AddButton(Btn);
    if Length(Button) <> OldPos then // check if button was succesfully added // TODO: RattleSN4K3: Improve AddButton interface returning properly index to be used by interaction check
    begin
      IIDvar := High(Interactions);

      // update mapping, IID to Desc index
      SetLength(MapIIDtoDescID, IIDvar+1);
      MapIIDtoDescID[IIDvar] := DescIndex;

      if (Length(Button[Result].Text) = 0) then // update text if not already set
        AddButtonText(AddX, AddY, Theme.Options.Description[DescIndex]);
    end;

  end;
begin
  inherited Create;

  TextDescription := AddText(Theme.Options.TextDescription);

  LoadFromTheme(Theme.Options);

  // Order is irrelevant to the represenatation, however InteractNext/Prev is not working with a different order // TODO: RattleSN4K3: allow InteractNext etc. work with themes having a different button layout
  AddButtonChecked(Theme.Options.ButtonGame, OPTIONS_DESC_INDEX_GAME,  ButtonGameIID);
  AddButtonChecked(Theme.Options.ButtonGraphics, OPTIONS_DESC_INDEX_GRAPHICS,  ButtonGraphicsIID);
  AddButtonChecked(Theme.Options.ButtonSound, OPTIONS_DESC_INDEX_SOUND,  ButtonSoundIID);
  AddButtonChecked(Theme.Options.ButtonInput, OPTIONS_DESC_INDEX_INPUT,  ButtonInputIID);

  AddButtonChecked(Theme.Options.ButtonLyrics, OPTIONS_DESC_INDEX_LYRICS,  ButtonLyricsIID);
  AddButtonChecked(Theme.Options.ButtonThemes, OPTIONS_DESC_INDEX_THEMES,  ButtonThemesIID);
  AddButtonChecked(Theme.Options.ButtonRecord, OPTIONS_DESC_INDEX_RECORD,  ButtonRecordIID);
  AddButtonChecked(Theme.Options.ButtonAdvanced, OPTIONS_DESC_INDEX_ADVANCED,  ButtonAdvancedIID);
  AddButtonChecked(Theme.Options.ButtonNetwork, OPTIONS_DESC_INDEX_NETWORK,  ButtonNetworkIID);

  AddButtonChecked(Theme.Options.ButtonWebcam, OPTIONS_DESC_INDEX_WEBCAM,  ButtonWebcamIID);
  AddButtonChecked(Theme.Options.ButtonJukebox, OPTIONS_DESC_INDEX_JUKEBOX,  ButtonJukeboxIID);

  AddButtonChecked(Theme.Options.ButtonExit, OPTIONS_DESC_INDEX_BACK,  ButtonExitIID);

  Interaction := 0;
end;

procedure TScreenOptions.OnShow;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenOptions');

  // continue possibly stopped bg-music (stopped in record options)
  SoundLib.StartBgMusic;
end;

procedure TScreenOptions.SetInteraction(Num: integer);
begin
  inherited SetInteraction(Num);
  UpdateTextDescriptionFor(Interaction);
end;

procedure TScreenOptions.SetAnimationProgress(Progress: real);
var i: integer;
begin
  // update all buttons
  for i := 0 to High(Button) do
    Button[i].Texture.ScaleW := Progress;
end;

procedure TScreenOptions.UpdateTextDescriptionFor(IID: integer);
var index: integer;
begin
  // Sanity check
  if (IID < 0 ) or (IID >= Length(MapIIDtoDescID)) then
    Exit;

  Text[TextDescription].Text := Theme.Options.Description[MapIIDtoDescID[IID]];
end;

end.
