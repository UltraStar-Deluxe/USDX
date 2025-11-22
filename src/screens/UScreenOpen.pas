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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenOpen.pas $
 * $Id: UScreenOpen.pas 1939 2009-11-09 00:27:55Z s_alexander $
 *}

unit UScreenOpen;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu,
  UMusic,
  UFiles,
  UIni,
  ULyrics,
  UMenuText,
  UPath,
  USongs,
  UTexture,
  UThemes,
  UTime,
  dglOpenGL,
  Math,
  sdl2,
  SysUtils;

type
  TScreenOpen = class(TMenu)
    private
      //fTextF:      array[0..1] of integer;
      FileNameID:  integer; // button ID of filename
      fFilename:   IPath;
      fBackScreen: PMenu;

    public
      constructor Create; override;
      procedure OnShow; override;
      function ParseInput(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean; override;

      {**
       * Set by the caller to provide a default filename.
       * Set to the selected filename after calling this screen or to PATH_NONE
       * if the screen was aborted.
       * TODO: maybe pass this value with a callback OnValueChanged()
       *}
      property Filename: IPath READ fFilename WRITE fFilename;
      {** The screen that is shown after this screen is closed (set by the caller) *}
      property BackScreen: PMenu READ fBackScreen WRITE fBackScreen;
  end;

const
  ID='ID_062';   //for help system

implementation

uses
  UDraw,
  UGraphic,
  UHelp,
  ULog,
  UMain,
  USkins,
  UUnicodeUtils;

function TScreenOpen.ParseInput(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
begin
  Result := true;

  if (PressedDown) then  // Key Down
  begin
    // check normal keys
    if (IsPrintableChar(CharCode)) then
    begin
      if (Interaction = 0) then
      begin
        Button[FileNameID].Text[0].Text := Button[FileNameID].Text[0].Text + UCS4ToUTF8String(CharCode);
        Exit;
      end;
    end;

    // check special keys
    case PressedKey of
      SDLK_BACKSPACE: // del
        begin
          if Interaction = 0 then
          begin
            Button[FileNameID].Text[0].DeleteLastLetter;
          end;
        end;

      SDLK_ESCAPE:
        begin
          //Empty Filename and go to last Screen
          fFileName := PATH_NONE;
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(fBackScreen);
        end;

      SDLK_RETURN:
        begin
          if (Interaction = 0) then
          begin
            InteractNext;
          end
          else if (Interaction = 1) then
          begin
            //Update Filename and go to last Screen
            fFileName := Path(Button[0].Text[0].Text);
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(fBackScreen);
          end
          else if (Interaction = 2) then
          begin
            //Empty Filename and go to last Screen
            fFileName := PATH_NONE;
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(fBackScreen);
          end;
        end;

      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;

      SDLK_LEFT,
      SDLK_UP:
        begin
          InteractPrev;
        end;

      SDLK_RIGHT,
      SDLK_DOWN:
        begin
          InteractNext;
        end;
    end;
  end;
end;

constructor TScreenOpen.Create;
begin
  inherited Create;

  fFilename := PATH_NONE;

  LoadFromTheme(Theme.EditOpen);

  // button with editable text for filename
  FileNameID := AddButton(Theme.EditOpen.ButtonFileName);
  Button[FileNameID].Text[0].Writable := true;

  // buttons
  AddButton(Theme.EditOpen.ButtonLoad);
  AddButton(Theme.EditOpen.ButtonBack);
end;

procedure TScreenOpen.OnShow;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenOpen');

  Interaction := 0;
  Button[FileNameID].Text[0].Text := fFilename.ToUTF8();
end;

end.
