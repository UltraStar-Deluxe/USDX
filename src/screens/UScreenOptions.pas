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

unit UScreenOptions;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SDL,
  SysUtils,
  UMenu,
  UDisplay,
  UMusic,
  UFiles,
  UIni,
  UThemes;

type
  TScreenOptions = class(TMenu)
    public
      TextDescription:    integer;
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure InteractNext; override;
      procedure InteractPrev; override;
      procedure InteractNextRow; override;
      procedure InteractPrevRow; override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

implementation

uses
  UGraphic,
  UUnicodeUtils;

function TScreenOptions.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
  if (PressedDown) then
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
          Ini.Save;
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenMain);
        end;
      SDLK_RETURN:
        begin
          if SelInteraction = 0 then
          begin
            AudioPlayback.PlaySound(SoundLib.Start);
            FadeTo(@ScreenOptionsGame);
          end;

          if SelInteraction = 1 then
          begin
            AudioPlayback.PlaySound(SoundLib.Start);
            FadeTo(@ScreenOptionsGraphics);
          end;

          if SelInteraction = 2 then
          begin
            AudioPlayback.PlaySound(SoundLib.Start);
            FadeTo(@ScreenOptionsSound);
          end;

          if SelInteraction = 3 then
          begin
            AudioPlayback.PlaySound(SoundLib.Start);
            FadeTo(@ScreenOptionsLyrics);
          end;

          if SelInteraction = 4 then
          begin
            AudioPlayback.PlaySound(SoundLib.Start);
            FadeTo(@ScreenOptionsThemes);
          end;

          if SelInteraction = 5 then
          begin
            AudioPlayback.PlaySound(SoundLib.Start);
            FadeTo(@ScreenOptionsRecord);
          end;

          if SelInteraction = 6 then
          begin
            AudioPlayback.PlaySound(SoundLib.Start);
            FadeTo(@ScreenOptionsAdvanced);
          end;

          if SelInteraction = 7 then
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
//var
// I:    integer; // Auto Removed, Unused Variable
begin
  inherited Create;

  TextDescription := AddText(Theme.Options.TextDescription);

  LoadFromTheme(Theme.Options);

  AddButton(Theme.Options.ButtonGame);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[0]);

  AddButton(Theme.Options.ButtonGraphics);
  if (Length(Button[1].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[1]);

  AddButton(Theme.Options.ButtonSound);
  if (Length(Button[2].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[2]);

  AddButton(Theme.Options.ButtonLyrics);
  if (Length(Button[3].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[3]);

  AddButton(Theme.Options.ButtonThemes);
  if (Length(Button[4].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[4]);

  AddButton(Theme.Options.ButtonRecord);
  if (Length(Button[5].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[5]);

  AddButton(Theme.Options.ButtonAdvanced);
  if (Length(Button[6].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[6]);

  AddButton(Theme.Options.ButtonExit);
  if (Length(Button[7].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);

  Interaction := 0;
end;

procedure TScreenOptions.OnShow;
begin
  inherited;
end;

procedure TScreenOptions.InteractNext;
begin
  inherited InteractNext;
  Text[TextDescription].Text := Theme.Options.Description[Interaction];
end;

procedure TScreenOptions.InteractPrev;
begin
  inherited InteractPrev;
  Text[TextDescription].Text := Theme.Options.Description[Interaction];
end;

procedure TScreenOptions.InteractNextRow;
begin
  inherited InteractNextRow;
  Text[TextDescription].Text := Theme.Options.Description[Interaction];
end;

procedure TScreenOptions.InteractPrevRow;
begin
  inherited InteractPrevRow;
  Text[TextDescription].Text := Theme.Options.Description[Interaction];
end;

procedure TScreenOptions.SetAnimationProgress(Progress: real);
begin
  Button[0].Texture.ScaleW := Progress;
  Button[1].Texture.ScaleW := Progress;
  Button[2].Texture.ScaleW := Progress;
  Button[3].Texture.ScaleW := Progress;
  Button[4].Texture.ScaleW := Progress;
  Button[5].Texture.ScaleW := Progress;
  Button[6].Texture.ScaleW := Progress;
  Button[7].Texture.ScaleW := Progress;
end;

end.
