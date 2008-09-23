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

unit UScreenEdit;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses UMenu, SDL, UThemes;

type
  TScreenEdit = class(TMenu)
    public
{      Tex_Background:     TTexture;
      FadeOut:            boolean;
      Path:               string;
      FileName:           string;}
      constructor Create; override;
      procedure onShow; override;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
{      function Draw: boolean; override;
      procedure Finish;}
  end;

implementation

uses UGraphic, UMusic, USkins, SysUtils;

function TScreenEdit.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
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
          FadeTo(@ScreenMain);
//          Result := false;
        end;
      SDLK_RETURN:
        begin
          if Interaction = 0 then
          begin
            AudioPlayback.PlaySound(SoundLib.Start);
            FadeTo(@ScreenEditConvert);
          end;
//          if Interaction = 1 then begin
//            Music.PlayStart;
//            FadeTo(@ScreenEditHeader);
//          end;

          if Interaction = 1 then
          begin
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(@ScreenMain);
          end;
        end;

      SDLK_DOWN:
        begin
          InteractNext;
        end;
      SDLK_UP:
        begin
          InteractPrev;
        end;
    end;
  end;
end;

constructor TScreenEdit.Create;
begin
  inherited Create;
  AddButton(400-200, 100 + 0*70, 400, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(10, 5, 0, 0, 0, 'Convert Midi to Txt');
//  Button[High(Button)].Text[0].Size := 11;

//  AddButton(400-200, 100 + 1*60, 400, 40, 'ButtonF');
//  AddButtonText(10, 5, 0, 0, 0, 'Edit Headers');

//  AddButton(400-200, 100 + 2*60, 400, 40, 'ButtonF');
//  AddButtonText(10, 5, 0, 0, 0, 'Set GAP');

  AddButton(400-200, 100 + 3*60, 400, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(10, 5, 0, 0, 0, 'Exit');

end;

procedure TScreenEdit.onShow;
begin
  inherited;

//  Interaction := 0;
end;

(*function TScreenEdit.Draw: boolean;
var
  Min:    integer;
  Sec:    integer;
  Tekst:  string;
  Pet:    integer;
  AktBeat:  integer;
begin
end;

procedure TScreenEdit.Finish;
begin
//
end;*)

end.
