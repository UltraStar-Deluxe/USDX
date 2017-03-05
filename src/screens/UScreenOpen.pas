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
  Math,
  SysUtils,
  gl,
  SDL,
  UPath,
  UMenu,
  UMusic,
  UFiles,
  UTime,
  USongs,
  UIni,
  ULog,
  UTexture,
  UMenuText,
  ULyrics,
  UThemes;

type
  TScreenOpen = class(TMenu)
    private
      //fTextF:      array[0..1] of integer;
      fTextN:      integer; // text-box ID of filename
      fFilename:   IPath;
      fBackScreen: PMenu;

      procedure AddBox(X, Y, W, H: real);
    public
      constructor Create; override;
      procedure OnShow; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;

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

implementation

uses
  UGraphic,
  UDraw,
  UMain,
  USkins,
  UUnicodeUtils;

function TScreenOpen.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;

  if (PressedDown) then  // Key Down
  begin
    // check normal keys
    if (IsPrintableChar(CharCode)) then
    begin
      if (Interaction = 0) then
      begin
        Text[fTextN].Text := Text[fTextN].Text + UCS4ToUTF8String(CharCode);
        Exit;
      end;
    end;

    // check special keys
    case PressedKey of
      SDLK_BACKSPACE: // del
        begin
          if Interaction = 0 then
          begin
            Text[fTextN].DeleteLastLetter;
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
          if (Interaction = 2) then
          begin
            //Update Filename and go to last Screen
            fFileName := Path(Text[fTextN].Text);
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(fBackScreen);
          end
          else if (Interaction = 1) then
          begin
            //Empty Filename and go to last Screen
            fFileName := PATH_NONE;
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(fBackScreen);
          end;
        end;

      SDLK_LEFT:
        begin
          InteractPrev;
        end;

      SDLK_RIGHT:
        begin
          InteractNext;
        end;

      SDLK_DOWN:
        begin
        end;

      SDLK_UP:
        begin
        end;
    end;
  end;
end;

procedure TScreenOpen.AddBox(X, Y, W, H: real);
begin
  AddStatic(X,   Y,   W,   H,   0, 0, 0, Skin.GetTextureFileName('MainBar'), TEXTURE_TYPE_COLORIZED);
  AddStatic(X+2, Y+2, W-4, H-4, 1, 1, 1, Skin.GetTextureFileName('MainBar'), TEXTURE_TYPE_COLORIZED);
end;

constructor TScreenOpen.Create;
begin
  inherited Create;

  fFilename := PATH_NONE;

  // line
  {
  AddStatic(20, 10, 80, 30, 0, 0, 0, 'MainBar', 'JPG', TEXTURE_TYPE_COLORIZED);
  AddText(35, 17, 1, 18, 1, 1, 1, 'line');
  TextSentence := AddText(120, 14, 1, 24, 0, 0, 0, '0 / 0');
  }

  // file list
  //AddBox(400, 100, 350, 450);

  //TextF[0] :=  AddText(430, 155,  0, 24, 0, 0, 0, 'a');
  //TextF[1] :=  AddText(430, 180,  0, 24, 0, 0, 0, 'a');

  // file name
  AddBox(20, 540, 500, 40);
  fTextN := AddText(50, 548, 0, 24, 0, 0, 0, fFileName.ToUTF8);
  AddInteraction(iText, fTextN);

  // buttons
  {AddButton(540, 540, 100, 40, Skin.SkinPath + Skin.ButtonF);
  AddButtonText(10, 5, 0, 0, 0, 'Cancel');

  AddButton(670, 540, 100, 40, Skin.SkinPath + Skin.ButtonF);
  AddButtonText(30, 5, 0, 0, 0, 'OK');}
  // buttons
  AddButton(540, 540, 100, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(10, 5, 0, 0, 0, 'Cancel');

  AddButton(670, 540, 100, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(30, 5, 0, 0, 0, 'OK');

end;

procedure TScreenOpen.OnShow;
begin
  inherited;

  Interaction := 0;
  Text[fTextN].Text := fFilename.ToUTF8();
end;

(*
function TScreenEditSub.Draw: boolean;
var
  Min:     integer;
  Sec:     integer;
  AktBeat: integer;
begin

end;

procedure TScreenEditSub.Finish;
begin
//
end;
*)

end.
