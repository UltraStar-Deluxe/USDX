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
 * $URL: $
 * $Id: $
 *}

unit UScreenAbout;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UDisplay,
  UIni,
  UMenu,
  UMusic,
  UThemes,
  sdl2,
  SysUtils;

type
  TScreenAbout = class(TMenu)
    private
      ShowCreditsRequested: boolean;
    public
      TextOverview:    integer;
      procedure ShowCreditsInAbout;
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure OnHide; override;
      procedure SetAnimationProgress(Progress: real); override;

      procedure SetOverview;
  end;

const
  ID='ID_002';   //for help system

implementation

uses
  UCommon,
  UDataBase,
  UGraphic,
  UHelp,
  ULanguage,
  ULog,
  USong,
  USongs,
  UUnicodeUtils,
  Classes;

function TScreenAbout.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check normal keys
    case PressedKey of
      SDLK_C:
        begin
          if not ShowCreditsRequested then
            ShowCreditsInAbout
          else begin
            ShowCreditsRequested := false;
            SetOverview;
          end;
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
          //Exit Button Pressed
          if Interaction = 1 then
          begin
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(@ScreenMain);
          end;

          // ultrastar deluxe team credits
          if Interaction = 0 then
          begin
            if not ShowCreditsRequested then
              ShowCreditsInAbout
            else begin
              ShowCreditsRequested := false;
              SetOverview;
            end;
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
      SDLK_UP:
      begin
          InteractPrev;
      end;
      SDLK_DOWN:
      begin
          InteractNext;
      end;
    end;
  end;
end;

constructor TScreenAbout.Create;
begin
  inherited Create;

  TextOverview := AddText(Theme.AboutMain.TextOverview);

  LoadFromTheme(Theme.AboutMain);

  AddButton(Theme.AboutMain.ButtonCredits);
  AddButton(Theme.AboutMain.ButtonExit);

  Interaction := 0;
end;

procedure TScreenAbout.OnShow;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenAbout');

  if not ShowCreditsRequested then
    SetOverview;
end;

procedure TScreenAbout.SetOverview;
var
  Overview: UTF8String;
begin
  // Format overview
  Overview := Language.Translate('ABOUT_OVERVIEW');
  Text[0].Text := Overview;
end;

procedure TScreenAbout.SetAnimationProgress(Progress: real);
var
  I: integer;
begin
  for I := 0 to high(Button) do
    Button[I].Texture.ScaleW := Progress;
end;

procedure TScreenAbout.ShowCreditsInAbout;
var
  S: UTF8String;
  SL: TStringList;
begin
  ShowCreditsRequested := true;
  S := 'Thank you to everyone who contributed to UltraStar Deluxe, including pre-Github contributors: alexanders, blindy, brunzel, canni, hennymcc, jaybinks, krueger, mezzox, mischi, mog, and whiteshark                                                                   Thanks to all our more recent contributors: s09bQ5, basisbit, barbeque-squared, RattleSN4K3, dgruss, AlexanderS, DeinAlptraum, daniel-j, complexlogic, bohning, HermannDppes, TheNailDev, ePirat, brianch, memcopy, kamischi, rhaamo, TheNotary, GaryCXJk, shazzzzam, jose1711, mrtnmtth, sleumas2000, tobijdc, pkerling, mkinoo, ricardosdl, jmfergeau, PonPonTheDreambunny, sarrchri, j-lag, hoehermann, finn-wa, Goostav5, letscodehu, raulmt, luto, douardda, marcszy91, seigneurfuo, DoubleDee73, mobacon, Bronkoknorb, Travisrowe, qamil95, gitter-badger, taligentx, and everyone who helped with reporting bugs, testing, translating, creating themes or in any other way!';

  // Assign text to the about text control. Some code paths set Text[0],
  // some store the index in TextOverview — update both to be safe.
  if (Length(Text) > 0) then
    Text[0].Text := S;

  if (TextOverview >= 0) and (TextOverview < Length(Text)) then
    Text[TextOverview].Text := S;
end;

procedure TScreenAbout.OnHide;
begin
  inherited;
  ShowCreditsRequested := false;
end;

end.
