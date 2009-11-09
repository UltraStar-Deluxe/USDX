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

unit UScreenPopup;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SDL,
  SysUtils,
  UMenu,
  UMusic,
  UFiles,
  UThemes;

type
  TPopupCheckHandler = procedure(Value: boolean; Data: Pointer);

  TScreenPopupCheck = class(TMenu)
    private
      fHandler: TPopupCheckHandler;
      fHandlerData: Pointer;

    public
      Visible: boolean; // whether the menu should be drawn

      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure ShowPopup(const Msg: UTF8String; Handler: TPopupCheckHandler;
          HandlerData: Pointer; DefaultValue: boolean = false);
      function Draw: boolean; override;
  end;

type
  TScreenPopup = class(TMenu)
    {
    private
      CurMenu: byte; //Num of the cur. Shown Menu
    }
    public
      Visible: boolean; //Whether the Menu should be Drawn

      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure OnHide; override;
      procedure ShowPopup(const Msg: UTF8String);
      function Draw: boolean; override;
  end;

  TScreenPopupError = class(TScreenPopup)
    public
      constructor Create;
  end;

  TScreenPopupInfo = class(TScreenPopup)
    public
      constructor Create;
  end;

var
  //ISelections: array of string;
  SelectValue: integer;

implementation

uses
  UGraphic,
  UMain,
  UIni,
  UTexture,
  ULanguage,
  UParty,
  UPlaylist,
  UDisplay,
  UUnicodeUtils;

{ TScreenPopupCheck }

function TScreenPopupCheck.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  Value: boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          Value := false;
          Visible := false;
          Result := false;
        end;

      SDLK_RETURN:
        begin
          Value := (Interaction = 0);
          Visible := false;
          Result := false;
        end;

      SDLK_DOWN:  InteractNext;
      SDLK_UP:    InteractPrev;
 
      SDLK_RIGHT: InteractNext;
      SDLK_LEFT:  InteractPrev;
    end;
  end;

  if (not Result) then
  begin
    if (@fHandler <> nil) then
      fHandler(Value, fHandlerData);
  end;
end;

constructor TScreenPopupCheck.Create;
begin
  inherited Create;

  fHandler := nil;
  fHandlerData := nil;

  AddText(Theme.CheckPopup.TextCheck);
  
  LoadFromTheme(Theme.CheckPopup);

  AddButton(Theme.CheckPopup.Button1);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, 'Button 1');

  AddButton(Theme.CheckPopup.Button2);
  if (Length(Button[1].Text) = 0) then
    AddButtonText(14, 20, 'Button 2');

  Interaction := 0;
end;

function TScreenPopupCheck.Draw: boolean;
begin
  Result := inherited Draw;
end;

procedure TScreenPopupCheck.OnShow;
begin
  inherited;
end;

procedure TScreenPopupCheck.ShowPopup(const Msg: UTF8String; Handler: TPopupCheckHandler;
    HandlerData: Pointer; DefaultValue: boolean);
begin
  if (DefaultValue) then
    Interaction := 0
  else
    Interaction := 1;
  Visible := true;  //Set Visible
  fHandler := Handler;
  fHandlerData := HandlerData;

  Text[0].Text := Language.Translate(msg);

  Button[0].Visible := true;
  Button[1].Visible := true;

  Button[0].Text[0].Text := Language.Translate('SONG_MENU_YES');
  Button[1].Text[0].Text := Language.Translate('SONG_MENU_NO');

  Background.OnShow
end;

{ TScreenPopup }

function TScreenPopup.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down

    case PressedKey of
      SDLK_Q:
        begin
          Result := false;
        end;

      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          Visible := false;
          Result := false;
        end;

      SDLK_RETURN:
        begin
          Visible := false;
          Result := false;
        end;

      SDLK_DOWN:    InteractNext;
      SDLK_UP:      InteractPrev;

      SDLK_RIGHT: InteractNext;
      SDLK_LEFT: InteractPrev;
    end;
  end;
end;

constructor TScreenPopup.Create;
begin
  inherited Create;

  AddText(Theme.ErrorPopup.TextError);

  LoadFromTheme(Theme.ErrorPopup);

  AddButton(Theme.ErrorPopup.Button1);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, 'Button 1');

  Interaction := 0;
end;

function TScreenPopup.Draw: boolean;
begin
  Draw := inherited Draw;
end;

procedure TScreenPopup.OnShow;
begin
  inherited;

end;

procedure TScreenPopup.OnHide;
begin
end;

procedure TScreenPopup.ShowPopup(const Msg: UTF8String);
begin
  Interaction := 0; //Reset Interaction
  Visible := true;  //Set Visible
  Background.OnShow;

{  //dirty hack... Text[0] is invisible for some strange reason
  for i:=1 to high(Text) do
    if i-1 <= high(msg) then
    begin
      Text[i].Visible := true;
      Text[i].Text := msg[i-1];
    end
    else
    begin
      Text[i].Visible := false;
    end;}
  Text[0].Text := msg;

  Button[0].Visible := true;

  Button[0].Text[0].Text := 'OK';
end;

{ TScreenPopupError }

constructor TScreenPopupError.Create;
begin
  inherited;
  Text[1].Text := Language.Translate('MSG_ERROR_TITLE');
end;

{ TScreenPopupInfo }

constructor TScreenPopupInfo.Create;
begin
  inherited;
  Text[1].Text := Language.Translate('MSG_INFO_TITLE');
end;

end.
