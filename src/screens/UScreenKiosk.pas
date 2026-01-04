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
 *}

unit UScreenKiosk;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  sdl2,
  UIni,
  ULanguage,
  UMenu,
  UMusic,
  UThemes,
  UKioskMode;

type
  TKioskPasswordMode = (kpmSetup, kpmConfirm);

  TScreenKiosk = class(TOptionsMenu)
    private
      FAgeSelectIdx: integer;
      FAgeSelectValue: integer;
      FSetPasswordButtonIdx: integer;
      FActionButtonIdx: integer;
      FAgeLabels: array of UTF8String;
      FPendingPassword: UTF8String;
      FPasswordMode: TKioskPasswordMode;

      procedure InitializeAgeOptions;
      procedure UpdateAgeSelection;
      procedure UpdateButtons;
      procedure BeginPasswordSetup;
      procedure HandlePasswordPrompt(Value: boolean; Data: Pointer);
      procedure HandlePrimaryAction;
      procedure ShowStartConfirmation;
      procedure OnStartConfirmed(Value: boolean; Data: Pointer);
      procedure ActivateKiosk;
      procedure PromptUnlock;
      procedure OnUnlockAttempt(Value: boolean; Data: Pointer);
      procedure ReturnToMain;
      function SelectedAgeLimit: integer;
      function AgeIndexForLimit(Age: integer): integer;
    protected
      procedure LoadWidgets; override;
    public
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
  end;

var
  ScreenKiosk: TScreenKiosk;

implementation

uses
  UDisplay,
  UGraphic,
  UHelp,
  ULog,
  UMain,
  UScreenPopup,
  UScreenSong,
  USongs;

procedure ScreenKioskPasswordPromptHandler(Value: boolean; Data: Pointer); forward;
procedure ScreenKioskStartConfirmHandler(Value: boolean; Data: Pointer); forward;
procedure ScreenKioskUnlockHandler(Value: boolean; Data: Pointer); forward;

const
  ID = 'ID_KIOSK';
  AgeSteps: array[0..18] of integer =
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, KioskAgeNoLimit);
  INTERACT_AGE  = 0;
  INTERACT_SET  = 1;
  INTERACT_GO   = 2;
  INTERACT_BACK = 3;

{ TScreenKiosk }

constructor TScreenKiosk.Create;
begin
  inherited Create;
  Description := Language.Translate('SING_KIOSK_DESC');
  WhereAmI := Language.Translate('SING_KIOSK_WHEREAMI');
  InitializeAgeOptions;
  FAgeSelectValue := AgeIndexForLimit(Ini.KioskAgeLimit);
  Load;
end;

procedure TScreenKiosk.InitializeAgeOptions;
var
  I: integer;
begin
  SetLength(FAgeLabels, Length(AgeSteps));
  for I := 0 to High(AgeSteps) do
  begin
    if AgeSteps[I] = KioskAgeNoLimit then
      FAgeLabels[I] := Language.Translate('SING_KIOSK_AGE_NO_LIMIT')
    else
      FAgeLabels[I] := Format(Language.Translate('SING_KIOSK_AGE_OPTION'), [AgeSteps[I]]);
  end;
end;

function TScreenKiosk.AgeIndexForLimit(Age: integer): integer;
var
  I: integer;
begin
  if Age <= KioskAgeNoLimit then
    Exit(High(AgeSteps));

  for I := 0 to High(AgeSteps) - 1 do
    if AgeSteps[I] = Age then
      Exit(I);

  if Age > AgeSteps[High(AgeSteps) - 1] then
    Exit(High(AgeSteps))
  else
    Result := 0;
end;

procedure TScreenKiosk.LoadWidgets;
begin
  FAgeSelectIdx := AddSelectSlide('SING_KIOSK_AGE', FAgeSelectValue, FAgeLabels);
  FSetPasswordButtonIdx := AddButton('SING_KIOSK_SET_PASSWORD');
  FActionButtonIdx := AddButton('SING_KIOSK_START');
end;

procedure TScreenKiosk.UpdateAgeSelection;
begin
  FAgeSelectValue := AgeIndexForLimit(Ini.KioskAgeLimit);
  if (FAgeSelectIdx >= 0) and (FAgeSelectIdx <= High(SelectsS)) then
    SelectsS[FAgeSelectIdx].SelectedOption := FAgeSelectValue;
end;

procedure TScreenKiosk.UpdateButtons;
begin
  if (FActionButtonIdx >= 0) and (FActionButtonIdx <= High(Button)) and (Length(Button[FActionButtonIdx].Text) > 0) then
  begin
    if KioskMode.Active then
      Button[FActionButtonIdx].Text[0].Text := Language.Translate('SING_KIOSK_UNLOCK')
    else
      Button[FActionButtonIdx].Text[0].Text := Language.Translate('SING_KIOSK_START');
  end;

  if (FSetPasswordButtonIdx >= 0) and (FSetPasswordButtonIdx <= High(Button)) and (Length(Button[FSetPasswordButtonIdx].Text) > 0) then
  begin
    Button[FSetPasswordButtonIdx].Selectable := not KioskMode.Active;
    if KioskMode.Active then
      Button[FSetPasswordButtonIdx].Text[0].Text := Language.Translate('SING_KIOSK_PASSWORD_LOCKED')
    else if Ini.KioskPasswordHash = '' then
      Button[FSetPasswordButtonIdx].Text[0].Text := Language.Translate('SING_KIOSK_SET_PASSWORD')
    else
      Button[FSetPasswordButtonIdx].Text[0].Text := Language.Translate('SING_KIOSK_CHANGE_PASSWORD');
  end;
end;

procedure TScreenKiosk.BeginPasswordSetup;
begin
  FPasswordMode := kpmSetup;
  FPendingPassword := '';
  ScreenPopupInsertUser.ShowPasswordPrompt(
    Language.Translate('SING_KIOSK_PASSWORD'),
    Language.Translate('SING_KIOSK_PASSWORD_HINT'),
    @ScreenKioskPasswordPromptHandler,
    Self
  );
end;

procedure TScreenKiosk.HandlePasswordPrompt(Value: boolean; Data: Pointer);
var
  Entered: UTF8String;
  Hash: UTF8String;
begin
  if not Value then
  begin
    FPasswordMode := kpmSetup;
    FPendingPassword := '';
    Exit;
  end;

  Entered := ScreenPopupInsertUser.Password;

  if FPasswordMode = kpmSetup then
  begin
    if Length(Entered) < 4 then
    begin
      ScreenPopupError.ShowPopup(Language.Translate('SING_KIOSK_PASSWORD_REQUIRED'));
      Exit;
    end;

    FPendingPassword := Entered;
    FPasswordMode := kpmConfirm;
    ScreenPopupInsertUser.ShowPasswordPrompt(
      Language.Translate('SING_KIOSK_PASSWORD_CONFIRM'),
      Language.Translate('SING_KIOSK_PASSWORD_CONFIRM_DESC'),
      @ScreenKioskPasswordPromptHandler,
      Self
    );
    Exit;
  end;

  if Entered <> FPendingPassword then
  begin
    ScreenPopupError.ShowPopup(Language.Translate('SING_KIOSK_PASSWORD_MISMATCH'));
    Exit;
  end;

  Hash := TKioskMode.HashPassword(Entered);
  Ini.KioskPasswordHash := Hash;
  KioskMode.SetPasswordHash(Hash);
  Ini.Save;

  ScreenPopupInfo.ShowPopup(Language.Translate('SING_KIOSK_PASSWORD_SET'));
  FPendingPassword := '';
  FPasswordMode := kpmSetup;
  UpdateButtons;
end;

procedure TScreenKiosk.HandlePrimaryAction;
begin
  if KioskMode.Active then
    PromptUnlock
  else
    ShowStartConfirmation;
end;

procedure TScreenKiosk.ShowStartConfirmation;
begin
  if Ini.KioskPasswordHash = '' then
  begin
    ScreenPopupError.ShowPopup(Language.Translate('SING_KIOSK_NEEDS_PASSWORD'));
    Exit;
  end;
  ScreenPopupCheck.ShowPopup(Language.Translate('SING_KIOSK_LOCKED_INFO'), @ScreenKioskStartConfirmHandler, Self, true);
end;

procedure TScreenKiosk.OnStartConfirmed(Value: boolean; Data: Pointer);
begin
  if Value then
    ActivateKiosk;
end;

procedure TScreenKiosk.ActivateKiosk;
begin
  if not KioskMode.HasPassword then
  begin
    ScreenPopupError.ShowPopup(Language.Translate('SING_KIOSK_NEEDS_PASSWORD'));
    Exit;
  end;

  Ini.KioskAgeLimit := SelectedAgeLimit;
  KioskMode.ActivateWithHash(Ini.KioskAgeLimit);
  UpdateKioskWindowLock;
  if Assigned(ScreenSong) then
    ScreenSong.Refresh
  else if Assigned(CatSongs) then
    CatSongs.Refresh;
  Ini.Save;

  ScreenPopupInfo.ShowPopup(Language.Translate('SING_KIOSK_STARTED'));
  FadeTo(@ScreenMain, SoundLib.Start);
end;

procedure TScreenKiosk.PromptUnlock;
begin
  ScreenPopupInsertUser.ShowPasswordPrompt(
    Language.Translate('SING_KIOSK_EXIT_TITLE'),
    Language.Translate('SING_KIOSK_EXIT_DESC'),
    @ScreenKioskUnlockHandler,
    Self
  );
end;

procedure ScreenKioskPasswordPromptHandler(Value: boolean; Data: Pointer);
begin
  if Assigned(Data) then
    TScreenKiosk(Data).HandlePasswordPrompt(Value, Data);
end;

procedure ScreenKioskStartConfirmHandler(Value: boolean; Data: Pointer);
begin
  if Assigned(Data) then
    TScreenKiosk(Data).OnStartConfirmed(Value, Data);
end;

procedure ScreenKioskUnlockHandler(Value: boolean; Data: Pointer);
begin
  if Assigned(Data) then
    TScreenKiosk(Data).OnUnlockAttempt(Value, Data);
end;

procedure TScreenKiosk.OnUnlockAttempt(Value: boolean; Data: Pointer);
begin
  if not Value then
    Exit;

  if not KioskMode.Unlock(ScreenPopupInsertUser.Password) then
  begin
    ScreenPopupError.ShowPopup(Language.Translate('SING_KIOSK_PASSWORD_INVALID'));
    Exit;
  end;

  UpdateButtons;
  UpdateKioskWindowLock;
  if Assigned(ScreenSong) then
    ScreenSong.Refresh
  else if Assigned(CatSongs) then
    CatSongs.Refresh;
  Ini.Save;
  ScreenPopupInfo.ShowPopup(Language.Translate('SING_KIOSK_STOPPED'));
  FadeTo(@ScreenMain, SoundLib.Back);
end;

procedure TScreenKiosk.ReturnToMain;
begin
  AudioPlayback.PlaySound(SoundLib.Back);
  FadeTo(@ScreenMain);
end;

function TScreenKiosk.SelectedAgeLimit: integer;
begin
  if (FAgeSelectValue >= 0) and (FAgeSelectValue <= High(AgeSteps)) then
    Result := AgeSteps[FAgeSelectValue]
  else
    Result := KioskAgeNoLimit;
end;

function TScreenKiosk.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
  if PressedDown then
  begin
    case PressedKey of
      SDLK_Q:
        begin
          Result := false;
          Exit;
        end;
    end;

    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE:
        ReturnToMain;

      SDLK_TAB:
        ScreenPopupHelp.ShowPopup();

      SDLK_RETURN:
        begin
          case SelInteraction of
            INTERACT_SET:
              begin
                if KioskMode.Active then
                  ScreenPopupInfo.ShowPopup(Language.Translate('SING_KIOSK_PASSWORD_LOCKED'))
                else
                  BeginPasswordSetup;
              end;
            INTERACT_GO:  HandlePrimaryAction;
            INTERACT_BACK: ReturnToMain;
          end;
        end;

      SDLK_DOWN:  InteractNext;
      SDLK_UP:    InteractPrev;
      SDLK_RIGHT:
        begin
          if SelInteraction = INTERACT_AGE then
            AudioPlayback.PlaySound(SoundLib.Option);
          InteractInc;
        end;
      SDLK_LEFT:
        begin
          if SelInteraction = INTERACT_AGE then
            AudioPlayback.PlaySound(SoundLib.Option);
          InteractDec;
        end;
    end;
  end;
end;

procedure TScreenKiosk.OnShow;
begin
  inherited;
  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenKiosk');
  UpdateAgeSelection;
  UpdateButtons;
  FPasswordMode := kpmSetup;
end;

end.