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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/branches/experimental/Lua/src/screens/UScreenPartyOptions.pas $
 * $Id: UScreenPartyOptions.pas 2036 2009-12-14 20:59:44Z whiteshark0 $
 *}

unit UScreenPartyRounds;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UCommon,
  UDisplay,
  UFiles,
  UMenu,
  UMusic,
  UThemes,
  sdl2,
  SysUtils;

type
  TScreenPartyRounds = class(TMenu)
    private
      Actual: integer;

      SelectRoundCount: cardinal;
      SelectRound: array [0..6] of cardinal;

      RoundCount: integer;
      Round: array [0..6] of integer;
      RoundMode: array [0..98] of integer;

      IModeNames: array of UTF8String;
      IModeIDs: array of integer;

      procedure UpdateInterface;
      procedure StartParty;
    public
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; Repeated: boolean = false): boolean; override;
      procedure OnShow; override;
      procedure InteractNext; override;
      procedure InteractPrev; override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

const
  ID='ID_032';   //for help system

implementation

uses
  UGraphic,
  UHelp,
  UIni,
  ULanguage,
  ULog,
  UMain,
  UMenuSelectSlide,
  UParty,
  UPlaylist,
  USong,
  USongs,
  UTexture,
  UUnicodeUtils;

procedure TScreenPartyRounds.InteractNext;
var
  I: integer;
begin

  if (Interaction = high(Theme.PartyRounds.SelectRound) + 1) and (Actual + high(Theme.PartyRounds.SelectRound) <= RoundCount) then
  begin
    Actual := Actual + 1;

    // update slides
    for I := 0 to high(Theme.PartyRounds.SelectRound) do
    begin
      SelectsS[SelectRound[I]].Text.Text := Format(Language.Translate('PARTY_SELECTMODE'), [(Actual + I + 1)]);
      SelectsS[SelectRound[I]].SetSelectOpt(RoundMode[Actual + I]);
    end;

  end
  else
    inherited InteractNext;

end;

procedure TScreenPartyRounds.InteractPrev;
var
  I: integer;
begin

  if (Interaction = 1) and (Actual > 0) then
  begin
    Actual := Actual - 1;

    // update slides
    for I := 0 to high(Theme.PartyRounds.SelectRound) do
    begin
      SelectsS[SelectRound[I]].Text.Text := Format(Language.Translate('PARTY_SELECTMODE'), [(Actual + I + 1)]);
      SelectsS[SelectRound[I]].SetSelectOpt(RoundMode[Actual + I]);
    end;

  end
  else
    inherited InteractPrev;

end;

procedure TScreenPartyRounds.UpdateInterface;
  var
    I: integer;
    ActualRounds: integer;
begin
  ActualRounds := RoundCount + 2;

  for I := 0 to High(SelectRound) do
    SelectsS[SelectRound[I]].Visible := (I < ActualRounds);
end;

procedure TScreenPartyRounds.StartParty;
  var
    GameRounds: ARounds;
    I: integer;
begin
  SetLength(GameRounds, RoundCount + 2);

  for I := 0 to High(GameRounds) do
    GameRounds[I] := IModeIds[RoundMode[I]];

  // start party game
  if (Party.StartGame(GameRounds)) then
  begin
    FadeTo(@ScreenPartyNewRound, SoundLib.Start);
  end
  else
  begin
    //error starting party game
    ScreenPopupError.ShowPopup(Language.Translate('ERROR_CAN_NOT_START_PARTY'));
  end;
end;

function TScreenPartyRounds.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; Repeated: boolean = false): boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check normal keys
    case PressedKey of
      SDLK_Q:
        begin
          Result := false;
          Exit;
        end;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE:
        begin
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenPartyPlayer);
        end;

      SDLK_TAB:
      begin
        ScreenPopupHelp.ShowPopup();
      end;

      SDLK_RETURN: StartParty;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:    InteractNext;
      SDLK_UP:      InteractPrev;
      SDLK_RIGHT:
        begin
          AudioPlayback.PlaySound(SoundLib.Option);
          InteractInc;

          if Interaction = 0 then
            UpdateInterface
          else
            RoundMode[Actual + Interaction - 1] := Round[Interaction - 1];
        end;
      SDLK_LEFT:
        begin
          AudioPlayback.PlaySound(SoundLib.Option);
          InteractDec;

          if Interaction = 0 then
            UpdateInterface
          else
            RoundMode[Actual + Interaction - 1] := Round[Interaction - 1];
        end;
    end;
  end;
end;

constructor TScreenPartyRounds.Create;
var
  I: integer;
  IRoundCount:  array[0..97] of UTF8String;
begin
  inherited Create;
  RoundCount := 5;

  for I := 0 to 97 do
    IRoundCount[I] := IntToStr(I + 2);

  //Load Screen From Theme
  LoadFromTheme(Theme.PartyRounds);

  Theme.PartyRounds.SelectRoundCount.oneItemOnly := true;
  Theme.PartyRounds.SelectRoundCount.showArrows := true;
  SelectRoundCount := AddSelectSlide(Theme.PartyRounds.SelectRoundCount, RoundCount, IRoundCount);

  SetLength(IModeNames, 1);
  IModeNames[0] := '---';
  for I := 0 to high(Theme.PartyRounds.SelectRound) do
  begin
    Theme.PartyRounds.SelectRound[I].oneItemOnly := true;
    Theme.PartyRounds.SelectRound[I].showArrows := true;
    SelectRound[I] := AddSelectSlide(Theme.PartyRounds.SelectRound[I], Round[I], IModeNames);
    SelectsS[SelectRound[I]].Text.Text := Format(Language.Translate('PARTY_SELECTMODE'), [(I + 1)]);
  end;

  for I := 0 to High(RoundMode) do
    RoundMode[I] := 0;

  Interaction := 0;
  Actual := 0;
end;

procedure TScreenPartyRounds.OnShow;
  var
    ModeList: AParty_ModeList;
    temp: TParty_RoundList;
    I, J: integer;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenPartyRounds)');

  // check if there are loaded modes
  if Party.ModesAvailable then
  begin
    UpdateInterface;

    ModeList := Party.GetAvailableModes;
    SetLength(IModeNames, Length(ModeList));
    SetLength(IModeIds, Length(ModeList));

    // sort
    for I := Length(ModeList) - 1 downto 1 do
    begin
      for J := 2 to i do
      begin
        If UTF8CompareText(ModeList[J - 1].Name, ModeList[J].Name) > 0 then
        begin
          temp := ModeList[J - 1];
          ModeList[J - 1] := ModeList[J];
          ModeList[J] := temp;
        end;
      end;
    end;

    for I := 0 to High(ModeList) do
    begin
      IModeNames[I] := ModeList[I].Name;
      IModeIds[I]   := ModeList[I].Index;
    end;

    for I := 0 to High(SelectRound) do
      UpdateSelectSlideOptions(SelectRound[I], IModeNames, Round[I]);
  end
  else
  begin
    // no mode available for current player setup
    ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_MODES_FOR_CURRENT_SETUP'));
    Party.Clear;
    Display.AbortScreenChange;
  end;
end;

procedure TScreenPartyRounds.SetAnimationProgress(Progress: real);
begin
  {for I := 0 to 6 do
    SelectS[I].Texture.ScaleW := Progress;}
end;

end.
