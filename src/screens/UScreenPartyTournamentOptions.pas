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

unit UScreenPartyTournamentOptions;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UDisplay,
  UFiles,
  UMenu,
  UMusic,
  UThemes,
  sdl2,
  SysUtils;

type
  TScreenPartyTournamentOptions = class(TMenu)
    private
      SelectRoundsFinal: cardinal;
      SelectRounds2Final: cardinal;
      SelectRounds4Final: cardinal;
      SelectRounds8Final: cardinal;

      RoundFinalCount:  integer;
      Round2FinalCount: integer;
      Round4FinalCount: integer;
      Round8FinalCount: integer;

    public
      constructor Create; override;
      procedure UpdateTournament;
      function ParseInput(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean; override;
      procedure OnShow; override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

const
  ID='ID_037';   //for help system
  IRoundCount:  array[0..2] of UTF8String = ('1', '3', '5');

implementation

uses
  UGraphic,
  UHelp,
  UIni,
  ULanguage,
  ULog,
  UMain,
  UNote,
  UPartyTournament,
  UPlaylist,
  USong,
  USongs,
  UTexture,
  UUnicodeUtils;

procedure TScreenPartyTournamentOptions.UpdateTournament();
begin

  if (SelectsS[SelectRounds8Final].Visible) then
    PartyTournament.Rounds[0] := StrToInt(IRoundCount[Round8FinalCount]);

  if (SelectsS[SelectRounds4Final].Visible) then
    PartyTournament.Rounds[1] := StrToInt(IRoundCount[Round4FinalCount]);

  if (SelectsS[SelectRounds2Final].Visible) then
    PartyTournament.Rounds[2] := StrToInt(IRoundCount[Round2FinalCount]);

  PartyTournament.Rounds[3] := StrToInt(IRoundCount[RoundFinalCount]);

end;

function TScreenPartyTournamentOptions.ParseInput(PressedKey: QWord; CharCode: UCS4Char; PressedDown: boolean; Parameter: integer): boolean;
var
  I, J : integer;
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
      SDLK_BACKSPACE :
        begin
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenPartyTournamentPlayer);
        end;

      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;

      SDLK_RETURN:
      begin
        UpdateTournament;
        FadeTo(@ScreenPartyTournamentRounds, SoundLib.Start);
      end;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:    InteractNext;
      SDLK_UP:      InteractPrev;
      SDLK_RIGHT:
        begin
          AudioPlayback.PlaySound(SoundLib.Option);
          InteractInc;
        end;
      SDLK_LEFT:
        begin
          AudioPlayback.PlaySound(SoundLib.Option);
          InteractDec;
        end;
    end;
  end;
end;

constructor TScreenPartyTournamentOptions.Create;
  var
    I: integer;
begin
  inherited Create;

  //Load Screen From Theme
  LoadFromTheme(Theme.PartyTournamentOptions);

  Theme.PartyTournamentOptions.SelectRoundsFinal.oneItemOnly := true;
  Theme.PartyTournamentOptions.SelectRoundsFinal.showArrows := true;
  SelectRoundsFinal := AddSelectSlide(Theme.PartyTournamentOptions.SelectRoundsFinal, RoundFinalCount, IRoundCount);

  Theme.PartyTournamentOptions.SelectRounds2Final.oneItemOnly := true;
  Theme.PartyTournamentOptions.SelectRounds2Final.showArrows := true;
  SelectRounds2Final := AddSelectSlide(Theme.PartyTournamentOptions.SelectRounds2Final, Round2FinalCount, IRoundCount);

  Theme.PartyTournamentOptions.SelectRounds4Final.oneItemOnly := true;
  Theme.PartyTournamentOptions.SelectRounds4Final.showArrows := true;
  SelectRounds4Final := AddSelectSlide(Theme.PartyTournamentOptions.SelectRounds4Final, Round4FinalCount, IRoundCount);

  Theme.PartyTournamentOptions.SelectRounds8Final.oneItemOnly := true;
  Theme.PartyTournamentOptions.SelectRounds8Final.showArrows := true;
  SelectRounds8Final := AddSelectSlide(Theme.PartyTournamentOptions.SelectRounds8Final, Round8FinalCount, IRoundCount);

  Interaction := 0;
end;

procedure TScreenPartyTournamentOptions.OnShow;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenPartyTournamentOptions)');

  SelectsS[SelectRounds2Final].Visible := false;
  SelectsS[SelectRounds4Final].Visible := false;
  SelectsS[SelectRounds8Final].Visible := false;

  if (PartyTournament.PlayersCount > 2) then
    SelectsS[SelectRounds2Final].Visible := true;

  if (PartyTournament.PlayersCount > 4) then
    SelectsS[SelectRounds4Final].Visible := true;

  if (PartyTournament.PlayersCount > 8) then
    SelectsS[SelectRounds8Final].Visible := true;

end;

procedure TScreenPartyTournamentOptions.SetAnimationProgress(Progress: real);
begin
  {for I := 0 to 6 do
    SelectS[I].Texture.ScaleW := Progress;}
end;

end.
