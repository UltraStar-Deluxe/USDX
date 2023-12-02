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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/screens/UScreenPartyWin.pas $
 * $Id: UScreenPartyWin.pas 2246 2010-04-18 13:43:36Z tobigun $
 *}

unit UScreenPartyTournamentWin;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UCommon,
  UDisplay,
  UIni,
  UMenu,
  UMusic,
  UThemes,
  sdl2,
  SysUtils;

type
  TScreenPartyTournamentWin = class(TMenu)
    public
      TextScorePlayer1:    cardinal;
      TextScorePlayer2:    cardinal;

      TextNamePlayer1:     cardinal;
      TextNamePlayer2:     cardinal;

      StaticBGPlayer1:     cardinal;
      StaticBGPlayer2:     cardinal;

      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure UpdateRounds;
      procedure SetAnimationProgress(Progress: real); override;
  end;

const
  ID='ID_039';   //for help system

implementation

uses
  UGraphic,
  UHelp,
  ULanguage,
  ULog,
  UMain,
  UNote,
  UPartyTournament,
  UScreenPartyTournamentRounds,
  UUnicodeUtils;

function TScreenPartyTournamentWin.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
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
      SDLK_BACKSPACE,
      SDLK_RETURN :
        begin
          AudioPlayback.PlaySound(SoundLib.Start);

          if ((StrToInt(Text[TextScorePlayer1].Text) > PartyTournament.Rounds[PartyTournament.Phase] / 2) or (StrToInt(Text[TextScorePlayer2].Text) > PartyTournament.Rounds[PartyTournament.Phase] / 2)) then
          begin

            if (PartyTournament.Phase <> 3) then
            begin
              SetLength(PartyTournament.EliminatedPlayers, Length(PartyTournament.EliminatedPlayers) + 1);

              if (StrToInt(Text[TextScorePlayer1].Text) > StrToInt(Text[TextScorePlayer2].Text)) then
                PartyTournament.EliminatedPlayers[High(PartyTournament.EliminatedPlayers)] := PartyTournament.Next.Player2
              else
                PartyTournament.EliminatedPlayers[High(PartyTournament.EliminatedPlayers)] := PartyTournament.Next.Player1;

            end
            else
            begin
              if (StrToInt(Text[TextScorePlayer1].Text) > StrToInt(Text[TextScorePlayer2].Text)) then
                PartyTournament.Winner := PartyTournament.Next.NamePlayer1
              else
                PartyTournament.Winner := PartyTournament.Next.NamePlayer2;

              PartyTournament.TournamentFinish := true;

            end;
          end;

          PartyTournament.LastPlayer := PartyTournament.Next.Player2;

          if (PartyTournament.Phase = 3) then
          begin
            if not (PartyTournament.TournamentFinish) then
              FadeTo(@ScreenSong)
            else
              FadeTo(@ScreenPartyTournamentRounds);
          end
          else
            FadeTo(@ScreenPartyTournamentRounds);

        end;
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;
    end;
  end;
end;

constructor TScreenPartyTournamentWin.Create;
begin
  inherited Create;

  LoadFromTheme(Theme.PartyTournamentWin);

  TextScorePlayer1 := AddText (Theme.PartyTournamentWin.TextScorePlayer1);
  TextScorePlayer2 := AddText (Theme.PartyTournamentWin.TextScorePlayer2);

  TextNamePlayer1 := AddText (Theme.PartyTournamentWin.TextNamePlayer1);
  TextNamePlayer2 := AddText (Theme.PartyTournamentWin.TextNamePlayer2);

  StaticBGPlayer1 := AddStatic (Theme.PartyTournamentWin.StaticBGPlayer1);
  StaticBGPlayer2 := AddStatic (Theme.PartyTournamentWin.StaticBGPlayer2);
end;

procedure TScreenPartyTournamentWin.OnShow;
var
  Col: TRGB;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenPartyTournamentWin)');

  Col := GetPlayerColor(Ini.SingColor[0]);

  Statics[StaticBGPlayer1].Texture.ColR := Col.R;
  Statics[StaticBGPlayer1].Texture.ColG := Col.G;
  Statics[StaticBGPlayer1].Texture.ColB := Col.B;

  Col := GetPlayerColor(Ini.SingColor[1]);

  Statics[StaticBGPlayer2].Texture.ColR := Col.R;
  Statics[StaticBGPlayer2].Texture.ColG := Col.G;
  Statics[StaticBGPlayer2].Texture.ColB := Col.B;

  Text[TextNamePlayer1].Text := PartyTournament.Next.NamePlayer1;
  Text[TextNamePlayer2].Text := PartyTournament.Next.NamePlayer2;

  UpdateRounds;

  Text[TextScorePlayer1].Text := PartyTournament.ResultPlayer[PartyTournament.Phase, PartyTournament.Next.Player1].Text;
  Text[TextScorePlayer2].Text := PartyTournament.ResultPlayer[PartyTournament.Phase, PartyTournament.Next.Player2].Text;

end;

procedure TScreenPartyTournamentWin.UpdateRounds;
var
  p: integer;
begin
  if (PartyTournament.ResultPlayer[PartyTournament.Phase, PartyTournament.Next.Player1].Text = '') then
    PartyTournament.ResultPlayer[PartyTournament.Phase, PartyTournament.Next.Player1].Text := '0';

  if (PartyTournament.ResultPlayer[PartyTournament.Phase, PartyTournament.Next.Player2].Text = '') then
    PartyTournament.ResultPlayer[PartyTournament.Phase, PartyTournament.Next.Player2].Text := '0';

  if (ScreenSing.SungToEnd) then
  begin
    if (player[0].ScoreTotalInt > player[1].ScoreTotalInt) then
      PartyTournament.ResultPlayer[PartyTournament.Phase, PartyTournament.Next.Player1].Text := IntToStr(StrToInt(PartyTournament.ResultPlayer[PartyTournament.Phase, PartyTournament.Next.Player1].Text) + 1);

    if (player[0].ScoreTotalInt < player[1].ScoreTotalInt) then
      PartyTournament.ResultPlayer[PartyTournament.Phase, PartyTournament.Next.Player2].Text := IntToStr(StrToInt(PartyTournament.ResultPlayer[PartyTournament.Phase, PartyTournament.Next.Player2].Text) + 1);
  end;

end;

procedure TScreenPartyTournamentWin.SetAnimationProgress(Progress: real);
begin
  {if (ScreenSingModi.PlayerInfo.NumPlayers >= 1) then
    Statics[StaticTeam1].Texture.ScaleW := Progress * ScreenSingModi.PlayerInfo.Playerinfo[0].Score / maxScore;
  if (ScreenSingModi.PlayerInfo.NumPlayers >= 2) then
    Statics[StaticTeam2].Texture.ScaleW := Progress * ScreenSingModi.PlayerInfo.Playerinfo[1].Score / maxScore;
  if (ScreenSingModi.PlayerInfo.NumPlayers >= 3) then
    Statics[StaticTeam3].Texture.ScaleW := Progress * ScreenSingModi.PlayerInfo.Playerinfo[2].Score / maxScore;}
end;

end.
