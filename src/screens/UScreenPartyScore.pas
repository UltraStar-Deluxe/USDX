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

unit UScreenPartyScore;

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
  UThemes;

type
  TScreenPartyScore = class(TMenu)
    public
      TextScoreTeam1:    cardinal;
      TextScoreTeam2:    cardinal;
      TextScoreTeam3:    cardinal;
      TextNameTeam1:     cardinal;
      TextNameTeam2:     cardinal;
      TextNameTeam3:     cardinal;
      StaticTeam1:       cardinal;
      StaticTeam1BG:     cardinal;
      StaticTeam1Deco:   cardinal;
      StaticTeam2:       cardinal;
      StaticTeam2BG:     cardinal;
      StaticTeam2Deco:   cardinal;
      StaticTeam3:       cardinal;
      StaticTeam3BG:     cardinal;
      StaticTeam3Deco:   cardinal;
      TextWinner:        cardinal;

      DecoTex:          array[0..5] of integer;
      DecoColor:        array[0..5] of Record
                                        R, G, B: real;
                        end;

      MaxScore:          word;
      
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

implementation

uses
  UGraphic,
  UMain,
  UParty,
  UScreenSingModi,
  ULanguage,
  UTexture,
  USkins,
  UUnicodeUtils;

function TScreenPartyScore.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
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
          AudioPlayback.PlaySound(SoundLib.Start);
          if (PartySession.CurRound < High(PartySession.Rounds)) then
            FadeTo(@ScreenPartyNewRound)
          else
          begin
            PartySession.EndRound;
            FadeTo(@ScreenPartyWin);
          end;
        end;

      SDLK_RETURN:
        begin
          AudioPlayback.PlaySound(SoundLib.Start);
          if (PartySession.CurRound < High(PartySession.Rounds)) then
            FadeTo(@ScreenPartyNewRound)
          else
            FadeTo(@ScreenPartyWin);
        end;
    end;
  end;
end;

constructor TScreenPartyScore.Create;
var
// I:    integer; // Auto Removed, Unused Variable
  Tex:  TTexture;
  R, G, B: real;
  Color: integer;
begin
  inherited Create;

  TextScoreTeam1 := AddText (Theme.PartyScore.TextScoreTeam1);
  TextScoreTeam2 := AddText (Theme.PartyScore.TextScoreTeam2);
  TextScoreTeam3 := AddText (Theme.PartyScore.TextScoreTeam3);
  TextNameTeam1 := AddText (Theme.PartyScore.TextNameTeam1);
  TextNameTeam2 := AddText (Theme.PartyScore.TextNameTeam2);
  TextNameTeam3 := AddText (Theme.PartyScore.TextNameTeam3);

  StaticTeam1 := AddStatic (Theme.PartyScore.StaticTeam1);
  StaticTeam1BG := AddStatic (Theme.PartyScore.StaticTeam1BG);
  StaticTeam1Deco := AddStatic (Theme.PartyScore.StaticTeam1Deco);
  StaticTeam2 := AddStatic (Theme.PartyScore.StaticTeam2);
  StaticTeam2BG := AddStatic (Theme.PartyScore.StaticTeam2BG);
  StaticTeam2Deco := AddStatic (Theme.PartyScore.StaticTeam2Deco);
  StaticTeam3 := AddStatic (Theme.PartyScore.StaticTeam3);
  StaticTeam3BG := AddStatic (Theme.PartyScore.StaticTeam3BG);
  StaticTeam3Deco := AddStatic (Theme.PartyScore.StaticTeam3Deco);

  TextWinner := AddText (Theme.PartyScore.TextWinner);

  //Load Deco Textures
  if Theme.PartyScore.DecoTextures.ChangeTextures then
  begin
    //Get Color
    LoadColor(R, G, B, Theme.PartyScore.DecoTextures.FirstColor);
    Color := $10000 * Round(R*255) + $100 * Round(G*255) + Round(B*255);
    DecoColor[0].R := R;
    DecoColor[0].G := G;
    DecoColor[0].B := B;

    //Load Texture
    Tex := Texture.LoadTexture(
      Skin.GetTextureFileName(Theme.PartyScore.DecoTextures.FirstTexture),
      Theme.PartyScore.DecoTextures.FirstTyp, Color);
    DecoTex[0] := Tex.TexNum;

    //Get Second Color
    LoadColor(R, G, B, Theme.PartyScore.DecoTextures.SecondColor);
    Color := $10000 * Round(R*255) + $100 * Round(G*255) + Round(B*255);
    DecoColor[1].R := R;
    DecoColor[1].G := G;
    DecoColor[1].B := B;

    //Load Second Texture
    Tex := Texture.LoadTexture(
      Skin.GetTextureFileName(Theme.PartyScore.DecoTextures.SecondTexture),
      Theme.PartyScore.DecoTextures.SecondTyp, Color);
    DecoTex[1] := Tex.TexNum;

    //Get Third Color
    LoadColor(R, G, B, Theme.PartyScore.DecoTextures.ThirdColor);
    Color := $10000 * Round(R*255) + $100 * Round(G*255) + Round(B*255);
    DecoColor[2].R := R;
    DecoColor[2].G := G;
    DecoColor[2].B := B;

    //Load Third Texture
    Tex := Texture.LoadTexture(
      Skin.GetTextureFileName(Theme.PartyScore.DecoTextures.ThirdTexture),
      Theme.PartyScore.DecoTextures.ThirdTyp, Color);
    DecoTex[2] := Tex.TexNum;
  end;

  LoadFromTheme(Theme.PartyScore);
end;

procedure TScreenPartyScore.OnShow;
var
  I, J: integer;
  Placings: array [0..5] of byte;
begin
  inherited;

  //Get Maxscore

  MaxScore := 0;
  for I := 0 to ScreenSingModi.PlayerInfo.NumPlayers - 1 do
  begin
    if (ScreenSingModi.PlayerInfo.Playerinfo[I].Score > MaxScore) then
      MaxScore := ScreenSingModi.PlayerInfo.Playerinfo[I].Score;
  end;

  //Get Placings
  for I := 0 to ScreenSingModi.PlayerInfo.NumPlayers - 1 do
  begin
    Placings[I] := 0;
    for J := 0 to ScreenSingModi.PlayerInfo.NumPlayers - 1 do
      if (ScreenSingModi.PlayerInfo.Playerinfo[J].Score > ScreenSingModi.PlayerInfo.Playerinfo[I].Score) then
        Inc(Placings[I]);
  end;

  //Set Static Length
  Static[StaticTeam1].Texture.ScaleW := ScreenSingModi.PlayerInfo.Playerinfo[0].Percentage / 100;
  Static[StaticTeam2].Texture.ScaleW := ScreenSingModi.PlayerInfo.Playerinfo[1].Percentage / 100;
  Static[StaticTeam3].Texture.ScaleW := ScreenSingModi.PlayerInfo.Playerinfo[2].Percentage / 100;

  //fix: prevents static from drawn out of bounds.
  if Static[StaticTeam1].Texture.ScaleW > 99 then Static[StaticTeam1].Texture.ScaleW := 99;
  if Static[StaticTeam2].Texture.ScaleW > 99 then Static[StaticTeam2].Texture.ScaleW := 99;
  if Static[StaticTeam3].Texture.ScaleW > 99 then Static[StaticTeam3].Texture.ScaleW := 99;

  //End Last Round
  PartySession.EndRound;

  //Set Winnertext
  Text[TextWinner].Text := Format(Language.Translate('PARTY_SCORE_WINS'), [PartySession.GetWinnerString(PartySession.CurRound)]);

  if (ScreenSingModi.PlayerInfo.NumPlayers >= 1) then
  begin
    Text[TextScoreTeam1].Text := InttoStr(ScreenSingModi.PlayerInfo.Playerinfo[0].Score);
    Text[TextNameTeam1].Text := UTF8String(ScreenSingModi.TeamInfo.Teaminfo[0].Name);

    //Set Deco Texture
    if Theme.PartyScore.DecoTextures.ChangeTextures then
    begin
      Static[StaticTeam1Deco].Texture.TexNum := DecoTex[Placings[0]];
      Static[StaticTeam1Deco].Texture.ColR := DecoColor[Placings[0]].R;
      Static[StaticTeam1Deco].Texture.ColG := DecoColor[Placings[0]].G;
      Static[StaticTeam1Deco].Texture.ColB := DecoColor[Placings[0]].B;
    end;

    Text[TextScoreTeam1].Visible := true;
    Text[TextNameTeam1].Visible := true;
    Static[StaticTeam1].Visible := true;
    Static[StaticTeam1BG].Visible := true;
    Static[StaticTeam1Deco].Visible := true;
  end
  else
  begin
    Text[TextScoreTeam1].Visible := false;
    Text[TextNameTeam1].Visible := false;
    Static[StaticTeam1].Visible := false;
    Static[StaticTeam1BG].Visible := false;
    Static[StaticTeam1Deco].Visible := false;
  end;

  if (ScreenSingModi.PlayerInfo.NumPlayers >= 2) then
  begin
    Text[TextScoreTeam2].Text := InttoStr(ScreenSingModi.PlayerInfo.Playerinfo[1].Score);
    Text[TextNameTeam2].Text := UTF8String(ScreenSingModi.TeamInfo.Teaminfo[1].Name);

    //Set Deco Texture
    if Theme.PartyScore.DecoTextures.ChangeTextures then
    begin
      Static[StaticTeam2Deco].Texture.TexNum := DecoTex[Placings[1]];
      Static[StaticTeam2Deco].Texture.ColR := DecoColor[Placings[1]].R;
      Static[StaticTeam2Deco].Texture.ColG := DecoColor[Placings[1]].G;
      Static[StaticTeam2Deco].Texture.ColB := DecoColor[Placings[1]].B;
    end;

    Text[TextScoreTeam2].Visible := true;
    Text[TextNameTeam2].Visible := true;
    Static[StaticTeam2].Visible := true;
    Static[StaticTeam2BG].Visible := true;
    Static[StaticTeam2Deco].Visible := true;
  end
  else
  begin
    Text[TextScoreTeam2].Visible := false;
    Text[TextNameTeam2].Visible := false;
    Static[StaticTeam2].Visible := false;
    Static[StaticTeam2BG].Visible := false;
    Static[StaticTeam2Deco].Visible := false;
  end;

  if (ScreenSingModi.PlayerInfo.NumPlayers >= 3) then
  begin
    Text[TextScoreTeam3].Text := InttoStr(ScreenSingModi.PlayerInfo.Playerinfo[2].Score);
    Text[TextNameTeam3].Text := UTF8String(ScreenSingModi.TeamInfo.Teaminfo[2].Name);

    //Set Deco Texture
    if Theme.PartyScore.DecoTextures.ChangeTextures then
    begin
      Static[StaticTeam3Deco].Texture.TexNum := DecoTex[Placings[2]];
      Static[StaticTeam3Deco].Texture.ColR := DecoColor[Placings[2]].R;
      Static[StaticTeam3Deco].Texture.ColG := DecoColor[Placings[2]].G;
      Static[StaticTeam3Deco].Texture.ColB := DecoColor[Placings[2]].B;
    end;

    Text[TextScoreTeam3].Visible := true;
    Text[TextNameTeam3].Visible := true;
    Static[StaticTeam3].Visible := true;
    Static[StaticTeam3BG].Visible := true;
    Static[StaticTeam3Deco].Visible := true;
  end
  else
  begin
    Text[TextScoreTeam3].Visible := false;
    Text[TextNameTeam3].Visible := false;
    Static[StaticTeam3].Visible := false;
    Static[StaticTeam3BG].Visible := false;
    Static[StaticTeam3Deco].Visible := false;
  end;
end;

procedure TScreenPartyScore.SetAnimationProgress(Progress: real);
begin
  if (ScreenSingModi.PlayerInfo.NumPlayers >= 1) then
    Static[StaticTeam1].Texture.ScaleW := Progress * ScreenSingModi.PlayerInfo.Playerinfo[0].Percentage / 100;
  if (ScreenSingModi.PlayerInfo.NumPlayers >= 2) then
    Static[StaticTeam2].Texture.ScaleW := Progress * ScreenSingModi.PlayerInfo.Playerinfo[1].Percentage / 100;
  if (ScreenSingModi.PlayerInfo.NumPlayers >= 3) then
    Static[StaticTeam3].Texture.ScaleW := Progress * ScreenSingModi.PlayerInfo.Playerinfo[2].Percentage / 100;
end;

end.
