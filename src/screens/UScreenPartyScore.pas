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
      SDLK_BACKSPACE,
      SDLK_RETURN :
        begin
          AudioPlayback.PlaySound(SoundLib.Start);

          Party.NextRound; //< go to next round

          if (not Party.GameFinished) then
          begin
            FadeTo(@ScreenPartyNewRound);
          end
          else
          begin
            FadeTo(@ScreenPartyWin);
          end;
        end;
    end;
  end;
end;

constructor TScreenPartyScore.Create;
var
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
  Ranking: AParty_TeamRanking;
begin
  inherited;

  // indicate that round is finished
  Party.RoundPlayed;

  // get rankings for current round
  Ranking := Party.Rounds[Party.CurrentRound].Ranking;


  {//Set Statics Length
  Statics[StaticTeam1].Texture.ScaleW := ScreenSingModi.PlayerInfo.Playerinfo[0].Percentage / 100;
  Statics[StaticTeam2].Texture.ScaleW := ScreenSingModi.PlayerInfo.Playerinfo[1].Percentage / 100;
  Statics[StaticTeam3].Texture.ScaleW := ScreenSingModi.PlayerInfo.Playerinfo[2].Percentage / 100;

  //fix: prevents statics from drawn out of bounds.
  if Statics[StaticTeam1].Texture.ScaleW > 99 then Statics[StaticTeam1].Texture.ScaleW := 99;
  if Statics[StaticTeam2].Texture.ScaleW > 99 then Statics[StaticTeam2].Texture.ScaleW := 99;
  if Statics[StaticTeam3].Texture.ScaleW > 99 then Statics[StaticTeam3].Texture.ScaleW := 99; }

  //Set Winnertext
  Text[TextWinner].Text := Format(Language.Translate('PARTY_SCORE_WINS'), [Party.GetWinnerString(Party.CurrentRound)]);

  if (Length(Party.Teams) >= 1) then
  begin
    Text[TextScoreTeam1].Text := InttoStr(Party.Teams[0].Score);
    Text[TextNameTeam1].Text := Utf8String(Party.Teams[0].Name);

    //Set Deco Texture
    if Theme.PartyScore.DecoTextures.ChangeTextures then
    begin
      if (Length(Ranking) >= 1) and (Ranking[0].Rank >= 1) and (Ranking[0].Rank <= Length(DecoTex)) then
      begin
        Statics[StaticTeam1Deco].Texture.TexNum := DecoTex[Ranking[0].Rank-1];
        Statics[StaticTeam1Deco].Texture.ColR := DecoColor[Ranking[0].Rank-1].R;
        Statics[StaticTeam1Deco].Texture.ColG := DecoColor[Ranking[0].Rank-1].G;
        Statics[StaticTeam1Deco].Texture.ColB := DecoColor[Ranking[0].Rank-1].B;
      end;
    end;

    Text[TextScoreTeam1].Visible := true;
    Text[TextNameTeam1].Visible := true;
    Statics[StaticTeam1].Visible := true;
    Statics[StaticTeam1BG].Visible := true;
    Statics[StaticTeam1Deco].Visible := true;
  end
  else
  begin
    Text[TextScoreTeam1].Visible := false;
    Text[TextNameTeam1].Visible := false;
    Statics[StaticTeam1].Visible := false;
    Statics[StaticTeam1BG].Visible := false;
    Statics[StaticTeam1Deco].Visible := false;
  end;

  if (Length(Party.Teams) >= 2) then
  begin
    Text[TextScoreTeam2].Text := InttoStr(Party.Teams[1].Score);
    Text[TextNameTeam2].Text := UTF8String(Party.Teams[1].Name);

    //Set Deco Texture
    if Theme.PartyScore.DecoTextures.ChangeTextures then
    begin
      if (Length(Ranking) >= 2) and (Ranking[1].Rank >= 1) and (Ranking[1].Rank <= Length(DecoTex)) then
      begin
        Statics[StaticTeam2Deco].Texture.TexNum := DecoTex[Ranking[1].Rank-1];
        Statics[StaticTeam2Deco].Texture.ColR := DecoColor[Ranking[1].Rank-1].R;
        Statics[StaticTeam2Deco].Texture.ColG := DecoColor[Ranking[1].Rank-1].G;
        Statics[StaticTeam2Deco].Texture.ColB := DecoColor[Ranking[1].Rank-1].B;
      end;
    end;

    Text[TextScoreTeam2].Visible := true;
    Text[TextNameTeam2].Visible := true;
    Statics[StaticTeam2].Visible := true;
    Statics[StaticTeam2BG].Visible := true;
    Statics[StaticTeam2Deco].Visible := true;
  end
  else
  begin
    Text[TextScoreTeam2].Visible := false;
    Text[TextNameTeam2].Visible := false;
    Statics[StaticTeam2].Visible := false;
    Statics[StaticTeam2BG].Visible := false;
    Statics[StaticTeam2Deco].Visible := false;
  end;

  if (Length(Party.Teams) >= 3) then
  begin
    Text[TextScoreTeam3].Text := InttoStr(Party.Teams[2].Score);
    Text[TextNameTeam3].Text := UTF8String(Party.Teams[2].Name);

    //Set Deco Texture
    if Theme.PartyScore.DecoTextures.ChangeTextures then
    begin
      if (Length(Ranking) >= 3) and (Ranking[2].Rank >= 1) and (Ranking[2].Rank <= Length(DecoTex)) then
      begin
        Statics[StaticTeam3Deco].Texture.TexNum := DecoTex[Ranking[2].Rank-1];
        Statics[StaticTeam3Deco].Texture.ColR := DecoColor[Ranking[2].Rank-1].R;
        Statics[StaticTeam3Deco].Texture.ColG := DecoColor[Ranking[2].Rank-1].G;
        Statics[StaticTeam3Deco].Texture.ColB := DecoColor[Ranking[2].Rank-1].B;
      end;
    end;

    Text[TextScoreTeam3].Visible := true;
    Text[TextNameTeam3].Visible := true;
    Statics[StaticTeam3].Visible := true;
    Statics[StaticTeam3BG].Visible := true;
    Statics[StaticTeam3Deco].Visible := true;
  end
  else
  begin
    Text[TextScoreTeam3].Visible := false;
    Text[TextNameTeam3].Visible := false;
    Statics[StaticTeam3].Visible := false;
    Statics[StaticTeam3BG].Visible := false;
    Statics[StaticTeam3Deco].Visible := false;
  end;
end;

procedure TScreenPartyScore.SetAnimationProgress(Progress: real);
begin
  {if (ScreenSingModi.PlayerInfo.NumPlayers >= 1) then
    Statics[StaticTeam1].Texture.ScaleW := Progress * ScreenSingModi.PlayerInfo.Playerinfo[0].Percentage / 100;
  if (ScreenSingModi.PlayerInfo.NumPlayers >= 2) then
    Statics[StaticTeam2].Texture.ScaleW := Progress * ScreenSingModi.PlayerInfo.Playerinfo[1].Percentage / 100;
  if (ScreenSingModi.PlayerInfo.NumPlayers >= 3) then
    Statics[StaticTeam3].Texture.ScaleW := Progress * ScreenSingModi.PlayerInfo.Playerinfo[2].Percentage / 100;}
end;

end.
