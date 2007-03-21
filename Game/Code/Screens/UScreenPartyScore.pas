unit UScreenPartyScore;

interface

uses
  UMenu, SDL, UDisplay, UMusic, SysUtils, UThemes;

type
  TScreenPartyScore = class(TMenu)
    public
      TextScoreTeam1:    Cardinal;
      TextScoreTeam2:    Cardinal;
      TextScoreTeam3:    Cardinal;
      TextNameTeam1:     Cardinal;
      TextNameTeam2:     Cardinal;
      TextNameTeam3:     Cardinal;
      StaticTeam1:       Cardinal;
      StaticTeam2:       Cardinal;
      StaticTeam3:       Cardinal;
      TextWinner:        Cardinal;

      MaxScore:          Word;
      
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

implementation

uses UGraphic, UMain, UParty, UScreenSingModi, ULanguage;

function TScreenPartyScore.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_Q:
        begin
          Result := false;
        end;

      SDLK_ESCAPE :
        begin
          Music.PlayStart;
          if (PartySession.CurRound <= High(PartySession.Rounds)) then
            FadeTo(@ScreenPartyNewRound)
          else
          begin
            PartySession.EndRound;
            FadeTo(@ScreenPartyWin);
          end;
        end;

      SDLK_RETURN:
        begin
          Music.PlayStart;
          if (PartySession.CurRound <= High(PartySession.Rounds)) then
            FadeTo(@ScreenPartyNewRound)
          else
          begin
            PartySession.EndRound;
            FadeTo(@ScreenPartyWin);
          end;
        end;
    end;
  end;
end;

constructor TScreenPartyScore.Create;
var
  I:    integer;
begin
  inherited Create;

  AddBackground(Theme.PartyScore.Background.Tex);

  TextScoreTeam1 := AddText (Theme.PartyScore.TextScoreTeam1);
  TextScoreTeam2 := AddText (Theme.PartyScore.TextScoreTeam2);
  TextScoreTeam3 := AddText (Theme.PartyScore.TextScoreTeam3);
  TextNameTeam1 := AddText (Theme.PartyScore.TextNameTeam1);
  TextNameTeam2 := AddText (Theme.PartyScore.TextNameTeam2);
  TextNameTeam3 := AddText (Theme.PartyScore.TextNameTeam3);

  StaticTeam1 := AddStatic (Theme.PartyScore.StaticTeam1);
  StaticTeam2 := AddStatic (Theme.PartyScore.StaticTeam2);
  StaticTeam3 := AddStatic (Theme.PartyScore.StaticTeam3);

  TextWinner := AddText (Theme.PartyScore.TextWinner);

  for I := 0 to High(Theme.PartyScore.Static) do
    AddStatic(Theme.PartyScore.Static[I]);

  for I := 0 to High(Theme.PartyScore.Text) do
    AddText(Theme.PartyScore.Text[I]);
end;

procedure TScreenPartyScore.onShow;
var
  I: Integer;
begin
  //Get Maxscore
  MaxScore := 0;
  for I := 0 to ScreenSingModi.PlayerInfo.NumPlayers - 1 do
  begin
    if (ScreenSingModi.PlayerInfo.Playerinfo[I].Score > MaxScore) then
      MaxScore := ScreenSingModi.PlayerInfo.Playerinfo[I].Score;
  end;

  //Set Static Length
  Static[StaticTeam1].Texture.ScaleW := ScreenSingModi.PlayerInfo.Playerinfo[0].Percentage / 100;
  Static[StaticTeam2].Texture.ScaleW := ScreenSingModi.PlayerInfo.Playerinfo[1].Percentage / 100;
  Static[StaticTeam3].Texture.ScaleW := ScreenSingModi.PlayerInfo.Playerinfo[2].Percentage / 100;

  //Set Winnertext
  Text[TextWinner].Text := Format(Language.Translate('PARTY_SCORE_WINS'), [PartySession.GetWinnerString(PartySession.CurRound)]);

  if (ScreenSingModi.PlayerInfo.NumPlayers >= 1) then
  begin
    Text[TextScoreTeam1].Text := InttoStr(ScreenSingModi.PlayerInfo.Playerinfo[0].Score);
    Text[TextNameTeam1].Text := String(ScreenSingModi.TeamInfo.Teaminfo[0].Name);

    Text[TextScoreTeam1].Visible := True;
    Text[TextNameTeam1].Visible := True;
    Static[StaticTeam1].Visible := True;
  end
  else
  begin
    Text[TextScoreTeam1].Visible := False;
    Text[TextNameTeam1].Visible := False;
    Static[StaticTeam1].Visible := False;
  end;

  if (ScreenSingModi.PlayerInfo.NumPlayers >= 2) then
  begin
    Text[TextScoreTeam2].Text := InttoStr(ScreenSingModi.PlayerInfo.Playerinfo[1].Score);
    Text[TextNameTeam2].Text := String(ScreenSingModi.TeamInfo.Teaminfo[1].Name);

    Text[TextScoreTeam2].Visible := True;
    Text[TextNameTeam2].Visible := True;
    Static[StaticTeam2].Visible := True;
  end
  else
  begin
    Text[TextScoreTeam2].Visible := False;
    Text[TextNameTeam2].Visible := False;
    Static[StaticTeam2].Visible := False;
  end;

  if (ScreenSingModi.PlayerInfo.NumPlayers >= 3) then
  begin
    Text[TextScoreTeam3].Text := InttoStr(ScreenSingModi.PlayerInfo.Playerinfo[2].Score);
    Text[TextNameTeam3].Text := String(ScreenSingModi.TeamInfo.Teaminfo[2].Name);

    Text[TextScoreTeam3].Visible := True;
    Text[TextNameTeam3].Visible := True;
    Static[StaticTeam3].Visible := True;
  end
  else
  begin
    Text[TextScoreTeam3].Visible := False;
    Text[TextNameTeam3].Visible := False;
    Static[StaticTeam3].Visible := False;
  end;


//  LCD.WriteText(1, '  Choose mode:  ');
//  UpdateLCD;
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
