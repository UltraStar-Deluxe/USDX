unit UScreenPartyWin;

interface

uses
  UMenu, SDL, UDisplay, UMusic, SysUtils, UThemes;

type
  TScreenPartyWin = class(TMenu)
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
      
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

implementation

uses UGraphic, UMain, UParty, UScreenSingModi, ULanguage;

function TScreenPartyWin.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
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
          FadeTo(@ScreenMain);
        end;

      SDLK_RETURN:
        begin
          Music.PlayStart;
          FadeTo(@ScreenMain);
        end;
    end;
  end;
end;

constructor TScreenPartyWin.Create;
var
  I:    integer;
begin
  inherited Create;

  AddBackground(Theme.PartyWin.Background.Tex);

  TextScoreTeam1 := AddText (Theme.PartyWin.TextScoreTeam1);
  TextScoreTeam2 := AddText (Theme.PartyWin.TextScoreTeam2);
  TextScoreTeam3 := AddText (Theme.PartyWin.TextScoreTeam3);
  TextNameTeam1 := AddText (Theme.PartyWin.TextNameTeam1);
  TextNameTeam2 := AddText (Theme.PartyWin.TextNameTeam2);
  TextNameTeam3 := AddText (Theme.PartyWin.TextNameTeam3);

  StaticTeam1 := AddStatic (Theme.PartyWin.StaticTeam1);
  StaticTeam2 := AddStatic (Theme.PartyWin.StaticTeam2);
  StaticTeam3 := AddStatic (Theme.PartyWin.StaticTeam3);

  TextWinner := AddText (Theme.PartyWin.TextWinner);

  for I := 0 to High(Theme.PartyWin.Static) do
    AddStatic(Theme.PartyWin.Static[I]);

  for I := 0 to High(Theme.PartyWin.Text) do
    AddText(Theme.PartyWin.Text[I]);
end;

procedure TScreenPartyWin.onShow;
var
  I: Integer;
begin

  //Set Winnertext
  Text[TextWinner].Text := Format(Language.Translate('PARTY_SCORE_WINS'), [PartySession.GetWinnerString(255)]);

  if (PartySession.Teams.NumTeams >= 1) then
  begin
    Text[TextScoreTeam1].Text := InttoStr(PartySession.Teams.TeamInfo[0].Score);
    Text[TextNameTeam1].Text := String(PartySession.Teams.TeamInfo[0].Name);

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

  if (PartySession.Teams.NumTeams >= 2) then
  begin
    Text[TextScoreTeam2].Text := InttoStr(PartySession.Teams.TeamInfo[1].Score);
    Text[TextNameTeam2].Text := String(PartySession.Teams.TeamInfo[1].Name);

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

  if (PartySession.Teams.NumTeams >= 3) then
  begin
    Text[TextScoreTeam3].Text := InttoStr(PartySession.Teams.TeamInfo[2].Score);
    Text[TextNameTeam3].Text := String(PartySession.Teams.TeamInfo[2].Name);

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

procedure TScreenPartyWin.SetAnimationProgress(Progress: real);
begin
  {if (ScreenSingModi.PlayerInfo.NumPlayers >= 1) then
    Static[StaticTeam1].Texture.ScaleW := Progress * ScreenSingModi.PlayerInfo.Playerinfo[0].Score / maxScore;
  if (ScreenSingModi.PlayerInfo.NumPlayers >= 2) then
    Static[StaticTeam2].Texture.ScaleW := Progress * ScreenSingModi.PlayerInfo.Playerinfo[1].Score / maxScore;
  if (ScreenSingModi.PlayerInfo.NumPlayers >= 3) then
    Static[StaticTeam3].Texture.ScaleW := Progress * ScreenSingModi.PlayerInfo.Playerinfo[2].Score / maxScore;}
end;

end.
