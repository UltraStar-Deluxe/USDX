unit UScreenPartyPlayer;

Interface

uses
  UMenu, SDL, UDisplay, UMusic, UPliki, SysUtils, UThemes;

type
  TScreenPartyPlayer = class(TMenu)
    public
      Team1Name: Cardinal;
      Player1Name: Cardinal;
      Player2Name: Cardinal;
      Player3Name: Cardinal;
      Player4Name: Cardinal;

      Team2Name: Cardinal;
      Player5Name: Cardinal;
      Player6Name: Cardinal;
      Player7Name: Cardinal;
      Player8Name: Cardinal;

      Team3Name: Cardinal;
      Player9Name: Cardinal;
      Player10Name: Cardinal;
      Player11Name: Cardinal;
      Player12Name: Cardinal;

      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

implementation

uses UGraphic, UMain, UIni, UTexture, UParty;

function TScreenPartyPlayer.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
var
  I, J:    integer;
  procedure IntNext;
  begin
    repeat
      InteractNext;
    until Button[Interaction].Visible;
  end;
  procedure IntPrev;
  begin
    repeat
      InteractPrev;
    until Button[Interaction].Visible;
  end;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_0..SDLK_9, SDLK_A..SDLK_Z, SDLK_SPACE, SDLK_MINUS, SDLK_EXCLAIM, SDLK_COMMA, SDLK_SLASH, SDLK_ASTERISK, SDLK_QUESTION, SDLK_QUOTE, SDLK_QUOTEDBL:
        begin
          Button[Interaction].Text[0].Text := Button[Interaction].Text[0].Text + chr(ScanCode);
        end;

      SDLK_BACKSPACE:
        begin
          Button[Interaction].Text[0].DeleteLastL;
        end;

      SDLK_ESCAPE :
        begin
          Ini.SaveNames;
          Music.PlayBack;
          FadeTo(@ScreenPartyOptions);
        end;

      SDLK_RETURN:
        begin

          for I := 0 to PartySession.Teams.NumTeams-1 do
          begin
            PartySession.Teams.Teaminfo[I].Name := PChar(Button[I*5].Text[0].Text);
            for J := 0 to PartySession.Teams.Teaminfo[I].NumPlayers-1 do
            begin
              PartySession.Teams.Teaminfo[I].Playerinfo[J].Name := PChar(Button[I*5 + J+1].Text[0].Text);
              PartySession.Teams.Teaminfo[I].Playerinfo[J].TimesPlayed := 0;
            end;
            PartySession.Teams.Teaminfo[I].Joker := Round (Length(PartySession.Rounds) * 0.85);
          end;

          Music.PlayStart;
          FadeTo(@ScreenPartyNewRound);
        end;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:    IntNext;
      SDLK_UP:      IntPrev;
      SDLK_RIGHT:   IntNext;
      SDLK_LEFT:    IntPrev;
    end;
  end;
end;

constructor TScreenPartyPlayer.Create;
var
  I:    integer;
begin
  inherited Create;

  AddBackground(Theme.PartyPlayer.Background.Tex);

  Team1Name := AddButton(Theme.PartyPlayer.Team1Name);
  AddButton(Theme.PartyPlayer.Player1Name);
  AddButton(Theme.PartyPlayer.Player2Name);
  AddButton(Theme.PartyPlayer.Player3Name);
  AddButton(Theme.PartyPlayer.Player4Name);

  Team2Name := AddButton(Theme.PartyPlayer.Team2Name);
  AddButton(Theme.PartyPlayer.Player5Name);
  AddButton(Theme.PartyPlayer.Player6Name);
  AddButton(Theme.PartyPlayer.Player7Name);
  AddButton(Theme.PartyPlayer.Player8Name);

  Team3Name := AddButton(Theme.PartyPlayer.Team3Name);
  AddButton(Theme.PartyPlayer.Player9Name);
  AddButton(Theme.PartyPlayer.Player10Name);
  AddButton(Theme.PartyPlayer.Player11Name);
  AddButton(Theme.PartyPlayer.Player12Name);

  for I := 0 to High(Theme.PartyPlayer.Static) do
    AddStatic(Theme.PartyPlayer.Static[I]);

  for I := 0 to High(Theme.PartyPlayer.Text) do
    AddText(Theme.PartyPlayer.Text[I]);

  Interaction := 0;
end;

procedure TScreenPartyPlayer.onShow;
var
  I:    integer;
begin
  If (PartySession.Teams.NumTeams>=1) then
  begin
    Button[0].Visible := True;
    Button[1].Visible := (PartySession.Teams.Teaminfo[0].NumPlayers >=1);
    Button[2].Visible := (PartySession.Teams.Teaminfo[0].NumPlayers >=2);
    Button[3].Visible := (PartySession.Teams.Teaminfo[0].NumPlayers >=3);
    Button[4].Visible := (PartySession.Teams.Teaminfo[0].NumPlayers >=4);
  end
  else
  begin
    Button[0].Visible := False;
    Button[1].Visible := False;
    Button[2].Visible := False;
    Button[3].Visible := False;
    Button[4].Visible := False;
  end;

  If (PartySession.Teams.NumTeams>=2) then
  begin
    Button[5].Visible := True;
    Button[6].Visible := (PartySession.Teams.Teaminfo[1].NumPlayers >=1);
    Button[7].Visible := (PartySession.Teams.Teaminfo[1].NumPlayers >=2);
    Button[8].Visible := (PartySession.Teams.Teaminfo[1].NumPlayers >=3);
    Button[9].Visible := (PartySession.Teams.Teaminfo[1].NumPlayers >=4);
  end
  else
  begin
    Button[5].Visible := False;
    Button[6].Visible := False;
    Button[7].Visible := False;
    Button[8].Visible := False;
    Button[9].Visible := False;
  end;

  If (PartySession.Teams.NumTeams>=3) then
  begin
    Button[10].Visible := True;
    Button[11].Visible := (PartySession.Teams.Teaminfo[2].NumPlayers >=1);
    Button[12].Visible := (PartySession.Teams.Teaminfo[2].NumPlayers >=2);
    Button[13].Visible := (PartySession.Teams.Teaminfo[2].NumPlayers >=3);
    Button[14].Visible := (PartySession.Teams.Teaminfo[2].NumPlayers >=4);
  end
  else
  begin
    Button[10].Visible := False;
    Button[11].Visible := False;
    Button[12].Visible := False;
    Button[13].Visible := False;
    Button[14].Visible := False;
  end;

end;

procedure TScreenPartyPlayer.SetAnimationProgress(Progress: real);
var
  I:    integer;
begin
  for I := 0 to high(Button) do
    Button[I].Texture.ScaleW := Progress;
end;

end.
