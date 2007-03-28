unit UScreenSongMenu;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UPliki, SysUtils, UThemes;

type
  TScreenSongMenu = class(TMenu)
    private
      CurMenu: Byte; //Num of the cur. Shown Menu
    public
      Visible: Boolean; //Whether the Menu should be Drawn

      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      function Draw: boolean; override;
      procedure MenuShow(sMenu: Byte);
      procedure HandleReturn;
  end;

const
  SM_Main = 1;
  SM_PlayList = 64 or 1;
  SM_Party_Main = 128 or 1;
  SM_Party_Joker = 128 or 2;

var
  ISelections: Array of String;
  SelectValue: Integer;


implementation

uses UGraphic, UMain, UIni, UTexture, ULanguage, UParty;

function TScreenSongMenu.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
  function IsVisible: Boolean;
  begin
    Result := True;
    if (Interactions[Interaction].Typ = 0) then
    begin
      Result := Button[Interactions[Interaction].Num].Visible;
    end
    else if (Interactions[Interaction].Typ = 1) then
    begin
      //Result := Selects[Interactions[Interaction].Num].Visible;
    end
    else if (Interactions[Interaction].Typ = 3) then
    begin
      Result := SelectsS[Interactions[Interaction].Num].Visible;
    end;
  end;

  Procedure SelectNext;
  begin
    repeat
      InteractNext;
    until IsVisible;
  end;

  Procedure SelectPrev;
  begin
    repeat
      InteractPrev;
    until IsVisible;
  end;
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
          Music.PlayBack;
          Visible := False;
        end;

      SDLK_RETURN:
        begin
          HandleReturn;
        end;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:    SelectNext;
      SDLK_UP:      SelectPrev;

      SDLK_RIGHT:
        begin
          if (Interaction=3) then
            InteractInc;
        end;
      SDLK_LEFT:
        begin
          if (Interaction=3) then
            InteractDec;
        end;
    end;
  end
  else // Key Up
    case PressedKey of
      SDLK_RETURN :
        begin
        end;
    end;
end;

constructor TScreenSongMenu.Create;
var
  I:    integer;
begin
  inherited Create;
  SetLength(ISelections, 1);
  ISelections[0] := 'Dummy';

  AddBackground(Theme.SongMenu.Background.Tex);

  AddButton(Theme.SongMenu.Button1);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, 'Button 1');

  AddButton(Theme.SongMenu.Button2);
  if (Length(Button[1].Text) = 0) then
    AddButtonText(14, 20, 'Button 2');

  AddButton(Theme.SongMenu.Button3);
  if (Length(Button[2].Text) = 0) then
    AddButtonText(14, 20, 'Button 3');

  AddSelectSlide(Theme.SongMenu.SelectSlide3, SelectValue, ISelections);

  AddButton(Theme.SongMenu.Button4);
  if (Length(Button[3].Text) = 0) then
    AddButtonText(14, 20, 'Button 4');

  AddText(Theme.SongMenu.TextMenu);

  for I := 0 to High(Theme.SongMenu.Static) do
    AddStatic(Theme.SongMenu.Static[I]);

  for I := 0 to High(Theme.SongMenu.Text) do
    AddText(Theme.SongMenu.Text[I]);

  Interaction := 0;
end;

function TScreenSongMenu.Draw: boolean;
begin
  inherited Draw;
end;

procedure TScreenSongMenu.onShow;
begin

end;

procedure TScreenSongMenu.MenuShow(sMenu: Byte);
begin
  Interaction := 0; //Reset Interaction
  Visible := True;  //Set Visible
  Case sMenu of
    SM_Main:
      begin
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_MAIN');

        Button[0].Visible := True;
        Button[1].Visible := True;
        Button[2].Visible := True;
        Button[3].Visible := True;
        SelectsS[0].Visible := False;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_PLAY');
        Button[1].Text[0].Text := Language.Translate('SONG_MENU_EDIT');
        Button[2].Text[0].Text := Language.Translate('SONG_MENU_PLAYMODI');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_CANCEL');
      end;

    SM_PlayList:
      begin
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PLAYLIST');

        Button[0].Visible := True;
        Button[1].Visible := False;
        Button[2].Visible := False;
        Button[3].Visible := True;
        SelectsS[0].Visible := False;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_ADD');
        Button[1].Text[0].Text := '';
        Button[2].Text[0].Text := '';
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_CANCEL');
      end;

    SM_Party_Main:
      begin
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PARTY_MAIN');

        Button[0].Visible := True;
        Button[1].Visible := False;
        Button[2].Visible := False;
        Button[3].Visible := True;
        SelectsS[0].Visible := False;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_PLAY');
        Button[1].Text[0].Text := Language.Translate('SONG_MENU_JOKER');
        Button[2].Text[0].Text := Language.Translate('SONG_MENU_PLAYMODI');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_JOKER');
      end;

    SM_Party_Joker:
      begin
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PARTY_JOKER');

        Button[0].Visible := (PartySession.Teams.NumTeams >= 1) AND (PartySession.Teams.Teaminfo[0].Joker > 0);
        Button[1].Visible := (PartySession.Teams.NumTeams >= 2) AND (PartySession.Teams.Teaminfo[1].Joker > 0);
        Button[2].Visible := (PartySession.Teams.NumTeams >= 3) AND (PartySession.Teams.Teaminfo[2].Joker > 0);
        Button[3].Visible := True;
        SelectsS[0].Visible := False;

        Button[0].Text[0].Text := String(PartySession.Teams.Teaminfo[0].Name);
        Button[1].Text[0].Text := String(PartySession.Teams.Teaminfo[1].Name);
        Button[2].Text[0].Text := String(PartySession.Teams.Teaminfo[2].Name);
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_CANCEL');
      end;
  end;
end;

procedure TScreenSongMenu.HandleReturn;
begin
  Case CurMenu of
    SM_Main:
      begin
        Visible := False;
        Case Interaction of
          0: //Button 1
            begin
              ScreenSong.StartSong;
            end;

          1: //Button 2
            begin
              ScreenSong.OpenEditor;
            end;

          2: //Button 3
            begin
              //Todo: Add SingleRound Modi Support
            end;

          3: //SelectSlide 3
            begin
              //Dummy
            end;

          4: //Button 4
            begin
              //Cancel... (Do Nothing)
            end;
        end;
      end;

    SM_PlayList:
      begin
        Visible := False;
        Case Interaction of
          0: //Button 1
            begin
              //
            end;

          1: //Button 2
            begin
              //
            end;

          2: //Button 3
            begin
              //Todo
            end;

          3: //SelectSlide 3
            begin
              //Dummy
            end;

          4: //Button 4
            begin
              //
            end;
        end;
      end;

    SM_Party_Main:
      begin
        Case Interaction of
          0: //Button 1
            begin
              //Start Singing
              ScreenSong.StartSong;
              Visible := False;
            end;

          4: //Button 4
            begin
              //Joker
              MenuShow(SM_Party_Joker);
            end;
        end;
      end;

    SM_Party_Joker:
      begin
        Visible := False;
        Case Interaction of
          0: //Button 1
            begin
              //Joker Team 1
              ScreenSong.DoJoker(0);
            end;

          1: //Button 2
            begin
              //Joker Team 2
              ScreenSong.DoJoker(1);
            end;

          2: //Button 3
            begin
              //Joker Team 3
              ScreenSong.DoJoker(2);
            end;

          4: //Button 4
            begin
              //Cancel... (Fo back to old Menu)
              MenuShow(SM_Party_Main);
            end;
        end;
      end;
  end;
end;

end.
 