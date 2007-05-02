unit UScreenPopup;

interface

uses
  UMenu, SDL, UMusic, UPliki, SysUtils, UThemes, dialogs, Messages;

type
  TScreenPopup = class(TMenu)
    private
      CurMenu: Byte; //Num of the cur. Shown Menu
    public
      Visible: Boolean; //Whether the Menu should be Drawn

      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure ShowPopup(sPopup: Byte);
      function Draw: boolean; override;
  end;

const
  PU_Error = 1;


var
  ISelections: Array of String;
  SelectValue: Integer;


implementation

uses UGraphic, UMain, UIni, UTexture, ULanguage, UParty, UPlaylist;

function TScreenPopup.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
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
          Result := false;
        end;

      SDLK_RETURN:
        begin
          Visible:=False;
          Result := false;
        end;

      SDLK_DOWN:    SelectNext;
      SDLK_UP:      SelectPrev;

      SDLK_RIGHT: SelectNext;
      SDLK_LEFT: SelectPrev;
    end;
  end
  else // Key Up
    case PressedKey of
      SDLK_RETURN :
        begin
        end;
    end;
end;

constructor TScreenPopup.Create;
var
  I:    integer;
begin
  inherited Create;
  SetLength(ISelections, 1);
  ISelections[0] := 'Dummy';

  AddBackground(Theme.CheckPopup.Background.Tex);

  AddButton(Theme.CheckPopup.Button1);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, 'Button 1');

  AddButton(Theme.CheckPopup.Button2);
  if (Length(Button[1].Text) = 0) then
    AddButtonText(14, 20, 'Button 2');

  AddText(Theme.CheckPopup.TextCheck);

  for I := 0 to High(Theme.CheckPopup.Static) do
    AddStatic(Theme.CheckPopup.Static[I]);

  for I := 0 to High(Theme.CheckPopup.Text) do
    AddText(Theme.CheckPopup.Text[I]);

  Interaction := 0;
end;

function TScreenPopup.Draw: boolean;
begin
  inherited Draw;
end;

procedure TScreenPopup.onShow;
begin

end;

procedure TScreenPopup.ShowPopup(sPopup: Byte);
begin
  Interaction := 0; //Reset Interaction
  Visible := True;  //Set Visible
  Case sPopup of
    PU_Error:
      begin
        Text[0].Text := 'Wirklich beenden?';{Language.Translate('SONG_MENU_NAME_MAIN');}

        Button[0].Visible := True;
        Button[1].Visible := True;
//        Button[2].Visible := True;
//        Button[3].Visible := True;
//        SelectsS[0].Visible := False;

        Button[0].Text[0].Text := 'JA';
        Button[1].Text[0].Text := 'NEIN';
//        Button[2].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_ADD');
//        Button[3].Text[0].Text := Language.Translate('SONG_MENU_EDIT');
      end;
  end;
end;

end.