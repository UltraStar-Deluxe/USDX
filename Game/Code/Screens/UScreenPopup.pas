unit UScreenPopup;

interface

uses
  UMenu, SDL, UMusic, UPliki, SysUtils, UThemes;

type
  TScreenPopupCheck = class(TMenu)
    private
      CurMenu: Byte; //Num of the cur. Shown Menu
    public
      Visible: Boolean; //Whether the Menu should be Drawn

      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure ShowPopup(msg: String);
      function Draw: boolean; override;
  end;

type
  TScreenPopupError = class(TMenu)
    private
      CurMenu: Byte; //Num of the cur. Shown Menu
    public
      Visible: Boolean; //Whether the Menu should be Drawn

      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure onHide; override;
      procedure ShowPopup(msg: array of String);
      function Draw: boolean; override;
  end;

var
//  ISelections: Array of String;
  SelectValue: Integer;


implementation

uses UGraphic, UMain, UIni, UTexture, ULanguage, UParty, UPlaylist, UDisplay;

function TScreenPopupCheck.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
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
          Display.CheckOK:=False;
          Display.NextScreenWithCheck:=NIL;
          Visible:=False;
          Result := false;
        end;

      SDLK_RETURN:
        begin
          case Interaction of
          0: begin
               Display.CheckOK:=True;
             end;
          1: begin
               Display.CheckOK:=False;
               Display.NextScreenWithCheck:=NIL;
             end;
          end;
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

constructor TScreenPopupCheck.Create;
var
  I:    integer;
begin
  inherited Create;

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

function TScreenPopupCheck.Draw: boolean;
begin
  inherited Draw;
end;

procedure TScreenPopupCheck.onShow;
begin

end;

procedure TScreenPopupCheck.ShowPopup(msg: String);
begin
  Interaction := 0; //Reset Interaction
  Visible := True;  //Set Visible

  Text[0].Text := Language.Translate(msg);

  Button[0].Visible := True;
  Button[1].Visible := True;

  Button[0].Text[0].Text := Language.Translate('SONG_MENU_YES');
  Button[1].Text[0].Text := Language.Translate('SONG_MENU_NO');
end;

// error popup

function TScreenPopupError.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
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

constructor TScreenPopupError.Create;
var
  I:    integer;
begin
  inherited Create;

  AddBackground(Theme.CheckPopup.Background.Tex);

  AddButton(Theme.ErrorPopup.Button1);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, 'Button 1');

  AddText(Theme.ErrorPopup.TextError);

  for I := 0 to High(Theme.ErrorPopup.Static) do
    AddStatic(Theme.ErrorPopup.Static[I]);

  for I := 0 to High(Theme.ErrorPopup.Text) do
    AddText(Theme.ErrorPopup.Text[I]);

  Interaction := 0;
end;

function TScreenPopupError.Draw: boolean;
begin
  inherited Draw;
end;

procedure TScreenPopupError.onShow;
begin

end;

procedure TScreenPopupError.onHide;
var i: integer;
begin
  for i:=0 to high(Text) do
    Text[i].Text:='';
end;

procedure TScreenPopupError.ShowPopup(msg: array of String);
var i: integer;
begin
  Interaction := 0; //Reset Interaction
  Visible := True;  //Set Visible

  //dirty hack... Text[0] is invisible for some strange reason
  for i:=1 to high(Text) do
    if i-1 <= high(msg) then
    begin
      Text[i].Visible:=True;
      Text[i].Text := msg[i-1];
    end
    else
    begin
      Text[i].Visible:=False;
    end;

  Button[0].Visible := True;

  Button[0].Text[0].Text := 'OK';
end;

end.
