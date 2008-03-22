unit UScreenPopup;

interface

{$I switches.inc}

uses
  UMenu, SDL, UMusic, UFiles, SysUtils, UThemes;

type
  TScreenPopupCheck = class(TMenu)
    public
      Visible: Boolean; //Whether the Menu should be Drawn

      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure ShowPopup(msg: String);
      function Draw: boolean; override;
  end;

type
  TScreenPopupError = class(TMenu)
{    private
      CurMenu: Byte; //Num of the cur. Shown Menu}
    public
      Visible: Boolean; //Whether the Menu should be Drawn

      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure onHide; override;
      procedure ShowPopup(msg: String);
      function Draw: boolean; override;
  end;

var
//  ISelections: Array of String;
  SelectValue: Integer;


implementation

uses UGraphic, UMain, UIni, UTexture, ULanguage, UParty, UPlaylist, UDisplay;

function TScreenPopupCheck.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    // check normal keys
    case WideUpperCase(CharCode)[1] of
      'Q':
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
          Display.CheckOK:=False;
          Display.NextScreenWithCheck:=NIL;
          Visible:=False;
          Result := false;
        end;

      SDLK_RETURN:
        begin
          case Interaction of
          0: begin
               //Hack to Finish Singscreen correct on Exit with Q Shortcut
               if (Display.NextScreenWithCheck = NIL) then
               begin
                 if (Display.CurrentScreen = @ScreenSing) then
                   ScreenSing.Finish
                 else if (Display.CurrentScreen = @ScreenSingModi) then
                   ScreenSingModi.Finish;
               end;

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

      SDLK_DOWN:    InteractNext;
      SDLK_UP:      InteractPrev;

      SDLK_RIGHT: InteractNext;
      SDLK_LEFT: InteractPrev;
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
  Draw:=inherited Draw;
end;

procedure TScreenPopupCheck.onShow;
begin
  inherited;
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

function TScreenPopupError.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down

    case PressedKey of
      SDLK_Q:
        begin
          Result := false;
        end;

      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          Visible:=False;
          Result := false;
        end;

      SDLK_RETURN:
        begin
          Visible:=False;
          Result := false;
        end;

      SDLK_DOWN:    InteractNext;
      SDLK_UP:      InteractPrev;

      SDLK_RIGHT: InteractNext;
      SDLK_LEFT: InteractPrev;
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
  Draw:=inherited Draw;
end;

procedure TScreenPopupError.onShow;
begin
  inherited;

end;

procedure TScreenPopupError.onHide;
begin
end;

procedure TScreenPopupError.ShowPopup(msg: String);
begin
  Interaction := 0; //Reset Interaction
  Visible := True;  //Set Visible

{  //dirty hack... Text[0] is invisible for some strange reason
  for i:=1 to high(Text) do
    if i-1 <= high(msg) then
    begin
      Text[i].Visible:=True;
      Text[i].Text := msg[i-1];
    end
    else
    begin
      Text[i].Visible:=False;
    end;}
  Text[0].Text:=msg;

  Button[0].Visible := True;

  Button[0].Text[0].Text := 'OK';
end;

end.
