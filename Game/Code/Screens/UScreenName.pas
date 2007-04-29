unit UScreenName;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UPliki, SysUtils, UThemes;

type
  TScreenName = class(TMenu)
    public
      Goto_SingScreen: Boolean; //If True then next Screen in SingScreen
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

implementation

uses UGraphic, UMain, UIni, UTexture;

function TScreenName.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
var
  I:    integer;
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
          FadeTo(@ScreenMain);
        end;

      SDLK_RETURN:
        begin
          for I := 1 to 6 do
            Ini.Name[I-1] := Button[I-1].Text[0].Text;
          Ini.SaveNames;
          Music.PlayStart;

          if GoTo_SingScreen then
            FadeTo(@ScreenSing)
          else
            FadeTo(@ScreenLevel);

          GoTo_SingScreen := False;
        end;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:    InteractNext;
      SDLK_UP:      InteractPrev;
      SDLK_RIGHT:   InteractNext;
      SDLK_LEFT:    InteractPrev;
    end;
  end;
end;

constructor TScreenName.Create;
var
  I:    integer;
begin
  inherited Create;

  LoadFromTheme(Theme.Name);


  for I := 1 to 6 do
    AddButton(Theme.Name.ButtonPlayer[I]);

  Interaction := 0;
end;

procedure TScreenName.onShow;
var
  I:    integer;
begin
  for I := 1 to 6 do
    Button[I-1].Text[0].Text := Ini.Name[I-1];

  for I := 1 to PlayersPlay do begin
    Button[I-1].Visible := true;
    Button[I-1].Selectable := true;
  end;

  for I := PlayersPlay+1 to 6 do begin
    Button[I-1].Visible := false;
    Button[I-1].Selectable := false;
  end;

end;

procedure TScreenName.SetAnimationProgress(Progress: real);
var
  I:    integer;
begin
  for I := 1 to 6 do
    Button[I-1].Texture.ScaleW := Progress;
end;

end.
