unit UScreenMain;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UPliki, SysUtils, UThemes, ULCD, ULight;

type
  TScreenMain = class(TMenu)
    public
      TextDescription:        integer;
      TextDescriptionLong:    integer;

      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure InteractNext; override;
      procedure InteractPrev; override;
      procedure UpdateLCD;
      procedure SetAnimationProgress(Progress: real); override;
      //function Draw: boolean; override;
  end;

implementation

uses Windows, UGraphic, UMain, UIni, UTexture, USongs, Textgl, opengl, ULanguage, UParty, UDLLManager, UScreenCredits;


function TScreenMain.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
var
I: Integer;
SDL_ModState:  Word;
begin
  Result := true;

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);

  //Deactivate Credits when Key is pressed
//  if Credits_Visible then
//  begin
//    Credits_Visible := False;
//    exit;
//  end;

  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_Q:
        begin
          Result := false;
        end;

      SDLK_ESCAPE :
        begin
          Result := False;
        end;

      SDLK_C:
        begin
          if (SDL_ModState = KMOD_LALT) then
          begin
            //Credits_Y := 600;
            //Credits_Alpha := 0;
            //Credits_Visible := True;
            Music.PlayStart;
            FadeTo(@ScreenCredits);
          end;
        end;
      SDLK_M:
        begin
          if (Ini.Players >= 1) AND (Length(DLLMan.Plugins)>=1) then
          begin
            Music.PlayStart;
            FadeTo(@ScreenPartyOptions);
          end;
        end;

      SDLK_S:
        begin
          Music.PlayStart;
          FadeTo(@ScreenStatMain);
        end;

      SDLK_RETURN:
        begin
          if (Interaction = 0) and (Length(Songs.Song) >= 1) then begin
            Music.PlayStart;
            if (Ini.Players >= 0) and (Ini.Players <= 3) then PlayersPlay := Ini.Players + 1;
            if (Ini.Players = 4) then PlayersPlay := 6;

            ScreenName.Goto_SingScreen := False;
            FadeTo(@ScreenName);
          end;
          if Interaction = 1 then begin
            Music.PlayStart;
            FadeTo(@ScreenEdit);
          end;
          if Interaction = 2 then begin
            Music.PlayStart;
            FadeTo(@ScreenOptions);
          end;
          if Interaction = 3 then begin
            Result := false;
          end;
        end;
      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:    InteractNext;
      SDLK_UP:      InteractPrev;
      SDLK_RIGHT:   InteractNext;
      SDLK_LEFT:    InteractPrev;
    end;
  end
  else // Key Up
    case PressedKey of
      SDLK_RETURN :
        begin
        end;
    end;
end;

constructor TScreenMain.Create;
var
  I:    integer;
begin
  inherited Create;

//  AddButton(400-200, 320, 400, 60, Skin.GameStart);
//  AddButton(400-200, 390, 400, 60, Skin.Editor);
//  AddButton(400-200, 460, 400, 60, Skin.Options);
//  AddButton(400-200, 530, 400, 60, Skin.Exit);

  AddBackground(Theme.Main.Background.Tex);

  AddButton(Theme.Main.ButtonSolo);
  AddButton(Theme.Main.ButtonEditor);
  AddButton(Theme.Main.ButtonOptions);
  AddButton(Theme.Main.ButtonExit);

  for I := 0 to High(Theme.Main.Static) do
    AddStatic(Theme.Main.Static[I]);

  for I := 0 to High(Theme.Main.Text) do
    AddText(Theme.Main.Text[I]);

  TextDescription := AddText(Theme.Main.TextDescription);
  TextDescriptionLong := AddText(Theme.Main.TextDescriptionLong);

  Interaction := 0;

  //Some Testing for Button Fade
{  Button[0].SelectH := Button[0].H * 3;
  Button[0].Fade := True;
  Button[0].FadeText := True;
  Button[0].DeSelectReflectionspacing := 280;

  Button[1].SelectH := Button[0].H * 3;
  Button[1].Fade := True;

  Button[2].SelectH := Button[0].H * 3;
  Button[2].Fade := True;
  Button[2].FadeText := True;

  Button[3].SelectH := Button[0].H * 3;
  Button[3].Fade := True;
  Button[3].FadeText := True;
}
end;

procedure TScreenMain.onShow;
begin
  LCD.WriteText(1, '  Choose mode:  ');
  UpdateLCD;
end;

procedure TScreenMain.InteractNext;
begin
  inherited InteractNext;
  Text[TextDescription].Text := Theme.Main.Description[Interaction];
  Text[TextDescriptionLong].Text := Theme.Main.DescriptionLong[Interaction];
  UpdateLCD;
  Light.LightOne(1, 200);
end;

procedure TScreenMain.InteractPrev;
begin
  inherited InteractPrev;
  Text[TextDescription].Text := Theme.Main.Description[Interaction];
  Text[TextDescriptionLong].Text := Theme.Main.DescriptionLong[Interaction];
  UpdateLCD;
  Light.LightOne(0, 200);
end;

procedure TScreenMain.UpdateLCD;
begin
  case Interaction of
    0:  LCD.WriteText(2, '      sing      ');
    1:  LCD.WriteText(2, '     editor     ');
    2:  LCD.WriteText(2, '    options     ');
    3:  LCD.WriteText(2, '      exit      ');
  end
end;

procedure TScreenMain.SetAnimationProgress(Progress: real);
begin
  Static[0].Texture.ScaleW := Progress;
  Static[0].Texture.ScaleH := Progress;
end;
end.
