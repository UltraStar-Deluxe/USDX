unit UScreenMain;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UPliki, SysUtils, UThemes, ULCD, ULight;

type
  TScreenMain = class(TMenu)
    public
      TextDescription:        integer;
      TextDescriptionLong:    integer;

      //Credits Mod
      Credits_Visible: Boolean;
      Credits_Y: Real;
      Credits_Time: Cardinal;
      Credits_Alpha: Cardinal;
      procedure DrawCredits;
      //Credits Mod End

      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure InteractNext; override;
      procedure InteractPrev; override;
      procedure UpdateLCD;
      procedure SetAnimationProgress(Progress: real); override;
      function Draw: boolean; override;
  end;

const Credits_Text: Array[0..49] of PChar = (
  ':SPACE:',
  'Main Idea: Corvus 5',
  'Thank you very much for this great Game',
  ':SPACE:',
  'The Ultrastar Deluxe Team:',
  ':SPACE:',
  'blindy:',
  'VFX Programming',
  ':SPACE:',
  'commandi00:',
  'Beta Testing',
  ':SPACE:',
  'Crazy Joker:',
  'Graphics',
  ':SPACE:',
  'DennistheMenace:',
  'Beta Testing and great Support in "the Board"',
  ':SPACE:',
  'Mog:',
  'Programming, Graphics',
  ':SPACE:',
  'Mota:',
  'Programming, Idea of creating this Mod',
  ':SPACE:',
  'Sawyer:',
  'Web Master, Programming',
  ':SPACE:',
  'Whiteshark:',
  'Programming, Creating Release',
  ':SPACE:',
  ':SPACE:',
  'Thanks to',
  ':SPACE:',
  'Blind Guard',
  'for supporting us and administrate this great Board',
  ':SPACE:',
  'The whole Community from www.ultra-star.dl.am',
  'for supporting us, supporting the newbies',
  'and remembering us to continue work',
  ':SPACE:',
  'You',
  'for using Ultrastar Deluxe',
  ':SPACE:',
  ':SPACE:',
  'Visit us at:',
  'http://www.ultrastardx.dl.am',
  'http://sourceforge.net/projects/ultrastardx/',
  'http://www.Ultra-Star.dl.am',
  'Please write Bug Reports and Feature Requests',
  'to help making this a better Game');


implementation

uses Windows, UGraphic, UMain, UIni, UTexture, USongs, Textgl, opengl, ULanguage, UParty, UDLLManager;


function TScreenMain.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
var
I: Integer;
SDL_ModState:  Word;
begin
  Result := true;

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);

  //Deactivate Credits when Key is pressed
  if Credits_Visible then
  begin
    Credits_Visible := False;
    exit;
  end;

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
            Credits_Y := 600;
            Credits_Alpha := 0;
            Credits_Visible := True;
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
  Button[0].SelectH := Button[0].H * 3;
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

function TScreenMain.Draw: boolean;
begin
Result := True;
if Credits_Visible then
  DrawCredits
else
  Result := inherited Draw;
end;

procedure TScreenMain.DrawCredits;
var
  T, I: Cardinal;
  Y: Real;
  Ver: PChar;
begin
  T := GetTickCount div 33;
  if T <> Credits_Time then
  begin
    Credits_Time := T;
    //Change Position
    Credits_Y := Credits_Y - 1;
    //Change Alpha
    Inc (Credits_Alpha, 3);
  end;

  //Draw BackGround
  DrawBG;


  //Draw pulsing Credits Text
  //Set Font
  SetFontStyle (2);
  SetFontItalic(False);
  SetFontSize(9);
  SetFontPos (10, 5);
  glColor4f(1, 0, 0, 0.2 + Abs((Credits_Alpha mod 150)/100 - 0.75));
  glPrint ('Credits! Press any Key to Continue');

  //Set Font Size for Credits
  SetFontSize(12);
  //Draw Version
  if (Credits_Y>-35) then
  begin
    Ver := PChar(Language.Translate('US_VERSION'));
    //Set Color
    if Credits_Y > 500 then
      glColor4f(1, 0.6, 0.08, (600 - Credits_Y)/100 - 0.2)
    else
      glColor4f(1, 0.6, 0.08, 0.8);
      
    SetFontPos (400 - glTextWidth(Ver)/2, Credits_Y);
    glprint(Ver);
  end;

  //Set Color + Start Pos
  glColor4f(0.8, 0.8, 1, 0.8);
  Y := Credits_Y + 50;

  //Search upper Position
  For I := 0 to high(Credits_Text) do
  begin
    if (Credits_Text[I]=':SPACE:') then //Spacer
      Y := Y + 55
    else
      Y := Y + 30;

    if Y > -35 then
      break;
  end;

  //Draw Text
  For T := I+1 to high(Credits_Text) do
  begin
    if (Credits_Text[T]=':SPACE:') then //Spacer
      Y := Y + 55
    else
    begin
      //Set Color
      if Y > 500 then
        glColor4f(0.8, 0.8, 1, (600 - Y)/100 - 0.2)
      else
        glColor4f(0.8, 0.8, 1, 0.8);

      SetFontPos (400 - glTextWidth(Credits_Text[T])/2, Y);
      glprint(Credits_Text[T]);
      Y := Y + 30;
    end;

    if Y > 600 then
      break;
  end;

  //If lowest Position is outside the Screen-> Show MainMenu
  if (Y <= 0) then
    Credits_Visible := False;
end;

end.
