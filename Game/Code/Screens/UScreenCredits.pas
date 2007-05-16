unit UScreenCredits;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, SysUtils, UThemes, ULCD, ULight;

type
  TScreenCredits = class(TMenu)
    public

      Credits_Y: Real;
      Credits_Time: Cardinal;
      Credits_Alpha: Cardinal;

      Fadeout:      boolean;
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      function Draw: boolean; override;
      procedure onShow; override;
      procedure onHide; override;
      procedure DrawCredits;
   end;

 const Credits_Text: Array[0..52] of PChar = (
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
  'Weezl',
  'for the credits screen music',
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

uses Dialogs,Windows, UGraphic, UMain, UIni, UTexture, USongs, Textgl, opengl, ULanguage;

function TScreenCredits.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_ESCAPE :
        begin
          FadeTo(@ScreenMain);
          Music.PlayBack;
        end;
     end;//esac
    end; //fi
end;

constructor TScreenCredits.Create;
var
  I:    integer;
begin
  inherited Create;

  AddBackground(Theme.Loading.Background.Tex);

//  for I := 0 to High(Theme.Loading.Static) do
//    AddStatic(Theme.Loading.Static[I]);

//  for I := 0 to High(Theme.Loading.Text) do
//    AddText(Theme.Loading.Text[I]);

  Fadeout := false;
end;

function TScreenCredits.Draw: boolean;
begin
  DrawCredits;
end;

procedure TScreenCredits.onShow;
begin
  Credits_Y := 600;
  Credits_Alpha := 0;
  //Music.SetLoop(true); Loop looped ned, so ne scheisse
  Music.Open(soundpath + 'wome_-_echoes.mp3'); //danke kleinster liebster weeeetüüüüü!!
  Music.Play;
end;

procedure TScreenCredits.onHide;
begin
  Music.Stop;
end;

procedure TScreenCredits.DrawCredits;
var
  T,I: Cardinal;
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
  //If lowest Position is outside the Screen-> Show MainMenu
  if (Y <= 0) then
  Y := 600;
  //Draw BackGround
  DrawBG;


  //Draw pulsing Credits Text
  //Set Font
  SetFontStyle (2);
  SetFontItalic(False);
  SetFontSize(9);
  SetFontPos (10, 5);
  glColor4f(1, 0, 0, 0.2 + Abs((Credits_Alpha mod 150)/100 - 0.75));
  glPrint ('You may press ESC now');

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

end;

end.
