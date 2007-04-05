unit UScreenOptions;

interface

uses
  UMenu, SDL, SysUtils, UDisplay, UMusic, UPliki, UIni, UThemes;

type
  TScreenOptions = class(TMenu)
    public
      TextDescription:    integer;
      constructor Create(Back: String); override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure InteractNext; override;
      procedure InteractPrev; override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

implementation

uses UGraphic;

function TScreenOptions.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_Q:
        begin
          Result := false;
        end;
      SDLK_ESCAPE:
        begin
          Ini.Save;
          Music.PlayBack;
          FadeTo(@ScreenMain);
        end;
      SDLK_RETURN:
        begin
          if SelInteraction = 0 then begin
            Music.PlayStart;
            FadeTo(@ScreenOptionsGame);
          end;

          if SelInteraction = 1 then begin
            Music.PlayStart;
            FadeTo(@ScreenOptionsGraphics);
          end;

          if SelInteraction = 2 then begin
            Music.PlayStart;
            FadeTo(@ScreenOptionsSound);
          end;

          if SelInteraction = 3 then begin
            Music.PlayStart;
            FadeTo(@ScreenOptionsLyrics);
          end;

          if SelInteraction = 4 then begin
            Music.PlayStart;
            FadeTo(@ScreenOptionsThemes);
          end;

          if SelInteraction = 5 then begin
            Music.PlayStart;
            FadeTo(@ScreenOptionsRecord);
          end;

          if SelInteraction = 6 then begin
            Ini.Save;
            Music.PlayBack;
            FadeTo(@ScreenMain);
          end;
        end;
      SDLK_LEFT:
      begin
          {case SelInteraction of
          0: InteractCustom(+2);
          1: InteractCustom(-1);
          2: InteractCustom(-1);
          3: InteractCustom(+2);
          4: InteractCustom(-1);
          5: InteractCustom(-1);
          end;}
          InteractPrev;
      end;
      SDLK_RIGHT:
      begin
          {case SelInteraction of
          0: InteractCustom(+1);
          1: InteractCustom(+1);
          2: InteractCustom(-2);
          3: InteractCustom(+1);
          4: InteractCustom(+1);
          5: InteractCustom(-2);
          end;}
          InteractNext;
      end;
      SDLK_UP:
      begin
          InteractPrev;
          {case SelInteraction of
          0: InteractCustom(+3);
          1: InteractCustom(+3);
          2: InteractCustom(+3);
          3: InteractCustom(-3);
          4: InteractCustom(-3);
          5: InteractCustom(-3);
          end; }
      end;
      SDLK_DOWN:
      begin
          {case SelInteraction of
          0: InteractCustom(+3);
          1: InteractCustom(+3);
          2: InteractCustom(+3);
          3: InteractCustom(-3);
          4: InteractCustom(-3);
          5: InteractCustom(-3);
          end;  }
          InteractNext;
      end;
    end;
  end;
end;

constructor TScreenOptions.Create(Back: String);
var
  I:    integer;
begin
  inherited Create(Back);

  // Game
{  AddButton(225, 100 + 0*60, 350, 50, Skin.Button, 'JPG', 'Transparent Range');
  AddButtonText(11, 10, 'Game');}

  // Graphics
{  AddButton(225, 100 + 1*60, 350, 50, Skin.Button, 'JPG', 'Transparent Range');
  AddButtonText(11, 10, 'Graphics');

  // Sound
  AddButton(225, 100 + 2*60, 350, 50, Skin.Button, 'JPG', 'Transparent Range');
  AddButtonText(11, 10, 'Sound');

  // Lyrics
  AddButton(225, 100 + 3*60, 350, 50, Skin.Button, 'JPG', 'Transparent Range');
  AddButtonText(11, 10, 'Lyrics');

  // Themes
  AddButton(225, 100 + 4*60, 350, 50, Skin.Button, 'JPG', 'Transparent Range');
  AddButtonText(11, 10, 'Themes');

  // Exit
  AddButton(225, 100 + 6*60, 350, 50, Skin.Exit);}

  AddBackground(Theme.Options.Background.Tex);

  AddButton(Theme.Options.ButtonGame);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[0]);

  AddButton(Theme.Options.ButtonGraphics);
  if (Length(Button[1].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[1]);

  AddButton(Theme.Options.ButtonSound);
  if (Length(Button[2].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[2]);

  AddButton(Theme.Options.ButtonLyrics);
  if (Length(Button[3].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[3]);

  AddButton(Theme.Options.ButtonThemes);
  if (Length(Button[4].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[4]);

  AddButton(Theme.Options.ButtonRecord);
  if (Length(Button[5].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[5]);

  AddButton(Theme.Options.ButtonExit);
  if (Length(Button[6].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[6]);

  for I := 0 to High(Theme.Options.Static) do
    AddStatic(Theme.Options.Static[I]);

  for I := 0 to High(Theme.Options.Text) do
    AddText(Theme.Options.Text[I]);

  TextDescription := AddText(Theme.Options.TextDescription);

  Interaction := 0;
end;

procedure TScreenOptions.onShow;
begin
//
end;

procedure TScreenOptions.InteractNext;
begin
  inherited InteractNext;
  Text[TextDescription].Text := Theme.Options.Description[Interaction];
end;

procedure TScreenOptions.InteractPrev;
begin
  inherited InteractPrev;
  Text[TextDescription].Text := Theme.Options.Description[Interaction];
end;


procedure TScreenOptions.SetAnimationProgress(Progress: real);
begin
  Button[0].Texture.ScaleW := Progress;
  Button[1].Texture.ScaleW := Progress;
  Button[2].Texture.ScaleW := Progress;
  Button[3].Texture.ScaleW := Progress;
  Button[4].Texture.ScaleW := Progress;
  Button[5].Texture.ScaleW := Progress;
end;

end.
