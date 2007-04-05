unit UScreenOptionsAdvanced;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UPliki, UIni, UThemes;

type
  TScreenOptionsAdvanced = class(TMenu)
    public
      constructor Create(Back: String); override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
  end;

implementation

uses UGraphic;

function TScreenOptionsAdvanced.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_ESCAPE :
        begin
          Ini.Save;
          Music.PlayBack;
          FadeTo(@ScreenOptions);
        end;
      SDLK_RETURN:
        begin
          if SelInteraction = 6 then begin
            Ini.Save;
            Music.PlayBack;
            FadeTo(@ScreenOptions);
          end;
        end;
      SDLK_DOWN:
        InteractNext;
      SDLK_UP :
        InteractPrev;
      SDLK_RIGHT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 5) then begin
            Music.PlayOption;
            InteractInc;
          end;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 5) then begin
            Music.PlayOption;
            InteractDec;
          end;
        end;
    end;
  end;
end;

constructor TScreenOptionsAdvanced.Create(Back: String);
var
  I:      integer;
begin
  inherited Create(Back);

  AddBackground(Theme.OptionsAdvanced.Background.Tex);

  for I := 0 to High(Theme.OptionsAdvanced.Static) do
    AddStatic(Theme.OptionsAdvanced.Static[I]);

  for I := 0 to High(Theme.OptionsAdvanced.Text) do
    AddText(Theme.OptionsAdvanced.Text[I]);

  AddSelect(Theme.OptionsAdvanced.SelectLoadAnimation, Ini.LoadAnimation, ILoadAnimation);
  AddSelect(Theme.OptionsAdvanced.SelectEffectPerfect, Ini.EffectPerfect, IEffectPerfect);
  AddSelect(Theme.OptionsAdvanced.SelectEffectGolden, Ini.EffectGolden, IEffectGolden);
  AddSelect(Theme.OptionsAdvanced.SelectLineBonus, Ini.LineBonus, ILineBonus);
  AddSelectSlide(Theme.OptionsAdvanced.SelectOnSongClick, Ini.OnSongClick, IOnSongClick);
  AddSelect(Theme.OptionsAdvanced.SelectAskbeforeDel, Ini.AskbeforeDel, IAskbeforeDel);

  AddButton(Theme.OptionsAdvanced.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[6]);

  Interaction := 0;
end;

procedure TScreenOptionsAdvanced.onShow;
begin
  Interaction := 0;
end;

end.
