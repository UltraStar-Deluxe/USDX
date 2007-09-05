unit UScreenOptionsSound;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, UIni, UThemes;

type
  TScreenOptionsSound = class(TMenu)
    public
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
  end;

implementation

uses UGraphic;

function TScreenOptionsSound.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
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

constructor TScreenOptionsSound.Create;
var
  I:      integer;
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsSound);

  AddSelect(Theme.OptionsSound.SelectMicBoost, Ini.MicBoost, IMicBoost);
  AddSelect(Theme.OptionsSound.SelectClickAssist, Ini.ClickAssist, IClickAssist);
  AddSelect(Theme.OptionsSound.SelectBeatClick, Ini.BeatClick, IBeatClick);
  AddSelect(Theme.OptionsSound.SelectThreshold, Ini.Threshold, IThreshold);

  //Song Preview
  AddSelectSlide(Theme.OptionsSound.SelectSlidePreviewVolume, Ini.PreviewVolume, IPreviewVolume);
  AddSelectSlide
  (Theme.OptionsSound.SelectSlidePreviewFading, Ini.PreviewFading, IPreviewFading);

  AddButton(Theme.OptionsSound.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);

  Interaction := 0;
end;

procedure TScreenOptionsSound.onShow;
begin
  Interaction := 0;
end;

end.
