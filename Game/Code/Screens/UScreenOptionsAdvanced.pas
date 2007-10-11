unit UScreenOptionsAdvanced;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, UIni, UThemes;

type
  TScreenOptionsAdvanced = class(TMenu)
    public
      constructor Create; override;
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
      SDLK_Q:
        begin
          Result := false;
        end;

      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          Ini.Save;
          AudioPlayback.PlayBack;
          FadeTo(@ScreenOptions);
        end;
      SDLK_RETURN:
        begin
          //SelectLoadAnimation Hidden because it is useless atm
          //if SelInteraction = 7 then begin
          if SelInteraction = 6 then begin
            Ini.Save;
            AudioPlayback.PlayBack;
            FadeTo(@ScreenOptions);
          end;
        end;
      SDLK_DOWN:
        InteractNext;
      SDLK_UP :
        InteractPrev;
      SDLK_RIGHT:
        begin
          //SelectLoadAnimation Hidden because it is useless atm
          //if (SelInteraction >= 0) and (SelInteraction <= 6) then begin
          if (SelInteraction >= 0) and (SelInteraction <= 5) then begin
            AudioPlayback.PlayOption;
            InteractInc;
          end;
        end;
      SDLK_LEFT:
        begin
          //SelectLoadAnimation Hidden because it is useless atm
          //if (SelInteraction >= 0) and (SelInteraction <= 6) then begin
          if (SelInteraction >= 0) and (SelInteraction <= 5) then begin
            AudioPlayback.PlayOption;
            InteractDec;
          end;
        end;
    end;
  end;
end;

constructor TScreenOptionsAdvanced.Create;
var
  I:      integer;
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsAdvanced);

  //SelectLoadAnimation Hidden because it is useless atm
  //AddSelect(Theme.OptionsAdvanced.SelectLoadAnimation, Ini.LoadAnimation, ILoadAnimation);
  AddSelect(Theme.OptionsAdvanced.SelectScreenFade, Ini.ScreenFade, IScreenFade);
  AddSelect(Theme.OptionsAdvanced.SelectEffectSing, Ini.EffectSing, IEffectSing);
  AddSelect(Theme.OptionsAdvanced.SelectLineBonus, Ini.LineBonus, ILineBonus);
  AddSelectSlide(Theme.OptionsAdvanced.SelectOnSongClick, Ini.OnSongClick, IOnSongClick);
  AddSelect(Theme.OptionsAdvanced.SelectAskbeforeDel, Ini.AskbeforeDel, IAskbeforeDel);
  AddSelect(Theme.OptionsAdvanced.SelectPartyPopup, Ini.PartyPopup, IPartyPopup);

  AddButton(Theme.OptionsAdvanced.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);

  Interaction := 0;
end;

procedure TScreenOptionsAdvanced.onShow;
begin
  Interaction := 0;
end;

end.
