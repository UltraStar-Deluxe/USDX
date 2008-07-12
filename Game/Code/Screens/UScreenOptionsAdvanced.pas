unit UScreenOptionsAdvanced;

interface

{$I switches.inc}

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, UIni, UThemes;

type
  TScreenOptionsAdvanced = class(TMenu)
    public
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
  end;

implementation

uses UGraphic, SysUtils;

function TScreenOptionsAdvanced.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    // check normal keys
    case WideCharUpperCase(CharCode)[1] of
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
          // Escape -> save nothing - just leave this screen
          
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenOptions);
        end;
      SDLK_RETURN:
        begin
          //SelectLoadAnimation Hidden because it is useless atm
          //if SelInteraction = 7 then begin
          if SelInteraction = 6 then begin
            Ini.Save;
            AudioPlayback.PlaySound(SoundLib.Back);
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
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractInc;
          end;
        end;
      SDLK_LEFT:
        begin
          //SelectLoadAnimation Hidden because it is useless atm
          //if (SelInteraction >= 0) and (SelInteraction <= 6) then begin
          if (SelInteraction >= 0) and (SelInteraction <= 5) then begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
          end;
        end;
    end;
  end;
end;

constructor TScreenOptionsAdvanced.Create;
//var
// I:      integer; // Auto Removed, Unused Variable
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsAdvanced);

  //SelectLoadAnimation Hidden because it is useless atm
  //AddSelect(Theme.OptionsAdvanced.SelectLoadAnimation, Ini.LoadAnimation, ILoadAnimation);
  AddSelectSlide(Theme.OptionsAdvanced.SelectScreenFade, Ini.ScreenFade, IScreenFade);
  AddSelectSlide(Theme.OptionsAdvanced.SelectEffectSing, Ini.EffectSing, IEffectSing);
  AddSelectSlide(Theme.OptionsAdvanced.SelectLineBonus, Ini.LineBonus, ILineBonus);
  AddSelectSlide(Theme.OptionsAdvanced.SelectOnSongClick, Ini.OnSongClick, IOnSongClick);
  AddSelectSlide(Theme.OptionsAdvanced.SelectAskbeforeDel, Ini.AskbeforeDel, IAskbeforeDel);
  AddSelectSlide(Theme.OptionsAdvanced.SelectPartyPopup, Ini.PartyPopup, IPartyPopup);

  AddButton(Theme.OptionsAdvanced.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);

  Interaction := 0;
end;

procedure TScreenOptionsAdvanced.onShow;
begin
  inherited;

  Interaction := 0;
end;

end.
