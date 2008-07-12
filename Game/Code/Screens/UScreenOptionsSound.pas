unit UScreenOptionsSound;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, UIni, UThemes;

type
  TScreenOptionsSound = class(TMenu)
    public
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
  end;

implementation

uses UGraphic, SysUtils;

function TScreenOptionsSound.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
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
          if (SelInteraction >= 0) and (SelInteraction <= 6) then begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractInc;
          end;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 6) then begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
          end;
        end;
    end;
  end;
end;

constructor TScreenOptionsSound.Create;
//var
// I:      integer; // Auto Removed, Unused Variable
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsSound);

  AddSelectSlide(Theme.OptionsSound.SelectMicBoost, Ini.MicBoost, IMicBoost);  // TODO - This need moving to ScreenOptionsRecord
  AddSelectSlide(Theme.OptionsSound.SelectClickAssist, Ini.ClickAssist, IClickAssist);
  AddSelectSlide(Theme.OptionsSound.SelectBeatClick, Ini.BeatClick, IBeatClick);
  AddSelectSlide(Theme.OptionsSound.SelectThreshold, Ini.ThresholdIndex, IThreshold);

  //Song Preview
  AddSelectSlide(Theme.OptionsSound.SelectSlidePreviewVolume, Ini.PreviewVolume, IPreviewVolume);
  AddSelectSlide(Theme.OptionsSound.SelectSlidePreviewFading, Ini.PreviewFading, IPreviewFading);

  AddButton(Theme.OptionsSound.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);

  Interaction := 0;
end;

procedure TScreenOptionsSound.onShow;
begin
  inherited;

  Interaction := 0;
end;

end.
