unit UScreenOptionsGraphics;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, UIni, UThemes;

type
  TScreenOptionsGraphics = class(TMenu)
    public
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
  end;

implementation

uses UGraphic, UMain;

function TScreenOptionsGraphics.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
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
{          if SelInteraction <= 1 then begin
            Restart := true;
          end;}
          if SelInteraction = 5 then begin
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
          if (SelInteraction >= 0) and (SelInteraction <= 4) then begin
            Music.PlayOption;
            InteractInc;
          end;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 4) then begin
            Music.PlayOption;
            InteractDec;
          end;
        end;
    end;
  end;
end;

constructor TScreenOptionsGraphics.Create;
var
  I:      integer;
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsGraphics);

  AddSelectSlide(Theme.OptionsGraphics.SelectSlideResolution, Ini.Resolution, IResolution);
  AddSelect(Theme.OptionsGraphics.SelectFullscreen, Ini.Fullscreen, IFullscreen);
  AddSelect(Theme.OptionsGraphics.SelectDepth, Ini.Depth, IDepth);
  AddSelect(Theme.OptionsGraphics.SelectOscilloscope, Ini.Oscilloscope, IOscilloscope);
  AddSelect(Theme.OptionsGraphics.SelectMovieSize, Ini.MovieSize, IMovieSize);


  AddButton(Theme.OptionsGraphics.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);

end;

procedure TScreenOptionsGraphics.onShow;
begin
  Interaction := 0;
end;

end.
