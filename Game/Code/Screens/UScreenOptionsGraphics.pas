unit UScreenOptionsGraphics;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UPliki, UIni, UThemes;

type
  TScreenOptionsGraphics = class(TMenu)
    public
      constructor Create(Back: String); override;
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
      SDLK_ESCAPE:
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

constructor TScreenOptionsGraphics.Create(Back: String);
var
  I:      integer;
begin
  inherited Create(Back);

  AddBackground(Theme.OptionsGraphics.Background.Tex);

  for I := 0 to High(Theme.OptionsGraphics.Static) do
    AddStatic(Theme.OptionsGraphics.Static[I]);

  for I := 0 to High(Theme.OptionsGraphics.Text) do
    AddText(Theme.OptionsGraphics.Text[I]);

  AddSelectSlide(Theme.OptionsGraphics.SelectSlideResolution, Ini.Resolution, IResolution);
  AddSelect(Theme.OptionsGraphics.SelectFullscreen, Ini.Fullscreen, IFullscreen);
  AddSelect(Theme.OptionsGraphics.SelectDepth, Ini.Depth, IDepth);
  AddSelect(Theme.OptionsGraphics.SelectOscilloscope, Ini.Oscilloscope, IOscilloscope);
  AddSelect(Theme.OptionsGraphics.SelectLineBonus, Ini.LineBonus, ILineBonus);
  AddSelect(Theme.OptionsGraphics.SelectMovieSize, Ini.MovieSize, IMovieSize);


  AddButton(Theme.OptionsGraphics.ButtonExit);
  AddButtonText(14, 20, 'Exit');

end;

procedure TScreenOptionsGraphics.onShow;
begin
  Interaction := 0;
end;

end.
