unit UScreenOptionsGraphics;

interface

{$I switches.inc}

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, UIni, UThemes;

type
  TScreenOptionsGraphics = class(TMenu)
    public
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
  end;

implementation

uses UGraphic, UMain, SysUtils, TypInfo;

function TScreenOptionsGraphics.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
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
{          if SelInteraction <= 1 then begin
            Restart := true;
          end;}
          if SelInteraction = 6 then begin
            Ini.Save;
            AudioPlayback.PlaySound(SoundLib.Back);
            // FIXME: changing the video mode does not work this way in windows
            // and MacOSX as all textures will be invalidated through this.
            // See the ALT+TAB code too.
            {$IFDEF Linux}
            Reinitialize3D();
            {$ENDIF}
            FadeTo(@ScreenOptions);
          end;
        end;
      SDLK_DOWN:
        InteractNext;
      SDLK_UP :
        InteractPrev;
      SDLK_RIGHT:
        begin
          if (SelInteraction >= 0) and (SelInteraction < 6) then begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractInc;
          end;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction < 6) then begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
          end;
        end;
    end;
  end;
end;

constructor TScreenOptionsGraphics.Create;
//var
// I:      integer; // Auto Removed, Unused Variable
begin
  inherited Create;
  LoadFromTheme(Theme.OptionsGraphics);

  AddSelectSlide(Theme.OptionsGraphics.SelectResolution, Ini.Resolution, IResolution);
  AddSelectSlide(Theme.OptionsGraphics.SelectFullscreen, Ini.Fullscreen, IFullscreen);
  AddSelectSlide(Theme.OptionsGraphics.SelectDepth,      Ini.Depth, IDepth);
  AddSelectSlide(Theme.OptionsGraphics.SelectVisualizer, Ini.VisualizerOption, IVisualizer);
  AddSelectSlide(Theme.OptionsGraphics.SelectOscilloscope, Ini.Oscilloscope, IOscilloscope);
  AddSelectSlide(Theme.OptionsGraphics.SelectMovieSize, Ini.MovieSize, IMovieSize);


  AddButton(Theme.OptionsGraphics.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);

end;

procedure TScreenOptionsGraphics.onShow;
begin
  inherited;

  Interaction := 0;
end;

end.
