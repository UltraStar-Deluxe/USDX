unit UScreenOptionsLyrics;

interface

{$I switches.inc}

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, UIni, UThemes;

type
  TScreenOptionsLyrics = class(TMenu)
    public
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
  end;

implementation

uses UGraphic;

function TScreenOptionsLyrics.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
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
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenOptions);
        end;
      SDLK_RETURN:
        begin
          if SelInteraction = 4 then begin
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
          if (SelInteraction >= 0) and (SelInteraction <= 3) then begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractInc;
          end;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 3) then begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
          end;
        end;
    end;
  end;
end;

constructor TScreenOptionsLyrics.Create;
//var
// I:      integer; // Auto Removed, Unused Variable
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsLyrics);

  AddSelect(Theme.OptionsLyrics.SelectLyricsFont, Ini.LyricsFont, ILyricsFont);
  AddSelectSlide(Theme.OptionsLyrics.SelectLyricsEffect, Ini.LyricsEffect, ILyricsEffect);
  AddSelect(Theme.OptionsLyrics.SelectSolmization, Ini.Solmization, ISolmization);
  AddSelect(Theme.OptionsLyrics.SelectNoteLines, Ini.NoteLines, INoteLines);


  AddButton(Theme.OptionsLyrics.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);

end;

procedure TScreenOptionsLyrics.onShow;
begin
  inherited;

  Interaction := 0;
end;

end.
