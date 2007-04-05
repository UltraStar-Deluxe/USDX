unit UScreenOptionsLyrics;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UPliki, UIni, UThemes;

type
  TScreenOptionsLyrics = class(TMenu)
    public
      constructor Create(Back: String); override;
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
      SDLK_ESCAPE :
        begin
          Ini.Save;
          Music.PlayBack;
          FadeTo(@ScreenOptions);
        end;
      SDLK_RETURN:
        begin
          if SelInteraction = 3 then begin
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
          if (SelInteraction >= 0) and (SelInteraction <= 2) then begin
            Music.PlayOption;
            InteractInc;
          end;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 2) then begin
            Music.PlayOption;
            InteractDec;
          end;
        end;
    end;
  end;
end;

constructor TScreenOptionsLyrics.Create(Back: String);
var
  I:      integer;
begin
  inherited Create(Back);

  AddBackground(Theme.OptionsLyrics.Background.Tex);

  for I := 0 to High(Theme.OptionsLyrics.Static) do
    AddStatic(Theme.OptionsLyrics.Static[I]);

  for I := 0 to High(Theme.OptionsLyrics.Text) do
    AddText(Theme.OptionsLyrics.Text[I]);

  AddSelect(Theme.OptionsLyrics.SelectLyricsFont, Ini.LyricsFont, ILyricsFont);
  AddSelect(Theme.OptionsLyrics.SelectLyricsEffect, Ini.LyricsEffect, ILyricsEffect);
  AddSelect(Theme.OptionsLyrics.SelectSolmization, Ini.Solmization, ISolmization);


  AddButton(Theme.OptionsLyrics.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[6]);

end;

procedure TScreenOptionsLyrics.onShow;
begin
  Interaction := 0;
end;

end.
