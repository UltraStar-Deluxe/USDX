unit UScreenOptionsGame;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, UIni, UThemes, USongs;

type
  TScreenOptionsGame = class(TMenu)
    public
      old_Tabs, old_Sorting: integer;
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure RefreshSongs;
  end;

implementation

uses UGraphic;

function TScreenOptionsGame.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_Q:
        begin
          Result := false;
        end;
      SDLK_ESCAPE :
        begin
          Music.PlayBack;
          RefreshSongs;
          FadeTo(@ScreenOptions);
        end;
      SDLK_RETURN:
        begin
          if SelInteraction = 6 then begin
            Music.PlayBack;
            RefreshSongs;
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

constructor TScreenOptionsGame.Create;
var
  I:      integer;
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsGame);

  //Refresh Songs Patch
  old_Sorting := Ini.Sorting;
  old_Tabs    := Ini.Tabs;

  AddSelect(Theme.OptionsGame.SelectPlayers, Ini.Players, IPlayers);
  AddSelect(Theme.OptionsGame.SelectDifficulty, Ini.Difficulty, IDifficulty);
  AddSelectSlide(Theme.OptionsGame.SelectLanguage, Ini.Language, ILanguage);
  AddSelect(Theme.OptionsGame.SelectTabs, Ini.Tabs, ITabs);
  AddSelectSlide(Theme.OptionsGame.SelectSorting, Ini.Sorting, ISorting);
  AddSelect(Theme.OptionsGame.SelectDebug, Ini.Debug, IDebug);


  AddButton(Theme.OptionsGame.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);

end;

//Refresh Songs Patch
procedure TScreenOptionsGame.RefreshSongs;
begin
if (ini.Sorting <> old_Sorting) or (ini.Tabs <> old_Tabs) then
    ScreenSong.Refresh;
end;

procedure TScreenOptionsGame.onShow;
begin
//  Interaction := 0;
end;

end.
