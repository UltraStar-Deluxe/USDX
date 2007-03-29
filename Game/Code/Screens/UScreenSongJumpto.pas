unit UScreenSongJumpto;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UPliki, SysUtils, UThemes;

type
  TScreenSongJumpto = class(TMenu)
    private
      Songsfound: String;
      NoSongsfound: String;
      CatText: String;

      //For ChangeMusic
      LastPlayed: Integer;
      VisibleBool: Boolean;
    public
      VisSongs: Integer;

      constructor Create; override;

      //Visible //Whether the Menu should be Drawn
      //Whether the Menu should be Drawn
      procedure SetVisible(Value: Boolean);
      property Visible: Boolean read VisibleBool write SetVisible;

      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      function Draw: boolean; override;

      procedure SetTextFound(const Count: Cardinal);
  end;

var
  IType: Array [0..2] of String;
  SelectType: Integer;


implementation

uses UGraphic, UMain, UIni, UTexture, ULanguage, UParty, USongs, UScreenSong;

function TScreenSongJumpto.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
  function IsVisible: Boolean;
  begin
    Result := True;
    if (Interactions[Interaction].Typ = 0) then
    begin
      Result := Button[Interactions[Interaction].Num].Visible;
    end
    else if (Interactions[Interaction].Typ = 1) then
    begin
      //Result := Selects[Interactions[Interaction].Num].Visible;
    end
    else if (Interactions[Interaction].Typ = 3) then
    begin
      Result := SelectsS[Interactions[Interaction].Num].Visible;
    end;
  end;

  Procedure SelectNext;
  begin
    repeat
      InteractNext;
    until IsVisible;
  end;

  Procedure SelectPrev;
  begin
    repeat
      InteractPrev;
    until IsVisible;
  end;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_0..SDLK_9, SDLK_A..SDLK_Z, SDLK_SPACE, SDLK_MINUS, SDLK_EXCLAIM, SDLK_COMMA, SDLK_SLASH, SDLK_ASTERISK, SDLK_QUESTION, SDLK_QUOTE, SDLK_QUOTEDBL:
        begin
          if Interaction = 0 then
          begin
            Button[0].Text[0].Text := Button[0].Text[0].Text + chr(ScanCode);
            SetTextFound(CatSongs.SetFilter(Button[0].Text[0].Text, SelectType));
          end;
        end;

      SDLK_BACKSPACE:
        begin
          if Interaction = 0 then
          begin
            Button[0].Text[0].DeleteLastL;
            SetTextFound(CatSongs.SetFilter(Button[0].Text[0].Text, SelectType));
          end;
        end;

      SDLK_ESCAPE :
        begin
          Music.PlayBack;
          Visible := False;
          if VisSongs = 0 then
            CatSongs.SetFilter('', 0);
        end;

      SDLK_RETURN:
        begin
          Visible := False;
          Music.PlayBack;
          if VisSongs = 0 then
            CatSongs.SetFilter('', 0);
        end;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:    SelectNext;
      SDLK_UP:      SelectPrev;

      SDLK_RIGHT:
        begin
          if (Interaction=1) then
          begin
            InteractInc;
            SetTextFound(CatSongs.SetFilter(Button[0].Text[0].Text, SelectType));
          end;
        end;
      SDLK_LEFT:
        begin
          if (Interaction=1) then
          begin
            InteractDec;
            SetTextFound(CatSongs.SetFilter(Button[0].Text[0].Text, SelectType));
          end;
        end;
    end;
  end
  else // Key Up
    case PressedKey of
      SDLK_RETURN :
        begin
        end;
    end;
end;

constructor TScreenSongJumpto.Create;
var
  I:    integer;
begin
  inherited Create;
  //Translate Texts     //TODO: Port to UTheme (Linebonus and PartyScreens, too
  IType[0] := Language.Translate('SONG_JUMPTO_TYPE1');
  IType[1] := Language.Translate('SONG_JUMPTO_TYPE2');
  IType[2] := Language.Translate('SONG_JUMPTO_TYPE3');
  SongsFound := Language.Translate('SONG_JUMPTO_SONGSFOUND');
  NoSongsFound := Language.Translate('SONG_JUMPTO_NOSONGSFOUND');
  CatText := Language.Translate('SONG_JUMPTO_CATTEXT');

  AddBackground(Theme.SongJumpto.Background.Tex);

  AddButton(Theme.SongJumpto.ButtonSearchText);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, '');

  SelectType := 0;
  AddSelectSlide(Theme.SongJumpto.SelectSlideType, SelectType, IType);

  AddText(Theme.SongJumpto.TextFound);

  for I := 0 to High(Theme.SongJumpto.Static) do
    AddStatic(Theme.SongJumpto.Static[I]);

  for I := 0 to High(Theme.SongJumpto.Text) do
    AddText(Theme.SongJumpto.Text[I]);

  Interaction := 0;
  LastPlayed  := 0;
end;

procedure TScreenSongJumpto.SetVisible(Value: Boolean);
begin
//If change from unvisible to Visible then OnShow
  if (VisibleBool = False) AND (Value = True) then
    OnShow;

  VisibleBool := Value;
end;

procedure TScreenSongJumpto.onShow;
begin
  //Reset Screen if no Old Search is Displayed
  if (CatSongs.CatNumShow <> -2) then
  begin
    Interaction := 0;
    SelectType := 0;

    Button[0].Text[0].Text := '';
    Text[0].Text := NoSongsFound;
  end;
end;

function TScreenSongJumpto.Draw: boolean;
begin
  Result := inherited Draw;
end;

procedure TScreenSongJumpto.SetTextFound(const Count: Cardinal);
begin
  if (Count = 0) then
    Text[0].Text := NoSongsFound
  else
    Text[0].Text := Format(SongsFound, [Count]);

  //Set CatTopLeftText
  ScreenSong.ShowCatTLCustom(Format(CatText, [Button[0].Text[0].Text]));

  //visSongs setzen
  VisSongs := Count;

  //Fix SongSelection
  ScreenSong.Interaction := 0;
  ScreenSong.SelectNext;
  ScreenSong.FixSelected;

  //Play Correct Music
  if (ScreenSong.Interaction <> LastPlayed) then
  begin
    LastPlayed := ScreenSong.Interaction;

    ScreenSong.ChangeMusic;
  end;
end;

end.
