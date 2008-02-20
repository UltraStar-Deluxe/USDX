unit UScreenSongJumpto;

interface

{$I switches.inc}

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, SysUtils, UThemes;

type
  TScreenSongJumpto = class(TMenu)
    private
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

uses UGraphic, UMain, UIni, UTexture, ULanguage, UParty, USongs, UScreenSong, ULog;

function TScreenSongJumpto.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_0..SDLK_9, SDLK_A..SDLK_Z, SDLK_SPACE, SDLK_MINUS, SDLK_EXCLAIM, SDLK_COMMA, SDLK_SLASH, SDLK_ASTERISK, SDLK_QUESTION, SDLK_QUOTE, SDLK_QUOTEDBL, SDLK_LEFTBRACKET, SDLK_SEMICOLON:
        begin
          if Interaction = 0 then
          begin
            Button[0].Text[0].Text := Button[0].Text[0].Text + chr(ScanCode);
            SetTextFound(CatSongs.SetFilter(Button[0].Text[0].Text, SelectType));
          end;
        end;

      SDLK_BACKSPACE:
        begin
          if (Interaction = 0) AND (Length(Button[0].Text[0].Text) > 0) then
          begin
            Button[0].Text[0].DeleteLastL;
            SetTextFound(CatSongs.SetFilter(Button[0].Text[0].Text, SelectType));
          end;
        end;

      SDLK_RETURN,
      SDLK_ESCAPE:
        begin
          Visible := False;
          AudioPlayback.PlaySound(SoundLib.Back);
          if (VisSongs = 0) AND (Length(Button[0].Text[0].Text) > 0) then
          begin
            ScreenSong.UnLoadDetailedCover;
            Button[0].Text[0].Text := '';
            CatSongs.SetFilter('', 0);
            SetTextFound(0);
          end;
        end;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:
        begin
          {SelectNext;
          Button[0].Text[0].Selected := (Interaction = 0);}
        end;

      SDLK_UP:
        begin
          {SelectPrev;
          Button[0].Text[0].Selected := (Interaction = 0); }
        end;

      SDLK_RIGHT:
        begin
          Interaction := 1;
          InteractInc;
          if (Length(Button[0].Text[0].Text) > 0) then
            SetTextFound(CatSongs.SetFilter(Button[0].Text[0].Text, SelectType));
          Interaction := 0;
        end;
      SDLK_LEFT:
        begin
          Interaction := 1;
          InteractDec;
          if (Length(Button[0].Text[0].Text) > 0) then
            SetTextFound(CatSongs.SetFilter(Button[0].Text[0].Text, SelectType));
          Interaction := 0;
        end;
    end;
  end;
end;

constructor TScreenSongJumpto.Create;
var
  I:    integer;
begin
  inherited Create;

  AddText(Theme.SongJumpto.TextFound);

  LoadFromTheme(Theme.SongJumpto);

  AddButton(Theme.SongJumpto.ButtonSearchText);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, '');

  SelectType := 0;
  AddSelectSlide(Theme.SongJumpto.SelectSlideType, SelectType, Theme.SongJumpto.IType);


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
  inherited;

  //Reset Screen if no Old Search is Displayed
  if (CatSongs.CatNumShow <> -2) then
  begin
    SelectsS[0].SetSelectOpt(0);

    Button[0].Text[0].Text := '';
    Text[0].Text := Theme.SongJumpto.NoSongsFound;
  end;

  //Select Input
  Interaction := 0;
  Button[0].Text[0].Selected := True;

  LastPlayed := ScreenSong.Interaction;
end;

function TScreenSongJumpto.Draw: boolean;
begin
  Result := inherited Draw;
end;

procedure TScreenSongJumpto.SetTextFound(const Count: Cardinal);
begin
  if (Count = 0) then
  begin
    Text[0].Text := Theme.SongJumpto.NoSongsFound;
    if (Length(Button[0].Text[0].Text) = 0) then
      ScreenSong.HideCatTL
    else
      ScreenSong.ShowCatTLCustom(Format(Theme.SongJumpto.CatText, [Button[0].Text[0].Text]));
  end
  else
  begin
    Text[0].Text := Format(Theme.SongJumpto.SongsFound, [Count]);

    //Set CatTopLeftText
    ScreenSong.ShowCatTLCustom(Format(Theme.SongJumpto.CatText, [Button[0].Text[0].Text]));
  end;


  //Set visSongs
  VisSongs := Count;

  //Fix SongSelection
  ScreenSong.Interaction := high(CatSongs.Song);
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
