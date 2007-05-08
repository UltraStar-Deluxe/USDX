unit UScreenPartyOptions;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UPliki, SysUtils, UThemes;

type
  TScreenPartyOptions = class(TMenu)
    public
      SelectLevel: Cardinal;
      SelectPlayList: Cardinal;
      SelectPlayList2: Cardinal;
      SelectRounds: Cardinal;
      SelectTeams: Cardinal;
      SelectPlayers1: Cardinal;
      SelectPlayers2: Cardinal;
      SelectPlayers3: Cardinal;

      PlayList:  Integer;
      PlayList2: Integer;
      Rounds:    Integer;
      NumTeams:  Integer;
      NumPlayer1, NumPlayer2, NumPlayer3: Integer;
      
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure SetAnimationProgress(Progress: real); override;
      procedure SetPlaylist2;
  end;

var
  IPlaylist: array[0..2] of String;
  IPlaylist2: array of String;
const
  ITeams: array[0..1] of String =('2', '3');
  IPlayers: array[0..3] of String =('1', '2', '3', '4');
  IRounds: array[0..5] of String = ('2', '3', '4', '5', '6', '7');

implementation

uses UGraphic, UMain, UIni, UTexture, ULanguage, UParty, UDLLManager, UPlaylist, USongs;

function TScreenPartyOptions.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
  var
    I, J: Integer;
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
          FadeTo(@ScreenMain);
        end;

      SDLK_RETURN:
        begin
          //Save Difficulty
          Ini.Difficulty := SelectsS[SelectLevel].SelectedOption;
          Ini.SaveLevel;
          //Save PlayList
          //(Todo)
          //Save Num Teams:
          PartySession.Teams.NumTeams := NumTeams + 2;
          PartySession.Teams.Teaminfo[0].NumPlayers := NumPlayer1+1;
          PartySession.Teams.Teaminfo[1].NumPlayers := NumPlayer2+1;
          PartySession.Teams.Teaminfo[2].NumPlayers := NumPlayer3+1;

          //Save Playlist
          PlaylistMan.Mode := Playlist;
          //If Category Selected Search Category ID
          if Playlist = 1 then
          begin
            J := -1;
            For I := 0 to high(CatSongs.Song) do
            begin
              if CatSongs.Song[I].Main then
                Inc(J);

              if J = Playlist2 then
              begin
                PlaylistMan.CurPlayList := I;
                Break;
              end;
            end;
          end
          else
            PlaylistMan.CurPlayList := Playlist2;

          //Start Party
          PartySession.StartNewParty(Rounds + 2);

          Music.PlayStart;
          //Go to Player Screen
          FadeTo(@ScreenPartyPlayer);
        end;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:    InteractNext;
      SDLK_UP:      InteractPrev;
      SDLK_RIGHT:
        begin
          Music.PlayOption;
          InteractInc;

          //Change Playlist2 if Playlist is Changed
          If (Interaction = 1) then
          begin
            SetPlaylist2;
          end //Change Team3 Players visibility
          Else If (Interaction = 4) then
          begin
              SelectsS[7].Visible := (NumTeams = 1);
          end;
        end;
      SDLK_LEFT:
        begin
          Music.PlayOption;
          InteractDec;

          //Change Playlist2 if Playlist is Changed
          If (Interaction = 1) then
          begin
            SetPlaylist2;
          end //Change Team3 Players visibility
          Else If (Interaction = 4) then
          begin
            SelectsS[7].Visible := (NumTeams = 1);
          end;
        end;
    end;
  end;
end;

constructor TScreenPartyOptions.Create;
var
  I:    integer;
begin
  inherited Create;
  //Fill IPlaylist
  IPlaylist[0] := Language.Translate('PARTY_PLAYLIST_ALL');
  IPlaylist[1] := Language.Translate('PARTY_PLAYLIST_CATEGORY');
  IPlaylist[2] := Language.Translate('PARTY_PLAYLIST_PLAYLIST');

  //Fill IPlaylist2
  SetLength(IPlaylist2, 1);
  IPlaylist2[0] := '---';

  //Clear all Selects
  NumTeams := 0;
  NumPlayer1 := 0;
  NumPlayer2 := 0;
  NumPlayer3 := 0;
  Rounds := 5;
  PlayList := 0;
  PlayList2 := 0;

  //Load Screen From Theme
  LoadFromTheme(Theme.PartyOptions);

  SelectLevel := AddSelectSlide (Theme.PartyOptions.SelectLevel, Ini.Difficulty, Theme.ILevel);
  SelectPlayList := AddSelectSlide (Theme.PartyOptions.SelectPlayList, PlayList, IPlaylist);
  SelectPlayList2 := AddSelectSlide (Theme.PartyOptions.SelectPlayList2, PlayList2, IPlaylist2);
  SelectRounds := AddSelectSlide (Theme.PartyOptions.SelectRounds, Rounds, IRounds);
  SelectTeams := AddSelectSlide (Theme.PartyOptions.SelectTeams, NumTeams, ITeams);
  SelectPlayers1 := AddSelectSlide (Theme.PartyOptions.SelectPlayers1, NumPlayer1, IPlayers);
  SelectPlayers2 := AddSelectSlide (Theme.PartyOptions.SelectPlayers2, NumPlayer2, IPlayers);
  SelectPlayers3 := AddSelectSlide (Theme.PartyOptions.SelectPlayers3, NumPlayer3, IPlayers);

  Interaction := 0;

  //Hide Team3 Players
  SelectsS[7].Visible := False;
end;

procedure TScreenPartyOptions.SetPlaylist2;
var I: Integer;
begin
  Case Playlist of
    0:
      begin
        SetLength(IPlaylist2, 1);
        IPlaylist2[0] := '---';
      end;
    1:
      begin
        SetLength(IPlaylist2, 0);
        For I := 0 to high(CatSongs.Song) do
        begin
          If (CatSongs.Song[I].Main) then
          begin
            SetLength(IPlaylist2, Length(IPlaylist2) + 1);
            IPlaylist2[high(IPlaylist2)] := CatSongs.Song[I].Artist;
          end;
        end;
      end;
    2:
      begin
        SetLength(IPlaylist2, Length(PlaylistMan.Playlists));
        PlaylistMan.GetNames(IPlaylist2);
      end;
  end;

  Playlist2 := 0;
  UpdateSelectSlideOptions(Theme.PartyOptions.SelectPlayList2, 2, IPlaylist2, Playlist2);
end;

procedure TScreenPartyOptions.onShow;
begin
  Randomize;

//  LCD.WriteText(1, '  Choose mode:  ');
//  UpdateLCD;
end;

procedure TScreenPartyOptions.SetAnimationProgress(Progress: real);
begin
  {for I := 0 to 6 do
    SelectS[I].Texture.ScaleW := Progress;}
end;

end.
