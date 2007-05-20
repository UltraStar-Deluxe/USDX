unit UScreenSong;

interface

uses
  UMenu, SDL, UMusic, UFiles, UTime, UDisplay, USongs, SysUtils, ULog, UThemes, UTexture, ULanguage,
  ULCD, ULight, UIni;

type
  TScreenSong = class(TMenu)
    public
      TextArtist:   integer;
      TextTitle:    integer;
      TextNumber:   integer;

      //Video Icon Mod
      VideoIcon:         Cardinal;

      TextCat:   integer;
      StaticCat: integer;

      SongCurrent:  real;
      SongTarget:   real;

      HighSpeed:    boolean;
      CoverFull:    boolean;
      CoverTime:    real;
      CoverX:       integer;
      CoverY:       integer;
      CoverW:       integer;
      is_jump:      boolean; // Jump to Song Mod
      is_jump_title:boolean; //Jump to SOng MOd-YTrue if search for Title

      EqualizerBands: array of Byte;
      EqualizerTime: Cardinal;
      EqualizerTime2: Byte;

      //Party Mod
      Mode: Byte;  //0 = Standard, 1= Go to PartyMode after Selection + Change to Random Song at Show
      //party Statics (Joker)
      StaticTeam1Joker1: Cardinal;
      StaticTeam1Joker2: Cardinal;
      StaticTeam1Joker3: Cardinal;
      StaticTeam1Joker4: Cardinal;
      StaticTeam1Joker5: Cardinal;

      StaticTeam2Joker1: Cardinal;
      StaticTeam2Joker2: Cardinal;
      StaticTeam2Joker3: Cardinal;
      StaticTeam2Joker4: Cardinal;
      StaticTeam2Joker5: Cardinal;

      StaticTeam3Joker1: Cardinal;
      StaticTeam3Joker2: Cardinal;
      StaticTeam3Joker3: Cardinal;
      StaticTeam3Joker4: Cardinal;
      StaticTeam3Joker5: Cardinal;

      StaticParty:    Array of Cardinal;
      TextParty:      Array of Cardinal;
      StaticNonParty: Array of Cardinal;
      TextNonParty:   Array of Cardinal;


      constructor Create; override;
      procedure SetScroll;
      procedure SetScroll1;
      procedure SetScroll2;
      procedure SetScroll3;
      procedure SetScroll4;
      procedure SetScroll5;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      function Draw: boolean; override;
      procedure onShow; override;
      procedure onHide; override;
      procedure SelectNext;
      procedure SelectPrev;
      procedure UpdateLCD;
      procedure SkipTo(Target: Cardinal);
      procedure FixSelected; //Show Wrong Song when Tabs on Fix
      procedure FixSelected2; //Show Wrong Song when Tabs on Fix
      procedure ShowCatTL(Cat: Integer);// Show Cat in Top left
      procedure ShowCatTLCustom(Caption: String);// Show Custom Text in Top left
      procedure HideCatTL;// Show Cat in Tob left
      procedure Refresh; //Refresh Song Sorting
      procedure DrawEqualizer;
      procedure ChangeMusic;
      //Party Mode
      procedure SelectRandomSong;
      procedure SetJoker;
      procedure SetStatics;
      //procedures for Menu
      procedure StartSong;
      procedure OpenEditor;
      procedure DoJoker(Team: Byte);
      procedure SelectPlayers;

      //Extensions
      procedure DrawExtensions;
  end;

implementation
uses UGraphic, UMain, UCovers, math, OpenGL12, Windows, USkins, UDLLManager, UParty, UPlaylist, UScreenSongMenu;

// ***** Public methods ****** //

//Show Wrong Song when Tabs on Fix
procedure TScreenSong.FixSelected;
var I, I2: Integer;
  begin
    if CatSongs.VisibleSongs > 0 then
    begin
      I2:= 0;
      for I := low(CatSongs.Song) to High(Catsongs.Song) do
      begin
        if CatSongs.Song[I].Visible then
          inc(I2);

        if I = Interaction - 1 then
          break;
      end;

      SongCurrent := I2;
      SongTarget  := I2;
    end;
  end;

procedure TScreenSong.FixSelected2;
var I, I2: Integer;
  begin
    if CatSongs.VisibleSongs > 0 then
    begin
      I2:= 0;
      for I := low(CatSongs.Song) to High(Catsongs.Song) do
      begin
        if CatSongs.Song[I].Visible then
          inc(I2);

        if I = Interaction - 1 then
          break;
      end;

      SongTarget  := I2;
    end;
  end;
//Show Wrong Song when Tabs on Fix End

  procedure TScreenSong.ShowCatTLCustom(Caption: String);// Show Custom Text in Top left
  begin
    Text[TextCat].Text := Caption;
    Text[TextCat].Visible := true;
    Static[StaticCat].Visible := False;
  end;

  //Show Cat in Top Left Mod
  procedure TScreenSong.ShowCatTL(Cat: Integer);
    begin
    //Change
    Text[TextCat].Text := CatSongs.Song[Cat].Artist;
    //showmessage(CatSongs.Song[Cat].Path + CatSongs.Song[Cat].Cover);
    //Static[StaticCat].Texture := Texture.GetTexture(Button[Cat].Texture.Name, 'Plain', true);

    Static[StaticCat].Texture := Texture.GetTexture(Button[Cat].Texture.Name, 'Plain', true);
    //Texture.GetTexture(Button[Cat].Texture.Name, 'Plain', false);
    //Button[Cat].
    //Cover


    //Show
    Text[TextCat].Visible := true;
    Static[StaticCat].Visible := True;
    end;

  procedure TScreenSong.HideCatTL;
    begin
    //Hide
    //Text[TextCat].Visible := false;
    Static[StaticCat].Visible := false;
    //New -> Show Text specified in Theme
    Text[TextCat].Visible := True;
    Text[TextCat].Text := Theme.Song.TextCat.Text;
    end;
    //Show Cat in Top Left Mod End


// Method for input parsing. If False is returned, GetNextWindow
// should be checked to know the next window to load;
function TScreenSong.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
var
  I:      integer;
  I2:     integer;
  HS:     integer;
  SDL_ModState:  Word;
  Letter: Char;
begin
  Result := true;

  //Song Screen Extensions (Jumpto + Menu)
  if (ScreenSongMenu.Visible) then
  begin
    Result := ScreenSongMenu.ParseInput(PressedKey, ScanCode, PressedDown);
    Exit;
  end
  else if (ScreenSongJumpto.Visible) then
  begin
    Result := ScreenSongJumpto.ParseInput(PressedKey, ScanCode, PressedDown);
    Exit;
  end;

  If (PressedDown) Then
  begin // Key Down

    SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);

    //Jump to Artist/Titel
    if (SDL_ModState and KMOD_LALT <> 0) AND (Mode = 0) AND (PressedKey >= SDLK_A) AND (PressedKey <= SDLK_Z) then
    begin
      Letter := UpCase(Chr(ScanCode));
      I2 := Length(CatSongs.Song);

      //Jump To Titel
      if (SDL_ModState = KMOD_LALT or KMOD_LSHIFT) then
      begin
        For I := 1 to high(CatSongs.Song) do
        begin
          if (CatSongs.Song[(I + Interaction) mod I2].Visible) AND (Length(CatSongs.Song[(I + Interaction) mod I2].Title)>0) AND (UpCase(CatSongs.Song[(I + Interaction) mod I2].Title[1]) = Letter) then
          begin
            SkipTo(CatSongs.VisibleIndex((I + Interaction) mod I2));

            Music.PlayChange;

            ChangeMusic;
            SetScroll4;
            UpdateLCD;
            //Break and Exit
            Exit;
          end;
        end;
      end
      //Jump to Artist
      else  if (SDL_ModState = KMOD_LALT) then
      begin
        For I := 1 to high(CatSongs.Song) do
        begin
          if (CatSongs.Song[(I + Interaction) mod I2].Visible) AND (Length(CatSongs.Song[(I + Interaction) mod I2].Artist)>0) AND (UpCase(CatSongs.Song[(I + Interaction) mod I2].Artist[1]) = Letter) then
          begin
            SkipTo(CatSongs.VisibleIndex((I + Interaction) mod I2));

            Music.PlayChange;

            ChangeMusic;
            SetScroll4;
            UpdateLCD;

            //Break and Exit
            Exit;
          end;
        end;
      end;
      Exit;
    end;

    case PressedKey of
      SDLK_Q:
        begin
          Result := false;
        end;
      SDLK_ESCAPE :
        begin
        if (Mode = 0) then
        begin
          //On Escape goto Cat-List Hack
          if (Ini.Tabs_at_startup = 1) AND (CatSongs.CatNumShow <> -1) then
            begin
            //Find Category
            I := Interaction;
            while not catsongs.Song[I].Main  do
              begin
              Dec (I);
              if (I < low(catsongs.Song)) then
                break;
              end;
            if (I<= 1) then
            Interaction := high(catsongs.Song)
            else
            Interaction := I - 1;

            //Stop Music
            Music.Stop;

            CatSongs.ShowCategoryList;

            //Show Cat in Top Left Mod
            HideCatTL;


            //Show Wrong Song when Tabs on Fix
            SelectNext;
            FixSelected;
            //SelectPrev;
            //CatSongs.Song[0].Visible := False;
            end
          else
          begin
          //On Escape goto Cat-List Hack End
            //Tabs off and in Search or Playlist -> Go back to Song view
            if (CatSongs.CatNumShow < -1) then
            begin
              //Atm: Set Empty Filter
              CatSongs.SetFilter('', 0);

              //Show Cat in Top Left Mod
              HideCatTL;
              Interaction := 0;

              //Show Wrong Song when Tabs on Fix
              SelectNext;
              FixSelected;

              ChangeMusic;
            end
            else
            begin
              Music.Stop;
              Music.PlayBack;

              FadeTo(@ScreenMain);
            end;

          end;
        end
        //When in party Mode then Ask before Close
        else if (Mode = 1) then
        begin
          Music.PlayBack;
          CheckFadeTo(@ScreenMain,'MSG_END_PARTY');
        end;
        end;
      SDLK_RETURN:
        begin
          if Length(Songs.Song) > 0 then begin
//            PortWriteB($378, 0);
            if CatSongs.Song[Interaction].Main then begin // clicked on Category Button

              //Show Cat in Top Left Mod
              ShowCatTL (Interaction);

              //I := CatSongs.VisibleIndex(Interaction);
              CatSongs.ClickCategoryButton(Interaction);
              {I2 := CatSongs.VisibleIndex(Interaction);
              SongCurrent := SongCurrent - I + I2;
              SongTarget := SongTarget - I + I2; }

//              if I<>I2 then beep;
            //  SetScroll4;

            //Show Wrong Song when Tabs on Fix
            SelectNext;
            FixSelected;

            //Play Music:
            ChangeMusic;

            end else begin // clicked on song
              if (Mode = 0) then //Normal Mode -> Start Song
              begin
                //Do the Action that is specified in Ini
                case Ini.OnSongClick of
                  0: StartSong;
                  1: SelectPlayers;
                  2:begin
                      If (CatSongs.CatNumShow = -3) then
                        ScreenSongMenu.MenuShow(SM_Playlist)
                      else
                        ScreenSongMenu.MenuShow(SM_Main);
                    end;
                end;
              end
              else if (Mode = 1) then //PartyMode -> Show Menu
              begin
                //Is this Right?
                if (Ini.PartyPopup = 1) then
                  ScreenSongMenu.MenuShow(SM_Party_Main)
                else
                  ScreenSong.StartSong;
              end;
            end;
          end;
        end;

      SDLK_M: //Show SongMenu
        begin
          if (Length(Songs.Song) > 0) then begin
            if (Mode = 0) then begin
              if not CatSongs.Song[Interaction].Main then begin // clicked on Song
                if CatSongs.CatNumShow = -3 then
                  ScreenSongMenu.MenuShow(SM_Playlist)
                else
                  ScreenSongMenu.MenuShow(SM_Main);
              end
              else
              begin
                ScreenSongMenu.MenuShow(SM_Playlist_Load);
              end;
            end //Party Mode -> Show Party Menu
            else ScreenSongMenu.MenuShow(SM_Party_Main);
          end;
        end;

      SDLK_P: //Show Playlist Menu
        begin
          if (Length(Songs.Song) > 0) AND (Mode = 0) then begin
              ScreenSongMenu.MenuShow(SM_Playlist_Load);
          end;
        end;

      SDLK_J: //Show Jumpto Menu
        begin
          if (Length(Songs.Song) > 0) AND (Mode = 0) then
          begin
            ScreenSongJumpto.Visible := True;
          end;
        end;

      SDLK_DOWN:
        begin
        if (Mode = 0) then
        begin
{          if Length(Songs.Song) > 0 then begin
            Music.PlayChange;
            InteractNext;
            Music.Close;
            if Music.Open(CatSongs.Song[Interaction].Path + CatSongs.Song[Interaction].Mp3) then Music.Play;
            SetScroll;
          end;}

          //Cat Change Hack
          if Ini.Tabs_at_startup = 1 then
            begin
            I := Interaction;
            if I <= 0 then I := 1;

            while not catsongs.Song[I].Main do
              begin
              Inc (I);
              if (I > high(catsongs.Song)) then
                I := low(catsongs.Song);
              end;

            Interaction := I;

            //Show Cat in Top Left Mod
            ShowCatTL (Interaction);

            CatSongs.ClickCategoryButton(Interaction);
            SelectNext;
            FixSelected;

            //Play Music:
            Music.PlayChange;
            ChangeMusic;

            end;

          //
          //Cat Change Hack End}
        end;
        end;
      SDLK_UP:
        begin
        if (Mode = 0) then
        begin
{          if Length(Songs.Song) > 0 then begin
            Music.PlayChange;
            InteractPrev;
            Music.Close;
            if Music.Open(CatSongs.Song[Interaction].Path + CatSongs.Song[Interaction].Mp3) then Music.Play;
            SetScroll;
          end;}
          //Cat Change Hack
          if Ini.Tabs_at_startup = 1 then
            begin
            I := Interaction;
            I2 := 0;
            if I <= 0 then I := 1;

            while not catsongs.Song[I].Main or (I2 = 0) do
              begin
              if catsongs.Song[I].Main then
                Inc(I2);
              Dec (I);
              if (I < low(catsongs.Song)) then
                I := high(catsongs.Song);
              end;

            Interaction := I;

            //Show Cat in Top Left Mod
            ShowCatTL (I);

            CatSongs.ClickCategoryButton(I);
            SelectNext;
            FixSelected;

            //Play Music:
            Music.PlayChange;
            ChangeMusic;

            end;

          //
          //Cat Change Hack End}
        end;
        end;

      SDLK_RIGHT:
        begin
          if (Length(Songs.Song) > 0) AND (Mode = 0) then begin
            Music.PlayChange;
            SelectNext;
//            InteractNext;
//           SongTarget := Interaction;
            ChangeMusic;
            SetScroll4;
            UpdateLCD;
            Light.LightOne(1, 200);
          end;
        end;

      SDLK_LEFT:
        begin
          if (Length(Songs.Song) > 0)AND (Mode = 0)  then begin
            Music.PlayChange;
            SelectPrev;
            ChangeMusic;
            SetScroll4;
            UpdateLCD;
            Light.LightOne(0, 200);
          end;
        end;

      SDLK_E:
        begin
          OpenEditor;
        end;

      SDLK_R:
        begin
          if (Length(Songs.Song) > 0) AND (Mode = 0) then begin

            if (SDL_ModState = KMOD_LSHIFT) AND (Ini.Tabs_at_startup = 1) then //Random Category
            begin
            I2 := 0; //Count Cats
            for I:= low(CatSongs.Song) to high (CatSongs.Song) do
              if CatSongs.Song[I].Main then Inc(I2);

            I2 := Random (I2)+1; //Zufall

            //Find Cat:
            for I:= low(CatSongs.Song) to high (CatSongs.Song) do
              begin
              if CatSongs.Song[I].Main then
                Dec(I2);
              if (I2<=0) then
                begin
                //Show Cat in Top Left Mod
                ShowCatTL (I);

                Interaction := I;

                CatSongs.ShowCategoryList;
                CatSongs.ClickCategoryButton(I);
                SelectNext;
                FixSelected;
                break;
                end;
              end;


            end
            else if (SDL_ModState = KMOD_LCTRL) AND (Ini.Tabs_at_startup = 1) then //random in All Categorys
            begin
            repeat
              I2 := Random(high(CatSongs.Song)+1) - low(CatSongs.Song)+1;
            until CatSongs.Song[I2].Main = false;

            //Search Cat
            for I := I2 downto low(CatSongs.Song) do
              begin
              if CatSongs.Song[I].Main then
                break;
              end;
            //In I ist jetzt die Kategorie in I2 der Song

            //Choose Cat
            CatSongs.ShowCategoryList;

            //Show Cat in Top Left Mod
              ShowCatTL (I);

            CatSongs.ClickCategoryButton(I);
            SelectNext;

            //Fix: Not Existing Song selected:
            //if (I+1=I2) then Inc(I2);

            //Choose Song
            SkipTo(I2-I);

            end
            else //Random in one Category
            begin
            SkipTo(Random(CatSongs.VisibleSongs));
            end;
            Music.PlayChange;

            ChangeMusic;
            SetScroll4;
            UpdateLCD;
          end;
        end;

      SDLK_1:
        begin //Jocker
          if (Mode = 1) AND (PartySession.Teams.NumTeams >= 1) AND (PartySession.Teams.Teaminfo[0].Joker > 0) then
          begin
            //Joker spielen
            Dec(PartySession.Teams.Teaminfo[0].Joker);
            SelectRandomSong;
            SetJoker;
          end;
        end;

      SDLK_2:
        begin //Jocker
          if (Mode = 1) AND (PartySession.Teams.NumTeams >= 2) AND (PartySession.Teams.Teaminfo[1].Joker > 0) then
          begin
            //Joker spielen
            Dec(PartySession.Teams.Teaminfo[1].Joker);
            SelectRandomSong;
            SetJoker;
          end;
        end;

      SDLK_3:
        begin //Jocker
          if (Mode = 1) AND (PartySession.Teams.NumTeams >= 3) AND (PartySession.Teams.Teaminfo[2].Joker > 0) then
          begin
            //Joker spielen
            Dec(PartySession.Teams.Teaminfo[2].Joker);
            SelectRandomSong;
            SetJoker;
          end;
        end;
    end;
  end;
end;

constructor TScreenSong.Create;
var
  Pet:    integer;
  I:      integer;
begin
  inherited Create;

  LoadFromTheme(Theme.Song);

  TextArtist := AddText(Theme.Song.TextArtist);
  TextTitle :=  AddText(Theme.Song.TextTitle);
  TextNumber := AddText(Theme.Song.TextNumber);

  //Show Cat in Top Left mod
  TextCat := AddText(Theme.Song.TextCat);
  StaticCat :=  AddStatic(Theme.Song.StaticCat);

  //Show Video Icon Mod
  VideoIcon := AddStatic(Theme.Song.VideoIcon);

  //Party Mode
  StaticTeam1Joker1 := AddStatic(Theme.Song.StaticTeam1Joker1);
  StaticTeam1Joker2 := AddStatic(Theme.Song.StaticTeam1Joker2);
  StaticTeam1Joker3 := AddStatic(Theme.Song.StaticTeam1Joker3);
  StaticTeam1Joker4 := AddStatic(Theme.Song.StaticTeam1Joker4);
  StaticTeam1Joker5 := AddStatic(Theme.Song.StaticTeam1Joker5);

  StaticTeam2Joker1 := AddStatic(Theme.Song.StaticTeam2Joker1);
  StaticTeam2Joker2 := AddStatic(Theme.Song.StaticTeam2Joker2);
  StaticTeam2Joker3 := AddStatic(Theme.Song.StaticTeam2Joker3);
  StaticTeam2Joker4 := AddStatic(Theme.Song.StaticTeam2Joker4);
  StaticTeam2Joker5 := AddStatic(Theme.Song.StaticTeam2Joker5);

  StaticTeam3Joker1 := AddStatic(Theme.Song.StaticTeam3Joker1);
  StaticTeam3Joker2 := AddStatic(Theme.Song.StaticTeam3Joker2);
  StaticTeam3Joker3 := AddStatic(Theme.Song.StaticTeam3Joker3);
  StaticTeam3Joker4 := AddStatic(Theme.Song.StaticTeam3Joker4);
  StaticTeam3Joker5 := AddStatic(Theme.Song.StaticTeam3Joker5);

  //Load Party or NonParty specific Statics and Texts
  SetLength(StaticParty, Length(Theme.Song.StaticParty));
  for I := 0 to High(Theme.Song.StaticParty) do
    StaticParty[I] := AddStatic(Theme.Song.StaticParty[I]);

  SetLength(TextParty, Length(Theme.Song.TextParty));
  for I := 0 to High(Theme.Song.TextParty) do
    TextParty[I] := AddText(Theme.Song.TextParty[I]);

  SetLength(StaticNonParty, Length(Theme.Song.StaticNonParty));
  for I := 0 to High(Theme.Song.StaticNonParty) do
    StaticNonParty[I] := AddStatic(Theme.Song.StaticNonParty[I]);

  SetLength(TextNonParty, Length(Theme.Song.TextNonParty));
  for I := 0 to High(Theme.Song.TextNonParty) do
    TextNonParty[I] := AddText(Theme.Song.TextNonParty[I]);

  // Song List
//  Songs.LoadSongList; // moved to the UltraStar unit
  CatSongs.Refresh;

  //Set Length of Button Array one Time Instead of one time for every Song
  SetButtonLength(Length(CatSongs.Song));
  for Pet := 0 to High(CatSongs.Song) do begin // creating all buttons
    // new
    Texture.Limit := 512;// 256 0.4.2 value, 512 in 0.5.0

    if not FileExists(CatSongs.Song[Pet].Path + CatSongs.Song[Pet].Cover) then
      CatSongs.Song[Pet].Cover := ''; // 0.5.0: if cover not found then show 'no cover'

    if CatSongs.Song[Pet].Cover = '' then
      AddButton(300 + Pet*250, 140, 200, 200, Skin.GetTextureFileName('SongCover'), 'JPG', 'Plain', Theme.Song.Cover.Reflections)
    else begin
      // cache texture if there is a need to this
      if not Covers.CoverExists(CatSongs.Song[Pet].Path + CatSongs.Song[Pet].Cover) then begin
        Texture.CreateCacheMipmap := true;
        Texture.GetTexture(CatSongs.Song[Pet].Path + CatSongs.Song[Pet].Cover, 'Plain', true); // preloads textures and creates cache mipmap
        Texture.CreateCacheMipmap := false;

        // puts this texture to the cache file
        Covers.AddCover(CatSongs.Song[Pet].Path + CatSongs.Song[Pet].Cover);

        // unload full size texture
        Texture.UnloadTexture(CatSongs.Song[Pet].Path + CatSongs.Song[Pet].Cover, false);

        // we should also add mipmap texture by calling createtexture and use mipmap cache as data source
      end;

      // and now load it from cache file (small place for the optimization by eliminating reading it from file, but not here)
      AddButton(300 + Pet*250, 140, 200, 200, CatSongs.Song[Pet].Path + CatSongs.Song[Pet].Cover, 'JPG', 'Plain', Theme.Song.Cover.Reflections);
    end;
    Texture.Limit := 1024*1024;
  end;



  // Randomize Patch
  Randomize;
  //Equalizer
  SetLength(EqualizerBands, Theme.Song.Equalizer.Bands);
  //ClearArray
  For I := low(EqualizerBands) to high(EqualizerBands) do
    EqualizerBands[I] := 3;

  if (Length(CatSongs.Song) > 0) then
    Interaction := 0;
end;

procedure TScreenSong.SetScroll;
var
  VS, B: Integer;
begin
  VS := CatSongs.VisibleSongs;
  if VS > 0 then
  begin
    //Set Positions
    Case Theme.Song.Cover.Style of
      3: SetScroll3;
      5:begin
          if VS > 5 then
            SetScroll5
          else
            SetScroll4;
        end;
      else SetScroll4;
    end;
    //Set Visibility of Video Icon
    Static[VideoIcon].Visible := (CatSongs.Song[Interaction].Video <> '');

    //Set Texts:
    Text[TextArtist].Text := CatSongs.Song[Interaction].Artist;
    Text[TextTitle].Text  :=  CatSongs.Song[Interaction].Title;
    if (Ini.Tabs_at_startup = 1) And (CatSongs.CatNumShow = -1) then
    begin
      Text[TextNumber].Text := IntToStr(CatSongs.Song[Interaction].OrderNum) + '/' + IntToStr(CatSongs.CatCount);
      Text[TextTitle].Text  := '(' + IntToStr(CatSongs.Song[Interaction].CatNumber) + ' ' + Language.Translate('SING_SONGS_IN_CAT') + ')';
    end
    else if (CatSongs.CatNumShow = -2) then
      Text[TextNumber].Text := IntToStr(CatSongs.VisibleIndex(Interaction)+1) + '/' + IntToStr(VS)
    else if (CatSongs.CatNumShow = -3) then
      Text[TextNumber].Text := IntToStr(CatSongs.VisibleIndex(Interaction)+1) + '/' + IntToStr(VS)
    else if (Ini.Tabs_at_startup = 1) then
      Text[TextNumber].Text := IntToStr(CatSongs.Song[Interaction].CatNumber) + '/' + IntToStr(CatSongs.Song[Interaction - CatSongs.Song[Interaction].CatNumber].CatNumber)
    else
      Text[TextNumber].Text := IntToStr(Interaction+1) + '/' + IntToStr(Length(CatSongs.Song));
  end
  else
  begin
    Text[TextNumber].Text := '0/0';
    Text[TextArtist].Text := '';
    Text[TextTitle].Text  := '';
    for B := 0 to High(Button) do
      Button[B].Visible := False;

  end;
end;

procedure TScreenSong.SetScroll1;
var
  B:      integer;    // button
  BMin:   integer;    // button min
  BMax:   integer;    // button max
  Src:    integer;
//  Dst:    integer;
  Count:  integer;    // Dst is not used. Count is used.
  Ready:  boolean;

  VisCount: integer;  // count of visible (or selectable) buttons
  VisInt:   integer;  // visible position of interacted button
  Typ:      integer;  // 0 when all songs fits the screen
  Placed:   integer;  // number of placed visible buttons
begin
//  Src := 0;
//  Dst := -1;
  Count := 1;
  Typ := 0;
  Ready := false;
  Placed := 0;

  VisCount := 0;
  for B := 0 to High(Button) do
    if CatSongs.Song[B].Visible then Inc(VisCount);

  VisInt := 0;
  for B := 0 to Interaction-1 do
    if CatSongs.Song[B].Visible then Inc(VisInt);


  if VisCount <= 6 then begin
    Typ := 0;
  end else begin
    if VisInt <= 3 then begin
      Typ := 1;
      Count := 7;
      Ready := true;
    end;

    if (VisCount - VisInt) <= 3 then begin
      Typ := 2;
      Count := 7;
      Ready := true;
    end;

    if not Ready then begin
      Typ := 3;
      Src := Interaction;
    end;
  end;



  // hide all buttons
  for B := 0 to High(Button) do begin
    Button[B].Visible := false;
    Button[B].Selectable := CatSongs.Song[B].Visible;
  end;

{  for B := Src to Dst do begin
//    Button[B].Visible := true;
    Button[B].Visible := CatSongs.Song[B].Visible;
    Button[B].Selectable := Button[B].Visible;
    Button[B].Y := 140 + (B-Src) * 60;
  end;}


  if Typ = 0 then begin
    for B := 0 to High(Button) do begin
      if CatSongs.Song[B].Visible then begin
        Button[B].Visible := true;
        Button[B].Y := 140 + (Placed) * 60;
        Inc(Placed);
      end;
    end;
  end;

  if Typ = 1 then begin
    B := 0;
    while (Count > 0) do begin
      if CatSongs.Song[B].Visible then begin
        Button[B].Visible := true;
        Button[B].Y := 140 + (Placed) * 60;
        Inc(Placed);
        Dec(Count);
      end;
      Inc(B);
    end;
  end;

  if Typ = 2 then begin
    B := High(Button);
    while (Count > 0) do begin
      if CatSongs.Song[B].Visible then begin
        Button[B].Visible := true;
        Button[B].Y := 140 + (6-Placed) * 60;
        Inc(Placed);
        Dec(Count);
      end;
      Dec(B);
    end;
  end;

  if Typ = 3 then begin
    B := Src;
    Count := 4;
    while (Count > 0) do begin
      if CatSongs.Song[B].Visible then begin
        Button[B].Visible := true;
        Button[B].Y := 140 + (3+Placed) * 60;
        Inc(Placed);
        Dec(Count);
      end;
      Inc(B);
    end;

    B := Src-1;
    Placed := 0;
    Count := 3;
    while (Count > 0) do begin
      if CatSongs.Song[B].Visible then begin
        Button[B].Visible := true;
        Button[B].Y := 140 + (2-Placed) * 60;
        Inc(Placed);
        Dec(Count);
      end;
      Dec(B);
    end;

  end;

  if Length(Button) > 0 then
    Static[1].Texture.Y := Button[Interaction].Y - 5; // selection texture
end;

procedure TScreenSong.SetScroll2;
var
  B:      integer;
  Wsp:    integer; // wspolczynnik przesuniecia wzgledem srodka ekranu
  Wsp2:   real;
begin
  // liniowe
  for B := 0 to High(Button) do
    Button[B].X := 300 + (B - Interaction) * 260;

  if Length(Button) >= 3 then begin
    if Interaction = 0 then
      Button[High(Button)].X := 300 - 260;

    if Interaction = High(Button) then
      Button[0].X := 300 + 260;
  end;

  // kolowe
{  for B := 0 to High(Button) do begin
    Wsp := (B - Interaction); // 0 dla srodka, -1 dla lewego, +1 dla prawego itd.
    Wsp2 := Wsp / Length(Button);
    Button[B].X := 300 + 10000 * sin(2*pi*Wsp2);
//    Button[B].Y := 140 + 50 * ;
  end;}
end;

procedure TScreenSong.SetScroll3; // with slide
var
  B:      integer;
  Wsp:    integer; // wspolczynnik przesuniecia wzgledem srodka ekranu
  Wsp2:   real;
begin
  SongTarget := Interaction;

  // liniowe
  for B := 0 to High(Button) do
  begin
    Button[B].X := 300 + (B - SongCurrent) * 260;
    if (Button[B].X < -Button[B].W) OR (Button[B].X > 800) then
      Button[B].Visible := False
    else
      Button[B].Visible := True;
  end;

{  if Length(Button) >= 3 then begin
    if Interaction = 0 then
      Button[High(Button)].X := 300 - 260;

    if Interaction = High(Button) then
      Button[0].X := 300 + 260;
  end;}

  // kolowe
{  for B := 0 to High(Button) do begin
    Wsp := (B - Interaction); // 0 dla srodka, -1 dla lewego, +1 dla prawego itd.
    Wsp2 := Wsp / Length(Button);
    Button[B].X := 300 + 10000 * sin(2*pi*Wsp2);
//    Button[B].Y := 140 + 50 * ;
  end;}
end;

procedure TScreenSong.SetScroll4; // rotate
var
  B:      integer;
  Wsp:    real;
  Z, Z2:      real;
  VS:     integer;
begin
  VS := CatSongs.VisibleSongs; // 0.5.0 (I): cached, very important

  // kolowe
  for B := 0 to High(Button) do begin
    Button[B].Visible := CatSongs.Song[B].Visible; // nowe
    if Button[B].Visible then begin // 0.5.0 optimization for 1000 songs - updates only visible songs, hiding in tabs becomes useful for maintaing good speed

    Wsp := 2 * pi * (CatSongs.VisibleIndex(B) - SongCurrent) /  VS {CatSongs.VisibleSongs};// 0.5.0 (II): takes another 16ms

    Z := (1 + cos(Wsp)) / 2;
    Z2 := (1 + 2*Z) / 3;


    Button[B].X := Theme.Song.Cover.X + (0.185 * Theme.Song.Cover.H * VS * sin(Wsp)) * Z2 - ((Button[B].H - Theme.Song.Cover.H)/2); // 0.5.0 (I): 2 times faster by not calling CatSongs.VisibleSongs
    Button[B].Z := Z / 2 + 0.3;

    Button[B].W := Theme.Song.Cover.H * Z2;

//    Button[B].Y := {50 +} 140 + 50 - 50 * Z2;
    Button[B].Y := Theme.Song.Cover.Y  + (Theme.Song.Cover.H - Abs(Button[B].H)) * 0.7 ;
    Button[B].H := Button[B].W;
    end;
  end;
end;

{procedure TScreenSong.SetScroll4; // rotate
var
  B:      integer;
  Wsp:    real;
  Z:      real;
  Z2, Z3:     real;
  VS:     integer;
  function modreal (const X, Y: real):real;
  begin
    Result := Frac(x / y) * y;
    if Result < -3 then
      Result := Result + Y
    else if Result > 3 then
      Result := Result - Y;
  end;
begin
  VS := CatSongs.VisibleSongs; // 0.5.0 (I): cached, very important
  Z3 := 1;
  if VS < 12 then
    Z2 := VS
  else
    Z2 := 12;

  // kolowe
  for B := 0 to High(Button) do begin
    Button[B].Visible := CatSongs.Song[B].Visible; // nowe
    if Button[B].Visible then begin // 0.5.0 optimization for 1000 songs - updates only visible songs, hiding in tabs becomes useful for maintaing good speed
      if ((ModReal(CatSongs.VisibleIndex(B) - SongCurrent, VS)>-3) AND (ModReal(CatSongs.VisibleIndex(B) - SongCurrent, VS)<3)) then
      begin
        if CatSongs.VisibleIndex(B)> SongCurrent then
          Wsp := 2 * pi * (CatSongs.VisibleIndex(B) - SongCurrent) /  Z2
        else
          Wsp := 2 * pi * (CatSongs.VisibleIndex(B) - SongCurrent) /  Z2;

        Z3 := 2;
        Z := (1 + cos(Wsp)) / 2;
        //Z2 := (1 + 2*Z) / 3;
        //Z2 := (0.5 + Z/2);
        //Z2 := sin(Wsp);

        //Z2 := Power (Z2,Z3);

        Button[B].W := Theme.Song.CoverW * Power(cos(Wsp), Z3);//Power(Z2, 3);

        //Button[B].X := Theme.Song.CoverX + ({Theme.Song.CoverX + Theme.Song.CoverW/2 + Theme.Song.CoverW*0.18 * VS {CatSongs.VisibleSongs {Length(Button) * sin(Wsp) {- Theme.Song.CoverX - Theme.Song.CoverW) * Z2; // 0.5.0 (I): 2 times faster by not calling CatSongs.VisibleSongs
        if (sin(Wsp)<0) then
          Button[B].X := sin(Wsp)*Theme.Song.CoverX*Theme.Song.CoverW*0.007 + Theme.Song.CoverX + Theme.Song.CoverW - Button[B].W
        else //*Theme.Song.CoverW*0.004*Z3
          Button[B].X := sin(Wsp)*Theme.Song.CoverX*Theme.Song.CoverW*0.007 + Theme.Song.CoverX;
        Button[B].Z := Z-0.00001;

//      Button[B].Y := {50 + 140 + 50 - 50 * Z2;
    //  Button[B].Y := (Theme.Song.CoverY  + 40 + 50 - 50 * Z2);
        Button[B].Y := (Theme.Song.CoverY  + Theme.Song.CoverW - Button[B].W);
        Button[B].H := Button[B].W;
        Button[B].Visible := True;
      end
      {else if (((CatSongs.VisibleIndex(B) - SongCurrent)>-3) AND ((CatSongs.VisibleIndex(B) - SongCurrent)<3)) OR ((round (CatSongs.VisibleIndex(B) - SongCurrent) mod VS > -3) AND ((CatSongs.VisibleIndex(B) - SongCurrent)<3)) then
      begin
        Wsp := 2 * pi * (CatSongs.VisibleIndex(B) - SongCurrent) /  12 ;// 0.5.0 (II): takes another 16ms

        Z := (1 + cos(Wsp)) / 2 -0.00001; //z < 0.49999 is behind the cover 1 is in front of the covers

        Button[B].W := Theme.Song.CoverW * Power(cos(Wsp), Z3);//Power(Z2, 3);

        if (sin(Wsp)<0) then
          Button[B].X := sin(Wsp)*Theme.Song.CoverX*Theme.Song.CoverW*0.007 + Theme.Song.CoverX + Theme.Song.CoverW - Button[B].W
        else
          Button[B].X := sin(Wsp)*Theme.Song.CoverX*Theme.Song.CoverW*0.007 + Theme.Song.CoverX;

        Button[B].Z := Z;

        Button[B].Y := (Theme.Song.CoverY  + Theme.Song.CoverW - Button[B].W);

        Button[B].H := Button[B].W;
        Button[B].Visible := True;
      end
      else Button[B].Visible := False;
    end;
  end;
end;      }

procedure TScreenSong.SetScroll5; // rotate
var
  B:      integer;
  Angle:    real;
  Pos:    Real;
  VS:     integer;
  diff:     real;
  X:        Real;
begin
  VS := CatSongs.VisibleSongs; // cache Visible Songs
  {Vars
  Theme.Song.CoverW: Radius des Kreises
  Theme.Song.CoverX: X Pos Linke Kante des gewählten Covers
  Theme.Song.CoverX: Y Pos Obere Kante des gewählten Covers
  Theme.Song.CoverH: Höhe der Cover

  (CatSongs.VisibleIndex(B) - SongCurrent)/VS = Abstand zum MIttleren Cover in %
  }

  //Change Pos of all Buttons
  for B := low(Button) to high(Button) do
  begin
    Button[B].Visible := CatSongs.Song[B].Visible; //Adjust Visability
    if Button[B].Visible then //Only Change Pos for Visible Buttons
    begin
      Pos := (CatSongs.VisibleIndex(B) - SongCurrent);
      if (Pos < -VS/2) then
        Pos := Pos + VS
      else if (Pos > VS/2) then
        Pos := Pos - VS;

      if (Abs(Pos) < 2.5) then {fixed Positions}
      begin
      Angle := Pi * (Pos / 5);
      //Button[B].Visible := False;

      Button[B].H := Abs(Theme.Song.Cover.H * cos(Angle*0.8));//Power(Z2, 3);

//      Button[B].Reflectionspacing := 15 * Button[B].H/Theme.Song.Cover.H;
      Button[B].DeSelectReflectionspacing := 15 * Button[B].H/Theme.Song.Cover.H;

      Button[B].Z := 0.95 - Abs(Pos) * 0.01;

      Button[B].Y := (Theme.Song.Cover.Y  + (Theme.Song.Cover.H - Abs(Theme.Song.Cover.H * cos(Angle))) * 0.5);

      Button[B].W := Button[B].H;

      Diff := (Button[B].H - Theme.Song.Cover.H)/2;


      X := Sin(Angle*1.3)*0.9;

      Button[B].X := Theme.Song.Cover.X + Theme.Song.Cover.W * X - Diff;

      end
      else
      begin {Behind the Front Covers}
//      Button[B].Visible := False;
//        if VS/2-abs(Pos)>VS*0.4 then Button[B].Visible := False;

        if Pos < 0 then
          Pos := (Pos - VS/2) /VS
        else
          Pos := (Pos + VS/2) /VS;

        Angle := pi * Pos*2;

        Button[B].Z := (0.4 - Abs(Pos/4)) -0.00001; //z < 0.49999 is behind the cover 1 is in front of the covers

        Button[B].H :=0.6*(Theme.Song.Cover.H-Abs(Theme.Song.Cover.H * cos(Angle/2)*0.8));//Power(Z2, 3);

        Button[B].W := Button[B].H;

        Button[B].Y := Theme.Song.Cover.Y  - (Button[B].H - Theme.Song.Cover.H)*0.75;

//        Button[B].Reflectionspacing := 15 * Button[B].H/Theme.Song.Cover.H;
        Button[B].DeSelectReflectionspacing := 15 * Button[B].H/Theme.Song.Cover.H;

        Diff := (Button[B].H - Theme.Song.Cover.H)/2;

        Button[B].X :=  Theme.Song.Cover.X+Theme.Song.Cover.H/2-Button[b].H/2 + (Theme.Song.Cover.H)*sin(Angle/2)*1.52;

      end;

      //Button[B].Y := (Theme.Song.Cover.Y  + (Theme.Song.Cover.H - Button[B].H)/1.5); //Cover at down border of the change field
//      Button[B].Y := (Theme.Song.Cover.Y  + (Theme.Song.Cover.H - Button[B].H) * 0.7);

    end;
  end;
end;

procedure TScreenSong.onShow;
begin
  Music.Stop;

  if Ini.Players <= 3 then PlayersPlay := Ini.Players + 1;
  if Ini.Players = 4 then PlayersPlay := 6;

  //Cat Mod etc
    if (Ini.Tabs_at_startup = 1) AND (CatSongs.CatNumShow = -1) then
    begin
      CatSongs.ShowCategoryList;
      FixSelected;
      //Show Cat in Top Left Mod
      HideCatTL;
    end;


  if Length(CatSongs.Song) > 0 then begin
    Music.SetLoop(false);
    Music.Open(CatSongs.Song[Interaction].Path + CatSongs.Song[Interaction].Mp3);
    Music.MoveTo(Music.Length / 4);
    Music.Play;
    SetScroll;
    UpdateLCD;
  end;

  //Playlist Mode
  if (Mode = 0) then
  begin
    //If Playlist Shown -> Select Next automatically
    if (CatSongs.CatNumShow = -3) then
    begin
      SelectNext;
      ChangeMusic;
    end;
  end
  //Party Mode
  else if (Mode = 1) then
  begin

    SelectRandomSong;
    //Show Menu directly in PartyMode
    //But only if selected in Options
    if (Ini.PartyPopup = 1) then
    begin
      ScreenSongMenu.MenuShow(SM_Party_Main);
    end;


  end;

  SetJoker;
  SetStatics;
end;

procedure TScreenSong.onHide;
begin
  //When hide then Stop Music (For Party Mode Popup on Exit)
  if (Display.NextScreen <> @ScreenSing) and (Display.NextScreen <> @ScreenSingModi) and (Music <> nil) then
    Music.Stop;
end;

procedure TScreenSong.DrawExtensions;
begin
  //Draw Song Menu
  if (ScreenSongMenu.Visible) then
  begin
    ScreenSongMenu.Draw;
  end
  else if (ScreenSongJumpto.Visible) then
  begin
    ScreenSongJumpto.Draw;
  end
end;

function TScreenSong.Draw: boolean;
var
  dx:   real;
  dt:   real;
begin
  dx := SongTarget-SongCurrent;
  dt := TimeSkip*7;
  if dt > 1 then dt := 1;
  SongCurrent := SongCurrent + dx*dt;

{  if SongCurrent > Catsongs.VisibleSongs then begin
    SongCurrent := SongCurrent - Catsongs.VisibleSongs;
    SongTarget := SongTarget - Catsongs.VisibleSongs;
  end;}

//  Log.BenchmarkStart(5);
  SetScroll;
//  Log.BenchmarkEnd(5);
//  Log.LogBenchmark('SetScroll4', 5);


  // 0.5.0: cover fade
  if (CoverTime < 1) and (CoverTime + TimeSkip >= 1) then begin
    // load new texture
    Texture.GetTexture(Button[Interaction].Texture.Name, 'Plain', false);
    Button[Interaction].Texture.Alpha := 1;
    Button[Interaction].Texture2 := Texture.GetTexture(Button[Interaction].Texture.Name, 'Plain', false);
    Button[Interaction].Texture2.Alpha := 1;
  end;
  CoverTime := CoverTime + TimeSkip;
  Button[Interaction].Texture2.Alpha := (CoverTime - 1) * 1.5;
  if Button[Interaction].Texture2.Alpha > 1 then Button[Interaction].Texture2.Alpha := 1;

  inherited Draw;


  //Draw Equalizer
  if Theme.Song.Equalizer.Visible then
    DrawEqualizer;

  DrawExtensions;
end;

procedure TScreenSong.SelectNext;
var
  Skip:   integer;
  I:      integer;
  VS:     Integer;
begin
  VS := CatSongs.VisibleSongs;

  if VS > 0 then
  begin
    CoverTime := 0;
    Button[Interaction].Texture := Texture.GetTexture(Button[Interaction].Texture.Name, 'Plain', true); // 0.5.0: show cached texture
    Button[Interaction].Texture2.Alpha := 0;

    //0.5.0: unload old full size texture
    if Button[Interaction].Texture.Name <> Skin.GetTextureFileName('SongCover') then
      Texture.UnloadTexture(Button[Interaction].Texture.Name, false);

    Skip := 1;

    // this 1 could be changed by CatSongs.FindNextVisible
    while (not CatSongs.Song[(Interaction + Skip) mod Length(Interactions)].Visible) do Inc(Skip);

    SongTarget := SongTarget + 1;//Skip;

    Interaction := (Interaction + Skip) mod Length(Interactions);

    // try to keep all at the beginning
    if SongTarget > VS-1 then begin
      SongTarget := SongTarget - VS;
      SongCurrent := SongCurrent - VS;
    end;

  end;
      // Interaction -> Button, ktorego okladke przeczytamy
      //  Button[Interaction].Texture := Texture.GetTexture(Button[Interaction].Texture.Name, 'Plain', false); // 0.5.0: show uncached texture
end;

procedure TScreenSong.SelectPrev;
var
  Skip:   integer;
  I:      integer;
  VS:     Integer;
begin
  VS := CatSongs.VisibleSongs;

  if VS > 0 then
  begin
    CoverTime := 0;
    Button[Interaction].Texture := Texture.GetTexture(Button[Interaction].Texture.Name, 'Plain', true); // 0.5.0: show cached texture
    Button[Interaction].Texture2.Alpha := 0;

    //0.5.0: unload old full size texture
    if Button[Interaction].Texture.Name <> Skin.GetTextureFileName('SongCover') then
      Texture.UnloadTexture(Button[Interaction].Texture.Name, false);

    Skip := 1;

    while (not CatSongs.Song[(Interaction - Skip + Length(Interactions)) mod Length(Interactions)].Visible) do Inc(Skip);
    SongTarget := SongTarget - 1;//Skip;

    Interaction := (Interaction - Skip + Length(Interactions)) mod Length(Interactions);

    // try to keep all at the beginning
    if SongTarget < 0 then begin
      SongTarget := SongTarget + CatSongs.VisibleSongs;
      SongCurrent := SongCurrent + CatSongs.VisibleSongs;
    end;

  //  Button[Interaction].Texture := Texture.GetTexture(Button[Interaction].Texture.Name, 'Plain', false); // 0.5.0: show uncached texture
  end;
end;

procedure TScreenSong.UpdateLCD;
begin
  LCD.HideCursor;
  LCD.Clear;
  LCD.WriteText(1, Text[TextArtist].Text);
  LCD.WriteText(2, Text[TextTitle].Text);
end;

//Procedure Change current played Preview
procedure TScreenSong.ChangeMusic;
begin
  if (NOT CatSongs.Song[Interaction].Main) AND(CatSongs.VisibleSongs > 0) then
  begin
    Music.Close;
    if Music.Open(CatSongs.Song[Interaction].Path + CatSongs.Song[Interaction].Mp3) then begin
      Music.MoveTo(Music.Length / 4);
      Music.Play;
    end;
  end
  else
    Music.Stop;
end;

procedure TScreenSong.SkipTo(Target: Cardinal); // 0.5.0
var
  Skip:   integer;
  I:      integer;
begin
  CoverTime := 0;
  Button[Interaction].Texture := Texture.GetTexture(Button[Interaction].Texture.Name, 'Plain', true); // 0.5.0: show cached texture
  Button[Interaction].Texture2.Alpha := 0;

  if Button[Interaction].Texture.Name <> Skin.GetTextureFileName('SongCover') then
    Texture.UnloadTexture(Button[Interaction].Texture.Name, false);

  Interaction := High(CatSongs.Song);
  SongTarget := 0;

  for I := 1 to Target+1 do
    SelectNext;

  FixSelected2;
end;

procedure TScreenSong.DrawEqualizer;
var
  Data: TFFTData; //Audio Data
  I, J: Integer;
  Res: byte;
  A, B: Integer;
  PosX, PosY: Integer;
  Pos: Real;
begin
if (not Music.Finished) AND (Theme.Song.Equalizer.Length > 0) then
begin


  A := GetTickCount div 44;

  if (A <> EqualizerTime) then
  begin
    EqualizerTime := A;
    Data := Music.GetFFTData;

    B:=0;
    Pos := 0;
    Res := floor(92/Theme.Song.Equalizer.Bands);//How much channels are used for one Band
    //Change Lengths
    for I := 0 to 92 do
    begin
      A := floor(I/Res);

      if (A<>B) then //Band changed
      begin
        if (Pos <= Theme.Song.Equalizer.Length) then
        begin
          if ((Pos < EqualizerBands[B]) AND (EqualizerBands[B]>1)) then
            EqualizerBands[B] := EqualizerBands[B] - 1
          else
            EqualizerBands[B] := floor(Pos);
        end
        else
          EqualizerBands[B] := 1;

        B := A;
        Pos := 0;
      end;

      if I > 35 then
        Data[i] := Data[i] * 8
      else if I > 11 then
        Data[i] := Data[i] * 4.5
      else
        Data[i] := Data[i] * 1.1;

      if (Data[i] >= 1) then
        Data[i] := 0.9999999999999;

      if Data[i]*Theme.Song.Equalizer.Length > Pos then
        Pos := Data[i]*Theme.Song.Equalizer.Length;
    end;

    //Change Last Band
    if (EqualizerBands[B] <= Theme.Song.Equalizer.Length) then
    begin
      if ((Pos < EqualizerBands[B]) AND (EqualizerBands[B]>1)) then
        EqualizerBands[B] := EqualizerBands[B] - 1
      else
        EqualizerBands[B] := floor(Pos)
    end
    else
      EqualizerBands[B] := 1;
  end;

  //Draw every Channel
  glColor4f(Theme.Song.Equalizer.ColR, Theme.Song.Equalizer.ColG, Theme.Song.Equalizer.ColB, Theme.Song.Equalizer.Alpha); //Set Color
  glDisable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);

  PosY := Theme.Song.Equalizer.Y;
  PosX := Theme.Song.Equalizer.X;

  For I := 0 to Theme.Song.Equalizer.Bands do
  begin
    if Theme.Song.Equalizer.Direction then
      PosY := Theme.Song.Equalizer.Y //+ (Theme.Song.Equalizer.H + Theme.Song.Equalizer.Space) * Theme.Song.Equalizer.Length
    else
      PosX := Theme.Song.Equalizer.X;
    //Draw for every visible quad
    for J := 1 to EqualizerBands[I] do
    begin
      glBegin(GL_QUADS);
        glVertex3f(PosX, PosY, Theme.Song.Equalizer.Z);
        glVertex3f(PosX, PosY+Theme.Song.Equalizer.H, Theme.Song.Equalizer.Z);
        glVertex3f(PosX+Theme.Song.Equalizer.W, PosY+Theme.Song.Equalizer.H, Theme.Song.Equalizer.Z);
        glVertex3f(PosX+Theme.Song.Equalizer.W, PosY, Theme.Song.Equalizer.Z);
      glEnd;

      if Theme.Song.Equalizer.Direction then //Vertically
        PosY := PosY - Theme.Song.Equalizer.H - Theme.Song.Equalizer.Space
      else //Horizontally
        PosX := PosX + Theme.Song.Equalizer.W + Theme.Song.Equalizer.Space;
    end;
    if Theme.Song.Equalizer.Direction then //Horizontally
      PosX := PosX + Theme.Song.Equalizer.W + Theme.Song.Equalizer.Space
    else //Vertically
      PosY := PosY + Theme.Song.Equalizer.H + Theme.Song.Equalizer.Space;
  end;
end;
end;

Procedure TScreenSong.SelectRandomSong;
var
  I, I2: Integer;
begin
  Case PlaylistMan.Mode of
      0:  //All Songs Just Select Random Song
        begin
          //When Tabs are activated then use Tab Method
          if (Ini.Tabs_at_startup = 1) then
          begin
            repeat
              I2 := Random(high(CatSongs.Song)+1) - low(CatSongs.Song)+1;
            until CatSongs.Song[I2].Main = false;

            //Search Cat
            for I := I2 downto low(CatSongs.Song) do
            begin
              if CatSongs.Song[I].Main then
                break;
            end;
            //In I ist jetzt die Kategorie in I2 der Song
            //I is the CatNum, I2 is the No of the Song within this Cat

            //Choose Cat
            CatSongs.ShowCategoryList;

            //Show Cat in Top Left Mod
            ShowCatTL (I);

            CatSongs.ClickCategoryButton(I);
            SelectNext;

            //Choose Song
            SkipTo(I2-I);
          end
          //When Tabs are deactivated use easy Method
          else
            SkipTo(Random(CatSongs.VisibleSongs));
        end;
      1:  //One Category Select Category and Select Random Song
        begin
          CatSongs.ShowCategoryList;
          CatSongs.ClickCategoryButton(PlaylistMan.CurPlayList);
          ShowCatTL(PlaylistMan.CurPlayList);

          SelectNext;
          FixSelected2;

          SkipTo(Random(CatSongs.VisibleSongs));
        end;
      2:  //Playlist: Select Playlist and Select Random Song
        begin
          PlaylistMan.SetPlayList(PlaylistMan.CurPlayList);

          SkipTo(Random(CatSongs.VisibleSongs));
          FixSelected2;
        end;
  end;

  Music.PlayChange;
  ChangeMusic;
  SetScroll;
  UpdateLCD;
end;

procedure TScreenSong.SetJoker;
begin
  //If Party Mode
  if Mode = 1 then //Show Joker that are available
  begin
    if (PartySession.Teams.NumTeams >= 1) then
    begin
      Static[StaticTeam1Joker1].Visible := (PartySession.Teams.Teaminfo[0].Joker >= 1);
      Static[StaticTeam1Joker2].Visible := (PartySession.Teams.Teaminfo[0].Joker >= 2);
      Static[StaticTeam1Joker3].Visible := (PartySession.Teams.Teaminfo[0].Joker >= 3);
      Static[StaticTeam1Joker4].Visible := (PartySession.Teams.Teaminfo[0].Joker >= 4);
      Static[StaticTeam1Joker5].Visible := (PartySession.Teams.Teaminfo[0].Joker >= 5);
    end
    else
    begin
      Static[StaticTeam1Joker1].Visible := False;
      Static[StaticTeam1Joker2].Visible := False;
      Static[StaticTeam1Joker3].Visible := False;
      Static[StaticTeam1Joker4].Visible := False;
      Static[StaticTeam1Joker5].Visible := False;
    end;

    if (PartySession.Teams.NumTeams >= 2) then
    begin
      Static[StaticTeam2Joker1].Visible := (PartySession.Teams.Teaminfo[1].Joker >= 1);
      Static[StaticTeam2Joker2].Visible := (PartySession.Teams.Teaminfo[1].Joker >= 2);
      Static[StaticTeam2Joker3].Visible := (PartySession.Teams.Teaminfo[1].Joker >= 3);
      Static[StaticTeam2Joker4].Visible := (PartySession.Teams.Teaminfo[1].Joker >= 4);
      Static[StaticTeam2Joker5].Visible := (PartySession.Teams.Teaminfo[1].Joker >= 5);
    end
    else
    begin
      Static[StaticTeam2Joker1].Visible := False;
      Static[StaticTeam2Joker2].Visible := False;
      Static[StaticTeam2Joker3].Visible := False;
      Static[StaticTeam2Joker4].Visible := False;
      Static[StaticTeam2Joker5].Visible := False;
    end;

    if (PartySession.Teams.NumTeams >= 3) then
    begin
      Static[StaticTeam3Joker1].Visible := (PartySession.Teams.Teaminfo[2].Joker >= 1);
      Static[StaticTeam3Joker2].Visible := (PartySession.Teams.Teaminfo[2].Joker >= 2);
      Static[StaticTeam3Joker3].Visible := (PartySession.Teams.Teaminfo[2].Joker >= 3);
      Static[StaticTeam3Joker4].Visible := (PartySession.Teams.Teaminfo[2].Joker >= 4);
      Static[StaticTeam3Joker5].Visible := (PartySession.Teams.Teaminfo[2].Joker >= 5);
    end
    else
    begin
      Static[StaticTeam3Joker1].Visible := False;
      Static[StaticTeam3Joker2].Visible := False;
      Static[StaticTeam3Joker3].Visible := False;
      Static[StaticTeam3Joker4].Visible := False;
      Static[StaticTeam3Joker5].Visible := False;
    end;
  end
  else
  begin //Hide all
    Static[StaticTeam1Joker1].Visible := False;
    Static[StaticTeam1Joker2].Visible := False;
    Static[StaticTeam1Joker3].Visible := False;
    Static[StaticTeam1Joker4].Visible := False;
    Static[StaticTeam1Joker5].Visible := False;

    Static[StaticTeam2Joker1].Visible := False;
    Static[StaticTeam2Joker2].Visible := False;
    Static[StaticTeam2Joker3].Visible := False;
    Static[StaticTeam2Joker4].Visible := False;
    Static[StaticTeam2Joker5].Visible := False;

    Static[StaticTeam3Joker1].Visible := False;
    Static[StaticTeam3Joker2].Visible := False;
    Static[StaticTeam3Joker3].Visible := False;
    Static[StaticTeam3Joker4].Visible := False;
    Static[StaticTeam3Joker5].Visible := False;
  end;
end;

procedure TScreenSong.SetStatics;
var
  I:       Integer;
  Visible: Boolean;
begin
  //Set Visibility of Party Statics and Text
  Visible := (Mode = 1);

  For I := 0 to high(StaticParty) do
    Static[StaticParty[I]].Visible := Visible;

  For I := 0 to high(TextParty) do
    Text[TextParty[I]].Visible := Visible;

  //Set Visibility of Non Party Statics and Text
  Visible := not Visible;

  For I := 0 to high(StaticNonParty) do
    Static[StaticNonParty[I]].Visible := Visible;

  For I := 0 to high(TextNonParty) do
    Text[TextNonParty[I]].Visible := Visible;
end;

//Procedures for Menu

procedure TScreenSong.StartSong;
begin
  CatSongs.Selected := Interaction;
  Music.Stop;
  //Party Mode
  if (Mode = 1) then
  begin
    FadeTo(@ScreenSingModi);
  end
  else
  begin
    FadeTo(@ScreenSing);
  end;
end;

procedure TScreenSong.SelectPlayers;
begin
  CatSongs.Selected := Interaction;
  Music.Stop;

  ScreenName.Goto_SingScreen := True;
  FadeTo(@ScreenName);
end;

procedure TScreenSong.OpenEditor;
begin
  if (Length(Songs.Song) > 0) and (not CatSongs.Song[Interaction].Main) AND (Mode = 0) then begin
    Music.Stop;
    Music.PlayStart;
    ScreenEditSub.Path := CatSongs.Song[Interaction].Path;
    ScreenEditSub.FileName := CatSongs.Song[Interaction].FileName;
    FadeTo(@ScreenEditSub);
  end;
end;

//Team No of Team (0-5)
procedure TScreenSong.DoJoker (Team: Byte);
begin
  if (Mode = 1) AND (PartySession.Teams.NumTeams >= Team + 1) AND (PartySession.Teams.Teaminfo[Team].Joker > 0) then
  begin
    //Joker spielen
    Dec(PartySession.Teams.Teaminfo[Team].Joker);
    SelectRandomSong;
    SetJoker;
  end;
end;

procedure TScreenSong.Refresh;
begin {
CatSongs.Refresh;
CatSongs.ShowCategoryList;
Interaction := 0;
SelectNext;
FixSelected; }

end;

end.