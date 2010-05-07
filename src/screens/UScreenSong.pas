{* UltraStar Deluxe - Karaoke Game
 *
 * UltraStar Deluxe is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *}

unit UScreenSong;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  SDL,
  UCommon,
  UDisplay,
  UPath,
  UFiles,
  UIni,
  ULanguage,
  ULog,
  UMenu,
  UMenuEqualizer,
  UMusic,
  USong,
  USongs,
  UTexture,
  UThemes,
  UTime;

type
  TScreenSong = class(TMenu)
    private
      Equalizer: Tms_Equalizer;

      PreviewOpened: Integer; // interaction of the Song that is loaded for preview music
                              // -1 if nothing is opened

      isScrolling: boolean;   // true if song flow is about to move

      procedure StartMusicPreview();
      procedure StopMusicPreview();
    public
      TextArtist:   integer;
      TextTitle:    integer;
      TextNumber:   integer;

      //Video Icon Mod
      VideoIcon: cardinal;

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

      //Party Mod
      Mode: TSingMode;

      //party Statics (Joker)
      StaticTeam1Joker1: cardinal;
      StaticTeam1Joker2: cardinal;
      StaticTeam1Joker3: cardinal;
      StaticTeam1Joker4: cardinal;
      StaticTeam1Joker5: cardinal;

      StaticTeam2Joker1: cardinal;
      StaticTeam2Joker2: cardinal;
      StaticTeam2Joker3: cardinal;
      StaticTeam2Joker4: cardinal;
      StaticTeam2Joker5: cardinal;

      StaticTeam3Joker1: cardinal;
      StaticTeam3Joker2: cardinal;
      StaticTeam3Joker3: cardinal;
      StaticTeam3Joker4: cardinal;
      StaticTeam3Joker5: cardinal;

      StaticParty:    array of cardinal;
      TextParty:      array of cardinal;
      StaticNonParty: array of cardinal;
      TextNonParty:   array of cardinal;

      constructor Create; override;
      procedure SetScroll;
      //procedure SetScroll1;
      //procedure SetScroll2;
      procedure SetScroll3;
      procedure SetScroll4;
      procedure SetScroll5;
      procedure SetScroll6;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      function ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean; override;
      function Draw: boolean; override;
      procedure GenerateThumbnails();
      procedure OnShow; override;
      procedure OnHide; override;
      procedure SelectNext;
      procedure SelectPrev;
      procedure SkipTo(Target: cardinal);
      procedure FixSelected; //Show Wrong Song when Tabs on Fix
      procedure FixSelected2; //Show Wrong Song when Tabs on Fix
      procedure ShowCatTL(Cat: integer);// Show Cat in Top left
      procedure ShowCatTLCustom(Caption: UTF8String);// Show Custom Text in Top left
      procedure HideCatTL;// Show Cat in Tob left
      procedure Refresh; //Refresh Song Sorting
      procedure ChangeMusic;
      //Party Mode
      procedure SelectRandomSong;
      procedure SetJoker;
      procedure SetStatics;
      //procedures for Menu
      procedure StartSong;
      procedure OpenEditor;
      procedure DoJoker(Team: integer);
      procedure SelectPlayers;

      procedure OnSongSelect;   // called when song flows movement stops at a song
      procedure OnSongDeSelect; // called before current song is deselected

      procedure UnloadDetailedCover;

      //Extensions
      procedure DrawExtensions;
  end;

implementation

uses
  Math,
  gl,
  UCovers,
  UGraphic,
  UMain,
  UMenuButton,
  UNote,
  UParty,
  UPlaylist,
  UScreenSongMenu,
  USkins,
  UUnicodeUtils;

// ***** Public methods ****** //

//Show Wrong Song when Tabs on Fix
procedure TScreenSong.FixSelected;
var
  I, I2: integer;
begin
  if CatSongs.VisibleSongs > 0 then
  begin
    I2:= 0;
    for I := Low(CatSongs.Song) to High(Catsongs.Song) do
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
var
  I, I2: integer;
begin
  if CatSongs.VisibleSongs > 0 then
  begin
    I2:= 0;
    for I := Low(CatSongs.Song) to High(Catsongs.Song) do
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

procedure TScreenSong.ShowCatTLCustom(Caption: UTF8String);// Show Custom Text in Top left
begin
  Text[TextCat].Text := Caption;
  Text[TextCat].Visible := true;
  Statics[StaticCat].Visible := false;
end;

//Show Cat in Top Left Mod
procedure TScreenSong.ShowCatTL(Cat: integer);
begin
  //Change
  Text[TextCat].Text := CatSongs.Song[Cat].Artist;
  //Statics[StaticCat].Texture := Texture.GetTexture(Button[Cat].Texture.Name, TEXTURE_TYPE_PLAIN, true);

  //Show
  Text[TextCat].Visible := true;
  Statics[StaticCat].Visible := true;
end;

procedure TScreenSong.HideCatTL;
begin
  //Hide
  //Text[TextCat].Visible := false;
  Statics[StaticCat].Visible := false;
  //New -> Show Text specified in Theme
  Text[TextCat].Visible := true;
  Text[TextCat].Text := Theme.Song.TextCat.Text;
end;
//Show Cat in Top Left Mod End

// Method for input parsing. If false is returned, GetNextWindow
// should be checked to know the next window to load;
function TScreenSong.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  I:      integer;
  I2:     integer;
  SDL_ModState:  word;
  UpperLetter: UCS4Char;
  TempStr: UTF8String;
begin
  Result := true;

  //Song Screen Extensions (Jumpto + Menu)
  if (ScreenSongMenu.Visible) then
  begin
    Result := ScreenSongMenu.ParseInput(PressedKey, CharCode, PressedDown);
    Exit;
  end
  else if (ScreenSongJumpto.Visible) then
  begin
    Result := ScreenSongJumpto.ParseInput(PressedKey, CharCode, PressedDown);
    Exit;
  end;

  if (PressedDown) then
  begin // Key Down

    SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);

    //Jump to Artist/Titel
    if ((SDL_ModState and KMOD_LALT <> 0) and (Mode = smNormal)) then
    begin
      UpperLetter := UCS4UpperCase(CharCode);

      if (UpperLetter in ([Ord('A')..Ord('Z'), Ord('0') .. Ord('9')]) ) then
      begin
        I2 := Length(CatSongs.Song);

        //Jump To Titel
        if (SDL_ModState = (KMOD_LALT or KMOD_LSHIFT)) then
        begin
          for I := 1 to High(CatSongs.Song) do
          begin
            if (CatSongs.Song[(I + Interaction) mod I2].Visible) then
            begin
              TempStr := CatSongs.Song[(I + Interaction) mod I2].Title;
              if (Length(TempStr) > 0) and
                 (UCS4UpperCase(UTF8ToUCS4String(TempStr)[0]) = UpperLetter) then
              begin
                SkipTo(CatSongs.VisibleIndex((I + Interaction) mod I2));

                AudioPlayback.PlaySound(SoundLib.Change);

                SetScroll4;
                //Break and Exit
                Exit;
              end;
            end;
          end;
        end
        //Jump to Artist
        else if (SDL_ModState = KMOD_LALT) then
        begin
          for I := 1 to High(CatSongs.Song) do
          begin
            if (CatSongs.Song[(I + Interaction) mod I2].Visible) then
            begin
              TempStr := CatSongs.Song[(I + Interaction) mod I2].Artist;
              if (Length(TempStr) > 0) and
                 (UCS4UpperCase(UTF8ToUCS4String(TempStr)[0]) = UpperLetter) then
              begin
                SkipTo(CatSongs.VisibleIndex((I + Interaction) mod I2));

                AudioPlayback.PlaySound(SoundLib.Change);

                SetScroll4;

                //Break and Exit
                Exit;
              end;
            end;
          end;
        end;
      end;

      Exit;
    end;

    // **********************
    // * workaround for LCTRL+R: it should be changed when we have a solution for the
    // * CTRL+'A'..'Z' problem
    if (SDL_ModState = KMOD_LCTRL) and (PressedKey = SDLK_R) then
      CharCode := UCS4Char('R');
    // **********************

    // check normal keys
    case UCS4UpperCase(CharCode) of
      Ord('Q'):
        begin
          Result := false;
          Exit;
        end;

      Ord('M'): //Show SongMenu
        begin
          if (Songs.SongList.Count > 0) then
          begin
            if (Mode = smNormal) then
            begin
              if (not CatSongs.Song[Interaction].Main) then // clicked on Song
              begin 
                if CatSongs.CatNumShow = -3 then
                begin
                  ScreenSongMenu.OnShow;
                  ScreenSongMenu.MenuShow(SM_Playlist);
                end
                else
                begin
                  ScreenSongMenu.OnShow;
                  ScreenSongMenu.MenuShow(SM_Main);
                end;
              end
              else
              begin
                ScreenSongMenu.OnShow;
                ScreenSongMenu.MenuShow(SM_Playlist_Load);
              end;
            end //Party Mode -> Show Party Menu
            else
            begin
              ScreenSongMenu.OnShow;
              ScreenSongMenu.MenuShow(SM_Party_Main);
            end;
          end;
          Exit;
        end;

      Ord('P'): //Show Playlist Menu
        begin
          if (Songs.SongList.Count > 0) and (Mode = smNormal) then
          begin
            ScreenSongMenu.OnShow;
            ScreenSongMenu.MenuShow(SM_Playlist_Load);
          end;
          Exit;
        end;

      Ord('J'): //Show Jumpto Menu
        begin
          if (Songs.SongList.Count > 0) and (Mode = smNormal) then
          begin
            ScreenSongJumpto.Visible := true;
          end;
          Exit;
        end;

      Ord('E'):
        begin
          OpenEditor;
          Exit;
        end;

      Ord('R'):
        begin
          if (Songs.SongList.Count > 0) and
             (Mode = smNormal) then
          begin
            if (SDL_ModState = KMOD_LSHIFT) and (Ini.TabsAtStartup = 1) then // random category
            begin
              I2 := 0; // count cats
              for I := 0 to High(CatSongs.Song) do
              begin
                if CatSongs.Song[I].Main then
                  Inc(I2);
              end;

              I2 := Random(I2 + 1); // random and include I2

              // find cat:
              for I := 0 to High(CatSongs.Song) do
                begin
                if CatSongs.Song[I].Main then
                  Dec(I2);
                if (I2 <= 0) then
                begin
                  // show cat in top left mod
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
            else if (SDL_ModState = KMOD_LCTRL) and (Ini.TabsAtStartup = 1) then // random in all categories
            begin
              repeat
                I2 := Random(High(CatSongs.Song) + 1);
              until (not CatSongs.Song[I2].Main);

              // search cat
              for I := I2 downto 0 do
              begin
              if CatSongs.Song[I].Main then
                break;
              end;
              
              // in I is now the categorie in I2 the song

              // choose cat
              CatSongs.ShowCategoryList;

              // show cat in top left mod
              ShowCatTL (I);

              CatSongs.ClickCategoryButton(I);
              SelectNext;

              // Fix: not existing song selected:
              //if (I + 1 = I2) then
                Inc(I2);

              // choose song
              SkipTo(I2 - I);
            end
            else // random in one category
            begin
              SkipTo(Random(CatSongs.VisibleSongs));
            end;
            AudioPlayback.PlaySound(SoundLib.Change);

            SetScroll4;
          end;
          Exit;
        end;
    end; // normal keys

    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          if (Mode = smNormal) then
          begin
            //On Escape goto Cat-List Hack
            if (Ini.TabsAtStartup = 1) and (CatSongs.CatNumShow <> -1) then
              begin
              //Find Category
              I := Interaction;
              while (not CatSongs.Song[I].Main) do
              begin
                Dec(I);
                if (I < 0) then
                  break;
              end;
              if (I <= 1) then
                Interaction := High(CatSongs.Song)
              else
                Interaction := I - 1;

              //Stop Music
              StopMusicPreview();

              CatSongs.ShowCategoryList;

              //Show Cat in Top Left Mod
              HideCatTL;

              //Show Wrong Song when Tabs on Fix
              SelectNext;
              FixSelected;
              //SelectPrev(true);
              //CatSongs.Song[0].Visible := false;
              end
            else
            begin
            //On Escape goto Cat-List Hack End
              //Tabs off and in Search or Playlist -> Go back to Song view
              if (CatSongs.CatNumShow < -1) then
              begin
                //Atm: Set Empty Filter
                CatSongs.SetFilter('', fltAll);

                //Show Cat in Top Left Mod
                HideCatTL;
                Interaction := 0;

                //Show Wrong Song when Tabs on Fix
                SelectNext;
                FixSelected;
              end
              else
              begin
                StopMusicPreview();
                AudioPlayback.PlaySound(SoundLib.Back);

                FadeTo(@ScreenMain);
              end;

            end;
          end
          //When in party Mode then Ask before Close
          else if (Mode = smPartyMode) then
          begin
            AudioPlayback.PlaySound(SoundLib.Back);
            CheckFadeTo(@ScreenMain,'MSG_END_PARTY');
          end;
        end;
      SDLK_RETURN:
        begin
          if (Songs.SongList.Count > 0) then
          begin
            if CatSongs.Song[Interaction].Main then
            begin // clicked on Category Button
              //Show Cat in Top Left Mod
              ShowCatTL (Interaction);

              //I := CatSongs.VisibleIndex(Interaction);
              CatSongs.ClickCategoryButton(Interaction);
              {I2 := CatSongs.VisibleIndex(Interaction);
              SongCurrent := SongCurrent - I + I2;
              SongTarget := SongTarget - I + I2; }

              //  SetScroll4;

              //Show Wrong Song when Tabs on Fix
              SelectNext;
              FixSelected;
            end
            else
            begin // clicked on song
              if (Mode = smNormal) then //Normal Mode -> Start Song
              begin
                //Do the Action that is specified in Ini
                case Ini.OnSongClick of
                  0: StartSong;
                  1: SelectPlayers;
                  2:begin
                      if (CatSongs.CatNumShow = -3) then
                        ScreenSongMenu.MenuShow(SM_Playlist)
                      else
                        ScreenSongMenu.MenuShow(SM_Main);
                    end;
                end;
              end
              else if (Mode = smPartyMode) then //PartyMode -> Show Menu
              begin
                if (Ini.PartyPopup = 1) then
                  ScreenSongMenu.MenuShow(SM_Party_Main)
                else
                  Party.CallAfterSongSelect;
              end;
            end;
          end;
        end;

      SDLK_DOWN:
        begin
          if (Mode = smNormal) then
          begin
            //Only Change Cat when not in Playlist or Search Mode
            if (CatSongs.CatNumShow > -2) then
            begin
              //Cat Change Hack
              if Ini.TabsAtStartup = 1 then
              begin
                I := Interaction;
                if I <= 0 then
                  I := 1;

                while not catsongs.Song[I].Main do
                begin
                  Inc (I);
                  if (I > High(catsongs.Song)) then
                    I := Low(catsongs.Song);
                end;

                Interaction := I;

                //Show Cat in Top Left Mod
                ShowCatTL (Interaction);

                CatSongs.ClickCategoryButton(Interaction);
                SelectNext;
                FixSelected;

                //Play Music:
                AudioPlayback.PlaySound(SoundLib.Change);
              end;

            //
            //Cat Change Hack End}
            end;
          end;
        end;
      SDLK_UP:
        begin
          if (Mode = smNormal) then
          begin
            //Only Change Cat when not in Playlist or Search Mode
            if (CatSongs.CatNumShow > -2) then
            begin
              //Cat Change Hack
              if Ini.TabsAtStartup = 1 then
              begin
                I := Interaction;
                I2 := 0;
                if I <= 0 then
                  I := 1;

                while not catsongs.Song[I].Main or (I2 = 0) do
                begin
                  if catsongs.Song[I].Main then
                    Inc(I2);
                  Dec (I);
                  if (I < Low(catsongs.Song)) then
                    I := High(catsongs.Song);
                end;

                Interaction := I;

                //Show Cat in Top Left Mod
                ShowCatTL (I);

                CatSongs.ClickCategoryButton(I);
                SelectNext;
                FixSelected;

                //Play Music:
                AudioPlayback.PlaySound(SoundLib.Change);
              end;
            end;
            //Cat Change Hack End}
          end;
        end;

      SDLK_RIGHT:
        begin
          if (Songs.SongList.Count > 0) and (Mode = smNormal) then
          begin
            AudioPlayback.PlaySound(SoundLib.Change);
            SelectNext;
            SetScroll4;
          end;
        end;

      SDLK_LEFT:
        begin
          if (Songs.SongList.Count > 0)and (Mode = smNormal)  then
          begin
            AudioPlayback.PlaySound(SoundLib.Change);
            SelectPrev;
            SetScroll4;
          end;
        end;

      SDLK_1:
        begin //Joker
          DoJoker(0);
        end;

      SDLK_2:
        begin //Joker
          DoJoker(1);
        end;

      SDLK_3:
        begin //Joker
          DoJoker(2);
        end;
    end;
  end; // if (PressedDown)
end;

function TScreenSong.ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean;
  var
    I, J: Integer;
    Btn: Integer;
begin
  Result := true;

  if (ScreenSongMenu.Visible) then
  begin
    Result := ScreenSongMenu.ParseMouse(MouseButton, BtnDown, X, Y);
    exit;
  end
  else if (ScreenSongJumpTo.Visible) then
  begin
    Result := ScreenSongJumpTo.ParseMouse(MouseButton, BtnDown, X, Y);
    exit;
  end
  else // no extension visible
  begin
    if (BtnDown) then
    begin
      //if RightMbESC is set, send ESC keypress
      if RightMbESC and (MouseButton = SDL_BUTTON_RIGHT) then
        Result:=ParseInput(SDLK_ESCAPE, 0, true)

     //song scrolling with mousewheel
      else if (MouseButton = SDL_BUTTON_WHEELDOWN) then
        ParseInput(SDLK_RIGHT, 0, true)

      else if (MouseButton = SDL_BUTTON_WHEELUP) then
        ParseInput(SDLK_LEFT, 0, true)

      //LMB anywhere starts
      else if (MouseButton = SDL_BUTTON_LEFT) then
      begin
        if (CatSongs.VisibleSongs > 4) then
        begin
          // select the second visible button left from selected
          I := 0;
          Btn := Interaction;
          while (I < 2) do
          begin
            Dec(Btn);
            if (Btn < 0) then
              Btn := High(CatSongs.Song);

            if (CatSongs.Song[Btn].Visible) then
              Inc(I);
          end;

          // test the 5 front buttons for click
          for I := 0 to 4 do
          begin
            if InRegion(X, Y, Button[Btn].GetMouseOverArea) then
            begin
              // song cover clicked
              if (I = 2) then
              begin // Selected Song clicked -> start singing
                ParseInput(SDLK_RETURN, 0, true);
              end
              else
              begin // one of the other 4 covers in the front clicked -> select it
                J := I - 2;
                while (J < 0) do
                begin
                  ParseInput(SDLK_LEFT, 0, true);
                  Inc(J);
                end;

                while (J > 0) do
                begin
                  ParseInput(SDLK_RIGHT, 0, true);
                  Dec(J);
                end;
              end;
              Break;
            end;

            Btn := CatSongs.FindNextVisible(Btn);
            if (Btn = -1) then
              Break;
          end;
        end
        else
          ParseInput(SDLK_RETURN, 0, true);
      end;
    end;
  end;
end;

constructor TScreenSong.Create;
var
  i: integer;
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
  for i := 0 to High(Theme.Song.StaticParty) do
    StaticParty[i] := AddStatic(Theme.Song.StaticParty[i]);

  SetLength(TextParty, Length(Theme.Song.TextParty));
  for i := 0 to High(Theme.Song.TextParty) do
    TextParty[i] := AddText(Theme.Song.TextParty[i]);

  SetLength(StaticNonParty, Length(Theme.Song.StaticNonParty));
  for i := 0 to High(Theme.Song.StaticNonParty) do
    StaticNonParty[i] := AddStatic(Theme.Song.StaticNonParty[i]);

  SetLength(TextNonParty, Length(Theme.Song.TextNonParty));
  for i := 0 to High(Theme.Song.TextNonParty) do
    TextNonParty[i] := AddText(Theme.Song.TextNonParty[i]);

  // Song List
  //Songs.LoadSongList; // moved to the UltraStar unit
  CatSongs.Refresh;

  GenerateThumbnails();

  // Randomize Patch
  Randomize;

  Equalizer := Tms_Equalizer.Create(AudioPlayback, Theme.Song.Equalizer);

  PreviewOpened := -1;
  isScrolling := false;
end;

procedure TScreenSong.GenerateThumbnails();
var
  I: integer;
  CoverButtonIndex: integer;
  CoverButton: TButton;
  CoverTexture: TTexture;
  Cover: TCover;
  CoverFile: IPath;
  Song: TSong;
begin
  if (Length(CatSongs.Song) <= 0) then
    Exit;

  // set length of button array once instead for every song
  SetButtonLength(Length(CatSongs.Song));

  // create all buttons
  for I := 0 to High(CatSongs.Song) do
  begin
    CoverButton := nil;

    // create a clickable cover
    CoverButtonIndex := AddButton(300 + I*250, 140, 200, 200, PATH_NONE, TEXTURE_TYPE_PLAIN, Theme.Song.Cover.Reflections);
    if (CoverButtonIndex > -1) then
      CoverButton := Button[CoverButtonIndex];
    if (CoverButton = nil) then
      Continue;

    Song := CatSongs.Song[I];

    CoverFile := Song.Path.Append(Song.Cover);
    if (not CoverFile.IsFile()) then
      Song.Cover := PATH_NONE;

    if (Song.Cover.IsUnset) then
      CoverFile := Skin.GetTextureFileName('SongCover');

    // load cover and cache its texture
    Cover := Covers.FindCover(CoverFile);
    if (Cover = nil) then
      Cover := Covers.AddCover(CoverFile);

    // use the cached texture
    // TODO: this is a workaround until the new song-loading works.
    // The TCover object should be added to the song-object. The thumbnails
    // should be loaded each time the song-screen is shown (it is real fast).
    // This way, we will not waste that much memory and have a link between
    // song and cover.
    if (Cover <> nil) then
    begin
      CoverTexture := Cover.GetPreviewTexture();
      Texture.AddTexture(CoverTexture, TEXTURE_TYPE_PLAIN, true);
      CoverButton.Texture := CoverTexture;

      // set selected to false -> the right texture will be displayed
      CoverButton.Selected := False;
    end;

    Cover.Free;
  end;

  // reset selection
  if (Length(CatSongs.Song) > 0) then
    Interaction := 0;
end;

{ called when song flows movement stops at a song }
procedure TScreenSong.OnSongSelect;
begin
  if (Ini.PreviewVolume <> 0) then
  begin
    StartMusicPreview;
  end;

  // fade in detailed cover
  CoverTime := 0;
end;

{ called before current song is deselected }
procedure TScreenSong.OnSongDeSelect;
begin
  CoverTime := 10;
  UnLoadDetailedCover;

  StopMusicPreview();
  PreviewOpened := -1;
end;

procedure TScreenSong.SetScroll;
var
  VS, B: integer;
begin
  VS := CatSongs.VisibleSongs;
  if VS > 0 then
  begin
    // Set Positions
    case Theme.Song.Cover.Style of
      3: SetScroll3;
      5: SetScroll5;
      6: SetScroll6;
      else SetScroll4;
    end;

    // Set visibility of video icon
    Statics[VideoIcon].Visible := CatSongs.Song[Interaction].Video.IsSet;

    // Set texts
    Text[TextArtist].Text := CatSongs.Song[Interaction].Artist;
    Text[TextTitle].Text  :=  CatSongs.Song[Interaction].Title;
    if (Ini.TabsAtStartup = 1) and (CatSongs.CatNumShow = -1) then
    begin
      Text[TextNumber].Text := IntToStr(CatSongs.Song[Interaction].OrderNum) + '/' + IntToStr(CatSongs.CatCount);
      Text[TextTitle].Text  := '(' + IntToStr(CatSongs.Song[Interaction].CatNumber) + ' ' + Language.Translate('SING_SONGS_IN_CAT') + ')';
    end
    else if (CatSongs.CatNumShow = -2) then
      Text[TextNumber].Text := IntToStr(CatSongs.VisibleIndex(Interaction)+1) + '/' + IntToStr(VS)
    else if (CatSongs.CatNumShow = -3) then
      Text[TextNumber].Text := IntToStr(CatSongs.VisibleIndex(Interaction)+1) + '/' + IntToStr(VS)
    else if (Ini.TabsAtStartup = 1) then
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
      Button[B].Visible := false;

  end;
end;

(*
procedure TScreenSong.SetScroll1;
var
  B:      integer;    // button
  Src:    integer;
  //Dst:    integer;
  Count:  integer;    // Dst is not used. Count is used.
  Ready:  boolean;

  VisCount: integer;  // count of visible (or selectable) buttons
  VisInt:   integer;  // visible position of interacted button
  Typ:      integer;  // 0 when all songs fits the screen
  Placed:   integer;  // number of placed visible buttons
begin
  //Src := 0;
  //Dst := -1;
  Count := 1;
  Typ := 0;
  Ready := false;
  Placed := 0;

  VisCount := 0;
  for B := 0 to High(Button) do
    if CatSongs.Song[B].Visible then
      Inc(VisCount);

  VisInt := 0;
  for B := 0 to Interaction-1 do
    if CatSongs.Song[B].Visible then
      Inc(VisInt);

  if VisCount <= 6 then
  begin
    Typ := 0;
  end
  else
  begin
    if VisInt <= 3 then
    begin
      Typ := 1;
      Count := 7;
      Ready := true;
    end;

    if (VisCount - VisInt) <= 3 then
    begin
      Typ := 2;
      Count := 7;
      Ready := true;
    end;

    if not Ready then
    begin
      Typ := 3;
      Src := Interaction;
    end;
  end;


  // hide all buttons
  for B := 0 to High(Button) do
  begin
    Button[B].Visible := false;
    Button[B].Selectable := CatSongs.Song[B].Visible;
  end;

  {
  for B := Src to Dst do
  begin
    //Button[B].Visible := true;
    Button[B].Visible := CatSongs.Song[B].Visible;
    Button[B].Selectable := Button[B].Visible;
    Button[B].Y := 140 + (B-Src) * 60;
  end;
  }

  if Typ = 0 then
  begin
    for B := 0 to High(Button) do
    begin
      if CatSongs.Song[B].Visible then
      begin
        Button[B].Visible := true;
        Button[B].Y := 140 + (Placed) * 60;
        Inc(Placed);
      end;
    end;
  end;

  if Typ = 1 then
  begin
    B := 0;
    while (Count > 0) do
    begin
      if CatSongs.Song[B].Visible then
      begin
        Button[B].Visible := true;
        Button[B].Y := 140 + (Placed) * 60;
        Inc(Placed);
        Dec(Count);
      end;
      Inc(B);
    end;
  end;

  if Typ = 2 then
  begin
    B := High(Button);
    while (Count > 0) do
    begin
      if CatSongs.Song[B].Visible then
      begin
        Button[B].Visible := true;
        Button[B].Y := 140 + (6-Placed) * 60;
        Inc(Placed);
        Dec(Count);
      end;
      Dec(B);
    end;
  end;

  if Typ = 3 then
  begin
    B := Src;
    Count := 4;
    while (Count > 0) do
    begin
      if CatSongs.Song[B].Visible then
      begin
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
    while (Count > 0) do
    begin
      if CatSongs.Song[B].Visible then
      begin
        Button[B].Visible := true;
        Button[B].Y := 140 + (2-Placed) * 60;
        Inc(Placed);
        Dec(Count);
      end;
      Dec(B);
    end;

  end;

  if Length(Button) > 0 then
    Statics[1].Texture.Y := Button[Interaction].Y - 5; // selection texture
end;

procedure TScreenSong.SetScroll2;
var
  B:      integer;
  //Factor:    integer; // factor of position relative to center of screen
  //Factor2:   real;
begin
  // line
  for B := 0 to High(Button) do
    Button[B].X := 300 + (B - Interaction) * 260;

  if Length(Button) >= 3 then
  begin
    if Interaction = 0 then
      Button[High(Button)].X := 300 - 260;

    if Interaction = High(Button) then
      Button[0].X := 300 + 260;
  end;

  // circle
  {
  for B := 0 to High(Button) do
  begin
    Factor := (B - Interaction); // 0 to center, -1: to left, +1 to right
    Factor2 := Factor / Length(Button);
    Button[B].X := 300 + 10000 * sin(2*pi*Factor2);
    //Button[B].Y := 140 + 50 * ;
  end;
  }
end;
*)

procedure TScreenSong.SetScroll3; // with slide
var
  B:      integer;
  //Factor:    integer; // factor of position relative to center of screen
  //Factor2:   real;
begin
  SongTarget := Interaction;

  // line
  for B := 0 to High(Button) do
  begin
    Button[B].X := 300 + (B - SongCurrent) * 260;
    if (Button[B].X < -Button[B].W) or (Button[B].X > 800) then
      Button[B].Visible := false
    else
      Button[B].Visible := true;
  end;

  {
  if Length(Button) >= 3 then
  begin
    if Interaction = 0 then
      Button[High(Button)].X := 300 - 260;

    if Interaction = High(Button) then
      Button[0].X := 300 + 260;
  end;
  }

  // circle
  {
  for B := 0 to High(Button) do
  begin
    Factor := (B - Interaction); // 0 to center, -1: to left, +1 to right
    Factor2 := Factor / Length(Button);
    Button[B].X := 300 + 10000 * sin(2*pi*Factor2);
    //Button[B].Y := 140 + 50 * ;
  end;
  }
end;

(**
 * Rotation
 *)
procedure TScreenSong.SetScroll4;
var
  B:      integer;
  Angle:  real;
  Z, Z2:  real;
  VS:     integer;
begin
  VS := CatSongs.VisibleSongs();

  for B := 0 to High(Button) do
  begin
    Button[B].Visible := CatSongs.Song[B].Visible;
    if Button[B].Visible then
    begin
      // angle between the cover and selected song-cover in radians
      Angle := 2*Pi * (CatSongs.VisibleIndex(B) - SongCurrent) /  VS;

      // calc z-position from angle
      Z := (1 + cos(Angle)) / 2;  // scaled to range [0..1]
      Z2 := (1 + 2*Z) / 3;        // scaled to range [1/3..1]

      // adjust cover's width and height according its z-position
      // Note: Theme.Song.Cover.W is not used as width and height are equal
      //   and Theme.Song.Cover.W is used as circle radius in Scroll5.
      Button[B].W := Theme.Song.Cover.H * Z2;
      Button[B].H := Button[B].W;

      // set cover position
      Button[B].X := Theme.Song.Cover.X +
                     (0.185 * Theme.Song.Cover.H * VS * sin(Angle)) * Z2 -
                     ((Button[B].H - Theme.Song.Cover.H)/2);
      Button[B].Y := Theme.Song.Cover.Y  +
                     (Theme.Song.Cover.H - Abs(Button[B].H)) * 0.7;
      Button[B].Z := Z / 2 + 0.3;
    end;
  end;
end;

(**
 * rotate
 *)
procedure TScreenSong.SetScroll5;
var
  B:      integer;
  Angle:    real;
  Pos:    real;
  VS:     integer;
  Padding:     real;
  X:        real;
  {
  Theme.Song.CoverW: circle radius
  Theme.Song.CoverX: x-pos. of the left edge of the selected cover
  Theme.Song.CoverY: y-pos. of the upper edge of the selected cover
  Theme.Song.CoverH: cover height
  }
begin
  VS := CatSongs.VisibleSongs();

  // Update positions of all buttons
  for B := 0 to High(Button) do
  begin
    Button[B].Visible := CatSongs.Song[B].Visible; // adjust visibility
    if Button[B].Visible then // Only change pos for visible buttons
    begin
      // Pos is the distance to the centered cover in the range [-VS/2..+VS/2]
      Pos := (CatSongs.VisibleIndex(B) - SongCurrent);
      if (Pos < -VS/2) then
        Pos := Pos + VS
      else if (Pos > VS/2) then
        Pos := Pos - VS;

      // Avoid overlapping of the front covers.
      // Use an alternate position for the five front covers. 
      if (Abs(Pos) < 2.5) then
      begin
        Angle := Pi * (Pos / Min(VS, 5)); // Range: (-1/4*Pi .. +1/4*Pi)

        Button[B].H := Abs(Theme.Song.Cover.H * cos(Angle*0.8));
        Button[B].W := Button[B].H;

        //Button[B].Reflectionspacing := 15 * Button[B].H/Theme.Song.Cover.H;
        Button[B].DeSelectReflectionspacing := 15 * Button[B].H/Theme.Song.Cover.H;

        Padding := (Button[B].H - Theme.Song.Cover.H)/2;
        X := Sin(Angle*1.3) * 0.9;

        Button[B].X := Theme.Song.Cover.X + Theme.Song.Cover.W * X - Padding;
        Button[B].Y := (Theme.Song.Cover.Y  + (Theme.Song.Cover.H - Abs(Theme.Song.Cover.H * cos(Angle))) * 0.5);
        Button[B].Z := 0.95 - Abs(Pos) * 0.01;

        if VS < 5 then
          Button[B].Texture.Alpha := 1 - Abs(Pos) / VS  * 2
        else
          Button[B].Texture.Alpha := 1;    
      end
      { only draw 3 visible covers in the background
        (the 3 that are on the opposite of the front covers}
      else if (VS > 7) and (Abs(Pos) > floor(VS/2) - 1.5) then
      begin
        // Transform Pos to range [-1..-3/4, +3/4..+1]
        { the 3 covers at the back will show up in the gap between the
          front cover and its neighbors
          one cover will be hiddenbehind the front cover,
          but this will not be a lack of performance ;) }
        if Pos < 0 then
          Pos := (Pos - 2 + ceil(VS/2))/8 - 0.75
        else
          Pos := (Pos + 2 - floor(VS/2))/8 + 0.75;

        // angle in radians [-2Pi..-Pi, +Pi..+2Pi]
        Angle := 2*Pi * Pos;

        Button[B].H := 0.6*(Theme.Song.Cover.H-Abs(Theme.Song.Cover.H * cos(Angle/2)*0.8));
        Button[B].W := Button[B].H;

        Padding := (Button[B].H - Theme.Song.Cover.H)/2;

        Button[B].X :=  Theme.Song.Cover.X+Theme.Song.Cover.H/2-Button[b].H/2+Theme.Song.Cover.W/320*((Theme.Song.Cover.H)*sin(Angle/2)*1.52);
        Button[B].Y := Theme.Song.Cover.Y  - (Button[B].H - Theme.Song.Cover.H)*0.75;
        Button[B].Z := (0.4 - Abs(Pos/4)) -0.00001; //z < 0.49999 is behind the cover 1 is in front of the covers

        Button[B].Texture.Alpha := 1;

        //Button[B].Reflectionspacing := 15 * Button[B].H/Theme.Song.Cover.H;
        Button[B].DeSelectReflectionspacing := 15 * Button[B].H/Theme.Song.Cover.H;
      end
      { all other covers are not visible }
      else
      Button[B].Visible := false;
    end;
  end;
end;

procedure TScreenSong.SetScroll6; // rotate (slotmachine style)
var
  B:      integer;
  Angle:  real;
  Pos:    real;
  VS:     integer;
  diff:   real;
  X:      real;
  Factor: real;
  Z, Z2:  real;
begin
  VS := CatSongs.VisibleSongs;
  if VS <= 5 then
  begin
    // circle
    for B := 0 to High(Button) do
    begin
      Button[B].Visible := CatSongs.Song[B].Visible;
      if Button[B].Visible then // optimization for 1000 songs - updates only visible songs, hiding in tabs becomes useful for maintaing good speed
      begin
  
        Factor := 2 * pi * (CatSongs.VisibleIndex(B) - SongCurrent) /  VS {CatSongs.VisibleSongs};// 0.5.0 (II): takes another 16ms

        Z := (1 + cos(Factor)) / 2;
        Z2 := (1 + 2*Z) / 3;

        Button[B].Y := Theme.Song.Cover.Y + (0.185 * Theme.Song.Cover.H * VS * sin(Factor)) * Z2 - ((Button[B].H - Theme.Song.Cover.H)/2); // 0.5.0 (I): 2 times faster by not calling CatSongs.VisibleSongs
        Button[B].Z := Z / 2 + 0.3;

        Button[B].W := Theme.Song.Cover.H * Z2;

        //Button[B].Y := {50 +} 140 + 50 - 50 * Z2;
        Button[B].X := Theme.Song.Cover.X  + (Theme.Song.Cover.H - Abs(Button[B].H)) * 0.7 ;
        Button[B].H := Button[B].W;
      end;
    end;
  end
  else
  begin
    //Change Pos of all Buttons
    for B := Low(Button) to High(Button) do
    begin
      Button[B].Visible := CatSongs.Song[B].Visible; //Adjust Visibility
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
          //Button[B].Visible := false;

          Button[B].H := Abs(Theme.Song.Cover.H * cos(Angle*0.8));//Power(Z2, 3);

          Button[B].DeSelectReflectionspacing := 15 * Button[B].H/Theme.Song.Cover.H;

          Button[B].Z := 0.95 - Abs(Pos) * 0.01;

          Button[B].X := (Theme.Song.Cover.X  + (Theme.Song.Cover.H - Abs(Theme.Song.Cover.H * cos(Angle))) * 0.5);

          Button[B].W := Button[B].H;

          Diff := (Button[B].H - Theme.Song.Cover.H)/2;

          X := Sin(Angle*1.3)*0.9;

          Button[B].Y := Theme.Song.Cover.Y + Theme.Song.Cover.W * X - Diff;
        end
        else
        begin {Behind the Front Covers}

          // limit-bg-covers hack
          if (abs(VS/2-abs(Pos))>10) then
            Button[B].Visible := false;
          if VS > 25 then
            VS:=25;
          // end of limit-bg-covers hack

          if Pos < 0 then
            Pos := (Pos - VS/2)/VS
          else
            Pos := (Pos + VS/2)/VS;

          Angle := Pi * Pos*2;

          Button[B].Z := (0.4 - Abs(Pos/4)) -0.00001; //z < 0.49999 is behind the cover 1 is in front of the covers

          Button[B].H :=0.6*(Theme.Song.Cover.H-Abs(Theme.Song.Cover.H * cos(Angle/2)*0.8));//Power(Z2, 3);

          Button[B].W := Button[B].H;

          Button[B].X := Theme.Song.Cover.X  - (Button[B].H - Theme.Song.Cover.H)*0.5;

          Button[B].DeSelectReflectionspacing := 15 * Button[B].H/Theme.Song.Cover.H;

          Button[B].Y := Theme.Song.Cover.Y+Theme.Song.Cover.H/2-Button[b].H/2+Theme.Song.Cover.W/320*(Theme.Song.Cover.H*sin(Angle/2)*1.52);
        end;
      end;
    end;
  end;
end;

procedure TScreenSong.OnShow;
begin
  inherited;
{**
 * Pause background music, so we can play it again on scorescreen
 *}
  SoundLib.PauseBgMusic;

  AudioPlayback.Stop;
  PreviewOpened := -1;

  if Ini.Players <= 3 then PlayersPlay := Ini.Players + 1;
  if Ini.Players  = 4 then PlayersPlay := 6;

  //Cat Mod etc
  if (Ini.TabsAtStartup = 1) and (CatSongs.CatNumShow = -1) then
  begin
    CatSongs.ShowCategoryList;
    FixSelected;
    //Show Cat in Top Left Mod
    HideCatTL;
  end;

  if Length(CatSongs.Song) > 0 then
  begin
    SetScroll;
  end;

  //Playlist Mode
  if (Mode = smNormal) then
  begin
    //If Playlist Shown -> Select Next automatically
    if (CatSongs.CatNumShow = -3) then
    begin
      SelectNext;
    end;
  end
  //Party Mode
  else if (Mode = smPartyMode) then
  begin
    SelectRandomSong;
    //Show Menu directly in PartyMode
    //But only if selected in Options
    if (Ini.PartyPopup = 1) then
    begin
      ScreenSongMenu.MenuShow(SM_Party_Main);
    end;
  end;

  isScrolling := true;
  SetJoker;
  SetStatics;
end;

procedure TScreenSong.OnHide;
begin
  // turn music volume to 100%
  AudioPlayback.SetVolume(1.0);

  // stop preview
  StopMusicPreview();
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
  dx: real;
  dt: real;
  I:  integer;
begin
  if isScrolling then
  begin
    dx := SongTarget-SongCurrent;
    dt := TimeSkip * 7;

    if dt > 1 then
      dt := 1;

    SongCurrent := SongCurrent + dx*dt;

    if SameValue(SongCurrent, SongTarget, 0.002) and (CatSongs.VisibleSongs > 0) then
    begin
      isScrolling := false;
      SongCurrent := SongTarget;
      OnSongSelect;
    end;
  end;

  {
  if SongCurrent > Catsongs.VisibleSongs then
  begin
    SongCurrent := SongCurrent - Catsongs.VisibleSongs;
    SongTarget := SongTarget - Catsongs.VisibleSongs;
  end;
  }

  //Log.BenchmarkStart(5);

  SetScroll;

  //Log.BenchmarkEnd(5);
  //Log.LogBenchmark('SetScroll4', 5);

  //Fading Functions, Only if Covertime is under 5 Seconds
  if (CoverTime < 5) then
  begin
    // cover fade
    if (CoverTime < 1) and (CoverTime + TimeSkip >= 1) then
    begin
      // load new texture
      Texture.GetTexture(Button[Interaction].Texture.Name, TEXTURE_TYPE_PLAIN, false);
      Button[Interaction].Texture.Alpha := 1;
      Button[Interaction].Texture2 := Texture.GetTexture(Button[Interaction].Texture.Name, TEXTURE_TYPE_PLAIN, false);
      Button[Interaction].Texture2.Alpha := 1;
    end;

    //Update Fading Time
    CoverTime := CoverTime + TimeSkip;

    //Update Fading Texture
    Button[Interaction].Texture2.Alpha := (CoverTime - 1) * 1.5;
    if Button[Interaction].Texture2.Alpha > 1 then
      Button[Interaction].Texture2.Alpha := 1;

  end;

  //inherited Draw;
  //heres a little Hack, that causes the Statics
  //are Drawn after the Buttons because of some Blending Problems.
  //This should cause no Problems because all Buttons on this screen
  //Has Z Position.
  //Draw BG
  DrawBG;

  //Instead of Draw FG Procedure:
  //We draw Buttons for our own
  for I := 0 to Length(Button) - 1 do
    Button[I].Draw;

  // Statics
  for I := 0 to Length(Statics) - 1 do
    Statics[I].Draw;

  // and texts
  for I := 0 to Length(Text) - 1 do
    Text[I].Draw;

  Equalizer.Draw;

  DrawExtensions;

  Result := true;
end;

procedure TScreenSong.SelectNext;
var
  Skip: integer;
  VS:   integer;
begin
  VS := CatSongs.VisibleSongs;

  if VS > 0 then
  begin
    if (not isScrolling) and (VS > 0) then
    begin
      isScrolling := true;
      OnSongDeselect;
    end;

    Skip := 1;

    // this 1 could be changed by CatSongs.FindNextVisible
    while (not CatSongs.Song[(Interaction + Skip) mod Length(Interactions)].Visible) do
      Inc(Skip);

    SongTarget := SongTarget + 1;//Skip;

    Interaction := (Interaction + Skip) mod Length(Interactions);

    // try to keep all at the beginning
    if SongTarget > VS-1 then
    begin
      SongTarget := SongTarget - VS;
      SongCurrent := SongCurrent - VS;
    end;

  end;

  // Interaction -> Button, load cover
  // show uncached texture
  //Button[Interaction].Texture := Texture.GetTexture(Button[Interaction].Texture.Name, TEXTURE_TYPE_PLAIN, false);
end;

procedure TScreenSong.SelectPrev;
var
  Skip: integer;
  VS:   integer;
begin
  VS := CatSongs.VisibleSongs;

  if VS > 0 then
  begin
    if (not isScrolling) and (VS > 0) then
    begin
      isScrolling := true;
      OnSongDeselect;
    end;

    Skip := 1;

    while (not CatSongs.Song[(Interaction - Skip + Length(Interactions)) mod Length(Interactions)].Visible) do
      Inc(Skip);
    SongTarget := SongTarget - 1;//Skip;

    Interaction := (Interaction - Skip + Length(Interactions)) mod Length(Interactions);

    // try to keep all at the beginning
    if SongTarget < 0 then
    begin
      SongTarget := SongTarget + CatSongs.VisibleSongs;
      SongCurrent := SongCurrent + CatSongs.VisibleSongs;
    end;

    // show uncached texture
    //Button[Interaction].Texture := Texture.GetTexture(Button[Interaction].Texture.Name, TEXTURE_TYPE_PLAIN, false);
  end;
end;

procedure TScreenSong.StartMusicPreview();
var
  Song: TSong;
begin
  AudioPlayback.Close();

  if CatSongs.VisibleSongs = 0 then
    Exit;
    
  Song := CatSongs.Song[Interaction];
  if not assigned(Song) then
    Exit;

  //fix: if main cat than there is nothing to play
  if Song.main then
    Exit;

  if AudioPlayback.Open(Song.Path.Append(Song.Mp3)) then
  begin
    PreviewOpened := Interaction;
    
    AudioPlayback.Position := AudioPlayback.Length / 4;
    // set preview volume
    if (Ini.PreviewFading = 0) then
    begin
      // music fade disabled: start with full volume
      AudioPlayback.SetVolume(IPreviewVolumeVals[Ini.PreviewVolume]);
      AudioPlayback.Play()
    end
    else
    begin
      // music fade enabled: start muted and fade-in
      AudioPlayback.SetVolume(0);
      AudioPlayback.FadeIn(Ini.PreviewFading, IPreviewVolumeVals[Ini.PreviewVolume]);
    end;
  end;
end;

procedure TScreenSong.StopMusicPreview();
begin
  // Stop preview of previous song
  AudioPlayback.Stop;
end;

// Changes previewed song
procedure TScreenSong.ChangeMusic;
begin
  StopMusicPreview();
  PreviewOpened := -1;
  StartMusicPreview();
end;

procedure TScreenSong.SkipTo(Target: cardinal);
var
  i: integer;
begin
  Interaction := High(CatSongs.Song);
  SongTarget  := 0;

  for i := 1 to Target+1 do
    SelectNext;

  FixSelected2;
end;

procedure TScreenSong.SelectRandomSong;
var
  I, I2: integer;
begin
  case PlaylistMan.Mode of
      smNormal:  // all songs just select random song
        begin
          // when tabs are activated then use tab method
          if (Ini.TabsAtStartup = 1) then
          begin
            repeat
              I2 := Low(CatSongs.Song) + Random(High(CatSongs.Song) + 1 - Low(CatSongs.Song));
            until CatSongs.Song[I2].Main = false;

            // search cat
            for I := I2 downto Low(CatSongs.Song) do
            begin
              if CatSongs.Song[I].Main then
                break;
            end;
            // I is the cat number, I2 is the no of the song within this cat

            // choose cat
            CatSongs.ShowCategoryList;

            // show cat in top left mod
            ShowCatTL(I);

            CatSongs.ClickCategoryButton(I);
            SelectNext;

            // choose song
            SkipTo(I2 - I);
          end
          // when tabs are deactivated use easy method
          else
            SkipTo(Random(CatSongs.VisibleSongs));
        end;
      smPartyMode:  // one category select category and select random song
        begin
          CatSongs.ShowCategoryList;
          CatSongs.ClickCategoryButton(PlaylistMan.CurPlayList);
          ShowCatTL(PlaylistMan.CurPlayList);

          SelectNext;
          FixSelected2;

          SkipTo(Random(CatSongs.VisibleSongs));
        end;
      smPlaylistRandom:  // playlist: select playlist and select random song
        begin
          PlaylistMan.SetPlayList(PlaylistMan.CurPlayList);

          SkipTo(Random(CatSongs.VisibleSongs));
          FixSelected2;
        end;
  end;

  AudioPlayback.PlaySound(SoundLib.Change);
  SetScroll;
end;

procedure TScreenSong.SetJoker;
begin
  // If Party Mode
  if Mode = smPartyMode then //Show Joker that are available
  begin
    if (Length(Party.Teams) >= 1) then
    begin
      Statics[StaticTeam1Joker1].Visible := (Party.Teams[0].JokersLeft >= 1);
      Statics[StaticTeam1Joker2].Visible := (Party.Teams[0].JokersLeft >= 2);
      Statics[StaticTeam1Joker3].Visible := (Party.Teams[0].JokersLeft >= 3);
      Statics[StaticTeam1Joker4].Visible := (Party.Teams[0].JokersLeft >= 4);
      Statics[StaticTeam1Joker5].Visible := (Party.Teams[0].JokersLeft >= 5);
    end
    else
    begin
      Statics[StaticTeam1Joker1].Visible := false;
      Statics[StaticTeam1Joker2].Visible := false;
      Statics[StaticTeam1Joker3].Visible := false;
      Statics[StaticTeam1Joker4].Visible := false;
      Statics[StaticTeam1Joker5].Visible := false;
    end;

    if (Length(Party.Teams) >= 2) then
    begin
      Statics[StaticTeam2Joker1].Visible := (Party.Teams[1].JokersLeft >= 1);
      Statics[StaticTeam2Joker2].Visible := (Party.Teams[1].JokersLeft >= 2);
      Statics[StaticTeam2Joker3].Visible := (Party.Teams[1].JokersLeft >= 3);
      Statics[StaticTeam2Joker4].Visible := (Party.Teams[1].JokersLeft >= 4);
      Statics[StaticTeam2Joker5].Visible := (Party.Teams[1].JokersLeft >= 5);
    end
    else
    begin
      Statics[StaticTeam2Joker1].Visible := false;
      Statics[StaticTeam2Joker2].Visible := false;
      Statics[StaticTeam2Joker3].Visible := false;
      Statics[StaticTeam2Joker4].Visible := false;
      Statics[StaticTeam2Joker5].Visible := false;
    end;

    if (Length(Party.Teams) >= 3) then
    begin
      Statics[StaticTeam3Joker1].Visible := (Party.Teams[2].JokersLeft >= 1);
      Statics[StaticTeam3Joker2].Visible := (Party.Teams[2].JokersLeft >= 2);
      Statics[StaticTeam3Joker3].Visible := (Party.Teams[2].JokersLeft >= 3);
      Statics[StaticTeam3Joker4].Visible := (Party.Teams[2].JokersLeft >= 4);
      Statics[StaticTeam3Joker5].Visible := (Party.Teams[2].JokersLeft >= 5);
    end
    else
    begin
      Statics[StaticTeam3Joker1].Visible := false;
      Statics[StaticTeam3Joker2].Visible := false;
      Statics[StaticTeam3Joker3].Visible := false;
      Statics[StaticTeam3Joker4].Visible := false;
      Statics[StaticTeam3Joker5].Visible := false;
    end;
  end
  else
  begin //Hide all
    Statics[StaticTeam1Joker1].Visible := false;
    Statics[StaticTeam1Joker2].Visible := false;
    Statics[StaticTeam1Joker3].Visible := false;
    Statics[StaticTeam1Joker4].Visible := false;
    Statics[StaticTeam1Joker5].Visible := false;

    Statics[StaticTeam2Joker1].Visible := false;
    Statics[StaticTeam2Joker2].Visible := false;
    Statics[StaticTeam2Joker3].Visible := false;
    Statics[StaticTeam2Joker4].Visible := false;
    Statics[StaticTeam2Joker5].Visible := false;

    Statics[StaticTeam3Joker1].Visible := false;
    Statics[StaticTeam3Joker2].Visible := false;
    Statics[StaticTeam3Joker3].Visible := false;
    Statics[StaticTeam3Joker4].Visible := false;
    Statics[StaticTeam3Joker5].Visible := false;
  end;
end;

procedure TScreenSong.SetStatics;
var
  I:       integer;
  Visible: boolean;
begin
  //Set Visibility of Party Statics and Text
  Visible := (Mode = smPartyMode);

  for I := 0 to High(StaticParty) do
    Statics[StaticParty[I]].Visible := Visible;

  for I := 0 to High(TextParty) do
    Text[TextParty[I]].Visible := Visible;

  //Set Visibility of Non Party Statics and Text
  Visible := not Visible;

  for I := 0 to High(StaticNonParty) do
    Statics[StaticNonParty[I]].Visible := Visible;

  for I := 0 to High(TextNonParty) do
    Text[TextNonParty[I]].Visible := Visible;
end;

//Procedures for Menu

procedure TScreenSong.StartSong;
begin
  CatSongs.Selected := Interaction;
  StopMusicPreview();

  //Party Mode
  if (Mode = smPartyMode) then
  begin
    FadeTo(@ScreenSing);
  end
  else
  begin
    FadeTo(@ScreenSing);
  end;
end;

procedure TScreenSong.SelectPlayers;
begin
  CatSongs.Selected := Interaction;
  StopMusicPreview();

  ScreenName.Goto_SingScreen := true;
  FadeTo(@ScreenName);
end;

procedure TScreenSong.OpenEditor;
begin
  if (Songs.SongList.Count > 0) and
     (not CatSongs.Song[Interaction].Main) and
     (Mode = smNormal) then
  begin
    StopMusicPreview();
    AudioPlayback.PlaySound(SoundLib.Start);
    CurrentSong := CatSongs.Song[Interaction];
    FadeTo(@ScreenEditSub);
  end;
end;

//Team No of Team (0-5)
procedure TScreenSong.DoJoker (Team: integer);
begin
  if (Mode = smPartyMode) and
     (High(Party.Teams) >= Team) and
     (Party.Teams[Team].JokersLeft > 0) then
  begin
    //Use Joker
    Dec(Party.Teams[Team].JokersLeft);
    SelectRandomSong;
    SetJoker;
  end;
end;

//Detailed Cover Unloading. Unloads the Detailed, uncached Cover of the cur. Song
procedure TScreenSong.UnloadDetailedCover;
begin
  // show cached texture
  Button[Interaction].Texture := Texture.GetTexture(Button[Interaction].Texture.Name, TEXTURE_TYPE_PLAIN, true);
  Button[Interaction].Texture2.Alpha := 0;

  if Button[Interaction].Texture.Name <> Skin.GetTextureFileName('SongCover') then
    Texture.UnloadTexture(Button[Interaction].Texture.Name, TEXTURE_TYPE_PLAIN, false);
end;

procedure TScreenSong.Refresh;
begin
  {
  CatSongs.Refresh;
  CatSongs.ShowCategoryList;
  Interaction := 0;
  SelectNext(true);
  FixSelected;
  }
end;

end.
