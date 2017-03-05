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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenSong.pas $
 * $Id: UScreenSong.pas 3152 2015-10-27 01:23:15Z basisbit $
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
  UCatCovers,
  UCommon,
  UDataBase,
  UDisplay,
  UDllManager,
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
  TVisArr = array of integer;

  TScreenSong = class(TMenu)
    private
      Equalizer: Tms_Equalizer;

      PreviewOpened: Integer; // interaction of the Song that is loaded for preview music
                              // -1 if nothing is opened

      isScrolling: boolean;   // true if song flow is about to move

      fCurrentVideo: IVideo;
      MainChessboardMinLine: integer;
      MainListMinLine: integer;

      LastVisibleSongIndex: integer;
      FirstVisibleSongIndex: integer;

      ListLastVisibleSongIndex: integer;
      ListFirstVisibleSongIndex: integer;
      MainListFirstVisibleSongIndex: integer;

      LastSelectMouse: integer;
      LastSelectTime: integer;

      procedure StartMusicPreview();
      procedure StartVideoPreview();
      procedure StopVideoPreview();
    public
      TextArtist:   integer;
      TextTitle:    integer;
      TextNumber:   integer;
      TextYear:     integer;

      MakeMedley:   boolean;

      //Video Icon Mod
      VideoIcon: cardinal;

      //Medley Icons
	 	  MedleyIcon:     cardinal;
	 	  CalcMedleyIcon: cardinal;
      TextMedleyArtist:   array of integer;
      TextMedleyTitle:    array of integer;
      TextMedleyNumber:   array of integer;
      StaticMedley:   array of integer;

      //Duet Icon
      DuetIcon:     cardinal;
      DuetChange:   boolean;

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

      //Scores
      TextScore:       integer;
      TextMaxScore:    integer;
      TextMediaScore:  integer;
      TextMaxScore2:   integer;
      TextMediaScore2: integer;
      TextScoreUser:   integer;
      TextMaxScoreLocal:    integer;
      TextMediaScoreLocal:  integer;
      TextScoreUserLocal:   integer;

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

      // for chessboard songmenu
      StaticActual: integer;

      // for list songmenu
      StaticList: array of integer;

      ListTextArtist:     array of integer;
      ListTextTitle:      array of integer;
      ListTextYear:       array of integer;
      ListVideoIcon:      array of integer;
      ListMedleyIcon:     array of integer;
      ListCalcMedleyIcon: array of integer;
      ListDuetIcon:       array of integer;

      PlayMidi: boolean;
      MidiFadeIn: boolean;
      FadeTime: cardinal;

      InfoMessageBG: cardinal;
      InfoMessageText: cardinal;

      Static2PlayersDuetSingerP1: cardinal;
      Static2PlayersDuetSingerP2: cardinal;
      Text2PlayersDuetSingerP1: cardinal;
      Text2PlayersDuetSingerP2: cardinal;

      Static3PlayersDuetSingerP1: cardinal;
      Static3PlayersDuetSingerP2: cardinal;
      Static3PlayersDuetSingerP3: cardinal;
      Text3PlayersDuetSingerP1: cardinal;
      Text3PlayersDuetSingerP2: cardinal;
      Text3PlayersDuetSingerP3: cardinal;

      Static4PlayersDuetSingerP3: cardinal;
      Static4PlayersDuetSingerP4: cardinal;

      Static6PlayersDuetSingerP4: cardinal;
      Static6PlayersDuetSingerP5: cardinal;
      Static6PlayersDuetSingerP6: cardinal;

      ColPlayer:  array[0..5] of TRGB;

      //CurrentPartyTime: cardinal;
      //PartyTime: cardinal;
      //TextPartyTime: cardinal;

      MessageTime: cardinal;
      MessageTimeFade: cardinal;

      ChessboardMinLine: integer;
      ListMinLine: integer;
      ListLastMinLine: integer;

      SongIndex:    integer; //Index of Song that is playing since UScreenScore...

      constructor Create; override;
      procedure SetScroll;
      procedure SetScrollRefresh;

      procedure SetRouletteScroll;
      procedure SetChessboardScroll;
      procedure SetCarouselScroll;
      procedure SetSlotMachineScroll;
      procedure SetSlideScroll;
      procedure SetListScroll;

      procedure SetRouletteScrollRefresh;
      procedure SetChessboardScrollRefresh;
      procedure SetCarouselScrollRefresh;
      procedure SetSlotMachineScrollRefresh;
      procedure SetSlideScrollRefresh;
      procedure SetListScrollRefresh;

      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;

      function ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean; override;
      function ParseMouseRoulette(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean;
      function ParseMouseChessboard(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean;

      function Draw: boolean; override;
      function FinishedMusic: boolean;

      procedure WriteMessage(msg: UTF8String);
      procedure FadeMessage();
      procedure CloseMessage();

      procedure GenerateThumbnails();
      procedure OnShow; override;
      procedure OnShowFinish; override;
      procedure OnHide; override;
      procedure SelectNext(onlyFix: boolean = true);
      procedure SelectPrev;
      procedure SelectNextRow;
      procedure SelectPrevRow;
      procedure SelectNextListRow;
      procedure SelectPrevListRow;
      procedure SkipTo(Target: cardinal; TargetInteraction: integer = 0; VS: integer = 0);
      procedure FixSelected; //Show Wrong Song when Tabs on Fix
      procedure FixSelected2; //Show Wrong Song when Tabs on Fix
      procedure ShowCatTL(Cat: integer);// Show Cat in Top left
      procedure ShowCatTLCustom(Caption: UTF8String);// Show Custom Text in Top left
      procedure HideCatTL;// Show Cat in Tob left
      procedure Refresh;//(GiveStats: boolean); //Refresh Song Sorting
      procedure ChangeSorting(Tabs: integer; Duet: boolean; Sorting: integer);
      procedure ChangeMusic;

      function FreeListMode: boolean;

      //Party Mode
      procedure SelectRandomSong;
      procedure SetJoker;
      procedure SetStatics;
      procedure ColorizeJokers;
      //procedure PartyTimeLimit;
      function PermitCategory(ID: integer): boolean;

      //procedures for Menu
      procedure StartSong;
      procedure OpenEditor;
      procedure DoJoker(Team: integer);
      procedure SelectPlayers;

      procedure OnSongSelect;   // called when song flows movement stops at a song
      procedure OnSongDeSelect; // called before current song is deselected

      procedure UnloadDetailedCover;

      procedure SongScore;

      //Extensions
      procedure DrawExtensions;

      //Medley
      procedure StartMedley(NumSongs: integer; MinSource: TMedleySource);
      function  getVisibleMedleyArr(MinSource: TMedleySource): TVisArr;

      procedure ColorDuetNameSingers;

      procedure StopMusicPreview();

      procedure ParseInputNextHorizontal(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean);
      procedure ParseInputPrevHorizontal(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean);

      procedure ParseInputNextVertical(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean);
      procedure ParseInputPrevVertical(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean);

      procedure ResetScrollList;
  end;

var
   ThumbnailsGenerated: boolean; //true if Thumbnails were already genearated during application run
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
  UUnicodeUtils, UMenuStatic;

const
  MAX_TIME = 30;
  MAX_MESSAGE = 3;
  MAX_TIME_MOUSE_SELECT = 800;

// ***** Public methods ****** //
function TScreenSong.FreeListMode: boolean;
begin
  if ((Mode = smNormal) or (Mode = smPartyTournament) or (Mode = smPartyFree) or (Mode = smJukebox)) then
    Result := true
  else
    Result := false;
end;

//Show Wrong Song when Tabs on Fix
procedure TScreenSong.FixSelected;
var
  I, I2: integer;
begin
  if (CatSongs.VisibleSongs > 0) then
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
  if (CatSongs.VisibleSongs > 0) then
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

procedure TScreenSong.ResetScrollList();
begin

  if (TSongMenuMode(Ini.SongMenu) = smList) then
  begin
    ListFirstVisibleSongIndex := 0;
    ListMinLine := 0;
    ListLastMinLine := 0;

    SetScrollRefresh;
  end;

end;

procedure TScreenSong.ParseInputNextHorizontal(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean);
var
  SDL_ModState: word;
begin
  CloseMessage();

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);

  if (Songs.SongList.Count > 0) and (FreeListMode) then
  begin
    if (SDL_ModState = KMOD_LCTRL) and (High(DLLMan.Websites) >= 0) then
    begin
      if (Ini.ShowWebScore < High(DLLMan.Websites)) then
        Ini.ShowWebScore := Ini.ShowWebScore + 1
      else
        Ini.ShowWebScore := 0;
      Ini.SaveShowWebScore;
      SongScore;
    end
    else
    begin
      if (TSongMenuMode(Ini.SongMenu) <> smList) then
      begin
        AudioPlayback.PlaySound(SoundLib.Change);
        SelectNext(false);
        SetScrollRefresh;
      end
      else
      begin
        // list change row
        SelectNextListRow;
        SetScrollRefresh;
      end;
    end;
  end;
end;

procedure TScreenSong.ParseInputPrevHorizontal(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean);
var
  SDL_ModState: word;
begin
  CloseMessage();

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);

  if (Songs.SongList.Count > 0) and (FreeListMode) then
  begin
    if (SDL_ModState = KMOD_LCTRL) and (High(DLLMan.Websites) >= 0) then
    begin
      if (Ini.ShowWebScore > 0) then
        Ini.ShowWebScore := Ini.ShowWebScore - 1
      else
        Ini.ShowWebScore := High(DLLMan.Websites);
      Ini.SaveShowWebScore;
      SongScore;
    end
    else
    begin
      if (TSongMenuMode(Ini.SongMenu) <> smList) then
      begin
        AudioPlayback.PlaySound(SoundLib.Change);
        SelectPrev;
        SetScrollRefresh;
      end
      else
      begin
        // list change row
        SelectPrevListRow;
        SetScrollRefresh;
      end;
    end;
  end;
end;

procedure TScreenSong.ParseInputNextVertical(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean);
var
  I: integer;
begin
  CloseMessage();

  if (FreeListMode) and not (TSongMenuMode(Ini.SongMenu) in [smList]) then
  begin
    //Only Change Cat when not in Playlist or Search Mode
    if (CatSongs.CatNumShow > -2) then
    begin
      if (TSongMenuMode(Ini.SongMenu) <> smChessboard) and (TSongMenuMode(Ini.SongMenu) <> smMosaic) then
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
        //Cat Change Hack End}
      end
      else
      begin
        // chessboard change row
        SelectNextRow;
        SetScrollRefresh;
      end;

      ResetScrollList;

    end
    else
    begin
      if (TSongMenuMode(Ini.SongMenu) in [smChessboard, smMosaic]) then
      begin
        // chessboard change row
        SelectNextRow;
        SetScrollRefresh;
      end;
    end;
  end;
end;

procedure TScreenSong.ParseInputPrevVertical(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean);
var
  I, I2: integer;
begin
  CloseMessage();

  if (FreeListMode) and not (TSongMenuMode(Ini.SongMenu) in [smList]) then
  begin
    //Only Change Cat when not in Playlist or Search Mode
    if (CatSongs.CatNumShow > -2) then
    begin
      if (TSongMenuMode(Ini.SongMenu) <> smChessboard) and (TSongMenuMode(Ini.SongMenu) <> smMosaic) then
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
        //Cat Change Hack End}
      end
      else
      begin
        // chessboard change row
        SelectPrevRow;
        SetScrollRefresh;
      end;

      ResetScrollList;

    end
    else
    begin
      if (TSongMenuMode(Ini.SongMenu) in [smChessboard, smMosaic]) then
      begin
        // chessboard change row
        SelectPrevRow;
        SetScrollRefresh;
      end;
    end;
  end;

end;

// Method for input parsing. If false is returned, GetNextWindow
// should be checked to know the next window to load;
function TScreenSong.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  I:      integer;
  I2:     integer;
  SDL_ModState:  word;
  UpperLetter: UCS4Char;
  TempStr: UTF8String;
  VerifySong, WebList: string;
  Fix: boolean;
  VS: integer;
begin
  Result := true;

  VS := CatSongs.VisibleSongs;

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
    if ((SDL_ModState and KMOD_LALT <> 0) and (FreeListMode)) then
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
                SkipTo(CatSongs.VisibleIndex((I + Interaction) mod I2), (I + Interaction) mod I2, VS);

                AudioPlayback.PlaySound(SoundLib.Change);

                SetScrollRefresh;
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
                SkipTo(CatSongs.VisibleIndex((I + Interaction) mod I2), (I + Interaction) mod I2, VS);

                AudioPlayback.PlaySound(SoundLib.Change);

                SetScrollRefresh;

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

      Ord('K'):
        begin
          //AudioPlayback.AddSoundEffect();
          //Exit;
        end;

      Ord('F'):
        begin
          if (Mode = smNormal) and (SDL_ModState = KMOD_LSHIFT) and MakeMedley then
          begin
            if Length(PlaylistMedley.Song)>0 then
            begin
              SetLength(PlaylistMedley.Song, Length(PlaylistMedley.Song)-1);
              PlaylistMedley.NumMedleySongs := Length(PlaylistMedley.Song);
            end;

            if Length(PlaylistMedley.Song)=0 then
              MakeMedley := false;
          end else if (Mode = smNormal) and (CatSongs.Song[Interaction].Medley.Source>=msCalculated) and
            (Length(getVisibleMedleyArr(msCalculated)) > 0) then
          begin
            MakeMedley := true;
            StartMedley(99, msCalculated);
          end;
        end;

      Ord('M'): //Show SongMenu
        begin
          if (Songs.SongList.Count > 0) then
          begin

            if not(MakeMedley) and (FreeListMode) and (Mode <> smPartyFree) and (Mode <> smPartyTournament) then
            begin
              if (not CatSongs.Song[Interaction].Main) then // clicked on Song
              begin
                if CatSongs.CatNumShow = -3 then
                begin
                  ScreenSongMenu.OnShow;

                  if (ScreenSong.Mode = smJukebox) then
                    ScreenSongMenu.MenuShow(SM_Jukebox)
                  else
                    ScreenSongMenu.MenuShow(SM_Playlist);
                end
                else
                begin
                  ScreenSongMenu.OnShow;

                  if (ScreenSong.Mode = smJukebox) then
                    ScreenSongMenu.MenuShow(SM_Jukebox)
                  else
                    ScreenSongMenu.MenuShow(SM_Main);
                end;
              end
              else
              begin
                ScreenSongMenu.OnShow;
                if (ScreenSong.Mode = smJukebox) then
                  ScreenSongMenu.MenuShow(SM_Jukebox)
                else
                  ScreenSongMenu.MenuShow(SM_Playlist_Load);
              end;
            end //Party Mode -> Show Party Menu
            else
            begin

              if (MakeMedley) then
              begin
                ScreenSongMenu.MenuShow(SM_Medley)
              end
              else
              begin
                ScreenSongMenu.OnShow;
                if (Mode <> smPartyFree) and (Mode <> smPartyTournament) then
                  ScreenSongMenu.MenuShow(SM_Party_Main)
                else
                  ScreenSongMenu.MenuShow(SM_Party_Free_Main);
              end;
            end;
          end;
          Exit;
        end;

      Ord('P'): //Show Playlist Menu
        begin
          if (Songs.SongList.Count > 0) and (FreeListMode) then
          begin
            ScreenSongMenu.OnShow;
            ScreenSongMenu.MenuShow(SM_Playlist_Load);
          end;
          Exit;
        end;

      Ord('J'): //Show Jumpto Menu
        begin
          if (Songs.SongList.Count > 0) and (FreeListMode) then
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

      Ord('S'):
        begin
          if (SDL_ModState = KMOD_LSHIFT) and not MakeMedley and
            (CatSongs.Song[Interaction].Medley.Source>=msCalculated) and
            (Mode = smNormal)then
            StartMedley(0, msCalculated)
          else if (CatSongs.Song[Interaction].Medley.Source>=msTag) and not MakeMedley and
            (Mode = smNormal) then
            StartMedley(0, msTag);
        end;

      Ord('D'):
        begin
          if (Mode = smNormal) and (SDL_ModState = KMOD_LSHIFT) and not MakeMedley and
            (length(getVisibleMedleyArr(msCalculated))>0) then
            StartMedley(5, msCalculated)
          else if (Mode = smNormal) and (Length(getVisibleMedleyArr(msTag)) > 0)
            and not MakeMedley then
            StartMedley(5, msTag);
        end;

      Ord('R'):
        begin
          if (Songs.SongList.Count > 0) and
             (FreeListMode) then
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

            SetScrollRefresh;
          end;
          Exit;
        end;

      Ord('W'):
        begin

          if not CatSongs.Song[Interaction].Main then
          begin
            WebList := '';

            for I:= 0 to High(Database.NetworkUser) do
            begin
              DllMan.LoadWebsite(I);
              VerifySong := DllMan.WebsiteVerifySong(CatSongs.Song[Interaction].MD5);

              if (VerifySong = 'OK_SONG') then
                WebList := Database.NetworkUser[I].Website + #13
            end;

            if (WebList <> '') then
              ScreenPopupInfo.ShowPopup(Format(Language.Translate('WEBSITE_EXIST_SONG'), [WebList]))
            else
              ScreenPopupError.ShowPopup(Language.Translate('WEBSITE_NOT_EXIST_SONG'));
          end;
        end;

    end; // normal keys

    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          CloseMessage();

          if (FreeListMode) then
          begin
            Fix := true;

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

              if not(TSongMenuMode(Ini.SongMenu) in [smChessboard, smCarousel, smSlide, smList, smMosaic]) then
              begin
                if (I <= 1) then
                  Interaction := High(CatSongs.Song)
                else
                  Interaction := I - 1;
              end
              else
              begin
                Interaction := I - 1;

                if (Interaction < 0) then
                begin
                  Interaction := 0;
                  Fix := false;
                end;
              end;

              //Stop Music
              //StopMusicPreview();
              OnSongDeSelect;

              CatSongs.ShowCategoryList;

              //Show Cat in Top Left Mod
              HideCatTL;

              //Show Wrong Song when Tabs on Fix
              if (Fix) then
              begin
                SelectNext;
                FixSelected;
              end;

              ChessboardMinLine := MainChessboardMinLine;
              ListMinLine := MainListMinLine;
              ListFirstVisibleSongIndex := MainListFirstVisibleSongIndex;

              if (TSongMenuMode(Ini.SongMenu) in [smList]) then
                ListLastMinLine := -1;

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

                //if (Mode = smPartyTournament) then
                //  CurrentPartyTime := MAX_TIME - StrToInt(Text[TextPartyTime].Text);

                case Mode of
                  smPartyFree: FadeTo(@ScreenPartyNewRound);
                  smJukebox: FadeTo(@ScreenJukeboxPlaylist);
                  smPartyTournament: FadeTo(@ScreenPartyTournamentRounds);
                  else FadeTo(@ScreenMain);
                end;

              end;

            end;
          end
          //When in party Mode then Ask before Close
          else if (Mode = smPartyClassic) then
          begin
            AudioPlayback.PlaySound(SoundLib.Back);
            CheckFadeTo(@ScreenMain,'MSG_END_PARTY');
          end;

          if (TSongMenuMode(Ini.SongMenu) in [smChessboard, smMosaic, smList]) then
            SetScrollRefresh;

        end;
      SDLK_RETURN:
        begin
          CloseMessage();

          LastSelectTime := SDL_GetTicks;

          if (Songs.SongList.Count > 0) then
          begin
            if CatSongs.Song[Interaction].Main then
            begin // clicked on Category Button
              MainChessboardMinLine := ChessboardMinLine;
              ChessboardMinLine := 0;

              MainListMinLine := ListMinLine;
              ListMinLine := 0;

              ListFirstVisibleSongIndex := 0;

              //Show Cat in Top Left Mod
              ShowCatTL (Interaction);

              CatSongs.ClickCategoryButton(Interaction);

              //Show Wrong Song when Tabs on Fix
              SelectNext;
              FixSelected;

              SetScrollRefresh;

              ResetScrollList;
            end
            else
            begin // clicked on song

              // Duets Warning
              if (CatSongs.Song[Interaction].isDuet) and (Mode <> smNormal) then
              begin
                ScreenPopupError.ShowPopup(Language.Translate('SING_ERROR_DUET_MODE_PARTY'));
                Exit;
              end;

              StopVideoPreview;
              StopMusicPreview;

              if (Mode = smNormal) then //Normal Mode -> Start Song
              begin
                if MakeMedley then
                begin
                  Mode := smMedley;

                  //Do the Action that is specified in Ini
                  case Ini.OnSongClick of
                    0: FadeTo(@ScreenSing);
                    1: SelectPlayers;
                    2: FadeTo(@ScreenSing);
                  end;
                end
                else
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
                end;
              end
              else
                if (Mode = smPartyClassic) then //PartyMode -> Show Menu
                begin
                  if (Ini.PartyPopup = 1) then
                    ScreenSongMenu.MenuShow(SM_Party_Main)
                  else
                    Party.CallAfterSongSelect;
                end;

                if (Mode = smPartyFree) then
                begin
                  Party.CallAfterSongSelect;
                end;

                if (Mode = smPartyTournament) then
                begin
                  ScreenSong.StartSong;
                end;

                if (Mode = smJukebox) then
                begin
                  if (Length(ScreenJukebox.JukeboxSongsList) > 0) then
                  begin
                    ScreenJukebox.CurrentSongID := ScreenJukebox.JukeboxVisibleSongs[0];
                    FadeTo(@ScreenJukebox);
                  end
                  else
                    ScreenPopupError.ShowPopup(Language.Translate('PARTY_MODE_JUKEBOX_NO_SONGS'));
                end;
              end;
          end;
        end;

      SDLK_DOWN:
        begin
          LastSelectTime := SDL_GetTicks;

          if (TSongMenuMode(Ini.SongMenu) in [smSlotMachine, smList]) then
            ParseInputNextHorizontal(PressedKey, CharCode, PressedDown)
          else
            ParseInputNextVertical(PressedKey, CharCode, PressedDown);
        end;

      SDLK_UP:
        begin
          LastSelectTime := SDL_GetTicks;

          if (TSongMenuMode(Ini.SongMenu) in [smSlotMachine, smList]) then
            ParseInputPrevHorizontal(PressedKey, CharCode, PressedDown)
          else
            ParseInputPrevVertical(PressedKey, CharCode, PressedDown);
        end;

      SDLK_RIGHT:
        begin
          LastSelectTime := SDL_GetTicks;

          if (TSongMenuMode(Ini.SongMenu) in [smSlotMachine, smList]) then
            ParseInputNextVertical(PressedKey, CharCode, PressedDown)
          else
            ParseInputNextHorizontal(PressedKey, CharCode, PressedDown);
        end;

      SDLK_LEFT:
        begin
          LastSelectTime := SDL_GetTicks;

          if (TSongMenuMode(Ini.SongMenu) in [smSlotMachine, smList]) then
            ParseInputPrevVertical(PressedKey, CharCode, PressedDown)
          else
            ParseInputPrevHorizontal(PressedKey, CharCode, PressedDown);
        end;
      SDLK_SPACE:
        begin
          if (Mode = smJukebox) and (not CatSongs.Song[Interaction].Main) then
            ScreenJukebox.AddSongToJukeboxList(Interaction);

          if (Mode = smNormal) and (CatSongs.Song[Interaction].isDuet) then
            DuetChange := not DuetChange;
        end;
      SDLK_1:
        begin //Joker
          if (SDL_ModState = KMOD_LSHIFT) then
          begin
          end
          else
          begin
            if (SDL_ModState = KMOD_LCTRL) then
            begin
            end
            else
              DoJoker(0);
            end;
        end;

      SDLK_2:
        begin //Joker
          if (SDL_ModState = KMOD_LSHIFT) then
          begin
          end
          else
          begin
            if (SDL_ModState = KMOD_LCTRL) then
            begin
            end
            else
              DoJoker(1);
          end;

        end;

      SDLK_3:
        begin //Joker
          if (SDL_ModState = KMOD_LSHIFT) then
          begin
          end
          else
          begin
            if (SDL_ModState = KMOD_LCTRL) then
            begin
            end
            else
              DoJoker(2);
          end;
      end;
    end;
  end; // if (PressedDown)
end;

function TScreenSong.ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean;
begin

  // transfer mousecords to the 800x600 raster we use to draw
  X := Round((X / (ScreenW / Screens)) * RenderW);
  if (X > RenderW) then
    X := X - RenderW;
  Y := Round((Y / ScreenH) * RenderH);

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

    case TSongMenuMode(Ini.SongMenu) of
      smChessboard: ParseMouseChessboard(MouseButton, BtnDown, X, Y);
      smMosaic: ParseMouseChessboard(MouseButton, BtnDown, X, Y);
      else
        ParseMouseRoulette(MouseButton, BtnDown, X, Y);
    end;

  end;
end;

function TScreenSong.ParseMouseChessboard(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean;
var
  B: integer;
begin
  Result := true;

  if (BtnDown) then
  begin
    //if RightMbESC is set, send ESC keypress
    if RightMbESC and (MouseButton = SDL_BUTTON_RIGHT) then
      Result:=ParseInput(SDLK_ESCAPE, 0, true)

    //song scrolling with mousewheel
    else if (MouseButton = SDL_BUTTON_WHEELDOWN) then
      ParseInput(SDLK_DOWN, 0, true)

    else if (MouseButton = SDL_BUTTON_WHEELUP) then
      ParseInput(SDLK_UP, 0, true)

    else
    begin

      // click cover
      for B := 0 to High(Button) do
      begin
        if (Button[B].Visible) then
        begin
          if InRegion(X, Y, Button[B].GetMouseOverArea) then
          begin
            ParseInput(SDLK_RETURN, 0, true)
          end;
        end;
      end;
    end;

  end
  else
  begin

    // hover cover
    for B := 0 to High(Button) do
    begin
      if (Button[B].Visible) then
      begin
        if InRegion(X, Y, Button[B].GetMouseOverArea) then
        begin
          if (Interaction <> B) then
          begin
            // play current hover
            isScrolling := false;
            OnSongDeSelect;
            Interaction := B;
            SetScrollRefresh;
            LastSelectMouse := SDL_GetTicks;
            LastSelectTime := SDL_GetTicks;
          end;
        end;
      end;
    end;

  end;
end;

function TScreenSong.ParseMouseRoulette(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean;
var
    I, J: Integer;
    Btn: Integer;
begin
  Result := true;

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
      if (CatSongs.VisibleSongs =3) or (CatSongs.VisibleSongs = 4) then
      begin
        // select the second visible button left from selected
        I := 0;
        Btn := Interaction;
        while (I < 1) do
        begin
          Dec(Btn);
          if (Btn < 0) then
            Btn := High(CatSongs.Song);

          if (CatSongs.Song[Btn].Visible) then
            Inc(I);
        end;

        // test the 3 front buttons for click
        for I := 0 to 2 do
        begin
          if InRegion(X, Y, Button[Btn].GetMouseOverArea) then
          begin
            // song cover clicked
            if (I = 1) then
            begin // Selected Song clicked -> start singing
              ParseInput(SDLK_RETURN, 0, true);
            end
            else
            begin // one of the other 4 covers in the front clicked -> select it
              J := I - 1;
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
      else if (CatSongs.VisibleSongs > 4) then
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

procedure TScreenSong.ColorizeJokers;
var
  StartJoker, I, J: integer;
  Col: TRGB;
begin

  StartJoker := StaticTeam1Joker1;

  for I:= 0 to 2 do
  begin
    Col := GetPlayerColor(Ini.SingColor[I]);

    for J := StartJoker + I * 5 to (StartJoker + I * 5 - 1) + 5  do
    begin
      Statics[J].Texture.ColR := Col.R;
      Statics[J].Texture.ColG := Col.G;
      Statics[J].Texture.ColB := Col.B;
    end;
  end;

end;

constructor TScreenSong.Create;
var
  I, Num, Padding: integer;
  TextArtistY, TextTitleY, TextYearY, StaticMedCY,
  StaticMedMY, StaticVideoY, StaticDuetY: integer;
  StaticY: real;
begin
  inherited Create;

  LoadFromTheme(Theme.Song);

  TextArtist := AddText(Theme.Song.TextArtist);
  TextTitle  := AddText(Theme.Song.TextTitle);
  TextNumber := AddText(Theme.Song.TextNumber);
  TextYear   := AddText(Theme.Song.TextYear);

  //Show Cat in Top Left mod
  TextCat := AddText(Theme.Song.TextCat);
  StaticCat :=  AddStatic(Theme.Song.StaticCat);

  //Show Video Icon Mod
  VideoIcon := AddStatic(Theme.Song.VideoIcon);

  //Meldey Icons
  MedleyIcon := AddStatic(Theme.Song.MedleyIcon);
  CalcMedleyIcon := AddStatic(Theme.Song.CalculatedMedleyIcon);

  //Duet Icon
  DuetIcon := AddStatic(Theme.Song.DuetIcon);

  //Show Scores
  TextScore       := AddText(Theme.Song.TextScore);
  TextMaxScore    := AddText(Theme.Song.TextMaxScore);
  TextMediaScore  := AddText(Theme.Song.TextMediaScore);
  TextMaxScore2   := AddText(Theme.Song.TextMaxScore2);
  TextMediaScore2 := AddText(Theme.Song.TextMediaScore2);
  TextScoreUser   := AddText(Theme.Song.TextScoreUser);
  TextMaxScoreLocal   := AddText(Theme.Song.TextMaxScoreLocal);
  TextMediaScoreLocal := AddText(Theme.Song.TextMediaScoreLocal);
  TextScoreUserLocal  := AddText(Theme.Song.TextScoreUserLocal);

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

  //TextPartyTime := AddText(Theme.Song.TextPartyTime);

  // Song List
  //Songs.LoadSongList; // moved to the UltraStar unit

  if (TSortingType(Ini.Sorting) <> sPlaylist) then
  begin
    CatSongs.Refresh;
    GenerateThumbnails();
  end;

  // Randomize Patch
  Randomize;

  Equalizer := Tms_Equalizer.Create(AudioPlayback, Theme.Song.Equalizer);

  PreviewOpened := -1;
  isScrolling := false;

  fCurrentVideo := nil;

  // Info Message
  InfoMessageBG := AddStatic(Theme.Song.InfoMessageBG);
  InfoMessageText := AddText(Theme.Song.InfoMessageText);

  // Duet Names Singers
  Static4PlayersDuetSingerP3 := AddStatic(Theme.Song.Static4PlayersDuetSingerP3);
  Static4PlayersDuetSingerP4 := AddStatic(Theme.Song.Static4PlayersDuetSingerP4);

  Static6PlayersDuetSingerP4 := AddStatic(Theme.Song.Static6PlayersDuetSingerP4);
  Static6PlayersDuetSingerP5 := AddStatic(Theme.Song.Static6PlayersDuetSingerP5);
  Static6PlayersDuetSingerP6 := AddStatic(Theme.Song.Static6PlayersDuetSingerP6);

  Text2PlayersDuetSingerP1 := AddText(Theme.Song.Text2PlayersDuetSingerP1);
  Text2PlayersDuetSingerP2 := AddText(Theme.Song.Text2PlayersDuetSingerP2);
  Static2PlayersDuetSingerP1 := AddStatic(Theme.Song.Static2PlayersDuetSingerP1);
  Static2PlayersDuetSingerP2 := AddStatic(Theme.Song.Static2PlayersDuetSingerP2);

  Text3PlayersDuetSingerP1 := AddText(Theme.Song.Text3PlayersDuetSingerP1);
  Text3PlayersDuetSingerP2 := AddText(Theme.Song.Text3PlayersDuetSingerP2);
  Text3PlayersDuetSingerP3 := AddText(Theme.Song.Text3PlayersDuetSingerP3);
  Static3PlayersDuetSingerP1 := AddStatic(Theme.Song.Static3PlayersDuetSingerP1);
  Static3PlayersDuetSingerP2 := AddStatic(Theme.Song.Static3PlayersDuetSingerP2);
  Static3PlayersDuetSingerP3 := AddStatic(Theme.Song.Static3PlayersDuetSingerP3);

  // Medley Playlist
  SetLength(TextMedleyArtist, Theme.Song.TextMedleyMax);
  SetLength(TextMedleyTitle, Theme.Song.TextMedleyMax);
  SetLength(TextMedleyNumber, Theme.Song.TextMedleyMax);
  SetLength(StaticMedley, Theme.Song.TextMedleyMax);

  for I := 0 to Theme.Song.TextMedleyMax - 1 do
  begin
    TextMedleyArtist[I] := AddText(Theme.Song.TextArtistMedley[I]);
    TextMedleyTitle[I] := AddText(Theme.Song.TextTitleMedley[I]);
    TextMedleyNumber[I] := AddText(Theme.Song.TextNumberMedley[I]);
    StaticMedley[I] := AddStatic(Theme.Song.StaticMedley[I]);
  end;

  StaticActual := AddStatic(Theme.Song.Cover.SelectX, Theme.Song.Cover.SelectY,
                            Theme.Song.Cover.SelectW, Theme.Song.Cover.SelectH, PATH_NONE);

  Num := Theme.Song.ListCover.Rows;

  SetLength(StaticList, Num);
  for I := 0 to Num - 1 do
  begin
    StaticY := Theme.Song.ListCover.Y + I * (Theme.Song.ListCover.H + Theme.Song.ListCover.Padding);

    StaticList[I] := AddListItem(
      Theme.Song.ListCover.X,
      StaticY,
      Theme.Song.ListCover.W,
      Theme.Song.ListCover.H,
      Theme.Song.ListCover.Z,
      Theme.Song.ListCover.ColR,
      Theme.Song.ListCover.ColG,
      Theme.Song.ListCover.ColB,
      Theme.Song.ListCover.DColR,
      Theme.Song.ListCover.DColG,
      Theme.Song.ListCover.DColB,
      Skin.GetTextureFileName(Theme.Song.ListCover.Tex),
      Skin.GetTextureFileName(Theme.Song.ListCover.DTex),
      Theme.Song.ListCover.Typ,
      Theme.Song.ListCover.Reflection,
      Theme.Song.ListCover.ReflectionSpacing);
  end;

  SetLength(ListTextArtist, Num);
  SetLength(ListTextTitle, Num);
  SetLength(ListTextYear, Num);
  SetLength(ListVideoIcon, Num);
  SetLength(ListMedleyIcon, Num);
  SetLength(ListCalcMedleyIcon, Num);
  SetLength(ListDuetIcon, Num);

  TextArtistY := Theme.Song.TextArtist.Y;
  TextTitleY := Theme.Song.TextTitle.Y;
  TextYearY := Theme.Song.TextYear.Y;

  StaticVideoY := Theme.Song.VideoIcon.Y;
  StaticMedMY := Theme.Song.MedleyIcon.Y;
  StaticMedCY := Theme.Song.CalculatedMedleyIcon.Y;
  StaticDuetY := Theme.Song.DuetIcon.Y;

  for I := 0 to Num - 1 do
  begin
    Padding := I * (Theme.Song.ListCover.H + Theme.Song.ListCover.Padding);

    Theme.Song.TextArtist.Y  := TextArtistY + Padding;
    ListTextArtist[I] := AddText(Theme.Song.TextArtist);

    Theme.Song.TextTitle.Y  := TextTitleY + Padding;
    ListTextTitle[I]  := AddText(Theme.Song.TextTitle);

    Theme.Song.TextYear.Y  := TextYearY + Padding;
    ListTextYear[I]   := AddText(Theme.Song.TextYear);

    Theme.Song.VideoIcon.Y  := StaticVideoY + Padding;
    ListVideoIcon[I]  := AddStatic(Theme.Song.VideoIcon);

    Theme.Song.MedleyIcon.Y  := StaticMedMY + Padding;
    ListMedleyIcon[I] := AddStatic(Theme.Song.MedleyIcon);

    Theme.Song.CalculatedMedleyIcon.Y  := StaticMedCY + Padding;
    ListCalcMedleyIcon[I] := AddStatic(Theme.Song.CalculatedMedleyIcon);

    Theme.Song.DuetIcon.Y  := StaticDuetY + Padding;
    ListDuetIcon[I] := AddStatic(Theme.Song.DuetIcon);
  end;

  MainChessboardMinLine := 0;
  MainListMinLine := 0;

  ChessboardMinLine := 0;
  ListMinLine := 0;

  ListFirstVisibleSongIndex := 0;

  LastSelectMouse := 0;
  LastSelectTime := 0;

end;

procedure TScreenSong.ColorDuetNameSingers();
var
  Col: TRGB;
begin
  if (PlayersPlay = 1) then
  begin
    Statics[Static2PlayersDuetSingerP1].Texture.ColR := ColPlayer[0].R;
    Statics[Static2PlayersDuetSingerP1].Texture.ColG := ColPlayer[0].G;
    Statics[Static2PlayersDuetSingerP1].Texture.ColB := ColPlayer[0].B;

    Col := GetPlayerLightColor(Ini.SingColor[0]);
    Statics[Static2PlayersDuetSingerP2].Texture.ColR := Col.R;
    Statics[Static2PlayersDuetSingerP2].Texture.ColG := Col.G;
    Statics[Static2PlayersDuetSingerP2].Texture.ColB := Col.B;
  end;

  if (PlayersPlay = 2) then
  begin
    Statics[Static2PlayersDuetSingerP1].Texture.ColR := ColPlayer[0].R;
    Statics[Static2PlayersDuetSingerP1].Texture.ColG := ColPlayer[0].G;
    Statics[Static2PlayersDuetSingerP1].Texture.ColB := ColPlayer[0].B;

    Statics[Static2PlayersDuetSingerP2].Texture.ColR := ColPlayer[1].R;
    Statics[Static2PlayersDuetSingerP2].Texture.ColG := ColPlayer[1].G;
    Statics[Static2PlayersDuetSingerP2].Texture.ColB := ColPlayer[1].B;
  end;

  if (PlayersPlay = 3) then
  begin
    Statics[Static3PlayersDuetSingerP1].Texture.ColR := ColPlayer[0].R;
    Statics[Static3PlayersDuetSingerP1].Texture.ColG := ColPlayer[0].G;
    Statics[Static3PlayersDuetSingerP1].Texture.ColB := ColPlayer[0].B;

    Statics[Static3PlayersDuetSingerP2].Texture.ColR := ColPlayer[1].R;
    Statics[Static3PlayersDuetSingerP2].Texture.ColG := ColPlayer[1].G;
    Statics[Static3PlayersDuetSingerP2].Texture.ColB := ColPlayer[1].B;

    Statics[Static3PlayersDuetSingerP3].Texture.ColR := ColPlayer[2].R;
    Statics[Static3PlayersDuetSingerP3].Texture.ColG := ColPlayer[2].G;
    Statics[Static3PlayersDuetSingerP3].Texture.ColB := ColPlayer[2].B;
  end;

  if (PlayersPlay = 4) then
  begin
    if (Screens = 1) then
    begin
      Statics[Static2PlayersDuetSingerP1].Texture.ColR := ColPlayer[0].R;
      Statics[Static2PlayersDuetSingerP1].Texture.ColG := ColPlayer[0].G;
      Statics[Static2PlayersDuetSingerP1].Texture.ColB := ColPlayer[0].B;

      Statics[Static2PlayersDuetSingerP2].Texture.ColR := ColPlayer[1].R;
      Statics[Static2PlayersDuetSingerP2].Texture.ColG := ColPlayer[1].G;
      Statics[Static2PlayersDuetSingerP2].Texture.ColB := ColPlayer[1].B;

      Statics[Static4PlayersDuetSingerP3].Texture.ColR := ColPlayer[2].R;
      Statics[Static4PlayersDuetSingerP3].Texture.ColG := ColPlayer[2].G;
      Statics[Static4PlayersDuetSingerP3].Texture.ColB := ColPlayer[2].B;

      Statics[Static4PlayersDuetSingerP4].Texture.ColR := ColPlayer[3].R;
      Statics[Static4PlayersDuetSingerP4].Texture.ColG := ColPlayer[3].G;
      Statics[Static4PlayersDuetSingerP4].Texture.ColB := ColPlayer[3].B;
    end
    else
    begin
      if (ScreenAct = 1) then
      begin
        Statics[Static2PlayersDuetSingerP1].Texture.ColR := ColPlayer[0].R;
        Statics[Static2PlayersDuetSingerP1].Texture.ColG := ColPlayer[0].G;
        Statics[Static2PlayersDuetSingerP1].Texture.ColB := ColPlayer[0].B;

        Statics[Static2PlayersDuetSingerP2].Texture.ColR := ColPlayer[1].R;
        Statics[Static2PlayersDuetSingerP2].Texture.ColG := ColPlayer[1].G;
        Statics[Static2PlayersDuetSingerP2].Texture.ColB := ColPlayer[1].B;
      end;

      if (ScreenAct = 2) then
      begin
        Statics[Static2PlayersDuetSingerP1].Texture.ColR := ColPlayer[2].R;
        Statics[Static2PlayersDuetSingerP1].Texture.ColG := ColPlayer[2].G;
        Statics[Static2PlayersDuetSingerP1].Texture.ColB := ColPlayer[2].B;

        Statics[Static2PlayersDuetSingerP2].Texture.ColR := ColPlayer[3].R;
        Statics[Static2PlayersDuetSingerP2].Texture.ColG := ColPlayer[3].G;
        Statics[Static2PlayersDuetSingerP2].Texture.ColB := ColPlayer[3].B;
      end;
    end;
  end;

  if (PlayersPlay = 6) then
  begin
    if (Screens = 1) then
    begin
        Statics[Static3PlayersDuetSingerP1].Texture.ColR := ColPlayer[0].R;
        Statics[Static3PlayersDuetSingerP1].Texture.ColG := ColPlayer[0].G;
        Statics[Static3PlayersDuetSingerP1].Texture.ColB := ColPlayer[0].B;

        Statics[Static3PlayersDuetSingerP2].Texture.ColR := ColPlayer[1].R;
        Statics[Static3PlayersDuetSingerP2].Texture.ColG := ColPlayer[1].G;
        Statics[Static3PlayersDuetSingerP2].Texture.ColB := ColPlayer[1].B;

        Statics[Static3PlayersDuetSingerP3].Texture.ColR := ColPlayer[2].R;
        Statics[Static3PlayersDuetSingerP3].Texture.ColG := ColPlayer[2].G;
        Statics[Static3PlayersDuetSingerP3].Texture.ColB := ColPlayer[2].B;

        Statics[Static6PlayersDuetSingerP4].Texture.ColR := ColPlayer[3].R;
        Statics[Static6PlayersDuetSingerP4].Texture.ColG := ColPlayer[3].G;
        Statics[Static6PlayersDuetSingerP4].Texture.ColB := ColPlayer[3].B;

        Statics[Static6PlayersDuetSingerP5].Texture.ColR := ColPlayer[4].R;
        Statics[Static6PlayersDuetSingerP5].Texture.ColG := ColPlayer[4].G;
        Statics[Static6PlayersDuetSingerP5].Texture.ColB := ColPlayer[4].B;

        Statics[Static6PlayersDuetSingerP6].Texture.ColR := ColPlayer[5].R;
        Statics[Static6PlayersDuetSingerP6].Texture.ColG := ColPlayer[5].G;
        Statics[Static6PlayersDuetSingerP6].Texture.ColB := ColPlayer[5].B;
    end
    else
    begin
      if (ScreenAct = 1) then
      begin
        Statics[Static3PlayersDuetSingerP1].Texture.ColR := ColPlayer[0].R;
        Statics[Static3PlayersDuetSingerP1].Texture.ColG := ColPlayer[0].G;
        Statics[Static3PlayersDuetSingerP1].Texture.ColB := ColPlayer[0].B;

        Statics[Static3PlayersDuetSingerP2].Texture.ColR := ColPlayer[1].R;
        Statics[Static3PlayersDuetSingerP2].Texture.ColG := ColPlayer[1].G;
        Statics[Static3PlayersDuetSingerP2].Texture.ColB := ColPlayer[1].B;

        Statics[Static3PlayersDuetSingerP3].Texture.ColR := ColPlayer[2].R;
        Statics[Static3PlayersDuetSingerP3].Texture.ColG := ColPlayer[2].G;
        Statics[Static3PlayersDuetSingerP3].Texture.ColB := ColPlayer[2].B;
      end;

      if (ScreenAct = 2) then
      begin
        Statics[Static3PlayersDuetSingerP1].Texture.ColR := ColPlayer[3].R;
        Statics[Static3PlayersDuetSingerP1].Texture.ColG := ColPlayer[3].G;
        Statics[Static3PlayersDuetSingerP1].Texture.ColB := ColPlayer[3].B;

        Statics[Static3PlayersDuetSingerP2].Texture.ColR := ColPlayer[4].R;
        Statics[Static3PlayersDuetSingerP2].Texture.ColG := ColPlayer[4].G;
        Statics[Static3PlayersDuetSingerP2].Texture.ColB := ColPlayer[4].B;

        Statics[Static3PlayersDuetSingerP3].Texture.ColR := ColPlayer[5].R;
        Statics[Static3PlayersDuetSingerP3].Texture.ColG := ColPlayer[5].G;
        Statics[Static3PlayersDuetSingerP3].Texture.ColB := ColPlayer[5].B;
      end;
    end;
  end;
end;

procedure TScreenSong.GenerateThumbnails();
var
  I: integer;
  CoverButtonIndex: integer;
  CoverButton: TButton;
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
      //Texture.AddTexture(CoverTexture, TEXTURE_TYPE_PLAIN, false);
      CoverButton.Texture := Cover.GetPreviewTexture();

      Song.CoverTex := CoverButton.Texture;  //basisbit ToDo 11.11.2015

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
    StartVideoPreview;
  end;

  // fade in detailed cover
  CoverTime := 0;

  SongIndex := -1;

  //SetScrollRefresh;
end;

{ called before current song is deselected }
procedure TScreenSong.OnSongDeSelect;
begin
  DuetChange := false;

  CoverTime := 10;
  UnLoadDetailedCover;

  StopMusicPreview();
  StopVideoPreview();
  PreviewOpened := -1;

  //SetScrollRefresh;
end;

procedure TScreenSong.SetScrollRefresh;
begin
  case TSongMenuMode(Ini.SongMenu) of
    smRoulette: SetRouletteScrollRefresh;
    smChessboard: SetChessboardScrollRefresh;
    smCarousel: SetCarouselScrollRefresh;
    smSlotMachine: SetSlotMachineScrollRefresh;
    smSlide: SetSlideScrollRefresh;
    smList: SetListScrollRefresh;
    smMosaic: SetChessboardScrollRefresh;
  end;
end;

procedure TScreenSong.SetScroll;
var
  VS, B: integer;
begin
  VS := CatSongs.VisibleSongs;
  if VS > 0 then
  begin

    case TSongMenuMode(Ini.SongMenu) of
      smRoulette: SetRouletteScroll;
      smChessboard: SetChessboardScroll;
      smCarousel: SetCarouselScroll;
      smSlotMachine: SetSlotMachineScroll;
      smSlide: SetSlideScroll;
      smList: SetListScroll;
      smMosaic: SetChessboardScroll;
    end;

    if (TSongMenuMode(Ini.SongMenu) <> smList) then
    begin
      // Set visibility of video icon
      Statics[VideoIcon].Visible := CatSongs.Song[Interaction].Video.IsSet;

      // Set visibility of medley icons
      Statics[MedleyIcon].Visible := (CatSongs.Song[Interaction].Medley.Source = msTag);
      Statics[CalcMedleyIcon].Visible := (CatSongs.Song[Interaction].Medley.Source = msCalculated);

      //Set Visibility of Duet Icon
      Statics[DuetIcon].Visible := CatSongs.Song[Interaction].isDuet;

      // Set texts
      Text[TextArtist].Text := CatSongs.Song[Interaction].Artist;
      Text[TextTitle].Text  :=  CatSongs.Song[Interaction].Title;

      if ((Ini.Tabs = 0) or (TSortingType(Ini.Sorting) <> sYear))
        and (CatSongs.Song[Interaction].Year <> 0) then
          Text[TextYear].Text  :=  InttoStr(CatSongs.Song[Interaction].Year)
      else
        Text[TextYear].Text  :=  '';
    end;

    // Duet Singers
    if (CatSongs.Song[Interaction].isDuet) then
    begin
      if (PlayersPlay = 3) or (PlayersPlay = 6) then
      begin
        Text[Text3PlayersDuetSingerP1].Visible := true;
        Text[Text3PlayersDuetSingerP2].Visible := true;
        Text[Text3PlayersDuetSingerP3].Visible := true;

        Statics[Static3PlayersDuetSingerP1].Visible := true;
        Statics[Static3PlayersDuetSingerP2].Visible := true;
        Statics[Static3PlayersDuetSingerP3].Visible := true;

        if (Screens = 1) and (PlayersPlay = 6) then
        begin
          Statics[Static6PlayersDuetSingerP4].Visible := true;
          Statics[Static6PlayersDuetSingerP5].Visible := true;
          Statics[Static6PlayersDuetSingerP6].Visible := true;
        end;
      end
      else
      begin
        Text[Text2PlayersDuetSingerP1].Visible := true;
        Text[Text2PlayersDuetSingerP2].Visible := true;

        Statics[Static2PlayersDuetSingerP1].Visible := true;
        Statics[Static2PlayersDuetSingerP2].Visible := true;

        if (Screens = 1) and (PlayersPlay = 4) then
        begin
          Statics[Static4PlayersDuetSingerP3].Visible := true;
          Statics[Static4PlayersDuetSingerP4].Visible := true;
        end;
      end;

      // Set duet texts
      if (DuetChange) then
      begin
        if (PlayersPlay = 3) or (PlayersPlay = 6) then
        begin
          if (PlayersPlay = 3) then
          begin
            Text[Text3PlayersDuetSingerP1].Text := CatSongs.Song[Interaction].DuetNames[1];
            Text[Text3PlayersDuetSingerP2].Text := CatSongs.Song[Interaction].DuetNames[0];
            Text[Text3PlayersDuetSingerP3].Text := CatSongs.Song[Interaction].DuetNames[1];
          end
          else
          begin
            if (ScreenAct = 1) then
            begin
              Text[Text3PlayersDuetSingerP1].Text := CatSongs.Song[Interaction].DuetNames[1];
              Text[Text3PlayersDuetSingerP2].Text := CatSongs.Song[Interaction].DuetNames[0];
              Text[Text3PlayersDuetSingerP3].Text := CatSongs.Song[Interaction].DuetNames[1];
            end
            else
            begin
              Text[Text3PlayersDuetSingerP1].Text := CatSongs.Song[Interaction].DuetNames[0];
              Text[Text3PlayersDuetSingerP2].Text := CatSongs.Song[Interaction].DuetNames[1];
              Text[Text3PlayersDuetSingerP3].Text := CatSongs.Song[Interaction].DuetNames[0];
            end;
          end;
        end
        else
        begin
          Text[Text2PlayersDuetSingerP1].Text := CatSongs.Song[Interaction].DuetNames[1];
          Text[Text2PlayersDuetSingerP2].Text := CatSongs.Song[Interaction].DuetNames[0];
        end;
      end
      else
      begin
        if (PlayersPlay = 3) or (PlayersPlay = 6) then
        begin
          if (PlayersPlay = 3) then
          begin
            Text[Text3PlayersDuetSingerP1].Text := CatSongs.Song[Interaction].DuetNames[0];
            Text[Text3PlayersDuetSingerP2].Text := CatSongs.Song[Interaction].DuetNames[1];
            Text[Text3PlayersDuetSingerP3].Text := CatSongs.Song[Interaction].DuetNames[0];
          end
          else
          begin
            if (ScreenAct = 1) then
            begin
              Text[Text3PlayersDuetSingerP1].Text := CatSongs.Song[Interaction].DuetNames[0];
              Text[Text3PlayersDuetSingerP2].Text := CatSongs.Song[Interaction].DuetNames[1];
              Text[Text3PlayersDuetSingerP3].Text := CatSongs.Song[Interaction].DuetNames[0];
            end
            else
            begin
              Text[Text3PlayersDuetSingerP1].Text := CatSongs.Song[Interaction].DuetNames[1];
              Text[Text3PlayersDuetSingerP2].Text := CatSongs.Song[Interaction].DuetNames[0];
              Text[Text3PlayersDuetSingerP3].Text := CatSongs.Song[Interaction].DuetNames[1];
            end;
          end;
        end
        else
        begin
          Text[Text2PlayersDuetSingerP1].Text := CatSongs.Song[Interaction].DuetNames[0];
          Text[Text2PlayersDuetSingerP2].Text := CatSongs.Song[Interaction].DuetNames[1];
        end;
      end;
    end
    else
    begin
      Text[Text2PlayersDuetSingerP1].Visible := false;
      Text[Text2PlayersDuetSingerP2].Visible := false;

      Statics[Static2PlayersDuetSingerP1].Visible := false;
      Statics[Static2PlayersDuetSingerP2].Visible := false;

      Text[Text3PlayersDuetSingerP1].Visible := false;
      Text[Text3PlayersDuetSingerP2].Visible := false;
      Text[Text3PlayersDuetSingerP3].Visible := false;

      Statics[Static3PlayersDuetSingerP1].Visible := false;
      Statics[Static3PlayersDuetSingerP2].Visible := false;
      Statics[Static3PlayersDuetSingerP3].Visible := false;

      Statics[Static4PlayersDuetSingerP3].Visible := false;
      Statics[Static4PlayersDuetSingerP4].Visible := false;

      Statics[Static6PlayersDuetSingerP4].Visible := false;
      Statics[Static6PlayersDuetSingerP5].Visible := false;
      Statics[Static6PlayersDuetSingerP6].Visible := false;

    end;

    //Set Song Score
    SongScore;

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
    Text[TextYear].Text  := '';

    Statics[VideoIcon].Visible := false;

    for B := 0 to High(Button) do
      Button[B].Visible := false;

  end;
end;

(**
 * Roulette
 *)
procedure TScreenSong.SetRouletteScroll;
var
  B:      integer;
  Angle:    real;
  Pos:    real;
  VS:     integer;
  Padding:     real;
  X:        real;
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

        //Padding := (Button[B].H - Theme.Song.Cover.H)/2;

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

procedure TScreenSong.SetRouletteScrollRefresh;
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
 * Chessboard
 *)
procedure TScreenSong.SetChessboardScroll;
var
  B:      integer;
  GridY, GridX, FactorH, FactorW: real;
  CoverH, CoverW, ZoomH, ZoomW, CurrentTick: integer;
  Padding:     integer;
  MaxRow, MaxCol, Line, LastLine, Index, Count: integer;
  CorrectX: real;
  First: boolean;
begin
  Padding := Theme.Song.Cover.Padding;
  GridX := Theme.Song.Cover.X;
  GridY := Theme.Song.Cover.Y;

  CoverH := Theme.Song.Cover.H;
  CoverW := Theme.Song.Cover.W;

  ZoomH := Theme.Song.Cover.ZoomThumbH;
  ZoomW := Theme.Song.Cover.ZoomThumbW;

  FactorH := Theme.Song.Cover.ZoomThumbH;
  FactorW := Theme.Song.Cover.ZoomThumbW;

  MaxRow := Theme.Song.Cover.Rows + ChessboardMinLine;
  MaxCol := Theme.Song.Cover.Cols;

  Index := 0;
  LastLine := 0;
  Count := 0;
  First := true;
  CorrectX := 0;

  // new song select by mouse
  if (LastSelectMouse <> 0) and (SDL_GetTicks > LastSelectMouse + MAX_TIME_MOUSE_SELECT) then
  begin
    LastSelectMouse := 0;
    OnSongSelect;
  end;

  // Update positions of all buttons
  for B := 0 to High(Button) do
  begin
    Button[B].Visible := CatSongs.Song[B].Visible; // adjust visibility

    if (Button[B].Visible) then
    begin
      LastVisibleSongIndex := B;

      if (First) then
      begin
        FirstVisibleSongIndex := B;
        First := false;
      end;
    end;

    Line := Count div MaxCol;

    if (Button[B].Visible) and (Line < MaxRow) then // Only change pos for visible buttons
    begin

      if (Line >= ChessboardMinLine) then
      begin
        if (Index = Interaction) then
        begin
          if (LastSelectTime <> 0) then
          begin
            CurrentTick := SDL_GetTicks;
            FactorH := CoverH + (1/((ZoomH - CoverH)/(CurrentTick - LastSelectTime)*0.6));
            FactorW := CoverW + (1/((ZoomW - CoverW)/(CurrentTick - LastSelectTime)*0.6));
          end
          else
          begin
            FactorH := ZoomH;
            FactorW := ZoomW;
          end;

          if (FactorH > ZoomH) then
            FactorH := ZoomH;

          if (FactorW > ZoomW) then
            FactorW := ZoomW;

          if (FactorW = ZoomW) and (FactorH = ZoomH) then
            LastSelectTime := 0;

          Button[B].H := FactorH;
          Button[B].W := FactorW;
          Button[B].Z := 1;
        end
        else
        begin
          Button[B].H := CoverH;
          Button[B].W := CoverW;
          Button[B].Z := 0.9;
        end;

        Button[B].Reflection := false;

        if (Count = 0) or (Line <> LastLine) then
        begin
          Button[B].X := GridX;
        end
        else
        begin
          Button[B].X := CorrectX + CoverW + Padding;
        end;

        CorrectX := Button[B].X;

        if (Index = Interaction) then
          Button[B].X := Button[B].X - (FactorW - CoverW)/2;

        if (Line = ChessboardMinLine)then
        begin
          Button[B].Y := GridY;

          if (Index = Interaction) then
            Button[B].Y := Button[B].Y - (FactorH - CoverH)/2;
        end
        else
        begin
          Button[B].Y := GridY + (CoverH + Padding) * (Line - ChessboardMinLine);

          if (Index = Interaction) then
            Button[B].Y := Button[B].Y - (FactorH - CoverH)/2;
        end;

        LastLine := Line;
      end
      else
        Button[B].Visible := false;

      Count := Count + 1;
    end
    else
      Button[B].Visible := false;

    Index := Index + 1;
  end;

end;

procedure TScreenSong.SetChessboardScrollRefresh;
begin
  Statics[StaticActual].Texture := Texture.GetTexture(Button[Interaction].Texture.Name, TEXTURE_TYPE_PLAIN, false);
  Statics[StaticActual].Texture.Alpha := 1;

  Statics[StaticActual].Texture.X := Theme.Song.Cover.SelectX;
  Statics[StaticActual].Texture.Y := Theme.Song.Cover.SelectY;
  Statics[StaticActual].Texture.W := Theme.Song.Cover.SelectW;
  Statics[StaticActual].Texture.H := Theme.Song.Cover.SelectH;
  Statics[StaticActual].Texture.Z := 1;

  Statics[StaticActual].Reflection := Theme.Song.Cover.SelectReflection;
  Statics[StaticActual].Reflectionspacing := Theme.Song.Cover.SelectReflectionSpacing;
end;

(**
 * Carousel
 *)
procedure TScreenSong.SetCarouselScroll;
var
  B, Count: integer;
begin

  Count := 0;

  // line
  for B := 0 to High(Button) do
  begin
    Button[B].Visible := CatSongs.Song[B].Visible;

    if (Button[B].Visible) then
    begin
      Button[B].X := Theme.Song.Cover.X + (Count - SongCurrent) * (Theme.Song.Cover.W + Theme.Song.Cover.Padding);
      Button[B].Y := Theme.Song.Cover.Y;
      Button[B].W := Theme.Song.Cover.W;
      Button[B].H := Theme.Song.Cover.H;

      if (Button[B].X < -Button[B].W) or (Button[B].X > 800) then
      begin
        Button[B].Visible := false;
      end
      else
        Button[B].Visible := true;

      Count := Count + 1;
    end;
  end;
end;

procedure TScreenSong.SetCarouselScrollRefresh;
begin
end;

(**
 * Slot Machine
 *)
procedure TScreenSong.SetSlotMachineScroll;
var
  B:      integer;
  Angle:  real;
  Pos:    real;
  VS:     integer;
  diff:   real;
  X:      real;
begin
  VS := CatSongs.VisibleSongs;

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

      //fixed Positions
      if (Abs(Pos) < 2.0) then
      begin
        Angle := Pi * (Pos / 5);

        Button[B].Texture.Alpha := 1 - Abs(Pos/1.5);

        Button[B].H := Abs(Theme.Song.Cover.H * cos(Angle*1.2));

        Button[B].DeSelectReflectionspacing := 15 * Button[B].H/Theme.Song.Cover.H;

        Button[B].Z := 0.05 - Abs(Pos) * 0.01;

        Button[B].X := (Theme.Song.Cover.X  + (Theme.Song.Cover.H - Abs(Theme.Song.Cover.H * cos(Angle))) * 0.8);

        Button[B].W := Button[B].H;

        Diff := (Button[B].H - Theme.Song.Cover.H)/2;

        X := Sin(Angle*1.3)*0.8;

        Button[B].Y := Theme.Song.Cover.Y + Theme.Song.Cover.W * X - Diff;

      end
      else
        Button[B].Visible := false;
     end;
  end;
end;

procedure TScreenSong.SetSlotMachineScrollRefresh;
begin
end;

(**
 * Slide
 *)
procedure TScreenSong.SetSlideScroll;
var
  B, Count, DiffH: integer;
  Scale:    real;
begin

  Count := 0;
  Scale := 0.90;
  DiffH := 20;

  // line
  for B := 0 to High(Button) do
  begin
    Button[B].Visible := CatSongs.Song[B].Visible;

    if (Button[B].Visible) then
    begin

      if (B <= Interaction) then
        Button[B].X := Theme.Song.Cover.X + (Count - SongCurrent) * Theme.Song.Cover.Padding
      else
        Button[B].X := Theme.Song.Cover.X + (Count - SongCurrent) * Theme.Song.Cover.W - (Count - SongCurrent) * (Theme.Song.Cover.W * Scale - Theme.Song.Cover.Padding);

      Button[B].Y := Theme.Song.Cover.Y;
      Button[B].W := Theme.Song.Cover.W;
      Button[B].H := Theme.Song.Cover.H;

      if (B < Interaction) then
      begin
        Button[B].Z := B/High(Button);

        Button[B].Texture.RightScale := Scale;
        Button[B].Texture.LeftScale := 1;

        Button[B].H := Theme.Song.Cover.H - DiffH;
        Button[B].W := Button[B].W * Scale;

        Button[B].Y := Theme.Song.Cover.Y + DiffH;

        Button[B].Texture.Alpha := 1;
      end
      else
      begin
        if (B > Interaction) then
        begin
          Button[B].Z := 1 - ((Count + 1)/100);

          Button[B].Texture.LeftScale := Scale;
          Button[B].Texture.RightScale := 1;

          Button[B].H := Theme.Song.Cover.H - DiffH;
          Button[B].W := Button[B].W * Scale;

          Button[B].Y := Theme.Song.Cover.Y + DiffH;

          Button[B].Texture.Alpha := 1;
        end
        else
        begin
          Button[B].Z := 1;

          Button[B].Texture.LeftScale := 1;
          Button[B].Texture.RightScale := 1;

          Button[B].H := Theme.Song.Cover.H;
          Button[B].W := Theme.Song.Cover.W;

          Button[B].Y := Theme.Song.Cover.Y;

          Button[B].Texture.Alpha := 1;
          Button[B].Texture.Z := 1;
        end;
      end;

      if (Button[B].X < -Button[B].W) or (Button[B].X > 800) then
      begin
        Button[B].Visible := false;
      end
      else
        Button[B].Visible := true;

      Count := Count + 1;
    end;
  end;
end;

procedure TScreenSong.SetSlideScrollRefresh;
begin
end;


(**
 * List
 *)
procedure TScreenSong.SetListScroll;
var
  B, Line:  integer;
  First: boolean;
begin
  Line := 0;
  First := true;

  // line
  for B := 0 to High(Button) do
  begin
    Button[B].Visible := CatSongs.Song[B].Visible;

    if (Button[B].Visible) then
    begin
      if (Line >= ListMinLine) then
      begin

        if (First) then
        begin
          ListFirstVisibleSongIndex := B;
          First := false;
        end;

        if (Line - ListMinLine < Theme.Song.ListCover.Rows) then
        begin
          Button[B].Z := 1;

          Button[B].X := Theme.Song.Cover.X;
          Button[B].Y := Theme.Song.Cover.Y + (Line - ListMinLine) * (Theme.Song.Cover.H + Theme.Song.Cover.Padding);
          Button[B].W := Theme.Song.Cover.W;
          Button[B].H := Theme.Song.Cover.H;

          Line := Line + 1;
        end
        else
        begin
          Button[B].Visible := false;
        end;
      end
      else
      begin
        Line := Line + 1;
        Button[B].Visible := false;
      end;
    end;
  end;

end;

procedure TScreenSong.SetListScrollRefresh;
var
  B, Count, I, VS:  integer;
  SongID: array of integer;
  Alpha: real;
begin

  Statics[StaticActual].Texture := Texture.GetTexture(Button[Interaction].Texture.Name, TEXTURE_TYPE_PLAIN, false);
  Statics[StaticActual].Texture.Alpha := 1;

  Statics[StaticActual].Texture.X := Theme.Song.Cover.SelectX;
  Statics[StaticActual].Texture.Y := Theme.Song.Cover.SelectY;
  Statics[StaticActual].Texture.W := Theme.Song.Cover.SelectW;
  Statics[StaticActual].Texture.H := Theme.Song.Cover.SelectH;
  Statics[StaticActual].Texture.Z := 1;

  Statics[StaticActual].Reflection := Theme.Song.Cover.SelectReflection;
  Statics[StaticActual].Reflectionspacing := Theme.Song.Cover.SelectReflectionSpacing;

  for I := 0 to High(StaticsList) do
  begin
    Text[ListTextArtist[I]].Text := '';
    Text[ListTextTitle[I]].Text  := '';
    Text[ListTextYear[I]].Text   := '';
    Statics[ListVideoIcon[I]].Visible  := false;
    Statics[ListMedleyIcon[I]].Visible := false;
    Statics[ListCalcMedleyIcon[I]].Visible := false;
    Statics[ListDuetIcon[I]].Visible := false;

    //reset
    StaticsList[I].Texture.TexNum := StaticsList[I].TextureDeSelect.TexNum;
    StaticsList[I].Texture.W := Theme.Song.ListCover.W;
    StaticsList[I].Texture.H := Theme.Song.ListCover.H;
    StaticsList[I].Texture.X := Theme.Song.ListCover.X;
  end;

  Count := 0;

  B := ListFirstVisibleSongIndex;
  VS := CatSongs.VisibleSongs;

  if (ListMinLine <> ListLastMinLine) then //and ((ListMinLine <= VS - Theme.Song.ListCover.Rows)) then// change start
  begin
    if (ListLastMinLine < ListMinLine) then
      B := CatSongs.FindNextVisible(B)
    else
      B := CatSongs.FindPreviousVisible(B);

    ListLastMinLine := ListMinLine;
  end;

  SetLength(SongID, 0);

  while ((Count <= High(StaticsList)) and (B <= High(CatSongs.Song))) do
  begin
    if (CatSongs.Song[B].Visible) then
    begin
      SetLength(SongID, Length(SongID) + 1);

      SongID[High(SongID)] := B;
      Count := Count + 1;
    end;

    B := B + 1;
  end;

  // save first category
  if CatSongs.Song[Interaction].Main then
    MainListFirstVisibleSongIndex := CatSongs.FindPreviousVisible(SongID[0]);

  for I := 0 to Count - 1 do
  begin
    if (SongID[I] = Interaction) then
    begin
      Alpha := 1;
      StaticsList[I].Texture.TexNum := StaticsList[I].TextureSelect.TexNum;
    end
    else
      Alpha := 0.7;

    // Set visibility of video icon
    Statics[ListVideoIcon[I]].Texture.Alpha := Alpha;
    Statics[ListVideoIcon[I]].Visible := CatSongs.Song[SongID[I]].Video.IsSet;

    // Set visibility of medley icons
    Statics[ListMedleyIcon[I]].Texture.Alpha := Alpha;
    Statics[ListMedleyIcon[I]].Visible := (CatSongs.Song[SongID[I]].Medley.Source = msTag);

    Statics[ListCalcMedleyIcon[I]].Texture.Alpha := Alpha;
    Statics[ListCalcMedleyIcon[I]].Visible := (CatSongs.Song[SongID[I]].Medley.Source = msCalculated);

    //Set Visibility of Duet Icon
    Statics[ListDuetIcon[I]].Texture.Alpha := Alpha;
    Statics[ListDuetIcon[I]].Visible := CatSongs.Song[SongID[I]].isDuet;

    // Set texts
    Text[ListTextArtist[I]].Alpha := Alpha;
    Text[ListTextArtist[I]].Text := CatSongs.Song[SongID[I]].Artist;

    Text[ListTextTitle[I]].Alpha := Alpha;
    Text[ListTextTitle[I]].Text  :=  CatSongs.Song[SongID[I]].Title;

    Text[ListTextYear[I]].Alpha := Alpha;

    if ((Ini.Tabs = 0) or (TSortingType(Ini.Sorting) <> sYear))
      and (CatSongs.Song[SongID[I]].Year <> 0) then
        Text[ListTextYear[I]].Text  :=  '(' + InttoStr(CatSongs.Song[SongID[I]].Year) + ')'
    else
      Text[ListTextYear[I]].Text  :=  '';

  end;

end;

procedure TScreenSong.OnShow;
var
  I: integer;
begin
  inherited;

  CloseMessage();

  if (TSongMenuMode(Ini.SongMenu) in [smChessboard, smList, smMosaic]) then
  begin
    Statics[StaticActual].Texture.X := Theme.Song.Cover.SelectX;
    Statics[StaticActual].Texture.Y := Theme.Song.Cover.SelectY;
    Statics[StaticActual].Texture.W := Theme.Song.Cover.SelectW;
    Statics[StaticActual].Texture.H := Theme.Song.Cover.SelectH;
    Statics[StaticActual].Texture.Z := 1;

    Statics[StaticActual].Reflection := Theme.Song.Cover.SelectReflection;
    Statics[StaticActual].Reflectionspacing := Theme.Song.Cover.SelectReflectionSpacing;

    Statics[StaticActual].Visible := true;
  end
  else
  begin
    Statics[StaticActual].Visible := false;
  end;

  if (TSongMenuMode(Ini.SongMenu) <> smList) then
  begin
    for I := 0 to High(StaticsList) do
    begin
      StaticsList[StaticList[I]].Visible := false;

      Text[ListTextArtist[I]].Visible := false;
      Text[ListTextTitle[I]].Visible  := false;
      Text[ListTextYear[I]].Visible   := false;
      Statics[ListVideoIcon[I]].Visible  := false;
      Statics[ListMedleyIcon[I]].Visible := false;
      Statics[ListCalcMedleyIcon[I]].Visible := false;
      Statics[ListDuetIcon[I]].Visible := false;
    end;

    Text[TextArtist].Visible := true;
    Text[TextTitle].Visible  := true;
    Text[TextYear].Visible   := true;
    Statics[VideoIcon].Visible  := true;
    Statics[MedleyIcon].Visible := true;
    Statics[CalcMedleyIcon].Visible := true;
    Statics[DuetIcon].Visible := true;
  end
  else
  begin
    for I := 0 to High(StaticsList) do
    begin
      StaticsList[StaticList[I]].Visible := true;

      Text[ListTextArtist[I]].Visible := true;
      Text[ListTextTitle[I]].Visible  := true;
      Text[ListTextYear[I]].Visible   := true;
      Statics[ListVideoIcon[I]].Visible  := true;
      Statics[ListMedleyIcon[I]].Visible := true;
      Statics[ListCalcMedleyIcon[I]].Visible := true;
      Statics[ListDuetIcon[I]].Visible := true;
    end;

    Text[TextArtist].Visible := false;
    Text[TextTitle].Visible  := false;
    Text[TextYear].Visible   := false;
    Statics[VideoIcon].Visible  := false;
    Statics[MedleyIcon].Visible := false;
    Statics[CalcMedleyIcon].Visible := false;
    Statics[DuetIcon].Visible := false;
  end;


  // for duet names
  ScreenSong.ColPlayer[0] := GetPlayerColor(Ini.SingColor[0]);
  ScreenSong.ColPlayer[1] := GetPlayerColor(Ini.SingColor[1]);
  ScreenSong.ColPlayer[2] := GetPlayerColor(Ini.SingColor[2]);
  ScreenSong.ColPlayer[3] := GetPlayerColor(Ini.SingColor[3]);
  ScreenSong.ColPlayer[4] := GetPlayerColor(Ini.SingColor[4]);
  ScreenSong.ColPlayer[5] := GetPlayerColor(Ini.SingColor[5]);

  //if (Mode = smPartyTournament) then
  //  Text[TextPartyTime].Text := IntToStr(MAX_TIME);

  {**
   * Pause background music
   *}
  SoundLib.PauseBgMusic;

  if SongIndex <> Interaction then
    AudioPlayback.Stop;

  PreviewOpened := -1;

  // reset video playback engine
  fCurrentVideo := nil;

  // reset Medley-Playlist
  SetLength(PlaylistMedley.Song, 0);
  MakeMedley := false;

  if Mode = smMedley then
    Mode := smNormal;

  if Ini.Players <= 3 then PlayersPlay := Ini.Players + 1;
  if Ini.Players  = 4 then PlayersPlay := 6;

  //Cat Mod etc
  if (Ini.TabsAtStartup = 1) and (CatSongs.CatNumShow = -1) then
  begin
    CatSongs.ShowCategoryList;

    if (TSongMenuMode(Ini.SongMenu) <> smCarousel) and (TSongMenuMode(Ini.SongMenu) <> smSlide) then
      FixSelected;

    //Show Cat in Top Left Mod
    HideCatTL;
  end;


  //Playlist Mode
  if not(Mode = smPartyClassic) then
  begin
    //If Playlist Shown -> Select Next automatically
    if not(Mode = smPartyFree) and (CatSongs.CatNumShow = -3) then
    begin
      SelectNext;
    end;
  end
  //Party Mode
  else
  begin
    SelectRandomSong;
    //Show Menu directly in PartyMode
    //But only if selected in Options
    if (Ini.PartyPopup = 1) then
    begin
      ScreenSongMenu.MenuShow(SM_Party_Main);
    end;
  end;

  if (ScreenSong.Mode = smJukebox) and (Ini.PartyPopup = 1) then
    ScreenSongMenu.MenuShow(SM_Jukebox);

  isScrolling := false;
  SetJoker;
  SetStatics;

  {
  if (Mode = smPartyFree) then
    Text[TextPartyTime].Visible := true
  else
    Text[TextPartyTime].Visible := false;
  }
end;

procedure TScreenSong.OnShowFinish;
begin
  DuetChange := false;

  isScrolling := true;
  CoverTime := 10;

  SetScrollRefresh;

  //if (Mode = smPartyTournament) then
  //  PartyTime := SDL_GetTicks();

end;

procedure TScreenSong.OnHide;
begin
  // turn music volume to 100%
  AudioPlayback.SetVolume(1.0);

  // stop preview
  StopMusicPreview();
  StopVideoPreview();
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
  end;
end;

function TScreenSong.FinishedMusic: boolean;
begin

  Result := AudioPlayback.Finished;

end;

function TScreenSong.Draw: boolean;
var
  dx:         real;
  dt:         real;
  VideoAlpha: real;
  Position:   real;
  I, J:       integer;
begin

  FadeMessage();

  if isScrolling then
  begin
    dx := SongTarget - SongCurrent;
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

  {   //basisbit todo this block was auskommentiert
  if SongCurrent > Catsongs.VisibleSongs then
  begin
    SongCurrent := SongCurrent - Catsongs.VisibleSongs;
    SongTarget := SongTarget - Catsongs.VisibleSongs;
  end;  }


  //Log.BenchmarkStart(5);

  // Song Menu Style
  SetScroll;

  //Log.BenchmarkEnd(5);
  //Log.LogBenchmark('SetScroll4', 5);

  if (AudioPlayback.Finished) then
    CoverTime := 0;

  //Fading Functions, Only if Covertime is under 5 Seconds
  if (TSongMenuMode(Ini.SongMenu) in [smChessboard, smMosaic, smList]) then
  begin
    if not(Assigned(fCurrentVideo)) then
      Statics[StaticActual].Texture.Alpha := 1
    else
    begin
      if (CoverTime < 9) and Assigned(fCurrentVideo) then
      begin

        //Update Fading Time
        CoverTime := CoverTime + TimeSkip;

        //Update Fading Texture
        Statics[StaticActual].Texture.Alpha := 1 - (CoverTime - 1) * 1.5;
        if Statics[StaticActual].Texture.Alpha < 0 then
          Statics[StaticActual].Texture.Alpha := 0;
      end;
    end;
  end
  else
  begin
    // cover fade
    if (CoverTime < 9) then
    begin
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
  end;

  //inherited Draw;
  //heres a little Hack, that causes the Statics
  //are Drawn after the Buttons because of some Blending Problems.
  //This should cause no Problems because all Buttons on this screen
  //Has Z Position.
  //Draw BG
  DrawBG;

  // StaticsList
  for I := 0 to Length(StaticsList) - 1 do
  begin
    StaticsList[I].Draw;
  end;

  // Jukebox Playlist
  if (Mode = smJukebox) then
  begin
    if Length(ScreenJukebox.JukeboxSongsList) > Theme.Song.TextMedleyMax then
      J := Length(ScreenJukebox.JukeboxSongsList) - Theme.Song.TextMedleyMax
    else
      J := 0;

    for I := 0 to Theme.Song.TextMedleyMax - 1 do
    begin
      if (Length(ScreenJukebox.JukeboxSongsList) > I + J) then
      begin
        Text[TextMedleyArtist[I]].Visible := true;
        Text[TextMedleyTitle[I]].Visible  := true;
        Text[TextMedleyNumber[I]].Visible := true;
        Statics[StaticMedley[I]].Visible  := true;

        Text[TextMedleyNumber[I]].Text := IntToStr(I + 1 + J);
        Text[TextMedleyArtist[I]].Text := CatSongs.Song[ScreenJukebox.JukeboxSongsList[I + J]].Artist;
        Text[TextMedleyTitle[I]].Text  := CatSongs.Song[ScreenJukebox.JukeboxSongsList[I + J]].Title;
      end
      else
      begin
        Text[TextMedleyArtist[I]].Visible := false;
        Text[TextMedleyTitle[I]].Visible  := false;
        Text[TextMedleyNumber[I]].Visible := false;
        Statics[StaticMedley[I]].Visible  := false;
      end;
    end;
  end
  else
  begin

    //Medley Playlist
    if Length(PlaylistMedley.Song) > Theme.Song.TextMedleyMax then
      J := Length(PlaylistMedley.Song) - Theme.Song.TextMedleyMax
    else
      J := 0;

    for I := 0 to Theme.Song.TextMedleyMax - 1 do
    begin
      if (Length(PlaylistMedley.Song) > I + J) and (MakeMedley) then
      begin
        Text[TextMedleyArtist[I]].Visible := true;
        Text[TextMedleyTitle[I]].Visible  := true;
        Text[TextMedleyNumber[I]].Visible := true;
        Statics[StaticMedley[I]].Visible  := true;

        Text[TextMedleyNumber[I]].Text := IntToStr(I + 1 + J);
        Text[TextMedleyArtist[I]].Text := CatSongs.Song[PlaylistMedley.Song[I + J]].Artist;
        Text[TextMedleyTitle[I]].Text  := CatSongs.Song[PlaylistMedley.Song[I + J]].Title;
      end
      else
      begin
        Text[TextMedleyArtist[I]].Visible := false;
        Text[TextMedleyTitle[I]].Visible  := false;
        Text[TextMedleyNumber[I]].Visible := false;
        Statics[StaticMedley[I]].Visible  := false;
      end;
    end;
  end;

  if (TSongMenuMode(Ini.SongMenu) in [smRoulette, smCarousel, smSlotMachine, smSlide])  then
    VideoAlpha := Button[interaction].Texture.Alpha * (CoverTime-1)
  else
    VideoAlpha := 1;

  //Instead of Draw FG Procedure:
  //We draw Buttons for our own
  for I := 0 to Length(Button) - 1 do
  begin
    if (TSongMenuMode(Ini.SongMenu) in [smChessboard, smMosaic, smList]) or (((I<>Interaction) or not Assigned(fCurrentVideo) or (VideoAlpha<1) or FinishedMusic)) then
        Button[I].Draw;
  end;

  //  StopVideoPreview;

  Position := AudioPlayback.Position;

  if Assigned(fCurrentVideo) then
  begin
    // Just call this once
    // when Screens = 2
    if (ScreenAct = 1) then
      fCurrentVideo.GetFrame(CatSongs.Song[Interaction].VideoGAP + Position);

    fCurrentVideo.SetScreen(ScreenAct);
    fCurrentVideo.Alpha := VideoAlpha;

    //set up window
    if (TSongMenuMode(Ini.SongMenu) in [smChessboard, smMosaic, smList]) then
    begin
        fCurrentVideo.SetScreenPosition(Theme.Song.Cover.SelectX, Theme.Song.Cover.SelectY, 1);
        fCurrentVideo.Width := Theme.Song.Cover.SelectW;
        fCurrentVideo.Height := Theme.Song.Cover.SelectH;

        fCurrentVideo.ReflectionSpacing := Theme.Song.Cover.SelectReflectionSpacing;
    end
    else
    begin
      with Button[interaction] do
      begin
        fCurrentVideo.SetScreenPosition(X, Y, Z);
        fCurrentVideo.Width := W;
        fCurrentVideo.Height := H;
        fCurrentVideo.ReflectionSpacing := Reflectionspacing;
      end;
    end;

    fCurrentVideo.AspectCorrection := acoCrop;

    fCurrentVideo.Draw;

    if Button[interaction].Reflection or (Theme.Song.Cover.SelectReflection) then
      fCurrentVideo.DrawReflection;
  end;

  // duet names
  if (CatSongs.Song[Interaction].isDuet) then
    ColorDuetNameSingers();

  // Statics
  for I := 0 to Length(Statics) - 1 do
    Statics[I].Draw;

  // and texts
  for I := 0 to Length(Text) - 1 do
    Text[I].Draw;

  Equalizer.Draw;

  DrawExtensions;

  //if (Mode = smPartyTournament) then
  //  PartyTimeLimit();

  Result := true;
end;

{
procedure TScreenSong.PartyTimeLimit();
var
  CurrentTick: cardinal;
  I, I2:       integer;
  TimeS: string;
  N_DuetSongs: integer;
begin
  CurrentTick := SDL_GetTicks();

  TimeS := IntToStr(Round(MAX_TIME - (CurrentTick - PartyTime)/ 1000) - CurrentPartyTime);

  if (StrToInt(TimeS) > 0) then
  begin
    Text[TextPartyTime].Text := TimeS;

    if (StrToInt(Text[TextPartyTime].Text) <= 5) then
    begin
      Text[TextPartyTime].ColR := Theme.Song.TextPartyTime.DColR;
      Text[TextPartyTime].ColG := Theme.Song.TextPartyTime.DColG;
      Text[TextPartyTime].ColB := Theme.Song.TextPartyTime.DColB;
    end
    else
    begin
      Text[TextPartyTime].ColR := Theme.Song.TextPartyTime.ColR;
      Text[TextPartyTime].ColG := Theme.Song.TextPartyTime.ColG;
      Text[TextPartyTime].ColB := Theme.Song.TextPartyTime.ColB;
    end;

  end
  else
  begin
    // number of duet songs
    N_DuetSongs := 0;
    for I := 0 to High(CatSongs.Song) do
    begin
      if (CatSongs.Song[I].isDuet) then
        Inc(N_DuetSongs);
    end;

    // random
    repeat
      if (Ini.TabsAtStartup = 1) then // random with category
      begin
        I := Random(High(CatSongs.Song) + 1);

        while CatSongs.Song[I].Main do
          Inc(I);

        I2 := I - 1;

        while not(CatSongs.Song[I2].Main) do
          Dec(I2);

        // show cat in top left mod
        ShowCatTL (I2);

        Interaction := I;

        CatSongs.ShowCategoryList;
        CatSongs.ClickCategoryButton(I2);
        SelectNext;
        FixSelected;
      end
      else // random in one category
      begin
        SkipTo(Random(CatSongs.VisibleSongs));
      end;
    until (Party.SongNotSungAndNotDuet(Interaction, N_DuetSongs));

    ParseInput(SDLK_RETURN, 0, true);
  end;

end;
}

procedure TScreenSong.SelectNext(onlyFix: boolean = true);
var
  Skip: integer;
  VS:   integer;
  NextInt: integer;
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

    NextInt := (Interaction + Skip) mod Length(Interactions);

    SongTarget := SongTarget + 1;//Skip;

    if not ((TSongMenuMode(Ini.SongMenu) in [smChessboard, smList, smMosaic]) and (NextInt < Interaction)) then
      Interaction := NextInt;

    // try to keep all at the beginning
    if SongTarget > VS-1 then
    begin
      SongTarget := SongTarget - VS;
      SongCurrent := SongCurrent - VS;
    end;

   // if ((TSongMenuMode(Ini.SongMenu) in [smList]) and (NextInt = 0)) then
   //   SongCurrent := -1;

    if (not onlyFix) then
    begin
      if (TSongMenuMode(Ini.SongMenu) in [smChessboard, smMosaic]) then
      begin
        if (not Button[Interaction].Visible) then
          ChessboardMinLine := ChessboardMinLine + 1;
      end;
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
  PrevInt: integer;
begin
  VS := CatSongs.VisibleSongs;

  if (VS > 0) then
  begin
    if (not isScrolling) then
    begin
      isScrolling := true;
      OnSongDeselect;
    end;

    Skip := 1;

    while (not CatSongs.Song[(Interaction - Skip + Length(Interactions)) mod Length(Interactions)].Visible) do
      Inc(Skip);

    SongTarget := SongTarget - 1;

    PrevInt := (Interaction - Skip + Length(Interactions)) mod Length(Interactions);

    if not ((TSongMenuMode(Ini.SongMenu) in [smChessboard, smList, smMosaic]) and (PrevInt > Interaction)) then
      Interaction := PrevInt;
    
    if (TSongMenuMode(Ini.SongMenu) in [smChessboard, smMosaic]) then
    begin
      if (not Button[Interaction].Visible) then
        ChessboardMinLine := ChessboardMinLine - 1;
    end;

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

procedure TScreenSong.SelectNextRow;
var
  Skip, SongIndex: integer;
  VS:   integer;
begin
  VS := CatSongs.VisibleSongs;

  AudioPlayback.PlaySound(SoundLib.Change);

  if VS > 0 then
  begin

    if (not isScrolling) and (VS > 0) then
    begin
      isScrolling := true;
      OnSongDeselect;
    end;

    Skip := 0;
    SongIndex := Interaction;

    while ((Skip <= Theme.Song.Cover.Cols) and (SongIndex < Length(CatSongs.Song))) do
    begin
      if (CatSongs.Song[SongIndex].Visible) then
      begin
        Inc(Skip);
      end;

      Inc(SongIndex);
    end;

    SongTarget := SongTarget + 1;

    if (Skip <= Theme.Song.Cover.Cols) then
    begin
      Interaction := LastVisibleSongIndex;
    end
    else
    begin
      if (CatSongs.Song[SongIndex - 1].Visible) then
        Interaction := SongIndex - 1;
    end;

    if (not Button[Interaction].Visible) then
      ChessboardMinLine := ChessboardMinLine + 1;
  end;
end;

procedure TScreenSong.SelectPrevRow;
var
  Skip, SongIndex: integer;
  VS:   integer;
begin
  VS := CatSongs.VisibleSongs;

  AudioPlayback.PlaySound(SoundLib.Change);

  if (VS > 0) then
  begin

    if (not isScrolling) and (VS > 0) then
    begin
      isScrolling := true;
      OnSongDeselect;
    end;

    Skip := 0;
    SongIndex := Interaction;

    while ((Skip <= Theme.Song.Cover.Cols) and (SongIndex > -1)) do
    begin
      if (CatSongs.Song[SongIndex].Visible) then
      begin
        Inc(Skip);
      end;

      Dec(SongIndex);
    end;

    SongTarget := SongTarget - 1;

    if (Skip <= Theme.Song.Cover.Cols) then
    begin
      Interaction := FirstVisibleSongIndex;
    end
    else
    begin
      if (CatSongs.Song[SongIndex + 1].Visible) then
        Interaction := SongIndex + 1;
    end;

  end;

  if (not Button[Interaction].Visible) then
    ChessboardMinLine := ChessboardMinLine - 1;
end;

procedure TScreenSong.SelectNextListRow;
var
  VS, MaxListLine: integer;
  NrMiddleSong: integer;
  MaxListLineTmp: real;
begin
  AudioPlayback.PlaySound(SoundLib.Change);
  SelectNext;

  if (not Button[Interaction].Visible) then
  begin
    ListLastMinLine := ListMinLine;
    ListMinLine := ListMinLine + 1;
  end;
end;

procedure TScreenSong.SelectPrevListRow;
begin
  AudioPlayback.PlaySound(SoundLib.Change);
  SelectPrev;

  if (not Button[Interaction].Visible) then
  begin
    ListLastMinLine := ListMinLine;
    ListMinLine := ListMinLine - 1;
  end;
end;

procedure TScreenSong.StartMusicPreview();
var
  Song: TSong;
  PreviewPos: real;
  I: integer;
  Vol: cardinal;
begin
  if SongIndex <> -1 then
  begin
    PreviewOpened := SongIndex;
    Exit;
  end;

  AudioPlayback.Close();

  if CatSongs.VisibleSongs = 0 then
    Exit;

  Song := CatSongs.Song[Interaction];
  if not assigned(Song) then
    Exit;

  //fix: if main cat than there is nothing to play
  if Song.main then
    Exit;

  PlayMidi := false;
  if AudioPlayback.Open(Song.Path.Append(Song.Mp3)) then
  begin
    PreviewOpened := Interaction;

    if Song.PreviewStart > 0 then
      PreviewPos := Song.PreviewStart
    else
      PreviewPos := AudioPlayback.Length / 4;

    // fix for invalid music file lengths
    if (PreviewPos > 120.0) then
      PreviewPos := 60.0;
    AudioPlayback.Position := PreviewPos;
  
    // set preview volume
    if Ini.PreviewFading = 0 then
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

procedure TScreenSong.StartVideoPreview();
var
  VideoFile:  IPath;
  Song:       TSong;

begin
  if (Ini.VideoPreview=0)  then
    Exit;

  if Assigned(fCurrentVideo) then
  begin
    fCurrentVideo.Stop();
    fCurrentVideo := nil;
  end;

  //if no audio open => exit
  if (PreviewOpened = -1) then
    Exit;

  if CatSongs.VisibleSongs = 0 then
    Exit;

  Song := CatSongs.Song[Interaction];
  if not assigned(Song) then
    Exit;

  //fix: if main cat than there is nothing to play
  if Song.main then
    Exit;

  VideoFile := Song.Path.Append(Song.Video);
  if (Song.Video.IsSet) and VideoFile.IsFile then
  begin
    fCurrentVideo := VideoPlayback.Open(VideoFile);
    if (fCurrentVideo <> nil) then
    begin
      fCurrentVideo.Position := Song.VideoGAP + AudioPlayback.Position;
      fCurrentVideo.Play;
    end;
  end;
end;

procedure TScreenSong.StopVideoPreview();
begin
  // Stop video preview of previous song
  if Assigned(fCurrentVideo) then
  begin
    fCurrentVideo.Stop();
    fCurrentVideo := nil;
  end;
end;

// Changes previewed song
procedure TScreenSong.ChangeMusic;
begin
  StopMusicPreview();
  StopVideoPreview();
  PreviewOpened := -1;
  StartMusicPreview();
  StartVideoPreview();
end;

procedure TScreenSong.SkipTo(Target: cardinal; TargetInteraction: integer = 0; VS: integer = 0);
var
  i: integer;
  MaxLine: real;
  ChessboardLine: real;
begin
  if (TSongMenuMode(Ini.SongMenu) in [smRoulette, smCarousel, smSlide, smSlotMachine]) then
  begin
    Interaction := High(CatSongs.Song);
    SongTarget  := 0;

    for i := 1 to Target+1 do
      SelectNext;

    FixSelected2;
  end;

  if (TSongMenuMode(Ini.SongMenu) in [smChessboard, smList, smMosaic]) then
  begin
    Interaction := TargetInteraction;
    SongTarget := Interaction;

    if not (Button[Interaction].Visible) then
    begin
      ChessboardLine := (CatSongs.VisibleIndex(Interaction) - Theme.Song.Cover.Cols * Theme.Song.Cover.Rows) / Theme.Song.Cover.Cols;

      if (Frac(ChessboardLine) > 0) then
        ChessboardMinLine := Round(ChessboardLine) + 1
      else
        ChessboardMinLine := Round(ChessboardLine);

      MaxLine := (VS - Theme.Song.Cover.Cols * Theme.Song.Cover.Rows) / Theme.Song.Cover.Cols;

      if (Frac(Maxline) > 0) then
        MaxLine := Round(MaxLine) + 1
      else
        MaxLine := Round(MaxLine);

      if (ChessboardMinLine > MaxLine) then
        ChessboardMinLine := Round(MaxLine);

    end;

    FixSelected;
    OnSongSelect;
  end;

end;

procedure TScreenSong.SelectRandomSong;
var
  I, I2, Count, RealTarget: integer;
  Target: cardinal;
begin
  case PlayListMan.Mode of
      smAll:  // all songs just select random song
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
              if CatSongs.Song[I].Main and (PermitCategory(I)) then
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
            // duets not playble
            if (Mode = smPartyClassic) then
            begin
              repeat
                Target := Random(CatSongs.VisibleSongs);

                RealTarget := -1;
                Count := -1;

                repeat
                  Inc(RealTarget);

                  if (CatSongs.Song[RealTarget].Visible) then
                    Inc(Count);
                until (Count = Target);

              until not(CatSongs.Song[RealTarget].isDuet);
            end
            else
              Target := Random(CatSongs.VisibleSongs);

            SkipTo(Target, RealTarget, CatSongs.VisibleSongs);
            //SkipTo(I2 - I);
          end
          // when tabs are deactivated use easy method
          else
          begin
            // duets not playble
            if (Mode = smPartyClassic) then
            begin
              repeat
                Target := Random(CatSongs.VisibleSongs);

                RealTarget := -1;
                Count := -1;

                repeat
                  Inc(RealTarget);

                  if (CatSongs.Song[RealTarget].Visible) then
                    Inc(Count);
                until (Count = Target);

              until not(CatSongs.Song[RealTarget].isDuet);
            end
            else
              Target := Random(CatSongs.VisibleSongs);

            SkipTo(Target);
          end;
        end;
      smCategory:  // one category select category and select random song
        begin

          CatSongs.ShowCategoryList;
          CatSongs.ClickCategoryButton(PlaylistMan.CurPlayList);
          ShowCatTL(PlaylistMan.CurPlayList);

          SelectNext;
          FixSelected2;

          // duets not playble
          if (Mode = smPartyClassic) then
          begin
            repeat
              Target := Random(CatSongs.VisibleSongs);

              RealTarget := -1;
              Count := -1;

              repeat
                Inc(RealTarget);

                if (CatSongs.Song[RealTarget].Visible) then
                  Inc(Count);
              until (Count = Target);

            until not(CatSongs.Song[RealTarget].isDuet);
          end
          else
            Target := Random(CatSongs.VisibleSongs);

          SkipTo(Target);
        end;
      smPlaylist:  // playlist: select playlist and select random song
        begin
          PlaylistMan.SetPlayList(PlaylistMan.CurPlayList);

          // duets not playble
          if (Mode = smPartyClassic) then
          begin
            repeat
              Target := Random(CatSongs.VisibleSongs);

              RealTarget := -1;
              Count := -1;

              repeat
                Inc(RealTarget);

                if (CatSongs.Song[RealTarget].Visible) then
                  Inc(Count);
              until (Count = Target);

            until not(CatSongs.Song[RealTarget].isDuet);
          end
          else
            Target := Random(CatSongs.VisibleSongs);

          SkipTo(Target);
          FixSelected2;
        end;
  end;

  AudioPlayback.PlaySound(SoundLib.Change);
  SetScroll;
end;

function TScreenSong.PermitCategory(ID: integer): boolean;
var
  I, NextCat, Count: integer;
begin
  NextCat := -1;

  if Mode = smPartyClassic then
  begin
    for I := ID + 1 to High(CatSongs.Song) do
    begin
      if (CatSongs.Song[I].Main) then
      begin
        NextCat := I;
        Break;
      end;
    end;

    if (NextCat = -1) then
      NextCat := High(CatSongs.Song) + 1;

    Count := 0;
    for I := ID + 1 to NextCat - 1 do
    begin
      if not(CatSongs.Song[I].isDuet) then
      begin
        Inc(Count);
        Break;
      end;
    end;

    if (Count > 0) then
      Result := true
    else
      Result := false;
  end
  else
    Result := true;
end;

procedure TScreenSong.SetJoker;
begin
  // If Party Mode
  if Mode = smPartyClassic then //Show Joker that are available
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
  Visible := (Mode = smPartyClassic);

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

  if (Mode = smPartyFree) then
    Party.SaveSungPartySong(Interaction);

  StopMusicPreview();

  FadeTo(@ScreenSing);
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
  if (Mode = smPartyClassic) and
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
  {// show cached texture
  // background texture (garbage disposal)
  if (Button[Interaction].Texture2.TexNum > 0) then
  begin
    glDeleteTextures(1, PGLuint(@Button[Interaction].Texture2.TexNum));
    Button[Interaction].Texture2.TexNum := 0;
  end;}

  //Button[Interaction].Texture2 := Texture.GetTexture(Button[Interaction].Texture.Name, TEXTURE_TYPE_PLAIN, true);
  //Button[Interaction].Texture := Button[Interaction].Texture2;

  if Button[Interaction].Texture.Name <> Skin.GetTextureFileName('SongCover') then
    Texture.UnloadTexture(Button[Interaction].Texture.Name, TEXTURE_TYPE_PLAIN, false);
end;

procedure TScreenSong.Refresh;
begin

end;

{
procedure TScreenSong.Refresh(GiveStats: boolean);
var
  Pet:    integer;
  I:      integer;
  Name:   IPath;
Label CreateSongButtons;

begin
  Log.BenchmarkStart(2);
  ClearButtons();
  Log.BenchmarkEnd(2);
  Log.LogBenchmark('--> Refresh Clear Buttons', 2);

  Log.BenchmarkStart(2);
  CatSongs.Refresh;
  Log.BenchmarkEnd(2);
  Log.LogBenchmark('--> Refresh CatSongs', 2);

  Log.BenchmarkStart(2);
  if (length(CatSongs.Song) > 0) then
  begin
    //Set Length of Button Array one Time Instead of one time for every Song
    SetButtonLength(Length(CatSongs.Song));

    I := 0;
    Pet := 0;
    CreateSongButtons:

    try
      for Pet := I to High(CatSongs.Song) do
      begin // creating all buttons
        if (CatSongs.Song[Pet].CoverTex.TexNum = -1) then
        begin
          Texture.Limit := 512;
          if CatSongs.Song[Pet].Cover = PATH_NONE then
            AddButton(300 + Pet*250, 140, 200, 200, Skin.GetTextureFileName('SongCover'), TEXTURE_TYPE_PLAIN, Theme.Song.Cover.Reflections)
          else
          begin
            Name := CatSongs.Song[Pet].Path + CatSongs.Song[Pet].Cover;
            // cache texture if there is a need to this
            if not Covers.CoverExists(Name) then
            begin
              Texture.CreateCacheMipmap := true;
              Texture.GetTexture(Name, 'Plain', true); // preloads textures and creates cache mipmap
              Texture.CreateCacheMipmap := false;

              // puts this texture to the cache file
              Covers.AddCover(Name);

              // unload full size texture
              Texture.UnloadTexture(Name, false);

              // we should also add mipmap texture by calling createtexture and use mipmap cache as data source
            end;

            // and now load it from cache file (small place for the optimization by eliminating reading it from file, but not here)
            AddButton(300 + Pet*250, 140, 200, 200, Name, 'JPG', 'Plain', Theme.Song.Cover.Reflections, true);
          end;
          Texture.Limit := 1024*1024;
        end else
          AddButton(300 + Pet*250, 140, 200, 200, Theme.Song.Cover.Reflections, CatSongs.Song[Pet].CoverTex);

        I := -1;
      end;
    except
      //When Error is reported the First time for this Song
      if (I <> Pet) then
      begin
        //Some Error reporting:
        Log.LogError('Could not load Cover (maybe damaged?): ' + CatSongs.Song[Pet].Path + CatSongs.Song[Pet].Cover);

        //Change Cover to NoCover and Continue Loading
        CatSongs.Song[Pet].Cover := '';
        I := Pet;
      end
      else //when Error occurs Multiple Times(NoSong Cover is damaged), then start loading next Song
      begin
        Log.LogError('NoCover Cover is damaged!');
        try
          AddButton(300 + Pet*250, 140, 200, 200, '', 'JPG', 'Plain', Theme.Song.Cover.Reflections, true);
        except
          Messagebox(0, PChar('No Cover Image is damage. Could not Workaround Song Loading, Ultrastar will exit now.'), PChar(Language.Translate('US_VERSION')), MB_ICONERROR or MB_OK);
          Halt;
        end;
        I := Pet + 1;
      end;
    end;

    if (I <> -1) then
      GoTo CreateSongButtons;

  end;
  Log.BenchmarkEnd(2);
  Log.LogBenchmark('--> Refresh Create Buttons', 2);

  FixSelected;
end;
}

  {
procedure TScreenSong.StartMedley(NumSongs: integer; MinSource: TMedleySource);
  procedure AddSong(SongNr: integer);
  begin
    SetLength(PlaylistMedley.Song, Length(PlaylistMedley.Song)+1);
    PlaylistMedley.Song[Length(PlaylistMedley.Song)-1] := SongNr;
  end;

  function SongAdded(SongNr: integer): boolean;
  var
    i: integer;
    skipped: boolean;
  begin
    skipped := false;
    for i := 0 to Length(PlaylistMedley.Song) - 1 do
    begin
      if SongNr = PlaylistMedley.Song[i] then
      begin
        skipped := true;
        break;
      end;
    end;
    Result := skipped;
  end;

  function NumSongsAdded(): Integer;
  begin
    Result := Length(PlaylistMedley.Song);
  end;

  function GetNextSongNr(MinS: TMedleySource): integer;
  var
    I, num: integer;
    unused_arr: array of integer;
    visible_arr: TVisArr;
  begin
    SetLength(unused_arr, 0);
    visible_arr := getVisibleMedleyArr(MinS);
    for I := 0 to Length(visible_arr) - 1 do
    begin
      if (not SongAdded(visible_arr[I])) then
      begin
        SetLength(unused_arr, Length(unused_arr)+1);
        unused_arr[Length(unused_arr)-1] := visible_arr[I];
      end;
    end;

    num := Random(Length(unused_arr));
    Result := unused_arr[num];
end;

var
  I: integer;
  VS: integer;

begin
  if (NumSongs>0) then
  begin

    VS := Length(getVisibleMedleyArr(MinSource));
    if VS < NumSongs then
      PlaylistMedley.NumMedleySongs := VS
    else
    PlaylistMedley.NumMedleySongs := NumSongs;

    //set up Playlist Medley
    SetLength(PlaylistMedley.Song, 0);
    for I := 0 to PlaylistMedley.NumMedleySongs - 1 do
    begin
      AddSong(GetNextSongNr(MinSource));
    end;
  end else //start this song
  begin
    SetLength(PlaylistMedley.Song, 1);
    PlaylistMedley.Song[0] := Interaction;
    PlaylistMedley.NumMedleySongs := 1;
  end;

  if (Mode = smNormal) then
  begin

    Mode := smMedley;
    StopMusicPreview();

    //TODO: how about case 2? menu for medley mode?
    case Ini.OnSongClick of
      0: FadeTo(@ScreenSing);
      1: SelectPlayers;
      2: FadeTo(@ScreenSing);
      //2: begin
      //   if (CatSongs.CatNumShow = -3) then
      //     ScreenSongMenu.MenuShow(SM_Playlist)
      //   else
      //     ScreenSongMenu.MenuShow(SM_Main);
      // end;
    end;
  end;
end;
}
//start Medley round
procedure TScreenSong.StartMedley(NumSongs: integer; MinSource: TMedleySource);
  procedure AddSong(SongNr: integer);
  begin
    SetLength(PlaylistMedley.Song, Length(PlaylistMedley.Song)+1);
    PlaylistMedley.Song[Length(PlaylistMedley.Song)-1] := SongNr;
  end;

  function SongAdded(SongNr: integer): boolean;
  var
    i: integer;
    skipped :boolean;
  begin
    skipped := false;
    for i := 0 to Length(PlaylistMedley.Song) - 1 do
    begin
      if (SongNr=PlaylistMedley.Song[i]) then
      begin
        skipped:=true;
        break;
      end;
    end;
    Result:=skipped;
  end;

  function NumSongsAdded(): Integer;
  begin
    Result := Length(PlaylistMedley.Song);
  end;

  function GetNextSongNr(MinS: TMedleySource): integer;
  var
    I, num: integer;
    unused_arr: array of integer;
    visible_arr: TVisArr;
  begin
    SetLength(unused_arr, 0);
    visible_arr := getVisibleMedleyArr(MinS);
    for I := 0 to Length(visible_arr) - 1 do
    begin
      if (not SongAdded(visible_arr[I])) then
      begin
        SetLength(unused_arr, Length(unused_arr)+1);
        unused_arr[Length(unused_arr)-1] := visible_arr[I];
      end;
    end;

    num := Random(Length(unused_arr));
    Result := unused_arr[num];
end;

var
  I: integer;
  VS: integer;

begin
  //Sel3 := 0;
  if (NumSongs > 0) and not MakeMedley then
  begin
    VS := Length(getVisibleMedleyArr(MinSource));
    if VS < NumSongs then
      PlaylistMedley.NumMedleySongs := VS
    else
    PlaylistMedley.NumMedleySongs := NumSongs;

    //set up Playlist Medley
    SetLength(PlaylistMedley.Song, 0);
    for I := 0 to PlaylistMedley.NumMedleySongs - 1 do
    begin
      AddSong(GetNextSongNr(MinSource));
    end;
  end else if not MakeMedley then //start this song
  begin
    SetLength(PlaylistMedley.Song, 1);
    PlaylistMedley.Song[0] := Interaction;
    PlaylistMedley.NumMedleySongs := 1;
  end
  else if MakeMedley then
  begin
    if (CatSongs.Song[Interaction].Medley.Source >= MinSource) then
    begin
      AddSong(Interaction);
      PlaylistMedley.NumMedleySongs := Length(PlaylistMedley.Song);
    end;
  end;

  if (Mode = smNormal) and not MakeMedley then
  begin
    Mode := smMedley;

    StopMusicPreview();

    //TODO: how about case 2? menu for medley mode?
    case Ini.OnSongClick of
      0: FadeTo(@ScreenSing);
      1: SelectPlayers;
      2: FadeTo(@ScreenSing);
      {2: begin
         if (CatSongs.CatNumShow = -3) then
           ScreenSongMenu.MenuShow(SM_Playlist)
         else
           ScreenSongMenu.MenuShow(SM_Main);
       end;}
    end;
  end
  else if MakeMedley then
  begin
    if PlaylistMedley.NumMedleySongs = NumSongs then
    begin
      Mode := smMedley;
      StopMusicPreview();

      //TODO: how about case 2? menu for medley mode?
      case Ini.OnSongClick of
        0: FadeTo(@ScreenSing);
        1: SelectPlayers;
        2: FadeTo(@ScreenSing);
        {2: begin
          if (CatSongs.CatNumShow = -3) then
            ScreenSongMenu.MenuShow(SM_Playlist)
          else
            ScreenSongMenu.MenuShow(SM_Main);
        end;}
      end;
    end;
  end;
end;

function TScreenSong.getVisibleMedleyArr(MinSource: TMedleySource): TVisArr;
var
  I:      integer;

begin
  SetLength(Result, 0);
  if CatSongs.Song[Interaction].Main then
  begin
    for I := 0 to Length(CatSongs.Song) - 1 do
    begin
      if not CatSongs.Song[I].Main and (CatSongs.Song[I].Medley.Source >= MinSource) then
      begin
        SetLength(Result, Length(Result)+1);
        Result[Length(Result)-1] := I;
      end;
    end;
  end else
  begin
    for I := 0 to Length(CatSongs.Song) - 1 do
    begin
      if CatSongs.Song[I].Visible and (CatSongs.Song[I].Medley.Source >= MinSource) then
      begin
        SetLength(Result, Length(Result)+1);
        Result[Length(Result)-1] := I;
      end;
    end;
  end;
end;

procedure TScreenSong.SongScore;
begin

  if (CatSongs.Song[Interaction].isDuet) or ((Mode <> smNormal) or (Ini.ShowScores = 0) or (CatSongs.Song[Interaction].Edition = '') or ((Ini.ShowScores = 1) and ((Text[TextMaxScore2].Text = '0') and (Text[TextMaxScoreLocal].Text = '0')))) then
  begin
    Text[TextScore].Visible           := false;
    Text[TextMaxScore].Visible        := false;
    Text[TextMediaScore].Visible      := false;
    Text[TextMaxScore2].Visible       := false;
    Text[TextMediaScore2].Visible     := false;
    Text[TextMaxScoreLocal].Visible   := false;
    Text[TextMediaScoreLocal].Visible := false;
    Text[TextScoreUserLocal].Visible  := false;
    Text[TextScoreUser].Visible       := false;
  end
  else
  begin
    if (Ini.ShowScores = 1) and (Text[TextMaxScoreLocal].Text = '0') and (High(DLLMan.Websites) < 0) then
    begin
      Text[TextScore].Visible           := false;
      Text[TextMaxScore].Visible        := false;
      Text[TextMediaScore].Visible      := false;
    end
    else
    begin
      Text[TextScore].Visible           := true;
      Text[TextMaxScore].Visible        := true;
      Text[TextMediaScore].Visible      := true;
    end;

    if (Ini.ShowScores = 1) and (Text[TextMaxScore2].Text = '0') then
    begin
      Text[TextMaxScore2].Visible       := false;
      Text[TextMediaScore2].Visible     := false;
      Text[TextScoreUser].Visible       := false;
    end
    else
    begin
      Text[TextMaxScore2].Visible       := true;
      Text[TextMediaScore2].Visible     := true;
      Text[TextScoreUser].Visible       := true;
    end;

    if (Ini.ShowScores = 1) and (Text[TextMaxScoreLocal].Text = '0') then
    begin
      Text[TextMaxScoreLocal].Visible   := false;
      Text[TextMediaScoreLocal].Visible := false;
      Text[TextScoreUserLocal].Visible  := false;
    end
    else
    begin
      Text[TextMaxScoreLocal].Visible   := true;
      Text[TextMediaScoreLocal].Visible := true;
      Text[TextScoreUserLocal].Visible  := true;
    end;

  end;

  //Set score
  if (High(DLLMan.Websites) >= 0) then
  begin
    Text[TextScore].Text       := UTF8Encode(DLLMan.Websites[Ini.ShowWebScore].Name);
    Text[TextMaxScore2].Text   := IntToStr(DataBase.ReadMax_Score(CatSongs.Song[Interaction].Artist, CatSongs.Song[Interaction].Title, DllMan.Websites[Ini.ShowWebScore].ID, Ini.Difficulty));
    Text[TextMediaScore2].Text := IntToStr(DataBase.ReadMedia_Score(CatSongs.Song[Interaction].Artist, CatSongs.Song[Interaction].Title, DllMan.Websites[Ini.ShowWebScore].ID, Ini.Difficulty));
    Text[TextScoreUser].Text   := DataBase.ReadUser_Score(CatSongs.Song[Interaction].Artist, CatSongs.Song[Interaction].Title, DllMan.Websites[Ini.ShowWebScore].ID, Ini.Difficulty);
  end;

  Text[TextMaxScoreLocal].Text   := IntToStr(DataBase.ReadMax_ScoreLocal(CatSongs.Song[Interaction].Artist, CatSongs.Song[Interaction].Title, Ini.Difficulty));
  Text[TextMediaScoreLocal].Text := IntToStr(DataBase.ReadMedia_ScoreLocal(CatSongs.Song[Interaction].Artist, CatSongs.Song[Interaction].Title, Ini.Difficulty));
  Text[TextScoreUserLocal].Text  := DataBase.ReadUser_ScoreLocal(CatSongs.Song[Interaction].Artist, CatSongs.Song[Interaction].Title, Ini.Difficulty);

end;


procedure TScreenSong.WriteMessage(msg: UTF8String);
begin

  MessageTime := SDL_GetTicks();

  Statics[InfoMessageBG].Texture.Alpha := 1;
  Text[InfoMessageText].Alpha := 1;

  Statics[InfoMessageBG].Visible := true;
  Text[InfoMessageText].Visible := true;
  Text[InfoMessageText].Text := msg;

end;

procedure TScreenSong.FadeMessage();
var
  factor: real;
begin

  if ((SDL_GetTicks - MessageTime)/1000 > MAX_MESSAGE) then
  begin
    if (MessageTimeFade = 0) then
      MessageTimeFade := SDL_GetTicks();

    factor := (SDL_GetTicks - MessageTimeFade)/1000/2;
    Statics[InfoMessageBG].Texture.Alpha := 1 - factor;
    Text[InfoMessageText].Alpha := 1 - factor;
  end
  else
    MessageTimeFade := 0;

  Statics[InfoMessageBG].Draw;
  Text[InfoMessageText].Draw;

end;

procedure TScreenSong.CloseMessage();
begin
  Statics[InfoMessageBG].Visible := false;
  Text[InfoMessageText].Visible := false;
end;

procedure TScreenSong.ChangeSorting(Tabs: integer; Duet: boolean; Sorting: integer);
var
  I, Count:      integer;
begin
  Ini.Sorting := Sorting;
  Ini.TabsAtStartup := Tabs;

  //ClearButtons();
  CatSongs.Refresh;
  Interaction := 0;
  HideCatTL;
  FixSelected2;
  ChangeMusic;

  Count := 0;
  for I := 0 to High(Button) do
  begin
    //while (CatSongs.Song[Count].Main) do
    //  Count := Count + 1;

    if (CatSongs.Song[I].CoverTex.TexNum > 0) then
      Button[I].Texture := CatSongs.Song[I].CoverTex;
    //else
    //Count := Count + 1;
  end;

  //for I := 0 to High(CatSongs.Song) do
  //if CatSongs.Song[Pet].Cover = PATH_NONE then
  //  AddButton(300 + I*250, 140, 200, 200, Skin.GetTextureFileName('SongCover'), TEXTURE_TYPE_PLAIN, Theme.Song.Cover.Reflections)

  //SetLength(Button, Length(CatSongs.Song));

  //

  //UnLoadDetailedCover;

//  SetScroll;


{
  if (length(CatSongs.Song) > 0) then
  begin
    //Set Length of Button Array one Time Instead of one time for every Song
    SetButtonLength(Length(CatSongs.Song));

    I := 0;
    Pet := 0;
    CreateSongButtons:

    try
      for Pet := I to High(CatSongs.Song) do
      begin // creating all buttons
        if (CatSongs.Song[Pet].CoverTex.TexNum = -1) then
        begin
          Texture.Limit := 512;
          if CatSongs.Song[Pet].Cover = PATH_NONE then
            AddButton(300 + Pet*250, 140, 200, 200, Skin.GetTextureFileName('SongCover'), TEXTURE_TYPE_PLAIN, Theme.Song.Cover.Reflections)
          else
          begin

            Name := CatSongs.Song[Pet].FileName;

            //Name := CatSongs.Song[Pet].Path + CatSongs.Song[Pet].Cover;
            // cache texture if there is a need to this
            if not Covers.CoverExists(Name) then
            begin
              //Texture.CreateCacheMipmap := true;
              //Texture.GetTexture(Name, 'Plain', true); // preloads textures and creates cache mipmap
              //Texture.CreateCacheMipmap := false;

              // puts this texture to the cache file
              Covers.AddCover(Name);

              // unload full size texture
              Texture.UnloadTexture(Name, TEXTURE_TYPE_PLAIN, false);

              // we should also add mipmap texture by calling createtexture and use mipmap cache as data source
            end;

            // and now load it from cache file (small place for the optimization by eliminating reading it from file, but not here)
            AddButton(300 + Pet*250, 140, 200, 200, Name, TEXTURE_TYPE_PLAIN, Theme.Song.Cover.Reflections);
          end;
          Texture.Limit := 1024*1024;
        end;// else
         // AddButton(300 + Pet*250, 140, 200, 200, Theme.Song.Cover.Reflections, CatSongs.Song[Pet].CoverTex);

        I := -1;
      end;
    except
      //When Error is reported the First time for this Song
      if (I <> Pet) then
      begin
        //Change Cover to NoCover and Continue Loading
        CatSongs.Song[Pet].Cover := PATH_NONE;
        I := Pet;
      end
      else //when Error occurs Multiple Times(NoSong Cover is damaged), then start loading next Song
      begin
        Log.LogError('NoCover Cover is damaged!');
        try
          AddButton(300 + Pet*250, 140, 200, 200, PATH_NONE, TEXTURE_TYPE_PLAIN, Theme.Song.Cover.Reflections);
        except
          Halt;
        end;
        I := Pet + 1;
      end;
    end;

    if (I <> -1) then
      GoTo CreateSongButtons;

  end;
  Log.BenchmarkEnd(2);
  Log.LogBenchmark('--> Refresh Create Buttons', 2);

  //FixSelected;

  // set length of button array once instead for every song
  //SetButtonLength(Length(CatSongs.Song));
  //SetLength(Button, Length(CatSongs.Song));

  // create all buttons
  //for I := 0 to High(CatSongs.Song) do
  //begin

    //if not (CatSongs.Song[I].Main) then
    //  Button[I].Texture.TexNum := Button[I].Texture.TexNum;//CatSongs.Song[1].CoverTex.TexNum;

    {
    CoverButton := nil;
    // create a clickable cover
    CoverButtonIndex := AddButton(300 + I*250, 140, 200, 200, PATH_NONE, TEXTURE_TYPE_PLAIN, Theme.Song.Cover.Reflections);
    if (CoverButtonIndex > -1) then
      CoverButton := Button[CoverButtonIndex];

    {
    if (CoverButton = nil) then
      Continue;

    Song := CatSongs.Song[I];

    CoverFile := Song.Path.Append(Song.Cover);
    if (not CoverFile.IsFile()) then
      Song.Cover := PATH_NONE;

    if (Song.Cover.IsUnset) then
      CoverFile := Skin.GetTextureFileName('SongCover');
    {
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
     }
  //end;

  // reset selection
  //if (Length(CatSongs.Song) > 0) then
  //  Interaction := 0;
//end;


  //Count := 0;
  //for I := 0 to High(Button) do
  //begin
  //  while (CatSongs.Song[Count].Main) do
  //    Count := Count + df1;
  //
  //  Button[I].Texture := CatSongs.Song[Count].CoverTex;
  //end;

  {changed := false;

  if (tabs and (Ini.Tabs = 0)) or (not tabs and (Ini.Tabs=1)) then
    changed := true;

  if (sorting <> Ini.Sorting) then
    changed := true;

  if not changed then
    Exit;
  }


  {if not CatSongs.Song[Interaction].Main then
  begin
    Artist := CatSongs.Song[Interaction].Artist;
    Title := CatSongs.Song[Interaction].Title;
    jump := true;
  end else
    jump := false;
  }

  //Refresh;
  //PlaylistMan.LoadPlayLists;

  {
  //if jump then
  //  I2 := PlaylistMan.FindSong(Artist, Title)
  //else
  //begin
    //Find Category
    I := Interaction;
    while not CatSongs.Song[I].Main  do
    begin
      Dec (I);
      if (I < low(CatSongs.Song)) then
        break;
    end;
    if (I<= 1) then
      Interaction := high(catsongs.Song)
    else
      Interaction := I - 1;

    HideCatTL;

    //Show Wrong Song when Tabs on Fix
    SelectNext;
    FixSelected;
    ChangeMusic;
  //end;

  if (Ini.Tabs=1) and not (CatSongs.CatNumShow = -3) and jump then
  begin
    //Search Cat
    for I := I2 downto low(CatSongs.Song) do
    begin
      if CatSongs.Song[I].Main then
        break;
    end;

    //Choose Cat
    CatSongs.ShowCategoryList;
    ShowCatTL(I);
    CatSongs.ClickCategoryButton(I);
  end;

  //Choose Song
  if jump then
  begin
    SkipTo(I2);
    SongCurrent := SongTarget;
    ChangeMusic;
  end;

  if (Ini.Tabs=0) then
    HideCatTL;

  Ini.Save;
  }

end;

end.
