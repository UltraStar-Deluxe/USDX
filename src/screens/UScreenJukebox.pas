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
 * $URL: $
 * $Id:  $
 *}

unit UScreenJukebox;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  SDL,
  TextGL,
  gl,
  UCommon,
  UFiles,
  UGraphicClasses,
  UIni,
  ULog,
  ULyrics,
  UMenu,
  UMusic,
  UPlaylist,
  USingScores,
  USongs,
  UTexture,
  UThemes,
  UPath,
  UTime,
  UHookableEvent,
  UVideo;

type
  TSongJukebox = class
  public
    Id: integer;

    // sorting methods
    Genre:      UTF8String;
    Edition:    UTF8String;
    Language:   UTF8String;
    Year:       Integer;

    Title:      UTF8String;
    Artist:     UTF8String;

  end;

  THandler = record
    changed:  boolean;
    change_time:  real;
  end;

  TLyricsSyncSource = class(TSyncSource)
    function GetClock(): real; override;
  end;

  TMusicSyncSource = class(TSyncSource)
    function GetClock(): real; override;
  end;

  TTimebarMode = (
    tbmCurrent,   // current song position
    tbmRemaining, // remaining time
    tbmTotal      // total time
  );

type
  TScreenJukebox = class(TMenu)
  private
    // items
    JukeboxStaticTimeProgress:       integer;
    JukeboxStaticTimeBackground:     integer;
    JukeboxStaticSongBackground:     integer;
    JukeboxStaticSongListBackground: integer;
    SongDescription: array[0..9] of integer;

    SelectColR: real;
    SelectColG: real;
    SelectColB: real;

    // options desc
    JukeboxStaticOptions: integer;
    JukeboxTextOptionsSongPosition: integer;
    JukeboxTextOptionsLyric: integer;
    JukeboxTextOptionsRandom: integer;
    JukeboxTextOptionsRepeat: integer;
    JukeboxTextOptionsFind: integer;
    JukeboxTextOptionsSort: integer;

    JukeboxTextTimeText: integer;
    JukeboxTextTimeDesc: integer;
    JukeboxTextSongText: integer;    //Button of Songtitle

    SongFinish: boolean;

    tmpLyricsUpperY: real;
    tmpLyricsLowerY: real;
    //tmp_mouse: integer;

    LyricsStart:     boolean;

    JukeboxFindSong: integer;
    JukeboxRepeatSongList: integer;
    JukeboxSongListOrder: integer;
    JukeboxRandomSongList: integer;
    JukeboxListText: integer;
    JukeboxCountText: integer;
    JukeboxLyric: integer;

    Filter:         UTF8String;

    FindSongList:   boolean;
    RepeatSongList: boolean;
    RandomMode:     boolean;
    OrderMode:      boolean;
    OrderType:      integer;

    fShowVisualization: boolean;
    fCurrentVideo: IVideo;
    fVideoClip:    IVideo;
    fLyricsSync: TLyricsSyncSource;
    fMusicSync: TMusicSyncSource;
    fTimebarMode: TTimebarMode;

  protected
    eSongLoaded: THookableEvent; //< event is called after lyrics of a song are loaded on OnShow
    Paused:     boolean; //pause Mod
    NumEmptySentences: integer;
  public
    ShowLyrics: boolean;
    CurrentSongList: integer;
    LastTick: cardinal;
    SongListVisible: boolean;

    ChangePosition: integer;

    JukeboxSongsList: array of integer;
    JukeboxVisibleSongs: array of integer;

    ActualInteraction: integer;
    ListMin:           integer;
    CurrentSongID:  integer;

    //VideoAspect
    VideoAspectText:    integer;
    VideoAspectStatic:  integer;
    AspectHandler:      THandler;
    AspectCorrection: TAspectCorrection;

    StaticPausePopup: integer;

    Tex_Background: TTexture;
    FadeOut: boolean;
    Lyrics:  TLyricEngine;

    StaticCover: integer;

    constructor Create; override;
    procedure OnShow; override;
    procedure OnShowFinish; override;
    procedure OnHide; override;

    function ParseInput(PressedKey: cardinal; CharCode: UCS4Char;
      PressedDown: boolean): boolean; override;
    function Draw: boolean; override;
    procedure DrawBlackBars();
    procedure DrawItems();

    procedure PlayMusic(ID: integer);
    procedure Play;
    procedure Finish;
    procedure Pause; // toggle pause

    procedure OnSentenceEnd(SentenceIndex: cardinal);     // for linebonus + singbar
    procedure OnSentenceChange(SentenceIndex: cardinal);  // for golden notes

    //procedure DeleteSong(Id: integer);
    procedure FilterSongList(Filter: UTF8String);
    procedure SongListSort(Order: integer);
    procedure Sort(Order: integer);
    procedure Reset;

    procedure AddSongToJukeboxList(ID: integer);
    function FinishedMusic: boolean;

    procedure RefreshCover;
    procedure DrawPlaylist;
  end;

implementation

uses
  Classes,
  Math,
  UDraw,
  UGraphic,
  ULanguage,
  UNote,
  URecord,
  USong,
  UDisplay,
  UParty,
  UScreenSong,
  UUnicodeUtils;

procedure TScreenJukebox.DrawBlackBars();
var
  X, X1, Y, Y1, Z, H, W: double;
begin
  fCurrentVideo.GetScreenPosition(X, Y, Z);

  // Upper
  X1 := 0;
  Y1 := 0;
  H := Y + 1;
  W := 800;

  glColor4f(0, 0, 0, 1);
  glbegin(gl_quads);
    glVertex2f(X1, Y1);
    glVertex2f(X1, Y1 + H);
    glVertex2f(X1 + W, Y1 + H);
    glVertex2f(X1 + W, Y1);
  glEnd;

  // Bottom
  X1 := 0;
  Y1 := 600;
  H := Y + 1;
  W := 800;

  glColor4f(0, 0, 0, 1);
  glbegin(gl_quads);
    glVertex2f(X1, Y1);
    glVertex2f(X1, Y1 - H);
    glVertex2f(X1 + W, Y1 - H);
    glVertex2f(X1 + W, Y1);
  glEnd;

  // Left
  X1 := 0;
  Y1 := 0;
  H := 600;
  W := X + 1;

  glColor4f(0, 0, 0, 1);
  glbegin(gl_quads);
    glVertex2f(X1, Y1);
    glVertex2f(X1, Y1 + H);
    glVertex2f(X1 + W, Y1 + H);
    glVertex2f(X1 + W, Y1);
  glEnd;

  // Right
  X1 := 800;
  Y1 := 0;
  H := 600;
  W := X + 1;

  glColor4f(0, 0, 0, 1);
  glbegin(gl_quads);
    glVertex2f(X1, Y1);
    glVertex2f(X1, Y1 + H);
    glVertex2f(X1 - W, Y1 + H);
    glVertex2f(X1 - W, Y1);
  glEnd;

end;

procedure TScreenJukebox.SongListSort(Order: integer);
begin

  case Order of
    1 : begin
          Button[JukeboxSongListOrder].Text[0].Text := Language.Translate('OPTION_VALUE_ARTIST');
          Sort(2);
          Sort(1);
        end;
    2 : begin
          Button[JukeboxSongListOrder].Text[0].Text := Language.Translate('OPTION_VALUE_TITLE');
          Sort(1);
          Sort(2);
        end;
    3 : begin
          Button[JukeboxSongListOrder].Text[0].Text := Language.Translate('OPTION_VALUE_EDITION');
          Sort(2);
          Sort(1);
          Sort(3);
        end;
    4 : begin
          Button[JukeboxSongListOrder].Text[0].Text := Language.Translate('OPTION_VALUE_GENRE');
          Sort(2);
          Sort(1);
          Sort(4);
        end;
    5 : begin
          Button[JukeboxSongListOrder].Text[0].Text := Language.Translate('OPTION_VALUE_LANGUAGE');
          Sort(2);
          Sort(1);
          Sort(5);
        end;
  end;

end;

procedure TScreenJukebox.Sort(Order: integer);
var
  I, J, X, Tmp, Comp: integer;
  Text: UTF8String;
  NotEnd: boolean;
begin

  for I:= 0 to High(JukeboxVisibleSongs) do
  begin
    J := I;
    X := JukeboxVisibleSongs[I];
    NotEnd := true;
    while (J > 0) and (NotEnd) do
    begin

      case Order of
        1 : Comp := UTF8CompareText(CatSongs.Song[X].Artist, CatSongs.Song[JukeboxVisibleSongs[J - 1]].Artist);
        2 : Comp := UTF8CompareText(CatSongs.Song[X].Title, CatSongs.Song[JukeboxVisibleSongs[J - 1]].Title);
        3 : Comp := UTF8CompareText(CatSongs.Song[X].Edition, CatSongs.Song[JukeboxVisibleSongs[J - 1]].Edition);
        4 : Comp := UTF8CompareText(CatSongs.Song[X].Genre, CatSongs.Song[JukeboxVisibleSongs[J - 1]].Genre);
        5 : Comp := UTF8CompareText(CatSongs.Song[X].Language, CatSongs.Song[JukeboxVisibleSongs[J - 1]].Language);
      end;

      if (Comp < 0) then
      begin
        JukeboxVisibleSongs[J] := JukeboxVisibleSongs[J - 1];
        J := J - 1;
      end
      else
        NotEnd := false;
    end;

    JukeboxVisibleSongs[J] := X;
  end;
end;

procedure TScreenJukebox.FilterSongList(Filter: UTF8String);
var
  I: integer;
  SongD: UTF8String;
begin

  if (Filter <> '') then
  begin
    SetLength(JukeboxVisibleSongs, 0);
    for I := 0 to High(JukeboxSongsList) do
    begin
      SongD := CatSongs.Song[JukeboxSongsList[I]].Artist + ' - ' + CatSongs.Song[JukeboxSongsList[I]].Title;

      if (UTF8ContainsStr(UTF8UpperCase(SongD), UTF8UpperCase(Filter))) then
      begin
        SetLength(JukeboxVisibleSongs, Length(JukeboxVisibleSongs) + 1);
        JukeboxVisibleSongs[High(JukeboxVisibleSongs)] := JukeboxSongsList[I];
      end;
    end;
  end
  else
  begin
    SetLength(JukeboxVisibleSongs, 0);

    for I := 0 to High(JukeboxSongsList) do
    begin
      SetLength(JukeboxVisibleSongs, Length(JukeboxVisibleSongs) + 1);
      JukeboxVisibleSongs[High(JukeboxVisibleSongs)] := JukeboxSongsList[I];
    end;
  end;

  ActualInteraction := 0;
  Interaction := 0;
  ListMin := 0;
end;

{
procedure TScreenJukebox.DeleteSong(Id: integer);
var
  I: integer;
  JukeboxSongsListTmp: array of integer;
  JukeboxVisibleSongsTmp: array of integer;
begin

  SetLength(JukeboxSongsListTmp, 0);

  for I := 0 to High(JukeboxSongsList) do
  begin
    if (I <> Id) then
    begin
      SetLength(JukeboxSongsListTmp, Length(JukeboxSongsListTmp) + 1);
      JukeboxSongsListTmp[High(JukeboxSongsListTmp)] := JukeboxSongsList[I];
    end;
  end;

  SetLength(JukeboxSongsList, Length(JukeboxSongsListTmp));
  for I := 0 to High(JukeboxSongsListTmp) do
    JukeboxSongsList[I] := JukeboxSongsListTmp[I];

  SetLength(JukeboxVisibleSongsTmp, 0);
  for I := 0 to High(JukeboxVisibleSongs) do
  begin
    if (I <> Id) then
    begin
      SetLength(JukeboxVisibleSongsTmp, Length(JukeboxVisibleSongsTmp) + 1);
      JukeboxVisibleSongsTmp[High(JukeboxVisibleSongsTmp)] := JukeboxVisibleSongs[I];
    end;
  end;

  SetLength(JukeboxVisibleSongs, Length(JukeboxVisibleSongsTmp));
  for I := 0 to High(JukeboxVisibleSongsTmp) do
    JukeboxVisibleSongs[I] := JukeboxVisibleSongsTmp[I];


//  ActualInteraction := 0;
//  Interaction := 0;
//  ListMin := 0;
end;
}

procedure TScreenJukebox.Reset;
begin
  CurrentSongList := 0;

  Interaction       := 0;
  ActualInteraction := 0;
  ListMin           := 0;
  //RepeatSongList    := false;
  RandomMode        := false;
  OrderMode         := true;
  FindSongList      := false;
  Filter            := '';
  ShowLyrics        := true;

  Button[JukeboxSongListOrder].SetSelect(true);

  case (Ini.Sorting) of
    5: begin
         OrderType := 1;
         Button[JukeboxSongListOrder].Text[0].Text := Language.Translate('OPTION_VALUE_ARTIST');
       end;
    6: begin
         OrderType := 2;
         Button[JukeboxSongListOrder].Text[0].Text := Language.Translate('OPTION_VALUE_TITLE');
       end;
    0: begin
         OrderType := 3;
         Button[JukeboxSongListOrder].Text[0].Text := Language.Translate('OPTION_VALUE_EDITION');
       end;
    1: begin
         OrderType := 4;
         Button[JukeboxSongListOrder].Text[0].Text := Language.Translate('OPTION_VALUE_GENRE');
       end;
    2: begin
         OrderType := 5;
         Button[JukeboxSongListOrder].Text[0].Text := Language.Translate('OPTION_VALUE_LANGUAGE');
       end;
    else
    begin
      OrderType := 1;
      OrderMode := false;
      Button[JukeboxSongListOrder].SetSelect(false);
      try
         Button[JukeboxSongListOrder].Text[0].Text := Language.Translate('OPTION_VALUE_ARTIST');
      finally
      end;

    end;
  end;

  Button[JukeboxFindSong].Text[0].Text := '';

  Button[JukeboxLyric].SetSelect(true);
  Button[JukeboxRandomSongList].SetSelect(false);
  Button[JukeboxRepeatSongList].SetSelect(false);
  Button[JukeboxFindSong].SetSelect(false);
end;

procedure OnEscapeJukebox(Value: boolean; Data: Pointer);
var
  tmp: integer;
begin
  Display.CheckOK := Value;
  if (Value) then
  begin
    Display.CheckOK := false;

    ScreenJukebox.RepeatSongList := false;

    ScreenJukebox.CurrentSongList := High(ScreenJukebox.JukeboxVisibleSongs);

    ScreenJukebox.Finish;
    ScreenJukebox.FadeOut := true;

    AudioPlayback.PlaySound(SoundLib.Back);

  end;
end;

// method for input parsing. if false is returned, getnextwindow
// should be checked to know the next window to load;

function TScreenJukebox.ParseInput(PressedKey: Cardinal; CharCode: UCS4Char;
  PressedDown: boolean): boolean;
var
  SDL_ModState: word;
  I, RValueI, RValueE: integer;
  tmp: integer;
  X, Y, Z: double;
begin
  Result := true;

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT +
    KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT + KMOD_RALT);

  if (PressedDown) then
  begin // key down
    // check normal keys

    if (FindSongList) and (SongListVisible) then
    begin
      if (IsPrintableChar(CharCode)) then
      begin
        LastTick := SDL_GetTicks();

        Button[JukeboxFindSong].Text[0].Text := Button[JukeboxFindSong].Text[0].Text +
                                          UCS4ToUTF8String(CharCode);

        Filter := Button[JukeboxFindSong].Text[0].Text;
        FilterSongList(Filter);
        Exit;
      end;
    end
    else
    begin
      case UCS4UpperCase(CharCode) of
        Ord('Q'):
        begin
          // when not ask before exit then finish now
          if (Ini.AskbeforeDel <> 1) then
            Finish
          // else just pause and let the popup make the work
          else if not Paused then
            Pause;

          Result := false;
          Exit;
        end;

        // show visualization
        Ord('V'):
        begin
          fShowVisualization := not fShowVisualization;

          if fShowVisualization then
          begin
            fCurrentVideo := Visualization.Open(PATH_NONE);
            fCurrentVideo.play;
          end
          else
          begin
            fCurrentVideo := fVideoClip;
          end;
          Exit;
        end;

        // pause
        Ord('P'):
        begin
          Pause;
          Exit;
        end;

        // toggle time display
        Ord('T'):
        begin
          LastTick := SDL_GetTicks();

          if (fTimebarMode = High(TTimebarMode)) then
            fTimebarMode := Low(TTimebarMode)
          else
            Inc(fTimebarMode);
          Exit;
        end;

        // allow search for songs
        Ord('J'):
        begin
          LastTick := SDL_GetTicks();
          FindSongList := not FindSongList;
          if (Filter = '') then
          begin
            if (FindSongList) then
              Button[JukeboxFindSong].Text[0].Text := '';
          end;
          Button[JukeboxFindSong].SetSelect(FindSongList);
          if FindSongList then
            FilterSongList(Filter)
          else
            FilterSongList('');
          Exit;
        end;

        //Randomixe Playlist
        Ord('R'):
        begin
          if (SongListVisible) then
          begin
            LastTick := SDL_GetTicks();

            Button[JukeboxRandomSongList].SetSelect(true);
            Button[JukeboxSongListOrder].SetSelect(false);

            RandomMode := true;
            OrderMode := false;

            for I := 0 to High(JukeboxVisibleSongs) * 2 do
            begin
              RValueI := RandomRange(0, High(JukeboxVisibleSongs) + 1);
              RValueE := RandomRange(0, High(JukeboxVisibleSongs) + 1);

              tmp := JukeboxVisibleSongs[RValueI];
              JukeboxVisibleSongs[RValueI] := JukeboxVisibleSongs[RValueE];
              JukeboxVisibleSongs[RValueE] := tmp;

              if (RValueI = CurrentSongList) then
                CurrentSongList := RValueE
              else
              begin
                if (RValueE = CurrentSongList) then
                  CurrentSongList := RValueI;
              end;
            end;
          end;
        end;
      end;
     end;

      // check special keys
     case PressedKey of
        SDLK_A:
        begin
          // change aspect ratio
          if (SDL_ModState = KMOD_LSHIFT) then
          begin
            if (AspectCorrection = acoCrop) then
              AspectCorrection := acoStretch
            else
            begin
              if (AspectCorrection = acoStretch) then
                AspectCorrection := acoLetterBox
              else
              begin
                if (AspectCorrection = acoLetterBox) then
                  AspectCorrection := acoCrop;
              end;
            end;

            //fCurrentVideo.ToggleAspectCorrection();
            //AspectHandler.changed := true;
            //AspectHandler.change_time := Czas.Teraz;
            //Static[VideoAspectStatic].Visible := true;
            //case UVideo.fAspectCorrection of
            //  acoStretch: Text[VideoAspectText].Text := Language.Translate('VIDEO_ASPECT_STRETCH');
            //  acoCrop: Text[VideoAspectText].Text := Language.Translate('VIDEO_ASPECT_CROP');
            //  acoLetterBox: Text[VideoAspectText].Text := Language.Translate('VIDEO_ASPECT_LETTER_BOX');
            //end;
            //DataBase.SetAspect(AktSong.Artist, AktSong.Title, integer(UVideo.fAspectCorrection));
            //Text[VideoAspectText].Visible := true;

            //fCurrentVideo.Draw;
          end;
        end;

        SDLK_L:
        begin
          if (SDL_ModState = KMOD_LCTRL) then
          begin
            LastTick := SDL_GetTicks();

            ShowLyrics := not ShowLyrics;
            Button[JukeboxLyric].SetSelect(ShowLyrics);
            Exit;
          end;
        end;

        SDLK_S:
        begin

          if (SongListVisible) and (SDL_ModState = KMOD_LCTRL) then
          begin
            LastTick := SDL_GetTicks();

            Button[JukeboxRandomSongList].SetSelect(false);
            Button[JukeboxSongListOrder].SetSelect(true);

            if (OrderMode) then
            begin
              if (OrderType < 5) then
              begin
                OrderType := OrderType + 1;
              end
              else
                OrderType := 1;
            end;

            RandomMode := false;
            OrderMode := true;

            SongListSort(OrderType);

            Exit;
          end;

        end;

        // repeat songlist
        SDLK_X:
        begin
          if (SDL_ModState = KMOD_LCTRL) then
          begin
            LastTick := SDL_GetTicks();

            RepeatSongList := not RepeatSongList;
            Button[JukeboxRepeatSongList].SetSelect(RepeatSongList);
            Exit;
          end;
        end;

        SDLK_ESCAPE:
        begin
          if (SongListVisible) then
            SongListVisible := false
          else
            ScreenPopupCheck.ShowPopup('MSG_END_JUKEBOX', OnEscapeJukebox, nil, false)
        end;

        SDLK_BACKSPACE:
        begin

          if (FindSongList) and (SongListVisible) then
          begin
            LastTick := SDL_GetTicks();
            Button[JukeboxFindSong].Text[0].DeleteLastLetter();
            Filter := Button[JukeboxFindSong].Text[0].Text;
            FilterSongList(Filter);
          end
          else
          begin
            ScreenPopupCheck.ShowPopup('MSG_END_JUKEBOX', OnEscapeJukebox, nil, false)
          end;
        end;

        SDLK_SPACE:
        begin
          Pause;
        end;

        SDLK_TAB: // change visualization preset
        begin
          if fShowVisualization then
            fCurrentVideo.Position := now; // move to a random position
        end;

        SDLK_RETURN:
        begin
          if (SongListVisible) then
          begin
            LastTick := SDL_GetTicks();
            CurrentSongList := ActualInteraction - 1;
            Finish;
            PlayMusic(CurrentSongList);
          end;
        end;

        SDLK_LEFT:
        begin
          {if not (SongListVisible) and (CurrentSongList > 0) then
          begin
            CurrentSongList := CurrentSongList - 2;
            PlayMusic(CurrentSongList);
          end;
           }
          if (SDL_ModState = KMOD_LSHIFT) then
          begin
            fCurrentVideo.GetScreenPosition(X, Y, Z);
            if (X < 200) then
            begin
              fCurrentVideo.SetScreenPosition(X + 2, Y, Z);
              fCurrentVideo.SetWidth(fCurrentVideo.GetWidth - 4);
            end;
          end;

        end;

        SDLK_RIGHT:
        begin
          {
          if not (SongListVisible) and (CurrentSongList < High(JukeboxVisibleSongs)) then
          begin
            CurrentSongList := CurrentSongList + 1;
            PlayMusic(CurrentSongList);
          end;
          }

          if (SDL_ModState = KMOD_LSHIFT) then
          begin
            fCurrentVideo.GetScreenPosition(X, Y, Z);
            fCurrentVideo.SetScreenPosition(X - 2, Y, Z);
            fCurrentVideo.SetWidth(fCurrentVideo.GetWidth + 4);
          end;

        end;

        SDLK_DELETE:
        begin
          if (SongListVisible) then
          begin
           // DeleteSong(ActualInteraction);
          end;
        end;

        SDLK_PAGEDOWN:
        begin
          {if (SongListVisible) and ((ActualInteraction + 10) < High(JukeboxVisibleSongs)) then
          begin

            LastTick := SDL_GetTicks();

            // TODO: BUG AT END
            if (ListMin + 10 < High(JukeboxVisibleSongs) - 9 + Interaction) then
            begin
              ListMin := ListMin + 10;
              ActualInteraction := ActualInteraction + 10;
            end
            else
            begin
              ListMin := High(JukeboxVisibleSongs) - 9 + Interaction;
              ActualInteraction := High(JukeboxVisibleSongs) - 9 + Interaction;
            end;

          end;}
        end;

        SDLK_PAGEUP:
        begin
        {
          if (SongListVisible) and (ActualInteraction - 9 > 0) then
          begin
            if (ListMin > 10) then
              ListMin := ListMin - 10
            else
              ListMin := 0;

            ActualInteraction := ActualInteraction - 10;

            if (ActualInteraction < 10) then
              Interaction := ActualInteraction;

            LastTick := SDL_GetTicks();
          end;}
        end;

        // up and down could be done at the same time,
        // but i don't want to declare variables inside
        // functions like this one, called so many times

        SDLK_DOWN:
        begin
          if (SongListVisible) then
          begin
            LastTick := SDL_GetTicks();

            if (SDL_ModState = KMOD_LCTRL) and (ActualInteraction < High(JukeboxVisibleSongs)) then
            begin
              Button[JukeboxSongListOrder].SetSelect(false);
              OrderMode := false;

              tmp := JukeboxVisibleSongs[ActualInteraction];
              JukeboxVisibleSongs[ActualInteraction] := JukeboxVisibleSongs[ActualInteraction + 1];
              JukeboxVisibleSongs[ActualInteraction + 1] := tmp;

              if (ActualInteraction + 1 = CurrentSongList) then
                CurrentSongList := CurrentSongList - 1
              else
              begin
                if (ActualInteraction = CurrentSongList) then
                  CurrentSongList := ActualInteraction + 1;
              end;
            end;

            if not(SDL_ModState = KMOD_LSHIFT) and not(SDL_ModState = KMOD_LALT) and (ActualInteraction < High(JukeboxVisibleSongs)) then
            begin
              ActualInteraction := ActualInteraction + 1;

              if (Interaction = 9) then
                ListMin := ListMin + 1
              else
                InteractInc;

            end;
          end;

          if not(SDL_ModState = KMOD_LALT) and not(SDL_ModState = KMOD_LSHIFT) and (not SongListVisible) then
          begin
            SongListVisible := true;
            LastTick := SDL_GetTicks();
          end;

          if (SDL_ModState = KMOD_LALT) then
          begin
            Lyrics.UpperLineY := Lyrics.UpperLineY + 2;
            Lyrics.LowerLineY := Lyrics.LowerLineY + 2;
            ChangePosition := ChangePosition + 2;
          end;

          if (SDL_ModState = KMOD_LSHIFT) then
          begin
            fCurrentVideo.GetScreenPosition(X, Y, Z);
            if (Y < 200) then
            begin
              fCurrentVideo.SetScreenPosition(X, Y + 2, Z);
              fCurrentVideo.SetHeight(fCurrentVideo.GetHeight - 4);
            end;
          end;

        end;
        SDLK_UP:
        begin
          if (SongListVisible) and (ActualInteraction > 0) then
          begin
            LastTick := SDL_GetTicks();

            if (SDL_ModState = KMOD_LCTRL) and (ActualInteraction > 0) then
            begin
              Button[JukeboxSongListOrder].SetSelect(false);
              OrderMode := false;

              tmp := JukeboxVisibleSongs[ActualInteraction];
              JukeboxVisibleSongs[ActualInteraction] := JukeboxVisibleSongs[ActualInteraction - 1];
              JukeboxVisibleSongs[ActualInteraction - 1] := tmp;

              if (ActualInteraction - 1 = CurrentSongList) then
                CurrentSongList := CurrentSongList + 1
              else
              begin
                if (ActualInteraction = CurrentSongList) then
                  CurrentSongList := ActualInteraction - 1;
              end;
            end;

            if not(SDL_ModState = KMOD_LSHIFT) and not(SDL_ModState = KMOD_LALT) then
            begin
              ActualInteraction := ActualInteraction - 1;

              if (Interaction = 0) then
                ListMin := ListMin - 1
              else
                InteractDec;
            end;
          end;

          if not(SDL_ModState = KMOD_LALT) and not(SDL_ModState = KMOD_LSHIFT) and (not SongListVisible) then
          begin
            SongListVisible := true;
            LastTick := SDL_GetTicks();
          end;

          if not(SDL_ModState = KMOD_LSHIFT) and (SDL_ModState = KMOD_LALT) then
          begin
            Lyrics.UpperLineY := Lyrics.UpperLineY - 2;
            Lyrics.LowerLineY := Lyrics.LowerLineY - 2;
            ChangePosition := ChangePosition - 2;
          end;

          if (SDL_ModState = KMOD_LSHIFT) then
          begin
            fCurrentVideo.GetScreenPosition(X, Y, Z);
            fCurrentVideo.SetScreenPosition(X, Y - 2, Z);
            fCurrentVideo.SetHeight(fCurrentVideo.GetHeight + 4);
          end;

        end;

      end;
    end;
end;

// pause mod
procedure TScreenJukebox.Pause;
begin
  if (not Paused) then  // enable pause
  begin
    // pause time
    Paused := true;

    LyricsState.Pause();

    // pause music
    AudioPlayback.Pause;

    // pause video
    if (fCurrentVideo <> nil) then
      fCurrentVideo.Pause;

  end
  else              // disable pause
  begin
    LyricsState.Start();

    // play music
    AudioPlayback.Play;

    // video
    if (fCurrentVideo <> nil) then
      fCurrentVideo.Pause;

    Paused := false;
  end;
end;
// pause mod end

constructor TScreenJukebox.Create;
var
  I, PosY: integer;
begin
  inherited Create;

  SongListVisible := false;
  ListMin := 0;
  ShowLyrics := false;

  //too dangerous, a mouse button is quickly pressed by accident
  RightMbESC := false;

  fShowVisualization := false;

  fCurrentVideo := nil;

  LoadFromTheme(Theme.Jukebox);

  StaticPausePopup := AddStatic(Theme.Sing.PausePopUp);

  // <note> pausepopup is not visibile at the beginning </note>
  Statics[StaticPausePopup].Visible := false;

  Lyrics := TLyricEngine.Create(
      Theme.LyricBar.UpperX, Theme.LyricBar.UpperY, Theme.LyricBar.UpperW, Theme.LyricBar.UpperH,
      Theme.LyricBar.LowerX, Theme.LyricBar.LowerY, Theme.LyricBar.LowerW, Theme.LyricBar.LowerH);

  fLyricsSync := TLyricsSyncSource.Create();
  fMusicSync := TMusicSyncSource.Create();

  //eSongLoaded := THookableEvent.Create('ScreenSing.SongLoaded');

  //Jukebox Items
  JukeboxStaticTimeProgress       := AddStatic(Theme.Jukebox.StaticTimeProgress);
  JukeboxStaticTimeBackground     := AddStatic(Theme.Jukebox.StaticTimeBackground);
  JukeboxStaticSongBackground     := AddStatic(Theme.Jukebox.StaticSongBackground);
  JukeboxStaticSongListBackground := AddStatic(Theme.Jukebox.StaticSongListBackground);

  JukeboxTextTimeText         := AddText(Theme.Jukebox.TextTimeText);
  JukeboxTextTimeDesc         := AddText(Theme.Jukebox.TextTimeDesc);
  JukeboxTextSongText         := AddText(Theme.Jukebox.TextSongText);

  PosY := Theme.Jukebox.SongDescription.Y;
  for I := 0 to 9 do
  begin
    Theme.Jukebox.SongDescription.Y := PosY + Theme.Jukebox.SongDescription.H * I;
    SongDescription[I] := AddButton(Theme.Jukebox.SongDescription);
  end;

  SelectColR := Theme.Jukebox.SongDescription.ColR;
  SelectColG := Theme.Jukebox.SongDescription.ColG;
  SelectColB := Theme.Jukebox.SongDescription.ColB;

  JukeboxFindSong := AddButton(Theme.Jukebox.FindSong);
  JukeboxRepeatSongList := AddButton(Theme.Jukebox.RepeatSongList);
  JukeboxSongListOrder := AddButton(Theme.Jukebox.SongListOrder);
  JukeboxRandomSongList := AddButton(Theme.Jukebox.RandomSongList);
  JukeboxLyric := AddButton(Theme.Jukebox.Lyric);

  Button[JukeboxFindSong].Selectable := false;
  Button[JukeboxRepeatSongList].Selectable := false;
  Button[JukeboxSongListOrder].Selectable := false;
  Button[JukeboxRandomSongList].Selectable := false;
  Button[JukeboxLyric].Selectable := false;

  JukeboxListText  := AddText(Theme.Jukebox.TextListText);
  JukeboxCountText := AddText(Theme.Jukebox.TextCountText);

  StaticCover := AddStatic(Theme.Jukebox.SongCover);

  JukeboxStaticOptions := AddStatic(Theme.Jukebox.StaticOptions);
  JukeboxTextOptionsSongPosition := AddText(Theme.Jukebox.TextOptionsSongPosition);
  JukeboxTextOptionsLyric := AddText(Theme.Jukebox.TextOptionsLyric);
  JukeboxTextOptionsRandom := AddText(Theme.Jukebox.TextOptionsRandom);
  JukeboxTextOptionsRepeat := AddText(Theme.Jukebox.TextOptionsRepeat);
  JukeboxTextOptionsFind := AddText(Theme.Jukebox.TextOptionsFind);
  JukeboxTextOptionsSort := AddText(Theme.Jukebox.TextOptionsSort);
end;

procedure TScreenJukebox.OnShow;
var
  V1:     boolean;
  V1TwoP: boolean;   // position of score box in two player mode
  V1ThreeP: boolean; // position of score box in three player mode
  V2R:    boolean;
  V2M:    boolean;
  V3R:    boolean;
  Color: TRGB;
begin
  inherited;

  Log.LogStatus('Begin', 'OnShow');

  {**
  * Pause background music
  *}
  SoundLib.PauseBgMusic;

  FadeOut := false;

  AspectCorrection := acoLetterBox;

  ChangePosition := 0;

  Lyrics.UpperLineX := Theme.LyricBarJukebox.UpperX;
  Lyrics.UpperLineY := Theme.LyricBarJukebox.UpperY;
  Lyrics.UpperLineW := Theme.LyricBarJukebox.UpperW;
  Lyrics.UpperLineH := Theme.LyricBarJukebox.UpperH;

  Lyrics.LowerLineX := Theme.LyricBarJukebox.LowerX;
  Lyrics.LowerLineY := Theme.LyricBarJukebox.LowerY;
  Lyrics.LowerLineW := Theme.LyricBarJukebox.LowerW;
  Lyrics.LowerLineH := Theme.LyricBarJukebox.LowerH;

  tmpLyricsUpperY := Lyrics.UpperLineY;
  tmpLyricsLowerY := Lyrics.LowerLineY;

  Lyrics.FontStyle := ftOutline1;
  Lyrics.LineColor_en.R := 1;
  Lyrics.LineColor_en.G := 1;
  Lyrics.LineColor_en.B := 1;
  Lyrics.LineColor_en.A := 1;

  Lyrics.LineColor_dis.R := 1;
  Lyrics.LineColor_dis.G := 1;
  Lyrics.LineColor_dis.B := 1;
  Lyrics.LineColor_dis.A := 1;

  Lyrics.LineColor_act.R := 1;
  Lyrics.LineColor_act.G := 0.75;
  Lyrics.LineColor_act.B := 0;
  Lyrics.LineColor_act.A := 1;

  Log.LogStatus('End', 'OnShow');
end;

procedure TScreenJukebox.OnShowFinish();
begin
  // hide cursor on singscreen show
  Display.SetCursor;

  Reset;

  PlayMusic(0);
end;

procedure TScreenJukebox.Play();
var
  I: integer;
begin
    AudioPlayback.Open(CurrentSong.Path.Append(CurrentSong.Mp3));
    AudioPlayback.SetVolume(1.0);

    //AudioPlayback.Position := CurrentSong.Start;
    AudioPlayback.Position := LyricsState.GetCurrentTime();

  // set time
  if (CurrentSong.Finish > 0) then
    LyricsState.TotalTime := CurrentSong.Finish / 1000
  else
  begin
       LyricsState.TotalTime := AudioPlayback.Length;
  end;

  LyricsState.UpdateBeats();

  // synchronize music
  if (Ini.SyncTo = Ord(stLyrics)) then
    AudioPlayback.SetSyncSource(fLyricsSync)
  else
    AudioPlayback.SetSyncSource(nil);

  // synchronize lyrics (do not set this before AudioPlayback is initialized)
  if (Ini.SyncTo = Ord(stMusic)) then
    LyricsState.SetSyncSource(fMusicSync)
  else
    LyricsState.SetSyncSource(nil);

  // start lyrics
  LyricsState.Start(true);

  // start music
  AudioPlayback.Play();

  // start timer
  CountSkipTimeSet;

  LastTick := SDL_GetTicks();

end;

procedure TScreenJukebox.OnHide;
begin
  // background texture
  if (Tex_Background.TexNum > 0) then
  begin
    glDeleteTextures(1, PGLuint(@Tex_Background.TexNum));
    Tex_Background.TexNum := 0;
  end;

  Background.OnFinish;
  Display.SetCursor;
end;

function TScreenJukebox.FinishedMusic: boolean;
begin
  Result := AudioPlayback.Finished;
end;

function TScreenJukebox.Draw: boolean;
var
  DisplayTime:  real;
  DisplayPrefix: string;
  DisplayMin:   integer;
  DisplaySec:   integer;
  CurLyricsTime: real;
  VideoFrameTime: Extended;
  Line: TLyricLine;
  LastWord: TLyricWord;
  CurrentTick: cardinal;
  Diff: real;
begin
  Background.Draw;

  // draw background picture (if any, and if no visualizations)
  // when we don't check for visualizations the visualizations would
  // be overdrawn by the picture when {UNDEFINED UseTexture} in UVisualizer
  if (not fShowVisualization) then
    SingDrawJukeboxBackground;

  // retrieve current lyrics time, we have to store the value to avoid
  // that min- and sec-values do not match
  CurLyricsTime := LyricsState.GetCurrentTime();

  // retrieve time for timebar text
  case (fTimebarMode) of
    tbmRemaining: begin
      DisplayTime := LyricsState.TotalTime - CurLyricsTime;
      DisplayPrefix := '-';
    end;
    tbmTotal: begin
      DisplayTime := LyricsState.TotalTime;
      DisplayPrefix := '#';
    end;
    else begin
      DisplayTime := CurLyricsTime;
      DisplayPrefix := '';
    end;
  end;

  DisplayMin := Round(DisplayTime) div 60;
  DisplaySec := Round(DisplayTime) mod 60;
  Text[JukeboxTextTimeText].Text := Format('%s%.2d:%.2d', [DisplayPrefix, DisplayMin, DisplaySec]);

  // update and draw movie
  if Assigned(fCurrentVideo) then
  begin
    // Just call this once
    // when Screens = 2
    if (ScreenAct = 1) then
    begin
      if (ShowFinish) then
      begin
        // everything is setup, determine the current position
        VideoFrameTime := CurrentSong.VideoGAP + LyricsState.GetCurrentTime();
      end
      else
      begin
        // Important: do not yet start the triggered timer by a call to
        // LyricsState.GetCurrentTime()
        VideoFrameTime := CurrentSong.VideoGAP;
      end;
      fCurrentVideo.GetFrame(VideoFrameTime);
    end;

    fCurrentVideo.AspectCorrection := AspectCorrection;
    fCurrentVideo.SetScreen(ScreenAct);
    fCurrentVideo.Draw;
    DrawBlackBars();
  end;

  // check for music finish
  //Log.LogError('Check for music finish: ' + BoolToStr(Music.Finished) + ' ' + FloatToStr(LyricsState.CurrentTime*1000) + ' ' + IntToStr(CurrentSong.Finish));
  if ShowFinish then
  begin
    if (not FinishedMusic) and
       ((CurrentSong.Finish = 0) or
        (LyricsState.GetCurrentTime() * 1000 <= CurrentSong.Finish)) then
    begin
      // analyze song if not paused
      if (not Paused) then
      begin
        SingJukebox(Self);
      end;
    end
    else
    begin
      if (not FadeOut) and (Screens=1) or (ScreenAct=2) then
      begin
        Finish;
      end;
    end;
  end;

  if (ScreenAct = 1) and (SongListVisible) then
    DrawPlaylist;

  //if (ShowLyrics) then
  //begin
  //  if (not(LyricsStart)) then
  //  begin
  //   if (CurLyricsTime >= GetTimeFromBeat(Line.Words[0].Start) - 2) then
  //   begin
        SingDrawJukebox;
  //      LyricsStart := true;
  //   end;
  //  end
  //  else
  //    SingDrawJukebox;
  //end;

  // draw pausepopup
  // FIXME: this is a workaround that the static is drawn over the lyrics, lines, scores and effects
  // maybe someone could find a better solution
  if Paused then
  begin
    Statics[StaticPausePopup].Visible := true;
    Statics[StaticPausePopup].Draw;
    Statics[StaticPausePopup].Visible := false;
  end;

  Result := true;
end;

procedure TScreenJukebox.Finish;
begin
  AudioInput.CaptureStop;
  AudioPlayback.Stop;
  AudioPlayback.SetSyncSource(nil);

  Lyrics.UpperLineY := tmpLyricsUpperY;
  Lyrics.LowerLineY := tmpLyricsLowerY;

  LyricsState.Stop();
  LyricsState.SetSyncSource(nil);

  // close video files
  fVideoClip := nil;
  fCurrentVideo := nil;

  SetFontItalic(false);

  if (CurrentSongList = High(JukeboxVisibleSongs)) then
  begin
    if (RepeatSongList) then
    begin
      // resyart playlist
      CurrentSongList := 0;
      CatSongs.Selected := JukeboxVisibleSongs[CurrentSongList];
      PlayMusic(CurrentSongList);
    end
    else
    begin
      FadeTo(@ScreenMain);
    end;
  end
  else
  begin
    CurrentSongList := CurrentSongList + 1;

    CatSongs.Selected := JukeboxVisibleSongs[CurrentSongList];

    PlayMusic(CurrentSongList);
  end;
end;

procedure TScreenJukebox.OnSentenceEnd(SentenceIndex: cardinal);
var
  PlayerIndex: byte;
  CurrentPlayer: PPLayer;
  CurrentScore: real;
  Line:      PLine;
  LinePerfection: real;  // perfection of singing performance on the current line
  Rating:    integer;
  LineScore: real;
  LineBonus: real;
  MaxSongScore: integer; // max. points for the song (without line bonus)
  MaxLineScore: real;    // max. points for the current line
const
  // TODO: move this to a better place
  MAX_LINE_RATING = 8;        // max. rating for singing performance
begin
  Line := @Lines[0].Line[SentenceIndex];

  // check for empty sentence
  if (Line.TotalNotes <= 0) then
    Exit;

  // set max song score
  if (Ini.LineBonus = 0) then
    MaxSongScore := MAX_SONG_SCORE
  else
    MaxSongScore := MAX_SONG_SCORE - MAX_SONG_LINE_BONUS;

  // Note: ScoreValue is the sum of all note values of the song
  MaxLineScore := MaxSongScore * (Line.TotalNotes / Lines[0].ScoreValue);
end;

 // Called on sentence change
 // SentenceIndex: index of the new active sentence
procedure TScreenJukebox.OnSentenceChange(SentenceIndex: cardinal);
begin
  // fill lyrics queue and set upper line to the current sentence
  while (Lyrics.GetUpperLineIndex() < SentenceIndex) or
    (not Lyrics.IsQueueFull) do
  begin
    // add the next line to the queue or a dummy if no more lines are available
    if (Lyrics.LineCounter <= High(Lines[0].Line)) then
      Lyrics.AddLine(@Lines[0].Line[Lyrics.LineCounter])
    else
      Lyrics.AddLine(nil);
  end;
end;

function TLyricsSyncSource.GetClock(): real;
begin
  Result := LyricsState.GetCurrentTime();
end;

function TMusicSyncSource.GetClock(): real;
begin
  Result := AudioPlayback.Position;
end;

procedure TScreenJukebox.AddSongToJukeboxList(ID: integer);
var
  I: integer;
  SongExist: boolean;
begin
  if (not CatSongs.Song[ID].Main) then
  begin
    SongExist := false;
    for I := 0 to High(JukeboxSongsList) do
    begin
      if (JukeboxSongsList[I] = ID) then
        SongExist := true;
    end;

    if (not SongExist) then
    begin
      SetLength(JukeboxSongsList, Length(JukeboxSongsList) + 1);
      JukeboxSongsList[High(JukeboxSongsList)] := ID;

      SetLength(JukeboxVisibleSongs, Length(JukeboxVisibleSongs) + 1);
      JukeboxVisibleSongs[High(JukeboxVisibleSongs)] := ID;
    end;
  end;
end;

procedure TScreenJukebox.DrawItems();
begin

  if (SDL_GetTicks() - LastTick <= 3000) then
  begin
    Statics[JukeboxStaticTimeBackground].Draw;
    Statics[JukeboxStaticTimeProgress].Draw;

  //  Statics[JukeboxStaticSongBackground].Draw;

    Statics[JukeboxStaticSongListBackground].Draw;
    Statics[StaticCover].Draw;

    Text[JukeboxTextTimeText].Draw;
    Text[JukeboxTextTimeDesc].Draw;
    Text[JukeboxTextSongText].Draw;

    // options desc
    Text[JukeboxTextOptionsSongPosition].Draw;
    Text[JukeboxTextOptionsLyric].Draw;
    Text[JukeboxTextOptionsRandom].Draw;
    Text[JukeboxTextOptionsRepeat].Draw;
    Text[JukeboxTextOptionsFind].Draw;
    Text[JukeboxTextOptionsSort].Draw;
    Statics[JukeboxStaticOptions].Draw;

  end
  else
    SongListVisible := false;

end;

procedure TScreenJukebox.PlayMusic(ID: integer);
var
  Index:  integer;
  VideoFile, BgFile: IPath;
  success: boolean;
  Max: integer;
  CoverPath: IPath;
begin

  // background texture
  if (Tex_Background.TexNum > 0) then
  begin
    glDeleteTextures(1, PGLuint(@Tex_Background.TexNum));
    Tex_Background.TexNum := 0;
  end;

  CurrentSong := CatSongs.Song[JukeboxVisibleSongs[ID]];

  // Cover
  RefreshCover;

  // reset video playback engine
  fCurrentVideo := nil;
  AspectCorrection := acoCrop;

  fTimebarMode := tbmCurrent;

  // FIXME: bad style, put the try-except into loadsong() and not here
  try
    // check if file is xml
    if CurrentSong.FileName.GetExtension.ToUTF8 = '.xml' then
      success := CurrentSong.AnalyseXML and CurrentSong.LoadXMLSong()
    else
      success := CurrentSong.Analyse and CurrentSong.LoadSong();
  except
    success := false;
  end;

  if (not success) then
  begin
    // error loading song -> go back to previous screen and show some error message
    Display.AbortScreenChange;
    // select new song in party mode
    if (Length(CurrentSong.LastError) > 0) then
      ScreenPopupError.ShowPopup(Format(Language.Translate(CurrentSong.LastError), [CurrentSong.ErrorLineNo]))
    else
      ScreenPopupError.ShowPopup(Language.Translate('ERROR_CORRUPT_SONG'));
    // FIXME: do we need this?
    CurrentSong.Path := CatSongs.Song[CatSongs.Selected].Path;
    Exit;
  end;

  {*
   * == Background ==
   * We have four types of backgrounds:
   *   + Blank        : Nothing has been set, this is our fallback
   *   + Picture      : Picture has been set, and exists - otherwise we fallback
   *   + Video        : Video has been set, and exists - otherwise we fallback
   *   + Visualization: + Off        : No visualization
   *                    + WhenNoVideo: Overwrites blank and picture
   *                    + On         : Overwrites blank, picture and video
   *}

  {*
   * set background to: video
   *}
  fShowVisualization := false;
  VideoFile := CurrentSong.Path.Append(CurrentSong.Video);
  if (Ini.VideoEnabled = 1) and CurrentSong.Video.IsSet() and VideoFile.IsFile then
  begin
    fVideoClip := VideoPlayback.Open(VideoFile);
    fCurrentVideo := fVideoClip;
    if (fVideoClip <> nil) then
    begin
      fShowVisualization := false;
      fCurrentVideo.Position := CurrentSong.VideoGAP + CurrentSong.Start;
      fCurrentVideo.Play;
    end;
  end;

  {*
   * set background to: picture
   *}
  if (CurrentSong.Background.IsSet) and (fVideoClip = nil)
    and (TVisualizerOption(Ini.VisualizerOption) = voOff)  then
  begin
    BgFile := CurrentSong.Path.Append(CurrentSong.Background);
    try
      Tex_Background := Texture.LoadTexture(BgFile);
    except
      Log.LogError('Background could not be loaded: ' + BgFile.ToNative);
      Tex_Background.TexNum := 0;
    end
  end
  else
  begin
    Tex_Background.TexNum := 0;
  end;

  {*
   * set background to: visualization (Overwrites all)
   *}
  if (TVisualizerOption(Ini.VisualizerOption) in [voOn]) then
  begin
    fShowVisualization := true;
    fCurrentVideo := Visualization.Open(PATH_NONE);
    if (fCurrentVideo <> nil) then
      fCurrentVideo.Play;
  end;

  {*
   * set background to: visualization (Videos are still shown)
   *}
  if ((TVisualizerOption(Ini.VisualizerOption) in [voWhenNoVideo]) and
     (fVideoClip = nil)) then
  begin
    fShowVisualization := true;
    fCurrentVideo := Visualization.Open(PATH_NONE);
    if (fCurrentVideo <> nil) then
      fCurrentVideo.Play;
  end;

  // prepare lyrics timer
  LyricsState.Reset();

  LyricsState.SetCurrentTime(CurrentSong.Start);
  LyricsState.StartTime := CurrentSong.Gap;
  if (CurrentSong.Finish > 0) then
    LyricsState.TotalTime := CurrentSong.Finish / 1000
  else
  begin
    LyricsState.TotalTime := AudioPlayback.Length;
  end;

  LyricsState.UpdateBeats();

  // main text
  Lyrics.Clear(CurrentSong.BPM[0].BPM, CurrentSong.Resolution);

  {*
   * set background to: picture
   *}
  if (CurrentSong.Background.IsSet) and (fVideoClip = nil)
    and (TVisualizerOption(Ini.VisualizerOption) = voOff)  then
  begin
    BgFile := CurrentSong.Path.Append(CurrentSong.Background);
    try
      Tex_Background := Texture.LoadTexture(BgFile);
    except
      Log.LogError('Background could not be loaded: ' + BgFile.ToNative);
      Tex_Background.TexNum := 0;
    end
  end
  else
  begin
    Tex_Background.TexNum := 0;
  end;

  // initialize lyrics by filling its queue
  while (not Lyrics.IsQueueFull) and
        (Lyrics.LineCounter <= High(Lines[0].Line)) do
  begin
    Lyrics.AddLine(@Lines[0].Line[Lyrics.LineCounter]);
  end;

  Text[JukeboxTextSongText].Visible := true;
  Text[JukeboxTextSongText].Text := CurrentSong.Artist + ' - ' + CurrentSong.Title;

  Max := 9;

  if (High(JukeboxVisibleSongs) < 9) then
    Max := High(JukeboxVisibleSongs);

  for Index := 0 to 9 do
    Button[SongDescription[Index]].Selectable := false;

  for Index := 0 to Max do
  begin
    Button[SongDescription[Index]].Visible := true;
    Button[SongDescription[Index]].Selectable := true;
  end;

  Button[JukeboxFindSong].Visible := true;
  Button[JukeboxRepeatSongList].Visible := true;
  Button[JukeboxSongListOrder].Visible := true;
  Button[JukeboxRandomSongList].Visible := true;
  Button[JukeboxLyric].Visible := true;

  CurrentSongID := JukeboxVisibleSongs[CurrentSongList];

  SongListVisible := true;

  Play();
end;

procedure TScreenJukebox.RefreshCover();
var
  CoverPath: IPath;
begin
  CoverPath :=  CurrentSong.Path.Append(CurrentSong.Cover);
  Statics[StaticCover].Texture := Texture.GetTexture(CoverPath, TEXTURE_TYPE_PLAIN, false);
  Statics[StaticCover].Texture.X := Theme.Jukebox.SongCover.X;
  Statics[StaticCover].Texture.Y := Theme.Jukebox.SongCover.Y;
  Statics[StaticCover].Texture.W := Theme.Jukebox.SongCover.W;
  Statics[StaticCover].Texture.H := Theme.Jukebox.SongCover.H;
  Statics[StaticCover].Texture.Alpha := 0.7;
end;

procedure TScreenJukebox.DrawPlaylist;
var
  I, Max: integer;
  SongDesc: UTF8String;
begin
  DrawItems;

  Max := 9;
  if (High(JukeboxVisibleSongs) < 9) then
    Max := High(JukeboxVisibleSongs);

  Text[JukeboxCountText].Text := IntToStr(ActualInteraction + 1) + '/' + IntToStr(length(JukeboxVisibleSongs));
  Text[JukeboxListText].Draw;
  Text[JukeboxCountText].Draw;

  Button[JukeboxFindSong].Draw;
  Button[JukeboxSongListOrder].Draw;

  Button[JukeboxRepeatSongList].Draw;
  Button[JukeboxRandomSongList].Draw;
  Button[JukeboxLyric].Draw;

  for I := 0 to Max do
  begin
    Button[SongDescription[I]].Visible := true;
    Button[SongDescription[I]].Selectable := true;

    SongDesc := CatSongs.Song[JukeboxVisibleSongs[I + ListMin]].Artist + ' - ' + CatSongs.Song[JukeboxVisibleSongs[I + ListMin]].Title;

    if (JukeboxVisibleSongs[I + ListMin] = CurrentSongID) and (I + ListMin <> ActualInteraction) then
    begin
      Button[SongDescription[I]].Text[0].ColR := SelectColR;
      Button[SongDescription[I]].Text[0].ColG := SelectColG;
      Button[SongDescription[I]].Text[0].ColB := SelectColB;
    end
    else
    begin
      Button[SongDescription[I]].Text[0].ColR := 1;
      Button[SongDescription[I]].Text[0].ColG := 1;
      Button[SongDescription[I]].Text[0].ColB := 1;
    end;

    Button[SongDescription[I]].Text[0].Text := SongDesc;
    Button[SongDescription[I]].Draw;
  end;

end;

end.


