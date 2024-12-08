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
  UCommon,
  UDataBase,
  UFiles,
  UGraphicClasses,
  UHookableEvent,
  UIni,
  ULyrics,
  UMenu,
  UMusic,
  UPath,
  UPlaylist,
  USingScores,
  USongs,
  UTexture,
  UThemes,
  UTime,
  UVideo,
  UWebcam,
  dglOpenGL,
  sdl2,
  SysUtils,
  TextGL;

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
    // move song
    MoveX, MoveY: real;
    MoveInicial, MoveFinal: integer;
    MoveSong: boolean;

    // items
    JukeboxStaticTimeProgress:       integer;
    JukeboxStaticTimeBackground:     integer;
    JukeboxStaticSongBackground:     integer;
    JukeboxStaticSongListBackground: integer;
    SongDescription:  array[0..9] of integer;
    SongDescriptionClone: integer;

    JukeboxStaticActualSongStatic: array of integer;
    JukeboxStaticActualSongCover:           integer;
    JukeboxTextActualSongArtist:            integer;
    JukeboxTextActualSongTitle:             integer;

    JukeboxSongListUp:   integer;
    JukeboxSongListDown: integer;

    // Jukebox SongMenu items
    JukeboxSongMenuPlayPause:            integer;
    JukeboxSongMenuPlaylist:             integer;
    JukeboxSongMenuOptions:              integer;
    JukeboxSongMenuNext:                 integer;
    JukeboxSongMenuPrevious:             integer;
    JukeboxStaticSongMenuTimeProgress:   integer;
    JukeboxStaticSongMenuTimeBackground: integer;
    JukeboxTextSongMenuTimeText:         integer;
    JukeboxStaticSongMenuBackground:     integer;

    SelectColR: real;
    SelectColG: real;
    SelectColB: real;

    JukeboxTextTimeText: integer;
    //JukeboxTextSongText: integer;

    SongFinish: boolean;

    tmpLyricsUpperY: real;
    tmpLyricsLowerY: real;
    //tmp_mouse: integer;

    JukeboxFindSong:       integer;
    JukeboxRepeatSongList: integer;
    JukeboxSongListOrder:  integer;
    JukeboxRandomSongList: integer;
    JukeboxListText:       integer;
    JukeboxCountText:      integer;
    JukeboxLyric:          integer;
    JukeboxOptions:        integer;
    JukeboxSongListClose:  integer;
    JukeboxSongListFixPin: integer;
    JukeboxPlayPause:      integer;

    Filter:         UTF8String;

    FindSongList:   boolean;
    RepeatSongList: boolean;
    RandomMode:     boolean;
    OrderMode:      boolean;
    OrderType:      integer;

    fShowVisualization: boolean;
    fShowWebcam:        boolean;
    fShowBackground:    boolean;

    fCurrentVideo: IVideo;
    fVideoClip:    IVideo;
    fLyricsSync:   TLyricsSyncSource;
    fMusicSync:    TMusicSyncSource;
    fTimebarMode:  TTimebarMode;

  protected
    eSongLoaded:       THookableEvent; //< event is called after lyrics of a song are loaded on OnShow
    Paused:            boolean; //pause Mod
    NumEmptySentences: integer;

  public
    ShowLyrics:  boolean;
    LyricsStart: boolean;

    CurrentSongList:     integer;
    LastTick:            cardinal;
    LastSongMenuTick:    cardinal;
    LastSongOptionsTick: cardinal;
    DoubleClickTime:     cardinal;
    CloseClickTime:      cardinal;
    SongListVisible:     boolean;
    SongListVisibleFix:  boolean;
    SongMenuVisible:     boolean;
    LastTickChangeSong:  cardinal;
    MouseDownList:       cardinal;
    MoveDown:            boolean;
    MouseUpList:         cardinal;
    MoveUp:              boolean;

    JukeboxSongsList:     array of integer;
    JukeboxVisibleSongs:  array of integer;

    ActualInteraction:   integer;
    ListMin:             integer;
    CurrentSongID:       integer;

    //VideoAspect
    VideoAspectText:     integer;
    VideoAspectStatic:   integer;
    AspectHandler:       THandler;
    AspectCorrection:    TAspectCorrection;

    Tex_Background: TTexture;
    FadeOut: boolean;
    Lyrics:  TLyricEngine;
    LyricsAlpha : real;
    StaticCover: integer;
    LyricHelper: TRGB;

    constructor Create; override;
    procedure OnShow; override;
    procedure OnShowFinish; override;
    procedure OnHide; override;

    function ParseInput(PressedKey: cardinal; CharCode: UCS4Char;
      PressedDown: boolean): boolean; override;
    function ParseMouse(MouseButton: Integer; BtnDown: Boolean; X, Y: integer): boolean; override;

    function Draw: boolean; override;
    procedure DrawBlackBars();

    procedure PlayMusic(ID: integer; ShowList: boolean);
    procedure Play;
    procedure Finish;
    procedure Pause; // toggle pause

    procedure OnSentenceChange(SentenceIndex: cardinal);  // for golden notes

    procedure DeleteSong(Id: integer);
    procedure FilterSongList(Filter: UTF8String);
    procedure GoToSongList(UpperLetter: UCS4Char);
    procedure SongListSort(Order: integer);
    procedure Sort(Order: integer);
    procedure Reset;

    procedure AddSongToJukeboxList(ID: integer);
    function FinishedMusic: boolean;

    procedure RefreshCover;
    procedure DrawPlaylist;
    procedure DrawMoveLine;
    procedure DrawSongInfo;
    procedure DrawSongMenu;
    procedure DrawLine(X, Y, W, H: real);

    procedure ChangeTime(Time: real);
    procedure ChangeSongPosition(Start, Final: integer);
    procedure ChangeOrderList();
    procedure RandomList();

    procedure PageDown(N: integer);
    procedure PageUp(N: integer);

    procedure ChangeLyricPosition(N: integer);
    procedure ChangeVideoWidth(N: integer);
    procedure ChangeVideoHeight(N: integer);

    procedure LoadJukeboxSongOptions();
  end;

const
  ID='ID_041';   //for help system

implementation

uses
  UBeatTimer,
  UDisplay,
  UDraw,
  UGraphic,
  UHelp,
  ULanguage,
  ULog,
  UMain,
  UMenuButton,
  UMenuInteract,
  UNote,
  UParty,
  URecord,
  USkins,
  UScreenJukeboxOptions,
  USong,
  UUnicodeUtils,
  Classes,
  Math;

const
  MAX_TIME_PLAYLIST = 4000; // msec

  MAX_TIME_SONGDESC = 2000; // msec
  MAX_TIME_FADESONGDESC = 2000; // msec

  MAX_TIME_MOUSE_CHANGELIST = 300; // msec

  MAX_TIME_MOUSE_CLOSE = 500; // msec

  MAX_TIME_SONGMENU = 3000; // msec

  MAX_TIME_SONGOPTIONS = 3000; // msec

procedure TScreenJukebox.DrawLine(X, Y, W, H: real);
begin
  glEnable(GL_BLEND);
  glColor4f(0.3, 0.3, 0.3, 0.4);
  glbegin(gl_quads);
   glVertex2f(X, Y);
   glVertex2f(X, Y + H);
   glVertex2f(X + W, Y + H);
   glVertex2f(X + W, Y);
  glEnd;
end;

procedure TScreenJukebox.DrawMoveLine();
begin
  glColor4f(SelectColR, SelectColG, SelectColB, 1);
  glbegin(gl_quads);
    glVertex2f(MoveX, MoveY);
    glVertex2f(MoveX, MoveY + 2);
    glVertex2f(MoveX + Button[SongDescription[0]].Texture.W, MoveY + 2);
    glVertex2f(MoveX + Button[SongDescription[0]].Texture.W, MoveY);
  glEnd;
end;

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

// TODO: this function does nothing?
procedure TScreenJukebox.GoToSongList(UpperLetter: UCS4Char);
var
  SongDesc: UTF8String;
  I, S, CurrentInteractionSong: integer;
  isCurrentPage, existNextSong: boolean;
  ListCurrentPage: array of integer;
begin
 {

  isCurrentPage := false;
  existNextSong := false;
  CurrentInteractionSong := ActualInteraction;

  for I := ActualInteraction + 1 to High(JukeboxVisibleSongs) do
  begin
    if (OrderType = 2) then
      SongDesc := CatSongs.Song[JukeboxSongsList[I]].TitleNoAccent
    else
      SongDesc := CatSongs.Song[JukeboxSongsList[I]].ArtistNoAccent;

    if (UTF8StartsText(UCS4ToUTF8String(UpperLetter), SongDesc)) then
    begin
      ActualInteraction := I;
      CurrentSongList := I;
      existNextSong := true;
      break;
    end

  end;

  // page songsid
        SetLength(JukeboxSongsListCurrentPage, 0);



  JukeboxSongsList[I]
        SetLength(JukeboxSongsListCurrentPage, Length(JukeboxSongsListCurrentPage) + 1);

        JukeboxSongsListCurrentPage[High(JukeboxSongsListCurrentPage)] := JukeboxVisibleSongs[I + ListMin];

  if (existNextSong) then
  begin
    while not (isCurrentPage) do
    begin

      for S := 0 to High(JukeboxSongsListCurrentPage) do
      begin
        if (JukeboxSongsListCurrentPage[S] = JukeboxSongsList[CurrentSongList]) then
        begin
          isCurrentPage := true;
          break;
        end;
      end;

      if not (isCurrentPage) then
      begin
        PageDown(10);
        ListMin := ListMin + 1;
      end;
    end;
  end;
  }

end;

procedure TScreenJukebox.FilterSongList(Filter: UTF8String);
var
  I: integer;
  SongD: UTF8String;
begin

  if (Filter <> '') then
  begin
    Filter := LowerCase(TransliterateToASCII(UTF8Decode(Filter)));

    SetLength(JukeboxVisibleSongs, 0);
    for I := 0 to High(JukeboxSongsList) do
    begin
      SongD := CatSongs.Song[JukeboxSongsList[I]].ArtistASCII + ' - ' + CatSongs.Song[JukeboxSongsList[I]].TitleASCII;

      if (UTF8ContainsStr(SongD, Filter)) then
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

  Button[SongDescription[0]].SetSelect(false);
end;


procedure TScreenJukebox.DeleteSong(Id: integer);
var
  ALength: Cardinal;
  TailElements: Cardinal;
  IndexVisibleSongs, IndexSongList, I: integer;
begin
  IndexVisibleSongs := Id;

  for I := 0 to High(JukeboxSongsList) do
  begin
    if (JukeboxVisibleSongs[IndexVisibleSongs] = JukeboxSongsList[I]) then
    begin
      IndexSongList := I;
      break;
    end;
  end;

  // visible songs
  ALength := Length(JukeboxVisibleSongs);
  Assert(ALength > 0);
  Assert(IndexVisibleSongs < ALength);
  Finalize(JukeboxVisibleSongs[IndexVisibleSongs]);
  TailElements := ALength - IndexVisibleSongs;
  if TailElements > 0 then
    Move(JukeboxVisibleSongs[IndexVisibleSongs + 1], JukeboxVisibleSongs[IndexVisibleSongs], SizeOf(integer) * TailElements);
  Initialize(JukeboxVisibleSongs[ALength - 1]);
  SetLength(JukeboxVisibleSongs, ALength - 1);

  // all playlist
  ALength := Length(JukeboxSongsList);
  Assert(ALength > 0);
  Assert(IndexSongList < ALength);
  Finalize(JukeboxSongsList[IndexSongList]);
  TailElements := ALength - IndexSongList;
  if TailElements > 0 then
    Move(JukeboxSongsList[IndexSongList + 1], JukeboxSongsList[IndexSongList], SizeOf(integer) * TailElements);
  Initialize(JukeboxSongsList[ALength - 1]);
  SetLength(JukeboxSongsList, ALength - 1);

end;


procedure TScreenJukebox.ChangeLyricPosition(N: integer);
begin
  Lyrics.UpperLineY := Lyrics.UpperLineY + N;
  Lyrics.LowerLineY := Lyrics.LowerLineY + N;
end;

procedure TScreenJukebox.ChangeVideoWidth(N: integer);
var
  X, Y, Z: double;
begin
  fCurrentVideo.GetScreenPosition(X, Y, Z);
  fCurrentVideo.SetScreenPosition(X - (N/2), Y, Z);
  fCurrentVideo.SetWidth(fCurrentVideo.GetWidth + N);
end;

procedure TScreenJukebox.ChangeVideoHeight(N: integer);
var
  X, Y, Z: double;
begin
  fCurrentVideo.GetScreenPosition(X, Y, Z);
  fCurrentVideo.SetScreenPosition(X, Y - (N/2), Z);
  fCurrentVideo.SetHeight(fCurrentVideo.GetHeight + N);
end;

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
  StopTextInput;
  Button[JukeboxPlayPause].SetSelect(false);
  Button[JukeboxFindSong].Text[0].Selected := false;
end;

procedure OnDeleteSong(Value: boolean; Data: Pointer);
var
  tmp: integer;
begin
  Display.CheckOK := Value;

  if (Value) then
  begin
    Display.CheckOK := false;

    ScreenJukebox.DeleteSong(ScreenJukebox.ActualInteraction);
  end;
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

function TScreenJukebox.ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean;
var
  I, Max: integer;
  Time: real;
begin
  Result := true;

  //Jukebox Screen Extensions (Options)
  if (ScreenJukeboxOptions.Visible) then
  begin
    Result := ScreenJukeboxOptions.ParseMouse(MouseButton, BtnDown, X, Y);
    Exit;
  end;

  LastTick := SDL_GetTicks();

  Max := 9;
  if (High(JukeboxVisibleSongs) < 9) then
    Max := High(JukeboxVisibleSongs);

  // transfer mousecords to the 800x600 raster we use to draw
  X := Round((X / (ScreenW / Screens)) * RenderW);
  if (X > RenderW) then
    X := X - RenderW;
  Y := Round((Y / ScreenH) * RenderH);

  if (SongListVisible) then
  begin
    // SetTextInput(Button[JukeboxFindSong].Selected);

    if (BtnDown) then //and (MouseButton = SDL_BUTTON_LEFT) then
    begin

      // move song
      if (Button[SongDescriptionClone].Visible) then
      begin
        Button[SongDescriptionClone].X := X;
        Button[SongDescriptionClone].Y := Y;
        Button[SongDescriptionClone].Text[0].X := X + Button[SongDescription[0]].Text[0].X - Button[SongDescription[0]].X;
        Button[SongDescriptionClone].Text[0].Y := Y + Button[SongDescription[0]].Text[0].Y - Button[SongDescription[0]].Y;

        //line position
        MoveSong := false;

        for I := 0 to Max do
        begin
          if InRegionX(X, Button[SongDescription[I]].GetMouseOverArea) then
            MoveSong := true;

          if InRegion(X, Y + Button[SongDescription[I]].Texture.H/2, Button[SongDescription[I]].GetMouseOverArea) then
          begin
            MoveX := Button[SongDescription[I]].Texture.X;
            MoveY := Button[SongDescription[I]].Texture.Y;
            MoveFinal := I + ListMin;
          end;
        end;

        if (MoveSong) and (Y >= Button[SongDescription[Max]].Texture.Y + Button[SongDescription[Max]].Texture.H/2) then
        begin
          MoveX := Button[SongDescription[Max]].Texture.X;
          MoveY := Button[SongDescription[Max]].Texture.Y + Button[SongDescription[Max]].Texture.H;
          MoveFinal := Max + ListMin + 1;
        end;

        if (MoveSong) and (Y >= Button[SongDescription[Max]].Texture.Y + Button[SongDescription[Max]].Texture.H) then
        begin
          MoveX := Button[SongDescription[Max]].Texture.X;
          MoveY := Button[SongDescription[Max]].Texture.Y + Button[SongDescription[Max]].Texture.H;
          MoveFinal := Max + ListMin + 1;

          if (MouseDownList = 0) then
            MouseDownList := SDL_GetTicks()
          else
          begin
            MoveDown := true;
          end;
        end
        else
        begin
          MouseDownList := 0;
          MoveDown := false;
        end;

        if (MoveSong) and (Y <= Button[SongDescription[0]].Texture.Y - Button[SongDescription[0]].Texture.H) then
        begin
          if (MouseUpList = 0) then
            MouseUpList := SDL_GetTicks()
          else
          begin
            MoveUp := true;
          end;
        end
        else
        begin
          MouseUpList := 0;
          MoveUp := false;
        end;
      end
      else
      begin
        //song scrolling with mousewheel
        if (MouseButton = SDL_BUTTON_WHEELDOWN) then
          Result := ParseInput(SDLK_PAGEDOWN, 0, true)

        else if (MouseButton = SDL_BUTTON_WHEELUP) then
          Result := ParseInput(SDLK_PAGEUP, 0, true)

        else
        begin
          // up/down songlist
          if InRegion(X, Y, Button[JukeboxSongListUp].GetMouseOverArea) then
          begin
            PageUp(10);
          end;

          if InRegion(X, Y, Button[JukeboxSongListDown].GetMouseOverArea) then
          begin
            PageDown(10);
          end;

          // change time
          if InRegion(X, Y, Text[JukeboxTextTimeText].GetMouseOverArea) then
          begin
            if (fTimebarMode = High(TTimebarMode)) then
              fTimebarMode := Low(TTimebarMode)
            else
              Inc(fTimebarMode);

            Ini.JukeboxTimebarMode := Ord(fTimebarMode);
            Ini.SaveJukeboxTimebarMode();
          end;

          // play or move song
          for I := 0 to Max do
          begin
            if InRegion(X, Y, Button[SongDescription[I]].GetMouseOverArea) then
            begin

              // start move song
              if not InRegion(X, Y, Button[SongDescription[Interaction]].GetMouseOverArea) then
              begin
                Button[SongDescriptionClone].X := X;
                Button[SongDescriptionClone].Y := Y;
                Button[SongDescriptionClone].Text[0].X := X + Button[SongDescription[I]].Text[0].X - Button[SongDescription[I]].X;
                Button[SongDescriptionClone].Text[0].Y := Y + Button[SongDescription[I]].Text[0].Y - Button[SongDescription[I]].Y;
                Button[SongDescriptionClone].Text[0].Text := CatSongs.Song[JukeboxVisibleSongs[Interaction + ListMin]].Artist + ' - ' + CatSongs.Song[JukeboxVisibleSongs[Interaction + ListMin]].Title;

                MoveX := Button[SongDescription[I]].Texture.X;
                MoveY := Button[SongDescription[I]].Texture.Y;
                MoveInicial := ListMin + Interaction;

                Button[SongDescriptionClone].Visible := true;
                Button[SongDescription[Interaction]].SetSelect(false);
              end
              else
              begin

                if (MouseButton = SDL_BUTTON_LEFT) then
                begin
                  if (SDL_GetTicks() - DoubleClickTime <= 500) then
                    Result := ParseInput(SDLK_RETURN, 0, true);

                  DoubleClickTime := SDL_GetTicks();
                end;

              end;
            end;
          end;

          // close songlist
          if InRegion(X, Y, Button[JukeboxSongListClose].GetMouseOverArea)then
          begin
            Result := ParseInput(SDLK_ESCAPE, 0, true);
            CloseClickTime := SDL_GetTicks();
            Button[JukeboxSongListClose].SetSelect(false);
          end;

          // fix songlist
          if InRegion(X, Y, Button[JukeboxSongListFixPin].GetMouseOverArea)then
          begin
            SongListVisibleFix := not SongListVisibleFix;

            if (SongListVisibleFix) then
              Ini.JukeboxSongMenu := 0
            else
              Ini.JukeboxSongMenu := 1;

            Ini.SaveJukeboxSongMenu;

            Button[JukeboxSongListFixPin].SetSelect(SongListVisibleFix);
          end;

          if InRegion(X, Y, Statics[JukeboxStaticTimeProgress].GetMouseOverArea) then
          begin
            Time := ((X - Statics[JukeboxStaticTimeProgress].Texture.X) * LyricsState.TotalTime)/(Statics[JukeboxStaticTimeProgress].Texture.W);

            ChangeTime(Time);
          end;

          if InRegion(X, Y, Button[JukeboxRepeatSongList].GetMouseOverArea) then
          begin
            RepeatSongList := not RepeatSongList;
            Button[JukeboxRepeatSongList].SetSelect(RepeatSongList);
          end;

          if InRegion(X, Y, Button[JukeboxSongListOrder].GetMouseOverArea) then
          begin
            ChangeOrderList();
          end;

          if InRegion(X, Y, Button[JukeboxRandomSongList].GetMouseOverArea) then
          begin
            RandomList();
          end;

          if InRegion(X, Y, Button[JukeboxLyric].GetMouseOverArea) then
          begin
            ShowLyrics := not ShowLyrics;
            Button[JukeboxLyric].SetSelect(ShowLyrics);
          end;

          if InRegion(X, Y, Button[JukeboxPlayPause].GetMouseOverArea) then
          begin
            Pause;
            Button[JukeboxPlayPause].SetSelect(Paused);
          end;

          if InRegion(X, Y, Button[JukeboxFindSong].GetMouseOverArea) then
          begin
            FindSongList := not FindSongList;

            if (Filter = '') then
            begin
              if (FindSongList) then
                Button[JukeboxFindSong].Text[0].Text := ''
            end;

            Button[JukeboxFindSong].SetSelect(FindSongList);
            SetTextInput(FindSongList);

            if not (FindSongList) and (Length(JukeboxSongsList) <> Length(JukeboxVisibleSongs)) then
            begin
              FilterSongList('');
            end
            else
            begin
              if (Filter <> '') then
                FilterSongList(Filter);
            end;

            if (FindSongList) then
              Button[JukeboxFindSong].Text[0].Selected := true
            else
              Button[JukeboxFindSong].Text[0].Selected := false;

          end;

          if InRegion(X, Y, Button[JukeboxOptions].GetMouseOverArea) then
          begin
            SongMenuVisible := false;
            SongListVisible := false;
            StopTextInput;

            Button[JukeboxOptions].SetSelect(false);
            ScreenJukeboxOptions.Visible := true;

            LastSongOptionsTick := SDL_GetTicks();
          end;

        end;
      end;
    end
    else
    begin

      // change music position
      if (MoveSong) and (ListMin = 0) and (Y <= Button[SongDescription[0]].Texture.Y - Button[SongDescription[0]].Texture.H) then
      begin
        Interaction := 0;
        ActualInteraction := 0;
        Button[SongDescription[0]].SetSelect(true);

        MoveFinal := 0;
      end;

      if (Button[SongDescriptionClone].Visible) and (MoveSong) then
      begin
        if (MoveInicial > MoveFinal) then
          MoveInicial := MoveInicial + 1;

        ChangeSongPosition(MoveInicial, MoveFinal);

        DoubleClickTime := 0;
      end;

      Button[SongDescriptionClone].Visible := false;

      for I := 0 to Max do
      begin
        if InRegion(X, Y, Button[SongDescription[I]].GetMouseOverArea) then
        begin
          Interaction := I;
          ActualInteraction := ListMin + I;
          Button[SongDescription[I]].SetSelect(true);
        end
        else
        begin
          Button[SongDescription[I]].SetSelect(false);
        end;
      end;

      // hover
      if (High(JukeboxVisibleSongs) > 9) then
      begin
        if InRegion(X, Y, Button[JukeboxSongListUp].GetMouseOverArea) then
            Button[JukeboxSongListUp].SetSelect(true)
        else
            Button[JukeboxSongListUp].SetSelect(false);

        if InRegion(X, Y, Button[JukeboxSongListDown].GetMouseOverArea) then
            Button[JukeboxSongListDown].SetSelect(true)
        else
            Button[JukeboxSongListDown].SetSelect(false);
      end;

      if InRegion(X, Y, Button[JukeboxSongListClose].GetMouseOverArea) then
        Button[JukeboxSongListClose].SetSelect(true)
      else
        Button[JukeboxSongListClose].SetSelect(false);

      //hover fix pin
      if (not SongListVisibleFix) then
      begin
        if InRegion(X, Y, Button[JukeboxSongListFixPin].GetMouseOverArea) then
          Button[JukeboxSongListFixPin].SetSelect(true)
        else
          Button[JukeboxSongListFixPin].SetSelect(false);
      end;

      if InRegion(X, Y, Button[JukeboxSongListOrder].GetMouseOverArea) then
        Button[JukeboxSongListOrder].SetSelect(true)
      else
        Button[JukeboxSongListOrder].SetSelect(not RandomMode);

      if InRegion(X, Y, Button[JukeboxRandomSongList].GetMouseOverArea) then
        Button[JukeboxRandomSongList].SetSelect(true)
      else
        Button[JukeboxRandomSongList].SetSelect(RandomMode);

      if InRegion(X, Y, Button[JukeboxRepeatSongList].GetMouseOverArea) then
        Button[JukeboxRepeatSongList].SetSelect(true)
      else
        Button[JukeboxRepeatSongList].SetSelect(RepeatSongList);

      if InRegion(X, Y, Button[JukeboxPlayPause].GetMouseOverArea) then
        Button[JukeboxPlayPause].SetSelect(true)
      else
        Button[JukeboxPlayPause].SetSelect(Paused);

      if InRegion(X, Y, Button[JukeboxLyric].GetMouseOverArea) then
        Button[JukeboxLyric].SetSelect(true)
      else
        Button[JukeboxLyric].SetSelect(ShowLyrics);

      if InRegion(X, Y, Button[JukeboxOptions].GetMouseOverArea) then
        Button[JukeboxOptions].SetSelect(true)
      else
        Button[JukeboxOptions].SetSelect(false);

      if InRegion(X, Y, Button[JukeboxFindSong].GetMouseOverArea) then
      begin
        Button[JukeboxFindSong].SetSelect(true);
        StartTextInput;
      end
      else
      begin
        Button[JukeboxFindSong].SetSelect(FindSongList);
        SetTextInput(FindSongList);
      end;

    end;
  end;

  if (SongMenuVisible) then
  begin
    //songmenu visible
    if (BtnDown) and (MouseButton = SDL_BUTTON_LEFT) then
    begin
      if InRegion(X, Y, Button[JukeboxSongMenuPlayPause].GetMouseOverArea) then
        Result := ParseInput(SDLK_SPACE, 0, true);

      if InRegion(X, Y, Button[JukeboxSongMenuPrevious].GetMouseOverArea) then
        Result := ParseInput(SDLK_LEFT, 0, true);

      if InRegion(X, Y, Button[JukeboxSongMenuNext].GetMouseOverArea) then
        Result := ParseInput(SDLK_RIGHT, 0, true);

      if InRegion(X, Y, Button[JukeboxSongMenuPlaylist].GetMouseOverArea) then
      begin
        SongMenuVisible := false;
        SongListVisible := true;

        LastTick := SDL_GetTicks();
      end;

      if InRegion(X, Y, Statics[JukeboxStaticSongMenuTimeProgress].GetMouseOverArea) then
      begin
        Time := ((X - Statics[JukeboxStaticSongMenuTimeProgress].Texture.X) * LyricsState.TotalTime)/(Statics[JukeboxStaticSongMenuTimeProgress].Texture.W);

        ChangeTime(Time);
      end;

      // change time
      if InRegion(X, Y, Text[JukeboxTextSongMenuTimeText].GetMouseOverArea) then
      begin
        if (fTimebarMode = High(TTimebarMode)) then
          fTimebarMode := Low(TTimebarMode)
        else
          Inc(fTimebarMode);

        Ini.JukeboxTimebarMode := Ord(fTimebarMode);
        Ini.SaveJukeboxTimebarMode();
      end;

      if InRegion(X, Y, Button[JukeboxSongMenuOptions].GetMouseOverArea) then
      begin
        SongMenuVisible := false;
        SongListVisible := false;
        StopTextInput;

        Button[JukeboxSongMenuOptions].SetSelect(false);

        ScreenJukeboxOptions.Visible := true;

        LastSongOptionsTick := SDL_GetTicks();
      end;
    end
    else
    begin
      // hover
      if InRegion(X, Y, Button[JukeboxSongMenuPlayPause].GetMouseOverArea) then
        Button[JukeboxSongMenuPlayPause].SetSelect(true)
      else
        Button[JukeboxSongMenuPlayPause].SetSelect(Paused);

      if InRegion(X, Y, Button[JukeboxSongMenuPrevious].GetMouseOverArea) then
        Button[JukeboxSongMenuPrevious].SetSelect(true)
      else
        Button[JukeboxSongMenuPrevious].SetSelect(false);

      if InRegion(X, Y, Button[JukeboxSongMenuNext].GetMouseOverArea) then
        Button[JukeboxSongMenuNext].SetSelect(true)
      else
        Button[JukeboxSongMenuNext].SetSelect(false);

      if InRegion(X, Y, Button[JukeboxSongMenuPlaylist].GetMouseOverArea) then
        Button[JukeboxSongMenuPlaylist].SetSelect(true)
      else
        Button[JukeboxSongMenuPlaylist].SetSelect(false);

      if InRegion(X, Y, Button[JukeboxSongMenuOptions].GetMouseOverArea) then
        Button[JukeboxSongMenuOptions].SetSelect(true)
      else
        Button[JukeboxSongMenuOptions].SetSelect(false);
    end
  end;

  if (not(SongListVisible) and not(ScreenJukeboxOptions.Visible) and (SDL_GetTicks - CloseClickTime > MAX_TIME_MOUSE_CLOSE))
    or (not(SongListVisible) and not(ScreenJukeboxOptions.Visible) and (Y <= 5)) then
  begin
    if (SongListVisibleFix) then
    begin
      SongListVisible := true;
    end
    else
    begin
      SongMenuVisible := true;
      LastSongMenuTick := SDL_GetTicks();
    end;
    if MouseButton = SDL_BUTTON_RIGHT then
    begin
      ScreenPopupCheck.ShowPopup('MSG_END_JUKEBOX', OnEscapeJukebox, nil, true)
    end;
  end;
end;

procedure TScreenJukebox.RandomList();
var
  I, RValueI, RValueE: integer;
  tmp: integer;
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

procedure TScreenJukebox.ChangeOrderList();
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
end;

procedure TScreenJukebox.ChangeSongPosition(Start, Final: integer);
var
  I_Index, F_Index: integer;
  ALength: Cardinal;
  TailElements: Cardinal;
begin
  LastTick := SDL_GetTicks();

  I_Index := Start;
  F_Index := Final;
  ALength := Length(JukeboxVisibleSongs);

  //insert
  Assert(F_Index <= ALength);
  SetLength(JukeboxVisibleSongs, ALength + 1);
  Finalize(JukeboxVisibleSongs[ALength]);
  TailElements := ALength - F_Index;
  if TailElements > 0 then
    Move(JukeboxVisibleSongs[F_Index], JukeboxVisibleSongs[F_Index + 1], SizeOf(I_Index) * TailElements);
  Initialize(JukeboxVisibleSongs[F_Index]);
  JukeboxVisibleSongs[F_Index] := JukeboxVisibleSongs[I_Index];

  ALength := Length(JukeboxVisibleSongs);

  //delete
  Assert(ALength > 0);
  Assert(I_Index < ALength);
  Finalize(JukeboxVisibleSongs[I_Index]);
  TailElements := ALength - I_Index;
  if TailElements > 0 then
    Move(JukeboxVisibleSongs[I_Index + 1], JukeboxVisibleSongs[I_Index], SizeOf(F_Index) * TailElements);
  Initialize(JukeboxVisibleSongs[ALength - 1]);
  SetLength(JukeboxVisibleSongs, ALength - 1);
end;

procedure TScreenJukebox.ChangeTime(Time: real);
begin
  LastTick := SDL_GetTicks();

  AudioPlayback.Position := Time;

  if (Assigned(fCurrentVideo)) then
    fCurrentVideo.Position := CurrentSong.VideoGAP + CurrentSong.Start + Time;

  // correct lyric timer
  LyricsState.StartTime := CurrentSong.GAP + Time;
  LyricsState.SetCurrentTime(Time);

  // main text
  Lyrics.Clear();

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
  UpperLetter: UCS4Char;
begin
  Result := true;

  //Jukebox Screen Extensions (Options)
  if (ScreenJukeboxOptions.Visible) then
  begin
    Result := ScreenJukeboxOptions.ParseInput(PressedKey, CharCode, PressedDown);
    Exit;
  end;

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT +
    KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT + KMOD_RALT);

  if (PressedDown) then
  begin // key down
    // check normal keys

    if (FindSongList) and (SongListVisible) then
    begin
      if (IsPrintableChar(CharCode)) then
      begin

        {if not (FindSongList) then
        begin
          UpperLetter := UCS4UpperCase(CharCode);

          GoToSongList(UpperLetter);
        end
        else
        begin}

        LastTick := SDL_GetTicks();

        Button[JukeboxFindSong].Text[0].Text := Button[JukeboxFindSong].Text[0].Text +
                                          UCS4ToUTF8String(CharCode);

        Filter := Button[JukeboxFindSong].Text[0].Text;
        FilterSongList(Filter);
        Exit;
        {end;}
      end
    end
    else
    begin
      case PressedKey of
        SDLK_Q:
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
        SDLK_V:
        begin
          if fShowWebcam then
          begin
            Webcam.Release;
            fShowWebCam:=false;
          end;
          if ((fShowBackground = true) and (Ini.VideoEnabled = 1) and CurrentSong.Video.IsSet())
                               or (fShowVisualization and not CurrentSong.Background.IsSet()) then //switch to video
          begin
            Log.LogStatus('decided to switch to video', 'UScreenSing.ParseInput');
            fShowBackground := false;
		    fShowWebCam := false;
            fCurrentVideo := nil;
            fShowVisualization := false;
            fCurrentVideo := fVideoClip;
            if (Assigned(fCurrentVideo)) then
               fCurrentVideo.Position := CurrentSong.VideoGAP + CurrentSong.Start + AudioPlayback.Position;
            Log.LogStatus('finished switching to video', 'UScreenSing.ParseInput');
          end
          else
          begin
            if fShowVisualization and CurrentSong.Background.IsSet() then
            begin //switch to Background only
              Log.LogStatus('decided to switch to background', 'UScreenSing.ParseInput');
              fShowBackground := true;
			  fShowWebCam := false;
              fCurrentVideo := nil;
              fShowVisualization := false;
              Log.LogStatus('finished switching to background', 'UScreenSing.ParseInput');
            end
            else
            begin //Video is currently visible, change to visualization
              Log.LogStatus('decided to switch to visualization', 'UScreenSing.ParseInput');
              fShowVisualization := true;
			  fShowWebCam := false;
              fCurrentVideo := Visualization.Open(PATH_NONE);
              fCurrentVideo.play;
              Log.LogStatus('finished switching to visualization', 'UScreenSing.ParseInput');
            end;
          end;
          Exit;
        end;

        // show Webcam
      SDLK_W:
      begin
        if (fShowWebCam = false) then
        begin
          fCurrentVideo := nil;
          fShowVisualization := false;
          fShowBackground := false;
          Webcam.Restart;
          if (Webcam.Capture = nil) then
          begin
            fShowWebCam := false;
            fShowBackground := true;
            ScreenPopupError.ShowPopup(Language.Translate('SING_OPTIONS_WEBCAM_NO_WEBCAM'))
          end
          else
            fShowWebCam := true;
        //  ChangeEffectLastTick := SDL_GetTicks;
        //  SelectsS[WebcamParamsSlide].Visible := true;
        //  LastTickFrame := SDL_GetTicks;
        end
        else
        begin
          Webcam.Release;
          fShowWebCam:=false;
        end;

        Exit;
      end;

        // allow search for songs
        SDLK_J:
        begin
          if (SongListVisible) then
          begin
            LastTick := SDL_GetTicks();
            FindSongList := not FindSongList;
            if (Filter = '') and (FindSongList) then
                Button[JukeboxFindSong].Text[0].Text := '';
            Button[JukeboxFindSong].SetSelect(FindSongList);
            SetTextInput(FindSongList);
            if FindSongList then
              FilterSongList(Filter)
            else
              FilterSongList('');
            Exit;
          end
          else
          begin
            SongListVisible := true;
            LastTick := SDL_GetTicks();
            FindSongList := true;
            if (Filter = '') then
                Button[JukeboxFindSong].Text[0].Text := '';
            Button[JukeboxFindSong].SetSelect(FindSongList);
            SetTextInput(FindSongList);
            FilterSongList(Filter);
            Exit;
          end;
        end;

        // skip intro
        SDLK_S:
        begin
          if (AudioPlayback.Position < CurrentSong.gap / 1000 - 6) then
          begin
            AudioPlayback.SetPosition(CurrentSong.gap / 1000.0 - 5.0);
              if (Assigned(fCurrentVideo)) then
                 fCurrentVideo.Position := CurrentSong.VideoGAP + CurrentSong.Start + (CurrentSong.gap / 1000.0 - 5.0);
          end;
          Exit;
        end;

        // pause
        SDLK_P:
        begin
          Pause;
          Exit;
        end;

        SDLK_R:
        begin
          if (SongListVisible) then
          begin
            RandomList();
          end;
        end;

        // toggle time display
        SDLK_T:
        begin
          LastTick := SDL_GetTicks();

          if (fTimebarMode = High(TTimebarMode)) then
            fTimebarMode := Low(TTimebarMode)
          else
            Inc(fTimebarMode);

          Ini.JukeboxTimebarMode := Ord(fTimebarMode);
          Ini.SaveJukeboxTimebarMode();

          Exit;
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
              AspectCorrection := acoLetterBox
            else
            begin
              if (AspectCorrection = acoHalfway) then
                AspectCorrection := acoCrop
              else
              begin
                if (AspectCorrection = acoLetterBox) then
                  AspectCorrection := acoHalfway;
              end;
            end;
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

        SDLK_O:
        begin
          if not (FindSongList) or not (SongListVisible) then
          begin
            ScreenJukeboxOptions.Visible := true;
            Button[JukeboxOptions].SetSelect(false);
            Exit;
          end;
        end;

        SDLK_S:
        begin

          if (SongListVisible) and (SDL_ModState = KMOD_LCTRL) then
          begin
            ChangeOrderList();
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

        SDLK_F:
        begin

          if (SongListVisible) and (SDL_ModState = KMOD_LCTRL) then
          begin
            LastTick := SDL_GetTicks();

            FindSongList := not FindSongList;

            if (Filter = '') then
            begin
              if (FindSongList) then
                Button[JukeboxFindSong].Text[0].Text := ''
            end;

            Button[JukeboxFindSong].SetSelect(FindSongList);
            SetTextInput(FindSongList);

            if not (FindSongList) then
            begin
              Button[JukeboxFindSong].Text[0].Selected := false;
              FilterSongList('');
            end
            else
            begin
              Button[JukeboxFindSong].Text[0].Selected := true;
              FilterSongList(Filter);
            end;

            Exit;
          end;
        end;

        SDLK_R:
        begin

          if (SongListVisible) and (SDL_ModState = KMOD_LCTRL) then
          begin
            RandomList();
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
            if (Filter = '') then
            begin
              FindSongList:=false;
              Button[JukeboxFindSong].SetSelect(FindSongList);
              SetTextInput(FindSongList);
              Exit;
            end;
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
          if not (FindSongList) then
            Pause;
        end;

        SDLK_TAB:
        begin
          if (SDL_ModState = KMOD_LCTRL) then // change visualization preset
          begin
            if fShowVisualization then
              fCurrentVideo.Position := now; // move to a random position

            if (fShowWebcam) then
            begin
              if (Ini.WebCamEffect < 10) then
                Ini.WebCamEffect := Ini.WebCamEffect + 1
              else
                Ini.WebCamEffect := 0;
            end;
          end
          else // show help popup
            ScreenPopupHelp.ShowPopup();
        end;

        SDLK_RETURN:
        begin
          if (SongListVisible) then
          begin
            LastTick := SDL_GetTicks();
            if (FindSongList) then
            begin
              FindSongList:=false;
              Button[JukeboxFindSong].SetSelect(FindSongList);
              SetTextInput(FindSongList);
            end;
            if (High(JukeboxVisibleSongs) < 0) then
            begin
              FilterSongList('');
            end;
            CurrentSongList := ActualInteraction - 1;
            Finish;
            PlayMusic(CurrentSongList, True);
          end;
        end;

        SDLK_LEFT:
        begin

          if not (SongListVisible) and (CurrentSongList > 0) then
          begin
            CurrentSongList := CurrentSongList - 1;
            PlayMusic(CurrentSongList, false);
          end;

        end;

        SDLK_RIGHT:
        begin

          if not (SongListVisible) and (CurrentSongList < High(JukeboxVisibleSongs)) then
          begin
            CurrentSongList := CurrentSongList + 1;
            PlayMusic(CurrentSongList, false);
          end;

        end;

        SDLK_DELETE:
        begin
          if (SongListVisible) then
          begin
            ScreenPopupCheck.ShowPopup('JUKEBOX_DELETE_SONG', OnDeleteSong, nil, false)
          end;
        end;

        SDLK_PAGEDOWN:
        begin
          PageDown(10);
        end;

        SDLK_PAGEUP:
        begin
          PageUp(10);
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

        end;

      end;
    end;
end;

procedure TScreenJukebox.PageDown(N: integer);
begin
  if (SongListVisible) and (High(JukeboxVisibleSongs) > 9) then
  begin
    LastTick := SDL_GetTicks();

    if (ListMin + N + Interaction < High(JukeboxVisibleSongs) - 9) then
    begin
      ActualInteraction := ListMin + N + Interaction;
      ListMin := ListMin + N;
    end
    else
    begin
      ActualInteraction := High(JukeboxVisibleSongs);
      ListMin := High(JukeboxVisibleSongs) - 9;
      Interaction := 9;
    end;
  end;
end;

procedure TScreenJukebox.PageUp(N: integer);
begin
  if (SongListVisible) and (High(JukeboxVisibleSongs) > 9) then
  begin
    LastTick := SDL_GetTicks();

    if (ListMin - N > 0) then
    begin
      ActualInteraction := ListMin - N;
      ListMin := ListMin - N;

      if ListMin < 0 then
        ListMin := 0;
    end
    else
    begin
      ActualInteraction := 0;
      ListMin := 0;
      Interaction := 0;
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
    if fCurrentVideo <> nil then
      fCurrentVideo.Pause;

  end
  else              // disable pause
  begin
    LyricsState.Start();

    // play music
    AudioPlayback.Play;

    // video
    if fCurrentVideo <> nil then
      fCurrentVideo.Pause;

    Paused := false;
  end;

  Button[JukeboxSongMenuPlayPause].SetSelect(Paused);
  Button[JukeboxPlayPause].SetSelect(Paused);

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

  RightMbESC := false;

  fShowVisualization := false;
  fShowWebcam := false;
  fShowBackground := false;

  fCurrentVideo := nil;

  LoadFromTheme(Theme.Jukebox);

  Lyrics := TLyricEngine.Create(
      Theme.LyricBar.UpperX, Theme.LyricBar.UpperY, Theme.LyricBar.UpperW, Theme.LyricBar.UpperH,
      Theme.LyricBar.LowerX, Theme.LyricBar.LowerY, Theme.LyricBar.LowerW, Theme.LyricBar.LowerH);

  fLyricsSync := TLyricsSyncSource.Create();
  fMusicSync := TMusicSyncSource.Create();

  //Jukebox Items
  JukeboxStaticTimeProgress       := AddStaticColorRectangle(Theme.Jukebox.StaticTimeProgress);
  JukeboxStaticTimeBackground     := AddStatic(Theme.Jukebox.StaticTimeBackground);
  JukeboxStaticSongBackground     := AddStatic(Theme.Jukebox.StaticSongBackground);
  JukeboxStaticSongListBackground := AddStatic(Theme.Jukebox.StaticSongListBackground);

  JukeboxTextTimeText         := AddText(Theme.Jukebox.TextTimeText);

  PosY := Theme.Jukebox.SongDescription.Y;
  for I := 0 to 9 do
  begin
    Theme.Jukebox.SongDescription.Y := PosY + Theme.Jukebox.SongDescription.H * I;
    SongDescription[I] := AddButton(Theme.Jukebox.SongDescription);
  end;

  SongDescriptionClone := AddButton(Theme.Jukebox.SongDescription);

  Button[SongDescriptionClone].DeSelectTexture.ColR := Theme.Jukebox.SongDescription.ColR;
  Button[SongDescriptionClone].DeSelectTexture.ColG := Theme.Jukebox.SongDescription.ColG;
  Button[SongDescriptionClone].DeSelectTexture.ColB := Theme.Jukebox.SongDescription.ColB;
  Button[SongDescriptionClone].Visible := false;

  SelectColR := Theme.Jukebox.SongDescription.ColR;
  SelectColG := Theme.Jukebox.SongDescription.ColG;
  SelectColB := Theme.Jukebox.SongDescription.ColB;

  JukeboxFindSong := AddButton(Theme.Jukebox.FindSong);
  JukeboxRepeatSongList := AddButton(Theme.Jukebox.RepeatSongList);
  JukeboxSongListOrder := AddButton(Theme.Jukebox.SongListOrder);
  JukeboxRandomSongList := AddButton(Theme.Jukebox.RandomSongList);
  JukeboxLyric := AddButton(Theme.Jukebox.Lyric);
  JukeboxOptions := AddButton(Theme.Jukebox.Options);
  JukeboxSongListClose := AddButton(Theme.Jukebox.SongListClose);
  JukeboxSongListFixPin := AddButton(Theme.Jukebox.SongListFixPin);
  JukeboxPlayPause := AddButton(Theme.Jukebox.SongListPlayPause);

  Button[JukeboxFindSong].Selectable := false;
  Button[JukeboxRepeatSongList].Selectable := false;
  Button[JukeboxSongListOrder].Selectable := false;
  Button[JukeboxRandomSongList].Selectable := false;
  Button[JukeboxLyric].Selectable := false;
  Button[JukeboxSongListClose].Selectable := false;
  Button[JukeboxOptions].Selectable := false;
  Button[JukeboxSongListFixPin].Selectable := false;
  Button[JukeboxPlayPause].Selectable := false;

  Button[JukeboxFindSong].Text[0].Writable := true;

  JukeboxListText  := AddText(Theme.Jukebox.TextListText);
  JukeboxCountText := AddText(Theme.Jukebox.TextCountText);

  StaticCover := AddStaticPosition(Theme.Jukebox.SongCover);

  SetLength(JukeboxStaticActualSongStatic, Length(Theme.Jukebox.StaticActualSongStatics));
  for I := 0 to High(Theme.Jukebox.StaticActualSongStatics) do
  begin
    JukeboxStaticActualSongStatic[I] := AddStatic(Theme.Jukebox.StaticActualSongStatics[i]);
  end;

  JukeboxStaticActualSongCover := AddStaticPosition(Theme.Jukebox.StaticActualSongCover);
  JukeboxTextActualSongArtist := AddText(Theme.Jukebox.TextActualSongArtist);
  JukeboxTextActualSongTitle := AddText(Theme.Jukebox.TextActualSongTitle);

  JukeboxSongListUp := AddButton(Theme.Jukebox.SongListUp);
  JukeboxSongListDown := AddButton(Theme.Jukebox.SongListDown);

  // Jukebox SongMenu Items
  JukeboxSongMenuPlayPause := AddButton(Theme.Jukebox.SongMenuPlayPause);
  JukeboxSongMenuNext      := AddButton(Theme.Jukebox.SongMenuNext);
  JukeboxSongMenuPrevious  := AddButton(Theme.Jukebox.SongMenuPrevious);
  JukeboxSongMenuPlaylist  := AddButton(Theme.Jukebox.SongMenuPlaylist);
  JukeboxSongMenuOptions   := AddButton(Theme.Jukebox.SongMenuOptions);

  Button[JukeboxSongMenuPlaylist].Selectable := false;
  Button[JukeboxSongMenuNext].Selectable := false;
  Button[JukeboxSongMenuPrevious].Selectable := false;
  Button[JukeboxSongMenuPlaylist].Selectable := false;
  Button[JukeboxSongMenuOptions].Selectable := false;

  JukeboxStaticSongMenuTimeProgress   := AddStaticColorRectangle(Theme.Jukebox.StaticSongMenuTimeProgress);
  JukeboxStaticSongMenuTimeBackground := AddStatic(Theme.Jukebox.StaticSongMenuTimeBackground);
  JukeboxTextSongMenuTimeText         := AddText(Theme.Jukebox.SongMenuTextTime);
  JukeboxStaticSongMenuBackground     := AddStatic(Theme.Jukebox.StaticSongMenuBackground);
end;

procedure TScreenJukebox.OnShow;
var
  Col: TRGB;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenJukebox');

  // songmenu
  if (Ini.JukeboxSongMenu = 1) then
    SongListVisibleFix := false
  else
    SongListVisibleFix := true;

  Button[JukeboxSongListFixPin].SetSelect(SongListVisibleFix);

  {**
  * Pause background music
  *}
  SoundLib.PauseBgMusic;

  FadeOut := false;

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

  Lyrics.FontFamily := Ini.JukeboxFont;
  Lyrics.FontStyle := Ini.JukeboxStyle;

  case Ini.JukeboxStyle of
    0, 1: // regular/bold (non-outline) font
    begin
      Lyrics.LineColor_en.R := Skin_FontR;
      Lyrics.LineColor_en.G := Skin_FontG;
      Lyrics.LineColor_en.B := Skin_FontB;
      Lyrics.LineColor_en.A := 1;

      Lyrics.LineColor_dis.R := 0.2;
      Lyrics.LineColor_dis.G := 0.2;
      Lyrics.LineColor_dis.B := 0.2;
      Lyrics.LineColor_dis.A := 1;

      if (Ini.JukeboxSingLineColor = High(UIni.ISingLineColor)) then
        Col := GetJukeboxLyricOtherColor(0)
      else
        Col := GetLyricColor(Ini.JukeboxSingLineColor);

      Lyrics.LineColor_act.R := Col.R; //0.02;
      Lyrics.LineColor_act.G := Col.G; //0.6;
      Lyrics.LineColor_act.B := Col.B; //0.8;
      Lyrics.LineColor_act.A := 1;
    end;
    2: // outline fonts
    begin
      if (Ini.JukeboxSingLineColor = High(UIni.ISingLineColor)) then
        Col := GetJukeboxLyricOtherColor(0)
      else
        Col := GetLyricColor(Ini.JukeboxSingLineColor);
      Lyrics.LineColor_act.R := Col.R;
      Lyrics.LineColor_act.G := Col.G;
      Lyrics.LineColor_act.B := Col.B;
      Lyrics.LineColor_act.A := 1;

      if (Ini.JukeboxActualLineColor = High(UIni.IActualLineColor)) then
        Col := GetJukeboxLyricOtherColor(1)
      else
        Col := GetLyricGrayColor(Ini.JukeboxActualLineColor);
      Lyrics.LineColor_en.R := Col.R;
      Lyrics.LineColor_en.G := Col.G;
      Lyrics.LineColor_en.B := Col.B;
      Lyrics.LineColor_en.A := 1;

      if (Ini.JukeboxNextLineColor = High(UIni.INextLineColor)) then
        Col := GetJukeboxLyricOtherColor(2)
      else
        Col := GetLyricGrayColor(Ini.JukeboxNextLineColor);
      Lyrics.LineColor_dis.R := Col.R;
      Lyrics.LineColor_dis.G := Col.G;
      Lyrics.LineColor_dis.B := Col.B;
      Lyrics.LineColor_dis.A := 1;
    end;
  end; // case

  SongMenuVisible := false;
  SongListVisible := true;

  Log.LogStatus('End', 'OnShow');
end;

procedure TScreenJukebox.OnShowFinish();
begin
  Reset;

  PlayMusic(0, true);
end;

procedure TScreenJukebox.Play();
var
  I: integer;
begin
    AudioPlayback.Open(CurrentSong.Path.Append(CurrentSong.Mp3),nil);
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
  LastTickChangeSong := SDL_GetTicks();
  LyricsStart := false;

end;

procedure TScreenJukebox.OnHide;
begin
  // background texture
  if (Tex_Background.TexNum > 0) then
  begin
    glDeleteTextures(1, PGLuint(@Tex_Background.TexNum));
    Tex_Background.TexNum := 0;
  end;
  if fShowWebcam then
        begin
          Webcam.Release;
          fShowWebCam:=false;
        end;
  Background.OnFinish;
  //Display.SetCursor;
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
  if (not fShowVisualization) or (fShowBackground) then
    SingDrawJukeboxBackground;

  if (fShowWebCam) then
    SingDrawWebCamFrame;


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
  Text[JukeboxTextSongMenuTimeText].Text := Format('%s%.2d:%.2d', [DisplayPrefix, DisplayMin, DisplaySec]);

  // update and draw movie
  if Assigned(fCurrentVideo) and (not fShowWebcam) then
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
      try
         fCurrentVideo.GetFrame(VideoFrameTime);
      except
      end;
    end;

    fCurrentVideo.AspectCorrection := acoLetterBox;
    fCurrentVideo.SetScreen(ScreenAct);
    fCurrentVideo.Draw;
    //DrawBlackBars();
  end;

  SingDrawJukebox;

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

  if (ScreenAct = 1) and (SongMenuVisible) then
    DrawSongMenu;

  if (ScreenAct = 1) and (ScreenJukeboxOptions.Visible) then
    ScreenJukeboxOptions.Draw;

  // draw time-bar
  if (ScreenAct = 1) and (ScreenJukebox.SongListVisible or ScreenJukebox.SongMenuVisible) then
    SingDrawJukeboxTimeBar();

  DrawSongInfo;

  // for move song
  if (Button[SongDescriptionClone].Visible) then
  begin
    LastTick := SDL_GetTicks();

    if (MoveSong) then
    begin
      DrawMoveLine();

      if (MoveDown) then
      begin
        if (SDL_GetTicks() - MouseDownList >= MAX_TIME_MOUSE_CHANGELIST) then
        begin
          MouseDownList := SDL_GetTicks();

          if (ListMin + 9 < High(JukeboxVisibleSongs)) then
          begin
            ListMin := ListMin + 1;
            ActualInteraction := ActualInteraction + 1;
          end;
        end;
      end;

      if (MoveUp) then
      begin
        if (SDL_GetTicks() - MouseUpList >= MAX_TIME_MOUSE_CHANGELIST) then
        begin
          MouseUpList := SDL_GetTicks();

          if (ListMin > 0) then
          begin
            ListMin := ListMin - 1;
            ActualInteraction := ActualInteraction - 1;
          end;
        end;
      end;

    end;

    Button[SongDescriptionClone].Draw();
  end;
  if Paused = true then
     SDL_Delay(33);
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
      PlayMusic(CurrentSongList, false);
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

    PlayMusic(CurrentSongList, false);
  end;
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
    if (Lyrics.LineCounter <= High(Tracks[0].Lines)) then
      Lyrics.AddLine(@Tracks[0].Lines[Lyrics.LineCounter])
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

procedure TScreenJukebox.DrawSongInfo();
var
  I: integer;
  Alpha: real;
  CurrentTick: integer;
begin

  if (not(SongListVisible) and not(ScreenJukeboxOptions.Visible) and (ScreenAct = 1)) or (ScreenAct = 2) then
  begin
    CurrentTick := SDL_GetTicks() - LastTickChangeSong;

    if (CurrentTick < MAX_TIME_SONGDESC) then
      Alpha := 0
    else
      Alpha := (CurrentTick - MAX_TIME_SONGDESC)/MAX_TIME_FADESONGDESC;

    if (CurrentTick - MAX_TIME_SONGDESC < MAX_TIME_FADESONGDESC) then
    begin
      for I := 0 to High(Theme.Jukebox.StaticActualSongStatics) do
      begin
        Statics[JukeboxStaticActualSongStatic[I]].Texture.Alpha := 1 - Alpha;
        Statics[JukeboxStaticActualSongStatic[I]].Draw;
      end;

      Statics[JukeboxStaticActualSongCover].Texture.Alpha := 1 - Alpha;
      Statics[JukeboxStaticActualSongCover].Draw;

      Text[JukeboxTextActualSongArtist].Alpha := 1 - Alpha;
      Text[JukeboxTextActualSongArtist].Draw;

      Text[JukeboxTextActualSongTitle].Alpha := 1 - Alpha;
      Text[JukeboxTextActualSongTitle].Draw;
    end;
  end;

end;

procedure TScreenJukebox.PlayMusic(ID: integer; ShowList: boolean);
var
  Index:  integer;
  VideoFile, BgFile: IPath;
  success: boolean;
  Max: integer;
  CoverPath: IPath;
begin

  // background texture (garbage disposal)
  if (Tex_Background.TexNum > 0) then
  begin
    glDeleteTextures(1, PGLuint(@Tex_Background.TexNum));
    Tex_Background.TexNum := 0;
  end;

  try
    if high(JukeboxVisibleSongs) < ID then
       ID:=0;
    if high(JukeboxVisibleSongs) < 0 then
       Finish;
    CurrentSong := CatSongs.Song[JukeboxVisibleSongs[ID]];
    Text[JukeboxTextActualSongArtist].Text := CurrentSong.Artist;
    Text[JukeboxTextActualSongTitle].Text := CurrentSong.Title;

  Button[JukeboxSongMenuPlaylist].SetSelect(false);
  Paused := false;

  // Cover
  RefreshCover;

  // reset video playback engine
  fVideoClip := nil;
  fCurrentVideo := nil;

  AspectCorrection := acoLetterBox;

  fTimebarMode := TTimebarMode(Ini.JukeboxTimebarMode);

  // FIXME: bad style, put the try-except into loadsong() and not here
  try
    success := CurrentSong.Analyse and CurrentSong.LoadSong(false);
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
      fCurrentVideo.Position := CurrentSong.VideoGAP + CurrentSong.Start;
      //fCurrentVideo.Play;
    end;
  end;

  {*
   * set background to: picture
   *}
  //if (CurrentSong.Background.IsSet) and (fVideoClip = nil)
  //  and (TVisualizerOption(Ini.VisualizerOption) = voOff)  then
  if (CurrentSong.Background.IsSet) then
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
  //if (TVisualizerOption(Ini.VisualizerOption) in [voOn]) then
  if (not fShowWebcam) and (TVisualizerOption(Ini.VisualizerOption) in [voOn]) then
  begin
    fShowVisualization := true;
    fShowBackground := false;
    fCurrentVideo := Visualization.Open(PATH_NONE);
    if (fCurrentVideo <> nil) then
      fCurrentVideo.Play;
  end;

  {*
   * set background to: visualization (Videos are still shown)
   *}
  //if ((TVisualizerOption(Ini.VisualizerOption) in [voWhenNoVideo]) and
  //   (fVideoClip = nil)) then
  if (not fShowWebcam) and ((TVisualizerOption(Ini.VisualizerOption) in [voWhenNoVideo]) and
      (fVideoClip = nil)) then
  begin
    fShowVisualization := true;
    fShowBackground := false;
    fCurrentVideo := Visualization.Open(PATH_NONE);
    if (fCurrentVideo <> nil) then
      fCurrentVideo.Play;
  end;

  // load options
  LoadJukeboxSongOptions();

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

  // initialize lyrics by filling its queue
  while (not Lyrics.IsQueueFull) and
        (Lyrics.LineCounter <= High(Tracks[0].Lines)) do
  begin
    Lyrics.AddLine(@Tracks[0].Lines[Lyrics.LineCounter]);
  end;

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
  Button[JukeboxSongListUp].Visible := true;
  Button[JukeboxSongListDown].Visible := true;
  Button[JukeboxSongListClose].Visible := true;
  Button[JukeboxOptions].Visible := true;
  Button[JukeboxPlayPause].Visible := true;

  CurrentSongID := JukeboxVisibleSongs[CurrentSongList];

  if (not SongListVisibleFix) then
    SongListVisible := ShowList;

  Play();
  except
    ;
  end;
end;

procedure TScreenJukebox.RefreshCover();
var
  CoverPath: IPath;
begin

  CoverPath := CurrentSong.Path.Append(CurrentSong.Cover);

  Statics[StaticCover].Texture := Texture.GetTexture(CoverPath, TEXTURE_TYPE_PLAIN, false);

  if (Statics[StaticCover].Texture.TexNum = 0) then
    Statics[StaticCover].Texture := Texture.GetTexture(Skin.GetTextureFileName('SongCover'), TEXTURE_TYPE_PLAIN, false);

  Statics[StaticCover].Texture.X := Theme.Jukebox.SongCover.X;
  Statics[StaticCover].Texture.Y := Theme.Jukebox.SongCover.Y;
  Statics[StaticCover].Texture.W := Theme.Jukebox.SongCover.W;
  Statics[StaticCover].Texture.H := Theme.Jukebox.SongCover.H;
  Statics[StaticCover].Texture.Alpha := 0.7;

  Statics[JukeboxStaticActualSongCover].Texture := Statics[StaticCover].Texture;
  Statics[JukeboxStaticActualSongCover].Texture.X := Theme.Jukebox.StaticActualSongCover.X;
  Statics[JukeboxStaticActualSongCover].Texture.Y := Theme.Jukebox.StaticActualSongCover.Y;
  Statics[JukeboxStaticActualSongCover].Texture.W := Theme.Jukebox.StaticActualSongCover.W;
  Statics[JukeboxStaticActualSongCover].Texture.H := Theme.Jukebox.StaticActualSongCover.H;
  Statics[JukeboxStaticActualSongCover].Texture.Alpha := 1;
end;

procedure TScreenJukebox.DrawPlaylist;
var
  Report: string;
  I, J, Max: integer;
  SongDesc, Artist, Title, TimeString: UTF8String;
  CurrentTick: integer;
  Time: real;
begin
  CurrentTick := SDL_GetTicks() - LastTick;

  if ((SongListVisibleFix) or (CurrentTick < MAX_TIME_PLAYLIST)) then
  begin
    SongMenuVisible := false;

    Statics[JukeboxStaticTimeBackground].Draw;
    Statics[JukeboxStaticTimeProgress].Draw;
    Statics[JukeboxStaticSongListBackground].Draw;

    Statics[StaticCover].Draw;

    Text[JukeboxTextTimeText].Draw;

    Button[JukeboxSongListUp].Draw;
    Button[JukeboxSongListDown].Draw;

    Max := 9;
    if (High(JukeboxVisibleSongs) < 9) then
      Max := High(JukeboxVisibleSongs);

    if (Max < 0) then
      ActualInteraction := -1;

    Text[JukeboxCountText].Text := IntToStr(ActualInteraction + 1) + '/' + IntToStr(length(JukeboxVisibleSongs));

    Text[JukeboxListText].Draw;
    Text[JukeboxCountText].Draw;

    Button[JukeboxFindSong].Draw;
    Button[JukeboxSongListOrder].Draw;
    Button[JukeboxRepeatSongList].Draw;
    Button[JukeboxRandomSongList].Draw;
    Button[JukeboxLyric].Draw;
    Button[JukeboxSongListClose].Draw;
    Button[JukeboxOptions].Draw;
    Button[JukeboxSongListFixPin].Draw;
    Button[JukeboxPlayPause].Draw;

    for I := 0 to 9 do
    begin
      try
        Button[SongDescription[I]].Visible := true;

        if (I <= Max) then
        begin
          Button[SongDescription[I]].Selectable := true;
          Artist := CatSongs.Song[JukeboxVisibleSongs[I + ListMin]].Artist;
          Title := CatSongs.Song[JukeboxVisibleSongs[I + ListMin]].Title;

          if (OrderType = 2) then
            SongDesc := Title + ' - ' + Artist
          else
            SongDesc := Artist + ' - ' + Title;

            {
          if (CatSongs.Song[JukeboxVisibleSongs[I + ListMin]].Finish <> 0) then
            Time := CatSongs.Song[JukeboxVisibleSongs[I + ListMin]].Finish/1000
          else
          begin
            AudioPlayback.Open(CatSongs.Song[JukeboxVisibleSongs[I + ListMin]].Mp3);
            Time := AudioPlayback.Length;
            AudioPlayback.Close();
          //  Time := CatSongs.Song[JukeboxVisibleSongs[I + ListMin]].MP3Length;

          end;

          TimeString := IntToStr(Round(Time) div 60) + ':' + Format('%.*d', [2, Round(Time) mod 60]);
          }
        end
        else
        begin
          Button[SongDescription[I]].Selectable := false;
          SongDesc := '';
          TimeString := '';
        end;

        if (Max > -1) then
        begin
          if (JukeboxVisibleSongs[I + ListMin] = CurrentSongID) and (not Button[SongDescription[I]].Selected) then
          begin
            Button[SongDescription[I]].Text[0].ColR := SelectColR;
            Button[SongDescription[I]].Text[0].ColG := SelectColG;
            Button[SongDescription[I]].Text[0].ColB := SelectColB;

            Button[SongDescription[I]].Text[1].ColR := SelectColR;
            Button[SongDescription[I]].Text[1].ColG := SelectColG;
            Button[SongDescription[I]].Text[1].ColB := SelectColB;
          end
          else
          begin
            Button[SongDescription[I]].Text[0].ColR := 1;
            Button[SongDescription[I]].Text[0].ColG := 1;
            Button[SongDescription[I]].Text[0].ColB := 1;

            Button[SongDescription[I]].Text[1].ColR := 1;
            Button[SongDescription[I]].Text[1].ColG := 1;
            Button[SongDescription[I]].Text[1].ColB := 1;
          end;
        end
        else
          Interaction := -1;

        Button[SongDescription[I]].Text[0].Text := SongDesc;
        Button[SongDescription[I]].Text[1].Text := TimeString;
        Button[SongDescription[I]].Draw;
      except
        on E : Exception do
        begin
          Report := 'Drawing of a jukebox entry failed. Check your theme files.' + LineEnding +
          'Stacktrace:' + LineEnding;
          if E <> nil then
          begin
	    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
	    'Message: ' + E.Message + LineEnding;
          end;
          Report := Report + BackTraceStrFunc(ExceptAddr);
          for J := 0 to ExceptFrameCount - 1 do
          begin
	    Report := Report + LineEnding + BackTraceStrFunc(ExceptFrames[J]);
          end;
          Log.LogWarn(Report, 'UScreenJukebox.DrawPlaylist');
        end;
      end;
    end;

    {
    DrawLine(Button[JukeboxSongListUp].X,
                    Button[JukeboxSongListUp].Y + Button[JukeboxSongListUp].H - 1,
                    Button[JukeboxSongListUp].W,
                    2);

    DrawLine(Button[JukeboxSongListDown].X,
                    Button[JukeboxSongListDown].Y - 1,
                    Button[JukeboxSongListDown].W,
                    2);
   }
  end
  else
    SongListVisible := false;

end;

procedure TScreenJukebox.DrawSongMenu;
var
  I, Max: integer;
  CurrentTick: integer;
begin
  CurrentTick := SDL_GetTicks() - LastSongMenuTick;

  if (CurrentTick < MAX_TIME_SONGMENU) then
  begin
    Statics[JukeboxStaticSongMenuTimeBackground].Draw;
    Statics[JukeboxStaticSongMenuTimeProgress].Draw;
    Statics[JukeboxStaticSongMenuBackground].Draw;

    Text[JukeboxTextSongMenuTimeText].Draw;

    Button[JukeboxSongMenuPlaylist].Draw;
    Button[JukeboxSongMenuOptions].Draw;
    Button[JukeboxSongMenuNext].Draw;
    Button[JukeboxSongMenuPrevious].Draw;
    Button[JukeboxSongMenuPlayPause].Draw;
  end
  else
    SongMenuVisible := false;
end;

procedure TScreenJukebox.LoadJukeboxSongOptions();
var
  Opts: TSongOptions;
begin

  Opts := DataBase.GetSongOptions(CurrentSong);

  if (Opts = nil) then
  begin
    ScreenJukeboxOptions.LoadDefaultOptions;
  end
  else
  begin

    if (Opts.LyricSingFillColor = '') then
    begin
      ScreenJukeboxOptions.LoadDefaultOptions;
      Exit;
    end;

    ScreenJukeboxOptions.LoadSongOptions(Opts);
  end;
end;

end.

