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

unit UScreenJukeboxOptions;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UCommon,
  UDataBase,
  UDisplay,
  UFiles,
  UMenu,
  UMusic,
  USong,
  USongs,
  UThemes,
  sdl2,
  SysUtils;

type
  TScreenJukeboxOptions = class(TMenu)
    private
      fVisible: boolean;

      OptionClose:   cardinal;
      OptionDefault: cardinal;
      OptionSave:    cardinal;

      TextSaved: cardinal;

      AspectVideo: array [0..2] of UTF8String;
      HeightVideo: array [0..200] of UTF8String;
      WidthVideo: array [0..200] of UTF8String;
      PositionLyric: array [0..100] of UTF8String;

      LineSelect:      integer;
      PropertySelect:  integer;
      LineColorSelect: integer;

      RedSelect:       integer;
      GreenSelect:     integer;
      BlueSelect:      integer;

      PointerR: integer;
      PointerG: integer;
      PointerB: integer;

      TexR:   integer;
      TexG:  integer;
      TexB:  integer;
      //TexColor:  integer;

      Red:   integer;
      Green: integer;
      Blue:  integer;

      RSalt: real;
      GSalt: real;
      BSalt: real;

      StartR: real;
      StartG: real;
      StartB: real;

      JukeboxOptionsSingLineColor:   integer;
      JukeboxOptionsActualLineColor: integer;
      JukeboxOptionsNextLineColor:   integer;

      JukeboxOptionsSingLineOtherColorR: integer;
      JukeboxOptionsSingLineOtherColorG: integer;
      JukeboxOptionsSingLineOtherColorB: integer;

      JukeboxOptionsActualLineOtherColorR: integer;
      JukeboxOptionsActualLineOtherColorG: integer;
      JukeboxOptionsActualLineOtherColorB: integer;

      JukeboxOptionsNextLineOtherColorR: integer;
      JukeboxOptionsNextLineOtherColorG: integer;
      JukeboxOptionsNextLineOtherColorB: integer;

      JukeboxOptionsSingLineOtherOColorR: integer;
      JukeboxOptionsSingLineOtherOColorG: integer;
      JukeboxOptionsSingLineOtherOColorB: integer;

      JukeboxOptionsActualLineOtherOColorR: integer;
      JukeboxOptionsActualLineOtherOColorG: integer;
      JukeboxOptionsActualLineOtherOColorB: integer;

      JukeboxOptionsNextLineOtherOColorR: integer;
      JukeboxOptionsNextLineOtherOColorG: integer;
      JukeboxOptionsNextLineOtherOColorB: integer;

      IsDefaultOptions: boolean;
      
      //Whether the Menu should be Drawn
      //Visible //Whether the Menu should be Drawn
      procedure SetVisible(Value: boolean);
    public

      AspectVideoIndex: integer;
      HeightVideoIndex: integer;
      WidthVideoIndex: integer;
      PositionLyricIndex: integer;

      AlphaLyricIndex: integer;
      ColorLyricIndex: integer;
      LineLyricIndex: integer;
      PropertyLyricIndex: integer;

      constructor Create; override;

      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      function ParseMouse(MouseButton: Integer; BtnDown: Boolean; X, Y: integer): boolean; override;

      procedure OnShow; override;
      function Draw: boolean; override;

      procedure ChangeOtherColor;
      procedure ChangeOtherOColor;
      procedure ChangeColor;
      procedure RefreshSelectsColors;

      procedure UpdatePropertyList;

      procedure Update;
      procedure UpdateLyricColor;
      procedure UpdateVideoProperty;

      procedure ResetVideo;

      function GetJukeboxOptionsLyricOtherColor(Line: integer): TRGB;
      function GetJukeboxOptionsLyricOtherOutlineColor(Line: integer): TRGB;

      procedure LoadDefaultOptions;
      procedure LoadSongOptions(Opts: TSongOptions);
      procedure SaveSongOptions;

      property Visible: boolean read fVisible write SetVisible;
  end;

const
  ID='ID_042';   //for help system

implementation

uses
  UGraphic,
  UHelp,
  UIni,
  ULanguage,
  ULog,
  UMain,
  UParty,
  UScreenJukebox,
  UTexture,
  UUnicodeUtils;

function TScreenJukeboxOptions.ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean;
begin
  Result := true;

  inherited ParseMouse(MouseButton, BtnDown, X, Y);

  // transfer mousecords to the 800x600 raster we use to draw
  X := Round((X / (ScreenW / Screens)) * RenderW);
  if (X > RenderW) then
    X := X - RenderW;
  Y := Round((Y / ScreenH) * RenderH);

  if (BtnDown) then
  begin
    if InRegion(X, Y, Button[OptionClose].GetMouseOverArea) then
    begin
      Visible := false;
      ScreenJukebox.CloseClickTime := SDL_GetTicks;
    end;
  end
  else
  begin
    //hover
    if InRegion(X, Y, Button[OptionClose].GetMouseOverArea) then
      Button[OptionClose].SetSelect(true)
    else
      Button[OptionClose].SetSelect(false);
  end;

end;

function TScreenJukeboxOptions.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  SDL_ModState:  word;
  Salt_Mod:      integer;
begin
  Result := true;

  if (PressedDown) then
  begin // Key Down

    SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
              + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);

    // check special keys
    case PressedKey of
      SDLK_O:
        begin
          Visible := false;
          Exit;
        end;

      SDLK_BACKSPACE,
      SDLK_ESCAPE:
        begin
          Visible := false;
          Exit;
        end;

      SDLK_TAB:
      begin
        ScreenPopupHelp.ShowPopup();
      end;

      SDLK_SPACE:
        begin
          ScreenJukebox.Pause;
        end;

      SDLK_RETURN:
        begin

          // default
          if (Interaction = 11) then
          begin
            LoadDefaultOptions;
          end;

          // save
          if (Interaction = 12) then
          begin
            SaveSongOptions;
          end;

          // close
          if (Interaction = 13) then
          begin
            Visible := false;
            ScreenJukebox.CloseClickTime := SDL_GetTicks;
            Exit;
          end;

        end;

      SDLK_DOWN:
        begin
          InteractNext;
        end;

      SDLK_UP:
        begin
          InteractPrev;
        end;

      SDLK_RIGHT:
        begin

          if (Interaction in [1,2,3, RedSelect, GreenSelect, BlueSelect]) then
          begin
            // saved info
            Text[TextSaved].Visible := false;
            IsDefaultOptions := false;

            if (Interaction in [RedSelect, GreenSelect, BlueSelect]) then
            begin
              if (SDL_ModState and (KMOD_LSHIFT or KMOD_RSHIFT) <> 0) and (SelectsS[SelInteraction].SelectOptInt <= 245) then
                Salt_Mod := 9
              else
                Salt_Mod := 0;

              if (SelectsS[PropertySelect].SelectedOption = 0) then
              begin
                case SelectsS[LineSelect].SelectedOption of
                  0: SelectsS[LineColorSelect].SetSelectOpt(High(ISingLineColor));
                  1: SelectsS[LineColorSelect].SetSelectOpt(High(IActualLineColor));
                  2: SelectsS[LineColorSelect].SetSelectOpt(High(INextLineColor));
                end;
              end;

              if (SelectsS[PropertySelect].SelectedOption = 1) then
                SelectsS[LineColorSelect].SetSelectOpt(2);

              if (SelInteraction = RedSelect) and (Red < 255) then
              begin
                Red := Red + Salt_Mod;
                SelectsS[SelInteraction].SelectOptInt := Red;
                Statics[PointerR].Texture.X := Statics[PointerR].Texture.X + RSalt * (Salt_Mod + 1);
              end;

              if (SelInteraction = GreenSelect) and (Green < 255) then
              begin
                Green := Green + Salt_Mod;
                SelectsS[SelInteraction].SelectOptInt := Green;
                Statics[PointerG].Texture.X := Statics[PointerG].Texture.X + GSalt * (Salt_Mod + 1);
              end;

              if (SelInteraction = BlueSelect) and (Blue < 255) then
              begin
                Blue := Blue + Salt_Mod;
                SelectsS[SelInteraction].SelectOptInt := Blue;
                Statics[PointerB].Texture.X := Statics[PointerB].Texture.X + BSalt * (Salt_Mod + 1);
              end;

              InteractInc;

              RefreshSelectsColors;
            end;

            if (Interaction = 1) and (WidthVideoIndex < 200) then
            begin
              InteractInc;
              ScreenJukebox.ChangeVideoWidth(4);
            end;

            if (Interaction = 2) and (AspectVideoIndex <> 2) and (HeightVideoIndex < 200) then
            begin
              InteractInc;
              ScreenJukebox.ChangeVideoHeight(4);
            end;

            if (Interaction = 3) then
            begin
              if (PositionLyricIndex < 100) then
              begin
                InteractInc;
                ScreenJukebox.ChangeLyricPosition(5);
              end;
            end;
          end
          else
          begin
            InteractInc;

            if (Interaction in [0, 4, 5, 6, 7]) then
            begin
              // saved info
              Text[TextSaved].Visible := false;
              IsDefaultOptions := false;

              Update;
            end;

          end;

        end;
      SDLK_LEFT:
        begin

          if (Interaction in [1,2,3, RedSelect, GreenSelect, BlueSelect]) then
          begin

            // saved info
            Text[TextSaved].Visible := false;
            IsDefaultOptions := false;

            if (Interaction in [RedSelect, GreenSelect, BlueSelect]) then
            begin
              if (SDL_ModState and (KMOD_LSHIFT or KMOD_RSHIFT) <> 0) and (SelectsS[SelInteraction].SelectOptInt >= 10) then
                Salt_Mod := 9
              else
                Salt_Mod := 0;

              if (SelectsS[PropertySelect].SelectedOption = 0) then
              begin
                case SelectsS[LineSelect].SelectedOption of
                  0: SelectsS[LineColorSelect].SetSelectOpt(High(ISingLineColor));
                  1: SelectsS[LineColorSelect].SetSelectOpt(High(IActualLineColor));
                  2: SelectsS[LineColorSelect].SetSelectOpt(High(INextLineColor));
                end;
              end;

              if (SelectsS[PropertySelect].SelectedOption = 1) then
                SelectsS[LineColorSelect].SetSelectOpt(2);

              if (SelInteraction = RedSelect) and (Red > 0) then
              begin
                Red := Red - Salt_Mod;
                SelectsS[SelInteraction].SelectOptInt := Red;
                Statics[PointerR].Texture.X := Statics[PointerR].Texture.X - RSalt * (Salt_Mod + 1);
              end;

              if (SelInteraction = GreenSelect) and (Green > 0) then
              begin
                Green := Green - Salt_Mod;
                SelectsS[SelInteraction].SelectOptInt := Green;
                Statics[PointerG].Texture.X := Statics[PointerG].Texture.X - GSalt * (Salt_Mod + 1);
              end;

              if (SelInteraction = BlueSelect) and (Blue > 0) then
              begin
                Blue := Blue - Salt_Mod;
                SelectsS[SelInteraction].SelectOptInt := Blue;
                Statics[PointerB].Texture.X := Statics[PointerB].Texture.X - BSalt * (Salt_Mod + 1);
              end;

              InteractDec;

              RefreshSelectsColors;

            end;

            if (Interaction = 1) and (WidthVideoIndex > 0) then
            begin
              InteractDec;
              ScreenJukebox.ChangeVideoWidth(-4);
            end;

            if (Interaction = 2) and (AspectVideoIndex <> 2) and (HeightVideoIndex > 0) then
            begin
              InteractDec;
              ScreenJukebox.ChangeVideoHeight(-4);
            end;

            if (Interaction = 3) then
            begin
              if (PositionLyricIndex > 0) then
              begin
                InteractDec;

                ScreenJukebox.ChangeLyricPosition(-5);
              end;
            end;

          end
          else
          begin

            InteractDec;

            if (Interaction in [0, 4, 5, 6, 7]) then
            begin
              // saved info
              Text[TextSaved].Visible := false;
              IsDefaultOptions := false;

              Update;
            end;

          end;
        end;
    end;
  end;
end;

constructor TScreenJukeboxOptions.Create;
var
  I : integer;
begin
  inherited Create;

  AspectVideoIndex := 0;
  HeightVideoIndex := 100;
  WidthVideoIndex := 100;
  AlphaLyricIndex := 0;
  PositionLyricIndex := 0;
  ColorLyricIndex := 0;
  LineLyricIndex := 0;
  PropertyLyricIndex := 0;

  AddStatic(Theme.Jukebox.StaticSongOptionsBackground);

  AddText(Theme.Jukebox.SongOptionsVideoText);
  AddText(Theme.Jukebox.SongOptionsLyricText);

  AspectVideo[0] := Language.Translate('JUKEBOX_SONGOPTIONS_VIDEO_STRETCH');
  AspectVideo[1] := Language.Translate('JUKEBOX_SONGOPTIONS_VIDEO_CROP');
  AspectVideo[2] := Language.Translate('JUKEBOX_SONGOPTIONS_VIDEO_LETTERBOX');

  Theme.Jukebox.SongOptionsVideoAspectSlide.showArrows  := true;
  Theme.Jukebox.SongOptionsVideoAspectSlide.oneItemOnly := true;
  AddSelectSlide(Theme.Jukebox.SongOptionsVideoAspectSlide, AspectVideoIndex, AspectVideo);

  for I := 0 to 100 do
    WidthVideo[I] := IntToStr(I - 100);

  for I := 1 to 100 do
    WidthVideo[I + 100] := IntToStr(I);

  Theme.Jukebox.SongOptionsVideoWidthSlide.showArrows  := true;
  Theme.Jukebox.SongOptionsVideoWidthSlide.oneItemOnly := true;
  AddSelectSlide(Theme.Jukebox.SongOptionsVideoWidthSlide, WidthVideoIndex, WidthVideo);

  for I := 0 to 100 do
    HeightVideo[I] := IntToStr(I - 100);

  for I := 1 to 100 do
    HeightVideo[I + 100] := IntToStr(I);

  Theme.Jukebox.SongOptionsVideoHeightSlide.showArrows  := true;
  Theme.Jukebox.SongOptionsVideoHeightSlide.oneItemOnly := true;
  AddSelectSlide(Theme.Jukebox.SongOptionsVideoHeightSlide, HeightVideoIndex, HeightVideo);

  for I := 0 to 50 do
    PositionLyric[I] := IntToStr(I - 50);

  for I := 1 to 50 do
    PositionLyric[I + 50] := IntToStr(I);

  Theme.Jukebox.SongOptionsLyricPositionSlide.showArrows  := true;
  Theme.Jukebox.SongOptionsLyricPositionSlide.oneItemOnly := true;
  AddSelectSlide(Theme.Jukebox.SongOptionsLyricPositionSlide, PositionLyricIndex, PositionLyric);

  Theme.Jukebox.SongOptionsLyricAlphaSlide.showArrows  := true;
  Theme.Jukebox.SongOptionsLyricAlphaSlide.oneItemOnly := true;
  AddSelectSlide(Theme.Jukebox.SongOptionsLyricAlphaSlide, AlphaLyricIndex, ILyricsAlpha);

  Theme.Jukebox.SongOptionsLyricLineSlide.showArrows  := true;
  Theme.Jukebox.SongOptionsLyricLineSlide.oneItemOnly := true;
  LineSelect := AddSelectSlide(Theme.Jukebox.SongOptionsLyricLineSlide, LineLyricIndex, ILineTranslated);

  Theme.Jukebox.SongOptionsLyricPropertySlide.showArrows  := true;
  Theme.Jukebox.SongOptionsLyricPropertySlide.oneItemOnly := true;
  PropertySelect := AddSelectSlide(Theme.Jukebox.SongOptionsLyricPropertySlide, PropertyLyricIndex, IPropertyTranslated);

  Theme.Jukebox.SongOptionsLyricColorSlide.showArrows  := true;
  Theme.Jukebox.SongOptionsLyricColorSlide.oneItemOnly := true;
  LineColorSelect := AddSelectSlide(Theme.Jukebox.SongOptionsLyricColorSlide, ColorLyricIndex, ISingLineColorTranslated);

  Theme.Jukebox.SelectR.showArrows := false;
  Theme.Jukebox.SelectR.oneItemOnly := true;
  RedSelect := AddSelectSlide(Theme.Jukebox.SelectR, Red, IRed);

  Theme.Jukebox.SelectG.showArrows := false;
  Theme.Jukebox.SelectG.oneItemOnly := true;
  GreenSelect := AddSelectSlide(Theme.Jukebox.SelectG, Green, IGreen);

  Theme.Jukebox.SelectB.showArrows := false;
  Theme.Jukebox.SelectB.oneItemOnly := true;
  BlueSelect := AddSelectSlide(Theme.Jukebox.SelectB, Blue, IBlue);

  TexR := AddStatic(Theme.Jukebox.TexR);
  TexG := AddStatic(Theme.Jukebox.TexG);
  TexB := AddStatic(Theme.Jukebox.TexB);

  PointerR := AddStatic(Theme.Jukebox.PointerR);
  PointerG := AddStatic(Theme.Jukebox.PointerG);
  PointerB := AddStatic(Theme.Jukebox.PointerB);

  RSalt := Statics[TexR].Texture.W/255;
  GSalt := Statics[TexR].Texture.W/255;
  BSalt := Statics[TexR].Texture.W/255;

  StartR := Statics[PointerR].Texture.X;
  StartG := Statics[PointerG].Texture.X;
  StartB := Statics[PointerB].Texture.X;

  Statics[TexR].Texture.ColR := 1;
  Statics[TexR].Texture.ColG := 0;
  Statics[TexR].Texture.ColB := 0;

  Statics[TexG].Texture.ColR := 0;
  Statics[TexG].Texture.ColG := 1;
  Statics[TexG].Texture.ColB := 0;

  Statics[TexB].Texture.ColR := 0;
  Statics[TexB].Texture.ColG := 0;
  Statics[TexB].Texture.ColB := 1;

  OptionDefault := AddButton(Theme.Jukebox.SongOptionsDefault);
  OptionSave := AddButton(Theme.Jukebox.SongOptionsSave);
  OptionClose := AddButton(Theme.Jukebox.SongOptionsClose);

  Button[OptionClose].Selectable:= false;

  TextSaved := AddText(Theme.Jukebox.SongOptionsTextSaved);

  Interaction := 0;
end;

procedure TScreenJukeboxOptions.SetVisible(Value: boolean);
begin
  //If change from invisible to Visible then OnShow
  if (fVisible = false) and (Value = true) then
    OnShow;

  ScreenJukebox.SongListVisible := false;
  fVisible := Value;
end;

procedure TScreenJukeboxOptions.OnShow;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenJukeboxOptions)');

  // saved info
  Text[TextSaved].Visible := false;

  UpdatePropertyList();

  Interaction := 0;
end;

function TScreenJukeboxOptions.Draw: boolean;
var
  I: integer;
begin

  for I := 0 to High(Statics) do
    Statics[I].Draw;

  for I := 0 to High(Text) do
    Text[I].Draw;

  for I := 0 to High(SelectsS) do
    SelectsS[I].Draw;

  for I := 0 to High(Button) do
    Button[I].Draw;

  Result := true;
end;

procedure TScreenJukeboxOptions.UpdateVideoProperty();
var
  I: integer;
  IHeight: array of UTF8String;
begin

  if (AspectVideoIndex = 2) then
  begin
    SetLength(IHeight, 1);
    IHeight[0] := '---';

    I := 0;
    UpdateSelectSlideOptions(2, IHeight, I);
  end
  else
  begin
    UpdateSelectSlideOptions(2, HeightVideo, HeightVideoIndex);
  end;

end;

procedure TScreenJukeboxOptions.Update();
begin

  if (Interaction = 0) then
  begin
    ScreenJukebox.AspectCorrection := TAspectCorrection(AspectVideoIndex);

    ResetVideo();

    UpdateVideoProperty();
  end;

  if (Interaction = 4) then
    ScreenJukebox.LyricsAlpha := ILyricsAlphaVals[AlphaLyricIndex];

  if (Interaction in [5,6,7]) then
  begin
    RefreshSelectsColors();
    UpdateLyricColor();
  end;
end;

function TScreenJukeboxOptions.GetJukeboxOptionsLyricOtherColor(Line: integer): TRGB;
begin
  case Line of
    0: begin
         Result.R := JukeboxOptionsSingLineOtherColorR/255;
         Result.G := JukeboxOptionsSingLineOtherColorG/255;
         Result.B := JukeboxOptionsSingLineOtherColorB/255;
       end;
    1: begin
         Result.R := JukeboxOptionsActualLineOtherColorR/255;
         Result.G := JukeboxOptionsActualLineOtherColorG/255;
         Result.B := JukeboxOptionsActualLineOtherColorB/255;
       end;
    2: begin
         Result.R := JukeboxOptionsNextLineOtherColorR/255;
         Result.G := JukeboxOptionsNextLineOtherColorG/255;
         Result.B := JukeboxOptionsNextLineOtherColorB/255;
       end;
  end;
end;

function TScreenJukeboxOptions.GetJukeboxOptionsLyricOtherOutlineColor(Line: integer): TRGB;
begin
  case Line of
    0: begin
         Result.R := JukeboxOptionsSingLineOtherOColorR/255;
         Result.G := JukeboxOptionsSingLineOtherOColorG/255;
         Result.B := JukeboxOptionsSingLineOtherOColorB/255;
       end;
    1: begin
         Result.R := JukeboxOptionsActualLineOtherOColorR/255;
         Result.G := JukeboxOptionsActualLineOtherOColorG/255;
         Result.B := JukeboxOptionsActualLineOtherOColorB/255;
       end;
    2: begin
         Result.R := JukeboxOptionsNextLineOtherOColorR/255;
         Result.G := JukeboxOptionsNextLineOtherOColorG/255;
         Result.B := JukeboxOptionsNextLineOtherOColorB/255;
       end;
  end;
end;

procedure TScreenJukeboxOptions.UpdateLyricColor();
var
  Col: TRGB;
begin
  if (JukeboxOptionsSingLineColor = High(ISingLineColor)) then
    Col := GetJukeboxOptionsLyricOtherColor(0)
  else
    Col := GetLyricColor(JukeboxOptionsSingLineColor);

  ScreenJukebox.Lyrics.LineColor_act.R := Col.R;
  ScreenJukebox.Lyrics.LineColor_act.G := Col.G;
  ScreenJukebox.Lyrics.LineColor_act.B := Col.B;

  ScreenJukebox.LyricHelper := Col;

  if (JukeboxOptionsActualLineColor = High(IActualLineColor)) then
    Col := GetJukeboxOptionsLyricOtherColor(1)
  else
    Col := GetLyricGrayColor(JukeboxOptionsActualLineColor);

  ScreenJukebox.Lyrics.LineColor_en.R := Col.R;
  ScreenJukebox.Lyrics.LineColor_en.G := Col.G;
  ScreenJukebox.Lyrics.LineColor_en.B := Col.B;

  if (JukeboxOptionsNextLineColor = High(INextLineColor)) then
    Col := GetJukeboxOptionsLyricOtherColor(2)
  else
    Col := GetLyricGrayColor(JukeboxOptionsNextLineColor);

  ScreenJukebox.Lyrics.LineColor_dis.R := Col.R;
  ScreenJukebox.Lyrics.LineColor_dis.G := Col.G;
  ScreenJukebox.Lyrics.LineColor_dis.B := Col.B;

end;

procedure TScreenJukeboxOptions.RefreshSelectsColors;
begin

  if (SelectsS[PropertySelect].SelectedOption = 1) then
  begin

    if (SelectsS[LineSelect].SelectedOption = 0) then
      UpdateSelectSlideOptions(LineColorSelect, ISingLineOColorTranslated, Ini.CurrentJukeboxSingLineOutlineColor);

    if (SelectsS[LineSelect].SelectedOption = 1) then
      UpdateSelectSlideOptions(LineColorSelect, IActualLineOColorTranslated, Ini.CurrentJukeboxActualLineOutlineColor);

    if (SelectsS[LineSelect].SelectedOption = 2) then
      UpdateSelectSlideOptions(LineColorSelect, INextLineOColorTranslated, Ini.CurrentJukeboxNextLineOutlineColor);

  end
  else
  begin

    if (SelectsS[LineSelect].SelectedOption = 0) then
      UpdateSelectSlideOptions(LineColorSelect, ISingLineColorTranslated, JukeboxOptionsSingLineColor);

    if (SelectsS[LineSelect].SelectedOption = 1) then
      UpdateSelectSlideOptions(LineColorSelect, IActualLineColorTranslated, JukeboxOptionsActualLineColor);

    if (SelectsS[LineSelect].SelectedOption = 2) then
      UpdateSelectSlideOptions(LineColorSelect, INextLineColorTranslated, JukeboxOptionsNextLineColor);

  end;

  if (SelInteraction in [RedSelect, GreenSelect, BlueSelect]) then
  begin
    if (SelectsS[PropertySelect].SelectedOption = 1) then
      ChangeOtherOColor()
    else
      ChangeOtherColor();

    UpdateLyricColor();
  end
  else
    ChangeColor;

end;

procedure TScreenJukeboxOptions.UpdatePropertyList;
var
  IProperty: array of UTF8String;
  I: integer;
begin

  if (Ini.JukeboxFont <> 0) then
  begin
    SetLength(IProperty, Length(IPropertyTranslated));
    for I := 0 to High(IPropertyTranslated) do
      IProperty[I] := IPropertyTranslated[I];
  end
  else
  begin
    SetLength(IProperty, 1);
    IProperty[0] := '---';

    Ini.JukeboxProperty := 0;
  end;

  UpdateSelectSlideOptions(PropertySelect, IProperty, Ini.JukeboxProperty);
end;

procedure TScreenJukeboxOptions.ChangeOtherColor;
begin

  case SelectsS[LineSelect].SelectedOption of
    0: begin
         JukeboxOptionsSingLineOtherColorR := Red;
         JukeboxOptionsSingLineOtherColorG := Green;
         JukeboxOptionsSingLineOtherColorB := Blue;
       end;
    1: begin
         JukeboxOptionsActualLineOtherColorR := Red;
         JukeboxOptionsActualLineOtherColorG := Green;
         JukeboxOptionsActualLineOtherColorB := Blue;
       end;
    2: begin
         JukeboxOptionsNextLineOtherColorR := Red;
         JukeboxOptionsNextLineOtherColorG := Green;
         JukeboxOptionsNextLineOtherColorB := Blue;
       end;
  end;

end;

procedure TScreenJukeboxOptions.ChangeOtherOColor;
var
  Line: integer;
begin

  case SelectsS[LineSelect].SelectedOption of
    0: begin
         JukeboxOptionsSingLineOtherOColorR := Red;
         JukeboxOptionsSingLineOtherOColorG := Green;
         JukeboxOptionsSingLineOtherOColorB := Blue;
       end;
    1: begin
         JukeboxOptionsActualLineOtherOColorR := Red;
         JukeboxOptionsActualLineOtherOColorG := Green;
         JukeboxOptionsActualLineOtherOColorB := Blue;
       end;
    2: begin
         JukeboxOptionsNextLineOtherOColorR := Red;
         JukeboxOptionsNextLineOtherOColorG := Green;
         JukeboxOptionsNextLineOtherOColorB := Blue;
       end;
  end;
end;

procedure TScreenJukeboxOptions.ChangeColor;
var
  Col: TRGB;
begin

  if (SelectsS[PropertySelect].SelectedOption = 0) then
  begin
    if (SelectsS[LineSelect].SelectedOption = 0) then
    begin
      if (SelectsS[LineColorSelect].SelectedOption <> High(ISingLineColor)) then
        Col := GetLyricColor(SelectsS[LineColorSelect].SelectedOption)
      else
        Col := GetJukeboxOptionsLyricOtherColor(SelectsS[LineSelect].SelectedOption);
    end;

    if (SelectsS[LineSelect].SelectedOption = 1) then
    begin
      if (SelectsS[LineColorSelect].SelectedOption <> High(IActualLineColor)) then
        Col := GetLyricGrayColor(SelectsS[LineColorSelect].SelectedOption)
      else
        Col := GetJukeboxOptionsLyricOtherColor(SelectsS[LineSelect].SelectedOption);
    end;

    if (SelectsS[LineSelect].SelectedOption = 2) then
    begin
      if (SelectsS[LineColorSelect].SelectedOption <> High(INextLineColor)) then
        Col := GetLyricGrayColor(SelectsS[LineColorSelect].SelectedOption)
      else
        Col := GetJukeboxOptionsLyricOtherColor(SelectsS[LineSelect].SelectedOption);
    end;

  end
  else
  begin
    if (SelectsS[LineColorSelect].SelectedOption <> 2) then
      Col := GetLyricOutlineColor(SelectsS[LineColorSelect].SelectedOption)
    else
      Col := GetJukeboxOptionsLyricOtherOutlineColor(SelectsS[LineSelect].SelectedOption);

    case SelectsS[LineSelect].SelectedOption of
      0: Ini.CurrentJukeboxSingLineOutlineColor := SelectsS[LineColorSelect].SelectedOption;
      1: Ini.CurrentJukeboxActualLineOutlineColor := SelectsS[LineColorSelect].SelectedOption;
      2: Ini.CurrentJukeboxNextLineOutlineColor := SelectsS[LineColorSelect].SelectedOption;
    end;
  end;


  Red := Round(Col.R * 255);
  Green := Round(Col.G * 255);
  Blue := Round(Col.B * 255);

  Statics[PointerR].Texture.X := StartR + RSalt * Red;
  Statics[PointerG].Texture.X := StartG + GSalt * Green;
  Statics[PointerB].Texture.X := StartB + BSalt * Blue;

  SelectsS[RedSelect].SetSelectOpt(Red);
  SelectsS[GreenSelect].SetSelectOpt(Green);
  SelectsS[BlueSelect].SetSelectOpt(Blue);
end;

procedure TScreenJukeboxOptions.ResetVideo;
begin

  if (WidthVideoIndex <> 100) then
    ScreenJukebox.ChangeVideoWidth((100 - WidthVideoIndex) * 4);

  if (HeightVideoIndex <> 100) then
    ScreenJukebox.ChangeVideoHeight((100 - HeightVideoIndex) * 4);

  WidthVideoIndex := 100;
  HeightVideoIndex := 100;

  UpdateVideoProperty;

  SelectsS[1].SetSelectOpt(WidthVideoIndex);

  if (AspectVideoIndex <> 2) then
    SelectsS[2].SetSelectOpt(HeightVideoIndex);

end;

procedure TScreenJukeboxOptions.LoadDefaultOptions;
begin

  IsDefaultOptions := true;

  ScreenJukebox.AspectCorrection := acoCrop;
  AspectVideoIndex := 1;

  ResetVideo;

  PositionLyricIndex := 98;
  ScreenJukebox.Lyrics.UpperLineY :=  Theme.LyricBar.UpperY;
  ScreenJukebox.Lyrics.LowerLineY :=  Theme.LyricBar.LowerY;

  AlphaLyricIndex := Ini.JukeboxAlpha;
  ScreenJukebox.LyricsAlpha := StrToFloat(ILyricsAlpha[Ini.JukeboxAlpha]);

  LineLyricIndex := 0;
  PropertyLyricIndex := 0;

  ColorLyricIndex := Ini.JukeboxSingLineColor;

  SelectsS[0].SetSelectOpt(AspectVideoIndex);

  SelectsS[3].SetSelectOpt(PositionLyricIndex);
  SelectsS[4].SetSelectOpt(AlphaLyricIndex);
  SelectsS[5].SetSelectOpt(LineLyricIndex);
  SelectsS[6].SetSelectOpt(PropertyLyricIndex);
  SelectsS[7].SetSelectOpt(ColorLyricIndex);

  Text[TextSaved].Visible := false;

  JukeboxOptionsSingLineColor := Ini.JukeboxSingLineColor;
  JukeboxOptionsActualLineColor := Ini.JukeboxActualLineColor;
  JukeboxOptionsNextLineColor := Ini.JukeboxNextLineColor;

  JukeboxOptionsSingLineOtherColorR := Ini.JukeboxSingLineOtherColorR;
  JukeboxOptionsSingLineOtherColorG := Ini.JukeboxSingLineOtherColorG;
  JukeboxOptionsSingLineOtherColorB := Ini.JukeboxSingLineOtherColorB;

  JukeboxOptionsActualLineOtherColorR := Ini.JukeboxActualLineOtherColorR;
  JukeboxOptionsActualLineOtherColorG := Ini.JukeboxActualLineOtherColorG;
  JukeboxOptionsActualLineOtherColorB := Ini.JukeboxActualLineOtherColorB;

  JukeboxOptionsNextLineOtherColorR := Ini.JukeboxNextLineOtherColorR;
  JukeboxOptionsNextLineOtherColorG := Ini.JukeboxNextLineOtherColorG;
  JukeboxOptionsNextLineOtherColorB := Ini.JukeboxNextLineOtherColorB;

  JukeboxOptionsSingLineOtherOColorR := Ini.JukeboxSingLineOtherOColorR;
  JukeboxOptionsSingLineOtherOColorG := Ini.JukeboxSingLineOtherOColorG;
  JukeboxOptionsSingLineOtherOColorB := Ini.JukeboxSingLineOtherOColorB;

  JukeboxOptionsActualLineOtherOColorR := Ini.JukeboxActualLineOtherOColorR;
  JukeboxOptionsActualLineOtherOColorG := Ini.JukeboxActualLineOtherOColorG;
  JukeboxOptionsActualLineOtherOColorB := Ini.JukeboxActualLineOtherOColorB;

  JukeboxOptionsNextLineOtherOColorR := Ini.JukeboxNextLineOtherOColorR;
  JukeboxOptionsNextLineOtherOColorG := Ini.JukeboxNextLineOtherOColorG;
  JukeboxOptionsNextLineOtherOColorB := Ini.JukeboxNextLineOtherOColorB;

  Ini.CurrentJukeboxSingLineOutlineColor := Ini.JukeboxSingLineOutlineColor;
  Ini.CurrentJukeboxActualLineOutlineColor := Ini.JukeboxActualLineOutlineColor;
  Ini.CurrentJukeboxNextLineOutlineColor := Ini.JukeboxNextLineOutlineColor;

  RefreshSelectsColors;

  UpdateLyricColor;

end;

procedure TScreenJukeboxOptions.LoadSongOptions(Opts: TSongOptions);
var
  Color: TRGB;
  Alpha: real;
begin

  ScreenJukebox.AspectCorrection := TAspectCorrection(Opts.VideoRatioAspect);
  AspectVideoIndex := Opts.VideoRatioAspect;

  ResetVideo;

  WidthVideoIndex := Opts.VideoWidth;
  HeightVideoIndex := Opts.VideoHeight;

  if (WidthVideoIndex <> 100) then
    ScreenJukebox.ChangeVideoWidth((100 - WidthVideoIndex) * 4);

  if (HeightVideoIndex <> 100) then
    ScreenJukebox.ChangeVideoHeight((100 - HeightVideoIndex) * 4);

  UpdateVideoProperty;

  SelectsS[1].SetSelectOpt(WidthVideoIndex);

  if (AspectVideoIndex <> 2) then
    SelectsS[2].SetSelectOpt(HeightVideoIndex);

  PositionLyricIndex := Opts.LyricPosition;

  ScreenJukebox.Lyrics.UpperLineY :=  Theme.LyricBar.UpperY - (98 - PositionLyricIndex) * 5;
  ScreenJukebox.Lyrics.LowerLineY :=  Theme.LyricBar.LowerY - (98 - PositionLyricIndex) * 5;

  AlphaLyricIndex := Opts.LyricAlpha;
  ScreenJukebox.LyricsAlpha := StrToFloat(ILyricsAlpha[AlphaLyricIndex]);

  LineLyricIndex := 0;
  PropertyLyricIndex := 0;

  SelectsS[0].SetSelectOpt(AspectVideoIndex);

  SelectsS[3].SetSelectOpt(PositionLyricIndex);
  SelectsS[4].SetSelectOpt(AlphaLyricIndex);
  SelectsS[5].SetSelectOpt(LineLyricIndex);
  SelectsS[6].SetSelectOpt(PropertyLyricIndex);

  SelectsS[7].SetSelectOpt(ColorLyricIndex);

  Text[TextSaved].Visible := false;

  // LYRIC FILL
  JukeboxOptionsSingLineColor := GetArrayIndex(IHexSingColor, Opts.LyricSingFillColor);

  // other color
  if (JukeboxOptionsSingLineColor = -1) then
  begin
    JukeboxOptionsSingLineColor := High(IHexSingColor);

    Color := HexToRGB(Opts.LyricSingFillColor);

    JukeboxOptionsSingLineOtherColorR := Round(Color.R);
    JukeboxOptionsSingLineOtherColorG := Round(Color.G);
    JukeboxOptionsSingLineOtherColorB := Round(Color.B);
  end;

  ColorLyricIndex := JukeboxOptionsSingLineColor;

  JukeboxOptionsActualLineColor := GetArrayIndex(IHexGrayColor, Opts.LyricActualFillColor);

  // other color
  if (JukeboxOptionsActualLineColor = -1) then
  begin
    JukeboxOptionsActualLineColor := High(IHexGrayColor);

    Color := HexToRGB(Opts.LyricActualFillColor);

    JukeboxOptionsActualLineOtherColorR := Round(Color.R);
    JukeboxOptionsActualLineOtherColorG := Round(Color.G);
    JukeboxOptionsActualLineOtherColorB := Round(Color.B);
  end;

  JukeboxOptionsNextLineColor := GetArrayIndex(IHexGrayColor, Opts.LyricNextFillColor);

  // other color
  if (JukeboxOptionsNextLineColor = -1) then
  begin
    JukeboxOptionsNextLineColor := High(IHexGrayColor);

    Color := HexToRGB(Opts.LyricNextFillColor);

    JukeboxOptionsNextLineOtherColorR := Round(Color.R);
    JukeboxOptionsNextLineOtherColorG := Round(Color.G);
    JukeboxOptionsNextLineOtherColorB := Round(Color.B);
  end;

  // LYRIC OUTLINE
  Ini.CurrentJukeboxSingLineOutlineColor := GetArrayIndex(IHexOColor, Opts.LyricSingOutlineColor);

  // other color
  if (Ini.CurrentJukeboxSingLineOutlineColor = -1) then
  begin
    Ini.CurrentJukeboxSingLineOutlineColor := High(IHexOColor);

    Color := HexToRGB(Opts.LyricSingOutlineColor);

    JukeboxOptionsSingLineOtherOColorR := Round(Color.R);
    JukeboxOptionsSingLineOtherOColorG := Round(Color.G);
    JukeboxOptionsSingLineOtherOColorB := Round(Color.B);
  end;

  Ini.CurrentJukeboxActualLineOutlineColor  := GetArrayIndex(IHexOColor, Opts.LyricActualOutlineColor);

  // other color
  if (Ini.CurrentJukeboxActualLineOutlineColor = -1) then
  begin
    Ini.CurrentJukeboxActualLineOutlineColor := High(IHexOColor);

    Color := HexToRGB(Opts.LyricActualOutlineColor);

    JukeboxOptionsActualLineOtherOColorR := Round(Color.R);
    JukeboxOptionsActualLineOtherOColorG := Round(Color.G);
    JukeboxOptionsActualLineOtherOColorB := Round(Color.B);
  end;

  Ini.CurrentJukeboxNextLineOutlineColor := GetArrayIndex(IHexOColor, Opts.LyricNextOutlineColor);

  // other color
  if (Ini.CurrentJukeboxNextLineOutlineColor = -1) then
  begin
    Ini.CurrentJukeboxNextLineOutlineColor := High(IHexOColor);

    Color := HexToRGB(Opts.LyricNextOutlineColor);

    JukeboxOptionsNextLineOtherOColorR := Round(Color.R);
    JukeboxOptionsNextLineOtherOColorG := Round(Color.G);
    JukeboxOptionsNextLineOtherOColorB := Round(Color.B);
  end;

  RefreshSelectsColors;

  UpdateLyricColor;

end;

procedure TScreenJukeboxOptions.SaveSongOptions;
var
  Options: TSongOptions;
  HexColorSing, HexColorActual, HexColorNext: string;
  HexOColorSing, HexOColorActual, HexOColorNext: string;
  OutlineColor_act: TRGB;
  OutlineColor_dis: TRGB;
  OutlineColor_en:  TRGB;
begin

  if not (IsDefaultOptions) then
  begin

    HexColorSing := RGBToHex(Round(ScreenJukebox.Lyrics.LineColor_act.R * 255),
                             Round(ScreenJukebox.Lyrics.LineColor_act.G * 255),
                             Round(ScreenJukebox.Lyrics.LineColor_act.B * 255));

    HexColorActual := RGBToHex(Round(ScreenJukebox.Lyrics.LineColor_en.R * 255),
                               Round(ScreenJukebox.Lyrics.LineColor_en.G * 255),
                               Round(ScreenJukebox.Lyrics.LineColor_en.B * 255));

    HexColorNext := RGBToHex(Round(ScreenJukebox.Lyrics.LineColor_dis.R * 255),
                             Round(ScreenJukebox.Lyrics.LineColor_dis.G * 255),
                             Round(ScreenJukebox.Lyrics.LineColor_dis.B * 255));

    if (Ini.CurrentJukeboxSingLineOutlineColor <> 2) then
      OutlineColor_act := GetLyricOutlineColor(Ini.CurrentJukeboxSingLineOutlineColor)
    else
    begin
      if (Display.CurrentScreen = @ScreenJukebox) then
        OutlineColor_act := ScreenJukeboxOptions.GetJukeboxOptionsLyricOtherOutlineColor(0)
      else
        OutlineColor_act := GetJukeboxLyricOtherOutlineColor(0);
    end;

    if (Ini.CurrentJukeboxActualLineOutlineColor <> 2) then
      OutlineColor_en := GetLyricOutlineColor(Ini.CurrentJukeboxActualLineOutlineColor)
    else
    begin
      if (Display.CurrentScreen = @ScreenJukebox) then
        OutlineColor_en := ScreenJukeboxOptions.GetJukeboxOptionsLyricOtherOutlineColor(1)
      else
        OutlineColor_en := GetJukeboxLyricOtherOutlineColor(1);
    end;

    if (Ini.CurrentJukeboxNextLineOutlineColor <> 2) then
      OutlineColor_dis := GetLyricOutlineColor(Ini.CurrentJukeboxNextLineOutlineColor)
    else
    begin
      if (Display.CurrentScreen = @ScreenJukebox) then
        OutlineColor_dis := ScreenJukeboxOptions.GetJukeboxOptionsLyricOtherOutlineColor(2)
      else
        OutlineColor_dis := GetJukeboxLyricOtherOutlineColor(2);
    end;

    HexOColorSing := RGBToHex(Round(OutlineColor_act.R * 255),
                              Round(OutlineColor_act.G * 255),
                              Round(OutlineColor_act.B * 255));

    HexOColorActual := RGBToHex(Round(OutlineColor_en.R * 255),
                                Round(OutlineColor_en.G * 255),
                                Round(OutlineColor_en.B * 255));

    HexOColorNext := RGBToHex(Round(OutlineColor_dis.R * 255),
                              Round(OutlineColor_dis.G * 255),
                              Round(OutlineColor_dis.B * 255));

    Options := TSongOptions.Create(AspectVideoIndex, WidthVideoIndex, HeightVideoIndex, PositionLyricIndex, AlphaLyricIndex,
      HexColorSing, HexColorActual, HexColorNext, HexOColorSing, HexOColorActual, HexOColorNext);

  end
  else
  begin
    Options := TSongOptions.Create(1, 100, 100, 98, 100, '', '', '', '', '', '');
  end;

  DataBase.SaveSongOptions(CatSongs.Song[ScreenJukebox.CurrentSongID], Options);

  Options.Free;

  Text[TextSaved].Visible := true;
end;

end.
