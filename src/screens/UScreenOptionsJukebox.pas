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
 * $Id: $
 *}

unit UScreenOptionsJukebox;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UCommon,
  UDisplay,
  UFiles,
  UIni,
  ULyrics,
  UMenu,
  UMusic,
  UThemes,
  sdl2,
  TextGL;

type
  TScreenOptionsJukebox = class(TMenu)
    private
      Lyrics: TLyricEngine;
      Line: TLine;

      FontSelect:      integer;
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
      TexColor:  integer;

      Red:   integer;
      Green: integer;
      Blue:  integer;

      RSalt: real;
      GSalt: real;
      BSalt: real;

      StartR: real;
      StartG: real;
      StartB: real;

      SingColor: array [0..2] of TRGB;
      SingOutlineColor: array [0..2] of TRGB;

      NextColor: array [0..2] of TRGB;
      NextOutlineColor: array [0..2] of TRGB;

      ActualColor: array [0..2] of TRGB;
      ActualOutlineColor: array [0..2] of TRGB;

    public
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      function Draw: boolean; override;
      procedure InteractInc; override;
      procedure InteractDec; override;

      procedure RefreshSelectsColors;
      procedure ChangeOtherColor;
      procedure ChangeOtherOColor;
      procedure ChangeColor;

      procedure UpdatePropertyList;
      procedure LyricSample;
  end;

const
  ID='ID_081';   //for help system

implementation

uses
  UGraphic,
  UHelp,
  ULog,
  UUnicodeUtils,
  SysUtils;

function TScreenOptionsJukebox.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  SDL_ModState:  word;
  Salt_Mod:      integer;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check normal keys
    case UCS4UpperCase(CharCode) of
      Ord('Q'):
        begin
          Result := false;
          Exit;
        end;
    end;

    SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
              + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);

    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          Ini.Save;
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenOptions);
        end;
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;
      SDLK_RETURN:
        begin
          if SelInteraction = 9 then
          begin
            Ini.Save;
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(@ScreenOptions);
          end;
        end;
      SDLK_DOWN:
        InteractNext;
      SDLK_UP :
        InteractPrev;
      SDLK_RIGHT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 8) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);

            if (SelInteraction >= 6) and (SelInteraction <= 8) then
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

            end;

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

            Statics[TexColor].Texture.ColR := Red/255;
            Statics[Texcolor].Texture.ColG := Green/255;
            Statics[TexColor].Texture.ColB := Blue/255;

            InteractInc;
          end;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 8) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);

            if (SelInteraction >= 6) and (SelInteraction <= 8) then
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
            end;

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

            Statics[TexColor].Texture.ColR := Red/255;
            Statics[TexColor].Texture.ColG := Green/255;
            Statics[TexColor].Texture.ColB := Blue/255;

            InteractDec;
          end;
        end;
    end;
  end;
end;

procedure TScreenOptionsJukebox.UpdatePropertyList;
var
  IProperty: array of UTF8String;
  I: integer;
begin

  if (SelectsS[FontSelect].SelectedOption <> 0) then
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

  UpdateSelectSlideOptions(Theme.OptionsJukebox.SelectProperty, PropertySelect, IProperty, Ini.JukeboxProperty);
end;

procedure TScreenOptionsJukebox.InteractInc;
begin
  inherited InteractInc;

  if (SelInteraction = 0) then
    UpdatePropertyList;

  RefreshSelectsColors;
end;

procedure TScreenOptionsJukebox.InteractDec;
begin
  inherited InteractDec;

  if (SelInteraction = 0) then
    UpdatePropertyList;

  RefreshSelectsColors;
end;

procedure TScreenOptionsJukebox.RefreshSelectsColors;
begin

  if (SelectsS[PropertySelect].SelectedOption = 1) then
  begin

    if (SelectsS[LineSelect].SelectedOption = 0) then
      UpdateSelectSlideOptions(Theme.OptionsJukebox.SelectColor, LineColorSelect, ISingLineOColorTranslated, Ini.JukeboxSingLineOutlineColor);

    if (SelectsS[LineSelect].SelectedOption = 1) then
      UpdateSelectSlideOptions(Theme.OptionsJukebox.SelectColor, LineColorSelect, IActualLineOColorTranslated, Ini.JukeboxActualLineOutlineColor);

    if (SelectsS[LineSelect].SelectedOption = 2) then
      UpdateSelectSlideOptions(Theme.OptionsJukebox.SelectColor, LineColorSelect, INextLineOColorTranslated, Ini.JukeboxNextLineOutlineColor);

  end
  else
  begin

    if (SelectsS[LineSelect].SelectedOption = 0) then
      UpdateSelectSlideOptions(Theme.OptionsJukebox.SelectColor, LineColorSelect, ISingLineColorTranslated, Ini.JukeboxSingLineColor);

    if (SelectsS[LineSelect].SelectedOption = 1) then
      UpdateSelectSlideOptions(Theme.OptionsJukebox.SelectColor, LineColorSelect, IActualLineColorTranslated, Ini.JukeboxActualLineColor);

    if (SelectsS[LineSelect].SelectedOption = 2) then
      UpdateSelectSlideOptions(Theme.OptionsJukebox.SelectColor, LineColorSelect, INextLineColorTranslated, Ini.JukeboxNextLineColor);

  end;

  if (SelInteraction >= 6) and (SelInteraction <= 8) then
  begin
    if (SelectsS[PropertySelect].SelectedOption = 1) then
      ChangeOtherOColor()
    else
      ChangeOtherColor();
  end;

  ChangeColor;

end;

procedure TScreenOptionsJukebox.ChangeOtherColor;
begin

  case SelectsS[LineSelect].SelectedOption of
    0: begin
         Ini.JukeboxSingLineOtherColorR := Red;
         Ini.JukeboxSingLineOtherColorG := Green;
         Ini.JukeboxSingLineOtherColorB := Blue;
       end;
    1: begin
         Ini.JukeboxActualLineOtherColorR := Red;
         Ini.JukeboxActualLineOtherColorG := Green;
         Ini.JukeboxActualLineOtherColorB := Blue;
       end;
    2: begin
         Ini.JukeboxNextLineOtherColorR := Red;
         Ini.JukeboxNextLineOtherColorG := Green;
         Ini.JukeboxNextLineOtherColorB := Blue;
       end;
  end;

end;

procedure TScreenOptionsJukebox.ChangeOtherOColor;
var
  Line: integer;
begin

  case SelectsS[LineSelect].SelectedOption of
    0: begin
         Ini.JukeboxSingLineOtherOColorR := Red;
         Ini.JukeboxSingLineOtherOColorG := Green;
         Ini.JukeboxSingLineOtherOColorB := Blue;
       end;
    1: begin
         Ini.JukeboxActualLineOtherOColorR := Red;
         Ini.JukeboxActualLineOtherOColorG := Green;
         Ini.JukeboxActualLineOtherOColorB := Blue;
       end;
    2: begin
         Ini.JukeboxNextLineOtherOColorR := Red;
         Ini.JukeboxNextLineOtherOColorG := Green;
         Ini.JukeboxNextLineOtherOColorB := Blue;
       end;
  end;
end;

procedure TScreenOptionsJukebox.ChangeColor;
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
        Col := GetJukeboxLyricOtherColor(SelectsS[LineSelect].SelectedOption);
    end;

    if (SelectsS[LineSelect].SelectedOption = 1) then
    begin
      if (SelectsS[LineColorSelect].SelectedOption <> High(IActualLineColor)) then
        Col := GetLyricGrayColor(SelectsS[LineColorSelect].SelectedOption)
      else
        Col := GetJukeboxLyricOtherColor(SelectsS[LineSelect].SelectedOption);
    end;

    if (SelectsS[LineSelect].SelectedOption = 2) then
    begin
      if (SelectsS[LineColorSelect].SelectedOption <> High(INextLineColor)) then
        Col := GetLyricGrayColor(SelectsS[LineColorSelect].SelectedOption)
      else
        Col := GetJukeboxLyricOtherColor(SelectsS[LineSelect].SelectedOption);
    end;

  end
  else
  begin
    if (SelectsS[LineColorSelect].SelectedOption <> 2) then
      Col := GetLyricOutlineColor(SelectsS[LineColorSelect].SelectedOption)
    else
      Col := GetJukeboxLyricOtherOutlineColor(SelectsS[LineSelect].SelectedOption)
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

  Statics[TexColor].Texture.ColR := Red/255;
  Statics[TexColor].Texture.ColG := Green/255;
  Statics[TexColor].Texture.ColB := Blue/255;

  Ini.CurrentJukeboxSingLineOutlineColor := Ini.JukeboxSingLineOutlineColor;
  Ini.CurrentJukeboxActualLineOutlineColor := Ini.JukeboxActualLineOutlineColor;
  Ini.CurrentJukeboxNextLineOutlineColor := Ini.JukeboxNextLineOutlineColor;
end;

constructor TScreenOptionsJukebox.Create;
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsJukebox);

  Theme.OptionsJukebox.SelectLyricsFont.showArrows := true;
  Theme.OptionsJukebox.SelectLyricsFont.oneItemOnly := true;
  FontSelect := AddSelectSlide(Theme.OptionsJukebox.SelectLyricsFont, Ini.JukeboxFont, ILyricsFontTranslated);

  Theme.OptionsJukebox.SelectLyricsEffect.showArrows := true;
  Theme.OptionsJukebox.SelectLyricsEffect.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsJukebox.SelectLyricsEffect, Ini.JukeboxEffect, ILyricsEffectTranslated);

  Theme.OptionsJukebox.SelectLyricsAlpha.showArrows := true;
  Theme.OptionsJukebox.SelectLyricsAlpha.oneItemOnly := true;
  AddSelectSlide(Theme.OptionsJukebox.SelectLyricsAlpha, Ini.JukeboxAlpha, ILyricsAlpha);

  Theme.OptionsJukebox.SelectLine.showArrows := true;
  Theme.OptionsJukebox.SelectLine.oneItemOnly := true;
  LineSelect := AddSelectSlide(Theme.OptionsJukebox.SelectLine, Ini.JukeboxLine, ILineTranslated);

  Theme.OptionsJukebox.SelectProperty.showArrows := true;
  Theme.OptionsJukebox.SelectProperty.oneItemOnly := true;
  PropertySelect := AddSelectSlide(Theme.OptionsJukebox.SelectProperty, Ini.JukeboxProperty, IPropertyTranslated);

  Theme.OptionsJukebox.SelectColor.showArrows := true;
  Theme.OptionsJukebox.SelectColor.oneItemOnly := true;
  LineColorSelect := AddSelectSlide(Theme.OptionsJukebox.SelectColor, Ini.JukeboxSingLineColor, ISingLineColorTranslated);

  Theme.OptionsJukebox.SelectR.showArrows := false;
  Theme.OptionsJukebox.SelectR.oneItemOnly := true;
  RedSelect := AddSelectSlide(Theme.OptionsJukebox.SelectR, Red, IRed);

  Theme.OptionsJukebox.SelectG.showArrows := false;
  Theme.OptionsJukebox.SelectG.oneItemOnly := true;
  GreenSelect := AddSelectSlide(Theme.OptionsJukebox.SelectG, Green, IGreen);

  Theme.OptionsJukebox.SelectB.showArrows := false;
  Theme.OptionsJukebox.SelectB.oneItemOnly := true;
  BlueSelect := AddSelectSlide(Theme.OptionsJukebox.SelectB, Blue, IBlue);

  TexR := AddStatic(Theme.OptionsJukebox.TexR);
  TexG := AddStatic(Theme.OptionsJukebox.TexG);
  TexB := AddStatic(Theme.OptionsJukebox.TexB);
  TexColor := AddStatic(Theme.OptionsJukebox.TexColor);

  PointerR := AddStatic(Theme.OptionsJukebox.PointerR);
  PointerG := AddStatic(Theme.OptionsJukebox.PointerG);
  PointerB := AddStatic(Theme.OptionsJukebox.PointerB);

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

  AddButton(Theme.OptionsJukebox.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(20, 5, Theme.Options.Description[OPTIONS_DESC_INDEX_BACK]);

  // lyric sample
  Lyrics := TLyricEngine.Create(
      Theme.OptionsJukebox.UpperX, Theme.OptionsJukebox.UpperY, Theme.OptionsJukebox.UpperW, Theme.OptionsJukebox.UpperH,
      Theme.OptionsJukebox.LowerX, Theme.OptionsJukebox.LowerY, Theme.OptionsJukebox.LowerW, Theme.OptionsJukebox.LowerH);

  //Line.Lyric := 'Lorem ipsum dolor sit amet';
  // 1st line
  SetLength(Line.Note, 6);
  Line.Note[0].Text := 'Lor';
  Line.Note[1].Text := 'em';
  Line.Note[2].Text := ' ipsum';
  Line.Note[3].Text := ' dolor';
  Line.Note[4].Text := ' sit';
  Line.Note[5].Text := ' amet';

  Line.Note[0].StartBeat := 0;
  Line.Note[1].StartBeat := 10;
  Line.Note[2].StartBeat := 20;
  Line.Note[3].StartBeat := 30;
  Line.Note[4].StartBeat := 40;
  Line.Note[5].StartBeat := 50;

  Line.Note[0].Duration := 10;
  Line.Note[1].Duration := 10;
  Line.Note[2].Duration := 10;
  Line.Note[3].Duration := 10;
  Line.Note[4].Duration := 10;
  Line.Note[5].Duration := 10;

  Line.TotalNotes := 6;
  Line.EndBeat := 60;
  Line.StartBeat := 0;
  Line.LastLine := true;
  Lyrics.AddLine(@Line);

  // 2nd line
  //consectetur adipiscing elit
  SetLength(Line.Note, 3);

  Line.Note[0].Text := 'consectetur';
  Line.Note[1].Text := ' adipiscing';
  Line.Note[2].Text := ' elit';

  Line.Note[0].StartBeat := 60;
  Line.Note[1].StartBeat := 70;
  Line.Note[2].StartBeat := 80;

  Line.Note[0].Duration := 10;
  Line.Note[1].Duration := 10;
  Line.Note[2].Duration := 10;

  Line.LastLine := true;

  Lyrics.AddLine(@Line);


  Lyrics.AddLine(@Line);

end;

procedure TScreenOptionsJukebox.LyricSample;
var
  Col: TRGB;
begin

  case Ini.JukeboxFont of
    0: // normal fonts
    begin
      Lyrics.FontStyle := ftNormal;
    end;
    1, 2: // outline fonts
    begin
      if (Ini.JukeboxFont = 1) then
        Lyrics.FontStyle := ftOutline1
      else
        Lyrics.FontStyle := ftOutline2;
    end;
  end; // case

  if (Ini.JukeboxSingLineColor = High(ISingLineColor)) then
    Col := GetJukeboxLyricOtherColor(0)
  else
    Col := GetLyricColor(Ini.JukeboxSingLineColor);

  Lyrics.LineColor_act.R := Col.R;
  Lyrics.LineColor_act.G := Col.G;
  Lyrics.LineColor_act.B := Col.B;

  if (Ini.JukeboxActualLineColor = High(IActualLineColor)) then
    Col := GetJukeboxLyricOtherColor(1)
  else
    Col := GetLyricGrayColor(Ini.JukeboxActualLineColor);

  Lyrics.LineColor_en.R := Col.R;
  Lyrics.LineColor_en.G := Col.G;
  Lyrics.LineColor_en.B := Col.B;

  if (Ini.JukeboxNextLineColor = High(INextLineColor)) then
    Col := GetJukeboxLyricOtherColor(2)
  else
    Col := GetLyricGrayColor(Ini.JukeboxNextLineColor);

  Lyrics.LineColor_dis.R := Col.R;
  Lyrics.LineColor_dis.G := Col.G;
  Lyrics.LineColor_dis.B := Col.B;

  Lyrics.Draw(LyricsState.MidBeat);
end;

procedure TScreenOptionsJukebox.OnShow;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenOptionsJukebox)');

  Interaction := 0;

  UpdatePropertyList();
  ChangeColor();

  LyricsState.StartTime := 0;
  LyricsState.UpdateBeats;
end;

function TScreenOptionsJukebox.Draw: boolean;
begin
  Result := inherited Draw;

  LyricSample();
end;

end.
