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
  UMenuWidget,
  UMenuSelectSlide,
  UMenuStatic,
  UMenuText,
  UMusic,
  UPath,
  UNote,
  UThemes,
  sdl2,
  TextGL;

type

  TColor = (colRed, colGreen, ColBlue);

  TLyricsColorPicker = class(IMenuWidget)
    private
      SelectR, SelectG, SelectB: TSelectSlide; // Owned by the menu, only used for drawing and changing position
      StaticR, StaticG, StaticB: TStatic;
      PointerR, PointerG, PointerB: TStatic;
      Sample: TStatic;
      RGBSalt: real;
      Red, Green, Blue: integer;

      function CreateStatic(X, Y, W, H, Z, ColR, ColG, ColB: real; const TexName: IPath): TStatic;

    public
      constructor Create(PosX, PosY: real; SelectR, SelectG, SelectB: TSelectSlide; Red, Green, Blue: integer);
      destructor Destroy; override;

      procedure SetX(PosX: real); override;
      function GetX(): real; override;
      procedure SetY(PosY: real); override;
      function GetY(): real; override;
      function GetW(): real; override;
      function GetH(): real; override;
      procedure Draw; override;
      procedure SetColor(Color: TColor; Value: integer);

  end;

  TScreenOptionsJukebox = class(TOptionsMenu)
    private
      Lyrics: TLyricEngine;
      Line: TLine;
      ColorPicker: TLyricsColorPicker;

      FontSelect:      integer;
      StyleSelect:     integer;
      LineSelect:      integer;
      PropertySelect:  integer;
      LineColorSelect: integer;
      RedSelect:       integer;
      GreenSelect:     integer;
      BlueSelect:      integer;

      Red:   integer;
      Green: integer;
      Blue:  integer;


      procedure AddColorPicker;
      function AddColorPickerSelectSlide(const Text: UTF8String; var Data: integer; const Values: array of UTF8String): integer;

    public
      constructor Create; override;
      destructor Destroy; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      function DrawFG: boolean; override;
      procedure InteractInc; override;
      procedure InteractDec; override;

      procedure RefreshSelectsColors;
      procedure ChangeOtherColor;
      procedure ChangeOtherOColor;
      procedure ChangeColor;

      procedure UpdatePropertyList;
      procedure LyricSample;

    protected
      procedure LoadWidgets; override;
  end;

const
  ID='ID_081';   //for help system

implementation

uses
  UGraphic,
  UHelp,
  ULanguage,
  ULog,
  UMenuButton,
  USkins,
  UTexture,
  UUnicodeUtils,
  SysUtils;

constructor TLyricsColorPicker.Create(PosX, PosY: real; SelectR, SelectG, SelectB: TSelectSlide; Red, Green, Blue: integer);
begin
  self.SelectR := SelectR;
  self.SelectG := SelectG;
  self.SelectB := SelectB;

  StaticR := CreateStatic(360, SelectR.Y + 3, 340, 10, 1, 1, 0, 0, Skin.GetTextureFileName('Picker'));
  StaticG := CreateStatic(360, SelectG.Y + 3, 340, 10, 1, 0, 1, 0, Skin.GetTextureFileName('Picker'));
  StaticB := CreateStatic(360, SelectB.Y + 3, 340, 10, 1, 0, 0, 1, Skin.GetTextureFileName('Picker'));
  PointerR := CreateStatic(StaticR.Texture.X, StaticR.Texture.Y - 1, 1, 12, 1, 0, 0, 0, Skin.GetTextureFileName('Picker'));
  PointerG := CreateStatic(StaticG.Texture.X, StaticG.Texture.Y - 1, 1, 12, 1, 0, 0, 0, Skin.GetTextureFileName('Picker'));
  PointerB := CreateStatic(StaticB.Texture.X, StaticB.Texture.Y - 1, 1, 12, 1, 0, 0, 0, Skin.GetTextureFileName('Picker'));


  Sample := CreateStatic(PosX, PosY, 145, 55, 1, 1, 1, 1, Skin.GetTextureFileName('Picker'));

  RGBSalt := StaticR.Texture.W/255;
  X := PosX;
  Y := PosY;
  PosYInit := PosY;
  SetColor(colRed, Red);
  SetColor(colGreen, Green);
  SetColor(colBlue, Blue);

end;

destructor TLyricsColorPicker.Destroy;
begin
  Sample.Free;
  StaticR.Free;
  StaticG.Free;
  StaticB.Free;
  PointerR.Free;
  PointerG.Free;
  PointerB.Free;
  // Select slides are owned by the menu
  inherited;
end;

procedure TLyricsColorPicker.SetX(PosX: real);
var
  SelectX: real;
  StaticX: real;
begin
  Sample.Texture.X := PosX;
  SelectX := PosX + 150;
  SelectR.X := SelectX;
  SelectG.X := SelectX;
  SelectB.X := SelectX;
  StaticX := PosX + 290;
  StaticR.Texture.X := StaticX;
  StaticG.Texture.X := StaticX;
  StaticB.Texture.X := StaticX;
  PointerR.Texture.X := StaticX;
  PointerG.Texture.X := StaticX;
  PointerB.Texture.X := StaticX;
end;

function TLyricsColorPicker.GetX(): real;
begin
  Result := Sample.Texture.X;
end;

procedure TLyricsColorPicker.SetY(PosY: real);
begin
  Sample.Texture.Y := PosY;
  SelectR.Y := PosY;
  SelectG.Y := SelectR.Y + SelectR.H + Theme.OptionsSub.WidgetVSpacing;
  SelectB.Y := SelectG.Y + SelectG.H + Theme.OptionsSub.WidgetVSpacing;
  StaticR.Texture.Y := SelectR.Y + 3;
  StaticG.Texture.Y := SelectG.Y + 3;
  StaticB.Texture.Y := SelectB.Y + 3;
  PointerR.Texture.Y := StaticR.Texture.Y - 1;
  PointerG.Texture.Y := StaticG.Texture.Y - 1;
  PointerB.Texture.Y := StaticB.Texture.Y - 1;

end;

function TLyricsColorPicker.GetY(): real;
begin
  Result := SelectR.Y;
end;

function TLyricsColorPicker.GetW(): real;
begin
  Result := SelectR.X + SelectR.W - Sample.Texture.X;
end;

function TLyricsColorPicker.GetH(): real;
begin
  Result := Sample.Texture.H;
end;

function TLyricsColorPicker.CreateStatic(X, Y, W, H, Z, ColR, ColG, ColB: real; const TexName: IPath): TStatic;
begin
  Result := TStatic.Create(Texture.GetTexture(TexName, TEXTURE_TYPE_TRANSPARENT, 0));

  // configures static
  Result.Texture.X := X;
  Result.Texture.Y := Y;
  Result.Texture.H := H;
  Result.Texture.W := W;
  Result.Texture.Z := Z;
  Result.Texture.ColR := ColR;
  Result.Texture.ColG := ColG;
  Result.Texture.ColB := ColB;
  Result.Texture.TexX1 := 0.0;
  Result.Texture.TexY1 := 0.0;
  Result.Texture.TexX2 := 1.0;
  Result.Texture.TexY2 := 1.0;
  Result.Texture.Alpha := 1.0;
  Result.Visible := true;
end;

procedure TLyricsColorPicker.Draw;
begin
  StaticR.Draw;
  StaticG.Draw;
  StaticB.Draw;
  PointerR.Draw;
  PointerG.Draw;
  PointerB.Draw;
  SelectR.Draw;
  SelectG.Draw;
  SelectB.Draw;
  Sample.Draw;
end;

procedure TLyricsColorPicker.SetColor(Color: TColor; Value: integer);
var
  Stat: TStatic;
  Point: TStatic;
  Col: ^integer;
  TexCol: ^real;
begin
  case Color of
    colRed:
      begin
        Stat := StaticR;
        Point := PointerR;
        Col := @Red;
        TexCol := @Sample.Texture.ColR;
      end;
    colGreen:
      begin
        Stat := StaticG;
        Point := PointerG;
        Col := @Green;
        TexCol := @Sample.Texture.ColG;
      end;
    colBlue:
      begin
        Stat := StaticB;
        Point := PointerB;
        Col := @Blue;
        TexCol := @Sample.Texture.ColB;
      end;
  end;
  Point.Texture.X := Stat.Texture.X + RGBSalt * Value;
  Col^ := Value;
  TexCol^ := Value / 255;
end;

function TScreenOptionsJukebox.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  SDL_ModState:  word;
  Salt_Mod:      integer;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check normal keys
    case PressedKey of
      SDLK_Q:
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
          if SelInteraction = 10 then
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
          if (SelInteraction >= 0) and (SelInteraction <= 9) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);

            if (SelInteraction >= 7) and (SelInteraction <= 9) then
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
              ColorPicker.SetColor(colRed, Red);
            end;

            if (SelInteraction = GreenSelect) and (Green < 255) then
            begin
              Green := Green + Salt_Mod;
              SelectsS[SelInteraction].SelectOptInt := Green;
              ColorPicker.SetColor(colGreen, Green);
            end;

            if (SelInteraction = BlueSelect) and (Blue < 255) then
            begin
              Blue := Blue + Salt_Mod;
              SelectsS[SelInteraction].SelectOptInt := Blue;
              ColorPicker.SetColor(colBlue, Blue);
            end;
            InteractInc;
          end;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 9) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);

            if (SelInteraction >= 7) and (SelInteraction <= 9) then
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

            if (SelInteraction = RedSelect) and (Red < 255) then
            begin
              Red := Red - Salt_Mod;
              SelectsS[SelInteraction].SelectOptInt := Red;
              ColorPicker.SetColor(colRed, Red);
            end;

            if (SelInteraction = GreenSelect) and (Green < 255) then
            begin
              Green := Green - Salt_Mod;
              SelectsS[SelInteraction].SelectOptInt := Green;
              ColorPicker.SetColor(colGreen, Green);
            end;

            if (SelInteraction = BlueSelect) and (Blue < 255) then
            begin
              Blue := Blue - Salt_Mod;
              SelectsS[SelInteraction].SelectOptInt := Blue;
              ColorPicker.SetColor(colBlue, Blue);
            end;
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

  if (SelectsS[StyleSelect].SelectedOption = ftOutline) then
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

procedure TScreenOptionsJukebox.InteractInc;
begin
  inherited InteractInc;

  if (SelInteraction = 1) then
    UpdatePropertyList;

  RefreshSelectsColors;
end;

procedure TScreenOptionsJukebox.InteractDec;
begin
  inherited InteractDec;

  if (SelInteraction = 1) then
    UpdatePropertyList;

  RefreshSelectsColors;
end;

procedure TScreenOptionsJukebox.RefreshSelectsColors;
begin

  if (SelectsS[PropertySelect].SelectedOption = 1) then
  begin

    if (SelectsS[LineSelect].SelectedOption = 0) then
      UpdateSelectSlideOptions(LineColorSelect, ISingLineOColorTranslated, Ini.JukeboxSingLineOutlineColor);

    if (SelectsS[LineSelect].SelectedOption = 1) then
      UpdateSelectSlideOptions(LineColorSelect, IActualLineOColorTranslated, Ini.JukeboxActualLineOutlineColor);

    if (SelectsS[LineSelect].SelectedOption = 2) then
      UpdateSelectSlideOptions(LineColorSelect, INextLineOColorTranslated, Ini.JukeboxNextLineOutlineColor);

  end
  else
  begin

    if (SelectsS[LineSelect].SelectedOption = 0) then
      UpdateSelectSlideOptions(LineColorSelect, ISingLineColorTranslated, Ini.JukeboxSingLineColor);

    if (SelectsS[LineSelect].SelectedOption = 1) then
      UpdateSelectSlideOptions(LineColorSelect, IActualLineColorTranslated, Ini.JukeboxActualLineColor);

    if (SelectsS[LineSelect].SelectedOption = 2) then
      UpdateSelectSlideOptions(LineColorSelect, INextLineColorTranslated, Ini.JukeboxNextLineColor);

  end;

  if (SelInteraction >= 7) and (SelInteraction <= 9) then
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

  SelectsS[RedSelect].SetSelectOpt(Red);
  SelectsS[GreenSelect].SetSelectOpt(Green);
  SelectsS[BlueSelect].SetSelectOpt(Blue);
  ColorPicker.SetColor(colRed, Red);
  ColorPicker.SetColor(colGreen, Green);
  ColorPicker.SetColor(colBlue, Blue);


  Ini.CurrentJukeboxSingLineOutlineColor := Ini.JukeboxSingLineOutlineColor;
  Ini.CurrentJukeboxActualLineOutlineColor := Ini.JukeboxActualLineOutlineColor;
  Ini.CurrentJukeboxNextLineOutlineColor := Ini.JukeboxNextLineOutlineColor;
end;

constructor TScreenOptionsJukebox.Create;
var
  ButtonExit: TButton;
begin
  inherited Create;
  Description := Language.Translate('SING_OPTIONS_JUKEBOX_DESC');
  WhereAmI := Language.Translate('SING_OPTIONS_JUKEBOX_WHEREAMI');
  Load;

  // lyric sample
  ButtonExit := Button[High(Button)];
  Lyrics := TLyricEngine.Create(
      ButtonExit.X + ButtonExit.W + 20, ButtonExit.Y - 5 , 400, 40,
      ButtonExit.X + ButtonExit.W + 20, ButtonExit.Y + 25, 400, 40);

  //Line.Lyric := 'Lorem ipsum dolor sit amet';
  // 1st line
  SetLength(Line.Notes, 6);
  Line.Notes[0].Text := 'Lor';
  Line.Notes[1].Text := 'em';
  Line.Notes[2].Text := ' ipsum';
  Line.Notes[3].Text := ' dolor';
  Line.Notes[4].Text := ' sit';
  Line.Notes[5].Text := ' amet';

  Line.Notes[0].StartBeat := 0;
  Line.Notes[1].StartBeat := 10;
  Line.Notes[2].StartBeat := 20;
  Line.Notes[3].StartBeat := 30;
  Line.Notes[4].StartBeat := 40;
  Line.Notes[5].StartBeat := 50;

  Line.Notes[0].Duration := 10;
  Line.Notes[1].Duration := 10;
  Line.Notes[2].Duration := 10;
  Line.Notes[3].Duration := 10;
  Line.Notes[4].Duration := 10;
  Line.Notes[5].Duration := 10;

  Line.ScoreValue := 6;
  Line.EndBeat := 60;
  Line.StartBeat := 0;
  Line.LastLine := true;
  Lyrics.AddLine(@Line);

  // 2nd line
  //consectetur adipiscing elit
  SetLength(Line.Notes, 3);

  Line.Notes[0].Text := 'consectetur';
  Line.Notes[1].Text := ' adipiscing';
  Line.Notes[2].Text := ' elit';

  Line.Notes[0].StartBeat := 60;
  Line.Notes[1].StartBeat := 70;
  Line.Notes[2].StartBeat := 80;

  Line.Notes[0].Duration := 10;
  Line.Notes[1].Duration := 10;
  Line.Notes[2].Duration := 10;

  Line.LastLine := true;

  Lyrics.AddLine(@Line);
  Lyrics.AddLine(@Line);
end;

destructor TScreenOptionsJukebox.Destroy;
begin
  ColorPicker.Free;
  inherited;
end;

procedure TScreenOptionsJukebox.LyricSample;
var
  Col: TRGB;
begin
  Lyrics.FontFamily := Ini.JukeboxFont;
  Lyrics.FontStyle  := Ini.JukeboxStyle;

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

function TScreenOptionsJukebox.DrawFG: boolean;
begin
  Result := inherited;

  LyricSample();
end;

function TScreenOptionsJukebox.AddColorPickerSelectSlide(const Text: UTF8String; var Data: integer; const Values: array of UTF8String): integer;
var
  SelectS: TSelectSlide;
  ThemeSelect: TThemeSelectSlide;
begin
  ThemeSelect := Theme.OptionsSub.SelectS;
  with ThemeSelect do
  begin
    X := 220;
    Y := YNextWidget;
    W := 100;
    H := 15;
    TextSize := 16;
  end;
  ThemeSelect.Text := Language.Translate(Text);
  Result := AddSelectSlide(ThemeSelect, Data, Values);
  SelectS := SelectsS[High(SelectsS)];
  SelectS.showArrows := False;
  SelectS.Scrollable := True;
  Inc(YNextWidget, Round(SelectS.H) + Theme.OptionsSub.WidgetVSpacing);
end;

procedure TScreenOptionsJukebox.AddColorPicker;
var
  NumWidgets: integer;
begin
  RedSelect := AddColorPickerSelectSlide('JUKEBOX_SONGOPTIONS_LYRIC_RGB_RED', Red, IRed);
  GreenSelect := AddColorPickerSelectSlide('JUKEBOX_SONGOPTIONS_LYRIC_RGB_GREEN', Green, IGreen);
  BlueSelect := AddColorPickerSelectSlide('JUKEBOX_SONGOPTIONS_LYRIC_RGB_BLUE', Blue, IBlue);
  ColorPicker := TLyricsColorPicker.Create(70, SelectsS[RedSelect].Y, SelectsS[RedSelect], SelectsS[GreenSelect], SelectsS[BlueSelect], Red, Green, Blue);
  NumWidgets := Length(Widgets);
  SetLength(Widgets, NumWidgets + 1);
  Widgets[NumWidgets] := ColorPicker;
  YNextWidget := Round(ColorPicker.Y + ColorPicker.H + Theme.OptionsSub.WidgetVSpacing);
end;

procedure TScreenOptionsJukebox.LoadWidgets;
begin
  // when editing this, also search for SelInteraction
  FontSelect := AddSelectSlide('SING_OPTIONS_LYRICS_FONT', Ini.JukeboxFont, FontFamilyNames);
  StyleSelect := AddSelectSlide('SING_OPTIONS_LYRICS_STYLE', Ini.JukeboxStyle, ILyricsStyleTranslated);
  AddSelectSlide('SING_OPTIONS_LYRICS_EFFECT', Ini.JukeboxEffect, ILyricsEffectTranslated);
  AddSelectSlide('JUKEBOX_SONGOPTIONS_LYRIC_ALPHA', Ini.JukeboxAlpha, ILyricsAlpha);
  LineSelect := AddSelectSlide('JUKEBOX_SONGOPTIONS_LYRIC_LINE', Ini.JukeboxLine, ILineTranslated);
  PropertySelect := AddSelectSlide('JUKEBOX_SONGOPTIONS_LYRIC_PROPERTY', Ini.JukeboxProperty, IPropertyTranslated);
  LineColorSelect := AddSelectSlide('JUKEBOX_SONGOPTIONS_LYRIC_COLOR', Ini.JukeboxSingLineColor, ISingLineColorTranslated);
  AddColorPicker;
end;

end.
