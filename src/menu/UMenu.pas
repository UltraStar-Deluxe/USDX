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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/menu/UMenu.pas $
 * $Id: UMenu.pas 3103 2014-11-22 23:21:19Z k-m_schindler $
 *}

unit UMenu;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  Math,
  gl,
  SDL,
  UPath,
  UMenuBackground,
  UMenuButton,
  UMenuButtonCollection,
  UMenuInteract,
  UMenuSelectSlide,
  UMenuStatic,
  UMenuText,
  UMusic,
  UTexture,
  UThemes;

type
{  Int16 = SmallInt;}

  PMenu = ^TMenu;
  TMenu = class
    protected
      Background:       TMenuBackground;

      Interactions:     array of TInteract;
      SelInteraction:   integer;

      ButtonPos:        integer;
      Button:           array of TButton;

      SelectsS:         array of TSelectSlide;
      ButtonCollection: array of TButtonCollection;
    public
      Text:        array of TText;
      Statics:     array of TStatic;
      StaticsList: array of TStatic;
      mX:         integer; // mouse X
      mY:         integer; // mouse Y

      Fade:       integer; // fade type
      ShowFinish: boolean; // true if there is no fade
      RightMbESC: boolean; // true to simulate ESC keypress when RMB is pressed

      destructor Destroy; override;
      constructor Create; overload; virtual;
      //constructor Create(Back: string); overload; virtual; // Back is a JPG resource name for background
      //constructor Create(Back: string; W, H: integer); overload; virtual; // W and H are the number of overlaps

      // interaction
      procedure AddInteraction(Typ, Num: integer);
      procedure SetInteraction(Num: integer); virtual;
      property Interaction: integer read SelInteraction write SetInteraction;

      // procedure load bg, texts, statics and button collections from themebasic
      procedure LoadFromTheme(const ThemeBasic: TThemeBasic);

      procedure PrepareButtonCollections(const Collections: AThemeButtonCollection);
      procedure AddButtonCollection(const ThemeCollection: TThemeButtonCollection; const Num: byte);

      // background
      procedure AddBackground(ThemedSettings: TThemeBackground);

      // static
      function AddStatic(ThemeStatic: TThemeStatic): integer; overload;
      function AddStatic(X, Y, W, H: real; const TexName: IPath): integer; overload;
      function AddStatic(X, Y, W, H: real; const TexName: IPath; Typ: TTextureType): integer; overload;
      function AddStatic(X, Y, W, H: real; ColR, ColG, ColB: real; const TexName: IPath; Typ: TTextureType): integer; overload;
      function AddStatic(X, Y, W, H, Z: real; ColR, ColG, ColB: real; const TexName: IPath; Typ: TTextureType): integer; overload;
      function AddStatic(X, Y, W, H: real; ColR, ColG, ColB: real; const TexName: IPath; Typ: TTextureType; Color: integer): integer; overload;
      function AddStatic(X, Y, W, H, Z: real; ColR, ColG, ColB: real; const TexName: IPath; Typ: TTextureType; Color: integer): integer; overload;
      function AddStatic(X, Y, W, H, Z: real; ColR, ColG, ColB: real; TexX1, TexY1, TexX2, TexY2: real; Alpha: real; const TexName: IPath; Typ: TTextureType; Color: integer; Reflection: boolean; ReflectionSpacing: real): integer; overload;

      // list
      function AddListItem(X, Y, W, H, Z: real; ColR, ColG, ColB: real; DColR, DColG, DColB: real; const TexName: IPath; const DTexName: IPath; Typ: TTextureType; Reflection: boolean; ReflectionSpacing: real): integer;

      // text
      function AddText(ThemeText: TThemeText): integer; overload;
      function AddText(X, Y: real; const Text_: UTF8String): integer; overload;
      function AddText(X, Y: real; Style: integer; Size, ColR, ColG, ColB: real; const Text: UTF8String): integer; overload;
      function AddText(X, Y, W: real; Style: integer; Size, ColR, ColG, ColB: real; Align: integer; const Text_: UTF8String; Reflection_: boolean; ReflectionSpacing_: real; Z : real; Writable: boolean): integer; overload;

      // button
      procedure SetButtonLength(Length: cardinal); //Function that Set Length of Button Array in one Step instead of register new Memory for every Button
      function AddButton(ThemeButton: TThemeButton): integer; overload;
      function AddButton(X, Y, W, H: real; const TexName: IPath): integer; overload;
      function AddButton(X, Y, W, H: real; const TexName: IPath; Typ: TTextureType; Reflection: boolean): integer; overload;
      function AddButton(X, Y, W, H, ColR, ColG, ColB, Int, DColR, DColG, DColB, DInt: real; const TexName: IPath; Typ: TTextureType; Reflection: boolean; ReflectionSpacing, DeSelectReflectionSpacing: real): integer; overload;
      procedure ClearButtons;
      procedure AddButtonText(AddX, AddY: real; const AddText: UTF8String); overload;
      procedure AddButtonText(AddX, AddY: real; ColR, ColG, ColB: real; const AddText: UTF8String); overload;
      procedure AddButtonText(AddX, AddY: real; ColR, ColG, ColB: real; Font: integer; Size: integer; Align: integer; const AddText: UTF8String); overload;
      procedure AddButtonText(CustomButton: TButton; AddX, AddY: real; ColR, ColG, ColB: real; Font: integer; Size: integer; Align: integer; const AddText: UTF8String); overload;

      // select slide
      function AddSelectSlide(ThemeSelectS: TThemeSelectSlide; var Data: integer; const Values: array of UTF8String): integer; overload;
      function AddSelectSlide(X, Y, W, H, SkipX, SBGW, ColR, ColG, ColB, Int, DColR, DColG, DColB, DInt,
        TColR, TColG, TColB, TInt, TDColR, TDColG, TDColB, TDInt,
        SBGColR, SBGColG, SBGColB, SBGInt, SBGDColR, SBGDColG, SBGDColB, SBGDInt,
        STColR, STColG, STColB, STInt, STDColR, STDColG, STDColB, STDInt: real;
        const TexName: IPath; Typ: TTextureType; const SBGName: IPath; SBGTyp: TTextureType;
        const Caption: UTF8String; var Data: integer): integer; overload;
      procedure AddSelectSlideOption(const AddText: UTF8String); overload;
      procedure AddSelectSlideOption(SelectNo: cardinal; const AddText: UTF8String); overload;
      procedure UpdateSelectSlideOptions(ThemeSelectSlide: TThemeSelectSlide; SelectNum: integer; const Values: array of UTF8String; var Data: integer);

//      function AddWidget(X, Y : UInt16; WidgetSrc : PSDL_Surface): Int16;
//      procedure ClearWidgets(MinNumber : Int16);
      procedure FadeTo(Screen: PMenu); overload;
      procedure FadeTo(Screen: PMenu; aSound: TAudioPlaybackStream); overload;
      //popup hack
      procedure CheckFadeTo(Screen: PMenu; Msg: UTF8String);

      function DrawBG: boolean; virtual;
      function DrawFG: boolean; virtual;
      function Draw: boolean; virtual;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown : boolean): boolean; virtual;
      function ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean; virtual;
      function InRegion(X, Y: real; A: TMouseOverRect): boolean;
      function InRegionX(X: real; A: TMouseOverRect): boolean;
      function InRegionY(Y: real; A: TMouseOverRect): boolean;
      function InteractAt(X, Y: real): integer;
      function CollectionAt(X, Y: real): integer;
      procedure OnShow; virtual;
      procedure OnShowFinish; virtual;
      procedure OnHide; virtual;

      procedure SetAnimationProgress(Progress: real); virtual;

      function IsSelectable(Int: cardinal): boolean;

      procedure InteractNext; virtual;
      procedure InteractCustom(CustomSwitch: integer); virtual;
      procedure InteractPrev; virtual;
      procedure InteractInc; virtual;
      procedure InteractDec; virtual;
      procedure InteractNextRow; virtual; // this is for the options screen, so button down makes sense
      procedure InteractPrevRow; virtual; // this is for the options screen, so button up makes sense
      procedure AddBox(X, Y, W, H: real);
  end;

function RGBFloatToInt(R, G, B: double): cardinal;

const
  MENU_MDOWN = 8;
  MENU_MUP   = 0;

  pmMove    = 1;
  pmClick   = 2;
  pmUnClick = 3;

  iButton           = 0; // interaction type
  iText             = 2;
  iSelectS          = 3;
  iBCollectionChild = 5;

//  fBlack = 0; // fade type
//  fWhite = 1;

implementation

uses
  UCommon,
  UCovers,
  UDisplay,
  UDrawTexture,
  UGraphic,
  ULog,
  UMain,
  USkins,
  UTime,
  //Background types
  UMenuBackgroundNone,
  UMenuBackgroundColor,
  UMenuBackgroundTexture,
  UMenuBackgroundVideo,
  UMenuBackgroundFade;

destructor TMenu.Destroy;
var
  I: integer;
begin
  for I := 0 to High(Button) do
    Button[I].Free;
  for I := 0 to High(ButtonCollection) do
    ButtonCollection[I].Free;
  for I := 0 to High(SelectsS) do
    SelectsS[I].Free;
  for I := 0 to High(Text) do
    Text[I].Free;
  for I := 0 to High(Statics) do
    Statics[I].Free;

  Background.Free;

  //Log.LogError('Unloaded Succesful: ' + ClassName);
  inherited;
end;

constructor TMenu.Create;
begin
  inherited;

  Fade := 0;//fWhite;

  SetLength(Statics, 0);
  SetLength(Button, 0);

  //Set ButtonPos to Autoset Length
  ButtonPos := -1;

  Background := nil;

  RightMbESC := true;
end;
{
constructor TMenu.Create(Back: string);
begin
  inherited Create;

  if Back <> '' then
  begin
//    BackImg := Texture.GetTexture(true, Back, TEXTURE_TYPE_PLAIN, 0);
    BackImg := Texture.GetTexture(Back, TEXTURE_TYPE_PLAIN, 0); // new theme system
    BackImg.W := 800;//640;
    BackImg.H := 600;//480;
    BackW := 1;
    BackH := 1;
  end
  else
    BackImg.TexNum := 0;

   //Set ButtonPos to Autoset Length
   ButtonPos := -1;
end;

constructor TMenu.Create(Back: string; W, H: integer);
begin
  Create(Back);
  BackImg.W := BackImg.W / W;
  BackImg.H := BackImg.H / H;
  BackW := W;
  BackH := H;
end;   }

function RGBFloatToInt(R, G, B: double): cardinal;
begin
  Result := (Trunc(255 * R) shl 16) or
            (Trunc(255 * G) shl  8) or
             Trunc(255 * B);
end;

procedure TMenu.AddInteraction(Typ, Num: integer);
var
  IntNum: integer;
begin
  IntNum := Length(Interactions);
  SetLength(Interactions, IntNum+1);
  Interactions[IntNum].Typ := Typ;
  Interactions[IntNum].Num := Num;
  Interaction := 0;
end;

procedure TMenu.SetInteraction(Num: integer);
var
  OldNum, OldTyp: integer;
  NewNum, NewTyp: integer;
begin
  // set inactive
  OldNum := Interactions[Interaction].Num;
  OldTyp := Interactions[Interaction].Typ;

  NewNum := Interactions[Num].Num;
  NewTyp := Interactions[Num].Typ;

  case OldTyp of
    iButton:  Button[OldNum].Selected := false;
    iText:    Text[OldNum].Selected := false;
    iSelectS: SelectsS[OldNum].Selected := false;
    //Button Collection Mod
    iBCollectionChild:
      begin
        Button[OldNum].Selected := false;
      
        // deselect collection if next button is not from collection
        if (NewTyp <> iButton) or (Button[NewNum].Parent <> Button[OldNum].Parent) then
          ButtonCollection[Button[OldNum].Parent-1].Selected := false;
      end;
  end;

  // set active
  SelInteraction := Num;
  case NewTyp of
    iButton:  Button[NewNum].Selected := true;
    iText:    Text[NewNum].Selected := true;
    iSelectS: SelectsS[NewNum].Selected := true;

    //Button Collection Mod
    iBCollectionChild:
      begin
        Button[NewNum].Selected := true;
        ButtonCollection[Button[NewNum].Parent-1].Selected := true;
      end;
  end;
end;

//----------------------
//LoadFromTheme - Load BG, Texts, Statics and
//Button Collections from ThemeBasic
//----------------------
procedure TMenu.LoadFromTheme(const ThemeBasic: TThemeBasic);
var
  I: integer;
begin
  //Add Button Collections (Set Button CollectionsLength)
  //Button Collections are Created when the first ChildButton is Created
  PrepareButtonCollections(ThemeBasic.ButtonCollection);

  //Add Background
  AddBackground(ThemeBasic.Background);

  //Add Statics and Texts
  for I := 0 to High(ThemeBasic.Statics) do
    AddStatic(ThemeBasic.Statics[I]);

  for I := 0 to High(ThemeBasic.Text) do
    AddText(ThemeBasic.Text[I]);
end;

procedure TMenu.AddBackground(ThemedSettings: TThemeBackground);
  var
    FileExt: string;

  function IsInArray(const Piece: string; const A: array of string): boolean;
  var
    I: integer;
  begin
    Result := false;
  
    for I := 0 to High(A) do
      if (A[I] = Piece) then
      begin
        Result := true;
        Exit;
      end;
  end;

  function TryBGCreate(Typ: cMenuBackground): boolean;
  begin
    Result := true;

    try
      Background := Typ.Create(ThemedSettings);
    except
      on E: EMenuBackgroundError do
      begin //Background failes to create
        Freeandnil(Background);
        Result := false;
      end;
    end;
  end;

begin
  FreeAndNil(Background);

  case ThemedSettings.BGType of
    bgtAuto: begin //Automaticly choose one out of BGT_Texture, BGT_Video or BGT_Color

      if (Length(ThemedSettings.Tex) > 0) then
      begin

        //At first some intelligent try to decide which BG to load
        FileExt := LowerCase(Skin.GetTextureFileName(ThemedSettings.Tex).GetExtension.ToUTF8);

        if IsInArray(FileExt, SUPPORTED_EXTS_BACKGROUNDTEXTURE) then
          TryBGCreate(TMenuBackgroundTexture)
        else if IsInArray(FileExt, SUPPORTED_EXTS_BACKGROUNDVIDEO) then
          TryBGCreate(TMenuBackgroundVideo);

        //If the intelligent method don't succeed
        //do it by trial and error
        if (Background = nil) then
        begin
          //Try Textured Bg
          if not TryBGCreate(TMenuBackgroundTexture) then
            TryBgCreate(TMenuBackgroundVideo); //Try Video BG

          //Color is fallback if Background = nil
        end;
      end;
    end;

    bgtColor: begin
      try
        Background := TMenuBackgroundColor.Create(ThemedSettings);
      except
        on E: EMenuBackgroundError do
        begin
          Log.LogError(E.Message);
          freeandnil(Background);
        end;
      end;
    end;

    bgtTexture: begin
      try
        Background := TMenuBackgroundTexture.Create(ThemedSettings);
      except
        on E: EMenuBackgroundError do
        begin
          Log.LogError(E.Message);
          freeandnil(Background);
        end;
      end;
    end;

    bgtVideo: begin
      try
        Background := TMenuBackgroundVideo.Create(ThemedSettings);
      except
        on E: EMenuBackgroundError do
        begin
          Log.LogError(E.Message);
          freeandnil(Background);
        end;
      end;
    end;

    bgtNone: begin
      try
        Background := TMenuBackgroundNone.Create(ThemedSettings);
      except
        on E: EMenuBackgroundError do
        begin
          Log.LogError(E.Message);
          freeandnil(Background);
        end;
      end;
    end;

    bgtFade: begin
      try
        Background := TMenuBackgroundFade.Create(ThemedSettings);
      except
        on E: EMenuBackgroundError do
        begin
          Log.LogError(E.Message);
          freeandnil(Background);
        end;
      end;
    end;
  end;

  //Fallback to None Background or Colored Background
  if (Background = nil) then
  begin
    if (ThemedSettings.BGType = bgtColor) then
      Background := TMenuBackgroundNone.Create(ThemedSettings)
    else
      Background := TMenuBackgroundColor.Create(ThemedSettings)
  end;
end;

//----------------------
//PrepareButtonCollections:
//Add Button Collections (Set Button CollectionsLength)
//----------------------
procedure TMenu.PrepareButtonCollections(const Collections: AThemeButtonCollection);
var
  I: integer;
begin
  SetLength(ButtonCollection, Length(Collections));
  for I := 0 to High(ButtonCollection) do
    AddButtonCollection(Collections[I], I);
end;

//----------------------
//AddButtonCollection:
//Create a Button Collection;
//----------------------
procedure TMenu.AddButtonCollection(const ThemeCollection: TThemeButtonCollection; const Num: byte);
var
  BT, BTLen:         integer;
  TempCol, TempDCol: cardinal;

begin
  if (Num > High(ButtonCollection)) then
    exit;

  TempCol := 0;

  // colorize hack
  if (ThemeCollection.Style.Typ = TEXTURE_TYPE_COLORIZED) then
  begin
    TempCol  := RGBFloatToInt(ThemeCollection.Style.ColR, ThemeCollection.Style.ColG, ThemeCollection.Style.ColB);
    TempDCol := RGBFloatToInt(ThemeCollection.Style.DColR, ThemeCollection.Style.DColG, ThemeCollection.Style.DColB);
    // give encoded color to GetTexture()
    ButtonCollection[Num] := TButtonCollection.Create(
      Texture.GetTexture(Skin.GetTextureFileName(ThemeCollection.Style.Tex), TEXTURE_TYPE_COLORIZED, TempCol),
      Texture.GetTexture(Skin.GetTextureFileName(ThemeCollection.Style.Tex), TEXTURE_TYPE_COLORIZED, TempDCol));
  end
  else
  begin
    ButtonCollection[Num] := TButtonCollection.Create(Texture.GetTexture(
      Skin.GetTextureFileName(ThemeCollection.Style.Tex), ThemeCollection.Style.Typ));
  end;

  //Set Parent menu
  ButtonCollection[Num].ScreenButton := @Self.Button;

  //Set Attributes
  ButtonCollection[Num].FirstChild := ThemeCollection.FirstChild;
  ButtonCollection[Num].CountChilds := ThemeCollection.ChildCount;
  ButtonCollection[Num].Parent := Num + 1;

  //Set Style
  ButtonCollection[Num].X := ThemeCollection.Style.X;
  ButtonCollection[Num].Y := ThemeCollection.Style.Y;
  ButtonCollection[Num].W := ThemeCollection.Style.W;
  ButtonCollection[Num].H := ThemeCollection.Style.H;
  if (ThemeCollection.Style.Typ <> TEXTURE_TYPE_COLORIZED) then
  begin
    ButtonCollection[Num].SelectColR := ThemeCollection.Style.ColR;
    ButtonCollection[Num].SelectColG := ThemeCollection.Style.ColG;
    ButtonCollection[Num].SelectColB := ThemeCollection.Style.ColB;
    ButtonCollection[Num].DeselectColR := ThemeCollection.Style.DColR;
    ButtonCollection[Num].DeselectColG := ThemeCollection.Style.DColG;
    ButtonCollection[Num].DeselectColB := ThemeCollection.Style.DColB;
  end;
  ButtonCollection[Num].SelectInt := ThemeCollection.Style.Int;
  ButtonCollection[Num].DeselectInt := ThemeCollection.Style.DInt;
  ButtonCollection[Num].Texture.TexX1 := 0;
  ButtonCollection[Num].Texture.TexY1 := 0;
  ButtonCollection[Num].Texture.TexX2 := 1;
  ButtonCollection[Num].Texture.TexY2 := 1;
  ButtonCollection[Num].SetSelect(false);

  ButtonCollection[Num].Reflection := ThemeCollection.Style.Reflection;
  ButtonCollection[Num].Reflectionspacing := ThemeCollection.Style.ReflectionSpacing;
  ButtonCollection[Num].DeSelectReflectionspacing := ThemeCollection.Style.DeSelectReflectionSpacing;

  ButtonCollection[Num].Z := ThemeCollection.Style.Z;

  //Some Things from ButtonFading
  ButtonCollection[Num].SelectH := ThemeCollection.Style.SelectH;
  ButtonCollection[Num].SelectW := ThemeCollection.Style.SelectW;

  ButtonCollection[Num].Fade := ThemeCollection.Style.Fade;
  ButtonCollection[Num].FadeText := ThemeCollection.Style.FadeText;
  if (ThemeCollection.Style.Typ = TEXTURE_TYPE_COLORIZED) then
  begin
    ButtonCollection[Num].FadeTex := Texture.GetTexture(
      Skin.GetTextureFileName(ThemeCollection.Style.FadeTex), TEXTURE_TYPE_COLORIZED, TempCol)
  end
  else
  begin
    ButtonCollection[Num].FadeTex := Texture.GetTexture(
      Skin.GetTextureFileName(ThemeCollection.Style.FadeTex), ThemeCollection.Style.Typ);
  end;
  ButtonCollection[Num].FadeTexPos := ThemeCollection.Style.FadeTexPos;

  BTLen := Length(ThemeCollection.Style.Text);
  for BT := 0 to BTLen-1 do
  begin
      AddButtonText(ButtonCollection[Num], ThemeCollection.Style.Text[BT].X, ThemeCollection.Style.Text[BT].Y,
        ThemeCollection.Style.Text[BT].ColR, ThemeCollection.Style.Text[BT].ColG, ThemeCollection.Style.Text[BT].ColB,
        ThemeCollection.Style.Text[BT].Font, ThemeCollection.Style.Text[BT].Size, ThemeCollection.Style.Text[BT].Align,
        ThemeCollection.Style.Text[BT].Text);
  end;
end;

function TMenu.AddStatic(ThemeStatic: TThemeStatic): integer;
begin
  Result := AddStatic(ThemeStatic.X, ThemeStatic.Y, ThemeStatic.W, ThemeStatic.H, ThemeStatic.Z,
    ThemeStatic.ColR, ThemeStatic.ColG, ThemeStatic.ColB,
    ThemeStatic.TexX1, ThemeStatic.TexY1, ThemeStatic.TexX2, ThemeStatic.TexY2, ThemeStatic.Alpha,
    Skin.GetTextureFileName(ThemeStatic.Tex),
    ThemeStatic.Typ, $FFFFFF, ThemeStatic.Reflection, ThemeStatic.Reflectionspacing);
end;

function TMenu.AddStatic(X, Y, W, H: real; const TexName: IPath): integer;
begin
  Result := AddStatic(X, Y, W, H, TexName, TEXTURE_TYPE_PLAIN);
end;

function TMenu.AddStatic(X, Y, W, H: real;
       ColR, ColG, ColB: real;
			 const TexName: IPath;
			 Typ: TTextureType): integer;
begin
  Result := AddStatic(X, Y, W, H, ColR, ColG, ColB, TexName, Typ, $FFFFFF);
end;

function TMenu.AddStatic(X, Y, W, H, Z: real;
       ColR, ColG, ColB: real;
			 const TexName: IPath;
			 Typ: TTextureType): integer;
begin
  Result := AddStatic(X, Y, W, H, Z, ColR, ColG, ColB, TexName, Typ, $FFFFFF);
end;

function TMenu.AddStatic(X, Y, W, H: real;
       const TexName: IPath;
			 Typ: TTextureType): integer;
var
  StatNum: integer;
begin
  // adds static
  StatNum := Length(Statics);
  SetLength(Statics, StatNum + 1);
  Statics[StatNum] := TStatic.Create(Texture.GetTexture(TexName, Typ, $FF00FF)); // new skin

  // configures static
  Statics[StatNum].Texture.X := X;
  Statics[StatNum].Texture.Y := Y;
  Statics[StatNum].Texture.W := W;
  Statics[StatNum].Texture.H := H;
  Statics[StatNum].Visible := true;
  Result := StatNum;
end;

function TMenu.AddStatic(X, Y, W, H: real;
                         ColR, ColG, ColB: real;
			 const TexName: IPath;
			 Typ: TTextureType;
			 Color: integer): integer;
begin
  Result := AddStatic(X, Y, W, H, 0, ColR, ColG, ColB, TexName, Typ, Color);
end;

function TMenu.AddStatic(X, Y, W, H, Z: real;
                         ColR, ColG, ColB: real;
			 const TexName: IPath;
			 Typ: TTextureType;
			 Color: integer): integer;
begin
  Result := AddStatic(X, Y, W, H, Z, ColR, ColG, ColB, 0, 0, 1, 1, 1, TexName, Typ, Color, false, 0);
end;

function TMenu.AddStatic(X, Y, W, H, Z: real;
                         ColR, ColG, ColB: real;
			 TexX1, TexY1, TexX2, TexY2: real; Alpha: real;
			 const TexName: IPath;
			 Typ: TTextureType;
			 Color: integer;
			 Reflection: boolean;
			 ReflectionSpacing: real): integer;
var
  StatNum: integer;
begin
  // adds static
  StatNum := Length(Statics);
  SetLength(Statics, StatNum + 1);

  // colorize hack
  if (Typ = TEXTURE_TYPE_COLORIZED) then
  begin
    // give encoded color to GetTexture()
    Statics[StatNum] := TStatic.Create(Texture.GetTexture(TexName, Typ, RGBFloatToInt(ColR, ColG, ColB)));
  end
  else
  begin
    Statics[StatNum] := TStatic.Create(Texture.GetTexture(TexName, Typ, Color)); // new skin
  end;

  // configures static
  Statics[StatNum].Texture.X := X;
  Statics[StatNum].Texture.Y := Y;

  //Set height and width via sprite size if omitted
  if(H = 0) then
    Statics[StatNum].Texture.H := Statics[StatNum].Texture.H
  else
    Statics[StatNum].Texture.H := H;

  if(W = 0) then
    Statics[StatNum].Texture.W := Statics[StatNum].Texture.W
  else
    Statics[StatNum].Texture.W := W;

  Statics[StatNum].Texture.Z := Z;
  if (Typ <> TEXTURE_TYPE_COLORIZED) then
  begin
    Statics[StatNum].Texture.ColR := ColR;
    Statics[StatNum].Texture.ColG := ColG;
    Statics[StatNum].Texture.ColB := ColB;
  end;
  Statics[StatNum].Texture.TexX1 := TexX1;
  Statics[StatNum].Texture.TexY1 := TexY1;
  Statics[StatNum].Texture.TexX2 := TexX2;
  Statics[StatNum].Texture.TexY2 := TexY2;
  Statics[StatNum].Texture.Alpha := Alpha;
  Statics[StatNum].Visible := true;

  //ReflectionMod
  Statics[StatNum].Reflection := Reflection;
  Statics[StatNum].ReflectionSpacing := ReflectionSpacing;

  Result := StatNum;
end;

function TMenu.AddListItem(X, Y, W, H, Z: real;
       ColR, ColG, ColB: real;
       DColR, DColG, DColB: real;
			 const TexName: IPath;
			 const DTexName: IPath;
			 Typ: TTextureType;
			 Reflection: boolean;
			 ReflectionSpacing: real): integer;
var
  StatNum: integer;
begin
  // adds static
  StatNum := Length(StaticsList);
  SetLength(StaticsList, StatNum + 1);

  StaticsList[StatNum] := TStatic.Create(Texture.GetTexture(TexName, Typ, RGBFloatToInt(ColR, ColG, ColB)));
  StaticsList[StatNum].TextureSelect := Texture.GetTexture(TexName, Typ, RGBFloatToInt(ColR, ColG, ColB));
  StaticsList[StatNum].TextureDeselect := Texture.GetTexture(DTexName, Typ, RGBFloatToInt(DColR, DColG, DColB));

  // configures static
  StaticsList[StatNum].Texture.X := X;
  StaticsList[StatNum].Texture.Y := Y;

  //Set height and width via sprite size if omitted
  if(H = 0) then
    StaticsList[StatNum].Texture.H := StaticsList[StatNum].Texture.H
  else
    StaticsList[StatNum].Texture.H := H;

  if(W = 0) then
    StaticsList[StatNum].Texture.W := StaticsList[StatNum].Texture.W
  else
    StaticsList[StatNum].Texture.W := W;

  StaticsList[StatNum].Texture.Z := Z;
  if (Typ <> TEXTURE_TYPE_COLORIZED) then
  begin
    StaticsList[StatNum].Texture.ColR := ColR;
    StaticsList[StatNum].Texture.ColG := ColG;
    StaticsList[StatNum].Texture.ColB := ColB;
  end;

  StaticsList[StatNum].Texture.Alpha := 1;
  StaticsList[StatNum].Visible := true;

  //ReflectionMod
  StaticsList[StatNum].Reflection := Reflection;
  StaticsList[StatNum].ReflectionSpacing := ReflectionSpacing;

  Result := StatNum;
end;

function TMenu.AddText(ThemeText: TThemeText): integer;
begin
  Result := AddText(ThemeText.X, ThemeText.Y, ThemeText.W, ThemeText.Font, ThemeText.Size,
    ThemeText.ColR, ThemeText.ColG, ThemeText.ColB, ThemeText.Align, ThemeText.Text, ThemeText.Reflection, ThemeText.ReflectionSpacing, ThemeText.Z, ThemeText.Writable);
end;

function TMenu.AddText(X, Y: real; const Text_: UTF8String): integer;
var
  TextNum: integer;
begin
  // adds text
  TextNum := Length(Text);
  SetLength(Text, TextNum + 1);
  Text[TextNum] := TText.Create(X, Y, Text_);
  Result := TextNum;
end;

function TMenu.AddText(X, Y: real;
                      Style: integer;
                      Size, ColR, ColG, ColB: real;
                      const Text: UTF8String): integer;
begin
  Result := AddText(X, Y, 0, Style, Size, ColR, ColG, ColB, 0, Text, false, 0, 0, false);
end;

function TMenu.AddText(X, Y, W: real;
                       Style: integer;
                       Size, ColR, ColG, ColB: real;
                       Align: integer;
                       const Text_: UTF8String;
                       Reflection_: boolean;
                       ReflectionSpacing_: real;
                       Z : real;
                       Writable: boolean): integer;
var
  TextNum: integer;
begin
  // adds text
  TextNum := Length(Text);
  SetLength(Text, TextNum + 1);
  Text[TextNum] := TText.Create(X, Y, W, Style, Size, ColR, ColG, ColB, Align, Text_, Reflection_, ReflectionSpacing_, Z, Writable);
  Result := TextNum;
end;

//Function that Set Length of Button boolean in one Step instead of register new Memory for every Button
procedure TMenu.SetButtonLength(Length: cardinal);
begin
  if (ButtonPos = -1) and (Length > 0) then
  begin
    //Set Length of Button
    SetLength(Button, Length);

    //Set ButtonPos to start with 0
    ButtonPos := 0;
  end;
end;

// Method to add a button in our TMenu. It returns the assigned ButtonNumber
function TMenu.AddButton(ThemeButton: TThemeButton): integer;
var
  BT:    integer;
  BTLen: integer;
begin
  Result := AddButton(ThemeButton.X, ThemeButton.Y, ThemeButton.W, ThemeButton.H,
    ThemeButton.ColR, ThemeButton.ColG, ThemeButton.ColB, ThemeButton.Int,
    ThemeButton.DColR, ThemeButton.DColG, ThemeButton.DColB, ThemeButton.DInt,
    Skin.GetTextureFileName(ThemeButton.Tex), ThemeButton.Typ,
    ThemeButton.Reflection, ThemeButton.Reflectionspacing, ThemeButton.DeSelectReflectionspacing);

  Button[Result].Z := ThemeButton.Z;

  //Button Visibility
  Button[Result].Visible := ThemeButton.Visible;

  //Some Things from ButtonFading
  Button[Result].SelectH := ThemeButton.SelectH;
  Button[Result].SelectW := ThemeButton.SelectW;

  Button[Result].Fade := ThemeButton.Fade;
  Button[Result].FadeText := ThemeButton.FadeText;
  if (ThemeButton.Typ = TEXTURE_TYPE_COLORIZED) then
  begin
    Button[Result].FadeTex := Texture.GetTexture(
      Skin.GetTextureFileName(ThemeButton.FadeTex), TEXTURE_TYPE_COLORIZED,
      RGBFloatToInt(ThemeButton.ColR, ThemeButton.ColG, ThemeButton.ColB));
  end
  else
  begin
    Button[Result].FadeTex := Texture.GetTexture(
      Skin.GetTextureFileName(ThemeButton.FadeTex), ThemeButton.Typ);
  end;

  Button[Result].FadeTexPos := ThemeButton.FadeTexPos;

  BTLen := Length(ThemeButton.Text);
  for BT := 0 to BTLen-1 do
  begin
    AddButtonText(ThemeButton.Text[BT].X, ThemeButton.Text[BT].Y,
      ThemeButton.Text[BT].ColR, ThemeButton.Text[BT].ColG, ThemeButton.Text[BT].ColB,
      ThemeButton.Text[BT].Font, ThemeButton.Text[BT].Size, ThemeButton.Text[BT].Align,
      ThemeButton.Text[BT].Text);
  end;

  // bautton collection mod
  if (ThemeButton.Parent <> 0) then
  begin
    // if collection exists then change interaction to child button
    if (@ButtonCollection[ThemeButton.Parent-1] <> nil) then
    begin
      Interactions[High(Interactions)].Typ := iBCollectionChild;
      Button[Result].Visible := false;

      for BT := 0 to BTLen-1 do
        Button[Result].Text[BT].Alpha := 0;

      Button[Result].Parent := ThemeButton.Parent;
      if (ButtonCollection[ThemeButton.Parent-1].Fade) then
        Button[Result].Texture.Alpha := 0;
    end;
  end;
end;

function TMenu.AddButton(X, Y, W, H: real; const TexName: IPath): integer;
begin
  Result := AddButton(X, Y, W, H, TexName, TEXTURE_TYPE_PLAIN, false);
end;

function TMenu.AddButton(X, Y, W, H: real; const TexName: IPath; Typ: TTextureType; Reflection: boolean): integer;
begin
  Result := AddButton(X, Y, W, H, 1, 1, 1, 1, 1, 1, 1, 0.5, TexName, TEXTURE_TYPE_PLAIN, Reflection, 15, 15);
end;

function TMenu.AddButton(X, Y, W, H, ColR, ColG, ColB, Int, DColR, DColG, DColB, DInt: real;
                         const TexName: IPath;
			 Typ: TTextureType;
                         Reflection: boolean;
			 ReflectionSpacing, DeSelectReflectionSpacing: real): integer;
begin
  // adds button
  //SetLength is used once to reduce Memory usement
  if (ButtonPos <> -1) then
  begin
    Result := ButtonPos;
    Inc(ButtonPos)
  end
  else //Old Method -> Reserve new Memory for every Button
  begin
    Result := Length(Button);
    SetLength(Button, Result + 1);
  end;

  // colorize hack
  if (Typ = TEXTURE_TYPE_COLORIZED) then
  begin
    // give encoded color to GetTexture()
    Button[Result] := TButton.Create(Texture.GetTexture(TexName, Typ, RGBFloatToInt(ColR, ColG, ColB)),
                                     Texture.GetTexture(TexName, Typ, RGBFloatToInt(DColR, DColG, DColB)));
  end
  else
  begin
    Button[Result] := TButton.Create(Texture.GetTexture(TexName, Typ));
  end;

  // configures button
  Button[Result].X := X;
  Button[Result].Y := Y;
  Button[Result].W := W;
  Button[Result].H := H;
  if (Typ <> TEXTURE_TYPE_COLORIZED) then
  begin
    Button[Result].SelectColR := ColR;
    Button[Result].SelectColG := ColG;
    Button[Result].SelectColB := ColB;
    Button[Result].DeselectColR := DColR;
    Button[Result].DeselectColG := DColG;
    Button[Result].DeselectColB := DColB;
  end;
  Button[Result].SelectInt := Int;
  Button[Result].DeselectInt := DInt;
  Button[Result].Texture.TexX1 := 0;
  Button[Result].Texture.TexY1 := 0;
  Button[Result].Texture.TexX2 := 1;
  Button[Result].Texture.TexY2 := 1;
  Button[Result].SetSelect(false);

  Button[Result].Reflection := Reflection;
  Button[Result].Reflectionspacing := ReflectionSpacing;
  Button[Result].DeSelectReflectionspacing := DeSelectReflectionSpacing;

  // button collection mod
  Button[Result].Parent := 0;

  // adds interaction
  AddInteraction(iButton, Result);
  Interaction := 0;
end;

procedure TMenu.ClearButtons;
begin
  Setlength(Button, 0);
end;

// method to draw our tmenu and all his child buttons
function TMenu.DrawBG: boolean;
begin
  Background.Draw;
  Result := true;
end;

function TMenu.DrawFG: boolean;
var
  J: integer;
begin
  // We don't forget about newly implemented static for nice skin ...
  for J := 0 to High(Statics) do
    Statics[J].Draw;

  // ... and slightly implemented menutext unit
  for J := 0 to High(Text) do
    Text[J].Draw;

  //  Draw all ButtonCollections
  for J := 0 to High(ButtonCollection) do
    ButtonCollection[J].Draw;

  // Second, we draw all of our buttons
  for J := 0 to High(Button) do
    Button[J].Draw;

  for J := 0 to High(SelectsS) do
    SelectsS[J].Draw;

  // Third, we draw all our widgets
  //  for J := 0 to Length(WidgetsSrc) - 1 do
  //    SDL_BlitSurface(WidgetsSrc[J], nil, ParentBackBuf, WidgetsRect[J]);
  Result := true;
end;

function TMenu.Draw: boolean;
begin
  DrawBG;
  DrawFG;
  Result := true;
end;

{
function TMenu.GetNextScreen(): PMenu;
begin
  Result := NextScreen;
end;
}

{
function TMenu.AddWidget(X, Y: UInt16; WidgetSrc: PSDL_Surface): Int16;
var
  WidgetNum: Int16;
begin
  if (Assigned(WidgetSrc)) then
  begin
    WidgetNum := Length(WidgetsSrc);

    SetLength(WidgetsSrc, WidgetNum + 1);
    SetLength(WidgetsRect, WidgetNum + 1);

    WidgetsSrc[WidgetNum] := WidgetSrc;
    WidgetsRect[WidgetNum] := new(PSDL_Rect);
    WidgetsRect[WidgetNum]^.x := X;
    WidgetsRect[WidgetNum]^.y := Y;
    WidgetsRect[WidgetNum]^.w := WidgetSrc^.w;
    WidgetsRect[WidgetNum]^.h := WidgetSrc^.h;

    Result := WidgetNum;
  end
  else
    Result := -1;
end;
}

{
procedure TMenu.ClearWidgets(MinNumber: Int16);
var
  J: Int16;
begin
  for J := MinNumber to (Length(WidgetsSrc) - 1) do
  begin
    SDL_FreeSurface(WidgetsSrc[J]);
    dispose(WidgetsRect[J]);
  end;

  SetLength(WidgetsSrc, MinNumber);
  SetLength(WidgetsRect, MinNumber);
end;
}

function TMenu.IsSelectable(Int: cardinal): boolean;
begin
  Result := true;
  case Interactions[Int].Typ of
    //Button
    iButton: Result := Button[Interactions[Int].Num].Visible and Button[Interactions[Int].Num].Selectable;

    //Select Slide
    iSelectS: Result := SelectsS[Interactions[Int].Num].Visible;

    //ButtonCollection Child
    iBCollectionChild:
      Result := (ButtonCollection[Button[Interactions[Int].Num].Parent - 1].FirstChild - 1 = Int) and ((Interactions[Interaction].Typ <> iBCollectionChild) or (Button[Interactions[Interaction].Num].Parent <> Button[Interactions[Int].Num].Parent));
  end;
end;

// implemented for the sake of usablility
// [curser down] picks the button left to the actual atm
// this behaviour doesn't make sense for two rows of buttons
procedure TMenu.InteractPrevRow;
var
  Int: integer;
begin
  // these two procedures just make sense for at least 5 buttons, because we
  // usually start a second row when there are more than 4 buttons
  Int := Interaction;

  Int := Int - 4;//ceil(Length(Interactions) / 2);

  //Set Interaction
  if ((Int < 0) or (Int > Length(Interactions) - 1)) then
    Int         := Interaction // invalid button, keep current one
  else
    Interaction := Int;        // select row above
end;

procedure TMenu.InteractNextRow;
var
  Int: integer;
begin
  Int := Interaction;

  Int := Int + 4; //ceil(Length(Interactions) / 2);

  //Set Interaction
  if ((Int < 0) or (Int > Length(Interactions) - 1)) then
    Int         := Interaction // invalid button, keep current one
  else
    Interaction := Int;        // select row above

end;

procedure TMenu.InteractNext;
var
  Int: integer;
begin
  Int := Interaction;

  // change interaction as long as it's needed
  repeat
    Int := (Int + 1) mod Length(Interactions);

    //If no Interaction is Selectable Simply Select Next
    if (Int = Interaction) then
      Break;

  until IsSelectable(Int);

  //Set Interaction
  Interaction := Int;
end;

procedure TMenu.InteractPrev;
var
  Int: integer;
begin
  Int := Interaction;

  // change interaction as long as it's needed
  repeat
    Int := Int - 1;
    if Int = -1 then
      Int := High(Interactions);

    //If no Interaction is Selectable Simply Select Next
    if (Int = Interaction) then
      Break;
  until IsSelectable(Int);

  //Set Interaction
  Interaction := Int
end;

procedure TMenu.InteractCustom(CustomSwitch: integer);
{ needed only for below
var
  Num:   integer;
  Typ:   integer;
  Again: boolean;
}
begin
  //Code Commented atm, because it needs to be Rewritten
  //it doesn't work with Button Collections
  {then
  begin
    CustomSwitch:= CustomSwitch*(-1);
    Again := true;
  // change interaction as long as it's needed
  while (Again = true) do
  begin
    Num := SelInteraction - CustomSwitch;
    if Num = -1 then
      Num := High(Interactions);
    Interaction := Num;
    Again := false; // reset, default to accept changing interaction

    // checking newly interacted element
    Num := Interactions[Interaction].Num;
    Typ := Interactions[Interaction].Typ;
    case Typ of
    iButton:
      begin
        if Button[Num].Selectable = false then
	  Again := true;
      end;
    end; // case
  end; // while
  end
  else if num>0 then
  begin
    Again := true;
    // change interaction as long as it's needed
  while (Again = true) do
  begin
    Num := (Interaction + CustomSwitch) Mod Length(Interactions);
    Interaction := Num;
    Again := false; // reset, default to accept changing interaction

    // checking newly interacted element
    Num := Interactions[Interaction].Num;
    Typ := Interactions[Interaction].Typ;
    case Typ of
    iButton:
      begin
        if Button[Num].Selectable = false then
	  Again := true;
      end;
    end; // case
  end; // while
    end     }
end;

procedure TMenu.FadeTo(Screen: PMenu);
begin
  Display.Fade := 0;
  Display.NextScreen := Screen;
end;

procedure TMenu.FadeTo(Screen: PMenu; aSound: TAudioPlaybackStream);
begin
  FadeTo( Screen );
  AudioPlayback.PlaySound( aSound );
end;

procedure OnSaveEncodingError(Value: boolean; Data: Pointer);
begin
  Display.CheckOK := Value;
  if (Value) then
  begin
    //Hack to Finish Singscreen correct on Exit with Q Shortcut
    if (Display.NextScreenWithCheck = nil) then
    begin
      if (Display.CurrentScreen = @ScreenSing) then
        ScreenSing.Finish
      {else if (Display.CurrentScreen = @ScreenSingModi) then
        ScreenSingModi.Finish;}
    end;
  end
  else
  begin
    Display.NextScreenWithCheck := nil;
  end;
end;

//popup hack
procedure TMenu.CheckFadeTo(Screen: PMenu; Msg: UTF8String);
begin
  Display.Fade := 0;
  Display.NextScreenWithCheck := Screen;
  Display.CheckOK := false;
  ScreenPopupCheck.ShowPopup(msg, OnSaveEncodingError, nil, false);
end;

procedure TMenu.AddButtonText(AddX, AddY: real; const AddText: UTF8String);
begin
  AddButtonText(AddX, AddY, 1, 1, 1, AddText);
end;

procedure TMenu.AddButtonText(AddX, AddY: real; ColR, ColG, ColB: real; const AddText: UTF8String);
var
  Il: integer;
begin
  with Button[High(Button)] do
  begin
    Il := Length(Text);
    SetLength(Text, Il+1);
    Text[Il] := TText.Create(X + AddX, Y + AddY, AddText);
    Text[Il].ColR := ColR;
    Text[Il].ColG := ColG;
    Text[Il].ColB := ColB;
    Text[Il].Int := 1;//0.5;
  end;
end;

procedure TMenu.AddButtonText(AddX, AddY: real; ColR, ColG, ColB: real; Font: integer; Size: integer; Align: integer; const AddText: UTF8String);
var
  Il: integer;
begin
  with Button[High(Button)] do
  begin
    Il := Length(Text);
    SetLength(Text, Il+1);
    Text[Il] := TText.Create(X + AddX, Y + AddY, AddText);
    Text[Il].ColR := ColR;
    Text[Il].ColG := ColG;
    Text[Il].ColB := ColB;
    Text[Il].Int := 1;//0.5;
    Text[Il].Style := Font;
    Text[Il].Size := Size;
    Text[Il].Align := Align;
  end;
end;

procedure TMenu.AddButtonText(CustomButton: TButton; AddX, AddY: real; ColR, ColG, ColB: real; Font: integer; Size: integer; Align: integer; const AddText: UTF8String);
var
  Il: integer;
begin
  with CustomButton do
  begin
    Il := Length(Text);
    SetLength(Text, Il+1);
    Text[Il] := TText.Create(X + AddX, Y + AddY, AddText);
    Text[Il].ColR := ColR;
    Text[Il].ColG := ColG;
    Text[Il].ColB := ColB;
    Text[Il].Int := 1;//0.5;
    Text[Il].Style := Font;
    Text[Il].Size := Size;
    Text[Il].Align := Align;
  end;
end;

function TMenu.AddSelectSlide(ThemeSelectS: TThemeSelectSlide; var Data: integer; const Values: array of UTF8String): integer;
var
  SO: integer;
begin
  Result := AddSelectSlide(ThemeSelectS.X, ThemeSelectS.Y, ThemeSelectS.W, ThemeSelectS.H, ThemeSelectS.SkipX, ThemeSelectS.SBGW,
    ThemeSelectS.ColR, ThemeSelectS.ColG, ThemeSelectS.ColB, ThemeSelectS.Int,
    ThemeSelectS.DColR, ThemeSelectS.DColG, ThemeSelectS.DColB, ThemeSelectS.DInt,
    ThemeSelectS.TColR, ThemeSelectS.TColG, ThemeSelectS.TColB, ThemeSelectS.TInt,
    ThemeSelectS.TDColR, ThemeSelectS.TDColG, ThemeSelectS.TDColB, ThemeSelectS.TDInt,
    ThemeSelectS.SBGColR, ThemeSelectS.SBGColG, ThemeSelectS.SBGColB, ThemeSelectS.SBGInt,
    ThemeSelectS.SBGDColR, ThemeSelectS.SBGDColG, ThemeSelectS.SBGDColB, ThemeSelectS.SBGDInt,
    ThemeSelectS.STColR, ThemeSelectS.STColG, ThemeSelectS.STColB, ThemeSelectS.STInt,
    ThemeSelectS.STDColR, ThemeSelectS.STDColG, ThemeSelectS.STDColB, ThemeSelectS.STDInt,
    Skin.GetTextureFileName(ThemeSelectS.Tex), ThemeSelectS.Typ,
    Skin.GetTextureFileName(ThemeSelectS.TexSBG), ThemeSelectS.TypSBG,
    ThemeSelectS.Text, Data);
  for SO := 0 to High(Values) do
    AddSelectSlideOption(Values[SO]);

  SelectsS[High(SelectsS)].Text.Size := ThemeSelectS.TextSize;
  SelectsS[High(SelectsS)].Text.Y := ThemeSelectS.Y + (ThemeSelectS.H /2 ) - (ThemeSelectS.TextSize / 2);
  
  SelectsS[High(SelectsS)].Texture.Z := ThemeSelectS.Z;
  SelectsS[High(SelectsS)].TextureSBG.Z := ThemeSelectS.Z;
  SelectsS[High(SelectsS)].Tex_SelectS_ArrowL.Z := ThemeSelectS.Z;
  SelectsS[High(SelectsS)].Tex_SelectS_ArrowR.Z := ThemeSelectS.Z;

  SelectsS[High(SelectsS)].showArrows := ThemeSelectS.showArrows;
  SelectsS[High(SelectsS)].oneItemOnly := ThemeSelectS.oneItemOnly;

  //Generate Lines
  SelectsS[High(SelectsS)].GenLines;

  SelectsS[High(SelectsS)].SelectedOption := SelectsS[High(SelectsS)].SelectOptInt; // refresh
end;

function TMenu.AddSelectSlide(X, Y, W, H, SkipX, SBGW, ColR, ColG, ColB, Int, DColR, DColG, DColB, DInt,
  TColR, TColG, TColB, TInt, TDColR, TDColG, TDColB, TDInt,
  SBGColR, SBGColG, SBGColB, SBGInt, SBGDColR, SBGDColG, SBGDColB, SBGDInt,
  STColR, STColG, STColB, STInt, STDColR, STDColG, STDColB, STDInt: real;
  const TexName: IPath; Typ: TTextureType; const SBGName: IPath; SBGTyp: TTextureType;
  const Caption: UTF8String; var Data: integer): integer;
var
  S: integer;
  I: integer;
begin
  S := Length(SelectsS);
  SetLength(SelectsS, S + 1);
  SelectsS[S] := TSelectSlide.Create;

  if (Typ = TEXTURE_TYPE_COLORIZED) then
  begin
    SelectsS[S].Colorized := true;
    SelectsS[S].Texture := Texture.GetTexture(TexName, Typ, RGBFloatToInt(ColR, ColG, ColB));
    SelectsS[S].DeselectTexture := Texture.GetTexture(TexName, Typ, RGBFloatToInt(DColR, DColG, DColB));
  end
  else
  begin
    SelectsS[S].Colorized := false;
    SelectsS[S].Texture := Texture.GetTexture(TexName, Typ);
    
    SelectsS[S].ColR := ColR;
    SelectsS[S].ColG := ColG;
    SelectsS[S].ColB := ColB;

    SelectsS[S].DColR := DColR;
    SelectsS[S].DColG := DColG;
    SelectsS[S].DColB := DColB;
  end;
  
  SelectsS[S].Int := Int;
  SelectsS[S].DInt := DInt; 

  SelectsS[S].X := X;
  SelectsS[S].Y := Y;
  SelectsS[S].W := W;
  SelectsS[S].H := H;  

  if (SBGTyp = TEXTURE_TYPE_COLORIZED) then
  begin
    SelectsS[S].ColorizedSBG := true;
    SelectsS[S].TextureSBG := Texture.GetTexture(SBGName, SBGTyp, RGBFloatToInt(SBGColR, SBGColG, SBGColB));
    SelectsS[S].DeselectTextureSBG := Texture.GetTexture(SBGName, SBGTyp, RGBFloatToInt(SBGDColR, SBGDColG, SBGDColB));
  end
  else
  begin
    SelectsS[S].ColorizedSBG := false;
    SelectsS[S].TextureSBG := Texture.GetTexture(SBGName, SBGTyp);

    SelectsS[S].SBGColR := SBGColR;
    SelectsS[S].SBGColG := SBGColG;
    SelectsS[S].SBGColB := SBGColB;

    SelectsS[S].SBGDColR := SBGDColR;
    SelectsS[S].SBGDColG := SBGDColG;
    SelectsS[S].SBGDColB := SBGDColB;
  end;


  SelectsS[S].SBGInt := SBGInt;
  SelectsS[S].SBGDInt := SBGDInt;
  
  SelectsS[High(SelectsS)].Tex_SelectS_ArrowL   := Tex_SelectS_ArrowL;
  SelectsS[High(SelectsS)].Tex_SelectS_ArrowL.X := X + W + SkipX;
  SelectsS[High(SelectsS)].Tex_SelectS_ArrowL.Y := Y + (H - Tex_SelectS_ArrowL.H) / 2;
  SelectsS[High(SelectsS)].Tex_SelectS_ArrowL.W := Tex_SelectS_ArrowL.W;
  SelectsS[High(SelectsS)].Tex_SelectS_ArrowL.H := Tex_SelectS_ArrowL.H;


  SelectsS[High(SelectsS)].Tex_SelectS_ArrowR   := Tex_SelectS_ArrowR;
  SelectsS[High(SelectsS)].Tex_SelectS_ArrowR.X := X + W + SkipX + SBGW - Tex_SelectS_ArrowR.W;
  SelectsS[High(SelectsS)].Tex_SelectS_ArrowR.Y := Y + (H - Tex_SelectS_ArrowR.H) / 2;
  SelectsS[High(SelectsS)].Tex_SelectS_ArrowR.W := Tex_SelectS_ArrowR.W;
  SelectsS[High(SelectsS)].Tex_SelectS_ArrowR.H := Tex_SelectS_ArrowR.H;

  SelectsS[S].TextureSBG.X := X + W + SkipX;
  SelectsS[S].TextureSBG.Y := Y;
  SelectsS[S].SBGW := SBGW;
  SelectsS[S].TextureSBG.H := H;

  SelectsS[S].Text.X := X + 20;
  SelectsS[S].Text.Y := Y + (SelectsS[S].TextureSBG.H / 2) - 15;
  SelectsS[S].Text.Text := Caption;
  SelectsS[S].Text.Size := 30;
  SelectsS[S].Text.Visible := true;
  SelectsS[S].TColR := TColR;
  SelectsS[S].TColG := TColG;
  SelectsS[S].TColB := TColB;
  SelectsS[S].TInt := TInt;
  SelectsS[S].TDColR := TDColR;
  SelectsS[S].TDColG := TDColG;
  SelectsS[S].TDColB := TDColB;
  SelectsS[S].TDInt := TDInt;

  SelectsS[S].STColR := STColR;
  SelectsS[S].STColG := STColG;
  SelectsS[S].STColB := STColB;
  SelectsS[S].STInt := STInt;
  SelectsS[S].STDColR := STDColR;
  SelectsS[S].STDColG := STDColG;
  SelectsS[S].STDColB := STDColB;
  SelectsS[S].STDInt := STDInt;

  // new
  SelectsS[S].Texture.TexX1 := 0;
  SelectsS[S].Texture.TexY1 := 0;
  SelectsS[S].Texture.TexX2 := 1;
  SelectsS[S].Texture.TexY2 := 1;
  SelectsS[S].TextureSBG.TexX1 := 0;
  SelectsS[S].TextureSBG.TexY1 := 0;
  SelectsS[S].TextureSBG.TexX2 := 1;
  SelectsS[S].TextureSBG.TexY2 := 1;

  // Sets Data to copy the value of selectops to global value;
  SelectsS[S].PData := @Data;
  // Configures Select options
  {//SelectsS[S].TextOpt[0].Text := IntToStr(I+1);
  SelectsS[S].TextOpt[0].Size := 30;
  SelectsS[S].TextOpt[0].Align := 1;

  SelectsS[S].TextOpt[0].ColR := SelectsS[S].STDColR;
  SelectsS[S].TextOpt[0].ColG := SelectsS[S].STDColG;
  SelectsS[S].TextOpt[0].ColB := SelectsS[S].STDColB;
  SelectsS[S].TextOpt[0].Int := SelectsS[S].STDInt;
  SelectsS[S].TextOpt[0].Visible := true; }

  // Sets default value of selectopt from Data;
  SelectsS[S].SelectedOption := Data;

  // Disables default selection
  SelectsS[S].SetSelect(false);

  {// Configures 3 select options
  for I := 0 to 2 do
  begin
    SelectsS[S].TextOpt[I].X := SelectsS[S].TextureSBG.X + 20 + (50 + 20) + (150 - 20) * I;
    SelectsS[S].TextOpt[I].Y := SelectsS[S].TextureSBG.Y + 20;
    SelectsS[S].TextOpt[I].Text := IntToStr(I+1);
    SelectsS[S].TextOpt[I].Size := 30;
    SelectsS[S].TextOpt[I].Align := 1;

    SelectsS[S].TextOpt[I].ColR := SelectsS[S].STDColR;
    SelectsS[S].TextOpt[I].ColG := SelectsS[S].STDColG;
    SelectsS[S].TextOpt[I].ColB := SelectsS[S].STDColB;
    SelectsS[S].TextOpt[I].Int := SelectsS[S].STDInt;
    SelectsS[S].TextOpt[I].Visible := true;
  end;}

  // adds interaction
  AddInteraction(iSelectS, S);
  Result := S;
end;

procedure TMenu.AddSelectSlideOption(const AddText: UTF8String);
begin
  AddSelectSlideOption(High(SelectsS), AddText);
end;

procedure TMenu.AddSelectSlideOption(SelectNo: cardinal; const AddText: UTF8String);
var
  SO: integer;
begin
  SO := Length(SelectsS[SelectNo].TextOptT);

  SetLength(SelectsS[SelectNo].TextOptT, SO + 1);
  SelectsS[SelectNo].TextOptT[SO] := AddText;
{
  SelectsS[S].SelectedOption := SelectsS[S].SelectOptInt; // refresh

  if SO = Selects[S].PData^ then
    Selects[S].SelectedOption := SO;
}
end;

procedure TMenu.UpdateSelectSlideOptions(ThemeSelectSlide: TThemeSelectSlide;
  SelectNum: integer; const Values: array of UTF8String; var Data: integer);
var
  SO: integer;
begin
  SetLength(SelectsS[SelectNum].TextOptT, 0);
  for SO := 0 to High(Values) do
    AddSelectSlideOption(SelectNum, Values[SO]);

  SelectsS[SelectNum].GenLines;

//  SelectsS[SelectNum].SelectedOption := SelectsS[SelectNum].SelectOptInt; // refresh
//  SelectS[SelectNum].SetSelectOpt(Data);
//  SelectS[SelectNum].SelectedOption := 0;//Data;

//  Log.LogError(IntToStr(High(SelectsS[SelectNum].TextOptT)));
//  if 0 <= High(SelectsS[SelectNum].TextOptT) then

  SelectsS[SelectNum].PData := @Data;
  SelectsS[SelectNum].SelectedOption := Data;
end;

procedure TMenu.InteractInc;
var
  Num:   integer;
  Value: integer;
begin
  case Interactions[Interaction].Typ of
    iSelectS: begin
        Num := Interactions[Interaction].Num;
        Value := SelectsS[Num].SelectedOption;
//        Value := (Value + 1) Mod (Length(SelectsS[Num].TextOptT));

        // limit
        Value := Value + 1;
        if Value <= High(SelectsS[Num].TextOptT) then
          SelectsS[Num].SelectedOption := Value;
      end;
    //Button Collection Mod
    iBCollectionChild:
      begin

        //Select Next Button in Collection
        for Num := 1 to High(Button) do
        begin
          Value := (Interaction + Num) Mod Length(Button);
          if Value = 0 then
          begin
            InteractNext;
            Break;
          end;
          if (Button[Value].Parent = Button[Interaction].Parent) then
          begin
            Interaction := Value;
            Break;
          end;
        end;
      end;
    //interact Next if there is Nothing to Change
    else InteractNext;
  end;
end;

procedure TMenu.InteractDec;
var
  Num:   integer;
  Value: integer;
begin
  case Interactions[Interaction].Typ of
    iSelectS: begin
        Num := Interactions[Interaction].Num;
        Value := SelectsS[Num].SelectedOption;
        Value := Value - 1;
//        if Value = -1 then
//          Value := High(SelectsS[Num].TextOptT);

        if Value >= 0 then
          SelectsS[Num].SelectedOption := Value;
      end;
    //Button Collection Mod
    iBCollectionChild:
      begin
        //Select Prev Button in Collection
        for Num := High(Button) downto 1 do
        begin
          Value := (Interaction + Num) Mod Length(Button);
          if Value = High(Button) then
          begin
            InteractPrev;
            Break;
          end;
          if (Button[Value].Parent = Button[Interaction].Parent) then
          begin
            Interaction := Value;
            Break;
          end;
        end;
      end;
    // interact prev if there is nothing to change
    else
      begin
        InteractPrev;
        // if buttoncollection with more than 1 entry then select last entry
        if (Button[Interactions[Interaction].Num].Parent <> 0) and (ButtonCollection[Button[Interactions[Interaction].Num].Parent-1].CountChilds > 1) then
        begin
          //Select Last Child
          for Num := High(Button) downto 1 do
          begin
            Value := (Interaction + Num) Mod Length(Button);
            if (Button[Value].Parent = Button[Interaction].Parent) then
            begin
              Interaction := Value;
              Break;
            end;
          end;
        end;
      end;
  end;
end;

procedure TMenu.AddBox(X, Y, W, H: real);
begin
  AddStatic(X,   Y,   W,   H,   0, 0, 0, Skin.GetTextureFileName('MainBar'), TEXTURE_TYPE_COLORIZED);
  AddStatic(X+2, Y+2, W-4, H-4, 1, 1, 1, Skin.GetTextureFileName('MainBar'), TEXTURE_TYPE_COLORIZED);
end;

procedure TMenu.OnShow;
begin
  // FIXME: this needs some work. First, there should be a variable like
  // VideoBackground so we can check whether a video-background is enabled or not.
  // Second, a video should be stopped if the screen is hidden, but the Video.Stop()
  // method is not implemented by now. This is necessary for theme-switching too.
  // At the moment videos cannot be turned off without restarting USDX.

  {// check if a background texture was found
  if (BackImg.TexNum = 0)  then
  begin
    // try to open an animated background
    // Note: newer versions of ffmpeg are able to open images like jpeg
    //   so do not pass an image's filename to VideoPlayback.Open()
    if fileexists( fFileName ) then
    begin
      if VideoPlayback.Open( fFileName ) then
      begin
        VideoBGTimer.SetTime(0);
        VideoPlayback.Play;
      end;
    end;
  end; }
  if (Background = nil) then
    AddBackground(DEFAULT_BACKGROUND);

  Background.OnShow;
end;

procedure TMenu.OnShowFinish;
begin
  // nothing
end;

procedure TMenu.OnHide;
begin
  // nothing
  Background.OnFinish;
end;

function TMenu.ParseInput(PressedKey: Cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  // nothing
  Result := true;
end;

function TMenu.ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean;
var
  nBut: integer;
  Action: TMouseClickAction;
begin
  //default mouse parsing: clicking generates return keypress,
  //  mousewheel selects in select slide
  //override ParseMouse to customize
  Result := true;

  if RightMbESC and (MouseButton = SDL_BUTTON_RIGHT) and BtnDown then
  begin
    //if RightMbESC is set, send ESC keypress
    Result:=ParseInput(SDLK_ESCAPE, 0, true);
  end;

  // transfer mousecords to the 800x600 raster we use to draw
  X := Round((X / (ScreenW / Screens)) * RenderW);
  if (X > RenderW) then
    X := X - RenderW;
  Y := Round((Y / ScreenH) * RenderH);

  // allways go to next screen if we don't have any interactions
  if Length(Interactions) = 0 then
  begin
    if (BtnDown) and (MouseButton = SDL_BUTTON_LEFT) then
      Result := ParseInput(SDLK_RETURN, 0, true);
  end
  else
  begin
    nBut := InteractAt(X, Y);
    if nBut >= 0 then
    begin
      //select on mouse-over
      if nBut <> Interaction then
        SetInteraction(nBut);

      Action := maNone;

      if (BtnDown) then
      begin
        if (MouseButton = SDL_BUTTON_LEFT) then
        begin
          //click button or SelectS
          if (Interactions[nBut].Typ = iSelectS) then
            Action := SelectsS[Interactions[nBut].Num].OnClick(X, Y)
          else
            Action := maReturn;
        end
        else if (MouseButton = SDL_BUTTON_WHEELDOWN) then
        begin //forward on select slide with mousewheel
          if (Interactions[nBut].Typ = iSelectS) then
            Action := maRight;
        end
        else if (MouseButton = SDL_BUTTON_WHEELUP) then
        begin //backward on select slide with mousewheel
          if (Interactions[nBut].Typ = iSelectS) then
            Action := maLeft;
        end;
      end;

        // do the action we have to do ;)
      case Action of
        maReturn: Result := ParseInput(SDLK_RETURN, 0, true);
        maLeft:   Result := ParseInput(SDLK_LEFT, 0, true);
        maRight:  Result := ParseInput(SDLK_RIGHT, 0, true);
      end;
    end
    else
    begin
      nBut := CollectionAt(X, Y);
      if (nBut >= 0) and (not ButtonCollection[nBut].Selected) then
      begin
        // if over button collection, that is not already selected
        // -> select first child but don't allow click
        nBut := ButtonCollection[nBut].FirstChild - 1;
        if nBut <> Interaction then
          SetInteraction(nBut);
      end;
    end;
  end;
end;

function TMenu.InRegion(X, Y: real; A: TMouseOverRect): boolean;
begin
  // check whether A contains X and Y
  Result := (X >= A.X) and (X <= A.X + A.W) and (Y >= A.Y) and (Y <= A.Y + A.H);
end;

function TMenu.InRegionX(X: real; A: TMouseOverRect): boolean;
begin
  // check whether A contains X
  Result := (X >= A.X) and (X <= A.X + A.W);
end;

function TMenu.InRegionY(Y: real; A: TMouseOverRect): boolean;
begin
  // check whether A contains Y
  Result := (Y >= A.Y) and (Y <= A.Y + A.H);
end;

//takes x,y coordinates and returns the interaction number
//of the control at this position
function TMenu.InteractAt(X, Y: real): integer;
var
  i, nBut: integer;
begin
  Result := -1;
  for i := Low(Interactions) to High(Interactions) do
  begin
    case Interactions[i].Typ of
      iButton:
        if InRegion(X, Y, Button[Interactions[i].Num].GetMouseOverArea) and
           Button[Interactions[i].Num].Visible and Button[Interactions[i].Num].Selectable then
        begin
          Result:=i;
          exit;
        end;
      iBCollectionChild:
        if InRegion(X, Y, Button[Interactions[i].Num].GetMouseOverArea) then
        begin
          Result:=i;
          exit;
        end;
      iSelectS:
        if InRegion(X, Y, SelectSs[Interactions[i].Num].GetMouseOverArea)  then
      	begin
          Result:=i;
          exit;
        end;
    end;
  end;
end;

//takes x,y coordinates and returns the button collection id
function TMenu.CollectionAt(X, Y: real): integer;
var
  i, nBut: integer;
begin
  Result := -1;
  for i:= Low(ButtonCollection) to High(ButtonCollection) do
  begin
    if InRegion(X, Y, ButtonCollection[i].GetMouseOverArea) and
        ButtonCollection[i].Visible then
    begin
      Result:=i;
      exit;
    end;
  end;
end;

procedure TMenu.SetAnimationProgress(Progress: real);
begin
  // nothing
end;

end.
