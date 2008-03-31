unit UMenu;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses OpenGL12, SysUtils, UTexture, UMenuStatic, UMenuText, UMenuButton, UMenuSelect, UMenuSelectSlide,
  UMenuInteract, UThemes, UMenuButtonCollection, Math, UMusic;

type
{  Int16 = SmallInt;}

  PMenu = ^TMenu;
  TMenu = class
    protected
      ButtonPos:      Integer;

      Interactions:   array of TInteract;
      SelInteraction: integer;
      Button:         array of TButton;
      Selects:        array of TSelect;
      SelectsS:       array of TSelectSlide;
      ButtonCollection: array of TButtonCollection;
      BackImg:        TTexture;
      BackW:          integer;
      BackH:          integer;

      fFileName : string;
    public
      Text:       array of TText;
      Static:     array of TStatic;
      mX:         integer; // mouse X
      mY:         integer; // mouse Y

      Fade:       integer; // fade type
      ShowFinish: boolean; // true if there is no fade


      destructor Destroy; override;
      constructor Create; overload; virtual;
      //constructor Create(Back: string); overload; virtual; // Back is a JPG resource name for background
      //constructor Create(Back: string; W, H: integer); overload; virtual; // W and H are the number of overlaps

      // interaction
      function WideCharUpperCase(wchar: WideChar) : WideString;
      procedure AddInteraction(Typ, Num: integer);
      procedure SetInteraction(Num: integer);
      property Interaction: integer read SelInteraction write SetInteraction;

      //Procedure Load BG, Texts, Statics and Button Collections from ThemeBasic
      procedure LoadFromTheme(const ThemeBasic: TThemeBasic);

      procedure PrepareButtonCollections(const Collections: AThemeButtonCollection);
      procedure AddButtonCollection(const ThemeCollection: TThemeButtonCollection; Const Num: Byte);

      // background
      procedure AddBackground(Name: string);

      // static
      function AddStatic(ThemeStatic: TThemeStatic): integer; overload;
      function AddStatic(X, Y, W, H: real; const Name: string): integer; overload;
      function AddStatic(X, Y, W, H: real; const Name: string; Typ: TTextureType): integer; overload;
      function AddStatic(X, Y, W, H: real; ColR, ColG, ColB: real; const Name: string; Typ: TTextureType): integer; overload;
      function AddStatic(X, Y, W, H, Z: real; ColR, ColG, ColB: real; const Name: string; Typ: TTextureType): integer; overload;
      function AddStatic(X, Y, W, H: real; ColR, ColG, ColB: real; const Name: string; Typ: TTextureType; Color: integer): integer; overload;
      function AddStatic(X, Y, W, H, Z: real; ColR, ColG, ColB: real; const Name: string; Typ: TTextureType; Color: integer): integer; overload;
      function AddStatic(X, Y, W, H, Z: real; ColR, ColG, ColB: real; TexX1, TexY1, TexX2, TexY2: real; const Name: string; Typ: TTextureType; Color: integer; Reflection: Boolean; ReflectionSpacing: Real): integer; overload;

      // text
      function AddText(ThemeText: TThemeText): integer; overload;
      function AddText(X, Y: real; const Text_: string): integer; overload;
      function AddText(X, Y: real; Style: integer; Size, ColR, ColG, ColB: real; const Text: string): integer; overload;
      function AddText(X, Y, W: real; Style: integer; Size, ColR, ColG, ColB: real; Align: integer; const Text_: string): integer; overload;

      // button
      Procedure SetButtonLength(Length: Cardinal); //Function that Set Length of Button Array in one Step instead of register new Memory for every Button
      function AddButton(ThemeButton: TThemeButton): integer; overload;
      function AddButton(X, Y, W, H: real; const Name: String): integer; overload;
      function AddButton(X, Y, W, H: real; const Name: String; Typ: TTextureType; Reflection: Boolean): integer; overload;
      function AddButton(X, Y, W, H, ColR, ColG, ColB, Int, DColR, DColG, DColB, DInt: real; const Name: String; Typ: TTextureType; Reflection: Boolean; ReflectionSpacing, DeSelectReflectionSpacing: Real): integer; overload;
      procedure ClearButtons;
      procedure AddButtonText(AddX, AddY: real; const AddText: string); overload;
      procedure AddButtonText(AddX, AddY: real; ColR, ColG, ColB: real; const AddText: string); overload;
      procedure AddButtonText(AddX, AddY: real; ColR, ColG, ColB: real; Font: integer; Size: integer; Align: integer; const AddText: string); overload;
      procedure AddButtonText(CustomButton: TButton; AddX, AddY: real; ColR, ColG, ColB: real; Font: integer; Size: integer; Align: integer; const AddText: string); overload;

      // select
      function AddSelect(ThemeSelect: TThemeSelect; var Data: integer; Values: array of string): integer; overload;
      function AddSelect(X, Y, W, H, SkipX, ColR, ColG, ColB, Int, DColR, DColG, DColB, DInt,
        TColR, TColG, TColB, TInt, TDColR, TDColG, TDColB, TDInt,
        SBGColR, SBGColG, SBGColB, SBGInt, SBGDColR, SBGDColG, SBGDColB, SBGDInt,
        STColR, STColG, STColB, STInt, STDColR, STDColG, STDColB, STDInt: real;
        const Name: String; Typ: TTextureType; const SBGName: String; SBGTyp: TTextureType;
        const Caption: string; var Data: integer): integer; overload;
      procedure AddSelectOption(AddX, AddY: real; const AddText: string); overload;
      procedure AddSelectOption(SelectNo: Cardinal; AddX, AddY: real; const AddText: string); overload;
      procedure UpdateSelectOptions(ThemeSelect: TThemeSelect; SelectNum: integer; Values: array of string; var Data: integer);

      // select slide
      function AddSelectSlide(ThemeSelectS: TThemeSelectSlide; var Data: integer; Values: array of string): integer; overload;
      function AddSelectSlide(X, Y, W, H, SkipX, SBGW, ColR, ColG, ColB, Int, DColR, DColG, DColB, DInt,
        TColR, TColG, TColB, TInt, TDColR, TDColG, TDColB, TDInt,
        SBGColR, SBGColG, SBGColB, SBGInt, SBGDColR, SBGDColG, SBGDColB, SBGDInt,
        STColR, STColG, STColB, STInt, STDColR, STDColG, STDColB, STDInt: real;
        const Name: String; Typ: TTextureType; const SBGName: String; SBGTyp: TTextureType;
        const Caption: string; var Data: integer): integer; overload;
      procedure AddSelectSlideOption(const AddText: string); overload;
      procedure AddSelectSlideOption(SelectNo: Cardinal; const AddText: string); overload;
      procedure UpdateSelectSlideOptions(ThemeSelectSlide: TThemeSelectSlide; SelectNum: integer; Values: array of string; var Data: integer);


//      function AddWidget(X, Y : UInt16; WidgetSrc : PSDL_Surface): Int16;
//      procedure ClearWidgets(MinNumber : Int16);
      procedure FadeTo(Screen: PMenu); overload;
      procedure FadeTo(Screen: PMenu; aSound: TAudioPlaybackStream); overload;
      //popup hack
      procedure CheckFadeTo(Screen: PMenu; msg: String);

      function DrawBG: boolean; virtual;
      function DrawFG: boolean; virtual;
      function Draw: boolean; virtual;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown : Boolean): Boolean; virtual;
      // FIXME: ParseMouse is not implemented in any subclass and not even used anywhere in the code
      //   -> do this before activation of this method
      //function ParseMouse(Typ: integer; X: integer; Y: integer): Boolean; virtual; abstract;
      function InRegion(X1, Y1, X2, Y2, X, Y: real): Boolean;
      function InStaticRegion(StaticNr: integer; X, Y: integer): Boolean;
      procedure onShow; virtual;
      procedure onShowFinish; virtual;
      procedure onHide; virtual;

      procedure SetAnimationProgress(Progress: real); virtual;

      function IsSelectable(Int: Cardinal): Boolean;
      
      procedure InteractNext; virtual;
      procedure InteractCustom(CustomSwitch: integer); virtual;
      procedure InteractPrev; virtual;
      procedure InteractInc; virtual;
      procedure InteractDec; virtual;

      procedure AddBox(X, Y, W, H: real);
  end;

const
  pmMove = 1;
  pmClick = 2;
  pmUnClick = 3;

  iButton = 0; // interaction type
  iSelect = 1;
  iText = 2;
  iSelectS = 3;
  iBCollectionChild = 5;

//  fBlack = 0; // fade type
//  fWhite = 1;

implementation

uses UCommon,
     ULog,
     UMain,
     UDrawTexture,
     UGraphic,
     UDisplay,
     UCovers,
     USkins;

destructor TMenu.Destroy;
begin
  inherited;
end;

constructor TMenu.Create;
begin
  Fade := 0;//fWhite;

  SetLength(Static, 0);
  SetLength(Button, 0);

  BackImg.TexNum := -1;

  //Set ButtonPos to Autoset Length
  ButtonPos := -1;


  VideoPlayback.Init;
end;
{
constructor TMenu.Create(Back: String);
begin
  inherited Create;

  if Back <> '' then begin
//    BackImg := Texture.LoadTexture(true, PChar(Back), 'JPG', TEXTURE_TYPE_PLAIN, 0);
    BackImg := Texture.LoadTexture(PChar(Back), 'JPG', TEXTURE_TYPE_PLAIN, 0); // new theme system
    BackImg.W := 800;//640;
    BackImg.H := 600;//480;
    BackW := 1;
    BackH := 1;
  end else
    BackImg.TexNum := -1;

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

function RGBFloatToInt(R, G, B: Double): Cardinal;
begin
  Result := (Trunc(255 * R) shl 16) or
            (Trunc(255 * G) shl  8) or
             Trunc(255 * B);
end;

procedure TMenu.AddInteraction(Typ, Num: integer);
var
  IntNum:   integer;
begin
  IntNum := Length(Interactions);
  SetLength(Interactions, IntNum+1);
  Interactions[IntNum].Typ := Typ;
  Interactions[IntNum].Num := Num;
  Interaction := 0;
end;

procedure TMenu.SetInteraction(Num: integer);
var
  OldNum, OldTyp:   integer;
  NewNum, NewTyp:   integer;
begin
  // set inactive
  OldNum := Interactions[Interaction].Num;
  OldTyp := Interactions[Interaction].Typ;

  NewNum := Interactions[Num].Num;
  NewTyp := Interactions[Num].Typ;

  case OldTyp of
    iButton:  Button[OldNum].Selected := False;
    iSelect:  Selects[OldNum].Selected := False;
    iText:    Text[OldNum].Selected := False;
    iSelectS: SelectsS[OldNum].Selected := False;
    //Button Collection Mod
    iBCollectionChild:
      begin
        Button[OldNum].Selected := False;
        
        //Deselect Collection if Next Button is Not from Collection
        if (NewTyp <> iButton) Or (Button[NewNum].Parent <> Button[OldNum].Parent) then
          ButtonCollection[Button[OldNum].Parent-1].Selected := False;
      end;
  end;

  // set active
  SelInteraction := Num;
  case NewTyp of
    iButton:  Button[NewNum].Selected := True;
    iSelect:  Selects[NewNum].Selected := True;
    iText:    Text[NewNum].Selected := True;
    iSelectS: SelectsS[NewNum].Selected := True;

    //Button Collection Mod
    iBCollectionChild:
      begin
        Button[NewNum].Selected := True;
        ButtonCollection[Button[NewNum].Parent-1].Selected := True;
      end;
  end;
end;

//----------------------
//LoadFromTheme - Load BG, Texts, Statics and
//Button Collections from ThemeBasic
//----------------------
procedure TMenu.LoadFromTheme(const ThemeBasic: TThemeBasic);
var
  I: Integer;
begin
  //Add Button Collections (Set Button CollectionsLength)
  //Button Collections are Created when the first ChildButton is Created
  PrepareButtonCollections(ThemeBasic.ButtonCollection);

  //Add Background
  AddBackground(ThemeBasic.Background.Tex);

  //Add Statics and Texts
  for I := 0 to High(ThemeBasic.Static) do
    AddStatic(ThemeBasic.Static[I]);

  for I := 0 to High(ThemeBasic.Text) do
    AddText(ThemeBasic.Text[I]);
end;

procedure TMenu.AddBackground(Name: string);
//var
//  lFileName : string;
begin
  if Name <> '' then
  begin
    fFileName := Skin.GetTextureFileName(Name);
    fFileName := AdaptFilePaths( fFileName );

    if fileexists( fFileName ) then
    begin
      BackImg   := Texture.GetTexture( fFileName , TEXTURE_TYPE_PLAIN);

      if ( BackImg.TexNum < 1 )  then
      begin
        if VideoPlayback.Open( fFileName ) then
          VideoPlayback.Play;
      end;

      BackImg.W := 800;
      BackImg.H := 600;
      BackW     := 1;
      BackH     := 1;
    end;
  end;
end;

//----------------------
//PrepareButtonCollections:
//Add Button Collections (Set Button CollectionsLength)
//----------------------
procedure TMenu.PrepareButtonCollections(const Collections: AThemeButtonCollection);
var
  I: Integer;
begin
  SetLength(ButtonCollection, Length(Collections));
  For I := 0 to High(ButtonCollection) do
    AddButtonCollection(Collections[I], I);
end;

//----------------------
//AddButtonCollection:
//Create a Button Collection;
//----------------------
procedure TMenu.AddButtonCollection(const ThemeCollection: TThemeButtonCollection; Const Num: Byte);
var
  BT, BTLen: Integer;
  TempCol, TempDCol: Cardinal;

begin
  if (Num > High(ButtonCollection)) then
    exit;

  TempCol := 0;

  // colorize hack
  if (ThemeCollection.Style.Typ = TEXTURE_TYPE_COLORIZED) then
  begin
    TempCol  := RGBFloatToInt(ThemeCollection.Style.ColR, ThemeCollection.Style.ColG, ThemeCollection.Style.ColB);
    TempDCol := RGBFloatToInt(ThemeCollection.Style.DColR, ThemeCollection.Style.DColG, ThemeCollection.Style.DColB);
    // give encoded color to loadtexture
    ButtonCollection[Num] := TButtonCollection.Create(
      Texture.LoadTexture(Skin.GetTextureFileName(ThemeCollection.Style.Tex), TEXTURE_TYPE_COLORIZED, TempCol),
      Texture.LoadTexture(Skin.GetTextureFileName(ThemeCollection.Style.Tex), TEXTURE_TYPE_COLORIZED, TempDCol));

  //  Button[Result] := TButton.Create(Texture.LoadTexture(PChar(Name), PChar(Format), PChar(Typ), ((((TempR2 shl 8) or TempG2) shl 8)or TempB2))); // use cache texture
  end
  else
    ButtonCollection[Num] := TButtonCollection.Create(Texture.GetTexture(Skin.GetTextureFileName(ThemeCollection.Style.Tex), ThemeCollection.Style.Typ, true)); // use cache texture

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
  if (ThemeCollection.Style.Typ <> TEXTURE_TYPE_COLORIZED) then begin
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
    ButtonCollection[Num].FadeTex := Texture.LoadTexture(
      PChar(Skin.GetTextureFileName(ThemeCollection.Style.FadeTex)), TEXTURE_TYPE_COLORIZED, TempCol)
  else
    ButtonCollection[Num].FadeTex := Texture.GetTexture(Skin.GetTextureFileName(ThemeCollection.Style.FadeTex), ThemeCollection.Style.Typ, true);
  ButtonCollection[Num].FadeTexPos := ThemeCollection.Style.FadeTexPos;


  BTLen := Length(ThemeCollection.Style.Text);
  for BT := 0 to BTLen-1 do begin
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
    ThemeStatic.TexX1, ThemeStatic.TexY1, ThemeStatic.TexX2, ThemeStatic.TexY2,
    Skin.GetTextureFileName(ThemeStatic.Tex),
    ThemeStatic.Typ, $FFFFFF, ThemeStatic.Reflection, ThemeStatic.Reflectionspacing);
end;

function TMenu.AddStatic(X, Y, W, H: real; const Name: string): integer;
begin
  Result := AddStatic(X, Y, W, H, Name, TEXTURE_TYPE_PLAIN);
end;

function TMenu.AddStatic(X, Y, W, H: real; ColR, ColG, ColB: real; const Name: string; Typ: TTextureType): integer;
var
  StatNum:  integer;
begin
  Result := AddStatic(X, Y, W, H, ColR, ColG, ColB, Name, Typ, $FFFFFF);
end;

function TMenu.AddStatic(X, Y, W, H, Z: real; ColR, ColG, ColB: real; const Name: string; Typ: TTextureType): integer;
var
  StatNum:  integer;
begin
  Result := AddStatic(X, Y, W, H, Z, ColR, ColG, ColB, Name, Typ, $FFFFFF);
end;

function TMenu.AddStatic(X, Y, W, H: real; const Name: string; Typ: TTextureType): integer;
var
  StatNum:  integer;
begin
  // adds static
  StatNum := Length(Static);
  SetLength(Static, StatNum + 1);
//  Static[StatNum] := TStatic.Create(Texture.LoadTexture(PChar(Name), PChar(Format), Typ, $FF00FF)); // $FFFFFF
//  Static[StatNum] := TStatic.Create(Texture.LoadTexture(Skin.SkinReg, PChar(Name), PChar(Format), Typ, $FF00FF)); // new skin system
  Static[StatNum] := TStatic.Create(Texture.LoadTexture(Name, Typ, $FF00FF)); // new skin

  // configures static
  Static[StatNum].Texture.X := X;
  Static[StatNum].Texture.Y := Y;
  Static[StatNum].Texture.W := W;
  Static[StatNum].Texture.H := H;
  Static[StatNum].Visible := true;
  Result := StatNum;
end;

function TMenu.AddStatic(X, Y, W, H: real; ColR, ColG, ColB: real; const Name: string; Typ: TTextureType; Color: integer): integer;
var
  StatNum:  integer;
begin
  Result := AddStatic(X, Y, W, H, 0, ColR, ColG, ColB, Name, Typ, Color);
end;

function TMenu.AddStatic(X, Y, W, H, Z: real; ColR, ColG, ColB: real; const Name: string; Typ: TTextureType; Color: integer): integer;
begin
  Result := AddStatic(X, Y, W, H, Z, ColR, ColG, ColB, 0, 0, 1, 1, Name, Typ, Color, False, 0);
//
end;

function TMenu.AddStatic(X, Y, W, H, Z: real; ColR, ColG, ColB: real; TexX1, TexY1, TexX2, TexY2: real; const Name: string; Typ: TTextureType; Color: integer; Reflection: Boolean; ReflectionSpacing: Real): integer;
var
  StatNum:  integer;
begin
  // adds static
  StatNum := Length(Static);
  SetLength(Static, StatNum + 1);

  // colorize hack
  if (Typ = TEXTURE_TYPE_COLORIZED) then
  begin
    // give encoded color to loadtexture
    Static[StatNum] := TStatic.Create(Texture.LoadTexture(Name, Typ, RGBFloatToInt(ColR, ColG, ColB)));
  end
  else
    Static[StatNum] := TStatic.Create(Texture.LoadTexture(Name, Typ, Color)); // new skin
  //  Static[StatNum] := TStatic.Create(Texture.GetTexture(Name, Typ));

  // configures static
  Static[StatNum].Texture.X := X;
  Static[StatNum].Texture.Y := Y;
  Static[StatNum].Texture.W := W;
  Static[StatNum].Texture.H := H;
  Static[StatNum].Texture.Z := Z;
  if (Typ <> TEXTURE_TYPE_COLORIZED) then begin
    Static[StatNum].Texture.ColR := ColR;
    Static[StatNum].Texture.ColG := ColG;
    Static[StatNum].Texture.ColB := ColB;
  end;
  Static[StatNum].Texture.TexX1 := TexX1;
  Static[StatNum].Texture.TexY1 := TexY1;
  Static[StatNum].Texture.TexX2 := TexX2;
  Static[StatNum].Texture.TexY2 := TexY2;
  Static[StatNum].Texture.Alpha := 1;
  Static[StatNum].Visible := true;

  //ReflectionMod
  Static[StatNum].Reflection := Reflection;
  Static[StatNum].ReflectionSpacing := ReflectionSpacing;

  Result := StatNum;
end;

function TMenu.AddText(ThemeText: TThemeText): integer;
begin
  Result := AddText(ThemeText.X, ThemeText.Y, ThemeText.W, ThemeText.Font, ThemeText.Size,
    ThemeText.ColR, ThemeText.ColG, ThemeText.ColB, ThemeText.Align, ThemeText.Text);
end;

function TMenu.AddText(X, Y: real; const Text_: string): integer;
var
  TextNum:  integer;
begin
  // adds text
  TextNum := Length(Text);
  SetLength(Text, TextNum + 1);
  Text[TextNum] := TText.Create(X, Y, Text_);
  Result := TextNum;
end;

function TMenu.AddText(X, Y: real; Style: integer; Size, ColR, ColG, ColB: real; const Text: string): integer;
begin
  Result := AddText(X, Y, 0, Style, Size, ColR, ColG, ColB, 0, Text);
end;

function TMenu.AddText(X, Y, W: real; Style: integer; Size, ColR, ColG, ColB: real; Align: integer; const Text_: string): integer;
var
  TextNum:  integer;
begin
  // adds text
  TextNum := Length(Text);
  SetLength(Text, TextNum + 1);
  Text[TextNum] := TText.Create(X, Y, W, Style, Size, ColR, ColG, ColB, Align, Text_);
  Result := TextNum;
end;

//Function that Set Length of Button Array in one Step instead of register new Memory for every Button
Procedure TMenu.SetButtonLength(Length: Cardinal);
begin
  if (ButtonPos = -1) AND (Length > 0) then
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
  BT:     integer;
  BTLen:  integer;
  temp:   integer;
  TempR, TempG, TempB, TempR2, TempG2, TempB2: Cardinal;
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
  if (ThemeButton.Typ = TEXTURE_TYPE_COLORIZED) then begin
    Button[Result].FadeTex := Texture.LoadTexture(
      Skin.GetTextureFileName(ThemeButton.FadeTex), TEXTURE_TYPE_COLORIZED, RGBFloatToInt(ThemeButton.ColR, ThemeButton.ColG, ThemeButton.ColB));
  end
  else
    Button[Result].FadeTex := Texture.GetTexture(Skin.GetTextureFileName(ThemeButton.FadeTex), ThemeButton.Typ, true);
  Button[Result].FadeTexPos := ThemeButton.FadeTexPos;


  BTLen := Length(ThemeButton.Text);
  for BT := 0 to BTLen-1 do begin
    AddButtonText(ThemeButton.Text[BT].X, ThemeButton.Text[BT].Y,
      ThemeButton.Text[BT].ColR, ThemeButton.Text[BT].ColG, ThemeButton.Text[BT].ColB,
      ThemeButton.Text[BT].Font, ThemeButton.Text[BT].Size, ThemeButton.Text[BT].Align,
      ThemeButton.Text[BT].Text);
  end;

  //BAutton Collection Mod
  if (ThemeButton.Parent <> 0) then
  begin
    //If Collection Exists then Change Interaction to Child Button
    if (@ButtonCollection[ThemeButton.Parent-1] <> nil) then
    begin
      Interactions[High(Interactions)].Typ := iBCollectionChild;
      Button[Result].Visible := False;

      for BT := 0 to BTLen-1 do
        Button[Result].Text[BT].Alpha := 0;

      Button[Result].Parent := ThemeButton.Parent;
      if (ButtonCollection[ThemeButton.Parent-1].Fade) then
        Button[Result].Texture.Alpha := 0;
    end;
  end;
  Log.BenchmarkEnd(6);
  Log.LogBenchmark('====> Screen Options32', 6);
end;

function TMenu.AddButton(X, Y, W, H: real; const Name: String): integer;
begin
  Result := AddButton(X, Y, W, H, Name, TEXTURE_TYPE_PLAIN, False);
end;

function TMenu.AddButton(X, Y, W, H: real; const Name: String; Typ: TTextureType; Reflection: Boolean): integer;
begin
  Result := AddButton(X, Y, W, H, 1, 1, 1, 1, 1, 1, 1, 0.5, Name, TEXTURE_TYPE_PLAIN, Reflection, 15, 15);
end;

function TMenu.AddButton(X, Y, W, H, ColR, ColG, ColB, Int, DColR, DColG, DColB, DInt: real;
                         const Name: String; Typ: TTextureType;
                         Reflection: Boolean; ReflectionSpacing, DeSelectReflectionSpacing: Real): integer;
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
  //  Button[Result] := TButton.Create(Texture.GetTexture(Name, Typ));

  // check here for cache
  //  Texture.GetTexture(Name, Typ, false); // preloads textures and creates cahce mipmap when needed
  //  if Covers.CoverExists(Name) then
  // colorize hack
  if (Typ = TEXTURE_TYPE_COLORIZED) then
  begin
    // give encoded color to loadtexture
    Button[Result] := TButton.Create(Texture.LoadTexture(Name, Typ, RGBFloatToInt(ColR, ColG, ColB)),
                                     Texture.LoadTexture(Name, Typ, RGBFloatToInt(DColR, DColG, DColB)));

  //  Button[Result] := TButton.Create(Texture.LoadTexture(PChar(Name), PChar(Format), PChar(Typ), ((((TempR2 shl 8) or TempG2) shl 8)or TempB2))); // use cache texture
  end
  else

    Button[Result] := TButton.Create(Texture.GetTexture(Name, Typ, true)); // use cache texture
  //  else
  //    Button[Result] := TButton.Create(Texture.GetTexture(Name, Typ, false)); // don't use cache texture}

  // configures button
  Button[Result].X := X;
  Button[Result].Y := Y;
  Button[Result].W := W;
  Button[Result].H := H;
  if (Typ <> TEXTURE_TYPE_COLORIZED) then begin
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

  //Button Collection Mod
  Button[Result].Parent := 0;


  // adds interaction
  AddInteraction(iButton, Result);
  Interaction := 0;
end;

procedure TMenu.ClearButtons;
begin
  Setlength(Button, 0);
end;

// Method to draw our TMenu and all his child buttons
function TMenu.DrawBG: boolean;
var
  PetX:   integer;
  PetY:   integer;
begin

  BackImg.ColR := 1;
  BackImg.ColG := 1;
  BackImg.ColB := 1;
  BackImg.TexX1 := 0;
  BackImg.TexY1 := 0;
  BackImg.TexX2 := 1;
  BackImg.TexY2 := 1;
  if (BackImg.TexNum <> -1) then
  begin
  // does anyone know what these loops were for?
{    // draw texture with overlapping
    for PetY := 1 to BackH do
      for PetX := 1 to BackW do begin
        BackImg.X := (PetX-1)/BackW * 800; //640
        BackImg.Y := (PetY-1)/BackH * 600; //480
        DrawTexture(BackImg);
      end; // for PetX}
    {BackImg.X:=BackW;
    BackImg.Y:=BackW;  }
    BackImg.X := 0;
    BackImg.Y := 0;
    BackImg.Z := 0; // todo: eddie: to the opengl experts: please check this! On the mac z is not initialized???
    BackImg.W := 800;
    BackImg.H := 600;
    DrawTexture(BackImg);
  end; // if


//  if assigned( VideoPlayback ) then
  begin
    VideoPlayback.GetFrame( now() );
    VideoPlayback.DrawGL(2);
  end;

  Result := true;
end;

function TMenu.DrawFG: boolean;
var
  J:      Integer;
begin
  // We don't forget about newly implemented static for nice skin ...
  for J := 0 to Length(Static) - 1 do
    Static[J].Draw;

  // ... and slightly implemented menutext unit
  for J := 0 to Length(Text) - 1 do
    Text[J].Draw;


  //  Draw all ButtonCollections
  For J := 0 to High(ButtonCollection) do
    ButtonCollection[J].Draw;

  // Second, we draw all of our buttons
  for J := 0 to Length(Button) - 1 do
    Button[J].Draw;

  // Third, we draw all of our selects
  for J := 0 to Length(Selects) - 1 do
    Selects[J].Draw(1);

  for J := 0 to Length(SelectsS) - 1 do
    SelectsS[J].Draw;

 // Third, we draw all our widgets
//  for J := 0 to Length(WidgetsSrc) - 1 do
//    SDL_BlitSurface(WidgetsSrc[J], nil, ParentBackBuf, WidgetsRect[J]);
  Result := True;
end;

function TMenu.Draw: boolean;
begin
  DrawBG;
  DrawFG;
  Result := True;
end;

{function TMenu.GetNextScreen(): PMenu;
begin
  Result := NextScreen;
end;}

{function TMenu.AddWidget(X, Y : UInt16; WidgetSrc : PSDL_Surface): Int16;
var
  WidgetNum : Int16;

begin
  If (Assigned(WidgetSrc)) Then
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
end;}

{procedure TMenu.ClearWidgets(MinNumber : Int16);
var
  J : Int16;
begin
  For J := MinNumber to (Length(WidgetsSrc) - 1) do
  begin
    SDL_FreeSurface(WidgetsSrc[J]);
    dispose(WidgetsRect[J]);
  end;

  SetLength(WidgetsSrc, MinNumber);
  SetLength(WidgetsRect, MinNumber);
end;}

function TMenu.IsSelectable(Int: Cardinal): Boolean;
begin
  Result := True;
  Case Interactions[Int].Typ of
    //Button
    iButton: Result := Button[Interactions[Int].Num].Visible and Button[Interactions[Int].Num].Selectable;
    //Select
    iSelect: Result := True;
    //Select Slide
    iSelectS: Result := SelectsS[Interactions[Int].Num].Visible;

    //ButtonCollection Child
    iBCollectionChild:
      Result := (ButtonCollection[Button[Interactions[Int].Num].Parent - 1].FirstChild - 1 = Int) AND ((Interactions[Interaction].Typ <> iBCollectionChild) OR (Button[Interactions[Interaction].Num].Parent <> Button[Interactions[Int].Num].Parent));
  end;
end;

procedure TMenu.InteractNext;
var
  Int: Integer;
begin
  Int := Interaction;

  // change interaction as long as it's needed
  repeat
    Int := (Int + 1) Mod Length(Interactions);

    //If no Interaction is Selectable Simply Select Next
    if (Int = Interaction) then Break;

  Until IsSelectable(Int);

  //Set Interaction
  Interaction := Int;
end;


procedure TMenu.InteractPrev;
var
  Int: Integer;
begin
  Int := Interaction;

  // change interaction as long as it's needed
  repeat
    Int := Int - 1;
    if Int = -1 then Int := High(Interactions);

    //If no Interaction is Selectable Simply Select Next
    if (Int = Interaction) then Break;
  Until IsSelectable(Int);

  //Set Interaction
  Interaction := Int
end;


procedure TMenu.InteractCustom(CustomSwitch: integer);
var
  Num:    integer;
  Typ:    integer;
  Again:  boolean;
begin
  //Code Commented atm, because it needs to be Rewritten
  //it doesn't work with Button Collections
  {then begin
    CustomSwitch:= CustomSwitch*(-1);
    Again := true;
  // change interaction as long as it's needed
  while (Again = true) do begin
    Num := SelInteraction - CustomSwitch;
    if Num = -1 then Num := High(Interactions);
    Interaction := Num;
    Again := false; // reset, default to accept changing interaction

    // checking newly interacted element
    Num := Interactions[Interaction].Num;
    Typ := Interactions[Interaction].Typ;
    case Typ of
    iButton:
      begin
        if Button[Num].Selectable = false then Again := True;
      end;
    end; // case
  end; // while
  end
  else if num>0 then begin
    Again := true;
    // change interaction as long as it's needed
  while (Again = true) do begin
    Num := (Interaction + CustomSwitch) Mod Length(Interactions);
    Interaction := Num;
    Again := false; // reset, default to accept changing interaction


    // checking newly interacted element
    Num := Interactions[Interaction].Num;
    Typ := Interactions[Interaction].Typ;
    case Typ of
    iButton:
      begin
        if Button[Num].Selectable = false then Again := True;
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


//popup hack
procedure TMenu.CheckFadeTo(Screen: PMenu; msg: String);
begin
  Display.Fade := 0;
  Display.NextScreenWithCheck := Screen;
  Display.CheckOK:=False;
  ScreenPopupCheck.ShowPopup(msg);
end;

procedure TMenu.AddButtonText(AddX, AddY: real; const AddText: string);
begin
  AddButtonText(AddX, AddY, 1, 1, 1, AddText);
end;

procedure TMenu.AddButtonText(AddX, AddY: real; ColR, ColG, ColB: real; const AddText: string);
var
  Il:   integer;
begin
  with Button[High(Button)] do begin
    Il := Length(Text);
    SetLength(Text, Il+1);
    Text[Il] := TText.Create(X + AddX, Y + AddY, AddText);
    Text[Il].ColR := ColR;
    Text[Il].ColG := ColG;
    Text[Il].ColB := ColB;
    Text[Il].Int := 1;//0.5;
  end;
end;

procedure TMenu.AddButtonText(AddX, AddY: real; ColR, ColG, ColB: real; Font: integer; Size: integer; Align: integer; const AddText: string);
var
  Il:   integer;
begin
  with Button[High(Button)] do begin
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

procedure TMenu.AddButtonText(CustomButton: TButton; AddX, AddY: real; ColR, ColG, ColB: real; Font: integer; Size: integer; Align: integer; const AddText: string);
var
  Il:   integer;
begin
  with CustomButton do begin
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

function TMenu.AddSelect(ThemeSelect: TThemeSelect; var Data: integer; Values: array of string): integer;
var
  SO:     integer;
begin
  Result := AddSelect(ThemeSelect.X, ThemeSelect.Y, ThemeSelect.W, ThemeSelect.H, ThemeSelect.SkipX,
    ThemeSelect.ColR, ThemeSelect.ColG, ThemeSelect.ColB, ThemeSelect.Int,
    ThemeSelect.DColR, ThemeSelect.DColG, ThemeSelect.DColB, ThemeSelect.DInt,
    ThemeSelect.TColR, ThemeSelect.TColG, ThemeSelect.TColB, ThemeSelect.TInt,
    ThemeSelect.TDColR, ThemeSelect.TDColG, ThemeSelect.TDColB, ThemeSelect.TDInt,
    ThemeSelect.SBGColR, ThemeSelect.SBGColG, ThemeSelect.SBGColB, ThemeSelect.SBGInt,
    ThemeSelect.SBGDColR, ThemeSelect.SBGDColG, ThemeSelect.SBGDColB, ThemeSelect.SBGDInt,
    ThemeSelect.STColR, ThemeSelect.STColG, ThemeSelect.STColB, ThemeSelect.STInt,
    ThemeSelect.STDColR, ThemeSelect.STDColG, ThemeSelect.STDColB, ThemeSelect.STDInt,
    Skin.GetTextureFileName(ThemeSelect.Tex), TEXTURE_TYPE_COLORIZED,
    Skin.GetTextureFileName(ThemeSelect.TexSBG), TEXTURE_TYPE_COLORIZED,
    ThemeSelect.Text, Data);
  for SO := 0 to High(Values) do
    AddSelectOption(ThemeSelect.X + ThemeSelect.W + ThemeSelect.SkipX + SO * 100 + 20, ThemeSelect.Y + 20, Values[SO]);
end;

function TMenu.AddSelect(X, Y, W, H, SkipX, ColR, ColG, ColB, Int, DColR, DColG, DColB, DInt,
  TColR, TColG, TColB, TInt, TDColR, TDColG, TDColB, TDInt,
  SBGColR, SBGColG, SBGColB, SBGInt, SBGDColR, SBGDColG, SBGDColB, SBGDInt,
  STColR, STColG, STColB, STInt, STDColR, STDColG, STDColB, STDInt: real;
  const Name: String; Typ: TTextureType; const SBGName: String; SBGTyp: TTextureType;
  const Caption: string; var Data: integer): integer;
var
  S:    integer;
begin
  S := Length(Selects);
  SetLength(Selects, S + 1);
  Selects[S] := TSelect.Create;

  if (Typ = TEXTURE_TYPE_COLORIZED) then
    Selects[S].Texture := Texture.LoadTexture(Name, Typ, RGBFloatToInt(ColR, ColG, ColB))
  else
    Selects[S].Texture := Texture.GetTexture(Name, Typ);
  Selects[S].X := X;
  Selects[S].Y := Y;
  Selects[S].W := W;
  Selects[S].H := H;
  Selects[S].ColR := ColR;
  Selects[S].ColG := ColG;
  Selects[S].ColB := ColB;
  Selects[S].Int := Int;
  Selects[S].DColR := DColR;
  Selects[S].DColG := DColG;
  Selects[S].DColB := DColB;
  Selects[S].DInt := DInt;

  if (SBGTyp = TEXTURE_TYPE_COLORIZED) then
    Selects[S].TextureSBG := Texture.LoadTexture(SBGName, SBGTyp, RGBFloatToInt(SBGColR, SBGColG, SBGColB))
  else
    Selects[S].TextureSBG := Texture.GetTexture(SBGName, SBGTyp);
  Selects[S].TextureSBG.X := X + W + SkipX;
  Selects[S].TextureSBG.Y := Y;
  Selects[S].TextureSBG.W := 450;
  Selects[S].TextureSBG.H := H;
  Selects[S].SBGColR := SBGColR;
  Selects[S].SBGColG := SBGColG;
  Selects[S].SBGColB := SBGColB;
  Selects[S].SBGInt := SBGInt;
  Selects[S].SBGDColR := SBGDColR;
  Selects[S].SBGDColG := SBGDColG;
  Selects[S].SBGDColB := SBGDColB;
  Selects[S].SBGDInt := SBGDInt;

  Selects[S].Text.X := X + 20;
  Selects[S].Text.Y := Y + 20;
  Selects[S].Text.Text := Caption;
  Selects[S].Text.Size := 10;
  Selects[S].Text.Visible := true;
  Selects[S].TColR := TColR;
  Selects[S].TColG := TColG;
  Selects[S].TColB := TColB;
  Selects[S].TInt := TInt;
  Selects[S].TDColR := TDColR;
  Selects[S].TDColG := TDColG;
  Selects[S].TDColB := TDColB;
  Selects[S].TDInt := TDInt;

  Selects[S].STColR := STColR;
  Selects[S].STColG := STColG;
  Selects[S].STColB := STColB;
  Selects[S].STInt := STInt;
  Selects[S].STDColR := STDColR;
  Selects[S].STDColG := STDColG;
  Selects[S].STDColB := STDColB;
  Selects[S].STDInt := STDInt;

  // new
  Selects[S].Texture.TexX1 := 0;
  Selects[S].Texture.TexY1 := 0;
  Selects[S].Texture.TexX2 := 1;
  Selects[S].Texture.TexY2 := 1;
  Selects[S].TextureSBG.TexX1 := 0;
  Selects[S].TextureSBG.TexY1 := 0;
  Selects[S].TextureSBG.TexX2 := 1;
  Selects[S].TextureSBG.TexY2 := 1;

  // Sets Data to copy the value of selectops to global value;
  Selects[S].PData := @Data;

  // Sets default value of selectopt from Data;
  Selects[S].SelectedOption := Data;

  // Disables default selection
  Selects[S].SetSelect(false);

  // adds interaction
  AddInteraction(iSelect, S);
end;

procedure TMenu.AddSelectOption(AddX, AddY: real; const AddText: string);
begin
  AddSelectOption (High(Selects), AddX, AddY, AddText);
end;

procedure TMenu.AddSelectOption(SelectNo: Cardinal; AddX, AddY: real; const AddText: string);
var
  SO:     integer;
begin
  SO := Length(Selects[SelectNo].TextOpt);
  SetLength(Selects[SelectNo].TextOpt, SO + 1);

  Selects[SelectNo].TextOpt[SO] := TText.Create;

  Selects[SelectNo].TextOpt[SO].X := AddX;
  Selects[SelectNo].TextOpt[SO].Y := AddY;
  Selects[SelectNo].TextOpt[SO].Text := AddText;
  Selects[SelectNo].TextOpt[SO].Size := 10;
  Selects[SelectNo].TextOpt[SO].ColR := Selects[SelectNo].STDColR;
  Selects[SelectNo].TextOpt[SO].ColG := Selects[SelectNo].STDColG;
  Selects[SelectNo].TextOpt[SO].ColB := Selects[SelectNo].STDColB;
  Selects[SelectNo].TextOpt[SO].Int := Selects[SelectNo].STDInt;
  Selects[SelectNo].TextOpt[SO].Visible := true;

  if SO = Selects[SelectNo].PData^ then Selects[SelectNo].SelectedOption := SO;
end;

procedure TMenu.UpdateSelectOptions(ThemeSelect: TThemeSelect; SelectNum: integer; Values: array of string; var Data: integer);
var
  SO:   integer;
begin
  SetLength(Selects[SelectNum].TextOpt, 0);
  for SO := 0 to High(Values) do
    AddSelectOption(SelectNum, ThemeSelect.X + ThemeSelect.W + ThemeSelect.SkipX + SO * 100 + 20, ThemeSelect.Y + 20, Values[SO]);
end;

function TMenu.AddSelectSlide(ThemeSelectS: TThemeSelectSlide; var Data: integer; Values: array of string): integer;
var
  SO:     integer;
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
    Skin.GetTextureFileName(ThemeSelectS.Tex), TEXTURE_TYPE_COLORIZED,
    Skin.GetTextureFileName(ThemeSelectS.TexSBG), TEXTURE_TYPE_COLORIZED,
    ThemeSelectS.Text, Data);
  for SO := 0 to High(Values) do
    AddSelectSlideOption(Values[SO]);

  SelectsS[High(SelectsS)].Text.Size := ThemeSelectS.TextSize;

  SelectsS[High(SelectsS)].Texture.Z := ThemeSelectS.Z;
  SelectsS[High(SelectsS)].TextureSBG.Z := ThemeSelectS.Z;

  //Generate Lines
  SelectsS[High(SelectsS)].GenLines;

  SelectsS[High(SelectsS)].SelectedOption := SelectsS[High(SelectsS)].SelectOptInt; // refresh
end;

function TMenu.AddSelectSlide(X, Y, W, H, SkipX, SBGW, ColR, ColG, ColB, Int, DColR, DColG, DColB, DInt,
  TColR, TColG, TColB, TInt, TDColR, TDColG, TDColB, TDInt,
  SBGColR, SBGColG, SBGColB, SBGInt, SBGDColR, SBGDColG, SBGDColB, SBGDInt,
  STColR, STColG, STColB, STInt, STDColR, STDColG, STDColB, STDInt: real;
  const Name: String; Typ: TTextureType; const SBGName: String; SBGTyp: TTextureType;
  const Caption: string; var Data: integer): integer;
var
  S:    integer;
  I:    integer;
begin
  S := Length(SelectsS);
  SetLength(SelectsS, S + 1);
  SelectsS[S] := TSelectSlide.Create;

  if (Typ = TEXTURE_TYPE_COLORIZED) then
    SelectsS[S].Texture := Texture.LoadTexture(Name, Typ, RGBFloatToInt(ColR, ColG, ColB))
  else
    SelectsS[S].Texture := Texture.GetTexture(Name, Typ);
  SelectsS[S].X := X;
  SelectsS[S].Y := Y;
  SelectsS[S].W := W;
  SelectsS[S].H := H;

  SelectsS[S].ColR := ColR;
  SelectsS[S].ColG := ColG;
  SelectsS[S].ColB := ColB;
  SelectsS[S].Int := Int;
  SelectsS[S].DColR := DColR;
  SelectsS[S].DColG := DColG;
  SelectsS[S].DColB := DColB;
  SelectsS[S].DInt := DInt;

  if (SBGTyp = TEXTURE_TYPE_COLORIZED) then
    SelectsS[S].TextureSBG := Texture.LoadTexture(SBGName, SBGTyp, RGBFloatToInt(SBGColR, SBGColG, SBGColB))
  else
    SelectsS[S].TextureSBG := Texture.GetTexture(SBGName, SBGTyp);
  SelectsS[S].TextureSBG.X := X + W + SkipX;
  SelectsS[S].TextureSBG.Y := Y;
  //SelectsS[S].TextureSBG.W := 450;
  SelectsS[S].SBGW := SBGW;
  SelectsS[S].TextureSBG.H := H;
  SelectsS[S].SBGColR := SBGColR;
  SelectsS[S].SBGColG := SBGColG;
  SelectsS[S].SBGColB := SBGColB;
  SelectsS[S].SBGInt := SBGInt;
  SelectsS[S].SBGDColR := SBGDColR;
  SelectsS[S].SBGDColG := SBGDColG;
  SelectsS[S].SBGDColB := SBGDColB;
  SelectsS[S].SBGDInt := SBGDInt;

  SelectsS[S].Text.X := X + 20;
  SelectsS[S].Text.Y := Y + (SelectsS[S].TextureSBG.H / 2) - 15;
  SelectsS[S].Text.Text := Caption;
  SelectsS[S].Text.Size := 10;
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
  SelectsS[S].TextOpt[0].Size := 10;
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
  for I := 0 to 2 do begin
    SelectsS[S].TextOpt[I].X := SelectsS[S].TextureSBG.X + 20 + (50 + 20) + (150 - 20) * I;
    SelectsS[S].TextOpt[I].Y := SelectsS[S].TextureSBG.Y + 20;
    SelectsS[S].TextOpt[I].Text := IntToStr(I+1);
    SelectsS[S].TextOpt[I].Size := 10;
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

procedure TMenu.AddSelectSlideOption(const AddText: string);
begin
  AddSelectSlideOption(High(SelectsS), AddText);
end;

procedure TMenu.AddSelectSlideOption(SelectNo: Cardinal; const AddText: string);
var
  SO:     integer;
begin
  SO := Length(SelectsS[SelectNo].TextOptT);
  
  SetLength(SelectsS[SelectNo].TextOptT, SO + 1);
  SelectsS[SelectNo].TextOptT[SO] := AddText;

  //SelectsS[S].SelectedOption := SelectsS[S].SelectOptInt; // refresh

  //if SO = Selects[S].PData^ then Selects[S].SelectedOption := SO;
end;

procedure TMenu.UpdateSelectSlideOptions(ThemeSelectSlide: TThemeSelectSlide; SelectNum: integer; Values: array of string; var Data: integer);
var
  SO:   integer;
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

function TMenu.InRegion(X1, Y1, X2, Y2, X, Y: real): Boolean;
begin
  Result := false;
  X1 := X1 * RenderW/640;
  X2 := X2 * RenderW/640;
  Y1 := Y1 * RenderH/480;
  Y2 := Y2 * RenderH/480;
  if (X >= X1) and (X <= X2) and (Y >= Y1) and (Y <= Y2) then
    Result := true;
end;

function TMenu.InStaticRegion(StaticNr: integer; X, Y: integer): Boolean;
begin
  Result := InRegion(Static[StaticNr].Texture.X,
    Static[StaticNr].Texture.Y,
    Static[StaticNr].Texture.X + Static[StaticNr].Texture.W - 1,
    Static[StaticNr].Texture.Y + Static[StaticNr].Texture.H - 1,
    X, Y);
end;

procedure TMenu.InteractInc;
var
  Num:    integer;
  Value:  integer;
begin
  case Interactions[Interaction].Typ of
    iSelect:  begin
        Num := Interactions[Interaction].Num;
        Value := Selects[Num].SelectedOption;
        Value := (Value + 1) Mod (Length(Selects[Num].TextOpt));
        Selects[Num].SelectedOption := Value;
      end;
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
        For Num := 1 to High(Button) do
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
  Num:    integer;
  Value:  integer;
begin
  case Interactions[Interaction].Typ of
    iSelect:  begin
        Num := Interactions[Interaction].Num;
        Value := Selects[Num].SelectedOption;
        Value := Value - 1;
        if Value = -1 then
          Value := High(Selects[Num].TextOpt);
        Selects[Num].SelectedOption := Value;
      end;
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
        For Num := High(Button) downto 1 do
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
    //interact Prev if there is Nothing to Change
    else
      begin
        InteractPrev;
        //If ButtonCollection with more than 1 Entry then Select Last Entry
        if (Button[Interactions[Interaction].Num].Parent <> 0) AND (ButtonCollection[Button[Interactions[Interaction].Num].Parent-1].CountChilds > 1) then
        begin
          //Select Last Child
          For Num := High(Button) downto 1 do
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

procedure TMenu.onShow;
begin
  // nothing
(*
  if fileexists( fFileName ) then
  begin
    if VideoPlayback.Open( fFileName ) then
      VideoPlayback.Play;
  end;
*)
end;

procedure TMenu.onShowFinish;
begin
  // nothing
end;

function TMenu.WideCharUpperCase(wchar: WideChar) : WideString;
begin
  // On Linux and MacOSX the cwstring unit is necessary for Unicode function-calls.
  // Otherwise you will get an EIntOverflow exception (thrown by unimplementedwidestring()).

  // The FPC implementation of WideUpperCase returns nil if wchar is #0 (e.g. if an arrow key is pressed)
  if (wchar <> #0) then
    Result := WideUpperCase(wchar)
  else
    Result := #0;
end;

procedure TMenu.onHide;
begin
  // nothing
end;

function TMenu.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
begin
  // nothing
  Result := true;
end;

procedure TMenu.SetAnimationProgress(Progress: real);
begin
  // nothing
end;

end.

