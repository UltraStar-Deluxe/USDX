unit UThemes;

interface

uses
IniFiles, SysUtils, Classes;

type
  TRGB = record
    R:    single;
    G:    single;
    B:    single;
  end;

  {TSkin = record
    GrayLeft:       string;
    GrayMid:        string;
    GrayRight:      string;

    NoteBGLeft:     string;
    NoteBGMid:      string;
    NoteBGRight:    string;

    NoteStar:       string;

    Ball:           string;



    //SingBar Mod
    SingBarBack:    string;
    SingBarBar:     string;
    SingBarFront:   string;
    //end Singbar Mod

    //PhrasenBonus - Line Bonus Mod
    SingLineBonusBack:  string;
    //PhrasenBonus - Line Bonus Mod



    WelcomeBG:      string;

    Background:     string;
    ScoreBG:        string;
    MainStart:      string;
    MainEditor:     string;
    MainOptions:    string;
    MainExit:       string;
    MainBar:        string;
    Cursor:         string;

    SongFade:       string;
    SongCover:      string;
    SongSelection:  string;

    SelectSong:     string;
    Button:         string;
    Bar:            string;
    P:              string;
    Arrow:          string;
    Arrow2:         string;
    ButtonF:        string;
    Star:           string;
    Line:           string;

//    ThemePath:    string;
    SkinReg:      boolean;
    SkinName:     string;
    SkinPath:     string;
    SkinColor:    integer;
  end;}

  TThemeBackground = record
    Tex:  string;
  end;

  TThemeStatic = record
    X:      integer;
    Y:      integer;
    Z:      real;
    W:      integer;
    H:      integer;
    Color:  string;
    ColR:   real;
    ColG:   real;
    ColB:   real;
    Tex:    string;
    Typ:    string;
    TexX1:  real;
    TexY1:  real;
    TexX2:  real;
    TexY2:  real;
  end;
  AThemeStatic = array of TThemeStatic;

  TThemeText = record
    X:      integer;
    Y:      integer;
    W:      integer;
    Color:  string;
    ColR:   real;
    ColG:   real;
    ColB:   real;
    Font:   integer;
    Size:   integer;
    Align:  integer;
    Text:   string;
  end;
  AThemeText = array of TThemeText;

  TThemeButton = record
    Text:   AThemeText;
    X:      integer;
    Y:      integer;
    W:      integer;
    H:      integer;
    Color:  string;
    ColR:   real;
    ColG:   real;
    ColB:   real;
    Int:    real;
    DColor: string;
    DColR:  real;
    DColG:  real;
    DColB:  real;
    DInt:   real;
    Tex:    string;
    Typ:    string;
    //Reflection Mod
    Reflection: Boolean;
  end;

  TThemeSelect = record
    Tex:    string;
    TexSBG: string;
    X:      integer;
    Y:      integer;
    W:      integer;
    H:      integer;
    Text:   string;
    ColR,  ColG,  ColB,  Int:     real;
    DColR, DColG, DColB, DInt:    real;
    TColR,  TColG,  TColB,  TInt:     real;
    TDColR, TDColG, TDColB, TDInt:    real;
    SBGColR,  SBGColG,  SBGColB,  SBGInt:     real;
    SBGDColR, SBGDColG, SBGDColB, SBGDInt:    real;
    STColR,  STColG,  STColB,  STInt:     real;
    STDColR, STDColG, STDColB, STDInt:    real;
    SkipX:    integer;
  end;

  TThemeSelectSlide = record
    Tex:    string;
    TexSBG: string;
    X:      integer;
    Y:      integer;
    W:      integer;
    H:      integer;
    
    //SBGW Mod
    SBGW:   integer;

    Text:   string;
    ColR,  ColG,  ColB,  Int:     real;
    DColR, DColG, DColB, DInt:    real;
    TColR,  TColG,  TColB,  TInt:     real;
    TDColR, TDColG, TDColB, TDInt:    real;
    SBGColR,  SBGColG,  SBGColB,  SBGInt:     real;
    SBGDColR, SBGDColG, SBGDColB, SBGDInt:    real;
    STColR,  STColG,  STColB,  STInt:     real;
    STDColR, STDColG, STDColB, STDInt:    real;
    SkipX:    integer;
  end;

  TThemeBasic = class
    Background:       TThemeBackground;
    Text:             AThemeText;
    Static:           AThemeStatic;
  end;

  TThemeLoading = class(TThemeBasic)
  end;

  TThemeMain = class(TThemeBasic)
    ButtonSolo:       TThemeButton;
//    ButtonMulti:      TThemeButton;
    ButtonEditor:     TThemeButton;
    ButtonOptions:    TThemeButton;
    ButtonExit:       TThemeButton;

    TextDescription:      TThemeText;
    TextDescriptionLong:  TThemeText;
    Description:          array[0..4] of string;
    DescriptionLong:      array[0..4] of string;
  end;

  TThemeName = class(TThemeBasic)
    ButtonPlayer:     array[1..6] of TThemeButton;
  end;

  TThemeLevel = class(TThemeBasic)
    ButtonEasy:       TThemeButton;
    ButtonMedium:     TThemeButton;
    ButtonHard:       TThemeButton;
  end;

  TThemeSong = class(TThemeBasic)
    TextArtist:       TThemeText;
    TextTitle:        TThemeText;
    TextNumber:       TThemeText;

    //Show Cat in TopLeft Mod
    TextCat:          TThemeText;
    StaticCat:        TThemeStatic;

    //Cover Mod
    Cover: record
      Reflections: Boolean;
      X: Integer;
      Y: Integer;
      Z: Integer;
      W: Integer;
      H: Integer;
      Style: Integer;
      end;

    //Equalizer Mod
    Equalizer: record
      Visible: Boolean;
      Direction: Boolean;
      Alpha: real;
      X: Integer;
      Y: Integer;
      Z: Real;
      W: Integer;
      H: Integer;
      Space: Integer;
      Bands: Integer;
      Length: Integer;
      ColR, ColG, ColB: Real;
      end;

    //Party Mode
    StaticTeam1Joker1: TThemeStatic;
    StaticTeam1Joker2: TThemeStatic;
    StaticTeam1Joker3: TThemeStatic;
    StaticTeam1Joker4: TThemeStatic;
    StaticTeam1Joker5: TThemeStatic;
    StaticTeam2Joker1: TThemeStatic;
    StaticTeam2Joker2: TThemeStatic;
    StaticTeam2Joker3: TThemeStatic;
    StaticTeam2Joker4: TThemeStatic;
    StaticTeam2Joker5: TThemeStatic;
    StaticTeam3Joker1: TThemeStatic;
    StaticTeam3Joker2: TThemeStatic;
    StaticTeam3Joker3: TThemeStatic;
    StaticTeam3Joker4: TThemeStatic;
    StaticTeam3Joker5: TThemeStatic;
  end;

   TThemeSing = class(TThemeBasic)
    StaticP1:         TThemeStatic;
    StaticP1ScoreBG:  TThemeStatic; //Static for ScoreBG
    TextP1:           TThemeText;
    TextP1Score:      TThemeText;


    StaticP2R:        TThemeStatic;
    StaticP2RScoreBG: TThemeStatic; //Static for ScoreBG
    TextP2R:          TThemeText;
    TextP2RScore:     TThemeText;

    StaticP2M:        TThemeStatic;
    StaticP2MScoreBG: TThemeStatic; //Static for ScoreBG
    TextP2M:          TThemeText;
    TextP2MScore:     TThemeText;

    StaticP3R:        TThemeStatic;
    StaticP3RScoreBG: TThemeStatic; //Static for ScoreBG
    TextP3R:          TThemeText;
    TextP3RScore:     TThemeText;
  end;

  TThemeScore = class(TThemeBasic)
    TextArtist:       TThemeText;
    TextTitle:        TThemeText;

    PlayerStatic:     array[1..6] of AThemeStatic;

    TextName:         array[1..6] of TThemeText;
    TextScore:        array[1..6] of TThemeText;

    TextNotes:            array[1..6] of TThemeText;
    TextNotesScore:       array[1..6] of TThemeText;
    TextLineBonus:        array[1..6] of TThemeText;
    TextLineBonusScore:   array[1..6] of TThemeText;
    TextGoldenNotes:      array[1..6] of TThemeText;
    TextGoldenNotesScore: array[1..6] of TThemeText;
    TextTotal:            array[1..6] of TThemeText;
    TextTotalScore:       array[1..6] of TThemeText;

    StaticBoxLightest:    array[1..6] of TThemeStatic;
    StaticBoxLight:       array[1..6] of TThemeStatic;
    StaticBoxDark:        array[1..6] of TThemeStatic;

    StaticBackLevel:        array[1..6] of TThemeStatic;
    StaticBackLevelRound:   array[1..6] of TThemeStatic;
    StaticLevel:            array[1..6] of TThemeStatic;
    StaticLevelRound:       array[1..6] of TThemeStatic;

//    Description:          array[0..5] of string;}
  end;

  TThemeTop5 = class(TThemeBasic)
    TextLevel:        TThemeText;
    TextArtistTitle:  TThemeText;

    StaticNumber:     AThemeStatic;
    TextNumber:       AThemeText;
    TextName:         AThemeText;
    TextScore:        AThemeText;
  end;

  TThemeOptions = class(TThemeBasic)
    ButtonGame:       TThemeButton;
    ButtonGraphics:   TThemeButton;
    ButtonSound:      TThemeButton;
    ButtonLyrics:     TThemeButton;
    ButtonThemes:     TThemeButton;
    ButtonRecord:     TThemeButton;
    ButtonExit:       TThemeButton;

    TextDescription:      TThemeText;
    Description:          array[0..6] of string;
  end;

  TThemeOptionsGame = class(TThemeBasic)
    SelectPlayers:      TThemeSelect;
    SelectDifficulty:   TThemeSelect;
    SelectLanguage:     TThemeSelectSlide;
    SelectTabs:         TThemeSelect;
    SelectSorting:      TThemeSelectSlide;
    SelectDebug:        TThemeSelect;
    ButtonExit:         TThemeButton;
  end;

  TThemeOptionsGraphics = class(TThemeBasic)
    SelectFullscreen:       TThemeSelect;
    SelectSlideResolution:  TThemeSelectSlide;
    SelectDepth:            TThemeSelect;
    SelectOscilloscope:     TThemeSelect;
    SelectLineBonus:        TThemeSelect;
    SelectMovieSize:        TThemeSelect;
    ButtonExit:             TThemeButton;
  end;

  TThemeOptionsSound = class(TThemeBasic)
    SelectMicBoost:       TThemeSelect;
    SelectClickAssist:    TThemeSelect;
    SelectBeatClick:      TThemeSelect;
    SelectThreshold:      TThemeSelect;
    //SelectTwoPlayerMode:  TThemeSelect;
    ButtonExit:           TThemeButton;
  end;

  TThemeOptionsLyrics = class(TThemeBasic)
    SelectLyricsFont:   TThemeSelect;
    SelectLyricsEffect: TThemeSelect;
    SelectSolmization:  TThemeSelect;
    ButtonExit:         TThemeButton;
  end;

  TThemeOptionsThemes = class(TThemeBasic)
    SelectTheme:        TThemeSelectSlide;
    SelectSkin:         TThemeSelectSlide;
    SelectColor:        TThemeSelectSlide;
    ButtonExit:         TThemeButton;
  end;

  TThemeOptionsRecord = class(TThemeBasic)
    SelectSlideCard:      TThemeSelectSlide;
    SelectSlideInput:     TThemeSelectSlide;
    SelectSlideChannelL:  TThemeSelectSlide;
    SelectSlideChannelR:  TThemeSelectSlide;
    ButtonExit:           TThemeButton;
  end;

  //ScreenSong Menue
  TThemeSongMenu = class(TThemeBasic)
    Button1: TThemeButton;
    Button2: TThemeButton;
    Button3: TThemeButton;
    Button4: TThemeButton;

    SelectSlide3: TThemeSelectSlide;

    TextMenu: TThemeText;
  end;

  TThemeSongJumpTo = class(TThemeBasic)
    ButtonSearchText: TThemeButton;
    SelectSlideType:  TThemeSelectSlide;
    TextFound:        TThemeText;
  end;

  //Party Screens
  TThemePartyNewRound = class(TThemeBasic)
    TextRound1:        TThemeText;
    TextRound2:        TThemeText;
    TextRound3:        TThemeText;
    TextRound4:        TThemeText;
    TextRound5:        TThemeText;
    TextRound6:        TThemeText;
    TextRound7:        TThemeText;
    TextWinner1:       TThemeText;
    TextWinner2:       TThemeText;
    TextWinner3:       TThemeText;
    TextWinner4:       TThemeText;
    TextWinner5:       TThemeText;
    TextWinner6:       TThemeText;
    TextWinner7:       TThemeText;
    TextNextRound:     TThemeText;
    TextNextRoundNo:   TThemeText;
    TextNextPlayer1:   TThemeText;
    TextNextPlayer2:   TThemeText;
    TextNextPlayer3:   TThemeText;

    StaticRound1:      TThemeStatic;
    StaticRound2:      TThemeStatic;
    StaticRound3:      TThemeStatic;
    StaticRound4:      TThemeStatic;
    StaticRound5:      TThemeStatic;
    StaticRound6:      TThemeStatic;
    StaticRound7:      TThemeStatic;

    TextScoreTeam1:    TThemeText;
    TextScoreTeam2:    TThemeText;
    TextScoreTeam3:    TThemeText;
    TextNameTeam1:     TThemeText;
    TextNameTeam2:     TThemeText;
    TextNameTeam3:     TThemeText;
    StaticTeam1:       TThemeStatic;
    StaticTeam2:       TThemeStatic;
    StaticTeam3:       TThemeStatic;

    ButtonNext:        TThemeButton;
  end;

  TThemePartyScore = class(TThemeBasic)
    TextScoreTeam1:    TThemeText;
    TextScoreTeam2:    TThemeText;
    TextScoreTeam3:    TThemeText;
    TextNameTeam1:     TThemeText;
    TextNameTeam2:     TThemeText;
    TextNameTeam3:     TThemeText;
    StaticTeam1:       TThemeStatic;
    StaticTeam2:       TThemeStatic;
    StaticTeam3:       TThemeStatic;

    TextWinner:        TThemeText;
  end;

  TThemePartyWin = class(TThemeBasic)
    TextScoreTeam1:    TThemeText;
    TextScoreTeam2:    TThemeText;
    TextScoreTeam3:    TThemeText;
    TextNameTeam1:     TThemeText;
    TextNameTeam2:     TThemeText;
    TextNameTeam3:     TThemeText;
    StaticTeam1:       TThemeStatic;
    StaticTeam2:       TThemeStatic;
    StaticTeam3:       TThemeStatic;

    TextWinner:        TThemeText;
  end;

  TThemePartyOptions = class(TThemeBasic)
    SelectLevel: TThemeSelectSlide;
    SelectPlayList: TThemeSelectSlide;
    SelectPlayList2: TThemeSelectSlide;
    SelectRounds: TThemeSelectSlide;
    SelectTeams: TThemeSelectSlide;
    SelectPlayers1: TThemeSelectSlide;
    SelectPlayers2: TThemeSelectSlide;
    SelectPlayers3: TThemeSelectSlide;

    {ButtonNext: TThemeButton;
    ButtonPrev: TThemeButton;}
  end;

  TThemePartyPlayer = class(TThemeBasic)
    Team1Name: TThemeButton;
    Player1Name: TThemeButton;
    Player2Name: TThemeButton;
    Player3Name: TThemeButton;
    Player4Name: TThemeButton;

    Team2Name: TThemeButton;
    Player5Name: TThemeButton;
    Player6Name: TThemeButton;
    Player7Name: TThemeButton;
    Player8Name: TThemeButton;

    Team3Name: TThemeButton;
    Player9Name: TThemeButton;
    Player10Name: TThemeButton;
    Player11Name: TThemeButton;
    Player12Name: TThemeButton;

    {ButtonNext: TThemeButton;
    ButtonPrev: TThemeButton;}
  end;

  TTheme = class
    {$IFDEF THEMESAVE}
    ThemeIni:         TIniFile;
    {$ELSE}
    ThemeIni:         TMemIniFile;
    {$ENDIF}

    Loading:          TThemeLoading;
    Main:             TThemeMain;
    Name:             TThemeName;
    Level:            TThemeLevel;
    Song:             TThemeSong;
    Sing:             TThemeSing;
    Score:            TThemeScore;
    Top5:             TThemeTop5;
    Options:          TThemeOptions;
    OptionsGame:      TThemeOptionsGame;
    OptionsGraphics:  TThemeOptionsGraphics;
    OptionsSound:     TThemeOptionsSound;
    OptionsLyrics:    TThemeOptionsLyrics;
    OptionsThemes:    TThemeOptionsThemes;
    OptionsRecord:    TThemeOptionsRecord;
    //Menu
    SongMenu:         TThemeSongMenu;
    SongJumpto:       TThemeSongJumpTo;
    //Party Screens:
    PartyNewRound:    TThemePartyNewRound;
    PartyScore:       TThemePartyScore;
    PartyWin:         TThemePartyWin;
    PartyOptions:      TThemePartyOptions;
    PartyPlayer:       TThemePartyPlayer;

    constructor Create(FileName: string); overload; // Initialize theme system
    constructor Create(FileName: string; Color: integer); overload; // Initialize theme system with color
    function LoadTheme(FileName: string; sColor: integer): boolean; // Load some theme settings from file

    procedure LoadColors;

    procedure ThemeLoadBasic(Theme: TThemeBasic; Name: string);
    procedure ThemeLoadBackground(var ThemeBackground: TThemeBackground; Name: string);
    procedure ThemeLoadText(var ThemeText: TThemeText; Name: string);
    procedure ThemeLoadTexts(var ThemeText: AThemeText; Name: string);
    procedure ThemeLoadStatic(var ThemeStatic: TThemeStatic; Name: string);
    procedure ThemeLoadStatics(var ThemeStatic: AThemeStatic; Name: string);
    procedure ThemeLoadButton(var ThemeButton: TThemeButton; Name: string);
    procedure ThemeLoadSelect(var ThemeSelect: TThemeSelect; Name: string);
    procedure ThemeLoadSelectSlide(var ThemeSelectS: TThemeSelectSlide; Name: string);

    procedure ThemeSave(FileName: string);
    procedure ThemeSaveBasic(Theme: TThemeBasic; Name: string);
    procedure ThemeSaveBackground(ThemeBackground: TThemeBackground; Name: string);
    procedure ThemeSaveStatic(ThemeStatic: TThemeStatic; Name: string);
    procedure ThemeSaveStatics(ThemeStatic: AThemeStatic; Name: string);
    procedure ThemeSaveText(ThemeText: TThemeText; Name: string);
    procedure ThemeSaveTexts(ThemeText: AThemeText; Name: string);
    procedure ThemeSaveButton(ThemeButton: TThemeButton; Name: string);

  end;

  TColor = record
    Name:   string;
    RGB:    TRGB;
  end;

function ColorExists(Name: string): integer;
procedure LoadColor(var R, G, B: real; ColorName: string);
function GetSystemColor(Color: integer): TRGB;
function ColorSqrt(RGB: TRGB): TRGB;

var
  //Skin:         TSkin;
  Theme:        TTheme;
  Color:        array of TColor;

implementation

uses
{{$IFDEF TRANSLATE}
 ULanguage,
{{$ENDIF}
USkins, UIni;

constructor TTheme.Create(FileName: string);
begin
  Create(FileName, 0);
end;

constructor TTheme.Create(FileName: string; Color: integer);
begin
  Loading := TThemeLoading.Create;
  Main := TThemeMain.Create;
  Name := TThemeName.Create;
  Level := TThemeLevel.Create;
  Song := TThemeSong.Create;
  Sing := TThemeSing.Create;
  Score := TThemeScore.Create;
  Top5 := TThemeTop5.Create;
  Options := TThemeOptions.Create;
  OptionsGame := TThemeOptionsGame.Create;
  OptionsGraphics := TThemeOptionsGraphics.Create;
  OptionsSound := TThemeOptionsSound.Create;
  OptionsLyrics := TThemeOptionsLyrics.Create;
  OptionsThemes := TThemeOptionsThemes.Create;
  OptionsRecord := TThemeOptionsRecord.Create;

  SongMenu := TThemeSongMenu.Create;
  SongJumpto := TThemeSongJumpto.Create;
  //Party Screens
  PartyNewRound := TThemePartyNewRound.Create;
  PartyWin := TThemePartyWin.Create;
  PartyScore := TThemePartyScore.Create;
  PartyOptions := TThemePartyOptions.Create;
  PartyPlayer := TThemePartyPlayer.Create;

  LoadTheme(FileName, Color);


  {Skin.GrayLeft :=    'Left Gray.bmp';
  Skin.GrayMid :=     'Mid Gray.bmp';
  Skin.GrayRight :=   'Right Gray.bmp';

  Skin.NoteBGLeft :=   'Note BG Left.bmp';
  Skin.NoteBGMid :=    'Note BG Mid.bmp';
  Skin.NoteBGRight :=  'Note BG Right.bmp';

  Skin.NoteStar := 'Note Star.jpg';



  //SingBar Mod
  Skin.SingBarBack := 'Sing Bar Back.jpg';
  Skin.SingBarBar := 'Sing Bar Bar.jpg';
  Skin.SingBarFront := 'Sing Bar Front.jpg';
  //end Singbar Mod

  //PhrasenBonus - Line Bonus Mod
  Skin.SingLineBonusBack := 'Line Bonus PopUp.jpg';




{  Skin.WelcomeBG :=   SkinPath + 'Welcome BG.jpg';
//  Skin.Background :=  SkinPath + 'Background.jpg';
  Skin.ScoreBG :=     SkinPath + 'Score BG.jpg';
//  Skin.GameStart :=   SkinPath + 'Game Start.jpg';
//  Skin.Editor :=      SkinPath + 'Editor.jpg';
//  Skin.Options :=     SkinPath + 'Options.jpg';
//  Skin.Exit :=        SkinPath + 'Exit.jpg';

  Skin.MainStart :=   SkinPath + 'Main Solo.jpg';
  Skin.MainEditor :=  SkinPath + 'Main Multi.jpg';
  Skin.MainOptions := SkinPath + 'Main Options.jpg';
  Skin.MainExit :=    SkinPath + 'Main Exit.jpg';
  Skin.MainBar :=     SkinPath + 'Main Bar.jpg';
  Skin.Cursor :=      SkinPath + 'Main Cursor.jpg';

  Skin.SongFade :=      SkinPath + 'Song Fade.jpg';}{
  Skin.SongCover :=     'Song Cover.jpg';
  {Skin.SongSelection := SkinPath + 'Song Selection.jpg';

  Skin.SelectSong :=  SkinPath + 'Select Song.jpg';}{
//  Skin.Button :=      SkinPath + 'MusicWheelItem song.jpg';
  Skin.Bar :=         'Bar.jpg';
{  Skin.P :=           SkinPath + 'P.jpg';
  Skin.Arrow :=       SkinPath + 'Arrow.jpg';
  Skin.Arrow2 :=      SkinPath + 'Arrow 2.jpg';}{
  Skin.ButtonF :=     'Button.jpg';
  Skin.Ball :=        'Ball3.bmp';
{  Skin.Star :=        SkinPath + 'Star.bmp';
  Skin.Line :=        SkinPath + 'Line.jpg';}
end;


function TTheme.LoadTheme(FileName: string; sColor: integer): boolean;
var
  I:    integer;
  Path: string;
begin
  Result := false;
  if FileExists(FileName) then begin
    Result := true;

    {$IFDEF THEMESAVE}
    ThemeIni := TIniFile.Create(FileName);
    {$ELSE}
    ThemeIni := TMemIniFile.Create(FileName);
    {$ENDIF}

    if ThemeIni.ReadString('Theme', 'Name', '') <> '' then begin

      {Skin.SkinName := ThemeIni.ReadString('Theme', 'Name', 'Singstar');
      Skin.SkinPath := 'Skins\' + Skin.SkinName + '\';
      Skin.SkinReg := false; }
      Skin.Color := sColor;

      Skin.LoadSkin(ISkin[Ini.SkinNo]);

      LoadColors;

//      ThemeIni.Free;
//      ThemeIni := TIniFile.Create('Themes\Singstar\Main.ini');

      // Loading
      ThemeLoadBasic(Loading, 'Loading');

      // Main
      ThemeLoadBasic(Main, 'Main');

      ThemeLoadText(Main.TextDescription, 'MainTextDescription');
      ThemeLoadText(Main.TextDescriptionLong, 'MainTextDescriptionLong');
      ThemeLoadButton(Main.ButtonSolo, 'MainButtonSolo');
      ThemeLoadButton(Main.ButtonEditor, 'MainButtonEditor');
      ThemeLoadButton(Main.ButtonOptions, 'MainButtonOptions');
      ThemeLoadButton(Main.ButtonExit, 'MainButtonExit');

      //Score Text Translation Start

      {{$IFDEF TRANSLATE}
      Main.Description[0] := Language.Translate('SING_SING');
      Main.DescriptionLong[0] := Language.Translate('SING_SING_DESC');
      Main.Description[1] := Language.Translate('SING_EDITOR');
      Main.DescriptionLong[1] := Language.Translate('SING_EDITOR_DESC');
      Main.Description[2] := Language.Translate('SING_GAME_OPTIONS');
      Main.DescriptionLong[2] := Language.Translate('SING_GAME_OPTIONS_DESC');
      Main.Description[3] := Language.Translate('SING_EXIT');
      Main.DescriptionLong[3] := Language.Translate('SING_EXIT_DESC');
      {{$ENDIF}

      //Score Text Translation End

      Main.TextDescription.Text := Main.Description[0];
      Main.TextDescriptionLong.Text := Main.DescriptionLong[0];

      // Name
      ThemeLoadBasic(Name, 'Name');

      for I := 1 to 6 do
        ThemeLoadButton(Name.ButtonPlayer[I], 'NameButtonPlayer'+IntToStr(I));

      // Level
      ThemeLoadBasic(Level, 'Level');

      ThemeLoadButton(Level.ButtonEasy, 'LevelButtonEasy');
      ThemeLoadButton(Level.ButtonMedium, 'LevelButtonMedium');
      ThemeLoadButton(Level.ButtonHard, 'LevelButtonHard');


      // Song
      ThemeLoadBasic(Song, 'Song');

      ThemeLoadText(Song.TextArtist, 'SongTextArtist');
      ThemeLoadText(Song.TextTitle, 'SongTextTitle');
      ThemeLoadText(Song.TextNumber, 'SongTextNumber');

      //Show Cat in TopLeft Mod
      ThemeLoadStatic(Song.StaticCat, 'SongStaticCat');
      ThemeLoadText(Song.TextCat, 'SongTextCat');

      //Load Cover Pos and Size from Theme Mod
      Song.Cover.X := ThemeIni.ReadInteger('SongCover', 'X', 400);
      Song.Cover.Y := ThemeIni.ReadInteger('SongCover', 'Y', 140);
      Song.Cover.Z := ThemeIni.ReadInteger('SongCover', 'Z', 250);
      Song.Cover.W := ThemeIni.ReadInteger('SongCover', 'W', 300);
      Song.Cover.H := ThemeIni.ReadInteger('SongCover', 'H', 200);
      Song.Cover.Style := ThemeIni.ReadInteger('SongCover', 'Style', 4);
      Song.Cover.Reflections := (ThemeIni.ReadInteger('SongCover', 'Reflections', 0) = 1);
      //Load Cover Pos and Size from Theme Mod End

      //Load Equalizer Pos and Size from Theme Mod
      Song.Equalizer.Visible := (ThemeIni.ReadInteger('SongEqualizer', 'Visible', 0) = 1);
      Song.Equalizer.Direction := (ThemeIni.ReadInteger('SongEqualizer', 'Direction', 0) = 1);
      Song.Equalizer.Alpha := ThemeIni.ReadInteger('SongEqualizer', 'Alpha', 1);
      Song.Equalizer.Space := ThemeIni.ReadInteger('SongEqualizer', 'Space', 1);
      Song.Equalizer.X := ThemeIni.ReadInteger('SongEqualizer', 'X', 650);
      Song.Equalizer.Y := ThemeIni.ReadInteger('SongEqualizer', 'Y', 430);
      Song.Equalizer.Z := ThemeIni.ReadInteger('SongEqualizer', 'Z', 1);
      Song.Equalizer.W := ThemeIni.ReadInteger('SongEqualizer', 'PieceW', 8);
      Song.Equalizer.H := ThemeIni.ReadInteger('SongEqualizer', 'PieceH', 8);
      Song.Equalizer.Bands := ThemeIni.ReadInteger('SongEqualizer', 'Bands', 5);
      Song.Equalizer.Length := ThemeIni.ReadInteger('SongEqualizer', 'Length', 12);

      //Color
      I := ColorExists(ThemeIni.ReadString('SongEqualizer', 'Color', 'Black'));
      if I >= 0 then begin
        Song.Equalizer.ColR := Color[I].RGB.R;
        Song.Equalizer.ColG := Color[I].RGB.G;
        Song.Equalizer.ColB := Color[I].RGB.B;
      end;
      //Load Equalizer Pos and Size from Theme Mod End

      //Party Mode
      ThemeLoadStatic(Song.StaticTeam1Joker1, 'SongStaticTeam1Joker1');
      ThemeLoadStatic(Song.StaticTeam1Joker2, 'SongStaticTeam1Joker2');
      ThemeLoadStatic(Song.StaticTeam1Joker3, 'SongStaticTeam1Joker3');
      ThemeLoadStatic(Song.StaticTeam1Joker4, 'SongStaticTeam1Joker4');
      ThemeLoadStatic(Song.StaticTeam1Joker5, 'SongStaticTeam1Joker5');

      ThemeLoadStatic(Song.StaticTeam2Joker1, 'SongStaticTeam2Joker1');
      ThemeLoadStatic(Song.StaticTeam2Joker2, 'SongStaticTeam2Joker2');
      ThemeLoadStatic(Song.StaticTeam2Joker3, 'SongStaticTeam2Joker3');
      ThemeLoadStatic(Song.StaticTeam2Joker4, 'SongStaticTeam2Joker4');
      ThemeLoadStatic(Song.StaticTeam2Joker5, 'SongStaticTeam2Joker5');

      ThemeLoadStatic(Song.StaticTeam3Joker1, 'SongStaticTeam3Joker1');
      ThemeLoadStatic(Song.StaticTeam3Joker2, 'SongStaticTeam3Joker2');
      ThemeLoadStatic(Song.StaticTeam3Joker3, 'SongStaticTeam3Joker3');
      ThemeLoadStatic(Song.StaticTeam3Joker4, 'SongStaticTeam3Joker4');
      ThemeLoadStatic(Song.StaticTeam3Joker5, 'SongStaticTeam3Joker5');

      // Sing
      ThemeLoadBasic(Sing, 'Sing');

      ThemeLoadStatic(Sing.StaticP1, 'SingP1Static');


      //ScoreBG Mod
      ThemeLoadStatic(Sing.StaticP1ScoreBG, 'SingP1Static2');//ScoreBG
      //end ScoreBG Mod


      ThemeLoadText(Sing.TextP1, 'SingP1Text');
      ThemeLoadText(Sing.TextP1Score, 'SingP1TextScore');

      ThemeLoadStatic(Sing.StaticP2R, 'SingP2RStatic');



      //ScoreBG Mod
      ThemeLoadStatic(Sing.StaticP2RScoreBG, 'SingP2RStatic2');
      //end ScoreBG Mod



      ThemeLoadText(Sing.TextP2R, 'SingP2RText');
      ThemeLoadText(Sing.TextP2RScore, 'SingP2RTextScore');

      ThemeLoadStatic(Sing.StaticP2M, 'SingP2MStatic');


      //ScoreBG Mod
      ThemeLoadStatic(Sing.StaticP2MScoreBG, 'SingP2MStatic2');
      //end ScoreBG Mod



      ThemeLoadText(Sing.TextP2M, 'SingP2MText');
      ThemeLoadText(Sing.TextP2MScore, 'SingP2MTextScore');

      ThemeLoadStatic(Sing.StaticP3R, 'SingP3RStatic');


       //ScoreBG Mod
      ThemeLoadStatic(Sing.StaticP3RScoreBG, 'SingP3RStatic2');
      //end ScoreBG Mod



      ThemeLoadText(Sing.TextP3R, 'SingP3RText');
      ThemeLoadText(Sing.TextP3RScore, 'SingP3RTextScore');

      // Score
      ThemeLoadBasic(Score, 'Score');

      ThemeLoadText(Score.TextArtist, 'ScoreTextArtist');
      ThemeLoadText(Score.TextTitle, 'ScoreTextTitle');

      for I := 1 to 6 do begin
        ThemeLoadStatics(Score.PlayerStatic[I], 'ScorePlayer' + IntToStr(I) + 'Static');

        ThemeLoadText(Score.TextName[I], 'ScoreTextName' + IntToStr(I));
        ThemeLoadText(Score.TextScore[I], 'ScoreTextScore' + IntToStr(I));
        ThemeLoadText(Score.TextNotes[I], 'ScoreTextNotes' + IntToStr(I));
        ThemeLoadText(Score.TextNotesScore[I], 'ScoreTextNotesScore' + IntToStr(I));
        ThemeLoadText(Score.TextLineBonus[I], 'ScoreTextLineBonus' + IntToStr(I));
        ThemeLoadText(Score.TextLineBonusScore[I], 'ScoreTextLineBonusScore' + IntToStr(I));
        ThemeLoadText(Score.TextGoldenNotes[I], 'ScoreTextGoldenNotes' + IntToStr(I));
        ThemeLoadText(Score.TextGoldenNotesScore[I], 'ScoreTextGoldenNotesScore' + IntToStr(I));
        ThemeLoadText(Score.TextTotal[I], 'ScoreTextTotal' + IntToStr(I));
        ThemeLoadText(Score.TextTotalScore[I], 'ScoreTextTotalScore' + IntToStr(I));

        ThemeLoadStatic(Score.StaticBoxLightest[I], 'ScoreStaticBoxLightest' + IntToStr(I));
        ThemeLoadStatic(Score.StaticBoxLight[I], 'ScoreStaticBoxLight' + IntToStr(I));
        ThemeLoadStatic(Score.StaticBoxDark[I], 'ScoreStaticBoxDark' + IntToStr(I));

        ThemeLoadStatic(Score.StaticBackLevel[I], 'ScoreStaticBackLevel' + IntToStr(I));
        ThemeLoadStatic(Score.StaticBackLevelRound[I], 'ScoreStaticBackLevelRound' + IntToStr(I));
        ThemeLoadStatic(Score.StaticLevel[I], 'ScoreStaticLevel' + IntToStr(I));
        ThemeLoadStatic(Score.StaticLevelRound[I], 'ScoreStaticLevelRound' + IntToStr(I));
      end;

      // Top5
      ThemeLoadBasic(Top5, 'Top5');
      
      ThemeLoadText(Top5.TextLevel, 'Top5TextLevel');
      ThemeLoadText(Top5.TextArtistTitle, 'Top5TextArtistTitle');
      ThemeLoadStatics(Top5.StaticNumber, 'Top5StaticNumber');
      ThemeLoadTexts(Top5.TextNumber, 'Top5TextNumber');
      ThemeLoadTexts(Top5.TextName, 'Top5TextName');
      ThemeLoadTexts(Top5.TextScore, 'Top5TextScore');

    // Options
    ThemeLoadBasic(Options, 'Options');

    ThemeLoadButton(Options.ButtonGame, 'OptionsButtonGame');
    ThemeLoadButton(Options.ButtonGraphics, 'OptionsButtonGraphics');
    ThemeLoadButton(Options.ButtonSound, 'OptionsButtonSound');
    ThemeLoadButton(Options.ButtonLyrics, 'OptionsButtonLyrics');
    ThemeLoadButton(Options.ButtonThemes, 'OptionsButtonThemes');
    ThemeLoadButton(Options.ButtonRecord, 'OptionsButtonRecord');
    ThemeLoadButton(Options.ButtonExit, 'OptionsButtonExit');

    {{$IFDEF TRANSLATE}
    Options.Description[0] := Language.Translate('SING_OPTIONS_GAME');
    Options.Description[1] := Language.Translate('SING_OPTIONS_GRAPHICS');
    Options.Description[2] := Language.Translate('SING_OPTIONS_SOUND');
    Options.Description[3] := Language.Translate('SING_OPTIONS_LYRICS');
    Options.Description[4] := Language.Translate('SING_OPTIONS_THEMES');
    Options.Description[5] := Language.Translate('SING_OPTIONS_RECORD');
    Options.Description[6] := Language.Translate('SING_OPTIONS_EXIT');
    {{$ENDIF}

    ThemeLoadText(Options.TextDescription, 'OptionsTextDescription');
    Options.TextDescription.Text := Options.Description[0];

    // Options Game
    ThemeLoadBasic(OptionsGame, 'OptionsGame');

    ThemeLoadSelect(OptionsGame.SelectPlayers, 'OptionsGameSelectPlayers');
    ThemeLoadSelect(OptionsGame.SelectDifficulty, 'OptionsGameSelectDifficulty');
    ThemeLoadSelectSlide(OptionsGame.SelectLanguage, 'OptionsGameSelectSlideLanguage');
    ThemeLoadSelect(OptionsGame.SelectTabs, 'OptionsGameSelectTabs');
    ThemeLoadSelectSlide(OptionsGame.SelectSorting, 'OptionsGameSelectSlideSorting');
    ThemeLoadSelect(OptionsGame.SelectDebug, 'OptionsGameSelectDebug');
    ThemeLoadButton(OptionsGame.ButtonExit, 'OptionsGameButtonExit');

    // Options Graphics
    ThemeLoadBasic(OptionsGraphics, 'OptionsGraphics');

    ThemeLoadSelect(OptionsGraphics.SelectFullscreen, 'OptionsGraphicsSelectFullscreen');
    ThemeLoadSelectSlide(OptionsGraphics.SelectSlideResolution, 'OptionsGraphicsSelectSlideResolution');
    ThemeLoadSelect(OptionsGraphics.SelectDepth, 'OptionsGraphicsSelectDepth');
    ThemeLoadSelect(OptionsGraphics.SelectOscilloscope, 'OptionsGraphicsSelectOscilloscope');
    ThemeLoadSelect(OptionsGraphics.SelectLineBonus, 'OptionsGraphicsSelectLineBonus');
    ThemeLoadSelect(OptionsGraphics.SelectMovieSize, 'OptionsGraphicsSelectMovieSize');
    ThemeLoadButton(OptionsGraphics.ButtonExit, 'OptionsGraphicsButtonExit');

    // Options Sound
    ThemeLoadBasic(OptionsSound, 'OptionsSound');

    ThemeLoadSelect(OptionsSound.SelectMicBoost, 'OptionsSoundSelectMicBoost');
    ThemeLoadSelect(OptionsSound.SelectClickAssist, 'OptionsSoundSelectClickAssist');
    ThemeLoadSelect(OptionsSound.SelectBeatClick, 'OptionsSoundSelectBeatClick');
    ThemeLoadSelect(OptionsSound.SelectThreshold, 'OptionsSoundSelectThreshold');
    //ThemeLoadSelect(OptionsSound.SelectTwoPlayerMode, 'OptionsSoundSelectTwoPlayerMode');
    ThemeLoadButton(OptionsSound.ButtonExit, 'OptionsSoundButtonExit');

    // Options Lyrics
    ThemeLoadBasic(OptionsLyrics, 'OptionsLyrics');

    ThemeLoadSelect(OptionsLyrics.SelectLyricsFont, 'OptionsLyricsSelectLyricsFont');
    ThemeLoadSelect(OptionsLyrics.SelectLyricsEffect, 'OptionsLyricsSelectLyricsEffect');
    ThemeLoadSelect(OptionsLyrics.SelectSolmization, 'OptionsLyricsSelectSolmization');
    ThemeLoadButton(OptionsLyrics.ButtonExit, 'OptionsLyricsButtonExit');

    // Options Themes
    ThemeLoadBasic(OptionsThemes, 'OptionsThemes');

      ThemeLoadSelectSlide(OptionsThemes.SelectTheme, 'OptionsThemesSelectTheme');
      ThemeLoadSelectSlide(OptionsThemes.SelectSkin, 'OptionsThemesSelectSkin');
      ThemeLoadSelectSlide(OptionsThemes.SelectColor, 'OptionsThemesSelectColor');
      ThemeLoadButton(OptionsThemes.ButtonExit, 'OptionsThemesButtonExit');

      // Options Record
      ThemeLoadBasic(OptionsRecord, 'OptionsRecord');

      ThemeLoadSelectSlide(OptionsRecord.SelectSlideCard, 'OptionsRecordSelectSlideCard');
      ThemeLoadSelectSlide(OptionsRecord.SelectSlideInput,  'OptionsRecordSelectSlideInput');
      ThemeLoadSelectSlide(OptionsRecord.SelectSlideChannelL, 'OptionsRecordSelectSlideChannelL');
      ThemeLoadSelectSlide(OptionsRecord.SelectSlideChannelR, 'OptionsRecordSelectSlideChannelR');
      ThemeLoadButton(OptionsRecord.ButtonExit, 'OptionsRecordButtonExit');

   //Song Menu
    ThemeLoadBasic (SongMenu, 'SongMenu');
    ThemeLoadButton(SongMenu.Button1, 'SongMenuButton1');
    ThemeLoadButton(SongMenu.Button2, 'SongMenuButton2');
    ThemeLoadButton(SongMenu.Button3, 'SongMenuButton3');
    ThemeLoadButton(SongMenu.Button4, 'SongMenuButton4');
    ThemeLoadSelectSlide(SongMenu.SelectSlide3, 'SongMenuSelectSlide3');

    ThemeLoadText(SongMenu.TextMenu, 'SongMenuTextMenu');

    //Song Jumpto
    ThemeLoadBasic (SongJumpto, 'SongJumpto');
    ThemeLoadButton(SongJumpto.ButtonSearchText, 'SongJumptoButtonSearchText');
    ThemeLoadSelectSlide(SongJumpto.SelectSlideType, 'SongJumptoSelectSlideType');
    ThemeLoadText(SongJumpto.TextFound, 'SongJumptoTextFound');

    //Party Screens:
    //Party NewRound
    ThemeLoadBasic(PartyNewRound, 'PartyNewRound');

    ThemeLoadText (PartyNewRound.TextRound1, 'PartyNewRoundTextRound1');
    ThemeLoadText (PartyNewRound.TextRound2, 'PartyNewRoundTextRound2');
    ThemeLoadText (PartyNewRound.TextRound3, 'PartyNewRoundTextRound3');
    ThemeLoadText (PartyNewRound.TextRound4, 'PartyNewRoundTextRound4');
    ThemeLoadText (PartyNewRound.TextRound5, 'PartyNewRoundTextRound5');
    ThemeLoadText (PartyNewRound.TextRound6, 'PartyNewRoundTextRound6');
    ThemeLoadText (PartyNewRound.TextRound7, 'PartyNewRoundTextRound7');
    ThemeLoadText (PartyNewRound.TextWinner1, 'PartyNewRoundTextWinner1');
    ThemeLoadText (PartyNewRound.TextWinner2, 'PartyNewRoundTextWinner2');
    ThemeLoadText (PartyNewRound.TextWinner3, 'PartyNewRoundTextWinner3');
    ThemeLoadText (PartyNewRound.TextWinner4, 'PartyNewRoundTextWinner4');
    ThemeLoadText (PartyNewRound.TextWinner5, 'PartyNewRoundTextWinner5');
    ThemeLoadText (PartyNewRound.TextWinner6, 'PartyNewRoundTextWinner6');
    ThemeLoadText (PartyNewRound.TextWinner7, 'PartyNewRoundTextWinner7');
    ThemeLoadText (PartyNewRound.TextNextRound, 'PartyNewRoundTextNextRound');
    ThemeLoadText (PartyNewRound.TextNextRoundNo, 'PartyNewRoundTextNextRoundNo');
    ThemeLoadText (PartyNewRound.TextNextPlayer1, 'PartyNewRoundTextNextPlayer1');
    ThemeLoadText (PartyNewRound.TextNextPlayer2, 'PartyNewRoundTextNextPlayer2');
    ThemeLoadText (PartyNewRound.TextNextPlayer3, 'PartyNewRoundTextNextPlayer3');

    ThemeLoadStatic (PartyNewRound.StaticRound1, 'PartyNewRoundStaticRound1');
    ThemeLoadStatic (PartyNewRound.StaticRound2, 'PartyNewRoundStaticRound2');
    ThemeLoadStatic (PartyNewRound.StaticRound3, 'PartyNewRoundStaticRound3');
    ThemeLoadStatic (PartyNewRound.StaticRound4, 'PartyNewRoundStaticRound4');
    ThemeLoadStatic (PartyNewRound.StaticRound5, 'PartyNewRoundStaticRound5');
    ThemeLoadStatic (PartyNewRound.StaticRound6, 'PartyNewRoundStaticRound6');
    ThemeLoadStatic (PartyNewRound.StaticRound7, 'PartyNewRoundStaticRound7');

    ThemeLoadText (PartyNewRound.TextScoreTeam1, 'PartyNewRoundTextScoreTeam1');
    ThemeLoadText (PartyNewRound.TextScoreTeam2, 'PartyNewRoundTextScoreTeam2');
    ThemeLoadText (PartyNewRound.TextScoreTeam3, 'PartyNewRoundTextScoreTeam3');
    ThemeLoadText (PartyNewRound.TextNameTeam1, 'PartyNewRoundTextNameTeam1');
    ThemeLoadText (PartyNewRound.TextNameTeam2, 'PartyNewRoundTextNameTeam2');
    ThemeLoadText (PartyNewRound.TextNameTeam3, 'PartyNewRoundTextNameTeam3');

    ThemeLoadStatic (PartyNewRound.StaticTeam1, 'PartyNewRoundStaticTeam1');
    ThemeLoadStatic (PartyNewRound.StaticTeam2, 'PartyNewRoundStaticTeam2');
    ThemeLoadStatic (PartyNewRound.StaticTeam3, 'PartyNewRoundStaticTeam3');

    ThemeLoadButton (PartyNewRound.ButtonNext, 'PartyNewRoundButtonNext');

    //Party Score
    ThemeLoadBasic(PartyScore, 'PartyScore');

    ThemeLoadText (PartyScore.TextScoreTeam1, 'PartyScoreTextScoreTeam1');
    ThemeLoadText (PartyScore.TextScoreTeam2, 'PartyScoreTextScoreTeam2');
    ThemeLoadText (PartyScore.TextScoreTeam3, 'PartyScoreTextScoreTeam3');
    ThemeLoadText (PartyScore.TextNameTeam1, 'PartyScoreTextNameTeam1');
    ThemeLoadText (PartyScore.TextNameTeam2, 'PartyScoreTextNameTeam2');
    ThemeLoadText (PartyScore.TextNameTeam3, 'PartyScoreTextNameTeam3');

    ThemeLoadStatic (PartyScore.StaticTeam1, 'PartyScoreStaticTeam1');
    ThemeLoadStatic (PartyScore.StaticTeam2, 'PartyScoreStaticTeam2');
    ThemeLoadStatic (PartyScore.StaticTeam3, 'PartyScoreStaticTeam3');

    ThemeLoadText (PartyScore.TextWinner, 'PartyScoreTextWinner');

    //Party Win
    ThemeLoadBasic(PartyWin, 'PartyWin');

    ThemeLoadText (PartyWin.TextScoreTeam1, 'PartyWinTextScoreTeam1');
    ThemeLoadText (PartyWin.TextScoreTeam2, 'PartyWinTextScoreTeam2');
    ThemeLoadText (PartyWin.TextScoreTeam3, 'PartyWinTextScoreTeam3');
    ThemeLoadText (PartyWin.TextNameTeam1, 'PartyWinTextNameTeam1');
    ThemeLoadText (PartyWin.TextNameTeam2, 'PartyWinTextNameTeam2');
    ThemeLoadText (PartyWin.TextNameTeam3, 'PartyWinTextNameTeam3');

    ThemeLoadStatic (PartyWin.StaticTeam1, 'PartyWinStaticTeam1');
    ThemeLoadStatic (PartyWin.StaticTeam2, 'PartyWinStaticTeam2');
    ThemeLoadStatic (PartyWin.StaticTeam3, 'PartyWinStaticTeam3');

    ThemeLoadText (PartyWin.TextWinner, 'PartyWinTextWinner');

    //Party Options
    ThemeLoadBasic(PartyOptions, 'PartyOptions');
    ThemeLoadSelectSlide(PartyOptions.SelectLevel, 'PartyOptionsSelectLevel');
    ThemeLoadSelectSlide(PartyOptions.SelectPlayList, 'PartyOptionsSelectPlayList');
    ThemeLoadSelectSlide(PartyOptions.SelectPlayList2, 'PartyOptionsSelectPlayList2');
    ThemeLoadSelectSlide(PartyOptions.SelectRounds, 'PartyOptionsSelectRounds');
    ThemeLoadSelectSlide(PartyOptions.SelectTeams, 'PartyOptionsSelectTeams');
    ThemeLoadSelectSlide(PartyOptions.SelectPlayers1, 'PartyOptionsSelectPlayers1');
    ThemeLoadSelectSlide(PartyOptions.SelectPlayers2, 'PartyOptionsSelectPlayers2');
    ThemeLoadSelectSlide(PartyOptions.SelectPlayers3, 'PartyOptionsSelectPlayers3');

    {ThemeLoadButton (ButtonNext, 'ButtonNext');
    ThemeLoadButton (ButtonPrev, 'ButtonPrev');}

    //Party Player
    ThemeLoadBasic(PartyPlayer, 'PartyPlayer');
    ThemeLoadButton(PartyPlayer.Team1Name, 'PartyPlayerTeam1Name');
    ThemeLoadButton(PartyPlayer.Player1Name, 'PartyPlayerPlayer1Name');
    ThemeLoadButton(PartyPlayer.Player2Name, 'PartyPlayerPlayer2Name');
    ThemeLoadButton(PartyPlayer.Player3Name, 'PartyPlayerPlayer3Name');
    ThemeLoadButton(PartyPlayer.Player4Name, 'PartyPlayerPlayer4Name');

    ThemeLoadButton(PartyPlayer.Team2Name, 'PartyPlayerTeam2Name');
    ThemeLoadButton(PartyPlayer.Player5Name, 'PartyPlayerPlayer5Name');
    ThemeLoadButton(PartyPlayer.Player6Name, 'PartyPlayerPlayer6Name');
    ThemeLoadButton(PartyPlayer.Player7Name, 'PartyPlayerPlayer7Name');
    ThemeLoadButton(PartyPlayer.Player8Name, 'PartyPlayerPlayer8Name');

    ThemeLoadButton(PartyPlayer.Team3Name, 'PartyPlayerTeam3Name');
    ThemeLoadButton(PartyPlayer.Player9Name, 'PartyPlayerPlayer9Name');
    ThemeLoadButton(PartyPlayer.Player10Name, 'PartyPlayerPlayer10Name');
    ThemeLoadButton(PartyPlayer.Player11Name, 'PartyPlayerPlayer11Name');
    ThemeLoadButton(PartyPlayer.Player12Name, 'PartyPlayerPlayer12Name');

    {ThemeLoadButton(ButtonNext, 'PartyPlayerButtonNext');
    ThemeLoadButton(ButtonPrev, 'PartyPlayerButtonPrev');}
  end;
      
    ThemeIni.Free;
  end;
end;

procedure TTheme.ThemeLoadBasic(Theme: TThemeBasic; Name: string);
begin
  ThemeLoadBackground(Theme.Background, Name);
  ThemeLoadTexts(Theme.Text, Name + 'Text');
  ThemeLoadStatics(Theme.Static, Name + 'Static');
end;

procedure TTheme.ThemeLoadBackground(var ThemeBackground: TThemeBackground; Name: string);
begin
  ThemeBackground.Tex := ThemeIni.ReadString(Name + 'Background', 'Tex', '');
end;

procedure TTheme.ThemeLoadText(var ThemeText: TThemeText; Name: string);
var
  C:    integer;
begin
  DecimalSeparator := '.';
  ThemeText.X := ThemeIni.ReadInteger(Name, 'X', 0);
  ThemeText.Y := ThemeIni.ReadInteger(Name, 'Y', 0);
  ThemeText.W := ThemeIni.ReadInteger(Name, 'W', 0);

  ThemeText.ColR := ThemeIni.ReadFloat(Name, 'ColR', 0);
  ThemeText.ColG := ThemeIni.ReadFloat(Name, 'ColG', 0);
  ThemeText.ColB := ThemeIni.ReadFloat(Name, 'ColB', 0);

  ThemeText.Font := ThemeIni.ReadInteger(Name, 'Font', 0);
  ThemeText.Size := ThemeIni.ReadInteger(Name, 'Size', 0);
  ThemeText.Align := ThemeIni.ReadInteger(Name, 'Align', 0);

  {{$IFDEF TRANSLATE}
  ThemeText.Text := Language.Translate(ThemeIni.ReadString(Name, 'Text', ''));
  {{$ELSE}{
  ThemeText.Text := ThemeIni.ReadString(Name, 'Text', '');
  {$ENDIF}

  ThemeText.Color := ThemeIni.ReadString(Name, 'Color', '');

  C := ColorExists(ThemeText.Color);
  if C >= 0 then begin
    ThemeText.ColR := Color[C].RGB.R;
    ThemeText.ColG := Color[C].RGB.G;
    ThemeText.ColB := Color[C].RGB.B;
  end;

  DecimalSeparator := ',';
end;

procedure TTheme.ThemeLoadTexts(var ThemeText: AThemeText; Name: string);
var
  T:      integer;
begin
  T := 1;
  while ThemeIni.SectionExists(Name + IntToStr(T)) do begin
    SetLength(ThemeText, T);
    ThemeLoadText(ThemeText[T-1], Name + IntToStr(T));
    Inc(T);
  end;
end;

procedure TTheme.ThemeLoadStatic(var ThemeStatic: TThemeStatic; Name: string);
var
  C:  integer;
begin
  DecimalSeparator := '.';

  ThemeStatic.Tex := ThemeIni.ReadString(Name, 'Tex', '');

  ThemeStatic.X := ThemeIni.ReadInteger(Name, 'X', 0);
  ThemeStatic.Y := ThemeIni.ReadInteger(Name, 'Y', 0);
  ThemeStatic.Z := ThemeIni.ReadFloat(Name, 'Z', 0);
  ThemeStatic.W := ThemeIni.ReadInteger(Name, 'W', 0);
  ThemeStatic.H := ThemeIni.ReadInteger(Name, 'H', 0);

  ThemeStatic.Typ := ThemeIni.ReadString(Name, 'Type', '');
  ThemeStatic.Color := ThemeIni.ReadString(Name, 'Color', '');

  C := ColorExists(ThemeStatic.Color);
  if C >= 0 then begin
    ThemeStatic.ColR := Color[C].RGB.R;
    ThemeStatic.ColG := Color[C].RGB.G;
    ThemeStatic.ColB := Color[C].RGB.B;
  end;

  ThemeStatic.TexX1 := ThemeIni.ReadFloat(Name, 'TexX1', 0);
  ThemeStatic.TexY1 := ThemeIni.ReadFloat(Name, 'TexY1', 0);
  ThemeStatic.TexX2 := ThemeIni.ReadFloat(Name, 'TexX2', 1);
  ThemeStatic.TexY2 := ThemeIni.ReadFloat(Name, 'TexY2', 1);

  DecimalSeparator := ',';
end;

procedure TTheme.ThemeLoadStatics(var ThemeStatic: AThemeStatic; Name: string);
var
  S:      integer;
begin
  S := 1;
  while ThemeIni.SectionExists(Name + IntToStr(S)) do begin
    SetLength(ThemeStatic, S);
    ThemeLoadStatic(ThemeStatic[S-1], Name + IntToStr(S));
    Inc(S);
  end;
end;

procedure TTheme.ThemeLoadButton(var ThemeButton: TThemeButton; Name: string);
var
  C:      integer;
  TLen:   integer;
  T:      integer;
begin
  DecimalSeparator := '.';
  ThemeButton.Tex := ThemeIni.ReadString(Name, 'Tex', '');
  ThemeButton.X := ThemeIni.ReadInteger(Name, 'X', 0);
  ThemeButton.Y := ThemeIni.ReadInteger(Name, 'Y', 0);
  ThemeButton.W := ThemeIni.ReadInteger(Name, 'W', 0);
  ThemeButton.H := ThemeIni.ReadInteger(Name, 'H', 0);

  ThemeButton.Typ := ThemeIni.ReadString(Name, 'Type', '');

  //Reflection Mod
  ThemeButton.Reflection := (ThemeIni.ReadInteger(Name, 'Reflection', 0) = 1);

  ThemeButton.ColR := ThemeIni.ReadFloat(Name, 'ColR', 1);
  ThemeButton.ColG := ThemeIni.ReadFloat(Name, 'ColG', 1);
  ThemeButton.ColB := ThemeIni.ReadFloat(Name, 'ColB', 1);
  ThemeButton.Int :=  ThemeIni.ReadFloat(Name, 'Int', 1);
  ThemeButton.DColR := ThemeIni.ReadFloat(Name, 'DColR', 1);
  ThemeButton.DColG := ThemeIni.ReadFloat(Name, 'DColG', 1);
  ThemeButton.DColB := ThemeIni.ReadFloat(Name, 'DColB', 1);
  ThemeButton.DInt :=  ThemeIni.ReadFloat(Name, 'DInt', 1);

  ThemeButton.Color := ThemeIni.ReadString(Name, 'Color', '');
  C := ColorExists(ThemeButton.Color);
  if C >= 0 then begin
    ThemeButton.ColR := Color[C].RGB.R;
    ThemeButton.ColG := Color[C].RGB.G;
    ThemeButton.ColB := Color[C].RGB.B;
  end;

  ThemeButton.DColor := ThemeIni.ReadString(Name, 'DColor', '');
  C := ColorExists(ThemeButton.DColor);
  if C >= 0 then begin
    ThemeButton.DColR := Color[C].RGB.R;
    ThemeButton.DColG := Color[C].RGB.G;
    ThemeButton.DColB := Color[C].RGB.B;
  end;

  TLen := ThemeIni.ReadInteger(Name, 'Texts', 0);
  SetLength(ThemeButton.Text, TLen);
  for T := 1 to TLen do
    ThemeLoadText(ThemeButton.Text[T-1], Name + 'Text' + IntToStr(T));

  DecimalSeparator := ',';
end;

procedure TTheme.ThemeLoadSelect(var ThemeSelect: TThemeSelect; Name: string);
var
  C:    integer;
begin
  DecimalSeparator := '.';

  {{$IFDEF TRANSLATE}
  ThemeSelect.Text := Language.Translate(ThemeIni.ReadString(Name, 'Text', ''));
  {{$ELSE}{
  ThemeSelect.Text := ThemeIni.ReadString(Name, 'Text', '');
  {$ENDIF}

  ThemeSelect.Tex := {Skin.SkinPath + }ThemeIni.ReadString(Name, 'Tex', '');
  ThemeSelect.TexSBG := {Skin.SkinPath + }ThemeIni.ReadString(Name, 'TexSBG', '');

  ThemeSelect.X := ThemeIni.ReadInteger(Name, 'X', 0);
  ThemeSelect.Y := ThemeIni.ReadInteger(Name, 'Y', 0);
  ThemeSelect.W := ThemeIni.ReadInteger(Name, 'W', 0);
  ThemeSelect.H := ThemeIni.ReadInteger(Name, 'H', 0);
  ThemeSelect.SkipX := ThemeIni.ReadInteger(Name, 'SkipX', 0);


  LoadColor(ThemeSelect.ColR, ThemeSelect.ColG,  ThemeSelect.ColB, ThemeIni.ReadString(Name, 'Color', ''));
  ThemeSelect.Int :=  ThemeIni.ReadFloat(Name, 'Int', 1);
  LoadColor(ThemeSelect.DColR, ThemeSelect.DColG,  ThemeSelect.DColB, ThemeIni.ReadString(Name, 'DColor', ''));
  ThemeSelect.DInt :=  ThemeIni.ReadFloat(Name, 'DInt', 1);

  LoadColor(ThemeSelect.TColR, ThemeSelect.TColG,  ThemeSelect.TColB, ThemeIni.ReadString(Name, 'TColor', ''));
  ThemeSelect.TInt :=  ThemeIni.ReadFloat(Name, 'TInt', 1);
  LoadColor(ThemeSelect.TDColR, ThemeSelect.TDColG,  ThemeSelect.TDColB, ThemeIni.ReadString(Name, 'TDColor', ''));
  ThemeSelect.TDInt :=  ThemeIni.ReadFloat(Name, 'TDInt', 1);

  LoadColor(ThemeSelect.SBGColR, ThemeSelect.SBGColG,  ThemeSelect.SBGColB, ThemeIni.ReadString(Name, 'SBGColor', ''));
  ThemeSelect.SBGInt :=  ThemeIni.ReadFloat(Name, 'SBGInt', 1);
  LoadColor(ThemeSelect.SBGDColR, ThemeSelect.SBGDColG,  ThemeSelect.SBGDColB, ThemeIni.ReadString(Name, 'SBGDColor', ''));
  ThemeSelect.SBGDInt :=  ThemeIni.ReadFloat(Name, 'SBGDInt', 1);

  LoadColor(ThemeSelect.STColR, ThemeSelect.STColG,  ThemeSelect.STColB, ThemeIni.ReadString(Name, 'STColor', ''));
  ThemeSelect.STInt :=  ThemeIni.ReadFloat(Name, 'STInt', 1);
  LoadColor(ThemeSelect.STDColR, ThemeSelect.STDColG,  ThemeSelect.STDColB, ThemeIni.ReadString(Name, 'STDColor', ''));
  ThemeSelect.STDInt :=  ThemeIni.ReadFloat(Name, 'STDInt', 1);


  DecimalSeparator := ',';
end;

procedure TTheme.ThemeLoadSelectSlide(var ThemeSelectS: TThemeSelectSlide; Name: string);
var
  C:    integer;
begin
  DecimalSeparator := '.';

  {{$IFDEF TRANSLATE}
  ThemeSelectS.Text := Language.Translate(ThemeIni.ReadString(Name, 'Text', ''));
  {{$ELSE}{
  ThemeSelectS.Text := ThemeIni.ReadString(Name, 'Text', '');
  {$ENDIF}

  ThemeSelectS.Tex := {Skin.SkinPath + }ThemeIni.ReadString(Name, 'Tex', '');
  ThemeSelectS.TexSBG := {Skin.SkinPath + }ThemeIni.ReadString(Name, 'TexSBG', '');

  ThemeSelectS.X := ThemeIni.ReadInteger(Name, 'X', 0);
  ThemeSelectS.Y := ThemeIni.ReadInteger(Name, 'Y', 0);
  ThemeSelectS.W := ThemeIni.ReadInteger(Name, 'W', 0);
  ThemeSelectS.H := ThemeIni.ReadInteger(Name, 'H', 0);
  ThemeSelectS.SkipX := ThemeIni.ReadInteger(Name, 'SkipX', 0);

  ThemeSelectS.SBGW := ThemeIni.ReadInteger(Name, 'SBGW', 450);

  LoadColor(ThemeSelectS.ColR, ThemeSelectS.ColG,  ThemeSelectS.ColB, ThemeIni.ReadString(Name, 'Color', ''));
  ThemeSelectS.Int :=  ThemeIni.ReadFloat(Name, 'Int', 1);
  LoadColor(ThemeSelectS.DColR, ThemeSelectS.DColG,  ThemeSelectS.DColB, ThemeIni.ReadString(Name, 'DColor', ''));
  ThemeSelectS.DInt :=  ThemeIni.ReadFloat(Name, 'DInt', 1);

  LoadColor(ThemeSelectS.TColR, ThemeSelectS.TColG,  ThemeSelectS.TColB, ThemeIni.ReadString(Name, 'TColor', ''));
  ThemeSelectS.TInt :=  ThemeIni.ReadFloat(Name, 'TInt', 1);
  LoadColor(ThemeSelectS.TDColR, ThemeSelectS.TDColG,  ThemeSelectS.TDColB, ThemeIni.ReadString(Name, 'TDColor', ''));
  ThemeSelectS.TDInt :=  ThemeIni.ReadFloat(Name, 'TDInt', 1);

  LoadColor(ThemeSelectS.SBGColR, ThemeSelectS.SBGColG,  ThemeSelectS.SBGColB, ThemeIni.ReadString(Name, 'SBGColor', ''));
  ThemeSelectS.SBGInt :=  ThemeIni.ReadFloat(Name, 'SBGInt', 1);
  LoadColor(ThemeSelectS.SBGDColR, ThemeSelectS.SBGDColG,  ThemeSelectS.SBGDColB, ThemeIni.ReadString(Name, 'SBGDColor', ''));
  ThemeSelectS.SBGDInt :=  ThemeIni.ReadFloat(Name, 'SBGDInt', 1);

  LoadColor(ThemeSelectS.STColR, ThemeSelectS.STColG,  ThemeSelectS.STColB, ThemeIni.ReadString(Name, 'STColor', ''));
  ThemeSelectS.STInt :=  ThemeIni.ReadFloat(Name, 'STInt', 1);
  LoadColor(ThemeSelectS.STDColR, ThemeSelectS.STDColG,  ThemeSelectS.STDColB, ThemeIni.ReadString(Name, 'STDColor', ''));
  ThemeSelectS.STDInt :=  ThemeIni.ReadFloat(Name, 'STDInt', 1);


  DecimalSeparator := ',';
end;

procedure TTheme.LoadColors;
var
  SL:     TStringList;
  C:      integer;
  S:      string;
  Col:    integer;
  RGB:    TRGB;
begin
  SL := TStringList.Create;
  ThemeIni.ReadSection('Colors', SL);

  // normal colors
  SetLength(Color, SL.Count);
  for C := 0 to SL.Count-1 do begin
    Color[C].Name := SL.Strings[C];

    S := ThemeIni.ReadString('Colors', SL.Strings[C], '');

    Color[C].RGB.R := StrToInt(Copy(S, 1, Pos(' ' , S)-1))/255;
    Delete(S, 1, Pos(' ', S));

    Color[C].RGB.G := StrToInt(Copy(S, 1, Pos(' ' , S)-1))/255;
    Delete(S, 1, Pos(' ', S));

    Color[C].RGB.B := StrToInt(S)/255;
  end;

  // skin color
  SetLength(Color, SL.Count + 3);
  C := SL.Count;
  Color[C].Name := 'ColorDark';
  Color[C].RGB := GetSystemColor(Skin.Color);  //Ini.Color);

  C := C+1;
  Color[C].Name := 'ColorLight';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

  C := C+1;
  Color[C].Name := 'ColorLightest';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

  // players colors
  SetLength(Color, Length(Color)+18);

  // P1
  C := C+1;
  Color[C].Name := 'P1Dark';
  Color[C].RGB := GetSystemColor(0); // 0 - blue

  C := C+1;
  Color[C].Name := 'P1Light';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

  C := C+1;
  Color[C].Name := 'P1Lightest';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

  // P2
  C := C+1;
  Color[C].Name := 'P2Dark';
  Color[C].RGB := GetSystemColor(3); // 3 - red

  C := C+1;
  Color[C].Name := 'P2Light';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

  C := C+1;
  Color[C].Name := 'P2Lightest';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

  // P3
  C := C+1;
  Color[C].Name := 'P3Dark';
  Color[C].RGB := GetSystemColor(1); // 1 - green

  C := C+1;
  Color[C].Name := 'P3Light';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

  C := C+1;
  Color[C].Name := 'P3Lightest';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

  // P4
  C := C+1;
  Color[C].Name := 'P4Dark';
  Color[C].RGB := GetSystemColor(4); // 4 - brown

  C := C+1;
  Color[C].Name := 'P4Light';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

  C := C+1;
  Color[C].Name := 'P4Lightest';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

  // P5
  C := C+1;
  Color[C].Name := 'P5Dark';
  Color[C].RGB := GetSystemColor(5); // 5 - yellow

  C := C+1;
  Color[C].Name := 'P5Light';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

  C := C+1;
  Color[C].Name := 'P5Lightest';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

  // P6
  C := C+1;
  Color[C].Name := 'P6Dark';
  Color[C].RGB := GetSystemColor(6); // 6 - violet

  C := C+1;
  Color[C].Name := 'P6Light';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

  C := C+1;
  Color[C].Name := 'P6Lightest';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);


  SL.Free;
end;

function ColorExists(Name: string): integer;
var
  C:  integer;
begin
  Result := -1;
  for C := 0 to High(Color) do
    if Color[C].Name = Name then Result := C;
end;

procedure LoadColor(var R, G, B: real; ColorName: string);
var
  C:    integer;
begin
  C := ColorExists(ColorName);
  if C >= 0 then begin
    R := Color[C].RGB.R;
    G := Color[C].RGB.G;
    B := Color[C].RGB.B;
  end;
end;

function GetSystemColor(Color: integer): TRGB;
begin
  case Color of
    0:  begin
          // blue
          Result.R := 71/255;
          Result.G := 175/255;
          Result.B := 247/255;
        end;
    1:  begin
          // green
          Result.R := 63/255;
          Result.G := 191/255;
          Result.B := 63/255;
        end;
    2:  begin
          // pink
          Result.R := 255/255;
{          Result.G := 63/255;
          Result.B := 192/255;}
          Result.G := 175/255;
          Result.B := 247/255;
        end;
    3:  begin
          // red
          Result.R := 247/255;
          Result.G := 71/255;
          Result.B := 71/255;
        end;
        //'Violet', 'Orange', 'Yellow', 'Brown', 'Black'
        //New Theme-Color Patch
    4:  begin
          // violet
          Result.R := 230/255;
          Result.G := 63/255;
          Result.B := 230/255;
        end;
    5:  begin
          // orange
          Result.R := 255/255;
          Result.G := 144/255;
          Result.B := 0;
        end;
    6:  begin
          // yellow
          Result.R := 230/255;
          Result.G := 230/255;
          Result.B := 95/255;
        end;
    7:  begin
          // brown
          Result.R := 192/255;
          Result.G := 127/255;
          Result.B := 31/255;
        end;
    8:  begin
          // black
          Result.R := 0;
          Result.G := 0;
          Result.B := 0;
        end;
    //New Theme-Color Patch End

    end;

  // pink
//  Col := clRed;
//  Color[C].ColR := (32 + Col and $FF) / (255 + 32);
//  Color[C].ColG := (32 + Col div 256 and $FF) / (255 + 32);
//  Color[C].ColB := (32 + Col div (256*256) and $FF) / (255 + 32);

  // purple
//  Color[C].ColR := 220/255;
//  Color[C].ColG := 95/255;
//  Color[C].ColB := 220/255;}

end;

function ColorSqrt(RGB: TRGB): TRGB;
begin
  Result.R := sqrt(RGB.R);
  Result.G := sqrt(RGB.G);
  Result.B := sqrt(RGB.B);
end;

procedure TTheme.ThemeSave(FileName: string);
var
  I:    integer;
begin
  {$IFDEF THEMESAVE}
  ThemeIni := TIniFile.Create(FileName);
  {$ELSE}
  ThemeIni := TMemIniFile.Create(FileName);
  {$ENDIF}

  ThemeSaveBasic(Loading, 'Loading');

  ThemeSaveBasic(Main, 'Main');
  ThemeSaveText(Main.TextDescription, 'MainTextDescription');
  ThemeSaveText(Main.TextDescriptionLong, 'MainTextDescriptionLong');
  ThemeSaveButton(Main.ButtonSolo, 'MainButtonSolo');
  ThemeSaveButton(Main.ButtonEditor, 'MainButtonEditor');
  ThemeSaveButton(Main.ButtonOptions, 'MainButtonOptions');
  ThemeSaveButton(Main.ButtonExit, 'MainButtonExit');

  ThemeSaveBasic(Name, 'Name');
  for I := 1 to 6 do
    ThemeSaveButton(Name.ButtonPlayer[I], 'NameButtonPlayer' + IntToStr(I));

  ThemeSaveBasic(Level, 'Level');
  ThemeSaveButton(Level.ButtonEasy, 'LevelButtonEasy');
  ThemeSaveButton(Level.ButtonMedium, 'LevelButtonMedium');
  ThemeSaveButton(Level.ButtonHard, 'LevelButtonHard');

  ThemeSaveBasic(Song, 'Song');
  ThemeSaveText(Song.TextArtist, 'SongTextArtist');
  ThemeSaveText(Song.TextTitle, 'SongTextTitle');
  ThemeSaveText(Song.TextNumber, 'SongTextNumber');

  //Show CAt in Top Left Mod
  ThemeSaveText(Song.TextCat, 'SongTextCat');
  ThemeSaveStatic(Song.StaticCat, 'SongStaticCat');

  ThemeSaveBasic(Sing, 'Sing');
  ThemeSaveStatic(Sing.StaticP1, 'SingP1Static');



  //ScoreBG Mod
  ThemeSaveStatic(Sing.StaticP1ScoreBG, 'SingP1Static2');
  //end ScoreBG Mod



  ThemeSaveText(Sing.TextP1, 'SingP1Text');
  ThemeSaveText(Sing.TextP1Score, 'SingP1TextScore');

  ThemeSaveStatic(Sing.StaticP2R, 'SingP2RStatic');



  //ScoreBG Mod
  ThemeSaveStatic(Sing.StaticP2RScoreBG, 'SingP2RStatic2');
  //end ScoreBG Mod



  ThemeSaveText(Sing.TextP2R, 'SingP2RText');
  ThemeSaveText(Sing.TextP2RScore, 'SingP2RTextScore');

  ThemeSaveStatic(Sing.StaticP2M, 'SingP2MStatic');



  //ScoreBG Mod
  ThemeSaveStatic(Sing.StaticP2MScoreBG, 'SingP2MStatic2');
  //end ScoreBG Mod




  ThemeSaveText(Sing.TextP2M, 'SingP2MText');
  ThemeSaveText(Sing.TextP2MScore, 'SingP2MTextScore');

  ThemeSaveStatic(Sing.StaticP3R, 'SingP3RStatic');



  //ScoreBG Mod
  ThemeSaveStatic(Sing.StaticP3RScoreBG, 'SingP3RStatic2');
  //end ScoreBG Mod




  ThemeSaveText(Sing.TextP3R, 'SingP3RText');
  ThemeSaveText(Sing.TextP3RScore, 'SingP3RTextScore');

  ThemeSaveBasic(Score, 'Score');
  ThemeSaveText(Score.TextArtist, 'ScoreTextArtist');
  ThemeSaveText(Score.TextTitle, 'ScoreTextTitle');

  for I := 1 to 6 do begin
    ThemeSaveStatics(Score.PlayerStatic[I], 'ScorePlayer' + IntToStr(I) + 'Static');

    ThemeSaveText(Score.TextName[I], 'ScoreTextName' + IntToStr(I));
    ThemeSaveText(Score.TextScore[I], 'ScoreTextScore' + IntToStr(I));
    ThemeSaveText(Score.TextNotes[I], 'ScoreTextNotes' + IntToStr(I));
    ThemeSaveText(Score.TextNotesScore[I], 'ScoreTextNotesScore' + IntToStr(I));
    ThemeSaveText(Score.TextLineBonus[I], 'ScoreTextLineBonus' + IntToStr(I));
    ThemeSaveText(Score.TextLineBonusScore[I], 'ScoreTextLineBonusScore' + IntToStr(I));
    ThemeSaveText(Score.TextGoldenNotes[I], 'ScoreTextGoldenNotes' + IntToStr(I));
    ThemeSaveText(Score.TextGoldenNotesScore[I], 'ScoreTextGoldenNotesScore' + IntToStr(I));
    ThemeSaveText(Score.TextTotal[I], 'ScoreTextTotal' + IntToStr(I));
    ThemeSaveText(Score.TextTotalScore[I], 'ScoreTextTotalScore' + IntToStr(I));

    ThemeSaveStatic(Score.StaticBackLevel[I], 'ScoreStaticBackLevel' + IntToStr(I));
    ThemeSaveStatic(Score.StaticBackLevelRound[I], 'ScoreStaticBackLevelRound' + IntToStr(I));
    ThemeSaveStatic(Score.StaticLevel[I], 'ScoreStaticLevel' + IntToStr(I));
    ThemeSaveStatic(Score.StaticLevelRound[I], 'ScoreStaticLevelRound' + IntToStr(I));
  end;

  ThemeSaveBasic(Top5, 'Top5');
  ThemeSaveText(Top5.TextLevel, 'Top5TextLevel');
  ThemeSaveText(Top5.TextArtistTitle, 'Top5TextArtistTitle');
  ThemeSaveStatics(Top5.StaticNumber, 'Top5StaticNumber');
  ThemeSaveTexts(Top5.TextNumber, 'Top5TextNumber');
  ThemeSaveTexts(Top5.TextName, 'Top5TextName');
  ThemeSaveTexts(Top5.TextScore, 'Top5TextScore');


  ThemeIni.Free;
end;

procedure TTheme.ThemeSaveBasic(Theme: TThemeBasic; Name: string);
begin
  ThemeIni.WriteInteger(Name, 'Texts', Length(Theme.Text));

  ThemeSaveBackground(Theme.Background, Name + 'Background');
  ThemeSaveStatics(Theme.Static, Name + 'Static');
  ThemeSaveTexts(Theme.Text, Name + 'Text');
end;

procedure TTheme.ThemeSaveBackground(ThemeBackground: TThemeBackground; Name: string);
begin
  if ThemeBackground.Tex <> '' then
    ThemeIni.WriteString(Name, 'Tex', ThemeBackground.Tex)
  else begin
    ThemeIni.EraseSection(Name);
  end;
end;

procedure TTheme.ThemeSaveStatic(ThemeStatic: TThemeStatic; Name: string);
begin
  DecimalSeparator := '.';
  ThemeIni.WriteInteger(Name, 'X', ThemeStatic.X);
  ThemeIni.WriteInteger(Name, 'Y', ThemeStatic.Y);
  ThemeIni.WriteInteger(Name, 'W', ThemeStatic.W);
  ThemeIni.WriteInteger(Name, 'H', ThemeStatic.H);

  ThemeIni.WriteString(Name, 'Tex', ThemeStatic.Tex);
  ThemeIni.WriteString(Name, 'Type', ThemeStatic.Typ);
  ThemeIni.WriteString(Name, 'Color', ThemeStatic.Color);

  ThemeIni.WriteFloat(Name, 'TexX1', ThemeStatic.TexX1);
  ThemeIni.WriteFloat(Name, 'TexY1', ThemeStatic.TexY1);
  ThemeIni.WriteFloat(Name, 'TexX2', ThemeStatic.TexX2);
  ThemeIni.WriteFloat(Name, 'TexY2', ThemeStatic.TexY2);

  DecimalSeparator := ',';
end;

procedure TTheme.ThemeSaveStatics(ThemeStatic: AThemeStatic; Name: string);
var
  S:      integer;
begin
  for S := 0 to Length(ThemeStatic)-1 do
    ThemeSaveStatic(ThemeStatic[S], Name + {'Static' +} IntToStr(S+1));

  ThemeIni.EraseSection(Name + {'Static' + }IntToStr(S+1));
end;

procedure TTheme.ThemeSaveText(ThemeText: TThemeText; Name: string);
begin
  DecimalSeparator := '.';
  ThemeIni.WriteInteger(Name, 'X', ThemeText.X);
  ThemeIni.WriteInteger(Name, 'Y', ThemeText.Y);

  ThemeIni.WriteInteger(Name, 'Font', ThemeText.Font);
  ThemeIni.WriteInteger(Name, 'Size', ThemeText.Size);
  ThemeIni.WriteInteger(Name, 'Align', ThemeText.Align);

  ThemeIni.WriteString(Name, 'Text', ThemeText.Text);
  ThemeIni.WriteString(Name, 'Color', ThemeText.Color);

  DecimalSeparator := ',';
end;

procedure TTheme.ThemeSaveTexts(ThemeText: AThemeText; Name: string);
var
  T:      integer;
begin
  for T := 0 to Length(ThemeText)-1 do
    ThemeSaveText(ThemeText[T], Name + {'Text' + }IntToStr(T+1));

  ThemeIni.EraseSection(Name + {'Text' + }IntToStr(T+1));
end;

procedure TTheme.ThemeSaveButton(ThemeButton: TThemeButton; Name: string);
var
  T:    integer;
begin
  DecimalSeparator := '.';
  ThemeIni.WriteString(Name, 'Tex', ThemeButton.Tex);
  ThemeIni.WriteInteger(Name, 'X', ThemeButton.X);
  ThemeIni.WriteInteger(Name, 'Y', ThemeButton.Y);
  ThemeIni.WriteInteger(Name, 'W', ThemeButton.W);
  ThemeIni.WriteInteger(Name, 'H', ThemeButton.H);

  ThemeIni.WriteString(Name, 'Type', ThemeButton.Typ);
  ThemeIni.WriteInteger(Name, 'Texts', Length(ThemeButton.Text));

  ThemeIni.WriteString(Name, 'Color', ThemeButton.Color);

{  ThemeButton.ColR := ThemeIni.ReadFloat(Name, 'ColR', 1);
  ThemeButton.ColG := ThemeIni.ReadFloat(Name, 'ColG', 1);
  ThemeButton.ColB := ThemeIni.ReadFloat(Name, 'ColB', 1);
  ThemeButton.Int :=  ThemeIni.ReadFloat(Name, 'Int', 1);
  ThemeButton.DColR := ThemeIni.ReadFloat(Name, 'DColR', 1);
  ThemeButton.DColG := ThemeIni.ReadFloat(Name, 'DColG', 1);
  ThemeButton.DColB := ThemeIni.ReadFloat(Name, 'DColB', 1);
  ThemeButton.DInt :=  ThemeIni.ReadFloat(Name, 'DInt', 1);}

{  C := ColorExists(ThemeIni.ReadString(Name, 'Color', ''));
  if C >= 0 then begin
    ThemeButton.ColR := Color[C].RGB.R;
    ThemeButton.ColG := Color[C].RGB.G;
    ThemeButton.ColB := Color[C].RGB.B;
  end;

  C := ColorExists(ThemeIni.ReadString(Name, 'DColor', ''));
  if C >= 0 then begin
    ThemeButton.DColR := Color[C].RGB.R;
    ThemeButton.DColG := Color[C].RGB.G;
    ThemeButton.DColB := Color[C].RGB.B;
  end;}

  for T := 0 to High(ThemeButton.Text) do
    ThemeSaveText(ThemeButton.Text[T], Name + 'Text' + IntToStr(T+1));

  DecimalSeparator := ',';
end;


end.
