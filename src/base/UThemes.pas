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

unit UThemes;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  ULog,
  IniFiles,
  SysUtils,
  Classes,
  UTexture;

type
  TRGB = record
    R: single;
    G: single;
    B: single;
  end;

  TRGBA = record
    R, G, B, A: double;
  end;

type
  TBackgroundType =
    (bgtNone, bgtColor, bgtTexture, bgtVideo, bgtFade, bgtAuto);

const
  BGT_Names: array [TBackgroundType] of string =
    ('none', 'color', 'texture', 'video', 'fade', 'auto');

type
  TThemeBackground = record
    BGType: TBackgroundType;
    Color:  TRGB;
    Tex:    string;
    Alpha:  real;
  end;

const
  //Defaul Background for Screens w/o Theme e.g. editor
  DEFAULT_BACKGROUND: TThemeBackground = (
    BGType: bgtColor;
    Color:  (R:1; G:1; B:1);
    Tex:    '';
    Alpha:  1.0
  );


type
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
    Typ:    TTextureType;
    TexX1:  real;
    TexY1:  real;
    TexX2:  real;
    TexY2:  real;
    //Reflection
    Reflection:           boolean;
    Reflectionspacing:    real;
  end;
  AThemeStatic = array of TThemeStatic;

  TThemeText = record
    X:      integer;
    Y:      integer;
    W:      integer;
    Z:      real;
    Color:  string;
    ColR:   real;
    ColG:   real;
    ColB:   real;
    Font:   integer;
    Size:   integer;
    Align:  integer;
    Text:   string;
    //Reflection
    Reflection:           boolean;
    ReflectionSpacing:    real;
  end;
  AThemeText = array of TThemeText;

  TThemeButton = record
    Text:   AThemeText;
    X:      integer;
    Y:      integer;
    Z:      real;
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
    Typ:    TTextureType;

    Visible: boolean;

    //Reflection Mod
    Reflection:           boolean;
    Reflectionspacing:    real;
    //Fade Mod
    SelectH:    integer;
    SelectW:    integer;
    Fade:       boolean;
    FadeText:   boolean;
    DeSelectReflectionspacing : real;
    FadeTex:    string;
    FadeTexPos: integer;

    //Button Collection Mod
    Parent: byte; //Number of the Button Collection this Button is assigned to. IF 0: No Assignement
  end;

  //Button Collection Mod
  TThemeButtonCollection = record
    Style: TThemeButton;
    ChildCount: byte; //No of assigned Childs
    FirstChild: byte; //No of Child on whose Interaction Position the Button should be
  end;

  AThemeButtonCollection = array of TThemeButtonCollection;
  PAThemeButtonCollection = ^AThemeButtonCollection;

  TThemeSelectSlide = record
    Tex:    string;
    TexSBG: string;
    X:      integer;
    Y:      integer;
    W:      integer;
    H:      integer;
    Z:      real;
    SBGW:   integer;

    TextSize: integer;

    showArrows:boolean;
    oneItemOnly:boolean;

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

  TThemeEqualizer = record
    Visible: boolean;
    Direction: boolean;
    Alpha: real;
    X: integer;
    Y: integer;
    Z: real;
    W: integer;
    H: integer;
    Space: integer;
    Bands: integer;
    Length: integer;
    ColR, ColG, ColB: real;
    Reflection:           boolean;
    Reflectionspacing:    real;
  end;

  PThemeBasic = ^TThemeBasic;
  TThemeBasic = class
    Background:       TThemeBackground;
    Text:             AThemeText;
    Static:           AThemeStatic;

    //Button Collection Mod
    ButtonCollection: AThemeButtonCollection;
  end;

  TThemeLoading = class(TThemeBasic)
    StaticAnimation:  TThemeStatic;
    TextLoading:      TThemeText;
  end;

  TThemeMain = class(TThemeBasic)
    ButtonSolo:       TThemeButton;
    ButtonMulti:      TThemeButton;
    ButtonStat:       TThemeButton;
    ButtonEditor:     TThemeButton;
    ButtonOptions:    TThemeButton;
    ButtonExit:       TThemeButton;

    TextDescription:      TThemeText;
    TextDescriptionLong:  TThemeText;
    Description:          array[0..5] of string;
    DescriptionLong:      array[0..5] of string;
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

    //Video Icon Mod
    VideoIcon:        TThemeStatic;

   //Show Cat in TopLeft Mod
    TextCat:          TThemeText;
    StaticCat:        TThemeStatic;

    //Cover Mod
    Cover: record
      Reflections: boolean;
      X: integer;
      Y: integer;
      Z: integer;
      W: integer;
      H: integer;
      Style: integer;
    end;

    //Equalizer Mod
    Equalizer: TThemeEqualizer;


    //Party and Non Party specific Statics and Texts
    StaticParty:    AThemeStatic;
    TextParty:      AThemeText;

    StaticNonParty: AThemeStatic;
    TextNonParty:   AThemeText;

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

    //TimeBar mod
    StaticTimeProgress:   TThemeStatic;
    TextTimeText      :   TThemeText;
    //eoa TimeBar mod

    StaticP1:         TThemeStatic;
    TextP1:           TThemeText;
    StaticP1ScoreBG:  TThemeStatic; //Static for ScoreBG
    TextP1Score:      TThemeText;

    //moveable singbar mod
    StaticP1SingBar:         TThemeStatic;
    StaticP1ThreePSingBar:   TThemeStatic;
    StaticP1TwoPSingBar:     TThemeStatic;
    StaticP2RSingBar:        TThemeStatic;
    StaticP2MSingBar:        TThemeStatic;
    StaticP3SingBar:         TThemeStatic;
    //eoa moveable singbar

    //added for ps3 skin
    //game in 2/4 player modi
    StaticP1TwoP:         TThemeStatic;
    StaticP1TwoPScoreBG:  TThemeStatic; //Static for ScoreBG
    TextP1TwoP:           TThemeText;
    TextP1TwoPScore:      TThemeText;
    //game in 3/6 player modi
    StaticP1ThreeP:         TThemeStatic;
    StaticP1ThreePScoreBG:  TThemeStatic; //Static for ScoreBG
    TextP1ThreeP:           TThemeText;
    TextP1ThreePScore:      TThemeText;
    //eoa

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

    //Linebonus Translations
    LineBonusText:    array [0..8] of string;

    //Pause Popup
     PausePopUp:      TThemeStatic;
  end;

  TThemeLyricBar = record
     IndicatorYOffset, UpperX, UpperW, UpperY, UpperH,
     LowerX, LowerW, LowerY, LowerH  : integer;
  end;

  TThemeScore = class(TThemeBasic)
    TextArtist:       TThemeText;
    TextTitle:        TThemeText;

    TextArtistTitle:  TThemeText;

    PlayerStatic:     array[1..6] of AThemeStatic;
    PlayerTexts:      array[1..6] of AThemeText;

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

    StaticRatings:        array[1..6] of TThemeStatic;

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
    ButtonAdvanced:   TThemeButton;
    ButtonExit:       TThemeButton;

    TextDescription:      TThemeText;
    Description:          array[0..7] of string;
  end;

  TThemeOptionsGame = class(TThemeBasic)
    SelectPlayers:      TThemeSelectSlide;
    SelectDifficulty:   TThemeSelectSlide;
    SelectLanguage:     TThemeSelectSlide;
    SelectTabs:         TThemeSelectSlide;
    SelectSorting:      TThemeSelectSlide;
    SelectDebug:        TThemeSelectSlide;
    ButtonExit:         TThemeButton;
  end;

  TThemeOptionsGraphics = class(TThemeBasic)
    SelectFullscreen:       TThemeSelectSlide;
    SelectResolution:       TThemeSelectSlide;
    SelectDepth:            TThemeSelectSlide;
    SelectVisualizer:       TThemeSelectSlide;
    SelectOscilloscope:     TThemeSelectSlide;
    SelectLineBonus:        TThemeSelectSlide;
    SelectMovieSize:        TThemeSelectSlide;
    ButtonExit:             TThemeButton;
  end;

  TThemeOptionsSound = class(TThemeBasic)
    SelectMicBoost:              TThemeSelectSlide;
    SelectBackgroundMusic:       TThemeSelectSlide;
    SelectClickAssist:           TThemeSelectSlide;
    SelectBeatClick:             TThemeSelectSlide;
    SelectThreshold:             TThemeSelectSlide;
    SelectSlidePreviewVolume:    TThemeSelectSlide;
    SelectSlidePreviewFading:    TThemeSelectSlide;
    SelectSlideVoicePassthrough: TThemeSelectSlide;
    ButtonExit:                  TThemeButton;
  end;

  TThemeOptionsLyrics = class(TThemeBasic)
    SelectLyricsFont:   TThemeSelectSlide;
    SelectLyricsEffect: TThemeSelectSlide;
//    SelectSolmization:  TThemeSelectSlide;
    SelectNoteLines:    TThemeSelectSlide;
    ButtonExit:         TThemeButton;
  end;

  TThemeOptionsThemes = class(TThemeBasic)
    SelectTheme:        TThemeSelectSlide;
    SelectSkin:         TThemeSelectSlide;
    SelectColor:        TThemeSelectSlide;
    ButtonExit:         TThemeButton;
  end;

  TThemeOptionsRecord = class(TThemeBasic)
    SelectSlideCard:       TThemeSelectSlide;
    SelectSlideInput:      TThemeSelectSlide;
    SelectSlideChannel:    TThemeSelectSlide;
    ButtonExit:            TThemeButton;
  end;

  TThemeOptionsAdvanced = class(TThemeBasic)
    SelectLoadAnimation:  TThemeSelectSlide;
    SelectEffectSing:     TThemeSelectSlide;
    SelectScreenFade:     TThemeSelectSlide;
    SelectLineBonus:      TThemeSelectSlide;
    SelectAskbeforeDel:   TThemeSelectSlide;
    SelectOnSongClick:    TThemeSelectSlide;
    SelectPartyPopup:     TThemeSelectSlide;
    ButtonExit:           TThemeButton;
  end;

  TThemeEdit = class(TThemeBasic)
    ButtonConvert:        TThemeButton;
    ButtonExit:           TThemeButton;

    TextDescription:      TThemeText;
    TextDescriptionLong:  TThemeText;
    Description:          array[0..5] of string;
    DescriptionLong:      array[0..5] of string;
  end;

  //Error- and Check-Popup
  TThemeError = class(TThemeBasic)
    Button1: TThemeButton;
    TextError: TThemeText;
  end;

  TThemeCheck = class(TThemeBasic)
    Button1: TThemeButton;
    Button2: TThemeButton;
    TextCheck: TThemeText;
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

    //Translated Texts
    Songsfound:       string;
    NoSongsfound:     string;
    CatText:          string;
    IType:            array [0..2] of string;
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
    TextTeam1Players:  TThemeText;
    TextTeam2Players:  TThemeText;
    TextTeam3Players:  TThemeText;

    StaticTeam1:       TThemeStatic;
    StaticTeam2:       TThemeStatic;
    StaticTeam3:       TThemeStatic;
    StaticNextPlayer1: TThemeStatic;
    StaticNextPlayer2: TThemeStatic;
    StaticNextPlayer3: TThemeStatic;
  end;

  TThemePartyScore = class(TThemeBasic)
    TextScoreTeam1:    TThemeText;
    TextScoreTeam2:    TThemeText;
    TextScoreTeam3:    TThemeText;
    TextNameTeam1:     TThemeText;
    TextNameTeam2:     TThemeText;
    TextNameTeam3:     TThemeText;
    StaticTeam1:       TThemeStatic;
    StaticTeam1BG:     TThemeStatic;
    StaticTeam1Deco:   TThemeStatic;
    StaticTeam2:       TThemeStatic;
    StaticTeam2BG:     TThemeStatic;
    StaticTeam2Deco:   TThemeStatic;
    StaticTeam3:       TThemeStatic;
    StaticTeam3BG:     TThemeStatic;
    StaticTeam3Deco:   TThemeStatic;

    DecoTextures:      record
      ChangeTextures:  boolean;

      FirstTexture:    string;
      FirstTyp:        TTextureType;
      FirstColor:      string;

      SecondTexture:   string;
      SecondTyp:       TTextureType;
      SecondColor:     string;

      ThirdTexture:    string;
      ThirdTyp:        TTextureType;
      ThirdColor:      string;
    end;


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
    StaticTeam1BG:     TThemeStatic;
    StaticTeam1Deco:   TThemeStatic;
    StaticTeam2:       TThemeStatic;
    StaticTeam2BG:     TThemeStatic;
    StaticTeam2Deco:   TThemeStatic;
    StaticTeam3:       TThemeStatic;
    StaticTeam3BG:     TThemeStatic;
    StaticTeam3Deco:   TThemeStatic;

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

  //Stats Screens
  TThemeStatMain = class(TThemeBasic)
    ButtonScores:     TThemeButton;
    ButtonSingers:    TThemeButton;
    ButtonSongs:      TThemeButton;
    ButtonBands:      TThemeButton;
    ButtonExit:       TThemeButton;

    TextOverview:     TThemeText;
  end;

  TThemeStatDetail = class(TThemeBasic)
    ButtonNext:       TThemeButton;
    ButtonPrev:       TThemeButton;
    ButtonReverse:    TThemeButton;
    ButtonExit:       TThemeButton;

    TextDescription:  TThemeText;
    TextPage:         TThemeText;
    TextList:         AThemeText;

    Description:      array[0..3] of string;
    DescriptionR:     array[0..3] of string;
    FormatStr:        array[0..3] of string;
    PageStr:          string;
  end;

  //Playlist Translations
  TThemePlaylist = record
    CatText:    string;
  end;

  TTheme = class
  private
    {$IFDEF THEMESAVE}
    ThemeIni:         TIniFile;
    {$ELSE}
    ThemeIni:         TMemIniFile;
    {$ENDIF}

    LastThemeBasic:   TThemeBasic;
    procedure CreateThemeObjects();
    
  public
    Loading:          TThemeLoading;
    Main:             TThemeMain;
    Name:             TThemeName;
    Level:            TThemeLevel;
    Song:             TThemeSong;
    Sing:             TThemeSing;
    LyricBar:         TThemeLyricBar;
    Score:            TThemeScore;
    Top5:             TThemeTop5;
    Options:          TThemeOptions;
    OptionsGame:      TThemeOptionsGame;
    OptionsGraphics:  TThemeOptionsGraphics;
    OptionsSound:     TThemeOptionsSound;
    OptionsLyrics:    TThemeOptionsLyrics;
    OptionsThemes:    TThemeOptionsThemes;
    OptionsRecord:    TThemeOptionsRecord;
    OptionsAdvanced:  TThemeOptionsAdvanced;
    //edit
    Edit:             TThemeEdit;
    //error and check popup
    ErrorPopup:       TThemeError;
    CheckPopup:       TThemeCheck;
    //ScreenSong extensions
    SongMenu:         TThemeSongMenu;
    SongJumpto:       TThemeSongJumpTo;
    //Party Screens:
    PartyNewRound:    TThemePartyNewRound;
    PartyScore:       TThemePartyScore;
    PartyWin:         TThemePartyWin;
    PartyOptions:     TThemePartyOptions;
    PartyPlayer:      TThemePartyPlayer;

    //Stats Screens:
    StatMain:         TThemeStatMain;
    StatDetail:       TThemeStatDetail;

    Playlist:         TThemePlaylist;

    ILevel: array[0..2] of string;

    constructor Create(const FileName: string); overload; // Initialize theme system
    constructor Create(const FileName: string; Color: integer); overload; // Initialize theme system with color
    function LoadTheme(FileName: string; sColor: integer): boolean; // Load some theme settings from file

    procedure LoadColors;

    procedure ThemeLoadBasic(Theme: TThemeBasic; const Name: string);
    procedure ThemeLoadBackground(var ThemeBackground: TThemeBackground; const Name: string);
    procedure ThemeLoadText(var ThemeText: TThemeText; const Name: string);
    procedure ThemeLoadTexts(var ThemeText: AThemeText; const Name: string);
    procedure ThemeLoadStatic(var ThemeStatic: TThemeStatic; const Name: string);
    procedure ThemeLoadStatics(var ThemeStatic: AThemeStatic; const Name: string);
    procedure ThemeLoadButton(var ThemeButton: TThemeButton; const Name: string; Collections: PAThemeButtonCollection = nil);
    procedure ThemeLoadButtonCollection(var Collection: TThemeButtonCollection; const Name: string);
    procedure ThemeLoadButtonCollections(var Collections: AThemeButtonCollection; const Name: string);
    procedure ThemeLoadSelectSlide(var ThemeSelectS: TThemeSelectSlide; const Name: string);
    procedure ThemeLoadEqualizer(var ThemeEqualizer: TThemeEqualizer; const Name: string);

    procedure ThemeSave(const FileName: string);
    procedure ThemeSaveBasic(Theme: TThemeBasic; const Name: string);
    procedure ThemeSaveBackground(ThemeBackground: TThemeBackground; const Name: string);
    procedure ThemeSaveStatic(ThemeStatic: TThemeStatic; const Name: string);
    procedure ThemeSaveStatics(ThemeStatic: AThemeStatic; const Name: string);
    procedure ThemeSaveText(ThemeText: TThemeText; const Name: string);
    procedure ThemeSaveTexts(ThemeText: AThemeText; const Name: string);
    procedure ThemeSaveButton(ThemeButton: TThemeButton; const Name: string);
  end;

  TColor = record
    Name:   string;
    RGB:    TRGB;
  end;

procedure glColorRGB(Color: TRGB);  overload;
procedure glColorRGB(Color: TRGB; Alpha: real);  overload;
procedure glColorRGB(Color: TRGBA); overload;
procedure glColorRGB(Color: TRGBA; Alpha: real); overload;

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
  UCommon,
  ULanguage,
  USkins,
  UIni,
  gl,
  glext,
  math;

//-----------
//Helper procs to use TRGB in Opengl ...maybe this should be somewhere else
//-----------
procedure glColorRGB(Color: TRGB);  overload;
begin
  glColor3f(Color.R, Color.G, Color.B);
end;

procedure glColorRGB(Color: TRGB; Alpha: real);  overload;
begin
  glColor4f(Color.R, Color.G, Color.B, Alpha);
end;

procedure glColorRGB(Color: TRGBA); overload;
begin
  glColor4f(Color.R, Color.G, Color.B, Color.A);
end;

procedure glColorRGB(Color: TRGBA; Alpha: real); overload;
begin
  glColor4f(Color.R, Color.G, Color.B, Min(Color.A, Alpha));
end;

constructor TTheme.Create(const FileName: string);
begin
  Create(FileName, 0);
end;

constructor TTheme.Create(const FileName: string; Color: integer);
begin
  inherited Create();

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
  OptionsAdvanced := TThemeOptionsAdvanced.Create;

  Edit := TThemeEdit.Create;

  ErrorPopup := TThemeError.Create;
  CheckPopup := TThemeCheck.Create;

  SongMenu := TThemeSongMenu.Create;
  SongJumpto := TThemeSongJumpto.Create;
  //Party Screens
  PartyNewRound := TThemePartyNewRound.Create;
  PartyWin := TThemePartyWin.Create;
  PartyScore := TThemePartyScore.Create;
  PartyOptions := TThemePartyOptions.Create;
  PartyPlayer := TThemePartyPlayer.Create;

  //Stats Screens:
  StatMain :=   TThemeStatMain.Create;
  StatDetail := TThemeStatDetail.Create;

  LoadTheme(FileName, Color);

end;

function TTheme.LoadTheme(FileName: string; sColor: integer): boolean;
var
  I:    integer;
begin
  Result := false;

  CreateThemeObjects();

  Log.LogStatus('Loading: '+ FileName, 'TTheme.LoadTheme');

  FileName := AdaptFilePaths(FileName);

  if not FileExists(FileName) then
  begin
    Log.LogError('Theme does not exist ('+ FileName +')', 'TTheme.LoadTheme');
  end;

  if FileExists(FileName) then
  begin
    Result := true;

    {$IFDEF THEMESAVE}
    ThemeIni := TIniFile.Create(FileName);
    {$ELSE}
    ThemeIni := TMemIniFile.Create(FileName);
    {$ENDIF}

    if ThemeIni.ReadString('Theme', 'Name', '') <> '' then
    begin

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
      ThemeLoadText(Loading.TextLoading, 'LoadingTextLoading');
      ThemeLoadStatic(Loading.StaticAnimation, 'LoadingStaticAnimation');

      // Main
      ThemeLoadBasic(Main, 'Main');

      ThemeLoadText(Main.TextDescription, 'MainTextDescription');
      ThemeLoadText(Main.TextDescriptionLong, 'MainTextDescriptionLong');
      ThemeLoadButton(Main.ButtonSolo, 'MainButtonSolo');
      ThemeLoadButton(Main.ButtonMulti, 'MainButtonMulti');
      ThemeLoadButton(Main.ButtonStat, 'MainButtonStats');
      ThemeLoadButton(Main.ButtonEditor, 'MainButtonEditor');
      ThemeLoadButton(Main.ButtonOptions, 'MainButtonOptions');
      ThemeLoadButton(Main.ButtonExit, 'MainButtonExit');

      //Main Desc Text Translation Start

      Main.Description[0] := Language.Translate('SING_SING');
      Main.DescriptionLong[0] := Language.Translate('SING_SING_DESC');
      Main.Description[1] := Language.Translate('SING_MULTI');
      Main.DescriptionLong[1] := Language.Translate('SING_MULTI_DESC');
      Main.Description[2] := Language.Translate('SING_STATS');
      Main.DescriptionLong[2] := Language.Translate('SING_STATS_DESC');
      Main.Description[3] := Language.Translate('SING_EDITOR');
      Main.DescriptionLong[3] := Language.Translate('SING_EDITOR_DESC');
      Main.Description[4] := Language.Translate('SING_GAME_OPTIONS');
      Main.DescriptionLong[4] := Language.Translate('SING_GAME_OPTIONS_DESC');
      Main.Description[5] := Language.Translate('SING_EXIT');
      Main.DescriptionLong[5] := Language.Translate('SING_EXIT_DESC');

      //Main Desc Text Translation End

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

      //Video Icon Mod
      ThemeLoadStatic(Song.VideoIcon, 'SongVideoIcon');

      //Show Cat in TopLeft Mod
      ThemeLoadStatic(Song.StaticCat, 'SongStaticCat');
      ThemeLoadText(Song.TextCat, 'SongTextCat');

      //Load Cover Pos and Size from Theme Mod
      Song.Cover.X := ThemeIni.ReadInteger('SongCover', 'X', 300);
      Song.Cover.Y := ThemeIni.ReadInteger('SongCover', 'Y', 190);
      Song.Cover.W := ThemeIni.ReadInteger('SongCover', 'W', 300);
      Song.Cover.H := ThemeIni.ReadInteger('SongCover', 'H', 200);
      Song.Cover.Style := ThemeIni.ReadInteger('SongCover', 'Style', 4);
      Song.Cover.Reflections := (ThemeIni.ReadInteger('SongCover', 'Reflections', 0) = 1);
      //Load Cover Pos and Size from Theme Mod End

      ThemeLoadEqualizer(Song.Equalizer, 'SongEqualizer');

      //Party and Non Party specific Statics and Texts
      ThemeLoadStatics (Song.StaticParty, 'SongStaticParty');
      ThemeLoadTexts (Song.TextParty, 'SongTextParty');

      ThemeLoadStatics (Song.StaticNonParty, 'SongStaticNonParty');
      ThemeLoadTexts (Song.TextNonParty, 'SongTextNonParty');

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


      //LyricBar      asd
      LyricBar.UpperX := ThemeIni.ReadInteger('SingLyricsUpperBar', 'X', 0);
      LyricBar.UpperW := ThemeIni.ReadInteger('SingLyricsUpperBar', 'W', 0);
      LyricBar.UpperY := ThemeIni.ReadInteger('SingLyricsUpperBar', 'Y', 0);
      LyricBar.UpperH := ThemeIni.ReadInteger('SingLyricsUpperBar', 'H', 0);
      LyricBar.IndicatorYOffset := ThemeIni.ReadInteger('SingLyricsUpperBar', 'IndicatorYOffset', 0);
      LyricBar.LowerX := ThemeIni.ReadInteger('SingLyricsLowerBar', 'X', 0);
      LyricBar.LowerW := ThemeIni.ReadInteger('SingLyricsLowerBar', 'W', 0);
      LyricBar.LowerY := ThemeIni.ReadInteger('SingLyricsLowerBar', 'Y', 0);
      LyricBar.LowerH := ThemeIni.ReadInteger('SingLyricsLowerBar', 'H', 0);

      // Sing
      ThemeLoadBasic(Sing, 'Sing');
      //TimeBar mod
       ThemeLoadStatic(Sing.StaticTimeProgress, 'SingTimeProgress');
       ThemeLoadText(Sing.TextTimeText, 'SingTimeText');
      //eoa TimeBar mod

    //moveable singbar mod
      ThemeLoadStatic(Sing.StaticP1SingBar, 'SingP1SingBar');
      ThemeLoadStatic(Sing.StaticP1TwoPSingBar, 'SingP1TwoPSingBar');
      ThemeLoadStatic(Sing.StaticP1ThreePSingBar, 'SingP1ThreePSingBar');
      ThemeLoadStatic(Sing.StaticP2RSingBar, 'SingP2RSingBar');
      ThemeLoadStatic(Sing.StaticP2MSingBar, 'SingP2MSingBar');
      ThemeLoadStatic(Sing.StaticP3SingBar, 'SingP3SingBar');
    //eoa moveable singbar

      ThemeLoadStatic(Sing.StaticP1, 'SingP1Static');
      ThemeLoadText(Sing.TextP1, 'SingP1Text');
      ThemeLoadStatic(Sing.StaticP1ScoreBG, 'SingP1Static2');
      ThemeLoadText(Sing.TextP1Score, 'SingP1TextScore');
  //Added for ps3 skin
  //This one is shown in 2/4P mode
  //if it exists, otherwise the one Player equivaltents are used
      if (ThemeIni.SectionExists('SingP1TwoPTextScore')) then
      begin
        ThemeLoadStatic(Sing.StaticP1TwoP, 'SingP1TwoPStatic');
        ThemeLoadText(Sing.TextP1TwoP, 'SingP1TwoPText');
        ThemeLoadStatic(Sing.StaticP1TwoPScoreBG, 'SingP1TwoPStatic2');
        ThemeLoadText(Sing.TextP1TwoPScore, 'SingP1TwoPTextScore');
      end
      else
      begin
        Sing.StaticP1TwoP := Sing.StaticP1;
        Sing.TextP1TwoP := Sing.TextP1;
        Sing.StaticP1TwoPScoreBG := Sing.StaticP1ScoreBG;
        Sing.TextP1TwoPScore := Sing.TextP1Score;
      end;

  //This one is shown in 3/6P mode
  //if it exists, otherwise the one Player equivaltents are used
      if (ThemeIni.SectionExists('SingP1TwoPTextScore')) then
      begin
        ThemeLoadStatic(Sing.StaticP1ThreeP, 'SingP1ThreePStatic');
        ThemeLoadText(Sing.TextP1ThreeP, 'SingP1ThreePText');
        ThemeLoadStatic(Sing.StaticP1ThreePScoreBG, 'SingP1ThreePStatic2');
        ThemeLoadText(Sing.TextP1ThreePScore, 'SingP1ThreePTextScore');
      end
      else
      begin
        Sing.StaticP1ThreeP := Sing.StaticP1;
        Sing.TextP1ThreeP := Sing.TextP1;
        Sing.StaticP1ThreePScoreBG := Sing.StaticP1ScoreBG;
        Sing.TextP1ThreePScore := Sing.TextP1Score;
      end;
  //eoa
      ThemeLoadStatic(Sing.StaticP2R, 'SingP2RStatic');
      ThemeLoadText(Sing.TextP2R, 'SingP2RText');
      ThemeLoadStatic(Sing.StaticP2RScoreBG, 'SingP2RStatic2');
      ThemeLoadText(Sing.TextP2RScore, 'SingP2RTextScore');

      ThemeLoadStatic(Sing.StaticP2M, 'SingP2MStatic');
      ThemeLoadText(Sing.TextP2M, 'SingP2MText');
      ThemeLoadStatic(Sing.StaticP2MScoreBG, 'SingP2MStatic2');
      ThemeLoadText(Sing.TextP2MScore, 'SingP2MTextScore');

      ThemeLoadStatic(Sing.StaticP3R, 'SingP3RStatic');
      ThemeLoadText(Sing.TextP3R, 'SingP3RText');
      ThemeLoadStatic(Sing.StaticP3RScoreBG, 'SingP3RStatic2');
      ThemeLoadText(Sing.TextP3RScore, 'SingP3RTextScore');

      //Line Bonus Texts
      Sing.LineBonusText[0] := Language.Translate('POPUP_AWFUL');
      Sing.LineBonusText[1] := Sing.LineBonusText[0];
      Sing.LineBonusText[2] := Language.Translate('POPUP_POOR');
      Sing.LineBonusText[3] := Language.Translate('POPUP_BAD');
      Sing.LineBonusText[4] := Language.Translate('POPUP_NOTBAD');
      Sing.LineBonusText[5] := Language.Translate('POPUP_GOOD');
      Sing.LineBonusText[6] := Language.Translate('POPUP_GREAT');
      Sing.LineBonusText[7] := Language.Translate('POPUP_AWESOME');
      Sing.LineBonusText[8] := Language.Translate('POPUP_PERFECT');

      //PausePopup
      ThemeLoadStatic(Sing.PausePopUp, 'PausePopUpStatic');

      // Score
      ThemeLoadBasic(Score, 'Score');

      ThemeLoadText(Score.TextArtist, 'ScoreTextArtist');
      ThemeLoadText(Score.TextTitle, 'ScoreTextTitle');
      ThemeLoadText(Score.TextArtistTitle, 'ScoreTextArtistTitle');

      for I := 1 to 6 do
      begin
        ThemeLoadStatics(Score.PlayerStatic[I],        'ScorePlayer' + IntToStr(I) + 'Static');
        ThemeLoadTexts(Score.PlayerTexts[I],           'ScorePlayer' + IntToStr(I) + 'Text');

        ThemeLoadText(Score.TextName[I],               'ScoreTextName'             + IntToStr(I));
        ThemeLoadText(Score.TextScore[I],              'ScoreTextScore'            + IntToStr(I));
        ThemeLoadText(Score.TextNotes[I],              'ScoreTextNotes'            + IntToStr(I));
        ThemeLoadText(Score.TextNotesScore[I],         'ScoreTextNotesScore'       + IntToStr(I));
        ThemeLoadText(Score.TextLineBonus[I],          'ScoreTextLineBonus'        + IntToStr(I));
        ThemeLoadText(Score.TextLineBonusScore[I],     'ScoreTextLineBonusScore'   + IntToStr(I));
        ThemeLoadText(Score.TextGoldenNotes[I],        'ScoreTextGoldenNotes'      + IntToStr(I));
        ThemeLoadText(Score.TextGoldenNotesScore[I],   'ScoreTextGoldenNotesScore' + IntToStr(I));
        ThemeLoadText(Score.TextTotal[I],              'ScoreTextTotal'            + IntToStr(I));
        ThemeLoadText(Score.TextTotalScore[I],         'ScoreTextTotalScore'       + IntToStr(I));

        ThemeLoadStatic(Score.StaticBoxLightest[I],    'ScoreStaticBoxLightest'    + IntToStr(I));
        ThemeLoadStatic(Score.StaticBoxLight[I],       'ScoreStaticBoxLight'       + IntToStr(I));
        ThemeLoadStatic(Score.StaticBoxDark[I],        'ScoreStaticBoxDark'        + IntToStr(I));

        ThemeLoadStatic(Score.StaticBackLevel[I],      'ScoreStaticBackLevel'      + IntToStr(I));
        ThemeLoadStatic(Score.StaticBackLevelRound[I], 'ScoreStaticBackLevelRound' + IntToStr(I));
        ThemeLoadStatic(Score.StaticLevel[I],          'ScoreStaticLevel'          + IntToStr(I));
        ThemeLoadStatic(Score.StaticLevelRound[I],     'ScoreStaticLevelRound'     + IntToStr(I));

        ThemeLoadStatic(Score.StaticRatings[I],        'ScoreStaticRatingPicture'  + IntToStr(I));
      end;

      // Top5
      ThemeLoadBasic(Top5, 'Top5');

      ThemeLoadText(Top5.TextLevel,       'Top5TextLevel');
      ThemeLoadText(Top5.TextArtistTitle, 'Top5TextArtistTitle');
      ThemeLoadStatics(Top5.StaticNumber, 'Top5StaticNumber');
      ThemeLoadTexts(Top5.TextNumber,     'Top5TextNumber');
      ThemeLoadTexts(Top5.TextName,       'Top5TextName');
      ThemeLoadTexts(Top5.TextScore,      'Top5TextScore');

      // Options
      ThemeLoadBasic(Options, 'Options');

      ThemeLoadButton(Options.ButtonGame,     'OptionsButtonGame');
      ThemeLoadButton(Options.ButtonGraphics, 'OptionsButtonGraphics');
      ThemeLoadButton(Options.ButtonSound,    'OptionsButtonSound');
      ThemeLoadButton(Options.ButtonLyrics,   'OptionsButtonLyrics');
      ThemeLoadButton(Options.ButtonThemes,   'OptionsButtonThemes');
      ThemeLoadButton(Options.ButtonRecord,   'OptionsButtonRecord');
      ThemeLoadButton(Options.ButtonAdvanced, 'OptionsButtonAdvanced');
      ThemeLoadButton(Options.ButtonExit,     'OptionsButtonExit');

      Options.Description[0] := Language.Translate('SING_OPTIONS_GAME_DESC');
      Options.Description[1] := Language.Translate('SING_OPTIONS_GRAPHICS_DESC');
      Options.Description[2] := Language.Translate('SING_OPTIONS_SOUND_DESC');
      Options.Description[3] := Language.Translate('SING_OPTIONS_LYRICS_DESC');
      Options.Description[4] := Language.Translate('SING_OPTIONS_THEMES_DESC');
      Options.Description[5] := Language.Translate('SING_OPTIONS_RECORD_DESC');
      Options.Description[6] := Language.Translate('SING_OPTIONS_ADVANCED_DESC');
      Options.Description[7] := Language.Translate('SING_OPTIONS_EXIT');

      ThemeLoadText(Options.TextDescription, 'OptionsTextDescription');
      Options.TextDescription.Text := Options.Description[0];

      // Options Game
      ThemeLoadBasic(OptionsGame, 'OptionsGame');

      ThemeLoadSelectSlide(OptionsGame.SelectPlayers,    'OptionsGameSelectPlayers');
      ThemeLoadSelectSlide(OptionsGame.SelectDifficulty, 'OptionsGameSelectDifficulty');
      ThemeLoadSelectSlide(OptionsGame.SelectLanguage,   'OptionsGameSelectSlideLanguage');
      ThemeLoadSelectSlide(OptionsGame.SelectTabs,       'OptionsGameSelectTabs');
      ThemeLoadSelectSlide(OptionsGame.SelectSorting,    'OptionsGameSelectSlideSorting');
      ThemeLoadSelectSlide(OptionsGame.SelectDebug,      'OptionsGameSelectDebug');
      ThemeLoadButton(OptionsGame.ButtonExit,            'OptionsGameButtonExit');

      // Options Graphics
      ThemeLoadBasic(OptionsGraphics, 'OptionsGraphics');

      ThemeLoadSelectSlide(OptionsGraphics.SelectFullscreen,   'OptionsGraphicsSelectFullscreen');
      ThemeLoadSelectSlide(OptionsGraphics.SelectResolution,   'OptionsGraphicsSelectSlideResolution');
      ThemeLoadSelectSlide(OptionsGraphics.SelectDepth,        'OptionsGraphicsSelectDepth');
      ThemeLoadSelectSlide(OptionsGraphics.SelectVisualizer,   'OptionsGraphicsSelectVisualizer');
      ThemeLoadSelectSlide(OptionsGraphics.SelectOscilloscope, 'OptionsGraphicsSelectOscilloscope');
      ThemeLoadSelectSlide(OptionsGraphics.SelectLineBonus,    'OptionsGraphicsSelectLineBonus');
      ThemeLoadSelectSlide(OptionsGraphics.SelectMovieSize,    'OptionsGraphicsSelectMovieSize');
      ThemeLoadButton(OptionsGraphics.ButtonExit,              'OptionsGraphicsButtonExit');

      // Options Sound
      ThemeLoadBasic(OptionsSound, 'OptionsSound');

      ThemeLoadSelectSlide(OptionsSound.SelectBackgroundMusic,       'OptionsSoundSelectBackgroundMusic');
      ThemeLoadSelectSlide(OptionsSound.SelectMicBoost,              'OptionsSoundSelectMicBoost');
      ThemeLoadSelectSlide(OptionsSound.SelectClickAssist,           'OptionsSoundSelectClickAssist');
      ThemeLoadSelectSlide(OptionsSound.SelectBeatClick,             'OptionsSoundSelectBeatClick');
      ThemeLoadSelectSlide(OptionsSound.SelectThreshold,             'OptionsSoundSelectThreshold');
      //Song Preview
      ThemeLoadSelectSlide(OptionsSound.SelectSlidePreviewVolume,    'OptionsSoundSelectSlidePreviewVolume');
      ThemeLoadSelectSlide(OptionsSound.SelectSlidePreviewFading,    'OptionsSoundSelectSlidePreviewFading');
      ThemeLoadSelectSlide(OptionsSound.SelectSlideVoicePassthrough, 'OptionsSoundSelectVoicePassthrough');

      ThemeLoadButton(OptionsSound.ButtonExit, 'OptionsSoundButtonExit');

      // Options Lyrics
      ThemeLoadBasic(OptionsLyrics, 'OptionsLyrics');

      ThemeLoadSelectSlide(OptionsLyrics.SelectLyricsFont,   'OptionsLyricsSelectLyricsFont');
      ThemeLoadSelectSlide(OptionsLyrics.SelectLyricsEffect, 'OptionsLyricsSelectLyricsEffect');
      //ThemeLoadSelectSlide(OptionsLyrics.SelectSolmization,     'OptionsLyricsSelectSolmization');
      ThemeLoadSelectSlide(OptionsLyrics.SelectNoteLines,    'OptionsLyricsSelectNoteLines');
      ThemeLoadButton(OptionsLyrics.ButtonExit,              'OptionsLyricsButtonExit');

      // Options Themes
      ThemeLoadBasic(OptionsThemes, 'OptionsThemes');

      ThemeLoadSelectSlide(OptionsThemes.SelectTheme, 'OptionsThemesSelectTheme');
      ThemeLoadSelectSlide(OptionsThemes.SelectSkin,  'OptionsThemesSelectSkin');
      ThemeLoadSelectSlide(OptionsThemes.SelectColor, 'OptionsThemesSelectColor');
      ThemeLoadButton(OptionsThemes.ButtonExit,       'OptionsThemesButtonExit');

      // Options Record
      ThemeLoadBasic(OptionsRecord, 'OptionsRecord');

      ThemeLoadSelectSlide(OptionsRecord.SelectSlideCard,     'OptionsRecordSelectSlideCard');
      ThemeLoadSelectSlide(OptionsRecord.SelectSlideInput,    'OptionsRecordSelectSlideInput');
      ThemeLoadSelectSlide(OptionsRecord.SelectSlideChannel,  'OptionsRecordSelectSlideChannel');
      ThemeLoadButton(OptionsRecord.ButtonExit,               'OptionsRecordButtonExit');

      //Options Advanced
      ThemeLoadBasic(OptionsAdvanced, 'OptionsAdvanced');

      ThemeLoadSelectSlide(OptionsAdvanced.SelectLoadAnimation, 'OptionsAdvancedSelectLoadAnimation');
      ThemeLoadSelectSlide(OptionsAdvanced.SelectScreenFade,    'OptionsAdvancedSelectScreenFade');
      ThemeLoadSelectSlide(OptionsAdvanced.SelectEffectSing,    'OptionsAdvancedSelectEffectSing');
      ThemeLoadSelectSlide(OptionsAdvanced.SelectLineBonus,     'OptionsAdvancedSelectLineBonus');
      ThemeLoadSelectSlide(OptionsAdvanced.SelectOnSongClick,   'OptionsAdvancedSelectSlideOnSongClick');
      ThemeLoadSelectSlide(OptionsAdvanced.SelectAskbeforeDel,  'OptionsAdvancedSelectAskbeforeDel');
      ThemeLoadSelectSlide(OptionsAdvanced.SelectPartyPopup,    'OptionsAdvancedSelectPartyPopup');
      ThemeLoadButton     (OptionsAdvanced.ButtonExit,          'OptionsAdvancedButtonExit');

      //Edit Menu
      ThemeLoadBasic (Edit,               'Edit');

      ThemeLoadButton(Edit.ButtonConvert, 'EditButtonConvert');
      ThemeLoadButton(Edit.ButtonExit,    'EditButtonExit');

      Edit.Description[0] := Language.Translate('SING_EDIT_BUTTON_DESCRIPTION_CONVERT');
      Edit.Description[1] := Language.Translate('SING_EDIT_BUTTON_DESCRIPTION_EXIT');

      ThemeLoadText(Edit.TextDescription, 'EditTextDescription');
      Edit.TextDescription.Text := Edit.Description[0];

      //error and check popup
      ThemeLoadBasic (ErrorPopup, 'ErrorPopup');
      ThemeLoadButton(ErrorPopup.Button1, 'ErrorPopupButton1');
      ThemeLoadText  (ErrorPopup.TextError,'ErrorPopupText');
      ThemeLoadBasic (CheckPopup, 'CheckPopup');
      ThemeLoadButton(CheckPopup.Button1, 'CheckPopupButton1');
      ThemeLoadButton(CheckPopup.Button2, 'CheckPopupButton2');
      ThemeLoadText(CheckPopup.TextCheck , 'CheckPopupText');

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
      //Translations
      SongJumpto.IType[0] := Language.Translate('SONG_JUMPTO_TYPE1');
      SongJumpto.IType[1] := Language.Translate('SONG_JUMPTO_TYPE2');
      SongJumpto.IType[2] := Language.Translate('SONG_JUMPTO_TYPE3');
      SongJumpto.SongsFound := Language.Translate('SONG_JUMPTO_SONGSFOUND');
      SongJumpto.NoSongsFound := Language.Translate('SONG_JUMPTO_NOSONGSFOUND');
      SongJumpto.CatText := Language.Translate('SONG_JUMPTO_CATTEXT');

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

      ThemeLoadText (PartyNewRound.TextTeam1Players, 'PartyNewRoundTextTeam1Players');
      ThemeLoadText (PartyNewRound.TextTeam2Players, 'PartyNewRoundTextTeam2Players');
      ThemeLoadText (PartyNewRound.TextTeam3Players, 'PartyNewRoundTextTeam3Players');

      ThemeLoadStatic (PartyNewRound.StaticTeam1, 'PartyNewRoundStaticTeam1');
      ThemeLoadStatic (PartyNewRound.StaticTeam2, 'PartyNewRoundStaticTeam2');
      ThemeLoadStatic (PartyNewRound.StaticTeam3, 'PartyNewRoundStaticTeam3');
      ThemeLoadStatic (PartyNewRound.StaticNextPlayer1, 'PartyNewRoundStaticNextPlayer1');
      ThemeLoadStatic (PartyNewRound.StaticNextPlayer2, 'PartyNewRoundStaticNextPlayer2');
      ThemeLoadStatic (PartyNewRound.StaticNextPlayer3, 'PartyNewRoundStaticNextPlayer3');

      //Party Score
      ThemeLoadBasic(PartyScore, 'PartyScore');

      ThemeLoadText (PartyScore.TextScoreTeam1, 'PartyScoreTextScoreTeam1');
      ThemeLoadText (PartyScore.TextScoreTeam2, 'PartyScoreTextScoreTeam2');
      ThemeLoadText (PartyScore.TextScoreTeam3, 'PartyScoreTextScoreTeam3');
      ThemeLoadText (PartyScore.TextNameTeam1, 'PartyScoreTextNameTeam1');
      ThemeLoadText (PartyScore.TextNameTeam2, 'PartyScoreTextNameTeam2');
      ThemeLoadText (PartyScore.TextNameTeam3, 'PartyScoreTextNameTeam3');

      ThemeLoadStatic (PartyScore.StaticTeam1, 'PartyScoreStaticTeam1');
      ThemeLoadStatic (PartyScore.StaticTeam1BG, 'PartyScoreStaticTeam1BG');
      ThemeLoadStatic (PartyScore.StaticTeam1Deco, 'PartyScoreStaticTeam1Deco');
      ThemeLoadStatic (PartyScore.StaticTeam2, 'PartyScoreStaticTeam2');
      ThemeLoadStatic (PartyScore.StaticTeam2BG, 'PartyScoreStaticTeam2BG');
      ThemeLoadStatic (PartyScore.StaticTeam2Deco, 'PartyScoreStaticTeam2Deco');
      ThemeLoadStatic (PartyScore.StaticTeam3, 'PartyScoreStaticTeam3');
      ThemeLoadStatic (PartyScore.StaticTeam3BG, 'PartyScoreStaticTeam3BG');
      ThemeLoadStatic (PartyScore.StaticTeam3Deco, 'PartyScoreStaticTeam3Deco');

      //Load Party Score DecoTextures Object
      PartyScore.DecoTextures.ChangeTextures := (ThemeIni.ReadInteger('PartyScoreDecoTextures', 'ChangeTextures', 0) = 1);
      PartyScore.DecoTextures.FirstTexture   :=  ThemeIni.ReadString('PartyScoreDecoTextures',  'FirstTexture', '');
      PartyScore.DecoTextures.FirstTyp       :=  ParseTextureType(ThemeIni.ReadString('PartyScoreDecoTextures', 'FirstTyp', ''), TEXTURE_TYPE_COLORIZED);
      PartyScore.DecoTextures.FirstColor     :=  ThemeIni.ReadString('PartyScoreDecoTextures',  'FirstColor', 'Black');

      PartyScore.DecoTextures.SecondTexture  :=  ThemeIni.ReadString('PartyScoreDecoTextures',  'SecondTexture', '');
      PartyScore.DecoTextures.SecondTyp      :=  ParseTextureType(ThemeIni.ReadString('PartyScoreDecoTextures', 'SecondTyp', ''), TEXTURE_TYPE_COLORIZED);
      PartyScore.DecoTextures.SecondColor    :=  ThemeIni.ReadString('PartyScoreDecoTextures',  'SecondColor', 'Black');

      PartyScore.DecoTextures.ThirdTexture   :=  ThemeIni.ReadString('PartyScoreDecoTextures',  'ThirdTexture', '');
      PartyScore.DecoTextures.ThirdTyp       :=  ParseTextureType(ThemeIni.ReadString('PartyScoreDecoTextures', 'ThirdTyp', ''), TEXTURE_TYPE_COLORIZED);
      PartyScore.DecoTextures.ThirdColor     :=  ThemeIni.ReadString('PartyScoreDecoTextures',  'ThirdColor', 'Black');

      ThemeLoadText (PartyScore.TextWinner, 'PartyScoreTextWinner');

      //Party Win
      ThemeLoadBasic(PartyWin, 'PartyWin');

      ThemeLoadText (PartyWin.TextScoreTeam1,    'PartyWinTextScoreTeam1');
      ThemeLoadText (PartyWin.TextScoreTeam2,    'PartyWinTextScoreTeam2');
      ThemeLoadText (PartyWin.TextScoreTeam3,    'PartyWinTextScoreTeam3');
      ThemeLoadText (PartyWin.TextNameTeam1,     'PartyWinTextNameTeam1');
      ThemeLoadText (PartyWin.TextNameTeam2,     'PartyWinTextNameTeam2');
      ThemeLoadText (PartyWin.TextNameTeam3,     'PartyWinTextNameTeam3');

      ThemeLoadStatic (PartyWin.StaticTeam1,     'PartyWinStaticTeam1');
      ThemeLoadStatic (PartyWin.StaticTeam1BG,   'PartyWinStaticTeam1BG');
      ThemeLoadStatic (PartyWin.StaticTeam1Deco, 'PartyWinStaticTeam1Deco');
      ThemeLoadStatic (PartyWin.StaticTeam2,     'PartyWinStaticTeam2');
      ThemeLoadStatic (PartyWin.StaticTeam2BG,   'PartyWinStaticTeam2BG');
      ThemeLoadStatic (PartyWin.StaticTeam2Deco, 'PartyWinStaticTeam2Deco');
      ThemeLoadStatic (PartyWin.StaticTeam3,     'PartyWinStaticTeam3');
      ThemeLoadStatic (PartyWin.StaticTeam3BG,   'PartyWinStaticTeam3BG');
      ThemeLoadStatic (PartyWin.StaticTeam3Deco, 'PartyWinStaticTeam3Deco');

      ThemeLoadText (PartyWin.TextWinner,        'PartyWinTextWinner');

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

      ThemeLoadBasic(StatMain, 'StatMain');

      ThemeLoadButton(StatMain.ButtonScores, 'StatMainButtonScores');
      ThemeLoadButton(StatMain.ButtonSingers, 'StatMainButtonSingers');
      ThemeLoadButton(StatMain.ButtonSongs, 'StatMainButtonSongs');
      ThemeLoadButton(StatMain.ButtonBands, 'StatMainButtonBands');
      ThemeLoadButton(StatMain.ButtonExit, 'StatMainButtonExit');

      ThemeLoadText (StatMain.TextOverview, 'StatMainTextOverview');


      ThemeLoadBasic(StatDetail, 'StatDetail');

      ThemeLoadButton(StatDetail.ButtonNext, 'StatDetailButtonNext');
      ThemeLoadButton(StatDetail.ButtonPrev, 'StatDetailButtonPrev');
      ThemeLoadButton(StatDetail.ButtonReverse, 'StatDetailButtonReverse');
      ThemeLoadButton(StatDetail.ButtonExit, 'StatDetailButtonExit');

      ThemeLoadText (StatDetail.TextDescription, 'StatDetailTextDescription');
      ThemeLoadText (StatDetail.TextPage, 'StatDetailTextPage');
      ThemeLoadTexts(StatDetail.TextList, 'StatDetailTextList');

      //Translate Texts
      StatDetail.Description[0] := Language.Translate('STAT_DESC_SCORES');
      StatDetail.Description[1] := Language.Translate('STAT_DESC_SINGERS');
      StatDetail.Description[2] := Language.Translate('STAT_DESC_SONGS');
      StatDetail.Description[3] := Language.Translate('STAT_DESC_BANDS');

      StatDetail.DescriptionR[0] := Language.Translate('STAT_DESC_SCORES_REVERSED');
      StatDetail.DescriptionR[1] := Language.Translate('STAT_DESC_SINGERS_REVERSED');
      StatDetail.DescriptionR[2] := Language.Translate('STAT_DESC_SONGS_REVERSED');
      StatDetail.DescriptionR[3] := Language.Translate('STAT_DESC_BANDS_REVERSED');

      StatDetail.FormatStr[0] := Language.Translate('STAT_FORMAT_SCORES');
      StatDetail.FormatStr[1] := Language.Translate('STAT_FORMAT_SINGERS');
      StatDetail.FormatStr[2] := Language.Translate('STAT_FORMAT_SONGS');
      StatDetail.FormatStr[3] := Language.Translate('STAT_FORMAT_BANDS');

      StatDetail.PageStr := Language.Translate('STAT_PAGE');

      //Playlist Translations
      Playlist.CatText := Language.Translate('PLAYLIST_CATTEXT');

      //Level Translations
      //Fill ILevel
      ILevel[0] := Language.Translate('SING_EASY');
      ILevel[1] := Language.Translate('SING_MEDIUM');
      ILevel[2] := Language.Translate('SING_HARD');
    end;

    ThemeIni.Free;
  end;
end;

procedure TTheme.ThemeLoadBasic(Theme: TThemeBasic; const Name: string);
begin
  ThemeLoadBackground(Theme.Background, Name);
  ThemeLoadTexts(Theme.Text, Name + 'Text');
  ThemeLoadStatics(Theme.Static, Name + 'Static');
  ThemeLoadButtonCollections(Theme.ButtonCollection, Name + 'ButtonCollection');

  LastThemeBasic := Theme;
end;

procedure TTheme.ThemeLoadBackground(var ThemeBackground: TThemeBackground; const Name: string);
var
  BGType: string;
  I: TBackgroundType;
begin
  BGType := LowerCase(ThemeIni.ReadString(Name + 'Background', 'Type', 'auto'));

  ThemeBackground.BGType := bgtAuto;
  for I := Low(BGT_Names) to High(BGT_Names) do
  begin
    if (BGT_Names[I] = BGType) then
    begin
      ThemeBackground.BGType := I;
      Break;
    end;
  end;

  ThemeBackground.Tex     := ThemeIni.ReadString(Name + 'Background', 'Tex', '');
  ThemeBackground.Color.R := ThemeIni.ReadFloat(Name + 'Background', 'ColR', 1);
  ThemeBackground.Color.G := ThemeIni.ReadFloat(Name + 'Background', 'ColG', 1);
  ThemeBackground.Color.B := ThemeIni.ReadFloat(Name + 'Background', 'ColB', 1);
  ThemeBackground.Alpha   := ThemeIni.ReadFloat(Name + 'Background', 'Alpha', 1);
end;

procedure TTheme.ThemeLoadText(var ThemeText: TThemeText; const Name: string);
var
  C: integer;
begin
  ThemeText.X     := ThemeIni.ReadInteger(Name, 'X', 0);
  ThemeText.Y     := ThemeIni.ReadInteger(Name, 'Y', 0);
  ThemeText.W     := ThemeIni.ReadInteger(Name, 'W', 0);

  ThemeText.Z     := ThemeIni.ReadFloat(Name, 'Z', 0);

  ThemeText.ColR  := ThemeIni.ReadFloat(Name, 'ColR', 0);
  ThemeText.ColG  := ThemeIni.ReadFloat(Name, 'ColG', 0);
  ThemeText.ColB  := ThemeIni.ReadFloat(Name, 'ColB', 0);

  ThemeText.Font  := ThemeIni.ReadInteger(Name, 'Font', 0);
  ThemeText.Size  := ThemeIni.ReadInteger(Name, 'Size', 0);
  ThemeText.Align := ThemeIni.ReadInteger(Name, 'Align', 0);

  ThemeText.Text  := Language.Translate(ThemeIni.ReadString(Name, 'Text', ''));
  ThemeText.Color := ThemeIni.ReadString(Name, 'Color', '');

  //Reflection
  ThemeText.Reflection         := (ThemeIni.ReadInteger(Name, 'Reflection', 0)) = 1;
  ThemeText.Reflectionspacing  := ThemeIni.ReadFloat(Name, 'ReflectionSpacing', 15);

  C := ColorExists(ThemeText.Color);
  if C >= 0 then
  begin
    ThemeText.ColR := Color[C].RGB.R;
    ThemeText.ColG := Color[C].RGB.G;
    ThemeText.ColB := Color[C].RGB.B;
  end;
end;

procedure TTheme.ThemeLoadTexts(var ThemeText: AThemeText; const Name: string);
var
  T: integer;
begin
  T := 1;
  while ThemeIni.SectionExists(Name + IntToStr(T)) do
  begin
    SetLength(ThemeText, T);
    ThemeLoadText(ThemeText[T-1], Name + IntToStr(T));
    Inc(T);
  end;
end;

procedure TTheme.ThemeLoadStatic(var ThemeStatic: TThemeStatic; const Name: string);
var
  C: integer;
begin
  ThemeStatic.Tex := ThemeIni.ReadString(Name, 'Tex', '');

  ThemeStatic.X := ThemeIni.ReadInteger(Name, 'X', 0);
  ThemeStatic.Y := ThemeIni.ReadInteger(Name, 'Y', 0);
  ThemeStatic.Z := ThemeIni.ReadFloat  (Name, 'Z', 0);
  ThemeStatic.W := ThemeIni.ReadInteger(Name, 'W', 0);
  ThemeStatic.H := ThemeIni.ReadInteger(Name, 'H', 0);

  ThemeStatic.Typ   := ParseTextureType(ThemeIni.ReadString(Name, 'Type', ''), TEXTURE_TYPE_PLAIN);
  ThemeStatic.Color := ThemeIni.ReadString(Name, 'Color', '');

  C := ColorExists(ThemeStatic.Color);
  if C >= 0 then
  begin
    ThemeStatic.ColR := Color[C].RGB.R;
    ThemeStatic.ColG := Color[C].RGB.G;
    ThemeStatic.ColB := Color[C].RGB.B;
  end;

  ThemeStatic.TexX1 := ThemeIni.ReadFloat(Name, 'TexX1', 0);
  ThemeStatic.TexY1 := ThemeIni.ReadFloat(Name, 'TexY1', 0);
  ThemeStatic.TexX2 := ThemeIni.ReadFloat(Name, 'TexX2', 1);
  ThemeStatic.TexY2 := ThemeIni.ReadFloat(Name, 'TexY2', 1);

  //Reflection Mod
  ThemeStatic.Reflection        := (ThemeIni.ReadInteger(Name, 'Reflection', 0) = 1);
  ThemeStatic.ReflectionSpacing := ThemeIni.ReadFloat(Name, 'ReflectionSpacing', 15);
end;

procedure TTheme.ThemeLoadStatics(var ThemeStatic: AThemeStatic; const Name: string);
var
  S: integer;
begin
  S := 1;
  while ThemeIni.SectionExists(Name + IntToStr(S)) do
  begin
    SetLength(ThemeStatic, S);
    ThemeLoadStatic(ThemeStatic[S-1], Name + IntToStr(S));
    Inc(S);
  end;
end;

//Button Collection Mod
procedure TTheme.ThemeLoadButtonCollection(var Collection: TThemeButtonCollection; const Name: string);
var T: integer;
begin
  //Load Collection Style
  ThemeLoadButton(Collection.Style, Name);

  //Load Other Attributes
  T := ThemeIni.ReadInteger (Name, 'FirstChild', 0);
  if (T > 0) And (T < 256) then
    Collection.FirstChild := T
  else
    Collection.FirstChild := 0;
end;

procedure TTheme.ThemeLoadButtonCollections(var Collections: AThemeButtonCollection; const Name: string);
var
  I: integer;
begin
  I := 1;
  while ThemeIni.SectionExists(Name + IntToStr(I)) do
  begin
    SetLength(Collections, I);
    ThemeLoadButtonCollection(Collections[I-1], Name + IntToStr(I));
    Inc(I);
  end;
end;
//End Button Collection Mod

procedure TTheme.ThemeLoadButton(var ThemeButton: TThemeButton; const Name: string; Collections: PAThemeButtonCollection);
var
  C:    integer;
  TLen: integer;
  T:    integer;
  Collections2: PAThemeButtonCollection;
begin
  if not ThemeIni.SectionExists(Name) then
  begin
    ThemeButton.Visible := False;
    exit;
  end;
  ThemeButton.Tex := ThemeIni.ReadString(Name, 'Tex', '');
  ThemeButton.X := ThemeIni.ReadInteger (Name, 'X', 0);
  ThemeButton.Y := ThemeIni.ReadInteger (Name, 'Y', 0);
  ThemeButton.Z := ThemeIni.ReadFloat   (Name, 'Z', 0);
  ThemeButton.W := ThemeIni.ReadInteger (Name, 'W', 0);
  ThemeButton.H := ThemeIni.ReadInteger (Name, 'H', 0);
  ThemeButton.Typ := ParseTextureType(ThemeIni.ReadString(Name, 'Type', ''), TEXTURE_TYPE_PLAIN);

  //Reflection Mod
  ThemeButton.Reflection := (ThemeIni.ReadInteger(Name, 'Reflection', 0) = 1);
  ThemeButton.ReflectionSpacing := ThemeIni.ReadFloat(Name, 'ReflectionSpacing', 15);

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
  if C >= 0 then
  begin
    ThemeButton.ColR := Color[C].RGB.R;
    ThemeButton.ColG := Color[C].RGB.G;
    ThemeButton.ColB := Color[C].RGB.B;
  end;

  ThemeButton.DColor := ThemeIni.ReadString(Name, 'DColor', '');
  C := ColorExists(ThemeButton.DColor);
  if C >= 0 then
  begin
    ThemeButton.DColR := Color[C].RGB.R;
    ThemeButton.DColG := Color[C].RGB.G;
    ThemeButton.DColB := Color[C].RGB.B;
  end;

  ThemeButton.Visible := (ThemeIni.ReadInteger(Name, 'Visible', 1) = 1);

  //Fade Mod
  ThemeButton.SelectH := ThemeIni.ReadInteger (Name, 'SelectH', ThemeButton.H);
  ThemeButton.SelectW := ThemeIni.ReadInteger (Name, 'SelectW', ThemeButton.W);

  ThemeButton.DeSelectReflectionspacing := ThemeIni.ReadFloat(Name, 'DeSelectReflectionSpacing', ThemeButton.Reflectionspacing);

  ThemeButton.Fade := (ThemeIni.ReadInteger(Name, 'Fade', 0) = 1);
  ThemeButton.FadeText := (ThemeIni.ReadInteger(Name, 'FadeText', 0) = 1);


  ThemeButton.FadeTex := ThemeIni.ReadString(Name, 'FadeTex', '');
  ThemeButton.FadeTexPos:= ThemeIni.ReadInteger(Name, 'FadeTexPos', 0);
  if (ThemeButton.FadeTexPos > 4) Or (ThemeButton.FadeTexPos < 0) then
    ThemeButton.FadeTexPos := 0;

  //Button Collection Mod
  T := ThemeIni.ReadInteger(Name, 'Parent', 0);

  //Set Collections to Last Basic Collections if no valid Value
  if (Collections = nil) then
    Collections2 := @LastThemeBasic.ButtonCollection
  else
    Collections2 := Collections;
  //Test for valid Value
  if (Collections2 <> nil) AND (T > 0) AND (T <= Length(Collections2^)) then
  begin
    Inc(Collections2^[T-1].ChildCount);
    ThemeButton.Parent := T;
  end
  else
    ThemeButton.Parent := 0;

  //Read ButtonTexts
  TLen := ThemeIni.ReadInteger(Name, 'Texts', 0);
  SetLength(ThemeButton.Text, TLen);
  for T := 1 to TLen do
    ThemeLoadText(ThemeButton.Text[T-1], Name + 'Text' + IntToStr(T));
end;

procedure TTheme.ThemeLoadSelectSlide(var ThemeSelectS: TThemeSelectSlide; const Name: string);
begin
  ThemeSelectS.Text := Language.Translate(ThemeIni.ReadString(Name, 'Text', ''));

  ThemeSelectS.Tex := {Skin.SkinPath + }ThemeIni.ReadString(Name, 'Tex', '');
  ThemeSelectS.TexSBG := {Skin.SkinPath + }ThemeIni.ReadString(Name, 'TexSBG', '');

  ThemeSelectS.X := ThemeIni.ReadInteger(Name, 'X', 0);
  ThemeSelectS.Y := ThemeIni.ReadInteger(Name, 'Y', 0);
  ThemeSelectS.W := ThemeIni.ReadInteger(Name, 'W', 0);
  ThemeSelectS.H := ThemeIni.ReadInteger(Name, 'H', 0);

  ThemeSelectS.Z := ThemeIni.ReadFloat(Name, 'Z', 0);

  ThemeSelectS.TextSize := ThemeIni.ReadInteger(Name, 'TextSize', 30);

  ThemeSelectS.SkipX := ThemeIni.ReadInteger(Name, 'SkipX', 0);

  ThemeSelectS.SBGW := ThemeIni.ReadInteger(Name, 'SBGW', 400);

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
end;

procedure TTheme.ThemeLoadEqualizer(var ThemeEqualizer: TThemeEqualizer; const Name: string);
var I: integer;
begin
  ThemeEqualizer.Visible := (ThemeIni.ReadInteger(Name, 'Visible', 0) = 1);
  ThemeEqualizer.Direction := (ThemeIni.ReadInteger(Name, 'Direction', 0) = 1);
  ThemeEqualizer.Alpha := ThemeIni.ReadInteger(Name, 'Alpha', 1);
  ThemeEqualizer.Space := ThemeIni.ReadInteger(Name, 'Space', 1);
  ThemeEqualizer.X := ThemeIni.ReadInteger(Name, 'X', 0);
  ThemeEqualizer.Y := ThemeIni.ReadInteger(Name, 'Y', 0);
  ThemeEqualizer.Z := ThemeIni.ReadInteger(Name, 'Z', 1);
  ThemeEqualizer.W := ThemeIni.ReadInteger(Name, 'PieceW', 8);
  ThemeEqualizer.H := ThemeIni.ReadInteger(Name, 'PieceH', 8);
  ThemeEqualizer.Bands := ThemeIni.ReadInteger(Name, 'Bands', 5);
  ThemeEqualizer.Length := ThemeIni.ReadInteger(Name, 'Length', 12);
  ThemeEqualizer.Reflection := (ThemeIni.ReadInteger(Name, 'Reflection', 0) = 1);
  ThemeEqualizer.ReflectionSpacing := ThemeIni.ReadFloat(Name, 'ReflectionSpacing', 15);

  //Color
  I := ColorExists(ThemeIni.ReadString(Name, 'Color', 'Black'));
  if I >= 0 then
  begin
    ThemeEqualizer.ColR := Color[I].RGB.R;
    ThemeEqualizer.ColG := Color[I].RGB.G;
    ThemeEqualizer.ColB := Color[I].RGB.B;
  end
  else
  begin
    ThemeEqualizer.ColR := 0;
    ThemeEqualizer.ColG := 0;
    ThemeEqualizer.ColB := 0;
  end;
end;

procedure TTheme.LoadColors;
var
  SL:     TStringList;
  C:      integer;
  S:      string;
begin
  SL := TStringList.Create;
  ThemeIni.ReadSection('Colors', SL);

  // normal colors
  SetLength(Color, SL.Count);
  for C := 0 to SL.Count-1 do
  begin
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
  C: integer;
begin
  Result := -1;
  for C := 0 to High(Color) do
    if Color[C].Name = Name then
      Result := C;
end;

procedure LoadColor(var R, G, B: real; ColorName: string);
var
  C: integer;
begin
  C := ColorExists(ColorName);
  if C >= 0 then
  begin
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
end;

function ColorSqrt(RGB: TRGB): TRGB;
begin
  Result.R := sqrt(RGB.R);
  Result.G := sqrt(RGB.G);
  Result.B := sqrt(RGB.B);
end;

procedure TTheme.ThemeSave(const FileName: string);
var
  I: integer;
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

  //TimeBar mod
  ThemeSaveStatic(Sing.StaticTimeProgress, 'SingTimeProgress');
  ThemeSaveText(Sing.TextTimeText, 'SingTimeText');
  //eoa TimeBar mod

  ThemeSaveStatic(Sing.StaticP1, 'SingP1Static');
  ThemeSaveText(Sing.TextP1, 'SingP1Text');
  ThemeSaveStatic(Sing.StaticP1ScoreBG, 'SingP1Static2');
  ThemeSaveText(Sing.TextP1Score, 'SingP1TextScore');

  //moveable singbar mod
  ThemeSaveStatic(Sing.StaticP1SingBar, 'SingP1SingBar');
  ThemeSaveStatic(Sing.StaticP1TwoPSingBar, 'SingP1TwoPSingBar');
  ThemeSaveStatic(Sing.StaticP1ThreePSingBar, 'SingP1ThreePSingBar');
  ThemeSaveStatic(Sing.StaticP2RSingBar, 'SingP2RSingBar');
  ThemeSaveStatic(Sing.StaticP2MSingBar, 'SingP2MSingBar');
  ThemeSaveStatic(Sing.StaticP3SingBar, 'SingP3SingBar');
  //eoa moveable singbar

  //Added for ps3 skin
  //This one is shown in 2/4P mode
  ThemeSaveStatic(Sing.StaticP1TwoP, 'SingP1TwoPStatic');
  ThemeSaveText(Sing.TextP1TwoP, 'SingP1TwoPText');
  ThemeSaveStatic(Sing.StaticP1TwoPScoreBG, 'SingP1TwoPStatic2');
  ThemeSaveText(Sing.TextP1TwoPScore, 'SingP1TwoPTextScore');

  //This one is shown in 3/6P mode
  ThemeSaveStatic(Sing.StaticP1ThreeP, 'SingP1ThreePStatic');
  ThemeSaveText(Sing.TextP1ThreeP, 'SingP1ThreePText');
  ThemeSaveStatic(Sing.StaticP1ThreePScoreBG, 'SingP1ThreePStatic2');
  ThemeSaveText(Sing.TextP1ThreePScore, 'SingP1ThreePTextScore');
  //eoa

  ThemeSaveStatic(Sing.StaticP2R, 'SingP2RStatic');
  ThemeSaveText(Sing.TextP2R, 'SingP2RText');
  ThemeSaveStatic(Sing.StaticP2RScoreBG, 'SingP2RStatic2');
  ThemeSaveText(Sing.TextP2RScore, 'SingP2RTextScore');

  ThemeSaveStatic(Sing.StaticP2M, 'SingP2MStatic');
  ThemeSaveText(Sing.TextP2M, 'SingP2MText');
  ThemeSaveStatic(Sing.StaticP2MScoreBG, 'SingP2MStatic2');
  ThemeSaveText(Sing.TextP2MScore, 'SingP2MTextScore');

  ThemeSaveStatic(Sing.StaticP3R, 'SingP3RStatic');
  ThemeSaveText(Sing.TextP3R, 'SingP3RText');
  ThemeSaveStatic(Sing.StaticP3RScoreBG, 'SingP3RStatic2');
  ThemeSaveText(Sing.TextP3RScore, 'SingP3RTextScore');

  ThemeSaveBasic(Score, 'Score');
  ThemeSaveText(Score.TextArtist, 'ScoreTextArtist');
  ThemeSaveText(Score.TextTitle, 'ScoreTextTitle');

  for I := 1 to 6 do
  begin
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

procedure TTheme.ThemeSaveBasic(Theme: TThemeBasic; const Name: string);
begin
  ThemeIni.WriteInteger(Name, 'Texts', Length(Theme.Text));

  ThemeSaveBackground(Theme.Background, Name + 'Background');
  ThemeSaveStatics(Theme.Static, Name + 'Static');
  ThemeSaveTexts(Theme.Text, Name + 'Text');
end;

procedure TTheme.ThemeSaveBackground(ThemeBackground: TThemeBackground; const Name: string);
begin
  if ThemeBackground.Tex <> '' then
    ThemeIni.WriteString(Name, 'Tex', ThemeBackground.Tex)
  else
  begin
    ThemeIni.EraseSection(Name);
  end;
end;

procedure TTheme.ThemeSaveStatic(ThemeStatic: TThemeStatic; const Name: string);
begin
  ThemeIni.WriteInteger(Name, 'X', ThemeStatic.X);
  ThemeIni.WriteInteger(Name, 'Y', ThemeStatic.Y);
  ThemeIni.WriteInteger(Name, 'W', ThemeStatic.W);
  ThemeIni.WriteInteger(Name, 'H', ThemeStatic.H);

  ThemeIni.WriteString(Name, 'Tex', ThemeStatic.Tex);
  ThemeIni.WriteString(Name, 'Type', TextureTypeToStr(ThemeStatic.Typ));
  ThemeIni.WriteString(Name, 'Color', ThemeStatic.Color);

  ThemeIni.WriteFloat(Name, 'TexX1', ThemeStatic.TexX1);
  ThemeIni.WriteFloat(Name, 'TexY1', ThemeStatic.TexY1);
  ThemeIni.WriteFloat(Name, 'TexX2', ThemeStatic.TexX2);
  ThemeIni.WriteFloat(Name, 'TexY2', ThemeStatic.TexY2);
end;

procedure TTheme.ThemeSaveStatics(ThemeStatic: AThemeStatic; const Name: string);
var
  S: integer;
begin
  for S := 0 to Length(ThemeStatic)-1 do
    ThemeSaveStatic(ThemeStatic[S], Name + {'Static' +} IntToStr(S+1));

  ThemeIni.EraseSection(Name + {'Static' + }IntToStr(S+1));
end;

procedure TTheme.ThemeSaveText(ThemeText: TThemeText; const Name: string);
begin
  ThemeIni.WriteInteger(Name, 'X', ThemeText.X);
  ThemeIni.WriteInteger(Name, 'Y', ThemeText.Y);

  ThemeIni.WriteInteger(Name, 'Font', ThemeText.Font);
  ThemeIni.WriteInteger(Name, 'Size', ThemeText.Size);
  ThemeIni.WriteInteger(Name, 'Align', ThemeText.Align);

  ThemeIni.WriteString(Name, 'Text', ThemeText.Text);
  ThemeIni.WriteString(Name, 'Color', ThemeText.Color);

  ThemeIni.WriteBool(Name, 'Reflection', ThemeText.Reflection);
  ThemeIni.WriteFloat(Name, 'ReflectionSpacing', ThemeText.ReflectionSpacing);
end;

procedure TTheme.ThemeSaveTexts(ThemeText: AThemeText; const Name: string);
var
  T: integer;
begin
  for T := 0 to Length(ThemeText)-1 do
    ThemeSaveText(ThemeText[T], Name + {'Text' + }IntToStr(T+1));

  ThemeIni.EraseSection(Name + {'Text' + }IntToStr(T+1));
end;

procedure TTheme.ThemeSaveButton(ThemeButton: TThemeButton; const Name: string);
var
  T: integer;
begin
  ThemeIni.WriteString(Name, 'Tex', ThemeButton.Tex);
  ThemeIni.WriteInteger(Name, 'X', ThemeButton.X);
  ThemeIni.WriteInteger(Name, 'Y', ThemeButton.Y);
  ThemeIni.WriteInteger(Name, 'W', ThemeButton.W);
  ThemeIni.WriteInteger(Name, 'H', ThemeButton.H);
  ThemeIni.WriteString(Name, 'Type', TextureTypeToStr(ThemeButton.Typ));
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
  if C >= 0 then
  begin
    ThemeButton.ColR := Color[C].RGB.R;
    ThemeButton.ColG := Color[C].RGB.G;
    ThemeButton.ColB := Color[C].RGB.B;
  end;

  C := ColorExists(ThemeIni.ReadString(Name, 'DColor', ''));
  if C >= 0 then
  begin
    ThemeButton.DColR := Color[C].RGB.R;
    ThemeButton.DColG := Color[C].RGB.G;
    ThemeButton.DColB := Color[C].RGB.B;
  end;}

  for T := 0 to High(ThemeButton.Text) do
    ThemeSaveText(ThemeButton.Text[T], Name + 'Text' + IntToStr(T+1));
end;

procedure TTheme.CreateThemeObjects();
begin
  freeandnil(Loading);
  Loading := TThemeLoading.Create;

  freeandnil(Main);
  Main := TThemeMain.Create;

  freeandnil(Name);
  Name := TThemeName.Create;

  freeandnil(Level);
  Level := TThemeLevel.Create;

  freeandnil(Song);
  Song := TThemeSong.Create;

  freeandnil(Sing);
  Sing := TThemeSing.Create;

  freeandnil(Score);
  Score := TThemeScore.Create;

  freeandnil(Top5);
  Top5 := TThemeTop5.Create;

  freeandnil(Options);
  Options := TThemeOptions.Create;

  freeandnil(OptionsGame);
  OptionsGame := TThemeOptionsGame.Create;

  freeandnil(OptionsGraphics);
  OptionsGraphics := TThemeOptionsGraphics.Create;

  freeandnil(OptionsSound);
  OptionsSound := TThemeOptionsSound.Create;

  freeandnil(OptionsLyrics);
  OptionsLyrics := TThemeOptionsLyrics.Create;

  freeandnil(OptionsThemes);
  OptionsThemes := TThemeOptionsThemes.Create;

  freeandnil(OptionsRecord);
  OptionsRecord := TThemeOptionsRecord.Create;

  freeandnil(OptionsAdvanced);
  OptionsAdvanced := TThemeOptionsAdvanced.Create;

  freeandnil(Edit);
  Edit := TThemeEdit.Create;

  freeandnil(ErrorPopup);
  ErrorPopup := TThemeError.Create;

  freeandnil(CheckPopup);
  CheckPopup := TThemeCheck.Create;

  freeandnil(SongMenu);
  SongMenu := TThemeSongMenu.Create;

  freeandnil(SongJumpto);
  SongJumpto := TThemeSongJumpto.Create;

  //Party Screens
  freeandnil(PartyNewRound);
  PartyNewRound := TThemePartyNewRound.Create;

  freeandnil(PartyWin);
  PartyWin := TThemePartyWin.Create;

  freeandnil(PartyScore);
  PartyScore := TThemePartyScore.Create;

  freeandnil(PartyOptions);
  PartyOptions := TThemePartyOptions.Create;

  freeandnil(PartyPlayer);
  PartyPlayer := TThemePartyPlayer.Create;

  //Stats Screens:
  freeandnil(StatMain);
  StatMain := TThemeStatMain.Create;

  freeandnil(StatDetail);
  StatDetail := TThemeStatDetail.Create;

 end;

end.
