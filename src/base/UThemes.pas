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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/base/UThemes.pas $
 * $Id: UThemes.pas 3131 2015-09-07 00:11:32Z basisbit $
 *}

unit UThemes;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  IniFiles,
  SysUtils,
  Classes,
  UCommon,
  ULog,
  UTexture,
  UPath;
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
  TThemePosition = record
    X: integer;
    Y: integer;
    H: integer;
    W: integer;
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
    Typ:    TTextureType;
    TexX1:  real;
    TexY1:  real;
    TexX2:  real;
    TexY2:  real;
    Alpha:  real;
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
    Color:   string;
    DColor:  string;
    ColR:   real;
    ColG:   real;
    ColB:   real;
    DColR:   real;
    DColG:   real;
    DColB:   real;
    Font:   integer;
    Size:   integer;
    Align:  integer;
    Text:   UTF8String;
    Writable: boolean; // true -> add a blink char (|) at the end
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
    Typ:    TTextureType;
    TexSBG: string;
    TypSBG: TTextureType;
    X:      integer;
    Y:      integer;
    W:      integer;
    H:      integer;
    Z:      real;
    SBGW:   integer;

    TextSize: integer;

    showArrows:boolean;
    oneItemOnly:boolean;

    Text:   UTF8String;
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
    Statics:           AThemeStatic;

    //Button Collection Mod
    ButtonCollection: AThemeButtonCollection;
  end;

  TThemeBox = record
    X: integer;
    Y: integer;
    W: integer;
    H: integer;
  end;

  TThemeLoading = class(TThemeBasic)
    StaticAnimation:  TThemeStatic;
    TextLoading:      TThemeText;
  end;

  TThemeMain = class(TThemeBasic)
    ButtonSolo:       TThemeButton;
    ButtonMulti:      TThemeButton;
    ButtonJukebox:    TThemeButton;
    ButtonStat:       TThemeButton;
    ButtonEditor:     TThemeButton;
    ButtonOptions:    TThemeButton;
    ButtonAbout:      TThemeButton;
    ButtonExit:       TThemeButton;

    TextDescription:      TThemeText;
    TextDescriptionLong:  TThemeText;
    Description:          array[0..7] of UTF8String;
    DescriptionLong:      array[0..7] of UTF8String;
  end;

  TThemeName = class(TThemeBasic)
    PlayerButtonName:          TThemeButton;
    PlayerButtonAvatar:        TThemeButton;

    PlayerScrollAvatar: record
      NumAvatars: integer;
      DistanceAvatars: integer;
    end;

    PlayerAvatar:        TThemeButton;

    PlayerSelect:        array [0..5] of TThemeStatic;
    PlayerSelectText:    array [0..5] of TThemeText;
    PlayerSelectAvatar:  array [0..5] of TThemeStatic;
    PlayerSelectCurrent: TThemeButton;
    
    SelectPlayersCount:  TThemeSelectSlide;
    SelectPlayerColor:   TThemeSelectSlide;
    SelectPlayerLevel:   TThemeSelectSlide;
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
    TextYear:         TThemeText;

    TextMedleyMax:    integer;

    TextArtistMedley: array of TThemeText;
    TextTitleMedley:  array of TThemeText;
    StaticMedley:     array of TThemeStatic;
    TextNumberMedley: array of TThemeText;

    //Video Icon Mod
    VideoIcon:        TThemeStatic;

    //Medley Icons
    MedleyIcon:             TThemeStatic;
    CalculatedMedleyIcon:   TThemeStatic;

    //Duet Icon
    DuetIcon:         TThemeStatic;

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
      Rows: integer;
      Cols: integer;
      Padding: integer;
      SelectX: integer;
      SelectY: integer;
      SelectW: integer;
      SelectH: integer;
      SelectReflection: boolean;
      SelectReflectionSpacing: integer;
      ZoomThumbW: integer;
      ZoomThumbH: integer;
      ZoomThumbStyle: integer;
      Tex: string;
    end;

    //Equalizer Mod
    Equalizer: TThemeEqualizer;

    //List Song Mod
    ListCover: record
      X: integer;
      Y: integer;
      Z: integer;
      W: integer;
      H: integer;
      Rows: integer;
      Padding: integer;
      Reflection: boolean;
      ReflectionSpacing: integer;
      Typ:    TTextureType;
      Tex: string;
      DTex: string;
      Color:  string;
      DColor:  string;
      ColR, ColG, ColB: real;
      DColR, DColG, DColB: real;
    end;


    //Party and Non Party specific Statics and Texts
    StaticParty:    AThemeStatic;
    TextParty:      AThemeText;

    StaticNonParty: AThemeStatic;
    TextNonParty:   AThemeText;

    InfoMessageText: TThemeText;
    InfoMessageBG:   TThemeStatic;

    //Screen Song Scores
    TextScore:       TThemeText;
    TextMaxScore:    TThemeText;
    TextMediaScore:  TThemeText;
    TextMaxScore2:   TThemeText;
    TextMediaScore2: TThemeText;
    TextScoreUser:   TThemeText;
    TextMaxScoreLocal:   TThemeText;
    TextMediaScoreLocal: TThemeText;
    TextScoreUserLocal:  TThemeText;

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

    TextPartyTime    : TThemeText;

    Static2PlayersDuetSingerP1: TThemeStatic;
    Static2PlayersDuetSingerP2: TThemeStatic;

    Static3PlayersDuetSingerP1: TThemeStatic;
    Static3PlayersDuetSingerP2: TThemeStatic;
    Static3PlayersDuetSingerP3: TThemeStatic;

    Static4PlayersDuetSingerP3: TThemeStatic;
    Static4PlayersDuetSingerP4: TThemeStatic;

    Static6PlayersDuetSingerP4: TThemeStatic;
    Static6PlayersDuetSingerP5: TThemeStatic;
    Static6PlayersDuetSingerP6: TThemeStatic;

    Text2PlayersDuetSingerP1:   TThemeText;
    Text2PlayersDuetSingerP2:   TThemeText;

    Text3PlayersDuetSingerP1:   TThemeText;
    Text3PlayersDuetSingerP2:   TThemeText;
    Text3PlayersDuetSingerP3:   TThemeText;
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
    StaticP1Avatar:   TThemeStatic;

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
    StaticP1TwoPAvatar:   TThemeStatic;
    StaticP1TwoPScoreBG:  TThemeStatic; //Static for ScoreBG
    TextP1TwoP:           TThemeText;
    TextP1TwoPScore:      TThemeText;
    //game in 3/6 player modi
    StaticP1ThreeP:         TThemeStatic;
    StaticP1ThreePAvatar:   TThemeStatic;
    StaticP1ThreePScoreBG:  TThemeStatic; //Static for ScoreBG
    TextP1ThreeP:           TThemeText;
    TextP1ThreePScore:      TThemeText;
    //eoa

    StaticP2R:        TThemeStatic;
    StaticP2RAvatar:  TThemeStatic;
    StaticP2RScoreBG: TThemeStatic; //Static for ScoreBG
    TextP2R:          TThemeText;
    TextP2RScore:     TThemeText;

    StaticP2M:        TThemeStatic;
    StaticP2MAvatar:  TThemeStatic;
    StaticP2MScoreBG: TThemeStatic; //Static for ScoreBG
    TextP2M:          TThemeText;
    TextP2MScore:     TThemeText;

    StaticP3R:        TThemeStatic;
    StaticP3RAvatar:  TThemeStatic;
    StaticP3RScoreBG: TThemeStatic; //Static for ScoreBG
    TextP3R:          TThemeText;
    TextP3RScore:     TThemeText;

    StaticDuetP1ThreeP:        TThemeStatic;
    StaticDuetP1ThreePAvatar:  TThemeStatic;
    TextDuetP1ThreeP:          TThemeText;
    StaticDuetP1ThreePScoreBG: TThemeStatic;
    TextDuetP1ThreePScore:     TThemeText;

    StaticDuetP2M:        TThemeStatic;
    StaticDuetP2MAvatar:  TThemeStatic;
    TextDuetP2M:          TThemeText;
    StaticDuetP2MScoreBG: TThemeStatic;
    TextDuetP2MScore:     TThemeText;

    StaticDuetP3R:        TThemeStatic;
    StaticDuetP3RAvatar:  TThemeStatic;
    TextDuetP3R:          TThemeText;
    StaticDuetP3RScoreBG: TThemeStatic;
    TextDuetP3RScore:     TThemeText;

    StaticDuetP1ThreePSingBar: TThemeStatic;
    StaticDuetP2MSingBar:      TThemeStatic;
    StaticDuetP3RSingBar:       TThemeStatic;

    //game in 4/6 player modi in 1 Screen
    StaticP1FourPSingBar: TThemeStatic;
    StaticP1FourP:        TThemeStatic;
    StaticP1FourPAvatar:  TThemeStatic;
    StaticP1FourPScoreBG: TThemeStatic; //Static for ScoreBG
    TextP1FourP:          TThemeText;
    TextP1FourPScore:     TThemeText;

    StaticP2FourPSingBar: TThemeStatic;
    StaticP2FourP:        TThemeStatic;
    StaticP2FourPAvatar:  TThemeStatic;
    StaticP2FourPScoreBG: TThemeStatic; //Static for ScoreBG
    TextP2FourP:          TThemeText;
    TextP2FourPScore:     TThemeText;

    StaticP3FourPSingBar: TThemeStatic;
    StaticP3FourP:        TThemeStatic;
    StaticP3FourPAvatar:  TThemeStatic;
    StaticP3FourPScoreBG: TThemeStatic; //Static for ScoreBG
    TextP3FourP:          TThemeText;
    TextP3FourPScore:     TThemeText;

    StaticP4FourPSingBar: TThemeStatic;
    StaticP4FourP:        TThemeStatic;
    StaticP4FourPAvatar:  TThemeStatic;
    StaticP4FourPScoreBG: TThemeStatic; //Static for ScoreBG
    TextP4FourP:          TThemeText;
    TextP4FourPScore:     TThemeText;

    StaticP1SixPSingBar: TThemeStatic;
    StaticP1SixP:        TThemeStatic;
    StaticP1SixPAvatar:  TThemeStatic;
    StaticP1SixPScoreBG: TThemeStatic; //Static for ScoreBG
    TextP1SixP:          TThemeText;
    TextP1SixPScore:     TThemeText;

    StaticP2SixPSingBar: TThemeStatic;
    StaticP2SixP:        TThemeStatic;
    StaticP2SixPAvatar:  TThemeStatic;
    StaticP2SixPScoreBG: TThemeStatic; //Static for ScoreBG
    TextP2SixP:          TThemeText;
    TextP2SixPScore:     TThemeText;

    StaticP3SixPSingBar: TThemeStatic;
    StaticP3SixP:        TThemeStatic;
    StaticP3SixPAvatar:  TThemeStatic;
    StaticP3SixPScoreBG: TThemeStatic; //Static for ScoreBG
    TextP3SixP:          TThemeText;
    TextP3SixPScore:     TThemeText;

    StaticP4SixPSingBar: TThemeStatic;
    StaticP4SixP:        TThemeStatic;
    StaticP4SixPAvatar:  TThemeStatic;
    StaticP4SixPScoreBG: TThemeStatic; //Static for ScoreBG
    TextP4SixP:          TThemeText;
    TextP4SixPScore:     TThemeText;

    StaticP5SixPSingBar: TThemeStatic;
    StaticP5SixP:        TThemeStatic;
    StaticP5SixPAvatar:  TThemeStatic;
    StaticP5SixPScoreBG: TThemeStatic; //Static for ScoreBG
    TextP5SixP:          TThemeText;
    TextP5SixPScore:     TThemeText;

    StaticP6SixPSingBar: TThemeStatic;
    StaticP6SixP:        TThemeStatic;
    StaticP6SixPAvatar:  TThemeStatic;
    StaticP6SixPScoreBG: TThemeStatic; //Static for ScoreBG
    TextP6SixP:          TThemeText;
    TextP6SixPScore:     TThemeText;

    // duet 4/6 players in one screen
    StaticP1DuetFourPSingBar: TThemeStatic;
    StaticP1DuetFourP:        TThemeStatic;
    StaticP1DuetFourPAvatar:  TThemeStatic;
    StaticP1DuetFourPScoreBG: TThemeStatic; //Static for ScoreBG
    TextP1DuetFourP:          TThemeText;
    TextP1DuetFourPScore:     TThemeText;

    StaticP2DuetFourPSingBar: TThemeStatic;
    StaticP2DuetFourP:        TThemeStatic;
    StaticP2DuetFourPAvatar:  TThemeStatic;
    StaticP2DuetFourPScoreBG: TThemeStatic; //Static for ScoreBG
    TextP2DuetFourP:          TThemeText;
    TextP2DuetFourPScore:     TThemeText;

    StaticP3DuetFourPSingBar: TThemeStatic;
    StaticP3DuetFourP:        TThemeStatic;
    StaticP3DuetFourPAvatar:  TThemeStatic;
    StaticP3DuetFourPScoreBG: TThemeStatic; //Static for ScoreBG
    TextP3DuetFourP:          TThemeText;
    TextP3DuetFourPScore:     TThemeText;

    StaticP4DuetFourPSingBar: TThemeStatic;
    StaticP4DuetFourP:        TThemeStatic;
    StaticP4DuetFourPAvatar:  TThemeStatic;
    StaticP4DuetFourPScoreBG: TThemeStatic; //Static for ScoreBG
    TextP4DuetFourP:          TThemeText;
    TextP4DuetFourPScore:     TThemeText;

    StaticP1DuetSixPSingBar: TThemeStatic;
    StaticP1DuetSixP:        TThemeStatic;
    StaticP1DuetSixPAvatar:  TThemeStatic;
    StaticP1DuetSixPScoreBG: TThemeStatic; //Static for ScoreBG
    TextP1DuetSixP:          TThemeText;
    TextP1DuetSixPScore:     TThemeText;

    StaticP2DuetSixPSingBar: TThemeStatic;
    StaticP2DuetSixP:        TThemeStatic;
    StaticP2DuetSixPAvatar:  TThemeStatic;
    StaticP2DuetSixPScoreBG: TThemeStatic; //Static for ScoreBG
    TextP2DuetSixP:          TThemeText;
    TextP2DuetSixPScore:     TThemeText;

    StaticP3DuetSixPSingBar: TThemeStatic;
    StaticP3DuetSixP:        TThemeStatic;
    StaticP3DuetSixPAvatar:  TThemeStatic;
    StaticP3DuetSixPScoreBG: TThemeStatic; //Static for ScoreBG
    TextP3DuetSixP:          TThemeText;
    TextP3DuetSixPScore:     TThemeText;

    StaticP4DuetSixPSingBar: TThemeStatic;
    StaticP4DuetSixP:        TThemeStatic;
    StaticP4DuetSixPAvatar:  TThemeStatic;
    StaticP4DuetSixPScoreBG: TThemeStatic; //Static for ScoreBG
    TextP4DuetSixP:          TThemeText;
    TextP4DuetSixPScore:     TThemeText;

    StaticP5DuetSixPSingBar: TThemeStatic;
    StaticP5DuetSixP:        TThemeStatic;
    StaticP5DuetSixPAvatar:  TThemeStatic;
    StaticP5DuetSixPScoreBG: TThemeStatic; //Static for ScoreBG
    TextP5DuetSixP:          TThemeText;
    TextP5DuetSixPScore:     TThemeText;

    StaticP6DuetSixPSingBar: TThemeStatic;
    StaticP6DuetSixP:        TThemeStatic;
    StaticP6DuetSixPAvatar:  TThemeStatic;
    StaticP6DuetSixPScoreBG: TThemeStatic; //Static for ScoreBG
    TextP6DuetSixP:          TThemeText;
    TextP6DuetSixPScore:     TThemeText;

    SingP1Oscilloscope:           TThemePosition;
    SingP1TwoPOscilloscope:       TThemePosition;
    SingP2ROscilloscope:          TThemePosition;
    SingP1ThreePOscilloscope:     TThemePosition;
    SingP2MOscilloscope:          TThemePosition;
    SingP3ROscilloscope:          TThemePosition;
    SingDuetP1ThreePOscilloscope: TThemePosition;
    SingDuetP2MOscilloscope:      TThemePosition;
    SingDuetP3ROscilloscope:      TThemePosition;
    SingP1FourPOscilloscope:      TThemePosition;
    SingP2FourPOscilloscope:      TThemePosition;
    SingP3FourPOscilloscope:      TThemePosition;
    SingP4FourPOscilloscope:      TThemePosition;
    SingP1SixPOscilloscope:       TThemePosition;
    SingP2SixPOscilloscope:       TThemePosition;
    SingP3SixPOscilloscope:       TThemePosition;
    SingP4SixPOscilloscope:       TThemePosition;
    SingP5SixPOscilloscope:       TThemePosition;
    SingP6SixPOscilloscope:       TThemePosition;
    SingP1DuetFourPOscilloscope:  TThemePosition;
    SingP2DuetFourPOscilloscope:  TThemePosition;
    SingP3DuetFourPOscilloscope:  TThemePosition;
    SingP4DuetFourPOscilloscope:  TThemePosition;
    SingP1DuetSixPOscilloscope:   TThemePosition;
    SingP2DuetSixPOscilloscope:   TThemePosition;
    SingP3DuetSixPOscilloscope:   TThemePosition;
    SingP4DuetSixPOscilloscope:   TThemePosition;
    SingP5DuetSixPOscilloscope:   TThemePosition;
    SingP6DuetSixPOscilloscope:   TThemePosition;

    StaticSongName:   TThemeStatic;
    TextSongName:     TThemeText;

    //Linebonus Translations
    LineBonusText:    array [0..8] of UTF8String;

    //Pause Popup
    PausePopUp:      TThemeStatic;

    InfoMessageText: TThemeText;
    InfoMessageBG:   TThemeStatic;

    StaticDuet: AThemeStatic;
  end;

  TThemeJukebox = class(TThemeBasic)
    StaticTimeProgress:   TThemeStatic;
    StaticTimeBackground: TThemeStatic;
    StaticSongBackground: TThemeStatic;
    StaticSongListBackground: TThemeStatic;
    TextTimeText:         TThemeText;
    TextSongText:         TThemeText;
    SongDescription:      TThemeButton;
    FindSong:             TThemeButton;
    RepeatSongList:       TThemeButton;
    SongListOrder:        TThemeButton;
    RandomSongList:       TThemeButton;
    Lyric:                TThemeButton;
    Options:              TThemeButton;
    SongListClose:        TThemeButton;
    SongListFixPin:       TThemeButton;
    TextListText:         TThemeText;
    TextCountText:        TThemeText;
    SongCover:            TThemeStatic;
    SongListPlayPause:    TThemeButton;

    StaticActualSongStatics:    AThemeStatic;
    StaticActualSongCover:      TThemeStatic;
    TextActualSongArtist:       TThemeText;
    TextActualSongTitle:        TThemeText;

    SongListUp:   TThemeButton;
    SongListDown: TThemeButton;

    //Jukebox SongMenu
    StaticSongMenuBackground:     TThemeStatic;
    SongMenuPlayPause:     TThemeButton;
    StaticSongMenuTimeProgress:   TThemeStatic;
    StaticSongMenuTimeBackground: TThemeStatic;
    SongMenuNext:          TThemeButton;
    SongMenuPrevious:      TThemeButton;
    SongMenuPlaylist:      TThemeButton;
    SongMenuTextTime:      TThemeText;
    SongMenuOptions:       TThemeButton;

    //Jukebox SongOptions
    SongOptionsTextSaved:        TThemeText;
    StaticSongOptionsBackground: TThemeStatic;
    SongOptionsClose:            TThemeButton;
    SongOptionsSave:             TThemeButton;
    SongOptionsDefault:          TThemeButton;
    SongOptionsVideoText:        TThemeText;
    SongOptionsLyricText:        TThemeText;
    SongOptionsVideoAspectSlide: TThemeSelectSlide;
    SongOptionsVideoWidthSlide:  TThemeSelectSlide;
    SongOptionsVideoHeightSlide: TThemeSelectSlide;
    SongOptionsLyricSizeSlide:       TThemeSelectSlide;
    SongOptionsLyricPositionSlide:   TThemeSelectSlide;
    SongOptionsLyricColorSlide:      TThemeSelectSlide;
    SongOptionsLyricLineSlide:       TThemeSelectSlide;
    SongOptionsLyricPropertySlide:   TThemeSelectSlide;
    SongOptionsLyricAlphaSlide:      TThemeSelectSlide;
    SelectR:            TThemeSelectSlide;
    SelectG:            TThemeSelectSlide;
    SelectB:            TThemeSelectSlide;
    TexR:               TThemeStatic;
    TexG:               TThemeStatic;
    TexB:               TThemeStatic;
    PointerR:           TThemeStatic;
    PointerG:           TThemeStatic;
    PointerB:           TThemeStatic;
  end;

  TThemeJukeboxPlaylist = class(TThemeBasic)
    SelectPlayList: TThemeSelectSlide;
    SelectPlayList2: TThemeSelectSlide;
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

    AvatarStatic:     array[1..6] of TThemeStatic;

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

    ButtonSend:  array[1..3] of TThemeButton;

    StaticNavigate:   TThemeStatic;
    TextNavigate:     TThemeText;

//    Description:          array[0..5] of string;}
  end;

  TThemeTop5 = class(TThemeBasic)
    TextLevel:        TThemeText;
    TextArtistTitle:  TThemeText;

    StaticNumber:     AThemeStatic;
    TextNumber:       AThemeText;
    TextName:         AThemeText;
    TextScore:        AThemeText;
    TextDate:         AThemeText;
  end;

  TThemeOptions = class(TThemeBasic)
    ButtonGame:       TThemeButton;
    ButtonGraphics:   TThemeButton;
    ButtonSound:      TThemeButton;
    ButtonLyrics:     TThemeButton;
    ButtonThemes:     TThemeButton;
    ButtonRecord:     TThemeButton;
    ButtonAdvanced:   TThemeButton;
    ButtonNetwork:    TThemeButton;
    ButtonWebcam:     TThemeButton;
    ButtonJukebox:    TThemeButton;
    ButtonExit:       TThemeButton;

    TextDescription:      TThemeText;
    Description:          array[0..10] of UTF8String;
  end;

  TThemeOptionsGame = class(TThemeBasic)
    SelectLanguage:     TThemeSelectSlide;
    SelectSongMenu:     TThemeSelectSlide;
    SelectSorting:      TThemeSelectSlide;
    SelectTabs:         TThemeSelectSlide;
    SelectShowScores:   TThemeSelectSlide;
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
    SelectBackgroundMusic:       TThemeSelectSlide;
    SelectClickAssist:           TThemeSelectSlide;
    SelectBeatClick:             TThemeSelectSlide;
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
    SelectThreshold:       TThemeSelectSlide;
    SelectMicBoost:        TThemeSelectSlide;
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
    SelectSingScores:     TThemeSelectSlide;
    SelectTopScores:      TThemeSelectSlide;
    ButtonExit:           TThemeButton;
  end;

  TThemeOptionsNetwork = class(TThemeBasic)
    SelectWebsite:        TThemeSelectSlide;
    SelectUsername:       TThemeSelectSlide;
    SelectSendName:       TThemeSelectSlide;
    SelectAutoMode:       TThemeSelectSlide;
    SelectAutoPlayer:     TThemeSelectSlide;
    SelectAutoScoreEasy:   TThemeSelectSlide;
    SelectAutoScoreMedium: TThemeSelectSlide;
    SelectAutoScoreHard:   TThemeSelectSlide;
    TextInsertUser:       TThemeText;
    ButtonInsert:         TThemeButton;
    ButtonExit:           TThemeButton;
  end;

  TThemeOptionsWebcam = class(TThemeBasic)
    SelectWebcam:         TThemeSelectSlide;
    SelectResolution:     TThemeSelectSlide;
    SelectFPS:            TThemeSelectSlide;
    SelectFlip:           TThemeSelectSlide;
    SelectBrightness:     TThemeSelectSlide;
    SelectSaturation:     TThemeSelectSlide;
    SelectHue:            TThemeSelectSlide;
    SelectEffect:         TThemeSelectSlide;

    ButtonPreVisualization: TThemeButton;
    ButtonExit:           TThemeButton;
  end;

  TThemeOptionsJukebox = class(TThemeBasic)
    SelectLyricsFont:   TThemeSelectSlide;
    SelectLyricsEffect: TThemeSelectSlide;
    SelectLyricsAlpha:  TThemeSelectSlide;
    SelectLine:         TThemeSelectSlide;
    SelectProperty:     TThemeSelectSlide;
    SelectColor:        TThemeSelectSlide;
    SelectR:            TThemeSelectSlide;
    SelectG:            TThemeSelectSlide;
    SelectB:            TThemeSelectSlide;
    TexR:               TThemeStatic;
    TexG:               TThemeStatic;
    TexB:               TThemeStatic;
    TexColor:           TThemeStatic;
    PointerR:           TThemeStatic;
    PointerG:           TThemeStatic;
    PointerB:           TThemeStatic;
    ButtonExit:         TThemeButton;
    UpperX, UpperW, UpperY, UpperH,
    LowerX, LowerW, LowerY, LowerH  : integer;
  end;

  TThemeEdit = class(TThemeBasic)
    ButtonConvert:        TThemeButton;
    ButtonExit:           TThemeButton;

    TextDescription:      TThemeText;
    TextDescriptionLong:  TThemeText;
    Description:          array[0..5] of UTF8string;
    DescriptionLong:      array[0..5] of UTF8string;
  end;

  TThemeEditSub = class(TThemeBasic)
      //in editor - headers
      BackgroundImage:     TThemeStatic;
      ButtonCurrentLine:   TThemeButton;
      ButtonCurrentNote:   TThemeButton;
      // buttons
      PlayOnly:             TThemeButton;
      PlayWithNote:         TThemeButton;
      PlayNote:             TThemeButton;
      previousseq:          TThemeButton;
      nextseq:              TThemeButton;
      undo:                 TThemeButton;
      gold:                 TThemeButton;
      freestyle:            TThemeButton;

      SlideTitle:          TThemeSelectSlide;
      SlideArtist:         TThemeSelectSlide;
      SlideMP3:            TThemeSelectSlide;
      SlideCover:          TThemeSelectSlide;
      SlideBackground:     TThemeSelectSlide;
      SlideBPM:            TThemeSelectSlide;
      SlideGAP:            TThemeSelectSlide;
      SlideStart:          TThemeSelectSlide;
      SlideDuration:       TThemeSelectSlide;
      SlideTone:           TThemeSelectSlide;
      SlideLyric:          TThemeSelectSlide;
      SelectVolAudio:      TThemeSelectSlide;
      SelectVolMidi:       TThemeSelectSlide;
      SelectVolClick:      TThemeSelectSlide;
      SlideVideoGap:       TThemeSelectSlide;
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

  TThemeInsertUser = class(TThemeBasic)
    TextInsertUser: TThemeText;
    ButtonUsername: TThemeButton;
    ButtonPassword: TThemeButton;
    Button1: TThemeButton;
    Button2: TThemeButton;
  end;

  TThemeSendScore = class(TThemeBasic)
    SelectSlide1: TThemeSelectSlide;
    SelectSlide2: TThemeSelectSlide;
    SelectSlide3: TThemeSelectSlide;
    ButtonUsername: TThemeButton;
    ButtonPassword: TThemeButton;
    Button1:  TThemeButton;
    Button2:  TThemeButton;
  end;

  TThemeScoreDownload = class(TThemeBasic)
    Button1: TThemeButton;
    TextSongScoreDownload: TThemeText;
    TextWebScoreDownload: TThemeText;
    DownloadProgressSong: TThemeStatic;
    DownloadProgressWeb: TThemeStatic;
  end;

  //ScreenSong Menue
  TThemeSongMenu = class(TThemeBasic)
    Button1: TThemeButton;
    Button2: TThemeButton;
    Button3: TThemeButton;
    Button4: TThemeButton;
    Button5: TThemeButton;

    SelectSlide1: TThemeSelectSlide;
    SelectSlide2: TThemeSelectSlide;
    SelectSlide3: TThemeSelectSlide;

    TextMenu: TThemeText;
  end;

  TThemeSongJumpTo = class(TThemeBasic)
    ButtonSearchText: TThemeButton;
    SelectSlideType:  TThemeSelectSlide;
    TextFound:        TThemeText;

    //Translated Texts
    Songsfound:       UTF8String;
    NoSongsfound:     UTF8String;
    CatText:          UTF8String;
    IType:            array [0..2] of UTF8String;
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
    SelectMode:  TThemeSelectSlide;
    SelectLevel: TThemeSelectSlide;
    SelectPlayList: TThemeSelectSlide;
    SelectPlayList2: TThemeSelectSlide;

    {ButtonNext: TThemeButton;
    ButtonPrev: TThemeButton;}
  end;

  TThemePartyPlayer = class(TThemeBasic)
    SelectTeams: TThemeSelectSlide;
    SelectPlayers1: TThemeSelectSlide;
    SelectPlayers2: TThemeSelectSlide;
    SelectPlayers3: TThemeSelectSlide;

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

  TThemePartyRounds = class(TThemeBasic)
    SelectRoundCount: TThemeSelectSlide;
    SelectRound: array [0..6] of TThemeSelectSlide;
  end;

  TThemePartyTournamentPlayer = class(TThemeBasic)
    SelectPlayers: TThemeSelectSlide;

    Player1Name: TThemeButton;
    Player2Name: TThemeButton;
    Player3Name: TThemeButton;
    Player4Name: TThemeButton;
    Player5Name: TThemeButton;
    Player6Name: TThemeButton;
    Player7Name: TThemeButton;
    Player8Name: TThemeButton;
    Player9Name: TThemeButton;
    Player10Name: TThemeButton;
    Player11Name: TThemeButton;
    Player12Name: TThemeButton;
    Player13Name: TThemeButton;
    Player14Name: TThemeButton;
    Player15Name: TThemeButton;
    Player16Name: TThemeButton;
  end;

  TThemePartyTournamentOptions = class(TThemeBasic)
    SelectRoundsFinal:  TThemeSelectSlide;
    SelectRounds2Final: TThemeSelectSlide;
    SelectRounds4Final: TThemeSelectSlide;
    SelectRounds8Final: TThemeSelectSlide;
  end;

  TThemePartyTournamentRounds = class(TThemeBasic)
    TextNamePlayer: array[0..1, 0..7] of TThemeButton;
    TextWinner: TThemeText;
    TextResult: TThemeText;
    NextPlayers: TThemeText;
  end;

  TThemePartyTournamentWin = class(TThemeBasic)
    TextScorePlayer1:    TThemeText;
    TextScorePlayer2:    TThemeText;
    TextNamePlayer1:     TThemeText;
    TextNamePlayer2:     TThemeText;
    StaticBGPlayer1:     TThemeStatic;
    StaticBGPlayer2:     TThemeStatic;
  end;

  //About
  TThemeAboutMain = class(TThemeBasic)
    ButtonCredits:    TThemeButton;
    ButtonExit:       TThemeButton;

    TextOverview:     TThemeText;
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

    Description:      array[0..3] of UTF8String;
    DescriptionR:     array[0..3] of UTF8String;
    FormatStr:        array[0..3] of UTF8String;
    PageStr:          UTF8String;
  end;

  //Playlist Translations
  TThemePlaylist = record
    CatText:    UTF8String;
  end;

  TThemeEntry = record
    Name: string;
    Filename: IPath;
    DefaultSkin: integer;
    Creator: string;
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
    procedure LoadHeader(FileName: IPath);
  public
    Themes:           array of TThemeEntry;
    Loading:          TThemeLoading;
    Main:             TThemeMain;
    Name:             TThemeName;
    Level:            TThemeLevel;
    Song:             TThemeSong;
    Sing:             TThemeSing;
    LyricBar:         TThemeLyricBar;
    LyricBarDuetP1:   TThemeLyricBar;
    LyricBarDuetP2:   TThemeLyricBar;
    LyricBarJukebox:  TThemeLyricBar;
    Jukebox:          TThemeJukebox;
    JukeboxPlaylist:  TThemeJukeboxPlaylist;
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
    OptionsNetwork:   TThemeOptionsNetwork;
    OptionsWebcam:    TThemeOptionsWebcam;
    OptionsJukebox:   TThemeOptionsJukebox;
    //edit
    Edit:             TThemeEdit;
    EditSub:          TThemeEditSub;
    //error and check popup
    ErrorPopup:         TThemeError;
    CheckPopup:         TThemeCheck;
    InsertUserPopup:    TThemeInsertUser;
    SendScorePopup:     TThemeSendScore;
    ScoreDownloadPopup: TThemeScoreDownload;
    //ScreenSong extensions
    SongMenu:         TThemeSongMenu;
    SongJumpto:       TThemeSongJumpTo;
    //Party Screens:
    PartyNewRound:    TThemePartyNewRound;
    PartyScore:       TThemePartyScore;
    PartyWin:         TThemePartyWin;
    PartyOptions:     TThemePartyOptions;
    PartyPlayer:      TThemePartyPlayer;
    PartyRounds:      TThemePartyRounds;

    //Tournament
    PartyTournamentPlayer: TThemePartyTournamentPlayer;
    PartyTournamentOptions: TThemePartyTournamentOptions;
    PartyTournamentRounds: TThemePartyTournamentRounds;
    PartyTournamentWin: TThemePartyTournamentWin;

    // About
    AboutMain:        TThemeAboutMain;

    //Stats Screens:
    StatMain:         TThemeStatMain;
    StatDetail:       TThemeStatDetail;

    Playlist:         TThemePlaylist;

    ILevel: array[0..2] of UTF8String;
    IMode:  array[0..3] of UTF8String;

    constructor Create;

    procedure LoadList;

    function LoadTheme(ThemeNum: integer; sColor: integer): boolean; // Load some theme settings from file

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
    procedure ThemeLoadPosition(var ThemePosition: TThemePosition; const Name: string);

    procedure ThemeSave(const FileName: string);
    procedure ThemeSaveBasic(Theme: TThemeBasic; const Name: string);
    procedure ThemeSaveBackground(ThemeBackground: TThemeBackground; const Name: string);
    procedure ThemeSaveStatic(ThemeStatic: TThemeStatic; const Name: string);
    procedure ThemeSaveStatics(ThemeStatic: AThemeStatic; const Name: string);
    procedure ThemeSaveText(ThemeText: TThemeText; const Name: string);
    procedure ThemeSaveTexts(ThemeText: AThemeText; const Name: string);
    procedure ThemeSaveButton(ThemeButton: TThemeButton; const Name: string);

    procedure ThemeScoreLoad;
    procedure ThemePartyLoad;
    procedure ThemeSongLoad;
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

function GetJukeboxLyricOtherColor(Line: integer): TRGB;
function GetJukeboxLyricOtherOutlineColor(Line: integer): TRGB;
function GetLyricColor(Color: integer): TRGB;
function GetLyricGrayColor(Color: integer): TRGB;
function GetLyricOutlineColor(Color: integer): TRGB;
function GetLyricBarColor(Color: integer): TRGB;

function GetPlayerColor(Color: integer): TRGB;
function GetPlayerLightColor(Color: integer): TRGB;
function GetPlayerLightColorV2(Color: integer): TRGB;
procedure LoadPlayersColors;
procedure LoadTeamsColors;

var
  //Skin:         TSkin;
  Theme:        TTheme;
  Color:        array of TColor;
  LastC:        integer;

implementation

uses
  ULanguage,
  USkins,
  UIni,
  UPathUtils,
  UFileSystem,
  TextGL,
  gl,
  glext,
  math,
  StrUtils;

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

constructor TTheme.Create;
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
  OptionsNetwork := TThemeOptionsNetwork.Create;
  OptionsWebcam := TThemeOptionsWebcam.Create;
  OptionsJukebox := TThemeOptionsJukebox.Create;

  Edit := TThemeEdit.Create;
  EditSub := TThemeEditSub.Create;

  ErrorPopup := TThemeError.Create;
  CheckPopup := TThemeCheck.Create;
  InsertUserPopup := TThemeInsertUser.Create;
  SendScorePopup := TThemeSendScore.Create;
  ScoreDownloadPopup := TThemeScoreDownload.Create;

  SongMenu := TThemeSongMenu.Create;
  SongJumpto := TThemeSongJumpto.Create;
  //Party Screens
  PartyNewRound := TThemePartyNewRound.Create;
  PartyWin := TThemePartyWin.Create;
  PartyScore := TThemePartyScore.Create;
  PartyOptions := TThemePartyOptions.Create;
  PartyPlayer := TThemePartyPlayer.Create;
  PartyRounds := TThemePartyRounds.Create;

  // Tournament
  PartyTournamentPlayer := TThemePartyTournamentPlayer.Create;
  PartyTournamentOptions := TThemePartyTournamentOptions.Create;
  PartyTournamentRounds := TThemePartyTournamentRounds.Create;
  PartyTournamentWin := TThemePartyTournamentWin.Create;

  // About
  AboutMain :=   TThemeAboutMain.Create;

  //Stats Screens:
  StatMain :=   TThemeStatMain.Create;
  StatDetail := TThemeStatDetail.Create;

  JukeboxPlaylist := TThemeJukeboxPlaylist.Create;

  //LoadTheme(FileName, Color);
  LoadList;
end;

procedure TTheme.LoadHeader(FileName: IPath);
  var
    Entry: TThemeEntry;
    Ini: TMemIniFile;
    SkinName: string;
    SkinsFound: boolean;
    ThemeVersion: string;
    I: integer;
    Len: integer;
    Skins: TUTF8StringDynArray;
begin
  Entry.Filename := ThemePath.Append(FileName);
  //read info from theme header
  Ini := TMemIniFile.Create(Entry.Filename.ToNative);

  Entry.Name := Ini.ReadString('Theme', 'Name', FileName.SetExtension('').ToNative);
  ThemeVersion := Trim(UpperCase(Ini.ReadString('Theme', 'US_Version', 'no version tag')));
  Entry.Creator := Ini.ReadString('Theme', 'Creator', 'Unknown');
  SkinName := Ini.ReadString('Theme', 'DefaultSkin', FileName.SetExtension('').ToNative);

  Ini.Free;

  // don't load theme with wrong version tag
  if ThemeVersion <> 'USD 110' then
  begin
    Log.LogWarn('Wrong Version (' + ThemeVersion + ') in Theme : ' + Entry.Name, 'Theme.LoadHeader');
  end
  else
  begin
    //Search for Skins for this Theme
    SkinsFound := false;
    for I := Low(Skin.Skin) to High(Skin.Skin) do
    begin
      if (CompareText(Skin.Skin[I].Theme, Entry.Name) = 0) then
      begin
        SkinsFound := true;
        break;
      end;
    end;

    if SkinsFound then
    begin
      { found a valid Theme }
      // set correct default skin
      Skin.GetSkinsByTheme(Entry.Name, Skins);
      Entry.DefaultSkin := max(0, GetArrayIndex(Skins, SkinName, true));

      Len := Length(Themes);
      SetLength(Themes, Len + 1);
      SetLength(ITheme, Len + 1);
      Themes[Len] := Entry;
      ITheme[Len] := Entry.Name;
    end;
  end;
end;

procedure TTheme.LoadList;
  var
    Iter: IFileIterator;
    FileInfo: TFileInfo;
begin
  Log.LogStatus('Searching for Theme : ' + ThemePath.ToNative + '*.ini', 'Theme.LoadList');

  Iter := FileSystem.FileFind(ThemePath.Append('*.ini'), 0);
  while (Iter.HasNext) do
  begin
    FileInfo := Iter.Next;
    Log.LogStatus('Found Theme: ' + FileInfo.Name.ToNative, 'Theme.LoadList');
    LoadHeader(Fileinfo.Name);
  end;
end;

function TTheme.LoadTheme(ThemeNum: integer; sColor: integer): boolean;
var
  I, J:    integer;
begin
  Result := false;

  CreateThemeObjects();

  Log.LogStatus('Loading: '+ Themes[ThemeNum].FileName.ToNative, 'TTheme.LoadTheme');

  if not Themes[ThemeNum].FileName.IsFile() then
  begin
    Log.LogError('Theme does not exist ('+ Themes[ThemeNum].FileName.ToNative +')', 'TTheme.LoadTheme');
  end;

  if Themes[ThemeNum].FileName.IsFile() then
  begin
    Result := true;

    {$IFDEF THEMESAVE}
    ThemeIni := TIniFile.Create(Themes[ThemeNum].FileName.ToNative);
    {$ELSE}
    ThemeIni := TMemIniFile.Create(Themes[ThemeNum].FileName.ToNative);
    {$ENDIF}

    if ThemeIni.ReadString('Theme', 'Name', '') <> '' then
    begin

      {Skin.SkinName := ThemeIni.ReadString('Theme', 'Name', 'Singstar');
      Skin.SkinPath := 'Skins\' + Skin.SkinName + '\';
      Skin.SkinReg := false; }
      Skin.Color := sColor;

      Skin.LoadSkin(ISkin[Ini.SkinNo], Themes[ThemeNum].Name);

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
      ThemeLoadButton(Main.ButtonJukebox, 'MainButtonJukebox');
      ThemeLoadButton(Main.ButtonStat, 'MainButtonStats');
      ThemeLoadButton(Main.ButtonEditor, 'MainButtonEditor');
      ThemeLoadButton(Main.ButtonOptions, 'MainButtonOptions');
      ThemeLoadButton(Main.ButtonAbout, 'MainButtonAbout');
      ThemeLoadButton(Main.ButtonExit, 'MainButtonExit');

      //Main Desc Text Translation Start

      Main.Description[0] := Language.Translate('SING_SING');
      Main.DescriptionLong[0] := Language.Translate('SING_SING_DESC');
      Main.Description[1] := Language.Translate('SING_MULTI');
      Main.DescriptionLong[1] := Language.Translate('SING_MULTI_DESC');
      Main.Description[2] := Language.Translate('SING_JUKEBOX');
      Main.DescriptionLong[2] := Language.Translate('SING_JUKEBOX_DESC');
      Main.Description[3] := Language.Translate('SING_STATS');
      Main.DescriptionLong[3] := Language.Translate('SING_STATS_DESC');
      Main.Description[4] := Language.Translate('SING_EDITOR');
      Main.DescriptionLong[4] := Language.Translate('SING_EDITOR_DESC');
      Main.Description[5] := Language.Translate('SING_GAME_OPTIONS');
      Main.DescriptionLong[5] := Language.Translate('SING_GAME_OPTIONS_DESC');
      Main.Description[6] := Language.Translate('SING_ABOUT');
      Main.DescriptionLong[6] := Language.Translate('SING_ABOUT_DESC');
      Main.Description[7] := Language.Translate('SING_EXIT');
      Main.DescriptionLong[7] := Language.Translate('SING_EXIT_DESC');

      //Main Desc Text Translation End

      Main.TextDescription.Text := Main.Description[0];
      Main.TextDescriptionLong.Text := Main.DescriptionLong[0];

      // Name
      ThemeLoadBasic(Name, 'Name');

      ThemeLoadButton(Name.PlayerButtonName, 'NamePlayerButtonName');
      ThemeLoadButton(Name.PlayerButtonAvatar, 'NamePlayerButtonAvatar');

      Name.PlayerScrollAvatar.NumAvatars := ThemeIni.ReadInteger('NamePlayerScrollAvatar', 'Count', 5);
      Name.PlayerScrollAvatar.DistanceAvatars := ThemeIni.ReadInteger('NamePlayerScrollAvatar', 'Distance', 40);

      ThemeLoadButton(Name.PlayerAvatar, 'NamePlayerAvatar');

      ThemeLoadSelectSlide(Name.SelectPlayersCount, 'NameSelectPlayerCount');
      ThemeLoadSelectSlide(Name.SelectPlayerColor, 'NameSelectPlayerColor');
      ThemeLoadSelectSlide(Name.SelectPlayerLevel, 'NameSelectPlayerLevel');

      for I := 0 to 5 do
      begin
        ThemeLoadStatic(Name.PlayerSelect[I], 'NamePlayerSelectStatic' + IntToStr(I + 1));
        ThemeLoadText(Name.PlayerSelectText[I], 'NamePlayerSelectStatic' + IntToStr(I + 1) + 'Text');
        ThemeLoadStatic(Name.PlayerSelectAvatar[I], 'NamePlayerSelectStatic' + IntToStr(I + 1) + 'Avatar');
      end;

      ThemeLoadButton(Name.PlayerSelectCurrent, 'NamePlayerSelectCurrent');

      // Level
      ThemeLoadBasic(Level, 'Level');

      ThemeLoadButton(Level.ButtonEasy, 'LevelButtonEasy');
      ThemeLoadButton(Level.ButtonMedium, 'LevelButtonMedium');
      ThemeLoadButton(Level.ButtonHard, 'LevelButtonHard');

      //Song
      ThemeSongLoad();

      //LyricBar
      LyricBar.UpperX := ThemeIni.ReadInteger('SingLyricsUpperBar', 'X', 0);
      LyricBar.UpperW := ThemeIni.ReadInteger('SingLyricsUpperBar', 'W', 0);
      LyricBar.UpperY := ThemeIni.ReadInteger('SingLyricsUpperBar', 'Y', 0);
      LyricBar.UpperH := ThemeIni.ReadInteger('SingLyricsUpperBar', 'H', 0);
      LyricBar.IndicatorYOffset := ThemeIni.ReadInteger('SingLyricsUpperBar', 'IndicatorYOffset', 0);
      LyricBar.LowerX := ThemeIni.ReadInteger('SingLyricsLowerBar', 'X', 0);
      LyricBar.LowerW := ThemeIni.ReadInteger('SingLyricsLowerBar', 'W', 0);
      LyricBar.LowerY := ThemeIni.ReadInteger('SingLyricsLowerBar', 'Y', 0);
      LyricBar.LowerH := ThemeIni.ReadInteger('SingLyricsLowerBar', 'H', 0);

      //LyricBarDuet
      LyricBarDuetP1.UpperX := ThemeIni.ReadInteger('SingLyricsDuetP1UpperBar', 'X', 0);
      LyricBarDuetP1.UpperW := ThemeIni.ReadInteger('SingLyricsDuetP1UpperBar', 'W', 0);
      LyricBarDuetP1.UpperY := ThemeIni.ReadInteger('SingLyricsDuetP1UpperBar', 'Y', 0);
      LyricBarDuetP1.UpperH := ThemeIni.ReadInteger('SingLyricsDuetP1UpperBar', 'H', 0);
      LyricBarDuetP1.IndicatorYOffset := ThemeIni.ReadInteger('SingLyricsDuetP1UpperBar', 'IndicatorYOffset', 0);
      LyricBarDuetP1.LowerX := ThemeIni.ReadInteger('SingLyricsDuetP1LowerBar', 'X', 0);
      LyricBarDuetP1.LowerW := ThemeIni.ReadInteger('SingLyricsDuetP1LowerBar', 'W', 0);
      LyricBarDuetP1.LowerY := ThemeIni.ReadInteger('SingLyricsDuetP1LowerBar', 'Y', 0);
      LyricBarDuetP1.LowerH := ThemeIni.ReadInteger('SingLyricsDuetP1LowerBar', 'H', 0);

      LyricBarDuetP2.UpperX := ThemeIni.ReadInteger('SingLyricsDuetP2UpperBar', 'X', 0);
      LyricBarDuetP2.UpperW := ThemeIni.ReadInteger('SingLyricsDuetP2UpperBar', 'W', 0);
      LyricBarDuetP2.UpperY := ThemeIni.ReadInteger('SingLyricsDuetP2UpperBar', 'Y', 0);
      LyricBarDuetP2.UpperH := ThemeIni.ReadInteger('SingLyricsDuetP2UpperBar', 'H', 0);
      LyricBarDuetP2.IndicatorYOffset := ThemeIni.ReadInteger('SingLyricsDuetP2UpperBar', 'IndicatorYOffset', 0);
      LyricBarDuetP2.LowerX := ThemeIni.ReadInteger('SingLyricsDuetP2LowerBar', 'X', 0);
      LyricBarDuetP2.LowerW := ThemeIni.ReadInteger('SingLyricsDuetP2LowerBar', 'W', 0);
      LyricBarDuetP2.LowerY := ThemeIni.ReadInteger('SingLyricsDuetP2LowerBar', 'Y', 0);
      LyricBarDuetP2.LowerH := ThemeIni.ReadInteger('SingLyricsDuetP2LowerBar', 'H', 0);

      // Lyric Jukebox
      { Need to change calculation in SongOptions
      LyricBarJukebox.UpperX := ThemeIni.ReadInteger('JukeboxLyricsUpperBar', 'X', 0);
      LyricBarJukebox.UpperW := ThemeIni.ReadInteger('JukeboxLyricsUpperBar', 'W', 0);
      LyricBarJukebox.UpperY := ThemeIni.ReadInteger('JukeboxLyricsUpperBar', 'Y', 0);
      LyricBarJukebox.UpperH := ThemeIni.ReadInteger('JukeboxLyricsUpperBar', 'H', 0);
      LyricBarJukebox.LowerX := ThemeIni.ReadInteger('JukeboxLyricsLowerBar', 'X', 0);
      LyricBarJukebox.LowerW := ThemeIni.ReadInteger('JukeboxLyricsLowerBar', 'W', 0);
      LyricBarJukebox.LowerY := ThemeIni.ReadInteger('JukeboxLyricsLowerBar', 'Y', 0);
      LyricBarJukebox.LowerH := ThemeIni.ReadInteger('JukeboxLyricsLowerBar', 'H', 0);
      LyricBarJukebox.IndicatorYOffset := ThemeIni.ReadInteger('JukeboxLyricsUpperBar', 'IndicatorYOffset', 0);
      }

      LyricBarJukebox.UpperX := 40;
      LyricBarJukebox.UpperW := 720;
      LyricBarJukebox.UpperY := 490;
      LyricBarJukebox.UpperH := 52;
      LyricBarJukebox.LowerX := 40;
      LyricBarJukebox.LowerW := 720;
      LyricBarJukebox.LowerY := 540;
      LyricBarJukebox.LowerH := 52;
      LyricBarJukebox.IndicatorYOffset := 8;

      // Jukebox
      ThemeLoadStatic(Jukebox.StaticTimeProgress, 'JukeboxTimeProgress');
      ThemeLoadStatic(Jukebox.StaticTimeBackground, 'JukeboxTimeBackground');
      ThemeLoadStatic(Jukebox.StaticSongBackground, 'JukeboxSongBackground');
      ThemeLoadStatic(Jukebox.StaticSongListBackground, 'JukeboxSongListBackground');
      //ThemeLoadText(Jukebox.TextTimeText, 'JukeboxTimeText');
      //ThemeLoadText(Jukebox.TextTimeDesc, 'JukeboxTimeDesc');
      //ThemeLoadText(Jukebox.TextSongText, 'JukeboxTextSong');
      ThemeLoadButton(Jukebox.SongDescription, 'JukeboxSongDescription');
      ThemeLoadButton(Jukebox.FindSong, 'JukeboxFind');
      ThemeLoadButton(Jukebox.RepeatSongList, 'JukeboxRepeat');
      ThemeLoadButton(Jukebox.SongListPlayPause, 'JukeboxPlayPause');
      ThemeLoadButton(Jukebox.SongListOrder, 'JukeboxSort');
      ThemeLoadButton(Jukebox.RandomSongList, 'JukeboxRandom');
      ThemeLoadButton(Jukebox.Lyric, 'JukeboxLyric');
      ThemeLoadButton(Jukebox.SongListClose, 'JukeboxSongListClose');
      ThemeLoadButton(Jukebox.Options, 'JukeboxOptions');
      ThemeLoadText(Jukebox.TextListText, 'JukeboxListText');
      ThemeLoadText(Jukebox.TextCountText, 'JukeboxCountText');
      ThemeLoadStatic(Jukebox.SongCover, 'JukeboxSongCover');

      ThemeLoadStatics(Jukebox.StaticActualSongStatics, 'JukeboxStaticActualSong');
      ThemeLoadStatic(Jukebox.StaticActualSongCover, 'JukeboxStaticActualSongCover');
      ThemeLoadText(Jukebox.TextActualSongArtist, 'JukeboxTextActualSongArtist');
      ThemeLoadText(Jukebox.TextActualSongTitle, 'JukeboxTextActualSongTitle');

      ThemeLoadButton(Jukebox.SongListUp, 'JukeboxSongListUp');
      ThemeLoadButton(Jukebox.SongListDown, 'JukeboxSongListDown');

      // Jukebox SongMenu
      ThemeLoadStatic(Jukebox.StaticSongMenuTimeProgress, 'JukeboxSongMenuTimeProgress');
      ThemeLoadStatic(Jukebox.StaticSongMenuTimeBackground, 'JukeboxSongMenuTimeBackground');
      ThemeLoadText(Jukebox.SongMenuTextTime, 'JukeboxSongMenuTextTime');

      ThemeLoadStatic(Jukebox.StaticSongMenuBackground, 'JukeboxSongMenuBackground');
      ThemeLoadButton(Jukebox.SongMenuPlayPause, 'JukeboxSongMenuPlayPause');

      ThemeLoadButton(Jukebox.SongMenuNext, 'JukeboxSongMenuNext');
      ThemeLoadButton(Jukebox.SongMenuPrevious, 'JukeboxSongMenuPrevious');
      ThemeLoadButton(Jukebox.SongMenuPlaylist, 'JukeboxSongMenuPlaylist');
      ThemeLoadButton(Jukebox.SongMenuOptions, 'JukeboxSongMenuOptions');

      // Jukebox SongOptions
      ThemeLoadText(Jukebox.SongOptionsTextSaved, 'JukeboxSongOptionsTextSaved');
      ThemeLoadStatic(Jukebox.StaticSongOptionsBackground, 'JukeboxSongOptionsBackground');
      ThemeLoadButton(Jukebox.SongOptionsClose, 'JukeboxSongOptionsClose');
      ThemeLoadButton(Jukebox.SongOptionsDefault, 'JukeboxSongOptionsDefault');
      ThemeLoadButton(Jukebox.SongOptionsSave, 'JukeboxSongOptionsSave');
      ThemeLoadButton(Jukebox.SongListFixPin, 'JukeboxSongListFixPin');
      ThemeLoadText(Jukebox.SongOptionsVideoText, 'JukeboxSongOptionsVideoText');
      ThemeLoadText(Jukebox.SongOptionsLyricText, 'JukeboxSongOptionsLyricText');
      ThemeLoadSelectSlide(Jukebox.SongOptionsVideoAspectSlide, 'JukeboxSongOptionsVideoAspectSlide');
      ThemeLoadSelectSlide(Jukebox.SongOptionsVideoWidthSlide, 'JukeboxSongOptionsVideoWidthSlide');
      ThemeLoadSelectSlide(Jukebox.SongOptionsVideoHeightSlide, 'JukeboxSongOptionsVideoHeightSlide');
      ThemeLoadSelectSlide(Jukebox.SongOptionsLyricSizeSlide, 'JukeboxSongOptionsLyricSizeSlide');
      ThemeLoadSelectSlide(Jukebox.SongOptionsLyricPositionSlide, 'JukeboxSongOptionsLyricPositionSlide');
      ThemeLoadSelectSlide(Jukebox.SongOptionsLyricAlphaSlide, 'JukeboxSongOptionsLyricAlphaSlide');
      ThemeLoadSelectSlide(Jukebox.SongOptionsLyricColorSlide, 'JukeboxSongOptionsLyricColorSlide');
      ThemeLoadSelectSlide(Jukebox.SongOptionsLyricLineSlide, 'JukeboxSongOptionsLyricLineSlide');
      ThemeLoadSelectSlide(Jukebox.SongOptionsLyricPropertySlide, 'JukeboxSongOptionsLyricPropertySlide');
      ThemeLoadSelectSlide(Jukebox.SelectR,    'JukeboxSongOptionsLyricSelectR');
      ThemeLoadSelectSlide(Jukebox.SelectG,    'JukeboxSongOptionsLyricSelectG');
      ThemeLoadSelectSlide(Jukebox.SelectB,    'JukeboxSongOptionsLyricSelectB');
      ThemeLoadStatic(Jukebox.PointerR,        'JukeboxSongOptionsLyricPointerR');
      ThemeLoadStatic(Jukebox.PointerG,        'JukeboxSongOptionsLyricPointerG');
      ThemeLoadStatic(Jukebox.PointerB,        'JukeboxSongOptionsLyricPointerB');
      ThemeLoadStatic(Jukebox.TexR,            'JukeboxSongOptionsLyricRed');
      ThemeLoadStatic(Jukebox.TexG,            'JukeboxSongOptionsLyricGreen');
      ThemeLoadStatic(Jukebox.TexB,            'JukeboxSongOptionsLyricBlue');

      // JukeboxPlaylist
      ThemeLoadBasic(JukeboxPlaylist, 'JukeboxPlaylist');
      ThemeLoadSelectSlide(JukeboxPlaylist.SelectPlayList, 'JukeboxPlaylistSelectPlayList');
      ThemeLoadSelectSlide(JukeboxPlaylist.SelectPlayList2, 'JukeboxPlaylistSelectPlayList2');

      // Sing
      ThemeLoadBasic(Sing, 'Sing');

      ThemeLoadStatics (Sing.StaticDuet, 'SingStaticDuet');

      //TimeBar mod
       ThemeLoadStatic(Sing.StaticTimeProgress, 'SingTimeProgress');
       ThemeLoadText(Sing.TextTimeText, 'SingTimeText');
      //eoa TimeBar mod

      ThemeLoadText (Sing.InfoMessageText, 'SingInfoMessageText');
      ThemeLoadStatic (Sing.InfoMessageBG, 'SingInfoMessageBG');

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
      ThemeLoadStatic(Sing.StaticP1Avatar, 'SingP1Avatar');


  //Added for ps3 skin
  //This one is shown in 2/4P mode
  //if it exists, otherwise the one Player equivaltents are used
      if (ThemeIni.SectionExists('SingP1TwoPTextScore')) then
      begin
        ThemeLoadStatic(Sing.StaticP1TwoP, 'SingP1TwoPStatic');
        ThemeLoadStatic(Sing.StaticP1TwoPAvatar, 'SingP1TwoPAvatar');
        ThemeLoadText(Sing.TextP1TwoP, 'SingP1TwoPText');
        ThemeLoadStatic(Sing.StaticP1TwoPScoreBG, 'SingP1TwoPStatic2');
        ThemeLoadText(Sing.TextP1TwoPScore, 'SingP1TwoPTextScore');
      end
      else
      begin
        Sing.StaticP1TwoP := Sing.StaticP1;
        Sing.StaticP1TwoPAvatar := Sing.StaticP1Avatar;
        Sing.TextP1TwoP := Sing.TextP1;
        Sing.StaticP1TwoPScoreBG := Sing.StaticP1ScoreBG;
        Sing.TextP1TwoPScore := Sing.TextP1Score;
      end;

  //This one is shown in 3/6P mode
  //if it exists, otherwise the one Player equivaltents are used
      if (ThemeIni.SectionExists('SingP1TwoPTextScore')) then
      begin
        ThemeLoadStatic(Sing.StaticP1ThreeP, 'SingP1ThreePStatic');
        ThemeLoadStatic(Sing.StaticP1ThreePAvatar, 'SingP1ThreePAvatar');
        ThemeLoadText(Sing.TextP1ThreeP, 'SingP1ThreePText');
        ThemeLoadStatic(Sing.StaticP1ThreePScoreBG, 'SingP1ThreePStatic2');
        ThemeLoadText(Sing.TextP1ThreePScore, 'SingP1ThreePTextScore');
      end
      else
      begin
        Sing.StaticP1ThreeP := Sing.StaticP1;
        Sing.StaticP1ThreePAvatar := Sing.StaticP1Avatar;
        Sing.TextP1ThreeP := Sing.TextP1;
        Sing.StaticP1ThreePScoreBG := Sing.StaticP1ScoreBG;
        Sing.TextP1ThreePScore := Sing.TextP1Score;
      end;
  //eoa
      ThemeLoadStatic(Sing.StaticP2R, 'SingP2RStatic');
      ThemeLoadText(Sing.TextP2R, 'SingP2RText');
      ThemeLoadStatic(Sing.StaticP2RScoreBG, 'SingP2RStatic2');
      ThemeLoadText(Sing.TextP2RScore, 'SingP2RTextScore');
      ThemeLoadStatic(Sing.StaticP2RAvatar, 'SingP2RAvatar');

      ThemeLoadStatic(Sing.StaticP2M, 'SingP2MStatic');
      ThemeLoadText(Sing.TextP2M, 'SingP2MText');
      ThemeLoadStatic(Sing.StaticP2MScoreBG, 'SingP2MStatic2');
      ThemeLoadText(Sing.TextP2MScore, 'SingP2MTextScore');
      ThemeLoadStatic(Sing.StaticP2MAvatar, 'SingP2MAvatar');

      ThemeLoadStatic(Sing.StaticP3R, 'SingP3RStatic');
      ThemeLoadText(Sing.TextP3R, 'SingP3RText');
      ThemeLoadStatic(Sing.StaticP3RScoreBG, 'SingP3RStatic2');
      ThemeLoadText(Sing.TextP3RScore, 'SingP3RTextScore');
      ThemeLoadStatic(Sing.StaticP3RAvatar, 'SingP3RAvatar');

      ThemeLoadStatic(Sing.StaticSongName, 'SingSongNameStatic');
      ThemeLoadText(Sing.TextSongName, 'SingSongNameText');

      // 3/6 players duet
      ThemeLoadStatic(Sing.StaticDuetP1ThreeP, 'SingDuetP1ThreePStatic');
      ThemeLoadText(Sing.TextDuetP1ThreeP, 'SingDuetP1ThreePText');
      ThemeLoadStatic(Sing.StaticDuetP1ThreePScoreBG, 'SingDuetP1ThreePStatic2');
      ThemeLoadText(Sing.TextDuetP1ThreePScore, 'SingDuetP1ThreePTextScore');
      ThemeLoadStatic(Sing.StaticDuetP1ThreePAvatar, 'SingDuetP1ThreePAvatar');

      ThemeLoadStatic(Sing.StaticDuetP2M, 'SingDuetP2MStatic');
      ThemeLoadText(Sing.TextDuetP2M, 'SingDuetP2MText');
      ThemeLoadStatic(Sing.StaticDuetP2MScoreBG, 'SingDuetP2MStatic2');
      ThemeLoadText(Sing.TextDuetP2MScore, 'SingDuetP2MTextScore');
      ThemeLoadStatic(Sing.StaticDuetP2MAvatar, 'SingDuetP2MAvatar');

      ThemeLoadStatic(Sing.StaticDuetP3R, 'SingDuetP3RStatic');
      ThemeLoadText(Sing.TextDuetP3R, 'SingDuetP3RText');
      ThemeLoadStatic(Sing.StaticDuetP3RScoreBG, 'SingDuetP3RStatic2');
      ThemeLoadText(Sing.TextDuetP3RScore, 'SingDuetP3RTextScore');
      ThemeLoadStatic(Sing.StaticDuetP3RAvatar, 'SingDuetP3RAvatar');

      ThemeLoadStatic(Sing.StaticDuetP1ThreePSingBar, 'SingDuetP1ThreePSingBar');
      ThemeLoadStatic(Sing.StaticDuetP2MSingBar, 'SingDuetP2MSingBar');
      ThemeLoadStatic(Sing.StaticDuetP3RSingBar, 'SingDuetP3RSingBar');

      //4P/6P mode in 1 Screen
      ThemeLoadStatic(Sing.StaticP1FourPSingBar, 'SingP1FourPSingBar');
      ThemeLoadStatic(Sing.StaticP1FourP, 'SingP1FourPStatic');
      ThemeLoadText(Sing.TextP1FourP, 'SingP1FourPText');
      ThemeLoadStatic(Sing.StaticP1FourPScoreBG, 'SingP1FourPStatic2');
      ThemeLoadText(Sing.TextP1FourPScore, 'SingP1FourPTextScore');
      ThemeLoadStatic(Sing.StaticP1FourPAvatar, 'SingP1FourPAvatar');

      ThemeLoadStatic(Sing.StaticP2FourPSingBar, 'SingP2FourPSingBar');
      ThemeLoadStatic(Sing.StaticP2FourP, 'SingP2FourPStatic');
      ThemeLoadText(Sing.TextP2FourP, 'SingP2FourPText');
      ThemeLoadStatic(Sing.StaticP2FourPScoreBG, 'SingP2FourPStatic2');
      ThemeLoadText(Sing.TextP2FourPScore, 'SingP2FourPTextScore');
      ThemeLoadStatic(Sing.StaticP2FourPAvatar, 'SingP2FourPAvatar');

      ThemeLoadStatic(Sing.StaticP3FourPSingBar, 'SingP3FourPSingBar');
      ThemeLoadStatic(Sing.StaticP3FourP, 'SingP3FourPStatic');
      ThemeLoadText(Sing.TextP3FourP, 'SingP3FourPText');
      ThemeLoadStatic(Sing.StaticP3FourPScoreBG, 'SingP3FourPStatic2');
      ThemeLoadText(Sing.TextP3FourPScore, 'SingP3FourPTextScore');
      ThemeLoadStatic(Sing.StaticP3FourPAvatar, 'SingP3FourPAvatar');

      ThemeLoadStatic(Sing.StaticP4FourPSingBar, 'SingP4FourPSingBar');
      ThemeLoadStatic(Sing.StaticP4FourP, 'SingP4FourPStatic');
      ThemeLoadText(Sing.TextP4FourP, 'SingP4FourPText');
      ThemeLoadStatic(Sing.StaticP4FourPScoreBG, 'SingP4FourPStatic2');
      ThemeLoadText(Sing.TextP4FourPScore, 'SingP4FourPTextScore');
      ThemeLoadStatic(Sing.StaticP4FourPAvatar, 'SingP4FourPAvatar');

      ThemeLoadStatic(Sing.StaticP1SixPSingBar, 'SingP1SixPSingBar');
      ThemeLoadStatic(Sing.StaticP1SixP, 'SingP1SixPStatic');
      ThemeLoadText(Sing.TextP1SixP, 'SingP1SixPText');
      ThemeLoadStatic(Sing.StaticP1SixPScoreBG, 'SingP1SixPStatic2');
      ThemeLoadText(Sing.TextP1SixPScore, 'SingP1SixPTextScore');
      ThemeLoadStatic(Sing.StaticP1SixPAvatar, 'SingP1SixPAvatar');

      ThemeLoadStatic(Sing.StaticP2SixPSingBar, 'SingP2SixPSingBar');
      ThemeLoadStatic(Sing.StaticP2SixP, 'SingP2SixPStatic');
      ThemeLoadText(Sing.TextP2SixP, 'SingP2SixPText');
      ThemeLoadStatic(Sing.StaticP2SixPScoreBG, 'SingP2SixPStatic2');
      ThemeLoadText(Sing.TextP2SixPScore, 'SingP2SixPTextScore');
      ThemeLoadStatic(Sing.StaticP2SixPAvatar, 'SingP2SixPAvatar');

      ThemeLoadStatic(Sing.StaticP3SixPSingBar, 'SingP3SixPSingBar');
      ThemeLoadStatic(Sing.StaticP3SixP, 'SingP3SixPStatic');
      ThemeLoadText(Sing.TextP3SixP, 'SingP3SixPText');
      ThemeLoadStatic(Sing.StaticP3SixPScoreBG, 'SingP3SixPStatic2');
      ThemeLoadText(Sing.TextP3SixPScore, 'SingP3SixPTextScore');
      ThemeLoadStatic(Sing.StaticP3SixPAvatar, 'SingP3SixPAvatar');

      ThemeLoadStatic(Sing.StaticP4SixPSingBar, 'SingP4SixPSingBar');
      ThemeLoadStatic(Sing.StaticP4SixP, 'SingP4SixPStatic');
      ThemeLoadText(Sing.TextP4SixP, 'SingP4SixPText');
      ThemeLoadStatic(Sing.StaticP4SixPScoreBG, 'SingP4SixPStatic2');
      ThemeLoadText(Sing.TextP4SixPScore, 'SingP4SixPTextScore');
      ThemeLoadStatic(Sing.StaticP4SixPAvatar, 'SingP4SixPAvatar');

      ThemeLoadStatic(Sing.StaticP5SixPSingBar, 'SingP5SixPSingBar');
      ThemeLoadStatic(Sing.StaticP5SixP, 'SingP5SixPStatic');
      ThemeLoadText(Sing.TextP5SixP, 'SingP5SixPText');
      ThemeLoadStatic(Sing.StaticP5SixPScoreBG, 'SingP5SixPStatic2');
      ThemeLoadText(Sing.TextP5SixPScore, 'SingP5SixPTextScore');
      ThemeLoadStatic(Sing.StaticP5SixPAvatar, 'SingP5SixPAvatar');

      ThemeLoadStatic(Sing.StaticP6SixPSingBar, 'SingP6SixPSingBar');
      ThemeLoadStatic(Sing.StaticP6SixP, 'SingP6SixPStatic');
      ThemeLoadText(Sing.TextP6SixP, 'SingP6SixPText');
      ThemeLoadStatic(Sing.StaticP6SixPScoreBG, 'SingP6SixPStatic2');
      ThemeLoadText(Sing.TextP6SixPScore, 'SingP6SixPTextScore');
      ThemeLoadStatic(Sing.StaticP6SixPAvatar, 'SingP6SixPAvatar');

      // duet 4/6 players in one screen
      ThemeLoadStatic(Sing.StaticP1DuetFourPSingBar, 'SingP1DuetFourPSingBar');
      ThemeLoadStatic(Sing.StaticP1DuetFourP, 'SingP1DuetFourPStatic');
      ThemeLoadText(Sing.TextP1DuetFourP, 'SingP1DuetFourPText');
      ThemeLoadStatic(Sing.StaticP1DuetFourPScoreBG, 'SingP1DuetFourPStatic2');
      ThemeLoadText(Sing.TextP1DuetFourPScore, 'SingP1DuetFourPTextScore');
      ThemeLoadStatic(Sing.StaticP1DuetFourPAvatar, 'SingP1DuetFourPAvatar');

      ThemeLoadStatic(Sing.StaticP2DuetFourPSingBar, 'SingP2DuetFourPSingBar');
      ThemeLoadStatic(Sing.StaticP2DuetFourP, 'SingP2DuetFourPStatic');
      ThemeLoadText(Sing.TextP2DuetFourP, 'SingP2DuetFourPText');
      ThemeLoadStatic(Sing.StaticP2DuetFourPScoreBG, 'SingP2DuetFourPStatic2');
      ThemeLoadText(Sing.TextP2DuetFourPScore, 'SingP2DuetFourPTextScore');
      ThemeLoadStatic(Sing.StaticP2DuetFourPAvatar, 'SingP2DuetFourPAvatar');

      ThemeLoadStatic(Sing.StaticP3DuetFourPSingBar, 'SingP3DuetFourPSingBar');
      ThemeLoadStatic(Sing.StaticP3DuetFourP, 'SingP3DuetFourPStatic');
      ThemeLoadText(Sing.TextP3DuetFourP, 'SingP3DuetFourPText');
      ThemeLoadStatic(Sing.StaticP3DuetFourPScoreBG, 'SingP3DuetFourPStatic2');
      ThemeLoadText(Sing.TextP3DuetFourPScore, 'SingP3DuetFourPTextScore');
      ThemeLoadStatic(Sing.StaticP3DuetFourPAvatar, 'SingP3DuetFourPAvatar');

      ThemeLoadStatic(Sing.StaticP4DuetFourPSingBar, 'SingP4DuetFourPSingBar');
      ThemeLoadStatic(Sing.StaticP4DuetFourP, 'SingP4DuetFourPStatic');
      ThemeLoadText(Sing.TextP4DuetFourP, 'SingP4DuetFourPText');
      ThemeLoadStatic(Sing.StaticP4DuetFourPScoreBG, 'SingP4DuetFourPStatic2');
      ThemeLoadText(Sing.TextP4DuetFourPScore, 'SingP4DuetFourPTextScore');
      ThemeLoadStatic(Sing.StaticP4DuetFourPAvatar, 'SingP4DuetFourPAvatar');


      ThemeLoadStatic(Sing.StaticP1DuetSixPSingBar, 'SingP1DuetSixPSingBar');
      ThemeLoadStatic(Sing.StaticP1DuetSixP, 'SingP1DuetSixPStatic');
      ThemeLoadText(Sing.TextP1DuetSixP, 'SingP1DuetSixPText');
      ThemeLoadStatic(Sing.StaticP1DuetSixPScoreBG, 'SingP1DuetSixPStatic2');
      ThemeLoadText(Sing.TextP1DuetSixPScore, 'SingP1DuetSixPTextScore');
      ThemeLoadStatic(Sing.StaticP1DuetSixPAvatar, 'SingP1DuetSixPAvatar');

      ThemeLoadStatic(Sing.StaticP2DuetSixPSingBar, 'SingP2DuetSixPSingBar');
      ThemeLoadStatic(Sing.StaticP2DuetSixP, 'SingP2DuetSixPStatic');
      ThemeLoadText(Sing.TextP2DuetSixP, 'SingP2DuetSixPText');
      ThemeLoadStatic(Sing.StaticP2DuetSixPScoreBG, 'SingP2DuetSixPStatic2');
      ThemeLoadText(Sing.TextP2DuetSixPScore, 'SingP2DuetSixPTextScore');
      ThemeLoadStatic(Sing.StaticP2DuetSixPAvatar, 'SingP2DuetSixPAvatar');

      ThemeLoadStatic(Sing.StaticP3DuetSixPSingBar, 'SingP3DuetSixPSingBar');
      ThemeLoadStatic(Sing.StaticP3DuetSixP, 'SingP3DuetSixPStatic');
      ThemeLoadText(Sing.TextP3DuetSixP, 'SingP3DuetSixPText');
      ThemeLoadStatic(Sing.StaticP3DuetSixPScoreBG, 'SingP3DuetSixPStatic2');
      ThemeLoadText(Sing.TextP3DuetSixPScore, 'SingP3DuetSixPTextScore');
      ThemeLoadStatic(Sing.StaticP3DuetSixPAvatar, 'SingP3DuetSixPAvatar');

      ThemeLoadStatic(Sing.StaticP4DuetSixPSingBar, 'SingP4DuetSixPSingBar');
      ThemeLoadStatic(Sing.StaticP4DuetSixP, 'SingP4DuetSixPStatic');
      ThemeLoadText(Sing.TextP4DuetSixP, 'SingP4DuetSixPText');
      ThemeLoadStatic(Sing.StaticP4DuetSixPScoreBG, 'SingP4DuetSixPStatic2');
      ThemeLoadText(Sing.TextP4DuetSixPScore, 'SingP4DuetSixPTextScore');
      ThemeLoadStatic(Sing.StaticP4DuetSixPAvatar, 'SingP4DuetSixPAvatar');

      ThemeLoadStatic(Sing.StaticP5DuetSixPSingBar, 'SingP5DuetSixPSingBar');
      ThemeLoadStatic(Sing.StaticP5DuetSixP, 'SingP5DuetSixPStatic');
      ThemeLoadText(Sing.TextP5DuetSixP, 'SingP5DuetSixPText');
      ThemeLoadStatic(Sing.StaticP5DuetSixPScoreBG, 'SingP5DuetSixPStatic2');
      ThemeLoadText(Sing.TextP5DuetSixPScore, 'SingP5DuetSixPTextScore');
      ThemeLoadStatic(Sing.StaticP5DuetSixPAvatar, 'SingP5DuetSixPAvatar');

      ThemeLoadStatic(Sing.StaticP6DuetSixPSingBar, 'SingP6DuetSixPSingBar');
      ThemeLoadStatic(Sing.StaticP6DuetSixP, 'SingP6DuetSixPStatic');
      ThemeLoadText(Sing.TextP6DuetSixP, 'SingP6DuetSixPText');
      ThemeLoadStatic(Sing.StaticP6DuetSixPScoreBG, 'SingP6DuetSixPStatic2');
      ThemeLoadText(Sing.TextP6DuetSixPScore, 'SingP6DuetSixPTextScore');
      ThemeLoadStatic(Sing.StaticP6DuetSixPAvatar, 'SingP6DuetSixPAvatar');

      // Oscilloscope Position
      ThemeLoadPosition(Sing.SingP1Oscilloscope, 'SingP1Oscilloscope');
      ThemeLoadPosition(Sing.SingP1TwoPOscilloscope, 'SingP1TwoPOscilloscope');
      ThemeLoadPosition(Sing.SingP2ROscilloscope, 'SingP2ROscilloscope');
      ThemeLoadPosition(Sing.SingP1ThreePOscilloscope, 'SingP1ThreePOscilloscope');
      ThemeLoadPosition(Sing.SingP2MOscilloscope, 'SingP2MOscilloscope');
      ThemeLoadPosition(Sing.SingP3ROscilloscope, 'SingP3ROscilloscope');
      ThemeLoadPosition(Sing.SingDuetP1ThreePOscilloscope, 'SingDuetP1ThreePOscilloscope');
      ThemeLoadPosition(Sing.SingDuetP2MOscilloscope, 'SingDuetP2MOscilloscope');
      ThemeLoadPosition(Sing.SingDuetP3ROscilloscope, 'SingDuetP3ROscilloscope');
      ThemeLoadPosition(Sing.SingP1FourPOscilloscope, 'SingP1FourPOscilloscope');
      ThemeLoadPosition(Sing.SingP2FourPOscilloscope, 'SingP2FourPOscilloscope');
      ThemeLoadPosition(Sing.SingP3FourPOscilloscope, 'SingP3FourPOscilloscope');
      ThemeLoadPosition(Sing.SingP4FourPOscilloscope, 'SingP4FourPOscilloscope');
      ThemeLoadPosition(Sing.SingP1SixPOscilloscope, 'SingP1SixPOscilloscope');
      ThemeLoadPosition(Sing.SingP2SixPOscilloscope, 'SingP2SixPOscilloscope');
      ThemeLoadPosition(Sing.SingP3SixPOscilloscope, 'SingP3SixPOscilloscope');
      ThemeLoadPosition(Sing.SingP4SixPOscilloscope, 'SingP4SixPOscilloscope');
      ThemeLoadPosition(Sing.SingP5SixPOscilloscope, 'SingP5SixPOscilloscope');
      ThemeLoadPosition(Sing.SingP6SixPOscilloscope, 'SingP6SixPOscilloscope');
      ThemeLoadPosition(Sing.SingP1DuetFourPOscilloscope, 'SingP1DuetFourPOscilloscope');
      ThemeLoadPosition(Sing.SingP2DuetFourPOscilloscope, 'SingP2DuetFourPOscilloscope');
      ThemeLoadPosition(Sing.SingP3DuetFourPOscilloscope, 'SingP3DuetFourPOscilloscope');
      ThemeLoadPosition(Sing.SingP4DuetFourPOscilloscope, 'SingP4DuetFourPOscilloscope');
      ThemeLoadPosition(Sing.SingP1DuetSixPOscilloscope, 'SingP1DuetSixPOscilloscope');
      ThemeLoadPosition(Sing.SingP2DuetSixPOscilloscope, 'SingP2DuetSixPOscilloscope');
      ThemeLoadPosition(Sing.SingP3DuetSixPOscilloscope, 'SingP3DuetSixPOscilloscope');
      ThemeLoadPosition(Sing.SingP4DuetSixPOscilloscope, 'SingP4DuetSixPOscilloscope');
      ThemeLoadPosition(Sing.SingP5DuetSixPOscilloscope, 'SingP5DuetSixPOscilloscope');
      ThemeLoadPosition(Sing.SingP6DuetSixPOscilloscope, 'SingP6DuetSixPOscilloscope');

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

      // Send Button's
      for I := 1 to 3 do
         ThemeLoadButton(Score.ButtonSend[I], 'ScoreButtonSend' + IntToStr(I));

      ThemeLoadStatic(Score.StaticNavigate, 'ScoreStaticNavigate');
      ThemeLoadText(Score.TextNavigate, 'ScoreTextNavigate');

      // Top5
      ThemeLoadBasic(Top5, 'Top5');

      ThemeLoadText(Top5.TextLevel,       'Top5TextLevel');
      ThemeLoadText(Top5.TextArtistTitle, 'Top5TextArtistTitle');
      ThemeLoadStatics(Top5.StaticNumber, 'Top5StaticNumber');
      ThemeLoadTexts(Top5.TextNumber,     'Top5TextNumber');
      ThemeLoadTexts(Top5.TextName,       'Top5TextName');
      ThemeLoadTexts(Top5.TextScore,      'Top5TextScore');
      ThemeLoadTexts(Top5.TextDate,       'Top5TextDate');

      // Options
      ThemeLoadBasic(Options, 'Options');

      ThemeLoadButton(Options.ButtonGame,     'OptionsButtonGame');
      ThemeLoadButton(Options.ButtonGraphics, 'OptionsButtonGraphics');
      ThemeLoadButton(Options.ButtonSound,    'OptionsButtonSound');
      ThemeLoadButton(Options.ButtonLyrics,   'OptionsButtonLyrics');
      ThemeLoadButton(Options.ButtonThemes,   'OptionsButtonThemes');
      ThemeLoadButton(Options.ButtonRecord,   'OptionsButtonRecord');
      ThemeLoadButton(Options.ButtonAdvanced, 'OptionsButtonAdvanced');
      ThemeLoadButton(Options.ButtonNetwork,  'OptionsButtonNetwork');
      ThemeLoadButton(Options.ButtonWebcam,   'OptionsButtonWebcam');
      ThemeLoadButton(Options.ButtonJukebox,  'OptionsButtonJukebox');
      ThemeLoadButton(Options.ButtonExit,     'OptionsButtonExit');

      Options.Description[0] := Language.Translate('SING_OPTIONS_GAME_DESC');
      Options.Description[1] := Language.Translate('SING_OPTIONS_GRAPHICS_DESC');
      Options.Description[2] := Language.Translate('SING_OPTIONS_SOUND_DESC');
      Options.Description[3] := Language.Translate('SING_OPTIONS_LYRICS_DESC');
      Options.Description[4] := Language.Translate('SING_OPTIONS_THEMES_DESC');
      Options.Description[5] := Language.Translate('SING_OPTIONS_RECORD_DESC');
      Options.Description[6] := Language.Translate('SING_OPTIONS_ADVANCED_DESC');
      Options.Description[7] := Language.Translate('SING_OPTIONS_NETWORK_DESC');
      Options.Description[8] := Language.Translate('SING_OPTIONS_WEBCAM_DESC');
      Options.Description[9] := Language.Translate('SING_OPTIONS_JUKEBOX_DESC');
      Options.Description[10] := Language.Translate('SING_OPTIONS_EXIT');

      ThemeLoadText(Options.TextDescription, 'OptionsTextDescription');
      Options.TextDescription.Text := Options.Description[0];

      // Options Game
      ThemeLoadBasic(OptionsGame, 'OptionsGame');

      ThemeLoadSelectSlide(OptionsGame.SelectLanguage,   'OptionsGameSelectSlideLanguage');
      ThemeLoadSelectSlide(OptionsGame.SelectSongMenu,   'OptionsGameSelectSongMenu');
      ThemeLoadSelectSlide(OptionsGame.SelectSorting,    'OptionsGameSelectSlideSorting');
      ThemeLoadSelectSlide(OptionsGame.SelectTabs,       'OptionsGameSelectTabs');
      ThemeLoadSelectSlide(OptionsGame.SelectShowScores, 'OptionsGameSelectShowScores');
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
      //ThemeLoadSelectSlide(OptionsSound.SelectMicBoost,              'OptionsSoundSelectMicBoost');
      ThemeLoadSelectSlide(OptionsSound.SelectClickAssist,           'OptionsSoundSelectClickAssist');
      ThemeLoadSelectSlide(OptionsSound.SelectBeatClick,             'OptionsSoundSelectBeatClick');
      //ThemeLoadSelectSlide(OptionsSound.SelectThreshold,             'OptionsSoundSelectThreshold');
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
      ThemeLoadSelectSlide(OptionsRecord.SelectThreshold,     'OptionsSoundSelectThreshold'); //basisbit TODO
      ThemeLoadSelectSlide(OptionsRecord.SelectMicBoost,      'OptionsSoundSelectMicBoost');
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
      ThemeLoadSelectSlide(OptionsAdvanced.SelectSingScores,    'OptionsAdvancedSelectSingScores');
      ThemeLoadSelectSlide(OptionsAdvanced.SelectTopScores,     'OptionsAdvancedSelectTopScores');
      ThemeLoadButton     (OptionsAdvanced.ButtonExit,          'OptionsAdvancedButtonExit');

      //Options Network
      ThemeLoadBasic(OptionsNetwork, 'OptionsNetwork');

      ThemeLoadSelectSlide(OptionsNetwork.SelectWebsite,       'OptionsNetworkSelectWebsite');
      ThemeLoadSelectSlide(OptionsNetwork.SelectUsername,      'OptionsNetworkSelectUsername');
      ThemeLoadSelectSlide(OptionsNetwork.SelectSendName,      'OptionsNetworkSelectSendSaveName');
      ThemeLoadSelectSlide(OptionsNetwork.SelectAutoMode,      'OptionsNetworkSelectAutoMode');
      ThemeLoadSelectSlide(OptionsNetwork.SelectAutoPlayer,      'OptionsNetworkSelectAutoPlayer');
      ThemeLoadSelectSlide(OptionsNetwork.SelectAutoScoreEasy,   'OptionsNetworkSelectAutoScoreEasy');
      ThemeLoadSelectSlide(OptionsNetwork.SelectAutoScoreMedium, 'OptionsNetworkSelectAutoScoreMedium');
      ThemeLoadSelectSlide(OptionsNetwork.SelectAutoScoreHard,   'OptionsNetworkSelectAutoScoreHard');
      ThemeLoadText(OptionsNetwork.TextInsertUser, 'OptionsNetworkTextInsertUser');

      ThemeLoadButton(OptionsNetwork.ButtonInsert,          'OptionsNetworkButtonInsert');
      ThemeLoadButton(OptionsNetwork.ButtonExit,          'OptionsNetworkButtonExit');

      //Options Webcam
      ThemeLoadBasic(OptionsWebcam, 'OptionsWebcam');

      ThemeLoadSelectSlide(OptionsWebcam.SelectWebcam,     'OptionsWebcamSelectWebcam');
      ThemeLoadSelectSlide(OptionsWebcam.SelectResolution, 'OptionsWebcamSelectResolution');
      ThemeLoadSelectSlide(OptionsWebcam.SelectFPS,        'OptionsWebcamSelectFPS');
      ThemeLoadSelectSlide(OptionsWebcam.SelectFlip,       'OptionsWebcamSelectFlip');
      ThemeLoadSelectSlide(OptionsWebcam.SelectBrightness, 'OptionsWebcamSelectBrightness');
      ThemeLoadSelectSlide(OptionsWebcam.SelectSaturation, 'OptionsWebcamSelectSaturation');
      ThemeLoadSelectSlide(OptionsWebcam.SelectHue,        'OptionsWebcamSelectHue');
      ThemeLoadSelectSlide(OptionsWebcam.SelectEffect,     'OptionsWebcamSelectEffect');

      ThemeLoadButton(OptionsWebcam.ButtonPreVisualization,          'OptionsWebcamButtonPreVisualization');
      ThemeLoadButton(OptionsWebcam.ButtonExit,          'OptionsWebcamButtonExit');

      // Options Jukebox
      ThemeLoadBasic(OptionsJukebox, 'OptionsJukebox');

      ThemeLoadSelectSlide(OptionsJukebox.SelectLyricsFont,   'OptionsJukeboxSelectLyricsFont');
      ThemeLoadSelectSlide(OptionsJukebox.SelectLyricsEffect, 'OptionsJukeboxSelectLyricsEffect');
      ThemeLoadSelectSlide(OptionsJukebox.SelectLyricsAlpha,   'OptionsJukeboxSelectLyricsAlpha');
      ThemeLoadSelectSlide(OptionsJukebox.SelectLine,         'OptionsJukeboxSelectLine');
      ThemeLoadSelectSlide(OptionsJukebox.SelectProperty,    'OptionsJukeboxSelectProperty');
      ThemeLoadSelectSlide(OptionsJukebox.SelectColor,        'OptionsJukeboxSelectColor');
      ThemeLoadSelectSlide(OptionsJukebox.SelectR,    'OptionsJukeboxSelectR');
      ThemeLoadSelectSlide(OptionsJukebox.SelectG,    'OptionsJukeboxSelectG');
      ThemeLoadSelectSlide(OptionsJukebox.SelectB,    'OptionsJukeboxSelectB');
      ThemeLoadStatic(OptionsJukebox.PointerR,        'OptionsJukeboxPointerR');
      ThemeLoadStatic(OptionsJukebox.PointerG,        'OptionsJukeboxPointerG');
      ThemeLoadStatic(OptionsJukebox.PointerB,        'OptionsJukeboxPointerB');
      ThemeLoadStatic(OptionsJukebox.TexR,            'OptionsJukeboxRed');
      ThemeLoadStatic(OptionsJukebox.TexG,            'OptionsJukeboxGreen');
      ThemeLoadStatic(OptionsJukebox.TexB,            'OptionsJukeboxBlue');
      ThemeLoadStatic(OptionsJukebox.TexColor,        'OptionsJukeboxColor');

      OptionsJukebox.UpperX := ThemeIni.ReadInteger('OptionsJukeboxUpperBar', 'X', 0);
      OptionsJukebox.UpperW := ThemeIni.ReadInteger('OptionsJukeboxUpperBar', 'W', 0);
      OptionsJukebox.UpperY := ThemeIni.ReadInteger('OptionsJukeboxUpperBar', 'Y', 0);
      OptionsJukebox.UpperH := ThemeIni.ReadInteger('OptionsJukeboxUpperBar', 'H', 0);
      OptionsJukebox.LowerX := ThemeIni.ReadInteger('OptionsJukeboxLowerBar', 'X', 0);
      OptionsJukebox.LowerW := ThemeIni.ReadInteger('OptionsJukeboxLowerBar', 'W', 0);
      OptionsJukebox.LowerY := ThemeIni.ReadInteger('OptionsJukeboxLowerBar', 'Y', 0);
      OptionsJukebox.LowerH := ThemeIni.ReadInteger('OptionsJukeboxLowerBar', 'H', 0);

      ThemeLoadButton(OptionsJukebox.ButtonExit,              'OptionsJukeboxButtonExit');

      //Edit Menu
      ThemeLoadBasic (Edit,               'Edit');

      ThemeLoadButton(Edit.ButtonConvert, 'EditButtonConvert');
      ThemeLoadButton(Edit.ButtonExit,    'EditButtonExit');

      Edit.Description[0] := Language.Translate('SING_EDIT_BUTTON_DESCRIPTION_CONVERT');
      Edit.Description[1] := Language.Translate('SING_EDIT_BUTTON_DESCRIPTION_EXIT');

      ThemeLoadText(Edit.TextDescription, 'EditTextDescription');
      Edit.TextDescription.Text := Edit.Description[0];

      // editor
      ThemeLoadBasic (EditSub,               'EditSub');
      ThemeLoadStatic(EditSub.BackgroundImage, 'EditSubBackgroundImage');
      // current position in editor
      ThemeLoadButton(EditSub.ButtonCurrentLine, 'EditSubButtonCurrentLine');
      ThemeLoadButton(EditSub.ButtonCurrentNote, 'EditSubButtonCurrentNote');
      ThemeLoadButton(EditSub.PlayOnly,    'EditSubBarStatic1');
      ThemeLoadButton(EditSub.PlayWithNote,    'EditSubBarStatic2');
      ThemeLoadButton(EditSub.PlayNote,    'EditSubBarStatic3');
      ThemeLoadButton(EditSub.previousseq,    'EditSubBarStatic4');
      ThemeLoadButton(EditSub.nextseq,    'EditSubBarStatic5');
      ThemeLoadButton(EditSub.gold,    'EditSubBarStatic6');
      ThemeLoadButton(EditSub.freestyle,    'EditSubBarStatic7');
      ThemeLoadButton(EditSub.undo,    'EditSubBarStatic8');
      ThemeLoadSelectSlide(EditSub.SlideTitle, 'EditSubTitle');
      ThemeLoadSelectSlide(EditSub.SlideArtist, 'EditSubArtist');
      ThemeLoadSelectSlide(EditSub.SlideMP3, 'EditSubMP3');
      ThemeLoadSelectSlide(EditSub.SlideCover, 'EditSubSlideCover');
      ThemeLoadSelectSlide(EditSub.SlideBackground, 'EditSubSlideBG');
      ThemeLoadSelectSlide(EditSub.SlideBPM, 'EditSubBPM');
      ThemeLoadSelectSlide(EditSub.SlideGAP, 'EditSubGAP');
      ThemeLoadSelectSlide(EditSub.SlideStart, 'EditSubStart');
      ThemeLoadSelectSlide(EditSub.SlideDuration, 'EditSubDuration');
      ThemeLoadSelectSlide(EditSub.SlideTone, 'EditSubTone');
      ThemeLoadSelectSlide(EditSub.SlideLyric, 'EditSubLyric');
      ThemeLoadSelectSlide(EditSub.SelectVolAudio, 'EditSubSelectVolAudio');
      ThemeLoadSelectSlide(EditSub.SelectVolMidi, 'EditSubSelectVolMidi');
      ThemeLoadSelectSlide(EditSub.SelectVolClick, 'EditSubSelectVolClick');
      ThemeLoadSelectSlide(EditSub.SlideVideoGap, 'EditSubVideoGap');

      //error popup
      ThemeLoadBasic (ErrorPopup, 'ErrorPopup');
      ThemeLoadButton(ErrorPopup.Button1, 'ErrorPopupButton1');
      ThemeLoadText  (ErrorPopup.TextError,'ErrorPopupText');

      //check popup
      ThemeLoadBasic (CheckPopup, 'CheckPopup');
      ThemeLoadButton(CheckPopup.Button1, 'CheckPopupButton1');
      ThemeLoadButton(CheckPopup.Button2, 'CheckPopupButton2');
      ThemeLoadText(CheckPopup.TextCheck , 'CheckPopupText');

      // insert user popup
      ThemeLoadBasic (InsertUserPopup, 'InsertUserPopup');
      ThemeLoadText  (InsertUserPopup.TextInsertUser , 'InsertUserPopupText');
      ThemeLoadButton(InsertUserPopup.ButtonUsername, 'InsertUserPopupButtonUsername');
      ThemeLoadButton(InsertUserPopup.ButtonPassword, 'InsertUserPopupButtonPassword');
      ThemeLoadButton(InsertUserPopup.Button1, 'InsertUserPopupButton1');
      ThemeLoadButton(InsertUserPopup.Button2, 'InsertUserPopupButton2');

      // send score popup
      ThemeLoadBasic (SendScorePopup, 'SendScorePopup');
      ThemeLoadSelectSlide(SendScorePopup.SelectSlide1, 'SendScorePopupSelectSlide1');
      ThemeLoadSelectSlide(SendScorePopup.SelectSlide2, 'SendScorePopupSelectSlide2');
      ThemeLoadSelectSlide(SendScorePopup.SelectSlide3, 'SendScorePopupSelectSlide3');
      ThemeLoadButton(SendScorePopup.ButtonUsername, 'SendScorePopupButtonUsername');
      ThemeLoadButton(SendScorePopup.ButtonPassword, 'SendScorePopupButtonPassword');
      ThemeLoadButton(SendScorePopup.Button1, 'SendScorePopupButton1');
      ThemeLoadButton(SendScorePopup.Button2, 'SendScorePopupButton2');

      // download score popup
      ThemeLoadBasic (ScoreDownloadPopup, 'ScoreDownloadPopup');
      ThemeLoadButton(ScoreDownloadPopup.Button1, 'ScoreDownloadPopupButton1');
      ThemeLoadText(ScoreDownloadPopup.TextSongScoreDownload , 'ScoreDownloadPopupSongText');
      ThemeLoadText(ScoreDownloadPopup.TextWebScoreDownload , 'ScoreDownloadPopupWebText');
      ThemeLoadStatic(ScoreDownloadPopup.DownloadProgressSong, 'ScoreDownloadPopupProgressSong');
      ThemeLoadStatic(ScoreDownloadPopup.DownloadProgressWeb, 'ScoreDownloadPopupProgressWeb');

      //Song Menu
      ThemeLoadBasic (SongMenu, 'SongMenu');
      ThemeLoadButton(SongMenu.Button1, 'SongMenuButton1');
      ThemeLoadButton(SongMenu.Button2, 'SongMenuButton2');
      ThemeLoadButton(SongMenu.Button3, 'SongMenuButton3');
      ThemeLoadButton(SongMenu.Button4, 'SongMenuButton4');
      ThemeLoadButton(SongMenu.Button5, 'SongMenuButton5');
      ThemeLoadSelectSlide(SongMenu.SelectSlide1, 'SongMenuSelectSlide1');
      ThemeLoadSelectSlide(SongMenu.SelectSlide2, 'SongMenuSelectSlide2');
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

      //Party Options
      ThemeLoadBasic(PartyOptions, 'PartyOptions');
      ThemeLoadSelectSlide(PartyOptions.SelectMode, 'PartyOptionsSelectMode');
      ThemeLoadSelectSlide(PartyOptions.SelectLevel, 'PartyOptionsSelectLevel');
      ThemeLoadSelectSlide(PartyOptions.SelectPlayList, 'PartyOptionsSelectPlayList');
      ThemeLoadSelectSlide(PartyOptions.SelectPlayList2, 'PartyOptionsSelectPlayList2');
      {ThemeLoadButton (ButtonNext, 'ButtonNext');
      ThemeLoadButton (ButtonPrev, 'ButtonPrev');}

      //Party Player
      ThemeLoadBasic(PartyPlayer, 'PartyPlayer');

      ThemeLoadSelectSlide(PartyPlayer.SelectTeams, 'PartyPlayerSelectTeams');
      ThemeLoadSelectSlide(PartyPlayer.SelectPlayers1, 'PartyPlayerSelectPlayers1');
      ThemeLoadSelectSlide(PartyPlayer.SelectPlayers2, 'PartyPlayerSelectPlayers2');
      ThemeLoadSelectSlide(PartyPlayer.SelectPlayers3, 'PartyPlayerSelectPlayers3');

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

      // Party Rounds
      ThemeLoadBasic(PartyRounds, 'PartyRounds');

      ThemeLoadSelectSlide(PartyRounds.SelectRoundCount, 'PartyRoundsSelectRoundCount');
      for I := 0 to High(PartyRounds.SelectRound) do
        ThemeLoadSelectSlide(PartyRounds.SelectRound[I], 'PartyRoundsSelectRound' + IntToStr(I + 1));

      {ThemeLoadButton(ButtonNext, 'PartyPlayerButtonNext');
      ThemeLoadButton(ButtonPrev, 'PartyPlayerButtonPrev');}

      //Party Tournament Player
      ThemeLoadBasic(PartyTournamentPlayer, 'PartyTournamentPlayer');

      ThemeLoadSelectSlide(PartyTournamentPlayer.SelectPlayers, 'PartyTournamentPlayerSelectPlayers');

      ThemeLoadButton(PartyTournamentPlayer.Player1Name, 'PartyTournamentPlayerPlayer1Name');
      ThemeLoadButton(PartyTournamentPlayer.Player2Name, 'PartyTournamentPlayerPlayer2Name');
      ThemeLoadButton(PartyTournamentPlayer.Player3Name, 'PartyTournamentPlayerPlayer3Name');
      ThemeLoadButton(PartyTournamentPlayer.Player4Name, 'PartyTournamentPlayerPlayer4Name');
      ThemeLoadButton(PartyTournamentPlayer.Player5Name, 'PartyTournamentPlayerPlayer5Name');
      ThemeLoadButton(PartyTournamentPlayer.Player6Name, 'PartyTournamentPlayerPlayer6Name');
      ThemeLoadButton(PartyTournamentPlayer.Player7Name, 'PartyTournamentPlayerPlayer7Name');
      ThemeLoadButton(PartyTournamentPlayer.Player8Name, 'PartyTournamentPlayerPlayer8Name');
      ThemeLoadButton(PartyTournamentPlayer.Player9Name, 'PartyTournamentPlayerPlayer9Name');
      ThemeLoadButton(PartyTournamentPlayer.Player10Name, 'PartyTournamentPlayerPlayer10Name');
      ThemeLoadButton(PartyTournamentPlayer.Player11Name, 'PartyTournamentPlayerPlayer11Name');
      ThemeLoadButton(PartyTournamentPlayer.Player12Name, 'PartyTournamentPlayerPlayer12Name');
      ThemeLoadButton(PartyTournamentPlayer.Player13Name, 'PartyTournamentPlayerPlayer13Name');
      ThemeLoadButton(PartyTournamentPlayer.Player14Name, 'PartyTournamentPlayerPlayer14Name');
      ThemeLoadButton(PartyTournamentPlayer.Player15Name, 'PartyTournamentPlayerPlayer15Name');
      ThemeLoadButton(PartyTournamentPlayer.Player16Name, 'PartyTournamentPlayerPlayer16Name');

      //Party Tournament Options
      ThemeLoadBasic(PartyTournamentOptions, 'PartyTournamentOptions');
      ThemeLoadSelectSlide(PartyTournamentOptions.SelectRoundsFinal, 'PartyTournamentOptionsSelectRoundsFinal');
      ThemeLoadSelectSlide(PartyTournamentOptions.SelectRounds2Final, 'PartyTournamentOptionsSelectRounds2Final');
      ThemeLoadSelectSlide(PartyTournamentOptions.SelectRounds4Final, 'PartyTournamentOptionsSelectRounds4Final');
      ThemeLoadSelectSlide(PartyTournamentOptions.SelectRounds8Final, 'PartyTournamentOptionsSelectRounds8Final');

      //Party Tournament Rounds

      ThemeLoadBasic(PartyTournamentRounds, 'PartyTournamentRounds');

      for I := 0 to 1 do
      begin
        for J := 0 to 7 do
        begin
          ThemeLoadButton (PartyTournamentRounds.TextNamePlayer[I, J], 'PartyTournamentRoundsTextNameBlock' + IntToStr(I + 1) + 'Player' + IntToStr(J + 1));
        end;
      end;

      ThemeLoadText(PartyTournamentRounds.TextWinner, 'PartyTournamentRoundsWinner');
      ThemeLoadText(PartyTournamentRounds.TextResult, 'PartyTournamentRoundsResult');

      ThemeLoadText(PartyTournamentRounds.NextPlayers, 'PartyTournamentRoundsNextPlayers');

      //Party Tournament Win
      ThemeLoadBasic(PartyTournamentWin, 'PartyTournamentWin');

      ThemeLoadText (PartyTournamentWin.TextScorePlayer1,    'PartyTournamentWinTextScorePlayer1');
      ThemeLoadText (PartyTournamentWin.TextScorePlayer2,    'PartyTournamentWinTextScorePlayer2');

      ThemeLoadText (PartyTournamentWin.TextNamePlayer1,     'PartyTournamentWinTextNamePlayer1');
      ThemeLoadText (PartyTournamentWin.TextNamePlayer2,     'PartyTournamentWinTextNamePlayer2');

      ThemeLoadStatic (PartyTournamentWin.StaticBGPlayer1,   'PartyTournamentWinStaticBGPlayer1');
      ThemeLoadStatic (PartyTournamentWin.StaticBGPlayer2,   'PartyTournamentWinStaticBGPlayer2');

      // About
      ThemeLoadBasic(AboutMain, 'AboutMain');
      ThemeLoadButton(AboutMain.ButtonCredits, 'AboutMainButtonCredits');
      ThemeLoadButton(AboutMain.ButtonExit, 'AboutMainButtonExit');
      ThemeLoadText (AboutMain.TextOverview, 'AboutMainTextOverview');

      // Stats
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

      //Mode Translations
      //Fill IMode
      IMode[0] := Language.Translate('PARTY_MODE_CLASSIC');
      IMode[1] := Language.Translate('PARTY_MODE_CLASSIC_FREE');
      IMode[2] := Language.Translate('PARTY_MODE_CHALLENGE');
      IMode[3] := Language.Translate('PARTY_MODE_TOURNAMENT');
    end;

    ThemeIni.Free;
  end;
end;

procedure TTheme.ThemeLoadBasic(Theme: TThemeBasic; const Name: string);
begin
  ThemeLoadBackground(Theme.Background, Name);
  ThemeLoadTexts(Theme.Text, Name + 'Text');
  ThemeLoadStatics(Theme.Statics, Name + 'Static');
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

  ThemeText.Font  := ThemeIni.ReadInteger(Name, 'Font', ftNormal);
  ThemeText.Size  := ThemeIni.ReadInteger(Name, 'Size', 0);
  ThemeText.Align := ThemeIni.ReadInteger(Name, 'Align', 0);

  ThemeText.Text  := Language.Translate(ThemeIni.ReadString(Name, 'Text', ''));
  ThemeText.Color := ThemeIni.ReadString(Name, 'Color', '');
  ThemeText.DColor := ThemeIni.ReadString(Name, 'DColor', '');

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

  C := ColorExists(ThemeText.DColor);
  if C >= 0 then
  begin
    ThemeText.DColR := Color[C].RGB.R;
    ThemeText.DColG := Color[C].RGB.G;
    ThemeText.DColB := Color[C].RGB.B;
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
  ThemeStatic.Alpha := ThemeIni.ReadFloat(Name, 'Alpha', 1);
  if ThemeIni.ReadString(Name, 'Type', '') = '' then Log.LogError('no texture type for ' + Name + ' found.', 'TTheme.ThemeLoadStatic');
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
  ThemeSelectS.Typ := ParseTextureType(ThemeIni.ReadString(Name, 'Type', ''), TEXTURE_TYPE_PLAIN);
  ThemeSelectS.TexSBG := {Skin.SkinPath + }ThemeIni.ReadString(Name, 'TexSBG', '');
  ThemeSelectS.TypSBG := ParseTextureType(ThemeIni.ReadString(Name, 'TypeSBG', ''), TEXTURE_TYPE_PLAIN);

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

  ThemeSelectS.showArrows := (ThemeIni.ReadInteger(Name, 'ShowArrows', 0) = 1);
  ThemeSelectS.oneItemOnly := (ThemeIni.ReadInteger(Name, 'OneItemOnly', 0) = 1);
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

procedure TTheme.ThemeLoadPosition(var ThemePosition: TThemePosition; const Name: string);
begin
  ThemePosition.X := ThemeIni.ReadInteger(Name, 'X', 0);
  ThemePosition.Y := ThemeIni.ReadInteger(Name, 'Y', 0);
  ThemePosition.H := ThemeIni.ReadInteger(Name, 'H', 0);
  ThemePosition.W := ThemeIni.ReadInteger(Name, 'W', 0);
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

  LastC := C;

  // players colors
  SetLength(Color, Length(Color)+18);

  LoadPlayersColors;

  SL.Free;

end;

procedure LoadPlayersColors;
var
  C:      integer;
begin

  C := LastC;

  // P1
  C := C+1;
  Color[C].Name := 'P1Dark';
  Color[C].RGB := GetPlayerColor(Ini.PlayerColor[0]);

  C := C+1;
  Color[C].Name := 'P1Light';
  Color[C].RGB := GetPlayerLightColor(Ini.PlayerColor[0]);

  C := C+1;
  Color[C].Name := 'P1Lightest';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

  // P2
  C := C+1;
  Color[C].Name := 'P2Dark';
  Color[C].RGB := GetPlayerColor(Ini.PlayerColor[1]);

  C := C+1;
  Color[C].Name := 'P2Light';
  Color[C].RGB := GetPlayerLightColor(Ini.PlayerColor[1]);

  C := C+1;
  Color[C].Name := 'P2Lightest';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

  // P3
  C := C+1;
  Color[C].Name := 'P3Dark';
  Color[C].RGB := GetPlayerColor(Ini.PlayerColor[2]);

  C := C+1;
  Color[C].Name := 'P3Light';
  Color[C].RGB := GetPlayerLightColor(Ini.PlayerColor[2]);

  C := C+1;
  Color[C].Name := 'P3Lightest';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

  // P4
  C := C+1;
  Color[C].Name := 'P4Dark';
  Color[C].RGB := GetPlayerColor(Ini.PlayerColor[3]);

  C := C+1;
  Color[C].Name := 'P4Light';
  Color[C].RGB := GetPlayerLightColor(Ini.PlayerColor[3]);

  C := C+1;
  Color[C].Name := 'P4Lightest';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

  // P5
  C := C+1;
  Color[C].Name := 'P5Dark';
  Color[C].RGB := GetPlayerColor(Ini.PlayerColor[4]);

  C := C+1;
  Color[C].Name := 'P5Light';
  Color[C].RGB := GetPlayerLightColor(Ini.PlayerColor[4]);

  C := C+1;
  Color[C].Name := 'P5Lightest';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

  // P6
  C := C+1;
  Color[C].Name := 'P6Dark';
  Color[C].RGB := GetPlayerColor(Ini.PlayerColor[5]);

  C := C+1;
  Color[C].Name := 'P6Light';
  Color[C].RGB := GetPlayerLightColor(Ini.PlayerColor[5]);

  C := C+1;
  Color[C].Name := 'P6Lightest';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

end;

procedure LoadTeamsColors;
var
  C:      integer;
begin

  C := LastC;

  // P1
  C := C+1;
  Color[C].Name := 'P1Dark';
  Color[C].RGB := GetPlayerColor(Ini.TeamColor[0]);

  C := C+1;
  Color[C].Name := 'P1Light';
  Color[C].RGB := GetPlayerLightColor(Ini.TeamColor[0]);

  C := C+1;
  Color[C].Name := 'P1Lightest';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

  // P2
  C := C+1;
  Color[C].Name := 'P2Dark';
  Color[C].RGB := GetPlayerColor(Ini.TeamColor[1]);

  C := C+1;
  Color[C].Name := 'P2Light';
  Color[C].RGB := GetPlayerLightColor(Ini.TeamColor[1]);

  C := C+1;
  Color[C].Name := 'P2Lightest';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

  // P3
  C := C+1;
  Color[C].Name := 'P3Dark';
  Color[C].RGB := GetPlayerColor(Ini.TeamColor[2]);

  C := C+1;
  Color[C].Name := 'P3Light';
  Color[C].RGB := GetPlayerLightColor(Ini.TeamColor[2]);

  C := C+1;
  Color[C].Name := 'P3Lightest';
  Color[C].RGB := ColorSqrt(Color[C-1].RGB);

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
          Result.R := 212/255;
          Result.G := 71/255;
          Result.B := 247/255;
        end;
    5:  begin
          // orange
          Result.R := 247/255;
          Result.G := 144/255;
          Result.B := 71/255;
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

function GetPlayerColor(Color: integer): TRGB;
begin
  case (Color) of
    1://blue
    begin
      Result.R := 5/255;
      Result.G := 153/255;
      Result.B := 204/255;
    end;
    2: //red
    begin
      Result.R := 230/255;
      Result.G := 0;
      Result.B := 0;
    end;
    3: //green
    begin
      Result.R := 0;
      Result.G := 190/255;
      Result.B := 0;
    end;
    4: //yellow
    begin
      Result.R := 255/255;
      Result.G := 255/255;
      Result.B := 0;
    end;
    5: //orange
    begin
      Result.R := 255/255;
      Result.G := 127/255;
      Result.B := 0;
    end;
    6: //pink
    begin
      Result.R := 255/255;
      Result.G := 110/255;
      Result.B := 180/255;
    end;
    7: //purple
    begin
      Result.R := 175/255;
      Result.G := 0;
      Result.B := 210/255;
    end;
    8: //gold
    begin
      Result.R := 218/255;
      Result.G := 165/255;
      Result.B := 32/255;
    end;
    9: //gray
    begin
      Result.R := 150/255;
      Result.G := 150/255;
      Result.B := 150/255;
    end;
    10: //dark blue
    begin
      Result.R := 0;
      Result.G := 0;
      Result.B := 220/255;
    end;
    11: //sky
    begin
      Result.R := 0;
      Result.G := 110/255;
      Result.B := 210/255;
    end;
    12: //cyan
    begin
      Result.R := 0/255;
      Result.G := 215/255;
      Result.B := 215/255;
    end;
    13: //flame
    begin
      Result.R := 210/255;
      Result.G := 70/255;
      Result.B := 0/255;
    end;
    14: //orchid
    begin
      Result.R := 210/255;
      Result.G := 0;
      Result.B := 210/255;
    end;
    15: //harlequin
    begin
      Result.R := 110/255;
      Result.G := 210/255;
      Result.B := 0;
    end;
    16: //lime
    begin
      Result.R := 160/255;
      Result.G := 210/255;
      Result.B := 0;
    end;
  end;
end;

function GetPlayerLightColor(Color: integer): TRGB;
begin
  case (Color) of
    1://blue
    begin
      Result.R := 145/255;
      Result.G := 215/255;
      Result.B := 240/255;
    end;
    2: //red
    begin
      Result.R := 245/255;
      Result.G := 162/255;
      Result.B := 162/255;
    end;
    3: //green
    begin
      Result.R := 152/255;
      Result.G := 250/255;
      Result.B := 153/255;
    end;
    4: //yellow
    begin
      Result.R := 255/255;
      Result.G := 246/255;
      Result.B := 143/255;
    end;
    5: //orange
    begin
      Result.R := 255/255;
      Result.G := 204/255;
      Result.B := 156/255;
    end;
    6: //pink
    begin
      Result.R := 255/255;
      Result.G := 192/255;
      Result.B := 205/255;
    end;
    7: //purple
    begin
      Result.R := 240/255;
      Result.G := 170/255;
      Result.B := 255/255;
    end;
    8: //gold
    begin
      Result.R := 255/255;
      Result.G := 214/255;
      Result.B := 118/255;
    end;
    9: //gray
    begin
      Result.R := 220/255;
      Result.G := 220/255;
      Result.B := 220/255;
    end;
  end;
end;

function GetPlayerLightColorV2(Color: integer): TRGB;
begin
  case (Color) of
    1://blue
    begin
      Result.R := 145/255;
      Result.G := 215/255;
      Result.B := 240/255;
    end;
    2: //red
    begin
      Result.R := 245/255;
      Result.G := 162/255;
      Result.B := 162/255;
    end;
    3: //green
    begin
      Result.R := 152/255;
      Result.G := 250/255;
      Result.B := 153/255;
    end;
    4: //yellow
    begin
      Result.R := 255/255;
      Result.G := 246/255;
      Result.B := 143/255;
    end;
    5: //orange
    begin
      Result.R := 255/255;
      Result.G := 204/255;
      Result.B := 156/255;
    end;
    6: //pink
    begin
      Result.R := 255/255;
      Result.G := 192/255;
      Result.B := 205/255;
    end;
    7: //violet
    begin
      Result.R := 240/255;
      Result.G := 170/255;
      Result.B := 255/255;
    end;
    8: //gold
    begin
      Result.R := 255/255;
      Result.G := 214/255;
      Result.B := 118/255;
    end;
    9: //gray
    begin
      Result.R := 220/255;
      Result.G := 220/255;
      Result.B := 220/255;
    end;
    10: //dark blue
    begin
      Result.R := 90/255;
      Result.G := 90/255;
      Result.B := 255/255;
    end;
    11: //sky
    begin
      Result.R := 80/255;
      Result.G := 160/255;
      Result.B := 235/255;
    end;
    12: //cyan
    begin
      Result.R := 150/255;
      Result.G := 230/255;
      Result.B := 230/255;
    end;
    13: //flame
    begin
      Result.R := 230/255;
      Result.G := 130/255;
      Result.B := 80/255;
    end;
    14: //orchid
    begin
      Result.R := 230/255;
      Result.G := 100/255;
      Result.B := 230/255;
    end;
    15: //harlequin
    begin
      Result.R := 160/255;
      Result.G := 230/255;
      Result.B := 90/255;
    end;
    16: //lime
    begin
      Result.R := 190/255;
      Result.G := 230/255;
      Result.B := 100/255;
    end;
  end;
end;

function ColorSqrt(RGB: TRGB): TRGB;
begin
  Result.R := sqrt(RGB.R);
  Result.G := sqrt(RGB.G);
  Result.B := sqrt(RGB.B);
end;


function GetJukeboxLyricOtherColor(Line: integer): TRGB;
begin
  case Line of
    0: begin
         Result.R := Ini.JukeboxSingLineOtherColorR/255;
         Result.G := Ini.JukeboxSingLineOtherColorG/255;
         Result.B := Ini.JukeboxSingLineOtherColorB/255;
       end;
    1: begin
         Result.R := Ini.JukeboxActualLineOtherColorR/255;
         Result.G := Ini.JukeboxActualLineOtherColorG/255;
         Result.B := Ini.JukeboxActualLineOtherColorB/255;
       end;
    2: begin
         Result.R := Ini.JukeboxNextLineOtherColorR/255;
         Result.G := Ini.JukeboxNextLineOtherColorG/255;
         Result.B := Ini.JukeboxNextLineOtherColorB/255;
       end;
  end;
end;

function GetJukeboxLyricOtherOutlineColor(Line: integer): TRGB;
begin
  case Line of
    0: begin
         Result.R := Ini.JukeboxSingLineOtherOColorR/255;
         Result.G := Ini.JukeboxSingLineOtherOColorG/255;
         Result.B := Ini.JukeboxSingLineOtherOColorB/255;
       end;
    1: begin
         Result.R := Ini.JukeboxActualLineOtherOColorR/255;
         Result.G := Ini.JukeboxActualLineOtherOColorG/255;
         Result.B := Ini.JukeboxActualLineOtherOColorB/255;
       end;
    2: begin
         Result.R := Ini.JukeboxNextLineOtherOColorR/255;
         Result.G := Ini.JukeboxNextLineOtherOColorG/255;
         Result.B := Ini.JukeboxNextLineOtherOColorB/255;
       end;
  end;
end;

function GetLyricColor(Color: integer): TRGB;
begin
  case Color of
    0:  begin
          // blue
          Result.R := 0;
          Result.G := 150/255;
          Result.B := 255/255;
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
          Result.G := 63/255;
          Result.B := 192/255;{
          Result.G := 175/255;
          Result.B := 247/255; }
        end;
    3:  begin
          // red
          Result.R := 220/255;
          Result.G := 0;
          Result.B := 0;
        end;
        //'Violet', 'Orange', 'Yellow', 'Brown', 'Black'
        //New Theme-Color Patch
    4:  begin
          // violet
          Result.R := 180/255;
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
          Result.R := 255/255;
          Result.G := 255/255;
          Result.B := 0;
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
        // daniel20 colors
    9:  //Turquoise
        begin
          Result.R := 0/255;
          Result.G := 255/255;
          Result.B := 230/255;
        end;
    10: //Salmon
        begin
          Result.R := 255/255;
          Result.G := 127/255;
          Result.B := 102/255;
        end;
    11: //GreenYellow
        begin
          Result.R := 153/255;
          Result.G := 255/255;
          Result.B := 102/255;
        end;
    12: //Lavender
        begin
          Result.R := 204/255;
          Result.G := 204/255;
          Result.B := 255/255;
        end;
    13: //Beige
        begin
          Result.R := 255/255;
          Result.G := 230/255;
          Result.B := 204/255;
        end;
    14: //Teal
        begin
          Result.R := 51/255;
          Result.G := 153/255;
          Result.B := 153/255;
        end;
    15: //Orchid
        begin
          Result.R := 153/255;
          Result.G := 0;
          Result.B := 204/255;
        end;
    16: //SteelBlue
        begin
          Result.R := 51/255;
          Result.G := 102/255;
          Result.B := 153/255;
        end;
    17: //Plum
        begin
          Result.R := 255/255;
          Result.G := 153/255;
          Result.B := 255/255;
        end;
    18: //Chocolate
        begin
          Result.R := 138/255;
          Result.G := 92/255;
          Result.B := 46/255;
        end;
    19: //Gold
        begin
          Result.R := 255/255;
          Result.G := 204/255;
          Result.B := 51/255;
        end;
    end;
end;

function GetLyricGrayColor(Color: integer): TRGB;
begin
  case Color of
    0:  begin
          // black
          Result.R := 0;
          Result.G := 0;
          Result.B := 0;
        end;
    1:  begin
          // gray +3
          Result.R := 32/255;
          Result.G := 32/255;
          Result.B := 32/255;
        end;
    2:  begin
          // gray +2
          Result.R := 64/255;
          Result.G := 64/255;
          Result.B := 64/255;
        end;
    3:  begin
          // gray +1
          Result.R := 96/255;
          Result.G := 96/255;
          Result.B := 96/255;
        end;
    4:  begin
          // gray
          Result.R := 128/255;
          Result.G := 128/255;
          Result.B := 128/255;
        end;
    5:  begin
          // gray -1
          Result.R := 160/255;
          Result.G := 160/255;
          Result.B := 160/255;
        end;
    6:  begin
          // gray -2
          Result.R := 192/255;
          Result.G := 192/255;
          Result.B := 192/255;
        end;
    7:  begin
          // gray -3
          Result.R := 214/255;
          Result.G := 214/255;
          Result.B := 214/255;
        end;
    8:  begin
          // white
          Result.R := 1;
          Result.G := 1;
          Result.B := 1;
        end;
    else
        begin
          // black
          Result.R := 0;
          Result.G := 0;
          Result.B := 0;
        end;

    end;
end;

function GetLyricOutlineColor(Color: integer): TRGB;
begin
  case Color of
    0:  begin
          // black
          Result.R := 0;
          Result.G := 0;
          Result.B := 0;
        end;
    1:  begin
          // white
          Result.R := 1;
          Result.G := 1;
          Result.B := 1;
        end;
  end;
end;

function GetLyricBarColor(Color: integer): TRGB;
begin
  Result := GetPlayerColor(Color);
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

  //ThemeSaveButton(Name.PlayerName, 'NameButtonPlayer');

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
  ThemeSaveStatics(Theme.Statics, Name + 'Static');
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

procedure TTheme.ThemePartyLoad;
begin

  ThemeIni := TMemIniFile.Create(Themes[Ini.Theme].FileName.ToNative);

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


  ThemeIni.Free;

end;

procedure TTheme.ThemeScoreLoad;
var
  I: integer;
  prefix: string;
begin

  ThemeIni := TMemIniFile.Create(Themes[Ini.Theme].FileName.ToNative);

  // Score
  ThemeLoadBasic(Score, 'Score');

  ThemeLoadText(Score.TextArtist, 'ScoreTextArtist');
  ThemeLoadText(Score.TextTitle, 'ScoreTextTitle');
  ThemeLoadText(Score.TextArtistTitle, 'ScoreTextArtistTitle');

  if (Ini.Players < 3) or (Ini.Screens = 1) then
    prefix := ''
  else
  begin
    // 4 players 1 screen
    if (Ini.Players = 3) then
      prefix := 'FourP';

    // 6 players 1 screen
    if (Ini.Players = 4) then
      prefix := 'SixP';
  end;

  for I := 1 to 6 do
  begin
    ThemeLoadStatics(Score.PlayerStatic[I],        'Score' + prefix + 'Player' + IntToStr(I) + 'Static');
    ThemeLoadTexts(Score.PlayerTexts[I],           'Score' + prefix + 'Player' + IntToStr(I) + 'Text');
    ThemeLoadStatic(Score.AvatarStatic[I],         'Score' + prefix + 'Player' + IntToStr(I) + 'Avatar');

    ThemeLoadText(Score.TextName[I],               'Score' + prefix + 'TextName'             + IntToStr(I));
    ThemeLoadText(Score.TextScore[I],              'Score' + prefix + 'TextScore'            + IntToStr(I));
    ThemeLoadText(Score.TextNotes[I],              'Score' + prefix + 'TextNotes'            + IntToStr(I));
    ThemeLoadText(Score.TextNotesScore[I],         'Score' + prefix + 'TextNotesScore'       + IntToStr(I));
    ThemeLoadText(Score.TextLineBonus[I],          'Score' + prefix + 'TextLineBonus'        + IntToStr(I));
    ThemeLoadText(Score.TextLineBonusScore[I],     'Score' + prefix + 'TextLineBonusScore'   + IntToStr(I));
    ThemeLoadText(Score.TextGoldenNotes[I],        'Score' + prefix + 'TextGoldenNotes'      + IntToStr(I));
    ThemeLoadText(Score.TextGoldenNotesScore[I],   'Score' + prefix + 'TextGoldenNotesScore' + IntToStr(I));
    ThemeLoadText(Score.TextTotal[I],              'Score' + prefix + 'TextTotal'            + IntToStr(I));
    ThemeLoadText(Score.TextTotalScore[I],         'Score' + prefix + 'TextTotalScore'       + IntToStr(I));

    ThemeLoadStatic(Score.StaticBoxLightest[I],    'Score' + prefix + 'StaticBoxLightest'    + IntToStr(I));
    ThemeLoadStatic(Score.StaticBoxLight[I],       'Score' + prefix + 'StaticBoxLight'       + IntToStr(I));
    ThemeLoadStatic(Score.StaticBoxDark[I],        'Score' + prefix + 'StaticBoxDark'        + IntToStr(I));

    ThemeLoadStatic(Score.StaticBackLevel[I],      'Score' + prefix + 'StaticBackLevel'      + IntToStr(I));
    ThemeLoadStatic(Score.StaticBackLevelRound[I], 'Score' + prefix + 'StaticBackLevelRound' + IntToStr(I));
    ThemeLoadStatic(Score.StaticLevel[I],          'Score' + prefix + 'StaticLevel'          + IntToStr(I));
    ThemeLoadStatic(Score.StaticLevelRound[I],     'Score' + prefix + 'StaticLevelRound'     + IntToStr(I));
    ThemeLoadStatic(Score.StaticRatings[I],        'Score' + prefix + 'StaticRatingPicture'  + IntToStr(I));
  end;

  ThemeIni.Free;
end;

procedure TTheme.ThemeSongLoad;
var
  I, C: integer;
  prefix: string;
begin
  case (TSongMenuMode(Ini.SongMenu)) of
    smRoulette: prefix := 'Roulette';
    smChessboard: prefix := 'Chessboard';
    smCarousel: prefix := 'Carousel';
    smSlotMachine: prefix := 'SlotMachine';
    smSlide: prefix := 'Slide';
    smList: prefix := 'List';
    smMosaic: prefix := 'Mosaic';
  end;

  ThemeIni := TMemIniFile.Create(Themes[Ini.Theme].FileName.ToNative);

  // Song
  ThemeLoadBasic(Song, 'Song' + prefix);

  ThemeLoadText(Song.TextArtist, 'Song' + prefix + 'TextArtist');
  ThemeLoadText(Song.TextTitle, 'Song' + prefix + 'TextTitle');
  ThemeLoadText(Song.TextNumber, 'Song' + prefix + 'TextNumber');
  ThemeLoadText(Song.TextYear, 'Song' + prefix + 'TextYear');

  // medley playlist
  Song.TextMedleyMax := ThemeIni.ReadInteger('Song' + prefix + 'TextMedleyMax', 'N', 4);

  SetLength(Song.TextArtistMedley, Song.TextMedleyMax);
  SetLength(Song.TextTitleMedley, Song.TextMedleyMax);
  SetLength(Song.TextNumberMedley, Song.TextMedleyMax);
  SetLength(Song.StaticMedley, Song.TextMedleyMax);

  for I := 0 to Song.TextMedleyMax - 1 do
  begin
    ThemeLoadText(Song.TextArtistMedley[I], 'Song' + prefix + 'TextMedleyArtist' + IntToStr(I + 1));
    ThemeLoadText(Song.TextTitleMedley[I], 'Song' + prefix + 'TextMedleyTitle' + IntToStr(I + 1));
    ThemeLoadText(Song.TextNumberMedley[I], 'Song' + prefix + 'TextMedleyNumber' + IntToStr(I + 1));
    ThemeLoadStatic(Song.StaticMedley[I], 'Song' + prefix + 'StaticMedley' + IntToStr(I + 1));
  end;

  //Video Icon Mod
  ThemeLoadStatic(Song.VideoIcon, 'Song' + prefix + 'VideoIcon');

  //Medley Icons
  ThemeLoadStatic(Song.MedleyIcon, 'Song' + prefix + 'MedleyIcon');
  ThemeLoadStatic(Song.CalculatedMedleyIcon, 'Song' + prefix + 'CalculatedMedleyIcon');

  //Duet Icon
  ThemeLoadStatic(Song.DuetIcon, 'Song' + prefix + 'DuetIcon');

  //Show Cat in TopLeft Mod
  ThemeLoadStatic(Song.StaticCat, 'Song' + prefix + 'StaticCat');
  ThemeLoadText(Song.TextCat, 'Song' + prefix + 'TextCat');

  //Load Cover Pos and Size from Theme Mod
  Song.Cover.X := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'X', 300);
  Song.Cover.Y := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'Y', 190);
  Song.Cover.W := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'W', 300);
  Song.Cover.H := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'H', 200);

  // 0 - roulette
  // 1 - chessboard
  // 2 - carousel
  // 3 - slotmachine
  // 4 - slide
  // 5 - list
  // 6 - mosaic
  
  if (TSongMenuMode(Ini.SongMenu) in [smChessboard, smMosaic]) then
  begin
    Song.Cover.Rows := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'Rows', 4);
    Song.Cover.Cols := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'Cols', 4);
    Song.Cover.Padding := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'Padding', 0);
    Song.Cover.SelectX := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'SelectX', 300);
    Song.Cover.SelectY := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'SelectY', 120);
    Song.Cover.SelectW := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'SelectW', 325);
    Song.Cover.SelectH := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'SelectH', 200);
    Song.Cover.SelectReflection := ThemeIni.ReadBool('Song' + prefix + 'Cover', 'SelectReflection', false);
    Song.Cover.SelectReflectionSpacing := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'SelectReflectionSpacing', 0);
    Song.Cover.ZoomThumbW := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'ZoomThumbW', 120);
    Song.Cover.ZoomThumbH := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'ZoomThumbH', 120);
    Song.Cover.ZoomThumbStyle := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'ZoomThumbStyle', 0);
    Song.Cover.Tex := ThemeIni.ReadString('Song' + prefix + 'Cover',  'Text', '');
  end;

  if (TSongMenuMode(Ini.SongMenu) in [smCarousel, smSlide]) then
  begin
    Song.Cover.Padding := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'Padding', 60);
  end;

  if (TSongMenuMode(Ini.SongMenu) = smList) then
  begin
    Song.Cover.SelectX := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'SelectX', 300);
    Song.Cover.SelectY := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'SelectY', 120);
    Song.Cover.SelectW := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'SelectW', 325);
    Song.Cover.SelectH := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'SelectH', 200);
    Song.Cover.SelectReflection := ThemeIni.ReadBool('Song' + prefix + 'Cover', 'SelectReflection', false);
    Song.Cover.SelectReflectionSpacing := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'SelectReflectionSpacing', 0);
    Song.Cover.Padding := ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'Padding', 4);

    Song.ListCover.Rows := ThemeIni.ReadInteger('Song' + prefix + 'SelectSong', 'Rows', 5);
    Song.ListCover.X := ThemeIni.ReadInteger('Song' + prefix + 'SelectSong', 'X', 300);
    Song.ListCover.Y := ThemeIni.ReadInteger('Song' + prefix + 'SelectSong', 'Y', 120);
    Song.ListCover.W := ThemeIni.ReadInteger('Song' + prefix + 'SelectSong', 'W', 325);
    Song.ListCover.H := ThemeIni.ReadInteger('Song' + prefix + 'SelectSong', 'H', 200);
    Song.ListCover.Z := ThemeIni.ReadInteger('Song' + prefix + 'SelectSong', 'Z', 1);
    Song.ListCover.Reflection := ThemeIni.ReadBool('Song' + prefix + 'SelectSong', 'Reflection', false);
    Song.ListCover.ReflectionSpacing := ThemeIni.ReadInteger('Song' + prefix + 'SelectSong', 'ReflectionSpacing', 0);
    Song.ListCover.Padding := ThemeIni.ReadInteger('Song' + prefix + 'SelectSong', 'Padding', 4);

    Song.ListCover.Typ   := ParseTextureType(ThemeIni.ReadString('Song' + prefix + 'SelectSong', 'Type', ''), TEXTURE_TYPE_PLAIN);
    Song.ListCover.Tex := ThemeIni.ReadString('Song' + prefix + 'SelectSong', 'Tex', '');
    Song.ListCover.DTex := ThemeIni.ReadString('Song' + prefix + 'SelectSong', 'DTex', '');

    Song.ListCover.Color := ThemeIni.ReadString('Song' + prefix + 'SelectSong', 'Color', '');

    C := ColorExists(Song.ListCover.Color);
    if C >= 0 then
    begin
      Song.ListCover.ColR := Color[C].RGB.R;
      Song.ListCover.ColG := Color[C].RGB.G;
      Song.ListCover.ColB := Color[C].RGB.B;
    end;

    Song.ListCover.DColor := ThemeIni.ReadString('Song' + prefix + 'SelectSong', 'DColor', '');

    C := ColorExists(Song.ListCover.DColor);
    if C >= 0 then
    begin
      Song.ListCover.DColR := Color[C].RGB.R;
      Song.ListCover.DColG := Color[C].RGB.G;
      Song.ListCover.DColB := Color[C].RGB.B;
    end;

  end;

  //  Song.Cover.Style := ThemeIni.ReadInteger('SongCover', 'Style', 4);
  Song.Cover.Reflections := (ThemeIni.ReadInteger('Song' + prefix + 'Cover', 'Reflections', 0) = 1);
  //Load Cover Pos and Size from Theme Mod End

  ThemeLoadEqualizer(Song.Equalizer, 'Song' + prefix + 'Equalizer');

  //Screen Song Scores
  ThemeLoadText(Song.TextScore, 'Song' + prefix + 'TextScore');
  ThemeLoadText(Song.TextMaxScore, 'Song' + prefix + 'TextMaxScore');
  ThemeLoadText(Song.TextMediaScore, 'Song' + prefix + 'TextMediaScore');
  ThemeLoadText(Song.TextMaxScore2, 'Song' + prefix + 'TextMaxScore2');
  ThemeLoadText(Song.TextMediaScore2, 'Song' + prefix + 'TextMediaScore2');
  ThemeLoadText(Song.TextScoreUser, 'Song' + prefix + 'TextScoreUser');
  ThemeLoadText(Song.TextMaxScoreLocal, 'Song' + prefix + 'TextMaxScoreLocal');
  ThemeLoadText(Song.TextMediaScoreLocal, 'Song' + prefix + 'TextMediaScoreLocal');
  ThemeLoadText(Song.TextScoreUserLocal, 'Song' + prefix + 'TextScoreUserLocal');

  //Party and Non Party specific Statics and Texts
  ThemeLoadStatics (Song.StaticParty, 'Song' + prefix + 'StaticParty');
  ThemeLoadTexts (Song.TextParty, 'Song' + prefix + 'TextParty');

  ThemeLoadStatics (Song.StaticNonParty, 'Song' + prefix + 'StaticNonParty');
  ThemeLoadTexts (Song.TextNonParty, 'Song' + prefix + 'TextNonParty');

  // Duet Singers
  ThemeLoadStatic (Song.Static2PlayersDuetSingerP1, 'Song' + prefix + 'Static2PlayersDuetSingerP1');
  ThemeLoadStatic (Song.Static2PlayersDuetSingerP2, 'Song' + prefix + 'Static2PlayersDuetSingerP2');
  ThemeLoadText (Song.Text2PlayersDuetSingerP1, 'Song' + prefix + 'Text2PlayersDuetSingerP1');
  ThemeLoadText (Song.Text2PlayersDuetSingerP2, 'Song' + prefix + 'Text2PlayersDuetSingerP2');

  ThemeLoadStatic (Song.Static3PlayersDuetSingerP1, 'Song' + prefix + 'Static3PlayersDuetSingerP1');
  ThemeLoadStatic (Song.Static3PlayersDuetSingerP2, 'Song' + prefix + 'Static3PlayersDuetSingerP2');
  ThemeLoadStatic (Song.Static3PlayersDuetSingerP3, 'Song' + prefix + 'Static3PlayersDuetSingerP3');
  ThemeLoadText (Song.Text3PlayersDuetSingerP1, 'Song' + prefix + 'Text3PlayersDuetSingerP1');
  ThemeLoadText (Song.Text3PlayersDuetSingerP2, 'Song' + prefix + 'Text3PlayersDuetSingerP2');
  ThemeLoadText (Song.Text3PlayersDuetSingerP3, 'Song' + prefix + 'Text3PlayersDuetSingerP3');

  // 4/6 players 1 screen
  ThemeLoadStatic (Song.Static4PlayersDuetSingerP3, 'Song' + prefix + 'Static4PlayersDuetSingerP3');
  ThemeLoadStatic (Song.Static4PlayersDuetSingerP4, 'Song' + prefix + 'Static4PlayersDuetSingerP4');

  ThemeLoadStatic (Song.Static6PlayersDuetSingerP4, 'Song' + prefix + 'Static6PlayersDuetSingerP4');
  ThemeLoadStatic (Song.Static6PlayersDuetSingerP5, 'Song' + prefix + 'Static6PlayersDuetSingerP5');
  ThemeLoadStatic (Song.Static6PlayersDuetSingerP6, 'Song' + prefix + 'Static6PlayersDuetSingerP6');

  //Party Mode
  ThemeLoadStatic(Song.StaticTeam1Joker1, 'Song' + prefix + 'StaticTeam1Joker1');
  ThemeLoadStatic(Song.StaticTeam1Joker2, 'Song' + prefix + 'StaticTeam1Joker2');
  ThemeLoadStatic(Song.StaticTeam1Joker3, 'Song' + prefix + 'StaticTeam1Joker3');
  ThemeLoadStatic(Song.StaticTeam1Joker4, 'Song' + prefix + 'StaticTeam1Joker4');
  ThemeLoadStatic(Song.StaticTeam1Joker5, 'Song' + prefix + 'StaticTeam1Joker5');

  ThemeLoadStatic(Song.StaticTeam2Joker1, 'Song' + prefix + 'StaticTeam2Joker1');
  ThemeLoadStatic(Song.StaticTeam2Joker2, 'Song' + prefix + 'StaticTeam2Joker2');
  ThemeLoadStatic(Song.StaticTeam2Joker3, 'Song' + prefix + 'StaticTeam2Joker3');
  ThemeLoadStatic(Song.StaticTeam2Joker4, 'Song' + prefix + 'StaticTeam2Joker4');
  ThemeLoadStatic(Song.StaticTeam2Joker5, 'Song' + prefix + 'StaticTeam2Joker5');

  ThemeLoadStatic(Song.StaticTeam3Joker1, 'Song' + prefix + 'StaticTeam3Joker1');
  ThemeLoadStatic(Song.StaticTeam3Joker2, 'Song' + prefix + 'StaticTeam3Joker2');
  ThemeLoadStatic(Song.StaticTeam3Joker3, 'Song' + prefix + 'StaticTeam3Joker3');
  ThemeLoadStatic(Song.StaticTeam3Joker4, 'Song' + prefix + 'StaticTeam3Joker4');
  ThemeLoadStatic(Song.StaticTeam3Joker5, 'Song' + prefix + 'StaticTeam3Joker5');

  ThemeLoadText (Song.TextPartyTime, 'Song' + prefix + 'TextPartyTime');

  ThemeLoadText (Song.InfoMessageText, 'Song' + prefix + 'InfoMessageText');
  ThemeLoadStatic (Song.InfoMessageBG, 'Song' + prefix + 'InfoMessageBG');
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

  freeandnil(Jukebox);
  Jukebox := TThemeJukebox.Create;

  freeandnil(JukeboxPlaylist);
  JukeboxPlaylist := TThemeJukeboxPlaylist.Create;

  freeandnil(AboutMain);
  AboutMain := TThemeAboutMain.Create;

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

  freeandnil(OptionsNetwork);
  OptionsNetwork := TThemeOptionsNetwork.Create;

  freeandnil(OptionsWebcam);
  OptionsWebcam := TThemeOptionsWebcam.Create;

  freeandnil(OptionsJukebox);
  OptionsJukebox := TThemeOptionsJukebox.Create;

  freeandnil(Edit);
  Edit := TThemeEdit.Create;

  freeandnil(EditSub);
  EditSub := TThemeEditSub.Create;

  freeandnil(ErrorPopup);
  ErrorPopup := TThemeError.Create;

  freeandnil(CheckPopup);
  CheckPopup := TThemeCheck.Create;

  freeandnil(InsertUserPopup);
  InsertUserPopup := TThemeInsertUser.Create;

  freeandnil(SendScorePopup);
  SendScorePopup := TThemeSendScore.Create;

  freeandnil(ScoreDownloadPopup);
  ScoreDownloadPopup := TThemeScoreDownload.Create;

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
