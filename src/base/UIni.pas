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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/base/UIni.pas $
 * $Id: UIni.pas 2630 2010-09-04 10:18:40Z brunzelchen $
 *}

unit UIni;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
  IniFiles,
  SysUtils,
  UCommon,
  ULog,
  UTextEncoding,
  UFilesystem,
  UPath;

type
  {**
   * TInputDeviceConfig stores the configuration for an input device.
   * Configurations will be stored in the InputDeviceConfig array.
   * Note that not all devices listed in InputDeviceConfig are active devices.
   * Some might be unplugged and hence unavailable.
   * Available devices are held in TAudioInputProcessor.DeviceList. Each
   * TAudioInputDevice listed there has a CfgIndex field which is the index to
   * its configuration in the InputDeviceConfig array.
   *}
  PInputDeviceConfig = ^TInputDeviceConfig;
  TInputDeviceConfig = record
    Name:               string;  //**< Name of the input device
    Input:              integer; //**< Index of the input source to use for recording
    Latency:            integer; //**< Latency in ms, or LATENCY_AUTODETECT for default

    {**
     * Mapping of recording channels to players, e.g. ChannelToPlayerMap[0] = 2
     * maps the channel 0 (left) to player 2.
     * A player index of 0 (CHANNEL_OFF) means that the channel is not assigned
     * to any player (the channel is off).
     *}
    ChannelToPlayerMap: array of integer;
  end;

{* Constants for TInputDeviceConfig *}
const
  CHANNEL_OFF = 0;         // for field ChannelToPlayerMap
  LATENCY_AUTODETECT = -1; // for field Latency

type

//Options

  TVisualizerOption      = (voOff, voWhenNoVideo, voOn);
  TBackgroundMusicOption = (bmoOff, bmoOn);
  TSongMenuMode = ( smRoulette, smChessboard, smCarousel, smSlotMachine, smSlide, smList, smMosaic);

  TIni = class
    private
      function ExtractKeyIndex(const Key, Prefix, Suffix: string): integer;
      function GetMaxKeyIndex(Keys: TStringList; const Prefix, Suffix: string): integer;
      function ReadArrayIndex(const SearchArray: array of UTF8String; IniFile: TCustomIniFile;
          IniSection: string; IniProperty: string; Default: integer): integer;

      procedure TranslateOptionValues;
      procedure LoadInputDeviceCfg(IniFile: TMemIniFile);
      procedure SaveInputDeviceCfg(IniFile: TIniFile);
      procedure LoadThemes(IniFile: TCustomIniFile);

      procedure LoadPaths(IniFile: TCustomIniFile);
      procedure LoadScreenModes(IniFile: TCustomIniFile);
      procedure LoadWebcamSettings(IniFile: TCustomIniFile);

    public
      // Players or Teams colors
      SingColor:      array [0..5] of integer;
      
      Name:           array[0..15] of UTF8String;
      PlayerColor:    array[0..5] of integer;
      TeamColor:      array[0..2] of integer;

      PlayerAvatar:   array[0..5] of UTF8String;
      PlayerLevel:    array[0..5] of integer;

      // Templates for Names Mod
      NameTeam:       array[0..2] of UTF8String;
      NameTemplate:   array[0..11] of UTF8String;

      //Filename of the opened iniFile
      Filename:       IPath;

      // Game
      Players:        integer;
      Difficulty:     integer;
      Language:       integer;
      SongMenu:       integer;
      Tabs:           integer;
      TabsAtStartup:  integer; //Tabs at Startup fix
      Sorting:        integer;
      ShowScores:     integer;
      ShowWebScore:   integer;
      Debug:          integer;

      // Graphics
      MaxFramerate:   byte;
      MaxFramerateGet: byte;
      Screens:        integer;
      Split:          integer;
      Resolution:     integer;
      Depth:          integer;
      VisualizerOption: integer;
      FullScreen:     integer;
      TextureSize:    integer;
      SingWindow:     integer;
      Oscilloscope:   integer;
      // not used
      //Spectrum:       integer;
      //Spectrograph:   integer;
      MovieSize:      integer;
      VideoPreview:   integer;
      VideoEnabled:   integer;

      // Sound
      MicBoost:       integer;
      ClickAssist:    integer;
      BeatClick:      integer;
      SavePlayback:   integer;
      ThresholdIndex: integer;
      AudioOutputBufferSizeIndex: integer;
      VoicePassthrough: integer;
      SoundFont:      string;

      SyncTo: integer;

      //Song Preview
      PreviewVolume:  integer;
      PreviewFading:  integer;

      // Lyrics
      LyricsFont:     integer;
      LyricsEffect:   integer;
      NoteLines:      integer;

      // Themes
      Theme:          integer;
      SkinNo:         integer;
      Color:          integer;
      BackgroundMusicOption: integer;

      // Record
      InputDeviceConfig: array of TInputDeviceConfig;

      // Advanced
      LoadAnimation:  integer;
      EffectSing:     integer;
      ScreenFade:     integer;
      AskBeforeDel:   integer;
      OnSongClick:    integer;
      LineBonus:      integer;
      PartyPopup:     integer;
      SingScores:     integer;
      TopScores:      integer;
      SingTimebarMode:       integer;
      JukeboxTimebarMode:    integer;

      // Controller
      Joypad:         integer;
      Mouse:          integer;

      // WebCam
      WebCamID:         integer;
      WebcamResolution: integer;
      WebCamFPS:        integer;
      WebCamFlip:       integer;
      WebCamBrightness: integer;
      WebCamSaturation: integer;
      WebCamHue:        integer;
      WebCamEffect:     integer;

      // Jukebox
      JukeboxSongMenu: integer;

      JukeboxFont:     integer;
      JukeboxEffect:   integer;
      JukeboxAlpha:    integer;

      JukeboxLine:      integer;
      JukeboxProperty:  integer;

      // Jukebox Lyric Fill Color
      JukeboxSingLineColor:   integer;
      JukeboxActualLineColor: integer;
      JukeboxNextLineColor:   integer;

      JukeboxSingLineOutlineColor:   integer;
      JukeboxActualLineOutlineColor: integer;
      JukeboxNextLineOutlineColor:   integer;

      CurrentJukeboxSingLineOutlineColor:   integer;
      CurrentJukeboxActualLineOutlineColor: integer;
      CurrentJukeboxNextLineOutlineColor:   integer;

      JukeboxSingLineOtherColorR: integer;
      JukeboxSingLineOtherColorG: integer;
      JukeboxSingLineOtherColorB: integer;

      JukeboxActualLineOtherColorR: integer;
      JukeboxActualLineOtherColorG: integer;
      JukeboxActualLineOtherColorB: integer;

      JukeboxNextLineOtherColorR: integer;
      JukeboxNextLineOtherColorG: integer;
      JukeboxNextLineOtherColorB: integer;

      JukeboxSingLineOtherOColorR: integer;
      JukeboxSingLineOtherOColorG: integer;
      JukeboxSingLineOtherOColorB: integer;

      JukeboxActualLineOtherOColorR: integer;
      JukeboxActualLineOtherOColorG: integer;
      JukeboxActualLineOtherOColorB: integer;

      JukeboxNextLineOtherOColorR: integer;
      JukeboxNextLineOtherOColorG: integer;
      JukeboxNextLineOtherOColorB: integer;


      // default encoding for texts (lyrics, song-name, ...)
      DefaultEncoding: TEncoding;

      procedure Load();
      procedure Save();
      procedure SaveNames;
      procedure SaveLevel;
      procedure SavePlayerColors;
      procedure SavePlayerAvatars;
      procedure SavePlayerLevels;
      procedure SaveTeamColors;
      procedure SaveShowWebScore;
      procedure SaveJukeboxSongMenu;

      procedure SaveSoundFont(Name: string);
      procedure SaveWebcamSettings();
      procedure SaveNumberOfPlayers;
      procedure SaveSingTimebarMode;
      procedure SaveJukeboxTimebarMode;

  end;

var
  Ini:         TIni;
  IResolution: TUTF8StringDynArray;
  ILanguage:   TUTF8StringDynArray;
  ITheme:      TUTF8StringDynArray;
  ISkin:       TUTF8StringDynArray;

{*
 * Options
 *}

const
  IPlayers:     array[0..4] of UTF8String = ('1', '2', '3', '4', '6');
  IPlayersVals: array[0..4] of integer    = ( 1 ,  2 ,  3 ,  4 ,  6 );

  IDifficulty:  array[0..2] of UTF8String = ('Easy', 'Medium', 'Hard');
  ITabs:        array[0..1] of UTF8String = ('Off', 'On');

const
  //ISorting:      array[0..9] of UTF8String = ('Edition', 'Genre', 'Language', 'Folder', 'Title', 'Artist', 'Artist2', 'Year', 'Decade', 'Playlist');
  ISorting:      array[0..8] of UTF8String = ('Edition', 'Genre', 'Language', 'Folder', 'Title', 'Artist', 'Artist2', 'Year', 'Decade');
  ISongMenuMode: array[0..6] of UTF8String = ('Roulette', 'Chessboard', 'Carousel', 'Slot Machine', 'Slide', 'List', 'Mosaic');

type
  TSortingType = (sEdition, sGenre, sLanguage, sFolder, sTitle, sArtist, sArtist2, sYear, sDecade, sPlaylist);

const
  IShowScores:       array[0..2] of UTF8String  = ('Off', 'When exists', 'On');

  IDebug:            array[0..1] of UTF8String  = ('Off', 'On');

  IMaxFramerate:     array[0..11] of UTF8String  = ('10', '20', '30', '40', '50', '60', '70', '80', '90', '100', '150', '200');
  IScreens:          array[0..1] of UTF8String  = ('1', '2');
  ISplit:            array[0..1] of UTF8String  = ('Off', 'On');
  IFullScreen:       array[0..1] of UTF8String  = ('Off', 'On');
  IDepth:            array[0..1] of UTF8String  = ('16 bit', '32 bit');
  IVisualizer:       array[0..2] of UTF8String  = ('Off', 'WhenNoVideo','On');

  IBackgroundMusic:  array[0..1] of UTF8String  = ('Off', 'On');

  ITextureSize:      array[0..3] of UTF8String  = ('64', '128', '256', '512');
  ITextureSizeVals:  array[0..3] of integer     = ( 64,   128,   256,   512);

  ISingWindow:       array[0..1] of UTF8String  = ('Small', 'Big');

  //SingBar Mod
  IOscilloscope:     array[0..1] of UTF8String  = ('Off', 'On');

  ISpectrum:         array[0..1] of UTF8String  = ('Off', 'On');
  ISpectrograph:     array[0..1] of UTF8String  = ('Off', 'On');
  IMovieSize:        array[0..2] of UTF8String  = ('Half', 'Full [Vid]', 'Full [BG+Vid]');
  IVideoPreview:     array[0..1] of UTF8String  = ('Off', 'On');
  IVideoEnabled:     array[0..1] of UTF8String  = ('Off', 'On');

  IClickAssist:      array[0..1] of UTF8String  = ('Off', 'On');
  IBeatClick:        array[0..1] of UTF8String  = ('Off', 'On');
  ISavePlayback:     array[0..1] of UTF8String  = ('Off', 'On');

  IThreshold:        array[0..3] of UTF8String  = ('5%', '10%', '15%', '20%');
  IThresholdVals:    array[0..3] of single  = (0.05, 0.10,  0.15,  0.20);

  IVoicePassthrough: array[0..1] of UTF8String  = ('Off', 'On');

const
  ISyncTo: array[0..2] of UTF8String  = ('Music', 'Lyrics', 'Off');
type
  TSyncToType = (stMusic, stLyrics, stOff);

const
  IAudioOutputBufferSize:     array[0..9] of UTF8String  = ('Auto', '256', '512', '1024', '2048', '4096', '8192', '16384', '32768', '65536');
  IAudioOutputBufferSizeVals: array[0..9] of integer     = ( 0,      256,   512 ,  1024 ,  2048 ,  4096 ,  8192 ,  16384 ,  32768 ,  65536 );

  IAudioInputBufferSize:      array[0..9] of UTF8String  = ('Auto', '256', '512', '1024', '2048', '4096', '8192', '16384', '32768', '65536');
  IAudioInputBufferSizeVals:  array[0..9] of integer     = ( 0,      256,   512 ,  1024 ,  2048 ,  4096 ,  8192 ,  16384 ,  32768 ,  65536 );

  //Song Preview
  IPreviewVolume:             array[0..10] of UTF8String = ('Off', '10%', '20%', '30%', '40%', '50%', '60%', '70%', '80%', '90%', '100%');
  IPreviewVolumeVals:         array[0..10] of single     = ( 0,   0.10,  0.20,  0.30,  0.40,  0.50,  0.60,  0.70,  0.80,  0.90,   1.00  );

  IPreviewFading:             array[0..5] of UTF8String  = ('Off', '1 Sec', '2 Secs', '3 Secs', '4 Secs', '5 Secs');
  IPreviewFadingVals:         array[0..5] of integer     = ( 0,     1,       2,        3,        4,        5      );

  ILyricsFont:    array[0..2] of UTF8String = ('Plain', 'OLine1', 'OLine2');
  ILyricsEffect:  array[0..4] of UTF8String = ('Simple', 'Zoom', 'Slide', 'Ball', 'Shift');
  ILyricsAlpha:   array[0..20] of UTF8String = ('0.00', '0.05', '0.10', '0.15', '0.20', '0.25', '0.30', '0.35', '0.40', '0.45', '0.50',
                                                '0.55', '0.60', '0.65', '0.70', '0.75', '0.80', '0.85', '0.90', '0.95', '1.00');
  ILyricsAlphaVals: array[0..20] of single = (0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50,
                                              0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 1.00);

  INoteLines:     array[0..1] of UTF8String = ('Off', 'On');

  //for lyric colors
  ILine:             array[0..2] of UTF8String = ('Sing', 'Actual', 'Next');
  IAttribute:        array[0..1] of UTF8String = ('Fill', 'Outline');
  ISingLineColor:    array[0..20] of UTF8String = ('Blue', 'Green', 'Pink', 'Red', 'Violet', 'Orange', 'Yellow', 'Brown', 'Black', 'Turquoise', 'Salmon', 'GreenYellow', 'Lavender', 'Beige', 'Teal', 'Orchid', 'SteelBlue', 'Plum', 'Chocolate', 'Gold', 'Other');
  IActualLineColor:  array[0..9] of UTF8String = ('Black', 'Gray +3', 'Gray +2', 'Gray +1', 'Gray', 'Gray -1', 'Gray -2', 'Gray -3', 'White', 'Other');
  INextLineColor:    array[0..9] of UTF8String = ('Black', 'Gray +3', 'Gray +2', 'Gray +1', 'Gray', 'Gray -1', 'Gray -2', 'Gray -3', 'White', 'Other');
  //outline
  ISingLineOColor:    array[0..2] of UTF8String = ('Black', 'White', 'Other');
  IActualLineOColor:  array[0..2] of UTF8String = ('Black', 'White', 'Other');
  INextLineOColor:    array[0..2] of UTF8String = ('Black', 'White', 'Other');

  IHexSingColor: array[0..20] of UTF8String = ('0096FF', '3FBF3F', 'FF3FC0', 'DC0000', 'B43FE6', 'FF9000', 'FFFF00', 'C07F1F', '000000', '00FFE6', 'FF7F66',
                                                '99FF66', 'CCCCFF', 'FFE6CC', '339999', '9900CC', '336699', 'FF99FF', '8A5C2E', 'FFCC33', '');
  //IHexColor:     array[0..9] of UTF8String = ('0096FF', '3FBF3F', 'FF3FC0', 'DC0000', 'B43FE6', 'FF9000', 'FFFF00', 'C07F1F', '000000', '');
  IHexGrayColor: array[0..9] of UTF8String = ('000000', '202020', '404040', '606060', '808080', 'A0A0A0', 'C0C0C0', 'D6D6D6', 'FFFFFF', '');
  IHexOColor:    array[0..2] of UTF8String = ('000000', 'FFFFFF', '');

  IJukeboxSongMenu: array[0..1] of UTF8String = ('Off', 'On');

  IColor:         array[0..8] of UTF8String = ('Blue', 'Green', 'Pink', 'Red', 'Violet', 'Orange', 'Yellow', 'Brown', 'Black');

  // Advanced
  ILoadAnimation: array[0..1] of UTF8String = ('Off', 'On');
  IEffectSing:    array[0..1] of UTF8String = ('Off', 'On');
  IScreenFade:    array[0..1] of UTF8String = ('Off', 'On');
  IAskbeforeDel:  array[0..1] of UTF8String = ('Off', 'On');
  ISingScores:    array[0..1] of UTF8String = ('Off', 'On');
  ITopScores:    array[0..1] of UTF8String = ('All', 'Player');
  IOnSongClick:   array[0..2] of UTF8String = ('Sing', 'Select Players', 'Open Menu');
  sStartSing = 0;
  sSelectPlayer = 1;
  sOpenMenu = 2;

  ILineBonus:     array[0..1] of UTF8String = ('Off', 'On');
  IPartyPopup:    array[0..1] of UTF8String = ('Off', 'On');

  IJoypad:        array[0..1] of UTF8String = ('Off', 'On');
  IMouse:         array[0..2] of UTF8String = ('Off', 'Hardware Cursor', 'Software Cursor');

  ISingTimebarMode:    array[0..2] of UTF8String = ('Current', 'Remaining', 'Total');
  IJukeboxTimebarMode: array[0..2] of UTF8String = ('Current', 'Remaining', 'Total');

  // Recording options
  IChannelPlayer: array[0..6] of UTF8String = ('Off', '1', '2', '3', '4', '5', '6');
  IMicBoost:      array[0..3] of UTF8String = ('Off', '+6dB', '+12dB', '+18dB');

  // Webcam
  IWebcamResolution: array[0..5] of UTF8String = ('160x120', '176x144', '320x240', '352x288', '640x480', '800x600');
  IWebcamFPS:        array[0..8] of UTF8String = ('10', '12', '15', '18', '20', '22', '25', '28', '30');
  IWebcamFlip:       array[0..1] of UTF8String = ('Off', 'On');

{*
 * Translated options
 *}

var
  ILanguageTranslated:         array of UTF8String;


  IDifficultyTranslated:       array[0..2] of UTF8String  = ('Easy', 'Medium', 'Hard');
  ITabsTranslated:             array[0..1] of UTF8String  = ('Off', 'On');

  ISongMenuTranslated:         array[0..6] of UTF8String  = ('Roulette', 'Chessboard', 'Carousel', 'Slot Machine', 'Slide', 'List', 'Mosaic');

  //ISortingTranslated:          array[0..9] of UTF8String  = ('Edition', 'Genre', 'Language', 'Folder', 'Title', 'Artist', 'Artist2', 'Year', 'Decade', 'Playlist');
  ISortingTranslated:          array[0..8] of UTF8String  = ('Edition', 'Genre', 'Language', 'Folder', 'Title', 'Artist', 'Artist2', 'Year', 'Decade');

  IShowScoresTranslated:       array[0..2] of UTF8String  = ('Off', 'WhenExists', 'On');

  IDebugTranslated:            array[0..1] of UTF8String  = ('Off', 'On');

  IFullScreenTranslated:       array[0..1] of UTF8String  = ('Off', 'On');
  IVisualizerTranslated:       array[0..2] of UTF8String  = ('Off', 'WhenNoVideo','On');

  IBackgroundMusicTranslated:  array[0..1] of UTF8String  = ('Off', 'On');
  ISingWindowTranslated:       array[0..1] of UTF8String  = ('Small', 'Big');

  //SingBar Mod
  IOscilloscopeTranslated:     array[0..1] of UTF8String  = ('Off', 'On');

  ISpectrumTranslated:         array[0..1] of UTF8String  = ('Off', 'On');
  ISpectrographTranslated:     array[0..1] of UTF8String  = ('Off', 'On');
  IMovieSizeTranslated:        array[0..2] of UTF8String  = ('Half', 'Full [Vid]', 'Full [BG+Vid]');
  IVideoPreviewTranslated:     array[0..1] of UTF8String  = ('Off', 'On');
  IVideoEnabledTranslated:     array[0..1] of UTF8String  = ('Off', 'On');

  IClickAssistTranslated:      array[0..1] of UTF8String  = ('Off', 'On');
  IBeatClickTranslated:        array[0..1] of UTF8String  = ('Off', 'On');
  ISavePlaybackTranslated:     array[0..1] of UTF8String  = ('Off', 'On');

  IVoicePassthroughTranslated: array[0..1] of UTF8String  = ('Off', 'On');

  ISyncToTranslated:           array[0..2] of UTF8String  = ('Music', 'Lyrics', 'Off');

  //Song Preview
  IPreviewVolumeTranslated:    array[0..10] of UTF8String = ('Off', '10%', '20%', '30%', '40%', '50%', '60%', '70%', '80%', '90%', '100%');

  IAudioOutputBufferSizeTranslated: array[0..9] of UTF8String  = ('Auto', '256', '512', '1024', '2048', '4096', '8192', '16384', '32768', '65536');

  IAudioInputBufferSizeTranslated:  array[0..9] of UTF8String  = ('Auto', '256', '512', '1024', '2048', '4096', '8192', '16384', '32768', '65536');

  IPreviewFadingTranslated:    array[0..5] of UTF8String  = ('Off', '1 Sec', '2 Secs', '3 Secs', '4 Secs', '5 Secs');

  ILyricsFontTranslated:       array[0..2] of UTF8String = ('Plain', 'OLine1', 'OLine2');
  ILyricsEffectTranslated:     array[0..4] of UTF8String = ('Simple', 'Zoom', 'Slide', 'Ball', 'Shift');
  INoteLinesTranslated:        array[0..1] of UTF8String = ('Off', 'On');
  IColorTranslated:            array[0..8] of UTF8String = ('Blue', 'Green', 'Pink', 'Red', 'Violet', 'Orange', 'Yellow', 'Brown', 'Black');
  IPlayerColorTranslated:      array[0..15] of UTF8String = ('Blue', 'Red', 'Green', 'Yellow', 'Orange', 'Pink',  'Violet', 'Brown', 'Gray', 'Dark Blue', 'Sky', 'Cyan', 'Flame', 'Orchid', 'Harlequin', 'Lime');

  //for lyric colors
  ILineTranslated:             array[0..2] of UTF8String = ('Sing', 'Actual', 'Next');
  IPropertyTranslated:         array[0..1] of UTF8String = ('Fill', 'Outline');

  ISingLineColorTranslated:    array[0..20] of UTF8String = ('Blue', 'Green', 'Pink', 'Red', 'Violet', 'Orange', 'Yellow', 'Brown', 'Black', 'Turquoise', 'Salmon', 'GreenYellow', 'Lavender', 'Beige', 'Teal', 'Orchid', 'SteelBlue', 'Plum', 'Chocolate', 'Gold', 'Other');
  IActualLineColorTranslated:  array[0..9] of UTF8String = ('Black', 'Gray +3', 'Gray +2', 'Gray +1', 'Gray', 'Gray -1', 'Gray -2', 'Gray -3', 'White', 'Other');
  INextLineColorTranslated:    array[0..9] of UTF8String = ('Black', 'Gray +3', 'Gray +2', 'Gray +1', 'Gray', 'Gray -1', 'Gray -2', 'Gray -3', 'White', 'Other');
  ISingLineOColorTranslated:   array[0..2] of UTF8String = ('Black', 'White', 'Other');
  IActualLineOColorTranslated: array[0..2] of UTF8String = ('Black', 'White', 'Other');
  INextLineOColorTranslated:   array[0..2] of UTF8String = ('Black', 'White', 'Other');

  // Advanced
  ILoadAnimationTranslated:    array[0..1] of UTF8String = ('Off', 'On');
  IEffectSingTranslated:       array[0..1] of UTF8String = ('Off', 'On');
  IScreenFadeTranslated:       array[0..1] of UTF8String = ('Off', 'On');
  IAskbeforeDelTranslated:     array[0..1] of UTF8String = ('Off', 'On');
  IOnSongClickTranslated:      array[0..2] of UTF8String = ('Sing', 'Select Players', 'Open Menu');
  ILineBonusTranslated:        array[0..1] of UTF8String = ('Off', 'On');
  IPartyPopupTranslated:       array[0..1] of UTF8String = ('Off', 'On');
  ISingScoresTranslated:       array[0..1] of UTF8String = ('Off', 'On');
  ITopScoresTranslated:        array[0..1] of UTF8String = ('All', 'Player');

  IJoypadTranslated:           array[0..1] of UTF8String = ('Off', 'On');
  IMouseTranslated:            array[0..2] of UTF8String = ('Off', 'Hardware Cursor', 'Software Cursor');

  ISingTimebarModeTranslated:      array[0..2] of UTF8String = ('Current', 'Remaining', 'Total');
  IJukeboxTimebarModeTranslated:      array[0..2] of UTF8String = ('Current', 'Remaining', 'Total');

  // Recording options
  IChannelPlayerTranslated:    array[0..6] of UTF8String = ('Off', '1', '2', '3', '4', '5', '6');
  IMicBoostTranslated:         array[0..3] of UTF8String = ('Off', '+6dB', '+12dB', '+18dB');

  // Network
  ISendNameTranslated:        array[0..1] of UTF8String = ('Off', 'On');
  IAutoModeTranslated:        array[0..2] of UTF8String = ('Off', 'Send', 'Guardar');
  IAutoPlayerTranslated:      array[0..6] of UTF8String = ('Player 1', 'Player 2', 'Player 3', 'Player 4', 'Player 5', 'Player 6', 'All');
  IAutoScoreEasyTranslated:   array of UTF8String;
  IAutoScoreMediumTranslated: array of UTF8String;
  IAutoScoreHardTranslated:   array of UTF8String;

  // Webcam
  IWebcamFlipTranslated:      array[0..1] of UTF8String = ('Off', 'On');
  IWebcamBrightness: array [0..200] of UTF8String;
  IWebcamSaturation: array [0..200] of UTF8String;
  IWebcamHue:        array [0..360] of UTF8String;
  IWebcamEffectTranslated:     array [0..10] of UTF8String;

  // Name
  IPlayerTranslated:      array[0..5] of UTF8String = ('Player 1', 'Player 2', 'Player 3', 'Player 4', 'Player 5', 'Player 6');

  IRed:       array[0..255] of UTF8String;
  IGreen:     array[0..255] of UTF8String;
  IBlue:      array[0..255] of UTF8String;


implementation

uses
  StrUtils,
  SDL,
  UCommandLine,
  UDataBase,
  UDllManager,
  ULanguage,
  UPlatform,
  UMain,
  URecord,
  USkins,
  UThemes,
  UPathUtils,
  UUnicodeUtils;

(**
 * Translate and set the values of options, which need translation.
 *)
procedure TIni.TranslateOptionValues;
var
  I: integer;
  Zeros: string;
begin
  // Load Languagefile
  if (Params.Language <> -1) then
    ULanguage.Language.ChangeLanguage(ILanguage[Params.Language])
  else
    ULanguage.Language.ChangeLanguage(ILanguage[Ini.Language]);

  SetLength(ILanguageTranslated, Length(ILanguage));
  for I := 0 to High(ILanguage) do
  begin
    ILanguageTranslated[I] := ULanguage.Language.Translate(
      'OPTION_VALUE_' + UpperCase(ILanguage[I])
    );
  end;

  IDifficultyTranslated[0]            := ULanguage.Language.Translate('OPTION_VALUE_EASY');
  IDifficultyTranslated[1]            := ULanguage.Language.Translate('OPTION_VALUE_MEDIUM');
  IDifficultyTranslated[2]            := ULanguage.Language.Translate('OPTION_VALUE_HARD');

  ITabsTranslated[0]                  := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  ITabsTranslated[1]                  := ULanguage.Language.Translate('OPTION_VALUE_ON');

  ISongMenuTranslated[0]              := ULanguage.Language.Translate('OPTION_VALUE_ROULETTE');
  ISongMenuTranslated[1]              := ULanguage.Language.Translate('OPTION_VALUE_CHESSBOARD');
  ISongMenuTranslated[2]              := ULanguage.Language.Translate('OPTION_VALUE_CAROUSEL');
  ISongMenuTranslated[3]              := ULanguage.Language.Translate('OPTION_VALUE_SLOTMACHINE');
  ISongMenuTranslated[4]              := ULanguage.Language.Translate('OPTION_VALUE_SLIDE');
  ISongMenuTranslated[5]              := ULanguage.Language.Translate('OPTION_VALUE_LIST');
  ISongMenuTranslated[6]              := ULanguage.Language.Translate('OPTION_VALUE_MOSAIC');

  ISortingTranslated[0]               := ULanguage.Language.Translate('OPTION_VALUE_EDITION');
  ISortingTranslated[1]               := ULanguage.Language.Translate('OPTION_VALUE_GENRE');
  ISortingTranslated[2]               := ULanguage.Language.Translate('OPTION_VALUE_LANGUAGE');
  ISortingTranslated[3]               := ULanguage.Language.Translate('OPTION_VALUE_FOLDER');
  ISortingTranslated[4]               := ULanguage.Language.Translate('OPTION_VALUE_TITLE');
  ISortingTranslated[5]               := ULanguage.Language.Translate('OPTION_VALUE_ARTIST');
  ISortingTranslated[6]               := ULanguage.Language.Translate('OPTION_VALUE_ARTIST2');
  ISortingTranslated[7]               := ULanguage.Language.Translate('OPTION_VALUE_YEAR');
  ISortingTranslated[8]               := ULanguage.Language.Translate('OPTION_VALUE_DECADE');
  //ISortingTranslated[9]               := ULanguage.Language.Translate('OPTION_VALUE_PLAYLIST');

  IShowScoresTranslated[0]            := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IShowScoresTranslated[1]            := ULanguage.Language.Translate('OPTION_VALUE_WHENEXIST');
  IShowScoresTranslated[2]            := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IDebugTranslated[0]                 := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IDebugTranslated[1]                 := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IFullScreenTranslated[0]            := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IFullScreenTranslated[1]            := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IVisualizerTranslated[0]            := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IVisualizerTranslated[1]            := ULanguage.Language.Translate('OPTION_VALUE_WHENNOVIDEO');
  IVisualizerTranslated[2]            := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IBackgroundMusicTranslated[0]       := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IBackgroundMusicTranslated[1]       := ULanguage.Language.Translate('OPTION_VALUE_ON');

  ISingWindowTranslated[0]            := ULanguage.Language.Translate('OPTION_VALUE_SMALL');
  ISingWindowTranslated[1]            := ULanguage.Language.Translate('OPTION_VALUE_BIG');

  IOscilloscopeTranslated[0]          := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IOscilloscopeTranslated[1]          := ULanguage.Language.Translate('OPTION_VALUE_ON');

  ISpectrumTranslated[0]              := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  ISpectrumTranslated[1]              := ULanguage.Language.Translate('OPTION_VALUE_ON');

  ISpectrographTranslated[0]          := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  ISpectrographTranslated[1]          := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IMovieSizeTranslated[0]             := ULanguage.Language.Translate('OPTION_VALUE_HALF');
  IMovieSizeTranslated[1]             := ULanguage.Language.Translate('OPTION_VALUE_FULL_VID');
  IMovieSizeTranslated[2]             := ULanguage.Language.Translate('OPTION_VALUE_FULL_VID_BG');

  IVideoPreviewTranslated[0]          := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IVideoPreviewTranslated[1]          := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IVideoEnabledTranslated[0]          := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IVideoEnabledTranslated[1]          := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IClickAssistTranslated[0]           := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IClickAssistTranslated[1]           := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IBeatClickTranslated[0]             := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IBeatClickTranslated[1]             := ULanguage.Language.Translate('OPTION_VALUE_ON');

  ISavePlaybackTranslated[0]          := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  ISavePlaybackTranslated[1]          := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IVoicePassthroughTranslated[0]      := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IVoicePassthroughTranslated[1]      := ULanguage.Language.Translate('OPTION_VALUE_ON');

  ISyncToTranslated[Ord(stMusic)]     := ULanguage.Language.Translate('OPTION_VALUE_MUSIC');
  ISyncToTranslated[Ord(stLyrics)]    := ULanguage.Language.Translate('OPTION_VALUE_LYRICS');
  ISyncToTranslated[Ord(stOff)]       := ULanguage.Language.Translate('OPTION_VALUE_OFF');

  ILyricsFontTranslated[0]            := ULanguage.Language.Translate('OPTION_VALUE_PLAIN');
  ILyricsFontTranslated[1]            := ULanguage.Language.Translate('OPTION_VALUE_OLINE1');
  ILyricsFontTranslated[2]            := ULanguage.Language.Translate('OPTION_VALUE_OLINE2');

  ILyricsEffectTranslated[0]          := ULanguage.Language.Translate('OPTION_VALUE_SIMPLE');
  ILyricsEffectTranslated[1]          := ULanguage.Language.Translate('OPTION_VALUE_ZOOM');
  ILyricsEffectTranslated[2]          := ULanguage.Language.Translate('OPTION_VALUE_SLIDE');
  ILyricsEffectTranslated[3]          := ULanguage.Language.Translate('OPTION_VALUE_BALL');
  ILyricsEffectTranslated[4]          := ULanguage.Language.Translate('OPTION_VALUE_SHIFT');

  INoteLinesTranslated[0]             := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  INoteLinesTranslated[1]             := ULanguage.Language.Translate('OPTION_VALUE_ON');

  for I := 0 to 255 do
  begin
    IRed[I]   := IntToStr(I);
    IGreen[I] := IntToStr(I);
    IBlue[I]  := IntToStr(I);
  end;

  ILineTranslated[0] := ULanguage.Language.Translate('OPTION_VALUE_TO_SING');
  ILineTranslated[1] := ULanguage.Language.Translate('OPTION_VALUE_ACTUAL');
  ILineTranslated[2] := ULanguage.Language.Translate('OPTION_VALUE_NEXT');

  IPropertyTranslated[0] := ULanguage.Language.Translate('OPTION_VALUE_FILL');
  IPropertyTranslated[1] := ULanguage.Language.Translate('OPTION_VALUE_OUTLINE');

  ISingLineColorTranslated[0] := ULanguage.Language.Translate('OPTION_VALUE_BLUE');
  ISingLineColorTranslated[1] := ULanguage.Language.Translate('OPTION_VALUE_GREEN');
  ISingLineColorTranslated[2] := ULanguage.Language.Translate('OPTION_VALUE_PINK');
  ISingLineColorTranslated[3] := ULanguage.Language.Translate('OPTION_VALUE_RED');
  ISingLineColorTranslated[4] := ULanguage.Language.Translate('OPTION_VALUE_VIOLET');
  ISingLineColorTranslated[5] := ULanguage.Language.Translate('OPTION_VALUE_ORANGE');
  ISingLineColorTranslated[6] := ULanguage.Language.Translate('OPTION_VALUE_YELLOW');
  ISingLineColorTranslated[7] := ULanguage.Language.Translate('OPTION_VALUE_BROWN');
  ISingLineColorTranslated[8] := ULanguage.Language.Translate('OPTION_VALUE_BLACK');
  ISingLineColorTranslated[9] := ULanguage.Language.Translate('OPTION_VALUE_TURQUOISE');
  ISingLineColorTranslated[10] := ULanguage.Language.Translate('OPTION_VALUE_SALMON');
  ISingLineColorTranslated[11] := ULanguage.Language.Translate('OPTION_VALUE_GREENYELLOW');
  ISingLineColorTranslated[12] := ULanguage.Language.Translate('OPTION_VALUE_LAVENDER');
  ISingLineColorTranslated[13] := ULanguage.Language.Translate('OPTION_VALUE_BEIGE');
  ISingLineColorTranslated[14] := ULanguage.Language.Translate('OPTION_VALUE_TEAL');
  ISingLineColorTranslated[15] := ULanguage.Language.Translate('OPTION_VALUE_ORCHID');
  ISingLineColorTranslated[16] := ULanguage.Language.Translate('OPTION_VALUE_STEELBLUE');
  ISingLineColorTranslated[17] := ULanguage.Language.Translate('OPTION_VALUE_PLUM');
  ISingLineColorTranslated[18] := ULanguage.Language.Translate('OPTION_VALUE_CHOCOLATE');
  ISingLineColorTranslated[19] := ULanguage.Language.Translate('OPTION_VALUE_GOLD');
  ISingLineColorTranslated[20] := ULanguage.Language.Translate('OPTION_VALUE_OTHER');

  IActualLineColorTranslated[0] := ULanguage.Language.Translate('OPTION_VALUE_BLACK');
  IActualLineColorTranslated[1] := ULanguage.Language.Translate('OPTION_VALUE_GRAY') + ' +3';
  IActualLineColorTranslated[2] := ULanguage.Language.Translate('OPTION_VALUE_GRAY') + ' +2';
  IActualLineColorTranslated[3] := ULanguage.Language.Translate('OPTION_VALUE_GRAY') + ' +1';
  IActualLineColorTranslated[4] := ULanguage.Language.Translate('OPTION_VALUE_GRAY');
  IActualLineColorTranslated[5] := ULanguage.Language.Translate('OPTION_VALUE_GRAY') + ' -1';
  IActualLineColorTranslated[6] := ULanguage.Language.Translate('OPTION_VALUE_GRAY') + ' -2';
  IActualLineColorTranslated[7] := ULanguage.Language.Translate('OPTION_VALUE_GRAY') + ' -3';
  IActualLineColorTranslated[8] := ULanguage.Language.Translate('OPTION_VALUE_WHITE');
  IActualLineColorTranslated[9] := ULanguage.Language.Translate('OPTION_VALUE_OTHER');

  INextLineColorTranslated[0] := ULanguage.Language.Translate('OPTION_VALUE_BLACK');
  INextLineColorTranslated[1] := ULanguage.Language.Translate('OPTION_VALUE_GRAY') + ' +3';
  INextLineColorTranslated[2] := ULanguage.Language.Translate('OPTION_VALUE_GRAY') + ' +2';
  INextLineColorTranslated[3] := ULanguage.Language.Translate('OPTION_VALUE_GRAY') + ' +1';
  INextLineColorTranslated[4] := ULanguage.Language.Translate('OPTION_VALUE_GRAY');
  INextLineColorTranslated[5] := ULanguage.Language.Translate('OPTION_VALUE_GRAY') + ' -1';
  INextLineColorTranslated[6] := ULanguage.Language.Translate('OPTION_VALUE_GRAY') + ' -2';
  INextLineColorTranslated[7] := ULanguage.Language.Translate('OPTION_VALUE_GRAY') + ' -3';
  INextLineColorTranslated[8] := ULanguage.Language.Translate('OPTION_VALUE_WHITE');
  INextLineColorTranslated[9] := ULanguage.Language.Translate('OPTION_VALUE_OTHER');

  ISingLineOColorTranslated[0] := ULanguage.Language.Translate('OPTION_VALUE_BLACK');
  ISingLineOColorTranslated[1] := ULanguage.Language.Translate('OPTION_VALUE_WHITE');
  ISingLineOColorTranslated[2] := ULanguage.Language.Translate('OPTION_VALUE_OTHER');

  IActualLineOColorTranslated[0] := ULanguage.Language.Translate('OPTION_VALUE_BLACK');
  IActualLineOColorTranslated[1] := ULanguage.Language.Translate('OPTION_VALUE_WHITE');
  IActualLineOColorTranslated[2] := ULanguage.Language.Translate('OPTION_VALUE_OTHER');

  INextLineOColorTranslated[0] := ULanguage.Language.Translate('OPTION_VALUE_BLACK');
  INextLineOColorTranslated[1] := ULanguage.Language.Translate('OPTION_VALUE_WHITE');
  INextLineOColorTranslated[2] := ULanguage.Language.Translate('OPTION_VALUE_OTHER');

  IColorTranslated[0] := ULanguage.Language.Translate('OPTION_VALUE_BLUE');
  IColorTranslated[1] := ULanguage.Language.Translate('OPTION_VALUE_GREEN');
  IColorTranslated[2] := ULanguage.Language.Translate('OPTION_VALUE_PINK');
  IColorTranslated[3] := ULanguage.Language.Translate('OPTION_VALUE_RED');
  IColorTranslated[4] := ULanguage.Language.Translate('OPTION_VALUE_VIOLET');
  IColorTranslated[5] := ULanguage.Language.Translate('OPTION_VALUE_ORANGE');
  IColorTranslated[6] := ULanguage.Language.Translate('OPTION_VALUE_YELLOW');
  IColorTranslated[7] := ULanguage.Language.Translate('OPTION_VALUE_BROWN');
  IColorTranslated[8] := ULanguage.Language.Translate('OPTION_VALUE_BLACK');

  IPlayerColorTranslated[0] := ULanguage.Language.Translate('OPTION_VALUE_BLUE');
  IPlayerColorTranslated[1] := ULanguage.Language.Translate('OPTION_VALUE_RED');
  IPlayerColorTranslated[2] := ULanguage.Language.Translate('OPTION_VALUE_GREEN');
  IPlayerColorTranslated[3] := ULanguage.Language.Translate('OPTION_VALUE_YELLOW');
  IPlayerColorTranslated[4] := ULanguage.Language.Translate('OPTION_VALUE_ORANGE');
  IPlayerColorTranslated[5] := ULanguage.Language.Translate('OPTION_VALUE_PINK');
  IPlayerColorTranslated[6] := ULanguage.Language.Translate('OPTION_VALUE_VIOLET');
  IPlayerColorTranslated[7] := ULanguage.Language.Translate('OPTION_VALUE_BROWN');
  IPlayerColorTranslated[8] := ULanguage.Language.Translate('OPTION_VALUE_GRAY');
  IPlayerColorTranslated[9] := ULanguage.Language.Translate('OPTION_VALUE_DARKBLUE');
  IPlayerColorTranslated[10] := ULanguage.Language.Translate('OPTION_VALUE_SKY');
  IPlayerColorTranslated[11] := ULanguage.Language.Translate('OPTION_VALUE_CYAN');
  IPlayerColorTranslated[12] := ULanguage.Language.Translate('OPTION_VALUE_FLAME');
  IPlayerColorTranslated[13] := ULanguage.Language.Translate('OPTION_VALUE_ORCHID');
  IPlayerColorTranslated[14] := ULanguage.Language.Translate('OPTION_VALUE_HARLEQUIN');
  IPlayerColorTranslated[15] := ULanguage.Language.Translate('OPTION_VALUE_GREENYELLOW');

  // Advanced
  ILoadAnimationTranslated[0]         := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  ILoadAnimationTranslated[1]         := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IEffectSingTranslated[0]            := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IEffectSingTranslated[1]            := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IScreenFadeTranslated[0]            := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IScreenFadeTranslated[1]            := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IAskbeforeDelTranslated[0]          := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IAskbeforeDelTranslated[1]          := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IOnSongClickTranslated[0]           := ULanguage.Language.Translate('OPTION_VALUE_SING');
  IOnSongClickTranslated[1]           := ULanguage.Language.Translate('OPTION_VALUE_SELECT_PLAYERS');
  IOnSongClickTranslated[2]           := ULanguage.Language.Translate('OPTION_VALUE_OPEN_MENU');

  ILineBonusTranslated[0]             := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  ILineBonusTranslated[1]             := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IPartyPopupTranslated[0]            := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IPartyPopupTranslated[1]            := ULanguage.Language.Translate('OPTION_VALUE_ON');

  ISingScoresTranslated[0]          := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  ISingScoresTranslated[1]          := ULanguage.Language.Translate('OPTION_VALUE_ON');

  ITopScoresTranslated[0]          := ULanguage.Language.Translate('OPTION_VALUE_ALL');
  ITopScoresTranslated[1]          := ULanguage.Language.Translate('OPTION_VALUE_PLAYER');

  IJoypadTranslated[0]                := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IJoypadTranslated[1]                := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IMouseTranslated[0]                 := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IMouseTranslated[1]                 := ULanguage.Language.Translate('OPTION_VALUE_HARDWARE_CURSOR');
  IMouseTranslated[2]                 := ULanguage.Language.Translate('OPTION_VALUE_SOFTWARE_CURSOR');

  ISingTimebarModeTranslated[0]          := ULanguage.Language.Translate('OPTION_VALUE_CURRENT');
  ISingTimebarModeTranslated[1]          := ULanguage.Language.Translate('OPTION_VALUE_REMAINING');
  ISingTimebarModeTranslated[2]          := ULanguage.Language.Translate('OPTION_VALUE_TOTAL');

  IJukeboxTimebarModeTranslated[0]          := ULanguage.Language.Translate('OPTION_VALUE_CURRENT');
  IJukeboxTimebarModeTranslated[1]          := ULanguage.Language.Translate('OPTION_VALUE_REMAINING');
  IJukeboxTimebarModeTranslated[2]          := ULanguage.Language.Translate('OPTION_VALUE_TOTAL');

  IAudioOutputBufferSizeTranslated[0] := ULanguage.Language.Translate('OPTION_VALUE_AUTO');
  IAudioOutputBufferSizeTranslated[1] := '256';
  IAudioOutputBufferSizeTranslated[2] := '512';
  IAudioOutputBufferSizeTranslated[3] := '1024';
  IAudioOutputBufferSizeTranslated[4] := '2048';
  IAudioOutputBufferSizeTranslated[5] := '4096';
  IAudioOutputBufferSizeTranslated[6] := '8192';
  IAudioOutputBufferSizeTranslated[7] := '16384';
  IAudioOutputBufferSizeTranslated[8] := '32768';
  IAudioOutputBufferSizeTranslated[9] := '65536';


  IAudioInputBufferSizeTranslated[0]  := ULanguage.Language.Translate('OPTION_VALUE_AUTO');
  IAudioInputBufferSizeTranslated[1]  := '256';
  IAudioInputBufferSizeTranslated[2]  := '512';
  IAudioInputBufferSizeTranslated[3]  := '1024';
  IAudioInputBufferSizeTranslated[4]  := '2048';
  IAudioInputBufferSizeTranslated[5]  := '4096';
  IAudioInputBufferSizeTranslated[6]  := '8192';
  IAudioInputBufferSizeTranslated[7]  := '16384';
  IAudioInputBufferSizeTranslated[8]  := '32768';
  IAudioInputBufferSizeTranslated[9]  := '65536';

  //Song Preview
  IPreviewVolumeTranslated[0]         := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IPreviewVolumeTranslated[1]         := '10%';
  IPreviewVolumeTranslated[2]         := '20%';
  IPreviewVolumeTranslated[3]         := '30%';
  IPreviewVolumeTranslated[4]         := '40%';
  IPreviewVolumeTranslated[5]         := '50%';
  IPreviewVolumeTranslated[6]         := '60%';
  IPreviewVolumeTranslated[7]         := '70%';
  IPreviewVolumeTranslated[8]         := '80%';
  IPreviewVolumeTranslated[9]         := '90%';
  IPreviewVolumeTranslated[10]        := '100%';


  IPreviewFadingTranslated[0]         :=        ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IPreviewFadingTranslated[1]         := '1 ' + ULanguage.Language.Translate('OPTION_VALUE_SEC');
  IPreviewFadingTranslated[2]         := '2 ' + ULanguage.Language.Translate('OPTION_VALUE_SECS');
  IPreviewFadingTranslated[3]         := '3 ' + ULanguage.Language.Translate('OPTION_VALUE_SECS');
  IPreviewFadingTranslated[4]         := '4 ' + ULanguage.Language.Translate('OPTION_VALUE_SECS');
  IPreviewFadingTranslated[5]         := '5 ' + ULanguage.Language.Translate('OPTION_VALUE_SECS');

  // Recording options
  IChannelPlayerTranslated[0]         := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IChannelPlayerTranslated[1]         := '1';
  IChannelPlayerTranslated[2]         := '2';
  IChannelPlayerTranslated[3]         := '3';
  IChannelPlayerTranslated[4]         := '4';
  IChannelPlayerTranslated[5]         := '5';
  IChannelPlayerTranslated[6]         := '6';

  IMicBoostTranslated[0]              := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IMicBoostTranslated[1]              := '+6dB';
  IMicBoostTranslated[2]              := '+12dB';
  IMicBoostTranslated[3]              := '+18dB';

  // Network
  IAutoModeTranslated[0]         := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IAutoModeTranslated[1]         := ULanguage.Language.Translate('OPTION_VALUE_SEND');
  IAutoModeTranslated[2]         := ULanguage.Language.Translate('OPTION_VALUE_SAVE');

  IAutoPlayerTranslated[0]         := ULanguage.Language.Translate('OPTION_PLAYER_1');
  IAutoPlayerTranslated[1]         := ULanguage.Language.Translate('OPTION_PLAYER_2');
  IAutoPlayerTranslated[2]         := ULanguage.Language.Translate('OPTION_PLAYER_3');
  IAutoPlayerTranslated[3]         := ULanguage.Language.Translate('OPTION_PLAYER_4');
  IAutoPlayerTranslated[4]         := ULanguage.Language.Translate('OPTION_PLAYER_5');
  IAutoPlayerTranslated[5]         := ULanguage.Language.Translate('OPTION_PLAYER_6');
  IAutoPlayerTranslated[6]         := ULanguage.Language.Translate('OPTION_ALL_PLAYERS');

  // Webcam
  IWebcamFlipTranslated[0]          := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IWebcamFlipTranslated[1]          := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IWebcamEffectTranslated[0] := ULanguage.Language.Translate('SING_OPTIONS_WEBCAM_EFFECT_NORMAL');
  IWebcamEffectTranslated[1] := ULanguage.Language.Translate('SING_OPTIONS_WEBCAM_EFFECT_GRAYSCALE');
  IWebcamEffectTranslated[2] := ULanguage.Language.Translate('SING_OPTIONS_WEBCAM_EFFECT_BLACK_WHITE');
  IWebcamEffectTranslated[3] := ULanguage.Language.Translate('SING_OPTIONS_WEBCAM_EFFECT_NEGATIVE');
  IWebcamEffectTranslated[4] := ULanguage.Language.Translate('SING_OPTIONS_WEBCAM_EFFECT_BINARY_IMAGE');
  IWebcamEffectTranslated[5] := ULanguage.Language.Translate('SING_OPTIONS_WEBCAM_EFFECT_DILATE');
  IWebcamEffectTranslated[6] := ULanguage.Language.Translate('SING_OPTIONS_WEBCAM_EFFECT_THRESHOLD');
  IWebcamEffectTranslated[7] := ULanguage.Language.Translate('SING_OPTIONS_WEBCAM_EFFECT_EDGES');
  IWebcamEffectTranslated[8] := ULanguage.Language.Translate('SING_OPTIONS_WEBCAM_EFFECT_GAUSSIAN_BLUR');
  IWebcamEffectTranslated[9] := ULanguage.Language.Translate('SING_OPTIONS_WEBCAM_EFFECT_EQUALIZED');
  IWebcamEffectTranslated[10] := ULanguage.Language.Translate('SING_OPTIONS_WEBCAM_EFFECT_ERODE');

  SetLength(IAutoScoreEasyTranslated, 10000);
  SetLength(IAutoScoreMediumTranslated, 10000);
  SetLength(IAutoScoreHardTranslated, 10000);

  for I := 0 to 9999 do
  begin
    case (I) of
      0..9 : Zeros := '000';
      10..99 : Zeros := '00';
      100..999 : Zeros := '0';
      1000..9999 : Zeros := '';
    end;

    IAutoScoreEasyTranslated[I]   := '+' + Zeros + IntToStr(I);
    IAutoScoreMediumTranslated[I] := '+' + Zeros + IntToStr(I);
    IAutoScoreHardTranslated[I]   := '+' + Zeros + IntToStr(I);
  end;

end;

procedure TIni.LoadWebcamSettings(IniFile: TCustomIniFile);
var
  I: integer;
begin
  for I:= 100 downto 1 do
  begin
    IWebcamBrightness[100 - I]   := '-' + IntToStr(I);
    IWebcamSaturation[100 - I]   := '-' + IntToStr(I);
  end;

  IWebcamBrightness[100]   := '0';
  IWebcamSaturation[100]   := '0';

  for I:= 1 to 100 do
  begin
    IWebcamBrightness[I + 100]   := '+' + IntToStr(I);
    IWebcamSaturation[I + 100]   := '+' + IntToStr(I);
  end;

  for I:= 180 downto 1 do
    IWebcamHue[180 - I]   := '-' + IntToStr(I);

  IWebcamHue[180]   := '0';

  for I:= 1 to 180 do
    IWebcamHue[I + 180]   := '+' + IntToStr(I);

end;

(**
 * Extracts an index of a key that is surrounded by a Prefix/Suffix pair.
 * Example: ExtractKeyIndex('MyKey[1]', '[', ']') will return 1.
 *)
function TIni.ExtractKeyIndex(const Key, Prefix, Suffix: string): integer;
var
  Value: string;
  Start: integer;
  PrefixPos, SuffixPos: integer;
begin
  Result := -1;

  PrefixPos := Pos(Prefix, Key);
  if (PrefixPos <= 0) then
    Exit;
  SuffixPos := Pos(Suffix, Key);
  if (SuffixPos <= 0) then
    Exit;

  Start := PrefixPos + Length(Prefix);

  // copy all between prefix and suffix
  Value  := Copy(Key, Start, SuffixPos - Start);
  Result := StrToIntDef(Value, -1);
end;

(**
 * Finds the maximum key-index in a key-list.
 * The indexes of the list are surrounded by Prefix/Suffix,
 * e.g. MyKey[1] (Prefix='[', Suffix=']')
 *)
function TIni.GetMaxKeyIndex(Keys: TStringList; const Prefix, Suffix: string): integer;
var
  i:        integer;
  KeyIndex: integer;
begin
  Result := -1;

  for i := 0 to Keys.Count-1 do
  begin
    KeyIndex := ExtractKeyIndex(Keys[i], Prefix, Suffix);
    if (KeyIndex > Result) then
      Result := KeyIndex;
  end;
end;

(**
 * Reads the property IniSeaction:IniProperty from IniFile and
 * finds its corresponding index in SearchArray.
 * If SearchArray does not contain the property value, the default value is
 * returned.
 *)
function TIni.ReadArrayIndex(const SearchArray: array of UTF8String; IniFile: TCustomIniFile;
    IniSection: string; IniProperty: string; Default: integer): integer;
var
  StrValue: string;
begin
  StrValue := IniFile.ReadString(IniSection, IniProperty, SearchArray[Default]);
  Result := GetArrayIndex(SearchArray, StrValue);
  if (Result = -1) then
  begin
    Result := Default;
  end;
end;

procedure TIni.LoadInputDeviceCfg(IniFile: TMemIniFile);
var
  DeviceCfg:    PInputDeviceConfig;
  DeviceIndex:  integer;
  ChannelCount: integer;
  ChannelIndex: integer;
  RecordKeys:   TStringList;
  i:            integer;
begin
  RecordKeys := TStringList.Create();

  // read all record-keys for filtering
  IniFile.ReadSection('Record', RecordKeys);

  SetLength(InputDeviceConfig, 0);

  for i := 0 to RecordKeys.Count-1 do
  begin
    // find next device-name
    DeviceIndex := ExtractKeyIndex(RecordKeys[i], 'DeviceName[', ']');
    if (DeviceIndex >= 0) then
    begin
      if not IniFile.ValueExists('Record', Format('DeviceName[%d]', [DeviceIndex])) then
        Continue;

      // resize list
      SetLength(InputDeviceConfig, Length(InputDeviceConfig)+1);

      // read an input device's config.
      // Note: All devices are appended to the list whether they exist or not.
      //   Otherwise an external device's config will be lost if it is not
      //   connected (e.g. singstar mics or USB-Audio devices).
      DeviceCfg := @InputDeviceConfig[High(InputDeviceConfig)];
      DeviceCfg.Name := IniFile.ReadString('Record', Format('DeviceName[%d]', [DeviceIndex]), '');
      DeviceCfg.Input := IniFile.ReadInteger('Record', Format('Input[%d]', [DeviceIndex]), 0);
      DeviceCfg.Latency := IniFile.ReadInteger('Record', Format('Latency[%d]', [DeviceIndex]), LATENCY_AUTODETECT);

      // find the largest channel-number of the current device in the ini-file
      ChannelCount := GetMaxKeyIndex(RecordKeys, 'Channel', Format('[%d]', [DeviceIndex]));
      if (ChannelCount < 0) then
        ChannelCount := 0;

      SetLength(DeviceCfg.ChannelToPlayerMap, ChannelCount);

      // read channel-to-player mapping for every channel of the current device
      // or set non-configured channels to no player (=0).
      for ChannelIndex := 0 to High(DeviceCfg.ChannelToPlayerMap) do
      begin
        DeviceCfg.ChannelToPlayerMap[ChannelIndex] :=
          IniFile.ReadInteger('Record', Format('Channel%d[%d]', [ChannelIndex+1, DeviceIndex]), CHANNEL_OFF);
      end;
    end;
  end;

  RecordKeys.Free();

  // MicBoost
  MicBoost := GetArrayIndex(IMicBoost, IniFile.ReadString('Record', 'MicBoost', 'Off'));
  // Threshold
  ThresholdIndex := GetArrayIndex(IThreshold, IniFile.ReadString('Record', 'Threshold', IThreshold[1]));
end;

procedure TIni.SaveInputDeviceCfg(IniFile: TIniFile);
var
  DeviceIndex:  integer;
  ChannelIndex: integer;
begin
  for DeviceIndex := 0 to High(InputDeviceConfig) do
  begin
    // DeviceName and DeviceInput
    IniFile.WriteString('Record', Format('DeviceName[%d]', [DeviceIndex+1]),
                        InputDeviceConfig[DeviceIndex].Name);
    IniFile.WriteInteger('Record', Format('Input[%d]', [DeviceIndex+1]),
                        InputDeviceConfig[DeviceIndex].Input);
    IniFile.WriteInteger('Record', Format('Latency[%d]', [DeviceIndex+1]),
                        InputDeviceConfig[DeviceIndex].Latency);

    // Channel-to-Player Mapping
    for ChannelIndex := 0 to High(InputDeviceConfig[DeviceIndex].ChannelToPlayerMap) do
    begin
      IniFile.WriteInteger('Record',
                          Format('Channel%d[%d]', [ChannelIndex+1, DeviceIndex+1]),
                          InputDeviceConfig[DeviceIndex].ChannelToPlayerMap[ChannelIndex]);
    end;
  end;

  // MicBoost
  IniFile.WriteString('Record', 'MicBoost', IMicBoost[MicBoost]);
  // Threshold
  IniFile.WriteString('Record', 'Threshold', IThreshold[ThresholdIndex]);
end;

procedure TIni.LoadPaths(IniFile: TCustomIniFile);
var
  PathStrings: TStringList;
  I:           integer;
begin
  PathStrings := TStringList.Create;
  IniFile.ReadSection('Directories', PathStrings);

  // Load song-paths
  for I := 0 to PathStrings.Count-1 do
  begin
    if (Pos('SONGDIR', UpperCase(PathStrings[I])) = 1) then
    begin
      AddSongPath(Path(IniFile.ReadString('Directories', PathStrings[I], '')));
    end;
  end;

  PathStrings.Free;
end;

procedure TIni.LoadThemes(IniFile: TCustomIniFile);
begin
  // No Theme Found
  if (Length(ITheme) = 0) then
  begin
    Log.CriticalError('Could not find any valid Themes.');
  end;

  Theme := GetArrayIndex(ITheme, IniFile.ReadString('Themes', 'Theme', 'DELUXE'), true);
  if (Theme = -1) then
    Theme := 0;

  // Skin
  Skin.onThemeChange;

  SkinNo := GetArrayIndex(ISkin, IniFile.ReadString('Themes',    'Skin',   ISkin[UThemes.Theme.Themes[Theme].DefaultSkin]));

  { there may be a not existing skin in the ini file
    e.g. due to manual edit or corrupted file.
    in this case we load the first Skin }
  if SkinNo = -1 then
    SkinNo := 0;

  // Color
  Color := GetArrayIndex(IColor, IniFile.ReadString('Themes',    'Color', IColor[Skin.GetDefaultColor(SkinNo)]));
end;

procedure TIni.LoadScreenModes(IniFile: TCustomIniFile);

  // swap two strings
  procedure swap(var s1, s2: UTF8String);
  var
    s3: string;
  begin
    s3 := s1;
    s1 := s2;
    s2 := s3;
  end;

var
  Modes: PPSDL_Rect;
  I:     integer;
begin
  MaxFramerate:= GetArrayIndex(IMaxFramerate, IniFile.ReadString('Graphics', 'MaxFramerate', '60'));
  MaxFramerateGet:= StrToInt(IMaxFramerate[MaxFramerate]);
  // Screens
  Screens := GetArrayIndex(IScreens, IniFile.ReadString('Graphics', 'Screens', IScreens[0]));

  // Split mode
  Split := GetArrayIndex(ISplit, IniFile.ReadString('Graphics', 'Split', ISplit[0]));

  // FullScreen
  FullScreen := GetArrayIndex(IFullScreen, IniFile.ReadString('Graphics', 'FullScreen', 'On'));

  // Resolution
  SetLength(IResolution, 0);

  // Check if there are any modes available
  // TODO: we should seperate windowed and fullscreen modes. Otherwise it is not
  // possible to select a reasonable fullscreen mode when in windowed mode
  if IFullScreen[FullScreen] = 'On' then
    Modes  := SDL_ListModes(nil, SDL_OPENGL or SDL_FULLSCREEN)
  else
    Modes  := SDL_ListModes(nil, SDL_OPENGL or SDL_RESIZABLE) ;

  if (Modes = nil) then
  begin
    Log.LogStatus( 'No resolutions Found' , 'Video');
  end
  else if (Modes = PPSDL_Rect(-1)) then
  begin
    // Fallback to some standard resolutions
    SetLength(IResolution, 18);
    IResolution[0] := '640x480';
    IResolution[1] := '800x600';
    IResolution[2] := '1024x768';
    IResolution[3] := '1152x666';;
    IResolution[4] := '1152x864';
    IResolution[5] := '1280x800';
    IResolution[6] := '1280x960';
    IResolution[7] := '1280x1024';
    IResolution[8] := '1366x768';
    IResolution[9] := '1400x1050';
    IResolution[10] := '1440x900';
    IResolution[11] := '1600x900';
    IResolution[12] := '1600x1200';
    IResolution[13] := '1680x1050';
    IResolution[14] := '1920x1080';
    IResolution[15] := '1920x1200';
    IResolution[16] := '2048x1152';
    IResolution[17] := '2560x1600';

    Resolution := GetArrayIndex(IResolution, IniFile.ReadString('Graphics', 'Resolution', '800x600'));
    if Resolution = -1 then
    begin
      SetLength(IResolution, Length(IResolution) + 1);
      IResolution[High(IResolution)] := IniFile.ReadString('Graphics', 'Resolution', '800x600');
      Resolution := High(IResolution);
    end;
  end
  else
  begin
    while assigned( Modes^ ) do //this should solve the biggest wine problem | THANKS Linnex (11.11.07)
    begin
      Log.LogStatus( 'Found Video Mode : ' + IntToStr(Modes^.w) + 'x' + IntToStr(Modes^.h) , 'Video');
      SetLength(IResolution, Length(IResolution) + 1);
      IResolution[High(IResolution)] := IntToStr(Modes^.w div (Screens+1)) + 'x' + IntToStr(Modes^.h);
      Inc(Modes);
    end;

    // reverse order
    Log.LogStatus( 'Log size of resolution: ' + IntToStr(Length(IResolution)), 'Video');
    for I := 0 to (Length(IResolution) div 2) - 1 do
    begin
      swap(IResolution[I], IResolution[High(IResolution)-I]);
    end;
    Resolution := GetArrayIndex(IResolution, IniFile.ReadString('Graphics', 'Resolution', '800x600'));

    if Resolution = -1 then
    begin
      Resolution := GetArrayIndex(IResolution, '800x600');
      if Resolution = -1 then
        Resolution := 0;
    end;
  end;

  // if no modes were set, then failback to 800x600
  // as per http://sourceforge.net/forum/message.php?msg_id=4544965
  // THANKS : linnex at users.sourceforge.net
  if Length(IResolution) < 1 then
  begin
    Log.LogStatus( 'Found Video Mode : NONE !!! ( Defaulted to 800 x 600 )', 'Video');
    SetLength(IResolution, 1);
    IResolution[0] := '800x600';
    Resolution := 0;
    Log.LogStatus('SDL_ListModes Defaulted Res To : ' + IResolution[0] , 'Graphics - Resolutions');

    // Default to fullscreen OFF, in this case !
    FullScreen := 0;
  end;

  // Depth
  Depth := GetArrayIndex(IDepth, IniFile.ReadString('Graphics', 'Depth', '32 bit'));
end;

procedure TIni.Load();
var
  IniFile: TMemIniFile;
  I:       integer;
  IShowWebScore: array of UTF8String;
  HexColor: string;
  Col: TRGB;
begin
  GamePath := Platform.GetGameUserPath;

  Log.LogStatus( 'GamePath : ' +GamePath.ToNative , '' );

  if (Params.ConfigFile.IsSet) then
    FileName := Params.ConfigFile
  else
    FileName := GamePath.Append('config.ini');

  Log.LogStatus('Using config : ' + FileName.ToNative, 'Ini');
  IniFile := TMemIniFile.Create(FileName.ToNative);

  // Name
  for I := 0 to 11 do
    Name[I] := IniFile.ReadString('Name', 'P'+IntToStr(I+1), 'Player'+IntToStr(I+1));

  // Color Player
  for I := 0 to 5 do
    PlayerColor[I] := IniFile.ReadInteger('PlayerColor', 'P'+IntToStr(I+1), I + 1);

  // Avatar Player
  for I := 0 to 5 do
    PlayerAvatar[I] := IniFile.ReadString('PlayerAvatar', 'P'+IntToStr(I+1), '');

  // Level Player
  for I := 0 to 5 do
    PlayerLevel[I] := IniFile.ReadInteger('PlayerLevel', 'P'+IntToStr(I+1), 0);

  // Color Team
  for I := 0 to 2 do
    TeamColor[I] := IniFile.ReadInteger('TeamColor', 'T'+IntToStr(I+1), I + 1);

  // Templates for Names Mod
  for I := 0 to 2 do
    NameTeam[I] := IniFile.ReadString('NameTeam', 'T'+IntToStr(I+1), 'Team'+IntToStr(I+1));
  for I := 0 to 11 do
    NameTemplate[I] := IniFile.ReadString('NameTemplate', 'Name'+IntToStr(I+1), 'Template'+IntToStr(I+1));

  // Players
  Players := GetArrayIndex(IPlayers, IniFile.ReadString('Game', 'Players', IPlayers[0]));

  // Difficulty
  Difficulty := GetArrayIndex(IDifficulty, IniFile.ReadString('Game', 'Difficulty', 'Easy'));

  // Language
  Language := GetArrayIndex(ILanguage, IniFile.ReadString('Game', 'Language', 'English'));

  // SongMenu
  SongMenu := GetArrayIndex(ISongMenuMode, IniFile.ReadString('Game', 'SongMenu', ISongMenuMode[Ord(smRoulette)]));

  // Tabs
  Tabs := GetArrayIndex(ITabs, IniFile.ReadString('Game', 'Tabs', ITabs[0]));
  TabsAtStartup := Tabs;	//Tabs at Startup fix

  // Song Sorting
  Sorting := GetArrayIndex(ISorting, IniFile.ReadString('Game', 'Sorting', ISorting[Ord(sEdition)]));

  // Show Score
  ShowScores := GetArrayIndex(IShowScores, IniFile.ReadString('Game', 'ShowScores', 'On'));

  // Read Users Info (Network)
  DataBase.ReadUsers;

  // Update Webs Scores
  DataBase.AddWebsite;

  // Webs Scores Path
  WebScoresPath := Path(IniFile.ReadString('Directories', 'WebScoresDir', WebsitePath.ToNative));
  if not(DirectoryExists(WebScoresPath.ToNative)) then
    WebScoresPath :=  WebsitePath;

  // ShowWebScore
  if (Length(DllMan.Websites) > 0) then
  begin
    SetLength(IShowWebScore, Length(DLLMan.Websites));
    for I:= 0 to High(DllMan.Websites) do
      IShowWebScore[I] := DllMan.Websites[I].Name;
    ShowWebScore := GetArrayIndex(IShowWebScore, IniFile.ReadString('Game', 'ShowWebScore', IShowWebScore[0]));
    if (ShowWebScore = -1) then
      ShowWebScore := 0;
  end;

  // Debug
  Debug := GetArrayIndex(IDebug, IniFile.ReadString('Game', 'Debug', IDebug[0]));

  LoadScreenModes(IniFile);

  LoadWebcamSettings(IniFile);

  // TextureSize (aka CachedCoverSize)
  TextureSize := GetArrayIndex(ITextureSize, IniFile.ReadString('Graphics', 'TextureSize', '256'));

  // SingWindow
  SingWindow := GetArrayIndex(ISingWindow, IniFile.ReadString('Graphics', 'SingWindow', 'Big'));

  // Oscilloscope
  Oscilloscope := GetArrayIndex(IOscilloscope, IniFile.ReadString('Graphics', 'Oscilloscope', IOscilloscope[0]));

  // Spectrum
  //Spectrum := GetArrayIndex(ISpectrum, IniFile.ReadString('Graphics', 'Spectrum', 'Off'));

  // Spectrograph
  //Spectrograph := GetArrayIndex(ISpectrograph, IniFile.ReadString('Graphics', 'Spectrograph', 'Off'));

  // MovieSize
  MovieSize := GetArrayIndex(IMovieSize, IniFile.ReadString('Graphics', 'MovieSize', IMovieSize[2]));

  // VideoPreview
  VideoPreview := GetArrayIndex(IVideoPreview, IniFile.ReadString('Graphics', 'VideoPreview', IVideoPreview[1]));

  // VideoEnabled
  VideoEnabled := GetArrayIndex(IVideoEnabled, IniFile.ReadString('Graphics', 'VideoEnabled', IVideoEnabled[1]));

  // ClickAssist
  ClickAssist := GetArrayIndex(IClickAssist, IniFile.ReadString('Sound', 'ClickAssist', 'Off'));

  // BeatClick
  BeatClick := GetArrayIndex(IBeatClick, IniFile.ReadString('Sound', 'BeatClick', IBeatClick[0]));

  // SavePlayback
  SavePlayback := GetArrayIndex(ISavePlayback, IniFile.ReadString('Sound', 'SavePlayback', ISavePlayback[0]));

  // AudioOutputBufferSize
  AudioOutputBufferSizeIndex := ReadArrayIndex(IAudioOutputBufferSize, IniFile, 'Sound', 'AudioOutputBufferSize', 0);

  //Preview Volume
  PreviewVolume := GetArrayIndex(IPreviewVolume, IniFile.ReadString('Sound', 'PreviewVolume', IPreviewVolume[7]));

  //Preview Fading
  PreviewFading := GetArrayIndex(IPreviewFading, IniFile.ReadString('Sound', 'PreviewFading', IPreviewFading[3]));

  //AudioRepeat aka VoicePassthrough
  VoicePassthrough := GetArrayIndex(IVoicePassthrough, IniFile.ReadString('Sound', 'VoicePassthrough', IVoicePassthrough[0]));

  SoundFont := IniFile.ReadString('Sound', 'SoundFont', '');

  // Lyrics Font
  LyricsFont := GetArrayIndex(ILyricsFont, IniFile.ReadString('Lyrics', 'LyricsFont', ILyricsFont[0]));

  // Lyrics Effect
  LyricsEffect := GetArrayIndex(ILyricsEffect, IniFile.ReadString('Lyrics', 'LyricsEffect', ILyricsEffect[4]));

  // NoteLines
  NoteLines := GetArrayIndex(INoteLines, IniFile.ReadString('Lyrics', 'NoteLines', INoteLines[1]));

  // DefaultEncoding
  DefaultEncoding := ParseEncoding(IniFile.ReadString('Lyrics', 'Encoding', ''), encAuto);

  LoadThemes(IniFile);

  LoadInputDeviceCfg(IniFile);

  // LoadAnimation
  LoadAnimation := GetArrayIndex(ILoadAnimation, IniFile.ReadString('Advanced', 'LoadAnimation', 'On'));

  // ScreenFade
  ScreenFade := GetArrayIndex(IScreenFade, IniFile.ReadString('Advanced', 'ScreenFade', 'On'));

  // Visualizations
  // <mog> this could be of use later..
  //  VisualizerOption :=
  //    TVisualizerOption(GetEnumValue(TypeInfo(TVisualizerOption),
  //            IniFile.ReadString('Graphics', 'Visualization', 'Off')));
  // || VisualizerOption := TVisualizerOption(GetArrayIndex(IVisualizer, IniFile.ReadString('Graphics', 'Visualization', 'Off')));
  VisualizerOption := GetArrayIndex(IVisualizer, IniFile.ReadString('Graphics', 'Visualization', 'Off'));

{**
 * Background music
 *}
  BackgroundMusicOption := GetArrayIndex(IBackgroundMusic, IniFile.ReadString('Sound', 'BackgroundMusic', 'On'));

  // EffectSing
  EffectSing := GetArrayIndex(IEffectSing, IniFile.ReadString('Advanced', 'EffectSing', 'On'));

  // AskbeforeDel
  AskBeforeDel := GetArrayIndex(IAskbeforeDel, IniFile.ReadString('Advanced', 'AskbeforeDel', 'On'));

  // OnSongClick
  OnSongClick := GetArrayIndex(IOnSongClick, IniFile.ReadString('Advanced', 'OnSongClick', 'Sing'));

  // Linebonus
  LineBonus := GetArrayIndex(ILineBonus, IniFile.ReadString('Advanced', 'LineBonus', ILineBonus[1]));

  // PartyPopup
  PartyPopup := GetArrayIndex(IPartyPopup, IniFile.ReadString('Advanced', 'PartyPopup', 'On'));

  // SingScores
  SingScores := GetArrayIndex(ISingScores, IniFile.ReadString('Advanced', 'SingScores', 'On'));

  // TopScores
  TopScores := GetArrayIndex(ITopScores, IniFile.ReadString('Advanced', 'TopScores', 'All'));

  // SyncTo
  SyncTo := GetArrayIndex(ISyncTo, IniFile.ReadString('Advanced', 'SyncTo', ISyncTo[Ord(stMusic)]));

  // Joypad
  Joypad := GetArrayIndex(IJoypad, IniFile.ReadString('Controller',    'Joypad',   IJoypad[0]));

  // Mouse
  Mouse := GetArrayIndex(IMouse, IniFile.ReadString('Controller',    'Mouse',   IMouse[2]));

  // SingTimebarMode
  SingTimebarMode := GetArrayIndex(ISingTimebarMode, IniFile.ReadString('Advanced', 'SingTimebarMode', 'Remaining'));

  // JukeboxTimebarMode
  JukeboxTimebarMode := GetArrayIndex(IJukeboxTimebarMode, IniFile.ReadString('Advanced', 'JukeboxTimebarMode', 'Current'));

  // WebCam
  WebCamID := IniFile.ReadInteger('Webcam', 'ID', 0);
  WebCamResolution := GetArrayIndex(IWebcamResolution, IniFile.ReadString('Webcam', 'Resolution', '320x240'));
  if (WebCamResolution = -1) then
    WebcamResolution := 2;
  WebCamFPS := GetArrayIndex(IWebcamFPS, IniFile.ReadString('Webcam', 'FPS', IWebcamFPS[4]));
  WebCamFlip := GetArrayIndex(IWebcamFlipTranslated, IniFile.ReadString('Webcam', 'Flip', 'On'));
  WebCamBrightness := GetArrayIndex(IWebcamBrightness, IniFile.ReadString('Webcam', 'Brightness', '0'));
  WebCamSaturation := GetArrayIndex(IWebcamSaturation, IniFile.ReadString('Webcam', 'Saturation', '0'));
  WebCamHue := GetArrayIndex(IWebcamHue, IniFile.ReadString('Webcam', 'Hue', '0'));
  WebCamEffect := IniFile.ReadInteger('Webcam', 'Effect', 0);

  // Jukebox
  JukeboxFont := GetArrayIndex(ILyricsFont, IniFile.ReadString('Jukebox', 'LyricsFont', ILyricsFont[2]));
  JukeboxEffect := GetArrayIndex(ILyricsEffect, IniFile.ReadString('Jukebox', 'LyricsEffect', ILyricsEffect[1]));
  JukeboxAlpha := GetArrayIndex(ILyricsAlpha, IniFile.ReadString('Jukebox', 'LyricsAlpha', ILyricsAlpha[20]));

  JukeboxSongMenu := GetArrayIndex(IJukeboxSongMenu, IniFile.ReadString('Jukebox', 'SongMenu', 'On'));


  JukeboxSingLineColor := GetArrayIndex(IHexSingColor, IniFile.ReadString('Jukebox', 'SingLineColor', IHexSingColor[0]));

  // other color
  if (JukeboxSingLineColor = -1) then
  begin
    JukeboxSingLineColor := High(IHexSingColor);

    HexColor := IniFile.ReadString('Jukebox', 'SingLineColor', IHexSingColor[0]);
    Col := HexToRGB(HexColor);

    Ini.JukeboxSingLineOtherColorR := Round(Col.R);
    Ini.JukeboxSingLineOtherColorG := Round(Col.G);
    Ini.JukeboxSingLineOtherColorB := Round(Col.B);
  end;

  JukeboxActualLineColor := GetArrayIndex(IHexGrayColor, IniFile.ReadString('Jukebox', 'ActualLineColor', IHexGrayColor[5]));

  // other color
  if (JukeboxActualLineColor = -1) then
  begin
    JukeboxActualLineColor := High(IHexGrayColor);

    HexColor := IniFile.ReadString('Jukebox', 'ActualLineColor', IHexGrayColor[5]);
    Col := HexToRGB(HexColor);

    Ini.JukeboxActualLineOtherColorR := Round(Col.R);
    Ini.JukeboxActualLineOtherColorG := Round(Col.G);
    Ini.JukeboxActualLineOtherColorB := Round(Col.B);
  end;

  JukeboxNextLineColor := GetArrayIndex(IHexGrayColor, IniFile.ReadString('Jukebox', 'NextLineColor', IHexGrayColor[3]));
  // other color
  if (JukeboxNextLineColor = -1) then
  begin
    JukeboxNextLineColor := High(IHexGrayColor);

    HexColor := IniFile.ReadString('Jukebox', 'NextLineColor', IHexGrayColor[3]);
    Col := HexToRGB(HexColor);

    Ini.JukeboxNextLineOtherColorR := Round(Col.R);
    Ini.JukeboxNextLineOtherColorG := Round(Col.G);
    Ini.JukeboxNextLineOtherColorB := Round(Col.B);
  end;

  JukeboxSingLineOutlineColor := GetArrayIndex(IHexOColor, IniFile.ReadString('Jukebox', 'SingLineOColor', IHexOColor[0]));
  // other color
  if (JukeboxSingLineOutlineColor = -1) then
  begin
    JukeboxSingLineOutlineColor := High(IHexOColor);

    HexColor := IniFile.ReadString('Jukebox', 'SingLineOColor', IHexOColor[0]);
    Col := HexToRGB(HexColor);

    Ini.JukeboxSingLineOtherOColorR := Round(Col.R);
    Ini.JukeboxSingLineOtherOColorG := Round(Col.G);
    Ini.JukeboxSingLineOtherOColorB := Round(Col.B);
  end;

  JukeboxActualLineOutlineColor := GetArrayIndex(IHexOColor, IniFile.ReadString('Jukebox', 'ActualLineOColor', IHexOColor[0]));
  // other color
  if (JukeboxActualLineOutlineColor = -1) then
  begin
    JukeboxActualLineOutlineColor := High(IHexOColor);

    HexColor := IniFile.ReadString('Jukebox', 'ActualLineOColor', IHexOColor[0]);
    Col := HexToRGB(HexColor);

    Ini.JukeboxActualLineOtherOColorR := Round(Col.R);
    Ini.JukeboxActualLineOtherOColorG := Round(Col.G);
    Ini.JukeboxActualLineOtherOColorB := Round(Col.B);
  end;

  JukeboxNextLineOutlineColor := GetArrayIndex(IHexOColor, IniFile.ReadString('Jukebox', 'NextLineOColor', IHexOColor[0]));
  // other color
  if (JukeboxNextLineOutlineColor = -1) then
  begin
    JukeboxNextLineOutlineColor := High(IHexOColor);

    HexColor := IniFile.ReadString('Jukebox', 'NextLineOColor', IHexOColor[0]);
    Col := HexToRGB(HexColor);

    Ini.JukeboxNextLineOtherOColorR := Round(Col.R);
    Ini.JukeboxNextLineOtherOColorG := Round(Col.G);
    Ini.JukeboxNextLineOtherOColorB := Round(Col.B);
  end;

  LoadPaths(IniFile);

  TranslateOptionValues;

  IniFile.Free;
end;

procedure TIni.Save;
var
  IniFile: TIniFile;
  HexColor: string;
  I: integer;
  C: TRGB;
begin
  if (Filename.IsFile and Filename.IsReadOnly) then
  begin
    Log.LogError('Config-file is read-only', 'TIni.Save');
    Exit;
  end;

  IniFile := TIniFile.Create(Filename.ToNative);

  // Players
  IniFile.WriteString('Game', 'Players', IPlayers[Players]);

  // Difficulty
  IniFile.WriteString('Game', 'Difficulty', IDifficulty[Difficulty]);

  // Language
  IniFile.WriteString('Game', 'Language', ILanguage[Language]);

  // Tabs
  IniFile.WriteString('Game', 'Tabs', ITabs[Tabs]);

  // SongMenu
  IniFile.WriteString('Game', 'SongMenu', ISongMenuMode[Ord(SongMenu)]);

  // Sorting
  IniFile.WriteString('Game', 'Sorting', ISorting[Sorting]);

  // Show Scores
  IniFile.WriteString('Game', 'ShowScores', IShowScores[ShowScores]);

  // Debug
  IniFile.WriteString('Game', 'Debug', IDebug[Debug]);

  // MaxFramerate
  IniFile.WriteString('Graphics', 'MaxFramerate', IMaxFramerate[MaxFramerate]);

  // Screens
  IniFile.WriteString('Graphics', 'Screens', IScreens[Screens]);

  // Split
  IniFile.WriteString('Graphics', 'Split', ISplit[Split]);

  // FullScreen
  IniFile.WriteString('Graphics', 'FullScreen', IFullScreen[FullScreen]);

  // Visualization
  IniFile.WriteString('Graphics', 'Visualization', IVisualizer[VisualizerOption]);

  // Resolution
  IniFile.WriteString('Graphics', 'Resolution', IResolution[Resolution]);

  // Depth
  IniFile.WriteString('Graphics', 'Depth', IDepth[Depth]);

  // TextureSize
  IniFile.WriteString('Graphics', 'TextureSize', ITextureSize[TextureSize]);

  // Sing Window
  IniFile.WriteString('Graphics', 'SingWindow', ISingWindow[SingWindow]);

  // Oscilloscope
  IniFile.WriteString('Graphics', 'Oscilloscope', IOscilloscope[Oscilloscope]);

  // Spectrum
  //IniFile.WriteString('Graphics', 'Spectrum', ISpectrum[Spectrum]);

  // Spectrograph
  //IniFile.WriteString('Graphics', 'Spectrograph', ISpectrograph[Spectrograph]);

  // Movie Size
  IniFile.WriteString('Graphics', 'MovieSize', IMovieSize[MovieSize]);

  // VideoPreview
  IniFile.WriteString('Graphics', 'VideoPreview', IVideoPreview[VideoPreview]);

  // VideoEnabled
  IniFile.WriteString('Graphics', 'VideoEnabled', IVideoEnabled[VideoEnabled]);

  // ClickAssist
  IniFile.WriteString('Sound', 'ClickAssist', IClickAssist[ClickAssist]);

  // BeatClick
  IniFile.WriteString('Sound', 'BeatClick', IBeatClick[BeatClick]);

  // AudioOutputBufferSize
  IniFile.WriteString('Sound', 'AudioOutputBufferSize', IAudioOutputBufferSize[AudioOutputBufferSizeIndex]);

  // Background music
  IniFile.WriteString('Sound', 'BackgroundMusic', IBackgroundMusic[BackgroundMusicOption]);

  // Song Preview
  IniFile.WriteString('Sound', 'PreviewVolume', IPreviewVolume[PreviewVolume]);

  // PreviewFading
  IniFile.WriteString('Sound', 'PreviewFading', IPreviewFading[PreviewFading]);

  // SavePlayback
  IniFile.WriteString('Sound', 'SavePlayback', ISavePlayback[SavePlayback]);

  // VoicePasstrough
  IniFile.WriteString('Sound', 'VoicePassthrough', IVoicePassthrough[VoicePassthrough]);

  // Lyrics Font
  IniFile.WriteString('Lyrics', 'LyricsFont', ILyricsFont[LyricsFont]);

  // Lyrics Effect
  IniFile.WriteString('Lyrics', 'LyricsEffect', ILyricsEffect[LyricsEffect]);

  // NoteLines
  IniFile.WriteString('Lyrics', 'NoteLines', INoteLines[NoteLines]);

  //Encoding default
  IniFile.WriteString('Lyrics', 'Encoding', EncodingName(DefaultEncoding));

  // Theme
  IniFile.WriteString('Themes', 'Theme', ITheme[Theme]);

  // Skin
  IniFile.WriteString('Themes', 'Skin', ISkin[SkinNo]);

  // Color
  IniFile.WriteString('Themes', 'Color', IColor[Color]);

  SaveInputDeviceCfg(IniFile);

  //LoadAnimation
  IniFile.WriteString('Advanced', 'LoadAnimation', ILoadAnimation[LoadAnimation]);

  //EffectSing
  IniFile.WriteString('Advanced', 'EffectSing', IEffectSing[EffectSing]);

  //ScreenFade
  IniFile.WriteString('Advanced', 'ScreenFade', IScreenFade[ScreenFade]);

  //AskbeforeDel
  IniFile.WriteString('Advanced', 'AskbeforeDel', IAskbeforeDel[AskBeforeDel]);

  //OnSongClick
  IniFile.WriteString('Advanced', 'OnSongClick', IOnSongClick[OnSongClick]);

  //Line Bonus
  IniFile.WriteString('Advanced', 'LineBonus', ILineBonus[LineBonus]);

  //Party Popup
  IniFile.WriteString('Advanced', 'PartyPopup', IPartyPopup[PartyPopup]);

  //SingScores
  IniFile.WriteString('Advanced', 'SingScores', ISingScores[SingScores]);

  //TopScores
  IniFile.WriteString('Advanced', 'TopScores', ITopScores[TopScores]);

  //SyncTo
  IniFile.WriteString('Advanced', 'SyncTo', ISyncTo[SyncTo]);

  // Joypad
  IniFile.WriteString('Controller', 'Joypad', IJoypad[Joypad]);

  // Mouse
  IniFile.WriteString('Controller', 'Mouse', IMouse[Mouse]);

  // SingTimebarMode
  IniFile.WriteString('Advanced', 'SingTimebarMode', ISingTimebarMode[SingTimebarMode]);

  // JukeboxTimebarMode
  IniFile.WriteString('Advanced', 'JukeboxTimebarMode', IJukeboxTimebarMode[JukeboxTimebarMode]);

  // Directories (add a template if section is missing)
  // Note: Value must be ' ' and not '', otherwise no key is generated on Linux
  if (not IniFile.SectionExists('Directories')) then
    IniFile.WriteString('Directories', 'SongDir1', ' ');

  if (not IniFile.ValueExists('Directories', 'WebScoresDir')) then
    IniFile.WriteString('Directories', 'WebScoresDir', ' ');

  // Jukebox
  IniFile.WriteString('Jukebox', 'LyricsFont', ILyricsFont[JukeboxFont]);
  IniFile.WriteString('Jukebox', 'LyricsEffect', ILyricsEffect[JukeboxEffect]);
  IniFile.WriteString('Jukebox', 'LyricsAlpha', ILyricsAlpha[JukeboxAlpha]);

  if (JukeboxSingLineColor <> High(ISingLineColor)) then
  begin
    C := GetLyricColor(JukeboxSingLineColor);
    HexColor := RGBToHex(Round(C.R * 255), Round(C.G * 255), Round(C.B * 255));
  end
  else
    HexColor := RGBToHex(JukeboxSingLineOtherColorR, JukeboxSingLineOtherColorG, JukeboxSingLineOtherColorB);

  IniFile.WriteString('Jukebox', 'SingLineColor', HexColor);

  if (JukeboxActualLineColor <> High(IActualLineColor)) then
  begin
    C := GetLyricGrayColor(JukeboxActualLineColor);
    HexColor := RGBToHex(Round(C.R * 255), Round(C.G * 255), Round(C.B * 255));
  end
  else
    HexColor := RGBToHex(JukeboxActualLineOtherColorR, JukeboxActualLineOtherColorG, JukeboxActualLineOtherColorB);

  IniFile.WriteString('Jukebox', 'ActualLineColor', HexColor);

  if (JukeboxNextLineColor <> High(INextLineColor)) then
  begin
    C := GetLyricGrayColor(JukeboxNextLineColor);
    HexColor := RGBToHex(Round(C.R * 255), Round(C.G * 255), Round(C.B * 255));
  end
  else
    HexColor := RGBToHex(JukeboxNextLineOtherColorR, JukeboxNextLineOtherColorG, JukeboxNextLineOtherColorB);

  IniFile.WriteString('Jukebox', 'NextLineColor', HexColor);

  if (JukeboxSingLineOutlineColor <> High(ISingLineOColor)) then
  begin
    C := GetLyricOutlineColor(JukeboxSingLineOutlineColor);
    HexColor := RGBToHex(Round(C.R * 255), Round(C.G * 255), Round(C.B * 255));
  end
  else
    HexColor := RGBToHex(JukeboxSingLineOtherOColorR, JukeboxSingLineOtherOColorG, JukeboxSingLineOtherOColorB);

  IniFile.WriteString('Jukebox', 'SingLineOColor', HexColor);

  if (JukeboxActualLineOutlineColor <> High(IActualLineOColor)) then
  begin
    C := GetLyricOutlineColor(JukeboxActualLineOutlineColor);
    HexColor := RGBToHex(Round(C.R * 255), Round(C.G * 255), Round(C.B * 255));
  end
  else
    HexColor := RGBToHex(JukeboxActualLineOtherOColorR, JukeboxActualLineOtherOColorG, JukeboxActualLineOtherOColorB);

  IniFile.WriteString('Jukebox', 'ActualLineOColor', HexColor);

  if (JukeboxNextLineOutlineColor <> High(INextLineOColor)) then
  begin
    C := GetLyricOutlineColor(JukeboxNextLineOutlineColor);
    HexColor := RGBToHex(Round(C.R * 255), Round(C.G * 255), Round(C.B * 255));
  end
  else
    HexColor := RGBToHex(JukeboxNextLineOtherOColorR, JukeboxNextLineOtherOColorG, JukeboxNextLineOtherOColorB);

  IniFile.WriteString('Jukebox', 'NextLineOColor', HexColor);

  IniFile.Free;
end;

procedure TIni.SaveNames;
var
  IniFile: TIniFile;
  I:       integer;
begin
  if not Filename.IsReadOnly() then
  begin
    IniFile := TIniFile.Create(Filename.ToNative);

    //Name Templates for Names Mod
    for I := 0 to High(Name) do
      IniFile.WriteString('Name', 'P' + IntToStr(I+1), Name[I]);
    for I := 0 to High(NameTeam) do
      IniFile.WriteString('NameTeam', 'T' + IntToStr(I+1), NameTeam[I]);
    for I := 0 to High(NameTemplate) do
      IniFile.WriteString('NameTemplate', 'Name' + IntToStr(I+1), NameTemplate[I]);

    IniFile.Free;
  end;
end;

procedure TIni.SaveLevel;
var
  IniFile: TIniFile;
begin
  if not Filename.IsReadOnly() then
  begin
    IniFile := TIniFile.Create(Filename.ToNative);

    // Difficulty
    IniFile.WriteString('Game', 'Difficulty', IDifficulty[Difficulty]);

    IniFile.Free;
  end;
end;

procedure TIni.SaveJukeboxSongMenu;
var
  IniFile: TIniFile;
begin
  if not Filename.IsReadOnly() then
  begin
    IniFile := TIniFile.Create(Filename.ToNative);

    IniFile.WriteString('Jukebox', 'SongMenu', IJukeboxSongMenu[JukeboxSongMenu]);

    IniFile.Free;
  end;
end;


procedure TIni.SaveShowWebScore;
var
  IniFile: TIniFile;
begin
  if not Filename.IsReadOnly() then
  begin
    IniFile := TIniFile.Create(Filename.ToNative);

    // ShowWebScore
    IniFile.WriteString('Game', 'ShowWebScore', DllMan.Websites[ShowWebScore].Name);

    IniFile.Free;
  end;
end;


procedure TIni.SavePlayerColors;

var
  IniFile: TIniFile;
  I: integer;
begin
  if not Filename.IsReadOnly() then
  begin
    IniFile := TIniFile.Create(Filename.ToNative);

    //Colors for Names Mod
    for I := 1 to 6 do
      IniFile.WriteString('PlayerColor', 'P' + IntToStr(I), IntToStr(PlayerColor[I-1]));

    IniFile.Free;
  end;
end;

procedure TIni.SavePlayerAvatars;
var
  IniFile: TIniFile;
  I: integer;
begin
  if not Filename.IsReadOnly() then
  begin
    IniFile := TIniFile.Create(Filename.ToNative);

    //Colors for Names Mod
    for I := 1 to 6 do
      IniFile.WriteString('PlayerAvatar', 'P' + IntToStr(I), PlayerAvatar[I-1]);

    IniFile.Free;
  end;
end;

procedure TIni.SavePlayerLevels;
var
  IniFile: TIniFile;
  I: integer;
begin
  if not Filename.IsReadOnly() then
  begin
    IniFile := TIniFile.Create(Filename.ToNative);

    for I := 1 to 6 do
      IniFile.WriteInteger('PlayerLevel', 'P' + IntToStr(I), PlayerLevel[I-1]);

    IniFile.Free;
  end;
end;

procedure TIni.SaveTeamColors;
var
  IniFile: TIniFile;
  I: integer;
begin
  if not Filename.IsReadOnly() then
  begin
    IniFile := TIniFile.Create(Filename.ToNative);

    //Colors for Names Mod
    for I := 1 to 3 do
      IniFile.WriteString('TeamColor', 'T' + IntToStr(I), IntToStr(TeamColor[I-1]));

    IniFile.Free;
  end;
end;

procedure TIni.SaveSoundFont(Name: string);
var
  IniFile: TIniFile;
  I: integer;
begin
  if not Filename.IsReadOnly() then
  begin
    IniFile := TIniFile.Create(Filename.ToNative);

    IniFile.WriteString('Sound', 'SoundFont', Name);

    IniFile.Free;
  end;
end;

procedure TIni.SaveWebcamSettings;
var
  IniFile: TIniFile;
begin
  if not Filename.IsReadOnly() then
  begin
    IniFile := TIniFile.Create(Filename.ToNative);

    // WebCam
    IniFile.WriteInteger('Webcam', 'ID', WebCamID);
    IniFile.WriteString('Webcam', 'Resolution', IWebcamResolution[WebcamResolution]);
    IniFile.WriteInteger('Webcam', 'FPS', StrToInt(IWebcamFPS[WebCamFPS]));

    IniFile.WriteString('Webcam', 'Flip', IWebcamFlip[WebcamFlip]);
    IniFile.WriteString('Webcam', 'Brightness', IWebcamBrightness[WebcamBrightness]);
    IniFile.WriteString('Webcam', 'Saturation', IWebcamSaturation[WebcamSaturation]);
    IniFile.WriteString('Webcam', 'Hue', IWebcamHue[WebcamHue]);
    IniFile.WriteInteger('Webcam', 'Effect', WebcamEffect);

    IniFile.Free;
  end;

end;

procedure TIni.SaveNumberOfPlayers;
var
  IniFile: TIniFile;
  I: integer;
begin
  if not Filename.IsReadOnly() then
  begin
    IniFile := TIniFile.Create(Filename.ToNative);

    // Players
    IniFile.WriteString('Game', 'Players', IPlayers[Players]);

    IniFile.Free;
  end;
end;

procedure TIni.SaveSingTimebarMode;
var
  IniFile: TIniFile;
  I: integer;
begin
  if not Filename.IsReadOnly() then
  begin
    IniFile := TIniFile.Create(Filename.ToNative);

    // Players
    IniFile.WriteString('Advanced', 'SingTimebarMode', ISingTimebarMode[SingTimebarMode]);

    IniFile.Free;
  end;
end;

procedure TIni.SaveJukeboxTimebarMode;
var
  IniFile: TIniFile;
  I: integer;
begin
  if not Filename.IsReadOnly() then
  begin
    IniFile := TIniFile.Create(Filename.ToNative);

    // Players
    IniFile.WriteString('Advanced', 'JukeboxTimebarMode', IJukeboxTimebarMode[JukeboxTimebarMode]);

    IniFile.Free;
  end;
end;

end.
