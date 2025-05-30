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

//TODO: lots of parts in this code should be rewritten in a more object oriented way.

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
  TextGL,
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
  DEFAULT_RESOLUTION = '800x600';
  DEFAULT_THEME = 'Modern';
  // TODO: the menu options only go up to 6, but there are internals that still go up to 12
  //  IMaxPlayerCount is untouched because lowering (or raising) it causes very strange behaviour, such as:
  //  * the game starts in a completely different language than the config specifies
  //  * the game crashes randomly
  //  8 and 12 players have never worked at any point in history
  //  it all needs refactoring at some point anyway because:
  //  * a lot of code works with the _index_ of IPlayers (instead of just the number of actual players)
  //  * it should be possible to play with 5 players [without duplicating a lot of code]
  //  * there might be a valid usecase for 0 players
  IMaxPlayerCount = 12;
  // Switch colors for players 2 and 4, since player 2 line color is used
  // for the second part in duet, and yellow (4) looks better than red (2)
  DefaultPlayerColors: array[0..IMaxPlayerCount-1] of integer = (1, 4, 3, 2, 5, 6, 7, 8, 9, 10, 11, 12);
  IPlayers:     array[0..4] of UTF8String = ('1', '2', '3', '4', '6');
  IPlayersVals: array[0..4] of integer    = ( 1 ,  2 ,  3 ,  4 ,  6 );

type

//Options

  TVisualizerOption      = (voOff, voWhenNoVideo, voWhenNoVideoAndImage, voOn);
  TBackgroundMusicOption = (bmoOff, bmoOn);
  TSongMenuMode = ( smRoulette, smChessboard, smCarousel, smSlotMachine, smSlide, smList, smMosaic);

  TIni = class
    private

      function ExtractKeyIndex(const Key, Prefix, Suffix: string): integer;
      function GetMaxKeyIndex(Keys: TStringList; const Prefix, Suffix: string): integer;
      function ReadArrayIndex(const SearchArray: array of UTF8String; IniFile: TCustomIniFile;
          IniSection: string; IniProperty: string; Default: integer; CaseInsensitive: boolean = false): integer; overload;
      function ReadArrayIndex(const SearchArray: array of UTF8String; IniFile: TCustomIniFile;
          IniSection: string; IniProperty: string; Default: integer; DefaultValue: UTF8String; CaseInsensitive: boolean = false): integer; overload;
      function InitializePianoKeyArray(const Values: array of Cardinal): TPianoKeyArray;

      procedure LoadInputDeviceCfg(IniFile: TMemIniFile);
      procedure SaveInputDeviceCfg(IniFile: TIniFile);
      procedure LoadThemes(IniFile: TCustomIniFile);

      procedure LoadPaths(IniFile: TCustomIniFile);
      procedure LoadScreenModes(IniFile: TCustomIniFile);
      procedure LoadWebcamSettings(IniFile: TCustomIniFile);

    public
      // Players or Teams colors
      SingColor:      array[0..(IMaxPlayerCount-1)] of integer;
      
      Name:           array[0..15] of UTF8String;
      PlayerColor:    array[0..(IMaxPlayerCount-1)] of integer;
      TeamColor:      array[0..2] of integer;

      PlayerAvatar:   array[0..(IMaxPlayerCount-1)] of UTF8String;
      PlayerLevel:    array[0..(IMaxPlayerCount-1)] of integer;

      // Templates for Names Mod
      NameTeam:       array[0..2] of UTF8String;
      NameTemplate:   array[0..(IMaxPlayerCount-1)] of UTF8String;

      // Filename of the opened iniFile
      Filename:       IPath;

      // Game
      Players:        integer;
      Difficulty:     integer;
      Language:       integer;
      SongMenu:       integer;
      Tabs:           integer;
      TabsAtStartup:  integer; // Tabs at Startup fix
      Sorting:        integer;
      ShowScores:     integer;
      ShowWebScore:   integer;
      Debug:          integer;
      AVDelay:        integer;
      MicDelay:       integer;

      // Graphics
      MaxFramerate:   byte;
      MaxFramerateGet: byte;
      Screens:        integer;
      Split:          integer;
      PositionX:      integer;
      PositionY:      integer;
      Resolution:     integer;             // Resolution for windowed mode
      ResolutionFullscreen:     integer;   // Resolution for real fullscreen (changing Video mode)
      Depth:          integer;
      VisualizerOption: integer;
      FullScreen:     integer;
      TextureSize:    integer;
      Oscilloscope:   integer;
      // not used
      //Spectrum:       integer;
      //Spectrograph:   integer;
      MovieSize:      integer;
      VideoPreview:   integer;
      VideoEnabled:   integer;
      PreferredCodecNames: string;

      // Sound
      MicBoost:       integer;
      ClickAssist:    integer;
      BeatClick:      integer;
      SavePlayback:   integer;
      ThresholdIndex: integer;
      AudioOutputBufferSizeIndex: integer;
      VoicePassthrough: integer;
      SoundFont:      string;
      ReplayGain:     integer;

      SyncTo: integer;

      // Song Preview
      PreviewVolume:  integer;
      PreviewFading:  integer;

      // Lyrics
      LyricsFont:     integer;
      LyricsStyle:    integer;
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
      DuetScores:     integer;
      TopScores:      integer;
      TopScreenSize:  integer;
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
      JukeboxStyle:    integer;
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

      PianoKeysLow: TPianoKeyArray;
      PianoKeysHigh: TPianoKeyArray;

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

      procedure TranslateOptionValues;

      { Sets resolution.
        @return (@true when resolution was added, @false otherwise) }
      function SetResolution(ResolutionString: string; RemoveCurrent: boolean = false; NoSave: boolean = false): boolean; overload;
      { Sets resolution.
        @return (@true when resolution was added, @false otherwise) }
      function SetResolution(w,h: integer; RemoveCurrent: boolean = false; NoSave: boolean = false): boolean; overload;
      { Sets resolution given by the index pointing to a resolution in IResolution.
        @return (@true when resolution ID was found, @false otherwise) }
      function SetResolution(index: integer): boolean; overload;
      function GetResolution(): string; overload;
      function GetResolution(out w,h: integer): string; overload;
      function GetResolution(index: integer; out ResolutionString: string): boolean; overload;

      function GetResolutionFullscreen(): string; overload;
      function GetResolutionFullscreen(out w,h: integer): string; overload;
      function GetResolutionFullscreen(index: integer; out ResolutionString: string): boolean; overload;

      procedure ClearCustomResolutions();

  end;

var
  Ini:                   TIni;
  IResolution:           TUTF8StringDynArray;
  IResolutionFullScreen: TUTF8StringDynArray;
  IResolutionCustom:     TUTF8StringDynArray;
  ILanguage:             TUTF8StringDynArray;
  ITheme:                TUTF8StringDynArray;
  ISkin:                 TUTF8StringDynArray;

{*
 * Options
 *}

const
  IDifficulty:  array[0..2] of UTF8String = ('Easy', 'Medium', 'Hard');
  ITabs:        array[0..1] of UTF8String = ('Off', 'On');

const
  ISorting:      array[0..10] of UTF8String = ('Edition', 'Genre', 'Language', 'Folder', 'Title', 'Artist', 'Artist2', 'Year', 'Year Reversed', 'Decade', 'Playlist');
  ISongMenuMode: array[0..6] of UTF8String = ('Roulette', 'Chessboard', 'Carousel', 'Slot Machine', 'Slide', 'List', 'Mosaic');

type
  TSortingType = (sEdition, sGenre, sLanguage, sFolder, sTitle, sArtist, sArtist2, sYear, sYearReversed, sDecade, sPlaylist);

const
  IShowScores:       array[0..2] of UTF8String  = ('Off', 'When exists', 'On');

  IDebug:            array[0..1] of UTF8String  = ('Off', 'On');

  IMaxFramerate:     array[0..11] of UTF8String  = ('10', '20', '30', '40', '50', '60', '70', '80', '90', '100', '150', '200');
  IScreens:          array[0..1] of UTF8String  = ('1', '2');
  ISplit:            array[0..1] of UTF8String  = ('Off', 'On');
  IFullScreen:       array[0..2] of UTF8String  = ('Off', 'On', 'Borderless');
  IDepth:            array[0..1] of UTF8String  = ('16 bit', '32 bit');
  IVisualizer:       array[0..3] of UTF8String  = ('Off', 'WhenNoVideo', 'WhenNoVideoAndImage', 'On');

  IBackgroundMusic:  array[0..1] of UTF8String  = ('Off', 'On');

  ITextureSize:      array[0..3] of UTF8String  = ('64', '128', '256', '512');
  ITextureSizeVals:  array[0..3] of integer     = ( 64,   128,   256,   512);

  // SingBar Mod
  IOscilloscope:     array[0..1] of UTF8String  = ('Off', 'On');

  ISpectrum:         array[0..1] of UTF8String  = ('Off', 'On');
  ISpectrograph:     array[0..1] of UTF8String  = ('Off', 'On');
  IMovieSize:        array[0..2] of UTF8String  = ('Half', 'Full [Vid]', 'Full [BG+Vid]');
  IVideoPreview:     array[0..1] of UTF8String  = ('Off', 'On');
  IVideoEnabled:     array[0..1] of UTF8String  = ('Off', 'On');

  IClickAssist:      array[0..1] of UTF8String  = ('Off', 'On');
  IBeatClick:        array[0..1] of UTF8String  = ('Off', 'On');
  ISavePlayback:     array[0..1] of UTF8String  = ('Off', 'On');
  IReplayGain:       array[0..1] of UTF8String  = ('Off', 'On');

  IThreshold:        array[0..7] of UTF8String  = ('5%', '10%', '15%', '20%', '25%', '30%', '40%', '60%');
  IThresholdVals:    array[0..7] of single      = (0.05, 0.10, 0.15,  0.20,  0.25,  0.30,  0.40,  0.60);

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

  // Song Preview
  IPreviewVolume:             array[0..12] of UTF8String = ('Off', '3%', '5%', '10%', '20%', '30%', '40%', '50%', '60%', '70%', '80%', '90%', '100%');
  IPreviewVolumeVals:         array[0..12] of single     = ( 0,  0.03,  0.05,  0.10,  0.20,  0.30,  0.40,  0.50,  0.60,  0.70,  0.80,  0.90,   1.00  );

  IPreviewFading:             array[0..5] of UTF8String  = ('Off', '1 Sec', '2 Secs', '3 Secs', '4 Secs', '5 Secs');
  IPreviewFadingVals:         array[0..5] of integer     = ( 0,     1,       2,        3,        4,        5      );

  ILyricsStyleCompat:   array[0..2] of UTF8String = ('Plain', 'Oline1', 'Oline2');
  ILyricsStyle:   array[0..2] of UTF8String = ('Regular', 'Bold', 'Outline');
  ILyricsEffect:  array[0..4] of UTF8String = ('Simple', 'Zoom', 'Slide', 'Ball', 'Shift');
  ILyricsAlpha:   array[0..20] of UTF8String = ('0.00', '0.05', '0.10', '0.15', '0.20', '0.25', '0.30', '0.35', '0.40', '0.45', '0.50',
                                                '0.55', '0.60', '0.65', '0.70', '0.75', '0.80', '0.85', '0.90', '0.95', '1.00');
  ILyricsAlphaVals: array[0..20] of single = (0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50,
                                              0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 1.00);

  INoteLines:     array[0..1] of UTF8String = ('Off', 'On');

  // for lyric colors
  ILine:             array[0..2] of UTF8String = ('Sing', 'Actual', 'Next');
  IAttribute:        array[0..1] of UTF8String = ('Fill', 'Outline');
  ISingLineColor:    array[0..20] of UTF8String = ('Blue', 'Green', 'Pink', 'Red', 'Violet', 'Orange', 'Yellow', 'Brown', 'Black', 'Turquoise', 'Salmon', 'GreenYellow', 'Lavender', 'Beige', 'Teal', 'Orchid', 'SteelBlue', 'Plum', 'Chocolate', 'Gold', 'Other');
  IActualLineColor:  array[0..9] of UTF8String = ('Black', 'Gray +3', 'Gray +2', 'Gray +1', 'Gray', 'Gray -1', 'Gray -2', 'Gray -3', 'White', 'Other');
  INextLineColor:    array[0..9] of UTF8String = ('Black', 'Gray +3', 'Gray +2', 'Gray +1', 'Gray', 'Gray -1', 'Gray -2', 'Gray -3', 'White', 'Other');
  // outline
  ISingLineOColor:    array[0..2] of UTF8String = ('Black', 'White', 'Other');
  IActualLineOColor:  array[0..2] of UTF8String = ('Black', 'White', 'Other');
  INextLineOColor:    array[0..2] of UTF8String = ('Black', 'White', 'Other');

  IHexSingColor: array[0..20] of UTF8String = ('0096FF', '3FBF3F', 'FF3FC0', 'DC0000', 'B43FE6', 'FF9000', 'FFFF00', 'C07F1F', '000000', '00FFE6', 'FF7F66',
                                                '99FF66', 'CCCCFF', 'FFE6CC', '339999', '9900CC', '336699', 'FF99FF', '8A5C2E', 'FFCC33', '');
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
  IDuetScores:    array[0..3] of UTF8String = ('Off', 'Separate', 'Combined', 'Both');
  ITopScores:     array[0..1] of UTF8String = ('All', 'Player');
  IOnSongClick:   array[0..2] of UTF8String = ('Sing', 'Select Players', 'Open Menu');
  sStartSing = 0;
  sSelectPlayer = 1;
  sOpenMenu = 2;

  ILineBonus:     array[0..1] of UTF8String = ('Off', 'On');
  IPartyPopup:    array[0..1] of UTF8String = ('Off', 'On');

  IJoypad:        array[0..1] of UTF8String = ('Off', 'On');
  IMouse:         array[0..2] of UTF8String = ('Off', 'System', 'Game');
  IMouseLegacy:         array[0..2] of UTF8String = ('Off', 'Hardware Cursor', 'Software Cursor'); // use to convert old config option to new

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
  ILyricsFont:                 array of UTF8String;

  IDifficultyTranslated:       array[0..2] of UTF8String  = ('Easy', 'Medium', 'Hard');
  ITabsTranslated:             array[0..1] of UTF8String  = ('Off', 'On');

  ISongMenuTranslated:         array[0..6] of UTF8String  = ('Roulette', 'Chessboard', 'Carousel', 'Slot Machine', 'Slide', 'List', 'Mosaic');

  //ISortingTranslated:          array[0..9] of UTF8String  = ('Edition', 'Genre', 'Language', 'Folder', 'Title', 'Artist', 'Artist2', 'Year', 'Decade', 'Playlist');
  ISortingTranslated:          array[0..9] of UTF8String  = ('Edition', 'Genre', 'Language', 'Folder', 'Title', 'Artist', 'Artist2', 'Year', 'Year Reversed', 'Decade');

  IShowScoresTranslated:       array[0..2] of UTF8String  = ('Off', 'WhenExists', 'On');

  IDebugTranslated:            array[0..1] of UTF8String  = ('Off', 'On');
  IAVDelay:                    array of UTF8String;
  IMicDelay:                   array of UTF8String;

  IFullScreenTranslated:       array[0..2] of UTF8String  = ('Off', 'On', 'Borderless');
  IVisualizerTranslated:       array[0..3] of UTF8String  = ('Off', 'WhenNoVideo', 'WhenNoVideoAndImage','On');

  IBackgroundMusicTranslated:  array[0..1] of UTF8String  = ('Off', 'On');

  // SingBar Mod
  IOscilloscopeTranslated:     array[0..1] of UTF8String  = ('Off', 'On');

  ISpectrumTranslated:         array[0..1] of UTF8String  = ('Off', 'On');
  ISpectrographTranslated:     array[0..1] of UTF8String  = ('Off', 'On');
  IMovieSizeTranslated:        array[0..2] of UTF8String  = ('Half', 'Full [Vid]', 'Full [BG+Vid]');
  IVideoPreviewTranslated:     array[0..1] of UTF8String  = ('Off', 'On');
  IVideoEnabledTranslated:     array[0..1] of UTF8String  = ('Off', 'On');

  IClickAssistTranslated:      array[0..1] of UTF8String  = ('Off', 'On');
  IBeatClickTranslated:        array[0..1] of UTF8String  = ('Off', 'On');
  IReplayGainTranslated:       array[0..1] of UTF8String  = ('Off', 'On');

  ISavePlaybackTranslated:     array[0..1] of UTF8String  = ('Off', 'On');

  IVoicePassthroughTranslated: array[0..1] of UTF8String  = ('Off', 'On');

  ISyncToTranslated:           array[0..2] of UTF8String  = ('Music', 'Lyrics', 'Off');

  // Song Preview
  IPreviewVolumeTranslated:    array[0..12] of UTF8String = ('Off', '3%', '5%', '10%', '20%', '30%', '40%', '50%', '60%', '70%', '80%', '90%', '100%');

  IAudioOutputBufferSizeTranslated: array[0..9] of UTF8String  = ('Auto', '256', '512', '1024', '2048', '4096', '8192', '16384', '32768', '65536');

  IAudioInputBufferSizeTranslated:  array[0..9] of UTF8String  = ('Auto', '256', '512', '1024', '2048', '4096', '8192', '16384', '32768', '65536');

  IPreviewFadingTranslated:    array[0..5] of UTF8String  = ('Off', '1 Sec', '2 Secs', '3 Secs', '4 Secs', '5 Secs');

  ILyricsStyleTranslated:      array[0..2] of UTF8String = ('Regular', 'Bold', 'Outline');
  ILyricsEffectTranslated:     array[0..4] of UTF8String = ('Simple', 'Zoom', 'Slide', 'Ball', 'Shift');
  INoteLinesTranslated:        array[0..1] of UTF8String = ('Off', 'On');
  IColorTranslated:            array[0..8] of UTF8String = ('Blue', 'Green', 'Pink', 'Red', 'Violet', 'Orange', 'Yellow', 'Brown', 'Black');
  IPlayerColorTranslated:      array[0..15] of UTF8String = ('Blue', 'Red', 'Green', 'Yellow', 'Orange', 'Pink',  'Violet', 'Brown', 'Gray', 'Dark Blue', 'Sky', 'Cyan', 'Flame', 'Orchid', 'Harlequin', 'Lime');

  // for lyric colors
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
  IDuetScoresTranslated:       array[0..3] of UTF8String = ('Off', 'Separate', 'Combined', 'Both');
  ITopScoresTranslated:        array[0..1] of UTF8String = ('All', 'Player');
  ITopScreenSize:              array of UTF8String;

  IJoypadTranslated:           array[0..1] of UTF8String = ('Off', 'On');
  IMouseTranslated:            array[0..2] of UTF8String = ('Off', 'On [System Cursor]', 'On [Game Cursor]');
  IMouseTranslatedLegacy:      array[0..2] of UTF8String = ('Off', 'Hardware Cursor', 'Software Cursor');

  ISingTimebarModeTranslated:      array[0..2] of UTF8String = ('Current', 'Remaining', 'Total');
  IJukeboxTimebarModeTranslated:      array[0..2] of UTF8String = ('Current', 'Remaining', 'Total');

  // Recording options
  IChannelPlayerTranslated:    array[0..IMaxPlayerCount] of UTF8String = ('Off', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12');
  IMicBoostTranslated:         array[0..3] of UTF8String = ('Off', '+6dB', '+12dB', '+18dB');

  // Network
  ISendNameTranslated:        array[0..1] of UTF8String = ('Off', 'On');
  IAutoModeTranslated:        array[0..2] of UTF8String = ('Off', 'Send', 'Guardar');
  IAutoPlayerTranslated:      array[0..IMaxPlayerCount] of UTF8String = ('Player 1', 'Player 2', 'Player 3', 'Player 4', 'Player 5', 'Player 6', 'Player 7', 'Player 8', 'Player 9', 'Player 10', 'Player 11', 'Player 12', 'All');
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
  IPlayerTranslated:      array[0..(IMaxPlayerCount-1)] of UTF8String = ('Player 1', 'Player 2', 'Player 3', 'Player 4', 'Player 5', 'Player 6', 'Player 7', 'Player 8', 'Player 9', 'Player 10', 'Player 11', 'Player 12');

  IRed:       array[0..255] of UTF8String;
  IGreen:     array[0..255] of UTF8String;
  IBlue:      array[0..255] of UTF8String;


implementation

uses
  StrUtils,
  sdl2,
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

const
  IGNORE_INDEX = -1;

(**
 * Translate and set the values of options, which need translation.
 *)
procedure TIni.TranslateOptionValues;
var
  I: integer;
  Zeros: string;
begin
  // Load language file, fallback to config language if param is invalid
  if (Params.Language > -1) and (Params.Language < Length(ILanguage)) then
    ULanguage.Language.ChangeLanguage(ILanguage[Params.Language])
  else
    ULanguage.Language.ChangeLanguage(ILanguage[Language]);

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
  ISortingTranslated[8]               := ULanguage.Language.Translate('OPTION_VALUE_YEAR_REVERSED');
  ISortingTranslated[9]               := ULanguage.Language.Translate('OPTION_VALUE_DECADE');
  //ISortingTranslated[10]               := ULanguage.Language.Translate('OPTION_VALUE_PLAYLIST');

  IShowScoresTranslated[0]            := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IShowScoresTranslated[1]            := ULanguage.Language.Translate('OPTION_VALUE_WHENEXIST');
  IShowScoresTranslated[2]            := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IDebugTranslated[0]                 := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IDebugTranslated[1]                 := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IFullScreenTranslated[0]            := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IFullScreenTranslated[1]            := ULanguage.Language.Translate('OPTION_VALUE_ON');
  IFullScreenTranslated[2]            := ULanguage.Language.Translate('OPTION_VALUE_BORDERLESS');

  IVisualizerTranslated[0]            := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IVisualizerTranslated[1]            := ULanguage.Language.Translate('OPTION_VALUE_WHENNOVIDEO');
  IVisualizerTranslated[2]            := ULanguage.Language.Translate('OPTION_VALUE_WHENNOVIDEOANDIMAGE');
  IVisualizerTranslated[3]            := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IBackgroundMusicTranslated[0]       := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IBackgroundMusicTranslated[1]       := ULanguage.Language.Translate('OPTION_VALUE_ON');

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

  IReplayGainTranslated[0]            := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IReplayGainTranslated[1]            := ULanguage.Language.Translate('OPTION_VALUE_ON');

  ISavePlaybackTranslated[0]          := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  ISavePlaybackTranslated[1]          := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IVoicePassthroughTranslated[0]      := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IVoicePassthroughTranslated[1]      := ULanguage.Language.Translate('OPTION_VALUE_ON');

  ISyncToTranslated[Ord(stMusic)]     := ULanguage.Language.Translate('OPTION_VALUE_MUSIC');
  ISyncToTranslated[Ord(stLyrics)]    := ULanguage.Language.Translate('OPTION_VALUE_LYRICS');
  ISyncToTranslated[Ord(stOff)]       := ULanguage.Language.Translate('OPTION_VALUE_OFF');

  ILyricsStyleTranslated[0]           := ULanguage.Language.Translate('OPTION_VALUE_REGULAR');
  ILyricsStyleTranslated[1]           := ULanguage.Language.Translate('OPTION_VALUE_BOLD');
  ILyricsStyleTranslated[2]           := ULanguage.Language.Translate('OPTION_VALUE_TEXTOUTLINE');

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
  ILineTranslated[1] := ULanguage.Language.Translate('OPTION_VALUE_CURRENT');
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

  IDuetScoresTranslated[0]            := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IDuetScoresTranslated[1]            := ULanguage.Language.Translate('OPTION_VALUE_SEPARATE');
  IDuetScoresTranslated[2]            := ULanguage.Language.Translate('OPTION_VALUE_COMBINED');
  IDuetScoresTranslated[3]            := ULanguage.Language.Translate('OPTION_VALUE_BOTH');

  ITopScoresTranslated[0]          := ULanguage.Language.Translate('OPTION_VALUE_ALL');
  ITopScoresTranslated[1]          := ULanguage.Language.Translate('OPTION_VALUE_PLAYER');

  IJoypadTranslated[0]                := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IJoypadTranslated[1]                := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IMouseTranslated[0]                 := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IMouseTranslated[1]                 := ULanguage.Language.Translate('OPTION_VALUE_SYSTEM_CURSOR');
  IMouseTranslated[2]                 := ULanguage.Language.Translate('OPTION_VALUE_GAME_CURSOR');

  IMouseTranslatedLegacy[0]           := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IMouseTranslatedLegacy[1]           := ULanguage.Language.Translate('OPTION_VALUE_HARDWARE_CURSOR');
  IMouseTranslatedLegacy[2]           := ULanguage.Language.Translate('OPTION_VALUE_SOFTWARE_CURSOR');

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

  // Song Preview
  IPreviewVolumeTranslated[0]         := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IPreviewVolumeTranslated[1]         := '3%';
  IPreviewVolumeTranslated[2]         := '5%';
  IPreviewVolumeTranslated[3]         := '10%';
  IPreviewVolumeTranslated[4]         := '20%';
  IPreviewVolumeTranslated[5]         := '30%';
  IPreviewVolumeTranslated[6]         := '40%';
  IPreviewVolumeTranslated[7]         := '50%';
  IPreviewVolumeTranslated[8]         := '60%';
  IPreviewVolumeTranslated[9]         := '70%';
  IPreviewVolumeTranslated[10]        := '80%';
  IPreviewVolumeTranslated[11]        := '90%';
  IPreviewVolumeTranslated[12]        := '100%';


  IPreviewFadingTranslated[0]         :=        ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IPreviewFadingTranslated[1]         := '1 ' + ULanguage.Language.Translate('OPTION_VALUE_SEC');
  IPreviewFadingTranslated[2]         := '2 ' + ULanguage.Language.Translate('OPTION_VALUE_SECS');
  IPreviewFadingTranslated[3]         := '3 ' + ULanguage.Language.Translate('OPTION_VALUE_SECS');
  IPreviewFadingTranslated[4]         := '4 ' + ULanguage.Language.Translate('OPTION_VALUE_SECS');
  IPreviewFadingTranslated[5]         := '5 ' + ULanguage.Language.Translate('OPTION_VALUE_SECS');

  // Recording options
  IChannelPlayerTranslated[0]         := ULanguage.Language.Translate('SING_OPTIONS_RECORD_NOONE');
  for I:=1 to IMaxPlayerCount do
  begin
    IChannelPlayerTranslated[I]       :=IntToStr(I);
  end;

  IMicBoostTranslated[0]              := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IMicBoostTranslated[1]              := '+6dB';
  IMicBoostTranslated[2]              := '+12dB';
  IMicBoostTranslated[3]              := '+18dB';

  // Network
  IAutoModeTranslated[0]         := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IAutoModeTranslated[1]         := ULanguage.Language.Translate('OPTION_VALUE_SEND');
  IAutoModeTranslated[2]         := ULanguage.Language.Translate('OPTION_VALUE_SAVE');

  for I:=0 to IMaxPlayerCount-1 do
  begin
    IAutoPlayerTranslated[I]       :=ULanguage.Language.Translate('OPTION_PLAYER_' + IntToStr(I));
  end;
  IAutoPlayerTranslated[12]         := ULanguage.Language.Translate('OPTION_ALL_PLAYERS');

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
    IniSection: string; IniProperty: string; Default: integer; CaseInsensitive: boolean = false): integer;
begin
  Result := ReadArrayIndex(SearchArray, IniFile, IniSection, IniProperty, Default, '', CaseInsensitive);
end;

function TIni.ReadArrayIndex(const SearchArray: array of UTF8String; IniFile: TCustomIniFile;
          IniSection: string; IniProperty: string; Default: integer; DefaultValue: UTF8String; CaseInsensitive: boolean = false): integer;
var
  StrValue: string;
begin
  StrValue := IniFile.ReadString(IniSection, IniProperty, '');
  Result := GetArrayIndex(SearchArray, StrValue, CaseInsensitive);
  if (Result < 0) then
  begin
    if (Default = IGNORE_INDEX) and (not UCommon.Equals(StrValue, DefaultValue, not CaseInsensitive)) then
    begin
      // prioritize default string value
      Result := GetArrayIndex(SearchArray, DefaultValue, CaseInsensitive);
    end;

    if (Result < 0) or (Result > High(SearchArray)) then Result := Default;
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
  MicBoost := ReadArrayIndex(IMicBoost, IniFile, 'Record', 'MicBoost', IGNORE_INDEX, 'Off');
  // Threshold
  ThresholdIndex := ReadArrayIndex(IThreshold, IniFile, 'Record', 'Threshold', 1);
end;

procedure TIni.SaveInputDeviceCfg(IniFile: TIniFile);
var
  DeviceIndex:  integer;
  ChannelIndex: integer;
  PlayerNumber: integer;
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
      PlayerNumber := InputDeviceConfig[DeviceIndex].ChannelToPlayerMap[ChannelIndex];
      if PlayerNumber > 0 then
      begin
        IniFile.WriteInteger('Record',
            Format('Channel%d[%d]', [ChannelIndex+1, DeviceIndex+1]),
            PlayerNumber);
      end
      else
      begin
        IniFile.DeleteKey('Record',
            Format('Channel%d[%d]', [ChannelIndex+1, DeviceIndex+1]));
      end;
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

  Theme := ReadArrayIndex(ITheme, IniFile, 'Themes', 'Theme', IGNORE_INDEX, DEFAULT_THEME, true);
  if (Theme = -1) then
    Theme := 0;

  // Skin
  Skin.onThemeChange;

  SkinNo := ReadArrayIndex(ISkin, IniFile, 'Themes', 'Skin', UThemes.Theme.Themes[Theme].DefaultSkin);

  { there may be a not existing skin in the ini file
    e.g. due to manual edit or corrupted file.
    in this case we load the first Skin }
  if SkinNo = -1 then
    SkinNo := 0;

  // Color
  Color := ReadArrayIndex(IColor, IniFile, 'Themes', 'Color', Skin.GetDefaultColor(SkinNo));
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
  I, Success, DisplayIndex:     integer;
  CurrentMode, ModeIter, MaxMode: TSDL_DisplayMode;
  CurrentRes, ResString: string;
begin
  MaxFramerate := ReadArrayIndex(IMaxFramerate, IniFile, 'Graphics', 'MaxFramerate', IGNORE_INDEX, '60');
  MaxFramerateGet:= StrToInt(IMaxFramerate[MaxFramerate]);
  // Screens
  Screens := ReadArrayIndex(IScreens, IniFile, 'Graphics', 'Screens', 0);

  // Split mode
  Split := ReadArrayIndex(ISplit, IniFile, 'Graphics', 'Split', 0);

  // FullScreen
  FullScreen := ReadArrayIndex(IFullScreen, IniFile, 'Graphics', 'FullScreen', IGNORE_INDEX, 'Borderless');

  // PositionX
  PositionX := StrToInt(IniFile.ReadString('Graphics', 'PositionX', '0'));

  // PositionY
  PositionY := StrToInt(IniFile.ReadString('Graphics', 'PositionY', '0'));

  // standard fallback resolutions
  SetLength(IResolution, 27);
  IResolution[0] := '640x480'; // VGA
  IResolution[1] := '720x480'; // SDTV 480i, EDTV 480p [TV]
  IResolution[2] := '720x576'; // SDTV 576i, EDTV 576p [TV]
  IResolution[3] := '768x576'; // SDTV 576i, EDTV 576p [TV]
  IResolution[4] := '800x600'; // SVGA
  IResolution[5] := '960x540'; // Quarter FHD
  IResolution[6] := '1024x768'; // XGA
  IResolution[7] := '1152x666';
  IResolution[8] := '1152x864'; // XGA+
  IResolution[9] := '1280x720'; // WXGA-H
  IResolution[10] := '1280x800'; // WXGA
  IResolution[11] := '1280x960'; // WXGA
  IResolution[12] := '1280x1024'; // SXGA
  IResolution[13] := '1366x768'; // HD
  IResolution[14] := '1400x1050'; // SXGA+
  IResolution[15] := '1440x900'; // WXGA+
  IResolution[16] := '1600x900'; // HD+
  IResolution[17] := '1600x1200'; // UXGA
  IResolution[18] := '1680x1050'; // WSXGA+
  IResolution[19] := '1920x1080'; // FHD
  IResolution[20] := '1920x1200'; // WUXGA
  IResolution[21] := '2048x1152'; // QWXGA
  IResolution[22] := '2560x1440'; // WQHD
  IResolution[23] := '2560x1600'; // WQXGA
  IResolution[24] := '3840x2160'; // 4K UHD
  IResolution[25] := '4096x2304'; // 4K
  IResolution[26] := '4096x3072'; // HXGA

  // Check if there are any modes available

  // retrieve currently used Video Display
  DisplayIndex := -1;
  MaxMode.h := 0; MaxMode.w := 0;
  CurrentMode.h := -1; CurrentMode.w := -1;
  for I := 0 to SDL_GetNumVideoDisplays() - 1 do
  begin
    Success := SDL_GetCurrentDisplayMode(I,  @CurrentMode);
    if Success = 0 then
    begin
      DisplayIndex := I;
      CurrentRes := BuildResolutionString(CurrentMode.w, CurrentMode.h);
      Break
    end;
  end;

  // retrieve available display modes, store into separate array
  if DisplayIndex >= 0 then
  begin
    for I := 0 to SDL_GetNumDisplayModes(DisplayIndex) - 1 do
    begin
      Success := SDL_GetDisplayMode(DisplayIndex, I, @ModeIter);
      if Success <> 0 then continue;

      ResString := BuildResolutionString(ModeIter.w, ModeIter.h);
      if GetArrayIndex(IResolutionFullScreen, ResString) < 0 then
      begin
        Log.LogStatus('Found Video Mode: ' + ResString, 'Video');
        SetLength(IResolutionFullScreen, Length(IResolutionFullScreen) + 1);
        IResolutionFullScreen[High(IResolutionFullScreen)] := ResString;

        if (ModeIter.w > MaxMode.w) or (ModeIter.h > ModeIter.h) then
        begin
          MaxMode := ModeIter;
        end;
      end;
    end;
  end;

  // if display modes are found, override fallback ones
  if Length(IResolutionFullScreen) > 0 then
  begin
    Log.LogStatus( 'Found resolutions: ' + IntToStr(Length(IResolutionFullScreen)), 'Video');
    IResolution := IResolutionFullScreen;

    // reverse order
    for I := 0 to (Length(IResolution) div 2) - 1 do swap(IResolution[I], IResolution[High(IResolution)-I]);
  end;

  // read fullscreen resolution and verify if possible
  ResString := IniFile.ReadString('Graphics', 'ResolutionFullscreen', CurrentRes);
  ResolutionFullscreen := GetArrayIndex(IResolutionFullScreen, ResString);

  // Check if there is a resolution configured, try using it
  ResString := IniFile.ReadString('Graphics', 'Resolution', '');
  if ResString = '' then
  begin
    ResString := CurrentRes; // either store desktop resolution or invalid which results into DEFAULT
  end;

  // check if stored resolution is valid
  Resolution := GetArrayIndex(IResolution, ResString);

  // if resolution cannot be found, check if is larger than max resolution
  if (Resolution < 0) and (MaxMode.w > 0) and (MaxMode.h > 0) and
     (ParseResolutionString(ResString, ModeIter.w, ModeIter.h)) and
     ((ModeIter.w > MaxMode.w) or (ModeIter.h > MaxMode.h)) then
  begin
    Log.LogInfo(Format('Exceeding resoluton found (%s). Reverting to standard resolution.', [ResString]), 'Video');
    ResString := CurrentRes;
    Resolution := GetArrayIndex(IResolution, ResString);
  end;

  // append unknown mode to list
  if (Resolution = -1) and (Length(ResString) >= 3) then
  begin
    SetLength(IResolution, Length(IResolution) + 1);
    IResolution[High(IResolution)] := ResString;
    Resolution := High(IResolution);

    // store also as custom resolution to eventually remove it upon window size change
    SetLength(IResolutionCustom, Length(IResolutionCustom) + 1);
    IResolutionCustom[High(IResolutionCustom)] := ResString;
  end;

  if (Length(IResolution) = 0) or (Resolution < 0) then
  begin
    // if no modes were set, then failback to DEFAULT_RESOLUTION (800x600)
    // as per http://sourceforge.net/forum/message.php?msg_id=4544965
    // THANKS : linnex at users.sourceforge.net
    SetLength(IResolution, Length(IResolution) + 1);
    IResolution[High(IResolution)] := DEFAULT_RESOLUTION;
    Resolution := GetArrayIndex(IResolution, DEFAULT_RESOLUTION);
    if Resolution < 0 then Resolution := 0;

    Log.LogStatus( Format('No video mode found! Default to: %s ', [IResolution[Resolution]]), 'Video');
    FullScreen := 0; // default to fullscreen OFF in this case
  end;

  // Depth
  Depth := ReadArrayIndex(IDepth, IniFile, 'Graphics', 'Depth', IGNORE_INDEX, '32 bit');
end;

function TIni.InitializePianoKeyArray(const Values: array of Cardinal): TPianoKeyArray;
var
  i: Integer;
begin
  SetLength(Result, Length(Values));
  for i := Low(Values) to High(Values) do
    Result[i] := Values[i];
end;

procedure TIni.Load();
var
  IniFile: TMemIniFile;
  I:       integer;
  IShowWebScore: array of UTF8String;
  HexColor: string;
  Col: TRGB;
  KeysLow: string;
  KeysHigh: string;
  ReadPianoKeysLow: TPianoKeyArray;
  ReadPianoKeysHigh: TPianoKeyArray;
begin
  LoadFontFamilyNames;
  ILyricsFont := FontFamilyNames;
  GamePath := Platform.GetGameUserPath;

  Log.LogStatus( 'GamePath : ' +GamePath.ToNative , '' );

  if (Params.ConfigFile.IsSet) then
    FileName := Params.ConfigFile
  else
    FileName := GamePath.Append('config.ini');

  Log.LogStatus('Using config : ' + FileName.ToNative, 'Ini');
  IniFile := TMemIniFile.Create(FileName.ToNative);

  for I := 0 to IMaxPlayerCount-1 do
  begin
    // Name
    Name[I] := IniFile.ReadString('Name', 'P'+IntToStr(I+1), 'Player'+IntToStr(I+1));
    // Color Player
    PlayerColor[I] := IniFile.ReadInteger('PlayerColor', 'P'+IntToStr(I+1), DefaultPlayerColors[I]);
    // Avatar Player
    PlayerAvatar[I] := IniFile.ReadString('PlayerAvatar', 'P'+IntToStr(I+1), '');
    // Level Player
    PlayerLevel[I] := IniFile.ReadInteger('PlayerLevel', 'P'+IntToStr(I+1), 0);
  end;

  // Color Team
  for I := 0 to 2 do
    TeamColor[I] := IniFile.ReadInteger('TeamColor', 'T'+IntToStr(I+1), I + 1);

  // Templates for Names Mod
  for I := 0 to 2 do
    NameTeam[I] := IniFile.ReadString('NameTeam', 'T'+IntToStr(I+1), 'Team'+IntToStr(I+1));
  for I := 0 to 11 do
    NameTemplate[I] := IniFile.ReadString('NameTemplate', 'Name'+IntToStr(I+1), 'Template'+IntToStr(I+1));

  // Players
  Players := ReadArrayIndex(IPlayers, IniFile, 'Game', 'Players', 0);

  // Difficulty
  Difficulty := ReadArrayIndex(IDifficulty, IniFile, 'Game', 'Difficulty', IGNORE_INDEX, 'Easy');

  // Language
  Language := ReadArrayIndex(ILanguage, IniFile, 'Game', 'Language', IGNORE_INDEX, 'English');
  if Language < 0 then Language := GetArrayIndex(ILanguage, 'English'); // Default to english
  if Language < 0 then Language := 0; // Default to first available

  // SongMenu
  SongMenu := ReadArrayIndex(ISongMenuMode, IniFile, 'Game', 'SongMenu', Ord(smRoulette));

  // Tabs
  Tabs := ReadArrayIndex(ITabs, IniFile, 'Game', 'Tabs', 0);
  TabsAtStartup := Tabs;	//Tabs at Startup fix

  // Song Sorting
  Sorting := ReadArrayIndex(ISorting, IniFile, 'Game', 'Sorting', Ord(sArtist));

  // Show Score
  ShowScores := ReadArrayIndex(IShowScores, IniFile, 'Game', 'ShowScores', IGNORE_INDEX, 'On');

  AVDelay := IniFile.ReadInteger('Game', 'AVDelay', 0);

  MicDelay := IniFile.ReadInteger('Game', 'MicDelay', 140);

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
    ShowWebScore := ReadArrayIndex(IShowWebScore, IniFile, 'Game', 'ShowWebScore', 0);
    if (ShowWebScore = -1) then
      ShowWebScore := 0;
  end;

  // Debug
  Debug := ReadArrayIndex(IDebug, IniFile, 'Game', 'Debug', 0);

  LoadScreenModes(IniFile);

  LoadWebcamSettings(IniFile);

  // TextureSize (aka CachedCoverSize)
  TextureSize := ReadArrayIndex(ITextureSize, IniFile, 'Graphics', 'TextureSize', IGNORE_INDEX, '256');

  // Oscilloscope
  Oscilloscope := ReadArrayIndex(IOscilloscope, IniFile, 'Graphics', 'Oscilloscope', 1);

  // Spectrum
  //Spectrum := ReadArrayIndex(ISpectrum, IniFile, 'Graphics', 'Spectrum', IGNORE_INDEX, 'Off');

  // Spectrograph
  //Spectrograph := ReadArrayIndex(ISpectrograph, IniFile, 'Graphics', 'Spectrograph', IGNORE_INDEX, 'Off');

  // MovieSize
  MovieSize := ReadArrayIndex(IMovieSize, IniFile, 'Graphics', 'MovieSize', 2);

  // VideoPreview
  VideoPreview := ReadArrayIndex(IVideoPreview, IniFile, 'Graphics', 'VideoPreview', 1);

  // VideoEnabled
  VideoEnabled := ReadArrayIndex(IVideoEnabled, IniFile, 'Graphics', 'VideoEnabled', 1);

  PreferredCodecNames := IniFile.ReadString('Graphics', 'PreferredCodecs', '');

  // ClickAssist
  ClickAssist := ReadArrayIndex(IClickAssist, IniFile, 'Sound', 'ClickAssist', IGNORE_INDEX, 'Off');

  // BeatClick
  BeatClick := ReadArrayIndex(IBeatClick, IniFile, 'Sound', 'BeatClick', 0);

  // SavePlayback
  SavePlayback := ReadArrayIndex(ISavePlayback, IniFile, 'Sound', 'SavePlayback', 0);

  // AudioOutputBufferSize
  AudioOutputBufferSizeIndex := ReadArrayIndex(IAudioOutputBufferSize, IniFile, 'Sound', 'AudioOutputBufferSize', 0);

  //Preview Volume
  PreviewVolume := ReadArrayIndex(IPreviewVolume, IniFile, 'Sound', 'PreviewVolume', 5);

  // ReplayGain
  ReplayGain := ReadArrayIndex(IReplayGain, IniFile, 'Sound', 'ReplayGain', 0);

  //Preview Fading
  PreviewFading := ReadArrayIndex(IPreviewFading, IniFile, 'Sound', 'PreviewFading', 3);

  //AudioRepeat aka VoicePassthrough
  VoicePassthrough := ReadArrayIndex(IVoicePassthrough, IniFile, 'Sound', 'VoicePassthrough', 0);
  
  SoundFont := IniFile.ReadString('Sound', 'SoundFont', '');

  // Lyrics Font
  LyricsFont := ReadArrayIndex(ILyricsFont, IniFile, 'Lyrics', 'LyricsFont', 0);

  // Lyrics Style
  LyricsStyle := ReadArrayIndex(ILyricsStyleCompat, IniFile, 'Lyrics', 'LyricsStyle', -1);
  if (LyricsStyle = -1) then
    LyricsStyle := ReadArrayIndex(ILyricsStyle, IniFile, 'Lyrics', 'LyricsStyle', 2);

  // Lyrics Effect
  LyricsEffect := ReadArrayIndex(ILyricsEffect, IniFile, 'Lyrics', 'LyricsEffect', 2);

  // NoteLines
  NoteLines := ReadArrayIndex(INoteLines, IniFile, 'Lyrics', 'NoteLines', 1);

  // DefaultEncoding
  DefaultEncoding := ParseEncoding(IniFile.ReadString('Lyrics', 'Encoding', ''), encAuto);

  LoadThemes(IniFile);

  LoadInputDeviceCfg(IniFile);

  // LoadAnimation
  LoadAnimation := ReadArrayIndex(ILoadAnimation, IniFile, 'Advanced', 'LoadAnimation', IGNORE_INDEX, 'On');

  // ScreenFade
  ScreenFade := ReadArrayIndex(IScreenFade, IniFile, 'Advanced', 'ScreenFade', IGNORE_INDEX, 'On');

  // Visualizations
  // <mog> this could be of use later..
  //  VisualizerOption :=
  //    TVisualizerOption(GetEnumValue(TypeInfo(TVisualizerOption),
  //            IniFile.ReadString('Graphics', 'Visualization', 'Off')));
  // || VisualizerOption := TVisualizerOption(GetArrayIndex(IVisualizer, IniFile.ReadString('Graphics', 'Visualization', 'Off')));
  VisualizerOption := ReadArrayIndex(IVisualizer, IniFile, 'Graphics', 'Visualization', IGNORE_INDEX, 'Off');

{**
 * Background music
 *}
  BackgroundMusicOption := ReadArrayIndex(IBackgroundMusic, IniFile, 'Sound', 'BackgroundMusic', IGNORE_INDEX, 'On');

  // EffectSing
  EffectSing := ReadArrayIndex(IEffectSing, IniFile, 'Advanced', 'EffectSing', IGNORE_INDEX, 'On');

  // AskbeforeDel
  AskBeforeDel := ReadArrayIndex(IAskbeforeDel, IniFile, 'Advanced', 'AskbeforeDel', IGNORE_INDEX, 'On');

  // OnSongClick
  OnSongClick := ReadArrayIndex(IOnSongClick, IniFile, 'Advanced', 'OnSongClick', IGNORE_INDEX, 'Sing');

  // Linebonus
  LineBonus := ReadArrayIndex(ILineBonus, IniFile, 'Advanced', 'LineBonus', 1);

  // PartyPopup
  PartyPopup := ReadArrayIndex(IPartyPopup, IniFile, 'Advanced', 'PartyPopup', IGNORE_INDEX, 'On');

  // SingScores
  SingScores := ReadArrayIndex(ISingScores, IniFile, 'Advanced', 'SingScores', IGNORE_INDEX, 'On');

  // DuetScores
  DuetScores := ReadArrayIndex(IDuetScores, IniFile, 'Advanced', 'DuetScores', IGNORE_INDEX, 'Off');

  // TopScores
  TopScores := ReadArrayIndex(ITopScores, IniFile, 'Advanced', 'TopScores', IGNORE_INDEX, 'Player');

  // TopScreenSize
  TopScreenSize := IniFile.ReadInteger('Advanced', 'TopScreenSize', 5);

  // SyncTo
  SyncTo := ReadArrayIndex(ISyncTo, IniFile, 'Advanced', 'SyncTo', Ord(stMusic));

  // Joypad
  Joypad := ReadArrayIndex(IJoypad, IniFile, 'Controller', 'Joypad', 0);

  // Mouse
  Mouse := ReadArrayIndex(IMouse, IniFile, 'Controller', 'Mouse', 2);
  if Mouse < 0 then // try finding legacy option
  begin
    Mouse := ReadArrayIndex(IMouseLegacy, IniFile, 'Controller', 'Mouse', 2);
  end;

  // SingTimebarMode
  SingTimebarMode := ReadArrayIndex(ISingTimebarMode, IniFile, 'Advanced', 'SingTimebarMode', IGNORE_INDEX, 'Remaining');

  // JukeboxTimebarMode
  JukeboxTimebarMode := ReadArrayIndex(IJukeboxTimebarMode, IniFile, 'Advanced', 'JukeboxTimebarMode', IGNORE_INDEX, 'Current');

  // WebCam
  WebCamID := IniFile.ReadInteger('Webcam', 'ID', 0);
  WebCamResolution := ReadArrayIndex(IWebcamResolution, IniFile, 'Webcam', 'Resolution', IGNORE_INDEX, '320x240');
  if (WebCamResolution = -1) then
    WebcamResolution := 2;
  WebCamFPS := ReadArrayIndex(IWebcamFPS, IniFile, 'Webcam', 'FPS', 4);
  WebCamFlip := ReadArrayIndex(IWebcamFlipTranslated, IniFile, 'Webcam', 'Flip', IGNORE_INDEX, 'On');
  WebCamBrightness := ReadArrayIndex(IWebcamBrightness, IniFile, 'Webcam', 'Brightness', IGNORE_INDEX, '0');
  WebCamSaturation := ReadArrayIndex(IWebcamSaturation, IniFile, 'Webcam', 'Saturation', IGNORE_INDEX, '0');
  WebCamHue := ReadArrayIndex(IWebcamHue, IniFile, 'Webcam', 'Hue', IGNORE_INDEX, '0');
  WebCamEffect := IniFile.ReadInteger('Webcam', 'Effect', 0);

  // Jukebox
  JukeboxFont := ReadArrayIndex(ILyricsFont, IniFile, 'Jukebox', 'LyricsFont', 0);
  JukeboxStyle := ReadArrayIndex(ILyricsStyle, IniFile, 'Jukebox', 'LyricsStyle', 2);
  JukeboxEffect := ReadArrayIndex(ILyricsEffect, IniFile, 'Jukebox', 'LyricsEffect', 2);
  JukeboxAlpha := ReadArrayIndex(ILyricsAlpha, IniFile, 'Jukebox', 'LyricsAlpha', 20);

  JukeboxSongMenu := ReadArrayIndex(IJukeboxSongMenu, IniFile, 'Jukebox', 'SongMenu', IGNORE_INDEX, 'On');


  JukeboxSingLineColor := ReadArrayIndex(IHexSingColor, IniFile, 'Jukebox', 'SingLineColor', High(IHexSingColor));

  // other color
  if (JukeboxSingLineColor = -1) or (JukeboxSingLineColor = High(IHexSingColor)) then
  begin
    JukeboxSingLineColor := High(IHexSingColor);

    HexColor := IniFile.ReadString('Jukebox', 'SingLineColor', '47B3FF');
    Col := HexToRGB(HexColor);

    Ini.JukeboxSingLineOtherColorR := Round(Col.R);
    Ini.JukeboxSingLineOtherColorG := Round(Col.G);
    Ini.JukeboxSingLineOtherColorB := Round(Col.B);
  end;

  JukeboxActualLineColor := ReadArrayIndex(IHexGrayColor, IniFile, 'Jukebox', 'ActualLineColor', High(IHexGrayColor));

  // other color
  if (JukeboxActualLineColor = -1) or (JukeboxActualLineColor = High(IHexGrayColor)) then
  begin
    JukeboxActualLineColor := High(IHexGrayColor);

    HexColor := IniFile.ReadString('Jukebox', 'ActualLineColor', IHexGrayColor[8]);
    Col := HexToRGB(HexColor);

    Ini.JukeboxActualLineOtherColorR := Round(Col.R);
    Ini.JukeboxActualLineOtherColorG := Round(Col.G);
    Ini.JukeboxActualLineOtherColorB := Round(Col.B);
  end;

  JukeboxNextLineColor := ReadArrayIndex(IHexGrayColor, IniFile, 'Jukebox', 'NextLineColor', High(IHexGrayColor));
  // other color
  if (JukeboxNextLineColor = -1) or (JukeboxNextLineColor = High(IHexGrayColor)) then
  begin
    JukeboxNextLineColor := High(IHexGrayColor);

    HexColor := IniFile.ReadString('Jukebox', 'NextLineColor', IHexGrayColor[6]);
    Col := HexToRGB(HexColor);

    Ini.JukeboxNextLineOtherColorR := Round(Col.R);
    Ini.JukeboxNextLineOtherColorG := Round(Col.G);
    Ini.JukeboxNextLineOtherColorB := Round(Col.B);
  end;

  JukeboxSingLineOutlineColor := ReadArrayIndex(IHexOColor, IniFile, 'Jukebox', 'SingLineOColor', 0);
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

  JukeboxActualLineOutlineColor := ReadArrayIndex(IHexOColor, IniFile, 'Jukebox', 'ActualLineOColor', 0);
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

  JukeboxNextLineOutlineColor := ReadArrayIndex(IHexOColor, IniFile, 'Jukebox', 'NextLineOColor', 0);
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

  // default values
  PianoKeysLow := InitializePianoKeyArray([60, 97, 121, 115, 120, 100, 99, 118, 103, 98, 104, 110, 109, 107, 44, 108, 46, 246, 45]);
  PianoKeysHigh := InitializePianoKeyArray([49, 113, 50, 119, 51, 101, 114, 53, 116, 54, 122, 117, 56, 105, 57, 111, 48, 112, 252, 96, 43]);
  // read from config if available
  KeysLow := IniFile.ReadString('KeyBindings', 'PianoKeysLow', '');
  KeysHigh := IniFile.ReadString('KeyBindings', 'PianoKeysHigh', '');
  ReadPianoKeysLow := SplitStringToIntArray(KeysLow);
  ReadPianoKeysHigh := SplitStringToIntArray(KeysHigh);
  // only use config if it matches the expected lengths
  if Length(ReadPianoKeysLow) = 19 then
    PianoKeysLow := ReadPianoKeysLow;
  if Length(ReadPianoKeysHigh) = 21 then
    PianoKeysHigh := ReadPianoKeysHigh;

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
  try
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

    IniFile.WriteInteger('Game', 'AVDelay', AVDelay);
    IniFile.WriteInteger('Game', 'MicDelay', MicDelay);

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
    IniFile.WriteString('Graphics', 'Resolution', GetResolution);
    IniFile.WriteString('Graphics', 'ResolutionFullscreen', GetResolutionFullscreen);

    // Position
    IniFile.WriteString('Graphics', 'PositionX', IntToStr(PositionX));
    IniFile.WriteString('Graphics', 'PositionY', IntToStr(PositionY));

    // Depth
    IniFile.WriteString('Graphics', 'Depth', IDepth[Depth]);

    // TextureSize
    IniFile.WriteString('Graphics', 'TextureSize', ITextureSize[TextureSize]);

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

    // ReplayGain
    IniFile.WriteString('Sound', 'ReplayGain', IReplayGain[ReplayGain]);

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

    // Lyrics Style
    IniFile.WriteString('Lyrics', 'LyricsStyle', ILyricsStyle[LyricsStyle]);

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

    //DuetScores
    IniFile.WriteString('Advanced', 'DuetScores', IDuetScores[DuetScores]);

    //TopScores
    IniFile.WriteString('Advanced', 'TopScores', ITopScores[TopScores]);

    //TopScreenSize
    IniFile.WriteInteger('Advanced', 'TopScreenSize', TopScreenSize);

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
    IniFile.WriteString('Jukebox', 'LyricsStyle', ILyricsStyle[JukeboxStyle]);
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

    IniFile.WriteString('KeyBindings', 'PianoKeysLow', MergeIntArrayToString(PianoKeysLow));
    IniFile.WriteString('KeyBindings', 'PianoKeysHigh', MergeIntArrayToString(PianoKeysHigh));

    IniFile.Free;

  end
  except
    On e :Exception do begin
      Log.LogWarn('Saving InputDeviceConfig failed: ' + e.Message, 'UIni.Save');
    end;
  end;
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
    for I := 1 to IMaxPlayerCount do
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
    for I := 1 to IMaxPlayerCount do
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

    for I := 1 to IMaxPlayerCount do
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


function TIni.SetResolution(ResolutionString: string; RemoveCurrent: boolean; NoSave: boolean): boolean;
  var
    Index: integer;
    Dirty: boolean;
begin
  Result := false;
  Dirty := false;

  // check if current resolution is custom and then remove anyway (no matter what RemoveCurrent is set)
  if (Resolution >= 0) then
  begin
    Index := GetArrayIndex(IResolutionCustom, IResolution[Resolution]);
    if Index >= 0 then
    begin
      StringDeleteFromArray(IResolutionCustom, Index);
      StringDeleteFromArray(IResolution, Resolution);
    end;
  end;

  Index := GetArrayIndex(IResolution, ResolutionString);
  if not NoSave and (Resolution <> Index) then Dirty := true;
  if (Resolution >= 0) and (RemoveCurrent) then StringDeleteFromArray(IResolution, Resolution);
  if Index < 0 then
  begin
    SetLength(IResolution, Length(IResolution) + 1);
    IResolution[High(IResolution)] := ResolutionString;
    index := High(IResolution);
    Result := true;

    if GetArrayIndex(IResolutionCustom, ResolutionString) < 0 then
    begin
      SetLength(IResolutionCustom, Length(IResolutionCustom) + 1);
      IResolutionCustom[High(IResolutionCustom)] := ResolutionString;
    end;
  end;

  if SetResolution(index) and Dirty then
  begin
    Log.LogStatus('Resolution overridden to: ' + ResolutionString, 'Video');
    Save();
  end;
end;

function TIni.SetResolution(w,h: integer; RemoveCurrent: boolean; NoSave: boolean): boolean;
begin

  // hacky fix to support multiplied resolution (in width) in multiple screen setup (Screens=2 and more)
  // TODO: RattleSN4K3: Improve the way multiplied screen resolution is applied and stored (see UGraphics::InitializeScreen; W := W * Screens)
  if (Screens > 0) and not ((Params.Split = spmSplit ) or (Split > 0)) then w := w div (Screens+1) // integral div
  else if (Params.Screens > 0) and not ((Params.Split = spmSplit ) or (Split > 0)) then w := w div (Params.Screens+1);

  Result := SetResolution(BuildResolutionString(w, h), RemoveCurrent, NoSave);
end;

function TIni.SetResolution(index: integer): boolean;
begin
  Result := false;
  if (index >= 0) and (index < Length(IResolution)) then
  begin
      Resolution := index;
      Result := true;
  end;
end;

function TIni.GetResolution(): string;
begin
  if Resolution >= 0 then Result := IResolution[Resolution]
  else if Length(IResolution) = 0 then Result := DEFAULT_RESOLUTION
  else Result := IResolution[0];
end;

function TIni.GetResolution(out w,h: integer): string;
begin
  Result := GetResolution();
  ParseResolutionString(Result, w, h);

  // hacky fix to support multiplied resolution (in width) in multiple screen setup (Screens=2 and more)
  // TODO: RattleSN4K3: Improve the way multiplied screen resolution is applied and stored (see UGraphics::InitializeScreen; W := W * Screens)
  if (Screens > 0) and not ((Params.Split = spmSplit ) or (Split > 0)) then w := w * (Screens+1)
  else if (Params.Screens > 0) and not ((Params.Split = spmSplit ) or (Split > 0)) then w := w * (Params.Screens+1);
end;

function TIni.GetResolution(index: integer; out ResolutionString: string): boolean;
begin
  Result := false;
  if (index >= 0) and (index < Length(IResolution)) then
  begin
      ResolutionString := IResolution[index];
      Result := true;
  end;
end;

function TIni.GetResolutionFullscreen(): string;
begin
  if ResolutionFullscreen >= 0 then Result := IResolutionFullScreen[ResolutionFullscreen]
  else if Length(IResolutionFullScreen) = 0 then Result := DEFAULT_RESOLUTION
  else Result := IResolutionFullScreen[0];
end;

function TIni.GetResolutionFullscreen(out w,h: integer): string;
begin
  Result := GetResolutionFullscreen();
  ParseResolutionString(Result, w, h);
end;

function TIni.GetResolutionFullscreen(index: integer; out ResolutionString: string): boolean;
begin
  Result := false;
  if (index >= 0) and (index < Length(IResolutionFullScreen)) then
  begin
      ResolutionString := IResolutionFullScreen[index];
      Result := true;
  end;
end;

procedure TIni.ClearCustomResolutions();
  var
    Index, i, custom: integer;
    ResString: string;
begin
  if Resolution < 0 then Exit;

  // check if current resolution is a custom one
  ResString := IResolution[Resolution];
  Index := GetArrayIndex(IResolutionCustom, ResString);
  for i := High(IResolutionCustom) downto 0 do
  begin
    custom := GetArrayIndex(IResolution, IResolutionCustom[i]);
    if (custom >= 0) and (Index <> i) then
    begin
      StringDeleteFromArray(IResolution, custom);
      StringDeleteFromArray(IResolutionCustom, i);
    end;
  end;

  // update index
  Resolution := GetArrayIndex(IResolution, ResString);
end;

end.
