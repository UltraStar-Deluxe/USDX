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
  // TInputDeviceConfig stores the configuration for an input device.
  // Configurations will be stored in the InputDeviceConfig array.
  // Note that not all devices listed in InputDeviceConfig are active devices.
  // Some might be unplugged and hence unavailable.
  // Available devices are held in TAudioInputProcessor.DeviceList. Each
  // TAudioInputDevice listed there has a CfgIndex field which is the index to
  // its configuration in the InputDeviceConfig array.
  // Name:
  //   the name of the input device
  // Input:
  //   the index of the input source to use for recording
  // ChannelToPlayerMap:
  //   mapping of recording channels to players, e.g. ChannelToPlayerMap[0] = 2
  //   maps the channel 0 (left) to player 2. A player index of 0 means that
  //   the channel is not assigned to a player.
  PInputDeviceConfig = ^TInputDeviceConfig;
  TInputDeviceConfig = record
    Name:               string;
    Input:              integer;
    Latency:            integer; //**< latency in ms, or LATENCY_AUTODETECT for default
    ChannelToPlayerMap: array of integer;
  end;

const
  LATENCY_AUTODETECT = -1;

type

//Options

  TVisualizerOption      = (voOff, voWhenNoVideo, voOn);
  TBackgroundMusicOption = (bmoOff, bmoOn);
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

    public
      Name:           array[0..11] of UTF8String;

      // Templates for Names Mod
      NameTeam:       array[0..2] of UTF8String;
      NameTemplate:   array[0..11] of UTF8String;

      //Filename of the opened iniFile
      Filename:       IPath;

      // Game
      Players:        integer;
      Difficulty:     integer;
      Language:       integer;
      Tabs:           integer;
      TabsAtStartup:  integer; //Tabs at Startup fix
      Sorting:        integer;
      Debug:          integer;

      // Graphics
      Screens:        integer;
      Resolution:     integer;
      Depth:          integer;
      VisualizerOption: integer;
      FullScreen:     integer;
      TextureSize:    integer;
      SingWindow:     integer;
      Oscilloscope:   integer;
      Spectrum:       integer;
      Spectrograph:   integer;
      MovieSize:      integer;

      // Sound
      MicBoost:       integer;
      ClickAssist:    integer;
      BeatClick:      integer;
      SavePlayback:   integer;
      ThresholdIndex: integer;
      AudioOutputBufferSizeIndex: integer;
      VoicePassthrough: integer;

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

      // Controller
      Joypad:         integer;
      Mouse:          integer;

      procedure Load();
      procedure Save();
      procedure SaveNames;
      procedure SaveLevel;
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
  ISorting:     array[0..6] of UTF8String = ('Edition', 'Genre', 'Language', 'Folder', 'Title', 'Artist', 'Artist2');
type
  TSortingType = (sEdition, sGenre, sLanguage, sFolder, sTitle, sArtist, sArtist2);

const  
  IDebug:            array[0..1] of UTF8String  = ('Off', 'On');

  IScreens:          array[0..1] of UTF8String  = ('1', '2');
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
  INoteLines:     array[0..1] of UTF8String = ('Off', 'On');

  IColor:         array[0..8] of UTF8String = ('Blue', 'Green', 'Pink', 'Red', 'Violet', 'Orange', 'Yellow', 'Brown', 'Black');

  // Advanced
  ILoadAnimation: array[0..1] of UTF8String = ('Off', 'On');
  IEffectSing:    array[0..1] of UTF8String = ('Off', 'On');
  IScreenFade:    array[0..1] of UTF8String = ('Off', 'On');
  IAskbeforeDel:  array[0..1] of UTF8String = ('Off', 'On');
  IOnSongClick:   array[0..2] of UTF8String = ('Sing', 'Select Players', 'Open Menu');
  sStartSing = 0;
  sSelectPlayer = 1;
  sOpenMenu = 2;

  ILineBonus:     array[0..1] of UTF8String = ('Off', 'On');
  IPartyPopup:    array[0..1] of UTF8String = ('Off', 'On');

  IJoypad:        array[0..1] of UTF8String = ('Off', 'On');
  IMouse:         array[0..2] of UTF8String = ('Off', 'Hardware Cursor', 'Software Cursor');

  // Recording options
  IChannelPlayer: array[0..6] of UTF8String = ('Off', '1', '2', '3', '4', '5', '6');
  IMicBoost:      array[0..3] of UTF8String = ('Off', '+6dB', '+12dB', '+18dB');

{*
 * Translated options
 *}

var
  ILanguageTranslated:         array of UTF8String;

  IDifficultyTranslated:       array[0..2] of UTF8String  = ('Easy', 'Medium', 'Hard');
  ITabsTranslated:             array[0..1] of UTF8String  = ('Off', 'On');

  ISortingTranslated:          array[0..6] of UTF8String  = ('Edition', 'Genre', 'Language', 'Folder', 'Title', 'Artist', 'Artist2');

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

  // Advanced
  ILoadAnimationTranslated:    array[0..1] of UTF8String = ('Off', 'On');
  IEffectSingTranslated:       array[0..1] of UTF8String = ('Off', 'On');
  IScreenFadeTranslated:       array[0..1] of UTF8String = ('Off', 'On');
  IAskbeforeDelTranslated:     array[0..1] of UTF8String = ('Off', 'On');
  IOnSongClickTranslated:      array[0..2] of UTF8String = ('Sing', 'Select Players', 'Open Menu');
  ILineBonusTranslated:        array[0..1] of UTF8String = ('Off', 'On');
  IPartyPopupTranslated:       array[0..1] of UTF8String = ('Off', 'On');

  IJoypadTranslated:           array[0..1] of UTF8String = ('Off', 'On');
  IMouseTranslated:            array[0..2] of UTF8String = ('Off', 'Hardware Cursor', 'Software Cursor');

  // Recording options
  IChannelPlayerTranslated:    array[0..6] of UTF8String = ('Off', '1', '2', '3', '4', '5', '6');
  IMicBoostTranslated:         array[0..3] of UTF8String = ('Off', '+6dB', '+12dB', '+18dB');

implementation

uses
  StrUtils,
  SDL,
  UCommandLine,
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
  
  ISortingTranslated[0]               := ULanguage.Language.Translate('OPTION_VALUE_EDITION');
  ISortingTranslated[1]               := ULanguage.Language.Translate('OPTION_VALUE_GENRE');
  ISortingTranslated[2]               := ULanguage.Language.Translate('OPTION_VALUE_LANGUAGE');
  ISortingTranslated[3]               := ULanguage.Language.Translate('OPTION_VALUE_FOLDER');
  ISortingTranslated[4]               := ULanguage.Language.Translate('OPTION_VALUE_TITLE');
  ISortingTranslated[5]               := ULanguage.Language.Translate('OPTION_VALUE_ARTIST');
  ISortingTranslated[6]               := ULanguage.Language.Translate('OPTION_VALUE_ARTIST2');

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

  IColorTranslated[0]                 := ULanguage.Language.Translate('OPTION_VALUE_BLUE');
  IColorTranslated[1]                 := ULanguage.Language.Translate('OPTION_VALUE_GREEN');
  IColorTranslated[2]                 := ULanguage.Language.Translate('OPTION_VALUE_PINK');
  IColorTranslated[3]                 := ULanguage.Language.Translate('OPTION_VALUE_RED');
  IColorTranslated[4]                 := ULanguage.Language.Translate('OPTION_VALUE_VIOLET');
  IColorTranslated[5]                 := ULanguage.Language.Translate('OPTION_VALUE_ORANGE');
  IColorTranslated[6]                 := ULanguage.Language.Translate('OPTION_VALUE_YELLOW');
  IColorTranslated[7]                 := ULanguage.Language.Translate('OPTION_VALUE_BROWN');
  IColorTranslated[8]                 := ULanguage.Language.Translate('OPTION_VALUE_BLACK');

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

  IJoypadTranslated[0]                := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IJoypadTranslated[1]                := ULanguage.Language.Translate('OPTION_VALUE_ON');

  IMouseTranslated[0]                 := ULanguage.Language.Translate('OPTION_VALUE_OFF');
  IMouseTranslated[1]                 := ULanguage.Language.Translate('OPTION_VALUE_HARDWARE_CURSOR');
  IMouseTranslated[2]                 := ULanguage.Language.Translate('OPTION_VALUE_SOFTWARE_CURSOR');

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
          IniFile.ReadInteger('Record', Format('Channel%d[%d]', [ChannelIndex+1, DeviceIndex]), 0);
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
  // Screens
  Screens := GetArrayIndex(IScreens, IniFile.ReadString('Graphics', 'Screens', IScreens[0]));

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
      IResolution[High(IResolution)] := IntToStr(Modes^.w) + 'x' + IntToStr(Modes^.h);
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

  // Tabs
  Tabs := GetArrayIndex(ITabs, IniFile.ReadString('Game', 'Tabs', ITabs[0]));
  TabsAtStartup := Tabs;	//Tabs at Startup fix

  // Song Sorting
  Sorting := GetArrayIndex(ISorting, IniFile.ReadString('Game', 'Sorting', ISorting[Ord(sEdition)]));

  // Debug
  Debug := GetArrayIndex(IDebug, IniFile.ReadString('Game', 'Debug', IDebug[0]));

  LoadScreenModes(IniFile);

  // TextureSize (aka CachedCoverSize)
  // Note: a default cached cover size of 128 pixels is big enough,
  // 256 pixels are already noticeably slow with 180 covers in the song-screen
  // displayed at once. In additon the covers.db will be too big.
  TextureSize := GetArrayIndex(ITextureSize, IniFile.ReadString('Graphics', 'TextureSize', '128'));

  // SingWindow
  SingWindow := GetArrayIndex(ISingWindow, IniFile.ReadString('Graphics', 'SingWindow', 'Big'));

  // Oscilloscope
  Oscilloscope := GetArrayIndex(IOscilloscope, IniFile.ReadString('Graphics', 'Oscilloscope', IOscilloscope[0]));

  // Spectrum
  Spectrum := GetArrayIndex(ISpectrum, IniFile.ReadString('Graphics', 'Spectrum', 'Off'));

  // Spectrograph
  Spectrograph := GetArrayIndex(ISpectrograph, IniFile.ReadString('Graphics', 'Spectrograph', 'Off'));

  // MovieSize
  MovieSize := GetArrayIndex(IMovieSize, IniFile.ReadString('Graphics', 'MovieSize', IMovieSize[2]));

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

  // Lyrics Font
  LyricsFont := GetArrayIndex(ILyricsFont, IniFile.ReadString('Lyrics', 'LyricsFont', ILyricsFont[0]));

  // Lyrics Effect
  LyricsEffect := GetArrayIndex(ILyricsEffect, IniFile.ReadString('Lyrics', 'LyricsEffect', ILyricsEffect[4]));

  // NoteLines
  NoteLines := GetArrayIndex(INoteLines, IniFile.ReadString('Lyrics', 'NoteLines', INoteLines[1]));

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

  // SyncTo
  SyncTo := GetArrayIndex(ISyncTo, IniFile.ReadString('Advanced', 'SyncTo', ISyncTo[Ord(stMusic)]));

  // Joypad
  Joypad := GetArrayIndex(IJoypad, IniFile.ReadString('Controller',    'Joypad',   IJoypad[0]));

  // Mouse
  Mouse := GetArrayIndex(IMouse, IniFile.ReadString('Controller',    'Mouse',   IMouse[2]));

  LoadPaths(IniFile);

  TranslateOptionValues;

  IniFile.Free;
end;

procedure TIni.Save;
var
  IniFile: TIniFile;
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

  // Sorting
  IniFile.WriteString('Game', 'Sorting', ISorting[Sorting]);

  // Debug
  IniFile.WriteString('Game', 'Debug', IDebug[Debug]);

  // Screens
  IniFile.WriteString('Graphics', 'Screens', IScreens[Screens]);

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
  IniFile.WriteString('Graphics', 'Spectrum', ISpectrum[Spectrum]);

  // Spectrograph
  IniFile.WriteString('Graphics', 'Spectrograph', ISpectrograph[Spectrograph]);

  // Movie Size
  IniFile.WriteString('Graphics', 'MovieSize', IMovieSize[MovieSize]);

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

  //SyncTo
  IniFile.WriteString('Advanced', 'SyncTo', ISyncTo[SyncTo]);

  // Joypad
  IniFile.WriteString('Controller', 'Joypad', IJoypad[Joypad]);

  // Mouse
  IniFile.WriteString('Controller', 'Mouse', IMouse[Mouse]);

  // Directories (add a template if section is missing)
  // Note: Value must be ' ' and not '', otherwise no key is generated on Linux
  if (not IniFile.SectionExists('Directories')) then
    IniFile.WriteString('Directories', 'SongDir1', ' ');

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

end.
