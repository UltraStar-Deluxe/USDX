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
  ULog,
  SysUtils;

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
    ChannelToPlayerMap: array of integer;
  end;

type

//Options

  TVisualizerOption      = (voOff, voWhenNoVideo, voOn);
  TBackgroundMusicOption = (bmoOff, bmoOn);
  TIni = class
    private
      function RemoveFileExt(FullName: string): string;
      function ExtractKeyIndex(const Key, Prefix, Suffix: string): integer;
      function GetMaxKeyIndex(Keys: TStringList; const Prefix, Suffix: string): integer;
      function GetArrayIndex(const SearchArray: array of string; Value: string; CaseInsensitiv: Boolean = False): integer;
      function ReadArrayIndex(const SearchArray: array of string; IniFile: TCustomIniFile;
          IniSection: string; IniProperty: string; Default: integer): integer;

      procedure LoadInputDeviceCfg(IniFile: TMemIniFile);
      procedure SaveInputDeviceCfg(IniFile: TIniFile);
      procedure LoadThemes(IniFile: TCustomIniFile);
      procedure LoadPaths(IniFile: TCustomIniFile);
      procedure LoadScreenModes(IniFile: TCustomIniFile);

    public
      Name:           array[0..11] of string;

      // Templates for Names Mod
      NameTeam:       array[0..2] of string;
      NameTemplate:   array[0..11] of string;

      //Filename of the opened iniFile
      Filename:       string;

      // Game
      Players:        integer;
      Difficulty:     integer;
      Language:       integer;
      Tabs:           integer;
      Tabs_at_startup:integer; //Tabs at Startup fix
      Sorting:        integer;
      Debug:          integer;

      // Graphics
      Screens:        integer;
      Resolution:     integer;
      Depth:          integer;
      VisualizerOption:integer; 
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
      AudioOutputBufferSizeIndex:integer;
      VoicePassthrough:integer;

      //Song Preview
      PreviewVolume:  integer;
      PreviewFading:  integer;

      // Lyrics
      LyricsFont:     integer;
      LyricsEffect:   integer;
      Solmization:    integer;
      NoteLines:      integer;

      // Themes
      Theme:          integer;
      SkinNo:         integer;
      Color:          integer;
      BackgroundMusicOption:integer;
      
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

      procedure Load();
      procedure Save();
      procedure SaveNames;
      procedure SaveLevel;
  end;

var
  Ini:            TIni;
  IResolution:    array of string;
  ILanguage:      array of string;
  ITheme:         array of string;
  ISkin:          array of string;



const
  IPlayers:       array[0..4] of string  = ('1', '2', '3', '4', '6');
  IPlayersVals:   array[0..4] of integer = ( 1 ,  2 ,  3 ,  4 ,  6 );

  IDifficulty:    array[0..2] of string = ('Easy', 'Medium', 'Hard');
  ITabs:          array[0..1] of string = ('Off', 'On');

  ISorting:       array[0..7] of string = ('Edition', 'Genre', 'Language', 'Folder', 'Title', 'Artist', 'Title2', 'Artist2');
  sEdition  = 0;
  sGenre    = 1;
  sLanguage = 2;
  sFolder   = 3;
  sTitle    = 4;
  sArtist   = 5;
  sTitle2   = 6;
  sArtist2  = 7;

  IDebug:         array[0..1] of string = ('Off', 'On');

  IScreens:       array[0..1] of string = ('1', '2');
  IFullScreen:    array[0..1] of string = ('Off', 'On');
  IDepth:         array[0..1] of string = ('16 bit', '32 bit');
  IVisualizer:    array[0..2] of string = ('Off', 'WhenNoVideo','On');

  IBackgroundMusic: array[0..1] of string = ('Off', 'On');


  ITextureSize:     array[0..2] of string  = ('128', '256', '512');
  ITextureSizeVals: array[0..2] of integer = ( 128,   256,   512);

  ISingWindow:    array[0..1] of string = ('Small', 'Big');

  //SingBar Mod
  IOscilloscope:  array[0..2] of string = ('Off', 'Osci', 'Bar');
//IOscilloscope:  array[0..1] of string = ('Off', 'On');

  ISpectrum:      array[0..1] of string = ('Off', 'On');
  ISpectrograph:  array[0..1] of string = ('Off', 'On');
  IMovieSize:     array[0..2] of string = ('Half', 'Full [Vid]', 'Full [BG+Vid]');

  IClickAssist:   array[0..1] of string = ('Off', 'On');
  IBeatClick:     array[0..1] of string = ('Off', 'On');
  ISavePlayback:  array[0..1] of string = ('Off', 'On');

  IThreshold:     array[0..3] of string = ('5%', '10%', '15%', '20%');
  IThresholdVals: array[0..3] of single = (0.05, 0.10,  0.15,  0.20);

  IVoicePassthrough:  array[0..1] of string = ('Off', 'On');

  IAudioOutputBufferSize:     array[0..9] of string  = ('Auto', '256', '512', '1024', '2048', '4096', '8192', '16384', '32768', '65536');
  IAudioOutputBufferSizeVals: array[0..9] of integer = ( 0,      256,   512 ,  1024 ,  2048 ,  4096 ,  8192 ,  16384 ,  32768 ,  65536 );

  IAudioInputBufferSize:     array[0..9] of string  = ('Auto', '256', '512', '1024', '2048', '4096', '8192', '16384', '32768', '65536');
  IAudioInputBufferSizeVals: array[0..9] of integer = ( 0,      256,   512 ,  1024 ,  2048 ,  4096 ,  8192 ,  16384 ,  32768 ,  65536 );

  //Song Preview
  IPreviewVolume:     array[0..10] of string = ('Off', '10%', '20%', '30%', '40%', '50%', '60%', '70%', '80%', '90%', '100%');
  IPreviewVolumeVals: array[0..10] of single = ( 0,   0.10,  0.20,  0.30,  0.40,  0.50,  0.60,  0.70,  0.80,  0.90,   1.00  );

  IPreviewFading:     array[0..5] of string  = ('Off', '1 Sec', '2 Secs', '3 Secs', '4 Secs', '5 Secs');
  IPreviewFadingVals: array[0..5] of integer = ( 0,     1,       2,        3,        4,        5      );


  ILyricsFont:    array[0..2] of string = ('Plain', 'OLine1', 'OLine2');
  ILyricsEffect:  array[0..4] of string = ('Simple', 'Zoom', 'Slide', 'Ball', 'Shift');
  ISolmization:   array[0..3] of string = ('Off', 'Euro', 'Jap', 'American');
  INoteLines:     array[0..1] of string = ('Off', 'On');

  IColor:         array[0..8] of string = ('Blue', 'Green', 'Pink', 'Red', 'Violet', 'Orange', 'Yellow', 'Brown', 'Black');

  // Advanced
  ILoadAnimation: array[0..1] of string = ('Off', 'On');
  IEffectSing:    array[0..1] of string = ('Off', 'On');
  IScreenFade:    array[0..1] of string =('Off', 'On');
  IAskbeforeDel:  array[0..1] of string = ('Off', 'On');
  IOnSongClick:   array[0..2] of string = ('Sing', 'Select Players', 'Open Menu');
  ILineBonus:     array[0..2] of string = ('Off', 'At Score', 'At Notes');
  IPartyPopup:    array[0..1] of string = ('Off', 'On');

  IJoypad:        array[0..1] of string = ('Off', 'On');

  // Recording options
  IChannelPlayer: array[0..6] of string = ('Off', '1', '2', '3', '4', '5', '6');
  IMicBoost:      array[0..3] of string = ('Off', '+6dB', '+12dB', '+18dB');

implementation

uses
  StrUtils,
  UMain,
  SDL,
  ULanguage,
  UPlatform,
  USkins,
  URecord,
  UCommandLine;

(**
 * Returns the filename without its fileextension
 *)
function TIni.RemoveFileExt(FullName: string): string;
begin
  Result := ChangeFileExt(FullName, '');
end;

(**
 * Extracts an index of a key that is surrounded by a Prefix/Suffix pair.
 * Example: ExtractKeyIndex('MyKey[1]', '[', ']') will return 1.
 *)
function TIni.ExtractKeyIndex(const Key, Prefix, Suffix: string): integer;
var
  Value: string;
  Start: integer;
begin
  Result := -1;

  if Pos(Prefix, Key) > -1 then
  begin
    Start  := Pos(Prefix, Key) + Length(Prefix);

    // copy all between prefix and suffix
    Value  := Copy(Key, Start, Pos(Suffix, Key)-1 - Start);
    Result := StrToIntDef(Value, -1);
  end;
end;

(**
 * Finds the maximum key-index in a key-list.
 * The indexes of the list are surrounded by Prefix/Suffix,
 * e.g. MyKey[1] (Prefix='[', Suffix=']')
 *)
function TIni.GetMaxKeyIndex(Keys: TStringList; const Prefix, Suffix: string): integer;
var
  i: integer;
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
 * Returns the index of Value in SearchArray
 * or -1 if Value is not in SearchArray.
 *)
function TIni.GetArrayIndex(const SearchArray: array of string; Value: string;
    CaseInsensitiv: Boolean = False): integer;
var
  i: integer;
begin
  Result := -1;

  for i := 0 to High(SearchArray) do
  begin
    if (SearchArray[i] = Value) or
       (CaseInsensitiv and (UpperCase(SearchArray[i]) = UpperCase(Value))) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

(**
 * Reads the property IniSeaction:IniProperty from IniFile and
 * finds its corresponding index in SearchArray.
 * If SearchArray does not contain the property value, the default value is
 * returned.
 *)
function TIni.ReadArrayIndex(const SearchArray: array of string; IniFile: TCustomIniFile;
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
  DeviceCfg: PInputDeviceConfig;
  DeviceIndex: integer;
  ChannelCount: integer;
  ChannelIndex: integer;
  RecordKeys: TStringList;
  i: integer;
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
        break;

      // resize list
      SetLength(InputDeviceConfig, Length(InputDeviceConfig)+1);

      // read an input device's config.
      // Note: All devices are appended to the list whether they exist or not.
      //   Otherwise an external device's config will be lost if it is not
      //   connected (e.g. singstar mics or USB-Audio devices).
      DeviceCfg := @InputDeviceConfig[High(InputDeviceConfig)];
      DeviceCfg.Name := IniFile.ReadString('Record', Format('DeviceName[%d]', [DeviceIndex]), '');
      DeviceCfg.Input := IniFile.ReadInteger('Record', Format('Input[%d]', [DeviceIndex]), 0);

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
  //MicBoost := GetArrayIndex(IMicBoost, IniFile.ReadString('Record', 'MicBoost', 'Off'));
  // Threshold
  //  ThresholdIndex := GetArrayIndex(IThreshold, IniFile.ReadString('Record', 'Threshold', IThreshold[1]));
end;

procedure TIni.SaveInputDeviceCfg(IniFile: TIniFile);
var
  DeviceIndex:    integer;
  ChannelIndex:   integer;
begin
  for DeviceIndex := 0 to High(InputDeviceConfig) do
  begin
    // DeviceName and DeviceInput
    IniFile.WriteString('Record', Format('DeviceName[%d]', [DeviceIndex+1]),
                        InputDeviceConfig[DeviceIndex].Name);
    IniFile.WriteInteger('Record', Format('Input[%d]', [DeviceIndex+1]),
                        InputDeviceConfig[DeviceIndex].Input);
                        
    // Channel-to-Player Mapping
    for ChannelIndex := 0 to High(InputDeviceConfig[DeviceIndex].ChannelToPlayerMap) do
    begin
      IniFile.WriteInteger('Record',
                          Format('Channel%d[%d]', [ChannelIndex+1, DeviceIndex+1]),
                          InputDeviceConfig[DeviceIndex].ChannelToPlayerMap[ChannelIndex]);
    end;
  end;

  // MicBoost
  //IniFile.WriteString('Record', 'MicBoost', IMicBoost[MicBoost]);
  // Threshold
  //IniFile.WriteString('Record', 'Threshold', IThreshold[ThresholdIndex]);
end;

procedure TIni.LoadPaths(IniFile: TCustomIniFile);
var
  PathStrings: TStringList;
  I: integer;
begin
  PathStrings := TStringList.Create;
  IniFile.ReadSection('Directories', PathStrings);

  // Load song-paths
  for I := 0 to PathStrings.Count-1 do
  begin
    if (AnsiStartsText('SongDir', PathStrings[I])) then
    begin
      AddSongPath(IniFile.ReadString('Directories', PathStrings[I], ''));
    end;
  end;

  PathStrings.Free;
end;

procedure TIni.LoadThemes(IniFile: TCustomIniFile);
var
  SearchResult: TSearchRec;
  ThemeIni:     TMemIniFile;
  ThemeName:    string;
  I: integer;
begin
  // Theme
  SetLength(ITheme, 0);
  Log.LogStatus('Searching for Theme : ' + ThemePath + '*.ini', 'Theme');

  FindFirst(ThemePath + '*.ini',faAnyFile, SearchResult);
  Repeat
    Log.LogStatus('Found Theme: ' + SearchResult.Name, 'Theme');

    //Read Themename from Theme
    ThemeIni := TMemIniFile.Create(SearchResult.Name);
    ThemeName := UpperCase(ThemeIni.ReadString('Theme','Name', RemoveFileExt(SearchResult.Name)));
    ThemeIni.Free;

    //Search for Skins for this Theme
    for I := Low(Skin.Skin) to High(Skin.Skin) do
    begin
      if UpperCase(Skin.Skin[I].Theme) = ThemeName then
      begin
        SetLength(ITheme, Length(ITheme)+1);
        ITheme[High(ITheme)] := RemoveFileExt(SearchResult.Name);
        break;
      end;
    end;
  until FindNext(SearchResult) <> 0;
  FindClose(SearchResult);

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

  SkinNo := GetArrayIndex(ISkin, IniFile.ReadString('Themes',    'Skin',   ISkin[0]));
end;

procedure TIni.LoadScreenModes(IniFile: TCustomIniFile);

  // swap two strings
  procedure swap(var s1, s2: string);
  var
    s3: string;
  begin
    s3 := s1;
    s1 := s2;
    s2 := s3;
  end;

var
  Modes: PPSDL_Rect;
  I: integer;
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
    SetLength(IResolution, 10);
    IResolution[0] := '640x480';
    IResolution[1] := '800x600';
    IResolution[2] := '1024x768';
    IResolution[3] := '1152x864';
    IResolution[4] := '1280x800';
    IResolution[5] := '1280x960';
    IResolution[6] := '1400x1050';
    IResolution[7] := '1440x900';
    IResolution[8] := '1600x1200';
    IResolution[9] := '1680x1050';
      
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
  I: integer;
begin
  GamePath := Platform.GetGameUserPath;

  Log.LogStatus( 'GamePath : ' +GamePath , '' );

  if (Params.ConfigFile <> '') then
    try
      FileName := Params.ConfigFile;
    except
      FileName := GamePath + 'config.ini';
    end
  else
    FileName := GamePath + 'config.ini';

  Log.LogStatus( 'Using config : ' + FileName , 'Ini');
  IniFile := TMemIniFile.Create( FileName );

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
  //Language.ChangeLanguage(ILanguage[Language]);
  
  // Tabs
  Tabs := GetArrayIndex(ITabs, IniFile.ReadString('Game', 'Tabs', ITabs[0]));
  Tabs_at_startup := Tabs;	//Tabs at Startup fix
  
  // Song Sorting
  Sorting := GetArrayIndex(ISorting, IniFile.ReadString('Game', 'Sorting', ISorting[0]));
  
  // Debug
  Debug := GetArrayIndex(IDebug, IniFile.ReadString('Game', 'Debug', IDebug[0]));

  LoadScreenModes(IniFile);

  // TextureSize
  TextureSize := GetArrayIndex(ITextureSize, IniFile.ReadString('Graphics', 'TextureSize', ITextureSize[1]));

  // SingWindow
  SingWindow := GetArrayIndex(ISingWindow, IniFile.ReadString('Graphics', 'SingWindow', 'Big'));

  // Oscilloscope
  Oscilloscope := GetArrayIndex(IOscilloscope, IniFile.ReadString('Graphics', 'Oscilloscope', 'Bar'));

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
  PreviewFading := GetArrayIndex(IPreviewFading, IniFile.ReadString('Sound', 'PreviewFading', IPreviewFading[1]));

  //AudioRepeat aka VoicePassthrough
  VoicePassthrough := GetArrayIndex(IVoicePassthrough, IniFile.ReadString('Sound', 'VoicePassthrough', IVoicePassthrough[0]));

  // Lyrics Font
  LyricsFont := GetArrayIndex(ILyricsFont, IniFile.ReadString('Lyrics', 'LyricsFont', ILyricsFont[1]));

  // Lyrics Effect
  LyricsEffect := GetArrayIndex(ILyricsEffect, IniFile.ReadString('Lyrics', 'LyricsEffect', ILyricsEffect[1]));

  // Solmization
  Solmization := GetArrayIndex(ISolmization, IniFile.ReadString('Lyrics', 'Solmization', ISolmization[0]));

  // NoteLines
  NoteLines := GetArrayIndex(INoteLines, IniFile.ReadString('Lyrics', 'NoteLines', INoteLines[1]));

  LoadThemes(IniFile);

  // Color
  Color := GetArrayIndex(IColor, IniFile.ReadString('Themes',    'Color',   IColor[0]));

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
  BackgroundMusicOption := GetArrayIndex(IBackgroundMusic, IniFile.ReadString('Sound', 'BackgroundMusic', 'Off'));

  // EffectSing
  EffectSing := GetArrayIndex(IEffectSing, IniFile.ReadString('Advanced', 'EffectSing', 'On'));

  // AskbeforeDel
  AskBeforeDel := GetArrayIndex(IAskbeforeDel, IniFile.ReadString('Advanced', 'AskbeforeDel', 'On'));

  // OnSongClick
  OnSongClick := GetArrayIndex(IOnSongClick, IniFile.ReadString('Advanced', 'OnSongClick', 'Sing'));

  // Linebonus
  LineBonus := GetArrayIndex(ILineBonus, IniFile.ReadString('Advanced', 'LineBonus', 'At Score'));

  // PartyPopup
  PartyPopup := GetArrayIndex(IPartyPopup, IniFile.ReadString('Advanced', 'PartyPopup', 'On'));

  // Joypad
  Joypad := GetArrayIndex(IJoypad, IniFile.ReadString('Controller',    'Joypad',   IJoypad[0]));

  LoadPaths(IniFile);
  
  IniFile.Free;
end;

procedure TIni.Save;
var
  IniFile:    TIniFile;
begin
  if (FileExists(Filename) and FileIsReadOnly(Filename)) then
  begin
    Log.LogError('Config-file is read-only', 'TIni.Save');
    Exit;
  end;

  IniFile := TIniFile.Create(Filename);

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

  // Solmization
  IniFile.WriteString('Lyrics', 'Solmization', ISolmization[Solmization]);

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

  // Joypad
  IniFile.WriteString('Controller', 'Joypad', IJoypad[Joypad]);

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
  if not FileIsReadOnly(Filename) then
  begin
    IniFile := TIniFile.Create(Filename);

    //Name Templates for Names Mod
    for I := 1 to 12 do
      IniFile.WriteString('Name', 'P' + IntToStr(I), Name[I-1]);
    for I := 1 to 3 do
      IniFile.WriteString('NameTeam', 'T' + IntToStr(I), NameTeam[I-1]);
    for I := 1 to 12 do
      IniFile.WriteString('NameTemplate', 'Name' + IntToStr(I), NameTemplate[I-1]);

    IniFile.Free;
  end;
end;

procedure TIni.SaveLevel;
var
  IniFile: TIniFile;
begin
  if not FileIsReadOnly(Filename) then
  begin
    IniFile := TIniFile.Create(Filename);

    // Difficulty
    IniFile.WriteString('Game', 'Difficulty', IDifficulty[Difficulty]);

    IniFile.Free;
  end;
end;

end.
