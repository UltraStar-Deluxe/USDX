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
  PInputDeviceConfig = ^TInputDeviceConfig;
  TInputDeviceConfig = record
    Name:               string;
    Input:              integer;
    ChannelToPlayerMap: array of integer;
  end;

type
  TIni = class
    private
      function ExtractKeyIndex(const key, prefix, suffix: String): Integer;
      function GetMaxKeyIndex(keys: TStringList; const prefix, suffix: String): Integer;
      procedure LoadInputDeviceCfg(IniFile: TMemIniFile);
      procedure SaveInputDeviceCfg(IniFile: TIniFile);
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
      Threshold:      integer;
      SDLBufferSize:  integer;

      //Song Preview
      PreviewVolume: integer;
      PreviewFading: integer;

      // Lyrics
      LyricsFont:     integer;
      LyricsEffect:   integer;
      Solmization:    integer;

      // Themes
      Theme:          integer;
      SkinNo:           integer;
      Color:          integer;

      // Record
      InputDeviceConfig: array of TInputDeviceConfig;

      // Advanced
      LoadAnimation:  integer;
      EffectSing:     integer;
      ScreenFade:     integer;
      AskbeforeDel:   integer;
      OnSongClick:    integer;
      LineBonus:      integer;
      PartyPopup:     integer;

      // Controller
      Joypad:         integer;

      // Soundcards
      SoundCard:      array[0..7, 1..2] of integer;

      // Devices
      LPT:            integer;

      procedure Load();
      procedure LoadSoundSettings();
      
      procedure Save();
      procedure SaveNames;
      procedure SaveLevel;
  end;


var
  Ini:    TIni;
  IResolution:    array of string;
  ILanguage:      array of string;
  ITheme:         array of string;
  ISkin:          array of string;

const
  IPlayers:       array[0..4] of string = ('1', '2', '3', '4', '6');
  IDifficulty:    array[0..2] of string = ('Easy', 'Medium', 'Hard');
  ITabs:          array[0..1] of string = ('Off', 'On');

  ISorting:       array[0..7] of string = ('Edition', 'Genre', 'Language', 'Folder', 'Title', 'Artist', 'Title2', 'Artist2');
  sEdition = 0;
  sGenre = 1;
  sLanguage = 2;
  sFolder = 3;
  sTitle = 4;
  sArtist = 5;
  sTitle2 = 6;
  sArtist2 = 7;

  IDebug:         array[0..1] of string = ('Off', 'On');

  IScreens:       array[0..1] of string = ('1', '2');
  IFullScreen:    array[0..1] of string = ('Off', 'On');
  IDepth:         array[0..1] of string = ('16 bit', '32 bit');
  ITextureSize:   array[0..2] of string = ('128', '256', '512');
  ISingWindow:    array[0..1] of string = ('Small', 'Big');

  //SingBar Mod
  IOscilloscope:  array[0..2] of string = ('Off', 'Osci', 'Bar');
  //IOscilloscope:  array[0..1] of string = ('Off', 'On');

  ISpectrum:      array[0..1] of string = ('Off', 'On');
  ISpectrograph:  array[0..1] of string = ('Off', 'On');
  IMovieSize:     array[0..2] of string = ('Half', 'Full [Vid]', 'Full [BG+Vid]');

  IMicBoost:      array[0..3] of string = ('Off', '+6dB', '+12dB', '+18dB');
  IClickAssist:   array[0..1] of string = ('Off', 'On');
  IBeatClick:     array[0..1] of string = ('Off', 'On');
  ISavePlayback:  array[0..1] of string = ('Off', 'On');
  IThreshold:     array[0..3] of string = ('5%', '10%', '15%', '20%');
  ISDLBufferSize:    array[0..8] of string = ('256', '512', '1024', '2048', '4096', '8192', '16384', '32768', '65536');
  
  //Song Preview
  IPreviewVolume: array[0..10] of string = ('Off', '10%', '20%', '30%', '40%', '50%', '60%', '70%', '80%', '90%', '100%');
  IPreviewFading: array[0..5] of string  = ('Off', '1 Sec', '2 Secs', '3 Secs', '4 Secs', '5 Secs');


  ILyricsFont:    array[0..2] of string = ('Plain', 'OLine1', 'OLine2');
  ILyricsEffect:  array[0..4] of string = ('Simple', 'Zoom', 'Slide', 'Ball', 'Shift');
  ISolmization:   array[0..3] of string = ('Off', 'Euro', 'Jap', 'American');

  IColor:         array[0..8] of string = ('Blue', 'Green', 'Pink', 'Red', 'Violet', 'Orange', 'Yellow', 'Brown', 'Black');

  // Advanced
  ILoadAnimation: array[0..1] of string = ('Off', 'On');
  IEffectSing:    array[0..1] of string = ('Off', 'On');
  IScreenFade: array [0..1] of String =('Off', 'On');
  IAskbeforeDel:  array[0..1] of string = ('Off', 'On');
  IOnSongClick:   array[0..2] of string = ('Sing', 'Select Players', 'Open Menu');
  ILineBonus:  array[0..2] of string = ('Off', 'At Score', 'At Notes');
  IPartyPopup: array[0..1] of string = ('Off', 'On');

  IJoypad:        array[0..1] of string = ('Off', 'On');
  ILPT:           array[0..2] of string = ('Off', 'LCD', 'Lights');

  IChannel:       array[0..6] of string = ('Off', '1', '2', '3', '4', '5', '6');

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

function TIni.ExtractKeyIndex(const key, prefix, suffix: String): Integer;
var
  value: String;
  start: Integer;
begin
  Result := -1;
  
  if Pos(prefix, key) > -1 then
  begin
    start := Pos(prefix, key) + Length(prefix);
    
    // copy all between prefix and suffix
    value := copy (key, start, Pos(suffix, key) - 1 - start);
    Result := StrToIntDef(value, -1);
  end;
end;

function TIni.GetMaxKeyIndex(keys: TStringList; const prefix, suffix: string): integer;
var
  i: integer;
  keyIndex: integer;
begin
  Result := -1;
  
  for i := 0 to keys.Count-1 do
  begin
    keyIndex := ExtractKeyIndex(keys[i], prefix, suffix);
    if (keyIndex > Result) then
      Result := keyIndex;
  end;
end;

procedure TIni.LoadInputDeviceCfg(IniFile: TMemIniFile);
var
  deviceIndex: integer;
  deviceCfg: PInputDeviceConfig;
  device: TAudioInputDevice;
  deviceIniIndex: integer;
  deviceIniStr: string;
  channelCount: integer;
  channelIndex: integer;
  newDevice: boolean;
  recordKeys: TStringList;
  i: integer;
begin
  recordKeys := TStringList.Create();

  // read all record-keys for filtering
  IniFile.ReadSection('Record', recordKeys);

  SetLength(InputDeviceConfig, 0);

  for i := 0 to recordKeys.Count-1 do
  begin
    // find next device-name
    deviceIniIndex := ExtractKeyIndex(recordKeys[i], 'DeviceName[', ']');
    if (deviceIniIndex >= 0) then
    begin
      deviceIniStr := IntToStr(deviceIniIndex);

      if not IniFile.ValueExists('Record', 'DeviceName['+deviceIniStr+']') then
        break;

      // resize list
      SetLength(InputDeviceConfig, Length(InputDeviceConfig)+1);

      // read an input device's config.
      // Note: All devices are appended to the list whether they exist or not.
      //   Otherwise an external device's config will be lost if it is not
      //   connected (e.g. singstar mics or USB-Audio devices).
      deviceCfg := @InputDeviceConfig[High(InputDeviceConfig)];
      deviceCfg.Name := IniFile.ReadString('Record', 'DeviceName['+deviceIniStr+']', '');
      deviceCfg.Input := IniFile.ReadInteger('Record', 'Input['+deviceIniStr+']', 0);

      // find the largest channel-number of the current device in the ini-file
      channelCount := GetMaxKeyIndex(recordKeys, 'Channel(', ')['+deviceIniStr+']');
      if (channelCount < 0) then
        channelCount := 0;

      SetLength(deviceCfg.ChannelToPlayerMap, channelCount);

      // read channel-to-player mapping for every channel of the current device
      // or set non-configured channels to no player (=0).
      for channelIndex := 0 to High(deviceCfg.ChannelToPlayerMap) do
      begin
        deviceCfg.ChannelToPlayerMap[channelIndex] :=
          IniFile.ReadInteger('Record', 'Channel('+IntToStr(channelIndex+1)+')['+deviceIniStr+']', 0);
      end;
    end;
  end;

  recordKeys.Free();

  // Input devices - append detected soundcards
  for deviceIndex := 0 to High(AudioInputProcessor.Device) do
  begin
    newDevice := true;
    for deviceIniIndex := 0 to High(InputDeviceConfig) do
    begin //Search for Card in List
      deviceCfg := @InputDeviceConfig[deviceIniIndex];
      device := AudioInputProcessor.Device[deviceIndex];

      if (deviceCfg.Name = Trim(device.Description)) then
      begin
        newDevice := false;

        // store highest channel index as an offset for the new channels
        channelIndex := High(deviceCfg.ChannelToPlayerMap);
        // add missing channels or remove non-existing ones
        SetLength(deviceCfg.ChannelToPlayerMap, device.AudioFormat.Channels);
        // initialize added channels to 0
        for i := channelIndex+1 to High(deviceCfg.ChannelToPlayerMap) do
        begin
          deviceCfg.ChannelToPlayerMap[i] := 0;
        end;

        // associate ini-index with device
        device.CfgIndex := deviceIniIndex;
        break;
      end;
    end;

    //If not in List -> Add
    if newDevice then
    begin
      // resize list
      SetLength(InputDeviceConfig, Length(InputDeviceConfig)+1);
      deviceCfg := @InputDeviceConfig[High(InputDeviceConfig)];
      device := AudioInputProcessor.Device[deviceIndex];
      
      // associate ini-index with device
      device.CfgIndex := High(InputDeviceConfig);

      deviceCfg.Name := Trim(device.Description);
      deviceCfg.Input := 0;
        
      channelCount := device.AudioFormat.Channels;
      SetLength(deviceCfg.ChannelToPlayerMap, channelCount);

      for channelIndex := 0 to channelCount-1 do
      begin
        // set default at first start of USDX (1st device, 1st channel -> player1)
        if ((channelIndex = 0) and (device.CfgIndex = 0)) then
          deviceCfg.ChannelToPlayerMap[0] := 1
        else
          deviceCfg.ChannelToPlayerMap[channelIndex] := 0;
      end;
    end;
  end;

end;

procedure TIni.SaveInputDeviceCfg(IniFile: TIniFile);
var
  deviceIndex: 		Integer;
  deviceIndexStr: 	String;
  channelIndex: 	Integer;
begin
  for deviceIndex := 0 to High(InputDeviceConfig) do
  begin
    deviceIndexStr := IntToStr(deviceIndex+1);
    
    // DeviceName and DeviceInput
    IniFile.WriteString('Record', 'DeviceName['+deviceIndexStr+']', InputDeviceConfig[deviceIndex].Name);
    IniFile.WriteString('Record', 'Input['+deviceIndexStr+']', IntToStr(InputDeviceConfig[deviceIndex].Input));
                        
    // Channel-to-Player Mapping
    for channelIndex := 0 to High(InputDeviceConfig[deviceIndex].ChannelToPlayerMap) do
    begin
      IniFile.WriteString('Record', 'Channel('+IntToStr(channelIndex+1)+')['+deviceIndexStr+']',
                          IntToStr(InputDeviceConfig[deviceIndex].ChannelToPlayerMap[channelIndex]));
    end;
  end;
end;

procedure TIni.Load();
var
  IniFile:    	TMemIniFile;
  ThemeIni:   	TMemIniFile;
  ThemeName:  	String;
  I:          	Integer;
  Modes:      	PPSDL_Rect;
  searchResult: TSearchRec;
    // returns name without fileextension
  function GetFileName (S: String):String;
  begin
  //Result := copy (S,0,StrRScan (PChar(S),char('.'))+1);
    Result := copy (S,0,Pos ('.ini',S)-1);
  end;
  
  // get index of value V in array a, returns -1 if value is not in array
  function GetArrayIndex(const A: array of String; V: String; caseInsensitiv: Boolean = False): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    
    for i := 0 To High(A) do
      if (A[i] = V) or (caseInsensitiv and (UpperCase(A[i]) = UpperCase(V))) then
      begin
        Result := i;
        break;
      end;
  end;
  
  // swap two strings
  procedure swap(var s1, s2: String);
  var
    s3: String;
  begin
    s3 := s1;
    s1 := s2;
    s2 := s3;
  end;
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

  // Screens
  Screens := GetArrayIndex(IScreens, IniFile.ReadString('Graphics', 'Screens', IScreens[0]));
  
  // FullScreen
  FullScreen := GetArrayIndex(IFullScreen, IniFile.ReadString('Graphics', 'FullScreen', 'On'));

  // Resolution
  SetLength(IResolution, 0);
  
  // Check if there are any modes available
  if IFullScreen[FullScreen] = 'On' then
    Modes  := SDL_ListModes(nil, SDL_OPENGL or SDL_FULLSCREEN or SDL_RESIZABLE)
  else 
    Modes  := SDL_ListModes(nil, SDL_OPENGL or SDL_RESIZABLE ) ;  
  
  if integer( Modes ) = 0 then
  begin
    Log.LogStatus( 'No resolutions Found' , 'Video');
  end
  else
  begin
    if integer( Modes ) = -1 then
    begin
      Log.LogStatus( 'ANY resolutions can be used - Fallback to some standard resolutions' , 'Video');
    
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
      for I := 0 to (Length(IResolution) div 2) - 1 do begin
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

  // MicBoost
  MicBoost := GetArrayIndex(IMicBoost, IniFile.ReadString('Sound', 'MicBoost', 'Off'));

  // ClickAssist
  ClickAssist := GetArrayIndex(IClickAssist, IniFile.ReadString('Sound', 'ClickAssist', 'Off'));

  // BeatClick
  BeatClick := GetArrayIndex(IBeatClick, IniFile.ReadString('Sound', 'BeatClick', IBeatClick[0]));

  // SavePlayback
  SavePlayback := GetArrayIndex(ISavePlayback, IniFile.ReadString('Sound', 'SavePlayback', ISavePlayback[0]));
  
  // Threshold
  Threshold := GetArrayIndex(IThreshold, IniFile.ReadString('Sound', 'Threshold', IThreshold[1]));

  // SDLBufferSize
  SDLBufferSize := GetArrayIndex(ISDLBufferSize, IniFile.ReadString('Sound', 'SDLBufferSize', '1024'));
  if (SDLBufferSize = -1) then
    SDLBufferSize := 1024
  else
    SDLBufferSize := StrToInt(ISDLBufferSize[SDLBufferSize]);
       
  //Preview Volume
  PreviewVolume := GetArrayIndex(IPreviewVolume, IniFile.ReadString('Sound', 'PreviewVolume', IPreviewVolume[7]));
  
  //Preview Fading
  PreviewFading := GetArrayIndex(IPreviewFading, IniFile.ReadString('Sound', 'PreviewFading', IPreviewFading[1]));
  
  // Lyrics Font
  LyricsFont := GetArrayIndex(ILyricsFont, IniFile.ReadString('Lyrics', 'LyricsFont', ILyricsFont[1]));

  // Lyrics Effect
  LyricsEffect := GetArrayIndex(ILyricsEffect, IniFile.ReadString('Lyrics', 'LyricsEffect', ILyricsEffect[1]));

  // Solmization
  Solmization := GetArrayIndex(ISolmization, IniFile.ReadString('Lyrics', 'Solmization', ISolmization[0]));
  
  // Theme
  SetLength(ITheme, 0);
  Log.LogStatus('Searching for Theme : ' + ThemePath + '*.ini', 'Theme');
  FindFirst(ThemePath + '*.ini',faAnyFile, searchResult);
  Repeat
    Log.LogStatus('Found Theme: ' + searchResult.Name, 'Theme');

    //Read Themename from Theme
    ThemeIni := TMemIniFile.Create(searchResult.Name);
    ThemeName := UpperCase(ThemeIni.ReadString('Theme','Name', GetFileName(searchResult.Name)));
    ThemeIni.Free;

    //Search for Skins for this Theme
    for I := Low(Skin.Skin) to High(Skin.Skin) do
    begin
      if UpperCase(Skin.Skin[I].Theme) = ThemeName then
      begin
        SetLength(ITheme, Length(ITheme)+1);
        ITheme[High(ITheme)] := GetFileName(searchResult.Name);
        break;
      end;
    end;
  Until FindNext(searchResult) <> 0;
  FindClose(searchResult);

  // No Theme Found
  if (Length(ITheme) = 0) then
  begin
    Log.CriticalError('Could not find any valid Themes.');
  end;

  Theme := GetArrayIndex(ITheme, IniFile.ReadString('Themes', 'Theme', 'DELUXE'), True);
  if (Theme = -1) then
       Theme := 0;
 
  // Skin
  Skin.onThemeChange;
  
  SkinNo := GetArrayIndex(ISkin, IniFile.ReadString('Themes',    'Skin',   ISkin[0]));

  // Color
  Color := GetArrayIndex(IColor, IniFile.ReadString('Themes',    'Color',   IColor[0]));
  
  // LoadAnimation
  LoadAnimation := GetArrayIndex(ILoadAnimation, IniFile.ReadString('Advanced', 'LoadAnimation', 'On'));

  // ScreenFade
  ScreenFade := GetArrayIndex(IScreenFade, IniFile.ReadString('Advanced', 'ScreenFade', 'On'));

  // EffectSing
  EffectSing := GetArrayIndex(IEffectSing, IniFile.ReadString('Advanced', 'EffectSing', 'On'));

  // AskbeforeDel
  AskbeforeDel := GetArrayIndex(IAskbeforeDel, IniFile.ReadString('Advanced', 'AskbeforeDel', 'On'));

  // OnSongClick
  OnSongClick := GetArrayIndex(IOnSongClick, IniFile.ReadString('Advanced', 'OnSongClick', 'Sing'));

  // Linebonus
  LineBonus := GetArrayIndex(ILineBonus, IniFile.ReadString('Advanced', 'LineBonus', 'At Score'));

  // PartyPopup
  PartyPopup := GetArrayIndex(IPartyPopup, IniFile.ReadString('Advanced', 'PartyPopup', 'On'));

  // Joypad
  Joypad := GetArrayIndex(IJoypad, IniFile.ReadString('Controller',    'Joypad',   IJoypad[0]));

  // LCD
  LPT := GetArrayIndex(ILPT, IniFile.ReadString('Devices',    'LPT',   ILPT[0]));

  // SongPath
  if (Params.SongPath <> '') then
    SongPath := IncludeTrailingPathDelimiter(Params.SongPath)
  else
    SongPath := IncludeTrailingPathDelimiter(IniFile.ReadString('Path', 'Songs', SongPath));
  
  IniFile.Free;
end;

procedure TIni.Save;
var
  IniFile:    TIniFile;
begin
  if not (FileExists(Filename) and FileIsReadOnly(Filename)) then
  begin

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

    // MicBoost
    IniFile.WriteString('Sound', 'MicBoost', IMicBoost[MicBoost]);

    // ClickAssist
    IniFile.WriteString('Sound', 'ClickAssist', IClickAssist[ClickAssist]);

    // BeatClick
    IniFile.WriteString('Sound', 'BeatClick', IBeatClick[BeatClick]);

    // Threshold
    IniFile.WriteString('Sound', 'Threshold', IThreshold[Threshold]);
    
    // SDLBufferSize
    IniFile.WriteString('Sound', 'SDLBufferSize', IntToStr(SDLBufferSize));

    // Song Preview
    IniFile.WriteString('Sound', 'PreviewVolume', IPreviewVolume[PreviewVolume]);
    
    // PreviewFading
    IniFile.WriteString('Sound', 'PreviewFading', IPreviewFading[PreviewFading]);

    // SavePlayback
    IniFile.WriteString('Sound', 'SavePlayback', ISavePlayback[SavePlayback]);

    // Lyrics Font
    IniFile.WriteString('Lyrics', 'LyricsFont', ILyricsFont[LyricsFont]);

    // Lyrics Effect
    IniFile.WriteString('Lyrics', 'LyricsEffect', ILyricsEffect[LyricsEffect]);

    // Solmization
    IniFile.WriteString('Lyrics', 'Solmization', ISolmization[Solmization]);

    // Theme
    IniFile.WriteString('Themes', 'Theme', ITheme[Theme]);

    // Skin
    IniFile.WriteString('Themes', 'Skin', ISkin[SkinNo]);

    // Color
    IniFile.WriteString('Themes', 'Color', IColor[Color]);

    SaveInputDeviceCfg(IniFile);
    //Log.LogError(InttoStr(Length(CardList)) + ' Cards Saved');

    //LoadAnimation
    IniFile.WriteString('Advanced',	'LoadAnimation', ILoadAnimation[LoadAnimation]);

    //EffectSing
    IniFile.WriteString('Advanced', 'EffectSing', IEffectSing[EffectSing]);

    //ScreenFade
    IniFile.WriteString('Advanced', 'ScreenFade', IScreenFade[ScreenFade]);

    //AskbeforeDel
    IniFile.WriteString('Advanced', 'AskbeforeDel', IAskbeforeDel[AskbeforeDel]);

    //OnSongClick
    IniFile.WriteString('Advanced', 'OnSongClick', IOnSongClick[OnSongClick]);

    //Line Bonus
    IniFile.WriteString('Advanced', 'LineBonus', ILineBonus[LineBonus]);

    //Party Popup
    IniFile.WriteString('Advanced', 'PartyPopup', IPartyPopup[PartyPopup]);

    // Joypad
    IniFile.WriteString('Controller', 'Joypad', IJoypad[Joypad]);

    IniFile.Free;
    
  end;
end;

procedure TIni.LoadSoundSettings;
var
  IniFile:	  TMemIniFile;
begin
  IniFile := TMemIniFile.Create(Filename);
  LoadInputDeviceCfg(IniFile);
  IniFile.Free;
end;

procedure TIni.SaveNames;
var
  IniFile:    TIniFile;
  I:          integer;
begin
  if not FileIsReadOnly(Filename) then begin
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
  IniFile:    TIniFile;
begin
  if not FileIsReadOnly(Filename) then begin
    IniFile := TIniFile.Create(Filename);

    // Difficulty
    IniFile.WriteString('Game', 'Difficulty', IDifficulty[Difficulty]);

    IniFile.Free;
  end;
end;

end.
