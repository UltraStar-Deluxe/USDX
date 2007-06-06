unit UIni;

interface
uses IniFiles, ULog, SysUtils;

type
  TIni = class
    Name:           array[0..11] of string;

    // Templates for Names Mod
    NameTeam:       array[0..2] of string;
    NameTemplate:   array[0..11] of string;

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
    Card:           integer; // not saved in config.ini

    CardList:       array of record
      Name:           string;
      Input:          integer;
      ChannelL:       integer;
      ChannelR:       integer;
    end;

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

    procedure Load;
    procedure Save;
    procedure SaveNames;
    procedure SaveLevel;
  end;


var
  Ini:    TIni;
  IResolution:    array of string;
  ILanguage:      array of string;
  ITheme:         array of string;
  ISkin:          array of string;
  ICard:          array of string;
  IInput:         array of string;

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
  IMovieSize:     array[0..1] of string = ('Half', 'Full');

  IMicBoost:      array[0..3] of string = ('Off', '+6dB', '+12dB', '+18dB');
  IClickAssist:   array[0..1] of string = ('Off', 'On');
  IBeatClick:     array[0..1] of string = ('Off', 'On');
  ISavePlayback:  array[0..1] of string = ('Off', 'On');
  IThreshold:     array[0..3] of string = ('5%', '10%', '15%', '20%');
  //Song Preview
  IPreviewVolume: array[0..10] of string = ('Off', '10%', '20%', '30%', '40%', '50%', '60%', '70%', '80%', '90%', '100%');
  IPreviewFading: array[0..5] of string  = ('Off', '1 Sec', '2 Secs', '3 Secs', '4 Secs', '5 Secs');


  ILyricsFont:    array[0..2] of string = ('Plain', 'OLine1', 'OLine2');
  ILyricsEffect:  array[0..3] of string = ('Simple', 'Zoom', 'Slide', 'Ball');
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

  IChannel:       array[0..6] of string = ('0', '1', '2', '3', '4', '5', '6');

implementation
uses UFiles, SDL, ULanguage, USkins, URecord;

procedure TIni.Load;
var
  IniFile:    TMemIniFile;
  ThemeIni:   TMemIniFile;
  Tekst:      string;
  Pet:        integer;
  B: boolean;
  I, I2, I3:          integer;
  S:          string;
  Modes:      PPSDL_Rect;
  SR: TSearchRec; //Skin List Patch

function GetFileName (S: String):String;
  begin
  //Result := copy (S,0,StrRScan (PChar(S),char('.'))+1);
  Result := copy (S,0,Pos ('.ini',S)-1);
  end;

begin
  GamePath := ExtractFilePath(ParamStr(0));
  IniFile := TMemIniFile.Create(GamePath + 'config.ini');

  // Name
  for I := 0 to 11 do
    Ini.Name[I] := IniFile.ReadString('Name', 'P'+IntToStr(I+1), 'Player'+IntToStr(I+1));


  // Templates for Names Mod
  for I := 0 to 2 do
    Ini.NameTeam[I] := IniFile.ReadString('NameTeam', 'T'+IntToStr(I+1), 'Team'+IntToStr(I+1));
  for I := 0 to 11 do
    Ini.NameTemplate[I] := IniFile.ReadString('NameTemplate', 'Name'+IntToStr(I+1), 'Template'+IntToStr(I+1));

  // Players
  Tekst := IniFile.ReadString('Game', 'Players', IPlayers[0]);
  for Pet := 0 to High(IPlayers) do
    if Tekst = IPlayers[Pet] then Ini.Players := Pet;

  // Difficulty
  Tekst := IniFile.ReadString('Game', 'Difficulty', 'Easy');
  for Pet := 0 to High(IDifficulty) do
    if Tekst = IDifficulty[Pet] then Ini.Difficulty := Pet;

  // Language
  Tekst := IniFile.ReadString('Game', 'Language', 'English');
  for Pet := 0 to High(ILanguage) do
    if Tekst = ILanguage[Pet] then Ini.Language := Pet;

//  Language.ChangeLanguage(ILanguage[Ini.Language]);

  // Tabs
  Tekst := IniFile.ReadString('Game', 'Tabs', ITabs[0]);
  for Pet := 0 to High(ITabs) do
    if Tekst = ITabs[Pet] then Ini.Tabs := Pet;

  //Tabs at Startup fix
  Ini.Tabs_at_startup := Ini.Tabs;

  // Sorting
  Tekst := IniFile.ReadString('Game', 'Sorting', ISorting[0]);
  for Pet := 0 to High(ISorting) do
    if Tekst = ISorting[Pet] then Ini.Sorting := Pet;

  // Debug
  Tekst := IniFile.ReadString('Game', 'Debug', IDebug[0]);
  for Pet := 0 to High(IDebug) do
    if Tekst = IDebug[Pet] then Ini.Debug := Pet;

  //if Ini.Debug = 1 then SongPath := 'E:\UltraStar 03\Songs\';

  // Screens
  Tekst := IniFile.ReadString('Graphics', 'Screens', IScreens[0]);
  for Pet := 0 to High(IScreens) do
    if Tekst = IScreens[Pet] then Ini.Screens := Pet;

  // Resolution
  SetLength(IResolution, 0);
  Modes := SDL_ListModes(nil, SDL_OPENGL or SDL_FULLSCREEN); // Check if there are any modes available
  repeat
//    Log.LogError(Format( ' %d x %d', [ modes^.w, modes^.h ] ) );
    SetLength(IResolution, Length(IResolution) + 1);
    IResolution[High(IResolution)] := IntToStr(Modes^.w) + 'x' + IntToStr(Modes^.h);
    Inc(Modes);
  until Modes^ = nil;

  // reverse order
  for I := 0 to (Length(IResolution) div 2) - 1 do begin
    S := IResolution[I];
    IResolution[I] := IResolution[High(IResolution)-I];
    IResolution[High(IResolution)-I] := S;
  end;

  Tekst := IniFile.ReadString('Graphics', 'Resolution', '800x600');
  for Pet := 0 to High(IResolution) do
    if Tekst = IResolution[Pet] then Ini.Resolution := Pet;

  // FullScreen
  Tekst := IniFile.ReadString('Graphics', 'FullScreen', 'On');
  for Pet := 0 to High(IFullScreen) do
    if Tekst = IFullScreen[Pet] then Ini.FullScreen := Pet;

  // Resolution
  Tekst := IniFile.ReadString('Graphics', 'Depth', '32 bit');
  for Pet := 0 to High(IDepth) do
    if Tekst = IDepth[Pet] then Ini.Depth := Pet;

  // Texture Size
  Tekst := IniFile.ReadString('Graphics', 'TextureSize', ITextureSize[1]);
  for Pet := 0 to High(ITextureSize) do
    if Tekst = ITextureSize[Pet] then Ini.TextureSize := Pet;

  // SingWindow
  Tekst := IniFile.ReadString('Graphics', 'SingWindow', 'Big');
  for Pet := 0 to High(ISingWindow) do
    if Tekst = ISingWindow[Pet] then Ini.SingWindow := Pet;

  // Oscilloscope
  Tekst := IniFile.ReadString('Graphics', 'Oscilloscope', 'Bar');
  for Pet := 0 to High(IOscilloscope) do
    if Tekst = IOscilloscope[Pet] then Ini.Oscilloscope := Pet;

  // Spectrum
  Tekst := IniFile.ReadString('Graphics', 'Spectrum', 'Off');
  for Pet := 0 to High(ISpectrum) do
    if Tekst = ISpectrum[Pet] then Ini.Spectrum := Pet;

  // Spectrograph
  Tekst := IniFile.ReadString('Graphics', 'Spectrograph', 'Off');
  for Pet := 0 to High(ISpectrograph) do
    if Tekst = ISpectrograph[Pet] then Ini.Spectrograph := Pet;

  // MovieSize
  Tekst := IniFile.ReadString('Graphics', 'MovieSize', IMovieSize[0]);
  for Pet := 0 to High(IMovieSize) do
    if Tekst = IMovieSize[Pet] then Ini.MovieSize := Pet;

  // MicBoost
  Tekst := IniFile.ReadString('Sound',    'MicBoost',    'Off');
  for Pet := 0 to High(IMicBoost) do
    if Tekst = IMicBoost[Pet] then Ini.MicBoost := Pet;

  // ClickAssist
  Tekst := IniFile.ReadString('Sound',    'ClickAssist', 'Off');
  for Pet := 0 to High(IClickAssist) do
    if Tekst = IClickAssist[Pet] then Ini.ClickAssist := Pet;

  // BeatClick
  Tekst := IniFile.ReadString('Sound',    'BeatClick', IBeatClick[0]);
  for Pet := 0 to High(IBeatClick) do
    if Tekst = IBeatClick[Pet] then Ini.BeatClick := Pet;

  // SavePlayback
  Tekst := IniFile.ReadString('Sound',    'SavePlayback', ISavePlayback[0]);
  for Pet := 0 to High(ISavePlayback) do
    if Tekst = ISavePlayback[Pet] then Ini.SavePlayback := Pet;

  // Threshold
  Tekst := IniFile.ReadString('Sound', 'Threshold', IThreshold[2]);
  for Pet := 0 to High(IThreshold) do
    if Tekst = IThreshold[Pet] then Ini.Threshold := Pet;

  //Song Preview
  Tekst := IniFile.ReadString('Sound', 'PreviewVolume', IPreviewVolume[7]);
  for Pet := 0 to High(IPreviewVolume) do
    if Tekst = IPreviewVolume[Pet] then Ini.PreviewVolume := Pet;

  Tekst := IniFile.ReadString('Sound', 'PreviewFading', IPreviewFading[1]);
  for Pet := 0 to High(IPreviewFading) do
    if Tekst = IPreviewFading[Pet] then Ini.PreviewFading := Pet;

  // Lyrics Font
  Tekst := IniFile.ReadString('Lyrics',    'LyricsFont',   ILyricsFont[1]);
  for Pet := 0 to High(ILyricsFont) do
    if Tekst = ILyricsFont[Pet] then Ini.LyricsFont := Pet;

  // Lyrics Effect
  Tekst := IniFile.ReadString('Lyrics',    'LyricsEffect',   ILyricsEffect[1]);
  for Pet := 0 to High(ILyricsEffect) do
    if Tekst = ILyricsEffect[Pet] then Ini.LyricsEffect := Pet;

  // Solmization
  Tekst := IniFile.ReadString('Lyrics',    'Solmization',   ISolmization[0]);
  for Pet := 0 to High(ISolmization) do
    if Tekst = ISolmization[Pet] then Ini.Solmization := Pet;

  // Theme

  //Theme List Patch
    SetLength(ITheme, 0);
    FindFirst('Themes\*.ini',faAnyFile,SR);
    Repeat
      ThemeIni := TMemIniFile.Create(SR.Name);
      Tekst := UpperCase(ThemeIni.ReadString('Theme','Name',GetFileName(SR.Name)));
      ThemeIni.Free;
      for Pet := low(Skin.Skin) to high(Skin.Skin) do
      begin
        if UpperCase(Skin.Skin[Pet].Theme) = Tekst then
        begin
          SetLength(ITheme, Length(ITheme)+1);
          ITheme[High(ITheme)] := GetFileName(SR.Name);
          break;
        end;
      end;
    Until FindNext(SR) <> 0;
    FindClose(SR);
  //Theme List Patch End }

  //No Theme Found
  if (Length(ITheme)=0) then
  begin
    Log.CriticalError('Could not find any valid Themes.');
  end;


  Tekst := IniFile.ReadString('Themes',    'Theme',   ITheme[0]);
  Ini.Theme := 0;
  for Pet := 0 to High(ITheme) do
    if Uppercase(Tekst) = Uppercase(ITheme[Pet]) then Ini.Theme := Pet;

  // Skin
  Skin.onThemeChange;
  Ini.SkinNo := 0;

  Tekst := IniFile.ReadString('Themes',    'Skin',   ISkin[0]);
  for Pet := 0 to High(ISkin) do
    if Tekst = ISkin[Pet] then Ini.SkinNo := Pet;

  // Color
  Tekst := IniFile.ReadString('Themes',    'Color',   IColor[0]);
  for Pet := 0 to High(IColor) do
    if Tekst = IColor[Pet] then Ini.Color := Pet;

  // Record - load ini list
  SetLength(CardList, 0);
  I := 1;
  while (IniFile.ValueExists('Record', 'DeviceName' + IntToStr(I)) = true) do begin
    //Automatically Delete not Existing Sound Cards
    S := IniFile.ReadString('Record', 'DeviceName' + IntToStr(I), '');
    //{
    B := False;
    //Look for Soundcard
    for I2 := 0 to High(Recording.SoundCard) do
    begin
      if (S = Trim(Recording.SoundCard[I2].Description)) then
      begin
      B := True;
      Break;
      end;
    end;

    if B then
    begin //}
      I3 := Length(CardList);
      SetLength(CardList, I3+1);
      Ini.CardList[I3].Name := S;
      Ini.CardList[I3].Input := IniFile.ReadInteger('Record', 'Input' + IntToStr(I), 0);
      Ini.CardList[I3].ChannelL := IniFile.ReadInteger('Record', 'ChannelL' + IntToStr(I), 0);
      Ini.CardList[I3].ChannelR := IniFile.ReadInteger('Record', 'ChannelR' + IntToStr(I), 0);
    end;
    Inc(I);
  end;

  // Record - append detected soundcards
  for I := 0 to High(Recording.SoundCard) do
  begin
    B := False;
    For I2 := 0 to High(CardList) do
    begin //Search for Card in List
      if (CardList[I2].Name = Trim(Recording.SoundCard[I].Description)) then
      begin
        B := True;
        Break;
      end;
    end;

    //If not in List -> Add
    If not B then
    begin
      I3 := Length(CardList);
      SetLength(CardList, I3+1);
      CardList[I3].Name := Trim(Recording.SoundCard[I].Description);
      CardList[I3].Input := 0;
      CardList[I3].ChannelL := 0;
      CardList[I3].ChannelR := 0;
      // default for new users
      if (Length(CardList) = 1) then
        CardList[I].ChannelL := 1;
    end;
  end;

  //Advanced Settings

  // LoadAnimation
  Tekst := IniFile.ReadString('Advanced', 'LoadAnimation', 'On');
  for Pet := 0 to High(ILoadAnimation) do
    if Tekst = ILoadAnimation[Pet] then Ini.LoadAnimation := Pet;

  // ScreenFade
  Tekst := IniFile.ReadString('Advanced', 'ScreenFade', 'On');
  for Pet := 0 to High(IScreenFade) do
    if Tekst = IScreenFade[Pet] then Ini.ScreenFade := Pet;

  // EffectSing
  Tekst := IniFile.ReadString('Advanced', 'EffectSing', 'On');
  for Pet := 0 to High(IEffectSing) do
    if Tekst = IEffectSing[Pet] then Ini.EffectSing := Pet;

  // AskbeforeDel
  Tekst := IniFile.ReadString('Advanced', 'AskbeforeDel', 'On');
  for Pet := 0 to High(IAskbeforeDel) do
    if Tekst = IAskbeforeDel[Pet] then Ini.AskbeforeDel := Pet;

  // OnSongClick
  Tekst := IniFile.ReadString('Advanced', 'OnSongClick', 'Sing');
  for Pet := 0 to High(IOnSongClick) do
    if Tekst = IOnSongClick[Pet] then Ini.OnSongClick := Pet;

  // Linebonus
  Tekst := IniFile.ReadString('Advanced', 'LineBonus', 'At Score');
  for Pet := 0 to High(ILineBonus) do
    if Tekst = ILineBonus[Pet] then Ini.LineBonus := Pet;

  // PartyPopup
  Tekst := IniFile.ReadString('Advanced', 'PartyPopup', 'On');
  for Pet := 0 to High(IPartyPopup) do
    if Tekst = IPartyPopup[Pet] then Ini.PartyPopup := Pet;


  // Joypad
  Tekst := IniFile.ReadString('Controller',    'Joypad',   IJoypad[0]);
  for Pet := 0 to High(IJoypad) do
    if Tekst = IJoypad[Pet] then Ini.Joypad := Pet;

  // LCD
  Tekst := IniFile.ReadString('Devices',    'LPT',   ILPT[0]);
  for Pet := 0 to High(ILPT) do
    if Tekst = ILPT[Pet] then Ini.LPT := Pet;


  // SongPath
  SongPath := IncludeTrailingPathDelimiter(IniFile.ReadString('Path', 'Songs', SongPath));


  IniFile.Free;
end;

procedure TIni.Save;
var
  IniFile:    TIniFile;
  Tekst:      string;
  I: Integer;
  S: String;
begin
  if not (FileExists(GamePath + 'config.ini') and FileIsReadOnly(GamePath + 'config.ini')) then begin
    IniFile := TIniFile.Create(GamePath + 'config.ini');

  // Players
  Tekst := IPlayers[Ini.Players];
  IniFile.WriteString('Game',     'Players',   Tekst);

  // Difficulty
  Tekst := IDifficulty[Ini.Difficulty];
  IniFile.WriteString('Game',     'Difficulty',   Tekst);

  // Language
  Tekst := ILanguage[Ini.Language];
  IniFile.WriteString('Game',     'Language',   Tekst);

  // Tabs
  Tekst := ITabs[Ini.Tabs];
  IniFile.WriteString('Game',     'Tabs',   Tekst);

  // Sorting
  Tekst := ISorting[Ini.Sorting];
  IniFile.WriteString('Game',     'Sorting',   Tekst);

  // Debug
  Tekst := IDebug[Ini.Debug];
  IniFile.WriteString('Game',     'Debug',   Tekst);

  // Screens
  Tekst := IScreens[Ini.Screens];
  IniFile.WriteString('Graphics', 'Screens', Tekst);

  // FullScreen
  Tekst := IFullScreen[Ini.FullScreen];
  IniFile.WriteString('Graphics', 'FullScreen', Tekst);

  // Resolution
  Tekst := IResolution[Ini.Resolution];
  IniFile.WriteString('Graphics', 'Resolution', Tekst);

  // Depth
  Tekst := IDepth[Ini.Depth];
  IniFile.WriteString('Graphics', 'Depth', Tekst);

  // Resolution
  Tekst := ITextureSize[Ini.TextureSize];
  IniFile.WriteString('Graphics', 'TextureSize', Tekst);

  // Sing Window
  Tekst := ISingWindow[Ini.SingWindow];
  IniFile.WriteString('Graphics', 'SingWindow', Tekst);

  // Oscilloscope
  Tekst := IOscilloscope[Ini.Oscilloscope];
  IniFile.WriteString('Graphics', 'Oscilloscope', Tekst);

  // Spectrum
  Tekst := ISpectrum[Ini.Spectrum];
  IniFile.WriteString('Graphics', 'Spectrum', Tekst);

  // Spectrograph
  Tekst := ISpectrograph[Ini.Spectrograph];
  IniFile.WriteString('Graphics', 'Spectrograph', Tekst);

  // Movie Size
  Tekst := IMovieSize[Ini.MovieSize];
  IniFile.WriteString('Graphics', 'MovieSize', Tekst);

  // MicBoost
  Tekst := IMicBoost[Ini.MicBoost];
  IniFile.WriteString('Sound',    'MicBoost',    Tekst);

  // ClickAssist
  Tekst := IClickAssist[Ini.ClickAssist];
  IniFile.WriteString('Sound',    'ClickAssist',    Tekst);

  // BeatClick
  Tekst := IBeatClick[Ini.BeatClick];
  IniFile.WriteString('Sound',    'BeatClick',    Tekst);

  // Threshold
  Tekst := IThreshold[Ini.Threshold];
  IniFile.WriteString('Sound',    'Threshold',    Tekst);

  // Song Preview
  Tekst := IPreviewVolume[Ini.PreviewVolume];
  IniFile.WriteString('Sound',    'PreviewVolume',    Tekst);

  Tekst := IPreviewFading[Ini.PreviewFading];
  IniFile.WriteString('Sound',    'PreviewFading',    Tekst);

  // SavePlayback
  Tekst := ISavePlayback[Ini.SavePlayback];
  IniFile.WriteString('Sound',    'SavePlayback',    Tekst);

  // Lyrics Font
  Tekst := ILyricsFont[Ini.LyricsFont];
  IniFile.WriteString('Lyrics',    'LyricsFont',    Tekst);

  // Lyrics Effect
  Tekst := ILyricsEffect[Ini.LyricsEffect];
  IniFile.WriteString('Lyrics',    'LyricsEffect',    Tekst);

  // Solmization
  Tekst := ISolmization[Ini.Solmization];
  IniFile.WriteString('Lyrics',    'Solmization',    Tekst);

  // Theme
  Tekst := ITheme[Ini.Theme];
  IniFile.WriteString('Themes',    'Theme',    Tekst);

  // Skin
  Tekst := ISkin[Ini.SkinNo];
  IniFile.WriteString('Themes',    'Skin',    Tekst);

  // Color
  Tekst := IColor[Ini.Color];
  IniFile.WriteString('Themes',    'Color',    Tekst);

  // Record
    for I := 0 to High(CardList) do begin
      S := IntToStr(I+1);

      Tekst := CardList[I].Name;
      IniFile.WriteString('Record', 'DeviceName' + S, Tekst);

      Tekst := IntToStr(CardList[I].Input);
      IniFile.WriteString('Record', 'Input' + S, Tekst);

      Tekst := IntToStr(CardList[I].ChannelL);
      IniFile.WriteString('Record', 'ChannelL' + S, Tekst);

      Tekst := IntToStr(CardList[I].ChannelR);
      IniFile.WriteString('Record', 'ChannelR' + S, Tekst);
    end;

    //Log.LogError(InttoStr(Length(CardList)) + ' Cards Saved');

  //Advanced Settings

  //LoadAnimation
  Tekst := ILoadAnimation[Ini.LoadAnimation];
  IniFile.WriteString('Advanced', 'LoadAnimation', Tekst);

  //EffectSing
  Tekst := IEffectSing[Ini.EffectSing];
  IniFile.WriteString('Advanced', 'EffectSing', Tekst);

  //ScreenFade
  Tekst := IScreenFade[Ini.ScreenFade];
  IniFile.WriteString('Advanced', 'ScreenFade', Tekst);

  //AskbeforeDel
  Tekst := IAskbeforeDel[Ini.AskbeforeDel];
  IniFile.WriteString('Advanced', 'AskbeforeDel', Tekst);

  //OnSongClick
  Tekst := IOnSongClick[Ini.OnSongClick];
  IniFile.WriteString('Advanced', 'OnSongClick', Tekst);

  //Line Bonus
  Tekst := ILineBonus[Ini.LineBonus];
  IniFile.WriteString('Advanced', 'LineBonus', Tekst);

  //Party Popup
  Tekst := IPartyPopup[Ini.PartyPopup];
  IniFile.WriteString('Advanced', 'PartyPopup', Tekst);

  // Joypad
  Tekst := IJoypad[Ini.Joypad];
  IniFile.WriteString('Controller',    'Joypad',    Tekst);

    IniFile.Free;
  end;
end;

procedure TIni.SaveNames;
var
  IniFile:    TIniFile;
  I:          integer;
begin
  if not FileIsReadOnly(GamePath + 'config.ini') then begin
    IniFile := TIniFile.Create(GamePath + 'config.ini');

    //Name
      // Templates for Names Mod
    for I := 1 to 12 do
      IniFile.WriteString('Name', 'P' + IntToStr(I), Ini.Name[I-1]);
    for I := 1 to 3 do
      IniFile.WriteString('NameTeam', 'T' + IntToStr(I), Ini.NameTeam[I-1]);
    for I := 1 to 12 do
      IniFile.WriteString('NameTemplate', 'Name' + IntToStr(I), Ini.NameTemplate[I-1]);

    IniFile.Free;
  end;
end;

procedure TIni.SaveLevel;
var
  IniFile:    TIniFile;
  I:          integer;
begin
  if not FileIsReadOnly(GamePath + 'config.ini') then begin
    IniFile := TIniFile.Create(GamePath + 'config.ini');

    // Difficulty
    IniFile.WriteString('Game', 'Difficulty', IDifficulty[Ini.Difficulty]);

    IniFile.Free;
  end;
end;

end.