unit UMusic;

interface

uses Classes, MPlayer, Windows, Messages, SysUtils, Forms, ULog, USongs, Bass;//, DXSounds;

procedure InitializeSound;

type
  TSoundCard = record
    Name:     string;
    Source:   array of string;
  end;

  TFFTData  = array [0..256] of Single;
  
  TCustomSoundEntry = record
    Filename: String;
    Handle: hStream;
  end;


  TMusic = class
    private
//      MediaPlayer:        TMediaPlayer;       // It will be replaced by another component;
{      MediaPlayerStart:   TMediaPlayer;       // or maybe not if done this way ;)
      MediaPlayerBack:    TMediaPlayer;
      MediaPlayerSwoosh:  TMediaPlayer;
      MediaPlayerChange:  TMediaPlayer;
      MediaPlayerOption:  TMediaPlayer;
      MediaPlayerClick:   TMediaPlayer;
      MediaPlayerDrum:    TMediaPlayer;
      MediaPlayerHihat:   TMediaPlayer;
      MediaPlayerClap:    TMediaPlayer;
      MediaPlayerShuffle: TMediaPlayer;}
      BassStart:          hStream;            // Wait, I've replaced this with BASS
      BassBack:           hStream;            // It has almost all features we need
      BassSwoosh:         hStream;
      BassChange:         hStream;            // Almost? It aleady has them all :)
      BassOption:         hStream;
      BassClick:          hStream;
      BassDrum:           hStream;
      BassHihat:          hStream;
      BassClap:           hStream;
      BassShuffle:        hStream;

      //Custom Sounds
      CustomSounds: array of TCustomSoundEntry;


      Loaded:   boolean;
      Loop:     boolean;
//    DXSound:  TDXSound;
//    Player:   TcmxMp3;
    public
      Bass:               hStream;
      
//      SoundCard:          array of TSoundCard;
      procedure InitializePlayback;
      procedure InitializeRecord;
      procedure SetVolume(Volume: integer);
      procedure SetMusicVolume(Volume: integer);
      procedure SetLoop(Enabled: boolean);
      function Open(Name: string): boolean; // true if succeed
      procedure Rewind;
      procedure MoveTo(Time: real);
      procedure Play;
      procedure Pause; //Pause Mod
      procedure Stop;
      procedure Close;
      function Finished: boolean;
      function Length: real;
      function Position: real;
      procedure PlayStart;
      procedure PlayBack;
      procedure PlaySwoosh;
      procedure PlayChange;
      procedure PlayOption;
      procedure PlayClick;
      procedure PlayDrum;
      procedure PlayHihat;
      procedure PlayClap;
      procedure PlayShuffle;
      procedure StopShuffle;
      procedure CaptureStart;
      procedure CaptureStop;
      procedure CaptureCard(RecordI, PlayerLeft, PlayerRight: byte);
      procedure StopCard(Card: byte);
      function LoadPlayerFromFile(var MediaPlayer: TMediaPlayer; Name: string): boolean;
      function LoadSoundFromFile(var hStream: hStream; Name: string): boolean;

      //Equalizer
      function GetFFTData: TFFTData;

      //Custom Sounds
      function LoadCustomSound(const Filename: String): Cardinal;
      procedure PlayCustomSound(const Index: Cardinal);

end;

const
  RecordSystem = 1;

type
  TMuzyka = record
    Path:   string;
    Start:  integer;        // start of song in ms
//    BPM:    array of TBPM;
//    Gap:    real;
    IlNut:  integer;
    DlugoscNut:   integer;
//    WartoscNut:   integer;
  end;

  TCzesci = record
    Akt:      integer;      // aktualna czesc utworu do rysowania
    High:     integer;
    Ilosc:    integer;
    Resolution: integer;
    NotesGAP: integer;
    Wartosc:  integer;
    Czesc:    array of record
      Start:    integer;
      StartNote:  integer;
      Lyric:      string;
      LyricWidth: real;
      Koniec:   integer;
      BaseNote: integer;
      HighNut:  integer;
      IlNut:    integer;
      TotalNotes: integer;
      Nuta:     array of record
        Color:      integer;
        Start:      integer;
        Dlugosc:    integer;
        Ton:        integer;
        TonGamy:    integer;
        Tekst:      string;
        FreeStyle:  boolean;
        Wartosc:    integer;    // zwykla nuta x1, zlota nuta x2




      end;
    end;
  end;

  TCzas = record              // wszystko, co dotyczy aktualnej klatki
//    BajtowTotal:  integer;
//    BajtowTeraz:  integer;
//    BajtowNaSek:  integer;
    OldBeat:      integer;    // poprzednio wykryty beat w utworze
    AktBeat:      integer;    // aktualny beat w utworze
    MidBeat:      real;       // dokladny AktBeat

    // should not be used
//    OldHalf:      integer;    // poprzednio wykryta polowka
//    AktHalf:      integer;    // aktualna polowka w utworze
//    MidHalf:      real;       // dokladny AktHalf

    // now we use this for super synchronization!
    // only used when analyzing voice
    OldBeatD:     integer;    // poprzednio wykryty beat w utworze
    AktBeatD:     integer;    // aktualny beat w utworze
    MidBeatD:     real;       // dokladny AktBeatD
    FracBeatD:    real;       // fractional part of MidBeatD

    // we use this for audiable clicks
    OldBeatC:     integer;    // poprzednio wykryty beat w utworze
    AktBeatC:     integer;    // aktualny beat w utworze
    MidBeatC:     real;       // dokladny AktBeatC
    FracBeatC:    real;       // fractional part of MidBeatC


    OldCzesc:     integer;    // poprzednio wyswietlana czesc
                              // akt jest w czesci.akt

    Teraz:        real;       // aktualny czas w utworze
    Razem:        real;       // caly czas utworu
//    TerazSek:     integer;
  end;

var
  Form:     TForm;
  Music:    TMusic;

  // muzyka
  Muzyka:   TMuzyka;

  // czesci z nutami;
  Czesci:   array of TCzesci;

  // czas
  Czas:     TCzas;

  fHWND:        Thandle;

const
  ModeStr:  array[TMPModes] of string = ('Not ready', 'Stopped', 'Playing', 'Recording', 'Seeking', 'Paused', 'Open');

implementation
uses UGraphic, URecord, UFiles, UIni, UMain, UThemes;

procedure InitializeSound;
begin
  Log.LogStatus('Initializing Playback', 'InitializeSound');  Music.InitializePlayback;
  Log.LogStatus('Initializing Record', 'InitializeSound');    Music.InitializeRecord;
end;

procedure TMusic.InitializePlayback;
var
  Pet:  integer;
  S:    integer;
begin
  Log.BenchmarkStart(4);
  Log.LogStatus('Initializing Playback Subsystem', 'Music Initialize');
  Loaded := false;
  Loop := false;
  fHWND := AllocateHWND( nil);

  if not BASS_Init(1, 44100, 0, fHWND, nil) then begin
    Application.MessageBox ('Could not initialize BASS', 'Error');
    Exit;
  end;

  Log.BenchmarkEnd(4); Log.LogBenchmark('--> Bass Init', 4);

  // config playing buffer
//  BASS_SetConfig(BASS_CONFIG_UPDATEPERIOD, 10);
//  BASS_SetConfig(BASS_CONFIG_BUFFER, 100);

{  MediaPlayer := TMediaPlayer.Create( nil );
  MediaPlayer.ParentWindow := fHWND;
  MediaPlayer.Wait := true;}

  Log.LogStatus('Loading Sounds', 'Music Initialize');

{  LoadPlayerFromFile(MediaPlayerStart,  SoundPath + 'Common Start.mp3');
  LoadPlayerFromFile(MediaPlayerBack,   SoundPath + 'Common Back.mp3');
  LoadPlayerFromFile(MediaPlayerSwoosh, SoundPath + 'menu swoosh.mp3');
  LoadPlayerFromFile(MediaPlayerChange, SoundPath + 'select music change music.mp3');
  LoadPlayerFromFile(MediaPlayerOption, SoundPath + 'option change col.mp3');
  LoadPlayerFromFile(MediaPlayerClick,  SoundPath + 'rimshot022b.mp3');

  LoadPlayerFromFile(MediaPlayerDrum,   SoundPath + 'bassdrumhard076b.mp3');
  LoadPlayerFromFile(MediaPlayerHihat,  SoundPath + 'hihatclosed068b.mp3');
  LoadPlayerFromFile(MediaPlayerClap,   SoundPath + 'claps050b.mp3');

  LoadPlayerFromFile(MediaPlayerShuffle, SoundPath + 'Shuffle.mp3');}

  Log.BenchmarkStart(4);
  LoadSoundFromFile(BassStart,  SoundPath + 'Common Start.mp3');
  LoadSoundFromFile(BassBack,   SoundPath + 'Common Back.mp3');
  LoadSoundFromFile(BassSwoosh, SoundPath + 'menu swoosh.mp3');
  LoadSoundFromFile(BassChange, SoundPath + 'select music change music 50.mp3');
  LoadSoundFromFile(BassOption, SoundPath + 'option change col.mp3');
  LoadSoundFromFile(BassClick,  SoundPath + 'rimshot022b.mp3');

//  LoadSoundFromFile(BassDrum,   SoundPath + 'bassdrumhard076b.mp3');
//  LoadSoundFromFile(BassHihat,  SoundPath + 'hihatclosed068b.mp3');
//  LoadSoundFromFile(BassClap,   SoundPath + 'claps050b.mp3');

//  LoadSoundFromFile(BassShuffle, SoundPath + 'Shuffle.mp3');

  Log.BenchmarkEnd(4); Log.LogBenchmark('--> Loading Sounds', 4);
end;

procedure TMusic.InitializeRecord;
var
  S:        integer;
  device:   integer;
  descr:    string;
  input:    integer;
  input2:   integer;
  flags:    integer;
  mic:      array[0..15] of integer;
  SC:       integer; // soundcard
  SCI:      integer; // soundcard input
begin
  if RecordSystem = 1 then begin
    SetLength(Sound, 6 {max players});//Ini.Players+1);
    for S := 0 to High(Sound) do begin //Ini.Players do begin
      Sound[S] := TSound.Create;
      Sound[S].Num := S;
      Sound[S].BufferNew := TMemoryStream.Create;
      SetLength(Sound[S].BufferLong, 1);
      Sound[S].BufferLong[0] := TMemoryStream.Create;
      Sound[S].n := 4*1024;
    end;


    // check for recording devices;
    {device := 0;
    descr := BASS_RecordGetDeviceDescription(device);

    SetLength(SoundCard, 0);
    while (descr <> '') do begin
      SC := High(SoundCard) + 1;
      SetLength(SoundCard, SC+1);

      Log.LogAnalyze('Device #'+IntToStr(device)+': '+ descr);
      SoundCard[SC].Description := Descr;

      // check for recording inputs
      mic[device] := -1; // default to no change
      input := 0;
      BASS_RecordInit(device);
      Log.LogAnalyze('Input #' + IntToStr(Input) + ': ' + BASS_RecordGetInputName(input));
      flags := BASS_RecordGetInput(input);

      SetLength(SoundCard[SC].Input, 0);
      while (flags <> -1) do begin
        SCI := High(SoundCard[SC].Input) + 1;
        SetLength(SoundCard[SC].Input, SCI+1);

        Log.LogAnalyze('Input #' + IntToStr(Input) + ': ' + BASS_RecordGetInputName(input));
        SoundCard[SC].Input[SCI].Name := BASS_RecordGetInputName(Input);

        if (flags and BASS_INPUT_TYPE_MASK) = BASS_INPUT_TYPE_MIC then begin
          mic[device] := input; // auto set microphone
        end;
        Inc(Input);
        flags := BASS_RecordGetInput(input);
      end;

      if mic[device] <> -1 then begin
        Log.LogAnalyze('Found the mic at input ' + IntToStr(Mic[device]))
      end else begin
        Log.LogAnalyze('Mic not found');
        mic[device] := 0; // setting to the first one (for kxproject)
      end;
      SoundCard[SC].InputSeleceted := Mic[Device];


      BASS_RecordFree;

      inc(Device);
      descr := BASS_RecordGetDeviceDescription(Device);
    end; // while}
  end; // if
end;

procedure TMusic.SetVolume(Volume: integer);
begin
  //Old Sets Wave Volume
  //BASS_SetVolume(Volume);
  //New: Sets Volume only for this Application
  BASS_SetConfig(BASS_CONFIG_GVOL_SAMPLE, Volume);
  BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, Volume);
  BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, Volume);
end;

procedure TMusic.SetMusicVolume(Volume: Integer);
begin
  //Max Volume Prevention
  if Volume > 100 then
    Volume := 100;

  //Set Volume
  BASS_ChannelSetAttributes (Bass, -1, Volume, -101);
end;

procedure TMusic.SetLoop(Enabled: boolean);
begin
  Loop := Enabled;
end;

function TMusic.Open(Name: string): boolean;
begin
  Loaded := false;
  if FileExists(Name) then begin
{    MediaPlayer.FileName := Name;
    MediaPlayer.Open;}

    Bass := Bass_StreamCreateFile(false, pchar(Name), 0, 0, 0);
    Loaded := true;
    //Set Max Volume
    SetMusicVolume (100);
  end;

  Result := Loaded;

//  Player := TcmxMp3.Create(Name);
end;

procedure TMusic.Rewind;
begin
  if Loaded then begin
//    MediaPlayer.Position := 0;
    
  end;
end;

procedure TMusic.MoveTo(Time: real);
var
  bytes:    integer;
begin
//  if Loaded then begin
//    MediaPlayer.StartPos := Round(Time);
    bytes := BASS_ChannelSeconds2Bytes(Bass, Time);
    BASS_ChannelSetPosition(Bass, bytes);
//  end;
end;

procedure TMusic.Play;
begin
  if Loaded then begin
//    MediaPlayer.Play;
    if Loop then BASS_ChannelPlay(Bass, True); // start from beginning... actually bass itself does not loop, nor does this TMusic Class
    BASS_ChannelPlay(Bass, False); // for setting position before playing
  end;
end;

procedure TMusic.Pause; //Pause Mod
begin
  if Loaded then begin
    BASS_ChannelPause(Bass); // Pauses Song
  end;
end;

procedure TMusic.Stop;
begin
  Bass_ChannelStop(Bass);
//  Bass_StreamFree(Bass);
//  if ModeStr[MediaPlayer.Mode] = 'Playing' then begin
//    MediaPlayer.Stop;
//  end;
end;

procedure TMusic.Close;
begin
  Bass_StreamFree(Bass);
//  Player.Free;
//  MediaPlayer.Close;
end;

function TMusic.Length: real;
var
  bytes:    integer;
begin
  Result := 60;

  bytes := BASS_StreamGetLength(Bass);
  Result := BASS_ChannelBytes2Seconds(Bass, bytes);

{  if Assigned(MediaPlayer) then begin
    if Loaded then Result := MediaPlayer.Length / 1000;
  end;}
//  if Assigned(Player) then
//    Result := Player.LengthInSeconds;
end;

function TMusic.Position: real;
var
  bytes:    integer;
begin
  Result := 0;//MediaPlayer.Position / 1000;
  bytes := BASS_ChannelGetPosition(BASS);
  Result := BASS_ChannelBytes2Seconds(BASS, bytes);
end;

function TMusic.Finished: boolean;
begin
  Result := false;
//  if ModeStr[MediaPlayer.Mode] = 'Stopped' then Result := true;
  if BASS_ChannelIsActive(BASS) = BASS_ACTIVE_STOPPED then begin
//    beep;
    Result := true;
  end;
end;

{function myeffect( chan : integer; stream : Pointer; len : integer; udata : Pointer ): Pointer; cdecl;
var
  dane:   pwordarray;
  pet:    integer;
  Prev:     smallint;
  PrevNew:  smallint;
begin
  dane := stream;
  Prev := 0;
  for pet := 0 to len div 2 -1 do begin
    PrevNew := Dane[Pet];

//    Dane[pet] := Round(PrevNew*1/8 + Prev*7/8);

    Prev := Dane[Pet];
  end;
end;}

procedure TMusic.PlayStart;
{var
  Music:    PMix_Chunk;}
begin
{  Mix_OpenAudio(44100, 16, 1, 16*1024);
  Music := Mix_LoadWAV('D:\Rozne\UltraStar\Old\Boys - Hej Sokoly 30s.wav');
  Mix_RegisterEffect(0, myeffect, nil, 0);
  Mix_PlayChannel(0, Music, 0);}

//  MediaPlayerStart.Rewind;
//  MediaPlayerStart.Play;
  BASS_ChannelPlay(BassStart, True);
end;

procedure TMusic.PlayBack;
begin
//  MediaPlayerBack.Rewind;
//  MediaPlayerBack.Play;
//  if not
  BASS_ChannelPlay(BassBack, True);// then
//    Application.MessageBox ('Error playing stream!', 'Error');
end;

procedure TMusic.PlaySwoosh;
begin
//  MediaPlayerSwoosh.Rewind;
//  MediaPlayerSwoosh.Play;
  BASS_ChannelPlay(BassSwoosh, True);
end;

procedure TMusic.PlayChange;
begin
//  MediaPlayerChange.Rewind;
//  MediaPlayerChange.Play;
  BASS_ChannelPlay(BassChange, True);
end;

procedure TMusic.PlayOption;
begin
//  MediaPlayerOption.Rewind;
//  MediaPlayerOption.Play;
  BASS_ChannelPlay(BassOption, True);
end;

procedure TMusic.PlayClick;
begin
//  MediaPlayerClick.Rewind;
//  MediaPlayerClick.Play;
  BASS_ChannelPlay(BassClick, True);
end;

procedure TMusic.PlayDrum;
begin
//  MediaPlayerDrum.Rewind;
//  MediaPlayerDrum.Play;
  BASS_ChannelPlay(BassDrum, True);
end;

procedure TMusic.PlayHihat;
begin
//  MediaPlayerHihat.Rewind;
//  MediaPlayerHihat.Play;
  BASS_ChannelPlay(BassHihat, True);
end;

procedure TMusic.PlayClap;
begin
//  MediaPlayerClap.Rewind;
//  MediaPlayerClap.Play;
  BASS_ChannelPlay(BassClap, True);
end;

procedure TMusic.PlayShuffle;
begin
//  MediaPlayerShuffle.Rewind;
//  MediaPlayerShuffle.Play;
  BASS_ChannelPlay(BassShuffle, True);
end;

procedure TMusic.StopShuffle;
begin
  BASS_ChannelStop(BassShuffle);
end;

procedure TMusic.CaptureStart;
var
  S:        integer;
  SC:       integer;
  P1:       integer;
  P2:       integer;
begin
  for S := 0 to High(Sound) do
    Sound[S].BufferLong[0].Clear;

{    case PlayersPlay of
      1:  begin
            CaptureCard(0, 0, 1, 0);
          end;
      2:  begin
            if Ini.TwoPlayerMode = 0 then begin
              CaptureCard(0, 0, 1, 2);
            end else begin
              CaptureCard(0, 0, 1, 0);
              CaptureCard(1, 1, 2, 0);
            end;
          end;
      3:  begin
            CaptureCard(0, 0, 1, 2);
            CaptureCard(1, 1, 3, 0);
          end;
    end; // case}

//  CaptureCard(0, 0, 0, 0);
//  end;

  {for SC := 0 to High(SoundCard) do begin
    P1 := Ini.SoundCard[SC, 1];
    P2 := Ini.SoundCard[SC, 2];
    if P1 > PlayersPlay then P1 := 0;
    if P2 > PlayersPlay then P2 := 0;
    CaptureCard(SC, P1, P2);
  end;       }
  // 0.5.2: new
  for SC := 0 to High(Ini.CardList) do begin
    P1 := Ini.CardList[SC].ChannelL;
    P2 := Ini.CardList[SC].ChannelR;
    if P1 > PlayersPlay then P1 := 0;
    if P2 > PlayersPlay then P2 := 0;
    if (P1 > 0) or (P2 > 0) then
      CaptureCard(SC, P1, P2);
  end;
end;

procedure TMusic.CaptureStop;
var
  SC:   integer;
  P1:       integer;
  P2:       integer;
begin
{  if RecordSystem = 1 then begin
    case PlayersPlay of
      1:  begin
            StopCard(0);
          end;
      2:  begin
            if Ini.TwoPlayerMode = 0 then begin
              StopCard(0);
            end else begin
              StopCard(0);
              StopCard(1);
            end;
          end;
      3:  begin
            StopCard(0);
            StopCard(1);
          end;
    end;
  end;}

  {for SC := 0 to High(SoundCard) do begin
    StopCard(SC);
  end; }

  // 0.5.2
  for SC := 0 to High(Ini.CardList) do begin
    P1 := Ini.CardList[SC].ChannelL;
    P2 := Ini.CardList[SC].ChannelR;
    if P1 > PlayersPlay then P1 := 0;
    if P2 > PlayersPlay then P2 := 0;
    if (P1 > 0) or (P2 > 0) then StopCard(SC);
  end;

end;

//procedure TMusic.CaptureCard(RecordI, SoundNum, PlayerLeft, PlayerRight: byte);
procedure TMusic.CaptureCard(RecordI, PlayerLeft, PlayerRight: byte);
var
  Error:      integer;
  ErrorMsg:   string;
begin
  if not BASS_RecordInit(RecordI) then begin
    Error := BASS_ErrorGetCode;

    ErrorMsg := IntToStr(Error);
    if Error = BASS_ERROR_DX then ErrorMsg := 'No DX5';
    if Error = BASS_ERROR_ALREADY then ErrorMsg := 'The device has already been initialized';
    if Error = BASS_ERROR_DEVICE then ErrorMsg := 'The device number specified is invalid';
    if Error = BASS_ERROR_DRIVER then ErrorMsg := 'There is no available device driver';

    {Log.LogAnalyze('Error initializing record [' + IntToStr(RecordI) + ', '
      + IntToStr(PlayerLeft) + ', '+ IntToStr(PlayerRight) + ']: '
      + ErrorMsg);}
    Log.LogError('Error initializing record [' + IntToStr(RecordI) + ', '
      + IntToStr(PlayerLeft) + ', '+ IntToStr(PlayerRight) + ']: '
      + ErrorMsg);
    Log.LogError('Music -> CaptureCard: Error initializing record: ' + ErrorMsg);


  end else begin

  //SoundCard[RecordI].BassRecordStream := BASS_RecordStart(44100, 2, MakeLong(0, 20) , @GetMicrophone, PlayerLeft + PlayerRight*256);
  Recording.SoundCard[RecordI].BassRecordStream := BASS_RecordStart(44100, 2, MakeLong(0, 20) , @GetMicrophone, PlayerLeft + PlayerRight*256);

  {if SoundCard[RecordI].BassRecordStream = 0 then begin
    Error := BASS_ErrorGetCode;

    ErrorMsg := IntToStr(Error);
    if Error = BASS_ERROR_INIT then ErrorMsg := 'Not successfully called';
    if Error = BASS_ERROR_ALREADY then ErrorMsg := 'Recording is already in progress';
    if Error = BASS_ERROR_NOTAVAIL then ErrorMsg := 'The recording device is not available';
    if Error = BASS_ERROR_FORMAT then ErrorMsg := 'The specified format is not supported';
    if Error = BASS_ERROR_MEM then ErrorMsg := 'There is insufficent memory';
    if Error = BASS_ERROR_UNKNOWN then ErrorMsg := 'Unknown';

    Log.LogError('Error creating record stream [' + IntToStr(RecordI) + ', '
      + IntToStr(PlayerLeft) + ', '+ IntToStr(PlayerRight) + ']: '
      + ErrorMsg);
  end;         }
  end;
end;

procedure TMusic.StopCard(Card: byte);
begin
  BASS_RecordSetDevice(Card);
  BASS_RecordFree;
end;

function TMusic.LoadPlayerFromFile(var MediaPlayer: TMediaPlayer; Name: string): boolean;
begin
  Log.LogStatus('Loading Sound: "' + Name + '"', 'LoadPlayerFromFile');
  if FileExists(Name) then begin
    try
      MediaPlayer := TMediaPlayer.Create( nil );
    except
      Log.LogError('Failed to create MediaPlayer', 'LoadPlayerFromFile');
    end;
    try
      MediaPlayer.ParentWindow := fHWND;
      MediaPlayer.Wait := true;
      MediaPlayer.FileName := Name;
      MediaPlayer.DeviceType := dtAutoSelect;
      MediaPlayer.Display := nil;
    except
      Log.LogError('Failed setting MediaPlayer: ' + MediaPlayer.ErrorMessage, 'LoadPlayerFromFile');
    end;
    try
      MediaPlayer.Open;
    except
      Log.LogError('Failed to open using MediaPlayer', 'LoadPlayerFromFile');
    end;
  end else begin
    Log.LogError('Sound not found: "' + Name + '"', 'LoadPlayerFromFile');
    exit;
  end;
end;

function TMusic.LoadSoundFromFile(var hStream: hStream; Name: string): boolean;
var
  L: Integer;
begin
  if FileExists(Name) then begin
    Log.LogStatus('Loading Sound: "' + Name + '"', 'LoadPlayerFromFile');
    try
      hStream := BASS_StreamCreateFile(False, pchar(Name), 0, 0, 0);
      //Add CustomSound
      L := High(CustomSounds) + 1;
      SetLength (CustomSounds, L + 1);
      CustomSounds[L].Filename := Name;
      CustomSounds[L].Handle := hStream;
    except
      Log.LogError('Failed to open using BASS', 'LoadPlayerFromFile');
    end;
  end else begin
    Log.LogError('Sound not found: "' + Name + '"', 'LoadPlayerFromFile');
    exit;
  end;
end;

//Equalizer
function TMusic.GetFFTData: TFFTData;
var
Data: TFFTData;
begin
  //Get Channel Data Mono and 256 Values
  BASS_ChannelGetData(Bass, @Result, BASS_DATA_FFT512);
  //Result := Data;
end;

function TMusic.LoadCustomSound(const Filename: String): Cardinal;
var
  S: hStream;
  I: Integer;
  F: String;
begin
  //Search for Sound in already loaded Sounds
  F := UpperCase(SoundPath + FileName);
  For I := 0 to High(CustomSounds) do
  begin
    if (UpperCase(CustomSounds[I].Filename) = F) then
    begin
      Result := I;
      Exit;
    end;
  end;

  if LoadSoundFromFile(S, SoundPath + Filename) then
    Result := High(CustomSounds)
  else
    Result := 0;
end;

procedure TMusic.PlayCustomSound(const Index: Cardinal);
begin
if Index <= High(CustomSounds) then
  BASS_ChannelPlay(CustomSounds[Index].Handle, True);
end;


end.
