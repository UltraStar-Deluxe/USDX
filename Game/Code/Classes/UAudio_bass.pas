unit UAudio_bass;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}


uses Classes,
     {$IFDEF win32}
     windows,
     {$ENDIF}
     Messages,
     SysUtils,
     {$IFNDEF FPC}
     Forms,
     {$ENDIF}
     bass,
     ULog,
     UMusic;

implementation

uses
     {$IFDEF LAZARUS}
     lclintf,
     {$ENDIF}
     URecord,
     UIni,
     UMain,
     UCommon,
     UThemes;

type
  TMPModes = (mpNotReady, mpStopped, mpPlaying, mpRecording, mpSeeking,
    mpPaused, mpOpen);

const
  ModeStr:  array[TMPModes] of string = ('Not ready', 'Stopped', 'Playing', 'Recording', 'Seeking', 'Paused', 'Open');

type
{$IFDEF UseBASSInput}
  TAudio_bass = class( TInterfacedObject, IAudioPlayback, IAudioInput)
{$ELSE}
  TAudio_bass = class( TInterfacedObject, IAudioPlayback)
{$ENDIF}
    private
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

    public
      Bass: hStream;
      function  GetName: String;

      {IAudioOutput interface}

      procedure InitializePlayback;
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
      function getPosition: real;
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
      function LoadSoundFromFile(var hStream: hStream; Name: string): boolean;

      //Equalizer
      function GetFFTData: TFFTData;

      // Interface for Visualizer
      function GetPCMData(var data: TPCMData): Cardinal;

      //Custom Sounds
      function LoadCustomSound(const Filename: String): Cardinal;
      procedure PlayCustomSound(const Index: Cardinal );

      {IAudioInput interface}
      {$IFDEF UseBASSInput}
      procedure InitializeRecord;

      procedure CaptureStart;
      procedure CaptureStop;
      procedure CaptureCard(Card: byte; CaptureSoundLeft, CaptureSoundRight: TSound);
      procedure StopCard(Card: byte);
      {$ENDIF}
  end;

{$IFDEF UseBASSInput}
  TBassSoundCard = class(TGenericSoundCard)
    RecordStream: HSTREAM;
  end;
{$ENDIF}


var
  singleton_MusicBass : IAudioPlayback;

function  TAudio_bass.GetName: String;
begin
  result := 'BASS';
end;

procedure TAudio_bass.InitializePlayback;
var
  Pet:  integer;
  S:    integer;
begin
//  Log.BenchmarkStart(4);
//  Log.LogStatus('Initializing Playback Subsystem', 'Music Initialize');

  Loaded := false;
  Loop   := false;

  if not BASS_Init(1, 44100, 0, 0, nil) then
  begin
    Log.LogError('Could not initialize BASS', 'Error');
    Exit;
  end;

//  Log.BenchmarkEnd(4); Log.LogBenchmark('--> Bass Init', 4);

  // config playing buffer
//  BASS_SetConfig(BASS_CONFIG_UPDATEPERIOD, 10);
//  BASS_SetConfig(BASS_CONFIG_BUFFER, 100);

//  Log.LogStatus('Loading Sounds', 'Music Initialize');

//  Log.BenchmarkStart(4);
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

//  Log.BenchmarkEnd(4);
//  Log.LogBenchmark('--> Loading Sounds', 4);
end;

procedure TAudio_bass.SetVolume(Volume: integer);
begin
  //Old Sets Wave Volume
  //BASS_SetVolume(Volume);
  //New: Sets Volume only for this Application

  BASS_SetConfig(BASS_CONFIG_GVOL_SAMPLE, Volume);
  BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, Volume);
  BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, Volume);
end;

procedure TAudio_bass.SetMusicVolume(Volume: Integer);
begin
  //Max Volume Prevention
  if Volume > 100 then
    Volume := 100;

  if Volume < 0 then
    Volume := 0;


  //Set Volume
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelSetAttributes (Bass, -1, Volume, -101);
end;

procedure TAudio_bass.SetLoop(Enabled: boolean);
begin
  Loop := Enabled;
end;

function TAudio_bass.Open(Name: string): boolean;
begin
  Loaded := false;
  if FileExists(Name) then
  begin
    Bass := Bass_StreamCreateFile(false, pchar(Name), 0, 0, 0);

    Loaded := true;
    //Set Max Volume
    SetMusicVolume (100);
  end;

  Result := Loaded;
end;

procedure TAudio_bass.Rewind;
begin
  if Loaded then begin
  end;
end;

procedure TAudio_bass.MoveTo(Time: real);
var
  bytes:    integer;
begin
  bytes := BASS_ChannelSeconds2Bytes(Bass, Time);
  BASS_ChannelSetPosition(Bass, bytes);
end;

procedure TAudio_bass.Play;
begin
  if Loaded then
  begin
    if Loop then
      BASS_ChannelPlay(Bass, True); // start from beginning... actually bass itself does not loop, nor does this TAudio_bass Class

    BASS_ChannelPlay(Bass, False); // for setting position before playing
  end;
end;

procedure TAudio_bass.Pause; //Pause Mod
begin
  if Loaded then begin
    BASS_ChannelPause(Bass); // Pauses Song
  end;
end;

procedure TAudio_bass.Stop;
begin
  Bass_ChannelStop(Bass);
end;

procedure TAudio_bass.Close;
begin
  Bass_StreamFree(Bass);
end;

function TAudio_bass.Length: real;
var
  bytes:    integer;
begin
  Result := 60;

  bytes  := BASS_ChannelGetLength(Bass);
  Result := BASS_ChannelBytes2Seconds(Bass, bytes);
end;

function TAudio_bass.getPosition: real;
var
  bytes:    integer;
begin
  Result := 0;

  bytes  := BASS_ChannelGetPosition(BASS);
  Result := BASS_ChannelBytes2Seconds(BASS, bytes);
end;

function TAudio_bass.Finished: boolean;
begin
  Result := false;

  if BASS_ChannelIsActive(BASS) = BASS_ACTIVE_STOPPED then
  begin
    Result := true;
  end;
end;

procedure TAudio_bass.PlayStart;
begin
  BASS_ChannelPlay(BassStart, True);
end;

procedure TAudio_bass.PlayBack;
begin
  BASS_ChannelPlay(BassBack, True);// then
end;

procedure TAudio_bass.PlaySwoosh;
begin
  BASS_ChannelPlay(BassSwoosh, True);
end;

procedure TAudio_bass.PlayChange;
begin
  BASS_ChannelPlay(BassChange, True);
end;

procedure TAudio_bass.PlayOption;
begin
  BASS_ChannelPlay(BassOption, True);
end;

procedure TAudio_bass.PlayClick;
begin
  BASS_ChannelPlay(BassClick, True);
end;

procedure TAudio_bass.PlayDrum;
begin
  BASS_ChannelPlay(BassDrum, True);
end;

procedure TAudio_bass.PlayHihat;
begin
  BASS_ChannelPlay(BassHihat, True);
end;

procedure TAudio_bass.PlayClap;
begin
  BASS_ChannelPlay(BassClap, True);
end;

procedure TAudio_bass.PlayShuffle;
begin
  BASS_ChannelPlay(BassShuffle, True);
end;

procedure TAudio_bass.StopShuffle;
begin
  BASS_ChannelStop(BassShuffle);
end;

function TAudio_bass.LoadSoundFromFile(var hStream: hStream; Name: string): boolean;
var
  L: Integer;
begin
  if FileExists(Name) then
  begin
    Log.LogStatus('Loading Sound: "' + Name + '"', 'LoadSoundFromFile');
    try
      hStream := BASS_StreamCreateFile(False, pchar(Name), 0, 0, 0);

      //Add CustomSound
      L := High(CustomSounds) + 1;
      SetLength (CustomSounds, L + 1);
      CustomSounds[L].Filename := Name;
      CustomSounds[L].Handle := hStream;
    except
      Log.LogError('Failed to open using BASS', 'LoadSoundFromFile');
    end;
  end
  else
  begin
    Log.LogError('Sound not found: "' + Name + '"', 'LoadSoundFromFile');
    exit;
  end;
end;

//Equalizer
function TAudio_bass.GetFFTData: TFFTData;
var
  Data: TFFTData;
begin
  //Get Channel Data Mono and 256 Values
  BASS_ChannelGetData(Bass, @Result, BASS_DATA_FFT512);
end;

{*
 * Copies interleaved PCM 16bit uint (maybe fake) stereo samples into data.
 * Returns the number of frames (= stereo/mono sample)
 *}
function TAudio_bass.GetPCMData(var data: TPCMData): Cardinal;
var
  info: BASS_CHANNELINFO;
  nBytes: DWORD;
begin
  //Get Channel Data Mono and 256 Values
  BASS_ChannelGetInfo(Bass, info);
  ZeroMemory(@data, sizeof(TPCMData));
  
  if (info.chans = 1) then
  begin
    // mono file -> add stereo channel
    {
    nBytes := BASS_ChannelGetData(Bass, @data[0], samples*sizeof(Smallint));
    // interleave data
    //CopyMemory(@data[1], @data[0], samples*sizeof(Smallint));
    }
    result := 0;
  end
  else
  begin
    // stereo file
    nBytes := BASS_ChannelGetData(Bass, @data, sizeof(TPCMData));
  end;
  if(nBytes <= 0) then
    result := 0
  else
    result := nBytes div sizeof(TPCMStereoSample);
end;

function TAudio_bass.LoadCustomSound(const Filename: String): Cardinal;
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

procedure TAudio_bass.PlayCustomSound(const Index: Cardinal );
begin
  if Index <= High(CustomSounds) then
    BASS_ChannelPlay(CustomSounds[Index].Handle, True);
end;

{$IFDEF UseBASSInput}

procedure TAudio_bass.InitializeRecord;
var
  device:     integer;
  Descr:      string;
  input:      integer;
  input2:     integer;
  InputName:  PChar;
  Flags:      integer;
  mic:        array[0..15] of integer;
  SC:         integer; // soundcard
  SCI:        integer; // soundcard input
  No:         integer;

function isDuplicate(Desc: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  //Check for Soundcard with same Description
  For I := 0 to SC-1 do
  begin
    if (Recording.SoundCard[I].Description = Desc) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

begin
  with Recording do
  begin
    // checks for recording devices and puts them into an array
    SetLength(SoundCard, 0);

    SC := 0;
    Descr := BASS_RecordGetDeviceDescription(SC);

    while (Descr <> '') do
    begin
      //If there is another SoundCard with the Same ID, Search an available Name
      if (IsDuplicate(Descr)) then
      begin
        No:= 1; //Count of SoundCards with  same Name
        Repeat
          Inc(No)
        Until not IsDuplicate(Descr + ' (' + InttoStr(No) + ')');

        //Set Description
        Descr := Descr + ' (' + InttoStr(No) + ')';
      end;

      SetLength(SoundCard, SC+1);

      // TODO: free object on termination
      SoundCard[SC] := TBassSoundCard.Create();
      SoundCard[SC].Description := Descr;

      //Get Recording Inputs
      SCI := 0;
      BASS_RecordInit(SC);

      InputName := BASS_RecordGetInputName(SCI);

      {$IFDEF DARWIN}
        // Under MacOSX the SingStar Mics have an empty
        // InputName. So, we have to add a hard coded
        // Workaround for this problem
        if (InputName = nil) and (Pos( 'USBMIC Serial#', Descr) > 0) then
        begin
          InputName := 'Microphone';
        end;
      {$ENDIF}

      SetLength(SoundCard[SC].Input, 1);
      SoundCard[SC].Input[SCI].Name := InputName;

      // process each input
      while (InputName <> nil) do
      begin
        Flags := BASS_RecordGetInput(SCI);
        if (SCI >= 1) {AND (Flags AND BASS_INPUT_OFF = 0)}  then
        begin
          SetLength(SoundCard[SC].Input, SCI+1);
          SoundCard[SC].Input[SCI].Name := InputName;
        end;

        //Set Mic Index
        if ((Flags and BASS_INPUT_TYPE_MIC) = 1) then
          SoundCard[SC].MicInput := SCI;

        Inc(SCI);
        InputName := BASS_RecordGetInputName(SCI);
      end;

      BASS_RecordFree;

      Inc(SC);
      Descr := BASS_RecordGetDeviceDescription(SC);
    end; // while
  end; // with Recording
end;

// TODO: code is used by all IAudioInput implementors
//   -> move to a common superclass (TAudioInput_Generic?)
procedure TAudio_bass.CaptureStart;
var
  S:  integer;
  SC: integer;
  PlayerLeft, PlayerRight: integer;
  CaptureSoundLeft, CaptureSoundRight: TSound;
begin
  for S := 0 to High(Recording.Sound) do
    Recording.Sound[S].BufferLong[0].Clear;

  for SC := 0 to High(Ini.CardList) do begin
    PlayerLeft  := Ini.CardList[SC].ChannelL-1;
    PlayerRight := Ini.CardList[SC].ChannelR-1;
    if PlayerLeft  >= PlayersPlay then PlayerLeft  := -1;
    if PlayerRight >= PlayersPlay then PlayerRight := -1;
    if (PlayerLeft > -1) or (PlayerRight > -1) then begin
      if (PlayerLeft > -1) then
        CaptureSoundLeft := Recording.Sound[PlayerLeft]
      else
        CaptureSoundLeft := nil;
      if (PlayerRight > -1) then
        CaptureSoundRight := Recording.Sound[PlayerRight]
      else
        CaptureSoundRight := nil;

      CaptureCard(SC, CaptureSoundLeft, CaptureSoundRight);
    end;
  end;
end;

// TODO: code is used by all IAudioInput implementors
//   -> move to a common superclass (TAudioInput_Generic?)
procedure TAudio_bass.CaptureStop;
var
  SC:   integer;
  PlayerLeft:  integer;
  PlayerRight: integer;
begin

  for SC := 0 to High(Ini.CardList) do begin
    PlayerLeft  := Ini.CardList[SC].ChannelL-1;
    PlayerRight := Ini.CardList[SC].ChannelR-1;
    if PlayerLeft  >= PlayersPlay then PlayerLeft  := -1;
    if PlayerRight >= PlayersPlay then PlayerRight := -1;
    if (PlayerLeft > -1) or (PlayerRight > -1) then
      StopCard(SC);
  end;

end;

{*
 * Bass input capture callback.
 * Params:
 *   stream - BASS input stream
 *   buffer - buffer of captured samples
 *   len - size of buffer in bytes
 *   user - players associated with left/right channels
 *}
function MicrophoneCallback(stream: HSTREAM; buffer: Pointer;
    len: Cardinal; Card: Cardinal): boolean; stdcall;
begin
  Recording.HandleMicrophoneData(buffer, len, Recording.SoundCard[Card]);
  Result := true;
end;

{*
 * Start input-capturing on Soundcard specified by Card.
 * Params:
 *   Card - soundcard index in Recording.SoundCard array
 *   CaptureSoundLeft  - sound(-buffer) used for left channel capture data
 *   CaptureSoundRight - sound(-buffer) used for right channel capture data
 *}
procedure TAudio_bass.CaptureCard(Card: byte; CaptureSoundLeft, CaptureSoundRight: TSound);
var
  Error:      integer;
  ErrorMsg:   string;
  bassSoundCard:  TBassSoundCard;
begin
  if not BASS_RecordInit(Card) then
  begin
    Error := BASS_ErrorGetCode;
    ErrorMsg := IntToStr(Error);
    if Error = BASS_ERROR_DX then ErrorMsg := 'No DX5';
    if Error = BASS_ERROR_ALREADY then ErrorMsg := 'The device has already been initialized';
    if Error = BASS_ERROR_DEVICE then ErrorMsg := 'The device number specified is invalid';
    if Error = BASS_ERROR_DRIVER then ErrorMsg := 'There is no available device driver';
    Log.LogError('Error initializing record [' + IntToStr(Card) + ']');
    Log.LogError('TAudio_bass.CaptureCard: Error initializing record: ' + ErrorMsg);
  end
  else
  begin
    bassSoundCard := TBassSoundCard(Recording.SoundCard[Card]);
    bassSoundCard.CaptureSoundLeft  := CaptureSoundLeft;
    bassSoundCard.CaptureSoundRight := CaptureSoundRight;

    // capture in 44.1kHz/stereo/16bit and a 20ms callback period
    bassSoundCard.RecordStream :=
      BASS_RecordStart(44100, 2, MakeLong(0, 20) , @MicrophoneCallback, Card);
  end;
end;

{*
 * Stop input-capturing on Soundcard specified by Card.
 * Params:
 *   Card - soundcard index in Recording.SoundCard array
 *}
procedure TAudio_bass.StopCard(Card: byte);
begin
  BASS_RecordSetDevice(Card);
  BASS_RecordFree;
end;

{$ENDIF}


initialization
  singleton_MusicBass := TAudio_bass.create();
  AudioManager.add( singleton_MusicBass );

finalization
  AudioManager.Remove( singleton_MusicBass );

end.
