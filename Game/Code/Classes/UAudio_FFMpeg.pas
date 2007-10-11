unit UAudio_FFMpeg;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}


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
     {$IFDEF FPC}
     lclintf,
     {$ENDIF}
     URecord,
     UIni,
     UMain,
     UThemes;

const
  RecordSystem = 1;

type
  TMPModes = (mpNotReady, mpStopped, mpPlaying, mpRecording, mpSeeking,
    mpPaused, mpOpen);

const
  ModeStr:  array[TMPModes] of string = ('Not ready', 'Stopped', 'Playing', 'Recording', 'Seeking', 'Paused', 'Open');

type
    TAudio_ffMpeg = class( TInterfacedObject, IAudioPlayback )
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
      fHWND:    THandle;

    public
      Bass: hStream;
      function  GetName: String;
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
      function LoadSoundFromFile(var hStream: hStream; Name: string): boolean;

      //Equalizer
      function GetFFTData: TFFTData;

      //Custom Sounds
      function LoadCustomSound(const Filename: String): Cardinal;
      procedure PlayCustomSound(const Index: Cardinal );
end;

var
  singleton_MusicFFMpeg : IAudioPlayback;

function  TAudio_ffMpeg.GetName: String;
begin
  result := 'FFMpeg';
end;

procedure TAudio_ffMpeg.InitializePlayback;
var
  Pet:  integer;
  S:    integer;
begin
  Log.BenchmarkStart(4);
  Log.LogStatus('Initializing Playback Subsystem', 'Music Initialize');

  Loaded := false;
  Loop   := false;

  {$ifdef win32}
  // TODO : JB_Linux ... is this needed ? :)
  fHWND  := AllocateHWND( nil); // TODO : JB_lazarus - can we do something different here ?? lazarus didnt like this function
  {$ENDIF}

  // TODO : jb_linux replace with something other than bass
  if not BASS_Init(1, 44100, 0, fHWND, nil) then
  begin
    {$IFNDEF FPC}
    // TODO : JB_linux find a way to do this nice..
    Application.MessageBox ('Could not initialize BASS', 'Error');
    {$ENDIF}
    Exit;
  end;

  Log.BenchmarkEnd(4); Log.LogBenchmark('--> Bass Init', 4);

  // config playing buffer
//  BASS_SetConfig(BASS_CONFIG_UPDATEPERIOD, 10);
//  BASS_SetConfig(BASS_CONFIG_BUFFER, 100);

  Log.LogStatus('Loading Sounds', 'Music Initialize');

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

  Log.BenchmarkEnd(4);
  Log.LogBenchmark('--> Loading Sounds', 4);
end;

procedure TAudio_ffMpeg.InitializeRecord;
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

procedure TAudio_ffMpeg.SetVolume(Volume: integer);
begin
  //Old Sets Wave Volume
  //BASS_SetVolume(Volume);
  //New: Sets Volume only for this Application


  // TODO : jb_linux replace with something other than bass
  BASS_SetConfig(BASS_CONFIG_GVOL_SAMPLE, Volume);
  BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, Volume);
  BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, Volume);
end;

procedure TAudio_ffMpeg.SetMusicVolume(Volume: Integer);
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

procedure TAudio_ffMpeg.SetLoop(Enabled: boolean);
begin
  Loop := Enabled;
end;

function TAudio_ffMpeg.Open(Name: string): boolean;
begin
  Loaded := false;
  if FileExists(Name) then
  begin
    // TODO : jb_linux replace with something other than bass
    Bass := Bass_StreamCreateFile(false, pchar(Name), 0, 0, 0);
    
    Loaded := true;
    //Set Max Volume
    SetMusicVolume (100);
  end;

  Result := Loaded;
end;

procedure TAudio_ffMpeg.Rewind;
begin
  if Loaded then begin
  end;
end;

procedure TAudio_ffMpeg.MoveTo(Time: real);
var
  bytes:    integer;
begin
  // TODO : jb_linux replace with something other than bass
  bytes := BASS_ChannelSeconds2Bytes(Bass, Time);
  BASS_ChannelSetPosition(Bass, bytes);
end;

procedure TAudio_ffMpeg.Play;
begin
  // TODO : jb_linux replace with something other than bass
  if Loaded then
  begin
    if Loop then
      BASS_ChannelPlay(Bass, True); // start from beginning... actually bass itself does not loop, nor does this TAudio_ffMpeg Class

    BASS_ChannelPlay(Bass, False); // for setting position before playing
  end;
end;

procedure TAudio_ffMpeg.Pause; //Pause Mod
begin
  // TODO : jb_linux replace with something other than bass
  if Loaded then begin
    BASS_ChannelPause(Bass); // Pauses Song
  end;
end;

procedure TAudio_ffMpeg.Stop;
begin
  // TODO : jb_linux replace with something other than bass
  Bass_ChannelStop(Bass);
end;

procedure TAudio_ffMpeg.Close;
begin
  // TODO : jb_linux replace with something other than bass
  Bass_StreamFree(Bass);
end;

function TAudio_ffMpeg.Length: real;
var
  bytes:    integer;
begin
  Result := 60;

  // TODO : jb_linux replace with something other than bass
  bytes  := BASS_ChannelGetLength(Bass);
  Result := BASS_ChannelBytes2Seconds(Bass, bytes);
end;

function TAudio_ffMpeg.Position: real;
var
  bytes:    integer;
begin
  Result := 0;

  // TODO : jb_linux replace with something other than bass
  bytes  := BASS_ChannelGetPosition(BASS);
  Result := BASS_ChannelBytes2Seconds(BASS, bytes);
end;

function TAudio_ffMpeg.Finished: boolean;
begin
  Result := false;

  // TODO : jb_linux replace with something other than bass
  if BASS_ChannelIsActive(BASS) = BASS_ACTIVE_STOPPED then
  begin
    Result := true;
  end;
end;

procedure TAudio_ffMpeg.PlayStart;
begin
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelPlay(BassStart, True);
end;

procedure TAudio_ffMpeg.PlayBack;
begin
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelPlay(BassBack, True);// then
end;

procedure TAudio_ffMpeg.PlaySwoosh;
begin
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelPlay(BassSwoosh, True);
end;

procedure TAudio_ffMpeg.PlayChange;
begin
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelPlay(BassChange, True);
end;

procedure TAudio_ffMpeg.PlayOption;
begin
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelPlay(BassOption, True);
end;

procedure TAudio_ffMpeg.PlayClick;
begin
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelPlay(BassClick, True);
end;

procedure TAudio_ffMpeg.PlayDrum;
begin
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelPlay(BassDrum, True);
end;

procedure TAudio_ffMpeg.PlayHihat;
begin
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelPlay(BassHihat, True);
end;

procedure TAudio_ffMpeg.PlayClap;
begin
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelPlay(BassClap, True);
end;

procedure TAudio_ffMpeg.PlayShuffle;
begin
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelPlay(BassShuffle, True);
end;

procedure TAudio_ffMpeg.StopShuffle;
begin
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelStop(BassShuffle);
end;

procedure TAudio_ffMpeg.CaptureStart;
var
  S:        integer;
  SC:       integer;
  P1:       integer;
  P2:       integer;
begin
  for S := 0 to High(Sound) do
    Sound[S].BufferLong[0].Clear;

  for SC := 0 to High(Ini.CardList) do begin
    P1 := Ini.CardList[SC].ChannelL;
    P2 := Ini.CardList[SC].ChannelR;
    if P1 > PlayersPlay then P1 := 0;
    if P2 > PlayersPlay then P2 := 0;
    if (P1 > 0) or (P2 > 0) then
      CaptureCard(SC, P1, P2);
  end;
end;

procedure TAudio_ffMpeg.CaptureStop;
var
  SC:   integer;
  P1:       integer;
  P2:       integer;
begin

  for SC := 0 to High(Ini.CardList) do begin
    P1 := Ini.CardList[SC].ChannelL;
    P2 := Ini.CardList[SC].ChannelR;
    if P1 > PlayersPlay then P1 := 0;
    if P2 > PlayersPlay then P2 := 0;
    if (P1 > 0) or (P2 > 0) then StopCard(SC);
  end;

end;

//procedure TAudio_ffMpeg.CaptureCard(RecordI, SoundNum, PlayerLeft, PlayerRight: byte);
procedure TAudio_ffMpeg.CaptureCard(RecordI, PlayerLeft, PlayerRight: byte);
var
  Error:      integer;
  ErrorMsg:   string;
begin
  if not BASS_RecordInit(RecordI) then
  begin
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


  end
  else
  begin
    Recording.SoundCard[RecordI].BassRecordStream := BASS_RecordStart(44100, 2, MakeLong(0, 20) , @GetMicrophone, PlayerLeft + PlayerRight*256);
  end;
end;

procedure TAudio_ffMpeg.StopCard(Card: byte);
begin
  // TODO : jb_linux replace with something other than bass
  BASS_RecordSetDevice(Card);
  BASS_RecordFree;
end;

function TAudio_ffMpeg.LoadSoundFromFile(var hStream: hStream; Name: string): boolean;
var
  L: Integer;
begin
  if FileExists(Name) then
  begin
    Log.LogStatus('Loading Sound: "' + Name + '"', 'LoadSoundFromFile');
    try
      // TODO : jb_linux replace with something other than bass
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
function TAudio_ffMpeg.GetFFTData: TFFTData;
var
  Data: TFFTData;
begin
  //Get Channel Data Mono and 256 Values
  BASS_ChannelGetData(Bass, @Result, BASS_DATA_FFT512);
end;

function TAudio_ffMpeg.LoadCustomSound(const Filename: String): Cardinal;
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

procedure TAudio_ffMpeg.PlayCustomSound(const Index: Cardinal );
begin
  if Index <= High(CustomSounds) then
    BASS_ChannelPlay(CustomSounds[Index].Handle, True);
end;


{*

Sorry guys... this is my mess :(
Im going to try and get ffmpeg to handle audio playback ( at least for linux )
and Im going to implement it nicly along side BASS, in TAudio_ffMpeg ( where I can )

http://www.dranger.com/ffmpeg/ffmpeg.html
http://www.dranger.com/ffmpeg/ffmpegtutorial_all.html

http://www.inb.uni-luebeck.de/~boehme/using_libavcodec.html

*}
{*
function TAudio_ffMpeg.FFMPeg_StreamCreateFile(abool : boolean; aFileName : pchar ): THandle;
var
 lFormatCtx : PAVFormatContext;
begin

(*
  if(SDL_OpenAudio(&wanted_spec, &spec) < 0)
  begin
    fprintf(stderr, "SDL_OpenAudio: %s\n", SDL_GetError());
    writeln( 'SDL_OpenAudio' );
    exit;
  end;
*)

(*
  if ( av_open_input_file( lFormatCtx, aFileName, NULL, 0, NULL ) <> 0 )
  begin
    writeln( 'Unable to open file '+ aFileName );
    exit;
  end;

  // Retrieve stream information
  if ( av_find_stream_info(pFormatCtx) < 0 )
  begin
  	writeln( 'Unable to Retrieve stream information' );
    exit;
  end;
*)

end;  *}

initialization
  singleton_MusicFFMpeg := TAudio_ffMpeg.create();

  writeln( 'UAudio_Bass - Register Playback' );
  AudioManager.add( IAudioPlayback( singleton_MusicFFMpeg ) );

finalization
  AudioManager.Remove( IAudioPlayback( singleton_MusicFFMpeg ) );


end.
