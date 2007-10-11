unit UMusic_BASS;

interface

{$I switches.inc}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}


uses Classes,

     {$IFDEF win32}
     windows,
     {$ENDIF}

//     UCommon,
     Messages,
     SysUtils,
     {$IFNDEF FPC}
     Forms,
     {$ENDIF}

     bass,
     ULog,
     UMusic;
//     USongs;
//     Classes;



type

  TMusic_bass = class( TInterfacedObject, IAudioPlayback, IAudioInput )
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

const
  RecordSystem = 1;

type
  TMPModes = (mpNotReady, mpStopped, mpPlaying, mpRecording, mpSeeking,
    mpPaused, mpOpen);

const
  ModeStr:  array[TMPModes] of string = ('Not ready', 'Stopped', 'Playing', 'Recording', 'Seeking', 'Paused', 'Open');

implementation

uses
     {$IFDEF FPC}
     lclintf,
     {$ENDIF}
     
//     avcodec,
//     avformat,
//     avutil,

//     UGraphic,
     URecord,
//     UFiles,
     UIni,
     UMain,
     UThemes;



procedure TMusic_bass.InitializePlayback;
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

  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  if not BASS_Init(1, 44100, 0, fHWND, nil) then
  begin
    {$IFNDEF FPC}
    // TODO : JB_linux find a way to do this nice..
    Application.MessageBox ('Could not initialize BASS', 'Error');
    {$ENDIF}
    Exit;
  end;
  {$ENDIF}

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

procedure TMusic_bass.InitializeRecord;
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

procedure TMusic_bass.SetVolume(Volume: integer);
begin
  //Old Sets Wave Volume
  //BASS_SetVolume(Volume);
  //New: Sets Volume only for this Application

  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  BASS_SetConfig(BASS_CONFIG_GVOL_SAMPLE, Volume);
  BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, Volume);
  BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, Volume);
  {$ENDIF}
end;

procedure TMusic_bass.SetMusicVolume(Volume: Integer);
begin
  //Max Volume Prevention
  if Volume > 100 then
    Volume := 100;

  if Volume < 0 then
    Volume := 0;


  //Set Volume
  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelSetAttributes (Bass, -1, Volume, -101);
  {$ENDIF}
end;

procedure TMusic_bass.SetLoop(Enabled: boolean);
begin
  Loop := Enabled;
end;

function TMusic_bass.Open(Name: string): boolean;
begin
  Loaded := false;
  if FileExists(Name) then
  begin
    {$IFDEF useBASS}
    // TODO : jb_linux replace with something other than bass
    Bass := Bass_StreamCreateFile(false, pchar(Name), 0, 0, 0);
    {$ENDIF}
    
    Loaded := true;
    //Set Max Volume
    SetMusicVolume (100);
  end;

  Result := Loaded;
end;

procedure TMusic_bass.Rewind;
begin
  if Loaded then begin
  end;
end;

procedure TMusic_bass.MoveTo(Time: real);
var
  bytes:    integer;
begin
  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  bytes := BASS_ChannelSeconds2Bytes(Bass, Time);
  BASS_ChannelSetPosition(Bass, bytes);
  {$ENDIF}
end;

procedure TMusic_bass.Play;
begin
  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  if Loaded then
  begin
    if Loop then
      BASS_ChannelPlay(Bass, True); // start from beginning... actually bass itself does not loop, nor does this TMusic_bass Class

    BASS_ChannelPlay(Bass, False); // for setting position before playing
  end;
  {$ENDIF}
end;

procedure TMusic_bass.Pause; //Pause Mod
begin
  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  if Loaded then begin
    BASS_ChannelPause(Bass); // Pauses Song
  end;
  {$ENDIF}
end;

procedure TMusic_bass.Stop;
begin
  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  Bass_ChannelStop(Bass);
  {$ENDIF}
end;

procedure TMusic_bass.Close;
begin
  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  Bass_StreamFree(Bass);
  {$ENDIF}
end;

function TMusic_bass.Length: real;
var
  bytes:    integer;
begin
  Result := 60;

  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  bytes  := BASS_ChannelGetLength(Bass);
  Result := BASS_ChannelBytes2Seconds(Bass, bytes);
  {$ENDIF}
end;

function TMusic_bass.Position: real;
var
  bytes:    integer;
begin
  Result := 0;

  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  bytes  := BASS_ChannelGetPosition(BASS);
  Result := BASS_ChannelBytes2Seconds(BASS, bytes);
  {$ENDIF}
end;

function TMusic_bass.Finished: boolean;
begin
  Result := false;

  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  if BASS_ChannelIsActive(BASS) = BASS_ACTIVE_STOPPED then
  begin
    Result := true;
  end;
  {$ENDIF}
end;

procedure TMusic_bass.PlayStart;
begin
  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelPlay(BassStart, True);
  {$ENDIF}
end;

procedure TMusic_bass.PlayBack;
begin
  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelPlay(BassBack, True);// then
  {$ENDIF}
end;

procedure TMusic_bass.PlaySwoosh;
begin
  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelPlay(BassSwoosh, True);
  {$ENDIF}
end;

procedure TMusic_bass.PlayChange;
begin
  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelPlay(BassChange, True);
  {$ENDIF}
end;

procedure TMusic_bass.PlayOption;
begin
  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelPlay(BassOption, True);
  {$ENDIF}
end;

procedure TMusic_bass.PlayClick;
begin
  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelPlay(BassClick, True);
  {$ENDIF}
end;

procedure TMusic_bass.PlayDrum;
begin
  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelPlay(BassDrum, True);
  {$ENDIF}
end;

procedure TMusic_bass.PlayHihat;
begin
  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelPlay(BassHihat, True);
  {$ENDIF}
end;

procedure TMusic_bass.PlayClap;
begin
  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelPlay(BassClap, True);
  {$ENDIF}
end;

procedure TMusic_bass.PlayShuffle;
begin
  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelPlay(BassShuffle, True);
  {$ENDIF}
end;

procedure TMusic_bass.StopShuffle;
begin
  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  BASS_ChannelStop(BassShuffle);
  {$ENDIF}
end;

procedure TMusic_bass.CaptureStart;
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

procedure TMusic_bass.CaptureStop;
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

//procedure TMusic_bass.CaptureCard(RecordI, SoundNum, PlayerLeft, PlayerRight: byte);
procedure TMusic_bass.CaptureCard(RecordI, PlayerLeft, PlayerRight: byte);
var
  Error:      integer;
  ErrorMsg:   string;
begin
  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass

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
  
  {$ENDIF}
end;

procedure TMusic_bass.StopCard(Card: byte);
begin
  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass
  BASS_RecordSetDevice(Card);
  BASS_RecordFree;
  {$ENDIF}
end;

function TMusic_bass.LoadSoundFromFile(var hStream: hStream; Name: string): boolean;
var
  L: Integer;
begin
  if FileExists(Name) then
  begin
    Log.LogStatus('Loading Sound: "' + Name + '"', 'LoadSoundFromFile');
    try
      {$IFDEF useBASS}
      // TODO : jb_linux replace with something other than bass
      hStream := BASS_StreamCreateFile(False, pchar(Name), 0, 0, 0);
      {$ELSE}
      hStream := FFMPeg_StreamCreateFile(False, pchar(Name) );
      {$ENDIF}
      

      
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
function TMusic_bass.GetFFTData: TFFTData;
var
Data: TFFTData;
begin
  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass

  //Get Channel Data Mono and 256 Values
  BASS_ChannelGetData(Bass, @Result, BASS_DATA_FFT512);
  //Result := Data;
  
  {$ENDIF}
end;

function TMusic_bass.LoadCustomSound(const Filename: String): Cardinal;
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

procedure TMusic_bass.PlayCustomSound(const Index: Cardinal );
begin
  {$IFDEF useBASS}
  // TODO : jb_linux replace with something other than bass

  if Index <= High(CustomSounds) then
    BASS_ChannelPlay(CustomSounds[Index].Handle, True);
    
  {$ENDIF}
end;


{*

Sorry guys... this is my mess :(
Im going to try and get ffmpeg to handle audio playback ( at least for linux )
and Im going to implement it nicly along side BASS, in TMusic_bass ( where I can )

http://www.dranger.com/ffmpeg/ffmpeg.html
http://www.dranger.com/ffmpeg/ffmpegtutorial_all.html

http://www.inb.uni-luebeck.de/~boehme/using_libavcodec.html

*}
{*
function TMusic_bass.FFMPeg_StreamCreateFile(abool : boolean; aFileName : pchar ): THandle;
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

end.
