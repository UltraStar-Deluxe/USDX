unit UAudioPlayback_Bass;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}


uses Classes,
     {$IFDEF win32}
     windows,
     {$ENDIF}
     SysUtils,
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
  TBassOutputStream = class(TAudioOutputStream)
    Handle: HSTREAM;

    constructor Create(); overload;
    constructor Create(stream: HSTREAM); overload;
  end;

type
  TAudioPlayback_Bass = class( TInterfacedObject, IAudioPlayback)
    private
      MusicStream:        HSTREAM;
      
      StartSoundStream:   HSTREAM;
      BackSoundStream:    HSTREAM;
      SwooshSoundStream:  HSTREAM;
      ChangeSoundStream:  HSTREAM;
      OptionSoundStream:  HSTREAM;
      ClickSoundStream:   HSTREAM;
      DrumSoundStream:    HSTREAM;
      HihatSoundStream:   HSTREAM;
      ClapSoundStream:    HSTREAM;
      ShuffleSoundStream: HSTREAM;

      //Custom Sounds
      CustomSounds: array of TCustomSoundEntry;
      Loaded:   boolean;
      Loop:     boolean;

    public
      function  GetName: String;

      {IAudioOutput interface}

      procedure InitializePlayback;
      procedure SetVolume(Volume: integer);
      procedure SetMusicVolume(Volume: integer);
      procedure SetLoop(Enabled: boolean);

      function Open(Name: string): boolean; // true if succeed
      
      procedure Rewind;
      procedure Play;
      procedure Pause; //Pause Mod
      procedure Stop;
      procedure Close;
      function Finished: boolean;
      function Length: real;
      function GetPosition: real;
      procedure SetPosition(Time: real);

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

      function LoadSoundFromFile(var stream: HSTREAM; Name: string): boolean;

      //Equalizer
      function GetFFTData: TFFTData;

      // Interface for Visualizer
      function GetPCMData(var data: TPCMData): Cardinal;

      //Custom Sounds
      function LoadCustomSound(const Filename: String): Cardinal;
      procedure PlayCustomSound(const Index: Cardinal );
  end;

var
  singleton_AudioPlaybackBass : IAudioPlayback;


constructor TBassOutputStream.Create();
begin
  inherited;
end;

constructor TBassOutputStream.Create(stream: HSTREAM);
begin
  Create();
  Handle := stream;
end;


function  TAudioPlayback_Bass.GetName: String;
begin
  result := 'BASS_Playback';
end;

procedure TAudioPlayback_Bass.InitializePlayback;
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
  LoadSoundFromFile(StartSoundStream,  SoundPath + 'Common Start.mp3');
  LoadSoundFromFile(BackSoundStream,   SoundPath + 'Common Back.mp3');
  LoadSoundFromFile(SwooshSoundStream, SoundPath + 'menu swoosh.mp3');
  LoadSoundFromFile(ChangeSoundStream, SoundPath + 'select music change music 50.mp3');
  LoadSoundFromFile(OptionSoundStream, SoundPath + 'option change col.mp3');
  LoadSoundFromFile(ClickSoundStream,  SoundPath + 'rimshot022b.mp3');

//  LoadSoundFromFile(DrumSoundStream,   SoundPath + 'bassdrumhard076b.mp3');
//  LoadSoundFromFile(HihatSoundStream,  SoundPath + 'hihatclosed068b.mp3');
//  LoadSoundFromFile(ClapSoundStream,   SoundPath + 'claps050b.mp3');

//  LoadSoundFromFile(ShuffleSoundStream, SoundPath + 'Shuffle.mp3');

//  Log.BenchmarkEnd(4);
//  Log.LogBenchmark('--> Loading Sounds', 4);
end;

procedure TAudioPlayback_Bass.SetVolume(Volume: integer);
begin
  //Old Sets Wave Volume
  //BASS_SetVolume(Volume);
  //New: Sets Volume only for this Application

  BASS_SetConfig(BASS_CONFIG_GVOL_SAMPLE, Volume);
  BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, Volume);
  BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, Volume);
end;

procedure TAudioPlayback_Bass.SetMusicVolume(Volume: Integer);
begin
  //Max Volume Prevention
  if Volume > 100 then
    Volume := 100;

  if Volume < 0 then
    Volume := 0;

  //Set Volume
  BASS_ChannelSetAttributes (MusicStream, -1, Volume, -101);
end;

procedure TAudioPlayback_Bass.SetLoop(Enabled: boolean);
begin
  Loop := Enabled;
end;

function TAudioPlayback_Bass.Open(Name: string): boolean;
begin
  Loaded := false;
  if FileExists(Name) then
  begin
    MusicStream := Bass_StreamCreateFile(false, pchar(Name), 0, 0, 0);

    Loaded := true;
    //Set Max Volume
    SetMusicVolume (100);
  end;

  Result := Loaded;
end;

procedure TAudioPlayback_Bass.Rewind;
begin
  if Loaded then begin
  end;
end;

procedure TAudioPlayback_Bass.SetPosition(Time: real);
var
  bytes:    integer;
begin
  bytes := BASS_ChannelSeconds2Bytes(MusicStream, Time);
  BASS_ChannelSetPosition(MusicStream, bytes);
end;

procedure TAudioPlayback_Bass.Play;
begin
  if Loaded then
  begin
    if Loop then
      BASS_ChannelPlay(MusicStream, True); // start from beginning... actually bass itself does not loop, nor does this TAudio_bass Class

    BASS_ChannelPlay(MusicStream, False); // for setting position before playing
  end;
end;

procedure TAudioPlayback_Bass.Pause; //Pause Mod
begin
  if Loaded then begin
    BASS_ChannelPause(MusicStream); // Pauses Song
  end;
end;

procedure TAudioPlayback_Bass.Stop;
begin
  Bass_ChannelStop(MusicStream);
end;

procedure TAudioPlayback_Bass.Close;
begin
  Bass_StreamFree(MusicStream);
end;

function TAudioPlayback_Bass.Length: real;
var
  bytes:    integer;
begin
  Result := 60;

  bytes  := BASS_ChannelGetLength(MusicStream);
  Result := BASS_ChannelBytes2Seconds(MusicStream, bytes);
end;

function TAudioPlayback_Bass.getPosition: real;
var
  bytes:    integer;
begin
  Result := 0;

  bytes  := BASS_ChannelGetPosition(MusicStream);
  Result := BASS_ChannelBytes2Seconds(MusicStream, bytes);
end;

function TAudioPlayback_Bass.Finished: boolean;
begin
  Result := false;

  if BASS_ChannelIsActive(MusicStream) = BASS_ACTIVE_STOPPED then
  begin
    Result := true;
  end;
end;

procedure TAudioPlayback_Bass.PlayStart;
begin
  BASS_ChannelPlay(StartSoundStream, True);
end;

procedure TAudioPlayback_Bass.PlayBack;
begin
  BASS_ChannelPlay(BackSoundStream, True);// then
end;

procedure TAudioPlayback_Bass.PlaySwoosh;
begin
  BASS_ChannelPlay(SwooshSoundStream, True);
end;

procedure TAudioPlayback_Bass.PlayChange;
begin
  BASS_ChannelPlay(ChangeSoundStream, True);
end;

procedure TAudioPlayback_Bass.PlayOption;
begin
  BASS_ChannelPlay(OptionSoundStream, True);
end;

procedure TAudioPlayback_Bass.PlayClick;
begin
  BASS_ChannelPlay(ClickSoundStream, True);
end;

procedure TAudioPlayback_Bass.PlayDrum;
begin
  BASS_ChannelPlay(DrumSoundStream, True);
end;

procedure TAudioPlayback_Bass.PlayHihat;
begin
  BASS_ChannelPlay(HihatSoundStream, True);
end;

procedure TAudioPlayback_Bass.PlayClap;
begin
  BASS_ChannelPlay(ClapSoundStream, True);
end;

procedure TAudioPlayback_Bass.PlayShuffle;
begin
  BASS_ChannelPlay(ShuffleSoundStream, True);
end;

procedure TAudioPlayback_Bass.StopShuffle;
begin
  BASS_ChannelStop(ShuffleSoundStream);
end;

function TAudioPlayback_Bass.LoadSoundFromFile(var stream: HSTREAM; Name: string): boolean;
var
  L: Integer;
begin
  if FileExists(Name) then
  begin
    Log.LogStatus('Loading Sound: "' + Name + '"', 'LoadSoundFromFile');
    try
      stream := BASS_StreamCreateFile(False, pchar(Name), 0, 0, 0);

      //Add CustomSound
      L := High(CustomSounds) + 1;
      SetLength (CustomSounds, L + 1);
      CustomSounds[L].Filename := Name;
      CustomSounds[L].Stream := TBassOutputStream.Create(stream);
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
function TAudioPlayback_Bass.GetFFTData: TFFTData;
var
  Data: TFFTData;
begin
  //Get Channel Data Mono and 256 Values
  BASS_ChannelGetData(MusicStream, @Result, BASS_DATA_FFT512);
end;

{*
 * Copies interleaved PCM 16bit uint (maybe fake) stereo samples into data.
 * Returns the number of frames (= stereo/mono sample)
 *}
function TAudioPlayback_Bass.GetPCMData(var data: TPCMData): Cardinal;
var
  info: BASS_CHANNELINFO;
  nBytes: DWORD;
begin
  //Get Channel Data Mono and 256 Values
  BASS_ChannelGetInfo(MusicStream, info);
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
    nBytes := BASS_ChannelGetData(MusicStream, @data, sizeof(TPCMData));
  end;
  if(nBytes <= 0) then
    result := 0
  else
    result := nBytes div sizeof(TPCMStereoSample);
end;

function TAudioPlayback_Bass.LoadCustomSound(const Filename: String): Cardinal;
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

procedure TAudioPlayback_Bass.PlayCustomSound(const Index: Cardinal );
begin
  if Index <= High(CustomSounds) then
  with CustomSounds[Index].Stream as TBassOutputStream do
  begin
    BASS_ChannelPlay(Handle, True);
  end;
end;


initialization
  singleton_AudioPlaybackBass := TAudioPlayback_Bass.create();
  AudioManager.add( singleton_AudioPlaybackBass );

finalization
  AudioManager.Remove( singleton_AudioPlaybackBass );

end.
